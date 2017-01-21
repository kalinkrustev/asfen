{
  @html(<b>)
  Connection Traffic Limiter
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  Using this unit, you can set up and activate the network traffic limiter.
  @html(<br>)

  Network traffic limiter can limit the number of
  packets that are actively being sent or received.
  @html(<br>)

  This can lower server's network traffic to a minimum,
  while still being able to work with a high number of
  active connections at once. Tests on some wireless network cards
  have shown that using this limiter can prevent frequent connection
  losses on low-bandwidth/high-traffic servers.
}
unit rtcConnLimit;

{$INCLUDE rtcDefs.inc}

interface

uses
  Windows,

  rtcTrashcan,

  memXList,
  memBinList,

  rtcSyncObjs,
  rtcInfo,
  rtcTimer,
  rtcThrPool;

const
  RTC_THREAD_INFO_INDEX:string='RTCTHR';

  // "Connect" action
  RTC_ACTION_CONNECT=1;
  // "Accept" action
  RTC_ACTION_ACCEPT=2;
  // "Read" action
  RTC_ACTION_READ=3;
  // "Write" action
  RTC_ACTION_WRITE=4;

  {@exclude}
  RTC_ACTION_FIRST=1;
  {@exclude}
  RTC_ACTION_LAST=4;

  {@exclude
   Max. repeat count for actins with 0 as RTC_LIMIT_CONN_MAXREPEAT[] }
  RTC_LIMIT_MAX_REPEAT:longword=MAXLONG;

var
  { To use a network traffic limiter, set @name to @true
    (Default = @false; not limiting active connection count). }
  RTC_LIMIT_CONN:boolean=False;

  { Number of active connections allowed at a time (0=unlimited).
    Defined for each action type: Connect, Accept, Read, Write (respectively) }
  RTC_LIMIT_CONN_COUNT:array[RTC_ACTION_FIRST..RTC_ACTION_LAST] of cardinal =
    (10, 0, 0, 1);

  { Number of same-action iterations a connection may go through,
    before it has to gives away the right to the next one waiting (0=unlimited).
    Defined for each action type: Connect, Accept, Read, Write (respectively) }
  RTC_LIMIT_CONN_MAXREPEAT:array[RTC_ACTION_FIRST..RTC_ACTION_LAST] of cardinal =
    (1, 1, 1, 1);

  { Timeout (in seconds) which may not be exceeded between iterations,
    or the connection will give the right up to the next one waiting (0=unlimited).
    Defined for each action type: Connect, Accept, Read, Write (respectively) }
  RTC_LIMIT_CONN_TIMEOUT:array[RTC_ACTION_FIRST..RTC_ACTION_LAST] of cardinal =
    (1, 1, 1, 1);

{ Used by all Connection components internaly,
  to tell the traffic limiter they want to start an action.
  @exclude }
function rtcStartAction(Thr: TRtcThread; action:cardinal): boolean;

{ Used by all connection components internaly,
  to tell the traffic limiter they just completed the last action.
  @exclude }
procedure rtcCloseAction(Thr: TRtcThread);

implementation

type
  TRtcWaitData=record
    WaitList:TXList; // action-based waiting list
    Thr_WorkCount:cardinal; // number of threads working with this action
    end;

  TRtcTimeoutObject=class(TRtcObject)
  public
    Timer:TRtcTimer;
    procedure Kill; override;
    end;

  TRtcTimeoutJob=class(TRtcJob)
  public
    procedure Run(Thr:TRtcThread); override;
    procedure Kill; override;
    end;

var
  CS:TRtcCritSec;
  WorkList:TBinList;  // global working threads list
  TimeList:TBinList;  // timeout list for working threads
  PauseList:TBinList; // global paused threads list
  Data:array[RTC_ACTION_FIRST..RTC_ACTION_LAST] of TRtcWaitData;

  Message_LimitTimeout:TRtcTimeoutJob=nil;

{ TRtcTimeoutObject }

procedure TRtcTimeoutObject.Kill;
  begin
  TRtcTimer.Stop(Timer);
  Free;
  end;

{ TRtcTimeoutJob }

procedure TRtcTimeoutJob.Run(Thr:TRtcThread);
  begin
  if RTC_LIMIT_CONN and assigned(Thr) then
    rtcCloseAction(Thr);
  end;

procedure TRtcTimeoutJob.Kill;
  begin
  // do not free. This object will be used for all Threads.
  end;

{ --- }

procedure RunPaused(action:integer);
  var
    TC:longword;
    wrk:cardinal;
    Thr:TRtcThread;
    Obj:TRtcTimeoutObject;
  begin
  with Data[action] do
    begin
    wrk:=WaitList.First;
    WaitList.removeFirst; // remove from waiting list
    Inc(Thr_WorkCount);

    PauseList.remove(wrk); // remove from paused list
    WorkList.insert(wrk, action); // add to working list

    TC:=RTC_LIMIT_CONN_MAXREPEAT[action];
    if TC=0 then TC:=RTC_LIMIT_MAX_REPEAT;
    TimeList.insert(wrk,TC);

    Thr:=TRtcThread(wrk);
    if RTC_LIMIT_CONN_TIMEOUT[action]>0 then
      begin
      obj:=TRtcTimeoutObject(Thr.Info.Obj[RTC_THREAD_INFO_INDEX]);
      if obj=nil then
        begin
        obj:=TRtcTimeoutObject.Create;
        obj.Timer:=TRtcTimer.Create(True);
        Thr.Info.Obj[RTC_THREAD_INFO_INDEX]:=Obj;
        end;
      TRtcTimer.Enable(Obj.Timer,
                       RTC_LIMIT_CONN_TIMEOUT[action]*1000,
                       Thr,Message_LimitTimeout,
                       True);
      end;
    Thr.Resume;
    end;
  end;

function rtcStartAction(Thr: TRtcThread; action:cardinal): boolean;
  var
    act:cardinal;
    TC:longword;
    Obj:TRtcTimeoutObject;
  begin
  if not RTC_LIMIT_CONN then
    Result:=True
  else
    begin
    Result:=False;

    CS.Enter;
    try
      if not assigned(WorkList) then
        begin
        PauseList:=tBinList.Create(128);
        WorkList:=tBinList.Create(128);
        TimeList:=tBinList.Create(128);
        end;

      act:=WorkList.search(longword(Thr));
      if (act>0) and (act<>action) then // action changing
        begin
        with Data[act] do
          begin
          // remove from working list ...
          WorkList.remove(longword(Thr));
          TimeList.remove(longword(Thr));
          Dec(Thr_WorkCount);

          // Disable timeout
          if assigned(Thr.Info.Obj[RTC_THREAD_INFO_INDEX]) then
            TRtcTimer.Disable(TRtcTimeoutObject(Thr.Info.Obj[RTC_THREAD_INFO_INDEX]).Timer);

          if WaitList.Count>0 then // other threads waiting
            RunPaused(act);

          act:=0;
          end;
        end;

      with Data[action] do
        begin
        if act>0 then // active
          begin
          TC:=TimeList.search(longword(Thr));
          if TC>0 then // can keep working
            begin
            // Reset timeout
            if assigned(Thr.Info.Obj[RTC_THREAD_INFO_INDEX]) then
              TRtcTimer.Reset(TRtcTimeoutObject(Thr.Info.Obj[RTC_THREAD_INFO_INDEX]).Timer);

            // continue ...
            if TC<RTC_LIMIT_MAX_REPEAT then
              begin
              Dec(TC);
              TimeList.change(longword(Thr),TC);
              end;
            Result:=True;
            end
          else // exceeding allowed time limit
            begin
            if WaitList.Count=0 then
              begin
              // Reset timeout
              if assigned(Thr.Info.Obj[RTC_THREAD_INFO_INDEX]) then
                TRtcTimer.Reset(TRtcTimeoutObject(Thr.Info.Obj[RTC_THREAD_INFO_INDEX]).Timer);

              // continue ...
              Result:=True;
              end
            else
              begin
              // Disable timeout
              if assigned(Thr.Info.Obj[RTC_THREAD_INFO_INDEX]) then
                TRtcTimer.Disable(TRtcTimeoutObject(Thr.Info.Obj[RTC_THREAD_INFO_INDEX]).Timer);

              // remove from working list ...
              WorkList.remove(longword(Thr));
              TimeList.remove(longword(Thr));
              Dec(Thr_WorkCount);

              RunPaused(action);

              // add to waiting list ...
              Thr.Pause;
              PauseList.insert(longword(Thr),action);
              WaitList.addLast(longword(Thr));
              end;
            end;
          end
        else if PauseList.search(longword(Thr))=0 then // not waiting
          begin
          if not assigned(WaitList) then
            begin
            WaitList:=tXList.Create(128);
            Thr_WorkCount:=0;
            end;

          if RTC_LIMIT_CONN_COUNT[action]=0 then
            begin
            // no limit, continue ...
            Result:=True;
            end
          else if Thr_WorkCount<RTC_LIMIT_CONN_COUNT[action] then // theres place for another thread
            begin
            if WaitList.Count=0 then // nobody waiting
              begin
              // start timeout timer ...
              if RTC_LIMIT_CONN_TIMEOUT[action]>0 then
                begin
                Obj:=TRtcTimeoutObject(Thr.Info.Obj[RTC_THREAD_INFO_INDEX]);
                if Obj=nil then
                  begin
                  Obj:=TRtcTimeoutObject.Create;
                  Obj.Timer:=TRtcTimer.Create(True);
                  Thr.Info.Obj[RTC_THREAD_INFO_INDEX]:=Obj;
                  end;
                TRtcTimer.Enable(Obj.Timer,
                                 RTC_LIMIT_CONN_TIMEOUT[action]*1000,
                                 Thr,Message_LimitTimeout,
                                 True);
                end;

              // start thread ...
              WorkList.insert(longword(Thr),action);
              if RTC_LIMIT_CONN_MAXREPEAT[action]>0 then
                TC:=RTC_LIMIT_CONN_MAXREPEAT[action]-1
              else
                TC:=RTC_LIMIT_MAX_REPEAT;
              TimeList.insert(longword(Thr),TC);
              Inc(Thr_WorkCount);

              Result:=True;
              end
            else
              begin
              RunPaused(action); // activate one

              // add to waiting list ...
              Thr.Pause;
              PauseList.insert(longword(Thr),action);
              WaitList.addLast(longword(Thr));
              end;
            end
          else
            begin
            // add to waiting list ...
            Thr.Pause;
            PauseList.insert(longword(Thr),action);
            WaitList.addLast(longword(Thr));
            end;
          end;
        end;
    finally
      CS.Leave;
      end;
    end;
  end;

procedure rtcCloseAction(Thr: TRtcThread);
  var
    act:cardinal;
  begin
  if not RTC_LIMIT_CONN then Exit;

  CS.Enter;
  try
    if not assigned(WorkList) then
      begin
      PauseList:=tBinList.Create(128);
      WorkList:=tBinList.Create(128);
      TimeList:=tBinList.Create(128);
      end;

    act:=WorkList.search(longword(Thr));
    if act>0 then // active
      begin
      // Disable timeout
      if assigned(Thr.Info.Obj[RTC_THREAD_INFO_INDEX]) then
        TRtcTimer.Disable(TRtcTimeoutObject(Thr.Info.Obj[RTC_THREAD_INFO_INDEX]).Timer);

      with Data[act] do
        begin
        // remove from working list ...
        WorkList.remove(longword(Thr));
        TimeList.remove(longword(Thr));
        Dec(Thr_WorkCount);

        if WaitList.Count>0 then // other threads waiting
          RunPaused(act);
        end;
      end
    else
      begin
      act:=PauseList.search(longword(Thr));
      if act>0 then
        begin
        with Data[act] do
          begin
          // remove from waiting list and resume
          PauseList.remove(longword(Thr));
          WaitList.removeThis(longword(Thr));
          Thr.Resume;
          end;
        end;
      end;
  finally
    CS.Leave;
    end;
  end;

procedure _Init;
  var
    a:integer;
  begin
  CS:=TRtcCritSec.Create;

  Message_LimitTimeout:=TRtcTimeoutJob.Create;

  PauseList:=nil;
  WorkList:=nil;
  TimeList:=nil;
  for a:=Low(Data) to High(Data) do
    with Data[a] do
      begin
      WaitList:=nil;
      Thr_WorkCount:=0;
      end;
  end;

procedure _Free;
  var
    a:integer;
  begin
  CS.Enter;
  try
    if assigned(PauseList) then
      begin
      PauseList.Free;
      PauseList:=nil;
      end;
    if assigned(WorkList) then
      begin
      WorkList.Free;
      WorkList:=nil;
      end;
    if assigned(TimeList) then
      begin
      TimeList.Free;
      TimeList:=nil;
      end;

    for a:=Low(Data) to High(Data) do
      if assigned(Data[a].WaitList) then
        begin
        Data[a].WaitList.Free;
        Data[a].WaitList:=nil;
        Data[a].Thr_WorkCount:=0;
        end;

    Garbage(Message_LimitTimeout);
  finally
    CS.Leave;
    end;

  Garbage(CS);
  end;

initialization
_Init;
finalization
_Free;
end.
