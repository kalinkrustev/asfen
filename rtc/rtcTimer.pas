{
  @html(<b>)
  Timer
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  Thread-safe timer class,
  tightly coupled with the RTC Window Handle and Thread Pool mechanisms.
  @html(<br>)

  This class is used internally by TRtcConnection and all its descendant classes
  to implement the timeout, reconnect and restart functionality.
}
unit rtcTimer;

{$INCLUDE rtcDefs.inc}

interface

uses
  rtcTrashcan,
  
  Windows,
  Messages,
  SysUtils,
  Classes,

  rtcSyncObjs,
  rtcThrPool,
  rtcHWndPool,
  rtcLog;

type
  // @Abstract(Events used by RTC Timer)
  TRtcTimerEvent = procedure of object;

  { @Abstract(RTC Timer class)

    This class ensures a Thread-Safe Timer by
    using the RTC Window Handle Pool and RTC Thread Pool
    instead of the TTimer class implementation. }
  TRtcTimer = class
  private
    FRunning:boolean;
    FAutoDisable:boolean;
    FAutoDestroy:boolean;
    FHandle:HWND;
    FEvent:TRtcTimerEvent;
    FThr:TRtcThread;
    FJob:TObject;

    FInterval:Cardinal;
    FNextTrigger:Cardinal;

  public
    // Create a Timer. To start the timer, use the @Link(Enable) method.
    constructor Create(Multi_Threaded:boolean); virtual;

    { @exclude }
    destructor Destroy; override;

    { Allways use Stop instead of Free or Destroy! }
    class procedure Stop(me:TObject);

    { Disable the Timer }
    class procedure Disable(me:TObject);

    { Enable the Timer to trigger 'Event' every 'Wait' miliseconds.
      If AutoDisable is True, Timer will be automatically disabled after the event has been triggered.
      If AutoDestroy is True, Timer object will be automaticaly destroyed after the event has been triggered. }
    class procedure Enable(me:TObject; Wait:Cardinal; Event:TRtcTimerEvent; AutoDisable:boolean=False; AutoDestroy:boolean=False); overload;

    { Enable the Timer to post the 'Job' to Thread 'Thr' every 'Wait' miliseconds.
      If AutoDisable is True, Timer will be automatically disabled after the event has been triggered.
      If AutoDestroy is True, Timer object will be automaticaly destroyed after the event has been triggered. }
    class procedure Enable(me:TObject; Wait:Cardinal; Thr:TRtcThread; Job:TObject; AutoDisable:boolean=False; AutoDestroy:boolean=False); overload;

    { Reset elapsed time counter.
      This will make the Timer skip one upcoming event. }
    class procedure Reset(me:TObject);

    { For internal use only!!!
      Called by the framework to call the Event for this timer.
      @exclude }
    class procedure Timer(me:TObject);
    end;

{ For internal use only!!!
  Called by the framework to find the Timer matching the
  Timer ID included in the Windows message.

  @exclude }
function rtcGetTimer(ID:LongWord):TRtcTimer;

implementation

uses
  memBinList;

var
  TimerList:tBinList;
  CS:TRtcCritSec;

procedure CloseTimerPool;
  begin
  CS.Enter;
  try
    if assigned(TimerList) then
      begin
      TimerList.Free;
      TimerList:=nil;
      end;
  finally
    CS.Leave;
    end;
  end;

function rtcStoreTimer(obj:TObject):boolean;
  begin
  Result:=False;
  if not assigned(CS) then Exit;

  CS.Enter;
  try
    if not assigned(TimerList) then
      TimerList:=tBinList.Create(128);

    if assigned(TimerList) then
      if TimerList.search(longint(Obj))=0 then
        begin
        Result:=True;
        TimerList.insert(longint(Obj), 1);
        end;
  finally
    CS.Leave;
    end;
  end;

function rtcRemoveTimer(obj:TObject):boolean;
  begin
  Result:=False;
  if not assigned(CS) then Exit;

  CS.Enter;
  try
    if assigned(TimerList) then
      if TimerList.search(longint(Obj))>0 then
        begin
        TimerList.remove(longint(Obj));
        Result:=True;
        end;
  finally
    CS.Leave;
    end;
  end;

function rtcGetTimer(ID:LongWord):TRtcTimer;
  begin
  Result:=nil;
  if not assigned(CS) then Exit;

  CS.Enter;
  try
    if assigned(TimerList) then
      if TimerList.search(ID)>0 then
        Result:=TRtcTimer(ID);
  finally
    if Result=nil then
      CS.Leave;
    end;
  end;

function rtcEnterTimer(ID:LongWord):boolean;
  begin
  Result:=False;
  if not assigned(CS) then Exit;

  CS.Enter;
  try
    if assigned(TimerList) then
      if TimerList.search(ID)>0 then
        Result:=True;
  finally
    if not Result then CS.Leave;
    end;
  end;

procedure rtcLeaveTimer;
  begin
  if not assigned(CS) then Exit;
  CS.Leave;
  end;

constructor TRtcTimer.Create(Multi_Threaded:boolean);
  begin
  inherited Create;
  FRunning:=False;
  FHandle := rtcGetHWND(Multi_Threaded);
  FInterval := 0;
  FNextTrigger := 0;

  rtcStoreTimer(self);
  end;

destructor TRtcTimer.Destroy;
  begin
  rtcRemoveTimer(self);

  if FRunning then
    begin
    FRunning:=False;
    KillTimer(FHandle,LongWord(self));
    end;
  rtcReturnHWND(FHandle);
  inherited Destroy;
  end;

class procedure TRtcTimer.Stop(me:TObject);
  begin
  if rtcRemoveTimer(me) then
    me.Free;
  end;

class procedure TRtcTimer.Timer(me:TObject);
  var
    FE:TRtcTimerEvent;
    TH:TRtcThread;
    JO:TObject;
    intimer:boolean;
  begin
  intimer:=True;
  with TRtcTimer(me) do
    try
      if FRunning and (GetTickCount>=FNextTrigger) then
        begin
        if assigned(FEvent) then
          begin
          FNextTrigger:=GetTickCount + FInterval;
          if FAutoDisable then
            begin
            FRunning:=False;
            KillTimer(FHandle, LongWord(me));
            end;
          FE:=FEvent;
          if FAutoDestroy then Free;

          rtcLeaveTimer;
          intimer:=False;

          FE;
          end
        else if assigned(FThr) then
          begin
          FNextTrigger:=GetTickCount+FInterval;
          if FAutoDisable then
            begin
            FRunning:=False;
            KillTimer(FHandle, LongWord(me));
            end;
          TH:=FThr;
          JO:=FJob;
          if FAutoDestroy then Free;

          rtcLeaveTimer;
          intimer:=False;

          TRtcThread.PostJob(TH, JO);
          end
        else // Disable ...
          begin
          FRunning:=False;
          KillTimer(FHandle, LongWord(me));
          if FAutoDestroy then Free;
          end;
        end;
    finally
      if intimer then rtcLeaveTimer;
      end;
  end;

class procedure TRtcTimer.Disable(me:TObject);
  begin
  if rtcEnterTimer(longword(me)) then
    with TRtcTimer(me) do try
      if FRunning then
        begin
        FRunning:=False;
        KillTimer(FHandle, LongWord(me));
        end;
    finally
      rtcLeaveTimer;
      end;
  end;

class procedure TRtcTimer.Reset(me:TObject);
  begin
  if rtcEnterTimer(longword(me)) then
    with TRtcTimer(me) do try
      if FRunning then
        FNextTrigger:=GetTickCount + FInterval;
    finally
      rtcLeaveTimer;
      end;
  end;

class procedure TRtcTimer.Enable(me:TObject; Wait: Cardinal; Event: TRtcTimerEvent; AutoDisable:boolean=False; AutoDestroy:boolean=False);
  begin
  if rtcEnterTimer(LongWord(me)) then
    with TRtcTimer(me) do try
      FAutoDisable:=AutoDisable or AutoDestroy;
      FAutoDestroy:=AutoDestroy;
      FThr:=nil;
      FEvent:=Event;
      FInterval:=Wait;
      FNextTrigger:=GetTickCount+Wait;
      if SetTimer(FHandle, LongWord(me), Wait, nil) = 0 then
        raise EOutOfResources.Create('No more timers available.')
      else
        FRunning:=True;
    finally
      rtcLeaveTimer;
      end;
  end;

class procedure TRtcTimer.Enable(me:TObject; Wait:Cardinal; Thr:TRtcThread; Job:TObject; AutoDisable:boolean=False; AutoDestroy:boolean=False);
  begin
  if rtcEnterTimer(LongWord(me)) then
    with TRtcTimer(me) do try
      FAutoDisable:=AutoDisable or AutoDestroy;
      FAutoDestroy:=AutoDestroy;
      FThr:=Thr;
      FJob:=Job;
      FEvent:=nil;
      FInterval:=Wait;
      FNextTrigger:=GetTickCount+Wait;
      if SetTimer(FHandle, LongWord(me), Wait, nil) = 0 then
        raise EOutOfResources.Create('No more timers available.')
      else
        FRunning:=True;
    finally
      rtcLeaveTimer;
      end;
  end;

function RtcTimerWindowProc(ahWnd   : HWND;
                            auMsg   : LongWord;
                            awParam : WPARAM;
                            alParam : LPARAM): Integer; stdcall;
  var
    Obj    : TObject;
  begin
  if auMsg=WM_TIMER then
    begin
    if (awParam<>0) then
      Obj:=rtcGetTimer(awParam)
    else
      Obj:=nil;

    if (Obj<>nil) and (Obj is TRtcTimer) then
      begin
      try
        TRtcTimer.Timer(Obj);
      except
        on E:Exception do
          Log('WM_TIMER',E);
        end;
      Result := 0;
      end
    else
      Result := DefWindowProc(ahWnd, auMsg, awParam, alParam);
    end
  else
    Result := DefWindowProc(ahWnd, auMsg, awParam, alParam);
  end;

var
  RtcTimerRegistered:boolean=False;

  RtcTimerWindowClass: TWndClass = (
        style         : 0;
        lpfnWndProc   : @RtcTimerWindowProc;
        cbClsExtra    : 0;
        cbWndExtra    : SizeOf(Pointer);
        hInstance     : 0;
        hIcon         : 0;
        hCursor       : 0;
        hbrBackground : 0;
        lpszMenuName  : nil;
        lpszClassName : 'RtcTimerWindowClass');

procedure RtcTimerUnregisterClass;
  begin
  if RtcTimerRegistered then
    begin
    Windows.UnregisterClass(RtcTimerWindowClass.lpszClassName, HInstance);
    RtcTimerRegistered:=False;
    end;
  end;

procedure RtcTimerRegisterClass;
  var
    TempClass       : TWndClass;
    ClassRegistered : Boolean;
  begin
  if not RTcTimerRegistered then
    begin
    RtcTimerWindowClass.hInstance := HInstance;
    ClassRegistered := GetClassInfo(HInstance,
                                    RtcTimerWindowClass.lpszClassName,
                                    TempClass);
    if not ClassRegistered then
      begin
      if Windows.RegisterClass(RtcTimerWindowClass)=0 then
        Exit;
      end;
    RtcTimerRegistered:=True;
    end;
  end;

initialization
CS:=TRtcCritSec.Create;

RTC_HWND_CLASS_NAME:=RtcTimerWindowClass.lpszClassName;
RtcTimerRegisterClass;
rtcInitMainHWND;

finalization
CloseTimerPool;
RtcTimerUnregisterClass;

rtcReleaseMainHWND;
Garbage(CS);
end.
