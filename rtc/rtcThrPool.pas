{
  @html(<b>)
  Thread Pool
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  Thread pooling mechanism used by all RTC connection components
  when component's @Link(TRtcConnection.MultiThreaded) property is set to True.
  @html(<br><br>)

  Unless you want to enhance the conncetion components or add your
  own connection providers, you will NEVER get in direct contact
  with this classes. They are being used internaly by most
  Connection Provider components to enable MultiThreaded execution.
  @html(<br><br>)

  The only thing you could get in contact with as a component user
  are the global Threading parameters @Link(RTC_THREAD_POOL_PLUS),
  @Link(RTC_THREAD_POOL_OVERSIZE) and @Link(RTC_THREAD_POOL_MAX).
  @html(<br><br>)

  Or, in case you need to post jobs to a connection component
  to enhance its functionality, with the @Link(TRtcJob) class.
}
unit rtcThrPool;

{$INCLUDE rtcDefs.inc}

interface

uses
  rtcTrashcan,

  Windows,
  // Messages,

  memXList,
  memBinList,

  rtcInfo,

  rtcLog,
  rtcSyncObjs,
  rtcHWndPool,

  SysUtils,
  Classes;

var
  // Min. number of unused threads to keep active
  RTC_THREAD_POOL_PLUS:word=3;
  // Max. number of unused threads to keep active
  RTC_THREAD_POOL_OVERSIZE:word=2048;
  // Max. number of threads in our thread pool.
  RTC_THREAD_POOL_MAX:word=64;
  // Thread Priority
  RTC_THREAD_PRIORITY:TThreadPriority=tpNormal;

  // Log unhandled thread exceptions?
  LOG_THREAD_EXCEPTIONS:boolean=False;

type
  { @Abstract(Exception to be raised when System Thread limit was reached and not a single thread could be created)
    @exclude }
  EThreadLimitReached = class(Exception);

  // Event for Synchronized calls
  TRtcSyncEvent = procedure of object;

  TRtcWorkerThread = class;
  TRtcThread = class;

  { @Abstract(RTC Job class)

    To be able to post jobs to a threaded connection component,
    you can derive your own classes from @Link(TRtcJob). By implementing
    the methods @Link(TRtcJob.Run) and @Link(TRtcObject.Kill), you can post
    any job with your user-defined data to the connection component's thread. }
  TRtcJob = class(TRtcObject)

    { This method will be called ONCE to run (execute) the job.
      It is the Run() method's responsibility to release the object
      when it has finished with its execution.

      If you post jobs to connection components,
      handle your expected exceptions properly.

      Exceptions caught by the Threading mechanism will
      not be passed any further. If exception gets raised and it
      returns to the Threading mechanism, the corresponding Thread
      object will be closed, all jobs will be Killed and th Thread
      will be released from memory, which will result in
      abortive disconnect. }
    procedure Run(Thr:TRtcThread); virtual; abstract;
    end;

  { @Abstract(Thread start/stop callback class) }
  TRtcThreadCallback = class
    { Called from inside each Thread, after it was started/created }
    procedure AfterThreadStart; virtual; abstract;
    { Called from inside each Thread, before it will be stopped/destroyed }
    procedure BeforeThreadStop; virtual; abstract;
    { Callled after all threads have been stopped.
      This is the method from which you should destroy the object by calling "Free" }
    procedure DestroyCallback; virtual; abstract;
    end;

  { @Abstract(Our threading class)

    We create threads ONLY using this class.
    This class implements all methods needed for synchronizing with the GUI,
    posting jobs, pausing, resuming and stopping the thread. }
  TRtcThread = class
  private
    MsgList:TXList;
    FInfo:TRtcInfo;

    FThr:TRtcWorkerThread;
    Paused:boolean;
    Quitting:boolean;

    class function GetJob(me:TObject): TObject;
    procedure Idle;

    procedure Run(Thr:TRtcWorkerThread);

  protected

    { Called by the Worker Thread to execute a job.
      For user-defined jobs (the ones not derived from TRtcJob),
      you need to override this method and call the inherited Work(Job).
      Resutn TRUE if Thread was freed. }
    function Work(Job:TObject):boolean; virtual;

    { Called by the Worket Thread to kill a job.
      For user-defined jobs(the ones not derived from TRtcJob),
      you need to override this method and call the inherited Kill(Job). }
    procedure Kill(Job:TObject); virtual;

  public
    // Create a Thread. To give the thread something to do, you will have to post a job to it.
    constructor Create; virtual;
    { @exclude }
    destructor Destroy; override;

    // Call the 'Event' synchronized (for GUI access). May only be used from within the thread.
    procedure Sync(Event:TRtcSyncEvent);

    // Pause thread execution (thread-safe call)
    procedure Pause;
    // Resume thread execution (thread-safe call)
    procedure Resume;

    // Lock threads
    class function Lock(me:TObject):boolean;

    // Unlock threads
    class procedure UnLock;

    // add new job for the thread (thread-safe call)
    class function PostJob(me:TObject; Job:TObject; HighPriority:boolean=False; AutoResume:boolean=False):boolean;

    // Stop the thread (thread-safe call: will post a QUIT message to thread and destroy it from inside the thread)
    class procedure Stop(me:TObject);

    // Get Thread ID
    function ThreadID:Cardinal;

    // Attach additional information to this Thread. May only be used from within the thread.
    property Info:TRtcInfo read FInfo;
    end;

  { Internal Class -> DO NOT CREATE!
    @exclude }
  TRtcWorkerThread = class(TThread)
  private
    Msg: boolean;
    Work: TRtcThread;

  protected
    Run:TRtcEvent;
    FEvent:TRtcSyncEvent;
    FHWND:HWND;

    procedure Execute; override;
    procedure Sync(Event:TRtcSyncEvent);

  public
    procedure MySyncEvent;

    procedure PostQuit;
    procedure PostWork(Thr:TRtcThread);

    constructor Create(CreateSuspended:boolean);
    destructor Destroy; override;
    end;

var
  { @exclude }
{$IFDEF CLR}
  MainThr:System.Threading.Thread;
{$ELSE}
  MainThrID:DWORD;
{$ENDIF}

{ Add a new Thread Callback.

  Please note that you can NOT remove a callback and that you need
  to add all callbacks before a first thread was created, which is best
  done from your units "initialization" section. To avoid memory leaks on
  application close, you should also implement the "DestroyCallback" method. }
procedure AddThreadCallback(const Callback:TRtcThreadCallback);

implementation

type
  TRtcQuitMessage=class
    end;

var
  ThreadPool:TBinList; // all running threads (sorted for searching)
  FreePool:TXList; // threads not in use (not sorted -> add/remove last)

  ThrList:tBinList; // list of all thread objects (sorted for fast searching)
  WaitList:tBinList; // list of all thread objects waiting for execution
  WorkList:tBinList; // list of all thread objects currently executing

  Message_Quit:TRtcQuitMessage;

  Thr_LastExec:integer; // value of the last thread object that has gone into execution

  CSThread:TRtcCritSec;

  InsideCallback:integer=0;
  ThreadCallbacks:array of TRtcThreadCallback;
  ThreadCallbackCount:integer=0;

  OpenCnt:integer;
  CSOpen:TRtcEvent;

{ Add a new Thread Callback }
procedure AddThreadCallback(const Callback:TRtcThreadCallback);
  begin
  CSThread.Enter;
  try
    Inc(ThreadCallbackCount);
    SetLength(ThreadCallbacks, ThreadCallbackCount);
    ThreadCallbacks[ThreadCallbackCount-1]:=Callback;
  finally
    CSThread.Leave;
    end;
  end;

{ Remove all Thread Callbacks }
procedure RemoveThreadCallbacks;
  var
    a:integer;
  begin
  CSThread.Enter;
  try
    for a:=0 to ThreadCallbackCount-1 do
      begin
      try
        ThreadCallbacks[a].DestroyCallback;
      except
        on E:Exception do
          if LOG_THREAD_EXCEPTIONS then
            Log('TRtcThreadCallback.DestroyCallback',E);
        end;
      ThreadCallbacks[a]:=nil;
      end;
    SetLength(ThreadCallbacks,0);
    ThreadCallbackCount:=0;
  finally
    CSThread.Leave;
    end;
  end;

procedure DoAfterThreadStart;
  var
    i:integer;
  begin
  CSThread.Enter;
  try
    Inc(InsideCallback);
    for i:=0 to ThreadCallbackCount-1 do
      try
        ThreadCallbacks[i].AfterThreadStart;
      except
        on E:Exception do
          if LOG_THREAD_EXCEPTIONS then
            Log('TRtcThreadCallback.AfterThreadStart',E);
        end;
  finally
    CSThread.Leave;
    end;
  end;

procedure DoBeforeThreadStop;
  var
    i:integer;
  begin
  CSThread.Enter;
  try
    for i:=ThreadCallbackCount-1 downto 0 do
      try
        ThreadCallbacks[i].BeforeThreadStop;
      except
        on E:Exception do
          if LOG_THREAD_EXCEPTIONS then
            Log('TRtcThreadCallback.BeforeThreadStop',E);
        end;
    Dec(InsideCallback);
    if InsideCallback=0 then
      RemoveThreadCallbacks;
  finally
    CSThread.Leave;
    end;
  end;

{ Work pool }

function GetWork:TRtcThread; // get next waiting object (remove it from waiting list, add it to working list)
  var
    i,wrk:cardinal;
  begin
  Result:=nil;
  if not assigned(ThrList) then Exit;

  if WaitList.Count>0 then
    begin
    if Thr_LastExec>0 then
      begin
      wrk:=WaitList.search_g(Thr_LastExec,i);
      if wrk<=0 then
        wrk:=WaitList.search_min(i);
      end
    else
      wrk:=WaitList.search_min(i);
    Thr_LastExec:=wrk;

    WaitList.remove(wrk); // remove from waiting list
    WorkList.insert(wrk,1); // add to working list

    Result:=TRtcThread(wrk);
    end;
  end;

procedure DoneWork(Thr:TObject); // remove object from working list
  begin
  if not assigned(ThrList) then Exit;

  if (WorkList.search(longword(Thr))>0) then
    WorkList.remove(longword(Thr));
  end;

function PutWork(Thr:TObject):boolean; // put object in waiting list
  begin
  if not assigned(ThrList) then
    Result:=False
  else if (WorkList.search(longword(Thr))=0) and // not working and
          (WaitList.search(longword(Thr))=0) then // not waiting
    begin
    WaitList.insert(longword(Thr),1); // add to waiting list
    Result:=True;
    end
  else
    Result:=False;
  end;

{ Thread Pool }

function GetThread:TRtcWorkerThread;
  var
    NWork:TRtcWorkerThread;
  begin
  Result:=nil;

  if not assigned(ThreadPool) then // Create pool if not open
    begin
    ThreadPool:=tBinList.Create(128);
    FreePool:=TXList.Create(128);
    end;

  if FreePool.Count>0 then // threads available
    begin
    Result:=TRtcWorkerThread(FreePool.Last);
    FreePool.removeLast; // remove from free threads list
    end
  else if ThreadPool.Count<RTC_THREAD_POOL_MAX then // thread limit not reached
    begin
    try
      Result:=TRtcWorkerThread.Create(False);
    except
      on E:Exception do
        begin
        if LOG_THREAD_EXCEPTIONS then
          Log('WorkerThread.Create',E);
        if ThreadPool.Count=0 then
          raise EThreadLimitReached.Create(E.ClassName+':'+E.Message)
        else
          Result:=nil;
        end;
      end;

    ThreadPool.insert(longword(Result),1);
    end;

  if assigned(Result) then
    begin
    NWork:=nil;
    while (ThreadPool.Count<RTC_THREAD_POOL_MAX) and // thread limit not reached
          (FreePool.Count<RTC_THREAD_POOL_PLUS) do // free thread count under our 'minimum'
      begin
      try
        NWork:=TRtcWorkerThread.Create(False);
      except
        on E:Exception do
          begin
          if LOG_THREAD_EXCEPTIONS then
            Log('WorkerThread.Create',E);
          Break;
          end;
        end;
      ThreadPool.insert(longword(NWork),1);
      FreePool.addLast(longword(NWork));
      end;
    end;
  end;

function ReturnThread(Thr:TRtcWorkerThread):boolean; // executed 1 object, returning for another
  var
    Work:TRtcThread;
  begin
  Result:=False;

  CSThread.Enter;
  try
    if assigned(ThreadPool) then
      begin
      if ThreadPool.search(longword(Thr))>0 then
        begin
        Work:=GetWork;
        if Work<>nil then // execution object waiting
          begin
          Thr.PostWork(Work);
          Result:=True;
          end
        else if FreePool.Count<RTC_THREAD_POOL_OVERSIZE then
          begin
          FreePool.AddLast(longword(Thr));
          Result:=True;
          end
        else
          ThreadPool.remove(longword(Thr));
        end;
      end;
  finally
    CSThread.Leave;
    end;
  end;

procedure ClosingThread(Thr:TRtcWorkerThread);
  begin
  Garbage(Thr);

  CSThread.Enter;
  try
    if assigned(ThreadPool) then
      if ThreadPool.search(longword(Thr))>0 then
        ThreadPool.remove(longword(Thr));
    Dec(OpenCnt);
    if OpenCnt=0 then CSOpen.SetEvent;
  finally
    CSThread.Leave;
    end;
  end;

procedure CloseThreadPool;
  var
    Work:TRtcWorkerThread;
    wrk,i:longword;
    havetowait:boolean;
    haveto_removecallbacks:boolean;
  begin
  havetowait:=False;

  CSThread.Enter;
  try
    haveto_removecallbacks:=InsideCallback=0;

    if assigned(ThreadPool) then
      begin
      havetowait:=True;
      while ThreadPool.Count>0 do
        begin
        wrk:=ThreadPool.search_min(i);
        ThreadPool.remove(wrk);
        Work:=TRtcWorkerThread(wrk);

        Work.PostQuit;
        end;

      Garbage(ThreadPool);
      Garbage(FreePool);
      end;

    // ------------
    if assigned(ThrList) then
      begin
      havetowait:=True;

      Garbage(ThrList);
      Garbage(WaitList);
      Garbage(WorkList);
      Garbage(Message_Quit);
      end;
    // ------------
  finally
    CSThread.Leave;
    end;

  if havetowait then CSOpen.WaitFor(10000);

  if haveto_removecallbacks then
    RemoveThreadCallbacks;

  Sleep(10);
  end;

{ TRtcThread }

constructor TRtcThread.Create;
  begin
  inherited;
  MsgList:=TXList.Create(128);
  FInfo:=TRtcInfo.Create;

  CSThread.Enter;
  try
    if not assigned(ThrList) then
      begin
      Message_Quit:=TRtcQuitMessage.Create;

      ThrList:=tBinList.Create(128);
      WaitList:=tBinList.Create(128);
      WorkList:=tBinList.Create(128);
      Thr_LastExec:=0;
      end;

    ThrList.insert(longword(self),1);
  finally
    CSThread.Leave;
    end;
  end;

destructor TRtcThread.Destroy;
  var
    o:TObject;
  begin
  CSThread.Enter;
  try
    if assigned(ThrList) then
      if ThrList.search(longword(self))>0 then
        begin
        if WaitList.search(longword(self))>0 then
          WaitList.remove(longword(self));
        if WorkList.search(longword(self))>0 then
          WorkList.remove(longword(self));
        ThrList.remove(longword(self));
        end;
  finally
    CSThread.Leave;
    end;

  while MsgList.Count>0 do
    begin
    o:=TObject(MsgList.Last);
    MsgList.removeLast;
    if o<>Message_Quit then
      Kill(o);
    end;
  MsgList.Free;

  FInfo.Free;

  inherited;
  end;

class function TRtcThread.Lock(me: TObject): boolean;
  begin
  Result:=False;
  CSThread.Enter;
  try
    if assigned(ThrList) then
      if ThrList.search(longword(me))>0 then
        Result:=True;
  finally
    if not Result then
      CSThread.Leave;
    end;
  end;

class procedure TRtcThread.UnLock;
  begin
  CSThread.Leave;
  end;

class procedure TRtcThread.Stop(me:TObject);
  begin
  CSThread.Enter;
  try
    if assigned(ThrList) then
      if ThrList.search(longword(me))>0 then
        TRtcThread.PostJob(me,Message_Quit,True,True);
  finally
    CSThread.Leave;
    end;
  end;

class function TRtcThread.PostJob(me:TObject; Job: TObject; HighPriority:boolean=False; AutoResume:boolean=False):boolean;
  var
    MyThr:TRtcWorkerThread;
    o:TObject;
  begin
  Result:=False;
  CSThread.Enter;
  try
    if assigned(ThrList) then
      begin
      if ThrList.search(longword(me))>0 then
        with TRtcThread(me) do begin
        if not Quitting then
          begin
          Result:=True;

          if Job=Message_Quit then
            begin
            while MsgList.Count>0 do
              begin
              o:=TObject(MsgList.Last);
              MsgList.removeLast;
              if o<>Message_Quit then
                Kill(o);
              end;
            end;

          if HighPriority then
            MsgList.addFirst(longword(Job))
          else
            MsgList.addLast(longword(Job));

          if AutoResume then
            Paused:=False;

          if not Paused then
            if PutWork(me) then // Post Thread to waiting list. If thread was idle, start executing next waiting thread.
              begin
              myThr:=GetThread;
              if assigned(myThr) then
                myThr.PostWork(GetWork);
              end;
          end;
        end;
      end;
  finally
    CSThread.Leave;
    end;
  end;

class function TRtcThread.GetJob(me:TObject): TObject;
  begin
  CSThread.Enter;
  try
    if assigned(ThrList)then
      begin
      if ThrList.search(longword(me))>0 then
        with TRtcThread(me) do begin
        if MsgList.Count>0 then
          begin
          Result:=TObject(MsgList.First);
          MsgList.removeFirst;
          end
        else
          Result:=nil;
        end
      else
        Result:=nil;
      end
    else
      Result:=nil;
  finally
    CSThread.Leave;
    end;
  end;

procedure TRtcThread.Pause;
  begin
  CSThread.Enter;
  try
    if assigned(ThrList) then
      if ThrList.search(longword(self))>0 then
        if not Paused and not Quitting then
          begin
          Paused:=True;
          if (WorkList.search(longword(self))=0) then // not currently working
            if WaitList.search(longword(self))>0 then // waiting for execution
              WaitList.remove(longword(self)); //remove from waiting list
          end;
  finally
    CSThread.Leave;
    end;
  end;

procedure TRtcThread.Resume;
  var
    myThr:TRtcWorkerThread;
  begin
  CSThread.Enter;
  try
    if assigned(ThrList) then
      if ThrList.search(longword(self))>0 then
        if Paused then
          begin
          Paused:=False;
          if (WorkList.search(longword(self))=0) then // not currently working
            if MsgList.Count>0 then
              if PutWork(self) then // add to waiting list
                begin
                myThr:=GetThread;
                if assigned(myThr) then
                  myThr.PostWork(GetWork);
                end;
          end;
  finally
    CSThread.Leave;
    end;
  end;

procedure TRtcThread.Idle;
  begin
  CSThread.Enter;
  try
    DoneWork(self);
    if not Paused and (MsgList.Count>0) then
      PutWork(self);
  finally
    CSThread.Leave;
    end;
  end;

procedure TRtcThread.Sync(Event: TRtcSyncEvent);
  begin
  if assigned(FThr) then
    TRtcWorkerThread(FThr).Sync(Event)
  else
    raise Exception.Create('No thread assigned.');
  end;

procedure TRtcThread.Run(Thr:TRtcWorkerThread);
  var
    Job:TObject;
    freed:boolean;
  begin
  Job:=GetJob(self);

  if Job=nil then
    Exit
  else if Job=Message_Quit then
    begin
    try
      Free;
    except
      on E:Exception do
        if LOG_THREAD_EXCEPTIONS then
          Log('RtcThread.Free',E);
      end;
    end
  else
    begin
    try
      freed:=Work(Job); // Work() has to free the 'Job' object.
    except
      on E:Exception do
        begin
        if LOG_THREAD_EXCEPTIONS then
          try
            Log('Work('+Job.ClassName+')',E);
          except
            Log('Work(undefined_Job)',E);
            end;
        try
          Free;
        except
          on E:Exception do
            if LOG_THREAD_EXCEPTIONS then
              Log('RtcThread.Free',E);
          end;
        Exit;
        end;
      end;
    try
      if not freed then Idle;
    except
      on E:Exception do
        if LOG_THREAD_EXCEPTIONS then
          Log('RtcThread.Idle',E);
      end;
    end;
  end;

function TRtcThread.Work(Job: TObject):boolean;
  begin
  Result:=False;
  if Job is TRtcJob then
    TRtcJob(Job).Run(self)
  else
    raise Exception.Create('Error!! Unknown Job class: '+Job.ClassName);
  end;

procedure TRtcThread.Kill(Job: TObject);
  begin
  if Job is TRtcJob then
    TRtcJob(Job).Kill;
  end;

function TRtcThread.ThreadID: Cardinal;
  begin
  if assigned(FThr) then
    Result:=FThr.ThreadID
  else
    Result:=0;
  end;

{ TRtcWorkerThread }

constructor TRtcWorkerThread.Create(CreateSuspended: boolean);
  begin
  CSThread.Enter;
  try
    Inc(OpenCnt);
    if OpenCnt=1 then
      CSOpen.ResetEvent;
  finally
    CSThread.Leave;
    end;

  inherited Create(True);
  Priority:=RTC_THREAD_PRIORITY;
  FreeOnTerminate:=False;
  Run:=TRtcEvent.Create(False,False);
  if not CreateSuspended then Resume;
  end;

destructor TRtcWorkerThread.Destroy;
  begin
  Run.Free;
  inherited;
  end;

procedure TRtcWorkerThread.Execute;
  begin
  try
    DoAfterThreadStart;
  except
    end;

  try
    while Run.WaitFor(INFINITE)=wr_Signaled do
      begin
      if Msg then
        begin
        Msg:=False;
        try
          Work.Run(self);
        except
          on E:Exception do
            if LOG_THREAD_EXCEPTIONS then
              Log('Work.Run',E);
            // ignore exceptions (do not want to kill this thread)
          end;
        if not ReturnThread(self) then
          Break;
        end
      else
        Break;
      end;
  except
    on E:Exception do
      if LOG_THREAD_EXCEPTIONS then
        Log('RtcWorkThread.Execute',E);
    end;
  ClosingThread(self);

  try
    DoBeforeThreadStop;
  except
    end;
  end;

procedure TRtcWorkerThread.MySyncEvent;
  begin
  FEvent;
  end;

procedure TRtcWorkerThread.PostWork(Thr: TRtcThread);
  begin
  Msg:=True;
  Work:=Thr;
  Work.FThr:=self;
  Run.SetEvent;
  end;

procedure TRtcWorkerThread.PostQuit;
  begin
  Run.SetEvent;
  end;

procedure TRtcWorkerThread.Sync(Event: TRtcSyncEvent);
  begin
  FEvent:=Event;
  {$IFDEF FPC}
  Synchronize(@MySyncEvent);
  {$ELSE}
  Synchronize(MySyncEvent);
  {$ENDIF}
  end;

initialization
{$IFDEF CLR}
MainThr := System.Threading.Thread.CurrentThread;
{$ELSE}
MainThrID:=GetCurrentThreadID;
{$ENDIF}

ThreadCallbackCount:=0;
SetLength(ThreadCallbacks,0);
InsideCallback:=0;

CSThread:=TRtcCritSec.Create;
OpenCnt:=0;
CSOpen:=TRtcEvent.Create(True,True);

finalization
CloseThreadPool;

Garbage(CSOpen);
Garbage(CSThread);
end.
