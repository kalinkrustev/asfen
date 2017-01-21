{
  "Threaded Connection Provider wrapper" - Copyright (c) Danijel Tkalcec
  @html(<br>)

  @exclude
}

unit rtcThrConnProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  Windows,

  rtcThrPool,

  rtcConnProv,
  rtcConnLimit;

type
  TRtcThrClientProvider = class(TRtcBasicClientProvider)
  protected
    function GetClientThread:TRtcThread; virtual; abstract;

  public
    procedure Release; override;

    function GetThread:TRtcThread; override;

    function inMainThread:boolean; override;
    function SyncEvent(Event:TRtcEvent):boolean; override;
    function inThread:boolean; override;

    function Pause:boolean; override;
    function Resume:boolean; override;

    function PostJob(Job:TObject; HighPriority:boolean):boolean; override;

    procedure Processing; override;

  (*** Methods that have to be implemented by the connection provider: *** ->

  protected
    procedure Enter; override;
    procedure Leave; override;

    function GetClientThread:TRtcThread; override;

  public
    procedure Connect(Force:boolean=False); override;
    procedure Disconnect; override;
    procedure InternalDisconnect; override;

    procedure Write(const s:string); override;
    function Read:string; override;

    <- *** end ***)
    end;

  TRtcNoThrClientProvider = class(TRtcBasicClientProvider)
  public
    function inMainThread:boolean; override;
    function SyncEvent(Event:TRtcEvent):boolean; override;
    function inThread:boolean; override;

    function GetThread:TRtcThread; override;

    function Pause:boolean; override;
    function Resume:boolean; override;

    function PostJob(Job:TObject; HighPriority:boolean):boolean; override;

    procedure Processing; override;

  (*** Methods that have to be implemented by the connection provider: *** ->

  protected
    procedure Enter; override;
    procedure Leave; override;

  public
    procedure Connect(Force:boolean=False); override;
    procedure Disconnect; override;
    procedure InternalDisconnect; override;

    procedure Write(const s:string); override;
    function Read:string; override;

    <- *** end ***)
    end;

  TRtcThrServerProvider = class(TRtcBasicServerProvider)
  protected
    function GetServerThread:TRtcThread; virtual; abstract;
    function GetClientThread:TRtcThread; virtual; abstract;

  public
    function SyncEvent(Event:TRtcEvent):boolean; override;

    function inMainThread:boolean; override;
    function inThread:boolean; override;

    function GetThread:TRtcThread; override;

    function Pause:boolean; override;
    function Resume:boolean; override;

    procedure Processing; override;

    function PostJob(Job:TObject; HighPriority:boolean):boolean; override;

  (*** Methods that have to be implemented by the connection provider: ***

  protected
    procedure Enter; override;
    procedure Leave; override;

    procedure CopyFrom(Dup:TRtcConnectionProvider);

    function GetClientThread:TRtcThread; override;
    function GetServerThread:TRtcThread; override;

  public
    procedure Listen; override;
    procedure Disconnect; override;
    procedure InternalDisconnect; override;

    function GetParent:TRtcConnectionProvider; override;

    procedure Write(const s:string); override;
    function Read:string; override;

  *** end ***)
    end;

  TRtcNoThrServerProvider = class(TRtcBasicServerProvider)
  public
    function SyncEvent(Event:TRtcEvent):boolean; override;

    function inMainThread:boolean; override;
    function inThread:boolean; override;

    function GetThread:TRtcThread; override;

    function Pause:boolean; override;
    function Resume:boolean; override;

    procedure Processing; override;

    function PostJob(Job:TObject; HighPriority:boolean):boolean; override;

  (*** Methods that have to be implemented by the connection provider: ***

  protected
    procedure Enter; override;
    procedure Leave; override;

    procedure CopyFrom(Dup:TRtcConnectionProvider);

  public
    procedure Listen; override;
    procedure Disconnect; override;
    procedure InternalDisconnect; override;

    function GetParent:TRtcConnectionProvider; override;

    procedure Write(const s:string); override;
    function Read:string; override;

  *** end ***)
    end;

implementation

{ TRtcThrClientProvider }

function TRtcThrClientProvider.inMainThread: boolean;
  begin
{$IFDEF CLR}
  Result := System.Threading.Thread.CurrentThread = MainThr;
{$ELSE}
  Result := GetCurrentThreadId=MainThrID;
{$ENDIF}
  end;

function TRtcThrClientProvider.inThread: boolean;
  begin
  if GetClientThread<>nil then
    Result:=GetClientThread.ThreadID=GetCurrentThreadId
  else if GetMultiThreaded then
    Result:=inMainThread
  else
    Result:=True;
  end;

function TRtcThrClientProvider.Pause: boolean;
  begin
  if GetClientThread<>nil then
    begin
    GetClientThread.Pause;
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcThrClientProvider.Resume: boolean;
  begin
  if GetClientThread<>nil then
    begin
    GetClientThread.Resume;
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcThrClientProvider.PostJob(Job: TObject; HighPriority: boolean): boolean;
  begin
  if GetClientThread<>nil then
    begin
    Result:=TRtcThread.PostJob(GetClientThread,Job,HighPriority);
    end
  else if (Job is TRtcJob) and not GetMultiThreaded {and inMainThread} then
    begin
    TRtcJob(Job).Run(nil);
    Result:=True;
    end
  else
    Result:=False;
  end;

procedure TRtcThrClientProvider.Processing;
  begin
  if RTC_LIMIT_CONN and (GetClientThread<>nil) then
    rtcCloseAction(GetClientThread);
  end;

function TRtcThrClientProvider.SyncEvent(Event: TRtcEvent): boolean;
  begin
  if assigned(Event) then
    begin
    if not GetMultiThreaded then
      begin
      Event;
      Result:=True;
      end
    else if inMainThread then
      begin
      Event;
      Result:=True;
      end
    else if GetClientThread<>nil then
      begin
      GetClientThread.Sync(Event);
      Result:=True;
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

procedure TRtcThrClientProvider.Release;
  begin
  Free;
  end;

function TRtcThrClientProvider.GetThread: TRtcThread;
  begin
  Result:=GetClientThread;
  end;

{ TRtcNoThrClientProvider }

function TRtcNoThrClientProvider.inMainThread: boolean;
  begin
{$IFDEF CLR}
  Result := System.Threading.Thread.CurrentThread = MainThr;
{$ELSE}
  Result := GetCurrentThreadId=MainThrID;
{$ENDIF}
  end;

function TRtcNoThrClientProvider.inThread: boolean;
  begin
  Result:=True; // inMainThread;
  end;

function TRtcNoThrClientProvider.Pause: boolean;
  begin
  Result:=False;
  end;

function TRtcNoThrClientProvider.Resume: boolean;
  begin
  Result:=False;
  end;

function TRtcNoThrClientProvider.PostJob(Job: TObject; HighPriority: boolean): boolean;
  begin
  if (Job is TRtcJob) then
    begin
    TRtcJob(Job).Run(nil);
    Result:=True;
    end
  else
    Result:=False;
  end;

procedure TRtcNoThrClientProvider.Processing;
  begin
  // nothing to do
  end;

function TRtcNoThrClientProvider.SyncEvent(Event: TRtcEvent): boolean;
  begin
  if assigned(Event) then
    begin
    if not GetMultiThreaded then
      begin
      Event;
      Result:=True;
      end
    else if inMainThread then
      begin
      Event;
      Result:=True;
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcNoThrClientProvider.GetThread: TRtcThread;
  begin
  Result:=nil;
  end;

{ TRtcThrServerProvider }

function TRtcThrServerProvider.inMainThread: boolean;
  begin
{$IFDEF CLR}
  Result := System.Threading.Thread.CurrentThread = MainThr;
{$ELSE}
  Result := GetCurrentThreadId=MainThrID;
{$ENDIF}
  end;

function TRtcThrServerProvider.inThread: boolean;
  begin
  if GetClientThread<>nil then
    Result:=GetClientThread.ThreadID=GetCurrentThreadId
  else if GetServerThread<>nil then
    Result:=GetServerThread.ThreadID=GetCurrentThreadId
  else if GetMultiThreaded then
    Result:=inMainThread
  else
    Result:=True;
  end;

function TRtcThrServerProvider.Pause: boolean;
  begin
  if GetClientThread<>nil then
    begin
    GetClientThread.Pause;
    Result:=True;
    end
  else if GetServerThread<>nil then
    begin
    GetServerThread.Pause;
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcThrServerProvider.Resume: boolean;
  begin
  if GetClientThread<>nil then
    begin
    GetClientThread.Resume;
    Result:=True;
    end
  else if GetServerThread<>nil then
    begin
    GetServerThread.Resume;
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcThrServerProvider.PostJob(Job: TObject; HighPriority: boolean): boolean;
  begin
  if GetClientThread<>nil then
    begin
    Result:=TRtcThread.PostJob(GetClientThread,Job,HighPriority);
    end
  else if GetServerThread<>nil then
    begin
    Result:=TRtcThread.PostJob(GetServerThread,Job,HighPriority);
    end
  else if (Job is TRtcJob) and not GetMultiThreaded {and inMainThread} then
    begin
    TRtcJob(Job).Run(nil);
    Result:=True;
    end
  else
    Result:=False;
  end;

procedure TRtcThrServerProvider.Processing;
  begin
  if RTC_LIMIT_CONN and (GetClientThread<>nil) then
    rtcCloseAction(GetClientThread);
  end;

function TRtcThrServerProvider.SyncEvent(Event: TRtcEvent): boolean;
  begin
  if assigned(Event) then
    begin
    if not GetMultiThreaded then
      begin
      Event;
      Result:=True;
      end
    else if inMainThread then
      begin
      Event;
      Result:=True;
      end
    else if GetClientThread<>nil then
      begin
      GetClientThread.Sync(Event);
      Result:=True;
      end
    else if GetServerThread<>nil then
      begin
      GetServerThread.Sync(Event);
      Result:=True;
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcThrServerProvider.GetThread: TRtcThread;
  begin
  Result:=GetClientThread;
  if not assigned(Result) then
    Result:=GetServerThread;
  end;

{ TRtcNoThrServerProvider }

function TRtcNoThrServerProvider.inMainThread: boolean;
  begin
{$IFDEF CLR}
  Result := System.Threading.Thread.CurrentThread = MainThr;
{$ELSE}
  Result := GetCurrentThreadId=MainThrID;
{$ENDIF}
  end;

function TRtcNoThrServerProvider.inThread: boolean;
  begin
  Result:=True; // inMainThread;
  end;

function TRtcNoThrServerProvider.Pause: boolean;
  begin
  Result:=False;
  end;

function TRtcNoThrServerProvider.Resume: boolean;
  begin
  Result:=False;
  end;

function TRtcNoThrServerProvider.PostJob(Job: TObject; HighPriority: boolean): boolean;
  begin
  if (Job is TRtcJob) then
    begin
    TRtcJob(Job).Run(nil);
    Result:=True;
    end
  else
    Result:=False;
  end;

procedure TRtcNoThrServerProvider.Processing;
  begin
  // nothing to do
  end;

function TRtcNoThrServerProvider.SyncEvent(Event: TRtcEvent): boolean;
  begin
  if assigned(Event) then
    begin
    if not GetMultiThreaded then
      begin
      Event;
      Result:=True
      end
    else if inMainThread then
      begin
      Event;
      Result:=True;
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcNoThrServerProvider.GetThread: TRtcThread;
  begin
  Result:=nil;
  end;

end.
