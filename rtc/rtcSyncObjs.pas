{
  "SyncObjcs" - Copyright (c) Danijel Tkalcec
  @exclude
}
unit rtcSyncObjs;

interface

{$INCLUDE rtcDefs.inc}

uses
{$IFDEF CLR}
  System.Threading;
{$ELSE}
  SyncObjs,
  Windows;
{$ENDIF}

type
  TRtcWaitResult = (wr_Signaled, wr_Timeout, wr_Abandoned, wr_Error);

  TRtcCritSec=class
  private
    {$IFDEF CLR}
    CS:System.Threading.Mutex;
    {$ELSE}
    CS:TRTLCriticalSection;
    {$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Enter;
    procedure Leave;
    end;

  TRtcRWSec=class
  private
    WriteSec:TEvent;
    {$IFDEF CLR}
    PassSec,ReadSec:System.Threading.Mutex;
    {$ELSE}
    PassSec,ReadSec:TRTLCriticalSection;
    {$ENDIF}
    Cnt,Cnt3:longint;
  public
    constructor Create;
    destructor Destroy; override;

    procedure EnterRead;  // Normal reader, no hurry.
    procedure LeaveRead;

    procedure ForceWrite;  // Need to write as fast as possible, force readers to stop reading.
    procedure EnterWrite;  // Normal writer, no hurry.
    procedure LeaveWrite;

    procedure ForceRead;  // Need to read as fast as possible, ignore waiting writers.
    procedure DoneRead;  // Done reading.
  end;

  TRtcEvent=class
  private
    CS:TEvent;
  public
    constructor Create(ManualReset,InitialState:boolean);
    destructor Destroy; override;

    function WaitFor(Timeout: LongWord): TRtcWaitResult;
    procedure SetEvent;
    procedure ResetEvent;
    end;

implementation

{ TRtcCritSec }

constructor TRtcCritSec.Create;
  begin
  inherited;
  {$IFDEF CLR}
  CS:=System.Threading.Mutex.Create;
  {$ELSE}
  InitializeCriticalSection(CS);
  {$ENDIF}
  end;

destructor TRtcCritSec.Destroy;
  begin
  {$IFDEF CLR}
  CS.Free;
  {$ELSE}
  DeleteCriticalSection(CS);
  {$ENDIF}
  inherited;
  end;

procedure TRtcCritSec.Enter;
  begin
  {$IFDEF CLR}
  CS.WaitOne;
  {$ELSE}
  EnterCriticalSection(CS);
  {$ENDIF}
  end;

procedure TRtcCritSec.Leave;
  begin
  {$IFDEF CLR}
  CS.ReleaseMutex;
  {$ELSE}
  LeaveCriticalSection(CS);
  {$ENDIF}
  end;

{ TRtcEvent }

constructor TRtcEvent.Create(ManualReset, InitialState: boolean);
  begin
  inherited Create;
  CS:=TEvent.Create(nil,ManualReset,InitialState,'');
  end;

destructor TRtcEvent.Destroy;
  begin
  CS.Free;
  inherited;
  end;

procedure TRtcEvent.ResetEvent;
  begin
  CS.ResetEvent;
  end;

procedure TRtcEvent.SetEvent;
  begin
  CS.SetEvent;
  end;

function TRtcEvent.WaitFor(Timeout: LongWord): TRtcWaitResult;
  begin
  case CS.WaitFor(Timeout) of
    wrSignaled: Result:=wr_Signaled;
    wrTimeout:  Result:=wr_Timeout;
    wrAbandoned:Result:=wr_Abandoned;
    else        Result:=wr_Error;
    end;
  end;

{ TRtcRWSec }

constructor TRtcRWSec.Create;
  begin
  inherited;
  Cnt:=0;Cnt3:=0;
  {$IFDEF CLR}
  PassSec:=System.Threading.Mutex.Create;
  ReadSec:=System.Threading.Mutex.Create;
  {$ELSE}
  InitializeCriticalSection(PassSec);
  InitializeCriticalSection(ReadSec);
  {$ENDIF}
  WriteSec:=TEvent.Create(nil,False,True,'');  // Auto-reset
  end;

destructor TRtcRWSec.Destroy;
  begin
  {$IFDEF CLR}
  PassSec.Free;
  ReadSec.Free;
  {$ELSE}
  DeleteCriticalSection(PassSec);
  DeleteCriticalSection(ReadSec);
  {$ENDIF}
  WriteSec.Free;
  inherited;
  end;

procedure TRtcRWSec.EnterRead;
  begin
  {$IFDEF CLR}
  PassSec.WaitOne;
  PassSec.ReleaseMutex;
  {$ELSE}
  EnterCriticalSection(PassSec);
  LeaveCriticalSection(PassSec);
  {$ENDIF}

  {$IFDEF CLR}
  ReadSec.WaitOne;
  {$ELSE}
  EnterCriticalSection(ReadSec);
  {$ENDIF}
  try
    if (Cnt=0) and (Cnt3=0) then // There are no readers inside
      WriteSec.WaitFor(INFINITE);  // Block all writers, this is the first reader.
    Inc(Cnt);
  finally
    {$IFDEF CLR}
    ReadSec.ReleaseMutex;
    {$ELSE}
    LeaveCriticalSection(ReadSec);
    {$ENDIF}
    end;
  end;

procedure TRtcRWSec.ForceRead;
  var
    OK:boolean;
  begin
  OK:=False;
  {$IFDEF CLR}
  ReadSec.WaitOne;
  {$ELSE}
  EnterCriticalSection(ReadSec);
  {$ENDIF}
  try
    if Cnt>0 then // There are normal readers inside, writers are blocked.
      begin
      Inc(Cnt3);
      OK:=True;
      end;
  finally
    {$IFDEF CLR}
    ReadSec.ReleaseMutex;
    {$ELSE}
    LeaveCriticalSection(ReadSec);
    {$ENDIF}
    end;

  if not OK then
    begin
    {$IFDEF CLR}
    PassSec.WaitOne;
    PassSec.ReleaseMutex;
    {$ELSE}
    EnterCriticalSection(PassSec);
    LeaveCriticalSection(PassSec);
    {$ENDIF}

    {$IFDEF CLR}
    ReadSec.WaitOne;
    {$ELSE}
    EnterCriticalSection(ReadSec);
    {$ENDIF}
    try
      if (Cnt=0) and (Cnt3=0) then // There are no readers inside
        WriteSec.WaitFor(INFINITE);  // Block all writers
      Inc(Cnt3);
    finally
      {$IFDEF CLR}
      ReadSec.ReleaseMutex;
      {$ELSE}
      LeaveCriticalSection(ReadSec);
      {$ENDIF}
      end;
    end;
  end;

procedure TRtcRWSec.LeaveRead;
  begin
  {$IFDEF CLR}
  ReadSec.WaitOne;
  {$ELSE}
  EnterCriticalSection(ReadSec);
  {$ENDIF}
  try
    Dec(Cnt);
    if (Cnt=0) and (Cnt3=0) then
      WriteSec.SetEvent;  // Un-block writers
  finally
    {$IFDEF CLR}
    ReadSec.ReleaseMutex;
    {$ELSE}
    LeaveCriticalSection(ReadSec);
    {$ENDIF}
    end;
  end;

procedure TRtcRWSec.DoneRead;
  begin
  {$IFDEF CLR}
  ReadSec.WaitOne;
  {$ELSE}
  EnterCriticalSection(ReadSec);
  {$ENDIF}
  try
    Dec(Cnt3);
    if (Cnt=0) and (Cnt3=0) then
      WriteSec.SetEvent;  // Un-block writers
  finally
    {$IFDEF CLR}
    ReadSec.ReleaseMutex;
    {$ELSE}
    LeaveCriticalSection(ReadSec);
    {$ENDIF}
    end;
  end;

procedure TRtcRWSec.EnterWrite;
  begin
  {$IFDEF CLR}
  PassSec.WaitOne;
  {$ELSE}
  EnterCriticalSection(PassSec);
  {$ENDIF}

  WriteSec.WaitFor(INFINITE);
  end;

procedure TRtcRWSec.ForceWrite;
  begin
  {$IFDEF CLR}
  PassSec.WaitOne;
  {$ELSE}
  EnterCriticalSection(PassSec);
  {$ENDIF}

  WriteSec.WaitFor(INFINITE);
  end;

procedure TRtcRWSec.LeaveWrite;
  begin
  WriteSec.SetEvent;

  {$IFDEF CLR}
  PassSec.ReleaseMutex;
  {$ELSE}
  LeaveCriticalSection(PassSec);
  {$ENDIF}
  end;

end.
