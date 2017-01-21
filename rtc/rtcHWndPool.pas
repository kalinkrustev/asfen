{
  "Window Handle Pool" - Copyright (c) Danijel Tkalcec
  @html(<br>)

  This unit implements a pool of Window Handles, associated with their threads.
  Sicne creating Windows Handles takes a considerable ammount of processing time,
  using a Window handle pool will speed things up.

  @exclude
}
unit rtcHWndPool;

{$INCLUDE rtcDefs.inc}

interface

uses
  rtcTrashcan,

  Windows,
  Messages,
  SysUtils,
  Classes,

  rtcSyncObjs;

const
  RTC_HWND_MSG_CODES=100000;    // Number of message codes needed to separate messages from old and new handles

var
  RTC_HWND_CLASS_NAME:PAnsiChar='';      // Class Name to be used when creating windows

  RTC_WND_THREAD_PRIORITY:TThreadPriority=tpNormal;

procedure rtcInitMainHWND;
procedure rtcReleaseMainHWND;

function rtcGetHWND(Multi_Threaded:boolean):HWND;
procedure rtcReturnHWND(W:HWND);

function rtcGetNextMsgCode(W:HWND):longword;

implementation

type
  tHWndThread=class(TThread)
  public
    constructor Create(CreateSuspended:boolean);
    destructor Destroy; override;

    procedure Execute; override;
    end;

var
  MyHdl,MyHdl_MT:HWND;
  MyHdl_cnt, MyHdl_MT_cnt:integer;
  MyMsgCode:cardinal=0;
  CSHWND:TRtcCritSec;
  thr:THwndThread;

  Inside:TRtcEvent;

function RtcAllocateHWnd:HWND;
  begin
  Result := CreateWindowEx(WS_EX_TOOLWINDOW,
                           RTC_HWND_CLASS_NAME,
                           '',        { Window name   }
                           WS_POPUP,  { Window Style  }
                           0, 0,      { X, Y          }
                           0, 0,      { Width, Height }
                           0,         { hWndParent    }
                           0,         { hMenu         }
                           HInstance, { hInstance     }
                           nil);      { CreateParam   }
  SetWindowLong(Result,GWL_USERDATA, 0);
  end;

function RtcDeallocateHWnd(Wnd: HWND): boolean;
  begin
  Result := DestroyWindow(Wnd);
  end;

{ tWSocketThread -> will be catching all multithreaded socket messages }

constructor tHWndThread.Create(CreateSuspended: boolean);
  begin
  inherited Create(True);
  FreeOnTerminate:=True;
  Priority:=RTC_WND_THREAD_PRIORITY;
  if not CreateSuspended then Resume;
  end;

destructor tHWndThread.Destroy;
  begin
  Inside.SetEvent;
  inherited;
  end;

procedure tHWndThread.Execute;
  var
    MsgRec:TMsg;
  begin
  MyHdl_MT:=RtcAllocateHWnd;
  Inc(MyHdl_MT_cnt);
  try
    Inside.SetEvent;
    while GetMessage(MsgRec,0,0,0) do
      begin
      TranslateMessage(MsgRec);
      DispatchMessage(MsgRec);
      end;
  finally
    rtcReturnHWND(MyHdl_MT);
    end;
  end;

function rtcGetHWND(Multi_Threaded:boolean):HWND;
  begin
  CSHWND.Enter;
  try
    if Multi_Threaded then
      begin
      if myHdl_MT=0 then
        begin
        Inside:=TRtcEvent.Create(True,False);
        thr:=THWndThread.Create(False);
        Inside.WaitFor(INFINITE); // wait for the thread to start and create a thread window handle.
        end;
      Inc(MyHdl_MT_cnt);
      Result:=MyHdl_MT;
      end
    else
      begin
      if MyHdl=0 then
        MyHdl:=RtcAllocateHWnd;
      Inc(MyHdl_cnt);
      Result:=MyHdl;
      end;
  finally
    CSHWND.Leave;
    end;
  end;

procedure rtcInitMainHWND;
  begin
  CSHWND.Enter;
  try
    if MyHdl<>0 then
      RtcDeallocateHWnd(myHdl);
    MyHdl:=RtcAllocateHWnd;
    Inc(MyHdl_cnt);
  finally
    CSHWND.Leave;
    end;
  end;

procedure rtcReleaseMainHWND;
  begin
  CSHWND.Enter;
  try
    Dec(MyHdl_cnt);
    if (MyHdl_cnt=0) and (MyHdl<>0) then
      begin
      RtcDeallocateHWnd(myHdl);
      MyHdl:=0;
      end;
  finally
    CSHWND.Leave;
    end;
  end;

procedure rtcReturnHWND(W:HWND);
  begin
  CSHWND.Enter;
  try
    if W=myHdl_MT then
      begin
      Dec(MyHdl_MT_cnt);
      if myHdl_MT_cnt=0 then
        begin
        RtcDeallocateHWnd(myHdl_MT);
        myHdl_MT:=0;
        end;
      end
    else if W=myHdl then
      begin
      Dec(MyHdl_cnt);
      if myHdl_cnt=0 then
        begin
        RtcDeallocateHWnd(myHdl);
        myHdl:=0;
        end;
      end;
  finally
    CSHWND.Leave;
    end;
  end;

function rtcGetNextMsgCode(W:HWND):longword;
  begin
  CSHWND.Enter;
  try
    MyMsgCode:=MyMsgCode+1;
    if MyMsgCode>RTC_HWND_MSG_CODES then
      MyMsgCode:=1;
    Result:=MyMsgCode;
  finally
    CSHWND.Leave;
    end;
  end;

initialization

  MyHdl:=0; MyHdl_cnt:=0;
  MyHdl_MT:=0; MyHdl_MT_cnt:=0;
  MyMsgCode:=0;
  thr:=nil;

  CSHWND:=TRtcCritSec.Create;

finalization
if assigned(thr) then
  begin
  try
    // Stop background thread ...
    Inside.ResetEvent;
    PostThreadMessage(thr.ThreadID, WM_QUIT,0,0);
    Inside.WaitFor(10000); // wait up to 10 seconds for thread to close
    Sleep(10); // Allow the thread to terminate.
    Inside.Free;
  except
    end;
  end;
Garbage(CSHWND);
end.
