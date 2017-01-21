{
  @html(<b>)
  ISAPI Application Component
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  Partial Copyright (c) Borland Inc.
  - TApplication interface compatibility
  - Exception Handling
  - Component creation

  @exclude
}
unit rtcISAPIApp;

{$INCLUDE rtcDefs.inc}

interface

uses
  Windows, Classes, SysUtils,
  ComObj, ActiveX, Isapi2,

  rtcSyncObjs;

type
  TRtcISAPIApplication = class(TComponent)
  private
    FTitle: string;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateForm(InstanceClass: TComponentClass; var Reference); virtual;
    procedure Initialize; virtual;
    procedure Run; virtual;

    // ISAPI entry points ->
    function GetExtensionVersion(var Ver: THSE_VERSION_INFO): BOOL;
    function HttpExtensionProc(var ECB: TEXTENSION_CONTROL_BLOCK): DWORD;
    function TerminateExtension(dwFlags: DWORD): BOOL;
    // <- ISAPI entry points

    property Title: string read FTitle write FTitle;
  end;

  THandleShutdownException = procedure(E: Exception);

var
  HandleShutdownException: THandleShutdownException = nil;
  Application: TRtcISAPIApplication = nil;

function GetExtensionVersion(var Ver: THSE_VERSION_INFO): BOOL; stdcall; export;
function HttpExtensionProc(var ECB: TEXTENSION_CONTROL_BLOCK): DWORD; stdcall; export;
function TerminateExtension(dwFlags: DWORD): BOOL; stdcall; export;

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

implementation

uses
  rtcISAPISrv;

// ISAPI interface

function GetExtensionVersion(var Ver: THSE_VERSION_INFO): BOOL;
  begin
  Result := Application.GetExtensionVersion(Ver);
  end;

function HttpExtensionProc(var ECB: TEXTENSION_CONTROL_BLOCK): DWORD;
  begin
  Result := Application.HttpExtensionProc(ECB);
  end;

function TerminateExtension(dwFlags: DWORD): BOOL;
  begin
  Result := Application.TerminateExtension(dwFlags);
  end;

{ TRtcISAPIApplication }

type
  TDLLProc = procedure (Reason: Integer);

var
  OldDllProc: TDLLProc;
  rtcLoaded:boolean=False;

procedure DoneVCLApplication;
  begin
  try
    Application.Free;
    Application := nil;
  except
    on E:Exception do
      if Assigned(HandleShutdownException) then
        begin
        Application := nil;
        // Classes.ApplicationHandleException := nil;
        HandleShutdownException(E);
        end;
    end;
  end;

procedure DLLExitProc(Reason: Integer);
  begin
{$IFDEF MSWINDOWS}
  if Reason = DLL_PROCESS_DETACH then
    DoneVCLApplication;
{$ENDIF}
  if Assigned(OldDllProc) then
    OldDllProc(Reason);
  end;

procedure HandleServerException(E: Exception; var ECB: TEXTENSION_CONTROL_BLOCK);
  var
    ResultText,
    ResultHeaders: string;
    Size: DWORD;
  begin
  ResultText := '<html><h1>Internal Server Error</h1><br>'+
                E.ClassName+': '+E.Message;
  Size := Length(ResultText);

  ECB.dwHTTPStatusCode := 500;
  ResultHeaders := 'Content-Type: text/html'#13#10 +
                   'Content-Length: '+IntToStr(length(ResultText))+#13#10+
                   #13#10;

  ECB.ServerSupportFunction(ECB.ConnID, HSE_REQ_SEND_RESPONSE_HEADER,
                            PChar('500 ' + E.Message), @Size, LPDWORD(ResultHeaders));

  ECB.WriteClient(ECB.ConnID, Pointer(ResultText), Size, 0);
  end;

constructor TRtcISAPIApplication.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  if IsLibrary then
    begin
    IsMultiThread := True;
    OldDllProc := DLLProc;
    DLLProc := @DLLExitProc;
    end
  else
    AddExitProc(DoneVCLApplication);
  end;

destructor TRtcISAPIApplication.Destroy;
  begin
  if rtcLoaded then
    begin
    rtcLoaded:=False;
    TRtcISAPIServer.UnLoad;
    end;
  inherited;
  end;

function TRtcISAPIApplication.GetExtensionVersion(var Ver: THSE_VERSION_INFO): BOOL;
  begin
  try
    Ver.dwExtensionVersion := MakeLong(HSE_VERSION_MINOR, HSE_VERSION_MAJOR);
    StrLCopy(Ver.lpszExtensionDesc, PChar(Title), HSE_MAX_EXT_DLL_NAME_LEN);
    Integer(Result) := 1;
  except
    Result := False;
    end;
  end;

function TRtcISAPIApplication.HttpExtensionProc(var ECB: TEXTENSION_CONTROL_BLOCK): DWORD;
  begin
  try
    Result:=TRtcISAPIServer.HttpExtensionProc(ECB);
    if Result=HSE_STATUS_ERROR then
      raise Exception.Create('TRtcISAPIServer.HttpExtensionProc() returned with STATUS_ERROR.<br>'#13#10+
                             'Please check if you have created the TDataModule with one TRtcISAPIServer component.');
  except
    if ExceptObject is Exception then
      HandleServerException(Exception(ExceptObject), ECB);
    Result := HSE_STATUS_ERROR;
    end;
  end;

function TRtcISAPIApplication.TerminateExtension(dwFlags: DWORD): BOOL;
  begin
  if rtcLoaded then
    begin
    rtcLoaded:=False;
    TRtcISAPIServer.UnLoad;
    end;
  Integer(Result) := 1;
  end;

procedure TRtcISAPIApplication.CreateForm(InstanceClass: TComponentClass; var Reference);
  var
    Instance: TComponent;
  begin
  Instance := TComponent(InstanceClass.NewInstance);
  TComponent(Reference) := Instance;
  try
    Instance.Create(Self);
  except
    TComponent(Reference) := nil;
    raise;
    end;
  end;

procedure TRtcISAPIApplication.Initialize;
  begin
  if InitProc <> nil then TProcedure(InitProc);
  end;

procedure TRtcISAPIApplication.Run;
  begin
  TRtcISAPIServer.Load;
  rtcLoaded:=True;
  end;

procedure InitApplication;
  begin
  CoInitFlags := COINIT_MULTITHREADED;
  Application := TRtcISAPIApplication.Create(nil);
  end;

initialization
InitApplication;
end.
