{ @exclude }
unit rtcService;

{$include rtcDefs.inc}

interface

uses
  Windows,
  WinSvc,
  SysUtils;

function IsServiceStarting(const ServiceName:string):boolean;

function IsDesktopMode(const ServiceName:string):boolean;

function IsMiniMode:boolean;

implementation

function IsServiceStarting(const ServiceName:string): Boolean;
  var
    Svc: Integer;
    SvcMgr: Integer;
    ServSt: TServiceStatus;
  begin
  Result := False;
  SvcMgr := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if SvcMgr = 0 then Exit;
  try
    Svc := OpenService (SvcMgr, PChar(ServiceName), SERVICE_QUERY_STATUS);
    if Svc = 0 then Exit;
    try
      if not QueryServiceStatus (Svc, ServSt) then Exit;
      Result := (ServSt.dwCurrentState = SERVICE_START_PENDING);
    finally
      CloseServiceHandle(Svc);
      end;
  finally
    CloseServiceHandle(SvcMgr);
    end;
  end;

function IsDesktopMode(const ServiceName:string):boolean;
  begin
  if (Win32Platform <> VER_PLATFORM_WIN32_NT) then
    Result := True
  else
    begin
    Result := not FindCmdLineSwitch('INSTALL', ['-', '/'], True) and
              not FindCmdLineSwitch('UNINSTALL', ['-', '/'], True) and
              not IsServiceStarting(ServiceName);
    end;
  end;

function IsMiniMode:boolean;
  begin
  Result:=FindCmdLineSwitch('M',['-','/'],True);
  end;

end.
