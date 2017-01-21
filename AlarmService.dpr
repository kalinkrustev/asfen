program AlarmService;

{%File 'FastMM\FastMM4Options.inc'}

uses
  FastMM4 in 'FastMM\FastMM4.pas',
  FastMM4Messages in 'FastMM\FastMM4Messages.pas',
  ErrorLog,
  SvcMgr,
  SysUtils,
  Windows,
  WinSvc,
  Forms,
  dm_Service in 'dm_Service.pas' {dmService: TService},
  dm_FixedConnPool in 'dm_FixedConnPool.pas' {dmFixedConnPool: TDataModule},
  SIP_Call in 'SIP_Call.pas',
  PJSUA in 'PJSUA.PAS',
  SIP_Action in 'SIP_Action.pas',
  SIP_Script in 'SIP_Script.pas',
  Log4D in 'log4d\Log4D.pas',
  Logger in 'Logger.pas',
  SIP_Thread in 'SIP_Thread.pas',
  dm_Alarm in 'dm_Alarm.pas' {dmAlarm: TDataModule},
  SIP_Library in 'SIP_Library.pas',
  Util in 'Util.pas',
  SIP_Sound in 'SIP_Sound.pas',
  SIP_App in 'SIP_App.pas',
  SIP_RingList in 'SIP_RingList.pas',
  SIP_Status in 'SIP_Status.pas',
  SIP_QueueManager in 'SIP_QueueManager.pas',
  SIP_Event in 'SIP_Event.pas',
  SIP_Env in 'SIP_Env.pas',
  SMS_Queue in 'SMS_Queue.pas',
  AdGSM in 'asyncpro\AdGSM.pas',
  Mail_Queue in 'Mail_Queue.pas',
  Event_RS232 in 'Event_RS232.pas',
  frm_Service in 'frm_Service.pas' {frmService},
  pcre in 'pcre\pcre.pas',
  PerlRegEx in 'pcre\PerlRegEx.pas',
  SIP_Monitor in 'SIP_Monitor.pas';

{$R *.RES}

function StartService: Boolean;
var
  Mgr,
  Svc: SC_HANDLE;
  UserName,
  ServiceStartName	: string;
  Config: Pointer;
  Size: DWord;
begin
  Result := False;
  Mgr := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if Mgr <> 0 then
  begin
    Svc := OpenService(Mgr, PChar(ServiceName), SERVICE_ALL_ACCESS);
    Result := Svc <> 0;
    if Result then
    begin
      QueryServiceConfig(Svc, nil, 0, Size);
      Config := AllocMem(Size);
      try
        QueryServiceConfig(Svc, Config, Size, Size);
        ServiceStartName := PQueryServiceConfig(Config)^.lpServiceStartName;
        if CompareText(ServiceStartName, 'LocalSystem') = 0 then
          ServiceStartName := 'SYSTEM';
      finally
        Dispose(Config);
      end;
      CloseServiceHandle(Svc);
    end;
    CloseServiceHandle(Mgr);
  end;
  if Result then
  begin
    Size := 256;
    SetLength(UserName, Size);
    GetUserName(PChar(UserName), Size);
    SetLength(UserName, StrLen(PChar(UserName)));
    Result := CompareText(UserName, ServiceStartName) = 0;
  end;
end;

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  //ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  ChDir(ExtractFilePath(ParamStr(0)));
  if SvcMgr.Application.Installing or StartService then
  begin
    dm_Alarm.Interactive:=False;
    if not SvcMgr.Application.DelayInitialize or SvcMgr.Application.Installing then
      SvcMgr.Application.Initialize;
    if not SvcMgr.Application.Installing then SvcMgr.Application.CreateForm(TdmAlarm, dmAlarm);
  SvcMgr.Application.CreateForm(TdmService, dmService);
    SvcMgr.Application.Run;
  end else
  begin
    dm_Alarm.Interactive:=True;
    //Forms.Application.ShowMainForm := False;
    SvcMgr.Application.Title := 'ASOS';
    Forms.Application.Initialize;
    Forms.Application.UpdateFormatSettings:=False;
    Forms.Application.ShowMainForm:=False;
    SvcMgr.Application.CreateForm(TdmAlarm, dmAlarm);
    SvcMgr.Application.CreateForm(TfrmService, frmService);
    Forms.Application.Run;
  end;
end.
