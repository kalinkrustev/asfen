unit dm_Service;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs;

type
  TdmService = class(TService)
    procedure ServicePause(Sender: TService; var Paused: Boolean);
    procedure ServiceContinue(Sender: TService; var Continued: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceShutdown(Sender: TService);
  private
    procedure StopService;
  public
    procedure Loaded;override;
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  dmService: TdmService;

implementation
uses dm_Alarm, Logger, Log4D;

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  dmService.Controller(CtrlCode);
end;

function TdmService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TdmService.Loaded;
begin
  DisplayName := ServiceName;
  Name := ServiceName;
  inherited Loaded;
end;

procedure TdmService.ServiceContinue(Sender: TService; var Continued: Boolean);
begin
  if dmAlarm<>nil then
    dmAlarm.Continue_;
end;

procedure TdmService.ServicePause(Sender: TService; var Paused: Boolean);
begin
  if dmAlarm<>nil then
    dmAlarm.Pause;
end;

procedure TdmService.ServiceShutdown(Sender: TService);
begin
  StopService;
end;

procedure TdmService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  StopService;
end;

procedure TdmService.StopService;
begin
  dmAlarm.StartupTimer.Enabled:=False;
  dmAlarm.Timer1.Enabled:=False;
  if dmAlarm.HTTP<>nil then dmAlarm.HTTP.StopListen;
  FreeAndNil(dmAlarm);

  if Log<>nil then
  begin
    Log.Info('Stopping '+ParamStr(0));
    Log.Info('#####################################################');
    Log:=nil;
  end;

  DefaultHierarchy.Shutdown;
end;

end.
