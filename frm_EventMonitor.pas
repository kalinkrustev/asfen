unit frm_EventMonitor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ActnMan, ActnCtrls, ActnList, XPStyleActnCtrls, DBCtrls, DB;

type
  TfrmEventMonitor = class(TForm)
    Events: TDBLookupListBox;
    ActionManager1: TActionManager;
    acClose: TAction;
    ActionToolBar1: TActionToolBar;
    acFireEvent: TAction;
    srcEvents: TDataSource;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acFireEventExecute(Sender: TObject);
    procedure EventsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmEventMonitor: TfrmEventMonitor;

implementation

uses dm_GUI, dm_AlarmDB;

{$R *.dfm}

procedure TfrmEventMonitor.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmEventMonitor.acFireEventExecute(Sender: TObject);
begin
  dmGUI.FireEvent(Events.KeyValue,dmAlarmDB.GetSetting(Settings[7][0]));
end;

procedure TfrmEventMonitor.EventsClick(Sender: TObject);
begin
  acFireEvent.Enabled:=Trim(VarToStr(Events.KeyValue))<>'';
end;

procedure TfrmEventMonitor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  dmGUI.EventList.Close;
  Action:=caFree;
end;

procedure TfrmEventMonitor.FormDestroy(Sender: TObject);
begin
  frmEventMonitor:=nil;
end;

procedure TfrmEventMonitor.FormShow(Sender: TObject);
begin
  dmGUI.EventList.Open;
end;

end.
