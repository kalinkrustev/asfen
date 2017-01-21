unit frm_Settings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, XPStyleActnCtrls, ActnMan, ToolWin, ActnCtrls, StdCtrls,
  ExtCtrls, Grids, ValEdit;

type
  TfrmSettings = class(TForm)
    ActionToolBar1: TActionToolBar;
    ActionManager1: TActionManager;
    acSave: TAction;
    acClose: TAction;
    SettingsEditor: TValueListEditor;
    procedure acCloseExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SettingsEditorStringsChange(Sender: TObject);
  private
    FDataChanged: Boolean;
    procedure SetDataChanged(const Value: Boolean);

  public
    procedure LoadSettings;
    procedure SaveSettings;
    property DataChanged:Boolean read FDataChanged write SetDataChanged;
  end;

  var frmSettings:TfrmSettings;

implementation
uses dm_GUI,dm_AlarmDB, frm_Menu;
{$R *.dfm}

procedure TfrmSettings.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmSettings.acSaveExecute(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfrmSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TfrmSettings.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if DataChanged and acSave.Visible then
  case dmGUI.ConfirmExit of
    mrYes:
      begin
        acSave.Execute;
        CanClose:=True;
      end;
    mrNo:
      CanClose:=True;
    else
      CanClose:=False;
  end;
end;

procedure TfrmSettings.FormDestroy(Sender: TObject);
begin
  frmSettings:=nil;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  LoadSettings;
end;

procedure TfrmSettings.LoadSettings;
var I,J:Integer;
begin
  SettingsEditor.Strings.Clear;
  for I := 0 to High(Settings) do
  begin
    J:=SettingsEditor.Strings.Add(Settings[I][0]+'='+dmAlarmDB.GetIni(Settings[I][0]));
    SettingsEditor.ItemProps[J].KeyDesc:=Settings[I][1];
  end;
  DataChanged:=False;
end;

procedure TfrmSettings.SaveSettings;
var
  I: Integer;
begin
  for I := 1 to SettingsEditor.Strings.Count do  //skip title row
    dmAlarmDB.SetIni(SettingsEditor.Cells[0,I],SettingsEditor.Cells[1,I]);
  DataChanged:=False;
  frmMenu.InitTimer;
end;

procedure TfrmSettings.SetDataChanged(const Value: Boolean);
begin
  FDataChanged := Value;
  acSave.Enabled:=DataChanged;
end;

procedure TfrmSettings.SettingsEditorStringsChange(Sender: TObject);
begin
  DataChanged:=True;
end;

end.
