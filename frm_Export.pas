unit frm_Export;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TfrmExport = class(TForm)
    Panel1: TPanel;
    Organisation: TCheckBox;
    Settings: TCheckBox;
    Panel2: TPanel;
    Users: TCheckBox;
    Stats: TCheckBox;
    OrganisationName: TCheckBox;
    Address1: TCheckBox;
    PersonName: TCheckBox;
    Address2: TCheckBox;
    Site: TCheckBox;
    RingGroupName: TCheckBox;
    MessageName: TCheckBox;
    ScriptName: TCheckBox;
    EventName: TCheckBox;
    Panel: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    procedure btnOkClick(Sender: TObject);
    procedure SettingsClick(Sender: TObject);
    procedure OrganisationClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure DisablePanel(C:TWinControl);
    procedure EnablePanel(C:TWinControl);
  public
    { Public declarations }
  end;

var
  frmExport: TfrmExport;

implementation

uses dm_AlarmDB, dm_GUI;

{$R *.dfm}

procedure TfrmExport.btnOkClick(Sender: TObject);
var S:String;

  procedure Check(C:array of TCheckBox);
  var I:Integer;
  begin
    for I := Low(C) to High(C) do
       if C[I].Checked then S:=S+C[I].Name+'|';
  end;
begin
  S:='|';
  if Organisation.Checked then
    Check([OrganisationName,Address1,PersonName,Address2,Site]);
  if Settings.Checked then
    Check([RingGroupName,MessageName,ScriptName,EventName]);
  Panel.Enabled:=False;
  try
    dmAlarmDB.ExportData(Organisation.Checked,
                         Settings.Checked,
                         Users.Checked,
                         Stats.Checked,
                         S);
    ShowMessage('Експортът завърши успешно')
  finally
    Panel.Enabled:=True;
  end;
  ModalResult:=mrOk;
end;

procedure TfrmExport.FormShow(Sender: TObject);
begin
  try
    btnOk.Enabled:=True;
    if not dmGUI.CanExport('Organisation') then
    begin
      Organisation.Checked:=False;
      Organisation.Enabled:=False;
    end else
      Organisation.Enabled:=True;
    if not dmGUI.CanExport('Settings') then
    begin
      Settings.Checked:=False;
      Settings.Enabled:=False;
    end else
      Settings.Enabled:=True;
    Users.Enabled:=dmGUI.CanExport('Users');
    if not Users.Enabled then Users.Checked:=False;
  except
    btnOk.Enabled:=False;
    raise;
  end;
end;

procedure TfrmExport.OrganisationClick(Sender: TObject);
begin
  if Organisation.Checked then
    EnablePanel(Panel1)
  else
    DisablePanel(Panel1);
end;

procedure TfrmExport.SettingsClick(Sender: TObject);
begin
  if Settings.Checked then
    EnablePanel(Panel2)
  else
    DisablePanel(Panel2);
end;

procedure TfrmExport.DisablePanel(C: TWinControl);
var I:Integer;
begin
  for I := 0 to C.ControlCount - 1 do
    if C.Controls[I] is TCheckBox then
    begin
      TCheckBox(C.Controls[I]).Checked:=False;
      TCheckBox(C.Controls[I]).Enabled:=False;
    end;
end;

procedure TfrmExport.EnablePanel(C: TWinControl);
var I:Integer;
begin
  for I := 0 to C.ControlCount - 1 do
    if C.Controls[I] is TCheckBox then
    begin
      TCheckBox(C.Controls[I]).Enabled:=True;
    end;
end;

end.
