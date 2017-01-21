unit frm_Mode;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TfrmMode = class(TForm)
    ModeSelect: TRadioGroup;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMode: TfrmMode;

implementation

uses dm_AlarmDB;

{$R *.dfm}

procedure TfrmMode.btnOkClick(Sender: TObject);
begin
  if ModeSelect.ItemIndex>=0 then
    dmAlarmDB.SetSetting('mode',IntToStr(ModeSelect.ItemIndex));
end;

procedure TfrmMode.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  ClientHeight:=Length(AlarmModes)*30+Panel1.Height;
  ModeSelect.Items.Clear;
  for I := 0 to Length(AlarmModes) - 1 do
      ModeSelect.Items.Add(AlarmModes[I]);
end;

procedure TfrmMode.FormShow(Sender: TObject);
var Mode:Integer;
begin
  Mode:=StrToIntDef(dmAlarmDB.GetSetting('mode'),-1);
  if (Mode>=0) and (Mode< ModeSelect.Items.Count) then
    ModeSelect.ItemIndex:=Mode
  else
    ModeSelect.ItemIndex:=-1;
end;

end.
