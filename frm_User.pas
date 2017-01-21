unit frm_User;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, DBCtrls, Mask, ExtCtrls, DB;

type
  TfrmUser = class(TForm)
    Panel4: TPanel;
    Label4: TLabel;
    UserName: TEdit;
    Panel5: TPanel;
    Label5: TLabel;
    Password: TEdit;
    Panel3: TPanel;
    Label3: TLabel;
    SpeedButton1: TSpeedButton;
    WorkGroup: TDBLookupComboBox;
    Panel1: TPanel;
    Panel2: TPanel;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnDelete: TBitBtn;
    srcWorkgroupList: TDataSource;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOkClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    CurrentUserID:Integer;
  public
    procedure LoadUser(UserID:Integer);
    procedure SaveUser;
    procedure DeleteUser;
  end;

procedure EditUser(UserID:Integer);

implementation

uses dm_GUI, frm_Workgroup;

{$R *.dfm}

procedure EditUser(UserID:Integer);
var F:TfrmUser;
begin
  F:=nil;
  dmGUI.ShowForm(TfrmUser,false,F,false);
  F.LoadUser(UserID);
  f.ShowModal;
end;

{ TfrmUser }

procedure TfrmUser.btnDeleteClick(Sender: TObject);
begin
  DeleteUser;
end;

procedure TfrmUser.btnOkClick(Sender: TObject);
begin
  SaveUser;
end;

procedure TfrmUser.DeleteUser;
begin
  if dmGUI.Confirm('Желаете ли изтриване на потребителя ?') and
     dmGUI.UserEdit.Locate('ID',CurrentUserID,[]) then
    dmGUI.UserEdit.Delete;
end;

procedure TfrmUser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
  dmGUI.UserList.Requery;
end;

procedure TfrmUser.FormCreate(Sender: TObject);
begin
  dmGUI.UserEdit.Open;
  dmGUI.WorkGroupList.Open;
end;

procedure TfrmUser.LoadUser(UserID: Integer);
begin
  CurrentUserID:=UserID;
  if dmGUI.UserEdit.Locate('ID',UserID,[]) then
  begin
    UserName.Text:=dmGUI.UserEdit.FieldByName('UserName').AsString;
    Password.Text:=dmGUI.UserEdit.FieldByName('Password').AsString;
    WorkGroup.KeyValue:=dmGUI.UserEdit.FieldByName('WorkGroup').Value;
    WorkGroup.Enabled:=UserID>1;
  end else
  begin
    UserName.Clear;
    Password.Clear;
    WorkGroup.KeyValue:=null;
  end;
end;

procedure TfrmUser.SaveUser;
begin
  if not dmGUI.UserEdit.Locate('ID',CurrentUserID,[]) then
  begin
    dmGUI.UserEdit.Append;
    dmGUI.UserEdit.FieldByName('ID').AsInteger:=CurrentUserID;
  end else
    dmGUI.UserEdit.Edit;
  dmGUI.UserEdit.FieldByName('UserName').AsString:=UserName.Text;
  dmGUI.UserEdit.FieldByName('Password').AsString:=Password.Text;
  dmGUI.UserEdit.FieldByName('WorkGroup').Value:=WorkGroup.KeyValue;
  dmGUI.UserEdit.Post;
end;

procedure TfrmUser.SpeedButton1Click(Sender: TObject);
begin
  dmGUI.ShowForm(TfrmWorkGroup,True,frmWorkGroup);
end;

end.
