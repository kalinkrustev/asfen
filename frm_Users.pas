unit frm_Users;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ActnList, XPStyleActnCtrls, ActnMan, ToolWin, ActnCtrls,
  ExtCtrls, Grids, DBGrids, StdCtrls, DBCtrls, Mask;

type
  TfrmUsers = class(TForm)
    ActionToolBar1: TActionToolBar;
    ActionManager1: TActionManager;
    acClose: TAction;
    acAdd: TAction;
    acDelete: TAction;
    srcUser: TDataSource;
    DBGrid1: TDBGrid;
    Splitter1: TSplitter;
    Panel1: TPanel;
    UserName: TDBEdit;
    Password: TDBEdit;
    WorkGroup: TDBLookupComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    acGroups: TAction;
    srcWorkgroupList: TDataSource;
    procedure acCloseExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acGroupsExecute(Sender: TObject);
    procedure srcUserDataChange(Sender: TObject; Field: TField);
    procedure srcUserStateChange(Sender: TObject);
  private
    procedure SetPassword(Sender: TField; const Text: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmUsers: TfrmUsers;

implementation
uses dm_GUI, frm_WorkGroup, dm_Images, Util;
{$R *.dfm}

procedure TfrmUsers.acAddExecute(Sender: TObject);
begin
  if UserName.CanFocus and UserName.Showing then UserName.SetFocus;
  srcUser.Dataset.Append;
end;

procedure TfrmUsers.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmUsers.acDeleteExecute(Sender: TObject);
begin
  if dmGUI.ConfirmDelete then
    srcUser.DataSet.Delete;
end;

procedure TfrmUsers.acGroupsExecute(Sender: TObject);
begin
  dmGUI.ShowForm(TfrmWorkGroup,True,frmWorkGroup);
end;

procedure TfrmUsers.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
  srcUser.DataSet.CheckBrowseMode;
end;

procedure TfrmUsers.FormDestroy(Sender: TObject);
begin
  frmUsers:=nil;
end;

procedure TfrmUsers.FormShow(Sender: TObject);
begin
  dmGUI.WorkGroupList.Open;
  dmGUI.UserEdit.Open;
end;

procedure TfrmUsers.srcUserDataChange(Sender: TObject; Field: TField);
begin
  WorkGroup.Enabled:=srcUser.DataSet.FieldByName('ID').AsString<>'1';
end;

procedure TfrmUsers.srcUserStateChange(Sender: TObject);
begin
  if Password.Field<>nil then
  begin
    Password.Field.OnSetText:=SetPassword;
    Password.Field.Required:=True;
    Password.Field.DisplayLabel:='парола';
  end;
  if UserName.Field<>nil then
  begin
    UserName.Field.Required:=True;
    UserName.Field.DisplayLabel:='потребителско име';
  end;
  if WorkGroup.Field<>nil then
  begin
    WorkGroup.Field.Required:=True;
    WorkGroup.Field.DisplayLabel:='потребителска група';
  end;
end;

procedure TfrmUsers.SetPassword(Sender: TField; const Text: string);
begin
  if Text='' then
    Sender.Clear
  else
  begin
    CheckValid(MatchPassword,Password,'Дължината на паролата трябва'#13#10'да е поне 6 символа');
    Sender.AsString:=Text;
  end;
end;

end.
