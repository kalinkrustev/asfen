unit frm_Workgroup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ActnMan, ActnCtrls, Grids, DBGrids, DB, ActnList,
  XPStyleActnCtrls;

type
  TfrmWorkgroup = class(TForm)
    grdWorkgroup: TDBGrid;
    srcWorkgroup: TDataSource;
    ActionManager1: TActionManager;
    acSave: TAction;
    acAdd: TAction;
    acDelete: TAction;
    acClose: TAction;
    ActionToolBar1: TActionToolBar;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmWorkgroup: TfrmWorkgroup;

implementation
uses dm_GUI;

{$R *.dfm}

procedure TfrmWorkgroup.acAddExecute(Sender: TObject);
begin
  srcWorkgroup.DataSet.Append;
end;

procedure TfrmWorkgroup.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmWorkgroup.acDeleteExecute(Sender: TObject);
begin
  if not dmGUI.ConfirmDelete then Exit;
  srcWorkgroup.DataSet.Delete;
end;

procedure TfrmWorkgroup.acSaveExecute(Sender: TObject);
begin
;
end;

procedure TfrmWorkgroup.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  dmGUI.WorkGroupEdit.CheckBrowseMode;
  dmGUI.WorkGroupEdit.Close;
  Action:=caFree;
end;

procedure TfrmWorkgroup.FormDeactivate(Sender: TObject);
begin
  if dmGUI.WorkGroupList.Active then dmGUI.WorkGroupList.Requery;
end;

procedure TfrmWorkgroup.FormDestroy(Sender: TObject);
begin
  frmWorkgroup:=nil;
end;

procedure TfrmWorkgroup.FormShow(Sender: TObject);
begin
  dmGUI.WorkGroupEdit.Open;
  grdWorkGroup.ReadOnly:=not acAdd.Visible;
end;

end.
