unit frm_Division;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ActnMan, ActnCtrls, Grids, DBGrids, DB, ActnList,
  XPStyleActnCtrls;

type
  TfrmDivision = class(TForm)
    DBGrid1: TDBGrid;
    srcDivision: TDataSource;
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
  frmDivision: TfrmDivision;

implementation
uses dm_GUI;

{$R *.dfm}

procedure TfrmDivision.acAddExecute(Sender: TObject);
begin
  srcDivision.DataSet.Append;
end;

procedure TfrmDivision.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmDivision.acDeleteExecute(Sender: TObject);
begin
  if not dmGUI.ConfirmDelete then Exit;
  srcDivision.DataSet.Delete;
end;

procedure TfrmDivision.acSaveExecute(Sender: TObject);
begin
;
end;

procedure TfrmDivision.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  srcDivision.DataSet.CheckBrowseMode;
  srcDivision.DataSet.Close;
  Action:=caFree;
end;

procedure TfrmDivision.FormDeactivate(Sender: TObject);
begin
  if dmGUI.DivisionList.Active then dmGUI.DivisionList.Requery;
end;

procedure TfrmDivision.FormDestroy(Sender: TObject);
begin
  frmDivision:=nil;
end;

procedure TfrmDivision.FormShow(Sender: TObject);
begin
  dmGUI.DivisionEdit.Open;
end;

end.
