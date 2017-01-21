unit frm_Position;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ActnMan, ActnCtrls, Grids, DBGrids, DB, ActnList,
  XPStyleActnCtrls;

type
  TfrmPosition = class(TForm)
    DBGrid1: TDBGrid;
    srcPositionEdit: TDataSource;
    ActionManager1: TActionManager;
    acSave: TAction;
    acAdd: TAction;
    acDelete: TAction;
    acClose: TAction;
    ActionToolBar1: TActionToolBar;
    srcPositionList: TDataSource;
    acPaste: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPosition: TfrmPosition;

implementation
uses AdoDB, dbClipImport;

{$R *.dfm}

procedure TfrmPosition.acAddExecute(Sender: TObject);
begin
  srcPositionEdit.DataSet.Append;
end;

procedure TfrmPosition.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmPosition.acDeleteExecute(Sender: TObject);
begin
  srcPositionEdit.DataSet.Delete;
end;

procedure TfrmPosition.acSaveExecute(Sender: TObject);
begin
;
end;

procedure TfrmPosition.acPasteExecute(Sender: TObject);
begin
  srcPositionEdit.DataSet.DisableControls;
  try
    ImportFromClipBoard(srcPositionEdit.DataSet);
  finally
    srcPositionEdit.DataSet.EnableControls;
  end;
end;

procedure TfrmPosition.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if srcPositionEdit.DataSet<>nil then
    srcPositionEdit.DataSet.CheckBrowseMode;
end;

procedure TfrmPosition.FormDeactivate(Sender: TObject);
begin
  if srcPositionList.DataSet is TADODataSet then
    if TADODataSet(srcPositionList.DataSet).Active then TADODataSet(srcPositionList.DataSet).Requery;
end;

procedure TfrmPosition.FormDestroy(Sender: TObject);
begin
  frmPosition:=nil;
end;

end.
