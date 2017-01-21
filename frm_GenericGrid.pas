unit frm_GenericGrid;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ActnList, XPStyleActnCtrls, ActnMan, ToolWin, ActnCtrls, Grids,
  DBGrids;

type
  TfrmGenericGrid = class(TForm)
    ActionToolBar1: TActionToolBar;
    ActionManager1: TActionManager;
    acDelete: TAction;
    acClose: TAction;
    acEdit: TAction;
    acCopy: TAction;
    acPaste: TAction;
    DataSource: TDataSource;
    DBGrid1: TDBGrid;
    acImport: TAction;
    procedure acDeleteExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure acImportExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
    ImportFile:String;
  end;

var
  frmGenericGrid: TfrmGenericGrid;

implementation

uses XMLClipboard, dm_AlarmDB, ADODB, dm_GUI;

{$R *.dfm}

procedure TfrmGenericGrid.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmGenericGrid.acCopyExecute(Sender: TObject);
begin
  DataSetToXML(DataSource);
end;

procedure TfrmGenericGrid.acDeleteExecute(Sender: TObject);
begin
  if dmGUI.ConfirmDelete then
    DataSource.DataSet.Delete;
end;

procedure TfrmGenericGrid.acEditExecute(Sender: TObject);
begin
  DataSource.DataSet.Edit;
end;

procedure TfrmGenericGrid.acImportExecute(Sender: TObject);
begin
  if MessageDlg('Импортирането ще презапише текущите данни!'#13#10'Желаете ли да продължите?',mtConfirmation,[mbYes,mbNo],0)<>mrYes then
    Exit;
  dmAlarmDB.ImportFromXLS(ImportFile,True);

  if DataSource.DataSet.Active and (DataSource.DataSet is TADODataset) then
    TADODataset(DataSource.DataSet).Requery;
end;

procedure TfrmGenericGrid.acPasteExecute(Sender: TObject);
begin
  XMLToDataSet(DataSource,True);
end;

procedure TfrmGenericGrid.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TfrmGenericGrid.FormDestroy(Sender: TObject);
begin
  frmGenericGrid:=nil;
end;

procedure TfrmGenericGrid.FormShow(Sender: TObject);
begin
  acImport.Visible:=(ImportFile<>'') and FileExists(ImportFile);
end;

end.
