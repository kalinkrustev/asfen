unit frm_City;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,VirtualDBTreeEx, ExtCtrls,VirtualTrees, DB, DBClient, StdCtrls, Grids,
  DBGrids, ToolWin, ActnMan, ActnCtrls, ActnList, XPStyleActnCtrls, Mask,
  DBCtrls, Buttons, ComCtrls;

type
  TfrmCity = class(TForm)
    TreePanel: TPanel;
    srcTree: TDataSource;
    ActionManager1: TActionManager;
    acPaste: TAction;
    ActionToolBar1: TActionToolBar;
    acDelete: TAction;
    acInsert: TAction;
    acEdit: TAction;
    EditPanel: TPanel;
    acSave: TAction;
    acCancel: TAction;
    EditName: TDBEdit;
    Kmetstvo: TDBEdit;
    Ekatte: TDBEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    document: TDBEdit;
    Label6: TLabel;
    category: TDBEdit;
    longitude: TDBEdit;
    latitude: TDBEdit;
    Label9: TLabel;
    Label10: TLabel;
    lastchange: TDBEdit;
    Label11: TLabel;
    Label12: TLabel;
    postcode: TDBEdit;
    Label13: TLabel;
    oblast: TDBEdit;
    Label14: TLabel;
    obstina: TDBEdit;
    srcKind: TDataSource;
    srcAltitude: TDataSource;
    srcTSB: TDataSource;
    srcRegion: TDataSource;
    kind: TDBLookupComboBox;
    altitude: TDBLookupComboBox;
    region: TDBLookupComboBox;
    tsb: TDBLookupComboBox;
    acClose: TAction;
    acImport: TAction;
    ProgressPanel: TPanel;
    ProgressText: TLabel;
    ProgressBar: TProgressBar;
    srcRaion: TDataSource;
    acCopy: TAction;
    Action1: TAction;
    btnCity1: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Label15: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acInsertExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure EditPanelExit(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure srcTreeStateChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Label15Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label8Click(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure acImportExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
  private
    procedure GetTreeText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure ReadPath(Sender: TBaseVirtualDBTreeEx; Var Path: String);
    procedure WritePath(Sender: TBaseVirtualDBTreeEx; Var Path: String);
    procedure SearchCity(Sender: TBaseVirtualTree; Node: PVirtualNode;
      const SearchText: WideString; var Result: Integer);
  public
    tree:TVirtualDBTreeEx;
  end;

var
  frmCity: TfrmCity;

implementation

uses dbClipImport, dm_Organisation, StrUtils, frm_GenericGrid, dm_Images,
  dm_AlarmDB, ADODB, XMLClipboard;

{$R *.dfm}

procedure TfrmCity.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmCity.acCopyExecute(Sender: TObject);
begin
  DataSetToXML(srcTree);
end;

procedure TfrmCity.acDeleteExecute(Sender: TObject);
begin
  if MessageDlg('Потвърдете изтриването',mtConfirmation,[mbYes,mbNo],0)=mrYes then
    Tree.DeleteSelection;
end;

procedure TfrmCity.acEditExecute(Sender: TObject);
begin
  srcTree.DataSet.Edit;
  EditName.SetFocus;
end;

procedure TfrmCity.acImportExecute(Sender: TObject);
begin
  if MessageDlg('Импортирането ще презапише текущите данни!'#13#10'Желаете ли да продължите?',mtConfirmation,[mbYes,mbNo],0)<>mrYes then
    Exit;

  Repaint;
  srcTree.DataSet.CheckBrowseMode;

  dmAlarmDB.ProgressBar:=ProgressBar;
  dmAlarmDB.ProgressText:=ProgressText;

  dmAlarmDB.ImportOrganisationsXls(Sender);

  if srcTSB.DataSet.Active and (srcTSB.DataSet is TADODataset) then
    TADODataset(srcTSB.DataSet).Requery;

  if srcRegion.DataSet.Active and (srcRegion.DataSet is TADODataset) then
    TADODataset(srcRegion.DataSet).Requery;

  if srcRaion.DataSet.Active and (srcRaion.DataSet is TADODataset) then
    TADODataset(srcRaion.DataSet).Requery;

  if srcTree.DataSet.Active and (srcTree.DataSet is TADODataset) then
    TADODataset(srcTree.DataSet).Requery;
end;

procedure TfrmCity.acInsertExecute(Sender: TObject);
begin
  srcTree.DataSet.Append;
  EditName.SetFocus;
end;

procedure TfrmCity.acPasteExecute(Sender: TObject);
begin
  srcTree.DataSet.DisableControls;
  try
    ImportFromClipBoard(srcTree.DataSet);
  finally
    srcTree.DataSet.EnableControls;
  end;
end;

procedure TfrmCity.acSaveExecute(Sender: TObject);
begin
  srcTree.DataSet.Post;
end;

procedure TfrmCity.Action1Execute(Sender: TObject);
begin
  dmAlarmDB.ExportToXls;
end;

procedure TfrmCity.acCancelExecute(Sender: TObject);
begin
  srcTree.DataSet.Cancel;
end;

procedure TfrmCity.EditPanelExit(Sender: TObject);
begin
  srcTree.DataSet.CheckBrowseMode;
end;

procedure TfrmCity.SearchCity(Sender: TBaseVirtualTree; Node: PVirtualNode; const SearchText: WideString;
    var Result: Integer);
var I: Integer;
begin
  Result:=1;
  for I := 0 to 2 do
  begin
    if AnsiSameText(SearchText,LeftStr(tree.Text[Node,I],Length(SearchText))) then
    begin
      Result:=0;
      Exit;
    end;
  end;
end;

procedure TfrmCity.srcTreeStateChange(Sender: TObject);
begin
  acSave.Enabled:=(srcTree.DataSet<>nil) and srcTree.DataSet.Active and (srcTree.DataSet.State in dsEditModes);
  acCancel.Enabled:=acSave.Enabled;
  acInsert.Enabled:=(srcTree.DataSet<>nil) and srcTree.DataSet.Active and not acCancel.Enabled;
  acDelete.Enabled:=(srcTree.DataSet<>nil) and srcTree.DataSet.Active and not acCancel.Enabled and not srcTree.DataSet.IsEmpty;
  acEdit.Enabled:=acDelete.Enabled;
end;

procedure TfrmCity.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if srcTree.DataSet <> nil then srcTree.DataSet.CheckBrowseMode;
  if FormStyle=fsMDIChild then Action:=caFree;
end;

procedure TfrmCity.FormCreate(Sender: TObject);
begin
  tree:=TVirtualDBTreeEx.Create(Self);
  tree.BorderStyle:=bsNone;
  tree.parent:=TreePanel;
  tree.Align:=alClient;
  tree.DBOptions:=tree.DBOptions-[dboParentStructure,dboWriteSecondary,dboWriteLevel]+[dboPathStructure];
  tree.OnReadPathFromDB:=ReadPath;
  tree.OnWritePathToDB:=WritePath;
  tree.PathFieldName:='kmetstvo';
  tree.ViewFieldName:='name';
  tree.KeyFieldName:='id';
  tree.DBDataFieldNames:='ekatte;kmetstvo';
  //tree.LevelFieldName:='level';
  tree.OnGetText:=GetTreeText;
  tree.TreeOptions.SelectionOptions:=tree.TreeOptions.SelectionOptions+[toFullRowSelect];
  tree.TreeOptions.MiscOptions:=tree.TreeOptions.MiscOptions-[toEditable];
  tree.IncrementalSearch:=isAll;
  tree.Header.Columns.Add.Width:=250;
  tree.Header.Columns[0].Text:='Населено място';
  tree.Header.Columns.Add.Width:=50;
  tree.Header.Columns[1].Text:='ЕКАТТЕ';
  tree.Header.Columns.Add.Width:=80;
  tree.Header.Columns[2].Text:='Кметство';
  tree.Header.Options:=tree.Header.Options+[hoVisible];
  tree.Header.Style:=hsFlatButtons;
  tree.DataSource:=srcTree;
  tree.OnIncrementalSearch:=SearchCity;
  tree.IncrementalSearchStart:=ssFocusedNode;

end;

procedure TfrmCity.FormDestroy(Sender: TObject);
begin
  frmCity:=nil;
end;

procedure TfrmCity.FormResize(Sender: TObject);
begin
  ProgressPanel.Left:=(ClientWidth-ProgressPanel.Width) div 2;
  ProgressPanel.Top:= (ClientHeight-ProgressPanel.Height) div 2;
end;

procedure TfrmCity.GetTreeText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  CellData: Variant;
  DBData: PDBNodeData;
begin
  DBData := TVirtualDBTreeEx(Sender).GetDBNodeData(Node);
  CellData := DBData.DBData;
  case Column of
// -1,0:   // this is the tree, allready handled
    1:begin
        CellText:=VarToStr(CellData[0]);
      end;
    2:begin
        CellText:=VarToStr(CellData[1]);
      end;
  end;
end;

procedure TfrmCity.Label15Click(Sender: TObject);
begin
  dmImages.ShowForm(TfrmGenericGrid,True,frmGenericGrid,False);
  frmGenericGrid.DataSource.DataSet:=srcKind.DataSet;
  frmGenericGrid.Caption:='Номенклатура вид населено място';
  frmGenericGrid.ShowModal;
end;

procedure TfrmCity.Label4Click(Sender: TObject);
begin
  dmImages.ShowForm(TfrmGenericGrid,True,frmGenericGrid,False);
  frmGenericGrid.DataSource.DataSet:=srcRegion.DataSet;
  frmGenericGrid.Caption:='Номенклатура региони';
  frmGenericGrid.ShowModal;
end;

procedure TfrmCity.Label7Click(Sender: TObject);
begin
  dmImages.ShowForm(TfrmGenericGrid,True,frmGenericGrid,False);
  frmGenericGrid.DataSource.DataSet:=srcAltitude.DataSet;
  frmGenericGrid.Caption:='Номенклатура надморски височини';
  frmGenericGrid.ShowModal;
end;

procedure TfrmCity.Label8Click(Sender: TObject);
begin
  dmImages.ShowForm(TfrmGenericGrid,True,frmGenericGrid,False);
  frmGenericGrid.DataSource.DataSet:=srcTsb.DataSet;
  frmGenericGrid.Caption:='Номенклатура статистически бюра';
  frmGenericGrid.ShowModal;
end;

procedure TfrmCity.ReadPath(Sender: TBaseVirtualDBTreeEx; var Path: String);
begin
  if Length(Path)<=3 then
  begin
    Path:='';
    Exit;
  end;
  Path:=StrToID(Copy(Tree.PathField.AsString,1,3));
  if Length(Tree.PathField.AsString)>5 then
  Path:=Path+'.'+StrToID(Copy(Tree.PathField.AsString,1,3))+Copy(Tree.PathField.AsString,4,2);
  while (Path<>'') and (Path[Length(Path)]='.') do Delete(Path,Length(Path),1);
end;

procedure TfrmCity.WritePath(Sender: TBaseVirtualDBTreeEx; var Path: String);
var S:String;
begin
  S:=StringReplace(Trim(Path),'.','',[rfReplaceAll]);
  if Length(S)<=5 then
    Tree.PathField.AsString:=S
  else
    Tree.PathField.AsString:=Copy(S,1,5)+'-'+Copy(S,6,2);
end;

end.
