unit frm_Organisation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,VirtualDBTreeEx, DB, DBClient, ActnList, XPStyleActnCtrls,
  ActnMan, ToolWin, ActnCtrls, Grids, DBGrids, VirtualTrees, DBCtrls, Buttons,
  StdCtrls, Mask;

type
  TfrmOrganisation = class(TForm)
    TreePanel: TPanel;
    srcTree: TDataSource;
    ActionToolBar1: TActionToolBar;
    ActionManager1: TActionManager;
    acNewOrganisation: TAction;
    acDelete: TAction;
    acClose: TAction;
    acEdit: TAction;
    acNewDivision: TAction;
    acNewPosition: TAction;
    Splitter1: TSplitter;
    Panel3: TPanel;
    GroupPrivate: TGroupBox;
    Label3: TLabel;
    Name1: TDBEdit;
    Label6: TLabel;
    Phone2: TDBEdit;
    GroupWork: TGroupBox;
    Label1: TLabel;
    Label5: TLabel;
    City2: TDBLookupComboBox;
    btnCity2: TSpeedButton;
    Label8: TLabel;
    Address2: TDBMemo;
    Label10: TLabel;
    City1: TDBLookupComboBox;
    btnCity1: TSpeedButton;
    Label11: TLabel;
    Address1: TDBMemo;
    Label2: TLabel;
    Label7: TLabel;
    Label4: TLabel;
    btnPosition: TSpeedButton;
    Phone: TDBEdit;
    Phone3: TDBEdit;
    Position: TDBLookupComboBox;
    Label9: TLabel;
    srcCityList: TDataSource;
    srcPosition: TDataSource;
    EMail: TDBEdit;
    Label12: TLabel;
    acRecode: TAction;
    Label13: TLabel;
    Name2: TDBEdit;
    Label14: TLabel;
    Name3: TDBEdit;
    Label15: TLabel;
    Site: TDBEdit;
    Label16: TLabel;
    DTMF: TDBEdit;
    Splitter2: TSplitter;
    Label17: TLabel;
    Obstina1: TDBEdit;
    Oblast1: TDBEdit;
    Label18: TLabel;
    Obstina2: TDBEdit;
    Oblast2: TDBEdit;
    Label19: TLabel;
    Raion1: TDBLookupComboBox;
    btnRaion1: TSpeedButton;
    srcRaion: TDataSource;
    Label20: TLabel;
    Raion2: TDBLookupComboBox;
    btnRaion2: TSpeedButton;
    acImportAdd: TAction;
    btnSetName: TSpeedButton;
    ScrollBox2: TScrollBox;
    acExportXML: TAction;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    acImportReplace: TAction;
    procedure FormCreate(Sender: TObject);
    procedure acNewOrganisationExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure acNewDivisionExecute(Sender: TObject);
    procedure acNewPositionExecute(Sender: TObject);
    procedure TableAfterScroll(DataSet: TDataSet);
    procedure acCloseExecute(Sender: TObject);
    procedure TableAfterOpen(DataSet: TDataSet);
    procedure acDeleteExecute(Sender: TObject);
    procedure srcTreeDataChange(Sender: TObject; Field: TField);
    procedure btnCity1Click(Sender: TObject);
    procedure btnCity2Click(Sender: TObject);
    procedure btnPositionClick(Sender: TObject);
    procedure acRecodeExecute(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure btnRaion1Click(Sender: TObject);
    procedure Raion2Enter(Sender: TObject);
    procedure Raion1Enter(Sender: TObject);
    procedure Raion1Exit(Sender: TObject);
    procedure Raion2Exit(Sender: TObject);
    procedure btnSetNameClick(Sender: TObject);
    procedure acExportXMLExecute(Sender: TObject);
    procedure acImportAddExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure srcTreeStateChange(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure acImportReplaceExecute(Sender: TObject);
  private
    tree:TVirtualDBTreeEx;
    procedure EditNode;
    procedure CreateNode(IsDivision:Boolean);
    procedure EnableActions;
    function  GetNodeID(N:PVirtualNode):Double;
    procedure GetTreeText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
    procedure SetEmail(Sender: TField; const Text: string);
    procedure SetPhone(Sender: TField; const Text: string);
    procedure SetPhone3(Sender: TField; const Text: string);
    procedure SetPhone2(Sender: TField; const Text: string);
    procedure SetDTMF(Sender: TField; const Text: string);
  public
    { Public declarations }
  end;

var
  frmOrganisation: TfrmOrganisation;

implementation

uses frm_City, frm_Position, frm_GenericGrid, dm_Images, Math, StrUtils,
  dm_GUI, Util;

{$R *.dfm}

procedure TfrmOrganisation.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmOrganisation.acDeleteExecute(Sender: TObject);
begin
  if MessageDlg('Потвърдете изтриването',mtConfirmation,[mbYes,mbNo],0)=mrYes then
  begin
    dmGUI.ChangeVersion('Organisation');
    Tree.DeleteSelection;
  end;
end;

procedure TfrmOrganisation.acNewDivisionExecute(Sender: TObject);
begin
  CreateNode(True);
end;

procedure TfrmOrganisation.acNewOrganisationExecute(Sender: TObject);
begin
  tree.FocusedNode:=nil;
  CreateNode(True);
end;

procedure TfrmOrganisation.acNewPositionExecute(Sender: TObject);
begin
  CreateNode(False);
end;

procedure TfrmOrganisation.CreateNode;
var N:PVirtualNode;
    Data:PDBVTData;
    Max:Double;
    Cnt:Integer;
begin
  //tree.AddNode(tree.FocusedNode);
  N:=Tree.GetFirst;
  Cnt:=0;
  if not IsDivision then
  begin
    while N<>nil do
    begin
      Data:=Tree.GetNodeData(N);
      if (Data<>nil) and (Data.ID>=1E12) then
        Cnt:=Cnt+1;
      N:=Tree.GetNext(N);
    end;

    if Cnt>=Limits.OrganisationCount then
    begin
      PopControlHint(Tree,'Позволено е дефиниране на максимум '+IntToStr(Limits.OrganisationCount)+' абоната в системата');
      Abort;
    end;
  end;

  dmGUI.ChangeVersion('Organisation');
  Max:=0;
  N:=nil;

  if (Tree.FocusedNode=nil) and (Tree.RootNode.ChildCount<99) then
  begin
    Max:=1;
    N:=tree.RootNode.FirstChild
  end
  else
  if (Tree.FocusedNode<>nil) and (Tree.FocusedNode.ChildCount<99) then
  begin
    Data := tree.GetNodeData(tree.FocusedNode);
    if Data.ID<1E12 then
    begin
      if not IsDivision then
      begin
        Max:=Data.ID*100;
        while Max<1E12 do Max:=Max*100;
        Max:=Max+1;
      end else
        Max:=Data.ID*100+1;
    end;
    N:=tree.FocusedNode.FirstChild;
  end;

  while N<>nil do
  begin
    Data := tree.GetNodeData(N);
    if (Data.ID>=Max) and (not IsDivision or (Data.ID<1E12)) then Max:=Data.ID+1;
    N:=N.NextSibling;
  end;
  if Max>0 then
  begin
    srcTree.DataSet.Append;
    Tree.ParentField.AsFloat:=Trunc(Max / 100);
    Tree.KeyField.AsFloat:=Max;
    if Max<100 then
      Tree.ViewField.AsString:='Нова организация'
    else
    if not IsDivision then
      Tree.ViewField.AsString:='Нова длъжност'
    else
      Tree.ViewField.AsString:='Нов отдел';
    srcTree.DataSet.Post;
    EditNode;
  end;

end;

procedure TfrmOrganisation.acEditExecute(Sender: TObject);
begin
  EditNode;
end;

procedure TfrmOrganisation.btnPositionClick(Sender: TObject);
begin
  //dmImages.ShowForm(TfrmPosition,True,frmPosition,False);
  //frmPosition.ShowModal;

  srcRaion.DataSet.Filtered:=False;
  dmImages.ShowForm(TfrmGenericGrid,True,frmGenericGrid,False);
  frmGenericGrid.ImportFile:=ExtractFilePath(ParamStr(0))+'xls\nom_dluj.xls';
  frmGenericGrid.DataSource.DataSet:=srcPosition.DataSet;
  frmGenericGrid.Caption:='Номенклатура длъжности';
  frmGenericGrid.ShowModal;
end;

type MyTree=class(TBaseVirtualDBTreeEx);
procedure TfrmOrganisation.acRecodeExecute(Sender: TObject);
var F:Double;
    N:PVirtualNode;
    Key,NewKey:TField;
begin
  srcTree.DataSet.DisableControls;
  try
    Key:=tree.KeyField;
    NewKey:=srcTree.DataSet.FieldByName('NewCode');
    srcTree.DataSet.First;
    while not srcTree.DataSet.Eof do
    begin
      N:=MyTree(tree).FindNode(nil,Key.AsFloat);
      F:=GetNodeID(N);
      if F<>Key.AsFloat then
      begin
        srcTree.DataSet.Edit;
        NewKey.AsFloat:=F;
        srcTree.DataSet.Post;
      end;
      srcTree.DataSet.Next;
    end;
    srcTree.DataSet.First;
    if srcTree.DataSet is TClientDataSet then
    begin
      TClientDataSet(srcTree.DataSet).IndexFieldNames:='NewCode';
    end;
    try
      while not srcTree.DataSet.Eof do
      begin
        if (NewKey.AsFloat<>0) and (NewKey.AsFloat<>Key.AsFloat) then
        begin
          srcTree.DataSet.Edit;
          Key.AsFloat:=NewKey.AsFloat;
          srcTree.DataSet.Post;
        end;
        srcTree.DataSet.Next;
      end;
    finally
      if srcTree.DataSet is TClientDataSet then
      begin
        TClientDataSet(srcTree.DataSet).IndexFieldNames:='Code';
      end;
    end;
    srcTree.DataSet.First;
  finally
    srcTree.DataSet.EnableControls;
  end;
end;

procedure TfrmOrganisation.acExportXMLExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    if SameText(ExtractFileExt(SaveDialog.FileName),'.xlw') then
      dmGUI.ExportOrganisationXLW(SaveDialog.FileName)
    else
    if SameText(ExtractFileExt(SaveDialog.FileName),'.xls') then
      dmGUI.ExportOrganisationXLS(SaveDialog.FileName)
    else  
    if SameText(ExtractFileExt(SaveDialog.FileName),'.xml') then
      dmGUI.ExportOrganisation(SaveDialog.FileName);
  end;
end;

procedure TfrmOrganisation.acImportAddExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    if SameText(ExtractFileExt(OpenDialog.FileName),'.xls') then
      dmGUI.ImportOrganisationXLS(OpenDialog.FileName,False)
    else
    if SameText(ExtractFileExt(OpenDialog.FileName),'.xml') then
      dmGUI.ImportOrganisation(OpenDialog.FileName,False);
    tree.FullCollapse();
  end
end;

procedure TfrmOrganisation.acImportReplaceExecute(Sender: TObject);
begin
  if dmGUI.Confirm('Импортирането ще подмени текущата организационна структура!'#13#10
  +'Единственият начин да я възстановите е от архив!'#13#10#13#10'Желаете ли да продължите?') and OpenDialog.Execute then
  begin
    if SameText(ExtractFileExt(OpenDialog.FileName),'.xls') then
      dmGUI.ImportOrganisationXLS(OpenDialog.FileName,True)
    else
    if SameText(ExtractFileExt(OpenDialog.FileName),'.xml') then
      dmGUI.ImportOrganisation(OpenDialog.FileName,True);
    tree.FullCollapse();
  end
end;

procedure TfrmOrganisation.Button2Click(Sender: TObject);
begin
  ShowMessage(FloatToStr(GetNodeID(Tree.FocusedNode)));
end;

procedure TfrmOrganisation.TableAfterOpen(DataSet: TDataSet);
begin
  EnableActions;
end;

procedure TfrmOrganisation.TableAfterScroll(DataSet: TDataSet);
begin
  EnableActions;
end;

procedure TfrmOrganisation.EnableActions;
begin
  acNewDivision.Enabled:=(srcTree.DataSet<>nil) and not srcTree.DataSet.IsEmpty and (Tree.KeyField.AsFloat<1E10);
  acNewPosition.Enabled:=(srcTree.DataSet<>nil) and not srcTree.DataSet.IsEmpty and (Tree.KeyField.AsFloat<1E12);
end;

procedure TfrmOrganisation.EditNode;
begin
  if tree.FocusedNode <> nil then
  begin
    srcTree.DataSet.Edit;
    tree.EditNode(tree.FocusedNode, tree.Header.MainColumn);
  end;
end;

function TfrmOrganisation.GetNodeID(N: PVirtualNode): Double;
var T:PVirtualNode;
    Data:PDBVTData;
    B:Boolean;
    I:Integer;
begin
  Result:=0;
  if N=nil then Exit;

  Data:=Tree.GetNodeData(N);
  if Data=nil then Exit;

  B:=Data.ID>=1E12;
  T:=N.Parent.FirstChild;
  I:=1;
  while (T<>N) and (T<>nil) do
  begin
    Data:=Tree.GetNodeData(T);
    if (Data<>nil) and not (B xor (Data.ID>=1E12)) then I:=I+1;
    T:=T.NextSibling;
  end;

  if N.Parent=tree.RootNode then Result:=I
  else
  if B then
  begin
    Result:=GetNodeID(N.Parent);
    while (Result<1E12) and (Result<>0) do Result:=Result*100;
    Result:=Result+I;
  end else
    Result:=100*GetNodeID(N.Parent)+I;
end;

procedure TfrmOrganisation.GetTreeText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: WideString);
var
  CellData: Variant;
  DBData: PDBNodeData;
  Data: PDBVTData;
begin
  DBData := TVirtualDBTreeEx(Sender).GetDBNodeData(Node);
  Data := Tree.GetNodeData(Node);
  CellData := DBData.DBData;
  case Column of
// -1,0:   // this is the tree, allready handled
    1:begin
        CellText:=Format('%1.0f',[Data.ID]);
        While(Length(CellText) mod 2<>0) do CellText:='0'+CellText;
      end;
  end;
end;

procedure TfrmOrganisation.Raion1Enter(Sender: TObject);
begin
  srcRaion.DataSet.Filter:='raion like '''+City1.Field.AsString+'%''';
  srcRaion.DataSet.Filtered:=True;
end;

procedure TfrmOrganisation.Raion1Exit(Sender: TObject);
begin
  srcRaion.DataSet.Filtered:=False;
end;

procedure TfrmOrganisation.Raion2Enter(Sender: TObject);
begin
  srcRaion.DataSet.Filter:='raion like '''+City2.Field.AsString+'%''';
  srcRaion.DataSet.Filtered:=True;
end;

procedure TfrmOrganisation.Raion2Exit(Sender: TObject);
begin
  srcRaion.DataSet.Filtered:=False;
end;

procedure TfrmOrganisation.btnCity2Click(Sender: TObject);
var V:Variant;
begin
  dmImages.ShowForm(TfrmCity,True,frmCity,False);
  if not varIsNull(City2.KeyValue) then
  begin
    V:=srcCityList.DataSet.Lookup('EKATTE',City2.KeyValue,'ID');
    if not VarIsNull(V) then
    begin
      if frmCity.tree.GoToRec(V) then
        frmCity.tree.TopNode:=frmCity.tree.FocusedNode;
    end;
  end;
  frmCity.ShowModal;
end;

procedure TfrmOrganisation.btnCity1Click(Sender: TObject);
var V:Variant;
begin
  dmImages.ShowForm(TfrmCity,True,frmCity,False);

  if dmGUI.Kind.Active then
     dmGUI.Kind.Requery
  else
     dmGUI.Kind.Open;
  if dmGUI.TSB.Active then
     dmGUI.TSB.Requery
  else
     dmGUI.TSB.Open;                                
  if dmGUI.Altitude.Active then
     dmGUI.Altitude.Requery
  else
     dmGUI.Altitude.Open;
  if dmGUI.Region.Active then
     dmGUI.Region.Requery
  else
     dmGUI.Region.Open;

  frmCity.srcTree.DataSet:=dmGUI.City;
  frmCity.srcKind.DataSet:=dmGUI.Kind;
  frmCity.srcTSB.DataSet:=dmGUI.TSB;
  frmCity.srcAltitude.DataSet:=dmGUI.Altitude;
  frmCity.srcRegion.DataSet:=dmGUI.Region;
  frmCity.srcRaion.DataSet:=dmGUI.Raion;

  if not varIsNull(City1.KeyValue) then
  begin
    V:=srcCityList.DataSet.Lookup('EKATTE',City1.KeyValue,'ID');
    if not VarIsNull(V) then
    begin
      if frmCity.tree.GoToRec(V) then
        frmCity.tree.TopNode:=frmCity.tree.FocusedNode;
    end;
  end;
  frmCity.ShowModal;
end;

procedure TfrmOrganisation.btnRaion1Click(Sender: TObject);
begin
  srcRaion.DataSet.Filtered:=False;
  dmImages.ShowForm(TfrmGenericGrid,True,frmGenericGrid,False);
  frmGenericGrid.DataSource.DataSet:=srcRaion.DataSet;
  frmGenericGrid.Caption:='Номенклатура райони';
  frmGenericGrid.ShowModal;
end;

procedure TfrmOrganisation.btnSetNameClick(Sender: TObject);
begin
  if srcTree.DataSet=nil then Exit;
  srcTree.DataSet.Edit;
  tree.ViewField.AsString:=Position.Text;
end;

procedure TfrmOrganisation.srcTreeDataChange(Sender: TObject; Field: TField);
var S:String;
begin
  ScrollBox2.Visible:=Tree.KeyField.AsFloat>=1E12;
  Splitter2.Visible:=ScrollBox2.Visible;
  Splitter2.Top:=ScrollBox2.Top-1;
  if City1.Field<>nil then
  begin
    S:=City1.Field.AsString;
    Raion1.Enabled:=(S='10135') or (S='56784') or (S='68134');
  end;
  if City2.Field<>nil then
  begin
    S:=City2.Field.AsString;
    Raion2.Enabled:=(S='10135') or (S='56784') or (S='68134');
  end;
  EnableActions;
end;

procedure TfrmOrganisation.srcTreeStateChange(Sender: TObject);
begin
  if EMail.Field<>nil then EMail.Field.OnSetText:=SetEmail;
  if Phone.Field<>nil then Phone.Field.OnSetText:=SetPhone;
  if Phone2.Field<>nil then Phone2.Field.OnSetText:=SetPhone2;
  if Phone3.Field<>nil then Phone3.Field.OnSetText:=SetPhone3;
  if DTMF.Field<>nil then DTMF.Field.OnSetText:=SetDTMF;
end;

procedure TfrmOrganisation.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if srcTree.DataSet<>nil then srcTree.DataSet.CheckBrowseMode;
  if FormStyle=fsMDIChild then Action:=caFree;
end;

procedure TfrmOrganisation.CompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var S1,S2:WideString;
begin
  GetTreeText(Sender,Node1,Column,ttStatic,S1);
  GetTreeText(Sender,Node2,Column,ttStatic,S2);
  if Column=1 then
  begin
    S1:=LeftStr(S1,Length(S2));
    S2:=LeftStr(S2,Length(S1));
  end;
  Result:=Sign(CompareStr(S1,S2));
end;


procedure TfrmOrganisation.FormCreate(Sender: TObject);
begin
  tree:=TVirtualDBTreeEx.Create(Self);
  tree.BorderStyle:=bsNone;
  tree.parent:=TreePanel;
  tree.Align:=alClient;
  tree.DBOptions:=tree.DBOptions-[dboPathStructure,dboWriteSecondary]+[dboParentStructure];
  tree.ParentFieldName:='parent';
  tree.ViewFieldName:='name';
  tree.KeyFieldName:='code';
  tree.OnCompareNodes:=CompareNodes;
  //tree.LevelFieldName:='level';
  tree.OnGetText:=GetTreeText;
  tree.TreeOptions.SelectionOptions:=tree.TreeOptions.SelectionOptions+[toFullRowSelect];
  tree.TreeOptions.AutoOptions:=tree.TreeOptions.AutoOptions+[toAutoSort];
  tree.Header.Columns.Add.Width:=320;
  tree.Header.Columns[0].Text:='Щатно разписание';
  tree.Header.Columns.Add.Width:=110;
  tree.Header.Columns[1].Text:='Код';
  tree.Header.Options:=tree.Header.Options+[hoVisible];
  tree.Header.Style:=hsFlatButtons;
  tree.Header.SortDirection:=sdAscending;
  tree.DataSource:=srcTree;
  tree.Header.SortColumn:=1;
end;

procedure TfrmOrganisation.FormDeactivate(Sender: TObject);
begin
  Tree.EndEditNode;
  if (srcTree.DataSet<>nil) and (srcTree.DataSet.Active) then
    srcTree.DataSet.CheckBrowseMode;
end;

procedure TfrmOrganisation.FormDestroy(Sender: TObject);
begin
  frmOrganisation:=nil;
end;

procedure TfrmOrganisation.FormShow(Sender: TObject);
begin
  srcTree.AutoEdit:=dmGUI.GetRight('Organisation.Edit','*');
end;

procedure TfrmOrganisation.SetEmail(Sender: TField; const Text: string);
begin
  if Text='' then
    Sender.Clear
  else
  begin
    CheckValid(MatchEMail,Email);
    Sender.AsString:=Text;
  end;
end;

procedure TfrmOrganisation.SetPhone(Sender: TField; const Text: string);
begin
  if Text='' then
    Sender.Clear
  else
  begin
    CheckValid(MatchPhone,Phone);
    Sender.AsString:=Text;
  end;
end;

procedure TfrmOrganisation.SetPhone3(Sender: TField; const Text: string);
begin
  if Text='' then
    Sender.Clear
  else
  begin
    CheckValid(MatchPhone,Phone3);
    Sender.AsString:=Text;
  end;
end;

procedure TfrmOrganisation.SetPhone2(Sender: TField; const Text: string);
begin
  if Text='' then
    Sender.Clear
  else
  begin
    CheckValid(MatchPhone,Phone2);
    Sender.AsString:=Text;
  end;
end;

procedure TfrmOrganisation.SetDTMF(Sender: TField; const Text: string);
begin
  if Text='' then
    Sender.Clear
  else
  begin
    CheckValid(MatchDTMF,DTMF,'Въведете минимум 3 цифри'#13#10'без повтарящи се съседни');
    Sender.AsString:=Text;
  end;
end;


end.
