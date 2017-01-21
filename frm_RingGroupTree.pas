unit frm_RingGroupTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, XPStyleActnCtrls, ActnMan, ToolWin, ActnCtrls, ExtCtrls,
  Grids, DBGrids, DB,VirtualDBTreeEx,VirtualTrees, StdCtrls, Mask, DBCtrls;

type
  TfrmRingGroupTree = class(TForm)
    ActionToolBar1: TActionToolBar;
    ActionManager1: TActionManager;
    acSave: TAction;
    acAdd: TAction;
    acDelete: TAction;
    acClose: TAction;
    acCheck: TAction;
    acUncheck: TAction;
    acUp: TAction;
    acDown: TAction;
    srcRingGroup: TDataSource;
    GroupGrid: TDBGrid;
    Splitter1: TSplitter;
    PanelTree: TPanel;
    srcTree: TDataSource;
    Panel1: TPanel;
    Splitter2: TSplitter;
    ListGroup: TListBox;
    LabelGroup: TStaticText;
    ScrollList: TTimer;
    acRename: TAction;
    Splitter3: TSplitter;
    Info: TMemo;
    acAutoCheck: TAction;
    procedure FormCreate(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure srcRingGroupDataChange(Sender: TObject; Field: TField);
    procedure acSaveExecute(Sender: TObject);
    procedure acUpExecute(Sender: TObject);
    procedure acDownExecute(Sender: TObject);
    procedure ListGroupDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListGroupKeyPress(Sender: TObject; var Key: Char);
    procedure ListGroupMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListGroupMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListGroupMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScrollListTimer(Sender: TObject);
    procedure ListGroupClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acUncheckExecute(Sender: TObject);
    procedure ListGroupKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GroupGridExit(Sender: TObject);
    procedure acRenameExecute(Sender: TObject);
    procedure acAutoCheckExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    tree:TCheckVirtualDBTreeEx;
    RIngGroup:Integer;
    FChanged,Loading: Boolean;

    draggedindex:integer;
    dropindex, previndex:integer;
    lastitemincrement:integer;
    dragflag:boolean;
    scrollamt:integer;

    procedure GetTreeText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure CompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
    procedure SetChanged(const Value: Boolean);
    procedure SaveCurrent;
    procedure LoadCurrent;
    procedure treeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure MoveItemID(ID: Double);
    procedure ClearChecked;
    function contiguous: boolean;
    function FirstSelectedIndex: integer;
    function LastSelectedIndex: integer;
    procedure MoveSelection(Source, Destination: Integer);
    procedure Redrawitems;
    procedure Stoptimer;
    procedure FocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    function NodeInfo(ID: Double): String;
    procedure Checking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    property  DataChanged:Boolean read FChanged write SetChanged;
    procedure UpdateCheckActions;
    procedure UncheckSelected;
  public
    { Public declarations }
  end;

var
  frmRingGroupTree: TfrmRingGroupTree;

implementation

uses dm_GUI, StrUtils, Math, dm_Images, Util;

{$R *.dfm}

procedure TfrmRingGroupTree.GetTreeText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: WideString);
var
  CellData: Variant;
  DBData: PDBNodeData;
  Data: PDBVTData;
begin
  DBData := TCheckVirtualDBTreeEx(Sender).GetDBNodeData(Node);
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

procedure TfrmRingGroupTree.SetChanged(const Value: Boolean);
begin
  FChanged := Value;
  acSave.Enabled:=FChanged;
end;

procedure TfrmRingGroupTree.srcRingGroupDataChange(Sender: TObject;
  Field: TField);
begin
  if Field=nil then
  begin
    if DataChanged and acSave.Visible and dmGUI.Confirm('Желаете ли запис на промените?') then SaveCurrent;
    LoadCurrent;
  end;
end;

function TfrmRingGroupTree.NodeInfo(ID:Double):String;
var Oblast1,Obstina1,City1,Address1,Raion1:String;
  Oblast2,Obstina2,City2,Raion2:String;
  UpCode:Int64;
  UpCodeD:Double;
  City:Int64;
begin
  Oblast1:='';
  Obstina1:='';
  City1:='';
  Address1:='';
  Raion1:='';
  UpCode:=Trunc(ID);
  while (UpCode>100) and (Oblast1='') or (Obstina1='') or (City1='') or (Address1='') do
  begin
    UpCodeD:=UpCode;
    if srcTree.DataSet.Locate('Code',UpCodeD,[]) then
    begin
      City:=Trunc(dmGUI.CityID(srcTree.DataSet.FieldByName('City1').AsString));
      if (City1='') and (City<>0) then City1:=dmGUI.CityName(City);
      if (Oblast1='') and (City<>0) then Oblast1:=dmGUI.CityName(City div 1000000000);
      if (Obstina1='') and (City<>0) then Obstina1:=dmGUI.CityName(City div 10000000);
      if (Address1='') then Address1:=srcTree.DataSet.FieldByName('Address1').AsString;
      if (Raion1='') then Raion1:=dmGUI.RaionName(srcTree.DataSet.FieldByName('Raion1').AsString);
    end;
    UpCode:=UpCode div 100;
    while (UpCode>100) and (UpCode mod 100=0) do UpCode:=UpCode div 100;
    if UpCode=0 then Break;
  end;

  if not srcTree.DataSet.Locate('Code',ID,[]) then
    Info.Lines.Text:=''
  else
  begin
    if ID>=1E12 then
    begin
      City:=Trunc(dmGUI.CityID(srcTree.DataSet.FieldByName('City2').AsString));
      if City<>0 then
      begin
        City2:=dmGUI.CityName(City);
        Oblast2:=dmGUI.CityName(City div 1000000000);
        Obstina2:=dmGUI.CityName(City div 10000000);
      end else
      begin
        Oblast2:='';
        Obstina2:='';
        City2:='';
      end;
      Raion2:=dmGUI.RaionName(srcTree.DataSet.FieldByName('Raion2').AsString);
      if Raion1<>'' then Raion1:='район '+Raion1+', ';
      if Raion2<>'' then Raion2:='район '+Raion2+', ';

      Info.Lines.Text:=Format(
                            'Сл. адрес: гр./с. %s, общ. %s, обл. %s'#13#10+
                            '%s'#13#10#13#10+
                            'Сл. %s '#13#10+
                            'EMail:%s'#13#10+
                            'Пункт:%s'#13#10#13#10+
                            'Име:%s %s %s'#13#10+
                            'Моб. %s'#13#10+
                            'Дом. %s'#13#10+
                            'Дом. адрес: гр./с. %s, общ. %s, обл. %s'#13#10+
                            '%s'#13#10,[
                            City1,
                            Obstina1,
                            Oblast1,
                            Raion1+Address1,
                            srcTree.DataSet.FieldByName('Phone').AsString,
                            srcTree.DataSet.FieldByName('EMail').AsString,
                            srcTree.DataSet.FieldByName('Site').AsString,
                            srcTree.DataSet.FieldByName('Name1').AsString,
                            srcTree.DataSet.FieldByName('Name2').AsString,
                            srcTree.DataSet.FieldByName('Name3').AsString,
                            srcTree.DataSet.FieldByName('Phone2').AsString,
                            srcTree.DataSet.FieldByName('Phone3').AsString,
                            City2,
                            Obstina2,
                            Oblast2,
                            Raion2+srcTree.DataSet.FieldByName('Address2').AsString
                            ]);
    end else
    begin
      if Raion1<>'' then Raion1:='район '+Raion1+', ';
      Info.Lines.Text:=Format(
                            'Сл. адрес: гр./с. %s, общ. %s, обл. %s'#13#10+
                            '%s'#13#10#13#10,[
                            City1,
                            Obstina1,
                            Oblast1,
                            Raion1+Address1
                            ]);
    end;
  end;
end;

procedure TfrmRingGroupTree.acAddExecute(Sender: TObject);
var F:String;
begin
  if srcRingGroup.DataSet.RecordCount>=Limits.GroupCount then
  begin
    PopControlHint(GroupGrid,'Позволено е дефиниране на максимум '+IntToStr(Limits.GroupCount)+' групи в системата');
    Abort;
  end;
  F:='';
  if not InputQuery('Създаване на група','Име на групата',F) or (F='') then Exit;
  srcRingGroup.DataSet.Append;
  srcRingGroup.DataSet.FieldByName('RingGroupName').AsString:=F;
  srcRingGroup.DataSet.Post;
end;

procedure TfrmRingGroupTree.acAutoCheckExecute(Sender: TObject);
begin
;
end;

procedure TfrmRingGroupTree.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmRingGroupTree.acDeleteExecute(Sender: TObject);
begin
  if not dmGUI.ConfirmDelete then Exit;
  srcRingGroup.DataSet.Delete;
end;

procedure TfrmRingGroupTree.acDownExecute(Sender: TObject);
var I,J:Integer;
begin
  if not contiguous then Exit;

  if ListGroup.selcount>0 then
  begin
    I:=FirstSelectedIndex;
    J:=lastselectedindex+1;
  end
  else
  begin
    I:=ListGroup.itemindex;
    J:=ListGroup.itemindex+1;
  end;

  if (I>=0) and (J<ListGroup.Count) then MoveSelection(I,J)
end;

procedure TfrmRingGroupTree.acRenameExecute(Sender: TObject);
var F:String;
begin
  F:=srcRingGroup.DataSet.FieldByName('RingGroupName').AsString;
  if not InputQuery('Преименуване на група','Име на групата',F) or (F='') then Exit;
  srcRingGroup.DataSet.Edit;
  srcRingGroup.DataSet.FieldByName('RingGroupName').AsString:=F;
  srcRingGroup.DataSet.Post;
end;

procedure TfrmRingGroupTree.acSaveExecute(Sender: TObject);
begin
  SaveCurrent;
end;

procedure TfrmRingGroupTree.acUncheckExecute(Sender: TObject);
begin
  UncheckSelected;
end;

procedure TfrmRingGroupTree.acUpExecute(Sender: TObject);
var I:Integer;
begin
  if not contiguous then Exit;

  if ListGroup.selcount>0 then I:=FirstSelectedIndex
  else I:=ListGroup.itemindex;

  if I>0 then MoveSelection(I,I-1)
end;

procedure TfrmRingGroupTree.FormActivate(Sender: TObject);
begin
  Realign;
  if dmGUI.ReloadOrganisation then
  begin
    Info.Clear;
    Tree.DataSource:=nil;
    Tree.DataSource:=srcTree;
    LoadCurrent;
  end;
end;

procedure TfrmRingGroupTree.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TfrmRingGroupTree.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  dmGUI.RingGroupsEdit.CheckBrowseMode;
  if DataChanged and acSave.Visible then
  case dmGUI.ConfirmExit of
    mrYes:
      begin
        SaveCurrent;
        CanClose:=True;
      end;
    mrNo:
      CanClose:=True;
    else
      CanClose:=False;
  end;
end;

procedure TfrmRingGroupTree.Checking(Sender: TBaseVirtualTree; Node: PVirtualNode; var NewState: TCheckState;
    var Allowed: Boolean);
begin
  if Node.CheckState=csMixedNormal then NewState:=csUncheckedNormal;
end;

procedure TfrmRingGroupTree.FormCreate(Sender: TObject);
begin
  tree:=TCheckVirtualDBTreeEx.Create(Self);
  tree.BorderStyle:=bsNone;
  tree.parent:=PanelTree;
  tree.Align:=alClient;
  tree.CheckImageKind:=ckXP;
  tree.DBOptions:=tree.DBOptions-[dboPathStructure,dboWriteSecondary]+[dboParentStructure,dboReadOnly];
  tree.ParentFieldName:='parent';
  tree.ViewFieldName:='name';
  tree.KeyFieldName:='code';
  tree.OnGetText:=GetTreeText;
  tree.OnCompareNodes:=CompareNodes;
  tree.OnChecking:=Checking;
  tree.TreeOptions.SelectionOptions:=tree.TreeOptions.SelectionOptions+[toFullRowSelect];
  tree.TreeOptions.AutoOptions:=tree.TreeOptions.AutoOptions+[toAutoSort]-[toAutoTristateTracking];
  tree.Header.Columns.Add.Width:=320;
  tree.Header.Columns[0].Text:='Щатно разписание';
  tree.Header.Columns.Add.Width:=110;
  tree.Header.Columns[1].Text:='Код';
  tree.Header.Options:=tree.Header.Options+[hoVisible];
  tree.Header.Style:=hsFlatButtons;
  tree.Header.SortDirection:=sdAscending;
  tree.DataSource:=srcTree;
  tree.OnChecked:=treeChecked;
  tree.OnFocusChanged:=FocusChanged;
  tree.Header.SortColumn:=1;
  DataChanged:=False;

  ListGroup.doublebuffered:=true;
  ListGroup.itemheight:=canvas.textheight('Ay')+5;
  ListGroup.height:=(ListGroup.height div ListGroup.itemheight)*ListGroup.itemheight+8;
  lastitemincrement:=ListGroup.height div ListGroup.itemheight-1;

end;

procedure TfrmRingGroupTree.FocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PDBVTData;
begin
  Data := Tree.GetNodeData(Node);
  NodeInfo(Data.ID);
end;

procedure TfrmRingGroupTree.treeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PDBVTData;
  I:Integer;
  CheckNode,LastChild,X:PVirtualNode;
begin
  if Loading then Exit;

  if Node.CheckState=csCheckedNormal then
  begin
    Data := Tree.GetNodeData(Node);
    if Data<>nil then
    begin
      if not acAutoCheck.Checked then
      begin
        MoveItemID(Data.ID);
      end else
      begin
        tree.OnChecked:=nil;
        try
          CheckNode := Node;
          LastChild := Tree.GetLastChild(CheckNode);
          while LastChild<>nil do
          begin
            X:=Tree.GetLastChild(LastChild);
            if X=nil then Break;
            LastChild:=X;
          end;
          while (CheckNode<>nil) do begin
            if CheckNode<>nil then
            begin
              Data := Tree.GetNodeData(CheckNode);
              if (Data<>nil) and (Data.ID>=1E12) then
              begin
                MoveItemID(Data.ID);
                //CheckNode.CheckState:=csCheckedNormal;
              end else
                Tree.CheckState[CheckNode]:=csMixedNormal;
            end;
            if (CheckNode=LastChild) or (LastChild=nil) then Break;
            CheckNode := Tree.GetNext(CheckNode);
          end;
        finally
          tree.OnChecked:=treeChecked;
        end;
      end;
    end;
    DataChanged:=True;
  end else
  if Node.CheckState=csUncheckedNormal then
  begin
    Data := Tree.GetNodeData(Node);
    if Data<>nil then
    begin
      if not acAutoCheck.Checked then
      begin
        I:=ListGroup.Items.IndexOfObject(TObject(Data));
        if I>=0 then ListGroup.Items.Delete(I);
      end else
      begin
        tree.OnChecked:=nil;
        try
          CheckNode := Node;
          LastChild := Tree.GetLastChild(CheckNode);
          while LastChild<>nil do
          begin
            X:=Tree.GetLastChild(LastChild);
            if X=nil then Break;
            LastChild:=X;
          end;
          while (CheckNode<>nil) do begin
            if CheckNode<>nil then
            begin
              Data := Tree.GetNodeData(CheckNode);
              if (Data<>nil) and (Data.ID>=1E12) then
              begin
                I:=ListGroup.Items.IndexOfObject(TObject(Data));
                if I>=0 then ListGroup.Items.Delete(I);
              end;
              Tree.CheckState[CheckNode]:=csUncheckedNormal;
            end;
            if (CheckNode=LastChild) or (LastChild=nil) then Break;
            CheckNode := Tree.GetNext(CheckNode);
          end;
        finally
          tree.OnChecked:=treeChecked;
        end;
      end;
    end;
    DataChanged:=True;
  end;

end;

type MyTree=class(TBaseVirtualDBTreeEx);

procedure TfrmRingGroupTree.UncheckSelected;
var
  I:Integer;
  N:PVirtualNode;
begin
  for I:= ListGroup.Count - 1 downto 0 do
  if ListGroup.Selected[I] then
  begin
    DataChanged := True;
    if ListGroup.Items.Objects[I]<>nil then
    begin
      N:=MyTree(tree).findnode(nil,PDBVTData(ListGroup.Items.Objects[I]).ID);
      if N<>nil then
        Tree.CheckState[N]:=csUncheckedNormal;
    end;
    //ListGroup.Items.Delete(I);
  end;
end;

procedure TfrmRingGroupTree.UpdateCheckActions;
begin

end;

procedure TfrmRingGroupTree.FormDestroy(Sender: TObject);
begin
  frmRingGroupTree:=nil;
end;

procedure TfrmRingGroupTree.FormShow(Sender: TObject);
var N:PVirtualNode;
  Data: PDBVTData;
begin
  dmGUI.RingGroupsEdit.Open;
  dmGUI.OrganisationsList.Open;
  dmGUI.City.Open;
  dmGUI.CityList.Open;
  dmGUI.Raion.Open;
  srcTree.DataSet:=dmGUI.OrganisationsList;
  srcRingGroup.Dataset:=dmGUI.RingGroupsEdit;
  N:=Tree.GetFirst;
  while N<>nil do
  begin
    Data:=Tree.GetNodeData(N);
    if (Data<>nil) and (Data.ID<1E12) then
      N.CheckType:=ctTriStateCheckBox;
    N:=Tree.GetNext(N);
  end;
end;


procedure TfrmRingGroupTree.MoveItemID(ID: Double);
var I:PVirtualNode;
begin
  I:=MyTree(tree).findnode(nil,ID);
  if I<>nil then
  begin
    if ListGroup.Items.IndexOfObject(tree.GetNodeData(I))<0 then
      ListGroup.Items.AddObject(tree.NodeText[I],tree.GetNodeData(I));
    tree.CheckState[I]:=csCheckedNormal;
    tree.FullyVisible[I]:=True;
  end else
    ListGroup.Items.Add(FloatToStr(ID));
end;

procedure TfrmRingGroupTree.ClearChecked;
var N:PVirtualNode;
begin
  N:=tree.GetFirstChecked(csCheckedNormal);
  while N<>nil do
  begin
    tree.CheckState[N]:=csUncheckedNormal;
    N:=tree.GetNextChecked(N,csCheckedNormal);
  end;
  N:=tree.GetFirstChecked(csMixedNormal);
  while N<>nil do
  begin
    tree.CheckState[N]:=csUncheckedNormal;
    N:=tree.GetNextChecked(N,csMixedNormal);
  end;
end;

procedure TfrmRingGroupTree.CompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
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

procedure TfrmRingGroupTree.LoadCurrent;
var Person:TField;
begin
  Loading:=True;
  try
    ClearChecked;
    tree.ClearSelection;
    tree.FullCollapse;

    ListGroup.Items.Clear;
    if dmGUI.RingGroupsEdit.IsEmpty then
    begin
      tree.Visible:=False;
    end else
    begin
      RingGroup:=dmGUI.RingGroupsEdit.FieldByName('ID').AsInteger;
      dmGUI.RingGroupMembersEdit.Close;
      dmGUI.RingGroupMembersEdit.CommandText:='select * from tblRingGroupMembers where RingGroup='+IntToStr(RingGroup)+' order by Priority,Person';
      dmGUI.RingGroupMembersEdit.Open;
      Person:=dmGUI.RingGroupMembersEdit.FieldByName('Person');
      while not dmGUI.RingGroupMembersEdit.Eof do
      begin
        MoveItemID(Person.AsFloat);
        dmGUI.RingGroupMembersEdit.Next;
      end;
      tree.Visible:=True;
    end;
  finally
    Loading:=False;
  end;
  DataChanged:=False;
  UpdateCheckActions;
end;

procedure TfrmRingGroupTree.SaveCurrent;
var I:Integer;
    Group,Person,Priority:TField;
begin
  dmGUI.ChangeVersion('Settings');
  dmGUI.Connection.Execute('delete from tblRingGroupMembers where RingGroup='+IntToStr(RingGroup));
  if ListGroup.Count<=0 then
  begin
    DataChanged:=False;
    Exit;
  end;

  dmGUI.RingGroupMembersEdit.Close;
  dmGUI.RingGroupMembersEdit.CommandText:='select * from tblRingGroupMembers where 1=2';
  dmGUI.RingGroupMembersEdit.Open;
  Group:=dmGUI.RingGroupMembersEdit.FieldByName('RingGroup');
  Person:=dmGUI.RingGroupMembersEdit.FieldByName('Person');
  Priority:=dmGUI.RingGroupMembersEdit.FieldByName('Priority');
  for I:= 0 to ListGroup.Count - 1 do
  begin
    dmGUI.RingGroupMembersEdit.Append;
    Group.AsInteger:=RingGroup;
    if ListGroup.Items.Objects[I]=nil then
      Person.AsString:=ListGroup.Items[I]
    else
      Person.AsFloat:=PDBVTData(ListGroup.Items.Objects[I]).ID;
    Priority.AsInteger:=I+1;
    dmGUI.RingGroupMembersEdit.Post;
  end;
  DataChanged:=False;
end;

procedure TfrmRingGroupTree.ScrollListTimer(Sender: TObject);
var
  newindex:integer;
begin
  {After first scroll, set scroll rate at .25 seconds per item}
  scrollList.interval:=250;
  with ListGroup do
  begin
    newindex:=ListGroup.topindex+scrollamt;
    if (newindex>=0) and (newindex<items.count)
    then
    begin
      previndex:=dropindex;
      if (dropindex=0) or (dropindex=items.count-1)
        or (itematpos(screentoclient(mouse.cursorpos),true)>=0)
      then stoptimer
      else
      begin
        if scrollamt>0 then dropindex:=newindex+lastitemincrement
        else dropindex:=newindex;
      end;
      topindex:=newindex;
      redrawitems;
    end;
  end;
end;

{********** Stoptimer ********}
procedure TfrmRingGroupTree.Stoptimer;
{We have a couple of things to do when scrolling is stopped, and we do it from
 several places, so might as well put it in a procedure}
begin
  with scrollList do
  begin
    enabled:=false;
    interval:=500;
  end;
end;

{************* Redrawitems ************}
procedure TfrmRingGroupTree.Redrawitems;
{now force the old and new "dragto" items to be redrawn}
begin
  if (draggedindex<>dropindex) then {only draw if not dropping on self}
  with ListGroup do
  begin
    if (previndex>=0) and (previndex<ListGroup.Count) then ListGroupDrawItem(ListGroup, previndex, Itemrect(previndex), []);
    if (dropindex <>previndex) then
    begin {only redraw current drop if different than previous draw}
      {for example when mosue moves within an item prev and current may be the same}
      if (dropindex>=0) and (dropindex<ListGroup.Count) then ListGroupDrawItem(ListGroup, dropindex, Itemrect(dropindex), []);
      previndex:=dropindex;
    end;
  end;
end;

procedure TfrmRingGroupTree.MoveSelection(Source,Destination:Integer);
var
  i: Integer;
begin
  if (Source<0) or (Source>=ListGroup.Count) then Exit;
  if (Destination<0) or (Destination>=ListGroup.Count) then Exit;

  //ListGroup.itemindex:=Destination;

  DataChanged := True;
  if ListGroup.SelCount>1 then
  begin
    if not contiguous then
      raise TMessageException.Create('Само непрекъснати блокове може да бъдат местени !')
    else
    for i := 0 to ListGroup.selcount - 1 do
    begin
      ListGroup.selected[Source] := false;
      ListGroup.items.move(Source, Destination);
      ListGroup.selected[Destination] := true;
      if Destination < Source then
      begin
        inc(Destination);
        inc(Source);
      end;
    end
  end else
  begin
    ListGroup.items.move(Source, Destination);
    ListGroup.itemindex:=Destination;
    ListGroup.Selected[Destination]:=true;
  end;
  UpdateCheckActions;
end;

procedure TfrmRingGroupTree.ListGroupMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var Src,Dest:Integer;
begin
  if not dragflag then exit;
  stoptimer;

  try
    Src:=draggedindex;
    Dest:=dropindex;
    draggedindex:=-1;
    dropindex:=-1;
    MoveSelection(Src,Dest);
  finally
    releasecapture;
    dragflag:=false;
  end;
end;


procedure TfrmRingGroupTree.ListGroupClick(Sender: TObject);
var N:PVirtualNode;
begin
  if ListGroup.ItemIndex>=0 then
  if ListGroup.Items.Objects[ListGroup.ItemIndex]<>nil then
  begin
    N:=MyTree(tree).findnode(nil,PDBVTData(ListGroup.Items.Objects[ListGroup.ItemIndex]).ID);
    if N<>nil then
    begin
      tree.ClearSelection;
      tree.Selected[N]:=True;
    end;
    tree.FocusedNode:=N;
  end;
end;

procedure TfrmRingGroupTree.ListGroupDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  if (odselected in state) then ListGroup.Canvas.brush.color:=clHighlight
  else ListGroup.Canvas.brush.color:=ListGroup.color;
  ListGroup.Canvas.fillrect(rect);
  if (odselected in state) then
    ListGroup.Canvas.font.Color:=clWhite
  else
    ListGroup.Canvas.font.Color:=clBlack;
  ListGroup.Canvas.textout(rect.left+2, rect.top+2, ListGroup.items[index]);
  if dragflag and (index=dropindex) then
  begin
    ListGroup.Canvas.pen.width:=2;
    ListGroup.Canvas.pen.color:=clred;
    if index>draggedindex then
    begin
      ListGroup.Canvas.moveto(rect.left, rect.bottom-2);
      ListGroup.Canvas.lineto(rect.right,rect.bottom-2);
    end
    else
    begin
      ListGroup.Canvas.moveto(rect.left, rect.top+2);
      ListGroup.Canvas.lineto(rect.right,rect.top+2);
    end;
    ListGroup.Canvas.pen.width:=1;
    ListGroup.Canvas.pen.color:=clBlack;
  end;
end;

procedure TfrmRingGroupTree.ListGroupKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=vk_Delete then
  begin
    UncheckSelected;
    Key:=0;
  end;
end;

procedure TfrmRingGroupTree.ListGroupKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key='+') and acUp.Execute then Key:=#0;
  if (Key='-') and acDown.Execute then Key:=#0;  
end;

function TfrmRingGroupTree.FirstSelectedIndex:integer;
var
  i:integer;
begin
  i:=0;
  result:=-1;
  with ListGroup do
  while (i<items.count) and (result<0) do
  begin
    if selected[i] then result:=i;
    inc(i);
  end;
end;

function TfrmRingGroupTree.LastSelectedIndex:integer;
var
  i:integer;
begin
  i:=ListGroup.items.count-1;
  result:=-1;
  with ListGroup do
  while (i>=0) and (result<0) do
  begin
    if selected[i] then result:=i;
    dec(i);
  end;
end;

procedure TfrmRingGroupTree.ListGroupMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if  ([ssshift,ssctrl,ssleft]*shift<>[])  then exit;
  with ListGroup do
  if selcount>0 then draggedindex:=FirstSelectedIndex
  else draggedindex:=ListGroup.itemindex;

  dropindex:=-1;
  previndex:=-1;

  //ListGroup.itemindex:=-1;

  dragflag:=true;
  setcapturecontrol(ListGroup);
  stoptimer;

end;

procedure TfrmRingGroupTree.ListGroupMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var I:Integer;
begin
  if not dragflag then exit;
  with ListGroup do
  begin
    {if mouse is back within bounds, then stop auto-scrolling}
    if (y>=0) and (y<=height) and scrolllist.enabled then stoptimer;
    if not (ScrollList.enabled) then
    begin  {Scrolling is not taking place}

      if (y<0) or (y>=height) then {start scrolling}
      begin {Mouse has moved outside of field, scroll the listbox as appropriate}
        if (y>height) and (not scrollList.enabled) and (dropindex<items.count-1) then
        begin
          scrollamt:=+1;
          scrollList.enabled:=true;
        end
        else
        {if mouse is high and  not at top then start scroll up }
        if (y<0) and (not scrollList.enabled) and (topindex>0)then
        begin
          scrollamt:=-1;
          if  (topindex>0) then scrollList.enabled:=true;
        end;
      end
      else {mouse is in range}
      begin
        I:=itemAtPos(point(0,y),true);
        if (I>=0) and not selected[I] then
        begin
          dropindex:=I;
          if dropindex<0 then dropindex:=topindex+lastitemincrement;
          if scrollList.enabled then stoptimer;
          RedrawItems;
        end else
          RedrawItems;
      end;
    end;
  end;
end;

function TfrmRingGroupTree.contiguous:boolean;
{check that multiple selected items are contiguous}
var
  i,n:integer;
begin
  result:=true;
  with ListGroup do
  begin
    if selcount<=1 then exit;
    n:=-1;
    i:=0;
    while i<items.count do
    begin
      if selected[i] then {found next selected item}
      begin
        if (n>=0) and (i<>n+1) then
        begin  {oops, not contigous!}
          result:=false;
          break;
        end;
        n:=i; {save previous selected item index}
      end;
      inc(i);
    end;
  end;
end;

procedure TfrmRingGroupTree.GroupGridExit(Sender: TObject);
begin
  dmGUI.RingGroupsEdit.CheckBrowseMode;
end;

end.
