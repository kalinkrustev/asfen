unit frm_RingGroup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ActnMan, ActnCtrls, Grids, DBGrids, DB, ActnList,
  XPStyleActnCtrls, ExtCtrls, ComCtrls, StdCtrls, ButtonGroup;

type
  TfrmRingGroup = class(TForm)
    DBGrid1: TDBGrid;
    srcRingGroup: TDataSource;
    ActionManager1: TActionManager;
    acSave: TAction;
    acAdd: TAction;
    acDelete: TAction;
    acClose: TAction;
    ActionToolBar1: TActionToolBar;
    Splitter1: TSplitter;
    acCheck: TAction;
    acUncheck: TAction;
    Panel1: TPanel;
    Panel2: TPanel;
    ListPerson: TListBox;
    ListGroup: TListBox;
    Toolbar2: TActionToolBar;
    Panel3: TPanel;
    LabelPerson: TStaticText;
    LabelGroup: TStaticText;
    ScrollList: TTimer;
    acUp: TAction;
    acDown: TAction;
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acCloseExecute(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure srcRingGroupDataChange(Sender: TObject; Field: TField);
    procedure acCheckExecute(Sender: TObject);
    procedure acUncheckExecute(Sender: TObject);
    procedure frmScriptacCloseExecute(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Panel3Resize(Sender: TObject);
    procedure ListPersonClick(Sender: TObject);
    procedure ListPersonDblClick(Sender: TObject);
    procedure ListGroupDblClick(Sender: TObject);
    procedure ListPersonKeyPress(Sender: TObject; var Key: Char);
    procedure ListGroupKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acSaveExecute(Sender: TObject);
    procedure ListGroupMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListGroupMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListGroupMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListGroupDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ScrollListTimer(Sender: TObject);
    procedure acUpExecute(Sender: TObject);
    procedure acDownExecute(Sender: TObject);
    procedure DBGrid1Exit(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    Panel3Width:Integer;
    RingGroup:Integer;
    FChanged:Boolean;

    draggedindex:integer;
    dropindex, previndex:integer;
    lastitemincrement:integer;
    dragflag:boolean;
    scrollamt:integer;

    procedure MoveItems(Src,Dest:TListBox);
    procedure MoveItemID(ID:Integer);
    procedure LoadCurrent;
    procedure SaveCurrent;
    procedure LoadUsers;
    procedure UpdateCheckActions;
    procedure SetChanged(const Value: Boolean);
    procedure Redrawitems;
    procedure Stoptimer;
    function FirstSelectedIndex: integer;
    function contiguous: boolean;
    function LastSelectedIndex: integer;
    property  DataChanged:Boolean read FChanged write SetChanged;
    procedure MoveSelection(Source,Destination:Integer);
  public
    { Public declarations }
  end;

var
  frmRingGroup: TfrmRingGroup;

implementation
uses dm_GUI;

{$R *.dfm}

procedure TfrmRingGroup.acAddExecute(Sender: TObject);
begin
  srcRingGroup.DataSet.Append;
end;

procedure TfrmRingGroup.acCheckExecute(Sender: TObject);
begin
  MoveItems(ListPerson,ListGroup);
  UpdateCheckActions;
end;

procedure TfrmRingGroup.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmRingGroup.acDeleteExecute(Sender: TObject);
begin
  if not dmGUI.ConfirmDelete then Exit;
  srcRingGroup.DataSet.Delete;
end;

procedure TfrmRingGroup.acDownExecute(Sender: TObject);
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

procedure TfrmRingGroup.acSaveExecute(Sender: TObject);
begin
  SaveCurrent;
end;

procedure TfrmRingGroup.acUncheckExecute(Sender: TObject);
begin
  MoveItems(ListGroup,ListPerson);
  UpdateCheckActions;
end;

procedure TfrmRingGroup.acUpExecute(Sender: TObject);
var I:Integer;
begin
  if not contiguous then Exit;

  if ListGroup.selcount>0 then I:=FirstSelectedIndex
  else I:=ListGroup.itemindex;

  if I>0 then MoveSelection(I,I-1)
end;

procedure TfrmRingGroup.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  dmGUI.RingGroupsEdit.CheckBrowseMode;
  dmGUI.RingGroupsEdit.Close;
  dmGUI.RingGroupMembersEdit.Close;
  Action:=caFree;
end;

procedure TfrmRingGroup.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if DataChanged then
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

procedure TfrmRingGroup.FormCreate(Sender: TObject);
begin
  Panel3Width:=Panel3.Width;
  DataChanged:=False;

  ListGroup.doublebuffered:=true;
  ListGroup.itemheight:=canvas.textheight('Ay')+5;
  ListGroup.height:=(ListGroup.height div ListGroup.itemheight)*ListGroup.itemheight+8;
  ListPerson.ItemHeight:=ListGroup.itemheight;
  lastitemincrement:=ListGroup.height div ListGroup.itemheight-1;

end;

procedure TfrmRingGroup.FormDeactivate(Sender: TObject);
begin
  if dmGUI.RingGroupsList.Active then dmGUI.RingGroupsList.Requery;
end;

procedure TfrmRingGroup.FormDestroy(Sender: TObject);
begin
  frmRingGroup:=nil;
end;

procedure TfrmRingGroup.FormShow(Sender: TObject);
begin
  dmGUI.RingGroupsEdit.Open;
  srcRingGroup.Dataset:=dmGUI.RingGroupsEdit;
end;

procedure TfrmRingGroup.frmScriptacCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmRingGroup.ListGroupDblClick(Sender: TObject);
begin
  AcUncheck.Execute;
end;

procedure TfrmRingGroup.ListGroupDrawItem(Control: TWinControl; Index: Integer;
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

procedure TfrmRingGroup.ListGroupKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key=#13) and AcUnCheck.Execute then Key:=#0;
  if (Key='+') and acUp.Execute then Key:=#0;
  if (Key='-') and acDown.Execute then Key:=#0;  
end;

function TfrmRingGroup.FirstSelectedIndex:integer;
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

function TfrmRingGroup.LastSelectedIndex:integer;
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

procedure TfrmRingGroup.ListGroupMouseDown(Sender: TObject;
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

procedure TfrmRingGroup.ListGroupMouseMove(Sender: TObject; Shift: TShiftState;
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

function TfrmRingGroup.contiguous:boolean;
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

procedure TfrmRingGroup.DBGrid1Exit(Sender: TObject);
begin
  if srcRingGroup.DataSet<>nil then
    srcRingGroup.DataSet.CheckBrowseMode;
end;

procedure TfrmRingGroup.MoveSelection(Source,Destination:Integer);
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

procedure TfrmRingGroup.ListGroupMouseUp(Sender: TObject; Button: TMouseButton;
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

procedure TfrmRingGroup.ListPersonClick(Sender: TObject);
begin
  UpdateCheckActions;
end;

procedure TfrmRingGroup.ListPersonDblClick(Sender: TObject);
begin
  AcCheck.Execute;
end;

procedure TfrmRingGroup.ListPersonKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key=#13) and AcCheck.Execute then Key:=#0;
end;

procedure TfrmRingGroup.LoadCurrent;
var Person:TField;
begin
  DataChanged:=False;
  LoadUsers;
  ListGroup.Items.Clear;
  RingGroup:=dmGUI.RingGroupsEdit.FieldByName('ID').AsInteger;
  dmGUI.RingGroupMembersEdit.Close;
  dmGUI.RingGroupMembersEdit.CommandText:='select * from tblRingGroupMembers where RingGroup='+IntToStr(RingGroup)+' order by Priority,Person';
  dmGUI.RingGroupMembersEdit.Open;
  Person:=dmGUI.RingGroupMembersEdit.FieldByName('Person');
  while not dmGUI.RingGroupMembersEdit.Eof do
  begin
    MoveItemID(Person.AsInteger);
    dmGUI.RingGroupMembersEdit.Next;
  end;
  UpdateCheckActions;
end;

procedure TfrmRingGroup.LoadUsers;
begin
  dmGUI.PersonList.Close;
  dmGUI.PersonList.Open;

  ListPerson.Clear;
  while not dmGUI.PersonList.Eof  do
  begin
    ListPerson.Items.AddObject(dmGUI.PersonList.FieldByName('FullName').AsString,TObject(dmGUI.PersonList.FieldByName('ID').AsInteger));
    dmGUI.PersonList.Next;
  end;
end;

procedure TfrmRingGroup.MoveItemID(ID: Integer);
var I:Integer;
begin
  I:=ListPerson.Items.IndexOfObject(TObject(ID));
  if I>=0 then
  begin
    ListGroup.Items.AddObject(ListPerson.Items[I],ListPerson.Items.Objects[I]);
    ListPerson.Items.Delete(I);
  end;
end;

procedure TfrmRingGroup.MoveItems(Src, Dest: TListBox);
var
  I: Integer;
begin
  DataChanged:=True;
  if Src.SelCount<=0 then Exit;
  I:=0;
  while I<Src.Count do
  begin
    if Src.Selected[I] then
    begin
      Dest.Items.AddObject(Src.Items[I],Src.Items.Objects[I]);
      Src.Items.Delete(I);
    end
    else
      I:=I+1;
  end;
end;

procedure TfrmRingGroup.Panel1Resize(Sender: TObject);
begin
  ListPerson.Width:=(Panel1.Width-Panel3Width) div 2;
  LabelPerson.Width:=ListPerson.Width;
  LabelGroup.Left:=ListGroup.Left;
  LabelGroup.Width:=ListGroup.Width;
end;

procedure TfrmRingGroup.Panel3Resize(Sender: TObject);
begin
  Toolbar2.Top:=(Panel3.Height-Toolbar2.Height) div 2;
end;

procedure TfrmRingGroup.SaveCurrent;
var I:Integer;
    Group,Person,Priority:TField;
begin
  dmGUI.Connection.Execute('delete from tblRingGroupMembers where RingGroup='+IntToStr(RingGroup));
  if ListGroup.Count<=0 then Exit;

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
    Person.AsInteger:=Integer(ListGroup.Items.Objects[I]);
    Priority.AsInteger:=I+1;
    dmGUI.RingGroupMembersEdit.Post;
  end;
  DataChanged:=False;
end;

procedure TfrmRingGroup.ScrollListTimer(Sender: TObject);
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
procedure TfrmRingGroup.Stoptimer;
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
procedure TfrmRingGroup.Redrawitems;
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

procedure TfrmRingGroup.SetChanged(const Value: Boolean);
begin
  FChanged := Value;
  acSave.Enabled:=FChanged;
end;

procedure TfrmRingGroup.srcRingGroupDataChange(Sender: TObject; Field: TField);
begin
  if Field=nil then
  begin
    if DataChanged and dmGUI.Confirm('Желаете ли запис на промените?') then SaveCurrent;
    LoadCurrent;
  end;
end;

procedure TfrmRingGroup.UpdateCheckActions;
begin
  acCheck.Enabled:=ListPerson.SelCount>0;
  acUnCheck.Enabled:=ListGroup.SelCount>0;
  acUp.Enabled:=FirstSelectedIndex>0;
  acDown.Enabled:=(LastSelectedIndex>=0) and (LastSelectedIndex<ListGroup.Count-1); 
end;

end.
