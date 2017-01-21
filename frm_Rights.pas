unit frm_Rights;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ToolWin, ActnMan, ActnCtrls, Grids, DBGrids, DB, ActnList,
  XPStyleActnCtrls, ExtCtrls, ComCtrls, Tabs, ADODB;

type
  TfrmRights = class(TForm)
    DBGrid1: TDBGrid;
    srcWorkGroup: TDataSource;
    ActionManager1: TActionManager;
    acClose: TAction;
    ActionToolBar1: TActionToolBar;
    Rights: TListView;
    Splitter1: TSplitter;
    acCheck: TAction;
    acUncheck: TAction;
    Panel1: TPanel;
    Groups: TTabSet;
    Q: TADOQuery;
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acCloseExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure srcWorkGroupDataChange(Sender: TObject; Field: TField);
    procedure RightsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure RightsEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure acCheckExecute(Sender: TObject);
    procedure acUncheckExecute(Sender: TObject);
    procedure RightsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure GroupsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RightsChanging(Sender: TObject; Item: TListItem;
      Change: TItemChange; var AllowChange: Boolean);
  private
    GroupRight:String;
    procedure LoadCurrent;
    procedure LoadRights;
    procedure LoadGroups;
  public
    { Public declarations }
  end;

var
  frmRights: TfrmRights;

implementation
uses dm_GUI;

{$R *.dfm}

procedure TfrmRights.acCheckExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Rights.Items.Count - 1 do
    if Rights.Items[I].Selected and not Rights.Items[I].Checked then
      Rights.Items[I].Checked:=True;
end;

procedure TfrmRights.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmRights.acUncheckExecute(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Rights.Items.Count - 1 do
    if Rights.Items[I].Selected and Rights.Items[I].Checked then
      Rights.Items[I].Checked:=False;
end;

procedure TfrmRights.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
  dmGUI.RightsEdit.Close;
  Rights.OnChange:=nil; // fired when some items are selected and the form is closing
  if dmGUI.RightsList.Active then dmGUI.RightsList.Requery;
end;

procedure TfrmRights.FormCreate(Sender: TObject);
begin
  Q.Connection:=dmGUI.RightsList.Connection;
end;

procedure TfrmRights.FormDestroy(Sender: TObject);
begin
  frmRights:=nil;
end;

procedure TfrmRights.FormShow(Sender: TObject);
begin
  LoadGroups;
  dmGUI.RightsEdit.Open;
  Groups.TabIndex:=0;
  srcWorkGroup.Dataset:=dmGUI.WorkGroupList;
end;

procedure TfrmRights.GroupsClick(Sender: TObject);
begin
  LoadRights;
  LoadCurrent;
end;

procedure TfrmRights.LoadCurrent;
var
  I: Integer;
  WorkGroup:Integer;
begin
  Rights.OnChange:=nil;
  try
    Rights.ClearSelection;
    WorkGroup:=dmGUI.WorkGroupList.FieldByName('ID').AsInteger;
    for I := 0 to Rights.Items.Count - 1 do
    begin
      if GroupRight='' then
        Rights.Items[I].Checked:=(WorkGroup=1) or dmGUI.RightsEdit.Locate('RightAbbr;WorkGroup',VarArrayOf([Trim(Rights.Items[I].SubItems.Text),WorkGroup]),[])
      else
        Rights.Items[I].Checked:=(WorkGroup=1) or dmGUI.RightsEdit.Locate('RightAbbr;WorkGroup;RightID',VarArrayOf([GroupRight,WorkGroup,Trim(Rights.Items[I].SubItems.Text)]),[])
    end;
  finally
    Rights.OnChange:=RightsChange;
  end;
  Rights.Enabled:=WorkGroup>1;
  if Rights.Enabled then
    Rights.Color:=clWindow
  else
    Rights.Color:=clBtnFace;
end;

procedure TfrmRights.LoadGroups;
var Field:TField;
begin
  dmGUI.NomRightsList.Close;
  dmGUI.NomRightsList.Open;
  Groups.Tabs.Clear;
  while not dmGUI.NomRightsList.Eof  do
  begin
    if dmGUI.NomRightsList.FieldByName('LookID').AsString='' then
        Field:=dmGUI.NomRightsList.FieldByName('LookTable')
      else
        Field:=dmGUI.NomRightsList.FieldByName('RightName');

    if Groups.Tabs.IndexOf(Field.AsString)<0 then
      Groups.Tabs.Add(Field.AsString);
    dmGUI.NomRightsList.Next;
  end;
end;

procedure TfrmRights.LoadRights;
var
  L: TListItem;
  Group,Where:String;
  Field:TField;
begin
  dmGUI.NomRightsList.Close;
  dmGUI.NomRightsList.Open;
  Rights.OnChange:=nil;
  try
    Rights.Clear;
    Group:=Groups.Tabs[Groups.TabIndex];
    while not dmGUI.NomRightsList.Eof  do
    begin
      if dmGUI.NomRightsList.FieldByName('LookID').AsString='' then
        Field:=dmGUI.NomRightsList.FieldByName('LookTable')
      else
        Field:=dmGUI.NomRightsList.FieldByName('RightName');
      if Field.AsString=Group then
      begin
        if SameText(Field.FieldName,'LookTable') then
        begin
          GroupRight:='';
          L:=Rights.Items.Add;
          L.Caption:=dmGUI.NomRightsList.FieldByName('RightName').AsString;
          L.SubItems.Text:=dmGUI.NomRightsList.FieldByName('RightAbbr').AsString;
        end else
        begin
          GroupRight:=dmGUI.NomRightsList.FieldByName('RightAbbr').AsString;
          Q.Close;
          Where:=dmGUI.NomRightsList.FieldByName('LookFilter').AsString;
          if Where<>'' then Where:='where '+Where;
          Q.SQL.Text:=Format('select %s id,%s txt from %s %s',[
               dmGUI.NomRightsList.FieldByName('LookID').AsString,
               dmGUI.NomRightsList.FieldByName('LookText').AsString,
               dmGUI.NomRightsList.FieldByName('LookTable').AsString,
               Where
             ]);
          Q.Open;
          Q.First;
          while not Q.Eof do
          begin
            L:=Rights.Items.Add;
            L.Caption:=Q.FieldByName('Txt').AsString;
            L.SubItems.Text:=Q.FieldByName('ID').AsString;
            Q.Next;
          end;
          Break;
        end;
      end;
      dmGUI.NomRightsList.Next;
    end;
  finally
    Rights.OnChange:=RightsChange;
  end;
end;

procedure TfrmRights.RightsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  WorkGroup: Integer;
begin
  if Change=ctState then
  begin
    WorkGroup:=dmGUI.WorkGroupList.FieldByName('ID').AsInteger;
    if GroupRight='' then
    begin
      if Item.Checked then
      begin
        if not dmGUI.RightsEdit.Locate('RightAbbr;WorkGroup',VarArrayOf([Trim(Item.SubItems.Text),WorkGroup]),[]) then
        begin
          dmGUI.RightsEdit.Append;
          dmGUI.RightsEdit.FieldByName('WorkGroup').AsInteger:=WorkGroup;
          dmGUI.RightsEdit.FieldByName('RightAbbr').AsString:=Trim(Item.SubItems.Text);
          dmGUI.RightsEdit.FieldByName('RightID').AsString:='*';
          dmGUI.RightsEdit.Post;
        end;
      end else
      begin
        if dmGUI.RightsEdit.Locate('RightAbbr;WorkGroup',VarArrayOf([Trim(Item.SubItems.Text),WorkGroup]),[]) then
           dmGUI.RightsEdit.Delete;
      end;
    end else
    begin
      if Item.Checked then
      begin
        if not dmGUI.RightsEdit.Locate('RightAbbr;WorkGroup;RightID',VarArrayOf([GroupRight,WorkGroup,Trim(Item.SubItems.Text)]),[]) then
        begin
          dmGUI.RightsEdit.Append;
          dmGUI.RightsEdit.FieldByName('WorkGroup').AsInteger:=WorkGroup;
          dmGUI.RightsEdit.FieldByName('RightAbbr').AsString:=GroupRight;
          dmGUI.RightsEdit.FieldByName('RightID').AsString:=Trim(Item.SubItems.Text);
          dmGUI.RightsEdit.Post;
        end;
      end else
      begin
        if dmGUI.RightsEdit.Locate('RightAbbr;WorkGroup;RightID',VarArrayOf([GroupRight,WorkGroup,Trim(Item.SubItems.Text)]),[]) then
           dmGUI.RightsEdit.Delete;
      end;
    end;
  end;
end;

procedure TfrmRights.RightsChanging(Sender: TObject; Item: TListItem;
  Change: TItemChange; var AllowChange: Boolean);
begin
  AllowChange:=(Change<>ctState) or
               ((Item.Checked and acUncheck.Visible) or
                (not Item.Checked and acCheck.Visible));
end;

procedure TfrmRights.RightsEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  AllowEdit:=False;
end;

procedure TfrmRights.RightsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  acCheck.Enabled:=Rights.SelCount>0;
  acUnCheck.Enabled:=Rights.SelCount>0;
end;

procedure TfrmRights.srcWorkGroupDataChange(Sender: TObject; Field: TField);
begin
  if Field=nil then LoadCurrent;  
end;

end.
