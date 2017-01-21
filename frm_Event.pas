unit frm_Event;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, ActnMan, ActnCtrls, ActnList, XPStyleActnCtrls,
  ExtCtrls, StdCtrls, DBCtrls, DB, SIP_Event, Grids, ValEdit, SIP_Script,
  Buttons;

type
  TfrmEvent = class(TForm)
    ActionManager1: TActionManager;
    acSave: TAction;
    acAdd: TAction;
    acDelete: TAction;
    acClose: TAction;
    ActionToolBar1: TActionToolBar;
    Scripts: TListView;
    Splitter1: TSplitter;
    srcScripts: TDataSource;
    srcRingGroups: TDataSource;
    Panel1: TPanel;
    EventName: TEdit;
    Label11: TLabel;
    ScriptSelect: TDBLookupComboBox;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    RingGroupSelect: TDBLookupComboBox;
    Label3: TLabel;
    Count: TEdit;
    Panel3: TPanel;
    StaticText1: TStaticText;
    Panel4: TPanel;
    StaticText2: TStaticText;
    Panel5: TPanel;
    StaticText3: TStaticText;
    Params: TValueListEditor;
    EventKind: TRadioGroup;
    EventID: TEdit;
    Label15: TLabel;
    Panel6: TPanel;
    srcEventType: TDataSource;
    CrisisKind: TDBLookupComboBox;
    btnCity1: TSpeedButton;
    Label4: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acCloseExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure ScriptsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure acDeleteExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure ScriptSelectClick(Sender: TObject);
    procedure CountExit(Sender: TObject);
    procedure EventKindClick(Sender: TObject);
    procedure RingGroupSelectKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CountChange(Sender: TObject);
    procedure ParamsStringsChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure CrisisKindClick(Sender: TObject);
  private
    Event:TSIPEvent;
    Changing:Boolean;
    FChanged: Boolean;
    Script:TSIPScript;
    procedure ListScript;
    procedure ShowParams;
    procedure EnableGroup(SIPEvent:TSIPEventScript);
    procedure UpdateList(Item:TListItem;SIPEvent:TSIPEventScript);
    procedure ShowAction(SIPEvent:TSIPEventScript);
    procedure UpdateSIPEventScript;
    procedure SetChanged(const Value: Boolean);
  public
    procedure LoadEvent(FileName:String);
    property  DataChanged:Boolean read FChanged write SetChanged;
  end;

implementation
uses dm_GUI, dm_AlarmDB, dm_Images, frm_GenericGrid;
{$R *.dfm}

procedure TfrmEvent.acAddExecute(Sender: TObject);
begin
  Event.Add;
  DataChanged:=True;
  ListScript;
  Scripts.ItemIndex:=Scripts.Items.Count-1;
end;

procedure TfrmEvent.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmEvent.acDeleteExecute(Sender: TObject);
var I:Integer;
begin
  I:=Scripts.ItemIndex;
  if (I>=0) and (I<Scripts.Items.Count) then
  begin
    if not dmGUI.ConfirmDelete then Exit;
    Event.Delete(I);
    DataChanged:=True;
    ListScript;
    if (I>=0) and (I<Scripts.Items.Count) then
      Scripts.ItemIndex:=I else
    if (I>0) and (I=Scripts.Items.Count) then
      Scripts.ItemIndex:=I-1
  end;
end;

procedure TfrmEvent.acSaveExecute(Sender: TObject);
begin
  UpdateSIPEventScript;
  Event.Mode:=dmGUI.DatabaseID;
  Event.EventKind:=EventKind.ItemIndex;
  Event.Parameters.Clear;
  Event.Parameters.AddStrings(Params.Strings);
  if not VarIsNull(CrisisKind.KeyValue) then
    Event.CrisisKind:=CrisisKind.KeyValue
  else
    Event.CrisisKind:=0;
    
  dmGUI.SaveFile('събития',EventName.Text,Event.Text,True);
  DataChanged:=False;
end;

procedure TfrmEvent.CountChange(Sender: TObject);
begin
  DataChanged:=True;
end;

procedure TfrmEvent.CountExit(Sender: TObject);
begin
  UpdateSIPEventScript;
end;

procedure TfrmEvent.EnableGroup;
begin
  if SIPEvent.ScriptID>0 then
  begin
    Script.Text:=dmGUI.LoadFile('сценарии',SIPEvent.ScriptID);
    if Script.IsGroup then
    begin
      RingGroupSelect.Enabled:=True;
      RingGroupSelect.KeyValue:=IntToStr(SIPEvent.RingGroup);
    end else
    begin
      RingGroupSelect.Enabled:=False;
      RingGroupSelect.KeyValue:=null;
    end;
  end else
  begin
    RingGroupSelect.Enabled:=True;
    RingGroupSelect.KeyValue:=IntToStr(SIPEvent.RingGroup);
  end;
end;

procedure TfrmEvent.EventKindClick(Sender: TObject);
begin
  ShowParams;
  DataChanged:=True;
end;

procedure TfrmEvent.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TfrmEvent.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if DataChanged and acSave.Visible then
  case dmGUI.ConfirmExit of
    mrYes:
      begin
        acSave.Execute;
        CanClose:=True;
      end;
    mrNo:
      CanClose:=True;
    else
      CanClose:=False;
  end;
end;

procedure TfrmEvent.FormCreate(Sender: TObject);
var I:Integer;
begin
  Event:=TSIPEvent.Create;
  EventKind.Items.Clear;
  for I := 0 to Length(EventKinds) - 1 do
    EventKind.Items.Add(EventKinds[I][0]);
  Script:=TSIPScript.Create;
end;

procedure TfrmEvent.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Event);
  FreeAndNil(Script);
end;

procedure TfrmEvent.FormShow(Sender: TObject);
begin
  dmGUI.RingGroupsList.Open;
  dmGUI.ScriptList.Open;
  dmGUI.FileList.Open;
  dmGUI.Crisis.Open;
end;

procedure TfrmEvent.CrisisKindClick(Sender: TObject);
begin
  DataChanged:=True;
end;

procedure TfrmEvent.ListScript;
var I:Integer;
    A:TListItem;
begin
  Scripts.Clear;
  Scripts.Items.BeginUpdate;
  try
    for I := 0 to Event.Count - 1 do
    begin
      A:=Scripts.Items.Add;
      UpdateList(A,Event.Items[I] as TSIPEventScript);
    end;
  finally
    Scripts.Items.EndUpdate;
  end;
  if Scripts.Items.Count>0 then Scripts.ItemIndex:=0;
  Panel2.Visible:=Scripts.Items.Count>0;
end;

procedure TfrmEvent.LoadEvent;
var ID:Integer;                                 
begin
  EventName.Text:=FileName;
  Hint:=FileName;
  Event.Text:=dmGUI.LoadFile('събития',EventName.Text,ID);
  EventID.Text:=IntToStr(ID);
  EventKind.ItemIndex:=Event.EventKind;
  CrisisKind.KeyValue:=Event.CrisisKind;
  ListScript;
  DataChanged:=False;
end;

procedure TfrmEvent.ParamsStringsChange(Sender: TObject);
begin
  DataChanged:=True;
end;

procedure TfrmEvent.RingGroupSelectKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_DELETE then RingGroupSelect.KeyValue:=null;
end;

type MyStrings=class(TValueListStrings);

procedure TfrmEvent.ScriptSelectClick(Sender: TObject);
begin
  UpdateSIPEventScript;
end;

procedure TfrmEvent.ScriptsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if (Item<>nil) and Selected then
    ShowAction(Item.Data);
end;

procedure TfrmEvent.SetChanged(const Value: Boolean);
begin
  FChanged := Value;
  acSave.Enabled:=DataChanged;
end;

procedure TfrmEvent.ShowAction(SIPEvent:TSIPEventScript);
   function ID(S:String):Variant;
   var I:Integer;
   begin
     I:=StrToIntDef(S,0);
     if I=0 then
       Result:=null
     else
       Result:=I;
   end;
begin
  Changing:=True;
  try
    EnableGroup(SIPEvent);
    ScriptSelect.KeyValue:=SIPEvent.ScriptID;
    Count.Text:=IntToStr(SIPEvent.Count);
  finally
    Changing:=False;
  end;
end;

procedure TfrmEvent.ShowParams;
var S:TStringList;
  I: Integer;
  Key:String;
begin
  Params.Strings.Clear;
  if EventKind.ItemIndex<0 then Exit;

  S:=TStringList.Create;
  try
    S.CommaText:=EventKinds[EventKind.ItemIndex][1];
    for I := 0 to S.Count - 1 do
    begin
      Key:=S.Names[I];
      Params.Strings.Add(Key+'='+Event.Parameters.Values[Key]);
      Params.ItemProps[Key].KeyDesc:=S.Values[Key];
    end;
  finally
    S.Free;
  end;
  
end;

procedure TfrmEvent.SpeedButton1Click(Sender: TObject);
begin
  dmImages.ShowForm(TfrmGenericGrid,True,frmGenericGrid,False);
  frmGenericGrid.DataSource.DataSet:=srcEventType.DataSet;
  frmGenericGrid.ImportFile:=ExtractFilePath(ParamStr(0))+'xls\nom_crisis.xls';
  frmGenericGrid.ShowModal;
end;

procedure TfrmEvent.UpdateSIPEventScript;
var SIPEvent:TSIPEventScript;
begin
  if Changing then Exit;
  
  if Scripts.ItemIndex<0 then Exit;

  DataChanged:=True;
  SIPEvent:=Event.Items[Scripts.ItemIndex] as TSIPEventScript;
  SIPEvent.ScriptID:=ScriptSelect.KeyValue;
  SIPEvent.RingGroup:=StrToIntDef(VarToStr(RingGroupSelect.KeyValue),0);
  SIPEvent.Count:=StrToIntDef(Count.Text,1);
  if SIPEvent.Count<1 then SIPEvent.Count:=1;
  Count.Text:=IntToStr(SIPEvent.Count);

  UpdateList(Scripts.Items[Scripts.ItemIndex],SIPEvent);
end;

procedure TfrmEvent.UpdateList(Item: TListItem; SIPEvent: TSIPEventScript);
begin
  if SIPEvent.ScriptID>0 then
    Item.Caption:=dmGUI.FileName(SIPEvent.ScriptID)
  else
    Item.Caption:='';
  Item.SubItems.Text:=IntToStr(SIPEvent.ScriptID);
  Item.Data:=SIPEvent;
  Item.ImageIndex:=37;
  EnableGroup(SIPEvent);
end;

end.
