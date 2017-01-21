unit frm_Script;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, ActnMan, ActnCtrls, ActnList, XPStyleActnCtrls,
  ExtCtrls, StdCtrls, DBCtrls, DB, SIP_Script, SIP_Action;

type
  TfrmScript = class(TForm)
    ActionManager1: TActionManager;
    acSave: TAction;
    acAdd: TAction;
    acDelete: TAction;
    acClose: TAction;
    ActionToolBar1: TActionToolBar;
    Actions: TListView;
    Splitter1: TSplitter;
    Action: TPageControl;
    None: TTabSheet;
    Ring: TTabSheet;
    Delay: TTabSheet;
    Play: TTabSheet;
    DTMF: TTabSheet;
    Jump: TTabSheet;
    Call: TTabSheet;
    RingKind: TRadioGroup;
    DelayMillisecond: TLabeledEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    JumpScriptSelect: TDBLookupComboBox;
    CallScriptSelect: TDBLookupComboBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Recordings: TDataSource;
    Scripts: TDataSource;
    PlayFileName: TDBLookupComboBox;
    Persons: TDataSource;
    RingGroups: TDataSource;
    Panel1: TPanel;
    ScriptName: TEdit;
    Label11: TLabel;
    Hangup: TTabSheet;
    Label12: TLabel;
    DTMFTimeOut: TLabeledEdit;
    DTMFFail: TCheckBox;
    SMS: TTabSheet;
    Label13: TLabel;
    SMSKind: TRadioGroup;
    SMSPersonSelect: TDBLookupComboBox;
    Label14: TLabel;
    SMSName: TDBLookupComboBox;
    ScriptID: TEdit;
    Label15: TLabel;
    RingPersonSelect: TDBLookupComboBox;
    Mail: TTabSheet;
    Label16: TLabel;
    Label17: TLabel;
    EMailKind: TRadioGroup;
    EMailPersonSelect: TDBLookupComboBox;
    EMailName: TDBLookupComboBox;
    Label18: TLabel;
    RingSequence: TEdit;
    Label19: TLabel;
    RetryBusy: TEdit;
    Label20: TLabel;
    DelayBusy: TEdit;
    Label21: TLabel;
    RetryBusyRoute: TEdit;
    Label22: TLabel;
    DelayBusyRoute: TEdit;
    Label23: TLabel;
    RetryFailDTMF: TEdit;
    Label24: TLabel;
    DelayFailDTMF: TEdit;
    acUp: TAction;
    acDown: TAction;
    procedure FormShow(Sender: TObject);
    procedure RingKindClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acCloseExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acAddExecute(Sender: TObject);
    procedure ActionChanged(Sender: TObject);
    procedure ActionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure acDeleteExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure DelayMillisecondChange(Sender: TObject);
    procedure RingPersonSelectClick(Sender: TObject);
    procedure RingGroupSelectClick(Sender: TObject);
    procedure PlayFileNameClick(Sender: TObject);
    procedure JumpScriptSelectClick(Sender: TObject);
    procedure CallScriptSelectClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DTMFFailClick(Sender: TObject);
    procedure SMSKindClick(Sender: TObject);
    procedure SMSPersonSelectClick(Sender: TObject);
    procedure SMSNameClick(Sender: TObject);
    procedure EMailKindClick(Sender: TObject);
    procedure EMailPersonSelectClick(Sender: TObject);
    procedure EMailNameClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RingSequenceChange(Sender: TObject);
    procedure RingSequenceKeyPress(Sender: TObject; var Key: Char);
    procedure acUpExecute(Sender: TObject);
    procedure acDownExecute(Sender: TObject);
  private
    Script:TSIPScript;
    Changing:Boolean;
    FDataChanged: Boolean;
    procedure ListScript;
    procedure UpdateList(Item:TListItem;Action:TSIPAction);
    procedure ShowAction(SIPAction:TSIPAction);
    procedure UpdateSIPAction;
    procedure SetDataChanged(const Value: Boolean);
    function  ActionText(A:TSIPAction):String;
  public
    property DataChanged:Boolean read FDataChanged write SetDataChanged;
    procedure LoadScript(FileName:String);
  end;

implementation
uses dm_GUI, dm_Images;
{$R *.dfm}

procedure TfrmScript.acAddExecute(Sender: TObject);
begin
  Script.AddActionString('none');
  DataChanged:=True;
  ListScript;
  Actions.ItemIndex:=Actions.Items.Count-1;
end;

procedure TfrmScript.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmScript.acDeleteExecute(Sender: TObject);
var I:Integer;
begin
  I:=Actions.ItemIndex;
  if (I>=0) and (I<Script.Count) then
  begin
    if not dmGUI.ConfirmDelete then Exit;
    Script.Delete(I);
    DataChanged:=True;
    ListScript;
    if (I>=0) and (I<Actions.Items.Count) then
      Actions.ItemIndex:=I else
    if (I>0) and (I=Actions.Items.Count) then
      Actions.ItemIndex:=I-1
  end;
end;

procedure TfrmScript.acDownExecute(Sender: TObject);
var I:Integer;
begin
  if (Actions.Selected<>nil) and
     (Actions.Selected.Index>=0) and
     (Actions.Selected.Index<Script.Count-1) then
  begin
    I:=Actions.Selected.Index+1;
    Script.Move(Actions.Selected.Index,Actions.Selected.Index+1);
    DataChanged:=True;
    ListScript;
    if (I>=0) and (I<Actions.Items.Count) then
      Actions.ItemIndex:=I;
  end;
end;

procedure TfrmScript.acSaveExecute(Sender: TObject);
begin
  dmGUI.SaveFile('сценарии',ScriptName.Text,Script.Text,True);
  DataChanged:=False;
end;

procedure TfrmScript.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TfrmScript.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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

procedure TfrmScript.FormCreate(Sender: TObject);
begin
  Script:=TSIPScript.Create;
end;

procedure TfrmScript.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Script);
end;

procedure TfrmScript.FormShow(Sender: TObject);
begin
  dmGUI.RingGroupsList.Open;
  dmGUI.PersonList.Open;
  dmGUI.ScriptList.Open;
  dmGUI.RecordingsList.Open;
  dmGUI.FileList.Open;
end;

procedure TfrmScript.JumpScriptSelectClick(Sender: TObject);
begin
  UpdateSIPAction;
end;

procedure TfrmScript.ListScript;
var I:Integer;
    A:TListItem;
begin
  Actions.Clear;
  Actions.Items.BeginUpdate;
  try
    for I := 0 to Script.Count - 1 do
    begin
      A:=Actions.Items.Add;
      UpdateList(A,Script.Action[I]);
    end;
  finally
    Actions.Items.EndUpdate;
  end;
  if Actions.Items.Count>0 then Actions.ItemIndex:=0;
  Action.Visible:=(Actions.Items.Count>0) and (Actions.ItemIndex>=0);
end;

procedure TfrmScript.LoadScript;
var ID:Integer;
begin
  ScriptName.Text:=FileName;
  Hint:=FileName;
  Script.Text:=dmGUI.LoadFile('сценарии',ScriptName.Text,ID);
  ScriptID.Text:=IntToStr(ID);
  ListScript;
  DataChanged:=False;
end;

procedure TfrmScript.PlayFileNameClick(Sender: TObject);
begin
  UpdateSIPAction;
end;

procedure TfrmScript.ActionChanged(Sender: TObject);
begin
  UpdateSIPAction;
end;

procedure TfrmScript.ActionsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if (Item<>nil) and Selected then
    ShowAction(Item.Data);
  Action.Visible:=(Actions.Items.Count>0) and (Actions.ItemIndex>=0);
  AcUp.Enabled:=(Actions.ItemIndex>0);
  AcDown.Enabled:=(Actions.ItemIndex>=0) and (Actions.ItemIndex<Actions.Items.Count-1);
end;

function TfrmScript.ActionText(A: TSIPAction): String;
begin
  case A.OpCode of
    OP_NONE: Result:='---';
    OP_RING: begin
               Result:='Позвъняване';
                if A.Operand[1]='person' then
                  Result:=Result+' на потребител '+dmGui.PersonName(A.Operand[2])
                else
                if A.Operand[1]='group' then
                  Result:=Result+' на група';
             end;
    OP_DELAY: Result:='Изчакване '+A.Operand[1]+' мсек';
    OP_PLAY: Result:='Гл. съобщение '+dmGUI.FileName(StrToIntDef(A.Operand[1],0));
    OP_DTMF: Result:='DTMF '+A.Operand[1]+' сек';
    OP_JUMP: Result:='Преход към '+dmGUI.FileName(StrToIntDef(A.Operand[1],0));
    OP_CALL: Result:='Извикване на '+dmGUI.FileName(StrToIntDef(A.Operand[1],0));
    OP_HANGUP: Result:='Прекъсване';
    OP_SMS: begin
              Result:='SMS '+dmGUI.FileName(StrToIntDef(A.Operand[1],0));
              if A.Operand[2]='person' then
                Result:=Result+' на потребител '+dmGui.PersonName(A.Operand[3])
              else
              if A.Operand[2]='group' then
                Result:=Result+' на група';
            end;
    OP_Mail: begin
              Result:='EMail '+dmGUI.FileName(StrToIntDef(A.Operand[1],0));
              if A.Operand[2]='person' then
                Result:=Result+' на потребител '+dmGui.PersonName(A.Operand[3])
              else
              if A.Operand[2]='group' then
                Result:=Result+' на група';
            end;
  end;
end;

procedure TfrmScript.acUpExecute(Sender: TObject);
var I:Integer;
begin
   if (Actions.Selected<>nil) and
      (Actions.Selected.Index>0) and
      (Actions.Selected.Index<Script.Count) then
  begin
    I:=Actions.Selected.Index-1;
    Script.Move(Actions.Selected.Index,Actions.Selected.Index-1);
    DataChanged:=True;
    ListScript;
    if (I>=0) and (I<Actions.Items.Count) then
      Actions.ItemIndex:=I;
  end;
end;

procedure TfrmScript.CallScriptSelectClick(Sender: TObject);
begin
  UpdateSIPAction;
end;

procedure TfrmScript.DelayMillisecondChange(Sender: TObject);
begin
  UpdateSIPAction;
end;

procedure TfrmScript.DTMFFailClick(Sender: TObject);
begin
  UpdateSIPAction;
end;

procedure TfrmScript.EMailKindClick(Sender: TObject);
begin
  EMailPersonSelect.Visible:=EMailKind.ItemIndex=1;
  UpdateSIPAction;
end;

procedure TfrmScript.EMailNameClick(Sender: TObject);
begin
  UpdateSIPAction;
end;

procedure TfrmScript.EMailPersonSelectClick(Sender: TObject);
begin
  UpdateSIPAction;
end;

procedure TfrmScript.RingGroupSelectClick(Sender: TObject);
begin
  UpdateSIPAction;
end;

procedure TfrmScript.RingKindClick(Sender: TObject);
begin
  RingPersonSelect.Visible:=RingKind.ItemIndex=1;
  UpdateSIPAction;
end;

procedure TfrmScript.RingPersonSelectClick(Sender: TObject);
begin
  UpdateSIPAction;
end;

procedure TfrmScript.RingSequenceChange(Sender: TObject);
begin
  UpdateSIPAction;
end;

procedure TfrmScript.RingSequenceKeyPress(Sender: TObject; var Key: Char);
begin
  if not (Key in ['0'..'9','s','m',#8]) then Key:=#0;
end;

procedure TfrmScript.SetDataChanged(const Value: Boolean);
begin
  FDataChanged := Value;
  acSave.Enabled:=FDataChanged;
end;

procedure TfrmScript.ShowAction(SIPAction: TSIPAction);
   function ID(S:String):Variant;
   var I:Int64;
   begin
     I:=StrToInt64Def(S,0);
     if I=0 then
       Result:=null
     else
       Result:=I;
   end;
begin
  Changing:=True;
  try
    case SIPAction.OpCode of
      OP_NONE:Action.ActivePage:=None;
      OP_RING:begin
                Action.ActivePage:=Ring;
                if SIPAction.Operand[1]='group' then
                begin
                  RingKind.ItemIndex:=0;
                end else
                if SIPAction.Operand[1]='person' then
                begin
                  RingKind.ItemIndex:=1;
                  RingPersonSelect.KeyValue:=ID(SIPAction.Operand[2]);
                end;
                RingKindClick(nil);
                RingSequence.Text:=SIPAction.Operand[3];
                RetryBusy.Text:=SIPAction.Operand[4];
                DelayBusy.Text:=SIPAction.Operand[5];
                RetryBusyRoute.Text:=SIPAction.Operand[6];
                DelayBusyRoute.Text:=SIPAction.Operand[7];
                RetryFailDTMF.Text:=SIPAction.Operand[8];
                DelayFailDTMF.Text:=SIPAction.Operand[9];
              end;
      OP_DELAY:begin
                 Action.ActivePage:=Delay;
                 DelayMillisecond.Text:=SIPAction.Operand[1];
               end;
      OP_PLAY:begin
                Action.ActivePage:=Play;
                PlayFileName.KeyValue:=SIPAction.Operand[1];
              end;
      OP_DTMF:begin
                Action.ActivePage:=DTMF;
                DTMFTimeOut.Text:=VarToStr(ID(SIPAction.Operand[1]));
                DTMFFail.Checked:=(VarToStr(ID(SIPAction.Operand[2]))<>'') and
                                  (VarToStr(ID(SIPAction.Operand[2]))<>'0');
              end;
      OP_JUMP:begin
                Action.ActivePage:=Jump;
                JumpScriptSelect.KeyValue:=SIPAction.Operand[1];
              end;
      OP_CALL:begin
                Action.ActivePage:=Call;
                CallScriptSelect.KeyValue:=SIPAction.Operand[1];
              end;
      OP_HANGUP:Action.ActivePage:=Hangup;
      OP_SMS:begin
                Action.ActivePage:=SMS;
                SMSName.KeyValue:=SIPAction.Operand[1];
                if SIPAction.Operand[2]='group' then
                begin
                  SMSKind.ItemIndex:=0;
                end else
                if SIPAction.Operand[2]='person' then
                begin
                  SMSKind.ItemIndex:=1;
                  SMSPersonSelect.KeyValue:=ID(SIPAction.Operand[3]);
                end;
                SMSKindClick(nil);
              end;
      OP_Mail:begin
                Action.ActivePage:=Mail;
                EMailName.KeyValue:=SIPAction.Operand[1];
                if SIPAction.Operand[2]='group' then
                begin
                  EMailKind.ItemIndex:=0;
                end else
                if SIPAction.Operand[2]='person' then
                begin
                  EMailKind.ItemIndex:=1;
                  EmailPersonSelect.KeyValue:=ID(SIPAction.Operand[3]);
                end;
                EMailKindClick(nil);
              end;
    end;
  finally
    Changing:=False;
  end;
end;

procedure TfrmScript.SMSKindClick(Sender: TObject);
begin
  SMSPersonSelect.Visible:=SMSKind.ItemIndex=1;
  UpdateSIPAction;
end;

procedure TfrmScript.SMSNameClick(Sender: TObject);
begin
  UpdateSIPAction;
end;

procedure TfrmScript.SMSPersonSelectClick(Sender: TObject);
begin
  UpdateSIPAction;
end;

procedure TfrmScript.UpdateSIPAction;
var SIPAction:TSIPAction;
  Op:String;
  
  function number(const S:String):String;
  var I:Integer;
  begin
    Result:=S;
    for I := 1 to Length(S) do
      if not(S[I] in ['0'..'9']) then
      begin
        Result:='';
        Break;
      end;
  end;

begin
  if Changing then Exit;

  if Actions.ItemIndex<0 then Exit;

  SIPAction:=Script.Action[Actions.ItemIndex];

  DataChanged:=True;
  if Action.ActivePage=None then
  begin
    SIPAction.OpCode:=OP_NONE;
    SIPAction.Operands:='';
  end else
  if Action.ActivePage=Ring then
  begin
    SIPAction.OpCode:=OP_RING;
    Op:=','+RingSequence.Text+','+Number(RetryBusy.Text)+','+
        Number(DelayBusy.Text)+','+Number(RetryBusyRoute.Text)+','+
        Number(DelayBusyRoute.Text)+','+Number(RetryFailDTMF.Text)+','+
        Number(DelayFailDTMF.Text);
    case RingKind.ItemIndex of
      1:SIPAction.Operands:='person,'+VarToStr(RingPersonSelect.KeyValue)+Op;
      0:SIPAction.Operands:='group,'+Op;
    end;
  end else
  if Action.ActivePage=Delay then
  begin
    SIPAction.OpCode:=OP_DELAY;
    SIPAction.Operands:=DelayMillisecond.Text;
  end else
  if Action.ActivePage=Play then
  begin
    SIPAction.OpCode:=OP_PLAY;
    SIPAction.Operands:=QuotedStr(VarToStr(PlayFileName.KeyValue));
  end else
  if Action.ActivePage=DTMF then
  begin
    SIPAction.OpCode:=OP_DTMF;
    if DTMFFail.Checked then
      SIPAction.Operands:=QuotedStr(DTMFTimeOut.Text)+',1'
    else
      SIPAction.Operands:=QuotedStr(DTMFTimeOut.Text)+',0';
  end else
  if Action.ActivePage=Jump then
  begin
    SIPAction.OpCode:=OP_JUMP;
    SIPAction.Operands:=QuotedStr(VarToStr(JumpScriptSelect.KeyValue));
  end else
  if Action.ActivePage=Call then
  begin
    SIPAction.OpCode:=OP_CALL;
    SIPAction.Operands:=QuotedStr(VarToStr(CallScriptSelect.KeyValue));
  end else
  if Action.ActivePage=Hangup then
  begin
    SIPAction.OpCode:=OP_HANGUP;
    SIPAction.Operands:='';
  end else
  if Action.ActivePage=SMS then
  begin
    SIPAction.OpCode:=OP_SMS;
    case SMSKind.ItemIndex of
      1:SIPAction.Operands:=QuotedStr(VarToStr(SMSName.KeyValue))+',person,'+VarToStr(SMSPersonSelect.KeyValue);
      0:SIPAction.Operands:=QuotedStr(VarToStr(SMSName.KeyValue))+',group';
    end;
  end else
  if Action.ActivePage=Mail then
  begin
    SIPAction.OpCode:=OP_Mail;
    case EMailKind.ItemIndex of
      1:SIPAction.Operands:=QuotedStr(VarToStr(EMailName.KeyValue))+',person,'+VarToStr(EMailPersonSelect.KeyValue);
      0:SIPAction.Operands:=QuotedStr(VarToStr(EMailName.KeyValue))+',group';
    end;
  end;

  UpdateList(Actions.Items[Actions.ItemIndex],SIPAction);
end;

procedure TfrmScript.UpdateList(Item: TListItem; Action: TSIPAction);
begin
  Item.Caption:=ActionText(Action);
  Item.Data:=Action;
  case Action.OpCode of
    OP_NONE:Item.ImageIndex:=None.ImageIndex;
    OP_RING:Item.ImageIndex:=Ring.ImageIndex;
    OP_DELAY:Item.ImageIndex:=Delay.ImageIndex;
    OP_PLAY:Item.ImageIndex:=Play.ImageIndex;
    OP_DTMF:Item.ImageIndex:=DTMF.ImageIndex;
    OP_JUMP:Item.ImageIndex:=Jump.ImageIndex;
    OP_CALL:Item.ImageIndex:=Call.ImageIndex;
    OP_HANGUP:Item.ImageIndex:=Hangup.ImageIndex;
    OP_SMS:Item.ImageIndex:=SMS.ImageIndex;
    OP_Mail:Item.ImageIndex:=Mail.ImageIndex;
  end;
end;

end.
