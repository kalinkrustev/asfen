unit frm_Menu;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, ToolWin, ActnMan, ActnCtrls, ActnList,
  XPStyleActnCtrls, ActnColorMaps, StdCtrls, Tabs, AppEvnts;

type
  TfrmMenu = class(TForm)
    ActionManager1: TActionManager;
    acPerson: TAction;
    acRingGroup: TAction;
    Toolbar: TActionToolBar;
    acRights: TAction;
    acFilesReports: TAction;
    LoginPanel: TPanel;
    UserName: TLabeledEdit;
    Password: TLabeledEdit;
    btnLogin: TButton;
    ToolbarPanel: TPanel;
    Center: TPanel;
    acLogin: TAction;
    acMode: TAction;
    acEventMonitor: TAction;
    Tabs: TTabSet;
    acSettings: TAction;
    DBSelect: TComboBox;
    StaticText1: TStaticText;
    acExport: TAction;
    acFilesScripts: TAction;
    acFilesMessages: TAction;
    acFilesEvents: TAction;
    acImport: TAction;
    Action1: TAction;
    Events: TApplicationEvents;
    IdleTimer: TTimer;
    acArchive: TAction;
    procedure acPersonExecute(Sender: TObject);
    procedure acRingGroupExecute(Sender: TObject);
    procedure acRightsExecute(Sender: TObject);
    procedure acFilesReportsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoginPanelResize(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure UserNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure acLoginExecute(Sender: TObject);
    procedure acModeExecute(Sender: TObject);
    procedure acEventMonitorExecute(Sender: TObject);
    procedure TabsChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure acSettingsExecute(Sender: TObject);
    procedure TabsGetImageIndex(Sender: TObject; TabIndex: Integer;
      var ImageIndex: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acExportExecute(Sender: TObject);
    procedure acFilesScriptsExecute(Sender: TObject);
    procedure acFilesMessagesExecute(Sender: TObject);
    procedure acFilesEventsExecute(Sender: TObject);
    procedure acImportExecute(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure EventsMessage(var Msg: tagMSG; var Handled: Boolean);
    procedure IdleTimerTimer(Sender: TObject);
    procedure acArchiveExecute(Sender: TObject);
    function EventsHelp(Command: Word; Data: Integer;
      var CallHelp: Boolean): Boolean;
  private
    LockPassword:String;
    procedure CloseAll;
    procedure ShowMode;
    procedure WMHelp(var Message: TWMHelp); message WM_HELP;
  public
    procedure InitTimer;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

var
  frmMenu: TfrmMenu;

implementation
uses dm_GUI, frm_Rights, frm_Script, frm_Files,
  dm_AlarmDB, frm_Mode, frm_EventMonitor, frm_Settings, frm_Wave, frm_Event,
  frm_Report, frm_RingGroupTree, frm_Organisation, dm_Images, frm_Export,
  frm_Users, frm_Password, Util, frm_Archive;

{$R *.dfm}

procedure TfrmMenu.acLoginExecute(Sender: TObject);
begin
  CloseAll;
  Application.ProcessMessages;
  if MdiChildCount>0 then Exit;

  ToolbarPanel.Visible:=False;
  LoginPanel.Visible:=True;
  IdleTimer.Enabled:=False;
  if UserName.CanFocus then UserName.SetFocus;
  Cascade;
end;

procedure TfrmMenu.acModeExecute(Sender: TObject);
begin
  if frmMode.ShowModal=mrOk then ShowMode;
end;

procedure TfrmMenu.acPersonExecute(Sender: TObject);
begin
  dmGUI.ShowForm(TfrmOrganisation,false,frmOrganisation);
//  frmOrganisation.acImport.Visible:=True;
//  frmOrganisation.acImport.OnExecute:=ImportOrganisations;
  dmGUI.City.Open;
  dmGUI.CityList.Open;
  dmGUI.Position.Open;
  dmGUI.Raion.Open;
  dmGUI.OrganisationsEdit.Open;
  if frmOrganisation.srcCityList.DataSet=nil then
    frmOrganisation.srcCityList.DataSet:=dmGUI.CityList;
  if frmOrganisation.srcPosition.DataSet=nil then
    frmOrganisation.srcPosition.DataSet:=dmGUI.Position;
  if frmOrganisation.srcRaion.DataSet=nil then
    frmOrganisation.srcRaion.DataSet:=dmGUI.Raion;
  if frmOrganisation.srcTree.DataSet=nil then
    frmOrganisation.srcTree.DataSet:=dmGUI.OrganisationsEdit;
end;

procedure TfrmMenu.acRightsExecute(Sender: TObject);
begin
  dmGUI.ShowForm(TfrmRights,true,frmRights);
end;

procedure TfrmMenu.acRingGroupExecute(Sender: TObject);
begin
  dmGUI.ShowForm(TfrmRingGroupTree,false,frmRingGroupTree);
end;

procedure TfrmMenu.acSettingsExecute(Sender: TObject);
begin
  dmGUI.ShowForm(TfrmSettings,False,frmSettings);
end;

procedure TfrmMenu.Action1Execute(Sender: TObject);
begin
  dmGUI.ShowForm(TfrmUsers,False,frmUsers);
end;

procedure TfrmMenu.btnLoginClick(Sender: TObject);
begin
  if DBSelect.ItemIndex<0 then Exit;
  dmGUI.Login(UserName.Text,Password.Text,DBSelect.ItemIndex+1);
  dmAlarmDB.Login(DBSelect.ItemIndex+1);
  LockPassword:=Password.Text;
  ShowMode;
  Password.Clear;
  LoginPanel.Visible:=False;
  ToolbarPanel.Visible:=True;
  InitTimer;
  IdleTimer.Enabled:=True;
  ActiveControl:=Toolbar;
end;

procedure TfrmMenu.CloseAll;
var
  i: Integer;
begin
  for i:= 0 to MdiChildCount - 1 do
    MDIChildren[i].Close;
end;

function TfrmMenu.EventsHelp(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
begin
  Application.HelpSystem.ShowTopicHelp(PChar(Data),Application.HelpFile);
  Result:=True;
  CallHelp:=False;
end;

procedure TfrmMenu.EventsMessage(var Msg: tagMSG; var Handled: Boolean);
begin
  if (Msg.message>=WM_MouseFirst) and (Msg.Message<=WM_MouseLast)
  or (Msg.message>=WM_KEYFIRST) and (Msg.message<=WM_KEYLAST) then
  begin
    if IdleTimer.Enabled then
    begin
      IdleTimer.Enabled:=False;
      IdleTimer.Enabled:=True;
    end;
  end;
  if (Msg.Message=WM_KillFocus)
  or (Msg.Message=WM_LBUTTONDOWN)
  or (Msg.Message=WM_RBUTTONDOWN)
  or (Msg.Message=WM_NCLBUTTONDOWN)
  or (Msg.Message=WM_NCRBUTTONDOWN)
  or (Msg.Message=WM_KEYDOWN)
  or (Msg.Message=WM_SYSKEYDOWN)
  or (Msg.Message=WM_ACTIVATE)
  or (Msg.message=CM_DEACTIVATE)
    then if (FHint<>nil) and FHint.HandleAllocated then FHint.ReleaseHandle;
end;

procedure TfrmMenu.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:=dmGUI.Confirm('Желаете ли да затворите приложението?');
end;

procedure TfrmMenu.FormCreate(Sender: TObject);
var I:Integer;
  S:TStringList;
begin
  Toolbar.ColorMap:=dmImages.ColorMap;
  dmImages.Tabs:=Tabs;
  DBSelect.Items.Clear;
  S:=TStringList.Create;
  try
    S.Text:=Lowercase(dmAlarmDB.DatabaseList);
    for I := 0 to Length(AlarmModes) - 1 do
      if S.IndexOf('asos'+IntToStr(I+1))>=0 then
        DBSelect.Items.Add(AlarmModes[I]);
  finally
    S.Free;
  end;
  if DBSelect.Items.Count=1 then DBSelect.ItemIndex:=0;
  
end;

procedure TfrmMenu.FormShow(Sender: TObject);
begin
  if (DBselect.ItemIndex<0) and DBSelect.CanFocus and DBSelect.Showing then
    DBSelect.SetFocus
  else
  if UserName.CanFocus and UserName.Showing then
    UserName.SetFocus;
end;

procedure TfrmMenu.IdleTimerTimer(Sender: TObject);
begin
  IdleTimer.Enabled:=False;
  GetPassword(LockPassword);
  IdleTimer.Enabled:=True;
end;

procedure TfrmMenu.InitTimer;
begin
  IdleTimer.Interval:=60000*StrToIntDef(dmAlarmDB.getIni('IdleLock'),1);
end;

{procedure TfrmMenu.ImportOrganisations(Sender: TObject);
begin
  if (Sender is TComponent)
  and (TComponent(Sender).Owner is TCustomForm) then
  begin
     TCustomForm(TComponent(Sender).Owner).Close;
     Application.ProcessMessages;
  end;
  dmGUI.City.Close;
  dmGUI.CityList.Close;
  dmGUI.Position.Close;
  dmGUI.Raion.Close;
  dmGUI.OrganisationsList.Close;
  try
    dmAlarmDB.ImportOrganisations(Sender);
  finally
    if dmGUI.PersonList.Active then dmGUI.PersonList.Requery;
  end;
end;}

procedure TfrmMenu.LoginPanelResize(Sender: TObject);
begin
  Center.Left:=(LoginPanel.Width-Center.Width) div 2;
  Center.Top:= (LoginPanel.Height-Center.Height) div 2;
end;

procedure TfrmMenu.Notification(AComponent: TComponent; Operation: TOperation);
var t:Integer;
begin
  inherited;
  if (Operation=opRemove) and not (csDestroying in ComponentState) then
  begin
    t:=Tabs.Tabs.IndexOfObject(AComponent);
    if t>=0 then
    begin
      Tabs.Tabs.Delete(t);
      if t>=Tabs.Tabs.Count then t:=t-1;
      Tabs.TabIndex:=-1;
      Tabs.TabIndex:=t;
    end;
  end;
end;

procedure TfrmMenu.ShowMode;
var Mode:Integer;
begin
  Mode:=dmGUI.DatabaseID-1;
  if (Mode>=0) and (Mode<Length(AlarmModes)) then
    Caption:='Естел АСО | Администратор | '+AlarmModes[Mode];
  Application.Title:=Caption;
end;

procedure TfrmMenu.TabsChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  if (NewTab>=0) and (Tabs.Tabs.Objects[NewTab] is TCustomForm) then
    TCustomForm(Tabs.Tabs.Objects[NewTab]).Show;
end;

procedure TfrmMenu.TabsGetImageIndex(Sender: TObject; TabIndex: Integer;
  var ImageIndex: Integer);
begin
  if Tabs.Tabs.Objects[TabIndex] is TfrmWave then ImageIndex:=33
  else
  if Tabs.Tabs.Objects[TabIndex] is TfrmScript then ImageIndex:=37
  else
  if Tabs.Tabs.Objects[TabIndex] is TfrmEvent then ImageIndex:=39
  else
  if Tabs.Tabs.Objects[TabIndex] is TfrmReport then ImageIndex:=43
  else
    ImageIndex:=-1;
end;

procedure TfrmMenu.UserNameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then btnLogin.Click;
end;

procedure TfrmMenu.WMHelp(var Message: TWMHelp);
var
  Control: TWinControl;
begin
  with Message.HelpInfo^ do
  begin
    if iContextType = HELPINFO_WINDOW then
    begin
      if LoginPanel.Visible then
        Control := FindControl(hItemHandle)
      else
        Control:=nil;  
      if Control=nil then Message.HelpInfo.hItemHandle:=Handle;
    end;
  end;
  inherited;
end;

procedure TfrmMenu.acArchiveExecute(Sender: TObject);
begin
  CloseAll;
  Application.ProcessMessages;
  if MdiChildCount>0 then Exit;

  if dmGUI.ShowForm(TfrmArchive,true,frmArchive)=mrOk then
    acLogin.Execute;
end;

procedure TfrmMenu.acEventMonitorExecute(Sender: TObject);
begin
  dmGUI.ShowForm(TfrmEventMonitor,true,frmEventMonitor);
end;

procedure TfrmMenu.acExportExecute(Sender: TObject);
begin
  dmGUI.ShowForm(TfrmExport,true,frmExport);
end;

procedure TfrmMenu.acFilesEventsExecute(Sender: TObject);
begin
  dmGUI.ShowForm(TfrmFiles,false,frmFilesEvents,True,nil,'ShowForm.Events');
  frmFilesEvents.Path:='събития';
  dmImages.UpdateTab(frmFilesEvents);
end;

procedure TfrmMenu.acFilesMessagesExecute(Sender: TObject);
begin
  dmGUI.ShowForm(TfrmFiles,false,frmFilesMessages,True,nil,'ShowForm.Messages');
  frmFilesMessages.Path:='съобщения';
  dmImages.UpdateTab(frmFilesMessages);
end;

procedure TfrmMenu.acFilesReportsExecute(Sender: TObject);
begin
  dmGUI.ShowForm(TfrmFiles,false,frmFilesReports,True,nil,'ShowForm.Reports');
  frmFilesReports.Path:='справки/събития';
  dmImages.UpdateTab(frmFilesReports);
end;

procedure TfrmMenu.acFilesScriptsExecute(Sender: TObject);
begin
  dmGUI.ShowForm(TfrmFiles,false,frmFilesScripts,True,nil,'ShowForm.Scripts');
  frmFilesScripts.Path:='сценарии';
  dmImages.UpdateTab(frmFilesScripts);
end;

procedure TfrmMenu.acImportExecute(Sender: TObject);
begin
  if not dmGUI.GetRight('ShowForm.TfrmImport','*') then
    raise TMessageException.Create('Нямате право на достъп до избраната функция !');
  if not dmGUI.Confirm('Импортът ще презапише данните, с тези които са били експортирани.'#13#10'Желаете ли да продължите?') then
    Exit;
  CloseAll;
  Application.ProcessMessages;
  if MdiChildCount>0 then Exit;

  dmAlarmDB.ImportData;
  ShowMessage('Импортът завърши успешно');
  acLogin.Execute;
end;

end.
