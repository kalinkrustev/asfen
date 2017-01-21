unit frm_Monitor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmMonitor = class(TForm)
    Button1: TButton;
    Scripts: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Sounds: TListBox;
    Button2: TButton;
    Label3: TLabel;
    RingGroups: TListBox;
    Status: TListBox;
    Label4: TLabel;
    Events: TListBox;
    Label5: TLabel;
    Button4: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private

  public
    procedure Reload;
  end;

var
  frmMonitor: TfrmMonitor;

implementation

uses dm_Alarm,SIP_Call,SIP_App,SIP_Script,SIP_Action, pjsua, SIP_RingList,
  SIP_Status, SIP_Event;

{$R *.dfm}

procedure TfrmMonitor.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMonitor.Button2Click(Sender: TObject);
var G:TSIPRingList;
begin
  if Scripts.ItemIndex>=0 then
  begin
    G:=nil;
    if RingGroups.ItemIndex>=0 then
      G:=dmAlarm.GroupRingList(Integer(RingGroups.Items.Objects[RingGroups.ItemIndex]),dmAlarm.InitEnv.DatabaseID);

    if G<>nil then
    begin
      dmAlarm.SIPApp.SIPQueueManager.Add(StrToIntDef(dmAlarm.InitEnv.SIPLibrary[Scripts.ItemIndex],0),G,1,dmAlarm.InitEnv);
    end;

  end;
end;

procedure TfrmMonitor.Button3Click(Sender: TObject);
begin
  dmAlarm.ReloadEnv;
end;

procedure TfrmMonitor.Button4Click(Sender: TObject);
begin
  if Events.ItemIndex>=0 then
    dmAlarm.FireEvent(StrToIntDef(dmAlarm.InitEnv.EventLibrary[Events.ItemIndex],0),dmAlarm.InitEnv.DatabaseID);
end;

procedure TfrmMonitor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(dmAlarm.SIPApp.SIPStatus);
  dmAlarm.Stop;
  Application.Terminate;
end;

procedure TfrmMonitor.FormShow(Sender: TObject);
begin
  Reload;
  dmAlarm.ReloadProc:=Reload;
end;

procedure TfrmMonitor.Reload;
var
  I: Integer;
  S:String;
begin
  Scripts.Items.Assign(dmAlarm.InitEnv.SIPLibrary);
  Sounds.Items.Assign(dmAlarm.InitEnv.SIPSound);
  RingGroups.Items.Assign(dmAlarm.InitEnv.RingGroups);
  Events.Items.Assign(dmAlarm.InitEnv.EventLibrary);

  for I := 0 to Scripts.Items.Count - 1 do
  begin
    S:=dmAlarm.InitEnv.FileNames.Values[Scripts.Items[I]];
    if S<>'' then Scripts.Items[I]:=S;
  end;
  for I := 0 to Events.Items.Count - 1 do
  begin
    S:=dmAlarm.InitEnv.FileNames.Values[Events.Items[I]];
    if S<>'' then Events.Items[I]:=S;
  end;
  for I := 0 to Sounds.Items.Count - 1 do
  begin
    S:=dmAlarm.InitEnv.FileNames.Values[Sounds.Items[I]];
    if S<>'' then Sounds.Items[I]:=S;
  end;

end;

end.
