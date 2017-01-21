unit frm_Wave;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, XPStyleActnCtrls, ActnMan, ToolWin, ActnCtrls, StdCtrls,
  ExtCtrls, WaveRecorders, WaveUtils, WavePlayers, DTMF;

type
  TfrmWave = class(TForm)
    ActionToolBar1: TActionToolBar;
    ActionManager1: TActionManager;
    acSave: TAction;
    acRecord: TAction;
    acPlayback: TAction;
    acClose: TAction;
    Panel1: TPanel;
    Label11: TLabel;
    WaveName: TEdit;
    acStop: TAction;
    Panel2: TPanel;
    Label1: TLabel;
    WaveLength: TEdit;
    Digit: TStaticText;
    MessageText: TMemo;
    Label2: TLabel;
    WaveID: TEdit;
    Label15: TLabel;
    acImport: TAction;
    OpenDialog: TOpenDialog;
    TotalLength: TEdit;
    Label3: TLabel;
    procedure acRecordExecute(Sender: TObject);
    procedure acPlaybackExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure acStopExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MessageTextChange(Sender: TObject);
    procedure acImportExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    Recorder:TLiveAudioRecorder;
    Player:TLiveAudioPlayer;
    FWave:String;
    PlayCount:Integer;
    FDataChanged: Boolean;
    DTMF:TDTMF;
    WaveLimit:Integer;
    FTotalSize: Int64;
    procedure OnDataRecord(Sender: TObject; const Buffer: Pointer; BufferSize: DWORD;
      var FreeIt: Boolean);
    function OnDataPlay(Sender: TObject; var Buffer: Pointer; var NumLoops: DWORD; var FreeIt: Boolean): DWORD;
    procedure RecordStarted(Sender: TObject);
    procedure RecordStopped(Sender: TObject);
    procedure PlayStarted(Sender: TObject);
    procedure PlayStopped(Sender: TObject);
    procedure SetDataChanged(const Value: Boolean);
    procedure SetWave(const Value: String);
    procedure ReadTotalSize;
    procedure SetTotalSize(const Value: Int64);
    //procedure DTMF(const S:PSmallIntArray;Size:Integer);
  public
    procedure LoadWave(FileName:String);
    property DataChanged:Boolean read FDataChanged write SetDataChanged;
    property Wave:String read FWave write SetWave;
    property TotalSize:Int64 read FTotalSize write SetTotalSize;
  end;

implementation
uses dm_GUI, WaveStorage, dm_Images, Util;
{$R *.dfm}

procedure TfrmWave.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmWave.acPlaybackExecute(Sender: TObject);
begin
  PlayCount:=0;
  Player.Active:=True;
end;

procedure TfrmWave.OnDataRecord(Sender: TObject; const Buffer: Pointer;
    BufferSize: DWORD; var FreeIt: Boolean);
var S:String;
    //I:Integer;
    //D:Char;
begin
  if BufferSize<=0 then Exit;
  SetLength(S,BufferSize);
  Move(Buffer^,S[1],BufferSize);
  if Length(Wave+S)>WaveLimit then
  begin
    Player.Active:=False;
    Recorder.Active:=False;
  end
  else
    Wave:=Wave+S;




  {for I := 0 to BufferSize div 2 do
  begin
    D:=DTMF.AddSample(PSmallIntArray(Buffer)[I]);
    if D<>#0 then Digit.Caption:=Digit.Caption+D+' ';
  end;}

  FreeIt:=True;
end;

procedure TfrmWave.PlayStarted(Sender: TObject);
begin
  acPlayback.Enabled:=False;
  acRecord.Enabled:=False;
  acImport.Enabled:=False;
  acStop.Enabled:=True;
end;

procedure TfrmWave.PlayStopped(Sender: TObject);
begin
  acPlayback.Enabled:=True;
  acRecord.Enabled:=True;
  acImport.Enabled:=True;
  acStop.Enabled:=False;
end;

procedure TfrmWave.RecordStarted(Sender: TObject);
begin
  acPlayback.Enabled:=False;
  acRecord.Enabled:=False;
  acImport.Enabled:=False;
  acStop.Enabled:=True;
end;

procedure TfrmWave.RecordStopped(Sender: TObject);
begin
  acPlayback.Enabled:=True;
  acRecord.Enabled:=True;
  acImport.Enabled:=True;
  acStop.Enabled:=False;
end;

procedure TfrmWave.SetDataChanged(const Value: Boolean);
begin
  FDataChanged := Value;
  acSave.Enabled:=DataChanged;
end;

procedure TfrmWave.SetTotalSize(const Value: Int64);
begin
  FTotalSize := Value;
  TotalLength.Text:=format('%1.1f сек.',[(FTotalSize+Length(Wave))/16000]);
end;

procedure TfrmWave.SetWave(const Value: String);
begin
  FWave := Value;
  WaveLength.Text:=format('%1.1f сек.',[Length(Wave)/16000]);
  TotalLength.Text:=format('%1.1f сек.',[(FTotalSize+Length(Wave))/16000]);
end;

procedure TfrmWave.acRecordExecute(Sender: TObject);
var T:Int64;
begin
  Wave:='';
  ReadTotalSize;

  WaveLimit:=Limits.SoundTotalLength*16000-TotalSize;
  if WaveLimit<=8000 then
  begin
    PopControlHint(TotalLength,'Позволено е дефиниране съобщения с максимум обща продължителност от '+IntToStr(Limits.SoundTotalLength)+' секунди');
    Abort;
  end;
  if WaveLimit>Limits.SoundLength*16000 then
    WaveLimit:=Limits.SoundLength*16000;

  Recorder.Active:=True;
  DataChanged:=True;
end;

procedure TfrmWave.acSaveExecute(Sender: TObject);
begin
  ReadTotalSize;
  if TotalSize+Length(Wave)>Limits.SoundTotalLength*16000 then
  begin
    PopControlHint(TotalLength,'Позволено е дефиниране съобщения с максимум обща продължителност от '+IntToStr(Limits.SoundTotalLength)+' секунди');
    Abort;
  end;

  dmGUI.SaveFile('съобщения',WaveName.Text,Wave,True);
  dmGUI.SaveFile('съобщения/текст',WaveName.Text,MessageText.Text,True);
  DataChanged:=False;
end;

procedure TfrmWave.acStopExecute(Sender: TObject);
begin
  Player.Active:=False;
  Recorder.Active:=False;
end;

procedure TfrmWave.acImportExecute(Sender: TObject);
var W:TWaveFile;
begin
  if not OpenDialog.Execute then Exit;
  W:=TWaveFile.Create(OpenDialog.FileName,fmOpenRead or fmShareDenyWrite);
  try
    if W.WaveFormat.nChannels<>1 then raise Exception.Create('Файлът не е в mono формат');
    if W.WaveFormat.wBitsPerSample<>16 then raise Exception.Create('Файлът не е в 16 bit формат');
    if W.WaveFormat.nSamplesPerSec<>8000 then raise Exception.Create('Файлът не е в 8 kHz формат');
    if W.Length>Limits.SoundLength*1000 then raise Exception.Create('Файлът съдържа над 5 минутен запис');
    DataChanged:=True;
    SetLength(FWave,W.DataSize);
    WaveLength.Text:=format('%1.1f сек.',[Length(FWave)/16000]);
    W.BeginRead;
    W.Read(FWave[1],Length(FWave));
    W.EndRead;
    Wave:=FWave;
  finally
    W.Free;
  end;
end;

procedure TfrmWave.FormActivate(Sender: TObject);
begin
  ReadTotalSize;
end;

procedure TfrmWave.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TfrmWave.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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

procedure TfrmWave.FormCreate(Sender: TObject);
begin
  DTMF:=TDTMF.Create(8000);
  Recorder:=TLiveAudioRecorder.Create(nil);
  Recorder.BufferLength:=100;
  Recorder.PCMFormat:=Mono16bit8000Hz;
  Recorder.OnData:=OnDataRecord;
  Recorder.OnActivate:=RecordStarted;
  Recorder.OnDeactivate:=RecordStopped;
  Recorder.Async:=True;
  Player:=TLiveAudioPlayer.Create(nil);
  Player.BufferLength:=100;
  Player.PCMFormat:=Mono16bit8000Hz;
  Player.BufferInternally:=False;
  Player.OnDataPtr:=OnDataPlay;
  Player.OnActivate:=PlayStarted;
  Player.OnDeactivate:=PlayStopped;
  Player.Async:=True;
  Player.BreakLoop
end;

procedure TfrmWave.FormDestroy(Sender: TObject);
begin
  freeAndNil(Recorder);
  freeAndNil(Player);
  freeAndNil(DTMF);
end;

procedure TfrmWave.LoadWave(FileName: String);
var ID:Integer;
begin
  WaveName.Text:=FileName;
  Hint:=FileName;
  MessageText.Text:=dmGUI.LoadFile('съобщения/текст',WaveName.Text,ID);
  Wave:=dmGUI.LoadFile('съобщения',WaveName.Text,ID);
  WaveID.Text:=IntToStr(ID);
  ReadTotalSize;
  DataChanged:=False;
end;

procedure TfrmWave.ReadTotalSize;
var S:String;
begin
  S:=WaveID.Text;
  if Trim(S)='' then
    S:='0';
  TotalSize := dmGUI.PathSize('съобщения',S);
end;

procedure TfrmWave.MessageTextChange(Sender: TObject);
begin
  DataChanged:=True;
end;

function TfrmWave.OnDataPlay(Sender: TObject; var Buffer: Pointer;
  var NumLoops: DWORD; var FreeIt: Boolean): DWORD;
begin
  PlayCount:=PlayCount+1;
  if PlayCount>2 then
  begin
    Result:=0;
  end else
  if PlayCount>1 then
  begin
    Result:=1;
    Buffer:=nil;
  end
  else
  begin
    Result:=Length(Wave);
    FreeIt:=False;
    if Result>0 then
      Buffer:=@Wave[1]
    else
      BuffeR:=nil;  
    NumLoops:=0;
  end;
end;

end.
