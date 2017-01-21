unit SIP_Event;

interface
uses Classes, SyncObjs;

type

  TSIPEventScript=class(TCollectionItem)
  private
    FRingGroup: Integer;
    FCount: Integer;
    FScriptID: Integer;
    procedure SetCount(const Value: Integer);
    procedure SetRingGroup(const Value: Integer);
    procedure SetScriptID(const Value: Integer);
  published
    property ScriptID:Integer read FScriptID write SetScriptID;
    property RingGroup:Integer read FRingGroup write SetRingGroup;
    property Count:Integer read FCount write SetCount;
  end;

  TSIPEvent=class(TCollection)
  private
    FParameters: TStrings;
    FName: String;
    FMode: Integer;
    FEventKind: Integer;
    FID: Integer;
    FCrisisKind: Integer;
    FReport:String;
    FReportFile:TFileStream;
    FQueueStatus:String;
    FTime:TDateTime;
    FDatabase: Integer;
    FQueueItems:Integer;
    FWait:Int64;
    FLog: Boolean;
    FLock: TCriticalSection;
    function GetText: String;
    procedure SetText(const Value: String);
    procedure SetParameters(const Value: TStrings);
    procedure SetName(const Value: String);
    function GetEventScript(Index: Integer): TSIPEventScript;
    procedure SetEventKind(const Value: Integer);
    procedure SetMode(const Value: Integer);
    procedure SetID(const Value: Integer);
    procedure SetCrisisKind(const Value: Integer);
    procedure SetDatabase(const Value: Integer);
    procedure SetLog(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy;override;
    function Add: TSIPEventScript;overload;
    property Text:String read GetText write SetText;
    property Name:String read FName write SetName;
    property EventScript[Index: Integer]: TSIPEventScript read GetEventScript;
    property DatabaseID:Integer read FID write SetID;
    property Log:Boolean read FLog write SetLog;
    function DateTime:TDateTime;
    procedure AddReport(const Info,Kind:String);
    property Database:Integer read FDatabase write SetDatabase;
    function Finished:Boolean;
    procedure UpdateWait(W:Int64);
    procedure BeginItem;
    procedure EndItem;
  published
    property Parameters:TStrings read FParameters write SetParameters;
    property EventKind:Integer read FEventKind write SetEventKind;
    property Mode:Integer read FMode write SetMode;
    property CrisisKind:Integer read FCrisisKind write SetCrisisKind default 0;
  end;

  TSIPEventLibrary=class(TStringList)
  private
    function GetEvent(ID:Integer): TSipEvent;
    function GetEventText(ID:Integer): String;
    procedure SetEventText(ID:Integer; const Value: String);
    function GetEventName(ID: Integer): String;
    procedure SetEventName(ID: Integer; const Value: String);
    procedure AddEvent(Event:TSipEvent);
    function GetEventDatabase(ID: Integer): Integer;
    procedure SetEventDatabase(ID: Integer; const Value: Integer);
  public
    Name:String;
    function AsJSON:String;
    procedure Clear;override;
    destructor Destroy;override;
    property Event[ID:Integer]:TSipEvent read GetEvent;
    property EventText[ID:Integer]:String read GetEventText write SetEventText;
    property EventName[ID:Integer]:String read GetEventName write SetEventName;
    property EventDatabase[ID:Integer]:Integer read GetEventDatabase write SetEventDatabase;
  end;

  TSIPEventComponent=class(TComponent)
  private
    FScript: TSIPEvent;
  published
    property Event:TSIPEvent read FScript write FScript;
  end;

const
  MaxRing=70*1000;
  MaxAnnounce=5*60*1000;
  MaxMonitor=30*1000;

implementation

uses Util, SysUtils, dm_Alarm, SIP_Monitor;

{ TSIPEvent }

function TSIPEvent.Add: TSIPEventScript;
begin
  Result := inherited Add as TSIPEventScript;
end;

var ReportDir:String;

procedure TSIPEvent.AddReport(const Info, Kind: String);
begin
  if not Log then Exit;
  FLock.Enter;
  try
    if Kind='queue' then
    begin
      if FQueueStatus<>'' then
        FQueueStatus:=FQueueStatus+',';
      FQueueStatus:=FQueueStatus+Info;
    end
    else
    begin
      FReport:=FReport+Info;
      if FReportFile=nil then
      begin
        FTime:=Now;
        FReportFile:=TFileStream.Create(ReportDir+
                                        FormatDateTime('yyyy-mm-dd hh-nn-ss',FTime)+
                                        '('+IntToStr(FID)+').log',
                                        fmCreate or fmShareDenyWrite);
      end;
      if (FReportFile<>nil) and (FReport<>'') then
      begin
        FReportFile.Write(FReport[1],Length(FReport));
        FReport:='';
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TSIPEvent.BeginItem;
begin
  Inc(FQueueItems);
end;

constructor TSIPEvent.Create;
begin
  inherited Create(TSIPEventScript);
  FLock:=TCriticalSection.Create;
  FParameters:=TStringList.Create;
end;

function TSIPEvent.DateTime: TDateTime;
begin
  Result:=FTime;
end;

destructor TSIPEvent.Destroy;
var FN:String;
  S:TStringStream;
begin
  FreeAndNil(FParameters);
  if FReportFile<>nil then
    FN:=FReportFile.FileName
  else
    FN:='';
  FreeAndNil(FReportFile);
  if (FN<>'') and (dmAlarm<>nil) then
  begin
    S:=TStringStream.Create(Format('{status:[%s],log:%s}',[FQueueStatus,StrEscape(FileToString(FN))]));
    try
      dmAlarm.AddReport(FormatDateTime('yyyy-mm-dd hh-nn-ss ',FTime)+FName,S,Database);
    finally
      S.Free;
    end;
  end;
  FreeAndNil(FLock);
  inherited;
end;

procedure TSIPEvent.EndItem;
begin
  Dec(FQueueItems);
  if FQueueItems=0 then
    FWait:=GetTick64+MaxMonitor;
end;

function TSIPEvent.Finished: Boolean;
begin
  Result:=(FWait>0) and (FWait<GetTick64);
end;

function TSIPEvent.GetEventScript(Index: Integer): TSIPEventScript;
begin
  Result := Items[Index] as TSIPEventScript;
end;

function TSIPEvent.GetText: String;
var C:TSIPEventComponent;
begin
  C:=TSIPEventComponent.Create(nil);
  try
    C.Event:=Self;
    Result:=ComponentToString(C);
  finally
    C.Free;
  end;
end;

procedure TSIPEvent.SetCrisisKind(const Value: Integer);
begin
  FCrisisKind := Value;
end;

procedure TSIPEvent.SetDatabase(const Value: Integer);
begin
  FDatabase := Value;
end;

procedure TSIPEvent.SetEventKind(const Value: Integer);
begin
  FEventKind := Value;
end;

procedure TSIPEvent.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TSIPEvent.SetLog(const Value: Boolean);
begin
  FLog := Value;
end;

procedure TSIPEvent.SetMode(const Value: Integer);
begin
  FMode := Value;
end;

procedure TSIPEvent.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TSIPEvent.SetParameters(const Value: TStrings);
begin
  FParameters.Assign(Value);
end;

procedure TSIPEvent.SetText(const Value: String);
var S:TSIPEventComponent;
begin
  if Value='' then
  begin
    Clear;
    Exit;
  end;
  S:=TSIPEventComponent.Create(nil);
  S.Event:=Self;
  try
    StringToComponent(Value,S);
  finally
    S.Free;
  end;
end;

procedure TSIPEvent.UpdateWait;
begin
  FWait:=GetTick64+W;
end;

{ TSIPEventScript }

procedure TSIPEventScript.SetCount(const Value: Integer);
begin
  FCount := Value;
end;

procedure TSIPEventScript.SetRingGroup(const Value: Integer);
begin
  FRingGroup := Value;
end;

procedure TSIPEventScript.SetScriptID(const Value: Integer);
begin
  FScriptID := Value;
end;

procedure TSIPEventLibrary.AddEvent(Event: TSipEvent);
begin
  AddObject(IntToStr(Event.DatabaseID),Event);
end;

function TSIPEventLibrary.AsJSON: String;
var I:Integer;
    E:TSIPEvent;
begin
  Result:='';
  for I := 0 to Count - 1 do
  begin
    E:=Objects[I] as TSIPEvent;
    if E<>nil then
    begin
      if Result<>'' then Result:=Result+',';
      Result:=Result+Format('{eventid:%d,eventname:%s}',[E.DatabaseID,StrEscape(E.Name)]);
    end;
  end;
end;

procedure TSIPEventLibrary.Clear;
var O:TObject;
    I:Integer;
begin
  for I := 0 to Count - 1 do
  begin
    O:=Objects[I];
    Objects[I]:=nil;
    FreeAndNil(O);
  end;
  inherited;
end;

destructor TSIPEventLibrary.Destroy;
begin
  Clear;
  inherited;
end;

function TSIPEventLibrary.GetEvent(ID:Integer): TSipEvent;
var I:Integer;
begin
  I:=IndexOf(IntToStr(ID));
  if I>=0 then
    Result:=Objects[I] as TSipEvent
  else
    Result:=nil;
end;

function TSIPEventLibrary.GetEventDatabase(ID: Integer): Integer;
var S:TSIPEvent;
begin
  S:=Event[ID];
  if S=nil then
    Result:=-1
  else
    Result:=Event[ID].Database;
end;

function TSIPEventLibrary.GetEventName(ID: Integer): String;
var S:TSIPEvent;
begin
  S:=Event[ID];
  if S=nil then
    Result:=''
  else
    Result:=Event[ID].Name;
end;

function TSIPEventLibrary.GetEventText(ID:Integer): String;
var S:TSIPEvent;
begin
  S:=Event[ID];
  if S=nil then
    Result:=''
  else
    Result:=Event[ID].Text;
end;

procedure TSIPEventLibrary.SetEventDatabase(ID: Integer; const Value: Integer);
var S:TSIPEvent;
begin
  if IndexOf(IntToStr(ID))<0 then S:=TSIPEvent.Create
  else S:=Event[ID];
  S.Database:=Value;
  S.DatabaseID:=ID;
  if IndexOf(IntToStr(ID))<0 then AddEvent(S);
end;

procedure TSIPEventLibrary.SetEventName(ID: Integer; const Value: String);
var S:TSIPEvent;
begin
  if IndexOf(IntToStr(ID))<0 then S:=TSIPEvent.Create
  else S:=Event[ID];
  S.Name:=Value;
  S.DatabaseID:=ID;
  if IndexOf(IntToStr(ID))<0 then AddEvent(S);
end;

procedure TSIPEventLibrary.SetEventText(ID:Integer; const Value: String);
var S:TSIPEvent;
begin
  if IndexOf(IntToStr(ID))<0 then S:=TSIPEvent.Create
  else S:=Event[ID];
  S.Text:=Value;
  S.DatabaseID:=ID;
  if IndexOf(IntToStr(ID))<0 then AddEvent(S);
end;


initialization
  ReportDir:=ExtractFilePath(ParamStr(0))+'Reports\';
  ForceDirectories(ReportDir);
end.
