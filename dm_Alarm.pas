unit dm_Alarm;

interface

uses
  SysUtils, Classes, Menus, ExtCtrls, Forms, dm_FixedConnPool,SIP_Thread,
  Controls, SIP_RingList,SIP_App, rtcHttpSrv, rtcSrvModule, rtcConn,
  rtcFunction, rtcInfo, SIP_Env, rtcDataSrv, DB, ADODB, Dialogs;

type
  TdmAlarm = class(TDataModule)
    TrayIcon1: TTrayIcon;
    PopupMenu1: TPopupMenu;
    mnuExit: TMenuItem;
    QRingGroup: TADOQuery;
    Timer1: TTimer;
    StartupTimer: TTimer;
    procedure mnuExitClick(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrayIcon1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StartupTimerTimer(Sender: TObject);
  private
    Connection:TFixedConnectionPool;
    ChannelCount:Integer;
    HTTPPort:String;
    procedure LoadAnnouncements(C:IConnection;SIPEnv:ISIPEnv);
    procedure LoadScripts(C:IConnection;SIPEnv:ISIPEnv);
    procedure LoadAddresses(C:IConnection;SIPEnv:ISIPEnv);
    procedure LoadRingGroups(C:IConnection;SIPEnv:ISIPEnv);
    procedure LoadEvents(C:IConnection;SIPEnv:ISIPEnv);
    procedure LoadSettings(C:IConnection;SIPEnv:ISIPEnv);
    procedure LoadFiles(C:IConnection;SIPEnv:ISIPEnv);
    procedure Reload(Env:ISIPEnv;Check:Boolean);
    procedure CheckDB(C:IConnection);
    procedure rtcReload(Sender:TRtcConnection;Param:TRtcFunctionInfo;Result:TRtcValue);
    procedure rtcFireEvent(Sender:TRtcConnection;Param:TRtcFunctionInfo;Result:TRtcValue);
    procedure CheckStatus(Sender: TRtcConnection);
    procedure DataStatus(Sender: TRtcConnection);
    procedure ExecEvent(Env: ISIPEnv; EventID: Integer; DatabaseID: Integer; Log:Boolean);
    function EventsJSON: String;
    function ReportsJSON: String;
    function ReportJSON(FileMode,FileID:Integer): String;
    function EventRingListJSON(EventID:Integer;Env: ISIPEnv): String;
    function EventRingList(EventMode,EventID:Integer): String;
    function CheckUserAndPassword(User,Password:String;DatabaseID: Integer;RightAbbr,RightID:String):boolean;
    function Confirm(Msg: String; DlgType: TMsgDlgType): Boolean;
    procedure Startup;
  public
    SIPApp:TSIPApp;
    SIPThread:TSIPThread;
    InitEnv:ISIPEnv;
    Mode:Integer;
    DiagnosticEvent:Integer;

    IniFile:String;

    HTTP:TRtcHttpServer;
    HTTPModule:TRtcServerModule;
    HTTPFunctions:TRtcFunctionGroup;
    HTTPStatus:TRtcDataProvider;

    ReloadProc:procedure of object;

    procedure SignalRS(Signal:Integer);
    procedure FireEvent(EventID,DatabaseID:Integer;Log:Boolean);
    procedure StopEvent(EventID:Integer);
    procedure ReloadEnv;
    procedure Pause;
    procedure Continue_;
    procedure Stop;
    function  GroupRingList(Group:Integer;DatabaseID:Integer):TSIPRingList;
    procedure AddReport(ReportName:String;Info:TStream;DatabaseID:Integer);
  end;

var
  dmAlarm: TdmAlarm;
  Interactive: Boolean;

const
  ServiceName='ASOS';

implementation
uses SIP_Sound, SIP_Event, Logger, dm_AlarmDB, Util, SIP_Script, Variants,
  Windows, SIP_Monitor, SIP_Call, StrUtils;
{$R *.dfm}

procedure TdmAlarm.AddReport(ReportName:String;Info:TStream;DatabaseID:Integer);
var C:IConnection;
    Q:TADOQuery;
begin
  C:=Connection.GetConnection;
  Q:=TADOQuery.Create(nil);
  try
    Q.Connection:=C.Connection;
    Q.ParamCheck:=True;
    Q.SQL.Text:='insert into asos'+IntToStr(DatabaseID)+'..tblReports(filepath,filename,filedata,lastchange) values (''справки/събития'',:name,:report,GetDate())';
    Q.Parameters.ParamByName('name').DataType:=ftString;
    Q.Parameters.ParamByName('name').Value:=LeftStr(ReportName,50);
    Q.Parameters.ParamByName('report').DataType:=ftBlob;
    Q.Parameters.ParamByName('report').LoadFromStream(Info,ftBlob);
    Q.ExecSQL;
  finally
    Q.Free;
  end;
end;

procedure TdmAlarm.ExecEvent(Env: ISIPEnv; EventID: Integer; DatabaseID: Integer; Log:Boolean);
var
  E: TSIPEvent;
  I: Integer;
  G: TSIPRingList;
  S: TSIPScript;
  StartID:Integer;
  Counter:PInteger;
  T,Report:String;
begin
  E := Env.EventLibrary.Event[EventID];
  E.Log:=Log;
  StartID:=0;
  Counter:=nil;
  Report:='';
  if E = nil then
    Env := nil
  else
  begin
    SIPApp.SIPQueueManager.Acquire;
    try
      T:=FormatDateTime('yyyy-mm-dd hh:nn:ss ',Now);
      for I := 0 to E.Count - 1 do
      begin
        S:=Env.SIPLibrary.Script[E.EventScript[I].ScriptID];
        if S=nil then Continue;
        if not S.IsGroup
        or (E.EventScript[I].RingGroup <= 0) then
          SIPApp.SIPQueueManager.Add(E.EventScript[I].ScriptID, nil, E.EventScript[I].Count, Env, E, StartID,Counter,Report)
        else
        begin
          G := GroupRingList(E.EventScript[I].RingGroup, DatabaseID);
          if G <> nil then
            SIPApp.SIPQueueManager.Add(E.EventScript[I].ScriptID, G, E.EventScript[I].Count, Env, E, StartID,Counter,Report);
        end;
      end;
      if Counter<>nil then
        E.AddReport(T+' Стартиране на оповестяване за събитие "'+E.Name+'", брой обекти за оповестяване: '+IntToStr(Counter^)+#13#10+Report,'');
    finally
      SIPApp.SIPQueueManager.Release;
    end;
    {if Counter<>nil then
      E.AddReport(#13#10'Изпълнение на сценарии'+', брой обекти за оповестяване: '+IntToStr(Counter^)+#13#10'========================='#13#10,'')
    else
      E.AddReport(#13#10'Изпълнение на сценарии'#13#10'========================='#13#10,'');}
  end;
end;

procedure TdmAlarm.Continue_;
begin
  SIPThread.Resume;
  HTTP.Listen;
  Timer1.Enabled:=True;
end;

procedure TdmAlarm.DataModuleCreate(Sender: TObject);
var Settings:TStringList;

begin
  if not Interactive then
    TrayIcon1.OnMouseDown:=nil
  else
    TrayIcon1.Visible:=True;  
  ChannelCount:=30;

  IniFile:=ChangeFileExt(ParamStr(0),'.ini');
  if FileExists(IniFile) then
  begin
    Settings:=TStringList.Create;
    try
      Settings.LoadFromFile(IniFile);
      Mode:=StrToIntDef(Settings.Values['Mode'],1);
      ChannelCount:=StrToIntDef(Settings.Values['SIPChannels'],30);
      HTTPPort:=Settings.Values['HTTPPort'];
      if not Interactive then
      begin
        StartupTimer.Interval:=StrToIntDef(Settings.Values['StartupTimer'],0)*1000;
      end else
        StartupTimer.Interval:=100;
    finally
      Settings.Free;
    end;
  end
  else
  begin
    if ParamCount>0 then
      Mode:=StrToIntDef(ParamStr(1),1)
    else
      Mode:=1;
    IniFile:='';
  end;

  if HTTPPort='' then HTTPPort:='8080';
  if StartupTimer.Interval<=0 then StartupTimer.Interval:=100;
  Randomize;

  StartupTimer.Enabled:=True;
end;

procedure TdmAlarm.Startup;
var
  F:TRtcFunction;

  procedure AddFunction(FunctionName:String;E:TRtcFunctionCallEvent);
  begin
    F:=TRtcFunction.Create(Self);
    F.FunctionName:=FunctionName;
    F.OnExecute:=E;
    F.Group:=HTTPFunctions;
  end;
begin
  Connection:=TFixedConnectionPool.Create(10,0,1000);

  InitEnv:=TSIPEnv.Create(1,False);
  try
    Reload(InitEnv,True);
  except
    on E:Exception do
    begin
      Application.HandleException(E);
      Application.Terminate;
    end;
  end;

  SIPThread:=TSIPThread.Create(StrToIntDef(InitEnv.SIPSettings.Values['StartAccount'],0),
                               ChannelCount,
                               StrToIntDef(InitEnv.SIPSettings.Values['PacketTime'],100),
                               InitEnv.SIPSettings.Values['SIPHost'],
                               InitEnv.SIPSettings.Values['SIPCodec']);
  SIPThread.CallDelay:=StrToIntDef(InitEnv.SIPSettings.Values['RingInterval'],200);

  SIPApp:=TSIPApp.Create(ChannelCount,
                         StrToIntDef(InitEnv.SIPSettings.Values['GSMComPort'],0),
                         StrToIntDef(InitEnv.SIPSettings.Values['GSMComBits'],8),
                         StrToIntDef(InitEnv.SIPSettings.Values['GSMComBaud'],115200),
                         StrToIntDef(InitEnv.SIPSettings.Values['GSMComStop'],1),
                         InitEnv.SIPSettings.Values['GSMComParity'],
                         StrToIntDef(InitEnv.SIPSettings.Values['RSComPort'],0),
                         SignalRS,
                         InitEnv.SIPSettings.Values['SMTPHost'],
                         InitEnv.SIPSettings.Values['SMTPPort'],
                         InitEnv.SIPSettings.Values['SMTPUser'],
                         InitEnv.SIPSettings.Values['SMTPPass'],
                         InitEnv.SIPSettings.Values['SMTPFrom']
                        );

  DiagnosticEvent:=StrToIntDef(InitEnv.SIPSettings.Values['DiagnosticEvent'],0);
  InitArray(InitEnv.SIPSettings.Values['SIPBusy'],SIPBUSY);
  InitArray(InitEnv.SIPSettings.Values['SIPBusyRoute'],SIPBUSYROUTE);
  InitArray(InitEnv.SIPSettings.Values['SIPNoAnswer'],SIPNOANSWER);
  InitArray(InitEnv.SIPSettings.Values['SIPNotFound'],SIPNOTFOUND);

  HTTP:=TRtcHttpServer.Create(self);
  HTTP.ServerPort:=HTTPPort;
  HTTPFunctions:=TRtcFunctionGroup.Create(self);
  HTTPModule:=TRtcServerModule.Create(self);
  HTTPModule.Server:=HTTP;
  HTTPModule.ModuleFileName:='/exec';
  HTTPModule.FunctionGroup:=HTTPFunctions;
  HTTPStatus:=TRtcDataProvider.Create(self);
  HTTPStatus.Server:=HTTP;
  HTTPStatus.OnDataReceived:=DataStatus;
  HTTPStatus.OnCheckRequest:=CheckStatus;

  AddFunction('Reload',rtcReload);
  AddFunction('FireEvent',rtcFireEvent);

  HTTP.Listen;

  Timer1.Enabled:=True;

end;

procedure TdmAlarm.StartupTimerTimer(Sender: TObject);
begin
  StartupTimer.Enabled:=False;
  Startup;
end;

procedure TdmAlarm.CheckDB;
var Q:TADOQuery;
  Exists:Boolean;
begin
  Q:=TADOQuery.Create(nil);
  try
    Q.Connection:=C.Connection;
    Q.ParamCheck:=True;
    Q.SQL.Text:='select count(*) from sysobjects where xtype=''v'' and name=''vCity''';
    Q.Open;
    Exists:=not Q.Fields[0].IsNull and (Q.Fields[0].AsInteger>0);
    Q.Close;
    if not Exists then
    begin
      Q.SQL.Text:=
      'create view vCity as'#13#10+
      'select c1.ekatte,c1.name+'', общ. ''+c2.name+'', обл. ''+c3.name as name'#13#10+
      'from tblcity c1'#13#10+
      'left join tblcity c2 on c2.id=round(c1.id/10000000,0,1)'#13#10+
      'left join tblcity c3 on c3.id=round(c2.id/100,0,1)'#13#10+
      'where len(c1.kmetstvo)>5';
      Q.ExecSQL;
    end;
  finally
    Q.Free;
  end;

end;

procedure TdmAlarm.CheckStatus(Sender: TRtcConnection);
begin
  with TRtcDataServer(Sender) do
  if SameText(Copy(Request.FileName,1,7),'/status')
  or SameText(Copy(Request.FileName,1,5),'/mode') then
    Accept;
end;

function TdmAlarm.CheckUserAndPassword;
var
    Env:ISIPEnv;
    C:IConnection;
    Q:TADODataSet;
    QUser,QPass:TField;
begin
  Env:=TSIPEnv.Create(DatabaseID,False);
  try
    C:=Connection.GetConnection;
    C.Connection.Execute('USE asos'+IntToStr(Env.DatabaseID));

    Q:=TADODataSet.Create(nil);
    try                      
      Q.CommandText:=Format('select u.* from tbluser u join tblrights r on r.workgroup=u.workgroup and r.rightabbr=''%s'' and r.rightid=''%s'' and u.username=''%s'' and u.password=''%s'' union select * from tblUser where workgroup=1 and username=''%s''',
                            [DoubleQuote(RightAbbr),
                             DoubleQuote(RightID),
                             DoubleQuote(User),
                             DoubleQuote(Password),
                             DoubleQuote(User)]);
      Q.CommandType:=cmdText;
      Q.Connection:=C.Connection;
      Q.Open;
      Result:=False;
      QUser:=Q.FieldByName('UserName');
      QPass:=Q.FieldByName('Password');
      while not Q.Eof do
      begin
        if AnsiSameText(QUser.AsString,User) and AnsiSameText(QPass.AsString,Password) then
        begin
          Result:=True;
          Break;
        end;
        Q.Next;
      end;
    finally
      Q.free;
    end;

  finally
    Env:=nil;
  end;
end;

procedure TdmAlarm.DataStatus(Sender: TRtcConnection);
var S:String;
  FN:String;
  Ext:String;
  EventID,ModeID:Integer;
  NewMode:Integer;
begin
  with TRtcDataServer(Sender) do
  if Request.Complete then
  begin
    S:=Request.FileName;
    if Pos('?',S)>0 then Delete(S,Pos('?',S),Length(S));
    Delete(S,1,7);
    if SameText(S,'/stopevent') then
    begin
      Request.Params.AddText(Sender.Read);
      Response.ContentType:='text/json; charset=windows-1251';
      EventID:=StrToIntDef(Request.Params.Value['eventid'],-1);
      if (EventID<=0) then
         Write('{success:false,errors:{username:"невалидни параметри"}}')
      else
      begin
        ModeID:=SIPApp.SIPQueueManager.Mode(EventID);
        if ModeID>0 then
        begin
          if CheckUserAndPassword(Request.Params.Value['username'],Request.Params.Value['password'],ModeID,'Event.Start',IntToStr(EventID)) then
            begin
              StopEvent(EventID);
              Write('{success:true}')
            end
            else
              Write('{success:false,errors:{username:"невалиден потребител или парола",password:"невалиден потребител или парола"}}');
        end else
           Write('{success:false,errors:{username:"събитието не е открито"}}');
      end;
    end else
    if SameText(S,'/fireevent') then
    begin
      Request.Params.AddText(Sender.Read);
      Response.ContentType:='text/json; charset=windows-1251';
      ModeID:=StrToIntDef(Request.Params.Value['mode'],-1);
      EventID:=StrToIntDef(Request.Params.Value['eventid'],-1);
      if (EventID<=0) or (ModeID<=0) then
         Write('{success:false,errors:{username:"невалидни параметри"}}')
      else
      if CheckUserAndPassword(Request.Params.Value['username'],Request.Params.Value['password'],ModeID,'Event.Start',IntToStr(EventID)) then
      begin
        FireEvent(EventID,ModeID,True);
        Write('{success:true}')
      end
      else
        Write('{success:false,errors:{username:"невалиден потребител или парола",password:"невалиден потребител или парола"}}');
    end else
    if SameText(S,'/mode') then
    begin
      if SameText(Request.Method,'put') then
      begin
        Request.Params.AddText(Sender.Read);
        Response.ContentType:='text/json; charset=windows-1251';
        NewMode:=StrToIntDef(Request.Params.Value['mode'],1);
        if CheckUserAndPassword(Request.Params.Value['username'],Request.Params.Value['password'],NewMode,'Mode.Set','*') then
        begin
          Mode:=NewMode;
          Write(Format('{success:true,mode:"%d"}',[Mode]))
        end
        else
          Write('{success:false,errors:{username:"невалиден потребител или парола",password:"невалиден потребител или парола"}}');
      end
      else
      if SameText(Request.Method,'get') then
      begin
        //Response.ContentType:='text/plain';
        //Write('mode'+IntToStr(Mode));
        Response.ContentType:='text/json; charset=windows-1251';
          Write(Format('{success:true,data:{mode:"%d"}}',[Mode]))
      end;
      Exit;
    end
    else
    if SameText(S,'/events') then
    begin
      Response.ContentType:='text/json; charset=windows-1251';
      Write(EventsJSON);
    end else
    if SameText(S,'/reports') then
    begin
      Response.ContentType:='text/json; charset=windows-1251';
      Write(ReportsJSON);
    end else
    if SameText(S,'/report') then
    begin
      Request.Params.AddText(Sender.Read);
      Response.ContentType:='text/json; charset=windows-1251';
      Write(ReportJSON(StrToIntDef(Request.Params.Value['mode'],0),StrToIntDef(Request.Params.Value['fileid'],0)));
    end else
    if SameText(S,'/ringlist') then
    begin
      Request.Params.AddText(Sender.Read);
      Response.ContentType:='text/json; charset=windows-1251';
      Write(EventRingList(StrToIntDef(Request.Params.Value['mode'],0),StrToIntDef(Request.Params.Value['eventid'],0)));
    end else
    if SameText(S,'/channels') or (S='') then
      Write('<html><head><meta http-equiv="Content-Type" content="text/html; charset=windows-1251"/></head><body><pre>'+SIPApp.SIPStatus.Get+'</pre></body></html>')
    else
    if SameText(S,'/queues') then
    begin
      Request.Params.AddText(Sender.Read);
      Write(Format('{"channels":%s,%s,"monitor":%s,"mode":%d}',[SIPApp.SIPStatus.Get,SIPApp.SIPQueueManager.QueueStatus(StrToIntDef(Request.Params.Value['queueid'],0)),SIPMonitorStatus,Mode]));
      Response.ContentType:='text/json; charset=windows-1251';
    end
    else
    begin
      Ext:=ExtractFileExt(S);
      S:=StringReplace(S,'/','\',[rfReplaceAll]);
      FN:=ExtractFilePath(ParamStr(0))+'monitor'+S;
      if FileExists(FN) then
      begin
        Response.ContentType:=GetContentType(Ext);
        Write(FileToString(FN));
      end
      else
      begin
        Response.Status(404,'invalid resource '+Request.FileName);
        Response.SendContent:=False;
        Write;
      end;
    end;
  end;
end;


procedure TdmAlarm.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(SIPApp);
  FreeAndNil(SIPThread);
  FreeAndNil(Connection);
end;

function TdmAlarm.EventRingListJSON(EventID:Integer;Env: ISIPEnv): String;
var
  E: TSIPEvent;
  I: Integer;
  G: TSIPRingList;
  S: TSIPScript;
begin
  Result:='';
  E := Env.EventLibrary.Event[EventID];
  if E <> nil then
    for I := 0 to E.Count - 1 do
    begin
      S:=Env.SIPLibrary.Script[E.EventScript[I].ScriptID];
      if S=nil then Continue;
      if not S.IsGroup or (E.EventScript[I].RingGroup <= 0) then
      else
      begin
        G := GroupRingList(E.EventScript[I].RingGroup, Env.DatabaseID);
        if G <> nil then
        begin
          if Result<>'' then Result:=Result+',';
          Result:=Result+G.Status('"state":'+StrEscape(Env.FileNames.Values[IntToStr(E.EventScript[I].ScriptID)])+',',IntToStr(I)+'-',False,False);
        end;
      end;
    end;
  Result:='['+Result+']';
   
end;


function TdmAlarm.EventRingList(EventMode, EventID: Integer): String;
var
    Env:ISIPEnv;
    C:IConnection;
begin
  Result:='';
  if (EventID<=0) or (EventMode<=0) then Exit;

  Env:=TSIPEnv.Create(EventMode,False);
  try
    C:=Connection.GetConnection;
    C.Connection.Execute('USE asos'+IntToStr(Env.DatabaseID));
    LoadEvents(C,Env);
    LoadFiles(C,Env);
    LoadScripts(C,Env);

    Result:=Format('{ringlist:%s}',[EventRingListJSON(EventID,Env)]);
  finally
    Env:=nil;
  end;
end;

function TdmAlarm.EventsJSON:String;
var
    Env:ISIPEnv;
    C:IConnection;
begin
  Env:=TSIPEnv.Create(Mode,False);
  try
    C:=Connection.GetConnection;
    C.Connection.Execute('USE asos'+IntToStr(Env.DatabaseID));
    LoadEvents(C,Env);
    LoadFiles(C,Env);
    Result:=Format('{events:[%s],mode:%d}',[Env.EventLibrary.AsJSON,Env.DatabaseID]);
  finally
    Env:=nil;
  end;
end;

procedure TdmAlarm.FireEvent(EventID,DatabaseID:Integer;Log:Boolean);
var
    Env:ISIPEnv;
begin
  Env:=TSIPEnv.Create(DatabaseID,Log);
  try
    Reload(Env,False);
  except
    Env:=nil;
    raise;
  end;
  ExecEvent(Env, EventID, DatabaseID, Log);
end;

function TdmAlarm.GroupRingList(Group: Integer;DatabaseID: Integer): TSIPRingList;
var Q:TADODataSet;
    C:IConnection;
begin
  Result:=nil;
  Q:=TADODataSet.Create(nil);
  try
    Q.CommandText:=Format(QRingGroup.SQL.Text,[Group]);
    Q.CommandType:=cmdText;
    C:=Connection.GetConnection;
    Q.Connection:=C.Connection;
    Q.Connection.Execute('USE asos'+IntToStr(DatabaseID));
    Q.Open;
    try
      while not Q.eof do
      begin
        if Result=nil then Result:=TSIPRingList.Create;
        Result.Add(Q);
        Q.Next;
      end;
    except
      if Result<>nil then Result.Free;
      raise;
    end;
  finally
    Q.free;
  end;
end;

procedure TdmAlarm.LoadAddresses;
var Q:TADODataSet;
begin
  Q:=TADODataSet.Create(nil);
  try
    Q.CommandText:='select Code,Phone,Phone2,Phone3,DTMF,EMail from tblOrganisation order by Code';
    Q.CommandType:=cmdText;
    Q.Connection:=C.Connection;
    Q.Open;
    SIPEnv.SIPAddress.Clear;
    SIPEnv.SIPDTMF.Clear;
    SIPEnv.SIPMobile.Clear;
    SIPEnv.SIPHome.Clear;
    SIPEnv.SIPMail.Clear;
    while not Q.eof do
    begin
      SIPEnv.SIPAddress.Add(IntToStr(Trunc(Q.FieldByName('Code').AsFloat))+'='+Q.FieldByName('Phone').AsString);
      SIPEnv.SIPMobile.Add(IntToStr(Trunc(Q.FieldByName('Code').AsFloat))+'='+Q.FieldByName('Phone2').AsString);
      SIPEnv.SIPHome.Add(IntToStr(Trunc(Q.FieldByName('Code').AsFloat))+'='+Q.FieldByName('Phone3').AsString);
      SIPEnv.SIPDTMF.Add(IntToStr(Trunc(Q.FieldByName('Code').AsFloat))+'='+Q.FieldByName('DTMF').AsString);
      SIPEnv.SIPMail.Add(IntToStr(Trunc(Q.FieldByName('Code').AsFloat))+'='+Q.FieldByName('EMail').AsString);
      Q.Next;
    end;
  finally
    Q.free;
  end;
end;

procedure TdmAlarm.LoadEvents;
var Q:TADODataSet;
begin
  Q:=TADODataSet.Create(nil);
  try
    Q.CommandText:='select * from tblFiles where FilePath=''събития''';
    Q.CommandType:=cmdText;
    Q.Connection:=C.Connection;
    Q.Open;
    SIPEnv.EventLibrary.Clear;
    while not Q.eof do
    begin
      SIPEnv.EventLibrary.EventText[Q.FieldByName('FileID').AsInteger]:=Q.FieldByName('FileData').AsString;
      SIPEnv.EventLibrary.EventName[Q.FieldByName('FileID').AsInteger]:=Q.FieldByName('FileName').AsString;
      SIPEnv.EventLibrary.EventDatabase[Q.FieldByName('FileID').AsInteger]:=SIPEnv.DatabaseID;
      Q.Next;
    end;
  finally
    Q.free;
  end;
//  SIPEnv.LoadName;
end;

procedure TdmAlarm.LoadFiles(C: IConnection; SIPEnv: ISIPEnv);
var Q:TADODataSet;
begin
  Q:=TADODataSet.Create(nil);
  try
    Q.CommandText:='select FileID,FileName from tblFiles';
    Q.CommandType:=cmdText;
    Q.Connection:=C.Connection;
    Q.Open;
    SIPEnv.FileNames.Clear;
    while not Q.eof do
    begin
      SIPEnv.FileNames.Add(Q.FieldByName('FileID').AsString+'='+Q.FieldByName('FileName').AsString);
      Q.Next;
    end;
  finally
    Q.free;
  end;
end;

procedure TdmAlarm.LoadRingGroups;
var Q:TADODataSet;
begin
  Q:=TADODataSet.Create(nil);
  try
    Q.CommandText:='select ID,RingGroupName from tblRingGroups order by ID';
    Q.CommandType:=cmdText;
    Q.Connection:=C.Connection;
    Q.Open;
    SIPEnv.RingGroups.Clear;
    while not Q.eof do
    begin
      SIPEnv.RingGroups.AddObject(Q.FieldByName('RingGroupName').AsString,TObject(Q.FieldByName('ID').AsInteger));
      Q.Next;
    end;
  finally
    Q.free;
  end;
end;

procedure TdmAlarm.LoadScripts;
var Q:TADODataSet;
begin
  Q:=TADODataSet.Create(nil);
  try
    Q.CommandText:='select * from tblFiles where FilePath=''сценарии''';
    Q.CommandType:=cmdText;
    Q.Connection:=C.Connection;
    Q.Open;
    SIPEnv.SIPLibrary.Clear;
    while not Q.eof do
    begin
      SIPEnv.SIPLibrary.ScriptText[Q.FieldByName('FileID').AsInteger]:=Q.FieldByName('FileData').AsString;
      SIPEnv.SIPLibrary.ScriptName[Q.FieldByName('FileID').AsInteger]:=Q.FieldByName('FileName').AsString;
      Q.Next;
    end;
  finally
    Q.free;
  end;
end;

procedure TdmAlarm.LoadSettings(C: IConnection; SIPEnv: ISIPEnv);
begin
  SIPEnv.SIPSettings.Clear;
  if FileExists(IniFile) then
    SIPEnv.SIPSettings.LoadFromFile(IniFile);
end;

{procedure TdmAlarm.LoadSettings(C: IConnection; SIPEnv: ISIPEnv);
var Q:TADODataSet;
    N:String;
begin
  Q:=TADODataSet.Create(nil);
  try
    Q.CommandText:='select * from tblSettings';
    Q.CommandType:=cmdText;
    Q.Connection:=C.Connection;
    Q.Open;
    SIPEnv.SIPSettings.Clear;
    while not Q.eof do
    begin
      N:=Q.FieldByName('SettingID').AsString;
      if N='0' then
        N:=''
      else
        N:='['+N+']';
      SIPEnv.SIPSettings.Add(Q.FieldByName('SettingName').AsString+N+'='+Q.FieldByName('SettingValue').AsString);
      Q.Next;
    end;
    SIPEnv.SIPSettings.Add('SIPHost=localhost');
  finally
    Q.free;
  end;
end;}

procedure TdmAlarm.LoadAnnouncements;
var Q:TADODataSet;
    S:TSIPAnnouncement;
begin
  Q:=TADODataSet.Create(nil);
  try
    Q.CommandText:=
      'select isnull(w.fileid,t.fileid) fileid,isnull(w.filename,t.filename) filename,w.filedata wavedata,t.filedata textdata'#13#10+
      'from'#13#10+
      '  (select * from tblfiles where filepath=''съобщения'') w'#13#10+
      'full join'#13#10+
      '  (select * from tblfiles where filepath=''съобщения/текст'') t on'#13#10+
      '  w.filename=t.filename';
    Q.CommandType:=cmdText;
    Q.Connection:=C.Connection;
    Q.Open;
    SIPEnv.SIPSound.Clear;
    while not Q.eof do
    begin
      S:=TSIPAnnouncement.Create;
      S.Wave:=Q.FieldByName('WaveData').AsString;
      S.Text:=Q.FieldByName('TextData').AsString;
      S.Name:=Q.FieldByName('FileName').AsString;
      SIPEnv.SIPSound.Buffer[Q.FieldByName('FileID').AsInteger]:=S;
      Q.Next;
    end;
  finally
    Q.free;
  end;
end;

function TdmAlarm.Confirm(Msg: String; DlgType: TMsgDlgType): Boolean;
begin
  Result:=MessageDlg(Msg,DlgType,[mbYes,mbNo],0)=mrYes;
end;

procedure TdmAlarm.mnuExitClick(Sender: TObject);
begin
  if not Confirm('Моля потвърдете затваряне на приложението',mtWarning) then Exit;
  Stop;
  Application.Terminate;
end;

procedure TdmAlarm.Pause;
begin
  Timer1.Enabled:=False;
  SIPThread.Suspend;
  HTTP.StopListen;
end;

procedure TdmAlarm.Reload(Env:ISIPEnv;Check:Boolean);
var C:IConnection;
begin
  C:=Connection.GetConnection;
  C.Connection.Execute('USE asos'+IntToStr(Env.DatabaseID));
  if Check then CheckDB(C);
  LoadScripts(C,Env);
  LoadAnnouncements(C,Env);
  LoadAddresses(C,Env);
  LoadRingGroups(C,Env);
  LoadEvents(C,Env);
  LoadSettings(C,Env);
  LoadFiles(C,Env);

  if Assigned(ReloadProc) then ReloadProc;

end;

procedure TdmAlarm.ReloadEnv;
begin
  Reload(InitEnv,False);
  DiagnosticEvent:=StrToIntDef(InitEnv.SIPSettings.Values['DiagnosticEvent'],0);
end;

function TdmAlarm.ReportJSON(FileMode,FileID : Integer): String;
var
  C:IConnection;
  Q:TADODataSet;
  ReportMode:Integer;
begin
  Result:='';
  if (FileID<=0) or (FileMode<=0) then Exit;
  C:=Connection.GetConnection;
  ReportMode:=Mode;
  C.Connection.Execute('USE asos'+IntToStr(ReportMode));

  Q:=TADODataSet.Create(nil);
  try
    Q.CommandText:='select FileData from tblReports where fileid='+IntTostr(FileID);
    Q.CommandType:=cmdText;
    Q.Connection:=C.Connection;
    Q.Open;
    Result:=Q.Fields[0].AsString;
  finally
    Q.free;
  end;
end;

function TdmAlarm.ReportsJSON: String;
var
  C:IConnection;
  Q:TADODataSet;
  ReportMode:Integer;
begin
  Result:='';
  C:=Connection.GetConnection;
  ReportMode:=Mode;
  C.Connection.Execute('USE asos'+IntToStr(ReportMode));

  Q:=TADODataSet.Create(nil);
  try
    Q.CommandText:='select FileID,FileName,LastChange from tblReports where filepath=''справки/събития'' order by 3 desc';
    Q.CommandType:=cmdText;
    Q.Connection:=C.Connection;
    Q.Open;
    while not Q.eof do
    begin
      if Result<>'' then Result:=Result+',';
      Result:=Result+Format('{fileid:%s,filename:%s}',[
              StrEscape(Q.FieldByName('FileID').AsString),
              StrEscape(Q.FieldByName('FileName').AsString)]);
      Q.Next;
    end;
  finally
    Q.free;
  end;
  Result:='{reports:['+Result+'],mode:'+IntToStr(ReportMode)+'}';
end;

procedure TdmAlarm.rtcFireEvent(Sender: TRtcConnection; Param: TRtcFunctionInfo;
  Result: TRtcValue);
begin
  FireEvent(Param.asInteger['EventID'],Param.asInteger['DatabaseID'],True);
end;

procedure TdmAlarm.rtcReload(Sender: TRtcConnection; Param: TRtcFunctionInfo;
  Result: TRtcValue);
begin
  Reload(InitEnv,False);
end;

procedure TdmAlarm.SignalRS(Signal: Integer);
var
  Env:ISIPEnv;
  I: Integer;
  E:TSIPEvent;
begin
  if Log<>nil then Log.Debug('Signal '+IntToStr(Signal));
  Env:=TSIPEnv.Create(Mode,True);
  try
    Reload(Env,False);
  except
    Env:=nil;
    raise;
  end;
  for I := 0 to Env.EventLibrary.Count - 1 do
  begin
    E:=TSIPEvent(Env.EventLibrary.Objects[I]);
    if (E.EventKind=0) and (E.Parameters.Values['ComSignal']=IntToStr(Signal)) then
      ExecEvent(Env, E.DatabaseID, Mode, True);
  end;
end;

procedure TdmAlarm.Stop;
begin
  Timer1.Enabled:=False;
  if HTTP<>nil then HTTP.StopListen;
  FreeAndNil(dmAlarm.SIPApp.SIPStatus);
  SIPThread.Terminate;
end;

procedure TdmAlarm.StopEvent(EventID:Integer);
begin
  SIPApp.Stop(EventID);
end;

procedure TdmAlarm.Timer1Timer(Sender: TObject);
begin
  if DiagnosticEvent<=0 then Exit;
  
  if SIPMonitor.Status_SIP+120000<GetTick64 then
  begin
    Timer1.Interval:=60000;
    FireEvent(DiagnosticEvent,Mode,False);
  end else
    Timer1.Interval:=15000;
end;

procedure TdmAlarm.TrayIcon1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  PopupMenu1.Popup(X,Y);
end;

end.
