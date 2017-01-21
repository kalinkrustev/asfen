unit SIP_Call;

interface
uses SIP_Script, Classes, SIP_App ,SIP_Action, SIP_RingList, SysUtils, SyncObjs,
     SIP_Env, pjsua, DTMF, Log4D, SIP_Event;

type
  TStatusKind=(skBusy,skBusyRoute,skNoAnswer,skNoDTMF,skNotFound,skError,skSuccess,skFailDTMF,skFailAnnounce);
  TDTMFState=(dNone,dNotDetected,dDetectedDigit,dDetectedAll,dNotExpected,dHangedup,dRetry);
  TSIPCall=class(TThread)
  private
    CurrentScript:TSIPScript;
    CurrentAction:Integer;
    CurrentMessage:String;
    CurrentMessageName:String;
    CurrentStartID:Integer;
    ReceiveCnt,SendCnt:Integer;
    ConversationLock:TEvent;
    Stop:Boolean;
    CurrentAddress:TSIPAddress;
    EnvLock:TCriticalSection;
    QueueID:Cardinal;
    DTMFFail:Boolean;
    DTMF_Decoder:TDTMF;
    DTMFDetectStr:String;
    CurrentEnv:ISIPEnv;
    CurrentEvent:TSIPEvent;
    Stack:TStringList;
    FHangupTimeOut:Int64;
    FIsHangupTimeOut:Boolean;
    FHangup:Boolean;
    procedure DoAction(Action: TSIPAction);
    procedure Ring(Action: TSIPAction);
    procedure SMS(Action: TSIPAction);
    procedure Mail(Action: TSIPAction);
    procedure Delay(Milliseconds:Cardinal);
    procedure Play(FileName:String;Last:Boolean;Action: TSIPAction);
    procedure DTMF;
    procedure Jump(ScriptID:Integer);
    procedure Call(ScriptID:Integer);
    procedure Hangup;
    procedure AddToSIPThread;
    procedure NextAction;
    procedure LastAction;
    procedure Status(const S:String);inline;
    procedure StatusAddress(const S:String);inline;
    procedure NextScript;
    procedure AddReport(Info:String);
    procedure EventReport(Info:String);
    procedure AddSamples(Buffer: pointer; Size: Integer);
    function StatusKind(Status: Integer): TStatusKind;
    procedure RingTimeout;
  protected
    FID:Integer;
    FSIPApp:TSIPApp;
    FReport:String;
    FLastScript:Integer;
    FLastScriptDescription:String;
    FLastPhone:String;
    FLastPlayAction:TSIPAction;
    FDTMFWaitCount,FDTMFFrameCount:Integer;
    ExpectedDigits,DetectedDigits:String;
    DTMFState:TDTMFState;
    FChannelLog:TLogLogger;
    FAccount:Integer;
    procedure Execute;override;
  public
    Phone:String;
    PhoneKind:String;
    CallID:Integer;
    DTMF_Port:pjmedia_port;
    procedure  InitDTMF;
    constructor Create(App:TSIPApp;ID:Integer);
    destructor Destroy;override;
    procedure  Signal;
    procedure  SignalFailure;
    procedure  StartSIP;
    procedure  Retry(Status:Integer;Description:String='');
    function  DTMFFrame:boolean;
    procedure DTMFDetect;
    procedure DTMFTimeout;
    procedure LogError(S:String;const Err: Exception = nil);
    procedure LogInfo(S:String);
    procedure LogDebug(S:String);
    procedure StopEvent(EventID:Integer);
    procedure StopCurrent;
    procedure Received(Buffer:Pointer;Size:Integer);
    procedure Sent(Buffer:Pointer;Size:Integer);
    property  InHangup:boolean read FHangup write FHangup;
    property  ChannelIndex:Integer read FID;
  end;

  const DefRetry=0;
  const DefDelay=15;

  type EventArray=array of Integer;

  var SIPBUSY,SIPBUSYROUTE,SIPNOANSWER,SIPNOTFOUND:EventArray;

  procedure InitArray(S:String;out A:EventArray);

implementation
uses SIP_Thread,Logger,dm_Alarm, Windows, SIP_Sound, StrUtils, SIP_Monitor,SIP_QueueManager;

procedure InitArray(S:String;out A:EventArray);
var L:TStrings;
  I: Integer;
begin
  L:=TStringList.Create;
  try
    L.CommaText:=S;
    SetLength(A,L.Count);
    for I := 0 to L.Count - 1 do
      A[I]:=StrToIntDef(L[I],0);
  finally
    L.Free;
  end;
end;

function InArray(E:Integer;A:EventArray):Boolean;
var
  I: Integer;
begin
  Result:=False;
  for I := 0 to Length(A)-1 do
  if A[I]=E then
  begin
    Result:=True;
    Break;
  end;
end;

{ TSIPCall }

procedure TSIPCall.AddToSIPThread;
begin
  ConversationLock.ResetEvent;
  dmAlarm.SIPThread.AddCall(Self);
  while True do
  case ConversationLock.WaitFor(1000) of
    wrSignaled:
    begin
      FHangupTimeOut:=0;
      FIsHangupTimeOut:=False;
      Break;
    end;
    wrAbandoned:
    begin
      LogError(Format('Conversation event abandoned CallID: %d, Phone: %s',[CallID,Phone]));
      Terminate;
      Break;
    end;
    wrTimeout:
    begin
      if Terminated then Break;
      if (FHangupTimeOut>0) and (GetTick64>FHangupTimeOut) then
      begin
        FHangupTimeOut:=0;
        FIsHangupTimeOut:=True;
        dmAlarm.SIPThread.AddCall(Self);
      end else
      if InHangup then
        dmAlarm.SIPThread.AddCall(Self);
    end;
    wrError:
    begin
      LogError(Format('Conversation event error: %d, CallID: %d, Phone: %s',[ConversationLock.LastError,CallID,Phone]));
      Terminate;
      Break;
    end;
  end;
end;

procedure TSIPCall.Call;
begin
  if CurrentScript<>nil then Stack.AddObject(IntToStr(CurrentScript.ScriptID),TObject(CurrentAction));
  AddReport('Извикване на сценарий '+IntToStr(ScriptID));
  Jump(ScriptID);
end;

function get_frame(port: ppjmedia_port;frame:ppjmedia_frame):integer;cdecl;
begin
  Result:=0;
end;

function put_frame(port: ppjmedia_port;frame:ppjmedia_frame):integer;cdecl;
var O:TObject;
begin
  Result:=0;
  if frame.frame_type<>1 then Exit;

  O:=TObject(port.port_data_p);
  if O is TSIPCall then
    TSIPCall(O).AddSamples(frame.buffer,frame.size);
end;

function on_destroy(port: ppjmedia_port):integer;cdecl;
begin
  Result:=0;
end;

procedure TSIPCall.AddSamples(Buffer: pointer; Size: Integer);
var I:Integer;
    C:Char;
    ID:Integer;
    Port:Integer;
begin
  ID:=CallID;
  if (ID>=0) and DTMFFrame then
  begin
    Exit;
  end;

  Size:=Size div 2; // Size is in bytes
  for I := 0 to Size - 1 do
  begin
    C:=DTMF_Decoder.AddSample(PSmallIntArray(Buffer)[I]);
    if C<>#0 then
    begin
      DetectedDigits:=RightStr(DetectedDigits+C,15);
      DTMFState:=dDetectedDigit;
      if (RightStr(DetectedDigits,Length(ExpectedDigits))=ExpectedDigits) then
      begin
        Port:=pjsua_call_get_conf_port(ID);
        if Port>0 then
          TSIPThread.CheckStatus(pjsua_conf_disconnect(Port,DTMF_Port.port_data_l),'pjsua_conf_disconnect');
        LogInfo(Format('[%10.10d] Successfully detected DTMF digits: %s',[ID,ExpectedDigits]));
        DTMFDetect;
        Signal;
      end else
        LogInfo(Format('[%10.10d] Last 15 detected DTMF digits: %s',[ID,DetectedDigits]));
    end;
  end;
end;

const PCM='pcm';

constructor TSIPCall.Create;
begin
  FID:=ID;
  AddLoggers(ChangeFileExt(ParamStr(0),'Channel.ini'),FID);

  FChannelLog:=DefaultHierarchy.GetLogger('Channel'+IntToStr(FID));
  FChannelLog.Info('#####################################################');
  FChannelLog.Info('Starting channel '+IntToStr(FID));
  FSIPApp:=App;
  DTMF_Port.port_data_l:=-1;
  ConversationLock:=TEvent.Create;
  Stop:=False;
  InHangup:=False;
  EnvLock:=TCriticalSection.Create;
  Stack:=TStringList.Create;
  FreeOnTerminate:=False;
  inherited Create(False);
  CallID:=-1;
  DTMFState:=dNone;
  Status('инициализиран');

  DTMFDetectStr:='dtmfdetect('+IntToStr(ID)+')';
  DTMF_Decoder:=TDTMF.Create(SIP_CLOCK_RATE);

  DTMF_Port.info.name.s:=@DTMFDetectStr[1];
  DTMF_Port.info.name.sz:=Length(DTMFDetectStr);
  DTMF_Port.info.signature:='dtmf';
  DTMF_Port.info.mediatype:=1;
  DTMF_Port.info.has_info:=1;
  DTMF_Port.info.need_info:=0;
  DTMF_Port.info.payload_type:=255;
  DTMF_Port.info.encoding_name.s:=@PCM[1];
  DTMF_Port.info.encoding_name.sz:=Length(PCM);
  DTMF_Port.info.clock_rate:=SIP_CLOCK_RATE;
  DTMF_Port.info.channel_count:=1;
  DTMF_Port.info.bits_per_sample:=16;
  DTMF_Port.info.samples_per_frame:=SIP_DTMF_FRAME;
  DTMF_Port.info.bytes_per_frame:=DTMF_Port.info.samples_per_frame*2;
  DTMF_Port.port_data_p:=self;
  DTMF_Port.get_frame:=get_frame;
  DTMF_Port.put_frame:=put_frame;
  DTMF_Port.on_destroy:=on_destroy;
end;

procedure TSIPCall.Delay;
begin
  if Milliseconds>0 then Sleep(Milliseconds);
end;

destructor TSIPCall.Destroy;
begin
  Status('унищожен');
  DTMF_Port.port_data_p:=nil;
  if DTMF_Port.port_data_l>=0 then
    TSIPThread.CheckStatus(pjmedia_port_destroy(@DTMF_Port),'pjmedia_port_destroy');
  //  TSIPThread.CheckStatus(pjsua_conf_remove_port(DTMF_Port.port_data_l),'pjsua_conf_remove_port');
  FreeAndNil(Stack);
  FreeAndNil(ConversationLock);
  FreeAndNil(DTMF_Decoder);

  FChannelLog.Info('Stopping channel '+IntToStr(FID));
  FChannelLog.Info('#####################################################');
  inherited;
  FreeAndNil(EnvLock);
end;

procedure TSIPCall.DoAction(Action:TSIPAction);
begin
  if Stop then
  begin
    Hangup;
    LastAction;
    Exit;
  end;
  if (Action=nil) or (InHangup) then
  begin
    Hangup;
    Exit;
  end;
  Status(Action.Action);
  //Log.Debug(Action.Action);
  case Action.OpCode of
    OP_RING:Ring(Action);
    OP_DELAY: Delay(StrToIntDef(Action.Operand[1],0));
    OP_PLAY: Play(Action.Operand[1],Action=FLastPlayAction,Action);
    OP_DTMF: DTMF;
    OP_JUMP: Jump(StrToIntDef(Action.Operand[1],0));
    OP_CALL: Call(StrToIntDef(Action.Operand[1],0));
    OP_HANGUP: Hangup;
    OP_SMS: SMS(Action);
    OP_Mail: Mail(Action);
  end;
  if not (Action.OpCode in [OP_CALL,OP_JUMP,OP_RING,OP_Play,OP_HANGUP,OP_DTMF]) then
    NextAction;
end;

procedure TSIPCall.DTMF;
begin
  if CallID<0 then
    NextAction
  else
    AddToSIPThread;
end;

procedure TSIPCall.DTMFDetect;
begin
  FHangupTimeOut:=0;
  FIsHangupTimeOut:=False;
  DTMFState:=dDetectedAll;
  AddReport('DTMF успешно разпознаване');
end;

function TSIPCall.DTMFFrame:boolean;
begin
  Inc(FDTMFFrameCount);
  Result:= FDTMFFrameCount>FDTMFWaitCount ;
end;

procedure TSIPCall.DTMFTimeout;
var Port:Integer;
begin
  FIsHangupTimeOut:=False;
  FHangupTimeOut:=0;
  if (CallID>=0) and (DTMF_Port.port_data_l>=0) then
  begin
    Port:=pjsua_call_get_conf_port(CallID);
    if Port>0 then
      TSIPThread.CheckStatus(pjsua_conf_disconnect(Port,DTMF_Port.port_data_l),'pjsua_conf_disconnect');
  end;
  LogInfo(Format('[%10.10d] DTMF timeout',[CallID]));
  AddReport('Таймаут при изчакване на DTMF');
  if DTMFFail then
    SignalFailure
  else
    Signal;
end;

procedure TSIPCall.RingTimeout;
begin
  FIsHangupTimeOut:=False;
  FHangupTimeOut:=0;
  LogInfo(Format('[%10.10d] Ring timeout',[CallID]));
  AddReport('Таймаут при позвъняване');
  TSIPThread.Hangup(Self);
end;

procedure TSIPCall.EventReport(Info: String);
begin
  if CurrentEvent<>nil then
    CurrentEvent.AddReport(FormatDateTime('yyyy-mm-dd hh:nn:ss ',Now)+Info+#13#10,'');
end;

procedure TSIPCall.Execute;
begin
  try
    while not Terminated do
    begin
      if ((CurrentScript=nil) and (CallID<0))
      or ((CurrentScript<>nil) and (CurrentScript.Count<=0)) then
        NextScript
      else
      if  (CurrentScript=nil) then
        DoAction(nil)
      else
      if CurrentAction<CurrentScript.Count then
        DoAction(CurrentScript.Action[CurrentAction])
    end;
    Status('Затворен');
  except
    on E:Exception do
    begin
      LogError(E.Message,E);
      Status(E.Message);
      StatusAddress(E.Message);
    end;
  end;
end;

procedure TSIPCall.Hangup;
begin
  if CallID<0 then
  begin
    if (dtmfState<>dRetry) and (CurrentMessage<>'') and (CurrentAddress<>nil) and (CurrentAddress.RingSequence<>'') then
    begin
      if CurrentAddress.RingSequence[1]='s' then
      begin
        Delete(CurrentAddress.RingSequence,1,1);
        Status('Генериране на SMS за '+CurrentAddress.MobilePhone);
        StatusAddress('генериране на SMS');
        AddReport('Генериране на SMS за '+CurrentAddress.MobilePhone);
        FSIPApp.SMSQueue.Add(CurrentMessageName,CurrentAddress.MobilePhone,CurrentMessage,CurrentAddress,QueueID);
      end
      else
      if CurrentAddress.RingSequence[1]='m' then
      begin
        Delete(CurrentAddress.RingSequence,1,1);
        Status('Генериране на EMail за '+CurrentAddress.EMail);
        StatusAddress('генериране на EMail');
        AddReport('Генериране на EMail за '+CurrentAddress.EMail);
        FSIPApp.MailQueue.Add(CurrentMessageName,CurrentAddress.EMail,CurrentMessage,CurrentEnv.SIPSettings.Values['SMTPSubject'],CurrentAddress,QueueID);
      end;
    end;
    NextAction;
  end
  else
    AddToSIPThread;
end;

procedure TSIPCall.InitDTMF;
begin
  if (CurrentScript<>nil) and CurrentScript.IsDTMF then
    DTMFState:=dNotDetected
  else
    DTMFState:=dHangedup;
end;

procedure TSIPCall.Jump(ScriptID: Integer);
begin
  if ScriptID<=0 then
    CurrentScript:=nil
  else
  begin
    CurrentScript:=CurrentEnv.SIPLibrary.Script[ScriptID];
    if (CurrentScript<>nil) and (CurrentScript.Count>0) then
    begin
      Status('Изпълнение на '+CurrentScript.Description);
      FLastScript:=ScriptID;
      FLastScriptDescription:=CurrentScript.Description;
      AddReport('Канал '+IntToStr(FID)+' - изпълнение на сценарий '+CurrentScript.Description);
    end else
      CurrentScript:=nil;
  end;
  CurrentAction:=0;
end;

procedure TSIPCall.LastAction;
begin
  CurrentAction:=0;
  CurrentScript:=nil;
end;

procedure TSIPCall.LogDebug(S: String);
begin
  S:=Format('{%2.2d} %s',[FID,S]);
  if Log<>nil then Log.Debug(S);
  FChannelLog.Debug(S);
end;

procedure TSIPCall.LogError(S: String;const Err: Exception);
begin
  S:=Format('{%2.2d} %s',[FID,S]);
  if Log<>nil then Log.Error(S,Err);
  FChannelLog.Error(S,Err);
end;

procedure TSIPCall.LogInfo(S: String);
begin
  S:=Format('{%2.2d} %s',[FID,S]);
  if Log<>nil then Log.Info(S);
  FChannelLog.Info(S);
end;

procedure TSIPCall.Mail(Action: TSIPAction);
var
  Announcement:TSIPAnnouncement;
  EMail:String;
begin
  Announcement:=CurrentEnv.SIPSound.Buffer[StrToIntDef(Action.Operand[1],0)];
  if (Announcement.Text<>'') and (CurrentAddress<>nil) and (CurrentAddress.TryCount=1) then
  begin
    EMail:='';
    if SameText(Action.Operand[2],'group') then
      EMail:=CurrentAddress.EMail
    else
    if SameText(Action.Operand[2],'person') then
    begin
      EMail:=CurrentEnv.SIPMail.Values[Action.Operand[3]];
    end;
    if EMail<>'' then
    begin
      Status('Генериране на EMail('+Announcement.Name+') за '+EMail);
      StatusAddress('генериране на EMail');
      AddReport('Генериране на EMail('+Announcement.Name+') за '+EMail);
      FSIPApp.MailQueue.Add(Announcement.Name,EMail,Announcement.Text,CurrentEnv.SIPSettings.Values['SMTPSubject'],CurrentAddress,QueueID);
    end;
  end;
end;

procedure TSIPCall.NextAction;
begin
  CurrentAction:=CurrentAction+1;
  if (CurrentScript<>nil) and (CurrentAction>=CurrentScript.Count) then
  begin
    if Stack.Count>0 then
    begin
      if FLastScript>0 then
        AddReport('Край на изпълнение на сценарий '+FLastScriptDescription);
      CurrentScript:=CurrentEnv.SIPLibrary.Script[StrToIntDef(Stack[Stack.Count-1],0)];
      if CurrentScript<>nil then
      begin
        FLastScript:=CurrentScript.ScriptID;
        FLastScriptDescription:=CurrentScript.Description;
      end
      else
      begin
        FLastScript:=0;
        FLastScriptDescription:='';
      end;
      CurrentAction:=Integer(Stack.Objects[Stack.Count-1]);
      Stack.Delete(Stack.Count-1);
      NextAction;
    end
    else
    begin
      CurrentAction:=0;
      CurrentScript:=nil
    end;
  end;
end;

procedure TSIPCall.NextScript;
var ScriptID:Integer;
    Event:TSIPEvent;
    StartID:Integer;
    MinDelay,MaxDelay:Integer;
begin
  Status('');

  if FLastScript>0 then
  begin
    AddReport('Край на изпълнение на сценарий '+FLastScriptDescription+#13#10);
    FLastScript:=0;
    FLastScriptDescription:='';
    if not (DTMFState in [dRetry,dNotExpected,dDetectedAll]) and (CurrentAddress<>nil) then
      FSIPApp.SIPQueueManager.Fail(CurrentAddress,QueueID,pkSIP);
  end;

  if (CurrentEvent<>nil) and (FReport<>'') then
    CurrentEvent.AddReport(FReport,'');

  CurrentEnv:=nil;

  FReport:='';
  EnvLock.Enter;
  try
    Stop:=False;
    InHangup:=False;
    CurrentEvent:=nil;
    CurrentStartID:=0;
  finally
    EnvLock.Leave;
  end;

  FSIPApp.SIPQueueManager.Next(ScriptID,CurrentAddress,QueueID,CurrentEnv,Event,StartID);

  if CurrentEnv<>nil then
  begin
    MinDelay:=StrToIntDef(CurrentEnv.SIPSettings.Values['ScriptMinDelay'],0);
    MaxDelay:=StrToIntDef(CurrentEnv.SIPSettings.Values['ScriptMaxDelay'],0);
    if (MinDelay>=0) then
    begin
      if MinDelay>10000 then MinDelay:=10000;
      if MaxDelay>10000 then MaxDelay:=10000;
      if MaxDelay<MinDelay then MaxDelay:=MinDelay;
      Sleep(MinDelay+Random(MaxDelay-MinDelay+1));
    end;
  end;

  EnvLock.Enter;
  try
    CurrentEvent:=Event;
    CurrentStartID:=StartID;
  finally
    EnvLock.Leave;
  end;

  if ScriptID<0 then
    Terminate
  else
  begin
    Jump(ScriptID);
    if CurrentAddress<>nil then
    begin
      AddReport(Format('Опит: %d, обект: %s',[CurrentAddress.TryCount,CurrentAddress.ObjectName]));
      //if CurrentScript<>nil then
      //  CurrentAddress.Data:=CurrentScript.Description
    end;
    DTMFState:=dNone;
    if CurrentScript<>nil then
      FLastPlayAction:=CurrentScript.LastPlayAction
    else
      FLastPlayAction:=nil
  end;
end;

procedure TSIPCall.Play;
var
  Announcement:TSIPAnnouncement;
begin
  if CallID<0 then
  begin
    Announcement:=CurrentEnv.SIPSound.Buffer[StrToIntDef(Action.Operand[1],0)];
    if (Announcement<>nil) and (Announcement.Text<>'') then
    begin
      CurrentMessage:=CurrentMessage+' '+Announcement.Text;
      CurrentMessageName:=CurrentMessageName+' '+Announcement.Name;
    end;
    NextAction;
  end
  else
  begin
    try
      AddToSIPThread;
    finally
      if (CallID>=0) and Last and (DTMFState=dHangedup) then
        DTMFState:=dNotExpected
    end;
  end;
end;

procedure TSIPCall.AddReport(Info: String);
begin
  if CurrentAddress<>nil then
    CurrentAddress.AddReport(StringOfChar(' ',Stack.Count*2)+Info)
  else
  begin
    if FReport<>'' then FReport:=FReport+#13#10;
    FReport:=FReport+FormatDateTime('yyyy-mm-dd hh:nn:ss ',Now)+StringOfChar(' ',Stack.Count*2)+Info;
  end;
end;

function TSIPCall.StatusKind(Status:Integer):TStatusKind;
begin
  case dtmfstate of
    dDetectedAll,dNotExpected: Result:=skSuccess;
    dDetectedDigit: Result:= skFailDTMF;
    dHangedup : Result:= skFailAnnounce;
    dNotDetected: Result:=skNoDTMF;
    else
      if InArray(Status,SIPBUSY) then
        Result:=skBusy
      else if InArray(Status,SIPBUSYROUTE) then
        Result:=skBusyRoute
      else if InArray(Status,SIPNOANSWER) then
        Result:=skNoAnswer
      else if InArray(Status,SIPNOTFOUND) then
        Result:=skNotFound
      else
        Result:=skError;
  end;
end;

procedure TSIPCall.StopCurrent;
begin
  EnvLock.Enter;
  try
    Stop:=True;
    dmAlarm.SIPThread.AddCall(Self);
  finally
    EnvLock.Leave;
  end;
end;

procedure TSIPCall.StopEvent(EventID: Integer);
begin
  EnvLock.Enter;
  try
    if (CurrentStartID=EventID) then
      Stop:=True;
    dmAlarm.SIPThread.AddCall(Self);
  finally
    EnvLock.Leave;
  end;
end;

procedure TSIPCall.Received(Buffer: Pointer; Size: Integer);
var Old:Integer;
begin
  Old:=ReceiveCnt div 10000;
  ReceiveCnt:=ReceiveCnt+Size;
  if Old<>(ReceiveCnt div 10000) then
     LogDebug('Bytes received: '+IntToStr(ReceiveCnt));
end;

procedure TSIPCall.Sent(Buffer: Pointer; Size: Integer);
var Old:Integer;
begin
  Old:=SendCnt div 10000;
  SendCnt:=SendCnt+Size;
  if Old<>(SendCnt div 10000) then
     LogDebug('Bytes sent: '+IntToStr(SendCnt));
end;

procedure TSIPCall.Retry;
var RetrySequence:Boolean;
  I:Integer;
  SK:TStatusKind;
begin
  if (QueueID>0) and (CurrentAddress<>nil) then
  begin
    RetrySequence:=False;
    SK:=StatusKind(Status);
    case SK of
      skBusy:begin
        CurrentAddress.Status:='заето';

        if Description<>'' then
          CurrentAddress.Status:=CurrentAddress.Status+' ('+Description+')';
        AddReport(CurrentAddress.Status);

        if CurrentAddress.RetryBusy>0 then
        begin
          DTMFState:=dRetry;
          CurrentAddress.RetryBusy:=CurrentAddress.RetryBusy-1;
          FSIPApp.SIPQueueManager.Retry(CurrentAddress,CurrentAddress.DelayBusy,QueueID);
        end else
          RetrySequence:=True;
      end;
      skBusyRoute:begin
        CurrentAddress.Status:='претоварване';

        if Description<>'' then
          CurrentAddress.Status:=CurrentAddress.Status+' ('+Description+')';
        AddReport(CurrentAddress.Status);

        if CurrentAddress.RetryBusyRoute>0 then
        begin
          DTMFState:=dRetry;
          CurrentAddress.RetryBusyRoute:=CurrentAddress.RetryBusyRoute-1;
          FSIPApp.SIPQueueManager.Retry(CurrentAddress,CurrentAddress.DelayBusyRoute,QueueID);
        end else
          RetrySequence:=True;
      end;
      skNoAnswer:
      begin
        CurrentAddress.Status:='неотговорил';

        if Description<>'' then
          CurrentAddress.Status:=CurrentAddress.Status+' ('+Description+')';
        AddReport(CurrentAddress.Status);

        RetrySequence:=True;
      end;
      skNoDTMF:begin
        CurrentAddress.Status:='липсва DTMF';

        if Description<>'' then
          CurrentAddress.Status:=CurrentAddress.Status+' ('+Description+')';
        AddReport(CurrentAddress.Status);

        RetrySequence:=True;
      end;
      skNotFound:
      begin
        CurrentAddress.Status:='грешен номер';

        if Description<>'' then
          CurrentAddress.Status:=CurrentAddress.Status+' ('+Description+')';
        AddReport(CurrentAddress.Status);

        for I := Length(CurrentAddress.RingSequence) downto 2 do
          if CurrentAddress.RingSequence[I]=CurrentAddress.RingSequence[1] then
            Delete(CurrentAddress.RingSequence,I,1);
        RetrySequence:=True;
      end;
      skSuccess:
      begin
        CurrentAddress.Status:=CurrentAddress.Method;

        if Description<>'' then
          CurrentAddress.Status:=CurrentAddress.Status+' ('+Description+')';

        AddReport('Успешно оповестяване, метод '+CurrentAddress.Status);
        EventReport('Успешно оповестяване на '+CurrentAddress.ObjectName+
        ' по '+CurrentAddress.Status+
        '(опит '+IntToStR(CurrentAddress.TryCount)+
        ') по сценарий '+FLastScriptDescription);

        FSIPApp.SIPQueueManager.Succeed(CurrentAddress,QueueID,pkSIP);
        //CurrentAddress:=nil;
      end;
      skFailDTMF:
      begin
        CurrentAddress.Status:='грешен DTMF';

        if Description<>'' then
          CurrentAddress.Status:=CurrentAddress.Status+' ('+Description+')';
        AddReport(CurrentAddress.Status);

        if CurrentAddress.RetryFailDTMF>0 then
        begin
          DTMFState:=dRetry;
          CurrentAddress.RetryFailDTMF:=CurrentAddress.RetryFailDTMF-1;
          FSIPApp.SIPQueueManager.Retry(CurrentAddress,CurrentAddress.DelayFailDTMF,QueueID);
        end else
          RetrySequence:=True;
      end;
      skFailAnnounce:
      begin
        CurrentAddress.Status:='прекъснал';

        if Description<>'' then
          CurrentAddress.Status:=CurrentAddress.Status+' ('+Description+')';
        AddReport(CurrentAddress.Status);

        if CurrentAddress.RetryFailDTMF>0 then
        begin
          DTMFState:=dRetry;
          CurrentAddress.RetryFailDTMF:=CurrentAddress.RetryFailDTMF-1;
          FSIPApp.SIPQueueManager.Retry(CurrentAddress,CurrentAddress.DelayFailDTMF,QueueID);
        end else
          RetrySequence:=True;
      end
      else
      begin
        CurrentAddress.Status:='грешка SIP '+IntToStr(Status);

        if Description<>'' then
          CurrentAddress.Status:=CurrentAddress.Status+' ('+Description+')';
        AddReport(CurrentAddress.Status);

        RetrySequence:=True;
      end;
    end;
    if RetrySequence then
    begin
      Delete(CurrentAddress.RingSequence,1,1);
      if CurrentAddress.RingSequence<>'' then
      begin
        DTMFState:=dRetry;
        FSIPApp.SIPQueueManager.Retry(CurrentAddress,1,QueueID);
      end
      else
      begin
        AddReport('Неуспешно оповестяване');
        EventReport('Неуспешно оповестяване на '+CurrentAddress.ObjectName+
        ' по сценарий '+FLastScriptDescription);
        FSIPApp.SIPQueueManager.Fail(CurrentAddress,QueueID,pkSIP);
      end;
    end;
  end;
end;

procedure TSIPCall.Ring;
var APhone,APhoneKind:String;
begin
  CurrentMessage:='';
  CurrentMessageName:='';
  APhone:='';
  APhoneKind:='тел. номер';
  if SameText(Action.Operand[1],'group') then
  begin
    if (CurrentAddress.TryCount=1) then
    begin
      CurrentAddress.RingSequence:=Action.operand[3];
      if CurrentAddress.RingSequence='' then
        CurrentAddress.RingSequence:='1';
      CurrentAddress.RetryBusy:=StrToIntDef(Action.operand[4],DefRetry);
      CurrentAddress.DelayBusy:=StrToIntDef(Action.operand[5],DefDelay);
      CurrentAddress.RetryBusyRoute:=StrToIntDef(Action.operand[6],DefRetry);
      CurrentAddress.DelayBusyRoute:=StrToIntDef(Action.operand[7],DefDelay);
      CurrentAddress.RetryFailDTMF:=StrToIntDef(Action.operand[8],DefRetry);
      CurrentAddress.DelayFailDTMF:=StrToIntDef(Action.operand[9],DefDelay);
    end;

    APhoneKind:=CurrentAddress.Method;
    if CurrentAddress.RingSequence<>'' then
    case CurrentAddress.RingSequence[1] of
      '1':begin
            APhone:=CurrentAddress.Phone;
          end;
      '2':begin
            APhone:=CurrentAddress.HomePhone;
          end;
      '3':begin
            APhone:=CurrentAddress.MobilePhone;
          end;
      's':begin
            NextAction;
            Exit;
          end;
      'm':begin
            NextAction;
            Exit;
          end;
    end;
  end
  else
  if SameText(Action.Operand[1],'person') then
  begin
    APhone:=CurrentEnv.SIPAddress.Values[Action.Operand[2]];
  end;
  if APhone='' then
  begin
    if CurrentAddress.Status='' then
       CurrentAddress.Status:='липсва '+APhoneKind;
    AddReport(CurrentAddress.Status);
    Retry(404,CurrentAddress.Status);
    LastAction;
    Exit;
  end;
  Phone:='sip:'+APhone;
  PhoneKind:=APhoneKind;
  if Pos('@',Phone)<=0  then Phone:=Phone+'@'+CurrentEnv.SIPSettings.Values['SIPHost'];
  AddToSIPThread;
end;

procedure TSIPCall.SMS;
var
  Announcement:TSIPAnnouncement;
  APhone:String;
begin
  Announcement:=CurrentEnv.SIPSound.Buffer[StrToIntDef(Action.Operand[1],0)];
  if (Announcement.Text<>'') and (CurrentAddress<>nil) and (CurrentAddress.TryCount=1) then
  begin
    APhone:='';
    if SameText(Action.Operand[2],'group') then
      APhone:=CurrentAddress.MobilePhone
    else
    if SameText(Action.Operand[2],'person') then
    begin
      Aphone:=CurrentEnv.SIPMobile.Values[Action.Operand[3]];
    end;
    if APhone<>'' then
    begin
      Status('Генериране на SMS('+Announcement.Name+') за '+APhone);
      StatusAddress('генериране на SMS');
      AddReport('Генериране на SMS('+Announcement.Name+') за '+APhone);
      FSIPApp.SMSQueue.Add(Announcement.Name,APhone,Announcement.Text,CurrentAddress,QueueID);
    end;
  end;
end;

procedure TSIPCall.Signal;
begin
  NextAction;
  ConversationLock.SetEvent;
end;

procedure TSIPCall.SignalFailure;
begin
  LastAction;
  ConversationLock.SetEvent;
end;

procedure TSIPCall.StartSIP;
var
  A:TSIPAction;
  Announcement:TSIPAnnouncement;
begin
  if ConversationLock.WaitFor(0)=wrSignaled then Exit;
  try
    if Stop then
    begin
      LastAction;
      if CallID>=0 then
        dmAlarm.SIPThread.Hangup(Self)
      else
        ConversationLock.SetEvent;
    end;
    if (CurrentScript=nil) and (CallID>=0) then
    begin
       AddReport('Прекъсване на връзката с '+FLastPhone);
       dmAlarm.SIPThread.Hangup(Self);
    end;
    if InHangup then
    begin
      AddReport(FLastPhone+' прекъсна връзката');
      dmAlarm.SIPThread.Hangup(Self);
    end;
    if (CurrentScript<>nil) and (CurrentAction>=0) and (CurrentAction<CurrentScript.Count) then
    begin
      A:=CurrentScript.Action[CurrentAction];
      if A<>nil then
      case A.OpCode of
        OP_RING:
        begin
          if FIsHangupTimeOut then
          begin
            RingTimeOut;
          end else
          if CallID>=0 then
          begin
            CurrentAction:=Currentaction-1;
            Status('прекъсване на връзката с '+FLastPhone);
            StatusAddress('прекъсване на връзката');
            AddReport('Прекъсване на връзката с '+FLastPhone);
            dmAlarm.SIPThread.Hangup(Self);
          end else
          begin
            FLastPhone:=Phone;
            Status('позвъняване на '+Phone);
            StatusAddress('позвъняване на '+PhoneKind);
            AddReport('Позвъняване на '+PhoneKind+' '+Phone);
            FHangupTimeOut:=GetTick64+StrToIntDef(CurrentEnv.SIPSettings.Values['RingTimeOut'],60)*1000;
            dmAlarm.SIPThread.MakeCall(Self);
          end;
        end;
        OP_PLAY:
        begin
          Status('предаване на съобщение '+A.Operand[1]+' на '+Phone);
          StatusAddress('предаване на съобщение');
          AddReport('Предаване на съобщение '+A.Operand[1]+' на '+Phone);
          Announcement:=CurrentEnv.SIPSound.Buffer[StrToIntDef(A.Operand[1],0)];
          dmAlarm.SIPThread.PlaySound(Self,Announcement)
        end;
        OP_HANGUP:
        begin
          Status('прекъсване на връзката с '+FLastPhone);
          StatusAddress('прекъсване на връзката');
          AddReport('Прекъсване на връзката с '+FLastPhone);
          dmAlarm.SIPThread.Hangup(Self);
        end;
        OP_DTMF:
        if FIsHangupTimeOut then
        begin
          DTMFTimeOut;
        end else
        begin
          FDTMFFrameCount:=0;
          DTMFFail:=StrToIntDef(A.Operand[2],0)<>0;
          FDTMFWaitCount:=StrToIntDef(A.Operand[1],0)*SIP_DTMF_RATE;
          Status('изчакване на DTMF от '+Phone);
          StatusAddress('изчакване на DTMF');
          AddReport('Изчакване на DTMF от '+Phone);
          ExpectedDigits:=CurrentAddress.DTMF+'#';
          DetectedDigits:='';
          FHangupTimeOut:=GetTick64+StrToIntDef(A.Operand[1],0)*1000;
          dmAlarm.SIPThread.WaitDTMF(Self);
        end;
      end;
    end;
  except
    on E:Exception do
    begin
      LastAction;
      ConversationLock.SetEvent;
    end;
  end;
end;

procedure TSIPCall.Status(const S: String);
begin
  if (CurrentScript<>nil) and (CurrentScript.Count>0) and (CurrentEvent<>nil) then
  begin
    if FSIPApp.SIPStatus<>nil then FSIPApp.SIPStatus.Status(FID,CurrentEvent.Name,CurrentScript.Description,IntToStr(CurrentAction+1),S);
  end else
  begin
    if FSIPApp.SIPStatus<>nil then FSIPApp.SIPStatus.Status(FID,'','','',S);
  end;
end;

procedure TSIPCall.StatusAddress(const S: String);
begin
  if CurrentAddress<>nil then CurrentAddress.Status:=S;
end;

end.
