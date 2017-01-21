unit SIP_Thread;

interface
uses pjsua,logger,SysUtils,SyncObjs,Classes,SIP_Call, SIP_Sound;

type
  TAccountInfo=record
    id,uri,host,scheme,account:String;
  end;
  TSIPThread=class(TThread)
  private
    config:pjconfig;
    logconfig:pjlogconfig;
    transport:pjtransport;
    media:pjmediaconfig;
    mediatransport:pjtransport;
    TransportID:Integer;
    pjpcmu:pjstring;
    CallList:TThreadList;
    CallLock:TEvent;
    FAccountStart:Integer;
    FHost:String;
    AccountInfo:array of TAccountInfo;
    Accounts:array of Integer;
    FPacketTime:Integer;
    FCodec:string;
    procedure InitPJSUA;
    procedure DestroyPJSUA;
    function CheckCalls:Boolean;
  protected
    procedure Execute;override;
  public
    SIPPool:pointer;
    CallDelay:Integer;
    constructor Create(AccountStart,AccountCount,PacketTime:Integer;Host,Codec:String);
    destructor Destroy;override;
    class procedure CheckStatus(Status: Integer; F: String;Channel:TSIPCall=nil);
    procedure Terminate;
    procedure AddCall(Call:TSIPCall);
    procedure MakeCall(Call:TSIPCall);
    class procedure Hangup(Call:TSIPCall);
    procedure WaitDTMF(Call:TSIPCall);
    class procedure PlayWave(Call:TSIPCall;FileName:String);
    procedure PlaySound(Call:TSIPCall;Sound:TSIPAnnouncement);
    procedure AddPort(Port:ppjmedia_port;Channel:TSIPCall);
  end;

  ESIPException=class(Exception);

const
  SIP_CLOCK_RATE=8000;
  SIP_DTMF_RATE=10;
  SIP_DTMF_FRAME=SIP_CLOCK_RATE div SIP_DTMF_RATE;


implementation
uses StrUtils, SIP_Monitor;

procedure TSIPThread.AddCall(Call:TSIPCall);
begin
  CallList.Add(Call);
  CallLock.SetEvent;
end;

procedure TSIPThread.AddPort(Port: ppjmedia_port;channel:TSIPCall);
begin
  CheckStatus(pjsua_conf_add_port(SIPpool,port,port.port_data_l),'pjsua_conf_add_port',Channel);
end;

function TSIPThread.CheckCalls;
var L:TList;
    C:TSIPCall;
begin
  L:=CallList.LockList;
  try
    if L.Count=0 then
    begin
      CallLock.ResetEvent;
      Result:=False;
    end
    else
    begin
      C:=L[0];
      L.Delete(0);
      C.StartSIP;
      Result:=True;
    end;
  finally
    CallList.UnlockList;
  end;
  if not Result then CallLock.WaitFor(5000);
end;

class procedure TSIPThread.CheckStatus(Status:Integer;F:String;Channel:TSIPCall);
begin
//  debug tests
//  if SameText(F,'pjsua_call_get_info') then Status:=-1;
//  if SameText(F,'pjsua_create') then Status:=-1;
//  if SameText(F,'pjsua_call_make_call') then Status:=-1;
//  if SameText(F,'pjmedia_mem_player_create') then Status:=-1;
  if Channel<>nil then
  begin
    if Status=0 then
      Channel.LogDebug(F+' : 0')
    else
    begin
      Channel.LogError(F+' : '+IntToStr(Status));
      raise ESIPException.Create(F+' : '+IntToStr(Status));
    end;
  end else
  begin
    if Status=0 then
    begin
      if Log<>nil then Log.Debug(F+' : 0')
    end
    else
    begin
      if Log<>nil then Log.Error(F+' : '+IntToStr(Status));
      raise ESIPException.Create(F+' : '+IntToStr(Status));
    end;
  end;
end;

constructor TSIPThread.Create;
begin
  inherited Create(False);
  CallList:=TThreadList.Create;
  CallList.Duplicates:=dupIgnore;
  CallLock:=TEvent.Create();
  CallDelay:=200;
  SetLength(Accounts,AccountCount);
  SetLength(AccountInfo,AccountCount);
  FAccountStart:=AccountStart;
  FHost:=Host;
  FPacketTime:=PacketTime;
  FCodec:=Codec;
  if FCodec='' then
    FCodec:='PCMA';
//(info:(name:(s:'memplayer'; sz:9); signature:1297116281; mediatype:1;
// has_info:1; need_info:0; payload_type:255; encoding_name:(s:'pcm'; sz:3);
// clock_rate:8000; channel_count:1; bits_per_sample:16; samples_per_frame:80;
// bytes_per_fram:160); port_data_p:nil; podr_data_l:0)
end;

destructor TSIPThread.Destroy;
begin
  inherited;
  FreeAndNil(CallLock);
  FreeAndNil(CallList);
end;

procedure TSIPThread.DestroyPJSUA;
begin
  try
    pjsua_call_hangup_all;
    if SIPPool<>nil then pjsua_pool_release(SIPPool);
    pjsua_set_no_snd_dev;
  finally
    pjsua_destroy;
  end;
end;

procedure TSIPThread.Execute;
begin
  try
    InitPJSUA;
  except
    on E:Exception do
    begin
      try
        if not (E is ESIPException) then
        if (Log<>nil) then Log.Error(E.Message,E);
      finally
        Terminate;
      end;
    end;
  end;
  try
  try
    while not Terminated do
    begin
      CheckCalls;
    end;
  except
    on E:Exception do if Log<>nil then Log.Error(E.Message,E);
  end;
  finally
    DestroyPJSUA;
  end;
end;


class procedure TSIPThread.Hangup(Call: TSIPCall);
var X:Integer;
begin
  X:=Call.CallID;
  if X<0 then Exit;
  Call.LogInfo(Format('[%10.10d] Ending Call to %s',[X,Call.Phone]));
  Call.InHangup:=False;
  TSIPThread.CheckStatus(pjsua_call_hangup(X,0,nil,nil),'pjsua_call_hangup',Call);
end;

//info.state
//PJSIP_INV_STATE_NULL 	        Before INVITE is sent or received
//PJSIP_INV_STATE_CALLING 	After INVITE is sent
//PJSIP_INV_STATE_INCOMING 	After INVITE is received.
//PJSIP_INV_STATE_EARLY 	After response with To tag.
//PJSIP_INV_STATE_CONNECTING 	After 2xx is sent/received.
//PJSIP_INV_STATE_CONFIRMED 	After ACK is sent/received.
//PJSIP_INV_STATE_DISCONNECTED 	Session is terminated.
//http://www.pjsip.org/pjsip/docs/html/structpjsip__event.htm

procedure call_state(call_id:integer;event:pointer);cdecl;
var info:pjcallinfo;
    data:TObject;
    S:String;
begin
  data:=pjsua_call_get_user_data(call_id);
  if data is TSIPCall then
  begin
    try
      TSIPThread.CheckStatus(pjsua_call_get_info(call_id,info),'pjsua_call_get_info',TSIPCall(data));

      if info.state=6 then
        TSIPCall(data).CallID:=-1;

      if info.last_status_text.sz>0 then
      begin
        SetLength(S,info.last_status_text.sz);
        Move(info.last_status_text.s^,S[1],info.last_status_text.sz);
      end
      else
        S:='';
        
      TSIPCall(data).LogDebug(Format('CallID:%d, State:%d, SIP: %d %s',[call_id,info.state,info.last_status,s]));
      case info.state of
        5:begin
            SIPMonitor.Status_SIP:=GetTick64;
            TSIPCall(data).InitDTMF;
            TSIPCall(data).Signal;
          end;
        6:begin
            TSIPCall(data).CallID:=-1;
            TSIPCall(data).Retry(info.last_status);
            TSIPCall(data).Signal;
          end;
      end;
    except
      on E:Exception do
      begin
        try
          if not (E is ESIPException) then TSIPCall(data).LogError(E.Message,E);
        finally
          TSIPCall(data).SignalFailure;
        end;
      end;
    end;
  end;
end;


//info.media_state
//PJSUA_CALL_MEDIA_NONE 	Call currently has no media
//PJSUA_CALL_MEDIA_ACTIVE 	The media is active
//PJSUA_CALL_MEDIA_LOCAL_HOLD 	The media is currently put on hold by local endpoint
//PJSUA_CALL_MEDIA_REMOTE_HOLD 	The media is currently put on hold by remote endpoint
//PJSUA_CALL_MEDIA_ERROR 	The media has reported error (e.g. ICE negotiation)

procedure media_state(call_id:integer);cdecl;
var info:pjcallinfo;
    data:TObject;
begin
  data:=pjsua_call_get_user_data(call_id);
  if data is TSIPCall then
  begin
    try
      TSIPThread.CheckStatus(pjsua_call_get_info(call_id,info),'pjsua_call_get_info',TSIPCall(data));
      case info.media_status of
        //1:TSIPCall(data).Signal;
        2,3,4:begin
          TSIPCall(data).InHangup:=True;
        end;
        //TSIPThread.CheckStatus(pjsua_call_hangup(call_id,0,nil,nil),'pjsua_call_hangup',TSIPCall(data));
      end;
    except
      on E:Exception do
      begin
        try
          if not (E is ESIPException) then TSIPCall(data).LogError(E.Message,E);
        finally
          TSIPCall(data).SignalFailure;
        end;
      end;
    end;
  end;
end;

procedure rx(call_id:integer;packet:pointer;bytesread:integer;stream:pointer);cdecl;
var
  data:TObject;
begin
  data:=pjsua_call_get_user_data(call_id);

  if data is TSIPCall then
  begin
    try
      if bytesread<0 then
      begin
        TSIPCall(Data).LogError('CallID:'+IntToStr(call_id)+',rx error:'+IntToStr(-bytesread));
        TSIPCall(Data).StopCurrent;
      end else
        TSIPCall(data).Received(packet,bytesread);
    except
      on E:Exception do
      begin
        try
          if not (E is ESIPException) then TSIPCall(data).LogError(E.Message,E);
        finally
          TSIPCall(data).SignalFailure;
        end;
      end;
    end;
  end else
  begin
    if bytesread<0 then
    begin
      if Log<>nil then Log.Error('CallID:'+IntToStr(call_id)+',rx error:'+IntToStr(-bytesread));
      TSIPThread.CheckStatus(pjsua_call_hangup(call_id,0,nil,nil),'pjsua_call_hangup');
    end;
  end;
end;

procedure tx(call_id:integer;packet:pointer;bytesread:integer;stream:pointer);cdecl;
var
  data:TObject;
begin
  data:=pjsua_call_get_user_data(call_id);
  if data is TSIPCall then
  begin
    try
      TSIPCall(data).Sent(packet,bytesread);
    except
      on E:Exception do
      begin
        try
          if not (E is ESIPException) then TSIPCall(data).LogError(E.Message,E);
        finally
          TSIPCall(data).SignalFailure;
        end;
      end;
    end;
  end;
end;

procedure pjlog(level:integer;msg:pointer;msg_len:integer);cdecl;
var S:String;
begin
  if msg_len>0 then
  begin
    SetLength(S,msg_len);
    Move(msg^,s[1],msg_len);
    if Log<>nil then Log.Debug(trim(s));
  end;
end;

procedure TSIPThread.InitPJSUA;
var I:Integer;
  AccountID:Integer;
  AccCfg:pjsua_acc_config;
begin
  CheckStatus(pjsua_create,'pjsua_create');
  SIPpool:=pjsua_pool_create('SIPThread',1024,1024);
  if SIPpool=nil then
    TSIPThread.CheckStatus(-1,'pjsua_pool_create returned nil');
  pjsua_config_default(config);
  config.max_calls:=64;
  pjsua_logging_config_default(logconfig);
  logconfig.log_sip_msg:=0;
  logconfig.callback:=@pjlog;
  config.callback.call_media_state:=@media_state;
  config.callback.call_state:=@call_state;
  config.callback.rx:=@rx;
  config.callback.tx:=@tx;
  pjsua_media_config_default(media);
  media.clock_rate:=SIP_CLOCK_RATE;
  media.audio_frame_ptime:=FPacketTime;
  media.ptime:=FPacketTime;
  media.max_media_ports:=1000;
  media.no_vad:=1;

  CheckStatus(pjsua_init(@config,@logconfig,@media),'pjsua_init');
  pjsua_transport_config_default(transport);
  transport.port:=5000;
  CheckStatus(pjsua_transport_create(1,transport,TransportID),'pjsua_transport_create');
  CheckStatus(pjsua_acc_add_local(TransportID,1,AccountID),'pjsua_acc_add_local');
  for I := 0 to Length(Accounts)-1 do
    Accounts[I]:=AccountID;
  if FAccountStart>0 then
  for I := 0 to Length(Accounts) - 1 do
  begin
    AccountInfo[I].id:='sip:'+IntToStr(FAccountStart+I)+'@'+FHost;
    AccountInfo[I].uri:='sip:'+FHost;
    AccountInfo[I].host:=FHost;
    AccountInfo[I].scheme:='digest';
    AccountInfo[I].account:=IntToStr(FAccountStart+I);
    pjsua_acc_config_default(AccCfg);
    AccCfg.id.s:=@AccountInfo[I].id[1];
    AccCfg.id.sz:=Length(AccountInfo[I].id);
    AccCfg.reg_uri.s:=@AccountInfo[I].uri[1];
    AccCfg.reg_uri.sz:=Length(AccountInfo[I].uri);
    AccCfg.cred_count:=1;
    AccCfg.cred_info[1].realm.s:=@AccountInfo[I].host[1];
    AccCfg.cred_info[1].realm.sz:=Length(AccountInfo[I].host);
    AccCfg.cred_info[1].scheme.s:=@AccountInfo[I].scheme[1];
    AccCfg.cred_info[1].scheme.sz:=Length(AccountInfo[I].scheme);
    AccCfg.cred_info[1].username.s:=@AccountInfo[I].account[1];
    AccCfg.cred_info[1].username.sz:=Length(AccountInfo[I].account);
    AccCfg.cred_info[1].data_type:=0;
    AccCfg.cred_info[1].data.s:=@AccountInfo[I].account[1];
    AccCfg.cred_info[1].data.sz:=Length(AccountInfo[I].account);
    CheckStatus(pjsua_acc_add(AccCfg,0,Accounts[I]),'pjsua_acc_add');
  end;
  pjsua_transport_config_default(mediatransport);
  mediatransport.port:=6000;
  CheckStatus(pjsua_media_transports_create(mediatransport),'pjsua_media_transports_create');
  pjpcmu.s:=@fcodec[1];
  pjpcmu.sz:=length(fcodec);
  CheckStatus(pjsua_codec_set_priority(pjpcmu,255),'pjsua_codec_set_priority');
  CheckStatus(pjsua_set_ec(0,0),'pjsua_set_ec');
  CheckStatus(pjsua_set_null_snd_dev,'pjsua_set_null_snd_dev');
  //Test;
  CheckStatus(pjsua_start,'pjsua_start');
end;

procedure TSIPThread.MakeCall(Call: TSIPCall);
var Phone:pjstring;
begin
  Call.LogInfo(Format('Calling %s',[Call.Phone]));
  Phone.s:=@Call.Phone[1];
  Phone.sz:=Length(Call.Phone);
  CheckStatus(pjsua_call_make_call(Accounts[Call.ChannelIndex-1],Phone,0,Call,nil,Call.CallID),'pjsua_call_make_call',Call);
  Sleep(CallDelay);
end;

type
  TSoundData=class(TObject)
    Sound:TSIPAnnouncement;
    PortID:Integer;
    Call:TSIPCall;
  end;

function endsound(port:pointer;userdata:TSoundData):integer;cdecl;
begin
  result:=-1;
  try
  try
    if userdata<>nil then
    begin
      TSIPThread.CheckStatus(pjsua_conf_remove_port(userdata.PortID),'pjsua_conf_remove_port',userdata.Call);
      TSIPThread.CheckStatus(pjmedia_port_destroy(port),'pjmedia_port_destroy',userdata.Call);
    end else
      TSIPThread.CheckStatus(pjmedia_port_destroy(port),'pjmedia_port_destroy');
    if userdata<>nil then userdata.Call.Signal;
  except
    on E:Exception do
    begin
      try
        if not (E is ESIPException) then if Log<>nil then Log.Error(E.Message,E);
      finally
        if (userdata<>nil) and (userdata.Call<>nil) then userdata.Call.SignalFailure;
      end;
    end;
  end;
  finally
    userdata.free;
  end;
end;

procedure TSIPThread.PlaySound(Call: TSIPCall; Sound: TSIPAnnouncement);
var port:pointer;
    portid:integer;
    Data:TSoundData;
    CallPort:Integer;
begin
  if Call.CallID<0 then
  begin
    Call.LogError(Format('PlaySound:call to %s is terminated',[Call.Phone]));
    Call.Signal;
    Exit;
  end;
  if Sound=nil then
  begin
    Call.LogError(Format('[%10.10d] Skipping unknown message to %s',[Call.CallID,Call.Phone]));
    Call.Signal;
    Exit;
  end;
  if Sound.Size<=0 then
  begin
    Call.LogInfo(Format('[%10.10d] Skipping empty message %s to %s',[Call.CallID,Sound.Name,Call.Phone]));
    Call.Signal;
    Exit;
  end;
  CallPort:=pjsua_call_get_conf_port(Call.CallID);
  if CallPort>0 then
  begin
    Call.LogInfo(Format('[%10.10d] Playing %s to %s',[Call.CallID,Sound.Name,Call.Phone]));
    TSIPThread.CheckStatus(pjmedia_mem_player_create(SIPpool,Sound.Buffer,Sound.Size,8000,1,80,16,0,port),'pjmedia_mem_player_create',Call);
    TSIPThread.CheckStatus(pjsua_conf_add_port(SIPpool,port,portid),'pjsua_conf_add_port',Call);
    Data:=TSoundData.Create;
    Data.Sound:=Sound;
    Data.PortID:=PortID;
    Data.Call:=Call;
    TSIPThread.CheckStatus(pjmedia_mem_player_set_eof_cb(port,data,@endsound),'pjmedia_mem_player_set_eof_cb',Call);
    TSIPThread.CheckStatus(pjsua_conf_connect(portid,CallPort),'pjsua_conf_connect',Call);
  end else
  begin
    Call.LogInfo(Format('[%10.10d] Missing port for call %d',[Call.CallID,Call.CallID]));
    Call.Signal;
    Exit;
  end;
end;

class procedure TSIPThread.PlayWave(Call: TSIPCall; FileName: String);
var
    pjwav:pjstring;
    PlayerID,PortID:Integer;
begin
  Call.LogInfo(Format('[%10.10d] Playing %s to %s',[Call.CallID,FileName,Call.Phone]));
  pjwav.s:=@FileName[1];
  pjwav.sz:=length(FileName);
  TSIPThread.CheckStatus(pjsua_player_create(pjwav,0,PlayerID),'pjsua_player_create',Call);
  PortID:=pjsua_player_get_conf_port(PlayerID);
  TSIPThread.CheckStatus(pjsua_conf_connect(PortID,pjsua_call_get_conf_port(Call.CallID)),'pjsua_conf_connect',Call);
end;

procedure TSIPThread.Terminate;
begin
  CallLock.SetEvent;
  inherited;
end;

procedure TSIPThread.WaitDTMF(Call:TSIPCall);
var CallPort:Integer;
begin
  if Call.CallID<0 then
  begin
    Call.LogError(Format('WaitDTMF:call to %s is terminated',[Call.Phone]));
    Call.Signal;
    Exit;
  end;

  if Call.DTMF_Port.port_data_l<0 then
    AddPort(@Call.DTMF_Port,Call);

  CallPort:=pjsua_call_get_conf_port(Call.CallID);
  if CallPort>0 then
  begin
    Call.LogInfo(Format('[%10.10d] Waiting DTMF from %s',[Call.CallID,Call.Phone]));
    TSIPThread.CheckStatus(pjsua_conf_connect(CallPort,Call.DTMF_Port.port_data_l),'pjsua_conf_connect',Call)
  end
  else
  begin
    Call.LogInfo(Format('[%10.10d] Missing port for call %d',[Call.CallID,Call.CallID]));
    Call.DTMFTimeout;
  end;
end;

end.
