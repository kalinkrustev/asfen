program sip;

{$APPTYPE CONSOLE}

uses
  SysUtils;

type pjstring=packed record
       s:pchar;
       sz:integer;
     end;
     ppjstring=^pjstring;

     pjtime=packed record
       sec:integer;
       msec:integer;
     end;

     pjtls=packed record
       ca_list_file:pjstring;
       cert_file:pjstring;
       privkey_file:pjstring;
       password:pjstring;
       method:integer;
       ciphers:pjstring;
       verify_server:integer;
       verify_client:integer;
       require_client_cert:integer;
       timeout:pjtime;
     end;
     pjtransport=packed record
       port:integer;
       public_addr:pjstring;
       bound_addr:pjstring; 
       tls_setting:pjtls;
     end;
     pjcredinfo=packed record
       realm:pjstring;
       scheme:pjstring;
       username:pjstring;
       data_type:integer;
       data:pjstring;
       aka_k:pjstring;
       aka_op:pjstring;
       aka_amf:pjstring;
       aka_cb:pointer;
     end;
     pjcallback=packed record
       call_state,
       incoming_call,
       call_tsx_state,
       call_media_state,
       dtmf_digit,
       call_transfer_request,
       call_transfer_status,
       call_replace_request,
       call_replaced,
       reg_state,
       buddy_state,
       pager,
       pager2,
       pager_status,
       pager_status2,
       typing,
       nat_detect:pointer;
     end;
     pjconfig=packed record
       max_calls:integer;
       thread_cnt:integer;
       nameserver_cnt:integer;
       nameserver:array[1..4] of pjstring;
       outbound_proxy_cnt:integer;
       outbound_proxy:array[1..4] of pjstring;
       stun_domain:pjstring;
       stun_host:pjstring;
       stun_relay_host:pjstring;
       nat_type_in_sdp:integer;
       require_100rel:integer;
       cred_count:integer;
       cred_info:array[1..8] of pjcredinfo;
       callback:pjcallback;
       user_agent:pjstring;
     end;

     pjcallinfo=packed record
       call_id:integer;
       role:integer;
       account_id:integer;
       local_info:pjstring;
       local_contact:pjstring;
       remote_info:pjstring;
       remote_contact:pjstring;
       callid:pjstring;
       state:integer;
       state_text:pjstring;
       last_status:integer;
       last_status_text:pjstring;
       media_status:integer;
       media_dir:integer;
       conf_slot:integer;
       connect_duration:pjtime;
       total_duration:pjtime;
       buf_local_info,
       buf_local_contact,
       buf_remote_info,
       buf_remote_contact,
       buf_callid,
       buf_last_status_text:array[1..128] of char;
     end;



function pjsua_create():integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_create';
procedure pjsua_config_default(var config:pjconfig); cdecl; external 'pjsua_lib.dll' name 'pjsua_config_default';
function pjsua_init(ua_cfg,log_cfg,media_cfg:pointer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_init';
function pjsua_acc_add_local(transport_id,is_default:integer;var account_id:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_acc_add_local';
procedure pjsua_transport_config_default(var transport:pjtransport) ; cdecl; external 'pjsua_lib.dll' name 'pjsua_transport_config_default';
function pjsua_transport_create(ttype:integer;var transport:pjtransport;var id:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_transport_create';
function pjsua_call_make_call(account:integer;var dst_uri:pjstring;options:integer;user_data:pointer;msg_data:pointer;var call_id:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_call_make_call';
function pjsua_player_create(var filename:pjstring;options:integer;var player_id:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_player_create';
function pjsua_player_get_conf_port(player_id:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_player_get_conf_port';
function pjsua_conf_connect(source,sink:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_conf_connect';
function pjsua_media_transports_create(var transport:pjtransport):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_media_transports_create';
function pjsua_call_get_conf_port(call_id:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_call_get_conf_port';
function pjsua_call_get_info(call_id:integer;var call_info:pjcallinfo):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_call_get_info';
function pjsua_destroy() :integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_destroy';
procedure pjsua_call_hangup_all(); cdecl; external 'pjsua_lib.dll' name 'pjsua_call_hangup_all';
function pjsua_set_ec(tail_ms,options:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_set_ec';
function pjsua_start():integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_start';
function pjsua_codec_set_priority(var codec_id:pjstring;priority:byte):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_codec_set_priority';
function pjsua_set_null_snd_dev():integer ; cdecl; external 'pjsua_lib.dll' name 'pjsua_set_null_snd_dev';

var T:pjtransport;
    TransportID,AccountID,CallID,PlayerID,PortID:Integer;
    uri:array of string;
    pjuri:array of pjstring;
    config:pjconfig;
    rtp:pjtransport;
    i:Integer;
    pcmu:string='PCMU';
    pjpcmu:pjstring;

procedure media_state(call_id:integer);cdecl;
var info:pjcallinfo;
    wav:string;
    pjwav:pjstring;
begin
  case random(2) of
    0:wav:='12345.wav';
    1:wav:='senorita.wav';
  end;
  writeln('pjsua_call_get_info:',pjsua_call_get_info(call_id,info));
  if info.media_status=1 then
  begin
    pjwav.s:=@wav[1];
    pjwav.sz:=length(wav);
    writeln('pjsua_player_create:',pjsua_player_create(pjwav,0,PlayerID));
    PortID:=pjsua_player_get_conf_port(PlayerID);

    pjsua_conf_connect(PortID,pjsua_call_get_conf_port(call_id));
  end;
end;


begin
  writeln('pjsua_create:',pjsua_create);
  try
    pjsua_config_default(config);
    config.callback.call_media_state:=@media_state;
    writeln('pjsua_init:',pjsua_init(@config,nil,nil));
    pjsua_transport_config_default(T);
    writeln('pjsua_transport_create:',pjsua_transport_create(1,T,TransportID));
    writeln('pjsua_acc_add_local:',pjsua_acc_add_local(TransportID,1,AccountID));

    if ParamCount>0 then
    begin
      SetLength(pjuri,ParamCount);
      SetLength(uri,ParamCount);
      for i:=0 to ParamCount-1 do
      begin
        uri[i]:='sip:'+ParamStr(i+1);
        pjuri[i].s:=@uri[i][1];
        pjuri[i].sz:=length(uri[i]);
      end
    end else
    begin
      SetLength(pjuri,1);
      SetLength(uri,1);
      uri[0]:='sip:120@192.168.0.120';
      pjuri[0].s:=@uri[0][1];
      pjuri[0].sz:=length(uri[0]);
    end;

    pjsua_transport_config_default(rtp);
    rtp.port:=5000;
    pjsua_media_transports_create(rtp);


    pjpcmu.s:=@pcmu[1];
    pjpcmu.sz:=length(pcmu);
    writeln('pjsua_codec_set_priority:',pjsua_codec_set_priority(pjpcmu,255));
    writeln('pjsua_set_ec:',pjsua_set_ec(0,0));
    writeln('pjsua_set_null_snd_dev:',pjsua_set_null_snd_dev);
    writeln('pjsua_start:',pjsua_start);

    for i:=0 to Length(pjuri)-1 do
    begin
      writeln('pjsua_make_call:',pjsua_call_make_call(AccountID,pjuri[i],0,nil,nil,CallID));
    end;

    readln;
    pjsua_call_hangup_all;
  finally
    pjsua_destroy;
  end;
  readln;
end.
