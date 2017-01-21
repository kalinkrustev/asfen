unit pjsua;
interface
type
     pjstring=packed record
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
     pjsip_cred_info=packed record
       realm:pjstring;// Use "*" to make a credential that can be used to authenticate against any challenges.
       scheme:pjstring;// "digest"
       username:pjstring;
       data_type:integer;// Type of data (0 for plaintext passwd).
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
       rx,
       tx,
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
       cred_info:array[1..8] of pjsip_cred_info;
       callback:pjcallback;
       user_agent:pjstring;
     end;

     pjlogconfig=packed record
       log_sip_msg:integer;
       level:integer;
       console_level:integer;
       decoration:integer;
       log_file:pjstring;
       callback:pointer;
     end;

     pjmediaconfig=packed record
       clock_rate:integer;
       audio_frame_ptime:integer;
       max_media_ports:integer;
       has_ioqueue:integer;
       thread_cnt:integer;
       quality:integer;
       ptime:integer;
       no_vad:integer;
       ilbc_mode:integer;
       tx_drop_pct:integer;
       rx_drop_pct:integer;
       ec_options:integer;
       ec_tail_len:integer;
       jb_init:integer;
       jb_min_pre:integer;
       jb_max_pre:integer;
       jb_max:integer;
       enable_ice:integer;
       enable_relay:integer;
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

     pjmedia_port_info=packed record
       name:pjstring;
       signature:array[1..4] of char;
       mediatype:integer;
       has_info:integer;
       need_info:integer;
       payload_type:cardinal;
       encoding_name:pjstring;
       clock_rate:cardinal;
       channel_count:cardinal;
       bits_per_sample:cardinal;
       samples_per_frame:cardinal;
       bytes_per_frame:cardinal;
     end;

     ppjmedia_frame=^pjmedia_frame;
     pjmedia_frame=packed record
       frame_type:integer;
       buffer:pointer;
       size:integer;
       timestamp:int64;
     end;

     ppjmedia_port=^pjmedia_port;
     pjmedia_port=packed record
       info:pjmedia_port_info;
       port_data_p:pointer;
       port_data_l:integer;
       put_frame:function(port: ppjmedia_port;frame:ppjmedia_frame):integer;cdecl;
       get_frame:function(port: ppjmedia_port;frame:ppjmedia_frame):integer;cdecl;
       on_destroy:function(port: ppjmedia_port):integer;cdecl;
     end;

     pjsua_acc_config=packed record
       priority:integer;
       id:pjstring;//sip:account@serviceprovider
       reg_uri:pjstring;//sip:serviceprovider
       publish_presence:integer;
       initial_authentication:integer;
       initial_algorithm:pjstring;
       pidf_tuple_id:pjstring;
       force_contact:pjstring;
       require_100rel:integer;
       proxy_cnt:cardinal;
       proxy:array[1..8] of pjstring;
       reg_timeout:cardinal;
       cred_count:cardinal;
       cred_info:array[1..8] of pjsip_cred_info;
       transport_id:integer;
       auto_update_nat:integer;
       ka_interval:cardinal;
       ka_data:pjstring;
     end;

function pjsua_create():integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_create';
procedure pjsua_config_default(var config:pjconfig); cdecl; external 'pjsua_lib.dll' name 'pjsua_config_default';
procedure pjsua_logging_config_default(var config:pjlogconfig); cdecl; external 'pjsua_lib.dll' name 'pjsua_logging_config_default';
procedure pjsua_media_config_default(var config:pjmediaconfig); cdecl; external 'pjsua_lib.dll' name 'pjsua_media_config_default';
function pjsua_init(ua_cfg,log_cfg,media_cfg:pointer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_init';
function pjsua_acc_add_local(transport_id,is_default:integer;var account_id:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_acc_add_local';
function pjsua_acc_add(var config:pjsua_acc_config;is_default:integer;var account_id:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_acc_add';
procedure pjsua_acc_config_default(var config:pjsua_acc_config) ; cdecl; external 'pjsua_lib.dll' name 'pjsua_acc_config_default';
procedure pjsua_transport_config_default(var transport:pjtransport) ; cdecl; external 'pjsua_lib.dll' name 'pjsua_transport_config_default';
function pjsua_transport_create(ttype:integer;var transport:pjtransport;var id:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_transport_create';
function pjsua_call_make_call(account:integer;var dst_uri:pjstring;options:integer;user_data:pointer;msg_data:pointer;var call_id:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_call_make_call';
function pjsua_call_hangup(callid:integer;code:integer;reason:ppjstring;msgdata:pointer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_call_hangup';
function pjsua_player_create(var filename:pjstring;options:integer;var player_id:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_player_create';
function pjsua_player_get_conf_port(player_id:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_player_get_conf_port';
function pjsua_conf_connect(source,sink:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_conf_connect';
function pjsua_conf_disconnect(source,sink:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_conf_disconnect';
function pjsua_media_transports_create(var transport:pjtransport):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_media_transports_create';
function pjsua_call_get_conf_port(call_id:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_call_get_conf_port';
function pjsua_call_get_info(call_id:integer;var call_info:pjcallinfo):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_call_get_info';
function pjsua_destroy() :integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_destroy';
procedure pjsua_call_hangup_all(); cdecl; external 'pjsua_lib.dll' name 'pjsua_call_hangup_all';
function pjsua_set_ec(tail_ms,options:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_set_ec';
function pjsua_start():integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_start';
function pjsua_codec_set_priority(var codec_id:pjstring;priority:byte):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_codec_set_priority';
function pjsua_set_null_snd_dev():integer ; cdecl; external 'pjsua_lib.dll' name 'pjsua_set_null_snd_dev';
function pjsua_set_no_snd_dev():pointer ; cdecl; external 'pjsua_lib.dll' name 'pjsua_set_no_snd_dev';
function pjsua_call_get_user_data(call_id:integer):pointer;cdecl; external 'pjsua_lib.dll' name 'pjsua_call_get_user_data';
function pjsua_pool_create(name:pchar;size,increment:integer):pointer;cdecl; external 'pjsua_lib.dll' name 'pjsua_pool_create';
procedure pjsua_pool_release(pool:pointer);cdecl; external 'pjsua_lib.dll' name 'pjsua_pool_release';
function pjsua_conf_add_port(pool:pointer;port:pointer;var portid:integer):integer;cdecl; external 'pjsua_lib.dll' name 'pjsua_conf_add_port';
function pjmedia_mem_player_create(pool,buffer:pointer;size,rate,channels,samples_per_frame,bits_per_sample,options:integer;var port:pointer):integer;cdecl; external 'pjsua_lib.dll' name 'pjmedia_mem_player_create';
function pjsua_set_snd_dev(capture,playback:integer):integer;cdecl; external 'pjsua_lib.dll' name 'pjsua_set_snd_dev';
function pjmedia_mem_player_set_eof_cb(port,userdata,callback:pointer):integer;cdecl; external 'pjsua_lib.dll' name 'pjmedia_mem_player_set_eof_cb';
function pjmedia_port_destroy(port:pointer):integer;cdecl; external 'pjsua_lib.dll' name 'pjmedia_port_destroy';
function pjsua_conf_remove_port(portid:integer):integer; cdecl; external 'pjsua_lib.dll' name 'pjsua_conf_remove_port';

implementation
end.
