{
  @html(<b>)
  Server Module for Remote Functions
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  This unit introduces @Link(TRtcServerModule), a server-side component for ENABLING remote functions.
  Implementing RTC Remote Functions is as easy as writing local functions.
}

unit rtcSrvModule;

{$INCLUDE rtcDefs.inc}

interface

uses
  rtcTrashcan,

  Classes,
  SysUtils,

  memBinList,

  rtcSyncObjs,
  rtcThrPool,
  rtcTimer,
  rtcInfo,
  rtcConn,
  rtcCrypt,

{$IFDEF COMPRESS}
  rtcZLib,
{$ENDIF}

  rtcFastStrings,
  rtcDataSrv,
  rtcFunction;

const
  // @exclude
  RTC_SERVERMODULE_DELAYED_CALL:string='$RTC_SERVERMODULE_DELAYED_CALL$';

type
  // @exclude
  TRtcCryptServer=class(TRtcObject)
  public
    HaveHello,HaveStart:boolean;
    ControlCounter:integer;
    ClientHello,ServerHello,
    ClientKey,ServerKey,
    ControlKey:string;

    Read,Write:TRtcCrypt;

    destructor Destroy; override;

    procedure Init;
    procedure Kill; override;
    end;

  { @abstract(Delayed Function Call object)
    This object is returned from the PrepareDelayedCall function and
    can be used to Wake up the delayed call. The only method you are
    allowed to use on this object is "WakeUp", and this one may
    only be called once, after which you have to "forget" about that object
    (set your variable's pointer to NIL), so you don't go and call it often. }
  TRtcDelayedCall = class(TRtcObject)
  protected
    SessionID:string;
    crypt:TRtcCryptServer;
    output:TRtcHugeString;
    {$IFDEF COMPRESS}
    Compress:TRtcCompressLevel;
    {$ENDIF}
    FMT:TRtcDataFormat;

    msDelay:integer;
    Param:TRtcFunctionInfo;
    CallEvent:TRtcFunctionCallEvent;
    AutoSessionsLive:integer;

    Called:boolean;
    Timer:TRtcTimer;
    WaitEvent:TRtcEvent;
    Conn:TRtcConnection;

    procedure Post(Multi_Threaded:boolean);

    procedure Execute;

    procedure Cancel;

  public
    // @exclude
    constructor Create;
    // @exclude
    destructor Destroy; override;

    // @exclude
    procedure Kill; override;

    // Use this method to wake the delayed call up,
    // so it will be called NOW! instead of when the timeout period has expired.
    procedure WakeUp;
    end;

  // @exclude
  EDelayedCall = class(EAbort)
  public
    call:TRtcDelayedCall;
    end;

  { @abstract(Accepts the request and uses a TRtcFunctionGroup component to
    execute received functions and prepares the result)

    ModuleProvider is the Remote Object execution point, that enables the Client(s) to
    send one or more objects to the server and get executed object(s) as a result.
    If there are any function calls found inside the objects received, those functions
    will be executed, so that the resulting object contains only data. That resulting
    object (or a set of objects) will be sent back to the client who sent the request.
    In case of an exception, execution will be aborted and the last object sent to the
    client will be an exception message: isType = rtc_Exception; asException = error message; @html(<br><br>)

    Raising an exception in any event implemented for the Module Provider,
    will result in sending an Exception object back to the client. }
  TRtcServerModule=class(TRtcAbsDataServerLink)
  private
    FFunctions:TRtcFunctionGroup;
    FModuleFileName:string;
    FModuleHost:string;
    FAutoSessionsLive: integer;
    FAutoSessionsLock: TRtcSessionLockType;

    FOnListenStart:TRtcNotifyEvent;
    FOnListenStop:TRtcNotifyEvent;
    FOnRequestAccepted:TRtcNotifyEvent;
    FOnResponseDone:TRtcNotifyEvent;
    FOnDisconnect:TRtcNotifyEvent;
    FOnSessionOpen:TRtcNotifyEvent;
    FOnSessionClose:TRtcNotifyEvent;

    FAutoEncrypt: integer;
    FForceEncrypt: boolean;
    FSecureKey: string;
    FAutoSessions: boolean;
    {$IFDEF COMPRESS}
    FCompress: TRtcCompressLevel;
    {$ENDIF}

    FDataFormats: TRtcDataFormatSupport;

    function GetCrypt(Session:TRtcSession):TRtcCryptServer;
    procedure NewCrypt(Session:TRtcSession);
    procedure DelCrypt(Session:TRtcSession);

    function GetFunctionGroup: TRtcFunctionGroup;
    procedure SetFunctionGroup(const Value: TRtcFunctionGroup);
    function GetModuleFileName: string;
    procedure SetModuleFileName(const Value: string);
    function GetModuleHost: string;
    procedure SetModuleHost(const Value: string);
    procedure SetAutoEncrypt(const Value: integer);
    procedure SetAutoSessions(const Value: boolean);
    procedure SetForceEncrypt(const Value: boolean);
    {$IFDEF COMPRESS}
    procedure SetCompress(const Value: TRtcCompressLevel);
    {$ENDIF}
    procedure SetAutoSessionsLock(const Value: TRtcSessionLockType);

  protected
    // @exclude
    procedure Call_ListenStart(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ListenStop(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_CheckRequest(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_RequestAccepted(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_DataReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_DataOut(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_DataIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_DataSent(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ReadyToSend(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_Disconnect(Sender:TRtcConnection); override;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

  published
    {$IFDEF COMPRESS}
    { Use this property to define what compression level you want to use when sending
      data from Server to client. Default Compression value is "cNone" (no compression).
      You can use different compression levels between client and server (for example,
      fast or no compression from client and max from server). If your server has to
      work with clients which don't support compression, you have to use "cNone". }
    property Compression:TRtcCompressLevel read FCompress write SetCompress default cNone;
    {$ENDIF}

    {  Use this property to define what Data Formats you want to accept for this ServerModule.
       Since this is a set, you can choose one supported format or all supported formats. }
    property DataFormats:TRtcDataFormatSupport read FDataFormats write FDataFormats default [fmt_RTC];

    { Set this property to a value other than 0 if you want to enable automatic
      Encryption for clients which have their EncryptionKey option activated.
      EncryptionKey value defines the length on the encryption key, in bytes.
      One byte represents encryption strength of 8 bits. To have 256-bit encryption,
      set EncryptionKey=32. @html(<br><br>)

      The final encryption key is combined from a key received by the client
      and a key generated by this ServerModule. When EncryptionKey is >0 by the
      ClientModule doing the call AND by this ServerModule, Encryption handshake will
      be done automatically by those two components, so that the user only has to set
      the values and use the components as if there is no encryption.

      If ServerModule's EncryptionKey property is 0 (zero), server will not allow data to be
      encrypted and all communication with all clients will flow without encryption.
      Clients which have ForceEncryption set to True, will not work if the server
      doesn't want to support encryption. }
    property EncryptionKey:integer read FAutoEncrypt write SetAutoEncrypt default 0;

    { If you need a 100% secure connection, define a Secure Key string
      (combination of letters, numbers and special characters) for each
      ServerModule/ClientModule pair, additionally to the EncryptionKey value.
      ClientModule will be able to communicate with the ServerModule ONLY if
      they both use the same SecureKey. Default value for the SecureKey is
      an empty string (means: no secure key). @html(<br><br>)

      SecureKey will be used in the encryption initialisation handshake,
      to decrypt the first key combination received by the ClientModule.
      Since all other data packages are already sent using some kind of encryption,
      by defining a SecureKey, you encrypt the only key part which would have
      been sent out without special encryption. }
    property SecureKey:string read FSecureKey write FSecureKey;

    { Setting this property to TRUE will tell the ServerModule to work with
      Clients ONLY if they requested to use encryption. If AutoEncryptKey is > 0 and
      Client doesn't request encryption, function calls will not be processed
      and any request coming from the client will be rejected, until
      client requests and initializes encryption. }
    property ForceEncryption:boolean read FForceEncrypt write SetForceEncrypt default False;

    { Using this property, you define how long a session will live (in seconds)
      when there are no requests from this client and the session was
      created by a call from ClientModule that has its AutoSessions property enabled.
      The higher this value, the longer a session will stay valid when there
      are no requests coming from the client for which the session was created.
      This value will be used only after the Client has sent a valid request
      which produces a valid response from the server. Before that, a default
      Session Live time of @Link(RTC_SESSION_TIMEOUT) seconds will be used. @html(<br><br>)

      Session Live counter is reset each time a new request is received from the same client,
      so that this parameter only removes sessions which are inactive longer than
      AutoSessionsLive seconds. To keep your server from nasty clients creating tons of
      sessions and leaving them inactive, keep this property under 600 seconds,
      even if you want your session to stay alive for a long time. You do not have to
      overexagurate this value, since every session consumes memory and client sessions which are not
      needed will ONLY THEN be removed from memory when this AutoSessionsLive timeout expires. }
    property AutoSessionsLive:integer read FAutoSessionsLive write FAutoSessionsLive default 0;

    { Set this property to TRUE if you want ClientModule's to be able to
      reqest a new session automatically by using the NEWID parameter.
      Session handling is built into the ClientModule and uses Request.Params to
      send the Session.ID to the server and Response.Cookie['ID'] to receive a
      new session ID from the server and initialize the session object. @html(<br><br>)

      Since session ID's are communicated automaticaly by the ClientModule and
      ServerModule components, all TRtcFunction and TRtcResult components used by
      this ClientModule will have direct access to the session object.
      When AutoSessions is set to true, Client's can automaticaly request a new
      session is no session exists or when a session expires. @html(<br><br>)

      When AutoSessions is FALSE, you have to request a new session by calling
      a remote server function to generate a new session and return the session ID. }
    property AutoSessions:boolean read FAutoSessions write SetAutoSessions default false;

    { When AutoSessions are used, you can define what client data you want to use to lock the
      Sessions to this client and deny access to session data from other clients. }
    property AutoSessionsLock:TRtcSessionLockType read FAutoSessionsLock write SetAutoSessionsLock default sesFwdLock;

    { If ModuleHost is specified, then Request.Host will be compared to the ModuleHost
      property to decide if this request should be processed by this ServerModule. @html(<br>)
      If your DataServer has to serve more different hosts, while your ServerModule
      is not supposed to react to requests from all those hosts, you can assign the
      host name to which this ServerModule belongs to. If ModuleHost is left blank,
      then this ServerModule will respond to any request asking for this servermodule's
      ModuleFileName, regardless of the HOST header. @html(<br><br>)

      To process all requests for a domain and all of its sub-domains, enter domain name ONLY
      (example: "realthinclient.com" for "realthinclient.com" and any sub-domain like
      "www.realthinclient.com", "mymod.myapp.realthinclient.com", etc). @html(<br>)
      To limit the requests to a sub-domain and its sub-sub-domains, enter the name
      of the highest sub-domain (example: "myapp.realthinclient.com" for
      "myapp.realthinclient.com" and any of its sub-domains like
      "mymod.myapp.realthinclient.com"). @html(<br>)
      To process ONLY requests pointed exactly to ONE HOST, add "." in front of your
      host name (example: ".realthinclient.com" will ONLY react to requests with
      "realthinclient.com" in their HOST header). }
    property ModuleHost:string read GetModuleHost write SetModuleHost;
    { This property will be compared to Request.FileName to decide if the
      request we just received was pointed to this ServerModule. Any request asking
      for this FileName will be processed by this ServerModule component.
      Since parameters are passed to the server module through request's Content
      body (rather than headers), we do not need to check the request for anything
      else than it's FileName to know if the request is directed to this module. }
    property ModuleFileName:string read GetModuleFileName write SetModuleFileName;
    { Set this property to tell the RtcServerModule to use this TRtcFunctionGroup
      component to execute all functions received as a request from clients. }
    property FunctionGroup:TRtcFunctionGroup read GetFunctionGroup write SetFunctionGroup;

    { Event to be triggered after a new Session was opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { Event to be triggered before an existing Session was to be closed. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;
    { Event to be triggered when the Server starts listening on the connection.
      You can use this event for component initialization. }
    property OnListenStart:TRtcNotifyEvent read FOnListenStart write FOnListenStart;
    { Event to be triggered when the Server stops listening on the connection.
      You can use this event for component deinitialization. }
    property OnListenStop:TRtcNotifyEvent read FOnListenStop write FOnListenStop;
    { Event to be triggered when a child DataProvider component accepts the Request.
      You can use this event for request initialization. For example,
      you could create a DataTunel and assign it to Tunel, to have reuqest data tunneled. }
    property OnRequestAccepted:TRtcNotifyEvent read FOnRequestAccepted write FOnRequestAccepted;
    { Event to be triggered after the response was sent out (Response.Done) }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { Event to be triggered when connection gets lost after a request was accepted.
      You can use this event for component deinitialization. }
    property OnDisconnect:TRtcNotifyEvent read FOnDisconnect write FOnDisconnect;
    end;

{ Post a function call to "Event" using "Param" parameters, delayed for "msDelay" miliseconds.
  If you need to make changes to parameters, do it BEFORE calling PostDelayedCall.
  This procedure ONLY works if  called from a function which was called by TRtcServerModule. }
procedure PostDelayedCall(msDelay:integer; Param:TRtcFunctionInfo; Event:TRtcFunctionCallEvent); overload;

{ Prepare a function call to "Event" using "Param" paremeters, which should be delayed for "msDelay" miliseconds.
  Use "PostDelayedCall" with the object returned from PrepareDelayedCall (TRtcDelayedCall) to post this call.
  You can also use the Result (TRtcDelayedCall) to execute the delayed call sooner (from any thread) by calling "TRtcDelayedCall.WakeUp".
  You can NOT CANCEL THE CALL after you have Posted it with PostDelayedCall.
  If connection should get lost while a Delayed Call is waiting, delayed call will be canceled automaticaly.
  If you need to make any changes to the parameters passed to the delayed call,
  you have to do it BEFORE using PrepareDelayedCall, because PrepareDelayedCall creates a copy of all parameters
  and any change to the original Params will not be reflected in that copy later. }
function PrepareDelayedCall(msDelay:integer; Param:TRtcFunctionInfo; Event:TRtcFunctionCallEvent):TRtcDelayedCall;

{ Post delayed call which was prepared using PrepareDelayedCall.
  You can use the Result (TRtcDelayedCall) to execute the delayed call sooner (from any thread) by using the "TRtcDelayedCall.WakeUp" method.
  You can NOT CANCEL THE CALL after you have Posted it by using PortDelayedCall, you can only Wake it up sooner.
  If connection should get lost while a Delayed Call is waiting, delayed call will be canceled automaticaly. }
procedure PostDelayedCall(cb:TRtcDelayedCall); overload;

{ If you prepared a delayed call, but do not want to Post it anymore,
  use this procedure to cancel it before calling PostDelayedCall. @html(<br>)
  DO NOT CALL THIS PROCEDURE AFTER YOU HAVE POSTED THE CALL!!!!
  You can NOT CANCEL THE CALL after you have Posted it (with PostDelayedCall).
  If connection should get lost while a Delayed Call is waiting, delayed call will be canceled automaticaly. }
procedure CancelDelayedCall(cb:TRtcDelayedCall);

implementation

{ Prepare a function call to "Event" using "Param" paremeters, which should be delayed for "msDelay" miliseconds.
  Use "PostDelayedCall" to post this delayed call, after you've stored the Result returned by this function.
  You can use the Result (TRtcDelayedCall) to execute the delayed call sooner (from any thread) by using the "TRtcDelayedCall.Call" method. }
function PrepareDelayedCall(msDelay:integer; Param:TRtcFunctionInfo; Event:TRtcFunctionCallEvent):TRtcDelayedCall;
  begin
  Result:=TRtcDelayedCall.Create;
  Result.msDelay:=msDelay;
  Result.Param:=TRtcFunctionInfo(Param.copyOf);
  Result.CallEvent:=Event;
  end;

{ Post delayed call which was prepared using PrepareDelayedCall. }
procedure PostDelayedCall(cb:TRtcDelayedCall); overload;
  var
    e:EDelayedCall;
  begin
  e:=EDelayedCall.Create('');
  e.call:=cb;
  raise e;
  end;

procedure CancelDelayedCall(cb:TRtcDelayedCall);
  begin
  cb.Cancel;
  end;

procedure PostDelayedCall(msDelay:integer; Param:TRtcFunctionInfo; Event:TRtcFunctionCallEvent);
  var
    e:EDelayedCall;
  begin
  e:=EDelayedCall.Create('');
  e.call:=PrepareDelayedCall(msDelay,Param,Event);
  raise e;
  end;

{ TRtcServerModule }

constructor TRtcServerModule.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FDataFormats:=[fmt_RTC];
  FAutoSessionsLock:=sesFwdLock;
  FFunctions:=nil;
  FModuleFileName:='';
  FModuleHost:='';
  end;

destructor TRtcServerModule.Destroy;
  begin
  FFunctions:=nil;
  FModuleFileName:='';
  FModuleHost:='';
  inherited;
  end;

procedure TRtcServerModule.Call_CheckRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    SetActiveLink(self);
    if Request.FileName=FModuleFileName then
      begin
      if FModuleHost<>'' then
        begin
        if copy(FModuleHost,1,1)<>'.' then // accepting domain with any sub-domain
          begin
          if (UpperCase(Request.Host)=FModuleHost) then // host = domain name
            Accept
          else if ( (length(Request.Host)>length(FModuleHost)) and // could be sub-domain
                    (Copy(Request.Host,length(Request.Host)-length(FModuleHost),1)='.') and // has '.' in the right place
                    ( UpperCase(Copy(Request.Host,length(Request.Host)-length(FModuleHost)+1,length(FModuleHost)))
                      = FModuleHost) ) then // is sub-domain
            Accept;
          end
        else if UpperCase(Request.Host)=Copy(FModuleHost,2,length(FModuleHost)-1) then
          Accept; // accepting a specific sub-domain only
        end
      else // Accept the request. It has our ModuleFileName as Request.FileName, we accept all hosts
        Accept;
      end;
    end;
  end;

procedure TRtcServerModule.Call_ListenStart(Sender: TRtcConnection);
  begin
  if assigned(FOnListenStart) then
    FOnListenStart(Sender);
  end;

procedure TRtcServerModule.Call_ListenStop(Sender: TRtcConnection);
  begin
  if assigned(FOnListenStop) then
    FOnListenStop(Sender);
  end;

procedure TRtcServerModule.Call_SessionClose(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionClose) then
    FOnSessionClose(Sender);
  end;

procedure TRtcServerModule.Call_SessionOpen(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionOpen) then
    FOnSessionOpen(Sender);
  end;

procedure TRtcServerModule.Call_RequestAccepted(Sender: TRtcConnection);
  begin
  try
    if assigned(FOnRequestAccepted) then
      FOnRequestAccepted(Sender);

    if assigned(Link) then
      Link.Call_RequestAccepted(Sender);
  except
    on E:Exception do
      with TRtcValue.Create do
        try
          asException:=E.Message;
          if TRtcDataServer(Sender).Request.Info.asBoolean['$XML'] then
            Sender.Write(toXMLrpcResponse)
          else
            Sender.Write(toCode);
        finally
          Free;
        end;
    end;
  end;

procedure TRtcServerModule.Call_ResponseDone(Sender: TRtcConnection);
  begin
  try
    if assigned(FOnResponseDone) then
      FOnResponseDone(Sender);

    if assigned(Link) then
      Link.Call_ResponseDone(Sender);
  except
    on E:Exception do
      with TRtcValue.Create do
        try
          asException:=E.Message;
          if TRtcDataServer(Sender).Request.Info.asBoolean['$XML'] then
            Sender.Write(toXMLrpcResponse)
          else
            Sender.Write(toCode);
        finally
          Free;
        end;
    end;
  end;

procedure TRtcServerModule.Call_Disconnect(Sender: TRtcConnection);
  begin
  try
    if assigned(FOnDisconnect) then
      FOnDisconnect(Sender);

    if assigned(Link) then
      Link.Call_Disconnect(Sender);
  except
    on E:Exception do
      with TRtcValue.Create do
        try
          asException:=E.Message;
          if TRtcDataServer(Sender).Request.Info.asBoolean['$XML'] then
            Sender.Write(toXMLrpcResponse)
          else
            Sender.Write(toCode);
        finally
          Free;
        end;
    end;
  end;

function RandomKey(len:integer):string;
  var
    a:integer;
  begin
  Result:='';
  for a:=1 to len do
    Result:=Result+Char(random(256));
  end;

procedure CryptRead(Crypt:TRtcCryptServer; var Data:string);
  begin
  if assigned(Crypt) and assigned(Crypt.Read) then
    Crypt.Read.DeCrypt(Data);
  end;

procedure CryptWrite(Crypt:TRtcCryptServer; var Data:string);
  begin
  if assigned(Crypt) and assigned(Crypt.Write) then
    Crypt.Write.Crypt(Data);
  end;

procedure TRtcServerModule.Call_DataReceived(Sender: TRtcConnection);
  var
    crypt:TRtcCryptServer;
    tmpcnt:integer;
    code,temp:string;
    output:TRtcHugeString;
    at:integer;
    MyData,MyResult:TRtcValueObject;
    mycall:TRtcDelayedCall;
    FMT:TRtcDataFormat;

  function IsValidControlKey(var Counter:integer; const s:string):boolean;
  {$IFDEF RtcDoNotCheckCryptControlSums}
    begin
    // Old implementation uses 3 - 8 byte control keys,
    // New implementation uses 9 - 14 byte control keys.
    Result:= length(s) in [3..14];
    end;
  {$ELSE}
    var
      i,a,b,Chk:integer;
    begin
    Inc(Counter);
    if Counter>99 then Counter:=1;

    // New implementation sends a counter and a checksum,
    // which can be used to validate the control key.
    if length(s) in [9..14] then
      begin
      b:=(14-length(s))*9+8;
      for i:=5 to length(s) do
        Inc(b, Ord(s[i])-Ord('0'));
      a:=(Ord(s[1])-Ord('0'))*10 +
         (Ord(s[2])-Ord('0'));
      Chk:=(Ord(s[3])-Ord('0'))*10 +
           (Ord(s[4])-Ord('0'));
      Result:= (a=b) and (Chk=Counter);
      end
    else
      Result:=False;
    end;
  {$ENDIF}

  function ExtractControlKey(var Counter:integer):string;
    var
      i,at,len:integer;
    begin
    Result:='';
    at:=0;len:=0;
    { Data will be closed with #13+ControlCode }
    for i:=length(code) downto 1 do
      case code[i] of
        '0'..'9':
          begin
          if len<14 then // we will never use control keys longer than 14 digits
            begin
            Result:=code[i]+Result;
            Inc(len);
            end
          else
            begin
            // Return empty result, which will tell the client to reinitialize encryption
            Result:='';
            Exit;
            end;
          end;
        #13:begin
          at:=i;
          Break;
          end;
        else
          begin
          Result:='';
          Exit;
          end;
        end;

    if at>0 then // number found
      begin
      if not IsValidControlKey(Counter, Result) then
        Result:=''
      else
        SetLength(code,at-1);
      end
    else
      Result:='';
    end;

  function ExtractFinalControlKey(var Counter:integer):string;
    var
      DecompressTo,
      i,at,len:integer;
    begin
    Result:='';len:=0;
    at:=0;
    DecompressTo:=0;
    { Data will be closed with:
      > #13+ControlCode is not compressed
      > #0+ControlCode if compressed. }
    for i:=length(code) downto 1 do
      case code[i] of
        '0'..'9':
          if len<14 then // we will never use control keys longer than 14 digits
            begin
            Result:=code[i]+Result;
            Inc(len);
            end
          else
            begin
            // Return empty result, which will tell the client to reinitialize encryption
            Result:='';
            Exit;
            end;
        #13:
          begin
          at:=i;
          // data is NOT compressed
          Break;
          end;
        #0:
          begin
          at:=i;
          // data is compressed
          DecompressTo:=at-1;
          Break;
          end;
        else
          begin
          Result:='';
          Exit;
          end;
        end;

    if at>0 then // number found
      begin
      if not IsValidControlKey(Counter, Result) then
        Result:=''
      else
        begin
        if DecompressTo>0 then
          begin
          {$IFDEF COMPRESS}
          try
            code:=ZDecompress_Str(code,DecompressTo);
          except
            on E:Exception do
              Result:='';
            end;
          {$ELSE}
            Result:='';
          {$ENDIF}
          end
        else
          SetLength(code,at-1);
        end;
      end
    else
      Result:='';
    end;

  begin
  with TRtcDataServer(Sender) do
    begin
    if Request.Started then
      begin
      if (Request.Query.Text<>'') then
        begin
        if (UpperCase(Request.Query['ACTION'])='HELLO') then
          begin
          // Don't have to handle now
          end
        else if (UpperCase(Request.Query['ACTION'])='START') then
          begin
          // Don't need to handle now
          end
        else if UpperCase(Request.Query['ID'])='NEW' then
          begin
          // Don't need to handle now
          end
        else if (Request.Query['ID']<>'') then
          begin
          // Need to handle now, because there could be a lot of data coming,
          // during which our Session could time out.
          if not FindSession(Request.Query['ID']) then
            if HaveSession(Request.Query['ID']) then
              Response.Status(410,'Session locked')
            else
              Response.Status(410,'Session not found');
          end
        end;
      end;

    if Request.Complete then
      begin
      if (Request.Query.Text<>'') then
        begin
        if (UpperCase(Request.Query['ACTION'])='HELLO') then
          begin
          code:=Read;

          if EncryptionKey=0 then // Encryption not supported
            begin
            // Find Client Session
            if (Request.Query['ID']<>'') and
               (UpperCase(Request.Query['ID'])<>'NEW') then
              begin
              if not FindSession(Request.Query['ID']) then
                begin
                if HaveSession(Request.Query['ID']) then
                  begin
                  Response.Status(410,'Session locked');
                  Write;
                  end
                else
                  begin
                  Response.Status(410,'Session not found');
                  Write;
                  end;
                Exit;
                end
              else
                DelCrypt(Session);
              end
            else if AutoSessions then
              begin
              OpenSession(AutoSessionsLock);
              Session.KeepAlive:=AutoSessionsLive;
              Response.Cookie['ID']:=Session.ID;
              end;
            Write; // send response with empty content
            end
          else if code='' then
            Write
          else
            begin
            // Find Client Session
            if (Request.Query['ID']<>'') and
               (UpperCase(Request.Query['ID'])<>'NEW') then
              begin
              if not FindSession(Request.Query['ID']) then
                begin
                if HaveSession(Request.Query['ID']) then
                  begin
                  Response.Status(410,'Session locked');
                  Write;
                  end
                else
                  begin
                  Response.Status(410,'Session not found');
                  Write;
                  end;
                Exit;
                end
              else
                DelCrypt(Session);
              end;

            if SecureKey<>'' then
              begin
              with TRtcCrypt.Create do
                begin
                Key:=SecureKey;
                DeCrypt(code);
                Free;
                end;
              end;

            tmpcnt:=0;
            temp:=ExtractControlKey(tmpcnt);
            if (code='') or (temp='') then
              begin
              Response.Status(409,'Encryption Key error.');
              Write;
              Exit;
              end;

            if not assigned(Session) then
              begin
              OpenSession(AutoSessionsLock);
              Session.KeepAlive:=AutoSessionsLive;
              Response.Cookie['ID']:=Session.ID;
              end;

            NewCrypt(Session);
            crypt:=GetCrypt(Session);
            crypt.ControlCounter:=tmpcnt;

            crypt.ControlKey:=temp;
            crypt.ClientHello:=code;

            crypt.ServerHello:=RandomKey(EncryptionKey);
            crypt.HaveHello:=True;

            // Encryption object for Write operation
            crypt.Write:=TRtcCrypt.Create;
            crypt.Write.Key:=crypt.ClientHello;

            // Encryption object for Read operation
            crypt.Read:=TRtcCrypt.Create;
            crypt.Read.Key:=crypt.ServerHello;

            // Prepare, encode and send the response
            code:=crypt.ServerHello+crypt.ControlKey;
            CryptWrite(crypt, code);
            Write(code);
            end;
          Exit;
          end
        else if (UpperCase(Request.Query['ACTION'])='START') then
          begin
          if (Request.Query['ID']<>'') and
             (UpperCase(Request.Query['ID'])<>'NEW') then
            begin
            if not FindSession(Request.Query['ID']) then
              begin
              if HaveSession(Request.Query['ID']) then
                begin
                Response.Status(410,'Session locked');
                Write;
                end
              else
                begin
                Response.Status(410,'Session not found');
                Write;
                end;
              Exit;
              end;
            end
          else
            begin
            Response.Status(410,'Session not initialized.');
            Write;
            Exit;
            end;

          code:=Read;

          if EncryptionKey=0 then // Encryption not supported
            Write // send response with empty content
          else if code='' then
            Write
          else
            begin
            crypt:=GetCrypt(Session);

            if crypt=nil then
              begin
              Response.Status(410,'Session not initialized.');
              Write;
              Exit;
              end
            else if not crypt.HaveHello then
              begin
              Response.Status(410,'Session handshake error.');
              Write;
              Exit;
              end;

            // Decode client request
            CryptRead(crypt, code);

            crypt.ControlKey:=ExtractControlKey(crypt.ControlCounter);
            if (crypt.ControlKey='') or (code='') then
              begin
              Response.Status(409,'Encryption Key error.');
              Write;
              Exit;
              end;

            crypt.ClientKey:=code;
            crypt.ServerKey:=RandomKey(EncryptionKey);
            crypt.HaveStart:=True;

            // Set Encryption kexs for Reading and Writing
            crypt.Write.Key:=crypt.ClientKey+crypt.ServerHello;
            crypt.Read.Key :=crypt.ServerKey+crypt.ClientHello;

            // Prepare, encode and send the response
            code:=crypt.ServerKey+crypt.ControlKey;
            CryptWrite(crypt, code);
            Write(code);
            end;
          Exit;
          end
        else if UpperCase(Request.Query['ID'])='NEW' then
          begin
          if ForceEncryption and (EncryptionKey>0) then
            begin
            // 412 = Precondition Failed (Encryption required)
            Response.Status(412,'Encryption required.');
            Write;
            Exit;
            end
          else if AutoSessions then
            begin
            OpenSession(AutoSessionsLock);
            Session.KeepAlive:=AutoSessionsLive;
            Response.Cookie['ID']:=Session.ID;
            end;
          end
        else if (Request.Query['ID']<>'') then
          begin
          // Return empty string if Session expxired or non-existing
          if Response.StatusCode=410 then
            begin
            Write;
            Exit;
            end;
          end
        end;

      code:=Read;

      if (UpperCase(Request.ContentType)='TEXT/XML') then
        begin
        if (fmt_XMLRPC in FDataFormats) and
           (isXMLString(code)) then // data received as XML-RPC
          begin
          FMT:=fmt_XMLRPC;
          Request.Info.asBoolean['$XML']:=True;
          crypt:=nil;
          end
        else
          begin
          Response.Status(404,'Data Format not supported.');
          Write;
          Exit;
          end;
        end
      else if (fmt_RTC in FDataFormats) then
        begin
        FMT:=fmt_RTC;
        crypt:=GetCrypt(Session);
        if assigned(crypt) and assigned(crypt.Read) then
          begin
          CryptRead(crypt, code);

          try
            crypt.ControlKey:=ExtractFinalControlKey(crypt.ControlCounter);
          except
            on E:Exception do
              begin
              Response.Status(409,'Encryption Error.');
              Write;
              Exit;
              end;
            end;

          if crypt.ControlKey='' then
            begin
            Response.Status(409,'Encryption Key Error.');
            Write;
            Exit;
            end;
          end
        else if ForceEncryption and (EncryptionKey>0) then
          begin
          // 412 = Precondition Failed (Encryption required)
          Response.Status(412,'Encryption required.');
          Write;
          Exit;
          end
        else if (length(Code)>0) and (Code[length(code)]=#0) then // compressed
          begin
          {$IFDEF COMPRESS}
          try
            code:=ZDecompress_Str(code,length(code)-1);
          except
            on E:Exception do
              begin
              Response.Status(409,'Decompression error.');
              Write;
              Exit;
              end;
            end;
          {$ELSE}
          Response.Status(409,'Compression not supported.');
          Write;
          Exit;
          {$ENDIF}
          end;
        end
      else
        begin
        // 404 = Request not supported.
        Response.Status(404,'Data Format not supported.');
        Write;
        Exit;
        end;

      output:=nil;

      at:=0;
      try
        if Code='' then
          Write
        else while at<length(code) do
          begin
          case FMT of
            fmt_XMLRPC: MyData:=TRtcValue.FromXMLrpc(code,at);
            else MyData:=TRtcValue.FromCode(code,at);
            end;
          {$IFDEF COMPRESS}
          if (FMT=fmt_RTC) and (FCompress<>cNone) and not assigned(output) and
             (at<length(code)) then
            output:=TRtcHugeString.Create;
          {$ENDIF}

          try
            if not isSimpleValue(MyData) then
              begin
              if assigned(FFunctions) then
                begin
                MyResult:=FFunctions.ExecuteData(Sender, MyData);
                if MyData<>MyResult then
                  begin
                  MyData.Free;
                  MyData:=MyResult;
                  end;
                end
              else
                raise Exception.Create('FunctionGroup property undefined, can not execute call.');
              end;

            if MyData<>nil then
              begin
              if assigned(output) then
                MyData.to_Code(output)
              else
                begin
                {$IFDEF COMPRESS}
                if (FMT=fmt_RTC) and (FCompress<>cNone) then
                  begin
                  { We want to send data compressed, but we haven't created the
                    "output" object until now, which means that this is the last result,
                    so we don't need "code" anymore. }
                  code:=myData.toCode;
                  if length(code)<RTC_MIN_COMPRESS_SIZE then
                    begin
                    CryptWrite(crypt,code);
                    Write(code);
                    code:='';
                    end
                  else
                    begin
                    case FCompress of
                      cFast: temp := ZCompress_Str(code, zcFastest);
                      cMax: temp := ZCompress_Str(code, zcMax);
                      else temp := ZCompress_Str(code, zcDefault);
                      end;

                    if length(temp)>=length(code)-1 then
                      begin
                      temp:='';
                      CryptWrite(crypt,code);
                      Write(code);
                      code:='';
                      end
                    else
                      begin
                      code:='';
                      CryptWrite(crypt,temp);
                      Write(temp);

                      temp:=#0;
                      CryptWrite(crypt,temp);
                      Write(temp);
                      temp:='';
                      end;
                    end;
                  end
                else
                {$ENDIF}
                  begin
                  if FMT=fmt_XMLRPC then
                    Write(MyData.toXMLrpcResponse)
                  else
                    begin
                    if assigned(crypt) and assigned(crypt.Write) then
                      begin
                      temp:=MyData.toCode;
                      CryptWrite(crypt,temp);
                      Write(temp);
                      end
                    else
                      Write(MyData.toCode);
                    end;
                  end;
                end;
              end
            else
              Write; // no content.
          finally
            MyData.Free;
            end;
          end;
      except
        on E:EDelayedCall do
          begin
          // Create and Post the DelayedCall
          mycall:=e.call;
          mycall.Conn:=Sender;
          mycall.output:=output;
          mycall.FMT:=FMT;
          {$IFDEF COMPRESS}
          mycall.Compress:=Compression;
          {$ENDIF}
          mycall.AutoSessionsLive:=AutoSessionsLive;
          if assigned(Session) then
            begin
            mycall.SessionID:=Session.ID;
            mycall.crypt:=crypt;
            end;
          Info.Obj[RTC_SERVERMODULE_DELAYED_CALL]:=mycall;
          if Sender.isExtension then
            mycall.Execute
          else
            mycall.Post(MultiThreaded);
          Exit; // not sending the reponse now!
          end;
        on E:Exception do
          with TRtcValue.Create do
            try
              asException:=E.Message;
              if assigned(output) then
                to_Code(output)
              else
                begin
                if FMT=fmt_XMLRPC then
                  Write(toXMLrpcResponse)
                else
                  begin
                  if assigned(crypt) and assigned(crypt.Write) then
                    begin
                    temp:=asCode;
                    CryptWrite(crypt,temp);
                    Write(temp);
                    end
                  else
                    Write(asCode);
                  end;
                end;
            finally
              Free;
            end;
        end;

      {$IFDEF COMPRESS}
      if assigned(output) then
        begin
        { we have stored uncompressed data in "output",
          now we need to compress it all and send it out. }
        try
          code:=output.Get;
        finally
          output.Free;
          end;

        if length(code)<RTC_MIN_COMPRESS_SIZE then
          begin
          CryptWrite(crypt,code);
          Write(code);
          code:='';
          end
        else
          begin
          case FCompress of
            cFast: temp := ZCompress_Str(code, zcFastest);
            cMax: temp := ZCompress_Str(code, zcMax);
            else temp := ZCompress_Str(code, zcDefault);
            end;

          if length(temp)>=length(code)-1 then
            begin
            temp:='';
            CryptWrite(crypt,code);
            Write(code);
            code:='';
            end
          else
            begin
            code:='';
            CryptWrite(crypt,temp);
            Write(temp);

            temp:=#0;
            CryptWrite(crypt,temp);
            Write(temp);
            temp:='';
            end;
          end;
        end;
      {$ENDIF}
      
      if assigned(crypt) and assigned(crypt.Write) then
        begin
        // Add control key to the end of our response
        code:=crypt.ControlKey;
        CryptWrite(crypt, code);
        Write(code);
        end;
      end;
    end;
  end;

procedure TRtcServerModule.Call_DataOut(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcServerModule.Call_DataIn(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcServerModule.Call_DataSent(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcServerModule.Call_ReadyToSend(Sender: TRtcConnection);
  begin
  // empty
  end;

function TRtcServerModule.GetFunctionGroup: TRtcFunctionGroup;
  begin
  try
    Result:=FFunctions;
    if not (Result is TRtcFunctionGroup) then
      Result:=nil;
  except
    Result:=nil;
    end;
  end;

procedure TRtcServerModule.SetFunctionGroup(const Value: TRtcFunctionGroup);
  begin
  FFunctions:=Value;
  end;

function TRtcServerModule.GetModuleFileName: string;
  begin
  Result:=FModuleFileName;
  end;

procedure TRtcServerModule.SetModuleFileName(const Value: string);
  begin
  if FModuleFileName<>Value then
    begin
    FModuleFileName:=Value;
    if FModuleFileName<>'' then
      begin
      // FileName has to start with '/'
      if Copy(FModuleFileName,1,1)<>'/' then
        FModuleFileName:='/'+FModuleFileName;
      end;
    end;
  end;

function TRtcServerModule.GetModuleHost: string;
  begin
  Result:=FModuleHost;
  end;

procedure TRtcServerModule.SetModuleHost(const Value: string);
  begin
  if FModuleHost<>Value then
    // Convert to uppercase now, so we don't have to do it on every request.
    FModuleHost:=UpperCase(Value);
  end;

function TRtcServerModule.GetCrypt(Session: TRtcSession): TRtcCryptServer;
  begin
  if not assigned(Session) then
    Result:=nil
  else
    Result:=TRtcCryptServer(Session.Obj[ModuleHost+ModuleFileName+':$Crypt$']);
  end;

procedure TRtcServerModule.NewCrypt(Session: TRtcSession);
  begin
  if TRtcCryptServer(Session.Obj[ModuleHost+ModuleFileName+':$Crypt$'])<>nil then
    TRtcCryptServer(Session.Obj[ModuleHost+ModuleFileName+':$Crypt$']).Init
  else
    Session.Obj[ModuleHost+ModuleFileName+':$Crypt$']:=TRtcCryptServer.Create;
  end;

procedure TRtcServerModule.DelCrypt(Session: TRtcSession);
  begin
  if assigned(Session) then
    if TRtcCryptServer(Session.Obj[ModuleHost+ModuleFileName+':$Crypt$'])<>nil then
      begin
      TRtcCryptServer(Session.Obj[ModuleHost+ModuleFileName+':$Crypt$']).Free;
      Session.Obj[ModuleHost+ModuleFileName+':$Crypt$']:=nil;
      end;
  end;

procedure TRtcServerModule.SetAutoEncrypt(const Value: integer);
  begin
  if Value<0 then
    raise Exception.Create('Negative values not allowed for EncryptionKey.');
  FAutoEncrypt := Value;
  if FAutoEncrypt>0 then
    FAutoSessions:=True
  else
    FForceEncrypt:=False;
  end;

procedure TRtcServerModule.SetAutoSessions(const Value: boolean);
  begin
  if not Value and (FAutoEncrypt>0) then
    raise Exception.Create('Set EncryptionKey to 0 before setting AutoSessions to False.');
  FAutoSessions := Value;
  if not FAutoSessions then
    begin
    FAutoEncrypt:=0;
    FForceEncrypt:=False;
    end;
  end;

procedure TRtcServerModule.SetForceEncrypt(const Value: boolean);
  begin
  FForceEncrypt := Value;
  if FForceEncrypt then
    begin
    FAutoSessions:=True;
    if FAutoEncrypt=0 then
      FAutoEncrypt:=16;
    end;
  end;

{$IFDEF COMPRESS}
procedure TRtcServerModule.SetCompress(const Value: TRtcCompressLevel);
  begin
  FCompress := Value;
  end;
{$ENDIF}

procedure TRtcServerModule.SetAutoSessionsLock(const Value: TRtcSessionLockType);
  begin
  FAutoSessionsLock := Value;
  end;

{ TRtcCryptServer }

destructor TRtcCryptServer.Destroy;
  begin
  Init;
  inherited;
  end;

procedure TRtcCryptServer.Init;
  begin
  HaveHello:=False;
  HaveStart:=False;
  ControlKey:='';
  ClientHello:='';
  ServerHello:='';
  ClientKey:='';
  ServerKey:='';
  ControlCounter:=0;
  if assigned(Read) then
    begin
    Read.Free;
    Read:=nil;
    end;
  if assigned(Write) then
    begin
    Write.Free;
    Write:=nil;
    end;
  end;

procedure TRtcCryptServer.Kill;
  begin
  Free;
  end;

{ TRtcDelayedCall }

type
  TRtcDelayedCallJob=class(TRtcJob)
  public
    Conn:TRtcConnection;
    constructor Create(Con: TRtcConnection);

    procedure Run(Thr:TRtcThread); override;
    procedure Kill; override;
    end;

var
  List:tBinList;
  CS:TRtcCritSec;

procedure AddObj(o:TObject);
  begin
  CS.Enter;
  try
    List.insert(longword(o),1);
  finally
    CS.Leave;
    end;
  end;

procedure DelObj(o:TObject);
  begin
  CS.Enter;
  try
    List.remove(longword(o));
  finally
    CS.Leave;
    end;
  end;

procedure SetObj(o:TObject;num:integer);
  begin
  CS.Enter;
  try
    if List.search(longword(o))>0 then
      List.change(longword(o),num);
  finally
    CS.Leave;
    end;
  end;

procedure KillObjs;
  var
    ob:TObject;
    i:cardinal;
  begin
  CS.Enter;
  try
    if assigned(List) then
      begin
      while List.Count>0 do
        begin
        ob:=TObject(List.search_min(i));
        if (ob<>nil) and (i=1) then
          ob.Free;
        end;
      List.Free;
      List:=nil;
      end;
  finally
    CS.Leave;
    end;
  end;

function CheckObj(o:TObject):integer;
  begin
  CS.Enter;
  try
    Result:=List.search(longword(o));
  finally
    CS.Leave;
    end;
  end;

function HaveObj(o:TObject):boolean;
  begin
  CS.Enter;
  try
    Result:=List.search(longword(o))>0;
  finally
    CS.Leave;
    end;
  end;

constructor TRtcDelayedCall.Create;
  begin
  inherited Create;
  WaitEvent:=nil;
  Timer:=nil;
  Called:=False;
  output:=nil;

  AddObj(Self);
  end;

destructor TRtcDelayedCall.Destroy;
  begin
  CS.Enter;
  try
    if not HaveObj(self) then Exit;
    DelObj(self);
    if assigned(WaitEvent) then
      WaitEvent.SetEvent;
    if assigned(Timer) then
      TRtcTimer.Stop(Timer);
  finally
    CS.Leave;
    end;

  if assigned(output) then
    begin
    output.Free;
    output:=nil;
    end;

  Param.Free;
  SessionID:='';
  Conn:=nil;
  inherited;
  end;

procedure TRtcDelayedCall.Kill;
  begin
  if not HaveObj(self) then Exit;
  Free;
  end;

procedure TRtcDelayedCall.Cancel;
  begin
  if not HaveObj(self) then Exit;
  Free;
  end;

procedure TRtcDelayedCall.Post(Multi_Threaded:boolean);
  var
    wait:boolean;
  begin
  CS.Enter;
  try
    if not HaveObj(self) then Exit;

    wait:=not Called;
    if wait then
      begin
      Timer:=TRtcTimer.Create(Multi_Threaded);
      {$IFDEF FPC}
      TRtcTimer.Enable(Timer,msDelay,@Execute,True);
      {$ELSE}
      TRtcTimer.Enable(Timer,msDelay,Execute,True);
      {$ENDIF}
      end;
  finally
    CS.Leave;
    end;
  if not wait then
    Execute;
  end;

procedure TRtcDelayedCall.Execute;
  var
    ob:TRtcDelayedCallJob;
    wait:boolean;
  begin
  wait:=False;

  CS.Enter;
  try
    if not HaveObj(self) then Exit;

    // Stop Timer if timer active
    if assigned(Timer) then
      begin
      TRtcTimer.Stop(Timer);
      Timer:=nil;
      end
    // Start Waiting Event if no timer
    else
      begin
      wait:=not Called;
      if wait then
        WaitEvent:=TRtcEvent.Create(True,False);
      end;
  finally
    CS.Leave;
    end;

  if wait then
    begin
    WaitEvent.WaitFor(msDelay);
    CS.Enter;
    try
      if not HaveObj(self) then Exit;

      WaitEvent.Free;
      WaitEvent:=nil;
    finally
      CS.Leave;
      end;
    end;

  // We don't want to have a situation where Execute would be called twice (for example, when Timer triggers and another thread uses "Call")
  CS.Enter;
  try
    if not HaveObj(self) or (CheckObj(self)=2) then Exit;

    SetObj(self,2);
  finally
    CS.Leave;
    end;

  if assigned(Conn) then
    begin
    ob:=TRtcDelayedCallJob.Create(Conn);
    if not Conn.PostJob(ob) then
      ob.Kill;
    end;
  end;

procedure TRtcDelayedCall.WakeUp;
  var
    needtocall:boolean;
  begin
  needtocall:=False;

  CS.Enter;
  try
    if not HaveObj(self) or (CheckObj(self)=2) then Exit;
    Called:=True;

    if assigned(Timer) then
      begin
      // Timer active. Need to stop the timer and call Execute
      needtocall:=True;
      TRtcTimer.Stop(Timer);
      Timer:=nil;
      end
    else if assigned(WaitEvent) then
      // Wait Event active. Only need to Set the Event flag
      WaitEvent.SetEvent;
  finally
    CS.Leave;
    end;

  if needtocall then
    Execute;
  end;

{ TRtDelayedCallJob }

constructor TRtcDelayedCallJob.Create(Con: TRtcConnection);
  begin
  inherited Create;
  Conn:=Con;
  end;

procedure TRtcDelayedCallJob.Kill;
  begin
  Free;
  end;

procedure TRtcDelayedCallJob.Run(Thr:TRtcThread);
  var
    call,mycall:TRtcDelayedCall;
    MyResult:TRtcValue;
    code,temp:string;
    released:boolean;
  begin
  released:=False;
  call:=nil;
  try
    if assigned(Conn) then with TRtcDataServer(Conn) do
      begin
      call:=TRtcDelayedCall(Info.Obj[RTC_SERVERMODULE_DELAYED_CALL]);
      if not assigned(call) then Exit;
      Info.Obj[RTC_SERVERMODULE_DELAYED_CALL]:=nil;

      if (call.SessionID<>'') then
        if not assigned(Session) or (Session.ID<>call.SessionID) then
          begin
          Disconnect;
          Exit;
          end;

      EnterEvent;
      try
        try
          MyResult:=TRtcValue.Create;
          try
            call.CallEvent(call.Conn, call.Param, myResult);

            if assigned(call.output) then
              myResult.to_Code(call.output)
            else
              begin
              {$IFDEF COMPRESS}
              if (call.FMT=fmt_RTC) and (call.Compress<>cNone) then
                begin
                code:=myResult.toCode;
                if length(code)<RTC_MIN_COMPRESS_SIZE then
                  begin
                  CryptWrite(call.crypt, code);
                  Write(code);
                  end
                else
                  begin
                  { we have stored uncompressed data in "output",
                    now we need to compress it all and send it out. }

                  case call.Compress of
                    cFast: temp:=ZCompress_Str(code,zcFastest);
                    cMax: temp:=ZCompress_Str(code,zcMax);
                    else temp:=ZCompress_Str(code,zcDefault);
                    end;

                  if length(temp)>=length(code)-1 then
                    begin
                    temp:='';
                    CryptWrite(call.crypt,code);
                    Write(code);
                    code:='';
                    end
                  else
                    begin
                    code:='';
                    CryptWrite(call.crypt,temp);
                    Write(temp);

                    temp:=#0;
                    CryptWrite(call.crypt,temp);
                    Write(temp);
                    temp:='';
                    end;
                  end;
                end
              else
              {$ENDIF}
                begin
                if call.FMT=fmt_XMLRPC then
                  Write(MyResult.toXMLrpcResponse)
                else
                  begin
                  if assigned(call.crypt) and assigned(call.crypt.Write) then
                    begin
                    temp:=MyResult.toCode;
                    CryptWrite(call.crypt,temp);
                    Write(temp);
                    end
                  else
                    Write(MyResult.toCode);
                  end;
                end;
              end;
          finally
            MyResult.Free;
            end;
        except
          on E:EDelayedCall do
            begin
            // Create and Post the DelayedCall
            mycall:=E.call;
            Info.Obj[RTC_SERVERMODULE_DELAYED_CALL]:=mycall;

            mycall.Conn:=Conn;
            mycall.output:=call.output;
            mycall.FMT:=call.FMT;
            {$IFDEF COMPRESS}
            mycall.Compress:=call.Compress;
            {$ENDIF}
            mycall.crypt:=call.crypt;
            mycall.SessionID:=call.SessionID;
            mycall.AutoSessionsLive:=call.AutoSessionsLive;

            call.crypt:=nil;
            call.output:=nil;

            call.Free;
            call:=nil;

            if Conn.isExtension then
              begin
              self.Free;
              released:=True;
              mycall.Execute;
              end
            else
              mycall.Post(MultiThreaded);

            Exit; // not sending the reponse from here!
            end;
          on E:Exception do
            with TRtcValue.Create do
              try
                asException:=E.Message;
                if assigned(call.output) then
                  to_Code(call.output)
                else
                  begin
                  if call.FMT=fmt_XMLRPC then
                    Write(toXMLrpcResponse)
                  else
                    begin
                    if assigned(call.crypt) and assigned(call.crypt.Write) then
                      begin
                      temp:=asCode;
                      CryptWrite(call.crypt,temp);
                      Write(temp);
                      end
                    else
                      Write(asCode);
                    end;
                  end;
              finally
                Free;
              end;
          end;

        {$IFDEF COMPRESS}
        if assigned(call.output) then
          begin
          try
            code:=call.output.Get;
          finally
            call.output.Free;
            call.output:=nil;
            end;

          if length(code)<RTC_MIN_COMPRESS_SIZE then
            begin
            CryptWrite(call.crypt, code);
            Write(code);
            end
          else
            begin
            { we have stored uncompressed data in "output",
              now we need to compress it all and send it out. }

            case call.Compress of
              cFast: temp:=ZCompress_Str(code,zcFastest);
              cMax: temp:=ZCompress_Str(code,zcMax);
              else temp:=ZCompress_Str(code,zcDefault);
              end;

            if length(temp)>=length(code)-1 then
              begin
              temp:='';
              CryptWrite(call.crypt,code);
              Write(code);
              code:='';
              end
            else
              begin
              code:='';
              CryptWrite(call.crypt,temp);
              Write(temp);

              temp:=#0;
              CryptWrite(call.crypt,temp);
              Write(temp);
              temp:='';
              end;
            end;
          end;
        {$ENDIF}
        
        if assigned(call.crypt) and assigned(call.crypt.Write) then
          begin
          // Add control key to the end of our response
          temp:=call.crypt.ControlKey;
          CryptWrite(call.crypt, temp);
          Write(temp);
          temp:='';
          end;

        Flush;

      finally
        LeaveEvent;
        end;
      end;
  finally
    if assigned(call) then
      call.Free;
    if not released then Free;
    end;
  end;

initialization
randomize;
CS:=TRtcCritSec.Create;
List:=tBinList.Create(128);

finalization
KillObjs;

Garbage(List);
Garbage(CS);
end.
