{
  @html(<b>)
  Client Module for Remote Functions
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  This unit introduces @Link(TRtcClientModule), the client-side component for ENABLING remote functions.
  By using @Link(TRtcClientModule), you can easily call remote server functions and get results
  in form of objects, passed to the event handlers you define. Also, by assigning a
  @Link(TRtcFunctionGroup) component to your @Link(TRtcClientModule) component,
  server can (as a result of any remote function call from the client) return functions which
  will be executed on the client side before the result object is passed to the local event handler.
  Implementing a RTC Remote Function is as easy as writing a local function.
}

unit rtcCliModule;

{$INCLUDE rtcDefs.inc}

interface

uses
  Classes,
  Windows,
  SysUtils,

  rtcTimer,
  rtcInfo,
  rtcConn,
  rtcCrypt,
  rtcSyncObjs,

  rtcDataCli,
  rtcFunction,

{$IFDEF COMPRESS}
  rtcZLib,
{$ENDIF}

  rtcFastStrings,
  memObjList;

type
  // @exclude
  EPostInteractive = class(EAbort);

  // @exclude
  TRtcInteractiveResult = class
  public
    FEvent:TRtcResult;
    Data,Result:TRtcValue;

    destructor Destroy; override;
    end;

  { @abstract(Used to store all calls for a single Post from a ClientModule)

    @exclude }
  TRtcClientModuleCallsArray=class(TRtcArray)
  private
    FEvents:array of TRtcResult;

    function GetEvent(index: integer): TRtcResult;
    procedure SetEvent(index: integer; const _Value: TRtcResult);

  public
    constructor Create; override;
    destructor Destroy; override;

    property Event[index:integer]:TRtcResult read GetEvent write SetEvent;
    end;

  // @exclude
  TRtcCryptClient=class(TRtcObject)
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

  // @exclude
  TRtcClientModuleData=class
  public
    FRequest:TRtcClientRequest;
    FData:TRtcValue;
    FPostLevel:integer;
    FCalls:TRtcClientModuleCallsArray;

    constructor Create; virtual;
    destructor Destroy; override;
    end;

  { @abstract(Use to call remote functions and receive their results)

    ClientModule is used to prepare remote function calls, post them to
    the server, accept server's response and call local event handlers with
    the result received for each call. You can post a single call or multiple
    function calls with one request, or even use a timer-based trigger to post
    all the requests prepared until now. }
  TRtcClientModule=class(TRtcAbsDataClientLink)
  private
    FMyData:TObjList;
    FMainThrData:TRtcClientModuleData;

    FIntCS:TRtcCritSec;
    FCS:TRtcCritSec;

    FIntTimer:TRtcTimer;
    FIntRes:TList;

    FFunctions:TRtcFunctionGroup;
    FRelease:boolean;

    FModuleFileName:string;
    FModuleHost:string;
    FAutoRepost:integer;
    FAutoSessions: boolean;

    FOnBeginRequest: TRtcNotifyEvent;
    FOnResponseAbort: TRtcNotifyEvent;
    FOnResponseDone: TRtcNotifyEvent;
    FOnResponseReject: TRtcNotifyEvent;
    FOnConnectLost: TRtcNotifyEvent;
    FOnSessionExpired: TRtcNotifyEvent;
    FOnRepostCheck: TRtcNotifyEvent;

    FOnSessionOpen: TRtcNotifyEvent;
    FOnSessionClose: TRtcNotifyEvent;

    FAutoEncrypt: integer;
    FForceEncrypt: boolean;
    FOnResponseError: TRtcNotifyEvent;
    FSecureKey: string;

    {$IFDEF COMPRESS}
    FCompress: TRtcCompressLevel;
    {$ENDIF}

    FOnNoEncryption: TRtcNotifyEvent;
    FOnNeedEncryption: TRtcNotifyEvent;
    FOnWrongEncryption: TRtcNotifyEvent;

    FOnLoginResult: TRtcResultEvent;
    FOnLoginAborted: TRtcResultEvent;

    FHyperThreading: boolean;
    FDataFormat: TRtcDataFormat;
    FOnLogin: TRtcFunctionPrepareEvent;

    FResetLogin,
    FAutoLogin: boolean;
    FLoginResult: TRtcResult;

    function CheckMyData:TRtcClientModuleData;
    function GetMyData:TRtcClientModuleData;
    procedure ClearMyData;

    function IsRemoteCallRequest(Sender:TRtcConnection):boolean;

    procedure NotifyResultAborted(Sender:TRtcConnection);

    procedure Response_Problem(Sender:TRtcConnection);

    procedure Call_SessionExpired(Sender:TRtcConnection);
    procedure Call_NoEncryption(Sender:TRtcConnection);
    procedure Call_NeedEncryption(Sender:TRtcConnection);
    procedure Call_WrongResponse(Sender:TRtcConnection);
    procedure Call_WrongEncryption(Sender:TRtcConnection);

    function GetCrypt(Session:TRtcSession):TRtcCryptClient;
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

    procedure PostInteractiveResult(Event:TRtcResult; Data,Result:TRtcValue);

    procedure DoInteractiveResult;

    function GetData: TRtcValue;
    function GetPostLevel: integer;
    function GetRequest: TRtcClientRequest;

    {$IFDEF COMPRESS}
    procedure SetCompress(const Value: TRtcCompressLevel);
    procedure SetDataFormat(const Value: TRtcDataFormat);
    {$ENDIF}

    procedure SetAutoLogin(const Value: boolean);

    procedure LoginCall(ResultHandler: TRtcResult; Sender:TRtcConnection; Insert:boolean=False); virtual;

    procedure Call_LoginResult(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);
    procedure Call_LoginAborted(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);

  protected
    // @exclude
    procedure Call_BeginRequest(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseData(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseAbort(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseReject(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); override;

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
    procedure Call_ConnectLost(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_RepostCheck(Sender:TRtcConnection); override;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    // You can call this method from Interactive result function to destroy the ClientModule object.
    procedure Release;

    { Use this method when you want to force the next remote call to make a new login attempt,
      even if the component thinks the user is already logged in. } 
    procedure ResetLogin;

    { If you want to post multiple calls in one request (and not each in its own request),
      you can use the "StartCalls" methods to open a new "call transaction", which is closed
      by a call to "Post". Each "StartCalls" has to be closed by a call to "Post". Using
      StartCalls/Call/Call/Post, you can combine a number of remote calls in one request.
      Another thing you don't have to worry about when using StartCalls/Post is clearing
      of the Data object in case of an exception during Data preparation. }
    procedure StartCalls; virtual;

    { After you have used the "Data" property to prepare the objects and functions
      you want to send to the server, use the "Call" method to define the event handler
      which has to receive the result after the data has been posted to the server.
      If you are not interested in the result values of your request, but just
      want to send this to the server as a "procedure call" and ignore the result,
      you can use "Call(nil)" and any result received will be simply thrown away.
      A result WILL be received in any case, which ensures that the function was executed.
      But, even in case of an exception, result with be ignored. @html(<br><br>)

      After "Call()", an object will be created to hold the prepared "Data" with a
      pointer to the TRtcResult component, after which the Data property will be cleared,
      so that you can prepare and Call new remote functions immediatelly. @html(<br><br>)

      If you are calling a remote function from inside other remote functions event,
      "FromInsideEvent" parameter has to be TRUE to avoid memory consumption and
      you should pass the Sender:TRtcConnection parameter to the Call() method.

      If you didn't start a separate call transaction using "StartCalls", your call will be
      automaticaly posted to the server in a single request. To post multiple calls in
      one request, simply call "StartCalls" before, prepare "Data" before each "Call" and
      after the last call, use the "Post" method to send everything out. }
    procedure Call(ResultHandler:TRtcResult; FromInsideEvent:boolean=False; Sender:TRtcConnection=nil); overload;

    { "Post" will decrease the call transaction level which was increased by a "StartCalls"
      and if the last StartCalls was closed by calling this Post, an object will be created
      to hold the prepared Request info and a list of remote calls will be sent to the server
      if connection with server is established. @html(<br><br>)

      When posting from inside a RTC event or a remote function,
      "FromInsideEvent" parameter has to be TRUE to avoid memory consumption and
      you should pass the Sender:TRtcConnection parameter to the Post() method.

      Events assigned to this TRtcClientModule will not be removed nor cleared,
      so you can define them at design-time and not worry about them at runtime. }
    procedure Post(FromInsideEvent:boolean=False; Sender:TRtcConnection=nil); virtual;

    { ONLY use this Request property if you need to prepare a request BEFORE posting.
      @html(<br>)
      DO NOT directly use this property when processing function calls.
      After a request has been posted, it is moved to the DataClient,
      so you can access it (from events) using Sender's Request property. }
    property Request:TRtcClientRequest read GetRequest;

    { To prepare a remote call, use this "Data" property to define and/or assign object(s)
      you want to send to the server. After you have defined the "Data" property to hold
      all information you want to send to the server, use the "Call" method to store that
      data and define the event handler to be used when a result is received. @html(<br>)
      ONLY use this Data property to prepare data for a remote call. @html(<br>)
      DO NOT directly use this property when processing the received result. }
    property Data:TRtcValue read GetData;

    { Using this property, you can check at which Calls level your ClientModule currently is.
      CallsLevel will be 0 outside of StartCalls, increased by 1 after each StartCalls
      and decreased by 1 after each Post. }
    property CallsLevel:integer read GetPostLevel;

  published
    {$IFDEF COMPRESS}
    { Use this property to define what compression level you want to use when sending
      data from Client to Server. Default Compression value is "cNone" (no compression).
      You can use different compression levels between client and server (for example,
      fast or no compression from client and max from server). If your client has to
      work with servers which don't support compression, you have to use "cNone". }
    property Compression:TRtcCompressLevel read FCompress write SetCompress default cNone;
    {$ENDIF}

    { Use this property to define what data format you want to use when sending data from
      Client to Server. If your client will only be talking to a Server written using RTC
      components, it is recommended to use "fmt_RTC", which upports all RTC data types, automatic
      compression, automatic encryption and automatic Sessions, as well as nested function calls
      and sending multiple function calls in a single request. On the other hand, if your client
      has to communicate with a Server which does NOT support the RTC format, you can use
      the "fmt_XMLRPC" format, which will work with Servers implementing the XML-RPC format. @html(<br><br>)
      NOTE: Since advanced functionality like automatic Compression, auto Encryption
      and automatic Sessions are native to the RTC format and other Servers do not implement it,
      those advanced properties will be ignored and assumed to be FALSE with all formats other
      then the "fmt_RTC" format. If you need those advanced options, use the "fmt_RTC" format. }
    property DataFormat:TRtcDataFormat read FDataFormat write SetDataFormat default fmt_RTC;

    { If you want to enable the possibility to use this Client Module to send remote function
      calls from multiple threads AT THE SAME TIME, where this component will acs as if
      it were X components, one for each thread, simply set HyperThreading to TRUE. @html(<br><br>)

      This is useful if you need to send remote function calls to another Server from
      within your Server running in multi-threaded mode and want to use only one set of
      rtcHttpClient/rtcClientModule components for all clients connected to your Server.
      Even in HyperThreading mode, only properties and methods needed to prepare and post remote
      function calls (Data, Request, Call, StartCalls, PostLevel and Post) will use a separate
      copy for each thread, while all other properties and methods exist only once for all threads,
      so don't try to modify them while your application is actively using the component in
      multi-threaded mode. @html(<br><br>)

      Leave HyperThreading as FALSE to use this component "the stadard way" (for example,
      when you're writing a client application where remote calls are done from the main thread
      or if you are creating a separate component for every thread that needs it). }
    property HyperThreading:boolean read FHyperThreading write FHyperThreading default False;

    { Set this property to a value other than 0 if you want to use automatic Encryption
      with a random generated key of "EncryptKey" bytes. One byte stands for
      encryption strength of 8 bits. For strong 256-bit encryption, use 32 bytes. @html(<br><br>)

      The final encryption key is combined from a client-side key and a key
      received from the server, where server decides about its encryption strength.
      If server doesn't support Encryption, data will not be encrypted,
      regardless of the value you use for AutoEncrypt. @html(<br><br>)

      EncryptionKey uses sessions to keep the encryption keys. When you set EncryptionKey
      to a value other than 0 (turn it ON), AutoSessions will be set to TRUE.
      Also, setting AutoSessions to FALSE will set EncryptionKey to 0 (OFF). }
    property EncryptionKey:integer read FAutoEncrypt write SetAutoEncrypt default 0;

    { If you need a 100% secure connection, define a Secure Key string
      (combination of letters, numbers and special characters) for each
      ServerModule/ClientModule pair, additionally to the EncryptionKey value.
      ClientModule will be able to communicate with the ServerModule ONLY if
      they both use the same SecureKey. Default value for the SecureKey is
      an empty string (means: no secure key). @html(<br><br>)

      SecureKey will be used in the encryption initialisation handshake,
      to encrypt the first key combination sent by the ClientModule.
      Since all other data packages are already sent using some kind of encryption,
      by defining a SecureKey, you encrypt the only key part which would have
      been sent out without special encryption. }
    property SecureKey:string read FSecureKey write FSecureKey;

    { Setting this property to TRUE will tell the ClientModule to work with the
      Server ONLY if Server supports encryption. If AutoEncryptKey is > 0 and
      server doesn't support encryption, function calls will not be passed to
      the server and any response coming from the server will be rejected, until
      server enables encryption. }
    property ForceEncryption:boolean read FForceEncrypt write SetForceEncrypt default False;

    { Set this property to TRUE if you want ClientModule to request a new session
      automatically if the Session.ID is not set when posting a request.
      Session handling is built into the ClientModule and uses Request.Params to
      send the Session.ID to the server and Response.Cookie['ID'] to receive a
      new session ID from the server and initialize the session object. @html(<br><br>)

      Since session ID's are communicated automaticaly by the ClientModule and
       ServerModule components, all TRtcFunction and TRtcResult components used by
      this ClientModule will have direct access to the session object.
      When AutoSessions is set to true, a new session will be requested if
      no session exists or when a session expires. @html(<br><br>)

      When AutoSessions is FALSE, you have to request a new session by calling
      a remote server function to generate a new session and return the session ID. }
    property AutoSessions:boolean read FAutoSessions write SetAutoSessions default false;

    { Set this property to a value other than 0 (zero) if you want the ClientModule to
      auto-repost any request up to "AutoRepost" times, in case the connection gets lost
      while sending data to server or receiving data from server.
      If value is lower than zero, requests will be reposted unlimited number of times. }
    property AutoRepost:integer read FAutoRepost write FAutoRepost default 0;

    { Set this property to TRUE if you want to enable the use of the
      OnLogin, OnLoginResult and OnLoginAborted events to implement automatic login. }
    property AutoLogin:boolean read FAutoLogin write SetAutoLogin default false;

    { "Request.Host" will be assigned this property before sending the request out. @html(<br>)
      It is not necessary to set this property if your server's ServerModule component
      left its ModuleHost property blank and there's no remote Functions used by that
      ServerModule which would explicitly use the "Request.Host" header. On the other hand,
      for servers which serve multiple hosts, mostly where ServerModule has assigned its
      ModuleHost property, it is very important to set this ClientModule's ModuleHost
      property to the appropriate host name. }
    property ModuleHost:string read GetModuleHost write SetModuleHost;
    { To be able to call remote functions, this ClientModule's ModuleFileName
      property has to be identical to the "ModuleFileName" property of the ServerModule
      which you want to use. "Request.FileName" will be assigned this property before
      sending any request out, so you won't be preparing the Request headers manualy. @html(<br>)
      All data (parameters and function calls) will be passed to the server module through
      request's Content body, so that ServerModule won't need to check the request headers
      for anything else than it's FileName to know if the request is directed to it. }
    property ModuleFileName:string read GetModuleFileName write SetModuleFileName;
    { Set this property to tell the RtcClientModule to use this TRtcFunctionGroup
      component to execute all functions received as a response from server, for
      any request sent from this TRtcClientModule component. }
    property FunctionGroup:TRtcFunctionGroup read GetFunctionGroup write SetFunctionGroup;

    { This event will be called if your SecretKey does not match the SecretKey
      specified by the ServerModule you're connecting to.
      On this event, you can decide not to work with that server (Response.Reject or Disconnect),
      or to update your SecretKey property to mirror the SercetKey of your ServerModule. }
    property OnEncryptWrongKey:TRtcNotifyEvent read FOnWrongEncryption write FOnWrongEncryption;
    { This event will be called if your EncryptionKey>0 and ForceEncryption=TRUE,
      but the Server says it does not support encryption for this ServerModule.
      On this event, you can decide not to work with that server (Response.Reject or Disconnect),
      or to set your ForceEncryption property to False and repost the request. }
    property OnEncryptNotSupported:TRtcNotifyEvent read FOnNoEncryption write FOnNoEncryption;
    { This event will be called if your EncryptionKey=0,
      but the Server wants to ForceEncryption for this ServerModule.
      On this event, you can decide to not work with that server (Response.Reject or Disconnect),
      or to activate encryption by setting the EncryptionKey. }
    property OnEncryptRequired:TRtcNotifyEvent read FOnNeedEncryption write FOnNeedEncryption;

    { This event will be called if we receave invalid response from the Server,
      which could mean that our Client or our Server are not up to date. }
    property OnResponseError:TRtcNotifyEvent read FOnResponseError write FOnResponseError;
    { This event will be called if you have called a remote function with a Session ID that
      has expired. You can choose to clear the local Session object and Repost the request
      with an empty session ID to receive a new session ID, or reject the Request.
      If you do not implement this event, Session ID will be cleared and the request
      will be reposted, so your client will receive a new Session ID. }
    property OnSessionExpired:TRtcNotifyEvent read FOnSessionExpired write FOnSessionExpired;
    { This event will be called after ClientModule component has prepared the request for sending,
      but before the request has been sent out (no writing has been done yet).
      You can use this event to check or update the request object before it is sent out. @html(<br>)
      This event does not have to be defined for the ClientModule to work. }
    property OnBeginRequest:TRtcNotifyEvent read FOnBeginRequest write FOnBeginRequest;
    { This event will be called after the last DataReceived event for this request,
      read after the request has been sent out and a complete response was received (Response.Done). @html(<br>)
      This event does not have to be defined for the ClientModule to work. }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { This event will be called after the OnConnectLost, OnConnectFail and OnConnectError events,
      if the request was NOT marked for reposting. }
    property OnRepostCheck:TRtcNotifyEvent read FOnRepostCheck write FOnRepostCheck;
    { This event will be called after the OnRepostCheck event, if the request was not marked for reposting.
      If this event gets triggered, it means that there is a major problem with the server and
      user has to be notified about that problem and consulted about further actions. }
    property OnResponseAbort:TRtcNotifyEvent read FOnResponseAbort write FOnResponseAbort;
    { This event will be called after the response has been rejected by calling "Response.Reject" }
    property OnResponseReject:TRtcNotifyEvent read FOnResponseReject write FOnResponseReject;

    { This event will be called after a new Session has been opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { This event will be called before an existing Session is going to be closed. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;

    { This event will be mapped as @Link(TRtcClient.OnConnectLost) event
      to the assigned DataClient component and called if your connection gets
      closed while you are still processing your request. }
    property OnConnectLost:TRtcNotifyEvent read FOnConnectLost write FOnConnectLost;

    { Use this event to implement automatic user login. Set AutoLogin to TRUE and
      this event will be fired immediately after the initial connection handshake.
      To prepare the remote function, use the "Data" object passed as a parameter.
      After the function call has returned, the OnLoginResult event will be triggered.
      If there was an error and the request was aborted, OnLoginAborted will be called. }
    property OnLogin:TRtcFunctionPrepareEvent read FOnLogin write FOnLogin;

    { Use this event to implement automatic user login. See "OnLogin" for more info. }
    property OnLoginResult:TRtcResultEvent read FOnLoginResult write FOnLoginResult;

    { Use this event to implement automatic user login. See "OnLogin" for more info. }
    property OnLoginAborted:TRtcResultEvent read FOnLoginAborted write FOnLoginAborted;
    end;

{ Call this procedure if user interaction is required anywhere inside your result event.
  When this procedure is called, the event will be posted to the main thread outside of
  the client connection's context, so that the connection can continue functioning
  and receiving new data, without being blocked by a window waiting for user input.
  Without using "PostInteractive" to post the event, the connection would be blocked until
  the event returns. This could take several minutes if user doesn't notice your window,
  which would most likely result in a session timeout on the server, so the user would
  be automaticaly logged out after he responded to your questions. @html(<br><br>)

  Even though events are posted outside of the connection context, a mechanism integrated
  into TRtcClientModule will take care that all events posted interactively from the same
  ClientModule's result event, do not overlap or get called in a different order. So,
  if you need your result events to block any upcoming resuts, you can post all your
  dependent events interactively to ensure they will get called AFTER the user responded,
  while the connection continues receiving new data from the server and keeps the session alive. @html(<br><br>)

  NOTE: This procedure works only when called from inside TRtcResult event
  which was triggered by TRtcClientModule to return a result from a remote function call.
  When a function is called asynchtonously outside of the connection context,
  Sender parameter is NIL. This has two reasons: @html(<br>)
  1. You can check "Sender" to know if your event would block a connection. @html(<br>)
  2. You can not expect the connection to remain in the same state forever and you
     can not use the connection directly from an interactive event. }
procedure PostInteractive;

implementation

procedure PostInteractive;
  begin
  raise EPostInteractive.Create('');
  end;

{ TRtcClientModuleData }

constructor TRtcClientModuleData.Create;
  begin
  inherited;
  FRequest:=nil;
  FData:=nil;
  FCalls:=nil;
  FPostLevel:=0;
  end;

destructor TRtcClientModuleData.Destroy;
  begin
  if assigned(FRequest) then
    begin
    FRequest.Free;
    FRequest:=nil;
    end;
  if assigned(FData) then
    begin
    FData.Free;
    FData:=nil;
    end;
  if assigned(FCalls) then
    begin
    FCalls.Free;
    FCalls:=nil;
    end;
  inherited;
  end;

{ TRtcClientModule }

constructor TRtcClientModule.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FHyperThreading:=False;

  FIntCS:=TRtcCritSec.Create;
  FIntRes:=TList.Create;
  FIntTimer:=nil;

  FRelease:=False;
  FFunctions:=nil;
  FModuleFileName:='';
  FModuleHost:='';
  FAutoRepost:=0;

  FCS:=TRtcCritSec.Create;
  FMyData:=tObjList.Create(32);
  FMainThrData:=TRtcClientModuleData.Create;

  FLoginResult:=TRtcResult.Create(nil);
  FLoginResult.OnReturn:=Call_LoginResult;
  FLoginResult.RequestAborted:=Call_LoginAborted;
  end;

destructor TRtcClientModule.Destroy;
  begin
  FFunctions:=nil;
  FModuleFileName:='';
  FModuleHost:='';

  if assigned(FMainThrData) then
    begin
    FMainThrData.Free;
    FMainThrData:=nil;
    end;
  if assigned(FMyData) then
    begin
    FMyData.Free;
    FMyData:=nil;
    end;

  FLoginResult.Free;

  FIntRes.Free;
  FIntCS.Free;
  FCS.Free;
  inherited;
  end;

function TRtcClientModule.GetCrypt(Session:TRtcSession): TRtcCryptClient;
  begin
  Result:=TRtcCryptClient(Session.Obj[ModuleHost+ModuleFileName+':$Crypt$']);
  end;

procedure TRtcClientModule.NewCrypt(Session:TRtcSession);
  begin
  if TRtcCryptClient(Session.Obj[ModuleHost+ModuleFileName+':$Crypt$'])<>nil then
    TRtcCryptClient(Session.Obj[ModuleHost+ModuleFileName+':$Crypt$']).Init
  else
    Session.Obj[ModuleHost+ModuleFileName+':$Crypt$']:=TRtcCryptClient.Create;
  end;

procedure TRtcClientModule.DelCrypt(Session:TRtcSession);
  begin
  if TRtcCryptClient(Session.Obj[ModuleHost+ModuleFileName+':$Crypt$'])<>nil then
    begin
    TRtcCryptClient(Session.Obj[ModuleHost+ModuleFileName+':$Crypt$']).Free;
    Session.Obj[ModuleHost+ModuleFileName+':$Crypt$']:=nil;
    end;
  end;

procedure TRtcClientModule.Call_ConnectLost(Sender: TRtcConnection);
  begin
  if assigned(FOnConnectLost) then
    if AutoSyncEvents then
      Sender.Sync(FOnConnectLost)
    else
      FOnConnectLost(Sender);
  end;

function RandomKey(len:integer):string;
  var
    a:integer;
  begin
  SetLength(Result,len);
  for a:=1 to len do
    Result[a]:=Char(random(256));
  end;

procedure CryptRead(Crypt:TRtcCryptClient; var Data:string);
  begin
  if assigned(Crypt) and assigned(Crypt.Read) then
    Crypt.Read.DeCrypt(Data);
  end;

procedure CryptWrite(Crypt:TRtcCryptClient; var Data:string);
  begin
  if assigned(Crypt) and assigned(Crypt.Write) then
    Crypt.Write.Crypt(Data);
  end;

function GenerateControlKey(var Counter:integer):string;
  var
    len,a,b,c:integer;
  begin
  Inc(Counter);
  if Counter>99 then Counter:=1;

  len:=5+random(5);
  SetLength(Result,len+4);
  b:=(10-len)*9+8;
  for a:=5 to len+4 do
    begin
    c:=random(10); Inc(b,c);
    Result[a]:=Char(c+Ord('0'));
    end;
  Result[1]:=Char(b div 10 + Ord('0'));
  Result[2]:=Char(b mod 10 + Ord('0'));
  Result[3]:=Char(Counter div 10 + Ord('0'));
  Result[4]:=Char(Counter mod 10 + Ord('0'));
  end;

procedure TRtcClientModule.Call_BeginRequest(Sender: TRtcConnection);
  var
    idx:integer;
    MyCalls:TRtcClientModuleCallsArray;
    compressed:boolean;
    code,temp:string;
    output:TRtcHugeString;
    crypt:TRtcCryptClient;
    DataReq:TRtcDataRequestInfo;
    MyRequest:TRtcClientRequest;
    obj:TRtcValueObject;
  begin
  if FResetLogin then
    begin
    FResetLogin:=False;
    TRtcDataClient(Sender).Session.Close;
    end;
    
  if (FDataFormat=fmt_RTC) and (EncryptionKey>0) then
    begin
    with TRtcDataClient(Sender) do
      begin
      crypt:=GetCrypt(Session);
      if (Request.Query['ACTION']='HELLO') then // Sending HELLO to the Server
        begin
        if Session.ID<>'' then
          Request.Query['ID']:=Session.ID
        else
          Request.Query['ID']:='';

        // Initialize encryption for this ClientModule
        NewCrypt(Session);
        crypt:=GetCrypt(Session);

        // Generate Client-Hello
        crypt.ClientHello:=RandomKey(EncryptionKey);

        { Generate randoml control number to add at the end of the request,
          so we can check if the response is correctly encrypted. }
        crypt.ControlKey := GenerateControlKey(crypt.ControlCounter);

        code:=crypt.ClientHello+#13+crypt.ControlKey;

        if SecureKey<>'' then
          begin
          with TRtcCrypt.Create do
            begin
            Key:=SecureKey;
            Crypt(code);
            Free;
            end;
          end;

        // Send ClientHello + ControlKey
        Write(code);
        Exit;
        end
      else if {(Session.ID='') or} (crypt=nil) or not crypt.HaveHello then
        begin
        if ModuleFileName='' then
          raise Exception.Create('Module FileName is undefined. Can not Post the request.');

        if (Request.Reposted>1) and (Session.ID<>'') then
          Session.Init;

        MyRequest:=TRtcClientRequest.Create;
        MyRequest.Method:='POST';
        MyRequest.FileName:=ModuleFileName;
        MyRequest.Query['ACTION']:='HELLO';
        if ModuleHost<>'' then
          MyRequest.Host:=ModuleHost;

        DataReq:=TRtcDataRequestInfo.Create;
        DataReq.Request:=MyRequest;
        DataReq.Events:=Self;
        try
          InsertRequest(DataReq);
        except
          DataReq.Events:=nil;
          DataReq.Free;
          end;
        Exit;
        end
      else if (Request.Query['ACTION']='START') then
        begin
        if Session.ID<>'' then
          Request.Query['ID']:=Session.ID
        else
          Request.Query['ID']:='';

        // Generate Client-Key
        crypt.ClientKey:=RandomKey(EncryptionKey);

        { Generate a random control number to add at the end of the request,
          so we can check if the response is correctly encrypted. }
        crypt.ControlKey := GenerateControlKey(crypt.ControlCounter);

        code:=crypt.ClientKey+#13+crypt.ControlKey;
        CryptWrite(crypt, code);

        // Send ClientKey + ControlKey
        Write( code );
        Exit;
        end
      else if not crypt.HaveStart then
        begin
        if ModuleFileName='' then
          raise Exception.Create('Module FileName is undefined. Can not Post the request.');

        MyRequest:=TRtcClientRequest.Create;
        MyRequest.Method:='POST';
        MyRequest.FileName:=ModuleFileName;
        MyRequest.Query['ACTION']:='START';
        if ModuleHost<>'' then MyRequest.Host:=ModuleHost;

        DataReq:=TRtcDataRequestInfo.Create;
        DataReq.Request:=MyRequest;
        DataReq.Events:=Self;
        try
          InsertRequest(DataReq);
        except
          DataReq.Events:=nil;
          DataReq.Free;
          end;
        Exit;
        end;
      end;
    end;

  with TRtcDataClient(Sender) do
    if Session.ID<>'' then
      Request.Query['ID']:=Session.ID
    else if AutoSessions then
      Request.Query['ID']:='NEW'
    else
      Request.Query['ID']:='';

  if FAutoLogin then
    if not assigned(FOnLogin) then
      raise Exception.Create('OnLogin event missing for ClientModule "'+ModuleFileName+'", but AutoLogin is TRUE.')
    else if not TRtcDataClient(Sender).Session.asBoolean['ClientModule.Login$'] and
            not TRtcDataClient(Sender).Request.Info.asBoolean['ClientModule.Login$'] then
      begin
      LoginCall(FLoginResult,Sender,True);
      Exit;
      end;

  if assigned(Link) then
    Link.Call_BeginRequest(Sender)
  else if assigned(Client) then
    Client.CallBeginRequest;

  if not TRtcDataClient(Sender).RequestInserted and
     not TRtcDataClient(Sender).Request.Skipped and
     not TRtcDataClient(Sender).Response.Rejected then
    begin
    if assigned(FOnBeginRequest) then
      if AutoSyncEvents then
        Sender.Sync(FOnBeginRequest)
      else
        FOnBeginRequest(Sender);

    if not TRtcDataClient(Sender).RequestInserted and
       not TRtcDataClient(Sender).Request.Skipped and
       not TRtcDataClient(Sender).Response.Rejected then
      begin
      with TRtcDataClient(Sender) do
        begin
        MyCalls:=TRtcClientModuleCallsArray(Request.Info.Obj['ClientModule.Call$']);
        if not assigned(MyCalls) then
          raise Exception.Create('Internal error! ClientModule objects undefined!');

        if FDataFormat=fmt_RTC then
          crypt:=GetCrypt(Session)
        else
          crypt:=nil;

        compressed:=False;

        {$IFDEF COMPRESS}
        if (FDataFormat=fmt_RTC) and (FCompress<>cNone) then
          begin
          if MyCalls.Count=1 then
            code:=MyCalls.asCode[0]
          else
            begin
            output:=TRtcHugeString.Create;
            try
              for idx:=0 to MyCalls.Count-1 do
                output.Add(MyCalls.asCode[idx]);
              code:= output.Get;
            finally
              output.Free;
              end;
            end;

          if length(code)<RTC_MIN_COMPRESS_SIZE then
            begin
            CryptWrite(crypt, code);
            Write(code);
            end
          else
            begin
            { Using compression,
              need to compress all data now. }
            case FCompress of
              cFast: temp:=ZCompress_Str(code,zcFastest);
              cMax: temp:=ZCompress_Str(code,zcMax);
              else temp:=ZCompress_Str(code,zcDefault);
              end;
            // use compressed version ONLY if smaller than uncompressed
            if length(temp)<length(code)-1 then
              begin
              code:='';
              CryptWrite(crypt, temp);
              Write(temp);

              temp:=#0;
              CryptWrite(crypt, temp);
              Write(temp);
              temp:='';

              compressed:=True;
              end
            else
              begin
              temp:='';
              CryptWrite(crypt, code);
              Write(code);
              code:='';
              end;
            end;
          end
        else
        {$ENDIF}
          begin
          if FDataFormat=fmt_RTC then
            begin
            if not assigned(crypt) then
              begin
              for idx:=0 to MyCalls.Count-1 do
                begin
                code:=MyCalls.asCode[idx];
                Write(code);
                end;
              end
            else
              begin
              for idx:=0 to MyCalls.Count-1 do
                begin
                code:=MyCalls.asCode[idx];
                CryptWrite(crypt, code);
                Write(code);
                end;
              end;
            end
          else
            begin
            for idx:=0 to MyCalls.Count-1 do
              begin
              obj:=MyCalls.asObject[idx];
              if not assigned(obj) then
                raise Exception.Create('XML-RPC Error! Can not make a "NIL" call!')
              else
                code:=obj.toXMLrpcRequest;
              Write(code);
              end;
            end;
          end;

        if (FDataFormat=fmt_RTC) then
          begin
          if assigned(crypt) and assigned(crypt.Write) then
            begin
            { Add random control number at the end of the request,
              so we can check if the response is correctly encrypted. }
            crypt.ControlKey := GenerateControlKey(crypt.ControlCounter);

            if not compressed then
              code:=#13+crypt.ControlKey
            else // #0 was allready added
              code:=crypt.ControlKey; // we just need to add the control number

            CryptWrite(crypt, code);
            Write(code);
            end;
          end;

        code:='';
        end;
      end;
    end;
  end;

procedure TRtcClientModule.Call_DataReceived(Sender: TRtcConnection);
  var
    idx:integer;
    code:string;
    at:integer;
    MyTemp:TRtcValue;
    crypt:TRtcCryptClient;
    c1,c2:string;
    c3:integer;
    MyData,MyResult:TRtcValueObject;
    MyCalls:TRtcClientModuleCallsArray;
  begin
  with TRtcDataClient(Sender) do if Response.Done then
    if AutoSyncEvents and not Sender.inMainThread then
      {$IFDEF FPC}
      Sender.Sync(@Call_DataReceived)
      {$ELSE}
      Sender.Sync(Call_DataReceived)
      {$ENDIF}
    else if Response.StatusCode=410 then // Status 410 = Gone: Session ID invalid, clear local Session info.
      begin
      Call_SessionExpired(Sender);
      end
    else if Response.StatusCode=412 then // Status 412 = Precondition Failed: Encryption required
      begin
      Call_NeedEncryption(Sender);
      end
    else if Response.StatusCode=409 then // Status 409 = Conflict: Wrong Encryption Key
      begin
      Call_WrongEncryption(Sender);
      end
    else if Response.StatusCode<>200 then // Accept only responses with status 200 OK.
      begin
      Call_WrongResponse(Sender);
      end
    else if (EncryptionKey>0) and
            (Request.Query['ACTION']='HELLO') then
      begin
      // Prepare Session
      if (Session.ID='') then
        begin
        if (Response.Cookie['ID']='') then
          begin
          if ForceEncryption then
            begin
            Call_WrongResponse(Sender);
            Exit;
            end
          else
            begin
            NewCrypt(Session);
            crypt:=GetCrypt(Session);
            end;
          end
        else
          begin
          c1:=GetCrypt(Session).ClientHello;
          c2:=GetCrypt(Session).ControlKey;
          c3:=GetCrypt(Session).ControlCounter;
          Session.Open(Response.Cookie['ID']); // Set new Session ID
          NewCrypt(Session);
          crypt:=GetCrypt(Session);
          crypt.ClientHello:=c1;
          crypt.ControlKey:=c2;
          crypt.ControlCounter:=c3;
          end;
        end
      else if (Response.Cookie['ID']<>'') and
              (Session.ID<>Response.Cookie['ID']) then
        begin
        c1:=GetCrypt(Session).ClientHello;
        c2:=GetCrypt(Session).ControlKey;
        c3:=GetCrypt(Session).ControlCounter;
        Session.Open(Response.Cookie['ID']); // Set new Session ID
        NewCrypt(Session);
        crypt:=GetCrypt(Session);
        crypt.ClientHello:=c1;
        crypt.ControlKey:=c2;
        crypt.ControlCounter:=c3;
        end
      else
        crypt:=GetCrypt(Session);

      code:=Read;
      crypt.HaveHello:=True;

      if code='' then // Server does not support encryption
        begin
        if ForceEncryption then
          begin
          Call_NoEncryption(Sender);
          Exit;
          end
        else
          begin
          crypt.Init;
          crypt.HaveHello:=True;
          crypt.HaveStart:=True;
          end;
        end
      else if length(code)<=length(crypt.ControlKey) then // Wrong response from server
        begin
        Call_WrongEncryption(Sender);
        Exit;
        end
      else
        begin
        // Prepare the Encryption object for Reading
        if assigned(crypt.Read) then
          crypt.Read.Free;

        crypt.Read:=TRtcCrypt.Create;
        crypt.Read.Key:=crypt.ClientHello;

        // DeCrypt Server-Hello + Client-Hello
        CryptRead(crypt, code);

        // Check if response ends with sent control key
        if Copy(code, length(code)-length(crypt.ControlKey)+1, length(crypt.ControlKey))<>
           crypt.ControlKey then
          begin
          Call_WrongEncryption(Sender);
          Exit;
          end
        else
          Delete(code, length(code)-length(crypt.ControlKey)+1, length(crypt.ControlKey));

        crypt.ServerHello:=code;

        // Prepare the Encryption object for Writing
        if assigned(crypt.Write) then crypt.Write.Free;
        crypt.Write:=TRtcCrypt.Create;
        crypt.Write.Key:=crypt.ServerHello;
        end;
      end
    else if (EncryptionKey>0) and
            (Request.Query['ACTION']='START') then
      begin
      crypt:=GetCrypt(Session);

      code:=Read;
      crypt.HaveStart:=True;

      if code='' then // Server canceled encryption
        begin
        if ForceEncryption then
          begin
          Call_NoEncryption(Sender);
          Exit;
          end
        else
          begin
          crypt.Init;
          crypt.HaveHello:=True;
          crypt.HaveStart:=True;
          end;
        end
      else if length(code)<=length(crypt.ControlKey) then // Wrong response from server
        begin
        Call_WrongEncryption(Sender);
        Exit;
        end
      else
        begin
        // Set new Reading Key
        crypt.Read.Key:=crypt.ClientKey + crypt.ServerHello;

        // DeCrypt response: Server-Key + Client-Key
        CryptRead(crypt, code);

        // Check if response ends with sent Control Key
        if Copy(code, length(code)-length(crypt.ControlKey)+1, length(crypt.ControlKey))<>
           crypt.ControlKey then
          begin
          Call_WrongEncryption(Sender);
          Exit;
          end
        else
          Delete(code, length(code)-length(crypt.ControlKey)+1, length(crypt.ControlKey));

        crypt.ServerKey:=code;

        // Set new Writing Key
        crypt.Write.Key:=crypt.ServerKey + crypt.ClientHello;
        end;
      end
    else
      begin
      if Request.Query['ID']='NEW' then // we have requested a new session ID
        if Response.Cookie['ID']<>'' then
          Session.Open(Response.Cookie['ID']);

      MyCalls:=TRtcClientModuleCallsArray(Request.Info.Obj['ClientModule.Call$']);
      if not assigned(MyCalls) then
        raise Exception.Create('Internal error! ClientModule objects undefined!');

      code := Read;

      if FDataFormat=fmt_RTC then
        begin
        crypt:=GetCrypt(Session);
        if assigned(crypt) and assigned(crypt.Read) then
          begin
          if code='' then
            begin
            Call_WrongEncryption(Sender);
            Exit;
            end
          else if length(code)<=length(crypt.ControlKey) then // Wrong response from server
            begin
            Call_WrongEncryption(Sender);
            Exit;
            end
          else
            begin
            // Response has to END with our ControlKey
            CryptRead(crypt, code);
            if Copy(code, length(code)-length(crypt.ControlKey)+1, length(crypt.ControlKey))<>
               crypt.ControlKey then
              begin
              Call_WrongEncryption(Sender);
              Exit;
              end
            else
              begin
              {$IFDEF COMPRESS}
              // There is #0 before the Control Key, data is compressed
              if Copy(code,length(code)-length(crypt.ControlKey),1)=#0 then
                begin
                try
                  code:=ZDecompress_Str(code, length(code)-length(crypt.ControlKey)-1);
                except
                  on E:Exception do
                    begin
                    Call_WrongResponse(Sender);
                    Exit;
                    end;
                  end;
                end
              else
                SetLength(code, length(code)-length(crypt.ControlKey));
              {$ELSE}
              // There is #0 before the Control Key, data is compressed
              if Copy(code,length(code)-length(crypt.ControlKey),1)=#0 then
                begin
                Call_WrongResponse(Sender);
                Exit;
                end
              else
                SetLength(code, length(code)-length(crypt.ControlKey));
              {$ENDIF}
              end;
            end;
          end
        else if ForceEncryption and (EncryptionKey>0) then
          begin
          Call_NoEncryption(Sender);
          Exit;
          end
        else if Copy(code,length(code),1)=#0 then // compressed data
          begin
          {$IFDEF COMPRESS}
          try
            code:=ZDecompress_Str(code, length(code)-1);
          except
            on E:Exception do
              begin
              Call_WrongResponse(Sender);
              Exit;
              end;
            end;
          {$ELSE}
          Call_WrongResponse(Sender);
          Exit;
          {$ENDIF}
          end;
        end;

      idx:=0;
      at:=0;
      try
        if Code='' then
          raise Exception.Create('No data received.')
        else while at<length(Code) do
          begin
          case FDataFormat of
            fmt_XMLRPC: MyData:=TRtcValue.FromXMLrpc(code,at);
            else        MyData:=TRtcValue.FromCode(code,at);
            end;
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
                end;
              end;
            if MyData<>nil then
              begin
              if idx<MyCalls.Count then
                begin
                if assigned(MyCalls.Event[idx]) then
                  begin
                  if not (MyData is TRtcValue) then
                    begin
                    MyTemp:=TRtcValue.Create;
                    MyTemp.asObject:=MyData;
                    MyData:=MyTemp;
                    end;
                  try
                    MyCalls.Event[idx].
                        Call_Return(Sender,
                                    TRtcValue(MyCalls.asObject[idx]),
                                    TRtcValue(MyData));
                  except
                    on E:EPostInteractive do
                      begin
                      PostInteractiveResult(MyCalls.Event[idx],
                                            TRtcValue(MyCalls.asObject[idx]),
                                            TRtcValue(MyData));
                      MyCalls.asObject[idx]:=nil;
                      MyData:=nil;
                      end;
                    end;
                  end;
                end
              else
                raise Exception.Create('More Results received than Calls sent.');
              end
            else
              raise Exception.Create('Response missing a result.');
            Inc(idx);
          finally
            if assigned(MyData) then
              MyData.Free;
            end;
          end;
      except
        on E:Exception do
          begin
          Response.StatusCode:=0; // Internal exception
          Response.StatusText:=E.Message;
          Call_WrongResponse(Sender);
          end;
        end;
      end;
  end;

{$IFDEF COMPRESS}
procedure TRtcClientModule.SetCompress(const Value: TRtcCompressLevel);
  begin
  FCompress := Value;
  end;
{$ENDIF}

procedure TRtcClientModule.Call_DataOut(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcClientModule.Call_DataIn(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcClientModule.Call_DataSent(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcClientModule.Call_ReadyToSend(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcClientModule.Call_ResponseData(Sender: TRtcConnection);
  begin
  if not TRtcDataClient(Sender).Request.Skipped and
     not TRtcDataClient(Sender).Response.Rejected then
    if assigned(Link) then
      Link.Call_ResponseData(Sender)
    else if assigned(Client) then
      Client.CallResponseData;
  end;

procedure TRtcClientModule.Call_ResponseDone(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseDone) then
    if AutoSyncEvents then
      Sender.Sync(FOnResponseDone)
    else
      FOnResponseDone(Sender);

  if not TRtcDataClient(Sender).Request.Skipped and
     not TRtcDataClient(Sender).Response.Rejected then
    if assigned(Link) then
      Link.Call_ResponseDone(Sender)
    else if assigned(Client) then
      Client.CallResponseDone;
  end;

procedure TRtcClientModule.Call_RepostCheck(Sender: TRtcConnection);
  begin
  if ((AutoRepost<0) or (TRtcDataClient(Sender).Request.Reposted<AutoRepost)) then
    TRtcDataClient(Sender).Request.Repost;

  if not TRtcDataClient(Sender).Request.Reposting then
    begin
    if assigned(FOnRepostCheck) then
      FOnRepostCheck(Sender);

    if not TRtcDataClient(Sender).Request.Reposting then
      begin
      if assigned(Link) then
        Link.Call_RepostCheck(Sender)
      else if assigned(Client) then
        Client.CallRepostCheck;
      end;
    end;
  end;

procedure TRtcClientModule.Call_ResponseAbort(Sender: TRtcConnection);
  begin
  if IsRemoteCallRequest(Sender) then
    begin
    if assigned(FOnResponseAbort) then
      if AutoSyncEvents then
        Sender.Sync(FOnResponseAbort)
      else
        FOnResponseAbort(Sender);

    if not TRtcDataClient(Sender).Request.Reposting then
      begin
      if assigned(Link) then
        Link.Call_ResponseAbort(Sender)
      else if assigned(Client) then
        Client.CallResponseAbort;

      if not TRtcDataClient(Sender).Request.Reposting then
        if AutoSyncEvents then
          Sender.Sync(NotifyResultAborted)
        else
          NotifyResultAborted(Sender);
      end;
    end
  else
    begin
    if assigned(Link) then
      Link.Call_ResponseAbort(Sender)
    else if assigned(Client) then
      Client.CallResponseAbort;
    end;
  end;

procedure TRtcClientModule.Call_ResponseReject(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseReject) then
    if AutoSyncEvents then
      Sender.Sync(FOnResponseReject)
    else
      FOnResponseReject(Sender);

  if assigned(Link) then
    Link.Call_ResponseReject(Sender)
  else if assigned(Client) then
    Client.CallResponseReject;
  end;

procedure TRtcClientModule.Call_SessionClose(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionClose) then
    if AutoSyncEvents then
      Sender.Sync(FOnSessionClose)
    else
      FOnSessionClose(Sender);

  if assigned(Link) then
    Link.Call_SessionClose(Sender)
  else if assigned(Client) then
    Client.CallSessionClose;
  end;

procedure TRtcClientModule.Call_SessionOpen(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionOpen) then
    if AutoSyncEvents then
      Sender.Sync(FOnSessionOpen)
    else
      FOnSessionOpen(Sender);

  if assigned(Link) then
    Link.Call_SessionOpen(Sender)
  else if assigned(Client) then
    Client.CallSessionOpen;
  end;

procedure TRtcClientModule.Call(ResultHandler: TRtcResult; FromInsideEvent:boolean=False; Sender:TRtcConnection=nil);
  var
    idx:integer;
    myData:TRtcClientModuleData;
  begin
  myData:=CheckMyData;

  if myData=nil then
    raise Exception.Create('No data defined. Use the Data property before "Call".');
  if myData.FData=nil then
    raise Exception.Create('No data defined. Use the Data property before "Call".');
  if myData.FData.isNull then
    raise Exception.Create('No data defined. Use the Data property before "Call".');

  {if not assigned(ResultHandler) then
    raise Exception.Create('Can not use "Call" with NIL as parameter.');
  if not assigned(ResultHandler.OnReturn) then
    raise Exception.Create('OnReturn event undefined for given TRtcResult component.'); }

  with myData do
    begin
    if not assigned(FCalls) then
      FCalls:=TRtcClientModuleCallsArray.Create;

    // Add Data and ResultHandler to our list of calls
    idx:=FCalls.Count;
    FCalls.asObject[idx]:=FData;
    FCalls.Event[idx]:=ResultHandler;
    // set to NIL
    FData:=nil;

    if FPostLevel=0 then
      begin
      Inc(FPostLevel);
      Post(FromInsideEvent, Sender);
      end;
    end;
  end;

procedure TRtcClientModule.LoginCall(ResultHandler: TRtcResult; Sender:TRtcConnection; Insert:boolean=False);
  var
    DataReq:TRtcDataRequestInfo;
    FCalls:TRtcClientModuleCallsArray;
    FRequest:TRtcClientRequest;
    CallData:TRtcValue;
  begin
  CallData:=TRtcValue.Create;
  try
    if AutoSyncEvents then
      Sender.Sync(FOnLogin, CallData)
    else
      FOnLogin(Sender, CallData);
  except
    CallData.Free;
    raise;
    end;
  // Create the "Calls" object with our remote function call
  FCalls:=TRtcClientModuleCallsArray.Create;
  FCalls.asObject[0]:=CallData;
  FCalls.Event[0]:=ResultHandler;

  // Create a new "Request" object
  FRequest:=TRtcClientRequest.Create;
  if ModuleFileName<>'' then
    FRequest.FileName:=ModuleFileName;
  if ModuleHost<>'' then
    FRequest.Host:=ModuleHost;
  FRequest.Method:='POST';

  if FDataFormat=fmt_XMLRPC then // need to send more info in header
    begin
    if FRequest.Host='' then
      FRequest.Host:=Sender.ServerAddr;
    if FRequest.Agent='' then
      FRequest.Agent:='RTC Client';
    FRequest.ContentType:='text/xml';
    end;

  // Assign our "Calls" object to the Request object, so we can access it after we post it.
  FRequest.Info.Obj['ClientModule.Call$']:=FCalls;
  FRequest.Info.asBoolean['ClientModule.Login$']:=True;

  // Create a "DataRequest" object and store the "Request" object into it
  DataReq:=TRtcDataRequestInfo.Create;
  DataReq.Request:=FRequest;
  DataReq.Events:=Self;

  // Insert the Request
  try
    if Insert then
      TRtcDataClient(Sender).InsertRequest(DataReq)
    else
      TRtcDataClient(Sender).PostRequest(DataReq,True);
  except
    DataReq.Free;
    raise;
    end;
  end;

procedure TRtcClientModule.StartCalls;
  begin
  with GetMyData do
    begin
    Inc(FPostLevel);
    if assigned(FData) then FData.Clear;
    end;
  end;

procedure TRtcClientModule.ClearMyData;
  var
    id:longword;
    obj:TObject;
    cli:TRtcClientModuleData;
  begin
  if FHyperThreading then
    begin
    id:=GetCurrentThreadId;
    if id<>MainThreadID then
      begin
      FCS.Enter;
      try
        obj:=FMyData.search(id);
        if obj<>nil then
          begin
          cli:=TRtcClientModuleData(obj);
          cli.Free;
          FMyData.remove(id);
          end;
      finally
        FCS.Leave;
        end;
      end;
    end;
  end;

function TRtcClientModule.CheckMyData: TRtcClientModuleData;
  var
    id:longword;
    obj:TObject;
  begin
  if not FHyperThreading then
    Result:=FMainThrData
  else
    begin
    id:=GetCurrentThreadId;
    if id=MainThreadID then
      Result:=FMainThrData
    else
      begin
      FCS.Enter;
      try
        obj:=FMyData.search(id);
        if obj<>nil then
          Result:=TRtcClientModuleData(obj)
        else
          Result:=nil;
      finally
        FCS.Leave;
        end;
      end;
    end;
  end;

function TRtcClientModule.GetMyData: TRtcClientModuleData;
  var
    id:longword;
    obj:TObject;
  begin
  if not FHyperThreading then
    Result:=FMainThrData
  else
    begin
    id:=GetCurrentThreadId;
    if id=MainThreadID then
      Result:=FMainThrData
    else
      begin
      FCS.Enter;
      try
        obj:=FMyData.search(id);
        if obj=nil then
          begin
          obj:=TRtcClientModuleData.Create;
          FMyData.insert(id, obj);
          end;
        Result:=TRtcClientModuleData(obj);
      finally
        FCS.Leave;
        end;
      end;
    end;
  end;

function TRtcClientModule.GetData: TRtcValue;
  var
    myData:TRtcClientModuleData;
  begin
  myData:=GetMyData;
  if not assigned(myData.FData) then
    myData.FData:=TRtcValue.Create;
  Result:=myData.FData;
  end;

function TRtcClientModule.GetPostLevel: integer;
  var
    myData:TRtcClientModuleData;
  begin
  myData:=GetMyData;
  Result:=myData.FPostLevel;
  end;

function TRtcClientModule.GetRequest: TRtcClientRequest;
  var
    myData:TRtcClientModuleData;
  begin
  myData:=GetMyData;
  if not assigned(myData.FRequest) then
    myData.FRequest:=TRtcClientRequest.Create;
  Result:=myData.FRequest;
  end;

procedure TRtcClientModule.Post(FromInsideEvent:boolean=False; Sender:TRtcConnection=nil);
  var
    DataReq:TRtcDataRequestInfo;
    myData:TRtcClientModuleData;
  begin
  myData:=CheckMyData;

  if myData=nil then
    raise Exception.Create('Have to use "StartCalls" before "Post",'#13#10+
                           'to post multiple calls in one request.');

  if myData.FPostLevel<=0 then
    raise Exception.Create('Have to use "StartCalls" before "Post",'#13#10+
                           'to post multiple calls in one request.');

  with myData do
    begin
    Dec(FPostLevel);
    if FPostLevel>0 then Exit;

    if assigned(FCalls) then
      begin
      if not assigned(FRequest) then
        FRequest:=TRtcClientRequest.Create;

      if ModuleFileName<>'' then
        FRequest.FileName:=ModuleFileName;
      if FRequest.FileName='' then
        raise Exception.Create('Module FileName is undefined. Can not Post the request.');
      if ModuleHost<>'' then
        FRequest.Host:=ModuleHost;

      FRequest.Method:='POST';

      if FDataFormat=fmt_XMLRPC then // need to send more info in header
        begin
        if (FRequest.Host='') and assigned(Sender) then
          FRequest.Host:=Sender.ServerAddr;
        if FRequest.Agent='' then
          FRequest.Agent:='RTC Client';
        FRequest.ContentType:='text/xml';
        end;

      // Assign our Calls to the Request object, so we can access it after we post it.
      FRequest.Info.Obj['ClientModule.Call$']:=FCalls;
      FCalls:=nil;

      DataReq:=TRtcDataRequestInfo.Create;
      DataReq.Request:=FRequest;
      DataReq.Events:=Self;
      FRequest:=nil;

      try
        if assigned(Sender) and (Sender is TRtcDataClient) then
          TRtcDataClient(Sender).PostRequest(DataReq,FromInsideEvent)
        else
          PostRequest(DataReq,FromInsideEvent);
      except
        DataReq.Free;
        raise;
        end;
      end;
    end;

  // Free ClientModuleData and remove it from the list
  ClearMyData;
  end;

function TRtcClientModule.GetFunctionGroup: TRtcFunctionGroup;
  begin
  try
    Result:=FFunctions;
    if not (Result is TRtcFunctionGroup) then
      Result:=nil;
  except
    Result:=nil;
    end;
  end;

procedure TRtcClientModule.SetFunctionGroup(const Value: TRtcFunctionGroup);
  begin
  FFunctions:=Value;
  end;

function TRtcClientModule.GetModuleFileName: string;
  begin
  Result:=FModuleFileName;
  end;

procedure TRtcClientModule.SetModuleFileName(const Value: string);
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

function TRtcClientModule.GetModuleHost: string;
  begin
  Result:=FModuleHost;
  end;

procedure TRtcClientModule.SetModuleHost(const Value: string);
  begin
  if FModuleHost<>Value then
    // Convert to uppercase now, so we don't have to do it on every request.
    FModuleHost:=UpperCase(Value);
  end;

procedure TRtcClientModule.Response_Problem(Sender: TRtcConnection);
  begin
  with TRtcDataClient(Sender) do
    begin
    if not Request.Reposting and not Response.Rejected then
      begin
      Call_RepostCheck(Sender);
      if not Request.Reposting and not Response.Rejected then
        Call_ResponseAbort(Sender);
      end;
    end;
  end;

procedure TRtcClientModule.Call_SessionExpired(Sender: TRtcConnection);
  begin
  DelCrypt(TRtcDataClient(Sender).Session);
  if assigned(FOnSessionExpired) then
    FOnSessionExpired(Sender);
  with TRtcDataClient(Sender) do
    begin
    Session.Init;
    Request.Query['ID']:='';
    if not Request.Reposting and not Response.Rejected then
      if Request.Reposted<1 then // if Session expires, we will try to repost 1 time ...
        Request.Repost
      else // ... and leave all other decisions to the user
        Response_Problem(Sender);
    end;
  end;

procedure TRtcClientModule.Call_WrongResponse(Sender: TRtcConnection);
  begin
  DelCrypt(TRtcDataClient(Sender).Session);
  if assigned(FOnResponseError) then
    FOnResponseError(Sender);
  Response_Problem(Sender);
  end;

procedure TRtcClientModule.Call_WrongEncryption(Sender: TRtcConnection);
  begin
  DelCrypt(TRtcDataClient(Sender).Session);
  if assigned(FOnWrongEncryption) then
    FOnWrongEncryption(Sender);
  Response_Problem(Sender);
  end;

procedure TRtcClientModule.Call_NoEncryption(Sender: TRtcConnection);
  begin
  DelCrypt(TRtcDataClient(Sender).Session);
  if assigned(FOnNoEncryption) then
    FOnNoEncryption(Sender);
  Response_Problem(Sender);
  end;

procedure TRtcClientModule.Call_NeedEncryption(Sender: TRtcConnection);
  begin
  DelCrypt(TRtcDataClient(Sender).Session);
  if assigned(FOnNeedEncryption) then
    FOnNeedEncryption(Sender);
  Response_Problem(Sender);
  end;

procedure TRtcClientModule.SetAutoEncrypt(const Value: integer);
  begin
  if Value<0 then
    raise Exception.Create('Negative values not allowed for EncryptionKey.');
  FAutoEncrypt := Value;
  if FAutoEncrypt > 0 then
    FAutoSessions:=True
  else
    FForceEncrypt:=False;
  end;

procedure TRtcClientModule.SetAutoSessions(const Value: boolean);
  begin
  FAutoSessions := Value;
  if not FAutoSessions then
    begin
    FAutoEncrypt:=0;
    FAutoLogin:=False;
    FForceEncrypt:=False;
    end;
  end;

procedure TRtcClientModule.SetForceEncrypt(const Value: boolean);
  begin
  FForceEncrypt := Value;
  if FForceEncrypt then
    begin
    FAutoSessions:=True;
    if FAutoEncrypt=0 then
      FAutoEncrypt:=16;
    end;
  end;

procedure TRtcClientModule.PostInteractiveResult(Event: TRtcResult; Data, Result: TRtcValue);
  var
    res:TRtcInteractiveResult;
  begin
  FIntCS.Enter;
  try
    res:=TRtcInteractiveResult.Create;
    res.FEvent:=Event;
    res.Data:=Data;
    res.Result:=Result;

    FIntRes.Add(res);

    if not assigned(FIntTimer) then
      begin
      FIntTimer:=TRtcTimer.Create(False);
      {$IFDEF FPC}
      TRtcTimer.Enable(FIntTimer,1,@DoInteractiveResult,True);
      {$ELSE}
      TRtcTimer.Enable(FIntTimer,1,DoInteractiveResult,True);
      {$ENDIF}
      end;
  finally
    FIntCS.Leave;
    end;
  end;

procedure TRtcClientModule.DoInteractiveResult;
  var
    res:TRtcInteractiveResult;
  begin
  FIntCS.Enter;
  try
    res:=TRtcInteractiveResult(FIntRes.Items[0]);
    FIntRes.Delete(0);
  finally
    FIntCS.Leave;
    end;

  try
    res.FEvent.Call_Return(nil, res.Data, res.Result);
  finally
    res.Free;

    FIntCS.Enter;
    try
      if FIntRes.Count>0 then
      {$IFDEF FPC}
        TRtcTimer.Enable(FIntTimer,1,@DoInteractiveResult,True)
      {$ELSE}
        TRtcTimer.Enable(FIntTimer,1,DoInteractiveResult,True)
      {$ENDIF}
      else
        begin
        TRtcTimer.Stop(FIntTimer);
        FIntTimer:=nil;
        end;
    finally
      FIntCS.Leave;
      end;
    end;
  if FRelease then
    Free;
  end;

procedure TRtcClientModule.Release;
  begin
  FRelease:=True;
  end;

procedure TRtcClientModule.NotifyResultAborted(Sender: TRtcConnection);
  var
    MyCalls:TRtcClientModuleCallsArray;
    event:TRtcResult;
    data:TRtcValue;
    a:integer;
  begin
  MyCalls:=TRtcClientModuleCallsArray(TRtcDataClient(Sender).Request.Info.Obj['ClientModule.Call$']);
  if assigned(MyCalls) then
    begin
    for a:=0 to MyCalls.Count-1 do
      begin
      event:=MyCalls.Event[a];
      if assigned(event) then
        begin
        data:=TRtcValue(MyCalls.AsObject[a]);
        event.Call_Aborted(Sender,data,nil);
        end;
      end;
    end;
  end;

function TRtcClientModule.IsRemoteCallRequest(Sender:TRtcConnection): boolean;
  begin
  Result:= assigned(TRtcClientModuleCallsArray(TRtcDataClient(Sender).Request.Info.Obj['ClientModule.Call$']));
  end;

procedure TRtcClientModule.SetDataFormat(const Value: TRtcDataFormat);
  begin
  FDataFormat := Value;
  end;

procedure TRtcClientModule.SetAutoLogin(const Value: boolean);
  begin
  FAutoLogin := Value;
  if FAutoLogin then
    FAutoSessions:=True;
  end;

procedure TRtcClientModule.Call_LoginAborted(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if assigned(FOnLoginAborted) then
    FOnLoginAborted(Sender,Data,Result);
  end;

procedure TRtcClientModule.Call_LoginResult(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if assigned(FOnLoginResult) then
    FOnLoginResult(Sender,Data,Result);

  if Result.isType<>rtc_Exception then
    TRtcDataClient(Sender).Session.asBoolean['ClientModule.Login$']:=True;
  end;

procedure TRtcClientModule.ResetLogin;
  begin
  FResetLogin:=True;
  end;

{ TRtcInteractiveResult }

destructor TRtcInteractiveResult.Destroy;
  begin
  Data.Free;
  Result.Free;
  inherited;
  end;

{ TRtcClientModuleCallsArray }

constructor TRtcClientModuleCallsArray.Create;
  begin
  inherited;
  SetLength(FEvents,0);
  end;

destructor TRtcClientModuleCallsArray.Destroy;
  begin
  SetLength(FEvents,0);
  inherited;
  end;

function TRtcClientModuleCallsArray.GetEvent(index: integer): TRtcResult;
  begin
  if (index>=0) and (index<=length(FEvents)) then
    Result:=FEvents[index]
  else
    Result:=nil;
  end;

procedure TRtcClientModuleCallsArray.SetEvent(index: integer; const _Value: TRtcResult);
  begin
  if length(FEvents)<index+1 then
    SetLength(FEvents, index+1);
  FEvents[index]:=_Value;
  end;

{ TRtcCryptClient }

destructor TRtcCryptClient.Destroy;
  begin
  Init;
  inherited;
  end;

procedure TRtcCryptClient.Kill;
  begin
  Free;
  end;

procedure TRtcCryptClient.Init;
  begin
  HaveHello:=False;
  HaveStart:=False;
  ControlCounter:=0;
  ClientHello:='';
  ServerHello:='';
  ClientKey:='';
  ServerKey:='';
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


initialization
randomize;
end.
