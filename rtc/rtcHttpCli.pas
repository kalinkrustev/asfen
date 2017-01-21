{
  @html(<b>)
  HTTP Client Connection
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  Introducing the @html(<b>) @Link(TRtcHttpClient) @html(</b>) component:
  @html(<br>)
  Client connection component for TCP/IP communication using HTTP requests.
}
unit rtcHttpCli;

{$INCLUDE rtcDefs.inc}

interface

uses
  Classes,

  rtcInfo,
  rtcConn,

  rtcFastStrings,
  rtcPlugins,
  rtcDataCli;

type
  // @exclude
  TRtcHttpClient=class;

  { @Abstract(Login info or authenticating the user on a secure server) }
  TRtcHttpUserLogin=class(TPersistent)
  private
    FUserName: string;
    FUserPassword: string;
    FCertSubject: string;
    FCertStoreType: TRtcCertStoreType;

    procedure SetUserName(const Value: string);
    procedure SetUserPassword(const Value: string);
    procedure SetCertStoreType(const Value: TRtcCertStoreType);
    procedure SetCertSubject(const Value: string);

  public
    Con:TRtcHttpClient;

    { Will be created by TRtcHttpClient component.
      @exclude }
    constructor Create;
    { Will be destroyed by TRtcHttpClient component.
      @exclude }
    destructor Destroy; override;

  published
    // Username
    property UserName:string read FUserName write SetUserName;
    // Password
    property UserPassword:string read FUserPassword write SetUserPassword;

    // Certificate store tye
    property CertStoreType:TRtcCertStoreType read FCertStoreType write SetCertStoreType default certAny;

    { String under "CN" in Certificate's "Subject" property under "Details",
      or "Issued to" in Certificate's "General" Tab. }
    property CertSubject:string read FCertSubject write SetCertSubject;
    end;

  { @Abstract(Client Connection component for TCP/IP communication using HTTP requests)

    Received data will be processed by TRtcHttpClient to gather Request
    information and make it easily accessible through the
    @Link(TRtcDataClient.Request) property.
    The same way, your response will be packed into a HTTP result header
    and sent out as a valid HTTP result, readable by any Web Browser.
    @html(<br>)
    @Link(TRtcHttpClient) also makes sure that you receive requests one by one
    and get the chance to answer them one-by-one, even if the client side
    sends all the requests at once (as one big request list), so
    you can relax and process all incomming requests, without worrying
    about overlapping your responses for different requests.
    @html(<br><br>)

    Properties to check first:
    @html(<br>)
    @Link(TRtcConnection.ServerAddr) - Local Address to bind the server to (leave empty for ALL)
    @html(<br>)
    @Link(TRtcConnection.ServerPort) - Port to listen on and wait for connections
    @html(<br><br>)

    Methods to check first:
    @html(<br>)
    @Link(TRtcClient.Connect) - Connect to Server
    @html(<br>)
    @Link(TRtcDataClient.Request), @Link(TRtcHttpClient.WriteHeader), @Link(TRtcHttpClient.Write) - Write (send) Request to Server
    @html(<br>)
    @Link(TRtcDataClient.Response), @Link(TRtcConnection.Read) - Read Server's Response
    @html(<br>)
    @Link(TRtcConnection.Disconnect) - Disconnect from Server
    @html(<br><br>)

    Events to check first:
    @html(<br>)
    @Link(TRtcConnection.OnConnect) - Connected to Server
    @html(<br>)
    @Link(TRtcConnection.OnDataSent) - Data sent to server (buffer now empty)
    @html(<br>)
    @Link(TRtcConnection.OnDataReceived) - Data available from server (check @Link(TRtcDataClient.Response))
    @html(<br>)
    @Link(TRtcHttpClient.OnInvalidResponse) - Received invalid response from Server
    @html(<br>)
    @Link(TRtcConnection.OnDisconnect) - Disconencted from Server
    @html(<br><br>)

    Check @Link(TRtcClient) and @Link(TRtcConnection) for more info.
    }
  TRtcHttpClient = class(TRtcDataClient)
  private
    FCryptPlugin:TRtcCryptPlugin;

    FUseProxy:boolean;
    FUseSSL:boolean;
    FUseWinHTTP:boolean;

    // User Parameters
    FMaxResponseSize:cardinal;
    FMaxHeaderSize:cardinal;
    FOnInvalidResponse:TRtcNotifyEvent;

    // Internal variables
    FWritten:boolean;
    FWriteBuffer:TRtcHugeString;

    FUserLogin: TRtcHttpUserLogin;

    function GetUseProxy: boolean;
    procedure SetUseProxy(const Value: boolean);

    function GetUseSSL: boolean;
    procedure SetUseSSL(const Value: boolean);

    function GetUseWinHTTP: boolean;
    procedure SetUseWinHTTP(const Value: boolean);

  protected
    // @exclude
    procedure UserDataChange;

    // @exclude
    procedure SetTriggers; override;
    // @exclude
    procedure ClearTriggers; override;
    // @exclude
    procedure SetParams; override;

    // @exclude
    function CreateProvider:TObject; override;


    // @exclude
    procedure TriggerDataSent; override;
    // @exclude
    procedure TriggerDataReceived; override;
    // @exclude
    procedure TriggerDataOut; override;

    // @exclude
    procedure TriggerInvalidResponse; virtual;
    // @exclude
    procedure CallInvalidResponse; virtual;

    // @exclude
    procedure SetRequest(const Value: TRtcClientRequest); override;
    // @exclude
    procedure SetResponse(const Value: TRtcClientResponse); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function New:TRtcHttpClient;

    // @exclude
    procedure LeaveEvent; override;

    { Flush all buffered data.
      @html(<br>)
      When using 'Write' without calling 'WriteHeader' before, all data
      prepared by calling 'Write' will be buffered until your event
      returns to its caller (automatically upon your event completion) or
      when you first call 'Flush'. Flush will check if Request.ContentLength is set
      and if not, will set the content length to the number of bytes buffered.
      @html(<br>)
      Flush does nothing if WriteHeader was called for this response.

      @exclude}
    procedure Flush; override;

    // You can call WriteHeader to send the Request header out.
    procedure WriteHeader(SendNow:boolean=True); overload; override;
    { You can call WriteHeader with empty 'HeaderText' parameter to
      tell the component that you do not want any HTTP header to be sent. }
    procedure WriteHeader(const HeaderText: string; SendNow:boolean=True); overload; override;

    // Use Write to send any Content (document body) out.
    procedure Write(const s:string=''); override;

  published
    { UserLogin data is ignored when CryptPlugin is assigned.

      If CryptPlugin is NOT assigned (not using third-party components for encryption) ...

      Using this property, you can define login information for a server which
      requires user authentication and/or a client certificate (using WinInet API).
      If you want to use Third-party SSL/SSH components for encryption,
      simply assign the plug-in to the CryptPlugin property. }
    property UserLogin:TRtcHttpUserLogin read FUserLogin write FUserLogin;

    { UseProxy is ignored (assumed to be FALSE) when CryptPlugin is assigned.

      If CryptPlugin is NOT assigned (not using third-party components for encryption) ...

      When UseProxy is TRUE, connection component will use a WinInet connection provider,
      which supports transfering data over HTTP proxy servers. When UseProxy is FALSE,
      proxy servers will be ignored and component will always try to open a direct
      connection to the server, ignoring any proxy settings in the system. }
    property UseProxy:boolean read GetUseProxy write SetUseProxy default False;

    { UseSSL is ignored (assumed to be TRUE) when CryptPlugin is assigned.

      If CryptPlugin is NOT assigned (not using third-party components for encryption) ...

      When UseSSL is TRUE, connection component will use a connection provider
      which supports transfering data using the Secure-Socket-Layer (SSL) over
      the HTTPS protocol and send all requests using the HTTPS protocol instead
      of the standard HTTP protocol. When UseSSL is FALSE, standard HTTP protocol
      will be used. Note that RTC Servers do NOT support SSL. }
    property UseSSL:boolean read GetUseSSL write SetUseSSL default False;

    { Set this property if you want to use the WinHTTP API.

      WinHTTP API is blocking, it supports Proxy and SSL options,
      but it ignores any parameters set in the "UserLogin" property.
      WinHTTP can be used instead of WinINET for applications running
      as Windows Services which do not have access to Internet Explorer settings. }
    property UseWinHTTP:boolean read GetUseWinHTTP write SetUseWinHTTP default False;

    { Maximum allowed size of the first response line, without header (0 = no limit).
      This is the first line in a HTTP response and includes Response.StatusCode and Response.StatusText }
    property MaxResponseSize:cardinal read FMaxResponseSize write FMaxResponseSize default 0;
    { Maximum allowed size of each response's header size (0 = no limit).
      This are all the remaining header lines in a HTTP response,
      which come after the first line and end with an empty line,
      after which usually comes the content (document body). }
    property MaxHeaderSize:cardinal read FMaxHeaderSize write FMaxHeaderSize default 0;

    { This event will be called if the received response exceeds your defined
      maximum response or header size. If both values are 0, this event will never be called. }
    property OnInvalidResponse:TRtcNotifyEvent read FOnInvalidResponse write FOnInvalidResponse;

    { To use SSL/SSH encryption using third-party components, simply assign the encryption
      plug-in here before you start using the Client connection (before first connect). }
    property CryptPlugin:TRtcCryptPlugin read FCryptPlugin write FCryptPlugin;
    end;

implementation

uses
  SysUtils,
  rtcConnProv,

  rtcWinHttpCliProv, // WinHTTP HTTP Client Provider
  rtcWInetHttpCliProv, // WinInet HTTP Client Provider
  rtcWSockHttpCliProv; // WSocket HTTP Client Provider

type
  TMyProvider1 = TRtcWSockHttpClientProvider; // direct TCP/IP
  TMyProvider2 = TRtcWInetHttpClientProvider; // WinInet over Proxy
  TMyProvider3 = TRtcWinHttpClientProvider; // WinHTTP over Proxy

{ TRtcHttpClient }

constructor TRtcHttpClient.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FUserLogin:=TRtcHttpUserLogin.Create;
  FUserLogin.Con:=self;

  FUseProxy:=False;
  FUseSSL:=False;
  FUseWinHTTP:=False;

  FWriteBuffer:=TRtcHugeString.Create;
  FWritten:=False;
  end;

destructor TRtcHttpClient.Destroy;
  begin
  FUserLogin.Free;
  FWriteBuffer.Free;
  inherited;
  end;

class function TRtcHttpClient.New: TRtcHttpClient;
  begin
  Result:=Create(nil);
  end;

function TRtcHttpClient.CreateProvider:TObject;
  begin
  if not assigned(Con) then
    begin
    if assigned(FCryptPlugin) then
      Con:=TMyProvider1.Create
    else if FUseWinHTTP then
      begin
      if HaveWinHTTP then
        Con:=TMyProvider3.Create
      else
        Con:=TMyProvider2.Create;
      end
    else if FUseProxy or FUseSSL or
      (FUserLogin.UserName<>'') or
      (FUserLogin.UserPassword<>'') then
      Con:=TMyProvider2.Create
    else
      Con:=TMyProvider1.Create;
    SetTriggers;
    end;
  Result:=Con;
  end;

procedure TRtcHttpClient.SetParams;
  begin
  inherited;
  if assigned(Con) then
    begin
    if Con is TMyProvider1 then
      begin
      TMyProvider1(Con).Request:=Request;
      TMyProvider1(Con).Response:=Response;
      TMyProvider1(Con).MaxResponseSize:=MaxResponseSize;
      TMyProvider1(Con).MaxHeaderSize:=MaxHeaderSize;
      end
    else if Con is TMyProvider2 then
      begin
      TMyProvider2(Con).useHttps:=FUseSSL;
      TMyProvider2(Con).UserName:=FUserLogin.UserName;
      TMyProvider2(Con).UserPassword:=FUserLogin.UserPassword;
      TMyProvider2(Con).CertStoreType:=FUserLogin.CertStoreType;
      TMyProvider2(Con).CertSubject:=FUserLogin.CertSubject;

      TMyProvider2(Con).Request:=Request;
      TMyProvider2(Con).Response:=Response;
      TMyProvider2(Con).MaxResponseSize:=MaxResponseSize;
      TMyProvider2(Con).MaxHeaderSize:=MaxHeaderSize;
      end
    else if Con is TMyProvider3 then
      begin
      TMyProvider3(Con).useHttps:=FUseSSL;
      TMyProvider3(Con).CertStoreType:=FUserLogin.CertStoreType;
      TMyProvider3(Con).CertSubject:=FUserLogin.CertSubject;

      TMyProvider3(Con).Request:=Request;
      TMyProvider3(Con).Response:=Response;
      TMyProvider3(Con).MaxResponseSize:=MaxResponseSize;
      TMyProvider3(Con).MaxHeaderSize:=MaxHeaderSize;
      end
    else
      raise Exception.Create('Connection Provider not recognized.');
    end;
  end;

procedure TRtcHttpClient.SetTriggers;
  begin
  inherited;
  if assigned(Con) then
    begin
    if Con is TMyProvider1 then
      TMyProvider1(Con).CryptPlugin:=CryptPlugin;
    {$IFDEF FPC}
      if Con is TMyProvider1 then
        TMyProvider1(Con).SetTriggerInvalidResponse(@TriggerInvalidResponse)
      else if Con is TMyProvider2 then
        TMyProvider2(Con).SetTriggerInvalidResponse(@TriggerInvalidResponse)
      else if Con is TMyProvider3 then
        TMyProvider3(Con).SetTriggerInvalidResponse(@TriggerInvalidResponse);
    {$ELSE}
      if Con is TMyProvider1 then
        TMyProvider1(Con).SetTriggerInvalidResponse(TriggerInvalidResponse)
      else if Con is TMyProvider2 then
        TMyProvider2(Con).SetTriggerInvalidResponse(TriggerInvalidResponse)
      else if Con is TMyProvider3 then
        TMyProvider3(Con).SetTriggerInvalidResponse(TriggerInvalidResponse);
    {$ENDIF}
    end;
  end;

procedure TRtcHttpClient.ClearTriggers;
  begin
  inherited;
  if assigned(Con) then
    begin
    if Con is TMyProvider1 then
      begin
      TMyProvider1(Con).CryptPlugin:=nil;
      TMyProvider1(Con).SetTriggerInvalidResponse(nil);
      end
    else if Con is TMyProvider2 then
      TMyProvider2(Con).SetTriggerInvalidResponse(nil)
    else if Con is TMyProvider3 then
      TMyProvider3(Con).SetTriggerInvalidResponse(nil);
    end;
  end;

procedure TRtcHttpClient.WriteHeader(SendNow:boolean=True);
  begin
  if assigned(Con) and (State<>conInactive) then
    begin
    if Request.Active then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
    if Con is TMyProvider1 then
      TMyProvider1(Con).WriteHeader(SendNow)
    else if Con is TMyProvider2 then
      TMyProvider2(Con).WriteHeader(SendNow)
    else if Con is TMyProvider3 then
      TMyProvider3(Con).WriteHeader(SendNow);
    end;
  end;

procedure TRtcHttpClient.WriteHeader(const HeaderText: string; SendNow:boolean=True);
  begin
  if assigned(Con) and (State<>conInactive) then
    begin
    if Request.Active then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
    if Con is TMyProvider1 then
      TMyProvider1(Con).WriteHeader(HeaderText, SendNow)
    else if Con is TMyProvider2 then
      TMyProvider2(Con).WriteHeader(HeaderText, SendNow)
    else if Con is TMyProvider3 then
      TMyProvider3(Con).WriteHeader(HeaderText, SendNow);
    end;
  end;

procedure TRtcHttpClient.Write(const s: string='');
  begin
  if assigned(Con) and (State<>conInactive) then
    begin
    if Request.Complete then
      raise Exception.Create('Error! Request already sent, can not send more request data now! Request Header wrong?');

    if Request.Active then
      begin
      { Header is out }

      if Request['Content-Length']<>'' then
        if Request.ContentLength - Request.ContentOut < length(s) then
          raise Exception.Create('Error! Sending more data out than specified in header.');

      { Data size is known or unimportant.
        We can just write the string out, without buffering }

      Con.Write(s);
      end
    else
      begin
      if (Request['CONTENT-LENGTH']<>'') and not FWritten then
        begin
        { Content length defined and no data buffered,
          send out header prior to sending first content bytes }
        WriteHeader(length(s)=0);
        if Request.ContentLength - Request.ContentOut < length(s) then
          raise Exception.Create('Error! Sending more data out than specified in header.');
        Con.Write(s);
        end
      else
        begin
        { Header is not out.
          Buffer all Write() operations,
          so we can determine content size and write it all out in a flush. }
        FWritten:=True;
        FWriteBuffer.Add(s);
        end;
      end;
    end;
  end;

procedure TRtcHttpClient.Flush;
  var
    Temp:string;
  begin
  if not FWritten then
    Exit
  else
    FWritten:=False; // so we don't re-enter this method.

  if assigned(Con) and (State<>conInactive) then
    begin
    Timeout.DataSending;

    if Request.Complete then
      raise Exception.Create('Error! Request was already sent! Can not send more data now! Request Header wrong?');

    if not Request.Active then
      begin
      if Request['CONTENT-LENGTH']='' then // length not specified
        begin
        Request.AutoLength:=True;
        Request.ContentLength:=FWriteBuffer.Size;
        end;

      if Con is TMyProvider1 then
        TMyProvider1(Con).WriteHeader(FWriteBuffer.Size=0)
      else if Con is TMyProvider2 then
        TMyProvider2(Con).WriteHeader(FWriteBuffer.Size=0)
      else if Con is TMyProvider3 then
        TMyProvider3(Con).WriteHeader(FWriteBuffer.Size=0);
      end;

    if FWriteBuffer.Size>0 then
      begin
      Temp:=FWriteBuffer.Get;
      FWriteBuffer.Clear;
      Con.Write(Temp);
      Temp:='';
      end;
    end;
  end;

procedure TRtcHttpClient.CallInvalidResponse;
  begin
  if assigned(OnInvalidResponse) then
    OnInvalidResponse(self);
  end;

procedure TRtcHttpClient.TriggerDataReceived;
  begin
  inherited;
  Flush;
  end;

procedure TRtcHttpClient.TriggerDataSent;
  begin
  if FWriteCount>0 then
    Timeout.DataSent;
  EnterEvent;
  try
    if FWriteCount>0 then
      begin
      CallDataSent;
      Flush;
      end;

    if not isClosing then
      begin
      CallReadyToSend;
      Flush;
      end;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcHttpClient.TriggerDataOut;
  begin
  inherited;
  Flush;
  end;

procedure TRtcHttpClient.TriggerInvalidResponse;
  begin
  EnterEvent;
  try
    CallInvalidResponse;
    Flush;

    Disconnect;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcHttpClient.SetRequest(const Value: TRtcClientRequest);
  begin
  inherited SetRequest(Value);
  if assigned(Con) then
      if Con is TMyProvider1 then
        TMyProvider1(Con).Request:=Request
      else if Con is TMyProvider2 then
        TMyProvider2(Con).Request:=Request
      else if Con is TMyProvider3 then
        TMyProvider3(Con).Request:=Request;
  end;

procedure TRtcHttpClient.SetResponse(const Value: TRtcClientResponse);
  begin
  inherited SetResponse(Value);
  if assigned(Con) then
      if Con is TMyProvider1 then
        TMyProvider1(Con).Response:=Response
      else if Con is TMyProvider2 then
        TMyProvider2(Con).Response:=Response
      else if Con is TMyProvider3 then
        TMyProvider3(Con).Response:=Response;
  end;

function TRtcHttpClient.GetUseProxy: boolean;
  begin
  Result:=FUseProxy;
  end;

procedure TRtcHttpClient.SetUseProxy(const Value: boolean);
  begin
  if Value<>FUseProxy then
    begin
    if assigned(Con) then
      if isConnected or isConnecting then
        Error('Can not change UseProxy after Connect.')
      else
        ReleaseProvider;
    FUseProxy:=Value;
    end;
  end;

function TRtcHttpClient.GetUseSSL: boolean;
  begin
  Result:=FUseSSL;
  end;

procedure TRtcHttpClient.SetUseSSL(const Value: boolean);
  begin
  if Value<>FUseSSL then
    begin
    if assigned(Con) then
      if isConnected or isConnecting then
        Error('Can not change UseSSL after Connect.')
      else
        ReleaseProvider;
    FUseSSL:=Value;
    end;
  end;

function TRtcHttpClient.GetUseWinHTTP: boolean;
  begin
  Result:=FUseWinHTTP;
  end;

procedure TRtcHttpClient.SetUseWinHTTP(const Value: boolean);
  begin
  if Value<>FUseWinHTTP then
    begin
    if assigned(Con) then
      if isConnected or isConnecting then
        Error('Can not change UseWinHTTP after Connect.')
      else
        ReleaseProvider;
    FUseWinHTTP:=Value;
    end;
  end;

procedure TRtcHttpClient.UserDataChange;
  begin
  if assigned(Con) then
    if isConnected or isConnecting then
      Error('Can not change UserLogin data after Connect.')
    else
      ReleaseProvider;
  end;

procedure TRtcHttpClient.LeaveEvent;
  begin
  inherited;
    if not InsideEvent then
      if assigned(Con) then
        if Con is TMyProvider2 then
          TMyProvider2(Con).LeavingEvent
        else if Con is TMyProvider3 then
          TMyProvider3(Con).LeavingEvent;
  end;

{ TRtcHttpUserLogin }

constructor TRtcHttpUserLogin.Create;
  begin

  end;

destructor TRtcHttpUserLogin.Destroy;
  begin

  inherited;
  end;

procedure TRtcHttpUserLogin.SetCertStoreType(const Value: TRtcCertStoreType);
  begin
  if Value<>FCertStoreType then
    begin
    Con.UserDataChange;
    FCertStoreType := Value;
    end;
  end;

procedure TRtcHttpUserLogin.SetCertSubject(const Value: string);
  begin
  if Value<>FCertSubject then
    begin
    Con.UserDataChange;
    FCertSubject := Value;
    end;
  end;

procedure TRtcHttpUserLogin.SetUserName(const Value: string);
  begin
  if Value<>FUserName then
    begin
    Con.UserDataChange;
    FUserName := Value;
    end;
  end;

procedure TRtcHttpUserLogin.SetUserPassword(const Value: string);
  begin
  if Value<>FUserPassword then
    begin
    Con.UserDataChange;
    FUserPassword := Value;
    end;
  end;

end.
