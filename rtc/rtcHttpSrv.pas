{
  @html(<b>)
  HTTP Server Connection
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  Introducing the @html(<b>) @Link(TRtcHttpServer) @html(</b>) component:
  @html(<br>)
  Server connection component for TCP/IP communication using HTTP requests.
  Received data will be processed to gather Request information and
  make it easily accessible through the @Link(TRtcDataServer.Request) property.
  The same way, your response will be packed into a HTTP result header
  and sent out as a valid HTTP result, readable by any Web Browser.
  @Link(TRtcHttpServer) also makes sure that you receive requests one by one,
  even if the client side sends all requests at once (as one big request list).
}
unit rtcHttpSrv;

{$INCLUDE rtcDefs.inc}

interface

uses
  Classes,

  rtcInfo,
  rtcConn,

  rtcFastStrings,
  rtcPlugins,
  rtcDataSrv;

type
  { @Abstract(Server Connection component for direct TCP/IP communication using the HTTP protocol)

    Received data will be processed by TRtcHttpServer to gather Request
    information and make it easily accessible through the
    @Link(TRtcDataServer.Request) property.
    The same way, your response will be packed into a HTTP result header
    and sent out as a valid HTTP result, readable by any Web Browser.
    @html(<br>)
    @Link(TRtcHttpServer) also makes sure that you receive requests one by one
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
    @Link(TRtcServer.Listen) - Start server
    @html(<br>)
    @Link(TRtcDataServer.Request), @Link(TRtcConnection.Read) - Read client request
    @html(<br>)
    @Link(TRtcDataServer.Response), @Link(TRtcHttpServer.WriteHeader), @Link(TRtcHttpServer.Write) - Write result to client
    @html(<br>)
    @Link(TRtcConnection.Disconnect) - Disconnect client
    @html(<br>)
    @Link(TRtcServer.StopListen) - Stop server
    @html(<br><br>)

    Events to check first:
    @html(<br>)
    @Link(TRtcServer.OnListenStart) - Server started
    @html(<br>)
    @Link(TRtcConnection.OnConnect) - new Client connected
    @html(<br>)
    @Link(TRtcConnection.OnDisconnect) - one Client disconnected
    @html(<br>)
    @Link(TRtcServer.OnListenStop) - Server stopped
    @html(<br><br>)

    Check @Link(TRtcDataServer), @Link(TRtcServer) and @Link(TRtcConnection) for more info.
    }
  TRtcHttpServer = class(TRtcDataServer)
  private
    FCryptPlugin:TRtcCryptPlugin;

    // User Parameters
    FMaxRequestSize:cardinal;
    FMaxHeaderSize:cardinal;
    FOnInvalidRequest:TRtcNotifyEvent;

    FWritten:boolean;
    FWriteBuffer:TRtcHugeString;

  protected
    // @exclude
    procedure CopyFrom(Dup: TRtcServer); override;

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
    procedure TriggerInvalidRequest; virtual;
    // @exclude
    procedure CallInvalidRequest; virtual;

    // @exclude
    procedure SetRequest(const Value: TRtcServerRequest); override;
    // @exclude
    procedure SetResponse(const Value: TRtcServerResponse); override;

  public
    // @exclude
    constructor Create(AOwner: TComponent); override;
    // @exclude
    destructor Destroy; override;

    // Constructor
    class function New:TRtcHttpServer;

    { Flush all buffered data.
      @html(<br>)
      When using 'Write' without calling 'WriteHeader' before, all data
      prepared by calling 'Write' will be buffered until your event
      returns to its caller (automatically upon your event completion) or
      when you first call 'Flush'. Flush will check if Response.ContentLength is set
      and if not, will set the content length to the number of bytes buffered.
      @html(<br>)
      Flush does nothing if WriteHeader was called for this response.

      @exclude}
    procedure Flush; override;

    // You can call WriteHeader to send the Response header out.
    procedure WriteHeader(SendNow:boolean=True); overload; override;
    { You can call WriteHeader with empty 'HeaderText' parameter to
      tell the component that you do not want any HTTP header to be sent. }
    procedure WriteHeader(const HeaderText: string; SendNow:boolean=True); overload; override;

    // Use Write to send the Content (document body) out.
    procedure Write(const s:string=''); override;

  published
    { Maximum allowed size of the request, without header (0 = no limit).
      This is the first line in a HTTP request and includes Request.Method and Request.URI }
    property MaxRequestSize:cardinal read FMaxRequestSize write FMaxRequestSize default 0;
    { Maximum allowed size of each request's header size (0 = no limit).
      This are all the remaining header lines in a HTTP request,
      which come after the first line and end with an empty line,
      after which usually comes the content (document body). }
    property MaxHeaderSize:cardinal read FMaxHeaderSize write FMaxHeaderSize default 0;

    { This event will be called if the received request exceeds your defined
      maximum request or header size. If both values are 0, this event will never be called. }
    property OnInvalidRequest:TRtcNotifyEvent read FOnInvalidRequest write FOnInvalidRequest;

    { To use SSL/SSH encryption, assign the encryption plug-in here,
      before you first start the Server Listener. }
    property CryptPlugin:TRtcCryptPlugin read FCryptPlugin write FCryptPlugin;
    end;

implementation

uses
  SysUtils,
  rtcConnProv,
  rtcWSockHttpSrvProv; // WSocket HTTP Server Provider

type
  TMyProvider = TRtcWSockHttpServerProvider;

class function TRtcHttpServer.New: TRtcHttpServer;
  begin
  Result:=Create(nil);
  end;

{ TRtcHttpServer }

function TRtcHttpServer.CreateProvider:TObject;
  begin
  if not assigned(Con) then
    begin
    Con:=TMyProvider.Create;
    SetTriggers;
    end;
  Result:=Con;
  end;

procedure TRtcHttpServer.CopyFrom(Dup: TRtcServer);
  begin
  inherited CopyFrom(Dup);

  MaxRequestSize:=TRtcHttpServer(Dup).MaxRequestSize;
  MaxHeaderSize:=TRtcHttpServer(Dup).MaxHeaderSize;
  OnInvalidRequest:=TRtcHttpServer(Dup).OnInvalidRequest;
  CryptPlugin:=TRtcHttpServer(Dup).CryptPlugin;
  end;

procedure TRtcHttpServer.SetParams;
  begin
  inherited;
  if assigned(Con) then
    begin
    TMyProvider(Con).Request:=Request;
    TMyProvider(Con).Response:=Response;
    TMyProvider(Con).MaxRequestSize:=MaxRequestSize;
    TMyProvider(Con).MaxHeaderSize:=MaxHeaderSize;
    end;
  end;

procedure TRtcHttpServer.SetTriggers;
  begin
  inherited;
  if assigned(Con) then
    begin
    TMyProvider(Con).CryptPlugin:=CryptPlugin;
    {$IFDEF FPC}
    TMyProvider(Con).SetTriggerInvalidRequest(@TriggerInvalidRequest);
    {$ELSE}
    TMyProvider(Con).SetTriggerInvalidRequest(TriggerInvalidRequest);
    {$ENDIF}
    end;
  end;

procedure TRtcHttpServer.ClearTriggers;
  begin
  inherited;
  if assigned(Con) then
    begin
    TMyProvider(Con).CryptPlugin:=nil;
    TMyProvider(Con).SetTriggerInvalidRequest(nil);
    end;
  end;

procedure TRtcHttpServer.WriteHeader(SendNow:boolean=True);
  begin
  if assigned(Con) and (State<>conInactive) then
    begin
    if Response.Sending then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
    TMyProvider(Con).WriteHeader(SendNow);
    end;
  end;

procedure TRtcHttpServer.WriteHeader(const HeaderText: string; SendNow:boolean=True);
  begin
  if assigned(Con) and (State<>conInactive) then
    begin
    if Response.Sending then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
    TMyProvider(Con).WriteHeader(HeaderText, SendNow);
    end;
  end;

procedure TRtcHttpServer.Write(const s: string='');
  begin
  if assigned(Con) and (State<>conInactive) then
    begin
    if Response.Sent then
      raise Exception.Create('Error! Answer allready sent for this request.');

    if Response.Sending then
      begin
      { Header is out }

      if Response['Content-Length']<>'' then
        if Response.ContentLength - Response.ContentOut < length(s) then
          raise Exception.Create('Error! Sending more data out than specified in header.');

      { Data size is known or unimportant.
        We can just write the string out, without buffering }

      Con.Write(s);
      end
    else
      begin
      if (Response['CONTENT-LENGTH']<>'') and not FWritten then // Direct writing if header was sent out.
        begin
        { Content length defined and no data buffered,
          send out header prior to sending first content bytes }
        WriteHeader(length(s)=0);
        if Response.ContentLength - Response.ContentOut < length(s) then
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

procedure TRtcHttpServer.Flush;
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

    if Response.Sent then
      raise Exception.Create('Error! Answer allready sent for this request.');

    if not Response.Sending then
      begin
      if Response['CONTENT-LENGTH']='' then // length not specified
        Response.ContentLength:=FWriteBuffer.Size;

      TMyProvider(Con).WriteHeader(False);
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

procedure TRtcHttpServer.CallInvalidRequest;
  begin
  if assigned(OnInvalidRequest) then
    OnInvalidRequest(self);
  end;

procedure TRtcHttpServer.TriggerDataReceived;
  begin
  inherited;
  Flush;
  end;

procedure TRtcHttpServer.TriggerDataSent;
  begin
  if FWriteCount>0 then
    Timeout.DataSent;
  EnterEvent;
  try
    if FWriteCount>0 then
      begin
      CallDataSent;
      Flush;

      if Response.Done then
        if Request.Close then
          Disconnect; // make sure we close the connection, as requested by the client.
      end;

    if not isClosing then
      begin
      CallReadyToSend;
      Flush;

      if (FWriteCount>0) and Response.Done then
        if Request.Close then
          Disconnect; // make sure we close the connection, as requested by the client.
      end;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcHttpServer.TriggerDataOut;
  begin
  inherited;
  Flush;
  end;

procedure TRtcHttpServer.TriggerInvalidRequest;
  begin
  EnterEvent;
  try
    CallInvalidRequest;
    Flush;

    Disconnect;
  finally
    LeaveEvent;
    end;
  end;

constructor TRtcHttpServer.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FWriteBuffer:=TRtcHugeString.Create;
  FWritten:=False;
  end;

destructor TRtcHttpServer.Destroy;
  begin
  FWriteBuffer.Free;
  inherited;
  end;

procedure TRtcHttpServer.SetRequest(const Value: TRtcServerRequest);
  begin
  inherited SetRequest(Value);
  if assigned(Con) then
    TMyProvider(Con).Request:=Request;
  end;

procedure TRtcHttpServer.SetResponse(const Value: TRtcServerResponse);
  begin
  inherited SetResponse(Value);
  if assigned(Con) then
    TMyProvider(Con).Response:=Response;
  end;

end.
