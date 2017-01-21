{
  @html(<b>)
  Plugable Message Server component
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  Introducing the @html(<b>) @Link(TRtcMessageServer) @html(</b>) component: @html(<br>)
  Plugable Server component can be used for direct client-server in-memory connections, or
  for "plugging" RTC functions/applications into third-party Servers (like NexusDB Server).
}
unit rtcMsgSrv;

{$INCLUDE rtcDefs.inc}

interface

uses
  Windows, Classes,

  rtcSyncObjs,
  rtcInfo,
  rtcConn,

  rtcFastStrings,
  rtcTransports,
  rtcDataSrv;

type
  { @Abstract(Message Server Connection component)

    Methods to check first:
    @html(<br>)
    @Link(TRtcDataServer.Request), @Link(TRtcConnection.Read) - Read client request
    @html(<br>)
    @Link(TRtcDataServer.Response), @Link(TRtcMessageServer.WriteHeader), @Link(TRtcMessageServer.Write) - Write result to client
    @html(<br><br>)

    Events to check first:
    @html(<br>)
    @Link(TRtcServer.OnListenStart) - Listener Started
    @html(<br>)
    @Link(TRtcConnection.OnConnect) - new Client connected
    @html(<br>)
    @Link(TRtcConnection.OnDataReceived) - Data available from client (check @Link(TRtcDataServer.Request))
    @html(<br>)
    @Link(TRtcConnection.OnDataSent) - Data sent to client (buffer now empty)
    @html(<br>)
    @Link(TRtcConnection.OnDisconnect) - one Client disconnected
    @html(<br>)
    @Link(TRtcServer.OnListenStop) - Listener Stopped
    @html(<br><br>)

    Check @Link(TRtcDataServer), @Link(TRtcServer) and @Link(TRtcConnection) for more info.
    }
  TRtcMessageServer = class(TRtcDataServer, IRTCMessageReceiver)
  private
    ConnPool:TList;
    FCS:TRtcCritSec;

    FWritten:boolean;
    FWriteBuffer:TRtcHugeString;

    function GetConnection:TRtcMessageServer;
    procedure PutConnection(conn:TRtcMessageServer);
    procedure CloseAllConnections;

  protected
    // @exclude
    procedure CopyFrom(Dup: TRtcServer); override;

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
    procedure SetRequest(const Value: TRtcServerRequest); override;
    // @exclude
    procedure SetResponse(const Value: TRtcServerResponse); override;
  public

    // This will always return TRUE for TRtcMessageServer.
    function isExtension:boolean; override;

    { aRequest contains data which needs to be processed,
      aReply will contain the response (if any).

      This method is called directly on this component and can
      be used to process data as it arrives (partial content),
      but it is NOT to be used from more than one thread at a time,
      nor for processing more than one physical connection,
      since it has an internal state specific to the connection. }
    procedure ProcessData(aRequest, aReply: TStream);

    { aRequest is a stream containing a complete HTTP request,
      aReply is a stream which will receive a complete HTTP response.
      This method is tread-safe and can be called from all threads at the same time,
      because a new replica of the connection component will be created internaly
      and used for processing the request, and ... destroyed before the method returns. }
    procedure ProcessMessage(aRequest, aReply: TStream);

    // @exclude
    constructor Create(AOwner: TComponent); override;
    // @exclude
    destructor Destroy; override;

    // Constructor
    class function New:TRtcMessageServer;

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
    end;

implementation

uses
  SysUtils,
  rtcConnProv,
  rtcMsgSrvProv; // ISAPI Server Provider

type
  TMyProvider = TRtcMessageServerProvider;

{ TRtcMessageServer }

constructor TRtcMessageServer.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FCS:=TRtcCritSec.Create;

  ConnPool:=nil;

  FWriteBuffer:=TRtcHugeString.Create;
  FWritten:=False;
  end;

destructor TRtcMessageServer.Destroy;
  begin
  CloseAllConnections;

  FWriteBuffer.Free;
  FCS.Free;
  inherited;
  end;

function TRtcMessageServer.GetConnection:TRtcMessageServer;
  begin
  Result:=nil;
  FCS.Enter;
  try
    if assigned(ConnPool) then
      begin
      if ConnPool.Count > 0 then
        begin
        Result:= ConnPool.items[ConnPool.Count-1];
        ConnPool.Delete(ConnPool.Count-1);
        end;
      end;
  finally
    FCS.Leave;
    end;
  { Now we either have the connection,
     or we need to create one. }
  if Result=nil then
    begin
    TriggerConnectionAccepting;

    Result:=TRtcMessageServer(self.copyOf);
    end;
  end;

procedure TRtcMessageServer.PutConnection(conn:TRtcMessageServer);
  begin
  FCS.Enter;
  try
    if not assigned(ConnPool) then
      ConnPool:=TList.Create;
    ConnPool.Add(conn);
  finally
    FCS.Leave;
    end;
  end;

procedure TRtcMessageServer.CloseAllConnections;
  var
    i    :integer;
    mycon:TRtcMessageServer;
  begin
  FCS.Enter;
  try
    if assigned(ConnPool) then
      begin
      for i:= 0 to ConnPool.count - 1 do
        begin
        mycon:= TRtcMessageServer(ConnPool.items[i]);
        mycon.Release;
        end;
      ConnPool.Clear;
      ConnPool.Free;
      ConnPool:=nil;
      end;
  finally
    FCS.Leave;
    end;
  end;

class function TRtcMessageServer.New: TRtcMessageServer;
  begin
  Result:=Create(nil);
  end;

function TRtcMessageServer.CreateProvider:TObject;
  begin
  if not assigned(Con) then
    begin
    Con:=TMyProvider.Create;
    SetTriggers;
    end;
  Result:=Con;
  end;

procedure TRtcMessageServer.CopyFrom(Dup: TRtcServer);
  begin
  inherited CopyFrom(Dup);
  end;

procedure TRtcMessageServer.SetParams;
  begin
  inherited;
  if assigned(Con) then
    begin
    TMyProvider(Con).Request:=Request;
    TMyProvider(Con).Response:=Response;
    end;
  end;

procedure TRtcMessageServer.WriteHeader(SendNow:boolean=True);
  begin
  if assigned(Con) and (State<>conInactive) then
    begin
    if Response.Sending then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
    TMyProvider(Con).WriteHeader;
    end;
  end;

procedure TRtcMessageServer.WriteHeader(const HeaderText: string; SendNow:boolean=True);
  begin
  if assigned(Con) and (State<>conInactive) then
    begin
    if Response.Sending then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
    TMyProvider(Con).WriteHeader(HeaderText);
    end;
  end;

procedure TRtcMessageServer.Write(const s: string='');
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

procedure TRtcMessageServer.Flush;
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

      TMyProvider(Con).WriteHeader;
      end;

    if FWriteBuffer.Size>0 then
      begin
      Temp:= FWriteBuffer.Get;
      FWriteBuffer.Clear;

      Con.Write(Temp);
      Temp:='';
      end;
    end;
  end;

procedure TRtcMessageServer.TriggerDataReceived;
  begin
  inherited;
  Flush;
  end;

procedure TRtcMessageServer.TriggerDataSent;
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

procedure TRtcMessageServer.TriggerDataOut;
  begin
  inherited;
  Flush;
  end;

procedure TRtcMessageServer.SetRequest(const Value: TRtcServerRequest);
  begin
  inherited SetRequest(Value);
  if assigned(Con) then
    TMyProvider(Con).Request:=Request;
  end;

procedure TRtcMessageServer.SetResponse(const Value: TRtcServerResponse);
  begin
  inherited SetResponse(Value);
  if assigned(Con) then
    TMyProvider(Con).Response:=Response;
  end;

procedure TRtcMessageServer.ProcessMessage(aRequest, aReply: TStream);
  var
    Server:TRtcMessageServer;
  begin
  Server:=GetConnection;
  try
    Server.EnterEvent;
    try
      Server.ProcessData(aRequest, aReply);
      if not Server.Response.Sent then
        raise Exception.Create('Error! A complete response has to be sent from ProcessMessage!');
    finally
      Server.LeaveEvent;
      end;
  finally
    PutConnection(Server);
    end;
  end;

procedure TRtcMessageServer.ProcessData(aRequest, aReply: TStream);
  begin
  TMyProvider(Con).ExecuteRequest(aRequest, aReply);
  end;

function TRtcMessageServer.isExtension: boolean;
  begin
  Result:=True;
  end;

end.
