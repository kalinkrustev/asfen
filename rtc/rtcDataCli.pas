{
  @html(<b>)
  Data Client Components
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  This unit implements a set of Client-side Data components. @Link(TRtcDataClient)
  is the base class for all Request/Response based client connections. It implements the
  mechanisms used by components like @Link(TRtcDataClientLink) @Link(TRtcDataRequest) and
  @Link(TRtcClientModule) and so prepares the road for different connection providers,
  which will all be able to use all higher-level RTC components. You won't be creating
  TRtcDataClient components, since it only implements the mechanism for working with
  other Data-related RTC components, but it doesn't implement a communication protocol.
  First RTC component which implements the connection is @Link(TRtcHttpClient). You should
  use that component if you want to communicate with a Http Server over TCP/IP.
}
unit rtcDataCli;

{$INCLUDE rtcDefs.inc}

interface

uses
  Windows,Messages,
  Classes,SysUtils,

  {$IFNDEF FPC}
  Forms,
  {$ENDIF}

  rtcInfo,
  rtcConn,

  memObjList,

  rtcSyncObjs,
  rtcThrPool;

type
  { @abstract(Basic Request Info)

    Objects of this type (inherited) are created for every request passed to DataClient
    and destroyed by DataClient after the Request has been processed or rejected.

    @exclude }
  TRtcClientRequestInfo=class
  private
    FWasInjected: boolean;
    FWasAborted: boolean;

  public
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_ResponseData(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_ResponseAbort(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_ResponseReject(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_BeginRequest(Sender:TRtcConnection); virtual; abstract;

    // @exclude
    procedure Call_DataOut(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_DataIn(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_DataSent(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_ReadyToSend(Sender:TRtcConnection); virtual; abstract;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); virtual; abstract;

    // @exclude
    procedure Call_DataReceived(Sender:TRtcConnection); virtual; abstract;

    // @exclude
    procedure Call_ConnectLost(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_RepostCheck(Sender:TRtcConnection); virtual; abstract;

    // @exclude
    function Get_Request:TRtcClientRequest; virtual; abstract;

    // @exclude
    property WasInjected:boolean read FWasInjected write FWasInjected default false;
    // @exclude
    property WasAborted:boolean read FWasAborted write FWasAborted default false;
    end;

  { @abstract(Client Session) }
  TRtcClientSession=class(TRtcSession)
  protected
    // @exclude
    FCon:TRtcConnection;

  public
    // @exclude
    procedure Init;

    // Open a new session with this ID
    procedure Open(const _ID:string); virtual;
    procedure Close; virtual;
    end;

  // @abstract(All Components used by the DataClient are derived from this class)
  TRtcClientComponent = class(TRtcComponent);

  TRtcAbsDataClientLink = class; // forward

  // @exclude
  TRtcDataClientLinkList = class
  private
    FList:TList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Value:TRtcAbsDataClientLink);
    procedure Remove(Value:TRtcAbsDataClientLink);

    procedure RemoveAll;

    function Count:integer;
    function Get(index:integer):TRtcAbsDataClientLink;
    end;

  { @Abstract(Data Client Connection component)

    Most of the events published by TRtcDataClient will NOT be defined directly on the
    @Link(TRtcDataClient), but instead of that will be passed on to posted DataRequest components,
    so that each DataRequest can process its request (send request out and accept response).
    Posted requests will be processed one-by-one, even if they are posted all at the same time.

    Properties to check first:
    @html(<br>)
    @Link(TRtcConnection.ServerAddr) - Address to connect to
    @html(<br>)
    @Link(TRtcConnection.ServerPort) - Port to connect to
    @html(<br><br>)

    Methods to check first:
    @html(<br>)
    @Link(TRtcClient.Connect) - Connect to Server
    @html(<br>)
    @Link(TRtcDataClient.Request), @Link(TRtcDataClient.WriteHeader), @Link(TRtcConnection.Write) - Write result to server
    @html(<br>)
    @Link(TRtcDataClient.Response), @Link(TRtcConnection.Read) - Read Server's response
    @html(<br>)
    @Link(TRtcConnection.Disconnect) - Disconnect from Server
    @html(<br><br>)

    Events to check first:
    @html(<br>)
    @Link(TRtcConnection.OnConnect) - Connected to Server
    @html(<br>)
    @Link(TRtcConnection.OnDataSent) - Data sent to server (buffer now empty)
    @html(<br>)
    @Link(TRtcDataClient.OnResponseAbort) - Connection Lost while receiving response, but Repost was not triggered.
    @html(<br>)
    @Link(TRtcConnection.OnDisconnect) - Disconnected from Server
    @html(<br><br>)

    Check @Link(TRtcClient) and @Link(TRtcConnection) for more info.
    }
  TRtcDataClient = class(TRtcClient)
  private
    FOnRepostCheck:TRtcNotifyEvent;
    FOnBeginRequest:TRtcNotifyEvent;
    FOnResponseDone:TRtcNotifyEvent;
    FOnResponseData:TRtcNotifyEvent;
    FOnResponseAbort:TRtcNotifyEvent;
    FOnResponseReject:TRtcNotifyEvent;

    FOnSessionOpen:TRtcNotifyEvent;
    FOnSessionClose:TRtcNotifyEvent;

    FMayInsertRequest:boolean;
    FRequestInserted:boolean;

    FRequestSkipped:integer;

    FAutoConnect:boolean;

    FCS:TRtcCritSec;
    FSession:TRtcClientSession;

    FRequestList:TList;
    FActiveRequest:TRtcClientRequestInfo;
    FIdle:boolean;

    FDataClientLinks:TRtcDataClientLinkList;

    FMyRequest, FRequest:TRtcClientRequest;
    FMyResponse, FResponse:TRtcClientResponse;

    procedure CheckRequestSkipped;

    function GetAutoConnect: boolean;
    procedure SetAutoConnect(const Value: boolean);

  protected

    // @exclude
    function isConnectionRequired:boolean; override;

    // @exclude
    procedure AddDataClientLink(Value:TRtcAbsDataClientLink);
    // @exclude
    procedure RemoveDataClientLink(Value:TRtcAbsDataClientLink);
    // @exclude
    procedure RemoveAllDataClientLinks;

    // @exclude
    procedure SetRequest(const Value: TRtcClientRequest); virtual;
    // @exclude
    procedure SetResponse(const Value: TRtcClientResponse); virtual;

    // @exclude
    procedure CallConnect; override;
    // @exclude
    procedure CallDataReceived; override;
    // @exclude
    procedure CallDataOut; override;
    // @exclude
    procedure CallDataIn; override;
    // @exclude
    procedure CallDataSent; override;
    // @exclude
    procedure CallReadyToSend; override;
    // @exclude
    procedure CallConnectLost; override;
    // @exclude
    procedure CallConnectFail; override;
    // @exclude
    procedure CallConnectError(E: Exception); override;

    // @exclude
    procedure AddRequest(Req:TRtcClientRequestInfo);
    // @exclude
    procedure RemoveAllRequests;
    // @exclude
    procedure RemoveRequest;
    // @exclude
    procedure StartRequest;

    // @exclude
    procedure PrepareNextRequest;

    // @exclude
    function CheckRequestWork:boolean;

    { @exclude
      Post a 'StartRequest' Job }
    procedure PostStartRequest;

    { Check if connection is Idle (no requests waiting or disconnected)
      @exclude }
    function isIdle:boolean; virtual;

  public
    // @exclude
    constructor Create(AOwner: TComponent); override;
    // @exclude
    destructor Destroy; override;

    { @exclude
      Internal function: Post a new Request Object.
      When posting from inside a RTC event or a remote function,
      "FromInsideEvent" parameter has to be TRUE to avoid memory consumption. }
    procedure PostRequest(Req:TRtcClientRequestInfo; FromInsideEvent:boolean=False); virtual;

    { @exclude
      Internal function: Insert a Request before active request.
      This procedure may ONLY be called from BeginRequest event
      to place another request before the active request. }
    procedure InsertRequest(Req:TRtcClientRequestInfo); virtual;

    // @exclude
    procedure CallBeginRequest; virtual;
    // @exclude
    procedure CallResponseDone; virtual;
    // @exclude
    procedure CallResponseData; virtual;
    // @exclude
    procedure CallResponseAbort; virtual;
    // @exclude
    procedure CallResponseReject; virtual;
    // @exclude
    procedure CallSessionOpen; virtual;
    // @exclude
    procedure CallSessionClose; virtual;
    // @exclude
    procedure CallRepostCheck; virtual;

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
    procedure Flush; virtual; abstract;

    // You can call WriteHeader to send the Request header out.
    procedure WriteHeader(SendNow:boolean=True); overload; virtual; abstract;
    { You can call WriteHeader with empty 'HeaderText' parameter to
      tell the component that you do not want any HTTP header to be sent. }
    procedure WriteHeader(const HeaderText: string; SendNow:boolean=True); overload; virtual; abstract;

    // Skip all requests (RequestAborted events will NOT BE triggered!!!)
    procedure SkipRequests; virtual;
    // Cancel all requests (will fire all RequestAborted events)
    procedure CancelRequests; virtual;

    // Check request count (number of requests waiting to be processed)
    function RequestCount:integer; virtual;
    { Wait for all posted requests and function calls to complete,
      be aborted, be calceled, or for the connection to close. @html(<br>)
      Using a timeout (seconds) you can specify how long you want to wait.
      (0=forever). Returns TRUE only if there are no more requests waiting.
      Returns FALSE if timed-out or terminating or connection can't be open
      or connection closed on purpose. }
    function WaitForCompletion(UserInteractionAllowed:boolean=False; _Timeout:cardinal=0):boolean; virtual;

    { This connection's Session info.
      If you will be using multiple client connections and
      need to store session information, you have to start a
      separate session for every client connection.
      @html(<br><br>)
      There is a difference between this DataClient's Session object and
      the DataServer's Session object. DataClient's Session object stays
      permamently defined all the time, from the point of object creation to its destruction.
      Different from the Server connection, }
    property Session:TRtcClientSession read FSession;

    // Another request has just been inserted before this one.
    property RequestInserted:boolean read FRequestInserted;

    { Access to current request information.
      Use Request property to prepare the request header.
      Here you can set all header variables and parameters.
      If there is no content body to send out (only header), you will at
      least have to call 'WriteHeader', or 'Write' without parameters once. }
    property Request:TRtcClientRequest read FRequest write SetRequest;
    { Access to current response information.
      Use Response property to read the response information received.
      Here is all the info that was available in response header.
      To read response's body, use the Read function. }
    property Response:TRtcClientResponse read FResponse write SetResponse;

  published
    { You have two ways of working with connections. One is to open a connection
      on application start by calling "Connect" and using "ReconnectOn" to keep the
      connection open until your application closes, the other is to use implicit
      connects when a connection is required (when you post a request of any kind). @html(<br>)
      You should set AutoConnect to TRUE if you do not want your connection to remain
      open all the time, but also do not want to call "Connect" when you need a connection.
      For connection to remain open as long as there are requests waiting to be processed,
      you will also need to set the appropriate ReconnectOn parameters. @html(<br><br>)

      When AutoConnect is TRUE, connection will be automaticaly opened when you
      post a request (no need to call Connect) and "ReconnectOn" parameters will be
      used to reconnect only if there are requests waiting in the queue. By using Timeout
      parameters, your connection will close after specified timeout and stay closed
      until needed again, when a new connection will be automaticaly initiated. @html(<br><br>)

      When AutoConnect is FALSE, connection has to be opened explicitly by calling "Connect"
      and "ReconnectOn" parameters will be used to keep the connection open, even if there
      are no requests waiting in the queue. }
    property AutoConnect:boolean read GetAutoConnect write SetAutoConnect default False;

    { Called before each request start.
      You can use this event to prepare the request. }
    property OnBeginRequest:TRtcNotifyEvent read FOnBeginRequest write FOnBeginRequest;
    { Called after the last DataReceived event for the response, when a response is done.
      You can use this event to react to the final response. }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { Called every time a data package comes from Server, immediatelly after OnDataReceived. }
    property OnResponseData:TRtcNotifyEvent read FOnResponseData write FOnResponseData;
    { Called after OnConnectLost,OnConnectFail and OnConnectError if Request was not reposted. }
    property OnRepostCheck:TRtcNotifyEvent read FOnRepostCheck write FOnRepostCheck;
    { Called after OnRepostCheck if Request was not reposted. }
    property OnResponseAbort:TRtcNotifyEvent read FOnResponseAbort write FOnResponseAbort;
    { Called if response has been rejected by calling Response.Reject. }
    property OnResponseReject:TRtcNotifyEvent read FOnResponseReject write FOnResponseReject;

    { Called after a new session has been opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { Called before an existing session is about to get closed. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;

    { This event will be triggered every time a chunk of your data
      prepared for sending has just been sent out. To know
      exactly how much of it is on the way, use the @Link(TRtcConnection.DataOut) property.
      @html(<br><br>)

      NOTE: Even though data has been sent out, it doesn't mean that
      the other side already received it. It could also be that connection will
      break before this package reaches the other end. }
    property OnDataOut;
    { This event will be triggered every time a chunk of data
      has just come in (received). To know exactly how much of it
      has just arrived, use the @Link(TRtcConnection.DataIn) property. }
    property OnDataIn;
    end;

  TRtcDataClientLink=class; // forward

  { @abstract(DataClient Link wrapper) }
  TRtcAbsDataClientLink=class(TRtcClientComponent)
  private
    FClient: TRtcDataClient;
    FLink: TRtcDataClientLink;
    FAutoSync: boolean;

  protected
    // @exclude
    function CheckLink(Value:TRtcAbsDataClientLink):boolean; virtual;
    // @exclude
    procedure RemoveLink(Value:TRtcAbsDataClientLink); virtual;
    // @exclude
    procedure RemoveClient(Value:TRtcDataClient); virtual;

    // @exclude
    function GetClient: TRtcDataClient; virtual;
    // @exclude
    procedure SetClient(const Value: TRtcDataClient); virtual;

    // @exclude
    function GetLink: TRtcDataClientLink; virtual;
    // @exclude
    procedure SetLink(const Value: TRtcDataClientLink); virtual;

    // @exclude
    procedure Call_BeginRequest(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_ResponseData(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_ResponseAbort(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_ResponseReject(Sender:TRtcConnection); virtual; abstract;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); virtual; abstract;

    // @exclude
    procedure Call_DataReceived(Sender:TRtcConnection); virtual; abstract;

    // @exclude
    procedure Call_DataOut(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_DataIn(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_DataSent(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_ReadyToSend(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_ConnectLost(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_RepostCheck(Sender:TRtcConnection); virtual; abstract;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    // Post a new Request Object
    procedure PostRequest(Req:TRtcClientRequestInfo; FromInsideEvent:boolean=False); virtual;
    { Insert a Request before active request.
      This procedure may ONLY be called from BeginRequest event
      to place another request before the active request. }
    procedure InsertRequest(Req:TRtcClientRequestInfo); virtual;

    // Skip all requests posted to the Client Connection used (RequestAborted events will NOT BE triggered!!!)
    procedure SkipRequests; virtual;
    // Cancel all requests posted to the Client Connection used and fire RequestAborted events
    procedure CancelRequests; virtual;

    // Check request count at Client Connection used (number of requests waiting to be processed)
    function RequestCount:integer; virtual;

    { Wait for all posted requests and function calls to complete,
      be aborted, be calceled, or for the connection to close. @html(<br>)
      Using a timeout (seconds) you can specify how long you want to wait.
      (0=forever). Returns TRUE only if there are no more requests waiting.
      Returns FALSE if timed-out or terminating or connection can't be open
      or connection closed on purpose. }
    function WaitForCompletion(UserInteractionAllowed:boolean=False; Timeout:cardinal=0):boolean; virtual;

  published
    { If all events which your component implements have to access the GUI,
      to avoid checking the "Sender.inMainThread" and calling Sender.Sync(Event)
      for every event, you can se this AutoSyncEvent property to true,
      which will ensure that any event assigned to this component will
      be called from the main thread (synchronized, when needed). }
    property AutoSyncEvents:boolean read FAutoSync write FAutoSync default false;
    { You can link your components (one or more) to a DataClientLink component
      by assigning your @Link(TRtcDataClientLink) component to chind component's Link property.
      Doing this, you only have to set the Client property for the master
      DataClientLink component and don't need to do it for every single
      DataRequest component. }
    property Link:TRtcDataClientLink read GetLink write SetLink;
    { You can also link your components (one or more) directly to your
      DataClient connection component by assigning your
      @Link(TRtcDataClient) connection component to this child component's Client property.
      This is useful if you don't want to use a DataClientLink. }
    property Client:TRtcDataClient read GetClient write SetClient;
    end;

  { @abstract(DataClient Link, used to group Data Requests)

    You can use TRtcDataClientLink components to group several related
    @Link(TRtcDataRequest) components. Simply set this component as the
    Link property for all your RtcDataSource components, so that
    you don't have to set the Client property for every single
    TRtcDataRequest component separately. This is useful especially
    when the component is used in a datamodule or a form without
    DataClient and you need to link all the components to
    a DataClient which is on another datamodule or form.
    @html(<br><br>)

    Check @Link(TRtcAbsDataClientLink) for more info. }
  TRtcDataClientLink=class(TRtcAbsDataClientLink)
  private
    FOnBeginRequest:TRtcNotifyEvent;
    FOnResponseDone:TRtcNotifyEvent;
    FOnResponseData:TRtcNotifyEvent;
    FOnResponseAbort:TRtcNotifyEvent;
    FOnResponseReject:TRtcNotifyEvent;
    FOnSessionOpen:TRtcNotifyEvent;
    FOnSessionClose:TRtcNotifyEvent;
    FOnRepostCheck:TRtcNotifyEvent;

  protected
    // @exclude
    FDataClientLinks:TRtcDataClientLinkList;

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
    procedure AddChildLink(Value:TRtcAbsDataClientLink);
    // @exclude
    procedure RemoveChildLink(Value:TRtcAbsDataClientLink);
    // @exclude
    procedure RemoveAllChildLinks;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

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
    procedure Call_RepostCheck(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); override;

  published
    { Use this event to initialize child requests linked to this DataLink }
    property OnBeginRequest:TRtcNotifyEvent read FOnBeginRequest write FOnBeginRequest;
    { Use this event to add additional control after each OnDataReceived package. }
    property OnResponseData:TRtcNotifyEvent read FOnResponseData write FOnResponseData;
    { Use this event to finalize child requests linked to this DataLink }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { Called after OnConnectLost, OnConnectFail and OnConnectError if request is not finished and not marked for reposting }
    property OnRepostCheck:TRtcNotifyEvent read FOnRepostCheck write FOnRepostCheck;
    { Called after OnRepostCheck if request is not finished and is not marked for reposting. }
    property OnResponseAbort:TRtcNotifyEvent read FOnResponseAbort write FOnResponseAbort;
    { Called after Response has been rejected by calling Response.Reject }
    property OnResponseReject:TRtcNotifyEvent read FOnResponseReject write FOnResponseReject;
    { Called after a new Session has been opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { Called before an existing session is about to be closed. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;
    end;

  { @abstract(Dual DataClient Link, used to create a connection pool)

    You can use TRtcDualDataClientLink components to create a small pool
    of client connections. When posting requests through this component,
    you will never really know which connection the request will be posted
    from, since the component itself will determine that, depending on 
    the number of currently active requests on each connection component.
    @html(<br><br>)

    Check @Link(TRtcDataClientLink) for more info. }
  TRtcDualDataClientLink=class(TRtcDataClientLink)
  private
    FClient2: TRtcDataClient;
    FLink2: TRtcDataClientLink;

  protected
    // @exclude
    function CheckLink(Value:TRtcAbsDataClientLink):boolean; override;
    // @exclude
    procedure RemoveLink(Value:TRtcAbsDataClientLink); override;
    // @exclude
    procedure RemoveClient(Value:TRtcDataClient); override;

    // @exclude
    function GetClient2: TRtcDataClient; virtual;
    // @exclude
    procedure SetClient2(const Value: TRtcDataClient); virtual;
    // @exclude
    procedure SetClient(const Value: TRtcDataClient); override;

    // @exclude
    function GetLink2: TRtcDataClientLink; virtual;
    // @exclude
    procedure SetLink2(const Value: TRtcDataClientLink); virtual;
    // @exclude
    procedure SetLink(const Value: TRtcDataClientLink); override;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    // @exclude
    procedure Call_ConnectLost(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_DataReceived(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_DataOut(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_DataIn(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_DataSent(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_ReadyToSend(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_BeginRequest(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_ResponseData(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_RepostCheck(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_ResponseAbort(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_ResponseReject(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_SessionClose(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_SessionOpen(Sender: TRtcConnection); override;

    // Post a new Request Object
    procedure PostRequest(Req:TRtcClientRequestInfo; FromInsideEvent:boolean=False); override;
    { Insert a Request before active request.
      This procedure may ONLY be called from BeginRequest event
      to place another request before the active request. }
    procedure InsertRequest(Req:TRtcClientRequestInfo); override;

    // Skip all requests posted to the Client Connection used (RequestAborted events will NOT BE triggered!!!)
    procedure SkipRequests; override;
    // Cancel all requests posted to the Client Connection used and fire RequestAborted events
    procedure CancelRequests; override;

    // Check request count at Client Connection used (number of requests waiting to be processed)
    function RequestCount:integer; override;

    { Wait for all posted requests and function calls to complete,
      be aborted, be calceled, or for the connection to close. @html(<br>)
      Using a timeout (seconds) you can specify how long you want to wait.
      (0=forever). Returns TRUE only if there are no more requests waiting.
      Returns FALSE if timed-out or terminating or connection can't be open
      or connection closed on purpose. }
    function WaitForCompletion(UserInteractionAllowed:boolean=False; Timeout:cardinal=0):boolean; override;

  published
    { You can link your components (one or more) to a DataClientLink component
      by assigning your @Link(TRtcDataClientLink) component to child component's Link property.
      Doing this, you only have to set the Client property for the master
      DataClientLink component and don't need to do it for every single
      DataRequest component. }
    property Link2:TRtcDataClientLink read GetLink2 write SetLink2;
    { You can also link your components (one or more) directly to your
      DataClient connection component by assigning your
      @Link(TRtcDataClient) connection component to this child component's Client property.
      This is useful if you don't want to use a DataClientLink. }
    property Client2:TRtcDataClient read GetClient2 write SetClient2;
    end;

  { @abstract(DataRequest Info Object)

    This object is created and filled by TRtcDataRequest,
    when you Post or Insert a DataRequest.

    @exclude }
  TRtcDataRequestInfo=class(TRtcClientRequestInfo)
  protected
    // @exclude
    FRequest:TRtcClientRequest;

    // @exclude
    FEvents:TRtcAbsDataClientLink;

  public
    // @exclude
    procedure Call_BeginRequest(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseData(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); override;
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

    // @exclude
    function Get_Request:TRtcClientRequest; override;

  public
    // Standard constructor
    constructor Create; virtual;

    // @exclude
    destructor Destroy; override;

    { Request Info, will be destroyed after the Request has been processed,
      has to be assigned a valid TRtcClientRequest object before posting. }
    property Request:TRtcClientRequest read FRequest write FRequest;

    { This DataClient Link component will be used to call events. }
    property Events:TRtcAbsDataClientLink read FEvents write FEvents;
    end;

  // @exclude
  TRtcDataRequestData=class
  public
    FRequest:TRtcClientRequest;

    constructor Create; virtual;
    destructor Destroy; override;
    end;

  { @abstract(Data Request, used to Prepare+Post or Insert+Prepare Requests for DataClient)

    This is the component you have to use to work with DataClient.
    Put TRtcDataRequest on a form or a datamodule and define events
    at design-time, which will be used for default request processing.
    @html(<br><br>)
    Then, at runtime, there are 2 ways you can use this DataRequest:
    @html(<br>)
    1.) From the Main thread, you can prepare the request using the
       @Link(TRtcDataRequest.Request) property and call @link(TRtcDataRequest.Post)
       to post the prepared request to its associated DataClient, or ...
    @html(<br>)
    2.) From inside a BeginRequest event handler (which is called by the DataClient
       connection component), you can insert this DataRequest, so that your active
       DataRequest is put on the 2nd position and this request is put on the first
       place, by calling the @Link(TRtcDataRequest.Insert) method. After calling
       @link(TRtcDataRequest.Insert), immediatelly a new @Link(TRtcDataClient.Request)
       is created for this inserted DataRequest, so you can initialize the request
       before exiting your event handler.
    @html(<br><br>)

    Check @Link(TRtcAbsDataClientLink) for more info. }
  TRtcDataRequest=class(TRtcAbsDataClientLink)
  private
    FCS:TRtcCritSec;
    FMyData:TObjList;
    FMainThrData:TRtcDataRequestData;
    FHyperThreading: boolean;

    function CheckMyData:TRtcDataRequestData;
    function GetMyData:TRtcDataRequestData;
    procedure ClearMyData;

    function GetRequest: TRtcClientRequest;

  protected
    // @exclude
    FAutoRepost:integer;


    // @exclude
    FOnBeginRequest: TRtcNotifyEvent;
    // @exclude
    FOnResponseData: TRtcNotifyEvent;
    // @exclude
    FOnResponseDone: TRtcNotifyEvent;
    // @exclude
    FOnResponseAbort: TRtcNotifyEvent;
    // @exclude
    FOnResponseReject: TRtcNotifyEvent;

    // @exclude
    FOnSessionOpen: TRtcNotifyEvent;
    // @exclude
    FOnSessionClose: TRtcNotifyEvent;

    // @exclude
    FOnConnectLost: TRtcNotifyEvent;
    // @exclude
    FOnDataReceived: TRtcNotifyEvent;
    // @exclude
    FOnReadyToSend: TRtcNotifyEvent;
    // @exclude
    FOnDataOut: TRtcNotifyEvent;
    // @exclude
    FOnDataIn: TRtcNotifyEvent;
    // @exclude
    FOnDataSent: TRtcNotifyEvent;
    // @exclude
    FOnRepostCheck: TRtcNotifyEvent;

    // @exclude
    procedure Call_BeginRequest(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseData(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); override;
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

    { Post this DataRequest to assigned DataClient.
      @html(<br><br>)

      After a call to Post, an object will be created to hold the
      prepared Request info, after which the Request property will be cleared,
      so that you can prepare and post new requests immediatelly,
      without waiting for the last request to complete.
      @html(<br><br>)

      When posting a new Request from inside an Event of a running request/response loop,
      "FromInsideEvent" parameter has to be TRUE to avoid memory consumption and
      always pass the Sender:TRtcConnection parameter through.

      Events assigned to this TRtcDataRequest will not be removed nor cleared,
      so you can define them at design-time and not worry about them at runtime. }
    procedure Post(FromInsideEvent:boolean=False; Sender:TRtcConnection=nil); virtual;

    { Insert this request before the active request.
      @html(<br><br>)

      DataRequest objects which are used for automatic initialisation
      can be inserted before the active request, but ONLY from inside
      the active request's BeginRequest event handler. After calling
      this Insert procedure, a new Request is created for the inserted
      DataRequest and can/should be modified/prepared before exiting the
      BeginRequest event. After BeginRequest event exists, the inserted
      DataRequest's BeginRequest events will be called, as if this event
      was posted before the one that inserted it.

      To make sure the request will be inserted to same connection,
      always pass the Sender:TRtcConnection parameter through. }
    procedure Insert(Sender:TRtcConnection=nil); virtual;

    { ONLY use this Request property to prepare a request BEFORE posting.
      @html(<br>)
      DO NOT directly use this property when processing the request.
      After a request has been posted, it is moved to the DataClient,
      so you can access it (from events) using Sender's Request property. }
    property Request:TRtcClientRequest read GetRequest;

  published
    { If you want to enable the possibility to use this Data Request to send requests
      from multiple threads AT THE SAME TIME, where this component will acs as if
      it were X components, one for each thread, simply set HyperThreading to TRUE. @html(<br><br>)

      This is useful if you need to send requests to another Server from
      within your Server running in multi-threaded mode and want to use only one set of
      rtcHttpClient/rtcDataRequest components for all clients connected to your Server.
      Even in HyperThreading mode, only properties and methods needed to prepare and post
      the request (Request and Post) will use a separate copy for each thread, while all
      other properties and methods exist only once for all threads, so don't try to modify
      them while your application is actively using the component in multi-threaded mode. @html(<br><br>)

      Leave HyperThreading as FALSE to use this component "the stadard way" (for example,
      when you're writing a client application where requests are posted from the main thread
      or if you are creating a separate component for every thread that needs it). }
    property HyperThreading:boolean read FHyperThreading write FHyperThreading default False;

    { Set this property to a value other than 0 (zero) if you want the DataRequest to
      auto-repost any request up to "AutoRepost" times, in case the connection gets lost
      while sending data to server or receiving data from server.
      AutoRepost = -1 means that request should be reposted infinitely. }
    property AutoRepost:integer read FAutoRepost write FAutoRepost default 0;

    { This event will be called when DataClient component is
      ready to start sending the request out.
      @html(<br><br>)
      If the request is already in memory (not a large file on disk),
      send it out using Sender's WriteHeader and/or Write methods.
      If the request is too large to be sent out at once (maybe a big file
      on disk), you should at least send the Request Header out in this event.
      @html(<br><br>)
      If you DO NOT call Write and/or WriteHeader from this event,
      then this Request will be skipped by the DataClient. }
    property OnBeginRequest:TRtcNotifyEvent read FOnBeginRequest write FOnBeginRequest;
    { This event will be called after each DataReceived event for this request
      and can be used to access info prepared by the OnDataReceived event. }
    property OnResponseData:TRtcNotifyEvent read FOnResponseData write FOnResponseData;
    { This event will be called after the last DataReceived event for this request,
      read after the request has been sent out and a complete response was received (Response.Done). }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { This event will be called after ConnectLost, ConnectFail and ConnectError events if request was not marked for reposting.

      If you want to re-post the request, you should call Request.Repost from
      this event, or the Request will NOT be reposted and OnResponseAbort
      event will be trigered. You can also use the AutoRepost property to tell the
      component to repost requests automaticaly (for a specified number of times or unlimited). }
    property OnRepostCheck:TRtcNotifyEvent read FOnRepostCheck write FOnRepostCheck;
    { This event will be called after the OnRepostCheck event if request was not marked for reposting. }
    property OnResponseAbort:TRtcNotifyEvent read FOnResponseAbort write FOnResponseAbort;

    { This event will be called after the response has been rejected by calling Response.Reject }
    property OnResponseReject:TRtcNotifyEvent read FOnResponseReject write FOnResponseReject;

    { This event will be called after a new Session has been opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { This event will be called before an existing Session is about to close. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;

    { This event will be mapped as TRtcDataClient.OnDataReceived event
      to the assigned DataClient component and called for all DataReceived
      events until the request is processed in full, skipped or rejected. }
    property OnDataReceived:TRtcNotifyEvent read FOnDataReceived write FOnDataReceived;

    { This event will be mapped as @Link(TRtcConnection.OnDataOut) event
      to the assigned DataClient component and called for all DataOut
      events until the request is processed in full, skipped or rejected. }
    property OnDataOut:TRtcNotifyEvent read FOnDataOut write FOnDataOut;

    { This event will be mapped as @Link(TRtcConnection.OnDataIn) event
      to the assigned DataClient component and called for all DataIn
      events until the request is processed in full, skipped or rejected. }
    property OnDataIn:TRtcNotifyEvent read FOnDataIn write FOnDataIn;

    { This event will be mapped as @Link(TRtcConnection.OnDataSent) event
      to the assigned DataClient component and called for all DataSent
      events until the request is processed in full, skipped or rejected. }
    property OnDataSent:TRtcNotifyEvent read FOnDataSent write FOnDataSent;

    { This event will be mapped as @Link(TRtcConnection.OnReadyToSend) event
      to the assigned DataClient component and called for all ReadyToSend
      events until the request is processed in full, skipped or rejected. }
    property OnReadyToSend:TRtcNotifyEvent read FOnReadyToSend write FOnReadyToSend;

    { This event will be mapped as @Link(TRtcClient.OnConnectLost) event
      to the assigned DataClient component and called if your connection gets
      closed while you are processing your request (sending or receiving data).
      @html(<br><br>)

      If you want to re-send the request, you can call Request.Repost from this event,
      or use the OnRepostCheck or OnResponseAborted events to do so. }
    property OnConnectLost:TRtcNotifyEvent read FOnConnectLost write FOnConnectLost;
    end;

implementation

type
  { @abstract(DataRequest Job)

    This Job is used by TRtcDataClient to post a signal to start a request

    @exclude }
  TRtcStartRequestJob=class(TRtcJob)
    Client:TRtcDataClient;

    procedure Kill; override;
    procedure Run(Thr:TRtcThread); override;
    end;

{ TRtcDataClientLinkList }

constructor TRtcDataClientLinkList.Create;
  begin
  inherited;
  FList:=TList.Create;
  end;

destructor TRtcDataClientLinkList.Destroy;
  begin
  FList.Free;
  inherited;
  end;

procedure TRtcDataClientLinkList.Add(Value: TRtcAbsDataClientLink);
  var
    idx:integer;
  begin
  idx:=FList.IndexOf(Value);
  if idx<0 then
    FList.Add(Value);
  end;

function TRtcDataClientLinkList.Count: integer;
  begin
  Result:=FList.Count;
  end;

function TRtcDataClientLinkList.Get(index:integer): TRtcAbsDataClientLink;
  begin
  if (index>=0) and (index<FList.Count) then
    Result:=TRtcAbsDataClientLink(FList.Items[index])
  else
    Result:=nil;
  end;

procedure TRtcDataClientLinkList.Remove(Value: TRtcAbsDataClientLink);
  var
    idx:integer;
  begin
  idx:=FList.IndexOf(Value);
  if idx>=0 then
    FList.Delete(idx);
  end;

procedure TRtcDataClientLinkList.RemoveAll;
  begin
  FList.Clear;
  end;

{ TRtcDataClient }

constructor TRtcDataClient.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FMyRequest:=TRtcClientRequest.Create;
  FMyResponse:=TRtcClientResponse.Create;

  FRequest:=FMyRequest;
  FResponse:=FMyResponse;

  FCS:=TRtcCritSec.Create;
  FSession:=TRtcClientSession.Create;
  FSession.FCon:=self;
  FMayInsertRequest:=False;
  FRequestInserted:=False;

  FDataClientLinks:=TRtcDataClientLinkList.Create;

  FRequestList:=TList.Create;
  FActiveRequest:=nil;
  FIdle:=True;
  FRequestSkipped:=0;
  end;

destructor TRtcDataClient.Destroy;
  begin
  RemoveAllDataClientLinks;
  FDataClientLinks.Free;
  FDataClientLinks:=nil;

  if assigned(FRequestList) then
    begin
    RemoveAllRequests;
    RemoveRequest;
    FRequestList.Free;
    FRequestList:=nil;
    end;

  FActiveRequest:=nil;
  FIdle:=True;

  FMyRequest.Free; FRequest:=nil;
  FMyResponse.Free; FResponse:=nil;

  FSession.Free;
  FCS.Free;
  inherited;
  end;

procedure TRtcDataClient.CallConnect;
  begin
  inherited;
  if MultiThreaded then
    PostStartRequest
  else
    StartRequest;
  end;

procedure TRtcDataClient.CallReadyToSend;
  begin
  if assigned(FActiveRequest) then
    begin
    FActiveRequest.Call_ReadyToSend(self);
    Flush;
    CheckRequestWork;
    end
  else
    inherited;
  end;

procedure TRtcDataClient.CallDataSent;
  begin
  if assigned(FActiveRequest) then
    begin
    FActiveRequest.Call_DataSent(self);
    Flush;
    CheckRequestWork;
    end
  else
    inherited;
  end;

procedure TRtcDataClient.CallDataOut;
  begin
  if assigned(FActiveRequest) then
    FActiveRequest.Call_DataOut(self);

  inherited;

  Flush;
  end;

procedure TRtcDataClient.CallDataIn;
  begin
  if assigned(FActiveRequest) then
    FActiveRequest.Call_DataIn(self);

  inherited;
  end;

procedure TRtcDataClient.CallDataReceived;
  begin
  if assigned(FActiveRequest) then
    begin
    FActiveRequest.Call_DataReceived(self);
    Flush;
    if CheckRequestWork then Exit;

    FActiveRequest.Call_ResponseData(self);
    Flush;
    if CheckRequestWork then Exit;

    if assigned(Request.OnData) then
      begin
      Request.OnData(self);
      Flush;
      if CheckRequestWork then Exit;
      end;

    if Response.Done then
      begin
      FActiveRequest.Call_ResponseDone(self);
      if CheckRequestWork then Exit;

      if assigned(Request.OnDone) then
        Request.OnDone(self);
      if CheckRequestWork then Exit;

      RemoveRequest;
      if MultiThreaded then
        PostStartRequest
      else
        StartRequest;
      end;
    end
  else
    inherited;
  end;

procedure TRtcDataClient.CallConnectLost;
  begin
  if assigned(FActiveRequest) then
    begin
    FActiveRequest.Call_ConnectLost(self);
    if not Response.Done and not Request.Reposting then
      begin
      FActiveRequest.Call_RepostCheck(self);
      if not Response.Done and not Request.Reposting then
        begin
        FActiveRequest.Call_ResponseAbort(self);
        if assigned(Request.OnAbort) then
          if not Request.Reposting then
            Request.OnAbort(self);
        end;
      end;

    inherited;

    if Request.Skipped then
      RemoveRequest
    else if Response.Rejected then
      begin
      FActiveRequest.Call_ResponseReject(self);
      if assigned(Request.OnReject) then
        Request.OnReject(self);
      RemoveRequest;
      end
    else if Request.Reposting then
      begin
      Request.Reposting:=False;
      Response.Clear;
      end
    else if Response.Done then
      RemoveRequest
    else
      begin
      RemoveRequest;
      // We will skip all remaining requests
      CancelRequests;
      end;

    // This will remove all skipped requests
    PrepareNextRequest;
    end
  else
    inherited;
  end;

procedure TRtcDataClient.CallConnectFail;
  begin
  PrepareNextRequest;

  if assigned(FActiveRequest) then
    begin
    FActiveRequest.Call_RepostCheck(self);
    if not Response.Done and not Request.Reposting then
      begin
      FActiveRequest.Call_ResponseAbort(self);
      if assigned(Request.OnAbort) then
        if not Request.Reposting then
          Request.OnAbort(self);
      end;

    inherited;

    if Request.Skipped then
      RemoveRequest
    else if Response.Rejected then
      begin
      FActiveRequest.Call_ResponseReject(self);
      if assigned(Request.OnReject) then
        Request.OnReject(self);
      RemoveRequest;
      end
    else if Request.Reposting then
      begin
      Request.Reposting:=False;
      Response.Clear;
      end
    else if Response.Done then
      RemoveRequest
    else
      begin
      RemoveRequest;
      // We will skip all remaining requests
      CancelRequests;
      end;

    // This will remove all skipped requests
    PrepareNextRequest;
    end
  else
    inherited;
  end;

procedure TRtcDataClient.CallConnectError(E:Exception);
  begin
  PrepareNextRequest;

  if assigned(FActiveRequest) then
    begin
    FActiveRequest.Call_RepostCheck(self);
    if not Response.Done and not Request.Reposting then
      begin
      FActiveRequest.Call_ResponseAbort(self);
      if assigned(Request.OnAbort) then
        if not Request.Reposting then
          Request.OnAbort(self);
      end;

    inherited;

    if Request.Skipped then
      RemoveRequest
    else if Response.Rejected then
      begin
      FActiveRequest.Call_ResponseReject(self);
      if assigned(Request.OnReject) then
        Request.OnReject(self);
      RemoveRequest;
      end
    else if Request.Reposting then
      begin
      Request.Reposting:=False;
      Response.Clear;
      end
    else if Response.Done then
      RemoveRequest
    else
      begin
      RemoveRequest;
      // We will skip all remaining requests
      CancelRequests;
      end;

    // This will remove all skipped requests
    PrepareNextRequest;
    end
  else
    inherited;
  end;

{ Called from TRtcDataRequest to add a new request,
  without interfering with other connection component operations. }
procedure TRtcDataClient.AddRequest(Req: TRtcClientRequestInfo);
  begin
  FCS.Enter;
  try
    FRequestList.Add(Req);
    FIdle:=False;
  finally
    FCS.Leave;
    end;
  end;

{ Called from TRtcDataRequest to add a new request,
  without interfering with other connection component operations. }
procedure TRtcDataClient.InsertRequest(Req: TRtcClientRequestInfo);
  begin
  FCS.Enter;
  try
    if not FMayInsertRequest then
      raise Exception.Create('You are not allowed to insert requests.')
    else if FRequestSkipped>0 then
      raise Exception.Create('Operation Interrupted by a "RequestSkipped" call.');

    FRequestInserted:=True;

    if assigned(FActiveRequest) then
      FRequestList.Insert(0,FActiveRequest);

    FActiveRequest:=Req;
    Req.WasInjected:=True;
    Request:=FActiveRequest.Get_Request;
  finally
    FCS.Leave;
    end;
  end;

procedure TRtcDataClient.PrepareNextRequest;
  var
    endloop:boolean;
  begin
  if not assigned(FActiveRequest) then
    begin
    FCS.Enter;
    try
      repeat
        endloop:=True;
        if FRequestList.Count>0 then
          begin
          FActiveRequest:= TRtcClientRequestInfo(FRequestList.Items[0]);
          Request:=FActiveRequest.Get_Request;

          FRequestList.Delete(0);
          end;
        if FRequestSkipped>0 then
          begin
          FCS.Leave;
          try
            EnterEvent;
            try
              if assigned(FActiveRequest) then
                FActiveRequest.Call_ResponseAbort(self)
              else
                CallResponseAbort;
            finally
              LeaveEvent;
              end;
          finally
            FCS.Enter;
            end;
          RemoveRequest;
          endloop:=False;
          end;
        until endloop;
    finally
      FCS.Leave;
      end;
    end;
  end;

{ Start Request if no request active }
procedure TRtcDataClient.StartRequest;
  var
    endmainloop:boolean;
  begin
  if not isConnected then
    begin
    if AutoConnect then
      Connect;
    Exit;
    end;

  // if assigned(FActiveRequest) then Exit;

  repeat
    endmainloop:=True;

    PrepareNextRequest;

    EnterEvent;
    try

      if isConnected and assigned(FActiveRequest) and not Request.Active then
        begin
        if (Session.ID<>'') and
           (Session.PeerAddr<>PeerAddr) then
          begin
          Session.Init;
          end;

        CheckRequestSkipped;

        if not Request.Skipped then
          begin
          FMayInsertRequest:=True;
          try
            // use a loop to simplify repeating the BeginRequest calls if a new request has been inserted
            repeat
              FRequestInserted:=False;

              if assigned(Request.OnBegin) then
                Request.OnBegin(self);

              if RequestInserted then
                Continue;

              FActiveRequest.Call_BeginRequest(self);

              until not RequestInserted;
          finally
            FMayInsertRequest:=False;
            end;

          Flush;
          end;

        if not isConnected then
          Exit
        else if not Request.Active then
          begin
          if Response.Rejected then
            begin
            EnterEvent;
            try
              if assigned(FActiveRequest) then
                FActiveRequest.Call_ResponseReject(self);
              if assigned(Request.OnReject) then
                Request.OnReject(self);
            finally
              LeaveEvent;
              end;
            end;
          RemoveRequest;
          endmainloop:=False;
          end
        else if not Response.Done then
          begin
          { Request has started sending something out }

          CheckRequestSkipped;

          if Request.Skipped then
            begin
            RemoveRequest;

            // Need to Reset the connection
            if isConnected then
              begin
              Disconnect;
              Reconnect;
              end;
            end
          else if Response.Rejected then
            begin
            EnterEvent;
            try
              if assigned(FActiveRequest) then
                FActiveRequest.Call_ResponseReject(self);
              if assigned(Request.OnReject) then
                Request.OnReject(self);
            finally
              LeaveEvent;
              end;
            RemoveRequest;

            // Need to Reset the connection
            if isConnected then
              begin
              Disconnect;
              Reconnect;
              end;
            end
          else if Request.Reposting then
            begin
            Request.Reposting:=False;
            Response.Clear;

            // Reset connection
            if isConnected then
              begin
              Disconnect;
              Reconnect;
              end;
            end;
          end
        else
          begin
          if Request.Skipped then
            RemoveRequest
          else if Response.Rejected then
            begin
            FActiveRequest.Call_ResponseReject(self);
            if assigned(Request.OnReject) then
              Request.OnReject(self);
            RemoveRequest;
            end
          else if Request.Reposting then
            begin
            Request.Reposting:=False;
            Response.Clear;
            end
          else
            endmainloop:=False;
          end;
        end;
    finally
      LeaveEvent;
      end;
    until endmainloop;
  end;

{ Remove all requests from Memory }
procedure TRtcDataClient.RemoveAllRequests;
  var
    _MyRequest:TRtcClientRequestInfo;
  begin
  FCS.Enter;
  try
    FRequestSkipped:=0;
    while FRequestList.Count>0 do
      begin
      _MyRequest := TRtcClientRequestInfo(FRequestList.Items[0]);
      _MyRequest.Free;

      FRequestList.Delete(0);
      end;
    if assigned(FActiveRequest) then
      FRequestSkipped:=1
    else
      FIdle:=True;
  finally
    FCS.Leave;
    end;
  end;

{ Remove the active Request from Memory }
procedure TRtcDataClient.RemoveRequest;
  var
    Remove_Next:boolean;
  begin
  if assigned(FActiveRequest) then
    begin
    FCS.Enter;
    try
      if FRequestSkipped>0 then
        Dec(FRequestSkipped);

      Remove_Next:=FActiveRequest.WasInjected and FActiveRequest.WasAborted;

      FActiveRequest.Free;
      FActiveRequest:=nil;
      if FRequestList.Count=0 then
        FIdle:=True;
    finally
      FCS.Leave;
      end;
    Request:=nil;

    Request.Clear;
    Response.Clear;

    if Remove_Next then
      begin
      PrepareNextRequest;
      if assigned(FActiveRequest) then
        begin
        FActiveRequest.Call_ResponseAbort(self);
        RemoveRequest;
        end;
      end;
    end;
  end;


{ Check the Request after any standard working event. }
function TRtcDataClient.CheckRequestWork:boolean;
  begin
  CheckRequestSkipped;

  if Request.Skipped or Response.Rejected then
    begin
    Result:=True;
    if Response.Rejected then
      begin
      EnterEvent;
      try
        if assigned(FActiveRequest) then
          FActiveRequest.Call_ResponseReject(self);
        if assigned(Request.OnReject) then
          Request.OnReject(self);
      finally
        LeaveEvent;
        end;
      end;

    if Request.Active then
      begin
      { Request has started sending something out }
      RemoveRequest;

      // Need to Reconnect, to reset the connection
      if isConnected then
        begin
        Disconnect;
        Reconnect;
        end;
      end
    else
      begin
      RemoveRequest;

      // Check if there are more requests waiting
      if MultiThreaded then
        PostStartRequest
      else
        StartRequest;
      end;
    end
  else if Request.Reposting then
    begin
    Request.Reposting:=False;
    Result:=True;
    if not Response.Done then
      begin
      Response.Clear;

      if isConnected then
        begin
        Disconnect;
        Reconnect;
        end;
      end
    else
      begin
      Response.Clear;
      if MultiThreaded then
        PostStartRequest
      else
        StartRequest;
      end;
    end
  else if not assigned(FActiveRequest) then
    Result:=True
  else
    Result:=False;
  end;

// Skip all requests (do not fire more RequestAborted events)
procedure TRtcDataClient.SkipRequests;
  begin
  RemoveAllRequests;
  end;

// Cancel all requests (fire all RequestAborted events)
procedure TRtcDataClient.CancelRequests;
  begin
  FCS.Enter;
  try
    FRequestSkipped:=FRequestList.Count;
    if assigned(FActiveRequest) then
      Inc(FRequestSkipped)
    else
      FIdle:=True;
  finally
    FCS.Leave;
    end;
  end;

// Post a StartRequest job
procedure TRtcDataClient.PostStartRequest;
  var
    ReqJob:TRtcStartRequestJob;
  begin
  ReqJob:=TRtcStartRequestJob.Create;
  ReqJob.Client:=self;
  if not PostJob(ReqJob) then
    begin
    if AutoConnect then
      Connect;
    if not PostJob(ReqJob) then
      begin
      ReqJob.Client:=nil;
      ReqJob.Free;
      end;
    end;
  end;

// Post a new request
procedure TRtcDataClient.PostRequest(Req: TRtcClientRequestInfo;FromInsideEvent:boolean=False);
  begin
  AddRequest(Req);
  if not FromInsideEvent then
    PostStartRequest;
  end;

function TRtcDataClient.RequestCount: integer;
  begin
  FCS.Enter;
  try
    if assigned(FRequestList) then
      Result:=FRequestList.Count
    else
      Result:=0;

    if assigned(FActiveRequest) then
      Result:=Result+1;
  finally
    FCS.Leave;
    end;
  end;

procedure TRtcDataClient.CallBeginRequest;
  begin
  if assigned(FOnBeginRequest) then
    FOnBeginRequest(self);
  end;

procedure TRtcDataClient.CallResponseDone;
  begin
  if assigned(FOnResponseDone) then
    FOnResponseDone(self);
  end;

procedure TRtcDataClient.CallRepostCheck;
  begin
  if assigned(FOnRepostCheck) then
    FOnRepostCheck(self);
  end;

procedure TRtcDataClient.CallResponseData;
  begin
  if assigned(FOnResponseData) then
    FOnResponseData(self);
  end;

procedure TRtcDataClient.CallResponseAbort;
  begin
  if assigned(FActiveRequest) then
    FActiveRequest.WasAborted:=True;
  if assigned(FOnResponseAbort) then
    FOnResponseAbort(self);
  end;

procedure TRtcDataClient.CallResponseReject;
  begin
  if assigned(FOnResponseReject) then
    FOnResponseReject(self);
  end;

procedure TRtcDataClient.CallSessionClose;
  begin
  if assigned(FOnSessionClose) then
    FOnSessionClose(self);
  end;

procedure TRtcDataClient.CallSessionOpen;
  begin
  if assigned(FOnSessionOpen) then
    FOnSessionOpen(self);
  end;

procedure TRtcDataClient.AddDataClientLink(Value: TRtcAbsDataClientLink);
  begin
  FDataClientLinks.Add(Value);
  end;

procedure TRtcDataClient.RemoveAllDataClientLinks;
  var
    Link:TRtcAbsDataClientLink;
  begin
  while FDataClientLinks.Count>0 do
    begin
    Link:=TRtcAbsDataClientLink(FDataClientLinks.Get(0));
    Link.RemoveClient(self);
    end;
  end;

procedure TRtcDataClient.RemoveDataClientLink(Value: TRtcAbsDataClientLink);
  begin
  FDataClientLinks.Remove(Value);
  end;

procedure TRtcDataClient.CheckRequestSkipped;
  begin
  FCS.Enter;
  try
    if FRequestSkipped>0 then
      if assigned(FActiveRequest) then
        Request.Skip;
  finally
    FCS.Leave;
    end;
  end;

procedure TRtcDataClient.SetRequest(const Value: TRtcClientRequest);
  begin
  FRequest := Value;
  if FRequest=nil then
    FRequest:=FMyRequest;
  end;

procedure TRtcDataClient.SetResponse(const Value: TRtcClientResponse);
  begin
  FResponse := Value;
  if FResponse=nil then
    FResponse:=FMyResponse;
  end;

function TRtcDataClient.isIdle: boolean;
  begin
  FCS.Enter;
  try
    Result:=FIdle;
  finally
    FCS.Leave;
    end;
  end;

function TRtcDataClient.WaitForCompletion(UserInteractionAllowed: boolean; _Timeout: cardinal): boolean;
  var
    Msg:TMsg;
    MyTime:cardinal;

  function PeekMsg:boolean;
    begin
    Result:=PeekMessage(Msg,0,WM_USER,$FFFF,PM_REMOVE) or
            PeekMessage(Msg,0,0,WM_KEYFIRST-1,PM_REMOVE);
    end;

  begin
  Result := isIdle;
  if Result then
    begin
    Result:=RequestCount=0;
    Exit;
    end;

  if _Timeout>0 then
    MyTime:=GetTickCount+_Timeout*1000
  else
    MyTime:=0;

  if not inMainThread then // When used from a Service
    begin
    {$IFDEF FPC}
    while not Result do
    {$ELSE}
    while not (Result or Application.Terminated) do
    {$ENDIF}
      begin
      while PeekMessage(Msg,0,0,WM_USER-1,PM_REMOVE) or
            PeekMessage(Msg,0,WM_USER,$FFFF,PM_NOREMOVE) do
        begin
        if Msg.message>=WM_USER then
          Exit
        else if (Msg.message=WM_QUIT) then
          begin
          {$IFDEF FPC}
          {$ELSE}
          Application.Terminate;
          {$ENDIF}
          Exit;
          end
        else
          begin
          TranslateMessage( Msg );
          DispatchMessage( Msg );
          end;
        end;
      Result:=isIdle;
      if not Result then
        if (MyTime>0) and (GetTickCount>=MyTime) then
          Exit
        else
          Sleep(10);
      end;
    if Result then
      Result:=RequestCount=0;
    end
  else if UserInteractionAllowed then
    begin
    {$IFDEF FPC}
    while not Result do
    {$ELSE}
    while not (Result or Application.Terminated) do
    {$ENDIF}
      begin
      while PeekMessage(Msg,0,0,0,PM_REMOVE) do
        begin
        if (Msg.message=WM_QUIT) then
          begin
          {$IFDEF FPC}
          {$ELSE}
          Application.Terminate;
          {$ENDIF}
          Exit;
          end
        else
          begin
          TranslateMessage( Msg );
          DispatchMessage( Msg );
          end;
        end;
      Result:=isIdle;
      if not Result then
        if (MyTime>0) and (GetTickCount>=MyTime) then
          Exit
        else
          Sleep(10);
      end;
    if Result then
      Result:=RequestCount=0;
    end
  else
    begin
    {$IFDEF FPC}
    while not Result do
    {$ELSE}
    while not (Result or Application.Terminated) do
    {$ENDIF}
      begin
      while PeekMsg do
        begin
        if (Msg.message=WM_QUIT) then
          begin
          {$IFDEF FPC}
          {$ELSE}
          Application.Terminate;
          {$ENDIF}
          Exit;
          end
        else
          begin
          TranslateMessage( Msg );
          DispatchMessage( Msg );
          end;
        end;
      Result:=isIdle;
      if not Result then
        if (MyTime>0) and (GetTickCount>=MyTime) then
          Exit
        else
          Sleep(10);
      end;
    if Result then
      Result:=RequestCount=0;
    end;
  end;

function TRtcDataClient.isConnectionRequired: boolean;
  begin
  if not FAutoConnect then
    Result:=True
  else
    Result:=RequestCount>0;
  end;

function TRtcDataClient.GetAutoConnect: boolean;
  begin
  Result:=FAutoConnect;
  end;

procedure TRtcDataClient.SetAutoConnect(const Value: boolean);
  begin
  if Value<>FAutoConnect then
    begin
    FAutoConnect:=Value;
    if FAutoConnect and isConnectionRequired then
      Connect;
    end;
  end;

{ TRtcAbsDataClientLink }

constructor TRtcAbsDataClientLink.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FClient:=nil;
  FLink:=nil;
  end;

destructor TRtcAbsDataClientLink.Destroy;
  begin
  Client:=nil; // remove from DataClient
  Link:=nil; // remove from parent DataClientLink
  inherited;
  end;

procedure TRtcAbsDataClientLink.SetLink(const Value: TRtcDataClientLink);
  var
    MyLink:TRtcDataClientLink;
  begin
  if Value<>FLink then
    begin
    if assigned(FLink) then
      begin
      FLink.RemoveChildLink(self);
      FLink:=nil;
      end;

    if assigned(Value) then
      begin
      Client:=nil; // can not be maped to DataClient and to DataClientLink at the same time.

      // Check for circular reference before assigning!
      MyLink:=Value;
      while (MyLink<>nil) and (MyLink<>self) do
        MyLink:=MyLink.Link;

      if MyLink=self then
        raise Exception.Create('Circular DataClientLink reference!');

      FLink:=Value;
      FLink.AddChildLink(self);
      end;
    end;
  end;

function TRtcAbsDataClientLink.GetLink: TRtcDataClientLink;
  begin
  Result:=FLink;
  end;

procedure TRtcAbsDataClientLink.SetClient(const Value: TRtcDataClient);
  begin
  if Value<>FClient then
    begin
    if assigned(FClient) then
      begin
      FClient.RemoveDataClientLink(self);
      FClient:=nil;
      end;

    if assigned(Value) then
      begin
      Link:=nil; // can not be linked to DataClientLink and DataClient at the same time.
      FClient:=Value;
      FClient.AddDataClientLink(self);
      end;
    end;
  end;

function TRtcAbsDataClientLink.GetClient: TRtcDataClient;
  begin
  Result:=FClient;
  end;

procedure TRtcAbsDataClientLink.PostRequest(Req: TRtcClientRequestInfo; FromInsideEvent:boolean=False);
  begin
  if assigned(FLink) then
    FLink.PostRequest(Req,FromInsideEvent)
  else if assigned(FClient) then
    FClient.PostRequest(Req,FromInsideEvent)
  else
    raise Exception.Create('PostRequest: Client connection undefined.');
  end;

procedure TRtcAbsDataClientLink.InsertRequest(Req: TRtcClientRequestInfo);
  begin
  if assigned(FLink) then
    FLink.InsertRequest(Req)
  else if assigned(FClient) then
    FClient.InsertRequest(Req)
  else
    raise Exception.Create('InsertRequest: Client connection undefined.');
  end;

function TRtcAbsDataClientLink.RequestCount: integer;
  begin
  if assigned(FLink) then
    Result:=FLink.RequestCount
  else if assigned(FClient) then
    Result:=FClient.RequestCount
  else
    raise Exception.Create('RequestCount: Client connection undefined.');
  end;

procedure TRtcAbsDataClientLink.SkipRequests;
  begin
  if assigned(FLink) then
    FLink.SkipRequests
  else if assigned(FClient) then
    FClient.SkipRequests
  else
    raise Exception.Create('SkipRequests: Client connection undefined.');
  end;

procedure TRtcAbsDataClientLink.CancelRequests;
  begin
  if assigned(FLink) then
    FLink.CancelRequests
  else if assigned(FClient) then
    FClient.CancelRequests
  else
    raise Exception.Create('CancelRequests: Client connection undefined.');
  end;

function TRtcAbsDataClientLink.WaitForCompletion(UserInteractionAllowed: boolean; Timeout: cardinal): boolean;
  begin
  if assigned(FLink) then
    Result:=FLink.WaitForCompletion(UserInteractionAllowed, Timeout)
  else if assigned(FClient) then
    Result:=FClient.WaitForCompletion(UserInteractionAllowed, Timeout)
  else
    raise Exception.Create('WaitForCompletion: Client connection undefined.');
  end;

function TRtcAbsDataClientLink.CheckLink(Value: TRtcAbsDataClientLink): boolean;
  begin
  if Value=FLink then
    Result:=True
  else if assigned(FLink) then
    Result:=FLink.CheckLink(Value)
  else
    Result:=False;
  end;

procedure TRtcAbsDataClientLink.RemoveClient(Value: TRtcDataClient);
  begin
  if Value=FClient then Client:=nil;
  end;

procedure TRtcAbsDataClientLink.RemoveLink(Value: TRtcAbsDataClientLink);
  begin
  if Value=FLink then Link:=nil;
  end;

{ TRtcDataRequest }

constructor TRtcDataRequestInfo.Create;
  begin
  inherited;
  FEvents:=nil;
  FRequest:=nil;
  end;

destructor TRtcDataRequestInfo.Destroy;
  begin
  FEvents:=nil;
  if assigned(FRequest) then
    begin
    FRequest.Free;
    FRequest:=nil;
    end;
  inherited;
  end;

procedure TRtcDataRequestInfo.Call_DataReceived(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_DataReceived(Sender);
  end;

procedure TRtcDataRequestInfo.Call_DataOut(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_DataOut(Sender);
  end;

procedure TRtcDataRequestInfo.Call_DataIn(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_DataIn(Sender);
  end;

procedure TRtcDataRequestInfo.Call_DataSent(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_DataSent(Sender);
  end;

procedure TRtcDataRequestInfo.Call_ConnectLost(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_ConnectLost(Sender);
  end;

procedure TRtcDataRequestInfo.Call_RepostCheck(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_RepostCheck(Sender);
  end;

procedure TRtcDataRequestInfo.Call_ReadyToSend(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_ReadyToSend(Sender);
  end;

procedure TRtcDataRequestInfo.Call_BeginRequest(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_BeginRequest(Sender);
  end;

procedure TRtcDataRequestInfo.Call_ResponseDone(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_ResponseDone(Sender);
  end;

procedure TRtcDataRequestInfo.Call_ResponseData(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_ResponseData(Sender);
  end;

procedure TRtcDataRequestInfo.Call_ResponseAbort(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_ResponseAbort(Sender);
  end;

procedure TRtcDataRequestInfo.Call_ResponseReject(Sender:TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_ResponseReject(Sender);
  end;

procedure TRtcDataRequestInfo.Call_SessionClose(Sender: TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_SessionClose(Sender);
  end;

procedure TRtcDataRequestInfo.Call_SessionOpen(Sender: TRtcConnection);
  begin
  if assigned(FEvents) then
    FEvents.Call_SessionOpen(Sender);
  end;

function TRtcDataRequestInfo.Get_Request: TRtcClientRequest;
  begin
  Result:=FRequest;
  end;

{ TRtcDataRequestData }

constructor TRtcDataRequestData.Create;
  begin
  inherited;
  FRequest:=nil;
  end;

destructor TRtcDataRequestData.Destroy;
  begin
  if assigned(FRequest) then
    begin
    FRequest.Free;
    FRequest:=nil;
    end;
  inherited;
  end;

{ TRtcDataRequest }

procedure TRtcDataRequest.Call_ConnectLost(Sender: TRtcConnection);
  begin
  if assigned(FOnConnectLost) then
    if AutoSyncEvents then
      Sender.Sync(FOnConnectLost)
    else
      FOnConnectLost(Sender);
  end;

procedure TRtcDataRequest.Call_DataReceived(Sender: TRtcConnection);
  begin
  if assigned(FOnDataReceived) then
    if AutoSyncEvents then
      Sender.Sync(FOnDataReceived)
    else
      FOnDataReceived(Sender);
  end;

procedure TRtcDataRequest.Call_DataOut(Sender: TRtcConnection);
  begin
  if assigned(FOnDataOut) then
    if AutoSyncEvents then
      Sender.Sync(FOnDataOut)
    else
      FOnDataOut(Sender);
  end;

procedure TRtcDataRequest.Call_DataIn(Sender: TRtcConnection);
  begin
  if assigned(FOnDataIn) then
    if AutoSyncEvents then
      Sender.Sync(FOnDataIn)
    else
      FOnDataIn(Sender);
  end;

procedure TRtcDataRequest.Call_DataSent(Sender: TRtcConnection);
  begin
  if assigned(FOnDataSent) then
    if AutoSyncEvents then
      Sender.Sync(FOnDataSent)
    else
      FOnDataSent(Sender);
  end;

procedure TRtcDataRequest.Call_ReadyToSend(Sender: TRtcConnection);
  begin
  if assigned(FOnReadyToSend) then
    if AutoSyncEvents then
      Sender.Sync(FOnReadyToSend)
    else
      FOnReadyToSend(Sender);
  end;

procedure TRtcDataRequest.Call_BeginRequest(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_BeginRequest(Sender)
  else if assigned(FClient) then
    FClient.CallBeginRequest;

  if not TRtcDataClient(Sender).RequestInserted and
     not TRtcDataClient(Sender).Request.Skipped and
     not TRtcDataClient(Sender).Response.Rejected then
    if assigned(FOnBeginRequest) then
      if AutoSyncEvents then
        Sender.Sync(FOnBeginRequest)
      else
        FOnBeginRequest(Sender);
  end;

procedure TRtcDataRequest.Call_ResponseData(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseData) then
    if AutoSyncEvents then
      Sender.Sync(FOnResponseData)
    else
      FOnResponseData(Sender);

  if not TRtcDataClient(Sender).Request.Skipped and
     not TRtcDataClient(Sender).Response.Rejected then
    if assigned(FLink) then
      FLink.Call_ResponseData(Sender)
    else if assigned(FClient) then
      FClient.CallResponseData;
  end;

procedure TRtcDataRequest.Call_ResponseDone(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseDone) then
    if AutoSyncEvents then
      Sender.Sync(FOnResponseDone)
    else
      FOnResponseDone(Sender);

  if not TRtcDataClient(Sender).Request.Skipped and
     not TRtcDataClient(Sender).Response.Rejected then
    if assigned(FLink) then
      FLink.Call_ResponseDone(Sender)
    else if assigned(FClient) then
      FClient.CallResponseDone;
  end;

procedure TRtcDataRequest.Call_RepostCheck(Sender: TRtcConnection);
  begin
  if ((AutoRepost<0) or (TRtcDataClient(Sender).Request.Reposted<AutoRepost)) then
    TRtcDataClient(Sender).Request.Repost
  else
    begin
    if assigned(FOnRepostCheck) then
      if not TRtcDataClient(Sender).Request.Reposting then
        FOnRepostCheck(Sender);

    if not TRtcDataClient(Sender).Request.Reposting then
      if assigned(FLink) then
        FLink.Call_RepostCheck(Sender)
      else if assigned(FClient) then
        FClient.CallRepostCheck;
    end;
  end;

procedure TRtcDataRequest.Call_ResponseAbort(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseAbort) then
    if AutoSyncEvents then
      Sender.Sync(FOnResponseAbort)
    else
      FOnResponseAbort(Sender);

  if not TRtcDataClient(Sender).Request.Reposting then
    if assigned(FLink) then
      FLink.Call_ResponseAbort(Sender)
    else if assigned(FClient) then
      FClient.CallResponseAbort;
  end;

procedure TRtcDataRequest.Call_ResponseReject(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseReject) then
    if AutoSyncEvents then
      Sender.Sync(FOnResponseReject)
    else
      FOnResponseReject(Sender);

  if assigned(FLink) then
    FLink.Call_ResponseReject(Sender)
  else if assigned(FClient) then
    FClient.CallResponseReject;
  end;

procedure TRtcDataRequest.Call_SessionOpen(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionOpen) then
    if AutoSyncEvents then
      Sender.Sync(FOnSessionOpen)
    else
      FOnSessionOpen(Sender);

  if assigned(FLink) then
    FLink.Call_SessionOpen(Sender)
  else if assigned(FClient) then
    FClient.CallSessionOpen;
  end;

procedure TRtcDataRequest.Call_SessionClose(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionClose) then
    if AutoSyncEvents then
      Sender.Sync(FOnSessionClose)
    else
      FOnSessionClose(Sender);

  if assigned(FLink) then
    FLink.Call_SessionClose(Sender)
  else if assigned(FClient) then
    FClient.CallSessionClose;
  end;

constructor TRtcDataRequest.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FCS:=TRtcCritSec.Create;
  FMyData:=tObjList.Create(32);
  FMainThrData:=TRtcDataRequestData.Create;
  end;

destructor TRtcDataRequest.Destroy;
  begin
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
  FCS.Free;
  inherited;
  end;

function TRtcDataRequest.CheckMyData: TRtcDataRequestData;
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
          Result:=TRtcDataRequestData(obj)
        else
          Result:=nil;
      finally
        FCS.Leave;
        end;
      end;
    end;
  end;

procedure TRtcDataRequest.ClearMyData;
  var
    id:longword;
    obj:TObject;
    cli:TRtcDataRequestData;
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
          cli:=TRtcDataRequestData(obj);
          cli.Free;
          FMyData.remove(id);
          end;
      finally
        FCS.Leave;
        end;
      end;
    end;
  end;

function TRtcDataRequest.GetMyData: TRtcDataRequestData;
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
          obj:=TRtcDataRequestData.Create;
          FMyData.insert(id, obj);
          end;
        Result:=TRtcDataRequestData(obj);
      finally
        FCS.Leave;
        end;
      end;
    end;
  end;

procedure TRtcDataRequest.Post(FromInsideEvent:boolean=False; Sender:TRtcConnection=nil);
  var
    myData:TRtcDataRequestData;
    DataReq:TRtcDataRequestInfo;
  begin
  myData:=CheckMyData;

  if myData=nil then
    raise Exception.Create('Prepare your request using the "Request" property before callind "Post".');

  if myData.FRequest=nil then
    raise Exception.Create('Prepare your request using the "Request" property before callind "Post".');

  with myData do
    begin
    DataReq:=TRtcDataRequestInfo.Create;
    DataReq.Request:=FRequest;
    DataReq.Events:=Self;
    FRequest:=nil;
    end;

  try
    if assigned(Sender) and (Sender is TRtcDataClient) then
      TRtcDataClient(Sender).PostRequest(DataReq,FromInsideEvent)
    else
      PostRequest(DataReq,FromInsideEvent);
  except
    DataReq.Free;
    raise;
    end;

  // Free temporary object from memory
  ClearMyData;
  end;

procedure TRtcDataRequest.Insert(Sender:TRtcConnection);
  var
    DataReq:TRtcDataRequestInfo;
  begin
  DataReq:=TRtcDataRequestInfo.Create;
  DataReq.Request:=TRtcClientRequest.Create;
  DataReq.Events:=Self;
  try
    if assigned(Sender) and (Sender is TRtcDataClient) then
      TRtcDataClient(Sender).InsertRequest(DataReq)
    else
      InsertRequest(DataReq);
  except
    DataReq.Free;
    raise;
    end;
  end;

function TRtcDataRequest.GetRequest: TRtcClientRequest;
  var
    myData:TRtcDataRequestData;
  begin
  myData:=GetMyData;
  if not assigned(myData.FRequest) then
    myData.FRequest:=TRtcClientRequest.Create;
  Result:=myData.FRequest;
  end;

{ TRtcStartRequestJob }

procedure TRtcStartRequestJob.Kill;
  begin
  Client:=nil;
  Free;
  end;

procedure TRtcStartRequestJob.Run(Thr:TRtcThread);
  begin
  try
    Client.StartRequest;
  except
    // ignore exceptions
    end;
  Client:=nil;
  Free;
  end;

{ TRtcClientSession }

procedure TRtcClientSession.Init;
  begin
  Close;
  Clear;
  FID:='';
  FCreated:=0;
  FPeerAddr:='';
  end;

procedure TRtcClientSession.Open(const _ID: string);
  begin
  if FID<>'' then Close;
  Clear;
  FID:=_ID;
  FCreated:=Now;
  if assigned(FCon) then
    FPeerAddr:=FCon.PeerAddr
  else
    FPeerAddr:='';
  if assigned(FCon) and (FCon is TRtcDataClient) then
    with TRtcDataClient(FCon) do
      if assigned(FActiveRequest) then
        FActiveRequest.Call_SessionOpen(FCon)
      else
        CallSessionOpen;
  end;

procedure TRtcClientSession.Close;
  begin
  if FID<>'' then
    begin
    if assigned(FCon) and (FCon is TRtcDataClient) then
      with TRtcDataClient(FCon) do
        if assigned(FActiveRequest) then
          FActiveRequest.Call_SessionClose(FCon)
        else
          CallSessionClose;
    FCreated:=0;
    FID:='';
    Clear;
    end;
  end;

{ TRtcDataClientLink }

constructor TRtcDataClientLink.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FDataClientLinks:=TRtcDataClientLinkList.Create;
  end;

destructor TRtcDataClientLink.Destroy;
  begin
  RemoveAllChildLinks;
  FDataClientLinks.Free;
  FDataClientLinks:=nil;
  inherited;
  end;

procedure TRtcDataClientLink.AddChildLink(Value: TRtcAbsDataClientLink);
  begin
  FDataClientLinks.Add(Value);
  end;

procedure TRtcDataClientLink.RemoveAllChildLinks;
  var
    _Link:TRtcAbsDataClientLink;
  begin
  while FDataClientLinks.Count>0 do
    begin
    _Link:=TRtcAbsDataClientLink(FDataClientLinks.Get(0));
    _Link.RemoveLink(self);
    end;
  end;

procedure TRtcDataClientLink.RemoveChildLink(Value: TRtcAbsDataClientLink);
  begin
  FDataClientLinks.Remove(Value);
  end;

procedure TRtcDataClientLink.Call_ConnectLost(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_ConnectLost(Sender);
  end;

procedure TRtcDataClientLink.Call_DataReceived(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_DataReceived(Sender);
  end;

procedure TRtcDataClientLink.Call_DataOut(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_DataOut(Sender);
  end;

procedure TRtcDataClientLink.Call_DataIn(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_DataIn(Sender);
  end;

procedure TRtcDataClientLink.Call_DataSent(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_DataSent(Sender);
  end;

procedure TRtcDataClientLink.Call_ReadyToSend(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_ReadyToSend(Sender);
  end;

procedure TRtcDataClientLink.Call_BeginRequest(Sender: TRtcConnection);
  begin
  if assigned(FLink) then
    FLink.Call_BeginRequest(Sender)
  else if assigned(FClient) then
    if Sender=FClient then FClient.CallBeginRequest;

  if not TRtcDataClient(Sender).RequestInserted then
    if assigned(FOnBeginRequest) then
      if AutoSyncEvents then
        Sender.Sync(FOnBeginRequest)
      else
        FOnBeginRequest(Sender);
  end;

procedure TRtcDataClientLink.Call_ResponseData(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseData) then
    if AutoSyncEvents then
      Sender.Sync(FOnResponseData)
    else
      FOnResponseData(Sender);

  if not TRtcDataClient(Sender).Request.Skipped and
     not TRtcDataClient(Sender).Response.Rejected then
    if assigned(FLink) then
      FLink.Call_ResponseData(Sender)
    else if assigned(FClient) then
      if Sender=FClient then FClient.CallResponseData;
  end;

procedure TRtcDataClientLink.Call_ResponseDone(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseDone) then
    if AutoSyncEvents then
      Sender.Sync(FOnResponseDone)
    else
      FOnResponseDone(Sender);

  if not TRtcDataClient(Sender).Request.Skipped and
     not TRtcDataClient(Sender).Response.Rejected then
    if assigned(FLink) then
      FLink.Call_ResponseDone(Sender)
    else if assigned(FClient) then
      if Sender=FClient then FClient.CallResponseDone;
  end;

procedure TRtcDataClientLink.Call_RepostCheck(Sender: TRtcConnection);
  begin
  if assigned(FOnRepostCheck) then
    if AutoSyncEvents then
      Sender.Sync(FOnRepostCheck)
    else
      FOnRepostCheck(Sender);

  if not TRtcDataClient(Sender).Request.Reposting then
    if assigned(FLink) then
      FLink.Call_RepostCheck(Sender)
    else if assigned(FClient) then
      if Sender=FClient then FClient.CallRepostCheck;
  end;

procedure TRtcDataClientLink.Call_ResponseAbort(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseAbort) then
    if AutoSyncEvents then
      Sender.Sync(FOnResponseAbort)
    else
      FOnResponseAbort(Sender);

  if not TRtcDataClient(Sender).Request.Reposting then
    if assigned(FLink) then
      FLink.Call_ResponseAbort(Sender)
    else if assigned(FClient) then
      if Sender=FClient then FClient.CallResponseAbort;
  end;

procedure TRtcDataClientLink.Call_ResponseReject(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseReject) then
    if AutoSyncEvents then
      Sender.Sync(FOnResponseReject)
    else
      FOnResponseReject(Sender);

  if assigned(FLink) then
    FLink.Call_ResponseReject(Sender)
  else if assigned(FClient) then
    if Sender=FClient then FClient.CallResponseReject;
  end;

procedure TRtcDataClientLink.Call_SessionClose(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionClose) then
    if AutoSyncEvents then
      Sender.Sync(FOnSessionClose)
    else
      FOnSessionClose(Sender);

  if assigned(FLink) then
    FLink.Call_SessionClose(Sender)
  else if assigned(FClient) then
    if Sender=FClient then FClient.CallSessionClose;
  end;

procedure TRtcDataClientLink.Call_SessionOpen(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionOpen) then
    if AutoSyncEvents then
      Sender.Sync(FOnSessionOpen)
    else
      FOnSessionOpen(Sender);

  if assigned(FLink) then
    FLink.Call_SessionOpen(Sender)
  else if assigned(FClient) then
    if Sender=FClient then FClient.CallSessionOpen;
  end;

{ TRtcDualDataClientLink }

constructor TRtcDualDataClientLink.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FClient2:=nil;
  FLink2:=nil;
  end;

destructor TRtcDualDataClientLink.Destroy;
  begin
  Client2:=nil; // remove from DataClient
  Link2:=nil; // remove from parent DataClientLink
  inherited;
  end;

procedure TRtcDualDataClientLink.CancelRequests;
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.CancelRequests
  else if assigned(FClient2) then
    FClient2.CancelRequests
  else
    raise Exception.Create('CancelRequests: 2nd Client connection undefined.');
  end;

function TRtcDualDataClientLink.GetClient2: TRtcDataClient;
  begin
  Result:=FClient2;
  end;

function TRtcDualDataClientLink.GetLink2: TRtcDataClientLink;
  begin
  Result:=FLink2;
  end;

procedure TRtcDualDataClientLink.InsertRequest(Req: TRtcClientRequestInfo);
  var
    c1,c2:integer;
  begin
  if assigned(FLink) then
    c1:=FLink.RequestCount
  else if assigned(FClient) then
    c1:=FClient.RequestCount
  else
    c1:=MaxLongInt;

  if assigned(FLink2) then
    c2:=FLink2.RequestCount
  else if assigned(FClient2) then
    c2:=FClient2.RequestCount
  else
    c2:=MaxLongInt;

  if c1<=c2 then
    begin
    if assigned(FLink) then
      FLink.InsertRequest(Req)
    else if assigned(FClient) then
      FClient.InsertRequest(Req)
    else
      raise Exception.Create('InsertRequest: Client connection undefined.');
    end
  else
    begin
    if assigned(FLink2) then
      FLink2.InsertRequest(Req)
    else if assigned(FClient2) then
      FClient2.InsertRequest(Req)
    else
      raise Exception.Create('InsertRequest: 2nd Client connection undefined.');
    end;
  end;

procedure TRtcDualDataClientLink.PostRequest(Req: TRtcClientRequestInfo; FromInsideEvent: boolean);
  var
    c1,c2:integer;
  begin
  if assigned(FLink) then
    c1:=FLink.RequestCount
  else if assigned(FClient) then
    c1:=FClient.RequestCount
  else
    c1:=MaxLongInt;

  if assigned(FLink2) then
    c2:=FLink2.RequestCount
  else if assigned(FClient2) then
    c2:=FClient2.RequestCount
  else
    c2:=MaxLongInt;

  if c1<=c2 then
    begin
    if assigned(FLink) then
      FLink.PostRequest(Req,FromInsideEvent)
    else if assigned(FClient) then
      FClient.PostRequest(Req,FromInsideEvent)
    else
      raise Exception.Create('PostRequest: Client connection undefined.');
    end
  else
    begin
    if assigned(FLink2) then
      FLink2.PostRequest(Req,FromInsideEvent)
    else if assigned(FClient2) then
      FClient2.PostRequest(Req,FromInsideEvent)
    else
      raise Exception.Create('PostRequest: 2nd Client connection undefined.');
    end;
  end;

function TRtcDualDataClientLink.RequestCount: integer;
  begin
  Result:=inherited RequestCount;
  if assigned(FLink2) then
    Result:=Result+FLink2.RequestCount
  else if assigned(FClient2) then
    Result:=Result+FClient2.RequestCount;
  end;

procedure TRtcDualDataClientLink.SetClient(const Value: TRtcDataClient);
  begin
  if Value<>FClient then
    begin
    if assigned(FClient2) and (FClient2=Value) then
      Client2:=nil;

    inherited SetClient(Value);
    end;
  end;

procedure TRtcDualDataClientLink.SetLink(const Value: TRtcDataClientLink);
  begin
  if Value<>FLink then
    begin
    if assigned(FLink2) and (FLink2=Value) then
      Link2:=nil;

    inherited SetLink(Value);
    end;
  end;

procedure TRtcDualDataClientLink.SetClient2(const Value: TRtcDataClient);
  begin
  if Value<>FClient2 then
    begin
    if assigned(FClient2) then
      begin
      FClient2.RemoveDataClientLink(self);
      FClient2:=nil;
      end;

    if assigned(Value) then
      begin
      Link2:=nil; // can not be linked to DataClientLink and DataClient at the same time.
      FClient2:=Value;
      FClient2.AddDataClientLink(self);
      end;
    end;
  end;

procedure TRtcDualDataClientLink.SetLink2(const Value: TRtcDataClientLink);
  begin
  if Value<>FLink2 then
    begin
    if assigned(FLink2) then
      begin
      FLink2.RemoveChildLink(self);
      FLink2:=nil;
      end;

    if assigned(Value) then
      begin
      Client2:=nil; // can not be maped to DataClient and to DataClientLink at the same time.

      // Check for circular reference before assigning!
      if Value=self then
        raise Exception.Create('Circular DataClientLink reference!');
      if Value.CheckLink(self) then
        raise Exception.Create('Circular DataClientLink reference!');
      if CheckLink(Value) then
        raise Exception.Create('Circular DataClientLink reference!');

      FLink2:=Value;
      FLink2.AddChildLink(self);
      end;
    end;
  end;

procedure TRtcDualDataClientLink.SkipRequests;
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.SkipRequests
  else if assigned(FClient2) then
    FClient2.SkipRequests
  else
    raise Exception.Create('SkipRequests: 2nd Client connection undefined.');
  end;

function TRtcDualDataClientLink.WaitForCompletion(UserInteractionAllowed: boolean; Timeout: cardinal): boolean;
  begin
  Result:=inherited WaitForCompletion(UserInteractionAllowed,Timeout);
  if Result then
    begin
    if assigned(FLink2) then
      Result:=FLink2.WaitForCompletion(UserInteractionAllowed, Timeout)
    else if assigned(FClient2) then
      Result:=FClient2.WaitForCompletion(UserInteractionAllowed, Timeout)
    else
      raise Exception.Create('WaitForCompletion: 2nd Client connection undefined.');
    end;
  end;

function TRtcDualDataClientLink.CheckLink(Value: TRtcAbsDataClientLink): boolean;
  begin
  Result:=inherited CheckLink(Value);
  if not Result then
    if Value=FLink2 then
      Result:=True
    else if assigned(FLink2) then
      Result:=FLink2.CheckLink(Value)
    else
      Result:=False;
  end;

procedure TRtcDualDataClientLink.RemoveClient(Value: TRtcDataClient);
  begin
  inherited RemoveClient(Value);
  if Value=FClient2 then Client2:=nil;
  end;

procedure TRtcDualDataClientLink.RemoveLink(Value: TRtcAbsDataClientLink);
  begin
  inherited RemoveLink(Value);
  if Value=FLink2 then Link2:=nil;
  end;

procedure TRtcDualDataClientLink.Call_ConnectLost(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_ConnectLost(Sender);
  end;

procedure TRtcDualDataClientLink.Call_DataReceived(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_DataReceived(Sender);
  end;

procedure TRtcDualDataClientLink.Call_DataOut(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_DataOut(Sender);
  end;

procedure TRtcDualDataClientLink.Call_DataIn(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_DataIn(Sender);
  end;

procedure TRtcDualDataClientLink.Call_DataSent(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_DataSent(Sender);
  end;

procedure TRtcDualDataClientLink.Call_ReadyToSend(Sender: TRtcConnection);
  begin
  inherited;
  if assigned(FLink2) then
    FLink2.Call_ReadyToSend(Sender);
  end;

procedure TRtcDualDataClientLink.Call_BeginRequest(Sender: TRtcConnection);
  begin
  if assigned(FLink2) then
    FLink2.Call_BeginRequest(Sender)
  else if assigned(FClient2) then
    if Sender=FClient2 then FClient2.CallBeginRequest;

  inherited;
  end;

procedure TRtcDualDataClientLink.Call_ResponseData(Sender: TRtcConnection);
  begin
  inherited;

  if not TRtcDataClient(Sender).Request.Skipped and
     not TRtcDataClient(Sender).Response.Rejected then
    if assigned(FLink2) then
      FLink2.Call_ResponseData(Sender)
    else if assigned(FClient2) then
      if Sender=FClient2 then FClient2.CallResponseData;
  end;

procedure TRtcDualDataClientLink.Call_ResponseDone(Sender: TRtcConnection);
  begin
  inherited;

  if not TRtcDataClient(Sender).Request.Skipped and
     not TRtcDataClient(Sender).Response.Rejected then
    if assigned(FLink2) then
      FLink2.Call_ResponseDone(Sender)
    else if assigned(FClient2) then
      if Sender=FClient2 then FClient2.CallResponseDone;
  end;

procedure TRtcDualDataClientLink.Call_RepostCheck(Sender: TRtcConnection);
  begin
  inherited;

  if not TRtcDataClient(Sender).Request.Reposting then
    if assigned(FLink2) then
      FLink2.Call_RepostCheck(Sender)
    else if assigned(FClient2) then
      if Sender=FClient2 then FClient2.CallRepostCheck;
  end;

procedure TRtcDualDataClientLink.Call_ResponseAbort(Sender: TRtcConnection);
  begin
  inherited;

  if not TRtcDataClient(Sender).Request.Reposting then
    if assigned(FLink2) then
      FLink2.Call_ResponseAbort(Sender)
    else if assigned(FClient2) then
      if Sender=FClient2 then FClient2.CallResponseAbort;
  end;

procedure TRtcDualDataClientLink.Call_ResponseReject(Sender: TRtcConnection);
  begin
  inherited;

  if assigned(FLink2) then
    FLink2.Call_ResponseReject(Sender)
  else if assigned(FClient2) then
    if Sender=FClient2 then FClient2.CallResponseReject;
  end;

procedure TRtcDualDataClientLink.Call_SessionClose(Sender: TRtcConnection);
  begin
  inherited;

  if assigned(FLink2) then
    FLink2.Call_SessionClose(Sender)
  else if assigned(FClient2) then
    if Sender=FClient2 then FClient2.CallSessionClose;
  end;

procedure TRtcDualDataClientLink.Call_SessionOpen(Sender: TRtcConnection);
  begin
  inherited;

  if assigned(FLink2) then
    FLink2.Call_SessionOpen(Sender)
  else if assigned(FClient2) then
    if Sender=FClient2 then FClient2.CallSessionOpen;
  end;

end.
