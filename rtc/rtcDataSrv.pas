{
  @html(<b>)
  Data Server Components
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  This unit implements a set of Server-side Data components. @Link(TRtcDataServer)
  implements a wrapper for all Request/Response based server-side connection components.
  By linking one or more @Link(TRtcDataServerLink) components, which are linked to one or
  more @Link(TRtcDataProvider) components, you add functionality to your server for
  specific Request handlers. @html(<br>)
  @Link(TRtcHttpServer) implements a HTTP-based TCP/IP server connection with uses a
  HTTP connection provider, so you can compile your DataProvider/ServerModule/Function
  components into a stand-alone HTTP server executable.
}
unit rtcDataSrv;

{$INCLUDE rtcDefs.inc}

interface

// When "UseGUIDs" is defined, CoCreateGUID() API function is used to create Session IDs.
{.$DEFINE UseGUIDs}

uses
  rtcTrashcan,

  Classes,
{$IFDEF useGUIDs}
  Windows, ComObj, ActiveX,
{$ENDIF}

  rtcLog,
  rtcInfo,
  rtcConn,

  rtcThrPool;

var
  { Default Session Live Time (in seconds).
    @html(<br><br>)

    Before a session defines its KeepAliveTime,
    the session will live for RTC_SESSION_TIMEOUT seconds after each call. }
  RTC_SESSION_TIMEOUT:integer=60;

{$IFNDEF UseGUIDs}
const
  // @exclude
  RTC_SESSIONID_LENGTH=28;
{$ENDIF}

type
  { @abstract(Components used in DataServer to implement server's functionality) }
  TRtcServerComponent=class(TRtcComponent)
  protected
    // @exclude
    function GetOrder: integer; virtual; abstract;
    end;

  // @exclude
  TRtcServerComponentList = class
  private
    FList:TList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Value:TRtcServerComponent);
    procedure Remove(Value:TRtcServerComponent);

    procedure RemoveAll;

    function Count:integer;
    function Get(index:integer):TRtcServerComponent;
    end;

  TRtcAbsDataServerLink = class;

  // @exclude
  TRtcDataServerLinkList = class(TRtcServerComponentList)
  public
    function GetLink(index:integer):TRtcAbsDataServerLink;
    end;

  { @abstract(Server Session information) }
  TRtcServerSession=class(TRtcSession)
  private
    FLastUsed,
    FExpireTime:TDateTime;
    FFinalExpire:TDateTime;
    FKeepAlive:integer;

  protected
    // @exclude
    class function Find(const _ID:string; const _PeerAddr,_ForwardedFor:string):TRtcServerSession;
    // @exclude
    class function Have(const _ID:string; const _PeerAddr,_ForwardedFor:string):boolean;
    // @exclude
    class function Open(_LockType:TRtcSessionLockType; const _PeerAddr,_ForwardedFor:string):TRtcServerSession;
    // @exclude
    class function CloseID(const _ID:string; const _PeerAddr,_ForwardedFor:string; _Event:TRtcSimpleEvent):boolean;

    // @exclude
    procedure SetFinalExpire(const _Value:TDateTime);
    // @exclude
    procedure SetKeepAlive(const _Value:integer);

    // @exclude
    procedure LockSessionID;
    // @exclude
    procedure UnlockSession(_Event:TRtcSimpleEvent);

    // @exclude
    procedure UnLock(_Event:TRtcSimpleEvent);

    // @exclude
    function DenyAccess(const _PeerAddr,_ForwardedFor:string):boolean;

  public
    // Close session: This is same as setting "FinalExpire" to "Now".
    procedure Close;

    // Session Last used (updated after each FindSession call)
    property LastUsed:TDateTime read FLastUsed;
    // Number of seconds this session will be kept alive after each usage (renew "ExpireTime").
    property KeepAlive:integer read FKeepAlive write SetKeepAlive;
    { Time when Session will Expire.
      It is auto-maintained, using the LiveTime and FinalExpire values. }
    property ExpireTime:TDateTime read FExpireTime;
    // If not zero, defines the Time and Date when this session will Expire for good.
    property FinalExpire:TDateTime read FFinalExpire write SetFinalExpire;
    end;

  { @Abstract(Universal Data Server Connection component)

    By using methods provided by this DataProvider component, you ensure that
    your code will be compatible with different connection providers,
    which makes it possible to write your code once and use it to compile
    it for different servers. @html(<br>)

    By using methods, events and properties available from TRtcDataServer,
    you can easily respond to requests by sending an appropriate result
    which will be readable by the connected client, be it a standard Web
    Browser or any application written by using the @Link(TRtcDataClient)
    connection component.

    @Link(TRtcDataServer) also makes sure that you receive requests one by one
    and get the chance to answer them one-by-one, even if the client side
    sends all the requests at once (as one big request list), so
    you can relax and process all incomming requests, without worrying
    about overlapping your responses for different requests.
    @html(<br><br>)

    Component which calls 'Accept' will gain complete control
    over the connection, until a complete response is sent out
    (or connection is closed).

    After the response is done, DataServer again takes over and waits for the
    next request, then the process repeats itself, until all requests have been
    processed or the connection has been closed.

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
    @Link(TRtcDataServer.Accept), @Link(TRtcDataServer.Request), @Link(TRtcConnection.Read) - Read and Accept client Request
    @html(<br>)
    @Link(TRtcDataServer.WriteHeader), @Link(TRtcConnection.Write) - Write result to client
    @html(<br>)
    @Link(TRtcConnection.Disconnect) - Disconnect client
    @html(<br>)
    @Link(TRtcServer.StopListen) - Stop server
    @html(<br><br>)

    Events to check first:
    @html(<br>)
    @Link(TRtcServer.OnListenStart) - Server started
    @html(<br>)
    @Link(TRtcConnection.OnConnecting) - new Client connecting
    @html(<br>)
    @Link(TRtcDataServer.OnRequestNotAccepted) - Request has been received but not accepted by any DataProvider component.
    @html(<br>)
    @Link(TRtcConnection.OnDataSent) - Data sent to client (buffer now empty)
    @html(<br>)
    @Link(TRtcConnection.OnDisconnecting) - one Client disconnecting
    @html(<br>)
    @Link(TRtcServer.OnListenStop) - Server stopped
    @html(<br><br>)

    Check @Link(TRtcServer) and @Link(TRtcConnection) for more info.
    }
  TRtcDataServer = class(TRtcServer)
  private
    FOnRequestAccepted:TRtcNotifyEvent;
    FOnRequestNotAccepted:TRtcNotifyEvent;
    FOnResponseDone:TRtcNotifyEvent;
    FOnSessionOpen:TRtcNotifyEvent;
    FOnSessionClose:TRtcNotifyEvent;

    FSession:TRtcServerSession;

    FActiveLink:TRtcAbsDataServerLink;

    FDataServerLinks:TRtcDataServerLinkList;
    FDataServerLinks_Owner:boolean;

    FMyRequest, FRequest:TRtcServerRequest;
    FMyResponse, FResponse:TRtcServerResponse;

  protected
    // @exclude
    procedure CopyFrom(Dup: TRtcServer); override;

    // @exclude
    procedure AddDataServerLink(Value:TRtcAbsDataServerLink);
    // @exclude
    procedure RemoveDataServerLink(Value:TRtcAbsDataServerLink);
    // @exclude
    procedure RemoveAllDataServerLinks;

    // @exclude
    procedure CallSessionOpen;
    // @exclude
    procedure CallSessionClose(Sender:TObject);

    // @exclude
    procedure CallListenStart; override;
    // @exclude
    procedure CallListenStop; override;

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
    procedure CallDisconnect; override;
    // @exclude
    procedure CallLastWrite; override;

    // @exclude
    procedure SetRequest(const Value: TRtcServerRequest); virtual;
    // @exclude
    procedure SetResponse(const Value: TRtcServerResponse); virtual;

    { @exclude }
    procedure InitSession;

    { **************************************************** }

    { NEW METHODS TO BE IMPLEMENTED BY THE CONNECTION COMPONENT }

  public

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
    procedure Flush; virtual; abstract;

    // You can call WriteHeader to send the Response header out.
    procedure WriteHeader(SendNow:boolean=True); overload; virtual; abstract;
    { You can call WriteHeader with empty 'HeaderText' parameter to
      tell the component that you do not want any HTTP header to be sent. }
    procedure WriteHeader(const HeaderText: string; SendNow:boolean=True); overload; virtual; abstract;

    { **************************************************** }

    // @exclude
    constructor Create(AOwner: TComponent); override;
    // @exclude
    destructor Destroy; override;

    // @exclude
    class function New:TRtcDataServer;

    // @exclude
    procedure CallRequestAccepted; virtual;
    // @exclude
    procedure CallRequestNotAccepted; virtual;
    // @exclude
    procedure CallResponseDone; virtual;

    { Used by DataServerLink components to tell DataServer that
      they are currently checking the request.
      Only needs to be called from OnCheckRequest,
      before the Request has been accepted.
      Once the request is accepted, ActiveDataServerLink will be
      used to process all future Data-related events, until
      the request has been processed or connection closed.
      @exclude }
    procedure SetActiveLink(Link:TRtcAbsDataServerLink);

    { Request handler has to call Accept before it starts processing
      the request, so that all further events remain mapped to the
      active event handlers and don't switch before a reply has been sent. }
    procedure Accept;

    { Find an existing Session with this ID.
      If Session with this ID does not exist,
      or session has expired or session is currently locked,
      returns FALSE. Otherwise, prepares the Session variable
      for use and returns TRUE. }
    function FindSession(const ID:string):boolean;

    { If there is a session with this ID, returns TRUE,
      even if that session is locked. }
    function HaveSession(const ID:string):boolean;

    { If you do not need the Session anymore and do not want to keep the
      session locked until request completes, you can release the Session
      Lock by calling "UnLockSession". After this call, you will no longer
      have access to the Session object, until you lock it again using FindSession. }
    procedure UnLockSession;

    { Create a new Session, with a new and unique Session ID. }
    procedure OpenSession(LockType:TRtcSessionLockType=sesFwdLock);

    { If there is a session with this ID,
      returns TRUE and closes the session. }
    function CloseSession(const ID:string):boolean;

    { Total number of open Sessions }
    function TotalSessionsCount:cardinal;

    { Total number of Sessions currently locked.
      A session is locked after a call to FindSession() and
      unlocked after the Event is done executing. }
    function TotalSessionsLocked:cardinal;

    { Total number of Sessions currently unlocked.
      A session is locked after a call to FindSession() and
      unlocked after the Event is done executing. }
    function TotalSessionsUnlocked:cardinal;

    { Current Request's Session info.
      @html(<br>)
      Before you can access the Session for the Request,
      you have to find the appropriate Session by using the
      FindSession function or create a new session by calling the
      OpenSession method. }
    property Session:TRtcServerSession read FSession;

    { Access to current request information.
      Use Request property to read the request information received.
      Here is all the info that was available in request header.
      To read request's body, use the Read function. }
    property Request:TRtcServerRequest read FRequest write SetRequest;
    { Access to current response information.
      Use Response property to prepare the response header.
      Here you can set all header variables and parameters.
      If there is no content body to send out (only header), you will at
      least have to call 'WriteHeader', or 'Write' without parameters once. }
    property Response:TRtcServerResponse read FResponse write SetResponse;

  published
    { Called after a new request has been accepted.
      You can use this event handler to create a DataTunel and
      assign it to Tunel, in case the request has to be tunelled. }
    property OnRequestAccepted:TRtcNotifyEvent read FOnRequestAccepted write FOnRequestAccepted;
    { Called after a new request has been received, but NOT ACCEPTED.
      You can use this event handler to respond to requests with no DataProvider. }
    property OnRequestNotAccepted:TRtcNotifyEvent read FOnRequestNotAccepted write FOnRequestNotAccepted;
    { Called after a processed response was Done sending data out. }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { Called after a new session has been opened.
      You can use the Session property from this event to get session ID. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { Called before an old session has been closed.
      You can use the Session property from this event to get Session ID. }
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

  TRtcDataServerLink=class;

  { @abstract(DataServer Link wrapper) }
  TRtcAbsDataServerLink=class(TRtcServerComponent)
  private
    FServer: TRtcDataServer;
    FLink: TRtcDataServerLink;
    FOrder: integer;

  protected
    // @exclude
    function CheckLink(Value:TRtcAbsDataServerLink):boolean; virtual;
    // @exclude
    procedure RemoveLink(Value:TRtcAbsDataServerLink); virtual;
    // @exclude
    procedure RemoveServer(Value:TRtcDataServer); virtual;

    // @exclude
    function GetServer: TRtcDataServer; virtual;
    // @exclude
    procedure SetServer(const Value: TRtcDataServer); virtual;

    // @exclude
    function GetLink: TRtcDataServerLink; virtual;
    // @exclude
    procedure SetLink(const Value: TRtcDataServerLink); virtual;

    // @exclude
    function GetOrder: integer; override;
    // @exclude
    procedure SetOrder(const Value: integer); virtual;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); virtual; abstract;

    // @exclude
    procedure Call_ListenStart(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_ListenStop(Sender:TRtcConnection); virtual; abstract;

    // @exclude
    procedure Call_CheckRequest(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_RequestAccepted(Sender:TRtcConnection); virtual; abstract;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); virtual; abstract;

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
    procedure Call_Disconnect(Sender:TRtcConnection); virtual; abstract;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

  published
    { You can link your components (one or more) to a DataServerLink component
      by assigning your @Link(TRtcDataServerLink) component to this Link property.
      Doing this, you only have to set the Server property for the master
      DataServerLink component and don't need to do it for every single
      DataSource component. }
    property Link:TRtcDataServerLink read GetLink write SetLink;
    { You can also link your components (one or more) directly to your
      DataServer connection component by assigning your
      @Link(TRtcDataServer) connection component to this Server property.
      This is useful if you have some complex functionality implemented
      in a single DataSource/DataServerLink component and don't need to drag
      the component through another DataServerLink component to get to
      your DataServer. }
    property Server:TRtcDataServer read GetServer write SetServer;
    { This is the Order in which the components will be asked to
      process a request. The smaller this Order number, the sooner
      a component will be asked to process a request, compared to
      other components connected to the DataServer at the same level.
      Since we could have more levels (more DataServerLink components
      connected to each other), Order only defines the priority
      at the same level.
      @html(<br><br>)

      For example, if DataSourceA has Order=50 and DataServerLinkB has Order=60,
      when both components are assigned to the same parent DataServerLink or directly
      to the DataServer, DataSourceA will receive requests for checking before any
      component assigned to DataServerLinkB, no matter which Order the components
      assigned to DataServerLinkB have. This is because the Order property only
      defines the order in the list of components assigned to the same parent
      component (DataServerLink or DataSource).
      @html(<br><br>)

      To make this simpler, just think of this Order as you would of TabOrder
      in a Form, where DataServer is the Form, DataServerLink are the Panels and
      DataSource are edit controls. TabOrder is defined for each control
      inside its parent (TEdit inside TPanel, child TPanel inside parent TPanel
      or master TPanel directly on the TForm).
      @html(<br><br>)

      Order is especially important for components which could handle
      same requests, like the PHP source handler and File source handler.
      If FileSource handler would have lower order number than the
      PHPSource handler, then the PHP soruce handler would never be executed,
      because all PHP files would be simply sent out in their source code form
      by the File Source handler. This is why the File Source handler has to have
      order number bigger that the PHPSource handler and (preferably) be
      connected to the same component (DataServerLink or DataServer). }
    property CheckOrder:integer read GetOrder write SetOrder default 0;
    end;

  { @abstract(DataServer Link, used to group Data Providers)

    You can use TRtcDataServerLink components to group several related
    @Link(TRtcDataProvider) components. Simply set this component as the
    Link property for all your RtcDataSource components, so that
    you don't have to set the Server property for every single
    TRtcDataProvider component separately. This is useful especially
    when the component is used in a datamodule or a form without
    dataserver and you need to link all the components to
    a DataServer which is on another datamodule or form.
    @html(<br><br>)

    Check @Link(TRtcAbsDataServerLink) for more info. }
  TRtcDataServerLink=class(TRtcAbsDataServerLink)
  private
    FOnListenStart:TRtcNotifyEvent;
    FOnListenStop:TRtcNotifyEvent;
    FOnRequestAccepted:TRtcNotifyEvent;
    FOnResponseDone:TRtcNotifyEvent;
    FOnDisconnect:TRtcNotifyEvent;
    FOnSessionOpen:TRtcNotifyEvent;
    FOnSessionClose:TRtcNotifyEvent;

  protected
    // @exclude
    FDataServerLinks:TRtcDataServerLinkList;

    { Other Call_ methods are not implemented here,
      since only CheckRequest event will be called on the Link,
      while all the other events are being called directly on
      the AbsDataServerLink component which implements them
      (for example, TRtcDataProvider).
      @exclude }
    procedure Call_CheckRequest(Sender:TRtcConnection); override;

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
    procedure AddChildLink(Value:TRtcAbsDataServerLink);
    // @exclude
    procedure RemoveChildLink(Value:TRtcAbsDataServerLink);
    // @exclude
    procedure RemoveAllChildLinks;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    { ListenStart and ListenStop methods are used for
      component initialization on server start and
      deinitialization on server stop.
      @exclude }
    procedure Call_ListenStart(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ListenStop(Sender:TRtcConnection); override;
    { Accepted can be used to prepare the Request after it has been accepted.
      @exclude }
    procedure Call_RequestAccepted(Sender:TRtcConnection); override;
    { ResponseDone can be used to complete thingsprepared with RequestAccepted.
      @exclude }
    procedure Call_ResponseDone(Sender:TRtcConnection); override;
    { Disconnect can be used for request deinitialization.
      @exclude }
    procedure Call_Disconnect(Sender:TRtcConnection); override;
    { New Session Open.
      @exclude }
    procedure Call_SessionOpen(Sender:TRtcConnection); override;
    { Existing Session Closing.
      @exclude }
    procedure Call_SessionClose(Sender:TRtcConnection); override;

  published
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
    { Event to be triggered when a response to server has been sent (Response.Done) }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { Event to be triggered when connection gets lost after a request was accepted.
      You can use this event for component deinitialization. }
    property OnDisconnect:TRtcNotifyEvent read FOnDisconnect write FOnDisconnect;
    { Event to be triggered after new Session was opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { Event to be triggered before existing Session closes. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;
    end;

  { @abstract(DualDataServerLink, used to link Data Providers to two Servers)

    You can use TRtcDualDataServerLink components to link several related
    @Link(TRtcDataProvider) components to two Servers (for example,
    HTTP and HTTPS). Simply set this component as the Link property
    for all your RtcDataSource components, then set both Servers as
    this components "Server" and "Server2", or "Link" and "Link2" properties.
    You can also combine multiple DualDataServerLink components if
    you want your code running with more Servers.
    @html(<br><br>)

    Check @Link(TRtcAbsDataServerLink) for more info. }
  TRtcDualDataServerLink=class(TRtcDataServerLink)
  private
    FServer2: TRtcDataServer;
    FLink2: TRtcDataServerLink;

  protected
    // @exclude
    function CheckLink(Value:TRtcAbsDataServerLink):boolean; override;
    // @exclude
    procedure RemoveLink(Value:TRtcAbsDataServerLink); override;
    // @exclude
    procedure RemoveServer(Value:TRtcDataServer); override;

    // @exclude
    function GetServer2: TRtcDataServer; virtual;
    // @exclude
    procedure SetServer2(const Value: TRtcDataServer); virtual;

    // @exclude
    function GetLink2: TRtcDataServerLink; virtual;
    // @exclude
    procedure SetLink2(const Value: TRtcDataServerLink); virtual;

    // @exclude
    procedure SetServer(const Value: TRtcDataServer); override;
    // @exclude
    procedure SetLink(const Value: TRtcDataServerLink); override;

  protected
    // @exclude
    procedure SetOrder(const Value: integer); override;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    // @exclude
    procedure Call_RequestAccepted(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender: TRtcConnection); override;
    // @exclude
    procedure Call_Disconnect(Sender:TRtcConnection); override;

  published
    { You can link your components (one or more) to a DataServerLink component
      by assigning your @Link(TRtcDataServerLink) component to this Link property.
      Doing this, you only have to set the Server property for the master
      DataServerLink component and don't need to do it for every single
      DataSource component. }
    property Link2:TRtcDataServerLink read GetLink2 write SetLink2;
    { You can also link your components (one or more) directly to your
      DataServer connection component by assigning your
      @Link(TRtcDataServer) connection component to this Server property.
      This is useful if you have some complex functionality implemented
      in a single DataSource/DataServerLink component and don't need to drag
      the component through another DataServerLink component to get to
      your DataServer. }
    property Server2:TRtcDataServer read GetServer2 write SetServer2;
    end;

  { @abstract(Data Provider, used to implement events for processing Requests from DataServer)

    You can use TRtcDataProvider components to implement event handlers
    for different requests and combine them to compile a
    Server which can handle any request implemented in those handlers.
    @html(<br><br>)

    By implementing events specified by this component, then
    assigning your @Link(TRtcDataServer) connection component to this
    component's @Link(TRtcAbsDataServerLink.Server) property, or
    @Link(TRtcDataServerLink) (which also has to be somewhere connected to
    the DataServer connection component) to this component's
    @Link(TRtcAbsDataServerLink.Link) property, you can simply integrate
    diferent request handlers into your DataServer. For example,
    a File source (to send files from disk) and a PHP source
    (to read php files from disk, process them by PHP parser and
    send the resulting page out).
    @html(<br><br>)

    Check @Link(TRtcAbsDataServerLink) for more info. }
  TRtcDataProvider=class(TRtcAbsDataServerLink)
  protected
    // @exclude
    FOnListenStart: TRtcNotifyEvent;
    // @exclude
    FOnListenStop: TRtcNotifyEvent;
    // @exclude
    FOnDisconnect: TRtcNotifyEvent;
    // @exclude
    FOnCheckRequest: TRtcNotifyEvent;
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
    FOnSessionOpen: TRtcNotifyEvent;
    // @exclude
    FOnSessionClose: TRtcNotifyEvent;

    // @exclude
    procedure Call_RequestAccepted(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ListenStart(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ListenStop(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_CheckRequest(Sender:TRtcConnection); override;

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

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); override;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

  published
    { This event will be mapped as @Link(TRtcServer.OnListenStart) event
      to the assigned Server component and called AFTER the Server's
      OnListenStart event, for all components. This event can be used
      to initialize the component after server starts listening. }
    property OnListenStart:TRtcNotifyEvent read FOnListenStart write FOnListenStart;
    { This event will be mapped as @Link(TRtcServer.OnListenStop) event
      to the assigned Server component and called BEFORE the Server's
      OnListenStop event, for all components. This event can be used
      to de-initialize the component before server stops listening. }
    property OnListenStop:TRtcNotifyEvent read FOnListenStop write FOnListenStop;

    { This event will be called after new session has been opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { This event will be called before existing session is about to close. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;

    { This event will be called when a new Request comes from a client and its
      headers have been read (but not content body). What this event has to do
      is to check the request information available, without reading the request
      body (!do not call 'Read' here) and decide wether it wants to process this
      request or not. If it wants to process the request, it just has to accept it
      here. The processing has to be done from other events defined for this DataProvider.
      @html(<br><br>)
      To accept the request as its own (and so be able to respond to the client),
      DataPrivder has to call "Accept" from it's 'OnCheckRequest' event handler.
      DataProvider has to be able to recognize the Request as it's own,
      simply by checking the Request property of the Sender (as TRtcDataServer),
      without reading the request body (content part).
      @html(<br><br>)

      Example of one OnCheckRequest and OnDataReceived events implementation:
      @longcode(#
        procedure TWebServer.TimeSourceCheckRequest(Sender: TRtcConnection);
          begin
          with TRtcDataServer(Sender) do
            if UpperCase(Request.FileName)='/TIME' then
              Accept; // Accept the request.
          // After accepting the request, all your other events will be mapped
          // to the connection, so you will have complete control over the
          // received data and the art of your response.
          end;
        procedure TWebServer.TimeSourceDataReceived(Sender: TRtcConnection);
          begin
          // If the request body is small or of no special interest to you,
          // You can simply respond to the request after it has been completely loaded.
          with TRtcDataServer(Sender) do
            if Request.Complete then
              Write(FormatDateTime('dddd, dd.mm.yyyy. hh:nn:ss',Now));
          end; #)
      This simple implementation is a complete implementation for a HTML page
      that shows current date and time on the server when user asks for "/TIME".
      To see what the above example does, click here: http://www.realthinclient.com/time
      @html(<br><br>)

      If your component doesn't Accept the request when it first receives
      it in its CheckRequest event, the same request will be passed to the
      next component in the list, until one component accepts the request.
      If the request was not accepted after all CheckRequest events from all
      components assigned to the Server were passed, then Server's
      OnRequestNotAccepted event will be called. If a component accepts a request,
      all furure events regarding this request will be mapped to the
      component which accepted the request.
      @html(<br><br>)

      This means that CheckRequest is the only event which will be called
      for all DataProvider components, until a component is found which
      wants to process the request (the one that Accepts the request).
      All other request-related events (all but OnListenStart and
      OnListenStop) will ONLY be called by the event handlers defined
      by the component which accepted the request. }
    property OnCheckRequest:TRtcNotifyEvent read FOnCheckRequest write FOnCheckRequest;

    { This event will be mapped as TRtcConnection.OnDataReceived event
      to the assigned Server component and called for all DataReceived
      events for the accepted request. This means that, after you
      have accepted a request from your component's CheckRequest event
      handler, all Server's OnDataReceived events will be only mapped to
      your component's OnDataReceived event. No other component, including
      the DataServer, will receive those events. }
    property OnDataReceived:TRtcNotifyEvent read FOnDataReceived write FOnDataReceived;

    { This event will be mapped as @Link(TRtcConnection.OnDataOut) event
      to the assigned Server component and called for all DataOut
      events for the accepted request. After you have accepted a request from
      your component's CheckRequest event handler, all Server's OnDataOut events
      will be mapped to your component's OnDataOut event. No other component,
      except for the DataServer, will receive those events. @html(<br><br>)

      You can use this event to count how many bytes have been written out. }
    property OnDataOut:TRtcNotifyEvent read FOnDataOut write FOnDataOut;

    { This event will be mapped as @Link(TRtcConnection.OnDataIn) event
      to the assigned Server component and called for all DataIn
      events for the accepted request. After you have accepted a request from
      your component's CheckRequest event handler, all Server's OnDataIn events
      will be mapped to your component's OnDataIn event. No other component,
      except for the DataServer, will receive those events.

      You can use this event to count how many bytes have been read in. }
    property OnDataIn:TRtcNotifyEvent read FOnDataIn write FOnDataIn;

    { This event will be mapped as @Link(TRtcConnection.OnDataSent) event
      to the assigned Server component and called for all DataSent
      events for the accepted request. This means that, after you
      have accepted a request from your component's CheckRequest event
      handler, all Server's OnDataSent events will be only mapped to
      your component's OnDataSent event. No other component, including
      the DataServer, will receive those events. }
    property OnDataSent:TRtcNotifyEvent read FOnDataSent write FOnDataSent;
    { This event will be mapped as @Link(TRtcConnection.OnReadyToSend) event
      to the assigned Server component and called for all ReadyToSend
      events for the accepted request. This means that, after you
      have accepted a request from your component's CheckRequest event
      handler, all Server's OnReadyToSend events will be only mapped to
      your component's OnReadyToSend event. No other component, including
      the DataServer, will receive those events, until you process the
      request completely. }
    property OnReadyToSend:TRtcNotifyEvent read FOnReadyToSend write FOnReadyToSend;
    { This event will be mapped as @Link(TRtcConnection.OnDisconnect) event
      to the assigned Server component and called if your connection gets
      closed while you are still processing the request you accepted.
      This means that, after you have accepted a request from your component's
      CheckRequest event handler, if connection closes before your complete result
      has been sent out to the client, your component will be the only one
      to receive this OnDisconnect event. No other component, including the
      DataServer, will receive the Disconnect event if your component did
      not finish processing the request and sending the resulting data.
      @html(<br><br>)

      If you want to react to clients connecting and disconnecting to your
      Data Server regardless of those event mappings, use the OnConnecting and
      OnDisconnecting events instead of OnConnect/OnDisconnect. }
    property OnDisconnect:TRtcNotifyEvent read FOnDisconnect write FOnDisconnect;
    end;

implementation

uses
  memStrObjList,
  SysUtils,
  rtcSyncObjs;

var
  SessCS:TRtcCritSec;
  SessExpList:tStrObjList; // Unlocked Sessions, sorted by Expiring date+time, PeerAddr & Session ID
  SessUnLockList:tStrObjList; // UnLocked Sessions, sorted by Session ID
  SessLockList:tStrObjList; // Locked Sessions, sorted by Session ID

procedure InitSessions;
  begin
  SessCS:=TRtcCritSec.Create;
  SessExpList:=tStrObjList.Create(128);
  SessUnLockList:=tStrObjList.Create(128);
  SessLockList:=tStrObjList.Create(128);
  end;

function GetTotalSessCount:cardinal;
  begin
  SessCS.Enter;
  try
    if assigned(SessUnLockList) then
      Result:=SessUnLockList.Count
    else
      Result:=0;
    if assigned(SessLockList) then
      Result:=Result+SessLockList.Count;
  finally
    SessCS.Leave;
    end;
  end;

function GetTotalLockSessCount:cardinal;
  begin
  SessCS.Enter;
  try
    if assigned(SessLockList) then
      Result:=SessLockList.Count
    else
      Result:=0;
  finally
    SessCS.Leave;
    end;
  end;

function GetTotalUnlockSessCount:cardinal;
  begin
  SessCS.Enter;
  try
    if assigned(SessUnLockList) then
      Result:=SessUnLockList.Count
    else
      Result:=0;
  finally
    SessCS.Leave;
    end;
  end;

procedure DoneSessions;
  var
    o:TObject;
    id:string;
  begin
  SessCS.Enter;
  try
    if assigned(SessUnLockList) then
      begin
      id:=SessUnLockList.search_min(o);
      while id<>'' do
        begin
        o.Free;
        SessUnLockList.remove(id);
        id:=SessUnLockList.search_min(o);
        end;
      SessUnLockList.Free;
      SessUnLockList:=nil;
      end;

    if assigned(SessLockList) then
      begin
      id:=SessLockList.search_min(o);
      while id<>'' do
        begin
        o.Free;
        SessLockList.remove(id);
        id:=SessLockList.search_min(o);
        end;
      SessLockList.Free;
      SessLockList:=nil;
      end;

    if assigned(SessExpList) then
      begin
      SessExpList.Free;
      SessExpList:=nil;
      end;
  finally
    SessCS.Leave;
    end;

  Garbage(SessCS);
  end;

{$IFDEF UseGUIDs}
function NewSessionID:string;
  var
    GUID: TGUID;
  function GuidToStr:string;
    begin
    SetLength(Result, 32);
    StrLFmt(PChar(Result), 32,'%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',   // do not localize
    [GUID.D1, GUID.D2, GUID.D3, GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
    GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]);
    end;
  begin
  if CoCreateGuid(GUID) = S_OK then
    Result := GUIDToStr
  else
    Result := '';
  end;
{$ELSE}
function NewSessionID(const OldID:string; const PeerAddr:string):string;
  var
    tmp:string;
  function PeerAddrToSessionID:string;
    const
      code:array[0..15] of char =
                ('a','A','b','B','c','C','d','D','e','E','f','F','g','G','h','H');
    var
      ip,loc:integer;
      myIP:byte;
      st:array[1..4] of string;
    begin
    Result:='';
    for ip:=1 to 4 do
      st[ip]:='';

    // Sort out numbers from the IP address
    ip:=1; loc:=1;
    while length(PeerAddr)>=loc do
      begin
      if PeerAddr[loc] in ['0'..'9'] then
        st[ip]:=st[ip]+PeerAddr[loc]
      else if PeerAddr[loc]='.' then
        Inc(ip);
      Inc(loc);
      end;

    // Convert IP numbers to Hex string
    for ip:=1 to 4 do
      begin
      if st[ip]<>'' then
        begin
        myIP:=StrToIntDef(st[ip],0);
        Result := Result +
                  code[myIP shr 4 and $F]+
                  code[myIP and $F];
        end
      else
        Result:=Result+code[0]+code[0];
      end;

    // Add 10 random letters/numbers
    for loc:=1 to 10 do
      begin
      ip:=random(10+26+26);
      if ip<10 then
        Result:=Result+char(Ord('0')+ip)
      else if ip<36 then
        Result:=Result+char(Ord('A')+ip-10)
      else
        Result:=Result+char(Ord('a')+ip-36)
      end;
    end;
  procedure IncID(var ID:string);
    var
      loc:integer;
      ok:boolean;
    begin
    ok:=False;
    loc:=length(ID);
    while loc>0 do
      begin
      if ID[loc]='9' then
        begin
        ID[loc]:='A';
        ok:=True;
        Break;
        end
      else if ID[loc]='Z' then
        begin
        ID[loc]:='a';
        ok:=True;
        Break;
        end
      else if ID[loc]='z' then
        ID[loc]:='0' // carry 1 forward
      else
        begin
        ID[loc]:=Char(Ord(ID[loc])+1);
        ok:=True;
        Break;
        end;
      Dec(Loc);
      end;
    if not ok then
      ID:='1'+ID;
    end;

  begin
  if OldID='' then
    Result:='1'+PeerAddrToSessionID
  else
    begin
    Result:=PeerAddrToSessionID;
    tmp:=Copy(OldID,1,length(OldID)-length(Result));
    IncID(tmp);
    Result:=tmp+Result;
    end;
  end;
{$ENDIF}

function SessionTimeToStr(v:TDateTime):String;
  var
    y,m,d,hh,mm,ss,ms:word;
    p:integer;
    str:string;
    len:word;
  begin
  Result:='00000000000000000';

  DecodeDate(v, y,m,d);
  DecodeTime(v, hh,mm,ss,ms);

  p:=1;
  str:=IntToStr(y); len:=length(str);
  Inc(p,4-len); Move(str[1],Result[p],len); Inc(p,len);

  str:=IntToStr(m); len:=length(str);
  Inc(p,2-len); Move(str[1],Result[p],len); Inc(p,len);

  str:=IntToStr(d); len:=length(str);
  Inc(p,2-len); Move(str[1],Result[p],len); Inc(p,len);

  str:=IntToStr(hh); len:=length(str);
  Inc(p,2-len); Move(str[1],Result[p],len); Inc(p,len);

  str:=IntToStr(mm); len:=length(str);
  Inc(p,2-len); Move(str[1],Result[p],len); Inc(p,len);

  str:=IntToStr(ss); len:=length(str);
  Inc(p,2-len); Move(str[1],Result[p],len); Inc(p,len);

  str:=IntToStr(ms); len:=length(str);
  Inc(p,3-len); Move(str[1],Result[p],len);
  end;

{function SessionTimeToStr(DT:TDateTime):string;
  begin
  Result:=FormatDateTime('yyyymmddhhnnss',DT);
  end;}

{ Find Session by SessionID and Lock the Session:
   - remove from UnLocked and Expiring list
   - add to Locked list }
function FindSession(const ID,PeerAddr,ForwardedFor: string):TRtcServerSession;
  var
    id2:string;
    o:TObject;
  begin
  if ID='' then
    Result:=nil
  else
    begin
    SessCS.Enter;
    try
      if assigned(SessUnLockList) then
        begin
        id2:=ID;
        {$IFNDEF UseGUIDs}
        // Set ID to desired length, so it will be sorted as a numeric value.
        while length(id2)<RTC_SESSIONID_LENGTH do
          id2:=' '+id2;
        {$ENDIF}

        // Find Session with Session ID
        o:=SessUnLockList.search(id2);
        if o<>nil then
          begin
          Result:=TRtcServerSession(o);

          // Check if we have the right to access this session
          if Result.DenyAccess(PeerAddr,ForwardedFor) then
            Result:=nil
          else
            begin
            // Remove from UnLocked list
            SessUnLockList.remove(id2);
            // Add to Locked list
            SessLockList.insert(id2,Result);

            // Remove from Expiring list
            SessExpList.remove(SessionTimeToStr(Result.ExpireTime)+Result.ID);
            end;
          end
        else
          Result:=nil;
        end
      else
        Result:=nil;
    finally
      SessCS.Leave;
      end;
    end;
  end;

{ Check if Session with SessionID exists }
function HaveSession(const ID,PeerAddr,ForwardedFor: string):boolean;
  var
    id2:string;
    o:TObject;
    sess:TRtcServerSession;
  begin
  if ID='' then
    Result:=false
  else
    begin
    SessCS.Enter;
    try
      if assigned(SessUnLockList) then
        begin
        id2:=ID;
        {$IFNDEF UseGUIDs}
        // Set ID to desired length, so it will be sorted as a numeric value.
        while length(id2)<RTC_SESSIONID_LENGTH do
          id2:=' '+id2;
        {$ENDIF}

        // Find Session with Session ID inside unlocked sessions list
        o:=SessUnLockList.search(id2);
        if o<>nil then
          begin
          sess:=TRtcServerSession(o);
          // Check if we have the right to access this session
          Result:=not sess.DenyAccess(PeerAddr,ForwardedFor);
          end
        else
          begin
          // Find Session with Session ID inside Locked Sessions list
          o:=SessLockList.search(id2);
          if o<>nil then
            begin
            sess:=TRtcServerSession(o);
            // Check if we have the right to access this session
            Result:=not sess.DenyAccess(PeerAddr,ForwardedFor);
            end
          else
            Result:=False;
          end;
        end
      else
        Result:=False;
    finally
      SessCS.Leave;
      end;
    end;
  end;

{ Find Session by SessionID and Close the Session }
function CloseSessionID(const ID,PeerAddr,ForwardedFor: string; _Event:TRtcSimpleEvent):boolean;
  var
    id2:string;
    o:TObject;
    sess:TRtcServerSession;
  begin
  if ID='' then
    Result:=false
  else
    begin
    SessCS.Enter;
    try
      if assigned(SessUnLockList) then
        begin
        id2:=ID;
        {$IFNDEF UseGUIDs}
        // Set ID to desired length, so it will be sorted as a numeric value.
        while length(id2)<RTC_SESSIONID_LENGTH do
          id2:=' '+id2;
        {$ENDIF}

        // Find Session with Session ID inside unlocked sessions list
        o:=SessUnLockList.search(id2);
        if o<>nil then
          begin
          sess:=TRtcServerSession(o);
          // Check if we have the right to access this session
          if sess.DenyAccess(PeerAddr,ForwardedFor) then
            Result:=False
          else
            begin
            // Remove from UnLocked list
            SessUnLockList.remove(id2);
            // Remove from Expiring list
            SessExpList.remove(SessionTimeToStr(sess.ExpireTime)+sess.ID);
            // Call SessionClose event.
            if assigned(_Event) then
              _Event(sess);
            // Free session object
            sess.Free;
            Result:=True;
            end;
          end
        else
          begin
          // Find Session with Session ID inside Locked Sessions list
          o:=SessLockList.search(id2);
          if o<>nil then
            begin
            sess:=TRtcServerSession(o);
            // Check if we have the right to access this session
            if sess.DenyAccess(PeerAddr,ForwardedFor) then
              Result:=False
            else
              begin
              sess.FinalExpire:=Now;
              Result:=True;
              end;
            end
          else
            Result:=False;
          end;
        end
      else
        Result:=False;
    finally
      SessCS.Leave;
      end;
    end;
  end;

{ TRtcDataServer }

class function TRtcDataServer.New: TRtcDataServer;
  begin
  Result:=Create(nil);
  end;

constructor TRtcDataServer.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FMyRequest:=TRtcServerRequest.Create;
  FMyResponse:=TRtcServerResponse.Create;

  FRequest:=FMyRequest;
  FResponse:=FMyResponse;

  FActiveLink:=nil;
  FDataServerLinks:=nil;
  FDataServerLinks_Owner:=False;
  FSession:=nil;
  end;

destructor TRtcDataServer.Destroy;
  begin
  if FDataServerLinks_Owner and assigned(FDataServerLinks) then
    begin
    RemoveAllDataServerLinks;
    FDataServerLinks_Owner:=False;
    FDataServerLinks.Free;
    FDataServerLinks:=nil;
    end;
  InitSession;
  FActiveLink:=nil;

  FMyRequest.Free; FRequest:=nil;
  FMyResponse.Free; FResponse:=nil;
  inherited;
  end;

procedure TRtcDataServer.CopyFrom(Dup: TRtcServer);
  begin
  inherited CopyFrom(Dup);

  FActiveLink:=nil;
  InitSession;

  OnRequestAccepted:=TRtcDataServer(Dup).OnRequestAccepted;
  OnRequestNotAccepted:=TRtcDataServer(Dup).OnRequestNotAccepted;
  OnResponseDone:=TRtcDataServer(Dup).OnResponseDone;
  OnSessionOpen:=TRtcDataServer(Dup).OnSessionOpen;
  OnSessionClose:=TRtcDataServer(Dup).OnSessionClose;

  if assigned(FDataServerLinks) and FDataServerLinks_Owner then
    begin
    FDataServerLinks.Free;
    FDataServerLinks:=nil;
    end;
  FDataServerLinks_Owner:=False;
  FDataServerLinks:=TRtcDataServer(Dup).FDataServerLinks;
  end;

procedure TRtcDataServer.CallListenStart;
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  inherited;
  FActiveLink:=nil;
  InitSession;
  if assigned(FDataServerLinks) then
    for idx:=0 to FDataServerLinks.Count-1 do
      begin
      FMyLink:=FDataServerLinks.GetLink(idx);
      FMyLink.Call_ListenStart(self);
      end;
  end;

procedure TRtcDataServer.CallListenStop;
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  if assigned(FDataServerLinks) then
    for idx:=0 to FDataServerLinks.Count-1 do
      begin
      FMyLink:=FDataServerLinks.GetLink(idx);
      FMyLink.Call_ListenStop(self);
      end;
  inherited;
  end;

{ DataServer.OnDataReceived event will be called only if
  there is no DataServerLink component to accept the request. }
procedure TRtcDataServer.CallDataReceived;
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  if Request.Accepted then
    begin
    if assigned(FActiveLink) then
      FActiveLink.Call_DataReceived(self)
    else
      CallRequestNotAccepted;
    Flush;
    end
  else
    begin
    FActiveLink:=nil;
    InitSession;

    if assigned(FDataServerLinks) then
      for idx:=0 to FDataServerLinks.Count-1 do
        begin
        FMyLink:=FDataServerLinks.GetLink(idx);
        FMyLink.Call_CheckRequest(self);
        if Request.Accepted or not Request.Active then
          Break;
        end;

    if Request.Active then
      if not Request.Accepted then
        begin
        InitSession;
        FActiveLink:=nil;
        CallRequestNotAccepted;
        Flush;
        end
      else if not Response.Sent then
        begin
        if assigned(FActiveLink) then
          FActiveLink.Call_DataReceived(self)
        else
          CallRequestNotAccepted;
        Flush;
        end;
    end;
  end;

{ DataServer.OnDataOut event will be called only if
  the request was not accepted by any DataServerLink component
  (DataServer is probably sending the response). }
procedure TRtcDataServer.CallDataOut;
  begin
  if assigned(FActiveLink) then
    FActiveLink.Call_DataOut(self);

  inherited;

  Flush;
  end;

{ DataServer.OnDataIn event will be called only if
  the request was not accepted by any DataServerLink component
  (DataServer is probably receiving a request). }
procedure TRtcDataServer.CallDataIn;
  begin
  if assigned(FActiveLink) then
    FActiveLink.Call_DataIn(self);

  inherited;
  end;

{ DataServer.OnDataSent event will be called only if
  the request was not accepted by any DataServerLink component
  (DataServer is probably sending the response). }
procedure TRtcDataServer.CallDataSent;
  begin
  if assigned(FActiveLink) then
    FActiveLink.Call_DataSent(self)
  else
    inherited;
  Flush;

  // Moved here from CallDataSent !!!!
  if Response.Done then
    begin
    CallResponseDone;

    Request.Accepted:=False;
    Request.Active:=False;
    FActiveLink:=nil;
    InitSession;
    end;
  end;

{ DataServer.OnReadyToSend event will be called if:
  1. DataServer.OnDataSent event was just triggered,
     which means that DataServer is processing a request, or
  2. Response just sent out for the last Request being processed. }
procedure TRtcDataServer.CallReadyToSend;
  begin
  if assigned(FActiveLink) then
    FActiveLink.Call_ReadyToSend(self)
  else
    inherited;
  Flush;
  end;

{ DataServer.OnDisconnect event will ONLY be called if
  DataServer was the one processing a request when
  conncetion got lost. This event means that data
  sent out was most likely not delivered.
  DataServer.OnDisconnect event will NOT be called
  if the request was not accepted.
  To catch all connect and disconnect events for DataServer,
  use OnClientConnect and onClientDisconnect. }
procedure TRtcDataServer.CallDisconnect;
  begin
  if Request.Accepted then
    begin
    if assigned(FActiveLink) then
      FActiveLink.Call_Disconnect(self);
    end;

  inherited; // call DataServer's OnDisconnect event

  FActiveLink:=nil;
  InitSession;
  end;

procedure TRtcDataServer.AddDataServerLink(Value: TRtcAbsDataServerLink);
  begin
  if not assigned(FDataServerLinks) then
    begin
    FDataServerLinks:=TRtcDataServerLinkList.Create;
    FDataServerLinks_Owner:=True;
    end;
  FDataServerLinks.Add(Value);
  end;

procedure TRtcDataServer.RemoveDataServerLink(Value: TRtcAbsDataServerLink);
  begin
  if assigned(FDataServerLinks) then
    FDataServerLinks.Remove(Value);
  end;

procedure TRtcDataServer.RemoveAllDataServerLinks;
  var
    Link:TRtcAbsDataServerLink;
  begin
  if FDataServerLinks_Owner then
    if assigned(FDataServerLinks) then
      while FDataServerLinks.Count>0 do
        begin
        Link:=TRtcAbsDataServerLink(FDataServerLinks.Get(0));
        Link.RemoveServer(self);
        end;
  end;

procedure TRtcDataServer.SetActiveLink(Link: TRtcAbsDataServerLink);
  begin
  FActiveLink:=Link;
  end;

{ TRtcDataServer }

procedure TRtcDataServer.Accept;
  begin
  if not Request.Accepted then
    begin
    Request.Accepted:=True;

    CallRequestAccepted;
    Flush;
    end;
  end;

procedure TRtcDataServer.CallRequestAccepted;
  begin
  if assigned(FActiveLink) then
    FActiveLink.Call_RequestAccepted(self);
  if assigned(FOnRequestAccepted) then
    FOnRequestAccepted(self);
  end;

procedure TRtcDataServer.CallResponseDone;
  begin
  if assigned(FActiveLink) then
    FActiveLink.Call_ResponseDone(self);
  if assigned(FOnResponseDone) then
    FOnResponseDone(self);
  end;

procedure TRtcDataServer.CallRequestNotAccepted;
  begin
  if assigned(FOnRequestNotAccepted) then
    FOnRequestNotAccepted(self)
  else
    Disconnect;
  end;

procedure TRtcDataServer.InitSession;
  begin
  if assigned(FSession) then
    begin
    {$IFDEF FPC}
    FSession.UnLock(@CallSessionClose);
    {$ELSE}
    FSession.UnLock(CallSessionClose);
    {$ENDIF}
    FSession:=nil;
    end;
  end;

function TRtcDataServer.HaveSession(const ID: string): boolean;
  begin
  Result:=TRtcServerSession.Have(ID,PeerAddr,Request.ForwardedFor);
  end;

function TRtcDataServer.CloseSession(const ID: string): boolean;
  begin
  {$IFDEF FPC}
  Result:=TRtcServerSession.CloseID(ID,PeerAddr,Request.ForwardedFor,@CallSessionClose);
  {$ELSE}
  Result:=TRtcServerSession.CloseID(ID,PeerAddr,Request.ForwardedFor,CallSessionClose);
  {$ENDIF}
  end;

function TRtcDataServer.FindSession(const ID: string): boolean;
  begin
  InitSession;
  FSession:=TRtcServerSession.Find(ID,PeerAddr,Request.ForwardedFor);
  Result:=assigned(FSession);
  end;

procedure TRtcDataServer.OpenSession(LockType:TRtcSessionLockType);
  begin
  InitSession;
  FSession:=TRtcServerSession.Open(LockType,PeerAddr,Request.ForwardedFor);
  if assigned(FSession) then
    CallSessionOpen;
  end;

procedure TRtcDataServer.CallSessionClose(Sender: TObject);
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
    FOldSess:TRtcServerSession;
  begin
  FOldSess:=FSession;
  EnterEvent;
  try
    FSession:=TRtcServerSession(Sender);

    if assigned(FDataServerLinks) then
      for idx:=0 to FDataServerLinks.Count-1 do
        begin
        FMyLink:=FDataServerLinks.GetLink(idx);
        FMyLink.Call_SessionClose(self);
        end;

    if assigned(FOnSessionClose) then
      FOnSessionClose(self);
  finally
    FSession:=FOldSess;
    LeaveEvent;
    end;
  end;

procedure TRtcDataServer.CallSessionOpen;
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  EnterEvent;
  try
    if assigned(FOnSessionOpen) then
      FOnSessionOpen(self);

    if assigned(FDataServerLinks) then
      for idx:=0 to FDataServerLinks.Count-1 do
        begin
        FMyLink:=FDataServerLinks.GetLink(idx);
        FMyLink.Call_SessionOpen(self);
        end;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcDataServer.SetRequest(const Value: TRtcServerRequest);
  begin
  FRequest := Value;
  end;

procedure TRtcDataServer.SetResponse(const Value: TRtcServerResponse);
  begin
  FResponse := Value;
  end;

procedure TRtcDataServer.CallLastWrite;
  begin
  inherited;
  InitSession;
  end;

function TRtcDataServer.TotalSessionsCount: cardinal;
  begin
  Result:=GetTotalSessCount;
  end;

function TRtcDataServer.TotalSessionsLocked: cardinal;
  begin
  Result:=GetTotalLockSessCount;
  end;

function TRtcDataServer.TotalSessionsUnlocked: cardinal;
  begin
  Result:=GetTotalUnlockSessCount;
  end;

procedure TRtcDataServer.UnLockSession;
  begin
  InitSession;
  end;

{ TRtcServerComponentList }

constructor TRtcServerComponentList.Create;
  begin
  inherited;
  FList:=TList.Create;
  end;

destructor TRtcServerComponentList.Destroy;
  begin
  FList.Free;
  inherited;
  end;

procedure TRtcServerComponentList.Add(Value: TRtcServerComponent);
  var
    idx:integer;
  begin
  idx:=FList.IndexOf(Value);
  if idx>=0 then
    FList.Delete(idx);

  idx:=0;
  while idx<FList.Count do
    if Get(idx).GetOrder>=Value.GetOrder then
      Break // insert here!
    else
      Inc(idx);

  FList.Insert(idx, Value);
  end;

function TRtcServerComponentList.Count: integer;
  begin
  Result:=FList.Count;
  end;

procedure TRtcServerComponentList.Remove(Value: TRtcServerComponent);
  var
    idx:integer;
  begin
  idx:=FList.IndexOf(Value);
  if idx>=0 then
    FList.Delete(idx);
  end;

procedure TRtcServerComponentList.RemoveAll;
  begin
  FList.Clear;
  end;

function TRtcServerComponentList.Get(index:integer): TRtcServerComponent;
  begin
  if (index>=0) and (index<FList.Count) then
    Result:=TRtcServerComponent(FList.Items[index])
  else
    Result:=nil;
  end;

{ TRtcDataServerLinkList }

function TRtcDataServerLinkList.GetLink(index:integer): TRtcAbsDataServerLink;
  begin
  if (index>=0) and (index<Count) then
    Result:=TRtcAbsDataServerLink(Get(index))
  else
    Result:=nil;
  end;

{ TRtcAbsDataServerLink }

constructor TRtcAbsDataServerLink.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FServer:=nil;
  FLink:=nil;
  FOrder:=0;
  end;

destructor TRtcAbsDataServerLink.Destroy;
  begin
  Server:=nil; // remove from DataServer
  Link:=nil; // remove from parent DataServerLink
  inherited;
  end;

procedure TRtcAbsDataServerLink.SetLink(const Value: TRtcDataServerLink);
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
      Server:=nil; // can not be maped to DataServer and to DataServerLink at the same time.

      // Check for circular reference before assigning!
      if Value=self then
        raise Exception.Create('Circular DataServerLink reference!');
      if Value.CheckLink(self) then
        raise Exception.Create('Circular DataServerLink reference!');
      if CheckLink(Value) then
        raise Exception.Create('Circular DataServerLink reference!');

      FLink:=Value;
      FLink.AddChildLink(self);
      end;
    end;
  end;

function TRtcAbsDataServerLink.GetLink: TRtcDataServerLink;
  begin
  Result:=FLink;
  end;

procedure TRtcAbsDataServerLink.SetServer(const Value: TRtcDataServer);
  begin
  if Value<>FServer then
    begin
    if assigned(FServer) then
      begin
      FServer.RemoveDataServerLink(self);
      FServer:=nil;
      end;

    if assigned(Value) then
      begin
      Link:=nil; // can not be linked to DataServerLink and DataServer at the same time.
      FServer:=Value;
      FServer.AddDataServerLink(self);
      end;
    end;
  end;

function TRtcAbsDataServerLink.GetServer: TRtcDataServer;
  begin
  Result:=FServer;
  end;

procedure TRtcAbsDataServerLink.SetOrder(const Value: integer);
  begin
  if Value<>FOrder then
    begin
    FOrder:=Value;
    // Update order
    if assigned(FLink) then
      FLink.AddChildLink(self)
    else if assigned(FServer) then
      FServer.AddDataServerLink(self);
    end;
  end;

function TRtcAbsDataServerLink.GetOrder: integer;
  begin
  Result:=FOrder;
  end;

procedure TRtcAbsDataServerLink.RemoveLink(Value: TRtcAbsDataServerLink);
  begin
  if Value=FLink then Link:=nil;
  end;

procedure TRtcAbsDataServerLink.RemoveServer(Value: TRtcDataServer);
  begin
  if Value=FServer then Server:=nil;
  end;

function TRtcAbsDataServerLink.CheckLink(Value: TRtcAbsDataServerLink):boolean;
  begin
  if Value=FLink then
    Result:=True
  else if assigned(FLink) then
    Result:=FLink.CheckLink(Value)
  else
    Result:=False;
  end;

{ TRtcDataServerLink }

constructor TRtcDataServerLink.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FDataServerLinks:=TRtcDataServerLinkList.Create;
  end;

destructor TRtcDataServerLink.Destroy;
  begin
  RemoveAllChildLinks;
  FDataServerLinks.Free;
  FDataServerLinks:=nil;
  inherited;
  end;

procedure TRtcDataServerLink.AddChildLink(Value: TRtcAbsDataServerLink);
  begin
  FDataServerLinks.Add(Value);
  end;

procedure TRtcDataServerLink.RemoveChildLink(Value: TRtcAbsDataServerLink);
  begin
  FDataServerLinks.Remove(Value);
  end;

procedure TRtcDataServerLink.RemoveAllChildLinks;
  var
    _Link:TRtcAbsDataServerLink;
  begin
  while FDataServerLinks.Count>0 do
    begin
    _Link:=TRtcAbsDataServerLink(FDataServerLinks.Get(0));
    _Link.RemoveLink(self);
    end;
  end;

procedure TRtcDataServerLink.Call_CheckRequest(Sender:TRtcConnection);
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  for idx:=0 to FDataServerLinks.Count-1 do
    begin
    FMyLink:=FDataServerLinks.GetLink(idx);
    FMyLink.Call_CheckRequest(Sender);
    if TRtcDataServer(Sender).Request.Accepted or
       not TRtcDataServer(Sender).Request.Active then
      Exit;
    end;
  end;

procedure TRtcDataServerLink.Call_ListenStart(Sender: TRtcConnection);
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  if assigned(FOnListenStart) then
    FOnListenStart(Sender);

  for idx:=0 to FDataServerLinks.Count-1 do
    begin
    FMyLink:=FDataServerLinks.GetLink(idx);
    FMyLink.Call_ListenStart(Sender);
    end;
  end;

procedure TRtcDataServerLink.Call_ListenStop(Sender: TRtcConnection);
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  for idx:=0 to FDataServerLinks.Count-1 do
    begin
    FMyLink:=FDataServerLinks.GetLink(idx);
    FMyLink.Call_ListenStop(Sender);
    end;

  if assigned(FOnListenStop) then
    FOnListenStop(Sender);
  end;

procedure TRtcDataServerLink.Call_RequestAccepted(Sender: TRtcConnection);
  begin
  if assigned(FOnRequestAccepted) then
    FOnRequestAccepted(Sender);

  if assigned(Link) then
    Link.Call_RequestAccepted(Sender);
  end;

procedure TRtcDataServerLink.Call_ResponseDone(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseDone) then
    FOnResponseDone(Sender);

  if assigned(Link) then
    Link.Call_ResponseDone(Sender);
  end;

procedure TRtcDataServerLink.Call_Disconnect(Sender:TRtcConnection);
  begin
  if assigned(FOnDisconnect) then
    FOnDisconnect(Sender);

  if assigned(Link) then
    Link.Call_Disconnect(Sender);
  end;

procedure TRtcDataServerLink.Call_DataReceived(Sender:TRtcConnection);
  begin
  // left empty
  end;

procedure TRtcDataServerLink.Call_DataOut(Sender:TRtcConnection);
  begin
  // left empty
  end;

procedure TRtcDataServerLink.Call_DataIn(Sender:TRtcConnection);
  begin
  // left empty
  end;

procedure TRtcDataServerLink.Call_DataSent(Sender:TRtcConnection);
  begin
  // left empty
  end;

procedure TRtcDataServerLink.Call_ReadyToSend(Sender:TRtcConnection);
  begin
  // left empty
  end;

procedure TRtcDataServerLink.Call_SessionClose(Sender: TRtcConnection);
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  for idx:=0 to FDataServerLinks.Count-1 do
    begin
    FMyLink:=FDataServerLinks.GetLink(idx);
    FMyLink.Call_SessionClose(Sender);
    end;

  if assigned(FOnSessionClose) then
    FOnSessionClose(Sender);
  end;

procedure TRtcDataServerLink.Call_SessionOpen(Sender: TRtcConnection);
  var
    idx:integer;
    FMyLink:TRtcAbsDataServerLink;
  begin
  if assigned(FOnSessionOpen) then
    FOnSessionOpen(Sender);

  for idx:=0 to FDataServerLinks.Count-1 do
    begin
    FMyLink:=FDataServerLinks.GetLink(idx);
    FMyLink.Call_SessionOpen(Sender);
    end;
  end;

{ TRtcDualDataServerLink }

constructor TRtcDualDataServerLink.Create(AOwner: TComponent);
  begin
  inherited;
  FServer2:=nil;
  FLink2:=nil;
  end;

destructor TRtcDualDataServerLink.Destroy;
  begin
  Server2:=nil;
  Link2:=nil;
  inherited;
  end;

function TRtcDualDataServerLink.GetServer2: TRtcDataServer;
  begin
  Result:=FServer2;
  end;

function TRtcDualDataServerLink.GetLink2: TRtcDataServerLink;
  begin
  Result:=FLink2;
  end;

procedure TRtcDualDataServerLink.SetServer2(const Value: TRtcDataServer);
  begin
  if Value<>FServer2 then
    begin
    if assigned(FServer) and (FServer=Value) then
      Server:=nil;

    if assigned(FServer2) then
      begin
      FServer2.RemoveDataServerLink(self);
      FServer2:=nil;
      end;

    if assigned(Value) then
      begin
      Link2:=nil;
      FServer2:=Value;
      FServer2.AddDataServerLink(self);
      end;
    end;
  end;

procedure TRtcDualDataServerLink.SetLink2(const Value: TRtcDataServerLink);
  begin
  if Value<>FLink2 then
    begin
    if assigned(FLink) and (FLink=Value) then
      Link:=nil;

    if assigned(FLink2) then
      begin
      FLink2.RemoveChildLink(self);
      FLink2:=nil;
      end;

    if assigned(Value) then
      begin
      Server2:=nil; // can not be maped to Server2 and to Link2 at the same time.

      // Check for circular reference before assigning!
      if Value=self then
        raise Exception.Create('Circular DataServerLink reference!');
      if Value.CheckLink(self) then
        raise Exception.Create('Circular DataServerLink reference!');
      if CheckLink(Value) then
        raise Exception.Create('Circular DataServerLink reference!');

      FLink2:=Value;
      FLink2.AddChildLink(self);
      end;
    end;
  end;

procedure TRtcDualDataServerLink.SetServer(const Value: TRtcDataServer);
  begin
  if Value<>FServer then
    begin
    if assigned(FServer2) and (FServer2=Value) then
      Server2:=nil;

    inherited SetServer(Value);
    end;
  end;

procedure TRtcDualDataServerLink.SetLink(const Value: TRtcDataServerLink);
  begin
  if Value<>FLink then
    begin
    if assigned(FLink2) and (FLink2=Value) then
      Link2:=nil;

    inherited SetLink(Value);
    end;
  end;

procedure TRtcDualDataServerLink.SetOrder(const Value: integer);
  begin
  if Value<>FOrder then
    begin
    FOrder:=Value;
    // Update order
    if assigned(FLink2) then
      FLink2.AddChildLink(self)
    else if assigned(FServer2) then
      FServer2.AddDataServerLink(self);
    if assigned(FLink) then
      FLink.AddChildLink(self)
    else if assigned(FServer) then
      FServer.AddDataServerLink(self);
    end;
  end;

procedure TRtcDualDataServerLink.RemoveLink(Value: TRtcAbsDataServerLink);
  begin
  inherited RemoveLink(Value);
  if Value=FLink2 then Link2:=nil;
  end;

function TRtcDualDataServerLink.CheckLink(Value: TRtcAbsDataServerLink):boolean;
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

procedure TRtcDualDataServerLink.RemoveServer(Value: TRtcDataServer);
  begin
  inherited RemoveServer(Value);
  if Value=FServer2 then Server2:=nil;
  end;

procedure TRtcDualDataServerLink.Call_RequestAccepted(Sender: TRtcConnection);
  begin
  inherited Call_RequestAccepted(Sender);

  if assigned(Link2) then
    Link2.Call_RequestAccepted(Sender);
  end;

procedure TRtcDualDataServerLink.Call_ResponseDone(Sender: TRtcConnection);
  begin
  inherited Call_ResponseDone(Sender);

  if assigned(Link2) then
    Link2.Call_ResponseDone(Sender);
  end;

procedure TRtcDualDataServerLink.Call_Disconnect(Sender:TRtcConnection);
  begin
  inherited Call_Disconnect(Sender);

  if assigned(Link2) then
    Link2.Call_Disconnect(Sender);
  end;

{ TRtcDataProvider }

constructor TRtcDataProvider.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FOnDisconnect:=nil;
  FOnCheckRequest:=nil;
  FOnDataReceived:=nil;
  FOnReadyToSend:=nil;
  FOnDataOut:=nil;
  FOnDataIn:=nil;
  FOnDataSent:=nil;
  end;

destructor TRtcDataProvider.Destroy;
  begin
  FOnDisconnect:=nil;
  FOnCheckRequest:=nil;
  FOnDataReceived:=nil;
  FOnReadyToSend:=nil;
  FOnDataOut:=nil;
  FOnDataIn:=nil;
  FOnDataSent:=nil;
  inherited;
  end;

procedure TRtcDataProvider.Call_CheckRequest(Sender:TRtcConnection);
  begin
  if assigned(OnCheckRequest) then
    begin
    TRtcDataServer(Sender).SetActiveLink(self);
    OnCheckRequest(Sender);
    end;
  end;

procedure TRtcDataProvider.Call_DataReceived(Sender:TRtcConnection);
  begin
  if assigned(OnDataReceived) then
    OnDataReceived(Sender);
  end;

procedure TRtcDataProvider.Call_DataOut(Sender:TRtcConnection);
  begin
  if assigned(OnDataOut) then
    OnDataOut(Sender);
  end;

procedure TRtcDataProvider.Call_DataIn(Sender:TRtcConnection);
  begin
  if assigned(OnDataIn) then
    OnDataIn(Sender);
  end;

procedure TRtcDataProvider.Call_DataSent(Sender:TRtcConnection);
  begin
  if assigned(OnDataSent) then
    OnDataSent(Sender);
  end;

procedure TRtcDataProvider.Call_ReadyToSend(Sender:TRtcConnection);
  begin
  if assigned(OnReadyToSend) then
    OnReadyToSend(Sender);
  end;

procedure TRtcDataProvider.Call_ListenStart(Sender:TRtcConnection);
  begin
  if assigned(OnListenStart) then
    OnListenStart(Sender);
  end;

procedure TRtcDataProvider.Call_ListenStop(Sender:TRtcConnection);
  begin
  if assigned(OnListenStop) then
    OnListenStop(Sender);
  end;

procedure TRtcDataProvider.Call_RequestAccepted(Sender: TRtcConnection);
  begin
  if assigned(Link) then
    Link.Call_RequestAccepted(Sender);
  end;

procedure TRtcDataProvider.Call_ResponseDone(Sender: TRtcConnection);
  begin
  if assigned(Link) then
    Link.Call_ResponseDone(Sender);
  end;

procedure TRtcDataProvider.Call_Disconnect(Sender:TRtcConnection);
  begin
  if assigned(OnDisconnect) then
    OnDisconnect(Sender);

  if assigned(Link) then
    Link.Call_Disconnect(Sender);
  end;

procedure TRtcDataProvider.Call_SessionOpen(Sender: TRtcConnection);
  begin
  if assigned(OnSessionOpen) then
    OnSessionOpen(Sender);
  end;

procedure TRtcDataProvider.Call_SessionClose(Sender: TRtcConnection);
  begin
  if assigned(OnSessionClose) then
    OnSessionClose(Sender);
  end;

{ TRtcServerSession }

class function TRtcServerSession.Find(const _ID,_PeerAddr,_ForwardedFor: string): TRtcServerSession;
  begin
  Result:=FindSession(_ID,_PeerAddr,_ForwardedFor);
  end;

class function TRtcServerSession.Have(const _ID,_PeerAddr,_ForwardedFor: string): boolean;
  begin
  Result:=HaveSession(_ID,_PeerAddr,_ForwardedFor);
  end;

class function TRtcServerSession.CloseID(const _ID,_PeerAddr,_ForwardedFor: string; _Event:TRtcSimpleEvent): boolean;
  begin
  Result:=CloseSessionID(_ID,_PeerAddr,_ForwardedFor,_Event);
  end;

{ Define Session ID and Lock the Session }
procedure TRtcServerSession.LockSessionID;
{$IFNDEF UseGUIDs}
  var
    id1,id2:string;
    o:TObject;
{$ENDIF}
  begin
  SessCS.Enter;
  try
    if assigned(SessLockList) then
      begin
      {$IFDEF UseGUIDs}
      FID:=NewSessionID;
      // Add Session to Locked list
      SessLockList.insert(FID, self);
      {$ELSE}
      // Find max. session ID in use
      if SessLockList.search_max(o)<>'' then
        id1:=TRtcServerSession(o).ID
      else
        id1:='';
      if SessUnlockList.search_max(o)<>'' then
        id2:=TRtcServerSession(o).ID
      else
        id2:='';
      if id2>id1 then id1:=id2;

      // Define new session ID
      id2:=NewSessionID(id1, PeerAddr);
      FID:=id2;

      // Set ID to desired length, so it will be sorted as a numeric value.
      while length(id2)<RTC_SESSIONID_LENGTH do // 10 for SessionID + 8 for PeerAddr + 10 random
        id2:=' '+id2;

      // Add Session to Locked list
      SessLockList.insert(id2, self);
      {$ENDIF}
      end;
  finally
    SessCS.Leave;
    end;
  end;

class function TRtcServerSession.Open(_LockType:TRtcSessionLockType; const _PeerAddr,_ForwardedFor: string): TRtcServerSession;
  begin
  // Create a new Session object
  Result:=TRtcServerSession.Create;
  Result.FLockType:=_LockType;
  Result.FPeerAddr:=_PeerAddr;
  Result.FForwardedFor:=_ForwardedFor;
  Result.FCreated:=Now;
  Result.FLastUsed:=Result.FCreated;

  // Define Session ID and Lock the Session object
  Result.LockSessionID;
  if Result.ID='' then
    begin
    Result.Free;
    raise Exception.Create('Unable to initialize session');
    end;
  end;

function TRtcServerSession.DenyAccess(const _PeerAddr, _ForwardedFor: string): boolean;
  begin
  if LockType=sesNoLock then
    Result:=False
  else if (LockType=sesFwdLock) and
          (ForwardedFor='') and // "X-FORWARDED-FOR" was NOT set by creator - check for IP
          (PeerAddr<>_PeerAddr) then
    Result:=True
  else if (LockType in [sesFwdLock, sesIPFwdLock]) and
          (ForwardedFor<>'') and // "X-FORWARDED-FOR" was set by creator - use value set
          (ForwardedFor<>_ForwardedFor) then
    Result:=True
  else if (LockType in [sesIPLock, sesIPFwdLock]) and
          (PeerAddr<>_PeerAddr) then
    Result:=True
  else
    Result:=False;
  end;

{ UnLock Session and remove all sessions that have expired:
   - remove from Locked list
   - add to UnLocked and Expiring list }
procedure TRtcServerSession.UnLockSession(_Event:TRtcSimpleEvent);
  var
    id2,ex:string;
    o:TObject;
  begin
  if not assigned(SessCS) then Exit;

  SessCS.Enter;
  try
    if assigned(SessUnLockList) then
      begin
      // Check if there are Sessions that have expired but are still in our session list
      ex:=SessExpList.search_min(o);
      while (ex<>'') and (TRtcServerSession(o).ExpireTime<=Now) do
        begin
        // Remove from Expiring list
        SessExpList.remove(ex);

        // Set ID to desired length, so it will be sorted as a numeric value.
        id2:=TRtcServerSession(o).ID;
        {$IFNDEF UseGUIDs}
        while length(id2)<RTC_SESSIONID_LENGTH do
          id2:=' '+id2;
        {$ENDIF}

        // Remove from UnLocked list
        SessUnLockList.remove(id2);

        if assigned(_Event) then
          _Event(o);
        // Free the Session object
        o.Free;

        ex:=SessExpList.search_min(o);
        end;

      id2:=FID;
      {$IFNDEF UseGUIDs}
      // Set ID to desired length, so it will be sorted as a numeric value.
      while length(id2)<RTC_SESSIONID_LENGTH do
        id2:=' '+id2;
      {$ENDIF}

      // Remove from Locked list
      SessLockList.remove(id2);

      // Free if expired, add to expiring list if not.
      if ExpireTime<=Now then
        begin
        if assigned(_Event) then
          _Event(self);
        Free;
        end
      else
        begin
        // Add to UnLocked list
        SessUnLockList.insert(id2, self);
        // Add to Expiring list
        SessExpList.insert(SessionTimeToStr(ExpireTime)+ID, self);
        end;
      end;
  finally
    SessCS.Leave;
    end;
  end;

procedure TRtcServerSession.UnLock(_Event:TRtcSimpleEvent);
  begin
  FLastUsed:=Now;

  // First update ExpireTime with LiveTime
  if FKeepAlive=0 then
    FExpireTime:=Now + RTC_SESSION_TIMEOUT/24/60/60
  else
    FExpireTime:=Now + FKeepAlive/24/60/60;

  // Check if session has to expire (FinalExpire value defined)
  if FFinalExpire<>0 then
    if FExpireTime>FFinalExpire then
      FExpireTime:=FFinalExpire;

  UnLockSession(_Event);
  end;

procedure TRtcServerSession.SetKeepAlive(const _Value: integer);
  begin
  FKeepAlive:=_Value;
  end;

procedure TRtcServerSession.SetFinalExpire(const _Value: TDateTime);
  begin
  FFinalExpire:=_Value;
  end;

procedure TRtcServerSession.Close;
  begin
  FFinalExpire:=Now;
  end;

initialization
Randomize;
InitSessions;
finalization
DoneSessions;
end.
