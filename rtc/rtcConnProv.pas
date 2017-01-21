{
  "Connection Provider wrapper" - Copyright (c) Danijel Tkalcec
  @html(<br>)

  This unit defines the connection provider component wrappers, which define the methods
  and properties every connection provider has to implement. RTC Component suite
  uses Connection Providers to implement connection functionality using low-level
  system code, while the Connection components themselves do not have any system-dependant
  code and therefor are 100% portable between all systems.
  @html(<br><br>)

  Connections create and use connection providers internaly and completely transparent
  from the connection component user. This lose coupling between the connection component
  and the connection provider makes it relatively easy to port the RTC connection components
  to any system. And even more important, port user's code to other systems,
  without major code modifications.

  @exclude
}

unit rtcConnProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  rtcTrashcan,
  SysUtils,
  Windows,

  rtcSyncObjs,
  rtcPlugins,
  rtcThrPool;

type
  // Supported connection states
  TRtcConnectionState = (
                        // Connection inactive
                        conInactive,
                        // Listener (waiting for connections)
                        conListening,
                        // Trying to connect
                        conActivating,
                        // Connection active
                        conActive,
                        // Connection was active, now closing
                        conClosing
                        );

  { Exception raised when the limit for client connections (specified in rtcConn)
    was reached and therefor a new connection could not be opened. }
  EClientLimitReached = class(Exception);

  // Standard connection provider event (no parameters)
  TRtcEvent = procedure of object;
  // Connection provider event with a boolean parameter
  TRtcBoolEvent = procedure(Value:boolean) of object;
  // Exception handling event for the connection provider
  TRtcExceptEvent = procedure(E:Exception) of object;

  // Error handling event for the connection provider
  TRtcErrEvent = procedure(Err:string) of object;
  // Event for creating a new connection provider
  TRtcProviderEvent = procedure(var Provider:TObject) of object;

  { @name is a basic wrapper for any connection provider component.
    It which defines abstract methods and some properties every connection provider
    has to implement. RTC Component suite uses Connection Providers to implement connection
    functionality using low-level system code, while the Connection components themselves
    do not have any system-dependant code and therefor are 100% portable between all systems.

    Connections create and use connection providers internaly and completely transparent
    from the connection component user. This lose coupling between the connection component
    and the connection provider makes it relatively easy to port the RTC connection components
    to any system. And even more important, port user's code to other systems,
    without major code modifications. }
  TRtcConnectionProvider = class
  private
    FInSync:boolean;
    FMultiThreaded:boolean;

    FAddr:string;
    FPort:string;

    FLost,
    FClosing,
    FSilent:boolean;

    // Used for triggering events (provider declares those as virtual abstract)

    FError:TRtcErrEvent;

    FOnReadyToRelease:TRtcEvent;

    FOnBeforeCreate:TrtcEvent;
    FOnAfterDestroy:TrtcEvent;

    FOnConnecting:TrtcEvent;
    FOnConnect:TrtcEvent;
    FOnDisconnect:TrtcEvent;
    FOnDisconnecting:TrtcEvent;

    FOnDataSent:TrtcEvent;
    FOnDataOut:TrtcEvent;
    FOnDataIn:TrtcEvent;
    FOnLastWrite:TrtcEvent;
    FOnDataReceived:TrtcEvent;
    FOnDataLost:TrtcEvent;

    FOnException:TrtcExceptEvent;

    procedure SetLost(a:boolean);
    function GetLost:boolean;

    procedure SetClosing(a:boolean);
    function GetClosing:boolean;

    procedure SetSilent(a:boolean);
    function GetSilent:boolean;

  protected
    { Conncetion provider has to set FDataOut to the number of byte sent out,
      before it calls the TriggerDataOut method. }
    FDataOut:int64;
    { Conncetion provider has to set FDataIn to the number of byte read in,
      before it calls the TriggerDataIn method. }
    FDataIn:int64;

    procedure Enter; virtual; abstract;
    procedure Leave; virtual; abstract;

    { Properties ready for usage by the connection provider (not used directly by the connection) }

    property Lost:boolean read GetLost write SetLost;
    property Closing:boolean read GetClosing write SetClosing;
    property Silent:boolean read GetSilent write SetSilent;

    { Triggers to be used by the ConncetionProvider.
      Connection sets those triggers using SetTrigger_ methods (below).
      Connection provider should use those methods to trigger
      all or most of those events, when they happen. }

    procedure Error(const text:string); virtual;

    procedure TriggerReadyToRelease; virtual;

    procedure TriggerBeforeCreate; virtual;
    procedure TriggerAfterDestroy; virtual;

    procedure TriggerConnecting; virtual;
    procedure TriggerConnect; virtual;
    procedure TriggerDisconnecting; virtual;
    procedure TriggerDisconnect; virtual;

    procedure TriggerDataSent; virtual;
    procedure TriggerDataOut; virtual;
    procedure TriggerDataIn; virtual;
    procedure TriggerLastWrite; virtual;
    procedure TriggerDataReceived; virtual;
    procedure TriggerDataLost; virtual;
    procedure TriggerException(E:Exception); virtual;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    { ********************************************* }

    function GetThread:TRtcThread; virtual; abstract;

    { Methods which have to be implemented by the Connection Provider }

    procedure Release; virtual; abstract;

    // Called when user calls Disconnect (wanted disconnect)
    procedure Disconnect; virtual; abstract;
    // Called when timeout happens or connection gets lost (unwanted disconnect)
    procedure InternalDisconnect; virtual; abstract;

    procedure Check; virtual; abstract;

    function GetParent:TRtcConnectionProvider; virtual; abstract;
    function GetState:TRtcConnectionState; virtual; abstract;

    { Support for multithreaded processing -> }
    function PostJob(Job:TObject; HighPriority:boolean):boolean; virtual; abstract;

    function inMainThread:boolean; virtual; abstract;

    function inThread:boolean; virtual; abstract;
    function SyncEvent(Event:TRtcEvent):boolean; virtual; abstract;

    procedure Processing; virtual; abstract;

    function Pause:boolean; virtual; abstract;
    function Resume:boolean; virtual; abstract;
    { <- multithreading }

    procedure Write(const s:string; SendNow:boolean=True); virtual; abstract;
    function Read:string; virtual; abstract;

    function GetPeerAddr:string; virtual; abstract;
    function GetPeerPort:string; virtual; abstract;
    function GetLocalAddr:string; virtual; abstract;
    function GetLocalPort:string; virtual; abstract;

    { ********************************************* }

    { Methods to be used by the Connection Provider ...
      Component which uses this connection provider has to use Set_ methods (below)
      before calling Connect or Listen, to set this properties. }

    function GetAddr:string;
    function GetPort:string;

    function GetMultiThreaded:boolean;

    { Methods used by the Connection component,
      to set connection properties,
      before calling Connect or Listen. }

    procedure SetAddr(val:string);
    procedure SetPort(val:string);
    procedure SetMultiThreaded(val:boolean);

    { Methods that set Triggers before using the connection provider.
      Those methods are used by the Conncetion component. }

    procedure SetError(Event:TRtcErrEvent);

    procedure SetTriggerReadyToRelease(Event:TrtcEvent);

    procedure SetTriggerBeforeCreate(Event:TrtcEvent);
    procedure SetTriggerAfterDestroy(Event:TrtcEvent);

    procedure SetTriggerConnecting(Event:TrtcEvent);
    procedure SetTriggerConnect(Event:TrtcEvent);
    procedure SetTriggerDisconnect(Event:TrtcEvent);
    procedure SetTriggerDisconnecting(Event:TrtcEvent);

    procedure SetTriggerDataSent(Event:TrtcEvent);
    procedure SetTriggerDataOut(Event:TrtcEvent);
    procedure SetTriggerDataIn(Event:TrtcEvent);
    procedure SetTriggerLastWrite(Event:TrtcEvent);
    procedure SetTriggerDataReceived(Event:TrtcEvent);
    procedure SetTriggerDataLost(Event:TrtcEvent);
    procedure SetTriggerException(Event:TrtcExceptEvent);

    { This property is used to check the number of bytes just sent out.
      It is used by the TriggerDataOut method. }
    property DataOut:int64 read FDataOut;
    { This property is used to check the number of bytes just read in.
      It is used by the TriggerDataIn method. }
    property DataIn:int64 read FDataIn;
    end;

  { @name is a wrapper for the Server-side connection provider.
    It declares abstract methods which all server-side connection providers
    have to implement, so they can be used by the Server-side connecion components. }
  TRtcServerProvider = class(TRtcConnectionProvider)
  private
    // Used for counting connections in use
    FOnConnectionAccepted:TRtcEvent; // we have acepted a new connection
    FOnConnectionAccepting:TRtcEvent; // check if we can accept a new connection
    FOnConnectionLost:TRtcEvent; // we have lost the connection

    // Used for triggering events (provider declares those as virtual abstract)
    FOnNewProvider:TRtcProviderEvent;

    FOnListenStart:TrtcEvent;
    FOnListenStop:TrtcEvent;

    FOnListenLost:TrtcEvent;
    FOnListenError:TrtcExceptEvent;

  public

    { Triggers to be used by the ConncetionProvider.
      Connection component, which uses the provider,
      will set those triggers using SetTrigger_ methods (below). }

    procedure TriggerNewProvider(var Provider:TObject); virtual;

    procedure TriggerConnectionAccepting; virtual;
    procedure TriggerConnectionAccepted; virtual;
    procedure TriggerConnectionLost; virtual;

    procedure TriggerListenStart; virtual;
    procedure TriggerListenStop; virtual;

    procedure TriggerListenError(E:Exception); virtual;
    procedure TriggerListenLost; virtual;

  public
    { *** Methods which have to be implemented by the Connection Provider *** }

    procedure Listen; virtual; abstract;

    { *** Methods used by the connection to set Triggers before using the connection provider. *** }

    procedure SetTriggerConnectionAccepting(Event:TrtcEvent); // check if we can acept a new connection
    procedure SetTriggerConnectionAccepted(Event:TrtcEvent); // we have acepted a new connection
    procedure SetTriggerConnectionLost(Event:TrtcEvent); // we have lost the connection

    procedure SetTriggerNewProvider(Event:TrtcProviderEvent);

    procedure SetTriggerListenStart(Event:TrtcEvent);
    procedure SetTriggerListenStop(Event:TrtcEvent);

    procedure SetTriggerListenError(Event:TrtcExceptEvent);
    procedure SetTriggerListenLost(Event:TrtcEvent);
    end;

  { @name is a wrapper for the Client-side connection provider.
    It declares abstract methods which all server-side connection providers
    have to implement, so they can be used by the Server-side connecion components. }
  TRtcClientProvider = class(TRtcConnectionProvider) // provides basic connection functionality
  private
    // Used for counting connections in use
    FOnConnectionOpening:TrtcBoolEvent; // we are opening a new connection
    FOnConnectionClosing:TrtcEvent; // we are closing the connection

    // Used for triggering events (provider declares those as virtual abstract)
    FOnConnectFail:TrtcEvent;
    FOnConnectLost:TrtcEvent;
    FOnConnectError:TrtcExceptEvent;

  protected

    { Triggers to be used by the ConncetionProvider.
      Connection component, which uses the provider,
      will set those triggers using SetTrigger_ methods (below). }

    procedure TriggerConnectionOpening(Force:boolean); virtual;
    procedure TriggerConnectionClosing; virtual;

    procedure TriggerConnectError(E:Exception); virtual;
    procedure TriggerConnectFail; virtual;
    procedure TriggerConnectLost; virtual;

  public
    { *** Methods which have to be implemented by the Connection Provider *** }

    procedure Connect(Force:boolean=False); virtual; abstract;

    { *** Methods used by the Connection to set Triggers before using the connection provider. *** }

    procedure SetTriggerConnectionOpening(Event:TrtcBoolEvent); // we are opening a new connection
    procedure SetTriggerConnectionClosing(Event:TrtcEvent); // we are closing the connection

    procedure SetTriggerConnectFail(Event:TrtcEvent);
    procedure SetTriggerConnectLost(Event:TrtcEvent);
    procedure SetTriggerConnectError(Event:TrtcExceptEvent);
    end;

  TRtcBasicClientProvider = class(TRtcClientProvider)
  protected
    FState:TRtcConnectionState;

    FPeerAddr,
    FPeerPort,
    FLocalAddr,
    FLocalPort:string;

    procedure SetLocalAddr(const Value: string);
    procedure SetLocalPort(const Value: string);
    procedure SetPeerAddr(const Value: string);
    procedure SetPeerPort(const Value: string);
    procedure SetState(value:TRtcConnectionState);

    property State:TRtcConnectionState read GetState write SetState;
    property PeerAddr:string read GetPeerAddr write SetPeerAddr;
    property PeerPort:string read GetPeerPort write SetPeerPort;
    property LocalAddr:string read GetLocalAddr write SetLocalAddr;
    property LocalPort:string read GetLocalPort write SetLocalPort;

  public
    constructor Create; override;

    procedure Release; override;

    function GetParent:TRtcConnectionProvider; override;
    function GetState:TRtcConnectionState; override;

    function GetPeerAddr:string; override;
    function GetPeerPort:string; override;
    function GetLocalAddr:string; override;
    function GetLocalPort:string; override;

    procedure Check; override;
    end;

  TRtcBasicServerProvider = class(TRtcServerProvider)
  protected
    FState:TRtcConnectionState;

    FPeerAddr,
    FPeerPort,
    FLocalAddr,
    FLocalPort:string;

    procedure SetLocalAddr(const Value: string);
    procedure SetLocalPort(const Value: string);
    procedure SetPeerAddr(const Value: string);
    procedure SetPeerPort(const Value: string);
    procedure SetState(value:TRtcConnectionState);

    property State:TRtcConnectionState read GetState write SetState;
    property PeerAddr:string read GetPeerAddr write SetPeerAddr;
    property PeerPort:string read GetPeerPort write SetPeerPort;
    property LocalAddr:string read GetLocalAddr write SetLocalAddr;
    property LocalPort:string read GetLocalPort write SetLocalPort;

  public
    constructor Create; override;

    procedure Release; override;

    function GetState:TRtcConnectionState; override;

    function GetPeerAddr:string; override;
    function GetPeerPort:string; override;
    function GetLocalAddr:string; override;
    function GetLocalPort:string; override;

    procedure Check; override;
    end;

function GetNextConnID:TRtcConnID;

implementation

{ TRtcConnectionProvider }

constructor TRtcConnectionProvider.Create;
  begin
  inherited;
  FInSync:=False;
  FClosing:=False;
  FSilent:=False;
  FLost:=True; // if connection is closing and we didnt call disconnect, we lost it.

  FDataOut:=0;
  FDataIn:=0;

  TriggerBeforeCreate;
  end;

destructor TRtcConnectionProvider.Destroy;
  begin
  TriggerAfterDestroy;

  inherited;
  end;

function TRtcConnectionProvider.GetClosing: boolean;
  begin
  Enter;
  try
    Result:=FClosing;
  finally
    Leave;
    end;
  end;

function TRtcConnectionProvider.GetLost: boolean;
  begin
  Enter;
  try
    Result:=FLost;
  finally
    Leave;
    end;
  end;

function TRtcConnectionProvider.GetSilent: boolean;
  begin
  Enter;
  try
    Result:=FSilent;
  finally
    Leave;
    end;
  end;

procedure TRtcConnectionProvider.SetClosing(a: boolean);
  begin
  Enter;
  try
    FClosing:=a;
  finally
    Leave;
    end;
  end;

procedure TRtcConnectionProvider.SetLost(a: boolean);
  begin
  Enter;
  try
    FLost:=a;
  finally
    Leave;
    end;
  end;

procedure TRtcConnectionProvider.SetSilent(a: boolean);
  begin
  Enter;
  try
    FSilent:=a;
  finally
    Leave;
    end;
  end;

procedure TRtcConnectionProvider.Error(const text: string);
  begin
  if assigned(FError) then
    FError(text);
  end;

function TRtcConnectionProvider.GetAddr: string;
  begin
  Result:=FAddr;
  end;

function TRtcConnectionProvider.GetPort: string;
  begin
  Result:=FPort;
  end;

procedure TRtcConnectionProvider.SetAddr(val: string);
  begin
  FAddr:=val;
  end;

procedure TRtcConnectionProvider.SetError(Event: TRtcErrEvent);
  begin
  FError:=Event;
  end;

procedure TRtcConnectionProvider.SetPort(val: string);
  begin
  FPort:=val;
  end;

procedure TRtcConnectionProvider.SetTriggerAfterDestroy(Event: TrtcEvent);
  begin
  FOnAfterDestroy:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerBeforeCreate(Event: TrtcEvent);
  begin
  FOnBeforeCreate:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerConnect(Event: TrtcEvent);
  begin
  FOnConnect:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerConnecting(Event: TrtcEvent);
  begin
  FOnConnecting:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerDisconnecting(Event: TrtcEvent);
  begin
  FOnDisconnecting:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerDataReceived(Event: TrtcEvent);
  begin
  FOnDataReceived:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerDataLost(Event: TrtcEvent);
  begin
  FOnDataLost:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerDataSent(Event: TrtcEvent);
  begin
  FOnDataSent:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerDataOut(Event: TrtcEvent);
  begin
  FOnDataOut:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerDataIn(Event: TrtcEvent);
  begin
  FOnDataIn:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerLastWrite(Event: TrtcEvent);
  begin
  FOnLastWrite:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerDisconnect(Event: TrtcEvent);
  begin
  FOnDisconnect:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerException(Event: TrtcExceptEvent);
  begin
  FOnException:=Event;
  end;

procedure TRtcConnectionProvider.SetTriggerReadyToRelease(Event: TrtcEvent);
  begin
  FOnReadyToRelease:=Event;
  end;

procedure TRtcConnectionProvider.TriggerAfterDestroy;
  begin
  if assigned(FOnAfterDestroy) then
    FOnAfterDestroy;
  end;

procedure TRtcConnectionProvider.TriggerBeforeCreate;
  begin
  if assigned(FOnBeforeCreate) then
    FOnBeforeCreate;
  end;

procedure TRtcConnectionProvider.TriggerConnect;
  begin
  if assigned(FOnConnect) then
    FOnConnect;
  end;

procedure TRtcConnectionProvider.TriggerConnecting;
  begin
  if assigned(FOnConnecting) then
    FOnConnecting;
  end;

procedure TRtcConnectionProvider.TriggerDisconnecting;
  begin
  if assigned(FOnDisconnecting) then
    FOnDisconnecting;
  end;

procedure TRtcConnectionProvider.TriggerDataReceived;
  begin
  if assigned(FOnDataReceived) then
    FOnDataReceived;
  end;

procedure TRtcConnectionProvider.TriggerDataLost;
  begin
  if assigned(FOnDataLost) then
    FOnDataLost;
  end;

procedure TRtcConnectionProvider.TriggerDataSent;
  begin
  if assigned(FOnDataSent) then
    FOnDataSent;
  end;

procedure TRtcConnectionProvider.TriggerDataOut;
  begin
  if assigned(FOnDataOut) then
    FOnDataOut;
  end;

procedure TRtcConnectionProvider.TriggerDataIn;
  begin
  if assigned(FOnDataIn) then
    FOnDataIn;
  end;

procedure TRtcConnectionProvider.TriggerLastWrite;
  begin
  if assigned(FOnLastWrite) then
    FOnLastWrite;
  end;

procedure TRtcConnectionProvider.TriggerDisconnect;
  begin
  if assigned(FOnDisconnect) then
    FOnDisconnect;
  end;

procedure TRtcConnectionProvider.TriggerException(E: Exception);
  begin
  if assigned(FOnException) then
    FOnException(E);
  end;

procedure TRtcConnectionProvider.TriggerReadyToRelease;
  begin
  if assigned(FOnReadyToRelease) then
    FOnReadyToRelease;
  end;

function TRtcConnectionProvider.GetMultiThreaded: boolean;
  begin
  Result:=FMultiThreaded;
  end;

procedure TRtcConnectionProvider.SetMultiThreaded(val: boolean);
  begin
  FMultiThreaded:=val;
  end;

{ TRtcServerProvider }

procedure TRtcServerProvider.SetTriggerConnectionAccepting(Event: TrtcEvent);
  begin
  FOnConnectionAccepting:=Event;
  end;

procedure TRtcServerProvider.SetTriggerConnectionAccepted(Event: TrtcEvent);
  begin
  FOnConnectionAccepted:=Event;
  end;

procedure TRtcServerProvider.SetTriggerListenStart(Event: TrtcEvent);
  begin
  FOnListenStart:=Event;
  end;

procedure TRtcServerProvider.SetTriggerListenStop(Event: TrtcEvent);
  begin
  FOnListenStop:=Event;
  end;

procedure TRtcServerProvider.SetTriggerListenLost(Event: TrtcEvent);
  begin
  FOnListenLost:=Event;
  end;

procedure TRtcServerProvider.SetTriggerNewProvider(Event: TrtcProviderEvent);
  begin
  FOnNewProvider:=Event;
  end;

procedure TRtcServerProvider.TriggerConnectionAccepting;
  begin
  if assigned(FOnConnectionAccepting) then
    FOnConnectionAccepting;
  end;

procedure TRtcServerProvider.TriggerConnectionAccepted;
  begin
  if assigned(FOnConnectionAccepted) then
    FOnConnectionAccepted;
  end;

procedure TRtcServerProvider.TriggerListenStart;
  begin
  if assigned(FOnListenStart) then
    FOnListenStart;
  end;

procedure TRtcServerProvider.TriggerListenStop;
  begin
  if assigned(FOnListenStop) then
    FOnListenStop;
  end;

procedure TRtcServerProvider.TriggerListenLost;
  begin
  if assigned(FOnListenLost) then
    FOnListenLost;
  end;

procedure TRtcServerProvider.SetTriggerConnectionLost(Event: TrtcEvent);
  begin
  FOnConnectionLost:=Event;
  end;

procedure TRtcServerProvider.TriggerConnectionLost;
  begin
  if assigned(FOnConnectionLost) then
    FOnConnectionLost;
  end;

procedure TRtcServerProvider.TriggerListenError(E: Exception);
  begin
  if assigned(FOnListenError) then
    FOnListenError(E);
  end;

procedure TRtcServerProvider.SetTriggerListenError(Event: TrtcExceptEvent);
  begin
  FOnListenError:=Event;
  end;

procedure TRtcServerProvider.TriggerNewProvider(var Provider: TObject);
  begin
  if assigned(FOnNewProvider) then
    FOnNewProvider(Provider);
  end;

{ TRtcClientProvider }

procedure TRtcClientProvider.SetTriggerConnectError(Event: TrtcExceptEvent);
  begin
  FOnConnectError:=Event;
  end;

procedure TRtcClientProvider.SetTriggerConnectFail(Event: TrtcEvent);
  begin
  FOnConnectFail:=Event;
  end;

procedure TRtcClientProvider.SetTriggerConnectLost(Event: TrtcEvent);
  begin
  FOnConnectLost:=Event;
  end;

procedure TRtcClientProvider.SetTriggerConnectionOpening(Event: TrtcBoolEvent);
  begin
  FOnConnectionOpening:=Event;
  end;

procedure TRtcClientProvider.SetTriggerConnectionClosing(Event: TrtcEvent);
  begin
  FOnConnectionClosing:=Event;
  end;

procedure TRtcClientProvider.TriggerConnectionClosing;
  begin
  if assigned(FOnConnectionClosing) then
    FOnConnectionClosing;
  end;

procedure TRtcClientProvider.TriggerConnectError(E: Exception);
  begin
  if assigned(FOnConnectError) then
    FOnConnectError(E);
  end;

procedure TRtcClientProvider.TriggerConnectFail;
  begin
  if assigned(FOnConnectFail) then
    FOnConnectFail;
  end;

procedure TRtcClientProvider.TriggerConnectLost;
  begin
  if assigned(FOnConnectLost) then
    FOnConnectLost;
  end;

procedure TRtcClientProvider.TriggerConnectionOpening(Force: boolean);
  begin
  if assigned(FOnConnectionOpening) then
    FOnConnectionOpening(Force);
  end;

{ TRtcBasicClientProvider }

constructor TRtcBasicClientProvider.Create;
  begin
  inherited;
  FState:=conInactive;

  FLocalAddr:='0.0.0.0';
  FLocalPort:='';
  FPeerAddr:='0.0.0.0';
  FPeerPort:='';
  end;

procedure TRtcBasicClientProvider.Release;
  begin
  Destroy;
  end;

procedure TRtcBasicClientProvider.Check;
  begin
  // do nothing
  end;

function TRtcBasicClientProvider.GetParent: TRtcConnectionProvider;
  begin
  Result:=nil;
  end;

function TRtcBasicClientProvider.GetLocalAddr: string;
  begin
  Result:=FLocalAddr;
  end;

function TRtcBasicClientProvider.GetLocalPort: string;
  begin
  Result:=FLocalPort;
  end;

function TRtcBasicClientProvider.GetPeerAddr: string;
  begin
  Result:=FPeerAddr;
  end;

function TRtcBasicClientProvider.GetPeerPort: string;
  begin
  Result:=FPeerPort;
  end;

function TRtcBasicClientProvider.GetState: TRtcConnectionState;
  begin
  Enter;
  try
    Result:=FState;
  finally
    Leave;
    end;
  end;

procedure TRtcBasicClientProvider.SetLocalAddr(const Value: string);
  begin
  FLocalAddr:=Value;
  end;

procedure TRtcBasicClientProvider.SetLocalPort(const Value: string);
  begin
  FLocalPort:=Value;
  end;

procedure TRtcBasicClientProvider.SetPeerAddr(const Value: string);
  begin
  FPeerAddr:=Value;
  end;

procedure TRtcBasicClientProvider.SetPeerPort(const Value: string);
  begin
  FPeerPort:=Value;
  end;

procedure TRtcBasicClientProvider.SetState(value: TRtcConnectionState);
  begin
  Enter;
  try
    FState:=Value;
  finally
    Leave;
    end;
  end;

{ TRtcBasicServerProvider }

constructor TRtcBasicServerProvider.Create;
  begin
  inherited;
  FState:=conInactive;

  FLocalAddr:='0.0.0.0';
  FLocalPort:='';
  FPeerAddr:='0.0.0.0';
  FPeerPort:='';
  end;

procedure TRtcBasicServerProvider.Release;
  begin
  Destroy;
  end;

procedure TRtcBasicServerProvider.Check;
  begin
  // do nothing
  end;

function TRtcBasicServerProvider.GetLocalAddr: string;
  begin
  Result:=FLocalAddr;
  end;

function TRtcBasicServerProvider.GetLocalPort: string;
  begin
  Result:=FLocalPort;
  end;

function TRtcBasicServerProvider.GetPeerAddr: string;
  begin
  Result:=FPeerAddr;
  end;

function TRtcBasicServerProvider.GetPeerPort: string;
  begin
  Result:=FPeerPort;
  end;

function TRtcBasicServerProvider.GetState: TRtcConnectionState;
  begin
  Result:=FState;
  end;

procedure TRtcBasicServerProvider.SetLocalAddr(const Value: string);
  begin
  FLocalAddr:=Value;
  end;

procedure TRtcBasicServerProvider.SetLocalPort(const Value: string);
  begin
  FLocalPort:=Value;
  end;

procedure TRtcBasicServerProvider.SetPeerAddr(const Value: string);
  begin
  FPeerAddr:=Value;
  end;

procedure TRtcBasicServerProvider.SetPeerPort(const Value: string);
  begin
  FPeerPort:=Value;
  end;

procedure TRtcBasicServerProvider.SetState(value: TRtcConnectionState);
  begin
  FState:=Value;
  end;

const
  MaxConnID:longint=2000000000;

var
  GlobalConnID:longint=1;
  CS:TRtcCritSec;

function GetNextConnID:TRtcConnID;
  begin
  CS.Enter;
  try
    if GlobalConnID>=MaxConnID then
      GlobalConnID:=1
    else
      Inc(GlobalConnID);
    Result:=GlobalConnID;
  finally
    CS.Leave;
    end;
  end;

initialization
CS:=TRtcCritSec.Create;
finalization
Garbage(CS);
end.
