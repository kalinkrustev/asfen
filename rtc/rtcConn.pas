{
  @html(<b>)
  Connection
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  This unit defines the basic wrappers for all RTC Connection Components
  and is used in all your units which work with any RTC connection component.
  The most-used and referenced components throughout the RTC Component Suite
  will be @Link(TRtcConnection), @Link(TRtcServer) and @Link(TRtcClient).
}

unit rtcConn;

{$INCLUDE rtcDefs.inc}

interface

uses
{$IFDEF MEMCONTROL}
  rtcMemory,
{$ENDIF}

  Classes,
  SysUtils,

  rtcTrashcan,

  rtcLog,
  rtcInfo,
  rtcTimer,
  rtcSyncObjs,
  rtcThrPool,

  rtcConnProv,

{$IFDEF IDE_1}
  FileCtrl,
{$ENDIF}

{$IFDEF CLR}
  System.Threading;
{$ELSE}
  Windows;
{$ENDIF}

const
  // Version of the RTC SDK components
  RTCSDK_VERSION:string='2.75';

var
  // Write all Timet-out disconnects to a Log file?
  LOG_TIMEOUT_DISCONNECTS:boolean=False;

  { @name defines the Memory limit in Bytes.
    No new connections will be accepted by any server connection
    component in this application after this limit is reached.
    @html(<br><br>)

    Each Windows installation has its memory limitations.
    By setting the memory limit for the app here, you can
    throw away new connections in case your memory usage goes
    over the normal value.
    @html(<br>)
    By setting the limit too low, new connections will be abandoned for no reason.
    @html(<br>)
    By setting the limit too high, if you don't check your free memory anywhere
    else in the application and the application needs a lot of memory, you
    could end up with Out-of-memory or Access-Violation exceptions.
    @html(<br><br>)

    One 32-bit application can not address more then 4GB of RAM,
    so the highest value you should use here is 4.000.000.000 (under 4 GB),
    in case you have more RAM then you could ever need.
    @html(<br>)
    Standard value here is 500.000.000 (under 500 MB). }
  RTC_MEMORY_LIMIT:int64=500000000;

  { @name sets the Total connection count limit,
    limiting the number of total active connections (server+client).
    @html(<br><br>)

    Windows has a total connection limit of arround 32K active connections.
    @html(<br>)
    There is no actual need to limit the connections for the application,
    but if you plan on running several servers on one PC, this limit can
    be used for each application, so that applications can co-exist.
    Limiting applications connections will allow other applications to
    use TCP/IP for themselves, even if your server has a very high load.
    @html(<br><br>)

    By using the standard value of 32.000 for all connection limits,
    your app will use any resources it can get from Windows.
    @html(<br><br>)

    In conjunction with @Link(RTC_CLIENT_CONNECT_LIMIT) and @Link(RTC_SERVER_ACCEPT_LIMIT),
    you can precisely define how much Windows connection resources your application may use
    for which purpose. }
  {$IFNDEF Evaluate}
    RTC_CONNECTION_LIMIT:integer=32000;
  {$ELSE}
  const
    RTC_CONNECTION_LIMIT=3; // Limit evaluation version to 3 connections.
  var
  {$ENDIF}

  { @name sets the connection count limit for the client,
    limiting the number of connections that can be made using any TRtcClient
    connection component descendant. If more client connections are attempted,
    an exception will be raised from the 'Connect' method. }
  RTC_CLIENT_CONNECT_LIMIT:integer=32000;

  { @name sets the limit for accepting connections on the server
    by using any TRtcServer connection component descendant. If more connections try
    to come, they will be abandoned (disconnected without asking or notifying). }
  RTC_SERVER_ACCEPT_LIMIT:integer=32000;

  { RTC_WAIT_BEFORE_RECONNECT sets the default Wait period (here in miliseconds) for Reconnect.
    @html(<br>)
    This parameter will be only used if ReconnectOn.Wait<=0. }
  RTC_WAIT_BEFORE_RECONNECT:integer=100;

  { RTC_WAIT_BEFORE_RESTART sets the default Wait period (here in miliseconds) for Restart.
    @html(<br>)
    This parameter will only be used if RestartOn.Wait<=0. }
  RTC_WAIT_BEFORE_RESTART:integer=100;

type
  { All RTC components use at least one class declared in this unit.

    For Delphi to add this unit to the uses clause,
    all RTC components inherit from this class.
   @exclude }
  TRtcComponent = class(TRtc_Component);

  { @abstract(Used for exceptions raised by TRtcConnection)

    To stay on the safe side, you should catch ALL exceptions
    of type "Exception" in order to handle exceptions that may be
    raised from the RTC communication components. Some underlying components
    (for example: connection providers) may use their own exception types. }
  ERtcConnection = class(Exception);

{ --- TRtcConnection --- }

  TRtcConnection = class;

  // @exclude
  TRtcSimpleEvent = procedure(Sender:TObject) of object;

  { TRtcUserEvent is the event handler type for user-defined events. }
  TRtcUserEvent = procedure(Sender:TRtcConnection; Obj:TObject) of object;

  { TRtcUserDataEvent is the event handler type for user-defined data events. }
  TRtcUserDataEvent = procedure(Sender:TRtcConnection; Obj:TObject; Data:TRtcValue) of object;

  { TRtcNotifyEvent is the standard event handler type.
    Using one basic event type makes writing event handles much easier,
    since most methods are exchangeable. }
  TRtcNotifyEvent = procedure(Sender:TRtcConnection) of object;

  { TRtcErrorEvent is the error event handler type.
    All events that react on exceptions will use handlers of this type. }
  TRtcErrorEvent = procedure(Sender:TRtcConnection; E:Exception) of object;

  { TRtcFunctionCallEvent is the function call event handler type. }
  TRtcFunctionCallEvent=procedure(Sender:TRtcConnection;
                                  Param:TRtcFunctionInfo;
                                  Result:TRtcValue) of object;

  { TRtcResultEvent is the event handler to receive the result of a remote function call. }
  TRtcResultEvent=procedure(Sender:TRtcConnection;
                                  Data:TRtcValue;
                                  Result:TRtcValue) of object;

  { TRtcFunctionPrepareEvent is the event handler to prepare a remote function call. }
  TRtcFunctionPrepareEvent=procedure(Sender:TRtcConnection;
                                     Data:TRtcValue) of object;

  { Data event }
  TRtcDataEvent=TRtcFunctionPrepareEvent;

  { @abstract(Provides Timeout capability to all TRtcConnection components)

    TRtcTimeout is tightly coupled with TRtcConnection component.
    It encapsulates all Timeout parametes for active client
    and server connections, defining how long a connection can
    stay Idle after a specific task, before a Timeout period is
    triggered which closes idle connections (timed-out disconnect). }
  TRtcTimeout = class(TPersistent)
  private
    FTimer: TRtcTimer;

    FInterval:integer;

    FAfterConnecting:integer;
    FAfterConnect:integer;
    FAfterDataReceived:integer;
    FAfterDataLost:integer;
    FAfterDataSend:integer;
    FAfterDataOut:integer;
    FAfterDataIn:integer;
    FAfterDataSent:integer;

    FConn:TRtcConnection;
    FThr:TRtcThread;
    FJob:TRtcJob;

    procedure TriggerTimeout;

    procedure TimerSet;
    procedure TimerReset;

    property Conn:TRtcConnection read FConn write FConn;

  public
    { @exclude }
    constructor Create(Con:TRtcConnection);
    { @exclude }
    destructor Destroy; override;

    { Start the Timeout counter.
      @exclude }
    procedure Start(Multi_Threaded:boolean);
    { Stop the Timeout counter
      @exclude }
    procedure Stop;

    { Temporary Disable the Timeout counter.
      You can use this to disable the timeout while you are processing data. }
    procedure Disable;
    { Enable the Timeout counter and set its intervals to 'timeout' miliseconds.
      You can use this to set the next timeout period,
      overwriting the last value set by the connection component.
      This new interval will only have effect until the connection component changes it,
      which could also be immeriatelly after your call to Enable(). }
    procedure Enable(timeout:integer);

    { Connceting started, set timeout to AfterConnecting.
      @exclude }
    procedure Connecting;
    { Connect accomplished, set timeout to AfterConnect.
      @exclude }
    procedure Connect;

    { Data Sending started, set timeout to AfterDataSend.
      @exclude }
    procedure DataSending;
    { Data sent Out, set timeout to AfterDataOut.
      @exclude }
    procedure DataOut;
    { Data read In, set timeout to AfterDataIn.
      @exclude }
    procedure DataIn;
    { Data Sent, set timeout to AfterDataSent.
      @exclude }
    procedure DataSent;

    { New Data received, set timeout to AfterDataReceived.
      @exclude }
    procedure DataReceived;
    { Data Packet Lost (UDP only), set timeout to AfterDataLost.
      @exclude }
    procedure DataLost;

  published
    { Time (seconds) to wait for a Connect (Connect means that you can send and/or
      receive data through the connection) after Connecting was initiated (Connect message received,
      but the connection is not yet ready to be used).
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterConnecting:integer read FAfterConnecting write FAfterConnecting default 0;
    { Time (seconds) to wait for something to come through the connection or for user
      to start sending data through, after a Connect (Connect means that you can send and/or
      receive data through the connection).
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterConnect:integer read FAfterConnect write FAfterConnect default 0;
    { Time (seconds) to wait for the next event to trigger for this connection, after a
      data package was received (OnDataReceived event was triggered) .
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterDataReceived:integer read FAfterDataReceived write FAfterDataReceived default 0;
    { Time (seconds) to wait for the next event to trigger for this connection,
      after a sent package was lost (UDP Only timeout; OnDataLost event was triggered).
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterDataLost:integer read FAfterDataLost write FAfterDataLost default 0;
    { Time (seconds) to wait for the next event to trigger for this connection,
      after Data was prepared for sending (Write() or some other method to fill
      data into sending buffer was called).
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterDataSend:integer read FAfterDataSend write FAfterDataSend default 0;
    { Time (seconds) to wait for the next event to trigger for this connection,
      after a Data chunk was sent out (OnDataOut event was triggered).
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterDataOut:integer read FAfterDataOut write FAfterDataOut default 0;
    { Time (seconds) to wait for the next event to trigger for this connection,
      after a Data chunk was received in (OnDataIn event was triggered).
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterDataIn:integer read FAfterDataIn write FAfterDataIn default 0;
    { Time (seconds) to wait for the next event to trigger for this connection,
      after the sending buffer was completey emptied and all Data was Sent out
      (OnDataSent event was triggered).
      Set to zero (0) to reset the timeout counter (will start counting again, using
      the last set timeout interval), or to a negative value (-1) to
      disable the timeout on this event. }
    property AfterDataSent:integer read FAfterDataSent write FAfterDataSent default 0;
    end;

  { @abstract(Basic connection component wrapper)
    TRtcConnection is the Basic connection component wrapper (client & server).
    It publishes methods and handles all the tasks that are common to all
    connection components. All connection components are indirect descendants
    of TRtcConnection and therefor inherit all its methods and properties.

    @longcode(#
    VERY IMPORANT!!! If you create connection components at runtime:
      * NEVER! use Create(), Free or Destroy on ANY connection component.
      * Allways use the 'NEW' class function (implemented by all TRtcConnection
        components) to create the connection object and
      * ONLY use the 'RELEASE' method to free the connection object. #)

    @html(<b>)
    MULTITHREADED CLIENT/SERVER:
    @html(</b><br><br>)

    When @Link(TRtcConnection.MultiThreaded) property for this connection is True,
    your events will be called from within a Thread inside a Thread pool.
    If you decide to use your component(s) in a multithreaded environment,
    you have to keep in mind that all Data is being sent and received in
    the background, from threads belonging to the RTC Thread Pool.
    This implementation is thoroughly tested and works perfectly in
    any mulithreaded environment (tested on a dual-processor machine with
    thousands of clients accessing the server at the same time). But,
    to keep the code working for you, you will have to follow a few simple rules.

    @longcode(#
    1. Accessing the component in MultiThreaded mode:

      A) After a connection is initiated ('Connect' was called),
         your code which needs to access any methods or properties of
         that component, HAS TO BE synchronized with any background threads
         that might be using the component to process background requests.

         The easiest way to work with any TRtcConnection component
         in a multithreaded mode is to implement everything as events.
         There are enough event handlers declared, so that most of
         the things can be done this way.

         The mechanism integrated into TRtcConnection components makes sure that
         events triggered by the connection component DO NOT OVERLAP with callback
         functions and messages called by Windows. This means that you can safely
         use the component from ANY EVENT you define for that component.
         When the event is triggered, you are the owner of the component
         and nothing will access the component during your event execution.

      B) But, there will probably be situations where you do not want to
         wait for a connection event in order to do something with your connection.

         To be able to access the connection components from anywhere else
         than your event handlers (for example, from a Button click event),
         even when they are running in multithreaded mode, you will use
         TRtcConnection components ability to POST A JOB to itself.

         All actions that TRtcConnection components do, are defined by
         small jobs that have to be executed. When there are no jobs waiting
         for execution, there are no Threads or CPU resources used.

         Jobs are objects that contain DATA AND EXECUTION CODE. You can
         create jobs by extending the TRtcJob class and implementing
         your classes Run and Kill methods (defined as abstract by TRtcJob).
         After creating an instance of your TRtcJob class and initializing
         it with the data you need to execute the Run method, you can
         post your job object to your connection component by using
         the PostJob method (also implemented by TRtcConnection).

    2. Accessing the GUI from events in MultiThreaded mode:

      For your components events in which you need to access the GUI (Graphical
      User Interface: any visual component) when working in multithreaded mode,
      you SHOULD use the connection components inMainThread property to check if
      your event is currently being executed from the mail thread. If the result is
      TRUE, you may continue execution. If the result is False, you HAVE TO use
      the connection components 'Sync' method to call your event synchronized
      (that could also be the same event you are now inside). The event passed
      as a parameter to Sync will be called from the Main Thread, puting the
      background thread (from which you called Sync) in a waiting state,
      until the event execution is over.

      Here is one 'Access GUI from OnDataReceived event' example:

        procedure TMyForm.ServerOnDataReceived(Sender:TRtcConnection);
          begin
          // first, check if you are inside the Main Thread.
          if not Sender.inMainThread then
            begin
            // we are not in the main thread.
            // to be able to access the GUI,
            // we will call the event synchronized.
            Sender.Sync(ServerOnDataReceived);
            end
          else
            begin
            // We are in the main Thread.
            // We can safely access the GUI from here.
            end;
          end;

      This OnDataReceived event handler example wants to have GUI access
      throughout the event execution, so it checks if it is inside
      the Main Thread and calls itself synchronized.

      Naturaly, for events which do not need to access the GUI,
      you can simply implement the event, without ever checking the
      inMainThread property or using the Sync method. #) }
  TRtcConnection = class(TRtcComponent)
  private
    FInfo:TRtcInfo;

    FCS:TRtcCritSec;
    FReconnecting:boolean;
    FRecTimer:TRtcTimer;

    FMultiThread:boolean;

    FFree:boolean;
    FReadyToRelease:boolean;

    FActive:boolean;
    FClosing:boolean;

    FParent: TRtcConnection;

    FTimeout: TRtcTimeout;

    FPort:string;
    FAddr:string;
    FOverLimit:boolean;

    FMyEvent:TRtcNotifyEvent;

    FMyErrMsg:Exception;
    FMyErrEvent:TRtcErrorEvent;

    FMyUserMsg:TObject;
    FMyUserEvent:TRtcUserEvent;

    FMyUserData:TRtcValue;
    FMyUserDataEvent:TRtcUserDataEvent;

    FMyCustMsg:TObject;
    FMyCustEvent:TRtcCustomEvent;

    FMyCustData:TRtcValue;
    FMyCustDataEvent:TRtcCustomDataEvent;

    FMyFunc:TRtcFunctionInfo;
    FMyFuncResult:TRtcValue;
    FMyFuncEvent:TRtcFunctionCallEvent;
    FMyFuncData:TRtcValue;
    FMyResultEvent:TRtcResultEvent;
    FMyFuncPrepEvent:TRtcFunctionPrepareEvent;

    FInsideSync:boolean;
    FInsideEvent:integer;
    FConnectOp:boolean;
    FConnecting:boolean;
    FListening:boolean;
    FDisconnecting:boolean;
    FStopping:boolean;
    FReleasing:boolean;

    FConnectTriggered:boolean;
    FConnectingTriggered:boolean;

    FOnConnect:TRtcNotifyEvent;
    FOnConnecting:TRtcNotifyEvent;
    FOnDisconnect:TRtcNotifyEvent;
    FOnDisconnecting:TRtcNotifyEvent;

    FOnException:TRtcErrorEvent;

    FOnDataOut:TRtcNotifyEvent;
    FOnDataIn:TRtcNotifyEvent;
    FOnDataSent:TRtcNotifyEvent;
    FOnDataReceived:TRtcNotifyEvent;
    FOnReadyToSend:TRtcNotifyEvent;

    function GetParent:TRtcConnection;

    { Check Parent connection. All connections connected to a local listener
      have this listener as their Parent. }
    property Parent:TRtcConnection read GetParent;

    function GetState: TRtcConnectionState;

    function GetVersionSDK:string;

    procedure SetVersionSDK(const s:string);

  protected
    { @exclude }
    FDataOut:cardinal;
    { @exclude }
    FDataIn:cardinal;
    { @exclude }
    FReadCount:int64;
    { @exclude }
    FWriteCount:int64;

    { Connection provider component,
      set by TRtcConnection after calling the CreateProvider method.
      @exclude }
    Con:TRtcConnectionProvider;

    // @exclude
    procedure SetMultiThread(const Value: boolean); virtual; abstract;

    { Create a new connection provider and return it as a result.
      @exclude }
    function CreateProvider:TObject; virtual; abstract;

    { Release the connection provider
      @exclude }
    procedure ReleaseProvider; virtual;

    { @name is called by TRtcConnection before Connect or Listen,
      to copy parameters from the Connection component to
      the Connection Provider component. This is the method in which
      the connection component should initialize the connection provider's
      parameters with the ones set for the connection component.
      @exclude }
    procedure SetParams; virtual;

    { @name is called by TRtcConnection before Connect or Listen,
      to set connection provider's triggers to connection component's
      events, so that connection provider and connection component
      can work together as a team.
      @exclude }
    procedure SetTriggers; virtual;

    { @name is called by TRtcConnection after a connection was closed,
      to clear all connection provider's triggers, so that he connection
      provider doesn't cause an access violation in case the connection
      component is being released after disconnect, while the connection
      provider is still receiving old events or finalizing the connection.
      @exclude }
    procedure ClearTriggers; virtual;

    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event) method.
      @exclude }
    procedure CallMyEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Err) method.
      @exclude }
    procedure CallMyError;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Obj) method.
      @exclude }
    procedure CallMyUserEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Obj,Data) method.
      @exclude }
    procedure CallMyUserDataEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Obj) method.
      @exclude }
    procedure CallMyCustomEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Obj,Data) method.
      @exclude }
    procedure CallMyCustomDataEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Par,Res) method.
      @exclude }
    procedure CallMyFuncEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Res) method.
      @exclude }
    procedure CallMyResultEvent;
    { @name is used internally to implement the synchronization
      mechanism used by the Sync(Event,Data) method.
      @exclude }
    procedure CallMyFuncPrepEvent;

    { AfterDestroy event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerAfterDestroy; virtual;

    { BeforeCreate event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerBeforeCreate; virtual;

    { ReadyToRelease event,
      needs to be implemented by connection component,
      so it can be mapped to a connection provider's trigger.
      @exclude}
    procedure TriggerReadyToRelease; virtual; abstract;

    { Connecting event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerConnecting; virtual;
    // @exclude
    procedure CallConnecting; virtual;

    { Connect event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerConnect; virtual;
    // @exclude
    procedure CallConnect; virtual;

    { Disconnecting event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerDisconnecting; virtual;
    // @exclude
    procedure CallDisconnecting; virtual;

    { Disconnect event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerDisconnect; virtual;
    // @exclude
    procedure CallDisconnect; virtual;

    { DataOut event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerDataOut; virtual;
    // @exclude
    procedure CallDataOut; virtual;

    { DataIn event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerDataIn; virtual;
    // @exclude
    procedure CallDataIn; virtual;

    { LastWrite event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerLastWrite; virtual;
    // @exclude
    procedure CallLastWrite; virtual;

    { DataSent event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerDataSent; virtual;
    // @exclude
    procedure CallDataSent; virtual;
    // @exclude
    procedure CallReadyToSend; virtual;

    { DataReceived event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerDataReceived; virtual;
    // @exclude
    procedure CallDataReceived; virtual;

    { DataLost event (used by UDP only),
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerDataLost; virtual;
    // @exclude
    procedure CallDataLost; virtual;

    { Exception event,
      ready to be mapped to a connection provider's trigger.
      @exclude }
    procedure TriggerException(E:Exception); virtual;
    // @exclude
    procedure CallException(E:Exception); virtual;

    { Called to raise exceptions
      @exclude }
    procedure Error(Err:string);

    { Used by the Timeout component to trigger an internal disconnect,
      which will result in a reconnect, if reconnect parameters are set.
      @exclude }
    procedure InternalDisconnect; virtual;

    // @exclude
    function InsideEvent:boolean;

    { This event will be triggered every time this connection component's
      buffer is completely empty and the other side has just become ready to
      accept new data. It is good to wait for this event before starting
      to send data out, even though you can start sending data directly
      from the @Link(OnConnect) event.
      @html(<br><br>)

      By responding to this event and sending the data only after it was
      triggered, you avoid keeping the data in the send buffer, especially
      if the data you are sending is being read from a file on disk,
      which wouldn't occupy any memory until loaded. }
    property OnReadyToSend:TRtcNotifyEvent read FOnReadyToSend write FOnReadyToSend;
    { This event will be triggered every time a chunk of your data
      prepared for sending has just been sent out. To know
      exactly how much of it is on the way, use the @Link(DataOut) property.
      @html(<br><br>)

      NOTE: Even though data has been sent out, it doesn't mean that
      the other side already received it. It could also be that connection will
      break before this package reaches the other end. }
    property OnDataOut:TRtcNotifyEvent read FOnDataOut write FOnDataOut;
    { This event will be triggered every time a chunk of data
      has just come in (received). To know exactly how much of it
      has just arrived, use the @Link(DataIn) property. }
    property OnDataIn:TRtcNotifyEvent read FOnDataIn write FOnDataIn;
    { This event will be triggered when all data prepared for sending
      has been sent out and the sending buffer has become empty again.
      @html(<br><br>)

      When sending large data blocks, try slicing them in small chunks,
      sending a chunk at a time and responding to this event to prepare
      and send the next chunk. This will keep your memory needs low. }
    property OnDataSent:TRtcNotifyEvent read FOnDataSent write FOnDataSent;
    { When this event triggers, it means that the other side has sent you
      some data and you can now read it. Check the connection component's
      description to see which properties and methods you can use
      to read the data received. }
    property OnDataReceived:TRtcNotifyEvent read FOnDataReceived write FOnDataReceived;

  public

    { DO NOT CALL THIS CONSTRUCTOR DIRECTLY!!!
      Use the 'NEW' class function to create a new object!!! }
    constructor Create(AOwner:TComponent); override;

    { DO NOT CALL THIS DESTRUCTOR DIRECTLY!!!
      DO NOT CALL 'FREE' DIRECTLY!!!
      Use the @Link(Release) method to destroy this object. }
    destructor Destroy; override;

    { Is this Server connection running as an extension or a plug-in to another app (ISAPI or CGI)?
      This property is used internaly by some components, to determine
      how internal things (like delayed calls) need to be processed. }
    function isExtension:boolean; virtual;

    { Needs to be called by connection components to signalize
      that component is entering a state where a user-defined
      event will be called. This is to prevent users from
      destroying the connection component from inside an event
      handler and to handle the Disconnect, Connect, Relase and
      other methods that affect connection status.
      @exclude }
    procedure EnterEvent; virtual;

    { Needs to be called by connection components to signalize
      that component is leaving a state where a user-defined
      event was called. This is to prevent users from
      destroying the connection component from inside an event
      handler and to handle the Disconnect, Connect, Relase and
      other methods that affect connection status.
      @exclude }
    procedure LeaveEvent; virtual;

    { If you create connection components at runtime,
      allways use the 'NEW' class function to create the connection
      object and 'RELEASE' procedure to free the connection object.
      @html(<br><br>)

      To make the code you write compatible with all new versions of Delphi,
      and to make sure that object is not released while it is still being used
      by internal connection component mechanisms,
      ONLY USE this 'Release' procedure to free any connection component and
      release all the resources that are used by the component.
      @html(<br><br>)

      After calling Release, you should also set the variable to NIL,
      in which you have stored the connection component. }
    procedure Release; virtual;

    { All native RTC connection components know when they get
      disconnected from peer and also trigger the events required.
      @html(<br><br>)

      But, there may be some third-party connection components coming,
      which will not know when a connection was closed by peer
      (for example, different implementations of blocking TCP/IP).
      @html(<br><br>)

      The @name method is here to cover such cases and give the connection
      provider means to write a method for checking if the connection is
      still active. In such cases, after calling the Check procedure,
      if the connection is still active, nothing will happen. But if the
      connection is dead, connection will be closed by the connection provider. }
    procedure Check; virtual;

    { *1.) If this is a listening connection,
      stop listening and close all connections open to this listener (gracefuly).
      All events that need to be triggered will be triggered.
      @html(<br><br>)

      *2.) If this is a client connection (initiated by this or the other side),
      close the connection (gracefuly).
      All events that need to be triggered will be triggered. }
    procedure Disconnect; virtual;

    { Is the connection in the process of closing (disconnecting)?
      Do not try to send any more data out if this is @true. }
    function isClosing:boolean; virtual;

    { Get this connection's Peer Address (to which Address/IP are we connected?) }
    function PeerAddr:string; virtual;

    { Get this connection's Peer Port (to which Port are we connected?) }
    function PeerPort:string; virtual;

    { Get this connection's Local Address (on which local IP are we?) }
    function LocalAddr:string; virtual;

    { Get this connection's Local Port (on which local Port are we?) }
    function LocalPort:string; virtual;

    { Return the total number of active connections, counting all connections that
      are opened to this process by any TRtcConnection class (all servers and clients). }
    function TotalConnectionCount:integer; virtual;

    { Return the number of active connections,
      open by our Client connection components. }
    function TotalClientConnectionCount:integer; virtual;

    { Return the number of active connections,
      open to our Servers connection components. }
    function TotalServerConnectionCount:integer; virtual;

    { Post a job to connection thread's job queue.
      @param(Job
      - If using existing connection components (not extenting them),
      'Job' object has to be of @Link(TRtcJob) type,
      or else the 'Unknown Job' exception will be raised
      from within the thread, which will close the connection. }
    function PostJob(Job:TObject; HighPriority:boolean=False):boolean; virtual;

    { Pause all connection operations (works only if @Link(MultiThreaded) is True). }
    function Pause:boolean;

    { Resume connection operations (works only if @Link(MultiThreaded) is True). }
    function Resume:boolean;

    { Call this procedure to signal connection that you
      have just started processing received data.
      When you start processing data, it means that
      you have received at least one whole data package.
      @html(<br><br>)

      Knowing this, thread pooling can better utilize
      thread usage, allowing other connections to
      start reading/writing data. }
    procedure Processing;

    { Call the 'Event' synchronized (from the Mail Thread).
      You have to use this method to call events which want to access
      the GUI (Graphical User Interface: everything visual).
      To check wether your event is being executed from inside the
      Main Thread, use the connection's @Link(inMainThread) function.
      @param(Event - TRtcNotifyEvent method to be called synchronized, from the Main Thread.)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcNotifyEvent):boolean; overload; virtual;

    { Same as Sync(Event), with a difference that you
      can synchronize @Link(TRtcErrorEvent) events with this call
      and 'Err' will be used as the Exception parameter when
      calling the 'Event'.
      @param(Event - TRtcErrorEvent method to be called synchronized, from the MainThread)
      @param(Err - Exception object to be passed to the event)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcErrorEvent; Err:Exception):boolean; overload; virtual;

    { Same as Sync(Event), with a difference that you
      can synchronize @Link(TRtcFunctionCallEvent) events with this call
      and 'Param' and 'Res' will be passed as parameters when calling the 'Event'.
      @param(Event - TRtcFunctionCallEvent method to be called synchronized, from the MainThread)
      @param(Par - TRtcFunctionCall object, containing all function call information)
      @param(Res - TRtcValue object, ready to receive the result information)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcFunctionCallEvent; Par:TRtcFunctionInfo; Res:TRtcValue):boolean; overload; virtual;

    { Same as Sync(Event), with the difference that you
      can synchronize @Link(TRtcResultEvent) events with this call
      and 'Res' will be passed as the result parameter when calling the 'Event'.
      @param(Event - TRtcFunctionCallEvent method to be called synchronized, from the MainThread)
      @param(Data - TRtcValue object, containing the information which was sent to the server, which has produced the "Res")
      @param(Res - TRtcValue object, containing the result information)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcResultEvent; Data:TRtcValue; Res:TRtcValue):boolean; overload; virtual;

    { Same as Sync(Event), with the difference that you
      can synchronize @Link(TRtcFunctionPrepareEvent) events with this call
      and 'Data' will be passed as the Data parameter when calling the 'Event'.
      @param(Event - TRtcFunctionPrepareEvent method to be called synchronized, from the MainThread)
      @param(Data - TRtcValue object, which you should use to prepare the remote function call)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcFunctionPrepareEvent; Data:TRtcValue):boolean; overload; virtual;

    { Call the 'Event' synchronized (from the Main Thread).
      You have to use this method to call events which want to access
      the GUI (Graphical User Interface: everything visual).
      To check wether your event is being executed from inside the
      Main Thread, use the connection's @Link(inMainThread) function.
      @param(Event - TRtcUserEvent method to be called synchronized, from the Main Thread.)
      @param(Obj - Object to be passed as parameter to the event)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcUserEvent; Obj:TObject):boolean; overload; virtual;

    { Call the 'Event' synchronized (from the Main Thread).
      You have to use this method to call events which want to access
      the GUI (Graphical User Interface: everything visual).
      To check wether your event is being executed from inside the
      Main Thread, use the connection's @Link(inMainThread) function.
      @param(Event - TRtcUserDataEvent method to be called synchronized, from the Main Thread.)
      @param(Obj - Object to be passed as parameter to the event)
      @param(Data - Value object to be passed as parameter to the event)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcUserDataEvent; Obj:TObject; Data:TRtcValue):boolean; overload; virtual;

    { Call the 'Event' synchronized (from the Main Thread).
      You have to use this method to call events which want to access
      the GUI (Graphical User Interface: everything visual).
      To check wether your event is being executed from inside the
      Main Thread, use the connection's @Link(inMainThread) function.
      @param(Event - TRtcCustomEvent method to be called synchronized, from the Main Thread.)
      @param(Obj - Object to be passed as parameter to the event)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcCustomEvent; Obj:TObject):boolean; overload; virtual;

    { Call the 'Event' synchronized (from the Main Thread).
      You have to use this method to call events which want to access
      the GUI (Graphical User Interface: everything visual).
      To check wether your event is being executed from inside the
      Main Thread, use the connection's @Link(inMainThread) function.
      @param(Event - TRtcCustomDataEvent method to be called synchronized, from the Main Thread.)
      @param(Obj - Object to be passed as parameter to the event)
      @param(Data - Value object to be passed as parameter to the event)
      @return(@True if the call was succesful) }
    function Sync(Event:TRtcCustomDataEvent; Obj:TObject; Data:TRtcValue):boolean; overload; virtual;

    { You can use this method to check if you are inside the Main thread,
      from which drawing and writing to the GUI is allowed. If true,
      you can directly access the GUI. If false, you have to use the
      Sync() method to call this or some other event synchronized,
      from where you can use the drawing routines. }
    function inMainThread:boolean; virtual;

    { You can use this method to check if your connection object is currently
      inside its Thread. If true, jobs do not have to be posted,
      they can be called directly. To check if you are allowed to
      access GUI (drawing or writing to the screen or canvas),
      use the @Link(inMainThread) function. }
    function inThread:boolean; virtual;

    { Read function is used to read data received. It may ONLY be called
      from inside your @Link(OnDataReceived) event handler.
      Its behavior depends on the connection component that implements it. }
    function Read:string; virtual;
    { Write function is used to send data out.
      Its behavior depends on the connection component that implements it. }
    procedure Write(const s:string=''); virtual;

    { @name (Read-Only) can be used from the @Link(OnDataOut) event,
      to check how much data has now been sent out. The value of this
      property changes with every @Link(OnDataOut) event and should
      only be read from your @Link(OnDataOut) event handler. }
    property DataOut:cardinal read FDataOut;

    { @name (Read-Only) can be used from the @Link(OnDataIn) event,
      to check how much data has just arrived. The value of this
      property changes with every @Link(OnDataIn) event and should
      only be read from your @Link(OnDataIn) event handler. }
    property DataIn:cardinal read FDataIn;

    { Total number of bytes read from the other side (Peer/Remote) through this connection. }
    property ReadCount:int64 read FReadCount;
    { Total number of bytes sent to the other side (Peer/Remote) through this connection. }
    property WriteCount:int64 read FWriteCount;

    { This property will be set to TRUE if the connection was established,
      even though our connection limit was reached (or over the limit).
      @html(<br><br>)

      You can check the number of total conncetions open using the
      @Link(TotalConnectionCount) function. If you find this connection
      should stay open (even though the limit was reached when it connected),
      change this property to FALSE, to handle it like any other connection.
      @html(<br><br>)

      Note: This property is not being used by any TRtcConnection component
      or its descendants. It is only intended for your use. Even if
      the overLimit property is TRUE, it will not affect this connection's behavior. }
    property OverLimit:boolean read FOverLimit write FOverLimit;

    { Check current connection state.
      There is no real need to use this property to check your connection state,
      since events will be triggered when the state changes. }
    property State:TRtcConnectionState read GetState;

    { Additional connection component information.
      @html(<br><br>)

      You can use this property as you see fit.
      The only purpose of the Info property is to give
      the component user ways to store additional
      information about this conncetion inside the
      connection component itself, without having to
      create and maintain separate data structures. }
    property Info:TRtcInfo read FInfo;

  published

    { RealThinClient SDK Version (for information only) }
    property Version_SDK:string read GetVersionSDK write SetVersionSDK stored False;

    { Set the @name property to @True if you want your connection to use the
      Thread pooling mechanism, which is integrated into the RealThinClient
      library and can be used by all RTC connection components. To find out what
      more you need to keep in mind when working in multithreaded mode, check
      the @Link(TRtcConnection) description.
      @html(<br><br>)

      NOTE: This property is read only before you call 'Listen' for the server component
      or 'Connect' for the client component. Changing this property when a connection
      is open will have no effect on the component, until the next time you
      start the listener or open a new connection.
      @html(<br><br>)

      WARNING: To safely use your components in MultiThreaded mode, also check
      the Descriptions for @Link(TRtcConnection), @Link(Sync), @Link(inMainThread),
      @Link(inThread) and @Link(PostJob). }
    property MultiThreaded:boolean read FMultiThread write SetMultiThread default False;

    { You can set all timeout parameters for the client connection component or
      default timeout periods for all client connections of the server connection component
      using this property. Check @Link(TRtcTimeout) for more information. }
    property Timeout:TRtcTimeout read FTimeout write FTimeout;

    { Server Address to connect to for the Client,
      Listening address (Bind) for the Server (leave empty to listen on all network cards). }
    property ServerAddr:string read FAddr write FAddr;

    { Port on the Host to connect to for the Client,
      Listening Port (Bind) for the Server (never leave empty). }
    property ServerPort:string read FPort write FPort;

    { This event will be called when a new connection is waiting to be
      initialized. This is still BEFORE a connection is ready to read/send data. }
    property OnConnecting:TRtcNotifyEvent read FOnConnecting write FOnConnecting;

    { This event will be called when a succesful connection is being closed. }
    property OnDisconnecting:TRtcNotifyEvent read FOnDisconnecting write FOnDisconnecting;

    { This event will be called after a new connection
      has been succesfully established and is ready to be used.
      The event triggered will receive the client connection object as parameter.

      @html(<br><br>)
      * 1) If this was a client connection on which you called 'Connect' to
        attempt a new connection to a server, this event handler will
        be the first event to trigger after 'OnConnecting' and will
        receive the current connection object as parameter, only if the connection
        to the server was succesfull. If server was not available, 'OnConnectFail'
        event will be triggered instead. Also, in case of failure, the 'OnDisconnect'
        event will NOT be triggered.

      @html(<br><br>)
      * 2) If this was a listening connection (server), this event
        handler will receive a new connection object as parameter
        and be called directly after the 'OnClientConnect' and
        'OnConnecting' events. In case needed, you can change all the
        actual events to be triggered for every client connection from
        the 'OnClientConnect' event handler. But, it is not advisable
        to do so. It is better to have 1 event handler for all client connections
        that belong to a specific server and use the 'Sender' parameter to
        distinguish between different clients. }
    property OnConnect:TRtcNotifyEvent read FOnConnect write FOnConnect;

    { This event will be called when a prior working connection (for which you
      have allready received the @Link(OnConnect) event) just got disconnected,
      by either side:
      @html(<br>)
      1.) You closed it by calling @Link(Disconnect), or
      @html(<br>)
      2.) the other side closed it, or
      @html(<br>)
      3.) the connection simply colapsed. }
    property OnDisconnect:TRtcNotifyEvent read FOnDisconnect write FOnDisconnect;

    { This event is used to process exceptions that happen in the background,
      for example while sending data from buffer to the other side.
      @html(<br><br>)

      Normaly, no exceptions should happen in the background.
      If an exception happens in the background and you set the event handler
      for the OnException event, you would receive the exception object,
      so you can handle it if needed (for example, write it to a Log file).
      @html(<br><br>)

      If there is no handler for this event, background exceptions will be ignored. }
    property OnException:TRtcErrorEvent read FOnException write FOnException;

    end;

{ --- TRtcServer --- }

  { @Abstract(Restart Parameters for TRtcServer components)

    @name is tightly coupled with @Link(TRtcServer) component.
    It encapsulates the parameters used to define how a server listener
    should behave when there is a problem with the listening connection. }
  TRtcRestartParam = class(TPersistent)
  private
    FListenLost:boolean;
    FListenError:boolean;
    FWait:integer;

  public
    { Will be created by TRtcServer component.
      @exclude }
    constructor Create;
    { Will be destroyed by TRtcServer component-
      @exclude }
    destructor Destroy; override;

  published
    { Set ListenLost to TRUE if you want a restart listener on Listener Lost
        (listener closed by the system). }
    property ListenLost:boolean read FListenLost write FListenLost default False;

    { Set ListenError to TRUE if you want a restart listener on Listener Error
        (listener could not be started). }
    property ListenError:boolean read FListenError write FListenError default False;

    { Wait defines how long (in second) component should wait before it tries to restart the listener. }
    property Wait:integer read FWait write FWait default 0;
    end;

  { @Abstract(Basic Server-side connection component wrapper)

    @name publishes methods and handles all the tasks that are common to all
    Server connection components. All Server-side connection components
    are indirect descendants of TRtcServer and therefor inherit
    all its methods and properties. Since @name is also a direct descendant
    of @Link(TRtcConnection), it also inherits all methods and properties
    from @Link(TRtcConnection). For more information, check @Link(TRtcConnection).
    @html(<br><br>)

    All @Link(TRtcServer) descendant components automatically create and
    initialize a separate object (same type as the Server component) for
    every accepted Client connection. It also automatically releases the
    same object (which it created) when the client connection closes.
    This means that you will have a separate object for every client connection,
    without having to create, maintain or release it. All properties and events
    which you have defined for the TRtcServer component will be automatically
    copied into the new component, so that you do not have to do any special
    initialization to work with the connection object.
    @html(<br><br>)

    The components created automatically for each new accepted client connection
    will be of the same type as the main Server listener component. For example,
    if you created a @Link(TRtcTcpServer) component, set its properties and
    called @Link(TRtcServer.Listen), you will have access to a copy of that
    @Link(TRtcTcpServer) component, which will start its own new life from
    the second the connection is accepted by the underlying connection provider.
    @html(<br><br>)

    All events which you defined for the lisneter component, will automatically
    be copied to the new client connection component. Remember that this is
    NOT the @Link(TRtcClient) descendant component, it is a new @Link(TRtcServer)
    component, but its @Link(TRtcServer.isClient) function will return TRUE and
    @Link(TRtcServer.isListening) will return FALSE, which is a complete oposite
    to the results of same function calls on the listening server component.
    @html(<br><br>)

    ALL EVENTS for client connection components will only be called with
    the client connection components as parameter, rather than the Server
    listener component. In fact, only the 'OnListen...' events and the
    'OnRestart' event are being called with the Server listener component
    as the 'Sender' parameter. All the other events are only used by the
    connection components which are created for client connections by the
    server connection component. }
  TRtcServer = class(TRtcConnection)
  private
    FClientConnectTriggered:boolean;

    FRestartOn:TRtcRestartParam;

    FOnClientConnect:TRtcNotifyEvent;
    FOnClientDisconnect:TRtcNotifyEvent;

    FOnOpen:TRtcNotifyEvent;
    FOnClose:TRtcNotifyEvent;

    FOnListenLost:TRtcNotifyEvent;
    FOnListenError:TRtcErrorEvent;
    FOnRestart:TRtcNotifyEvent;

    procedure CallConnectionAccepting;

  protected

    // @exclude
    procedure SetMultiThread(const Value: boolean); override;

    { Called by @Link(TRtcConnection) to copy values from 'Dup' connection
     (Server connection in most cases) to this one.
      This way, the new client connection is being prepared for usage.
      @html(<br><br>)

      If you should write a new connection class inherited from @Link(TRtcConnection),
      you have to overwrite this method and implement it so that it copies
      all connection properties defined by your class from 'Dup' to this one,
      (will be a wnewly created component).
      @html(<br><br>)

      CopyFrom() is being called from connection providers after a new
      client connection has been created and needs to be prepared for usage.

      @exclude}
    procedure CopyFrom(Dup:TRtcServer); virtual;

    { This is ConnectionAccepting trigger,
      ready to be mapped to a connection provider.
      Will trigger an exception if connection may not be accepted (for any reason).
      @exclude }
    procedure TriggerConnectionAccepting; virtual;

    { This is ConnectionAccepted trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerConnectionAccepted; virtual;

    { This is ConnectionLost trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerConnectionLost; virtual;

    { This is a trigger to create a new connection provider,
      used by all server connection providers to get a fresh
      conncetion component, which they can initialize and use.
      @exclude }
    procedure TriggerNewProvider(var Provider:TObject); virtual;

    { This is a ReadyToRelease trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerReadyToRelease; override;

    { This is a ClientConnect trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerClientConnect; virtual;
    // @exclude
    procedure CallClientConnect; virtual;

    { This is a ClientDisconnect trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerClientDisconnect; virtual;
    // @exclude
    procedure CallClientDisconnect; virtual;

    { This is a ListenStart trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerListenStart; virtual;
    // @exclude
    procedure CallListenStart; virtual;

    { This is a ListenStop trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerListenStop; virtual;
    // @exclude
    procedure CallListenStop; virtual;

    { This is a ListenLost trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerListenLost; virtual;
    // @exclude
    procedure CallListenLost; virtual;

    { This is a ListenError trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerListenError(E:Exception); virtual;
    // @exclude
    procedure CallListenError(E:Exception); virtual;

    { This is a Restart trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerRestart; virtual;
    // @exclude
    procedure CallRestart; virtual;

    { @name sets all parameters for the connection provider,
      introduced by the @Link(TRtcServer) connection component.
      It calls 'inherited' to set inherited properties.
      @exclude }
    procedure SetParams; override;
    { @name sets all triggers for the connection provider,
      introduced by the @Link(TRtcServer) connection component.
      It calls 'inherited' to set inherited triggers.
      @exclude }
    procedure SetTriggers; override;
    { @name clears all triggers for the connection provider,
      introduced by the @Link(TRtcServer) connection component.
      It calls 'inherited' to clear inherited triggers.
      @exclude }
    procedure ClearTriggers; override;

    { @name is a trigger, implementing a Timer event to
      restart the listener connection.
      @exclude }
    procedure OnRestart_Timer; virtual;

  public
    { When creating TRtcServer components at runtime,
      never use Create(). Always use the NEW class function,
      implemented by all RTC connection components. }
    constructor Create(AOwner: TComponent); override;
    { To destroy connection component created at runtime,
      never use Free or Destroy. Always use the @Link(TRtcConnection.Release) method. }
    destructor Destroy; override;

    { Creates a copy of this connection component,
      with all properties and events set,
      including a new connection provider,
      with preset properties and events. }
    function copyOf:TRtcServer; virtual;

    { Start listening for incomming connections on specified @Link(TRtcConnection.ServerPort),
      bound to a specified @Link(TRtcConnection.ServerAddr). }
    procedure Listen; virtual;

    { Stop Listening for incomming connections
      and close all open connections as soon as possible. }
    procedure StopListen; virtual;

    { Calling this method will create a timer and call Listen
      after 'WaitSeconds' seconds. If you call this method with
      0 as parameter, default value for RTC_WAIT_BEFORE_RECONNECT will be used. }
    procedure Restart(WaitSeconds:integer=0); virtual;

    { Returns TRUE if this is a listener (server) and it is listening. }
    function isListening:boolean; virtual;

    { Returns TRUE if this is a client connection,
      FALSE if it is the listener. }
    function isClient:boolean; virtual;

  published

    { By setting this property to True, you do not have to do anything
      special to initiate a renewed Listen after specific events occure.
      For more information, check @Link(TRtcRestartParam). }
    property RestartOn:TRtcRestartParam read FRestartOn write FRestartOn;

    { This will be the first event to trigger when a new client establishes
      a new connection with your listening server component.
      Same as the 'OnConnect' event, this event will allways be called
      from the client connection's thread and with a client connection as parameter.
      @html(<br><br>)

      If all your client connections will use the same events handlers for all
      client connections coming through this server component (this is the
      prefered method to use RTC connection components), you don't need to
      implement this event handler.
      @html(<br><br>)

      But, in case you want to use different events for different clients (maybe
      depending on the IP address or some other information you get immediatelly
      after connect and before any real data has been sent or received, you can
      use this event handler to set new events, which will be used instead of
      the default event handlers, which you defined for your TRtcServer component. }
    property OnClientConnect:TRtcNotifyEvent read FOnClientConnect write FOnClientConnect;

    { This event will be the last event to trigger when a working connection that
      came from a client to your listening server gets lost.
      Same as the 'OnDisconnect' event, this event will allways be called
      from the client connection's thread and with a client connection as parameter.
      @html(<br><br>)

      This event's primary purpuse is to give you a chance to free the
      resources you occupied for this connection. There will be no more
      events triggered for this connection after 'OnClientDisconnect'.
      @html(<br><br>)

      This will be the last event where you can still use the Client connection.
      After this event, Client connection and its component will be destroyed.
      @longcode(#
       NOTE: OnClientDisconnect allways comes in pair with prior OnClientConnect.
             OnClientDisconnect will not be triggered for connections
             where OnClientConnect was not triggered before.
             #) }
    property OnClientDisconnect:TRtcNotifyEvent read FOnClientDisconnect write FOnClientDisconnect;

    { Listener was started. This means that we're waiting on a
      specific port for incomming connections. This event will be
      triggered if the call to "Listen" was succesfull. }
    property OnListenStart:TRtcNotifyEvent read FOnOpen write FOnOpen;

    { Listener stopped. This means that we're no longer waiting on
      a specific port for incomming connections. This event will be
      triggered if the call to "Disconnect" was succesfull.
      @longcode(#
       NOTE: OnListenStop allways comes in pair with proior OnListenStart.
             OnListenStop will not be triggered for server components
             where OnListenStart was not triggered before.
             #) }
    property OnListenStop:TRtcNotifyEvent read FOnClose write FOnClose;

    { This event will be called when our listener closes,
      without us calling the 'Disconnect' method. }
    property OnListenLost:TRtcNotifyEvent read FOnListenLost write FOnListenLost;

    { This event will be called when listener can not start because of an error. }
    property OnListenError:TRtcErrorEvent read FOnListenError write FOnListenError;

    { This event will be triggered just before starting a new listening
      attempt, when a listener had to be restarted (listener killed by OS). }
    property OnRestart:TRtcNotifyEvent read FOnRestart write FOnRestart;

    end;

{ --- TRtcClient --- }

  { @Abstract(Reconnect parameters for TRtcClient components)

    @name is tightly coupled with @Link(TRtcClient) component.
    It encapsulates the parameters used to define how a client
    connection should act when a connection can not be
    established, or when connection gets lost. }
  TRtcReconnectParam = class(TPersistent)
  private
    FConnectError:boolean;
    FConnectFail:boolean;
    FConnectLost:boolean;
    FWait:integer;

  public
    { @class will be created by TRtcClient component.
      @exclude }
    constructor Create;
    { @class will be destroyed by TRtcClient component.
      @exclude }
    destructor Destroy; override;

  published
    { Set ConnectError to TRUE if you want automatic reconnect on Connect Errr. }
    property ConnectError:boolean read FConnectError write FConnectError default False;

    { Set ConnectLost to TRUE if you want automatic reconnect on Connect Lost. }
    property ConnectLost:boolean read FConnectLost write FConnectLost default False;

    { Set ConnectFail to TRUE if you want automatic reconnect on Connect Fail. }
    property ConnectFail:boolean read FConnectFail write FConnectFail default False;

    { Wait defines how long (in seconds) component should wait before it tries to reconnect. }
    property Wait:integer read FWait write FWait default 0;
    end;

  { @Abstract(Basic Client-side connection component wrapper)

    @name publishes methods and handles all the tasks that are common to all
    Client connection components. All Client-side connection components
    are indirect descendants of TRtcClient and therefor inherit
    all its methods and properties. Since @name is also a direct descendant
    of @Link(TRtcConnection), it also inherits all methods and properties
    from @Link(TRtcConnection). For more information, check @Link(TRtcConnection). }
  TRtcClient = class(TRtcConnection)
  private
    FReconnectOn:TRtcReconnectParam;

    FOnConnectError:TRtcErrorEvent;
    FOnConnectFail:TRtcNotifyEvent;
    FOnConnectLost:TRtcNotifyEvent;

    FOnReconnect:TRtcNotifyEvent;

  protected
    // @exclude
    function isConnectionRequired:boolean; virtual;

    // @exclude
    procedure SetMultiThread(const Value: boolean); override;

    { This is a ConnectionOpening trigger,
      ready to be mapped to a connection provider.
      Will trigger an exception if connection may not be opened (for any reason).
      @exclude }
    procedure TriggerConnectionOpening(Force:boolean=False); virtual;

    { This is a ConnectionClosing trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerConnectionClosing; virtual;

    { This is a ReadyToRelease trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerReadyToRelease; override;

    { This is a ConnectFail trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerConnectFail; virtual;
    // @exclude
    procedure CallConnectFail; virtual;

    { This is a ConnectLost trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerConnectLost; virtual;
    // @exclude
    procedure CallConnectLost; virtual;

    { This is a ConnectError trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerConnectError(E:Exception); virtual;
    // @exclude
    procedure CallConnectError(E:Exception); virtual;

    { This is a Reconnect trigger,
      ready to be mapped to a connection provider.
      @exclude }
    procedure TriggerReconnect; virtual;
    // @exclude
    procedure CallReconnect; virtual;

    { @name sets all parameters for the connection provider,
      introduced by the @Link(TRtcClient) connection component.
      It calls 'inherited' to set inherited properties.
      @exclude }
    procedure SetParams; override;
    { @name sets all triggers for the connection provider,
      introduced by the @Link(TRtcClient) connection component.
      It calls 'inherited' to set inherited triggers.
      @exclude }
    procedure SetTriggers; override;
    { @name clears all triggers for the connection provider,
      introduced by the @Link(TRtcClient) connection component.
      It calls 'inherited' to clear inherited triggers.
      @exclude }
    procedure ClearTriggers; override;

    { @name is a trigger, implementing a Timer event to
      reconnect the client connection.
      @exclude }
    procedure OnReconnect_Timer; virtual;

    { Returns TRUE if this connection is connected or currently connecting. }
    function isConnecting:boolean; virtual;

  public

    { When creating TRtcClient components at runtime,
      never use Create(). Always use the NEW class function,
      implemented by all RTC connection components. }
    constructor Create(AOwner: TComponent); override;
    { To destroy connection component created at runtime,
      never use Free or Destroy. Always use the @Link(TRtcConnection.Release) method. }
    destructor Destroy; override;

    { Connect to @Link(TRtcConnection.ServerAddr) on @Link(TRtcConnection.ServerPort) (a Listener has to be waiting there).
      Before you start sending data out, wait for the @Link(TRtcConnection.OnConnect) event.
      If there will be something to read from the connection, the @Link(TRtcConnection.OnDataReceived)
      event will be triggered. Do not call @Link(TRtcConnection.Read) from anywhere else that
      the @Link(TRtcConnection.OnDataReceived) event.
      @html(<br><br>)

      You can use the 'Force' parameter to force a connection attempt
      even if the connection count limit has been reached.
      @html(<br><br>)

      If the host address can not be resolved, port is invalid or something
      else is wrong with connecion data, and you have defined the 'OnConnectError' event,
      then the 'OnConnectError' event will be triggered, but there will be NO EXCEPTIONS
      raised from the 'Connect' procedure.
      @html(<br><br>)

      To initiate a new conncetion attempt after a connect error, you
      can simply call 'Connect' or 'Reconnect' from this event.
      By calling 'Reconnect', you can define how long you want to wait before
      a new connection attempt will be made, while a call to 'Connect' uses
      a default time period set up with a RTC_WAIT_BEFORE_RECONNECT constant.
      @html(<br><br>)

      If this connect attempt raises an exception, it means that either
      you have reached your connection limit or something bad has gone wrong
      and you should handle that exception as internal error.
      @html(<br><br>)

      When all is clear for the connection attempt, 'Connect' procedure will
      return immediatelly, and real connecting is done in the background. Do not
      try to send or read data immediatelly after the call to 'Connect', you will
      end up with exceptions (socket not connected or something like that).

      @longcode(#
      If there were no exceptions comming from 'Connect',
      there are three possibilities (only one can happen):

      A) If connection attempt results in an ERROR:
           "OnConnectError" event will be triggered.
           If 'ReconnectOnError' is TRUE,
             reconnect will be triggered.

      B) Connection attempt FAILS:
           "OnConnectFail" event will be triggered.
           If 'ReconnectOnFail' is TRUE,
             reconnect will be triggered.

      C) Connection attempt SUCCEEDS:
           first "OnConnecting" event is triggered,
           then "OnConnect" events will be triggered.

           When a working connection closes,
             "OnDisconnect" and "OnDisconnecting" events will be triggered.

             After that, if the connection was NOT closed on purpose,
               "OnConnectLost" event will be triggered.
               If "ReconnectOnLost" is TRUE, reconnect will be triggered. #)

      Connection attempts that end in ERROR or FAIL,
      only trigger the @Link(OnConnectError) or @Link(OnConnectFail) event.
      They DO NOT trigger the @Link(TRtcConnection.OnDisconnect)
      or @Link(TRtcConnection.OnDisconnecting) events. }
    procedure Connect(Force:boolean=False); virtual;

    { Returns TRUE if this connection is connected
      (ready to send/receive data). }
    function isConnected:boolean; virtual;

    { Calling this method will create a timer and call the OnReconnect event
      after 'WaitSeconds' seconds. If you call this method with
      0 as parameter, default value for RTC_WAIT_BEFORE_RECONNECT will be used. }
    procedure Reconnect(WaitSeconds:integer=0); virtual;

  published

    { By setting properties in @name to True, you do not have to do anything
      special to initiate a Reconnect after specific events occur. }
    property ReconnectOn:TRtcReconnectParam read FReconnectOn write FReconnectOn;

    { This event will be called when it is certain that a connection
      attempt using "Connect" from this object has failed.
      @html(<br><br>)

      Other implementations might first call "OnConnect" then call "OnDisconnect"
      when a connection attempt has failed, but I think it's better to keep those
      situations (connection open then closed, or connection failed) separated,
      to know what exactly happened. If "OnConnectFail" event is triggered,
      you can be sure that either something is wrong with your Addr/Port,
      or the Host (server) you're connecting to is not listening,
      or there is a firewall between the two of you that blocks the connection. }
    property OnConnectFail:TRtcNotifyEvent read FOnConnectFail write FOnConnectFail;

    { This event will be called when a connection that we have opened closes,
      without us calling the 'Disconnect' method. }
    property OnConnectLost:TRtcNotifyEvent read FOnConnectLost write FOnConnectLost;

    { This event will be called when you try to open a new connection
      and an error occured inside the 'Connect' method, saying that
      either the connection limit was reached, host address can not
      be resolved, TCP/IP is not working or anything else that can
      happen before the actual connecting process starts.
      @html(<br><br>)

      This method will be called with the connection object and the
      exception that occured as parameters.
      @html(<br><br>)

      To try reconnecting, you can change the connection parameters (if needed)
      and call the 'Reconnect' method from this event.
      You cal also @Link(TRtcConnection.Release) the connection object from here,
      in case youcreated it at runtime. }
    property OnConnectError:TRtcErrorEvent read FOnConnectError write FOnConnectError;

    { This event will be triggered just before making a new connection
      attempt after a connect error, failed connection or lost connection. }
    property OnReconnect:TRtcNotifyEvent read FOnReconnect write FOnReconnect;
    end;

  { @abstract(RTC Client Request info) }
  TRtcClientRequest = class(TRtcRequest)
  private
    FOnBegin:TRtcNotifyEvent;
    FOnData:TRtcNotifyEvent;
    FOnDone:TRtcNotifyEvent;
    FOnAbort:TRtcNotifyEvent;
    FOnReject:TRtcNotifyEvent;

    FStarted:boolean;
    FActive:boolean;
    FComplete:boolean;

    FDataOut: int64;

    FSkipped: boolean;
    FReposting: boolean;
    FAutoLength: boolean;
    FReposted: integer;

  public
    // @exclude
    constructor Create; override;
    // @exclude
    destructor Destroy; override;

    { @exclude }
    procedure Clear; override;
    { @exclude }
    procedure Init; virtual;

    // Skip this request, response irrelevant
    procedure Skip; virtual;
    // Repost this request
    procedure Repost; virtual;

    // Content Length was calculated using prepared data.
    property AutoLength:boolean read FAutoLength write FAutoLength;

    // Request started (started sending data out)
    property Started:boolean read FStarted write FStarted default false;
    // Request active (request in the process of receiving or sending)
    property Active:boolean read FActive write FActive default false;
    // Request complete (last call to DataSent using this request)
    property Complete:boolean read FComplete write FComplete default false;

    // Is this request in the process of reposting itself?
    property Reposting:boolean read FReposting write FReposting default false;
    // Number of times this request has been reposted
    property Reposted:integer read FReposted write FReposted default 0;
    // Has this request been skipped?
    property Skipped:boolean read FSkipped write FSkipped default false;
    { Request information ... }

    { Number of Data bytes (Content only) that has been sent to the Client.
      This value is increased only AFTER the data has been sent out,
      which does not have to be immediatelly after you call "Write",
      because data could be buffered throughout your Event code. @html(<br>)
      When sending files in smaller blocks, where you use the OnDataSent
      event to continue sending the file after the last block was sent,
      you can use ContentOut to check how much of data you have already sent. }
    property ContentOut:int64 read FDataOut write FDataOut;
    // Synonim for ContentOut
    property DataOut:int64 read FDataOut write FDataOut;

    // Event called before the first RequestStart event
    property OnBegin:TRtcNotifyEvent read FOnBegin write FOnBegin;
    // Event called after last ResponseData event
    property OnData:TRtcNotifyEvent read FOnData write FOnData;
    // Event called after last ResponseDone event
    property OnDone:TRtcNotifyEvent read FOnDone write FOnDone;

    // Event called after Request has been terminated by peer disconnect and repost was not requested
    property OnAbort:TRtcNotifyEvent read FOnAbort write FOnAbort;
    // Event called after Response has been Rejected by Request handler (wrong server response)
    property OnReject:TRtcNotifyEvent read FOnReject write FOnReject;
    end;

  { @abstract(RTC Client Response info) }
  TRtcClientResponse = class(TRtcResponse)
  private
    FStarted:boolean;
    FRejected: boolean;
    FReceiving:boolean;
    FDone:boolean;

    FDataIn:int64;

  public
    // @exclude
    constructor Create; override;
    // @exclude
    destructor Destroy; override;

    { @exclude }
    procedure Clear; override;

    // Reject this response, it is inacceptable (wrong server or server version?)
    procedure Reject; virtual;

    // Started to receive Response for this request (first call to DataReceived for this request)
    property Started:boolean read FStarted write FStarted;
    // Response is being received from the Server
    property Receiving:boolean read FReceiving write FReceiving;
    // Response has been received (complete) from the Server
    property Done:boolean read FDone write FDone;

    // Has this request been rejected?
    property Rejected:boolean read FRejected write FRejected default false;

    { Number of Data bytes (Content only) that has been received from the Server.
      This value is increased immediatelly when data is received, even before you "Read" it. }
    property ContentIn:int64 read FDataIn write FDataIn;
    // Synonim for ContentIn
    property DataIn:int64 read FDataIn write FDataIn;
    end;

  { @abstract(RTC Server Request info) }
  TRtcServerRequest = class(TRtcRequest)
  private
    FStarted:boolean;
    FActive:boolean;
    FComplete:boolean;

    FDataIn:int64;

    FAccepted: boolean;

  public
    // @exclude
    constructor Create; override;
    // @exclude
    destructor Destroy; override;

    { @exclude }
    procedure Clear; override;

    { Request information ... }

    // Request started (first call to DataReceived using this request)
    property Started:boolean read FStarted write FStarted default false;
    // Request active (request in the process of receiving or sending)
    property Active:boolean read FActive write FActive default false;
    // Request complete (last call to DataReceived using this request)
    property Complete:boolean read FComplete write FComplete default false;

    // Has the Request been accepted by a request handler?
    property Accepted:boolean read FAccepted write FAccepted;

    // Number of bytes allready read from Request Data (Content)
    property ContentIn:int64 read FDataIn write FDataIn;
    // Synonim for ContentIn
    property DataIn:int64 read FDataIn write FDataIn;
    end;

  { @abstract(RTC Server Response info) }
  TRtcServerResponse = class(TRtcResponse)
  private
    FStarted:boolean;
    FSending:boolean;
    FSent:boolean;
    FDone:boolean;

    FSendContent: boolean;

    FDataOut:int64;

  public
    // @exclude
    constructor Create; override;
    // @exclude
    destructor Destroy; override;

    { @exclude }
    procedure Clear; override;

    // Set Status code and Text
    procedure Status(Code:integer; const Text:string); overload;
    // Set Status code and Text by using a string like "200 OK"
    procedure Status(CodeText:string); overload;

    // Just Started sending Response to the Client
    property Started:boolean read FStarted write FStarted default false;
    // Response is being sent to the Client
    property Sending:boolean read FSending write FSending;
    // Response has been sent (complete) to the Client
    property Done:boolean read FDone write FDone;
    // Response has been prepared for sending (complete) to the Client
    property Sent:boolean read FSent write FSent;

    { Send content after header? (default = True)
      Set this to False if you only want to send a header out and
      will not be sending any content after it, even though you
      set ContentLength to a value larger than 0.
      This property is used (set to False) for request methods like "HEAD",
      where the server has to respond only with a header,
      but content-length and all other header attributes have to be
      the same as if you were sending the whole document. }
    property SendContent:boolean read FSendContent write FSendContent;

    // Number of Data bytes (Content only) that has been sent to the Client
    property ContentOut:int64 read FDataOut write FDataOut;
    // Synonim for ContentOut
    property DataOut:int64 read FDataOut write FDataOut;
    end;

implementation

type
  TRtcBaseServerClass = class of TRtcServer;

var
  ServerConnection_Cnt:integer=0;
  ClientConnection_Cnt:integer=0;
  TotalConnection_Cnt:integer=0;
  Conn_CS:TRtcCritSec;

function ClientConnection_Open(Force:boolean=False):boolean;
  {$IFDEF MEMCONTROL}
  var
    mem:longint;
  {$ENDIF}
  begin
  {$IFDEF MEMCONTROL}
  mem:=Get_MemoryInUse;
  {$ENDIF}
  Result:=False;

  Conn_CS.Enter;
  try
    {$IFDEF MEMCONTROL}
    if mem>=RTC_MEMORY_LIMIT then
      begin
      if Force then
        begin
        Inc(ClientConnection_Cnt);
        Inc(TotalConnection_Cnt);
        end
      else
        raise EClientLimitReached.Create('Memory limit reached.')
      end
    else
    {$ENDIF}
    if TotalConnection_Cnt>=RTC_CONNECTION_LIMIT then
      begin
      {$IFDEF Evaluate}
        raise EClientLimitReached.Create('Evaluation version: CONNECTION LIMIT (3) REACHED.');
      {$ELSE}
      if Force then
        begin
        Inc(ClientConnection_Cnt);
        Inc(TotalConnection_Cnt);
        end
      else
        raise EClientLimitReached.Create('Total connection limit reached.')
      {$ENDIF}
      end
    else if ClientConnection_Cnt>=RTC_CLIENT_CONNECT_LIMIT then
      begin
      if Force then
        begin
        Inc(ClientConnection_Cnt);
        Inc(TotalConnection_Cnt);
        end
      else
        raise EClientLimitReached.Create('Client connection limit reached.');
      end
    else
      begin
      Result:=True;
      Inc(ClientConnection_Cnt);
      Inc(TotalConnection_Cnt);
      end;
  finally
    Conn_CS.Leave;
    end;
  end;

procedure ClientConnection_Close;
  begin
  Conn_CS.Enter;
  try
    if (ClientConnection_Cnt<=0) or
       (TotalConnection_Cnt<=0) then
      raise ERtcConnection.Create('Error calling Connection_Close. No open connections registered!');
    Dec(ClientConnection_Cnt);
    Dec(TotalConnection_Cnt);
  finally
    Conn_CS.Leave;
    end;
  end;

function ClientConnection_Count:integer;
  begin
  Conn_CS.Enter;
  try
    Result:=ClientConnection_Cnt;
  finally
    Conn_CS.Leave;
    end;
  end;

procedure ServerConnection_CanAccept;
  {$IFDEF MEMCONTROL}
  var
    mem:longint;
  {$ENDIF}
  begin
  {$IFDEF MEMCONTROL}
  mem:=Get_MemoryInUse;
  {$ENDIF}

  Conn_CS.Enter;
  try
    {$IFDEF MEMCONTROL}
    if mem>=RTC_MEMORY_LIMIT then
      raise EClientLimitReached.Create('Memory limit reached.')
    else
    {$ENDIF}
    if TotalConnection_Cnt>=RTC_CONNECTION_LIMIT then
    {$IFDEF Evaluate}
      raise EClientLimitReached.Create('Evaluation Version: CONNECTION LIMIT (3) REACHED.')
    {$ELSE}
      raise EClientLimitReached.Create('Total connection limit reached.')
    {$ENDIF}
    else if ServerConnection_Cnt>=RTC_SERVER_ACCEPT_LIMIT then
      raise EClientLimitReached.Create('Server connection limit reached.');
  finally
    Conn_CS.Leave;
    end;
  end;

procedure ServerConnection_Accept;
  begin
  Conn_CS.Enter;
  try
    Inc(ServerConnection_Cnt);
    Inc(TotalConnection_Cnt);
  finally
    Conn_CS.Leave;
    end;
  end;

procedure ServerConnection_Close;
  begin
  Conn_CS.Enter;
  try
    if (ServerConnection_Cnt<=0) or
       (TotalConnection_Cnt<=0) then
      raise ERtcConnection.Create('Error calling Connection_Close. No open connections registered!');
    Dec(ServerConnection_Cnt);
    Dec(TotalConnection_Cnt);
  finally
    Conn_CS.Leave;
    end;
  end;

function ServerConnection_Count:integer;
  begin
  Conn_CS.Enter;
  try
    Result:=ServerConnection_Cnt;
  finally
    Conn_CS.Leave;
    end;
  end;

function TotalConnection_Count:integer;
  begin
  Conn_CS.Enter;
  try
    Result:=TotalConnection_Cnt;
  finally
    Conn_CS.Leave;
    end;
  end;

{ TRtcConnection }

procedure TRtcServer.CopyFrom(Dup: TRtcServer);
  begin
  ServerAddr:=Dup.ServerAddr;
  ServerPort:=Dup.ServerPort;
  MultiThreaded:=Dup.MultiThreaded;

  Timeout.AfterConnecting:=Dup.Timeout.AfterConnecting;
  Timeout.AfterConnect:=Dup.Timeout.AfterConnect;

  Timeout.AfterDataReceived:=Dup.Timeout.AfterDataReceived;
  Timeout.AfterDataLost:=Dup.Timeout.AfterDataLost;

  Timeout.AfterDataSend:=Dup.Timeout.AfterDataSend;
  Timeout.AfterDataOut:=Dup.Timeout.AfterDataOut;
  Timeout.AfterDataIn:=Dup.Timeout.AfterDataIn;
  Timeout.AfterDataSent:=Dup.Timeout.AfterDataSent;

  OnConnecting:=Dup.OnConnecting;
  OnDisconnecting:=Dup.OnDisconnecting;
  OnConnect:=Dup.OnConnect;
  OnDisconnect:=Dup.OnDisconnect;
  OnClientConnect:=Dup.OnClientConnect;
  OnClientDisconnect:=Dup.OnClientDisconnect;

  OnListenStart:=Dup.OnListenStart;
  OnListenStop:=Dup.OnListenStop;

  OnException:=Dup.OnException;

  OnDataOut:=Dup.OnDataOut;
  OnDataIn:=Dup.OnDataIn;
  OnDataSent:=Dup.OnDataSent;
  OnDataReceived:=Dup.OnDataReceived;
  OnReadyToSend:=Dup.OnReadyToSend;
  end;

function TRtcConnection.GetVersionSDK: string;
  begin
  Result:=RTCSDK_VERSION;
  end;

procedure TRtcConnection.SetVersionSDK(const s: string);
  begin
  // This setter method has to exist,
  // or Delphi would not show the property in the IDE
  end;

constructor TRtcConnection.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FCS:=TRtcCritSec.Create;
  FReconnecting:=False;
  FRecTimer:=nil;

  FInfo:=TRtcInfo.Create;

  FMultiThread:=False;

  FFree:=False;
  FReadyToRelease:=False;

  Con:=nil;

  FActive:=False;
  FParent:=nil;

  FConnectTriggered:=False;
  FConnectingTriggered:=False;

  FTimeout:=TRtcTimeout.Create(self);

  FPort:='';
  FAddr:='';
  FOverLimit:=False;

  FInsideSync:=False;
  FInsideEvent:=0;
  FReleasing:=False;
  FDisconnecting:=False;
  FStopping:=False;
  FConnecting:=False;
  FListening:=False;
  FConnectOp:=False;

  FOnConnect:=nil;
  FOnConnecting:=nil;
  FOnDisconnect:=nil;
  FOnDisconnecting:=nil;

  FOnException:=nil;
  end;

destructor TRtcConnection.Destroy;
  begin
  FTimeout.Free;

  FInfo.Free;

  FCS.Enter;
  try
    if FReconnecting then
      begin
      FReconnecting:=False;
      TRtcTimer.Stop(FRecTimer);
      FRecTimer:=nil;
      end;
  finally
    FCS.Leave;
    end;

  FCS.Free;

  FPort:='';
  FAddr:='';

  inherited;
  end;

constructor TRtcServer.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FClientConnectTriggered:=False;

  FRestartOn:=TRtcRestartParam.Create;

  FOnClientConnect:=nil;
  FOnClientDisconnect:=nil;

  FOnOpen:=nil;
  FOnClose:=nil;
  end;

destructor TRtcServer.Destroy;
  begin
  ReleaseProvider;

  FRestartOn.Free;

  inherited;
  end;

constructor TRtcClient.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FReconnectOn:=TRtcReconnectParam.Create;
  end;

destructor TRtcClient.Destroy;
  begin
  ReleaseProvider;

  FReconnectOn.Free;

  inherited;
  end;

// Counting active connections

procedure TRtcServer.CallConnectionAccepting;
  begin
  ServerConnection_CanAccept;
  end;

procedure TRtcServer.TriggerConnectionAccepting;
  begin
  CallConnectionAccepting;
  end;

procedure TRtcServer.TriggerConnectionAccepted;
  begin
  if not FActive then
    begin
    ServerConnection_Accept;

    Timeout.Start(MultiThreaded);
    Timeout.Connecting;

    FActive:=True;
    TriggerClientConnect;
    end;
  end;

procedure TRtcServer.TriggerConnectionLost;
  begin
  if FActive then
    begin
    ServerConnection_Close;

    FActive:=False;
    TriggerClientDisconnect;

    Timeout.Stop;
    end;
  end;

procedure TRtcClient.TriggerConnectionOpening(Force: boolean);
  begin
  if not FActive then
    begin
    if not ClientConnection_Open(Force) then
      begin
      OverLimit:=True;
      end;

    Timeout.Start(MultiThreaded);
    Timeout.Connecting;

    FActive:=True;
    end;
  end;

procedure TRtcClient.TriggerConnectionClosing;
  begin
  if FActive then
    begin
    ClientConnection_Close;
    FActive:=False;

    Timeout.Stop;
    end;
  end;

function TRtcConnection.Read: string;
  begin
  if assigned(Con) and (State<>conInactive) then
    begin
    Result:=Con.Read;
    // FReadCount:=FReadCount+length(Result);
    if length(Result)>0 then
      Timeout.DataReceived;
    end
  else
    Result:='';
  end;

procedure TRtcConnection.Write(const s: string);
  begin
  if assigned(Con) and (State<>conInactive) then
    begin
    Timeout.DataSending;
    Con.Write(s);
    end;
  end;

procedure TRtcConnection.TriggerDataReceived;
  begin
  EnterEvent;
  try
    CallDataReceived;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcConnection.TriggerDataSent;
  begin
  if FWriteCount>0 then
    Timeout.DataSent;
  EnterEvent;
  try
    if FWriteCount>0 then
      CallDataSent;
    if not isClosing then
      CallReadyToSend;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcConnection.TriggerDataOut;
  begin
  Timeout.DataOut;
  EnterEvent;
  try
    FDataOut:=Con.DataOut;
    FWriteCount:=FWriteCount+FDataOut;

    CallDataOut;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcConnection.TriggerDataIn;
  begin
  Timeout.DataIn;
  EnterEvent;
  try
    FDataIn:=Con.DataIn;
    FReadCount:=FReadCount+FDataIn;

    CallDataIn;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcConnection.TriggerLastWrite;
  begin
  EnterEvent;
  try
    CallLastWrite;
  finally
    LeaveEvent;
    end;
  end;

// Raising errors for the Connection provider ...

procedure TRtcConnection.Error(Err:string);
  begin
  raise ERtcConnection.Create(Err);
  end;

// Methods used by the component user ...

function TRtcConnection.TotalConnectionCount: integer;
  begin
  Result:=TotalConnection_Count;
  end;

function TRtcConnection.TotalClientConnectionCount: integer;
  begin
  Result:=ClientConnection_Count;
  end;

function TRtcConnection.TotalServerConnectionCount: integer;
  begin
  Result:=ServerConnection_Count;
  end;

function TRtcConnection.inThread: boolean;
  begin
  if assigned(Con) then
    begin
    if Con.inThread then
      Result:=True
    else if inMainThread and FInsideSync then
      Result:=True
    else
      Result:=False;
    end
  else if not MultiThreaded then
    Result:=True
  else
    Result:=False;
  end;

function TRtcConnection.inMainThread: boolean;
  begin
  if not MultiThreaded then
    Result:=True
  else if assigned(Con) then
    Result:=Con.inMainThread
  else
    begin
  {$IFDEF CLR}
    Result := System.Threading.Thread.CurrentThread = MainThr;
  {$ELSE}
    Result := GetCurrentThreadId=MainThrID;
  {$ENDIF}
    end;
  end;

{ TRtcReconJob --- > }

type
  TRtcReconJob=class(TRtcJob)
  public
    Conn:TRtcClient;
    procedure Run(Thr:TRtcThread); override;
    procedure Kill; override;
    end;

procedure TRtcReconJob.Kill;
  begin
  Free;
  end;

procedure TRtcReconJob.Run(Thr:TRtcThread);
  begin
  if assigned(Conn) then
    Conn.OnReconnect_Timer;
  Free;
  end;

{ <--- TRtcReconJob }

procedure TRtcClient.Reconnect(WaitSeconds: integer=0);
  begin
  if InsideEvent then // called from inside event
    begin
    FReleasing:=False;
    FConnecting:=True;
    end
  else
    begin
    FCS.Enter;
    try
      if FReconnecting then Exit;
      FReconnecting:=True;
      FRecTimer:=TRtcTimer.Create(MultiThreaded);
    finally
      FCS.Leave;
      end;

    {$IFDEF FPC}
    if WaitSeconds<=0 then
      TRtcTimer.Enable(FRecTimer,RTC_WAIT_BEFORE_RECONNECT, @OnReconnect_Timer, True, True)
    else
      TRtcTimer.Enable(FRecTimer,WaitSeconds*1000, @OnReconnect_Timer, True, True);
    {$ELSE}
    if WaitSeconds<=0 then
      TRtcTimer.Enable(FRecTimer,RTC_WAIT_BEFORE_RECONNECT, OnReconnect_Timer, True, True)
    else
      TRtcTimer.Enable(FRecTimer,WaitSeconds*1000, OnReconnect_Timer, True, True);
    {$ENDIF}
    end;
  end;

{ TRtcRestartJob --- > }

type
  TRtcRestartJob=class(TRtcJob)
  public
    Conn:TRtcServer;
    procedure Run(Thr:TRtcThread); override;
    procedure Kill; override;
    end;

procedure TRtcRestartJob.Kill;
  begin
  Free;
  end;

procedure TRtcRestartJob.Run(Thr:TRtcThread);
  begin
  if assigned(Conn) then
    Conn.OnRestart_Timer;
  Free;
  end;

{ < --- TRtcRestartJob }

procedure TRtcServer.Restart(WaitSeconds: integer=0);
  begin
  if InsideEvent then
    begin
    FReleasing:=False;
    FListening:=True;
    end
  else
    begin
    FCS.Enter;
    try
      if FReconnecting then Exit;
      FReconnecting:=True;
      FRecTimer:=TRtcTimer.Create(MultiThreaded);
    finally
      FCS.Leave;
      end;
      
    {$IFDEF FPC}
    if WaitSeconds<=0 then
      TRtcTimer.Enable(FRecTimer,RTC_WAIT_BEFORE_RESTART, @OnRestart_Timer, True, True)
    else
      TRtcTimer.Enable(FRecTimer,WaitSeconds*1000, @OnRestart_Timer, True, True);
    {$ELSE}
    if WaitSeconds<=0 then
      TRtcTimer.Enable(FRecTimer,RTC_WAIT_BEFORE_RESTART, OnRestart_Timer, True, True)
    else
      TRtcTimer.Enable(FRecTimer,WaitSeconds*1000, OnRestart_Timer, True, True);
    {$ENDIF}
    end;
  end;

procedure TRtcClient.OnReconnect_Timer;
  var
    job:TRtcReconJob;
  begin
  if not inThread and assigned(Con) and (Con.GetThread<>nil) then
    begin
    job:=TRtcReconJob.Create;
    job.Conn:=self;
    if not PostJob(job) then
      job.Kill
    else
      Exit;
    end;

  FCS.Enter;
  try
    FReconnecting:=False;
    FRecTimer:=nil;
  finally
    FCS.Leave;
    end;

  try
    TriggerReconnect;
  except
    on E:Exception do
      try
        TriggerException(E);
      except
        //
      end;
    end;
  end;

procedure TRtcServer.OnRestart_Timer;
  var
    job:TRtcRestartJob;
  begin
  if not inThread and assigned(con) and (Con.GetThread<>nil) then
    begin
    job:=TRtcRestartJob.Create;
    job.Conn:=self;
    if not PostJob(job) then
      job.Kill
    else
      Exit;
    end;

  FCS.Enter;
  try
    FReconnecting:=False;
    FRecTimer:=nil;
  finally
    FCS.Leave;
    end;

  try
    TriggerRestart;
  except
    on E:Exception do
      begin
      try
        TriggerException(E);
      except
        //
        end;
      end;
    end;
  end;

procedure TRtcConnection.Release;
  var
    MyCon:TRtcConnectionProvider;
  begin
  if InsideEvent then
    FReleasing:=True
  else
    begin
    if assigned(Con) then
      begin
      MyCon:=Con;
      Con:=nil;
      MyCon.Release;
      end
    else
      Free;
    end;
  end;

procedure TRtcClient.Connect(Force: boolean);
  begin
  if assigned(Con) and MultiThreaded then
    begin
    SetParams;
    TRtcClientProvider(Con).Connect(Force);
    end
  else if InsideEvent then
    begin
    FReleasing:=False;
    FConnecting:=True;
    end
  else if assigned(Con) then
    begin
    SetParams;
    TRtcClientProvider(Con).Connect(Force);
    end
  else
    begin
    CreateProvider;
    SetParams;
    TRtcClientProvider(Con).Connect(Force);
    end;
  end;

procedure TRtcConnection.Disconnect;
  begin
  if assigned(Con) and MultiThreaded then
    Con.Disconnect
  else if InsideEvent then
    begin
    FReleasing:=False;
    FClosing:=True;
    FDisconnecting:=True;
    end
  else if assigned(Con) then
    Con.Disconnect;
  end;

procedure TRtcConnection.InternalDisconnect;
  begin
  if assigned(Con) then
    Con.InternalDisconnect;
  end;

procedure TRtcServer.StopListen;
  begin
  if assigned(Con) and MultiThreaded then
    begin
    if isClient then
      Parent.Disconnect
    else
      Con.Disconnect;
    end
  else if InsideEvent then
    begin
    if isClient then
      Parent.Disconnect
    else
      begin
      FReleasing:=False;
      FClosing:=True;
      FDisconnecting:=True;
      FStopping:=True;
      end
    end
  else if isClient then
    Parent.Disconnect
  else if assigned(Con) then
    Con.Disconnect;
  end;

procedure TRtcServer.Listen;
  begin
  if InsideEvent then
    begin
    FReleasing:=False;
    FListening:=True;
    end
  else if isClient then
    TRtcServer(Parent).Listen
  else if assigned(Con) then
    begin
    SetParams;
    TRtcServerProvider(Con).Listen;
    end
  else if State=conInactive then
    begin
    CreateProvider;
    SetParams;
    TRtcServerProvider(Con).Listen;
    end
  else
    Restart;
  end;

function TRtcConnection.isClosing:boolean;
  begin
  if FReleasing or FClosing or FDisconnecting then
    Result:=True
  else if not assigned(Con) then
    Result:=True
  else if State in [conInactive,conClosing] then
    Result:=True
  else
    Result:=False;
  end;

procedure TRtcConnection.Check;
  begin
  if assigned(Con) then
    Con.Check;
  end;

function TRtcServer.isClient:boolean;
  begin
  Result:=assigned(Parent);
  end;

function TRtcServer.isListening: boolean;
  begin
  if assigned(Con) then
    Result:=not isClient and
            not (FClosing or FDisconnecting) and
            ( (State=conListening) or
              (State=conActive) )
  else
    Result:=False;
  end;

function TRtcClient.isConnected: boolean;
  begin
  if assigned(Con) then
    Result:=(State=conActive) and not (FClosing or FDisconnecting)
  else
    Result:=False;
  end;

function TRtcClient.isConnecting: boolean;
  begin
  FCS.Enter;
  try
    if assigned(Con) then
      Result:=(FReconnecting or FConnecting) and not FReleasing
    else
      Result:=False;
  finally
    FCS.Leave;
    end;
  end;

function TRtcConnection.LocalAddr: string;
  begin
  if assigned(Con) then
    Result:=Con.GetLocalAddr
  else
    Result:=FAddr;
  end;

function TRtcConnection.LocalPort: string;
  begin
  if assigned(Con) then
    Result:=Con.GetLocalPort
  else
    Result:=FPort;
  end;

function TRtcConnection.PeerAddr: string;
  begin
  if assigned(Con) then
    Result:=Con.GetPeerAddr
  else
    Result:='';
  end;

function TRtcConnection.PeerPort: string;
  begin
  if assigned(Con) then
    Result:=Con.GetPeerPort
  else
    Result:='';
  end;

function TRtcConnection.PostJob(Job:TObject; HighPriority:boolean=False):boolean;
  begin
  if assigned(Job) then
    begin
    if assigned(Con) then
      Result:=Con.PostJob(Job,HighPriority)
    else if not MultiThreaded and (Job is TRtcJob) {and inMainThread} then
      begin
      TRtcJob(Job).Run(nil);
      Result:=True;
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcConnection.Pause:boolean;
  begin
  if assigned(Con) then
    Result:=Con.Pause
  else
    Result:=False;
  end;

function TRtcConnection.Resume:boolean;
  begin
  if assigned(Con) then
    Result:=Con.Resume
  else
    Result:=False;
  end;

procedure TRtcConnection.Processing;
  begin
  if assigned(Con) then
    begin
    Timeout.Disable; // Disable the timeout while processing requests.
    Con.Processing;
    end;
  end;

function TRtcConnection.Sync(Event: TRtcNotifyEvent):boolean;
  begin
  if assigned(Event) then
    begin
    if inMainThread then
      begin
      Event(self);
      Result:=True;
      end
    else if inThread then // call the event synchronized
      begin
      FMyEvent:=Event;
      {$IFDEF FPC}
      Result:=Con.SyncEvent(@CallMyEvent);
      {$ELSE}
      Result:=Con.SyncEvent(CallMyEvent);
      {$ENDIF}
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcConnection.Sync(Event: TRtcErrorEvent; Err:Exception):boolean;
  begin
  if assigned(Event) then
    begin
    if inMainThread then
      begin
      Event(self,Err);
      Result:=True;
      end
    else if inThread then // call the event synchronized
      begin
      FMyErrEvent:=Event;
      FMyErrMsg:=Err;
      {$IFDEF FPC}
      Result:=Con.SyncEvent(@CallMyError);
      {$ELSE}
      Result:=Con.SyncEvent(CallMyError);
      {$ENDIF}
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcConnection.Sync(Event: TRtcUserEvent; Obj:TObject):boolean;
  begin
  if assigned(Event) then
    begin
    if inMainThread then
      begin
      Event(self,Obj);
      Result:=True;
      end
    else if inThread then // call the event synchronized
      begin
      FMyUserEvent:=Event;
      FMyUserMsg:=Obj;
      {$IFDEF FPC}
      Result:=Con.SyncEvent(@CallMyUserEvent);
      {$ELSE}
      Result:=Con.SyncEvent(CallMyUserEvent);
      {$ENDIF}
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcConnection.Sync(Event: TRtcUserDataEvent; Obj:TObject; Data:TRtcValue):boolean;
  begin
  if assigned(Event) then
    begin
    if inMainThread then
      begin
      Event(self,Obj,Data);
      Result:=True;
      end
    else if inThread then // call the event synchronized
      begin
      FMyUserDataEvent:=Event;
      FMyUserData:=Data;
      FMyUserMsg:=Obj;
      {$IFDEF FPC}
      Result:=Con.SyncEvent(@CallMyUserDataEvent);
      {$ELSE}
      Result:=Con.SyncEvent(CallMyUserDataEvent);
      {$ENDIF}
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcConnection.Sync(Event: TRtcCustomEvent; Obj:TObject):boolean;
  begin
  if assigned(Event) then
    begin
    if inMainThread then
      begin
      Event(self,Obj);
      Result:=True;
      end
    else if inThread then // call the event synchronized
      begin
      FMyCustEvent:=Event;
      FMyCustMsg:=Obj;
      {$IFDEF FPC}
      Result:=Con.SyncEvent(@CallMyCustomEvent);
      {$ELSE}
      Result:=Con.SyncEvent(CallMyCustomEvent);
      {$ENDIF}
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcConnection.Sync(Event: TRtcCustomDataEvent; Obj:TObject; Data:TRtcValue):boolean;
  begin
  if assigned(Event) then
    begin
    if inMainThread then
      begin
      Event(self,Obj,Data);
      Result:=True;
      end
    else if inThread then // call the event synchronized
      begin
      FMyCustDataEvent:=Event;
      FMyCustData:=Data;
      FMyCustMsg:=Obj;
      {$IFDEF FPC}
      Result:=Con.SyncEvent(@CallMyCustomDataEvent);
      {$ELSE}
      Result:=Con.SyncEvent(CallMyCustomDataEvent);
      {$ENDIF}
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcConnection.Sync(Event: TRtcFunctionCallEvent; Par:TRtcFunctionInfo; Res:TRtcValue):boolean;
  begin
  if assigned(Event) then
    begin
    if inMainThread then
      begin
      Event(self,Par,Res);
      Result:=True;
      end
    else if inThread then // call the event synchronized
      begin
      FMyFuncEvent:=Event;
      FMyFunc:=Par;
      FMyFuncResult:=Res;
      {$IFDEF FPC}
      Result:=Con.SyncEvent(@CallMyFuncEvent);
      {$ELSE}
      Result:=Con.SyncEvent(CallMyFuncEvent);
      {$ENDIF}
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcConnection.Sync(Event: TRtcResultEvent; Data:TRtcValue; Res:TRtcValue):boolean;
  begin
  if assigned(Event) then
    begin
    if inMainThread then
      begin
      Event(self,Data,Res);
      Result:=True;
      end
    else if inThread then // call the event synchronized
      begin
      FMyResultEvent:=Event;
      FMyFuncResult:=Res;
      FMyFuncData:=Data;
      {$IFDEF FPC}
      Result:=Con.SyncEvent(@CallMyResultEvent);
      {$ELSE}
      Result:=Con.SyncEvent(CallMyResultEvent);
      {$ENDIF}
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

function TRtcConnection.Sync(Event: TRtcFunctionPrepareEvent; Data:TRtcValue):boolean;
  begin
  if assigned(Event) then
    begin
    if inMainThread then
      begin
      Event(self,Data);
      Result:=True;
      end
    else if inThread then // call the event synchronized
      begin
      FMyFuncPrepEvent:=Event;
      FMyFuncData:=Data;
      {$IFDEF FPC}
      Result:=Con.SyncEvent(@CallMyFuncPrepEvent);
      {$ELSE}
      Result:=Con.SyncEvent(CallMyFuncPrepEvent);
      {$ENDIF}
      end
    else
      Result:=False;
    end
  else
    Result:=False;
  end;

// Internal methods and properties

function TRtcConnection.GetParent: TRtcConnection;
  begin
  Result:=FParent;
  end;

procedure TRtcConnection.CallMyEvent;
  begin
  FInsideSync:=True;
  try
    FMyEvent(self);
  finally
    FInsideSync:=False;
    end;
  end;

procedure TRtcConnection.CallMyError;
  begin
  FInsideSync:=True;
  try
    FMyErrEvent(self,FMyErrMsg);
  finally
    FInsideSync:=False;
    end;
  end;

procedure TRtcConnection.CallMyUserEvent;
  begin
  FInsideSync:=True;
  try
    FMyUserEvent(self,FMyUserMsg);
  finally
    FInsideSync:=False;
    end;
  end;

procedure TRtcConnection.CallMyUserDataEvent;
  begin
  FInsideSync:=True;
  try
    FMyUserDataEvent(self,FMyUserMsg,FMyUserData);
  finally
    FInsideSync:=False;
    end;
  end;

procedure TRtcConnection.CallMyCustomEvent;
  begin
  FInsideSync:=True;
  try
    FMyCustEvent(self,FMyCustMsg);
  finally
    FInsideSync:=False;
    end;
  end;

procedure TRtcConnection.CallMyCustomDataEvent;
  begin
  FInsideSync:=True;
  try
    FMyCustDataEvent(self,FMyCustMsg,FMyCustData);
  finally
    FInsideSync:=False;
    end;
  end;

procedure TRtcConnection.CallMyFuncEvent;
  begin
  FInsideSync:=True;
  try
    FMyFuncEvent(self,FMyFunc,FMyFuncResult);
  finally
    FInsideSync:=False;
    end;
  end;

procedure TRtcConnection.CallMyResultEvent;
  begin
  FInsideSync:=True;
  try
    FMyResultEvent(self,FMyFuncData,FMyFuncResult);
  finally
    FInsideSync:=False;
    end;
  end;

procedure TRtcConnection.CallMyFuncPrepEvent;
  begin
  FInsideSync:=True;
  try
    FMyFuncPrepEvent(self,FMyFuncData);
  finally
    FInsideSync:=False;
    end;
  end;

// Trigger implementation ...

procedure TRtcServer.TriggerClientConnect;
  begin
  if not FClientConnectTriggered then
    begin
    FClientConnectTriggered:=True;

    EnterEvent;
    try
      CallClientConnect;
    finally
      LeaveEvent;
      end;
    end;
  end;

procedure TRtcServer.TriggerClientDisconnect;
  begin
  if FClientConnectTriggered then
    begin
    FClientConnectTriggered:=False;

    EnterEvent;
    try
      CallClientDisconnect;
    finally
      LeaveEvent;
      end;
    end;
  end;

procedure TRtcConnection.TriggerConnecting;
  begin
  if not FConnectingTriggered then
    begin
    FConnectingTriggered:=True;

    EnterEvent;
    try
      CallConnecting;
    finally
      LeaveEvent;
      end;

    Timeout.Connecting;
    end;
  end;

procedure TRtcConnection.TriggerDisconnecting;
  begin
  if FConnectingTriggered then
    begin
    FConnectingTriggered:=False;

    EnterEvent;
    try
      CallDisconnecting;
    finally
      LeaveEvent;
      end;
    end;
  end;

procedure TRtcConnection.TriggerConnect;
  begin
  if not FConnectTriggered then
    begin
    FConnectTriggered:=True;

    EnterEvent;
    try
      CallConnect;
    finally
      LeaveEvent;
      end;

    Timeout.Connect;
    end;
  end;

procedure TRtcClient.TriggerConnectError(E: Exception);
  begin
  Timeout.Disable;

  EnterEvent;
  try
    if assigned(OnConnectError) then
      begin
      FClosing:=True;
      try
        CallConnectError(E);
      finally
        FClosing:=False;
        end;
      FCS.Enter;
      try
        if ReconnectOn.ConnectError and
           isConnectionRequired and
           not FDisconnecting and
           not (E is EClientLimitReached) and
           not FReleasing and // not reconnecting if Release called on error.
           not FReconnecting then
          FConnecting:=True;
      finally
        FCS.Leave;
        end;
      end
    else
      begin
      FClosing:=True;
      try
        CallConnectError(E);
      finally
        FClosing:=False;
        end;
      if ReconnectOn.ConnectError and isConnectionRequired then
        begin
        FCS.Enter;
        try
          if not FDisconnecting and
             not FReleasing and // not reconnecting if Release called on error.
             not FReconnecting then
            FConnecting:=True;
        finally
          FCS.Leave;
          end;
        end
      else if not FConnecting then
        raise ERtcConnection.Create(E.Message);
      end;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcServer.TriggerListenError(E: Exception);
  begin
  EnterEvent;
  try
    if assigned(OnListenError) then
      begin
      FClosing:=True;
      try
        CallListenError(E);
      finally
        FClosing:=False;
        end;
      FCS.Enter;
      try
        if RestartOn.ListenError and
           not FListening and
           not FReleasing and // not reconnecting if Release called on error.
           not FReconnecting then
          FListening:=True;
      finally
        FCS.Leave;
        end;
      end
    else
      begin
      FClosing:=True;
      try
        CallListenError(E);
      finally
        FClosing:=False;
        end;
      if RestartOn.ListenError then
        begin
        FCS.Enter;
        try
          if not FListening and
             not FReleasing and // not reconnecting if Release called on error.
             not FReconnecting then
            FListening:=True;
        finally
          FCS.Leave;
          end;
        end
      else
        raise ERtcConnection.Create(E.Message);
      end;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcClient.TriggerConnectFail;
  begin
  Timeout.Disable;

  EnterEvent;
  try
    FClosing:=True;

    CallConnectFail;
  finally
    FClosing:=False;
    LeaveEvent;
    end;

  FCS.Enter;
  try
    if ReconnectOn.ConnectFail and
       isConnectionRequired and
       not FDisconnecting and
       not FReleasing and // not reconnecting if Release called on error.
       not FReconnecting then
      FConnecting:=True;
  finally
    FCS.Leave;
    end;
  end;

procedure TRtcClient.TriggerConnectLost;
  begin
  Timeout.Disable;

  EnterEvent;
  try
    FClosing:=True;

    CallConnectLost;

    FCS.Enter;
    try
      if ReconnectOn.ConnectLost and
         isConnectionRequired and
         not FDisconnecting and
         not FReleasing and // not reconnecting if Release called on error.
         not FReconnecting then
        FConnecting:=True;
    finally
      FCS.Leave;
      end;
  finally
    FClosing:=False;
    LeaveEvent;
    end;
  end;

procedure TRtcConnection.TriggerDisconnect;
  begin
  if FConnectTriggered then
    begin
    FConnectTriggered:=False;

    EnterEvent;
    try
      FClosing:=True;

      CallDisconnect;
    finally
      FClosing:=False;
      LeaveEvent;
      end;
    end;
  end;

procedure TRtcConnection.TriggerException(E: Exception);
  begin
  EnterEvent;
  try
    CallException(E);
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcServer.TriggerListenStart;
  begin
  EnterEvent;
  try
    CallListenStart;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcServer.TriggerListenStop;
  begin
  EnterEvent;
  try
    FClosing:=True;
    CallListenStop;
  finally
    FClosing:=False;
    LeaveEvent;
    end;
  end;

procedure TRtcServer.TriggerListenLost;
  begin
  EnterEvent;
  try
    FClosing:=True;
    CallListenLost;
  finally
    FClosing:=False;
    LeaveEvent;
    end;

  if RestartOn.ListenLost and
     not FReleasing then // not reconnecting if Release called on error.
    FListening:=True;
  end;

procedure TRtcClient.TriggerReconnect;
  begin
  if State=conActive then
    Disconnect;

  if State<>conInactive then
    begin
    if ReconnectOn.Wait>0 then
      Reconnect(ReconnectOn.Wait)
    else
      Reconnect(1);
    Exit;
    end;

  Timeout.Disable;

  EnterEvent;
  try
    CallReconnect;
  finally
    LeaveEvent;
    end;

  if not FReleasing and // Do not reconnect if released.
     not FConnecting and
     not FListening and
     not FStopping and
     not FDisconnecting then
    Connect;
  end;

procedure TRtcServer.TriggerRestart;
  begin
  if State=conListening then
    StopListen;

  if State<>conInactive then
    begin
    if RestartOn.Wait>0 then
      Restart(RestartOn.Wait)
    else
      Restart(1);
    Exit;
    end;

  EnterEvent;
  try
    CallRestart;
  finally
    LeaveEvent;
    end;

  if not FReleasing and // Do not reconnect if released.
     not FListening then
    Listen;
  end;

procedure TRtcClient.TriggerReadyToRelease;
  begin
  if InsideEvent then
    FReadyToRelease:=True
  else
    begin
    FReadyToRelease:=False;
    if FReleasing then
      begin
      Timeout.Stop;

      FClosing:=False;
      FReleasing:=False;
      FConnecting:=False;
      FListening:=False;
      if FDisconnecting then
        begin
        FDisconnecting:=False;
        Disconnect;
        end;
      Release;
      end
    else if FConnecting then
      begin
      Timeout.Disable;

      FClosing:=False;
      FConnecting:=False;
      if FDisconnecting then
        begin
        FClosing:=False;
        FDisconnecting:=False;
        Disconnect;
        end;
      Reconnect(ReconnectOn.Wait);
      end
    else if FDisconnecting then
      begin
      Timeout.Disable;

      FClosing:=False;
      FDisconnecting:=False;

      Disconnect;
      end;
    end;
  end;

procedure TRtcServer.TriggerReadyToRelease;
  var
    Par:TRtcServer;
  begin
  if InsideEvent then
    FReadyToRelease:=True
  else
    begin
    FReadyToRelease:=False;
    if FReleasing then
      begin
      Timeout.Stop;

      FClosing:=False;
      FReleasing:=False;
      FConnecting:=False;
      FListening:=False;
      if FStopping then
        begin
        FStopping:=False;
        if isClient then
          StopListen
        else
          Release;
        end
      else
        begin
        if FDisconnecting then
          begin
          FDisconnecting:=False;
          Disconnect;
          end;
        Release;
        end;
      end
    else if FListening then
      begin
      Timeout.Disable;

      FListening:=False;
      if FStopping or FDisconnecting then
        begin
        FDisconnecting:=False;
        FStopping:=False;
        if isClient then
          begin
          Par:=TRtcServer(Parent);
          Par.StopListen;
          Par.Restart(RestartOn.Wait);
          end
        else
          begin
          StopListen;
          Restart(RestartOn.Wait);
          end;
        end
      else
        begin
        if isClient then
          TRtcServer(Parent).Restart(RestartOn.Wait)
        else
          Restart(RestartOn.Wait);
        end;
      end
    else if FDisconnecting then
      begin
      Timeout.Disable;

      FClosing:=False;
      FDisconnecting:=False;

      if FStopping then
        begin
        FStopping:=False;
        StopListen;
        end
      else
        Disconnect;
      end;
    end;
  end;

function TRtcConnection.GetState: TRtcConnectionState;
  begin
  if assigned(Con) then
    Result:=Con.GetState
  else
    Result:=conInactive;
  end;

procedure TRtcConnection.ReleaseProvider;
  var
    MyCon:TRtcConnectionProvider;
  begin
  if assigned(Con) then
    begin
    ClearTriggers;
    MyCon:=Con;
    Con:=nil;
    MyCon.Release;
    end;
  end;

procedure TRtcServer.TriggerNewProvider(var Provider: TObject);
  var
    cla:TRtcBaseServerClass;
    cl:TRtcServer;
  begin
  cla:=TRtcBaseServerClass(self.ClassType);
  cl:=cla.Create(nil);
  try
    cl.FParent:=self;
    Provider := cl.CreateProvider; // Con:=TRtcServer(Provider);
    cl.CopyFrom(self);
    cl.SetParams;
  except
    try
      cl.Free;
    except
      end;
    Provider:=nil;
    raise;
    end;
  end;

procedure TRtcConnection.SetTriggers;
  begin
  if assigned(Con) then
    with Con do
      begin
      {$IFDEF FPC}
      SetError(@self.Error);

      SetTriggerBeforeCreate(@self.TriggerBeforeCreate);
      SetTriggerAfterDestroy(@self.TriggerAfterDestroy);

      SetTriggerConnecting(@self.TriggerConnecting);
      SetTriggerConnect(@self.TriggerConnect);
      SetTriggerDisconnect(@self.TriggerDisconnect);
      SetTriggerDisconnecting(@self.TriggerDisconnecting);

      SetTriggerReadyToRelease(@self.TriggerReadyToRelease);

      SetTriggerLastWrite(@self.TriggerLastWrite);

      SetTriggerDataOut(@self.TriggerDataOut);
      SetTriggerDataIn(@self.TriggerDataIn);
      SetTriggerDataSent(@self.TriggerDataSent);
      SetTriggerDataReceived(@self.TriggerDataReceived);
      SetTriggerDataLost(@self.TriggerDataLost);
      SetTriggerException(@self.TriggerException);
      {$ELSE}
      SetError(self.Error);

      SetTriggerBeforeCreate(self.TriggerBeforeCreate);
      SetTriggerAfterDestroy(self.TriggerAfterDestroy);

      SetTriggerConnecting(self.TriggerConnecting);
      SetTriggerConnect(self.TriggerConnect);
      SetTriggerDisconnect(self.TriggerDisconnect);
      SetTriggerDisconnecting(self.TriggerDisconnecting);

      SetTriggerReadyToRelease(self.TriggerReadyToRelease);

      SetTriggerLastWrite(self.TriggerLastWrite);

      SetTriggerDataOut(self.TriggerDataOut);
      SetTriggerDataIn(self.TriggerDataIn);
      SetTriggerDataSent(self.TriggerDataSent);
      SetTriggerDataReceived(self.TriggerDataReceived);
      SetTriggerDataLost(self.TriggerDataLost);
      SetTriggerException(self.TriggerException);
      {$ENDIF}
      end;
  end;

procedure TRtcServer.SetTriggers;
  begin
  inherited;
  if assigned(Con) then
    with TRtcServerProvider(Con) do
      begin
      {$IFDEF FPC}
      SetTriggerConnectionAccepting(@self.TriggerConnectionAccepting);
      SetTriggerConnectionAccepted(@self.TriggerConnectionAccepted);
      SetTriggerConnectionLost(@self.TriggerConnectionLost);

      SetTriggerNewProvider(@self.TriggerNewProvider);

      SetTriggerListenStart(@self.TriggerListenStart);
      SetTriggerListenStop(@self.TriggerListenStop);

      SetTriggerListenError(@self.TriggerListenError);
      SetTriggerListenLost(@self.TriggerListenLost);
      {$ELSE}
      SetTriggerConnectionAccepting(self.TriggerConnectionAccepting);
      SetTriggerConnectionAccepted(self.TriggerConnectionAccepted);
      SetTriggerConnectionLost(self.TriggerConnectionLost);

      SetTriggerNewProvider(self.TriggerNewProvider);

      SetTriggerListenStart(self.TriggerListenStart);
      SetTriggerListenStop(self.TriggerListenStop);

      SetTriggerListenError(self.TriggerListenError);
      SetTriggerListenLost(self.TriggerListenLost);
      {$ENDIF}
      end;
  end;

procedure TRtcClient.SetTriggers;
  begin
  inherited;
  if assigned(Con) then
    with TRtcClientProvider(Con) do
      begin
      {$IFDEF FPC}
      SetTriggerConnectionOpening(@self.TriggerConnectionOpening);
      SetTriggerConnectionClosing(@self.TriggerConnectionClosing);

      SetTriggerConnectFail(@self.TriggerConnectFail);
      SetTriggerConnectLost(@self.TriggerConnectLost);
      SetTriggerConnectError(@self.TriggerConnectError);
      {$ELSE}
      SetTriggerConnectionOpening(self.TriggerConnectionOpening);
      SetTriggerConnectionClosing(self.TriggerConnectionClosing);

      SetTriggerConnectFail(self.TriggerConnectFail);
      SetTriggerConnectLost(self.TriggerConnectLost);
      SetTriggerConnectError(self.TriggerConnectError);
      {$ENDIF}
      end;
  end;

procedure TRtcConnection.ClearTriggers;
  begin
  if assigned(Con) then
    with Con do
      begin
      SetError(nil);

      SetTriggerReadyToRelease(nil);

      SetTriggerBeforeCreate(nil);
      SetTriggerAfterDestroy(nil);

      SetTriggerConnecting(nil);
      SetTriggerConnect(nil);
      SetTriggerDisconnect(nil);
      SetTriggerDisconnecting(nil);

      SetTriggerLastWrite(nil);
      SetTriggerDataOut(nil);
      SetTriggerDataIn(nil);
      SetTriggerDataSent(nil);
      SetTriggerDataReceived(nil);
      SetTriggerDataLost(nil);
      SetTriggerException(nil);
      end;
  end;

procedure TRtcServer.ClearTriggers;
  begin
  inherited;
  if assigned(Con) then
    with TRtcServerProvider(Con) do
      begin
      SetTriggerConnectionAccepting(nil);
      SetTriggerConnectionAccepted(nil);
      SetTriggerConnectionLost(nil);

      SetTriggerNewProvider(nil);

      SetTriggerListenStart(nil);
      SetTriggerListenStop(nil);

      SetTriggerListenLost(nil);
      SetTriggerListenError(nil);
      end;
  end;

procedure TRtcClient.ClearTriggers;
  begin
  inherited;
  if assigned(Con) then
    with TRtcClientProvider(Con) do
      begin
      SetTriggerConnectionOpening(nil);
      SetTriggerConnectionClosing(nil);

      SetTriggerConnectFail(nil);
      SetTriggerConnectLost(nil);
      SetTriggerConnectError(nil);
      end;
  end;

procedure TRtcConnection.SetParams;
  begin
  FDataOut:=0;
  FDataIn:=0;
  FReadCount:=0;
  FWriteCount:=0;
  if assigned(Con) then
    with Con do
      begin
      SetAddr(self.ServerAddr);
      SetPort(self.ServerPort);
      SetMultiThreaded(self.MultiThreaded);
      end;
  end;

procedure TRtcClient.SetParams;
  begin
  inherited;
  if assigned(Con) then
    with TRtcClientProvider(Con) do
      begin
      //
      end;
  end;

procedure TRtcServer.SetParams;
  begin
  inherited;
  if assigned(Con) then
    with TRtcServerProvider(Con) do
      begin
      //
      end;
  end;

procedure TRtcConnection.TriggerAfterDestroy;
  begin
  Timeout.Stop;

  Con:=nil;
  if InsideEvent then
    FFree:=True
  else
    Free;
  end;

procedure TRtcConnection.TriggerBeforeCreate;
  begin
  //
  end;

procedure TRtcConnection.EnterEvent;
  begin
  Inc(FInsideEvent);
  end;

procedure TRtcConnection.LeaveEvent;
  begin
  Dec(FInsideEvent);
  if FInsideEvent=0 then
    if FFree then
      Free;
  end;

procedure TRtcConnection.CallConnect;
  begin
  if assigned(OnConnect) then
    OnConnect(self);
  end;

procedure TRtcConnection.CallConnecting;
  begin
  if assigned(OnConnecting) then
    OnConnecting(self);
  end;

procedure TRtcConnection.CallDataReceived;
  begin
  if assigned(OnDataReceived) then
    OnDataReceived(self);
  end;

procedure TRtcConnection.CallDataOut;
  begin
  if assigned(OnDataOut) then
    OnDataOut(self);
  end;

procedure TRtcConnection.CallDataIn;
  begin
  if assigned(OnDataIn) then
    OnDataIn(self);
  end;

procedure TRtcConnection.CallLastWrite;
  begin
  // placeholder
  end;

procedure TRtcConnection.CallDataSent;
  begin
  if assigned(OnDataSent) then
    OnDataSent(self);
  end;

procedure TRtcConnection.CallDisconnect;
  begin
  if assigned(OnDisconnect) then
    OnDisconnect(self);
  end;

procedure TRtcConnection.CallDisconnecting;
  begin
  if assigned(OnDisconnecting) then
    OnDisconnecting(self);
  end;

procedure TRtcConnection.CallException(E: Exception);
  begin
  if assigned(OnException) then
    OnException(self,E);
  end;

procedure TRtcConnection.CallReadyToSend;
  begin
  if assigned(OnReadyToSend) then
    OnReadyToSend(self);
  end;

procedure TRtcServer.CallClientConnect;
  begin
  if assigned(OnClientConnect) then
    OnClientConnect(self);
  end;

procedure TRtcServer.CallClientDisconnect;
  begin
  if assigned(OnClientDisconnect) then
   OnClientDisconnect(self);
  end;

procedure TRtcServer.CallListenError(E: Exception);
  begin
  if assigned(OnListenError) then
    OnListenError(self,E);
  end;

procedure TRtcServer.CallListenLost;
  begin
  if assigned(OnListenLost) then
    OnListenLost(self);
  end;

procedure TRtcServer.CallListenStart;
  begin
  if assigned(OnListenStart) then
    OnListenStart(self);
  end;

procedure TRtcServer.CallListenStop;
  begin
  if assigned(OnListenStop) then
    OnListenStop(self);
  end;

procedure TRtcServer.CallRestart;
  begin
  if assigned(OnRestart) then
    OnRestart(self);
  end;

procedure TRtcClient.CallConnectError(E: Exception);
  begin
  if assigned(OnConnectError) then
    OnConnectError(self,E);
  end;

procedure TRtcClient.CallConnectFail;
  begin
  if assigned(OnConnectFail) then
    OnConnectFail(self);
  end;

procedure TRtcClient.CallConnectLost;
  begin
  if assigned(OnConnectLost) then
    OnConnectLost(self);
  end;

procedure TRtcClient.CallReconnect;
  begin
  if assigned(OnReconnect) then
    OnReconnect(self);
  end;

function TRtcConnection.InsideEvent: boolean;
  begin
  Result:=inThread and (FInsideEvent>0);
  // Result:=FInsideEvent>0;
  end;

procedure TRtcClient.SetMultiThread(const Value: boolean);
  begin
  if Value<>FMultiThread then
    begin
    if assigned(Con) then
      if isConnected or isConnecting then
        Error('Can not change MultiThreaded after Connect.')
      else
        ReleaseProvider;
    FMultiThread := Value;
    end;
  end;

procedure TRtcServer.SetMultiThread(const Value: boolean);
  begin
  if Value<>FMultiThread then
    begin
    if not isClient and assigned(Con) then
      if isListening then
        Error('Can not change MultiThreaded after Listen.')
      else
        ReleaseProvider;
    FMultiThread := Value;
    end;
  end;

procedure TRtcConnection.CallDataLost;
  begin
  // NO implementation
  end;

procedure TRtcConnection.TriggerDataLost;
  begin
  // NO implementation
  end;

function TRtcConnection.isExtension: boolean;
  begin
  Result:=False;
  end;

{ TRtcReconnectParam }

constructor TRtcReconnectParam.Create;
  begin
  inherited;
  FConnectError:=False;
  FConnectLost:=False;
  FConnectFail:=False;
  FWait:=0;
  end;

destructor TRtcReconnectParam.Destroy;
  begin
  inherited;
  end;

{ TRtcRestartParam }

constructor TRtcRestartParam.Create;
  begin
  inherited;
  FListenLost:=False;
  FListenError:=False;
  FWait:=0;
  end;

destructor TRtcRestartParam.Destroy;
  begin
  inherited;
  end;

{ TRtTimeoutDisconnect }

type
  TRtcTimeoutDisconnect=class(TRtcJob)
  public
    Conn:TRtcConnection;
    constructor Create(Con:TRtcConnection);

    procedure Run(Thr:TRtcThread); override;
    procedure Kill; override;
    end;

constructor TRtcTimeoutDisconnect.Create(Con: TRtcConnection);
  begin
  inherited Create;
  Conn:=Con;
  end;

procedure TRtcTimeoutDisconnect.Kill;
  begin
  // do not destroy
  end;

procedure TRtcTimeoutDisconnect.Run(Thr:TRtcThread);
  begin
  try
    if assigned(Conn) then
      begin
      if LOG_TIMEOUT_DISCONNECTS then
        Log('ABORT with Timeout. Local '+Conn.LocalAddr+':'+Conn.LocalPort+', Peer '+Conn.PeerAddr+':'+Conn.PeerPort);

      Conn.InternalDisconnect;
      end;
  except
    on E:Exception do
      Log('TRtcTimeoutDisconnect.Run',E);
    end;
  end;

{ TRtcTimeout }

procedure TRtcTimeout.TriggerTimeout;
  begin
  try
    if assigned(Conn) then
      begin
      if LOG_TIMEOUT_DISCONNECTS then
        Log('ABORT with Timeout. Local '+Conn.LocalAddr+':'+Conn.LocalPort+', Peer '+Conn.PeerAddr+':'+Conn.PeerPort);

      Conn.InternalDisconnect;
      end;
  except
    on E:Exception do
      Log('TRtcTimeout.TriggerTimeout',E);
    end;
  end;

constructor TRtcTimeout.Create(Con:TRtcConnection);
  begin
  inherited Create;

  FAfterConnecting:=0;
  FAfterConnect:=0;
  FAfterDataReceived:=0;
  FAfterDataLost:=0;
  FAfterDataSend:=0;
  FAfterDataOut:=0;
  FAfterDataIn:=0;
  FAfterDataSent:=0;

  FInterval:=-1;

  FConn:=Con;
  FThr:=nil;
  FJob:=nil;
  FTimer:=nil;
  end;

destructor TRtcTimeout.Destroy;
  begin
  if assigned(FTimer) then
    begin
    TRtcTimer.Stop(FTimer);
    FTimer:=nil;
    end;
  if assigned(FJob) then
    begin
    FJob.Free;
    FJob:=nil;
    end;

  inherited;
  end;

procedure TRtcTimeout.Start(Multi_Threaded:boolean);
  begin
  if not assigned(FTimer) then
    FTimer:=TRtcTimer.Create(Multi_Threaded);
  FInterval:=-1;
  if Multi_Threaded then
    begin
    if assigned(FConn) and assigned(FConn.Con) then
      begin
      FThr:=FConn.Con.GetThread;
      if not assigned(FJob) then
        FJob:=TRtcTimeoutDisconnect.Create(FConn);
      end
    else
      FThr:=nil;
    end
  else
    FThr:=nil;
  end;

procedure TRtcTimeout.Stop;
  begin
  if assigned(FTimer) then
    begin
    TRtcTimer.Stop(FTimer);
    FTimer:=nil;
    end;
  FInterval:=-1;
  FThr:=nil;
  end;

procedure TRtcTimeout.TimerSet;
  begin
  if assigned(FThr) then
    TRtcTimer.Enable(FTimer,FInterval*1000, FThr, FJob)
  else
    begin
    {$IFDEF FPC}
      TRtcTimer.Enable(FTimer,FInterval*1000, @TriggerTimeout);
    {$ELSE}
      TRtcTimer.Enable(FTimer,FInterval*1000, TriggerTimeout);
    {$ENDIF}
    end;
  end;

procedure TRtcTimeout.TimerReset;
  begin
  TRtcTimer.Reset(FTimer);
  end;

procedure TRtcTimeout.Connecting;
  begin
  if assigned(FTimer) then
    if FAfterConnecting<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterConnecting>0 then
      begin
      if FInterval<>FAfterConnecting then
        begin
        FInterval:=FAfterConnecting;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.Connect;
  begin
  if assigned(FTimer) then
    if FAfterConnect<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterConnect>0 then
      begin
      if FInterval<>FAfterConnect then
        begin
        FInterval:=FAfterConnect;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.DataReceived;
  begin
  if assigned(FTimer) then
    if FAfterDataReceived<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterDataReceived>0 then
      begin
      if FInterval<>FAfterDataReceived then
        begin
        FInterval:=FAfterDataReceived;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.DataLost;
  begin
  if assigned(FTimer) then
    if FAfterDataLost<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterDataLost>0 then
      begin
      if FInterval<>FAfterDataLost then
        begin
        FInterval:=FAfterDataLost;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.DataSending;
  begin
  if assigned(FTimer) then
    if FAfterDataSend<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterDataSend>0 then
      begin
      if FInterval<>FAfterDataSend then
        begin
        FInterval:=FAfterDataSend;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.DataOut;
  begin
  if assigned(FTimer) then
    if FAfterDataOut<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterDataOut>0 then
      begin
      if FInterval<>FAfterDataOut then
        begin
        FInterval:=FAfterDataOut;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.DataIn;
  begin
  if assigned(FTimer) then
    if FAfterDataIn<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterDataIn>0 then
      begin
      if FInterval<>FAfterDataIn then
        begin
        FInterval:=FAfterDataIn;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.DataSent;
  begin
  if assigned(FTimer) then
    if FAfterDataSent<0 then
      TRtcTimer.Disable(FTimer)
    else if FAfterDataSent>0 then
      begin
      if FInterval<>FAfterDataSent then
        begin
        FInterval:=FAfterDataSent;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.Enable(timeout:integer);
  begin
  if assigned(FTimer) then
    if timeout<0 then
      TRtcTimer.Disable(FTimer)
    else if timeout>0 then
      begin
      if FInterval<>timeout then
        begin
        FInterval:=timeout;
        TimerSet;
        end
      else
        TimerReset;
      end
    else
      TimerReset;
  end;

procedure TRtcTimeout.Disable;
  begin
  if assigned(FTimer) then
    TRtcTimer.Disable(FTimer);
  FInterval:=-1;
  end;

{ TRtcClientRequest }

procedure TRtcClientRequest.Clear;
  begin
  inherited;
  Init;
  FReposted:=0;
  end;

procedure TRtcClientRequest.Init;
  begin
  FStarted:=False;
  FActive:=False;
  FComplete:=False;

  FSkipped:=False;
  FReposting:=False;
  if FAutoLength then
    begin
    FAutoLength:=False;
    Value['Content-Length']:='';
    end;

  FDataOut:=0;
  end;

procedure TRtcClientRequest.Skip;
  begin
  FSkipped:=True;
  end;

procedure TRtcClientRequest.Repost;
  begin
  if not Reposting then
    begin
    Init;

    FReposting:=True;
    Inc(FReposted);
    end;
  end;

constructor TRtcClientRequest.Create;
  begin
  inherited;
  end;

destructor TRtcClientRequest.Destroy;
  begin
  inherited;
  end;

{ TRtcClientResponse }

procedure TRtcClientResponse.Clear;
  begin
  inherited;
  StatusCode:=0;
  StatusText:='';
  FStarted:=False;
  FReceiving:=False;
  FRejected:=False;
  FDone:=False;

  FDataIn:=0;
  end;

constructor TRtcClientResponse.Create;
  begin
  inherited;
  end;

destructor TRtcClientResponse.Destroy;
  begin
  inherited;
  end;

procedure TRtcClientResponse.Reject;
  begin
  FRejected:=True;
  end;

{ TRtcServerRequest }

procedure TRtcServerRequest.Clear;
  begin
  inherited;
  FStarted:=False;
  FComplete:=False;

  FAccepted:=False;

  FDataIn:=0;
  end;

constructor TRtcServerRequest.Create;
  begin
  inherited;
  end;

destructor TRtcServerRequest.Destroy;
  begin
  inherited;
  end;

{ TRtcServerResponse }

constructor TRtcServerResponse.Create;
  begin
  inherited;
  StatusCode:=200;
  StatusText:='OK';
  end;

destructor TRtcServerResponse.Destroy;
  begin
  StatusText:='';
  inherited;
  end;

procedure TRtcServerResponse.Clear;
  begin
  inherited;
  StatusCode:=200;
  StatusText:='OK';

  FStarted:=False;
  FSending:=False;
  FDone:=False;
  FSent:=False;

  SendContent:=True;

  FDataOut:=0;
  end;

procedure TRtcServerResponse.Status(Code: integer; const Text: string);
  begin
  StatusCode:=Code;
  StatusText:=Text;
  end;

procedure TRtcServerResponse.Status(CodeText: string);
  var
    c,s:string;
    p:integer;
  begin
  p:=Pos(' ',CodeText);
  if p>0 then
    begin
    c:=Copy(CodeText,1,p-1);
    s:=Copy(CodeText,p+1,length(CodeText)-p);
    end
  else
    begin
    c:=CodeText;
    s:='X';
    end;
  try
    StatusCode:=StrToInt(c);
    StatusText:=s;
  except
    StatusCode:=500; // Server error!
    StatusText:=CodeText;
    end;
  end;

function TRtcServer.copyOf: TRtcServer;
  var
    cla:TRtcBaseServerClass;
  begin
  cla:=TRtcBaseServerClass(self.ClassType);
  Result:=cla.Create(nil);
  try
    Result.FParent:=self;
    Result.CreateProvider; // Con:=TRtcServer(Provider);
    Result.CopyFrom(self);
    Result.SetParams;
  except
    try
      Result.Free;
    except
      end;
    raise;
    end;
  end;

function TRtcClient.isConnectionRequired: boolean;
  begin
  Result:=True;
  end;

initialization
Conn_CS:=TRtcCritSec.Create;
finalization
Garbage(Conn_CS);
end.
