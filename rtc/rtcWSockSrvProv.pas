{
  "Server Connection Provider" - Copyright (c) Danijel Tkalcec
  @html(<br>)

  Server Connection Provider implementation using a modified version
  of TWSocket, TWSocketServer & TWSocketClient
  from F.Piette's Internet Component Suite (ICS).

  To implement a connection provider, we only need to extend the TRtcConnectionProvider class.

  Files used from F.Piette's Internet Component Suite (ICS) library:
    WSockBuf.pas
    WSocket.pas

  @exclude
}

unit rtcWSockSrvProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  rtcTrashcan,

  Classes,
  SysUtils,

  Windows, Messages, // Windows and Messages units are used only in Multithreading, to send messages

{$IFDEF CLR}
  DTkalcec.Ics.WSocket,
  DTkalcec.Ics.WSocketServer,
{$ELSE}
  WSocket_rtc, // WinSock classes
{$ENDIF}

  memBinList,
  memBinTree,

  rtcSyncObjs,
  rtcLog,
  rtcThrPool,

  rtcPlugins,
  rtcThrConnProv, // Threaded connection provider wrapper

  rtcConnProv, // Basic connection provider wrapper
  rtcConnLimit;

const
  LOG_REFUSED_CONNECTIONS:boolean=False;

type
  TRtcWSockServerProvider = class;

  TRtcWSockServerProtocol = (proTCP, proUDP);

  TRtcWSockClientThread = class(TRtcThread)
  public
    H_Sock: TSocket;
    Par: TRtcWSockServerProvider;
    _Silent: boolean;

    RtcConn: TRtcWSockServerProvider;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Init;
    function Work(Job:TObject):boolean; override;
    procedure Kill(Job:TObject); override;
    end;

  TRtcWSocketClient = class(TWSocketClient)
  public
    Thr: TRtcWSockClientThread;

    procedure Call_FD_CLOSE(Err:word); override;
    procedure Call_FD_READ; override;
    procedure Call_FD_WRITE; override;
    end;

  TRtcWSockServerThread = class(TRtcThread)
  public
    RtcConn: TRtcWSockServerProvider;
    Releasing: boolean;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure StartListen;
    procedure StopListen;

    function Work(Job:TObject):boolean; override;
    procedure Kill(Job:TObject); override;
    end;

  TRtcWSocketServer = class(TWSocketServer)
  public
    Thr: TRtcWSockServerThread;

    procedure Call_FD_READ; override;
    procedure Call_FD_WRITE; override;
    procedure Call_FD_ACCEPT; override;
    end;

  TRtcWSockServerProvider = class(TRtcThrServerProvider)
  private
    FConnID:longint;

    Conn:TWSocket;

    FRawOut,
    FPlainOut:int64;

    FCryptPlugin: TRtcCryptPlugin;

    FProtocol: TRtcWSockServerProtocol;

    FReadBuff:string;

    FCS:TRtcCritSec;

    FClientList:tBinList;
    FThrList:tBinList;

    FMultiCast          : Boolean;
    FMultiCastAddrStr   : String;
    FReuseAddr          : Boolean;

    FListenerUp:boolean;

    Client_Thread: TRtcWSockClientThread;
    Server_Thread: TRtcWSockServerThread;

    FParent:TRtcWSockServerProvider;

    procedure wsOnBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure wsOnChangeState(Sender: TObject; OldState,NewState: TSocketState);

    procedure wsOnSessionAvailable(Sender: TObject; ErrCode: Word);
    procedure wsOnSessionClosed(Sender: TObject; ErrCode: Word);

    procedure wsOnDataReceived(Sender: TObject; ErrCode: Word);
    procedure wsOnDataSent(Sender: TObject; ErrCode: Word);
    procedure wsOnDataOut(Sender: TObject; Len: Cardinal);
    procedure wsOnDataIn(Sender: TObject; Len: Cardinal);

  protected

    procedure Enter; override;
    procedure Leave; override;

    function GetClientThread:TRtcThread; override;
    function GetServerThread:TRtcThread; override;

    procedure AddClient(Client:TRtcWSockServerProvider);
    procedure RemoveClient(Client:TRtcWSockServerProvider);
    procedure KillClients;

    function ClientCount:integer;

    procedure AddThread(Thr:TRtcThread);
    procedure RemoveThread(Thr:TRtcThread);
    procedure KillThreads;

    function _Active:boolean;
    function _Visible:boolean;

    procedure CopyFrom(Dup:TRtcConnectionProvider);

    function PostWrite(HighPriority:boolean=False):boolean;
    function PostRead(HighPriority:boolean=False):boolean;

    procedure StartListener;

    procedure DirectWrite(const s: string);
    procedure BufferWrite(const s: string);

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Release; override;

    function GetParent:TRtcConnectionProvider; override;

    procedure Check; override;
    procedure InternalDisconnect; override;

    procedure Listen; override;
    procedure Disconnect; override;

    function Read: string; override;
    procedure Write(const s: string; SendNow:boolean=True); override;

    property Proto:TRtcWSockServerProtocol read FProtocol write FProtocol;

    property UdpMultiCast       : Boolean           read  FMultiCast
                                                    write FMultiCast;
    property UdpMultiCastAddr   : String            read  FMultiCastAddrStr
                                                    write FMultiCastAddrStr;
    property UdpReuseAddr       : Boolean           read  FReuseAddr
                                                    write FReuseAddr;

    property CryptPlugin        : TRtcCryptPlugin   read FCryptPlugin
                                                    write FCryptPlugin;
    end;

implementation

{$IFDEF CLR}
uses
  System.Security, Math;
{$ENDIF}

{ TRtcWSockClientThread }

type
  TRtcBaseMessage=class
    end;
  TRtcInfoMessage=class(TRtcBaseMessage)
  public
    Error:word;
    constructor Create(Value:word);
    end;
  TRtcCloseMessage=class(TRtcInfoMessage)
    end;

var
  Message_WSAccept,
  Message_WSInit,
  Message_WSStop,
  Message_WSRelease_Silent,
  Message_WSRelease_Normal,
  Message_WSCloseConn,
  Message_WSRelease,
  Message_WSRead,
  Message_WSWrite,
  Message_WSClose:TRtcBaseMessage;


{ TRtcWSockServerProvider }

constructor TRtcWSockServerProvider.Create;
  begin
  inherited;

  FRawOut:=0;
  FPlainOut:=0;
  FConnID:=GetNextConnID;

  FCS:=TRtcCritSec.Create;

  FClientList:=TBinList.Create(128);
  FThrList:=TBinList.Create(128);

  FPeerPort:='';
  FPeerAddr:='0.0.0.0';
  FLocalPort:='';
  FLocalAddr:='0.0.0.0';

  FProtocol:=proTCP;
  FMultiCastAddrStr:='';

  FReadBuff:='';
  SetLength(FReadBuff, WSOCK_READ_BUFFER_SIZE);

  FParent:=nil;

  FListenerUp:=False;

  Conn:=nil;
  end;

destructor TRtcWSockServerProvider.Destroy;
  begin
  { Before destroying this connection object,
    we will disconnect this and all related open connections. }

  Closing:=True;
  Silent:=True;

  try
    if assigned(Conn) then
      InternalDisconnect;

    if assigned(FParent) and not FParent.Silent then
      TriggerConnectionLost;

  finally
    SetLength(FReadBuff,0);

    FParent:=nil;
    FClientList.Free;
    FThrList.Free;
    FCS.Free;

    FPeerPort:='';
    FPeerAddr:='';
    FLocalPort:='';
    FLocalAddr:='';
    FMultiCastAddrStr:='';
    end;

  if assigned(Client_Thread) then
    TRtcThread.PostJob(Client_Thread, Message_WSStop, True);

  if assigned(Server_Thread) then
    TRtcThread.PostJob(Server_Thread, Message_WSStop, True);

  inherited;
  end;

procedure TRtcWSockServerProvider.CopyFrom(Dup: TRtcConnectionProvider);
  begin
  Proto:=TRtcWSockServerProvider(Dup).Proto;
  FCryptPlugin:=TRtcWSockServerProvider(Dup).FCryptPlugin;

  {$IFDEF FPC}
  Conn.OnBgException:=@wsOnBgException;
  {$ELSE}
  Conn.OnBgException:=wsOnBgException;
  {$ENDIF}

  if Conn is TWSocketServer then
    begin
    with Conn as TWSocketServer do
      begin
      case Proto of
        proTCP:Protocol:=spTcp;
        proUDP:Protocol:=spUdp;
        end;

      {$IFDEF FPC}
      OnSessionAvailable:=@wsOnSessionAvailable;
      OnChangeState:=@wsOnChangeState;
      {$ELSE}
      OnSessionAvailable:=wsOnSessionAvailable;
      OnChangeState:=wsOnChangeState;
      {$ENDIF}
      end;
    end
  else if Conn is TWSocketClient then
    begin
    with Conn as TWSocketClient do
      begin
      case Proto of
        proTCP:Protocol:=spTcp;
        proUDP:Protocol:=spUdp;
        end;

      {$IFDEF FPC}
      OnDataReceived:=@wsOnDataReceived;
      OnDataSent:=@wsOnDataSent;
      OnDataOut:=@wsOnDataOut;
      OnDataIn:=@wsOnDataIn;
      OnChangeState:=@wsOnChangeState;
      {$ELSE}
      OnDataReceived:=wsOnDataReceived;
      OnDataSent:=wsOnDataSent;
      OnDataOut:=wsOnDataOut;
      OnDataIn:=wsOnDataIn;
      OnChangeState:=wsOnChangeState;
      {$ENDIF}
      end;
    end;
  end;

procedure TRtcWSockServerProvider.Enter;
  begin
  FCS.Enter;
  end;

procedure TRtcWSockServerProvider.Leave;
  begin
  FCS.Leave;
  end;

procedure TRtcWSockServerProvider.Listen;
  begin
  if assigned(Server_Thread) then
    TRtcThread.PostJob(Server_Thread, Message_WSInit)
  else if GetMultiThreaded then
    begin
    Server_Thread := TRtcWSockServerThread.Create;
    Server_Thread.RtcConn:= self;
    TRtcThread.PostJob(Server_Thread,Message_WSInit);
    end
  else
    StartListener;
  end;

procedure TRtcWSockServerProvider.Disconnect;
  begin
  if assigned(Client_Thread) then
    TRtcThread.PostJob(Client_Thread, Message_WSRelease)
  else if assigned(Server_Thread) then
    TRtcThread.PostJob(Server_Thread, Message_WSCloseConn)
  else
    begin
    Lost:=False;
    InternalDisconnect;
    end;
  end;

procedure TRtcWSockServerProvider.InternalDisconnect;
  var
    myCon2:TWSocket;
    s_out:string;
  begin
  if Conn=nil then
    begin
    Closing:=True;
    Exit;
    end;

  if Conn is TWSocketServer then
    begin
    with Conn as TWSocketServer do
      begin
      OnBgException:=nil;
      OnChangeState:=nil;

      OnSessionAvailable:=nil;
      end;

    Closing:=True;
    State:=conInactive;

    if FListenerUp then
      begin
      FListenerUp:=False;
      if not Silent then
        TriggerListenStop;
      end;

    myCon2:=Conn;
    Conn:=nil;  // hide connections from component

    if GetMultiThreaded then
      KillThreads
    else
      KillClients;

    try
      myCon2.Close;
    except
      on E:Exception do
        if LOG_SOCKET_ERRORS then
          Log('WSockServerProvider.InternalDisconnect: MyCon2.Close',E);
      end;
    try
      myCon2.Release;
    except
      on E:Exception do
        if LOG_AV_ERRORS then
          Log('WSockServerProvider.InternalDisconnect: MyCon2.Release',E);
      end;

    if Lost then
      TriggerListenLost;

    {if assigned(Server_Thread) then
      begin
      Server_Thread.RtcConn:=nil;
      Server_Thread.Stop;
      Server_Thread:=nil;
      end;}
    TriggerReadyToRelease;
    end
  else if State in [conActive,conActivating] then
    begin
    if State=conActive then
      State:=conClosing
    else
      State:=conInactive;

    with Conn do // deactivate all events for this client connection
      begin
      OnBgException:=nil;

      OnDataReceived:=nil;
      OnDataSent:=nil;
      OnDataOut:=nil;
      OnDataIn:=nil;
      end;

    if not Closing then
      begin
      if assigned(FCryptPlugin) then
        begin
        s_out:='';
        FCryptPlugin.BeforeDisconnect(FConnID,s_out);
        if s_out<>'' then
          begin
          DirectWrite(s_out);
          s_out:='';
          end;
        end;
      wsOnSessionClosed(self,0);
      end
    else
      begin
      myCon2:=Conn;
      Conn:=nil;

      try
        MyCon2.Close;
      except
        on E:Exception do
          if LOG_SOCKET_ERRORS then
            Log('WSockServerProvider.InternalDisconnect: MyCon.Close',E);
        end;
      try
        MyCon2.Release;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('WSockServerProvider.InternalDisconnect: MyCon.Release',E);
        end;
      end;
    end;
  end;

procedure TRtcWSockServerProvider.wsOnDataOut(Sender: TObject; Len: Cardinal);
  begin
  if _Visible then
    begin
    if (State=conListening) and (Proto=proUDP) then
      begin
      FDataOut:=Len;
      TriggerDataOut;
      TriggerReadyToRelease;
      end
    else if State=conActive then
      begin
      if assigned(FCryptPlugin) then
        begin
        Dec(FRawOut,Len);
        if (FRawOut=0) and (FPlainOut>0) then
          begin
          FDataOut:=FPlainOut;
          FPlainOut:=0;
          TriggerDataOut;
          TriggerReadyToRelease;
          end;
        end
      else
        begin
        FDataOut:=Len;
        TriggerDataOut;
        TriggerReadyToRelease;
        end;
      end;
    end;
  end;

procedure TRtcWSockServerProvider.wsOnDataIn(Sender: TObject; Len: Cardinal);
  begin
  if _Visible then
    begin
    if (State=conListening) and (Proto=proUDP) then
      begin
      FDataIn:=Len;
      TriggerDataIn;
      TriggerReadyToRelease;
      end
    else if State=conActive then
      begin
      if not assigned(FCryptPlugin) then
        begin
        FDataIn:=Len;
        TriggerDataIn;
        TriggerReadyToRelease;
        end;
      end;
    end;
  end;

function TRtcWSockServerProvider.Read: string;
  var
    len:longint;
    s_in, s_out:string;
  begin
  if Proto=proTCP then
    begin
    len:=Conn.Receive(FReadBuff[1], length(FReadBuff));
    if len<=0 then
      Result:=''
    else if assigned(FCryptPlugin) then
      begin
      // Decrypt input data ...
      SetLength(s_in,len);
      Move(FReadBuff[1],s_in[1],len);
      s_out:=''; Result:='';
      FCryptPlugin.DataReceived(FConnID, s_in, s_out, Result);
      if length(Result)>0 then
        begin
        // Trigger the "OnDataIn" event ...
        FDataIn:=length(Result);
        TriggerDataIn;
        end;
      if s_out<>'' then
        DirectWrite(s_out);
      end
    else
      begin
      SetLength(Result,len);
      Move(FReadBuff[1],Result[1],len);
      end;
    end
  else
    begin
    Result:=FReadBuff;
    FReadBuff:='';
    end;
  end;

procedure TRtcWSockServerProvider.Write(const s: string; SendNow:boolean=True);
  var
    s_out:string;
  begin
  if Closing then
    Exit;

  if Conn=nil then
    Error('Not connected.');

  if assigned(FCryptPlugin) then
    begin
    FCryptPlugin.DataToSend(FConnID,s,s_out);
    Inc(FPlainOut, length(s));
    if s_out<>'' then
      DirectWrite(s_out);
    end
  else if SendNow then
    DirectWrite(s)
  else
    BufferWrite(s);
  end;

procedure TRtcWSockServerProvider.DirectWrite(const s: string);
  var
    len:integer;
  begin
  if Conn is TWSocketServer then // Server will send to all connected clients
    begin
    { This implementation is for test purposes only.
      Data should be only sent to clients using the appropriate connection objects. }

    if Proto=proUDP then
      begin
      len:=Conn.SendStr(s);
      if len<0 then
        Error('Error #'+IntToStr(Conn.LastError)+': '+WSocketErrorDesc(Conn.LastError))
      else if len<length(s) then
        Error('Wanted to write '+IntToStr(length(s))+' bytes, written only '+IntToStr(len)+' bytes.');
      end;
    end
  else
    begin
    if RTC_LIMIT_CONN and assigned(Client_Thread) then
      if not rtcStartAction(Client_Thread, RTC_ACTION_WRITE) then
        begin
        if assigned(FCryptPlugin) then
          Inc(FRawOut, length(s));
        len:=Conn.BuffStr(s);
        if len<0 then
          Error('Error #'+IntToStr(Conn.LastError)+': '+WSocketErrorDesc(Conn.LastError))
        else if len<length(s) then
          Error('Wanted to write '+IntToStr(length(s))+' bytes, written only '+IntToStr(len)+' bytes.');

        PostWrite(True);
        Exit;
        end;

    if assigned(FCryptPlugin) then
      Inc(FRawOut, length(s));
    len:=Conn.SendStr(s);
    if len<0 then
      Error('Error #'+IntToStr(Conn.LastError)+': '+WSocketErrorDesc(Conn.LastError))
    else if len<>length(s) then
      Error('Wanted to write '+IntToStr(length(s))+' bytes, written only '+IntToStr(len)+' bytes.');
    end;
  end;

procedure TRtcWSockServerProvider.BufferWrite(const s: string);
  var
    len:integer;
  begin
  if assigned(FCryptPlugin) then
    Inc(FRawOut, length(s));
  len:=Conn.BuffStr(s);
  if len<0 then
    Error('Error #'+IntToStr(Conn.LastError)+': '+WSocketErrorDesc(Conn.LastError))
  else if len<>length(s) then
    Error('Wanted to write '+IntToStr(length(s))+' bytes, written only '+IntToStr(len)+' bytes.');
  end;

procedure TRtcWSockServerProvider.wsOnChangeState(Sender: TObject;
    OldState, NewState: TSocketState);
  begin
  if Closing then Exit;

  if assigned(Conn) then
    begin
    if (Sender is TWSocketServer) then
      begin
      if NewState=wsListening then
        begin
        FListenerUp:=True;
        try
          FLocalAddr:=Conn.GetXAddr;
          FLocalPort:=Conn.GetXPort;
          FPeerAddr:='';
          FPeerPort:='';
        except
          on E:Exception do
            if LOG_SOCKET_ERRORS then
              Log('ChangeState.GetXAddr',E);
          end;

        TriggerListenStart;
        TriggerReadyToRelease;
        end
      else if NewState=wsClosed then
        begin
        { This is important, so we catch the case
          where Listener gets cut off by the OS. }
        InternalDisconnect;
        end;
      end
    else
      begin
      if NewState=wsConnected then
        begin
        FLocalAddr:=Conn.GetXAddr;
        if FLocalAddr<>'0.0.0.0' then
          begin
          FLocalPort:=Conn.GetXPort;
          FPeerAddr:=Conn.GetPeerAddr;
          FPeerPort:=Conn.GetPeerPort;

          TriggerConnecting;
          end;
        end
      else if NewState=wsClosed then
        wsOnSessionClosed(Sender,0);
      end;
    end;
  end;

procedure TRtcWSockServerProvider.wsOnSessionAvailable(Sender: TObject; ErrCode:Word);
  var
    cl: TRtcWSockServerProvider;
    obj: TObject;
    _Client: TWSocketClient;
    HSock: TSocket;
  begin
  if Closing then Exit;

  HSock:=TWSocketServer(Sender).Accept;
  if HSock=INVALID_SOCKET then Exit; // not a socket

  try
    TriggerConnectionAccepting;
  except
    on E:Exception do
      begin
      if LOG_REFUSED_CONNECTIONS then
        Log('Connection refused with Message: '+E.Message);
      WSocket_closesocket(HSock);
      Exit; // connection refused.
      end;
    end;

  if Closing then
    begin
    WSocket_closesocket(HSock);
    if LOG_REFUSED_CONNECTIONS then
      Log('Connection refused: Server closing.');
    Exit; // connection refused.
    end;

  if GetMultiThreaded then
    begin
    cl:=nil;
    try
      TriggerNewProvider(obj); // create new connection provider
      if obj=nil then
        raise Exception.Create('Connection provider not created.')
      else if obj is TRtcWSockServerProvider then
        cl:=TRtcWSockServerProvider(obj)
      else
        raise Exception.Create('Wrong connection provider class created.');

      cl.FParent:=self;

      cl.Client_Thread := TRtcWSockClientThread.Create;
      with cl.Client_Thread do
        begin
        Par:=self;
        _Silent:=False;
        H_Sock:=HSock;
        HSock:=0;
        RtcConn:= cl;
        end;

    except
      on E:Exception do
        begin
        if LOG_AV_ERRORS then
          Log('SesAvail(MultiThreaded)',E);

        if assigned(cl) then
          begin
          try
            cl.InternalDisconnect;
          except
            on E:Exception do
              if LOG_SOCKET_ERRORS then
                Log('SesAvail cl.Disconnect',E);
            end;
          {try
            cl.Free;
          except
            on E:Exception do
              Log('SesAvail cl.Free',E);
            end;}
          end;

        try
          if HSock<>0 then
            WSocket_closesocket(HSock);
        except
          on E:Exception do
            if LOG_SOCKET_ERRORS then
              Log('SesAvail WSocket_close',E);
          end;

        Exit;
        end;
      end;

    AddThread(cl.Client_Thread); // make sure we remove this thread on Disconnect.
    TRtcThread.PostJob(cl.Client_Thread, Message_WSInit);
    end
  else // NOT MULTI-THREADED!
    begin
    cl:=nil;
    try
      // Create Provider object
      TriggerNewProvider(obj); // create new connection provider
      if obj=nil then
        raise Exception.Create('Connection provider not created.')
      else if obj is TRtcWSockServerProvider then
        cl:=TRtcWSockServerProvider(obj)
      else
        raise Exception.Create('Wrong connection provider class created.');

      cl.FParent:=self;
      _Client:=TWSocketClient.Create(nil);

      cl.Conn:=_Client;
      cl.CopyFrom(self); // initialize connection object
      cl.State:=conActivating;

      cl.TriggerConnectionAccepted;

      _Client.HSocket:=HSock;

      HSock:=0;
    except
      on E:Exception do
        begin
        if LOG_SOCKET_ERRORS then
          Log('SesAvail(not MultiThreaded)',E);

        if assigned(cl) then
          begin
          try
            cl.InternalDisconnect;
          except
            on E:Exception do
              if LOG_SOCKET_ERRORS then
                Log('SesAvail cl.Disconnect',E);
            end;
          try
            cl.Free;
          except
            on E:Exception do
              if LOG_AV_ERRORS then
                Log('SesAvail cl.Free',E);
            end;
          end;

        try
          if HSock<>0 then
            WSocket_closesocket(HSock);
        except
          on E:Exception do
            if LOG_SOCKET_ERRORS then
              Log('SesAvail WSock_Close',E);
          end;
        end;
      end;
    end;
  end;

procedure TRtcWSockServerProvider.wsOnSessionClosed(Sender: TObject; ErrCode:Word);
  var
    myParent:TRtcWSockServerProvider;
    myCon:TWSocket;
  begin
  { Client connection closed.

    This method is called when one of the active connections get closed.
    It handles connections closing for all active connection types
    (incomming and outgoing connections). }

  if not Silent then
    if not assigned(FParent) then
      TriggerDisconnecting
    else if not FParent.Silent then
      TriggerDisconnecting;

  if assigned(Conn) and not Closing then // Connection object still here ?
    begin
    Closing:=True; // Let everyone know we are closing now ...

    myParent:=nil;
    try
      myParent:=FParent;

      if (State in [conActive,conClosing]) and assigned(myParent) then // Connection was activated.
        begin
        myParent.RemoveClient(self);
        if not MyParent.Silent then
          begin
          if assigned(FCryptPlugin) then
            FCryptPlugin.AfterDisconnect(FConnID);

          TriggerDisconnect;
          end;
        end;

    finally
      try
        if assigned(myParent) and not myParent.Silent then
          TriggerConnectionLost;
      except
        on E:Exception do
          if LOG_EVENT_ERRORS then
            Log('Server.OnSessionClosed.TriggerConnectionLost',E);
        end;

      State:=conInactive;

      { We need to remove all events from this connection
        before we can actually destroy our own connection object. }
      with Conn do
        begin
        OnBgException:=nil;
        OnChangeState:=nil;

        OnDataReceived:=nil;
        OnDataSent:=nil;
        OnDataOut:=nil;
        OnDataIn:=nil;
        end;

      myCon:=Conn;
      Conn:=nil;

      try
        MyCon.Close;
      except
        on E:Exception do
          if LOG_SOCKET_ERRORS then
            Log('SesClosed MyCon.Close',E);
        end;
      try
        MyCon.Release;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('SesClosed MyCon.Release',E);
        end;
      end;

    if not Silent then
      if assigned(Client_Thread) then
        TRtcThread.PostJob(Client_Thread, Message_WSRelease)
      else
        Free;
    end;
  end;

procedure TRtcWSockServerProvider.wsOnDataReceived(Sender: TObject; ErrCode: Word);
  var
    len:integer;
    s_out:string;
  begin
  if _Visible then
    begin
    if (State=conListening) and (Proto=proUDP) then // UDP Server
      begin
      FPeerPort:='';
      FPeerAddr:='';

      len:=Conn.GetRcvdCount;
      if len>=0 then
        begin
        SetLength(FReadBuff,len);
        len:=Conn.Receive(FReadBuff[1], length(FReadBuff));

        FPeerPort:=Conn.GetSrcPort;
        FPeerAddr:=Conn.GetSrcAddr;

        if len<0 then
          begin
          FReadBuff:='';
          TriggerDataLost;
          TriggerReadyToRelease;
          end
        else
          begin
          if len<>length(FReadBuff) then
            SetLength(FReadBuff,len);
          TriggerDataReceived;
          TriggerReadyToRelease;
          end;
        end
      else
        begin
        FReadBuff:='';
        TriggerDataLost;
        TriggerReadyToRelease;
        end;
      end
    else
      begin
      if State=conActivating then
        begin
        if FLocalAddr<>'0.0.0.0' then
          begin
          State:=conActive;
          FParent.AddClient(self);

          if assigned(FCryptPlugin) then
            begin
            s_out:='';
            FCryptPlugin.AfterConnect(FConnID,s_out);
            if s_out<>'' then
              begin
              DirectWrite(s_out);
              s_out:='';
              end;
            end;
          TriggerConnect;
          end;
        end;

      if State=conActive then
        begin
        TriggerDataReceived;
        TriggerReadyToRelease;
        end;
      end;
    end;
  end;

procedure TRtcWSockServerProvider.wsOnDataSent(Sender: TObject; ErrCode: Word);
  var
    s_out:string;
  begin
  if _Visible then
    begin
    if (State=conListening) and (Proto=proUDP) then
      begin
      TriggerDataSent;
      TriggerReadyToRelease;
      end
    else
      begin
      if State=conActivating then
        begin
        if FLocalAddr<>'0.0.0.0' then
          begin
          State:=conActive;
          FParent.AddClient(self);

          if assigned(FCryptPlugin) then
            begin
            s_out:='';
            FCryptPlugin.AfterConnect(FConnID,s_out);
            if s_out<>'' then
              begin
              DirectWrite(s_out);
              s_out:='';
              end;
            end;
          TriggerConnect;
          end;
        end;

      if State=conActive then
        begin
        TriggerDataSent;
        TriggerReadyToRelease;
        end;
      end;
    end;
  end;

procedure TRtcWSockServerProvider.wsOnBgException(Sender: TObject; E: Exception;
    var CanClose: Boolean);
  begin
  if (E is EClientLimitReached) or
     (E is EThreadLimitReached) then // ignore those exceptions
    CanClose:=False
  else
    begin
    CanClose:=True;
    try
      TriggerException(E);
    except
      on E:Exception do
        if LOG_SOCKET_ERRORS then
          Log('BgExcept Trigger',E);
      // ignore all exceptions here
      end;
    end;
  end;

function TRtcWSockServerProvider.GetParent: TRtcConnectionProvider;
  begin
  Result:=FParent;
  end;

function TRtcWSockServerProvider._Active: boolean;
  begin
  Result:=not Closing and assigned(Conn) and
         (FState in [conActive,conActivating,conListening]);
  end;

function TRtcWSockServerProvider._Visible: boolean;
  begin
  Result:=not Closing and (FState in [conActive,conActivating,conListening]) and
          ((FParent=nil) or not FParent.Silent) and assigned(Conn);
  end;

procedure TRtcWSockServerProvider.Release;
  begin
  if assigned(Client_Thread) then
    TRtcThread.PostJob(Client_Thread, Message_WSRelease)
  else if assigned(Server_Thread) then
    TRtcThread.PostJob(Server_Thread, Message_WSRelease)
  else
    inherited;
  end;

procedure TRtcWSockServerProvider.AddClient(Client: TRtcWSockServerProvider);
  begin
  Enter;
  try
    FClientList.insert(longword(Client),1);
  finally
    Leave;
    end;
  end;

procedure TRtcWSockServerProvider.RemoveClient(Client: TRtcWSockServerProvider);
  begin
  Enter;
  try
    if FClientList.search(longword(Client))>0 then
      FClientList.Remove(longword(Client));
  finally
    Leave;
    end;
  end;

procedure TRtcWSockServerProvider.AddThread(Thr: TRtcThread);
  begin
  Enter;
  try
    FThrList.insert(longword(Thr),1);
  finally
    Leave;
    end;
  end;

procedure TRtcWSockServerProvider.RemoveThread(Thr: TRtcThread);
  begin
  Enter;
  try
    if FThrList.search(longword(Thr))>0 then
      FThrList.Remove(longword(Thr));
  finally
    Leave;
    end;
  end;

{function TRtcWSockServerProvider.Client( a: integer): TRtcWSockServerProvider;
  begin
  Enter;
  try
    if (a>=0) and (a<FClientList.Count) then
      Result:=TRtcWSockServerProvider(FClientList.Items[a])
    else
      Result:=nil;
  finally
    Leave;
    end;
  end;}

function TRtcWSockServerProvider.ClientCount: integer;
  begin
  Enter;
  try
    Result:=FClientList.Count;
  finally
    Leave;
    end;
  end;

procedure TRtcWSockServerProvider.KillThreads;
  var
    Thr:TRtcWSockClientThread;
    i:longword;
  begin
  Enter;
  try
    repeat
      if FThrList.Count>0 then
        begin
        Thr:=TRtcWSockClientThread(FThrList.search_min(i));
        FThrList.Remove(longword(Thr));
        end
      else
        Thr:=nil;
      if assigned(Thr) then
        if Silent then
          TRtcThread.PostJob(Thr, Message_WSRelease_Silent, True)
        else;
          TRtcThread.PostJob(Thr, Message_WSRelease_Normal, True);
      until Thr=nil;
  finally
    Leave;
    end;
  end;

procedure TRtcWSockServerProvider.KillClients;
  var
    cl:TRtcWSockServerProvider;
    i:longword;
  begin
  Enter;
  try
    repeat
      if FClientList.Count>0 then
        begin
        cl:=TRtcWSockServerProvider(FClientList.search_min(i));
        FClientList.Remove(longword(cl));
        end
      else
        cl:=nil;
      if assigned(cl) then
        try
          if assigned(cl.Client_Thread) then
            begin
            if Silent then
              TRtcThread.PostJob(cl.Client_Thread, Message_WSRelease_Silent, True)
            else;
              TRtcThread.PostJob(cl.Client_Thread, Message_WSRelease_Normal, True);
            end
          else
            begin
            cl.Silent:=Silent;
            cl.InternalDisconnect;
            end;
        except
          on E:Exception do
            if LOG_AV_ERRORS then
              Log('KillClients cl.Stop/Disconnect',E);
          end;
      until cl=nil;
  finally
    Leave;
    end;
  end;

procedure TRtcWSockServerProvider.Check;
  var
    addr:string;
  begin
  if assigned(Conn) then
    begin
    addr:=Conn.GetXAddr;
    if addr='0.0.0.0' then
      begin
      if LOG_SOCKET_ERRORS then
        Log('CLOSING from Check. Socket not connected to local address.');
      Conn.Close;
      raise EWinSockException.Create('Socket not connected to local address.');
      end;
    addr:=Conn.GetPeerAddr;
    if addr='0.0.0.0' then
      begin
      if LOG_SOCKET_ERRORS then
        Log('CLOSING from Check. Socket not connected to peer address.');
      Conn.Close;
      raise EWinSockException.Create('Socket not connected to peer address.');
      end;
    end;
  end;

function TRtcWSockServerProvider.GetClientThread: TRtcThread;
  begin
  Result:=Client_Thread;
  end;

function TRtcWSockServerProvider.GetServerThread: TRtcThread;
  begin
  Result:=Server_Thread;
  end;

procedure TRtcWSockServerProvider.StartListener;
  var
    MyCon:TWSocketServer;
    MyPort:string;
  begin
  if (State=conListening) or (State=conActivating) then
    Exit; // already listening !!!

  if State<>conInactive then
    raise Exception.Create('Can not start listener again. Connection in use.');

  if assigned(Conn) then
    Error('Can not start listener. Connection in use.');

  try
    if Proto=proUDP then
      FReadBuff:='';

    FListenerUp:=False;
    Closing:=False;
    Silent:=False;
    Lost:=True;

    MyPort:=Trim(GetPort);
    if length(MyPort)=0 then
      Error('Port undefined.');

    State:=conActivating;
    try
      if assigned(Server_Thread) then
        begin
        Conn:=TRtcWSocketServer.Create(nil);
        TRtcWSocketServer(Conn).Thr:=Server_Thread;
        end
      else
        Conn:=TWSocketServer.Create(nil);

      with Conn as TWSocketServer do
        begin
        case Proto of
          proTCP:Protocol:=spTcp;
          proUDP:
            begin
            Protocol:=spUdp;
            UdpMultiCast:=Self.UdpMultiCast;
            UdpMultiCastAddrStr:=Self.UdpMultiCastAddr;
            UdpReuseAddr:=Self.UdpReuseAddr;

            {$IFDEF FPC}
            OnDataReceived:=@wsOnDataReceived;
            OnDataSent:=@wsOnDataSent;
            OnDataOut:=@wsOnDataOut;
            OnDataIn:=@wsOnDataIn;
            {$ELSE}
            OnDataReceived:=wsOnDataReceived;
            OnDataSent:=wsOnDataSent;
            OnDataOut:=wsOnDataOut;
            OnDataIn:=wsOnDataIn;
            {$ENDIF}
            end;
          end;

        if self.GetAddr='' then
          Addr:='0.0.0.0'
        else
          Addr:=self.GetAddr;

        MultiThreaded:=assigned(Server_Thread);

        Port:=MyPort;

        {$IFDEF FPC}
        OnBgException:=@wsOnBgException;
        OnChangeState:=@wsOnChangeState;
        OnSessionAvailable:=@wsOnSessionAvailable;
        {$ELSE}
        OnBgException:=wsOnBgException;
        OnChangeState:=wsOnChangeState;
        OnSessionAvailable:=wsOnSessionAvailable;
        {$ENDIF}
        end;

      State:=conListening;

      Conn.Listen;

    except
      on E:Exception do
        begin
        State:=conInactive;
        try
          if assigned(Conn) then
            begin
            MyCon:=Conn as TWSocketServer;
            Conn:=nil;

            with MyCon do
              begin
              OnBgException:=nil;
              OnChangeState:=nil;
              end;

            MyCon.Free;
            end;
        except
          on E:Exception do
            if LOG_AV_ERRORS then
              Log('Listen.except For',E);
          end;
        raise;
        end;
      end;

  except
    on E:EClientLimitReached do // connection limit reached
      begin
      TriggerListenError(E);
      TriggerReadyToRelease;
      end;
    on E:EThreadLimitReached do // connection limit reached
      begin
      TriggerListenError(E);
      TriggerReadyToRelease;
      end;
    on E:EWinSockException do // any kind of socket error
      begin
      TriggerListenError(E);
      TriggerReadyToRelease;
      end;
    on E:Exception do
      begin
      TriggerReadyToRelease;
      raise;
      end;
    end;
  end;

function TRtcWSockServerProvider.PostWrite(HighPriority:boolean=False):boolean;
  begin
  if assigned(Client_Thread) then
    begin
    TRtcThread.PostJob(Client_Thread,Message_WSWrite,HighPriority);
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcWSockServerProvider.PostRead(HighPriority:boolean=False):boolean;
  begin
  if assigned(Client_Thread) then
    begin
    TRtcThread.PostJob(Client_Thread,Message_WSRead,HighPriority);
    Result:=True;
    end
  else
    Result:=False;
  end;

constructor TRtcWSockClientThread.Create;
  begin
  inherited;
  _Silent:=False;
  RtcConn:=nil;
  Par:=nil;
  end;

destructor TRtcWSockClientThread.Destroy;
  begin
  if RTC_LIMIT_CONN then
    rtcCloseAction(self);

  if assigned(Par) then
    Par.RemoveThread(self);

  if assigned(RtcConn) then
    try
      if _Silent then
        begin
        RtcConn.Closing:=True;
        RtcConn.Silent:=True;
        RtcConn.FParent:=nil;
        end
      else
        RtcConn.InternalDisconnect;
      RtcConn.Free;
    except
      on E:Exception do
        if LOG_SOCKET_ERRORS then
          Log('CliThread.Destroy RtcConn.Free',E);
    end;

  try
    if H_Sock<>0 then
      WSocket_closesocket(H_Sock);
  except
    on E:Exception do
      if LOG_SOCKET_ERRORS then
        Log('CliThread.Destroy WSock_Close',E);
    end;

  inherited;
  end;

procedure TRtcWSockClientThread.Init;
  begin
  with RtcConn do
    begin
    Conn := TRtcWSocketClient.Create(nil);
    TRtcWSocketClient(Conn).Thr:=self;
    Conn.MultiThreaded:=True;

    CopyFrom(Par); // initialize connection object
    State:=conActivating;
    Conn.HSocket := H_Sock;
    H_Sock := 0;

    TriggerConnectionAccepted; // if we are over connection limit, EConnectionLimitReached exception will be triggered.
    end;
  end;

function TRtcWSockClientThread.Work(Job: TObject):boolean;
  begin
  Result:=False;
  try
    if Job=Message_WSRead then
      begin
      if not assigned(RtcConn) or
         not assigned(RtcConn.Conn) then Exit;

      if RTC_LIMIT_CONN and not rtcStartAction(self, RTC_ACTION_READ) then
        TRtcThread.PostJob(self,Job,True)
      else
        RtcConn.Conn.Do_FD_READ;
      end
    else if Job=Message_WSWrite then
      begin
      if not assigned(RtcConn) or
         not assigned(RtcConn.Conn) then Exit;

      if RTC_LIMIT_CONN then
        if not RtcConn.Conn.AllSent then // data waiting to be sent
          begin
          if not rtcStartAction(self, RTC_ACTION_WRITE) then
            begin
            TRtcThread.PostJob(self,Job,True);
            Exit;
            end;
          end
        else
          rtcCloseAction(self);

      RtcConn.Conn.Do_FD_WRITE;
      end
    else if Job=Message_WSClose then
      begin
      if not assigned(RtcConn) or
         not assigned(RtcConn.Conn) then Exit;

      if RTC_LIMIT_CONN then
        rtcCloseAction(self);

      RtcConn.Conn.Do_FD_CLOSE(1);
      end
    else if Job=Message_WSInit then
      begin
      if RTC_LIMIT_CONN and not rtcStartAction(self, RTC_ACTION_ACCEPT) then
        TRtcThread.PostJob(self,Job,True)
      else
        Init;
      end
    else if Job=Message_WSRelease_Silent then
      begin
      Par:=nil;
      _Silent:=True;
      Result:=True;

      Free;
      end
    else if Job=Message_WSRelease_Normal then
      begin
      Par:=nil;
      _Silent:=False;
      Result:=True;

      Free;
      end
    else if Job=Message_WSRelease then
      begin
      Result:=True;

      Free;
      end
    else if Job=Message_WSStop then
      begin
      Par:=nil;
      RtcConn:=nil;
      Result:=True;

      Free;
      end
    else if Job is TRtcCloseMessage then
      begin
      try
        if not assigned(RtcConn) or
           not assigned(RtcConn.Conn) then Exit;

        if RTC_LIMIT_CONN then
          rtcCloseAction(self);

        RtcConn.Conn.Do_FD_CLOSE(TRtcCloseMessage(Job).Error);
      finally
        Job.Free;
        end;
      end
    else
      Result:=inherited Work(Job);
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('ClientThread.Work',E);
      raise;
      end;
    end;
  end;

procedure TRtcWSockClientThread.Kill(Job: TObject);
  begin
  if Job is TRtcCloseMessage then
    Job.Free
  else
    inherited Kill(Job);
  end;

{ TRtcWSocketClient }

procedure TRtcWSocketClient.Call_FD_READ;
  begin
  try
    TRtcThread.PostJob(Thr,Message_WSRead);
  except
    on E:Exception do
      if LOG_AV_ERRORS then
        Log('Client.Call_FD_READ',E);
    end;
  end;

procedure TRtcWSocketClient.Call_FD_WRITE;
  begin
  try
    TRtcThread.PostJob(Thr,Message_WSWrite);
  except
    on E:Exception do
      if LOG_AV_ERRORS then
        Log('Client.Call_FD_WRITE',E);
    end;
  end;

procedure TRtcWSocketClient.Call_FD_CLOSE(Err: word);
  var
    cjob:TObject;
  begin
  try
    if Err=0 then
      TRtcThread.PostJob(Thr,Message_WSClose,True,True)
    else
      begin
      cjob:=TRtcCloseMessage.Create(Err);
      if not TRtcThread.PostJob(Thr,cjob,True,True) then
        cjob.Free;
      end;
  except
    on E:Exception do
      if LOG_AV_ERRORS then
        Log('Client.Call_FD_CLOSE',E);
    end;
  end;

{ TRtcInfoMessage }

constructor TRtcInfoMessage.Create(Value: word);
  begin
  inherited Create;
  Error:=Value;
  end;

{ TRtcWSockServerThread }

constructor TRtcWSockServerThread.Create;
  begin
  inherited;
  Releasing:=False;
  RtcConn:=nil;
  end;

destructor TRtcWSockServerThread.Destroy;
  begin
  if assigned(RtcConn) then
    begin
    try
      StopListen;
      if Releasing then
        RtcConn.Free
      else if assigned(RtcConn.Server_Thread) then
        RtcConn.Server_Thread:=nil;
    except
      on E:Exception do
        if LOG_AV_ERRORS then
          Log('WSockServerThread.Destroy',E);
        // ignore exceptions
      end;
    RtcConn:=nil;
    end;
  inherited;
  end;

procedure TRtcWSockServerThread.StartListen;
  begin
  RtcConn.StartListener;
  end;

procedure TRtcWSockServerThread.StopListen;
  begin
  if assigned(RtcConn) then
    begin
    try
      RtcConn.Lost:=False;
      RtcConn.InternalDisconnect;
    except
      on E:Exception do
        if LOG_SOCKET_ERRORS then
          Log('WSockServerThread.StopListen : RtConn.InternalDisconnect',E);
        // ignore exceptions
      end;
    end;
  end;

function TRtcWSockServerThread.Work(Job: TObject):boolean;
  begin
  Result:=False;
  try
    if Job=Message_WSRead then
      begin
      if not assigned(RtcConn) or
         not assigned(RtcConn.Conn) then Exit;

      RtcConn.Conn.Do_FD_READ;
      end
    else if Job=Message_WSWrite then
      begin
      if not assigned(RtcConn) or
         not assigned(RtcConn.Conn) then Exit;

      RtcConn.Conn.Do_FD_WRITE;
      end
    else if Job=Message_WSAccept then
      begin
      if not assigned(RtcConn) or
         not assigned(RtcConn.Conn) then Exit;

      RtcConn.Conn.Do_FD_ACCEPT;
      end
    else if Job=Message_WSInit then
      StartListen
    else if Job=Message_WSCloseConn then
      StopListen
    else if Job=Message_WSRelease then
      begin
      Releasing:=True;
      Result:=True;

      Free;
      end
    else if Job=Message_WSStop then
      begin
      RtcConn:=nil;
      Result:=True;

      Free;
      end
    else
      Result:=inherited Work(Job);
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('ServerThread.Work',E);
      raise;
      end;
    end;
  end;

procedure TRtcWSockServerThread.Kill(Job: TObject);
  begin
  inherited Kill(Job);
  end;

{ TRtcWSocketServer }

procedure TRtcWSocketServer.Call_FD_ACCEPT;
  begin
  try
    TRtcThread.PostJob(Thr,Message_WSAccept);
  except
    on E:Exception do
      if LOG_SOCKET_ERRORS then
        Log('Server.Call_FD_ACCEPT',E);
    end;
  end;

procedure TRtcWSocketServer.Call_FD_READ;
  begin
  try
    TRtcThread.PostJob(Thr,Message_WSRead);
  except
    on E:Exception do
      if LOG_SOCKET_ERRORS then
        Log('Server.Call_FD_READ',E);
    end;
  end;

procedure TRtcWSocketServer.Call_FD_WRITE;
  begin
  try
    TRtcThread.PostJob(Thr,Message_WSWrite);
  except
    on E:Exception do
      if LOG_SOCKET_ERRORS then
        Log('Server.Call_FD_WRITE',E);
    end;
  end;

initialization
Message_WSAccept:=TRtcBaseMessage.Create;
Message_WSInit:=TRtcBaseMessage.Create;
Message_WSStop:=TRtcBaseMessage.Create;
Message_WSRead:=TRtcBaseMessage.Create;
Message_WSWrite:=TRtcBaseMessage.Create;
Message_WSClose:=TRtcBaseMessage.Create;
Message_WSCloseConn:=TRtcBaseMessage.Create;
Message_WSRelease:=TRtcBaseMessage.Create;
Message_WSRelease_Silent:=TRtcBaseMessage.Create;
Message_WSRelease_Normal:=TRtcBaseMessage.Create;

finalization
Garbage(Message_WSAccept);
Garbage(Message_WSInit);
Garbage(Message_WSStop);
Garbage(Message_WSRead);
Garbage(Message_WSWrite);
Garbage(Message_WSClose);
Garbage(Message_WSCloseConn);
Garbage(Message_WSRelease);
Garbage(Message_WSRelease_Silent);
Garbage(Message_WSRelease_Normal);
end.
