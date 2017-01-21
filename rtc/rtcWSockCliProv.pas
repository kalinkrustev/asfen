{
  "Client Connection Provider (WinSock)" - Copyright (c) Danijel Tkalcec
  @html(<br>)

  Client connection provider implementation using a modified
  TWSocket class from F.Piette's Internet Component Suite (ICS).

  @exclude
}
unit rtcWSockCliProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  rtcTrashcan,
  
  Classes,
  SysUtils,

  // Windows, Messages, // Windows and Messages units are used only in Multithreading, to send messages

  rtcSyncObjs,
{$IFDEF CLR}
  DTkalcec.Ics.WSocket,
{$ELSE}
  WSocket_rtc, // Client Socket
{$ENDIF}

  rtcLog,
  rtcConnProv, // Basic connection provider wrapper
  rtcConnLimit,

  rtcPlugins,
  rtcThrPool,
  rtcThrConnProv; // Threaded connection provider wrapper

type
  TRtcWSockClientProvider = class;

  TRtcWSockClientProtocol = (proTCP, proUDP);

  TRtcWSockClientThread = class(TRtcThread)
  public
    RtcConn: TRtcWSockClientProvider;
    Releasing: boolean;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure OpenConn;
    procedure CloseConn(_lost:boolean);

    function Work(Job:TObject):boolean; override;
    procedure Kill(Job:TObject); override;
    end;

  TRtcWSocketClient = class(TWSocketClient)
  public
    Thr: TRtcWSockClientThread;

    procedure Call_FD_CONNECT(Err:word); override;
    procedure Call_FD_CLOSE(Err:word); override;
    procedure Call_FD_READ; override;
    procedure Call_FD_WRITE; override;
    end;

  TRtcWSockClientProvider = class(TRtcThrClientProvider)
  private
    FConnID:longint;

    Conn:TWSocket;

    FProtocol: TRtcWSockClientProtocol;

    FRawOut,
    FPlainOut:int64;

    FCryptPlugin: TRtcCryptPlugin;

    FReadBuff:string;

    FCS:TRtcCritSec;

    Client_Thread : TRtcWSockClientThread;

    FMultiCast          : Boolean;
    FMultiCastIpTTL     : Integer;
    FReuseAddr          : Boolean;

    procedure wsOnBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure wsOnChangeState(Sender: TObject; OldState,NewState: TSocketState);

    procedure wsOnSessionClosed(Sender: TObject; ErrorCode: Word);

    procedure wsOnDataReceived(Sender: TObject; ErrCode: Word);
    procedure wsOnDataSent(Sender: TObject; ErrCode: Word);
    procedure wsOnDataOut(Sender: TObject; Len: cardinal);
    procedure wsOnDataIn(Sender: TObject; Len: cardinal);

    procedure OpenConnection(Force:boolean);

  protected

    procedure Enter; override;
    procedure Leave; override;

    function _Active:boolean;
    function _Visible:boolean;

    function PostWrite(HighPriority:boolean=False):boolean;
    function PostRead(HighPriority:boolean=False):boolean;

    function GetClientThread:TRtcThread; override;

    procedure DirectWrite(const s:string);
    procedure BufferWrite(const s:string);

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Release; override;

    procedure Connect(Force:boolean=False); override;

    procedure InternalDisconnect; override;
    procedure Disconnect; override;

    procedure Check; override;

    function Read: string; override;
    procedure Write(const s: string; sendNow:boolean=True); override;

    property Proto:TRtcWSockClientProtocol read FProtocol write FProtocol;

    property UdpMultiCast       : Boolean           read  FMultiCast
                                                    write FMultiCast;
    property UdpMultiCastMaxHops: Integer           read  FMultiCastIpTTL
                                                    write FMultiCastIpTTL;
    property UdpReuseAddr       : Boolean           read  FReuseAddr
                                                    write FReuseAddr;
    property CryptPlugin        : TRtcCryptPlugin   read FCryptPlugin
                                                    write FCryptPlugin;
    end;

implementation

{$IFDEF CLR}
uses
  System.Security;
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
  TRtcConnectMessage=class(TRtcInfoMessage)
    end;

var
  Message_WSStop,
  Message_WSRelease,
  Message_WSOpenConn,
  Message_WSCloseConn,
  Message_WSConnect,
  Message_WSClose,
  Message_WSRead,
  Message_WSWrite:TRtcBaseMessage;

{ TRtcWSockClientProvider }

constructor TRtcWSockClientProvider.Create;
  begin
  inherited;

  FRawOut:=0;
  FPlainOut:=0;

  FConnID:=GetNextConnID;

  FCS:=TRtcCritSec.Create;

  Closing:=False;

  FPeerPort:='';
  FPeerAddr:='0.0.0.0';
  FLocalPort:='';
  FLocalAddr:='0.0.0.0';

  FProtocol:=proTCP;
  UdpMultiCastMaxHops:=1;

  FReadBuff:='';
  SetLength(FReadBuff, WSOCK_READ_BUFFER_SIZE);

  Conn:=nil;
  end;

destructor TRtcWSockClientProvider.Destroy;
  begin
  { Before destroying this connection object,
    we will disconnect this and all related open connections. }

  Closing:=True;
  Silent:=True;

  if assigned(Conn) then
    InternalDisconnect;

  TriggerConnectionClosing;

  if assigned(Client_Thread) then
    TRtcThread.PostJob(Client_Thread, Message_WSStop, True);

  SetLength(FReadBuff,0);
  FCS.Free;

  inherited;
  end;

procedure TRtcWSockClientProvider.Enter;
  begin
  FCS.Enter;
  end;

procedure TRtcWSockClientProvider.Leave;
  begin
  FCS.Leave;
  end;

function TRtcWSockClientProvider.Read: string;
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

procedure TRtcWSockClientProvider.Write(const s: string; SendNow:boolean=True);
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

procedure TRtcWSockClientProvider.DirectWrite(const s: string);
  var
    len:integer;
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
  else if len<length(s) then
    Error('Wanted to write '+IntToStr(length(s))+' bytes, written only '+IntToStr(len)+' bytes.');
  end;

procedure TRtcWSockClientProvider.BufferWrite(const s: string);
  var
    len:integer;
  begin
  if assigned(FCryptPlugin) then
    Inc(FRawOut, length(s));
  len:=Conn.BuffStr(s);
  if len<0 then
    Error('Error #'+IntToStr(Conn.LastError)+': '+WSocketErrorDesc(Conn.LastError))
  else if len<length(s) then
    Error('Wanted to write '+IntToStr(length(s))+' bytes, written only '+IntToStr(len)+' bytes.');
  end;

procedure TRtcWSockClientProvider.wsOnChangeState(Sender: TObject; OldState, NewState: TSocketState);
  var
    s_out:string;
  begin
  if Closing then Exit;

  if assigned(Conn) then
    if NewState=wsConnected then
      begin
      if Proto=proTCP then
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
      else
        begin
        FLocalAddr:='127.0.0.1';
        FLocalPort:=Conn.GetXPort;
        FPeerAddr:=Conn.GetPeerAddr;
        FPeerPort:=Conn.GetPeerPort;
        TriggerConnecting;

        State:=conActive;

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
      end
    else if NewState=wsClosed then
      wsOnSessionClosed(Sender, 0);
  end;

procedure TRtcWSockClientProvider.wsOnSessionClosed(Sender: TObject; ErrorCode:Word);
  begin
  { Client connection closed.

    This method is called when one of the active connections get closed.
    It handles connections closing for all active connection types
    (incomming and outgoing connections). }

  TriggerDisconnecting;

  if assigned(Conn) and not Closing then // Connection object still here ?
    begin
    Closing:=True; // Let everyone know we are closing now ...

    try
      TriggerConnectionClosing;

      if State in [conInactive,conActivating] then // Connection still not activated,
        TriggerConnectFail // we have a "Failed connection" here, rather then a Disconnect.
      else
        begin
        if assigned(FCryptPlugin) then
          FCryptPlugin.AfterDisconnect(FConnID);

        TriggerDisconnect;
        if Lost then
          TriggerConnectLost;
        end;
    finally

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

      try
        Conn.Close;
      except
        on E:Exception do
          if LOG_SOCKET_ERRORS then
            Log('WSockClientProvider.OnSessionClosed: Conn.Close',E); // ignore all errors here
        end;
      try
        Conn.Release;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('WSockClientProvider.OnSessionClosed: Conn.Release',E); // ignore all errors here
        end;
      Conn:=nil;

      TriggerReadyToRelease;
      end;
    end;
  end;

procedure TRtcWSockClientProvider.wsOnDataReceived(Sender: TObject; ErrCode: Word);
  var
    len:integer;
    s_out:string;
  begin
  if _Visible then
    begin
    if (State=conActivating) then // call "Connected" only after we know that we can relly send data.
      begin
      if FLocalAddr<>'0.0.0.0' then
        begin
        State:=conActive;

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
      if Proto=proUDP then
        begin
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
        TriggerDataReceived;
        TriggerReadyToRelease;
        end;
      end;
    end;
  end;

procedure TRtcWSockClientProvider.wsOnDataSent(Sender: TObject; ErrCode: Word);
  var
    s_out:string;
  begin
  if _Visible then
    begin
    if (State=conActivating) then // call "Connected" only after we know that we can relly send data.
      begin
      if FLocalAddr<>'0.0.0.0' then
        begin
        State:=conActive;

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

    if State=conActive then // do not call this when it comes for the first time, if we haven't been sending anything out yet.
      begin
      TriggerDataSent;
      TriggerReadyToRelease;
      end;
    end;
  end;

procedure TRtcWSockClientProvider.wsOnDataOut(Sender: TObject; Len: cardinal);
  begin
  if _Visible then
    begin
    if State=conActive then
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

procedure TRtcWSockClientProvider.wsOnDataIn(Sender: TObject; Len: cardinal);
  begin
  if _Visible then
    begin
    if State=conActive then
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

procedure TRtcWSockClientProvider.wsOnBgException(Sender: TObject; E: Exception;
    var CanClose: Boolean);
  begin
  if (E is EClientLimitReached) then
    CanClose:=False
  else
    begin
    CanClose:=True;
    try
      TriggerException(E);
    except
      on E:Exception do
        if LOG_EVENT_ERRORS then
          Log('WSockClientProvider.OnBgException: TriggerException',E);
        // ignore all exceptions here
      end;
    end;
  end;

function TRtcWSockClientProvider._Active: boolean;
  begin
  Result:=not Closing and (FState in [conActive,conActivating]) and assigned(Conn);
  end;

function TRtcWSockClientProvider._Visible: boolean;
  begin
  Result:=not Closing and (FState in [conActive,conActivating]) and assigned(Conn);
  end;

procedure TRtcWSockClientProvider.Check;
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

function TRtcWSockClientProvider.GetClientThread: TRtcThread;
  begin
  Result:=Client_Thread;
  end;

procedure TRtcWSockClientProvider.Connect(Force:boolean=False);
  begin
  if assigned(Client_Thread) then
    TRtcThread.PostJob(Client_Thread, Message_WSOpenConn)
  else if GetMultiThreaded then
    begin
    Client_Thread := TRtcWSockClientThread.Create;
    Client_Thread.RtcConn:= self;
    TRtcThread.PostJob(Client_Thread, Message_WSOpenConn);
    end
  else
    OpenConnection(Force);
  end;

procedure TRtcWSockClientProvider.OpenConnection(Force:boolean);
  begin
  if (State=conActive) or (State=conActivating) then Exit; // already connected !!!

  if State<>conInactive then
    raise Exception.Create('Can not connect again. Connection in use.');

  try
    if Proto=proUDP then
      FReadBuff:='';

    Lost:=True;
    Closing:=False;
    Silent:=False;
    FDataOut:=0;
    FDataIn:=0;
    FLocalAddr:='0.0.0.0';
    FPeerAddr:='0.0.0.0';

    TriggerConnectionOpening(Force);

    try
      if assigned(Client_Thread) then
        begin
        Conn:=TRtcWSocketClient.Create(nil);
        TRtcWSocketClient(Conn).Thr:=Client_Thread;
        end
      else
        Conn:=TWSocket.Create(nil);

      with Conn do
        begin
        case Proto of
          proTCP:Protocol:=spTcp;
          proUDP:
            begin
            Protocol:=spUdp;
            UdpMultiCast:=Self.UdpMultiCast;
            UdpMultiCastIpTTL:=Self.UdpMultiCastMaxHops;
            UdpReuseAddr:=Self.UdpReuseAddr;
            end;
          end;

        Addr:=self.GetAddr;
        Port:=self.GetPort;

        MultiThreaded:=assigned(Client_Thread);

        {$IFDEF FPC}
        OnBgException:=@wsOnBgException;
        OnChangeState:=@wsOnChangeState;

        OnDataReceived:=@wsOnDataReceived;
        OnDataSent:=@wsOnDataSent;
        OnDataOut:=@wsOnDataOut;
        OnDataIn:=@wsOnDataIn;
        {$ELSE}
        OnBgException:=wsOnBgException;
        OnChangeState:=wsOnChangeState;

        OnDataReceived:=wsOnDataReceived;
        OnDataSent:=wsOnDataSent;
        OnDataOut:=wsOnDataOut;
        OnDataIn:=wsOnDataIn;
        {$ENDIF}
        end;

      try
        State:=conActivating;
        Conn.Connect;
      except
        on E:Exception do
          begin
          if _Active then
            begin
            State:=conInactive;
            try
              with Conn do
                begin
                OnBgException:=nil;

                OnChangeState:=nil;

                OnDataReceived:=nil;
                OnDataSent:=nil;
                OnDataOut:=nil;
                OnDataIn:=nil;
                end;
              Conn.Free;
            finally
              Conn:=nil;
              end;
            end;
          raise;
          end;
        end;
    except
      TriggerConnectionClosing;
      raise;
      end;
  except
    on E:EClientLimitReached do // connection limit reached
      begin
      TriggerConnectError(E);
      TriggerReadyToRelease;
      end;
    on E:EWinSockException do // any kind of socket error
      begin
      TriggerConnectError(E);
      TriggerReadyToRelease;
      end;
    on E:Exception do
      begin
      TriggerReadyToRelease;
      raise;
      end;
    end;
  end;

procedure TRtcWSockClientProvider.Disconnect;
  begin
  if assigned(Client_Thread) then
    TRtcThread.PostJob(Client_Thread, Message_WSCloseConn)
  else
    begin
    Lost:=False;
    InternalDisconnect;
    end;
  end;

procedure TRtcWSockClientProvider.InternalDisconnect;
  var
    s_out:string;
  begin
  if not assigned(Conn) then // not connected
    begin
    Closing:=True;
    Exit; // silent exit if nothing to do.
    end;

  if State in [conActive,conActivating] then
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
      try
        Conn.Close;
      except
        on E:Exception do
          if LOG_SOCKET_ERRORS then
            Log('WSockClientProvider.InternalDisconnect: Conn.Close',E); // ignore all errors here
        end;
      try
        Conn.Release;
      except
        on E:Exception do
          if LOG_SOCKET_ERRORS then
            Log('WSockClientProvider.InternalDisconnect: Conn.Release',E); // ignore all errors here
        end;
      Conn:=nil;
      end;
    end;
  end;

procedure TRtcWSockClientProvider.Release;
  begin
  if assigned(Client_Thread) then
    TRtcThread.PostJob(Client_Thread, Message_WSRelease, True)
  else
    inherited;
  end;

function TRtcWSockClientProvider.PostWrite(HighPriority:boolean=False):boolean;
  begin
  if assigned(Client_Thread) then
    begin
    TRtcThread.PostJob(Client_Thread,Message_WSWrite,HighPriority);
    Result:=True;
    end
  else
    Result:=False;
  end;

function TRtcWSockClientProvider.PostRead(HighPriority:boolean=False):boolean;
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
  Releasing:=False;
  RtcConn:=nil;
  end;

procedure TRtcWSockClientThread.OpenConn;
  begin
  RtcConn.OpenConnection(False);
  end;

procedure TRtcWSockClientThread.CloseConn(_lost:boolean);
  begin
  if RTC_LIMIT_CONN then
    rtcCloseAction(self);

  if assigned(RtcConn) then
    begin
    try
      RtcConn.Lost:=_lost;
      if not Releasing then
        RtcConn.InternalDisconnect;
    except
      on E:Exception do
        if LOG_SOCKET_ERRORS then
          Log('WSockClientThread.CloseConn : RtConn.InternalDisconnect',E);
        // ignore exceptions
      end;
    end;
  end;

destructor TRtcWSockClientThread.Destroy;
  begin
  CloseConn(false);
  if assigned(RtcConn) then
    begin
    try
      if Releasing then
        RtcConn.Free
      else if assigned(RtcConn.Client_Thread) then
        RtcConn.Client_Thread:=nil;
    finally
      RtcConn:=nil;
      end;
    end;
  inherited;
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
    else if Job=Message_WSConnect then
      begin
      if not assigned(RtcConn) or
         not assigned(RtcConn.Conn) then Exit;

      if RTC_LIMIT_CONN then rtcCloseAction(self);
      RtcConn.Conn.Do_FD_CONNECT(0);
      end
    else if Job=Message_WSClose then
      begin
      if not assigned(RtcConn) or
         not assigned(RtcConn.Conn) then Exit;

      if RTC_LIMIT_CONN then
        rtcCloseAction(self);
      RtcConn.Conn.Do_FD_CLOSE(0);
      end
    else if Job=Message_WSOpenConn then
      begin
      if RTC_LIMIT_CONN and not rtcStartAction(self, RTC_ACTION_CONNECT) then
        TRtcThread.PostJob(self,job,True)
      else
        OpenConn;
      end
    else if Job=Message_WSCloseConn then
      begin
      CloseConn(false);
      end
    else if Job=Message_WSStop then
      begin
      RtcConn:=nil;
      Result:=True;

      Free;
      end
    else if Job=Message_WSRelease then
      begin
      Releasing:=True;
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
    else if Job is TRtcConnectMessage then
      begin
      try
        if not assigned(RtcConn) or
           not assigned(RtcConn.Conn) then Exit;

        if RTC_LIMIT_CONN then
          rtcCloseAction(self);

        RtcConn.Conn.Do_FD_CONNECT(TRtcConnectMessage(Job).Error);
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
        Log('WSockClientThread.Work',E);
      try
        CloseConn(true);
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('WSockClientThread.Wor: CloseConn()',E);
        end;
      // raise;
      end;
    end;
  end;

procedure TRtcWSockClientThread.Kill(Job: TObject);
  begin
  if (Job is TRtcCloseMessage) or
     (Job is TRtcConnectMessage) then
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
      if LOG_SOCKET_ERRORS then
        Log('WSockClient.Call_FD_READ',E);
    end;
  end;

procedure TRtcWSocketClient.Call_FD_WRITE;
  begin
  try
    TRtcThread.PostJob(Thr,Message_WSWrite);
  except
    on E:Exception do
      if LOG_SOCKET_ERRORS then
        Log('WSockClient.Call_FD_WRITE',E);
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
      if LOG_SOCKET_ERRORS then
        Log('WSockClient.Call_FD_CLOSE',E);
    end;
  end;

procedure TRtcWSocketClient.Call_FD_CONNECT(Err: word);
  var
    cjob:TObject;
  begin
  try
    if Err=0 then
      TRtcThread.PostJob(Thr,Message_WSConnect)
    else
      begin
      cjob:=TRtcConnectMessage.Create(Err);
      if not TRtcThread.PostJob(Thr,cjob) then
        cjob.Free;
      end;
  except
    on E:Exception do
      if LOG_SOCKET_ERRORS then
        Log('WSockClient.Call_FD_CONNECT',E);
    end;
  end;

{ TRtcInfoMessage }

constructor TRtcInfoMessage.Create(Value: word);
  begin
  inherited Create;
  Error:=Value;
  end;

initialization
Message_WSOpenConn:=TRtcBaseMessage.Create;
Message_WSCloseConn:=TRtcBaseMessage.Create;
Message_WSConnect:=TRtcBaseMessage.Create;
Message_WSRead:=TRtcBaseMessage.Create;
Message_WSWrite:=TRtcBaseMessage.Create;
Message_WSClose:=TRtcBaseMessage.Create;
Message_WSStop:=TRtcBaseMessage.Create;
Message_WSRelease:=TRtcBaseMessage.Create;

finalization
Garbage(Message_WSOpenConn);
Garbage(Message_WSCloseConn);
Garbage(Message_WSConnect);
Garbage(Message_WSRead);
Garbage(Message_WSWrite);
Garbage(Message_WSClose);
Garbage(Message_WSStop);
Garbage(Message_WSRelease);
end.
