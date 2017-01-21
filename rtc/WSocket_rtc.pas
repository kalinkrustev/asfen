{
  "Low-level WinSock class"

  Copyright (C) 1996-2001 by François PIETTE
  > TCustomWSocket class v4.34 from Sep 08, 2001.

  Copyright (c) 2004-2007 by Danijel Tkalcec
  > TCustomWSocket class redesigned
      - made the class thread-safe
      - improved error control
      - using RTC Thread pool
      - using RTC Windows Handle pool
      - using RTC Timer pool
      - using RTC in-memory List
      - using RTC synchronization classes
      - using RTC logging
      - and a lot more
  
  @exclude
}

{$INCLUDE rtcDefs.inc}

{$IFDEF FPC}
{$PACKRECORDS 1}
{$ENDIF}

unit WSocket_rtc;

{$H+}

{$IFDEF FPC}
  {$B-}           { Enable partial boolean evaluation   }
  {$T-}           { Untyped pointers                    }
  {$X+}           { Enable extended syntax              }
{$ENDIF}

interface

uses
  rtcTrashcan,
  
{$IFNDEF FPC}
  Messages,
{$ENDIF}
  Windows,
  Classes, SysUtils,

  memXList,

  rtcSyncObjs,
  rtcLog,
  rtcTimer,
  rtcSocketPool,
  rtcHWndPool,

  rtcThrPool;

var
  LOG_SOCKET_ERRORS:boolean=False;
  LOG_MESSAGE_ERRORS:boolean=False;
  LOG_AV_ERRORS:boolean=False;
  LOG_EVENT_ERRORS:boolean=False;

const
  WSOCK_PACKET_SIZE=1460;

  // Buffer used by WinSock to buffer outgoing data
  WSOCK_SEND_BUFFER_SIZE:integer=WSOCK_PACKET_SIZE*44;
  // Buffer used by WinSock to buffer incoming data
  WSOCK_READ_BUFFER_SIZE:integer=WSOCK_PACKET_SIZE*44;

  // Max packet size sent out at once
  WSOCK_MAX_SEND_SIZE:integer=WSOCK_PACKET_SIZE*44;

  // Maximum socket Receive Timeout in miliseconds
  WSOCK_RECV_TIMEOUT:integer=1000*60*30; // 30 minutes timeout (silly number here, since we want time unlimited)
  // Maximum socket Receive Timeout in miliseconds
  WSOCK_SEND_TIMEOUT:integer=1000*60*30; // 30 minutes timeout (ridicilous number, since we want time unlimited)

const
  SD_RECEIVE     = 0;
  SD_SEND        = 1;
  SD_BOTH        = 2;

  TXPROTO_UDP = 'udp';
  TXPROTO_TCP = 'tcp';
  
const
  SOCKET_ERROR = -1;

  SOCK_STREAM     = 1;               { stream socket }
  SOCK_DGRAM      = 2;               { datagram socket }

  IP_MULTICAST_IF     = 2;           { set/get IP multicast interface   }
  IP_MULTICAST_TTL    = 3;           { set/get IP multicast timetolive  }
  IP_DEFAULT_MULTICAST_TTL   = 1;    { normally limit m'casts to 1 hop  }
  IP_ADD_MEMBERSHIP   = 5;           { add  an IP group membership      }

  IPPROTO_UDP    =  17;             { user datagram protocol }
  IPPROTO_TCP    =   6;             { tcp }
  IPPROTO_IP     =   0;             { dummy for IP }

  TCP_NODELAY     = $0001;
  SO_KEEPALIVE    = $0008;          { keep connections alive }

  AF_INET         = 2;               { internetwork: UDP, TCP, etc. }
  PF_INET         = AF_INET;

  SO_BROADCAST    = $0020;          { permit sending of broadcast msgs }
  SO_LINGER       = $0080;          { linger on close if data present }
  SO_REUSEADDR    = $0004;          { allow local address reuse }
  SO_SNDTIMEO     = $1005;          { socket sending timeout }
  SO_RCVTIMEO     = $1006;          { socket receiving timeout }

  INADDR_ANY       = $00000000;
  INADDR_LOOPBACK  = $7F000001;
  INADDR_BROADCAST = -1;
  INADDR_NONE      = -1;

  WSADESCRIPTION_LEN     =   256;
  WSASYS_STATUS_LEN      =   128;

  FD_READ         = $01;
  FD_WRITE        = $02;
  FD_OOB          = $04;
  FD_ACCEPT       = $08;
  FD_CONNECT      = $10;
  FD_CLOSE        = $20;

{ All Windows Sockets error constants are biased by WSABASEERR from the "normal" }

  WSABASEERR              = 10000;
{ Windows Sockets definitions of regular Microsoft C error constants }
  WSAEINTR                = (WSABASEERR+4);
  WSAEBADF                = (WSABASEERR+9);
  WSAEACCES               = (WSABASEERR+13);
  WSAEFAULT               = (WSABASEERR+14);
  WSAEINVAL               = (WSABASEERR+22);
  WSAEMFILE               = (WSABASEERR+24);
{ Windows Sockets definitions of regular Berkeley error constants }
  WSAEWOULDBLOCK          = (WSABASEERR+35);
  WSAEINPROGRESS          = (WSABASEERR+36);
  WSAEALREADY             = (WSABASEERR+37);
  WSAENOTSOCK             = (WSABASEERR+38);
  WSAEDESTADDRREQ         = (WSABASEERR+39);
  WSAEMSGSIZE             = (WSABASEERR+40);
  WSAEPROTOTYPE           = (WSABASEERR+41);
  WSAENOPROTOOPT          = (WSABASEERR+42);
  WSAEPROTONOSUPPORT      = (WSABASEERR+43);
  WSAESOCKTNOSUPPORT      = (WSABASEERR+44);
  WSAEOPNOTSUPP           = (WSABASEERR+45);
  WSAEPFNOSUPPORT         = (WSABASEERR+46);
  WSAEAFNOSUPPORT         = (WSABASEERR+47);
  WSAEADDRINUSE           = (WSABASEERR+48);
  WSAEADDRNOTAVAIL        = (WSABASEERR+49);
  WSAENETDOWN             = (WSABASEERR+50);
  WSAENETUNREACH          = (WSABASEERR+51);
  WSAENETRESET            = (WSABASEERR+52);
  WSAECONNABORTED         = (WSABASEERR+53);
  WSAECONNRESET           = (WSABASEERR+54);
  WSAENOBUFS              = (WSABASEERR+55);
  WSAEISCONN              = (WSABASEERR+56);
  WSAENOTCONN             = (WSABASEERR+57);
  WSAESHUTDOWN            = (WSABASEERR+58);
  WSAETOOMANYREFS         = (WSABASEERR+59);
  WSAETIMEDOUT            = (WSABASEERR+60);
  WSAECONNREFUSED         = (WSABASEERR+61);
  WSAELOOP                = (WSABASEERR+62);
  WSAENAMETOOLONG         = (WSABASEERR+63);
  WSAEHOSTDOWN            = (WSABASEERR+64);
  WSAEHOSTUNREACH         = (WSABASEERR+65);
  WSAENOTEMPTY            = (WSABASEERR+66);
  WSAEPROCLIM             = (WSABASEERR+67);
  WSAEUSERS               = (WSABASEERR+68);
  WSAEDQUOT               = (WSABASEERR+69);
  WSAESTALE               = (WSABASEERR+70);
  WSAEREMOTE              = (WSABASEERR+71);
  WSAHOST_NOT_FOUND       = (WSABASEERR+1001);
  WSAEDISCON              = (WSABASEERR+101);
  WSASYSNOTREADY          = (WSABASEERR+91);
  WSAVERNOTSUPPORTED      = (WSABASEERR+92);
  WSANOTINITIALISED       = (WSABASEERR+93);
  WSATRY_AGAIN            = (WSABASEERR+1002);
  WSANO_RECOVERY          = (WSABASEERR+1003);
  WSANO_DATA              = (WSABASEERR+1004);
  WSANO_ADDRESS           = WSANO_DATA;

  SOL_SOCKET      = $ffff;          {options for socket level }

  SO_SNDBUF       = $1001;          { send buffer size }
  SO_RCVBUF       = $1002;          { receive buffer size }

  IOCPARM_MASK = $7f;
  IOC_VOID     = $20000000;
  IOC_OUT      = $40000000;
  IOC_IN       = $80000000;
  IOC_INOUT    = (IOC_IN or IOC_OUT);

  FIONREAD     = IOC_OUT or { get # bytes to read }
    ((Longint(SizeOf(Longint)) and IOCPARM_MASK) shl 16) or
    (Longint(Byte('f')) shl 8) or 127;
  FIONBIO      = IOC_IN or { set/clear non-blocking i/o }
    ((Longint(SizeOf(Longint)) and IOCPARM_MASK) shl 16) or
    (Longint(Byte('f')) shl 8) or 126;
  FIOASYNC     = IOC_IN or { set/clear async i/o }
    ((Longint(SizeOf(Longint)) and IOCPARM_MASK) shl 16) or
    (Longint(Byte('f')) shl 8) or 125;

type
  u_int = integer;
  u_char = char;
  u_short = word;
  u_long = dword;
  TSocket = u_int;

  SunB = packed record
    s_b1, s_b2, s_b3, s_b4: u_char;
  end;
  SunW = packed record
    s_w1, s_w2: u_short;
  end;

  in_addr = record
    case integer of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: u_long);
  end;

  PInAddr = ^TInAddr;
  TInAddr = in_addr;
  sockaddr_in = packed record
    case Integer of
      0: (sin_family: u_short;
          sin_port: u_short;
          sin_addr: TInAddr;
          sin_zero: array[0..7] of Char);
      1: (sa_family: u_short;
          sa_data: array[0..13] of Char)
  end;

  PSOCKADDR = ^TSockAddr;

  TSockAddrIn = sockaddr_in;
  TSockAddr = sockaddr_in;

  WSAData = packed record // !!! also WSDATA
    wVersion: Word;
    wHighVersion: Word;
    szDescription: array[0..WSADESCRIPTION_LEN] of Char;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of Char;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PChar;
  end;
  TWSAData = WSAData;

  PServEnt = ^TServEnt;
  servent = packed record
    s_name: PChar;
    s_aliases: ^PChar;
    s_port: Word;
    s_proto: PChar;
  end;
  TServEnt = servent;

  PProtoEnt = ^TProtoEnt;
  protoent = packed record
    p_name: PChar;
    p_aliases: ^Pchar;
    p_proto: Smallint;
  end;
  TProtoEnt = protoent;

  PHostEnt = ^THostEnt;
  hostent = packed record
    h_name: PChar;
    h_aliases: ^PChar;
    h_addrtype: Smallint;
    h_length: Smallint;
    case Byte of
      0: (h_addr_list: ^PChar);
      1: (h_addr: ^PChar)
  end;
  THostEnt = hostent;

  PLinger = ^TLinger;
  linger = packed record
    l_onoff: u_short;
    l_linger: u_short;
  end;
  TLinger = linger;

const
  WM_CLOSE_DELAYED          = WM_USER + 1;
  WM_TSOCKET_CLOSE          = WM_USER + 2;
  WM_WSOCKET_RELEASE        = WM_USER + 3;

  WM_ASYNCSELECT_FIRST      = WM_USER + 4;
  WM_ASYNCSELECT_LAST       = WM_ASYNCSELECT_FIRST + RTC_HWND_MSG_CODES - 1;

  WSA_WSOCKET_TIMEOUT       = 12001;

  winsockdll = 'wsock32.dll';      { 32 bits TCP/IP system DLL }

  winsocket2 = 'ws2_32.dll';       { 32 bits TCP/IP system DLL version 2}

  INVALID_SOCKET = TSocket(NOT(0));

type
  TWndMethod         = procedure(var Message: TMessage) of object;
  EWinSockException   = class(Exception);
  TBgExceptionEvent  = procedure (Sender : TObject;
                                  E : Exception;
                                  var CanClose : Boolean) of object;

  TSocketState       = (wsInvalidState,
                        wsOpened,     wsBound,
                        wsConnecting, wsSocksConnected, wsConnected,
                        wsAccepting,  wsListening,
                        wsClosed);

  TSocketProtocol = (spTcp, spUdp);

  TSocketLingerOnOff = (wsLingerOff, wsLingerOn, wsLingerNoSet);

  TDataReceived     = procedure (Sender: TObject; ErrCode: Word) of object;
  TDataSent          = procedure (Sender: TObject; ErrCode: Word) of object;
  TDataOut           = procedure (Sender: TObject; Len:Cardinal) of object;
  TDataIn           = procedure (Sender: TObject; Len:Cardinal) of object;

  TSessionAvailable  = procedure (Sender: TObject; ErrCode: Word) of object;

  TDnsLookupDone     = procedure (Sender: TObject; ErrCode: Word) of object;
  TChangeState       = procedure (Sender: TObject;
                                 OldState, NewState : TSocketState) of object;

  TCustomWSocket = class(TComponent)
  private
    FProtoStr:string;
    FProto:integer;
    FProtoType:integer;

    FMultiThreaded:boolean;

    FSrc    : TSockAddrIn;
    FSrcLen : Integer;

    FLastError          : Integer;
    FWindowHandle       : HWND;
    FMessageCode        : Cardinal;

    FHSocket            : TSocket;
    FASocket            : TSocket;               { Accepted socket }

    FAddrStr            : String;
    FAddrResolved       : Boolean;
    FAddrFormat         : Integer;
    FAddrAssigned       : Boolean;

    FLocalPortResolved  : Boolean;
    FPortStr            : String;
    FPortAssigned       : Boolean;
    FPortResolved       : Boolean;
    FPortNum            : Integer;

    FLocalPortStr       : String;
    FLocalPortNum       : Integer;
    FLocalAddr          : String;     { IP address for local interface to use }

    FBufList            : TXList;
    FBufSize            : Integer;

    FProtocol           : TSocketProtocol;

    FListenBacklog      : Integer;

    bAllSent            : Boolean;
    FSentOut            : Integer;
    FSentFlag           : Boolean;
    FReadyToSend        : Boolean;
    FDnsLookupHandle    : THandle;

    FMultiCast          : Boolean;
    FMultiCastAddrStr   : String;
    FMultiCastIpTTL     : Integer;
    FReuseAddr          : Boolean;

    FState              : TSocketState;
    FRcvdFlag           : Boolean;
    FSelectEvent        : LongInt;
    FOnSessionAvailable : TSessionAvailable;

    FOnChangeState      : TChangeState;
    FOnDataReceived    : TDataReceived;
    FOnDataSent         : TDataSent;
    FOnDataOut          : TDataOut;
    FOnDataIn           : TDataIn;
    FOnDnsLookupDone    : TDnsLookupDone;
    FOnError            : TNotifyEvent;
    FOnBgException      : TBgExceptionEvent;

    procedure   WndProc(var MsgRec: TMessage); virtual;
    procedure   AllocateSocketHWnd; virtual;
    procedure   DeallocateSocketHWnd; virtual;
    procedure   SocketError(sockfunc: string);

    procedure   WMASyncSelect(var msg: TMessage);

    procedure   ChangeState(NewState : TSocketState);
    procedure   TryToSend; virtual;
    procedure   ASyncReceive(Error : Word);
    procedure   AssignDefaultValue; virtual;
    procedure   InternalClose(bShut : Boolean; Error : Word); virtual;
    procedure   InternalAbort(ErrCode : Word); virtual;

    procedure   SetAddr(InAddr : String);
    function    GetAddr : String;
    procedure   SetRemotePort(sPort : String); virtual;
    function    GetRemotePort : String;

    procedure   SetLocalAddr(sLocalAddr : String);
    procedure   SetLocalPort(sLocalPort : String);
    procedure   BindSocket; virtual;

    function    RealSend(Data : Pointer; Len : Integer) : Integer; virtual;
    procedure   RaiseExceptionFmt(const Fmt : String; args : array of const); virtual;
    procedure   RaiseException(const Msg : String); virtual;

    function    TriggerDataReceived(Error : Word) : Boolean; virtual;
    procedure   TriggerSessionAvailable(Error : Word); virtual;

    procedure   TriggerDataSent(Error : Word); virtual;
    procedure   TriggerDataOut(Len:Cardinal); virtual;
    procedure   TriggerDataIn(Len:Cardinal); virtual;
    procedure   TriggerChangeState(OldState, NewState : TSocketState); virtual;

    procedure   TriggerDNSLookupDone(Error : Word); virtual;

    procedure   TriggerError; virtual;
    function    DoRecv(var Buffer;
                       BufferSize : Integer;
                       Flags      : Integer) : Integer; virtual;

    procedure   DupConnected; virtual;

    procedure   CancelDnsLookup; virtual;

    procedure   SetLingerOption(aborting:boolean=False);

    procedure   PutDataInSendBuffer(Data : Pointer; Len : Integer);
    procedure   DeleteBufferedData;

    procedure   Dup(NewHSocket : TSocket); virtual;
    procedure   Shutdown(How : Integer); virtual;

  public
    sin         : TSockAddrIn;

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   Connect; virtual;
    procedure   Close; virtual;
    procedure   CloseDelayed; virtual;

    procedure   Release; virtual;
    procedure   Abort; virtual;
    procedure   Listen; virtual;
    function    Accept: TSocket; virtual;

    function    GetRcvdCount : LongInt; virtual;
    function    Receive(var Buffer; BufferSize: integer) : integer; virtual;
    function    SendData(Data : Pointer; Len : Integer) : integer; virtual;
    function    ToBuff(Data : Pointer; Len : Integer) : integer; virtual;

    function    ReceiveStr : string; virtual;
    function    SendStr(const Str : String) : Integer; virtual;
    function    BuffStr(const Str : String) : Integer; virtual;

    function    GetPeerAddr: string; virtual;
    function    GetPeerPort: string; virtual;

    function    GetXPort: string; virtual;
    function    GetXAddr: string; virtual;

    function    GetSrcPort: string;
    function    GetSrcAddr: string;

    procedure   SetProtocol(Value:TSocketProtocol);

    procedure   Do_FD_CONNECT(Err:word); virtual;
    procedure   Do_FD_CLOSE(Err:word); virtual;
    procedure   Do_FD_READ; virtual;
    procedure   Do_FD_WRITE; virtual;
    procedure   Do_FD_ACCEPT; virtual;
    procedure   Do_CloseDelayed; virtual;
    procedure   Do_Release; virtual;

    procedure   Call_FD_CONNECT(Err:word); virtual;
    procedure   Call_FD_CLOSE(Err:word); virtual;
    procedure   Call_FD_READ; virtual;
    procedure   Call_FD_WRITE; virtual;
    procedure   Call_FD_ACCEPT; virtual;
    procedure   Call_CloseDelayed; virtual;
    procedure   Call_Release; virtual;

    function GetWindowHandle:HWND;

  protected
    procedure   HandleBackGroundException(E: Exception); virtual;

    property Protocol : TSocketProtocol             read FProtocol
                                                    write SetProtocol;

    property PortNum : Integer                      read  FPortNum;
    property Handle : HWND                          read  GetWindowHandle;
    property MessageCode : Cardinal                 read  FMessageCode;
    property HSocket : TSocket                      read  FHSocket
                                                    write Dup;

    property Addr : string                          read  GetAddr
                                                    write SetAddr;
    property Port : string                          read  GetRemotePort
                                                    write SetRemotePort;

    property LocalPort : String                     read  FLocalPortStr
                                                    write SetLocalPort;
    property LocalAddr : String                     read  FLocalAddr
                                                    write SetLocalAddr;

    property PeerAddr : String                      read  GetPeerAddr;
    property PeerPort : String                      read  GetPeerPort;

    property State : TSocketState                   read  FState;

    property AllSent   : Boolean                    read  bAllSent;

    property LastError : Integer                    read  FLastError;

    property BufSize   : Integer                    read  FBufSize
                                                    write FBufSize;
    property ListenBacklog : Integer                read  FListenBacklog
                                                    write FListenBacklog;

    property UdpMultiCast       : Boolean           read  FMultiCast
                                                    write FMultiCast;
    property UdpMultiCastAddrStr: String            read  FMultiCastAddrStr
                                                    write FMultiCastAddrStr;
    property UdpMultiCastIpTTL  : Integer           read  FMultiCastIpTTL
                                                    write FMultiCastIpTTL;
    property UdpReuseAddr       : Boolean           read  FReuseAddr
                                                    write FReuseAddr;

    property OnDataReceived : TDataReceived       read  FOnDataReceived
                                                    write FOnDataReceived;
    property OnDataSent      : TDataSent            read  FOnDataSent
                                                    write FOnDataSent;
    property OnDataOut       : TDataOut             read  FOnDataOut
                                                    write FOnDataOut;
    property OnDataIn        : TDataIn             read  FOnDataIn
                                                    write FOnDataIn;
    property OnSessionAvailable : TSessionAvailable read  FOnSessionAvailable
                                                    write FOnSessionAvailable;
    property OnChangeState      : TChangeState      read  FOnChangeState
                                                    write FOnChangeState;
    property OnDnsLookupDone    : TDnsLookupDone    read  FOnDnsLookupDone
                                                    write FOnDnsLookupDone;
    property OnError            : TNotifyEvent      read  FOnError
                                                    write FOnError;
    property OnBgException      : TBgExceptionEvent read  FOnBgException
                                                    write FOnBgException;
    property MultiThreaded: boolean                 read FMultiThreaded
                                                    write FMultiThreaded;
  end;

  TWSocket = class(TCustomWSocket)
  public
    property PortNum;
    property Handle;
    property HSocket;
    property BufSize;
    property AllSent;

  published
    property MultiThreaded;

    property Addr;
    property Port;
    property LocalAddr;
    property LocalPort;
    property PeerPort;
    property PeerAddr;
    property State;

    property Protocol;

    property UdpMultiCast;
    property UdpMultiCastAddrStr;
    property UdpMultiCastIpTTL;
    property UdpReuseAddr;

    property LastError;

    property ListenBacklog;

    property OnDataReceived;
    property OnDataSent;
    property OnDataOut;
    property OnDataIn;
    property OnSessionAvailable;

    property OnChangeState;

    property OnDnsLookupDone;
    property OnError;
    property OnBgException;
  end;

  TWSocketServer = class(TWSocket);
  TWSocketClient = class(TWSocket);

procedure WinSockLoad;

function WSocketErrorDesc(error: integer) : string;

function WSocket_closesocket(s: TSocket): Integer;
function WSocket_shutdown(s: TSocket; how: Integer): Integer;

function WSocket_htons(hostshort: u_short): u_short;
function WSocket_ntohs(netshort: u_short): u_short;
function WSocket_ntohl(netlong: u_long): u_long;

implementation

(* Temporary Buffer *)

type
  TBuffer = class(TObject)
    Buf      : Pointer;
    FBufSize : Integer;
    WrCount  : Integer;
    RdCount  : Integer;
  public
    constructor Create(nSize : Integer); virtual;
    destructor  Destroy; override;
    function    Write(Data : Pointer; Len : Integer) : Integer;
    function    Read(Data : Pointer; Len : Integer) : Integer;
    function    Peek(var Len : Integer) : Pointer;
    function    Remove(Len : Integer) : Integer;
    procedure   SetBufSize(newSize : Integer);
    property    BufSize : Integer read FBufSize write SetBufSize;
  end;

constructor TBuffer.Create(nSize : Integer);
begin
    inherited Create;
    WrCount  := 0;
    RdCount  := 0;
    BufSize := nSize;
end;

destructor TBuffer.Destroy;
begin
    if Assigned(Buf) then
      FreeMem(Buf, FBufSize);

    inherited Destroy;
end;

procedure TBuffer.SetBufSize(newSize : Integer);
  var
    newBuf : Pointer;
  begin
  if newSize <= 0 then
    newSize := 1514;

  if newSize = FBufSize then
    Exit;

  if WrCount = RdCount then 
    begin
    { Buffer is empty }
    if Assigned(Buf) then
      FreeMem(Buf, FBufSize);
    FBufSize := newSize;
    if FBufSize>0 then
      GetMem(Buf, FBufSize)
    else
      Buf := nil;
    end
  else 
    begin
    { Buffer contains data }
    if newSize>0 then
      begin
      GetMem(newBuf, newSize);
      if WrCount>0 then
        Move(Buf^, newBuf^, WrCount);
      end
    else
      newBuf := nil;
    if Assigned(Buf) then
      FreeMem(Buf, FBufSize);
    FBufSize := newSize;
    Buf      := newBuf;
    end;
  end;

function TBuffer.Write(Data : Pointer; Len : Integer) : Integer;
var
    Remaining : Integer;
    Copied    : Integer;
begin
    Remaining := FBufSize - WrCount;
    if Remaining <= 0 then
        Result := 0
    else begin
        if Len <= Remaining then
            Copied := Len
        else
            Copied := Remaining;
        if Copied>0 then
          Move(Data^, (PChar(Buf) + WrCount)^, Copied);
        WrCount := WrCount + Copied;
        Result  := Copied;
    end;
end;

function TBuffer.Read(Data : Pointer; Len : Integer) : Integer;
var
    Remaining : Integer;
    Copied    : Integer;
begin
    Remaining := WrCount - RdCount;
    if Remaining <= 0 then
        Result := 0
    else begin
        if Len <= Remaining then
            Copied := Len
        else
            Copied := Remaining;
        if Copied>0 then
          Move((PChar(Buf) + RdCount)^, Data^, Copied);
        RdCount := RdCount + Copied;

        if RdCount = WrCount then begin
            RdCount := 0;
            WrCount := 0;
        end;

        Result := Copied;
    end;
end;

function TBuffer.Peek(var Len : Integer) : Pointer;
var
    Remaining : Integer;
begin
    Remaining := WrCount - RdCount;
    if Remaining <= 0 then begin
        Len    := 0;
        Result := nil;
    end
    else begin
        Len    := Remaining;
        Result := Pointer(PChar(Buf) + RdCount);
    end;
end;

function TBuffer.Remove(Len : Integer) : Integer;
var
    Remaining : Integer;
    Removed   : Integer;
begin
    Remaining := WrCount - RdCount;
    if Remaining <= 0 then
        Result := 0
    else begin
        if Len < Remaining then
            Removed := Len
        else
            Removed := Remaining;
        RdCount := RdCount + Removed;

        if RdCount = WrCount then begin
            RdCount := 0;
            WrCount := 0;
        end;

        Result := Removed;
    end;
end;

(* WinSock stuff *)

const
    SIO_RCVALL = $98000001;

var
    GReqVerLow      : BYTE    = 1;
    GReqVerHigh     : BYTE    = 1;

type
    TWSAStartup            = function (wVersionRequired: word;
                                       var WSData: TWSAData): Integer; stdcall;
    TWSACleanup            = function : Integer; stdcall;
    TWSASetLastError       = procedure (iError: Integer); stdcall;
    TWSAGetLastError       = function : Integer; stdcall;
    TWSACancelAsyncRequest = function (hAsyncTaskHandle: THandle): Integer; stdcall;
    TWSAAsyncGetHostByName = function (HWindow: HWND;
                                       wMsg: u_int;
                                       name, buf: PChar;
                                       buflen: Integer): THandle; stdcall;
    TWSAAsyncGetHostByAddr = function (HWindow: HWND;
                                       wMsg: u_int; addr: PChar;
                                       len, Struct: Integer;
                                       buf: PChar;
                                       buflen: Integer): THandle; stdcall;
    TWSAAsyncSelect        = function (s: TSocket;
                                       HWindow: HWND;
                                       wMsg: u_int;
                                       lEvent: Longint): Integer; stdcall;
    TGetServByName         = function (name, proto: PChar): PServEnt; stdcall;
    TGetProtoByName        = function (name: PChar): PProtoEnt; stdcall;
    TGetHostByName         = function (name: PChar): PHostEnt; stdcall;
    TGetHostByAddr         = function (addr: Pointer; len, Struct: Integer): PHostEnt; stdcall;
    TGetHostName           = function (name: PChar; len: Integer): Integer; stdcall;
    TOpenSocket            = function (af, Struct, protocol: Integer): TSocket; stdcall;
    TShutdown              = function (s: TSocket; how: Integer): Integer; stdcall;
    TSetSockOpt            = function (s: TSocket; level, optname: Integer;
                                       optval: PChar;
                                       optlen: Integer): Integer; stdcall;
    TGetSockOpt            = function (s: TSocket; level, optname: Integer;
                                       optval: PChar;
                                       var optlen: Integer): Integer; stdcall;
    TSendTo                = function (s: TSocket; var Buf;
                                       len, flags: Integer;
                                       var addrto: TSockAddr;
                                       tolen: Integer): Integer; stdcall;
    TSend                  = function (s: TSocket; var Buf;
                                       len, flags: Integer): Integer; stdcall;
    TRecv                  = function (s: TSocket;
                                       var Buf;
                                       len, flags: Integer): Integer; stdcall;
    TRecvFrom              = function (s: TSocket;
                                       var Buf; len, flags: Integer;
                                       var from: TSockAddr;
                                       var fromlen: Integer): Integer; stdcall;
    Tntohs                 = function (netshort: u_short): u_short; stdcall;
    Tntohl                 = function (netlong: u_long): u_long; stdcall;
    TListen                = function (s: TSocket;
                                       backlog: Integer): Integer; stdcall;
    TIoctlSocket           = function (s: TSocket; cmd: DWORD;
                                       var arg: u_long): Integer; stdcall;
    TInet_ntoa             = function (inaddr: TInAddr): PChar; stdcall;
    TInet_addr             = function (cp: PChar): u_long; stdcall;
    Thtons                 = function (hostshort: u_short): u_short; stdcall;
    Thtonl                 = function (hostlong: u_long): u_long; stdcall;
    TGetSockName           = function (s: TSocket; var name: TSockAddr;
                                       var namelen: Integer): Integer; stdcall;
    TGetPeerName           = function (s: TSocket; var name: TSockAddr;
                                       var namelen: Integer): Integer; stdcall;
    TConnect               = function (s: TSocket; var name: TSockAddr;
                                       namelen: Integer): Integer; stdcall;
    TCloseSocket           = function (s: TSocket): Integer; stdcall;
    TBind                  = function (s: TSocket; var addr: TSockAddr;
                                       namelen: Integer): Integer; stdcall;
    TAccept                = function (s: TSocket; addr: PSockAddr;
                                       addrlen: PInteger): TSocket; stdcall;
var
    _WSAStartup            : TWSAStartup;
    _WSACleanup            : TWSACleanup;

    _WSAGetLastError       : TWSAGetLastError;
    _WSACancelAsyncRequest : TWSACancelAsyncRequest;
    _WSAAsyncSelect        : TWSAAsyncSelect;
    _GetServByName         : TGetServByName;
    _GetHostByName         : TGetHostByName;
    _Socket                : TOpenSocket;
    _Shutdown              : TShutdown;
    _SetSockOpt            : TSetSockOpt;
    _GetSockOpt            : TGetSockOpt;
    _SendTo                : TSendTo;
    _Send                  : TSend;
    _Recv                  : TRecv;
    _RecvFrom              : TRecvFrom;
    _ntohs                 : Tntohs;
    _ntohl                 : Tntohl;
    _Listen                : TListen;
    _IoctlSocket           : TIoctlSocket;
    _Inet_ntoa             : TInet_ntoa;
    _Inet_addr             : TInet_addr;
    _htons                 : Thtons;
    _GetSockName           : TGetSockName;
    _GetPeerName           : TGetPeerName;
    _Connect               : TConnect;
    _CloseSocket           : TCloseSocket;
    _Bind                  : TBind;
    _Accept                : TAccept;

// *** API calls not needed ...
//   _WSASetLastError       : TWSASetLastError;
//   _WSAAsyncGetHostByName : TWSAAsyncGetHostByName;
//   _WSAAsyncGetHostByAddr : TWSAAsyncGetHostByAddr;
//   _GetProtoByName        : TGetProtoByName;
//   _GetHostByAddr         : TGetHostByAddr;
//   _GetHostName           : TGetHostName;
//   _htonl                 : Thtonl;

    FDllHandle     : THandle  = 0;
    LibCS: TRtcCritSec;

const
    socksNoError              = 20000;
    socksProtocolError        = 20001;
    socksVersionError         = 20002;
    socksAuthMethodError      = 20003;
    socksGeneralFailure       = 20004;
    socksConnectionNotAllowed = 20005;
    socksNetworkUnreachable   = 20006;
    socksHostUnreachable      = 20007;
    socksConnectionRefused    = 20008;
    socksTtlExpired           = 20009;
    socksUnknownCommand       = 20010;
    socksUnknownAddressType   = 20011;
    socksUnassignedError      = 20012;
    socksInternalError        = 20013;
    socksDataReceiveError     = 20014;
    socksAuthenticationFailed = 20015;
    socksRejectedOrFailed     = 20016;
    socksHostResolutionFailed = 20017;

var
    GInitData      : TWSADATA;

function WSAGetLastError:integer;
  begin
  Result := _WSAGetLastError();
  end;

function atoi(value : string) : Word;
var
    i : Integer;
begin
    Result := 0;
    i := 1;
    while (i <= Length(Value)) and (Value[i] = ' ') do
        i := i + 1;
    while (i <= Length(Value)) and (Value[i] >= '0') and (Value[i] <= '9')do begin
        Result := Result * 10 + ord(Value[i]) - ord('0');
        i := i + 1;
    end;
end;

function IsDigit(Ch : Char) : Boolean;
begin
    Result := (ch >= '0') and (ch <= '9');
end;


function WSocketIsDottedIP(const S : String) : Boolean;
var
    I          : Integer;
    DotCount   : Integer;
    NumVal     : Integer;
begin
    Result     := FALSE;
    DotCount   := 0;
    NumVal     := 0;
    I          := 1;
    { Skip leading spaces }
    while (I <= Length(S)) and (S[I] = ' ') do
        Inc(I);
    { Can't begin with a dot }
    if (I <= Length(S)) and (S[I] = '.') then
        Exit;
    { Scan full string }
    while I <= Length(S) do begin
        if S[I] = '.' then begin
            Inc(DotCount);
            if (DotCount > 3) or (NumVal > 255) then
                Exit;
            NumVal := 0;
            { A dot must be followed by a digit }
            if (I >= Length(S)) or (not (S[I + 1] in ['0'..'9'])) then
                Exit;
        end
        else if S[I] in ['0'..'9'] then
            NumVal := NumVal * 10 + Ord(S[I]) - Ord('0')
        else begin
            { Not a digit nor a dot. Accept spaces until end of string }
            while (I <= Length(S)) and (S[I] = ' ') do
                Inc(I);
            if I <= Length(S) then
                Exit;  { Not a space, do not accept }
            break;     { Only spaces, accept        }
        end;
        Inc(I);
    end;
    { We must have exactly 3 dots }
    if (DotCount <> 3) or (NumVal > 255) then
        Exit;
    Result := TRUE;
end;

function WSocket_closesocket(s: TSocket): Integer;
  begin
  if FDllHandle=0 then
    raise EWinSockException.Create('WinSock not loaded.');
  Result:=_CloseSocket(S);
  end;

function WSocket_shutdown(s: TSocket; how: Integer): Integer;
  begin
  if FDllHandle=0 then
    raise EWinSockException.Create('WinSock not loaded.');
  Result:=_Shutdown(S,how);
  end;

function WSocket_htons(hostshort: u_short): u_short;
  begin
  if FDllHandle=0 then
    raise EWinSockException.Create('WinSock not loaded.');
  Result:=_htons(hostshort);
  end;

function WSocket_ntohs(netshort: u_short): u_short;
  begin
  if FDllHandle=0 then
    raise EWinSockException.Create('WinSock not loaded.');
  Result:=_ntohs(netshort);
  end;

function WSocket_ntohl(netlong: u_long): u_long;
  begin
  if FDllHandle=0 then
    raise EWinSockException.Create('WinSock not loaded.');
  Result:=_ntohl(netlong);
  end;

procedure TCustomWSocket.RaiseException(const Msg : String);
begin
    if Assigned(FOnError) then
        TriggerError
    else
        raise EWinSockException.Create(Msg);
end;

procedure TCustomWSocket.RaiseExceptionFmt(const Fmt : String; args : array of const);
begin
    if Assigned(FOnError) then
        TriggerError
    else
        raise EWinSockException.CreateFmt(Fmt, args);
end;

function WSocketErrorDesc(error: integer) : string;
begin
    case error of
    0,
    WSABASEERR:
      WSocketErrorDesc := 'No Error';
    WSAEINTR:
      WSocketErrorDesc := 'Interrupted system call';
    WSAEBADF:
      WSocketErrorDesc := 'Bad file number';
    WSAEACCES:
      WSocketErrorDesc := 'Permission denied';
    WSAEFAULT:
      WSocketErrorDesc := 'Bad address';
    WSAEINVAL:
      WSocketErrorDesc := 'Invalid argument';
    WSAEMFILE:
      WSocketErrorDesc := 'Too many open files';
    WSAEWOULDBLOCK:
      WSocketErrorDesc := 'Operation would block';
    WSAEINPROGRESS:
      WSocketErrorDesc := 'Operation now in progress';
    WSAEALREADY:
      WSocketErrorDesc := 'Operation already in progress';
    WSAENOTSOCK:
      WSocketErrorDesc := 'Socket operation on non-socket';
    WSAEDESTADDRREQ:
      WSocketErrorDesc := 'Destination address required';
    WSAEMSGSIZE:
      WSocketErrorDesc := 'Message too long';
    WSAEPROTOTYPE:
      WSocketErrorDesc := 'Protocol wrong type for socket';
    WSAENOPROTOOPT:
      WSocketErrorDesc := 'Protocol not available';
    WSAEPROTONOSUPPORT:
      WSocketErrorDesc := 'Protocol not supported';
    WSAESOCKTNOSUPPORT:
      WSocketErrorDesc := 'Socket type not supported';
    WSAEOPNOTSUPP:
      WSocketErrorDesc := 'Operation not supported on socket';
    WSAEPFNOSUPPORT:
      WSocketErrorDesc := 'Protocol family not supported';
    WSAEAFNOSUPPORT:
      WSocketErrorDesc := 'Address family not supported by protocol family';
    WSAEADDRINUSE:
      WSocketErrorDesc := 'Address or Port already in use';
    WSAEADDRNOTAVAIL:
      WSocketErrorDesc := 'Address or Port not available';
    WSAENETDOWN:
      WSocketErrorDesc := 'Network is down';
    WSAENETUNREACH:
      WSocketErrorDesc := 'Network is unreachable';
    WSAENETRESET:
      WSocketErrorDesc := 'Network dropped connection on reset';
    WSAECONNABORTED:
      WSocketErrorDesc := 'Connection aborted';
    WSAECONNRESET:
      WSocketErrorDesc := 'Connection reset by peer';
    WSAENOBUFS:
      WSocketErrorDesc := 'No buffer space available';
    WSAEISCONN:
      WSocketErrorDesc := 'Socket is already connected';
    WSAENOTCONN:
      WSocketErrorDesc := 'Socket is not connected';
    WSAESHUTDOWN:
      WSocketErrorDesc := 'Can''t send after socket shutdown';
    WSAETOOMANYREFS:
      WSocketErrorDesc := 'Too many references: can''t splice';
    WSAETIMEDOUT:
      WSocketErrorDesc := 'Connection timed out';
    WSAECONNREFUSED:
      WSocketErrorDesc := 'Connection refused';
    WSAELOOP:
      WSocketErrorDesc := 'Too many levels of symbolic links';
    WSAENAMETOOLONG:
      WSocketErrorDesc := 'File name too long';
    WSAEHOSTDOWN:
      WSocketErrorDesc := 'Host is down';
    WSAEHOSTUNREACH:
      WSocketErrorDesc := 'No route to host';
    WSAENOTEMPTY:
      WSocketErrorDesc := 'Directory not empty';
    WSAEPROCLIM:
      WSocketErrorDesc := 'Too many processes';
    WSAEUSERS:
      WSocketErrorDesc := 'Too many users';
    WSAEDQUOT:
      WSocketErrorDesc := 'Disc quota exceeded';
    WSAESTALE:
      WSocketErrorDesc := 'Stale NFS file handle';
    WSAEREMOTE:
      WSocketErrorDesc := 'Too many levels of remote in path';
    WSASYSNOTREADY:
      WSocketErrorDesc := 'Network sub-system is unusable';
    WSAVERNOTSUPPORTED:
      WSocketErrorDesc := 'WinSock DLL cannot support this application';
    WSANOTINITIALISED:
      WSocketErrorDesc := 'WinSock not initialized';
    WSAHOST_NOT_FOUND:
      WSocketErrorDesc := 'Host not found';
    WSATRY_AGAIN:
      WSocketErrorDesc := 'Non-authoritative host not found';
    WSANO_RECOVERY:
      WSocketErrorDesc := 'Non-recoverable error';
    WSANO_DATA:
      WSocketErrorDesc := 'No Data';
    else
      WSocketErrorDesc := 'Not a WinSock error';
    end;
end;

procedure TCustomWSocket.AssignDefaultValue;
begin
    FSrcLen:=0;

    FillChar(sin, Sizeof(sin), 0);
    sin.sin_family     := AF_INET;
    FAddrFormat        := PF_INET;

    FPortAssigned      := FALSE;
    FPortResolved      := FALSE;

    FAddrAssigned      := FALSE;
    FAddrResolved      := FALSE;

    FLocalPortResolved := FALSE;

    FLocalPortStr      := '0';
    FLocalAddr         := '0.0.0.0';

    FHSocket           := INVALID_SOCKET;
    FSelectEvent       := 0;
    FState             := wsClosed;

    FSentOut           := 0;
    FSentFlag          := TRUE; // message will be sent from Windows
    FReadyToSend       := FALSE; // we are not yet ready to send
    bAllSent           := TRUE;
end;


{ All exceptions *MUST* be handled. If an exception is not handled, the     }
{ application will be shut down !                                           }
procedure TCustomWSocket.HandleBackGroundException(E: Exception);
var
    CanAbort : Boolean;
begin
    CanAbort := TRUE;
    { First call the error event handler, if any }
    if Assigned(FOnBgException) then begin
        try
          FOnBgException(Self, E, CanAbort);
        except
          on E:Exception do
            Log('FOnBgException',E);
        end;
    end;
    { Then abort the socket }
    if CanAbort then begin
        try
            Abort;
        except
          on E:Exception do
            Log('Abort',E);
        end;
    end;
end;


{ This procedure handle all messages for TWSocket. All exceptions must be   }
{ handled or the application will be shutted down !                         }
{ If WndProc is overriden in descendent components, then the same exception }
{ handling *MUST* be setup because descendent component code is executed    }
{ before the base class code.                                               }
procedure TCustomWSocket.WndProc(var MsgRec: TMessage);
  begin
  with MsgRec do
    begin
    if (Msg >= WM_ASYNCSELECT_FIRST) and (Msg <= WM_ASYNCSELECT_LAST) then
      WMASyncSelect(MsgRec)
    else if Msg = WM_CLOSE_DELAYED then
      Call_CloseDelayed
    else if Msg = WM_WSOCKET_RELEASE then
      Call_Release
    else
      Result := DefWindowProc(Handle, Msg, wParam, LParam);
    end;
  end;


{ This function is a callback function. It means that it is called by       }
{ windows. This is the very low level message handler procedure setup to    }
{ handle the message sent by windows (winsock) to handle messages.          }
{ This message handler works with X connections by using 1 Windows handle   }
function RtcSocketWindowProc(ahWnd   : HWND;
                           auMsg   : LongWord;
                           awParam : WPARAM;
                           alParam : LPARAM): Integer; stdcall;
  var
    Obj    : TObject;
    MsgRec : TMessage;
    Sock   : TSocket;
    iStatus: Integer;
    Hdl: HWND;
    Msg: LongWord;
  begin
  try
    case auMsg of
      WM_TIMER:
        begin
        if (awParam<>0) then
          Obj:=rtcGetTimer(awParam)
        else
          Obj:=nil;

        if (Obj<>nil) and (Obj is TRtcTimer) then
          begin
          try
            TRtcTimer.Timer(Obj);
          except
            on E:Exception do
              Log('WM_TIMER',E);
            end;
          Result := 0;
          end
        else
          Result := DefWindowProc(ahWnd, auMsg, awParam, alParam);
        end;

      WM_TSOCKET_CLOSE:
        begin
        Sock:=awParam;
        try
          iStatus := _closesocket(Sock);
          if iStatus<>0 then
            if WSAGetLastError = WSAEWOULDBLOCK then
              begin
              if LOG_SOCKET_ERRORS then
                Log('WM_TSOCKET_CLOSE: WM_TSOCKET_CLOSE would block.');
              _shutdown(Sock,SD_BOTH);
              if not PostMessage(ahWnd,WM_TSOCKET_CLOSE,Sock,0) then // can not post message?
                _closesocket(Sock);
              end;
        except
          on E:Exception do
            Log('WM_TSOCKET_CLOSE',E);
          end;
        Result:=0;
        end;

      WM_ASYNCSELECT_FIRST .. WM_ASYNCSELECT_LAST:
        begin
        Sock:=awParam;

        Hdl:=0;
        Msg:=0;
        Obj:=nil;

        rtcEnterSocket;
        try
          if (Sock<>0) and (Sock<>INVALID_SOCKET) then
            begin
            Obj:=rtcGetSocket(Sock);
            if Obj<>nil then
              begin
              if Obj is TWSocket then
                begin
                Hdl:=TWSocket(Obj).Handle;
                Msg:=TWSocket(Obj).MessageCode;
                end
              else
                Obj:=nil;
              end;
            end;
        finally
          rtcLeaveSocket;
          end;

        if Obj<>nil then
          begin
          if (Hdl=ahWnd) and (Msg=auMsg) then
            begin
            MsgRec.Msg    := auMsg;
            MsgRec.WParam := awParam;
            MsgRec.LParam := alParam;
            try
              TWSocket(Obj).WndProc(MsgRec);
            except
              on E:Exception do
                begin
                Log('WM_ASYNCSELECT(wparam='+IntToStr(awParam)+', lparam='+IntToStr(alParam)+')',E);
                rtcEnterSocket;
                try
                  Obj:=rtcGetSocket(Sock);
                  if Obj<>nil then
                    if Obj is TWSocket then
                      try
                        TWSocket(Obj).HandleBackGroundException(E);
                      except
                        on E:Exception do
                          Log('HandleBgException',E);
                        end;
                finally
                  rtcLeaveSocket;
                  end;
                end;
              end;
            Result := MsgRec.Result;
            end
          else
            begin
            if LOG_MESSAGE_ERRORS then
              Log('MESSAGE ERROR: hdl='+IntToStr(ahWnd)+',msg='+IntToStr(auMsg)+',sock='+IntToStr(awParam)+',code='+IntToStr(alParam)+' received for Object where hdl='+IntToStr(Hdl)+',msg='+IntToStr(Msg));
            Result := 0; // Old Message -> IGNORE! // DefWindowProc(ahWnd, auMsg, awParam, alParam);
            end;
          end
        else
          begin
          // Log('MESSAGE ERROR: hdl='+IntToStr(ahWnd)+',msg='+IntToStr(auMsg)+',sock='+IntToStr(awParam)+',code='+IntToStr(alParam)+' received for non-existing Object.');
          Result := 0; // Old Message -> IGNORE! // DefWindowProc(ahWnd, auMsg, awParam, alParam);
          end;
        end;

      WM_CLOSE_DELAYED,
      WM_WSOCKET_RELEASE:
        begin
        Obj:=TObject(awParam);

        if (Obj<>nil) and (Obj is TWSocket) then
          begin
          if TWSocket(Obj).Handle=ahWnd then
            begin
            MsgRec.Msg    := auMsg;
            MsgRec.WParam := awParam;
            MsgRec.LParam := alParam;
            try
              TWSocket(Obj).WndProc(MsgRec);
            except
              on E:Exception do
                Log('WM_RELEASE(wparam='+IntToStr(awParam)+', lparam='+IntToStr(alParam)+')',E);
              end;
            Result := MsgRec.Result;
            end
          else
            begin
            // Log('WSOCKET ERROR! Want Handle '+IntToStr(TWSocket(Obj).Handle)+' got handle: '+IntToStr(ahWnd));
            Result := 0; // Old Message, IGNORE! // DefWindowProc(ahWnd, auMsg, awParam, alParam);
            end;
          end
        else
          Result := 0; // Old Message, IGNORE! // DefWindowProc(ahWnd, auMsg, awParam, alParam);
        end;
      else
        Result := DefWindowProc(ahWnd, auMsg, awParam, alParam);
      end;
  except
    on E:Exception do
      begin
      Log('WndProc_MAIN(Wnd='+IntToStr(ahWnd)+', '+
                       'Msg='+IntToStr(auMsg)+', '+
                       'wParam='+IntToStr(awParam)+', '+
                       'lParam='+IntToStr(alParam)+')',E);
      Result:=0;
      end;
    end;
  end;

{ This global variable is used to store the windows class characteristic    }
{ and is needed to register the window class used by TWSocket               }
var
    XSocketRegistered:boolean=False;

{$IFDEF FPC}
    RtcSocketWindowClass: TWndClass = (
        style         : 0;
        lpfnWndProc   : WndProc(RtcSocketWindowProc);
        cbClsExtra    : 0;
        cbWndExtra    : SizeOf(Pointer);
        hInstance     : 0;
        hIcon         : 0;
        hCursor       : 0;
        hbrBackground : 0;
        lpszMenuName  : nil;
        lpszClassName : 'RtcSocketWindowClass');
{$ELSE}
    RtcSocketWindowClass: TWndClass = (
        style         : 0;
        lpfnWndProc   : @RtcSocketWindowProc;
        cbClsExtra    : 0;
        cbWndExtra    : SizeOf(Pointer);
        hInstance     : 0;
        hIcon         : 0;
        hCursor       : 0;
        hbrBackground : 0;
        lpszMenuName  : nil;
        lpszClassName : 'RtcSocketWindowClass');
{$ENDIF}

{ Unregister the window class use by the component. This is necessary to do }
{ so from a DLL when the DLL is unloaded (that is when DllEntryPoint is     }
{ called with dwReason equal to DLL_PROCESS_DETACH.                         }
procedure WSocketUnregisterClass;
  begin
  if XSocketRegistered then
    begin
    Windows.UnregisterClass(RtcSocketWindowClass.lpszClassName, HInstance);
    XSocketRegistered:=False;
    end;
  end;

function WSocketRegisterClass:integer;
  var
    TempClass       : TWndClass;
    ClassRegistered : BOOL;
  begin
  Result:=0;

  if not XSocketRegistered then
    begin
    { Check if the window class is already registered                   }
    RtcSocketWindowClass.hInstance := HInstance;
    ClassRegistered := GetClassInfo(HInstance,
                                    RtcSocketWindowClass.lpszClassName,
                                    TempClass);
    if not ClassRegistered then begin
       { Not yet registered, do it right now                            }
       Result := Windows.RegisterClass(RtcSocketWindowClass);
       if Result = 0 then
           Exit;
       end;
    XSocketRegistered:=True;
    end
  else
    Result:=1;
  end;

procedure TCustomWSocket.AllocateSocketHWnd;
begin
    FWindowHandle := rtcGetHWND(MultiThreaded);
    FMessageCode  := 0;

    if FWindowHandle = 0 then
        RaiseException('Cannot create a hidden window for TWSocket');
end;

procedure TCustomWSocket.DeallocateSocketHWnd;
begin
    if FWindowHandle = 0 then Exit;

    rtcReturnHWND(FWindowHandle);

    FWindowHandle := 0;
    FMessageCode := 0;
end;

constructor TCustomWSocket.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);

    Protocol := spTcp;

    FWindowHandle:=0;

    FBufList        := TXList.Create(32);
    FBufSize        := WSOCK_MAX_SEND_SIZE;           { Default buffer size }
    ListenBacklog   := 200;

    FMultiCastIpTTL := IP_DEFAULT_MULTICAST_TTL;

    AssignDefaultValue;

end;

destructor TCustomWSocket.Destroy;
begin
    rtcRemoveSocket(self);

    try
        CancelDnsLookup;             { Cancel any pending dns lookup      }
    except
        { Ignore any exception here }
    end;

    if FState <> wsClosed then       { Close the socket if not yet closed }
        Close;

    DeleteBufferedData;
    if Assigned(FBufList) then begin
        FBufList.Free;
        FBufList := nil;
    end;

    DeallocateSocketHWnd;

    inherited Destroy;
end;

procedure TCustomWSocket.Dup(NewHSocket : TSocket);
  var
    iStatus : Integer;
    optval,
    optlen : integer;
  begin
  if (NewHSocket = 0) or (NewHSocket = INVALID_SOCKET) then
    begin
    FLastError:=WSAEINVAL;
    SocketError('Dup');
    Exit;
    end;

  if FState <> wsClosed then
    begin
      rtcRemoveSocket(self);

      iStatus := _closesocket(FHSocket);
      FHSocket := INVALID_SOCKET;
      if iStatus <> 0 then
        begin
        FLastError:=WSAGetLastError;
        SocketError('Dup (closesocket)');
        Exit;
        end;
      ChangeState(wsClosed);
    end;

  FHsocket := NewHSocket;

  // Check SEND Buffer size
  optval := 0;
  optlen := sizeof(optval);
  iStatus := _getsockopt(FHsocket, SOL_SOCKET,
                                             SO_SNDBUF, @optval, optlen);
  if iStatus <> 0 then
    begin
    FLastError:=WSAGetLastError;
    SocketError('getsockopt(SOL_SOCKET, SO_SNDBUF)');
    Exit;
    end;

  if optlen=sizeof(optval) then
    begin
    BufSize:=optval;
    if (BufSize=0) or (BufSize>WSOCK_MAX_SEND_SIZE) then
      BufSize:=WSOCK_MAX_SEND_SIZE;
    end
  else
    BufSize:=WSOCK_MAX_SEND_SIZE;

  SetLingerOption;

  rtcStoreSocket(self, FHSocket);
  FMessageCode := WM_ASYNCSELECT_FIRST + rtcGetNextMsgCode(Handle) - 1;

  FSelectEvent := FD_READ or FD_WRITE or FD_CLOSE;

  iStatus      := _WSAASyncSelect(FHSocket, Handle,
                                                   FMessageCode, FSelectEvent);
  if iStatus <> 0 then
    begin
    FLastError:=WSAGetLastError;
    SocketError('WSAAsyncSelect');
    Exit;
    end;

  DupConnected;
  end;

procedure TCustomWSocket.DupConnected;
  begin
  ChangeState(wsConnected);
  end;

{ Get the number of char received and waiting to be read                    }
function TCustomWSocket.GetRcvdCount : LongInt;
  var
    Temp : u_long;
    Res : integer;
  begin
  if csDesigning in ComponentState then
    begin
    Result := -1;
    Exit;
    end;
  Res:=_ioctlsocket(FHSocket, FIONREAD, Temp);
  if Res = SOCKET_ERROR then
    begin
    FLastError:=WSAGetLastError;
    if (FLastError=WSAEWOULDBLOCK) or
       (FLastError=WSABASEERR) then
      begin
      if LOG_SOCKET_ERRORS then
        Log('RCV COUNT would block. Socket '+IntToStr(FHSocket)+'.');
      Result:=0;
      end
    else
      begin
      Result:=-1;
      if LOG_SOCKET_ERRORS then
        Log('RCV COUNT err ['+WSocketErrorDesc(FLastError)+'] (abort). Socket '+IntToStr(FHSocket)+'.');
      end;
    end
  else
    Result := LongInt(Temp);
  end;

procedure TCustomWSocket.ChangeState(NewState : TSocketState);
var
    OldState : TSocketState;
begin
    OldState := FState;
    FState   := NewState;
    if OldState <> NewState then       { 20030226 }
      TriggerChangeState(OldState, NewState);
end;

{ DoRecv is a simple wrapper around winsock recv function to make it        }
{ a virtual function.                                                       }
function TCustomWSocket.DoRecv(
      var Buffer;
      BufferSize : Integer;
      Flags      : Integer) : Integer;
  begin
    if (Protocol=spUdp) then
      begin
      FSrcLen := SizeOf(FSrc);
      Result := _recvfrom(FHSocket, Buffer, BufferSize, Flags, FSrc, FSrcLen);
      { If we received the requested size, we may need to receive more }
      FRcvdFlag := (Result >= BufferSize);
      end
    else
      begin
      Result := _recv(FHSocket, Buffer, BufferSize, Flags);
      { If we received the requested size, we may need to receive more }
      FRcvdFlag := (Result >= BufferSize);
      end;
    if Result>0 then
      TriggerDataIn(Result);
  end;

{ The socket is non-blocking, so this routine will only receive as much     }
{ data as it is available.                                                  }
function TCustomWSocket.Receive(var Buffer; BufferSize: integer) : integer;
  begin
  Result := DoRecv(Buffer, BufferSize, 0);
  if Result < 0 then
    begin
    FLastError := WSAGetLastError;
    if (FLastError=WSAEWOULDBLOCK) or
       (FLastError=WSABASEERR) then
      begin
      // Log('RECEIVE would block, Socket '+IntToStr(FHSocket)+'.');
      Result:=0;
      end
    else
      begin
      if LOG_SOCKET_ERRORS then
        Log('RECEIVE err ['+WSocketErrorDesc(FLastError)+'], Socket '+IntToStr(FHSocket)+'.');
      if Protocol=spTcp then
        Result:=0;
      // Close;
      // raise EWinSockException.Create('Can not receive. Error #'+IntToStr(FLastError)+': '+WSocketErrorDesc(FLastError));
      end;
    end
  else if Result=0 then
    begin
    FLastError:=WSAECONNABORTED;
    if Protocol=spTcp then
      Result:=0;
    // Log('RECEIVE 0 bytes (abort), Socket '+IntToStr(FHSocket)+'.');
    // Close;
    // raise EWinSockException.Create('Can not receive. Error #'+IntToStr(FLastError)+': '+WSocketErrorDesc(FLastError));
    end;
  {else
    begin
    if Result=BufferSize then
      Log('RECEIVE '+IntToStr(Result)+' bytes, Socket '+IntToStr(FHSocket)+'.')
    else
      Log('RECEIVE LESS '+IntToStr(Result)+'/'+IntToStr(BufferSize)+' bytes, Socket '+IntToStr(FHSocket)+'.');
    end;}
  end;

{ Receive as much data as possible into a string                            }
{ You should avoid this function and use Receive. Using string will be      }
{ much slower because data will be copied several times.                    }
{ ReceiveStr will *NOT* wait for a line to be received. It just read        }
{ already received characters and return them as a string.                  }
function TCustomWSocket.ReceiveStr : string;
  var
    lCount : LongInt;
  begin
  lCount := GetRcvdCount;

  if LCount < 0 then
    begin  { GetRcvdCount returned an error }
    SetLength(Result, 0);
    Exit;
    end;

  if lCount = 0 then        { GetRcvdCount say nothing, will try anyway }
    LCount := 512;        { some reasonable arbitrary value           }

  SetLength(Result, lCount);
  lCount := Receive(Result[1], lCount);
  if lCount > 0 then
    SetLength(Result, lCount)
  else
    SetLength(Result, 0);
  end;

function TCustomWSocket.RealSend(Data : Pointer; Len : Integer) : Integer;
begin
  if FProtoType=SOCK_DGRAM then
    begin
    if (self is TWSocketServer) and (FSrcLen>0) then
      Result := _SendTo(FHSocket, Data^, Len, 0, TSockAddr(FSrc), FSrcLen)
    else
      Result := _SendTo(FHSocket, Data^, Len, 0, TSockAddr(sin), SizeOf(sin));
    end
  else
    Result := _Send(FHSocket, Data^, Len, 0);
  if Result>0 then
    TriggerDataOut(Result);
end;

procedure TCustomWSocket.TryToSend;
  var
    oBuffer   : TBuffer;
    Len       : Integer;
    Count     : Integer;
    Data      : Pointer;
    WantClose : Boolean;
  begin
  if bAllSent or not FReadyToSend then
    Exit;

  if (FHSocket = INVALID_SOCKET) or                { No more socket      }
     (FBufList.Count = 0) then                     { Nothing to send     }
    Exit;

  repeat
    WantClose := False;

    repeat
      oBuffer := TBuffer(FBufList.First);
      Data    := oBuffer.Peek(Len);
      if Len <= 0 then
        begin
        { Buffer is empty }
        if FBufList.Count <= 1 then
          begin
          { Everything has been sent }
          bAllSent := TRUE;
          Break;
          end
        else
          begin
          oBuffer.Free;
          FBufList.removeFirst;
          end;
        end
      else
        begin
        FLastError:=0;

        {if Len>WSOCK_MAX_SEND_SIZE then
          Len:=WSOCK_MAX_SEND_SIZE;}

        Count := RealSend(Data, Len);

        if Count > 0 then
          begin
          Inc(FSentOut,Count);
          oBuffer.Remove(Count);
          if (Count<Len) then
            begin
            if LOG_SOCKET_ERRORS then
              Log('SEND ERROR! Count<Len. Socket '+IntToStr(FHSocket));
            FReadyToSend:=False;
            FSentFlag:=True;
            Break;
            {end
          else if (FSentOut>=BufSize*4) then // not all sent, will be posting a message to retry
            begin
            // Log('SEND '+IntToStr(FSentOut)+' bytes, buffer full. Socket '+IntToStr(FHSocket));
            Break;}
            end;
          end
        else if Count < 0 then
          begin
          FLastError:=WSAGetLastError;
          if FLastError=WSAEWOULDBLOCK then
            begin
            // Log('SEND '+IntToStr(FSentOut)+' bytes, now would block. Socket '+IntToStr(FHSocket));
            FReadyToSend:=False;
            FSentFlag:=True;
            end
          else if (FLastError<>WSAENOBUFS) and  // not BUFFER Full error
                  (FLastError<>WSABASEERR) then // not BASE Error (no error)
            begin
            // Log('SEND '+IntToStr(FSentOut)+' bytes, err '+IntToStr(FLastError)+' (need to close). Socket '+IntToStr(FHSocket));
            FReadyToSend:=False;
            WantClose:=True;
            end
          else if LOG_SOCKET_ERRORS then
            Log('SEND '+IntToStr(FSentOut)+' bytes, base error. Socket '+IntToStr(FHSocket));
          Break;
          end
        else
          begin
          if LOG_SOCKET_ERRORS then
            Log('SEND '+IntToStr(FSentOut)+' bytes, Count=0. Socket '+IntToStr(FHSocket));
          FLastError:=WSAECONNABORTED;
          FReadyToSend:=False;
          WantClose:=True;
          Break; { Closed by remote }
          end;
        end;
      until False;

    if WantClose then
      begin
      // Log('ABORT from TryToSend for Socket '+IntToStr(FHSocket)+'. Error #'+IntToStr(FLastError)+': '+WSocketErrorDesc(FLastError));
      FSentFlag:=True;
      // Close;
      // raise EWinSockException('Can not send. Error #'+IntToStr(FLastError)+': '+WSocketErrorDesc(FLastError));
      end
    else
      begin
      if bAllSent and not FSentFlag then
        FSentFlag:=PostMessage(Handle,
                               FMessageCode,
                               FHSocket,
                               FD_WRITE);
      end;
    until FSentFlag or bAllSent;
  end;

procedure TCustomWSocket.PutDataInSendBuffer(Data : Pointer; Len : Integer);
  var
    oBuffer  : TBuffer;
    cWritten : Integer;
    bMore    : Boolean;
  begin
  if (Len <= 0) or (Data = nil) then
    exit;

  if FBufList.Count = 0 then
    begin
    oBuffer := TBuffer.Create(FBufSize);
    FBufList.addLast(longword(oBuffer));
    end
  else
    oBuffer := TBuffer(FBufList.Last);

  bMore := TRUE;
  while bMore do
    begin
    cWritten := oBuffer.Write(Data, Len);
    if cWritten >= Len then
      bMore := FALSE
    else
      begin
      Len  := Len - cWritten;
      Data := PChar(Data) + cWritten;
      if Len < 0 then
        bMore := FALSE
      else
        begin
        oBuffer := TBuffer.Create(FBufSize);
        FBufList.addLast(longword(oBuffer));
        end;
      end;
    end;
  end;

{ Return -1 if error, else return number of byte written                    }
function TCustomWSocket.SendData(Data : Pointer; Len : Integer) : integer;
  begin
    if (FState <> wsConnected) and (FState <> wsSocksConnected) then
      begin
      FLastError:=WSAENOTCONN;
      SocketError('Send');
      Result := -1;
      Exit;
      end;

    if Len <= 0 then
      begin
      Result := 0;
      Exit;
      end;

    if Protocol=spTcp then
      begin
      bAllSent := FALSE;

      Result   := Len;
      PutDataInSendBuffer(Data, Len);

      TryToSend;
      end
    else
      begin
      Result:=RealSend(Data,Len);
      if Result=Len then
        PostMessage(Handle,
                    FMessageCode,
                    FHSocket,
                    FD_WRITE)
      else if Result<0 then
        FLastError:=WSAGetLastError;
      end;
  end;


function TCustomWSocket.ToBuff(Data : Pointer; Len : Integer) : integer;
  begin
    if (FState <> wsConnected) and (FState <> wsSocksConnected) then
      begin
      FLastError:=WSAENOTCONN;
      SocketError('Send');
      Result := -1;
      Exit;
      end;

    if Len <= 0 then
      begin
      Result := 0;
      Exit;
      end;

    if Protocol=spTcp then
      begin
      bAllSent := FALSE;

      Result   := Len;
      PutDataInSendBuffer(Data, Len);
      end
    else
      begin
      Result:=RealSend(Data,Len);
      if Result=Len then
        PostMessage(Handle,
                    FMessageCode,
                    FHSocket,
                    FD_WRITE)
      else if Result<0 then
        FLastError:=WSAGetLastError;
      end;
  end;

{ Return -1 if error, else return number of byte written                    }
function TCustomWSocket.SendStr(const Str : String) : Integer;
begin
    if Length(Str) > 0 then
        Result := SendData(@Str[1], Length(Str))
    else
        Result := 0;
end;

function TCustomWSocket.BuffStr(const Str : String) : Integer;
begin
    if Length(Str) > 0 then
        Result := ToBuff(@Str[1], Length(Str))
    else
        Result := 0;
end;

procedure TCustomWSocket.ASyncReceive(Error: Word);
  var
    bMore    : Boolean;
    lCount   : LongWord;
    TrashCan : array [0..1023] of char;
  begin
  bMore := TRUE;
  while bMore do
    begin
    FLastError := 0;
    if not TriggerDataReceived(Error) then
      begin
      { Nothing wants to receive, we will receive and throw away  23/07/98 }
      if DoRecv(TrashCan, SizeOf(TrashCan), 0) = SOCKET_ERROR then
        begin
        FLastError := WSAGetLastError;
        if FLastError = WSAEWOULDBLOCK then
          begin
          FLastError := 0;
          Break;
          end;
        end;
      end;

    if FLastError <> 0 then
      begin
      bMore := FALSE;
      { -1 value is not a true error but is used to break the loop }
      if FLastError = -1 then
        FLastError := 0;
      end
    { Check if we have something new arrived, if yes, process it }
    else
      begin
      if _ioctlsocket(FHSocket, FIONREAD, lCount) = SOCKET_ERROR then
        begin
        FLastError := WSAGetLastError;
        bMore      := FALSE;
        end
      else if lCount = 0 then
        bMore := FALSE;
      end;
    end;
  end;

procedure TCustomWSocket.Do_FD_CONNECT(Err:word);
  begin
  if FState <> wsConnected then
    begin
    ChangeState(wsConnected);
    if (Err <> 0) and (FState <> wsClosed) then
      Close;
    end;
  end;

procedure TCustomWSocket.Do_FD_READ;
  var
    cnt:integer;
    soc:TSocket;
  begin
  if FState <> wsConnected then
    ChangeState(wsConnected);

  soc:=FHSocket;

  FRcvdFlag:=False;
  TriggerDataReceived(0);

  if rtcCheckSocket(soc)<>Self then
    Exit; // connection destroyed from DataReceived

  if FRcvdFlag and (Protocol=spTcp) then
    begin
    cnt:=0;
    while FHSocket<>INVALID_SOCKET do
      begin
      Inc(cnt);
      if cnt<10 then
        begin
        FRcvdFlag:=False;
        TriggerDataReceived(0);

        if rtcCheckSocket(soc)<>Self then
          Exit; // connection destroyed from DataReceived

        if not FRcvdFlag then Break;
        end
      else
        begin
        if GetRcvdCount>0 then
          begin
          if PostMessage(Handle,FMessageCode,
                         FHSocket,FD_READ) then // can post message?
            Break
          else
            cnt:=0;
          end
        else
          Break;
        end;
      end;
    end;
  end;

procedure TCustomWSocket.Do_FD_WRITE;
  begin
  FReadyToSend:=True;
  FSentFlag:=False;
  FSentOut:=0;

  if FState <> wsConnected then
    ChangeState(wsConnected);

  if bAllSent then
    TriggerDataSent(0)
  else
    TryToSend;
  end;

procedure TCustomWSocket.Do_FD_CLOSE(Err:word);
begin
  if FState <> wsConnecting then
    begin
    if (Err = 0) and // not closing with error
       (FState <> wsClosed) then // connection still open
      ASyncReceive(0);

    Close;
    end;
end;

procedure TCustomWSocket.Do_FD_ACCEPT;
begin
    TriggerSessionAvailable(0);
end;

procedure TCustomWSocket.WMASyncSelect(var msg: TMessage);
  begin
  case msg.lParamLo of
    FD_CONNECT:
      begin
      Call_FD_CONNECT(msg.LParamHi);
      end;
    FD_ACCEPT:
      begin
      Call_FD_ACCEPT;
      end;
    FD_WRITE:
      begin
      Call_FD_WRITE;
      end;
    FD_READ:
      begin
      Call_FD_READ;
      end;
    FD_CLOSE:
      begin
      Call_FD_CLOSE(msg.LParamHi);
      end;
    end;
  end;

{procedure GetIPList(phe  : PHostEnt; ToList : TStrings);
type
    TaPInAddr = array [0..255] of PInAddr;
    PaPInAddr = ^TaPInAddr;
var
    pptr : PaPInAddr;
    I    : Integer;
begin
    pptr := PaPInAddr(Phe^.h_addr_list);

    I := 0;
    while pptr^[I] <> nil do begin
        ToList.Add(StrPas(WSocket_inet_ntoa(pptr^[I]^)));
        Inc(I);
    end;
end;}

procedure TCustomWSocket.SetRemotePort(sPort : String);
begin
    if FPortAssigned and (FPortStr = sPort) then
        Exit;

    if FState <> wsClosed then begin
        RaiseException('Cannot change Port if not closed');
        Exit;
    end;

    FPortStr := Trim(sPort);
    if Length(FPortStr) = 0 then begin
        FPortAssigned := FALSE;
        Exit;
    end;

    FPortResolved := FALSE;
    FPortAssigned := TRUE;
end;

function TCustomWSocket.GetRemotePort : String;
begin
    Result := FPortStr;
end;

procedure TCustomWSocket.SetLocalPort(sLocalPort : String);
begin
    if FState <> wsClosed then begin
        RaiseException('Cannot change LocalPort if not closed');
        Exit;
    end;

    FLocalPortStr      := sLocalPort;
    FLocalPortResolved := FALSE;
end;

procedure TCustomWSocket.SetLocalAddr(sLocalAddr : String);
begin
    if FState <> wsClosed then begin
        RaiseException('Cannot change LocalAddr if not closed');
        Exit;
    end;

    if Length(sLocalAddr) = 0 then
        sLocalAddr := '0.0.0.0';
    FLocalAddr := sLocalAddr;
end;

function TCustomWSocket.GetXPort: string;
var
    saddr    : TSockAddrIn;
    saddrlen : integer;
    _port     : integer;
begin
    Result := 'error';
    if FState in [wsConnected, wsBound, wsListening] then begin
        saddrlen := sizeof(saddr);
        if _GetSockName(FHSocket, TSockAddr(saddr), saddrlen) = 0 then begin
            _port     := _ntohs(saddr.sin_port);
            Result   := Format('%d',[_port]);
        end;
    end;
end;

function TCustomWSocket.GetXAddr: string;
var
    saddr    : TSockAddrIn;
    saddrlen : integer;
begin
    Result := '0.0.0.0';
    if FState in [wsConnected, wsBound, wsListening] then begin
        saddrlen := sizeof(saddr);
        if _GetSockName(FHSocket, TSockAddr(saddr), saddrlen) = 0 then
            Result := StrPas(_inet_ntoa(saddr.sin_addr));
     end;
end;

procedure TCustomWSocket.SetAddr(InAddr : String);
begin
    if FAddrAssigned and (FAddrStr = InAddr) then
        Exit;

    if FState <> wsClosed then begin
        RaiseException('Cannot change Addr if not closed');
        Exit;
    end;

    FAddrStr := Trim(InAddr);
    if Length(FAddrStr) = 0 then begin
        FAddrAssigned := FALSE;
        Exit;
    end;

    FAddrResolved       := FALSE;
    FAddrAssigned       := TRUE;
end;

function WSocket_ResolveHost(InAddr : String) : TInAddr;
var
    szData  : array [0..256] of char;
    Phe     : Phostent;
    IPAddr  : u_long;
begin
    if (Length(InAddr) = 0) or (Length(InAddr) >= SizeOf(szData)) then
        raise EWinSockException.Create('WSocketResolveHost: ''' + InAddr + ''' Invalid Hostname.');

    StrPCopy(szData, Trim(InAddr)); { Length already checked above }
    if WSocketIsDottedIP(InAddr) then begin
        { Address is a dotted numeric address like 192.161.124.32 }
        IPAddr := _inet_addr(szData);
        if IPAddr = u_long(INADDR_NONE) then begin
            if StrComp(szData, '255.255.255.255') = 0 then begin
                Result.s_addr := u_long(INADDR_BROADCAST);
                Exit;
            end;
            raise EWinSockException.Create('WSocketResolveHost: ''' + InAddr + ''' Invalid IP address.');
        end;
        Result.s_addr := IPAddr;
        Exit;
    end;

    { Address is a hostname }
    Phe := _GetHostByName(szData);
    if Phe = nil then
        raise EWinSockException.CreateFmt(
                 'WSocketResolveHost: Cannot convert host address ''%s'', Error #%d',
                 [InAddr, _WSAGetLastError]);
    Result.s_addr := PInAddr(Phe^.h_addr_list^)^.s_addr;
end;

{ Convert port name or number to number in host order (ftp -> 21)           }
function WSocket_ResolvePort(Port : String; Proto : String) : Word;
var
    szPort   : array [0..31] of char;
    szProto  : array [0..31] of char;
    Pse      : Pservent;
begin
    if (Length(Port) = 0) or (Length(Port) >= SizeOf(szPort)) then
        raise EWinSockException.Create('WSocketResolvePort: Invalid Port.');

    if (Length(Proto) = 0) or (Length(Proto) >= SizeOf(szProto)) then
        raise EWinSockException.Create('WSocketResolvePort: Invalid Proto.');

    if IsDigit(Port[1]) then
        Result := atoi(Port)
    else begin
        StrPCopy(szPort, Trim(Port));   { Length already checked above }
        StrPCopy(szProto, Trim(Proto)); { Length already checked above }
        if szProto[0] = #0 then
            Pse := _GetServByName(szPort, nil)
        else
            Pse := _GetServByName(szPort, szProto);
        if Pse = nil then
            raise EWinSockException.CreateFmt(
                     'WSocketResolvePort: Cannot convert port ''%s'', Error #%d',
                     [Port, _WSAGetLastError]);
        Result := _ntohs(Pse^.s_port);
    end;
end;

function TCustomWSocket.GetAddr : String;
begin
    Result := FAddrStr;
end;

function TCustomWSocket.GetPeerAddr: string;
var
    saddr    : TSockAddrIn;
    saddrlen : integer;
    szAddr   : PChar;
    Res: integer;
begin
    if Protocol=spTcp then
      begin
      Result := '0.0.0.0';
      if FState = wsConnected then
        begin
          saddrlen := sizeof(saddr);
          Res:=_GetPeerName(FHSocket, TSockAddr(saddr), saddrlen);
          if Res = 0 then
            begin
            szAddr := _inet_ntoa(saddr.sin_addr);
            Result := szAddr;
            end
          else
            begin
            FLastError:=WSAGetLastError;
            SocketError('GetPeerAddr');
            Exit;
            end;
        end;
      end
    else
      Result:=_inet_ntoa(Sin.sin_addr);
end;


function TCustomWSocket.GetSrcAddr: string;
  begin
  if FSrcLen>0 then
    Result:=_inet_ntoa(FSrc.sin_addr)
  else
    Result:='0.0.0.0';
  end;

function TCustomWSocket.GetPeerPort: string;
var
    saddr    : TSockAddrIn;
    saddrlen : integer;
    Res : integer;
begin
    if Protocol=spTcp then
      begin
      Result := 'error';
      if FState = wsConnected then
        begin
          saddrlen := sizeof(saddr);
          Res:=_GetPeerName(FHSocket, TSockAddr(saddr), saddrlen);
          if Res = 0 then
            Result := IntToStr(_ntohs(saddr.sin_port))
          else
            begin
            FLastError:=WSAGetLastError;
            SocketError('GetPeerPort');
            Exit;
            end;
        end;
      end
    else
      Result:=IntToStr(_ntohs(Sin.sin_port));
end;

function TCustomWSocket.GetSrcPort: string;
  begin
  if FSrcLen>0 then
    Result:=IntToStr(_ntohs(FSrc.sin_port))
  else
    Result:='0';
  end;

procedure TCustomWSocket.CancelDnsLookup;
  var
    Res:integer;
begin
    if FDnsLookupHandle = 0 then
        Exit;
    Res:=_WSACancelAsyncRequest(FDnsLookupHandle);
    if Res <> 0 then
      begin
      FLastError:=WSAGetLastError;
      FDnsLookupHandle := 0;
      SocketError('WSACancelAsyncRequest');
      Exit;
      end;
    FDnsLookupHandle := 0;

    if not (csDestroying in ComponentState) then
        TriggerDnsLookupDone(WSAEINTR);
end;

procedure TCustomWSocket.BindSocket;
var
    SockName      : TSockAddr;
    SockNamelen   : Integer;
    LocalSockName : TSockAddrIn;
begin
    FillChar(LocalSockName, Sizeof(LocalSockName), 0);
    SockNamelen                   := sizeof(LocalSockName);
    LocalSockName.sin_family      := AF_INET;
    LocalSockName.sin_port        := _htons(FLocalPortNum);
    LocalSockName.sin_addr.s_addr := WSocket_ResolveHost(FLocalAddr).s_addr;

    if _bind(HSocket, LocalSockName, SockNamelen) <> 0 then
        begin
        RaiseExceptionFmt('winsock.bind failed, error #%d', [_WSAGetLastError]);
        Exit;
        end;
    SockNamelen := sizeof(SockName);
    if _getsockname(FHSocket, SockName, SockNamelen) <> 0 then
        begin
        RaiseExceptionFmt('winsock.getsockname failed, error #%d',
                          [_WSAGetLastError]);
        Exit;
        end;
    FLocalPortNum := _ntohs(SockName.sin_port);
    FLocalPortStr := IntToStr(FLocalPortNum);
end;

procedure TCustomWSocket.SetLingerOption(aborting:boolean=False);
  var
    iStatus : integer;
    li      : TLinger;
  begin
  if FHSocket = INVALID_SOCKET then begin
      RaiseException('Cannot set linger option at this time');
      Exit;
  end;

  if aborting then
    begin
    // Linger = True, Timeout = 0, HARD CLOSE
    li.l_onoff := 1;
    li.l_linger := 0;
    end
  else
    begin
    // Linger = False, Timeout = 0, graceful close
    li.l_onoff := 0;
    li.l_linger := 0;
    end;
  iStatus     := _setsockopt(FHSocket, SOL_SOCKET,
                                    SO_LINGER, @li, SizeOf(li));
  if iStatus <> 0 then
    begin
    FLastError:=WSAGetLastError;
    SocketError('setsockopt(SO_LINGER)');
    Exit;
    end;
  end;

procedure TCustomWSocket.Connect;
  var
    iStatus : integer;
    optval  : integer;
    optlen  : integer;
    lAddr   : TInAddr;
  begin
  WinSockLoad;

    if (FHSocket <> INVALID_SOCKET) and (FState <> wsClosed) then begin
        RaiseException('Connect: Socket already in use');
        Exit;
    end;

    if  not FPortAssigned then begin
        RaiseException('Connect: No Port Specified');
        Exit;
    end;

    if not FAddrAssigned then begin
        RaiseException('Connect: No IP Address Specified');
        Exit;
    end;

    try
        if not FPortResolved then begin
            { The next line will trigger an exception in case of failure }
            FPortNum      := WSocket_ResolvePort(FPortStr, FProtoStr);
            sin.sin_port  := _htons(FPortNum);
            FPortResolved := TRUE;
        end;

        if not FLocalPortResolved then begin
            { The next line will trigger an exception in case of failure }
            FLocalPortNum      := WSocket_ResolvePort(FLocalPortStr, FProtoStr);
            FLocalPortResolved := TRUE;
        end;

        if not FAddrResolved then begin
            { The next line will trigger an exception in case of failure }
            sin.sin_addr.s_addr := WSocket_ResolveHost(FAddrStr).s_addr;
            FAddrResolved := TRUE;
        end;
    except
        on E:Exception do
          begin
            RaiseException('connect: ' + E.Message);
            Exit;
        end;
    end;

    { Remove any data from the internal output buffer }
    { (should already be empty !)                     }
    DeleteBufferedData;

    FHSocket := _socket(FAddrFormat, FProtoType, FProto);
    if FHSocket = INVALID_SOCKET then
      begin
      FLastError:=WSAGetLastError;
      SocketError('Connect (socket)');
      Exit;
      end;
    ChangeState(wsOpened);

    if FState <> wsOpened then
      begin  { 07/07/02 }
        { Socket has been closed in the OnChangeState event ! }
        FLastError:=WSAEINVAL;
        SocketError('Connect (Invalid operation in OnChangeState)');
        Exit;
      end;

    if FProtoType = SOCK_DGRAM then
      begin
      BindSocket;
      if FMultiCast then
        begin
        if FMultiCastIpTTL <> IP_DEFAULT_MULTICAST_TTL then
          begin
          optval := FMultiCastIpTTL; { set time-to-live for multicast }
          iStatus := _setsockopt(FHSocket, IPPROTO_IP, IP_MULTICAST_TTL,
                                @optval, SizeOf(optval));
          if iStatus <> 0 then
            begin
            FLastError:=WSAGetLastError;
            SocketError('setsockopt(IP_MULTICAST_TTL)');
            Exit;
            end;
          end;
        if FLocalAddr <> '0.0.0.0' then
          begin                      { RK }
          laddr.s_addr := WSocket_ResolveHost(FLocalAddr).s_addr;
          iStatus      := _SetSockOpt(FHSocket, IPPROTO_IP,
                                     IP_MULTICAST_IF,
                                     PChar(@laddr), SizeOf(laddr));
          if iStatus <> 0 then
            begin
            FLastError:=WSAGetLastError;
            SocketError('setsockopt(IP_MULTICAST_IF)');
            Exit;
            end;
          end;                                                       { /RK }
        end;

      if sin.sin_addr.S_addr = u_long(INADDR_BROADCAST) then
        begin
        OptVal  := 1;
        iStatus := _setsockopt(FHSocket, SOL_SOCKET, SO_BROADCAST,
                                      PChar(@OptVal), SizeOf(OptVal));
        if iStatus <> 0 then
          begin
          FLastError:=WSAGetLastError;
          SocketError('setsockopt(SO_BROADCAST)');
          Exit;
          end;
        end;
      end
    else
      begin
      { Socket type is SOCK_STREAM }
      // REUSE ADDR.
      optval  := -1;
      iStatus := _setsockopt(FHSocket, SOL_SOCKET,
                                                 SO_REUSEADDR, @optval, SizeOf(optval));
      if iStatus <> 0 then
        begin
        FLastError:=WSAGetLastError;
        SocketError('setsockopt(SO_REUSEADDR)');
        Exit;
        end;

      SetLingerOption;

      // NO DELAY
      optval := -1; { -1=true, 0=false }
      iStatus := _setsockopt(FHsocket, IPPROTO_TCP,
                                                 TCP_NODELAY, @optval, SizeOf(optval));
      if iStatus <> 0 then
        begin
        FLastError:=WSAGetLastError;
        SocketError('setsockopt(IPPROTO_TCP, TCP_NODELAY)');
        Exit;
        end;

      // KEEP-ALIVE
      optval  := -1;
      iStatus := _setsockopt(FHSocket, SOL_SOCKET,
                                                 SO_KEEPALIVE, @optval, SizeOf(optval));
      if iStatus <> 0 then
        begin
        FLastError:=WSAGetLastError;
        SocketError('setsockopt(SO_KEEPALIVE)');
        Exit;
        end;

      // Set RECV_TIMEO
      optval := WSOCK_RECV_TIMEOUT;
      {iStatus :=} _setsockopt(FHsocket, SOL_SOCKET,
                                                 SO_RCVTIMEO, @optval, SizeOf(optval));
      (* if iStatus <> 0 then
        begin
        FLastError := WSAGetLastError;
        SocketError('setsockopt(SOL_SOCKET, SO_RCVTIMEO)');
        Exit;
        end; *)

      // Set SND_TIMEO
      optval := WSOCK_SEND_TIMEOUT;
      {iStatus :=} _setsockopt(FHsocket, SOL_SOCKET,
                                                 SO_SNDTIMEO, @optval, SizeOf(optval));
      (* if iStatus <> 0 then
        begin
        FLastError := WSAGetLastError;
        SocketError('setsockopt(SOL_SOCKET, SO_SNDTIMEO)');
        Exit;
        end; *)

      // Set READ Buffer
      optval := WSOCK_READ_BUFFER_SIZE;
      iStatus := _setsockopt(FHsocket, SOL_SOCKET,
                                                 SO_RCVBUF, @optval, SizeOf(optval));
      if iStatus <> 0 then
        begin
        FLastError := WSAGetLastError;
        SocketError('setsockopt(SOL_SOCKET, SO_RCVBUF)');
        Exit;
        end;

      // Set SEND Buffer
      optval := WSOCK_SEND_BUFFER_SIZE;
      iStatus := _setsockopt(FHsocket, SOL_SOCKET,
                                                 SO_SNDBUF, @optval, SizeOf(optval));
      if iStatus <> 0 then
        begin
        FLastError := WSAGetLastError;
        SocketError('setsockopt(SOL_SOCKET, SO_SNDBUF)');
        Exit;
        end;

      // Check SEND Buffer size
      optval := 0;
      optlen := sizeof(optval);
      iStatus := _getsockopt(FHsocket, SOL_SOCKET,
                                                 SO_SNDBUF, @optval, optlen);
      if iStatus <> 0 then
        begin
        FLastError:=WSAGetLastError;
        SocketError('getsockopt(SOL_SOCKET, SO_SNDBUF)');
        Exit;
        end;

      if optlen=sizeof(optval) then
        begin
        BufSize:=optval;
        if (BufSize=0) or (BufSize>WSOCK_MAX_SEND_SIZE) then
          BufSize:=WSOCK_MAX_SEND_SIZE;
        end
      else
        BufSize:=WSOCK_MAX_SEND_SIZE;

      if (FLocalPortNum <> 0) or (FLocalAddr <> '0.0.0.0') then
        BindSocket;

      end;

    FMessageCode := WM_ASYNCSELECT_FIRST + rtcGetNextMsgCode(Handle) - 1;
    FSelectEvent := FD_READ   or FD_WRITE or FD_CLOSE or FD_CONNECT;

    iStatus       := _WSAASyncSelect(FHSocket, Handle,
                                                         FMessageCode, FSelectEvent);
    if iStatus <> 0 then
      begin
      FLastError:=WSAGetLastError;
      SocketError('WSAAsyncSelect');
      Exit;
      end;

    rtcStoreSocket(self, FHSocket);

    if FProtoType = SOCK_DGRAM then
      ChangeState(wsConnected)
    else
      begin
      iStatus := _connect(FHSocket, TSockAddr(sin), sizeof(sin));
      if iStatus = 0 then
        ChangeState(wsConnecting)
      else
        begin
        FLastError := WSAGetLastError;
        if FLastError = WSAEWOULDBLOCK then
          ChangeState(wsConnecting)
        else
          begin
          SocketError('Connect');
          Exit;
          end;
        end;
      end;
  end;

procedure TCustomWSocket.Listen;
type
    ip_mreq = record
        imr_multiaddr : in_addr;
        imr_interface : in_addr;
    end;
var
    optval,
    iStatus        : Integer;
    mreq    : ip_mreq;
    szAddr : array[0..256] of char;
begin
  WinSockLoad;

    if not FPortAssigned then
        begin
        FLastError:=WSAEINVAL;
        SocketError('listen: port not assigned');
        Exit;
        end;

    if not FAddrAssigned then
        begin
        FLastError:=WSAEINVAL;
        SocketError('listen: address not assigned');
        Exit;
        end;

    try
        if not FPortResolved then begin
            { The next line will trigger an exception in case of failure }
            FPortNum      := WSocket_ResolvePort(FPortStr, FProtoStr);
            sin.sin_port  := _htons(FPortNum);
            FPortResolved := TRUE;
        end;

        if not FAddrResolved then begin
            { The next line will trigger an exception in case of failure }
            sin.sin_addr.s_addr := WSocket_ResolveHost(FAddrStr).s_addr;
            FAddrResolved       := TRUE;
        end;
    except
        on E:Exception do begin
            RaiseException('listen: ' + E.Message);
            Exit;
        end;
    end;

    { Remove any data from the internal output buffer }
    { (should already be empty !)                     }
    DeleteBufferedData;

    FHSocket := _socket(FAddrFormat, FProtoType, FProto);
    if FHSocket = INVALID_SOCKET then
      begin
      FLastError:=WSAGetLastError;
      SocketError('socket');
      exit;
      end;

    if FProtoType = SOCK_DGRAM then
      begin
      if FReuseAddr then
        begin
        { Enable multiple tasks to listen on duplicate address and port }
        optval  := -1;
        iStatus := _setsockopt(FHSocket, SOL_SOCKET, SO_REUSEADDR, @optval, SizeOf(optval));

        if iStatus <> 0 then
          begin
          FLastError:=WSAGetLastError;
          SocketError('setsockopt(SO_REUSEADDR)');
          Exit;
          end;
        end;
      end;

    iStatus := _bind(FHSocket, TSockAddr(sin), sizeof(sin));
    if iStatus = 0 then
      ChangeState(wsBound)
    else
      begin
      FLastError:=WSAGetLastError;
      SocketError('Bind');
      Close;
      Exit;
      end;

    if FProtoType = SOCK_DGRAM then
      begin
      if FMultiCast then
        begin
        { Use setsockopt() to join a multicast group }
        { mreq.imr_multiaddr.s_addr := Inet_addr('225.0.0.37');}
        { mreq.imr_multiaddr.s_addr :=  sin.sin_addr.s_addr;}
        { mreq.imr_multiaddr.s_addr :=  Inet_addr(FAddrStr);}
        StrPCopy(szAddr, FMultiCastAddrStr);
        mreq.imr_multiaddr.s_addr :=  _inet_addr(szAddr);
        { mreq.imr_interface.s_addr := htonl(INADDR_ANY);} { RK}
        mreq.imr_interface.s_addr := WSocket_ResolveHost(FAddrStr).s_addr;
        iStatus := _setsockopt(FHSocket, IPPROTO_IP,
                              IP_ADD_MEMBERSHIP, @mreq, SizeOf(mreq));
        if iStatus <> 0 then
          begin
          FLastError:=WSAGetLastError;
          SocketError('setsockopt(IP MULTICAST)');
          Exit;
          end;
        end;

      end
    else
      begin
      iStatus := _listen(FHSocket, FListenBacklog);
      if iStatus <> 0 then
        begin
        FLastError:=WSAGetLastError;
        SocketError('Listen');
        Exit;
        end;

      // NO DELAY
      optval := -1; { -1=true, 0=false }
      iStatus := _setsockopt(FHSocket, IPPROTO_TCP,
                                                 TCP_NODELAY, @optval, SizeOf(optval));
      if iStatus <> 0 then
        begin
        FLastError:=WSAGetLastError;
        SocketError('WSAASyncSelect');
        Exit;
        end;

      // KEEP-ALIVE
      optval  := -1;
      iStatus := _setsockopt(FHSocket, SOL_SOCKET,
                                                 SO_KEEPALIVE, @optval, SizeOf(optval));
      if iStatus <> 0 then
        begin
        FLastError:=WSAGetLastError;
        SocketError('WSAASyncSelect');
        Exit;
        end;

      // Set RECV_TIMEO
      optval := WSOCK_RECV_TIMEOUT;
      {iStatus :=} _setsockopt(FHSocket, SOL_SOCKET,
                                                 SO_RCVTIMEO, @optval, SizeOf(optval));
      (* if iStatus <> 0 then
        begin
        FLastError := WSAGetLastError;
        SocketError('setsockopt(SOL_SOCKET, SO_RCVTIMEO)');
        Exit;
        end; *)

      // Set SND_TIMEO
      optval := WSOCK_SEND_TIMEOUT;
      {iStatus :=} _setsockopt(FHSocket, SOL_SOCKET,
                                                 SO_SNDTIMEO, @optval, SizeOf(optval));
      (* if iStatus <> 0 then
        begin
        FLastError := WSAGetLastError;
        SocketError('setsockopt(SOL_SOCKET, SO_SNDTIMEO)');
        Exit;
        end; *)

      // Set READ Buffer size
      optval := WSOCK_READ_BUFFER_SIZE;
      iStatus := _setsockopt(FHSocket, SOL_SOCKET,
                                                 SO_RCVBUF, @optval, SizeOf(optval));
      if iStatus <> 0 then
        begin
        FLastError:=WSAGetLastError;
        SocketError('WSAASyncSelect');
        Exit;
        end;

      // Set SEND Buffer size
      optval := WSOCK_SEND_BUFFER_SIZE;
      iStatus := _setsockopt(FHSocket, SOL_SOCKET,
                                                 SO_SNDBUF, @optval, SizeOf(optval));
      if iStatus <> 0 then
        begin
        FLastError:=WSAGetLastError;
        SocketError('WSAASyncSelect');
        Exit;
        end;

      SetLingerOption(True);
      end;

    rtcStoreSocket(self, FHSocket);

    if Protocol=spTcp then
      FSelectEvent := FD_ACCEPT
    else
      FSelectEvent := FD_READ or FD_WRITE or FD_ACCEPT or FD_CLOSE;

    FMessageCode := WM_ASYNCSELECT_FIRST + rtcGetNextMsgCode(Handle) - 1;

    iStatus      := _WSAASyncSelect(FHSocket, Handle,
                                           FMessageCode, FSelectEvent);
    if iStatus <> 0 then
      begin
      FLastError:=WSAGetLastError;
      SocketError('WSAASyncSelect');
      exit;
      end;

  if FProtoType = SOCK_DGRAM then
    begin
    ChangeState(wsListening);
    if FHSocket<>INVALID_SOCKET then
      ChangeState(wsConnected);
    end
  else
    ChangeState(wsListening);
  end;

function TCustomWSocket.Accept: TSocket;
var
   len     : integer;
begin
    if FState <> wsListening then
      begin
      FLastError:=WSAEINVAL;
      SocketError('not a listening socket');
      Result := INVALID_SOCKET;
      Exit;
      end;

    len := sizeof(sin);
    FASocket := _accept(FHSocket, @sin, @len);
    if FASocket = INVALID_SOCKET then
      begin
      FLastError:=WSAGetLastError;
      SocketError('Accept');
      Result := INVALID_SOCKET;
      end
    else
      Result := FASocket;
end;

procedure TCustomWSocket.Shutdown(How : Integer);
begin
    if FHSocket <> INVALID_SOCKET then
        _shutdown(FHSocket, How);
end;

procedure TCustomWSocket.DeleteBufferedData;
  begin
  { Delete all data buffer }
  while FBufList.Last>0 do
    begin
    TBuffer(FBufList.Last).Free;
    FBufList.removeLast;
    end;
  end;

procedure TCustomWSocket.Abort;
begin
    InternalAbort(0);
end;

procedure TCustomWSocket.InternalAbort(ErrCode : Word);
begin
    CancelDnsLookup;
    DeleteBufferedData;
    { Be sure to close as fast as possible (abortive close) }
    if (State = wsConnected) and (FProto=IPPROTO_TCP) then
      SetLingerOption(True);
    InternalClose(FALSE, ErrCode);
end;

procedure TCustomWSocket.Close;
begin
    InternalClose(TRUE, 0);
end;

procedure TCustomWSocket.CloseDelayed;
begin
    // Send FSocket to message handler
    PostMessage(Handle, WM_CLOSE_DELAYED, longint(self), 0);
end;

procedure TCustomWSocket.Release;
begin
    // Send FSocket to message handler
    PostMessage(Handle, WM_WSOCKET_RELEASE, longint(self), 0);
end;

procedure TCustomWSocket.Do_CloseDelayed;
begin
    Close;
end;

procedure TCustomWSocket.Do_Release;
begin
    Destroy;
end;

procedure TCustomWSocket.InternalClose(bShut : Boolean; Error : Word);
  var
    iStatus : integer;
  begin
  if not rtcRemoveSocket(self) then Exit;

  if FHSocket = INVALID_SOCKET then
    begin
    if FState <> wsClosed then
      begin
      ChangeState(wsClosed);
      AssignDefaultValue;
      end;
    Exit;
    end;

  if FState = wsClosed then
    Exit;

  if bShut then
    ShutDown(SD_BOTH);

  if FHSocket <> INVALID_SOCKET then
    begin
    { Close the socket }
    iStatus := _closesocket(FHSocket);
    if iStatus<>0 then
      begin
      FLastError := WSAGetLastError;
      if (FLastError = WSABASEERR) then
        begin
        // Not an error.
        end
      else if (FLastError <> WSAEWOULDBLOCK) then
        begin
        FHSocket := INVALID_SOCKET;
        { Ignore the error occuring when winsock DLL not      }
        { initialized (occurs when using TWSocket from a DLL) }
        SocketError('Disconnect (closesocket)');
        Exit;
        end
      else
        PostMessage(Handle,WM_TSOCKET_CLOSE,FHSocket,0);
      end;

    FHSocket := INVALID_SOCKET;
    end;

  ChangeState(wsClosed);
  {try
    AssignDefaultValue;
  except
    on E:Exception do
      Log('InternalClose.AssignDefaultValue',E);
    end;}
  end;

procedure TCustomWSocket.TriggerSessionAvailable(Error : Word);
begin
    if Assigned(FOnSessionAvailable) then
        FOnSessionAvailable(Self, Error);
end;

function TCustomWSocket.TriggerDataReceived(Error : Word) : Boolean;
begin
    Result:=assigned(FOnDataReceived);
    if Result then
      FOnDataReceived(Self, Error);
end;

procedure TCustomWSocket.TriggerDataSent(Error : Word);
begin
    if Assigned(FOnDataSent) then
        FOnDataSent(Self, Error);
end;

procedure TCustomWSocket.TriggerDataOut(Len: Cardinal);
begin
    if Assigned(FOnDataOut) then
        FOnDataOut(Self, Len);
end;

procedure TCustomWSocket.TriggerDataIn(Len: Cardinal);
begin
    if Assigned(FOnDataIn) then
        FOnDataIn(Self, Len);
end;

procedure TCustomWSocket.TriggerError;
begin
    if Assigned(FOnError) then
        FOnError(Self);
end;

procedure TCustomWSocket.TriggerDNSLookupDone(Error : Word);
begin
    if Assigned(FOnDNSLookupDone) then
        FOnDNSLookupDone(Self, Error);
end;

procedure TCustomWSocket.TriggerChangeState(OldState, NewState : TSocketState);
begin
    if Assigned(FOnChangeState) then
        FOnChangeState(Self, OldState, NewState);
end;

procedure TCustomWSocket.SocketError(sockfunc: string);
  var
    Error  : integer;
    Line   : string;
  begin
  Error := FLastError; // WSocket_WSAGetLastError;
  Line  := 'Error '+ IntToStr(Error) + ' in function ' + sockfunc +
           #13#10 + WSocketErrorDesc(Error);

  if (Error = WSAECONNRESET) or
     (Error = WSAENOTCONN)   then
    begin
      rtcRemoveSocket(self);

      _closesocket(FHSocket);

      FHSocket := INVALID_SOCKET;

      ChangeState(wsClosed);
    end;
  raise EWinSockException.Create(Line);
  end;


procedure TCustomWSocket.SetProtocol(Value: TSocketProtocol);
  begin
  FProtocol:=Value;
  case Value of
    spTcp:begin
          FProtoStr:=TXPROTO_TCP;
          FProto:=IPPROTO_TCP;
          FProtoType:=SOCK_STREAM;
          end;
    spUdp:begin
          FProtoStr:=TXPROTO_UDP;
          FProto:=IPPROTO_UDP;
          FProtoType:=SOCK_DGRAM;
          end;
    end;
  end;

procedure TCustomWSocket.Call_CloseDelayed;
  begin
  Do_CloseDelayed;
  end;

procedure TCustomWSocket.Call_FD_ACCEPT;
  begin
  Do_FD_ACCEPT;
  end;

procedure TCustomWSocket.Call_FD_CLOSE(Err: word);
  begin
  Do_FD_CLOSE(Err);
  end;

procedure TCustomWSocket.Call_FD_CONNECT(Err: word);
  begin
  Do_FD_CONNECT(Err);
  end;

procedure TCustomWSocket.Call_FD_READ;
  begin
  Do_FD_READ;
  end;

procedure TCustomWSocket.Call_FD_WRITE;
  begin
  Do_FD_WRITE;
  end;

procedure TCustomWSocket.Call_Release;
  begin
  Do_Release;
  end;

function TCustomWSocket.GetWindowHandle: HWND;
  begin
  if FWindowHandle=0 then
    AllocateSocketHWnd;
  Result:=FWindowHandle;
  end;

function WinSockGetProc(const ProcName : String) : Pointer;
  begin
  if Length(ProcName) = 0 then
    Result := nil
  else
    begin
    Result := GetProcAddress(FDllHandle, @ProcName[1]);
    if Result = nil then
      raise EWinSockException.Create('Procedure ' + ProcName +
                                     ' not found in ' + winsockdll +
                                     ' Error #' + IntToStr(GetLastError));
    end;
  end;

procedure WinSockLoad;
  var
    LastError : LongInt;
  begin
  LibCS.Enter;
  try
    if FDllHandle = 0 then
      begin
      FDllHandle := LoadLibrary(@winsockdll[1]);
      if FDllHandle = 0 then
        raise EWinSockException.Create('Unable to load ' + winsockdll +
                                       ' Error #' + IntToStr(GetLastError));

      try
        _WSAStartup            := TWSAStartup(WinSockGetProc('WSAStartup'));
        _WSACleanup            := TWSACleanup(WinSockGetProc('WSACleanup'));
        _WSAGetLastError       := TWSAGetLastError(WinSockGetProc('WSAGetLastError'));
        _WSACancelAsyncRequest := TWSACancelAsyncRequest(WinSockGetProc('WSACancelAsyncRequest'));
        _WSAAsyncSelect        := TWSAAsyncSelect(WinSockGetProc('WSAAsyncSelect'));
        _GetServByName         := TGetServByName(WinSockGetProc('getservbyname'));
        _GetHostByName         := TGetHostByName(WinSockGetProc('gethostbyname'));
        _Socket                := TOpenSocket(WinSockGetProc('socket'));
        _Shutdown              := TShutdown(WinSockGetProc('shutdown'));
        _SetSockOpt            := TSetSockOpt(WinSockGetProc('setsockopt'));
        _GetSockOpt            := TGetSockOpt(WinSockGetProc('getsockopt'));
        _SendTo                := TSendTo(WinSockGetProc('sendto'));
        _Send                  := TSend(WinSockGetProc('send'));
        _Recv                  := TRecv(WinSockGetProc('recv'));
        _RecvFrom              := TRecvFrom(WinSockGetProc('recvfrom'));
        _ntohs                 := Tntohs(WinSockGetProc('ntohs'));
        _ntohl                 := Tntohl(WinSockGetProc('ntohl'));
        _Listen                := TListen(WinSockGetProc('listen'));
        _IoctlSocket           := TIoctlSocket(WinSockGetProc('ioctlsocket'));
        _Inet_ntoa             := TInet_ntoa(WinSockGetProc('inet_ntoa'));
        _Inet_addr             := TInet_addr(WinSockGetProc('inet_addr'));
        _htons                 := Thtons(WinSockGetProc('htons'));
        _GetSockName           := TGetSockName(WinSockGetProc('getsockname'));
        _GetPeerName           := TGetPeerName(WinSockGetProc('getpeername'));
        _Connect               := TConnect(WinSockGetProc('connect'));
        _CloseSocket           := TCloseSocket(WinSockGetProc('closesocket'));
        _Bind                  := TBind(WinSockGetProc('bind'));
        _Accept                := TAccept(WinSockGetProc('accept'));

        // *** API calls not needed ...
        //_WSASetLastError       := TWSASetLastError(WinSockGetProc('WSASetLastError'));
        //_WSAAsyncGetHostByName := TWSAAsyncGetHostByName(WinSockGetProc('WSAAsyncGetHostByName'));
        //_WSAAsyncGetHostByAddr := TWSAAsyncGetHostByAddr(WinSockGetProc('WSAAsyncGetHostByAddr'));
        //_GetProtoByName        := TGetProtoByName(WinSockGetProc('getprotobyname'));
        //_GetHostByAddr         := TGetHostByAddr(WinSockGetProc('gethostbyaddr'));
        //_GetHostName           := TGetHostName(WinSockGetProc('gethostname'));
        //_htonl                 := Thtonl(WinSockGetProc('htonl'));

      except
        FreeLibrary(FDllHandle);
        FDllHandle:=0;
        raise;
        end;

      {$IFDEF FPC}
      LastError := _WSAStartup($101, GInitData);
      {$ELSE}
      LastError := _WSAStartup(MAKEWORD(GReqVerLow, GReqVerHigh) { $202 $101}, GInitData);
      {$ENDIF}
      if LastError <> 0 then
        raise EWinSockException.CreateFmt('%s: WSAStartup error #%d',
                                           [winsockdll, LastError]);
      end;
  finally
    LibCS.Leave;
    end;
  end;

procedure WinSockUnload;
  begin
  LibCS.Enter;
  try
    if FDllHandle<>0 then
      begin
      try
        _WSACleanup;
      finally
        FreeLibrary(FDllHandle);
        FDllHandle:=0;
        end;
      end;
  finally
    LibCS.Leave;
    end;
  end;

type
  TMyWinSock=class
    public
    constructor Create;
    destructor Destroy; override;
    end;

var
  myWinSock:TMyWinSock;

{ TMyWinSock }

constructor TMyWinSock.Create;
  begin
  LibCS:=TRtcCritSec.Create;

  RTC_HWND_CLASS_NAME:=RtcSocketWindowClass.lpszClassName;
  WSocketRegisterClass;
  rtcInitMainHWND;
  end;

destructor TMyWinSock.Destroy;
  begin
  rtcReleaseMainHWND;
  WinSockUnload;

  WSocketUnregisterClass;
  LibCS.Free;
  inherited;
  end;

initialization
MyWinSock:=TMyWinSock.Create;
finalization
Garbage(MyWinSock);
end.
