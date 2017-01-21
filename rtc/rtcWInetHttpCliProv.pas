{
  "HTTP Client provider (WinInet)" - Copyright (c) Danijel Tkalcec
  @html(<br>)

  Using WinInet API to implement a HTTP Client
  connection provider through HTTP Proxy servers

  @exclude
}
unit rtcWInetHttpCliProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  rtcTrashcan,

  SysUtils,
  Windows,
  Classes,

  rtcSyncObjs,
  rtcThrPool,
  rtcFastStrings,

  rtcLog,
  rtcInfo,
  rtcConn,
  rtcConnProv,
  rtcThrConnProv;

const
  LOG_WINET_ERRORS:boolean=False;

type
  HINTERNET = Pointer;

{ INTERNET_BUFFERS - combines headers and data. May be chained for e.g. file }
{ upload or scatter/gather operations. For chunked read/write, lpcszHeader }
{ contains the chunked-ext }
  PInternetBuffers = ^INTERNET_BUFFERS;
  INTERNET_BUFFERS = record
    dwStructSize: DWORD;      { used for API versioning. Set to sizeof(INTERNET_BUFFERS) }
    Next: PInternetBuffers;   { chain of buffers }
    lpcszHeader: PAnsiChar;       { pointer to headers (may be NULL) }
    dwHeadersLength: DWORD;   { length of headers if not NULL }
    dwHeadersTotal: DWORD;    { size of headers if not enough buffer }
    lpvBuffer: Pointer;       { pointer to data buffer (may be NULL) }
    dwBufferLength: DWORD;    { length of data buffer if not NULL }
    dwBufferTotal: DWORD;     { total size of chunk, or content-length if not chunked }
    dwOffsetLow: DWORD;       { used for read-ranges (only used in HttpSendRequest2) }
    dwOffsetHigh: DWORD;
  end;

  RtcWInetException = class(Exception);

  TRtcWInetHttpClientProvider = class;

  TRtcWInetClientThread = class(TRtcThread)
  public
    RtcConn:TRtcWInetHttpClientProvider;
    Releasing:boolean;

  public
    constructor Create; override;
    destructor Destroy; override;

    function Work(Job:TObject):boolean; override;

    procedure OpenConn;
    procedure CloseConn(_lost:boolean);

    // procedure Connect;
    // procedure Disconnect;
    // procedure Release;
    end;

  TRtcWInetHttpClientProvider = class(TRtcThrClientProvider)
  private
    Client_Thread:TRtcWInetClientThread;

    Forc:boolean;

    FCS:TRtcCritSec;

    FBufferIn:INTERNET_BUFFERS;

    FOnInvalidResponse:TRtcEvent;

    FResponseBuffer:TRtcHugeString;

    FReadBuffer:string;

    FMaxHeaderSize:integer;
    FMaxResponseSize:integer;

    FHeaderOut:boolean;
    FHeaderEx:boolean;
    LenToWrite:int64;

    FRequest:TRtcClientRequest;
    FResponse:TRtcClientResponse;

    FDataWasSent:boolean;

    hSession, hConnect, hRequest: hInternet;
	  hStore, pContext: pointer;
    hStoreReady: boolean;

    FUseHttps: boolean;
    FUserName: string;
    FUserPassword: string;
    FCertSubject: string;
    FCertStoreType: TRtcCertStoreType;

  protected
    procedure Enter; override;
    procedure Leave; override;

    function GetClientThread:TRtcThread; override;

    procedure TriggerInvalidResponse; virtual;

    procedure AcceptResponse; virtual;

    function _Active:boolean;

    procedure OpenConnection;

    function SetupCertificate:boolean;

    procedure SendHeaderOut(const s:string);

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Connect(Force:boolean=False); override;
    procedure Disconnect; override;
    procedure Release; override;

    procedure InternalDisconnect; override;

    procedure LeavingEvent; virtual;

    procedure SetTriggerInvalidResponse(Event:TRtcEvent);

    procedure WriteHeader(SendNow:boolean=True); overload; virtual;
    procedure WriteHeader(const Header_Text:string; SendNow:boolean=True); overload; virtual;

    procedure Write(const s:string; SendNow:boolean=True); override;
    function Read:string; override;

    property Request:TRtcClientRequest read FRequest write FRequest;
    property Response:TRtcClientResponse read FResponse write FResponse;

    // Max. allowed size of the first (status) line in response header
    property MaxResponseSize:integer read FMaxResponseSize write FMaxResponseSize;
    // Max. allowed size of the complete response Header
    property MaxHeaderSize:integer read FMaxHeaderSize write FMaxHeaderSize;

    // Use HTTPS protocol instead of HTTP
    property useHttps:boolean read FUseHttps write FUseHttps;

    property UserName:string read FUserName write FUserName;
    property UserPassword:string read FUserPassword write FUserPassword;

    property CertStoreType:TRtcCertStoreType read FCertStoreType write FCertStoreType;
    property CertSubject:string read FCertSubject write FCertSubject;
    end;

implementation

const
  INTERNET_DEFAULT_HTTP_PORT = 80;                  {    "     "  HTTP   " }
  INTERNET_DEFAULT_HTTPS_PORT = 443;                {    "     "  HTTPS  " }
  INTERNET_OPEN_TYPE_PRECONFIG = 0;  { use registry configuration }
  INTERNET_SERVICE_HTTP = 3;

  INTERNET_OPTION_SECURITY_FLAGS              = 31;

  INTERNET_FLAG_NO_CACHE_WRITE = $04000000;  { don't write this item to the cache }
  INTERNET_FLAG_RELOAD = $80000000;                 { retrieve the original item }
  INTERNET_FLAG_SECURE = $00800000;  { use PCT/SSL if applicable (HTTP) }

  INTERNET_FLAG_IGNORE_CERT_CN_INVALID        = $00001000; { bad common name in X509 Cert. }
  INTERNET_FLAG_IGNORE_CERT_DATE_INVALID      = $00002000; { expired X509 Cert. }
  INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS      = $00004000; { ex: http:// to https:// }
  INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP       = $00008000; { ex: https:// to http:// }

  SECURITY_FLAG_IGNORE_REVOCATION             = $00000080;
  SECURITY_FLAG_IGNORE_UNKNOWN_CA             = $00000100;
  SECURITY_FLAG_IGNORE_WRONG_USAGE            = $00000200;
  SECURITY_FLAG_IGNORE_CERT_CN_INVALID        = INTERNET_FLAG_IGNORE_CERT_CN_INVALID;
  SECURITY_FLAG_IGNORE_CERT_DATE_INVALID      = INTERNET_FLAG_IGNORE_CERT_DATE_INVALID;
  SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTPS      = INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS;
  SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTP       = INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP;

  INTERNET_ERROR_BASE                         = 12000;
  ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED      = INTERNET_ERROR_BASE + 44;
  ERROR_INTERNET_INVALID_CA                   = INTERNET_ERROR_BASE + 45;
  HTTP_QUERY_RAW_HEADERS_CRLF                 = 22; { special: all headers }

const
  CRLF = #13#10;

  winetdll = 'wininet.dll';
  wincrypt = 'crypt32.dll';

  CERT_STORE_CLOSE_FORCE_FLAG = 1;

  X509_ASN_ENCODING:DWORD = 1;
  PKCS_7_ASN_ENCODING:DWORD = 65536;

  CERT_FIND_SUBJECT_STR_A = 458759;
  CERT_FIND_SUBJECT_STR_W = 524295;
  CERT_FIND_ISSUER_STR_A = 458756;
  CERT_FIND_ISSUER_STR_W = 524292;

  INTERNET_OPTION_CLIENT_CERT_CONTEXT = 84;

  HSR_INITIATE    = $00000008;                       { iterative operation (completed by HttpEndRequest) }

type
  INTERNET_PORT = Word;

  EWinCryptException=Class(Exception);
  EWinInetException=Class(Exception);

  TRtcBaseMessage=class
    end;

  HCERTSTORE = pointer;
  PCERT_INFO = pointer;

  CERT_CONTEXT = packed record
	  dwCertEncodingType:DWORD;
	  pbCertEncoded:pointer;
	  cbCertEncoded:DWORD;
	  pCertInfo:PCERT_INFO;
	  hCertStore:HCERTSTORE;
    end;

  PCCERT_CONTEXT = ^CERT_CONTEXT;

  {CertOpenSystemStoreA}
  TCertOpenSystemStore =        function(hprov:pointer;
                                         szSubsystemProtocol:LPTSTR):HCERTSTORE; stdcall;
  {CertCloseStore}
  TCertCloseStore =             function(hCertStore:HCERTSTORE;
                                         dwFlags:DWORD):BOOL; stdcall;
  {CertFindCertificateInStore}
  TCertFindCertificateInStore = function(hCertStore:HCERTSTORE;
                                         dwCertEncodingType:DWORD;
                                         dwFindFlags:DWORD;
                                         dwFindType:DWORD;
                                         pvFindPara:PChar;
                                         pPrevCertContext:PCCERT_CONTEXT):PCCERT_CONTEXT; stdcall;
  {CertFreeCertificateContext}
  TCertFreeCertificateContext = function(pCertContext:PCCERT_CONTEXT):BOOL; stdcall;

  {InternetOpen}
  TInternetOpen =               function(lpszAgent: PChar; dwAccessType: DWORD;
                                         lpszProxy, lpszProxyBypass:
                                         PChar; dwFlags: DWORD): HINTERNET; stdcall;

  {InternetConnect}
  TInternetConnect =             function(hInet: HINTERNET; lpszServerName: PChar;
                                          nServerPort: INTERNET_PORT;
                                          lpszUsername: PChar; lpszPassword: PChar;
                                          dwService: DWORD; dwFlags: DWORD;
                                          dwContext: DWORD): HINTERNET; stdcall;

  {InternetCloseHandle}
  TInternetCloseHandle =         function(hInet: HINTERNET): BOOL; stdcall;

  {InternetQueryOption}
  TInternetQueryOption =         function(hInet: HINTERNET; dwOption: DWORD;
                                          lpBuffer: Pointer;
                                          var lpdwBufferLength: DWORD): BOOL; stdcall;

  {HttpOpenRequest}
  THttpOpenRequest =             function(hConnect: HINTERNET; lpszVerb: PChar;
                                          lpszObjectName: PChar;
                                          lpszVersion: PChar; lpszReferrer: PChar;
                                          lplpszAcceptTypes: PLPSTR; dwFlags: DWORD;
                                          dwContext: DWORD): HINTERNET; stdcall;

  {HttpSendRequest}
  THttpSendRequest =             function(hRequest: HINTERNET; lpszHeaders: PChar;
                                          dwHeadersLength: DWORD; lpOptional: Pointer;
                                          dwOptionalLength: DWORD): BOOL; stdcall;

  {HttpSendRequestEx}
  THttpSendRequestEx =           function(hRequest: HINTERNET; lpBuffersIn: PInternetBuffers;
                                          lpBuffersOut: PInternetBuffers;
                                          dwFlags: DWORD; dwContext: DWORD): BOOL; stdcall;

  {HttpEndRequest}
  THttpEndRequest =              function(hRequest: HINTERNET;
                                          lpBuffersOut: PInternetBuffers; dwFlags: DWORD;
                                          dwContext: DWORD): BOOL; stdcall;

  {InternetReadFile}
  TInternetReadFile =            function(hFile: HINTERNET; lpBuffer: Pointer;
                                          dwNumberOfBytesToRead: DWORD;
                                          var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall;

  {InternetWriteFile}
  TInternetWriteFile =           function(hFile: HINTERNET; lpBuffer: Pointer;
                                          dwNumberOfBytesToWrite: DWORD;
                                          var lpdwNumberOfBytesWritten: DWORD): BOOL; stdcall;

  {HttpQueryInfo}
  THttpQueryInfo =               function(hRequest: HINTERNET; dwInfoLevel: DWORD;
                                          lpvBuffer: Pointer; var lpdwBufferLength: DWORD;
                                          var lpdwReserved: DWORD): BOOL; stdcall;

  {InternetSetOption}
  TInternetSetOption =           function(hInet: HINTERNET; dwOption: DWORD;
                                          lpBuffer: Pointer;
                                          dwBufferLength: DWORD): BOOL; stdcall;


var
  CertOpenSystemStore: TCertOpenSystemStore;
  CertCloseStore: TCertCloseStore;
  CertFindCertificateInStore: TCertFindCertificateInStore;
  CertFreeCertificateContext: TCertFreeCertificateContext;

  InternetOpen: TInternetOpen;
  InternetConnect: TInternetConnect;
  InternetCloseHandle: TInternetCloseHandle;
  HttpOpenRequest: THttpOpenRequest;
  HttpSendRequest: THttpSendRequest;
  HttpSendRequestEx: THttpSendRequestEx;
  HttpEndRequest: THttpEndRequest;
  InternetReadFile: TInternetReadFile;
  InternetWriteFile: TInternetWriteFile;
  HttpQueryInfo: THttpQueryInfo;
  InternetSetOption: TInternetSetOption;
  InternetQueryOption: TInternetQueryOption;

  LibCS:TRtcCritSec;

  Message_WSStop,
  Message_WSRelease,
  Message_WSOpenConn,
  Message_WSCloseConn:TRtcBaseMessage;

  FDllHandle:THandle = 0;
  FDllHandle2:THandle = 0;

function WinCryptGetProc(const ProcName : String) : Pointer;
  begin
  if Length(ProcName) = 0 then
    Result := nil
  else
    begin
    Result := GetProcAddress(FDllHandle, @ProcName[1]);
    if Result = nil then
      raise EWinCryptException.Create('Procedure ' + ProcName +
                                      ' not found in ' + wincrypt +
                                      ' Error #' + IntToStr(GetLastError));
    end;
  end;

procedure WinCryptLoad;
  begin
  LibCS.Enter;
  try
    if FDllHandle = 0 then
      begin
      FDllHandle := LoadLibrary(@wincrypt[1]);
      if FDllHandle = 0 then
        raise EWinCryptException.Create('Unable to load ' + wincrypt +
                                      ' Error #' + IntToStr(GetLastError));

      try
        CertOpenSystemStore := TCertOpenSystemStore(WinCryptGetProc('CertOpenSystemStoreA'));
        CertCloseStore := TCertCloseStore(WinCryptGetProc('CertCloseStore'));
        CertFindCertificateInStore := TCertFindCertificateInStore(WinCryptGetProc('CertFindCertificateInStore'));
        CertFreeCertificateContext := TCertFreeCertificateContext(WinCryptGetProc('CertFreeCertificateContext'));
      except
        FreeLibrary(FDllHandle);
        FDllHandle:=0;
        raise;
        end;
      end;
  finally
    LibCS.Leave;
    end;
  end;

procedure WinCryptUnload;
  begin
  LibCS.Enter;
  try
    if FDllHandle<>0 then
      begin
      FreeLibrary(FDllHandle);
      FDllHandle:=0;
      end;
  finally
    LibCS.Leave;
    end;
  end;

function WinInetGetProc(const ProcName : String) : Pointer;
  begin
  if Length(ProcName) = 0 then
    Result := nil
  else
    begin
    Result := GetProcAddress(FDllHandle2, @ProcName[1]);
    if Result = nil then
      raise EWinInetException.Create('Procedure ' + ProcName +
                                      ' not found in ' + winetdll +
                                      ' Error #' + IntToStr(GetLastError));
    end;
  end;

procedure WinInetLoad;
  begin
  LibCS.Enter;
  try
    if FDllHandle2 = 0 then
      begin
      FDllHandle2 := LoadLibrary(@winetdll[1]);
      if FDllHandle2 = 0 then
        raise EWinCryptException.Create('Unable to load ' + winetdll +
                                      ' Error #' + IntToStr(GetLastError));

      try
        InternetOpen := TInternetOpen(WinInetGetProc('InternetOpenA'));
        InternetConnect := TInternetConnect(WinInetGetProc('InternetConnectA'));
        InternetCloseHandle := TInternetCloseHandle(WinInetGetProc('InternetCloseHandle'));
        HttpOpenRequest := THttpOpenRequest(WinInetGetProc('HttpOpenRequestA'));
        HttpSendRequest := THttpSendRequest(WinInetGetProc('HttpSendRequestA'));
        HttpSendRequestEx := THttpSendRequestEx(WinInetGetProc('HttpSendRequestExA'));
        HttpEndRequest := THttpEndRequest(WinInetGetProc('HttpEndRequestA'));
        InternetReadFile := TInternetReadFile(WinInetGetProc('InternetReadFile'));
        InternetWriteFile := TInternetWriteFile(WinInetGetProc('InternetWriteFile'));
        HttpQueryInfo := THttpQueryInfo(WinInetGetProc('HttpQueryInfoA'));
        InternetSetOption := TInternetSetOption(WinInetGetProc('InternetSetOptionA'));
        InternetQueryOption := TInternetQueryOption(WinInetGetProc('InternetQueryOptionA'));
      except
        FreeLibrary(FDllHandle2);
        FDllHandle2:=0;
        raise;
        end;
      end;
  finally
    LibCS.Leave;
    end;
  end;

procedure WinInetUnload;
  begin
  LibCS.Enter;
  try
    if FDllHandle2<>0 then
      begin
      FreeLibrary(FDllHandle2);
      FDllHandle2:=0;
      end;
  finally
    LibCS.Leave;
    end;
  end;

{ TRtcWInetHttpClientProvider }

constructor TRtcWInetHttpClientProvider.Create;
  begin
  inherited;
  FUseHttps:=False;

  FCS:=TRtcCritSec.Create;
  FResponseBuffer:=TRtcHugeString.Create;

  FDataWasSent:=False;
  SetLength(FReadBuffer,32000);

  hStore:=nil;
  hStoreReady:=False;
  end;

destructor TRtcWInetHttpClientProvider.Destroy;
  begin
  Silent:=True;
  Closing:=True;

  InternalDisconnect;

  if assigned(Client_Thread) then
    TRtcThread.PostJob(Client_Thread, Message_WSStop, True);

  FResponseBuffer.Free;
  FResponseBuffer:=nil;

  FReadBuffer:='';
  FCS.Free;

  if hStore<>nil then
    begin
    try
      CertCloseStore(hStore,CERT_STORE_CLOSE_FORCE_FLAG);
    except
      end;
    hStore:=nil;
    hStoreReady:=False;
    end;

  inherited;
  end;

procedure TRtcWInetHttpClientProvider.Enter;
  begin
  FCS.Enter;
  end;

procedure TRtcWInetHttpClientProvider.Leave;
  begin
  FCS.Leave;
  end;

procedure TRtcWInetHttpClientProvider.SetTriggerInvalidResponse(Event: TRtcEvent);
  begin
  FOnInvalidResponse:=Event;
  end;

procedure TRtcWInetHttpClientProvider.TriggerInvalidResponse;
  begin
  if assigned(FOnInvalidResponse) then
    FOnInvalidResponse;
  end;

function TRtcWInetHttpClientProvider.GetClientThread: TRtcThread;
  begin
  Result:=Client_Thread;
  end;

procedure TRtcWInetHttpClientProvider.Connect(Force: boolean);
  begin
  if assigned(Client_Thread) and not inThread then
    TRtcThread.PostJob(Client_Thread, Message_WSOpenConn)
  else
    begin
    if GetMultiThreaded then
      begin
      if not assigned(Client_Thread) then
        begin
        Client_Thread:=TRtcWInetClientThread.Create;
        Client_Thread.RtcConn:=self;
        end;
      Forc:=Force;
      TRtcThread.PostJob(Client_Thread, Message_WSOpenConn);
      end
    else
      OpenConnection;
    end;
  end;

procedure TRtcWInetHttpClientProvider.OpenConnection;
  var
    myPort:integer;
  begin
  if (State=conActive) or (State=conActivating) then Exit; // already connected !!!

  if State<>conInactive then
    raise Exception.Create('Can not connect again, connection in use.');

  if FUseHttps then
    myPort:=StrToIntDef(GetPort,INTERNET_DEFAULT_HTTPS_PORT)
  else
    myPort:=StrToIntDef(GetPort,INTERNET_DEFAULT_HTTP_PORT);

  WinInetLoad;

  try
    if CertStoreType<>certNone then
      WinCryptLoad;

    Lost:=True;
    Closing:=False;
    Silent:=False;

    Request.Init;
    Response.Clear;

    State:=conActivating;

    TriggerConnectionOpening(Forc);

    try
      hSession := InternetOpen(nil, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    except
      hSession := nil;
      end;

    if hSession=nil then
      raise RtcWInetException.Create('Error initializing Internet API [Code #'+IntToStr(GetLastError)+'].');

    try
      hConnect := InternetConnect(hSession, PChar(GetAddr), myPort,
                                  PChar(FUserName), PChar(FUserPassword),
                                  INTERNET_SERVICE_HTTP, 0, 0);
    except
      hConnect := nil;
      end;

    if hConnect=nil then
      raise RtcWInetException.Create('Error opening Internet Connection [Code #'+IntToStr(GetLastError)+'].');

    State:=conActive;

    TriggerConnecting;
    TriggerConnect;
  except
    on E:Exception do
      begin
      if hConnect<>nil then
        begin
        InternetCloseHandle(hConnect);
        hConnect:=nil;
        end;
      if hSession<>nil then
        begin
        InternetCloseHandle(hSession);
        hSession:=nil;
        end;

      TriggerConnectionClosing;
      TriggerConnectError(E);
      TriggerReadyToRelease;
      end;
    end;
  end;

procedure TRtcWInetHttpClientProvider.Disconnect;
  var
    hReq:HINTERNET;
  begin
  Lost:=False;
  if assigned(Client_Thread) and not inThread then
    begin
    if TRtcThread.Lock(Client_Thread) then
      try
        if hRequest<>nil then
          begin
          try
            hReq:=hRequest;
            hRequest:=nil;
            InternetCloseHandle(hReq);
          except
            end;
          end;
        TRtcThread.PostJob(Client_Thread, Message_WSCloseConn);
      finally
        TRtcThread.UnLock;
        end;
    end
  else
    InternalDisconnect;
  end;

procedure TRtcWInetHttpClientProvider.InternalDisconnect;
  var
    hReq:HINTERNET;
  begin
  if Closing then Exit;

  Closing:=True;

  State:=conClosing;

  if hRequest<>nil then
    begin
    try
      hReq:=hRequest;
      hRequest:=nil;
      InternetCloseHandle(hReq);
    except
      end;
    end;

  if hConnect<>nil then
    begin
    try
      InternetCloseHandle(hConnect);
    except
      end;
    hConnect:=nil;
    end;

  if hSession<>nil then
    begin
    try
      InternetCloseHandle(hSession);
    except
      end;
    hSession:=nil;
    end;

  if State=conClosing then
    begin
    TriggerDisconnecting;
    TriggerConnectionClosing;

    State:=conInactive;
    try
      TriggerDisconnect;
      if Lost then
        TriggerConnectLost;
    except
      end;

    FHeaderOut:=False;
    FDataWasSent:=False;
    TriggerReadyToRelease;
    end;
  end;

function TRtcWInetHttpClientProvider.Read: string;
  begin
  if not _Active then
    begin
    Result:='';
    Exit;
    end;

  if FResponseBuffer.Size>0 then
    begin
    Result:=FResponseBuffer.Get;
    FResponseBuffer.Clear;
    end
  else
    Result:='';
  end;

procedure TRtcWInetHttpClientProvider.SendHeaderOut(const s:string);
  var
    MyHeader:string;
    certOK:boolean;
    ex:Exception;
    lastErr:DWORD;
  begin
  FHeaderOut:=False;
  FHeaderEx:=False;
  certOK:=False;

  myHeader:=Request.HeaderText;
  repeat
    if hRequest=nil then
      Break
    else if Request.Contentlength=length(s) then // Send content out in 1 API call
      begin
      FHeaderEx:=False;

      if Request.ContentLength=0 then // No content
        begin
        if myHeader<>'' then
          FHeaderOut:=HttpSendRequest(hRequest, Addr(MyHeader[1]), length(MyHeader), nil, 0)
        else
          FHeaderOut:=HttpSendRequest(hRequest, nil, 0, nil, 0);
        end
      else // Content in "s"
        begin
        if myHeader<>'' then
          FHeaderOut:=HttpSendRequest(hRequest, Addr(MyHeader[1]), length(MyHeader), Addr(s[1]), length(s))
        else
          FHeaderOut:=HttpSendRequest(hRequest, nil, 0, Addr(s[1]), length(s));
        end;
      end
    else
      begin

      FBufferIn.dwStructSize := SizeOf(FBufferIn);
      FBufferIn.dwBufferTotal := Request.ContentLength;
      FBufferIn.dwBufferLength := 0;
      FBufferIn.dwHeadersTotal := length(MyHeader);
      FBufferIn.dwHeadersLength := length(MyHeader);
      FBufferIn.dwOffsetHigh := 0;
      FBufferIn.dwOffsetLow := 0;
      if length(MyHeader)>0 then
        FBufferIn.lpcszHeader := Addr(MyHeader[1])
      else
        FBufferIn.lpcszHeader := nil;
      FBufferIn.lpvBuffer := nil;
      FBufferIn.Next := nil;

      FHeaderOut := HttpSendRequestEx(hRequest, @FBufferIn, nil, HSR_INITIATE, 0);
      FHeaderEx := FHeaderOut;
      end;

    if hRequest=nil then
      begin
      FHeaderOut:=False;
      Break;
      end
    else if not FHeaderOut then
      begin
      lastErr:=GetLastError;
      if (lastErr = ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED) then
        begin
        if certOK or (FCertStoreType=certNone) then
          Break
        else
          begin
          certOK:=True;
          if not SetupCertificate then Break;
          end;
        end
      else if (lastErr = ERROR_INTERNET_INVALID_CA) then
        begin
        if certOK or (FCertStoreType=certNone) then
          Break
        else
          begin
          certOK:=True;
          if not SetupCertificate then Break;
          end;
        end
      else
        Break;
      end;
    until FHeaderOut;

  if not FHeaderOut then
    begin
    if _Active then
      begin
      ex:=RtcWInetException.Create('Error Sending the Request [Code #'+IntToStr(GetLastError)+'].');
      try
        TriggerException(ex);
      finally
        ex.Free;
        end;
      InternalDisconnect;
      end;
    end
  else
    begin
    LenToWrite:=Request.ContentLength-length(s);

    FDataOut:=length(Request.Method)+length(Request.URI)+10;
    if not FHeaderEx then
      begin
      FDataOut:=FDataOut+length(myHeader)+length(s);
      Request.ContentOut:=length(s);
      end
    else
      begin
      FDataOut:=FDataOut+length(myHeader);
      Request.ContentOut:=0;
      end;
    TriggerDataOut;

    FDataWasSent:=True; // will call DataSent
    end;
  end;

procedure TRtcWInetHttpClientProvider.WriteHeader(SendNow:boolean=True);
  var
    ex:Exception;
    hReq:HINTERNET;
  begin
  if not _Active then Exit;

  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  if hRequest<>nil then
    begin
    try
      hReq:=hRequest;
      hRequest:=nil;
      InternetCloseHandle(hReq);
    except
      end;
    end;

  if FUseHttps then
    hRequest := HttpOpenRequest(hConnect, PChar(Request.Method), PChar(Request.URI), 'HTTP/1.1',
                '', nil, INTERNET_FLAG_RELOAD or INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_SECURE, 0)
  else
    hRequest := HttpOpenRequest(hConnect, PChar(Request.Method), PChar(Request.URI), 'HTTP/1.1',
                '', nil, INTERNET_FLAG_RELOAD or INTERNET_FLAG_NO_CACHE_WRITE, 0);

  if hRequest=nil then
    begin
    if _Active then
      begin
      ex:=RtcWInetException.Create('Error opening HTTP Request [Code #'+IntToStr(GetLastError)+'].');
      try
        TriggerException(ex);
      finally
        ex.Free;
        end;
      InternalDisconnect;
      end;
    Exit;
    end;

  if FUseHttps and (FCertStoreType<>certNone) and not hStoreReady then
    SetupCertificate;

  if SendNow or (Request.ContentLength=0) then
    SendHeaderOut('');

  if hRequest=nil then
    begin
    if _Active then
      InternalDisconnect;
    Exit;
    end;

  if not FHeaderOut then
    begin
    LenToWrite:=Request.ContentLength;
    FDataWasSent:=True;
    end;

  Request.Started:=True;
  Request.Active:=True;
  end;

procedure TRtcWInetHttpClientProvider.WriteHeader(const Header_Text: string; SendNow:boolean=True);
  begin
  if not _Active then Exit;

  Request.HeaderText:=Header_Text;
  WriteHeader(SendNow);
  end;

procedure TRtcWInetHttpClientProvider.Write(const s: string; SendNow:boolean=True);
  var
    bOK:boolean;
    ex:Exception;
    bWritten:DWORD;
  begin
  if not _Active then Exit;

  if not Request.Active then
    raise Exception.Create('Sending data without header.');

  if not FHeaderOut then
    SendHeaderOut(s);

  if s='' then Exit;

  if FHeaderEx then
    begin
    bOK := InternetWriteFile(hRequest, Addr(s[1]), length(s), bWritten);
    if not bOK or (bWritten<>dword(length(s))) then
      if _Active then
        begin
        ex:=RtcWInetException.Create('Error Sending the Request [Code #'+IntToStr(GetLastError)+'].');
        try
          TriggerException(ex);
        finally
          ex.Free;
          end;
        InternalDisconnect;
        Exit;
        end;

    FDataOut:=length(s);
    LenToWrite:=LenToWrite-FDataOut;
    Request.ContentOut:=Request.ContentOut + FDataOut;

    TriggerDataOut;
    FDataWasSent:=True; // will call DataSent
    end;
  end;

procedure TRtcWInetHttpClientProvider.LeavingEvent;
  begin
  If _Active and FDataWasSent then
    begin
    FDataWasSent:=False;

    if LenToWrite=0 then
      begin
      Request.Complete:=True;
      TriggerDataSent;
      if Request.Complete and not Response.Done then
        AcceptResponse;
      end
    else
      TriggerDataSent;
    end;
  TriggerReadyToRelease;
  end;

procedure TRtcWInetHttpClientProvider.AcceptResponse;
  var
    dwBufLen,dwIndex:DWord;
    LenToRead:int64;

    hReq:HINTERNET;

    InBuffer,
    myHeader:string;

    BytesRead:DWord;

    ex:Exception;

  function ReadNextBlock:boolean;
    var
      ReadNowBytes:int64;
    begin
    BytesRead:=0;

    if LenToRead>0 then
      begin
      ReadNowBytes:=LenToRead;
      if ReadNowBytes>length(FReadBuffer) then
        ReadNowBytes:=length(FReadBuffer);
      end
    else
      ReadNowBytes:=length(FReadBuffer);

    if hRequest=nil then
      Result:=False
    else
      Result:=InternetReadFile(hRequest, Addr(FReadBuffer[1]), ReadNowBytes, BytesRead);

    if Result then
      if BytesRead>0 then
        begin
        FDataIn:=BytesRead;
        TriggerDataIn;
        end;
    end;

  begin
  if not _Active then Exit;

  if not FHeaderOut then // This should not happen!
    raise Exception.Create('AcceptResponse was called before WriteHeader.');

  if FHeaderEx then
    HttpEndRequest(hRequest, nil, 0, 0);

  FHeaderOut:=False;
  Response.Started:=True;
  Response.Receiving:=True;

  FResponseBuffer.Clear;

  // Get Raw Header ...
  myHeader:=' ';
  dwBufLen:=1;
  dwIndex:=0;

  if hRequest=nil then
    begin
    InternalDisconnect;
    Exit;
    end;

  try
    if not HttpQueryInfo(hRequest, HTTP_QUERY_RAW_HEADERS_CRLF, Addr(myHeader[1]), dwBufLen, dwIndex) then
      begin
      if not _Active then Exit;

      if GetLastError<>ERROR_INSUFFICIENT_BUFFER then
        begin
        if _Active then
          begin
          ex:=RtcWInetException.Create('Error Reading a Response Header [Code #'+IntToStr(GetLastError)+'].');
          try
            TriggerException(ex);
          finally
            ex.Free;
            end;
          InternalDisconnect;
          end;
        Exit;
        end
      else if hRequest<>nil then
        begin
        SetLength(myHeader, dwBufLen);
        if not HttpQueryInfo(hRequest, HTTP_QUERY_RAW_HEADERS_CRLF, Addr(myHeader[1]), dwBufLen, dwIndex) then
          begin
          if _Active then
            begin
            ex:=RtcWInetException.Create('Error Reading a Response Header [Code #'+IntToStr(GetLastError)+'].');
            try
              TriggerException(ex);
            finally
              ex.Free;
              end;
            InternalDisconnect;
            end;
          Exit;
          end;
        end
      else
        begin
        InternalDisconnect;
        Exit;
        end;
      end
    else
      SetLength(myHeader,dwBufLen);

    FDataIn:=length(myHeader);
    TriggerDataIn;

    Response.HeaderText:=myHeader;

    if Request.Method='HEAD' then
      begin
      LenToRead:=0;
      Response.Done:=True;
      if _Active then
        TriggerDataReceived;
      Exit;
      end
    else if Response['CONTENT-LENGTH']<>'' then
      begin
      LenToRead:=Response.ContentLength;
      if LenToRead=0 then
        begin
        Response.Done:=True;
        if _Active then
          TriggerDataReceived;
        Exit;
        end;
      end
    else
      LenToRead:=-1;

    InBuffer:='';

    while _Active and not Response.Done do
      begin
      if not ReadNextBlock then
        begin
        if _Active then
          begin
          ex:=RtcWInetException.Create('Error Reading a Response Header [Code #'+IntToStr(GetLastError)+'].');
          try
            TriggerException(ex);
          finally
            ex.Free;
            end;
          InternalDisconnect;
          end;
        Exit;
        end
      else if BytesRead>0 then
        InBuffer:=InBuffer+Copy(FReadBuffer,1,BytesRead)
      else if (LenToRead>0) and (BytesRead=0) then
        begin
        if _Active then
          begin
          ex:=RtcWInetException.Create('Error Reading a Response Header [Code #'+IntToStr(GetLastError)+'].');
          try
            TriggerException(ex);
          finally
            ex.Free;
            end;
          InternalDisconnect;
          end;
        Exit;
        end;

      if (LenToRead>0) or (LenToRead=-1) then
        begin
        if (LenToRead>length(InBuffer)) or // need more than we have
           (LenToRead=-1) then // size unknown
          begin
          Response.ContentIn:=Response.ContentIn + length(InBuffer);

          if LenToRead>0 then
            Dec(LenToRead, length(InBuffer))
          else if BytesRead=0 then // last byte read
            begin
            LenToRead:=0;
            Response.Done:=True;
            Request.Active:=False;

            FHeaderOut:=False;
            end;

          FResponseBuffer.Add(InBuffer);

          InBuffer:='';
          end
        else
          begin
          Response.ContentIn:=Response.ContentIn + LenToRead;

          FResponseBuffer.Add(Copy(InBuffer,1,LenToRead));

          Delete(InBuffer,1,LenToRead);

          LenToRead:=0;
          Response.Done:=True;
          Request.Active:=False;

          FHeaderOut:=False;
          end;
        end
      else
        begin
        Response.Done:=True;
        Request.Active:=False;

        FHeaderOut:=False;
        end;

      if not _Active then Exit;

      if Response.Done then
        begin
        TriggerDataReceived;
        Exit;
        end
      else
        begin
        TriggerDataReceived;
        Response.Started:=False;
        end;
      end;
  finally
    if _Active and not Request.Active then
      begin
      FResponseBuffer.Clear;
      if hRequest<>nil then
        begin
        try
          hReq:=hRequest;
          hRequest:=nil;
          InternetCloseHandle(hReq);
        except
          end;
        end;
      end;
    end;
  end;

function TRtcWInetHttpClientProvider._Active: boolean;
  begin
  Result:=not Closing and (FState in [conActive,conActivating]);
  end;

procedure TRtcWInetHttpClientProvider.Release;
  begin
  if assigned(Client_Thread) then
    TRtcThread.PostJob(Client_Thread, Message_WSRelease, True)
  else
    inherited;
  end;

function TRtcWInetHttpClientProvider.SetupCertificate:boolean;
  var
    lpszStoreName,
    lpszSubjectName:PChar;
    dwFlags, dwBuffLen:DWORD;
    pDWFlags:^DWORD;
    res:bool;
  begin
  Result:=False;

  if hStore<>nil then
    begin
    try
      CertCloseStore(hStore, CERT_STORE_CLOSE_FORCE_FLAG);
    except
      end;
    hStore:=nil;
    hStoreReady:=False;
    end;

  if FCertStoreType=certAny then
    begin
    dwBuffLen:=sizeof(dwFlags);
    pdwFlags:=addr(dwFlags);
    InternetQueryOption (hRequest, INTERNET_OPTION_SECURITY_FLAGS,
            pdwFlags, dwBuffLen);

    pdwFlags^ := pdwFlags^
                or SECURITY_FLAG_IGNORE_UNKNOWN_CA
                or SECURITY_FLAG_IGNORE_CERT_CN_INVALID
                or SECURITY_FLAG_IGNORE_CERT_DATE_INVALID
                or SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTPS
                or SECURITY_FLAG_IGNORE_REDIRECT_TO_HTTP
                or SECURITY_FLAG_IGNORE_WRONG_USAGE
                or SECURITY_FLAG_IGNORE_REVOCATION;

    res := InternetSetOption (hRequest, INTERNET_OPTION_SECURITY_FLAGS,
                              pdwFlags, dwBuffLen );

    if res then
      begin
      // hStoreReady:=True;
      Result:=True;
      end;
    end
  else
    begin
    case FCertStoreType of
      certMY: lpszStoreName := 'MY';
      certCA: lpszStoreName := 'CA';
      certROOT: lpszStoreName := 'ROOT';
      certSPC: lpszStoreName := 'SPC';
      else Exit;
      end;

    hStore := CertOpenSystemStore(nil, lpszStoreName);
    if hStore<>nil then
      begin
      lpszSubjectName:=PChar(FCertSubject);

      pContext := CertFindCertificateInStore(hStore,
          X509_ASN_ENCODING or PKCS_7_ASN_ENCODING,
          0, CERT_FIND_SUBJECT_STR_A, lpszSubjectName, nil);

      if (pContext<>nil) then
        begin
        if hRequest<>nil then
          begin
          res := InternetSetOption(hRequest,
                                   INTERNET_OPTION_CLIENT_CERT_CONTEXT,
                                   pContext, sizeof(CERT_CONTEXT));
          if res then
            begin
            hStoreReady:=True;
            Result:=True;
            end;
          end;
        end;
      end;
    end;
  end;

{ TRtcWInetClientThread }

constructor TRtcWInetClientThread.Create;
  begin
  inherited;
  RtcConn:=nil;
  end;

procedure TRtcWInetClientThread.OpenConn;
  begin
  RtcConn.OpenConnection;
  end;

procedure TRtcWInetClientThread.CloseConn(_lost:boolean);
  begin
  if assigned(RtcConn) then
    begin
    try
      RtcConn.Lost:=_lost;
      if not Releasing then
        RtcConn.InternalDisconnect;
    except
      on E:Exception do
        if LOG_WINET_ERRORS then
          Log('WInetClientThread.CloseConn : RtConn.InternalDisconnect',E);
        // ignore exceptions
      end;
    end;
  end;

destructor TRtcWInetClientThread.Destroy;
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

function TRtcWInetClientThread.Work(Job: TObject):boolean;
  begin
  Result:=False;
  try
    if Job=Message_WSOpenConn then
      OpenConn
    else if Job=Message_WSCloseConn then
      CloseConn(false)
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
    else
      Result:=inherited Work(Job);
  except
    on E:Exception do
      begin
      if LOG_WINET_ERRORS then
        Log('WInetClientThread.Work',E);
      CloseConn(true);
      // raise;
      end;
    end;
  end;

type
  TMyWinInet=class
    public
    constructor Create;
    destructor Destroy; override;
    end;

var
  MyWinInet:TMyWinInet;

{ TMyWinInet }

constructor TMyWinInet.Create;
  begin
  inherited;
  LibCS:=TRtcCritSec.Create;

  Message_WSOpenConn:=TRtcBaseMessage.Create;
  Message_WSCloseConn:=TRtcBaseMessage.Create;
  Message_WSStop:=TRtcBaseMessage.Create;
  Message_WSRelease:=TRtcBaseMessage.Create;
  end;

destructor TMyWinInet.Destroy;
  begin
  WinInetUnload;
  WinCryptUnload;

  Message_WSOpenConn.Free;
  Message_WSCloseConn.Free;
  Message_WSStop.Free;
  Message_WSRelease.Free;
  LibCS.Free;
  inherited;
  end;

initialization
MyWinInet:=TMyWinInet.Create;
finalization
Garbage(MyWinInet);
end.
