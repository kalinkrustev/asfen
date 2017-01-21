{
  "ISAPI Server Connection Provider" - Copyright (c) Danijel Tkalcec
  @html(<br>)

  @exclude
}
unit rtcISAPISrvProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  Windows, Classes, SysUtils,

  isapi2,

  rtcFastStrings,
  rtcLog, rtcSyncObjs, rtcConn,
  rtcConnProv, rtcThrConnProv;

type
  TRtcISAPIServerProvider = class(TRtcNoThrServerProvider)
  private
    ECB: TEXTENSION_CONTROL_BLOCK;

    FCS:TRtcCritSec;
    FRequest:TRtcServerRequest;
    FResponse:TRtcServerResponse;

    FRequestBuffer:TRtcHugeString;

    LenToWrite:int64; // number of bytes to write out using inherited Write()
    LenToSend:int64;

    FHeaderOut:boolean;

  protected
    procedure Enter; override;
    procedure Leave; override;

    procedure CopyFrom(Dup:TRtcConnectionProvider);

    function ServerVariable(s:string; len:cardinal):string;

  public
    constructor Create; override;
    destructor Destroy; override;

    function GetParent:TRtcConnectionProvider; override;

    procedure Connect(var _ECB: TEXTENSION_CONTROL_BLOCK);
    procedure ExecuteRequest;

    procedure Listen; override;
    procedure Disconnect; override;
    procedure InternalDisconnect; override;

    procedure TriggerDataSent; override;
    procedure TriggerDataOut; override;

    procedure WriteHeader; overload;
    procedure WriteHeader(const Header_Text:string); overload;

    procedure Write(const ResultData:string; SendNow:boolean=True); override;
    function Read:string; override;

    property Request:TRtcServerRequest read FRequest write FRequest;
    property Response:TRtcServerResponse read FResponse write FResponse;
    end;

implementation

{ TRtcISAPIServerProvider }

const
  CRLF:string=#13#10;

constructor TRtcISAPIServerProvider.Create;
  begin
  inherited;
  FCS:=TRtcCritSec.Create;

  FRequestBuffer:=TRtcHugeString.Create;

  LenToWrite:=0;
  LenToSend:=0;
  FHeaderOut:=False;

  FRequest:=nil;
  FResponse:=nil;
  end;

destructor TRtcISAPIServerProvider.Destroy;
  begin
  Enter;
  try
    FRequestBuffer.Free;

    LenToWrite:=0;
    LenToSend:=0;
    FHeaderOut:=False;
  finally
    Leave;
    FCS.Free;
    FCS:=nil;
    end;
  inherited;
  end;

procedure TRtcISAPIServerProvider.WriteHeader;
  var
    s1,s2:string;
    len:DWORD;
  begin
  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  s1:=IntToStr(Response.StatusCode)+' '+Response.StatusText;
  s2:=Response.HeaderText;
  if Request.Close then s2:=s2+'Connection: close'+CRLF;
  s2:=s2+CRLF;

  Response.Sending:=True;
  Response.Started:=True;

  if Response.SendContent and
    (Response['CONTENT-LENGTH']='')  then // streaming data
    begin
    raise Exception.Create('Streaming content not supported by ISAPI.');
    LenToWrite:=-1;
    LenToSend:=-1;
    end
  else
    begin
    if not Response.SendContent then
      Response['CONTENT-LENGTH']:='';

    LenToWrite:=Response.ContentLength;
    LenToSend:=LenToWrite;
    end;

  Response.Sent:=LenToWrite=0;
  if Response.Sent then
    TriggerLastWrite;
    
  FHeaderOut:=True;

  s1:=s1+#0;
  s2:=s2+#0;
  len:=length(s1);

  ECB.dwHttpStatusCode:=Response.StatusCode;
  ECB.ServerSupportFunction(ECB.ConnID, HSE_REQ_SEND_RESPONSE_HEADER, @s1[1], @len, @s2[1]);

  FDataOut:=0;
  end;

procedure TRtcISAPIServerProvider.WriteHeader(const Header_Text:string);
  var
    s1,s2:string;
    len:DWORD;
  begin
  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  if Header_Text<>'' then
    begin
    Response.HeaderText:=Header_Text;

    s1:=IntToStr(Response.StatusCode)+' '+Response.StatusText;
    s2:=Response.HeaderText;
    if Request.Close then s2:=s2+'Connection: close'+CRLF;
    s2:=s2+CRLF;
    end
  else
    begin
    raise Exception.Create('Streaming content not supported by ISAPI.');
    s1:='';
    s2:='';
    Request.Close:=True;
    end;

  Response.Sending:=True;
  Response.Started:=True;

  if Response.SendContent and
    (Response['CONTENT-LENGTH']='')  then // streaming data
    begin
    raise Exception.Create('Streaming content not supported by ISAPI.');
    LenToWrite:=-1;
    LenToSend:=-1;
    end
  else
    begin
    if not Response.SendContent then
      Response['CONTENT-LENGTH']:='';

    LenToWrite:=Response.ContentLength;
    LenToSend:=LenToWrite;
    end;

  Response.Sent:=LenToWrite=0;
  if Response.Sent then
    TriggerLastWrite;

  FHeaderOut:=True;

  FHeaderOut:=True;

  len:=length(s1);
  s1:=s1+#0;
  s2:=s2+#0;

  ECB.dwHttpStatusCode:=Response.StatusCode;
  ECB.ServerSupportFunction(ECB.ConnID, HSE_REQ_SEND_RESPONSE_HEADER, @s1[1], @len, @s2[1]);

  FDataOut:=0;
  end;

procedure TRtcISAPIServerProvider.Write(const ResultData: string; SendNow:boolean=True);
  var
    len,len2,loc:cardinal;
  begin
  if length(ResultData)=0 then Exit;

  if not FHeaderOut then
    raise Exception.Create('Trying to send Data without Header. Call WriteHeader before Write.');

  if LenToWrite>=0 then
    begin
    if length(ResultData)>LenToWrite then
      raise Exception.Create('Trying to send more Data out than specified in Header.');

    Dec(LenToWrite, length(ResultData));
    end;

  Response.Sent:=LenToWrite=0;
  Response.ContentOut:=Response.ContentOut + length(ResultData);

  if Response.Sent then
    TriggerLastWrite;

  len:=length(ResultData);
  len2:=len;

  loc:=1;
  repeat
    if not ECB.WriteClient(ECB.ConnID,@ResultData[loc],len,0) then
      raise Exception.Create('Error sending data out (WriteClient returned FALSE).')
    else if len=0 then
      raise Exception.Create('Error sending data out (WriteClient length = 0).');
    Inc(loc,len); len2:=len2-len;
    len:=len2;
    until len2=0;

  FDataOut:=length(ResultData);
  if FDataOut>0 then
    try
      TriggerDataOut;
    finally
      FDataOut:=0;
      TriggerDataSent;
      end;
  end;

function TRtcISAPIServerProvider.Read: string;
  begin
  if FRequestBuffer.Size>0 then
    begin
    Result:=FRequestBuffer.Get;
    FRequestBuffer.Clear;
    end
  else
    Result:='';
  end;

procedure TRtcISAPIServerProvider.TriggerDataSent;
  begin
  if Response.Sending then
    Response.Started:=False;

  inherited TriggerDataSent;
  end;

procedure TRtcISAPIServerProvider.TriggerDataOut;
  begin
  if Response.Sending then
    begin
    if LenToSend>=0 then
      begin
      Dec(LenToSend, DataOut);
      Response.Done := LenToSend=0;
      end;

    if Response.Done then
      begin
      Request.Started:=False;
      Request.Active:=False;
      Response.Started:=False;
      Response.Sending:=False;
      FHeaderOut:=False;
      end;
    end;

  inherited TriggerDataOut;
  end;

function TRtcISAPIServerProvider.GetParent: TRtcConnectionProvider;
  begin
  Result:=nil;
  end;

procedure TRtcISAPIServerProvider.Listen;
  begin
  State:=conListening;
  TriggerListenStart;
  end;

procedure TRtcISAPIServerProvider.Disconnect;
  begin
  InternalDisconnect;
  end;

procedure TRtcISAPIServerProvider.InternalDisconnect;
  begin
  if State<>conInactive then
    begin
    if State=conActive then
      begin
      TriggerDisconnecting;
      TriggerDisconnect;
      TriggerConnectionLost;
      end
    else if State=conListening then
      TriggerListenStop;
    State:=conInactive;
    end;
  end;

procedure TRtcISAPIServerProvider.CopyFrom(Dup: TRtcConnectionProvider);
  begin
  //
  end;

procedure TRtcISAPIServerProvider.Enter;
  begin
  FCS.Enter;
  end;

procedure TRtcISAPIServerProvider.Leave;
  begin
  FCS.Leave;
  end;

procedure TRtcISAPIServerProvider.Connect(var _ECB: TEXTENSION_CONTROL_BLOCK);
  begin
  ECB:=_ECB;

  LocalAddr:=ServerVariable('SERVER_ADDR',128);
  LocalPort:=ServerVariable('SERVER_PORT',32);
  PeerAddr:=ServerVariable('REMOTE_ADDR',128);
  PeerPort:=ServerVariable('REMOTE_PORT',32);

  if not (State=conActive) then
    begin
    State:=conActive;
    TriggerConnectionAccepted;
    TriggerConnecting;
    TriggerConnect;
    end;
  end;

procedure TRtcISAPIServerProvider.ExecuteRequest;
  var
    s:string;
    len,len2:cardinal;
    MyPos:integer;
  begin
  Request.Clear;
  Response.Clear;

  s:=ServerVariable('URL',1024);
  if s<>'' then
    begin
    Request.FileName:='/';
    Request.URI:=s;
    end;

  Request.Method:=ECB.lpszMethod;
  Request.FileName:=ECB.lpszPathInfo;

  Request.Query.Text:=ECB.lpszQueryString;
  Request.ContentLength:=ECB.cbTotalBytes;

  s:=ServerVariable('ALL_RAW',8192);
  if s<>'' then Request.HeaderText:=s;

  if CompareText(Copy(Request.ContentType,1,19),'MULTIPART/FORM-DATA')=0 then
    begin
    MyPos:=Pos('BOUNDARY=',UpperCase(Request.ContentType));
    if MyPos>0 then
      // Get MULTIPART Boundary (Params.Delimiter)
      begin
      Request.Params.Delimiter:=
        Copy(Request.ContentType, MyPos+9, length(Request.ContentType)-MyPos-8);
      if (Copy(Request.Params.Delimiter,1,1)='"') and
         (Copy(Request.Params.Delimiter,
               length(Request.Params.Delimiter),1)='"') then
        begin
        Request.Params.Delimiter:=
           Copy(Request.Params.Delimiter, 2, length(Request.Params.Delimiter)-2);
        end;
      end;
    end;

  if ECB.cbAvailable>0 then
    begin
    Request.ContentIn:=ECB.cbAvailable;
    SetString(s,PChar(ECB.lpbData),ECB.cbAvailable);
    len:=length(s);
    FrequestBuffer.Add(s);
    end;

  FDataIn:=length(Request.URI)+length(Request.HeaderText);
  TriggerDataIn;

  Request.Started:=True;
  Request.Active:=True;
  Request.Complete:= Request.ContentLength=Request.ContentIn;

  if Request.FileName<>'' then
    TriggerDataReceived
  else if Request.Complete then
    begin
    Response.StatusCode:=301;
    Response.StatusText:='Moved Permanently';
    Response['LOCATION']:= Request.URI+'/';
    Response.ContentLength:=0;
    WriteHeader;
    end;

  Request.Started:=False;
  while Request.ContentIn<Request.ContentLength do
    begin
    len:=Request.ContentLength-Request.ContentIn;
    if len>32000 then len:=32000;

    len2:=len;
    SetLength(s,len);
    if not ECB.ReadClient(ECB.ConnID,@s[1],len) then
      raise Exception.Create('Error receiving data (ReadClient returned FALSE).')
    else if len<len2 then
      begin
      if len>0 then
        SetLength(s,len)
      else
        raise Exception.Create('Error receiving data (ReadClient returned 0 bytes).');
      end;

    FDataIn:=len;
    TriggerDataIn;

    Request.ContentIn:=Request.ContentIn+len;
    FrequestBuffer.Add(s);
    Request.Complete:= Request.ContentIn=Request.ContentLength;

    if Request.FileName<>'' then
      begin
      TriggerDataReceived;
      Sleep(0);
      end
    else if Request.Complete then
      begin
      Response.StatusCode:=301;
      Response.StatusText:='Moved Permanently';
      Response['LOCATION']:= Request.URI+'/';
      Response.ContentLength:=0;
      WriteHeader;
      end;
    end;

  FHeaderOut:=False;
  end;

function TRtcISAPIServerProvider.ServerVariable(s: string; len:cardinal): string;
  var
    len2:cardinal;
  begin
  len2:=len;
  SetLength(Result,len);
  if not ECB.GetServerVariable(ECB.ConnID, PChar(s),@Result[1],len) then
    begin
    if len>len2 then // more data available
      begin
      SetLength(Result,len);
      if not ECB.GetServerVariable(ECB.ConnID, PChar(s),@Result[1],len) then
        len:=1;
      end
    else
      len:=1;
    end;
  SetLength(Result,len-1);
  end;

end.
