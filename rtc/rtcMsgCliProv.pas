{
  "Message Client provider" - Copyright (c) Danijel Tkalcec
  @html(<br>)

  @exclude
}
unit rtcMsgCliProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  rtcTrashcan,

  SysUtils,
  Windows,
  Classes,

  rtcSyncObjs,
  rtcThrPool,

  rtcLog,
  rtcInfo,
  rtcConn,
  rtcConnProv,
  rtcThrConnProv,

  rtcFastStrings,
  rtcTransports;

const
  LOG_MSGCLI_EXCEPTIONS:boolean=False;

type
  TRtcMsgClientProvider = class;

  RtcMsgCliException = class(Exception);

  TRtcMsgClientThread = class(TRtcThread)
  public
    RtcConn:TRtcMsgClientProvider;
    Releasing:boolean;

  public
    constructor Create; override;
    destructor Destroy; override;

    function Work(Job:TObject):boolean; override;

    procedure OpenConn;
    procedure CloseConn(_lost:boolean);
    end;

  TRtcMsgClientProvider = class(TRtcThrClientProvider)
  private
    Client_Thread:TRtcMsgClientThread;

    RequestStream, ResponseStream:TMemoryStream;

    FServer:IRTCMessageReceiver;

    Forc:boolean;

    FCS:TRtcCritSec;

    FOnInvalidResponse:TRtcEvent;

    FResponseBuffer:TRtcHugeString;

    FReadBuffer:string;

    FMaxHeaderSize:integer;
    FMaxResponseSize:integer;

    FHeaderOut:boolean;
    LenToWrite:int64;

    FRequest:TRtcClientRequest;
    FResponse:TRtcClientResponse;

    FDataWasSent:boolean;

  protected
    procedure Enter; override;
    procedure Leave; override;

    function GetClientThread:TRtcThread; override;

    procedure TriggerInvalidResponse; virtual;

    procedure AcceptResponse; virtual;

    function _Active:boolean;

    procedure OpenConnection;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Connect(Force:boolean=False); override;
    procedure Disconnect; override;
    procedure Release; override;

    procedure InternalDisconnect; override;

    procedure LeavingEvent; virtual;

    procedure SetTriggerInvalidResponse(Event:TRtcEvent);

    procedure WriteHeader; overload; virtual;
    procedure WriteHeader(const Header_Text:string); overload; virtual;

    procedure Write(const s:string; SendNow:boolean=True); override;
    function Read:string; override;

    property Request:TRtcClientRequest read FRequest write FRequest;
    property Response:TRtcClientResponse read FResponse write FResponse;

    // Max. allowed size of the first (status) line in response header
    property MaxResponseSize:integer read FMaxResponseSize write FMaxResponseSize;
    // Max. allowed size of the complete response Header
    property MaxHeaderSize:integer read FMaxHeaderSize write FMaxHeaderSize;

    property Server:IRTCMessageReceiver read FServer write FServer;
    end;

implementation

const
  CRLF = #13#10;
  END_MARK = CRLF+CRLF;

type
  TRtcBaseMessage=class
    end;

var
  Message_WSStop,
  Message_WSRelease,
  Message_WSOpenConn,
  Message_WSCloseConn:TRtcBaseMessage;

{ TRtcMsgClientProvider }

constructor TRtcMsgClientProvider.Create;
  begin
  inherited;

  RequestStream:=TMemoryStream.Create;
  ResponseStream:=TMemoryStream.Create;

  FCS:=TRtcCritSec.Create;

  FResponseBuffer:=TRtcHugeString.Create;

  FDataWasSent:=False;
  SetLength(FReadBuffer,32000);
  end;

destructor TRtcMsgClientProvider.Destroy;
  begin
  Silent:=True;
  // Closing:=True;

  if assigned(Client_Thread) then
    TRtcThread.PostJob(Client_Thread, Message_WSStop, True)
  else
    InternalDisconnect;

  FResponseBuffer.Free;
  FResponseBuffer:=nil;

  RequestStream.Free;
  RequestStream:=nil;
  ResponseStream.Free;
  ResponseStream:=nil;

  FReadBuffer:='';
  FCS.Free;

  inherited;
  end;

procedure TRtcMsgClientProvider.Enter;
  begin
  FCS.Enter;
  end;

procedure TRtcMsgClientProvider.Leave;
  begin
  FCS.Leave;
  end;

procedure TRtcMsgClientProvider.SetTriggerInvalidResponse(Event: TRtcEvent);
  begin
  FOnInvalidResponse:=Event;
  end;

procedure TRtcMsgClientProvider.TriggerInvalidResponse;
  begin
  if assigned(FOnInvalidResponse) then
    FOnInvalidResponse;
  end;

function TRtcMsgClientProvider.GetClientThread: TRtcThread;
  begin
  Result:=Client_Thread;
  end;

procedure TRtcMsgClientProvider.Connect(Force: boolean);
  begin
  if assigned(Client_Thread) and not inThread then
    TRtcThread.PostJob(Client_Thread, Message_WSOpenConn)
  else
    begin
    if GetMultiThreaded then
      begin
      if not assigned(Client_Thread) then
        begin
        Client_Thread:=TRtcMsgClientThread.Create;
        Client_Thread.RtcConn:=self;
        end;
      Forc:=Force;
      TRtcThread.PostJob(Client_Thread, Message_WSOpenConn);
      end
    else
      OpenConnection;
    end;
  end;

procedure TRtcMsgClientProvider.OpenConnection;
  begin
  if (State=conActive) or (State=conActivating) then Exit; // already connected !!!

  if State<>conInactive then
    raise Exception.Create('Can not connect again, connection in use.');

  try
    Lost:=True;
    Closing:=False;
    Silent:=False;

    Request.Init;
    Response.Clear;

    State:=conActivating;

    TriggerConnectionOpening(Forc);

    if not assigned(FServer) then
      raise RtcMsgCliException.Create('Error connecting, Server component not assigned!');

    RequestStream.Clear;
    ResponseStream.Clear;

    State:=conActive;

    TriggerConnecting;
    TriggerConnect;
  except
    on E:Exception do
      begin
      TriggerConnectionClosing;
      TriggerConnectError(E);
      TriggerReadyToRelease;
      end;
    end;
  end;

procedure TRtcMsgClientProvider.Disconnect;
  begin
  Lost:=False;
  if assigned(Client_Thread) and not inThread then
    begin
    if TRtcThread.Lock(Client_Thread) then
      try
        TRtcThread.PostJob(Client_Thread, Message_WSCloseConn);
      finally
        TRtcThread.UnLock;
        end;
    end
  else
    InternalDisconnect;
  end;

procedure TRtcMsgClientProvider.InternalDisconnect;
  begin
  if Closing then Exit;

  Closing:=True;

  State:=conClosing;

  RequestStream.Clear;
  ResponseStream.Clear;

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
    TriggerReadyToRelease;
    end;
  end;

function TRtcMsgClientProvider.Read: string;
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

procedure TRtcMsgClientProvider.WriteHeader;
  var
    s:string;
  begin
  if not _Active then Exit;

  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  s:=Request.Method+' '+Request.URI+' HTTP/1.1'+CRLF+
     Request.HeaderText;

  if Request.Close then s:=s+'Connection: close'+CRLF;

  s:=s+CRLF;

  RequestStream.Write(s[1],length(s));

  FDataOut:=length(s);
  TriggerDataOut;

  Request.Started:=True;
  Request.Active:=True;

  LenToWrite:=Request.ContentLength;

  ResponseStream.Clear;

  FDataWasSent:=True;
  end;

procedure TRtcMsgClientProvider.WriteHeader(const Header_Text: string);
  begin
  if not _Active then Exit;

  Response.HeaderText:=Header_Text;
  WriteHeader;
  end;

procedure TRtcMsgClientProvider.Write(const s: string; SendNow:boolean=True);
  begin
  if not _Active then Exit;

  if s='' then Exit;

  if not Request.Active then
    raise Exception.Create('Sending data without header.');

  RequestStream.Write(s[1], length(s));

  FDataOut:=length(s);
  LenToWrite:=LenToWrite-FDataOut;
  TriggerDataOut;

  FDataWasSent:=True; // will call DataSent
  end;

procedure TRtcMsgClientProvider.LeavingEvent;
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

procedure TRtcMsgClientProvider.AcceptResponse;
  var
    s,
    StatusLine,
    HeadStr:string;

    len,len2,
    HeadLen,
    MyPos:integer;

    FChunked,
    FHaveResponse,
    FResponseLine:boolean;

    FChunkState:integer;

    LenToRead:int64;
    InBuffer:string;

  function HexToInt(s:string):integer;
    var
      i,len:integer;
      c:char;
    begin
    Result:=0;
    len:=length(s);
    i:=1;
    while len>0 do
      begin
      c:=s[len];
      if c in ['1'..'9'] then
        Result:=Result+i*(Ord(c)-Ord('0'))
      else if s[len] in ['A'..'F'] then
        Result:=Result+i*(Ord(c)-Ord('A')+10)
      else if s[len] in ['a'..'f'] then
        Result:=Result+i*(Ord(c)-Ord('a')+10);
      i:=i*16;Dec(len);
      end;
    end;

  procedure ResponseError;
    begin
    FResponseLine:=False;
    TriggerInvalidResponse;
    end;

  procedure ClearResponse;
    begin
    FResponseBuffer.Clear;

    FResponseLine:=False;
    FResponse.Clear;
    LenToRead:=-1;
    end;

  procedure ProcessData(const data:string);
    var
      s:string;
      FDone:boolean;
    begin
    FDone:=False;
    InBuffer := InBuffer + data;
    repeat
      if not FHaveResponse then // Don't have the header yet ...
        begin
        if not FResponseLine then
          begin
          // Accept streaming data as response
          if ((length(InBuffer)>=5) and (CompareText(Copy(InBuffer,1,5),'HTTP/')<>0)) or
             ((length(InBuffer)=1) and (CompareText(InBuffer,'H')<>0)) or
             ((length(InBuffer)=2) and (CompareText(InBuffer,'HT')<>0)) or
             ((length(InBuffer)=3) and (CompareText(InBuffer,'HTT')<>0)) or
             ((length(InBuffer)=4) and (CompareText(InBuffer,'HTTP')<>0)) then
            begin
            ClearResponse;

            Response.Receiving:=True;
            Response.Started:=True;

            FHaveResponse:=True;
            FResponseLine:=True;
            LenToRead:=-1; // Unlimited length (streaming data until disconnected)

            Continue;
            end;

          MyPos:=Pos(CRLF,InBuffer);
          if (MaxResponseSize>0) and
             ( (MyPos>MaxResponseSize+1) or
               ((MyPos<=0) and (length(InBuffer)>MaxResponseSize+length(CRLF))) ) then
            begin
            ClearResponse;

            ResponseError;
            Exit;
            end
          else if (MyPos>0) then
            begin
            ClearResponse;

            StatusLine:=Copy(InBuffer,1,MyPos-1);
            Delete(InBuffer,1,MyPos+length(CRLF)-1);

            if CompareText(Copy(StatusLine,1,5),'HTTP/')<>0 then
              begin
              ResponseError;
              Exit;
              end;

            Response.Receiving:=True;
            Response.Started:=True;

            { Our line probably looks like this:
              HTTP/1.1 200 OK }
            MyPos:=Pos(' ',StatusLine); // first space before StatusCode
            if MyPos<=0 then
              begin
              ResponseError;
              Exit;
              end;
            Delete(StatusLine,1,MyPos); // remove 'HTTP/1.1 '

            MyPos:=Pos(' ',StatusLine); // space after StatusCode
            if MyPos<=0 then
              begin
              ResponseError;
              Exit;
              end;

            s:=Copy(StatusLine,1,MyPos-1); // StatusCode
            Delete(StatusLine,1,MyPos); // StatusText

            if (s<>'') and (StatusLine<>'') then
              begin
              try
                Response.StatusCode:=StrToInt(s);
                Response.StatusText:=StatusLine;
              except
                // if there is something wrong with this, just ignore the exception
                end;
              end;

            FResponseLine:=True;
            end;
          end;

        if FResponseLine then
          begin
          // See if we can get the whole header ...
          HeadLen:=Pos(CRLF, InBuffer);
          if HeadLen<>1 then
            HeadLen:=Pos(END_MARK, InBuffer);

          if HeadLen=1 then
            begin
            // Delete CRLF from the body
            Delete(InBuffer,1,2);

            if Response.StatusCode=100 then
              begin // special handling of the "100:Continuing" Http status code
              FResponseLine:=False;
              Continue;
              end;

            // No Header: disconnect closes the response.
            Request.Close:=True;

            if Request.Method='HEAD' then
              begin
              FChunked:=False;
              LenToRead:=0;
              end;

            FHaveResponse:=True;
            end
          else if (MaxHeaderSize>0) and
             ( (HeadLen>MaxHeaderSize) or
               ((HeadLen<=0) and (length(InBuffer)>MaxHeaderSize+length(END_MARK))) ) then
            begin
            ResponseError;
            Exit;
            end
          else if HeadLen>0 then
            begin
            // Separate header from the body
            HeadStr:=Copy(InBuffer, 1, HeadLen+length(END_MARK)-1);
            Delete(InBuffer,1,HeadLen+length(END_MARK)-1);

            FHaveResponse:=True;

            // Scan for all header attributes ...
            MyPos:=Pos(CRLF, HeadStr);
            while (MyPos>1) do // at least 1 character inside line
              begin
              StatusLine:=Copy(HeadStr,1,MyPos-1);
              Delete(HeadStr,1,MyPos+Length(CRLF)-1);

              MyPos:=Pos(':',StatusLine);
              if MyPos>0 then
                begin
                s:=Trim(Copy(StatusLine,1,MyPos-1));
                Delete(StatusLine,1,MyPos);
                StatusLine:=Trim(StatusLine);

                if CompareText(s,'TRANSFER-ENCODING')=0 then
                  begin
                  if UpperCase(StatusLine)='CHUNKED' then
                    begin
                    FChunked:=True;
                    FChunkState:=0;
                    end;
                  end
                else if CompareText(s,'CONTENT-LENGTH')=0 then
                  begin
                  LenToRead:=StrToInt64Def(StatusLine,0);

                  Response.ContentLength:=LenToRead;
                  end
                else if CompareText(s,'CONNECTION')=0 then
                  begin
                  if Trim(Uppercase(StatusLine))='CLOSE' then
                    Request.Close:=True
                  else if Trim(Uppercase(StatusLine))='KEEP-ALIVE' then
                    Request.Close:=False;
                  end;

                Response[s]:=StatusLine;
                end;

              MyPos:=Pos(CRLF, HeadStr);
              end;

            if LenToRead=-1 then
              Request.Close:=True;

            if Request.Method='HEAD' then
              begin
              FChunked:=False;
              LenToRead:=0;
              end;

            StatusLine:='';
            HeadStr:='';
            end;
          end;
        end;

      if FHaveResponse then // Processing a response ...
        begin
        if FChunked then // Read data as chunks
          begin
          if (FChunkState=0) and (InBuffer<>'') then // 1.step = read chunk size
            begin
            MyPos:=Pos(CRLF,InBuffer);
            if MyPos>0 then
              begin
              StatusLine:=Trim(Copy(InBuffer,1,MyPos-1));
              Delete(InBuffer,1,MyPos+1);

              LenToRead:=HexToInt(StatusLine);

              FChunkState:=1; // ready to read data
              end;
            end;

          if (FChunkState=1) and (InBuffer<>'') then // 2.step = read chunk data
            begin
            if (LenToRead>length(InBuffer)) then // need more than we have
              begin
              Response.ContentIn:=Response.ContentIn+length(InBuffer);

              if LenToRead>0 then
                Dec(LenToRead, length(InBuffer));

              FResponseBuffer.Add(InBuffer);
              InBuffer:='';

              inherited TriggerDataReceived;

              Response.Started:=False;
              end
            else
              begin
              if LenToRead>0 then
                begin
                Response.ContentIn:=Response.ContentIn+LenToRead;

                FResponseBuffer.Add(Copy(InBuffer,1,LenToRead));

                Delete(InBuffer,1,LenToRead);
                LenToRead:=0;
                FChunkState:=2; // this is not the last chunk, ready to read CRLF
                end
              else
                FChunkState:=3; // this was last chunk, ready to read CRLF
              end;
            end;

          if (FChunkState>=2) and (length(InBuffer)>=2) then // 3.step = close chunk
            begin
            LenToRead:=-1;
            Delete(InBuffer,1,2); // Delete CRLF

            if FChunkState=2 then
              begin
              FChunkState:=0;
              end
            else
              begin
              Response.Done:=True;
              Request.Active:=False;
              FHaveResponse:=False; // get ready for next request
              FChunked:=False;
              FChunkState:=0;
              FResponseLine:=False;
              FHeaderOut:=False;

              FDone:=True;
              end;

            inherited TriggerDataReceived;

            Response.Started:=False;
            end;
          end
        else // Read data as stream or with predefined length
          begin
          if (LenToRead>0) or (LenToRead=-1) then
            begin
            if (LenToRead>length(InBuffer)) or
               (LenToRead=-1) then // need more than we have
              begin
              Response.ContentIn:=Response.ContentIn+length(InBuffer);

              if LenToRead>0 then
                Dec(LenToRead, length(InBuffer));

              FResponseBuffer.Add(InBuffer);

              InBuffer:='';
              end
            else
              begin
              Response.ContentIn:=Response.ContentIn+LenToRead;

              FResponseBuffer.Add(Copy(InBuffer,1,LenToRead));

              Delete(InBuffer,1,LenToRead);

              LenToRead:=0;
              Response.Done:=True;
              Request.Active:=False;
              FHaveResponse:=False; // get ready for next request
              FChunked:=False;
              FResponseLine:=False;
              FHeaderOut:=False;

              FDone:=True;
              end;
            end
          else
            begin
            Response.Done:=True;
            Request.Active:=False;
            FHaveResponse:=False; // get ready for next request
            FChunked:=False;
            FResponseLine:=False;
            FHeaderOut:=False;

            FDone:=True;
            end;

          inherited TriggerDataReceived;

          Response.Started:=False;
          end;
        end
      else
        Break; // Failing to fetch a header will break the loop.

      until (InBuffer='') or FDone;
    end;

  begin
  if not _Active then Exit;

  if not assigned(FServer) then
    raise RtcMsgCliException.Create('Error! Server component removed!');

  FServer.ProcessMessage(RequestStream, ResponseStream);

  FResponseBuffer.Clear;

  FChunked:=False;
  FChunkState:=0;

  FHaveResponse:=False;
  FResponseLine:=False;
  LenToRead:=0;

  RequestStream.Clear;
  ResponseStream.Position:=0;

  try
    while (ResponseStream.Position<ResponseStream.Size) and not Response.Done do
      begin
      len:=ResponseStream.Size-ResponseStream.Position;
      if len>32000 then len:=32000;

      SetLength(s,len);
      len2:=ResponseStream.Read(s[1],len);

      FDataIn:=len;
      TriggerDataIn;

      ProcessData(s);
      if len2<len then Break;
      end;
  finally
    ResponseStream.Clear;
    if _Active and not Request.Active then
      FResponseBuffer.Clear;
    end;
  end;

function TRtcMsgClientProvider._Active: boolean;
  begin
  Result:=not Closing and (FState in [conActive,conActivating]);
  end;

procedure TRtcMsgClientProvider.Release;
  begin
  if assigned(Client_Thread) then
    TRtcThread.PostJob(Client_Thread, Message_WSRelease, True)
  else
    inherited;
  end;

{ TRtcMsgClientThread }

constructor TRtcMsgClientThread.Create;
  begin
  inherited;
  RtcConn:=nil;
  end;

procedure TRtcMsgClientThread.OpenConn;
  begin
  RtcConn.OpenConnection;
  end;

procedure TRtcMsgClientThread.CloseConn(_lost:boolean);
  begin
  if assigned(RtcConn) then
    begin
    try
      if RtcConn.State<>conInactive then
        begin
        RtcConn.Lost:=_lost;
        RtcConn.InternalDisconnect;
        end;
    except
      on E:Exception do
        if LOG_MSGCLI_EXCEPTIONS then
          Log('MsgClientThread.CloseConn : RtConn.InternalDisconnect',E);
        // ignore exceptions
      end;
    end;
  end;

destructor TRtcMsgClientThread.Destroy;
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

function TRtcMsgClientThread.Work(Job: TObject):boolean;
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
      if LOG_MSGCLI_EXCEPTIONS then
        Log('MsgClientThread.Work',E);
      CloseConn(true);
      // raise;
      end;
    end;
  end;

type
  TMyMsgCli=class
    public
    constructor Create;
    destructor Destroy; override;
    end;

var
  MyMsgCli:TMyMsgCli;

{ TMyWinInet }

constructor TMyMsgCli.Create;
  begin
  inherited;
  Message_WSOpenConn:=TRtcBaseMessage.Create;
  Message_WSCloseConn:=TRtcBaseMessage.Create;
  Message_WSStop:=TRtcBaseMessage.Create;
  Message_WSRelease:=TRtcBaseMessage.Create;
  end;

destructor TMyMsgCli.Destroy;
  begin
  Message_WSOpenConn.Free;
  Message_WSCloseConn.Free;
  Message_WSStop.Free;
  Message_WSRelease.Free;
  inherited;
  end;

initialization
MyMsgCli:=TMyMsgCli.Create;
finalization
Garbage(MyMsgCli);
end.
