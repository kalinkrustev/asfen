{
  "HTTP Client Provider (WinSock)" - Copyright (c) Danijel Tkalcec
  @html(<br>)

  Using TRtcWSockClientProvider to implement a HTTP Client provider

  @exclude
}
unit rtcWSockHttpCliProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  Classes,
  SysUtils,

  rtcLog,
  rtcConn,
  rtcConnProv,

  rtcFastStrings,
  rtcWSockCliProv;

type
  TRtcWSockHttpClientProvider = class(TRtcWSockClientProvider)
  private
    FOnInvalidResponse:TRtcEvent;

    FMaxHeaderSize:integer;
    FMaxResponseSize:integer;

    FRequest:TRtcClientRequest;
    FResponse:TRtcClientResponse;

    FResponseBuffer:TRtcHugeString;

    FResponseWaiting:boolean;

    ReqComplete:boolean; // internal Request.Complete indicator (to avoid problems with changing Request objects)

    FChunked:boolean;
    FChunkState:byte;

    FResponseLine:boolean; // response line received
    InBuffer:string; // data received, including HTTP header (header will be stripped when read)
    FHaveResponse:boolean; // response header accepted, receiving request data.
    LenToRead:int64; // number of bytes left to read from last Request

    LenToWrite:int64; // number of bytes to write out using inherited Write()
    LenToSend:int64; // number of bytes left to send out (DataOut event)
    FHeaderOut:boolean;

  protected
    procedure ClearResponse;

    procedure TriggerConnect; override;
    procedure TriggerConnectLost; override;
    procedure TriggerDataReceived; override;
    procedure TriggerDataSent; override;
    procedure TriggerDataOut; override;

    procedure TriggerInvalidResponse; virtual;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SetTriggerInvalidResponse(Event:TRtcEvent);

    procedure WriteHeader(SendNow:boolean=True); overload;
    procedure WriteHeader(const Header_Text:string; SendNow:boolean=True); overload;

    procedure Write(const ResultData:string; SendNow:boolean=True); override;

    // On DataReceived, read server response body using this:
    function Read:string; override;

    property Request:TRtcClientRequest read FRequest write FRequest;
    property Response:TRtcClientResponse read FResponse write FResponse;

    // Max. allowed size of the first (status) line in response header
    property MaxResponseSize:integer read FMaxResponseSize write FMaxResponseSize;
    // Max. allowed size of the complete response Header
    property MaxHeaderSize:integer read FMaxHeaderSize write FMaxHeaderSize;
    end;

implementation

const
  CRLF = #13#10;
  END_MARK = CRLF+CRLF;

{ TRtcWSockHttpClientProvider }

procedure TRtcWSockHttpClientProvider.SetTriggerInvalidResponse(Event: TRtcEvent);
  begin
  FOnInvalidResponse:=Event;
  end;

procedure TRtcWSockHttpClientProvider.TriggerInvalidResponse;
  begin
  if assigned(FOnInvalidResponse) then
    FOnInvalidResponse;
  end;

constructor TRtcWSockHttpClientProvider.Create;
  begin
  inherited;
  FResponseBuffer:=TRtcHugeString.Create;

  InBuffer:='';
  LenToWrite:=0;
  LenToSend:=0;
  FHeaderOut:=False;
  FResponseLine:=False;
  ReqComplete:=False;
  end;

destructor TRtcWSockHttpClientProvider.Destroy;
  begin
  FResponseBuffer.Free;

  InBuffer:='';
  LenToWrite:=0;
  LenToSend:=0;
  FResponseLine:=False;
  FHeaderOut:=False;
  FResponseWaiting:=False;

  inherited;
  end;

procedure TRtcWSockHttpClientProvider.ClearResponse;
  begin
  FResponseBuffer.Clear;

  FResponseLine:=False;
  FResponse.Clear;
  LenToRead:=-1;
  end;

procedure TRtcWSockHttpClientProvider.TriggerConnect;
  begin
  Request.Init;

  FResponseBuffer.Clear;

  InBuffer:='';
  LenToWrite:=0;
  LenToSend:=0;
  FHeaderOut:=False;
  FResponseLine:=False;
  FResponseWaiting:=False;
  FHaveResponse:=False;
  FChunked:=False;
  FChunkState:=0;

  ClearResponse;

  inherited;
  end;

procedure TRtcWSockHttpClientProvider.TriggerConnectLost;
  begin
  if FHaveResponse then // Processing a response ...
    begin
    if not FChunked and (LenToRead=-1) then // No content-length and not chunked
      begin
      LenToRead:=0;
      Response.Done:=True;
      Request.Active:=False;
      FHaveResponse:=False; // get ready for next request
      FResponseLine:=False;
      FHeaderOut:=False;
      FChunked:=False;
      FChunkState:=0;

      ReqComplete:=False; // DataReceived events have to wait until a new request has been sent out

      inherited TriggerDataReceived;
      end;
    end;
  inherited;
  end;

procedure TRtcWSockHttpClientProvider.TriggerDataReceived;
  var
    s,
    StatusLine,
    HeadStr:string;
    HeadLen,
    MyPos:integer;

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
    ReqComplete:=False; // no more reading, please!
    FResponseLine:=False;

    TriggerInvalidResponse;
    end;

  begin
  if not ReqComplete then
    begin
    if assigned(CryptPlugin) then
      begin
      // Read string from buffer
      InBuffer:=InBuffer + inherited Read;

      if InBuffer='' then
        begin
        FResponseWaiting:=True;
        Exit;
        end
      else
        FResponseWaiting:=False;
      end
    else
      begin
      FResponseWaiting:=True;
      Exit;
      end;
    end
  else
    FResponseWaiting:=False;

  // Read string from buffer
  InBuffer:=InBuffer + inherited Read;

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
          if MyPos<=0 then // no Status Text
            begin
            s:=StatusLine;
            StatusLine:='';
            end
          else
            begin
            s:=Copy(StatusLine,1,MyPos-1); // StatusCode
            Delete(StatusLine,1,MyPos); // StatusText
            end;

          if s<>'' then
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

          if Response.StatusCode=100 then
            begin // special handling of the "100:Continuing" Http status code
            FResponseLine:=False;
            Continue;
            end;

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
                if CompareText(StatusLine,'CHUNKED')=0 then
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

            ReqComplete:=False; // DataReceived events have to wait until a new request has been sent out
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

            ReqComplete:=False; // DataReceived events have to wait until a new request has been sent out
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

          ReqComplete:=False; // DataReceived events have to wait until a new request has been sent out
          end;

        inherited TriggerDataReceived;

        Response.Started:=False;
        end;
      end
    else
      Break; // Failing to fetch a header will break the loop.

    until (InBuffer='') or not ReqComplete;
  end;

procedure TRtcWSockHttpClientProvider.WriteHeader(SendNow:boolean=True);
  var
    s:string;
  begin
  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  s:=Request.Method+' '+Request.URI+' HTTP/1.1'+CRLF+
     Request.HeaderText;

  if Request.Close then s:=s+'Connection: close'+CRLF;

  s:=s+CRLF;

  Request.Started:=True;
  Request.Active:=True;

  LenToWrite:=Request.ContentLength;
  LenToSend:=length(s) + Request.ContentLength;

  FHeaderOut:=True;
  inherited Write(s, SendNow or (LenToWrite<=0));
  end;

procedure TRtcWSockHttpClientProvider.WriteHeader(const Header_Text:string; SendNow:boolean=True);
  var
    s:string;
  begin
  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  if Header_Text<>'' then
    Request.HeaderText:=Header_Text;

  s:=Request.Method+' '+Request.URI+' HTTP/1.1'+CRLF +
     Request.HeaderText;

  if Request.Close then s:=s+'Connection: close'+CRLF;

  s:=s+CRLF;

  Request.Started:=True;
  Request.Active:=True;

  LenToWrite:=Request.ContentLength;
  LenToSend:=length(s) + Request.ContentLength;

  FHeaderOut:=True;
  inherited Write(s, SendNow or (LenToWrite<=0));
  end;

procedure TRtcWSockHttpClientProvider.Write(const ResultData: string; SendNow:boolean=True);
  begin
  if length(ResultData)=0 then Exit;

  if not FHeaderOut then
    raise Exception.Create('Trying to send Data without Header. Call WriteHeader before Write.');

  if length(ResultData)>LenToWrite then
    raise Exception.Create('Trying to send more Data out than specified in Header.');

  Dec(LenToWrite, length(ResultData));

  Request.ContentOut:=Request.ContentOut + length(ResultData);

  inherited Write(ResultData, SendNow);
  end;

function TRtcWSockHttpClientProvider.Read: string;
  begin
  if FResponseBuffer.Size>0 then
    begin
    Result:=FResponseBuffer.Get;
    FResponseBuffer.Clear;
    end
  else
    Result:='';
  end;

procedure TRtcWSockHttpClientProvider.TriggerDataSent;
  begin
  if Request.Active then
    Request.Started:=False;

  inherited TriggerDataSent;

  if FResponseWaiting then
    if ReqComplete then
      TriggerDataReceived;
  end;

procedure TRtcWSockHttpClientProvider.TriggerDataOut;
  begin
  if not ReqComplete then
    if assigned(Request) and Request.Active then
      begin
      Dec(LenToSend, DataOut);
      ReqComplete := LenToSend<=0;
      Request.Complete := ReqComplete;
      end;

  inherited TriggerDataOut;
  end;

end.
