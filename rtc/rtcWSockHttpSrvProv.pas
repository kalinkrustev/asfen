{
  "HTTP Server Provider (WinSock)" - Copyright (c) Danijel Tkalcec
  @html(<br>)

  Using TRtcWSockServerProvider to implement a HTTP Server provider.

  @exclude
}
unit rtcWSockHttpSrvProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  Classes,
  SysUtils,

  rtcLog,
  rtcConn,
  rtcConnProv,

  rtcFastStrings,
  rtcWSockSrvProv;

type
  TRtcWSockHttpServerProvider = class(TRtcWSockServerProvider)
  private
    FOnInvalidRequest:TRtcEvent;

    FMaxHeaderSize:integer;
    FMaxRequestSize:integer;

    FRequest:TRtcServerRequest;
    FResponse:TRtcServerResponse;

    FRequestBuffer:TRtcHugeString;

    FRequestWaiting:boolean; // will be set when request is waiting to be read.

    FChunked:boolean;
    FChunkState:byte;

    FRequestLine:boolean; // request line received
    InBuffer:string; // data received, including HTTP header (header will be stripped when read)
    FHaveRequest:boolean; // request header accepted, receiving request data.
    LenToRead:int64; // number of bytes left to read from last Request

    LenToWrite:int64; // number of bytes to write out using inherited Write()
    LenToSend:int64; // number of bytes left to send out (DataOut event)
    FHeaderOut:boolean;

    procedure ClearRequest;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure TriggerDisconnect; override;
    procedure TriggerDataReceived; override;
    procedure TriggerDataSent; override;
    procedure TriggerDataOut; override;

    procedure TriggerInvalidRequest; virtual;
    procedure SetTriggerInvalidRequest(Event:TRtcEvent);

    procedure WriteHeader(SendNow:boolean=True); overload;
    procedure WriteHeader(const Header_Text:string; SendNow:boolean=True); overload;

    procedure Write(const ResultData:string; SendNow:boolean=True); override;

    // 1. On DataReceived, read client request info using this:
    function Read:string; override;

    property Request:TRtcServerRequest read FRequest write FRequest;
    property Response:TRtcServerResponse read FResponse write FResponse;

    property MaxRequestSize:integer read FMaxRequestSize write FMaxRequestSize;
    property MaxHeaderSize:integer read FMaxHeaderSize write FMaxHeaderSize;
    end;

implementation

const
  CRLF = #13#10;
  END_MARK = CRLF+CRLF;

{ TRtcWSockHttpServerProvider }

procedure TRtcWSockHttpServerProvider.SetTriggerInvalidRequest(Event: TRtcEvent);
  begin
  FOnInvalidRequest:=Event;
  end;

procedure TRtcWSockHttpServerProvider.TriggerInvalidRequest;
  begin
  if assigned(FOnInvalidRequest) then
    FOnInvalidRequest;
  end;

constructor TRtcWSockHttpServerProvider.Create;
  begin
  inherited;
  FRequestBuffer:=TRtcHugeString.Create;

  InBuffer:='';
  LenToWrite:=0;
  LenToSend:=0;
  FHeaderOut:=False;
  FRequestLine:=False;
  FRequest:=nil;
  FResponse:=nil;
  FChunked:=False;
  FChunkState:=0;
  end;

destructor TRtcWSockHttpServerProvider.Destroy;
  begin
  FRequestBuffer.Free;

  InBuffer:='';
  LenToWrite:=0;
  LenToSend:=0;
  FRequestLine:=False;
  FHeaderOut:=False;

  inherited;
  end;

procedure TRtcWSockHttpServerProvider.ClearRequest;
  begin
  FRequestBuffer.Clear;

  FRequestLine:=False;
  FRequest.Clear;
  FResponse.Clear;
  LenToRead:=0;
  end;

procedure TRtcWSockHttpServerProvider.TriggerDisconnect;
  begin
  inherited;
  FRequestBuffer.Clear;

  InBuffer:='';
  LenToWrite:=0;
  LenToSend:=0;
  FHeaderOut:=False;
  FRequestLine:=False;

  ClearRequest;
  end;

procedure TRtcWSockHttpServerProvider.TriggerDataReceived;
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

  procedure RequestError;
    begin
    FRequestLine:=False;

    TriggerInvalidRequest;
    end;

  begin
  if Request.Complete and not Response.Done then
    begin
    if assigned(CryptPlugin) then
      begin
      // Read string from buffer
      InBuffer:=InBuffer + inherited Read;

      if InBuffer='' then
        begin
        FRequestWaiting:=True;
        Exit;
        end
      else
        FRequestWaiting:=False;
      end
    else
      begin
      FRequestWaiting:=True;
      Exit;
      end;
    end
  else
    FRequestWaiting:=False;

  // Read string from buffer
  InBuffer:=InBuffer + inherited Read;

  while InBuffer<>'' do
    begin

    if not FHaveRequest then // Don't have the header yet ...
      begin
      if not FRequestLine then
        begin
        MyPos:=Pos(CRLF,InBuffer);
        if (MaxRequestSize>0) and
           ( (MyPos>MaxRequestSize+1) or
             ((MyPos<=0) and (length(InBuffer)>MaxRequestSize+length(CRLF))) ) then
          begin
          ClearRequest;
          Request.FileName:=InBuffer;
          RequestError;
          Exit;
          end
        else if (MyPos>0) then
          begin
          ClearRequest;
          StatusLine:=Copy(InBuffer,1,MyPos-1);
          Delete(InBuffer,1,MyPos+length(CRLF)-1);

          MyPos:=Pos(' HTTP/', UpperCase(StatusLine));
          if MyPos<=0 then
            MyPos:=Pos(' HTTPS/', UpperCase(StatusLine));

          if MyPos<=0 then
            begin
            Request.FileName:=StatusLine;
            RequestError;
            Exit;
            end
          else
            begin
            Request.Started:=True;
            Request.Active:=True;

            // Request Method
            MyPos:=Pos(' ',StatusLine);
            if MyPos<=0 then
              begin
              Request.FileName:=StatusLine;
              RequestError;
              Exit;
              end;

            Request.Method:=Trim(Copy(StatusLine,1,MyPos-1));
            Delete(StatusLine,1,MyPos);

            // Request FileName
            MyPos:=Pos(' ',StatusLine);
            if MyPos<=0 then
              begin
              Request.FileName:=StatusLine;
              RequestError;
              Exit;
              end;

            Request.FileName:=Copy(StatusLine,1,MyPos-1);
            Delete(StatusLine,1,MyPos);

            // Request HTTP type
            MyPos:=Pos('/',StatusLine);
            if MyPos<=0 then
              begin
              RequestError;
              Exit;
              end;

            if Copy(StatusLine,MyPos+1,3)='1.0' then
              Request.Close:=True;

            MyPos:=Pos('?',Request.FileName);
            if MyPos>0 then
              begin
              Request.Query.Text:=Copy(Request.FileName,MyPos+1,length(Request.FileName)-MyPos);
              Request.FileName:=Copy(Request.FileName,1,MyPos-1);
              end
            else
              Request.Query.Clear;

            FRequestLine:=True;
            end;
          end;
        end;

      if FRequestLine then
        begin
        // See if we can get the whole header ...
        HeadLen:=Pos(CRLF, InBuffer);
        if HeadLen<>1 then
          HeadLen:=Pos(END_MARK, InBuffer);

        if HeadLen=1 then
          begin
          Delete(InBuffer,1,2);
          FHaveRequest:=True;
          end
        else if (MaxHeaderSize>0) and
           ( (HeadLen>MaxHeaderSize) or
             ((HeadLen<=0) and (length(InBuffer)>MaxHeaderSize+length(END_MARK))) ) then
          begin
          RequestError;
          Exit;
          end
        else if HeadLen>0 then
          begin
          // Separate header from the body
          HeadStr:=Copy(InBuffer, 1, HeadLen+length(END_MARK)-1);
          Delete(InBuffer,1,HeadLen+length(END_MARK)-1);

          FHaveRequest:=True;

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

              if CompareText(s,'CONTENT-LENGTH')=0 then
                begin
                LenToRead:=StrToInt64Def(StatusLine,0);
                Request.ContentLength:=LenToRead;
                end
              else
                Request[s]:=StatusLine;
              end;

            MyPos:=Pos(CRLF, HeadStr);
            end;

          if CompareText(Request['CONNECTION'],'CLOSE')=0 then
            Request.Close:=True;

          if CompareText(Request['TRANSFER-ENCODING'],'CHUNKED')=0 then
            begin
            FChunked:=True;
            FChunkState:=0;
            end
          else
            FChunked:=False;

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

          StatusLine:='';
          HeadStr:='';
          end;
        end;
      end;

    if FHaveRequest then // Processing a request ...
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
            Request.ContentIn:=Request.ContentIn+length(InBuffer);

            if LenToRead>0 then
              Dec(LenToRead, length(InBuffer));

            FRequestBuffer.Add(InBuffer);
            InBuffer:='';

            inherited TriggerDataReceived;

            Request.Started:=False;
            end
          else
            begin
            if LenToRead>0 then
              begin
              Request.ContentIn:=Request.ContentIn+LenToRead;

              FRequestBuffer.Add(Copy(InBuffer,1,LenToRead));

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

          if FChunkState=2 then // not the last chunk
            begin
            FChunkState:=0; // will continue with next chunk
            end
          else
            begin
            Request.Complete:=True;
            FHaveRequest:=False; // get ready for next request
            FRequestLine:=False;
            end;

          inherited TriggerDataReceived;

          Request.Started:=False;
          end;
        end
      else
        begin
        if LenToRead>0 then
          begin
          if LenToRead>length(InBuffer) then // need more than we have
            begin
            Request.ContentIn:=Request.ContentIn + length(InBuffer);

            FRequestBuffer.Add(InBuffer);

            Dec(LenToRead, length(InBuffer));

            InBuffer:='';
            end
          else
            begin
            Request.ContentIn:=Request.ContentIn + LenToRead;

            FRequestBuffer.Add(Copy(InBuffer,1,LenToRead));

            Delete(InBuffer,1,LenToRead);

            LenToRead:=0;
            Request.Complete:=True;
            FHaveRequest:=False; // get ready for next request
            FRequestLine:=False;
            end;
          end
        else
          begin
          Request.Complete:=True;
          FHaveRequest:=False; // get ready for next request
          FRequestLine:=False;
          end;

        inherited TriggerDataReceived;
        Request.Started:=False;
        end;

      if Request.Complete and not Response.Done then
        begin
        FRequestWaiting:=InBuffer<>'';
        Break; // need to wait for the request to be processed, before we can go to the next one.
        end;
      end
    else
      Break; // Failing to fetch a header will break the loop.
    end;
  end;

procedure TRtcWSockHttpServerProvider.WriteHeader(SendNow:boolean=True);
  var
    s:string;
  begin
  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  s:='HTTP/1.1 '+IntToStr(Response.StatusCode)+' '+Response.StatusText+CRLF+
     Response.HeaderText;

  if Request.Close then s:=s+'Connection: close'+CRLF;

  s:=s+CRLF;

  Response.Sending:=True;
  Response.Started:=True;

  if Response.SendContent and
    (Response['CONTENT-LENGTH']='')  then // streaming data
    begin
    LenToWrite:=-1;
    LenToSend:=-1;
    end
  else
    begin
    if not Response.SendContent then
      Response['CONTENT-LENGTH']:='';

    LenToWrite:=Response.ContentLength;
    LenToSend:=length(s) + Response.ContentLength;
    end;

  Response.Sent:=LenToWrite=0;

  if Response.Sent then
    TriggerLastWrite;

  FHeaderOut:=True;
  inherited Write(s, SendNow or (LenToWrite<=0));
  end;

procedure TRtcWSockHttpServerProvider.WriteHeader(const Header_Text:string; SendNow:boolean=True);
  var
    s:string;
  begin
  if FHeaderOut then
    raise Exception.Create('Last header intercepted with new header, before data sent out.');

  if Header_Text<>'' then
    begin
    Response.HeaderText:=Header_Text;

    s:='HTTP/1.1 '+IntToStr(Response.StatusCode)+' '+Response.StatusText+CRLF+
       Response.HeaderText;

    if Request.Close then s:=s+'Connection: close'+CRLF;

    s:=s+CRLF;
    end
  else
    begin
    s:='';
    Request.Close:=True;
    end;

  Response.Sending:=True;
  Response.Started:=True;

  if Response.SendContent and
    (Response['CONTENT-LENGTH']='')  then // streaming data
    begin
    LenToWrite:=-1;
    LenToSend:=-1;
    end
  else
    begin
    if not Response.SendContent then
      Response['CONTENT-LENGTH']:='';

    LenToWrite:=Response.ContentLength;
    LenToSend:=length(s) + Response.ContentLength;
    end;

  Response.Sent:=LenToWrite=0;

  if Response.Sent then
    TriggerLastWrite;

  FHeaderOut:=True;
  inherited Write(s, SendNow or (LenToWrite<=0));
  end;

procedure TRtcWSockHttpServerProvider.Write(const ResultData: string; SendNow:boolean=True);
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

  inherited Write(ResultData,SendNow);
  end;

function TRtcWSockHttpServerProvider.Read: string;
  begin
  if FRequestBuffer.Size>0 then
    begin
    Result:=FRequestBuffer.Get;
    FRequestBuffer.Clear;
    end
  else
    Result:='';
  end;

procedure TRtcWSockHttpServerProvider.TriggerDataSent;
  begin
  if Response.Sending then
    Response.Started:=False;

  inherited TriggerDataSent;

  if Response.Done then
    begin
    ClearRequest;
    if FRequestWaiting then
      TriggerDataReceived;
    end;
  end;

procedure TRtcWSockHttpServerProvider.TriggerDataOut;
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

end.
