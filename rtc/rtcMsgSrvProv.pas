{
  "Message Server Connection Provider" - Copyright (c) Danijel Tkalcec
  @html(<br>)

  @exclude
}
unit rtcMsgSrvProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  Windows, Classes, SysUtils,

  rtcFastStrings,
  rtcLog, rtcSyncObjs, rtcConn,
  rtcConnProv, rtcThrConnProv;

type
  TRtcMessageServerProvider = class(TRtcNoThrServerProvider)
  private
    FRequestLine,
    FHaveRequest:boolean;
    InBuffer:string;
    LenToRead:integer;

    OutStream:TStream;

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

  public
    constructor Create; override;
    destructor Destroy; override;

    function GetParent:TRtcConnectionProvider; override;

    procedure Connect;
    procedure ExecuteRequest(InStream, _OutStream:TStream);

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

{ TRtcMessageServerProvider }

const
  CRLF = #13#10;
  END_MARK = CRLF+CRLF;

constructor TRtcMessageServerProvider.Create;
  begin
  inherited;
  FCS:=TRtcCritSec.Create;

  FRequestLine:=False;
  FHaveRequest:=False;
  InBuffer:='';
  LenToRead:=0;

  FRequestBuffer:=TRtcHugeString.Create;

  LenToWrite:=0;
  LenToSend:=0;
  FHeaderOut:=False;

  FRequest:=nil;
  FResponse:=nil;
  end;

destructor TRtcMessageServerProvider.Destroy;
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

procedure TRtcMessageServerProvider.WriteHeader;
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
    raise Exception.Create('Streaming content not supported by a Message Server.');
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

  OutStream.Write(s[1], length(s));

  FDataOut:=0;
  if Response.Sent then
    try
      TriggerDataOut;
    finally
      TriggerDataSent;
    end;
  end;

procedure TRtcMessageServerProvider.WriteHeader(const Header_Text:string);
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
    raise Exception.Create('Streaming content not supported by a Message Server.');
    s:='';
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

  OutStream.Write(s[1], length(s));

  if Response.Sent then
    begin
    FDataOut:=0;
    try
      TriggerDataOut;
    finally
      TriggerDataSent;
      end;
    end;
  end;

procedure TRtcMessageServerProvider.Write(const ResultData: string; SendNow:boolean=True);
  var
    len:cardinal;
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
  OutStream.Write(ResultData[1],length(ResultData));

  FDataOut:=len;
  try
    TriggerDataOut;
  finally
    FDataOut:=0;
    TriggerDataSent;
    end;
  end;

function TRtcMessageServerProvider.Read: string;
  begin
  if FRequestBuffer.Size>0 then
    begin
    Result:=FRequestBuffer.Get;
    FRequestBuffer.Clear;
    end
  else
    Result:='';
  end;

procedure TRtcMessageServerProvider.TriggerDataSent;
  begin
  if Response.Sending then
    Response.Started:=False;

  inherited TriggerDataSent;
  end;

procedure TRtcMessageServerProvider.TriggerDataOut;
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

function TRtcMessageServerProvider.GetParent: TRtcConnectionProvider;
  begin
  Result:=nil;
  end;

procedure TRtcMessageServerProvider.Listen;
  begin
  State:=conListening;
  TriggerListenStart;
  end;

procedure TRtcMessageServerProvider.Disconnect;
  begin
  InternalDisconnect;
  end;

procedure TRtcMessageServerProvider.InternalDisconnect;
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

procedure TRtcMessageServerProvider.CopyFrom(Dup: TRtcConnectionProvider);
  begin
  //
  end;

procedure TRtcMessageServerProvider.Enter;
  begin
  FCS.Enter;
  end;

procedure TRtcMessageServerProvider.Leave;
  begin
  FCS.Leave;
  end;

procedure TRtcMessageServerProvider.Connect;
  begin
  // We need to cheat our component user, since we don't have that information ...

  LocalAddr:='127.0.0.1'; //ServerVariable('SERVER_ADDR',128);
  LocalPort:='80'; // ServerVariable('SERVER_PORT',32);
  PeerAddr:='127.0.0.1'; // ServerVariable('REMOTE_ADDR',128);
  PeerPort:='80'; // ServerVariable('REMOTE_PORT',32);

  State:=conActive;
  TriggerConnectionAccepted;
  TriggerConnecting;
  TriggerConnect;
  end;

procedure TRtcMessageServerProvider.ExecuteRequest(InStream, _OutStream:TStream);
  var
    StatusLine,
    HeadStr:string;

    len,len2:int64;

    HeadLen,
    MyPos:integer;

    s:string;

  procedure ClearRequest;
    begin
    FRequestBuffer.Clear;

    FRequestLine:=False;
    FRequest.Clear;
    FResponse.Clear;
    LenToRead:=0;
    end;

  procedure ProcessData(const data:string);
    begin
    InBuffer:=InBuffer + data;

    while InBuffer<>'' do
      begin
      if not FHaveRequest then // Don't have the header yet ...
        begin
        if not FRequestLine then
          begin
          MyPos:=Pos(CRLF,InBuffer);
          if (MyPos>0) then
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
              raise Exception.Create('HTTP Header error: Request Header missing!');
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
                raise Exception.Create('HTTP Header error: Request method missing!');
                end;

              Request.Method:=Trim(Copy(StatusLine,1,MyPos-1));
              Delete(StatusLine,1,MyPos);

              // Request FileName
              MyPos:=Pos(' ',StatusLine);
              if MyPos<=0 then
                begin
                Request.FileName:=StatusLine;
                raise Exception.Create('HTTP Header error: Request file name missing!');
                end;

              Request.FileName:=Copy(StatusLine,1,MyPos-1);
              Delete(StatusLine,1,MyPos);

              // Request HTTP type
              MyPos:=Pos('/',StatusLine);
              if MyPos<=0 then
                raise Exception.Create('HTTP Header error: HTTP type missing!');

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
                else if CompareText(s,'CONNECTION')=0 then
                  begin
                  if Trim(Uppercase(StatusLine))='CLOSE' then
                    Request.Close:=True
                  else if Trim(Uppercase(StatusLine))='KEEP-ALIVE' then
                    Request.Close:=False;
                  end
                else if CompareText(s,'CONTENT-TYPE')=0 then
                  begin
                  Request.ContentType:=StatusLine;
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
                  end;

                Request[s]:=StatusLine;
                end;

              MyPos:=Pos(CRLF, HeadStr);
              end;

            StatusLine:='';
            HeadStr:='';
            end;
          end;
        end;

      if FHaveRequest then // Processing a request ...
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

        TriggerDataReceived;
        Request.Started:=False;

        if Request.Complete and not Response.Done then
          Break; // need to wait for the request to be processed, before we can go to the next one.
        end
      else
        Break; // Failing to fetch a header will break the loop.
      end;
    end;

  begin
  if State<>conActive then Connect;

  OutStream:=_OutStream;
  InStream.Position:=0;

  while InStream.Position<InStream.Size do
    begin
    len:=InStream.Size-InStream.Position;
    if len>32000 then len:=32000;

    SetLength(s,len);
    len2:=InStream.Read(s[1],len);

    FDataIn:=len;
    TriggerDataIn;

    ProcessData(s);
    if len2<len then Break;
    end;

  if State<>conInactive then Disconnect;
  end;

end.
