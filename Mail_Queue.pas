unit Mail_Queue;

interface
uses Classes, SyncObjs, IdSMTP, IdMessage, IdComponent, IdCoderHeader, SIP_RingList;

type
  TMailQueue=class(TThread)
  private
    SMTP:TIdSMTP;
    Mail:TIdMessage;
    MailList:TThreadList;
    Event:TEvent;
    From:String;
    FSIPApp:TObject;
    procedure MailInitializeISO(var VTransferHeader: TTransfer;
      var VHeaderEncoding: Char; var VCharSet: string);
    procedure SMTPStatus(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: String);
    procedure Send(Email,Body, Subject:String);
    procedure Reconnect;
  protected
    procedure Execute;override;
  public
    constructor Create(SMTPHost,SMTPPort,SMTPUser,SMTPPassword,SMTPFrom:String;App:TObject);
    destructor Destroy;override;
    procedure Add(const Name,Email,Body,Subject:String;Address:TSIPAddress;QueueID:Cardinal);
  end;

implementation
uses SysUtils, Logger, Windows, SIP_Monitor, SIP_App, SIP_QueueManager;

{ TMailQueue }

procedure TMailQueue.SMTPStatus(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: String);
var S:String;
begin
  case AStatus of
    hsResolving:S:='resolving '+AStatusText;
    hsConnecting:S:='connecting '+AStatusText;
    hsConnected:S:='connected '+AStatusText;
    hsDisconnecting:S:='disconnecting '+AStatusText;
    hsDisconnected:S:='disconnected '+AStatusText;
    hsStatusText:S:=AStatusText;
  end;
  if Log<>nil then Log.Debug(S);
end;

type
  TMail=class(TObject)
    EMail:String;
    Body:String;
    Subject:String;
    Name:String;
    Address:TSIPAddress;
    QueueID:Cardinal;
  end;

procedure TMailQueue.Add(const Name, Email, Body, Subject: String;Address:TSIPAddress;QueueID:Cardinal);
var S:TMail;
begin
  S:=TMail.Create;
  S.Name:=Name;
  S.Email:=Email;
  S.Body:=Body;
  S.Subject:=Subject;
  S.Address:=Address;
  S.QueueID:=QueueID;
  TSIPApp(FSIPApp).SIPQueueManager.StartMail(Address,QueueID);
  MailList.Add(S);
  Event.SetEvent;
end;

constructor TMailQueue.Create;
begin
  FSIPApp:=App;
  SMTP:=TidSMTP.Create(nil);
  SMTP.MailAgent:='asos';
  SMTP.OnStatus:=SMTPStatus;
  SMTP.Host    :=SMTPHost;
  SMTP.Port    :=StrToIntDef(SMTPPort,25);
  SMTP.UserName:=SMTPUser;
  SMTP.Password:=SMTPPassword;
  Mail:=TIdMessage.Create(nil);
  Mail.Encoding:=mePlainText;
  Mail.ConvertPreamble:=True;
  Mail.OnInitializeISO := MailInitializeISO;
  From:=SMTPFrom;
  Event:=TEvent.Create(nil,True,False,'',False);
  MailList:=TThreadList.Create;
  inherited Create(False);
end;

destructor TMailQueue.Destroy;
begin
  Terminate;
  Event.SetEvent;
  inherited;
  FreeAndNil(Mail);
  FreeAndNil(SMTP);
end;

procedure TMailQueue.Execute;
var
  S:TMail;
  L:TList;
begin
  while not Terminated do
  begin
    L:=MailList.LockList;
    S:=nil;
    try
      SIPMonitor.Count_Mail:=L.Count;
      if L.Count>0 then
      begin
        S:=L[0];
        L.Delete(0);
      end;
      if (L.Count<=0) then Event.ResetEvent;
    finally
      MailList.UnlockList;
    end;
    if S<>nil then
    begin
      try
        try
          //s.Address.Status:='изпращане на EMail';
          Send(s.EMail,s.Body,s.Subject);
          s.Address.AddReport('Успешно изпратен EMail('+S.Name+') към '+s.EMail);
          TSIPApp(FSIPApp).SIPQueueManager.Succeed(s.Address,s.QueueID,pkMail);
        except
          on E:Exception do
          begin
            s.Address.AddReport('Неуспешно изпращане на EMail('+S.Name+') към '+s.EMail+' : '+E.Message);
            TSIPApp(FSIPApp).SIPQueueManager.Fail(s.Address,s.QueueID,pkMail);
            if Log<>nil then Log.Error('Грешка при изпращане към "'+s.EMail+'" на EMail "'+s.Body+'" : '+E.Message);
          end;
        end;
      finally
        FreeAndNil(S);
      end;
    end else
      Reconnect;
    if not Terminated then
      Event.WaitFor(15000);
  end;
end;

procedure TMailQueue.MailInitializeISO(var VTransferHeader: TTransfer;
  var VHeaderEncoding: Char; var VCharSet: string);
begin
  VCharSet:='windows-1251';
end;

procedure TMailQueue.Reconnect;
begin
  if SMTP.Host='' then Exit;

  try
  try
    SMTP.Connect;
    SIPMonitor.Status_Mail:=GetTick64;
  finally
    SMTP.Disconnect;
  end;
  except

  end;

end;

procedure TMailQueue.Send(Email, Body, Subject: String);
begin
  if SMTP.Host='' then
  begin
    Log.Error('Липсва настройка на SMTP Host. Не е изпратено съобщение до '+EMail);
    raise Exception.Create('липсва настройка SMTP Host');
  end;

  Mail.Clear;
  Mail.Charset:='windows-1251';
  Mail.AddHeader('X-Generator: asos');
  Mail.From.Address:=From;
  Mail.Recipients.EmailAddresses:=Email;
  Mail.Subject:=Subject;
  Mail.Body.Text:=Body;

  if SMTP.Connected then SMTP.Disconnect;

  SMTP.Connect;
  try
    SIPMonitor.Status_Mail:=GetTick64;
    SMTP.Send(Mail);
  finally
    if SMTP.Connected then SMTP.Disconnect;
  end;

end;

end.
