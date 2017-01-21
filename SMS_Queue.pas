unit SMS_Queue;

interface
uses Classes, AdGSM, AdPort, SyncObjs, SIP_RingList;

type
  TSMSQueue=class(TThread)
  private
    ComPort:TApdComPort;
    GSM:TApdGSMPhone;
    SMSList:TThreadList;
    Event:TEvent;
    FSIPApp:TObject;
  protected
    FPort,FBits,FBaud,FStopBits:Integer;
    FParity:String;
    procedure Execute;override;
  public
    constructor Create(const Port,Bits,Baud,StopBits:Integer;Parity:String;App:TObject);
    destructor Destroy;override;
    procedure Add(const Name,Phone,Msg:String;Address:TSIPAddress;QueueID:Cardinal);
  end;

implementation
uses SysUtils,Windows, Logger, SIP_Monitor, SIP_App, SIP_QueueManager;

type
  TSMS=class(TObject)
    Name,
    Phone:String;
    Msg:String;
    Address:TSIPAddress;
    QueueID:Cardinal;
  end;

{ TSMSQueue }

procedure TSMSQueue.Add(const Name, Phone, Msg: String;Address:TSIPAddress;QueueID:Cardinal);
var S:TSMS;
begin
  S:=TSMS.Create;
  S.Name:=Name;
  S.Phone:=Phone;
  S.Msg:=Msg;
  S.Address:=Address;
  S.QueueID:=QueueID;
  TSIPApp(FSIPApp).SIPQueueManager.StartSMS(Address,QueueID);
  SMSList.Add(S);
  Event.SetEvent;
end;

constructor TSMSQueue.Create;
begin
  FSIPApp:=App;
  FPort:=Port;
  FBits:=Bits;
  FBaud:=Baud;
  FStopBits:=StopBits;
  FParity:=Parity;
  Event:=TEvent.Create(nil,True,False,'',False);
  SMSList:=TThreadList.Create;
  inherited Create(False);
end;

destructor TSMSQueue.Destroy;
begin
  Terminate;
  Event.SetEvent;
  inherited;
  FreeAndNil(GSM);
  FreeAndNil(ComPort);
  FreeAndNil(SMSList);
  FreeAndNil(Event);
end;

procedure TSMSQueue.Execute;
var
  S:TSMS;
  L:TList;
begin
  if FPort<=0 then Exit;
  
  ComPort:=TApdComPort.Create(nil);
  ComPort.ComNumber:=FPort;
  ComPort.DataBits:=FBits;
  ComPort.Baud:=FBaud;
  ComPort.StopBits:=FStopBits;
  ComPort.HWFlowOptions:=[hwfUseRTS,hwfRequireCTS];
  ComPort.SWFlowOptions:=swfNone;
  ComPort.LogName:='GSM.log';
  ComPort.Logging:=tlOn;
  ComPort.TraceName:='GSMTrace.log';
  ComPort.Tracing:=tlOn;

  if SameText(FParity,'odd') then
    ComPort.Parity:=pOdd
  else
  if SameText(FParity,'even') then
    ComPort.Parity:=pEven
  else
  if SameText(FParity,'mark') then
    ComPort.Parity:=pMark
  else
  if SameText(FParity,'space') then
    ComPort.Parity:=pSpace
  else
    ComPort.Parity:=pNone;

  GSM:=TApdGSMPhone.Create(nil);
  GSM.QuickConnect:=True;
  GSM.ComPort:=ComPort;
  GSM.GSMMode:=gmText;
  //GSM.SMSCenter:='+35988000301';
  GSM.Connect;
  while not Terminated do
  begin
    L:=SMSList.LockList;
    S:=nil;
    try
      GSM.Diagnose;
      SIPMonitor.Status_GSM:=GetTick64;
      SIPMonitor.GSM_Registration:=GSM.Registration;
    except
      SIPMonitor.GSM_Registration:='';
    end;
    
    if SIPMonitor.GSM_Registration<>'' then
      try
        SIPMonitor.Count_GSM:=L.Count;
        if L.Count>0 then
        begin
          S:=L[0];
          L.Delete(0);
        end;
        if (L.Count<=0) then Event.ResetEvent;
      finally
        SMSList.UnlockList;
      end;

    if S<>nil then
    begin
      try
        GSM.SMSAddress:=S.Phone;
        GSM.SMSMessage:=S.Msg;
        if Log<>nil then Log.Info('SMS('+S.Phone+')'+StringReplace(StringReplace(S.Msg,#13,'<CR>',[rfReplaceAll]),#10,'<LF>',[rfReplaceAll]));
        try
          //s.Address.Status:='изпращане на SMS';
          GSM.SendMessage;
          //Sleep(2000);
          SIPMonitor.Status_GSM:=GetTick64;
          s.Address.AddReport('Успешно изпратен SMS('+S.Name+') към '+s.Phone);
          TSIPApp(FSIPApp).SIPQueueManager.Succeed(s.Address,s.QueueID,pkSMS);
        except
          on E:Exception do
          begin
            s.Address.AddReport('Неуспешно изпращане на SMS('+S.Name+') към '+s.Phone+' : '+E.Message);
            TSIPApp(FSIPApp).SIPQueueManager.Fail(s.Address,s.QueueID,pkSMS);
            if Log<>nil then Log.Error('Грешка при изпращане към "'+s.Phone+'" на SMS "'+s.Msg+'" : '+E.Message);
          end;
        end;
      finally
        FreeAndNil(S);
      end;
    end;
    if not Terminated then Event.WaitFor(2000);
  end;
end;

end.
