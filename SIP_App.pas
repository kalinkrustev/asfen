unit SIP_App;

interface
uses SIP_Status,Classes,SIP_QueueManager,Contnrs, SMS_Queue, Mail_Queue, Event_RS232;

type
  TSIPApp = class (TObject)
  public
    SIPStatus:TSIPStatus;
    SIPQueueManager:TSIPQueueManager;
    SIPChannels:TObjectList;
    SMSQueue:TSMSQueue;
    MailQueue:TMailQueue;
    EventRS232:TEventRS232;
    constructor Create(ChannelCount,ComPort,Bits,Baud,StopBits:Integer;Parity:String;RSPort:Integer;Signal:TSignalProc;SMTPHost,SMTPPort,SMTPUser,SMTPPass,SMTPFrom:String);
    procedure Stop(EventID:Integer);
    destructor  Destroy;override;
  end;

implementation
uses SysUtils, SIP_Call;
{ TSIPApp }

constructor TSIPApp.Create;
var I:Integer;
begin
  SIPStatus:=TSIPStatus.Create(ChannelCount);
  SIPQueueManager:=TSIPQueueManager.Create(Self,ChannelCount+1);
  SMSQueue:=TSMSQueue.Create(ComPort,Bits,Baud,StopBits,Parity,Self);
  MailQueue:=TMailQueue.Create(SMTPHost,SMTPPort,SMTPUser,SMTPPass,SMTPFrom,Self);
  SIPChannels:=TObjectList.Create;
  SIPChannels.OwnsObjects:=True;
  for I := 1 to ChannelCount do
    SIPChannels.Add(TSIPCall.Create(Self,I));
  EventRS232:=TEventRS232.Create(RSPort,Signal);
end;

destructor TSIPApp.Destroy;
begin
  SIPQueueManager.Close;
  FreeAndNil(EventRS232);
  FreeAndNil(SIPChannels);
  FreeAndNil(MailQueue);
  FreeAndNil(SMSQueue);
  FreeAndNil(SIPQueueManager);
  FreeAndNil(SIPStatus);
  inherited;
end;

procedure TSIPApp.Stop(EventID: Integer);
var
  I: Integer;
begin
  SIPQueueManager.Stop(EventID);
  for I := 0 to SIPChannels.Count - 1 do
  TSIPCall(SIPChannels[I]).StopEvent(EventID);
end;

end.
