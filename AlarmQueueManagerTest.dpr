program AlarmQueueManagerTest;

uses
  Forms,
  frm_QueueManagerTest in 'frm_QueueManagerTest.pas' {frmQueueManagerTest},
  SIP_QueueManager in 'SIP_QueueManager.pas',
  SIP_RingList in 'SIP_RingList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmQueueManagerTest, frmQueueManagerTest);
  Application.Run;
end.
