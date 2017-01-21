unit Event_RS232;

interface
uses Classes, AdGSM, AdPort, SyncObjs;

type
  TSignalProc=procedure(Signal:Integer) of object;
  TEventRS232=class(TThread)
  private
    ComPort:TApdComPort;
    procedure TriggerAvail(CP: TObject; Count: Word);
  protected
    FPort:Integer;
    FSignal:TSignalProc;
    Bytes:array[0..4] of Byte;
    Triggers,TriggersOld,State:array[1..18] of Boolean;
    OnCount,OffCount:array[1..18] of Integer;
    BytePos:Integer;
    procedure Receive(B:Byte);
    procedure Execute;override;
  public
    constructor Create(Port:Integer;Signal:TSignalProc);
    procedure Test;
    destructor Destroy;override;
  end;

const
  OnTriggerCount=2;
  OffTriggerCount=4;

implementation
uses SysUtils, Windows, Messages, SIP_Monitor;
{ TEventRS232 }

constructor TEventRS232.Create;
begin
  FPort:=Port;
  FSignal:=Signal;
  inherited Create(False);
end;

const
  am_stop = wm_user+1;

destructor TEventRS232.Destroy;
begin
  if (ComPort<>nil) then PostMessage(ComPort.ComWindow,am_stop,0,0);
  Terminate;
  inherited;
  FreeAndNil(ComPort);
end;

procedure TEventRS232.Test;
begin
  Receive($a5);
  Receive(0);
  Receive(1);
  Receive(0);
  Receive(0);
  Receive(0);
  Receive($a5);
  Receive(0);
  Receive(0);
  Receive($a5);
  Receive(0);
  Receive(1);
  Receive(0);
  Receive($a6);
end;

procedure TEventRS232.TriggerAvail(CP: TObject; Count: Word);
var
  I: Integer;
begin
  for I := 1 to Count do
     Receive(Ord(ComPort.GetChar));
end;

procedure TEventRS232.Execute;
var
  Msg : TMsg;
begin
  if FPort<=0 then Exit;
  
  ComPort:=TApdComPort.Create(nil);
  ComPort.ComNumber:=FPort;
  ComPort.DataBits:=8;
  ComPort.Baud:=19200;
  ComPort.StopBits:=2;
  ComPort.HWFlowOptions:=[];
  ComPort.SWFlowOptions:=swfNone;
  ComPort.Parity:=pNone;
  ComPort.LogName:='RS232.log';
  ComPort.Logging:=tlOn;
  ComPort.TraceName:='RS232Trace.log';
  ComPort.Tracing:=tlOn;
  ComPort.OnTriggerAvail:=TriggerAvail;
  BytePos:=-1;

  ComPort.Open:=True;
  while not Terminated and GetMessage(Msg,0,0,0) and (Msg.Message <> am_Stop) do
    DispatchMessage(Msg);
  ComPort.Open:=False;
end;

procedure TEventRS232.Receive(B: Byte);
var
  I: Integer;
begin
  if B=$A5 then BytePos:=0;
  if BytePos<0 then Exit;

  Bytes[BytePos]:=B;
  BytePos:=BytePos+1;
  if BytePos>=5 then
  begin
    if ((Bytes[0]+Bytes[1]+Bytes[2]+Bytes[3]) and $FF)=Bytes[4] then
    begin
      SIPMonitor.Status_RS:=GetTick64;
      TriggersOld:=Triggers;
      for I := 0 to 17 do
        Triggers[I+1]:=(Bytes[1+I div 6] and (1 shl (I mod 6)))=0;
      for I := 1 to 18 do
      begin
        if Triggers[I]=TriggersOld[I] then
        begin
          if Triggers[I] then
            Inc(OnCount[I])
          else
            Inc(OffCount[I]);
          if OnCount[I]>OnTriggerCount-1 then OnCount[I]:=OnTriggerCount-1;
          if OffCount[I]>OffTriggerCount-1 then OffCount[I]:=OffTriggerCount-1;
        end
        else
        begin
          OnCount[I]:=0;
          OffCount[I]:=0;
        end;
        if State[I] then
        begin
          if OffCount[I]>=OffTriggerCount-1 then State[I]:=False;
        end
        else
        begin
          if OnCount[I]>=OnTriggerCount-1 then
          begin
            State[I]:=True;
            if Assigned(FSignal) then
              FSignal(I);
          end;
        end;
      end;

      {if Assigned(FSignal) then
        for I := 1 to 18 do
          if Triggers[I] and not TriggersOld[I] then
            FSignal(I);}
    end;
    BytePos:=-1;
  end;
end;

end.
