unit SIP_Monitor;

interface

function GetTick64:Int64;
function SIPMonitorStatus:String;

type
  TSIPMonitor=record
    Status_RS,
    Status_GSM,
    Count_GSM,
    Status_Mail,
    Count_Mail,
    Status_SIP:Int64;
    GSM_Registration:String;
  end;

var
  SIPMonitor:TSIPMonitor;

implementation
uses Windows, SysUtils, Util;

var
  OldTick:Int64=0;
  TickCount:Int64=0;

function GetTick64:Int64;
var NewTick:Int64;
begin
  NewTick:=GetTickCount;
  if NewTick>=OldTick then
    TickCount:=TickCount-OldTick+NewTick
  else
    TickCount:=TickCount-OldTick+NewTick+High(DWORD);
  Result:=TickCount;
  OldTick:=NewTick;
end;

function SIPMonitorStatus:String;
var X:TSIPMonitor;
  Tick:Int64;

  function Status(A,B:Int64):String;
  begin
    if A>B then
      Result:='false'
    else
      Result:='true';
  end;
begin
  X:=SIPMonitor;
  Tick:=GetTick64;
  Result:=Format('{rs:%s, gsm:%s, mail:%s, sip:%s, gsmcount:"%d", mailcount:"%d", gsmreg:%s}',[
    Status(Tick,X.Status_RS+5000),
    Status(Tick,X.Status_GSM+10000),
    Status(Tick,X.Status_Mail+60000),
    Status(Tick,X.Status_SIP+150000),
    X.Count_GSM,
    X.Count_Mail,
    StrEscape(X.GSM_Registration)
  ])

end;

end.
