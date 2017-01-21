unit SIP_RingList;

interface
uses Classes,DB, SyncObjs;

type
  TSIPAddress=class(TCriticalSection)
  private
    FStatus: String;
    FReport: String;
    procedure SetStatus(const Value: String);
  public
    Phone,
    DTMF,
    MobilePhone,
    HomePhone,
    OrganisationName,
    DivisionName,
    PersonName,
    PositionName,
    Site,
    EMail,
    Code,
    RingGroupName,
    Address1,
    Address2:String;
    Tick:Int64;
    ID:Integer;
    RetryBusy,RetryBusyRoute,RetryFailDTMF:Integer;
    DelayBusy,DelayBusyRoute,DelayFailDTMF:Integer;
    RingSequence:String;
    ActivePhone:String;
    TryCount:Integer;
    StatusTime:TDateTime;
    ResultSIP,ResultMail,ResultSMS:String;
    function Method:String;
    function SucceedMethod:String;
    function ObjectName:String;
    property Status:String read FStatus write SetStatus;
    procedure AddReport(const S:String);
    function SucceedState:String;
  end;

  TSIPRingList=class(TThreadList)
  private
    Counter:Integer;
  public
    procedure Add(Q:TDataSet);
    procedure AddAddress(A:TSIPAddress;Delay:Cardinal);
    function Get:TSIPAddress;
    function  Count:Integer;
    function Status(Prefix:String;IDPrefix:String;Journal:Boolean;AutoState:Boolean):String;
    destructor Destroy;override;
  end;

implementation

uses Windows,Util, SysUtils, SIP_Monitor;

{ TSIPCallList }

procedure TSIPRingList.Add(Q:TDataSet);
var A:TSIPAddress;
  function Field(const S:String):String;
  var F:TField;
  begin
    F:=Q.FindField(S);
    if F<>nil then
      Result:=F.AsString
    else
      Result:='';
  end;
begin
  if (Field('Phone')='') and (Field('MobilePhone')='') and (Field('HomePhone')='') then Exit;
  A:=TSIPAddress.Create;

  A.ID:=InterlockedIncrement(Counter);
  A.Phone:=Field('Phone');
  A.MobilePhone:=Field('MobilePhone');
  A.HomePhone:=Field('HomePhone');
  A.DTMF:=Field('DTMF');
  A.Code:=Field('Code');
  A.RingGroupName:=Field('RingGroupName');
  A.OrganisationName:=Field('OrganisationName');
  A.DivisionName:=Field('DivisionName');
  A.PositionName:=Field('PositionName');
  A.PersonName:=Field('PersonName');
  A.email:=Field('email');
  A.site:=Field('site');
  A.address1:=Field('address1');
  A.address2:=Field('address2');
  A.TryCount:=1;

  A.Tick:=0;
  AddAddress(A,0);
end;

procedure TSIPRingList.AddAddress;
var
  B:TSIPAddress;
  L:TList;
  I:Integer;
begin
  if (Delay=0) then
    inherited Add(A)
  else
  begin
    A.Tick:=GetTick64+Delay*1000;
    L:=LockList;
    try
      if L.Count=0 then
        L.Add(A)
      else
        for I := L.Count - 1 downto 0 do
        begin
          B:=L[I];
          if (I=0) or (A.Tick>=B.Tick) then
          begin
            L.Insert(I,A);
            Break;
          end;
        end;
    finally
      UnlockList;
    end;
  end;
end;

function TSIPRingList.Count: Integer;
var L:TList;
begin
  L:=LockList;
  try
    Result:=L.Count;
  finally
    UnlockList
  end;
end;

destructor TSIPRingList.Destroy;
var A:TSIPAddress;
    L:TList;
  I: Integer;
begin
  L:=LockList;
  try
    for I := 0 to L.Count - 1 do
    begin
      A:=L.Items[I];
      A.Free;
    end;
    L.Clear;
  finally
    UnlockList;
  end;
  inherited;
end;

function TSIPRingList.Get;
var A:TSIPAddress;
    L:TList;
begin
  L:=LockList;
  try
    if L.Count>0 then
    begin
      A:=L.Items[0];
      if (A.Tick>GetTick64) then
      begin
        Result:=nil;
        Exit;
      end;
      L.Delete(0);
      Result:=A;
    end else
    begin
      Result:=nil;
    end;
  finally
    UnlockList;
  end;
end;

function TSIPRingList.Status;
var A:TSIPAddress;
    L:TList;
    I:Integer;
begin
  Result:='';
  L:=LockList;
  try
    for I := 0 to L.Count - 1 do
    begin
      A:=L.Items[I];
      if I>0 then Result:=Result+',{' else Result:=Result+'{';
      Result:=Result+Prefix;
      if AutoState then
        Result:=Result+',"state":"'+A.SucceedState+'",';
      Result:=Result+'"try":'+IntToStr(A.TryCount)+',';
      Result:=Result+'"status":'+StrEscape(A.Status)+',';
      if A.StatusTime>0 then
        Result:=Result+'"statustime":'+StrEscape(FormatDateTime('yyyy-mm-dd hh:nn:ss',A.StatusTime))+',';
      {if A.Data<>'' then
        Result:=Result+'"data":'+StrEscape(A.Data)+',';}
      Result:=Result+'"resultsip":'+StrEscape(A.ResultSIP)+',';
      Result:=Result+'"resultsms":'+StrEscape(A.ResultSMS)+',';
      Result:=Result+'"resultmail":'+StrEscape(A.ResultMail)+',';
      if Journal then
        Result:=Result+'"journal":'+StrEscape(A.FReport)+',';
      Result:=Result+'"id":'+StrEscape(IDPrefix+IntToStr(A.ID))+',';
      Result:=Result+'"phone":'+StrEscape(A.Phone)+',';
      Result:=Result+'"mobilephone":'+StrEscape(A.MobilePhone)+',';
      Result:=Result+'"homephone":'+StrEscape(A.HomePhone)+',';
      Result:=Result+'"code":'+StrEscape(A.Code)+',';
      Result:=Result+'"ringgroupname":'+StrEscape(A.RingGroupName)+',';
      Result:=Result+'"organisationname":'+StrEscape(A.OrganisationName)+',';
      Result:=Result+'"divisionname":'+StrEscape(A.DivisionName)+',';
      Result:=Result+'"positionname":'+StrEscape(A.positionname)+',';
      Result:=Result+'"personname":'+StrEscape(A.personname)+',';
      Result:=Result+'"email":'+StrEscape(A.email)+',';
      Result:=Result+'"site":'+StrEscape(A.site)+',';
      Result:=Result+'"address1":'+StrEscape(A.address1)+',';
      Result:=Result+'"address2":'+StrEscape(A.address2);
      Result:=Result+'}';
    end;
  finally
    UnlockList;
  end;
end;

{ TSIPAddress }

procedure TSIPAddress.AddReport(const S: String);
begin
  Enter;
  try
    if FReport<>'' then FReport:=FReport+#13#10;
    FReport:=FReport+FormatDateTime('yyyy-mm-dd hh:nn:ss ',Now)+S;
  finally
    Leave;
  end;
end;

function TSIPAddress.Method: String;
begin
  Result:='';
  if RingSequence='' then
  begin
    Result:='служ. тел.';
  end else
  case RingSequence[1] of
      '1':Result:='служ. тел.';
      '2':Result:='дом. тел.';
      '3':Result:='моб. тел.';
      's':Result:='SMS';
      'm':Result:='EMail';
  end;
end;

function TSIPAddress.ObjectName: String;
  procedure Add(const S:String);
  begin
    if S<>'' then
    begin
      if Result<>'' then Result:=Result+', ';
      Result:=Result+S;
    end;
  end;
begin
  Result:='';
  Add(OrganisationName);
  Add(DivisionName);
  Add(PositionName);
end;

procedure TSIPAddress.SetStatus(const Value: String);
begin
  FStatus := Value;
  StatusTime:=Now;
end;

function TSIPAddress.SucceedMethod: String;
begin
  Result:='';
  if RingSequence='' then
  begin
    Result:='succeedwork';
  end else
  case RingSequence[1] of
      '1':Result:='succeedwork';
      '2':Result:='succeedhome';
      '3':Result:='succeedmobile';
      's':Result:='fail';
      'm':Result:='fail';
  end;
end;

function TSIPAddress.SucceedState: String;
begin
  if SameText(ResultSIP,'fail') or SameText(ResultSIP,'') then
    Result:='алтернативно оповестени'
  else
    Result:='успешно оповестени';
end;

end.
