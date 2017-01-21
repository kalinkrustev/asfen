unit SIP_QueueManager;

interface
uses SyncObjs, Classes, SIP_RingList, Contnrs, SIP_Env, SIP_Event;

type
  TProcessingKind =(pkSIP,pkSMS,pkMail);
  TSIPQueueManager=class(TCriticalSection)
  private
    FList:TObjectList;
    FCurrent:Integer;
    FEvent:TEvent;
    FQuitEvent:TEvent;
    Handles:array [1..2] of THandle;
    FSIPApp:TObject;
    FID:Integer;
    FCounter:Integer;
    procedure Status(S:String);inline;
    procedure ShowStatus;
  public
    constructor Create(App:TObject;ID:Integer);
    destructor Destroy;override;
    procedure Add(ScriptID:Integer;Addresses:TSIPRingList; Count:Integer; SIPEnv:ISIPEnv; SIPEvent:TSIPEvent; var StartID:Integer; var Counter:PInteger;var Report:String);
    procedure Next(out ScriptID:Integer;out Address:TSIPAddress;out QueueID:Cardinal;out Env:ISIPEnv; out Event:TSIPEvent; out StartID:Integer);
    procedure Retry(Address:TSIPAddress;Delay,QueueID:Cardinal);
    procedure Fail(Address:TSIPAddress;QueueID:Cardinal;ProcessingKind:TProcessingKind);
    procedure Succeed(Address:TSIPAddress;QueueID:Cardinal;ProcessingKind:TProcessingKind);
    procedure StartMail(Address:TSIPAddress;QueueID:Cardinal);
    procedure StartSMS(Address:TSIPAddress;QueueID:Cardinal);
    function QueueStatus(ID:Integer):String;
    procedure Stop(EventID:Integer);
    function Mode(EventID:Integer):Integer;
    procedure Close;
    function IsEmpty:Boolean;
  end;

implementation
uses Windows,SysUtils,SIP_App,Util,SIP_Monitor;

type
  TSIPQueueItem=class(TObject)
    ScriptID:Integer;
    Addresses:TSIPRingList;
    AddressesRetry:TSIPRingList;
    AddressesSuccess:TSIPRingList;
    AddressesFailure:TSIPRingList;
    AddressesProcessing:TSIPRingList;
    EMails:TSIPRingList;
    SMSes:TSIPRingList;
    AddressCount:PInteger;
    AddressCounter:Integer;
    Count:Integer;
    Current:Integer;
    Retry:Boolean;
    Env:ISIPEnv;
    Event:TSIPEvent;
    QueueID:Cardinal;
    Stop:Boolean;
    StartID:Integer;
    ID:Integer;
    destructor Destroy;override;
    function Status(Journal:Boolean):String;
  end;

{ TSIPQueueManager }

var
  EventCounter:Integer=0;
  ItemCounter:Integer=0;

procedure TSIPQueueManager.Add(ScriptID: Integer; Addresses: TSIPRingList; Count:Integer; SIPEnv:ISIPEnv; SIPEvent:TSIPEvent; var StartID:Integer; var Counter:PInteger;var Report:String);
var Item:TSIPQueueItem;
    AddressCount:String;
    ScriptName:String;
    IsFirst:Boolean;
begin
  Acquire;
  try

    Inc(FCounter);
    Item:=TSIPQueueItem.Create;
    Item.ID:=3*InterlockedIncrement(ItemCounter);
    if StartID>0  then
    begin
      IsFirst:=False;
      Item.StartID:=StartID;
      Item.AddressCount:=Counter;
    end
    else
    begin
      IsFirst:=True;
      Item.StartID:=InterlockedIncrement(EventCounter);
      Item.AddressCount:=@Item.AddressCounter;
      Counter:=Item.AddressCount;
      StartID:=Item.StartID;
    end;

    Item.QueueID:=FCounter;
    Item.ScriptID:=ScriptID;
    Item.Addresses:=Addresses;
    Item.AddressesRetry:=TSIPRingList.Create;
    Item.AddressesFailure:=TSIPRingList.Create;
    Item.AddressesSuccess:=TSIPRingList.Create;
    Item.AddressesProcessing:=TSIPRingList.Create;
    Item.EMails:=TSIPRingList.Create;
    Item.SMSes:=TSIPRingList.Create;
    if Item.Addresses<>nil then
      Item.AddressCount^:=Item.AddressCount^+Item.Addresses.Count
    else
      Item.AddressCount^:=Item.AddressCount^+1;

    Item.Current:=0;
    Item.Count:=Count;
    Item.Retry:=False;
    Item.Env:=SIPEnv;
    Item.Event:=SIPEvent;
    Item.Event.BeginItem;
    ScriptName:=SIPEnv.SIPLibrary.ScriptName[ScriptID];
    if ScriptName<>'' then
      ScriptName:=' => Оповестяване по сценарий "'+ScriptName+'"';

    if Addresses=nil then
      AddressCount:=', индивидуално'
    else
    if Addresses.Count=1 then
      AddressCount:=' на 1 обект'
    else
    if Addresses.Count>0 then
      AddressCount:=' на '+IntToStr(Addresses.Count)+' обекта'
    else
      AddressCount:='';

    if ScriptName<>'' then Report:=Report+ScriptName+AddressCount+#13#10;

    FList.Add(Item);
    ShowStatus;
    if FList.Count=1 then FEvent.SetEvent;
  finally
    Release;
  end;
end;

procedure TSIPQueueManager.Close;
begin
  FQuitEvent.SetEvent;
end;

constructor TSIPQueueManager.Create;
begin
  inherited Create;
  FID:=ID;
  FSIPApp:=app;
  FEvent:=TEvent.Create(nil,False,False,'',False);
  FQuitEvent:=TEvent.Create;
  Handles[1]:=FQuitEvent.Handle;
  Handles[2]:=FEvent.Handle;
  FList:=TObjectList.Create;
  FList.OwnsObjects:=True;
  FCurrent:=0;
end;

destructor TSIPQueueManager.Destroy;
begin
  FreeAndNil(FQuitEvent);
  FreeAndNil(FEvent);
  FreeAndNil(FList);
  inherited;
end;

procedure TSIPQueueManager.Fail(Address: TSIPAddress; QueueID: Cardinal;ProcessingKind:TProcessingKind);
var
  Q:TSIPQueueItem;
  I,J: Integer;
begin
  Acquire;
  try
    for I := 0 to FList.Count - 1 do
    begin
      Q:=TSIPQueueItem(FList[I]);
      if Q.QueueID=QueueID then
      begin
        try
          J:=Q.AddressesSuccess.LockList.IndexOf(Address);
        finally
          Q.AddressesSuccess.UnlockList;
        end;
        case ProcessingKind of
          pkSIP: begin
            Q.AddressesProcessing.Remove(Address);
            Address.ResultSIP:='fail';
          end;
          pkSMS: begin
            Q.SMSes.Remove(Address);
            Address.ResultSMS:='fail';
          end;
          pkMail: begin
            Q.EMails.Remove(Address);
            Address.ResultMail:='fail';
          end;
        end;
        if J<0 then
          try
            J:=Q.SMSes.LockList.IndexOf(Address);
          finally
            Q.SMSes.UnlockList;
          end;
        if J<0 then
          try
            J:=Q.EMails.LockList.IndexOf(Address);
          finally
            Q.EMails.UnlockList;
          end;
        if J<0 then
          Q.AddressesFailure.AddAddress(Address,0);
        if ((Q.Addresses=nil) or (Q.Addresses.Count<=0)) and (Q.AddressesRetry.Count<=0) and (Q.AddressesProcessing.Count<=0) and (Q.EMails.Count<=0) and (Q.SMSes.Count<=0) then
          Q.Event.EndItem;
        Break;
      end;
    end;
  finally
    Release;
  end;
end;

function TSIPQueueManager.IsEmpty: Boolean;
begin
  Acquire;
  try
    Result:=FList.Count<=0;
  finally
    Release;
  end;
end;

function TSIPQueueManager.Mode(EventID: Integer): Integer;
var
  Q:TSIPQueueItem;
  I: Integer;
begin
  Result:=0;
  Acquire;
  try
    for I := FList.Count - 1 downto 0 do
    begin
      Q:=TSIPQueueItem(FList[I]);
      if Q.StartID=EventID then
      begin
        Result:=Q.Env.DatabaseID;
        Break;
      end;
    end;
  finally
    Release;
  end;
end;

procedure TSIPQueueManager.Next(out ScriptID:Integer;out Address:TSIPAddress;out QueueID:Cardinal;out Env:ISIPEnv; out Event:TSIPEvent; out StartID:Integer);
var newAddress:TSIPAddress;
    Q:TSIPQueueItem;
    W:DWORD;
    Finished:Boolean;
    Stop:Integer;
    DoSleep:Boolean;
begin
  W:=WaitForMultipleObjects(2,@Handles[1],False,INFINITE);
  if W<>WAIT_OBJECT_0+1 then
  begin
    ScriptID:=-1;
    Address:=nil;
    Exit;
  end;
  DoSleep:=False;
  Acquire;
  try
  try
    ScriptID:=0;
    Address:=nil;
    Env:=nil;
    Event:=nil;
    StartID:=0;
    if FList.Count>0 then
    begin
      Stop:=FCurrent;
      repeat
        Q:=TSIPQueueItem(FList[FCurrent]);
        if Q.Addresses=nil then
        begin
          newAddress:=nil;
          Address:=nil;
          QueueID:=0;
          ScriptID:=Q.ScriptID;
          Env:=Q.Env;
          Event:=Q.Event;
          StartID:=Q.StartID;
          if Q.Event<>nil then
            Q.Event.UpdateWait(MaxAnnounce);
          FList.Delete(FCurrent);
          if FCurrent>=FList.Count then FCurrent:=0;
          if Stop>=FList.Count then Stop:=Flist.Count-1;
        end
        else
        begin
          newAddress:=nil;
          if Q.Stop then
          begin
            Finished:=Q.Event.Finished
          end else
          if Q.Count<=0 then
            Finished:=True
          else
          begin
            if Q.Retry then
            begin
              newAddress:=Q.AddressesRetry.Get();
              Q.Retry:=Q.Addresses.Count<=0;
            end else
              newAddress:=nil;

            if newAddress=nil then
            begin
              Q.Retry:=True;
              newAddress:=Q.Addresses.Get;
            end;

            if newAddress<>nil then
            begin
              Q.Event.UpdateWait(MaxAnnounce);
              Q.AddressesProcessing.AddAddress(newAddress,0);
              Q.Event.AddReport(FormatDateTime('yyyy-mm-dd hh:nn:ss ',Now)+'Оповестяване на '+newAddress.ObjectName+' (опит '+IntToStr(newAddress.TryCount)+')'#13#10,'');
            end;

            //Finished:=Q.Event.Finished;
            Finished:=Q.Event.Finished and ((Q.Addresses=nil) or (Q.Addresses.Count<=0)) and (Q.AddressesRetry.Count<=0) and (Q.AddressesProcessing.Count<=0) and (Q.EMails.Count<=0) and (Q.SMSes.Count<=0);
          end;
          if Finished then
          begin
            FList.Delete(FCurrent);
            if FCurrent>=FList.Count then FCurrent:=0;
            if Stop>=FList.Count then Stop:=Flist.Count-1;
          end else
          if newAddress=nil then
          begin
            Q.Current:=0;
            FCurrent:=(FCurrent+1) mod FList.Count;
            if FCurrent=Stop then
            begin
              DoSleep:=True;
              Break;
            end;
          end else
          begin
            Address:=newAddress;
            QueueID:=Q.QueueID;
            ScriptID:=Q.ScriptID;
            Env:=Q.Env;
            Event:=Q.Event;
            StartID:=Q.StartID;
            Q.Current:=(Q.Current+1) mod Q.Count;
            if Q.Current=0 then
              FCurrent:=(FCurrent+1) mod FList.Count;
          end;
        end;
      until (newAddress<>nil) or (FList.Count<=0);
    end;
    ShowStatus;
  finally
    if FList.Count>0 then FEvent.SetEvent;
  end;
  finally
    Release;
  end;
  if DoSleep then Sleep(200);
end;

procedure TSIPQueueManager.Retry(Address:TSIPAddress;Delay,QueueID:Cardinal);
var
  Q:TSIPQueueItem;
  I: Integer;
begin
  Acquire;
  try
    Inc(Address.TryCount);
    for I := 0 to FList.Count - 1 do
    begin
      Q:=TSIPQueueItem(FList[I]);
      if Q.QueueID=QueueID then
      begin
        Q.AddressesProcessing.Remove(Address);
        Q.AddressesRetry.AddAddress(Address,Delay);
        Q.Event.UpdateWait(MaxAnnounce);
        Break;
      end;
    end;
  finally
    Release;
  end;
end;

procedure TSIPQueueManager.ShowStatus;
var
  I:Integer;
  S:String;
  Q:TSIPQueueItem;
  Cnt:Integer;
begin
    S:='';
    for I := 0 to FList.Count - 1 do
    begin
      Q:=TSIPQueueItem(FList[I]);
      if Q.Addresses=nil then
        Cnt:=1
      else
        Cnt:=Q.Addresses.Count;
      S:=S+IntToStr(Q.ScriptID)+'('+IntToStr(Cnt)+','+IntToStr(Q.AddressesRetry.Count)+') ';
    end;
    Status(S);
end;

function TSIPQueueManager.QueueStatus(ID:Integer): String;
var
  I: Integer;
  Item:TSIPQueueItem;
  Queues:String;
  IDList:TList;
  procedure Add(const S:String);
  begin
    if Result='' then Result:=S else
    if S<>'' then Result:=Result+','+S;
  end;
begin
  Acquire;
  try
    Result:='';
    Queues:='';
    IDList:=TList.Create;
    try
      for I := 0 to FList.Count - 1 do
      begin
        Item:=TSIPQueueItem(FList[I]);
        if (ID<=0) or (Item.StartID=ID) then
        begin
          Add(Item.Status(False));
        end;
        if IDList.IndexOf(Pointer(Item.StartID))<0 then
        begin
          IDList.Add(Pointer(Item.StartID));
          if Queues<>'' then Queues:=Queues+',';
          Queues:=Queues+Format('{queueid:%d,name:%s,time:%s,stopped:%d,count:%d}',[Item.StartID,StrEscape(Item.Event.Name),StrEscape(FormatDateTime('mm-dd hh:nn:ss',Item.Event.DateTime)),Ord(Item.Stop),Item.AddressCount^]);
        end;
      end;
      if IDList.Count>1 then
      begin
        if Queues<>'' then Queues:=','+Queues;
        Queues:='{queueid:0,name:''Всички'',time:null,stopped:0}'+Queues;
      end;
    finally
      IDList.Free;
    end;
    Result:=Format('"queues":[%s],"queuerows":[%s]',[Queues,Result]);
  finally
    Release;
  end;
end;

procedure TSIPQueueManager.StartMail(Address: TSIPAddress; QueueID: Cardinal);
var
  Q:TSIPQueueItem;
  I: Integer;
begin
  Acquire;
  try
    for I := 0 to FList.Count - 1 do
    begin
      Q:=TSIPQueueItem(FList[I]);
      if Q.QueueID=QueueID then
      begin
        Q.EMails.AddAddress(Address,0);
        Break;
      end;
    end;
  finally
    Release;
  end;
end;

procedure TSIPQueueManager.StartSMS(Address: TSIPAddress; QueueID: Cardinal);
var
  Q:TSIPQueueItem;
  I: Integer;
begin
  Acquire;
  try
    for I := 0 to FList.Count - 1 do
    begin
      Q:=TSIPQueueItem(FList[I]);
      if Q.QueueID=QueueID then
      begin
        Q.SMSes.AddAddress(Address,0);
        Break;
      end;
    end;
  finally
    Release;
  end;
end;

procedure TSIPQueueManager.Status(S: String);
begin
  //if TSIPApp(FSIPApp).SIPStatus<>nil then TSIPApp(FSIPApp).SIPStatus.Status(FID,S);
end;


procedure TSIPQueueManager.Stop(EventID: Integer);
var
  Q:TSIPQueueItem;
  I: Integer;
begin
  Acquire;
  try
    for I := FList.Count - 1 downto 0 do
    begin
      Q:=TSIPQueueItem(FList[I]);
      if Q.StartID=EventID then
      begin
        Q.Event.UpdateWait(MaxRing);
        Q.Stop:=True;
      end;
    end;
  finally
    Release;
  end;
end;

procedure TSIPQueueManager.Succeed(Address: TSIPAddress; QueueID: Cardinal;ProcessingKind:TProcessingKind);
var
  Q:TSIPQueueItem;
  I: Integer;
begin
  Acquire;
  try
    for I := 0 to FList.Count - 1 do
    begin
      Q:=TSIPQueueItem(FList[I]);
      if Q.QueueID=QueueID then
      begin
        case ProcessingKind of
          pkSIP: begin
            Q.AddressesProcessing.Remove(Address);
            Address.ResultSIP:=Address.SucceedMethod;
          end;
          pkSMS: begin
            Q.SMSes.Remove(Address);
            //Address.Status:='SMS';
            Address.ResultSMS:='succeed';
          end;
          pkMail: begin
            Q.EMails.Remove(Address);
            //Address.Status:='Email';
            Address.ResultMail:='succeed';
          end;
        end;
        Q.AddressesSuccess.AddAddress(Address,0);
        Q.AddressesFailure.Remove(Address);
        if ((Q.Addresses=nil) or (Q.Addresses.Count<=0)) and (Q.AddressesRetry.Count<=0) and (Q.AddressesProcessing.Count<=0) and (Q.EMails.Count<=0) and (Q.SMSes.Count<=0) then
           Q.Event.EndItem;
        Break;
      end;
    end;
  finally
    Release;
  end;
end;

{ TSIPQueueItem }

destructor TSIPQueueItem.Destroy;
begin
  if Event<>nil then
  begin
    Event.AddReport(FormatDateTime('yyyy-mm-dd hh:nn:ss ',Now)+'Край на оповестяване'#13#10,'');
    Event.AddReport(Status(True),'queue');
  end;
  FreeAndNil(Addresses);
  FreeAndNil(AddressesRetry);
  FreeAndNil(AddressesFailure);
  FreeAndNil(AddressesSuccess);
  FreeAndNil(AddressesProcessing);
  EMails.Clear;
  SMSes.Clear;
  FreeAndNil(EMails);
  FreeAndNil(SMSes);
  inherited;
end;

function TSIPQueueItem.Status(Journal:Boolean): String;
  procedure Add(const S:String);
  begin
    if Result='' then Result:=S else
    if S<>'' then Result:=Result+','+S;
  end;
begin
  Result:='';
  if Addresses=nil then Exit;
  Add(Addresses.Status('"queueid":'+IntToStr(StartID)+',"state":"чакащи",',IntToStr(ID)+'-1-',Journal,False));
  Add(AddressesRetry.Status('"queueid":'+IntToStr(StartID)+',"state":"за повторение",',IntToStr(ID)+'-2-',Journal,False));
  Add(AddressesSuccess.Status('"queueid":'+IntToStr(StartID),IntToStr(ID)+'-',Journal,True));
  Add(AddressesFailure.Status('"queueid":'+IntToStr(StartID)+',"state":"неуспешно оповестени",',IntToStr(ID)+'-3-',Journal,False));
  Add(AddressesProcessing.Status('"queueid":'+IntToStr(StartID)+',"state":"оповестяване",',IntToStr(ID)+'-4-',Journal,False));
  Add(EMails.Status('"queueid":'+IntToStr(StartID)+',"state":"изпращане на EMail",',IntToStr(ID+1)+'-5-',Journal,False));
  Add(SMSes.Status('"queueid":'+IntToStr(StartID)+',"state":"изпращане на SMS",',IntToStr(ID+2)+'-6-',Journal,False));
end;

end.
