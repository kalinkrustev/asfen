unit SIP_Env;

interface
uses SIP_Library,SIP_Sound,Classes,SIP_Event;

type
  ISIPEnv = interface
    function SIPLibrary:TSIPLibrary;
    function SIPSound:TSIPAnnouncementList;
    function SIPAddress:TStringList;
    function SIPDTMF:TStringList;
    function SIPMobile:TStringList;
    function SIPHome:TStringList;
    function SIPMail:TStringList;
    function SIPSettings:TStringList;
    function RingGroups:TStringList;
    function EventLibrary:TSIPEventLibrary;
    function FileNames:TStringList;
    function DatabaseID:Integer;
  end;

  TSIPEnv =class (TInterfacedObject,ISIPEnv)
  private
    FSIPLibrary:TSIPLibrary;
    FSIPSound:TSIPAnnouncementList;
    FSIPAddress:TStringList;
    FSIPMobile:TStringList;
    FSIPHome:TStringList;
    FSIPMail:TStringList;
    FSIPDTMF:TStringList;
    FRingGroups:TStringList;
    FSIPSettings:TStringList;
    FEventLibrary:TSIPEventLibrary;
    FFileNames:TStringList;
    FDatabaseID:Integer;
    FID:Integer;
  public
    constructor Create(DatabaseID:Integer;Log:Boolean);
    destructor  Destroy;override;
    function SIPLibrary:TSIPLibrary;
    function SIPSound:TSIPAnnouncementList;
    function SIPAddress:TStringList;
    function SIPMobile:TStringList;
    function SIPHome:TStringList;
    function SIPMail:TStringList;
    function SIPDTMF:TStringList;
    function SIPSettings:TStringList;
    function RingGroups:TStringList;
    function EventLibrary:TSIPEventLibrary;
    function FileNames:TStringList;
    function DatabaseID:Integer;
  end;

implementation
uses SysUtils, dm_Alarm, Windows, Util;
{ TSIPEnv }

var EventCounter:Integer=0;

constructor TSIPEnv.Create;
begin
  FID:=InterlockedIncrement(EventCounter);
  FDatabaseID:=DatabaseID;
  FSIPLibrary:=TSIPLibrary.Create;
  FSIPSound:=TSIPAnnouncementList.Create;
  FSIPAddress:=TStringList.Create;
  FSIPMobile:=TStringList.Create;
  FSIPHome:=TStringList.Create;
  FSIPMail:=TStringList.Create;
  FSIPDTMF:=TStringList.Create;
  FRingGroups:=TStringList.Create;
  FEventLibrary:=TSIPEventLibrary.Create;
  FSIPSettings:=TStringList.Create;
  FFileNames:=TStringList.Create;
end;

function TSIPEnv.DatabaseID: Integer;
begin
  Result:=FDatabaseID;
end;

destructor TSIPEnv.Destroy;
begin
  FreeAndNil(FFileNames);
  FreeAndNil(FSIPSettings);
  FreeAndNil(FEventLibrary);
  FreeAndNil(FSIPLibrary);
  FreeAndNil(FSIPSound);
  FreeAndNil(FSIPAddress);
  FreeAndNil(FSIPMobile);
  FreeAndNil(FSIPHome);
  FreeAndNil(FSIPMail);
  FreeAndNil(FSIPDTMF);
  FreeAndNil(FRingGroups);
  inherited;
end;

function TSIPEnv.EventLibrary: TSIPEventLibrary;
begin
  Result:=FEventLibrary;
end;

function TSIPEnv.FileNames: TStringList;
begin
  Result:=FFileNames;
end;

function TSIPEnv.RingGroups: TStringList;
begin
  Result:=FRingGroups;
end;

function TSIPEnv.SIPAddress: TStringList;
begin
  Result:=FSIPAddress;
end;

function TSIPEnv.SIPMobile: TStringList;
begin
  Result:=FSIPMobile;
end;

function TSIPEnv.SIPHome: TStringList;
begin
  Result:=FSIPHome;
end;

function TSIPEnv.SIPMail: TStringList;
begin
  Result:=FSIPMail;
end;

function TSIPEnv.SIPDTMF: TStringList;
begin
  Result:=FSIPDTMF;
end;

function TSIPEnv.SIPLibrary: TSIPLibrary;
begin
  Result:=FSIPLibrary;
end;

function TSIPEnv.SIPSettings: TStringList;
begin
  Result:=FSIPSettings;
end;

function TSIPEnv.SIPSound: TSIPAnnouncementList;
begin
  Result:=FSIPSound;
end;

end.
