unit dm_Organisation;

interface

uses
  SysUtils, Classes, DB, DBClient, ADODB;

type
  TdmOrganisation = class(TDataModule)
    Cities: TClientDataSet;
    CitiesID: TFloatField;
    CitiesEKATTE: TStringField;
    CitiesName: TStringField;
    CitiesKmetstvo: TStringField;
    CitiesOblast: TStringField;
    CitiesObstina: TStringField;
    CitiesClone: TClientDataSet;
    Organisations: TClientDataSet;
    OrganisationsName: TStringField;
    OrganisationsLevel: TIntegerField;
    OrganisationsCode: TFloatField;
    OrganisationsParent: TFloatField;
    OrganisationsFullName: TStringField;
    OrganisationsPhone: TStringField;
    OrganisationsPhone2: TStringField;
    OrganisationsPhone3: TStringField;
    OrganisationsPosition: TStringField;
    Positions: TClientDataSet;
    PositionsCode1: TStringField;
    PositionsCode2: TStringField;
    PositionsCode: TStringField;
    PositionsPositionName: TStringField;
    PositionsClone: TClientDataSet;
    OrganisationsEMail: TStringField;
    OrganisationsNewCode: TFloatField;
    CitiesKind: TIntegerField;
    CitiesCategory: TIntegerField;
    CitiesAltitude: TIntegerField;
    CitiesDocument: TStringField;
    CitiesTsb: TStringField;
    CitiesRegion: TStringField;
    CitiesPostCode: TStringField;
    CitiesPhoneCode: TStringField;
    CitiesLongitude: TStringField;
    CitiesLatitude: TStringField;
    CitiesLastChange: TDateTimeField;
    OrganisationsDTMF: TStringField;
    OrganisationsName2: TStringField;
    OrganisationsName3: TStringField;
    OrganisationsSite: TStringField;
    Altitude: TClientDataSet;
    Altitudealtitude: TStringField;
    Altitudename: TStringField;
    Kind: TClientDataSet;
    Kindkind: TStringField;
    Kindname: TStringField;
    Kindfullname: TStringField;
    Region: TClientDataSet;
    Regionregion: TStringField;
    Regionekatte: TStringField;
    Regionname: TStringField;
    Regiondocument: TStringField;
    TSB: TClientDataSet;
    TSBtsb: TStringField;
    TSBname: TStringField;
    OrganisationsObstina1: TStringField;
    OrganisationsOblast1: TStringField;
    OrganisationsObstina2: TStringField;
    OrganisationsOblast2: TStringField;
    OrganisationsCity1: TStringField;
    OrganisationsCity2: TStringField;
    CitiesNameHex: TStringField;
    Raion: TClientDataSet;
    Raionraion: TStringField;
    Raionname: TStringField;
    OrganisationsRaion1: TStringField;
    OrganisationsRaion2: TStringField;
    RaionClone: TClientDataSet;
    Raionnamehex: TStringField;
    OrganisationsAddress1: TStringField;
    OrganisationsAddress2: TStringField;
    procedure CitiesAfterOpen(DataSet: TDataSet);
    procedure CitiesBeforePost(DataSet: TDataSet);
    procedure OrganisationsCalcFields(DataSet: TDataSet);
    procedure PositionsCode1SetText(Sender: TField; const Text: string);
    procedure PositionsCode2SetText(Sender: TField; const Text: string);
    procedure PositionsAfterOpen(DataSet: TDataSet);
    procedure CitiesCloneFilterRecord(DataSet: TDataSet; var Accept: Boolean);
    procedure DataModuleCreate(Sender: TObject);
    procedure CitiesOblastSetText(Sender: TField; const Text: string);
    procedure CitiesObstinaSetText(Sender: TField; const Text: string);
    procedure CitiesOblastGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure CitiesObstinaGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure OrganisationsOblast1GetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure OrganisationsObstina1GetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure OrganisationsObstina2GetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure OrganisationsOblast2GetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure OrganisationsCity1Change(Sender: TField);
    procedure OrganisationsCity2Change(Sender: TField);
    procedure RaionAfterOpen(DataSet: TDataSet);
    procedure RaionCloneBeforePost(DataSet: TDataSet);
    procedure OrganisationsPositionChange(Sender: TField);
  private
    CitiesCloneKmetstvo:TField;
    procedure OpenData(C:TClientDataSet);
    function CityName(ID:Double):String;
    function CityID(EKATTE:String):Double;
    function Hex(S: String): String;
  public
    { Public declarations }
  end;

var
  dmOrganisation: TdmOrganisation;

function StrToID(S:String):String;

implementation
uses StrUtils, Variants, Windows;

{$R *.dfm}

function StrToID(S:String):String;
var I:Integer;
begin
  Result:='';
  S:=UpperCase(S);
  for I := 1 to Length(S)  do
  if (Ord(S[I])<99) and (Ord(S[I])>0) then
    Result:=Result+Format('%2.2d',[Ord(S[I])])
  else
    Result:=Result+'32';
end;


procedure TdmOrganisation.CitiesAfterOpen(DataSet: TDataSet);
begin
  CitiesClone.Filtered:=False;
  CitiesClone.CloneCursor(Cities,False,True);
  CitiesClone.FieldByName('Name').DisplayWidth:=20;
  CitiesClone.FieldByName('EKATTE').DisplayWidth:=6;
  CitiesClone.FieldByName('Kmetstvo').DisplayWidth:=6;
  CitiesCloneKmetstvo:=CitiesClone.FieldByName('Kmetstvo');
  CitiesClone.IndexFieldNames:='NameHex';
  CitiesClone.First;
  CitiesClone.Filtered:=True;
end;

function TdmOrganisation.Hex(S:String):String;
begin
  if S<>'' then
  begin
    SetLength(Result,Length(S)*2);
    BinToHex(@S[1],@Result[1],Length(S));
  end
  else Result:='';
end;

procedure TdmOrganisation.CitiesBeforePost(DataSet: TDataSet);
var S:String;
begin
  S:=StrToID(Copy(CitiesKmetstvo.AsString,1,3));
  if Length(CitiesKmetstvo.AsString)>3 then S:=S+Copy(CitiesKmetstvo.AsString,4,2);
  if Length(CitiesKmetstvo.AsString)>6 then S:=S+Copy(CitiesKmetstvo.AsString,7,2)+CitiesEKATTE.AsString;
  CitiesID.AsString:=S;
  CitiesNameHex.AsString:=Hex(CitiesName.AsString);
end;

procedure TdmOrganisation.CitiesCloneFilterRecord(DataSet: TDataSet;
  var Accept: Boolean);
begin
  Accept:=Length(CitiesCloneKmetstvo.AsString)>5;
end;

procedure TdmOrganisation.CitiesOblastGetText(Sender: TField; var Text: string;
  DisplayText: Boolean);
begin
  Text:=Copy(CitiesKmetstvo.AsString,1,3);
  Text:=CityName(StrToFloatDef(StrToID(Text),0));
end;

procedure TdmOrganisation.CitiesOblastSetText(Sender: TField;
  const Text: string);
begin
  if Length(CitiesKmetstvo.AsString)<Length(Text) then CitiesKmetstvo.AsString:=Text;
end;

procedure TdmOrganisation.CitiesObstinaGetText(Sender: TField; var Text: string;
  DisplayText: Boolean);
begin
  Text:=Copy(CitiesKmetstvo.AsString,1,3);
  Text:=CityName(StrToFloatDef(StrToID(Text)+Copy(CitiesKmetstvo.AsString,4,2),0));
end;

procedure TdmOrganisation.CitiesObstinaSetText(Sender: TField;
  const Text: string);
begin
  if Length(CitiesKmetstvo.AsString)<Length(Text) then CitiesKmetstvo.AsString:=Text;
end;

function TdmOrganisation.CityID(EKATTE: String): Double;
var V:Variant;
begin
  if Trim(EKATTE)='' then Result:=0 else
  begin
    V:=CitiesClone.Lookup('EKATTE',EKATTE,'ID');
    if not VarIsNull(V) then
      Result:=V
    else
      Result:=0;
  end;
end;

function TdmOrganisation.CityName(ID: Double): String;
begin
  if ID<=0 then
    Result:='' 
  else
    Result:=VarToStr(Cities.Lookup('ID',ID,'Name'));
end;

procedure TdmOrganisation.DataModuleCreate(Sender: TObject);
begin
  OpenData(Altitude);
  OpenData(Kind);
  OpenData(Region);
  OpenData(TSB);
  OpenData(Cities);
  OpenData(Positions);
  OpenData(Raion);
  OpenData(Organisations);
end;

procedure TdmOrganisation.OpenData(C: TClientDataSet);
begin
  if not FileExists(C.FileName) then
    C.CreateDataSet
  else
    C.Open;
  C.LogChanges:=False;
  C.MergeChangeLog;
end;

procedure TdmOrganisation.OrganisationsCalcFields(DataSet: TDataSet);
var I:Int64;
begin
  I:=Trunc(OrganisationsCode.AsFloat/100);
  while (I>0) and ((I mod 100)=0) do I:=I div 100;
  OrganisationsParent.AsFloat:=I;
end;

procedure TdmOrganisation.OrganisationsCity1Change(Sender: TField);
begin
  OrganisationsOblast1.AsString:=OrganisationsOblast1.Text;
  OrganisationsObstina1.AsString:=OrganisationsObstina1.Text;
end;

procedure TdmOrganisation.OrganisationsCity2Change(Sender: TField);
begin
  OrganisationsOblast2.AsString:=OrganisationsOblast2.Text;
  OrganisationsObstina2.AsString:=OrganisationsObstina2.Text;
end;

procedure TdmOrganisation.OrganisationsOblast1GetText(Sender: TField;
  var Text: string; DisplayText: Boolean);
begin
  Text:=CityName(Trunc(CityID(OrganisationsCity1.AsString)) div 1000000000);
end;

procedure TdmOrganisation.OrganisationsOblast2GetText(Sender: TField;
  var Text: string; DisplayText: Boolean);
begin
  Text:=CityName(Trunc(CityID(OrganisationsCity2.AsString)) div 1000000000);
end;

procedure TdmOrganisation.OrganisationsObstina1GetText(Sender: TField;
  var Text: string; DisplayText: Boolean);
begin
  Text:=CityName(Trunc(CityID(OrganisationsCity1.AsString)) div 10000000);
end;

procedure TdmOrganisation.OrganisationsObstina2GetText(Sender: TField;
  var Text: string; DisplayText: Boolean);
begin
  Text:=CityName(Trunc(CityID(OrganisationsCity2.AsString)) div 10000000);
end;

procedure TdmOrganisation.OrganisationsPositionChange(Sender: TField);
begin
  if (OrganisationsName.AsString='')
  or (AnsiCompareText(OrganisationsName.AsString,'нова длъжност')=0) 
  or (AnsiCompareText(VarToStr(OrganisationsName.OldValue),'нова длъжност')=0) then
    OrganisationsName.AsString:=VarToStr(PositionsClone.Lookup('Code',OrganisationsPosition.Value,'PositionName'));

end;

procedure TdmOrganisation.PositionsAfterOpen(DataSet: TDataSet);
begin
  PositionsClone.CloneCursor(Positions,False,True);
  PositionsClone.FieldByName('PositionName').DisplayWidth:=50;
  PositionsClone.FieldByName('Code').DisplayWidth:=6;
  PositionsClone.IndexFieldNames:='Code';
  PositionsClone.First;
end;

procedure TdmOrganisation.PositionsCode1SetText(Sender: TField;
  const Text: string);
begin
  PositionsCode.AsString:=LeftStr(Text,4)+RightStr('0000'+PositionsCode.AsString,4);
end;

procedure TdmOrganisation.PositionsCode2SetText(Sender: TField;
  const Text: string);
begin
  PositionsCode.AsString:=LeftStr(PositionsCode.AsString+'0000',4)+LeftStr(Text,4);
end;

procedure TdmOrganisation.RaionAfterOpen(DataSet: TDataSet);
begin
  RaionClone.Filtered:=False;
  RaionClone.CloneCursor(Raion,False,True);
  RaionClone.IndexFieldNames:='NameHex';
  RaionClone.FieldByName('namehex').Visible:=False;
  RaionClone.FieldByName('name').DisplayWidth:=20;
end;

procedure TdmOrganisation.RaionCloneBeforePost(DataSet: TDataSet);
begin
  DataSet.FieldByName('namehex').AsString:=Hex(DataSet.FieldByName('name').AsString);
end;

end.
