unit SIP_Sound;

interface
uses Classes;

type
  TSIPAnnouncement=class(TObject)
  private
    function GetBuffer: Pointer;
    function GetSize: Integer;
  published
  public
    Wave:String;
    Name:String;
    Text:String;
    property Size:Integer read GetSize;
    property Buffer:Pointer read GetBuffer;
  end;

  TSIPAnnouncementList=class(TStringList)
  private
    function GetBuffer(ID:Integer): TSIPAnnouncement;
    procedure SetBuffer(ID:Integer; const Value: TSIPAnnouncement);
  public
    procedure Clear;override;
    destructor Destroy;override;
    property Buffer[ID:Integer]:TSIPAnnouncement read GetBuffer write SetBuffer;
  end;

implementation
uses SysUtils;
{ TSIP_Sound }

procedure TSIPAnnouncementList.Clear;
var O:TObject;
    I:Integer;
begin
  for I := 0 to Count - 1 do
  begin
    O:=Objects[I];
    Objects[I]:=nil;
    FreeAndNil(O);
  end;
  inherited;
end;

destructor TSIPAnnouncementList.Destroy;
begin
  Clear;
  inherited;
end;


function TSIPAnnouncementList.GetBuffer(ID:Integer): TSIPAnnouncement;
var I:Integer;
begin
  I:=IndexOf(IntToStr(ID));
  if I>=0 then
    Result := Objects[I] as TSIPAnnouncement
  else
    Result := nil;
end;

procedure TSIPAnnouncementList.SetBuffer(ID:Integer; const Value: TSIPAnnouncement);
var I:Integer;
begin
  I:=IndexOf(IntToStr(ID));
  if I>=0 then
    Objects[I]:=Value
  else
    AddObject(IntToStr(ID),Value);
end;

{ TSIPSound }

function TSIPAnnouncement.GetBuffer: Pointer;
begin
  if Wave='' then Result:=nil
  else Result:=@Wave[1];
end;

function TSIPAnnouncement.GetSize: Integer;
begin
  Result:=Length(Wave);
end;

end.
