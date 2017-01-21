unit SIP_Library;

interface
uses Classes,SIP_Script;

type
  TSIPLibrary=class(TStringList)
  private
    function GetScript(ID:Integer): TSipScript;
    function GetScriptText(ID:Integer): String;
    procedure SetScriptText(ID:Integer; const Value: String);
    function GetScriptName(ID: Integer): String;
    procedure SetScriptName(ID: Integer; const Value: String);
    procedure AddScript(Script:TSipScript);
  public
    Name:String;
    procedure Clear;override;
    destructor Destroy;override;
    property Script[ID:Integer]:TSipScript read GetScript;
    property ScriptText[ID:Integer]:String read GetScriptText write SetScriptText;
    property ScriptName[ID:Integer]:String read GetScriptName write SetScriptName;
  end;

implementation
uses Util, SysUtils;

{ TSIPScript }

procedure TSIPLibrary.AddScript(Script: TSipScript);
begin
  AddObject(IntToStr(Script.ScriptID),Script);
end;

procedure TSIPLibrary.Clear;
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

destructor TSIPLibrary.Destroy;
begin
  Clear;
  inherited;
end;

function TSIPLibrary.GetScript(ID:Integer): TSipScript;
var I:Integer;
begin
  I:=IndexOf(IntToStr(ID));
  if I>=0 then
    Result:=Objects[I] as TSipScript
  else
    Result:=nil;
end;

function TSIPLibrary.GetScriptName(ID: Integer): String;
var S:TSIPScript;
begin
  S:=Script[ID];
  if S=nil then
    Result:=''
  else
    Result:=S.Description;
end;

function TSIPLibrary.GetScriptText(ID:Integer): String;
var S:TSIPScript;
begin
  S:=Script[ID];
  if S=nil then
    Result:=''
  else
    Result:=Script[ID].Text;
end;

procedure TSIPLibrary.SetScriptName(ID: Integer; const Value: String);
var S:TSIPScript;
begin
  if IndexOf(IntToStr(ID))<0 then S:=TSIPScript.Create
  else S:=Script[ID];
  S.Description:=Value;
  S.ScriptID:=ID;
  if IndexOf(IntToStr(ID))<0 then AddScript(S);
end;

procedure TSIPLibrary.SetScriptText(ID:Integer; const Value: String);
var S:TSIPScript;
begin
  if IndexOf(IntToStr(ID))<0 then S:=TSIPScript.Create
  else S:=Script[ID];
  S.Text:=Value;
  S.ScriptID:=ID;
  if IndexOf(IntToStr(ID))<0 then AddScript(S);
end;

end.
