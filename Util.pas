unit Util;

interface
uses Classes, SysUtils,Controls,StdCtrls;

type TStringFunction=function(S:String):Boolean;

function ComponentToString(C:TComponent):String;
procedure StringToComponent(S:String;C:TComponent);
function FileToString(const FileName: AnsiString): AnsiString;
procedure StringToFile(const FileName, Contents: AnsiString);
function StrEscape(const S:String):String;
function GetContentType(S:String):String;
function DoubleQuote(S:String):String;
function MatchEMail(S:String):Boolean;
function MatchPhone(S:String):Boolean;
function MatchDTMF(S:String):Boolean;
function MatchPassword(S:String):Boolean;
procedure CheckValid(F:TStringFunction;C:TCustomEdit;Msg:String='Невалидна стойност');

procedure PopControlHint(C:TControl;H:String);
var FHint :THintWindow;

type
  TMessageException=class(Exception);

implementation
uses PerlRegEx, Windows, Forms;

function ComponentToString(C:TComponent):String;
var S:TStringstream;
    M:TMemoryStream;
begin
  S:=TStringStream.Create('');
  M:=TMemoryStream.Create;
  try
    M.WriteComponent(C);
    M.Position:=0;
    ObjectBinaryToText(M,S);
    Result:=S.DataString;
  finally
    M.Free;
    S.Free;
  end;
end;

procedure StringToComponent(S:String;C:TComponent);
var SS:TStringstream;
    M:TMemoryStream;
begin
  SS:=TStringStream.Create(S);
  M:=TMemoryStream.Create;
  try
    SS.Position:=0;
    ObjectTextToBinary(SS,M);
    M.Position:=0;
    M.ReadComponent(C);
  finally
    M.Free;
    SS.Free;
  end;
end;

function FileToString(const FileName: AnsiString): AnsiString;
var
  fs: TFileStream;
  len: Integer;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    len := fs.Size;
    SetLength(Result, len);
    if len > 0 then
      fs.ReadBuffer(Result[1], len);
  finally
    fs.Free;
  end;
end;

procedure StringToFile(const FileName, Contents: AnsiString);
var
  fs: TFileStream;
  len: Integer;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  try
    len := Length(Contents);
    if len > 0 then
      fs.WriteBuffer(Contents[1], Length(Contents));
  finally
    fs.Free;
  end;
end;

function StrEscape(const S:String):String;
begin
  Result:='"'+StringReplace(
              StringReplace(
              StringReplace(
              StringReplace(
              StringReplace(S,'\','\\',[rfReplaceAll]),
                              '/','\/',[rfReplaceAll]),
                              '"','\"',[rfReplaceAll]),
                              #13,'\r',[rfReplaceAll]),
                              #10,'\n',[rfReplaceAll])
             +'"';
end;

function GetContentType(S:String):String;
begin
  if SameText(S,'.html')
  or SameText(S,'.htm') then Result:='text/html' else
  if SameText(S,'.css')then Result:='text/css' else
  if SameText(S,'.js')then Result:='text/javascript' else
  if SameText(S,'.xml')then Result:='text/xml' else
  Result:='text/plain'
end;

function DoubleQuote(S:String):String;
begin
  Result:=StringReplace(S,'''','''''',[rfReplaceAll]);
end;

var MailRegEx:TPerlRegEx;

function MatchEMail(S:String):Boolean;
begin
  if MailRegEx=nil then
  begin
    MailRegEx:=TPerlRegEx.Create(nil);
    MailRegEx.Options:=MailRegEx.Options+[preCaseLess];
    MailRegEx.RegEx:='^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,5}$';
  end;
  MailRegEx.Subject:=S;
  Result:=MailRegEx.Match;
end;

procedure PopControlHint;
var R,RR:TRect;
begin
  if FHint=nil then
  begin
    FHint:=HintWindowClass.Create(Application);
  end;
  FHint.ReleaseHandle;
  if C=nil then
  begin
    R.Top:=Screen.Height div 2-10;
    R.Left:=Screen.Width div 2-30;
    R.Bottom:=Screen.Height div 2+10;
    R.Right:=Screen.Width div 2+30;
  end else
  begin
    R:=C.ClientRect;
    R.TopLeft:=C.ClientToScreen(R.TopLeft);
    R.BottomRight:=C.ClientToScreen(R.BottomRight);
  end;
  FHint.BoundsRect:=R;
  if R.Right-R.Left>200 then
    RR:=FHint.CalcHintRect(R.Right-R.Left,H,nil)
  else
    RR:=FHint.CalcHintRect(200,H,nil);
  FHint.Width:=RR.Right;
  FHint.Height:=RR.Bottom;
  FHint.Top:=R.Top-FHint.Height-3;
  if FHint.Top<0 then FHint.Top:=R.Bottom+1;
  FHint.ActivateHint(FHint.BoundsRect,H);
  FHint.Refresh;
end;

function MatchPhone(S:String):Boolean;
var I:Integer;
begin
  Result:=False;
  
  for I := 1 to Length(S) do
    if not(S[I] in ['0'..'9']) then
      Exit;

  Result:=True;
end;

function MatchDTMF(S:String):Boolean;
var I:Integer;
begin
  Result:=False;

  if Length(S)<3 then Exit;
  
  for I := 1 to Length(S) do
    if not(S[I] in ['0'..'9']) then
      Exit;

  for I := 2 to Length(S) do
    if S[I]=S[I-1] then
      Exit;

  Result:=True;
end;

procedure CheckValid;
begin
  if not F(C.Text) then
  begin
    if C.Showing and C.CanFocus then C.SetFocus;
    PopControlHint(C,Msg);
    Abort;
  end;
end;

function MatchPassword(S:String):Boolean;
begin
  Result:=Length(S)>5;
end;

end.
