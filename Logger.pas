unit Logger;

interface
uses Log4D, SysUtils;

var Log:TLogLogger=nil;

procedure AddLoggers(FN:String;ID:Integer);

implementation
uses Classes,Util;

procedure AddLoggers(FN:String;ID:Integer);
var S:TStringList;
begin
  S:=TStringList.Create;
  try
    S.Text:=StringReplace(FileToString(FN),'[id]',IntToStr(ID),[rfReplaceAll]);
    TLogPropertyConfigurator.Configure(S);
  finally
    S.Free;
  end;
end;

initialization
  ChDir(ExtractFilePath(ParamStr(0)));
  TLogPropertyConfigurator.Configure(ChangeFileExt(ParamStr(0),'.ini'));
  Log:=DefaultHierarchy.GetLogger('alarm');
  if Log<>nil then
  if Log.GetAppender('Fil1')=nil then
    Log:=nil;
  
  if Log<>nil then
  begin
    Log.Info('#####################################################');
    Log.Info('Starting '+ParamStr(0));
  end;  
finalization
  if Log<>nil then
  begin
    Log.Info('Stopping '+ParamStr(0));
    Log.Info('#####################################################');
  end;  
end.
