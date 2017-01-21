unit ErrorLog;

interface
uses SysUtils, Logger, Windows;

procedure ReportException(E:Exception);

implementation
uses jclDebug, Classes, JclHookExcept, Util, SyncObjs;

var ReportEvent:TMutex;

procedure LogException(E:Exception;const Msg:String);
begin
  if ReportEvent.WaitFor(1000) <> wrSignaled then Exit;
  try
    if Log<>nil then Log.Error(Msg,E) else
    begin
      StringToFile(ExtractFilePath(ParamStr(0))+'asos-'+IntToStr(GetCurrentProcessId)+FormatDateTime(' yyyy-mm-dd hh-nn-ss zzz',now)+'.error.log',Msg);
      OutputDebugString(PChar(Msg));
      Sleep(1);
    end;
  finally
    ReportEvent.Release;
  end;
end;

procedure ReportException(E:Exception);
var Msg:String;
  S:TStrings;
begin
  Msg:='';
  try
    Msg:=E.Message+' ('+E.ClassName+')';
    if JclLastExceptStackList<>nil then
    begin
      S:=TStringList.Create;
      try
        JclLastExceptStackList.AddToStrings(S);
        Msg:=Msg+#13#10+S.Text;
      finally
        S.Free;
      end;
    end;
    LogException(E,Msg);
  except
    on EE:Exception do
    begin
      LogException(EE,EE.Message+#13#10'Cause:'+Msg);
    end;
  end;
end;

procedure Notifier(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
begin
  if Assigned(ExceptObj) and (not IsIgnoredException(ExceptObj.ClassType)) and
  (ExceptObj is Exception) then
    ReportException(Exception(ExceptObj));

end;

initialization
  ReportEvent:=TMutex.Create;
  JclStackTrackingOptions:=JclStackTrackingOptions+[stExceptFrame,stStack,stAllModules,stRawMode];
  JclStartExceptionTracking;
  JclAddExceptNotifier(Notifier,npNormal);
finalization
  JclStopExceptionTracking;
end.
