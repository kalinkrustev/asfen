{
  @html(<b>)
  Log File Creation
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  This unit gives you thread-safe Log writing support.
}
unit rtcLog;

{$INCLUDE rtcDefs.inc}

interface

uses
  rtcTrashcan,
  Windows, SysUtils,

  {$IFDEF IDE_1}
  FileCtrl,
  {$ENDIF}

  rtcSyncObjs;

var
  { Write Logged exception into the Log file?
    Dafault=True. By changing this to False will remove any
    Connection component exceptions from the Log file. }
  LOG_EXCEPTIONS:boolean=True;

  { If you want old log files to be deleted after several days,
    you can specify how long (in days) files should be kept.
    If this variable is 0 (default), log files will NOT be deleted. }
  RTC_LOGS_LIVE_DAYS:integer=0;

  { Sub-Folder inside AppFileName's directory where all LOG files will be stored.
    If you want LOG files to be created in the same folder as AppFile (EXE/DLL),
    set LOG_FOLDER to an empty string before calling "StartLog".
    For this value to have any effect, you need to set it before calling "StartLog". }
  LOG_FOLDER:string='LOG';

  { Full path to the LOG folder. If you leave this variable empty (default),
    it will be initialized automatically by using the AppFileName and LOG_FOLDER
    variables, immediately before the first log entry needs to be written.
    If you want your LOG files written to a specific folder by using full path,
    you can do it by setting this variable before the first Log entry is written.
    RTC_LOG_FOLDER should ALWAYS end with '\' }
  RTC_LOG_FOLDER:string='';

{ Write exception with a short string description into the Global App Log file.
  This procedure will have no effect if Log writer not started
  (by calling StartLog) or LOG_EXCEPTIONS is @false }
procedure Log(const s:string; E:Exception; const name:string=''); overload;

{ Write message into the Global App Log file.
  This procedure will have no effect if Log writer not started. }
procedure Log(const s:string; const name:string=''); overload;

{ Write message into the Log file for the current date.
  This procedure will have no effect if Log writer not started. }
procedure XLog(const s:string; const name:string=''); overload;

{ Before Log() procedures will have any effect,
  you have to call this procedure to start the Log writer.
  Without it, no Log file. }
procedure StartLog;

{ To stop Log file creation, simply call this procedure.
  To continue log writing, call StartLog. }
procedure StopLog;

implementation

uses
  rtcInfo;

var
  ThrCS:TRtcCritSec;
  doLog:boolean=False;

procedure StartLog;
  begin
  ThrCS.Enter;
  try
    doLog:=True;
  finally
    ThrCS.Leave;
    end;
  end;

procedure StopLog;
  begin
  ThrCS.Enter;
  try
    doLog:=False;
  finally
    ThrCS.Leave;
    end;
  end;

procedure WriteToLog(const ext:string; const text:string);

  procedure Delete_old_logs;
    var
      vdate      :TDatetime;
      sr         :TSearchRec;
      intFileAge :LongInt;
      myfileage  :TDatetime;
    begin
    try
      vdate:= Now - RTC_LOGS_LIVE_DAYS;
      if FindFirst(RTC_LOG_FOLDER + '*.log', faAnyFile - faDirectory, sr) = 0 then
        repeat
          intFileAge := FileAge(RTC_LOG_FOLDER + sr.name);
          if intFileAge > -1 then
            begin
            myfileage:= FileDateToDateTime(intFileAge);
            if myfileage < vdate then
              DeleteFile(pchar(RTC_LOG_FOLDER + sr.name));
            end;
          until (FindNext(sr) <> 0);
    finally
      FindClose(sr);
      end;
    end;

  procedure File_Append(const fname:string; const Data:string);
    var
      f:integer;
    begin
    f:=FileOpen(fname,fmOpenReadWrite+fmShareDenyNone);
    if f<0 then
      begin
      try
        if RTC_LOGS_LIVE_DAYS > 0 then
          Delete_old_logs;
      except
        // ignore problems with file deletion
        end;
      f:=FileCreate(fname);
      end;
    if f>=0 then
      try
        if FileSeek(f,0,2)>=0 then
          FileWrite(f,data[1],length(data));
      finally
        FileClose(f);
        end;
    end;

  begin
  if RTC_LOG_FOLDER='' then
    begin
    RTC_LOG_FOLDER:=ExtractFilePath(AppFileName);
    if Copy(RTC_LOG_FOLDER,length(RTC_LOG_FOLDER),1)<>'\' then
      RTC_LOG_FOLDER:=RTC_LOG_FOLDER+'\';

    if LOG_FOLDER<>'' then
      begin
      RTC_LOG_FOLDER:=RTC_LOG_FOLDER+LOG_FOLDER;
      if Copy(RTC_LOG_FOLDER,length(RTC_LOG_FOLDER),1)<>'\' then
        RTC_LOG_FOLDER:=RTC_LOG_FOLDER+'\';

      if not DirectoryExists(RTC_LOG_FOLDER) then
        if not CreateDir(RTC_LOG_FOLDER) then
          begin
          RTC_LOG_FOLDER:=GetTempDirectory;
          if Copy(RTC_LOG_FOLDER,length(RTC_LOG_FOLDER),1)<>'\' then
            RTC_LOG_FOLDER:=RTC_LOG_FOLDER+'\';

          RTC_LOG_FOLDER:=RTC_LOG_FOLDER+LOG_FOLDER;
          if not DirectoryExists(RTC_LOG_FOLDER) then
            CreateDir(RTC_LOG_FOLDER);
            
          if Copy(RTC_LOG_FOLDER,length(RTC_LOG_FOLDER),1)<>'\' then
            RTC_LOG_FOLDER:=RTC_LOG_FOLDER+'\';
          end;
      end;
    end;

  File_Append(RTC_LOG_FOLDER+ExtractFileName(AppFileName)+'.'+ext, text);
  end;

procedure XLog(const s:string; const name:string='');
  var
    d:TDateTime;
    fname,
    s2:string;
  begin
  ThrCS.Enter;
  try
    if not doLog then Exit; // Exit here !!!!

    try
      d:=Now;
      s2:=FormatDateTime('yyyy-mm-dd hh:nn:ss; ',d);

      if name<>'' then
        fname:=FormatDateTime('yyyy_mm_dd',d)+'.'+name+'.log'
      else
        fname:=FormatDateTime('yyyy_mm_dd',d)+'.log';

      WriteToLog(fname,s2+s+#13#10);
    except
      end;

  finally
    ThrCS.Leave;
    end;
  end;

procedure Log(const s:string; const name:string='');
  var
    d:TDateTime;
    fname,
    s2:string;
  begin
  ThrCS.Enter;
  try
    if not doLog then Exit; // Exit here !!!!

    try
      d:=Now;
      s2:=FormatDateTime('yyyy-mm-dd hh:nn:ss; ',d);
      if name<>'' then
        fname:=name+'.log'
      else
        fname:='log';

      WriteToLog(fname,s2+s+#13#10);
    except
      end;

  finally
    ThrCS.Leave;
    end;
  end;

procedure Log(const s:string; E:Exception; const name:string='');
  begin
  if LOG_EXCEPTIONS then
    Log(s+' Exception! '+E.ClassName+': '+E.Message,name);
  end;

initialization
ThrCS:=TRtcCritSec.Create;
finalization
Garbage(ThrCS);
end.
