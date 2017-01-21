unit dm_AlarmDB;

interface

uses
  SysUtils, Classes, DB, ADODB, dm_FixedConnPool, ComCtrls, StdCtrls;

type
  TdmAlarmDB = class(TDataModule)
    RSettings: TADOQuery;
    WSettings: TADOCommand;
    WSettings1: TADOQuery;
    TransferExport: TADOStoredProc;
    Databases: TADOQuery;
    TransferImport: TADOStoredProc;
    ADOQuery: TADOQuery;
    ADOConnection: TADOConnection;
    SaveXLS: TADOQuery;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    Conn:IConnection;
    Pool1,Pool2:TFixedConnectionPool;
    Database:String;
    LastDrive:Integer;
    Mapping:TStringList;
    DBDir:String;
    procedure ImportFromXML(FN: String; C: IConnection);
  public
    ProgressBar:TProgressBar;
    ProgressText:TLabel;
    function GetSetting(Key:String):String;
    procedure SetSetting(Key:String;Value:String);
    function GetIni(Key: String): String;
    procedure SetIni(Key, Value: String);
    procedure SetConnection(C:IConnection);
    procedure ImportOrganisations(Sender:TObject);
    procedure ImportOrganisationsXls(Sender:TObject);
    procedure ExportToXLS;
    procedure ImportFromXLS(FN: String; Delete:Boolean=True);
    procedure ExportData(Organisation,Settings,Users,Stats:Boolean;Options:String);
    procedure ImportData;
    procedure Login(DatabaseID:Integer);
    function  DatabaseList:String;
    function  BackupFile(DBName:String):String;
    procedure RestoreDB(FileName:String);
  end;

const
  AlarmModes : array [0..3] of string = ('Тренировъчен режим','Дежурен режим','Преход военновременен режим','Военновременен режим');
  EventKinds : array [0..1,0..1] of string =
        (
         ('COM порт','"ComSignal=Номер датчик"'),
         ('Клавиатура','"KeyboardKeys=Клавишна комбинация"')
        );
  Settings : array [0..28,0..1] of string =
        (('SIPHost','SIP Централа хост'),
         ('SIPChannels','SIP брой канали'),
         ('StartAccount','SIP стартов account'),
         ('HTTPPort','HTTP port'),
         ('StartupTimer','Изчакване при стартиране като сървис (s)'),
         ('RingInterval','Таймер за изпълнение на стъпка (ms)'),
         ('RingTimeOut','Таймер за звънене (s)'),
         ('ScriptMinDelay','Минимално време за изчакване преди сценарий (ms)'),
         ('ScriptMaxDelay','Максимално време за изчакване преди сценарий (ms)'),
         ('RSComPort','Датчици COM port'),
         ('GSMComPort','GSM COM port'),
         ('GSMComBaud','GSM COM baud'),
         ('GSMComBits','GSM COM bits'),
         ('GSMComStop','GSM COM stop bits'),
         ('GSMComParity','GSM COM parity'),
         ('SMTPHost','SMTP хост'),
         ('SMTPPort','SMTP порт'),
         ('SMTPUser','SMTP потребителско име'),
         ('SMTPPass','SMTP парола'),
         ('SMTPFrom','SMTP подател'),
         ('SMTPSubject','EMail subject'),
         ('IdleLock','Интервал за заключване при неактивност(мин)'),
         ('DiagnosticEvent','Диагностично събитие ID'),
         ('SIPBusy','SIP събития за заето'),
         ('SIPBusyRoute','SIP събития за заети пътища'),
         ('SIPNoAnswer','SIP събития за неотговорил'),
         ('SIPNotFound','SIP събития за грешен номер'),
         ('SIPCodec','SIP Codec'),
         ('PacketTime','RTP Frame (ms)')
        );

var
  dmAlarmDB: TdmAlarmDB;

implementation
uses Variants, DBClient, Clipbrd, XMLClipboard, Forms, Windows, Util, Controls,
     Logger,DBLogDlg;

{$R *.dfm}

{ TdmAlarmDB }

function TdmAlarmDB.BackupFile;
var Drives:Cardinal;
  I,Drive: Integer;
  VolumeName:array[1..100] of char;
  V1,V2:Cardinal;
begin
  Result:='';
  if GetEnvironmentVariable('org.krustev.host')<>'' then
  begin
    Result:=ExtractFilePath(ParamStr(0))+DBName+'.bak';
    Exit;
  end;
  
  if Log<>nil then Log.Debug('Start getting logical drives');
  Drives:=GetLogicalDrives;
  for Drive := -1 to 31 do
  begin
    if Drive=-1 then
      I:=LastDrive
    else
      I:=Drive;
    if I<0 then Continue;
       
    if (1 shl I) and Drives<>0 then
    begin
       {if Log<>nil then Log.Debug('Getting drive type '+chr(ord('a')+i)+':\');
       if GetDriveType(PChar(chr(ord('a')+i)+':\'))=DRIVE_REMOVABLE then
       begin }
         if Log<>nil then Log.Debug('Getting volume info '+chr(ord('a')+i)+':\');
         if GetVolumeInformation(PChar(chr(ord('a')+i)+':\'),@VolumeName[1],Length(VolumeName),nil,V1,V2,nil,0) then
            if SameText(Copy(VolumeName,1,StrLen(@VolumeName[1])),DBName+'-flash') then
            begin
              Result:=chr(ord('a')+i)+':\'+DBName+'.bak';
              LastDrive:=i;
              break;
            end;
       {end;}
    end;
  end;
  if Log<>nil then  Log.Debug('End getting logical drives');
end;

function TdmAlarmDB.DatabaseList: String;
begin
  Databases.Close;
  Databases.Open;
  Result:='';
  while not Databases.Eof do
  begin
    Result:=Result+Databases.Fields[0].AsString+#13#10;
    Databases.Next;
  end;
end;

procedure TdmAlarmDB.DataModuleCreate(Sender: TObject);
begin
  Pool1:=TFixedConnectionPool.Create(1);
  Pool2:=TFixedConnectionPool.Create(1);
  SetConnection(Pool1.GetConnection);
  TransferImport.Connection:=nil;
  LastDrive:=-1;
  DBDir:=ExtractFilePath(ParamStr(0))+'Data\';
end;

procedure TdmAlarmDB.DataModuleDestroy(Sender: TObject);
begin
  Conn:=nil;
  FreeAndNil(Mapping);
end;

procedure TdmAlarmDB.ExportData(Organisation, Settings, Users, Stats: Boolean;
  Options:String);
var FN:String;
begin
  FN:=BackupFile(Database);
  if FN='' then raise TMessageException.Create('Не е намерена флаш памет за база данни '+database);

  TransferExport.Parameters.ParamByName('@ExportOrganisation').Value:=Organisation;
  TransferExport.Parameters.ParamByName('@ExportSettings').Value:=Settings;
  TransferExport.Parameters.ParamByName('@ExportUsers').Value:=Users;
  TransferExport.Parameters.ParamByName('@ExportStats').Value:=Stats;
  TransferExport.Parameters.ParamByName('@FileName').Value:=FN;
  TransferExport.Parameters.ParamByName('@Options').Value:=Options;

  if Organisation then SetSetting('ExpOrganisation',GetSetting('DBOrganisation'));
  if Settings then SetSetting('ExpSettings',GetSetting('DBSettings'));
  if Users then SetSetting('ExpUsers',GetSetting('DBUsers'));

  Screen.Cursor:=crHourglass;
  try
    Application.ProcessMessages;
    TransferExport.Close;
    try
      TransferExport.Open;
      if not TransferExport.IsEmpty
      and not SameText(TransferExport.Fields[0].AsString,'success') then
        raise TMessageException.Create(TransferExport.Fields[0].AsString);
    finally
      TransferExport.Close;
    end;
  finally
    Screen.Cursor:=crDefault;
    Application.ProcessMessages;
  end;
end;

procedure TdmAlarmDB.ExportToXLS;
begin
  SaveXLS.SQL.Text:='select raion,name into ["Excel 8.0;Database='+ExtractFilePath(ParamStr(0))+'xls\ek_raion_exp.xls'+'"]..[ek_raion] from tblraion';
  SaveXLS.ExecSQL;
end;

function TdmAlarmDB.GetSetting(Key: String): String;
begin
  RSettings.Close;
  RSettings.Parameters.ParamByName('key').Value:=Key;
  RSettings.Parameters.ParamByName('id').Value:=0;
  RSettings.Open;
  Result:=RSettings.FieldByName('SettingValue').AsString;
  RSettings.Close;
end;

function TdmAlarmDB.GetIni(Key: String): String;
var S:TStrings;
  FN:String;
begin
  FN:=ExtractFilePath(ParamStr(0))+'AlarmService.ini';
  S:=TStringList.Create;
  try
    if FileExists(FN) then S.LoadFromFile(FN);
    Result:=S.Values[Key];
  finally
    S.Free;
  end;
end;

procedure TdmAlarmDB.SetConnection(C: IConnection);
var I:Integer;
begin
  Conn:=C;
  for I := 0 to ComponentCount - 1 do
  if Components[I] is TCustomADODataSet  then
  begin
    TCustomADODataSet(Components[I]).Active:=False;
    TCustomADODataSet(Components[I]).Connection:=C.Connection;
  end else
  if Components[I] is TADOCommand  then
    TADOCommand(Components[I]).Connection:=C.Connection
  else
  if Components[I] is TADOStoredProc  then
    TADOStoredProc(Components[I]).Connection:=C.Connection;

  ADOQuery.Connection:=ADOConnection;
end;

procedure TdmAlarmDB.SetSetting(Key, Value: String);
begin
  WSettings.Parameters.ParamByName('name1').Value:=Key;
  WSettings.Parameters.ParamByName('name2').Value:=Key;
  WSettings.Parameters.ParamByName('name3').Value:=Key;
  WSettings.Parameters.ParamByName('value2').Value:=Value;
  WSettings.Parameters.ParamByName('value3').Value:=Value;
  WSettings.Parameters.ParamByName('id1').Value:=0;
  WSettings.Parameters.ParamByName('id2').Value:=0;
  WSettings.Parameters.ParamByName('id3').Value:=0;
  WSettings.Execute;
end;

procedure TdmAlarmDB.SetIni(Key, Value: String);
var S:TStrings;
  FN:String;
begin
  FN:=ExtractFilePath(ParamStr(0))+'AlarmService.ini';
  S:=TStringList.Create;
  try
    if FileExists(FN) then S.LoadFromFile(FN);
    if S.IndexOfName(Key)>=0 then
      S.Values[Key]:=Value
    else
    if Value<>'' then
    begin
      if S.Count=0 then S.Add('##########   SETTINGS   ############');
      S.Insert(1,Key+'='+Value);
    end;
    S.SaveToFile(FN);
  finally
    S.Free;
  end;
end;

procedure TdmAlarmDB.ImportData;
var FN:String;
    C:IConnection;
begin
  FN:=BackupFile(Database);
  if (FN='') then FN:='j:\'+Database+'.bak';

  if FN='' then raise TMessageException.Create('Не е намерена флаш памет за база данни '+database);
  if not FileExists(FN) then raise TMessageException.Create('Не са открити експортирани данни за база данни '+database);

  TransferImport.Parameters.ParamByName('@FileName').Value:=FN;

  Screen.Cursor:=crHourglass;
  try
    Application.ProcessMessages;
    TransferImport.Close;
    C:=Pool2.GetConnection('FILE NAME=import.udl',False);
    C.Connection.Close;
    C.Connection.Open('sa','710511par4evi453');
    C.Connection.Execute('USE '+Database);
    TransferImport.Connection:=C.Connection;
    try
    try
      try
        TransferImport.Open;
        if not TransferImport.IsEmpty
        and not SameText(TransferImport.Fields[0].AsString,'success') then
          raise TMessageException.Create(TransferImport.Fields[0].AsString);
      except
        TransferImport.Connection.Execute('if @@TRANCOUNT>0 rollback tran');
        raise;
      end;
    finally
       TransferImport.Close;
    end;
    finally
      TransferImport.Connection:=nil;
    end;
    C.Connection.Close;
    C:=nil;
  finally
    Screen.Cursor:=crDefault;
    Application.ProcessMessages;
  end;

end;


const XLSMap=
   'ek_atte.table=tblcity'#13#10+
   'ek_atte.source=ekatte;name;kmetstvo;kind;category;altitude;document;tsb'#13#10+
   'ek_atte.destination=ekatte;name;kmetstvo;kind;category;altitude;document;tsb'#13#10+
   'ek_kmet.table=tblcity'#13#10+
   'ek_kmet.source=kmetstvo;ekatte;name;category;document'#13#10+
   'ek_kmet.destination=kmetstvo;ekatte;name;category;document'#13#10+
   'ek_obst.table=tblcity'#13#10+
   'ek_obst.source=obstina;ekatte;name;category;document'#13#10+
   'ek_obst.destination=kmetstvo;ekatte;name;category;document'#13#10+
   'ek_obl.table=tblcity'#13#10+
   'ek_obl.source=oblast;ekatte;name;region;document'#13#10+
   'ek_obl.destination=kmetstvo;ekatte;name;region;document'#13#10+
   'ek_raion.table=tblraion'#13#10+
   'ek_raion.source=raion;name'#13#10+
   'ek_raion.destination=raion;name'#13#10+
   'ek_regio.table=tblregion'#13#10+
   'ek_regio.source=region;ekatte;name;document'#13#10+
   'ek_regio.destination=region;ekatte;name;document'#13#10+
   'ek_tsb.table=tbltsb'#13#10+
   'ek_tsb.source=tsb;name'#13#10+
   'ek_tsb.destination=tsb;name'#13#10+
   'nom_crisis.table=tblcrisis'#13#10+
   'nom_crisis.source=id;name'#13#10+
   'nom_crisis.destination=id;name'#13#10+
   'nom_dluj.table=tblposition'#13#10+
   'nom_dluj.source=code;positionname'#13#10+
   'nom_dluj.destination=code;positionname'#13#10+
   ''#13#10
   ;

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

procedure TdmAlarmDB.ImportFromXLS(FN: String; Delete:Boolean);
var T:TADOTable;
    SrcList,DestList:TList;
  I: Integer;
  Kmetstvo,ID,Ekatte:TField;
  S:String;
begin
  ADOQuery.Close;
  ADOConnection.Close;
  ADOConnection.ConnectionString:=
  'Provider=Microsoft.Jet.OLEDB.4.0;Data Source='+FN+';Extended Properties="Excel 8.0; HDR=Yes; IMEX=1; MaxScanRows=0;"';

  FN:=ChangeFileExt(ExtractFileName(FN),'');
  ADOConnection.Open;
  try
    ADOQuery.SQL.Text:='select * from ['+FN+'$]';
    ADOQuery.Open;

    try
      if Mapping=nil then
      begin
        Mapping:=TStringList.Create;
        Mapping.Text:=XLSMap;
      end;

      if Delete then Conn.Connection.Execute('delete from '+Mapping.Values[Fn+'.table']);

      T:=TADOTable.Create(nil);
      try

        T.Connection:=Conn.Connection;
        T.TableName:=Mapping.Values[FN+'.table'];
        T.Open;
        Kmetstvo:=T.FindField('Kmetstvo');
        Ekatte:=T.FindField('Ekatte');
        ID:=T.FindField('ID');
        SrcList:=TList.Create;
        DestList:=TList.Create;
        try
          ADOQuery.GetFieldList(SrcList,Mapping.Values[FN+'.source']);
          T.GetFieldList(DestList,Mapping.Values[FN+'.destination']);
          while not ADOQuery.Eof do
          begin
            T.Append;
            for I := 0 to DestList.Count - 1 do
              TField(DestList[I]).AsString:=TField(SrcList[I]).AsString;
            if (ID<>nil) and (Kmetstvo<>nil) and (Ekatte<>nil) then
            begin
              S:=StrToID(Copy(Kmetstvo.AsString,1,3));
              if Length(Kmetstvo.AsString)>3 then S:=S+Copy(Kmetstvo.AsString,4,2);
              if Length(Kmetstvo.AsString)>6 then S:=S+Copy(Kmetstvo.AsString,7,2)+Ekatte.AsString;
              if S<>'' then
              begin
                ID.AsString:=S;
                T.Post;
              end else
                T.Cancel;
            end else
              T.Post;
            ADOQuery.Next;
          end;
        finally
          SrcList.Free;
          DestList.Free;
        end;
      finally
        T.Free;
      end;
    finally
      ADOQuery.Close;
    end;
  finally
    ADOConnection.Close;
  end;
end;

procedure TdmAlarmDB.ImportFromXML(FN: String;C:IConnection);
var Q:TADOQuery;
    CDS:TClientDataSet;
    F:TField;
    I:Integer;
    SQL:String;
    TableName:String;
    T:TADOTable;
begin
  CDS:=TClientDataSet.Create(nil);
  try
    CDS.LoadFromFile(FN);
    DataSetToXML(CDS);
    TableName:='tbl'+ChangeFileExt(ExtractFileName(FN),'');
    SQL:='if exists(select * from sysobjects where name='''+TableName+''' and xtype in (''u'',''v'')) drop table '+TableName+';'#13#10+
         'create table '+TableName+'('#13#10;
    for I := 0 to CDS.FieldCount - 1 do
    begin
      F:=CDS.Fields[I];
      if SameText(F.FieldName,'LastChange')
      or SameText(F.FieldName,'LastUser') then Continue;

      if I>0 then SQL:=SQL+','#13#10;

      if F is TIntegerField then
        SQL:=SQL+F.FieldName+' int'
      else
      if F is TFloatField then
        SQL:=SQL+F.FieldName+' float'
      else
      if F is TDateTimeField then
        SQL:=SQL+F.FieldName+' datetime'
      else
      if F is TBlobField then
        SQL:=SQL+F.FieldName+' image'
      else
      if F is TStringField then
        SQL:=SQL+F.FieldName+' varchar('+IntToStr(F.Size)+')'
      else
        raise
          exception.create('Unknow field type '+F.ClassName);
      if I=0 then SQL:=SQL+' not null';
    end;
    SQL:=SQL+','#13#13'LastUser int';
    SQL:=SQL+','#13#13'LastChange datetime';
    SQL:=SQL+','#13#10'primary key('+CDS.Fields[0].FieldName+'))';
  finally
    CDS.Free;
  end;
  Q:=TADOQuery.Create(nil);
  try
    Q.SQL.Text:=SQL;
    Q.Connection:=C.Connection;
    Q.ExecSQL;
  finally
    Q.Free;
  end;
  T:=TADOTable.Create(nil);
  try
    T.Connection:=C.Connection;
    T.TableName:=TableName;
    T.Open;
    XMLToDataSet(T,False);
  finally
    T.Free;
  end;
end;

procedure TdmAlarmDB.ImportOrganisations;

procedure Status(I:Integer;Txt:String);
begin
  if ProgressBar<>nil then
    ProgressBar.Position:=I;
  if ProgressText<>nil then
  begin
    ProgressText.Caption:=Txt;
    ProgressText.Repaint;
  end;
end;

begin
  if ProgressBar<>nil then
  begin
    ProgressBar.Max:=8;
    ProgressBar.Position:=0;
    ProgressBar.Parent.Show;
    ProgressBar.Parent.BringToFront;
  end;
  try
    Status(1,'импорт на градове');
    ImportFromXML('City.xml',Conn);

    Status(2,'импорт на надморски височини');
    ImportFromXML('Altitude.xml',Conn);

    Status(3,'импорт на длъжности');
    ImportFromXML('Position.xml',Conn);

    Status(4,'импорт на видове нас. места');
    ImportFromXML('Kind.xml',Conn);

    Status(5,'импорт на райони');
    ImportFromXML('Raion.xml',Conn);

    Status(6,'импорт на региони');
    ImportFromXML('Region.xml',Conn);

    Status(7,'импорт на ТСБ');
    ImportFromXML('Tsb.xml',Conn);

    Status(8,'импорт на обекти');
    ImportFromXML('Organisation.xml',Conn);
  finally
    if ProgressBar<>nil then
      ProgressBar.Parent.Hide;
  end;
end;

procedure TdmAlarmDB.ImportOrganisationsXls(Sender: TObject);

procedure Status(I:Integer;Txt:String);
begin
  if ProgressBar<>nil then
    ProgressBar.Position:=I;
  if ProgressText<>nil then
  begin
    ProgressText.Caption:=Txt;
    ProgressText.Repaint;
  end;
end;

procedure CheckFile(FN:String);
begin
  if not FileExists(FN) then
  raise TMessageException.Create('Липсва файл '+FN);

end;

begin
  CheckFile(ExtractFilePath(ParamStr(0))+'xls\ek_obl.xls');
  CheckFile(ExtractFilePath(ParamStr(0))+'xls\ek_obst.xls');
  CheckFile(ExtractFilePath(ParamStr(0))+'xls\ek_atte.xls');
  CheckFile(ExtractFilePath(ParamStr(0))+'xls\ek_raion.xls');
  CheckFile(ExtractFilePath(ParamStr(0))+'xls\ek_regio.xls');
  CheckFile(ExtractFilePath(ParamStr(0))+'xls\ek_tsb.xls');

  if ProgressBar<>nil then
  begin
    ProgressBar.Max:=6;
    ProgressBar.Position:=0;
    ProgressBar.Parent.Show;
    ProgressBar.Parent.BringToFront;
  end;
  Conn.Connection.BeginTrans;
  try
  try
    Status(1,'импорт на области');
    ImportFromXLS(ExtractFilePath(ParamStr(0))+'xls\ek_obl.xls');

    Status(2,'импорт на общини');
    ImportFromXLS(ExtractFilePath(ParamStr(0))+'xls\ek_obst.xls',False);

    Status(3,'импорт на населени места');
    ImportFromXLS(ExtractFilePath(ParamStr(0))+'xls\Ek_atte.xls',False);

    Status(4,'импорт на райони');
    ImportFromXLS(ExtractFilePath(ParamStr(0))+'xls\Ek_raion.xls');

    Status(5,'импорт на региони');
    ImportFromXLS(ExtractFilePath(ParamStr(0))+'xls\Ek_regio.xls');

    Status(6,'импорт на ТСБ');
    ImportFromXLS(ExtractFilePath(ParamStr(0))+'xls\Ek_tsb.xls');

    Conn.Connection.Execute('update tblcity set region=(select a.region from (select region,kmetstvo from tblcity where id<1000000) a'#13#10+
                            'where a.kmetstvo = left(tblcity.kmetstvo,3)) where id>=1000000');

    Conn.Connection.CommitTrans;
  except
    Conn.Connection.RollbackTrans;
    raise;
  end;

  finally
    if ProgressBar<>nil then
      ProgressBar.Parent.Hide;
  end;
end;

procedure TdmAlarmDB.Login(DatabaseID: Integer);
begin
  Conn.Connection.Execute('USE asos'+IntToStr(DatabaseID));
  Database:='asos'+IntToStr(DatabaseID);
end;

procedure TdmAlarmDB.RestoreDB(FileName: String);
var C:IConnection;
begin

  if not FileExists(FileName) then raise TMessageException.Create('Не е открит архив '+FileName);

  Conn.Connection.Close;
  try
    Screen.Cursor:=crHourglass;
    try
      Application.ProcessMessages;
      C:=Pool2.GetConnection('FILE NAME=import.udl',False);
      C.Connection.Close;
      C.Connection.Open('sa','710511par4evi453');
      try
        C.Connection.Execute(Format('RESTORE DATABASE [%s] FROM  DISK = N''%s'' WITH MOVE N''alarm_D'' TO N''%s%s.Mdf'', MOVE N''alarm_L'' TO N''%s%s.LDF''',[
           Database,
           FileName,
           DBDir,
           Database,
           DBDir,
           Database
        ]));
        C.Connection.Execute('use '+Database);
        C.Connection.Execute('exec sp_change_users_login ''Update_One'', ''Alarm'', ''Alarm'' ,''par4evi453''');
      finally
        C.Connection.Close;
      end;
      C:=nil;
    finally
      Screen.Cursor:=crDefault;
      Application.ProcessMessages;
    end;
  finally
    Conn.Connection.Open('Alarm','par4evi453');
    Conn.Connection.Execute('USE '+Database);
  end;

end;

end.
