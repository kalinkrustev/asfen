unit dm_GUI;

interface

uses
  SysUtils, Classes, DBXpress, WideStrings, ImgList, Controls, DB, DBClient,
  SqlExpr, Provider, ADODB, Forms, ActnMan, ActnColorMaps, Dialogs, rtcHttpCli,
  rtcCliModule, rtcFunction, rtcInfo, rtcConn;

type
  TdmGUI = class(TDataModule)
    Connection: TADOConnection;
    WorkgroupEdit: TADODataSet;
    RingGroupsEdit: TADODataSet;
    RingGroupMembersEdit: TADODataSet;
    WorkGroupList: TADODataSet;
    RightsList: TADODataSet;
    NomRightsList: TADODataSet;
    RightsEdit: TADODataSet;
    RecordingsList: TADODataSet;
    ScriptList: TADODataSet;
    RingGroupsList: TADODataSet;
    FilesEdit: TADODataSet;
    UserList: TADODataSet;
    UserEdit: TADODataSet;
    EventList: TADODataSet;
    OrganisationsList: TADODataSet;
    OrganisationsEdit: TADODataSet;
    City: TADODataSet;
    Position: TADODataSet;
    Raion: TADODataSet;
    CityListQ: TADODataSet;
    PersonList: TADODataSet;
    Kind: TADODataSet;
    TSB: TADODataSet;
    Altitude: TADODataSet;
    Region: TADODataSet;
    CityList: TClientDataSet;
    prvCityList: TDataSetProvider;
    FileList: TClientDataSet;
    prvFileList: TDataSetProvider;
    FileListQ: TADODataSet;
    OrgExport: TClientDataSet;
    prvOrg: TDataSetProvider;
    MaxID: TADODataSet;
    Crisis: TADODataSet;
    QPathSize: TADOQuery;
    OrganisationExp: TADODataSet;
    OrganisationImp: TADOQuery;
    OrgDelete: TADOQuery;
    procedure max(Sender: TObject);
    procedure SetLast(DataSet: TDataSet);
    procedure UsersChange(DataSet: TDataSet);
    procedure AbortChanges(DataSet: TDataSet);
    procedure PositionEditBeforeDelete(DataSet: TDataSet);
    procedure OrganisationsListCalcFields(DataSet: TDataSet);
    procedure OrganisationsEditCalcFields(DataSet: TDataSet);
    procedure PositionAfterOpen(DataSet: TDataSet);
    procedure RaionAfterOpen(DataSet: TDataSet);
    procedure PersonListAfterOpen(DataSet: TDataSet);
    procedure CityListAfterOpen(DataSet: TDataSet);
    procedure NomAfterOpen(DataSet: TDataSet);
    procedure FileListAfterOpen(DataSet: TDataSet);
    procedure OrganisationsBeforeEdit(DataSet: TDataSet);
    procedure OrganisationsChange(DataSet: TDataSet);
    procedure SettingsEdit(DataSet: TDataSet);
    procedure SettingsChange(DataSet: TDataSet);
    procedure UsersEdit(DataSet: TDataSet);
    procedure RingGroupsEditAfterPost(DataSet: TDataSet);
    procedure RingGroupsEditAfterDelete(DataSet: TDataSet);
    procedure OrganisationsEditAfterPost(DataSet: TDataSet);
    procedure OrganisationsEditAfterDelete(DataSet: TDataSet);
    procedure OrganisationsEditAfterOpen(DataSet: TDataSet);
  private
    CurrentUser: Integer;
    CurrentWorkGroup:Integer;
    CurrentDatabase: Integer;
    OrganisationChanged:Boolean;
    HTTP:TRtcHttpClient;
    HTTPModule:TRtcClientModule;
    HTTPResult:TRtcResult;
    OrganisationsCode,OrganisationsParent,
    OrganisationsEditCode,OrganisationsEditParent:TField;
    OrganisationsName,
    OrganisationsPosition,
    FileListFileName:TField;
    procedure HTTPReturn(Sender:TRtcConnection;
                                  Data:TRtcValue;
                                  Result:TRtcValue);
    procedure ReloadFiles;
    procedure CreateParent(D: TDataSet);
    procedure OrganisationsOblastGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure OrganisationsObstinaGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure CityOblastGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure CityObstinaGetText(Sender: TField; var Text: string;
      DisplayText: Boolean);
    procedure OrganisationsCity1Change(Sender: TField);
    procedure OrganisationsCity2Change(Sender: TField);
    procedure CityOblastSetText(Sender: TField; const Text: string);
    procedure CityObstinaSetText(Sender: TField; const Text: string);
    procedure ReadSchema;
    function  GetDBVersion(Group:String):Int64;
    function  GetExportVersion(Group:String):Int64;
    function  GetFileVersion(Group:String):Int64;
    procedure SetDBVersion(Group:String;V:Int64);
    procedure CheckVersion(Group:String);
    procedure OnFormLoad(Sender:TObject);
    procedure OrganisationsPositionChange(Sender: TField);
  public
    function GetRight(RightAbbr,RightID:String):Boolean;
    procedure FireEvent(EventID:Integer;Host:String);
    procedure Reload;
    function ShowForm(C:TComponentClass;Modal:Boolean;var F;Show:Boolean=True;Owner:TComponent=nil;RightName:String=''):Integer;
    procedure SaveFile(Path,FileName,Content:String;Overwrite:Boolean);
    function  LoadFile(Path,FileName:String;var FileID:Integer):String;overload;
    function  LoadFile(Path:String;FileID:Integer):String;overload;
    function  DeleteDBFile(Path,FileName:String):Boolean;
    function  PathSize(Path,ExcludeID:String):Int64;
    function  RenameFile(Path,FileName,NewFileName:String):Boolean;
    function  Confirm(Msg:String;DlgType:TMsgDlgType=mtConfirmation):Boolean;
    function  ConfirmDelete:Boolean;
    function  ConfirmExit(Msg:String='Желаете ли запис на промените?';DlgType:TMsgDlgType=mtConfirmation):Integer;
    procedure Login(UserName,Password:String;DatabaseID:Integer);
    function  DatabaseID:Integer;
    function  FileName(FileID:Integer):String;
    function  CanExport(Group:String):Boolean;
    procedure ChangeVersion(Group:String);
    function  CityName(ID:Double):String;
    function  CityID(EKATTE:String):Double;
    function  PersonName(ID:String):String;
    function  RaionName(ID:String):String;
    function ReloadOrganisation:Boolean;
    procedure ExportOrganisation(FileName:String);
    procedure ImportOrganisation(FileName:String;Replace:Boolean);
    procedure ExportOrganisationXLW(FileName:String);
    procedure ExportOrganisationXLS(FileName:String);
    procedure ImportOrganisationXLS(FileName:String;Replace:Boolean);
  end;

  Limits=class(TObject)
    const
      OrganisationCount=1000;
      GroupCount=30;
      ScriptCount=50;
      SoundCount=50;
      SoundTotalLength=20*60;
      SoundLength=5*60;
  end;

var
  dmGUI: TdmGUI;



implementation
uses Variants, dm_Images, Util, dm_AlarmDB, Windows, ActnList, GWGExporterXLSTXT, XLSFile;

{$R *.dfm}

procedure TdmGUI.ChangeVersion(Group: String);
var FV,ExpV,DBV:Int64;
begin
  FV:=GetFileVersion(Group);
  ExpV:=GetExportVersion(Group);
  DBV:=GetDBVersion(Group);
  if (FV<>DBV) or  not ((ExpV=FV) or (ExpV+1=FV)) then
    raise TMessageException.Create('Извършени са промени по другата база данни, трябва да'+
                             #13#10'импортирате данните, преди да правите промени по тях.');
  if ExpV=FV then
    SetDBVersion(Group,FV+1);
end;

procedure TdmGUI.CheckVersion(Group: String);
var FV,ExpV,DBV:Int64;
begin
  FV:=GetFileVersion(Group);
  ExpV:=GetExportVersion(Group);
  DBV:=GetDBVersion(Group);
  if (FV<>DBV) or  not ((ExpV=FV) or (ExpV+1=FV)) then
    raise TMessageException.Create('Извършени са промени по другата база данни, трябва да'+
                             #13#10'импортирате данните, преди да правите промени по тях.');
end;

function TdmGUI.CanExport(Group: String):Boolean;
var FV,DBV:Int64;
begin
  FV:=GetFileVersion(Group);
  DBV:=GetDBVersion(Group);
  Result:=(FV>0) and (FV=DBV);
end;

function TdmGUI.CityID(EKATTE: String): Double;
var V:Variant;
begin
  if Trim(EKATTE)='' then Result:=0 else
  begin
    V:=CityList.Lookup('EKATTE',EKATTE,'ID');
    if not VarIsNull(V) then
      Result:=V
    else
      Result:=0;
  end;
end;

procedure TdmGUI.CityListAfterOpen(DataSet: TDataSet);
begin
  CityList.FieldByName('Name').DisplayWidth:=20;
  CityList.FieldByName('EKATTE').DisplayWidth:=6;
  CityList.FieldByName('Kmetstvo').DisplayWidth:=6;
end;

function TdmGUI.CityName(ID: Double): String;
begin
  if ID<=0 then
    Result:=''
  else
    Result:=VarToStr(City.Lookup('ID',ID,'Name'));
end;

function TdmGUI.Confirm(Msg: String; DlgType: TMsgDlgType): Boolean;
begin
  Result:=MessageDlg(Msg,DlgType,[mbYes,mbNo],0)=mrYes;
end;

function TdmGUI.ConfirmDelete: Boolean;
begin
  Result := Confirm('Потвърждавате ли изтриването?');
end;

function TdmGUI.ConfirmExit;
begin
  Result:=MessageDlg(Msg,DlgType,[mbYes,mbNo,mbCancel],0)
end;

procedure CreateFields(FTable:TDataSet;aFieldDefs:TFieldDefs);
var I:Integer;
begin
  if FTable.FieldDefs<>aFieldDefs then FTable.FieldDefs.Assign(aFieldDefs);
  with FTable do
  if ObjectView then
  begin
    for I := 0 to FieldDefs.Count - 1 do
      with FieldDefs[I] do
        if (DataType <> ftUnknown) and not (faHiddenCol in Attributes) then
          CreateField(FTable);
  end else
  begin
    for I := 0 to FieldDefList.Count - 1 do
      with FieldDefList[I] do
        if (DataType <> ftUnknown) and not (faHiddenCol in Attributes)
           and not (DataType in ObjectFieldTypes) then
          CreateField(FTable, nil, FieldDefList.Strings[I]);
  end;
end;

procedure TdmGUI.CreateParent(D:TDataSet);
var F:TField;
begin
  D.FieldDefs.Update;
  CreateFields(D,D.FieldDefs);
  F:=TFloatField.Create(D);
  F.FieldKind:=fkCalculated;
  F.FieldName:='Parent';
  F.DataSet:=D;

  F:=TStringField.Create(D);
  F.FieldKind:=fkCalculated;
  F.Size:=50;
  F.FieldName:='Obstina1';
  F.Tag:=Integer(D.FieldByName('City1'));
  F.OnGetText:=OrganisationsObstinaGetText;
  F.DataSet:=D;

  F:=TStringField.Create(D);
  F.FieldKind:=fkCalculated;
  F.Size:=50;
  F.FieldName:='Oblast1';
  F.Tag:=Integer(D.FieldByName('City1'));
  F.OnGetText:=OrganisationsOblastGetText;
  F.DataSet:=D;

  F:=TStringField.Create(D);
  F.FieldKind:=fkCalculated;
  F.Size:=50;
  F.FieldName:='Obstina2';
  F.Tag:=Integer(D.FieldByName('City2'));
  F.OnGetText:=OrganisationsObstinaGetText;
  F.DataSet:=D;

  F:=TStringField.Create(D);
  F.FieldKind:=fkCalculated;
  F.Size:=50;
  F.FieldName:='Oblast2';
  F.Tag:=Integer(D.FieldByName('City2'));
  F.OnGetText:=OrganisationsOblastGetText;
  F.DataSet:=D;
end;

function TdmGUI.DatabaseID: Integer;
begin
  Result:=CurrentDatabase;
end;

procedure TdmGUI.max(Sender: TObject);
begin
  HTTP:=TRtcHttpClient.Create(Self);
  HTTP.AutoConnect:=True;
  HTTP.MultiThreaded:=False;
  HTTP.ReconnectOn.ConnectLost:=True;
  HTTP.ServerAddr:='localhost';
  HTTP.ServerPort:='8080';
  HTTPModule:=TRtcClientModule.Create(Self);
  HTTPModule.Client:=HTTP;
  HTTPModule.ModuleFileName:='/exec';
  HTTPResult:=TRtcResult.Create(Self);
  HTTPResult.OnReturn:=HTTPReturn;
end;

function TdmGUI.DeleteDBFile(Path, FileName: String): Boolean;
begin
  Path:=Trim(Path);
  FileName:=Trim(FileName);
  if FileName='' then raise TMessageException.Create('Името на файла не може да е празно');
  if Path='' then raise TMessageException.Create('Липсва тип на файла');
  if not FilesEdit.Active then FilesEdit.Open else FilesEdit.Requery;

  Result:=False;
  if not FilesEdit.Locate('FilePath;FileName',VarArrayOf([Path,FileName]),[loCaseInsensitive]) then
    raise TMessageException.Create('Файл с име '+Path+'\'+FileName+' не съществува')
  else
  begin
    if Confirm('Желаете ли изтриване на файл '+Path+'\'+FileName) then
    begin
      ChangeVersion('Settings');
      FilesEdit.Delete;
      ReloadFiles;
      Result:=True;
    end;
  end;
end;

procedure TdmGUI.ExportOrganisationXLW(FileName: String);
var X:TGWGExporterXLSTXT;
begin
  OrganisationExp.Close;
  OrganisationExp.Open;
  try
    X:=TGWGExporterXLSTXT.Create(nil);
    try
      X.DataSet:=OrganisationExp;
      X.FileToSave:=FileName;
      X.Save_As;
    finally
      X.Free;
    end;
  finally
    OrganisationExp.Close;
  end;
end;

procedure TdmGUI.ExportOrganisationXLS(FileName: String);
var X:TXLSfile;
   I,Row:Integer;
begin
  OrganisationExp.Close;
  OrganisationExp.Open;
  try
    Row:=1;
    X:=TXLSfile.Create(nil);
    X.FileName:=FileName;
    for I := 0 to OrganisationExp.FieldCount - 1 do
       X.AddStrCell(I,0,[],OrganisationExp.Fields[I].FieldName);
    try
      while not OrganisationExp.Eof do
      begin
        for I := 0 to OrganisationExp.FieldCount - 1 do
          X.AddStrCell(I,Row,[],OrganisationExp.Fields[I].Text);
        OrganisationExp.Next;
        Row:=Row+1;
      end;
      X.write;
    finally
      X.Free;
    end;
  finally
    OrganisationExp.Close;
  end;
end;


procedure TdmGUI.ExportOrganisation(FileName: String);
begin
  if OrganisationsList.Active then
    OrganisationsList.Requery()
  else
    OrganisationsList.Open;
  OrgExport.Close;
  OrgExport.Open;
  OrgExport.SaveToFile(FileName,dfXML);
end;

procedure TdmGUI.FileListAfterOpen(DataSet: TDataSet);
begin
  FileListFileName:=FileList.FieldByName('FileName');
end;

function TdmGUI.FileName(FileID: Integer): String;
begin
  if FileList.FindKey([FileID]) then
    Result:=FileListFileName.AsString
  else
    Result:=IntToStr(FileID)+' ???';
end;

procedure TdmGUI.FireEvent;
var F:TRtcFunctionInfo;
begin
  F:=HTTPModule.Data.NewFunction('FireEvent');
  F.asInteger['EventID']:=EventID;
  F.asInteger['DatabaseID']:=DatabaseID;
  HTTP.Disconnect;
  if Host='' then Host:='localhost';
  HTTP.ServerAddr:=Host;
  HTTPModule.Call(HTTPResult);
  HTTPModule.WaitForCompletion;
end;

function TdmGUI.GetDBVersion(Group: String): Int64;
begin
  Result:=StrToIntDef(dmAlarmDB.GetSetting('DB'+Group),0);
end;

function TdmGUI.GetExportVersion(Group: String): Int64;
begin
  Result:=StrToIntDef(dmAlarmDB.GetSetting('Exp'+Group),0);
end;

function TdmGUI.GetFileVersion(Group: String): Int64;
var FN:String;
  DB:String;
  V:TStrings;
begin
  DB:='asos'+IntToStr(DatabaseID);
  FN:=dmAlarmDB.BackupFile(DB);
  FN:=ChangeFileExt(FN,'.ver');
  if not FileExists(FN) then
    raise TMessageException.Create('За тази операция с база данни asos'+IntToStr(DatabaseID)+#13#10'трябва да включите съответната флаш памет.');
  V:=TStringList.Create;
  try
    V.LoadFromFile(FN);
    Result:=StrToIntDef(V.Values[Group],0);
  finally
    V.Free;
  end;
end;

function TdmGUI.GetRight(RightAbbr, RightID: String): Boolean;
begin
  if not RightsList.Active then RightsList.Open;
  if not NomRightsList.Active then NomRightsList.Open;
  Result:=(CurrentWorkGroup=1)
    or not NomRightsList.Locate('RightAbbr',RightAbbr,[loCaseInsensitive])
    or RightsList.Locate('RightAbbr;WorkGroup;RightID',VarArrayOf([RightAbbr,CurrentWorkGroup,RightID]),[loCaseInsensitive]);
end;

procedure TdmGUI.HTTPReturn(Sender: TRtcConnection; Data, Result: TRtcValue);
begin
end;

procedure TdmGUI.ImportOrganisation(FileName: String;Replace:Boolean);
var Cnt,I:Integer;
    Src,Dest:array of TField;
    Max,PosCnt:Double;
    ImpCode,ImpPosition:TField;

  function AddCode(Code,Add:Double):Double;
  var X1:Double;
      X2:Int64;
  begin
    if Add=0 then Result:=Code else
    begin
      X2:=Trunc(Code);
      X1:=Add;
      while X2>=100 do
      begin
        X2:=X2 div 100;
        X1:=X1*100;
      end;
      if X2+Add>=100 then
        Result:=0
      else
        Result:=Code+X1;
    end;
  end;

begin

  OrgExport.LoadFromFile(FileName);

  try
    ChangeVersion('Organisation');

    if Replace then
    begin
      OrgDelete.ExecSQL;
      OrganisationsEdit.Requery;
    end;

    MaxID.Close;
    MaxID.Open;
    try
      Max:=MaxID.FieldByName('Code').AsFloat;
      PosCnt:=MaxID.FieldByName('Cnt').AsFloat;
      if PosCnt>Limits.OrganisationCount then
        raise Exception.Create('Позволено е дефиниране на максимум '+IntToStr(Limits.OrganisationCount)+' абоната в системата');
    finally
      MaxID.Close;
    end;
    
    OrgExport.First;
    ImpCode:=OrgExport.FindField('Code');
    ImpPosition:=OrgExport.FindField('PositionName');

    if (ImpCode=nil) and (ImpPosition=nil) then
      raise Exception.Create('Във файла не са открити колони Code и PositionName, поне едната трябва да присъства!');

    while not OrgExport.Eof do
    begin
      if ((ImpCode<>nil) and (ImpCode.AsString<>'') and (ImpCode.AsFloat>1E12))
      or ((ImpPosition<>nil) and (ImpPosition.AsString<>'')) then
      begin
        PosCnt:=PosCnt+1;
        if PosCnt>Limits.OrganisationCount then
          raise Exception.Create('Позволено е дефиниране на максимум '+IntToStr(Limits.OrganisationCount)+' абоната в системата');
      end;
      OrgExport.Next;
    end;
    OrgExport.First;
    Cnt:=0;
    for I:=0 to orgexport.Fields.Count-1 do
    begin
      if not SameText(orgexport.Fields[I].FieldName,'LastUser') then
      if not SameText(orgexport.Fields[I].FieldName,'LastChange') then
      if OrganisationsEdit.FindField(orgexport.Fields[I].FieldName)<>nil then Cnt:=Cnt+1;
    end;
    if Cnt>0 then
    begin
      SetLength(Src,Cnt);
      SetLength(Dest,Cnt);
      Cnt:=0;
      for I:=0 to orgexport.Fields.Count-1 do
      if not SameText(orgexport.Fields[I].FieldName,'LastUser') then
      if not SameText(orgexport.Fields[I].FieldName,'LastChange') then
      if OrganisationsEdit.FindField(orgexport.Fields[I].FieldName)<>nil then
      begin
        Dest[Cnt]:=OrganisationsEdit.FieldByName(orgexport.Fields[I].FieldName);
        Src[Cnt]:=orgexport.Fields[I];
        Cnt:=Cnt+1;
      end;
      orgexport.First;
      OrganisationsEdit.DisableControls;
      try
        while not orgexport.Eof do
        begin
          OrganisationsEdit.Append;
          for I:=0 to Length(Src)-1 do
          if SameText(Src[I].FieldName,'code') then
          begin
            Dest[I].AsFloat:=AddCode(Src[i].AsFloat,Max);
            if Dest[I].AsFloat=0 then Break; //for
          end  else
          if not Src[I].IsNull then
            Dest[I].AsString:=Src[I].AsString;
          if OrganisationsEditCode.AsFloat=0 then
            OrganisationsEdit.Cancel
          else
            OrganisationsEdit.CheckBrowseMode;
          orgexport.Next;
        end;
      finally
        OrganisationsEdit.EnableControls;
      end;
    end;
  finally
    OrgExport.Close;
  end;
end;

procedure TdmGUI.ImportOrganisationXLS(FileName: String;Replace:Boolean);
var I:Integer;
    Src,Dest:array of TField;
    Max,PosCnt:Double;
    Counters:Array[1..7] of Integer;
    ImpCode,ImpPosition:TField;

  function AddCode(Code,Add:Double;Level:Integer):Double;
  var X1:Double;
      X2:Int64;
      J:Integer;
  begin
    if Level>0 then
    begin
      for J := Level+1 to 7 do
        Counters[J]:=0;
      Counters[Level]:=Counters[Level]+1;
      if Counters[1]<=0 then
        raise Exception.Create('Липсва име на организация!');
      Code:=0;
      for J := 1 to Level do
        Code:=Code*100+Counters[J];
    end else
    if Code>0 then
    begin
      X2:=Trunc(Code);
      J:=7;
      while (X2>=100) do
      begin
        Counters[J]:=X2 mod 100;
        J:=J-1;
        X2:=X2 div 100;
      end;
      Counters[1]:=X2 mod 100;
    end;

    if Add=0 then Result:=Code else
    begin
      X2:=Trunc(Code);
      X1:=Add;
      while X2>=100 do
      begin
        X2:=X2 div 100;
        X1:=X1*100;
      end;
      if X2+Add>=100 then
        Result:=0
      else
        Result:=Code+X1;
    end;
  end;

begin

  for I := 1 to 7 do
    Counters[I]:=0;

  OrganisationImp.Close;
  OrganisationImp.ConnectionString:='Provider=Microsoft.Jet.OLEDB.4.0;Data Source='+FileName+';Extended Properties="Excel 5.0; HDR=Yes; IMEX=1; MaxScanRows=0;"';
  OrganisationImp.Open;
  try
    ChangeVersion('Organisation');

    if Replace then
    begin
      OrgDelete.ExecSQL;
      OrganisationsEdit.Requery;
    end;

    MaxID.Close;
    MaxID.Open;
    try
      Max:=MaxID.FieldByName('Code').AsFloat;
      PosCnt:=MaxID.FieldByName('Cnt').AsFloat;
      if PosCnt>Limits.OrganisationCount then
        raise Exception.Create('Позволено е дефиниране на максимум '+IntToStr(Limits.OrganisationCount)+' абоната в системата');
    finally
      MaxID.Close;
    end;
    
    OrganisationImp.First;
    ImpCode:=OrganisationImp.FindField('Code');
    ImpPosition:=OrganisationImp.FindField('PositionName');

    if (ImpCode=nil) and (ImpPosition=nil) then
      raise Exception.Create('Във файла не са открити колони Code и PositionName, поне едната трябва да присъства!');

    while not OrganisationImp.Eof do
    begin
      if ((ImpCode<>nil) and (ImpCode.AsString<>'') and (ImpCode.AsFloat>1E12))
      or ((ImpPosition<>nil) and (ImpPosition.AsString<>'')) then
      begin
        PosCnt:=PosCnt+1;
        if PosCnt>Limits.OrganisationCount then
          raise Exception.Create('Позволено е дефиниране на максимум '+IntToStr(Limits.OrganisationCount)+' абоната в системата');
      end;
      OrganisationImp.Next;
    end;
    OrganisationImp.First;

    if OrganisationImp.Fields.Count>0 then
    begin
      SetLength(Src,OrganisationImp.Fields.Count);
      SetLength(Dest,OrganisationImp.Fields.Count);
      for I:=0 to OrganisationImp.Fields.Count-1 do
      if not SameText(OrganisationImp.Fields[I].FieldName,'LastUser') and
         not SameText(OrganisationImp.Fields[I].FieldName,'Sort') and
         not SameText(OrganisationImp.Fields[I].FieldName,'LastChange') then
      begin
        Dest[I]:=OrganisationsEdit.FindField(OrganisationImp.Fields[I].FieldName);
        Src[I]:=OrganisationImp.Fields[I];
      end else
      begin
        Dest[I]:=nil;
        Src[I]:=nil;
      end;
      OrganisationImp.First;
      OrganisationsEdit.DisableControls;

      try
        while not OrganisationImp.Eof do
        begin
          OrganisationsEdit.Append;
          for I:=0 to Length(Src)-1 do
          if (Src[I]<>nil) and (Src[I].AsString<>'') then
          begin
            if SameText(Src[I].FieldName,'code') then
              OrganisationsEditCode.AsFloat:=AddCode(Src[i].AsFloat,Max,0)
            else
            if SameText(Src[I].FieldName,'company') and (OrganisationsEditCode.AsFloat=0) then
            begin
              OrganisationsEditCode.AsFloat:=AddCode(0,Max,1);
              OrganisationsName.AsString:=Src[I].AsString;
            end
            else
            if SameText(Src[I].FieldName,'department1') and (OrganisationsEditCode.AsFloat=0) then
            begin
              OrganisationsEditCode.AsFloat:=AddCode(0,Max,2);
              OrganisationsName.AsString:=Src[I].AsString;
            end
            else
            if SameText(Src[I].FieldName,'department2') and (OrganisationsEditCode.AsFloat=0) then
            begin
              OrganisationsEditCode.AsFloat:=AddCode(0,Max,3);
              OrganisationsName.AsString:=Src[I].AsString;
            end
            else
            if SameText(Src[I].FieldName,'department3') and (OrganisationsEditCode.AsFloat=0) then
            begin
              OrganisationsEditCode.AsFloat:=AddCode(0,Max,4);
              OrganisationsName.AsString:=Src[I].AsString;
            end
            else
            if SameText(Src[I].FieldName,'department4') and (OrganisationsEditCode.AsFloat=0) then
            begin
              OrganisationsEditCode.AsFloat:=AddCode(0,Max,5);
              OrganisationsName.AsString:=Src[I].AsString;
            end
            else
            if SameText(Src[I].FieldName,'department5') and (OrganisationsEditCode.AsFloat=0) then
            begin
              OrganisationsEditCode.AsFloat:=AddCode(0,Max,6);
              OrganisationsName.AsString:=Src[I].AsString;
            end
            else
            if SameText(Src[I].FieldName,'positionname') and (OrganisationsEditCode.AsFloat=0) then
            begin
              OrganisationsEditCode.AsFloat:=AddCode(0,Max,7);
              OrganisationsName.AsString:=Src[I].AsString;
            end
            else
            if Dest[I]<>nil then
              Dest[I].AsString:=Src[I].AsString;
          end;
          if OrganisationsEditCode.AsFloat=0 then
            OrganisationsEdit.Cancel
          else
            OrganisationsEdit.CheckBrowseMode;
          OrganisationImp.Next;
        end;
      finally
        OrganisationsEdit.EnableControls;
      end;
    end;
  finally
    OrganisationImp.Close;
    OrganisationImp.ConnectionString:='';
  end;
end;

procedure TdmGUI.NomAfterOpen(DataSet: TDataSet);
begin
  DataSet.FieldByName('LastUser').Visible:=False;
  DataSet.FieldByName('LastChange').Visible:=False;
  if DataSet.FindField('NameHex')<>nil then
    DataSet.FieldByName('NameHex').Visible:=False;
end;

function TdmGUI.LoadFile(Path, FileName: String;var FileID:Integer): String;
begin
  Path:=Trim(Path);
  FileName:=Trim(FileName);
  if FileName='' then raise TMessageException.Create('Името на файла не може да е празно');
  if Path='' then raise TMessageException.Create('Липсва тип на файла');
  if not FilesEdit.Active then FilesEdit.Open else FilesEdit.Requery;

  if not FilesEdit.Locate('FilePath;FileName',VarArrayOf([Path,FileName]),[loCaseInsensitive]) then
  begin
    Result:='';
    FileID:=0;
  end
  else
  begin
    Result:=FilesEdit.FieldByName('FileData').AsString;
    FileID:=FilesEdit.FieldByName('FileID').AsInteger;
  end;
end;

function TdmGUI.LoadFile(Path: String; FileID: Integer): String;
begin
  Path:=Trim(Path);
  if Path='' then raise TMessageException.Create('Липсва тип на файла');
  if not FilesEdit.Active then FilesEdit.Open else FilesEdit.Requery;

  if not FilesEdit.Locate('FilePath;FileID',VarArrayOf([Path,FileID]),[loCaseInsensitive]) then
  begin
    Result:='';
  end
  else
  begin
    Result:=FilesEdit.FieldByName('FileData').AsString;
  end;
end;

procedure TdmGUI.Login(UserName, Password: String;DatabaseID:Integer);
begin
  if Password='' then raise TMessageException.Create('Въведете парола');
  
  Connection.Close;
  Connection.Open('Alarm','par4evi453');
  try
    Connection.Execute('USE asos'+IntToStr(DatabaseID));
  except
    on E:Exception do
    begin
      E.Message:='Недостъпна база данни';
      raise;
    end;
  end;
  UserList.Close;
  UserList.Open;
  if not UserList.Locate('UserName;Password',VarArrayOf([UserName,Password]),[loCaseInsensitive])
  or UserList.FieldByName('WorkGroup').IsNull then raise TMessageException.Create('Невалиден потребител / парола');
  CurrentWorkGroup:=UserList.FieldByName('WorkGroup').AsInteger;
  CurrentUser:=UserList.FieldByName('ID').AsInteger;
  CurrentDatabase:=DatabaseID;
  ReadSchema;
  WorkGroupList.Open;
end;

procedure TdmGUI.OrganisationsListCalcFields(DataSet: TDataSet);
var I:Int64;
begin
  I:=Trunc(OrganisationsCode.AsFloat/100);
  while (I>0) and ((I mod 100)=0) do I:=I div 100;
  OrganisationsParent.AsFloat:=I;
end;

procedure TdmGUI.OrganisationsChange(DataSet: TDataSet);
begin
  ChangeVersion('Organisation');
end;

procedure TdmGUI.OnFormLoad(Sender: TObject);
var F:TCustomForm;
  I:Integer;
begin
  F:=Sender as TCustomForm;
  if CurrentWorkGroup>1 then
    for I := 0 to F.componentcount - 1 do
      if (f.Components[I] is TAction) and
      TAction(f.Components[I]).Visible then
      TAction(f.Components[I]).Visible:=GetRight('Action.'+F.ClassName+'.'+f.Components[I].Name,'*');
end;

procedure TdmGUI.OrganisationsBeforeEdit(DataSet: TDataSet);
begin
  if not OrganisationsEdit.ControlsDisabled then
    CheckVersion('Organisation');
end;

procedure TdmGUI.OrganisationsEditAfterDelete(DataSet: TDataSet);
begin
  OrganisationChanged:=True;
end;

procedure TdmGUI.OrganisationsPositionChange(Sender: TField);
begin
  if (OrganisationsName.AsString='')
  or (AnsiCompareText(OrganisationsName.AsString,'нова длъжност')=0)
  or (AnsiCompareText(VarToStr(OrganisationsName.OldValue),'нова длъжност')=0)
  //or (Sender.DataSet.State = dsInsert)
  then
    OrganisationsName.AsString:=VarToStr(Position.Lookup('Code',OrganisationsPosition.Value,'PositionName'));
end;

procedure TdmGUI.OrganisationsEditAfterOpen(DataSet: TDataSet);
begin
  OrganisationsName:=OrganisationsEdit.FieldByName('Name');
  OrganisationsPosition:=OrganisationsEdit.FieldByName('Position');
  OrganisationsEditCode:=OrganisationsEdit.FieldByName('Code');
  OrganisationsPosition.OnChange:=OrganisationsPositionChange;
end;

procedure TdmGUI.OrganisationsEditAfterPost(DataSet: TDataSet);
begin
  OrganisationChanged:=True;
end;

procedure TdmGUI.OrganisationsEditCalcFields(DataSet: TDataSet);
var I:Int64;
begin
  I:=Trunc(OrganisationsEditCode.AsFloat/100);
  while (I>0) and ((I mod 100)=0) do I:=I div 100;
  OrganisationsEditParent.AsFloat:=I;
end;

function TdmGUI.PathSize(Path,ExcludeID: String): Int64;
begin
  QPathSize.Close;
  QPathSize.SQL.Text:='select sum(datalength(filedata)) as pathsize from tblfiles where filepath='''+Path+''' and fileid<>'+ExcludeID;
  QPathSize.Open;
  try
    Result:=Trunc(QPathSize.FieldByName('pathsize').AsFloat);
  finally
    QPathSize.Close;
  end;  
end;

procedure TdmGUI.PersonListAfterOpen(DataSet: TDataSet);
begin
  PersonList.FieldByName('Code').DisplayWidth:=15;
end;

function TdmGUI.PersonName(ID: String): String;
var Code:Double;
  V:Variant;
begin
  Code:=StrToFloatDef(ID,0);
  if Code<=0 then
    Result:=''
  else
  begin
    V:=PersonList.Lookup('Code',Code,'Name1;Name2;Name3');
    if VarIsArray(V) then
      Result:=VarToStr(V[0])+' '+VarToStr(V[1])+' '+VarToStr(V[2])
    else
      Result:='';
  end;
end;

procedure TdmGUI.PositionAfterOpen(DataSet: TDataSet);
begin
  Position.FieldByName('PositionName').DisplayWidth:=50;
  Position.FieldByName('Code').DisplayWidth:=8;
  Position.FieldByName('LastUser').Visible:=False;
  Position.FieldByName('LastChange').Visible:=False;
end;

procedure TdmGUI.PositionEditBeforeDelete(DataSet: TDataSet);
begin
  if not dmGUI.ConfirmDelete then Abort;
end;

procedure TdmGUI.RaionAfterOpen(DataSet: TDataSet);
begin
  Raion.FieldByName('NameHex').Visible:=False;
  Raion.FieldByName('Name').DisplayWidth:=20;
  DataSet.FieldByName('LastUser').Visible:=False;
  DataSet.FieldByName('LastChange').Visible:=False;
end;

function TdmGUI.RaionName(ID: String): String;
begin
  Result:=VarToStr(Raion.Lookup('raion',ID,'Name'));
end;

procedure TdmGUI.ReadSchema;
var F:TField;
begin
  if OrganisationsList.FieldDefs.Updated then Exit;

  CreateParent(OrganisationsList);
  OrganisationsCode:=OrganisationsList.FieldByName('Code');
  OrganisationsCode.DisplayWidth:=15;
  OrganisationsParent:=OrganisationsList.FieldByName('Parent');
  CreateParent(OrganisationsEdit);
  OrganisationsEditCode:=OrganisationsEdit.FieldByName('Code');
  OrganisationsEditParent:=OrganisationsEdit.FieldByName('Parent');
  OrganisationsEdit.FieldByName('City1').OnChange:=OrganisationsCity1Change;
  OrganisationsEdit.FieldByName('City2').OnChange:=OrganisationsCity2Change;

  City.FieldDefs.Update;
  CreateFields(City,City.FieldDefs);

  F:=TStringField.Create(City);
  F.FieldKind:=fkCalculated;
  F.Size:=50;
  F.FieldName:='Obstina';
  F.Tag:=Integer(City.FieldByName('Kmetstvo'));
  F.OnGetText:=CityObstinaGetText;
  F.OnSetText:=CityObstinaSetText;
  F.DataSet:=City;

  F:=TStringField.Create(City);
  F.FieldKind:=fkCalculated;
  F.Size:=50;
  F.FieldName:='Oblast';
  F.Tag:=Integer(City.FieldByName('Kmetstvo'));
  F.OnGetText:=CityOblastGetText;
  F.OnSetText:=CityOblastSetText;
  F.DataSet:=City;

end;

procedure TdmGUI.Reload;
begin
  HTTPModule.Data.NewFunction('Reload');
  HTTPModule.Call(HTTPResult);
  HTTPModule.WaitForCompletion;
end;

procedure TdmGUI.ReloadFiles;
begin
  if RecordingsList.Active then RecordingsList.Requery;
  if ScriptList.Active then ScriptList.Requery;
  if EventList.Active then EventList.Requery;
  FileList.Close;
  FileList.Open;
end;

function TdmGUI.ReloadOrganisation;
begin
  Result:=OrganisationChanged;
  if OrganisationChanged then
  begin
    if OrganisationsList.Active then OrganisationsList.Requery;
    OrganisationChanged:=False;
  end;
end;

function TdmGUI.RenameFile(Path, FileName, NewFileName: String): Boolean;
begin
  Path:=Trim(Path);
  FileName:=Trim(FileName);
  if FileName='' then raise TMessageException.Create('Името на файла не може да е празно');
  if Path='' then raise TMessageException.Create('Липсва тип на файла');
  if not FilesEdit.Active then FilesEdit.Open;

  if Length(NewFileName)>FilesEdit.FieldByName('FileName').Size then
    raise TMessageException.Create('Полето FileName не може да побере име на файл: '+FileName+'.'#13#10'Максималния размер за име на файл е:'+IntToStr(FilesEdit.FieldByName('FileName').Size));

  if FilesEdit.Locate('FilePath;FileName',VarArrayOf([Path,NewFileName]),[loCaseInsensitive]) then
    raise TMessageException.Create('Файл с име '+Path+'\'+NewFileName+' вече съществува');
  if not FilesEdit.Locate('FilePath;FileName',VarArrayOf([Path,FileName]),[loCaseInsensitive]) then
    raise TMessageException.Create('Файл с име '+Path+'\'+FileName+' не съществува')
  else
  begin
    ChangeVersion('Settings');
    FilesEdit.Edit;
    FilesEdit.FieldByName('FileName').AsString:=NewFileName;
    FilesEdit.Post;
    ReloadFiles;
    Result:=True;
  end;

end;

procedure TdmGUI.RingGroupsEditAfterDelete(DataSet: TDataSet);
begin
  if dmGUI.RingGroupsList.Active then dmGUI.RingGroupsList.Requery;
end;

procedure TdmGUI.RingGroupsEditAfterPost(DataSet: TDataSet);
begin
  if dmGUI.RingGroupsList.Active then dmGUI.RingGroupsList.Requery;
end;

procedure TdmGUI.SaveFile(Path, FileName, Content: String;Overwrite:Boolean);
var Appended:Boolean;
begin
  Path:=Trim(Path);
  FileName:=Trim(FileName);
  if FileName='' then raise TMessageException.Create('Името на файла не може да е празно');
  if Path='' then raise TMessageException.Create('Липсва тип на файла');
  if not FilesEdit.Active then FilesEdit.Open;

  if Length(Path)>FilesEdit.FieldByName('FilePath').Size then
    raise TMessageException.Create('Полето FilePath не може да побере данните: '+Path);
  if Length(FileName)>FilesEdit.FieldByName('FileName').Size then
    raise TMessageException.Create('Полето FileName не може да побере име на файл: '+FileName+'.'#13#10'Максималния размер за име на файл е:'+IntToStr(FilesEdit.FieldByName('FileName').Size));

  if not FilesEdit.Locate('FilePath;FileName',VarArrayOf([Path,FileName]),[loCaseInsensitive]) then
  begin
    ChangeVersion('Settings');
    FilesEdit.Append;
    FilesEdit.FieldByName('FilePath').AsString:=Path;
    FilesEdit.FieldByName('FileName').AsString:=FileName;
    Appended:=True;
  end
  else
  begin
    if not Overwrite then
       raise TMessageException.Create('Файл с име '+Path+'\'+FileName+' вече съществува');
    ChangeVersion('Settings');
    FilesEdit.Edit;
    Appended:=False;
  end;
  FilesEdit.FieldByName('FileData').AsString:=Content;
  FilesEdit.Post;

  if Appended then ReloadFiles;
end;

procedure TdmGUI.SetDBVersion(Group: String; V: Int64);
var BFN,FN:String;
  DB:String;
  S:TStrings;
begin
  DB:='asos'+IntToStr(DatabaseID);
  BFN:=dmAlarmDB.BackupFile(DB);
  FN:=ChangeFileExt(BFN,'.ver');
  if not FileExists(FN) then
    raise TMessageException.Create('За да правите промени по база данни asos'+IntToStr(DatabaseID)+#13#10'трябва да включите съответната флаш памет.');
  if FileExists(BFN) and not SysUtils.DeleteFile(BFN) then
    raise TMessageException.Create('Не може да бъде изтрит файл '+BFN);
  S:=TStringList.Create;
  try
    S.LoadFromFile(FN);
    S.Values[Group]:=IntToStr(V);
    dmAlarmDB.WSettings.Connection.BeginTrans;
    try
      dmAlarmDB.SetSetting('DB'+Group,IntToStr(V));
      S.SaveToFile(FN);
      dmAlarmDB.WSettings.Connection.CommitTrans;
    except
      dmAlarmDB.WSettings.Connection.RollbackTrans;
      raise;
    end;
  finally
    S.Free;
  end;
end;

procedure TdmGUI.SetLast(DataSet: TDataSet);
var F:TField;
begin
  F:=DataSet.FindField('LastUser');
  if F<>nil then F.AsInteger:=CurrentUser;
  F:=DataSet.FindField('LastChange');
  if F<>nil then F.AsDateTime:=now;
  if DataSet=OrganisationsEdit then
    ChangeVersion('Organisation') else
  if DataSet=City then
    ChangeVersion('Organisation') else
  if DataSet=Kind then
    ChangeVersion('Organisation') else
  if DataSet=TSB then
    ChangeVersion('Organisation') else
  if DataSet=Position then
    ChangeVersion('Organisation') else
  if DataSet=Altitude then
    ChangeVersion('Organisation') else
  if DataSet=Region then
    ChangeVersion('Organisation') else
  if DataSet=Raion then
    ChangeVersion('Organisation') else
  if DataSet=RingGroupsEdit then
    ChangeVersion('Settings') else
  if DataSet=RingGroupMembersEdit then
    ChangeVersion('Settings') else
  if DataSet=Crisis then
    ChangeVersion('Settings') else
  if DataSet=UserEdit then
    ChangeVersion('Users') else
  if DataSet=WorkgroupEdit then
    ChangeVersion('Users') else
  if DataSet=RightsEdit then
    ChangeVersion('Users');
end;

procedure TdmGUI.SettingsChange(DataSet: TDataSet);
begin
  ChangeVersion('Settings');
end;

procedure TdmGUI.SettingsEdit(DataSet: TDataSet);
begin
  CheckVersion('Settings');
end;

procedure TdmGUI.AbortChanges(DataSet: TDataSet);
begin
  raise TMessageException.Create('Таблицата '+DataSet.Name+' не е достъпна за редакция');
end;

function TdmGUI.ShowForm(C:TComponentClass;Modal:Boolean;var F;Show:Boolean=True;Owner:TComponent=nil;RightName:String=''):Integer;
begin
  if CurrentWorkGroup>1 then
  begin
    if RightName='' then RightName:='ShowForm.'+C.ClassName;
    if not GetRight(RightName,'*') then
      raise TMessageException.Create('Нямате право на достъп до избраната екранна форма !');
  end;
  Result:=dmImages.ShowForm(C,Modal,F,Show,Owner,OnFormLoad);
end;

procedure TdmGUI.UsersEdit(DataSet: TDataSet);
begin
  CheckVersion('Users');
end;

procedure TdmGUI.UsersChange(DataSet: TDataSet);
begin
  if (DataSet=WorkgroupEdit) and (WorkgroupEdit.FieldByName('ID').AsInteger=1) then
    raise TMessageException.Create('Не може да бъде изтрита потребителска група '+WorkgroupEdit.FieldByName('GroupName').AsString);
  if (DataSet=UserEdit) and (UserEdit.FieldByName('ID').AsInteger=1) then
    raise TMessageException.Create('Не може да бъде изтрит потребител '+UserEdit.FieldByName('UserName').AsString);
  ChangeVersion('Users');
end;

procedure TdmGUI.OrganisationsOblastGetText(Sender: TField;
  var Text: string; DisplayText: Boolean);
begin
  Text:=CityName(Trunc(CityID(TField(Sender.Tag).AsString)) div 1000000000);
end;

procedure TdmGUI.OrganisationsObstinaGetText(Sender: TField;
  var Text: string; DisplayText: Boolean);
begin
  Text:=CityName(Trunc(CityID(TField(Sender.Tag).AsString)) div 10000000);
end;

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

procedure TdmGUI.CityOblastGetText(Sender: TField;
  var Text: string; DisplayText: Boolean);
begin
  Text:=Copy(TField(Sender.Tag).AsString,1,3);
  Text:=CityName(StrToFloatDef(StrToID(Text),0));
end;

procedure TdmGUI.CityObstinaGetText(Sender: TField;
  var Text: string; DisplayText: Boolean);
begin
  Text:=Copy(TField(Sender.Tag).AsString,1,3);
  Text:=CityName(StrToFloatDef(StrToID(Text)+Copy(TField(Sender.Tag).AsString,4,2),0));
end;

procedure TdmGUI.CityOblastSetText(Sender: TField;
  const Text: string);
begin
  if Length(TField(Sender.Tag).AsString)<Length(Text) then TField(Sender.Tag).AsString:=Text;
end;

procedure TdmGUI.CityObstinaSetText(Sender: TField;
  const Text: string);
begin
  if Length(TField(Sender.Tag).AsString)<Length(Text) then TField(Sender.Tag).AsString:=Text;
end;

procedure TdmGUI.OrganisationsCity1Change(Sender: TField);
begin
  OrganisationsEdit.FieldByName('Oblast1').AsString:=OrganisationsEdit.FieldByName('Oblast1').Text;
  OrganisationsEdit.FieldByName('Obstina1').AsString:=OrganisationsEdit.FieldByName('Obstina1').Text;
end;

procedure TdmGUI.OrganisationsCity2Change(Sender: TField);
begin
  OrganisationsEdit.FieldByName('Oblast2').AsString:=OrganisationsEdit.FieldByName('Oblast2').Text;
  OrganisationsEdit.FieldByName('Obstina2').AsString:=OrganisationsEdit.FieldByName('Obstina2').Text;
end;


end.
