unit GWGExporterXLSTXT;

interface

uses
  Windows, Messages, SysUtils, Classes, DB,
  DBCTrls, DBGrids, Dialogs , clipbrd, comobj,
  Variants, Forms;
var
    //Thanx to Tomasz Wasiuk
    CXlsEof: array[0..1] of Word = ($0A, 00);
    CXlsLabel: array[0..5] of Word = ($204, 0, 0, 0, 0, 0);
    CXlsNumber: array[0..4] of Word = ($203, 14, 0, 0, 0);
    CXlsBoolerr: array[0..5] of Word = ($205, 8,0, 0, 0, 0);
    CXlsRk: array[0..4] of Word = ($27E, 10, 0, 0, 0);

type

  TProgress =
  procedure(Sender : TObject; var Progress : Integer ; SendedRows : Integer ; m_time : TdateTime ) of object;
  TGWGExporterXLSTXT = class(TComponent)
  private
    MyWatch : Boolean;
    MyFirst : Boolean;
    FDataLink: TDataLink;
    FDataSet: TDataSet;
    FDBGrid: TDBGrid;
    FExport_Format : Integer;
    fRows_To_Send : Integer;
    fFirst_Rows_To_Send : Integer;
    fLast_Rows_To_Send : Integer;
    fFileToSave : String;
    fExt_File : String;

    fIniFile : String;
    fSaveFormat : String;
    FonProgress : Tprogress;
    SaveDialog1 : TSaveDialog;

    my_time : Tdatetime;
    procedure setOnProgress(const Value : TProgress );


    { Private declarations }
  protected
    function  GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure SetDataSet(const Value: TDataSet);
    procedure SetDBGrid(Value: TDBGrid);
    procedure SetRowsToSend(Value: Integer);
    procedure setFirstRowsToSend(Value: Integer);
    procedure setLastRowsToSend(Value: Integer);
    procedure SetFileToSave(strsavefile : String);
    procedure setFExport_Format(Value : Integer);

//to xls write...
    procedure XlsBeginStream(const filename : String);
    procedure XlsWriteCellLabel(XlsStream: TStream; const ACol, ARow: Word;
      const AValue: string);
    procedure XlsEndStream(XlsStream: TStream);
    procedure XlsWriteCellNumber(XlsStream: TStream; const ACol, ARow: Word;
      const AValue: Double);
    procedure XlsWriteCellRk(XlsStream: TStream; const ACol, ARow: Word;
      const AValue: Integer);
    procedure XlsWriteCellBoolerr(XlsStream: TStream; const ACol, ARow: Word;
      const AValue: Boolean);
//to OPENOFFICE_TOOLS...
    function convertToURL(winAddr: String): String;
    function dummyArray: Variant;
    { Protected declarations }
  public
    constructor create(Aowner : Tcomponent) ; override;
    destructor Destroy; override;
    procedure SAVE_AS;
    procedure setDefaultProperty;
    { DONE : dodaj destructor ... }

    { Public declarations }
  published

    property DataSet: TDataSet
      read  FDataSet
      write SetDataSet;
    property DataSource: TDataSource
      read GetDataSource
      write SetDataSource;
    property DBGrid: TDBGrid
      read FDBGrid
      write SetDBGrid;
    property RowsToSend : Integer
      read fRows_to_Send
      write setRowsToSend
      default 65535;
    property FirstRowsToSend : Integer
      read fFirst_Rows_to_Send
      write setFirstRowsToSend
      default 1;
    property LastRowsToSend : Integer
      read fLast_Rows_to_Send
      write setLastRowsToSend
      default 65535;
    property OnProgress : TProgress
      read FonProgress
      write SetonProgress ;
    property EXportFormat : Integer
      read  FExport_Format
      write setFExport_Format
      default 1 ;
    property FileToSave : String
      read  fFileToSave
      write SetFileToSave;
    property IniFile : String
      read  fIniFile
      write fIniFile;


    property SaveFormat : String
      read  fSaveFormat;

    { Published declarations }
  end;

 {$R biff4.res}
implementation
//Thanx to Bernard Marcell
const USASCIIexcl =   // US-ASCII characters to be replaced by % hexa
  '%%25 %20<%3C>%3E#%23"%22{%7B}%7D|%7C\%5C^%5E[%5B]%5D`%60';
const UTF8chars =        // non US-ASCII characters to be replaced by % hexa % hexa
  'À%D0%90Á%D0%91Â%D0%92Ã%D0%93Ä%D0%94Å%D0%95Æ%D0%96Ç%D0%97È%D0%98É%D0%99Ê%D0%9AË%D0%9BÌ%D0%9CÍ%D0%9DÎ%D0%9EÏ%D0%9F' +
  'Ð%D0%A0Ñ%D0%A1Ò%D0%A2Ó%D0%A3Ô%D0%A4Õ%D0%A5Ö%D0%A6×%D0%A7Ø%D0%A8Ù%D0%A9Ú%D0%AAÛ%D0%ABÜ%D0%ACÝ%D0%ADÞ%D0%AEß%D0%AF' +
  'à%D1%B0á%D1%B1â%D1%B2ã%D1%B3ä%D1%B4å%D1%B5æ%D1%B6ç%D1%B7è%D1%B8é%D1%B9ê%D1%BAë%D1%BBì%D1%BCí%D1%BDî%D1%BEï%D1%BF' +
  'ð%D1%80ñ%D1%81ò%D1%82ó%D1%83ô%D1%84õ%D1%85ö%D1%86÷%D1%87ø%D1%88ù%D1%89ú%D1%8Aû%D1%8Bü%D1%8Cý%D1%8Dþ%D1%8Eÿ%D1%8F' +
  'Œ%D0%8Aœ%D1%9A';

const URLprefix : Array [1..7] of String =
    ('file:', 'ftp:', 'news:', 'http:', 'mailto:', 'macro:', 'private:');

procedure TGWGExporterXLSTXT.SetFileToSave(strsavefile : String);
begin
  if not (csDesigning in ComponentState) then
  begin
    fFileToSave :=strsavefile;
  end;

  if MyFirst = True then
  begin
    MyFirst:=False;
    exit;
  end;
  if MyWatch = False then exit;
  if csDesigning in ComponentState then
  begin
    Savedialog1.Title:= 'Save as...';
    if fExt_File = '.XLS' then
    begin
      SaveDialog1.Filter:='MsExcel Files (*.xls)|*.xls';
      SaveDialog1.DefaultExt:='xls';
      Savedialog1.FileName:= 'Export.xls';
    end;

    if fExt_File = '.TXT' then
    begin
      SaveDialog1.Filter:='Text Files (*.txt)|*.txt';
      SaveDialog1.DefaultExt:='txt';
      Savedialog1.FileName:= 'Export.txt';
    end;

    if fExt_File = '.CSV' then
    begin
      SaveDialog1.Filter:='CSV Files (*.csv)|*.csv';
      SaveDialog1.DefaultExt:='csv';
      Savedialog1.FileName:= 'Export.csv';
    end;

    if SaveDialog1.Execute then
    begin
      MyWatch := False;
      fFileToSave := SaveDialog1.FileName;
      MyWatch := True;
    end ;
  end;



end;

procedure TGWGExporterXLSTXT.setFExport_Format(Value : Integer);
begin
  if (value < 1) or (value > 3) then value := 1;

  FExport_Format := Value;

  if Value = 1 then
  begin
    fSaveFormat := 'Excel';
    fExt_File :=   '.XLS';
  end
  else
  if Value = 2 then
  begin
    fSaveFormat := 'TXT TAB delimited';
    fExt_File :=   '.TXT';
  end
  else
  if Value = 3 then
  begin
    fSaveFormat := 'CSV';
    fExt_File :=   '.CSV';
  end;
  if (fFileToSave <> '') and (UpperCase(ExtractFileExt(fFileToSave))<>fExt_File) then
  fFileToSave := fFileToSave+fExt_File;
end;

procedure TGWGExporterXLSTXT.setOnProgress(const Value : TProgress );
begin
  FonProgress := Value;
end;

procedure TGWGExporterXLSTXT.setFirstRowsToSend(Value: Integer);
begin
  if Value <= 0 Then Value := 1;
  if VALUE > fLast_Rows_To_Send THEN fLast_Rows_To_Send := Value;
  fFirst_Rows_To_Send := Value;
  SetRowsToSend(fLast_Rows_To_Send - Value + 1);

end;

procedure TGWGExporterXLSTXT.setLastRowsToSend(Value: Integer);
begin
  if Value <= 0 Then Value := 1;
  if VALUE < fFirst_Rows_To_Send THEN fFirst_Rows_To_Send := Value;
  fLast_Rows_To_Send := Value;
  SetRowsToSend(Value + 1 - fFirst_Rows_To_Send);
end;


procedure TGWGExporterXLSTXT.SetRowsToSend(Value: Integer);
begin
  if Value <= 0 Then Value := 1;
  if (VALUE > 65535) and (fExt_File = '.XLS') THEN Value := 65535;
  fRows_To_Send := Value;
  fLast_Rows_To_Send := Value + fFirst_Rows_To_Send-1;
end;

constructor TGWGExporterXLSTXT.create(Aowner : Tcomponent) ;
begin
  inherited create(Aowner);
  MyFirst := True;
  FDataLink := TDataLink.Create;
  MyWatch := False;
  Savedialog1 := TSaveDialog.Create(self);
  setDefaultProperty;
end;

destructor TGWGExporterXLSTXT.Destroy ;
begin
  FDataLink.Free;
  Savedialog1.free;
  inherited Destroy;
end;

procedure TGWGExporterXLSTXT.SetDBGrid(Value: TDBGrid);
begin
  FDBGrid:=Value;
  FDataSet := nil;
  FDataLink.DataSource:=nil;
  if value<>nil then
  Value.FreeNotification( Self );
end;


procedure TGWGExporterXLSTXT.SetDataSet(const Value: TDataSet);
begin
  FDataSet := Value;
  FDBGrid := nil;
  FDataLink.DataSource := nil;
  if Value <> nil then
  Value.FreeNotification(Self);
end;

procedure TGWGExporterXLSTXT.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  FDataSet := nil;
  FDBGrid := nil;
  if Value <> nil then
  Value.FreeNotification( Self );
end;


function TGWGExporterXLSTXT.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TGWGExporterXLSTXT.SAVE_AS;
var

  C1,C2 : TDateTime;
  i, i2 : Integer;
  i3 : integer;

  il_fields : Smallint;
  WH_UNKN : Smallint;


  isunkn : Boolean;
  tnext : boolean;
  allowexp : string;
  allowexp2 : string;
  
  myFile : string;

  licznik : integer;

  tab_pola : array [0..256] of integer;
  nazw_pola : TStringList;
  nazw_pola2 : TStringList;
  FDS : TDataSet;
  Progress : Integer;
  Fname : String;
  FStream : TFileStream;
  CellLabel : string;

  dane_pole   : Array[1..256] of string;
  dane_oper   : Array[1..256] of string;
  dane_type   : Array[1..256] of string;
  dane_cons   : Array[1..256] of string;

  del : char;
  plik     : textfile;
  s : string;
begin


  // Begin Settings
  tnext := false;
  c1:=now;
  del := chr(9);

  allowexp:='';
  isunkn := False; // field ftUnknown

  if (fIniFile='') or (not FileExists(fIniFile))then fIniFile:='';

  FDS := nil;

  If FDataSet <> nil then
  begin
    FDS := FDataSet;
  end;

  If FDataLink.DataSource <> nil then
  begin
    if FDataLink.DataSource.DataSet <> nil then
    begin
      FDS := FDataLink.DataSource.DataSet;
    end;
  end;
  If FDBGrid  <> nil then
  begin
    if FDBGrid.DataSource <> nil then
    begin
      if FDBGrid.DataSource.DataSet <> nil then
      begin
        FDS := FDBGrid.DataSource.DataSet;
      end;
    end;
  end;

  if (FDS = nil) then
  begin
    ShowMessage('Data is not assigned...');
    exit;
  end;
  if (FDS <> nil) and (not FDS.Active) then
  begin
    ShowMessage('Data is not opened...');
    exit;
  end;
//******************************************
// incorrect settings
  if FirstRowsToSend > FDS.RecordCount then
  setFirstRowsToSend(1);

  if fds.RecordCount < RowsToSend then
  SetRowsToSend(fds.RecordCount);

  if LastRowsToSend > FDS.RecordCount then
  setLastRowsToSend(FDS.RecordCount);

//******************************************

// default settings table of import
  For i := 1 to FDS.FieldCount+1 do
  begin
    allowexp:= allowexp+'N';
  end;

  il_fields := FDS.FieldCount; 

  for i := 0 to il_fields-1 do
  begin
    if FDS.Fields[i].DataType =  ftUnknown then
    begin
     isunkn := True; // IS ftUnkonown
     WH_UNKN := i;   //Nr field ftUnknown
    end;
  end;

  if isunkn = False then dec(il_fields); 

  nazw_pola := TStringList.Create;

  For i := 0 to FDS.FieldCount-1 do
  nazw_pola.Add(FDS.Fields[i].FieldName);

  // Tables of import to field types
  For i := 0 to Il_fields do
  begin
    if FDS.Fields[i].DataType =  ftString   then allowexp[i+1] := 'S'; //format to string
    if FDS.Fields[i].DataType =  ftSmallint   then allowexp[i+1] := 'i'; // Integer
    if FDS.Fields[i].DataType =  ftInteger   then allowexp[i+1] := 'i'; //...
    if FDS.Fields[i].DataType =  ftWord   then allowexp[i+1] := 'i';
    if FDS.Fields[i].DataType =  ftBoolean   then allowexp[i+1] := 'b';
    if FDS.Fields[i].DataType =  ftFloat   then allowexp[i+1] := 'S';//'f';
    if FDS.Fields[i].DataType =  ftCurrency   then allowexp[i+1] := 'T';
    if FDS.Fields[i].DataType =  ftBCD   then allowexp[i+1] := 'T';
    if FDS.Fields[i].DataType =  ftDate   then allowexp[i+1] := 'T';
    if FDS.Fields[i].DataType =  ftTime   then allowexp[i+1] := 'T';
    if FDS.Fields[i].DataType =  ftDateTime   then allowexp[i+1] := 'T';
    if FDS.Fields[i].DataType =  ftAutoInc   then allowexp[i+1] := 'i';
    if FDS.Fields[i].DataType =  ftMemo   then
    begin
      allowexp[i+1] := 'S';
      if fExt_File <> '.XLS' then allowexp[i+1] := 'N';
    end;
    if FDS.Fields[i].DataType =  ftFmtMemo   then
    begin
     allowexp[i+1] := 'T';
     if fExt_File <> '.XLS' then allowexp[i+1] := 'N';
    end;
    if FDS.Fields[i].DataType =  ftWideString   then allowexp[i+1] := 'S';
    if FDS.Fields[i].DataType =  ftLargeint   then allowexp[i+1] := 'T';
    if FDS.Fields[i].DataType =  ftGuid   then allowexp[i+1] := 'S';
    if FDS.Fields[i].DataType =  ftFMTBcd   then allowexp[i+1] := 'T';
    if FDS.Fields[i].DataType =  ftUnknown    then allowexp[i+1] := 'U';
  end;

  //delete unvisible field ftUnknown
  if isunkn then
  begin
    system.Delete(allowexp, WH_UNKN+1,1);
  end;
  for i := 0 to length(allowexp) do tab_pola[i]:=i;

//******************************* 
  If (FDBGrid  <> nil) and (fIniFile = '') then
  begin
    nazw_pola2 := TStringList.Create;

    i3 := FDBGrid.Columns.Count;

    for i := 0 to i3-1 do
    begin
      nazw_pola2.Add(FDBGrid.Columns[i].fieldname);
    end;

    for i :=  0 to nazw_pola2.Count-1 do
    begin
      for i2 := 0 to nazw_pola.Count-1 do
      begin
        if nazw_pola2[i] = nazw_pola[i2] then
        begin
          allowexp2:= allowexp2+allowexp[i2+1];
          tab_pola[i] := i2;
          break;
        end;
      end;
    end;
    allowexp:=allowexp2;
    nazw_pola := nazw_pola2;

  end;
//***************************************
// repaired incorrect settings of filename to save
  if fFileToSave = '' then SetFileToSave(ExtractFilePath(Application.ExeName)+'EXPORT_PRB'+fExt_File);
  //if (fFileToSave <> '') and (UpperCase(ExtractFileExt(fFileToSave))<>fExt_File) then
  //fFileToSave := fFileToSave+fExt_File;
//***************************************
  //delimiter text
  if fExt_File = '.CSV' then del := ';';
  if fIniFile <> '' then
  begin
    AssignFile(plik, fIniFile );
    Reset(plik);
    i :=1;
    repeat
      readln(plik, dane_pole[i]);
      readln(plik, dane_oper[i]);
      readln(plik, dane_type[i]);
      readln(plik, dane_cons[i]);
      inc(i);
    until eof(plik);
    closefile(plik);
    dec(i);
    i3 := i;
  end;

//*****************************************
///////////////////////////////////////////
// *** Write text file with ini file *** //
///////////////////////////////////////////
//*****************************************

  if (fIniFile <> '') and (fExt_File <>  '.XLS') then
  begin
    AssignFile(plik , FileToSave );
    rewrite(plik);
    s := '';
    for i := 1 to i3  do
    begin
      s := s + dane_pole[i] + del;
    end;

    System.Delete(s,length(s),1);
    Writeln(plik, s);

    FDS.First;
    if FirstRowsToSend <> 1 then fds.MoveBy(FirstRowsToSend-1);
    licznik := 0;
//****************************
//****** Write rows text *****
//****************************
//___________________________________________
    repeat
      // firs record without next!!
      if tnext = true then FDS.Next
      else tnext := true;
      Application.ProcessMessages;
      inc(licznik);

      s:='';

      For i2 := 1 to i3 do
      begin
        If UpperCase(dane_oper[i2]) = 'NOT'then s := s+del
        else
        If UpperCase(dane_oper[i2]) = 'CONST' then s := s+dane_cons[i2]+del
        else
         s := s + FDS.fieldbyname(dane_pole[i2]).AsString    + del;
      end;
      while copy(s,length(s),1)= del do System.Delete(s,length(s),1);
      Writeln(plik, s);
      if licznik mod 50 = 0 then
      begin
        Progress := (((licznik+1)*100) div RowsToSend) ;
        c2 := now;
        my_time := c2-c1;
        If Assigned(OnProgress) then
        OnProgress(self , Progress, licznik, my_time) ;
      end;
    until (licznik >= RowsToSend) or (FDS.eof);
//_______________________________________________

    Progress := (((licznik+1)*100) div RowsToSend) ;
    c2 := now;
    my_time := c2-c1;
    If Assigned(OnProgress) then
    OnProgress(self , Progress, licznik, my_time) ;
    CloseFile(plik);
    exit;    ///!!! END
  end;

//**********************************************
//******** xls write begin (with ini file) *****
//**********************************************
  IF (fIniFile <> '') AND (fExt_File='.XLS') THEN
  BEGIN
    fname := fFileToSave;
    XlsBeginStream(Fname);
    FStream := TFileStream.Create(fname, fmOpenReadWrite);
    FStream.Seek(FStream.size ,0);
//********
    FDS.First;
    if FirstRowsToSend <> 1 then fds.MoveBy(FirstRowsToSend-1);
//******** xls write FIRST ROW ***

    For i := 0 to i3-1 do
      XlsWriteCellLabel(FStream,i,0,dane_pole[i+1]);

    if FDS.RecordCount = 0 then
    begin
      FStream.Free;
      exit;
    end;
    licznik := 0;
//****************************
//****** Write cells XLS *****
//****************************
//___________________________________________
    repeat

      // firs record without next!!
      if tnext = true then FDS.Next
      else tnext := true;
      Application.ProcessMessages;
      inc(licznik);

      For i := 0 to i3-1 do
      begin
/// write with of table  ********
        If (UpperCase(dane_oper[i+1]) = 'WRITE') then
        begin
          if UpperCase(dane_type[i+1]) = 'N' then
             XlsWriteCellNumber(FStream,i,licznik,FDS.fieldbyname(dane_pole[i+1]).AsFloat)
            else
            if UpperCase(dane_type[i+1]) = 'I' then
               XlsWriteCellRk(FStream,i,licznik,FDS.fieldbyname(dane_pole[i+1]).AsInteger)
              else
              if  UpperCase(dane_type[i+1]) = 'B' then
                 XlsWriteCellBoolerr(FStream,i,licznik,FDS.fieldbyname(dane_pole[i+1]).AsBoolean)
              else
              begin
                s := FDS.fieldbyname(dane_pole[i+1]).AsString;
                if s <> '' then XlsWriteCellLabel(FStream,i,licznik,s);
              end;
        end;
/// write constans ***********
        If (UpperCase(dane_oper[i+1]) = 'CONST') then
        begin
          s := dane_cons[i+1] ;
          if s = ' ' then s := '';
          if UpperCase(dane_type[i+1]) = 'N' then
             XlsWriteCellNumber(FStream,i,licznik,StrToFloat(s))
            else
            if UpperCase(dane_type[i+1]) = 'I' then
               XlsWriteCellRk(FStream,i,licznik,StrToInt(s))
              else
              if  UpperCase(dane_type[i+1]) = 'B' then
                 begin
                 if (s <> '') and (UpperCase(s[1]) = 'T' ) then
                   XlsWriteCellBoolerr(FStream , i , licznik , True )
                   else XlsWriteCellBoolerr(FStream , i , licznik , False )
                 end
              else
              begin
                if s <> '' then XlsWriteCellLabel(FStream,i,licznik,s);
              end;
        end;
      end;

      if licznik mod 50 = 0 then
      begin
        Progress := (((licznik+1)*100) div RowsToSend) ;
        c2 := now;
        my_time := c2-c1;
        If Assigned(OnProgress) then
        OnProgress(self , Progress, licznik, my_time) ;
      end;
    until (licznik >= RowsToSend) or (FDS.eof);
//_______________________________________________

    Progress := (((licznik+1)*100) div RowsToSend) ;
    c2 := now;
    my_time := c2-c1;
    If Assigned(OnProgress) then
    OnProgress(self , Progress, licznik, my_time) ;

    XlsEndStream(FStream);
    FStream.Free;
    EXIT;   /// !!! END
  END;

///////////////////////////////////////////////////////
// ******** xls write begin (without INI file) ***** //
///////////////////////////////////////////////////////

  IF (fIniFile='') AND (fExt_File='.XLS') THEN
  BEGIN
    fname := fFileToSave;
    XlsBeginStream(Fname);
    FStream := TFileStream.Create(fname, fmOpenReadWrite);
    FStream.Seek(FStream.size ,0);
//********
    FDS.First;
    if FirstRowsToSend <> 1 then fds.MoveBy(FirstRowsToSend-1);

    i3 := length(allowexp);
    if (FDBGrid = nil) and (not isunkn) then dec(i3);
//******** xls write FIRST ROW ***

    For i := 0 to i3-1 do
      XlsWriteCellLabel(FStream,i,0,nazw_pola[i]);
    nazw_pola.Free;

    if FDS.RecordCount = 0 then
    begin
      FStream.Free;
      exit;
    end;
    licznik := 0;
//****************************
//****** Write cells XLS *****
//****************************

//___________________________________________
    repeat

      // firs record without next!!
      if tnext = true then FDS.Next
      else tnext := true;
      Application.ProcessMessages;
      inc(licznik);
      i3 := length(allowexp);
      if (FDBGrid = nil) and (not isunkn) then dec(i3);
      For i := 0 to i3-1 do
      begin

        // Is allowed fied to export ?
        if allowexp[i+1] <> 'N' then
        begin
         if  allowexp[i+1] = 'b' then
           XlsWriteCellBoolerr(FStream,i,licznik,FDS.Fields[tab_pola[i]].AsBoolean)
         else if allowexp[i+1] = 'f' then
           XlsWriteCellNumber(FStream,i,licznik,FDS.Fields[tab_pola[i]].AsFloat)
         else if allowexp[i+1] = 'i' then
           XlsWriteCellRk(FStream,i,licznik,FDS.Fields[tab_pola[i]].AsInteger)
         else
         begin;
           CellLabel:=FDS.Fields[tab_pola[i]].AsString;
           if CellLabel <> '' then
           XlsWriteCellLabel(FStream,i,licznik,CellLabel);
         end;
        end;
      end;

      if licznik mod 50 = 0 then
      begin
        Progress := (((licznik+1)*100) div RowsToSend) ;
        c2 := now;
        my_time := c2-c1;
        If Assigned(OnProgress) then
        OnProgress(self , Progress, licznik, my_time) ;
      end;
    until (licznik >= RowsToSend) or (FDS.eof);
//_______________________________________________

    Progress := (((licznik+1)*100) div RowsToSend) ;
    c2 := now;
    my_time := c2-c1;
    If Assigned(OnProgress) then
    OnProgress(self , Progress, licznik, my_time) ;

    XlsEndStream(FStream);
    FStream.Free;
    EXIT; // end !!!
  END;
//*****************************************
//*    SAVE TEXT FILE WITHOUT INI FILE    *
//*****************************************
  IF (fIniFile='') AND (fExt_File <> '.XLS') THEN
  BEGIN
    AssignFile(plik , FileToSave );
    rewrite(plik);
//********
    FDS.First;
    if FirstRowsToSend <> 1 then fds.MoveBy(FirstRowsToSend-1);

    i3 := length(allowexp);
    if (FDBGrid = nil) and (not isunkn) then dec(i3);
//******** xls write FIRST ROW ***

    s := '';
    For i := 0 to i3-1 do
      s := s + nazw_pola[i] + del ;

    System.Delete(s,length(s),1);
    Writeln(plik, s);
    nazw_pola.Free;

    if FDS.RecordCount = 0 then
    begin
      CloseFile(plik);
      exit;
    end;
    licznik := 0;
//****************************
//****** Write AS text fields *****
//****************************

//___________________________________________
    repeat

      // firs record without next!!
      if tnext = true then FDS.Next
      else tnext := true;
      Application.ProcessMessages;
      inc(licznik);
      i3 := length(allowexp);
      if (FDBGrid = nil) and (not isunkn) then dec(i3);

      s :='';
      For i := 0 to i3-1 do
      begin

        // Is allowed fied to export ?
        if allowexp[i+1] <> 'N' then
        begin
           s :=  s + FDS.Fields[tab_pola[i]].AsString + del;
        end;
      end;

      //write row
      System.Delete(s,length(s),1);
      Writeln(plik, s);

      if licznik mod 50 = 0 then
      begin
        Progress := (((licznik+1)*100) div RowsToSend) ;
        c2 := now;
        my_time := c2-c1;
        If Assigned(OnProgress) then
        OnProgress(self , Progress, licznik, my_time) ;
      end;
    until (licznik >= RowsToSend) or (FDS.eof);
//_______________________________________________

    Progress := (((licznik+1)*100) div RowsToSend) ;
    c2 := now;
    my_time := c2-c1;
    If Assigned(OnProgress) then
    OnProgress(self , Progress, licznik, my_time) ;

    CloseFile(plik);
    EXIT; // end !!!
  END;

end;

procedure TGWGExporterXLSTXT.setDefaultProperty;
begin
  MyWatch := True;
  fRows_to_send := 65535;
  fFirst_Rows_to_send := 1;
  fLast_Rows_to_send := 65535;
  setFExport_Format(1);
  fFileToSave:='';
  FDataSet := NIL;
  FDBGrid := NIL;
  FDataLink.DataSource := NIL ;

end;
////////////////////////////////

//*********************************************
//** XLS***XLS***XLS***XLS***XLS***XLS***XLS **
//*********************************************

//THX to Tomasz Wasiuk ...
procedure TGWGExporterXLSTXT.XlsBeginStream( const filename : String);
var
  XLS_HEADER : TResourceStream;
begin
  XLS_HEADER := TResourceStream.Create(HInstance, 'XLS4HEADER', RT_RCDATA);
  XLS_HEADER.SaveToFile(filename);
  XLS_HEADER.Free;                          
end;                                   

//***XLS***XLS***XLS***XLS***XLS***XLS***XLS***

procedure TGWGExporterXLSTXT.XlsEndStream(XlsStream: TStream);
var Size:Integer;
begin
  XlsStream.WriteBuffer(CXlsEof, SizeOf(CXlsEof));
  XlsStream.WriteBuffer(CXlsEof, SizeOf(CXlsEof));
  Size:=XlsStream.Size-403;
  XlsStream.Seek($17E,soFromBeginning);
  XlsStream.Write(Size,4);
end;
//***XLS***XLS***XLS***XLS***XLS***XLS***XLS***
procedure TGWGExporterXLSTXT.XlsWriteCellLabel(XlsStream: TStream; const ACol, ARow: Word;
const AValue: string);
var
  L: Word;
begin
  L := Length(AValue);
  CXlsLabel[1] := 8 + L;
  CXlsLabel[2] := ARow;
  CXlsLabel[3] := ACol;
  CXlsLabel[5] := L;
  XlsStream.WriteBuffer(CXlsLabel, SizeOf(CXlsLabel));
  XlsStream.WriteBuffer(Pointer(AValue)^, L);
end;

//***XLS***XLS***XLS***XLS***XLS***XLS***XLS***

procedure TGWGExporterXLSTXT.XlsWriteCellRk(XlsStream: TStream; const ACol, ARow: Word; const
AValue: Integer);
var 
  V: Integer; 
begin 
  CXlsRk[2] := ARow;
  CXlsRk[3] := ACol;
  XlsStream.WriteBuffer(CXlsRk, SizeOf(CXlsRk));
  V := (AValue shl 2) or 2; 
  XlsStream.WriteBuffer(V, 4); 
end;

//***XLS***XLS***XLS***XLS***XLS***XLS***XLS***

procedure TGWGExporterXLSTXT.XlsWriteCellBoolerr(XlsStream: TStream; const ACol, ARow: Word;
const AValue: Boolean);
begin
  CXlsBoolerr[2] := ARow;
  CXlsBoolerr[3] := ACol;
  if AValue=False then CXlsBoolerr[5] := 0
  else CXlsBoolerr[5] := 1;
  XlsStream.WriteBuffer(CXlsBoolerr, SizeOf(CXlsBoolerr));
end;

procedure TGWGExporterXLSTXT.XlsWriteCellNumber(XlsStream: TStream; const ACol, ARow: Word;
const AValue: Double);
begin
  CXlsNumber[2] := ARow;
  CXlsNumber[3] := ACol;
  XlsStream.WriteBuffer(CXlsNumber, SizeOf(CXlsNumber));
  XlsStream.WriteBuffer(AValue, 8);
end;
//*********************************************
//** XLS***XLS***XLS***XLS***XLS***XLS***XLS **
//*********************************************

//*****************************************
//** OPENOFFICE_TOOLS***OPENOFFICE_TOOLS **
//*****************************************
//**************************
//*Thanx to Bernard Marcell*
//**************************
function TGWGExporterXLSTXT.convertToURL(winAddr: String): String;
var
  x : Integer; s, sLow : String;

  function escapeToUTF8URL(c: Char): String;
  var
    x: Integer;
  begin
    if ord(c) < 128  then begin
      x:= Pos(c, USASCIIexcl);
      if (c in ['0'..'9', 'a'..'z', 'A'..'Z']) or (x = 0) then
        Result:= c    // accepted, reserved or not reserved characters
      else            // excluded characters, to be converted
        Result:= Copy(USASCIIexcl, x+1, 3);
    end else begin    // convert to UTF8 with two bytes
      x:= Pos(c, UTF8chars);
      if x > 0 then
        Result:= Copy(UTF8chars, x+1, 6)
      else            // unknown character ( update table UTF8chars ? )
        Result:= '?';
    end;
  end;

  function existsPrefix: Boolean;
  var
    x: Integer;
  begin
    Result:= False;
    for x:= 1 to High(URLprefix) do begin
      if Pos(URLprefix[x], sLow) = 1 then
        begin Result:= True; break; end;
    end;
  end;

begin { -------- convertToURL ---------- }
  s:= StringReplace(winAddr, '\', '/', [rfReplaceAll]);
  sLow:= AnsiLowerCase(s);
  if existsPrefix then
    Result:= ''
  else
    if Pos('@', sLow) > 0 then
      Result:= 'mailto:'
    else
      Result:= 'file:///';
  for x:= 1 to Length(s) do
    Result:= Result + escapeToUTF8URL(s[x]);
end;

//** OPENOFFICE_TOOLS***OPENOFFICE_TOOLS **

function TGWGExporterXLSTXT.dummyArray: Variant;
begin
  Result:= VarArrayCreate([0, -1], varVariant);
end;
//*****************************************
//** OPENOFFICE_TOOLS***OPENOFFICE_TOOLS **
//*****************************************


end.
