{ ******************************************************** }
{                                                          }
{                    dbClipImport unit                     }
{        Copyright (c) 1997,1998,1999,2000,2001            }
{             by Joselito G. Real                          }
{                                                          }
{    You can use this unit as freely as you wish,          }
{    compile it with your code, distribute the source but  }
{    you should include my copyright notice in full.       }
{    If you use in part, please credit it as such.         }
{                                                          }
{ ******************************************************** }

unit dbClipImport;

interface

uses SysUtils, Classes, db, ClipBrd, DBGrids;


{ Simply use this unit to add the capability to import TEXT
  data from ClipBoard, StringLists, Text Files, and CSV (comma-
  separated variable length ASCII file) to any TDataSet
  descendants,  including but not limited to DBISAM Tables,
  dBASE & Paradox,  tables, Advantage Tables, etc. This unit
  will not import blobs, graphics nor import memo fields
  containing CR-LF and non-ASCII characters. This unit will
  work with Datasets having blob or graphics fields as long as
  you don't include the fieldnames of these fields in the header
  of the import file or clipboard. This unit can also import memo
  fields as long as the memo fields are composed of a properly
  delimited string.

  The first row should be the fieldname header,
  subsequent rows are data to import. This will only import
  data for the matching fieldnames, it will skip blank column
  names, and any data under the column names not found in
  the destination dataset are also skipped. If you have more
  datafields than your header on the subsequent lines, those
  extra datafields will be ignored.

  This will import data regardless of column position.
  This uses direct index location of data fields, and this saves
  some CPU time especially if you have many data fields.

  Included in this unit are four methods and two functions:

  a) ImportFromClipBoard(const ADataSet:TDataSet);
       Simply type something like
            ImportFromClipBoard(MyTable);
       and this will "paste" the data from a clipboard to your table.

  b) ImportFromFile(const AFileName:String; ADelimiter:Char;
                          const ADataSet:TDataSet);
       Simply type something like
            ImportFromFile('C:\Temp\MyImportFile.TXT',#9, MyTable);
       and this will import the data from a tab-delimited file to
       your dataset. You don't care the position of the fields
       as long as the first row is the header field names. Although
       my implementation can be faster than a direct file reading,
       files greater than 4 MB to import, my implementation can
       become memory resource intensive, a direct file reading
       line by line coupled with read-ahead buffering at 32KB at a time
       may be more efficient and a lot faster than my current
       implementation.

  c) ImportFromStrings(const AStrings:TStrings; ADelimiter:Char;
                       const ADataSet:TDataSet);
     you can use this method to import data from a stringlist,
     as long as element 0 of the list is your header, and you
     can use any delimiter. In fact my ImportFromClipboard and
     ImportFromFile methods uses this procedure. There
     are other numerous applications aside from these.

  d) ImportFromCSVFile(const AFileName:String;
                       const ADataSet:TDataSet);
       If your file is CSV (comma separated variable length), simply
       type something like
            ImportFromFile('C:\Temp\MyImportFile.CSV', MyTable);
       and this will import the data from a CSV file to your Dataset.
       CSV files are commonly produced by Excel. My implementation
       of CSV import uses two parser functions: GetLeftWord and
       GetLeftWordCSV. The File is directly opened for read-only
       and this procedure reads the file line by line and will
       therefore have no memory constraints compared to the
       other ImportFromFile method mentioned earlier.

  e) GetLeftWord(var ASentence:string; ADelimiter:char):string;
     as the name implies, it gets and removes the left most word
     from sentence string. This is the parser that I use to break
     down a line string into individual columns one at a time.

  f) GetLeftWordCSV(var ASentence:string):string;
     this is a special parser designed for CSV strings. This is
     the parser that I use to break down a CSV line string into
     individual columns one at a time.

}


procedure ImportFromClipBoard(const ADataSet:TDataSet);

procedure ImportFromFile(const AFileName:String; ADelimiter:Char;
                         const ADataSet:TDataSet);

procedure ImportFromStrings(const AStrings:TStrings; ADelimiter:Char;
                            const ADataSet:TDataSet);

procedure ImportFromCSVFile(const AFileName:String;
                            const ADataSet:TDataSet);

function GetLeftWordCSV(var ASentence:string):string;

function GetLeftWord(var ASentence:string; ADelimiter:char):string;

procedure ExportToClipBoard(G:TDBGrid);

function  ExportToString(G:TDBGrid):String;

implementation

function GetLeftWordCSV(var ASentence:string):string;
begin
  Result:='';
  ASentence:=Trim(ASentence);// needed to remove spaces and strange chars for CSV
  if Length(ASentence)=0 then exit;
  if ASentence[1]='"' then begin
    Delete(ASentence,1,1);
    Result:=GetLeftWord(ASentence,'"');
    GetLeftWord(ASentence,',');//get rid of comma
    exit;
  end;
  Result:=GetLeftWord(ASentence,',');
end;

function GetLeftWord(var ASentence:string; ADelimiter:char):string;
var i:integer;
begin
  Result := '';
  i := Pos(ADelimiter,ASentence);
  if i = 0 then begin
    Result := Trim(ASentence); ASentence := '';
    exit;
  end;
  Result:=trim(Copy(ASentence,1,i-1));
  Delete(ASentence,1,i);
end;

procedure ImportFromStrings(const AStrings:TStrings; ADelimiter:Char;
                            const ADataSet:TDataSet);
var
  i,j,HeaderCount:integer;
  FldPtr:array[0..255] of TField;
  aColValue,aLineString:string;
  HasCommonField:boolean;
  F:TField;
begin
  {first row must be headers, string list columns should be
   delimited by a common char }
  if AStrings.Count < 2 then exit; // nothing to import
  ALineString:=AStrings[0]; // parse the header
  HasCommonField:=False;
  HeaderCount:=0;
  while (length(ALineString) > 0) and (HeaderCount < 255) do begin
    aColValue := GetLeftWord(ALineString,ADelimiter);
    F:=ADataSet.FindField(aColValue);
    FldPtr[HeaderCount] := F;
    if F <> nil then HasCommonField:=True;
    inc(HeaderCount);
  end;
  if not HasCommonField then exit; // not a single field found on list
  for i := 1 to AStrings.Count-1 do begin
    ALineString := AStrings[i];
    j := -1;
    ADataSet.Append;
    while (length(ALineString) > 0) and (j < HeaderCount-1) do begin
      aColValue := GetLeftWord(ALineString,ADelimiter);
      inc(j);
      if FldPtr[j] = nil then continue; // skip unmatched columns
      try
        FldPtr[j].Text := aColValue;
      except
        // simply ignore all data conversion errors
        // or handle it here as you wish
      end;
    end; // while
    ADataSet.Post;
  end; // for
end; // procedure

procedure ImportFromClipBoard(const ADataSet:TDataSet);
var AStringList:TStringList;
begin
  AStringList:=TStringList.Create;
  try
    AStringList.Text:=ClipBoard.AsText;
    // Excel's clipboard is tab delimited (#9)
    ImportFromStrings(AStringList,#9,ADataSet);
  finally
    AStringList.Free;
  end;
end;

procedure ImportFromFile(const AFileName:String; ADelimiter:Char;
                         const ADataSet:TDataSet);
var AStringList:TStringList;
begin
  if not FileExists(AFileName) then exit;
  AStringList:=TStringList.Create;
  try
    AStringList.LoadFromFile(AFileName);
    ImportFromStrings(AStringList,ADelimiter,ADataSet);
  finally
    AStringList.Free;
  end;
end;

procedure ImportFromCSVFile(const AFileName:String;
                            const ADataSet:TDataSet);
var
  aTextFile:TextFile;
  j,HeaderCount:integer;
  FldPtr:array[0..255] of integer;
  aColValue,aLineString:string;
  HasCommonField:boolean;
begin
  if not FileExists(AFileName) then exit;
  AssignFile(aTextFile,AFileName);
  Reset(aTextFile);
  try
    { first row must be headers }
    Readln(aTextFile, ALineString);
    HasCommonField:=False;
    HeaderCount:=0;
    while (length(ALineString) > 0) and (HeaderCount < 255) do begin
      aColValue := GetLeftWordCSV(ALineString);
      j := ADataSet.FieldDefs.IndexOf(aColValue);
      FldPtr[HeaderCount] := j;
      if j > -1 then HasCommonField:=True;
      inc(HeaderCount);
    end;
    if not HasCommonField then exit; // not a single field found on list
    while not EOF(aTextFile) do begin
      Readln(aTextFile,ALineString);
      j := -1;
      ADataSet.Append;
      while (length(ALineString) > 0) and (j < HeaderCount-1) do begin
        aColValue := GetLeftWordCSV(ALineString);
        inc(j);
        if FldPtr[j] = -1 then continue; // skip unmatched columns
        try
          ADataSet.Fields[FldPtr[j]].AsString := aColValue;
        except
          // simply ignore all data conversion errors
          // or handle it here as you wish
        end;
      end; // while
      ADataSet.Post;
    end; // for
  finally
    CloseFile(aTextFile);
  end;
end; // procedure

procedure ExportToClipBoard(G:TDBGrid);
begin
    ClipBoard.Clear;
    Clipboard.AsText:=ExportToString(G);
end;

function  ExportToString(G:TDBGrid):String;
var S:String;
    J: Integer;
    F:Array of TField;
    I,Cnt:Integer;
    B:String;
begin
  Result:='';
  Cnt:=G.Columns.Count;
  SetLength(F,Cnt);
  S:='';
  for I:=0 to Cnt-1 do
  begin
    F[I]:=G.Columns[I].Field;
    S:=S+F[I].FieldName+#9;
  end;
  if S='' then Exit;
  Delete(S,Length(S),1);
  S:=S+#13#10;
  B:=G.DataSource.DataSet.Bookmark;
  try
    G.DataSource.DataSet.DisableControls;
    try
      G.DataSource.DataSet.First;
      if G.SelectedRows.Count>0 then
      begin
        for J := 0 to G.SelectedRows.Count - 1 do
        begin
          G.DataSource.DataSet.BookMark:=G.SelectedRows[J];
          for i:=0 to Cnt-1 do
            S:=S+F[I].Text+#9;
          Delete(S,Length(S),1);
          S:=S+#13#10;
        end;
      end
      else
      while not G.DataSource.DataSet.Eof do
      begin
        for i:=0 to Cnt-1 do
          S:=S+F[I].Text+#9;

        Delete(S,Length(S),1);
        S:=S+#13#10;
        G.DataSource.DataSet.Next;
      end
    finally
      G.DataSource.DataSet.EnableControls;
    end;
  finally
    G.DataSource.DataSet.Bookmark:=B;
  end;
  Result:=S;
end;

end.

