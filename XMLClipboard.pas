unit XMLClipboard;

interface

function DataSetToXML(Data:TObject;Columns:String='';PutClipboard:Boolean=True):String;
procedure XMLToDataSet(Data:TObject;AllowPlain:Boolean);

implementation
uses DB,DBGrids,Classes,JvSimpleXml,SysUtils,Clipbrd,DBClient,dbClipImport;

type
  TRecordColumn = record
    Visible: Boolean;
    Exportable: Boolean;
    ColumnName: string;
    Column: TColumn;
    Field: TField;
  end;

  TXMLComponent = class(TComponent)
  private
    FColumnCount: Integer;
    FRecordColumns: array of TRecordColumn;
    LastResult:String;
    function ClassNameNoT(AField: TField): string;
    function ExportField(AField: TField): Boolean;
  public
    procedure CheckVisibleColumn(Grid:TDBGrid);
    function DoExport(Grid:TDBGrid): String;
  published
     property XML:String read LastResult write LastResult;
  end;


function TXMLComponent.ExportField(AField: TField): Boolean;
begin
  Result := not (AField.DataType in [ftUnknown, ftBlob, ftGraphic,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftADT,
    ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, ftVariant,
    ftInterface, ftIDispatch, ftGuid]);
end;

procedure TXMLComponent.CheckVisibleColumn(Grid:TDBGrid);
var
  I: Integer;
begin
  FColumnCount := Grid.Columns.Count;
  SetLength(FRecordColumns, FColumnCount);
  for I := 0 to FColumnCount - 1 do
  begin
    FRecordColumns[I].Column := Grid.Columns[I];
    FRecordColumns[I].Visible := Grid.Columns[I].Visible;
    FRecordColumns[I].ColumnName := Grid.Columns[I].FieldName;
    FRecordColumns[I].Field := Grid.Columns[I].Field;
    if FRecordColumns[I].Visible and (FRecordColumns[I].Field <> nil) then
      FRecordColumns[I].Exportable := ExportField(FRecordColumns[I].Field)
    else
      FRecordColumns[I].Exportable := False;
  end;
end;

function TXMLComponent.ClassNameNoT(AField: TField): string;
begin
  Result := AField.ClassName;
  if Result[1] = 'T' then
    Delete(Result, 1, 1);
  if SameText('Field', Copy(Result, Length(Result) - 4, 5)) then { do not localize }
    Delete(Result, Length(Result) - 4, 5);
  Result:=LowerCase(Result);
  if SameText(Result,'WideString') then result:='string' else
  if SameText(Result,'FixedChar') then result:='string' else
  if SameText(Result,'Smallint') then result:='i2' else
  if SameText(Result,'Integer') then result:='i4' else
  if SameText(Result,'LargeInt') then result:='i8' else
  if SameText(Result,'Word') then result:='r8' else
  if SameText(Result,'Float') then result:='r8' else
  if SameText(Result,'Currency') then result:='r8' else
  if SameText(Result,'SQLTimeStamp') then result:='datetime' else
  if SameText(Result,'BCD') then result:='fixed' else
  if SameText(Result,'FMTBCD') then result:='fixed' else
  if SameText(Result,'Bytes') then result:='bin.hex' else
  if SameText(Result,'VarBytes') then result:='bin.hex' else
  if SameText(Result,'Blob') then result:='bin.hex' else
  if SameText(Result,'Memo') then result:='bin.hex' else
  if SameText(Result,'Graphic') then result:='bin.hex' else
  if SameText(Result,'TypedBinary') then result:='bin.hex' else
  if SameText(Result,'FmtMemo') then result:='bin.hex' else
  if SameText(Result,'ParadoxOle') then result:='bin.hex' else
  if SameText(Result,'dBaseOle') then result:='bin.hex';
end;

function TXMLComponent.DoExport(Grid:TDBGrid): String;
var
  FXML: TJvSimpleXML;
  I: Integer;
//  ARecNo, lRecCount: Integer;
  lBookmark: TBookmark;
  lRootNode: TJvSimpleXmlElemClassic;
  lDataNode: TJvSimpleXmlElem;
  lFieldsNode: TJvSimpleXmlElem;
  lRecordNode: TJvSimpleXmlElem;
begin
  CheckVisibleColumn(Grid);
  FXML := TJvSimpleXML.Create(nil);
  try
    FXML.Options := [sxoAutoCreate, sxoAutoIndent];
    FXML.Prolog.StandAlone:=True;
    FXML.Root.Clear;

    // create root node
    FXML.Root.Name := 'DATAPACKET';
    lRootNode := FXML.Root;
    lRootNode.Properties.Add('version', '2.0');


    // add column header and his property
    lDataNode := lRootNode.Items.Add('METADATA');
    lFieldsNode := lDataNode.Items.Add('FIELDS');
    for I := 0 to FColumnCount - 1 do
      with FRecordColumns[I] do
        if Visible and (Field <> nil) then
        begin
          with lFieldsNode.Items.Add('FIELD') do
          begin
            Properties.Add('attrname', ColumnName);
            Properties.Add('fieldtype', ClassNameNoT(Field));
            Properties.Add('width', Column.Width);
          end;
        end;

    // now add all the record
    lRecordNode := lRootNode.Items.Add('ROWDATA');

      with Grid.DataSource.DataSet do
      begin
//        ARecNo := 0;
//        lRecCount := RecordCount;
        //DoProgress(0, lRecCount, ARecNo, Caption);
        DisableControls;
        lBookmark := GetBookmark;
        First;
        try
          while not Eof do
          begin
            with lRecordNode.Items.Add('ROW') do
            begin
              for I := 0 to FColumnCount - 1 do
                if FRecordColumns[I].Exportable and not FRecordColumns[I].Field.IsNull then
                  with FRecordColumns[I] do
                    Properties.Add(ColumnName, Field.AsString);
            end;

            Next;
//            Inc(ARecNo);
//            if not DoProgress(0, lRecCount, ARecNo, Caption) then Last;
          end;
//          DoProgress(0, lRecCount, lRecCount, Caption);
        finally
          if (lBookmark <> nil) and BookmarkValid(lBookmark) then
            GotoBookmark(lBookmark);
          if lBookmark <> nil then
            FreeBookmark(lBookmark);
          EnableControls;
        end;
      end;
    Result:=FXML.XMLData;
    LastResult:=Result;
  finally
    FXML.Free;
  end;
end;

function DataSetToXML(Data:TObject;Columns:String='';PutClipboard:Boolean=True):String;
var Grid:TDBGrid;
    DataSource:TDataSource;
    XML:TXMLComponent;
    C:TStringList;
    I:Integer;
begin
  if (Data is TDBGrid) and TDBGrid(Data).ReadOnly then Exit; 
  XML:=TXMLComponent.Create(nil);
  try
    XML.Name:='XMLComponent';
    XML.Tag:=1;
    if (Columns<>'') and (Data is TDBGrid) then
      Data:=TDBGrid(Data).DataSource;
    if Data is TDataSet then
    begin
      DataSource:=TDataSource.Create(XML);
      DataSource.DataSet:=TDataSet(Data);
      Data:=DataSource;
    end;
    if Data is TDataSource then
    begin
      Grid:=TDBGrid.Create(XML);
      Grid.DataSource:=TDataSource(Data);
      if Columns<>'' then
      begin
        C:=TStringList.Create;
        try
          C.CommaText:=Columns;
          for I:=0 to C.Count-1 do
          begin
            Grid.Columns.Add.FieldName:=C[I];
          end;
        finally
          C.Free;
        end;
      end;
      Data:=Grid;
    end;
    if Data is TDBGrid then
    begin
      Grid:=TDBGrid(Data);
      Result:=XML.DoExport(Grid);
      if PutClipBoard then
      begin
        ClipBoard.Open;
        try
          ClipBoard.AsText:=ExportToString(Grid);
          ClipBoard.SetComponent(XML);
        finally
          Clipboard.Close;
        end;
      end;
    end;
  finally
    XML.Free;
  end;
end;

procedure XMLToDataSet;
var C:TComponent;
    CDS:TClientDataSet;
    Src,Dest:array of TField;
    D:TDataSet;
    Cnt,I:Integer;
begin
  if Data is TDBGrid then Data:=TDBGrid(Data).DataSource;
  if Data is TDataSource then Data:=TDataSource(Data).DataSet;
  if not (Data is TDataSet) then Exit;
  D:=TDataSet(Data);
  C:=ClipBoard.GetComponent(nil,nil);
  try
    if C is TXMLComponent then
    begin
      if C.Tag=1 then
      begin
        CDS:=TClientDataSet.Create(C);
        CDS.XMLData:=TXMLComponent(C).XML;
        Cnt:=0;
        for I:=0 to CDS.Fields.Count-1 do
        begin
          if D.FindField(CDS.Fields[I].FieldName)<>nil then Cnt:=Cnt+1;
        end;
        if Cnt>0 then
        begin
          SetLength(Src,Cnt);
          SetLength(Dest,Cnt);
          Cnt:=0;
          for I:=0 to CDS.Fields.Count-1 do
          if D.FindField(CDS.Fields[I].FieldName)<>nil then
          begin
            Dest[Cnt]:=D.FieldByName(CDS.Fields[I].FieldName);
            Src[Cnt]:=CDS.Fields[I];
            Cnt:=Cnt+1;
          end;
          CDS.First;
          while not CDS.Eof do
          begin
            D.Append;
            for I:=0 to Length(Src)-1 do
            if not Src[I].IsNull then
              Dest[I].AsString:=Src[I].AsString;
            D.CheckBrowseMode;  
            CDS.Next;
          end;
        end;
      end;
    end else
    if AllowPlain then ImportFromClipBoard(D);
  finally
    C.Free;
  end;
end;

initialization
  RegisterClasses([TXMLComponent]);
end.
