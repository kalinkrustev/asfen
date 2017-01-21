{ @exclude }
unit rtcDBTypes;

interface

uses
  DB, DBClient, rtcInfo;

{$include rtcDefs.inc}

const
  RTC_DB2FIELD_TYPE: array[TFieldType] of TRtcFieldTypes =
    ( ft_Unknown, ft_String, ft_Smallint, ft_Integer, ft_Word,
      ft_Boolean, ft_Float, ft_Currency, ft_BCD, ft_Date, ft_Time, ft_DateTime,
      ft_Bytes, ft_VarBytes, ft_AutoInc, ft_Blob, ft_Memo, ft_Graphic, ft_FmtMemo,
      ft_ParadoxOle, ft_DBaseOle, ft_TypedBinary, ft_Cursor, ft_FixedChar, ft_WideString,
      ft_Largeint, ft_ADT, ft_Array, ft_Reference, ft_DataSet
      {$IFNDEF IDE_0}
      ,ft_OraBlob, ft_OraClob, ft_Variant,
       ft_Interface, ft_IDispatch,ft_Guid
        {$IFNDEF IDE_1}
        , ft_TimeStamp, ft_FMTBcd
          {$IFDEF IDE_2006up}
            , ft_WideString, ft_WideString, ft_TimeStamp, ft_Variant
          {$ENDIF}
        {$ENDIF}
      {$ENDIF} );

  RTC_FIELD2DB_TYPE: array[TRtcFieldTypes] of TFieldType =
    ( ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
      ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
      ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
      ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
      {$IFDEF IDE_1}
        ftString, // TDataSet in D4 doesn't really support WideString
      {$ELSE}
        ftWideString,
      {$ENDIF}
      ftLargeint, ftADT, ftArray, ftReference, ftDataSet
      {$IFNDEF IDE_1}
        ,ftOraBlob, ftOraClob, ftVariant, ftInterface,
         ftIDispatch,ftGuid, ftTimeStamp, ftFMTBcd
      {$ELSE}
        ,ftBlob, ftMemo, ftString, ftBlob, ftBlob,
        ftString, ftDateTime, ftBcd
      {$ENDIF});

// Can be used to copy data (this code only supports native types, no blobs) from a RTC Dataset to a TDataSet
procedure RtcDataSetToDelphi(rtcDS:TRtcDataSet; DelphiDS:TDataSet; ClearFieldDefs:boolean=True);

// Can be used to copy data (this code only supports native types, no blobs) from a TDataSet to a RTC Dataset
procedure DelphiDataSetToRtc(DelphiDS:TDataSet; rtcDS:TRtcDataSet; ClearFieldDefs:boolean=True);

implementation

procedure RtcDataSetToDelphi(rtcDS:TRtcDataSet; DelphiDS:TDataSet; ClearFieldDefs:boolean=True);
  var
    flds:integer;
    fldname:string;
    field:TField;
  begin
  if ClearFieldDefs then
    begin
    DelphiDS.Active:=False;
    DelphiDS.FieldDefs.Clear;
    for flds:=0 to rtcDS.FieldCount-1 do
      begin
      fldname:=rtcDS.FieldName[flds];
      DelphiDS.FieldDefs.Add(fldname,
                             RTC_FIELD2DB_TYPE[rtcDS.FieldType[fldname]],
                             rtcDS.FieldSize[fldname],
                             rtcDS.FieldRequired[fldname]);
      end;
    if DelphiDS is TClientDataSet then
      TClientDataSet(DelphiDS).CreateDataSet;
    end;

  if not DelphiDS.Active then
    DelphiDS.Active:=True;

  rtcDS.First;
  while not rtcDS.EOF do
    begin
    DelphiDS.Append;
    for flds:=0 to rtcDS.FieldCount-1 do
      begin
      fldname:=rtcDS.FieldName[flds];
      field:=delphiDS.FindField(fldname);
      if assigned(field) then
        field.Value:=rtcDS.Value[fldname];
      end;
    rtcDS.Next;
    end;
  end;

procedure DelphiDataSetToRtc(DelphiDS:TDataSet; rtcDS:TRtcDataSet; ClearFieldDefs:boolean=True);
  var
    fdef:TFieldDef;
    flds:integer;
    fldname:string;
    field:TField;
  begin
  if ClearFieldDefs then
    begin
    rtcDS.Clear;
    for flds:=0 to DelphiDS.FieldCount-1 do
      begin
      fdef:=DelphiDS.FieldDefs.Items[flds];
      fldname:=fdef.Name;

      field:=DelphiDS.FindField(fldname);
      if assigned(field) then
        rtcDS.SetField(fldname,
                       RTC_DB2FIELD_TYPE[fdef.DataType],
                       fdef.Size,
                       fdef.Required);
      end;
    end;

  DelphiDS.First;
  while not delphiDS.EOF do
    begin
    rtcDS.Append;
    for flds:=0 to rtcDS.FieldCount-1 do
      begin
      fldname:=rtcDS.FieldName[flds];
      field:=delphiDS.FindField(fldname);
      if assigned(field) then
        rtcDS.Value[fldname]:=field.Value;
      end;
    delphiDS.Next;
    end;
  end;

end.
