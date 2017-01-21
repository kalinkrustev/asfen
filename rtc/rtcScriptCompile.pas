{
  @html(<b>)
  RTC Script Compiler
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  This unit implements the RTC Script Compiler.
}

unit rtcScriptCompile;

interface

{$include rtcDefs.inc}

uses
  SysUtils, Classes,

  rtcInfo, rtcFunction, rtcConn;

type
  // @exclude
  ERtcScript=class(Exception);

  // @exclude
  ERtcForward=class(EAbort);

  // @exclude
  TRtcScriptCompiler=class
    private
      FGroup: TRtcFunctionGroup;
      FDenyRTCFunctionCalls: boolean;
      FDenyScriptFunctionCalls: boolean;

      FFilePath:string;
      FFileName:string;

      FScriptOpen1,FScriptOpen2,
      FScriptClose1,FScriptClose2:char;

      FScript:string;
      Size,Loc:integer;
      row,col:integer;

      procedure skipWhitespace;

      function findScriptBegin:String;

      function isParam: boolean;
      function isName: boolean;
      function isNumber: boolean;
      function isString(long_strings:boolean): boolean;
      function isOperator: boolean;
      function isFrontOperator: boolean;
      function isMidOperator: boolean;
      function isHighOperator: boolean;
      function isAssignment: boolean;
      function isComparison: boolean;
      function isClosing(const s:string): boolean;
      function isOpening: boolean;
      function isSeparator: boolean;
      function isDot: boolean;

      function peekName:string;

      function getName:string;
      function getProperty:string;
      function getNumber:string;
      function getOperator:string;
      function getOpening:string;

      procedure skipClosing(const s:string);
      procedure skipSeparator;
      procedure skipDot;

      function getString:string;

      function EOF:boolean;

      function CompileValue(long_strings:boolean; MidOperators:boolean=True):TRtcValueObject;

      function CompileScript:TRtcValueObject;

      procedure SetScript(const Value: string);

      function GetScriptClose: string;
      function GetScriptOpen: string;
      procedure SetScriptClose(const Value: string);
      procedure SetScriptOpen(const Value: string);

    public
      constructor Create;
      destructor Destroy; override;

      function Compile:TRtcValue;

      property Script:string read FScript write SetScript;
      property FileName:string read FFileName write FFileName;
      property FilePath:string read FFilePath write FFilePath;

      { Characters used to open a script block.
        There always need to be exectly 2 characters for this,
        and only a subset of character combinations is allowed. }
      property ScriptOpen:string read GetScriptOpen write SetScriptOpen;
      { Characters used to close a script block.
        There always need to be exectly 2 characters for this,
        and only a subset of character combinations is allowed. }
      property ScriptClose:string read GetScriptClose write SetScriptClose;

      { If a FunctionGroup is assigned before compilation,
        each function name used in script will be checked (does the function exist?) }
      property FunctionGroup:TRtcFunctionGroup read FGroup write FGroup;

      { If you set DenyRTCFunctionCalls to TRUE, any attempts to call a RTC function
        (implemented using TRtcFunction and TRtcFunctionGroup components) from within
        the Script will fail and an exception will be raised. }
      property DenyRTCFunctionCalls:boolean read FDenyRTCFunctionCalls write FDenyRTCFunctionCalls;

      { If you set DenyScriptFunctionCalls to TRUE, any attempts to call a Script function
        (functions implemented inside the script) will fail (exception) during script execution. }
      property DenyScriptFunctionCalls:boolean read FDenyScriptFunctionCalls write FDenyScriptFunctionCalls;
      end;

implementation

const
  RTC_NAMESTART_CHARSET:set of char=['a'..'z','A'..'Z','_','$','@'];
  RTC_NAME_CHARSET:set of char=['a'..'z','A'..'Z','0'..'9','_','$','@'];
  RTC_NAME_CHARSET2:set of char=['a'..'z','A'..'Z','0'..'9','_','$','@','.'];

  RTC_NUMSTART_CHARSET:set of char=['0'..'9'];
  RTC_NUM_CHARSET:set of char=['0'..'9','.'];

  RTC_OPERATOR_CHARSET:set of char=['+','-','*','/','%','>','<','=','&','|','!',':','^','#'];
  RTC_ASSIGN_CHARSET:set of char=['+','-','*','/','%','&','|',':','^'];

  RTC_LIST_SEPARATORS:set of char=[',',';'];

  RTC_WHITESPACE:set of char=[#9,#32,#10,#13];

{ TRtcScriptCompiler }

constructor TRtcScriptCompiler.Create;
  begin
  inherited;
  ScriptOpen:='<?';
  ScriptClose:='?>';
  FFilePath:='';
  FFileName:='';
  FScript:='';
  Loc:=1;
  Row:=1;Col:=1;
  Size:=0;
  FGroup:=nil;
  FDenyRTCFunctionCalls:=False;
  FDenyScriptFunctionCalls:=False;
  end;

destructor TRtcScriptCompiler.Destroy;
  begin
  FFilePath:='';
  FFileName:='';
  FFilePath:='';
  FScript:='';
  FGroup:=nil;
  inherited;
  end;

procedure TRtcScriptCompiler.SetScript(const Value: string);
  begin
  FScript:=Value;
  Loc:=1;
  Row:=1;Col:=1;
  Size:=length(FScript);
  end;

function TRtcScriptCompiler.EOF: boolean;
  begin
  Result:=loc>size;
  end;

function TRtcScriptCompiler.findScriptBegin: String;
  var
    i:integer;
    found:boolean;
  begin
  found:=false;
  for i:=loc to size-1 do
    begin
    if (Script[i]=FScriptOpen1) and (Script[i+1]=FScriptOpen2) then
      begin
      found:=True;
      // Get text before <?
      Result:=Copy(Script,loc,i-loc);
      loc:=i;
      Break;
      end
    else if Script[i]=#10 then
      begin
      Inc(row);
      col:=1;
      end
    else
      Inc(col);
    end;

  if not found then
    begin
    // Get all remaining text
    Result:=Copy(Script,loc,size+1-loc);
    loc:=size+1;
    end;
  end;

procedure TRtcScriptCompiler.skipWhitespace;
  begin
  while loc<size do
    begin
    case Script[loc] of
      #32,#13,#9:
        begin
        Inc(col);
        Inc(loc);
        end;
      #10:
        begin
        Inc(loc);
        Inc(row);
        col:=1;
        end;
      '/':
        begin
        if Script[loc+1]='/' then
          begin
          // Skip //
          Inc(loc,2);Inc(col,2);
          while (loc<size) and (Script[loc]<>#10) do
            begin
            Inc(loc);Inc(col);
            end;
          // skip #10
          Inc(loc); Inc(row); col:=1;
          end
        else
          Break;
        end;
      '(':
        begin
        if Script[loc+1]='*' then
          begin
          // Skip (*
          Inc(loc,2);Inc(col,2);
          while (loc<size) do
            begin
            case Script[loc] of
              '*':if Script[loc+1]=')' then
                    Break
                  else
                    begin
                    Inc(loc);
                    Inc(col);
                    end;
              #10:begin
                  Inc(loc);
                  Inc(row);
                  col:=1;
                  end;
              else
                begin
                Inc(loc);Inc(col);
                end;
              end;
            end;
          if loc<size then
            begin
            // skip *)
            Inc(loc,2);Inc(col,2);
            end
          else
            raise ERtcScript.Create('Comment started with "(*", but not closed with "*)"');
          end
        else
          Break;
        end;
      else
        Break;
      end;
    end;
  end;

function TRtcScriptCompiler.isString(long_strings:boolean): boolean;
  begin
  if Script[loc] in ['"',#39] then
    Result:=True
  else if long_strings and (Script[loc]=FScriptClose1) and (Script[loc+1]=FScriptClose2) then
    Result:=True
  else
    Result:=False;
  end;

function TRtcScriptCompiler.getString:string;
  var
    i,c,r:integer;
    found:boolean;
    terminator:char;
  begin
  if (loc<size) and (Script[loc]=FScriptClose1) and (Script[loc+1]=FScriptClose2) then
    begin
    // skip ?>
    Inc(loc,2); Inc(col,2);

    found:=false;
    for i:=loc to size-1 do
      begin
      if (Script[i]=FScriptOpen1) and (Script[i+1]=FScriptOpen2) then
        begin
        found:=True;
        // Get text before <?
        Result:=Copy(Script,loc,i-loc);
        loc:=i+2; Inc(col,2);
        Break;
        end
      else if Script[i]=#10 then
        begin
        Inc(row);
        col:=1;
        end
      else
        Inc(col);
      end;

    if not found then
      begin
      // Get all remaining text
      Result:=Copy(Script,loc,size+1-loc);
      loc:=size+1;
      end;
    end
  else
    begin
    terminator:=Script[loc];

    // skip " '
    Inc(loc);Inc(col);

    c:=col; r:=row;
    found:=false;

    if terminator='"' then
      begin
      for i:=loc to size do
        case Script[i] of
          '"':
            begin
            col:=c; row:=r;
            found:=True;
            if i=loc then
              Result:=''
            else
              Result:=Copy(Script,loc,i-loc);
            loc:=i+1;
            Break;
            end;
          #10:
            begin
            Break;
            {Inc(r); c:=1;}
            end;
          else
            Inc(c);
          end;
      end
    else
      begin
      for i:=loc to size do
        case Script[i] of
          #39:
            begin
            col:=c; row:=r;
            found:=True;
            if i=loc then
              Result:=''
            else
              Result:=Copy(Script,loc,i-loc);
            loc:=i+1;
            Break;
            end;
          #10:
            begin
            Break;
            { Inc(r); c:=1; }
            end;
          else
            Inc(c);
          end;
      end;
    if not found then
      raise ERtcScript.Create('Unterminated short string (starts with '+terminator+').'#13#10+
                              'Short strings have to be closed before the end of line.');
    end;
  end;

function TRtcScriptCompiler.isName: boolean;
  begin
  Result:=Script[loc] in RTC_NAMESTART_CHARSET;
  end;

function TRtcScriptCompiler.isDot: boolean;
  begin
  Result:=Script[loc]='.';
  end;

function TRtcScriptCompiler.isParam: boolean;
  var
    i:integer;
    ws:boolean;
  begin
  ws:=False;
  Result:=false;
  for i:=loc to size-1 do
    if Script[i] in RTC_NAME_CHARSET then
      begin
      if ws then Break;
      end
    else if Script[i] in RTC_WHITESPACE then
      ws:=True
    else if Script[i]=':' then
      begin
      if not (Script[i+1] in ['<','>','=']) then
        Result:=True;
      Break;
      end
    else
      Break;
  end;

function TRtcScriptCompiler.getName: string;
  var
    i:integer;
    found:boolean;
    r,c:integer;
  begin
  r:=row; c:=col;
  found:=false;
  for i:=loc to size-1 do
    if Script[i] in RTC_NAME_CHARSET then
      Inc(c)
    else
      begin
      row:=r; col:=c;
      found:=True;
      // Get name
      Result:=Copy(Script,loc,i-loc);
      loc:=i;
      Break;
      end;

  if not found then
    raise ERtcScript.Create('Malformed Name, ending with '+Script[i]);
  end;

function TRtcScriptCompiler.getProperty: string;
  var
    i:integer;
    found:boolean;
    r,c:integer;
  begin
  r:=row; c:=col;
  found:=false;
  for i:=loc to size-1 do
    if Script[i] in RTC_NAME_CHARSET2 then
      Inc(c)
    else
      begin
      row:=r; col:=c;
      found:=True;
      // Get name
      Result:=Copy(Script,loc,i-loc);
      loc:=i;
      Break;
      end;

  if not found then
    raise ERtcScript.Create('Malformed Name, ending with '+Script[i]);
  end;

function TRtcScriptCompiler.peekName: string;
  var
    i:integer;
    found:boolean;
  begin
  found:=false;
  for i:=loc to size-1 do
    if Script[i] in RTC_NAME_CHARSET then
      found:=True
    else
      begin
      if found then
        Result:=Copy(Script,loc,i-loc);
      Break;
      end;

  if not found then
    Result:='';
  end;

function TRtcScriptCompiler.isNumber: boolean;
  begin
  Result:=Script[loc] in RTC_NUMSTART_CHARSET;
  end;

function TRtcScriptCompiler.getNumber: string;
  var
    i:integer;
    found:boolean;
    r,c:integer;
  begin
  r:=row; c:=col;
  found:=false;
  for i:=loc to size-1 do
    if Script[i] in RTC_NUM_CHARSET then
      Inc(c)
    else
      begin
      row:=r; col:=c;
      found:=True;
      // Get number
      Result:=Copy(Script,loc,i-loc);
      loc:=i;
      Break;
      end;

  if not found then
    raise ERtcScript.Create('Malformed Number, ending with '+Script[i]);
  end;

function TRtcScriptCompiler.isOperator: boolean;
  begin
  Result:=Script[loc] in RTC_OPERATOR_CHARSET;
  end;

function TRtcScriptCompiler.isAssignment: boolean;
  begin
  Result:=(Script[loc+1]='=') and
          (Script[loc] in RTC_ASSIGN_CHARSET);
  end;

function TRtcScriptCompiler.isComparison: boolean;
  begin
  Result:=Script[loc] in ['<','>','='];
  if not Result then
    if Script[loc+1]='=' then
      Result:= Script[loc]='!';
  end;

function TRtcScriptCompiler.isFrontOperator: boolean;
  begin
  if Script[loc] in ['-','!','#','^'] then
    Result:= (Script[loc+1]<>'=')
  else
    Result:=False;
  end;

function TRtcScriptCompiler.isMidOperator: boolean;
  begin
  if Script[loc]='!' then
    begin
    if Script[loc+1]='=' then
      Result:=True
    else
      Result:=False;
    end
  else
    Result:=isOperator;
  end;

function TRtcScriptCompiler.isHighOperator: boolean;
  var
    n:string;
  begin
  Result:=Script[loc] in ['*','/','%','^'];
  if not Result then
    if isName then
      begin
      n:=UpperCase(peekName);
      Result:=(n='DIV') or (n='MOD');
      end;
  end;

function TRtcScriptCompiler.getOperator: string;
  begin
  if Script[loc] in ['<','>','='] then
    begin
    if Script[loc+1] in ['<','>','='] then
      begin
      Result:=Script[loc]+Script[loc+1];
      Inc(loc,2);Inc(col,2);
      end
    else
      begin
      Result:=Script[loc];
      Inc(loc);Inc(col);
      end;
    end
  else if Script[loc] in RTC_OPERATOR_CHARSET then
    begin
    if Script[loc+1]='=' then
      begin
      Result:=Script[loc]+Script[loc+1];
      Inc(loc,2);Inc(col,2);
      end
    else if (Script[loc]=':') and (Script[loc+1] in ['>','<']) then
      begin
      Result:=Script[loc]+Script[loc+1];
      Inc(loc,2);Inc(col,2);
      end
    else
      begin
      Result:=Script[loc];
      Inc(loc);Inc(col);
      end;
    end
  else
    raise ERtcScript.Create('Malformed operator, starting with '+Script[loc]);
  end;

function TRtcScriptCompiler.isOpening: boolean;
  begin
  if (Script[loc] in ['(','[','{']) then
    Result:=True
  else if isName and (UpperCase(peekName)='BEGIN') then
    Result:=True
  else
    Result:=False;
  end;

function TRtcScriptCompiler.getOpening: string;
  begin
  if (Script[loc] in ['(','[','{']) then
    begin
    Result:=Script[loc];
    Inc(loc);Inc(col);
    end
  else if isName and (UpperCase(peekName)='BEGIN') then
    Result:=getName
  else
    Result:='';
  end;

function TRtcScriptCompiler.isClosing(const s:string): boolean;
  begin
  if (s='(') and (Script[loc]=')') then
    begin
    Result:=True;
    end
  else if (s='[') and (Script[loc]=']') then
    begin
    Result:=True;
    end
  else if (s='{') and (Script[loc]='}') then
    begin
    Result:=True;
    end
  else if (UpperCase(s)='BEGIN') and isName and (UpperCase(peekName)='END') then
    begin
    Result:=True;
    end
  else
    Result:=False;
  end;

procedure TRtcScriptCompiler.skipClosing(const s:string);
  begin
  if (s='(') and (Script[loc]=')') then
    begin
    Inc(loc);Inc(col);
    end
  else if (s='[') and (Script[loc]=']') then
    begin
    Inc(loc);Inc(col);
    end
  else if (s='{') and (Script[loc]='}') then
    begin
    Inc(loc);Inc(col);
    end
  else if (UpperCase(s)='BEGIN') and isName and (UpperCase(peekName)='END') then
    begin
    getName;
    end
  else
    raise ERtcScript.Create('Opened with "'+s+'", closing with "'+Script[loc]+'"?');
  end;

function TRtcScriptCompiler.isSeparator: boolean;
  begin
  Result:= Script[loc] in RTC_LIST_SEPARATORS;
  end;

procedure TRtcScriptCompiler.skipSeparator;
  begin
  Inc(loc);Inc(col);
  end;

procedure TRtcScriptCompiler.skipDot;
  begin
  Inc(loc);Inc(col);
  skipWhiteSpace;
  end;

function TRtcScriptCompiler.CompileValue(long_strings:boolean; MidOperators:boolean=True): TRtcValueObject;
  var
    idx:integer;
    op, fname, parname:string;
    obj: TRtcValueObject;
    func: TRtcFunctionInfo;
    arr: TRtcArray;
    rec: TRtcRecord;
  begin
  Result:=nil;
  try
    if isSeparator then
      begin
      Result:=nil;
      skipSeparator;
      Exit; // No more processing!
      end
    else if isFrontOperator then
      begin
      fname:=getOperator;

      func:=TRtcFunctionInfo.Create;
      func.FunctionName:='?';
      func.asInteger['$R']:=row;
      func.asInteger['$C']:=col;
      func.asString['C']:=fname;
      Result:=func;

      skipWhitespace;
      func.asObject['Y']:=CompileValue(True, False);
      end
    else if isString(long_strings) then
      Result:=TRtcStringValue.Create(getString)
    else if isNumber then
      begin
      fname:=getNumber;
      if Pos('.',fname)=0 then
        Result:=TRtcLargeIntValue.Create(StrToInt64(fname))
      else
        Result:=TRtcFloatValue.Create(Str2Float(fname));
      end
    else if isOpening then
      begin
      op:=getOpening;

      skipWhitespace;
      if isClosing(op) then
        Result:=nil
      else if isParam then // record (fields with names)
        begin
        obj:=nil;
        rec:=nil;
        Result:=nil;
        idx:=0;

        repeat
          if isParam then
            begin
            parname:=getProperty;
            skipWhitespace;
            // Skip ":"
            Inc(loc);Inc(col);
            skipWhitespace;
            end
          else
            parname:=IntToStr(idx);

          if not (isSeparator or isClosing(op)) then
            begin
            obj:=CompileValue(true);
            if assigned(obj) then
              begin
              if not assigned(rec) then
                begin
                rec:=TRtcRecord.Create;
                Result:=rec;
                end;
              rec.asObject[parname]:=obj;
              end;
            SkipWhitespace;
            end;
          Inc(idx);

          if isSeparator then
            begin
            skipSeparator;
            skipWhitespace;
            end
          else if not isClosing(op) then
            begin
            if not (obj is TRtcStringValue) then
              if not isString(long_strings) then
                raise ERtcScript.Create('List separator or closing expected');
            end;

          until isClosing(op);
        end
      else // array (enumeration, no element names)
        begin
        obj:=nil;
        arr:=nil;
        Result:=nil;
        idx:=0;

        repeat
          if not (isSeparator or isClosing(op)) then
            begin
            obj:=CompileValue(true);
            if assigned(obj) then
              begin
              if not assigned(Result) then
                Result:=obj
              else if not assigned(arr) then
                begin
                arr:=TRtcArray.Create;
                arr.asObject[idx-1]:=Result;
                arr.asObject[idx]:=obj;
                Result:=arr;
                end
              else
                arr.asObject[idx]:=obj;
              end;
            skipWhitespace;
            end;
          Inc(idx);

          if isSeparator then
            begin
            skipSeparator;
            skipWhitespace;
            end
          else if not isClosing(op) then
            begin
            if not (obj is TRtcStringValue) then
              if not isString(long_strings) then
                raise ERtcScript.Create('List separator or closing expected');
            end;

          until isClosing(op);
        end;

      skipClosing(op);
      end
    else if isName then
      begin
      fname:=GetName;
      parname:=UpperCase(fname);
      skipWhitespace;

      if (parname='CODE') then
        begin
        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='@';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        Result:=func;

        func.asObject['Y']:=CompileValue(True, True);
        end
      else if (parname='NEW') then
        begin
        if not isName then
          raise ERtcScript.Create('Type name expected after NEW');

        fname:=UpperCase(GetName);
        skipWhiteSpace;

        if (fname<>'ARRAY') and
           (fname<>'RECORD') and
           (fname<>'DATASET') and
           (fname<>'STREAM') and (fname<>'BYTESTREAM') and
           (fname<>'INTEGER') and (fname<>'INT') and (fname<>'INT32') and
           (fname<>'LARGEINT') and (fname<>'INT64') and
           (fname<>'DATETIME') and (fname<>'DATE') and (fname<>'TIME') and
           (fname<>'FLOAT') and (fname<>'DOUBLE') and
           (fname<>'CURRENCY') and
           (fname<>'BOOLEAN') and (fname<>'BOOL') and
           (fname<>'WIDESTRING') and
           (fname<>'TEXT') then
          raise ERtcScript.Create('Type "'+fname+'" not supported in NEW');

        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='?';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:='NEW';
        func.asString['X']:=fname;
        Result:=func;
        end
      else if (parname='NULL') or (parname='NIL') then
        Result:=nil
      else if (parname='NOT') or (parname='RAISE') then
        begin
        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='?';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:=parname;
        Result:=func;

        skipWhitespace;
        func.asObject['Y']:=CompileValue(True);
        end
      else if (parname='IF') then
        begin
        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='!';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:='I';
        Result:=func;

        func.asObject['I']:=CompileValue(TRUE);
        skipWhitespace;
        if not isName or (UpperCase(getName)<>'THEN') then
          raise ERtcScript.Create('"IF" statement: "THEN" expected');
        skipWhitespace;
        func.asObject['X']:=CompileValue(TRUE);
        skipWhitespace;
        if isName then
          begin
          if UpperCase(getName)<>'ELSE' then
            raise ERtcScript.Create('"IF" statement: "ELSE" or ";" expected');
          skipWhitespace;
          func.asObject['Y']:=CompileValue(TRUE);
          end;
        end
      else if (parname='REPEAT') then
        begin
        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='!';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:='R';
        Result:=func;

        obj:=nil;
        arr:=nil;
        idx:=0;
        repeat
          if not isSeparator and not (isName and (UpperCase(peekName)='UNTIL')) then
            begin
            obj:=CompileValue(true);
            if assigned(obj) then
              begin
              if not assigned(arr) then
                arr:=func.newArray('X');
              arr.asObject[idx]:=obj;
              end;
            skipWhitespace;
            end;
          Inc(idx);

          if isSeparator then
            begin
            repeat
              skipSeparator;
              skipWhitespace;
              until not isSeparator;
            if isName and (UpperCase(peekName)='UNTIL') then
              Break;
            end
          else if isName and (UpperCase(peekName)='UNTIL') then
            Break
          else
            begin
            if not (obj is TRtcStringValue) then
              if not isString(long_strings) then
                raise ERtcScript.Create('"REPEAT" statement: Command separator or "UNTIL" expected');
            end;
          until false;

        // skip "UNTIL"
        getName;
        skipWhitespace;

        func.asObject['I']:=CompileValue(TRUE);
        end
      else if (parname='WHILE') then
        begin
        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='!';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:='W';
        Result:=func;

        func.asObject['I']:=CompileValue(TRUE);
        skipWhitespace;
        if not isName or (UpperCase(getName)<>'DO') then
          raise ERtcScript.Create('"WHILE" statement: "DO" expected');
        skipWhitespace;
        func.asObject['X']:=CompileValue(TRUE);
        end
      else if (parname='FOREACH') then
        begin
        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='!';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:='E';
        Result:=func;

        func.asObject['I']:=CompileValue(TRUE);
        skipWhitespace;

        if not isName or (UpperCase(getName)<>'DO') then
          raise ERtcScript.Create('"FOREACH" statement: "DO" expected');
        skipWhitespace;

        func.asObject['X']:=CompileValue(TRUE);
        end
      else if (parname='FOR') then
        begin
        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='!';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:='F';
        Result:=func;

        if not isName then
          raise ERtcScript.Create('"FOR" statement: Variable name expected');
        parname:=getName;

        if Copy(parname,1,1)<>'$' then
          raise ERtcScript.Create('"FOR" statement: Variable name has to start with "$"');

        func.asVarName['V']:=Copy(parname,2,length(parname)-1);
        skipWhitespace;

        if not isOperator or (getOperator<>':=') then
          raise ERtcScript.Create('"FOR" statement: ":=" expected');
        skipWhitespace;

        func.asObject['A']:=CompileValue(TRUE);
        skipWhitespace;

        if not isName then
          raise ERtcScript.Create('"FOR" statement: "TO" or "DOWNTO" expected');
        parname:=UpperCase(getName);
        if parname='TO' then
          func.asBoolean['D']:=True
        else if parname='DOWNTO' then
          func.asBoolean['D']:=False
        else
          raise ERtcScript.Create('"FOR" statement: "TO" or "DOWNTO" expected');
        skipWhitespace;

        func.asObject['B']:=CompileValue(TRUE);
        skipWhitespace;

        if not isName or (UpperCase(getName)<>'DO') then
          raise ERtcScript.Create('"FOR" statement: "DO" expected');
        skipWhitespace;

        func.asObject['X']:=CompileValue(TRUE);
        end
      else if (parname='FUNCTION') then
        begin
        if DenyScriptFunctionCalls then
          raise ERtcScript.Create('Local script Functions denied (see Script Compiler properties).');

        func:=TRtcFunctionInfo.Create;
        func.FunctionName:='!';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asString['C']:='X';
        Result:=func;

        if not isName then
          raise ERtcScript.Create('"FUNCTION" declaration: Function name expected');
        parname:=getName;

        if Copy(parname,1,1)<>'$' then
          raise ERtcScript.Create('"FUNCTION" declaration: Script Function names have to start with "$"');

        func.asVarName['V']:=Copy(parname,2,length(parname)-1);
        skipWhitespace;

        func.asObject['X']:=CompileValue(TRUE);
        end
      else if (parname='TRUE') then
        begin
        Result:=TRtcBooleanValue.Create(true);
        end
      else if (parname='FALSE') then
        begin
        Result:=TRtcBooleanValue.Create(false);
        end
      else if (parname='AND') or (parname='OR') or (parname='XOR') or
         (parname='IN') or (parname='IS') then
        raise ERtcScript.Create('"'+parname+'" without Left-side parameter')
      else if (parname='ELSE') then
        raise ERtcScript.Create('"ELSE" without "IF"')
      else if (parname='UNTIL') then
        raise ERtcScript.Create('"UNTIL" without "REPEAT"')
      else if (parname='DO') then
        raise ERtcScript.Create('"DO" without "WHILE", "FOR" or "FOREACH"')
      else if (parname='TO') or (parname='DOWNTO') then
        raise ERtcScript.Create('"'+parname+'" without "FOR"')
      else if (parname='END') then
        raise ERtcScript.Create('"END" without "BEGIN"')
      else if (parname='THEN') then
        raise ERtcScript.Create('"THEN" without "IF"')
      else
        begin
        if not isOpening then
          begin
          if Copy(fname,1,1)='$' then
            begin
            if fname='$' then
              raise ERtcScript.Create('Parameter expected after $');
            // Variable names always begin with $
            func:=TRtcFunctionInfo.Create;
            func.FunctionName:='$';
            func.asInteger['$R']:=row;
            func.asInteger['$C']:=col;
            func.asVarName['$V']:=Copy(fname,2,length(fname)-1);
            end
          else if Copy(fname,1,1)='@' then
            begin
            if fname='@' then
              raise ERtcScript.Create('Parameter expected after @');
            // Variable names always begin with $
            func:=TRtcFunctionInfo.Create;
            func.FunctionName:='$';
            func.asInteger['$R']:=row;
            func.asInteger['$C']:=col;
            func.asVarName['$V']:=Copy(fname,2,length(fname)-1);
            func.asBoolean['$A']:=True;
            end
          else if (parname='REQUEST') or
             (parname='RESPONSE') or
             (parname='SESSION') or
             (parname='QUERY') or
             (parname='INPUT') or
             (parname='CLIENT') or
             (parname='SERVER') then
            begin
            if (parname<>'CLIENT') and (parname<>'SERVER') and
               (parname<>'INPUT') and (parname<>'QUERY') and not isDot then
              raise ERtcScript.Create('Parameter expected after '+parname);
            func:=TRtcFunctionInfo.Create;
            func.FunctionName:='$';
            func.asInteger['$R']:=row;
            func.asInteger['$C']:=col;
            func.asVarName['$V']:='-';
            func.asVarName['$P']:=parname;
            end
          else
            begin
            if assigned(FGroup) then
              begin
              if not FGroup.FunctionExists(fname) then
                raise ERtcScript.Create('RTC Function "'+fname+'" not found in FunctionGroup.')
              else if DenyRTCFunctionCalls then
                raise ERtcScript.Create('Can not call RTC Function "'+fname+'" (RTC Function calls denied).');
              end
            else if DenyRTCFunctionCalls then
              raise ERtcScript.Create('Can not call RTC Function "'+fname+'" (no FunctionGroup and RTC Function calls denied).');

            func:=TRtcFunctionInfo.Create;
            func.FunctionName:=fname;
            end;
          Result:=func;
          end
        else
          begin
          op:=getOpening;

          if Copy(fname,1,1)='$' then
            begin
            func:=TRtcFunctionInfo.Create;
            func.FunctionName:='$';
            func.asInteger['$R']:=row;
            func.asInteger['$C']:=col;
            func.asVarName['$V']:=Copy(fname,2,length(fname)-1);
            end
          else if Copy(fname,1,1)='@' then
            begin
            func:=TRtcFunctionInfo.Create;
            func.FunctionName:='$';
            func.asInteger['$R']:=row;
            func.asInteger['$C']:=col;
            func.asVarName['$V']:=Copy(fname,2,length(fname)-1);
            func.asBoolean['$A']:=True;
            end
          else if (parname='REQUEST') or
             (parname='RESPONSE') or
             (parname='SESSION') or
             (parname='QUERY') or
             (parname='INPUT') or
             (parname='CLIENT') or
             (parname='SERVER') then
            begin
            func:=TRtcFunctionInfo.Create;
            func.FunctionName:='$';
            func.asInteger['$R']:=row;
            func.asInteger['$C']:=col;
            func.asVarName['$V']:='-';
            func.asVarName['$P']:=parname;
            end
          else
            begin
            if assigned(FGroup) then
              begin
              if not FGroup.FunctionExists(fname) then
                raise ERtcScript.Create('RTC Function "'+fname+'" not found in FunctionGroup.')
              else if DenyRTCFunctionCalls then
                raise ERtcScript.Create('Can not call RTC Function "'+fname+'" (RTC Function calls denied).');
              end
            else if DenyRTCFunctionCalls then
              raise ERtcScript.Create('Can not call RTC Function "'+fname+'" (no FunctionGroup and RTC Function calls denied).');

            func:=TRtcFunctionInfo.Create;
            func.FunctionName:=fname;
            end;
          Result:=func;

          skipWhitespace;
          if isClosing(op) then
            begin
            // No parameters
            end
          else if isParam then // parameters with names
            begin
            obj:=nil;
            idx:=0;
            repeat
              if isParam then
                begin
                parname:=getProperty;
                skipWhitespace;
                // Skip ":"
                Inc(loc);Inc(col);
                skipWhitespace;
                end
              else
                parname:=IntToStr(idx);

              if not (isSeparator or isClosing(op)) then
                begin
                obj:=CompileValue(true);
                if assigned(obj) then
                  func.asObject[parname]:=obj;
                skipWhitespace;
                end;
              Inc(idx);

              if isSeparator then
                begin
                skipSeparator;
                skipWhitespace;
                end
              else if not isClosing(op) then
                begin
                if not (obj is TRtcStringValue) then
                  if not isString(long_strings) then
                    raise ERtcScript.Create('List separator or closing expected');
                end;

              until isClosing(op);
            end
          else // parameters without names - will be passed as "PARAMS" array
            begin
            obj:=nil;
            arr:=nil;
            idx:=0;
            repeat
              if not (isSeparator or isClosing(op)) then
                begin
                obj:=CompileValue(true);
                if assigned(obj) then
                  begin
                  if not assigned(arr) then
                    arr:=func.newArray('PARAMS');
                  arr.asObject[idx]:=obj;
                  end;
                skipWhitespace;
                end;
              Inc(idx);

              if isSeparator then
                begin
                skipSeparator;
                skipWhitespace;
                end
              else if not isClosing(op) then
                begin
                if not (obj is TRtcStringValue) then
                  if not isString(long_strings) then
                    raise ERtcScript.Create('List separator or closing expected');
                end;

              until isClosing(op);
            end;

          skipClosing(op);
          end;
        end;
      end
    else
      raise ERtcScript.Create('Script Error! Terminated with "'+Script[loc]+'"');

    skipWhitespace;
    while isDot do
      begin
      skipDot;

      fname:=GetProperty;
      skipWhitespace;

      if not isOpening then
        begin
        func:=TRtcFunctionInfo.Create;

        func.FunctionName:='?';
        func.asString['C']:='.';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asObject['X']:=Result;
        func.asString['Y']:=fname;
        Result:=func;
        end
      else
        begin
        op:=getOpening;

        func:=TRtcFunctionInfo.Create;

        func.FunctionName:='?';
        func.asString['C']:='.';
        func.asInteger['$R']:=row;
        func.asInteger['$C']:=col;
        func.asObject['X']:=Result;
        Result:=func;
      
        with func.newFunction('Y','$.') do
          begin
          asInteger['$R']:=row;
          asInteger['$C']:=col;
          asVarName['$V']:=fname;

          skipWhitespace;
          if not isClosing(op) then
            begin
            asObject['X']:=CompileValue(True);
            skipWhitespace;
            if not isClosing(op) then
              raise ERtcScript.Create('Closing expected');
            end;
          end;
        skipClosing(op);
        end;
      skipWhitespace;
      end;

    if MidOperators then
      begin
      repeat
        if isAssignment then
          begin
          fname:=getOperator;
          skipWhitespace;

          if Result is TRtcFunctionInfo then
            begin
            func:=TRtcFunctionInfo(Result);
            if (func.FunctionName<>'$') and
               ( (func.FunctionName<>'?') or (func.asString['C']<>'.') ) then
              raise ERtcScript.Create('Left side can not be assigned to');
            end
          else
            raise ERtcScript.Create('Left side can not be assigned to');

          func:=TRtcFunctionInfo.Create;
          func.FunctionName:='$!';
          func.asString['C']:=fname;
          func.asInteger['$R']:=row;
          func.asInteger['$C']:=col;
          func.asObject['X']:=Result;
          Result:=func;

          func.asObject['Y']:=CompileValue(True, True);
          end
        else if isComparison then
          begin
          fname:=getOperator;
          skipWhitespace;

          func:=TRtcFunctionInfo.Create;
          func.FunctionName:='?';
          func.asString['C']:=fname;
          func.asInteger['$R']:=row;
          func.asInteger['$C']:=col;
          func.asObject['X']:=Result;
          Result:=func;

          func.asObject['Y']:=CompileValue(True, True);
          end
        else if isMidOperator then
          begin
          fname:=getOperator;
          skipWhitespace;

          func:=TRtcFunctionInfo.Create;
          func.FunctionName:='?';
          func.asString['C']:=fname;
          func.asInteger['$R']:=row;
          func.asInteger['$C']:=col;
          func.asObject['X']:=Result;
          Result:=func;

          func.asObject['Y']:=CompileValue(True, False);
          end
        else if isName then
          begin
          parname:=UpperCase(peekName);

          if (parname='AND') or (parname='OR') or (parname='XOR') or
             (parname='IN') or (parname='IS') or
             (parname='DIV') or (parname='MOD') or
             (parname='SHL') or (parname='SHR') then
            begin
            fname:=getName;
            skipWhitespace;

            func:=TRtcFunctionInfo.Create;
            func.FunctionName:='?';
            func.asString['C']:=fname;
            func.asInteger['$R']:=row;
            func.asInteger['$C']:=col;
            func.asObject['X']:=Result;
            Result:=func;

            func.asObject['Y']:=CompileValue(True, False);
            end
          else
            Break;
          end
        else
          Break;
        skipWhitespace;
        until not (isMidOperator or isName);
      end
    else if isHighOperator then
      begin
      if isName then
        fname:=getName
      else
        fname:=getOperator;
      skipWhitespace;

      func:=TRtcFunctionInfo.Create;
      func.FunctionName:='?';
      func.asString['C']:=fname;
      func.asInteger['$R']:=row;
      func.asInteger['$C']:=col;
      func.asObject['X']:=Result;
      Result:=func;

      func.asObject['Y']:=CompileValue(True, False);
      end;
  except
    on E:Exception do
      begin
      if assigned(Result) then Result.Free;
      raise;
      end;
    end;
  end;

function TRtcScriptCompiler.CompileScript: TRtcValueObject;
  var
    arr:TRtcArray;
    obj:TRtcValueObject;
  begin
  // Skip '<?'
  Inc(loc,2); Inc(col,2);

  Result:=nil; arr:=nil;

  try
    repeat
      skipWhitespace;
      obj:=CompileValue(false);
      if assigned(obj) then
        begin
        if not assigned(Result) then
          Result:=obj
        else if not assigned(arr) then
          begin
          arr:=TRtcArray.Create;
          arr.asObject[0]:=Result;
          arr.asObject[1]:=obj;
          Result:=arr;
          end
        else
          arr.asObject[arr.Count]:=obj;
        end;

      skipWhitespace;
      if isSeparator then
        begin
        skipSeparator;
        skipWhitespace;
        if (Script[loc]=FScriptClose1) and (Script[loc+1]=FScriptClose2) then
          Break;
        end
      else if (Script[loc]=FScriptClose1) and (Script[loc+1]=FScriptClose2) then
        Break
      else if not isString(false) then
        if not (assigned(obj) and (obj is TRtcStringValue)) then
          raise ERtcScript.Create('Command separator or script closing expected')
      until false;
  except
    on E:Exception do
      begin
      if assigned(Result) then Result.Free;
      raise;
      end;
    end;

  // Skip '?>'
  Inc(loc,2); Inc(col,2);
  end;

function TRtcScriptCompiler.Compile: TRtcValue;
  var
    arr:TRtcArray;
    obj:TRtcValueObject;
    str:string;
  begin
  Result:=TRtcValue.Create;
  arr:=Result.newArray;
  try
    while not EOF do
      begin
      str:=findScriptBegin;
      if str<>'' then
        arr.asString[arr.Count]:=str;

      if EOF then Break;
      obj:=CompileScript;
      if assigned(obj) then
        arr.asObject[arr.Count]:=obj;
      end;
  except
    on E:ERtcForward do
      begin
      Result.Free;
      raise ERtcScript.Create('['+IntToStr(row)+':'+IntToStr(col)+'] '+
                              'Compiling '+FFileName+':'+#13#10+E.Message);
      end;
    on E:Exception do
      begin
      Result.Free;
      raise ERtcScript.Create('['+IntToStr(row)+':'+IntToStr(col)+'] '+
                              E.ClassName+' compiling '+FFileName+':'+#13#10+E.Message);
      end;
    end;
  end;

function TRtcScriptCompiler.GetScriptClose: string;
  begin
  Result:=FScriptClose1+FScriptClose2;
  end;

function TRtcScriptCompiler.GetScriptOpen: string;
  begin
  Result:=FScriptOpen1+FScriptOpen2;
  end;

procedure TRtcScriptCompiler.SetScriptClose(const Value: string);
  begin
  if length(Value)<>2 then
    raise Exception.Create('Need 2 characters for Script closing')
  else
    begin
    FScriptClose1:=value[1];
    FScriptClose2:=value[2];
    end;
  end;

procedure TRtcScriptCompiler.SetScriptOpen(const Value: string);
  begin
  if length(Value)<>2 then
    raise Exception.Create('Need 2 characters for Script opening')
  else
    begin
    FScriptOpen1:=value[1];
    FScriptOpen2:=value[2];
    end;
  end;

end.
