unit SIP_Action;

interface
uses Classes;

type
  TSIPOperation=(OP_NONE,OP_RING,OP_DELAY,OP_PLAY,OP_DTMF,OP_JUMP,OP_CALL,OP_HANGUP,OP_SMS,OP_Mail);
  TSIPAction=class(TObject)
  private
    OperandsArray:array of String;
    function GetOperand(Index: Integer): String;
    procedure SetOperands(const Value: String);
    procedure SetAction(const Value: String);
    function GetAction: String;
    function GetOperands: String;
  public
    OpCode:TSIPOperation;
    property Operand[Index:Integer]:String read GetOperand;
    property Operands:String read GetOperands write SetOperands;
  published
    property Action:String read GetAction write SetAction;
  end;

implementation
uses TypInfo, SysUtils;
{ TSIPAction }

function TSIPAction.GetAction: String;
begin
  Result:=GetEnumName(TypeInfo(TSIPOperation),Integer(OpCode));
  if Pos('OP_',Result)=1 then Delete(Result,1,3);
  Result:=LowerCase(Result)+'('+Operands+')';
end;

function TSIPAction.GetOperand(Index: Integer): String;
begin
  if (Index<=0) or (Index>Length(OperandsArray)) then Result:='' else Result:=OperandsArray[Index-1];
end;

function TSIPAction.GetOperands: String;
var
  I: Integer;
  S:TStringList;
begin
  Result:='';
  S:=TStringList.Create;
  try
    for I := 0 to Length(OperandsArray) - 1 do
      S.Add(OperandsArray[I]);
    S.QuoteChar:='''';
    Result:=S.DelimitedText;
  finally
    S.Free;
  end;
end;

procedure TSIPAction.SetAction(const Value: String);
var I,O:Integer;
    Code,OP:String;
begin
  I:=Pos('(',Value);
  Code:=Value;
  if I>0 then
  begin
    Op:=Copy(Value,I+1,Length(Value));
    while (Length(Op)>0) and (Ord(Op[Length(Op)])<=32) do Delete(Op,Length(Op),1);
    if (Length(Op)>0) and (Op[Length(Op)]=')') then Delete(Op,Length(Op),1);
    Operands:=Op;
    Delete(Code,I,Length(Code));
  end;
  O:=GetEnumValue(TypeInfo(TSIPOperation),'OP_'+UpperCase(Code));
  if O<0 then OpCode:=OP_NONE else OpCode:=TSIPOperation(O);
end;

procedure TSIPAction.SetOperands(const Value: String);
var SLine:TStringList;
  I: Integer;
begin
  SLine:=TStringList.Create;
  try
    SLine.QuoteChar:='''';
    SLine.DelimitedText:=Value;
    SetLength(OperandsArray,SLine.Count);
    for I := 0 to SLine.Count - 1 do
       OperandsArray[I]:=SLine[I];
  finally
    Sline.Free;
  end;
end;

end.
