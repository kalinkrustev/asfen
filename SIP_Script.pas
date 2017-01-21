unit SIP_Script;

interface
uses Contnrs,SIP_Action,Classes;

type
  TSIPScript=class(TPersistent)
  private
    FObjects:TObjectList;
    FScriptID: Integer;
    function GetAction(Index: Integer): TSIPAction;
    procedure SetName(const Value: String);
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    function GetText: String;
    procedure SetText(const Value: String);
    procedure SetScriptID(const Value: Integer);
  public
    FName:String;
    procedure AddAction(OpCode:TSIPOperation;Operands:String);
    procedure AddActionString(Action:String);
    property Action[Index: Integer]:TSIPAction read GetAction;
    constructor Create;
    destructor Destroy;override;
    function Count:Integer;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Delete(Index:Integer);
    property Text:String read GetText write SetText;
    function IsGroup:Boolean;
    function IsDTMF:Boolean;
    procedure Move(Index1,Index2:Integer);
    function LastPlayAction:TSIPAction;
  published
    property Description:String read FName write SetName;
    property ScriptID:Integer read FScriptID write SetScriptID;
  end;

  TSIPScriptComponent=class(TComponent)
  private
    FScript: TSIPScript;
  published
    property Script:TSIPScript read FScript write FScript;
  end;

implementation
uses SysUtils, Util;



{ TSIPScript }

procedure TSIPScript.AddAction(OpCode: TSIPOperation; Operands: String);
var A:TSIPAction;
begin
  A:=TSIPAction.Create;
  A.OpCode:=OpCode;
  A.Operands:=Operands;
  FObjects.Add(A);
end;

procedure TSIPScript.AddActionString(Action: String);
var A:TSIPAction;
begin
  A:=TSIPAction.Create;
  A.Action:=Action;
  FObjects.Add(A);
end;

function TSIPScript.Count: Integer;
begin
  Result:=FObjects.Count;
end;

constructor TSIPScript.Create;
begin
  FObjects:=TObjectList.Create;
end;

procedure TSIPScript.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Actions', ReadData, WriteData, true);
end;

procedure TSIPScript.Delete(Index: Integer);
begin
  FObjects.Delete(Index);
end;

destructor TSIPScript.Destroy;
begin
  FreeAndNil(FObjects);
  inherited;
end;

function TSIPScript.GetAction(Index: Integer): TSIPAction;
begin
  Result:=FObjects.Items[Index] as TSIPAction;
end;

function TSIPScript.GetText: String;
var C:TSIPScriptComponent;
begin
  C:=TSIPScriptComponent.Create(nil);
  try
    C.Script:=Self;
    Result:=ComponentToString(C);
  finally
    C.Free;
  end;
end;

function TSIPScript.IsDTMF: Boolean;
var
  I: Integer;
begin
  Result:=False;
  for I := 0 to FObjects.Count - 1 do
    if (Action[I].OpCode = OP_DTMF) then
    begin
      Result:=True;
      Break;
    end;
end;

function TSIPScript.IsGroup: Boolean;
var
  I: Integer;
begin
  Result:=False;
  for I := 0 to FObjects.Count - 1 do
    if ((Action[I].OpCode in [OP_RING]) and SameText(Action[I].Operand[1],'group'))
    or ((Action[I].OpCode in [OP_SMS,OP_Mail]) and SameText(Action[I].Operand[2],'group')) then
    begin
      Result:=True;
      Break;
    end;
end;

function TSIPScript.LastPlayAction: TSIPAction;
var I:Integer;
    Ring:Boolean;
begin
  Result:=nil;
  Ring:=False;
  for I := 0 to FObjects.Count - 1 do
    if not Ring and (Action[I].OpCode in [OP_RING]) then Ring:=True else
    if Ring and (Action[I].OpCode in [OP_PLAY]) then Result:=Action[I];
end;

procedure TSIPScript.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;

  FObjects.Clear;
  while not Reader.EndOfList do AddActionString(Reader.ReadString);

  Reader.ReadListEnd;
end;

procedure TSIPScript.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TSIPScript.SetScriptID(const Value: Integer);
begin
  FScriptID := Value;
end;

procedure TSIPScript.SetText(const Value: String);
var S:TSIPScriptComponent;
begin
  if Value='' then
  begin
    FObjects.Clear;
    Exit;
  end;
  S:=TSIPScriptComponent.Create(nil);
  S.Script:=Self;
  try
    StringToComponent(Value,S);
  finally
    S.Free;
  end;
end;

procedure TSIPScript.Move(Index1, Index2: Integer);
begin
  if (Index1>=0) and (Index1<FObjects.Count) and (Index2>=0) and (Index2<FObjects.Count) then
    FObjects.Move(Index1,Index2);
end;

procedure TSIPScript.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do Writer.WriteString(Action[I].Action);
  Writer.WriteListEnd;
end;

end.
