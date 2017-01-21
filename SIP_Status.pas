unit SIP_Status;

interface
uses SyncObjs,Classes;

type
  TSIPStatus=class(TCriticalSection)
  private
    FStatus,FEvent,FScript,FAction:array of String;
  public
    procedure Status(ID:Integer;const Event,Script,Action,Status:String);
    function  Get:String;
    constructor Create(ChannelCount:Integer);
  end;

implementation
uses SysUtils, Util;

{ TSIPStatus }

constructor TSIPStatus.Create;
begin
  inherited Create;
  SetLength(FStatus,ChannelCount);
  SetLength(FEvent,ChannelCount);
  SetLength(FScript,ChannelCount);
  SetLength(FAction,ChannelCount);
end;

procedure TSIPStatus.Status(ID: Integer; const Event,Script,Action,Status:String);
begin
  Acquire;
  try
    ID:=ID-1;
    if (ID>=0) and (ID<Length(FStatus)) then
    begin
      FEvent[ID]:=Event;
      FScript[ID]:=Script;
      FAction[ID]:=Action;
      FStatus[ID]:=Status;
    end
    else
      raise Exception.Create('Invalid status ID='+intTostr(ID)+' (status = '+Status+')');
  finally
    Release;
  end;
end;

function TSIPStatus.Get: String;

var R:String;

  procedure Add(const S:String);
  begin
    if R='' then R:=S else
    if S<>'' then R:=R+','+S;
  end;

var I,J:Integer;
begin
  Result:='{}';
  R:='';
  J:=Length(FStatus) div 2;
  Acquire;
  try
      for I := 0 to Length(FStatus)-1 do
      begin
        if I=J then
        begin
          Result:='{chanlo:['+R+']';
          R:='';
        end;
        
        Add(Format('{channel:%d,status:%s,event:%s,script:%s,action:%s}',[
                   I+1,
                   StrEscape(FStatus[I]),
                   StrEscape(FEvent[I]),
                   StrEscape(FScript[I]),
                   StrEscape(FAction[I])
                   ]));

      end;
      Result:=Result+',chanhi:['+R+']}'
  finally
    Release;
  end;
end;

end.
