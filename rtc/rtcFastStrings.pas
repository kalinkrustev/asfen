{
  "Fast Huge String manipulation and search classes" - Copyright (c) Danijel Tkalcec
  @exclude
}

unit rtcFastStrings;

{$include rtcDefs.inc}

interface

uses
  memStrIntList, SysUtils, Classes;

const
  RTC_STROBJ_SHIFT = 4; // = 16
  RTC_STROBJ_PACK = 1 shl RTC_STROBJ_SHIFT;
  RTC_STROBJ_AND = RTC_STROBJ_PACK-1;

type
  tRtcStrRec=packed record
    str:string;
    siz:integer;
    end;
  tRtcStrArr=array[0..RTC_STROBJ_PACK-1] of tRtcStrRec;
  PRtcStrArr=^tRtcStrArr;
  tRtcStrArray=array of PRtcStrArr;

  TRtcHugeString=class
  private
    FData:tRtcStrArray;
    FPack:PRtcStrArr;

    FDataCnt,
    FPackCnt,
    FPackFree,
    FPackLoc:integer;

    FCount:integer;
    FSize:int64;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure Add(const s:String);
    function Get:String;

    property Size:int64 read FSize;
    end;

  tRtcStrObjRec=packed record
    str:string;
    obj:TObject;
    end;
  tRtcStrObjArr=array[0..RTC_STROBJ_PACK-1] of tRtcStrObjRec;
  PRtcStrObjArr=^tRtcStrObjArr;

  tRtcStrObjArray=array of PRtcStrObjArr;

  tRtcFastStrObjList=class
  private
    FData:tRtcStrObjArray; // array of PRtcStrObjArr;
    FPack:PRtcStrObjArr;
    Tree:TStrIntList;

    FDataCnt, 
    FPackCnt:integer;
    FCnt:integer;

    function GetName(const index: integer): string;
    function GetValue(const index: integer): TObject;
    procedure SetName(const index: integer; const _Value: string);
    procedure SetValue(const index: integer; const _Value: TObject);
    function GetCount: integer;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure DestroyObjects;

    function Add(const Name:string; _Value:TObject=nil):integer;
    function Find(const Name:string):integer;

    property Objects[const index:integer]:TObject read GetValue write SetValue;
    property Strings[const index:integer]:string read GetName write SetName;

    property Count:integer read GetCount;
    end;

implementation

{ TRtcHugeString }

constructor TRtcHugeString.Create;
  begin
  inherited;

  SetLength(FData,0);
  FDataCnt:=0;

  New(FPack);
  FillChar(FPack^,SizeOf(FPack^),0);

  FPackCnt:=0;
  FPackFree:=0;
  FPackLoc:=0;

  FCount:=0;
  FSize:=0;
  end;

destructor TRtcHugeString.Destroy;
  begin
  Clear;
  Dispose(FPack);
  inherited;
  end;

procedure TRtcHugeString.Clear;
  var
    a,b:integer;
    FPack2:PRtcStrArr;
  begin
  if FDataCnt>0 then
    begin
    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        FPack2^[b].str:='';
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    FDataCnt:=0;
    end;

  if FPackCnt>0 then
    begin
    for b:=0 to FPackCnt-1 do
      FPack^[b].str:='';
    FPackCnt:=0;
    FPackFree:=0;
    FPackLoc:=0;
    end;

  FSize:=0;
  FCount:=0;
  end;

procedure TRtcHugeString.Add(const s: String);
  var
    len:integer;
  begin
  len:=length(s);
  if len>0 then
    begin
    FSize:=FSize + len;

    if FPackFree>=len then
      begin
      with FPack^[FPackCnt-1] do
        begin
        Move(s[1], str[FPackLoc], len);
        Inc(siz, len);
        end;
      Inc(FPackLoc,len);
      Dec(FPackFree,len);
      end
    else
      begin
      if FPackCnt>=RTC_STROBJ_PACK then
        begin
        if length(FData)<=FDataCnt then
          SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
        FData[FDataCnt]:=FPack;
        Inc(FDataCnt);

        New(FPack);
        FillChar(FPack^,SizeOf(FPack^),0);
        FPackCnt:=0;
        end;

      if len>255 then
        begin
        with FPack^[FPackCnt] do
          begin
          str:=s;
          siz:=len;
          end;
        FPackFree:=0;
        FPackLoc:=0;
        end
      else
        begin
        with FPack^[FPackCnt] do
          begin
          SetLength(str, 290);
          Move(s[1],str[1],len);
          siz:=len;
          end;
        FPackFree:=290-len;
        FPackLoc:=len+1;
        end;
      Inc(FPackCnt);
      Inc(FCount);
      end;
    end;
  end;

function TRtcHugeString.Get: String;
  var
    a,b,loc:integer;
    FPack2:PRtcStrArr;
  begin
  if FCount>1 then
    begin
    SetLength(Result, FSize);
    loc:=1;

    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          Move(str[1], Result[loc], siz);
          Inc(loc, siz);
          end;
      end;

    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        begin
        Move(str[1], Result[loc], siz);
        Inc(loc, siz);
        end;

    if loc<>FSize+1 then
      raise Exception.Create('Internal Huge String error.');
    end
  else if FCount>0 then
    begin
    with FPack^[0] do
      begin
      SetLength(Result, siz);
      Move(str[1], Result[1], siz);
      end;
    end
  else
    Result:='';
  end;

{ tRtcFastStrObjList }

constructor tRtcFastStrObjList.Create;
  begin
  inherited;
  Tree:=tStrIntList.Create(RTC_STROBJ_PACK);

  SetLength(FData,0);
  New(FPack);
  FillChar(FPack^,SizeOf(FPack^),0);

  FCnt:=0;
  FDataCnt:=0;
  FPackCnt:=0;
  end;

destructor tRtcFastStrObjList.Destroy;
  begin
  Clear;
  Dispose(FPack);
  Tree.Free;
  inherited;
  end;

procedure tRtcFastStrObjList.Clear;
  var
    a,b:integer;
    FPack2:PRtcStrObjArr;
  begin
  if FPackCnt>0 then
    begin
    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        begin
        str:='';
        obj:=nil;
        end;
    FPackCnt:=0;
    end;

  if FDataCnt>0 then
    begin
    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          str:='';
          obj:=nil;
          end;
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    FDataCnt:=0;
    end;

  Tree.removeall;
  FCnt:=0;
  end;

procedure tRtcFastStrObjList.DestroyObjects;
  var
    a,b:integer;
    FPack2:PRtcStrObjArr;
  begin
  if FPackCnt>0 then
    begin
    for b:=0 to FPackCnt-1 do
      with FPack^[b] do
        begin
        str:='';
        if assigned(obj) then
          begin
          obj.Free;
          obj:=nil;
          end;
        end;
    FPackCnt:=0;
    end;

  if FDataCnt>0 then
    begin
    for a:=0 to FDataCnt-1 do
      begin
      FPack2:=FData[a];
      for b:=0 to RTC_STROBJ_PACK-1 do
        with FPack2^[b] do
          begin
          str:='';
          if assigned(obj) then
            begin
            obj.Free;
            obj:=nil;
            end;
          end;
      Dispose(FPack2);
      end;
    SetLength(FData,0);
    FDataCnt:=0;
    end;

  Tree.removeall;
  FCnt:=0;
  end;

function tRtcFastStrObjList.Add(const Name: string; _Value:TObject=nil): integer;
  begin
  if FPackCnt>=RTC_STROBJ_PACK then
    begin
    if length(FData)<=FDataCnt then
      SetLength(FData, FDataCnt + RTC_STROBJ_PACK);
    FData[FDataCnt]:=FPack;
    Inc(FDataCnt);

    New(FPack);
    FillChar(FPack^,SizeOf(FPack^),0);
    FPackCnt:=0;
    end;

  Tree.insert(UpperCase(Name), FCnt);
  with FPack[FPackCnt] do
    begin
    str:=Name;
    obj:=_Value;
    end;
  Inc(FPackCnt);
  Inc(FCnt);

  Result:=FCnt-1;
  end;

function tRtcFastStrObjList.Find(const Name: string): integer;
  begin
  Result:=Tree.search(UpperCase(Name));
  end;

function tRtcFastStrObjList.GetName(const index: integer): string;
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    Result:=FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].str
  else
    Result:=FPack^[index and RTC_STROBJ_AND].str;
  end;

function tRtcFastStrObjList.GetValue(const index: integer): TObject;
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    Result:=FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].obj
  else
    Result:=FPack^[index and RTC_STROBJ_AND].obj;
  end;

procedure tRtcFastStrObjList.SetName(const index: integer; const _Value: string);
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    begin
    with FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND] do
      begin
      Tree.remove(UpperCase(str));
      str:=_Value;
      Tree.insert(UpperCase(_Value), index);
      end;
    end
  else
    begin
    with FPack^[index and RTC_STROBJ_AND] do
      begin
      Tree.remove(UpperCase(str));
      str:=_Value;
      Tree.insert(UpperCase(_Value), index);
      end;
    end;
  end;

procedure tRtcFastStrObjList.SetValue(const index: integer; const _Value: TObject);
  begin
  if index shr RTC_STROBJ_SHIFT<FDataCnt then
    FData[index shr RTC_STROBJ_SHIFT]^[index and RTC_STROBJ_AND].obj:=_Value
  else
    FPack^[index and RTC_STROBJ_AND].obj:=_Value;
  end;

function tRtcFastStrObjList.GetCount: integer;
  begin
  Result:=FCnt;
  end;
end.
