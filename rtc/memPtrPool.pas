{
  "Pointer Pool" - Copyright (c) Danijel Tkalcec
  @exclude
}

unit memPtrPool;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils;

const
  MinPoolSize=10;
  MaxPoolSize=MaxLongInt div SizeOf(pointer);

type
  tPtrPoolElems = array[1..MaxPoolSize] of pointer;

  pPtrPoolElems = ^tPtrPoolElems;

  tPtrPool = class(TObject)
    private
      pObjs:pPtrPoolElems;
      fCount,fSize:integer;
      procedure SetSize(x:integer);
    public
      constructor Create(Size:integer=0);
      destructor Destroy; override;
      function Put(x:pointer):boolean; // if Pool is full, return FALSE and Free object memory
      function Get:pointer; // if Pool is empty, return FALSE (you have to create the Object)
      property Size:integer read fSize write SetSize;
      property Count:integer read fCount;
    end;

implementation

{ tPrtPool }

constructor tPtrPool.Create(Size: integer);
begin
  inherited Create;
  fSize:=Size;
  if fSize>0 then
    GetMem(pObjs,Sizeof(pointer)*fSize)
  else
    pObjs:=nil;
  fCount:=0;
end;

destructor tPtrPool.Destroy;
begin
  fCount:=0;
  if fSize>0 then
    begin
    FreeMem(pObjs); pObjs:=nil;
    fSize:=0;
    end;
  inherited;
end;

function tPtrPool.Get:pointer;
begin
  if fCount>0 then
    begin
    Result:=pObjs^[fCount];
    Dec(fCount);
    end
  else
    Result:=nil;
end;

function tPtrPool.Put(x: pointer): boolean;
begin
  if fCount<fSize then
    begin
    Inc(fCount);
    pObjs^[fCount]:=x;
    Result:=True;
    end
  else
    Result:=False;
end;

procedure tPtrPool.SetSize(x: integer);
begin
  if x<>fSize then
    begin
    fSize:=x;
    ReallocMem(pObjs,fSize*SizeOf(pointer));
    end;
end;

end.

