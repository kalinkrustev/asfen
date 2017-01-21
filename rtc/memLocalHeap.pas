{
  "Local Heap Manager" - Copyright (c) Danijel Tkalcec
  @exclude
}

unit memLocalHeap;

{$INCLUDE rtcDefs.inc}

interface

type
  TLocalHeapManager = class
  public
    Total_Alloc:cardinal;

    class function UsesLocalHeap:boolean;

    constructor Create;
    destructor Destroy; override;

    function Clear:boolean;

    function Check_Mem(p: Pointer): Cardinal;
    function Get_Mem(size: Cardinal): Pointer;
    function Free_Mem(p: Pointer): Integer;
    function Realloc_Mem(p: Pointer; size: Cardinal): Pointer;
    function Get_HeapStatus: THeapStatus;
    end;

implementation

{ The next lines were copied from Borland's Memory Manager definition file.

  Start copy ->> }

type
  PUsed = ^TUsed;
  TUsed = packed record
    sizeFlags: Integer;
  end;

const
  cAlign        = 4;
  cThisUsedFlag = 2;
  cPrevFreeFlag = 1;
  cFillerFlag   = Integer($80000000);
  cFlags        = cThisUsedFlag or cPrevFreeFlag or cFillerFlag;

function TLocalHeapManager.Check_Mem(p: Pointer): Cardinal;
  begin
  Result := (PUsed(PChar(p)-sizeof(PUsed)).sizeFlags and not cFlags) - sizeof(TUsed);
  end;

{ <<- end copy. }

function TLocalHeapManager.Get_Mem(size: Cardinal): Pointer;
  begin
  Result:=SysGetMem(size);
  Total_Alloc := Total_Alloc + size;
  end;

function TLocalHeapManager.Free_Mem(p: Pointer): Integer;
  begin
  Total_Alloc := Total_Alloc - Check_Mem(p);
  Result:=SysFreeMem(p);
  end;

function TLocalHeapManager.Realloc_Mem(p: Pointer; size: Cardinal): Pointer;
  begin
  Total_Alloc := Total_Alloc - Check_Mem(p);
  Result:=SysReallocMem(p,size);
  if Result<>nil then
    Total_Alloc := Total_Alloc + size
  else
    Total_Alloc := Total_Alloc + Check_Mem(p);
  end;

function TLocalHeapManager.Get_HeapStatus: THeapStatus;
  begin
  Result:=GetHeapStatus;
  end;

constructor TLocalHeapManager.Create;
  begin
  inherited Create;
  end;

destructor TLocalHeapManager.Destroy;
  begin
  inherited;
  end;

function TLocalHeapManager.Clear:boolean;
  begin
  result:=False;
  end;

class function TLocalHeapManager.UsesLocalHeap:boolean;
  begin
  Result:=False;
  end;

var
  OldExitProc:procedure;

procedure DeInitMem;
  begin
  if assigned(OldExitProc) then
    OldExitProc;
  end;

initialization
finalization
{$IFDEF IDE_1}
OldExitProc:=ExitProc;
ExitProc:=@DeInitMem;
{$ELSE}
OldExitProc:=ExitProcessProc;
ExitProcessProc:=DeInitMem;
{$ENDIF}
end.
