{
  @html(<b>)
  Memory Manager
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  To enable the use of the RTC Memory manager in your project,
  you have to put the rtcMemory unit as first unit in your Project's USES clause.
  In addition to that, you have to DEFINE the "RtcMemManager" compiler directive.
  @html(<br><br>)

  If this compiler directive is not declared, the standard Delphi Memory manager
  will be used, even if you use the rtcMemory unit in your project file.
}

unit rtcMemory;

{$INCLUDE rtcDefs.inc}

{$IFNDEF FPC}
  {$O-}
{$ENDIF}

interface

{$IFDEF RtcMemManager}

uses
  memLocalHeap, // LocalHeapManager
  memManager,   // Mem_Manager

  rtcSyncObjs;

const
  // Default settings for heap management (do not modify!)
  LargeHeap_Info:THeapInfo = (minsize:2048;  blocksize:256; alloc:4; pool:128;
                             free:true; freeany:true; organize:true; sysmem:true);

{$ENDIF}

const
  // Automatically initialize the memory manager?
  AutoInit_Mem:boolean=True;

  { If @true, Mem_Manager will be used for larger blocks and LocalHeapManager for smaller blocks.
    If @false, Mem_Manager will NOT BE USED. }
  UseMemManager:boolean=True;

  // Size of min. block which will be stored using our Heap Manager
  MinBlockStored:integer = 512;

// @exclude
function MemGetBufferSize(Heap:byte):longint;
// @exclude
function MemGetTotalFree(Heap:byte):longint;
// @exclude
function MemGetTotalSize(Heap:byte):longint;
// @exclude
function MemGetUnusedSize(Heap:byte):longint;
// @exclude
function MemGetFragmentCount(Heap:byte):longint;

{ Get Complete Heap Status }
function Get_HeapStatus:THeapStatus;

{ Check the ammount of memoy in use (bytes) }
function Get_MemoryInUse:longint;

{ Check how much Address Space is used by the Application (KB) }
function Get_AddressSpaceUsed: Cardinal;

{ Initialize the Memory manager.

  This will be called automatically in unit initialization,
  if @Link(AutoInit_Mem) is set to @true (default setting). }
procedure Init_Mem;

implementation

uses SysUtils, Windows;

function Get_AddressSpaceUsed: Cardinal;
var
  LMemoryStatus: TMemoryStatus;
begin
  {Set the structure size}
  LMemoryStatus.dwLength := SizeOf(LMemoryStatus);
  {Get the memory status}
  GlobalMemoryStatus(LMemoryStatus);
  {The result is the total address space less the free address space}
  Result := (LMemoryStatus.dwTotalVirtual - LMemoryStatus.dwAvailVirtual) shr 10;
end;

{$IFDEF RtcMemManager}
var
  DMA:TMemoryManager;

  RealHeap:TLocalHeapManager;
  LargeHeap:TMem_Manager;

  MM_CS:TRtcCritSec;
  Inside_Mem:integer;

  MemReady:boolean=False;
  SysMemReady:boolean=False;

{ Global Memory Manager }

function MemGetBufferSize(Heap:byte):longint;
  begin
  MM_CS.Enter;
  try
    if not MemReady then
      Result:=0
    else
      Result:=LargeHeap.GetBufferSize(Heap);
  finally
    MM_CS.Leave;
    end;
  end;

function MemGetTotalFree(Heap:byte):longint;
  begin
  MM_CS.Enter;
    try
    if not MemReady then
      Result:=0
    else
      Result:=LargeHeap.GetTotalFree(Heap);
  finally
    MM_CS.Leave;
    end;
  end;

function MemGetTotalSize(Heap:byte):longint;
  begin
  MM_CS.Enter;
  try
    if not MemReady then
      Result:=0
    else
      Result:=LargeHeap.GetTotalSize(Heap);
  finally
    MM_CS.Leave;
    end;
  end;

function MemGetUnusedSize(Heap:byte):longint;
  begin
  MM_CS.Enter;
  try
    if not MemReady then
      Result:=0
    else
      Result:=LargeHeap.GetUnusedSize(Heap);
  finally
    MM_CS.Leave;
    end;
  end;

function MemGetFragmentCount(Heap:byte):longint;
  begin
  MM_CS.Enter;
  try
    if not MemReady then
      Result:=0
    else
      Result:=LargeHeap.GetFragmentCount(Heap);
  finally
    MM_CS.Leave;
    end;
  end;

function Free_Mem(p:pointer):longint;
  begin
  MM_CS.Enter;
  try
    Inc(Inside_Mem);
    if Inside_Mem=1 then
      begin
      if LargeHeap.Free_Mem(p) then
        Result:=0
      else
        Result:=RealHeap.Free_Mem(p);
      end
    else
      Result:=RealHeap.Free_Mem(p);
  finally
    Dec(Inside_Mem);
    MM_CS.Leave;
    end;
  end;

function Get_Mem(size:longint):pointer;
  begin
  MM_CS.Enter;
  try
    Inc(Inside_Mem);

    if (size<=MinBlockStored) or // small block, prefer Delphi Memory manager
       (Inside_Mem>1) then // or ... called from inside our Mem. Manager, need Delphi manager
      begin
      Result:=RealHeap.Get_Mem(size);

      if Result=nil then
        OutOfMemoryError;
      {if (Result=nil) and (Inside_Mem=1) then
        Result:=LargeHeap.Get_Mem(size);}
      end
    else
      begin
      Result:=LargeHeap.Get_Mem(size);

      if Result=nil then
        OutOfMemoryError;
        {Result:=RealHeap.Get_Mem(size);}
      end;
  finally
    Dec(Inside_Mem);
    MM_CS.Leave;
    end;
  end;

function Realloc_Mem(P: Pointer; Size: Integer): Pointer;
  var
    OldSize:integer;
    lh:boolean;
  begin
  if Size=0 then
    begin
    if p<>nil then Free_Mem(p);
    Result:=nil;
    end
  else if p=nil then
    begin
    Result:=Get_Mem(size);
    end
  else
    begin
    Result:=nil;

    MM_CS.Enter;
    try
      Inc(Inside_Mem);

      if Inside_Mem=1 then
        begin
        OldSize:=LargeHeap.Check_Mem(p);
        if OldSize=0 then
          begin
          OldSize:=RealHeap.Check_Mem(p);
          lh:=False;
          end
        else
          lh:=True;
        end
      else
        begin
        OldSize:=RealHeap.Check_Mem(p);
        lh:=False;
        end;

      if (size>OldSize) or // growing
         (size<OldSize-16) then // shrinking for at least 16 bytes
        begin
        if not lh then
          Result:=RealHeap.Realloc_Mem(p,size);
        end
      else // not changing size
        Result:=p;

      if Result=nil then
        begin
        // Allocate New Block
        Dec(Inside_Mem);
        try
          Result:=Get_Mem(size);
        finally
          Inc(Inside_Mem);
          end;

        if Result<>nil then
          begin
          // Copy old Block to new location
          if size<OldSize then Move(p^,Result^,size)
          else Move(p^,Result^,OldSize);

          // Free old Block
          if lh then
            LargeHeap.Free_Mem(p)
          else
            RealHeap.Free_Mem(p);
          end
        else
          begin
          if size<OldSize then
            Result:=p // return old memory block, without resizing
          else
            begin
            // free old memory block, return NIL
            if lh then
              LargeHeap.Free_Mem(p)
            else
              RealHeap.Free_Mem(p);
            end;
          end;
        end;
    finally
      Dec(Inside_Mem);
      MM_CS.Leave;
      end;
    end;
  if Result=nil then
    OutOfMemoryError;
  end;

const
  NewMemMgr: TMemoryManager = (
    GetMem: Get_Mem;
    FreeMem: Free_Mem;
    ReallocMem: Realloc_Mem);

function Free_Mem2(p:pointer):longint;
  begin
  Result:=RealHeap.Free_Mem(p);
  end;

function Get_Mem2(size:longint):pointer;
  begin
  Result:=RealHeap.Get_Mem(size);
  end;

function Realloc_Mem2(P: Pointer; Size: Integer): Pointer;
  begin
  Result:=RealHeap.Realloc_Mem(p,Size);
  end;

const
  SysMemMgr: TMemoryManager = (
    GetMem: Get_Mem2;
    FreeMem: Free_Mem2;
    ReallocMem: Realloc_Mem2);

function Get_HeapStatus:THeapStatus;
  begin
  MM_CS.Enter;
  try
    if SysMemReady then
      begin
      Result:=RealHeap.Get_HeapStatus;
      if MemReady then
        begin
        Inc(Result.TotalAddrSpace, LargeHeap.Total_AddrSpace);
        Inc(Result.TotalAllocated, LargeHeap.Total_Alloc);
        Inc(Result.TotalFree, MemGetTotalFree(0));
        end;
      end
    else
      Result:=GetHeapStatus;
  finally
    MM_CS.Leave;
    end;
  end;

function Get_MemoryInUse:longint;
  begin
  MM_CS.Enter;
  try
    if SysMemReady then
      begin
      Result:=RealHeap.Total_Alloc;
      if MemReady then
        Inc(Result, LargeHeap.Total_Alloc);
      end
    else
      Result:=GetHeapStatus.TotalAllocated;
  finally
    MM_CS.Leave;
    end;
  end;

procedure Init_Mem;
  begin
  MM_CS.Enter;
  try
    if UseMemManager then
      begin
      if not MemReady then
        begin
        RealHeap:=TLocalHeapManager.Create;

        Inside_Mem:=0;

        LargeHeap:=TMem_Manager.Create(True);
        LargeHeap.HeapInfo:=LargeHeap_Info;

        GetMemoryManager(DMA);
        SetMemoryManager(NewMemMgr);

        SysMemReady:=True;
        MemReady:=True;
        end;
      end
    else
      begin
      if not SysMemReady then
        begin
        RealHeap:=TLocalHeapManager.Create;

        GetMemoryManager(DMA);
        SetMemoryManager(SysMemMgr);

        SysMemReady:=True;
        end;
      end;
  finally
    MM_CS.Leave;
    end;
  end;
initialization
MM_CS:=TRtcCritSec.Create;
if AutoInit_Mem then Init_Mem;
{$ELSE}

function MemGetBufferSize(Heap:byte):longint;
  begin
  Result:=0;
  end;

function MemGetTotalFree(Heap:byte):longint;
  begin
  Result:=0;
  end;

function MemGetTotalSize(Heap:byte):longint;
  begin
  Result:=0;
  end;

function MemGetUnusedSize(Heap:byte):longint;
  begin
  Result:=0;
  end;

function MemGetFragmentCount(Heap:byte):longint;
  begin
  Result:=0;
  end;

function Get_HeapStatus:THeapStatus;
  begin
  {$IFDEF FPC}
    GetHeapStatus(Result);
  {$ELSE}
    Result:=GetHeapStatus;
  {$ENDIF}
  end;

function Get_MemoryInUse:longint;
  begin
  {$IFDEF FPC}
    Result:=Get_HeapStatus.CurrHeapUsed;
  {$ELSE}
    Result:=GetHeapStatus.TotalAllocated;
  {$ENDIF}
  end;

procedure Init_Mem;
  begin
  end;

{$ENDIF}
end.
