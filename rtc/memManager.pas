{
  "Memory Heap Manager" - Copyright (c) Danijel Tkalcec
  @exclude
}

unit memManager;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils, rtcSyncObjs,

  memBinTree;

type
  THeapInfo=packed record
    minsize,         // Minimum Heap Block Size
    blocksize,       // Heap Block Size Increment
    alloc,           // Memory Allocation Unit
    pool:cardinal;   // Free blocks Pool Size
    free,            // want to free empty Heap blocks?
    freeany,         // want to free empty Heap Block even if it is not the last block? (could unused memory holes)
    organize,        // want to reorganize empty Heap blocks?
    sysmem:boolean;  // want to use SysGetMem?
    end;

const
  Min_MemFreeBlockSize = 6; // Blocks smaller than this will be "swallowed" by neighbouring block, to minimize fragmentation

  MaxHeaps=1048; // Maximum number of heaps to use. 1024 heaps give us at least 1 GB allocation space
  MinAllocUnit=1024;
  HeapAllocUnit=1024;

  StdHeap_Info:THeapInfo = (minsize:2024; blocksize:1024; alloc:4; pool:128;
                            Free:True; FreeAny:True; Organize:True; SysMem:True);

type
  tHeapDataStreamHandle=packed record
		TotalSize:cardinal;      // Data size in File
		UnusedSize:cardinal;     // Unused bytes in "Data"
    Fragments:cardinal;        // Number of Unused Fragments
		end;

	tHeapDataBlockHeader=packed record
    BlockSize:cardinal; // Real Data Block size
    Unused:byte;
    end;

{ ... Heap management ...
  A Heap is a managed chunk of memory.
  Heap bock is internally allocated as 1 memory block
  and has the ability to reserve and free smaller blocks inside it,
  providing memory mamangement for the memory allocated in that block. }

  tMyMemory = array[1 .. MaxLongInt] of byte;
  pMyMemory = ^tMyMemory;

	tHeap_Manager=class
	private
    fSysMem:boolean;
    FFree:tBinTree;
    FSS:pMyMemory;

    procedure CheckDataBlock(Block:cardinal;var FDataHead:tHeapDataBlockHeader);
    procedure SetBlockIsUsed(Block:cardinal;FDataHead:tHeapDataBlockHeader);

    procedure AddFreeBlock(Block,Size:cardinal);
    procedure DelFreeBlock(Block:cardinal);
    procedure EditFreeBlock(Block,Size:cardinal);
    procedure ChangeFreeBlock(Old_Block,New_Block,Size:cardinal);

    procedure SetBlockIsFree(Block,Size:cardinal);
    function GetFirstFreeBlock(MinSize,MaxSize:cardinal;var Size:cardinal):cardinal;
  protected
		FHead:tHeapDataStreamHandle;

	public
    FSS_Size:cardinal;
    FSS2:cardinal;

		constructor Create(Size,FreeBlocksPool:cardinal; UseSysGetMem:boolean);
		destructor Destroy; override;

    function GetTotalSize:cardinal;
    function GetUnusedSize:cardinal;
    function GetBufferSize:cardinal;
    function GetFragmentCount:cardinal;
    function GetTotalFree:cardinal;

    function GetBlock(Size:cardinal):pointer;
    function FreeBlock(Loc:pointer):cardinal;
    function isBlock(Loc:pointer):boolean;

    function BlockSize(Loc:pointer):cardinal;
		end;

{ ... Memory management ...
  Memory Manager's job is to manages a number of Heaps to make memory
  allocation and deallocation dynamic and still transparent to the user.
  Memory manager will create heeps when needed and free them when they are not needed. }

  tMem_Manager=class
  private
    MM:array[1..MaxHeaps] of tHeap_Manager;

  public
    HeapInfo:THeapInfo;
    NumHeaps:cardinal;

    Total_Alloc,
    Total_AddrSpace:cardinal;

    constructor Create(UseSysGetMem:boolean);
    destructor Destroy; override;

    function Free_Mem(p:pointer):boolean;
    function Get_Mem(size:cardinal):pointer;
    function Check_Mem(P: Pointer):cardinal;

    function GetTotalFree(Heap:byte):cardinal;
    function GetBufferSize(Heap:byte):cardinal;
    function GetTotalSize(Heap:byte):cardinal;
    function GetUnusedSize(Heap:byte):cardinal;
    function GetFragmentCount(Heap:byte):cardinal;

    function Clear:boolean;
    end;

implementation

const
	DS_DSize=SizeOf(tHeapDataBlockHeader);

{$IFDEF MSWINDOWS}
const
  kernel = 'kernel32.dll';

function LocalAlloc(flags, size: cardinal): Pointer; stdcall;
  external kernel name 'LocalAlloc';
function LocalFree(addr: Pointer): Pointer; stdcall;
  external kernel name 'LocalFree';

function myGetMem(size:cardinal):pointer;
  begin
  Result:=LocalAlloc(0,size);
  end;

procedure myFreeMem(p:pointer);
  begin
  LocalFree(p);
  end;
{$ELSE}
function myGetMem(size:cardinal):pointer;
  begin
  Result:=SysGetMem(size);
  end;

procedure myFreeMem(p:pointer);
  begin
  SysFreeMem(size);
  end;
{$ENDIF}

{ MemoryManager }

constructor tHeap_Manager.Create(Size,FreeBlocksPool:cardinal;UseSysGetMem:boolean);
	begin
  inherited Create;

  if UseSysGetMem then
    FSS:=MyGetMem(Size)
  else
    FSS:=SysGetMem(Size);

  if FSS=nil then
    OutOfMemoryError;

  fSysMem:=UseSysGetMem;

  FSS_Size:=Size;

  FillChar(FHead,SizeOf(FHead),0);

  FSS2:=cardinal(FSS);

  FFree:=tBinTree.Create(FreeBlocksPool);
	end;

destructor tHeap_Manager.Destroy;
	begin
  FFree.Free;
  if assigned(FSS) and (FSS_Size>0) then
    begin
    if fSysMem then
      myFreeMem(FSS)
    else
      SysFreeMem(FSS);
    FSS:=nil;
    FSS_Size:=0;
    end;
	inherited;
	end;

(* PRIVATE METHODS *)

procedure tHeap_Manager.CheckDataBlock(Block:cardinal;var FDataHead:tHeapDataBlockHeader);
	begin
  Move(FSS^[Block],FDataHead,DS_DSize);
	end;

procedure tHeap_Manager.SetBlockIsUsed(Block:cardinal;FDataHead:tHeapDataBlockHeader);
	begin
  Move(FDataHead,FSS^[Block],DS_DSize);
	end;

(****************************************************************************)

procedure tHeap_Manager.AddFreeBlock(Block,Size:cardinal);
  begin
  if FFree.search(Block)<>0 then
    raise Exception.Create('Access Violation: Memory block to be released allready free.');
  FFree.insert(Block,Size);
  Inc(FHead.Fragments);
  end;

procedure tHeap_Manager.DelFreeBlock(Block:cardinal);
  begin
  if FFree.search(Block)=0 then
    raise Exception.Create('Access Violation: Memory block to be reserved is not free.');
  FFree.remove(Block);
  Dec(FHead.Fragments);
  end;

procedure tHeap_Manager.EditFreeBlock(Block,Size:cardinal);
  begin
  if FFree.search(Block)=0 then
    raise Exception.Create('Access Violation: Memory block to be resized is not free.');
  FFree.remove(Block);
  FFree.insert(Block,Size);
  end;

procedure tHeap_Manager.ChangeFreeBlock(Old_Block,New_Block,Size:cardinal);
  begin
  if FFree.search(Old_Block)=0 then
    raise Exception.Create('Access Violation: Memory block to be resized is not free.');
  FFree.remove(Old_Block);
  FFree.insert(New_Block,Size);
  end;

(*************************************************************************)

procedure tHeap_Manager.SetBlockIsFree(Block,Size:cardinal);
	var
		FE_NextBlock,FE_BlockSize:cardinal;
		s,Tmp:cardinal;
    Loc,Blck:cardinal;
	procedure SetNewFreeSize;
		begin
		Inc(Size,FE_BlockSize);
		if Blck+Size>FHead.TotalSize then
			begin
      DelFreeBlock(Blck);
			Dec(FHead.UnusedSize,Size);
			Dec(FHead.TotalSize,Size);
			end;
		end;
  procedure AddTheBlock;
    begin
    Blck:=Block;
    AddFreeBlock(Block,Size);
    Inc(FHead.UnusedSize,Size);
    SetNewFreeSize;
    end;
	begin
  if FHead.Fragments>0 then
    begin
    Blck:=FFree.search_le(Block,FE_BlockSize);
    if (FE_BlockSize>0) and (Blck+FE_BlockSize=Block) then  // There is a block Left from us (linked)
      begin
      FE_NextBlock:=FFree.search_g(Blck,Tmp);
      s:=Size;
      if FE_NextBlock=Block+Size then // There's a Block Right to us (linked)
        begin
        Inc(Size,FE_BlockSize);
        Loc:=FE_NextBlock;
        FE_BlockSize:=Tmp;
        DelFreeBlock(Loc);
        end;
      EditFreeBlock(Blck,Size+FE_BlockSize);
      Inc(FHead.UnusedSize,s);
      SetNewFreeSize;
      end
    else if (FE_BlockSize>0) and (Blck+FE_BlockSize>Block) then // Memory allready freed
      begin
      raise Exception.Create('Memory allready released.');
      end
    else
      begin
      Blck:=Block;
      FE_NextBlock:=FFree.search_g(Blck,FE_BlockSize);
      if FE_NextBlock=Block+Size then // There's a block Right to us (linked)
        begin
        ChangeFreeBlock(FE_NextBlock,Blck,Size+FE_BlockSize);
        Inc(FHead.UnusedSize,Size);
        SetNewFreeSize;
        end
      else // No blocks near us.
        begin
        FE_BlockSize:=0;
        AddTheBlock;
        end;
      end;
    end
  else
    begin
    FE_BlockSize:=0;
    AddTheBlock;
    end;
	end;

function tHeap_Manager.GetFirstFreeBlock(MinSize,MaxSize:cardinal;var Size:cardinal):cardinal;
	var
		Block,Loc:cardinal;
    Size_X,Siz:cardinal;
		FE_BlockSize:cardinal;
    myBlock:tHeapDataBlockHeader;
  function GetTheBlock:cardinal;
    begin
    Size:=MaxSize;
    Size_X:=MaxSize+DS_DSize;

    Block:=FHead.TotalSize+1;
    if FSS_Size<Block+Size_X then
      Result:=0
    else
      begin
      Inc(FHead.TotalSize,Size_X);
      myBlock.BlockSize:=Size;
      myBlock.Unused:=0;
      SetBlockIsUsed(Block,myBlock);
      Result:=Block+DS_DSize;
      end;
    end;
	begin
  if FHead.Fragments>0 then
    begin
    if MinSize>MaxSize then MinSize:=MaxSize;
    Size_X:=MinSize+DS_DSize;

    Siz:=FFree.isearch_ge(Size_X,Loc);
    if Siz>=Size_X then
      begin
      Block:=Loc;
      FE_BlockSize:=Siz;

      Dec(FHead.UnusedSize,FE_BlockSize);
      DelFreeBlock(Block);

      if FE_BlockSize>=MaxSize+DS_DSize+Min_MemFreeBlockSize then
        begin
        Size:=MaxSize;
        Size_X:=Size+DS_DSize;
        SetBlockIsFree(Block+Size_X,FE_BlockSize-Size_X);
        end
      else
        begin
        Size_X:=FE_BlockSize;
        Size:=Size_X-DS_DSize;
        if Size>MaxSize then Size:=MaxSize;
        end;
      myBlock.BlockSize:=Size;
      myBlock.Unused:=Size_X-Size-DS_DSize;
      SetBlockIsUsed(Block,myBlock);
      Result:=Block+DS_DSize;
      end
    else
      Result:=GetTheBlock;
    end
  else
    Result:=GetTheBlock;
	end;

(* GLOBAL METHODS *)

function tHeap_Manager.GetTotalSize:cardinal;
  begin
  Result:=FHead.TotalSize;
  end;

function tHeap_Manager.GetUnusedSize:cardinal;
  begin
  Result:=FHead.UnusedSize;
  end;

function tHeap_Manager.GetBufferSize:cardinal;
  begin
  Result:=FSS_Size-FHead.TotalSize;
  end;

function tHeap_Manager.GetTotalFree:cardinal;
  begin
  Result:=GetBufferSize+GetUnusedSize;
  end;

function tHeap_Manager.GetFragmentCount:cardinal;
  begin
  Result:=FHead.Fragments;
  end;

function tHeap_Manager.GetBlock(Size:cardinal):pointer;
  var
    Block:cardinal;
	begin
	Block:=GetFirstFreeBlock(Size,Size,Size);
  if Block>0 then Result:=Addr(FSS^[Block]) else Result:=nil;
	end;

function tHeap_Manager.FreeBlock(Loc:pointer):cardinal;
	var
		FD:tHeapDataBlockHeader;
    Block:cardinal;
	begin
  if (cardinal(Loc)<FSS2) or
     (cardinal(Loc)>=FSS2+FSS_Size) then
    begin
    Result:=0;
    Exit;
    end;

  Block:=cardinal(Loc)-cardinal(FSS)+1;

	Dec(Block,DS_DSize);
	CheckDataBlock(Block,FD);
 	SetBlockIsFree(Block,FD.BlockSize+DS_DSize+FD.Unused);

  Result:=FD.BlockSize;
	end;

function tHeap_Manager.isBlock(Loc:pointer):boolean;
  begin
  Result:=(cardinal(Loc)>=FSS2) and
          (cardinal(Loc)<FSS2+FSS_Size);
  end;

function tHeap_Manager.BlockSize(Loc:pointer):cardinal;
  var
    FD:tHeapDataBlockHeader;
    Block:cardinal;
	begin
  Block:=cardinal(Loc)-cardinal(FSS2)+1;
 	CheckDataBlock(Block-DS_DSize,FD);
  Result:=FD.BlockSize;
	end;

{ tMem_Manager }

function roundto(a,b:cardinal):cardinal;
  begin
  Result:=(a+(b-1)) div b  * b;
  end;

constructor tMem_Manager.Create(UseSysGetMem:boolean);
  begin
  inherited Create;
  Total_Alloc:=0;
  Total_AddrSpace:=0;
  HeapInfo:=StdHeap_Info;
  NumHeaps:=0;
  end;

destructor tMem_Manager.Destroy;
  begin
  Clear;
  inherited;
  end;

function tMem_Manager.Clear:boolean;
  begin
  while NumHeaps>0 do
    begin
    MM[NumHeaps].Free;
    Dec(NumHeaps);
    end;
  Result:=True;
  end;

function tMem_Manager.GetBufferSize(Heap: byte): cardinal;
  var
    a:cardinal;
  begin
  if Heap=0 then
    begin
    Result:=0;
    for a:=1 to NumHeaps do
      Result:=Result+MM[a].GetBufferSize;
    end
  else if Heap<=NumHeaps then
    Result:=MM[Heap].GetBufferSize
  else
    Result:=0;
  end;

function tMem_Manager.GetTotalFree(Heap: byte): cardinal;
  var
    a:cardinal;
  begin
  if Heap=0 then
    begin
    Result:=0;
    for a:=1 to NumHeaps do
      Result:=Result+MM[a].GetTotalFree;
    end
  else if Heap<=NumHeaps then
    Result:=MM[Heap].GetTotalFree
  else
    Result:=0;
  end;

function tMem_Manager.GetFragmentCount(Heap: byte): cardinal;
  var
    a:cardinal;
  begin
  if Heap=0 then
    begin
    Result:=0;
    for a:=1 to NumHeaps do
      Result:=Result+MM[a].GetFragmentCount;
    end
  else if Heap<=NumHeaps then
    Result:=MM[Heap].GetFragmentCount
  else
    Result:=0;
  end;

function tMem_Manager.GetTotalSize(Heap: byte): cardinal;
  var
    a:cardinal;
  begin
  if Heap=0 then
    begin
    Result:=0;
    for a:=1 to NumHeaps do
      Result:=Result+MM[a].GetTotalSize;
    end
  else if Heap<=NumHeaps then
    Result:=MM[Heap].GetTotalSize
  else
    Result:=0;
  end;

function tMem_Manager.GetUnusedSize(Heap: byte): cardinal;
  var
    a:cardinal;
  begin
  if Heap=0 then
    begin
    Result:=0;
    for a:=1 to NumHeaps do
      Result:=Result+MM[a].GetUnusedSize;
    end
  else if Heap<=NumHeaps then
    Result:=MM[Heap].GetUnusedSize
  else
    Result:=0;
  end;

function tMem_Manager.Free_Mem(p: pointer): boolean;
  var
    a,b:cardinal;
    TMM:tHeap_Manager;
  begin
  if p=nil then
    Result:=True
  else
    begin
    Result:=False;
    for a:=NumHeaps downto 1 do
      begin
      if MM[a].isBlock(p) then
        begin
        Total_Alloc := Total_Alloc - MM[a].FreeBlock(p);
        if NumHeaps>1 then // using NumHeaps>1 instead of NumHeaps<=0 will leave at least one heap block open, to speed up memory allocation.
          begin
          if (HeapInfo.free and (HeapInfo.freeany or (a=NumHeaps))) and
             (MM[a].GetTotalSize=0) then
            begin
            Total_AddrSpace := Total_AddrSpace - MM[a].FSS_Size;
            MM[a].Free;
            if NumHeaps>a then
              for b:=a+1 to NumHeaps do
                MM[b-1]:=MM[b];
            Dec(NumHeaps);
            end
          else if HeapInfo.organize then
            begin
            b:=a;
            while (b<NumHeaps) and
                  (MM[b].GetBufferSize>MM[b+1].GetBufferSize) do
              begin
              TMM:=MM[b]; MM[b]:=MM[b+1]; MM[b+1]:=TMM;
              Inc(b);
              end;
            end;
          end;
        Result:=True;
        Break;
        end;
      end;
    end;
  end;

function tMem_Manager.Get_Mem(size: cardinal): pointer;
  var
    a,mbsize:cardinal;
    TMM:tHeap_Manager;
  begin
  Result:=nil;

  if Size<=0 then Exit;
  Size:=roundto(Size,HeapInfo.alloc);

  a:=1;
  while a<=NumHeaps do
    begin
    Result:=MM[a].GetBlock(Size);
    if Result<>nil then
      begin
      if HeapInfo.organize then
        while (a>1) and
              (MM[a-1].GetBufferSize>MM[a].GetBufferSize) do
          begin
          TMM:=MM[a]; MM[a]:=MM[a-1]; MM[a-1]:=TMM;
          Dec(a);
          end;
      Break;
      end
    else
      Inc(a);
    end;

  if (Result=nil) and (NumHeaps<MaxHeaps) then
    begin
    mbsize:=roundto(size+MinAllocUnit,HeapInfo.blocksize*HeapAllocUnit);
    if mbsize<HeapInfo.minsize*HeapAllocUnit then
      mbsize:=HeapInfo.minsize*HeapAllocUnit;
    try
      MM[NumHeaps+1]:=tHeap_Manager.Create(mbsize,HeapInfo.pool,HeapInfo.sysmem);
    except
      Exit; // Out of Memory
      end;
    Inc(NumHeaps);
    Total_AddrSpace := Total_AddrSpace + MM[NumHeaps].FSS_Size;

    Result:=MM[NumHeaps].GetBlock(roundto(Size,HeapInfo.alloc));
    if HeapInfo.organize and (Result<>nil) then
      begin
      a:=NumHeaps;
      while (a>1) and
            (MM[a-1].GetBufferSize>MM[a].GetBufferSize) do
        begin
        TMM:=MM[a]; MM[a]:=MM[a-1]; MM[a-1]:=TMM;
        Dec(a);
        end;
      end;
    end;

  Total_Alloc := Total_Alloc + size;
  end;

function tMem_Manager.Check_Mem(P: Pointer):cardinal;
  var
    a:cardinal;
  begin
  Result:=0;
  for a:=1 to NumHeaps do
    if MM[a].isBlock(p) then
      begin
      Result:=MM[a].BlockSize(p);
      Break;
      end;
  end;

end.
