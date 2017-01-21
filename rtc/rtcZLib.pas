{ @exclude
******************************************************************************
*  This is a modified version of "ZLibEx.pas" from base2 technologies        *
*                                                                            *
*  copyright (c) 2004-2007 Danijel Tkalcec                                   *
*  copyright (c) 2000-2005 base2 technologies                                *
*  copyright (c) 1997 Borland International                                  *
*                                                                            *
*  acknowledgements                                                          *
*    erik turner                                                             *
*      Z*Stream routines                                                     *
*                                                                            *
*    burak kalayci                                                           *
*      informing me about the zlib 1.1.4 update and the 1.2.1 update         *
*                                                                            *
*    vicente sánchez-alarcos                                                 *
*      informing me about the zlib 1.2.2 update                              *
*                                                                            *
*    luigi sandon                                                            *
*      pointing out the missing loop condition (Z_STREAM_END) in             *
*        ZInternalCompressStream and ZInternalDecompressStream               *
*                                                                            *
*    ferry van genderen                                                      *
*      assiting me fine tune and beta test ZInternalCompressStream and       *
*        ZInternalDecompressStream                                           *
*                                                                            *
*    mathijs van veluw                                                       *
*      informing me about the zlib 1.2.3 update                              *
*****************************************************************************}
unit rtcZlib;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils, Classes,
  memPtrPool, rtcSyncObjs;

const
  ZLIB_VERSION = '1.2.3';
  ZLIB_VERNUM  = $1230;

type
  TZAlloc = function (opaque: Pointer; items, size: Integer): Pointer;
  TZFree  = procedure (opaque, block: Pointer);

  TZCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax);

  {** TZStreamRec ***********************************************************}

  TZStreamRec = packed record
    next_in  : PChar;     // next input byte
    avail_in : Longint;   // number of bytes available at next_in
    total_in : Longint;   // total nb of input bytes read so far

    next_out : PChar;     // next output byte should be put here
    avail_out: Longint;   // remaining free space at next_out
    total_out: Longint;   // total nb of bytes output so far

    msg      : PChar;     // last error message, NULL if no error
    state    : Pointer;   // not visible by applications

    zalloc   : TZAlloc;   // used to allocate the internal state
    zfree    : TZFree;    // used to free the internal state
    opaque   : Pointer;   // private data object passed to zalloc and zfree

    data_type: Integer;   // best guess about the data type: ascii or binary
    adler    : Longint;   // adler32 value of the uncompressed data
    reserved : Longint;   // reserved for future use
  end;

  EZLibError = class(Exception)
  public
    class function New(code: Integer):EZLibError;
  end;

{** zlib public routines ****************************************************}

function ZCompress_Str(const inBuffer: String; level: TZCompressionLevel):string;

function ZDecompress_Str(const inBuffer: String; inSize:integer=0):string;

implementation

{** link zlib code **********************************************************}
{$L deflate.obj}
{$L inflate.obj}
{$L inftrees.obj}
{$L infback.obj}
{$L inffast.obj}
{$L trees.obj}
{$L compress.obj}
{$L adler32.obj}
{$L crc32.obj}

{*****************************************************************************
*  note: do not reorder the above -- doing so will result in external        *
*  functions being undefined                                                 *
*****************************************************************************}

const
  {** flush constants *******************************************************}

  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;
  Z_BLOCK         = 5;

  {** return codes **********************************************************}

  Z_OK            = 0;
  Z_STREAM_END    = 1;
  Z_NEED_DICT     = 2;
  Z_ERRNO         = (-1);
  Z_STREAM_ERROR  = (-2);
  Z_DATA_ERROR    = (-3);
  Z_MEM_ERROR     = (-4);
  Z_BUF_ERROR     = (-5);
  Z_VERSION_ERROR = (-6);

  {** compression levels ****************************************************}

  Z_NO_COMPRESSION       =   0;
  Z_BEST_SPEED           =   1;
  Z_BEST_COMPRESSION     =   9;
  Z_DEFAULT_COMPRESSION  = (-1);

  {** compression strategies ************************************************}

  Z_FILTERED            = 1;
  Z_HUFFMAN_ONLY        = 2;
  Z_RLE                 = 3;
  Z_FIXED               = 4;
  Z_DEFAULT_STRATEGY    = 0;

  {** data types ************************************************************}

  Z_BINARY   = 0;
  Z_ASCII    = 1;
  Z_TEXT     = Z_ASCII;
  Z_UNKNOWN  = 2;

  {** compression methods ***************************************************}

  Z_DEFLATED = 8;

  {** return code messages **************************************************}

  _z_errmsg: array[0..9] of PChar = (
    'need dictionary',      // Z_NEED_DICT      (2)
    'stream end',           // Z_STREAM_END     (1)
    'ok',                   // Z_OK             (0)
    'file error',           // Z_ERRNO          (-1)
    'stream error',         // Z_STREAM_ERROR   (-2)
    'data error',           // Z_DATA_ERROR     (-3)
    'insufficient memory',  // Z_MEM_ERROR      (-4)
    'buffer error',         // Z_BUF_ERROR      (-5)
    'incompatible version', // Z_VERSION_ERROR  (-6)
    ''
  );

  ZLevels: Array [TZCompressionLevel] of Shortint = (
    Z_NO_COMPRESSION,
    Z_BEST_SPEED,
    Z_DEFAULT_COMPRESSION,
    Z_BEST_COMPRESSION
  );

{** deflate routines ********************************************************}

function deflateInit_(var strm: TZStreamRec; level: Integer; version: PChar;
  recsize: Integer): Integer;
  external;

function deflate(var strm: TZStreamRec; flush: Integer): Integer;
  external;

function deflateEnd(var strm: TZStreamRec): Integer;
  external;

{** inflate routines ********************************************************}

function inflateInit_(var strm: TZStreamRec; version: PChar;
  recsize: Integer): Integer;
  external;

function inflate(var strm: TZStreamRec; flush: Integer): Integer;
  external;

function inflateEnd(var strm: TZStreamRec): Integer;
  external;

function inflateReset(var strm: TZStreamRec): Integer;
  external;

{** zlib function implementations *******************************************}

function zcalloc(opaque: Pointer; items, size: Integer): Pointer;
begin
  GetMem(result,items * size);
end;

procedure zcfree(opaque, block: Pointer);
begin
  FreeMem(block);
end;

{** c function implementations **********************************************}

procedure _memset(p: Pointer; b: Byte; count: Integer); cdecl;
begin
  FillChar(p^,count,b);
end;

procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  Move(source^,dest^,count);
end;

{** custom zlib routines ****************************************************}

function DeflateInit(var stream: TZStreamRec; level: Integer): Integer;
begin
  result := deflateInit_(stream,level,ZLIB_VERSION,SizeOf(TZStreamRec));
end;

function InflateInit(var stream: TZStreamRec): Integer;
begin
  result := inflateInit_(stream,ZLIB_VERSION,SizeOf(TZStreamRec));
end;

{****************************************************************************}

function ZCompressCheck(code: Integer): Integer;
begin
  result := code;

  if code < 0 then
  begin
    raise EZLibError.New(code);
  end;
end;

function ZDecompressCheck(code: Integer): Integer;
begin
  Result := code;

  if code < 0 then
  begin
    raise EZLibError.New(code);
  end;
end;

{
procedure ZInternalCompress(var zstream: TZStreamRec; const inBuffer: Pointer;
  inSize: Integer; out outBuffer: Pointer; out outSize: Integer);
const
  delta = 256;
begin
  outSize := ((inSize + (inSize div 10) + 12) + 255) and not 255;
  GetMem(outBuffer,outSize);

  try
    try
      zstream.next_in := inBuffer;
      zstream.avail_in := inSize;
      zstream.next_out := outBuffer;
      zstream.avail_out := outSize;

      while ZCompressCheck(deflate(zstream,Z_FINISH)) <> Z_STREAM_END do
      begin
        Inc(outSize,delta);
        ReallocMem(outBuffer,outSize);

        zstream.next_out := PChar(Integer(outBuffer) + zstream.total_out);
        zstream.avail_out := delta;
      end;
    finally
      ZCompressCheck(deflateEnd(zstream));
    end;

    ReallocMem(outBuffer,zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

procedure ZInternalDecompress(zstream: TZStreamRec; const inBuffer: Pointer;
  inSize: Integer; out outBuffer: Pointer; out outSize: Integer;
  outEstimate: Integer);
var
  delta: Integer;
begin
  delta := (inSize + 255) and not 255;

  if outEstimate = 0 then outSize := delta
  else outSize := outEstimate;

  GetMem(outBuffer,outSize);

  try
    try
      zstream.next_in := inBuffer;
      zstream.avail_in := inSize;
      zstream.next_out := outBuffer;
      zstream.avail_out := outSize;

      while ZDecompressCheck(inflate(zstream,Z_NO_FLUSH)) <> Z_STREAM_END do
      begin
        Inc(outSize,delta);
        ReallocMem(outBuffer,outSize);

        zstream.next_out := PChar(Integer(outBuffer) + zstream.total_out);
        zstream.avail_out := delta;
      end;
    finally
      ZDecompressCheck(inflateEnd(zstream));
    end;

    ReallocMem(outBuffer,zstream.total_out);
    outSize := zstream.total_out;
  except
    FreeMem(outBuffer);
    raise;
  end;
end;

procedure ZCompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer;
  level: TZCompressionLevel);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);

  ZCompressCheck(DeflateInit(zstream,ZLevels[level]));

  ZInternalCompress(zstream,inBuffer,inSize,outBuffer,outSize);
end;

procedure ZDecompress(const inBuffer: Pointer; inSize: Integer;
  out outBuffer: Pointer; out outSize: Integer; outEstimate: Integer);
var
  zstream: TZStreamRec;
begin
  FillChar(zstream,SizeOf(TZStreamRec),0);

  ZDecompressCheck(InflateInit(zstream));

  ZInternalDecompress(zstream,inBuffer,inSize,outBuffer,outSize,outEstimate);
end;
}

{** string routines *********************************************************}

const
  delta=1024;

{$IFDEF zLibPool}
var
  CS:TRtcCritSec;
  pool:tPtrPool=nil;
  p_first,p_last:pointer;

function GetPoolBlock:pointer;
  begin
  CS.Enter;
  try
    Result:=pool.Get;
    if Result=nil then
      GetMem(Result, delta);
  finally
    CS.Leave;
    end;
  end;

procedure PutPoolBlock(p:pointer);
  begin
  CS.Enter;
  try
    if (longword(p)>=longword(p_first)) and (longword(p)<=longword(p_last)) then
      pool.Put(p)
    else
      FreeMem(p);
  finally
    CS.Leave;
    end;
  end;

procedure MakePoolBlocks;
  var
    a:integer;
  begin
  CS.Enter;
  try
    if assigned(pool) then Exit;

    pool:=tPtrPool.Create(8192);

    GetMem(p_first, delta*pool.size);
    p_last:=p_first;
    for a:=0 to pool.size-1 do
      begin
      pool.Put(p_last);
      p_last:=pointer(longint(p_last)+delta);
      end;
  finally
    CS.Leave;
    end;
  end;

procedure FreePoolBlocks;
  begin
  CS.Enter;
  try
    if assigned(pool) then
      begin
      FreeMem(p_first);
      pool.Free;
      pool:=nil;
      end;
  finally
    CS.Leave;
    end;
  end;

{$ELSE}

function GetPoolBlock:pointer;
  begin
  GetMem(Result, delta);
  end;

procedure PutPoolBlock(p:pointer);
  begin
  FreeMem(p);
  end;

procedure MakePoolBlocks;
  begin
  end;

{$ENDIF}

function ZCompress_Str(const inBuffer: String; level: TZCompressionLevel):string;
  var
    SL:array of pointer;
    loc,a,slcount:integer;
    zstream: TZStreamRec;
  begin
  FillChar(zstream, SizeOf(TZStreamRec), 0);

  MakePoolBlocks;

  Result:='';

  slcount:=0;
  SetLength(SL,32);
  try
    zstream.next_in := @inBuffer[1];
    zstream.avail_in := length(inBuffer);

    Inc(slcount);
    SL[slcount-1]:=GetPoolBlock;

    zstream.next_out := SL[slcount-1];
    zstream.avail_out := delta;

    ZCompressCheck(DeflateInit(zstream, ZLevels[level]));

    try
      while ZCompressCheck(deflate(zstream, Z_FINISH)) <> Z_STREAM_END do
      begin
        Inc(slcount);
        if slcount>length(SL) then
          SetLength(SL, length(SL)+32);
        SL[slcount-1]:=GetPoolBlock;

        zstream.next_out := SL[slcount-1];
        zstream.avail_out := delta;
      end;
    finally
      ZCompressCheck(deflateEnd(zstream));
    end;

    loc:=1;
    SetLength(Result, zstream.total_out);
    if slcount>0 then
      begin
      for a:=0 to slcount-2 do
        begin
        Move(SL[a]^,Result[loc],delta);
        Inc(loc,delta);
        end;
      Move(SL[slcount-1]^,Result[loc], length(Result)-loc+1);
      end;
  finally
    if slcount>0 then
      for a:=0 to slcount-1 do
        begin
        PutPoolBlock(SL[a]);
        SL[a]:=nil;
        end;
    SetLength(SL,0);
    end;
  end;

function ZDecompress_Str(const inBuffer: String; inSize:integer):string;
  var
    SL:array of pointer;
    loc,a,slcount:integer;
    zstream: TZStreamRec;
  begin
  if inSize=0 then
    inSize:=length(inBuffer)
  else if inSize>length(inBuffer) then
    raise Exception.Create('Error! Can not decompress more than received.');

  FillChar(zstream, SizeOf(TZStreamRec), 0);

  MakePoolBlocks;

  Result:='';

  slcount:=0;
  SetLength(SL, 32);
  try
    zstream.next_in := @inBuffer[1];
    zstream.avail_in := inSize;

    Inc(slcount);
    SL[slcount-1]:=GetPoolBlock;

    zstream.next_out := SL[slcount-1];
    zstream.avail_out := delta;

    ZDecompressCheck(InflateInit(zstream));

    try
      while ZDecompressCheck(inflate(zstream, Z_NO_FLUSH)) <> Z_STREAM_END do
      begin
        Inc(slcount);
        if slcount>length(SL) then
          SetLength(SL, length(SL)+32);
        SL[slcount-1]:=GetPoolBlock;

        zstream.next_out := SL[slcount-1];
        zstream.avail_out := delta;
      end;
    finally
      ZDecompressCheck(inflateEnd(zstream));
    end;

    loc:=1;
    SetLength(Result, zstream.total_out);
    if slcount>0 then
      begin
      for a:=0 to slcount-2 do
        begin
        Move(SL[a]^,Result[loc],delta);
        Inc(loc,delta);
        end;
      Move(SL[slcount-1]^,Result[loc], length(Result)-loc+1);
      end;
  finally
    if slcount>0 then
      for a:=0 to slcount-1 do
        begin
        PutPoolBlock(SL[a]);
        SL[a]:=nil;
        end;
    SetLength(SL,0);
    end;
  end;

{** EZLibError **************************************************************}

class function EZLibError.New(code: Integer):EZLibError;
  begin
  Result:=Create(_z_errmsg[2 - code]);
  end;

{$IFDEF zLibPool}
initialization
CS:=TRtcCritSec.Create;
finalization
FreePoolBlocks;
Garbage(CS);
{$ENDIF}
end.
