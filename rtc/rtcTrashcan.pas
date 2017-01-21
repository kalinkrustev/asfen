{
  @html(<b>)
  Trashcah
  @html(</b>)
  - Copyright (c) Danijel Tkalcec
  @html(<br><br>)

  Garbage collection used by the RTC SDK to avoid destroying objects which
  need to remain in memory until all other things have been destroyed.

  @exclude
}
unit rtcTrashcan;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,
  rtcSyncObjs;

// Add pointer "p" to garbage collector.
procedure Garbage(p:pointer); overload;

// Add object "o" to garbage collector.
procedure Garbage(o:TObject); overload;

implementation

var
  CS:TRtcCritSec;
  p_can:array of pointer;
  o_can:array of TObject;
  pcan_cnt:integer=0;
  ocan_cnt:integer=0;

// Add pointer "p" to garbage collector.
procedure Garbage(p:pointer); overload;
  begin
  if p=nil then Exit;
  CS.Enter;
  try
    Inc(pcan_cnt);
    if length(p_can)<pcan_cnt then
      SetLength(p_can, length(p_can)+32);
    p_can[pcan_cnt-1]:=p;
  finally
    CS.Leave;
    end;
  end;

// Add object "o" to garbage collector.
procedure Garbage(o:TObject); overload;
  var
    a:integer;
  begin
  if o=nil then Exit;
  CS.Enter;
  try
    for a:=0 to ocan_cnt-1 do
      if o_can[a]=o then
        raise Exception.Create('Object already added!');
    Inc(ocan_cnt);
    if length(o_can)<ocan_cnt then
      SetLength(o_can, length(o_can)+32);
    o_can[ocan_cnt-1]:=o;
  finally
    CS.Leave;
    end;
  end;

procedure CleanGarbage;
  var
    a:integer;
  begin
  CS.Enter;
  try
    for a:=0 to pcan_cnt-1 do
      FreeMem(p_can[a]);
    pcan_cnt:=0;
    for a:=0 to ocan_cnt-1 do
      o_can[a].Free;
    ocan_cnt:=0;
  finally
    CS.Leave;
    end;

  SetLength(p_can,0);
  SetLength(o_can,0);

  CS.Free;
  end;

initialization
CS:=TRtcCritSec.Create;
SetLength(p_can,0);
SetLength(o_can,0);

finalization
CleanGarbage;
end.
