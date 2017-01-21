{
  "Socket Handle Pool" - Copyright (c) Danijel Tkalcec
  @exclude
}
unit rtcSocketPool;

{$INCLUDE rtcDefs.inc}

interface

uses
  rtcTrashcan,
  
  Windows,
  SysUtils,
  Classes,

  rtcSyncObjs,
  rtcLog,

  rtcThrPool;

procedure rtcEnterSocket;
procedure rtcLeaveSocket;

// Get socket (need to call rtcEnterSocket before & rtcLeaveSocket after)
function rtcGetSocket(sock:longword):TObject;

// Check socket (no need to call rtcEnterSocket and rtcLeaveSocket)
function rtcCheckSocket(sock:longword):TObject;

// Store socket (thread-safe)
procedure rtcStoreSocket(obj:TObject; sock:longword);
// Remove socket (thread-safe)
function rtcRemoveSocket(obj:TObject):boolean;

implementation

uses
  memBinTree;

var
  SockList:tBinTree;
  CSHWND:TRtcCritSec;

procedure rtcStoreSocket(obj:TObject; sock:longword);
  //var i:longword;
  begin
  CSHWND.Enter;
  try
    {i:=SockList.search(longword(Obj));
    if i<>0 then // object in the list
      begin
      Log('Exception! Socket not removed.');
      SockList.remove(longword(Obj));
      end;
    if SockList.isearch(sock)<>0 then
      Log('Exception! Other object using same socket!');}
    // add socket-to-object reference
    SockList.insert(longword(Obj), Sock);
  finally
    CSHWND.Leave;
    end;
  end;

function rtcRemoveSocket(obj:TObject):boolean;
  var
    i:longword;
  begin
  Result:=False;
  CSHWND.Enter;
  try
    i:=SockList.search(longword(Obj));
    if i<>0 then // object in the list
      begin
      SockList.remove(longword(Obj));
      Result:=True;
      end;
  finally
    CSHWND.Leave;
    end;
  end;

procedure rtcEnterSocket;
  begin
  CSHWND.Enter;
  end;

procedure rtcLeaveSocket;
  begin
  CSHWND.Leave;
  end;

function rtcGetSocket(Sock:longword):TObject;
  var
    i:longword;
  begin
  i:=SockList.isearch(Sock);
  if i<>0 then
    Result:=TObject(i)
  else
    Result:=nil;
  end;

function rtcCheckSocket(Sock:longword):TObject;
  var
    i:longword;
  begin
  CSHWND.Enter;
  try
    i:=SockList.isearch(Sock);
    if i<>0 then
      Result:=TObject(i)
    else
      Result:=nil;
  finally
    CSHWND.Leave;
    end;
  end;

initialization
CSHWND:=TRtcCritSec.Create;
SockList:=tBinTree.Create(256);
finalization
Garbage(SockList);
Garbage(CSHWND);
end.
