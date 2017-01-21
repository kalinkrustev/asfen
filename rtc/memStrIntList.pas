{
  "Balanced Binary search List" - Copyright (c) Danijel Tkalcec
  @exclude
}

unit memStrIntList;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils, memPtrPool;

type
  itemType=string;
  infoType=longint;

type
  pnode=^tnode;
  tnode=packed record
    key:itemType;
    info:infoType;
    b:boolean;
    l,r:pnode;
    end;

  pnodearr=^tnodearr;
  tnodearr=array[0..(MaxLongInt div SizeOf(tnode))-1] of tnode;

  pParentList=^tParentList;
  tParentList=record
    NodeCount:byte;
    Nodes:array[0..100] of pnode;
    end;

  tStrIntList=class(tObject)
  private
    myPoolSize:longint;
    myPools:array of pointer;
    pool:tPtrPool;
    cnt:longint;
    head,z:pnode;
    Parents:pParentList;
    procedure del_node(node:pnode);
    function new_node(const k:itemType; const i:infoType; const bi:boolean; const ll,rr:pnode):pnode;

  public
    constructor Create(size:integer);
    destructor Destroy; override;

    function empty:boolean;

    function Count:longint;

    procedure PoolSize(size:integer);

    function search(const v:itemType):infoType;      // Search for exact "v"
    function search_min(var i:infoType):itemType;
    function search_max(var i:infoType):itemType;
    function search_l(const v:itemType; var i:infoType):itemType;  // Search index lower than "v"
    function search_g(const v:itemType; var i:infoType):itemType;  // Search index higher than "v"
    function search_le(const v:itemType; var i:infoType):itemType;  // Search index for lower or equel to "v"
    function search_ge(const v:itemType; var i:infoType):itemType;  // Search index for higher or equal to "v"

    procedure change(const v:itemType;const info:infoType);
    procedure insert(const v:itemType;const info:infoType);
    procedure remove(const v:itemType);
    procedure removeall;
  public
    property RootNode:pnode read head;
    property NilNode:pnode read z;
    end;

implementation

const
  itemMin='';
  infoNil=-1;

function tStrIntList.Empty;
  begin
  Result:=head^.r=z;
  end;

function tStrIntList.New_Node;
  var
    a:longint;
    p:pnodearr;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      GetMem(p,SizeOf(tnode)*myPoolSize); // Create new list

      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      myPools[length(myPools)-1]:=p; // store list

      pool.Size:=pool.Size+ myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@p^[a]); // store pointers to records inside pool

      Result:=pool.Get;
      end;
    end
  else
    GetMem(Result,SizeOf(tnode));
  FillChar(Result^,SizeOf(tnode),0);
  with Result^ do
    begin
    key:=k;
    info:=i;
    l:=ll;
    r:=rr;
    b:=bi;
    end;
  end;

procedure tStrIntList.PoolSize;
// PoolSize;
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tStrIntList.Del_Node;
// del_node
  begin
  node^.key:=itemMin;
  node^.info:=infoNil;
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

constructor tStrIntList.Create;
// Create
  begin
  inherited Create;
  cnt:=0;
  myPoolSize:=size;
  pool:=tPtrPool.Create;
  z:=new_node(itemMin,infoNil,false,nil,nil);
  z^.l:=z; z^.r:=z;
  head:=new_node(itemMin,infoNil,false,z,z);
  New(Parents);
  end;

procedure tStrIntList.Change;
// Change
  var
    x:pnode;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  x^.info:=info;
  end;

procedure tStrIntList.RemoveAll;
// RemoveAll
  procedure RemoveThis(var t:pnode);
    begin
    if t^.l<>z then RemoveThis(t^.l);
    if t^.r<>z then RemoveThis(t^.r);
    del_node(t);
    t:=z;
    end;
  begin
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=infoNil;
  head^.key:=itemMin;
  cnt:=0;
  end;

destructor tStrIntList.Destroy;
// Destroy;
  var
    a:longint;
  begin
  RemoveAll;

  Dispose(Parents);

  if assigned(head) then
    del_node(head);

  if assigned(z) then
    del_node(z);

  for a:=0 to Length(myPools)-1 do
    FreeMem(myPools[a]);
  SetLength(myPools,0);
  pool.destroy;

  inherited;
  end;

function tStrIntList.Search;
// Search
  var
    x:pnode;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tStrIntList.Search_Min;
// Search_Min
  var
    x:pnode;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.l<>z do x:=x^.l;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=infoNil;
    Result:=itemMin;
    end;
  end;

function tStrIntList.Search_Max;
// Search_Max
  var
    x:pnode;
  begin
  x:=head^.r;
  if x<>z then
    begin
    while x^.r<>z do x:=x^.r;
    i:=x^.info;
    Result:=x^.key;
    end
  else
    begin
    i:=infoNil;
    Result:=itemMin;
    end;
  end;

function tStrIntList.Search_L;
// Search_L
  var
    x,y:pnode;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      begin
      if (x^.key=v) and (x^.l<>z) then y:=x^.l;
      x:=x^.l;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tStrIntList.Search_G;
// Search_G
  var
    x,y:pnode;
  begin
  x:=head^.r; y:=head;
  while x<>z do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      begin
      if (x^.key=v) and (x^.r<>z) then y:=x^.r;
      x:=x^.r;
      end;
    end;
  Result:=y^.key;
  i:=y^.info;
  end;

function tStrIntList.Search_LE;
// Search_LE
  var
    x,y:pnode;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key<v) then
      begin
      y:=x;
      x:=x^.r;
      end
    else
      x:=x^.l;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

function tStrIntList.Search_GE;
// Search_GE
  var
    x,y:pnode;
  begin
  x:=head^.r; y:=head;
  while (x<>z) and (v<>x^.key) do
    begin
    if (x^.key>v) then
      begin
      y:=x;
      x:=x^.l;
      end
    else
      x:=x^.r;
    end;
  if x<>z then
    begin
    Result:=x^.key;
    i:=x^.info;
    end
  else
    begin
    Result:=y^.key;
    i:=y^.info;
    end;
  end;

procedure tStrIntList.Insert;
// Insert
  var
    nx,x,p,g,gg,c:pnode;
  procedure split;
    procedure p_rotate_g;
      begin
      c:=p;
      if (v<c^.key) then
        begin
        p:=c^.l;
        c^.l:=p^.r;
        p^.r:=c;
        end
      else
        begin
        p:=c^.r;
        c^.r:=p^.l;
        p^.l:=c;
        end;
      if (v<g^.key) then g^.l:=p else g^.r:=p;
      end;
    procedure x_rotate_gg;
      begin
      c:=g;
      if (v<c^.key) then
        begin
        x:=c^.l;
        c^.l:=x^.r;
        x^.r:=c;
        end
      else
        begin
        x:=c^.r;
        c^.r:=x^.l;
        x^.l:=c;
        end;
      if (v<gg^.key) then gg^.l:=x else gg^.r:=x;
      end;
    begin
    x^.b:=true;
    x^.l^.b:=false;
    x^.r^.b:=false;
    if (p^.b) then
      begin
      g^.b:=true;
      if (v<g^.key)<>(v<p^.key) then p_rotate_g;
      x_rotate_gg;
      x^.b:=false;
      end;
    head^.r^.b:=false;
    end;

  begin
  nx:=new_node(v,info,True,z,z);

  // Key Sort
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v<x^.key) then x:=x^.l else x:=x^.r;
    if (x^.l^.b and x^.r^.b) then split;
    end;
  x:=nx;
  if (v<p^.key) then p^.l:=x else p^.r:=x;
  split;

  Inc(cnt);
  end;

procedure tStrIntList.Remove;
// Remove
  var
    cb:boolean;
    c,p,g,y,p2,x,t:pnode;
  procedure AddParentNode(node:pnode);
    begin
    if node<>nil then
      with Parents^ do
        begin
        Nodes[NodeCount]:=node;
        Inc(NodeCount);
        end;
    end;
  function GetParentNode:pnode;
    begin
    with Parents^ do
      if NodeCount=0 then
        Result:=z
      else
        begin
        Dec(NodeCount);
        Result:=Nodes[NodeCount];
        end;
    end;
  procedure InitParentNodes;
    begin
    Parents^.NodeCount:=0;
    end;
  procedure SwapParentNode;
    var
      a:byte;
    begin
    with Parents^ do
      for a:=0 to NodeCount-1 do
        if Nodes[a]=t then
          begin
          Nodes[a]:=c;
          Break;
          end;
    end;
  procedure deleteFixup;
    procedure p_rotateLeft_g;
      begin
      AddParentNode(g);
      p^.r := y^.l;
      if (p = g^.r) then g^.r := y else g^.l := y;
      y^.l := p;
      g:=y; y:=p^.r;
      end;
    procedure p_rotateRight_g;
      begin
      AddParentNode(g);
      p^.l := y^.r;
      if (p = g^.l) then g^.l := y else g^.r := y;
      y^.r := p;
      g:=y; y:=p^.l;
      end;
    procedure y_rotateRight_p;
      begin
      c := y^.l;
      y^.l := c^.r;
      if (p^.r = y) then p^.r := c else p^.l := c;
      c^.r := y;
      y := p^.r;
      end;
    procedure y_rotateLeft_p;
      begin
      c := y^.r;
      y^.r := c^.l;
      if (p^.l = y) then p^.l := c else p^.r := c;
      c^.l := y;
      y := p^.l;
      end;
    begin
    p:=GetParentNode;
    g:=GetParentNode;
    while (x <> head^.r) and (x^.b = false) do
      begin
      if (x = p^.l) then
        begin
        y:=p^.r;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          p_rotateLeft_g;
          end;
        if (y^.l^.b = false) and (y^.r^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r^.b = false) then
            begin
            y^.l^.b := false;
            y^.b := true;
            y_rotateRight_p;
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.r^.b := false;
          p_rotateLeft_g;
          x:=head^.r;
          break;
          end;
        end
      else
        begin
        y:=p^.l;
        if (y^.b = true) then
          begin
          y^.b := false;
          p^.b := true;
          p_rotateRight_g;
          end;
        if (y^.r^.b = false) and (y^.l^.b = false) then
          begin
          y^.b := true;
          x := p; p := g; g := GetParentNode;
          end
        else
          begin
          if (y^.l^.b = false) then
            begin
            y^.r^.b := false;
            y^.b := true;
            y_rotateLeft_p;
            end;
          y^.b := p^.b;
          p^.b := false;
          y^.l^.b := false;
          p_rotateRight_g;
          x:=head^.r;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b := false;
    end;

  begin
  InitParentNodes;

  p:=head; t:=head^.r;
  AddParentNode(p);
  while (t<>z) and (v<>t^.key) do
    begin
    p:=t;
    AddParentNode(p);
    if (v<t^.key) then t:=t^.l else t:=t^.r;
    end;

  if t=z then
    raise Exception.Create('Key not found !');

  if (t^.r=z) then
    begin
    cb:=t^.b;
    x:=t^.l;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else if (t^.l=z) then
    begin
    cb:=t^.b;
    x:=t^.r;
    if (p^.l=t) then p^.l:=x else p^.r:=x;
    end
  else
    begin
    p2:=p; c:=t^.r;
    if c^.l=z then
      begin
      AddParentNode(c);
      x:=c^.r;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end
    else
      begin
      AddParentNode(t);
      repeat
        AddParentNode(c); p:=c;
        c:=c^.l;
        until c^.l=z;
      SwapParentNode;
      x:=c^.r; p^.l:=x;
      cb:=c^.b;
      c^.b:=t^.b;
      c^.l:=t^.l;
      c^.r:=t^.r;
      if p2^.l=t then p2^.l:=c else p2^.r:=c;
      end;
    end;
  if cb=false then deleteFixup;

  del_node(t);

  Dec(cnt);
  end;

function tStrIntList.Count: longint;
  begin
  Result:=cnt;
  end;

end.
