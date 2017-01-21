{
  "Balanced Binary Search Tree" - Copyright (c) Danijel Tkalcec
  @exclude
}

unit memBinTree;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils, memPtrPool;

type
  itemType=longword;
  infoType=longword;

type
  pnode=^tnode;
  tnode=record
    key:itemType;
    info:infoType;
    b,b2:boolean;
    l,l2,r,r2:pnode;
    end;

  pnodearr=^tnodearr;
  tnodearr=array[0..(MaxLongInt div SizeOf(tnode))-1] of tnode;

  pParentList=^tParentList;
  tParentList=record
    NodeCount:byte;
    Nodes:array[0..100] of pnode;
    end;

  tBinTree=class(tObject)
  private
    myPoolSize:longint;
    myPools:array of pointer;
    pool:tPtrPool;
    cnt:cardinal;
    head,z:pnode;
    Parents:pParentList;
    procedure del_node(node:pnode);
    function new_node(const k:itemType; const i:infoType; const bi:boolean; const ll,rr:pnode):pnode;
  public
    constructor Create(size:integer);
    destructor Destroy; override;

    function empty:boolean;

    function Count:cardinal;

    procedure PoolSize(size:integer);

    function search(const v:itemType):infoType;      // Search for exact "v"
    function search_min(var i:infoType):itemType;
    function search_max(var i:infoType):itemType;
    function search_l(const v:itemType; var i:infoType):itemType;  // Search index lower than "v"
    function search_g(const v:itemType; var i:infoType):itemType;  // Search index higher than "v"
    function search_le(const v:itemType; var i:infoType):itemType;  // Search index for lower or equel to "v"
    function search_ge(const v:itemType; var i:infoType):itemType;  // Search index for higher or equal to "v"

    function isearch(const v:infoType):itemType;      // Search for info, exact "v"
    function isearch_min(var i:itemType):infoType;
    function isearch_max(var i:itemType):infoType;
    function isearch_l(const v:infoType; var i:itemType):infoType;  // Search for info lower than "v"
    function isearch_g(const v:infoType; var i:itemType):infoType;  // Search for info higher than "v"
    function isearch_le(const v:infoType; var i:itemType):infoType;  // Search for info lower or equel to "v"
    function isearch_ge(const v:infoType; var i:itemType):infoType;  // Search for info higher or equal to "v"

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
  itemMin=0;
  infoNil=0;

function tBinTree.Empty:boolean;
  begin
  Result:=head^.r=z;
  end;

function tBinTree.New_Node(const k:itemType; const i:infoType; const bi:boolean; const ll,rr:pnode):pnode;
  var
    a:longint;
    p:pnodearr;
  begin
  if myPoolSize>0 then
    begin
    Result:=pool.Get;
    if Result=nil then // Pool empty, need to resize pool and create a new list
      begin
      SetLength(myPools,Length(myPools)+1); // Resize myPools list
      GetMem(p,SizeOf(tnode)*myPoolSize); // Create new list
      myPools[length(myPools)-1]:=p; // store list
      pool.Size:=pool.Size+myPoolSize; // resize Pool
      for a:=0 to myPoolSize-1 do
        pool.Put(@p^[a]);
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
    l:=ll; l2:=l;
    r:=rr; r2:=r;
    b:=bi; b2:=b;
    end;
  end;

procedure tBinTree.PoolSize(size:integer);
// PoolSize;
  begin
  if (pool.Size=0) or (myPoolSize>0) then
    myPoolSize:=size;
  end;

procedure tBinTree.Del_Node(node:pnode);
// del_node
  begin
  if myPoolSize>0 then
    pool.Put(node)
  else
    FreeMem(node);
  end;

constructor tBinTree.Create(size:integer);
// Create
  begin
  inherited Create;
  cnt:=0;
  myPoolSize:=size;
  pool:=tPtrPool.Create;
  z:=new_node(itemMin,infoNil,false,nil,nil);
  z^.l:=z; z^.r:=z;
  z^.l2:=z; z^.r2:=z;
  head:=new_node(itemMin,infoNil,false,z,z);
  head^.l2:=z; head^.r2:=z;
  New(Parents);
  end;

procedure tBinTree.Change(const v:itemType;const info:infoType);
// Change
  begin
  remove(v);
  insert(v,info);
  end;

procedure tBinTree.RemoveAll;
// RemoveAll
  procedure RemoveThis(var t:pnode);
    begin
    if t^.l<>z then RemoveThis(t^.l);
    if t^.r<>z then RemoveThis(t^.r);
    t^.info:=infoNil;
    t^.key:=itemMin;
    t^.r2:=z;
    t^.l2:=z;
    del_node(t);
    t:=z;
    end;
  begin
  if head^.r<>z then RemoveThis(head^.r);
  head^.info:=infoNil;
  head^.key:=itemMin;
  head^.r2:=z;
  head^.l2:=z;
  cnt:=0;
  end;

destructor tBinTree.Destroy;
// Destroy;
  var
    a:longint;
  begin
  RemoveAll;

  Dispose(Parents);

  if assigned(head) then
    begin
    head^.info:=infoNil;
    head^.key:=itemMin;
    del_node(head);
    end;

  if assigned(z) then
    begin
    z^.info:=infoNil;
    z^.key:=itemMin;
    del_node(z);
    end;

  for a:=0 to Length(myPools)-1 do
    FreeMem(myPools[a]);
  SetLength(myPools,0);
  pool.destroy;

  inherited;
  end;

function tBinTree.Search(const v:itemType):infoType;
// Search
  var
    x:pnode;
  begin
  x:=head^.r;
  while (x<>z) and (v<>x^.key) do
    if (v<x^.key) then x:=x^.l else x:=x^.r;
  Result:=x^.info;
  end;

function tBinTree.iSearch(const v:infoType):itemType;
// iSearch
  var
    x:pnode;
  begin
  x:=head^.r2;
  while (x<>z) and (v<>x^.info) do
    if (v<x^.info) then x:=x^.l2 else x:=x^.r2;
  Result:=x^.key;
  end;

function tBinTree.Search_Min(var i:infoType):itemType;
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

function tBinTree.iSearch_Min(var i:itemType):infoType;
// iSearch_Min
  var
    x:pnode;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.l2<>z) do x:=x^.l2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=itemMin;
    Result:=infoNil;
    end;
  end;

function tBinTree.Search_Max(var i:infoType):itemType;
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

function tBinTree.iSearch_Max(var i:itemType):infoType;
// iSearch_Max
  var
    x:pnode;
  begin
  x:=head^.r2;
  if x<>z then
    begin
    while (x^.r2<>z) do x:=x^.r2;
    i:=x^.key;
    Result:=x^.info;
    end
  else
    begin
    i:=itemMin;
    Result:=infoNil;
    end;
  end;

function tBinTree.Search_L(const v:itemType; var i:infoType):itemType;
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

function tBinTree.iSearch_L(const v:infoType; var i:itemType):infoType;
// iSearch_L
  var
    x,y:pnode;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      begin
      if (x^.info=v) and (x^.l2<>z) then y:=x^.l2;
      x:=x^.l2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tBinTree.Search_G(const v:itemType; var i:infoType):itemType;
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

function tBinTree.iSearch_G(const v:infoType; var i:itemType):infoType;
// iSearch_G
  var
    x,y:pnode;
  begin
  x:=head^.r2; y:=head;
  while x<>z do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      begin
      if (x^.info=v) and (x^.r2<>z) then y:=x^.r2;
      x:=x^.r2;
      end;
    end;
  Result:=y^.info;
  i:=y^.key;
  end;

function tBinTree.Search_LE(const v:itemType; var i:infoType):itemType;
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

function tBinTree.iSearch_LE(const v:infoType; var i:itemType):infoType;
// iSearch_LE
  var
    x,y:pnode;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info<v) then
      begin
      y:=x;
      x:=x^.r2;
      end
    else
      x:=x^.l2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

function tBinTree.Search_GE(const v:itemType; var i:infoType):itemType;
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

function tBinTree.iSearch_GE(const v:infoType; var i:itemType):infoType;
// iSearch_GE
  var
    x,y:pnode;
  begin
  x:=head^.r2; y:=head;
  while (x<>z) and (v<>x^.info) do
    begin
    if (x^.info>v) then
      begin
      y:=x;
      x:=x^.l2;
      end
    else
      x:=x^.r2;
    end;
  if x<>z then
    begin
    Result:=x^.info;
    i:=x^.key;
    end
  else
    begin
    Result:=y^.info;
    i:=y^.key;
    end;
  end;

procedure tBinTree.Insert(const v:itemType;const info:infoType);
// Insert
  var
    nx,x,p,g,gg,c:pnode;
    v2:infoType;
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
  procedure split2;
    procedure p_rotate_g;
      begin
      if (v2<g^.info) then c:=g^.l2
      else if (v2>g^.info) then c:=g^.r2
      else if (v<g^.key) then c:=g^.l2
      else c:=g^.r2;
      if (v2<c^.info) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else if (v2>c^.info) then
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end
      else if (v<c^.key) then
        begin
        p:=c^.l2;
        c^.l2:=p^.r2;
        p^.r2:=c;
        end
      else
        begin
        p:=c^.r2;
        c^.r2:=p^.l2;
        p^.l2:=c;
        end;
      if (v2<g^.info) then g^.l2:=p
      else if (v2>g^.info) then g^.r2:=p
      else if (v<g^.key) then g^.l2:=p
      else g^.r2:=p;
      end;
    procedure x_rotate_gg;
      begin
      if (v2<gg^.info) then c:=gg^.l2
      else if (v2>gg^.info) then c:=gg^.r2
      else if (v<gg^.key) then c:=gg^.l2
      else c:=gg^.r2;
      if (v2<c^.info) then
        begin
        x:=c^.l2;
        c^.l2:=x^.r2;
        x^.r2:=c;
        end
      else if (v2>c^.info) then
        begin
        x:=c^.r2;
        c^.r2:=x^.l2;
        x^.l2:=c;
        end
      else if (v<c^.key) then
        begin
        x:=c^.l2;
        c^.l2:=x^.r2;
        x^.r2:=c;
        end
      else
        begin
        x:=c^.r2;
        c^.r2:=x^.l2;
        x^.l2:=c;
        end;
      if (v2<gg^.info) then gg^.l2:=x
      else if (v2>gg^.info) then gg^.r2:=x
      else if (v<gg^.key) then gg^.l2:=x
      else gg^.r2:=x;
      end;
    begin
    x^.b2:=true;
    x^.l2^.b2:=false;
    x^.r2^.b2:=false;
    if (p^.b2) then
      begin
      g^.b2:=true;
      if ( (v2<g^.info) or ((v2=g^.info) and (v<g^.key)) ) <>
         ( (v2<p^.info) or ((v2=p^.info) and (v<p^.key)) ) then p_rotate_g;
      x_rotate_gg;
      x^.b2:=false;
      end;
    head^.r2^.b2:=false;
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

  // Info Sort
  v2:=info;
  x:=head; p:=head; g:=head;
  while (x<>z) do
    begin
    gg:=g; g:=p; p:=x;
    if (v2<x^.info) then x:=x^.l2
    else if (v2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    if (x^.l2^.b2 and x^.r2^.b2) then split2;
    end;
  x:=nx;
  if (v2<p^.info) then p^.l2:=x
  else if (v2>p^.info) then p^.r2:=x
  else if (v<p^.key) then p^.l2:=x
  else p^.r2:=x;
  split2;
  Inc(cnt);
  end;

procedure tBinTree.Remove(const v:itemType);
// Remove
  var
    cb:boolean;
    c,p,g,y,p2,x,t:pnode;
    v2:infoType;
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
  procedure deleteFixup2;
    procedure p_rotateLeft_g;
      begin
      AddParentNode(g);
      p^.r2 := y^.l2;
      if (p = g^.r2) then g^.r2 := y else g^.l2 := y;
      y^.l2 := p;
      g:=y; y:=p^.r2;
      end;
    procedure p_rotateRight_g;
      begin
      AddParentNode(g);
      p^.l2 := y^.r2;
      if (p = g^.l2) then g^.l2 := y else g^.r2 := y;
      y^.r2 := p;
      g:=y; y:=p^.l2;
      end;
    procedure y_rotateRight_p;
      begin
      c := y^.l2;
      y^.l2 := c^.r2;
      if (p^.r2 = y) then p^.r2 := c else p^.l2 := c;
      c^.r2 := y;
      y := p^.r2;
      end;
    procedure y_rotateLeft_p;
      begin
      c := y^.r2;
      y^.r2 := c^.l2;
      if (p^.l2 = y) then p^.l2 := c else p^.r2 := c;
      c^.l2 := y;
      y := p^.l2;
      end;
    begin
    p:=GetParentNode;
    g:=GetParentNode;
    while (x <> head^.r2) and (x^.b2 = false) do
      begin
      if (x = p^.l2) then
        begin
        y:=p^.r2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          p_rotateLeft_g;
          end;
        if (y^.l2^.b2 = false) and (y^.r2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := GetParentNode;
          end
        else if (p<>head) then
          begin
          if (y^.r2^.b2 = false) then
            begin
            y^.l2^.b2 := false;
            y^.b2 := true;
            y_rotateRight_p;
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.r2^.b2 := false;
          p_rotateLeft_g;
          x:=head^.r2;
          break;
          end;
        end
      else
        begin
        y:=p^.l2;
        if (y^.b2 = true) then
          begin
          y^.b2 := false;
          p^.b2 := true;
          p_rotateRight_g;
          end;
        if (y^.r2^.b2 = false) and (y^.l2^.b2 = false) then
          begin
          y^.b2 := true;
          x := p; p := g; g := GetParentNode;
          end
        else
          begin
          if (y^.l2^.b2 = false) then
            begin
            y^.r2^.b2 := false;
            y^.b2 := true;
            y_rotateLeft_p;
            end;
          y^.b2 := p^.b2;
          p^.b2 := false;
          y^.l2^.b2 := false;
          p_rotateRight_g;
          x:=head^.r2;
          break;
          end;
        end;
      end;
    if (x<>z) then x^.b2 := false;
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

  v2:=t^.info;
  InitParentNodes;
  p:=head;x:=head^.r2;
  AddParentNode(p);
  while (x<>z) and (v<>x^.key) do
    begin
    p:=x;
    AddParentNode(p);
    if (v2<x^.info) then x:=x^.l2
    else if (v2>x^.info) then x:=x^.r2
    else if (v<x^.key) then x:=x^.l2
    else x:=x^.r2;
    end;
  if t<>x then
    raise Exception.Create('BinTree -> structure corrupt !');

  if (t^.r2=z) then
    begin
    cb:=t^.b2;
    x:=t^.l2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else if (t^.l2=z) then
    begin
    cb:=t^.b2;
    x:=t^.r2;
    if (p^.l2=t) then p^.l2:=x else p^.r2:=x;
    end
  else
    begin
    p2:=p; c:=t^.r2;
    if c^.l2=z then
      begin
      AddParentNode(c);
      x:=c^.r2;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end
    else
      begin
      AddParentNode(t);
      repeat
        AddParentNode(c); p:=c;
        c:=c^.l2;
        until c^.l2=z;
      SwapParentNode;
      x:=c^.r2; p^.l2:=x;
      cb:=c^.b2;
      c^.b2:=t^.b2;
      c^.l2:=t^.l2;
      c^.r2:=t^.r2;
      if p2^.l2=t then p2^.l2:=c else p2^.r2:=c;
      end;
    end;
  if cb=false then deleteFixup2;
  t^.info:=infoNil;
  t^.key:=itemMin;
  del_node(t);
  Dec(cnt);
  end;

function tBinTree.Count: cardinal;
  begin
  Result:=cnt;
  end;

end.
