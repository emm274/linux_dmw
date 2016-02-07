unit xtri1; interface

uses
  Windows,Classes,Contnrs,OTypes,XList,XTri;

{$DEFINE METHOD2}

type
  PRib1 = ^TRib1;
  TRib1 = record
    p1,p2,t1,t2: Longint;
{$IFDEF METHOD1}
    a,b,c: Int64; lt,rb: TPoint;
{$ENDIF}
  end;

  PRibArray1 = ^TRibArray1;
  TRibArray1 = array[0..1023] of TRib1;

  PTri1 = ^TTri1; TTri1 = record
    p1,p2,p3, r1,r2,r3: Longint;
    minX,minY,maxX,maxY: Longint
  end;

  PTriArray1 = ^TTriArray1;
  TTriArray1 = array[0..1023] of TTri1;

  TRibList = class(TCustomList)
    constructor Create;
  end;

  TTriList = class(TCustomList)
    constructor Create;
  end;

  TTriEvent = procedure(it: Integer) of object;

  TCashMap = Array[0..7] of Integer;

  TTriangulator1 = class(VPointList)

    constructor Create;
    destructor Destroy; override;

    procedure Clear_data;

    function BeginRegion(const lt,rb: TPoint;
                         ndp: Integer): Boolean;
    function EndRegion: Integer;

    procedure Reset_stat;

    function Clean(baseCount: Integer;
                   dist: Double; distK: Integer): Integer;

    function Calculate(ATops: Pointer;
                       Atopn,AStride: Integer): Integer;

    function Add_node(const v: VPoint): Integer;
    function Ins_node(const v: VPoint; it: Integer): Integer;

    function Verify: Integer;

    procedure log_model(log: TStrings);

    procedure dump_log;

    procedure dump(Dest: PChar;
                   bw,bh,skip: Integer;
                   sys: psys; gb: PGPoly);

  protected
    function GetStatCount: Integer; virtual;

    procedure begin_region(const lt,rb: TPoint);

    function Cash_static(const lt,rb: TPoint;
                         step: Integer): Boolean;

    function LocateTri(pp: PPoint;
                       out Ind: Integer): Integer;

    procedure splitTri(it,rc,ip: Integer);

    function ThisTriangle(it: Integer; pp: PPoint): Integer;

    procedure Improve_triangulation;

    procedure zval_to_int;
    procedure zval_to_real;

  private
    fRibList: TRibList;
    fTriList: TTriList;
    fwlList: TIntegerlist;

    fTops: Longint;
    ftopn: Integer;
    fStride: Integer;
    fTriSize: Integer;

    fbp: PIntegers;
    fnl: integer;

    fwl: PIntegers;
    fnwl: Integer;
    fnlf: Integer;

    fTris: PTriArray1;
    ftrin: Integer;

    fRibs: PRibArray1;
    fribn: Integer;

    fCash: PIntegers;
    fCashSizeX: Integer;
    fCashSizeY: Integer;
    fCashDepth: Integer;
    fCashOrg: TPoint;
    fCashScale: TGauss;
    fCashBound: TSize;
    fCashMap: TCashMap;

    fCashUsed: Integer;
    fOutside: Integer;
    fTriTested: Int64;
    fElapsed: Longint;
    fRetCode: Integer;

    fLastTri: Integer;

    fLastVertex: TPoint;
    fCashDist: Double;

    fZones: TObjectList;
    fZoneScale: TGauss;
    fZoneSize: Integer;

    fImprove_enabled: Boolean;

    fOnTri: TTriEvent;
    fOnTriList: TNotifyEvent;

    function BeginProcess(const lt,rb: TPoint;
                          ndp: Integer): Boolean;

    function EndProcess: Integer;

    function Get_Vertex(I: Integer): TPoint;

    procedure SetRib(ir, p1,p2, t1,t2: integer);
    procedure SetTri(it, p1,p2,p3, r1,r2,r3: Integer);

    procedure zone_tri(it: Integer; new: Boolean);

    function Cash_init(const lt,rb: TPoint): Boolean;
    procedure Cash_resize;

    procedure Cash_tri(it: Integer);

    procedure resize_buffers;

    procedure rib_relink(ir,At1,At2: Integer);
    function rib_to_top(var tr: TTri1; r1: Integer): Boolean;
    function NextTri(it,ir: Integer): Integer;

    function InsertVertex(ip: Integer): Integer;

  public
    property Tops: Longint read fTops;
    property topn: Integer read ftopn;
    property Stride: Integer read fStride;
    property TriSize: Integer read fTriSize;

    property Tris: PTriArray1 read fTris;
    property trin: Integer read ftrin;

    property TriList: TTriList read fTriList;

    property Vertexes[I: Integer]: TPoint read Get_Vertex;

    property Improve_enabled: Boolean write fImprove_enabled;

    property RetCode: Integer read fRetCode;

    property OnTri: TTriEvent write fOnTri;
    property OnTriList: TNotifyEvent write fOnTriList;
  end;

implementation

uses
  Math,Sysutils,Graphics,OTypes1,
  Convert,OFiles,OGauss,XLine;

procedure spvp(const p1,p2,p3: TPoint; out sp,vp: Double);
var
  dx2,dy2,dx3,dy3: Double;
begin
  dx2:=p2.x-p1.x; dy2:=p2.y-p1.y;
  dx3:=p3.x-p1.x; dy3:=p3.y-p1.y;

  sp:=dx2*dx3 + dy2*dy3;
  vp:=dy3*dx2 - dx3*dy2
end;

procedure Init_map(map: PIntegers; nx: Integer);
var
  x,y: Integer;
begin
  for y:=-1 to 1 do
  for x:=-1 to 1 do
  if (x <> 0) or (y <> 0) then begin
    map[0]:=y*nx + x; map:=@map[1]
  end
end;

constructor TRibList.Create;
begin
  inherited Create(Sizeof(TRib1),1024)
end;

constructor TTriList.Create;
begin
  inherited Create(Sizeof(TTri1),1024)
end;

constructor TTriangulator1.Create;
begin
  inherited Create(1024);

  fRibList:=TRibList.Create;
  fTriList:=TTriList.Create;
  fwlList:=TIntegerlist.Create;

  fZones:=TObjectList.Create;

  fTriSize:=Sizeof(TTri1);
  fImprove_enabled:=true
end;

destructor TTriangulator1.Destroy;
begin
  xFreePtr(fCash);

  fwlList.Free;
  fTriList.Free;
  fRibList.Free;

  fZones.Free;

  inherited
end;

procedure TTriangulator1.Clear_data;
begin
  fCash:=xFreePtr(fCash);

  fZones.Clear;

  fwlList.Clear;
  fTriList.Clear;
  fRibList.Clear;

  fTris:=nil; ftrin:=0;
  fRibs:=nil; fribn:=0;

  fbp:=nil; fnl:=0;
  fwl:=nil; fnwl:=0;
  fnlf:=0;

  fCashUsed:=0;
  fOutside:=0;
  fTriTested:=0;
  fRetCode:=0;

  fImprove_enabled:=true;

  Clear;
end;

function TTriangulator1.Get_Vertex(I: Integer): TPoint;
begin
  Result:=PPoint(fTops + I*fstride)^
end;

procedure TTriangulator1.SetRib(ir, p1,p2, t1,t2: integer);
var
  rb: TRib1; v1,v2: TPoint; ax,ay: Int64;
begin
  rb.p1:=p1; rb.p2:=p2;
  rb.t1:=t1; rb.t2:=t2;

{$IFDEF METHOD1}
  v1:=PPoint(fTops + p1*fstride)^;
  v2:=PPoint(fTops + p2*fstride)^;

  rb.a:=v2.Y-v1.Y;
  rb.b:=v1.X-v2.X;

  ax:=v1.X; ay:=v1.Y;
  rb.c:=ay*v2.X - ax*v2.Y;

  if v1.X < v2.X then begin
    rb.lt.X:=v1.X; rb.rb.X:=v2.X;
  end
  else begin
    rb.lt.X:=v2.X; rb.rb.X:=v1.X;
  end;

  if v1.Y < v2.Y then begin
    rb.lt.Y:=v1.Y; rb.rb.Y:=v2.Y;
  end
  else begin
    rb.lt.Y:=v2.Y; rb.rb.Y:=v1.Y;
  end;
{$ENDIF}

  fRibs[ir]:=rb;
end;

procedure TTriangulator1.SetTri(it, p1,p2,p3, r1,r2,r3: Integer);
var
  tr: TTri1; v1,v2,v3: TPoint;
begin
  tr.p1:=p1; tr.p2:=p2; tr.p3:=p3;
  tr.r1:=r1; tr.r2:=r2; tr.r3:=r3;

  v1:=PPoint(fTops + p1*fstride)^;
  v2:=PPoint(fTops + p2*fstride)^;
  v3:=PPoint(fTops + p3*fstride)^;

  tr.minX:=v1.X;
  if v2.X < tr.minX then tr.minX:=v2.X;
  if v3.X < tr.minX then tr.minX:=v3.X;

  tr.minY:=v1.Y;
  if v2.Y < tr.minY then tr.minY:=v2.Y;
  if v3.Y < tr.minY then tr.minY:=v3.Y;

  tr.maxX:=v1.X;
  if v2.X > tr.maxX then tr.maxX:=v2.X;
  if v3.X > tr.maxX then tr.maxX:=v3.X;

  tr.maxY:=v1.Y;
  if v2.Y > tr.maxY then tr.maxY:=v2.Y;
  if v3.Y > tr.maxY then tr.maxY:=v3.Y;

  fTris[it]:=tr;

  if fCashDepth < 0 then Cash_tri(it);

  if Assigned(fOnTri) then fOnTri(it)
end;

procedure TTriangulator1.zone_tri(it: Integer; new: Boolean);
var
  z1,z2: TPoint; i,j,iz: Integer; zone: TIntegerList;
begin
  with fTris[it] do begin
    z1.X:=Trunc((minX - fCashOrg.X) * fZoneScale.x);
    z1.Y:=Trunc((minY - fCashOrg.Y) * fZoneScale.y);
    z2.X:=Trunc((maxX - fCashOrg.X) * fZoneScale.x);
    z2.Y:=Trunc((maxY - fCashOrg.Y) * fZoneScale.y);
    if z1.X < 0 then z1.X:=0;
    if z1.Y < 0 then z1.Y:=0;
    if z2.X >= fZoneSize then z2.X:=fZoneSize-1;
    if z2.Y >= fZoneSize then z2.Y:=fZoneSize-1;
  end;

  for j:=z1.Y to z2.Y do
  for i:=z1.X to z2.X do begin

    iz:=j * fZoneSize + i;
    zone:=fZones[iz] as TIntegerList;

    if new then
      zone.Add(@it)
    else
    if zone.IndexOf(it) < 0 then
      zone.Add(@it)
  end
end;

procedure TTriangulator1.zval_to_int;
var
  i,p: Integer;
begin
  p:=fTops + 8;
  for i:=1 to ftopn do begin
    PInteger(p)^:=Round(PFloat(p)^*100); Inc(p,12)
  end
end;

procedure TTriangulator1.zval_to_real;
var
  i,p: Integer;
begin
  p:=fTops + 8;
  for i:=1 to ftopn do begin
    PFloat(p)^:=PInteger(p)^ / 100; Inc(p,12)
  end
end;

function TTriangulator1.BeginProcess(const lt,rb: TPoint;
                                     ndp: Integer): Boolean;
var
  n2,n3: Integer;
begin
  Result:=false;

  fImprove_enabled:=true;

  ndp:=Max(1024,ndp); n2:=ndp*2; n3:=ndp*3;

  fnwl:=10*ndp; fnlf:=0;

  if fwlList.Resize(n2 + fnwl) then
  if fRibList.Resize(n3) then
  if fTriList.Resize(n3) then begin
    fbp:=fwlList.First;
    fwl:=@fbp[n2]; Dec(fnwl,10);

    fRibs:=fRibList.First;
    fTris:=fTriList.First;

    if Assigned(fOnTriList) then
    fOnTriList(nil);

    fElapsed:=GetTickCount;

    Result:=Cash_init(lt,rb)
  end
end;

function TTriangulator1.EndProcess: Integer;
var
  n: Integer;
begin
  Result:=0; fRetCode:=Verify;

  n:=GetStatCount;
  if n > 0 then begin
    fCashUsed:=Round(fCashUsed / n * 100);
    fOutside:=Round(fOutside / n * 100);
    fTriTested:=fTriTested div n;
    Result:=ftrin
  end;

  fElapsed:=GetTickCount - fElapsed;
end;

function TTriangulator1.GetStatCount: Integer;
begin
  Result:=ftopn
end;

procedure TTriangulator1.Reset_stat;
begin
  fElapsed:=GetTickCount;
  fCashUsed:=0; fOutside:=0;
  fTriTested:=0;
end;

function TTriangulator1.Cash_init(const lt,rb: TPoint): Boolean;
var
  i: Integer;
  zone: TIntegerList;
begin
  fLastVertex.X:=lt.X + lt.X - rb.X;
  fLastVertex.Y:=lt.Y + lt.Y - rb.Y;

  fLastTri:=-1;

  fCashOrg:=lt;
  fCashBound.cx:=rb.X-lt.X+1;
  fCashBound.cy:=rb.Y-lt.Y+1;
  fCashScale.x:=2 / fCashBound.cx;
  fCashScale.y:=2 / fCashBound.cy;
  fCashSizeX:=2; fCashSizeY:=2;
  fCash:=xAllocInt(4);
  fCashDepth:=5 * 2 * 2;

  with fCashBound do
  fCashDist:=Max(cx / 2,cy / 2);

  if Assigned(fCash) then
  for i:=0 to 3 do fCash[i]:=-1;

  Init_map(@fCashMap,fCashSizeX);

  fZoneScale.x:=3 / fCashBound.cx;
  fZoneScale.y:=3 / fCashBound.cy;
  fZoneSize:=3;

  for i:=0 to 3*3-1 do begin
    zone:=TIntegerList.Create;
    try
      fZones.Add(zone);
    finally
    end
  end;

  Result:=Assigned(fCash)
end;

procedure TTriangulator1.Cash_resize;
var
  buf,lp,tmp: PIntegers; i,j,m,m2: Integer;
begin
  if Assigned(fCash) then begin

    m:=fCashSizeX; m2:=m * 2;
    buf:=xAllocInt(m2 * m2);
    if Assigned(buf) then begin

      lp:=buf;
      for j:=0 to m2-1 do
      for i:=0 to m2-1 do begin
        lp[0]:=fCash[(j div 2)*m + (i div 2)];
        lp:=@lp[1]
      end;

      tmp:=fCash; fCash:=buf;
      fCashSizeX:=m2; fCashSizeY:=m2;
      fCashScale.x:=m2 / fCashBound.cx;
      fCashScale.y:=m2 / fCashBound.cy;
      fCashDepth:=5 * m2 * m2;

      with fCashBound do
      fCashDist:=Max(cx / m2,cy / m2);

      Init_map(@fCashMap,fCashSizeX);

      xFreePtr(tmp)
    end
  end
end;

function TTriangulator1.Cash_static(const lt,rb: TPoint;
                                    step: Integer): Boolean;
var
  it,w,h,s: Integer; buf,tmp: PIntegers;
begin
  Result:=false;

  w:=int_Tiles(rb.X-lt.X+1,step);
  h:=int_Tiles(rb.Y-lt.Y+1,step);

  s:=w*h; buf:=xAllocInt(s);
  if Assigned(buf) then begin

    tmp:=fCash; fCash:=buf;
    fCashSizeX:=w; fCashSizeY:=h;

    fCashOrg:=lt;
    fCashBound.cx:=w;
    fCashBound.cy:=h;

    fCashScale.x:=1 / step;
    fCashScale.y:=1 / step;

    fCashDepth:=-1; fCashDist:=step;

    Init_map(@fCashMap,fCashSizeX);

    for it:=0 to ftrin-1 do Cash_tri(it);

    xFreePtr(tmp); Result:=true
  end
end;

procedure TTriangulator1.Cash_tri(it: Integer);
var
  di: PIntegers;
  vp: PVPoly; v1,v2,v3: VPoint;
  g: GOrient; o: TPoint; r: Double;
  iy1,iy2,ix1,ix2,w,h,x,y: Integer;
  dy1,dy2,dy3, x1,x2,xt: Double;
  a1,a2,b1,b2,c1,c2: Double;
  k: tgauss;
begin
  vp:=Pointer(Tops);
  with Tris[it] do begin
    v1:=vp[p1]; v2:=vp[p2]; v3:=vp[p3];
  end;

  if Assigned(fCash) then begin

    di:=fCash; w:=fCashSizeX; h:=fCashSizeY;

    o:=fCashOrg; k:=fCashScale;
    g[0].x:=(v1.x - o.X) * k.x;
    g[0].y:=(v1.y - o.Y) * k.y;
    g[1].x:=(v2.x - o.X) * k.x;
    g[1].y:=(v2.y - o.Y) * k.y;
    g[2].x:=(v3.x - o.X) * k.x;
    g[2].y:=(v3.y - o.Y) * k.y;

    if g[1].y < g[0].y then begin
      g[3]:=g[0]; g[0]:=g[1]; g[1]:=g[3]
    end;

    if g[2].y < g[0].y then begin
      g[3]:=g[0]; g[0]:=g[2]; g[2]:=g[3]
    end;

    if g[2].y < g[1].y then begin
      g[3]:=g[1]; g[1]:=g[2]; g[2]:=g[3]
    end;

    iy1:=Trunc(g[0].y);
    iy2:=Trunc(g[2].y+1);

    if iy1 < 0 then iy1:=0;
    if iy2 >= h then iy2:=h-1;

    dy1:=g[1].y - g[0].y;
    dy2:=g[2].y - g[0].y;
    dy3:=g[2].y - g[1].y;

    if iy1 <= iy2 then

    if dy2 > Small then begin
      b2:=(g[2].x - g[0].x) / dy2;
      b1:=g[0].x - g[0].y*b2;

      if dy1 > Small then begin
        a2:=(g[1].x - g[0].x) / dy1;
        a1:=g[0].x - g[0].y*a2;

        for y:=iy1 to iy2 do
        if y >= g[0].y then begin
          if y > g[1].y then Break;

          x1:=a1 + y*a2;
          x2:=b1 + y*b2;
          if x1 > x2 then begin
            xt:=x1; x1:=x2; x2:=xt
          end;

          ix1:=Round(x1); ix2:=Trunc(x2);
          if ix1 < 0 then ix1:=0;
          if ix2 >= w then ix2:=w-1;

          for x:=ix1 to ix2 do
          di[y*w+x]:=it
        end
      end;

      if dy3 > Small then begin
        c2:=(g[2].x - g[1].x) / dy3;
        c1:=g[1].x - g[1].y*c2;

        for y:=iy1 to iy2 do
        if y >= g[1].y then
        if y <= g[2].y then begin

          x1:=b1 + y*b2;
          x2:=c1 + y*c2;
          if x1 > x2 then begin
            xt:=x1; x1:=x2; x2:=xt
          end;

          ix1:=Round(x1); ix2:=Trunc(x2);
          if ix1 < 0 then ix1:=0;
          if ix2 >= w then ix2:=w-1;

          for x:=ix1 to ix2 do
          di[y*w+x]:=it
        end
      end
    end
  end
end;

procedure TTriangulator1.resize_buffers;
var
  ndp,n2: Integer;
begin
  if fribn + 100 > fRibList.Count then begin
    fRibList.Extend(fribn + 10000);
    fRibs:=fRibList.First
  end;

  if ftrin + 100 > fTriList.Count then begin
    fTriList.Extend(ftrin + 10000);
    fTris:=fTriList.First;

    if Assigned(fOnTriList) then
    fOnTriList(nil)
  end;

  if ftopn + 100 > fwlList.Count div 12 then begin

    ndp:=ftopn + 10000;
    fnwl:=10*ndp; n2:=ndp*2;

    fwlList.Extend(n2 + fnwl);
    fbp:=fwlList.First;
    fwl:=@fbp[n2]; Dec(fnwl,10);
  end
end;

function TTriangulator1.BeginRegion(const lt,rb: TPoint;
                                    ndp: Integer): Boolean;
begin
  Result:=false; Clear_data;
  if ndp = 0 then ndp:=100000;
  if BeginProcess(lt,rb,ndp) then begin
    begin_region(lt,rb);
    Result:=true
  end
end;

function TTriangulator1.EndRegion: Integer;
begin
  EndProcess; Result:=ftrin
end;

procedure TTriangulator1.begin_region(const lt,rb: TPoint);
var
  i1,i2,i3,i4,z: Integer;
  pp: PPoint;
begin
  z:=znil1;

  i1:=AddItem(lt.X,lt.Y,z);
  i2:=AddItem(rb.X,lt.Y,z);
  i3:=AddItem(rb.X,rb.Y,z);
  i4:=AddItem(lt.X,rb.Y,z);

  pp:=First; fTops:=Longint(pp);
  ftopn:=Count; fStride:=12;

  SetRib(0,i1,i2,0,-1);
  SetRib(1,i2,i3,1,-1);
  SetRib(2,i3,i4,1,-1);
  SetRib(3,i4,i1,0,-1);
  SetRib(4,i2,i4,0,1);

  SetTri(0, i1,i2,i4, 0,4,3);
  SetTri(1, i2,i3,i4, 1,2,4);

  fribn:=5; fnl:=0; ftrin:=2;

  zone_tri(0,true); zone_tri(1,true);
end;

function TTriangulator1.Clean(baseCount: Integer;
                              dist: Double; distK: Integer): Integer;

procedure off_triangle(it: Integer; tp: PTri1);

function rib_off_triangle(ir,it: Integer): Integer;
var
  rb: PRib1;
begin
  rb:=@fRibs[ir];
  if rb.t2 = it then
    rb.t2:=-1
  else begin
    rb.t1:=rb.t2; rb.t2:=-1
  end;

  Result:=-1
end;

begin
  with tp^ do begin
    r1:=rib_off_triangle(r1,it);
    r2:=rib_off_triangle(r2,it);
    r3:=rib_off_triangle(r3,it)
  end
end;

var
  vp: PVPoly; tp: PTriArray1;
  ir,it,j: Integer; v1,v2,v3: VPoint;
  rp: PRibArray1; tr: PTri1; l: LOrient;
  rb: TRib1;
begin
  vp:=Pointer(fTops); 

  tp:=fTris;
  for it:=0 to ftrin-1 do begin

    with tp[0] do begin
      v1:=vp[p1]; v2:=vp[p2]; v3:=vp[p3];
    end;

    if (v1.z <= znil)
    or (v2.z <= znil)
    or (v3.z <= znil) then
    off_triangle(it,@tp[0]);

    tp:=@tp[1]
  end;

  repeat
    rp:=fRibs; j:=0;
    for ir:=0 to fribn-1 do begin

      rb:=rp[0]; rp:=@rp[1];

      if rb.t2 < 0 then
      if rb.t1 >= 0 then begin

        it:=rb.t1; tr:=@fTris[it];

        with tr^ do
        if r1 >= 0 then
        if r2 >= 0 then
        if r3 >= 0 then

        if (r1 = ir) or (r2 = ir) or (r3 = ir) then begin
          l[0]:=PPoint(@vp[p1])^;
          l[1]:=PPoint(@vp[p2])^;
          l[2]:=PPoint(@vp[p3])^;

          if not Triangle_Verify(@l,0.002) then begin
            off_triangle(it,tr); Inc(j)
          end else
          if dist > 0 then
          if rb.p1 < baseCount then
          if rb.p2 < baseCount then begin

            l[0]:=PPoint(@vp[rb.p1])^;
            l[1]:=PPoint(@vp[rb.p2])^;

            if Long_Dist(l[0],l[1]) > dist then begin
              off_triangle(it,tr); Inc(j)
            end
          end
        end
      end;
    end;
  until j = 0;

  j:=0; tp:=fTris;
  for it:=0 to ftrin-1 do begin

    with tp[it] do
    if r1 >= 0 then
    if r2 >= 0 then
    if r3 >= 0 then begin
      tp[j]:=tp[it]; Inc(j)
    end
  end; ftrin:=j;

  Result:=ftrin;
end;

procedure TTriangulator1.rib_relink(ir,At1,At2: Integer);
begin
  with fRibs[ir] do begin
    if t1 = At1 then t1:=At2 else
    if t2 = At1 then t2:=At2
  end
end;

function TTriangulator1.rib_to_top(var tr: TTri1; r1: Integer): Boolean;
var
  tmp: TTri1;
begin
  if tr.r2 = r1 then begin tmp:=tr;
    tr.p1:=tmp.p2; tr.p2:=tmp.p3; tr.p3:=tmp.p1;
    tr.r1:=tmp.r2; tr.r2:=tmp.r3; tr.r3:=tmp.r1;
  end else
  if tr.r3 = r1 then begin tmp:=tr;
    tr.p1:=tmp.p3; tr.p2:=tmp.p1; tr.p3:=tmp.p2;
    tr.r1:=tmp.r3; tr.r2:=tmp.r1; tr.r3:=tmp.r2;
  end;

  Result:=tr.r1 = r1
end;

function TTriangulator1.NextTri(it,ir: Integer): Integer;
var
  rb: TRib1;
begin
  Result:=-1; rb:=fRibs[ir];
  if rb.t1 = it then Result:=rb.t2 else
  if rb.t2 = it then Result:=rb.t1
end;

procedure TTriangulator1.Improve_triangulation;

function BadPare(i1,i2,i3,i4: Integer): Boolean;
var
  p1,p2,p3,p4: TPoint; sa,sb: Double;
  v14,v24,v13,v23: TGauss;
Begin
  Result:=false;

  p1:=PPoint(fTops + i1*fstride)^;
  p2:=PPoint(fTops + i2*fstride)^;
  p3:=PPoint(fTops + i3*fstride)^;
  p4:=PPoint(fTops + i4*fstride)^;

  v14.x:=p4.X-p1.X; v14.y:=p4.Y-p1.Y;
  v24.x:=p4.X-p2.X; v24.y:=p4.Y-p2.Y;

  v13.x:=p3.X-p1.X; v13.y:=p3.Y-p1.Y;
  v23.x:=p3.X-p2.X; v23.y:=p3.Y-p2.Y;

  sa:=v14.x*v24.x + v14.y*v24.y;
  sb:=v13.x*v23.x + v13.y*v23.y;

  if (sa < 0) and (sb < 0) then
    Result:=true
  else
  if (sa < 0) or (sb < 0) then

  if (v24.x*v14.y - v14.x*v24.y)*sb +
     (v13.x*v23.y - v13.y*v23.x)*sa < 0 then
    Result:=true
end;

const
  nrep = 100;
var
  irep,ilf,nlfc,ir, it1,it2: Integer;
  rb: TRib1; t1,t2: TTri1;
begin
  if fImprove_enabled then
  for irep:=1 to nrep do begin

    nlfc:=fnlf;
    for ilf:=0 to fnlf-1 do begin

      ir:=fwl[ilf]; rb:=fRibs[ir];

      it1:=rb.t1; it2:=rb.t2;
      if (it1 >= 0) and (it2 >= 0) then begin

        t1:=Tris[it1]; t2:=Tris[it2];

        if rib_to_top(t1,ir) then
        if rib_to_top(t2,ir) then

        if BadPare(t1.p1,t1.p2,t1.p3,t2.p3) then begin

          SetRib(ir, t1.p3,t2.p3, rb.t1,rb.t2);

          SetTri(it1, t1.p2,t1.p3,t2.p3, t1.r2,ir,t2.r3);
          SetTri(it2, t1.p1,t2.p3,t1.p3, t2.r2,ir,t1.r3);

          rib_relink(t1.r3, it1,it2);
          rib_relink(t2.r3, it2,it1);

          zone_tri(it1,false);
          zone_tri(it2,false);

          if nlfc + 4 <= fnwl then begin
            fwl[nlfc]:=t1.r2; Inc(nlfc);
            fwl[nlfc]:=t1.r3; Inc(nlfc);
            fwl[nlfc]:=t2.r2; Inc(nlfc);
            fwl[nlfc]:=t2.r3; Inc(nlfc);
          end
        end
      end
    end;

    for ilf:=fnlf to nlfc-1 do
    fwl[ilf-fnlf]:=fwl[ilf];

    fnlf:=nlfc - fnlf;
    if fnlf = 0 then Break
  end
end;

function TTriangulator1.ThisTriangle(it: Integer; pp: PPoint): Integer;

function iline(p, a,b: PPoint): Integer;
var
  x,y,x1,y1,x2,y2: Integer;
  mx1,my1,mx2,my2: Integer;
  ax,_x,_y,_x1,_y1: Int64;
begin
  x:=p.X; x1:=a.X; x2:=b.X;
  y:=p.Y; y1:=a.Y; y2:=b.Y;

  _x:=x; _y:=y; _x1:=x1; _y1:=y1;

  ax:=_x * (y2-y1) + _y * (x1-x2) +
      (_y1*x2 - _x1*y2);

  if ax = 0 then begin Result:=0;

    mx1:=x1; my1:=y1; mx2:=x2; my2:=y2;
    if mx1 > mx2 then begin mx1:=x2; mx2:=x1 end;
    if my1 > my2 then begin my1:=y2; my2:=y1 end;

    if (x >= mx1) and (x <= mx2) then
    if (y >= my1) and (y <= my2) then Result:=2

  end else

  if ax < 0 then Result:=-1
            else Result:=+1
end;

var
  r1,r2,r3: Int64; tr: TTri1;
  p: TPoint; p1,p2,p3: TPoint;
  rc1,rc2,rc3: Integer;
  rb: TRib1; ip: Int64;
begin
  Result:=0;  p:=pp^; ip:=Int64(p);

  tr:=fTris[it];
  if (p.X >= tr.minX) and (p.X <= tr.maxX) then
  if (p.Y >= tr.minY) and (p.Y <= tr.maxY) then begin

{$IFDEF METHOD1}
    rb:=fRibs[tr.r1];
    r1:=rb.a*p.X + rb.b*p.Y + rb.c;
    if tr.p1 <> fRibs[tr.r1].p1 then r1:=-r1;

    if r1 = 0 then begin
      if (p.X >= rb.lt.X) and (p.X <= rb.rb.X) then
      if (p.Y >= rb.lt.Y) and (p.Y <= rb.rb.Y) then begin
        p1:=PPoint(fTops + tr.p1*fstride)^;
        p2:=PPoint(fTops + tr.p2*fstride)^;
        if ip = Int64(p1) then Result:=10;
        if ip = Int64(p2) then Result:=20;
        Inc(Result,1);
      end
    end else
    if r1 < 0 then begin
      rb:=fRibs[tr.r2];
      r2:=rb.a*p.X + rb.b*p.Y + rb.c;
      if tr.p2 <> fRibs[tr.r2].p1 then r2:=-r2;

      if r2 = 0 then begin
        if (p.X >= rb.lt.X) and (p.X <= rb.rb.X) then
        if (p.Y >= rb.lt.Y) and (p.Y <= rb.rb.Y) then begin
          p3:=PPoint(fTops + tr.p3*fstride)^;
          if ip = Int64(p3) then Result:=30;
          Inc(Result,2);
        end
      end else
      if r2 < 0 then begin
        rb:=fRibs[tr.r3];
        r3:=rb.a*p.X + rb.b*p.Y + rb.c;
        if tr.p3 <> fRibs[tr.r3].p1 then r3:=-r3;

        if r3 = 0 then begin
          if (p.X >= rb.lt.X) and (p.X <= rb.rb.X) then
          if (p.Y >= rb.lt.Y) and (p.Y <= rb.rb.Y) then
          Inc(Result,3);
        end else

        if r3 < 0 then Result:=4
      end
    end
{$ELSE}
    p1:=PPoint(fTops + tr.p1*fstride)^;
    p2:=PPoint(fTops + tr.p2*fstride)^;
    p3:=PPoint(fTops + tr.p3*fstride)^;

    rc1:=iline(@p, @p1,@p2);
    if Abs(rc1) <> 1 then begin
      if rc1 = 2 then begin
        if ip = Int64(p1) then Result:=10 else
        if ip = Int64(p2) then Result:=20;
        Inc(Result,1)
      end
    end else
    if rc1 < 0 then begin
      rc2:=iline(@p, @p2,@p3);
      if Abs(rc2) <> 1 then begin
        if rc2 = 2 then begin
          if ip = Int64(p3) then Result:=30;
          Inc(Result,2)
        end
      end else
      if rc2 < 0 then begin
        rc3:=iline(@p, @p3,@p1);
        if Abs(rc3) <> 1 then begin
          if rc3 = 2 then Result:=3
        end else
        if rc3 < 0 then Result:=4
      end
    end
{$ENDIF}

  end
end;

function TTriangulator1.LocateTri(pp: PPoint;
                                  out Ind: Integer): Integer;
var
  it,it1,ix,iy,i,rc,w,h,bx: Integer;
  rect: PInteger; len: Double;
  zone: TIntegerList; zp: PInteger;
  v: TPoint; mp: PIntegers;
begin
  Result:=-1;

  rc:=0; v:=pp^; rect:=nil;

  if Assigned(fCash) then begin

    ix:=Trunc((v.X - fCashOrg.X) * fCashScale.x);
    iy:=Trunc((v.Y - fCashOrg.Y) * fCashScale.y);

    w:=fCashSizeX; h:=fCashSizeY;

    if fCashDepth < 0 then begin

      if (ix >= 0) and (ix < w) then
      if (iy >= 0) and (iy < h) then begin

        bx:=iy*w + ix;
        it:=fCash[bx]; rc:=0;

        if it >= 0 then begin
          rc:=ThisTriangle(it,@v);
          if rc > 0 then

            Result:=it

          else
          if (ix > 0) and (ix < w-1) then
          if (iy > 0) and (iy < h-1) then begin begin

              mp:=@fCashMap;
              for i:=0 to 7 do begin
                it1:=fCash[bx + mp[0]];
                if it1 <> it then begin
                  rc:=ThisTriangle(it1,@v);
                  if rc > 0 then begin
                    fCash[bx]:=it1;
                    Result:=it1; Break
                  end; it:=it1
                end; mp:=@mp[1]
              end

            end
          end
        end
      end

    end
    else begin
      if ix < 0 then ix:=0;
      if ix >= w then ix:=w-1;
      if iy < 0 then iy:=0;
      if iy >= h then iy:=h-1;
      rect:=@fCash[iy * w + ix];

      len:=Hypot(v.X - fLastVertex.X,v.Y - fLastVertex.Y);

      if len <= fCashDist then begin

        it:=fLastTri;
        if it >= 0 then begin
          Inc(fTriTested);
          rc:=ThisTriangle(it,pp);
          if rc > 0 then begin
            fLastTri:=it; Result:=it;
          end
        end
      end;

      if Result < 0 then begin
        it:=rect^;
        if it >= 0 then begin
          Inc(fTriTested);
          rc:=ThisTriangle(it,pp);
          if rc > 0 then begin
            fLastTri:=it; Result:=it;
          end
        end
      end
    end
  end;

  if Result >= 0 then

    Inc(fCashUsed)

  else begin

    if fZones.Count > 0 then begin
      ix:=Trunc((v.X - fCashOrg.X) * fZoneScale.x);
      iy:=Trunc((v.Y - fCashOrg.Y) * fZoneScale.y);

      if (ix >= 0) and (ix < fZoneSize) then
      if (iy >= 0) and (iy < fZoneSize) then begin
        zone:=fZones[iy*fZoneSize + ix] as TIntegerlist;
        zp:=zone.Last;
        for i:=1 to zone.Count do begin

          it:=zp^; Dec(Longint(zp),4);

          Inc(fTriTested);
          rc:=ThisTriangle(it,pp);
          if rc > 0 then begin
            Result:=it; Break
          end
        end
      end
    end;

    if Result < 0 then begin
      it:=ftrin-1;
      while it >= 0 do begin
        Inc(fTriTested);
        rc:=ThisTriangle(it,pp);
        if rc > 0 then begin
          Result:=it; Break
        end;

        Dec(it)
      end
    end;

    if Result >= 0 then
    if Assigned(rect) then begin
      rect^:=it; fLastTri:=it;
    end
  end;

  Ind:=rc
end;

procedure TTriangulator1.splitTri(it,rc,ip: Integer);

procedure split3(it,ip: Integer);
var
  r1,r2,r3, t1,t2,t3: Integer; tr: TTri1;
begin
  tr:=fTris[it];

  r1:=fribn;
  r2:=fribn+1;
  r3:=fribn+2;

  t1:=it;
  t2:=ftrin;
  t3:=ftrin+1;

  SetRib(r1,tr.p1,ip, t3,t1);
  SetRib(r2,tr.p2,ip, t1,t2);
  SetRib(r3,tr.p3,ip, t2,t3);

  SetTri(t1, tr.p1,tr.p2,ip, tr.r1,r2,r1);
  SetTri(t2, tr.p2,tr.p3,ip, tr.r2,r3,r2);
  SetTri(t3, tr.p3,tr.p1,ip, tr.r3,r1,r3);

  rib_relink(tr.r2,t1,t2);
  rib_relink(tr.r3,t1,t3);

  Inc(fribn,3); Inc(ftrin,2);

  zone_tri(t2,true);
  zone_tri(t3,true);

  fwl[fnlf]:=tr.r1; Inc(fnlf);
  fwl[fnlf]:=tr.r2; Inc(fnlf);
  fwl[fnlf]:=tr.r3; Inc(fnlf);
  Improve_triangulation;
end;

procedure split2(it1,rc,ip: Integer);
var
  ir,it2, t1,t2,t3,t4, r1,r2,r3,r4: Integer;
  tr1,tr2,tmp: TTri1; rb: TRib1;
begin
  tr1:=fTris[it1];

  if rc = 2 then begin tmp:=tr1;
    tr1.p1:=tmp.p2; tr1.p2:=tmp.p3; tr1.p3:=tmp.p1;
    tr1.r1:=tmp.r2; tr1.r2:=tmp.r3; tr1.r3:=tmp.r1;
  end else
  if rc = 3 then begin tmp:=tr1;
    tr1.p1:=tmp.p3; tr1.p2:=tmp.p1; tr1.p3:=tmp.p2;
    tr1.r1:=tmp.r3; tr1.r2:=tmp.r1; tr1.r3:=tmp.r2;
  end;

  ir:=tr1.r1; rb:=fRibs[ir];

  if ((rb.t1 = it1) and (rb.t2 < 0))
  or ((rb.t1 < 0) and (rb.t2 = it1)) then begin

    t1:=it1; t2:=ftrin;
    r1:=ir; r2:=fribn; r3:=fribn+1;

    SetRib(r1, tr1.p1,ip, t1,-1);
    SetRib(r2, ip,tr1.p2, t2,-1);
    SetRib(r3, ip,tr1.p3, t1,t2);

    SetTri(t1, tr1.p1,ip,tr1.p3, r1,r3,tr1.r3);
    SetTri(t2, ip,tr1.p2,tr1.p3, r2,tr1.r2,r3);

    rib_relink(tr1.r2, t1,t2);

    Inc(fribn,2); Inc(ftrin);

    zone_tri(t2,true);

    fwl[fnlf]:=tr1.r2; Inc(fnlf);
    fwl[fnlf]:=tr1.r3; Inc(fnlf);
    Improve_triangulation;

  end
  else begin
    it2:=rb.t1;
    if it2 = it1 then it2:=rb.t2;
    tr2:=fTris[it2];

    if rib_to_top(tr2,ir) then begin

      r1:=ir; r2:=fribn; r3:=fribn+1; r4:=fribn+2;

      t1:=it1; t2:=ftrin;
      t3:=it2; t4:=ftrin+1;

      SetRib(r1, tr1.p1,ip, t1,t4);
      SetRib(r2, ip,tr1.p2, t2,t3);
      SetRib(r3, ip,tr1.p3, t1,t2);
      SetRib(r4, ip,tr2.p3, t3,t4);

      SetTri(t1, tr1.p1,ip,tr1.p3, r1,r3,tr1.r3);
      SetTri(t2, ip,tr1.p2,tr1.p3, r2,tr1.r2,r3);
      SetTri(t3, tr2.p1,ip,tr2.p3, r2,r4,tr2.r3);
      SetTri(t4, ip,tr2.p2,tr2.p3, r1,tr2.r2,r4);

      rib_relink(tr1.r2, t1,t2);
      rib_relink(tr2.r2, t3,t4);

      Inc(fribn,3); Inc(ftrin,2);

      zone_tri(t2,true);
      zone_tri(t4,true);

      fwl[fnlf]:=r1;     Inc(fnlf);
      fwl[fnlf]:=r2;     Inc(fnlf);
      fwl[fnlf]:=tr1.r2; Inc(fnlf);
      fwl[fnlf]:=tr1.r3; Inc(fnlf);
      fwl[fnlf]:=tr2.r2; Inc(fnlf);
      fwl[fnlf]:=tr2.r3; Inc(fnlf);
      Improve_triangulation;

    end
  end
end;

var
  pp: PPoint;
begin
  if rc = 4 then split3(it,ip) else
  if rc < 4 then split2(it,rc,ip);
  pp:=PPoint(fTops + ip*fstride);
  fLastVertex:=pp^
end;

function TTriangulator1.InsertVertex(ip: Integer): Integer;
var
  pp: PPoint; rc: Integer;
begin
  pp:=PPoint(fTops + ip*fstride);
  Result:=LocateTri(pp,rc);

  if Result >= 0 then
  splitTri(Result,rc,ip)
end;

function TTriangulator1.Calculate(ATops: Pointer;
                                  Atopn,AStride: Integer): Integer;

function RibVisible(ir: Integer; ip: PPoint): Boolean;
var
  dx,dy, sp,vp: Double; v1,v2: TPoint;
begin
  with fRibs[ir] do begin
    v1:=PPoint(fTops + p1*fstride)^;
    v2:=PPoint(fTops + p2*fstride)^;
  end;

  dx:=v2.X-v1.X; dy:=v2.Y-v1.Y;

  vp:=dx*(ip.Y-v2.Y) - dy*(ip.X-v2.X);
  Result:=vp < -epsln
end;

procedure point_outside(ip: Integer);
var
  pp: PPoint;
  iliv,ilvs,il,rc: Integer;
  rb: TRib1; ixvs,ixvspv: Boolean;

begin
  pp:=PPoint(fTops + ip*fstride);

  iliv:=0; ilvs:=0;
  ixvs:=RibVisible(fbp[0],pp);

  for il:=1 to fnl-1 do begin

    ixvspv:=ixvs;
    ixvs:=RibVisible(fbp[il],pp);

    if ixvs then begin
      if not ixvspv then begin
        ilvs:=il;
        if iliv <> 0 then Break
      end
    end
    else begin
      if ixvspv then begin
        iliv:=il; if ilvs <> 0 then Break
      end
    end
  end;

  if iliv+ilvs = 0 then ilvs:=fnl-1;

  if ilvs < iliv then Inc(ilvs,fnl);

  // Shift (rotate) the border to have the invisible border
  // line segments contained in the first part of the array.

  if iliv > 0 then begin

    for il:=0 to iliv do
    fbp[il+fnl]:=fbp[il];

    for il:=0 to fnl-1 do
    fbp[il]:=fbp[il+iliv];

    Dec(ilvs,iliv)
  end;

  // Add triangles, update border
  if ilvs < fnl then begin

    for il:=ilvs to fnl-1 do begin

      rc:=fbp[il]; rb:=fRibs[rc];

      if il > ilvs then
        fRibs[fribn-1].t2:=trin
      else begin
        SetRib(fribn,rb.p1,ip,trin,-1);
        fbp[il]:=fribn; Inc(fribn);
      end;

      if il = fnl-1 then fbp[ilvs+1]:=fribn;

      SetRib(fribn,ip,rb.p2,trin,-1);
      Inc(fribn);

      fRibs[rc].t2:=trin;

      SetTri(ftrin, rb.p2,rb.p1,ip,
                    rc,fribn-2,fribn-1);
      Inc(ftrin);

      zone_tri(ftrin-1,true);

      fwl[fnlf]:=rc; Inc(fnlf);
      Improve_triangulation;
    end;

    fnl:=ilvs+2
  end;
end;

function pack_tin: Integer;
var
  i,j,ndp: Integer; tp: PTriArray1;
begin
  tp:=fTris; ndp:=ftopn;

  j:=0;
  for i:=0 to ftrin-1 do begin

    with tp[i] do
    if p1 < ndp then
    if p2 < ndp then
    if p3 < ndp then begin
      tp[j]:=tp[i]; Inc(j)
    end
  end;

  ftrin:=j; Result:=j
end;

function tang_insert1(const lt,rb: TPoint): Integer;
var
  ip,ndp: Integer; pp: PPoint;
begin
  Result:=-1; ndp:=ftopn;

  Truncate(0);
  if Realloc(ndp+4) then begin

    pp:=PPoint(fTops);
    LoadBuffer(pp,ndp);

    begin_region(lt,rb);
    ftopn:=ndp;

    for ip:=0 to ndp-1 do begin

      if fCashDepth > 0 then
      if ip > fCashDepth then Cash_resize;

      if InsertVertex(ip) < 0 then
      Inc(fOutside);

      resize_buffers;
    end;
  end;

  Clear; Result:=pack_tin
end;

function tang_insert2: Integer;
var
  ip,ndp,i,i1,i2,i3: Integer;
  p1,p2,p3: TPoint; sp,vp: Double;
begin
  Result:=-1; ndp:=ftopn;

  i1:=0; i2:=0; i3:=0;
  p1:=PPoint(fTops + i1*fstride)^;

  for ip:=1 to ndp-1 do begin
    p2:=PPoint(fTops + ip*fstride)^;
    if not CompareMem(@p1,@p2,Sizeof(TPoint)) then
    begin i2:=ip; Break end;
  end;

  if i2 > 0 then
  for ip:=i2+1 to ndp-1 do begin

    p3:=PPoint(fTops + ip*fstride)^;

    spvp(p3,p1,p2, sp,vp);
    if Abs(vp) > Abs(sp) * epsln then begin
      spvp(p1,p2,p3, sp,vp);
      if vp < 0 then iSwap(i1,i2);
      i3:=ip; Break;
    end
  end;

  if i3 > 0 then begin

    SetRib(0,i1,i2,0,-1); fbp[0]:=0;
    SetRib(1,i2,i3,0,-1); fbp[1]:=1;
    SetRib(2,i3,i1,0,-1); fbp[2]:=2;

    SetTri(0, i1,i2,i3, 0,1,2);

    fribn:=3; fnl:=3; ftrin:=1;

    for ip:=Max(i1,i2)+1 to ndp-1 do begin

      if fCashDepth > 0 then
      if ip > fCashDepth then Cash_resize;

      if ip <> i3 then
      if InsertVertex(ip) < 0 then begin
        point_outside(ip); Inc(fOutside)
      end;

      resize_buffers;
    end
  end;

  Result:=trin;
end;

var
  lt,rb: TPoint;
begin
  Result:=-1; Clear_data;

  fTops:=Longint(ATops);
  ftopn:=Atopn; fstride:=AStride;

  if Atopn >= 3 then
  if Tops_bound(fTops,ftopn,fStride, lt,rb) then

  if BeginProcess(lt,rb,ftopn) then begin

    if true then
      Result:=tang_insert1(lt,rb)
    else
      Result:=tang_insert2;

    EndProcess;
  end;
end;

function TTriangulator1.Add_node(const v: VPoint): Integer;
var
  pp: PVertex;
begin
  Result:=Add(@v);

  pp:=First; fTops:=Longint(pp);
  ftopn:=Count; fStride:=12;

  if fCashDepth > 0 then
  if ftopn > fCashDepth then
  Cash_resize; resize_buffers;
end;

function TTriangulator1.Ins_node(const v: VPoint; it: Integer): Integer;
var
  rc: Integer;
begin
  Result:=Add_node(v);
  if Result > 0 then

  if it >= 0 then begin
    rc:=ThisTriangle(it,@v);
    if (rc > 0) and (rc <= 4) then
    splitTri(it,rc,Result);
  end else

  if InsertVertex(Result) < 0 then
    Inc(fOutside);
end;

function TTriangulator1.Verify: Integer;

function TriContainsRib(it,ir: Integer): Boolean;
begin
  Result:=true; if it >= 0 then with Tris[it] do
  Result:=(ir = r1) or (ir = r2) or (ir = r3)
end;

function RibContainsTri(ir,it: Integer): Boolean;
begin
  with fRibs[ir] do
  Result:=(it = t1) or (it = t2)
end;

var
  il,ir,it: Integer; rib: TRib1; tri: TTri1;
begin
  Result:=0;

  if fTops > 0 then
  if Assigned(fRibs) then
  if Assigned(fTris) then begin

    for ir:=0 to fribn-1 do begin
      rib:=fRibs[ir];

      if rib.p1 = rib.p2 then begin
        Result:=-1; Break
      end else

      if not TriContainsRib(rib.t1,ir)
      or not TriContainsRib(rib.t2,ir) then begin
        Result:=-2; Break
      end
    end;

    if Result = 0 then
    for it:=0 to trin-1 do begin
      tri:=Tris[it];
      if not RibContainsTri(tri.r1,it)
      or not RibContainsTri(tri.r2,it)
      or not RibContainsTri(tri.r2,it) then begin
        Result:=-3; Break
      end;

      if (tri.p1 = tri.p2)
      or (tri.p1 = tri.p3)
      or (tri.p2 = tri.p3) then begin
        Result:=-4; Break
      end
    end;

    if Result = 0 then
    for il:=0 to fnl-1 do begin

      rib:=fRibs[ fbp[il] ];
      if (rib.t1 >= 0) and (rib.t2 >= 0) then begin
        Result:=-5; Break
      end
    end
  end
end;

procedure TTriangulator1.log_model(log: TStrings);
begin
  log.Add('NPoints : '+IntToStr(ftopn));
  log.Add('TriCount: '+IntToStr(ftrin));
  log.Add('RibCount: '+IntToStr(fribn));
  log.Add('Verify  : '+IntToStr(fRetCode));
  log.Add('');

  log.Add('CashUsed : '+IntToStr(fCashUsed)+'%');

  if fOutside > 0 then
  log.Add('Outside  : '+IntToStr(fOutside));

  log.Add('TriTested: '+IntToStr(fTriTested));
  log.Add('Elapsed  : '+RealToStr(fElapsed/1000,1)+'sec');
end;

procedure TTriangulator1.dump_log;
var
  log: TStringList; fn: TShortstr;
begin
  log:=TStringList.Create;
  try
    log_model(log);
    StrWorkPath(fn,'tops.txt');
    log.SaveToFile(Strpas(fn));
  finally
    log.Free
  end
end;

procedure TTriangulator1.dump(Dest: PChar;
                              bw,bh,skip: Integer;
                              sys: psys; gb: PGPoly);

function tr_to_tr2(tp: PTriArray1; tp2: PTriArray2;
                   sp: PIntegers; skip: Integer): Integer;
var
  it,i: Integer; tr: TTri1; tr2: TTri2;
begin
  Result:=ftrin;

  if sp = nil then begin
    for it:=0 to ftrin-1 do begin
      tr:=tp[it];
      tr2.p1:=tr.p1; tr2.t1:=NextTri(it,tr.r2);
      tr2.p2:=tr.p2; tr2.t2:=NextTri(it,tr.r3);
      tr2.p3:=tr.p3; tr2.t3:=NextTri(it,tr.r1);
      tp2[it]:=tr2
    end
  end
  else begin
    for it:=0 to ftrin-1 do sp[it]:=it;

    for it:=0 to ftrin-1 do begin
      tr:=tp[it];

      if (tr.p1 < skip)
      or (tr.p2 < skip)
      or (tr.p3 < skip) then begin
        sp[it]:=-1;
        for i:=it+1 to ftrin-1 do
        Dec(sp[i]);
      end
    end;

    Result:=0;
    for it:=0 to ftrin-1 do
    if sp[it] >= 0 then begin
      tr:=tp[it];
      tr2.p1:=tr.p1 - skip;
      tr2.p2:=tr.p2 - skip;
      tr2.p3:=tr.p3 - skip;

      tr2.t1:=sp[ NextTri(it,tr.r2) ];
      tr2.t2:=sp[ NextTri(it,tr.r3) ];
      tr2.t3:=sp[ NextTri(it,tr.r1) ];

      tp2[Result]:=tr2; Inc(Result)
    end

  end
end;

var
  h,i,z1,z2,hn: Integer;
  tp: PTriArray1; rib: TRib1;
  lp: PVertexes; tp2: PTriArray2;

  vp: PVPoly;
  np: pxyz_array; np1: PIPoly;
  hs: TSize; hp,sp: PIntegers;

  hdr: ttin_hdr; buf: tbytes;
  v: VPoint;

begin
  FileErase(Dest);

  if topn-skip > 0 then
  if trin > 0 then begin

    vp:=xAllocPtr(topn * 12);
    np:=xAllocPtr(topn * Sizeof(txyz));
    tp2:=xAllocPtr(trin * Sizeof(TTri2));

    sp:=nil; if skip > 0 then
    sp:=xAllocInt(trin);

    if Assigned(vp) then
    if Assigned(tp2) then begin

      if sp = nil then skip:=0;

      Fillchar(hdr,Sizeof(hdr),0);
      hdr.mag:=tin_mag1; hdr.ver:=1;
      hdr.vn:=topn-skip;

      lp:=PVertexes(tops);
      lp:=@lp[skip];

      z1:=Round(lp[0].z*100); z2:=z1;

      for i:=0 to hdr.vn-1 do begin
        v.x:=lp[0].x;
        v.y:=lp[0].y;
        v.z:=Round(lp[0].z*100);

        if v.z < z1 then z1:=v.z;
        if v.z > z2 then z2:=v.z;

        vp[i]:=v; lp:=@lp[1]
      end;

      tp:=fTris;
      hdr.tn:=tr_to_tr2(tp,tp2,sp,skip);

      np1:=Pointer(np);
      tops_normals(np,np1, vp,hdr.vn,
                   @tp2[0],hdr.tn,Sizeof(TTri2),0);

      hs:=GetHashSize(hdr.tn,bw,bh);
      hn:=hs.cx * hs.cy;
      hp:=xAllocInt(hn * 2);

      if Assigned(hp) then begin

        h:=FileCreate(Strpas(Dest));
        if h > 0 then begin

          hdr.vp:=512;
          hdr.tp:=hdr.vp + hdr.vn*12;

          hdr.hash:=hs;
          hdr.hp:=hdr.tp + hdr.tn*24;

          if (skip = 0) and (fnl > 0) then
          hdr.bn:=fnl + 1;

          hdr.bp:=hdr.hp + hn*4;
          hdr.np:=hdr.bp + hdr.bn*4;

          hdr.stride:=12;
          hdr.bw:=bw; hdr.bh:=bh;

          hdr.zmin:=z1; hdr.zmax:=z2;

          if Assigned(sys) then begin
            hdr.sys_on:=1; hdr.sys:=sys^;
            for i:=0 to 3 do hdr.gb[i]:=gb[i]
          end;

          Fillchar(buf,Sizeof(buf),0);
          Move(hdr,buf,Sizeof(hdr));
          FileWrite(h,buf,512);

          FileWrite(h,vp^,hdr.vn * 12);
          FileWrite(h,tp2^,hdr.tn * Sizeof(TTri2));

          fill_hash(vp, tp2,hdr.tn, hp, hs.cx,hs.cy,bw,bh);
          FileWrite(h,hp^,hn * 4);

          if hdr.bn > 0 then begin
            for i:=0 to fnl-1 do begin
              rib:=fRibs[ fbp[i] ];
              FileWrite(h,rib.p1,4);
            end;

            FileWrite(h,rib.p2,4)
          end;

          FileWrite(h,np1^,hdr.vn * 4);
          FileClose(h)
        end
      end;

      xFreePtr(hp)
    end;

    xFreePtr(sp);
    xFreePtr(tp2);
    xFreePtr(np);
    xFreePtr(vp);
  end;
end;

end.
