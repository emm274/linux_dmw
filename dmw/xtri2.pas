unit xTri2; interface

uses
  Windows,Classes,Math,
  OTypes,OTypes1,XList,XTri,XIntf;

type
  PTri3 = ^TTri3; TTri3 = record
    p1,p2,p3, t1,t2,t3: Longint;
    minX,minY,maxX,maxY: Longint
  end;

  PTriArray3 = ^TTriArray3;
  TTriArray3 = array[0..1023] of TTri3;

  TTriList = class(TCustomList)
    constructor Create;
  end;

  TTriEvent = procedure(it: Integer) of object;

  TTriangulator2 = class(VPointList,ITriangulator2)

    constructor Create;
    destructor Destroy; override;

    procedure Clear_data; virtual;

    procedure Reset_stat;

    function BeginRegion(const lt,rb: TPoint;
                         bp: PVPoly; ndp: int): Boolean; stdcall;

    function EndRegion(pack: int): int; stdcall;

    function GetTriCount: int; stdcall;
    function GetTri(i: int; V: PVPoly): int; stdcall;
    function PickupTri(p: PPoint): int; stdcall;

    function Ins_node(const v: VPoint; it: int): int; stdcall;

    function GetTri2(i: int; tp: PIntegers): int; stdcall;

    procedure print(Dest: PChar); stdcall;

    function GetBound(Bound: IPointList): int; stdcall;

    function Trunc_last: bool;

    function Add_node(const v: VPoint): Integer;

    function Clean(baseCount: Integer;
                   dist,angle: Double;
                   loopK: Integer): Integer;

    function Clip(bp: PLPoly; n: int): int;

    function Calculate(ATops: Pointer;
                       Atopn,AStride,ASkip: Integer): Integer;

    function Verify: Integer;

    procedure log_model(const log: ILog); virtual;

    procedure dump(Dest: PChar;
                   bw,bh,skip: Integer;
                   sys: psys; gb: PGPoly);

    procedure zval_to_int;
    procedure zval_to_real;

    function filter(cb: TIntegerFunc): int;

  protected
    function GetStatCount: Integer; virtual;

    procedure begin_region(const lt,rb: TPoint; bp: PVPoly);

    function AddTri(p1,p2,p3, t1,t2,t3: int): int;

    function side_to_top(var tr: TTri3; it: Integer): Boolean;

    function LocateTri(pp: PPoint;
                       out Ind: Integer): Integer;

    procedure splitTri(it,rc,ip: Integer);

    function ThisTriangle(it: Integer; pp: PPoint): Integer;

    function BadPare(i1,i2,i3,i4: Integer): Boolean; virtual;

    procedure Improve_triangulation;

  private
    fTriList: TTriList;
    fwlList: TPointList;

    fTops: Pointer;
    ftopn: Integer;
    fStride: Integer;
    fTriSize: Integer;

    fwl: PLPoly;
    fnwl: Integer;
    fnlf: Integer;

    fTris: PTriArray3;
    ftrin: Integer;

    fTriTested1: Int64;
    fTriTested2: Integer;

    fOutside: Integer;

    fElapsed: Longint;
    fRetCode: Integer;

    fLastTri: Integer;

    fcash: PIntegers;
    fhs_m2: Double;
    fhs_r: Double;
    fhs: TSize;

    fOrg: TPoint;
    fRgn: TSize;
    fhp_kx: Double;
    fhp_ky: Double;

    fImprove_enabled: Boolean;
    fCash_enabled: Boolean;

    fOnTri: TTriEvent;
    fOnTriList: TNotifyEvent;

    function BeginProcess(const lt,rb: TPoint;
                          ndp: Integer): Boolean;

    function EndProcess: Integer;

    function Get_VPoint(I: Integer): VPoint;
    function Get_Point(I: Integer): TPoint;
    procedure Set_Point(I: Integer; const P: TPoint);

    procedure SetTri(it, p1,p2,p3, t1,t2,t3: Integer);

    procedure tri_relink(it,it1,it2: Integer);

    function Cash_init(const lt,rb: TPoint): Boolean;

    procedure cash_fill(it1,it2: Integer);
    procedure cash_tri(it: Integer);

    procedure Cash_resize;

    function ResizeTri: bool;

    procedure resize_buffers(np: Integer);

    function InsertVertex(ip: Integer): Integer;

    function pack_tin(skip: Integer): Integer;
    function comp_tin(sp: PIntegers): Integer;

    procedure tri_off(it: Integer);

  public
    property Tops: Pointer read fTops;
    property topn: Integer read ftopn;
    property Stride: Integer read fStride;
    property TriSize: Integer read fTriSize;

    property Tris: PTriArray3 read fTris;
    property trin: Integer read ftrin;

    property TriList: TTriList read fTriList;

    property VPoints[I: Integer]: VPoint read Get_VPoint;

    property Points[I: Integer]: TPoint read Get_Point
                                        write Set_Point;

    property Improve_enabled: Boolean write fImprove_enabled;
    property Cash_enabled: Boolean write fCash_enabled;

    property RetCode: Integer read fRetCode;

    property OnTri: TTriEvent write fOnTri;
    property OnTriList: TNotifyEvent write fOnTriList;
  end;

function GetTriangulator2(out Obj): HResult; stdcall;

implementation

uses
  Sysutils,
  Convert,OFiles,
  XLine,XPoly,xutils;

procedure rotate_tri1(var tr: TTri3);
var
  t1,t2: TTri3;
begin
  t1:=tr;
  t2.p1:=t1.p2; t2.p2:=t1.p3; t2.p3:=t1.p1;
  t2.t1:=t1.t2; t2.t2:=t1.t3; t2.t3:=t1.t1;
  tr:=t2;
end;

procedure rotate_tri2(var tr: TTri3);
var
  t1,t2: TTri3;
begin
  t1:=tr;
  t2.p1:=t1.p3; t2.p2:=t1.p1; t2.p3:=t1.p2;
  t2.t1:=t1.t3; t2.t2:=t1.t1; t2.t3:=t1.t2;
  tr:=t2;
end;

function GetTriangulator2(out Obj): HResult; stdcall;
var
  tobj: TTriangulator2;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  tobj:=TTriangulator2.Create;
  try
    if tobj.GetInterface(ITriangulator2,Obj) then
    begin Result:=S_OK; tobj:=nil end
  finally
    tobj.Free
  end
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

constructor TTriList.Create;
begin
  inherited Create(Sizeof(TTri3),1024*64)
end;

constructor TTriangulator2.Create;
begin
  inherited Create(1024*64);

  fTriList:=TTriList.Create;
  fwlList:=TPointlist.Create(1024*64);

  fhs_r:=8; fTriSize:=Sizeof(TTri3);
  fImprove_enabled:=true
end;

destructor TTriangulator2.Destroy;
begin
  fcash:=xFreePtr(fcash);

  fwlList.Free;
  fTriList.Free;

  inherited
end;

procedure TTriangulator2.Clear_data;
begin
  fcash:=xFreePtr(fcash);

  fwlList.Clear;
  fTriList.Clear;

  fTris:=nil; ftrin:=0;

  fwl:=nil; fnwl:=0; fnlf:=0;

  fOutside:=0;

  fTriTested1:=1;
  fTriTested2:=1;

  fRetCode:=0;

  fImprove_enabled:=true;
  Clear;
end;

function TTriangulator2.Get_VPoint(I: Integer): VPoint;
begin
  Result:=PVPoint(fTops + I*fStride)^
end;

function TTriangulator2.Get_Point(I: Integer): TPoint;
begin
  Result:=PPoint(fTops + I*fStride)^
end;

procedure TTriangulator2.Set_Point(I: Integer; const P: TPoint);
begin
  PPoint(fTops + I*fStride)^:=P
end;

procedure TTriangulator2.SetTri(it, p1,p2,p3, t1,t2,t3: Integer);
var
  tr: TTri3; v1,v2,v3: TPoint;
begin
  tr.p1:=p1; tr.p2:=p2; tr.p3:=p3;
  tr.t1:=t1; tr.t2:=t2; tr.t3:=t3;

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

  if Assigned(fOnTri) then fOnTri(it)
end;

procedure TTriangulator2.zval_to_int;
var
  i: int; p: PByte;
begin
  p:=PByte(fTops+8);
  for i:=1 to ftopn do begin
    PInteger(p)^:=Round(PFloat(p)^*100); Inc(p,12)
  end
end;

procedure TTriangulator2.zval_to_real;
var
  i: int; p: PByte;
begin
  p:=PByte(fTops+8);
  for i:=1 to ftopn do begin
    PFloat(p)^:=PInteger(p)^ / 100; Inc(p,12)
  end
end;

function TTriangulator2.BeginProcess(const lt,rb: TPoint;
                                     ndp: Integer): Boolean;
var
  n3: Integer;
begin
  Result:=false;

  fImprove_enabled:=true;

  ndp:=Max(1024,ndp); n3:=ndp*3;

  fnwl:=10*ndp; fnlf:=0;

  if fwlList.Resize(fnwl) then
  if fTriList.Resize(n3) then begin
    fwl:=fwlList.First;
    Dec(fnwl,10);

    fTris:=fTriList.First;

    if Assigned(fOnTriList) then
    fOnTriList(nil);

    fElapsed:=GetTickCount;

    Cash_init(lt,rb); Result:=true
  end
end;

function TTriangulator2.EndProcess: Integer;
var
  n: Integer;
begin
  Result:=0; fRetCode:=Verify;

  n:=GetStatCount;
  if n > 0 then begin
    fOutside:=Round(fOutside / n * 100);
    Result:=ftrin
  end;

  fElapsed:=GetTickCount - fElapsed;
end;

function TTriangulator2.GetStatCount: Integer;
begin
  Result:=ftopn
end;

procedure TTriangulator2.Reset_stat;
begin
  fElapsed:=GetTickCount;
  fTriTested1:=1; fTriTested2:=1;
  fOutside:=0;
end;

function TTriangulator2.Cash_init(const lt,rb: TPoint): Boolean;
var
  i,hn: Integer;
begin
  fRgn.cx:=rb.X-lt.X+1;
  fRgn.cy:=rb.Y-lt.Y+1;

  fcash:=xFreePtr(fcash);
  fhs.cx:=2; fhs.cy:=2;
  fhs_m2:=2*2;

  fOrg:=lt;
  fhp_kx:=fhs.cx / fRgn.cx;
  fhp_ky:=fhs.cy / fRgn.cy;

  if fCash_enabled then begin
    hn:=fhs.cx * fhs.cy;
    fcash:=xAllocInt(hn * 2);

    if Assigned(fcash) then begin
      for i:=0 to hn-1 do fcash[i]:=-1;
      Result:=true
    end
  end
end;

procedure TTriangulator2.Cash_resize;
var
  c,r,r1,hn,it,bx: Integer;
  sz: TSize; buf,bp: PIntegers;
begin
  sz.cx:=fhs.cx*2;
  sz.cy:=fhs.cy*2;

  hn:=sz.cx * sz.cy;
  buf:=xAllocInt(hn * 2);
  if Assigned(buf) then begin

    bp:=fcash;
    for r:=0 to fhs.cy-1 do begin
      r1:=r+r;

      for c:=0 to fhs.cx-1 do begin
        it:=bp[0]; bp:=@bp[1];

        bx:=r1*fhs.cx + (c+c);
        buf[bx]:=it; buf[bx+1]:=it;
        Inc(bx,fhs.cx);
        buf[bx]:=it; buf[bx+1]:=it;
      end;
    end;

    xFreePtr(fcash); fcash:=buf;
    fhs:=sz; fhs_m2:=fhs_m2*4;

    fhp_kx:=fhs.cx / fRgn.cx;
    fhp_ky:=fhs.cy / fRgn.cy;
  end
end;

procedure TTriangulator2.cash_fill(it1,it2: Integer);
var
  i,hn: Integer;
begin
  if Assigned(fcash) then begin
    hn:=fhs.cx * fhs.cy;
    for i:=0 to hn-1 do fcash[i]:=it1;
    if it2 >= 0 then cash_tri(it2)
  end;
end;

procedure TTriangulator2.cash_tri(it: Integer);
var
  x1,x2,x,y: Integer;
  p1,p2,p3,t: TPoint; tr: TTri3;
  ctg12,ctg13,ctg23: Double;
begin
  tr:=fTris[it];
  p1:=PPoint(fTops + tr.p1*fstride)^;
  p2:=PPoint(fTops + tr.p2*fstride)^;
  p3:=PPoint(fTops + tr.p3*fstride)^;

  p1.X:=Trunc((p1.X - fOrg.X) * fhp_kx);
  p1.Y:=Trunc((p1.Y - fOrg.Y) * fhp_ky);
  p2.X:=Trunc((p2.X - fOrg.X) * fhp_kx);
  p2.Y:=Trunc((p2.Y - fOrg.Y) * fhp_ky);
  p3.X:=Trunc((p3.X - fOrg.X) * fhp_kx);
  p3.Y:=Trunc((p3.Y - fOrg.Y) * fhp_ky);

  if p2.Y < p1.Y then begin
    t:=p1; p1:=p2; p2:=t
  end;

  if p3.Y < p1.Y then begin
    t:=p1; p1:=p3; p3:=t
  end;

  if p3.Y < p2.Y then begin
    t:=p2; p2:=p3; p3:=t
  end;

  if p1.Y < p3.Y then begin

    ctg13:=(p3.X-p1.X)/(p3.Y-p1.Y);

    if p1.Y < p2.Y then begin

      ctg12:=(p2.X-p1.X)/(p2.Y-p1.Y);

      for y:=p1.Y to p2.Y do begin
        x1:=p1.X + Trunc(y*ctg12);
        x2:=p1.X + Trunc(y*ctg13);

        if (y >= 0) and (y < fhs.cy) then

        if x1 < x2 then begin
          for x:=x1 to x2 do
          if (x >= 0) and (x < fhs.cx) then
          fcash[y*fhs.cx + x]:=it
        end
        else begin
          for x:=x2 to x1 do
          if (x >= 0) and (x < fhs.cx) then
          fcash[y*fhs.cx + x]:=it
        end
      end;
    end;

    if p2.Y < p3.Y then begin

      ctg23:=(p3.X-p2.X)/(p3.Y-p2.Y);
      for y:=p2.Y to p3.Y do begin
        x1:=p2.X + Trunc(y*ctg23);
        x2:=p1.X + Trunc(y*ctg13);

        if (y >= 0) and (y < fhs.cy) then

        if x1 < x2 then begin
          for x:=x1 to x2 do
          if (x >= 0) and (x < fhs.cx) then
          fcash[y*fhs.cx + x]:=it
        end
        else begin
          for x:=x2 to x1 do
          if (x >= 0) and (x < fhs.cx) then
          fcash[y*fhs.cx + x]:=it
        end
      end;
    end

  end
end;

function TTriangulator2.AddTri(p1,p2,p3, t1,t2,t3: int): int;
begin
  resize_buffers(Count);
  if ftrin < fTriList.Count then begin
    Result:=ftrin; Inc(ftrin);
    SetTri(Result, p1,p2,p3, t1,t2,t3);
  end
end;

function TTriangulator2.ResizeTri: bool;
begin
  Result:=true;
  if ftrin + 100 > fTriList.Count then begin
    fTriList.Extend(ftrin + 100000);
    fTris:=fTriList.First;

    if Assigned(fOnTriList) then
    fOnTriList(nil)
  end;
end;

procedure TTriangulator2.resize_buffers(np: Integer);
var
  ndp: Integer;
begin
  ResizeTri;

  if fwlList.Count < 64000 then
  if ftopn + 100 > fwlList.Count div 12 then begin

    ndp:=ftopn + 10000;
    fnwl:=10*ndp;

    fwlList.Extend(fnwl);
    fwl:=fwlList.First;
    Dec(fnwl,10);
  end;

  if Assigned(fcash) then
  if np > fhs_r*fhs_m2 then
  Cash_resize;
end;

function TTriangulator2.side_to_top(var tr: TTri3; it: Integer): Boolean;
var
  tmp: TTri3;
begin
  Result:=false;
  if tr.t1 = it then Result:=true
  else
  if tr.t2 = it then begin tmp:=tr;
    tr.p1:=tmp.p2; tr.p2:=tmp.p3; tr.p3:=tmp.p1;
    tr.t1:=tmp.t2; tr.t2:=tmp.t3; tr.t3:=tmp.t1;
    Result:=true
  end else
  if tr.t3 = it then begin tmp:=tr;
    tr.p1:=tmp.p3; tr.p2:=tmp.p1; tr.p3:=tmp.p2;
    tr.t1:=tmp.t3; tr.t2:=tmp.t1; tr.t3:=tmp.t2;
    Result:=true
  end
end;

procedure TTriangulator2.tri_relink(it,it1,it2: Integer);
begin
  if it >= 0 then with fTris[it] do

  if t1 = it1 then t1:=it2 else
  if t2 = it1 then t2:=it2 else
  if t3 = it1 then t3:=it2
end;

function TTriangulator2.BadPare(i1,i2,i3,i4: Integer): Boolean;
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

procedure TTriangulator2.Improve_triangulation;
const
  nrep = 200;
var
  irep,ilf,nlfc, it1,it2: Integer;
  p: TPoint; t1,t2: TTri3;
begin
  if fImprove_enabled then
  for irep:=1 to nrep do begin

    nlfc:=fnlf;
    for ilf:=0 to fnlf-1 do begin

      p:=fwl[ilf];

      it1:=p.X; it2:=p.Y;
      if (it1 >= 0) and (it2 >= 0) then begin

        t1:=Tris[it1]; t2:=Tris[it2];

        if side_to_top(t1,it2) then
        if side_to_top(t2,it1) then

        if BadPare(t1.p2,t1.p3,t1.p1,t2.p1) then begin

          SetTri(it1, t1.p1,t2.p1,t1.p3, t2.t3,t1.t2,it2);
          SetTri(it2, t1.p1,t1.p2,t2.p1, t2.t2,it1,t1.t3);

          tri_relink(t1.t3, it1,it2);
          tri_relink(t2.t3, it2,it1);

          if nlfc + 4 <= fnwl then begin
            fwl[nlfc]:=Point(it1,t2.t3); Inc(nlfc);
            fwl[nlfc]:=Point(it1,t1.t2); Inc(nlfc);
            fwl[nlfc]:=Point(it2,t2.t2); Inc(nlfc);
            fwl[nlfc]:=Point(it2,t1.t3); Inc(nlfc);
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

function TTriangulator2.ThisTriangle(it: Integer; pp: PPoint): Integer;
var
  tr: TTri3; ip: Int64;
  p: TPoint; p1,p2,p3: TPoint;
  rc1,rc2,rc3: Integer;
begin
  Result:=0; p:=pp^; ip:=Int64(p);

  tr:=fTris[it];
  if (p.X >= tr.minX) and (p.X <= tr.maxX) then
  if (p.Y >= tr.minY) and (p.Y <= tr.maxY) then begin

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

  end
end;

function TTriangulator2.LocateTri(pp: PPoint;
                                  out Ind: Integer): Integer;
var
  ix,iy,ih,it,it1,it2,k,rc: Integer;
  p,p1,p2,p3: TPoint; ip: Int64; tr: TTri3;
begin
  Result:=-1; Ind:=-1;

  p:=pp^; ip:=Int64(p);

  ix:=pp.X - fOrg.X;
  iy:=pp.Y - fOrg.Y;

  if (ix >= 0) and (ix < fRgn.cx) then
  if (iy >= 0) and (iy < fRgn.cy) then begin

    it:=fLastTri;
    if Assigned(fcash) then begin
      ix:=Trunc(ix * fhp_kx);
      if ix < 0 then ix:=0;
      if ix >= fhs.cx then ix:=fhs.cx-1;

      iy:=Trunc(iy * fhp_ky);
      if iy < 0 then iy:=0;
      if iy >= fhs.cy then iy:=fhs.cy-1;

      ih:=iy*fhs.cx + ix;
      it:=fcash[ih];
    end;

    it1:=-2; k:=1;

    while it >= 0 do begin

      tr:=fTris[it]; it2:=it; it:=-1;

      p1:=PPoint(fTops + tr.p1*fstride)^;
      p2:=PPoint(fTops + tr.p2*fstride)^;
      p3:=PPoint(fTops + tr.p3*fstride)^;

      rc:=-1; if tr.t3 <> it1 then
      rc:=iline(@p, @p1,@p2);

      if rc = 1 then
        it:=tr.t3
      else begin

        if rc = 2 then begin rc:=1;
          if ip = Int64(p1) then Inc(rc,10) else
          if ip = Int64(p2) then Inc(rc,20);
          Result:=it2; Break
        end;

        rc:=-1; if tr.t1 <> it1 then
        rc:=iline(@p, @p2,@p3);

        if rc = 1 then
          it:=tr.t1
        else begin
          if rc = 2 then begin rc:=2;
            if ip = Int64(p3) then Inc(rc,30);
            Result:=it2; Break
          end;

          rc:=-1; if tr.t2 <> it1 then
          rc:=iline(@p, @p3,@p1);

          if rc = 1 then
            it:=tr.t2
          else begin
            if rc = 2 then rc:=3 else rc:=4;
            Result:=it2; Break
          end
        end
      end;

      it1:=it2; Inc(k)
    end;

    Inc(fTriTested1,k);
    Inc(fTriTested2);

    if Result >= 0 then begin
      if Assigned(fcash) then
      fcash[ih]:=Result;
      fLastTri:=Result;
    end;

    Ind:=rc
  end
end;

procedure TTriangulator2.splitTri(it,rc,ip: Integer);

procedure split3(it,ip: Integer);
var
  t1,t2,t3: Integer; tr: TTri3;
begin
  tr:=fTris[it];

  t1:=it; t2:=ftrin; t3:=ftrin+1;

  SetTri(t1, tr.p1,tr.p2,ip, t2,t3,tr.t3);
  SetTri(t2, tr.p2,tr.p3,ip, t3,t1,tr.t1);
  SetTri(t3, tr.p3,tr.p1,ip, t1,t2,tr.t2);
  Inc(ftrin,2);

  tri_relink(tr.t1,it,t2);
  tri_relink(tr.t2,it,t3);

  fwl[fnlf]:=Point(t1,tr.t3); Inc(fnlf);
  fwl[fnlf]:=Point(t2,tr.t1); Inc(fnlf);
  fwl[fnlf]:=Point(t3,tr.t2); Inc(fnlf);

  Improve_triangulation;
end;

procedure split2(it1,rc,ip: int);
var
  ir,it2, t1,t2,t3,t4, r1,r2,r3,r4: int;
  tr1,tr2,tmp: TTri3;
begin
  tr1:=fTris[it1];

  if rc = 2 then begin tmp:=tr1;
    tr1.p1:=tmp.p2; tr1.p2:=tmp.p3; tr1.p3:=tmp.p1;
    tr1.t1:=tmp.t2; tr1.t2:=tmp.t3; tr1.t3:=tmp.t1;
  end else
  if rc = 3 then begin tmp:=tr1;
    tr1.p1:=tmp.p3; tr1.p2:=tmp.p1; tr1.p3:=tmp.p2;
    tr1.t1:=tmp.t3; tr1.t2:=tmp.t1; tr1.t3:=tmp.t2;
  end;

  it2:=tr1.t3;
  if it2 < 0 then begin

    t1:=it1; t2:=ftrin;

    SetTri(t1, tr1.p1,ip,tr1.p3, t2,tr1.t2,-1);
    SetTri(t2, ip,tr1.p2,tr1.p3, tr1.t1,t1,-1);
    Inc(ftrin);

    tri_relink(tr1.t1,it1,t2);

    fwl[fnlf]:=Point(t1,tr1.t2); Inc(fnlf);
    fwl[fnlf]:=Point(t2,tr1.t1); Inc(fnlf);

    Improve_triangulation;
  end
  else begin
    tr2:=fTris[it2];

    if side_to_top(tr2,it1) then begin

      t1:=it1; t2:=ftrin;
      t3:=it2; t4:=ftrin+1;

      SetTri(t1, tr1.p1,ip,tr1.p3, t2,tr1.t2,t4);
      SetTri(t2, ip,tr1.p2,tr1.p3, tr1.t1,t1,t3);
      SetTri(t3, tr2.p2,ip,tr2.p1, t4,tr2.t3,t2);
      SetTri(t4, ip,tr2.p3,tr2.p1, tr2.t2,t3,t1);
      Inc(ftrin,2);

      tri_relink(tr1.t1,it1,t2);
      tri_relink(tr2.t2,it2,t4);

      fwl[fnlf]:=Point(t1,tr1.t2); Inc(fnlf);
      fwl[fnlf]:=Point(t2,tr1.t1); Inc(fnlf);
      fwl[fnlf]:=Point(t3,tr2.t3); Inc(fnlf);
      fwl[fnlf]:=Point(t4,tr2.t2); Inc(fnlf);
      fwl[fnlf]:=Point(t1,t4); Inc(fnlf);
      fwl[fnlf]:=Point(t2,t3); Inc(fnlf);

      Improve_triangulation
    end
  end
end;

begin
  if ResizeTri then
  if rc = 4 then split3(it,ip) else
  if rc < 4 then split2(it,rc,ip);
end;

function TTriangulator2.InsertVertex(ip: Integer): Integer;
var
  pp: PPoint; rc: Integer;
begin
  pp:=PPoint(fTops + ip*fstride);
  Result:=LocateTri(pp,rc);

  if Result >= 0 then
  splitTri(Result,rc,ip)
end;

function TTriangulator2.BeginRegion(const lt,rb: TPoint;
                                    bp: PVPoly; ndp: Integer): Boolean;
begin
  Result:=false; Clear_data;
  if ndp = 0 then ndp:=100000;
  if BeginProcess(lt,rb,ndp) then begin
    begin_region(lt,rb,bp);
    Result:=true
  end
end;

procedure TTriangulator2.begin_region(const lt,rb: TPoint; bp: PVPoly);
var
  i1,i2,i3,i4,z: Integer;
begin
  if Assigned(bp) then begin
    i1:=Add(@bp[0]);
    i2:=Add(@bp[1]);
    i3:=Add(@bp[2]);
    i4:=Add(@bp[3])
  end
  else begin z:=znil1;
    i1:=AddItem(lt.X,lt.Y,z);
    i2:=AddItem(rb.X,lt.Y,z);
    i3:=AddItem(rb.X,rb.Y,z);
    i4:=AddItem(lt.X,rb.Y,z);
  end;

  fTops:=First;
  ftopn:=Count;
  fStride:=12;

  SetTri(0, i1,i2,i4, 1,-1,-1);
  SetTri(1, i2,i3,i4, -1,0,-1);
  ftrin:=2; cash_fill(0,1);
  fLastTri:=0;
end;

function TTriangulator2.EndRegion(pack: int): Integer;
begin
  if pack >= 0 then
  Result:=pack_tin(4);
  EndProcess; Result:=ftrin
end;

procedure TTriangulator2.print(Dest: PChar);
var
  txt: TTextfile;
  it: int; tr: TTri3;
  p1,p2,p3: PPoint;
  s: String;
begin
  txt:=TTextfile.Create;
  try
    if txt.Make(Dest) then
    for it:=0 to ftrin-1 do begin
      tr:=fTris[it];
      p1:=PPoint(fTops + tr.p1*fstride);
      p2:=PPoint(fTops + tr.p2*fstride);
      p3:=PPoint(fTops + tr.p3*fstride);

      s:=Format('%d:'#9'{%d,%d} {%d,%d} {%d,%d}',
      [it,p1.X,p1.Y,p2.X,p2.Y,p3.X,p3.Y]);

      txt.WriteStr(s);
    end
  finally
    txt.Free
  end
end;

function TTriangulator2.GetTriCount: Integer;
begin
  Result:=ftrin
end;

function TTriangulator2.GetTri(i: int; V: PVPoly): int;
var
  tr: TTri3;
begin
  Result:=-1;
  if (i >= 0) and (i < ftrin) then begin
    tr:=fTris[i];
    V[0]:=PVPoint(fTops + tr.p1*fstride)^;
    V[1]:=PVPoint(fTops + tr.p2*fstride)^;
    V[2]:=PVPoint(fTops + tr.p3*fstride)^;
    Result:=i
  end
end;

function TTriangulator2.GetTri2(i: int; tp: PIntegers): int;
var
  tr: TTri3;
begin
  Result:=-1;
  if (i >= 0) and (i < ftrin) then begin
    tr:=fTris[i];
    tp[0]:=tr.t1;
    tp[1]:=tr.t2;
    tp[2]:=tr.t3;
    Result:=i
  end
end;

function TTriangulator2.GetBound(Bound: IPointList): int;

function bound_continue(Bound: IPointList; i: int): int;
begin
  with PVPoint(fTops + i*fstride)^ do
  Bound.AddPoint(-1,x,y);
  Result:=i
end;

function set_t1(var tr: TTri3; t1: int): bool;
var
  t: TTri3;
begin
  Result:=false; t:=tr;

  if t.t1 = t1 then
    Result:=true
  else
  if t.t2 = t1 then begin
    rotate_tri1(t); Result:=true
  end else
  if t.t3 = t1 then begin
    rotate_tri2(t); Result:=true
  end;

  tr:=t;
end;

var
  it,it0,it1,it2,i1,i2,rc: int;
  tp: PTriArray3; tr1,tr2: TTri3;
begin
  Bound.Clear;

  tp:=fTris; it0:=-1; i1:=-1; i2:=-1;

  for it:=0 to ftrin-1 do begin
    tr1:=tp[it];
    if tr1.t1 >= 0 then
    if tr1.t2 < 0 then
      rotate_tri1(tr1)
    else
    if tr1.t3 < 0 then
      rotate_tri2(tr1);

    if tr1.t1 < 0 then begin
      i1:=bound_continue(Bound,tr1.p2);
      i2:=bound_continue(Bound,tr1.p3);

      if tr1.t2 < 0 then begin
        rotate_tri1(tr1);
        i2:=bound_continue(Bound,tr1.p3);
      end;

      if tr1.t2 < 0 then begin
        rotate_tri1(tr1);
        i2:=bound_continue(Bound,tr1.p3);
      end;

      it0:=it; it1:=it; Break
    end
  end;

  rc:=-1;
  if i1 >= 0 then
  if i1 <> i2 then
  if it0 >= 0 then
  while tr1.t2 >= 0 do begin

    it2:=tr1.t2; tr2:=tp[it2];
    if not set_t1(tr2,it1) then begin
      rc:=-1; Break;
    end;

    if tr2.t2 < 0 then begin
      rotate_tri1(tr2);
      i2:=bound_continue(Bound,tr2.p3);
      if i1 = i2 then begin
        rc:=0; Break;
      end;

      if tr2.t2 < 0 then begin
        rotate_tri1(tr2);
        i2:=bound_continue(Bound,tr2.p3);
        if i1 = i2 then begin
          rc:=0; Break;
        end
      end
    end;

    if it2 = it0 then begin
      rc:=1; Break;
    end;

    tr1:=tr2; it1:=it2
  end;

  Result:=Bound.GetCount
end;

function TTriangulator2.Trunc_last: bool;
begin
  Result:=false;
  if ftrin > 0 then begin
    Dec(ftrin); Result:=true
  end
end;

function TTriangulator2.PickupTri(p: PPoint): int;
var
  it,rc: int; tr: TTri3;
  a,b: TPoint; p1,p2,p3: PPoint;
  dx,dy,dist,tmp: Int64;
begin
  Result:=LocateTri(p,rc);

  if Result < 0 then begin

    a:=p^; dist:=MaxLongint;

    for it:=0 to ftrin-1 do begin

      tr:=fTris[it];
      p1:=PPoint(fTops + tr.p1*fstride);
      p2:=PPoint(fTops + tr.p2*fstride);
      p3:=PPoint(fTops + tr.p3*fstride);

      b.X:=(p1.X+p2.X+p3.X) div 3;
      b.Y:=(p1.Y+p2.Y+p3.Y) div 3;

      dx:=Abs(b.X-a.X); dy:=Abs(b.Y-a.Y);

      tmp:=dx*dx + dy*dy;
      if (Result < 0) or (tmp < dist) then begin
        Result:=it; dist:=tmp
      end
    end
  end
end;

function TTriangulator2.Clean(baseCount: Integer;
                              dist,angle: Double;
                              loopK: Integer): Integer;

function Is_tri_off(it,baseCount: Integer;
                    dist,minCos: Double): Boolean;

function Is_sharp(ax,ay,bx,by: Integer;
                  l1,l2,minCos: Double): Boolean;
var
  cos: Double;
begin
  cos:=(Int64(ax)*bx + Int64(ay)*by) / (l1*l2);
  Result:=(cos >= 0) and (cos < minCos)
end;

var
  vp: PVPoly; v: TPoint3; tr: TTri3;
  a,b,c: TPoint; an,bn,cn: Double;
begin
  Result:=false;

  tr:=fTris[it]; with tr do
  if (t1 < 0) or (t2 < 0) or (t3 < 0) then begin

    vp:=fTops;
    v[0]:=PPoint(@vp[p1])^;
    v[1]:=PPoint(@vp[p2])^;
    v[2]:=PPoint(@vp[p3])^;

    if not Triangle_Verify(@v,0.002) then
      Result:=true
    else

    if dist > 0 then begin

      a.X:=v[1].x-v[0].x;
      a.Y:=v[1].y-v[0].y;
      an:=Hypot(a.X,a.Y);

      b.X:=v[2].x-v[1].x;
      b.Y:=v[2].y-v[1].y;
      bn:=Hypot(b.X,b.Y);

      c.X:=v[0].x-v[2].x;
      c.Y:=v[0].y-v[2].y;
      cn:=Hypot(c.X,c.Y);

      if not Result and (t1 < 0) then
      if (p2 < baseCount) and (p3 < baseCount) then
      if bn > dist then
        Result:=true
      else
      if minCos > 0 then
      if bn > Min(an,cn)*4 then

      if an > cn then
        Result:=Is_sharp(b.X,b.Y,-a.X,-a.Y, bn,an,minCos)
      else
        Result:=Is_sharp(c.X,c.Y,-b.X,-b.Y, cn,bn,minCos);

      if not Result and (t2 < 0) then
      if (p3 < baseCount) and (p1 < baseCount) then
      if cn > dist then
        Result:=true
      else
      if minCos > 0 then
      if cn > Min(an,bn)*4 then

      if an > bn then
        Result:=Is_sharp(a.X,a.Y,-c.X,-c.Y, an,cn,minCos)
      else
        Result:=Is_sharp(c.X,c.Y,-b.X,-b.Y, cn,bn,minCos);

      if not Result and (t3 < 0) then
      if (p1 < baseCount) and (p2 < baseCount) then
      if an > dist then
        Result:=true
      else
      if minCos > 0 then
      if an > Min(bn,cn)*4 then

      if bn > cn then
        Result:=Is_sharp(b.X,b.Y,-a.X,-a.Y, bn,an,minCos)
      else
        Result:=Is_sharp(a.X,a.Y,-c.X,-c.Y, an,cn,minCos);

    end
  end
end;

var
  sp: PIntegers;
  tp: PTriArray3; tr: TTri3;
  i,j,k,k1,loop: Integer;
  minCos: Double;
begin
  if trin > 1 then begin
    sp:=xAllocInt(trin);
    if Assigned(sp) then begin

      minCos:=-1; if Angle > 0 then
      minCos:=Cos(Angle/180*Pi);

      for i:=0 to ftrin-1 do sp[i]:=0;
      if baseCount < 0 then baseCount:=ftopn;

      tp:=fTris; loop:=0; k:=0;

      while true do begin k1:=0;

        for i:=0 to ftrin-1 do
        if sp[i] = 0 then

        if Is_tri_off(i,baseCount,dist,minCos) then begin
          tri_off(i); Inc(k1); sp[i]:=-1;

          tr:=fTris[i];

          j:=tr.t1; if (j >= 0) and (sp[j] = 0) then
          if Is_tri_off(j,baseCount,dist,minCos) then
          begin tri_off(j); Inc(k1); sp[j]:=-1 end;

          j:=tr.t2; if (j >= 0) and (sp[j] = 0) then
          if Is_tri_off(j,baseCount,dist,minCos) then
          begin tri_off(j); Inc(k1); sp[j]:=-1 end;

          j:=tr.t3; if (j >= 0) and (sp[j] = 0) then
          if Is_tri_off(j,baseCount,dist,minCos) then
          begin tri_off(j); Inc(k1); sp[j]:=-1 end;
        end;

        Inc(k,k1); if k1 = 0 then Break;
        Inc(loop); if loop >= loopK then Break;
      end;

      if k > 0 then begin

        j:=0;
        for i:=0 to ftrin-1 do
        if sp[i] = 0 then begin
          sp[i]:=j; Inc(j)
        end;

        comp_tin(sp);
      end
    end;

    xFreeptr(sp)
  end;

  Result:=ftrin
end;

function TTriangulator2.Clip(bp: PLPoly; n: int): int;
var
  sp: PIntegers; i,j,old,rc: Integer;
  tp: PTriArray3; vp: PVPoly; tr: TTri3;
  p1,p2,p3: TPoint; c: TGauss;
begin
  if trin > 1 then begin
    sp:=xAllocInt(trin);
    if Assigned(sp) then repeat

      vp:=fTops; old:=ftrin;

      tp:=fTris; j:=0;
      for i:=0 to ftrin-1 do begin

        sp[i]:=-1; rc:=1; tr:=tp[i];

        if (tr.t1 < 0) or (tr.t2 < 0) or (tr.t3 < 0) then begin

          p1:=PPoint(@vp[tr.p1])^;
          p2:=PPoint(@vp[tr.p2])^;
          p3:=PPoint(@vp[tr.p3])^;

          c.x:=(p1.X + p2.X + p3.X)/3;
          c.y:=(p1.Y + p2.Y + p3.Y)/3;

          if Assigned(bp) then
            rc:=_rPolygonContainsPixel(bp,n-1,c.x,c.y,nil)
          else

        end;

        if rc < 0 then begin
          sp[i]:=-1; tri_off(i);
        end
        else begin
          sp[i]:=j; Inc(j)
        end
      end;

      comp_tin(sp);
    until ftrin = old;

    xFreeptr(sp)
  end;

  Result:=ftrin
end;

function TTriangulator2.filter(cb: TIntegerFunc): int;
var
  sp: PIntegers; i,j: int;
  tp: PTriArray3; tr: TTri3;
begin
  sp:=xAllocInt(trin);
  if Assigned(sp) then begin

    tp:=fTris; j:=0;
    for i:=0 to ftrin-1 do begin

      if cb(i) then begin
        sp[i]:=-1; tri_off(i)
      end
      else begin
        sp[i]:=j; Inc(j)
      end
    end;

    comp_tin(sp);
  end;

  xFreeptr(sp); Result:=ftrin
end;

function TTriangulator2.pack_tin(skip: int): int;
var
  sp: PIntegers; i,j: int;
  tp: PTriArray3; tr: TTri3;
begin
  sp:=xAllocInt(trin);
  if Assigned(sp) then begin

    tp:=fTris; j:=0;
    for i:=0 to ftrin-1 do begin

      sp[i]:=-1; tr:=tp[i];

      with tr do
      if p1 >= skip then
      if p2 >= skip then
      if p3 >= skip then begin
        sp[i]:=j; Inc(j)
      end;

      if sp[i] < 0 then tri_off(i)
    end;

    comp_tin(sp);
  end;

  xFreeptr(sp); Result:=ftrin
end;

function TTriangulator2.comp_tin(sp: PIntegers): Integer;
var
  i,j: int; tr: TTri3; tp: PTriArray3;
begin
  j:=0; tp:=fTris;
  for i:=0 to ftrin-1 do
  if sp[i] >= 0 then begin tr:=tp[i];
    if tr.t1 >= 0 then tr.t1:=sp[tr.t1];
    if tr.t2 >= 0 then tr.t2:=sp[tr.t2];
    if tr.t3 >= 0 then tr.t3:=sp[tr.t3];
    tp[j]:=tr; Inc(j)
  end;

  ftrin:=j; Result:=ftrin
end;

procedure TTriangulator2.tri_off(it: Integer);

procedure tri_link_off(it,tr: Integer);
begin
  if it >= 0 then
  with fTris[it] do
  if t1 = tr then t1:=-1 else
  if t2 = tr then t2:=-1 else
  if t3 = tr then t3:=-1
end;

var
  tr: TTri3;
begin
  tr:=fTris[it];
  tri_link_off(tr.t1,it);
  tri_link_off(tr.t2,it);
  tri_link_off(tr.t3,it);
end;

function TTriangulator2.Add_node(const v: VPoint): Integer;
begin
  Result:=Add(@v);
  fTops:=First;
  ftopn:=Count;
  fStride:=12;
end;

function TTriangulator2.Ins_node(const v: VPoint; it: Integer): Integer;
var
  rc: Integer;
begin
  Result:=Add(@v);
  fTops:=First;
  ftopn:=Count;
  fStride:=12;

  resize_buffers(ftopn);

  if Result > 0 then

  if it >= 0 then begin
    rc:=ThisTriangle(it,@v);
    if (rc > 0) and (rc <= 4) then
    splitTri(it,rc,Result);
  end else

  if it <> INT_NAN then
  if InsertVertex(Result) < 0 then
    Inc(fOutside);
end;

function TTriangulator2.Calculate(ATops: Pointer;
                                  Atopn,AStride,ASkip: Integer): Integer;
var
  ip: Integer; lt,rb: TPoint;
begin
  Result:=-1; Clear_data;

  fTops:=ATops;
  ftopn:=Atopn;
  fstride:=AStride;

  if Atopn >= 7 then
  if Tops_bound(fTops,4,fStride, lt,rb) then
  if BeginProcess(lt,rb,ftopn) then begin

    SetTri(0, 0,1,3, 1,-1,-1);
    SetTri(1, 1,2,3, -1,0,-1);
    ftrin:=2; cash_fill(0,1);
    fLastTri:=0;

    for ip:=4 to ftopn-1 do begin
      if InsertVertex(ip) < 0 then Inc(fOutside);
      resize_buffers(ip-fOutside)
    end;

    Result:=pack_tin(ASkip);
    EndProcess;
  end;
end;

function TTriangulator2.Verify: Integer;

function TriContainsTri(it1,it2: Integer): Boolean;
begin
  Result:=true; if it1 >= 0 then with Tris[it1] do
  Result:=(it2 = t1) or (it2 = t2) or (it2 = t3)
end;

function TriContainsRib(it,ip1,ip2: Integer): Boolean;
begin
  Result:=true; if it >= 0 then with Tris[it] do
  Result:=((p1=ip1) and (p2=ip2)) or
          ((p2=ip1) and (p3=ip2)) or
          ((p3=ip1) and (p1=ip2))
end;

var
  it: Integer; tri: TTri3;
begin
  Result:=0;

  if Assigned(fTops) then
  if Assigned(fTris) then begin

    for it:=0 to trin-1 do begin
      tri:=Tris[it];

      with tri do
      if not TriContainsTri(t1,it)
      or not TriContainsTri(t2,it)
      or not TriContainsTri(t3,it) then begin
        Result:=-2; Break
      end;

      with tri do
      if not TriContainsRib(t1,p3,p2)
      or not TriContainsRib(t2,p1,p3)
      or not TriContainsRib(t3,p2,p1) then begin
        Result:=-3; Break
      end;

      with tri do
      if (p1 = p2)
      or (p1 = p3)
      or (p2 = p3) then begin
        Result:=-4; Break
      end
    end
  end
end;

procedure TTriangulator2.log_model(const log: ILog);
begin
  if Assigned(log) then begin
    log.sAdd('NPoints : '+IntToStr(ftopn));
    log.sAdd('TriCount: '+IntToStr(ftrin));
    log.sAdd('Verify  : '+IntToStr(fRetCode));
    log.Add('');

    if fOutside > 0 then
    log.sAdd('Outside  : '+IntToStr(fOutside));

    log.sAdd('TriTested: '+RealToStr(fTriTested1/fTriTested2,1));
    if Assigned(fcash) then log.sAdd('CashCoef : '+RealToStr(fhs_r,1));
    log.sAdd('Elapsed  : '+RealToStr(fElapsed/1000,3)+'sec');
  end
end;

procedure TTriangulator2.dump(Dest: PChar;
                              bw,bh,skip: Integer;
                              sys: psys; gb: PGPoly);

function tr_to_tr2(tp3: PTriArray3; tp2: PTriArray2;
                   skip: Integer): Integer;
var
  it: Integer; tr: TTri3;
begin
  Result:=ftrin;
  for it:=0 to ftrin-1 do begin
    tr:=tp3[it];
    Dec(tr.p1,skip);
    Dec(tr.p2,skip);
    Dec(tr.p3,skip);
    tp2[it]:=PTri2(@tr)^
  end
end;

var
  log: TTextFile;

  h,i,z1,z2,hn: int;
  tp: PTriArray3; lp,vp: PVPoly;
  tp2: PTriArray2;

  np: pxyz_array; np1: PIPoly;
  hs: TSize; hp: PIntegers;

  hdr: ttin_hdr; buf: tbytes;
  v: VPoint; n: txyz;
  fn: TShortstr; s: String;
begin
  FileErase(Dest);

  log:=TTextFile.Create;
  try
    if topn-skip > 0 then
    if trin > 0 then begin

      vp:=xAllocPtr(topn * 12);
      np:=xAllocPtr(topn * Sizeof(txyz));
      tp2:=xAllocPtr(trin * Sizeof(TTri2));

      if Assigned(vp) then
      if Assigned(np) then
      if Assigned(tp2) then begin

        Fillchar(hdr,Sizeof(hdr),0);
        hdr.mag:=tin_mag1; hdr.ver:=1;
        hdr.vn:=topn-skip;

        lp:=PVPoly(tops);
        lp:=@lp[skip];

        z1:=lp[0].z; z2:=z1;

        for i:=0 to hdr.vn-1 do begin
          v:=lp[0]; lp:=@lp[1];
          if v.z < z1 then z1:=v.z;
          if v.z > z2 then z2:=v.z;
          vp[i]:=v;
        end;

        tp:=fTris;
        hdr.tn:=tr_to_tr2(tp,tp2,skip);

        np1:=Pointer(np);
        tops_normals(np,np1, vp,hdr.vn,
                     @tp2[0],hdr.tn,Sizeof(TTri2),
                     0);

        if log_enabled then begin
          StrUpdateExt(fn,Dest,'.txt');
          if log.Make(fn) then begin
            s:=Format('vn=%d, z=[%d..%d]',[hdr.vn,z1,z2]);
            log.WriteStr(s);

            for i:=0 to hdr.vn-1 do begin

              v:=vp[i];

              with np1[i] do
              n:=_xyz(x/10000,y/10000,0);
              with n do n.z:=Sqrt(1-x*x-y*y);

              s:=IntToStr(i)+':'#9+
                 IntToStr(v.x)+#9+
                 IntToStr(v.y)+#9+
                 IntToStr(v.z)+#9+

                 RealToStr(n.x,4)+#9+
                 RealToStr(n.y,4)+#9+
                 RealToStr(n.z,4);

              log.WriteStr(s)
            end
          end
        end;

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

            FileWrite(h,np1^,hdr.vn * 4);
            FileClose(h)
          end
        end;

        xFreePtr(hp)
      end;

      xFreePtr(tp2);
      xFreePtr(np);
      xFreePtr(vp);
    end
  finally
    log.Free
  end
end;

end.
