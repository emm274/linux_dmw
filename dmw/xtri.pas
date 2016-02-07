unit xtri; interface

uses
  Windows,
  Classes,Math,
  OTypes,OTypes1,XList;

const
  epsln = 1.0E-06;
  znil  = -999999;
  znil1 = -99999999;

type
  PVertex = ^TVertex;
  TVertex = record x,y: long; z: Single end;

  PVertexes = ^TVertexes;
  TVertexes = array[0..1023] of TVertex;

  PRib = ^TRib;
  TRib = record
    p1,p2,t1,t2: Integer
  end;

  PRibArray = ^TRibArray;
  TRibArray = array[0..1023] of TRib;

  PTri = ^TTri; TTri = record
    p1,p2,p3, r1,r2,r3: Integer
  end;

  PTriArray = ^TTriArray;
  TTriArray = array[0..1023] of TTri;

  TTriangulator = class

    constructor Create;
    destructor Destroy; override;

    function Get_Tris(tp2: PTriArray2; cls: Float): Integer;

    function Get_TriWords(var Buf: PTriWords): Integer;
    function Get_Bound(var Buf: PIntegers): Integer;

    function Get_Ribs(var Buf: PIntegers): int;

    function Get_cline(lp: PLPoly; n1,n2: Integer;
                       out Buf: PLPoly): Integer;

    function Calculate(ATops: Pointer;
                       Atopn,AStride: Integer): Integer;

    procedure filter_bound_ribs(f1,f2: int);

    function Verify: Integer;

    procedure dump_log;
    procedure dump(Dest: PChar;
                   bw,bh,skip: Integer;
                   sys: psys; gb: PGPoly);

  private
    fTops: Pointer;
    ftopn: Integer;
    fStride: Integer;
    fCline: Integer;

    fbp: PIntegers;
    fnl: int;

    fwl: PIntegers;
    fnwl: Integer;
    fnlf: Integer;

    fTris: PTriArray;
    fRibs: PRibArray;

    fribn: Integer;
    ftrin: Integer;

    fElapsed: Integer;

    function RibOf(p1,p2: Integer): Integer;

    function Get_Vertex(I: int): TPoint;

    procedure SetRib(ir, p1,p2, t1,t2: integer);
    procedure SetTri(it, p1,p2,p3, r1,r2,r3: Integer);

    function RibVisible(ir: Integer; ip: PPoint): Boolean;
    function rib_to_top(var tr: TTri; r1: Integer): Boolean;
    procedure rib_relink(ir,At1,At2: Integer);

    function NextTri(it,ir: Integer): Integer;

  protected
    procedure begin_process; virtual;
    procedure end_process; virtual;

    function log_vertex(ip: Integer): Boolean; virtual;
    procedure print(msg: String); virtual;

    procedure DrawBound(cl: Integer); virtual;
    procedure DrawTri(it,cl: Integer); virtual;
    procedure DrawRib(ir,cl: Integer); virtual;
    procedure DrawPoint(ip,cl: Integer); virtual;

  public
    property Tops: Pointer read fTops;
    property topn: Integer read ftopn;
    property Stride: Integer read fStride;

    property bp: PIntegers read fbp;
    property nl: Integer read fnl;

    property Tris: PTriArray read fTris;
    property Ribs: PRibArray read fRibs;

    property ribn: Integer read fribn;
    property trin: Integer read ftrin;

    property Vertexes[I: Integer]: TPoint read Get_Vertex;
  end;

function Vertex(x,y: Integer; z: Single): TVertex;

function tops_bound(tops: Pointer; ndp,stride: int;
                    out lt,rb: TPoint): Boolean;

function GetHashSize(tn,bw,bh: Integer): TSize;

procedure fill_hash(vp: PVPoly;
                    tp: PTriArray2; tn: Integer;
                    hp: PIntegers; nx,ny, bw,bh: Integer);

function Calc_tri(lp: PLPoly; ndp: Integer;
                  var Tri: PTriWords): Integer;
stdcall;

function Bound_tri(lp: PLPoly; ndp: Integer;
                   var Bound: PIntegers): Integer;
stdcall;

function Calc_tri2(var tp2: PTriArray2;
                   lp: PLPoly; ndp: Integer;
                   cls: Float): Integer;
stdcall;

function Calc_tri2a(var tp1: PTriArray2;
                    tp1_size: int;

                    var tp2: PTriArray2;
                    lp: PLPoly; ndp: int;
                    cls: Float): int;
stdcall;

function Calc_ribs(lp: PLPoly; ndp: Integer;
                   var Buf: PIntegers): Integer;
stdcall;

function Calc_ribs1a(lp: PLPoly; ndp,f1,f2: int;
                     var Buf: PIntegers): int;
stdcall;

function tri_cline(lp: PLPoly; n1,n2: Integer;
                   out Buf: PLPoly; tin: PChar): Integer;
stdcall;

function Free_tris(Tris: PTriWords): Pointer; stdcall;
function Free_ribs(Ribs: PIntegers): Pointer; stdcall;

function Tops_to_tin(Path,Dest: PChar): Boolean;

procedure tops_normals(np: pxyz_array; np1: PIPoly;
                       vp: PVPoly; vn: Integer;
                       tp: PIntegers; tn,tstride: Integer;
                       method: Integer);

implementation

uses
  Sysutils,xutils,
  Convert,OFiles,
  XLine,XPoly,
  OGauss;

const
  debug = false;

function Vertex(x,y: Integer; z: Single): TVertex;
begin
  Result.x:=x;
  Result.y:=y;
  Result.z:=z;
end;

procedure spvp(const p1,p2,p3: TPoint; out sp,vp: Double);
var
  dx2,dy2,dx3,dy3: Double;
begin
  dx2:=p2.x-p1.x; dx3:=p3.x-p1.x;
  dy2:=p2.y-p1.y; dy3:=p3.y-p1.y;

  sp:=dx2*dx3 + dy2*dy3;
  vp:=dy3*dx2 - dx3*dy2;
end;

function tops_bound(tops: Pointer; ndp,stride: int;
                    out lt,rb: TPoint): Boolean;
var
  i,x1,y1,x2,y2: int; pp: PPoint;
begin
  Result:=false;

  lt:=Point(0,0); rb:=lt;

  if ndp > 0 then begin

    pp:=Tops;

    x1:=pp.X; x2:=x1;
    y1:=pp.Y; y2:=y1;

    for i:=2 to ndp do begin
      Inc(TPointer(pp),Stride);
      with pp^ do begin
        if X < x1 then x1:=X;
        if Y < y1 then y1:=Y;
        if X > x2 then x2:=X;
        if Y > y2 then y2:=Y;
      end
    end;

    if x1 < x2 then
    if y1 < y2 then begin
      lt:=Point(x1,y1);
      rb:=Point(x2,y2);
      Result:=true
    end
  end
end;

procedure tops_normals(np: pxyz_array; np1: PIPoly;
                       vp: PVPoly; vn: Integer;
                       tp: PIntegers; tn,tstride: Integer;
                       method: Integer);

const
  epsln = 1E-6;

function idet(a11,a12,a21,a22: int): Double;
begin
  Result:=Int64(a11)*a22 - Int64(a12)*a21
end;

function norm(v: PVPoly): txyz;
var
  dx1,dy1,dz1,dx2,dy2,dz2: int;
begin
  dx1:=v[1].x-v[0].x;
  dy1:=v[1].y-v[0].y;
  dz1:=v[1].z-v[0].z;
  dx2:=v[2].x-v[0].x;
  dy2:=v[2].y-v[0].y;
  dz2:=v[2].z-v[0].z;

  Result.x:=idet(dy1,dz1,dy2,dz2);
  Result.y:=-idet(dx1,dz1,dx2,dz2);
  Result.z:=idet(dx1,dy1,dx2,dy2)
end;

function norm1(v: PVPoly; out n: txyz): Boolean;
var
  t: txyz; r: Double;
begin
  Result:=false; t:=norm(v);

  with t do begin
    r:=Sqrt(x*x + y*y + z*z);
    if r >= Small then begin
      x:=x/r; y:=y/r; z:=z/r;
      Result:=true
    end
  end;

  n:=t
end;

function norm2(v: PVPoly; out n: txyz; d: PDoubles): Boolean;
var
  dx1,dy1,dz1,dx2,dy2,dz2: int; t: txyz;
  d1,d2,d3: double;
begin
  Result:=false;

  dx1:=v[1].x-v[0].x;
  dy1:=v[1].y-v[0].y;
  dz1:=v[1].z-v[0].z;
  dx2:=v[2].x-v[0].x;
  dy2:=v[2].y-v[0].y;
  dz2:=v[2].z-v[0].z;

  t.x:=idet(dy1,dz1,dy2,dz2);
  t.y:=-idet(dx1,dz1,dx2,dz2);
  t.z:=idet(dx1,dy1,dx2,dy2);

  if t.z > (dx1*dx2+dy1*dy2)*epsln then begin
    n:=t;

    d[0]:=Hypot(dx1,dy1);
    d[1]:=Hypot(v[2].x-v[1].x,v[2].y-v[1].y);
    d[2]:=Hypot(dx2,dy2);

    Result:=true
  end
end;

procedure norm3(v: PVPoly; var n1,n2: txyz);
var
  dx,dy,dz,dxy,k: double; n: txyz;
begin
  dx:=v[1].x-v[0].x;
  dy:=v[1].y-v[0].y;
  dz:=v[1].z-v[0].z;
  dxy:=Hypot(dx,dy);

  if dxy > epsln then begin

    k:=-dz/dxy;
    n.x:=k*dx;
    n.y:=k*dy;
    n.z:=dxy;

    with n1 do begin
      if z < 0 then z:=0;
      x:=x+n.x;
      y:=y+n.y;
      z:=z+n.z;
    end;

    with n2 do begin
      if z < 0 then z:=0;
      x:=x+n.x;
      y:=y+n.y;
      z:=z+n.z;
    end
  end
end;

function iscalar(ax,ay,bx,by: int): Double;
begin
  Result:=Int64(ax)*bx + Int64(ay)*by
end;

function iweight(ax,ay,bx,by: int; d: Double): Double;
begin
  Result:=Sqrt(1 - iscalar(ax,ay,bx,by)/d) / d
end;

var
  ip,it,i1,i2,i3,znil1,rc: int;
  v: VOrient; n: txyz; a,b,c: TPoint;
  r,k,an,bn,cn,w1,w2,w3: Double;
  d: doubles3; n1: IPoint;
begin
  znil1:=Round(znil * 100+1);

  for ip:=0 to vn-1 do np[ip].z:=-1;

  w1:=1; w2:=1; w3:=1;

  for it:=1 to tn do begin

    i1:=tp[0]; i2:=tp[1]; i3:=tp[2];
    v[0]:=vp[i1]; v[1]:=vp[i2]; v[2]:=vp[i3];

    if v[0].z > znil1 then
    if v[1].z > znil1 then
    if v[2].z > znil1 then

    if Method = 3 then begin
      v[3]:=v[0];
      norm3(@v[0],np[i1],np[i2]);
      norm3(@v[1],np[i2],np[i3]);
      norm3(@v[2],np[i3],np[i1]);
    end
    else begin

      rc:=0;

      case Method of
    0:  begin
          n:=norm(@v); rc:=1
        end;

    1:  if norm1(@v,n) then begin

          a.X:=v[1].x-v[0].x;
          a.Y:=v[1].y-v[0].y;
          an:=Hypot(a.X,a.Y);

          b.X:=v[2].x-v[1].x;
          b.Y:=v[2].y-v[1].y;
          bn:=Hypot(b.X,b.Y);

          c.X:=v[0].x-v[2].x;
          c.Y:=v[0].y-v[2].y;
          cn:=Hypot(c.X,c.Y);

          w1:=iweight(a.X,a.Y,-c.X,-c.Y, an*cn);
          w2:=iweight(b.X,b.Y,-a.X,-a.Y, bn*an);
          w3:=iweight(c.X,c.Y,-b.X,-b.Y, cn*bn);

          rc:=1;
        end;

    2:  if norm2(@v,n,@d) then begin
          w1:=1 / (d[2]*d[0]);
          w2:=1 / (d[0]*d[1]);
          w3:=1 / (d[1]*d[2]);

          w1:=n.z*w1*w1*w1;
          w2:=n.z*w2*w2*w2;
          w3:=n.z*w3*w3*w3;

          rc:=1
        end
      end;

      if rc = 1 then begin

        with np[i1] do begin
          if z < 0 then z:=0;
          x:=x+n.x*w1;
          y:=y+n.y*w1;
          z:=z+n.z*w1;
        end;

        with np[i2] do begin
          if z < 0 then z:=0;
          x:=x+n.x*w2;
          y:=y+n.y*w2;
          z:=z+n.z*w2;
        end;

        with np[i3] do begin
          if z < 0 then z:=0;
          x:=x+n.x*w3;
          y:=y+n.y*w3;
          z:=z+n.z*w3;
        end
      end
    end;

    Inc(TPointer(tp),tstride)
  end;

  if Assigned(np1) then
  for ip:=0 to vn-1 do begin

    n:=np[ip]; k:=0;
    if n.z > 0 then begin
      r:=Sqrt(n.x*n.x + n.y*n.y + n.z*n.z);
      k:=10000/r;
    end;

    n1.x:=Round(n.x*k);
    n1.y:=Round(n.y*k);
    np1[ip]:=n1
  end
end;

function GetHashSize(tn,bw,bh: Integer): TSize;
var
  k1,k2: Double;
begin
  k1:=bh/bw; k2:=Sqrt(3*tn / k1);
  if k2 > 1000 then k2:=1000;

  Result.cx:=Max(2,Round(k2));
  Result.cy:=Max(2,Round(k2*k1))
end;

procedure fill_hash(vp: PVPoly;
                    tp: PTriArray2; tn: Integer;
                    hp: PIntegers; nx,ny, bw,bh: Integer);
var
  i,n: Integer; dp: PSingles;
  tr: TTri2; v1,v2,v3: VPoint;
  x1,y1,x2,y2,ix,iy,bx: Integer;
  kx,ky,cx,cy,tx,ty,r: Double;

begin
  n:=nx * ny; dp:=@hp[n];

  for i:=0 to n-1 do begin
    hp[i]:=-1; dp[i]:=-1
  end;

  kx:=nx / bw; ky:=ny / bh;

  for i:=0 to tn-1 do begin
    tr:=tp[i];
    v1:=vp[tr.p1];
    v2:=vp[tr.p2];
    v3:=vp[tr.p3];

    x1:=v1.x; x2:=x1;
    if v2.x < x1 then x1:=v2.x;
    if v2.x > x2 then x2:=v2.x;
    if v3.x < x1 then x1:=v3.x;
    if v3.x > x2 then x2:=v3.x;

    y1:=v1.y; y2:=y1;
    if v2.y < y1 then y1:=v2.y;
    if v2.y > y2 then y2:=v2.y;
    if v3.y < y1 then y1:=v3.y;
    if v3.y > y2 then y2:=v3.y;

    tx:=(v1.x + v2.x + v3.x) / 3;
    ty:=(v1.y + v2.y + v3.y) / 3;

    x1:=Trunc(x1*kx)-2;
    if x1 < 0 then x1:=0;

    x2:=Trunc(x2*kx)+2;
    if x2 >= nx then x2:=nx-1;

    y1:=Trunc(y1*ky)-2;
    if y1 < 0 then y1:=0;

    y2:=Trunc(y2*ky)+2;
    if y2 >= ny then y2:=ny-1;

    for iy:=y1 to y2 do
    for ix:=x1 to x2 do begin
      cx:=ix/kx; cy:=iy/ky;
      r:=Hypot(tx-cx,ty-cy);

      bx:=iy*nx + ix;
      if (hp[bx] < 0) or (r < dp[bx]) then
      begin hp[bx]:=i; dp[bx]:=r end
    end
  end
end;

function order_points1(tops: Pointer;
                       ndp,stride: int;
                       wp: PIntegers): int;

function sort_wk(wp: PIntegers; wk: PDoubles;
                 ndp: Integer): Boolean;
const
  buf_max = 1024;
type
  TBuffer = Array[0..buf_max-1] of Integer;
var
  top,bot,off,stop,rc: Integer;
  cx,bx, ind,tmp,blk,len: Integer;
  big: Pointer; si,di: PIntegers;
  k1: Double; buf,mov: TBuffer;
begin
  Result:=false;

  big:=xAllocPtr(ndp*4);
  if Assigned(big) then begin

    off:=1; stop:=ndp;
    while off < stop do begin

      bot:=0;
      while bot < ndp do begin
        ind:=bot; Inc(bot,off); top:=bot; Inc(bot,off);

        while (ind < top) and (ind < ndp) do begin

          k1:=wk[wp[ind]]; tmp:=top;

          while top < bot do begin
            if top >= ndp then Break;
            if k1 <= wk[wp[top]] then Break;
            Inc(top)
          end;

          if top > tmp then begin

            len:=top-tmp;
            cx:=len*Sizeof(Integer);

            if len <= buf_max then
              Move(wp[tmp],buf,cx)
            else
              Move(wp[tmp],big^,cx);

            blk:=(tmp-ind)*Sizeof(Integer);

            si:=@wp[tmp]; di:=@wp[top];

            while blk > 0 do begin {move block}
              bx:=Sizeof(mov);
              if bx > blk then bx:=blk;

              Dec(blk,bx);
              Dec(TPointer(si),bx);
              Dec(TPointer(di),bx);

              Move(si[0],mov,bx);
              Move(mov,di[0],bx)
            end;

            if len <= buf_max then
              Move(buf,wp[ind],cx)
            else
              Move(big^,wp[ind],cx);

            Inc(ind,len)
          end;

          Inc(ind)
        end
      end;

      Inc(off,off)
    end;

    Result:=true
  end;

  xFreePtr(big)
end;

var
  wk: PDoubles; lp: PPoint;
  i,j, i1,i2,i3: Integer;
  sp,vp,r1,r2,cx,cy: Double;
  p1,p2,p3: PPoint; lt,rb: TPoint;

begin
  Result:=0;

  wk:=xAllocDoubles(ndp);
  if Assigned(wk) then
  if tops_bound(tops,ndp,stride, lt,rb) then begin

    for i:=0 to ndp-1 do wp[i]:=i;

    lp:=Pointer(tops);
    for i:=0 to ndp-1 do begin
      with lp^ do
      wk[i]:=Hypot(X-lt.X,Y-lt.Y);
      Inc(TPointer(lp),stride);
    end;

    if sort_wk(wp,wk,ndp) then begin

      for i:=0 to ndp-2 do
      if wp[i] >= 0 then begin

        p1:=PPoint(tops + wp[i]*stride);
        r1:=wk[wp[i]];

        for j:=i+1 to ndp-1 do
        if wp[j] >= 0 then begin
          p2:=PPoint(tops + wp[j]*stride);
          r2:=wk[wp[j]];

          if CompareMem(p1,p2,8) then
            wp[j]:=-1
          else
          if r1+1 < r2 then Break
        end
      end;

      j:=0;
      for i:=0 to ndp-1 do
      if wp[i] >= 0 then begin
        wp[j]:=wp[i]; Inc(j)
      end;

      Result:=j; ndp:=j;

      if ndp >= 3 then begin

        i1:=wp[0]; p1:=PPoint(tops + i1*stride);
        i2:=wp[1]; p2:=PPoint(tops + i2*stride);

        cx:=p1.X/2 + p2.X/2; wk[i1]:=0;
        cy:=p1.Y/2 + p2.Y/2; wk[i2]:=0;

        for i:=0 to ndp-1 do begin
          i3:=wp[i]; p3:=PPoint(tops + i3*stride);
          wk[i3]:=Hypot(p3.X-cx,p3.Y-cy);
        end;

        sort_wk(wp,wk,ndp);

        //  If necessary, modify the ordering in such a way that the
        //  first three data points are not collinear.

        i1:=wp[0]; p1:=PPoint(tops + i1*stride);
        i2:=wp[1]; p2:=PPoint(tops + i2*stride);

        i:=2;
        while i < ndp do begin
          i3:=wp[i]; p3:=PPoint(tops + i3*stride);
          spvp(p3^,p1^,p2^, sp,vp);
          if Abs(vp) > Abs(sp) * epsln then begin

            spvp(p1^,p2^,p3^, sp,vp);

            if vp < 0 then begin
              wp[0]:=i2; wp[1]:=i1
            end;

            Break;
          end; Inc(i)
        end;

        if i = ndp then  // Fatal error!
          Result:=-2     // All collinear data points.
        else
        if i > 2 then begin
          for j:=i downto 3 do
          wp[j]:=wp[j-1]; wp[2]:=i3
        end
      end
    end
  end;

  xFreeptr(wk)
end;

function order_points2(tops,ndp,stride: Integer;
                       wp: PIntegers): Integer;
var
  wk: PDoubles;
  i,j,jpmn, i1,i2,i3: Integer;
  dsqmn,dsqi,sp,vp,r,dx,dy: Double;
  lp: Longint; lp1,lp2: PPoint;
  p1,p2,p3: TPoint; c: TGauss;

begin
  Result:=0;

  wk:=xAllocDoubles(ndp);
  if Assigned(wk) then begin

    lp:=tops;

    Fillchar(wp^,ndp*4,0);

    i1:=0; p1:=PPoint(lp)^;
    i2:=0; lp2:=PPoint(lp);

    for i:=1 to ndp-1 do begin
      Inc(TPointer(lp2),stride);

      dx:=lp2.x - p1.x;
      dy:=lp2.y - p1.y;
      dsqmn:=dx*dx + dy*dy;

      if dsqmn > epsln then begin
        i2:=i; Break
      end;

      wp[i]:=-i; // Two points are identical.
    end;

    if i2 = 0 then
      Result:=-1
    else begin

      lp1:=PPoint(lp);
      for i:=0 to ndp-2 do begin

        p1:=lp1^;

        if wp[i] >= 0 then begin

          lp2:=lp1;
          Inc(TPointer(lp2),stride);

          for j:=i+1 to ndp-1 do begin

            if wp[j] >= 0 then begin
              dx:=lp2.x - p1.x;
              dy:=lp2.y - p1.y;
              dsqi:=dx*dx + dy*dy;

              if dsqi <= epsln then
                wp[j]:=-j // Two points are identical.
              else
              if dsqi < dsqmn then begin
                dsqmn:=dsqi; i1:=i; i2:=j
              end;
            end;

            Inc(TPointer(lp2),stride);
          end;
        end;

        Inc(TPointer(lp1),stride);
      end;

      // Compute the midpoint of the closest two data points.

      p1:=PPoint(lp + i1*stride)^;
      p2:=PPoint(lp + i2*stride)^;

      c.x:=p1.x/2 + p2.x/2;
      c.y:=p1.y/2 + p2.y/2;

      //  Sort the other (NDP-2) data points in ascending order of
      //  distance from the midpoint and store the sorted data point
      //  numbers in the IWP array.

      j:=0; lp1:=PPoint(lp);

      for i:=0 to ndp-1 do begin

        if i = i1 then begin
          wp[j]:=wp[0]; wp[0]:=i1;
          wk[j]:=wk[0]; wk[0]:=0; Inc(j)
        end else
        if i = i2 then begin
          wp[j]:=wp[1]; wp[1]:=i2;
          wk[j]:=wk[1]; wk[1]:=0; Inc(j)
        end else

        if wp[i] >= 0 then begin
          wp[j]:=i; with lp1^ do
          wk[j]:=Hypot(x-c.x,y-c.y);
          Inc(j)
        end;

        Inc(TPointer(lp1),stride);
      end;

      Result:=j; ndp:=j;

      for i:=2 to ndp-2 do begin

        dsqmn:=wk[i]; jpmn:=i;

        for j:=i+1 to ndp-1 do
        if wk[j] < dsqmn then begin
          dsqmn:=wk[j]; jpmn:=j
        end;

        if jpmn > i then begin
          j:=wp[i]; wp[i]:=wp[jpmn]; wp[jpmn]:=j;
          r:=wk[i]; wk[i]:=wk[jpmn]; wk[jpmn]:=r
        end
      end;

      //  If necessary, modify the ordering in such a way that the
      //  first three data points are not collinear.

      i:=2;
      while i < ndp do begin
        i3:=wp[i];
        p3:=PPoint(lp + i3*stride)^;
        spvp(p3,p1,p2, sp,vp);
        if Abs(vp) > Abs(sp) * epsln then begin

          spvp(p1,p2,p3, sp,vp);

          if vp < 0 then begin
            wp[0]:=i2; wp[1]:=i1
          end;

          Break;
        end
      end;

      if i = ndp then  // Fatal error!
        Result:=-2     // All collinear data points.
      else
      if i > 2 then begin
        for j:=i downto 3 do
        wp[j]:=wp[j-1]; wp[2]:=i3
      end
    end
  end;

  xFreeptr(wk)
end;

constructor TTriangulator.Create;
begin
  inherited Create;
end;

destructor TTriangulator.Destroy;
begin
  xFreePtr(fbp); xFreePtr(fwl);
  inherited
end;

function TTriangulator.Get_Vertex(I: Integer): TPoint;
begin
  Result:=PPoint(fTops + I*fstride)^
end;

procedure TTriangulator.SetRib(ir, p1,p2, t1,t2: integer);
var
  rb: TRib; 
begin
  rb.p1:=p1; rb.p2:=p2;
  rb.t1:=t1; rb.t2:=t2; Ribs[ir]:=rb;
end;

procedure TTriangulator.SetTri(it, p1,p2,p3, r1,r2,r3: Integer);
var
  tp: PTri;
begin
  tp:=@fTris[it];
  tp.p1:=p1; tp.p2:=p2; tp.p3:=p3;
  tp.r1:=r1; tp.r2:=r2; tp.r3:=r3;
end;

function TTriangulator.Get_Tris(tp2: PTriArray2; cls: Float): Integer;

procedure tri_link_off(tp2: PTriArray2; it,tr: Integer);
begin
  if it >= 0 then
  with tp2[it] do
  if t1 = tr then t1:=-1 else
  if t2 = tr then t2:=-1 else
  if t3 = tr then t3:=-1
end;

var
  sp: PIntegers; i,j,k: Integer;
  tr: TTri; tr2: TTri2; l: LOrient;
begin
  Result:=ftrin;
  if ftrin > 0 then begin

    for i:=0 to ftrin-1 do begin
      tr:=fTris[i];
      tr2.p1:=tr.p1; tr2.t1:=NextTri(i,tr.r2);
      tr2.p2:=tr.p2; tr2.t2:=NextTri(i,tr.r3);
      tr2.p3:=tr.p3; tr2.t3:=NextTri(i,tr.r1);
      tp2[i]:=tr2
    end;

    sp:=nil; if cls > 0 then
    sp:=xAllocInt(ftrin);

    if Assigned(sp) then begin

      for i:=0 to ftrin-1 do sp[i]:=1;

      repeat k:=0;

        for i:=0 to ftrin-1 do
        if sp[i] > 0 then begin
          tr2:=tp2[i]; with tr2 do
          if (t1 < 0) or (t2 < 0) or (t3 < 0) then begin
            l[0]:=PPoint(fTops + p1*fstride)^;
            l[1]:=PPoint(fTops + p2*fstride)^;
            l[2]:=PPoint(fTops + p3*fstride)^;

            if not Triangle_Verify(@l,cls) then begin
              tri_link_off(tp2,tr2.t1,i);
              tri_link_off(tp2,tr2.t2,i);
              tri_link_off(tp2,tr2.t3,i);
              Inc(k); sp[i]:=-1
            end
          end;
        end;

      until k = 0;

      j:=0;
      for i:=0 to ftrin-1 do
      if sp[i] > 0 then begin
        sp[i]:=j; Inc(j)
      end;

      j:=0;
      for i:=0 to ftrin-1 do
      if sp[i] >= 0 then begin
        tr2:=tp2[i]; with tr2 do begin
          if tr2.t1 >= 0 then tr2.t1:=sp[tr2.t1];
          if tr2.t2 >= 0 then tr2.t2:=sp[tr2.t2];
          if tr2.t3 >= 0 then tr2.t3:=sp[tr2.t3];
        end; tp2[j]:=tr2; Inc(j)
      end;

      Result:=j
    end
  end
end;

function TTriangulator.Get_TriWords(var Buf: PTriWords): Integer;
var
  i: Integer; tr: TTriWord;
  si: PTriArray; di: PTriWords;
begin
  Result:=0; Buf:=xFreePtr(Buf);

  if trin > 0 then begin
    Buf:=xAllocPtr(trin*SizeOf(TTriWord));

    if Buf <> nil then begin

      si:=Tris; di:=Buf;
      for i:=1 to trin do begin
        tr[0]:=si[0].p1+1;
        tr[1]:=si[0].p3+1;
        tr[2]:=si[0].p2+1; di[0]:=tr;

        si:=@si[1]; di:=@di[1];
      end;

      Result:=trin
    end
  end
end;

function TTriangulator.Get_Bound(var Buf: PIntegers): Integer;
var
  i,ir,p2: Integer; r: TRib;
begin
  Result:=0; Buf:=xFreePtr(Buf);

  if nl > 2 then begin
    Buf:=xAllocInt(nl+1);
    if Assigned(Buf) then begin

      ir:=bp[0]; r:=Ribs[ir];
      Buf[0]:=r.p1; Buf[1]:=r.p2;
      Result:=2; p2:=r.p2;

      for i:=1 to nl-1 do begin
        ir:=bp[i]; r:=Ribs[ir];
        if r.p1 = p2 then p2:=r.p2 else p2:=r.p1;
        Buf[Result]:=p2; Inc(Result)
      end
    end
  end
end;

function TTriangulator.Get_Ribs(var Buf: PIntegers): Integer;
var
  i,k: int; rb: PRib; di: pint;
begin
  Result:=0; Buf:=xFreePtr(Buf);

  if ribn > 0 then begin
    Buf:=xAllocInt(ribn*2);

    if Buf <> nil then begin

      k:=0; di:=Pointer(Buf);

      rb:=Pointer(Ribs);
      for i:=1 to ribn do begin

        if (rb.t1 >= 0) or (rb.t2 >= 0) then begin
          di^:=rb.p1; Inc(di);
          di^:=rb.p2; Inc(di);
          Inc(k)
        end;

        Inc(rb)
      end;

      Result:=k*2
    end
  end
end;

function TTriangulator.RibOf(p1,p2: Integer): Integer;
var
  i: Integer; r: TRib;
begin
  Result:=-1;
  for i:=0 to ribn-1 do begin
    r:=Ribs[i];
    if ((r.p1 = p1) and (r.p2 = p2))
    or ((r.p1 = p2) and (r.p2 = p1)) then
    begin Result:=i; Break end
  end
end;

function TTriangulator.RibVisible(ir: Integer; ip: PPoint): Boolean;
var
  dx,dy,vp: Int64; v1,v2: TPoint;
begin
  with Ribs[ir] do begin
    v1:=PPoint(fTops + p1*fstride)^;
    v2:=PPoint(fTops + p2*fstride)^;
  end;

  dx:=v2.X-v1.X; dy:=v2.Y-v1.Y;

  vp:=dx*(ip.Y-v2.Y) - dy*(ip.X-v2.X);
  Result:=vp < 0
end;

function TTriangulator.rib_to_top(var tr: TTri; r1: Integer): Boolean;
var
  tmp: TTri;
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

procedure TTriangulator.rib_relink(ir,At1,At2: Integer);
begin
  with Ribs[ir] do begin
    if t1 = At1 then t1:=At2 else
    if t2 = At1 then t2:=At2;

    if t1 < 0 then begin
      t1:=t2; t2:=-1
    end
  end
end;

function TTriangulator.NextTri(it,ir: Integer): Integer;
var
  rb: TRib;
begin
  Result:=-1; rb:=Ribs[ir];
  if rb.t1 = it then Result:=rb.t2 else
  if rb.t2 = it then Result:=rb.t1
end;

function TTriangulator.Get_cline(lp: PLPoly; n1,n2: Integer;
                                 out Buf: PLPoly): Integer;

function Contains_tri(lp: PLPoly; lp_n, it: Integer): Boolean;
var
  tr: TTri; a,b,c: TPoint;
  px,py, x1,y1,x2,y2,x3,y3: Double;
begin
  Result:=false;

  if it >= 0 then begin
    tr:=Tris[it];
    a:=Vertexes[tr.p1];
    b:=Vertexes[tr.p2];
    c:=Vertexes[tr.p3];

    px:=a.X/3 + b.X/3 + c.X/3;
    py:=a.Y/3 + b.Y/3 + c.Y/3;

    x1:=a.X/2 + b.X/2;
    y1:=a.Y/2 + b.Y/2;

    x2:=a.X/2 + c.X/2;
    y2:=a.Y/2 + c.Y/2;

    x3:=b.X/2 + c.X/2;
    y3:=b.Y/2 + c.Y/2;

    if rPolygonContainsPixel(lp,lp_n,px,py) >= 0 then
    if rPolygonContainsPixel(lp,lp_n,x1,y1) >= 0 then
    if rPolygonContainsPixel(lp,lp_n,x2,y2) >= 0 then
    if rPolygonContainsPixel(lp,lp_n,x3,y3) >= 0 then
    Result:=true
  end
end;

procedure NextRib(xy: TPointList; ir,dir: Integer);
var
  rb: TRib;
  a,b,c: TPoint;
begin
  rb:=Ribs[ir];
  a:=Vertexes[rb.p1];
  b:=Vertexes[rb.p2];
  Middle_Point(a,b,c);

  if dir = 1 then xy.Continue(c)
             else xy.Continue1(c)
end;

procedure BeginTri(xy: TPointList;
                   lp: PLPoly; lp_n: Integer;
                   it0,ir0,dir: Integer);
var
  it1,it2,ir: Integer; tr: TTri;
begin
  it1:=it0; ir:=ir0;
  while it1 >= 0 do begin
    tr:=Tris[it1]; it2:=-1;

    if Contains_tri(lp,lp_n,it1) then
    if rib_to_top(tr,ir) then

    if (tr.p1 < fCline) <> (tr.p3 < fCLine) then begin
      ir:=tr.r3; NextRib(xy,ir,dir);
      it2:=NextTri(it1,ir)
    end else

    if (tr.p2 < fCline) <> (tr.p3 < fCLine) then begin
      ir:=tr.r2; NextRib(xy,tr.r2,dir);
      it2:=NextTri(it1,ir)
    end;

    if ir = ir0 then Break;
    if it2 = it0 then Break;
    it1:=it2
  end;
end;

var
  xy: TPointList;
  i,ir,lp_n: Integer; rb: TRib;
begin
  Result:=0; Buf:=nil; lp_n:=n1+n2;

  xy:=TPointList.Create(1024);
  try
    fCline:=n1;

    i:=RibOf(0,n1+n2-1);
    if i >= 0 then begin rb:=Ribs[i];
      if Contains_tri(lp,lp_n,rb.t1) then ir:=i else
      if Contains_tri(lp,lp_n,rb.t2) then ir:=i;
    end;

    if ir < 0 then
    for i:=0 to ribn-1 do begin rb:=Ribs[i];
      if (rb.p1 < fCline) <> (rb.p2 < fCLine) then

      if Contains_tri(lp,lp_n,rb.t1) then ir:=i else
      if Contains_tri(lp,lp_n,rb.t2) then ir:=i;
      if ir >= 0 then Break
    end;

    if ir >= 0 then begin
      NextRib(xy,ir,1); rb:=Ribs[ir];
      BeginTri(xy,lp,lp_n,rb.t1,ir,1);
      BeginTri(xy,lp,lp_n,rb.t2,ir,-1);
    end;

    if xy.Count > 1 then
    Result:=xy.Release(Pointer(Buf))
  finally
    xy.Free
  end
end;

function TTriangulator.Calculate(Atops: Pointer;
                                 Atopn,Astride: Integer): Integer;

procedure Improve_triangulation;

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
  rb: TRib; t1,t2: TTri;
begin
  for irep:=1 to nrep do begin

    nlfc:=fnlf;
    for ilf:=0 to fnlf-1 do begin

      ir:=fwl[ilf]; rb:=Ribs[ir];

      it1:=rb.t1; it2:=rb.t2;
      if (it1 >= 0) and (it2 >= 0) then begin

        t1:=Tris[it1]; t2:=Tris[it2];

        if rib_to_top(t1,ir) then
        if rib_to_top(t2,ir) then

        if BadPare(t1.p1,t1.p2,t1.p3,t2.p3) then begin

          if debug then DrawRib(ir,-1);

          SetRib(ir, t1.p3,t2.p3, rb.t1,rb.t2);

          SetTri(it1, t1.p2,t1.p3,t2.p3, t1.r2,ir,t2.r3);
          SetTri(it2, t1.p1,t2.p3,t1.p3, t2.r2,ir,t1.r3);

          rib_relink(t1.r3, it1,it2);
          rib_relink(t2.r3, it2,it1);

          if debug then begin
            print('flip '+IntToStr(ir));
            drawtri(it1,2);
            drawtri(it2,2)
          end;

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

procedure point_outside(ip: Integer);
var
  pp: PPoint;
  iliv,ilvs,il,rc: Integer;
  rb: TRib; ixvs,ixvspv: Boolean;

begin
  pp:=PPoint(fTops + ip*fstride);

  iliv:=0; ilvs:=0;
  ixvs:=RibVisible(bp[0],pp);

  for il:=1 to nl-1 do begin

    ixvspv:=ixvs;
    ixvs:=RibVisible(bp[il],pp);

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

  if iliv+ilvs = 0 then ilvs:=nl-1;

  if ilvs < iliv then Inc(ilvs,nl);

  // Shift (rotate) the border to have the invisible border
  // line segments contained in the first part of the array.

  if iliv > 0 then begin

    for il:=0 to iliv do
    fbp[il+nl]:=fbp[il];

    for il:=0 to nl-1 do
    fbp[il]:=bp[il+iliv];

    Dec(ilvs,iliv)
  end;

  // Add triangles, update border
  if ilvs < nl then begin

    for il:=ilvs to nl-1 do begin

      rc:=bp[il]; rb:=Ribs[rc];

      if il > ilvs then
        Ribs[ribn-1].t2:=trin
      else begin
        SetRib(fribn,rb.p1,ip,trin,-1);
        fbp[il]:=ribn; Inc(fribn);
      end;

      if il = nl-1 then fbp[ilvs+1]:=ribn;

      SetRib(fribn,ip,rb.p2,trin,-1);
      Inc(fribn);

      Ribs[rc].t2:=trin;

      SetTri(ftrin, rb.p2,rb.p1,ip,
                    rc,ribn-2,ribn-1);
      Inc(ftrin);

      if debug then begin
        DrawTri(trin-1,2);
        print('flip begin');
      end;

      fwl[fnlf]:=rc; Inc(fnlf);
      Improve_triangulation;
    end;

    fnl:=ilvs+2;
  end;

  if debug then DrawBound(9);
end;

var
  n4,n2,ndp: Integer;
  wp: PIntegers; nwp: Integer;
  i,i1,i2,i3,ip,iw: Integer;
begin
  Result:=-1;

  fTops:=ATops;
  ftopn:=Atopn;
  fstride:=AStride;

  fbp:=xFreePtr(fbp);
  fwl:=xFreePtr(fwl);

  fTris:=nil; ftrin:=0;
  fRibs:=nil; fribn:=0;
  fElapsed:=0;

  if Atopn >= 3 then begin

    ndp:=Atopn; n2:=ndp*2; n4:=ndp*4;

    fnwl:=10*ndp; fnlf:=0;

    fwl:=xAllocPtr(ndp*Sizeof(int)+
                   fnwl*Sizeof(int));

    fbp:=xAllocPtr(n2*Sizeof(int)+
                   n4*SizeOf(TRib)+
                   n4*SizeOf(TTri));

    if Assigned(fwl) then
    if Assigned(fbp) then begin

      Dec(fnwl,10);

      fRibs:=@bp[n2]; fTris:=@Ribs[n4];

      Dec(fnwl,ftopn); wp:=@fwl[fnwl];
      nwp:=order_points1(fTops,ftopn,fStride, wp);
      if nwp >= 3 then begin

        begin_process;

        i1:=wp[0]; i2:=wp[1]; i3:=wp[2];

        SetRib(0,i1,i2,0,-1); fbp[0]:=0;
        SetRib(1,i2,i3,0,-1); fbp[1]:=1;
        SetRib(2,i3,i1,0,-1); fbp[2]:=2;

        SetTri(0, i1,i2,i3, 0,1,2);

        fribn:=3; fnl:=3; ftrin:=1;

        if debug then begin
          DrawPoint(i1,14);
          DrawPoint(i2,14);
          DrawPoint(i3,14);

          DrawTri(trin-1,2);
          DrawBound(9)
        end;
 
        for iw:=3 to nwp-1 do begin
          ip:=wp[iw]; if debug then
          if not log_vertex(ip) then Break;
          point_outside(ip);
        end;

        end_process; Result:=trin
      end
    end
  end
end;

procedure TTriangulator.filter_bound_ribs(f1,f2: int);

procedure rib_off_tri(var rb: TRib; it: int);
begin
  if rb.t1 = it then rb.t1:=-1 else
  if rb.t2 = it then rb.t2:=-1
end;

var
  ir,it,loop,rc: int; rb: TRib; tr: TTri;
  v1,v2,v3: TPoint; cos1,cos2,minCos1,minCos2: double;
begin
  minCos1:=Cos(f1*DegToRad);
  minCos2:=Cos(f2*DegToRad);

  for loop:=1 to 32 do begin

    rc:=0;
    for ir:=0 to ribn-1 do begin

      rb:=fRibs[ir];

      if ((rb.t1 < 0) and (rb.t2 >= 0))
      or ((rb.t1 >= 0) and (rb.t2 < 0)) then begin

        it:=rb.t1;
        if it < 0 then it:=rb.t2;

        if it >= 0 then begin
          tr:=Tris[it];
          if rib_to_top(tr,ir) then begin

            v1:=Get_Vertex(tr.p1);
            v2:=Get_Vertex(tr.p2);
            v3:=Get_Vertex(tr.p3);

            if Int_cos3(v1,v2,v3,1,cos1) then
            if cos1 < -minCos2 then

            if Int_cos3(v2,v1,v3,1,cos1) then
            if cos1 < -minCos2 then

            if Min(cos1,cos2) < minCos1 then begin
              rib_off_tri(fRibs[tr.r1],it);
              rib_off_tri(fRibs[tr.r2],it);
              rib_off_tri(fRibs[tr.r3],it);
              Inc(rc)
            end
          end
        end
      end
    end;

    if rc = 0 then Break
  end
end;

function TTriangulator.Verify: Integer;

function TriContainsRib(it,ir: Integer): Boolean;
begin
  Result:=true; if it >= 0 then with Tris[it] do
  Result:=(ir = r1) or (ir = r2) or (ir = r3)
end;

function RibContainsTri(ir,it: Integer): Boolean;
begin
  with Ribs[ir] do
  Result:=(it = t1) or (it = t2)
end;

var
  il,ir,it: int; rb: TRib; tr: TTri;
begin
  Result:=0;

  for ir:=0 to ribn-1 do begin
    rb:=Ribs[ir];

    if rb.p1 = rb.p2 then begin
      Result:=-1; Break
    end else

    if not TriContainsRib(rb.t1,ir)
    or not TriContainsRib(rb.t2,ir) then begin
      Result:=-2; Break
    end
  end;

  if Result = 0 then
  for it:=0 to trin-1 do begin
    tr:=Tris[it];
    if not RibContainsTri(tr.r1,it)
    or not RibContainsTri(tr.r2,it)
    or not RibContainsTri(tr.r2,it) then begin
      Result:=-3; Break
    end;

    if (tr.p1 = tr.p2)
    or (tr.p1 = tr.p3)
    or (tr.p2 = tr.p3) then begin
      Result:=-4; Break
    end;

    rb:=Ribs[tr.r1];
  end;

  if Result = 0 then
  for il:=0 to nl-1 do begin
    rb:=fRibs[ bp[il] ];
    if (rb.t1 >= 0) and (rb.t2 >= 0) then begin
      Result:=-5; Break
    end
  end
end;

procedure TTriangulator.dump_log;
var
  log: TTextfile; rc: Integer;
begin
  log:=TTextfile.Create;
  try
    rc:=Verify;
    if log.Make_work('tops.txt') then begin
      log.WriteStr('NPoints : '+IntToStr(ftopn));
      log.WriteStr('TriCount: '+IntToStr(ftrin));
      log.WriteStr('RibCount: '+IntToStr(fribn));
      log.WriteStr('Verify  : '+IntToStr(rc)); log.WriteStr('');
      log.WriteStr('Elapsed  : '+RealToStr(fElapsed/1000,1)+'sec');
    end
  finally
    log.Free
  end
end;

procedure TTriangulator.dump(Dest: PChar;
                             bw,bh,skip: Integer;
                             sys: psys; gb: PGPoly);

function tr_to_tr2(tp: PTriArray; tp2: PTriArray2;
                   sp: PIntegers; skip: Integer): Integer;
var
  it,i: Integer; tr: TTri; tr2: TTri2;
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
  tp: PTriArray; rib: TRib;
  lp: PVertexes; tp2: PTriArray2;

  vp: PVPoly;

  np: pxyz_array; np1: PIPoly;

  hs: TSize; hp,sp: PIntegers;

  lp1: PLPoly; v: VPoint;
  hdr: ttin_hdr; buf: tbytes;
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
    if Assigned(np) then
    if Assigned(tp2) then begin

      if sp = nil then skip:=0;

      Fillchar(hdr,Sizeof(hdr),0);
      hdr.mag:=tin_mag1; hdr.ver:=1;
      hdr.vn:=topn-skip;

      tp:=fTris;

      z1:=0; z2:=0;
      if Stride = 8 then begin
        lp1:=PLPoly(tops);
        lp1:=@lp1[skip];

        v.z:=0;
        for i:=0 to hdr.vn-1 do begin
          v.x:=lp1[0].X;
          v.y:=lp1[0].Y;
          vp[i]:=v; lp1:=@lp1[1]
        end;
      end
      else begin
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
      end;

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
          hdr.bn:=nl + 1;

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
  end
end;

procedure TTriangulator.begin_process;
begin
  fElapsed:=GetTickCount
end;

procedure TTriangulator.end_process;
begin
  fElapsed:=GetTickCount-fElapsed
end;

function TTriangulator.log_vertex(ip: Integer): Boolean;
begin
  Result:=true;
end;

procedure TTriangulator.print(msg: string);
begin
end;

procedure TTriangulator.DrawBound(cl: Integer);
begin
end;

procedure TTriangulator.DrawTri(it,cl: Integer);
begin
end;

procedure TTriangulator.DrawRib(ir,cl: Integer);
begin
end;

procedure TTriangulator.DrawPoint(ip,cl: Integer);
begin
end;

function Calc_tri(lp: PLPoly; ndp: Integer;
                  var Tri: PTriWords): Integer;
var
  tang: TTriangulator;
begin
  Result:=0; Tri:=xFreePtr(Tri);

  tang:=TTriangulator.Create;
  try
    if ndp > 3 then
    if tang.Calculate(lp,ndp,8) > 0 then begin
      Result:=tang.Get_TriWords(Tri);
    end;
  finally
    tang.Free;
  end
end;

function Bound_tri(lp: PLPoly; ndp: Integer;
                   var Bound: PIntegers): Integer;
var
  tang: TTriangulator;
begin
  Result:=0; Bound:=xFreePtr(Bound);

  tang:=TTriangulator.Create;
  try
    if ndp > 3 then
    if tang.Calculate(lp,ndp,8) > 0 then begin
      Result:=tang.Get_Bound(Bound);
    end;
  finally
    tang.Free;
  end
end;


function Calc_tri2(var tp2: PTriArray2;
                   lp: PLPoly; ndp: Integer;
                   cls: Float): Integer;
var
  tang: TTriangulator;
begin
  Result:=0; tp2:=xFreePtr(tp2);

  tang:=TTriangulator.Create;
  try
    if ndp > 3 then
    if tang.Calculate(lp,ndp,8) > 0 then begin

      tp2:=xAllocPtr(tang.trin*SizeOf(TTri2));
      if Assigned(tp2) then
      Result:=tang.Get_Tris(tp2,cls)
    end;
  finally
    tang.Free;
  end
end;

function Calc_tri2a(var tp1: PTriArray2;
                    tp1_size: int;

                    var tp2: PTriArray2;
                    lp: PLPoly; ndp: int;
                    cls: Float): int;
var
  tang: TTriangulator; tn: int;
begin
  Result:=0; tp2:=xFreePtr(tp2);

  tang:=TTriangulator.Create;
  try
    if ndp > 3 then
    if tang.Calculate(lp,ndp,8) > 0 then begin

      tn:=tang.trin;
      if tn <= tp1_size then
        Result:=tang.Get_Tris(tp1,cls)
      else begin
        tp2:=xAllocPtr(tang.trin*SizeOf(TTri2));
        if Assigned(tp2) then begin
          Result:=tang.Get_Tris(tp2,cls);
          tp1:=tp2
        end
      end
    end
  finally
    tang.Free;
  end
end;

function Calc_ribs(lp: PLPoly; ndp: Integer;
                   var Buf: PIntegers): Integer;
var
  tang: TTriangulator;
begin
  Result:=0; Buf:=xFreePtr(Buf);

  tang:=TTriangulator.Create;
  try
    if ndp > 3 then
    if tang.Calculate(lp,ndp,8) > 0 then
    Result:=tang.Get_Ribs(Buf);
  finally
    tang.Free;
  end
end;

function Calc_ribs1a(lp: PLPoly; ndp,f1,f2: int;
                     var Buf: PIntegers): int;
var
  tang: TTriangulator;
begin
  Result:=0; Buf:=xFreePtr(Buf);

  tang:=TTriangulator.Create;
  try
    if ndp > 3 then
    if tang.Calculate(lp,ndp,8) > 0 then begin
      tang.filter_bound_ribs(f1,f2);
      Result:=tang.Get_Ribs(Buf);
    end;
  finally
    tang.Free;
  end
end;

function tri_cline(lp: PLPoly; n1,n2: Integer;
                    out Buf: PLPoly; tin: PChar): Integer;
var
  tang: TTriangulator;
  lt,rb: TPoint;
begin
  Result:=0; Buf:=nil;

  tang:=TTriangulator.Create;
  try
    if n1+n2 > 3 then
    if tang.Calculate(lp,n1+n2,8) > 0 then
    if tang.Verify = 0 then begin

      Result:=tang.Get_cline(lp,n1,n2,Buf);

      if Assigned(tin) then begin
        FileErase(tin);

        Max_Poly_Bound(lp,n1+n2,lt,rb);
        tang.dump(tin,rb.X-lt.X+1,rb.Y-lt.Y+1,0,nil,nil);
      end
    end;
  finally
    tang.Free;
  end
end;

function Free_tris(Tris: PTriWords): Pointer;
begin
  Result:=xFreePtr(Tris)
end;

function Free_ribs(Ribs: PIntegers): Pointer; stdcall;
begin
  Result:=xFreePtr(Ribs)
end;

function Tops_to_tin(Path,Dest: PChar): Boolean;
var
  vm: TReadfile;
  tr: TTriangulator;
  tops: PPoint; ndp,w,h: Integer;
  tin: TShortstr;
begin
  Result:=false;

  vm:=TReadfile.Create;
  tr:=TTriangulator.Create;
  try
    if vm.Open(Path) then
    if vm.Size > 32 then begin
      tops:=@vm.Buf[32];
      ndp:=vm.Get_long(0);
      w:=vm.Get_long(24);
      h:=vm.Get_long(28);
      tr.Calculate(tops,ndp,12);
      tr.dump_log;

      StrUpdateExt(tin,Path,'.tin');
      if Strlen(Dest) > 0 then StrCopy(tin,Dest);

      tr.dump(tin,w,h,0,nil,nil);
      Result:=true
    end;
  finally
    tr.Free;
    vm.Free
  end
end;

end.
