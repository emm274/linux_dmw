unit xcurve; interface

uses
  Classes,Math,otypes,xlist;

type
  TCurveCoeff = record
    a,b,c,d: Double
  end;

type
  TCurveXY = class
    function Value(t: double): TGauss; virtual; abstract;
  end;

  TValueXY = function(t: double): TGauss of object;

  TCurve = class(TCurveXY)
    function Value(t: double): TGauss; override;

    function iTriangle(lp: PLPoly): double;
    function iBezier(lp: PLPoly): double;
    function lBezier(lp: PLPoly): double;
    function gBezier(gp: PGPoly): double;
    function lPlot(t: double): TPoint;
    procedure Plot_t(t: double; out x,y: double);
    procedure Norm_t(t,z: double; out x,y: double);
    procedure Dir_t(t,d: double; out x,y: double);
    function Diff_t(t: double): TGauss;

  private
    cx,cy: TCurveCoeff;
    _len: double;

    function dLength(t: double): double;
    function dSquare(t: double): double;

  public
    property Length: double read _len;
    function Square: double;
  end;

  TCurvePoly = class(TCurve)

    function GetPoly(dst: PLLine;
                     dstMax,maxSP: int;
                     eps: double): int;

  private
    fdst: PLLine;
    fdstMax: int;
    fmaxSP: int;
    feps: double;
  end;

  TLocateCurve = class(TCurve)
    constructor Create(const lt,rb: TPoint);

    function This_curve(lp: PLLine;
                        scan: Boolean;
                        out Ind: Integer;
                        out P: TPoint): Float;

  private
    _lt,_rb: TPoint;
    flt,frb: TGauss;

    function Locate: Float;
  end;

  TSignCurve = class(TCurve)
    constructor Create;
    destructor Destroy; override;

    function Plot_lp(lp: PLPoly; N: Integer): PLLine;
  private

    PolyBuf: PLLine;
    PolyMax: Integer;
    sp_max: Integer;

    Fkx,Fky: double;

    Flt,Frb: TPoint;

    function Push(x,y: double): boolean;

  public
    property kx: Double write Fkx;
    property ky: Double write Fky;

    property lt: TPoint read Flt;
    property rb: TPoint read Frb;
  end;

  TCurveTo = class(TCurve)
    procedure Plot_rib(pp: PLPoly);
    procedure Plot_lp(lp: PLLine);

    function Run_lp(lp: PLLine): Extended;
    procedure Run_from_to; virtual;

    procedure Push(x,y: double); virtual;
    procedure Lock; virtual;
  end;

  TDrawPolyFunc = function(lp: PLPoly; lp_n: Integer): Integer of object;

  TDrawCurve = class
    constructor Create(ADrawOp: TDrawPolyFunc;
                       Max: Integer; Lock: Boolean);

    destructor Destroy; override;

    function Draw_lcurve(lp: PLPoly; n: int): int;
    function Draw_pcurve(lp: PLPoly; n,lock: int): int;

    function Draw_gcurve(xy: PGPoly; Count: int): int;

    function Draw_ellipse(lp: PLPoly; n: int): int;
    function Draw_sector(lp: PLPoly): Integer;

    function Draw_bspline(lp: PLPoly; n,i1,i2,lock: int): int;

    function Draw_bsplinev(lp: PLPoly; vp: PGPoly; n: int): int;

    function Draw_bspline1(lp: PLPoly; n,rank: int): int;
    function Draw_bspline2(lp: PLPoly; n: int): int;

  private
    fbuf: PLLine;
    fbufMax: Integer;

    fDrawOp: TDrawPolyFunc;
    fDrawCount: Integer;

    fEps: double;
    fStep: double;
    fLock: longbool;

    fCx,fCy,fRad: double;

    fbsplineK: double;

    fOnCurve: TGaussFunc;

    function beginDraw: bool;

    procedure draw_Poly(lp: PLLine);
    procedure dst_Poly(lp: PLLine);

    procedure dst_Point(x,y: Double);

    procedure plot_curve(value: TValueXY);

  public
    property Eps: double write fEps;
    property bsplineK: double write fbsplineK; 
    property OnCurve: TGaussFunc write fOnCurve;
  end;

  TCurveToPoly = class(TDrawCurve)

    constructor Create(Adest: PLLine;
                       AInd,AMax: int;
                       AStep,AEps: double);
  private
    fdest: PLLine;
    fInd,fMax: int;
    fErr: int;

    function lp_Dump(lp: PLPoly; n: Integer): Integer;

  public
    property Ind: Integer read fInd;
    property Err: int read fErr write fErr;
  end;

  TCurveToPointList = class(TDrawCurve)

    constructor Create(Adest: TPointList;
                       AStep,AEps: double);
  private
    fdest: TPointList;
    function lp_Dump(lp: PLPoly; n: Integer): Integer;
  end;

  TCurveToPolyline = class(TXyzList)

    function Load(Curve: TXyzList; eps: Double): Integer;

    function xLoad(Curve: TXyzList; eps: Double;
                   MaxCount: Integer): Integer;

    function xLine(const v0,v3: txyz;
                   const z1,z2,eps: Double;
                   MaxCount: Integer): Integer;

  private
    feps: Double;
    cx,cy,cz: TCurveCoeff;

    procedure OutCurve;
  end;

  TCurveToLCurve = class(TDrawCurve)
    constructor Create;
    destructor Destroy; override;
  private
    fBuf: TPointList;
    function CurveProc(gp: PGPoly; N: Integer): Integer;
  public
    property Buf: TPointList read fBuf;
  end;

function Init_c(x0,x1,x2,x3: double): TCurveCoeff;
function Plot_c(const c: TCurveCoeff; t: Double): Double;
function Diff_c(const c: TCurveCoeff; t: double): double;

procedure lplot_t(lp: PLPoly; t: double; vp: PLPoly);
procedure lplot_c(lp: PLPoly; t: double; cp: PLPoly);

function Get_bspline3b(v1,dv1,v2,dv2: double): TCurveCoeff;

function CurveLength(lp: PLLine): Extended;

function CurveToLLine(dst,src: PLLine; max: Integer;
                      dist,cls: double): Integer;

function CurveToLPoly(dst,src: PLLine;
                      dstMax: int; eps: double): int;

function CurveToPoints(dst,src: PLLine;
                       cnt,max: Integer;
                       step,skip: Double): Integer;

function LLineToCurve(dst,src: PLLine; max: int; eps: double): int;

function BsplineToLCurve(dst,src: PLLine; max: int): int;

function SectorToLPoly(lp: PLLine; ind,max: int;
                       const p3: TPoint; eps: Double): int;

implementation

uses
  xline,xpoly,xy,xbezier;

type
  TValue = function(t: double): double of object;

function Integral(val: TValue): double;
const
  x: array[0..6] of double =
    (-0.883862,-0.529657,-0.323912,0,
     0.323912,0.529657,0.883862);
var
  i: Integer;
begin
  Result:=0; for i:=0 to 6 do
  Result:=Result+val((x[i]+1)/2);
  Result:=Result/7
end;

function Init_c(x0,x1,x2,x3: double): TCurveCoeff;
begin
  with Result do begin
    d:=x0; c:=3*(x1-x0); b:=3*(x2-x1)-c; a:=x3-x0-c-b
  end
end;

function Init_l(x0,x3: Double): TCurveCoeff;
begin
  with Result do begin
    d:=x0; c:=x3-x0; b:=0; a:=0
  end
end;

function Plot_c(const c: TCurveCoeff; t: Double): Double;
begin
  Result:=t*(t*(t*c.a + c.b) + c.c) + c.d
end;

function Diff_c(const c: TCurveCoeff; t: double): double;
begin
  Result:=t*(3*c.a*t+2*c.b)+c.c
end;

procedure Backup_c(const c: TCurveCoeff;
                   out x0,x1,x2,x3: double);
begin
  x0:=c.d;
  x1:=x0+c.c/3; x2:=x1+(c.b+c.c)/3;
  x3:=x0+c.a+c.c+c.b
end;

function ctrl_c(const c: TCurveCoeff; t: double): double;
var
  _c: TCurveCoeff; x0,x1,x2,x3: double;
begin
  _c:=c;
  with _c do begin
    a:=a*t*t*t; b:=b*t*t; c:=c*t
  end;

  Backup_c(_c, x0,x1,x2,x3);
  Result:=x3+x3-x2
end;

function TCurve.iTriangle(lp: PLPoly): double;
var
  a,b,c: TPoint; p1,p2: TGauss;
begin
  a:=lp[0]; b:=lp[1]; c:=lp[2];
  p1.x:=a.X+(b.X-a.X)*4/5;
  p1.y:=a.Y+(b.Y-a.Y)*4/5;

  p2.x:=c.X+(b.X-c.X)*4/5;
  p2.y:=c.Y+(b.Y-c.Y)*4/5;

  cx:=Init_c(a.X,p1.x,p2.x,c.X);
  cy:=Init_c(a.Y,p1.y,p2.y,c.Y);
  _len:=Integral(dLength); Result:=_len
end;

function TCurve.iBezier(lp: PLPoly): double;
begin
  cx:=Init_c(lp[0].X,lp[1].X,lp[3].X,lp[2].X);
  cy:=Init_c(lp[0].Y,lp[1].Y,lp[3].Y,lp[2].Y);
  _len:=Integral(dLength); Result:=_len
end;

function TCurve.lBezier(lp: PLPoly): double;
var
  p3: TPoint;
begin
  p3.X:=lp[2].X+lp[2].X-lp[3].X;
  p3.Y:=lp[2].Y+lp[2].Y-lp[3].Y;
  cx:=Init_c(lp[0].X,lp[1].X,p3.X,lp[2].X);
  cy:=Init_c(lp[0].Y,lp[1].Y,p3.Y,lp[2].Y);
  _len:=Integral(dLength); Result:=_len
end;

function TCurve.gBezier(gp: PGPoly): double;
begin
  cx:=Init_c(gp[0].x,gp[1].x,gp[3].x,gp[2].x);
  cy:=Init_c(gp[0].y,gp[1].y,gp[3].y,gp[2].y);
  _len:=Integral(dLength); Result:=_len
end;

function TCurve.lPlot(t: double): TPoint;
var
  x,y: Double;
begin
  Plot_t(t, x,y);
  Result.X:=Round(x);
  Result.Y:=Round(y);
end;

procedure TCurve.Plot_t(t: double; out x,y: double);
begin
  with cx do x:=t*(t*(a*t + b) + c) + d;
  with cy do y:=t*(t*(a*t + b) + c) + d;
end;

function TCurve.Value(t: double): TGauss;
begin
  with cx do Result.x:=t*(t*(a*t + b) + c) + d;
  with cy do Result.y:=t*(t*(a*t + b) + c) + d;
end;

procedure TCurve.Norm_t(t,z: double; out x,y: double);
var
  nx,ny,len: double;
begin
  if t < 0 then t:=0;
  if t > 1 then t:=1;

  with cx do x:=t*(t*(a*t + b) + c) + d;
  with cy do y:=t*(t*(a*t + b) + c) + d;

  nx:=-Diff_c(cx,t);
  ny:= Diff_c(cy,t);

  len:=Hypot(nx,ny);
  if len > Small then begin
    x:=x + nx*z/len;
    y:=y + ny*z/len;
  end
end;

procedure TCurve.Dir_t(t,d: double; out x,y: double);
var
  dx,dy,len: Double;
begin
  dx:=Diff_c(cx,t);
  dy:=Diff_c(cy,t);

  len:=Hypot(dx,dy);
  if len > Small then begin
    x:=dx*d/len; y:=dy*d/len;
  end
  else Plot_t(t, x,y)
end;

function TCurve.Diff_t(t: double): TGauss;
begin
  Result.x:=Diff_c(cx,t);
  Result.y:=Diff_c(cy,t);
end;

function TCurve.Square: double;
begin
  Result:=Integral(dSquare)/2
end;

function TCurve.dLength(t: double): double;
var
  dx,dy: double;
begin
  dx:=Diff_c(cx,t);
  dy:=Diff_c(cy,t);
  Result:=Hypot(dx,dy)
end;

function TCurve.dSquare(t: double): double;
var
  x,y,dx,dy: double;
begin
  Plot_t(t, x,y);

  dx:=Diff_c(cx,t);
  dy:=Diff_c(cy,t);

  Result:=x*dy - y*dx
end;

function TCurvePoly.GetPoly(dst: PLLine;
                            dstMax,maxSP: int;
                            eps: double): int;

procedure plot(const a: TPoint; t1,t2: double; sp: int);
var
  t: double; b,c: TPoint;
begin
  t:=(t1+t2)/2; b:=lPlot(t); c:=lPlot(t2);

  if sp < fmaxSP then
  if Abs(Dist_to_Line(b,a,c)) > feps then begin
    plot(a,t1,t,sp+1);
    LPoly_push(fdst,fdstMax,b);
    plot(a,t,t2,sp+1)
  end;

  LPoly_push(fdst,fdstMax,c);
end;

var
  a,b,c: TPoint;
begin
  fdst:=dst;
  fdstMax:=dstMax;
  fmaxSP:=maxSP;

  feps:=eps;
  if feps < 1.5 then feps:=1.5;

  dst.N:=-1; a:=lPlot(0);
  LPoly_push(fdst,fdstMax,a);
  plot(a,0,1,0);

  Result:=fdst.N
end;

constructor TLocateCurve.Create(const lt,rb: TPoint);
begin
  inherited Create;
  _lt:=lt; flt:=_Gauss(lt.x,lt.y);
  _rb:=rb; frb:=_Gauss(rb.x,rb.y);
end;

function TLocateCurve.This_curve(lp: PLLine;
                                 scan: Boolean;
                                 out Ind: Integer;
                                 out P: TPoint): Float;
var
  i: int; pp: LOrient;
begin
  Result:=-1; Ind:=0; P:=Point(0,0);

  if lp.N >= 3 then begin

    pp[2]:=lp.Pol[0];
    pp[3]:=lp.Pol[1]; i:=2;

    if PortContainsPoint(_lt,_rb, pp[2].x,pp[2].y) then begin
      Ind:=0; P:=pp[2]; Result:=0
    end else

    while i < lp.N do begin
      pp[0]:=pp[2]; pp[2]:=lp.Pol[i]; Inc(i);
      pp[1]:=pp[3]; pp[3]:=lp.Pol[i]; Inc(i);

      if PortContainsPoint(_lt,_rb, pp[2].x,pp[2].y) then begin
        Ind:=i-2; P:=pp[2]; Result:=0; Break
      end else
      if scan then begin
        lBezier(@pp); Result:=Locate;
        if Result >= 0 then begin
          Ind:=i-4; P:=lPlot(Result); Break
        end
      end
    end
  end
end;

function TLocateCurve.Locate: Float;

function Locate_dt(t1,t2: Float): Float;
var
  g1,g2,gc: TGauss; tc: Float;
  d1,d2,d3: Double;
begin
  Result:=-1;
  Plot_t(t1,g1.x,g1.y);
  Plot_t(t2,g2.x,g2.y);

  if flt.x < Max(g1.x,g2.x) then
  if frb.x > Min(g1.x,g2.x) then
  if flt.y < Max(g1.y,g2.y) then
  if frb.y > Min(g1.y,g2.y) then begin

    tc:=t1/2 + t2/2; Plot_t(tc,gc.x,gc.y);

    d1:=Hypot(g2.x-g1.x,g2.y-g1.y);
    d2:=Hypot(gc.x-g1.x,gc.y-g1.y);
    d3:=Hypot(g2.x-gc.x,g2.y-gc.y);

    if d1 < 0.9*(d2 + d3) then begin
      Result:=Locate_dt(t1,tc);
      if Result < 0 then
      Result:=Locate_dt(tc,t2);
    end else
    if xLocate_Line(g1,g2, flt,frb, gc) then begin
      d2:=Hypot(gc.x-g1.x,gc.y-g1.y);
      Result:=t1 + (t2-t1) * d2 / d1
    end
  end
end;

var
  i: Integer;
begin
  Result:=-1;

  for i:=0 to 3 do begin
    Result:=Locate_dt(i/4,(i+1)/4);
    if Result >= 0 then Break
  end
end;

constructor TSignCurve.Create;
begin
  inherited Create; PolyMax:=8000-1;
  PolyBuf:=Alloc_LLine(PolyMax);
  sp_max:=8; Fkx:=1; Fky:=1
end;

destructor TSignCurve.Destroy;
begin
  xFreePtr(PolyBuf);
  inherited Destroy
end;

function TSignCurve.Plot_lp(lp: PLPoly; N: Integer): PLLine;

function xPlot(sp: Integer; t1,z1,t2,z2: double): boolean;
var
  x1,y1,x2,y2, x3,y3,t3,z3, dist: double;
begin
  Norm_t(t1,z1, x1,y1);
  Norm_t(t2,z2, x2,y2);
  if sp = 0 then Push(x1,y1);

  t3:=(t1+t2)/2; z3:=(z1+z2)/2;
  Norm_t(t3,z3, x3,y3);

  dist:=Hypot(x2-x1,y2-y1);

  if sp <= sp_max then if dist > 16 then begin
    xPlot(sp+1, t1,z1,t3,z3); xPlot(sp+1, t3,z3,t2,z2)
  end;

  Result:=Push(x2,y2)
end;

var
  t1,z1,t2,z2: double; i: Integer;

begin
  Result:=nil;

  if n > 0 then if Length > 0 then
  if PolyBuf <> nil then begin

    repeat PolyBuf.N:=-1;
      t1:=lp[0].x*Fkx; z1:=lp[0].y*Fky;

      i:=1; while i <= n do begin
        t2:=lp[i].x*Fkx; z2:=lp[i].y*Fky;
        if not xPlot(0,t1,z1,t2,z2) then Break;
        t1:=t2; z1:=z2; Inc(i)
      end;

      sp_max:=sp_max div 2;

    until i > n;

    if PolyBuf.N > 0 then
    Result:=PolyBuf
  end
end;

function TSignCurve.Push(x,y: double): boolean;
var
  p: TPoint;
begin
  p.x:=Round(x); p.y:=Round(y);

  Result:=true; with PolyBuf^ do
  if N = -1 then begin
    N:=0; Pol[0]:=p;
    Flt:=p; Frb:=p
  end else

  if not Points_Equal(Pol[N],p) then begin
    Max_lPort(Flt,Frb, p);

    if N = PolyMax then
      Result:=false
    else begin
      Inc(N); Pol[N]:=p
    end
  end
end;

procedure TCurveTo.Plot_rib(pp: PLPoly);

procedure xPlot(sp: Integer; t1,t2: double;
                const p1,p2: tgauss);
var
  t3,d1,d2: double; p3: tgauss;
begin
  t3:=(t1+t2)/2; Plot_t(t3,p3.x,p3.y);

  d1:=Hypot(p2.x-p1.x,p2.y-p1.y);

  d2:=Hypot(p3.x-p1.x,p3.y-p1.y)+
      Hypot(p2.x-p3.x,p2.y-p3.y);

  if (sp <= 8) and (d1 < 0.8*d2) then begin
    xPlot(sp+1, t1,t3, p1,p3); xPlot(sp+1, t3,t2, p3,p2)
  end else Push(p2.x,p2.y)
end;

var
  p1,p2: tgauss; t1,t2: Double; i: Integer;
begin
  Push(pp[0].x,pp[0].y); lBezier(pp);

  p2.x:=pp[0].x; p2.y:=pp[0].y;

  t2:=0; for i:=1 to 16 do begin
    t1:=t2; t2:=t2+1/16; p1:=p2;
    Plot_t(t2,p2.x,p2.y);
    xPlot(0,t1,t2,p1,p2)
  end;

  Push(pp[2].x,pp[2].y);
end;

procedure TCurveTo.Plot_lp(lp: PLLine);
var
  pp: LOrient; i: Integer;
begin
  if lp.N >= 3 then begin

    pp[2]:=lp.Pol[0]; pp[3]:=lp.Pol[1];

    i:=2; while i < lp.N do begin
      pp[0]:=pp[2]; pp[2]:=lp.Pol[i]; Inc(i);
      pp[1]:=pp[3]; pp[3]:=lp.Pol[i]; Inc(i);

      Plot_rib(@pp);
    end;

    if CurveLock(lp) then Lock
  end
end;

function TCurveTo.Run_lp(lp: PLLine): Extended;
var
  i: Integer; p: LOrient;
begin
  Result:=0;

  with lp^ do if N > 1 then begin
    p[2]:=Pol[0]; p[3]:=Pol[1];

    i:=2; while i < N do begin
      p[0]:=p[2]; p[2]:=Pol[i]; Inc(i);
      p[1]:=p[3]; p[3]:=Pol[i]; Inc(i);

      Result:=Result+lBezier(@p);
      Run_from_to
    end
  end
end;

procedure TCurveTo.Run_from_to;
begin
end;

procedure TCurveTo.Push(x,y: double);
begin
end;

procedure TCurveTo.Lock;
begin
end;

type
  TCurveToLLine = class(TCurveTo)
    constructor Create(dst: PLLine; max: Integer; dist,cls: double);
    procedure Push(x,y: double); override;
    procedure Lock; override;

  private
    _dst: PLLine; _max: Integer;
    _dist,_cls: double;
  end;

constructor TCurveToLLine.Create(dst: PLLine; max: Integer; dist,cls: double);
begin
  inherited Create; _dst:=dst;
  _max:=max; _dist:=dist; _cls:=cls
end;

procedure TCurveToLLine.Push(x,y: double);
var
  p: TPoint;
begin
  p.X:=Round(x); p.Y:=Round(y);
  Poly_Push(_dst,_max,p,_dist,_cls);
end;

procedure TCurveToLLine.Lock;
begin
  with _dst^ do if N > 1 then
  if not PolyLock(_dst) then begin
    if N < _max then Inc(N);
    Pol[N]:=Pol[0]
  end
end;

type
  TCurveToPoints = class(TCurveTo)

    constructor Create(dst: PLLine;
                       max: Integer;
                       step: Double);

    procedure Run_from_to; override;
    procedure Push(x,y: double); override;

  private
    _dst: PLLine;
    _max: Integer;
    _step: Double;

    _dist: Double;
  end;

constructor TCurveToPoints.Create(dst: PLLine;
                                  max: Integer;
                                  step: Double);
begin
  inherited Create;
  _dst:=dst; _max:=max;
  _step:=step; _dist:=0;
end;

procedure TCurveToPoints.Run_from_to;
var
  len, x,y: Double;
begin
  len:=Length;

  if len > Small then
  if _step > Small then

  while _dist <= len do begin
    Plot_t(_dist/len, x,y); Push(x,y);
    _dist:=_dist + _step;
  end;

  _dist:=_dist - len;

  if _dist < 0 then begin
    Plot_t(0, x,y); Push(x,y);
    _dist:=0
  end
end;

procedure TCurveToPoints.Push(x,y: double);
begin
  with _dst^ do
  if N < _max then begin Inc(N);
    Pol[N].x:=Round(x); Pol[N].y:=Round(y);
  end
end;

function CurveLength(lp: PLLine): Extended;
var
  curve: TCurveTo;
begin
  Result:=0;

  curve:=TCurveTo.Create;
  try
    Result:=curve.Run_lp(lp)
  finally
    curve.Free
  end
end;

function CurveToLLine(dst,src: PLLine; max: Integer; dist,cls: double): Integer;
var
  curve: TCurveToLLine;
begin
  dst.N:=-1;

  curve:=TCurveToLLine.Create(dst,max,dist,cls);
  try
    curve.Plot_lp(src);
  finally
    curve.Free;
  end;

  Result:=dst.N
end;

type
  TCurveToLPoly = class(TCurveTo)
    constructor Create;
    destructor Destroy; override;
    procedure Push(x,y: double); override;
    procedure Lock; override;

    function GetPoly(dst: PLLine; dstMax: int; eps: double): int;

  private
    fbuf: TPointList;
  end;

constructor TCurveToLPoly.Create;
begin
  inherited Create;
  fbuf:=TPointList.Create(4096)
end;

destructor TCurveToLPoly.Destroy;
begin
  fbuf.Free; inherited
end;

procedure TCurveToLPoly.Push(x,y: double);
begin
  fbuf.AddItem(Round(x),Round(y))
end;

procedure TCurveToLPoly.Lock;
begin
  fbuf.Lock
end;

function TCurveToLPoly.GetPoly(dst: PLLine; dstMax: int; eps: double): int;
begin
  dst.N:=-1; if eps > 0 then
  fbuf.Truncate( eps_LPoly(fbuf.First,fbuf.Count-1,eps)+1 );

  if fbuf.Count < dstMax then
  Result:=fbuf.Get_line(dst,dstMax)
end;

function CurveToLPoly(dst,src: PLLine;
                      dstMax: int; eps: double): int;
var
  plot: TCurveToLPoly;
begin
  dst.N:=-1;

  if CurveLock(src) then
    CurveToLLine(dst,src,dstMax,eps*2,eps)
  else begin
    plot:=TCurveToLPoly.Create;
    try
      plot.Plot_lp(src);
      plot.GetPoly(dst,dstMax,eps)
    finally
      plot.Free
    end
  end;

  Result:=dst.N
end;

function CurveToPoints(dst,src: PLLine;
                       cnt,max: Integer;
                       step,skip: Double): Integer;
var
  Curve: TCurveToPoints;
begin
  dst.N:=-1;

  if src.N >= 3 then

  if max = 1 then begin

    dst.Pol[0]:=src.Pol[0];
    dst.Pol[1]:=src.Pol[src.N-1];
    dst.N:=1

  end else

  if max > 1 then begin

    if cnt > 0 then
    step:=(CurveLength(src) - skip)/cnt;

    if step > 1 then begin

      curve:=TCurveToPoints.Create(dst,max,step);
      try
        curve.Run_lp(src);
      finally
        curve.Free
      end;

      if dst.N >= 0 then
      if not Points_Equal(src.Pol[src.N-1],
                          dst.Pol[dst.N]) then
      with dst^ do begin
        Inc(N); Pol[N]:=src.Pol[src.N-1]
      end;

    end
  end;

  if CurveLock(src) then
  if not PolyLock(dst) then

  with dst^ do
  if N > 0 then
  if N < max then begin
    Inc(N); Pol[N]:=Pol[0]
  end;

  Result:=dst.N
end;

function lLineToBezier(dst,src: PLLine; Max: Integer): Integer;
var
  i: Integer; c: TPoint;
begin
  Result:=-1;

  with src^ do
  if N > 1 then
  if N+N+3 <= Max then begin
    Inc(Result); dst.Pol[Result]:=Pol[0];

    for i:=1 to N do begin
      Middle_Point(Pol[i-1],Pol[i],c);

      if i = 1 then begin
        Inc(Result); dst.Pol[Result]:=c;
      end;

      Inc(Result); dst.Pol[Result]:=c;
      Inc(Result); dst.Pol[Result]:=Pol[i]
    end;

    Mirror_Point(c,Pol[N],c);
    Inc(Result); dst.Pol[Result]:=Pol[N];
    Inc(Result); dst.Pol[Result]:=c;
  end;

  dst.N:=Result
end;

function LLineToCurve(dst,src: PLLine; max: int; eps: double): int;
begin
  Result:=xLLineToCurve(dst,src,max,eps)
end;

constructor TDrawCurve.Create(ADrawOp: TDrawPolyFunc;
                              Max: Integer; Lock: Boolean);
begin
  inherited Create;

  fDrawOp:=ADrawOp;
  if Max = 0 then Max:=32000;
  fbuf:=Alloc_LLine(Max);
  fbufMax:=Max-1; fLock:=Lock;
  fEps:=1; fStep:=1.5;

  fbsplineK:=1/3
end;

destructor TDrawCurve.Destroy;
begin
  xFreePtr(fbuf);
  inherited
end;

function TDrawCurve.beginDraw: bool;
begin
  Result:=false;

  fDrawCount:=0;
  if Assigned(fbuf) then begin
    fbuf.N:=-1; Result:=true
  end
end;

procedure TDrawCurve.draw_Poly(lp: PLLine);
begin
  if Assigned(fDrawOp) then
  Inc(fDrawCount,fDrawOp(@lp.Pol,lp.N))
end;

procedure TDrawCurve.dst_Poly(lp: PLLine);
begin
  with lp^ do
  if N > 0 then begin
    if fLock and (N > 1) then
    if not Points_Equal(Pol[0],Pol[N]) then
    begin Inc(N); Pol[N]:=Pol[0] end;
    draw_Poly(lp)
  end
end;

procedure TDrawCurve.dst_Point(x,y: double);
var
  p: TPoint; lp_max: Integer;
begin
  lp_max:=fbufMax;
  if fLock then Dec(lp_max);

  with fbuf^ do begin

    if N = lp_max then begin
      p:=Pol[N]; dst_Poly(fbuf);
      N:=0; Pol[0]:=p;
    end;

    p.x:=Round(x); p.y:=Round(y);

    if N < 0 then begin
      N:=0; Pol[0]:=p;
    end else
    if not Points_Equal(Pol[N],p) then
    begin Inc(N); Pol[N]:=p end
  end
end;

procedure TDrawCurve.plot_curve(value: TValueXY);

procedure plot_dt(value: TValueXY;
                  sp: int; t1,t2: double;
                  lp: PGPoly);
var
  t,e,r: double; p: tgauss4;
begin
  t:=(t1+t2)/2;

  p[0]:=lp[0]; p[2]:=lp[1];

  r:=Hypot(p[2].x-p[0].x,p[2].y-p[0].y);
  if r < fStep then
    dst_Point(p[2].x,p[2].y)
  else begin
    p[1]:=value(t);

    e:=xDist_to_Line(p[1].x,p[1].y,
                     p[0].x,p[0].y,
                     p[2].x,p[2].y);

    if (sp > 16) or (Abs(e) < fEps) then
      dst_Point(p[2].x,p[2].y)
    else begin
      Plot_dt(value,sp+1, t1,t, @p[0]);
      Plot_dt(value,sp+1, t,t2, @p[1])
    end
  end
end;

var
  i: int; t1,t2: double; p: GVector;
begin
  if Assigned(fDrawOp)
  or not Assigned(fOnCurve) then begin
    t2:=0; p[1]:=value(t2);
    dst_Point(p[1].x,p[1].y);

    for i:=1 to 4 do begin
      t1:=t2; t2:=i/4;
      p[0]:=p[1]; p[1]:=Value(t2);
      plot_dt(value,0, t1,t2, @p)
    end
  end
end;

function TDrawCurve.Draw_lcurve(lp: PLPoly; n: int): int;
var
  c: TCurve; i: int;
begin
  c:=TCurve.Create;
  try
    if beginDraw then
    if n >= 3 then begin

      i:=0; Dec(n,3);
      while i <= n do begin
        c.lBezier(@lp[i]); Inc(i,2);
        plot_curve(c.Value);
      end;

      dst_Poly(fbuf)
    end;
  finally
    c.Free
  end;

  Result:=fDrawCount
end;

function TDrawCurve.Draw_pcurve(lp: PLPoly; n,lock: int): int;

function ctrl(p: PGPoly): TGauss;
begin
  Result.x:=p[1].x+(p[2].x-p[0].x)/3;
  Result.y:=p[1].y+(p[2].y-p[0].y)/3;
end;

var
  c: TCurve; i: int; g,p: GOrient; b,b1: TGauss;
begin
  c:=TCurve.Create;
  try
    if beginDraw then
    if n > 0 then begin

      if lock < 0 then begin
        lock:=0; if n > 2 then
        if lp[0].X = lp[n].X then
        if lp[0].Y = lp[n].Y then
        lock:=1
      end;

      if n > 0 then begin

        p[0].x:=lp[n-1].X;
        p[0].y:=lp[n-1].Y;
        p[1].x:=lp[0].X;
        p[1].y:=lp[0].Y;
        p[2].x:=lp[1].X;
        p[2].y:=lp[1].Y;

        g[2]:=p[1];
        if lock <> 0 then
          b:=ctrl(@p)
        else
          b:=g[2];

        b1:=b; // сохранить 1 ctrl точку

        for i:=1 to n do begin
          g[0]:=g[2]; g[1]:=b;
          g[2]:=p[2];

          if i < n then begin
            p[0]:=p[1]; p[1]:=p[2];
            p[2].x:=lp[i+1].X;
            p[2].y:=lp[i+1].Y;
            b:=ctrl(@p)
          end else
          if lock <> 0 then
            b:=b1
          else
            b:=g[2];

          g[3].x:=g[2].x*2-b.x;
          g[3].y:=g[2].y*2-b.y;

          if (i = 1) and (lock = 0) and (i < n) then
            g[1]:=g[3]
          else
          if (i = n) and (lock = 0) then
            g[3]:=g[1];

          c.gBezier(@g);
          plot_curve(c.Value);
        end
      end;

      draw_Poly(fbuf)
    end;
  finally
    c.Free
  end;

  Result:=fDrawCount
end;

function TDrawCurve.Draw_gcurve(xy: PGPoly; Count: int): int;
var
  c: TCurve; i,n: int; pp: GOrient;
begin
  c:=TCurve.Create;
  try
    if beginDraw then
    if Count > 1 then begin
      n:=Count-4;

      i:=0; while i <= n do begin

        pp[0]:=xy[0]; pp[1]:=xy[1];
        pp[2]:=xy[2]; pp[3]:=xy[3];
        xy:=@xy[2]; Inc(i,2);

        pp[3].x:=pp[2].x+pp[2].x-pp[3].x;
        pp[3].y:=pp[2].y+pp[2].y-pp[3].y;

        c.gBezier(@pp);
        plot_curve(c.Value);
      end;

      dst_Poly(fbuf)
    end;
  finally
    c.Free
  end;

  Result:=fDrawCount
end;

function TDrawCurve.Draw_Ellipse(lp: PLPoly; n: int): int;

type
  tellipse = record
    x0,y0, a,b,c,s: Double
  end;

function x_prj(f: Double; const e: tellipse): TPoint;
var
  sin,cos: Extended; x,y: Double;
begin
  SinCos(f, sin,cos);

  with e do begin
    x:=a * cos; y:=b * sin;
    Result.X:=Round( x0 + c*x + s*y );
    Result.Y:=Round( y0 - (-s*x + c*y) )
  end
end;

procedure plot(const a,b: TPoint;
               const e: tellipse;
               f1,f2: double; sp: int);
var
  f: double; p: TPoint;
begin
  if sp <= 32 then begin

    f:=(f1+f2)/2; p:=x_prj(f,e);

    if Abs(Dist_to_Line(p, a,b)) > fEps then begin
      plot(a,p, e,f1,f, sp+1);
      dst_Point(p.X,p.Y);
      plot(p,b, e,f,f2, sp+1)
    end
  end
end;

var
  i: int; f1,f2: double;
  a,b,t: TPoint; e: tellipse;
begin
  if beginDraw then
  if n > 0 then begin

    Fillchar(e,Sizeof(e),0);

    a:=lp[0]; b:=lp[1];
    e.a:=Long_Dist(a,b);

    if e.a > 0 then begin
      e.b:=e.a; if n = 2 then
      e.b:=Abs(Dist_to_Line(lp[2], a,b));

      if e.b = 0 then begin
        Mirror_Point(b,a,t);
        dst_Point(t.X,t.Y);
        dst_Point(b.X,b.Y);
        draw_Poly(fbuf)
      end
      else begin
        e.x0:=a.X; e.y0:=a.Y;
        e.c:=(b.X-a.X)/e.a;
        e.s:=(b.Y-a.Y)/e.a;

        f2:=0; b:=x_prj(f2,e); t:=b;
        for i:=1 to 4 do begin
          f1:=f2; a:=b;
          
          f2:=f2+Pi/2;
          b:=x_prj(f2,e);

          dst_Point(a.X,a.Y);
          plot(a,b, e,f1,f2, 0)
        end;

        dst_Point(t.X,t.Y);
        draw_Poly(fbuf)
      end
    end
    else begin
      fbuf.N:=0; fbuf.Pol[0]:=a;
      draw_Poly(fbuf)
    end
  end;

  Result:=fDrawCount
end;

function TDrawCurve.Draw_Sector(lp: PLPoly): Integer;

procedure plot(const a,b: TPoint; f1,f2: double; sp: int);
var
  p: TPoint; f: double; _c,_s: Extended;
begin
  if sp <= 32 then begin
    f:=(f1+f2)/2; SinCos(f,_s,_c);
    p.X:=Round(fCx + fRad * _c);
    p.Y:=Round(fCy + fRad * _s);

    if Abs(Dist_to_Line(p, a,b)) > fEps then begin
      plot(a,p, f1,f,sp+1);
      dst_Point(p.X,p.Y);
      plot(p,b, f,f2,sp+1)
    end
  end
end;

var
  a,b,c,r, f,f1,f2,df, cx,cy: double;
  p1,p2,p3: TPoint; _c,_s: Extended;
begin
  if beginDraw then begin

    p1:=lp[0]; p2:=lp[1]; p3:=lp[2];
    c:=Long_Dist(p1,p2)/2;

    if c >= 1 then begin

      a:=Dist_to_Line(p3, p1,p2);

      if Abs(a) >= 1 then begin

        f:=iArcTan(p1.X,p1.Y,p2.X,p2.Y)+Pi/2;
        b:=(Sqr(c)-Sqr(a))/(2*a);

        SinCos(f, _s,_c);
        cx:=(p1.X/2+p2.X/2) + b * _c;
        cy:=(p1.Y/2+p2.Y/2) + b * _s;

        f1:=xArcTan(cx,cy,p1.X,p1.Y);
        f2:=xArcTan(cx,cy,p2.X,p2.Y);

        while f1 < 0 do f1:=f1+2*Pi;
        while f2 < 0 do f2:=f2+2*Pi;

        if Abs(f1-f2) > Pi then
        if f2 > f1 then f2:=f2-2*Pi
                   else f2:=f2+2*Pi;

        r:=Abs(a+b); df:=f2-f1; 

        if (a < 0) <> (b < 0) then
        if df < 0 then df:=df+2*Pi
                  else df:=df-2*Pi;

        dst_Point(p1.X,p1.Y);

        fCx:=cx; fCy:=cy; fRad:=r;
        plot(p1,p2, f1,f1+df,0);

        dst_Point(p2.X,p2.Y);
        draw_Poly(fbuf)
      end
    end
  end;

  Result:=fDrawCount
end;

type
  tbspline = class
    procedure start(v,dv: double); virtual;
    procedure next(v,dv: double); virtual;

    procedure Get_abcd(out c: TCurveCoeff); virtual;
    procedure Backup(out x0,x1,x2,x3: double);

    function value(t: double): double; virtual;
    function dv1(t: double): double; virtual;
    function dv2(t: double): double; virtual;

  private
    fCount: int;
    pp: doubles4;
  public
    property Count: int read fCount;
  end;

  tbspline2d = class(TCurveXY)
    destructor Destroy; override;

    procedure start(const p,v: tgauss);
    procedure next(const p,v: tgauss);

    function Value(t: double): TGauss; override;

    procedure Backup(pp: PGPoly);

  protected
    fcx,fcy: tbspline;
    
    function GetCount: int;
  public
    property Count: int read GetCount;
  end;

// Интерполяция полиномом T=(1,t,t2,t3)
(*
  P[0]    (1, 0, 0) -- 1 точка
  P[1]    (1, 1, 1) -- 2 точка
  P'[0]   (0, 1, 0) -- 1-производная в 1 точке *)
  tbspline2a = class(tbspline)
    procedure start(v,dv: double); override;
    procedure next(v,dv: double); override;

    function value(t: double): double; override;
    function dv1(t: double): double; override;
    function dv2(t: double): double; override;
  end;

(*
  P[0]    (1, 0, 0, 0) -- 1 точка
  P[1]    (1, 1, 1, 1) -- 2 точка
  P'[0]   (0, 1, 0, 0) -- 1 производная в 1 точке
  P"[0]   (0, 0, 2, 0) -- 2 производная в 1 точке *)
  tbspline2b = class(tbspline2a)
    procedure next(v,dv: double); override;

    function value(t: double): double; override;
    function dv1(t: double): double; override;
    function dv2(t: double): double; override;
  end;

(*
  P[0]  (1, 0, 0)   (2, 0, 0)
  P[1]  (1, 1, 1)   (-3, 4, -1)
  P[2]  (1, 2, 4)   (1, -2, 1) *)
  tbspline3 = class(tbspline)
    function value(t: double): double; override;
    function dv1(t: double): double; override;
    function dv2(t: double): double; override;
  end;

(*
  P[0]    (1, 0, 0, 0)    (1,     0,  0,     0)
  P[1]    (1, 1, 1, 1)    (0,     0,  0,     1)
  P[2]    (1, 2, 4, 8)    (-1.75, 2, -0.25, -1.5)
  P'[0]   (0, 1, 0, 0)    (0.75, -1,  0.25,  0.5) *)
  tbspline3a = class(tbspline)
    procedure start(v,dv: double); override;
    procedure next(v,dv: double); override;

    function value(t: double): double; override;
    function dv1(t: double): double; override;
    function dv2(t: double): double; override;
  end;

(*  ребро внутри
  P[0]    (1, 0, 0, 0)    (1,   0,  0,  0)
  P[1]    (1, 1, 1, 1)    (0,   0,  1,  0)
  P'[0]   (0, 1, 0, 0)    (-3,  3, -2, -1)
  P'[1]   (0, 1, 2, 3)    (2,  -2,  1,  1) *)
  tbspline3b = class(tbspline)
    procedure init(v1,dv1,v2,dv2: double);

    procedure Get_abcd(out c: TCurveCoeff); override;
    function value(t: double): double; override;
    function dv1(t: double): double; override;
    function dv2(t: double): double; override;
  end;

(* первое ребро
  P[0]    (1, 0, 0, 0)    (1,      0,     0,    0)
  P[1]    (1, 1, 1, 1)    (-1.5, 1.5, -0.25, -0.5)
  P"[0]   (0, 0, 2, 0)    (0,      0,   0.5,    0)
  P'[1]   (0, 1, 2, 3)    (0.5, -0.5, -0.25,  0.5) *)
  tbspline3b1 = class(tbspline3b)
    procedure Get_abcd(out c: TCurveCoeff); override;
    function value(t: double): double; override;
    function dv1(t: double): double; override;
    function dv2(t: double): double; override;
  end;

(* последнее ребро
  P[0]    (1, 0, 0, 0)    (   1,    0,    0,     0)
  P[1]    (1, 1, 1, 1)    (   0,    0,    1,     0),
  P'[0]   (0, 1, 0, 0)    (-1.5,  1.5, -1.5, -0.25),
  P"[1]   (0, 0, 2, 6)    ( 0.5, -0.5,  0.5,  0.25) *)
  tbspline3b2 = class(tbspline3b)
    procedure Get_abcd(out c: TCurveCoeff); override;
    function value(t: double): double; override;
    function dv1(t: double): double; override;
    function dv2(t: double): double; override;
  end;

  tbsplineP2a = class(tbspline2d)
    constructor Create;
  end;

  tbsplineP2b = class(tbspline2d)
    constructor Create;
  end;

  tbsplineP3a = class(tbspline2d)
    constructor Create;
    function Value(t: double): TGauss; override;

  private
    ft0: double;
  public
    property t0: double write ft0;
  end;

  tbsplineP3b = class(tbspline2d)
    constructor Create;
    procedure Init(p: PGPoly);
  end;

  tbsplineP3b1 = class(tbsplineP3b)
    constructor Create;
  end;

  tbsplineP3b2 = class(tbsplineP3b)
    constructor Create;
  end;

procedure tbspline.start(v,dv: double);
begin
end;

procedure tbspline.next(v,dv: double);
begin
end;

function tbspline.value(t: double): double;
begin
  Result:=0
end;

function tbspline.dv1(t: double): double;
begin
  Result:=0
end;

function tbspline.dv2(t: double): double;
begin
  Result:=0
end;

procedure tbspline.Get_abcd(out c: TCurveCoeff);
begin
  c.a:=0; c.b:=0; c.c:=0; c.d:=0
end;

procedure tbspline.Backup(out x0,x1,x2,x3: double);
var
  c: TCurveCoeff;
begin
  Get_abcd(c);
  Backup_c(c, x0,x1,x2,x3);
end;

destructor tbspline2d.Destroy;
begin
  fcx.Free; fcy.Free;
  inherited
end;

function tbspline2d.GetCount: int;
begin
  Result:=fcx.Count
end;

procedure tbspline2d.start(const p,v: tgauss);
begin
  fcx.start(p.x,v.x);
  fcy.start(p.y,v.y);
end;

procedure tbspline2d.next(const p,v: tgauss);
begin
  fcx.next(p.x,v.x);
  fcy.next(p.y,v.y);
end;

function tbspline2d.Value(t: double): TGauss;
begin
  Result.x:=fcx.value(t);
  Result.y:=fcy.value(t)
end;

procedure tbspline2d.Backup(pp: PGPoly);
begin
  fcx.Backup(pp[0].x,pp[1].x,pp[2].x,pp[3].x);
  fcy.Backup(pp[0].y,pp[1].y,pp[2].y,pp[3].y);
end;

procedure tbspline2a.start(v,dv: double);
begin
  pp[0]:=v;
  pp[1]:=v;
  pp[2]:=0;
  pp[3]:=0;
  fCount:=1
end;

procedure tbspline2a.next(v,dv: double);
begin
  if fCount = 1 then
    pp[2]:=v-pp[0]
  else
    pp[2]:=dv1(1);

  pp[0]:=pp[1];
  pp[1]:=v; Inc(fCount)
end;

function tbspline2a.value(t: double): double;
var
  t2: double;
begin
  t2:=t*t;
  Result:=pp[0] * (1-t2) +
          pp[1] * (t2) +
          pp[2] * (t-t2)
end;

function tbspline2a.dv1(t: double): double;
begin
  Result:=pp[0] * (-2*t) +
          pp[1] * (2*t) +
          pp[2] * (1-2*t)
end;

function tbspline2a.dv2(t: double): double;
begin
  Result:=pp[0] * (-2) +
          pp[1] * (2) +
          pp[2] * (-2)
end;

procedure tbspline2b.next(v,dv: double);
var
  _dv1,_dv2: double;
begin
  if fCount = 1 then
    pp[2]:=v-pp[0]
  else begin
    _dv1:=dv1(1); _dv2:=dv2(1);
  end;

  pp[0]:=pp[1]; pp[1]:=v;
  pp[2]:=_dv1; pp[3]:=_dv2;

  Inc(fCount)
end;

function tbspline2b.value(t: double): double;
var
  t2,t3: double;
begin
  t2:=t*t; t3:=t2*t;
  Result:=pp[0] * (1-t3) +
          pp[1] * (t3) +
          pp[2] * (t-t3) +
          pp[3] * (t2-t3)/2;
end;

function tbspline2b.dv1(t: double): double;
var
  t2: double;
begin
  t2:=t*t;
  Result:=pp[0] * (-3*t2) +
          pp[1] * (3*t2) +
          pp[2] * (1-3*t2) +
          pp[3] * (t-t2*3/2);
end;

function tbspline2b.dv2(t: double): double;
begin
  Result:=pp[0] * (-6*t) +
          pp[1] * (6*t) +
          pp[2] * (-6*t) +
          pp[3] * (1-3*t);
end;

function tbspline3.value(t: double): double;
var
  t2: double;
begin
  t2:=t*t;
  Result:=pp[0] * (2-3*t+t2) +
          pp[1] * (4*t-2*t2) +
          pp[2] * (-t+t2)
end;

function tbspline3.dv1(t: double): double;
begin
  Result:=pp[0] * (-3+2*t) +
          pp[1] * (4-4*t) +
          pp[2] * (-1+2*t)
end;

function tbspline3.dv2(t: double): double;
begin
  Result:=pp[0] * (2) +
          pp[1] * (-4) +
          pp[2] * (2)
end;

procedure tbspline3a.start(v,dv: double);
begin
  pp[0]:=v;
  pp[1]:=v;
  pp[2]:=v;
  pp[3]:=0;
  fCount:=1
end;

procedure tbspline3a.next(v,dv: double);
begin
  if fCount = 1 then begin
    pp[1]:=v; pp[3]:=v-pp[0]
  end
  else begin

    if fCount > 2 then begin
      pp[3]:=dv1(1);
      pp[0]:=pp[1]; pp[1]:=pp[2];
    end;

    pp[2]:=v
  end;

  Inc(fCount)
end;

function tbspline3a.value(t: double): double;
var
  t2,t3: double;
begin
  t2:=t*t; t3:=t2*t;
  Result:=pp[0] * (1-1.75*t2+0.75*t3) +
          pp[1] * (2*t2-t3) +
          pp[2] * (-0.25*t2+0.25*t3) +
          pp[3] * (t-1.5*t2+0.5*t3);
end;

function tbspline3a.dv1(t: double): double;
var
  t2: double;
begin
  t2:=t*t;
  Result:=pp[0] * (-3.5*t+2.25*t2) +
          pp[1] * (4*t-3*t2) +
          pp[2] * (-0.5*t+0.75*t2) +
          pp[3] * (1-3*t+1.5*t2);
end;

function tbspline3a.dv2(t: double): double;
begin
  Result:=pp[0] * (-3+4.5*t) +
          pp[1] * (4-6*t) +
          pp[2] * (-5+1.5*t) +
          pp[3] * (-3+3*t);
end;

procedure tbspline3b.init(v1,dv1,v2,dv2: double);
begin
  pp[0]:=v1;
  pp[1]:=v2;
  pp[2]:=dv1;
  pp[3]:=dv2;
end;

procedure tbspline3b.Get_abcd(out c: TCurveCoeff);
begin
  c.a:= 2*pp[0] -2*pp[1] +1*pp[2] +1*pp[3];
  c.b:=-3*pp[0] +3*pp[1] -2*pp[2] -1*pp[3];
  c.c:=pp[2]; c.d:=pp[0]
end;

function tbspline3b.value(t: double): double;
var
  t2,t3: double;
begin
  t2:=t*t; t3:=t2*t;
  Result:=pp[0] * (1-3*t2+2*t3) +
          pp[1] * (3*t2-2*t3) +
          pp[2] * (t-2*t2+t3) +
          pp[3] * (-t2+t3);
end;

function tbspline3b.dv1(t: double): double;
var
  t2: double;
begin
  t2:=t*t;
  Result:=pp[0] * (-6*t+6*t2) +
          pp[1] * (6*t-6*t2) +
          pp[2] * (1-4*t+3*t2) +
          pp[3] * (-2*t+3*t2)
end;

function tbspline3b.dv2(t: double): double;
begin
  Result:=pp[0] * (-6+12*t) +
          pp[1] * (6-12*t) +
          pp[2] * (-4+6*t) +
          pp[3] * (-2+6*t)
end;

procedure tbspline3b1.Get_abcd(out c: TCurveCoeff);
begin
  c.a:=0.5*pp[0]-0.5*pp[1]-0.25*pp[2]+0.5*pp[3];
  c.b:=0.5*pp[2];
  c.c:=-1.5*pp[0]+1.5*pp[1]-0.25*pp[2]-0.5*pp[3];
  c.d:=pp[0]
end;

function tbspline3b1.value(t: double): double;
var
  t2,t3: double;
begin
  t2:=t*t; t3:=t2*t;
  Result:=pp[0] * (1-1.5*t+0.5*t3) +
          pp[1] * (+1.5*t-0.5*t3) +
          pp[2] * (-0.25*t+0.5*t2-0.25*t3) +
          pp[3] * (-0.5*t+0.5*t3);
end;

function tbspline3b1.dv1(t: double): double;
var
  t2: double;
begin
  t2:=t*t;
  Result:=pp[0] * (-1.5+1.5*t2) +
          pp[1] * (+1.5-1.5*t2) +
          pp[2] * (-0.25+t-0.75*t2) +
          pp[3] * (-0.5+1.5*t2);
end;

function tbspline3b1.dv2(t: double): double;
begin
  Result:=pp[0] * (3*t) +
          pp[1] * (-3*t) +
          pp[2] * (1-1.5*t) +
          pp[3] * (3*t)
end;

procedure tbspline3b2.Get_abcd(out c: TCurveCoeff);
begin
  c.a:=0.5*pp[0] -0.5*pp[1] +0.5*pp[2] +0.25*pp[3];
  c.b:=-1.5*pp[0] +1.5*pp[1] -1.5*pp[2] -0.25*pp[3];
  c.c:=pp[2]; c.d:=pp[0]
end;

function tbspline3b2.value(t: double): double;
var
  t2,t3: double;
begin
  t2:=t*t; t3:=t2*t;
  Result:=pp[0] * (1-1.5*t2+0.5*t3) +
          pp[1] * (1.5*t2-0.5*t3) +
          pp[2] * (t-1.5*t2+0.5*t3) +
          pp[3] * (-0.25*t2+0.25*t3);
end;

function tbspline3b2.dv1(t: double): double;
var
  t2: double;
begin
  t2:=t*t;
  Result:=pp[0] * (-3*t+1.5*t2) +
          pp[1] * (3*t-1.5*t2) +
          pp[2] * (1-3*t+1.5*t2) +
          pp[3] * (-0.5*t+0.75*t2);
end;

function tbspline3b2.dv2(t: double): double;
begin
  Result:=pp[0] * (-3+3*t) +
          pp[1] * (3-3*t) +
          pp[2] * (-3+3*t) +
          pp[3] * (-0.5+1.5*t)
end;

constructor tbsplineP2a.Create;
begin
  inherited Create;
  fcx:=tbspline2a.Create;
  fcy:=tbspline2a.Create;
end;

constructor tbsplineP2b.Create;
begin
  inherited Create;
  fcx:=tbspline2b.Create;
  fcy:=tbspline2b.Create;
end;

constructor tbsplineP3a.Create;
begin
  inherited Create;
  fcx:=tbspline3a.Create;
  fcy:=tbspline3a.Create;
end;

function tbsplineP3a.Value(t: double): TGauss;
begin
  Result.x:=fcx.value(ft0+t);
  Result.y:=fcy.value(ft0+t);
end;

constructor tbsplineP3b.Create;
begin
  inherited Create;
  fcx:=tbspline3b.Create;
  fcy:=tbspline3b.Create;
end;

procedure tbsplineP3b.Init(p: PGPoly);
begin
  (fcx as tbspline3b).init(p[0].x,p[1].x,p[2].x,p[3].x);
  (fcy as tbspline3b).init(p[0].y,p[1].y,p[2].y,p[3].y);
end;

constructor tbsplineP3b1.Create;
begin
  inherited Create;
  fcx:=tbspline3b1.Create;
  fcy:=tbspline3b1.Create;
end;

constructor tbsplineP3b2.Create;
begin
  inherited Create;
  fcx:=tbspline3b2.Create;
  fcy:=tbspline3b2.Create;
end;

function Get_bspline3b(v1,dv1,v2,dv2: double): TCurveCoeff;
var
  b: tbspline3b;
begin
  b:=tbspline3b.Create;
  try
    b.init(v1,dv1,v2,dv2);
    b.Get_abcd(Result);
  finally
    b.Free
  end
end;

function TDrawCurve.Draw_bspline(lp: PLPoly; n,i1,i2,lock: int): int;

function dv3(p: PGPoly): TGauss;
begin
  Result.x:=(p[2].x-p[0].x)*fbsplineK;
  Result.y:=(p[2].y-p[0].y)*fbsplineK;
end;

procedure OutCurve(c: tbspline2d);
var
  p: GOrient;
begin
  c.Backup(@p); fOnCurve(@p,4);
end;

var
  c: tbsplineP3b;
  c1: tbsplineP3b1;
  c2: tbsplineP3b2;
  i: int; g,p,_p: GOrient;
begin
  if beginDraw then
  if n > 0 then begin

    if lock < 0 then begin
      lock:=0; if n > 2 then
      if lp[0].X = lp[n].X then
      if lp[0].Y = lp[n].Y then
      lock:=1
    end;

    c:=tbsplineP3b.Create;
    try
      if n > 0 then begin

        if Assigned(fOnCurve) then
        fOnCurve(nil,0);

        p[0].x:=lp[n-1].X;
        p[0].y:=lp[n-1].Y;
        p[1].x:=lp[0].X;
        p[1].y:=lp[0].Y;
        p[2].x:=lp[1].X;
        p[2].y:=lp[1].Y;

        g[2]:=p[1];
        if lock <> 0 then
          g[3]:=dv3(@p)
        else begin
          g[3].x:=0;
          g[3].y:=0;
        end;

        g[4]:=g[3]; // сохранить d' в 1 точке

        for i:=1 to n do begin
          g[0]:=g[2]; g[1]:=g[3];
          g[2]:=p[2];

          if i < n then begin
            p[0]:=p[1]; p[1]:=p[2];
            p[2].x:=lp[i+1].X;
            p[2].y:=lp[i+1].Y;
            g[3]:=dv3(@p)
          end else
          if lock <> 0 then
            g[3]:=g[4]
          else begin
            g[3].x:=0;
            g[3].y:=0;
          end;

          if (i > i1) and (i <= i2) then

          if (i = 1) and (lock = 0) then begin
            c1:=tbsplineP3b1.Create;
            try
              c1.Init(@g);
              plot_curve(c1.Value);

              if Assigned(fOnCurve) then
              OutCurve(c1);

            finally
              c1.Free
            end
          end else

          if (i = n) and (lock = 0) then begin
            c2:=tbsplineP3b2.Create;
            try
              c2.Init(@g);
              plot_curve(c2.Value);

              if Assigned(fOnCurve) then
              OutCurve(c2);

            finally
              c2.Free
            end
          end
          else begin
            c.Init(@g);
            plot_curve(c.Value);

            if Assigned(fOnCurve) then
            OutCurve(c);
          end
        end
      end
    finally
      c.Free
    end;

    draw_Poly(fbuf)
  end;

  Result:=fDrawCount
end;

function TDrawCurve.Draw_bsplinev(lp: PLPoly; vp: PGPoly; n: int): int;
var
  c: tbsplineP3b;
  i: int; g: GOrient;
begin
  if beginDraw then
  if n > 0 then begin

    c:=tbsplineP3b.Create;
    try
      if Assigned(fOnCurve) then
      fOnCurve(nil,0);

      g[2].x:=lp[0].X;
      g[2].y:=lp[0].Y;
      g[3]:=vp[0];

      for i:=1 to n do begin
        g[0]:=g[2]; g[1]:=g[3];

        g[2].x:=lp[i].X;
        g[2].y:=lp[i].Y;
        g[3]:=vp[i];

        c.Init(@g);
        plot_curve(c.Value);
      end
    finally
      c.Free
    end;

    draw_Poly(fbuf)
  end;

  Result:=fDrawCount
end;

function TDrawCurve.Draw_bspline1(lp: PLPoly; n,rank: int): int;
var
  c: tbspline2d; i: int; a,b,v: tgauss;
begin
  if beginDraw then
  if n > 0 then begin

    if rank = 3 then
      c:=tbsplineP2b.Create
    else
      c:=tbsplineP2a.Create;
    try
      a.x:=lp[0].X;
      a.y:=lp[0].Y;
      v.x:=0; v.y:=0;

      c.start(a,v);
      for i:=1 to n do begin
        b.x:=lp[i].X;
        b.y:=lp[i].Y;

        if Abs(b.x-a.x)+Abs(b.y-a.y) > 0.1 then begin
          a:=b; c.next(b,v);
          plot_curve(c.Value);
        end
      end;
    finally
      c.Free
    end;

    draw_Poly(fbuf)
  end;

  Result:=fDrawCount
end;

function TDrawCurve.Draw_bspline2(lp: PLPoly; n: int): int;
var
  c: tbsplineP3a; i: int; a,b,v: tgauss;
begin
  if beginDraw then
  if n > 0 then begin

    c:=tbsplineP3a.Create;
    try
      a.x:=lp[0].X;
      a.y:=lp[0].Y;
      v.x:=0; v.y:=0;

      c.start(a,v);
      for i:=1 to n do begin
        b.x:=lp[i].X;
        b.y:=lp[i].Y;

        if Abs(b.x-a.x)+Abs(b.y-a.y) > 0.1 then begin
          a:=b; c.next(b,v);
          if c.Count >= 3 then
          plot_curve(c.Value);
        end
      end;

      if c.Count = 2 then
        plot_curve(c.Value)
      else
      if c.Count > 2 then begin
        c.t0:=1; plot_curve(c.Value); c.t0:=0
      end

    finally
      c.Free
    end;

    draw_Poly(fbuf)
  end;

  Result:=fDrawCount
end;

procedure lplot_t(lp: PLPoly; t: double; vp: PLPoly);
var
  c: TCurve; a,b: TGauss;
begin
  vp[0]:=lp[0];
  vp[1]:=lp[1];

  c:=TCurve.Create;
  try
    c.lBezier(lp);

    a:=c.Value(t);
    b.x:=ctrl_c(c.cx,t);
    b.y:=ctrl_c(c.cy,t);

    vp[0].X:=Round(a.x);
    vp[0].Y:=Round(a.y);

    vp[1].X:=Round(b.x);
    vp[1].Y:=Round(b.y);
  finally
    c.Free
  end
end;

procedure lplot_c(lp: PLPoly; t: double; cp: PLPoly);
var
  cx,cy: TCurveCoeff;
  p0,p1,p2,p3: TPoint;
begin
  p0:=lp[0]; p1:=lp[1]; p2:=lp[2]; p3:=lp[3];
  p3.X:=p2.X+p2.X-p3.X; p3.Y:=p2.Y+p2.Y-p3.Y;

  cx:=Init_c(p0.X,p1.X,p3.X,p2.X);
  cy:=Init_c(p0.Y,p1.Y,p3.Y,p2.Y);

  cp[0]:=p0;
  cp[1].X:=Round(ctrl_c(cx,0));
  cp[1].Y:=Round(ctrl_c(cy,0));

  cp[2].X:=Round(Plot_c(cx,t));
  cp[2].Y:=Round(Plot_c(cy,t));
  cp[3].X:=Round(ctrl_c(cx,t));
  cp[3].Y:=Round(ctrl_c(cy,t));

  cx:=Init_c(p2.X,p3.X,p1.X,p0.X);
  cy:=Init_c(p2.Y,p3.Y,p1.Y,p0.Y);

  cp[4]:=p2; t:=1-t;
  cp[5].X:=p2.X+p2.X-Round(ctrl_c(cx,t));
  cp[5].Y:=p2.Y+p2.Y-Round(ctrl_c(cy,t));
end;

constructor TCurveToPoly.Create(Adest: PLLine;
                                AInd,AMax: int;
                                AStep,AEps: double);
begin
  inherited Create(lp_Dump,0,false);
  fdest:=Adest; fInd:=AInd; fMax:=AMax;
  fStep:=AStep; fEps:=Max(1,fStep/3);
  if AEps > 0 then fEps:=AEps
end;

function TCurveToPoly.lp_Dump(lp: PLPoly; n: Integer): Integer;
var
  i: int;
begin
  Result:=0;

  if fdest.N+n+1 > fMax then
    fErr:=1
  else begin

    if fdest.N < 0 then begin
      Load_Poly(fdest,lp,n+1);
      fInd:=fdest.N
    end
    else begin
      for i:=0 to n do
      fInd:=xPoly_Insert(fdest,fInd,fMax,lp[i]);
    end;

    Result:=fdest.N+1
  end
end;

constructor TCurveToPointList.Create(Adest: TPointList;
                                     AStep,AEps: double);
begin
  inherited Create(lp_Dump,0,false);
  fdest:=Adest; fStep:=AStep;
  fEps:=Max(1,fStep/3);
  if AEps > 0 then fEps:=AEps
end;

function TCurveToPointList.lp_Dump(lp: PLPoly; n: Integer): Integer;
begin
  fdest.Add_poly(lp,n+1);
  Result:=n+1
end;

procedure TCurveToPolyline.OutCurve;

function Plot_t(t: Double): txyz;
begin
  Result.x:=Plot_c(cx,t);
  Result.y:=Plot_c(cy,t);
  Result.z:=Plot_c(cz,t);
end;

procedure sector(const v1,v2: txyz;
                 t1,t2: Double; sp: Integer);
var
  v: txyz; e,t: Double; 
begin
  t:=t1/2 + t2/2; v:=Plot_t(t);

  e:=xDist_to_Line(v.x,v.z, v1.x,v1.z,v2.x,v2.z);

  if sp < 32 then
  if Abs(e) > feps then begin
    sector(v1,v,t1,t,sp+1); Add(@v);
    sector(v,v2,t,t2,sp+1);
  end
end;

var
  i: Integer; t1,t2: Double; v1,v2: txyz;
begin
  t2:=0; v2:=Plot_t(0); Add(@v2);

  for i:=1 to 4 do begin
    t1:=t2; v1:=v2;
    t2:=i/4; v2:=Plot_t(t2);
    sector(v1,v2, t1,t2, 0);
    Add(@v2)
  end;
end;

function TCurveToPolyline.Load(Curve: TXyzList; eps: Double): Integer;
var
  i: Integer; lp: pxyz_Array;
  pp: array[0..3] of txyz;
begin
  Clear; feps:=eps;

  i:=0; lp:=Curve.First;
  while i <= Curve.Count-4 do begin

    Move(lp[0],pp,Sizeof(txyz)*4);

    with pp[3] do begin
      x:=pp[2].x + pp[2].x - x;
      y:=pp[2].y + pp[2].y - y;
      z:=pp[2].z + pp[2].z - z;
    end;

    cx:=Init_c(pp[0].x,pp[1].x,pp[3].x,pp[2].x);
    cy:=Init_c(pp[0].y,pp[1].y,pp[3].y,pp[2].y);
    cz:=Init_c(pp[0].z,pp[1].z,pp[3].z,pp[2].z);

    OutCurve; Inc(i,2); lp:=@lp[2]
  end;

  Result:=Count
end;

function TCurveToPolyline.xLoad(Curve: TXyzList; eps: Double;
                                MaxCount: Integer): Integer;
begin
  Clear;

  while true do begin
    Result:=Load(Curve,eps);
    if Result = 0 then Break;
    if MaxCount = 0 then Break;
    if Result <= MaxCount then Break;
    eps:=eps * 1.5
  end;

  Result:=Count
end;

function TCurveToPolyline.xLine(const v0,v3: txyz;
                                const z1,z2,eps: Double;
                                MaxCount: Integer): Integer;
begin
  feps:=eps;

  while true do begin Clear;

    cx:=Init_l(v0.x,v3.x); cy:=Init_l(v0.y,v3.y);
    cz:=Init_c(v0.z,z1,z2,v3.z); OutCurve;

    if Count = 0 then Break;
    if MaxCount = 0 then Break;
    if Count <= MaxCount then Break;
    feps:=feps * 1.5
  end;

  Result:=Count
end;

constructor TCurveToLCurve.Create;
begin
  inherited Create(nil,0,false);
  fbuf:=TPointList.Create(4096);
  fOnCurve:=CurveProc
end;

destructor TCurveToLCurve.Destroy;
begin
  fbuf.Free; inherited
end;

function TCurveToLCurve.CurveProc(gp: PGPoly; N: Integer): Integer;
var
  i: int; p: LOrient;
begin
  Result:=0;
  if gp = nil then
    fbuf.Clear
  else begin

    for i:=0 to 3 do begin
      p[i].X:=Round(gp[i].x);
      p[i].Y:=Round(gp[i].y);
    end;

    if fbuf.Count = 0 then begin
      fbuf.Add(@p[0]);
      fbuf.Add(@p[1]);
    end;

    p[2].X:=p[3].X+p[3].X-p[2].X;
    p[2].Y:=p[3].Y+p[3].Y-p[2].Y;

    fbuf.Add(@p[3]);
    fbuf.Add(@p[2])
  end
end;

function BsplineToLCurve(dst,src: PLLine; max: int): int;
var
  c: TCurveToLCurve;
begin
  Result:=-1;

  c:=TCurveToLCurve.Create;
  try
    with src^ do
    c.Draw_bspline(@Pol,N, 0,N,-1);

    if c.Buf.Count > 0 then
    if c.Buf.Count < max then
    Result:=Load_Poly(dst,c.Buf.First,c.Buf.Count)
  finally
    c.Free
  end
end;

function SectorToLPoly(lp: PLLine; ind,max: int;
                       const p3: TPoint; eps: Double): int;
var
  add: TCurveToPoly; i,di: int; l: LOrient;
begin
  Result:=ind;

  with lp^ do begin

    di:=1; if max < 0 then di:=-1;
    if ind < 0 then i:=1 else i:=ind-di;
    l[0]:=Pol[i]; l[1]:=Pol[i+di]; l[2]:=p3;
    if ind < 0 then i:=0;

    add:=TCurveToPoly.Create(lp,i,max,eps*3,eps);
    try
      begin
        add.Draw_sector(@l);
        Result:=add.Ind+di
      end;
    finally
      add.Free;
    end
  end
end;

end.