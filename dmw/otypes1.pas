unit otypes1; interface

uses
  Classes,Windows,otypes;
  
const
  tin_mag1 = $306E6974;

const
  znil = -999999;

  MaxTriCount = 256;

type
(*
          0
          *
         / \
       2/   \1
       /     \
     1*-------*2
          0
*)

  PTri2 = ^TTri2; TTri2 = record
    p1,p2,p3, t1,t2,t3: int
  end;

  PTriArray2 = ^TTriArray2;
  TTriArray2 = array[0..MaxTriCount-1] of TTri2;

type
  ttin_hdr = record
    mag,ver: Longint;

    vn,vp,np: Longint;
    tn,tp: Longint;
    bn,bp: Longint;

    stride: Longint;

    bw,bh: Longint;
    zmin,zmax: Longint;

    hash: TSize;
    hp: Longint;

    sys_on: Longint;
    sys: tsys;

    gb: TGauss4
  end;

  ttrg_geo = record
    sys_on: Integer; sys: tsys;
    xy,bl: TGauss4
  end;

  PTriWeight = ^TTriWeight;
  TTriWeight = record
    k: Array[0..2,0..2] of Double;
    s2: Double;
  end;

  PTriWeights = ^TTriWeights;
  TTriWeights = Array[0..255] of TTriWeight;

function Tri2(p1,p2,p3, t1,t2,t3: int): TTri2;

function iline(p, a,b: PPoint): int;
function itri(p, a,b,c: TPoint): int;

// [p1,p2] - diag1; [p3,p4] - diag2
function iBadPare(const p1,p2, p3,p4: TPoint): Boolean;

function TriWeighti(lp: PLPoly; out w: TTriWeight): Boolean;

function TriWeight(lp: PGPoly; out w: TTriWeight): Boolean;

function TriWeightd(x1,y1,x2,y2,x3,y3: Double; out w: TTriWeight): Boolean;

function TriValue(x,y: Double; Val: PDoubles;
                  const w: TTriWeight): Double;

function TriValued(x,y, v1,v2,v3: Double; const w: TTriWeight): Double;

implementation

function Tri2(p1,p2,p3, t1,t2,t3: int): TTri2;
var
  t: TTri2;
begin
  Fillchar(t,Sizeof(t),0);
  t.p1:=p1; t.p2:=p2; t.p3:=p3;
  t.t1:=t1; t.t2:=t2; t.t3:=t3;
  Result:=t
end;

function iline(p, a,b: PPoint): int;
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

function itri(p, a,b,c: TPoint): int;
var
  rc1,rc2,rc3: int;
begin
  Result:=0;

  rc1:=iline(@p, @a,@b);
  if Abs(rc1) <> 1 then begin
    if rc1 = 2 then Result:=1
  end else
  if rc1 < 0 then begin
    rc2:=iline(@p, @b,@c);
    if Abs(rc2) <> 1 then begin
      if rc2 = 2 then Result:=2
    end else
    if rc2 < 0 then begin
      rc3:=iline(@p, @c,@a);
      if Abs(rc3) <> 1 then begin
        if rc3 = 2 then Result:=3
      end else
      if rc3 < 0 then Result:=4
    end
  end
end;

// [p1,p2] - diag1; [p3,p4] - diag2
function iBadPare(const p1,p2, p3,p4: TPoint): Boolean;
var
  sa,sb: Double; v14,v24,v13,v23: TGauss;
Begin
  Result:=false;

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
    Result:=true;
end;

function TriWeighti(lp: PLPoly; out w: TTriWeight): Boolean;

function TriSquare(lp: PLPoly): Double;
var
  x0,y0,x1,y1,x2,y2: Double;
begin
  x0:=lp[0].X; y0:=lp[0].Y;
  x1:=lp[1].X; y1:=lp[1].Y;
  x2:=lp[2].X; y2:=lp[2].Y;

  Result:=(+x1*y2 -x2*y1
           -x0*y2 +x2*y0
           +x0*y1 -x1*y0) / 2
end;

procedure abc(k: PDoubles; const p2,p3: TPoint);
var
  x2,y2,x3,y3: Double;
begin
  x2:=p2.X; y2:=p2.Y;
  x3:=p3.X; y3:=p3.Y;

  k[0]:=x2*y3 - x3*y2;
  k[1]:=y2 - y3;
  k[2]:=x3 - x2;
end;

begin
  Result:=false;
  Fillchar(w,Sizeof(w),0);

  w.s2:=TriSquare(lp)*2;

  if Abs(w.s2) > 1E-3 then begin
    abc(@w.k[0],lp[1],lp[2]);
    abc(@w.k[1],lp[2],lp[0]);
    abc(@w.k[2],lp[0],lp[1]);
    Result:=true
  end
end;

function TriWeight(lp: PGPoly; out w: TTriWeight): Boolean;

function TriSquare(lp: PGPoly): Double;
begin
  Result:=lp[0].x*lp[1].y - lp[1].x*lp[0].y +
          lp[1].x*lp[2].y - lp[2].x*lp[1].y +
          lp[2].x*lp[0].y - lp[0].x*lp[2].y;
  Result:=Result / 2
end;

procedure abc(k: PDoubles; const p2,p3: TGauss);
begin
  k[0]:=p2.x*p3.y - p3.x*p2.y;
  k[1]:=p2.y - p3.y;
  k[2]:=p3.x - p2.x;
end;

begin
  Result:=false;
  Fillchar(w,Sizeof(w),0);

  w.s2:=TriSquare(lp)*2;
  if Abs(w.s2) > 1E-3 then begin
    abc(@w.k[0],lp[1],lp[2]);
    abc(@w.k[1],lp[2],lp[0]);
    abc(@w.k[2],lp[0],lp[1]);
    Result:=true
  end
end;

function TriWeightd(x1,y1,x2,y2,x3,y3: Double; out w: TTriWeight): Boolean;
var
  g: GOrient;
begin
  g[0]:=_Gauss(x1,y1);
  g[1]:=_Gauss(x2,y2);
  g[2]:=_Gauss(x3,y3);
  Result:=TriWeight(@g,w)
end;

function TriValue(x,y: Double; Val: PDoubles;
                  const w: TTriWeight): Double;
var
  mu: Array[0..2] of Double;
begin
  Result:=0;
  if Abs(w.s2) > 1E-6 then begin
    mu[0]:=w.k[0,0] + w.k[0,1]*x + w.k[0,2]*y;
    mu[1]:=w.k[1,0] + w.k[1,1]*x + w.k[1,2]*y;
    mu[2]:=w.k[2,0] + w.k[2,1]*x + w.k[2,2]*y;
    Result:=(mu[0]*Val[0] + mu[1]*Val[1] + mu[2]*Val[2]) / w.s2
  end
end;

function TriValued(x,y, v1,v2,v3: Double; const w: TTriWeight): Double;
var
  mu1,mu2,mu3: Double;
begin
  Result:=0;
  if Abs(w.s2) > 1E-6 then begin
    mu1:=w.k[0,0] + w.k[0,1]*x + w.k[0,2]*y;
    mu2:=w.k[1,0] + w.k[1,1]*x + w.k[1,2]*y;
    mu3:=w.k[2,0] + w.k[2,1]*x + w.k[2,2]*y;
    Result:=(mu1*v1 + mu2*v2 + mu3*v3) / w.s2
  end
end;

end.