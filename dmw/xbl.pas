unit xbl; interface

uses
  Math,otypes,xline;

type
  TEllipsoid = record
    A,A2,Alfa, B,F,Es,Es1,Esp,E: Extended
  end;

const
  Ellipsoids_Max = 23;

  Ellipsoids: array[0..Ellipsoids_Max] of TEllipsoid =

    ((A:6378245;     Alfa:298.3),
     (A:6378245;     Alfa:298.3),              // [KA] Krasovsky 1940
     (A:6378388;     Alfa:298.26),             // [WD] WGS 1972

     (A:6378200;     Alfa:298.3),              // [HE] Helmert 1906
     (A:6378270;     Alfa:297.0),              // [HO] Hough 1960
     (A:6378249.145; Alfa:293.465),            // [CD] Clark 1880
     (A:6378206.4;   Alfa:294.9786982),        // [CC] Clark 1866
     (A:6377397.155; Alfa:299.1528128),        // [BR] Bessel 1841
     (A:6377483.865; Alfa:299.1528128),        // [BN] Bessel-N 1841
     (A:6378137;     Alfa:298.257223563),      // WGS-1984

     (A:6377563.396; Alfa:299.3249646),        // [AA] Airy 1830
     (A:6377340.189; Alfa:299.3249646),        // [AM] Modified Airy
     (A:6377276.345; Alfa:300.8017),           // [EA] Everest 1830
     (A:6377309.613; Alfa:300.8017),           // [EE] Everest 1948
     (A:6377301.243; Alfa:300.8017),           // [EC] Everest 1956
     (A:6377298.556; Alfa:300.8017),           // [EB] Everest Brunei
     (A:6377309.613; Alfa:300.8017),           // [EF] Everest Pakistan

     (A:6378137;     Alfa:298.257222101),      // [RF] GRS 1980
     (A:6378160;     Alfa:298.247),            // [ID] Indonesian 1974
     (A:6378388;     Alfa:297),                // [IN] International 1924
     (A:6378155;     Alfa:298.3),              // [FA] Modified Fischer 1960
     (A:6378160;     Alfa:298.25),             // [SA] South American 1969
     (A:6378160;     Alfa:298.25),             // [AN] Australian
     (A:6378136;     Alfa:298.25784)           // ПЗ-90
    );

var
  Small: Extended;
  pi2,pi_2,pi_4,c_Rn,c_B2: Extended;
  c_elp: Integer; c_: TEllipsoid;

function Ellipsoids_Count: Integer; stdcall;

procedure Get_Ellipsoid(elp: Integer; out a,b: Double); stdcall;

function Set_Ellipsoid(elp: Integer): Integer; stdcall;
function Set_Radius(Rn: Double): Double; stdcall;
procedure Set_B2(B2: Double); stdcall;

function c_RO(Fi: double): Extended;

procedure pc_33_bl_to_xy(Lat,Lon, L0: Double; out X,Y: Double); stdcall;

procedure XY_to_BL(x,y, lc,b1,b2: double; elp,prj: byte; var b,l: double); stdcall;
procedure BL_to_XY(b,l, lc,b1,b2: double; elp,prj: byte; var x,y: double); stdcall;

procedure get_n0_n1(b0: Double; elp: Integer;
                    out n0,n1: Extended); stdcall;

procedure bl_to_xyz(b,l,h: Double; elp: Integer;
                    out x,y,z: Double); stdcall;

function xyz_to_bl(x,y,z: Double; elp: Integer;
                   out _b,_l,_h: Double): Boolean; stdcall;

procedure GEO_GEO(b1,l1,h1: double; elp1,elp2: Integer;
                  geo1,geo2: PDatum7; out b2,l2,h2: double); stdcall;

procedure GEO_GEO1(b1,l1,h1: double; elp1,elp2: Integer;
                   geo1,geo2: PDatum7; out b2,l2,h2: double); stdcall;

procedure GEO_WGS(b1,l1,h1: double; elp: Integer;
                  geo: PDatum7; out b2,l2,h2: double); stdcall;

procedure WGS_GEO(b1,l1,h1: double; elp: Integer;
                  geo: PDatum7; out b2,l2,h2: double); stdcall;

procedure S42_WGS(b1,l1,h1: double; out b2,l2,h2: double); stdcall;
procedure WGS_S42(b1,l1,h1: double; out b2,l2,h2: double); stdcall;

implementation

const
  EPS10 =	1.0E-10;

function Ellipsoids_Count: Integer;
begin
  Result:=Ellipsoids_Max
end;

procedure Get_Ellipsoid(elp: Integer; out a,b: Double);
var
  e: TEllipsoid;
begin
  e:=Ellipsoids[0];
  if elp in [1..Ellipsoids_Max] then
  e:=Ellipsoids[elp];
  a:=e.A; b:=e.B
end;

procedure Ellipsoids_Init;
var
  i: Integer;
begin
  for i:=0 to Ellipsoids_Max do
  with Ellipsoids[i] do begin
    A2:=Sqr(A); F:=1/Alfa; B:=A*(1-F);
    Es:=2*F - F*F; E:=Sqrt(Es); Es1:=1 - Es;
    Esp:=Es/(1-Es)
  end;

  pi2:=2*Pi; pi_2:=Pi/2; pi_4:=Pi/4;
  Small:=Pi/180/60/60/100;
  c_:=Ellipsoids[1]
end;

function Radian(g,m,s: Integer): double;
begin
  Result:=(g + m/60 + s/3600) / 180 * Pi
end;

function pj_msfn(Fi: Double): Double;
var
  s,c: Extended;
begin
  SinCos(Fi, s,c);
  Result:=c / Sqrt(1 - c_.Es*s*s)
end;

function pj_tsfn(Fi: Double): Double;
var
  s: Extended;
begin
  s:=Sin(Fi) * c_.E;
  Result:=Tan(pi_4 - Fi/2) /
          Power((1-s)/(1+s),c_.E/2);
end;

function pj_phi2(ts: Double): Double;
var
	i: Integer;
	e, e2, phi,dphi, s: Double;
begin
  e:=c_.E; e2:=e/2;
	phi:=pi_2 - 2*Arctan(ts);

	i:=16;
  repeat s:=e * Sin(phi);
		dphi:=pi_2 - 2*Arctan(ts * Power((1-s)/(1+s),e2)) - phi;
		phi:=phi + dphi; Dec(i);
  until (Abs(dphi) < EPS10) or (i = 0);

  Result:=phi
end;

function c_RO(Fi: double): Extended;
var
  s,c: Extended;
begin
  SinCos(Fi, s,c);
  Result:=c_.A * c / Sqrt(1 - c_.Es*s*s)
end;

procedure gk_BL_XY(B,L, L0: double; out X,Y: double);
var
  N,N1,RAS,A2,A4,A6,F,Z,P,R,ALF,TZ,TZ2,TH,T2,
  a11,a21,a31,b11,b21,b31,c11,c21,c31,d11,d21,d31,
  K2,K4,K6,LL,LLL: double; Sign: Boolean;
begin
  N:=c_.F/(2-c_.F);
  Sign:=B < 0; B:=Abs(B);

  N1:=Sqr(N); R:=c_.A*(1+N1/4+N1*N1/64)/(1+N);
  A2:=N/2-N1*(2/3)+N*N1*(5/16);
  A4:=N1*13/48-N*N1*3/5;
  A6:=N*N1*61/240;
  K2:=2*(N-N1/3-N*N1*2/3);
  K4:=N1*5/3-N1*N*16/15;
  K6:=N1*N*26/15;

  LLL:=ABS(L); IF LLL >= Pi THEN
  begin LLL:=pi2-LLL; L:=-LLL END;
  IF L < 0 THEN LL:=L+Pi2 ELSE LL:=L;

  IF Abs(L0) >= Pi THEN L0:=L0-pi2;
  IF L0 < 0 THEN L0:=L0+Pi2; RAS:=LL-L0;

  F:=B-K2*SIN(2*B)+K4*SIN(4*B)-K6*SIN(6*B);

  TZ:=(Sin(F)/Cos(F))/Cos(RAS); TZ2:=Sqr(TZ);
  Z:=ArcTan(TZ); TH:=COS(F)*SIN(RAS);
  T2:=Sqr(TH); P:=Ln((1+TH)/(1-TH))/2;
  a11:=2*TZ/(1+TZ2); b11:=(1-TZ2)/(1+TZ2);
  c11:=2*TH/(1-T2); d11:=(1+T2)/(1-T2);
  a21:=2*a11*b11; c21:=2*c11*d11;
  b21:=1-2*Sqr(a11); d21:=1+2*Sqr(c11);
  a31:=a11*b21+a21*b11; c31:=c11*d21+c21*d11;
  b31:=b11*b21-a11*a21; d31:=d11*d21+c11*c21;

  X:=R * (Z+A2*a11*d11+A4*a21*d21+A6*a31*d31);
  Y:=R * (P+A2*b11*c11+A4*b21*c21+A6*b31*c31);

  if Sign then X:=-X
end;

procedure gk_XY_BL(X,Y, L0: double; out B,L: double);
var
  N,N1,B2,B4,B6,F,Z,P,R,K2,K4,K6,U,V,
  a11,a21,a31,b11,b21,b31,c11,c21,c31,d11,d21,d31,
  sp,chp,SinF,TanL: double; sign,west: Boolean;
begin
  N:=c_.F/(2-c_.F);
  sign:=X < 0; X:=Abs(X);
  west:=L0 < 0;

  if Abs(L0) >= Pi then L0:=L0-pi2;
  if L0 < 0 then L0:=L0+pi2;

  N1:=Sqr(N); R:=c_.A*(1+N1/4+N1*N1/64)/(1+N);
  B2:=N/2-N1*(2/3)+N*N1*(37/96); B4:=N1/48+N*N1/15;
  B6:=N*N1*17/480; K2:=2*(N-N1/3-N*N1);
  K4:=N1*7/3-N1*N*8/5; K6:=N1*N*56/15;
  U:=X/R; V:=Y/R; a11:=Sin(2*U); b11:=Cos(2*U);
  a21:=2*a11*b11; b21:=1-2*(a11*a11);
  a31:=a11*b21+a21*b11; b31:=b11*b21-a11*a21;
  c11:=(exp(2*V)-exp(-2*V))/2; d11:=sqrt(1+sqr(c11));
  c21:=2*c11*d11; d21:=1+2*sqr(c11);
  c31:=c11*d21+c21*d11; d31:=c11*c21+d11*d21;
  Z:=U-B2*a11*d11-B4*a21*d21-B6*a31*d31;
  P:=V-B2*b11*c11-B4*b21*c21-B6*b31*c31;
  sp:=(exp(P)-exp(-P))/2; chp:=sqrt(1+Sqr(sp));
  SinF:=Sin(Z)/chp; F:=ArcSin(SinF);
  TanL:=sp/Cos(Z); L:=ArcTan(TanL);
  B:=F+K2*Sin(2*F)+K4*sin(4*F)+K6*sin(6*F);

  if sign then B:=-B;

  if west then
  while L0 > 0 do L0:=L0-Pi2;

  L:=L + L0;
end;

procedure m_BL_XY(B,L, B0: double; var X,Y: double);
var
  p: double; sign: Boolean;
begin
  sign:=B < 0; B:=Abs(B);
  p:=c_RO(Abs(B0));

  if Abs(B-Pi_2) <= EPS10 then
  B:=Pi_2-EPS10*2;

  X:=-p*Ln(pj_tsfn(B)); Y:=p*L;
  if sign then X:=-X
end;

procedure m_XY_BL(X,Y, B0: double; var B,L: double);
var
  p,fi: double; sign: Boolean;
begin
  sign:=X < 0; X:=Abs(X);
  p:=c_RO(Abs(B0));

  fi:=pj_phi2(Exp(-X/p));
  if sign then fi:=-fi;

  B:=fi; L:=Y/p;
end;

procedure get_n0_n1(b0: Double; elp: Integer;
                    out n0,n1: Extended);
var
  s: Extended;
begin
  if elp <> c_elp then
  Set_Ellipsoid(elp); s:=Sin(b0);
  n0:=c_.A / Sqrt(1 - c_.Es * s * s);
  n1:=c_.Es * n0 * s
end;

//  Вычисление геоцентрических координат по геодезическим
procedure bl_to_xyz(b,l,h: Double; elp: Integer;
                    out x,y,z: Double);
var
  e2, cb,sb, cl,sl, n: Extended;
begin
  if elp <> c_elp then
  Set_Ellipsoid(elp);

  e2:=c_.Es;

  SinCos(b, sb,cb);
  SinCos(l, sl,cl);

  n:=c_.A / Sqrt(1 - e2 * sb * sb);

  x:=(n + h) * cb * cl;
  y:=(n + h) * cb * sl;
  z:=(n * (1 - e2) + h) * sb
end;

//  Вычисление геодезических координат по геоцентрическим
function xyz_to_bl(x,y,z: Double; elp: Integer;
                   out _b,_l,_h: Double): Boolean;
const
  eps1 = 0.000001;
  eps2 = 0.000000001;

var
  i: Integer;
  a,b, a2,b2,a3,b3,r, d,d1,d2,z1,z2, e2, c2,s2: Extended;
  f,l, c,s, m, p,q, u,v, df: Extended; sz: Boolean;
begin
  Result:=false;

  if elp <> c_elp then
  Set_Ellipsoid(elp);

  _b:=0; _l:=0; _h:=0;

  a:=c_.A; a2:=Sqr(a);
  b:=c_.B; b2:=Sqr(b);
  e2:=c_.Es;

  d2:=Sqr(x) + Sqr(y); d:=0;
  if d2 > eps1 then d:=Sqrt(d2);

  z2:=Sqr(z); r:=d2 + z2;

  if r > a2 / 2.0 then
  if r < a2 * 1.5 then begin

    sz:=z < 0; z:=Abs(z);

    if Abs(x) < Eps1 then l:=0
    else l:=Arctan2(y,x);

    c2:=d2 / a2 + z2 / b2;
    r:=Sqrt(a2 * c2 - e2 * d2);

    if b * d > 0.7 * a * r then
      f:=ArcSin((a * z) / (b * r))
    else
      f:=ArcCos((b * z) / (a * r));

    for i:=1 to 16 do begin

      SinCos(f, s,c);
      c2:=c * c; s2:=s * s;

      r:=Sqrt(a2 * c2 + b2 * s2);

      a3:=a2 / r;  b3:=b2 / r;
      m:=a3 * (b3 / r);

      d1:=a3 * c; z1:=b3 * s;
      p:=d - d1; q:=z - z1;

      u:=q * c - p * s;
      v:=p * c + q * s;

      df:=Arctan(u / (m + v));

      f:=f+df;
      if Abs(df) < EPS2 then break;
    end;

    if sz then f:=-f;

    _b:=f; _l:=l; _h:=v;

    Result:=true
  end
end;

function xyz_to_xyz(const v: txyz;
                    geo: PDatum7;
                    dir: Integer): txyz;
var
  g: TDatum7;
begin
  g:=geo^; if dir < 0 then begin
    g.dX:=-g.dX; g.dY:=-g.dY; g.dZ:=-g.dZ;
    g.wx:=-g.wx; g.wy:=-g.wy; g.wz:=-g.wz;
    g.m:=-g.m
  end;

  g.m:=(1 + g.m); with g,v do begin
    Result.x:=m * (    x +wz*y -wy*z) + dX;
    Result.y:=m * (-wz*x +   y +wx*z) + dY;
    Result.z:=m * (+wy*x -wx*y +   z) + dZ;
  end
end;

procedure east_longitude(l1: Double; var l2: Double);
begin
  if l2 - l1 > +Pi then l2:=l2 - 2*Pi else
  if l2 - l1 < -Pi then l2:=l2 + 2*Pi;
end;

procedure GEO_GEO(b1,l1,h1: double; elp1,elp2: Integer;
                  geo1,geo2: PDatum7; out b2,l2,h2: double);
var
  v1,v2,vc: txyz;
begin
  bl_to_xyz(b1,l1,h1,elp1,v1.x,v1.y,v1.z);

  if elp1 = 9 then geo1:=nil;
  vc:=v1; if Assigned(geo1) then
  vc:=xyz_to_xyz(v1,geo1,1);

  if elp2 = 9 then geo2:=nil;
  v2:=vc; if Assigned(geo2) then
  v2:=xyz_to_xyz(vc,geo2,-1);

  xyz_to_bl(v2.x,v2.y,v2.z,elp2,b2,l2,h2);
  east_longitude(l1,l2)
end;

procedure GEO_GEO1(b1,l1,h1: double; elp1,elp2: Integer;
                   geo1,geo2: PDatum7; out b2,l2,h2: double);
var
  v1,v2,vc: txyz;
begin
  bl_to_xyz(b1,l1,h1,elp1,v1.x,v1.y,v1.z);

  vc:=xyz_to_xyz(v1,geo1,1);
  v2:=xyz_to_xyz(vc,geo2,-1);

  xyz_to_bl(v2.x,v2.y,v2.z,elp2,b2,l2,h2);
end;

procedure GEO_WGS(b1,l1,h1: double; elp: Integer;
                  geo: PDatum7; out b2,l2,h2: double); 
var
  v: txyz;
begin
  bl_to_xyz(b1,l1,h1,elp,v.x,v.y,v.z);
  with xyz_to_xyz(v,geo,1) do
  xyz_to_bl(x,y,z,9,b2,l2,h2);
  east_longitude(l1,l2)
end;

procedure WGS_GEO(b1,l1,h1: double; elp: Integer;
                  geo: PDatum7; out b2,l2,h2: double); 
var
  v: txyz;
begin
  bl_to_xyz(b1,l1,h1,9,v.x,v.y,v.z);
  with xyz_to_xyz(v,geo,-1) do
  xyz_to_bl(x,y,z,elp,b2,l2,h2);
  east_longitude(l1,l2)
end;

const
  geo42: TDatum7 =
  (dX: 26.3;
   dY: -132.6;
   dZ: -76.3;
   wx: -1.06659E-6;
   wy: -1.93925E-6;
   wz: -4.36332E-6;
   m:  -0.12E-6);

procedure S42_WGS(b1,l1,h1: double; out b2,l2,h2: double); 
var
  v: txyz;
begin
  bl_to_xyz(b1,l1,h1,1,v.x,v.y,v.z);
  with xyz_to_xyz(v,@geo42,1) do
  xyz_to_bl(x,y,z,9,b2,l2,h2);
  east_longitude(l1,l2)
end;

procedure WGS_S42(b1,l1,h1: double; out b2,l2,h2: double); 
var
  v: txyz;
begin
  bl_to_xyz(b1,l1,h1,9,v.x,v.y,v.z);
  with xyz_to_xyz(v,@geo42,-1) do
  xyz_to_bl(x,y,z,1,b2,l2,h2);
  east_longitude(l1,l2)
end;

function b_Range(b: Double): Double;
begin
  Result:=Max(-Pi_2,Min(Pi_2,b))
end;

function abs_b(b: Double): Double;
begin
  Result:=Min(Pi_2,Abs(b))
end;

procedure Calc_U_R(fi: Double; out U,R: Extended);
var
  c,s, B,Rx: Extended;
begin
  SinCos(fi, s,c);

  B:=c_.E * s;
  B:=Arctan(B/Sqrt(1-Sqr(B)));

  Rx:=Exp(c_.E * Ln(Cotan(pi_4 + B/2)));

  U:=Rx * Tan(pi_4 + fi/2);
  R:=c_.A * c /Sqrt(1 - Sqr(c_.E*s));
end;

function conus_b2(var b1,b2: Double): Boolean;
var
  _b1,_b2,dg: Double;
begin
  if b1 > b2 then xSwap(b1,b2);

  Result:=b1 < 0;

  b1:=abs_b(b1);
  b2:=abs_b(b2);

  _b1:=b1; _b2:=b2;
  if _b2 < _b1 then begin
    _b1:=b2; _b2:=b1
  end;

  dg:=Pi/180;

  if _b2 < _b1+dg then begin
    _b2:=_b1+dg; if _b2 >= pi_2 then begin
      _b2:=pi_2-dg; _b1:=_b2-dg*6
    end
  end;

  b1:=_b1; b2:=_b2;
end;

// Длина дуги от экватора до параллели Fi
function c_SO(B: double): double;
var
  Y: Double;
begin
  gk_BL_XY(B,0,0, Result,Y)
end;

function conus_dl(L,Lc: Extended): Extended;
begin
  Result:=L - Lc;
  while Result < -Pi do Result:=Result + Pi2;
  while Result > Pi do Result:=Result - Pi2
end;

var
  conus1: record
     _alf,_C,_RO1: Extended;
     _C2,_C4,_C6,_C8: Extended;
     _B1,_B2,_L1: Double;
     south: Longbool
   end;

procedure conus1_Init(b1,b2,l1: Double);
var
   Fx,Ux,Rx,U1,R1,U2,R2: Extended;
   e2,e4,e6,e8: Extended;
begin
  with conus1 do
  if (_B1 <> b1)
  or (_B2 <> b2)
  or (_L1 <> l1) then begin

    _B1:=b1; _B2:=b2;
    south:=conus_b2(b1,b2); _L1:=l1;

    Calc_U_R(b1, U1,R1);
    Calc_U_R(b2, U2,R2);

    _alf:=(Ln(R1)-Ln(R2))/(Ln(U2)-Ln(U1));
    Fx:=ArcSin(_alf);

    Calc_U_R(Fx, Ux,Rx);

    _C:=Sqrt(R1*Rx*Exp(Ln(U1*Ux) * _alf)) / _alf;
    _RO1:=_C/Exp(Ln(U1) * _alf);
    _RO1:=_C;

    e2:=c_.Es; e4:=sqr(e2);
    e6:=e2*e4; e8:=sqr(e4);

    _C2:=e2/2+5/24*e4+e6/12+13/360*e8;
    _C4:=7/48*e4+29/240*e6+811/11520*e8;
    _C6:=7/120*e6+81/1120*e8;
    _C8:=4279/161280*e8;
  end
end;

procedure conus1_BL_XY(b,l: Double; out x,y: Double);
var
   p,u,dl, sin,cos: Extended;
begin
  with conus1 do begin
    Calc_U_R(Abs(b),u,p);
    p:=_C/Exp(Ln(u) * _alf);

    dl:=conus_dl(l,_L1);
    SinCos(_alf*dl, sin,cos);
    x:=_RO1 - p*cos; y:=p*sin;
    if south then x:=-x
  end
end;

procedure conus1_XY_BL(x,y: Double; out b,l: Double);
var
  u2,fi: Extended; h: Double;
begin
  with conus1 do begin
    h:=_RO1 - Abs(x);

    if Abs(h) < 0.1 then l:=_L1 else
    l:=ArcTan2(y,h)/_alf + _L1;

    u2:=Sqr(Exp(Ln(_C/ Hypot(h,y) ) / _alf));

    fi:=ArcSin((u2-1)/(u2+1));

    b:=fi + _C2*Sin(2*fi) + _C4*Sin(4*fi) +
            _C6*Sin(6*fi) + _C8*Sin(8*fi);

    if south then b:=-b
  end
end;

type
  RC_ru = record
     _alf,_C,_RO1: Extended;
     _B1,_B2,_L1: Double;
   end;

procedure conus_ru_BL_XY(b,l: Double; const rc: RC_ru; out x,y: Double);
var
  p,dl, sin,cos: Extended;
begin
  p:=rc._C - c_SO(b);
  dl:=conus_dl(l,rc._L1);
  SinCos(rc._alf * dl, sin,cos);
  x:=rc._RO1 - p*cos; y:=p*sin;
end;

procedure conus_ru_XY_BL(x,y: Double; const rc: RC_ru; out b,l: Double);
var
  p: Extended;
begin
  x:=rc._RO1 - x; p:=Hypot(x,y);
  gk_XY_BL(rc._C-p,0, 0, b,l);

  if Abs(x) < 0.1 then l:=rc._L1 else
  l:=ArcTan2(y,x)/rc._alf + rc._L1;
end;

var
  conus2: RC_ru;

procedure conus2_Init(b1,b2,l1: Double);
begin
  with conus2 do
  if (_B1 <> b1)
  or (_B2 <> b2)
  or (_L1 <> l1) then begin

    _B1:=b1; _B2:=b2;
    conus_b2(b1,b2); _L1:=l1;

    _alf:=(c_RO(b1)-c_RO(b2))/(c_SO(b2)-c_SO(b1));
    _C:=c_RO(b1)/_alf + c_SO(b1);
    _RO1:=c_RO(b1)
  end
end;

procedure conus2_BL_XY(b,l: Double; out x,y: Double);
begin
  conus_ru_BL_XY(b,l, conus2, x,y)
end;

procedure conus2_XY_BL(x,y: Double; out b,l: Double);
begin
  conus_ru_XY_BL(x,y, conus2, b,l)
end;

var
  conus3: RC_ru;

procedure conus3_Init(b1,b2,l1: Double);
var
  bc, r1,r2,rc, s1,s2,sc: Extended;
begin
  with conus3 do
  if (_B1 <> b1)
  or (_B2 <> b2)
  or (_L1 <> l1) then begin

    _B1:=b1; _B2:=b2; _L1:=l1;
    conus_b2(b1,b2); bc:=(b1+b2)/2;

    r1:=c_RO(b1); r2:=c_RO(b2); rc:=c_RO(bc);
    s1:=c_SO(b1); s2:=c_SO(b2); sc:=c_SO(bc);

    _C:=(r2*s1-r1*s2)/(r2-r1);
    _alf:=Sqrt(r2*rc/((_C-sc)*(_C-s2)));
    _alf:=0.850966;

    _RO1:=c_RO(b1)
  end
end;

procedure conus3_BL_XY(b,l: Double; out x,y: Double);
begin
  conus_ru_BL_XY(b,l, conus3, x,y)
end;

procedure conus3_XY_BL(x,y: Double; out b,l: Double);
begin
  conus_ru_XY_BL(x,y, conus3, b,l)
end;

var
  conus4: record
     _alf,_RO1: Extended;
     _B1,_B2,_L1: Double;
   end;

procedure conus4_Init(b1,b2,l1: Double);
var
  teta: Extended;
begin
  with conus4 do
  if (_B1 <> b1)
  or (_B2 <> b2)
  or (_L1 <> l1) then begin

    _B1:=b1; _B2:=b2; _L1:=l1;
    conus_b2(b1,b2); teta:=(b2-b1)/2;

    _alf:=Sin(b1/2+b2/2) * Cos(teta) / Cos(teta/2);
    _RO1:=c_RO(b1)
  end
end;

procedure conus4_BL_XY(b,l: Double; out x,y: Double);
var
  p,dl, sin,cos: Extended;
begin
  with conus4 do begin
    p:=c_RO(b_Range(b));
    dl:=conus_dl(l,_L1);
    SinCos(_alf*dl, sin,cos);
    x:=_RO1 - p*cos; y:=p*sin;
  end
end;

procedure conus4_XY_BL(x,y: Double; out b,l: Double);
var
  p,dp,fi,df,old,grad: Extended; i: Integer;
begin
  with conus4 do begin
    x:=_RO1 - x; grad:=Pi/180;

    if Abs(x) < 0.1 then l:=_L1 else
    l:=ArcTan2(y,x)/_alf + _L1
  end;

  p:=Hypot(x,y); dp:=0;

  if Abs(p) >= c_.A then fi:=0 else
  fi:=ArcCos(p/c_.A);

  for i:=1 to 1024 do begin
    old:=dp; dp:=p-c_RO(fi);

    if Abs(dp) > 111000 then begin
      if (old < 0) <> (dp < 0) then
      grad:=grad/2; df:=grad;
      if dp < 0 then df:=-df;
    end else

    if Abs(dp) > 0.1 then
      df:=dp/30/36000*grad
    else
      Break;

    if fi < 0 then df:=-df;
    fi:=fi - df
  end;

  b:=fi
end;

// Lambert Conformal conic
var
  lcc: record
     _b0,_l0,_b1,_b2: Double;
     _c,_n,_p0,_k0: Extended;
   end;

procedure lcc_Init(l0,b0,b1,b2,k0: Double);
var
//  sin_b1,cos_b1: Extended;
   m1,ml1: Double;
begin
  with lcc do
  if (_l0 <> l0)
  or (_b0 <> b0)
  or (_b1 <> b1)
  or (_b2 <> b2)
  or (_k0 <> k0) then begin

    _b0:=b0; _l0:=l0;
    _b1:=b1; _b2:=b2;
    _b2:=Max(_b2,_b1); _k0:=k0;

    _n:=Sin(_b1);

		m1:=pj_msfn(_b1); ml1:=pj_tsfn(_b1);

    if Abs(_b2 - _b1) >= EPS10 then
    _n:=ln(m1 / pj_msfn(_b2)) /
        ln(ml1 / pj_tsfn(_b2));

    _c:=c_.A * m1 / Power(ml1, _n) / _n;
    _p0:=_c * Power(pj_tsfn(_b0),_n);
  end
end;

procedure lcc_BL_XY(b,l: Double; out x,y: Double);
var
  p,dl, sin,cos: Extended;
begin
  with lcc do begin
    p:=_c * Power(pj_tsfn(b),_n);

    dl:=conus_dl(l,_l0);
    SinCos(_n*dl, sin,cos);

    x:=_k0 * (_p0 - p*cos);
    y:=_k0 * p*sin;
  end
end;

procedure lcc_XY_BL(x,y: Double; out b,l: Double);
var
  p: Extended;
begin
  with lcc do begin
    x:=x / _k0; y:=y / _k0;

    x:=_p0 - x; p:=Hypot(x,y);

    if _n < 0 then begin
      x:=-x; y:=-y; p:=-p;
    end;

    if Abs(x) < 0.1 then l:=_l0 else
    l:=ArcTan2(y,x)/_n + _L0;

		b:=pj_phi2(Power(p/_c,1/_n))
  end
end;

procedure pc_BL_XY(b,l, L0: Double; out x,y: Double);
var
  temp,dX, NR, ro, sinphi,cosphi: Extended;
  delt: Extended; Sign: Boolean;
begin
  Sign:=b < 0; b:=Abs(b); dX:=c_SO(b);

  SinCos(b, sinphi,cosphi);
  temp:= 1.0 - c_.Es * Sqr(sinPhi);
  NR:=c_.a / Sqrt(temp);

  if Abs(b) < 0.00001 then begin
   x:=dX; y:=NR*(L-L0);
  end
  else begin
    ro:=NR*cosphi/sinphi;
    delt:=(L-L0)*sinphi;
    x:=dX+ro*(1-cos(delt));
    y:=ro*sin(Delt)
  end;

  if Sign then x:=-x
end;

function pc_XY_BL(X,Y, L0: double; out B,L: double): Integer;
var
  tau, Fi,Lpr, db,dl: Double;
  tgc: TGauss; Sign: Boolean;
begin
  Sign:=X < 0; X:=Abs(X);

  gk_XY_BL(X,Y, L0, Fi,Lpr); Lpr:=Lpr - L0;

  tau:=1.9e-7;
  for Result:=1 to 1024 do begin
    pc_BL_XY(fi,Lpr,0,tgc.x,tgc.y);

    dl:=tau*(y-tgc.y); Lpr:=Lpr+dl;
    db:=tau*(x-tgc.x); fi:=fi+db;

    if Max(ABS(db),ABS(dl)) < Small then Break
  end;

  if Sign then Fi:=-Fi;
  B:=Fi; L:=Lpr+L0;
end;

const
  _Small = 1.e-12;

function phi_Range(B: Double; B1,B2: Integer): Double;
var
  _B1,_B2: Double;
begin
  B:=Abs(B); _B1:=B1/180*Pi; _B2:=B2/180*Pi;
  Result:=Min(Max(B,_B1),_B2)
end;

function back_sin(cos: Extended): Extended;
begin
  if Abs(cos) > 1-_Small then Result:=0
  else Result:=Sqrt(1 - cos*cos)
end;

function stereo_u(B: Double): Extended;
var
  w,f: Extended;
begin
  w:=c_.E * Sin(B); f:=Pi_4 + B/2;
  Result:=Tan(f) * Power((1-w)/(1+w),c_.E/2);
end;

function norm_usm(Phi,k0: Extended): Extended;
var
  w,f,u: Extended;
begin
  w:=c_.E * Sin(Phi); f:=pi_4 + Phi*0.5;

  if Abs(f-Pi_2) < _Small then begin
    u:=Cotan(f) * Power((1+w)/(1-w),0.5*c_.E);
    Result:=2 * Arctan2(1/k0,U) - Pi_2;
  end
  else begin
    u:=Tan(f) * Power((1-w)/(1+w),0.5*c_.E);
    Result:=2 * Arctan(u/k0) - Pi_2;
  end;
end;

function cross_usm(Phi,k0,alf: Extended): Extended;
var
  w,f,u: Extended;
begin
  w:=c_.E * Sin(Phi); f:=Pi_4 + Phi/2;
  u:=Tan(f) * Power((1-w)/(1+w),c_.E/2);
  Result:=2 * Arctan(k0 * Power(u,alf)) - Pi_2;
end;

function pole_usm(phi,k0: Extended): Extended;
var
  w,f,u: Extended;
begin
  w:=c_.E*Sin(phi); f:=Pi_4 + phi/2;
  u:=Cotan(f) * Power((1+w)/(1-w),c_.E/2);
  Result:=2 * Arctan2(1/k0,u) - Pi_2
end;

function back_pole(usm,k0: Extended): Double;
var
  phi,db,tau,usm_: Extended;
  loop: Integer;
begin
  phi:=usm; tau:=1;

  for loop:=1 to 100000 do begin
    usm_:=pole_usm(phi,k0);
    db:=tau*(usm-usm_); Phi:=Phi + db;
    if Abs(db) < _Small then Break
  end;

  Result:=Phi
end;

function back_norm(usm,k0: Extended): Double;
var
  phi,db,tau,usm_: Extended;
  loop: Integer;
begin
  phi:=usm; tau:=1;

  for loop:=1 to 100000 do begin
    usm_:=norm_usm(phi,k0);
    db:=tau*(usm-usm_); Phi:=Phi + db;
    if Abs(db) < _Small then Break
  end;

  Result:=Phi
end;

function back_cross(sinPhi,k0,alf: Extended): Double;
var
  phi,db,tau,usm,usm_: Extended;
  loop: Integer;
begin
  usm:=Arcsin(sinPhi);

  phi:=usm; tau:=1;
  for loop:=1 to 100000 do begin

    usm_:=cross_usm(phi, k0,alf);

    db:=tau*(usm-usm_); phi:=phi + db;
    if Abs(db) < _Small then Break
  end;

  Result:=phi
end;

function ro_SinCos(X,Y: Double;
                   out sinA,cosA: Extended): Extended;
begin
  Result:=Hypot(X,Y);

  if Result > small then begin
    sinA:=Y/Result; cosA:=X/Result;
  end
  else begin
    sinA:=0; cosA:=1
  end;
end;

function Init_Rn(ne,Bn,u0: Extended): Extended;
begin
  Result:=c_.A / Sqrt(ne) * Cos(Bn) / Cos(u0);

  if c_Rn >= 6000000 then
  if c_Rn <= 7000000 then
  Result:=c_Rn;
end;

function Zet_ro1(Rn,Zet: Extended): Extended;
var
  Sinz: Extended;
begin
  Result:=Rn; Sinz:=Sin(Zet);
  if Abs(Sinz) > 1E-6 then
  Result:=Rn*Zet/Sinz
end;

function Zet_ro2(Rn,Zet: Extended): Extended;
var
  Sinz: Extended;
begin
  Result:=Rn; Sinz:=Sin(Zet);
  if Abs(Sinz) > 1E-6 then
  Result:=2*Rn*Sin(Zet/2)/Sinz
end;

// Поперечная равноугольная цилиндрическая Меркатора
var
  mCross: record
    _k0,_Rn: Extended; _Bn: Double;
  end;

procedure Mcross_Init(Bn: Double);
var
  sb,cb, ne, u,u0: Extended;
begin
  with mCross do

  if Bn <> _Bn then begin
    Bn:=phi_Range(Bn,45,90);

    _Bn:=Bn; SinCos(Bn, sb,cb);
    ne:=1 - c_.Es * sb * sb;

    u0:=Pi_2 - Arctan( Sqrt(ne/(1 - c_.Es)) * (cb/sb) );

    u:=stereo_u(Bn);

    _k0:=Cotan(PI_4 + u0*0.5) * u;

    _Rn:=Init_Rn(ne,Bn,u0);
  end
end;

procedure mCross_BL_XY(B,L: Double; out X,Y: Double);
var
  usm, cl,sl, kvB,kvL: extended;
begin
  SinCos(L,sl,cl);

  with mCross do begin
    usm:=pole_usm(B,_k0);

    kvB:=Arcsin(-Cos(usm) * cl);  //z
    kvL:=Arctan(CoTan(usm) * sl); //a

    Y:=_Rn * kvL;
    X:=_Rn * Ln(Tan(pi_4 + 0.5*kvB))
  end
end;

procedure mCross_XY_BL(X,Y: Double; out B,L: Double);
var
  r0,usm,kvB,kvL: Extended;
begin
  r0:=mCross._Rn;

  kvB:=2.0*Arctan(Exp(X/R0))-Pi_2; kvL:=Y/R0;

  usm:=Arcsin(cos(kvb)*cos(kvl));

  if (Abs(kvB) < _Small) and
     (Abs(kvL) < _Small) then
    L:=0
  else
    L:=Arctan2(sin(kvL),-Tan(kvB));

  B:=back_pole(usm,mCross._k0)
end;

type
  tilted_ctrl = record
    L0_alf,_Rn,_k0,_alf,_sinu0,_cosu0: Extended;
    _Bn,_B0,_L0: Double;
  end;

procedure tilted_Init(var ctrl: tilted_ctrl; Bn,B0,L0: Double);
var
  sb, ne,ne_2, u,u0, n,m,sfern: Extended;
begin
  with ctrl do

  if (_Bn <> Bn)
  or (_B0 <> B0)
  or (_L0 <> L0) then begin

    _Bn:=Bn; _B0:=B0; _L0:=L0;

    sb:=Sin(Bn);
    ne:=1 - c_.Es * sb * sb; ne_2:=Sqrt(ne);

    n:=c_.a / ne_2;  m:=n * (1 - c_.Es) / ne;
    _Rn:=Sqrt(m*n);

    sfern:=Arctan(_Rn / n * Tan(Bn));

    _alf:=1/Sqrt(1 - c_.Es);
    if Abs(sfern) > Small then
    _alf:=sb / Sin(sfern);

    u:=stereo_u(Bn);
    _k0:=Tan(Pi_4 + sfern*0.5) / Power(u,_alf);

    u:=stereo_u(B0);
    u0:=2 * Arctan(_k0 * Power(u,_alf)) - Pi_2;

    L0_alf:=l0 * _alf; SinCos(u0, _sinu0,_cosu0);
  end
end;

// Азимутальная косая стереографическая
var
  tStereo: tilted_ctrl;

procedure tStereo_BL_XY(B,L: Double; out X,Y: Double);
var
  usm,sinu,cosu,sindW,cosdw, ro: Extended;
begin
  with tStereo do begin
    usm:=cross_usm(B,_k0,_alf);
    SinCos(usm,sinu,cosu);
    SinCos(L*_alf - L0_alf,sindw,cosdw);

    ro:=2 * _Rn / (1 + _sinu0*sinu + _cosu0*cosu*cosdw);

    X:=(_cosu0*sinu - _sinu0*cosu*cosdw) * ro;
    Y:=cosu*sindW * ro;
  end
end;

procedure tStereo_XY_BL(X,Y: Double; out B,L: Double);
var
  ro,sinA,cosA: Extended;
  tanzet,tanzet2,sinzet,cosZet,sinPhi,dl: Extended;
begin
  ro:=ro_SinCos(X,Y, sinA,cosA);

  with tStereo do begin
    tanzet:=ro / (2 * _Rn);
    tanzet2:=Sqr(tanzet);

    sinZet:=2*tanzet/(1 + tanzet2);
    cosZet:=(1 - tanzet2)/(1 + tanzet2);

    sinPhi:=cosZet*_sinu0 + sinZet*_cosu0*CosA;
    dl:=Arctan2(sinZet*sinA,cosZet*_cosu0 - sinZet*_sinu0*CosA);

    L:=(L0_alf + dl)/_alf;
    B:=back_cross(sinPhi, _k0,_alf)
  end
end;

type
  tpole_ctrl = record
    _k0,_Rn: Extended;
    _sinu0,_cosu0: Extended;
    _Bn,_L0: Double;
  end;

procedure pole_Init(var ctrl: tpole_ctrl; Bn,L0: Double);
var
  sb,cb,ne, u,u0: Extended;
begin
  with ctrl do

  if (_Bn <> Bn)
  or (_L0 <> L0) then begin

    _Bn:=Bn; _L0:=L0;

    SinCos(Bn,sb,cb); ne:=1 - c_.Es * sb * sb;
    u0:=Pi_2 - Arctan(Sqrt(ne/(1-c_.Es)) * (cb/sb));

    u:=stereo_u(Bn);
    _k0:=Cotan(Pi_4 + u0*0.5) * u;

    _Rn:=Init_Rn(ne,Bn,u0);

    SinCos(u0,_sinu0,_cosu0);
  end
end;

// Азимутальная косая стереографическая (полюс)
var
  pStereo: tpole_ctrl;

procedure pStereo_BL_XY(B,L: Double; out X,Y: Double);
var
  usm,sinu,cosu,sindw,cosdw, ro: Extended;
begin
  with pStereo do begin
    usm:=pole_usm(B,_k0);

    SinCos(usm,sinu,cosu);
    SinCos(L - _L0,sindw,cosdw);
    ro:=2 * _Rn / (1 + _sinu0*sinu + _cosu0*cosu*cosdw);
    X:=(_cosu0*sinu - _sinu0*cosu*cosdw) * ro;
    Y:=cosu*sindw * ro
  end
end;

procedure pStereo_XY_BL(X,Y: Double; out B,L: Double);
var
  ro,sinA,cosA, tanzet,tanzet2: Extended;
  sinzet,cosZet,sinPhi,dl: Extended;
begin
  with pStereo do begin
    ro:=ro_SinCos(X,Y, sinA,cosA);

    tanzet:=ro / (2*_Rn);
    tanzet2:=Sqr(tanzet);

    sinZet:=2 * tanzet/(1 + tanzet2);
    cosZet:=(1 - tanzet2)/(1 + tanzet2);

    sinPhi:=cosZet*_sinu0 + sinZet*_cosu0*CosA;
    dl:=Arctan2(sinZet*sinA,cosZet*_cosu0 - sinZet*_sinu0*CosA);
    L:=_L0 + dl;

    B:=back_pole( Arcsin(sinPhi),_k0 );
  end
end;

type
  tnorm_ctrl = record
    _k0,_Rn: Extended;
    _Bn,_L0,_Sign,_Sign1: Double;
  end;

procedure norm_Init(var ctrl: tnorm_ctrl; Bn,L0,Sign: Double);
var
  sb,cb, ne, u,u0: Extended;
begin
  with ctrl do
  if (_Bn <> Bn)
  or (_L0 <> L0) then begin

    _Bn:=Bn; _L0:=L0; _Sign:=Sign;

    _Sign1:=1;
    if Bn < 0 then begin
      Bn:=Abs(Bn); _Sign1:=-1;
    end;

    Bn:=phi_Range(Bn,10,80);

    SinCos(Bn, sb,cb);
    ne:=1 - c_.Es * sb * sb;

    u0:=Pi_2 - Arctan(Sqrt(ne /(1-c_.Es)) * (cb/sb));

    u:=stereo_u(Bn);
    _k0:=Cotan(PI_4 + u0*0.5) * u;

    _Rn:=Init_Rn(ne,Bn,u0)
  end
end;

// Азимутальная нормальная,зеркальная стереографическая
var
  nStereo: tnorm_ctrl;
  mStereo: tnorm_ctrl;

procedure nStereo_BL_XY(B,L: Double; out X,Y: Double);
var
  c,s, usm, ro: Extended;
begin
  with nStereo do begin
    usm:=pole_usm(B*_Sign1,_k0);

    SinCos(usm, s,c);
    ro:=2 * _Rn * c / (1 + s);

    SinCos(L - _L0, s,c);
    X:=ro * c * _Sign * _Sign1;
    Y:=ro * s;
  end
end;

procedure nStereo_XY_BL(X,Y: Double; out B,L: Double);
var
  usm,ro: Extended;
begin
  with nStereo do begin
    X:=X * _Sign * _Sign1; ro:=Hypot(X,Y);

    if ro = 0 then
      B:=Pi_2
    else begin
      usm:=Pi_2 - 2*Arctan(ro/(_Rn*2));
      B:=back_pole(usm,_k0)*_Sign1
    end;

    L:=_L0 + Arctan2(Y,X);
  end
end;

// Азимутальная нормальная,зеркальная гномоническая
var
  nGnomon: tnorm_ctrl;
  mGnomon: tnorm_ctrl;

procedure nGnomon_BL_XY(B,L: Double; out X,Y: Double);
var
  c,s, usm,ro: extended;
begin
  with nGnomon do begin
    usm:=norm_usm(B,_k0);

    ro:=_Rn * Cotan(usm);

    SinCos(L - _L0, s,c);
    X:=ro * c * _Sign; Y:=ro * s
  end
end;

procedure nGnomon_XY_BL(X,Y: Double; out B,L: Double);
var
  usm,ro: extended;
begin
  with nGnomon do begin
    X:=X * _Sign; ro:=Hypot(X,Y);

    if ro = 0 then
      B:=Pi_2
    else begin
      usm:=Arctan2(_Rn,ro);
      B:=back_norm(usm,_k0)
    end;

    L:=_L0 + Arctan2(Y,X)
  end
end;

type
  tcross_ctrl = record
    _Rn,_alf,L0_alf: Extended;
    _L0: Double;
  end;

procedure cross_Init(var ctrl: tcross_ctrl; L0: Double);
var
  n,m: Extended;
begin
  with ctrl do
  if L0 <> _L0 then begin
    n:=c_.A; m:=c_.A *(1 - c_.Es);
    _Rn:=Sqrt(m*n);

    _alf:=1 / Sqrt(1 - c_.Es);

    L0_alf:=_alf * L0; _L0:=L0
  end
end;

// Азимутальная поперечная стереографическая
var
  cStereo: tcross_ctrl;

procedure cStereo_BL_XY(B,L: Double; out X,Y: double);
var
  usm,sinu,cosu,sindw,cosdw, ro: Extended;
begin
  with cStereo do begin
    usm:=cross_usm(B,1,_alf);

    SinCos(usm,sinu,cosu);
    SinCos(L*_alf - L0_alf,sindw,cosdw);

    ro:=2*_Rn /(1 + cosu*cosdw);
    X:=sinu * ro;
    Y:=cosu*sindw * ro
  end
end;

procedure cStereo_XY_BL(X,Y: Double; out B,L: Double);
var
  ro, sinA,cosA, tanzet,tanzet2: Extended;
  sinzet,cosZet,sinPhi,dl: Extended;
begin
  with cStereo do begin
    ro:=ro_SinCos(X,Y, sinA,cosA);

    tanzet:=ro / (2*_Rn);
    tanzet2:=Sqr(tanzet);

    sinZet:=2 * tanzet/(1 + tanzet2);
    cosZet:=(1 - tanzet2)/(1 + tanzet2);

    sinPhi:=sinZet*CosA;
    dl:=Arctan2(sinZet*sinA,cosZet);

    L:=(L0_alf + dl)/_alf;
    B:=back_cross(sinPhi,1,_alf)
  end
end;

// Азимутальная поперечная гномоническая
var
  cGnomon: tcross_ctrl;

procedure cGnomon_BL_XY(B,L: Double; out X,Y: double);
var
  usm,sinu,cosu,sindw,cosdw, ro: Extended;
begin
  with cGnomon do begin
    usm:=cross_usm(B,1,_alf);

    SinCos(usm,sinu,cosu);
    SinCos(L*_alf - L0_alf,sindw,cosdw);

    ro:=_Rn/(cosu*cosdw);
    X:=sinu * ro;
    Y:=cosu * sindw * ro;
  end
end;

procedure cGnomon_XY_BL(X,Y: Double; out B,L: Double);
var
  ro, sinA,cosA, zet,sinzet,cosZet,sinPhi,dl: Extended;
begin
  ro:=ro_SinCos(X,Y, sinA,cosA);

  with cGnomon do begin
    zet:=Arctan(ro/_Rn);
    SinCos(zet,sinZet,CosZet);
    sinPhi:=sinZet*CosA;
    dl:=Arctan2(sinZet*sinA,cosZet);

    L:=(L0_alf + dl)/_alf;
    B:=back_cross(sinPhi,1,_alf)
  end
end;

// Азимутальная косая гномоническая
var
  tGnomon: tilted_ctrl;

procedure tGnomon_BL_XY(B,L: Double; out X,Y: Double);
var
  usm,sinu,cosu,sindw,cosdw, ro: extended;
begin
  with tGnomon do begin
    usm:=cross_usm(B,_k0,_alf);

    SinCos(usm,sinu,cosu);
    SinCos(L*_alf - L0_alf,sindw,cosdw);
    ro:=_Rn / (_sinu0*sinu + _cosu0*cosu*cosdw);
    X:=(_cosu0*sinu - _sinu0*cosu*cosdw) * ro;
    Y:=cosu*sindw * ro;
  end
end;

procedure tGnomon_XY_BL(X,Y: Double; out B,L: Double);
var
  ro,sinA,cosA,zet,sinzet,cosZet,sinPhi,dl: extended;
begin
  with tGnomon do begin
    ro:=ro_SinCos(X,Y, sinA,cosA);

    zet:=Arctan(ro/_Rn);
    SinCos(zet,sinZet,CosZet);

    sinPhi:=cosZet*_sinu0 + sinZet*_cosu0*CosA;
    dl:=Arctan2(sinZet*sinA,cosZet*_cosu0 - sinZet*_sinu0*CosA);

    L:=(L0_alf + dl)/_alf;
    B:=back_cross(sinPhi,_k0,_alf)
  end
end;

// Азимутальная косая гномоническая (полюс)
var
  pGnomon: tpole_ctrl;

procedure pGnomon_BL_XY(B,L: Double; out X,Y: Double);
var
  usm,sinu,cosu,sindw,cosdw, ro: extended;
begin
  with pGnomon do begin
    usm:=pole_usm(B,_k0);

    SinCos(usm,sinu,cosu);
    SinCos(L - _L0,sindw,cosdw);

    ro:=_Rn/(_sinu0*sinu + _cosu0*cosu*cosdw);
    X:=(_cosu0*sinu - _sinu0*cosu*cosdw) * ro;
    Y:=cosu*sindW * ro
  end
end;

procedure pGnomon_XY_BL(X,Y: Double; out B,L: Double);
var
  ro,sinA,cosA, zet,sinzet,cosZet,sinPhi,dl: extended;
begin
  with pGnomon do begin
    ro:=ro_SinCos(X,Y, sinA,cosA);

    zet:=Arctan(ro/_Rn);
    SinCos(zet,sinZet,CosZet);
    sinPhi:=cosZet*_sinu0 + sinZet*_cosu0*CosA;
    dl:=Arctan2(sinZet*sinA,cosZet*_cosu0 - sinZet*_sinu0*CosA);

    L:=_L0 + dl;
    B:=back_pole( Arcsin(sinPhi),_k0)
  end
end;

// Азимутальная косая равнопромежуточная Постеля
var
  tPostel: tilted_ctrl;

procedure tPostel_BL_XY(B,L: Double; out X,Y: Double);
var
  usm,sinu,cosu,sindw,cosdw, zet,ro: extended;
begin
  with tPostel do begin
    usm:=cross_usm(B,_k0,_alf);

    SinCos(usm,sinu,cosu);
    SinCos(L*_alf - L0_alf,sindw,cosdw);

    Zet:=Arccos(_sinu0*sinu + _cosu0*cosu*cosdw);
    ro:=Zet_ro1(_Rn,Zet);

    X:=(_cosu0*sinu - _sinu0*cosu*cosdw) * ro;
    Y:=cosu*sindw * ro;
  end
end;

procedure tPostel_XY_BL(X,Y: Double; out B,L: Double);
var
  ro,sinA,cosA, Zet,sinzet,cosZet,sinPhi,dl: extended;
begin
  with tPostel do begin
    ro:=ro_SinCos(X,Y, sinA,cosA);

    Zet:=ro/_Rn;
    SinCos(Zet, sinZet,cosZet);

    sinPhi:=cosZet*_sinu0 + sinZet*_cosu0*CosA;
    dl:=Arctan2(sinZet*sinA,cosZet*_cosu0 - sinZet*_sinu0*CosA);

    L:=(L0_alf + dl)/_alf;
    B:=back_cross(sinPhi,_k0,_alf)
  end
end;

// Азимутальная косая равнопромежуточная Постеля (полюс)
var
  pPostel: tpole_ctrl;

procedure pPostel_BL_XY(B,L: Double; out X,Y: Double);
var
  usm,sinu,cosu,sindw,cosdw, zet,ro: extended;
begin
  with pPostel do begin
    usm:=pole_usm(B,_k0);
    SinCos(usm,sinu,cosu);
    SinCos(L - _L0,sindw,cosdw);

    Zet:=Arccos(_sinu0*sinu + _cosu0*cosu*cosdw);
    ro:=Zet_ro1(_Rn,Zet);

    X:=(_cosu0*sinu - _sinu0*cosu*cosdw) * ro;
    Y:=cosu*sindW * ro;
  end
end;

procedure pPostel_XY_BL(X,Y: Double; out B,L: Double);
var
  ro,sinA,cosA, Zet,sinzet,cosZet,sinPhi,dl: extended;
begin
  with pPostel do begin
    ro:=ro_SinCos(X,Y, sinA,cosA);

    Zet:=ro/_Rn;
    SinCos(Zet, sinZet,cosZet);

    SinPhi:=cosZet*_sinu0 + sinZet*_cosu0*CosA;
    dl:=Arctan2(sinZet*sinA,cosZet*_cosu0 - sinZet*_sinu0*CosA);

    L:=_L0 + dl;
    B:=back_pole( Arcsin(sinPhi),_k0)
  end
end;

// Азимутальная нормальная,зеркальная равнопромежуточная Постеля
var
  nPostel: tnorm_ctrl;
  mPostel: tnorm_ctrl;

procedure nPostel_BL_XY(B,L: Double; out X,Y: Double);
var
  usm,sinu,cosu,sindw,cosdw, ro: Extended;
begin
  with nPostel do begin
    usm:=pole_usm(Abs(B),_k0);
    SinCos(usm,sinu,cosu);
    SinCos(L - _L0,sindw,cosdw);

    ro:=Zet_ro1(_Rn,Pi_2-usm);

    X:=-cosu*cosdw * ro * _Sign;
    Y:=cosu*sindW * ro;
  end
end;

procedure nPostel_XY_BL(X,Y: Double; out B,L: Double);
var
  ro,sinA,cosA, Zet,sinzet,cosZet,sinPhi,dl: extended;
begin
  with nPostel do begin
    X:=X * _Sign;
    ro:=ro_SinCos(X,Y, sinA,cosA);

    Zet:=ro/_Rn;
    SinCos(Zet, sinZet,cosZet);

    sinPhi:=cosZet;
    dl:=Arctan2(sinZet*sinA,-sinZet*CosA);

    L:=_L0 + dl;
    B:=back_pole( Arcsin(sinPhi),_k0)*_Sign1;
  end
end;

// Азимутальная поперечная равнопромежуточная Постеля
var
  cPostel: tcross_ctrl;

procedure cPostel_BL_XY(B,L: Double; out X,Y: Double);
var
  usm,sinu,cosu, sindW,cosdw, Zet,ro: extended;
begin
  with cPostel do begin
    usm:=cross_usm(B,1,_alf);

    SinCos(usm,sinu,cosu);
    SinCos(L*_alf - L0_alf,sindw,cosdw);
    Zet:=Arccos(cosu*cosdw);
    ro:=Zet_ro1(_Rn,Zet);

    X:=sinu * ro;
    Y:=cosu*sindW * ro;
  end
end;

procedure cPostel_XY_BL(X,Y: Double; out B,L: Double);
var
  ro,sinA,cosA, zet,sinzet,cosZet,sinPhi,dl: extended;
begin
  with cPostel do begin
    ro:=ro_SinCos(X,Y, sinA,cosA);

    Zet:=ro/_Rn;
    SinCos(Zet, sinZet,cosZet);

    sinPhi:=sinZet*CosA;
    dl:=Arctan2(sinZet*sinA,cosZet);

    L:=(L0_alf + dl)/_alf;
    B:=back_cross(sinPhi,1,_alf)
  end
end;

// Азимутальная равновеликая Ламберта
var
  sLambert: record
    _Rn,_Rn2, _sinu0,_cosu0: Extended;
    _B0,_L0: Double;
  end;

function Lambert_S(b: Double): Extended;
var
  sb, esb, Es2: Extended;
begin
  sb:=Sin(b); esb:=sb * c_.Es; Es2:=Sqr(c_.Es);

  Result:=0.5*c_.A2*(1-Es2)*(sb/(1-Sqr(esb)) +
          0.5 / c_.Es * Ln((1+esb)/(1-esb)));
end;

procedure sLambert_Init(B0,L0: Double);
begin
  with sLambert do
  if (B0 <> _B0)
  or (L0 <> _L0) then begin _B0:=B0; _L0:=L0;

    _Rn2:=0.5*c_.A2*(1 + (1 - 0.5*c_.Es*Ln((1+c_.es)/(1-c_.es))));
    _Rn:=Sqrt(_Rn2);

    _sinu0:=Lambert_S(B0) / _Rn2;
    _cosu0:=back_sin(_sinu0)
  end
end;

procedure sLambert_BL_XY(B,L: Double; out X,Y: Double);
var
  sinu,cosu,sindw,cosdw,cosZet, zet,ro: extended;
begin
  with sLambert do begin
    sinu:=Lambert_S(B) / _Rn2;
    if sinu > 1 then sinu:=1;
    if sinu < -1 then sinu:=-1;
    cosu:=back_sin(sinu);

    SinCos(L - _L0,sindw,cosdw);

    cosZet:=_sinu0*sinu + _cosu0*cosu*cosdw;
    if Abs(cosZet) > 1 then begin
      X:=NAN; Y:=NAN
    end
    else begin
      Zet:=ArcCos(cosZet);
      ro:=Zet_ro2(_Rn,Zet);

      X:=(_cosu0*sinu - _sinu0*cosu*cosdw) * ro;
      Y:=cosu*sindw * ro;
    end
  end
end;

procedure sLambert_XY_BL(X,Y: Double; out B,L: Double);
var
  ro, sinA,cosA,Zet,sinzet,cosZet,sinPhi,dl: Extended;
  loop: Integer; usm,usm_,tau,db,Phi: Extended;
begin
  B:=NAN; L:=NAN;

  with sLambert do begin
    ro:=ro_SinCos(X,Y, sinA,cosA) / 2;

    if ro <= _Rn then begin

      Zet:=2*ArcSin(ro/_Rn);
      SinCos(Zet, sinZet,cosZet);

      sinPhi:=cosZet*_sinu0 + sinZet*_cosu0*CosA;
      dl:=Arctan2(sinZet*sinA,cosZet*_cosu0 - sinZet*_sinu0*CosA);

      L:=(_L0 + dl);

      Phi:=Arcsin(sinPhi);
      usm:=Phi; tau:=1;

      for loop:=1 to 100000 do begin
        usm_:=Arcsin(Lambert_S(Phi) / _Rn2);
        db:=tau*(usm-usm_); Phi:=Phi + db;
        if Abs(db) < _Small then Break
      end;

      B:=Phi;
    end
  end
end;

const
  laea_N_POLE =	0;
  laea_S_POLE	= 1;
  laea_EQUIT	= 2;
  laea_OBLIQ	= 3;

procedure pj_authset(apa: PDoubles);
const
  P00 = 0.33333333333333333333;
  P01 = 0.17222222222222222222;
  P02 = 0.10257936507936507936;
  P10 = 0.06388888888888888888;
  P11 = 0.06640211640211640211;
  P20 = 0.01641501294219154443;
var
  es,t: Double;
begin
  es:=c_.Es;

  APA[0]:=es * P00; t:=es * es;
	APA[0]:=APA[0] + t * P01;
	APA[1]:=t * P10;	t:=t * es;
	APA[0]:=APA[0] + t * P02;
	APA[1]:=APA[1] + t * P11;
	APA[2]:=t * P20;
end;

function pj_authlat(beta: Double; APA: PDoubles): Double;
var
  t: Double;
begin
	t:=beta+beta;
  Result:=beta + APA[0]*Sin(t) + APA[1]*Sin(t+t) + APA[2]*Sin(t+t+t)
end;

function pj_qsfn(sinb: Double): Double;
const
  eps = 1.0e-7;
var
	con,e: Double;
begin
  e:=c_.E; con:=e * sinb;
  Result:=c_.Es1 * (sinb / (1 - con*con) -
    		  (0.5 / e) * Ln((1 - con) / (1 + con)))
end;

const
  laea_method = 0;

type
  tlaea = record
    elp,mode: Integer;
    sinb0,cosb0: Extended;
    sinb1,cosb1: Extended;
    _b0,_l0,_qp,_rq,_dd: Double;
    xmf,ymf: Double;
    apa: Array[0..2] of Double;
  end;

var
  laea: tlaea;

procedure laea_Init(b0,l0: Double);
begin
  with laea do
  if (_b0 <> b0) or (_l0 <> l0)
  or (mode < 0) or (elp <> c_elp) then begin

    _b0:=b0; _l0:=l0; elp:=c_elp;

    mode:=laea_OBLIQ;

    if Abs(Abs(b0)-Pi/2) < EPS10 then begin
      if b0 > 0 then mode:=laea_N_POLE
                else mode:=laea_S_POLE
    end else
    if Abs(b0) < EPS10 then mode:=laea_EQUIT;

    _qp:=pj_qsfn(1); pj_authset(@apa);

		case mode of
  laea_EQUIT:
      begin
			  _rq:=Sqrt(0.5 * _qp); _dd:=1 / _rq;
			  xmf:=1; ymf:=0.5 * _qp;
      end;
  laea_OBLIQ:
      begin
			  _rq:=Sqrt(0.5 * _qp);
        SinCos(_b0,sinb0,cosb0);
			  sinb1:=pj_qsfn(sinb0) / _qp;
        cosb1:=Sqrt(1 - sinb1 * sinb1);

        _dd:=cosb0 / (Sqrt(1 - c_.Es * sinb0 * sinb0) * _rq * cosb1);
        ymf:=_rq / _dd; xmf:=_rq * _dd;
      end;
    end
  end
end;

procedure laea_bl_to_xy(B,L: Double; out X,Y: Double);
var
	cosl,sinl: Extended;
  sinb,cosb,q,t: Double;
begin
  X:=0; Y:=0;

  SinCos(L-laea._L0,sinl,cosl);
	q:=pj_qsfn(Sin(B));

  with laea do
  if mode in [laea_N_POLE,laea_S_POLE] then begin

    if mode = laea_N_POLE then begin
      B:=Pi/2 + B; q:=_qp - q;
    end
    else begin
      B:=B - Pi/2; q:=_qp + q;
    end;

  	if Abs(B) < EPS10 then

    else
		if q >= 0 then begin
      q:=Sqrt(q); X:=q * sinl;
      if mode = laea_N_POLE then q:=-q;
			Y:=cosl * q;
    end
  end
  else begin
    sinb:=q / _qp;
    cosb:=Sqrt(1 - sinb * sinb);

    if mode = laea_EQUIT then
      q:=1 + cosb * cosl
    else
      q:=1 + sinb1 * sinb + cosb1 * cosb * cosl;

  	if Abs(q) < EPS10 then

    else begin
      q:=Sqrt(2 / q);
      X:=xmf * q * cosb * sinl;

      if mode = laea_EQUIT then q:=q * sinb else
      q:=q * (cosb1 * sinb - sinb1 * cosb * cosl);

      Y:=ymf * q
    end
  end;

  t:=X; X:=Y*c_.A; Y:=t*c_.A;
end;

procedure laea_xy_to_bl(X,Y: Double; out B,L: Double);
var
  ab,Ce,q,rho,t: Double;
  cCe,sCe: Extended;
begin
  t:=X; X:=Y/c_.A; Y:=t/c_.A;

  with laea do begin

    B:=_b0; L:=0; ab:=0;

    if mode in [laea_N_POLE,laea_S_POLE] then begin
      if mode = laea_N_POLE then y:=-y;

      q:=X*X + Y*Y;
      if Abs(q) < EPS10 then Exit;

      ab:=1 - q / _qp;
      if mode = laea_S_POLE then ab:=-ab;
    end
    else begin
      X:=X / _dd; Y:=Y * _dd; rho:=Hypot(X,Y);

      if rho < EPS10 then Exit;

      Ce:=2 * ArcSin(0.5 * rho / _rq);
      SinCos(Ce,sCe,cCe); X:=X * sCe;

  		if mode = laea_EQUIT then begin
        ab:=Y * sCe / rho; Y:=rho * cCe;
      end
      else begin
        ab:=cCe * sinb1 + Y * sCe * cosb1 / rho;
			  Y:=rho * cosb1 * cCe - Y * sinb1 * sCe;
      end;

      q:=_qp * ab
    end;

    B:=pj_authlat(ArcSin(ab),@apa);
    L:=_l0 + ArcTan2(X,Y)
  end
end;

type
  tcylps = record
    tilted: tilted_ctrl;
    _sinZ0,_C,_D: Extended;
  end;

procedure cylps_init(var ctrl: tcylps; Bn,B0,L0,Z0,D: Double);
begin
  with ctrl do begin
    tilted_Init(tilted, Bn,B0,L0);
    _sinZ0:=Sin(Z0); _C:=D + _sinZ0; _D:=D
  end
end;

// Косая перспективно-цилиндрическая проекция
var
  cylps1: tcylps; // ЦНИИГАИКа
  cylps2: tcylps; // Соловьева

procedure cylps_BL_XY(B,L: Double;
                      const ctrl: tcylps;
                      out X,Y: Double);
var
  usm, sinu,cosu, sindw,cosdw: Extended;
  cosZet,sinZet, sinA,cosA,A: Extended;
begin
  with ctrl,tilted do begin

    usm:=cross_usm(B,_k0,_alf);
    SinCos(usm,sinu,cosu);
    SinCos(L*_alf - L0_alf,sindw,cosdw);

    CosZet:=_sinu0*sinu + _cosu0*cosu*cosdw;
    SinZet:=back_sin(cosZet);

    cosA:=_cosu0*sinu - _sinu0*cosu*cosdw;
    sinA:=cosu*sindw;

    A:=Arctan2(sinA,cosA);

    X:=_C * _Rn * cosZet / (_D + sinZet);
    Y:=-_Rn * _sinZ0 * A;
  end
end;

procedure cylps_XY_BL(X,Y: Double;
                      const ctrl: tcylps;
                      out B,L: double);
var
  A,sinA,cosA,cosZet,sinZet,sinPhi: Extended;
  Xnorm,X_,tau,db,dl: Extended; i: Integer;
begin
  with ctrl,tilted do begin
    A:=-Y / (_Rn * _sinZ0);
    SinCos(A,sinA,cosA);

    Xnorm:=X / (_C * _Rn);
    cosZet:=Xnorm * _D;

    tau:=1;
    for i:=1 to 100000 do begin
      sinZet:=back_sin(cosZet);

      X_:=cosZet/(_D + sinZet);
      db:=tau * (Xnorm - X_);
      cosZet:=cosZet + db;

      if  Abs(db) < _Small then Break
    end;

    sinZet:=back_sin(cosZet);

    sinPhi:=cosZet * _sinu0 + sinZet*_cosu0*CosA;
    dl:=Arctan2(sinZet*sinA,cosZet*_cosu0 - sinZet*_sinu0*CosA);
    if dl < 0 then dl:=dl + pi2;

    L:=(L0_alf + dl)/_alf;
    B:=back_cross(sinPhi, _k0,_alf)
  end
end;

type
  tcuba_koeff = record
    Bo,Bo_sin,Lo: Double;
    Xo,Yo,Ro,RoYo,Ko: Double;
    _A,_B,_C,_D,_E,_F: Double;
  end;

var
  cuba_north: tcuba_koeff;
  cuba_south: tcuba_koeff;

procedure Init_cuba_koeff;
begin
// Cuba Norte
// proj=lcc ellps=clrk66 lat_1=22d21N lon_0=81dW
// x_0=500000 y_0=280296.016 k_0=0.99993602

// 4482-3-3_NORTE.mif
// CoordSys Earth Projection 3, 70, "m",
// -81, 22.35, 21.7, 23, 500000, 280296.016
// Bounds (-58736235.7828, -43436576.3626) (59736235.7828, 73676127.4553)

  with cuba_north do begin
    Bo:=Radian(22,21,00);
    Lo:=Radian(81,00,00);
    Bo_sin:=Sin(Bo);

    Xo  :=500000;
    Yo  :=280296.016;
    Ro  :=15519703.984;
    RoYo:=15800000;
    Ko  :=18054166.78;

    _A:=-1845.3775184;
    _B:=-0.001918667;
    _C:=-0.00002625211;
    _D:=-0.0000000007788;
    _E:=-0.000000000000609;
    _F:=-5E-17;
  end;

// Cuba Sud
// proj=lcc ellps=clrk66 lat_1=20d43'N lon_0=76d50'W
// x_0=500000 y_0=229126.939 k_0=0.99994848

// 4876-1-2_SUR.MIF
// CoordSys Earth Projection 3, 70, "m",
// -76.83333, 20.71667, 20.13333, 21.3, 500000, 229126.93900000001
// Bounds (-57522481.1956, -40922778.6437) (58522481.1956, 69483523.5621)
  with cuba_south do begin
    Bo:=Radian(20,43,00);
    Lo:=Radian(76,50,00);
    Bo_sin:=Sin(Bo);

    Xo  :=500000;
    Yo  :=229126.939;
    Ro  :=16870873.061;
    RoYo:=17100000;
    Ko  :=19211878.96;

    _A:=-1845.0355435;
    _B:=-0.001804494;
    _C:=-0.00002626412;
    _D:=-0.0000000007163;
    _E:=-0.000000000000601;
    _F:=-5E-17;
  end;
end;

procedure cuba_xy_to_bl(X,Y: Double;
                        const koeff: tcuba_koeff;
                        out B,L: Double);
const
  c2 = 0.194456126; // Coefficient for the Chevichev polinomial
  c4 = 0.000384966; // ( until 4 term)., adopted for Cuba
  c6 = 0.000001045; // and used to get the latitudinal Bi value
  c8 = 0.000000003; // from (Fi sub i). =BFClear?
var
  deltaX, deltaY: Double;
  Fi,FiPrima,deltaFi: Double;  // Meridian convergence

  Ri: Double;                  // Cone Radius in paralell i.
  Ui: Double;                  // UI Magnitud
begin
  with koeff do begin
    deltaX:=Y - Xo;
    deltaY:=RoYo - X;

    // Calculate Meridian convergence
    Fi:=ArcTan(deltaX/deltaY);

    L:=Fi/Sin(Bo) - Lo;

    // In our case the cone radius untill paralell Bi is obtained by=20
    Ri:=Abs(deltaY)/Cos(Fi);

    //Calculate with this UI =BF=BF=BF ???
    Ui:=Power(Ko/Ri, 1/Bo_sin);

    // And then isometrica latitud on the elipzoid for the expresion=20
    FiPrima:=ArcTan(Ui)-Pi/4;
    FiPrima:=2 * FiPrima;

    deltaFi:=C2*Sin(2*FiPrima) +
             C4*Sin(4*FiPrima) +
             C6*Sin(6*FiPrima) +
             C8*Sin(8*FiPrima);

    B:=FiPrima + deltaFi/180*Pi;
  end
end;

procedure cuba_bl_to_xy(B,L: Double;
                        const koeff: tcuba_koeff;
                        out X,Y: Double);
Var
  deltaL,deltaFi,R,teta: Double;
  sin,cos: Extended;
begin
  with koeff do begin
    deltaFi:=(B - Bo) * (180 / Pi) * 60;
    R:=Ro +
       _A * deltaFi +
       _B * Power(deltaFi, 2) +
       _C * Power(deltaFi, 3) +
       _D * Power(deltaFi, 4) +
       _E * Power(deltaFi, 5) +
       _F * Power(deltaFi, 6);

    deltaL:=Lo + L;
    teta:=deltaL * Bo_sin;

    SinCos(teta,sin,cos);
    Y:=Xo + R * sin;
    X:=Yo + Ro - R * cos;
  end
end;

procedure google_bl_to_xy(B,L: Double; out X,Y: Double);
var
  r,z,bmax: Double;
begin
  bmax:=89.5/180*Pi;
  if B > +bmax then B:=+bmax;
  if B < -bmax then B:=-bmax;

  r:=c_RO(0); z:=Sin(B);
  Y:=L*r; X:=0.5 * Ln((1+z)/(1-z))*r;
end;

procedure google_xy_to_bl(X,Y: Double; out B,L: Double);
var
  r,z: Double;
begin
  r:=c_RO(0); z:=X/r;
  B:=2 * ArcTan(Exp(z)) - Pi/2;
  L:=Y/r;
end;

// Поликоническая проекция ЦНИИГАиК (БСЭ)
procedure pc_33_bl_to_xy(Lat,Lon, L0: Double; out X,Y: Double);
const
  a1 = 1.0062606; // масштаб 1:80000000
  c1 = 1.003278;

  a2 = 0.9974256; // масштаб 1:55000000
  c2 = 0.9944092;

  u: Array[0..21] of Double =
    (52.81,-0.035186,0.033369,-0.016102,0.0029195,
     -0.000004,-0.0079884,0.0053805,0.035531,-12.72,
     1.5086,-1.9888,0.26824,0.12153,0.061294,-0.41449,
     0.18542,0.099726,-0.049223,-0.050489,0.0064187,
     -0.0025242);

  v: Array[0..19] of Double =
    (63.354,0.065299,2.1814,0.021535,0.0029811,
     -0.0037641,0.000832,-0.0000633,0.055675,2.7757,
     0.0274,-0.078446,0.048269,0.048869,-0.6516,
     -0.014802,-0.016348,0.001497,0.0015999,0.0011988);

var
  xk,yk, b,l, bl, b2, l2,l3: Double;
begin
  Lon:=Lon-L0;
  b:=Abs(Lat); l:=Abs(Lon);

  b2:=b*b; bl:=b*l;
  l2:=l*l; l3:=l2*l;

  xk:=v[0]+l*(v[1]+l*(v[2]+l*(v[3]+l*(v[4]+l*(v[5]+l*(v[6]+l*v[7])))))) +
      b*(v[8] + b*(v[9] + b*v[10])) +
      bl *(v[11]+b*(v[12]+b*(v[16]+b*v[18])))+
      l2*b*(v[13] + b*v[14])+
      l3*b*(v[15] + b*v[19] + l*v[17]);

  yk:=u[0]+l*(u[1]+l*(u[2]+l*(u[3]+l*(u[4]+l*u[5])))) +
      bl*(u[13] + b*(u[14] + b*(u[15] + b*(u[16] + b*(u[17] + b*u[18])))))+
      b*(u[8] + b*(u[9] + b*(u[10] + b*(u[11] + b*u[12]))))+
      l2*b*(u[19] + u[6]*b + u[20]*l + u[7]*bl + u[21]*l2);

  x:=b*xk*1E5; if Lat < 0 then x:=-x;
  y:=l*yk*1E5; if Lon < 0 then y:=-y;
end;

function Set_Ellipsoid(elp: Integer): Integer;
begin
  if elp < 0 then elp:=0;
  if elp > Ellipsoids_Max then elp:=0;
  c_:=Ellipsoids[elp]; c_elp:=elp;
  Result:=elp
end;

const
  bl_comf = 360000;

procedure BL_to_XY(b,l, lc,b1,b2: double; elp,prj: byte; var x,y: double);
begin
  if elp <> c_elp then
  Set_Ellipsoid(elp);

  case prj of
0,
1:  gk_BL_XY(b,l,lc,x,y);

2:  begin
      gk_BL_XY(b,l,lc,x,y);
      x:=x*0.9996; y:=y*0.9996;
    end;

3:  m_BL_XY(b,l,b1,x,y);

4:  begin
      conus1_Init(b1,b2,lc);
      conus1_BL_XY(b,l, x,y);
    end;

5:  ; // avia

6:  pc_BL_XY(b,l, lc, x,y);

7:  begin
      conus2_Init(b1,b2,lc);
      conus2_BL_XY(b,l, x,y);
    end;

8:  begin
      conus3_Init(b1,b2,lc);
      conus3_BL_XY(b,l, x,y);
    end;

9:  begin
      Mcross_Init(b1);
      mCross_BL_XY(b,l, x,y);
    end;

10: begin
      tilted_Init(tStereo, b1,b2,lc);
      tStereo_BL_XY(b,l, x,y);
    end;

11: begin
      pole_Init(pStereo, b1,lc);
      pStereo_BL_XY(b,l, x,y);
    end;

12: begin
      norm_Init(nStereo, b1,lc,1);
      nStereo_BL_XY(b,l, x,y);
    end;

13: begin
      norm_Init(nStereo, b1,lc,-1);
      nStereo_BL_XY(b,l, x,y);
    end;

14: begin
      norm_Init(nGnomon, b1,lc,1);
      nGnomon_BL_XY(b,l, x,y);
    end;

15: begin
      norm_Init(nGnomon, b1,lc,-1);
      nGnomon_BL_XY(b,l, x,y);
    end;

16: begin
      cross_Init(cStereo, lc);
      cStereo_BL_XY(b,l, x,y);
    end;

17: begin
      cross_Init(cGnomon, lc);
      cGnomon_BL_XY(b,l, x,y);
    end;

18: begin
      tilted_Init(tGnomon, b1,b2,lc);
      tGnomon_BL_XY(b,l, x,y);
    end;

19: begin
      pole_init(pGnomon, b1,lc);
      pGnomon_BL_XY(b,l, x,y);
    end;

20: begin
      tilted_Init(tPostel, b1,b2,lc);
      tPostel_BL_XY(b,l, x,y);
    end;

21: begin
      pole_Init(pPostel, b1,lc);
      pPostel_BL_XY(b,l, x,y);
    end;

22: begin
      norm_Init(nPostel, b1,lc,1);
      nPostel_BL_XY(b,l, x,y);
    end;

23: begin
      norm_Init(nPostel, b1,lc,-1);
      nPostel_BL_XY(b,l, x,y);
    end;

24: begin
      cross_Init(cPostel, lc);
      cPostel_BL_XY(b,l, x,y);
    end;

25: case laea_method of
  0:  begin
        laea_init(b1,lc);
        laea_bl_to_xy(b,l, x,y);
      end;
  1:  begin
        sLambert_Init(b1,lc);
        sLambert_BL_XY(b,l, x,y);
      end
    end;

26: begin
      tilted_Init(cylps1.tilted, b1,b2,lc);
      cylps_BL_XY(b,l, cylps1, x,y);
    end;

27: begin
      tilted_Init(cylps2.tilted, b1,b2,lc);
      cylps_BL_XY(b,l, cylps2, x,y);
    end;

28: begin
      lcc_Init(lc,b1,b2,c_B2,1);
      lcc_BL_XY(b,l, x,y)
    end;

29: begin
      x:=b * (180/Pi) * bl_comf;
      y:=l * (180/Pi) * bl_comf
    end;

30: cuba_bl_to_xy(b,l, cuba_north, x,y);
31: cuba_bl_to_xy(b,l, cuba_south, x,y);

32: google_bl_to_xy(b,l, x,y);

  end
end;

procedure XY_to_BL(x,y, lc,b1,b2: double; elp,prj: byte; var b,l: double);
begin
  if elp <> c_elp then
  Set_Ellipsoid(elp);

  case prj of
0,
1:  gk_XY_BL(x,y,lc,b,l);

2:  begin
      x:=x/0.9996; y:=y/0.9996;
      gk_XY_BL(x,y,lc,b,l);
    end;

3:  m_XY_BL(x,y,b1,b,l);

4:  begin
      conus1_Init(b1,b2,lc);
      conus1_XY_BL(x,y, b,l);
    end;

5:  ; // avia

6:  pc_XY_BL(x,y, lc, b,l);

7:  begin
      conus2_Init(b1,b2,lc);
      conus2_XY_BL(x,y, b,l);
    end;

8:  begin
      conus3_Init(b1,b2,lc);
      conus3_XY_BL(x,y, b,l);
    end;

9:  begin
      Mcross_Init(b1);
      mCross_XY_BL(x,y, b,l);
    end;

10: begin
      tilted_Init(tStereo, b1,b2,lc);
      tStereo_XY_BL(x,y, b,l);
    end;

11: begin
      pole_Init(pStereo, b1,lc);
      pStereo_XY_BL(x,y, b,l);
    end;

12: begin
      norm_Init(nStereo, b1,lc,1);
      nStereo_XY_BL(x,y, b,l);
    end;

13: begin
      norm_Init(nStereo, b1,lc,-1);
      nStereo_XY_BL(x,y, b,l);
    end;

14: begin
      norm_Init(nGnomon, b1,lc,1);
      nGnomon_XY_BL(x,y, b,l);
    end;

15: begin
      norm_Init(nGnomon, b1,lc,-1);
      nGnomon_XY_BL(x,y, b,l);
    end;

16: begin
      cross_Init(cStereo, lc);
      cStereo_XY_BL(x,y, b,l);
    end;

17: begin
      cross_Init(cGnomon, lc);
      cGnomon_XY_BL(x,y, b,l);
    end;

18: begin
      tilted_Init(tGnomon, b1,b2,lc);
      tGnomon_XY_BL(x,y, b,l);
    end;

19: begin
      pole_init(pGnomon, b1,lc);
      pGnomon_XY_BL(x,y, b,l);
    end;

20: begin
      tilted_Init(tPostel, b1,b2,lc);
      tPostel_XY_BL(x,y, b,l);
    end;

21: begin
      pole_Init(pPostel, b1,lc);
      pPostel_XY_BL(x,y, b,l);
    end;

22: begin
      norm_Init(nPostel, b1,lc,1);
      nPostel_XY_BL(x,y, b,l);
    end;

23: begin
      norm_Init(nPostel, b1,lc,-1);
      nPostel_XY_BL(x,y, b,l);
    end;

24: begin
      cross_Init(cPostel, lc);
      cPostel_XY_BL(x,y, b,l);
    end;

25: case laea_method of
  0:  begin
        laea_init(b1,lc);
        laea_xy_to_bl(x,y, b,l)
      end;
  1:  begin
        sLambert_Init(b1,lc);
        sLambert_XY_BL(x,y, b,l);
      end
    end;

26: begin
      tilted_Init(cylps1.tilted, b1,b2,lc);
      cylps_XY_BL(x,y, cylps1, b,l);
    end;

27: begin
      tilted_Init(cylps2.tilted, b1,b2,lc);
      cylps_XY_BL(x,y, cylps2, b,l);
    end;

28:  begin
      lcc_Init(lc,b1,b2,c_B2,1);
      lcc_XY_BL(x,y, b,l)
    end;

29: begin
      b:=b_Range(x / bl_comf / (180/Pi));
      l:=y / bl_comf / (180/Pi);
    end;

30: cuba_xy_to_bl(x,y, cuba_north, b,l);
31: cuba_xy_to_bl(x,y, cuba_south, b,l);

32: google_xy_to_bl(x,y, b,l);

  end
end;

function _fi(Grad: Integer): Double;
begin
  Result:=Grad/180*Pi
end;

procedure test_conus5;
var
  x,y,b,l,b1,l1,k1,k2,x1,y1,x2,y2: Double;
begin
  Set_Ellipsoid(9);

  lcc_Init(95/180*Pi,
           40/180*Pi,
           20/180*Pi,
           60/180*Pi,1);

  l:=74.817342/180*Pi;
  b:=63.139412/180*Pi;

  lcc_BL_XY(b,l, x,y);

  k1:=y / -643.0683;
  k2:=x / 1618.2487;

  x:=x/1852.6;
  y:=y/1852.6;

  b:=(63 + (08 + 21.8832/60)/60) / 180*Pi;
  l:=(74 + (49 + 02.4312/60)/60) / 180*Pi;

  lcc_BL_XY(b,l, x,y);

  k1:=x/2604318.864015;
  k2:=y/-1034918.128351;

  x:=x/1852.6;
  y:=y/1852.6;

y:=-643.06829*1852.6;
x:=1618.24872*1852.6;

  Set_Ellipsoid(6); // clrk66

// Cuba Norte
// proj=lcc ellps=clrk66 lat_1=22d21N lon_0=81dW
// x_0=500000 y_0=280296.016 k_0=0.99993602

  b:=Radian(22,21,0);
  l:=Radian(-81,0,0);
  lcc_Init(l,b,b,b,0.99993602);

  b:=23/180*Pi; l:=-80.5/180*Pi;

  cuba_bl_to_xy(b,l, cuba_north, x1,y1);
  lcc_BL_XY(b,l, x2,y2);
  x2:=x2 + 280296.016;
  y2:=y2 + 500000;

  k1:=x1/x2; k2:=y1/y2;

// Cuba Sud
// proj=lcc ellps=clrk66 lat_1=20d43'N lon_0=76d50'W
// x_0=500000 y_0=229126.939 k_0=0.99994848

  b:=Radian(20,43,00);
  l:=-Radian(76,50,00);
  lcc_Init(l,b,b,b,0.99994848);

  b:=21/180*Pi; l:=-77/180*Pi;

  cuba_bl_to_xy(b,l, cuba_south, x1,y1);
  lcc_BL_XY(b,l, x2,y2);
  x2:=x2 + 229126.939;
  y2:=y2 + 500000;

  k1:=x1/x2; k2:=y1/y2;

  lcc_init(-Radian(76,50,0),
           Radian(20,43,0),
           20.13333/180*Pi,
           21.3/180*Pi,1);

  l:=-76.364225/180*Pi;
  b:=20.924807/180*Pi;
  lcc_BL_XY(b,l, x1,y1);
  x1:=x1 + 229126.939;
  y1:=y1 + 500000;

  k1:=y1/548795.92;
  k2:=x1/252239.17;

  cuba_bl_to_xy(b,l, cuba_south, x1,y1);
  k1:=y1/548795.92;
  k2:=x1/252239.17;
end;

procedure laea_slambert_test;
begin
end;

procedure xbl_init;
var
  f80,f60,f25,f45,f75, x,y,b,l: Double;
begin
  Ellipsoids_Init; c_Rn:=0; c_elp:=-1;

  f80:=_fi(80); f60:=_fi(60);
  f25:=_fi(25); f45:=_fi(45);
  f75:=_fi(75);

  Fillchar(conus1,Sizeof(conus1),0);
  conus1_Init(1.11,1.22,0.5);

  Fillchar(conus2,Sizeof(conus2),0);
  conus2_Init(1.11,1.22,0.5);

  Fillchar(conus3,Sizeof(conus3),0);
  conus3_Init(1.11,1.22,0.5);

  Fillchar(conus4,Sizeof(conus4),0);
  conus4_Init(1.11,1.22,0.5);

  Fillchar(lcc,Sizeof(lcc),0);
  lcc_Init(_fi(90),_fi(40),_fi(20),_fi(60),1);

  Fillchar(Mcross,Sizeof(Mcross),0);
  Mcross_Init(f80);

  Fillchar(tStereo,Sizeof(tStereo),0);
  tilted_Init(tStereo, f80,f60,f60);

  Fillchar(pStereo,Sizeof(pStereo),0);
  pole_init(pStereo, f80,f60);

  Fillchar(nStereo,Sizeof(nStereo),0);
  norm_Init(nStereo, f80,f60,1);

  Fillchar(mStereo,Sizeof(mStereo),0);
  norm_Init(mStereo, f80,f60,-1);

  Fillchar(nGnomon,Sizeof(nGnomon),0);
  norm_Init(nGnomon, f80,f60,1);

  Fillchar(mGnomon,Sizeof(mGnomon),0);
  norm_Init(mGnomon, f80,f60,-1);

  Fillchar(cStereo,Sizeof(cStereo),0);
  cross_Init(cStereo, f60);

  Fillchar(cGnomon,Sizeof(cGnomon),0);
  cross_Init(cGnomon, f60);

  Fillchar(tGnomon,Sizeof(tGnomon),0);
  tilted_Init(tGnomon, f80,f60,f60);

  Fillchar(pGnomon,Sizeof(pGnomon),0);
  pole_init(pGnomon, f80,f60);

  Fillchar(tPostel,Sizeof(tPostel),0);
  tilted_Init(tPostel, f80,f60,f60);

  Fillchar(pPostel,Sizeof(pPostel),0);
  pole_init(pPostel, f80,f60);

  Fillchar(nPostel,Sizeof(nPostel),0);
  norm_Init(nPostel, f80,f60,1);

  Fillchar(mPostel,Sizeof(mPostel),0);
  norm_Init(mPostel, f80,f60,-1);

  Fillchar(cPostel,Sizeof(cPostel),0);
  cross_Init(cPostel,f60);

  Fillchar(sLambert,Sizeof(sLambert),0);
  sLambert_Init(f80,f60);

  Fillchar(cylps1,Sizeof(cylps1),0); // ЦНИИГАИКа
  cylps_init(cylps1,f60,f25,-f80,f80,3);

  Fillchar(cylps2,Sizeof(cylps2),0); // Соловьева
  cylps_init(cylps2,f60,f75,-f80,f45,1);

  Init_cuba_koeff;

  Fillchar(laea,Sizeof(laea),0);
  laea.mode:=-1
end;

function Set_Radius(Rn: Double): Double;
begin
  if Abs(Rn-c_Rn) > 1 then

  if Rn >= 6000000 then
  if Rn <= 7000000 then begin
    c_Rn:=Rn; xbl_init;
  end;

  Result:=c_Rn
end;

procedure Set_B2(B2: Double);
begin
  c_B2:=B2
end;

initialization xbl_Init;

end.