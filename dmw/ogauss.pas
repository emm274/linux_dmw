unit ogauss; interface {$H-}

uses
  Classes,Math,otypes,xline;

const
  dmScale_Max = 11;

  dmScale: array[0..11] of Integer =
  (0,1000000,500000,200000,100000,50000,25000,10000,5000,2000,1000,500);

type
  plg_Transit = ^lg_Transit;
  lg_Transit = record
    s: tsys; l,g: GOrient;
  end;

  PCoords = ^TCoords;
  TCoords = record
    pps,prj,elp: Integer;
    dat: TDatum; xy: GOrient
  end;

  PAffine = ^TAffine;
  TAffine = record
    a0,ax,ay, b0,bx,by: Double;
  end;

  PAffines = ^TAffines;
  TAffines = Array[0..255] of TAffine;

  AB_Transit = object
    a0,ax,ay, b0,bx,by: Double;
    function Transit(x,y: double): TGauss;
    function iTransit(x,y: double): TPoint;
    function GPoly_Transit(G: PGPoly; Count: Integer): Double;
    procedure LoadFrom(T: AB_Transit);
    procedure Reset;
  end;

  AB_Matrix = object(AB_Transit)
    function Open(a,b: PGPoly): Boolean;

    function ab_Open(a,b: PGPoly; n: Integer): Boolean;

    function xOpen(const ip1,ip2,ip3: TGauss;
                   const op1,op2,op3: TGauss): Boolean;

    function iOpen(const p1,p2,p3: TPoint;
                   const q1,q2,q3: TPoint): Boolean;

    function Open_Min_Sqr(ip,op: PGPoly; n: Integer): Boolean;

    function Open_lg(const lg: lg_Transit): Boolean;

    procedure Get_Matrix(out T: Real3x3);
    procedure Set_Matrix(const T: Real3x3);

    function Get_affine: TAffine;
  end;

  ab_tri = record a,b,c: double end;

  AB_Quadro = object
    bg: GOrient; t: array[0..3] of ab_tri;
    procedure Assign(var av,bv: GORient);
    function Transit(x,y: double): tgauss;
  end;

  AB_Grid = object okay: Boolean;
    bg: gOrient; q: Real2x2; p,z: tgauss;
    function Assign(var av,bv: gORient): Boolean;
    function Transit(x,y: double): tgauss;
  end;

  AB_Orient = object
    sys: tsys;
    zoneX: Double;
    zoneY: Double;

    ag,bg: GOrient;
    q_ab,q_ba: AB_Grid;
    x_ab,x_ba: AB_Matrix;

    xy_s,gk_s: tsys;

    x_fast,x_init,x_info: Boolean;

    procedure Init;
    function Equal_ab(var T: AB_Orient): Boolean;
    function This_lg(const T: lg_Transit): Boolean;

    procedure Get_lg_tr(out Tr: Real3x3);
    procedure Get_lz_tr(out Tr: Real3x3);

    procedure LoadFrom(var T: AB_Orient);
    procedure Orient(const av,bv: LOrient);

    function Assign(const T: lg_Transit;
                    p_lc: double): Boolean;

    procedure Set_origin(const g: TGeoPoint);

    procedure Change_Transit(const sys2: tsys; is_sm: Boolean);

    function is_gk_prj: int;

    function lg_res: double;
    function lg_ppm: double;
    function prj_res: double;

    function Forw(x,y: Double): TGauss;
    function Back(x,y: Double): TGauss;

    procedure A_to_B(const a: TPoint; out b: TPoint);
    procedure B_to_A(const a: TPoint; out b: TPoint);

    procedure L_to_G(const l: TPoint; out g: tgauss);
    procedure G_to_L(const g: tgauss; out l: TPoint);

    procedure G_pps_L(const g: tgauss; pps: int; out p: TPoint);

    procedure P_to_G(const p: tgauss; out g: tgauss);
    procedure G_to_P(const g: tgauss; out p: tgauss);

    procedure L_to_bl(const p: TPoint; out r: tgeoid);
    procedure L_to_ms(const p: TPoint; out r: tgeoid);

    function L_to_R(const l: TPoint; out r: tgeoid): Integer;
    procedure R_to_L(const r: tgeoid; out l: TPoint);

    procedure L_to_Z(const l: TPoint; out g: tgauss);
    procedure Z_to_L(const g: tgauss; out l: TPoint);

    function L_to_prj(const l: TPoint; elp: int): tgauss;

    function z_xy_to_pix(const g: tgauss): tgauss;

    procedure WGS_XY(B,L: Double; out g: tgauss);

    procedure BL_XY(B,L: Double; out g: tgauss);
    procedure XY_BL(const g: tgauss; out r: tgeoid);

    procedure z_BL_XY(const r: tgeoid; out g: tgauss);
    procedure z_XY_BL(const g: tgauss; out r: tgeoid);

    function dm_sys_xy(const p: Tpoint; const s: tsys): TGauss;
    function xy_sys_dm(const g: tgauss; const s: tsys): TPoint;

    procedure dm_to_xy(const p: TPoint; out g: tgauss);
    procedure xy_to_dm(const g: tgauss; out p: TPoint);

    procedure L_to_Geo(const p: TPoint; out x: xgeoid);

    function g_to_geo(x,y: Double): XGeoid;

    procedure x_L_to_R(const p: TPoint; out x: xgeoid);
    procedure x_G_to_R(const g: tgauss; out x: xgeoid);
    procedure x_R_to_Z(const x: xgeoid; out g: tgauss);
    procedure x_R_to_G(const x: xgeoid; out g: tgauss);
    procedure x_R_to_L(const x: xgeoid; out p: TPoint);
    procedure z_R_to_L(const x: xgeoid; out p: TPoint);
    procedure xxx_to_L(const x: xgeoid; out p: TPoint);

    function x_R_to_P(const x: xgeoid): TGauss;

    procedure wgs_loc_win(W: PGPoly; L: PLPoly);

    function Align_longitude(l: Double): Double;

    function lg_Delta: double;
    function lg_Delta1: double;
    procedure lg_Quadro;

    function q_Prepare(d: double): Boolean;
    function g_Prepare(n: Integer): double;
    function translate_Prepare: double;
    function x_Prepare: double;

    procedure c_Prepare(const c: tgauss);
    procedure t_Prepare(var T: AB_Orient);

    function sys_dist(const a,b: TPoint; elp: int): double;

    function xy_Dist(const a,b: TPoint; out f: Double): double;

    function gLength(const a,b: tgauss; out f: double): double;
    function xLength(const a,b: TPoint; out f: double): double;
    function xProject(const p: TPoint; r,f: double): TPoint;

    function p_rf_p(const p: TPoint; r,f: double;
                    scale,ed: Integer): TPoint;

    function step_rf(const p1,p2: TPoint; r,f: double;
                     scale,ed: Integer): TPoint;

    function lpoly_Length(lp: PLPoly; lp_n: int): double;

    function lpoly_Length3d(lp: PLPoly; hp: Pintegers;
                            lp_n, z_res: int): TGauss;

    function lpoly_Square(lp: PLPoly; lp_n: int): double;

    function Poly_Length(lp: PLLine): double;

    function xyz_Length(lp: PLLine; hp: Pintegers;
                        z_res: Integer): TGauss;

    function Poly_Square(lp: PLLine): double;
    function Curve_Length(lp: PLLine; out s: double): double;

    function poly_gk(lp: PLPoly; lp_n: int): Boolean;

    function set_gk_s(const p: TPoint): Boolean;
    function s_L_to_G(const p: TPoint): TGauss;

    function Get_pps_Bound(out lt,rb: tgauss): Integer;

    procedure Get_Gauss_Bound(const l_lt,l_rb: TPoint;
                              out g_lt,g_rb: tgauss);

    procedure Get_Local_Bound(const g_lt,g_rb: tgauss;
                              out l_lt,l_rb: TPoint);

    function Get_grect(lp: PLLine; fr: PGPoly): bool;

    procedure Get_Link_Point(Ind: Integer; out V: TPoint);

    function Geoid_Port(const lt,rb: tgauss;
                        sc_ind,scale: Integer;
                        lp: PLLine; lp_Max: Integer;
                        link: Boolean): Integer;

    procedure Geoid_Latitude(lp: PLLine; b,l1,l2,dl: double);
    procedure Geoid_Longitude(lp: PLLine; b1,b2,db,l: double);

    function Insert_bl(lp: PLLine; const g: tgeoid): Integer;

    procedure Move_bl_Port(lp: PLLine; const a,b: TPoint);

    function Get_Poly_frame(lp: PLLine; lp_Max: Integer): Integer;

    procedure sxf_ramka(lp: PLLine);
  end;

function ed_k(scale,ed: Integer): Double;

function sys_Datum7(const sys: tsys): TDatum7;

function ru_sys(y: Double): tsys;
function ru_list(l: Double): tsys;

function Latitude(ch: char): double;

procedure Radian_to_Second(b,l: double; var x,y: longint);
procedure Second_to_Radian(x,y: longint; var b,l: double);

procedure s_BL_BL(const r1: tgeoid; const s1,s2: tsys; out r2: tgeoid);

procedure BL_WGS(const r1: tgeoid; const s1: tsys; out r2: tgeoid);
procedure WGS_BL(const r1: tgeoid; const s2: tsys; out r2: tgeoid);

function lg_Equal(const T1,T2: lg_Transit): Boolean;
function lg_Rect(var lg: lg_Transit): bool;

function Is_msk_sys(const s: tsys): Boolean;

procedure msk_XY_BL(x,y: Double; const s: tsys; out b,l: double);
procedure msk_BL_XY(b,l: Double; const s: tsys; out x,y: double);

procedure lc_XY_BL(x,y, lc: Double; const s: tsys; out b,l: double);
procedure lc_BL_XY(b,l, lc: Double; const s: tsys; out x,y: double);

function prj_outside(x,y: Double; const s: tsys): xgeoid;
function prj_inside(const x: xgeoid; const s: tsys): tgauss;

function prj_to_xy(const r: xgeoid;
                   const sys: tsys;
                   out g: TGauss): Integer;

function prj_to_lg(x,y: Double; const s1,s2: tsys): TGauss;
function prj_to_prj(x,y: Double; const s1,s2: tsys): TGauss;

function x_XY_BL(const g: tgauss; const sys: tsys): tgeoid;
function x_BL_XY(const r: tgeoid; const sys: tsys): tgauss;

function ab_Project(x,y: Double; const T1,T2: ab_Orient): TGauss;
function AB_Backup(x,y: Double; const T1,T2: ab_Orient): TPoint;

function Gauss_Longitude(y: Double): Double;

function zone_Projection(XY: PGPoly;
                         elp,prj: Integer;
                         BL: PGPoly): Boolean;

function GPoly_normalize(G: PGPoly; n: Integer): Real3x3;

function GPoly_Equal(G1,G2: PGPoly; n: Integer;
                     Eps: Double): Boolean;

function GLine_Equal(G1,G2: PGLine; Eps: Double): Boolean;

function gLongitude(y: double): double;
function cLongitude(G: PGPoly): double;
function xLongitude(G: PGPoly): double;

function prj_Longitude(p_lc,lc: Double;
                       prj: Integer): Double;

function cLatitude(G: PGPoly): double;
function minLatitude(G: PGPoly): double;

procedure xy_ru_bl(x,y: Double; out b,l: Double); stdcall;
procedure bl_ru_xy(b,l: Double; out x,y: Double); stdcall;

procedure xy_wgs_gk(x1,y1: Double; out x2,y2: Double);
procedure bl_wgs_gk(b1,l1: Double; out b2,l2: Double);

function dm_Nom_Scan(nom: string; _Double: Boolean;
                     out g: GOrient; out sc: Integer): string;

function sxf_Nom(scale: longint; nom: string): string;
function dm_Nom_Trunc(nom: string; _Double: Boolean): string;
function dm_Nom_Scale(nom: string; _Double: Boolean): Integer;

function nom200(lat,lon: double): string;

procedure Init_lg_Transit(out lg: lg_Transit);

procedure Project_lg_Transit(var lg: lg_Transit);

function Restore_lg_Transit(var lg: lg_Transit;
                            fr: PGPoly; n: Integer): Integer;

function East_lg_Transit(var a: lg_Transit;
                         const b: lg_Transit): Integer;

function wgs_lg_Transit(const lg: lg_Transit; Fr: PGPoly): Boolean;

function lg_get_scale(const lg: lg_Transit): int;

procedure Link_lg_prj(var a: tsys; const b: tsys);

procedure Link_lg_Transit(var a: lg_Transit;
                          const b: lg_Transit);

function prj_xy_to_x(const g: tgauss; const sys: tsys): xgeoid;

function prj_x_to_x(const g: xgeoid; const sys: tsys): xgeoid;

function lg_Precision(const T: lg_Transit): Double;

function lg_Contains_Point(const lg: lg_Transit;
                           const x: xgeoid): Boolean;

procedure Update_mm_transit(var lg: lg_Transit; x,y,w,h: double);
procedure Sort_lg_Transit(l,g: PGPoly);

function Local_lg_Transit(var lg: lg_Transit;
                          L: PLPoly; ppm: Float): Float;

function Gauss_to_Local(G: PGPoly; L: PLPoly; ppm: Float): Float;

procedure Max_Project_Bound(w,h: Double; const Tr: Real3x3;
                            Fr: PGPoly; out lt,rb: tgauss);

function xGeoid_Dist(const g1,g2: XGeoid): Double;

function gk_to_lc(x,y: Double; elp: int; var lc: Double): Boolean;
function utm_to_lc(x,y: Double; elp: int; var lc: Double): Boolean;

function gk_projection(x,y: Double; elp: Integer;
                       var lc: Double): Boolean;

function utm_projection(x,y: Double; elp: Integer;
                        var lc: Double): Boolean;

function sys_projection(var sys: tsys; x,y: Double): Integer;
function rsw_projection(var sys: tsys; x,y: Double): int;

function dm_Scale_Index(sc: Integer): Integer;

procedure Get_GBound_Dists(D: PDoubles; G: PGPoly);
procedure Get_GBound_Min_Max(G: PGPoly; out min_d,max_d: Double);
function Is_GBound(G: PGPoly; R: Double): Boolean;

function StrSys(Str: PChar; ed: Integer;
                const Sys: tsys): PChar;

function geoToken(Str: PChar; out sys: tsys): Boolean;
function mifToken(Str: PChar; out Sys: tsys): Integer;
function sysToken(Str: PChar; out Sys: tsys): Integer;

function lg_LoadFrom(out lg: lg_Transit; Path: PChar): Boolean;

implementation

uses
  Sysutils,
  convert,ofiles,
  xy,xpoly,xpoly1,
  xcurve,xbl_use,xmath;

function ed_k(scale,ed: Integer): Double;
begin
  Result:=1;
  case ed of
1:  Result:=1000;
2:  Result:=1852.6;
3:  Result:=185.26;
4:  Result:=scale/1000
  end;
end;

procedure AB_Transit.Reset;
begin
  a0:=0; ax:=1; ay:=0; b0:=0; bx:=0; by:=1
end;

function AB_Transit.Transit(x,y: double): tgauss;
begin
  Result.x:=a0 + ax*x + ay*y;
  Result.y:=b0 + bx*x + by*y
end;

function AB_Transit.iTransit(x,y: double): TPoint;
begin
  Result.x:=Round(a0 + ax*x + ay*y);
  Result.y:=Round(b0 + bx*x + by*y)
end;

function AB_Transit.GPoly_Transit(G: PGPoly; Count: Integer): Double;
var
  I: Integer; len1,len2: Double;
  a1,a2,b1,b2: TGauss;
begin
  Result:=0; len1:=0; len2:=0;

  for I:=0 to Count-1 do begin
    a1:=a2; b1:=b2; a2:=G[I];
    b2:=Transit(a2.x,a2.y); G[I]:=b2;

    if I > 0 then begin
      len1:=len1 + Gauss_Dist(a1,a2);
      len2:=len2 + Gauss_Dist(b1,b2);
    end;
  end;

  if len1 > 0 then
  Result:=len2/len1
end;

procedure AB_Transit.LoadFrom(T: AB_Transit);
begin
  Reset;
  a0:=T.a0; ax:=T.ax; ay:=T.ay;
  b0:=T.b0; bx:=T.bx; by:=T.by
end;

function AB_Matrix.Open(a,b: PGPoly): Boolean;
begin
  Result:=xOpen(a[0],a[1],a[2], b[0],b[1],b[2])
end;

function AB_Matrix.ab_Open(a,b: PGPoly; n: Integer): Boolean;
var
  f1,f2,len1,len2: Double; T: Real3x3;
begin
  Result:=false; Reset;

  if n > 3 then
    Result:=Open_Min_Sqr(a,b, n)
  else
  if n = 3 then
    Result:=Open(a,b)
  else
  if n > 0 then begin
    a0:=b[0].x-a[0].x;
    b0:=b[0].y-a[0].y;

    if n = 2 then begin
      len1:=Gauss_Dist(a[0],a[1]);
      len2:=Gauss_Dist(b[0],b[1]);

      if (len1 > 1) and (len2 > 1) then begin
        f1:=ArcTan2(a[1].y-a[0].y,a[1].x-a[0].x);
        f2:=ArcTan2(b[1].y-b[0].y,b[1].x-b[0].x);

        Begin_3x3(T,-a[0].x,-a[0].y);
        t_Scale_3x3(T,len2/len1);
        fi_Rotate_3x3(T,f1-f2);
        Set_Matrix(T);

        a0:=a0 + b[0].x;
        b0:=b0 + b[0].y; Result:=true
      end
    end
  end
end;

function AB_Matrix.xOpen(const ip1,ip2,ip3: tgauss;
                         const op1,op2,op3: tgauss): Boolean;
type
  float3 = array[1..3] of double;

function Mult(a,b: float3; d: double): double;
begin
  Mult:=(a[1]*b[1] + a[2]*b[2] + a[3]*b[3]) / d
end;

var
  a,b,c,x,y: float3; d: double;
begin
  Result:=false; Reset;

  a[1]:=ip2.x * ip3.y - ip3.x * ip2.y; b[1]:=ip2.y-ip3.y; c[1]:=ip3.x-ip2.x;
  a[2]:=ip3.x * ip1.y - ip1.x * ip3.y; b[2]:=ip3.y-ip1.y; c[2]:=ip1.x-ip3.x;
  a[3]:=ip1.x * ip2.y - ip2.x * ip1.y; b[3]:=ip1.y-ip2.y; c[3]:=ip2.x-ip1.x;

  x[1]:=op1.x; x[2]:=op2.x; x[3]:=op3.x;
  y[1]:=op1.y; y[2]:=op2.y; y[3]:=op3.y; d:=a[1]+a[2]+a[3];

  if d <> 0 then begin
    a0:=Mult(a,x,d); b0:=Mult(a,y,d);
    ax:=Mult(b,x,d); bx:=Mult(b,y,d);
    ay:=Mult(c,x,d); by:=Mult(c,y,d);
    Result:=true
  end
end;

function AB_Matrix.iOpen(const p1,p2,p3: TPoint;
                         const q1,q2,q3: TPoint): Boolean;
var
  ip1,ip2,ip3, op1,op2,op3: tgauss;
begin
  ip1.x:=p1.x; ip1.y:=p1.y; op1.x:=q1.x; op1.y:=q1.y;
  ip2.x:=p2.x; ip2.y:=p2.y; op2.x:=q2.x; op2.y:=q2.y;
  ip3.x:=p3.x; ip3.y:=p3.y; op3.x:=q3.x; op3.y:=q3.y;

  Result:=xOpen(ip1,ip2,ip3, op1,op2,op3)
end;

function AB_Matrix.Open_Min_Sqr(ip,op: PGPoly; n: Integer): Boolean;
var
  i,loop: Integer; ix,iy,ox,oy,
  A,B,C,D,E,F,G,H,T, t0,tx,ty: double;
  FA_BC, EA_BB, BD_HA, CB_FA: double;
begin
  Result:=n > 0; Reset;

  if n = 3 then
    Result:=xOpen(ip[0],ip[1],ip[2], op[0],op[1],op[2])
  else
  for loop:=1 to 2 do begin

    A:=0; B:=0; C:=0; D:=0;
    E:=0; F:=0; H:=0; G:=0; T:=0;

    for i:=0 to n-1 do begin

      with ip[i] do begin ix:=x; iy:=y end;
      with op[i] do begin ox:=x; oy:=y end;

      if loop = 2 then ox:=oy;

      A:= A + ix*ix;
      B:= B + ix*iy;
      C:= C + ix*1;
      E:= E + iy*iy;
      F:= F + iy*1;
      G:= G + 1*1;
      D:= D - ix*ox;
      H:= H - iy*ox;
      T:= T - ox
    end;

    FA_BC:=F*A-B*C; EA_BB:=E*A-B*B;
    BD_HA:=B*D-H*A; CB_FA:=C*B-F*A;

    t0:=FA_BC*CB_FA + EA_BB*(-C*C+G*A);

    if (t0 = 0) or (EA_BB = 0) or (A = 0) then
      Break
    else begin
      t0:=((A*E-B*B)*(D*C-T*A) + BD_HA*CB_FA) / t0;
      ty:=(BD_HA - t0*FA_BC) / EA_BB;
      tx:=-(D + C*t0 + B*ty) / A;

      if loop = 1 then
        begin a0:=t0; ax:=tx; ay:=ty end
      else
        begin b0:=t0; bx:=tx; by:=ty end
    end
  end
end;

function AB_Matrix.Open_lg(const lg: lg_Transit): Boolean;
var
  i: Integer; g: GOrient;
begin
  g:=lg.g;

  if lg.s.pps = 1 then
  for i:=0 to 3 do with g[i] do
  prj_BL_XY(x,y,lg.s,x,y);

  Result:=Open(@lg.l,@g)
end;

procedure AB_Matrix.Get_Matrix(out T: Real3x3);
begin
  T[1,1]:=ax; T[1,2]:=ay; T[1,3]:=a0;
  T[2,1]:=bx; T[2,2]:=by; T[2,3]:=b0;
  T[3,1]:=0;  T[3,2]:=0;  T[3,3]:=1
end;

procedure AB_Matrix.Set_Matrix(const T: Real3x3);
begin
  ax:=T[1,1]; ay:=T[1,2]; a0:=T[1,3];
  bx:=T[2,1]; by:=T[2,2]; b0:=T[2,3];
end;

function AB_Matrix.Get_affine: TAffine;
var
  t: TAffine;
begin
  t.a0:=a0; t.ax:=ax; t.ay:=ay;
  t.b0:=b0; t.bx:=bx; t.by:=by;
  Result:=t
end;

procedure Tri_Init(const a,b: tgauss; var tri: ab_tri);
var
  ax,ay: double;
begin
  ax:=a.x; ay:=a.y; tri.a:=ax*b.y-ay*b.x;
  tri.b:=a.y-b.y; tri.c:=b.x-a.x
end;

function Tri_Square(x,y: double; var tri: ab_tri): double;
begin
  with tri do Tri_Square:=a+b*x+c*y
end;

procedure AB_Quadro.Assign(var av,bv: GORient);
var
  i: Integer;
begin
  av[4]:=av[0]; for i:=0 to 3 do
  Tri_Init(av[i],av[i+1],t[i]); bg:=bv
end;

function AB_Quadro.Transit(x,y: double): tgauss;
var
  m0,m1,l0,l1, a,b,c,d: double;
begin
  m1:=Tri_Square(x,y,t[0]); m0:=Tri_Square(x,y,t[2]);
  l1:=Tri_Square(x,y,t[3]); l0:=Tri_Square(x,y,t[1]);

  m0:=m0/(m0+m1); m1:=1-m0; l0:=l0/(l0+l1); l1:=1-l0;

  a:=m0*l0; b:=m0*l1; c:=m1*l1; d:=m1*l0;

  Result.x:=a*bg[0].x+b*bg[1].x+c*bg[2].x+d*bg[3].x;
  Result.y:=a*bg[0].y+b*bg[1].y+c*bg[2].y+d*bg[3].y
end;

function AB_Grid.Assign(var av,bv: GORient): Boolean;
var
  x,y: double; ag: gOrient;
begin
  ag:=av; bg:=bv;
  Sort_lg_Transit(@ag,@bg);

  q[1,1]:=ag[1].x-ag[0].x; q[1,2]:=ag[3].x-ag[0].x;
  q[2,1]:=ag[1].y-ag[0].y; q[2,2]:=ag[3].y-ag[0].y;

  okay:=false; if Minus_2x2(q) then begin
    x:=ag[2].x-ag[0].x; y:=ag[2].y-ag[0].y;
    z.x:=q[1,1]*x+q[1,2]*y-1; z.y:=q[2,1]*x+q[2,2]*y-1;
    if Abs(1+z.x) > Small then if Abs(1+z.y) > Small then
    okay:=true;
  end;

  p:=ag[0]; Result:=okay
end;

function AB_Grid.Transit(x,y: double): tgauss;
var
  h,v,s,t, a,b,c,d, fx,fy,det: double; i: Integer;
begin
  Result.x:=x; Result.y:=y;

  if okay then begin
    x:=x-p.x; y:=y-p.y;

    h:=q[1,1]*x + q[1,2]*y;
    v:=q[2,1]*x + q[2,2]*y;

    t:=h/(1+z.x); s:=v/(1+z.y);

    for i:=1 to 16 do begin
      a:=1+s*z.x; b:=t*z.x;
      c:=s*z.y;   d:=1+t*z.y;

      fx:=t*a - h; fy:=s*d - v;

      if Abs(fx)+Abs(fy) < Small/100 then Break;

      det:=Sqr(a)+Sqr(d);

      if det < Small then begin
        t:=t+Small; s:=s+Small;
      end
      else begin
        t:=t - (d*fx-b*fy)/det;
        s:=s - (a*fy-c*fx)/det
      end
    end;

    a:=(1-t)*(1-s); b:=t*(1-s); c:=t*s; d:=(1-t)*s;

    Result.x:=a*bg[0].x+b*bg[1].x+c*bg[2].x+d*bg[3].x;
    Result.y:=a*bg[0].y+b*bg[1].y+c*bg[2].y+d*bg[3].y
  end
end;

procedure AB_Orient.Init;
begin
  x_fast:=true; x_init:=false;
  x_ab.Reset; x_ba.Reset; sys:=sys_nil;
  xy_s:=sys_nil; xy_s.pps:=-1;
  gk_s:=sys_nil; gk_s.pps:=-1;
  zoneX:=0; zoneY:=0;
end;

procedure AB_Orient.Get_lg_tr(out Tr: Real3x3);
begin
  x_ab.Get_Matrix(Tr)
end;

procedure AB_Orient.Get_lz_tr(out Tr: Real3x3);
begin
  x_ab.Get_Matrix(Tr); if sys.pps = 1 then
  t_Move_3x3(Tr,zoneX,zoneY)
end;

procedure AB_Orient.LoadFrom(var T: AB_Orient);
begin
  sys:=T.sys;
  zoneX:=T.zoneX; zoneY:=T.zoneY;
  ag:=T.ag; bg:=T.bg; x_Prepare
end;

function AB_Orient.Equal_ab(var T: AB_Orient): Boolean;
begin
  Result:=false;

  if sys_Equal(sys,T.sys) then
  if zoneY = T.zoneY then

  Result:=true
end;

function AB_Orient.This_lg(const T: lg_Transit): Boolean;
begin
  Result:=sys_Equal(sys,T.s)
end;

procedure AB_Orient.Orient(const av,bv: LORient);
var
  i: Integer;
begin
  for i:=0 to 3 do begin
    ag[i].x:=av[i].x; ag[i].y:=av[i].y;
    bg[i].x:=bv[i].x; bg[i].y:=bv[i].y
  end; g_Prepare(4); x_init:=true
end;

function AB_Orient.Assign(const T: lg_Transit;
                          p_lc: double): Boolean;
const
  Small = 0.0001;
var
  i,iz: Integer;
  b,l,_b,_l, dx,dy,k: double;
begin
  Result:=true; Init;

  sys:=T.s; ag:=T.l; bg:=T.g;

  with sys do
  if pps = 1 then begin

    if elp = 0 then
    if prj = 1 then elp:=1 else
    if prj = 2 then elp:=9;

    if prj in prj_lc then begin
      if prj = 0 then prj:=1; if p_lc < 0 then
      p_lc:=prj_Longitude(lc,cLongitude(@bg),prj);
      lc:=p_lc; zoneY:=prj_zoneY(sys)
    end;

    for i:=0 to 3 do
    with bg[i] do begin b:=x; l:=y;

      sys_BL_XY(b,l, sys, x,y);
      sys_XY_BL(x,y, sys, _b,_l);

      if (Abs_Angle(b,_b) > Small)
      or (Abs_Angle(l,_l) > Small) then
        Result:=false
      else
      if prj <= 2 then begin
        if (b > 0) <> (x > 0) then Result:=false
      end
    end
  end
  else begin
    if prj in [1..2] then begin
      iz:=Trunc(bg[0].y / 1000000);
      zoneY:=iz * 1000000 + 500000;
    end
  end;

  if Abs(sys.x0)+Abs(sys.y0) > 0 then begin
    zoneX:=sys.x0; zoneY:=sys.y0;
  end;

  xy_s.pps:=-1;

  if Result then begin

    k:=1; if ag[0].x <> 0 then
      k:=bg[0].x/ag[0].x
    else if ag[2].x <> 0 then
      k:=bg[2].x/ag[2].x;

    dx:=0; dy:=0; for i:=0 to 3 do begin
      dx:=dx+Abs(ag[i].x*k-bg[i].x)/4;
      dy:=dy+Abs(ag[i].y*k-bg[i].y)/4;
    end;

    if dx/2+dy/2 < 1 then begin

      k:=1; for i:=0 to 3 do
      if Abs(bg[i].x) > 0 then begin
        k:=Round(ag[i].x/bg[i].x);
        Break
      end;

      for i:=0 to 3 do begin
        bg[i].x:=ag[i].x/k;
        bg[i].y:=ag[i].y/k
      end;
    end;

    x_Prepare; x_init:=true;
  end
  else begin
    bg:=ag; x_Prepare;
  end
end;

procedure AB_Orient.Set_origin(const g: TGeoPoint);
var
  i: int; res: double; o,t: TGauss;
begin
  res:=lg_res;
  if res >= 0.001 then
  if g.s.pps = sys.pps then begin

    o.x:=g.x; o.y:=g.y;
    if sys.pps = 1 then
    sys_BL_XY(o.x,o.y,sys,o.x,o.y);

    Frame_to_GOrient(o.x,o.y,1000,1000, @bg);

    for i:=0 to 3 do begin
      t.x:=(bg[i].y - o.y)/res;
      t.y:=(o.x-bg[i].x)/res;
      ag[i]:=t
    end;


    x_Prepare; x_init:=true;
  end
end;

procedure AB_Orient.Change_Transit(const sys2: tsys; is_sm: Boolean);
var
  i: Integer; lg: lg_Transit;
  g: txyz; s1,s2: tsys;
begin
  s1:=sys; if s1.pps = 1 then
  if s1.prj = 0 then s1.prj:=1;

  sys:=s1; s2:=sys2;

  if s1.prj > 0 then

  if s2.pps = 1 then begin
    Init_lg_Transit(lg);
    lg.s:=s2; lg.l:=ag;
    xy_s.pps:=-1;

    for i:=0 to 3 do begin

      with bg[i] do
      if s1.pps = 0 then
        prj_XY_BL(x,y,s1,g.x,g.y)
      else
        sys_XY_BL(x,y,s1,g.x,g.y);

      g.z:=0; if s1.elp <> s2.elp then
      g:=vGEO_GEO(g,s1,s2);

      lg.g[i]:=PGauss(@g)^
    end;

    if not is_sm then
      Assign(lg,lg.s.lc)
    else

    if (Max(s2.elp,1) <> Max(s1.elp,1))
    or (Max(s2.prj,1) <> Max(s1.prj,1))
    or (lg_Precision(lg) > 5) then begin

      if (s1.prj in [0..2])
      and xEqual(s1.lc,s2.lc) then
        Assign(lg,s2.lc)
      else begin
        xy_s:=lg.s;
        if Abs(xy_s.y0) = 0 then
        xy_s.y0:=prj_zoneY(xy_s)
      end
    end
    else Assign(lg,s2.lc)
  end
end;

function AB_Orient.is_gk_prj: int;
begin
  with sys do begin
    if prj = 0 then
    sys_projection(sys,bg[0].x,bg[0].y);
    if prj > 0 then zoneY:=prj_zoneY(sys)
  end;

  Result:=sys.prj
end;

function AB_Orient.lg_res: Double;
begin
  Result:=Gauss_res(bg[0],bg[2], ag[0],ag[2])
end;

function AB_Orient.lg_ppm: Double;
begin
  Result:=Gauss_res(ag[0],ag[2], bg[0],bg[2]);
  Result:=Max(Result,0.001)
end;

function AB_Orient.prj_res: Double;
var
  g1,g2: tgeoid; dist,fi: double;
begin
  Result:=1; if sys.prj >= 4 then begin
    XY_BL(bg[0],g1); XY_BL(bg[2],g2);

    Set_Ellipsoid(sys.elp);

    dist:=Geoid_Dist(g1.b,g1.l, g2.b,g2.l, fi);

    if dist > 1 then
    Result:=Gauss_Dist(bg[0],bg[1])/dist
  end
end;

procedure AB_Orient.A_to_B(const a: TPoint; out b: TPoint);
var
  g: tgauss;
begin
  if x_fast then
    g:=x_ab.Transit(a.x,a.y)
  else
    g:=q_ab.Transit(a.x,a.y);

  b.x:=Round(g.x); b.y:=Round(g.y)
end;

procedure AB_Orient.B_to_A(const a: TPoint; out b: TPoint);
var
  g: tgauss;
begin
  if x_fast then
    g:=x_ba.Transit(a.x,a.y)
  else
    g:=q_ba.Transit(a.x,a.y);

  b.x:=Round(g.x); b.y:=Round(g.y)
end;

function AB_Orient.Forw(x,y: Double): TGauss;
begin
  if x_fast then
    Result:=x_ab.Transit(x,y)
  else
    Result:=q_ab.Transit(x,y)
end;

function AB_Orient.Back(x,y: Double): TGauss;
begin
  if x_fast then
    Result:=x_ba.Transit(x,y)
  else
    Result:=q_ba.Transit(x,y);
end;

procedure AB_Orient.L_to_G(const l: TPoint; out g: tgauss);
begin
  if x_fast then
    g:=x_ab.Transit(l.x,l.y)
  else
    g:=q_ab.Transit(l.x,l.y)
end;

procedure AB_Orient.G_to_L(const g: tgauss; out l: TPoint);
var
  t: tgauss;
begin
  if x_fast then
    t:=x_ba.Transit(g.x,g.y)
  else
    t:=q_ba.Transit(g.x,g.y);

  l.X:=Round(t.x); l.Y:=Round(t.y)
end;

procedure AB_Orient.G_pps_L(const g: tgauss; pps: int; out p: TPoint);
begin
  if pps >= 0 then begin
    if pps = 0 then Z_to_L(g,p)
    else R_to_L(tgeoid(g),p)
  end
  else begin
    if sys.pps = 0 then Z_to_L(g,p)
    else R_to_L(tgeoid(g),p)
  end
end;

procedure AB_Orient.P_to_G(const p: tgauss; out g: tgauss);
begin
  if x_fast then g:=x_ab.Transit(p.x,p.y) else
  g:=q_ab.Transit(p.x,p.y)
end;

procedure AB_Orient.G_to_P(const g: tgauss; out p: tgauss);
begin
  if x_fast then p:=x_ba.Transit(g.x,g.y)
  else p:=q_ba.Transit(g.x,g.y);
end;

procedure AB_Orient.L_to_bl(const p: TPoint; out r: tgeoid);
var
  g: tgauss;
begin
  L_to_G(p,g);

  if sys.pps = 0 then begin
    g.x:=g.x - zoneX;
    g.y:=g.y - zoneY;
  end;

  XY_BL(g,r)
end;

procedure AB_Orient.L_to_ms(const p: TPoint; out r: tgeoid);
var
  g: tgeoid;
begin
  L_to_bl(p,g);
  if sys.pps = 1 then begin
    g.b:=g.b/Pi*180*3600*100;
    g.l:=g.l/Pi*180*3600*100;
  end; r:=g
end;

function AB_Orient.L_to_R(const l: TPoint; out r: tgeoid): Integer;
var
  g: tgauss;
begin
  L_to_G(l,g); if sys.pps = 0 then
  r:=tgeoid(g) else XY_BL(g,r);
  Result:=sys.pps
end;

procedure AB_Orient.R_to_L(const r: tgeoid; out l: TPoint);
var
  g: tgauss;
begin
  if sys.pps = 0 then g:=tgauss(r) else
  BL_XY(r.b,r.l,g); G_to_L(g,l)
end;

function AB_Orient.L_to_prj(const l: TPoint; elp: int): tgauss;
var
  g: TGauss; r: TGeoid; s: tsys; v: txyz;
begin
  L_to_Z(l,g);

  if (elp = 1) and (sys.prj <> 1) then begin
    z_XY_BL(g,r);
    v.x:=r.b; v.y:=r.l; v.z:=0;

    if sys.elp <= 1 then s:=sys
    else s:=sys_ru42; s.prj:=1;

    if sys.elp > 1 then
    v:=vGEO_GEO(v,sys,s);

    s.lc:=zLongitude(r.l);
    prj_BL_XY(v.x,v.y,s,g.x,g.y)
  end else
  if (elp = 9) and (sys.prj <> 2) then begin
    z_XY_BL(g,r);
    v.x:=r.b; v.y:=r.l; v.z:=0;

    s:=sys_wgs84; s.prj:=2;

    if sys.elp <> 9 then
    v:=vGEO_WGS(v,sys);

    s.lc:=zLongitude(r.l);
    prj_BL_XY(v.x,v.y,s,g.x,g.y)
  end;

  Result:=g
end;

procedure AB_Orient.L_to_Z(const l: TPoint; out g: tgauss);
begin
  L_to_G(l,g);
  if sys.pps = 1 then begin
    g.x:=g.x+zoneX; g.y:=g.y+zoneY
  end
end;

procedure AB_Orient.Z_to_L(const g: tgauss; out l: TPoint);
var
  t: tgauss;
begin
  t:=g;
  if sys.pps = 1 then begin
    t.x:=t.x-zoneX; t.y:=t.y-zoneY
  end; G_to_L(t,l)
end;

function AB_Orient.z_xy_to_pix(const g: tgauss): tgauss;
var
  t: tgauss;
begin
  t:=g;
  if sys.pps = 1 then begin
    t.x:=t.x-zoneX; t.y:=t.y-zoneY
  end; Result:=Back(t.x,t.y)
end;

function AB_Orient.dm_sys_xy(const p: TPoint; const s: tsys): TGauss;
var
  g: TGauss; r: XGeoid; t: tsys;
begin
  if prj_Equal(sys,s) then
    L_to_Z(p,g)
  else begin
    x_L_to_R(p,r); t:=s; t.pps:=0;
    prj_to_xy(r,t,g)
  end;

  Result:=g
end;

function AB_Orient.xy_sys_dm(const g: tgauss; const s: tsys): TPoint;
var
  r: XGeoid; p: TPoint; b,l,h: Double;
begin
  if prj_Equal(s,sys) then
    Z_to_L(g,p)
  else begin
    r.x:=g.x; r.y:=g.y;
    r.s:=s; r.s.pps:=0;
    x_R_to_L(r,p)
  end;

  Result:=p
end;

procedure AB_Orient.dm_to_xy(const p: TPoint; out g: tgauss);
var
  v: txyz; t: TGauss;
begin
  L_to_G(p,t);
  if sys.prj > 0 then

  if xy_s.pps <= 0 then begin
    t.x:=t.x + zoneX;
    t.y:=t.y + zoneY
  end
  else begin
    if sys.pps = 0 then
      prj_XY_BL(t.x,t.y,sys,v.x,v.y)
    else
      sys_XY_BL(t.x,t.y,sys,v.x,v.y);

    v.z:=0;
    if sys.elp <> xy_s.elp then
    v:=vGEO_GEO(v,sys,xy_s);

    sys_BL_XY(v.x,v.y, xy_s, t.x,t.y);
    t.y:=t.y + xy_s.y0
  end;

  g:=t
end;

procedure AB_Orient.xy_to_dm(const g: tgauss; out p: TPoint);
var
  t: TGauss; v: txyz;
begin
  t:=g;

  if sys.prj > 0 then
  if xy_s.pps = 1 then begin
    sys_XY_BL(t.x,t.y - xy_s.y0,xy_s, v.x,v.y);

    v.z:=0;
    if sys.elp <> xy_s.elp then
    v:=vGEO_GEO(v,xy_s,sys);

    if sys.pps = 0 then
      prj_BL_XY(v.x,v.y,sys,t.x,t.y)
    else
      sys_BL_XY(v.x,v.y,sys,t.x,t.y)

  end
  else begin
    t.x:=t.x - zoneX;
    t.y:=t.y - zoneY
  end;

  G_to_L(t,p)
end;

procedure AB_Orient.WGS_XY(B,L: Double; out g: tgauss);
var
  h: Double;
begin
  with sys do begin if elp <> 9 then
    WGS_GEO(B,L,0, elp,@dat, B,L,h);
    sys_BL_XY(B,L, sys, g.x,g.y);
  end
end;

procedure AB_Orient.BL_XY(B,L: Double; out g: tgauss);
begin
  sys_BL_XY(B,L, sys, g.x,g.y)
end;

procedure AB_Orient.XY_BL(const g: tgauss; out r: tgeoid);
begin
  sys_XY_BL(g.x,g.y, sys, r.b,r.l)
end;

procedure AB_Orient.z_BL_XY(const r: tgeoid; out g: tgauss);
begin
  BL_XY(r.b,r.l,g);
  g.x:=g.x + zoneX;
  g.y:=g.y + zoneY;
end;

procedure AB_Orient.z_XY_BL(const g: tgauss; out r: tgeoid);
var
  t: TGauss;
begin
  t:=g;
  t.x:=t.x - zoneX;
  t.y:=t.y - zoneY;
  XY_BL(t,r)
end;

procedure AB_Orient.L_to_Geo(const p: TPoint; out x: xgeoid);
var
  g: tgauss;
begin
  x.s:=sys;

  if sys.pps = 1 then
    L_to_R(p,tgeoid(g))
  else begin
    L_to_G(p,g);
    g.x:=g.x - sys.x0;
    g.y:=g.y - sys.y0;

    if sys.prj > 0 then
    if Abs(sys.x0)+Abs(sys.y0) > 1 then begin
      sys_XY_BL(g.x,g.y,sys,g.x,g.y); x.s.pps:=1
    end
  end;

  x.x:=g.x; x.y:=g.y;
end;

function AB_Orient.g_to_geo(x,y: Double): XGeoid;
var
  g: TGauss; r: XGeoid;
begin
  g.x:=x; g.y:=y;
  if sys.pps = 1 then begin
    g.x:=g.x + zoneX;
    g.y:=g.y + zoneY;
  end;

  x_G_to_R(g,r); Result:=r
end;

procedure AB_Orient.x_L_to_R(const p: TPoint; out x: xgeoid);
var
  g: tgauss;
begin
  if sys.pps = 0 then L_to_G(p,g)
  else L_to_R(p,tgeoid(g));

  x.x:=g.x; x.y:=g.y; x.s:=sys
end;

procedure AB_Orient.x_G_to_R(const g: tgauss; out x: xgeoid);
var
  t: tgauss;
begin
  t:=g; if sys.pps = 1 then z_XY_BL(g,tgeoid(t));
  x.x:=t.x; x.y:=t.y; x.s:=sys
end;

procedure AB_Orient.x_R_to_Z(const x: xgeoid; out g: tgauss);
var
  t: tgauss; r: tgeoid; s1,s2: tsys; tz: double;
begin
  t.x:=x.x; t.y:=x.y;

  s1:=x.s;
  if (s1.pps = 1) or (s1.prj > 0) then begin

    s2:=sys; with bg[0] do
    sys_projection(s2,x,y);

    if (s2.pps = 1) or (s2.prj > 0) then

    if (s1.pps = 1) or not prj_Equal(s1,s2) then begin

      if s1.pps = 0 then begin
        r:=x_XY_BL(t,s1); t:=tgauss(r)
      end;

      if s1.elp <> s2.elp then
      GEO_GEO(t.x,t.y,0,
              s1.elp,s2.elp,
              @s1.dat,@s2.dat,
              t.x,t.y,tz);

      sys_BL_XY(t.x,t.y, s2, t.x,t.y);

      if s2.prj <> sys.prj then
        t.y:=t.y + prj_zoneY(s2)
      else begin
        t.x:=t.x + zoneX;
        t.y:=t.y + zoneY;
      end
    end else
    if (s2.pps = 1) and (s1.pps = 0) then
    if Abs(s1.x0)+Abs(s1.y0) > 0 then begin
      t.x:=t.x - s1.x0 + zoneX;
      t.y:=t.y - s1.y0 + zoneY;
    end
  end;

  g:=t
end;

procedure AB_Orient.x_R_to_G(const x: xgeoid; out g: tgauss);
begin
  x_R_to_Z(x,g);
  if sys.pps = 1 then begin
    g.x:=g.x-zoneX; g.y:=g.y-zoneY
  end
end;

procedure AB_Orient.x_R_to_L(const x: xgeoid; out p: TPoint);
var
  g: tgauss;
begin
  x_R_to_Z(x,g); Z_to_L(g,p)
end;

function AB_Orient.x_R_to_P(const x: xgeoid): TGauss;
var
  g: tgauss;
begin
  x_R_to_G(x,g);
  Result:=Back(g.x,g.y)
end;

procedure AB_Orient.z_R_to_L(const x: xgeoid; out p: TPoint);
var
  t: xgeoid;
begin
  t:=x; if t.s.pps = 1 then
  if sys.pps = 1 then
  t.y:=Align_Longitude(t.y);

  x_R_to_L(t,p)
end;

procedure AB_Orient.xxx_to_L(const x: xgeoid; out p: TPoint);
var
  g: tgauss;
begin
  if x.s.pps = 1 then
    BL_XY(x.x,x.y,g)
  else begin
    g.x:=x.x - zoneX;
    g.y:=x.y - zoneY
  end;

  G_to_L(g,p)
end;

procedure AB_Orient.wgs_loc_win(W: PGPoly; L: PLPoly);
var
  i: int; p: LOrient; g: GOrient; gz: Double;
begin
  Bound_to_GOrient(W[0],W[1], @g);

  for i:=0 to 3 do with g[i] do begin
    if sys.elp <> 9 then
    WGS_GEO(x,y,0,sys.elp,@sys.dat,x,y,gz);
    BL_XY(x,y,g[i]); G_to_L(g[i],p[i])
  end;

  Max_Poly_Bound(@p,4,L[0],L[1])
end;

function AB_Orient.Align_longitude(l: Double): Double;
var
  lt,rb: tgauss;
begin
  Result:=l;

  if sys.pps = 1 then
  if Get_pps_Bound(lt,rb) = 1 then

  if l < lt.y then begin
    l:=l + Pi*2; if l >= lt.y then
    if l <= rb.y then Result:=l
  end else
  if l > rb.y then begin
    l:=l - Pi*2; if l >= lt.y then
    if l <= rb.y then Result:=l
  end
end;

function AB_Orient.lg_Delta: double;
var
  i: Integer; g: tgauss;
begin
  Result:=0;
  for i:=0 to 3 do begin
    P_to_G(ag[i],g);
    Result:=Max( Result,Gauss_Dist(g,bg[i]));
  end;
end;

function AB_Orient.lg_Delta1: double;
var
  i: Integer; g: tgauss; r: TGeoid;
begin
  Result:=0;

  if sys.pps > 0 then
  for i:=0 to 3 do begin
    XY_BL(bg[i],r); BL_XY(r.b,r.l,g);
    Result:=Max( Result,Gauss_Dist(g,bg[i]));
  end;
end;

procedure AB_Orient.lg_Quadro;
var
  d: double;
begin
  if x_fast then begin
    d:=lg_Delta; if d > 10 then
    if not q_Prepare(d/2) then
    g_Prepare(4)
  end
end;

function AB_Orient.q_Prepare(d: double): Boolean;
begin
  if q_ab.Assign(ag,bg) then
  if q_ba.Assign(bg,ag) then begin

    x_fast:=true; if d > 0 then
    if lg_Delta < d then x_fast:=false;

  end; Result:=not x_fast
end;

function AB_Orient.g_Prepare(n: Integer): double;
begin
  x_ab.Open_Min_Sqr(@ag,@bg,n);
  x_ba.Open_Min_Sqr(@bg,@ag,n);
  x_fast:=true; Result:=lg_Delta
end;

function AB_Orient.x_Prepare: double;
begin
  x_ab.xOpen(ag[0],ag[1],ag[2], bg[0],bg[1],bg[2]);
  x_ba.xOpen(bg[0],bg[1],bg[2], ag[0],ag[1],ag[2]);
  x_fast:=true; Result:=lg_Delta
end;

function AB_Orient.translate_Prepare: double;
begin
  x_ab.ab_Open(@ag,@bg,2);
  x_ba.ab_Open(@bg,@ag,2);
  x_fast:=true; Result:=lg_Delta
end;

procedure AB_Orient.c_Prepare(const c: tgauss);
var
  i,j: Integer; dist,tmp: double;
  g: GOrient; gc: tgauss;
begin
  g:=bg; gc:=c;

  if sys.pps = 1 then begin
    XY_BL(c,tgeoid(gc));

    for i:=0 to 3 do
    XY_BL(bg[i],tgeoid(g[i]))
  end;

  for i:=0 to 3 do begin
    dist:=Gauss_Dist(g[i],gc);

    for j:=i+1 to 3 do begin
      tmp:=Gauss_Dist(g[j],gc);
      if tmp < dist then begin
        Swap_TGauss(ag[i],ag[j]);
        Swap_TGauss(bg[i],bg[j]);
        dist:=tmp
      end
    end
  end;

  x_Prepare
end;

procedure AB_Orient.t_Prepare(var T: AB_Orient);
var
  i: Integer; r: xgeoid;
begin
  if sys.pps = 1 then
  if T.sys.pps = 1 then begin

    for i:=0 to 3 do begin
      x_G_to_R(bg[i],r);
      T.x_R_to_G(r,bg[i])
    end;

    sys:=T.sys;
    zoneX:=T.zoneX;
    zoneY:=T.zoneY;
    xy_s.pps:=-1;

    c_Prepare( gauss_Centre(@T.bg,4) )
  end
end;

function AB_Orient.sys_dist(const a,b: TPoint; elp: int): double;
var
  g1,g2: TGauss; r1,r2: TGeoid; s: tsys; z: double;
begin
  L_to_G(a,g1); L_to_G(b,g2);

  if elp > 0 then
  if sys.prj > 0 then
  if (sys.elp <> elp)
  or ((elp = 1) and (sys.prj <> 1))
  or ((elp = 9) and (sys.prj <> 2)) then begin

    XY_BL(g1,r1); XY_BL(g2,r2);

    if elp = 1 then s:=sys_gk(r1.l)
               else s:=sys_utm(r1.l);

    if elp <> sys.elp then begin

      with r1 do
      GEO_GEO(b,l,0, sys.elp,s.elp,@sys.dat,@s.dat, b,l,z);

      with r2 do
      GEO_GEO(b,l,0, sys.elp,s.elp,@sys.dat,@s.dat, b,l,z);
    end;

    sys_BL_XY(r1.b,r1.l,s,g1.x,g1.y);
    sys_BL_XY(r2.b,r2.l,s,g2.x,g2.y);
  end;

  Result:=Gauss_Dist(g1,g2)
end;

function AB_Orient.xy_Dist(const a,b: TPoint; out f: Double): double;
var
  g1,g2: TGauss;
begin
  L_to_G(a,g1); L_to_G(b,g2);
  f:=xAzimuth(g1,g2);
  Result:=Gauss_Dist(g1,g2);
end;

function AB_Orient.gLength(const a,b: tgauss; out f: double): double;
var
  r1,r2: tgeoid; s: tsys;
begin
  if (sys.pps = 0) or x_info then begin
    f:=xAzimuth(a,b);
    Result:=Gauss_Dist(a,b)
  end
  else begin s:=sys;
    if gk_s.pps = 1 then s:=gk_s;

    sys_XY_BL(a.x,a.y,s,r1.b,r1.l);
    sys_XY_BL(b.x,b.y,s,r2.b,r2.l);
    
    Result:=Geoid_Dist(r1.b,r1.l, r2.b,r2.l, f);

    if Result < 1 then
    if sys.prj <> prj_deg then begin
      f:=xAzimuth(a,b);
      Result:=Gauss_Dist(a,b)
    end
  end
end;

function AB_Orient.xLength(const a,b: TPoint; out f: double): double;
var
  g1,g2: tgauss;
begin
  L_to_G(a,g1); L_to_G(b,g2);
  Result:=gLength(g1,g2,f);
end;

function AB_Orient.xProject(const p: TPoint; r,f: double): TPoint;
var
  g1,g2: tgauss;
begin
  L_to_G(p,g1);
  g2:=prj_xy(g1.x,g1.y, r,r,f);
  G_to_L(g2,Result)
end;

function AB_Orient.p_rf_p(const p: TPoint; r,f: double;
                          scale,ed: Integer): TPoint;
var
  k: Double; is_bl: Boolean;
  g,g1,g2: tgauss; r1,r2: tgeoid;
begin
  k:=ed_k(scale,ed);

  if (sys.pps = 1) and (ed in [0..3]) then begin
    L_to_R(p,r1);
    Geoid_Forw(r1.b,r1.l, r*k,f, r2.b,r2.l);
    R_to_L(r2,Result)
  end
  else begin
    is_bl:=(sys.pps = 1) and
           (ed in [2..3]);

    if is_bl then begin
      r:=r*Pi/180/60;
      if ed = 3 then r:=r/10;

      L_to_R(p,tgeoid(g));
      BL_XY(g.x,g.y,g1);

      g:=prj_xy(g.x,g.y, r,r,0);
      BL_XY(g.x,g.y,g2);

      r:=Gauss_Dist(g1,g2);
      k:=1
    end;

    L_to_G(p,g); r:=r * k;
    g:=prj_xy(g.x,g.y, r,r,f);
    G_to_L(g,Result);
  end
end;

function AB_Orient.step_rf(const p1,p2: TPoint; r,f: double;
                           scale,ed: Integer): TPoint;
var
  g, g1,g2: tgauss;
begin
  r:=r * ed_k(scale,ed);
  L_to_G(p1,g1); L_to_G(p2,g2);
  g:=step_xy(g1.x,g1.y,g2.x,g2.y, r,f);
  G_to_L(g,Result);
end;

function AB_Orient.lpoly_Length(lp: PLPoly; lp_n: int): double;
var
  i: int; g1,g2: tgauss; f: double; eax: Extended;
begin
  eax:=0;

  if lp_n > 0 then begin

    g2:=s_L_to_G(lp[0]);
    for i:=1 to lp_n do begin
      g1:=g2; g2:=s_L_to_G(lp[i]);
      eax:=eax + gLength(g1,g2,f)
    end
  end;

  Result:=eax
end;

function AB_Orient.lpoly_Length3d(lp: PLPoly; hp: Pintegers;
                                  lp_n, z_res: int): TGauss;
var
  i,bl: integer; g1,g2: tgauss;
  z1,z2,dx,dy,dz,dxy,f: Double;
  eax,ebx: Extended;
begin
  eax:=0; ebx:=0;

  if lp_N > 0 then begin

    bl:=0; if sys.pps = 1 then
    if sys.prj >= 2 then bl:=1;

    g1:=s_L_to_G(lp[0]); z1:=hp[0]/z_res;

    for i:=1 to lp_N do begin
      g2:=s_L_to_G(lp[i]);
      z2:=hp[i]/z_res; dz:=z2-z1;

      if bl = 1 then begin
        dxy:=gLength(g1,g2,f);
        eax:=eax + Hypot(dxy,dz);
        ebx:=ebx + dxy;
      end
      else begin
        dx:=g2.x-g1.x; dy:=g2.y-g1.y;
        eax:=eax + Sqrt(dx*dx + dy*dy + dz*dz);
        ebx:=ebx + Hypot(dx,dy);
      end;

      g1:=g2; z1:=z2;
    end
  end;

  Result.x:=eax; Result.y:=ebx;
end;

function AB_Orient.lpoly_Square(lp: PLPoly; lp_n: int): double;
var
  i: int; g1,g2: tgauss;
begin
  Result:=0;
  if lp_N > 0 then begin
    g2:=s_L_to_G(lp[0]);

    for i:=1 to lp_N do begin
      g1:=g2; g2:=s_L_to_G(lp[i]);
      Result:=Result + g1.x*g2.y - g2.x*g1.y
    end; Result:=Result/2
  end
end;

function AB_Orient.Poly_Length(lp: PLLine): double;
begin
  Result:=lpoly_Length(@lp.Pol,lp.N)
end;

function AB_Orient.xyz_Length(lp: PLLine; hp: Pintegers;
                              z_res: Integer): TGauss;
begin
  Result:=lpoly_Length3d(@lp.Pol,hp,lp.N,z_res)
end;

function AB_Orient.Poly_Square(lp: PLLine): double;
begin
  Result:=lpoly_Square(@lp.Pol,lp.N)
end;

function AB_Orient.Curve_Length(lp: PLLine; out s: double): double;
var
  i,j: Integer; Curve: TCurve;
  p: LOrient; g: GOrient;
begin
  Result:=0; s:=0;

  Curve:=TCurve.Create;
  try
    with lp^ do if N > 1 then begin
      p[2]:=Pol[0]; p[3]:=Pol[1];

      i:=2; while i < N do begin
        p[0]:=p[2]; p[2]:=Pol[i]; Inc(i);
        p[1]:=p[3]; p[3]:=Pol[i]; Inc(i);
        Mirror_Point(p[3],p[2],p[3]);

        for j:=0 to 3 do g[j]:=s_L_to_G(p[j]);

        Result:=Result+Curve.gBezier(@g);
        s:=s + Curve.Square;

        Mirror_Point(p[3],p[2],p[3])
      end
    end;
  finally
    Curve.Free
  end
end;

function AB_Orient.poly_gk(lp: PLPoly; lp_n: int): Boolean;
var
  r: int; a,b,c: TPoint;
begin
  Result:=false;

  if sys.pps = 1 then
  if sys.prj > 2 then begin

    Max_Poly_Bound(lp,lp_n+1,a,b);
    r:=Max(b.X-a.X,b.Y-a.Y);

    if r*lg_res < 200000 then begin
      Middle_Point(a,b,c);
      Result:=set_gk_s(c)
    end
  end
end;

function AB_Orient.set_gk_s(const p: TPoint): Boolean;
var
  r: TGeoid;
begin
  Result:=false;
  if sys.pps = 1 then
  if sys.prj > 2 then begin
    L_to_R(p,r); r.l:=zLongitude(r.l);
    gk_s:=sys7(1,1,sys.elp,0,0,r.l);
    if sys.elp > 1 then gk_s.prj:=2;
    Result:=true
  end
end;

function AB_Orient.s_L_to_G(const p: TPoint): TGauss;
var
  g: TGauss; r: TGeoid;
begin
  if gk_s.pps < 1 then
    L_to_G(p,g)
  else begin
    L_to_R(p,r);
    sys_BL_XY(r.b,r.l,gk_s,g.x,g.y)
  end;

  Result:=g
end;

function AB_Orient.Get_pps_Bound(out lt,rb: tgauss): Integer;
var
  i: Integer; g: GOrient; r1,r2,r3,r4: TGeoid;
begin
  g:=bg; if sys.pps = 1 then
  for i:=0 to 3 do XY_BL(bg[i],tgeoid(g[i]));
  Max_Gauss_Bound(@g,4,lt,rb);

  if sys.prj in prj_tilted then begin
    g:=bg; Sort_lg_Transit(nil,@g);
    XY_BL(g[0],r1); XY_BL(g[1],r2);
    XY_BL(g[2],r3); XY_BL(g[3],r4);
    
    if (r1.l > 0) <> (r2.l > 0)
    or (r3.l > 0) <> (r4.l > 0) then begin

      if lt.x > 0 then rb.x:=Pi/2
      else lt.x:=-Pi/2;

      lt.y:=0; rb.y:=Pi*2
    end
  end;

  Result:=sys.pps
end;

procedure AB_Orient.Get_Gauss_Bound(const l_lt,l_rb: TPoint;
                                    out g_lt,g_rb: tgauss);

var
  i: Integer; L: LOrient; G: GOrient;
begin
  Bound_to_LPoly(l_lt,l_rb,@L);
  for i:=0 to 3 do L_to_G(L[i],G[i]);
  Max_Gauss_Bound(@G,4,g_lt,g_rb);
end;

procedure AB_Orient.Get_Local_Bound(const g_lt,g_rb: tgauss;
                                    out l_lt,l_rb: TPoint);
var
  i: Integer; L: LOrient; G: GOrient;
begin
  Bound_to_GOrient(g_lt,g_rb,@G);
  for i:=0 to 3 do G_to_L(G[i],L[i]);
  Max_Poly_Bound(@L,4,l_lt,l_rb)
end;

function AB_Orient.Get_grect(lp: PLLine; fr: PGPoly): bool;
var
  i: int; g: TGauss;
begin
  Result:=false;

  if PolyLock(lp) then begin

    for i:=0 to lp.N do begin
      L_to_Z(lp.Pol[i],g);

      if i > 0 then
        Max_GPort(fr[0],fr[1],g)
      else begin
        fr[0]:=g; fr[1]:=g
      end
    end;

    Result:=true
  end
end;

procedure AB_Orient.Get_Link_Point(Ind: Integer; out V: TPoint);
var
  G: GOrient; p: TPoint; i: Integer;
  r,_r: Double; lt,rb, g1,g2: tgauss;
begin
  V:=_LGauss(ag[0].x,ag[0].y);

  if (Ind >= 0) and (Ind <= 3) then begin

    Get_pps_Bound(lt,rb);
    Bound_to_GOrient(lt,rb, @G);

    g1:=G[Ind];
    for i:=0 to 3 do begin
      p.x:=Round(ag[i].x);
      p.y:=Round(ag[i].y);
      L_to_R(p,tgeoid(g2));

      _r:=Gauss_Dist(g1,g2);
      if (i = 0) or (_r < r) then begin
        r:=_r; V:=p
      end
    end
  end
end;

function AB_Orient.Geoid_Port(const lt,rb: tgauss;
                              sc_ind,scale: Integer;
                              lp: PLLine; lp_Max: Integer;
                              link: Boolean): Integer;

procedure Insert_Corner(lp: PLLine;
                        const g: TGeoid;
                        const p: TGauss;
                        link: Boolean);
begin
  if Insert_bl(lp,g) >= 0 then
  if link then with lp.Pol[lp.N] do begin
    x:=Round(p.x); y:=Round(p.y)
  end
end;

function inter_step(b,l, db,dl, min_r: Double): Integer;
var
  _db,_dl,sec,r: Double;
  p1,p2,p3: TPoint;
begin
  Result:=1; _db:=db; _dl:=dl;

  sec:=1/3600 * Pi / 180;
  R_to_L(_Geoid(b,l),p1);

  while Max(_db,_dl) > sec do begin
    R_to_L(_Geoid(b+_db,l+_dl),p2);
    R_to_L(_Geoid(b+_db/2,l+_dl/2),p3);

    if Points_Equal(p1,p2) then Break;

    r:=Abs(Dist_to_Line(p3, p1,p2));

    if r <= min_r then Break; Inc(Result);

    _db:=db/Result; _dl:=dl/Result
  end
end;

function degree_step(def: Integer;
                     l1,l2: Double): Integer;
var
  dl: Integer;
begin
  dl:=Round(Abs((l2-l1)/Pi*180));
  if dl >= 80 then dl:=dl div 10 else
  if dl >= 40 then dl:=dl div 5 else
  if dl >= 20 then dl:=dl div 2;
  Result:=Max(def,dl);
end;

const
  _step: array[1..2] of Integer = (60,30);

var
  g: tgeoid; db,dl,r: double;
  i,j, di1,di2, dj,st: Integer;
  t: lg_Transit;

begin
  Result:=-1;

  if Assigned(lp) then lp.N:=-1;

  db:=rb.x-lt.x; dl:=rb.y-lt.y;

  di1:=1; di2:=1; dj:=1;

  if sys.prj <= 2 then begin
    st:=15; if sc_ind in [1..2] then
    st:=_step[sc_ind];
    
    di1:=Round(Abs(dl)/Radian(0,st,0));

    if sc_ind = 0 then
    if Abs(dl) < 1/Pi then di1:=1;
    di2:=di1
  end
  else begin
    r:=Max(1, scale / 2000 * lg_ppm);
    di1:=inter_step(rb.x,lt.y, 0,dl, r);
    di2:=inter_step(lt.x,lt.y, 0,dl, r);

    if sys.prj = 6 then
      dj:=inter_step(lt.x,lt.y, db,0, r)
    else
    if sys.prj in [4,28] then begin
      di1:=degree_step(di1,lt.y,rb.y);
      di2:=degree_step(di1,lt.y,rb.y);
    end
  end;

  while di1 + di2 + dj + dj >= lp_Max do begin
    di1:=Max(1,di1 div 2);
    di2:=Max(1,di2 div 2);
    dj:=Max(1,dj div 2);
  end;

  g.b:=lt.x; g.l:=lt.y;

  t.L:=ag; for i:=0 to 3 do
  XY_BL(bg[i],tgeoid(t.G[i]));
  Sort_lg_Transit(@t.L,@t.G);

  Insert_Corner(lp,g,t.L[0],link);
  for j:=1 to dj-1 do begin
    g.b:=g.b+db/dj; Insert_bl(lp,g)
  end; g.b:=rb.x;

  Insert_Corner(lp,g,t.L[1],link);
  for i:=1 to di1-1 do begin
    g.l:=g.l+dl/di1; Insert_bl(lp,g);
  end; g.l:=rb.y;

  Insert_Corner(lp,g,t.L[2],link);
  for j:=1 to dj-1 do begin
    g.b:=g.b-db/dj; Insert_bl(lp,g)
  end; g.b:=lt.x;

  Insert_Corner(lp,g,t.L[3],link);
  for i:=1 to di2-1 do begin
    g.l:=g.l-dl/di2; Insert_bl(lp,g);
  end; g.l:=lt.y;

  if Assigned(lp) then begin
    Inc(lp.N); with lp^ do Pol[N]:=Pol[0]
  end;

  Result:=di1+di2+2+1
end;

procedure AB_Orient.Geoid_Latitude(lp: PLLine; b,l1,l2,dl: double);
var
  g: tgeoid;
begin
  lp.N:=-1; g.b:=b; g.l:=l1;
  while g.l <= l2+0.001 do begin
    Insert_bl(lp,g); g.l:=g.l+dl
  end
end;

procedure AB_Orient.Geoid_Longitude(lp: PLLine; b1,b2,db,l: double);
var
  g: tgeoid;
begin
  lp.N:=-1; g.b:=b1; g.l:=l;
  while g.b <= b2+0.001 do begin
    Insert_bl(lp,g); g.b:=g.b+db
  end
end;

function AB_Orient.Insert_bl(lp: PLLine; const g: tgeoid): Integer;
begin
  Result:=-1;
  if Assigned(lp) then with lp^ do begin
    Inc(N); R_to_L(g,Pol[N]); Result:=N
  end
end;

procedure AB_Orient.Move_bl_Port(lp: PLLine; const a,b: TPoint);
var
  i: Integer; db,dl: Double;
  g1,g2,g: tgeoid;
begin
  L_to_R(a,g1); L_to_R(b,g2);
  db:=g2.b-g1.b; dl:=g2.l-g1.l;

  for i:=0 to lp.N do begin
    L_to_R(lp.Pol[i],g);
    g.b:=g.b+db; g.l:=g.l+dl;
    R_to_L(g,lp.Pol[i])
  end
end;

function AB_Orient.Get_Poly_frame(lp: PLLine; lp_Max: Integer): Integer;

procedure next_point(lp: PLLine; const p: TPoint);
begin
  with lp^ do begin
    Inc(N); Pol[N]:=p
  end
end;

procedure add_side(lp: PLLine; const g1,g2: TGauss);
var
  i,k: Integer;
  a,b,c: TPoint;
  g: TGauss; dx,dy,eps: Double;
begin
  R_to_L(TGeoid(g1),a);
  R_to_L(TGeoid(g2),b);

  dx:=g2.x-g1.x;
  dy:=g2.y-g1.y; k:=1;

  while k < 8 do begin Inc(k);
    g.x:=g1.x + dx/k;
    g.y:=g1.y + dy/k;
    R_to_L(TGeoid(g),c);

    g.x:=g1.x + dx*2/k;
    g.y:=g1.y + dy*2/k;
    R_to_L(TGeoid(g),b);

    eps:=Abs(Dist_to_Line(c,a,b));

    if eps < 1 then begin
      Dec(k); Break
    end
  end;

  dx:=dx/k; dy:=dy/k;

  for i:=1 to k-1 do begin
    g.x:=g1.x + dx*i;
    g.y:=g1.y + dy*i;
    R_to_L(TGeoid(g),c);
    next_point(lp,c)
  end
end;

var
  i: Integer; L: LOrient; G: GOrient;
begin
  lp.N:=-1;

  for i:=0 to 3 do
  L[i]:=_LGauss(ag[i].x,ag[i].y);
  L[4]:=L[0];

  G:=bg; if sys.pps = 1 then
  for i:=0 to 3 do
  XY_BL(bg[i],TGeoid(G[i]));

  G[4]:=G[0];

  for i:=0 to 3 do begin
    next_point(lp,L[i]);
    add_side(lp,G[i],G[i+1]);
  end;

  next_point(lp,L[0]);

  Result:=lp.N
end;

procedure AB_Orient.sxf_ramka(lp: PLLine);
var
  i,j: int; t,d: double;
  g,g1,g2,lt,rb: TGauss; tmp: TLLine;
begin
  if PolyLock(lp) then
  if lp.N < 255 then begin

    Get_pps_bound(lt,rb);

    j:=0;
    for i:=0 to lp.N do begin
      L_to_R(lp.Pol[i],TGeoid(g));
      t:=Gauss_dist(g,lt);
      if (i = 0) or (t < d) then begin
        d:=t; j:=i
      end
    end;

    if j > 0 then
    Lock_Rotate(lp,@tmp,j);

    L_to_R(lp.Pol[0],TGeoid(g1));
    L_to_R(lp.Pol[1],TGeoid(g2));

    if g1.x > g2.x then
    Swap_Poly(lp)
  end
end;

function sys_Datum7(const sys: tsys): TDatum7;
var
  dat: TDatum7;
begin
  dat:=sys.dat;

  if sys.elp = 6 then
  if sys.prj in [prj_cuba,prj_cuba+1] then
  dat:=cu_Datum;

  Result:=dat
end;

function ru_sys(y: Double): tsys;
var
  s: tsys;
begin
  Fillchar(s,Sizeof(s),0);
  s.pps:=1; s.elp:=1; s.prj:=1;
  s.lc:=GLongitude(y);
  s.dat:=ru42_Datum;
  Result:=s
end;

function ru_list(l: Double): tsys;
var
  s: tsys;
begin
  Fillchar(s,Sizeof(s),0);
  s.pps:=1; s.elp:=1; s.prj:=1;
  s.lc:=zLongitude(l);
  s.dat:=ru42_Datum;
  Result:=s
end;

function Latitude(ch: char): double;
begin
  Result:=Radian((ord(ch)-ord('A'))*4,0,0)
end;

procedure Radian_to_Second(b,l: double; var x,y: longint);
begin
  x:=Round(b * 100000000);
  y:=Round(l * 100000000)
end;

procedure Second_to_Radian(x,y: longint; var b,l: double);
begin
  b:=x / 100000000; l:=y / 100000000
end;

procedure s_BL_BL(const r1: tgeoid; const s1,s2: tsys; out r2: tgeoid);
var
  t: tgeoid; h: Double;
begin
  t:=r1; if not elp_Equal(s1.elp,s2.elp) then
  GEO_GEO(t.b,t.l,0, s1.elp,s2.elp, @s1.dat,@s2.dat, t.b,t.l,h);
  r2:=t
end;

procedure BL_WGS(const r1: tgeoid; const s1: tsys; out r2: tgeoid);
var
  t: tgeoid; h: Double;
begin
  t:=r1; if s1.elp <> 9 then
  GEO_WGS(r1.b,r1.l,0, s1.elp,@s1.dat, t.b,t.l,h);
  r2:=t
end;

procedure WGS_BL(const r1: tgeoid; const s2: tsys; out r2: tgeoid);
var
  t: tgeoid; h: Double;
begin
  t:=r1; if s2.elp <> 9 then
  WGS_GEO(r1.b,r1.l,0, s2.elp, @s2.dat, t.b,t.l,h);
  r2:=t
end;

function lg_Equal(const T1,T2: lg_Transit): Boolean;
begin
  Result:=sys_Equal(T1.s,T2.s)
end;

function lg_Rect(var lg: lg_Transit): bool;
const
  eps = 0.0001;
var
  i: int; lt,rb,g: TGauss;
begin
  Result:=true;

  if lg.s.prj = 0 then
  with lg.g[0] do
  sys_projection(lg.s, x,y);

  Max_Gauss_Bound(@lg.g,4, lt,rb);

  for i:=0 to 3 do begin
    g:=lg.g[i];
    if (eps_Equal(g.x,lt.x,eps) or
        eps_Equal(g.x,rb.x,eps)) and

       (eps_Equal(g.y,lt.y,eps) or
        eps_Equal(g.y,rb.y,eps)) then

    else begin
      Result:=false; Break
    end
  end
end;

function Is_msk_sys(const s: tsys): Boolean;
begin
  with s do
  Result:=(x0 < -1) or (y0 < -1)
end;

procedure msk_XY_BL(x,y: Double; const s: tsys; out b,l: double);
begin
  with s do
  if (x0 < -1) or (y0 < -1) then
    sys_XY_BL(x-x0,y-y0,s,b,l)
  else
    sys_XY_BL(x,y - prj_zoneY(s), s, b,l)
end;

procedure msk_BL_XY(b,l: Double; const s: tsys; out x,y: double);
begin
  sys_BL_XY(b,l,s,x,y);

  with s do
  if (x0 < -1) or (y0 < -1) then begin
    x:=x + s.x0; y:=y + s.y0;
  end
  else y:=y + prj_zoneY(s)
end;

procedure lc_XY_BL(x,y, lc: Double; const s: tsys; out b,l: double);
var
  t: tsys;
begin
  t:=s; t.lc:=lc; prj_XY_BL(x,y, t, b,l)
end;

procedure lc_BL_XY(b,l, lc: Double; const s: tsys; out x,y: double);
var
  t: tsys;
begin
  t:=s; t.lc:=lc; prj_BL_XY(b,l, t, x,y)
end;

function prj_outside(x,y: Double; const s: tsys): xgeoid;
var
  r: xgeoid;
begin
  r.x:=x; r.y:=y; r.s:=s; if r.s.pps = 1 then
  sys_XY_BL(x,y, s, r.x,r.y); Result:=r
end;

function prj_inside(const x: xgeoid; const s: tsys): tgauss;
var
  g: tgauss; r: tgeoid; pps1: Integer;
begin
  g.x:=x.x; g.y:=x.y; pps1:=0;

  if x.s.pps = 1 then begin
    r.b:=g.x; r.l:=g.y; pps1:=1;
  end else

  if (s.pps = 1)
  or not prj_Equal(x.s,s) then begin
    r:=x_XY_BL(g,x.s); pps1:=1;
  end;

  if pps1 = 1 then begin
    if not elp_Equal(x.s.elp,s.elp) then
    s_BL_BL(r, x.s,s, r);

    if s.pps = 1 then
      sys_BL_XY(r.b,r.l, s, g.x,g.y)
    else
    if s.prj > 0 then
      prj_BL_XY(r.b,r.l, s, g.x,g.y)
    else
      prj_BL_XY(r.b,r.l, x.s, g.x,g.y);
  end;

  Result:=g
end;

function prj_to_xy(const r: xgeoid;
                   const sys: tsys;
                   out g: TGauss): Integer;
var
  gx,gy,gh: Double; rs: tsys;
begin
  Result:=sys.pps; gx:=r.x; gy:=r.y;

  rs:=r.s; with sys do

  if rs.pps = 0 then begin

    if prj > 0 then
    if rs.prj > 0 then

    if not prj_Equal(rs,sys) then begin
      prj_XY_BL(gx,gy, rs, gx,gy);

      if r.s.elp <> elp then
      GEO_GEO(gx,gy,0, rs.elp,elp,
              @rs.dat,@dat, gx,gy,gh);

      prj_BL_XY(gx,gy, sys, gx,gy);
    end else
    if (r.s.x0 <> x0)
    or (r.s.y0 <> y0) then begin
      gx:=gx-r.s.x0+x0;

      if Abs(rs.y0) = 0 then gy:=gy-prj_zoneY(rs)
                        else gy:=gy-rs.y0;

      if Abs(y0) = 0 then gy:=gy+prj_zoneY(sys)
                     else gy:=gy+y0;
    end

  end else
  if prj = 0 then
    prj_BL_XY(gx,gy, r.s, gx,gy)
  else begin
    if r.s.elp <> elp then
    GEO_GEO(gx,gy,0, r.s.elp,elp,
            @r.s.dat,@dat, gx,gy,gh);

    if (pps = 1) or (prj > 0) then
      prj_BL_XY(gx,gy, sys, gx,gy)
    else
      prj_BL_XY(gx,gy, r.s, gx,gy)
  end;

  g.x:=gx; g.y:=gy
end;

function prj_to_lg(x,y: Double; const s1,s2: tsys): TGauss;
var
  r: TGeoid; g: TGauss; h: Double;
begin
  g.x:=x; g.y:=y;

  if not sys_Equal(s1,s2) then begin
    prj_XY_BL(x,y, s1, r.b,r.l);

    if not elp_Equal(s1.elp,s2.elp) then
    GEO_GEO(r.b,r.l,0, s1.elp,s2.elp, @s1.dat,@s2.dat, r.b,r.l,h);

    sys_BL_XY(r.b,r.l, s2, g.x,g.y);
  end else

  if s1.prj in [1..2] then
  g.y:=g.y - prj_zoneY(s1);

  Result:=g
end;

function prj_to_prj(x,y: Double; const s1,s2: tsys): TGauss;
var
  v1,v2: txyz; t: TGauss;
begin
  Result.x:=x; Result.y:=y;

  if not prj_Equal(s1,s2) then
  if (s1.pps > 0) or (s1.prj > 0) then
  if (s2.pps > 0) or (s2.prj > 0) then begin
    prj_XY_BL(x,y, s1, v1.x,v1.y); v1.z:=0;
    v2:=vGEO_GEO(v1,s1,s2);
    prj_BL_XY(v2.x,v2.y, s2, t.x,t.y);
    Result:=t
  end
end;

function x_XY_BL(const g: tgauss; const sys: tsys): tgeoid;
var
  x,y, b,l: double; a,iz: Integer; s: tsys;
begin
  x:=g.x; y:=g.y; s:=sys;

  if s.prj in [0..2] then
  if Abs(s.x0)+Abs(s.y0) > 1 then begin
    x:=x-s.x0; y:=y-s.y0;
  end
  else begin
    iz:=Trunc(y) div 1000000;
    y:=y - (iz*1000000 + 500000);

    if sys.prj = 2 then Dec(iz,30);
    a:=Round(iz*6)-6;

    s.lc:=(a-(a mod 6)+3) * Pi / 180
  end;

  sys_XY_BL(x,y, s, b,l);
  Result.b:=b; Result.l:=l
end;

function x_BL_XY(const r: tgeoid; const sys: tsys): tgauss;
var
  x,y: double; s: tsys;
begin
  s:=sys;
  if s.prj in [0..2] then
  if Abs(r.l - s.lc) > 6/180*Pi then
  s.lc:=zLongitude(r.l);

  sys_BL_XY(r.b,r.l, s, x,y);
  y:=y + prj_ZoneY(s);

  Result.x:=x; Result.y:=y
end;

function AB_Project(x,y: Double; const T1,T2: ab_Orient): TGauss;
var
  g: TGauss; r: TGeoid; z: double;
begin
  g:=T1.x_ab.Transit(x,y);

  if T2.sys.prj > 0 then begin

    if T1.sys.pps = 0 then begin
      g.x:=g.x - T1.zoneX;
      g.y:=g.y - T1.zoneY;
    end; T1.XY_BL(g, r);

    if T1.sys.elp <> T2.sys.elp then
    GEO_GEO(r.b,r.l,0,
            T1.sys.elp,T2.sys.elp,
            @T1.sys.dat,@T2.sys.dat,
            r.b,r.l,z);

    T2.BL_XY(r.b,r.l,g);

    if T2.sys.pps = 0 then begin
      g.x:=g.x + T2.zoneX;
      g.y:=g.y + T2.zoneY;
    end
  end;

  Result:=g
end;

function AB_Backup(x,y: Double; const T1,T2: ab_Orient): TPoint;
var
  g: TGauss; r: XGeoid; p: TPoint;
begin
  g.x:=x; g.y:=y;
  if T1.sys.pps = 1 then begin
    g.x:=x + T1.zoneX;
    g.y:=y + T1.zoneY;
  end;

  T1.x_G_to_R(g,r);
  T2.x_R_to_L(r,p); Result:=p
end;

function Gauss_Longitude(y: Double): Double;
var
  a,iz: Integer;
begin
  iz:=Trunc(y) div 1000000; a:=Round(iz*6)-6;
  Result:=(a-(a mod 6)+3) * Pi / 180;
end;

function zone_Projection(XY: PGPoly;
                         elp,prj: Integer;
                         BL: PGPoly): Boolean;
var
  p,q: tgauss; r: tgeoid;
  i: Integer; utm,zone: Double; s: tsys;
begin
  Result:=false; p:=XY[0];

  if p.x > 2000000 then
  if p.y > 1000000 then begin

    Result:=true; utm:=0;
    s:=sys7(1,prj,elp,0,0,0);
    if prj = 2 then utm:=30000000;
    s.lc:=Gauss_Longitude(p.y-utm);
    zone:=prj_ZoneY(s);

    for i:=0 to 3 do begin

      p:=XY[i]; p.y:=p.y - zone;

      if (p.x < 2000000)
      or (Abs(p.y) > 1000000) then begin
        Result:=false; Break
      end
      else begin
        sys_XY_BL(p.x,p.y, s, r.b,r.l);
        sys_BL_XY(r.b,r.l, s, q.x,q.y);

        if Gauss_Dist(p,q) > 2 then begin
          Result:=false; Break
        end;

        if Assigned(BL) then
        BL[i]:=tgauss(r)
      end
    end
  end
end;

function GPoly_normalize(G: PGPoly; n: Integer): Real3x3;
var
  i: Integer; s: Extended;
  x0,y0,k: Double; lt,rb,v: TGauss;
begin
  Max_Gauss_Bound(G,n, lt,rb);

  x0:=lt.x/2 + rb.x/2;
  y0:=lt.y/2 + rb.y/2;

  s:=0; for i:=0 to n-1 do begin
    v.x:=G[i].x - x0;
    v.y:=G[i].y - y0;
    s:=s + (v.x*v.x + v.y*v.y);
  end;

  s:=Sqrt(s/n); k:=1;
  if s > Small then k:=Sqrt(2)/s;

  Init_3x3(Result, -k*x0,-k*y0, k,k)
end;

function GPoly_Equal(G1,G2: PGPoly; n: Integer;
                     Eps: Double): Boolean;
var
  i: Integer;
begin
  Result:=true; for i:=0 to n-1 do
  if Gauss_Dist(g1[i],g2[i]) > Eps then
  begin Result:=false; Break end
end;

function GLine_Equal(G1,G2: PGLine; Eps: Double): Boolean;
begin
  Result:=false; if G1.N = G2.N then
  Result:=GPoly_Equal(@G1.Pol,@G2.Pol,G1.N+1,Eps)
end;

function gLongitude(y: double): double;
var
  a,iz: Integer;
begin
  Result:=0; iz:=Trunc(y) div 1000000;
  if iz > 0 then begin a:=(iz*6)-6;
    Result:=(a-(a mod 6)+3) * Pi / 180
  end
end;

function cLongitude(G: PGPoly): double;
var
  i: Integer;
begin
  Result:=0; for i:=0 to 3 do
  Result:=Result+G[i].y/4
end;

function xLongitude(G: PGPoly): double;
begin
  Result:=zLongitude(cLongitude(G))
end;

function iLongitude(G: PGPoly): double;
var
  a,m: integer;
begin
  R_to_G(cLongitude(G),0, a,m);
  iLongitude:=a div 6
end;

function prj_Longitude(p_lc,lc: Double; prj: Integer): Double;
var
  r: Double;
begin
  Result:=0;

  if prj in prj_lc then begin

    Result:=p_lc;

    if prj in [0,1,2] then
      r:=Radian(9,0,0)
    else
      r:=Radian(120,0,0);

    if (Result = 0)
    or (Result < lc-r)
    or (Result > lc+r) then

    if prj in [0,1,2] then
      Result:=zLongitude(lc)
    else
      Result:=lc

  end else

  if prj in [10..prj_deg] then

    Result:=p_lc

end;

function cLatitude(G: PGPoly): double;
var
  i: Integer;
begin
  Result:=0; for i:=0 to 3 do
  Result:=Result + G[i].x/4
end;

function minLatitude(G: PGPoly): double;
var
  i: Integer; x: double;
begin
  Result:=g[0].x; for i:=1 to 3 do begin
    x:=G[i].x; if Result > x then Result:=x
  end
end;

procedure xy_ru_bl(x,y: Double; out b,l: Double);
var
  lc: Double; zone: Integer;
begin
  lc:=gLongitude(y);
  zone:=Trunc(y / 1000000) * 1000000 + 500000;
  XY_to_BL(x,y-zone, lc,0,0, 1,1, b,l);
end;

procedure bl_ru_xy(b,l: Double; out x,y: Double);
var
  lc: Double;
begin
  lc:=zLongitude(l);
  BL_to_XY(b,l, lc,0,0, 1,1, x,y);
  y:=y + xZone(lc)
end;

procedure xy_wgs_gk(x1,y1: Double; out x2,y2: Double);
var
  lc,b,l,h,dy: Double; dat: TDatum7;
begin
  lc:=gLongitude(y1);
  dy:=Trunc(y1 / 1000000) * 1000000 + 500000;
  XY_to_BL(x1,y1-dy, lc,0,0, 9,1, b,l);

  dat:=ru42_Datum;
  WGS_GEO(b,l,0, 1, @dat, b,l,h);

  lc:=zLongitude(l); 
  BL_to_XY(b,l, lc,0,0, 1,1, x2,y2);
  y2:=y2 + xZone(lc)
end;

procedure bl_wgs_gk(b1,l1: Double; out b2,l2: Double);
var
  dat: TDatum7; h: Double;
begin
  dat:=ru95_Datum;
  WGS_GEO(b1,l1,0, 1, @dat, b2,l2,h);
end;

function dm_Nom_Scan(nom: string; _Double: Boolean;
                     out g: GOrient; out sc: Integer): string;

function AtoW(var fa; l: Integer): Integer;
var
  a: array[0..15] of char absolute fa;
  int,i: Integer; ch: char;
begin
  Result:=0; int:=0; i:=0;

  while i < L do begin ch:=a[i];
    if (ch >= '0') and (ch <= '9') then begin
      int:=(int*10)+ord(ch)-ord('0'); Inc(i)
    end else Exit
  end;

  Result:=int
end;

function Sqrt(var nom: string; ind: Integer;
              out _j: Integer): Boolean;
var
  j: Integer;
begin
  Result:=true; _j:=0;

  if length(nom) >= ind then begin
    Result:=false; j:=AtoW(nom[ind],1);

    if ind = 20 then begin
      if not (j in [1..9]) then Exit;
    end else

    if not (j in [1..4]) then Exit;

    _j:=j; Result:=true
  end
end;

var
  i,j,k,d,ind,col,row: Integer;
  ch: char; _j: array[5..9] of Integer;
  b,l,db,dl: double;
begin
  Result:=''; sc:=0; for i:=0 to 3 do
  begin g[i].x:=0; g[i].y:=0 end;

  i:=Pos('X',nom); if i = 0 then
  i:=Pos('x',nom); if i > 0 then
  nom[0]:=chr(i-1); i:=1;

  if length(nom) > 2 then
  if nom[1] in ['A'..'Z'] then
  if nom[2] = '-' then nom:='0.'+nom;

  if (length(nom) >= 6) and (nom[2] = '.') then begin

    if nom[1] = 'X' then nom[1]:='0'; ch:=UpCase(nom[3]);

    if ch in ['A'..'Z'] then begin

      if ch < 'P' then _Double:=false;

      b:=Latitude(ch); i:=AtoW(nom[5],2);
      if (i < 1) or (i > 70) then Exit;

      l:=Radian((i-31)*6,0,0); k:=1; nom[3]:=ch;
      db:=Radian(4,0,0); dl:=Radian(6,0,0); sc:=1;

      if nom[1] = '1' then b:=-b-db;

      if length(nom) >= 10 then begin
        if UpCase(nom[8]) <> 'X' then
        if UpCase(nom[10]) = 'X' then begin
          nom[10]:=nom[9]; nom[9]:=nom[8]; nom[8]:='0'
        end;

        j:=AtoW(nom[8],3);

        if j > 0 then begin

          k:=12; sc:=4;

          if j > 500 then begin
            Dec(j,500); k:=2; sc:=2
          end else
          if j > 200 then begin
            Dec(j,200); k:=6; sc:=3
          end;

          if j > k*k then Exit; Dec(j);

          ind:=12; if sc = 4 then

          if (length(nom) >= 14) and
             (nom[13] <> '-') then begin
            i:=AtoW(nom[12],3);
            if i > 256 then Exit else
            if i > 0 then begin
              row:=(i-1) div 16;
              col:=(i-1) mod 16;
              d:=8; for i:=5 to 8 do begin ind:=1;
                if col >= d then begin Inc(ind); Dec(col,d) end;
                if row >= d then begin Inc(ind,2); Dec(row,d) end;
                Inc(sc); _j[sc]:=ind; d:=d div 2
              end;

              if length(nom) >= 16 then begin
                ind:=AtoW(nom[16],1);
                if not (ind in [1..9]) then Exit;
                Inc(sc); _j[sc]:=ind
              end
            end
          end else

          while ind <= 20 do begin
            if not Sqrt(nom,ind,i) then Exit;
            if i = 0 then Break else begin
              Inc(sc); _j[sc]:=i
            end; Inc(ind,2)
          end;

          if _Double then

          if sc <= 4 then begin
            if ch >= 'T' then begin
              case sc of
            2:  if not Odd(i) then Exit else
                if j mod 2 <> 0 then Exit;
            3:  if j mod 3 <> 0 then Exit;
            4:  if j mod 4 <> 0 then Exit;
              end
            end else
            if ch >= 'P' then begin
              if j mod 2 <> 0 then Exit;
            end
          end else
          if sc < 9 then
          if not Odd(_j[sc]) then Exit;

          db:=db/k; dl:=dl/k;

          b:=b + db*(k-(j div k)-1);
          l:=l + dl*(j mod k);

          if _Double then
          if sc <= 4 then
          if ch >= 'T' then begin
            if k = 6 then dl:=dl*3 else
            dl:=dl*4
          end else
          if ch >= 'P' then dl:=dl*2;

          if sc > 4 then begin
            for i:=5 to sc do begin
              d:=2; if i = 9 then d:=3;
              db:=db/d; dl:=dl/d;

              j:=_j[i]-1;

              b:=b + db*(1-(j div d));
              l:=l + dl*(j mod d);
            end;

            if _Double then
            dl:=dl + dl
          end
        end
      end else
      if _Double then
      if ch >= 'T' then begin
        if (i mod 4) <> 1 then Exit;
        dl:=dl*4
      end else
      if ch >= 'P' then begin
        if (i mod 2) <> 1 then Exit;
        dl:=dl*2
      end;

      if sc in [1..9] then begin

        case sc of
      1:  i:=7;
      2,
      3,
      4:  i:=11;
      5:  i:=13;
      6:  i:=15;
      7:  i:=17;
      8:  i:=19;
      9:  i:=21;
        end;

        if sc >= 8 then
        if nom[13] <> '-' then
        Dec(i,4);

        g[0].x:=b;    g[0].y:=l;
        g[1].x:=b+db; g[1].y:=l;
        g[2].x:=b+db; g[2].y:=l+dl;
        g[3].x:=b;    g[3].y:=l+dl;
        g[4].x:=b;    g[4].y:=l+dl/2;

      end else Exit

    end
  end;

  nom[0]:=chr(i-1); Result:=nom
end;

function sxf_Nom(scale: longint; nom: string): string;

function intScan(s: string; var int: Integer): string;
var
  i: Integer; ch: char;
begin
  int:=0; Result:='';

  for i:=1 to length(s) do begin
    ch:=s[i]; if ch in ['0'..'9'] then
      int:=(int*10)+ord(ch)-ord('0')
    else
    if ch = 'À' then int:=1 else
    if ch = 'Á' then int:=2 else
    if ch = 'Â' then int:=3 else
    if ch = 'Ã' then int:=4 else

    begin
      Delete(s,1,i); Result:=s; Break
    end
  end
end;

var
  i,j: Integer; s: string;
begin
  Result:='';

  if length(nom) >= 6 then begin
    nom:=WinString(nom);
    s:=Copy(nom,1,6); Delete(nom,1,6);

    if scale = 1000000 then
      Result:=s
    else
    if scale <= 500000 then
    if length(nom) >= 2 then
    if nom[1] = '-' then begin
      Delete(nom,1,1); nom:=intScan(nom,i);

      if i > 0 then begin
        if scale = 500000 then begin
          if i <= 2*2 then begin
            Result:=s+'-'+IntToStr(500+i)
          end
        end else
        if scale = 200000 then begin
          if i <= 6*6 then begin
            Result:=s+'-'+IntToStr(200+i)
          end
        end else
        if scale <= 100000 then begin
          if i <= 12*12 then begin
            s:=s+'-'+xIntToStr(i,3,'0');
            for j:=1 to 2 do
            if length(nom) > 0 then begin
              nom:=intScan(nom,i);
              if i in [1..4] then
                s:=s+'-'+IntToStr(i)
              else Break
            end;

            Result:=s
          end
        end

      end

    end else Result:=s
  end;
end;

function dm_Nom_Scale(nom: string; _Double: Boolean): Integer;
var
  g: GOrient; sc: Integer; s: String;
begin
  Result:=0;
  s:=dm_Nom_Scan(nom,_Double,g,sc);
  if Length(s) > 0 then Result:=sc
end;

function dm_Nom_Trunc(nom: string; _Double: Boolean): string;
var
  sc: Integer; g: gOrient;
begin
  Result:=dm_Nom_Scan(nom,_Double,g,sc)
end;

function nom200(lat,lon: double): string;
var
  b,l,z,i,j: int; s,t: String;
begin
  b:=Round(lat*180*60/Pi);
  l:=Round(lon*180*60/Pi);

  s:=Char(ord('A')+(b div (4*60)));

  z:=30 + (l div (6*60))+1;
  s:=s+ItoA(z,2,t);

  i:=(l mod (6*60)) div 60;
  j:=(b mod (4*60)) div 40;
  j:=5 - j;

  s:=s+ItoA(j*6+i+1,2,t);
  Result:=s
end;

procedure Init_lg_Transit(out lg: lg_Transit);
begin
  FillChar(lg,SizeOf(lg),0);

  lg.l[0].x:=0;     lg.l[0].y:=0;
  lg.l[1].x:=10000; lg.l[1].y:=0;
  lg.l[2].x:=10000; lg.l[2].y:=10000;
  lg.l[3].x:=0;     lg.l[3].y:=10000;
  lg.l[4].x:=0;     lg.l[4].y:=0;
  lg.g:=lg.l;
end;

function verify_lc(lc,l1,l2: Double; prj: Integer): Double;
var
  dl: Double;
begin
  if prj in prj_lc then begin

    if prj in [0,1,2] then
      dl:=Radian(9,0,0)
    else
      dl:=Radian(120,0,0);

    if (lc < l1) or (lc > l2) then

    if (Abs(lc-l2) > dl)
    or (Abs(lc-l1) > dl) then begin
      lc:=(l1+l2)/2; if prj <= 2 then
      lc:=zLongitude(lc)
    end
  end;

  Result:=lc
end;

procedure Project_lg_Transit(var lg: lg_Transit);
var
  i: Integer; lt,rb: TGauss;
begin
  with lg do begin
    Max_Gauss_Bound(@g,4, lt,rb);

    if s.prj in prj_lc then
    s.lc:=verify_lc(s.lc, lt.y,rb.y,s.prj);

    for i:=0 to 4 do
    sys_BL_XY(g[i].x,g[i].y, s, l[i].x,l[i].y);
  end
end;

function Restore_lg_Transit(var lg: lg_Transit;
                            fr: PGPoly; n: Integer): Integer;

function min_dist(const p: TGauss; gp: PGPoly; n: Integer): Double;
var
  i: Integer;
begin
  Result:=Gauss_dist(p,gp[0]);
  for i:=1 to n-1 do
  Result:=Min(Result,Gauss_dist(p,gp[i]))
end;

var
  i,j,k,n1: Integer;
  t: GOrient; tr,bt: Real3x3;
  d1,d2: Double; a,b: TGauss;
begin
  if n in [2..3] then
  if Assigned(fr) then

  with lg do begin k:=n; t:=g;

    if s.pps = 1 then begin
      Max_Gauss_Bound(@g,n, t[0],t[1]);

      if s.prj in prj_lc then
      s.lc:=verify_lc(s.lc, t[0].y,t[1].y,s.prj);

      for i:=0 to n-1 do
      sys_BL_XY(g[i].x,g[i].y, s, t[i].x,t[i].y);
    end;

    if n = 2 then begin
      if Solve_vector(@l,@t, tr) then k:=4
    end else
    if n = 3 then begin
      if Solve_affine(@l,@t, tr,bt) then k:=4
    end;

    n1:=4; for i:=0 to 3 do t[i]:=fr[i];

    if k = 4 then
    while n < 4 do begin

      j:=0; d1:=min_dist(t[0],@l,n);

      for i:=1 to n1-1 do begin
        d2:=min_dist(fr[i],@l,n);
        if d2 > d1 then begin
          d1:=d2; j:=i
        end
      end;

      a:=t[j]; Dec(n1); t[j]:=t[n1];

      b:=Projective_3x3(a.x,a.y,tr);

      if s.pps = 1 then
      sys_XY_BL(b.x,b.y, s, b.x,b.y);

      l[n]:=a; g[n]:=b; Inc(n)
    end

  end; Result:=n
end;

function East_lg_Transit(var a: lg_Transit;
                         const b: lg_Transit): Integer;

function lg_transit_dir(const lg: lg_Transit): Integer;
var
  i,dir: Integer; y: Double;
begin
  Result:=0;

  if lg.s.pps = 1 then begin

    dir:=0;
    for i:=0 to 3 do begin
      y:=lg.g[i].y;
      if y < 0 then begin
        if dir = 0 then dir:=-1 else
        if dir = +1 then begin
          dir:=0; Break
        end
      end else
      if y > 0 then begin
         if dir = 0 then dir:=+1 else
        if dir = -1 then begin
          dir:=0; Break
        end
      end
    end;

    Result:=dir
  end
end;

procedure lg_transit_swap(var lg: lg_Transit; sign: Integer);
var
  i: Integer; df: Double;
begin
  if lg.s.pps = 1 then begin

    df:=sign*2*Pi;

    with lg.s do
    if (df > 0) <> (lc >= 0) then
    lc:=lc + df;

    for i:=0 to 3 do
    with lg.g[i] do y:=y + df
  end
end;

var
  i,d1,d2: Integer; y1,y2: Double;
begin
  d1:=0;

  if a.s.pps = 1 then
  if b.s.pps = 1 then begin
    d1:=lg_transit_dir(b);
    d2:=lg_transit_dir(a);

    y1:=0; y2:=0;
    for i:=0 to 3 do begin
      y1:=y1 + a.g[i].y/4;
      y2:=y2 + b.g[i].y/4;
    end;

    if Abs(y1-y2) > Pi then

    if d1 < 0 then begin
      if d2 > 0 then lg_transit_swap(a,-1)
    end else
    if d1 > 0 then begin
      if d2 < 0 then lg_transit_swap(a,+1)
    end
  end;

  Result:=d1
end;

function wgs_lg_Transit(const lg: lg_Transit; Fr: PGPoly): Boolean;
var
  i: int; w: GOrient; g: TGauss; gz: Double; s: tsys;
begin
  Result:=false; s:=lg.s;
  if (s.pps = 1) or (s.prj > 0) then begin

    for i:=0 to 3 do begin
      g:=lg.g[i]; if s.pps = 0 then
      prj_XY_BL(g.x,g.y,s,g.x,g.y);

      if s.elp <> 9 then
      GEO_WGS(g.x,g.y,0, s.elp,@s.dat, g.x,g.y,gz);
      w[i]:=g
    end;

    Max_Gauss_Bound(@w,4,Fr[0],Fr[1]);
    Result:=true
  end
end;

function lg_get_scale(const lg: lg_Transit): int;
var
  i,db,scale: int;
  lt,rb,g: TGauss;
begin
  scale:=100000;
  if lg.s.prj = 1 then begin
    for i:=0 to 3 do begin
      g:=lg.g[i];
      prj_XY_BL(g.x,g.y,lg.s,g.x,g.y);
      if i > 0 then
        Max_GPort(lt,rb,g)
      else begin
        lt:=g; rb:=g
      end
    end;

    db:=Round((rb.x-lt.x)*180*60/Pi);

    if db < 7 then scale:=25000 else
    if db < 15 then scale:=50000 else
    if db < 30 then scale:=100000 else
    if db < 60 then scale:=200000 else
    if db < 3*60 then scale:=500000 else
                      scale:=1000000
  end;

  Result:=scale
end;

procedure Link_lg_prj(var a: tsys; const b: tsys);
var
  s: tsys;
begin
  if a.pps = 0 then
  if a.prj = 0 then
  if b.prj > 0 then begin
    s:=a; a:=b; a.pps:=0;
    if s.elp > 0 then begin
      a.elp:=s.elp; a.dat:=s.dat
    end
  end;
end;

procedure Link_lg_Transit(var a: lg_Transit;
                          const b: lg_Transit);
var
  i: int; g: tgauss; r,t: tgeoid;
  asys,bsys: tsys; dx,dy,y1,y2: double;
begin
  asys:=a.s; bsys:=b.s;

  if (asys.prj > 0) or (bsys.prj > 0) then begin
    with a.g[0] do sys_Projection(asys,x,y);
    with b.g[0] do sys_Projection(bsys,x,y);
  end;

  Link_lg_prj(bsys,asys);
  Link_lg_prj(asys,bsys);

  if sys_Equal(asys,bsys) then begin

    if asys.pps = 0 then
    if bsys.pps = 0 then
    if (asys.x0 <> bsys.x0)
    or (asys.y0 <> bsys.y0) then begin

      dx:=bsys.x0 - asys.x0;

      y1:=asys.y0; y2:=bsys.y0;
      if y1 = 0 then y1:=prj_Zoney(asys);
      if y2 = 0 then y2:=prj_Zoney(bsys);

      for i:=0 to 4 do begin
        g:=a.g[i];
        g.x:=g.x+dx;
        g.y:=g.y-y1+y2;
        a.g[i]:=g
      end;

      a.g[5]:=a.g[0]
    end
  end
  else begin

    for i:=0 to 4 do begin
      g:=a.g[i]; r:=tgeoid(g);

      if bsys.pps = 0 then begin
        if asys.pps = 1 then begin

          if asys.elp <> bsys.elp then begin
            g:=gGEO_GEO(g,asys,bsys);
            r:=tgeoid(g)
          end;

          g:=x_BL_XY(r, bsys)
        end
      end else
      if bsys.pps = 1 then begin
        if asys.pps = 0 then
        prj_XY_BL(g.x,g.y,asys,r.b,r.l);

        g:=TGauss(r);
        if asys.elp <> bsys.elp then
        g:=gGEO_GEO(g,asys,bsys)
      end;

      a.g[i]:=g
    end;

    a.g[5]:=a.g[0]
  end;

  a.s:=bsys; East_lg_Transit(a,b)
end;

function geo_to_lg(const x: xgeoid; const sys: tsys): TGauss;
var
  g: txyz;
begin
  if sys.pps = 0 then
    Result:=geo_to_prj(x,sys)
  else begin
    g.x:=x.x; g.y:=x.y; g.z:=0;

    if x.s.pps = 0 then
    prj_XY_BL(g.x,g.y, x.s, g.x,g.y);

    if x.s.elp <> sys.elp then
    g:=vGEO_GEO(g, x.s,sys);

    Result:=PGauss(@g)^
  end
end;

function prj_xy_to_x(const g: tgauss; const sys: tsys): xgeoid;
var
  r: xgeoid;
begin
  r.x:=g.x; r.y:=g.y; r.s:=sys;
  if r.s.pps = 1 then
  prj_XY_BL(r.x,r.y, sys, r.x,r.y);
  Result:=r
end;

function prj_x_to_x(const g: xgeoid; const sys: tsys): xgeoid;
var
  t: xgeoid; h: Double;
begin
  t:=g;

  if t.s.pps <> sys.pps then

  if t.s.pps = 0 then
    prj_XY_BL(t.x,t.y, t.s, t.x,t.y)
  else
    prj_BL_XY(t.x,t.y, t.s, t.x,t.y);

  if sys.pps = 1 then
  if not elp_Equal(t.s.elp,sys.elp) then
  GEO_GEO(t.x,t.y,0, t.s.elp,sys.elp, @t.s.dat,@sys.dat, t.x,t.y,h);

  t.s:=sys; Result:=t;
end;

function lg_Precision(const T: lg_Transit): Double;
var
  lg: AB_Orient;
begin
  lg.Init; lg.Assign(T,T.s.lc);
  Result:=lg.lg_Delta
end;

function lg_Contains_Point(const lg: lg_Transit;
                           const x: xgeoid): Boolean;
var
  p, lt,rb: tgauss;
begin
  p:=geo_to_lg(x,lg.s);
  Max_Gauss_Bound(@lg.g,4, lt,rb);
  Result:=GaussContainsPoint(lt,rb, p)
end;

procedure Update_mm_transit(var lg: lg_Transit; x,y,w,h: double);
var
  lt,rb: tgauss;
begin
  Max_Gauss_Bound(@lg.l,4, lt,rb);

  FillChar(lg,SizeOf(lg),0);
  Bound_to_GOrient(lt,rb, @lg.l);

  Frame_to_GOrient(y,x, h,w, @lg.g);

  lg.g[4]:=lg.g[0];
  lg.g[0]:=lg.g[1];
  lg.g[1]:=lg.g[2];
  lg.g[2]:=lg.g[3];
  lg.g[3]:=lg.g[4];
end;

procedure Sort_lg_Transit(l,g: PGPoly);
var
  lt,rb,v: tgauss; d,t: double;
  i,j: Integer; R: GOrient;
begin
  Max_Gauss_Bound(g,4,lt,rb);
  Bound_to_GOrient(lt,rb,@R);

  for i:=0 to 3 do begin

    v:=R[i]; d:=Gauss_Dist(v,g[i]);

    for j:=i+1 to 3 do begin

      t:=Gauss_Dist(v,g[j]);

      if t < d then begin
        d:=t; Swap_TGauss(g[i],g[j]);
        if l <> nil then Swap_TGauss(l[i],l[j])
      end

    end
  end

end;

function Local_lg_Transit(var lg: lg_Transit;
                          L: PLPoly; ppm: Float): Float;
var
  I: Integer; a,b,c: TPoint;
  lt,rb: tgauss;
begin
  if lg.s.pps = 0 then
    Result:=Gauss_to_Local(@lg.g,L,ppm)
  else begin
    Max_Gauss_Bound(@lg.g,4, lt,rb);
    lg.G[4].x:=lt.x; lg.G[4].y:=lt.y/2 + rb.y/2;

    Project_lg_Transit(lg);
    Result:=Gauss_to_Local(@lg.l,L,ppm);

    if lg.s.prj in [1,2,5] then begin
      a:=L[1]; b:=L[2]; c:=L[2];
      c.y:=a.y; for i:=0 to 4 do
      Rotate_LPoint(a,b,c, L[i],L[i])
    end
  end;
end;

function Gauss_to_Local(G: PGPoly; L: PLPoly; ppm: Float): Float;
var
  loop,i,rc: int; lt,rb,t: TGauss; p: TPoint; k,e: double;
begin
  k:=1; if ppm >= 0.01 then k:=ppm;

  Max_Gauss_Bound(G,5, lt,rb);

  for loop:=1 to 100 do begin

    rc:=0; e:=k*2;
    for i:=0 to 4 do begin
      t:=G[i];
      p.X:=Round((t.y-lt.y)*k);
      p.Y:=Round((rb.x-t.x)*k);
      L[i]:=p;

      if Abs(p.X/k - (t.y-lt.y)) < e then
      if Abs(p.Y/k - (rb.x-t.x)) < e then
      Inc(rc);
    end;

    if rc = 5 then Break;
    if k < 1E-3 then Break;
    k:=k/2
  end;

  Result:=k
end;

procedure Max_Project_Bound(w,h: Double; const Tr: Real3x3;
                            Fr: PGPoly; out lt,rb: tgauss);
var
  G: GOrient;
begin
  G[0]:=Transit_3x3(0,0,Tr);
  G[1]:=Transit_3x3(w,0,Tr);
  G[2]:=Transit_3x3(w,h,Tr);
  G[3]:=Transit_3x3(0,h,Tr);
  G[4]:=G[0];

  if Assigned(Fr) then
  Move(G,Fr^,Sizeof(TGauss)*5);

  Max_Gauss_Bound(@G,4,lt,rb);
end;

function xGeoid_Dist(const g1,g2: XGeoid): Double;
var
  a,b: TGauss; f: Double;
begin
  a:=_Gauss(g1.x,g1.y);
  b:=_Gauss(g2.x,g2.y);

  if g1.s.pps = 0 then
    Result:=Gauss_Dist(a,b)
  else begin
    Result:=Geoid_Dist(a.x,a.y, b.x,b.y, f);
    if Result < 1 then Result:=Gauss_Dist(a,b)
  end
end;

function Is_prj(x,y: Double; elp,prj: Integer; lc: Double): Boolean;
var
  b,l,_x,_y: Double;
begin
  Result:=false;

  XY_to_BL(x,y, lc,0,0, elp,prj, b,l);
  BL_to_XY(b,l, lc,0,0, elp,prj, _x,_y);

  if Abs(_x-x) < 1 then
  if Abs(_y-y) < 1 then
  Result:=true
end;

function gk_to_lc(x,y: Double; elp: int; var lc: Double): Boolean;
var
  l0: Double;
begin
  Result:=false; lc:=0;

  if y >= 1000000 then
  if y <= 60000000 then begin
    l0:=GLongitude(y);
    if l0 > Pi then l0:=l0-Pi*2;
    y:=y - xZone(l0);

    if Is_prj(x,y, elp,1, l0) then begin
      lc:=l0; Result:=true
    end
  end
end;

function utm_to_lc(x,y: Double; elp: int; var lc: Double): Boolean;
var
  l0: Double;
begin
  Result:=false; lc:=0;

  if y >= 1000000 then
  if y <= 60000000 then begin
    y:=y - 30000000;
    if y < 0 then y:=y + 60000000;
    l0:=GLongitude(y);
    if l0 > Pi then l0:=l0-Pi*2;
    y:=y - xZone(l0);

    if Is_prj(x,y, elp,2, l0) then begin
      lc:=l0; Result:=true
    end
  end
end;

function gk_projection(x,y: Double; elp: Integer;
                       var lc: Double): Boolean;
var
  l0: Double;
begin
  Result:=false; lc:=0;

  if y >= 2000000 then
  if y <= 31000000 then begin
    l0:=GLongitude(y);
    y:=y - xZone(l0);

    if Is_prj(x,y, elp,1, l0) then begin
      lc:=l0; Result:=true
    end
  end
end;

function utm_projection(x,y: Double; elp: Integer;
                        var lc: Double): Boolean;
var
  l0: Double;
begin
  Result:=false; lc:=0;

  if (y >= 31000000) and
     (y <= 61000000) then begin
    y:=y - 30000000;
    l0:=GLongitude(y);
    y:=y - xZone(l0);

    if Is_prj(x,y, elp,2, l0) then begin
      lc:=l0; Result:=true
    end
  end else
  if (y >= 1000000) and
     (y < 31000000) then begin
    l0:=GLongitude(y);
    y:=y - xZone(l0);

    l0:=-Pi + l0;
    if Is_prj(x,y, elp,2, l0) then begin
      lc:=l0; Result:=true
    end
  end
end;

function sys_projection(var sys: tsys; x,y: Double): Integer;
var
  l0: Double;
begin
  with sys do
  if (pps = 0) and (prj = 0) then

  if lc <> 0 then begin

    if Is_prj(x,y, elp,1, lc) then prj:=1 else
    if Is_prj(x,y, elp,2, lc) then prj:=2;

    if prj > 0 then begin
      sys.x0:=0; sys.y0:=0
    end

  end else

  if gk_projection(x,y,elp,l0) then begin
    prj:=1; lc:=l0; if elp = 0 then begin
      elp:=1; dat:=ru42_Datum
    end
  end else
  if utm_projection(x,y,elp,l0) then begin
    prj:=2; lc:=l0; if elp = 0 then elp:=9
  end;

  Result:=sys.prj
end;

function rsw_projection(var sys: tsys; x,y: Double): int;
var
  l0: Double;
begin
  with sys do
  if (pps = 0) and (prj = 0) then
  if gk_projection(x,y,elp,l0) then begin
    prj:=1; lc:=l0; if elp = 0 then begin
      elp:=1; dat:=ru42_Datum
    end
  end;

  Result:=sys.prj
end;

function dm_Scale_Index(sc: Integer): Integer;
var
  i: Integer;
begin
  Result:=0;

  for i:=1 to dmScale_Max do
  if sc = dmScale[i] then begin
    Result:=i; Break
  end
end;

procedure Get_GBound_Dists(D: PDoubles; G: PGPoly);
begin
  D[0]:=Gauss_Dist(G[0],G[1]);
  D[1]:=Gauss_Dist(G[1],G[2]);
  D[2]:=Gauss_Dist(G[2],G[3]);
  D[3]:=Gauss_Dist(G[0],G[3]);
end;

procedure Get_GBound_Min_Max(G: PGPoly; out min_d,max_d: Double);
var
  l: array[0..3] of double; i: Integer;
begin
  Get_GBound_Dists(@l,G);

  min_d:=l[0]; max_d:=l[0];
  for i:=1 to 3 do begin
    min_d:=Min(min_d,l[i]);
    max_d:=Min(max_d,l[i])
  end;
end;

function Is_GBound(G: PGPoly; R: Double): Boolean;
var
  min_d,max_d: Double;
begin
  Result:=true;
  Get_GBound_Min_Max(G, min_d,max_d);
  if Abs(min_d) < R then Result:=false else
  if max_d/min_d > 100 then Result:=false
end;

function StrSys(Str: PChar; ed: Integer; const Sys: tsys): PChar;

function rStrCat(Str: PChar; v: Double; last: Boolean): PChar;
var
  i,m: int; t,e: Double; s: String;
begin
  m:=0; t:=Abs(v); e:=1E-6;
  for i:=1 to 6 do
  if Frac(t) <= e then
    Break
  else begin
    t:=t*10; e:=e/10; Inc(m)
  end;

  if m = 0 then
    s:=RealToStr(v,-1)
  else
    s:=RealToStr(v,m);

  Result:=pStrCat(Str,s);
  if not last then StrCat(Str,' ')
end;

var
  s: tsys; mask,pmask: Integer;
begin
  Result:=StrCopy(Str,''); s:=Sys;

  s.lc:=s.lc*180/Pi;
  s.b1:=s.b1*180/Pi;
  s.b2:=s.b2*180/Pi;
  s.b3:=s.b3*180/Pi;

  Datum_disp(s.dat);

  if ed < 0 then
    iStrCat(Str,s.pps,false)
  else
    iStrCat(Str,ed,false);

  with s do begin
    iStrCat(str,elp,false);
    iStrCat(str,prj,false);

    if prj in [0..prj_max] then begin

      mask:=0; pmask:=prj_mask[prj];

      if Abs(lc) > 0.1 then Inc(mask,1);
      if Abs(b1) > 0.1 then Inc(mask,2);
      if Abs(b2) > 0.1 then Inc(mask,4);
      if Abs(b3) > 0.1 then Inc(mask,8);

      if Abs(k0-1) < 0.1 then
      if Abs(k0-1) > 1E-8 then Inc(mask,16);

      mask:=mask and pmask;

      if prj in prj_Radius then
      if rn > 6000000 then Inc(mask,32);

      if Abs(x0) > 1 then Inc(mask,64);
      if Abs(y0) > 1 then Inc(mask,128);

      if mask <> 0 then begin
        fStrCat(str,lc,6,false); mask:=mask shr 1
      end;
      if mask <> 0 then begin
        fStrCat(str,b1,6,false); mask:=mask shr 1
      end;
      if mask <> 0 then begin
        fStrCat(str,b2,6,false); mask:=mask shr 1
      end;
      if mask <> 0 then begin
        fStrCat(str,b3,6,false); mask:=mask shr 1
      end;
      if mask <> 0 then begin
        fStrCat(str,k0,8,false); mask:=mask shr 1
      end;
      if mask <> 0 then begin
        rStrCat(str,rn,false); mask:=mask shr 1
      end;
      if mask <> 0 then begin
        rStrCat(str,x0,false);; mask:=mask shr 1
      end;
      if mask <> 0 then begin
        rStrCat(str,y0,false); mask:=mask shr 1
      end;
    end;

    pStrCat(str,'# ');

    rStrCat(str,dat.dX,false);
    rStrCat(str,dat.dY,false);
    rStrCat(str,dat.dZ,false);

    if (Abs(dat.wX) > 1E-3)
    or (Abs(dat.wY) > 1E-3)
    or (Abs(dat.wZ) > 1E-3)
    or (Abs(dat.m) > 1E-3) then begin
      fStrCat(str,dat.wX,6,false);
      fStrCat(str,dat.wY,6,false);
      fStrCat(str,dat.wZ,6,false);
      fStrCat(str,dat.m,6,false);
    end
  end
end;

function SysVerify(const Sys: tsys): Boolean;
var
  s: tsys;
begin
  Result:=false; s:=Sys;

  s.lc:=s.lc * 180 / Pi;
  s.b1:=s.b1 * 180 / Pi;
  s.b2:=s.b2 * 180 / Pi;

  Datum_disp(s.dat);

  if s.pps in [0,1] then
  if s.elp in [0..elp_Max] then
  if s.prj in [0..prj_Max] then

  if Abs(s.lc) < 360 then
  if Abs(s.b1) < 90 then
  if Abs(s.b2) < 90 then

  with s.dat do
  if Abs(dX) < 1000 then
  if Abs(dY) < 1000 then
  if Abs(dZ) < 1000 then

  if Abs(wX) < 1 then
  if Abs(wY) < 1 then
  if Abs(wZ) < 1 then
  if Abs(m)  < 1 then

  Result:=true
end;

function geoToken(Str: PChar; out sys: tsys): Boolean;
type
  TValueArray = Array[0..31] of Double;
var
  pc: PChar;
  i,n: Integer; s: tsys; v: Double;
  vp1,vp2: PDoubles; vv: TValueArray;
begin
  Result:=false; sys:=sys_nil;

  Fillchar(s,Sizeof(s),0);

  if IntToken(Str,s.elp) then
  if IntToken(Str,s.prj) then begin

    vp1:=@s.lc; vp2:=@s.dat;

    pc:=StrScan(Str,'#');
    if Assigned(pc) then begin

      pc[0]:=#0;
      LonToken(Str,s.lc);
      LatToken(Str,s.b1);
      LatToken(Str,s.b2);
      LatToken(Str,s.b3);

      RealToken(Str,s.k0);
      RealToken(Str,s.rn);
      RealToken(Str,s.x0);
      RealToken(Str,s.y0);

      StrCopy(Str,@pc[1]);
      for i:=0 to 6 do
      RealToken(Str,vp2[i])

    end
    else begin

      n:=0; Fillchar(vv,Sizeof(vv),0);

      while RealToken(Str,v) do
      if n < 8 then begin
        vv[n]:=v; Inc(n)
      end;

      if n <= 3 then
        for i:=0 to n-1 do vp1[i]:=vv[i]
      else
      if n >= 6 then begin
        for i:=0 to 2 do vp1[i]:=vv[i];
        for i:=0 to 6 do vp2[i]:=vv[i+3];
      end;
    end;

    for i:=0 to 3 do
    vp1[i]:=vp1[i]/180*Pi; // lc,b1,b2,b3

    Datum_norm(s.dat);

    if SysVerify(s) then begin
      Result:=true; Sys:=s
    end
  end
end;

function mifToken(Str: PChar; out Sys: tsys): Integer;
var
  ed: Integer;
begin
  Result:=10; Sys:=sys_nil;
  if IntToken(Str,ed) then begin
    if (ed > 0) and (ed <= 1000) then Result:=ed;
    if geoToken(Str,Sys) then Sys.pps:=1
  end
end;

function sysToken(Str: PChar; out Sys: tsys): Integer;
var
  pps: Integer;
begin
  Result:=-1; sys:=sys_nil;

  if IntToken(Str,pps) then
  if geoToken(Str,Sys) then begin
    Sys.pps:=pps; Result:=pps
  end
end;

function lg_LoadFrom(out lg: lg_Transit; Path: PChar): Boolean;
var
  vm: TReadFile;
begin
  Result:=false;

  Fillchar(lg,Sizeof(lg),0);

  vm:=TReadFile.Create;
  try
    if vm.ext_Open(Path,'.LG') then
    if vm.Size = Sizeof(lg) then begin
      vm.Load(0,lg,Sizeof(lg));

      if lg.s.pps in [0..1] then
      if lg.s.prj in [0..prj_max] then
      if lg.s.elp in [0..elp_max] then

      Result:=true
    end
  finally
    vm.Free
  end
end;

end.
