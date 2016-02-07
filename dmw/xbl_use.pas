unit xbl_use; interface

uses
  Math,otypes,convert;

const
  elp_Max = 22;

  prj_deg  = 29;
  prj_cuba = 30;
  prj_geo  = 32;

  prj_max = prj_geo;

  prj_synchro = [0,1,2,5,prj_cuba,prj_cuba+1];

  prj_Radius = [9,11..15,19,21..23];
  prj_tilted = prj_Radius + [25..27];

  prj_lc = [0,1,2,4,5,6,7,8];
  prj_b3 = [28];

  prj_mask: array[0..prj_max] of Byte =
  (0, 1,1,2,7, 3,1,7,7, 2,7,3,3, 3,3,3,1, 1,7,3,7, 3,3,3,1, 3,7,7, 31,0, 0,0,0);

function Ellipsoids_Count: Integer; stdcall;
function Set_Ellipsoid(elp: Integer): Integer; stdcall;
procedure Get_Ellipsoid(elp: Integer; out a,b: Double); stdcall;

procedure XY_to_BL(x,y, lc,b1,b2: double; elp,prj: byte; var b,l: double); stdcall;
procedure BL_to_XY(b,l, lc,b1,b2: double; elp,prj: byte; var x,y: double); stdcall;

procedure pc_33_bl_to_xy(B,L, L0: Double; out X,Y: Double); stdcall;

function Geoid_Dist(b1,l1, b2,l2: double; var fi: double): double; stdcall;
function Geoid_Forw(b1,l1, r,fi: double; out b2,l2: double): double; stdcall;

procedure get_n0_n1(b0: Double; elp: Integer;
                    out n0,n1: Extended); stdcall;

procedure bl_to_xyz(b,l,h: Double; elp: Integer;
                    out x,y,z: Double); stdcall;

function xyz_to_bl(x,y,z: Double; elp: Integer;
                   out _b,_l,_h: Double): Boolean; stdcall;

procedure GEO_GEO(b1,l1,h1: double; elp1,elp2: Integer;
                  geo1,geo2: PDatum7; out b2,l2,h2: double); stdcall;

procedure GEO_WGS(b1,l1,h1: double; elp: Integer;
                  geo: PDatum7; out b2,l2,h2: double); stdcall;

procedure WGS_GEO(b1,l1,h1: double; elp: Integer;
                  geo: PDatum7; out b2,l2,h2: double); stdcall;

function vGEO_GEO(const v: txyz; const s1,s2: tsys): txyz;
function gGEO_GEO(const g: TGauss; const s1,s2: tsys): TGauss;

function vGEO_WGS(const v: txyz; const s1: tsys): txyz;
function vWGS_GEO(const v: txyz; const s2: tsys): txyz;

function get_bl_scale(b,l: Double): Double;

procedure sys_XY_BL(x,y: Double; const s: tsys; out b,l: double);
procedure sys_BL_XY(b,l: Double; const s: tsys; out x,y: double);

function prj_Equal(const s1,s2: tsys): Boolean;
function sys_Equal(const s1,s2: tsys): Boolean;

function StrPrjParam(Str: PChar; const s: tsys): PChar;
function PrjParamStr(const s: tsys): String;

function zLongitude(lc: double): double;

function xZone(lc: double): double;
function iZone(lc: double): int;

function prj_zoneY(const s: tsys): Double;

procedure prj_XY_BL(x,y: Double; const s: tsys; out b,l: double);
procedure prj_BL_XY(b,l: Double; const s: tsys; out x,y: double);

procedure loc_XY_BL(x,y: Double; const s: tsys; out b,l: double);
procedure loc_BL_XY(b,l: Double; const s: tsys; out x,y: double);

function cuba_sys(var sys: tsys): Boolean;

function gp_to_prj(const g: TGauss; const gsys,psys: tsys): TGauss;
function geo_to_prj(const g: TGeoPoint; const sys: tsys): TGauss;
function geo_to_xy(const g: TGeoPoint; const sys: tsys): TGauss;

function geo_to_sys(g: PGeoPoint; const s: tsys; out o: TGauss): Boolean;

function Get_wgs_sys(const lt,rb: TGauss): tsys;

function x_prj_to_prj(x,y: Double; const s1,s2: tsys): TGauss;

function Get_sys_mpp(mpp: double;
                     const g: TGeoPoint;
                     const dest: tsys): double;

function sys_yandex_mpp(mpp: double;
                        const g: TGeoPoint): double;

function yandex_sys_mpp(mpp: double;
                        const g: TGeoPoint): double;

function Get_yandex_dist(const g1,g2: TGauss; const sys: tsys): double;

function Get_yandex_mpp(lev: int): double;
function Get_yandex_lev(mpp: double): int;

function Get_yandex_poi(b,l: double;
                        const sys: tsys;
                        lev: int): double;

function rmu_merc(bc: double): double;

procedure get_merc_rect(b,l,rad: double;
                        out lt,rb: TGauss);

implementation

uses
  Sysutils;

const
  dll_bl = '_bl';

var
  pi2: Double;

function Ellipsoids_Count: Integer; external dll_bl;
function Set_Ellipsoid(elp: Integer): Integer; external dll_bl;
procedure Get_Ellipsoid(elp: Integer; out a,b: Double); external dll_bl;

function Set_Radius(Rn: Double): Double; stdcall; external dll_bl;
procedure Set_B2(B2: Double); stdcall; external dll_bl;

procedure XY_to_BL(x,y, lc,b1,b2: double; elp,prj: byte; var b,l: double);
external dll_bl;
procedure BL_to_XY(b,l, lc,b1,b2: double; elp,prj: byte; var x,y: double);
external dll_bl;

procedure pc_33_bl_to_xy(B,L, L0: Double; out X,Y: Double);
external dll_bl;

procedure get_n0_n1(b0: Double; elp: Integer;
                    out n0,n1: Extended);
external dll_bl;

procedure bl_to_xyz(b,l,h: Double; elp: Integer;
                    out x,y,z: Double);
external dll_bl;

function xyz_to_bl(x,y,z: Double; elp: Integer;
                   out _b,_l,_h: Double): Boolean;
external dll_bl;

procedure BL_BL(b,l: double; elp,elp_: Integer;
                v,v_: PDatum; out b_,l_: double);
external dll_bl;

function Geoid_Dist(b1,l1, b2,l2: double; var fi: double): double;
external dll_bl;

function Geoid_Forw(b1,l1, r,fi: double; out b2,l2: double): double;
external dll_bl;

procedure GEO_GEO(b1,l1,h1: double; elp1,elp2: Integer;
                  geo1,geo2: PDatum7; out b2,l2,h2: double);
external dll_bl;

procedure GEO_WGS(b1,l1,h1: double; elp: Integer;
                  geo: PDatum7; out b2,l2,h2: double);
external dll_bl;

procedure WGS_GEO(b1,l1,h1: double; elp: Integer;
                  geo: PDatum7; out b2,l2,h2: double);
external dll_bl;

function vGEO_GEO(const v: txyz;
                  const s1,s2: tsys): txyz;
var
  t: txyz;
begin
  GEO_GEO(v.x,v.y,v.z, s1.elp,s2.elp,
          @s1.dat,@s2.dat, t.x,t.y,t.z);
  Result:=t
end;

function gGEO_GEO(const g: TGauss; const s1,s2: tsys): TGauss;
var
  t: TGauss; tz: double;
begin
  GEO_GEO(g.x,g.y,0, s1.elp,s2.elp,
          @s1.dat,@s2.dat, t.x,t.y,tz);
  Result:=t
end;

function vGEO_WGS(const v: txyz; const s1: tsys): txyz;
var
  t: txyz;
begin
  t:=v; if s1.elp <> 9 then
  GEO_WGS(t.x,t.y,t.z, s1.elp, @s1.dat, t.x,t.y,t.z);
  Result:=t
end;

function vWGS_GEO(const v: txyz; const s2: tsys): txyz;
var
  t: txyz;
begin
  t:=v; if s2.elp <> 9 then
  WGS_GEO(t.x,t.y,t.z, s2.elp, @s2.dat, t.x,t.y,t.z);
  Result:=t
end;

function get_bl_scale(b,l: Double): Double;
var
  x1,y1,x2,y2,st: Double;
begin
  Result:=1; st:=Radian(0,0,1);
  BL_to_XY(b,l,l,0,0,1,1,x1,y1);
  BL_to_XY(b+st,l+st,l,0,0,1,1,x2,y2);
  Result:=Hypot(x2-x1,y2-y1)/ Hypot(st,st)
end;

procedure sys_XY_BL(x,y: Double; const s: tsys; out b,l: double);
begin
  with s do begin
    if prj in prj_Radius then Set_Radius(Rn);
    if prj in prj_b3 then Set_B2(b3);
    XY_to_BL(x,y, lc,b1,b2, elp,prj, b,l)
  end
end;

procedure sys_BL_XY(b,l: Double; const s: tsys; out x,y: double);
begin
  with s do begin
    if prj in prj_Radius then Set_Radius(Rn);
    if prj in prj_b3 then Set_B2(b3);
    BL_to_XY(b,l, lc,b1,b2, elp,prj, x,y)
  end
end;

function StrPrjParam(Str: PChar; const s: tsys): PChar;
var
  mask: Integer; k: Double;
begin
  Result:=StrCopy(Str,'');

  if s.prj in [1..prj_max] then begin
    mask:=prj_mask[s.prj];

    k:=180/Pi;

    if mask and 1 <> 0 then
    pStrCat(Str,'lc='+xRealToStr(s.lc*k,3));

    if mask and 2 <> 0 then begin
      if Strlen(Str) > 0 then pStrCat(Str,', ');
      pStrCat(Str,'b1='+xRealToStr(s.b1*k,3));
    end;

    if mask and 4 <> 0 then begin
      if Strlen(Str) > 0 then pStrCat(Str,', ');
      pStrCat(Str,'b2='+xRealToStr(s.b2*k,3));
    end;

    if mask and 8 <> 0 then begin
      if Strlen(Str) > 0 then pStrCat(Str,', ');
      pStrCat(Str,'b3='+xRealToStr(s.b3*k,3));
    end;

    if (Abs(s.x0) >= 1)
    or (Abs(s.y0) >= 1) then begin
      StrCat(Str,' [');

      if Abs(s.x0) >= 1 then
      pStrCat(Str,'x0='+xRealToStr(s.x0,3));

      if Abs(s.y0) >= 1 then begin
        if Abs(s.x0) >= 1 then StrCat(Str,',');
        pStrCat(Str,'y0='+xRealToStr(s.y0,3));
      end;

      StrCat(Str,']');
    end
  end
end;

function PrjParamStr(const s: tsys): String;
var
  str: TShortstr;
begin
  Result:=Strpas(StrPrjParam(Str,s))
end;

function prj_Equal(const s1,s2: tsys): Boolean;
const
  small = 1E-6;
var
  elp1,elp2,mask: Integer; a,b: tsys;
begin
  Result:=false;

  a:=s1; elp1:=a.elp;
  b:=s2; elp2:=b.elp;

  if elp1 = 0 then elp1:=elp2;
  if elp2 = 0 then elp2:=elp1;

  if elp1 = elp2 then

  if (a.prj = 0) or (b.prj = 0) then

    Result:=true

  else
  if a.prj = b.prj then begin

    Result:=true; mask:=0;
    if a.prj in [0..prj_max] then
    mask:=prj_mask[a.prj];

    if mask and 1 <> 0 then
    if Abs(a.lc - b.lc) > small then
    Result:=false;

    if mask and 2 <> 0 then
    if Abs(a.b1 - b.b1) > small then
    Result:=false;

    if mask and 4 <> 0 then
    if Abs(a.b2 - b.b2) > small then
    Result:=false;

    if mask and 8 <> 0 then
    if Abs(a.b3 - b.b3) > small then
    Result:=false;

  end
end;

function sys_Equal(const s1,s2: tsys): Boolean;
begin
  Result:=false;
  if s1.pps = s2.pps then
  Result:=prj_Equal(s1,s2)
end;

function zLongitude(lc: double): double;
var
  g: Integer;
begin
  g:=Trunc(Abs(lc)*180/Pi);
  Result:=Radian(g-(g mod 6)+3,0,0);
  if lc < 0 then Result:=-Result
end;

function xZone(lc: double): double;
var
  a,m,z: int;
begin
  if lc < 0 then lc:=lc+pi2;
  R_to_G(lc,0, a,m); z:=succ(a div 6);
  Result:=z*1000000+500000
end;

function iZone(lc: double): int;
begin
  Result:=Trunc(xZone(lc)/1000000)
end;

function prj_zoneY(const s: tsys): Double;
var
  iz: int;
begin
  Result:=0;
  with s do if prj <= 2 then begin
    Result:=xZone(lc);
    if prj = 2 then begin
      Result:=Result + 30000000;
      iz:=Round(Result/1000000);
      if iz > 60 then
      Result:=Result - 60000000
    end
  end;
end;

procedure prj_XY_BL(x,y: Double; const s: tsys; out b,l: double);
begin
  if Abs(s.x0)+Abs(s.y0) > 0 then
    sys_XY_BL(x-s.x0,y-s.y0, s, b,l)
  else
    sys_XY_BL(x,y - prj_zoneY(s), s, b,l)
end;

procedure prj_BL_XY(b,l: Double; const s: tsys; out x,y: double);
begin
  sys_BL_XY(b,l, s, x,y);

  if Abs(s.x0)+Abs(s.y0) > 0 then begin
    x:=x+s.x0; y:=y+s.y0
  end
  else y:=y + prj_zoneY(s)
end;

procedure loc_XY_BL(x,y: Double; const s: tsys; out b,l: double);
begin
  sys_XY_BL(x - s.x0,y - s.y0, s, b,l)
end;

procedure loc_BL_XY(b,l: Double; const s: tsys; out x,y: double);
var
  gx,gy: Double;
begin
  sys_BL_XY(b,l, s, gx,gy);
  x:=gx + s.x0; y:=gy + s.y0
end;

procedure prj_shift(xy: PGPoly; n: int; const s1,s2: tsys);
var
  i: int; dx,dy: double;
begin
  if (s1.x0 <> s2.x0)
  or (s1.y0 <> s2.y0) then begin

    dx:=0; dy:=0;
    if Abs(s1.x0)+Abs(s1.y0) > 0 then begin
      dx:=-s1.x0; dy:=-s1.y0
    end
    else dy:=-prj_zoneY(s1);

    if Abs(s2.x0)+Abs(s2.y0) > 0 then begin
      dx:=dx+s2.x0; dy:=dy+s2.y0
    end
    else dy:=dy+prj_zoneY(s2);

    for i:=0 to n-1 do
    with xy[i] do begin
      x:=x+dx; y:=y+dy
    end
  end
end;

function cuba_sys(var sys: tsys): Boolean;
const
  small = 1E-6;
begin
  Result:=false;

  if sys.elp = 6 then
  if Eps_Equal(sys.y0,500000,0.001) then

// CoordSys Earth Projection 3, 70, "m",
// -81, 22.35, 21.7, 23, 500000, 280296.016

  if  Eps_Equal(sys.lc,-81,small)
  and Eps_Equal(sys.b1,22.35,small)
  and Eps_Equal(sys.b2,21.7,small)
  and Eps_Equal(sys.b3,23,small)
  and Eps_Equal(sys.x0,280296.016,small) then begin
    sys:=sys7(1,prj_cuba,6,0,0,0); Result:=true
  end else

// CoordSys Earth Projection 3, 70, "m",
// -76.83333, 20.71667, 20.13333, 21.3, 500000, 229126.93900000001

  if  Eps_Equal(sys.lc,-76.83333,small)
  and Eps_Equal(sys.b1,20.71667,small)
  and Eps_Equal(sys.b2,20.13333,small)
  and Eps_Equal(sys.b3,21.3,small)
  and Eps_Equal(sys.x0,229126.939,small) then begin
    sys:=sys7(1,prj_cuba+1,6,0,0,0); Result:=true
  end
end;

function geo_to_sys(g: PGeoPoint; const s: tsys; out o: TGauss): Boolean;
var
  x,y,h: Double; s1,s2: tsys;
begin
  Result:=false; x:=g.x; y:=g.y;

  s1:=g.s; with s1 do
  if (prj = 0) and (pps = 1) then prj:=1;

  s2:=s; with s2 do
  if (prj = 0) and (pps = 1) then prj:=1;

  if s1.prj = 0 then
    Result:=s2.prj = 0
  else
  if s2.prj > 0 then begin

    if not elp_Equal(s1.elp,s2.elp) then begin

      if s1.pps = 0 then begin
        prj_XY_BL(x,y,s1,x,y); s1.pps:=1
      end;

      GEO_GEO(x,y,0, s1.elp,s2.elp,
              @s1.dat,@s2.dat, x,y,h);
    end;

    if s2.pps = 0 then begin

      if s1.pps = 1 then
        prj_BL_XY(x,y,s1,x,y)
      else
      if not prj_Equal(s1,s2) then begin
        prj_XY_BL(x,y,s1,x,y);
        prj_BL_XY(x,y,s2,x,y);
      end

    end else
    if s1.pps = 0 then
      prj_XY_BL(x,y,s1,x,y);

    Result:=true
  end;

  o.x:=x; o.y:=y
end;

function gp_to_prj(const g: TGauss; const gsys,psys: tsys): TGauss;
var
  v: txyz; s1,s2: tsys;
begin
  v.x:=g.x; v.y:=g.y; v.z:=0;

  s1:=gsys;

  s2:=psys; with s2 do
  if (prj=0) and (pps=1) then prj:=1;

  if s2.prj = 0 then begin
    if s1.pps = 1 then
    prj_BL_XY(v.x,v.y, s1, v.x,v.y)
  end else
  if s1.pps = 1 then begin
    if s1.elp <> s2.elp then
    v:=vGEO_GEO(v, s1,s2);

    prj_BL_XY(v.x,v.y, s2, v.x,v.y)
  end else

  if prj_Equal(s1,s2) then begin
    if (s1.x0 <> s2.x0)
    or (s1.y0 <> s2.y0) then
    prj_shift(@v,1,s1,s2)
  end
  else begin
    prj_XY_BL(v.x,v.y, s1, v.x,v.y);

    if s1.elp <> s2.elp then
    v:=vGEO_GEO(v, s1,s2);

    prj_BL_XY(v.x,v.y, s2, v.x,v.y)
  end;

  Result.x:=v.x;
  Result.y:=v.y
end;

function geo_to_prj(const g: TGeoPoint; const sys: tsys): TGauss;
var
  t: TGauss;
begin
  t.x:=g.x; t.y:=g.y;
  Result:=gp_to_prj(t,g.s,sys)
end;

function geo_to_xy(const g: TGeoPoint; const sys: tsys): TGauss;
var
  t: TGauss;
begin
  t:=geo_to_prj(g,sys);
  if sys.pps = 1 then
  t.y:=t.y - prj_ZoneY(sys);
  Result:=t
end;

function Get_wgs_sys(const lt,rb: TGauss): tsys;
var
  s: tsys; dy,lc: Double;
begin
  s:=sys7(1,3,9,0,0,0);
  s.b1:=(lt.x/2 + rb.x/2)*180/Pi;
  s.b1:=Round(s.b1)/180*Pi;

  dy:=3/180*Pi;
  lc:=zLongitude((lt.y+rb.y)/2);
  if lt.y >= lc-dy then
  if rb.y <= lc+dy then begin
    s.prj:=2; s.lc:=lc; s.b1:=0;
  end;

  Result:=s
end;

function x_prj_to_prj(x,y: Double; const s1,s2: tsys): TGauss;
var
  v: txyz; t: TGauss;
begin
  prj_XY_BL(x,y, s1, v.x,v.y); v.z:=0;
  if s1.elp <> s2.elp then v:=vGEO_GEO(v,s1,s2);
  prj_BL_XY(v.x,v.y, s2, t.x,t.y);
  Result:=t
end;

function Get_sys_mpp(mpp: double;
                     const g: TGeoPoint;
                     const dest: tsys): double;
var
  g1,g2: TGauss; v1,v2: txyz; s1,s2: tsys; gz: double;
begin
  Result:=mpp;

  s1:=g.s; s2:=dest;
  if s1.prj > 0 then
  if s2.prj > 0 then begin

    if s1.pps = 0 then begin
      g1.x:=g.x; g1.y:=g.y;
      prj_XY_BL(g1.x,g1.y,s1,v1.x,v1.y)
    end
    else begin
      v1.x:=g.x; v1.y:=g.y;
      prj_BL_XY(v1.x,v1.y,s1,g1.x,g1.y)
    end;

    g2.x:=g1.x; g2.y:=g1.y+mpp;
    prj_XY_BL(g2.x,g2.y,s1,v2.x,v2.y);

    if s1.elp <> s2.elp then begin
      v1.z:=0; v1:=vGEO_GEO(v1,s1,s2);
      v2.z:=0; v2:=vGEO_GEO(v2,s1,s2);
    end;

    sys_BL_XY(v1.x,v1.y, s2, g1.x,g1.y);
    sys_BL_XY(v2.x,v2.y, s2, g2.x,g2.y);

    Result:=Hypot(g2.x-g1.x,g2.y-g1.y)
  end
end;

function sys_yandex_mpp(mpp: double;
                        const g: TGeoPoint): double;
var
  s: tsys;
begin
  s:=sys7(1,3,9, 0,0,0);
  Result:=Get_sys_mpp(mpp,g,s)
end;

function yandex_sys_mpp(mpp: double;
                        const g: TGeoPoint): double;
var
  t: TGeoPoint; tz: double;
begin
  Result:=mpp; t:=g;

  if t.s.prj <> 0 then begin

    if t.s.pps = 0 then begin
      prj_XY_BL(t.x,t.y,t.s,t.x,t.y);
      t.s.pps:=1
    end;

    with t do if s.elp <> 9 then
    GEO_WGS(x,y,0, s.elp,@s.dat, x,y,tz);

    t.s:=sys7(1,3,9, 0,0,0);
    Result:=Get_sys_mpp(mpp,t,g.s)
  end
end;

function Get_yandex_dist(const g1,g2: TGauss; const sys: tsys): double;
var
  v1,v2: txyz;
begin
  v1:=_xyz(g1.x,g1.y,0);
  v2:=_xyz(g2.x,g2.y,0);

  if sys.elp <> 9 then begin
    v1:=vGEO_WGS(v1,sys);
    v2:=vGEO_WGS(v2,sys);
  end;

  with v1 do BL_to_XY(x,y, 0,0,0, 9,3, x,y);
  with v2 do BL_to_XY(x,y, 0,0,0, 9,3, x,y);

  Result:=Hypot(v2.x-v1.x,v2.y-v1.y)
end;

function Get_yandex_mpp(lev: int): double;
var
  iw: int; x1,y1,x2,y2: double;
begin
  BL_to_XY(0,-Pi, 0,0,0, 9,3, x1,y1);
  BL_to_XY(0,+Pi, 0,0,0, 9,3, x2,y2);

  iw:=256;
  while lev > 0 do begin
    iw:=iw*2; Dec(lev)
  end;

  Result:=(y2-y1)/iw
end;

function Get_yandex_lev(mpp: double): int;
var
  iw,lev: int; x1,y1,x2,y2,k,r,t: double;
begin
  Result:=-1; r:=0;

  BL_to_XY(0,-Pi, 0,0,0, 9,3, x1,y1);
  BL_to_XY(0,+Pi, 0,0,0, 9,3, x2,y2);

  k:=(y2-y1)/mpp; iw:=256;

  for lev:=0 to 22 do begin

    t:=Abs(1 - k/iw);
    if (Result < 0) or (t < r) then begin
      Result:=lev; r:=t
    end;

    iw:=iw*2;
  end;
end;

function Get_yandex_poi(b,l: double;
                        const sys: tsys;
                        lev: int): double;
var
  mpp, x,y, b2,l2: double; v1,v2: txyz;
begin
  mpp:=Get_yandex_mpp(lev);

  BL_to_XY(b,l, 0,0,0, 9,3, x,y);
  XY_to_BL(x,y+mpp, 0,0,0, 9,3, b2,l2);

  v1:=_xyz(b,l,0);
  v2:=_xyz(b2,l2,0);

  if sys.elp <> 9 then begin
    v1:=vWGS_GEO(v1,sys);
    v2:=vWGS_GEO(v2,sys);
  end;

  loc_BL_XY(v1.x,v1.y, sys, v1.x,v1.y);
  loc_BL_XY(v2.x,v2.y, sys, v2.x,v2.y);

  Result:=Hypot(v2.x-v1.x,v2.y-v1.y)
end;

function rmu_merc(bc: double): double;
var
  b1,b2,l1,l2,b3,x1,y1,x2,y2: double;
begin
  b1:=bc; b2:=bc+1/180*Pi;
  l1:=0;

  BL_to_XY(b1,l1, 0,bc,0, 9,3, x1,y1);
  BL_to_XY(b2,l1, 0,bc,0, 9,3, x2,y2);

  y2:=x2-x1;
  XY_to_BL(x1,y2, 0,bc,0, 9,3, b3,l2);

  Result:=(l2-l1)/(b2-b1)
end;

procedure get_merc_rect(b,l,rad: double;
                        out lt,rb: TGauss);
var
  s: tsys; x,y: double;
begin
  s:=sys7(1,3,9,b,0,0);

  sys_BL_XY(b,l,s,x,y);
  sys_XY_BL(x-rad,y-rad,s,lt.x,lt.y);
  sys_XY_BL(x+rad,y+rad,s,rb.x,rb.y);
end;

begin
  pi2:=Pi*2
end.