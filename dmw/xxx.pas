unit xxx; interface

uses
  Math,otypes,xyz;

type
  PReper = ^TReper;
  TReper = record o,a,u,v: txyz end;

  PRepers = ^TRepers;
  TRepers = array[0..7] of TReper;

//  CAMERAS:  номер снимка,
//            фокусное, центр_х, центр_у,
//            o->x, o->y, o->z,
//            a->x, a->y, a->z,
//            u->x, u->y, u->z,
//            v->x, v->y, v->z
//
//  TRS:      центр_лев_х, центр_лев_у, центр_прав_х, центр_прав_у,
//            фокусное,
//            r[0].o->x, r[0].o->y, r[0].o->z,
//            r[0].a->x, r[0].a->y, r[0].a->z,
//            r[0].u->x, r[0].u->y, r[0].u->z,
//            r[0].v->x, r[0].v->y, r[0].v->z,
//            r[1].o->x, r[1].o->y, r[1].o->z,
//            r[1].a->x, r[1].a->y, r[1].a->z,
//            r[1].u->x, r[1].u->y, r[1].u->z,
//            r[1].v->x, r[1].v->y, r[1].v->z,
//            r[2].o->x, r[2].o->y, r[2].o->z,
//            r[2].a->x, r[2].a->y, r[2].a->z,
//            r[2].u->x, r[2].u->y, r[2].u->z,
//            r[2].v->x, r[2].v->y, r[2].v->z,

var
  xxx_err: Integer;

function Reper_identify: TReper;

function this_reper(const R: TReper): Boolean;

function Reper_space(const R: TReper): TSpace;
function Space_reper(const S: TSpace): TReper;
function Matrix_reper(const T: TMatrix): TReper;

procedure Reper_alfa_beta_psi(const R: TReper;
                              out alfa,beta,psi: Double);

function vc(r1,r2: PRepers;
            focus_sp: double;
            const p_in: txyz;
            out p_out: txyz): Boolean;

function inverse_vc(r1,r2: PRepers;
                    focus_sp: Double;
                    const p_in: txyz;
                    out p_out: txyz): Boolean;

function xyz_ll(const c1,v1,c2,v2: txyz; out p: txyz): Double;

implementation

function Reper_identify: TReper;
begin
  Result.o:=xyz_nil;
  Result.u:=_xyz(1,0,0);
  Result.v:=_xyz(0,1,0);
  Result.a:=_xyz(0,0,1);
end;

function this_reper(const R: TReper): Boolean;
var
  s: tspace;
begin
  s:=Reper_space(R);
  Result:=this_space(s)
end;

function Reper_space(const R: TReper): TSpace;
begin
  Result[1]:=R.u;
  Result[2]:=R.v;
  Result[3]:=R.a
end;

function Space_reper(const S: TSpace): TReper;
begin
  Result.o:=xyz_nil;
  Result.u:=S[1];
  Result.v:=S[2];
  Result.a:=S[3];
end;

function Matrix_reper(const T: TMatrix): TReper;
begin
  Result:=Space_reper( Space_Matrix(T) )
end;

procedure Reper_alfa_beta_psi(const R: TReper;
                              out alfa,beta,psi: Double);
begin
  alfa:=Arctan2(-R.u.z,R.a.z);
  psi:=Arcsin(-R.v.z);
  beta:=Arctan2(R.v.x,R.v.y);
end;

function tu_coord_in_reper(const r: treper;
                           const v: txyz): txyz;
begin
  Result.x:=v_scalar(r.u,v);
  Result.y:=v_scalar(r.v,v);
  Result.z:=v_scalar(r.a,v);
end;

function tu_vector_by_coord(const r: treper;
                            const coord: txyz): txyz;
var
  a,b,c: txyz;
begin
  a:=xyz_scale(r.u,coord.x);
  b:=xyz_scale(r.v,coord.y);
  c:=xyz_scale(r.a,coord.z);
  Result:=xyz_add(xyz_add(a,b),c)
end;

function dist_3d_lines(const p1,v1, p2,v2: txyz): txyz;
var
  a,b,c,d, b1,b2, dd,dd1,dd2: Double;
  p,v: txyz;
begin
  a:= v_scalar(v1,v1);
  b:=-v_scalar(v1,v2);
  d:= v_scalar(v2,v2);
  c:=b;

  p:=xyz_sub(p1,p2);
  b1:=-v_scalar(v1,p);
  b2:= v_scalar(v2,p);

  dd:=a * d - b * c;
  dd1:=b1 * d - b2 * b;
  dd2:=a * b2 - c * b1;

  if Abs(dd) < 1.0e-10 then
    Result:=p1
  else begin
    v:=xyz_scale(v1,dd1/dd);
    Result:=xyz_add(p1,v)
  end
end;

//  r1[0],  r1[1] : "CAMERAS"
//  r2[0], r2[1], r2[2], focus_sp : "TRS"
//  p_in :  (x_pixel_sp, y_pixel_sp, disp_sp) относительно центра (TRS)
//  p_out : (x_space, y_spase, z_space)  внутренняя система
function vc(r1,r2: PRepers;
            focus_sp: double;
            const p_in: txyz;
            out p_out: txyz): Boolean;

function eye_vector(const v: txyz;
                    const A,B,C: TReper): txyz;
var
  v1,v2: txyz;
begin
  v1:=tu_vector_by_coord(a,v);
  v2:=tu_coord_in_reper(b,v1);
  Result:=tu_vector_by_coord(c,v2)
end;

var
  v, lv,rv: txyz;
begin
  Result:=true; p_out:=xyz_nil;

  v:=p_in; v.z:=focus_sp;
  lv:=eye_vector(v, r2[2],r2[0],r1[0]);

  v.x:=p_in.x + p_in.z;
  rv:=eye_vector(v, r2[2],r2[1],r1[1]);

  p_out:=dist_3d_lines(r1[0].o,lv, r1[1].o,rv)
end;

//  r1[0],  r1[1] : "CAMERAS"
//  r2[0],  r2[1],  r2[2], focus_sp : "TRS"
//  p_in : (x_space, y_spase, z_space) внутренняя система
//  p_out: (x_pixel_sp, y_pixel_sp, disp_sp) относительно центра (TRS)

function inverse_vc(r1,r2: PRepers;
                    focus_sp: Double;
                    const p_in: txyz;
                    out p_out: txyz): Boolean;
var
  a,a1,a2,a3: txyz;
  p,p1,p2, v,v1,v2,v12: txyz;
  aa,bb,cc,dd,k: Double;
begin
  Result:=false;
  p_out:=xyz_nil;
  xxx_err:=0;

  p:=p_in;
  v:=xyz_sub(p,r1[0].o);
  v1:=tu_coord_in_reper(r1[0],v);
  v2:=tu_vector_by_coord(r2[0],v1);
  v:=tu_coord_in_reper(r2[2],v2);

  if Abs(v.z) < 1.0e-20 then begin
    xxx_err:=1; Exit
  end;

  k:=focus_sp / v.z;
  p_out.x:=v.x * k;
  p_out.y:=v.y * k;

  a1.x:=0;
  a1.y:=focus_sp;
  a1.z:=-p_out.y;

  a2:=tu_vector_by_coord(r2[2], a1);
  a3:=tu_coord_in_reper(r2[1], a2);
  a:=tu_vector_by_coord(r1[1], a3);

  aa:=v_scalar(a,a);
  if aa < 1.0e-20 then begin
    xxx_err:=6; Exit
  end;

  v:=xyz_sub(p,r1[1].o);
  k:=-v_scalar(v,a) / aa;

  v:=xyz_scale(a,k);
  p2:=xyz_add(p,v);

  v:=xyz_sub(r1[0].o,r1[1].o);
  k:=-v_scalar(v,a) / aa;

  v:=xyz_scale(a,k);
  p1:=xyz_add(r1[0].o,v);

  v:=xyz_sub(r1[0].o,p);
  v1:=xyz_sub(p2,p1);
  v2:=xyz_sub(p,p1);

  k:=v_scalar(v1,v);
  if Abs(k) < 1.0e-20 then begin
    xxx_err:=7; Exit
  end;

  k:=v_scalar(v2,v) / k;
  v1:=xyz_scale(v1,k);
  p1:=xyz_add(p1,v1);

  v2:=xyz_vec(a,v); v:=v2;

  v2:=xyz_sub(r1[1].o,p1);
  v1:=xyz_sub(p,p1);
  v12:=xyz_add(v1,v2);
  v12:=xyz_scale(v12,0.5);

  aa:=v_scalar(v,v);
  if aa < 1.0e-20 then begin
    xxx_err:=8; Exit
  end;

  bb:=v_scalar(v, v12);
  cc:=v_scalar(v1, v2);

  dd:= bb * bb - aa * cc;
  if dd < 1.0e-200 then begin
    xxx_err:=5; Exit
  end;

  dd:=sqrt(dd);

  k:=(bb + dd) / aa;
  v1:=xyz_scale(v,k);
  v1:=xyz_add(p1,v1);

  k:=(bb - dd) / aa;
  v2:=xyz_scale(v,k);
  v2:=xyz_add(p1,v2);

  aa:=xyz_dist2(v1,p);
  bb:=xyz_dist2(v2,p);

  if aa < bb then p1:=v1
  else p1:=v2;

  v:=xyz_sub(p1,r1[1].o);
  v1:=tu_coord_in_reper(r1[1], v);
  v2:=tu_vector_by_coord(r2[1], v1);
  v:=tu_coord_in_reper(r2[2], v2);

  if Abs(v.z) < 1.0e-20 then begin
    xxx_err:=3; Exit
  end;

  p_out.z:=v.x * focus_sp/v.z - p_out.x;
  Result:=true
end;

function solve_linear2(A1,B1,C1, A2,B2,C2: Double;
                       out t,s: Double): Boolean;
var
  D: Double;
begin
  Result:=false; t:=0; s:=0;

  D:=A1*B2 - B1*A2;
  if Abs(D) > Small then begin
    t:=(C1*B2 - B1*C2) / D;
    s:=(A1*C2 - C1*A2) / D;
    Result:=true
  end;
end;

function xyz_ll(const c1,v1,c2,v2: txyz; out p: txyz): Double;
(*
  x1 + u1*t = x2 + u2*s
  y1 + v1*t = y2 + v2*s  => t,s?
  z1 + w1*t = z2 + w2*s

  F=Distance( P(t),P(s) );

  F't = 0; A1*t + B1*s = C1;
  F's = 0; A2*t + B2*s = C2;

  t = Dt / D; s = Ds / D;
*)

var
  r1,r2,sp, t,s: Double; b,p1,p2: txyz;
begin
  Result:=-1; p:=xyz_nil;

  b:=xyz_sub(c2,c1);

  with v1 do r1:=x*x + y*y + z*z;
  with v2 do r2:=x*x + y*y + z*z;

  sp:=v_scalar(v1,v2);

  if solve_linear2(-r1, sp,-v_scalar(v1,b),
                   -sp, r2,-v_scalar(v2,b),
                   t,s) then begin

    p1.x:=c1.x + t*v1.x;
    p1.y:=c1.y + t*v1.y;
    p1.z:=c1.z + t*v1.z;

    p2.x:=c2.x + s*v2.x;
    p2.y:=c2.y + s*v2.y;
    p2.z:=c2.z + s*v2.z;

    p.x:=p1.x/2 + p2.x/2;
    p.y:=p1.y/2 + p2.y/2;
    p.z:=p1.z/2 + p2.z/2;

    Result:=xyz_dist(p1,p2)
  end
end;

begin
  xxx_err:=0
end.

