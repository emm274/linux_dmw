unit xyz; interface

uses
  Math,otypes,xline;

type
  TSpace = array[1..3] of txyz;

  VSpace = record case integer of
0: (s: TSpace);
1: (a1,a2,a3, b1,b2,b3, c1,c2,c3: Double);
2: (r1,r2,r3, r4,r5,r6, r7,r8,r9: Double);
  end;

  XSpace = record
    mode: Integer; o: txyz; e: TSpace
  end;

  pcube = ^tcube;
  tcube = record case Integer of
  0: (xmin,ymin,zmin, xmax,ymax,zmax: Double);
  1: (vmin,vmax: txyz)
  end;

  TXyzBox = record c,s: txyz end;

  txyz_volume = Array[0..7] of txyz;
  txyz_quads = Array[0..3] of txyz;

  PMatrix = ^TMatrix;
  TMatrixf = array[0..3,0..3] of float;
  TMatrix = array[0..3,0..3] of Double;
  EMatrix = array[0..3,0..3] of Extended;

  PRotate_awk = ^TRotate_awk;
  TRotate_awk = record
    a,w,k: Double; s: VSpace;
    sin_a,cos_a: Extended;
    sin_w,cos_w: Extended;
    sin_k,cos_k: Extended;
  end;

  PRotate_awk_array = ^TRotate_awk_array;                 
  TRotate_awk_array = Array[0..15] of TRotate_awk;

  tline3 = record c,n: txyz end;
  tplane3 = record n: txyz; d: Double end;

  TFrustumPlane3 = record
    pl: tplane3; mfa: txyz;
  end;

  TFrustumPyramid = Array[0..5] of TFrustumPlane3;

function xyz_nil: txyz;
function xyz_rad(const r: txyz): txyz;
function xyz_is_nil(const v: txyz): Boolean;

function Matrix_identify: TMatrix;

function Matrix_assign(r00,r01,r02,r03: Double;
                       r10,r11,r12,r13: Double;
                       r20,r21,r22,r23: Double;
                       r30,r31,r32,r33: Double): TMatrix;

function Real3x3_Matrix(const T: Real3x3): TMatrix;
function Matrix_Real3x3(const T: TMatrix): Real3x3;

function inverse_matrix(const Tr: TMatrix; out Tr_: TMatrix): Boolean;

function Matrix_norm(k, x0,y0,z0: Double): TMatrix;
function Matrix_denorm(const N: TMatrix): TMatrix;

function Matrix_reper(const T: TMatrix): TSpace;

function Matrix_space(const S: TSpace): TMatrix;
function Space_Matrix(const T: TMatrix): TSpace;

function Matrix_Scale(k: Double): TMatrix;
function Matrix_XScale(kx,ky,kz: Double): TMatrix;
function Matrix_Translate(x,y,z: Double): TMatrix;
function Matrix_RotateX(fi: Double): TMatrix;
function Matrix_RotateY(fi: Double): TMatrix;
function Matrix_RotateZ(fi: Double): TMatrix;

function Matrix_rotx(c,s: Double): TMatrix;
function Matrix_roty(c,s: Double): TMatrix;
function Matrix_rotz(c,s: Double): TMatrix;

function Matrix_rot(fi, vx,vy,vz: Double): TMatrix;

function Matrix_Normal(fx,fy: Double): TMatrix;

function Matrix_head(x,z: Double): TMatrix;
function Matrix_pitch(x,y: Double): TMatrix;
function Matrix_LookAt(x,y,z: Double): TMatrix;

function Matrix_Orient(alfa,beta,psi: Double): TMatrix;
function Matrix_Reverse(alfa,beta,psi: Double): TMatrix;
function Rotate_Reverse(const R: TRotate_awk): TMatrix;

function rot_xyz_to_mat(const R: txyz): TMatrix;
function rot_yxz_to_mat(const R: txyz): TMatrix;

function mat_to_rot_xyz(const R: TMatrix): txyz;
function mat_to_rot_yxz(const R: TMatrix): txyz;

function Matrix_to_Orient(const R: TMatrix): txyz;
function Matrix_to_Rotate(const R: TMatrix): txyz;

function Matrix_x_s(const A: TMatrix; s: Double): TMatrix;
function Matrix_add(const A,B: TMatrix): TMatrix;
function Matrix_mult(const A,B: TMatrix): TMatrix;
procedure Matrix_forw(var A: TMatrix; const B: TMatrix);
procedure Matrix_plan(var A: TMatrix; const B: Real3x3);

procedure Scale_Matrix(var T: TMatrix; k: Double);
procedure XScale_Matrix(var T: TMatrix; kx,ky,kz: Double);

procedure RotateX_Matrix(var T: TMatrix; fi: Double);
procedure RotateY_Matrix(var T: TMatrix; fi: Double);
procedure RotateZ_Matrix(var T: TMatrix; fi: Double);

procedure rotx_Matrix(var T: TMatrix; c,s: Double);
procedure roty_Matrix(var T: TMatrix; c,s: Double);
procedure rotz_Matrix(var T: TMatrix; c,s: Double);

procedure xy_Rotate_Matrix(var T: TMatrix; x,y: Double);
procedure xz_Rotate_Matrix(var T: TMatrix; x,z: Double);
procedure Translate_Matrix(var T: TMatrix; x,y,z: Double);

procedure Orient_Matrix(var T: TMatrix; alfa,beta,psi: Double);

function Line_Matrix(out Tr: TMatrix;
                     const a,b: txyz): Boolean;

function iLine_Matrix(out Tr: TMatrix;
                      const a,b: txyz): Boolean;

function lRotate_Matrix(out Tr: TMatrix;
                        const a,b,c: txyz): Boolean;

function swap_matrix(const T: TMatrix): TMatrix;

procedure Matrix_x_vector(const T: TMatrix; U,V: PDoubles);

function xyz_transform(x,y,z: Double; const T: TMatrix): txyz;
function xyz_matrix(const v: txyz; const T: TMatrix): txyz;

procedure xyzv_matrix(V: pxyz_array; Count: Integer;
                      const T: TMatrix);

procedure xyzv_min_max(V: pxyz_array; n: Integer;
                       out vmin,vmax: txyz);

function xyzv_centre(vp: pxyz_array; vn: int): txyz;

function Bound_to_Volume(const lt,rb: txyz; V: pxyz_Array): Integer;

function cube_identify: tcube;

procedure max_cube(cube: pcube; v: pxyz);
procedure add_cube(cube1,cube2: pcube);

function max_dist_to_cube(const v: txyz; const c: tcube): Double;

function eps_dv(const v,e: txyz): Boolean;

function xyz_equal(const a,b: txyz; eps: Double): Boolean;

procedure xyz_inc(var a: txyz; const b: txyz);
procedure xyz_dec(var a: txyz; const b: txyz);
procedure xyz_max(var a: txyz; const b: txyz);
procedure xyz_sqr(var a: txyz; const b: txyz);
procedure xyz_mult(var v: txyz; k: Double);
procedure xyz_sqrt(var v: txyz; k: Double);

function xyz_scale(const v: txyz; k: Double): txyz;
function xyz_scale1(const v,k: txyz): txyz;
function xyz_add(const a,b: txyz): txyz;
function xyz_dir(const a,b: txyz): txyz;
function xyz_sub(const a,b: txyz): txyz;
function xyz_vec(const a,b: txyz): txyz;

function xyz_centre(const a,b: txyz): txyz;

function v_scalar(const a,b: txyz): Double;
function v_scalar_0(const a,b: txyz): Boolean;
function v_scalar_1(const a,b: txyz): Boolean;

function xyz_dist(const a,b: txyz): Double;
function xyz_dist2(const a,b: txyz): Double;

function xyz_length(const v: txyz): Double;
function xyz_norm(var v: txyz): Boolean;

function xyz_step(const a,b: txyz; r: Double): txyz;
function xyz_project(const a,b: txyz; r: Double): txyz;

function xyz_alfa(const a,b: txyz): Double;
function xyz_beta(const a,b: txyz): Double;

function xyz_ray(r, alfa,beta: Double): txyz;
function xyz_up(const a,b: txyz): txyz;

function xyz_h_z(const v,h: txyz): Double;

function lines_angle(const a,b: txyz): Double;

function this_space(const s: TSpace): Boolean;
function norm_space(var s: TSpace): Double;

function space_x_space(const a,b: TSpace): TSpace;
function space_vw_space(const v,w: TSpace): TSpace;

function Orient_to_Space(alfa,beta,psi: Double): TSpace;
function Space_to_Orient(const S: TSpace): txyz;

procedure Space_scale(var S: TSpace; k: Double);

function x_to_x(const v: txyz; const S: TSpace): txyz;
function x_to_p(x,y,f: Double; const R: TSpace): TGauss;

procedure inside_to_center(const ins: txyz;
                        const sys: XSpace;
                        out cen: txyz);

procedure center_to_inside(const cen: txyz;
                           const sys: XSpace;
                           out ins: txyz);

function Rotate_identify: TRotate_awk;
function Rotate_rewind(a,w,k: Double): TRotate_awk;

procedure Rotate_init(out R: TRotate_awk; a,w,k: Double);
procedure Rotate_forw(var R: TRotate_awk; da,dw,dk: Double);
procedure Rotate_next(var R: TRotate_awk; da,dw,dk: Double);
procedure Rotate_back(var R: TRotate_awk; da,dw,dk: Double);

function diff_awk(const p,v: txyz; const R: TRotate_awk): TSpace;

function plane3(const p,n: txyz): tplane3;

function FrustumPlane3(const p,n: txyz): TFrustumPlane3;

function Init_line3(out ln: tline3; const a,b: txyz): Boolean;
function Init_plane3(out pl: tplane3; const a,b: txyz): Boolean;
function Triangle_plane3(out pl: tplane3; tp: pxyz_array): Boolean;

function solve_plane(out pl: tplane3; vp: pxyz_array; vn: int): Boolean;

function dist_to_line3(const ln: tline3; const p: txyz): Double;
function d3_to_l3(const p,a,b: txyz): Double;

function ilp3(const ln: tline3;
              const pl: tplane3;
              out v: txyz): Boolean;

function ivpt(const a,b, c,n: txyz;
              out v: txyz): Boolean;

function izp3(const pl: tplane3;
              nz: double; out vz: double): Boolean;

function boxFrustumTest(const box: TXyzBox;
                        const pl: TFrustumPlane3): Boolean;

function plane_Matrix(const pl: tplane3; vp: pxyz_array): TMatrix;
function triangle_Matrix(out Tr: TMatrix; vp: pxyz_array): Boolean;

implementation

function xyz_nil: txyz;
begin
  Result.x:=0; Result.y:=0; Result.z:=0;
end;

function xyz_rad(const r: txyz): txyz;
begin
  Result.x:=r.x/180*Pi;
  Result.y:=r.y/180*Pi;
  Result.z:=r.z/180*Pi;
end;

function xyz_is_nil(const v: txyz): Boolean;
begin
  Result:=(v.x = 0) and (v.y = 0) and (v.z = 0)
end;

function Matrix_identify: TMatrix;
var
  i: Integer;
begin
  Fillchar(Result,Sizeof(Result),0);
  for i:=0 to 3 do Result[i,i]:=1;
end;

function Matrix_assign(r00,r01,r02,r03: Double;
                       r10,r11,r12,r13: Double;
                       r20,r21,r22,r23: Double;
                       r30,r31,r32,r33: Double): TMatrix;
var
  T: TMatrix;
begin
  T[0,0]:=r00; T[0,1]:=r01; T[0,2]:=r02; T[0,3]:=r03;
  T[1,0]:=r10; T[1,1]:=r11; T[1,2]:=r12; T[1,3]:=r13;
  T[2,0]:=r20; T[2,1]:=r21; T[2,2]:=r22; T[2,3]:=r23;
  T[3,0]:=r30; T[3,1]:=r31; T[3,2]:=r32; T[3,3]:=r33;
  Result:=T
end;

function Real3x3_Matrix(const T: Real3x3): TMatrix;
var
  M: TMatrix;
begin
  M:=Matrix_identify;
  M[0,0]:=T[1,1]; M[0,1]:=T[1,2]; M[0,3]:=T[1,3];
  M[1,0]:=T[2,1]; M[1,1]:=T[2,2]; M[1,3]:=T[2,3];
  Result:=M
end;

function Matrix_Real3x3(const T: TMatrix): Real3x3;
var
  R: Real3x3;
begin
  R:=Identify_3x3;
  R[1,1]:=T[0,0]; R[1,2]:=T[0,1]; R[1,3]:=T[0,3];
  R[2,1]:=T[1,0]; R[2,2]:=T[1,1]; R[2,3]:=T[1,3];
  Result:=R
end;

function inverse_matrix(const Tr: TMatrix; out Tr_: TMatrix): Boolean;
const
  Small = 1E-8;
var
  i,j: Integer;
  T: EMatrix; det: Extended;
begin
  Result:=true; Tr_:=Matrix_identify;

  for i:=0 to 3 do
  for j:=0 to 3 do T[i,j]:=Tr[i,j];

  det:=T[0,3]*T[1,2]*T[2,1]*T[3,0] -
       T[0,2]*T[1,3]*T[2,1]*T[3,0] - T[0,3]*T[1,1]*T[2,2]*T[3,0] +
       T[0,1]*T[1,3]*T[2,2]*T[3,0] + T[0,2]*T[1,1]*T[2,3]*T[3,0] -
       T[0,1]*T[1,2]*T[2,3]*T[3,0] - T[0,3]*T[1,2]*T[2,0]*T[3,1] +
       T[0,2]*T[1,3]*T[2,0]*T[3,1] + T[0,3]*T[1,0]*T[2,2]*T[3,1] -
       T[0,0]*T[1,3]*T[2,2]*T[3,1] - T[0,2]*T[1,0]*T[2,3]*T[3,1] +
       T[0,0]*T[1,2]*T[2,3]*T[3,1] + T[0,3]*T[1,1]*T[2,0]*T[3,2] -
       T[0,1]*T[1,3]*T[2,0]*T[3,2] - T[0,3]*T[1,0]*T[2,1]*T[3,2] +
       T[0,0]*T[1,3]*T[2,1]*T[3,2] + T[0,1]*T[1,0]*T[2,3]*T[3,2] -
       T[0,0]*T[1,1]*T[2,3]*T[3,2] - T[0,2]*T[1,1]*T[2,0]*T[3,3] +
       T[0,1]*T[1,2]*T[2,0]*T[3,3] + T[0,2]*T[1,0]*T[2,1]*T[3,3] -
       T[0,0]*T[1,2]*T[2,1]*T[3,3] - T[0,1]*T[1,0]*T[2,2]*T[3,3] +
       T[0,0]*T[1,1]*T[2,2]*T[3,3];

  if Abs(det) < Small then

    Result:=false

  else begin
    Tr_[0,0]:=(-T[1,3]*T[2,2]*T[3,1] + T[1,2]*T[2,3]*T[3,1] +
                T[1,3]*T[2,1]*T[3,2] - T[1,1]*T[2,3]*T[3,2] -
                T[1,2]*T[2,1]*T[3,3] + T[1,1]*T[2,2]*T[3,3])/det;

    Tr_[0,1]:=( T[0,3]*T[2,2]*T[3,1] - T[0,2]*T[2,3]*T[3,1] -
                T[0,3]*T[2,1]*T[3,2] + T[0,1]*T[2,3]*T[3,2] +
                T[0,2]*T[2,1]*T[3,3] - T[0,1]*T[2,2]*T[3,3])/det;

    Tr_[0,2]:=(-T[0,3]*T[1,2]*T[3,1] + T[0,2]*T[1,3]*T[3,1] +
                T[0,3]*T[1,1]*T[3,2] - T[0,1]*T[1,3]*T[3,2] -
                T[0,2]*T[1,1]*T[3,3] + T[0,1]*T[1,2]*T[3,3])/det;

    Tr_[0,3]:=( T[0,3]*T[1,2]*T[2,1] - T[0,2]*T[1,3]*T[2,1] -
                T[0,3]*T[1,1]*T[2,2] + T[0,1]*T[1,3]*T[2,2] +
                T[0,2]*T[1,1]*T[2,3] - T[0,1]*T[1,2]*T[2,3])/det;

    Tr_[1,0]:=( T[1,3]*T[2,2]*T[3,0] - T[1,2]*T[2,3]*T[3,0] -
                T[1,3]*T[2,0]*T[3,2] + T[1,0]*T[2,3]*T[3,2] +
                T[1,2]*T[2,0]*T[3,3] - T[1,0]*T[2,2]*T[3,3])/det;

    Tr_[1,1]:=(-T[0,3]*T[2,2]*T[3,0] + T[0,2]*T[2,3]*T[3,0] +
                T[0,3]*T[2,0]*T[3,2] - T[0,0]*T[2,3]*T[3,2] -
                T[0,2]*T[2,0]*T[3,3] + T[0,0]*T[2,2]*T[3,3])/det;

    Tr_[1,2]:=( T[0,3]*T[1,2]*T[3,0] - T[0,2]*T[1,3]*T[3,0] -
                T[0,3]*T[1,0]*T[3,2] + T[0,0]*T[1,3]*T[3,2] +
                T[0,2]*T[1,0]*T[3,3] - T[0,0]*T[1,2]*T[3,3])/det;

    Tr_[1,3]:=(-T[0,3]*T[1,2]*T[2,0] + T[0,2]*T[1,3]*T[2,0] +
                T[0,3]*T[1,0]*T[2,2] - T[0,0]*T[1,3]*T[2,2] -
                T[0,2]*T[1,0]*T[2,3] + T[0,0]*T[1,2]*T[2,3])/det;

    Tr_[2,0]:=(-T[1,3]*T[2,1]*T[3,0] + T[1,1]*T[2,3]*T[3,0] +
                T[1,3]*T[2,0]*T[3,1] - T[1,0]*T[2,3]*T[3,1] -
                T[1,1]*T[2,0]*T[3,3] + T[1,0]*T[2,1]*T[3,3])/det;

    Tr_[2,1]:=( T[0,3]*T[2,1]*T[3,0] - T[0,1]*T[2,3]*T[3,0] -
                T[0,3]*T[2,0]*T[3,1] + T[0,0]*T[2,3]*T[3,1] +
                T[0,1]*T[2,0]*T[3,3] - T[0,0]*T[2,1]*T[3,3])/det;

    Tr_[2,2]:=(-T[0,3]*T[1,1]*T[3,0] + T[0,1]*T[1,3]*T[3,0] +
                T[0,3]*T[1,0]*T[3,1] - T[0,0]*T[1,3]*T[3,1] -
                T[0,1]*T[1,0]*T[3,3] + T[0,0]*T[1,1]*T[3,3])/det;

    Tr_[2,3]:=( T[0,3]*T[1,1]*T[2,0] - T[0,1]*T[1,3]*T[2,0] -
                T[0,3]*T[1,0]*T[2,1] + T[0,0]*T[1,3]*T[2,1] +
                T[0,1]*T[1,0]*T[2,3] - T[0,0]*T[1,1]*T[2,3])/det;

    Tr_[3,0]:=( T[1,2]*T[2,1]*T[3,0] - T[1,1]*T[2,2]*T[3,0] -
                T[1,2]*T[2,0]*T[3,1] + T[1,0]*T[2,2]*T[3,1] +
                T[1,1]*T[2,0]*T[3,2] - T[1,0]*T[2,1]*T[3,2])/det;

    Tr_[3,1]:=(-T[0,2]*T[2,1]*T[3,0] + T[0,1]*T[2,2]*T[3,0] +
                T[0,2]*T[2,0]*T[3,1] - T[0,0]*T[2,2]*T[3,1] -
                T[0,1]*T[2,0]*T[3,2] + T[0,0]*T[2,1]*T[3,2])/det;

    Tr_[3,2]:=( T[0,2]*T[1,1]*T[3,0] - T[0,1]*T[1,2]*T[3,0] -
                T[0,2]*T[1,0]*T[3,1] + T[0,0]*T[1,2]*T[3,1] +
                T[0,1]*T[1,0]*T[3,2] - T[0,0]*T[1,1]*T[3,2])/det;

    Tr_[3,3]:=(-T[0,2]*T[1,1]*T[2,0] + T[0,1]*T[1,2]*T[2,0] +
                T[0,2]*T[1,0]*T[2,1] - T[0,0]*T[1,2]*T[2,1] -
                T[0,1]*T[1,0]*T[2,2] + T[0,0]*T[1,1]*T[2,2])/det;
  end
end;

function Matrix_norm(k, x0,y0,z0: Double): TMatrix;
begin
  Result:=Matrix_identify;
  Result[0,0]:=k; Result[0,3]:=-k*x0;
  Result[1,1]:=k; Result[1,3]:=-k*y0;
  Result[2,2]:=k; Result[2,3]:=-k*z0;
end;

function Matrix_denorm(const N: TMatrix): TMatrix;
var
  k,x0,y0,z0: Double;
begin
  Result:=Matrix_identify;

  k:=1 / N[0,0];

  x0:=-N[0,3] * k;
  y0:=-N[1,3] * k;
  z0:=-N[2,3] * k;

  Result[0,0]:=k; Result[0,3]:=x0;
  Result[1,1]:=k; Result[1,3]:=y0;
  Result[2,2]:=k; Result[2,3]:=z0;
end;

function Matrix_reper(const T: TMatrix): TSpace;
begin
  Result[1]:=_xyz(T[0,0],T[1,0],T[2,0]);
  Result[2]:=_xyz(T[0,1],T[1,1],T[2,1]);
  Result[3]:=_xyz(T[0,2],T[1,2],T[2,2]);
end;

function Matrix_space(const s: TSpace): TMatrix;
var
  i: Integer;
begin
  Result:=Matrix_identify;

  for i:=0 to 2 do with s[i+1] do begin
    Result[i,0]:=x; Result[i,1]:=y; Result[i,2]:=z;
  end
end;

function Space_Matrix(const T: TMatrix): TSpace;
var
  i: Integer;
begin
  for i:=0 to 2 do begin
    Result[i+1].x:=T[i,0];
    Result[i+1].y:=T[i,1];
    Result[i+1].z:=T[i,2];
  end
end;

function Matrix_Scale(k: Double): TMatrix;
var
  i: Integer;
begin
  Result:=Matrix_identify;
  for i:=0 to 2 do Result[i,i]:=k;
end;

function Matrix_XScale(kx,ky,kz: Double): TMatrix;
begin
  Result:=Matrix_identify;
  Result[0,0]:=kx;
  Result[1,1]:=ky;
  Result[2,2]:=kz;
end;

function Matrix_Translate(x,y,z: Double): TMatrix;
begin
  Result:=Matrix_identify;
  Result[0,3]:=x; Result[1,3]:=y;
  Result[2,3]:=z;
end;

function Matrix_RotateX(fi: Double): TMatrix;
var
  c,s: Extended;
begin
  SinCos(fi,s,c);
  Result:=Matrix_identify;
  Result[1,1]:=c; Result[1,2]:=-s;
  Result[2,1]:=s; Result[2,2]:=c;
end;

function Matrix_RotateY(fi: Double): TMatrix;
var
  c,s: Extended;
begin
  SinCos(fi,s,c);
  Result:=Matrix_identify;
  Result[0,0]:=c;  Result[0,2]:=-s;
  Result[2,0]:=s; Result[2,2]:=c;
end;

function Matrix_RotateZ(fi: Double): TMatrix;
var
  c,s: Extended;
begin
  SinCos(fi,s,c);
  Result:=Matrix_identify;
  Result[0,0]:=c; Result[0,1]:=-s;
  Result[1,0]:=s; Result[1,1]:=c;
end;

function Matrix_rotx(c,s: Double): TMatrix;
begin
  Result:=Matrix_identify;
  Result[1,1]:=c; Result[1,2]:=-s;
  Result[2,1]:=s; Result[2,2]:=c;
end;

function Matrix_roty(c,s: Double): TMatrix;
begin
  Result:=Matrix_identify;
  Result[0,0]:=c;  Result[0,2]:=-s;
  Result[2,0]:=s; Result[2,2]:=c;
end;

function Matrix_rotz(c,s: Double): TMatrix;
begin
  Result:=Matrix_identify;
  Result[0,0]:=c; Result[0,1]:=-s;
  Result[1,0]:=s; Result[1,1]:=c;
end;

function Matrix_rot(fi, vx,vy,vz: Double): TMatrix;
var
  d: Double; r: TMatrix; 
begin
  r:=Matrix_identify;
  if fi <> 0 then

  if Abs(vx) = 1 then
    r:=Matrix_RotateX(vx*fi)
  else
  if Abs(vy) = 1 then
    r:=Matrix_RotateY(vy*fi)
  else
  if Abs(vz) = 1 then
    r:=Matrix_RotateZ(vz*fi)
  else begin
    d:=Hypot(vy,vz);
    if Abs(d) < Small then
      r:=Matrix_RotateX(vx*fi)
    else begin
      r:=Matrix_rotx(vz/d,vy/d);
      roty_matrix(r,d,vx);

      RotateZ_Matrix(r,fi);

      roty_matrix(r,d,-vx);
      rotx_matrix(r,vz/d,-vy/d)
    end
  end;

  Result:=r
end;

function Matrix_Normal(fx,fy: Double): TMatrix;
var
  sinX,cosX,sinY,cosY: Extended;
begin
  SinCos(fx,sinX,cosX);
  SinCos(fy,sinY,cosY);
  Result:=Matrix_identify;
  Result[0,0]:=cosY;
  Result[0,2]:=-sinY;
  Result[1,0]:=-sinY*sinX;
  Result[1,1]:=cosX;
  Result[1,2]:=-cosY*sinX;
  Result[2,0]:=cosX*sinY;
  Result[2,1]:=sinX;
  Result[2,2]:=cosY*cosX;
end;

function Matrix_head(x,z: Double): TMatrix;
var
  c,s,r: Double;
begin
  Result:=Matrix_identify;
  r:=Hypot(x,z); if r > 0.001 then begin
    c:=x/r; s:=z/r;
    Result[0,0]:=c;  Result[0,2]:=-s;
    Result[2,0]:=s;  Result[2,2]:=c;
  end
end;

function Matrix_pitch(x,y: Double): TMatrix;
var
  c,s,r: Double;
begin
  Result:=Matrix_identify;
  r:=Hypot(x,y); if r > 0.001 then begin
    c:=x/r; s:=y/r;
    Result[1,1]:=c; Result[1,2]:=-s;
    Result[2,1]:=s; Result[2,2]:=c;
  end
end;

function Matrix_LookAt(x,y,z: Double): TMatrix;
var
  T1,T2: TMatrix;
begin
  T1:=Matrix_head(x,z);
  T2:=Matrix_pitch(Hypot(x,z),y);
  Result:=Matrix_mult(T1,T2)
end;

function Matrix_Orient(alfa,beta,psi: Double): TMatrix;
var
  s: TSpace;
begin
  s:=Orient_to_Space(alfa,beta,psi);
  Result:=Matrix_space(s)
end;

function Matrix_Reverse(alfa,beta,psi: Double): TMatrix;
var
  s: TSpace;
begin
  s:=Orient_to_Space(alfa,beta,psi);
  Result:=Swap_Matrix( Matrix_space(s) )
end;

function Rotate_Reverse(const R: TRotate_awk): TMatrix;
var
  t: TMatrix;
begin
  t:=Matrix_space(R.s.s);
  Result:=Swap_Matrix(t);
end;

function Matrix_to_Orient(const R: TMatrix): txyz;
begin
  Result.y:=Arctan2(-R[0,2],R[2,2]); // alfa
  Result.x:=Arcsin(-R[1,2]);         // psi
  Result.z:=Arctan2(R[1,0],R[1,1]);  // beta
end;

function rot_xyz_to_mat(const R: txyz): TMatrix;
var
  m: TMatrix;
begin
  m:=Matrix_RotateX(R.x);
  RotateY_Matrix(m,R.y);
  RotateZ_Matrix(m,R.z);
  Result:=m;
end;

function rot_yxz_to_mat(const R: txyz): TMatrix;
var
  m: TMatrix;
begin
  m:=Matrix_RotateY(R.x);
  RotateX_Matrix(m,R.y);
  RotateZ_Matrix(m,R.z);
  Result:=m;
end;

function mat_to_rot_xyz(const R: TMatrix): txyz;
begin
  Result.x:=Arctan2(R[2,1],R[2,2]);
  Result.y:=Arcsin(R[2,0]);
  Result.z:=Arctan2(R[1,0],R[0,0]);
end;

function mat_to_rot_yxz(const R: TMatrix): txyz;
begin
  Result.x:=Arcsin(R[2,1]);
  Result.y:=Arctan2(R[2,0],R[2,2]);
  Result.z:=Arctan2(-R[0,1],R[1,1]);
end;

// R = Ry*Rx*Rz
function Matrix_to_Rotate(const R: TMatrix): txyz;
begin
  Result:=mat_to_rot_yxz(R)
end;

function Matrix_x_s(const A: TMatrix; s: Double): TMatrix;
var
  i,j: Integer;
begin
  for i:=0 to 3 do
  for j:=0 to 3 do
  Result[i,j]:=A[i,j] * s
end;

function Matrix_add(const A,B: TMatrix): TMatrix;
var
  i,j: Integer;
begin
  for i:=0 to 3 do
  for j:=0 to 3 do
  Result[i,j]:=A[i,j] + B[i,j]
end;

function Matrix_mult(const A,B: TMatrix): TMatrix;
var
  i,j,k: Integer; Ax: Extended;
begin
  for i:=0 to 3 do for j:=0 to 3 do begin
    Ax:=0; for k:=0 to 3 do Ax:=Ax+A[i,k]*B[k,j];
    Result[i,j]:=Ax
  end
end;

procedure Matrix_forw(var A: TMatrix; const B: TMatrix);
var
  T: TMatrix;
begin
  T:=Matrix_mult(B,A); A:=T
end;

procedure Matrix_plan(var A: TMatrix; const B: Real3x3);
var
  T: TMatrix;
begin
  T:=Matrix_mult(Real3x3_Matrix(B),A); A:=T
end;

procedure Scale_Matrix(var T: TMatrix; k: Double);
var
  S: TMatrix;
begin
  S:=Matrix_mult( Matrix_Scale(k),T ); T:=S
end;

procedure XScale_Matrix(var T: TMatrix; kx,ky,kz: Double);
var
  S: TMatrix;
begin
  S:=Matrix_mult( Matrix_XScale(kx,ky,kz),T ); T:=S
end;

procedure RotateX_Matrix(var T: TMatrix; fi: Double);
var
  S: TMatrix;
begin
  S:=Matrix_mult( Matrix_RotateX(fi),T ); T:=S
end;

procedure RotateY_Matrix(var T: TMatrix; fi: Double);
var
  S: TMatrix;
begin
  S:=Matrix_mult( Matrix_RotateY(fi),T ); T:=S
end;

procedure RotateZ_Matrix(var T: TMatrix; fi: Double);
var
  S: TMatrix;
begin
  S:=Matrix_mult( Matrix_RotateZ(fi),T ); T:=S
end;

procedure rotx_Matrix(var T: TMatrix; c,s: Double);
var
  r: TMatrix;
begin
  r:=Matrix_mult( Matrix_rotx(c,s),T ); T:=r
end;

procedure roty_Matrix(var T: TMatrix; c,s: Double);
var
  r: TMatrix;
begin
  r:=Matrix_mult( Matrix_roty(c,s),T ); T:=r
end;

procedure rotz_Matrix(var T: TMatrix; c,s: Double);
var
  r: TMatrix;
begin
  r:=Matrix_mult( Matrix_rotz(c,s),T ); T:=r
end;

procedure xy_Rotate_Matrix(var T: TMatrix; x,y: Double);
var
  R,S: TMatrix; l,cos,sin: Extended;
begin
  l:=Hypot(x,y); if l > 0.001 then begin

    cos:=x/l; sin:=y/l;

    R:=Matrix_identify;
    R[0,0]:=cos;  R[0,1]:=sin;
    R[1,0]:=-sin; R[1,1]:=cos;

    S:=Matrix_mult(R,T); T:=S
  end
end;

procedure xz_Rotate_Matrix(var T: TMatrix; x,z: Double);
var
  R,S: TMatrix; l,cos,sin: Extended;
begin
  l:=Hypot(x,z); if l > 0.001 then begin

    cos:=x/l; sin:=z/l;

    R:=Matrix_identify;
    R[0,0]:=cos;  R[0,2]:=sin;
    R[2,0]:=-sin; R[2,2]:=cos;

    S:=Matrix_mult(R,T); T:=S
  end
end;

procedure Translate_Matrix(var T: TMatrix; x,y,z: Double);
var
  S: TMatrix;
begin
  S:=Matrix_mult( Matrix_Translate(x,y,z),T ); T:=S
end;

procedure Orient_Matrix(var T: TMatrix; alfa,beta,psi: Double);
var
  S: TMatrix;
begin
  S:=Matrix_mult( Matrix_Orient(alfa,beta,psi),T ); T:=S
end;

function Line_Matrix(out Tr: TMatrix;
                     const a,b: txyz): Boolean;
var
  v: txyz; l2: Double;
begin
  Result:=false; Tr:=Matrix_Identify;

  v:=xyz_dir(a,b);

  l2:=Hypot(v.x,v.y);
  if Hypot(l2,v.z) > 0.001 then begin
    Tr:=Matrix_Translate(-a.x,-a.y,-a.z);
    xy_Rotate_Matrix(Tr,v.x,v.y);
    xz_Rotate_Matrix(Tr,l2,v.z);
    Result:=true
  end
end;

function iLine_Matrix(out Tr: TMatrix;
                      const a,b: txyz): Boolean;
var
  v: txyz; l2: Double;
begin
  Result:=false; Tr:=Matrix_Identify;

  v:=xyz_dir(a,b);

  l2:=Hypot(v.x,v.y);
  if Hypot(l2,v.z) > 0.001 then begin
    xz_Rotate_Matrix(Tr,l2,-v.z);
    xy_Rotate_Matrix(Tr,v.x,-v.y);
    Translate_Matrix(Tr,a.x,a.y,a.z);
    Result:=true
  end
end;

function lRotate_Matrix(out Tr: TMatrix;
                        const a,b,c: txyz): Boolean;
var
  n: txyz; l2: Double; p1,p2: txyz; tr_: TMatrix;
begin
  Result:=false; Tr:=Matrix_Identify;

  n:=xyz_vec(xyz_dir(a,b),xyz_dir(a,c));

  l2:=Hypot(n.x,n.y);
  if Hypot(l2,n.z) > 0.001 then begin
    Tr:=Matrix_Translate(-a.x,-a.y,-a.z);
    xy_Rotate_Matrix(Tr,n.x,n.y);
    xz_Rotate_Matrix(Tr,n.z,l2);

    if Inverse_matrix(Tr,tr_) then begin

      p1:=xyz_transform(b.x,b.y,b.z,tr);
      p2:=xyz_transform(c.x,c.y,c.z,tr);
      RotateZ_Matrix(Tr,ArcTan2(p2.y,p2.x) - ArcTan2(p1.y,p1.x));

      Matrix_forw(Tr,tr_); Result:=true
    end
  end
end;

function swap_matrix(const T: TMatrix): TMatrix;
var
  i,j: Integer; S: TMatrix;
begin
  for i:=0 to 3 do for j:=0 to 3 do
  S[i,j]:=T[j,i]; Result:=S
end;

procedure Matrix_x_vector(const T: TMatrix; U,V: PDoubles);
var
  i,k: Integer; Ax: Extended;
begin
  for i:=0 to 3 do begin Ax:=0;
    for k:=0 to 3 do Ax:=Ax+T[i,k]*U[k];
    V[i]:=Ax
  end
end;

function xyz_transform(x,y,z: Double; const T: TMatrix): txyz;
begin
  Result.x:=x*T[0,0] + y*T[0,1] + z*T[0,2] + T[0,3];
  Result.y:=x*T[1,0] + y*T[1,1] + z*T[1,2] + T[1,3];
  Result.z:=x*T[2,0] + y*T[2,1] + z*T[2,2] + T[2,3];
end;

function xyz_matrix(const v: txyz; const T: TMatrix): txyz;
var
  w: txyz;
begin
  with v do begin
    w.x:=x*T[0,0] + y*T[0,1] + z*T[0,2] + T[0,3];
    w.y:=x*T[1,0] + y*T[1,1] + z*T[1,2] + T[1,3];
    w.z:=x*T[2,0] + y*T[2,1] + z*T[2,2] + T[2,3];
    Result:=w
  end
end;

procedure xyzv_matrix(v: pxyz_array; count: Integer;
                      const T: TMatrix);
var
  i: Integer; _v: txyz;
begin
  for i:=1 to Count do begin

    with v[0] do begin
      _v.x:=x*T[0,0] + y*T[0,1] + z*T[0,2] + T[0,3];
      _v.y:=x*T[1,0] + y*T[1,1] + z*T[1,2] + T[1,3];
      _v.z:=x*T[2,0] + y*T[2,1] + z*T[2,2] + T[2,3];
    end;

    v[0]:=_v; v:=@v[1]
  end
end;

procedure xyzv_min_max(v: pxyz_array; n: Integer;
                       out vmin,vmax: txyz);
var
  i: Integer; v1,v2: txyz;
begin
  v1:=v[0]; v2:=v1;

  for i:=1 to n-1 do begin
    v:=@v[1];

    v1.x:=Min(v1.x,v[0].x);
    v2.x:=Max(v2.x,v[0].x);

    v1.y:=Min(v1.y,v[0].y);
    v2.y:=Max(v2.y,v[0].y);

    v1.z:=Min(v1.z,v[0].z);
    v2.z:=Max(v2.z,v[0].z);
  end;

  vmin:=v1; vmax:=v2
end;

function xyzv_centre(vp: pxyz_array; vn: int): txyz;
var
  i: int; v: txyz; cx,cy,cz: Extended;
begin
  cx:=0; cy:=0; cz:=0;

  if vn > 0 then begin
    for i:=0 to vn-1 do begin
      v:=vp[i];
      cx:=cx+v.x;
      cy:=cy+v.y;
      cz:=cz+v.z;
    end;

    cx:=cx/vn;
    cy:=cy/vn;
    cz:=cz/vn;
  end;

  Result.x:=cx;
  Result.y:=cy;
  Result.z:=cz;
end;
  
function Bound_to_Volume(const lt,rb: txyz; V: pxyz_Array): Integer;
var
  i: Integer;
begin
  V[0].x:=lt.x; V[0].y:=lt.y;
  V[1].x:=lt.x; V[1].y:=rb.y;
  V[2].x:=rb.x; V[2].y:=rb.y;
  V[3].x:=rb.x; V[3].y:=lt.y;
  for i:=0 to 3 do v[i].z:=lt.z;

  for i:=4 to 7 do begin
    v[i]:=v[i-4]; v[i].z:=rb.z
  end; Result:=8
end;

function cube_identify: tcube;
begin
  Result.vmin:=xyz_nil;
  Result.vmax:=xyz_nil;
end;

procedure max_cube(cube: pcube; v: pxyz);
begin
  with cube^ do begin
    xmin:=Min(xmin,v.x); xmax:=Max(xmax,v.x);
    ymin:=Min(ymin,v.y); ymax:=Max(ymax,v.y);
    zmin:=Min(zmin,v.z); zmax:=Max(zmax,v.z);
  end
end;

procedure add_cube(cube1,cube2: pcube);
begin
  max_cube(cube1,@cube2.vmin);
  max_cube(cube1,@cube2.vmax);
end;

function max_dist_to_cube(const v: txyz; const c: tcube): Double;
var
  i,n: Integer; vl: txyz_volume; r: Double;
begin
  r:=xyz_dist(v,c.vmin);
  r:=Max(r,xyz_dist(v,c.vmin));

  n:=Bound_to_Volume(c.vmin,c.vmax,@vl);
  for i:=0 to n-1 do
  r:=Max(r,xyz_dist(v,vl[i]));

  Result:=r
end;

function eps_dv(const v,e: txyz): Boolean;
begin
  Result:=(Abs(v.x) > e.x) or
          (Abs(v.y) > e.y) or
          (Abs(v.z) > e.z)
end;

function xyz_equal(const a,b: txyz; eps: Double): Boolean;
begin
  Result:=(Round(a.x*eps) = Round(b.x*eps)) and
          (Round(a.y*eps) = Round(b.y*eps)) and
          (Round(a.z*eps) = Round(b.z*eps))
end;

procedure xyz_inc(var a: txyz; const b: txyz);
begin
  a.x:=a.x + b.x;
  a.y:=a.y + b.y;
  a.z:=a.z + b.z;
end;

procedure xyz_dec(var a: txyz; const b: txyz);
begin
  a.x:=a.x - b.x;
  a.y:=a.y - b.y;
  a.z:=a.z - b.z;
end;

procedure xyz_max(var a: txyz; const b: txyz);
begin
  a.x:=Max(a.x,b.x);
  a.y:=Max(a.y,b.y);
  a.z:=Max(a.z,b.z);
end;

procedure xyz_sqr(var a: txyz; const b: txyz);
begin
  a.x:=a.x + b.x*b.x;
  a.y:=a.y + b.y*b.y;
  a.z:=a.z + b.z*b.z;
end;

procedure xyz_mult(var v: txyz; k: Double);
begin
  v.x:=v.x * k;
  v.y:=v.y * k;
  v.z:=v.z * k;
end;

procedure xyz_sqrt(var v: txyz; k: Double);
begin
  v.x:=Sqrt(v.x * k);
  v.y:=Sqrt(v.y * k);
  v.z:=Sqrt(v.z * k);
end;

function xyz_scale(const v: txyz; k: Double): txyz;
begin
  Result.x:=v.x * k;
  Result.y:=v.y * k;
  Result.z:=v.z * k;
end;

function xyz_scale1(const v,k: txyz): txyz;
begin
  Result.x:=v.x * k.x;
  Result.y:=v.y * k.y;
  Result.z:=v.z * k.z;
end;

function xyz_add(const a,b: txyz): txyz;
begin
  Result.x:=a.x + b.x;
  Result.y:=a.y + b.y;
  Result.z:=a.z + b.z;
end;

function xyz_dir(const a,b: txyz): txyz;
begin
  Result.x:=b.x - a.x;
  Result.y:=b.y - a.y;
  Result.z:=b.z - a.z;
end;

function xyz_sub(const a,b: txyz): txyz;
begin
  Result.x:=a.x - b.x;
  Result.y:=a.y - b.y;
  Result.z:=a.z - b.z;
end;

// векторное произведение 2-х векторов
function xyz_vec(const a,b: txyz): txyz;
begin
  Result.x:=a.y*b.z - a.z*b.y;
  Result.y:=a.z*b.x - a.x*b.z;
  Result.z:=a.x*b.y - a.y*b.x;
end;

function xyz_dist(const a,b: txyz): Double;
var
  vx,vy,vz: Double;
begin
  vx:=b.x-a.x; vy:=b.y-a.y; vz:=b.z-a.z;
  Result:=Sqrt(vx*vx + vy*vy + vz*vz)
end;

function xyz_dist2(const a,b: txyz): Double;
var
  vx,vy,vz: Double;
begin
  vx:=b.x-a.x; vy:=b.y-a.y; vz:=b.z-a.z;
  Result:=vx*vx + vy*vy + vz*vz
end;

function xyz_length(const v: txyz): Double;
begin
  with v do Result:=Sqrt(x*x + y*y + z*z)
end;

function xyz_norm(var v: txyz): Boolean;
var
  r: Double;
begin
  Result:=false; r:=xyz_length(v);
  if r > Small then begin
    v.x:=v.x/r; v.y:=v.y/r;
    v.z:=v.z/r; Result:=true
  end
end;

function xyz_centre(const a,b: txyz): txyz;
begin
  Result.x:=a.x/2 + b.x/2;
  Result.y:=a.y/2 + b.y/2;
  Result.z:=a.z/2 + b.z/2;
end;

function v_scalar(const a,b: txyz): Double;
begin
  Result:=a.x*b.x + a.y*b.y + a.z*b.z
end;

const
  v_small = 0.0001;

function v_scalar_0(const a,b: txyz): Boolean;
begin
  Result:=eps_Equal(v_scalar(a,b),0,v_small)
end;

function v_scalar_1(const a,b: txyz): Boolean;
begin
  Result:=eps_Equal(v_scalar(a,b),1,v_small)
end;

function xyz_step(const a,b: txyz; r: Double): txyz;
var
  v: txyz; len: Double;
begin
  v:=xyz_nil;

  len:=xyz_dist(a,b);
  if len >= v_small then
  v:=xyz_scale(xyz_dir(a,b),1/len);

  Result:=xyz_scale(v,r)
end;

function xyz_project(const a,b: txyz; r: Double): txyz;
begin
  Result:=xyz_add(a,xyz_step(a,b,r))
end;

function xyz_alfa(const a,b: txyz): Double;
var
  v: txyz;
begin
  v:=xyz_dir(a,b);
  Result:=ArcTan2(v.y,Hypot(v.x,v.z))
end;

function xyz_beta(const a,b: txyz): Double;
var
  v: txyz;
begin
  v:=xyz_dir(a,b);
  Result:=ArcTan2(v.z,v.x);
end;

function xyz_ray(r, alfa,beta: Double): txyz;
var
  s,c: Extended;
begin
  SinCos(alfa,s,c);
  Result.y:=r*s; r:=r*c;

  SinCos(beta,s,c);
  Result.x:=r*c;
  Result.z:=r*s;
end;

function xyz_up(const a,b: txyz): txyz;
var
  alfa,beta,r: Double; up: txyz;
begin
  alfa:=xyz_alfa(a,b);
  beta:=xyz_beta(a,b);
  r:=xyz_dist(a,b);

  up:=xyz_ray(r, alfa+Pi/2,beta);
  xyz_norm(up); Result:=up
end;

function xyz_h_z(const v,h: txyz): Double;
begin
  with v do
  Result:=z + h.z + h.x*x + h.y*y;
end;

function lines_angle(const a,b: txyz): Double;
var
  rr: Double;
begin
  rr:=xyz_length(a)*xyz_length(b);
  Result:=0; if rr > Small then
  Result:=Arccos( v_scalar(a,b) / rr)
end;

function this_space(const s: TSpace): Boolean;
begin
  Result:=v_scalar_0(s[1],s[2]) and
          v_scalar_0(s[1],s[3]) and
          v_scalar_0(s[2],s[3]) and

          v_scalar_1(s[1],s[1]) and
          v_scalar_1(s[2],s[2]) and
          v_scalar_1(s[3],s[3])
end;

function norm_space(var s: TSpace): Double;
begin
  Result:=Sqrt( v_scalar(s[1],s[1]) );

  if Result > v_small then begin
    xyz_mult(s[1],1/Result);
    xyz_mult(s[2],1/Result);
    xyz_mult(s[3],1/Result)
  end
end;

function space_x_xyz(const a: txyz;
                     const b: tspace): txyz;

var
  t: TSpace;
begin
  t[1]:=xyz_scale(b[1],a.x);
  t[2]:=xyz_scale(b[2],a.y);
  t[3]:=xyz_scale(b[3],a.z);

  Result:=xyz_add(t[1],xyz_add(t[2],t[3]))
end;

function space_x_space(const a,b: TSpace): TSpace;
var
  i: Integer; a_,b_: TSpace;
begin
  a_:=a; b_:=b; for i:=1 to 3 do
  Result[i]:=space_x_xyz(a_[i],b_)
end;

function space_vw_space(const v,w: TSpace): TSpace;
var
  i: Integer;
begin
  for i:=1 to 3 do begin
    Result[i].x:=v_scalar(v[i],w[1]);
    Result[i].y:=v_scalar(v[i],w[2]);
    Result[i].z:=v_scalar(v[i],w[3])
  end
end;

function Orient_to_Space(alfa,beta,psi: Double): TSpace;
var
  sin_alfa,cos_alfa: Extended;
  sin_beta,cos_beta: Extended;
  sin_psi,cos_psi: Extended;
begin
  SinCos(alfa,sin_alfa,cos_alfa);
  SinCos(beta,sin_beta,cos_beta);
  SinCos(psi,sin_psi,cos_psi);

  Result[1].x:=cos_alfa*cos_beta - sin_alfa*sin_psi*sin_beta;
  Result[1].y:=-cos_alfa*sin_beta - sin_alfa*sin_psi*cos_beta;
  Result[1].z:=-sin_alfa*cos_psi;

  Result[2].x:=cos_psi*sin_beta;
  Result[2].y:=cos_psi*cos_beta;
  Result[2].z:=-sin_psi;

  Result[3].x:=sin_alfa*cos_beta +  cos_alfa*sin_psi*sin_beta;
  Result[3].y:=-sin_alfa*sin_beta + cos_alfa*sin_psi*cos_beta;
  Result[3].z:=cos_alfa*cos_psi
end;

function Space_to_Orient(const S: TSpace): txyz;
begin
  Result.y:=Arctan2(-S[1].z,S[3].z); // alfa
  Result.x:=Arcsin(-S[2].z);         // psi
  Result.z:=Arctan2(S[2].x,S[2].y);  // beta
end;

procedure Space_scale(var S: TSpace; k: Double);
begin
  xyz_mult(S[1],k);
  xyz_mult(S[2],k);
  xyz_mult(S[3],k);
end;

function x_to_x(const v: txyz; const S: TSpace): txyz;
begin
  Result.x:=S[1].x * v.x + S[1].y * v.y + S[1].z * v.z;
  Result.y:=S[2].x * v.x + S[2].y * v.y + S[2].z * v.z;
  Result.z:=S[3].x * v.x + S[3].y * v.y + S[3].z * v.z;
end;

function x_to_p(x,y,f: Double; const R: TSpace): TGauss;
var
  v: txyz;
begin
  v.x:=R[1].x * x + R[1].y * y + R[1].z * f;
  v.y:=R[2].x * x + R[2].y * y + R[2].z * f;
  v.z:=R[3].x * x + R[3].y * y + R[3].z * f;
  if Abs(v.z) > Small then begin
    f:=f / v.z; v.x:=v.x * f; v.y:=v.y * f;
  end;

  Result.x:=v.x; Result.y:=v.y;
end;

procedure inside_to_center(const ins: txyz;
                           const sys: XSpace;
                           out cen: txyz);
var
  x,y,z: Double;
begin
  x:=ins.x; y:=ins.y; z:=ins.z; with sys do begin
    cen.x:=x * e[1].x + y * e[1].y + z * e[1].z + o.x;
    cen.y:=x * e[2].x + y * e[2].y + z * e[2].z + o.y;
    cen.z:=x * e[3].x + y * e[3].y + z * e[3].z + o.z
  end
end;

procedure center_to_inside(const cen: txyz;
                           const sys: XSpace;
                           out ins: txyz);
var
  x,y,z: Double;
begin
  with sys do begin
    x:=cen.x - o.x; y:=cen.y - o.y; z:=cen.z - o.z;
    ins.x:=x * e[1].x + y * e[2].x + z * e[3].x;
    ins.y:=x * e[1].y + y * e[2].y + z * e[3].y;
    ins.z:=x * e[1].z + y * e[2].z + z * e[3].z;
  end
end;

function Rotate_identify: TRotate_awk;
begin
  Rotate_init(Result,0,0,0)
end;

function Rotate_rewind(a,w,k: Double): TRotate_awk;
var
  T: TMatrix; v: txyz;
begin
  T:=Matrix_Reverse(a,k,w);
  v:=Matrix_to_Orient(T);
  Rotate_init(Result, v.y,v.x,v.z)
end;

procedure Rotate_init(out R: TRotate_awk; a,w,k: Double);
begin
  R.a:=pi_Angle(a);
  R.w:=pi_Angle(w);
  R.k:=pi_Angle(k);
  R.s.s:=Orient_to_Space(a,k,w);

  SinCos(a,R.sin_a,R.cos_a);
  SinCos(w,R.sin_w,R.cos_w);
  SinCos(k,R.sin_k,R.cos_k);
end;

procedure Rotate_forw(var R: TRotate_awk; da,dw,dk: Double);
begin
  with R do Rotate_init(R,a+da,w+dw,k+dk)
end;

procedure Rotate_next(var R: TRotate_awk; da,dw,dk: Double);
var
  T,dT: TMatrix; v: txyz;
begin
  T:=Matrix_Orient(R.a,R.k,R.w);
  dT:=Matrix_Orient(da,dk,dw);
  Matrix_forw(T,dT);

  v:=Matrix_to_Orient(T);
  Rotate_init(R, v.y,v.x,v.z)
end;

procedure Rotate_back(var R: TRotate_awk; da,dw,dk: Double);
var
  T,dT: TMatrix; v: txyz;
begin
  T:=Matrix_Orient(R.a,R.k,R.w);
  dT:=Matrix_Reverse(da,dk,dw);
  Matrix_forw(T,dT);

  v:=Matrix_to_Orient(T);
  Rotate_init(R, v.y,v.x,v.z)
end;

function diff_awk(const p,v: txyz; const R: TRotate_awk): TSpace;
begin
  with R do begin
    Result[1].x:=-v.z;
    Result[1].y:=0;
    Result[1].z:=v.x;

    Result[2].x:=-sin_a*v.y;
    Result[2].y:=-sin_w*(sin_k*p.x + cos_k*p.y) - cos_w*p.z;
    Result[2].z:=cos_a*v.y;

    Result[3].x:=s.a2*p.x - s.a1*p.y;
    Result[3].y:=s.b2*p.x - s.b1*p.y;
    Result[3].z:=s.c2*p.x - s.c1*p.y
  end
end;

function plane3(const p,n: txyz): tplane3;
var
  pl: tplane3;
begin
  pl.n:=n; xyz_norm(pl.n);
  pl.d:=-v_scalar(pl.n,p);
  Result:=pl
end;

function Init_line3(out ln: tline3; const a,b: txyz): Boolean;
begin
  Result:=false; ln.c:=a;
  ln.n:=xyz_dir(a,b);
  Result:=xyz_norm(ln.n)
end;

function Init_plane3(out pl: tplane3; const a,b: txyz): Boolean;
begin
  Result:=false;
  pl.n:=xyz_dir(a,b);
  if xyz_norm(pl.n) then begin
    pl.d:=-v_scalar(pl.n,a);
    Result:=true
  end
end;

function Triangle_plane3(out pl: tplane3; tp: pxyz_array): Boolean;
var
  v1,v2: txyz;
begin
  Result:=false;

  v1:=xyz_dir(tp[0],tp[1]);
  v2:=xyz_dir(tp[0],tp[2]);

  pl.n:=xyz_vec(v1,v2);
  if xyz_norm(pl.n) then begin
    pl.d:=-v_scalar(pl.n,tp[0]);
    Result:=true
  end
end;

type
  txyz3 = Array[0..2] of txyz;

function vt_x_v(vp: pxyz_array; vn: int): txyz3;
var
  i,j,k,n1: int; m,p: pdoubles; ax: Extended;
  r: txyz3;
begin
  m:=Pointer(vp);
  p:=@r; n1:=vn-1;

  for i:=0 to 2 do
  for j:=0 to 2 do begin
    ax:=0; for k:=0 to n1 do
    ax:=ax+m[k*vn+i]*m[k*vn+j];
    p[0]:=ax; p:=@p[1]
  end;

  Result:=r
end;

procedure mt3_x_c(var m: txyz3; c: double);
var
  i: int; v: PDoubles;
begin
  v:=@m;
  for i:=0 to 8 do
  v[i]:=v[i]*c
end;

procedure mt3_sub_mt(var m1: txyz3; const m2: txyz3);
var
  i: int; v1,v2: PDoubles;
begin
  v1:=@m1; v2:=@m2;
  for i:=0 to 8 do
  v1[i]:=v1[i]-v2[i]
end;

// Геометрические основы комп. графики, стр. 72
function solve_plane(out pl: tplane3; vp: pxyz_array; vn: int): Boolean;
var
  c: txyz; m,s1,s2: txyz3;
begin
  Result:=false;

  Fillchar(pl,Sizeof(pl),0);

  if vn = 3 then
    Result:=Triangle_plane3(pl,vp)
  else
  if vn > 3 then begin
    c:=xyzv_centre(vp,vn);

    s1:=vt_x_v(@c,1);
    s2:=vt_x_v(vp,vn);

    m:=s1;

    mt3_x_c(m,5);
    mt3_sub_mt(m,s2);
  end
end;

function dist_to_line3(const ln: tline3; const p: txyz): Double;
var
  w,n: txyz;
begin
  w:=xyz_dir(ln.c,p);
  n:=xyz_vec(ln.n,w);
  Result:=xyz_length(n)
end;

function d3_to_l3(const p,a,b: txyz): Double;
var
  ln: tline3;
begin
  Result:=xyz_dist(p,a);
  if Init_line3(ln,a,b) then
  Result:=dist_to_line3(ln,p)
end;

function ilp3(const ln: tline3;
              const pl: tplane3;
              out v: txyz): Boolean;
var
  t: Double;
begin
  Result:=false;

  t:=v_scalar(pl.n,ln.n);
  if Abs(t) > 1E-8 then begin
    t:=-(v_scalar(pl.n,ln.c)+pl.d) / t;
    v.x:=ln.c.x + ln.n.x * t;
    v.y:=ln.c.y + ln.n.y * t;
    v.z:=ln.c.z + ln.n.z * t;
    Result:=true
  end
end;

function ivpt(const a,b, c,n: txyz;
              out v: txyz): Boolean;
var
  ln: tline3; pl: tplane3;
begin
  Result:=false; v:=xyz_nil;

  pl:=plane3(c,n);

  if Init_line3(ln,a,b) then
  Result:=ilp3(ln,pl,v)
end;

function izp3(const pl: tplane3;
              nz: double; out vz: double): Boolean;
var
  t: Double;
begin
  Result:=false;

  t:=pl.n.z*nz;
  if Abs(t) > 1E-8 then begin
    t:=-pl.d / t; vz:=nz * t;
    Result:=true
  end
end;

function FrustumPlane3(const p,n: txyz): TFrustumPlane3;
var
  fp: TFrustumPlane3;
begin
  fp.pl:=plane3(p,n); with fp.pl.n do
  fp.mfa:=_xyz(Abs(x),Abs(y),Abs(z));
  Result:=fp
end;

function boxFrustumTest(const box: TXyzBox;
                        const pl: TFrustumPlane3): Boolean;
var
  v: Double;
begin
  with box,pl.pl do
  v:=c.x*n.x + s.x*pl.mfa.x +
     c.y*n.y + s.y*pl.mfa.y +
     c.z*n.z + s.z*pl.mfa.z + d;
	Result:=v > 0.0
end;

function PointInFrustum(const v: txyz;
                        const Frustum: TFrustumPyramid): Integer;
var
  dist: Double;
begin
  Result:=1;

  with v do begin
    with Frustum[4].pl do // near
    dist:=X*n.x + Y*n.y + Z*n.z + d;

    if dist < 0 then Result:=32 else begin

      with Frustum[5].pl do // far
      dist:=X*n.x + Y*n.y + Z*n.z + d;

      if dist < 0 then Result:=64 else begin
        with Frustum[0].pl do // left
        dist:=X*n.x + Y*n.y + Z*n.z + d;

        if dist < 0 then Result:=2 else begin
          with Frustum[1].pl do // top
          dist:=X*n.x + Y*n.y + Z*n.z + d;

          if dist < 0 then Result:=4 else begin

            with Frustum[2].pl do // right
            dist:=X*n.x + Y*n.y + Z*n.z + d;

            if dist < 0 then Result:=8 else begin

              with Frustum[3].pl do // down
              dist:=X*n.x + Y*n.y + Z*n.z + d;

              if dist < 0 then Result:=16
            end
          end
        end
      end
    end
  end;
end;

function plane_Matrix(const pl: tplane3; vp: pxyz_array): TMatrix;
var
  tr,rot: TMatrix; v: txyz;
begin
  with vp[0] do
  tr:=Matrix_Translate(-x,-y,-z);

  with pl.n do begin
    rot:=Matrix_Identify;
    xy_Rotate_Matrix(rot,x,y);
    xz_Rotate_Matrix(rot,Hypot(x,y),z);
    xz_Rotate_Matrix(rot,0,-1);

    with xyz_sub(vp[1],vp[0]) do
    v:=xyz_transform(x,y,z,rot);

    xy_Rotate_Matrix(rot,v.x,v.y);
  end;

  Matrix_forw(tr,rot); Result:=tr
end;

function triangle_Matrix(out Tr: TMatrix; vp: pxyz_array): Boolean;
var
  pl: tplane3;
begin
  Result:=false;
  tr:=Matrix_Identify;

  if Triangle_plane3(pl,vp) then begin
    tr:=plane_Matrix(pl,vp);
    Result:=true
  end
end;

end.