unit xmath; interface

uses
  Math,otypes,xline,xyz;

type
  TStatEps = record
    np: Integer; ax: Extended;
    avev,maxv: Double;
  end;

function SolveEquation(a,b: PDoubles; n: Integer): Boolean;

function Solve_triangle(lp: PGPoly; lp_v,coeff: PDoubles): Boolean;

function Value_triangle(lp: PGPoly; lp_v: PDoubles;
                        const p: TGauss; out val: Double): Boolean;

function Solve_proj(p,q: PGPoly; out T: Real3x3): Boolean;
function Solve_proj1(a,b: PGPoly; out T: Real3x3): Boolean;
                        
function Solve_projective(p,q: PGPoly; out T1,T2: Real3x3): Boolean;
function Solve_projective1(p,q: PGPoly; out T1,T2: Real3x3): Boolean;

function Solve_affine(p,q: PGPoly; out T1,T2: Real3x3): Boolean;

function Solve_vector(a,b: PGPoly; out Tr: Real3x3): Boolean;
                      
function Is_solve_romb(p: PGPoly): Boolean;

function m_prj_to_line(x,y, k,b: Double): TGauss;
function m_dist_to_line(x,y, k,b: Double): Double;

function Solve_line(XY: PDoubles; Count,Stride: Integer;
                    out k,b: Double): Boolean;

function Is_Solve_line(XY: PDoubles; Count,Stride: Integer;
                       Eps: Double; out k,b: Double): Boolean;

function m_Solve_line(XY: PDoubles; Count,Stride: Integer;
                      out k,b: Double; out m: TMoments): Boolean;

function xSolve_line(XY: PDoubles; Count,Stride: Integer;
                     out k,b: Double; eps: Double): Integer;

function Min_Max_line(XY: PDoubles; Count,Stride: Integer;
                      k,b: Double; out v1,v2: TGauss): Double;

function win_GSmooth(kf: PFloats; r: Integer; s: Float): Integer;

procedure gauss_Singles(dst,src: PSingles; N: Integer;
                        r: Integer; Sigma: Single);

function single_range(vp: PSingles; vn: Integer;
                      out vmin,vmax: Single): Integer;

function stat_Identify: TStatEps;
procedure stat_Next(var stat: TStatEps; v: Double);
function stat_End(var stat: TStatEps; k: Double): Double;

implementation

uses
  xy;
  
(*
   Solve linear equation: A * x = b
   Parameters:
     a: n*n dim coefficient matrix. (destroyed when return)
     b: n dim vector. (carry the result when return)
     n: the order of the equation.
   Return:
     false: "a" is a singular matrix, (cannot inverse, thus has no solution).
     true:  successful. the result x is carried by the vector b.
*)

function SolveEquation(a,b: PDoubles; n: Integer): Boolean;
var
  i,j,k, i_, p,q, n1: Integer;
	d,t: Double; js: TIntegers;
begin
  Result:=false; n1:=n - 1;

  if n <= 512 then begin Result:=true;

    // Gauss elimination.
    for k:=0 to n - 2 do begin

      // Find the largest value in a[k..n-1][k..n-1].
      d:=0.0;
      for i:=k to n1 do
      for j:=k to n1 do begin

        t:=Abs(a[i * n + j]);
        if t > d then begin
          d:=t; js[k]:=j; i_:=i
        end
      end;

      // Swap the largest value to a[k][k]
      if Abs(d) < 1e-6 then begin // Singular matrix.
        Result:=false; Break
      end
      else begin

        if js[k] <> k then
        for i:=0 to n1 do begin
          p:=i * n + k; q:=i * n + js[k];
          t:=a[p]; a[p]:=a[q]; a[q]:=t;
        end;

        if i_ <> k then begin
          for j:=k to n1 do begin
            p:=k * n + j; q:=i_ * n + j;
            t:=a[p]; a[p]:=a[q]; a[q]:=t;
          end;

          t:=b[k]; b[k]:=b[i_]; b[i_]:=t;
        end
      end;

      // Gauss elimination. (1 step)
      d:=a[k * n + k];
      for j:=k + 1 to n1 do begin  // Normalize a[k][k..n-1].
        q:=k * n + j; a[q]:=a[q] / d;
      end;

      b[k]:=b[k] / d;
      for i:=k + 1 to n1 do begin  // Update a[k+1..n-1][k+1..n-1].

        p:=i * n + k;
        for j:=k + 1 to n1 do begin
          q:=i * n + j;
          a[q]:=a[q] - a[p] * a[k * n + j];
        end;

        b[i]:=b[i] - a[p] * b[k];
      end
    end;

    if Result then begin

      // Check to see if it is a singular matrix.
      d:=a[n1 * n + n1];

      if Abs(d) < 1e-6 then Result:=false

      else begin
        // Solve the equation based on U.
        b[n1]:=b[n1] / d;
        for i:=n - 2 downto 0 do begin
          t:=0; for j:=i + 1 to n1 do
          t:=t + a[i * n + j] * b[j];
          b[i]:=b[i] - t;
        end;

        // Change back to the original order.
        js[n1]:=n1;
        for k:=n1 downto 0 do begin
          if js[k] <> k then begin
            i:=js[k]; t:=b[k];
            b[k]:=b[i]; b[i]:=t;
          end
        end
      end
    end
  end
end;

function Solve_triangle(lp: PGPoly; lp_v,coeff: PDoubles): Boolean;
var
  i: Integer;
  a: array[0..2,0..2] of Double;
  b: array[0..2] of Double;
begin
  Result:=false;

  for i:=0 to 2 do begin
    a[i,0]:=1;
    a[i,1]:=lp[i].x;
    a[i,2]:=lp[i].y;
    b[i]:=lp_v[i];
    coeff[i]:=0
  end;

  if SolveEquation(@a,@b,3) then begin
    for i:=0 to 2 do coeff[i]:=b[i];
    Result:=true
  end
end;

function Value_triangle(lp: PGPoly; lp_v: PDoubles;
                        const p: TGauss; out val: Double): Boolean;
var
  c: Array[0..2] of Double;
begin
  Result:=false; val:=0;

  if Solve_triangle(lp,lp_v,@c) then begin
    val:=c[0] + c[1]*p.x + c[2]*p.y;
    Result:=true
  end
end;

procedure Norm_gauss(G: PGPoly; N: Integer;
                     out nt,bt: Real3x3);
var
  lt,rb,c: TGauss; dx,dy: Double;
begin
  Max_Gauss_Bound(G,N,lt,rb);

  Middle_gauss(lt,rb,c);
  dx:=Max(0.01,rb.x-lt.x);
  dy:=Max(0.01,rb.y-lt.y);

  Init_3x3(nt,-c.x,-c.y,1,1);
  xy_Scale_3x3(nt,2/dx,2/dy);

  Init_3x3(bt,0,0,dx/2,dy/2);
  t_Move_3x3(bt,c.x,c.y)
end;

function Solve_proj(p,q: PGPoly; out T: Real3x3): Boolean;
var
  i,j: Integer; ap: PDoubles;
  a: array[0..7,0..7] of Double;
  b: array[0..7] of Double;
  x,y,u,v: Double;
begin
  Result:=false;

  ap:=@a;
  for i:=0 to 7 do begin
    j:=i mod 4;

    x:=p[j].x; y:=p[j].y;
    u:=q[j].x; v:=q[j].y;

    if i < 4 then begin
      ap[0]:=x;
      ap[1]:=y;
      ap[2]:=1;
      ap[3]:=0;
      ap[4]:=0;
      ap[5]:=0;
      ap[6]:=-u*x;
      ap[7]:=-u*y;
      b[i]:=u
    end
    else begin
      ap[0]:=0;
      ap[1]:=0;
      ap[2]:=0;
      ap[3]:=x;
      ap[4]:=y;
      ap[5]:=1;
      ap[6]:=-v*x;
      ap[7]:=-v*y;
      b[i]:=v
    end;

    ap:=@ap[8]
  end;

  if SolveEquation(@a,@b,8) then begin
    Move(b,T,8*Sizeof(Double));
    T[3,3]:=1; Result:=true
  end
end;

function Solve_proj1(a,b: PGPoly; out T: Real3x3): Boolean;
var
  i: int; o1,o2: TGauss; tr,prj: Real3x3;
begin
  Result:=false; o1:=a[0]; o2:=b[0];

  for i:=0 to 3 do begin
    with a[i] do begin x:=x-o1.x; y:=y-o1.y end;
    with b[i] do begin x:=x-o2.x; y:=y-o2.y end;
  end;

  if Solve_proj(a,b,prj) then begin
    init_3x3(tr,-o1.x,-o1.y,1,1);
    forw_3x3(tr,prj);
    t_Move_3x3(tr,o2.x,o2.y);
    T:=tr; Result:=true
  end
end;

function Solve_projective(p,q: PGPoly; out T1,T2: Real3x3): Boolean;
begin
  Result:=false;
  T1:=Identify_3x3; T2:=T1;

  if Solve_proj(p,q,T1) then

  if Solve_proj(q,p,T2)
  or Inverse_3x3(T1,T2) then

  Result:=true
end;

function Solve_projective1(p,q: PGPoly; out T1,T2: Real3x3): Boolean;
var
  i: Integer; n1,n2,_n1,_n2,_t1,_t2: Real3x3;
begin
  Result:=false;

  T1:=Identify_3x3; T2:=T1;

  Norm_gauss(p,4,n1,_n1);
  Norm_gauss(q,4,n2,_n2);

  for i:=0 to 3 do begin
    p[i]:=Transit_3x3(p[i].x,p[i].y,n1);
    q[i]:=Transit_3x3(q[i].x,q[i].y,n2);
  end;

  if Solve_projective(p,q,_t1,_t2) then begin

    T1:=n1;
    Forw_3x3(T1,_t1);
    Forw_3x3(T1,_n2);

    T2:=n2;
    Forw_3x3(T2,_t2);
    Forw_3x3(T2,_n1);

    Result:=true
  end
end;

function Solve_affine(p,q: PGPoly; out T1,T2: Real3x3): Boolean;

function Solve_ab(p,q: PGPoly; out Tr: Real3x3): Boolean;
var
  i,j: Integer; ap: PDoubles;
  a: array[0..5,0..5] of Double;
  b: array[0..5] of Double;
  x,y,u,v: Double;
begin
  Result:=false;

  ap:=@a;
  for i:=0 to 5 do begin
    j:=i mod 3;

    x:=p[j].x; y:=p[j].y;
    u:=q[j].x; v:=q[j].y;

    if i < 3 then begin
      ap[0]:=x;
      ap[1]:=y;
      ap[2]:=1;
      ap[3]:=0;
      ap[4]:=0;
      ap[5]:=0;
      b[i]:=u
    end
    else begin
      ap[0]:=0;
      ap[1]:=0;
      ap[2]:=0;
      ap[3]:=x;
      ap[4]:=y;
      ap[5]:=1;
      b[i]:=v
    end;

    ap:=@ap[6]
  end;

  if SolveEquation(@a,@b,6) then begin
    Move(b,Tr,6*Sizeof(Double));
    Tr[3,1]:=0; Tr[3,1]:=0; Tr[3,3]:=1;
    Result:=true
  end
end;

begin
  Result:=false;

  Begin_3x3(T1,0,0); T2:=T1;

  Result:=Solve_ab(p,q,T1) and
          Inverse_3x3(T1,T2)
end;

function Solve_vector(a,b: PGPoly; out Tr: Real3x3): Boolean;

function xAngle(const p1,p2: TGauss): Double;
const
  Small = 1E-6;
var
  dx,dy: Double;
begin
  dx:=p2.x-p1.x; dy:=p2.y-p1.y;

  if Abs(dy) < Small then begin
    if dx < 0 then Result:=Pi else Result:=0
  end else
  if Abs(dx) < Small then begin
    if dy < 0 then Result:=3/2*Pi else Result:=Pi/2
  end
  else Result:=ArcTan2(dy,dx);
end;

var
  a1,b1,a2,b2,d1,d2: TGauss;
  l1,l2,f1,f2: Double;
begin                             
  Result:=false; Tr:=Identify_3x3;

  a1:=a[0]; b1:=a[1];
  a2:=b[0]; b2:=b[1];

  d1:=Gauss_dir(a1,a2);
  d2:=Gauss_dir(b1,b2);

  l1:=Hypot(d1.x,d1.y);
  l2:=Hypot(d2.x,d2.y);

  if (l1 >= 0.001) and (l2 >= 0.001) then begin

    f1:=xAngle(a1,a2);
    f2:=xAngle(b1,b2);

    with a1 do Begin_3x3(Tr,-x,-y);
    t_Scale_3x3(Tr,l2/l1);
    fi_Rotate_3x3(Tr,f1-f2);
    with b1 do t_Move_3x3(Tr,x,y);

    Result:=true
  end
end;

function Is_only_romb(const p1,p2,q1,q2: TGauss): Boolean;
var
  c: TGauss;
begin
  Result:=false;

  if LL2(p1.x,p1.y,p2.x,p2.y,
         q1.x,q1.y,q2.x,q2.y, 0.2,0.8, c) then

  if LL2(q1.x,q1.y,q2.x,q2.y,
         p1.x,p1.y,p2.x,p2.y, 0.2,0.8, c) then

  Result:=true
end;

function Is_solve_romb(p: PGPoly): Boolean;
begin
  Result:=Is_only_romb(p[0],p[1],p[2],p[3]) or
          Is_only_romb(p[0],p[2],p[1],p[3]) or
          Is_only_romb(p[0],p[3],p[1],p[2])
end;

// y = k*x + b
function Solve_line(XY: PDoubles; Count,Stride: Integer;
                    out k,b: Double): Boolean;
var
  i,bx: Integer; lp: PDoubles;
  sx,sy,sxy,sx2, d: Extended;
  x,y, x1,y1,x2,y2,dx: Double;
begin
  Result:=false; k:=0; b:=0;

  bx:=Max(2,Stride); lp:=XY;

  if Count = 2 then begin
    x1:=lp[0]; y1:=lp[1]; lp:=@lp[bx];
    x2:=lp[0]; y2:=lp[1]; dx:=x2-x1;

    if Abs(dx) < Small then begin
      k:=0; b:=y1
    end
    else begin
      k:=(y2-y1)/dx; b:=y1 - x1*k
    end;

    Result:=true
  end else
  if Count > 2 then begin

    sx2:=0; sx:=0; sy:=0; sxy:=0;

    for i:=1 to Count do begin
      x:=lp[0]; y:=lp[1]; lp:=@lp[bx];
      sx:=sx + x; sy:=sy + y;
      sxy:=sxy + x*y;
      sx2:=sx2 + x*x;
    end;

    d:=Count*sx2 - sx*sx;
    if Abs(d) > Small then begin
      k:=(Count*sxy - sx*sy) / d;
      b:=(sx2*sy - sx*sxy) / d;
      Result:=true
    end

  end
end;

function m_prj_to_line(x,y, k,b: Double): TGauss;
var
  b_,c: Double;
begin
  b_:=-x - k*y; c:=k*k + 1;

  Result.y:=(b - k*b_)/c;
  Result.x:=-k*Result.y - b_;
end;

function m_dist_to_line(x,y, k,b: Double): Double;
var
  s: Double;
begin
  Result:=0;
  s:=Sqrt(k*k + 1); if s > Small then
  Result:=(k*x - y + b) / s
end;

function Is_Solve_line(XY: PDoubles; Count,Stride: Integer;
                       Eps: Double; out k,b: Double): Boolean;
var
  i,bx: Integer; lp: PDoubles; v: Double;
begin
  Result:=Solve_line(XY,Count,Stride, k,b);

  if Result then begin

    bx:=Max(2,Stride); lp:=XY;
    for i:=1 to Count do begin

      v:=Abs(m_dist_to_line(lp[0],lp[1], k,b));

      if v > Eps then begin
        Result:=false; Break
      end;

      lp:=@lp[bx];
    end
  end
end;

function m_Solve_line(XY: PDoubles; Count,Stride: Integer;
                      out k,b: Double; out m: TMoments): Boolean;
var
  i,bx: Integer; lp: PDoubles;
  v,vc: Double; ax: Extended;
begin
  Result:=Solve_line(XY,Count,Stride, k,b);
  Fillchar(m,Sizeof(m),0);

  if Result then begin

    bx:=Max(2,Stride); lp:=XY; ax:=0;
    for i:=1 to Count do begin
      v:=Abs(m_dist_to_line(lp[0],lp[1], k,b));
      if v > m[0] then m[0]:=v; ax:=ax + v;
      lp:=@lp[bx];
    end;

    vc:=ax / Count; m[1]:=vc;

    lp:=XY; ax:=0;
    for i:=1 to Count do begin
      v:=Abs(m_dist_to_line(lp[0],lp[1], k,b));
      ax:=ax + Sqr(v/Count); lp:=@lp[bx];
    end;

    m[2]:=Sqrt(ax)
  end
end;

function xSolve_line(XY: PDoubles; Count,Stride: Integer;
                     out k,b: Double; eps: Double): Integer;
var
  i, j1,j2, dj,n, bx: Integer;
  _k,_b, t,r1,r2: Double;
  lp: PDoubles;
begin
  Result:=0; k:=0; b:=0;

  bx:=Max(2,Stride); n:=Count;

  while n >= 2 do begin

    if not Solve_line(XY,n,bx, _k,_b) then Break;
    Result:=n; k:=_k; b:=_b; if n = 2 then Break;

    lp:=XY; j1:=-1; j2:=-1; r1:=-eps; r2:=eps;

    for i:=0 to n-1 do begin

      t:=m_dist_to_line(lp[0],lp[1], _k,_b);

      if t < 0 then begin

        if t < r1 then begin
          j1:=i; r1:=t
        end

      end else
      if t > r2 then begin
        j2:=i; r2:=t
      end;

      lp:=@lp[bx];
    end;

    if j1 > j2 then
    begin i:=j1; j1:=j2; j2:=i end;

    if j2 < 0 then Break;

    if j1 >= 0 then begin
      Dec(j2); Dec(n); dj:=n - j1; if dj > 0 then
      Move(XY[(j1+1)*bx],XY[j1*bx],dj*bx*Sizeof(Double));
    end;

    Dec(n); dj:=n - j2; if dj > 0 then
    Move(XY[(j2+1)*bx],XY[j2*bx],dj*bx*Sizeof(Double));
  end
end;

function Min_Max_line(XY: PDoubles; Count,Stride: Integer;
                      k,b: Double; out v1,v2: TGauss): Double;
var
  i,bx: Integer; lp: PDoubles;
  p1,p2,p,lt,rb: TGauss;
begin
  Result:=0; v1:=gauss_nil; v2:=v1;

  if Count > 0 then begin

    bx:=Max(2,Stride); lp:=XY;
    for i:=1 to Count do begin
      p:=m_prj_to_line(lp[0],lp[1], k,b);

      if i = 1 then begin
        p1:=p; p2:=p; lt:=p; rb:=p
      end else
      if (p.x < lt.x) or (p.x > rb.x)
      or (p.y < lt.y) or (p.y > rb.y) then begin

        lt.x:=Min(p.x,lt.x); rb.x:=Max(p.x,rb.x);
        lt.y:=Min(p.y,lt.y); rb.y:=Max(p.y,rb.y);

        if (p1.x > lt.x) and (p1.y > lt.y) and
           (p1.x < rb.x) and (p1.y < rb.y) then
          p1:=p
        else
          p2:=p
      end;

      lp:=@lp[bx];
    end;

    v1:=p1; v2:=p2; Result:=Gauss_Dist(p1,p2)
  end
end;

function win_GSmooth(kf: PFloats; r: Integer; s: Float): Integer;
var
  i,r2,x: Integer; fx,ks: Double;
begin
  Result:=0; r2:=r+r;

  if s >= 0.01 then
  if (r > 0) and (r <= 32) then begin

    ks:=0; x:=-r;

    for i:=0 to r2 do begin
      fx:=Exp(-0.5*x*x/(s*s)) / (s * Sqrt(2*Pi));
      kf[i]:=fx; ks:=ks + fx; Inc(x)
    end;

    for i:=0 to r2 do kf[i]:=kf[i]/ks;

    Result:=r2+1
  end;

  if Result = 0 then
  if r >= 0 then begin
    for i:=0 to r2 do kf[i]:=0;
    kf[r]:=1
  end
end;

procedure gauss_Singles(dst,src: PSingles; N: Integer;
                        r: Integer; Sigma: Single);
var
  i,j,r2,len: Integer;
  si,di,vp,kp: PSingles; ax: Extended;
  val,vk: Array[0..33] of Single;
begin
  r2:=r+r; if N > r2 then
  if (r > 0) and (r <= 16) then
  if win_GSmooth(@vk,r,Sigma) > 0 then begin

    len:=Sizeof(Single)*r;

    Move(src[0],dst[0],len);
    Move(src[N-r],dst[N-r],len);

    Fillchar(val,Sizeof(val),0);

    si:=src;
    for i:=0 to r2-1 do begin
      val[i]:=si[0]; si:=@si[1]
    end;

    di:=@dst[r];
    for i:=1 to N-r2 do begin

      val[r2]:=si[0]; si:=@si[1];

      ax:=0; vp:=@val; kp:=@vk;
      for j:=0 to r2 do begin
        ax:=ax + vp[0]*kp[0];
        vp[0]:=vp[1]; vp:=@vp[1];
        kp:=@kp[1]
      end;

      if Abs(ax) > MaxSingle then
      ax:=MaxSingle;

      di[0]:=ax; di:=@di[1]
    end
  end
end;

function single_range(vp: PSingles; vn: Integer;
                      out vmin,vmax: Single): Integer;
var
  v1,v2,v: Single;
begin
  Result:=vn; vmin:=0; vmax:=0;

  if vn > 0 then begin
    v1:=vp[0]; v2:=v1; Dec(vn);

    while vn > 0 do begin
      vp:=@vp[1]; Dec(vn); v:=vp[0];
      if v < v1 then v1:=v;
      if v > v2 then v2:=v;
    end;

    vmin:=v1; vmax:=v2;
  end
end;

function stat_Identify: TStatEps;
var
  s: TStatEps;
begin
  Fillchar(s,Sizeof(s),0); Result:=s
end;

procedure stat_Next(var stat: TStatEps; v: Double);
begin
  with stat do begin
    Inc(np); ax:=ax + v;
    if v > maxv then maxv:=v
  end
end;

function stat_end(var stat: TStatEps; k: Double): Double;
begin
  with stat do begin
    if np > 0 then avev:=ax / np;
    Result:=avev + (maxv-avev)*k;
  end
end;

end.