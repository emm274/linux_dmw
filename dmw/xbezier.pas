unit xbezier; interface

uses
  otypes;

function xLLineToCurve(dst_lp,src_lp: PLLine;
                       dstMax: int; eps: double): int;

implementation

uses
  Math,xline,xpoly;

const
  eps_smooth: double = 0.3;

procedure ADDV2(a,b: tgauss; var c: tgauss);
begin
  c.x:=a.x + b.x;
  c.y:=a.y + b.y;
end;

procedure SUBV2(a,b: tgauss; var c: tgauss);
begin
  c.x:=a.x - b.x;
  c.y:=a.y - b.y;
end;

procedure MULV2(a: double; b: tgauss; var c: tgauss);
begin
  c.x:=a * b.x;
  c.y:=a * b.y;
end;

function DIST2(a,b: tgauss): double;
begin
  Result:=HYPOT(a.x - b.x, a.y - b.y)
end;

procedure NORM2(var v: tgauss);
var
  len: double;
begin
  len:=HYPOT(v.x,v.y);
  if len > 0 then begin
    v.x:=v.x/len; v.y:=v.y/len
  end
end;

procedure MIDDL2(a,b: tgauss; var c: tgauss);
begin
  ADDV2(a, b, c); MULV2(0.5, c, c)
end;

function DETV2(a,b: tgauss): double;
begin
  Result:=a.x * b.y - a.y * b.x
end;

procedure ORTV2(a: tgauss; var b: tgauss);
begin
  b.x:=-a.y; b.y:=a.x
end;

function SCAL2(a,b: tgauss): double;
begin
  Result:=a.x * b.x + a.y * b.y
end;

function MODV2(a: tgauss): double;
begin
  Result:=SCAL2(a, a)
end;

function distpl(const p1,p2,q: tgauss): double;
var
  v,v1: tgauss; d: double;
begin
  Result:=-1.0;

  SUBV2(p2,p1, v); SUBV2(q,p1,v1);

  d:=MODV2(v); if d >= 1.0e-10 then
  Result:=Abs(DETV2(v,v1)) / Sqrt(d)
end;


function syst(a11,a12,a21,a22,b1,b2: double; var x,y: double): Integer;
var
  d: double;
begin
  Result:=0; x:=0; y:=0;

  d:=a11*a22-a12*a21;
  if Abs(d) >= 1.0e-20 then begin
    x:=(b1*a22-b2*a12)/d;
    y:=(a11*b2-a21*b1)/d;
    Result:=1
  end
end;

procedure lineintersect(const p1,v1,p2,v2: tgauss; var z: tgauss);
begin
  syst(v1.y,-v1.x,v2.y,-v2.x,DETV2(p1,v1),DETV2(p2,v2), z.x,z.y)
end;

function llsq(p: PGPoly; m: Integer; eps: double;
              var p0,v0: tgauss): Integer;
var
  n,v1: tgauss; i,j,k: Integer;
  a,b,c,h,sx,sy,sx2,sy2,sxy,f: double;

begin
  if m = 2 then begin
    Result:=m; p0:=p[0];
    SUBV2(p[1],p[0],v0);
    Exit
  end;

  k:=m; j:=0; sx:=0; sy:=0; 
  sx2:=0; sy2:=0; sxy:=0;

  for i:=1 to m-1 do begin

    SUBV2(p[i],p[0],v1);

    sx:=sx+v1.x;
    sy:=sy+v1.y;

    sx2:=sx2+(v1.x*v1.x);
    sy2:=sy2+(v1.y*v1.y);
    sxy:=sxy+(v1.x*v1.y);

    if i < 2 then continue;

    j:=i+1; f:=j;
    a:=sx2-sx*sx/f;
    b:=sxy-sx*sy/f;
    c:=sy2-sy*sy/f;
    h:=HYPOT(a-c,b+b);

    if Abs(b) < 1.0e-20 then
      if a > c then begin
        n.x:=0; n.y:=1
      end
      else begin
        n.x:=1; n.y:=0
      end
    else begin
      n.x:=a-c-h; n.y:=b+b
    end;

    f:=(a*n.x*n.x+2.*b*n.x*n.y+c*n.y*n.y)/MODV2(n);

    if f > eps then begin k:=i; Break end
  end;

  ORTV2(n,v0);
  SUBV2(p[k-1],p[0],v1);

  if SCAL2(v0,v1) < 0 then
  MULV2(-1,v0,v0); f:=j;

  p0.x:=p[0].x + sx/f;
  p0.y:=p[0].y + sy/f;

  Result:=k
end;

procedure maxdist(p: PGPoly; k: Integer; var p_out: tgauss);
var
  i: Integer; q: tgauss;
begin
  if k <= 0 then
    p_out:=p[0]
  else begin
    q.x:=0; q.y:=0;

    for i:=0 to k-1 do
    ADDV2(q,p[i],q);

    MULV2(1/k, q, q);
    p_out:=q
  end
end;

function cls_line(lp: PGPoly; n: Integer;
                  lq: PGPoly; eps: double): Integer;
var
  i,k: Integer;
  p1,v1, p0, pd,vd,
  v01,v12, pp: tgauss;
begin
  Result:=0;

  if n > 2 then begin
    lq[Result]:=lp[0]; Inc(Result);

    i:=0; while i < n-1 do begin

      k:=llsq(@lp[i],n-i, eps, pd, vd);
      if i+k = n-2 then Inc(k);

      maxdist(@lp[i],k, pd);
      SUBV2(lp[i+k-1],lp[i], v1);

      p0:=lp[i+k-1];
      ORTV2(v1, vd);

      lineintersect(p0, v1, pd, vd, pp);
      SUBV2(pd, pp, v01);

      if i > 0 then begin
        ADDV2(p1, v01, p1);
        ADDV2(p1, v12, p1);

        lq[Result]:=p1; Inc(Result)
      end;

      v12:=v01; p1:=p0;
      Inc(i,k - 1)
    end;

    lq[Result]:=p0; Inc(Result)
  end
end;

function Bezier_line(lp: pGPoly; np: Integer;
                     vp: pGPoly; lock: Boolean): Integer;
var
  i: Integer; c: tgauss;
begin
  Result:=0;
  if np > 2 then begin
    if not lock then begin
      vp[Result]:=lp[0]; Inc(Result)
    end;

    for i:=1 to np-1 do begin
      Middle_Gauss(lp[i-1],lp[i],c);

      if not lock then
      if i = 1 then begin
        vp[Result]:=c; Inc(Result);
      end;

      vp[Result]:=c; Inc(Result);
      vp[Result]:=lp[i]; Inc(Result);
    end;

    if lock then begin
      vp[Result]:=vp[0]; Inc(Result);
      vp[Result]:=vp[1]; Inc(Result);
    end
    else begin
      vp[Result]:=lp[np-1]; Inc(Result);
      Mirror_Gauss(c,lp[np-1],c);
      vp[Result]:=c; Inc(Result)
    end
  end
end;

function xLLineToCurve(dst_lp,src_lp: PLLine;
                       dstMax: int; eps: double): int;
var
  pp,qq,vv: PGPoly;
  i, np,nq,nv: Integer;
  lock: Boolean;
begin
  Result:=-1;

  np:=src_lp.N+1;
  if np > 3 then begin
    lock:=PolyLock(src_lp);

    pp:=xAllocPtr(np*SizeOf(tgauss));
    qq:=xAllocPtr(np*SizeOf(tgauss));
    vv:=xAllocPtr((np+np+2)*SizeOf(tgauss));

    if pp <> nil then
    if qq <> nil then
    if vv <> nil then begin

      for i:=0 to np-1 do begin
        pp[i].x:=src_lp.Pol[i].x;
        pp[i].y:=src_lp.Pol[i].y;
      end;

      nv:=0;

      if eps < 0 then
        nv:=Bezier_line(pp,np, vv,lock)
      else begin
        if eps < 0.1 then eps:=4;
        nq:=cls_line(pp,np, qq,eps * eps_smooth * eps_smooth);
        if nq > 1 then nv:=Bezier_line(qq,nq, vv,lock);
      end;

      if nv > 0 then
      if nv < dstMax then begin
        for i:=0 to nv-1 do begin
          dst_lp.Pol[i].x:=Round(vv[i].x);
          dst_lp.Pol[i].y:=Round(vv[i].y);
        end;

        dst_lp.N:=nv-1; Result:=nv-1
      end
    end;

    xFreePtr(pp);
    xFreePtr(qq);
    xFreePtr(vv);
  end;
end;

end.





