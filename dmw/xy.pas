unit xy; interface

uses
  otypes;

procedure Max_Gauss_Bound(G: PGPoly; N: Integer;
                          out lt,rb: tgauss);

function Bound_to_GOrient(const lt,rb: tgauss; G: PGPoly): int;

function Frame_to_GOrient(x,y,w,h: Double; G: PGPoly): int;
function Rect_to_GOrient(x1,y1,x2,y2: Double; G: PGPoly): int;

function xy_bound_eps(bp: PGPoly; bn: int; const p: TGauss): double;

function xLocate_Line(const a,b, lt,rb: TGauss; out c: TGauss): Boolean;

procedure gauss_Swap(G: PGPoly; N: Integer);
procedure gauss_xy_swap(G: PGPoly; n: Integer);

function xy_gauss_Length(G: PGPoly; N: int): double;
function gauss_Square(G: PGPoly; N: Integer): double;

function gauss_Polygon(G: PGPoly; n: Integer;
                       out c: TGauss): double;

function gauss_Centre(G: PGPoly; n: Integer): TGauss;
function gauss_xCentre(G: PGPoly; n: Integer): TGauss;

function gauss_Indexof(g: PGPoly; n1: int;
                       const p: TGauss;
                       eps: Double): int;

function FrameContainsFrame(fr1,fr2: PGPoly): Boolean;
function FrameInsideFrame(fr1,fr2: PGPoly): Boolean;
procedure FrameAddFrame(fr1,fr2: PGPoly);

function eps_xz(xz: PGPoly; n: int; eps: double): int;

implementation

uses
  Math,XLine;

procedure Max_Gauss_Bound(G: PGPoly; N: Integer;
                          out lt,rb: TGauss);
var
  i: Integer; x1,y1,x2,y2: Double;
begin
  x1:=G[0].x; y1:=G[0].y; x2:=x1; y2:=y1;

  for i:=1 to N-1 do with G[i] do begin
    if x < x1 then x1:=x; if x > x2 then x2:=x;
    if y < y1 then y1:=y; if y > y2 then y2:=y;
  end;

  lt.x:=x1; lt.y:=y1; rb.x:=x2; rb.y:=y2;
end;

function Bound_to_GOrient(const lt,rb: TGauss; G: PGPoly): int;
begin
  G[0].x:=lt.x; G[0].y:=lt.y;
  G[1].x:=rb.x; G[1].y:=lt.y;
  G[2].x:=rb.x; G[2].y:=rb.y;
  G[3].x:=lt.x; G[3].y:=rb.y;
  G[4].x:=lt.x; G[4].y:=lt.y;
  Result:=4
end;

function Frame_to_GOrient(x,y,w,h: Double; G: PGPoly): int;
var
  lt,rb: TGauss;
begin
  lt.x:=x; lt.y:=y; rb.x:=x+w; rb.y:=y+h;
  Result:=Bound_to_GOrient(lt,rb, G)
end;

function Rect_to_GOrient(x1,y1,x2,y2: Double; G: PGPoly): int;
begin
  G[0].x:=x1; G[0].y:=y1;
  G[1].x:=x2; G[1].y:=y1;
  G[2].x:=x2; G[2].y:=y2;
  G[3].x:=x1; G[3].y:=y2;
  G[4].x:=x1; G[4].y:=y1;
  Result:=4
end;

function xy_bound_eps(bp: PGPoly; bn: int; const p: TGauss): double;
var
  i: int; t: double;
begin
  Result:=Gauss_Dist(p,bp[0]);
  for i:=1 to bn-1 do begin
    t:=Gauss_Dist(p,bp[i]);
    Result:=Min(Result,t)
  end
end;

function xLocate_Line(const a,b, lt,rb: TGauss; out c: TGauss): Boolean;
var
  p1,p2,d, t1,t2,t: TGauss;
  kx,ky, r: double;
begin
  Result:=false; Middle_Gauss(lt,rb,c);

  p1.x:=Min(a.x,b.x); p1.y:=Min(a.y,b.y);
  p2.x:=Max(a.x,b.x); p2.y:=Max(a.y,b.y);

  if (p1.x < rb.x) and (p2.x > lt.x) then
  if (p1.y < rb.y) and (p2.y > lt.y) then begin

    r:=Max(1,(rb.x-lt.x) / 2);

    d.x:=b.x-a.x; d.y:=b.y-a.y;

    if d.x = 0 then begin
      if (lt.y >= p1.y-r) and (rb.y <= p2.y+r) then
      begin c.x:=a.x; Result:=true end
    end else
    if d.y = 0 then begin
      if (lt.x >= p1.x-r) and (rb.x <= p2.x+r) then
      begin c.y:=a.y; Result:=true end
    end
    else begin
      kx:=d.x/d.y; ky:=d.y/d.x;

      t1.x:=lt.x; t1.y:=a.y+(lt.x-a.x)*ky;
      t2.x:=rb.x; t2.y:=a.y+(rb.x-a.x)*ky;

      if t1.y > t2.y then begin
        t:=t1; t1:=t2; t2:=t
      end;

      if t1.y < rb.y then
      if t2.y > lt.y then begin

        if t1.y < lt.y then begin
          t1.x:=a.x+(lt.y-a.y)*kx; t1.y:=lt.y
        end;
        if t2.y > rb.y then begin
          t2.x:=a.x+(rb.y-a.y)*kx; t2.y:=rb.y
        end;

        c.x:=t1.x/2+t2.x/2;
        c.y:=t1.y/2+t2.y/2;

        Result:=true
      end
    end
  end
end;

procedure gauss_Swap(G: PGPoly; N: Integer);
var
  i1,i2: int; si,di: PGauss; v: TGauss;
begin
  i1:=0; i2:=N-1;

  si:=@G[0]; di:=@G[i2];

  while i1 < i2 do begin
    v:=si^; si^:=di^; di^:=v;
    Inc(si); Inc(i1);
    Dec(di); Dec(i2);
  end
end;

function xy_gauss_Length(G: PGPoly; N: int): double;
var
  i: int; p1,p2: TGauss; s: Extended;
begin
  p2:=G[0]; s:=0;
  for i:=1 to n-1 do begin
    p1:=p2; G:=@G[1]; p2:=G[0];
    s:=s + Hypot(p2.x-p1.x,p2.y-p1.y)
  end;

  Result:=s
end;

function gauss_Square(G: PGPoly; n: Integer): double;
var
  i: Integer; p1,p2: TGauss; s: Extended;
begin
  p2:=G[0]; s:=0;
  for i:=1 to n-1 do begin
    p1:=p2; G:=@G[1]; p2:=G[0];
    s:=s + p1.x*p2.y - p2.x*p1.y
  end;

  Result:=s/2
end;

procedure gauss_xy_swap(G: PGPoly; n: Integer);
var
  i: Integer;
begin
  for i:=1 to n do begin
    with G[0] do xSwap(x,y); G:=@G[1]
  end
end;

function gauss_Polygon(G: PGPoly; n: Integer;
                       out c: TGauss): double;
var
  i: Integer; x1,y1,x2,y2: Double; ax,ay: Extended;
begin
  Result:=gauss_Square(G,n); c:=G[0];

  if Abs(Result) > Small then begin

    ax:=0; ay:=0; x2:=G[0].x; y2:=G[0].y;

    for i:=1 to n-1 do begin
      x1:=x2; y1:=y2; G:=@G[1];
      x2:=G[0].x; y2:=G[0].y;

      ax:=ax + (y2-y1)*(x1*x1+x1*x2+x2*x2)/6;
      ay:=ay + (x2-x1)*(y1*y1+y1*y2+y2*y2)/6;
    end;

    c.x:=ax/Result; c.y:=-ay/Result
  end;

  Result:=Abs(Result)
end;

function gauss_Centre(G: PGPoly; n: Integer): TGauss;
var
  i: Integer; cx,cy: Extended;
begin
  cx:=0; cy:=0;

  for i:=1 to n do begin
    cx:=cx + G[0].x;
    cy:=cy + G[0].y;
    G:=@G[1]
  end;

  Result.x:=cx / Max(1,n);
  Result.y:=cy / Max(1,n);
end;

function gauss_xCentre(g: PGPoly; n: Integer): TGauss;
var
  t: PGPoly; i,j: Integer; a,b: tgauss;
begin
  Result.x:=0; Result.y:=0;

  if n <= 2 then
    Result:=gauss_Centre(g,n)
  else begin

    t:=xAllocPtr(n * SizeOf(tgauss));

    if t = nil then Result:=gauss_Centre(g,n)

    else begin
      for i:=0 to n-1 do t[i]:=g[i];

      while n > 1 do begin

        i:=0; j:=0;

        while i < n do begin
          a:=t[i]; Inc(i); b:=a;
          if i < n then b:=t[i]; Inc(i);
          t[j].x:=a.x/2+b.x/2;
          t[j].y:=a.y/2+b.y/2;
          Inc(j)
        end;

        n:=(n+1) div 2
      end;

      Result:=t[0]
    end;

    xFreePtr(t)
  end
end;

function gauss_Indexof(g: PGPoly; n1: int;
                       const p: TGauss;
                       eps: Double): int;
var
  i,j: int; t,r: Double;
begin
  Result:=-1;

  j:=0; r:=Gauss_dist(p,g[0]);

  for i:=1 to n1 do begin
    t:=Gauss_dist(p,g[i]);
    if t < r then begin
      r:=t; j:=i
    end
  end;

  if r < eps then Result:=j
end;

function FrameContainsFrame(fr1,fr2: PGPoly): Boolean;
begin
  Result:=
  (fr1[0].x <= fr2[1].x) and (fr2[0].x <= fr1[1].x) and
  (fr1[0].y <= fr2[1].y) and (fr2[0].y <= fr1[1].y)
end;

function FrameInsideFrame(fr1,fr2: PGPoly): Boolean;
begin
  Result:=
  (fr1[0].x >= fr2[0].x) and (fr1[1].x <= fr2[1].x) and
  (fr1[0].y >= fr2[0].y) and (fr1[1].y <= fr2[1].y)
end;

procedure FrameAddFrame(fr1,fr2: PGPoly);
begin
  fr1[0].x:=Min(fr1[0].x,fr2[0].x);
  fr1[0].y:=Min(fr1[0].y,fr2[0].y);
  fr1[1].x:=Max(fr1[1].x,fr2[1].x);
  fr1[1].y:=Max(fr1[1].y,fr2[1].y);
end;

type
  teps_xz = class
    function Execute(xz: PGPoly; n: int;
                     eps: double): int;
  private
    fxz: PGPoly; fsp: int;
    feps: double;
  end;

function teps_xz.Execute(xz: PGPoly; n: int;
                         eps: double): int;

procedure part(xz: PGPoly; n: int);
var
  i,j: int; r,t,len,zk, z: double; a,b,p: TGauss;
begin
  if n > 0 then begin j:=-1;

    if n > 1 then begin
      a:=xz[0]; b:=xz[n]; j:=1; r:=-1;

      len:=b.x-a.x;
      if len > 0 then begin
        zk:=(b.y-a.y)/len;
        for i:=1 to n-1 do begin

          p:=xz[i];
          z:=a.y + (p.x-a.x)*zk;

          t:=Abs(p.y-z);
          if (r < 0) or (t > r) then begin
            j:=i; r:=t
          end
        end;
      end;

      if r <= eps then j:=-1
    end;

    if j <= 0 then begin
      Inc(fsp); fxz[fsp]:=xz[n];
    end
    else begin
      part(xz,j);
      part(@xz[j],n-j)
    end
  end
end;

begin
  fxz:=xz; fsp:=0; feps:=eps;
  part(xz,n); Result:=fsp
end;

function eps_xz(xz: PGPoly; n: int; eps: double): int;
var
  cls: teps_xz;
begin
  Result:=n;

  cls:=teps_xz.Create;
  try
    Result:=cls.Execute(xz,n,eps)
  finally
    cls.Free
  end
end;

end.