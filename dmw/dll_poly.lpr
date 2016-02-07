library dll_poly;

uses
  Classes,OTypes,XLine;

function PolyLength(lp: PLPoly; n: Integer): double; stdcall;
var
  i: int; eax: Extended;
begin
  eax:=0; for i:=1 to n do begin
    eax:=eax + Long_Dist(lp[0],lp[1]); lp:=@lp[1]
  end; Result:=eax
end;

procedure Dump_Poly(lp: PLLine; fn: PChar); stdcall;
var
  t: text; i: Integer;
begin
  Assign(t,StrPas(fn)); Rewrite(t);
  with lp^ do for i:=0 to N do
  with Pol[i] do writeln(t,x:8,' ',y:8);
  System.Close(t)
end;

procedure Swap_Poly(lp: PLLine); stdcall;
var
  i1,i2: Integer; p: TPoint;
begin
  if lp <> nil then with lp^ do begin
    i1:=0; i2:=N; while i1 < i2 do begin
      p:=Pol[i1]; Pol[i1]:=Pol[i2]; Pol[i2]:=p;
      Inc(i1); Dec(i2)
    end
  end
end;

procedure Swap_Curve(lp: PLLine); stdcall;
var
  i: Integer; p1,p2: TPoint;
begin
  if lp <> nil then with lp^ do begin
    i:=0; while i < N do begin
      p1:=Pol[i]; p2:=Pol[i+1];
      Mirror_Point(p2,p1,p2); Pol[i]:=p2;
      Inc(i); Pol[i]:=p1; Inc(i)
    end; Swap_Poly(lp)
  end
end;

function Square_Poly(lp: PLPoly; N: Integer): double; stdcall;
var
  i: Integer; x1,y1,x2,y2: double;
begin
  Result:=0; if N > 0 then begin

    x2:=lp[0].x; y2:=lp[0].y;

    for i:=1 to N do begin
      x1:=x2; y1:=y2; x2:=lp[i].x; y2:=lp[i].y;
      Result:=Result + x1*y2 - x2*y1
    end
  end; Result:=Result/2
end;

function Centre_Poly(lp: PLLine; var cx,cy: double): double; stdcall;
var
  pp: PLPoly;
  i: Integer; ax,ay,s: Extended;
  p1,p2,v1,v2: TGauss; si,x0,y0: Double;
begin
  Result:=0; cx:=0; cy:=0;

  if lp.N >= 3 then begin pp:=@lp.Pol;

    x0:=pp[0].X; y0:=pp[0].Y;
    p2.x:=pp[1].X; p2.y:=pp[1].Y;
    v2.x:=p2.x - x0; v2.y:=p2.y - y0;

    ax:=0; ay:=0; s:=0; pp:=@pp[2];

    for i:=2 to lp.N-1 do begin
      p1:=p2; v1:=v2;
      p2.x:=pp[0].X; p2.y:=pp[0].Y;
      v2.x:=p2.x - x0; v2.y:=p2.y - y0;
      si:=v1.x*v2.y - v2.x*v1.y; s:=s + si;
      ax:=ax + si*(x0 + p1.x + p2.x)/6;
      ay:=ay + si*(y0 + p1.y + p2.y)/6;
      pp:=@pp[1]
    end;

    s:=s/2; cx:=x0; cy:=y0;

    if Abs(s) > Small then begin
      cx:=ax/s; cy:=ay/s;
    end;

    Result:=s
  end
end;

procedure Link_Poly(lp: PLLine; x,y: longint); stdcall;
var
  i: Integer; p: TPoint;
begin
  p.X:=x; p.Y:=y; with lp^ do begin i:=0;
    if Long_Dist(Pol[N],p) < Long_Dist(Pol[0],p) then i:=N;
    Pol[i]:=p
  end
end;

procedure Move_Poly(lp: PLLine; dx,dy: longint); stdcall;
var
  i: Integer;
begin
  with lp^ do for i:=0 to N do with Pol[i] do
  begin Inc(x,dx); Inc(y,dy) end
end;

procedure Get_Poly_Min_Max(lp: PLLine; var min,max: double); stdcall;
var
  i: Integer; p1,p2: TPoint;
  tmin,tmax,d: double;
begin
  tmin:=1; tmax:=0; with lp^ do if N > 0 then begin
    p1:=Pol[0]; p2:=Pol[1]; tmin:=Long_Dist(p1,p2);

    tmax:=tmin; for i:=2 to N do begin
      p1:=p2; p2:=Pol[i]; d:=Long_Dist(p1,p2);
      if d < tmin then tmin:=d; if d > tmax then tmax:=d;
    end
  end; min:=tmin; max:=tmax
end;

function Get_Poly_Max_Rib(lp: PLLine): Integer; stdcall;
var
  i: int; l,t: double;
begin
  with lp^ do begin
    Result:=0; l:=Long_Dist(Pol[0],Pol[1]);

    for i:=1 to N-1 do begin
      t:=Long_Dist(Pol[i],Pol[i+1]);
      if t > l then begin l:=t; Result:=i end
    end
  end
end;

procedure Max_Poly_Bound(lp: PLPoly; n: Integer; var lt,rb: TPoint); stdcall;
var
  i: int; a,b,p: TPoint;
begin
  a:=lp[0]; b:=lp[0];
  for i:=1 to n-1 do begin p:=lp[i];
    if p.X < a.X then a.x:=p.x;
    if p.X > b.X then b.X:=p.X;
    if p.Y < a.Y then a.Y:=p.Y;
    if p.Y > b.Y then b.Y:=p.Y
  end; lt:=a; rb:=b
end;

exports
  PolyLength,
  Dump_Poly,
  Swap_Poly,
  Swap_Curve,
  Square_Poly,
  Centre_Poly,
  Link_Poly,
  Move_Poly,

  Get_Poly_Min_Max,
  Get_Poly_Max_Rib,

  Max_Poly_Bound;

begin
end.

