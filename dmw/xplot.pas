unit xplot; interface

uses
  Classes,LCLType,
  Math,SysUtils,
  otypes,xlist,
  xclasses;

type
  THorzLine = procedure(x1,x2,y: Integer) of object;

  TClipFrame = class(TCustomList)
    constructor Create(ACapacity: Integer);

    procedure Add_Frame(lp: PLPoly; lp_N: int);

    procedure Add_Contour(lp: PLLine);

    function Load_stripe(lp1: PLPoly; n1: int;
                         lp2: PLPoly; n2: int): int;

    function Load_stripe1(lp1,lp2: PLLine): int;

    function Get_mask(y: Integer;
                      Mask: PIntegers): Integer;

    function FrameContainsLine(const p1,p2: TPoint): Boolean;
    function ContainsPoint(pX,pY: Integer): Boolean;

    function GetCentreX(cy: int; out cx: int): Boolean;
    function GetCentre(out c: TPoint): Boolean;

    procedure Scan_mask(ATop,ABottom: Integer);

    function GetSquare(cp: PGauss): Double;

  private
    flt,frb: TPoint;
    fOnHorzLine: THorzLine;

    procedure SetFrame(lp: PLLine);

  public
    property OnHorzLine: THorzLine write fOnHorzLine;

    property Frame: PLLine write SetFrame;
    property frame_lt: TPoint read flt;
    property frame_rb: TPoint read frb;
  end;

  TThickPolyline = class
    constructor Create(AOnPoint: TIntegerProc2;
                       AOnBreak: TEventProc);

    destructor Destroy; override;

    function Polyline(lp: PLPoly; lp_n: int; delta: float): int;

  private
    fbuf: TPointList;
    fPoints: PLPoly;
    fNormals: PLPoly;

    fEdgeSharpness: float;
    froundEdges: bool;

    fOnPoint: TIntegerProc2;
    fOnBreak: TEventProc;

    ftopX,ftopY: int64;
    flastX,flastY: int64;
    fCount: int
  end;

  TThickPolyList = class(TPolyList)
    constructor Create;
    destructor Destroy; override;

    function Grow(lp: PLPoly; lp_n: int; delta: float): int;

  private
    fthick: TThickPolyline;

    procedure __AddPoint(x,y: int);
    procedure __endContour;
  end;

implementation

uses
  Convert,XLine,XPoly;

function Insert_xpos(Mask: PIntegers;
                     Count,XPos: Integer): Integer;
var
  i,j,Ind,Val: Integer;
begin
  Result:=Count; Ind:=Count;

  for i:=Count-1 downto 0 do begin
    Val:=Mask[i];

    if XPos < Val then
      Ind:=i
    else begin
      if XPos = Val then begin
        Ind:=-1; Dec(Result);
        for j:=i to Result-1 do
        Mask[j]:=Mask[j+1]
      end;

      Break
    end
  end;

  if Ind >= 0 then begin
    for i:=Count downto Ind+1 do
    Mask[i]:=Mask[i-1];

    Mask[Ind]:=XPos; Inc(Result)
  end
end;

function GetClipCentref(lp: PLLine; out c: TGauss): Boolean;
var
  clip: TClipFrame;
  i,k,yc,r,t: int; lt,rb: TPoint; p: double;
  swap: longbool; ip: PIntegers; line: TIntegers;
begin
  Result:=false; c.x:=0; c.y:=0;

  if PolyLock(lp) then begin
    Max_Poly_Bound(@lp.Pol,lp.N, lt,rb);

    yc:=Round(lt.Y/2 + rb.Y/2);
    swap:=rb.X-lt.X > rb.Y-lt.Y;

    if swap then begin
      yc:=Round(lt.X/2 + rb.X/2);

      for i:=0 to lp.N do
      with lp.Pol[i] do iSwap(X,Y);
    end;

    clip:=TClipFrame.Create(1024);
    try
      clip.Add_Contour(lp);
      k:=clip.Get_mask(yc,@line);

      r:=-1; ip:=@line;

      for i:=1 to k do begin
        t:=ip[1]-ip[0];
        if t > r then begin
          r:=t; p:=ip[0]+t/2;
        end;

        ip:=@ip[2]
      end;

      if r > 0 then begin

        if swap then begin
          c.x:=yc; c.y:=p
        end
        else begin
          c.x:=p; c.y:=yc
        end;

        Result:=true
      end

    finally
      clip.Free
    end;

    if swap then for i:=0 to lp.N do
    with lp.Pol[i] do iSwap(X,Y)
  end
end;

function GetClipCentrei(lp: PLLine; out c: TPoint): Boolean;
var
  t: TGauss;
begin
  Result:=false; c:=Point(0,0);
  if GetClipCentref(lp,t) then begin
    c.X:=Round(t.x);
    c.Y:=Round(t.y);
    Result:=true
  end
end;

type
  TLongRib = record
    a,b: TPoint; ctg: Double
  end;

  PLongRibs = ^TLongRibs;
  TLongRibs = array[0..1023] of TLongRib;

constructor TClipFrame.Create(ACapacity: Integer);
begin
  inherited Create(Sizeof(TLongRib),ACapacity)
end;

procedure TClipFrame.Add_Frame(lp: PLPoly; lp_N: int);
var
  i,j,ind,up,jy: int; rp: PLongRibs;
  p1,p2,_lt,_rb: TPoint; r: TLongRib;
begin
  if Assigned(lp) then
  if lp_N > 0 then begin
    Max_Poly_Bound(lp,lp_N+1, _lt,_rb);

    if Count > 0 then
      Add_LRect(flt,frb,_lt,_rb)
    else begin
      flt:=_lt; frb:=_rb
    end;

    p2:=lp[0]; for i:=1 to lp_N do begin
      p1:=p2; p2:=lp[i];

      if p1.Y <> p2.Y then begin

        up:=p1.Y;
        if p2.Y < up then up:=p2.Y;

        rp:=First; ind:=-1;

        for j:=Count-1 downto 0 do begin
          jy:=rp[j].a.Y;
          if jy > up then ind:=j else
          if jy <= up then Break
        end;

        if p1.Y < p2.Y then begin r.a:=p1; r.b:=p2 end
        else                begin r.a:=p2; r.b:=p1 end;

        with r do ctg:=(b.X-a.X) / (b.Y-a.Y);

        if ind < 0 then Add(@r) else
        Insert_range(@r,ind,1)
      end else
      if Assigned(fOnHorzLine) then
      fOnHorzLine(Min(p1.X,p2.X),Max(p1.X,p2.X),p1.Y)
    end
  end
end;

procedure TClipFrame.Add_Contour(lp: PLLine);
begin
  if Assigned(lp) then
  Add_Frame(@lp.Pol,lp.N)
end;

procedure TClipFrame.SetFrame(lp: PLLine);
begin
  Clear;
  flt.x:=0; flt.y:=0; frb:=flt;
  Add_Contour(lp);
end;

function TClipFrame.Load_stripe(lp1: PLPoly; n1: int;
                                lp2: PLPoly; n2: int): int;
var
  v: LVector;
begin
  Clear;
  Add_Frame(lp1,n1);
  Add_Frame(lp2,n2);

  v[0]:=lp1[0];
  v[1]:=lp2[0];
  Add_Frame(@v,1);

  v[0]:=lp1[n1];
  v[1]:=lp2[n2];
  Add_Frame(@v,1);
end;

function TClipFrame.Load_stripe1(lp1,lp2: PLLine): int;
begin
  Result:=Load_stripe(@lp1.Pol,lp1.N, @lp2.Pol,lp2.N)
end;

function TClipFrame.Get_mask(y: Integer;
                             Mask: PIntegers): Integer;
var
  i,x: int; lp: PLongRibs;
begin
  Result:=0; if Count > 0 then
  if (y >= flt.Y) and (y <= frb.Y) then begin

    lp:=First;
    for i:=0 to Count-1 do begin

      with lp[0] do
      if y < a.Y then Break else
      if y < b.Y then begin
        x:=a.X + Round((Y-a.Y) * ctg);
        Result:=Insert_xpos(Mask,Result,x);
      end;

      lp:=@lp[1]
    end;

    Result:=Result div 2
  end
end;

function TClipFrame.FrameContainsLine(const p1,p2: TPoint): Boolean;
begin
  Result:=false;
  if Min(p1.X,p2.X) <= frb.X then
  if Max(p1.X,p2.X) >= flt.X then
  if Min(p1.Y,p2.Y) <= frb.Y then
  if Max(p1.Y,p2.Y) >= flt.Y then
  Result:=true
end;

function TClipFrame.ContainsPoint(pX,pY: Integer): Boolean;
var
  i,k: int; lp: PIntegers;
  rp: PLongRibs; line: TIntegers;
begin
  Result:=false; lp:=@line;
  k:=Get_mask(pY,lp);

  if k = 0 then begin

    if Count > 0 then
    if (pY >= flt.Y) and (pY <= frb.Y) then begin

      rp:=First;
      for i:=0 to Count-1 do begin

        with rp[0] do
        if ((pX = a.X) and (pY = a.Y))
        or ((pX = b.X) and (pY = b.Y)) then
        begin Result:=true; Break end;

        rp:=@rp[1]
      end
    end;

  end else

  while k > 0 do begin
    if (pX >= lp[0]) and (pX <= lp[1]) then
    begin Result:=true; Break end;
    lp:=@lp[2]; Dec(k);
  end
end;

function TClipFrame.GetCentreX(cy: int; out cx: int): Boolean;
var
  i,k,dx,tmp: int; lp: PIntegers; line: TIntegers;
begin
  Result:=false;
  cx:=Round(flt.X/2+frb.X/2);

  lp:=@line; k:=Get_mask(cy,lp);

  if k > 0 then begin

    dx:=lp[1]-lp[0];
    cx:=lp[0] + (dx div 2);

    for i:=2 to k do begin
      lp:=@lp[2];
      tmp:=lp[1]-lp[0];
      if tmp > dx then begin
        dx:=tmp;
        cx:=lp[0] + (dx div 2);
      end
    end;

    Result:=true
  end;
end;

function TClipFrame.GetCentre(out c: TPoint): Boolean;
var
  cx,cy: int;
begin
  cy:=Round(flt.Y/2+frb.Y/2);
  Result:=GetCentreX(cy,cx);
  c:=Point(cx,cy)
end;

procedure TClipFrame.Scan_mask(ATop,ABottom: Integer);
var
  i,cx,y,y1,y2: Integer;
  si: PIntegers; xline: TIntegers;
begin
  y1:=Max(ATop,flt.Y);
  y2:=Min(ABottom,frb.Y);

  if Count > 0 then
  if Assigned(fOnHorzLine) then
  for y:=y1 to y2 do begin

    si:=@xline; cx:=Get_mask(y,si);

    for i:=1 to cx do begin
      fOnHorzLine(si[0],si[1],y); si:=@si[2];
    end;
  end
end;

function TClipFrame.GetSquare(cp: PGauss): Double;
var
  i,k, y1,k1,y2,k2, y: int;
  eax: Extended; si: PIntegers;
  xline: TIntegers;
begin
  y1:=0; k1:=0; y2:=0; k2:=0;

  eax:=0; if Count > 0 then
  for y:=flt.Y to frb.Y do begin

    si:=@xline; k:=Get_mask(y,si);

    if Assigned(cp) then
    if k <> 1 then begin
      if k1 > k2 then begin
        y2:=y1; k2:=k1
      end; k1:=0
    end
    else begin
      if k1 = 0 then y1:=y;
      Inc(k1);
    end;

    for i:=1 to k do begin
      eax:=eax + (si[1]-si[0]);
      si:=@si[2];
    end;
  end;

  if Assigned(cp) then begin
    if k1 > k2 then begin
      y2:=y1; k2:=k1
    end;

    if k2 = 0 then
      y2:=(flt.Y + frb.Y) div 2
    else
      Inc(y2,k2 div 2);

    si:=@xline; k:=Get_mask(y2,si);

    if k > 0 then begin
      cp.x:=(si[0] + si[1]) div 2;
      cp.y:=y2
    end
    else begin
      cp.x:=(flt.X + frb.X) div 2;
      cp.y:=(flt.Y + frb.Y) div 2;
    end
  end;

  Result:=eax
end;

const
 // poly_shl = 12;
  poly_pix = $1000;
  poly_pix2 = $1000 div 2;

function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer;
begin
  Result:=Int64(Multiplicand) * Int64(Multiplier) div Divisor;
end;

constructor TThickPolyline.Create(AOnPoint: TIntegerProc2;
                                  AOnBreak: TEventProc);
begin
  inherited Create;
  fOnPoint:=AOnPoint;
  fOnBreak:=AOnBreak;
  fbuf:=TPointList.Create(4096);
  fEdgeSharpness:=0;
  froundEdges:=true
end;

destructor TThickPolyline.Destroy;
begin
  fbuf.Free;
  inherited
end;

function TThickPolyline.Polyline(lp: PLPoly; lp_n: int; delta: float): int;

function BuildNormals(lp: PLPoly; lp_n: int): int;
var
  i,n,dx,dy: int; np,si,di: PPoint;
  a,b: TPoint; vx,vy,f: float;
begin
  Result:=-1;

  n:=lp_n+1;
  if fbuf.Extend(n*2) then begin
    fPoints:=fbuf.First;
    fNormals:=@fPoints[n];

    si:=Pointer(lp);
    di:=Pointer(fPoints);
    np:=Pointer(fNormals);

    b:=si^; di^:=b; Result:=0;

    for i:=1 to lp_n do begin
      a:=b; Inc(si); b:=si^;

      dx:=b.X-a.X;
      dy:=b.Y-a.Y;
      if (dx <> 0) or (dy <> 0) then begin

        vx:=dx; vy:=dy;
        f:=1 / Hypot(vx,vy);

        np.X:=+Round(vy*f*poly_pix);
        np.Y:=-Round(vx*f*poly_pix);
        Inc(np);

        Inc(di); di^:=b;
        Inc(Result)
      end;

      np.X:=0; np.Y:=0
    end
  end
end;

procedure AddPoint(x,y: int64);
begin
  if fCount = 0 then begin
    ftopX:=x; ftopY:=y;
  end;

  flastX:=x; flastY:=y;

  if true then begin
    x:=(x+poly_pix2) div poly_pix;
    y:=(y+poly_pix2) div poly_pix;
  end;

  if Assigned(fOnPoint) then
  fOnPoint(x,y);

  Inc(fCount)
end;

procedure RoundEdge(x,y: int64);
var
  i,k,vx,vy: int; cx,cy,r,f,df: double;
  sin,cos: Extended; px,py: int64;
begin
  vx:=x-flastX;
  vy:=y-flastY;
  r:=Hypot(vx,vy)/2;
  if r >= 2 then begin
    cx:=(flastX+x)/2;
    cy:=(flastY+y)/2;

    k:=Round(Pi*r/(8*poly_pix));
    if k < 2 then k:=2 else
    if k > 16 then k:=16;

    f:=ArcTan2(-vy,-vx);
    df:=Pi/k;

    for i:=1 to k-1 do begin
      SinCos(f+df*i,sin,cos);
      px:=Round(cx + cos*r);
      py:=Round(cy + sin*r);
      AddPoint(px,py);
    end
  end;

  AddPoint(x,y)
end;

var
  i,n1,n2,loop,bits: int;
  np,pp: PPoint; qn,pn: TPoint;
  Px,Py,Qx,Qy, Ax,Ay, Bx,By, Cx,Cy, Dx,Dy: int64;
  R,E, D,D1,cos: int64; closed: bool;
begin
  Result:=0; fCount:=0;

  n1:=BuildNormals(lp,lp_n);
  if n1 > 0 then begin

    D:=Round(Delta * poly_pix);
    E:=Round(D * (1 - fEdgeSharpness));

    if D > 0 then begin
      lp:=fPoints;

      closed:=false; n2:=n1;
      if int64(lp[0]) = int64(lp[n1]) then begin
        closed:=true; Dec(n2);
      end;

      pp:=Pointer(fPoints);
      np:=Pointer(fNormals);
      qn:=fNormals[n2];

      D1:=D;
      for loop:=1 to 2 do begin

        for i:=0 to n2 do begin
          Px:=pp.X * poly_pix;
          Py:=pp.Y * poly_pix;
          pn:=np^;

          if loop = 1 then begin
            Inc(pp); Inc(np)
          end
          else begin
            Dec(pp); Dec(np)
          end;

          Bx:=int64(pn.X) * D1 div poly_pix;
          By:=int64(pn.Y) * D1 div poly_pix;

          if (i = 0) and not closed then begin
            if (loop = 2) and froundEdges then
              RoundEdge(Px+Bx,Py+By)
            else
              AddPoint(Px+Bx,Py+By)
          end
          else begin
            Ax:=int64(qn.X) * D1 div poly_pix;
            Ay:=int64(qn.Y) * D1 div poly_pix;

            if i = n1 then
              AddPoint(Px+Ax,pY+Ay)
            else begin
              Cx:=Ax + Bx;
              Cy:=Ay + By;

              R:=(Ax*Cx + Ay*Cy) div D;

              if R = 0 then begin
                AddPoint(pX+Ax,pY+Ay);
                AddPoint(pX+Bx,pY+By)
              end
              else begin
                R:=Abs(R); bits:=0;

                Dx:=Bx-Ax; Dy:=By-Ay;

                cos:=qn.X*pn.Y - qn.Y*pn.X;

                if cos > 0 then
                if (R <= E)
                or (Abs(Dx)+Abs(Dy) >= D) then
                  bits:=1+2;

                if bits and 1 <> 0 then
                AddPoint(pX+Ax,pY+Ay);

                if cos >= 0 then
                R:=Round(Hypot(Cx,Cy));

                Cx:=Cx * D div R;
                Cy:=Cy * D div R;

                if i = 0 then
                  AddPoint(Px+Cx,Py+Cy)
                else begin
                  Dx:=Qx-Px; Dy:=Qy-Py;
                  if Cx*Cx+Cy*Cy < Dx*Dx+Dy*Dy then
                  AddPoint(Px+Cx,Py+Cy);
                end;

                if bits and 2 <> 0 then
                AddPoint(pX+Bx,pY+By)
              end
            end
          end;

          Qx:=Px; Qy:=Py; qn:=pn
        end;

        if closed then begin
          if fCount > 0 then
          AddPoint(ftopX,ftopY);

          if Assigned(fOnBreak) then
            fOnBreak()
          else
            Break;

          Inc(Result,fCount);
          fCount:=0;
        end
        else begin
          Dec(pp); Dec(np)
        end;

        Dec(np);
        qn:=fNormals[0];
        D1:=-D1
      end;

      if not closed then begin
        if fCount > 0 then
        if froundEdges then
          RoundEdge(ftopX,ftopY)
        else
          AddPoint(ftopX,ftopY);

        Inc(Result,fCount)
      end
    end
  end
end;

constructor TThickPolyList.Create;
begin
  inherited Create;
  fthick:=TThickPolyline.Create(__AddPoint,__endContour)
end;

destructor TThickPolyList.Destroy;
begin
  fthick.Free;
  inherited
end;

procedure TThickPolyList.__AddPoint(x,y: int);
begin
  next_point(Point(x,y),nil)
end;

procedure TThickPolyList.__endContour;
begin
  End_contour
end;

function TThickPolyList.Grow(lp: PLPoly; lp_n: int; delta: float): int;
begin
  Clear;
  fthick.Polyline(lp,lp_n,delta);
  Result:=PartCount
end;

end.



