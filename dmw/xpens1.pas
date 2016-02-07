unit xpens1; interface

uses
  Classes,Graphics,Math,
  LCLType,otypes,xlist,xclip,
  xclasses;

type
  XFigure = class
    procedure Pixel(x,y: Integer); virtual; abstract;
    procedure Lastic(x,y: Integer);
    procedure Line(x1,y1,x2,y2: Integer);
    procedure Rectangle(x1,y1,x2,y2: Integer);
    procedure Ellipse(x1,y1,x2,y2: Integer);
    procedure Triangle(x1,y1,x2,y2: Integer);
    procedure bEllipse(xc,yc,a0,b0,code: int);
    procedure bRing(xc,yc,r,code: int);
  end;

  TFigure = class(xFigure)

    dc: TCanvas; bit: TBitMap;
    zoom,xpos,ypos,width: Integer;
    draw,plot: Boolean;

    constructor Create(dst: TCanvas; src: TBitMap;
                       dz,dx,dy: Integer; cl: TColor;
                       mov: boolean);

    procedure Pixel(x,y: Integer); override;
  end;

  TFigurePoly = class(xFigure)
    constructor Create(APoly: TPointList);
    procedure Pixel(x,y: Integer); override;
  private
    fPoly: TPointList;
  end;

  TMaskMap = class(xFigure)
    function BeginDraw(const Amap: Bitmap): bool;

    procedure xLine(const a,b: TPoint);
    procedure xRib(const a,b: TPoint);
    procedure Fill;

    procedure Pixel(x,y: Integer); override;

  private
    fmap: Bitmap;
    fCount: int;
    fRect: TRect;
  public
    property Count: int read fCount write fCount;
    property Rect: TRect read fRect;
  end;

  TDrawCanvas = class(TCanvas)

    constructor Create;
    destructor Destroy;

    function begin_paint(r: Integer): Boolean;

    function ContainsPoint(const p: TPoint): Boolean;

    function ContainsPolyLine(lp: PLLine): Boolean;

    function ClipLine(const a,b: TPoint;
                      out c,d: TPoint): Boolean;

    procedure OutLine(const a,b: TPoint);

    procedure Push_font;
    procedure Pop_font;

    function InverseColor(Color: Integer): Integer;

  private
    fSaveFont: TFont;
    fClipRect: TRect;
    fClearColor: Integer;
  public
    property ClipRect: TRect read fClipRect;
    property lt: TPoint read fClipRect.TopLeft;
    property rb: TPoint read fClipRect.BottomRight;

    property ClearColor: int read fClearColor
                             write fClearColor;
  end;

  TOutPoly = class
    constructor Create(Adc: TCanvas);

    procedure MoveTo(const Ap: TPoint);
    procedure LineTo(const Ap: TPoint);

    procedure Polyline(lp: PLPoly; n: int);

    function ContainsRect(const _lt,_rb: TPoint): Boolean;
    function ContainsPoint(const P: TPoint): Boolean;

  private
    fdc: TCanvas;
    flt,frb,fq: TPoint;
    fdn: LongBool;
  public
    property dc: TCanvas read fdc;
    property lt: TPoint read flt;
    property rb: TPoint read frb;
  end;

  TDrawValues = class
    constructor Create(ADC: TCanvas);
    procedure Draw(X,Y: Integer; Str: PChar);
  private
    fDC: TCanvas;
    fRect: TRect;
  end;

  TClipCanvas = class(TClipPoly)
    procedure BeginDraw(ADC: TCanvas);

    procedure Polygon(lp: PLPoly; n: Integer);

  protected
    procedure lp_Dump(lp: PLLine); override;

  private
    fDC: TCanvas;
    flt,frb: TPoint;
  end;

implementation

uses
  Windows,LCLIntf,
  xline,xpoly,xpens,xgdi;

const
  winding = false;

procedure xFigure.Lastic(x,y: Integer);
var
  ix,iy: Integer;
begin
  for iy:=y-1 to y+1 do
  for ix:=x-1 to x+1 do
  Pixel(ix,iy)
end;

procedure xFigure.Line(x1,y1,x2,y2: Integer);
var
  x,y,dx,dy,d,inc1,inc2: Integer;
  done,_xy,x_sign,y_sign: boolean;
begin
  _xy:=true; repeat done:=true;

    dx:=x2 - x1; x_sign:=dx < 0; dx:=Abs(dx);
    dy:=y2 - y1; y_sign:=dy < 0; dy:=Abs(dy);

    if dx < dy then begin
      _xy:=false; done:=false;
      x:=x1; x1:=y1; y1:=x; x:=x2; x2:=y2; y2:=x
    end;
  until done;

  inc1:=dy shl 1; inc2:=(dy - dx) shl 1; d:=inc1 - dx;

  x:=x1; if x_sign then dx:=-1 else dx:=1;
  y:=y1; if y_sign then dy:=-1 else dy:=1;

  if _XY then Pixel(x,y) else Pixel(y,x);

  while x <> x2 do begin Inc(x,dx);

    if d < 0 then
      Inc(d,inc1)
    else begin
      Inc(d,inc2); Inc(y,dy)
    end;

    if _XY then Pixel(x,y) else Pixel(y,x)
  end
end;

procedure xFigure.Rectangle(x1,y1,x2,y2: Integer);
begin
  Line(x1,y1,x2,y1); Line(x2,y1,x2,y2);
  Line(x2,y2,x1,y2); Line(x1,y2,x1,y1)
end;

procedure xFigure.Ellipse(x1,y1,x2,y2: Integer);
var
  cx,cy, a,b, f,df: double; i: Integer;
  p,q: TPoint;
begin
  if x1 < x2 then
  if y1 < y2 then begin

    cx:=(x1+x2)/2; a:=(x2-x1)/2;
    cy:=(y1+y2)/2; b:=(y2-y1)/2;

    f:=0; df:=2*Pi / 360;

    for i:=1 to 360 do begin

      p.x:=Round(cx + a * cos(f));
      p.y:=Round(cy + b * sin(f));

      if (i = 1) or not Points_Equal(p,q) then
      Pixel(p.x,p.y); q:=p; f:=f+df
    end;

  end
end;

procedure xFigure.Triangle(x1,y1,x2,y2: Integer);
var
  dx: Integer;
begin
  dx:=(x2-x1) div 2;
  Line(x1,y2,x1+dx,y1);
  Line(x2-dx,y1,x2,y2);
  Line(x1,y2,x2,y2)
end;

procedure xFigure.bEllipse(xc,yc,a0,b0,code: int);

procedure bPixel(x1,y1,x2,y2,code: int);
begin
  if x1 <> x2 then begin
    if code and 4 > 0 then Pixel(x1,y2);
    if code and 8 > 0 then Pixel(x2,y2);
    if y1 <> y2 then begin
      if code and 2 > 0 then Pixel(x1,y1);
      if code and 1 > 0 then Pixel(x2,y1)
    end
  end
  else begin
    if code and 4 > 0 then Pixel(x1,y2);
    if y1 <> y2 then
      if code and 2 > 0 then Pixel(x1,y1)
  end
end;

var
  x,y,x1,x2,y1,y2,up_y, a,b,aa,bb,aa2,bb2,d,dx,dy: int;
begin
  x:=0; y:=b0;

  a:=a0; aa:=a * a; aa2:=aa shl 1;
  b:=b0; bb:=b * b; bb2:=bb shl 1;

  d:=bb - aa*b + aa shr 4; dx:=0; dy:=aa2 * b;

  while dx < dy do begin
    x1:=xc-x; x2:=xc+x; y1:=yc-y; y2:=yc+y;

    bPixel(x1,y1,x2,y2,code);

    if d > 0 then begin Up_y:=y;
      Dec(y); Dec(dy,aa2); Dec(d,dy)
    end;

    Inc(x); Inc(dx,bb2); Inc(d,bb+dx)
  end;

  Inc(d,(3*(aa-bb) div 2 - (dx+dy)) div 2);

  while y >= 0 do begin
    x1:=xc-x; x2:=xc+x; y1:=yc-y; y2:=yc+y;

    bPixel(x1,y1,x2,y2,code);

    if d < 0 then begin
      Inc(x); Inc(dx,bb2); Inc(d,dx)
    end;

    Dec(y); Dec(dy,aa2); Inc(d,aa-dy)
  end
end;

procedure xFigure.bRing(xc,yc,r,code: int);
var
  x,y,delta,error: int;
begin
  x:=0; y:=r;
  delta:=(2 - 2*r);
  error:=0;

  while y >= 0 do begin

    if code and 1 <> 0 then pixel(xc+x,yc+y);
    if code and 2 <> 0 then pixel(xc+x,yc-y);
    if code and 4 <> 0 then pixel(xc-x,yc+y);
    if code and 8 <> 0 then pixel(xc-x,yc-y);

    error:=2*(delta + y) - 1;
    if ((delta < 0) and (error <= 0)) then begin
      Inc(x); delta:=delta + (2 * x + 1);
    end
    else begin
      error:=2*(delta - x) - 1;
      if ((delta > 0) and (error > 0)) then begin
        Dec(y); delta:=delta + (1 - 2 * y);
      end
      else begin
        Inc(x);
        delta:=delta + (2 * (x - y));
        Dec(y);
      end
    end
  end
end;

constructor TFigure.Create(dst: TCanvas; src: tBitMap;
                           dz,dx,dy: Integer; cl: TColor; mov: boolean);
begin
  inherited Create;
  dc:=dst; bit:=src; zoom:=dz; xpos:=dx; ypos:=dy;
  xResetCanvas(dc,clBlack,cl); width:=1; draw:=mov;
  xResetCanvas(bit.Canvas,cl,cl); plot:=false
end;

procedure TFigure.Pixel(x,y: Integer);
var
  ix,iy, x1,y1,x2,y2: Integer;
begin
  ix:=x+xpos; iy:=y+ypos;

  if ix >= 0 then if ix < bit.Width then
  if iy >= 0 then if iy < bit.Height then begin

    if draw then begin
      if plot then bit.CanVas.Pixels[ix,iy]:=dc.Brush.Color
    end
    else dc.Brush.Color:=bit.CanVas.Pixels[ix,iy];

    if zoom = 1 then
      dc.Pixels[x,y]:=dc.Brush.Color
    else begin
      x1:=x*zoom; y1:=y*zoom; x2:=x1+zoom; y2:=y1+zoom;
      if zoom >= 8 then begin Inc(x1); Inc(y1) end;
      dc.FillRect(Rect(x1,y1,x2,y2));
    end
  end
end;

constructor TFigurePoly.Create(APoly: TPointList);
begin
  inherited Create; fPoly:=APoly
end;

procedure TFigurePoly.Pixel(x,y: Integer);
var
  bp: PPoint;
begin
  bp:=fPoly.Last;
  if bp = nil then
    fPoly.AddItem(x,y)
  else
  if (x <> bp.X) or (y <> bp.Y) then
    fPoly.AddItem(x,y)
end;

function TMaskMap.BeginDraw(const Amap: Bitmap): bool;
begin
  fmap:=Amap;
  Result:=Assigned(fmap.bmBits)
end;

procedure TMaskMap.xLine(const a,b: TPoint);
var
  x1,y1,x2,y2: int;
begin
  x1:=Min(a.X,b.X); x2:=Max(a.X,b.X);
  if x1 < 0 then x1:=0;
  if x2 >= fmap.bmWidth then x2:=fmap.bmWidth-1;

  y1:=Min(a.Y,b.Y); y2:=Max(a.Y,b.Y);
  if y1 < 0 then y1:=0;
  if y2 >= fmap.bmHeight then y2:=fmap.bmHeight-1;

  with fRect do
  if fCount = 0 then begin
    Left:=x1; Top:=y1; Right:=x2; Bottom:=y2
  end
  else begin
    if x1 < Left   then Left  :=x1;
    if x2 > Right  then Right :=x2;
    if y1 < Top    then Top   :=y1;
    if y2 > Bottom then Bottom:=y2;
  end;

  Line(a.X,a.Y,b.X,b.Y)
end;

procedure TMaskMap.xRib(const a,b: TPoint);
var
  ix,ctg: double; x,y,x2,y2,bx: Integer;
  bp: PBytes; di: PByte; p1,p2: TPoint;
begin
  if a.Y > b.Y then begin
    p1:=b; p2:=a
  end
  else begin
    p1:=a; p2:=b
  end;

  x2:=fmap.bmWidth-1;
  y2:=fmap.bmHeight-1;
  if p1.Y < p2.Y then
  if p1.Y <= y2 then
  if p2.Y >= 0 then begin

    bx:=fmap.bmWidthBytes;
    bp:=fmap.bmBits;

    ix:=p1.X; ctg:=(p2.X-p1.X)/(p2.Y-p1.Y);

    if p1.Y < 0 then begin
      ix:=ix - p1.Y*ctg; p1.Y:=0
    end;

    Dec(p2.Y); if p2.Y > y2 then p2.Y:=y2;

    for y:=p1.Y to p2.Y do begin
      x:=Round(ix); ix:=ix+ctg;
      if x < 0 then x:=0;
      if x > x2 then x:=x2;

      di:=@bp[y*bx+x];
      di^:=di^ xor 2
    end
  end
end;

procedure TMaskMap.Fill;
var
  x,y,x1,y1,x2,y2,bx,al: int;
  bp: PBytes; di: PByte;
begin
  x1:=fRect.Left; x2:=fRect.Right;
  y1:=fRect.Top; y2:=fRect.Bottom;

  bp:=fmap.bmBits;
  bx:=fmap.bmWidthBytes;

  for y:=y1 to y2 do begin
    di:=@bp[y*bx+x1]; al:=0;

    for x:=x1 to x2 do begin
      if di^ and 2 <> 0 then begin
        al:=al xor 1; di^:=di^ or 1
      end else di^:=di^ or al;
      Inc(di)
    end
  end
end;

procedure TMaskMap.Pixel(x,y: Integer);
var
  bx: int;
begin
  if fmap.bmBitsPixel = 8 then
  if (x >= 0) and (x < fmap.bmWidth) then
  if (y >= 0) and (y < fmap.bmHeight) then begin

    with fRect do
    if fCount = 0 then begin
      Left:=x; Top:=y; Right:=x; Bottom:=y
    end
    else begin
      if x < Left   then Left  :=x;
      if x > Right  then Right :=x;
      if y < Top    then Top   :=y;
      if y > Bottom then Bottom:=y;
    end;

    bx:=y*fmap.bmWidthBytes + x;
    PBytes(fmap.bmBits)[bx]:=1;
    Inc(fCount)
  end
end;

constructor TDrawCanvas.Create;
begin
  inherited;
  fSaveFont:=TFont.Create;
  fClearColor:=clWhite
end;

destructor TDrawCanvas.Destroy;
begin
  fSaveFont.Free; inherited
end;

function TDrawCanvas.InverseColor(Color: Integer): Integer;
var
  i,hi1,hi2,al: Integer;
begin
  if fClearColor <> clNone then begin
    hi1:=RGB_to_Gray(fClearColor);
    hi2:=RGB_to_Gray(Color);

    if Abs(hi1-hi2) < 30 then
    for i:=0 to 2 do begin
      al:=tlong(Color).b[i];
      if al > 127 then Dec(al,90)
                  else Inc(al,90);
      tlong(Color).b[i]:=al
    end
  end;

  Result:=Color;
end;

function TDrawCanvas.begin_paint(r: Integer): Boolean;
begin
  Result:=false;
  if Handle <> 0 then begin
    fClipRect:=xClipRect(Self,Max(r,8));
    Result:=true
  end
end;

function TDrawCanvas.ContainsPoint(const p: TPoint): Boolean;
begin
  with fClipRect do
  Result:=(p.X >= Left) and
          (p.X <= Right) and
          (p.Y >= Top) and
          (p.Y <= Bottom)
end;

function TDrawCanvas.ContainsPolyLine(lp: PLLine): Boolean;
var
  lt1,rb1: TPoint;
begin
  Max_Poly_Bound(@lp.Pol,lp.N+1, lt1,rb1);
  Result:=PortContainsPort(lt,rb, lt1,rb1)
end;

function TDrawCanvas.ClipLine(const a,b: TPoint;
                              out c,d: TPoint): Boolean;
begin
  Result:=xClip_Line(lt,rb, a,b,c,d)
end;

procedure TDrawCanvas.OutLine(const a,b: TPoint);
var
  a1,b1: TPoint;
begin
  if xClip_Line(lt,rb, a,b,a1,b1) then begin
    MoveTo(a1.X,a1.Y); LineTo(b1.X,b1.Y)
  end
end;

procedure TDrawCanvas.Push_font;
begin
  fSaveFont.Assign(Font);
end;

procedure TDrawCanvas.Pop_font;
begin
  Font.Assign(fSaveFont);
end;

constructor TOutPoly.Create(Adc: TCanvas);
begin
  inherited Create; fdc:=Adc;

  with xClipRect(fdc,16) do begin
    flt:=TopLeft; frb:=BottomRight;
  end;
end;

procedure TOutPoly.MoveTo(const Ap: TPoint);
begin
  fq:=Ap; fdn:=false
end;

procedure TOutPoly.LineTo(const Ap: TPoint);
var
  q,p, a,b: TPoint;
begin
  q:=fq; p:=Ap;

  if not Points_Equal(q,p) then

  if PortContainsPoint(flt,frb, p.X,p.Y) then begin

    if fdn then
      fdc.LineTo(p.X,p.Y)
    else
    if xClip_Line(flt,frb, q,p, a,b) then begin
      fdc.MoveTo(a.X,a.Y);
      fdc.LineTo(b.X,b.Y);
      fdn:=true
    end

  end
  else begin
    if xClip_Line(flt,frb, q,p, a,b) then begin
      fdc.MoveTo(a.X,a.Y);
      fdc.LineTo(b.X,b.Y);
    end;

    fdn:=false
  end;

  fq:=p
end;

procedure TOutPoly.Polyline(lp: PLPoly; n: int);
var
  i: int;
begin
  if n > 0 then begin
    MoveTo(lp[0]);
    for i:=1 to n do
    LineTo(lp[i])
  end
end;

function TOutPoly.ContainsRect(const _lt,_rb: TPoint): Boolean;
begin
  Result:=
  (flt.X <= _rb.X) and (_lt.X <= frb.X) and
  (flt.Y <= _rb.Y) and (_lt.Y <= frb.Y)
end;

function TOutPoly.ContainsPoint(const P: TPoint): Boolean;
begin
  Result:=
  (P.X >= flt.X) and (P.X <= frb.X) and
  (P.Y >= flt.Y) and (p.Y <= frb.Y)
end;

constructor TDrawValues.Create(ADC: TCanvas);
begin
  inherited Create; fDC:=ADC;
  fRect.Right:=-1;
end;

procedure TDrawValues.Draw(X,Y: Integer; Str: PChar);
var
  r: TRect; sz: TSize;
begin
  sz:=fDC.TextExtent(Str);
  r:=Rect(X,Y,X+sz.cx,Y+sz.cy);

  if (fRect.Right < fRect.Left)
  or (r.Left > fRect.Right)
  or (r.Right < fRect.Left)
  or (r.Top > fRect.Bottom)
  or (r.Bottom < fRect.Top) then begin
    fDC.TextOut(X,Y,Str); with r do
    fRect:=Rect(Left-16,Top-16,Right+16,Bottom+16)
  end
end;

procedure TClipCanvas.BeginDraw(ADC: TCanvas);
begin
  fDC:=ADC; if Assigned(fDC) then
  with xClipRect(fDC,8) do begin
    flt:=TopLeft; frb:=BottomRight;
  end
end;

procedure TClipCanvas.Polygon(lp: PLPoly; N: Integer);
begin
  Draw_LPoly(lp,n, flt,frb,true)
end;

procedure TClipCanvas.lp_Dump(lp: PLLine);
begin
  if Assigned(fDC) then
  LCLIntf.Polygon(fDC.Handle,lp.Pol,lp.N+1,winding)
end;

end.
