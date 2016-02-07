unit xdib; interface {$H-}

uses
  Classes,LCLType,
  Math,SysUtils,
  otypes,idib,
  xmask,xplot,
  img_x;

type
  TOnBeginPaint = procedure(Sender: TObject; PaintR: PRect) of object;
  TOnEndPaint = procedure(Sender: TObject) of object;

  XCanvas = class
    constructor Create;
    destructor Destroy; override;

    procedure SetDrawText(const ADrawText: IDrawText);

    function GetMap(out map: Bitmap): Boolean;
    function GetDC: HDC;

    function Contains_sign(const P: TPoint): Boolean;

    function Contains_point(x,y, dx,dy: Integer): Boolean;
    function Contains_rect(x1,y1,x2,y2, dx,dy: Integer): Boolean;
    function Contains_port(const p: PPoly; dx,dy: Integer): Boolean;

    function Contains_polygon(lp: PLLine; dr: Integer): Boolean;

    function Clip_Figure(const lt,rb: TPoint): Boolean;
    function Clip_Polyline(lp: PLPoly; N: Integer): Boolean;

    procedure Set_Xor_Mode;
    procedure Set_Copy_Mode;

    procedure SetAlfaCenter(a,b: PPoint; alfa2: int);

    function BeginPaint(PaintR: PRect): Boolean;
    procedure EndPaint;

    procedure ClipPolyPolygon(lp: PPoly; cp: PIntegers; N: integer);
    procedure ClipPaintRect;

    procedure xPen(w,cl: Integer); virtual;
    procedure extPen(w,cl: Integer); virtual;
    procedure clPen(w,cl: Integer); virtual;
    procedure rgb_Pen(w,cl: Integer); virtual;
    procedure rgb_xPen(w,st,cl: Integer); virtual;

    procedure xBrush(st,cl: Integer); virtual;
    procedure clBrush(st,cl: Integer); virtual;
    procedure rgb_Brush(cl: Integer); virtual;

    procedure MoveTo(x,y: Integer); virtual;
    procedure LineTo(x,y: Integer); virtual;

    procedure FillRect(const R: PRect); virtual;
    procedure Rectangle(x1,y1,x2,y2: Integer); virtual;
    procedure Ellipse(x1,y1,x2,y2: Integer); virtual;

    procedure Romb(x,y,r: Integer);
    procedure RectPoint(x,y,r: Integer);
    procedure CrossPoint(x,y,r: Integer);
    procedure MarkPoint(x,y,r,cl: int);
    procedure DrawPoint(x,y,r: Integer);
   procedure Arrow(const a,b: TPoint; R: Integer);
    procedure Punkt(x,y,dx, up,dn,ic: int);

    procedure vPolyLine(lp: PPoly; N,Rx,Ry: int);

    procedure PolyLine(lp: PPoly; N: Integer); virtual;
    procedure Polygon(lp: PPoly; N: Integer); virtual;
    procedure Line(x1,y1,x2,y2: Integer); virtual;
    procedure SetPixel(x,y,cl: Integer); virtual;

    procedure SetTextColor(fc: ColorRef); virtual;
    procedure TextOut(x,y: int; Str: PWideChar); virtual;
    function TextExtent(Str: PWideChar): TPoint; virtual;

    procedure TextOuts(x,y: int; const Str: WideString);
    function TextExtents(const Str: WideString): TPoint;

    procedure TextOutc(x,y,ic: int; Str: PChar);
    procedure TextOutcs(x,y,ic: int; const Str: String);

    procedure BeginPath;
    procedure ClosePath;

    procedure Frame(x1,y1,x2,y2: Integer);

    procedure draw_Line(const a,b: TPoint);
    procedure dmw_Line(const a,b: TPoint; cl: Integer);

    procedure PolyPolyLine(lp: PPoly; cp: PIntegers; N: int);
    procedure PolyPolygon(lp: PPoly; cp: PIntegers; N: int);

    procedure dmwPolygon(lp: PPoly; cp: PIntegers; N, cl: int);
    procedure alfPolygon(lp: PPoly; cp: PIntegers; N, pc,pw,fc,alfa: int);
    procedure ivgPolygon(lp: PPoly; cp: PIntegers; N,pc,fc: int);

    procedure outPolygon(lp: PPoly; N: Integer);

    procedure PolyStroke(lp: PPoly;
                         cp: PIntegers; N: int;
                         Step: float; Len: int);

    function Get_Scale(sc: double): double;

    function MetersPerPixel: double;

    procedure SetBkMode_TRANSPARENT; virtual;
    function lp_Clip: Boolean; virtual;

    procedure xRepeat(Source: XCanvas);

    procedure FillCanvas(fc: int);

    procedure ResetCanvas(fc,pc: int);
    procedure rgb_ResetCanvas(fc,pc: int);

    procedure Move_Mask_X(dX: Integer);
    procedure Move_Mask_Y(dY: Integer);

  protected
    fCanvas: ICanvas;
    fDrawText: IDrawText;

    fGround: Integer;

    function GetClipRect: TRect; virtual;

    function GetClearColor: Integer;

  private
    fmaskX: int;
    fmaskY: int;
    fmaskLoc: int;

    fBitMask: TBitMask;
    fPaintRect: TRect;
    fmm_k: Double;

    fStroke: TClipFrame;

    fAlfa2: int;
    fAlfaK: float;
    fCenter: TPoint;

    fppmm: double;

    fOnBeginPaint: TOnBeginPaint;
    fOnEndPaint: TOnEndPaint;

    x_Fill: bool;
    x_Thin: bool;
    x_Clip: bool;
    x_Poly: bool;

    function GetWidth: int;
    function GetHeight: int;

    function Get_clip_lt: TPoint;
    function Get_clip_rb: TPoint;

  public
    property Canvas: ICanvas read fCanvas;

    property Width: int read GetWidth;
    property Height: int read GetHeight;
    property ClipRect: TRect read GetClipRect;

    property Fill: bool read x_Fill write x_Fill;
    property Thin: bool read x_Thin write x_Thin;
    property Clip: bool read x_Clip write x_Clip;
    property Poly: bool write x_Poly;

    property Ground: Integer read fGround
                             write fGround;

    property ClearColor: Integer read GetClearColor;

    property BitMask: TBitMask read fBitMask;

    property maskX: int read fmaskX write fmaskX;
    property maskY: int read fmaskY write fmaskY;
    property maskLoc: int read fmaskLoc;

    property mm_k: Double read Fmm_k write Fmm_k;

    property ppmm: double read fppmm;

    property clip_lt: TPoint read Get_clip_lt;
    property clip_rb: TPoint read Get_clip_rb;

    property TextColor: ColorRef write SetTextColor;

    property OnBeginPaint: TOnBeginPaint write FOnBeginPaint;
    property OnEndPaint: TOnEndPaint write FOnEndPaint;
  end;

  TDIB = class;

  TOnPaint = procedure(dib: TDIB; PaintR,ClipR: PRect) of object;

  TDIB = class(XCanvas)
    constructor Create;

    procedure SetWindow(R: PRect);
    procedure MaxWindow;

    procedure DrawWindow(x1,y1,x2,y2: Integer);

    function Alloc_dc(w,h,bits: int): bool; virtual; abstract;
    procedure Free_dc; virtual; abstract;

    function Realloc_dc(w,h,bits: int): bool;
    procedure Clear_dc(free: bool);

    function Alloc(w,h,bits: int): bool;

    function Alloc_rgb(w,h: Integer): Boolean;
    function Realloc_rgb(w,h: Integer): Boolean;

    function xDraw(xPos,xRange,xMax: Integer;
                   yPos,yRange,yMax: Integer;
                   OnPaint: TOnPaint): Boolean;

    procedure Backup(ix,iy: Integer; out ox,oy: Integer);

    procedure xMove(xPos,yPos: Integer);
    function xCopy(dst: XCanvas): Boolean;
    procedure xCard(dc: XCanvas);

    procedure XPaint(R: PRect; dX,dY,k: Float; imPath: PChar);

    procedure XGray; virtual;

    procedure xStereo(Photo_a,Photo_b: TDib; Top: int);
    procedure xPlastic(Plastic: TDib);

    function BackupPixel(X,Y: int; out cl: int): bool;

  protected
    fMinX,fMinY,fMaxX,fMaxY: int;
    f_lt,f_rb: TPoint;

    fPix_src: Float;
    fPix_dst: Integer;

    function GetAllocated: bool; virtual;

    function GetClipRect: TRect; override;

    procedure xClipPaint(PaintR,ClipR: PRect;
                         OnPaint: TOnPaint); virtual;

  private
    v_max: TPoint;
    fPosX,fPosY: int;

    fAnimate: PColors;

    fImaging: bool;
    fIsMixed: bool;
    fIsGray: bool;
    fIsContrast: bool;
    fIsRoad: bool;

    function GetActive: bool;
    function GetEmpty: bool;
    function GetScale: Float;

  public
    property Active: bool read GetActive;
    property Allocated: bool read GetAllocated;

    property Animate: PColors read fAnimate
                              write fAnimate;

    property Imaging: bool read fImaging
                              write fImaging;

    property Empty: bool read GetEmpty;

    property v_lt: TPoint read f_lt;

    property Pix_src: Float write fPix_src;
    property Pix_dst: Integer write fPix_dst;

    property Scale: Float read GetScale;

    property IsGray: bool read fIsGray write fIsGray;
    property IsContrast: bool write fIsContrast;
    property IsRoad: bool read fIsRoad write fIsRoad;
  end;

const
  line_mm: array[0..63] of float =
  (0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50, {0..9}
   0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1.00, {10..19}

   1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,           {20..29}
   2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3.0,           {30..39}

   3.2,3.4,3.6,3.8,4.0,4.2,4.4,4.6,4.8,5.0,           {40..49}
   5.2,5.4,5.6,5.8,6.0,6.2,6.4,6.6,6.8,7.0,           {50..59}

   7.2,7.4,7.6,7.8);                                  {60..63}

function line_mm_of(mm: float): int;

function ivg_draw_hg(xdc: XCanvas;
                     xc,yc,typ,color: int;
                     iv,ic: PIntegers; ik: int;
                     r1: int): Boolean;

procedure ivg_draw_Sign(DC: XCanvas;
                        const a,b: TPoint;
                        p1,p2: Integer);

implementation

uses
  Windows,Graphics,
  convert,ofiles,
  xdc,xddw,xgdi,ivg,
  xline,xpoly,xpens,
  ipens,ximages;

function line_mm_of(mm: float): int;
var
  i: int;
begin
  Result:=0;
  for i:=0 to 63 do begin
    if mm < line_mm[i] then Break;
    Result:=i
  end
end;

function AlignMask(X,dX,Loc: Integer): Integer;
begin
  Result:=dX; if Result <> 0 then begin
    Inc(Result,X); while Result < 0 do Inc(Result,Loc);
    while Result >= Loc do Dec(Result,Loc)
  end
end;

constructor XCanvas.Create;
begin
  inherited Create; fGround:=15;
  x_Fill:=true; fmaskLoc:=8;
  Fmm_k:=1/(xMetersPerPixel(0)*1000);
  fBitMask:=TBitMask.Create(false);
  fStroke:=TClipFrame.Create(4096);
  fAlfa2:=-1
end;

destructor XCanvas.Destroy;
begin
  fCanvas:=nil;
  fDrawText:=nil;
  fStroke.Free;
  fBitMask.Free;
  inherited
end;

procedure XCanvas.SetDrawText(const ADrawText: IDrawText);
begin
  fDrawText:=ADrawText;
  if Assigned(fCanvas) then
  fCanvas.SetDrawText(ADrawText);
end;

function XCanvas.GetWidth: int;
begin
  Result:=fCanvas.GetWidth;
end;

function XCanvas.GetHeight: int;
begin
  Result:=fCanvas.GetHeight;
end;

function XCanvas.GetDC: HDC;
begin
  Result:=Canvas.GetDC;
end;

function XCanvas.GetMap(out map: Bitmap): Boolean;
begin
  Result:=fCanvas.GetMap(map)
end;

function XCanvas.Get_clip_lt: TPoint;
begin
  Result.x:=-64;
  Result.y:=-64;
end;

function XCanvas.Get_clip_rb: TPoint;
begin
  Result.x:=Width+64;
  Result.y:=Height+64
end;

function XCanvas.GetClipRect: TRect;
begin
  fCanvas.GetClipRect(Result);
end;

function XCanvas.Contains_sign(const P: TPoint): Boolean;
begin
  Result:=false;
  if (P.X >= -16) and (P.X < Width+16) then
  if (P.Y >= -16) and (P.Y < Height+16) then
  Result:=true
end;

function XCanvas.Contains_point(x,y,dx,dy: Integer): Boolean;
begin
  Result:=false;
  if (x >= -dx) and (x < Width+dx) then
  if (y >= -dy) and (y < Height+dy) then
  Result:=true
end;

function XCanvas.Contains_rect(x1,y1,x2,y2, dx,dy: Integer): Boolean;
begin
  Result:=false;
  if (x2 >= -dx) and (x1 < Width+dx) then
  if (y2 >= -dy) and (y1 < Height+dy) then
  Result:=true
end;

function XCanvas.Contains_port(const p: PPoly; dx,dy: Integer): Boolean;
begin
  Result:=false;
  if (p[1].X >= -dx) and (p[0].X < Width+dx) then
  if (p[1].Y >= -dy) and (p[0].Y < Height+dy) then
  Result:=true
end;

function XCanvas.Contains_polygon(lp: PLLine; dr: Integer): Boolean;
var
  i,w,h: Integer;
begin
  Result:=true; w:=Width+dr; h:=Height+dr;
  for i:=0 to lp.N do with lp.Pol[i] do
  if (X < dr) or (X > w) or (Y < dr) or (Y > h) then
  begin Result:=false; Break end
end;

function XCanvas.Clip_Figure(const lt,rb: TPoint): Boolean;
begin
  Result:=(lt.x < -256) or (rb.x > Width+256) or
          (lt.y < -256) or (rb.y > Height+256)
end;

function XCanvas.Clip_Polyline(lp: PLPoly; N: Integer): Boolean;
var
  lt,rb: TPoint;
begin
  Max_Poly_Bound(lp,N+1, lt,rb);
  Result:=Clip_Figure(lt,rb)
end;

function XCanvas.GetClearColor: Integer;
begin
  if tlong(fGround).b[3]  <> 0 then
    Result:=fGround and $FFFFFF
  else
    Result:=dib_Color(fGround);
end;

procedure XCanvas.Set_Xor_Mode;
begin
  Canvas.Set_Xor_Mode(ClearColor);
end;

procedure XCanvas.Set_Copy_Mode;
begin
  Canvas.Set_Copy_Mode
end;

function XCanvas.BeginPaint(PaintR: PRect): Boolean;
var
  w,h: int;
begin
  Result:=Canvas.BeginPaint(PaintR,fmaskX,fmaskY);

  FillChar(FPaintRect,SizeOf(FPaintRect),0);
  if PaintR <> nil then FPaintRect:=PaintR^;

  fppmm:=1 / Canvas.MetersPerPixel / 1000;

  w:=Canvas.GetWidth;
  h:=Canvas.GetHeight;

  fBitMask.Alloc_dc(w,h);
  fBitMask.ClipRect:=PaintR;

  if Assigned(fOnBeginPaint) then
  FOnBeginPaint(Self,PaintR)
end;

procedure XCanvas.EndPaint;
begin
  if Assigned(FOnEndPaint) then
  FOnEndPaint(Self);

  Canvas.EndPaint;
end;

procedure XCanvas.ClipPolyPolygon(lp: PPoly; cp: PIntegers; N: Integer);
begin
  Canvas.ClipPolygon(lp,cp,N)
end;

procedure XCanvas.ClipPaintRect;
var
  p: PRect;
begin
  p:=nil; with fPaintRect do
  if (Left < Right) and (Top < Bottom) then
  p:=@fPaintRect;

  Canvas.SetClipRect(p)
end;

procedure XCanvas.xPen(w,cl: Integer);
begin
  if x_Thin then w:=1;
  Canvas.SetPen(0,w,dib_Color(cl),0)
end;

procedure XCanvas.extPen(w,cl: Integer);
begin
  if x_Thin then w:=1;
  Canvas.SetPen(0,w,dib_Color(cl),1)
end;

procedure XCanvas.clPen(w,cl: Integer);
begin
  if x_Thin then w:=1;
  Canvas.SetPen(0,w,dmw_Unpack_cl(cl),0)
end;

procedure XCanvas.rgb_Pen(w,cl: Integer);
begin
  fCanvas.SetPen(0,w,cl,0);
end;

procedure XCanvas.rgb_xPen(w,st,cl: Integer);
begin
  Canvas.SetPen(st,w,cl,0);
end;

procedure XCanvas.xBrush(st,cl: Integer);
begin
  Canvas.SetBrush(st,dib_Color(cl))
end;

procedure XCanvas.clBrush(st,cl: Integer);
begin
  Canvas.SetBrush(st,dmw_Unpack_cl(cl))
end;

procedure XCanvas.rgb_Brush(cl: Integer);
begin
  Canvas.SetBrush(0,cl)
end;

procedure XCanvas.BeginPath;
begin
end;

procedure XCanvas.ClosePath;
begin
end;

procedure XCanvas.MoveTo(x,y: Integer);
begin
  Canvas.MoveTo(x,y)
end;

procedure XCanvas.LineTo(x,y: Integer);
begin
  Canvas.LineTo(x,y)
end;

procedure XCanvas.FillRect(const R: PRect);
begin
  Canvas.FillRect(R)
end;

procedure XCanvas.Rectangle(x1,y1,x2,y2: Integer);
begin
  Canvas.Rectangle(x1,y1,x2+1,y2+1)
end;

procedure XCanvas.Ellipse(x1,y1,x2,y2: Integer);
begin
  Canvas.Ellipse(x1,y1,x2+1,y2+1)
end;

procedure XCanvas.Frame(x1,y1,x2,y2: Integer);
begin
  MoveTo(x1,y1); LineTo(x2,y1);
  LineTo(x2,y2); LineTo(x1,y2);
  LineTo(x1,y1)
end;

procedure XCanvas.Romb(x,y,r: Integer);
var
  p: LOrient;
begin
  p[0]:=Point(x-r,y);
  p[1]:=Point(x,y-r);
  p[2]:=Point(x+r,y);
  p[3]:=Point(x,y+r); p[4]:=p[0];
  Canvas.PolyLine(@p,5);
end;

procedure XCanvas.RectPoint(x,y,r: Integer);
begin
  if (x >= -r-8) and (x < Width+r+8) then
  if (y >= -r-8) and (y < Height+r+8) then 
  Canvas.Rectangle(x-r,y-r,x+r+1,y+r+1)
end;

procedure XCanvas.CrossPoint(x,y,r: Integer);
var
  dc: ICanvas;
begin
  dc:=Canvas;
  if (x >= -r-8) and (x < Width+r+8) then
  if (y >= -r-8) and (y < Height+r+8) then begin
    dc.MoveTo(x-r,y-r); dc.LineTo(x+r,y+r);
    dc.MoveTo(x-r,y+r); dc.LineTo(x+r,y-r);
  end
end;

procedure XCanvas.MarkPoint(x,y,r,cl: int);
begin
  rgb_Pen(1,clBlack);
  rgb_brush(clBlack);
  Rectangle(x-r-1,y-r-1,x+r+2,y+r+2);

  if cl = 0 then cl:=clYellow;
  rgb_pen(1,cl);

  Frame(x-r,y-r,x+r,y+r)
end;

procedure XCanvas.DrawPoint(x,y,r: Integer);
begin
  Frame(x-r,y-r,x+r,y+r)
end;

procedure XCanvas.Arrow(const a,b: TPoint; r: Integer);
begin
  Draw_line(a,b);
  iDrawArrow(Canvas, a,b,r)
end;

procedure XCanvas.Punkt(x,y,dx, up,dn,ic: int);
begin
  iDrawPunkt(Canvas,x-dx,x+dx,y-up,y+dn,dib_Color(ic))
end;

procedure XCanvas.vPolyLine(lp: PPoly; N,Rx,Ry: int);
var
  i,x1,y1,x2,y2: int; a,b: TPoint;
  dx,dy,d,r,l: Double;
begin
  x1:=-Ry; y1:=-Ry; x2:=Width+Ry; y2:=Height+Ry;

  r:=Rx; d:=-r; 
  for i:=0 to N-1 do begin
    a:=lp[i]; b:=lp[i+1];
    dx:=b.X-a.X; dy:=b.Y-a.Y;

    if (dx <> 0) or (dy <> 0) then begin

      l:=Hypot(dx,dy);
      dx:=dx/l; dy:=dy/l;

      while d+r < l do begin
        d:=d + r;
        b.X:=a.X + Round(dx*d);
        b.Y:=a.Y + Round(dy*d);

        if (b.X > x1) and (b.X < x2) then
        if (b.Y > y1) and (b.Y < y2) then
        iDrawArrow(Canvas, a,b,Ry)
      end;

      d:=d - l
    end
  end
end;

procedure XCanvas.Draw_Line(const a,b: TPoint);
var
  lt,rb, _a,_b: TPoint;
begin
  lt.x:=0; rb.x:=Width;
  lt.y:=0; rb.y:=Height;
  if xClip_Line(lt,rb, a,b, _a,_b) then
  Line(_a.x,_a.y,_b.x,_b.y)
end;

procedure XCanvas.dmw_Line(const a,b: TPoint; cl: Integer);
var
  lt,rb,_a,_b: TPoint; w: Integer;
begin
  if cl > 0 then begin
    w:=(cl shr 5) and 3; xPen(w+1,cl);
  end;

  lt.x:=Min(a.x,b.x); lt.y:=Min(a.y,b.y);
  rb.x:=Max(a.x,b.x); rb.y:=Max(a.y,b.y);

  if Clip_Figure(lt,rb) then begin
    if xClip_Line(lt,rb, a,b, _a,_b) then
    Line(_a.x,_a.y,_b.x,_b.y)
  end else

  Line(a.x,a.y,b.x,b.y)
end;

procedure XCanvas.PolyPolyLine(lp: PPoly; cp: PIntegers; N: Integer);
begin
  if cp = nil then
    Canvas.Polyline(lp,N+1)
  else
    Canvas.PolyPolyline(lp,cp,N)
end;

procedure XCanvas.PolyPolygon(lp: PPoly; cp: PIntegers; N: Integer);
begin
  if cp = nil then
    Canvas.Polygon(lp,N+1)
  else
    Canvas.PolyPolygon(lp,cp,N)
end;

procedure XCanvas.PolyLine(lp: PPoly; N: Integer);
begin
  Canvas.PolyLine(lp,N+1)
end;

procedure XCanvas.Polygon(lp: PPoly; N: Integer);
begin
  Canvas.Polygon(lp,N+1)
end;

procedure XCanvas.Line(x1,y1,x2,y2: Integer);
begin
  Canvas.MoveTo(x1,y1); Canvas.LineTo(x2,y2)
end;

procedure XCanvas.SetPixel(x,y,cl: Integer);
begin
  Canvas.SetPixel(x,y,cl);
end;

procedure XCanvas.SetTextColor(fc: ColorRef);
begin
  Canvas.SetTextColor(fc)
end;

procedure XCanvas.TextOut(x,y: int; Str: PWideChar);
begin
  Canvas.TextOut(x,y,Str);
end;

function XCanvas.TextExtent(Str: PWideChar): TPoint;
begin
  Canvas.TextExtent(Str,Result)
end;

procedure XCanvas.TextOuts(x,y: int; const Str: WideString);
begin
  Canvas.TextOut(x,y,PWideChar(Str))
end;

function XCanvas.TextExtents(const Str: WideString): TPoint;
begin
  Result:=TextExtent(PWideChar(Str))
end;

procedure XCanvas.TextOutc(x,y,ic: int; Str: PChar);
var
  s: WideString; sz: TPoint;
begin
  s:=Str; sz:=TextExtents(s);
  if Contains_rect(x,y,x+sz.X,y+sz.Y,16,16) then begin
    if ic >= 0 then
    TextColor:=dib_Color(ic);
    TextOuts(x,y,s);
  end
end;

procedure XCanvas.TextOutcs(x,y,ic: int; const Str: String);
var
  s: TShortstr;
begin
  TextOutc(x,y,ic,StrPCopy(s,Str))
end;

procedure XCanvas.dmwPolygon(lp: PPoly; cp: PIntegers; N, cl: int);
var
  alfa,msk,pw,pc,fc: int;
begin
  alfa:=-1;

  msk:=tlong(cl).b[1] shr 2;
  pw:=succ(tlong(cl).b[2] and 3);
  if x_Thin then pw:=1;

  if tlong(cl).b[2] and $20 <> 0 then begin
    pc:=dmw_Unpack_cl(cl); fc:=pc; pw:=1;
    alfa:=msk shl 2; msk:=0;
  end else
  if tlong(cl).b[2] and $80 <> 0 then begin
    pc:=dmw_Unpack_cl(cl);
    fc:=pc; pw:=4
  end
  else begin
    pc:=dib_Color(cl shr 5);
    fc:=dib_Color(cl)
  end;

  if x_Poly then begin
    pc:=fc; pw:=0;
  end;

  if not x_Fill then msk:=7;

  Canvas.dmwPolygon(lp,cp,N, alfa,msk,fc, pw,pc)
end;

procedure XCanvas.alfPolygon(lp: PPoly; cp: PIntegers; N, pc,pw,fc,alfa: int);
var
  dc: ICanvas;
begin
  dc:=Canvas;

  if alfa = 0 then
    dc.fillPolygon(lp,cp,N,0,fc)
  else
    dc.alfaPolygon(lp,cp,N,alfa,alfa,fc,nil,-1);

  if pw > 0 then begin
    dc.SetPen(0,1,pc,0);
    dc.PolyPolyline(lp,cp,N)
  end
end;

procedure XCanvas.ivgPolygon(lp: PPoly; cp: PIntegers; N,pc,fc: int);
var
  dc: ICanvas; st,pw, msk,alfa: int;
begin
  dc:=Canvas;

  pc:=hgl_unpack_color(pc,st,pw);
  fc:=hgp_unpack_color(fc,msk,alfa);

  if (alfa = 0) and (fAlfa2 < 0) then
    dc.fillPolygon(lp,cp,N,msk,fc)
  else begin
    alfa:=255-alfa;

    if fAlfa2 >= 0 then
      dc.alfaPolygon(lp,cp,N,
                     alfa,fAlfa2,fc,
                     @fCenter,fAlfaK)
    else
      dc.alfaPolygon(lp,cp,N,
                     alfa,-1,fc,
                     nil,0)
  end;

  if pw > 0 then begin
    if x_Thin then pw:=1;
    dc.SetPen(st,pw,pc,0);
    PolyPolyline(lp,cp,N);

    dc.SetPen(0,1,pc,0)
  end
end;

procedure XCanvas.SetAlfaCenter(a,b: PPoint; alfa2: int);
var
  l: Double;
begin
  fAlfa2:=-1;

  if Assigned(a) then begin
    l:=Long_dist(a^,b^);
    if l > 2 then begin
      fCenter:=a^; fAlfaK:=1/l;
      fAlfa2:=255-alfa2
    end
  end
end;

procedure XCanvas.outPolygon(lp: PPoly; N: Integer);
begin
  if x_Fill then Polygon(lp,N)
  else Polyline(lp,N)
end;

procedure XCanvas.PolyStroke(lp: PPoly;
                             cp: PIntegers; N: int;
                             Step: float; Len: int);
var
  i,k,y,y1,y2,dx,dy,x1,x2,dc_w,dc_h: int;
  xp: PIntegers; line: TIntegers;
begin
  fStroke.Clear;

  if cp = nil then
    fStroke.Add_Frame(lp,N)
  else
    for i:=0 to N-1 do begin
      k:=cp[0]; cp:=@cp[1];
      fStroke.Add_Frame(lp,k-1);
      lp:=@lp[k]
    end;

  if fStroke.Count > 0 then begin

    y1:=fStroke.frame_lt.Y;
    y2:=fStroke.frame_rb.Y;

    dc_w:=Width; dc_h:=Height;

    if y1 < 0 then y1:=0;
    if y2 > dc_h then y2:=dc_h;

    if y1 < y2 then begin

      dy:=Round(Step*1000);
      if dy < 100 then dy:=1000;

      y1:=y1*1000; y2:=y2*1000;
      y1:=int_Round(y1,dy);

      while y1 < y2 do begin

        y:=y1 div 1000;

        xp:=@line;
        k:=fStroke.Get_mask(y,xp);

        while k > 0 do begin
          x1:=xp[0]; xp:=@xp[1];
          x2:=xp[0]; xp:=@xp[1];

          if Len <> 0 then begin
            dx:=x2-x1;
            if Len > 0 then begin
              if dx < Len then x2:=x1-1;
            end
            else begin
              if dx >= -Len then x2:=x1-1;
            end
          end;

          if x1 < x2 then begin
            if x1 < 0 then x1:=0;
            if x2 >= dc_w then x2:=dc_w-1;

            if x1 < x2 then begin
              MoveTo(x1,y); LineTo(x2,y)
            end
          end;

          Dec(k)
        end;

        Inc(y1,dy)
      end
    end
  end
end;

function XCanvas.Get_Scale(sc: double): double;
begin
  Result:=sc; if sc > 0 then
  Result:=1/(MetersPerPixel*sc)
end;

function XCanvas.MetersPerPixel: double;
begin
  Result:=Canvas.MetersPerPixel
end;

procedure XCanvas.SetBkMode_TRANSPARENT;
begin
  Canvas.SetBkMode(TRANSPARENT)
end;

function XCanvas.lp_Clip: Boolean;
begin
  Result:=true
end;

procedure XCanvas.xRepeat(Source: XCanvas);
begin
  Canvas.CopyRect(nil,Source.Canvas,nil);
end;

procedure XCanvas.ResetCanvas(fc,pc: int);
begin
  xBrush(0,fc); xPen(1,pc)
end;

procedure XCanvas.rgb_ResetCanvas(fc,pc: int);
begin
  rgb_Brush(fc); rgb_Pen(1,pc)
end;

procedure XCanvas.FillCanvas(fc: int);
begin
  Canvas.Clear(dib_Color(fc))
end;

procedure XCanvas.Move_Mask_X(dX: Integer);
begin
  fmaskX:=AlignMask(fMaskX,dX,fmaskLoc)
end;

procedure XCanvas.Move_Mask_Y(dY: Integer);
begin
  fmaskY:=AlignMask(fmaskY,dY,fmaskLoc)
end;

constructor TDib.Create;
begin
  inherited Create;
  fPix_src:=1; fPix_dst:=1
end;

function TDib.GetAllocated: bool;
begin
  Result:=false;
end;

function TDib.GetActive: bool;
begin
  Result:=Allocated
end;

function TDib.GetEmpty: bool;
begin
  Result:=true;

  if Active then
  if f_lt.X < f_rb.X then
  if f_lt.Y < f_rb.Y then

  Result:=false
end;

function TDib.GetScale: Float;
begin
  Result:=fPix_src / fPix_dst
end;

function TDib.GetClipRect: TRect;
begin
  Result:=Rect(fMinX,fMinY,fMaxX,fMaxY)
end;

procedure TDib.SetWindow(R: PRect);
begin
  fMinX:=R.Left; fMinY:=R.Top;
  fMaxX:=R.Right; fMaxY:=R.Bottom;
  if fMaxX >= Width then fMaxX:=Width-1;
  if fMaxY >= Height then fMaxY:=Height-1
end;

procedure TDib.MaxWindow; var R: TRect;
begin
  R:=Rect(0,0,Width-1,Height-1);
  SetWindow(@R)
end;

procedure TDib.xClipPaint(PaintR,ClipR: PRect; OnPaint: TOnPaint);
begin
  if BeginPaint(ClipR) then 
  OnPaint(Self,PaintR,ClipR);
  EndPaint;
end;

function TDib.Alloc(w,h,bits: int): bool;
begin
  if not Active
  or (Abs(w-Width) > 64)
  or (Abs(h-Height) > 64) then begin

    Free_dc;

    if not fImaging then
    Alloc_dc(w,h,bits);

    MaxWindow; Clear_dc(false)
  end;

  Result:=Active
end;

function TDib.Realloc_dc(w,h,bits: int): bool;
begin
  if not Active
  or (Width <> w)
  or (Height <> h) then begin
    Free_dc; Alloc_dc(w,h,bits)
  end;

  Result:=Active
end;

function TDib.Realloc_rgb(w,h: int): Boolean;
begin
  Result:=Realloc_dc(w,h,0)
end;

function TDib.Alloc_rgb(w,h: int): Boolean;
begin
  if not Active
  or (Abs(w-Width) > 64)
  or (Abs(h-Height) > 64) then
  Alloc_dc(w,h,0);

  Result:=Active
end;

procedure TDib.Clear_dc(free: bool);
begin
  if Active then begin MaxWindow;
    f_lt.x:=1; f_lt.y:=1; f_rb.x:=0; f_rb.y:=0;
    if free then Free_dc else FillCanvas(15);
    v_max.x:=0; v_max.y:=0;
  end
end;

procedure TDib.DrawWindow(x1,y1,x2,y2: Integer);
begin
  f_lt.x:=x1; f_rb.x:=x2;
  f_lt.y:=y1; f_rb.y:=y2;
  fPosX:=0; fPosY:=0;
end;

function TDib.xDraw(xPos,xRange,xMax: Integer;
                    yPos,yRange,yMax: Integer;
                    OnPaint: TOnPaint): Boolean;

procedure Paint(PaintR,ClipR: PRect; OnPaint: TOnPaint);
begin
  fIsMixed:=false;

  rgb_Brush(ClearColor);
  rgb_Pen(0,0);

  FillRect(PaintR); SetWindow(PaintR);

  Dec(PaintR.Left); Dec(PaintR.Top);
  Inc(PaintR.Right); Inc(PaintR.Bottom);

  if ClipR <> nil then begin
    Dec(ClipR.Left); Dec(ClipR.Top);
    Inc(ClipR.Right); Inc(ClipR.Bottom);
  end;

  if Assigned(OnPaint) then
  xClipPaint(PaintR,ClipR,OnPaint);

  MaxWindow
end;

procedure CopyRect(const dstR,srcR: TRect);
var
  dst,src: TRect;
begin
  with dstR do dst:=Rect(Left,Top,Right+1,Bottom+1);
  with srcR do src:=Rect(Left,Top,Right+1,Bottom+1);
  Canvas.CopyRect(@dst,nil,@src);
end;

var
  lt,rb: TPoint; x,y,w,h: int;
  S,T,R: TRect; ClipR: PRect;
begin
  Result:=false; if Active then

  if fPix_dst > 0 then

  if fIsRoad
  or (xPos < f_lt.X) or (xPos+xRange > f_rb.X)
  or (yPos < f_lt.Y) or (yPos+yRange > f_rb.Y) then begin
    w:=Width div fPix_dst; h:=Height div fPix_dst;

    x:=Min(xPos + (xRange div 2) + (w div 2),xMax) - w;
    y:=Min(yPos + (yRange div 2) + (h div 2),yMax) - h;

    lt.X:=Max(0,x); rb.X:=lt.X + w; v_max.X:=xMax;
    lt.Y:=Max(0,y); rb.Y:=lt.Y + h; v_max.Y:=yMax;

    R:=Rect(0,0,Width-1,Height-1);
    w:=0; h:=0; ClipR:=nil;

    if not fIsMixed then
    if not IsGray then
    if not fIsRoad then

    if (lt.X = f_lt.X) and (rb.X = f_rb.X) and
       (lt.Y < f_rb.Y) and (rb.Y > f_lt.Y) then begin

      S:=R; T:=R; h:=(f_lt.Y-lt.Y) * fPix_dst;

      if h > 0 then begin
        Dec(S.Bottom,h); Inc(T.Top,h);
        R.Bottom:=R.Top+h
      end
      else begin
        Dec(S.Top,h); Inc(T.Bottom,h);
        R.Top:=(f_rb.Y-lt.Y) * fPix_dst
      end;

      copyRect(T,S); ClipR:=@R
    end else

    if (lt.Y = f_lt.Y) and (rb.Y = f_rb.Y) and
       (lt.X < f_rb.X) and (rb.X > f_lt.X) then begin

      S:=R; T:=R; w:=(f_lt.X-lt.X) * fPix_dst;

      if w > 0 then begin
        Dec(S.Right,w); Inc(T.Left,w);
        R.Right:=R.Left+w
      end
      else begin
        Dec(S.Left,w); Inc(T.Right,w);
        R.Left:=(f_rb.X-lt.X) * fPix_dst
      end;

      copyRect(T,S); ClipR:=@R
    end;

    Move_Mask_X(w); Move_Mask_Y(h);

    f_lt:=lt; f_rb:=rb; xMove(xPos,yPos);

    Paint(@R,ClipR, OnPaint);
    Result:=true
  end
end;

procedure TDib.Backup(ix,iy: Integer; out ox,oy: Integer);
begin
  ox:=(fPosX + ix div fPix_dst) * fPix_dst;
  oy:=(fPosY + iy div fPix_dst) * fPix_dst
end;

procedure TDib.xMove(xPos,yPos: Integer);
begin
  fPosX:=xPos-f_lt.X;
  fPosY:=yPos-f_lt.Y;
end;

function TDib.xCopy(dst: XCanvas): Boolean;
var
  si,di: ICanvas;
  src_R,dst_R,card_R: TRect;
begin
  Result:=false;

  si:=Canvas;
  di:=dst.Canvas;

  if not Empty then
  if Assigned(di) then begin

    dst_R:=Rect(0,0,dst.Width,dst.Height);

    card_R:=dst_R; with src_R do begin
      Backup(dst_R.Left,dst_R.Top,Left,Top);

      Backup(dst_R.Right + fPix_dst,
             dst_R.Bottom + fPix_dst,Right,Bottom);

      Left:=Max(Left,0); Right:=Min(Right,Width);
      Top:=Max(Top,0); Bottom:=Min(Bottom,Height)
    end;

    dst_R.Left:=src_R.Left - fPosX * fPix_dst;
    dst_R.Top:=src_R.Top - fPosY * fPix_dst;

    dst_R.Right:=dst_R.Left+src_R.Right-src_R.Left;
    dst_R.Bottom:=dst_R.Top+src_R.Bottom-src_R.Top;

    if (src_R.Left < src_R.Right) and
       (src_R.Top <= src_R.Bottom) then begin

      if (dst_R.Left   <> card_R.Left)
      or (dst_R.Top    <> card_R.Top)
      or (dst_R.Right  <> card_R.Right)
      or (dst_R.Bottom <> card_R.Bottom) then begin
        di.SetBrush(0,dib_Color(Ground));
        di.FillRect(@card_R)
      end;

      di.CopyRect(@dst_R,si,@src_R);
      Result:=true
    end
    else begin
      di.SetBrush(0,dib_Color(Ground));
      di.FillRect(@card_R)
    end
  end
end;

procedure TDib.xCard(dc: XCanvas);
begin
  dc.maskX:=maskX; dc.Move_Mask_X(-fPosX * fPix_dst);
  dc.maskY:=maskY; dc.Move_Mask_Y(-fPosY * fPix_dst)
end;

procedure TDib.XPaint(R: PRect; dX,dY,k: Float; imPath: PChar);
var
  cv: ICanvas;
begin
  cv:=Canvas;

  if StrLen(imPath) > 0 then

    xImagePaint(cv,R,
                Round((f_lt.X*fPix_src - dX) * k),
                Round((f_lt.Y*fPix_src - dY) * k),
                fPix_src / fPix_dst * k,imPath,nil)

  else begin
    cv.SetBrush(0,clLtGray);
    cv.FillRect(nil);
  end
end;

procedure TDib.XGray;
begin
end;

procedure TDib.xStereo(Photo_a,Photo_b: TDib; Top: int);
begin
end;

procedure TDib.xPlastic(Plastic: TDib);
begin
end;

function TDib.BackupPixel(X,Y: int; out cl: int): bool;
begin
  Result:=false; cl:=0;

  Backup(X,Y,X,Y);

  if (X >= 0) and (X < Width) then
  if (Y >= 0) and (Y < Height) then begin
    cl:=Canvas.GetPixel(x,y); Result:=true
  end
end;

procedure Test_Mask(DC: XCanvas; Mask: TBitMask);
var
  x,y: Integer; line: PBytes;
begin
  with Mask.MaskRect do
  for y:=Top to Bottom do begin

    line:=Mask.ScanLine[y];

    if line <> nil then for x:=Left to Right do
    if line[x div 8] and B_Bit[x and 7] <> 0 then
    dc.SetPixel(x,y,1)
  end;

  Mask.Clear_Mask
end;

function ivg_draw_hg(xdc: XCanvas;
                     xc,yc,typ,color: int;
                     iv,ic: PIntegers; ik: int;
                     r1: int): Boolean;
var
  i,k,ax,alfa,pc,pw,ptyp,fc,ix,iy: int;
  kv,x1,x2,kf,f0,f1,f2: Double;
  ln: TLLine; c: TPoint;
begin
  Result:=false;

  pc:=hg_unpack_color(color,alfa,ptyp);
  alfa:=alfa shl 2;

  c.X:=xc; c.Y:=yc;

  f0:=Pi/2; kf:=2*Pi;

  pw:=0; if ptyp > 0 then pw:=1;

  if r1 >= 1 then
  if ik >= 1 then

  case typ of

1:  if ik = 1 then begin

    end
    else begin
      ax:=0;
      for i:=0 to ik-1 do Inc(ax,iv[i]);

      if ax > 0 then begin
        kv:=1/ax; x2:=0; ax:=0;

        for i:=0 to ik-1 do begin
          Inc(ax,iv[i]); fc:=ic[i];
          x1:=x2; x2:=ax*kv;

          f1:=x1*kf-f0; f2:=x2*kf-f0;

          ln.N:=LPoly_Sectorc(@ln.Pol[0], xc,yc,r1,f1,f2);

          xdc.alfPolygon(@ln.Pol,nil,ln.N, pc,pw,fc,alfa);
        end;

        Result:=true
      end
    end;

2:  begin
      ax:=0;
      for i:=0 to ik-1 do ax:=Max(ax,iv[i]);

      if ax > 0 then begin kv:=r1/ax;

        x1:=xc - r1/2;
        for i:=0 to ik-1 do begin
          iy:=yc - Round(iv[i]*kv); fc:=ic[i];

          ix:=Round(x1);
          ln.Pol[0]:=Point(ix,yc);
          ln.Pol[1]:=Point(ix,iy);

          x1:=x1 + r1/ik;
          ix:=Round(x1);

          ln.Pol[2]:=Point(ix,iy);
          ln.Pol[3]:=Point(ix,yc);
          ln.Pol[4]:=ln.Pol[0]; ln.N:=4;

          xdc.alfPolygon(@ln.Pol,nil,ln.N, pc,pw,fc,alfa);
        end;

        Result:=true
      end
    end;
  end
end;

procedure ivg_draw_Sign(DC: XCanvas;
                        const a,b: TPoint;
                        p1,p2: Integer);
var
  color,v1,v2: int; b1: TPoint;
begin
  case p1 and $FF of
0:  begin
      color:=hgr_unpack_color(p2,v1,v2);
      DC.rgb_Pen(v1,color);

      if Get_Next_Point(a,b,v2,b1) then
      DC.Line(a.X,a.Y,b1.X,b1.Y);
    end
  end
end;

end.
