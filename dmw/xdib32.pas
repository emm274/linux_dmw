unit xdib32; interface

{$mode delphi}

uses
  Classes, LCLType, GR32, xpens1,
  otypes, idib, xdib, xmask;

type
  TPen32 = record
    Style,Width,Color,Quality: int
  end;

  TBrush32 = record
    Style,Color: int
  end;

  TCanvas32 = class(TInterfacedObject,ICanvas)

    constructor Create(ABitmap: TBitmap32);
    destructor Destroy; override;

    function GetWidth: int; stdcall;
    function GetHeight: int; stdcall;

    function GetDC: HDC; stdcall;
    function GetMap(out map: Bitmap): Boolean; stdcall;

    function GetBitmap: Pointer; stdcall;
    procedure BitmapChanged; stdcall;

    procedure SetDrawText(const ADrawText: IDrawText); stdcall;

    procedure SetPen(ps,w,cl,qu: int); stdcall;
    procedure SetBrush(bs,cl: int); stdcall;

    procedure SetPenColor(cl: int); stdcall;
    procedure SetBrushColor(cl: int); stdcall;

    procedure MoveTo(x,y: int); stdcall;
    procedure LineTo(x,y: int); stdcall;

    procedure Clear(cl: int); stdcall;

    procedure FillRect(const R: PRect); stdcall;

    procedure Rectangle(x1,y1,x2,y2: int); stdcall;
    procedure Ellipse(x1,y1,x2,y2: int); stdcall;

    procedure PolyPolyline(lp: PPoly; cp: PIntegers; N: int); stdcall;
    procedure Polyline(lp: PPoly; N: int); stdcall;

    procedure PolyPolygon(lp: PPoly; cp: PIntegers; N: int); stdcall;
    procedure Polygon(lp: PPoly; N: int); stdcall;

    function GetPixel(x,y: int): int; stdcall;
    procedure SetPixel(x,y,cl: int); stdcall;

    procedure DrawFocusRect(const R: TRect); stdcall;

    procedure SetTextColor(fc: int); stdcall;
    procedure TextOut(x,y: int; Str: PWideChar); stdcall;
    function TextExtent(Str: PWideChar; out ts: TPoint): int; stdcall;

    function MetersPerPixel: double; stdcall;

    procedure Set_Xor_Mode(Ground: int); stdcall;
    procedure Set_Copy_Mode; stdcall;

    function BeginPaint(R: PRect; maskX,maskY: int): Boolean; stdcall;
    procedure EndPaint; stdcall;

    function GetClipRect(out R: TRect): Boolean; stdcall;
    procedure SetClipRect(const R: PRect); stdcall;

    procedure ClipPolygon(lp: PPoly; cp: PIntegers; N: int); stdcall;

    procedure SetBkMode(Mode: int); stdcall;

    procedure dmwPolygon(lp: PPoly; cp: PIntegers; N, alfa,msk,fc, pw,pc: int); stdcall;
    procedure fillPolygon(lp: PPoly; cp: PIntegers; N, msk,fc: int); stdcall;

    procedure alfaPolygon(lp: PPoly; cp: PIntegers;
                          N, alfa1,alfa2,fc: int;
                          at: PPoint; ak: float); stdcall;

    procedure CopyRect(Dst_R: PRect; Source: ICanvas; Src_R: PRect); stdcall;

    procedure CopyToDC(Dest: HDC); stdcall;

    procedure drawChar(x,y: int; data: PBytes; width,height,pitch: int); stdcall;
    procedure drawCharBW(x,y: int; data: PBytes; width,height,pitch: int); stdcall;

    procedure StretchDIBits(ox,oy,ow,oh: int;
                            bp: PBytes; inf: PBitmapInfo;
                            ix,iy,iw,ih: int;
                            alfa,blend: int); stdcall;
  private
    fBitmap: TBitmap32;
    fmask: TBitMask;

    fDrawText: IDrawText;

    fmap: Bitmap;

    fWidth: int;
    fHeight: int;

    fPen: TPen32;
    fBrush: TBrush32;

    fPosition: TPoint;
    fClipRect: TRect;

    fXorMode: bool;
  end;

  XBitmap32 = class(TDib)
    constructor Create;
    destructor Destroy; override;

    procedure toBitmap(const map: Bitmap);

    function Alloc_dc(w,h,bits: int): bool; override;
    procedure Free_dc; override;

  protected
    function GetAllocated: bool; override;

  private
    fBitmap: TBitmap32;
  end;

implementation

uses
  Graphics,
  gdk2,
  gtk2,
  gdk2pixbuf,
  glib2,
  gtk2Def,
  freetypep,
  img_x,xdc,
  xplot;

{$ASMMODE INTEL}

function Color32(Color: uint): uint;
asm
{$ifdef CPUX86_64}
  mov     RAx,RDi
{$endif}
  BSWAP   EAX
  MOV     AL,$FF
  ROR     EAX,8
end;

constructor TCanvas32.Create(ABitmap: TBitmap32);
begin
  inherited Create;
  fBitmap:=ABitmap;
  fmask:=TBitMask.Create(true);
end;

destructor TCanvas32.Destroy;
begin
  fDrawText:=nil;
  fmask.Free;
  inherited;
end;

procedure TCanvas32.BitmapChanged;
begin
  fWidth:=0; fHeight:=0;
  if Assigned(fBitmap) then begin
    fWidth:=fBitmap.Width;
    fHeight:=fBitmap.Height;
  end;

  if fWidth*fHeight > 0 then
  fmask.Alloc_dc(fWidth,fHeight)
end;

procedure TCanvas32.SetDrawText(const ADrawText: IDrawText);
begin
  fDrawText:=ADrawText
end;

function TCanvas32.GetWidth: int;
begin
  Result:=fBitmap.Width
end;

function TCanvas32.GetHeight: int;
begin
  Result:=fBitmap.Height
end;

function TCanvas32.GetDC: HDC;
begin
  Result:=0;
end;

function TCanvas32.GetMap(out map: Bitmap): Boolean;
var
  m: Bitmap;
begin
  Result:=false;
  Fillchar(m,Sizeof(m),0);

  m.bmWidth:=fBitmap.Width;
  m.bmHeight:=fBitmap.Height;
  m.bmBitsPixel:=32;
  m.bmWidthBytes:=m.bmWidth*4;
  m.bmPlanes:=1;
  m.bmBits:=fBitmap.Bits;

  map:=m;
  Result:=Assigned(m.bmBits)
end;

function TCanvas32.GetBitmap: Pointer;
begin
  Result:=fBitmap
end;

procedure TCanvas32.SetPen(ps,w,cl,qu: int);
begin
  if w = 0 then w:=fPen.Width;
  if w <= 1 then qu:=0;

  fPen.Style:=ps;
  fPen.Width:=w;
  fPen.Color:=Color32(cl);
  fPen.Quality:=qu;
end;

procedure TCanvas32.SetBrush(bs,cl: int);
begin
  fBrush.Style:=bs;
  fBrush.Color:=Color32(cl)
end;

procedure TCanvas32.SetPenColor(cl: int);
begin
  fPen.Color:=Color32(cl)
end;

procedure TCanvas32.SetBrushColor(cl: int);
begin
  fBrush.Color:=Color32(cl)
end;

procedure TCanvas32.MoveTo(x,y: int);
begin
  fPosition:=Point(x,y)
end;

procedure TCanvas32.LineTo(x,y: int);
var
  p: TPoint;
begin
  p:=fPosition;
  fBitmap.LineS(p.X,p.Y,x,y,fPen.Color);
  fPosition:=Point(x,y)
end;

procedure TCanvas32.Clear(cl: int);
begin
  fBitmap.Clear(cl);
end;

procedure TCanvas32.FillRect(const R: PRect);
var
  t: TRect; x2,y2: int;
begin
  if Assigned(R) then t:=R^
                 else t:=fClipRect;

  Inc(t.Right); Inc(t.Bottom);

  x2:=fWidth-1; y2:=fHeight-1;

  if t.Left < 0 then t.Left:=0;
  if t.Right >= x2 then t.Right:=x2;

  if t.Top < 0 then t.Top:=0;
  if t.Bottom >= y2 then t.Bottom:=y2;

  if (t.Left <= t.Right) and (t.Top <= t.Bottom) then
  fBitmap.FillRect(t.Left,t.Top,t.Right,t.Bottom,fBrush.Color)
end;

procedure TCanvas32.Rectangle(x1,y1,x2,y2: int);
begin
  if fBrush.Style <> int(bsCLEAR) then
  fBitmap.FillRect(x1,y1,x2,y2,fBrush.Color);

  if fPen.Style <> int(psCLEAR) then
  fBitmap.FrameRectS(x1,y1,x2,y2,fPen.Color)
end;

procedure TCanvas32.Ellipse(x1,y1,x2,y2: int);
begin
//  Canvas.Ellipse(x1,y1,x2,y2);
end;

procedure TCanvas32.PolyPolyline(lp: PPoly; cp: PIntegers; N: int);
var
  i,k: int;
begin
  if cp = nil then
    Polyline(lp,N)
  else begin
    for i:=0 to N-1 do begin
      k:=cp[i];
      PolyLine(lp,k);
      lp:=@lp[k]
    end
  end
end;

procedure TCanvas32.Polyline(lp: PPoly; N: int);

procedure test(lp: PPoly; N: int);
var
  poly: TThickPolyList; i,_n,pw: int;
  _lp: PPoly; _hp: PIntegers;
begin
  poly:=TThickPolyList.Create;
  try
    pw:=fPen.Width+4;
    if poly.Grow(lp,N-1,pw/2) > 0 then begin

      fPen.Width:=1;

      for i:=0 to poly.PartCount-1 do begin
        _n:=poly.Seek_poly(i,_lp,_hp);
        PolyLine(_lp,_n);
      end;

      fPen.Width:=pw
    end;
  finally
    poly.Free;
  end;
end;

var
  i,pc: int; a,b: TPoint; thick: bool;
begin
  if N > 1 then begin

    thick:=false;
    if fPen.Width >= 2 then

    if false then
      test(lp,N)
    else
    if Assigned(fmap.bmBits) then
    if fmask.ThickPolyLine(lp,N-1, fPen.Width/2) > 0 then begin
      fmask.Fill_map32(fmap,0,fPen.Color);
      fmask.Clear_Mask; thick:=true
    end;

    if not thick then begin
      b:=lp[0]; pc:=fPen.Color;
      for i:=1 to N-1 do begin
        a:=b; b:=lp[i];
        fBitmap.LineS(a.X,a.Y,b.X,b.Y,pc);
      end
    end
  end
end;

procedure TCanvas32.PolyPolygon(lp: PPoly; cp: PIntegers; N: int);
begin
  fillPolygon(lp,cp,N,0,fBrush.Color)
end;

procedure TCanvas32.Polygon(lp: PPoly; N: int);
var
  c: int;
begin
  c:=N; PolyPolygon(lp,@c,1)
end;

function TCanvas32.GetPixel(x,y: int): int;
begin
  Result:=fBitmap.Pixel[x,y]
end;

procedure TCanvas32.SetPixel(x,y,cl: int);
begin
  if cl = -1 then cl:=fPen.Color;
  fBitmap.SetPixelT(x,y,cl);
end;

procedure TCanvas32.DrawFocusRect(const R: TRect);
begin
//  Canvas.DrawFocusRect(R);
end;

procedure TCanvas32.SetTextColor(fc: int);
begin
  SetPenColor(fc)
end;

procedure TCanvas32.TextOut(x,y: int; Str: PWideChar);
begin
  if Assigned(fDrawText) then
  fDrawText.Draw(Self,x,y,0,Str);
end;

function TCanvas32.TextExtent(Str: PWideChar; out ts: TPoint): int;
begin
  Result:=0;
  if Assigned(fDrawText) then begin
    ts:=fDrawText.GetExtent(Str);
    Result:=ts.X
  end
end;

function TCanvas32.MetersPerPixel: double;
begin
  Result:=xdc.xMetersPerPixel(0)
end;

procedure TCanvas32.Set_Xor_Mode(Ground: int);
begin
  fXorMode:=true
end;

procedure TCanvas32.Set_Copy_Mode;
begin
  fXorMode:=false
end;

function TCanvas32.BeginPaint(R: PRect; maskX,maskY: int): Boolean;
var
  w,h: int;
begin
  Result:=false; EndPaint;

  w:=GetWidth; h:=GetHeight;
  if (w > 0) and (h > 0) then begin

    GetMap(fmap);

    SetClipRect(R);

    fmask.Alloc_dc(w,h);
    fmask.ClipRect:=R;

    Result:=true
  end
end;

procedure TCanvas32.EndPaint;
begin
  SetClipRect(nil)
end;

function TCanvas32.GetClipRect(out R: TRect): Boolean;
begin
  Result:=false;
  R:=fClipRect;
  if R.Left <= R.Right then
  if R.Top <= R.Bottom then
  Result:=true
end;

procedure TCanvas32.SetClipRect(const R: PRect);
begin
  fClipRect:=Rect(0,0,GetWidth,GetHeight);
  if R <> nil then fClipRect:=R^
end;

procedure TCanvas32.ClipPolygon(lp: PPoly; cp: PIntegers; N: int);
var
  i,k: int;
begin
  (*
  if fBitmap.Bits <> nil then begin

    if cp = nil then begin
      rgn:=CreatePolygonRgn(lp^,N+1,ALTERNATE);
      ExtSelectClipRgn(dc.Handle,rgn,RGN_COPY);
      if rgn <> 0 then DeleteObject(rgn)
    end else
    for i:=1 to N do begin
      k:=cp[0]; cp:=@cp[1];
      rgn:=CreatePolygonRgn(lp^,k,ALTERNATE);
      lp:=@lp[k];

      if rgn <> 0 then begin
        ExtSelectClipRgn(dc.Handle,rgn,RGN_COPY);
        DeleteObject(rgn)
      end
    end
  end
  *)
end;

procedure TCanvas32.SetBkMode(Mode: int);
begin
end;

procedure TCanvas32.fillPolygon(lp: PPoly; cp: PIntegers; N, msk,fc: int);
var
  map: Bitmap;
begin
  if GetMap(map) then
  if fmask.PolyPolygon(lp,cp,N) then begin
    fmask.Fill_map32(map,msk,fc);
    fmask.Clear_Mask
  end;
end;

procedure TCanvas32.dmwPolygon(lp: PPoly; cp: PIntegers; N, alfa,msk,fc, pw,pc: int);
var
  map: Bitmap;
begin
  pc:=Color32(pc);
  fc:=Color32(fc);

  if msk = 7 then begin
    if (pw = 0) or (pw = 4) then pw:=1;
    SetPen(0,pw,pc,0);
    PolyPolyLine(lp,cp,N);
  end
  else begin
    if GetMap(map) then
    if fmask.Active then

    if alfa >= 0 then
      fmask.alfa_Polygon(@map,lp,cp,N,alfa,alfa,fc,nil,-1)
    else
      fillPolygon(lp,cp,N, msk,fc);

    if pw <> 4 then begin
      if pw = 0 then pw:=1;
      SetPen(0,pw,pc,0);
      PolyPolyLine(lp,cp,N);
    end
  end
end;

procedure TCanvas32.alfaPolygon(lp: PPoly; cp: PIntegers;
                                N, alfa1,alfa2,fc: int;
                                at: PPoint; ak: float); stdcall;
var
  map: Bitmap;
begin
  if GetMap(map) then
  if fmask.Active then
  fmask.alfa_Polygon(@map,lp,cp,N,alfa1,alfa2,fc,at,ak)
end;

procedure TCanvas32.CopyRect(Dst_R: PRect; Source: ICanvas; Src_R: PRect);
var
  r1,r2: TRect; p: Pointer; si: TBitmap32;
  w1,h1, w2,h2: int;
begin
  if Assigned(Dst_R) then r1:=Dst_R^ else
  r1:=Rect(0,0,GetWidth,GetHeight);

  if Assigned(Src_R) then r2:=Src_R^ else
  r2:=Rect(0,0,GetWidth,GetHeight);

  if Source = nil then

  else begin
    p:=Source.GetBitmap;

    if Assigned(p) then
    if TObject(p) is TBitmap32 then begin
      si:=TObject(p) as TBitmap32;

      w1:=r1.Right-r1.Left;
      h1:=r1.Bottom-r1.top;

      w2:=r2.Right-r2.Left;
      h2:=r2.Bottom-r2.top;

      if (w1 = w2) and (h1 = h2) then
        si.DrawTo(fBitmap,r1.Left,r1.Top,r2)
      else
        si.DrawTo(fBitmap,r1,r2)
    end
  end
end;

procedure TCanvas32.CopyToDC(Dest: HDC);
var
  w,h: int; bits: Pointer;
begin
  w:=fBitmap.Width;
  h:=fBitmap.Height;
  bits:=fBitmap.Bits;

  if (w > 0) and (h > 0) then
  if bits <> nil then

  gdk_draw_rgb_32_image(
    TGtkDeviceContext(Dest).Drawable,
    TGtkDeviceContext(Dest).GC,
    0,0,w,h,
    GDK_RGB_DITHER_NORMAL,
    Pguchar(bits),w*4)
end;

procedure TCanvas32.drawChar(x,y: int; data: PBytes; width,height,pitch: int);
var
  w,h,a,a1: int; si,_si,_di: PBytes; di: pint; fc: tlong;
begin
  w:=fBitmap.Width;
  h:=fBitmap.Height;
  di:=Pointer(fBitmap.Bits);

  si:=data;
  if x < 0 then begin
    Inc(width,x); si:=@si[-x]; x:=0;
  end;

  if x+width > w then width:=w-x;

  if width > 0 then begin

    if y < 0 then begin
      Inc(height,y); si:=@si[-y*pitch]; y:=0;
    end;

    if y+height > h then height:=h-y;

    if height > 0 then begin
      Inc(di,y*w+x);

      fc.c:=fPen.Color;

      for y:=1 to height do begin

        _si:=si; _di:=Pointer(di);

        for x:=1 to width do begin

          a:=_si[0];
          if a > 0 then begin
            a1:=255-a;
            _di[0]:=(int(_di[0])*a1 + int(fc.b[0])*a) shr 8;
            _di[1]:=(int(_di[1])*a1 + int(fc.b[1])*a) shr 8;
            _di[2]:=(int(_di[2])*a1 + int(fc.b[2])*a) shr 8;
          end;

          _si:=@_si[1]; _di:=@_di[4]
        end;

        si:=@si[pitch]; Inc(di,w)
      end

    end
  end
end;

procedure TCanvas32.drawCharBW(x,y: int; data: PBytes; width,height,pitch: int);
var
  w,h,i,bit,_x,ax: int; si,_si,_di: PBytes; di: pint; fc: tlong;
begin
  w:=fBitmap.Width;
  h:=fBitmap.Height;
  di:=Pointer(fBitmap.Bits);

  si:=data;

  if x+width > w then width:=w-x;
  if width > 0 then begin

    if y < 0 then begin
      Inc(height,y); si:=@si[-y*pitch]; y:=0;
    end;

    if y+height > h then height:=h-y;

    if height > 0 then begin
      Inc(di,y*w+x);

      fc.c:=fPen.Color;

      for y:=1 to height do begin

        _si:=si; _di:=Pointer(di); _x:=x;

        ax:=_si[0]; bit:=128;
        for i:=0 to width-1 do begin

          if _x >= 0 then
          if ax and bit <> 0 then begin
            _di[0]:=fc.b[0];
            _di[1]:=fc.b[1];
            _di[2]:=fc.b[2];
          end;

          bit:=bit shr 1;
          if bit = 0 then begin
            bit:=128; _si:=@_si[1]; ax:=_si[0]
          end;

          Inc(_x); _di:=@_di[4]
        end;

        si:=@si[pitch]; Inc(di,w)
      end

    end
  end
end;

procedure TCanvas32.StretchDIBits(ox,oy,ow,oh: int;
                                  bp: PBytes; inf: PBitmapInfo;
                                  ix,iy,iw,ih: int;
                                  alfa,blend: int);
var
  x,y,w,h,_x,_y,line,bits,loc: int;
  fx,fy,kx,ky,dx,dy: double; ax: tlong;
  di,_di: pint; si: PBytes;
begin
  w:=fBitmap.Width;
  h:=fBitmap.Height;
  di:=Pointer(fBitmap.Bits);

  ky:=ih/oh; kx:=iw/ow;

  if ox+ow > w then ow:=w-ox;
  if oy+oh > h then oh:=h-oy;

  dx:=ix;
  if ox < 0 then begin
    Inc(ow,ox); dx:=dx+Abs(ox)*kx;
    ox:=0
  end;

  dy:=iy;
  if oy < 0 then begin
    Inc(oh,oy); dy:=dy+Abs(oy)*ky;
    oy:=0
  end;

  if ow*oh > 0 then begin

    bits:=inf.bmiHeader.biBitCount;
    line:=img_Line(inf.bmiHeader.biWidth,bits);
    loc:=bits div 8;

    Inc(di,oy*w+ox); fy:=0;

    for y:=1 to oh do begin

      _di:=Pointer(di); fx:=0;

      for x:=1 to ow do begin

        _x:=Round(fx*kx+dx);
        _y:=Round(fy*ky+dy);

        ax.c:=0;
        if loc = 1 then begin
        end else
        if loc = 3 then begin
          si:=@bp[_y*line + _x*3];
          ax.b[0]:=si[0];
          ax.b[1]:=si[1];
          ax.b[2]:=si[2];
        end;

        _di^:=ax.c; Inc(_di);

        fx:=fx+1
      end;

      fy:=fy+1; Inc(di,w)
    end
  end;
end;

constructor XBitmap32.Create;
begin
  inherited Create;
  fBitmap:=TBitmap32.Create;
  fCanvas:=TCanvas32.Create(fBitmap)
end;

destructor XBitmap32.Destroy;
begin
  Free_dc;
  fCanvas:=nil;
  fBitmap.Free;
  inherited
end;

function XBitmap32.GetAllocated: bool;
begin
  Result:=fBitmap.Bits <> nil;
end;

procedure XBitmap32.toBitmap(const map: Bitmap);
begin
  if fBitmap.Bits <> nil then
  if map.bmBits <> nil then
  if fBitmap.Width = map.bmWidth then
  if fBitmap.Height = map.bmHeight then
end;

function XBitmap32.Alloc_dc(w,h,bits: int): bool;
begin
  fBitmap.SetSize(w,h);
  fCanvas.BitmapChanged;
  Clear_dc(false); Result:=Active
end;

procedure XBitmap32.Free_dc;
begin
  fBitmap.Delete;
  fCanvas.BitmapChanged;
end;

end.

