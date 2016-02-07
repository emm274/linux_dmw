unit xdibcv; interface

{$mode delphi}

uses
  Classes, Graphics,
  LCLType, LCLIntf, Math,
  otypes, idib, xdib, xmask;

type
  TCanvasObj = class(TInterfacedObject,ICanvas)

    constructor Create;
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

  protected
    function Get_Canvas: TCanvas; virtual; abstract;
    function Get_Width: int; virtual;
    function Get_Height: int; virtual;
    function Get_Map(out map: Bitmap): Boolean; virtual;

  private
    fMaskDC: TBitMask;
    fClearColor: int;

  public
    property Canvas: TCanvas read Get_Canvas;
  end;

  TCanvasCV = class(TCanvasObj)
    constructor Create(ACanvas: TCanvas);
  protected
    function Get_Canvas: TCanvas; override;
  private
    fCanvas: TCanvas;
  end;

  TCanvasBmp = class(TCanvasObj)
    constructor Create(ABmp: TBitmap);
  protected
    function Get_Canvas: TCanvas; override;
    function Get_Width: int; override;
    function Get_Height: int; override;
    function Get_Map(out map: Bitmap): Boolean; override;
  private
    fBmp: TBitmap;
  end;

  XPaintDC = class(XCanvas)
    constructor Create(ADC: TCanvas);
  private
    fDC: TCanvas;
  end;

  XBmp = class(XCanvas)
    constructor Create(ABmp: TBitmap);
  private
    fBmp: TBitmap;
  public
    property Bmp: TBitmap read fBmp;
  end;

  XBitmap = class(TDib)
    constructor Create;
    destructor Destroy; override;

    function AssignBmp(Bmp: TBitmap; vX,vY: int): TBitmap;

    function PixelBits: Integer;

    procedure Assign_bmp(h: hBitMap);
    function Release_bmp: hBitMap;

    function Alloc_dc(w,h,bits: int): bool; override;
    procedure Free_dc; override;

  protected
    procedure xClipPaint(PaintR,ClipR: PRect;
                         OnPaint: TOnPaint); override;

    function GetAllocated: bool; override;

  private
    fBit: TBitMap;
    fDepth: Integer;

  public
    Transparent: Integer;

    property BitMap: TBitMap read fBit;
    property Depth: Integer read fDepth write fDepth;
  end;

implementation

uses
  Windows,
  convert,xdc,xddw,
  xgdi,xpens,img_x,
  ximages;

const
  winding = false;

constructor TCanvasObj.Create;
begin
  inherited;
  FMaskDC:=TBitMask.Create(false);
end;

destructor TCanvasObj.Destroy;
begin
  FMaskDC.Free;
  inherited;
end;

function TCanvasObj.Get_Width: int;
begin
  Result:=Canvas.ClipRect.Right
end;

function TCanvasObj.Get_Height: int;
begin
  Result:=Canvas.ClipRect.Bottom
end;

function TCanvasObj.Get_Map(out map: Bitmap): Boolean;
begin
  Result:=false;
  Fillchar(map,Sizeof(map),0);
end;

function TCanvasObj.GetWidth: int;
begin
  Result:=Get_Width
end;

function TCanvasObj.GetHeight: int;
begin
  Result:=Get_Height
end;

function TCanvasObj.GetDC: HDC;
begin
  Result:=0;
  if Canvas.HandleAllocated then
  Result:=Canvas.Handle;
end;

function TCanvasObj.GetMap(out map: Bitmap): Boolean;
begin
  Result:=Get_map(map)
end;

function TCanvasObj.GetBitmap: Pointer;
begin
  Result:=Self
end;

procedure TCanvasObj.BitmapChanged;
begin
end;

procedure TCanvasObj.SetDrawText(const ADrawText: IDrawText);
begin
end;

procedure TCanvasObj.SetPen(ps,w,cl,qu: int);
var
  dc: TCanvas; lb: TLogBrush;
begin
  dc:=Canvas;

  if w = 0 then w:=dc.Pen.Width;
  if w <= 1 then qu:=0;

  if qu = 0 then begin
    if w < 1 then w:=1;
    dc.Pen.Style:=TPenStyle(ps);
    Canvas.Pen.Color:=cl;
    Canvas.Pen.Width:=w
  end
  else begin
    lb.lbStyle:=BS_SOLID;
    lb.lbColor:=cl;
    lb.lbHatch:=0;

    Canvas.Pen.Handle:=

    ExtCreatePen(PS_GEOMETRIC or
                 PS_SOLID or
                 PS_ENDCAP_FLAT or
                 PS_JOIN_MITER,
                 w,lb,0,nil);
  end
end;

procedure TCanvasObj.SetBrush(bs,cl: int);
var
  dc: TCanvas;
begin
  dc:=Canvas;

  dc.Brush.Color:=cl;
  if bs < 0 then bs:=int(bsCLEAR);
  dc.Brush.Style:=TBrushStyle(bs)
end;

procedure TCanvasObj.SetPenColor(cl: int);
begin
  Canvas.Pen.Color:=cl;
end;

procedure TCanvasObj.SetBrushColor(cl: int);
begin
  Canvas.Brush.Color:=cl;
end;

procedure TCanvasObj.MoveTo(x,y: int);
begin
  Canvas.MoveTo(x,y)
end;

procedure TCanvasObj.LineTo(x,y: int);
begin
  Canvas.LineTo(x,y)
end;

procedure TCanvasObj.Clear(cl: int);
var
  dc: TCanvas;
begin
  dc:=Canvas;
  if dc.HandleAllocated then begin
    dc.FillRect(0,0,GetWidth,GetHeight);
    fClearColor:=cl
  end;
end;

procedure TCanvasObj.FillRect(const R: PRect);
var
  dc: TCanvas; t: TRect;
begin
  dc:=Canvas;
  if Assigned(R) then t:=R^
                 else t:=dc.ClipRect;

  dc.FillRect(t.Left,t.Top,t.Right+1,t.Bottom+1)
end;

procedure TCanvasObj.Rectangle(x1,y1,x2,y2: int);
begin
  Canvas.Rectangle(x1,y1,x2,y2);
end;

procedure TCanvasObj.Ellipse(x1,y1,x2,y2: int);
begin
  Canvas.Ellipse(x1,y1,x2,y2);
end;

procedure TCanvasObj.PolyPolyline(lp: PPoly; cp: PIntegers; N: int);
var
  dc: HDC; i,k: int;
begin
  dc:=Canvas.Handle;

  if cp = nil then
    LCLIntf.Polyline(dc,lp^,N)
  else begin
    for i:=0 to N-1 do begin
      k:=cp[i];
      LCLIntf.PolyLine(dc,lp^,k);
      lp:=@lp[k]
    end
  end
end;

procedure TCanvasObj.Polyline(lp: PPoly; N: int);
begin
  LCLIntf.PolyLine(Canvas.Handle,lp^,N)
end;

procedure TCanvasObj.PolyPolygon(lp: PPoly; cp: PIntegers; N: int);
var
  dc: TCanvas; br: HBrush; i,k: int;
begin
  dc:=Canvas;

  if cp = nil then
    LCLIntf.Polygon(dc.Handle,lp^,N,winding)
  else begin
    br:=CreateSolidBrush(fClearColor);

    for i:=0 to N-1 do begin k:=cp[i];

      LCLIntf.Polygon(dc.Handle,lp^,k,winding);
      lp:=@lp[k];

      if i = 0 then begin
        dc.Brush.Handle:=br; br:=0
      end
    end;

    if br <> 0 then DeleteObject(br)
  end
end;

procedure TCanvasObj.Polygon(lp: PPoly; N: int);
begin
  if N > 3 then LCLIntf.Polygon(Canvas.Handle,lp^,N,winding)
end;

function TCanvasObj.GetPixel(x,y: int): int;
begin
  Result:=Canvas.Pixels[x,y];
end;

procedure TCanvasObj.SetPixel(x,y,cl: int);
var
  dc: TCanvas;
begin
  dc:=Canvas;
  if cl = -1 then cl:=dc.Pen.Color;
  dc.Pixels[x,y]:=cl
end;

procedure TCanvasObj.DrawFocusRect(const R: TRect);
begin
  Canvas.DrawFocusRect(R);
end;

procedure TCanvasObj.SetTextColor(fc: int);
begin
  Canvas.Font.Color:=fc
end;

procedure TCanvasObj.TextOut(x,y: int; Str: PWideChar);
begin
  Canvas.TextOut(x,y,Str)
end;

function TCanvasObj.TextExtent(Str: PWideChar; out ts: TPoint): int;
var
  sz: TSize;
begin
  sz:=Canvas.TextExtent(Str);
  ts:=Point(sz.cx,sz.cy);
  Result:=sz.cx
end;

function TCanvasObj.MetersPerPixel: double;
begin
  Result:=xdc.xMetersPerPixel(Canvas.Handle)
end;

procedure TCanvasObj.Set_Xor_Mode(Ground: int);
var
  cl: TColor;
begin
  cl:=clWhite;
  if cl = Ground then cl:=clGray;
  xResetCanvas(Canvas,cl,cl);
  Canvas.Pen.Mode:=pmNot;
end;

procedure TCanvasObj.Set_Copy_Mode;
begin
  Canvas.Pen.Mode:=pmCopy
end;

function TCanvasObj.BeginPaint(R: PRect; maskX,maskY: int): Boolean;
var
  dc: TCanvas; w,h: int;
begin
  Result:=false;

  EndPaint; dc:=Canvas;

  if Assigned(dc) then
  if dc.HandleAllocated then begin

    w:=Get_Width; h:=Get_Height;
    if (w > 0) and (h > 0) then begin

      SetClipRect(R);
      SetBrushOrgEx(dc.Handle, maskX,maskY);

      fMaskDC.Alloc_dc(w,h);
      fMaskDC.ClipRect:=R;

      Result:=true
    end
  end;
end;

procedure TCanvasObj.EndPaint;
begin
  SetClipRect(nil)
end;

function TCanvasObj.GetClipRect(out R: TRect): Boolean;
begin
  Result:=false;
  R:=Canvas.ClipRect;
  if R.Left <= R.Right then
  if R.Top <= R.Bottom then
  Result:=true
end;

procedure TCanvasObj.SetClipRect(const R: PRect);
var
  dc: TCanvas; rgn: HRGN;
begin
  dc:=Canvas;

  if Assigned(dc) then
  if dc.HandleAllocated then begin
    rgn:=0; if R <> nil then
    rgn:=CreateRectRgn(R.Left,R.Top,R.Right+1,R.Bottom+1);

    SelectClipRgn(Canvas.Handle,rgn);
    if rgn <> 0 then DeleteObject(rgn)
  end
end;

procedure TCanvasObj.ClipPolygon(lp: PPoly; cp: PIntegers; N: int);
var
  dc: TCanvas; rgn: HRGN; i,k: int;
begin
  dc:=Canvas;

  if Assigned(dc) then
  if dc.HandleAllocated then begin

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
end;

procedure TCanvasObj.SetBkMode(Mode: int);
begin
  LCLIntf.SetBkMode(Canvas.Handle,TRANSPARENT)
end;

procedure TCanvasObj.fillPolygon(lp: PPoly; cp: PIntegers;
                                 N, msk,fc: int);

procedure Fill_dc(dc: TCanvas; cl: int);
var
  x,y,al,bit: int; si: PByte; r: TRect;
begin
  r:=fMaskDC.MaskRect;;

  for y:=r.Top to r.Bottom do begin

    si:=fMaskDC.ScanLine[y];
    if si <> nil then begin

      Inc(si,r.Left div 8);
      bit:=B_Bit[r.Left and 7];

      al:=si^;
      for x:=r.Left to r.Right do begin
        if al and bit <> 0 then
        dc.Pixels[x,y]:=cl;

        bit:=bit shr 1;
        if bit = 0 then begin
          bit:=$80; Inc(si); al:=si^
        end
      end

    end
  end
end;

var
  dc: TCanvas; map: Bitmap;
begin
  dc:=Canvas;

  if (msk = 0)
  or not fmaskDC.Active then begin
    SetBrush(0,fc);
    SetPen(0,1,fc,0);
    PolyPolygon(lp,cp,N)
  end else

  if fMaskDC.PolyPolygon(lp,cp,N) then begin
    fMaskDC.Pat_Mask(msk);

    if GetMap(map) then
      fMaskDC.Fill_map(map,fc)
    else
      Fill_dc(dc,fc);

    fMaskDC.Clear_Mask
  end;
end;

procedure TCanvasObj.dmwPolygon(lp: PPoly; cp: PIntegers; N, alfa,msk,fc, pw,pc: int);
var
  dc: TCanvas;
  map: Bitmap; mapp: PBitmap;
begin
  dc:=Canvas;

  if pw >= 0 then begin
    dc.Pen.Style:=psSolid;
    dc.Pen.Color:=pc;

    if pw = 0 then dc.Pen.Width:=1
              else dc.Pen.Width:=pw;
  end;

  if msk = 7 then begin
    if pw = 4 then dc.Pen.Width:=1;
    if pw > 0 then
    PolyPolyLine(lp,cp,N);
  end
  else begin

    mapp:=nil;
    if alfa >= 0 then
    if fMaskDC.Active then
    if GetMap(map) then
    if map.bmBitsPixel = 24 then
    mapp:=@map;

    if Assigned(mapp) then
      fMaskDC.alfa_Polygon(mapp,lp,cp,N,alfa,alfa,fc,nil,-1)
    else begin
      if pw = 4 then
      dc.Pen.Style:=psClear;

      if msk = 0 then begin
       dc.Brush.Color:=fc;
       dc.Brush.Style:=bsSolid;
       PolyPolygon(lp,cp,N)
      end
      else begin
        fillPolygon(lp,cp,N,msk,fc);
        if pw <> 4 then PolyPolyline(lp,cp,N)
      end

    end
  end
end;

procedure TCanvasObj.alfaPolygon(lp: PPoly; cp: PIntegers;
                                 N, alfa1,alfa2,fc: int;
                                 at: PPoint; ak: float); stdcall;
var
  map: Bitmap;
begin
  if not fMaskDC.Active
  or not GetMap(map) then
    fillPolygon(lp,cp,N,0,fc)
  else
    fMaskDC.alfa_Polygon(@map,lp,cp,N,alfa1,alfa2,fc,at,ak)
end;

procedure TCanvasObj.CopyRect(Dst_R: PRect; Source: ICanvas; Src_R: PRect);
var
  r1,r2: TRect; si,di: HDC;
begin
  if Assigned(Dst_R) then r1:=Dst_R^ else
  r1:=Rect(0,0,Get_Width,Get_Height);

  if Assigned(Src_R) then r2:=Src_R^ else
  r2:=Rect(0,0,Get_Width,Get_Height);

  di:=GetDC;
  if di <> 0 then begin

    si:=0;
    if Source = nil then si:=di
                    else si:=Source.GetDC;

    if si <> 0 then
      StretchBlt(di, r1.Left,r1.Top,r1.Right-r1.Left,r1.Bottom-r1.Top,
                 si, r2.Left,r2.Top,r2.Right-r2.Left,r2.Bottom-r2.Top,
                 SRCCOPY)
  end
end;

procedure TCanvasObj.CopyToDC(Dest: HDC);
var
  dc: HDC; w,h: int;
begin
  dc:=GetDC;
  if dc <> 0 then
  BitBlt(Dest,
         0,0,GetWidth,GetHeight,
         dc,0,0,SRCCOPY)
end;

procedure TCanvasObj.drawChar(x,y: int; data: PBytes; width,height,pitch: int);
begin
end;

procedure TCanvasObj.drawCharBW(x,y: int; data: PBytes; width,height,pitch: int);
begin
end;

procedure TCanvasObj.StretchDIBits(ox,oy,ow,oh: int;
                                   bp: PBytes; inf: PBitmapInfo;
                                   ix,iy,iw,ih: int;
                                   alfa,blend: int);
begin
end;

constructor TCanvasCV.Create(ACanvas: TCanvas);
begin
  inherited Create; fCanvas:=ACanvas
end;

function TCanvasCV.Get_Canvas: TCanvas;
begin
  Result:=fCanvas
end;

constructor TCanvasBmp.Create(ABmp: TBitmap);
begin
  inherited Create; fBmp:=ABmp
end;

function TCanvasBmp.Get_Canvas: TCanvas;
begin
  Result:=fBmp.Canvas
end;

function TCanvasBmp.Get_Width: int;
begin
  Result:=fBmp.Width
end;

function TCanvasBmp.Get_Height: int;
begin
  Result:=fBmp.Height
end;

function TCanvasBmp.Get_Map(out map: Bitmap): Boolean;
begin
  Result:=false;
  Fillchar(map,Sizeof(map),0);

  if fBmp.HandleAllocated then
  if GetObject(fBmp.Handle,Sizeof(map),@map) > 0 then
  Result:=Assigned(map.bmBits)
end;

constructor XPaintDC.Create(ADC: TCanvas);
begin
  inherited Create; fDC:=ADC;
  fCanvas:=TCanvasCV.Create(fDC)
end;

constructor XBmp.Create(ABmp: TBitmap);
begin
  inherited Create; fBmp:=ABmp;
  fCanvas:=TCanvasBmp.Create(fBmp)
end;

constructor XBitmap.Create;
begin
  inherited Create;
  fBit:=TBitMap.Create;
  fCanvas:=TCanvasBmp.Create(fBit)
end;

destructor XBitmap.Destroy;
begin
  Free_dc;
  fCanvas:=nil;
  fBit.Free;
  inherited
end;

function XBitmap.AssignBmp(Bmp: TBitmap; vX,vY: int): TBitmap;
begin
  Result:=fBit; fBit:=Bmp;

  fMinX:=0; fMaxX:=Bmp.Width;
  fMinY:=0; fMaxY:=Bmp.Height;

  f_lt.X:=vX; f_rb.X:=vX + fMaxX;
  f_lt.Y:=vY; f_rb.Y:=vY + fMaxY;
  fPix_src:=1; fPix_dst:=1;
end;

function XBitmap.GetAllocated: bool;
begin
  Result:=fBit.HandleAllocated;
end;

function XBitmap.PixelBits: Integer;
begin
  Result:=Bitmap_Depth(fBit)
end;

procedure XBitmap.Assign_bmp(h: hBitMap);
begin
  fBit.Handle:=h; MaxWindow
end;

function XBitmap.Release_bmp: hBitMap;
begin
  Result:=fBit.ReleaseHandle;
end;

function XBitmap.Alloc_dc(w,h,bits: int): bool;
var
  ds: Pointer;
begin
  Free_dc;

  if bits = 1 then
    fBit.Handle:=CreateBitmap(w,h,1,1,nil)
  else
    fBit.Handle:=dib_alloc_rgb(w,h,ds);

  if not fBit.HandleAllocated then
  if bits <> 1 then
  fBit.Handle:=xCreateCompatibleBitMap(w,h);

  Clear_dc(false); Result:=Active
end;

procedure XBitmap.Free_dc;
begin
  dib_clear_bitmap(fBit);
end;

procedure XBitmap.xClipPaint(PaintR,ClipR: PRect; OnPaint: TOnPaint);
var
  S: TSize; R: TRect; Alt,Tmp: TBitmap; ds: Pointer;
begin
  Alt:=TBitMap.Create;
  try
    S.cx:=Width; S.cy:=Height;

    if not fBit.Monochrome then
    if Assigned(ClipR) then begin

      if PixelBits = 24 then
      Alt.Handle:=dib_alloc_bmp(S.cx,S.cy,24,nil,0,ds);

      if not Alt.HandleAllocated then
      Alt.Handle:=xCreateCompatibleBitMap(S.cx,S.cy)
    end;

    if not Alt.HandleAllocated then

      inherited xClipPaint(PaintR,ClipR,OnPaint)

    else begin

      BitBlt(Alt.Canvas.Handle, 0,0,S.cx,S.cy,
             fBit.Canvas.Handle, 0,0, SRCCOPY);

      Tmp:=fBit; fBit:=Alt;

      inherited xClipPaint(PaintR,ClipR,OnPaint);

      R:=Rect(0,0,S.cx,S.cy);
      if Assigned(clipR) then
      with ClipR^ do R:=Rect(Left,Top,Right+1,Bottom+1);

      xBitBlt(Tmp.Canvas,@R,Alt.Canvas,nil);

      fBit:=Tmp
    end;
  finally
    Alt.Free
  end;
end;

end.

