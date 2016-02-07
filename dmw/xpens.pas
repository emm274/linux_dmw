unit xpens; interface {$H-}

uses
  Classes,Graphics,
  LCLType,LCLIntf,Math,
  otypes,xline;

const
  txt_up = 1;
  txt_cx = 2;
  txt_cy = 4;
  txt_rx = 8;

type
  TOutText = class
    constructor Create(ADC: TCanvas; h,w,fi,fc: integer; FName: PChar);
    destructor Destroy; override;

    function OutText(px,py,fc: int; Str: PChar): TPoint;
    function GetBound(bp: PLPoly; px,py: int; Str: PChar): int;
  private
    fDC: TCanvas;
    frot: Real3x3;
    fSize: TPoint;
    fOld: TLogFont;

    function xOutText(ADC: HDC; px,py: int; Str: PChar): TPoint;

  end;

procedure dc_FillRect(dc: HDC; x1,y1,x2,y2, cl: Integer);
function dc_ClipRect(dc: HDC; d: Integer): TRect;

procedure dc_Line(dc: HDC; x1,y1,x2,y2: Integer);

procedure dc_PolyLine(dc: HDC; lp: PPoly; N: Integer);

procedure dc_PolyPolyLine(dc: HDC;
                          lp: PPoly; lp_N: Integer;
                          cp: PIntegers; cp_N: Integer);

function xGridRect(const R: TRect): TRect;
function xClipRect(dc: TCanvas; Delta: Integer): TRect;

procedure xResetCanvas(dc: TCanvas; pc,fc: TColor);

procedure xBitBlt(Dst: TCanvas; DstR: PRect;
                  Src: TCanvas; From: PPoint);

procedure xSetPencil(dc: TCanvas; st: TPenStyle;
                     w: Integer; cl: TColor);

procedure xThinPencil(dc: TCanvas; cl: TColor);
procedure xThickPencil(dc: TCanvas; w: int; cl: TColor);

procedure xFillCanvas(dc: TCanvas; cl: TColor);

procedure xPaintClip(dc: TCanvas; const Rect: TRect);
procedure xPaintExclude(dc: TCanvas; const Rect: TRect);
procedure xPainTRect(dc: TCanvas; const Rect: TRect);
procedure xPaintFrame(dc: TCanvas; const Rect: TRect);

procedure xFrameLine(dc: TCanvas; x1,y1,x2,y2, cl1,cl2: Integer);

procedure xDownBox(dc: TCanvas; x1,y1,x2,y2,cl: Integer);
procedure xButtBox(dc: TCanvas; x1,y1,x2,y2,cl: Integer);

procedure xDownBox1(dc: TCanvas; x1,y1,x2,y2,cl: Integer);
procedure xButtBox1(dc: TCanvas; x1,y1,x2,y2,cl: Integer);

procedure xDownRect(dc: TCanvas; const R: TRect; cl: TColor);
procedure xFreeRect(dc: TCanvas; const R: TRect; cl: TColor);

procedure xThinBox(dc: TCanvas; x1,y1,x2,y2,cl: Integer);
procedure uThinBox(dc: TCanvas; x1,y1,x2,y2,cl: Integer);

procedure xHorz_dots(dc: TCanvas; X1,X2,dX,Y,Color: Integer);
procedure xVert_dots(dc: TCanvas; X,Y1,Y2,dY,Color: Integer);

procedure xDotLine(dc: TCanvas; x1,x2,y: Integer);
procedure xVertLine(dc: TCanvas; x,y1,y2: Integer);
procedure xHorzLine(dc: TCanvas; x1,x2,y: Integer);
procedure xDrawLine(dc: TCanvas; x1,y1,x2,y2: Integer);

procedure xVertDash(dc: TCanvas; x,y1,y2,dy: Integer);
procedure xHorzDash(dc: TCanvas; x1,x2,y,dx: Integer);

procedure xVertThick(dc: TCanvas; x,y1,y2,thick: Integer);
procedure xHorzThick(dc: TCanvas; x1,x2,y,thick: Integer);

procedure xDrawPoint(dc: TCanvas; x,y,d: Integer);
procedure xMarkPoint(dc: TCanvas; x,y,d,cl: Integer);
procedure xCrossPoint(dc: TCanvas; x,y,d: Integer);
procedure xCursorPoint(dc: TCanvas; x,y,d: Integer);
procedure xEllipse(dc: TCanvas; x,y,r,cl,thick: Integer);

procedure xc_Cross(dc: TCanvas; x,y,d,cl: Integer);
procedure lb_Cross(dc: TCanvas; x,y,d,cl: Integer);

procedure xc_Romb(dc: TCanvas; x,y,dx,dy,pc,bc: integer);
procedure lb_Romb(dc: TCanvas; x,y,dx,dy,cl: integer);

procedure lb_Rectangle(dc: TCanvas; x,y,d,cl: Integer);

procedure xDraw_Line(dc: HDC; x1,y1,x2,y2: Integer);
procedure xDraw_Rect(dc: HDC; x1,y1,x2,y2: Integer);
procedure xCross_Point(dc: HDC; x,y,d: Integer);

procedure xRay(dc: TCanvas; xc,yc: Integer;
               r1,r2,f: Double; cl: integer);

procedure xSector(dc: TCanvas; xc,yc: Integer;
                  r,f1,f2: Double; cl: integer;
                  lp: PLPoly);

procedure fSector(dc: HDC; xc,yc: Integer;
                  r,f1,f2: Double);

procedure xRectangle(dc: TCanvas; x1,y1,x2,y2, cl: integer);
procedure xFocusRect1(dc: TCanvas; x1,y1,x2,y2,dr, cl: integer);

procedure xFillMask(dc: TCanvas; x1,y1,x2,y2, msk,cl: integer);

procedure xDisp_line(dc: TCanvas; const a,b: TPoint; cl: Integer);
procedure xDisp_thin(dc: TCanvas; lp: PLPoly; n,w,cl: Integer);
procedure xDisp_bound(dc: TCanvas; lp: PLPoly; n,cl: Integer);
procedure xDisp_track(dc: TCanvas; lp: PLPoly; n: Integer);
procedure xDisp_frame(dc: TCanvas; lp: PLPoly; n: Integer);

procedure xDraw_arrow(dc: TCanvas; const a,b: TPoint; r: Integer);
procedure xDraw_arrow1(dc: TCanvas; const a,b: TPoint; r1,r2: Integer);

procedure fTriangle(dc: TCanvas;
                    xc,yc, dx,up,dn, dy: integer;
                    fi: Float; pc,bc: TColor);

procedure xTriangle(dc: TCanvas; x1,x2,y1,y2: int; pc,bc: TColor);
procedure lTriangle(dc: TCanvas; x1,x2,y1,y2: int; pc,bc: TColor);
procedure rTriangle(dc: TCanvas; x1,x2,y1,y2: int; pc,bc: TColor);

procedure xPunkt(dc: TCanvas; x1,x2,y1,y2, cl: integer);
procedure xPunkt1(dc: TCanvas; x1,x2,y1,y2, cl: integer);

procedure xRomb(dc: TCanvas; x1,x2,y,dx,dy: integer; pc,bc: TColor);
procedure yRomb(dc: TCanvas; x,y1,y2,dx,dy: integer; pc,bc: TColor);
procedure vRomb(dc: TCanvas; x1,y1,x2,y2,dx,dy: integer;
                lc,fc,uc,rc: TColor);

procedure xDrawPixel(dc: TCanvas; x,y,r: int);
procedure xRectPixel(dc: TCanvas; x,y,r,cl: int);
procedure xDrawRect(dc: TCanvas; x,y,r: integer);

procedure xDrawMarker(dc: TCanvas; x,y,d,cl: integer);
procedure xHideMarker(dc: TCanvas; x,y,d,cl: integer);
procedure xLinkMarker(dc: TCanvas; x,y,d,cl: integer);
procedure xFlagMarker(dc: TCanvas; X,Y,W,H,Color: Integer);

procedure xDrawTarget(dc: TCanvas; x,y: integer);
procedure xDrawLocator(dc: TCanvas; x,y,d: integer);
procedure xDrawCursor(dc: TCanvas; x,y,d: integer);
procedure xThickCursor(dc: TCanvas; x,y,r: Integer);
procedure xDrawMovePixel(dc: TCanvas; x1,y1,x2,y2,d: integer);

procedure xDrawShape(dc: TCanvas;
                     cx,cy: Integer; fi,k: Double;
                     typ, a,b1,b2: Integer; cl: TColor);

procedure xDrawIcon(dc: TCanvas; Id, x,y,w,h,mode: Integer);
procedure xDrawTool(dc: TCanvas; Id, x,y,w,h: Integer);

function xSwapRect(const R: TRect): TRect;

function xTruncStr(dc: TCanvas; Width: Integer; var s: string): Integer;
function rTruncStr(dc: TCanvas; const R: TRect; var s: string): Integer;

procedure OutValue(dc: HDC; x,y,h,w,fc,bc: integer; FName,Str: PChar);

procedure xOutTextA(dc: TCanvas; x,y,h,w,fi,fc: integer; FName,Str: PChar);

function xOutText(dc: TCanvas; x,y: integer; const s: string): Integer;

procedure xTextOut(dc: TCanvas; x,y,flags: integer; s: string);
procedure xTextPoint(dc: TCanvas; x,y: integer; s: string);

procedure yTextOut(dc: TCanvas; x,y1,y2: integer; s: string);
procedure fTextOut(dc: TCanvas; x,y,bc,fc: integer; s: string);

procedure xTextRect(dc: TCanvas; x1,y1,x2,y2,d: int; const s: string);
procedure yTextRect(dc: TCanvas; const R: TRect; const s: String);
procedure cTextRect(dc: TCanvas; const r: TRect; d: int; const s: string);

procedure xTextDown(dc: TCanvas; x,y,w,h: integer; s: string);
procedure xTextDraw(dc: TCanvas; x,y,f: integer; s: string);
function xTextCentre(dc: TCanvas; x1,x2,y: integer; s: string): Integer;

function xTextRight(dc: TCanvas; x1,x2,y: integer;
                    const s: string): Integer;

procedure xTextAxe(dc: TCanvas; x1,x2,y: integer; s: string);
procedure xTextTrunc(dc: TCanvas; x1,x2,y: integer; s: string);

function xVertText(dc: TCanvas; x1,y1,y2: integer;
                   s: string; var R: TRect): boolean;

procedure xTextRomb(dc: TCanvas;
                    x,y1,y2, dx,dy,d: int;
                    s: string);

procedure xTextHint(dc: TCanvas;
                    x,y,d1,d2,left,up: int;
                    bc,pc,fc: int;
                    Str: PChar);

function xRGB(r,g,b: Integer): TColor;

implementation

uses
  Windows,SysUtils,xgdi;

const
  winding = false;

constructor TOutText.Create(ADC: TCanvas; h,w,fi,fc: integer; FName: PChar);
var
  lfont: TLogFont;
begin
  inherited Create; fDC:=ADC;

  Fillchar(lfont,Sizeof(lfont),0);
  with lfont do begin
    lfHeight:=h; lfWeight:=w;

    lfCharSet:=DEFAULT_CHARSET;
    lfQuality:=Proof_Quality;

    StrPCopy(lfFaceName,FName);

    if fi <> 0 then begin
      lfEscapement:=fi+1;
      lfOrientation:=fi;
    end
  end;

  frot:=Identify_3x3; if fi <> 0 then
  frot:=Rotate_3x3(fi/1800*Pi);

  GetObject(fDC.Font.Handle,Sizeof(fOld),@fOld);

  fDC.Font.Handle:=CreateFontIndirect(lfont);

  if fc <> clNone then
  fDC.Font.Color:=fc
end;

destructor TOutText.Destroy;
begin
  fDC.Font.Handle:=CreateFontIndirect(fOld);
  inherited
end;

function TOutText.xOutText(ADC: HDC; px,py: int; Str: PChar): TPoint;
var
  eol: PChar; p,q,s: TPoint; sz: TSize;
  pc: PChar; l: int; t: TShortstr;
begin
  p:=Point(px,py); s:=Point(0,0);

  while Str[0] <> #0 do begin

    pc:=Str;
    eol:=StrScan(pc,#10);

    if eol = nil then
      l:=Strlen(pc)
    else begin
      l:=eol-pc; t[0]:=#0;
      if l > 0 then begin
        if l > 255 then l:=255;
        StrLCopy(t,pc,l)
      end; pc:=t
    end;

    if l > 0 then begin
      if ADC <> 0 then
      TextOut(ADC,p.X,p.Y,pc,l);

      GetTextExtentPoint(fDC.Handle,pc,l,sz);
      sz.cy:=sz.cy * 8 div 10;

      if sz.cx > s.X then s.X:=sz.cx;
      Inc(s.Y,sz.cy);

      q:=iTransit_3x3(0,sz.cy,frot);
      Inc(p.X,q.X); Inc(p.Y,q.Y)
    end;

    if eol = nil then Break;
    Str:=@eol[1]
  end;

  fSize:=s; Result:=p
end;

function TOutText.OutText(px,py,fc: int; Str: PChar): TPoint;
begin
  if fc <> clNone then fDC.Font.Color:=fc;
  Result:=xOutText(fDC.Handle, px,py, Str)
end;

function TOutText.GetBound(bp: PLPoly; px,py: int; Str: PChar): int;
var
  i,w,h,r: int;
begin
  Result:=0;

  xOutText(0, px,py, Str);
  w:=fSize.X; h:=fSize.Y;
  if w*h > 0 then begin

    r:=4; Inc(w,r); Inc(h,r);

    bp[0]:=Point(-r,-r);
    bp[1]:=iTransit_3x3(w,-r,frot);
    bp[2]:=iTransit_3x3(w,h,frot);
    bp[3]:=iTransit_3x3(-r,h,frot);

    for i:=0 to 3 do
    with bp[i] do begin
      Inc(X,px); Inc(Y,py)
    end;

    bp[4]:=bp[0]; Result:=5
  end
end;

procedure dc_FillRect(dc: HDC; x1,y1,x2,y2, cl: Integer);
var
  bru: HBrush;
begin
  bru:=CreateSolidBrush(cl);
  FillRect(dc,Rect(x1,y1,x2,y2),bru);
  if bru <> 0 then DeleteObject(bru);
end;

function dc_ClipRect(dc: HDC; d: Integer): TRect;
var
  R: TRect;
begin
  GetClipBox(dc,@R);
  Result:=IncClipRect(R,d)
end;

procedure dc_Line(dc: HDC; x1,y1,x2,y2: Integer);
begin
  MoveToEx(DC,x1,y1,nil);
  LineTo(DC,x2,y2)
end;

procedure dc_PolyLine(dc: HDC; lp: PPoly; N: Integer);
var
  bx: Integer;
begin
  if N <= 2048 then
    PolyLine(dc,lp^,N)
  else

  while N > 0 do begin
    bx:=Min(N,2048);
    PolyLine(dc,lp^,bx);
    lp:=@lp[bx]; Dec(N,bx)
  end
end;

procedure dc_PolyPolyLine(dc: HDC;
                          lp: PPoly; lp_N: Integer;
                          cp: PIntegers; cp_N: Integer);
var
  i,n: Integer;
begin
  if cp = nil then
    PolyLine(dc,lp^,lp_N)
  else
    for i:=1 to cp_N do begin
      n:=cp[0]; cp:=@cp[1];
      dc_PolyLine(dc,lp,n);
      lp:=@lp[n]
    end
end;

function xGridRect(const R: TRect): TRect;
begin
  with R do
  Result:=Rect(Left+1,Top+1,Right-1,Bottom-1)
end;

function xClipRect(dc: TCanvas; Delta: Integer): TRect;
var
  R: TRect;
begin
  R:=dc.ClipRect;
  Result:=IncClipRect(R,Delta)
end;

procedure xResetCanvas(dc: TCanvas; pc,fc: TColor);
begin
  with dc do begin
    if pc <> clNone then begin
      Pen.Style:=psSOLID; Pen.Width:=1;
      Pen.Color:=pc;
    end;

    if fc <> clNone then begin
      Brush.Style:=bsSolid; Brush.Color:=fc
    end
  end
end;

procedure xBitBlt(Dst: TCanvas; DstR: PRect;
                  Src: TCanvas; From: PPoint);
var
  P: TPoint;
begin
  with DstR^ do begin
    if Assigned(From) then P:=From^
    else P:=Point(Left,Top);

    BitBlt(Dst.Handle, Left,Top,Right,Bottom,
           Src.Handle, P.X,P.Y, SRCCOPY)

  end
end;

procedure xSetPencil(dc: TCanvas; st: TPenStyle;
                     w: Integer; cl: TColor);
begin
  with dc.Pen do begin
    Style:=st; Width:=w; Color:=cl
  end
end;

procedure xThinPencil(dc: TCanvas; cl: TColor);
begin
  with dc.Pen do begin
    Style:=psSOLID; Width:=1; Color:=cl
  end
end;

procedure xThickPencil(dc: TCanvas; w: int; cl: TColor);
begin
  with dc.Pen do begin
    Style:=psSOLID; Width:=w; Color:=cl
  end
end;

procedure xFillCanvas(dc: TCanvas; cl: TColor);
var
  R: TRect;
begin
  with dc.ClipRect do
  R:=Rect(Left,Top,Right+1,Bottom+1);
  xResetCanvas(dc,clBlack,cl);
  dc.FillRect(R)
end;

procedure xPaintClip(dc: TCanvas; const Rect: TRect);
var
  rgn: hRGN;
begin
  SelectClipRgn(dc.Handle,0);

  with Rect do
  rgn:=CreateRectRgn(Left,Top,Right,Bottom);

  if rgn <> 0 then begin
    SelectClipRgn(dc.Handle,rgn);
    DeleteObject(rgn)
  end
end;

procedure xPaintExclude(dc: TCanvas; const Rect: TRect);
begin
  with Rect do
  ExcludeClipRect(dc.Handle,Left,Top,Right,Bottom)
end;

procedure xPaintRect(dc: TCanvas; const Rect: TRect);
begin
  with Rect do
  dc.Rectangle(Left,Top,Right,Bottom)
end;

procedure xPaintFrame(dc: TCanvas; const Rect: TRect);
begin
  with Rect do begin
    dc.Brush.Style:=bsClear;
    dc.Rectangle(Left-1,Top-1,Right+1,Bottom+1);
    dc.Rectangle(Left,Top,Right+2,Bottom+2);
    dc.Rectangle(Left+1,Top+1,Right+3,Bottom+3);
  end
end;

procedure xFrameLine(dc: TCanvas; x1,y1,x2,y2, cl1,cl2: Integer);
begin
  xThinPencil(dc,cl1);
  dc.MoveTo(x1,y2); dc.LineTo(x1,y1);
  dc.LineTo(x2,y1); dc.Pen.Color:=cl2;
  dc.LineTo(x2,y2); dc.LineTo(x1,y2);
end;

procedure xDownBox(dc: TCanvas; x1,y1,x2,y2,cl: Integer);
begin
  with dc do begin
    xResetCanvas(dc,clBlack,cl);
    Rectangle(x1+1,y1+1,x2,y2);

    Pen.Color:=clGray; MoveTo(x1,y2-1);
    LineTo(x1,y1); LineTo(x2,y1);
    Pen.Color:=clWhite; LineTo(x2,y2);
    LineTo(x1,y2)
  end
end;

procedure xFreeBox(dc: TCanvas; x1,y1,x2,y2,cl: Integer);
begin
  with dc do begin
    xResetCanvas(dc,clBlack,cl);
    Rectangle(x1+1,y1+1,x2,y2);

    Pen.Color:=clWhite; MoveTo(x1,y2-1);
    LineTo(x1,y1+2); LineTo(x2,y1+2);
    Pen.Color:=clGray; LineTo(x2,y2);
    LineTo(x1,y2)
  end
end;

procedure xButtBox(dc: TCanvas; x1,y1,x2,y2,cl: Integer);
begin
  with dc do begin
    xResetCanvas(dc,clBlack,cl);
    Rectangle(x1,y1,x2+1,y2+1);
    Inc(x1); Inc(y1); Dec(x2); Dec(y2);

    Pen.Color:=clGray; MoveTo(x2,y1);
    LineTo(x2,y2); LineTo(x1,y2);
    Pen.Color:=clWhite; LineTo(x1,y1);
    LineTo(x2,y1)
  end
end;

procedure xDownBox1(dc: TCanvas; x1,y1,x2,y2,cl: Integer);
begin
  with dc do begin
    xResetCanvas(dc,cl,cl);
    Rectangle(x1+1,y1+1,x2,y2);

    Pen.Color:=clBlack;
    MoveTo(x1,y2); LineTo(x1,y1);
    LineTo(x2,y1); Pen.Color:=clWhite;
    LineTo(x2,y2); LineTo(x1,y2);

    Inc(x1); Inc(y1); Dec(x2); Dec(y2);
    Pen.Color:=clGray; MoveTo(x1,y2);
    LineTo(x1,y1); LineTo(x2,y1);
  end
end;

procedure xButtBox1(dc: TCanvas; x1,y1,x2,y2,cl: Integer);
begin
  with dc do begin
    xResetCanvas(dc,cl,cl);
    Rectangle(x1,y1,x2+1,y2+1);

    Pen.Color:=clWhite;
    MoveTo(x1,y2); LineTo(x1,y1);
    LineTo(x2,y1); Pen.Color:=clBlack;
    LineTo(x2,y2); LineTo(x1,y2);

    Inc(x1); Inc(y1); Dec(x2); Dec(y2);
    Pen.Color:=clGray; MoveTo(x2,y1);
    LineTo(x2,y2); LineTo(x1,y2);
  end
end;

procedure xDownRect(dc: TCanvas; const R: TRect; cl: TColor);
begin
  with R do
  xDownBox(dc, Left,Top,Right-1,Bottom-1, cl)
end;

procedure xFreeRect(dc: TCanvas; const R: TRect; cl: TColor);
begin
  with R do
  xFreeBox(dc, Left,Top,Right-1,Bottom-1, cl)
end;

procedure xThinBox(dc: TCanvas; x1,y1,x2,y2,cl: Integer);
begin
  with dc do begin
    xResetCanvas(dc,clGray,cl);
    Rectangle(x1,y1,x2+1,y2+1);

    Pen.Color:=clWhite; MoveTo(x2,y1);
    LineTo(x2,y2); LineTo(x1,y2)
  end
end;

procedure uThinBox(dc: TCanvas; x1,y1,x2,y2,cl: Integer);
begin
  with dc do begin
    xResetCanvas(dc,clBlack,cl);
    Rectangle(x1,y1,x2+1,y2+1);

    Pen.Color:=clWhite; MoveTo(x1,y2-1);
    LineTo(x1,y1); LineTo(x2,y1)
  end
end;

procedure xHorz_dots(dc: TCanvas; X1,X2,dX,Y,Color: Integer);
var
  X: Integer;
begin
  X:=X1; while X <= X2 do begin
    dc.Pixels[X,Y]:=Color; Inc(X,dX)
  end
end;

procedure xVert_dots(dc: TCanvas; X,Y1,Y2,dY,Color: Integer);
var
  Y: Integer;
begin
  Y:=Y1; while Y <= Y2 do begin
    dc.Pixels[X,Y]:=Color; Inc(Y,dY)
  end
end;

procedure xDotLine(dc: TCanvas; x1,x2,y: Integer);
var
  cl: TColor;
begin
  with dc do begin
    cl:=Pen.Color; Pen.Color:=Brush.Color;
    MoveTo(x1,y); LineTo(x2,y); while x1 <= x2 do
    begin Pixels[x1,y]:=cl; Inc(x1,2) end
  end
end;

procedure xVertLine(dc: TCanvas; x,y1,y2: Integer);
begin
  with dc do begin
    MoveTo(x,y1); LineTo(x,y2)
  end
end;

procedure xHorzLine(dc: TCanvas; x1,x2,y: Integer);
begin
  with dc do begin
    MoveTo(x1,y); LineTo(x2,y)
  end
end;

procedure xDrawLine(dc: TCanvas; x1,y1,x2,y2: Integer);
begin
  with dc do begin
    MoveTo(x1,y1); LineTo(x2,y2)
  end
end;

procedure xVertDash(dc: TCanvas; x,y1,y2,dy: Integer);
begin
  while y1 < y2 do begin
    dc.MoveTo(x,y1); Inc(y1,dy);
    dc.LineTo(x,y1); Inc(y1,dy);
  end
end;

procedure xHorzDash(dc: TCanvas; x1,x2,y,dx: Integer);
begin
  while x1 < x2 do begin
    dc.MoveTo(x1,y); Inc(x1,dx);
    dc.LineTo(x1,y); Inc(x1,dx);
  end
end;

procedure xVertThick(dc: TCanvas; x,y1,y2,thick: Integer);
begin
  dc.FillRect(Rect(x,y1,x+thick,y2+1))
end;

procedure xHorzThick(dc: TCanvas; x1,x2,y,thick: Integer);
begin
  dc.FillRect(Rect(x1,y,x2+1,y+thick))
end;

procedure xDrawPoint(dc: TCanvas; x,y,d: Integer);
begin
  dc.MoveTo(x-d,y-d);
  dc.LineTo(x+d,y-d);
  dc.LineTo(x+d,y+d);
  dc.LineTo(x-d,y+d);
  dc.LineTo(x-d,y-d);
end;

procedure xMarkPoint(dc: TCanvas; x,y,d,cl: Integer);
begin
  with dc do begin
    Brush.Style:=bsClear;
    xThinPencil(dc,clBlack);

    Rectangle(x-d+1,y-d+1,x+d,y+d);
    Rectangle(x-d-1,y-d-1,x+d+2,y+d+2);

    if cl = 0 then cl:=clYellow; Pen.Color:=cl;
    Rectangle(x-d,y-d,x+d+1,y+d+1);
  end
end;

procedure xCrossPoint(dc: TCanvas; x,y,d: Integer);
var
  x1,y1,x2,y2: Integer;
begin
  x1:=x-d; y1:=y-d; x2:=x+d; y2:=y+d;

  if dc.Pen.Width > 1 then begin
    xDrawLine(dc, x1,y1,x2,y2);
    xDrawLine(dc, x1,y2,x2,y1)
  end
  else begin
    xDrawLine(dc, x1,y1,x2+1,y2+1);
    xDrawLine(dc, x1,y2,x2+1,y1-1)
  end
end;

procedure xCursorPoint(dc: TCanvas; x,y,d: Integer);
begin
  xHorzLine(dc,x-d,x+d,y);
  xVertLine(dc,x,y-d,y+d);
end;

procedure xEllipse(dc: TCanvas; x,y,r,cl,thick: Integer);
begin
  if cl <> clNone then dc.Pen.Color:=cl;
  if thick > 0 then begin
    dc.Pen.Style:=psSolid;
    dc.Pen.Width:=thick
  end;

  dc.Brush.Style:=bsClear;
  dc.Ellipse(x-r,y-r,x+r+1,y+r+1);
  dc.Brush.Style:=bsSolid;

  if thick > 0 then
  dc.Pen.Width:=1
end;

procedure xc_Cross(dc: TCanvas; x,y,d,cl: Integer);
begin
  xThinPencil(dc,cl);
  xCursorPoint(dc, x,y,d)
end;

procedure lb_Cross(dc: TCanvas; x,y,d,cl: Integer);
begin
  dc.Pen.Style:=psSOLID;
  dc.Pen.Width:=1;

  xRectangle(dc,x-d-1,y-1,x+d,y+1,clBlack);
  xRectangle(dc,x-1,y-d-1,x+1,y+d,clBlack);

  xc_Cross(dc, x,y,d,cl);
end;

procedure xc_Romb(dc: TCanvas; x,y,dx,dy,pc,bc: integer);
var
  p: TOrient;
begin
  p[0].x:=x-dx; p[0].y:=y;
  p[1].x:=x;    p[1].y:=y-dy;
  p[2].x:=x+dx; p[2].y:=y;
  p[3].x:=x;    p[3].y:=y+dy;
  p[4]:=p[0];

  xThinPencil(dc,pc);

  if bc <> clNone then begin
    dc.Brush.Color:=bc;
    Polygon(dc.handle,p,5,winding)
  end else

  PolyLine(dc.handle,p,5)
end;

procedure lb_Romb(dc: TCanvas; x,y,dx,dy,cl: integer);
begin
  xc_Romb(dc, x,y,dx+1,dy+1,clBlack,clNone);
  xc_Romb(dc, x,y,dx-1,dy-1,clBlack,clNone);
  xc_Romb(dc, x,y,dx,dy,cl,clNone);
end;

procedure lb_Rectangle(dc: TCanvas; x,y,d,cl: Integer);
var
  x1,y1,x2,y2: Integer;
begin
  x1:=x-d; y1:=y-d; x2:=x+d; y2:=y+d;
  xRectangle(dc,x1-1,y1-1,x2+1,y2+1,clBlack);
  xRectangle(dc,x1  ,y1  ,x2  ,y2  ,cl);
  xRectangle(dc,x1+1,y1+1,x2-1,y2-1,clBlack);
end;

procedure xDraw_Line(dc: HDC; x1,y1,x2,y2: Integer);
begin
  MoveToEx(dc, x1,y1,nil);
  LineTo(dc, x2,y2)
end;

procedure xDraw_Rect(dc: HDC; x1,y1,x2,y2: Integer);
begin
  xDraw_Line(dc,x1,y1,x2,y1);
  xDraw_Line(dc,x1,y2,x2,y2);
  xDraw_Line(dc,x1,y1,x1,y2);
  xDraw_Line(dc,x2,y1,x2,y2);
end;

procedure xCross_Point(dc: HDC; x,y,d: Integer);
var
  x1,y1,x2,y2: Integer;
begin
  x1:=x-d; y1:=y-d; x2:=x+d; y2:=y+d;
  xDraw_Line(dc, x1,y1,x2+1,y2+1);
  xDraw_Line(dc, x1,y2,x2+1,y1-1)
end;

procedure xRay(dc: TCanvas; xc,yc: Integer;
               r1,r2,f: Double; cl: integer);
var
  s,c: Extended;
begin
  if cl <> clNone then dc.Pen.Color:=cl;

  SinCos(f, s,c);
  dc.MoveTo(Round(xc + (c * r1)),
            Round(yc + (s * r1)));

  dc.LineTo(Round(xc + (c * r2)),
            Round(yc + (s * r2)));
end;

procedure xSector(dc: TCanvas; xc,yc: Integer;
                  r,f1,f2: Double; cl: integer;
                  lp: PLPoly);
var
  i,x,y: Integer; kf: Double; s,c: Extended;
begin
  if cl <> clNone then dc.Pen.Color:=cl;

  kf:=(f2-f1)/64;

  for i:=0 to 64 do begin
    SinCos(f1 + i*kf, s,c);

    x:=Round(xc + (c * r));
    y:=Round(yc + (s * r));

    if i = 0 then dc.MoveTo(x,y)
    else dc.LineTo(x,y);

    if i = 0 then
    if Assigned(lp) then lp[0]:=Point(x,y)
  end;

  if Assigned(lp) then lp[1]:=Point(x,y)
end;

procedure fSector(dc: HDC; xc,yc: Integer;
                  r,f1,f2: Double);
var
  i,n,x,y: Integer; kf: Double;
  s,c: Extended; l: TLPoly;
begin
  kf:=(f2-f1)/16;

  l[0]:=Point(xc,yc); n:=1;

  for i:=0 to 16 do begin
    SinCos(f1 + i*kf, s,c);

    x:=Round(xc + (c * r));
    y:=Round(yc + (s * r));
    l[n]:=Point(x,y); Inc(n);
  end;

  l[n]:=Point(xc,yc); Inc(n);
  Polygon(DC,l,n,winding)
end;

procedure xRectangle(dc: TCanvas; x1,y1,x2,y2,cl: integer);
begin
  with dc do begin
    if cl <> clNone then Pen.Color:=cl;
    MoveTo(x1,y1); LineTo(x2,y1); LineTo(x2,y2);
    LineTo(x1,y2); LineTo(x1,y1)
  end
end;

procedure xFocusRect1(dc: TCanvas; x1,y1,x2,y2,dr, cl: integer);
begin
  with dc do begin
    if cl <> clNone then Pen.Color:=cl;
    MoveTo(x1,y1+dr); LineTo(x1,y1); LineTo(x1+dr,y1);
    MoveTo(x2-dr,y1); LineTo(x2,y1); LineTo(x2,y1+dr);
    MoveTo(x2,y2-dr); LineTo(x2,y2); LineTo(x2-dr,y2);
    MoveTo(x1+dr,y2); LineTo(x1,y2); LineTo(x1,y2-dr);
  end
end;

procedure xFillMask(dc: TCanvas; x1,y1,x2,y2, msk,cl: integer);
var
  x,y: integer; mask: TFillMask;
begin
  mask:=cFillMask[msk and 15];
  for y:=y1 to y2 do begin
    msk:=mask[y and 15]; for x:=x1 to x2 do
    if msk and HexBit[x and 15] > 0 then
    dc.Pixels[x,y]:=cl
  end
end;

procedure xDisp_line(dc: TCanvas; const a,b: TPoint; cl: Integer);
var
  lt,rb, _a,_b: TPoint;
begin
  with xClipRect(dc,64) do begin
    lt:=TopLeft; rb:=BottomRight;
  end;

  if xClip_Line(lt,rb, a,b, _a,_b) then begin
    if cl <> clNone then xThinPencil(dc,cl);
    dc.MoveTo(_a.X,_a.Y); dc.LineTo(_b.X,_b.Y)
  end;
end;

procedure xDisp_thin(dc: TCanvas; lp: PLPoly; n,w,cl: Integer);
var
  i: Integer; R: TRect;
  lt,rb, a,b, p1,p2: TPoint;
begin
  if n > 0 then begin
    with xClipRect(dc,64) do begin
      lt:=TopLeft; rb:=BottomRight
    end;

    if cl <> clNone then
    xSetPencil(dc,psSolid,w,cl);

    p2:=lp[0]; for i:=1 to n do begin
      p1:=p2; p2:=lp[i];
      if xClip_Line(lt,rb, p1,p2, a,b) then begin
        dc.MoveTo(a.X,a.Y);
        dc.LineTo(b.X,b.Y);
      end
    end
  end
end;

procedure xDisp_frame(dc: TCanvas; lp: PLPoly; n: Integer);
begin
  if n > 0 then begin
    xDisp_thin(dc,lp,n,3,clLtGray);
    xDisp_thin(dc,lp,n,1,clYellow);
  end
end;

procedure xDisp_bound(dc: TCanvas; lp: PLPoly; n,cl: Integer);
var
  I: Integer; R: TRect;
  lt,rb, a,b, p1,p2: TPoint;
begin
  if n > 0 then begin
    R:=xClipRect(dc,64);
    lt:=Point(R.Left,R.Top);
    rb:=Point(R.Right,R.Bottom);

    xSetPencil(dc,psSolid,3,clBlack);
    p2:=lp[0]; for I:=1 to n do begin
      p1:=p2; p2:=lp[I];
      if xClip_Line(lt,rb, p1,p2, a,b) then
      xDrawLine(dc, a.x,a.y,b.x,b.y);
    end;

    xThinPencil(dc,cl);
    p2:=lp[0]; for I:=1 to n do begin
      p1:=p2; p2:=lp[I];
      if xClip_Line(lt,rb, p1,p2, a,b) then
      xDrawLine(dc, a.x,a.y,b.x,b.y);
    end;
  end
end;

procedure xDisp_track(dc: TCanvas; lp: PLPoly; n: Integer);
var
  i: Integer;
  lt,rb,a,b,_a,_b: TPoint;
  dn: Boolean;
begin
  if n > 0 then begin

    with xClipRect(dc,64) do begin
      lt:=TopLeft; rb:=BottomRight
    end;

    dn:=false; b:=lp[0];

    for i:=1 to n-1 do begin
      a:=b; lp:=@lp[1]; b:=lp[0];

      if not Points_Equal(a,b) then
      if PortContainsPoint(lt,rb, b.x,b.y) then begin

        if dn then
          dc.LineTo(b.x,b.y)
        else
        if xClip_Line(lt,rb, a,b, _a,_b) then begin
          dc.MoveTo(_a.x,_a.y);
          dc.LineTo(_b.x,_b.y); dn:=true
        end

      end
      else begin
        if xClip_Line(lt,rb, a,b, _a,_b) then begin
          dc.MoveTo(_a.x,_a.y);
          dc.LineTo(_b.x,_b.y);
        end;

        dn:=false
      end
    end
  end
end;

procedure xDraw_arrow(dc: TCanvas; const a,b: TPoint; r: Integer);
var
  fi,df: Double; p1,p2: TPoint;
begin
  fi:=ArcTan2(b.Y-a.Y,b.X-a.X); df:=4/5*Pi;

  p1:=prj_LPoint(b,r,fi-df);
  p2:=prj_LPoint(b,r,fi+df);

  dc.MoveTo(p1.X,p1.Y);
  dc.LineTo(b.X,b.Y);
  dc.LineTo(p2.X,p2.Y);
end;

procedure xDraw_arrow1(dc: TCanvas; const a,b: TPoint; r1,r2: Integer);
var
  dx,dy,cl,w: Integer; fi,df,k: Double; l: LOrient;
begin
  dx:=b.X-a.X; dy:=b.Y-a.Y;
  if (dx = 0) and (dy = 0) then dx:=1;

  fi:=ArcTan2(dy,dx); df:=4/5*Pi;

  k:=r2/Hypot(dx,dy);

  l[0]:=prj_LPoint(b,r1,fi-df);
  l[2]:=prj_LPoint(b,r1,fi+df);
  l[3].X:=b.X - Round(dx*k);
  l[3].Y:=b.y - Round(dy*k);
  l[1]:=b; l[4]:=l[0];

  cl:=dc.Brush.Color;
  w:=dc.Pen.Width;

  dc.Pen.Width:=1;
  dc.Brush.Color:=dc.Pen.Color;

  Polygon(dc.Handle,l,5,winding);

  dc.Pen.Width:=w;
  dc.Brush.Color:=cl;
end;

procedure fTriangle(dc: TCanvas;
                    xc,yc, dx,up,dn, dy: integer;
                    fi: Float; pc,bc: TColor);
var
  p: LOrient; tr: Real3x3;
  i,n,x1,x2,y1,y2: Integer;
begin
  x1:=xc-dx; x2:=xc+dx;
  y1:=yc-up; y2:=yc+dn;

  p[0]:=Point(x1,y2); i:=1;

  if dy > 0 then begin
    p[i]:=Point(xc,y2-dy); Inc(i);
  end;

  p[i]:=Point(x2,y2); Inc(i);
  p[i]:=Point(xc,y1); Inc(i);
  p[i]:=p[0]; n:=i+1;

  begin_3x3(tr,-xc,-yc);
  fi_Rotate_3x3(tr,fi);
  t_Move_3x3(tr,xc,yc);

  for i:=0 to n-1 do with p[i] do
  p[i]:=iTransit_3x3(X,Y,tr);

  if pc <> clNone then dc.Pen.Color:=pc;
  if bc <> clNone then dc.Brush.Color:=bc;

  Polygon(dc.handle,p,n,winding);
  PolyLine(dc.handle,p,n)
end;

procedure xTriangle(dc: TCanvas; x1,x2,y1,y2: int; pc,bc: TColor);
var
  p: TOrient;
begin
  p[0].X:=x1; p[0].Y:=y2; p[1].X:=x2; p[1].Y:=y2;
  p[2].X:=Round(x1/2 + x2/2); p[2].Y:=y1; p[3]:=p[0];

  if pc <> clNone then dc.Pen.Color:=pc;
  if bc <> clNone then dc.Brush.Color:=bc;

  Polygon(dc.handle,p,4,winding);
  PolyLine(dc.handle,p,4)
end;

procedure lTriangle(dc: TCanvas; x1,x2,y1,y2: int; pc,bc: TColor);
var
  p: TOrient;
begin
  p[0].X:=x1; p[0].Y:=y1; p[1].X:=x1; p[1].Y:=y2;
  p[2].X:=x2; p[2].Y:=Round(y1/2 + y2/2); p[3]:=p[0];

  if pc <> clNone then dc.Pen.Color:=pc;
  if bc <> clNone then dc.Brush.Color:=bc;

  Polygon(dc.handle,p,4,winding);
  PolyLine(dc.handle,p,4)
end;

procedure rTriangle(dc: TCanvas; x1,x2,y1,y2: int; pc,bc: TColor);
var
  p: TOrient;
begin
  p[0].X:=x2; p[0].Y:=y1; p[1].X:=x2; p[1].Y:=y2;
  p[2].X:=x1; p[2].Y:=Round(y1/2 + y2/2); p[3]:=p[0];

  if pc <> clNone then dc.Pen.Color:=pc;
  if bc <> clNone then dc.Brush.Color:=bc;

  Polygon(dc.handle,p,4,winding);
  PolyLine(dc.handle,p,4)
end;

procedure xPunkt(dc: TCanvas; x1,x2,y1,y2, cl: integer);

procedure xDraw(dc: TCanvas; x1,x2,y1,y2, cl: integer);
var
  p: TOrient;
begin
  p[0].x:=x1; p[0].y:=y2; p[1].x:=x2; p[1].y:=y2;
  p[2].x:=Round(x1/2 + x2/2); p[2].y:=y1; p[3]:=p[0];
  dc.Pen.Color:=cl; PolyLine(dc.handle,p,4);
end;

begin
  xDraw(dc,x1,x2,y1,y2,clBlack);
  xDraw(dc,x1+4,x2-4,y1+4,y2-2,clBlack);
  xDraw(dc,x1+2,x2-2,y1+2,y2-1,cl);
end;

procedure xPunkt1(dc: TCanvas; x1,x2,y1,y2, cl: integer);

procedure xDraw(dc: TCanvas; x1,x2,y1,y2, cl: integer);
var
  p: TOrient;
begin
  p[0].x:=x1; p[0].y:=y1; p[1].x:=x2; p[1].y:=y1;
  p[2].x:=Round(x1/2 + x2/2); p[2].y:=y2; p[3]:=p[0];
  dc.Pen.Color:=cl; PolyLine(dc.handle,p,4);
end;

begin
  xDraw(dc,x1,x2,y1,y2,clBlack);
  xDraw(dc,x1+2,x2-2,y1+2,y2-2,clBlack);
  xDraw(dc,x1+1,x2-1,y1+1,y2-1,cl);
end;

procedure xRomb(dc: TCanvas; x1,x2,y,dx,dy: integer; pc,bc: TColor);
var
  p: TOrient;
begin
  p[0].x:=x1;    p[0].y:=y;
  p[1].x:=x1+dx; p[1].y:=y+dy;
  p[2].x:=x2+dx; p[2].y:=y+dy;
  p[3].x:=x2;    p[3].y:=y;

  dc.Pen.Color:=pc; dc.Brush.Color:=bc;
  p[4]:=p[0];

  Polygon(dc.handle,p,4,winding);
  PolyLine(dc.handle,p,5)
end;

procedure yRomb(dc: TCanvas; x,y1,y2,dx,dy: integer; pc,bc: TColor);
var
  p: tOrient;
begin
  if y1 < y2 then begin
    p[0].x:=x;    p[0].y:=y1;
    p[1].x:=x;    p[1].y:=y2;
    p[2].x:=x+dx; p[2].y:=y2+dy;
    p[3].x:=x+dx; p[3].y:=y1+dy;

    dc.Pen.Color:=pc; dc.Brush.Color:=bc;
    p[4]:=p[0];

    Polygon(dc.handle,p,4,winding);
    PolyLine(dc.handle,p,5)
  end
end;

procedure vRomb(dc: TCanvas; x1,y1,x2,y2,dx,dy: integer;
                lc,fc,uc,rc: TColor);
begin
  xRomb(dc,x1,x2,y2,dx,dy,lc,uc);
  xRomb(dc,x1,x2,y1,dx,dy,lc,uc);

  dc.Brush.Color:=fc;
  dc.Rectangle(x1,y1,x2+1,y2+1);
  yRomb(dc,x2,y1,y2,dx,dy,lc,rc);
end;

procedure xDrawMarker(dc: TCanvas; x,y,d,cl: integer);
begin
  with dc do begin
    xThinPencil(dc,clBlack);
    Brush.Style:=bsSOLID; Brush.Color:=cl;
    Rectangle(x-d,y-d,x+d+1,y+d+1)
  end
end;

procedure xHideMarker(dc: TCanvas; x,y,d,cl: integer);
begin
  with dc do begin
    Brush.Style:=bsSOLID; Brush.Color:=cl;
    FillRect(Rect(x-d,y-d,x+d+1,y+d+1))
  end
end;

procedure xLinkMarker(dc: TCanvas; x,y,d,cl: integer);
begin
  xRectangle(dc,x-d+1,y-d+1,x+d-1,y+d-1,clBlack);
  xRectangle(dc,x-d  ,y-d  ,x+d  ,y+d  ,cl);
  xRectangle(dc,x-d-1,y-d-1,x+d+1,y+d+1,clBlack)
end;

procedure xFlagMarker(dc: TCanvas; X,Y,W,H,Color: Integer);
var
  y1,y2,x2: Integer; l: LOrient;
begin
  if Color <> clNone then
  xResetCanvas(dc,clBlack,Color);

  y1:=Y-H; x2:=X+W;
  y2:=y1 + H * 3 div 4;

  dc.Rectangle(X,y1,X+2,Y+1);
  dc.Rectangle(X-2,Y,X+4,Y+2);

  Inc(X,1);
  l[0]:=Point(X,y1);
  l[1]:=Point(x2,(y1+y2) div 2);
  l[2]:=Point(X,y2); l[3]:=l[0];

  Polygon(dc.Handle,l,4,winding)
end;

procedure xDrawPixel(dc: TCanvas; x,y,r: int);
begin
  xRectangle(DC,x-r,y-r,x+r,y+r,clWhite)
end;

procedure xRectPixel(dc: TCanvas; x,y,r,cl: int);
begin
  xRectangle(DC,x-r,y-r,x+r,y+r,cl)
end;

procedure xDrawRect(dc: TCanvas; x,y,r: integer);
begin
  xRectangle(DC,x-r,y-r,x+r,y+r,clNone);
  if dc.Pen.Width = 2 then begin
    dc.Pixels[x-r-1,y-r-1]:=dc.Pen.Color;
    dc.Pixels[x-r,y-r-1]:=dc.Pen.Color;
  end
end;

procedure xDrawTarget(dc: TCanvas; x,y: integer);
begin
  xDrawLine(dc,x-16,y,x-4,y); xDrawLine(dc,x+4,y,x+16,y);
  xDrawLine(dc,x,y-16,x,y-4); xDrawLine(dc,x,y+4,x,y+16)
end;

procedure xDrawLocator(dc: TCanvas; x,y,d: integer);
var
  x1,y1,x2,y2: integer;
begin
  x1:=x-d; y1:=y-d; x2:=x+d; y2:=y+d;

  xDrawLine(dc,x1,y1,x1+3,y1); xDrawLine(dc,x2-3,y1,x2,y1);
  xDrawLine(dc,x1,y2,x1+3,y2); xDrawLine(dc,x2-3,y2,x2,y2);

  xDrawLine(dc,x1,y1,x1,y1+3); xDrawLine(dc,x1,y2-3,x1,y2);
  xDrawLine(dc,x2,y1,x2,y1+3); xDrawLine(dc,x2,y2-3,x2,y2);
end;

procedure xDrawCursor(dc: TCanvas; x,y,d: integer);
begin
  xDrawLine(dc,x-d,y,x+d,y);
  xDrawLine(dc,x,y-d,x,y+d);
end;

procedure xThickCursor(dc: TCanvas; x,y,r: Integer);
var
  w,t: Integer;
begin
  w:=dc.Pen.Width;
  if w <= 1 then begin
    dc.MoveTo(x-r,y); dc.LineTo(x+r,y);
    dc.MoveTo(x,y-r); dc.LineTo(x,y+r);
  end
  else begin
    t:=Max(1,w div 2);
    if t*2 < w then Inc(t);
    dc.Brush.Color:=dc.Pen.Color;

    dc.Pen.Width:=1;
    dc.Rectangle(x-r,y-t,x+r,y+t);
    dc.Rectangle(x-t,y-r,x+t,y+r);
    dc.Pen.Width:=w
  end
end;

procedure xDrawMovePoint(dc: TCanvas; x1,y1,x2,y2,d: integer);
begin
  xDrawPixel(dc,x1,y1,d);
  dc.MoveTo(x1,y1); dc.LineTo(x2,y2);
  xDrawTarget(dc,x2,y2)
end;

procedure xDrawMovePixel(dc: TCanvas; x1,y1,x2,y2,d: integer);
begin
  xDrawPixel(dc,x1,y1,d);
  dc.MoveTo(x1,y1); dc.LineTo(x2,y2);
  xDrawPixel(dc,x2,y2,d)
end;

procedure xDrawShape(dc: TCanvas;
                     cx,cy: Integer; fi,k: Double;
                     typ, a,b1,b2: Integer; cl: TColor);
var
  i,n: Integer; c,p: TPoint;
  l: LOrient; tr: Real3x3;
begin
  if typ = 1 then begin
    l[0].X:=-a; l[0].Y:=b1;
    l[1].X:=-a; l[1].Y:=-b2;
    l[2].X:= a; l[2].Y:=-b2;
    l[3].X:= a; l[3].Y:=b1;
    n:=4;
  end
  else begin
    l[0].X:=-a; l[0].Y:=b1;
    l[1].X:= 0; l[1].Y:=-b2;
    l[2].X:= a; l[2].Y:=b1;
    n:=3;
  end;

  tr:=Rotate_3x3(fi);

  if Abs(k-1) > 0.01 then
  t_Scale_3x3(tr,k);
  
  tr[1,3]:=cx; tr[2,3]:=cy;

  for i:=0 to n-1 do with l[i] do
  l[i]:=iTransit_3x3(X,Y,tr); l[n]:=l[0];

  if cl = clNone then
    Polyline(dc.Handle,l,n+1)
  else begin
    xResetCanvas(dc,clBlack,cl);
    Polygon(dc.Handle,l,n+1,winding);
    dc.Brush.Color:=clWhite
  end
end;

procedure xDrawIcon(dc: TCanvas; Id, x,y,w,h, mode: Integer);
var
  MemDC: HDC; old,bit: hBitMap;
begin
  bit:=LoadBitmap(HInstance,MakeIntResource(Id));
  MemDC:=CreateCompatibleDC(DC.handle); old:=SelectObject(MemDC,bit);
  BitBlt(dc.handle,x,y,w,h,MemDC,0,0,mode); SelectObject(MemDC,old);
  DeleteDC(MemDC); DeleteObject(bit)
end;

procedure xDrawTool(dc: TCanvas; Id, x,y,w,h: Integer);
var
  t: tBitMap;
begin
  t:=tBitMap.Create;
  t.handle:=LoadBitmap(HInstance,MakeIntResource(Id));

  dc.BrushCopy(Rect(x,y,x+w,y+h),t,
  Rect(0,0,w,h),t.TransparenTColor);

  t.Free;
end;

function xSwapRect(const R: TRect): TRect;
begin
  Result:=R;

  if R.Left > R.Right then begin
    Result.Left:=R.Right; Result.Right:=R.Left
  end;

  if R.Top > R.Bottom then begin
    Result.Top:=R.Bottom; Result.Bottom:=R.Top
  end
end;

function xTruncStr(dc: TCanvas; Width: Integer; var s: string): Integer;
begin
  while length(s) > 1 do begin
    if dc.TextWidth(s) < Width then Break;
    System.Delete(s,1,1)
  end;

  Result:=length(s)
end;

function rTruncStr(dc: TCanvas; const R: TRect; var s: string): Integer;
begin
  Result:=xTruncStr(dc,R.Right-R.Left-8,s)
end;

procedure OutValue(dc: HDC; x,y,h,w,fc,bc: integer; FName,Str: PChar);
var
  ix,iy,cl: int; fnt,old: HFont; lfont: TLogFont;
begin
  Fillchar(lfont,Sizeof(lfont),0);
  with lfont do begin
    lfHeight:=h; lfWeight:=w;

    lfCharSet:=DEFAULT_CHARSET;
    lfQuality:=Proof_Quality;

    StrPCopy(lfFaceName,FName);
  end;

  fnt:=CreateFontIndirect(lfont);
  old:=SelectObject(dc,fnt);

  SetTextColor(dc,bc);

  for iy:=-1 to +1 do
  for ix:=-1 to +1 do
  if (ix <> 0) or(iy <> 0) then
  TextOut(dc,x+ix,y+iy,Str,Strlen(Str));

  SetTextColor(dc,fc);

  TextOut(dc,x,y,Str,Strlen(Str));

  SelectObject(dc,old);
  DeleteObject(fnt);
end;

procedure xOutTextA(dc: TCanvas; x,y,h,w,fi,fc: integer; FName,Str: PChar);
var
  plot: TOutText;
begin
  plot:=TOutText.Create(dc, h,w,fi,fc, FName);
  try
    plot.OutText(x,y,clNone,Str)
  finally
    plot.Free
  end
end;

function xOutText(dc: TCanvas; x,y: integer; const s: string): Integer;
begin
  dc.TextOut(x,y,s);
  Result:=x + dc.TextWidth(s)
end;

procedure xTextOut(dc: TCanvas; x,y,flags: integer; s: string);
var
  sz: TSize;
begin
  sz:=dc.TextExtent(s);

  if flags and txt_up <> 0 then Dec(y,sz.cy);
  if flags and txt_cx <> 0 then Dec(x,sz.cx div 2);
  if flags and txt_cy <> 0 then Dec(y,sz.cy div 2);
  if flags and txt_rx <> 0 then Dec(x,sz.cx);

  dc.TextOut(x,y,s);
end;

procedure fTextOut(dc: TCanvas; x,y,bc,fc: integer; s: string);
var
  i,j: int;
begin
  dc.Font.Color:=bc;
  for j:=-1 to +1 do
  for i:=-1 to +1 do
  dc.TextOut(x+i,y+j,s);

  dc.Font.Color:=fc;
  dc.TextOut(x,y,s);
end;

procedure xTextPoint(dc: TCanvas; x,y: integer; s: string);
var
  sz: TSize;
begin
  sz:=dc.TextExtent(s);
  dc.TextOut(x - sz.cx div 2,y - sz.cy div 2,s);
end;

procedure yTextOut(dc: TCanvas; x,y1,y2: integer; s: string);
begin
  dc.TextOut(x,y1+(y2-y1+1-dc.TextHeight(s)) div 2,s);
end;

procedure yTextRect(dc: TCanvas; const R: TRect; const s: String);
var
  h,y: int;
begin
  h:=dc.TextHeight(s);
  y:=(R.Top + R.Bottom - h) div 2;
  dc.TextRect(R,R.Left,y,s)
end;

procedure xTextRect(dc: TCanvas; x1,y1,x2,y2,d: int; const s: string);
var
  x,y: int; ts: TSize; r: TRect;
begin
  r.Left:=x1+d; r.Right:=x2-d+1;
  r.Top:=y1+d; r.Bottom:=y2-d+1;

  ts:=dc.TextExtent(s);
  x:=x1+(x2-x1+1-ts.cx) div 2;
  y:=y1+(y2-y1+1-ts.cy) div 2;

  if (x >= r.Left) and (y >= r.Top) then
  dc.TextRect(r,x,y,s);
end;

procedure cTextRect(dc: TCanvas; const r: TRect; d: int; const s: string);
var
  x,y: int; ts: TSize; _r: TRect;
begin
  ts:=dc.TextExtent(s);

  _r:=r;

  with _r do begin

    if d > 0 then begin
      Inc(_r.Left,d); Dec(_r.Right,d);
      Inc(_r.Top,d);  Dec(_r.Bottom,d)
    end;

    x:=Left + (Right-Left-ts.cx) div 2;
    y:=Top  + (Bottom-Top-ts.cy) div 2
  end;

  dc.TextRect(_r,x,y,s)
end;

procedure xTextDown(dc: TCanvas; x,y,w,h: integer; s: string);
var
  i,iy,dy,dx: integer;
begin
  dy:=0; for i:=1 to length(s) do
  Inc(dy,dc.TextHeight(s[i]));

  if dy <= h then begin
    iy:=(h-dy) div 2; dc.Brush.Style:=bsClear;

    for i:=1 to length(s) do begin
      dx:=dc.TextWidth(s[i]);
      dc.TextOut(x + (w-dx) div 2,y+iy,s[i]);
      Inc(iy,dc.TextHeight(s[i]))
    end;

    dc.Brush.Style:=bsSOLID
  end
end;

procedure xTextDraw(dc: TCanvas; x,y,f: integer; s: string);
var
  lfont: tLogFont; Metrics: TTextMetric;
  old,fnt: HFont;
begin
  if GetTextMetrics(dc.handle,Metrics) then begin

    with lFont do begin
      lfHeight:=Metrics.tmHeight;
      lfWidth :=0;

      lfEscapement :=f*10; { ROTATION! in 0.1 deg }
      lfOrientation:=f*10;

      lfWeight:=Metrics.tmWeight;
      lfItalic:=Metrics.tmItalic;

      lfUnderline:=Metrics.tmUnderlined;
      lfStrikeOut:=Metrics.tmStruckOut;

      lfCharSet:=Metrics.tmCharSet;

      lfOutPrecision  :=Out_Default_Precis;
      lfClipPrecision :=Clip_Default_Precis;
      lfQuality       :=Proof_Quality;
      lfPitchAndFamily:=Metrics.tmPitchAndFamily;
      StrPCopy(lfFaceName,dc.Font.Name);
    end;

    fnt:=CreateFontIndirect(lFont);
    old:=SelectObject(dc.handle, fnt);
    dc.TextOut(x,y,s); SelectObject(dc.handle,old);
    DeleteObject(fnt)
  end;
end;

function xTextCentre(dc: TCanvas; x1,x2,y: integer; s: string): Integer;
begin
  dc.TextOut(x1+(x2-x1-dc.TextWidth(s)) div 2,y,s);
  Result:=y+dc.TextHeight(s)
end;

function xTextRight(dc: TCanvas; x1,x2,y: integer;
                    const s: string): Integer;
begin
  dc.TextOut(Max(x1,x2-dc.TextWidth(s)),y,s);
  Result:=y+dc.TextHeight(s)
end;

procedure xTextAxe(dc: TCanvas; x1,x2,y: integer; s: string);
var
  p: TSize;
begin
  p:=dc.TextExtent(s);
  dc.TextOut(Max(x1,x2-p.cx),y-p.cy div 2,s)
end;

procedure xTextTrunc(dc: TCanvas; x1,x2,y: integer; s: string);
begin
  while dc.TextWidth(s) > (x2-x1) do s[0]:=pred(s[0]);
  dc.TextOut(x1+(x2-x1-dc.TextWidth(s)) div 2,y,s);
end;

function xVertText(dc: TCanvas; x1,y1,y2: integer;
                   s: string; var R: TRect): boolean;
var
  x2,t: word; p: tOrient;
begin
  Result:=false;
  R.Left:=x1+1; R.Top:=y2;
  R.Right:=x1; R.Bottom:=y1;

  if length(s) > 0 then begin

    t:=dc.TextWidth('0');
    if y1+dc.TextWidth(s)+t+t < y2 then begin
      x2:=x1+dc.TextHeight(s)+1;

      p[0].x:=x1; p[0].y:=y2;
      p[1].x:=x1; p[1].y:=y1;
      p[2].x:=x2; p[2].y:=y1+t;
      p[3].x:=x2; p[3].y:=y2-t;

      dc.Pen.Width:=1;
      dc.Brush.Color:=clWhite;
      Polygon(dc.handle,p,4,winding);
      xTextDraw(dc,x1+1,y2-t,90,s);

      R.Left:=x1; R.Top:=p[2].y;
      R.Right:=x2; R.Bottom:=p[3].y;

      Result:=true
    end
  end
end;

procedure xTextRomb(dc: TCanvas; x,y1,y2, dx,dy,d: integer; s: string);
var
  i,h, tw,th: integer; ch: char;
begin
  tw:=dc.TextWidth(s)+d;
  th:=dc.TextHeight(s)+d; h:=y2-y1;

  if (tw <= dx) and (th <= h) then begin
    tw:=(dx-tw) div 2; Inc(x,tw);
    Inc(y1,(h-th) div 2 - tw);

    for i:=1 to length(s) do begin ch:=s[i];
      dc.TextOut(x,y1,ch); tw:=dc.TextWidth(ch);
      Inc(x,tw); Dec(y1,tw)
    end
  end
end;

procedure xTextHint(dc: TCanvas;
                    x,y,d1,d2,left,up: int;
                    bc,pc,fc: int;
                    Str: PChar);
var
  sz: TSize; x1,y1,x2,y2,dx,dy: int;
begin
  sz:=dc.TextExtent(Str);

  if bc = clNone then
    xResetCanvas(dc,clBlack,clWhite)
  else
    xResetCanvas(dc,pc,bc);

  x1:=x-d1; x2:=x+sz.cx+d1;
  y1:=y-d1; y2:=y+sz.cy+d1;

  dx:=d2; dy:=d2;
  if left = 1 then dx:=-(x2-x1+d2);
  if up = 1 then dy:=-(y2-y1+d2);

  Inc(x1,dx); Inc(y1,dy);
  Inc(x2,dx); Inc(y2,dy);

  dc.Rectangle(x1,y1,x2,y2);

  if fc <> clNone then
  dc.Font.Color:=fc;

  dc.TextOut(x1+d1,y1+d1,Str)
end;

function xHi(h: byte): byte;
begin
  Result:=(h and 31) shl 3
end;

function xRGB(r,g,b: Integer): TColor;
begin
  Result:=r or (g shl 8) or (b shl 16)
end;

end.