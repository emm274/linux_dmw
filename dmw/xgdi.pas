unit xgdi; interface

uses
  Classes,Graphics,
  LCLType,LCLIntf,Math,
  Windows,otypes,xline;

type
  TYCbCr = record Y,Cb,Cr: Double end;
  TYIQ = record Y,I,Q: Double end;
  TRGB = record R,G,B: Double end;

  TGammaParams = record
    typ: Integer; gamma: Float;
    curve: TGLine; chan: TChannel
  end;

  TImageGamma = record Index: Integer;
    Gamma: Array[0..3] of TGammaParams;
  end;

const
  RGB_YIQ_T: Real3x3 =
    ((0.299,  0.587,  0.114),
     (0.596, -0.274, -0.322),
     (0.212, -0.523,  0.311));

  YIQ_RGB_T: Real3x3 =
    ((1.0,  0.956,  0.621),
     (1.0, -0.272, -0.647),
     (1.0, -1.105,  1.702));

  BGR_YIQ_T: Real3x3 =
    ((0.114,  0.587,  0.299),
     (-0.322, -0.274, 0.596),
     (0.311, -0.523,  0.212));

  YIQ_BGR_T: Real3x3 =
    ((1.0, -1.105,  1.702),
     (1.0, -0.272, -0.647),
     (1.0,  0.956,  0.621));

  RGB_YUV_T: Real3x3 =
    (( 0.299,  0.587,  0.114),
     (-0.147, -0.289,  0.437),
     ( 0.615, -0.515, -0.100));

  YUV_RGB_T: Real3x3 =
    ((1.0,  0.0,    1.140),
     (1.0, -0.394, -0.581),
     (1.0,  2.028,  0.0));

   icb_square = 1;
   icb_invert = 2;
   icb_gamma  = 4;

type
  PImageParams = ^TImageParams;
  TImageParams = record
    Grays,Percent,Black,White,Flags: integer;
    im_w,im_h,min_cl,max_cl: Integer;
    hist: TRGB_Hist; chan: TChannel;
    gamma: TImageGamma
  end;

function win_to_rgb(Color: TColor): TColor;

procedure gamma_init(var Params: TGammaParams);

procedure Fill_gamma_quads(Quads: PRGBQuads;
                           const Params: TGammaParams);

procedure GetGammaChannels(const Gamma: TImageGamma;
                           out ch0,ch1,ch2,ch3: PBytes);

procedure Init_Channel(chan: PBytes; Count: Integer);
procedure Init_Gray_Channel(chan: PBytes);

procedure Gray_Channel(chan: PBytes; Hist: PIntegers;
                       Left,Right,Count,Flags,hi1,hi2: Integer);

function Hist_Channel(chan: PBytes; Hist: PIntegers;
                      Count,Flags,hi1,hi2: Integer;
                      Threshold: Float): TPoint;

function Fill_Channel(chan: PBytes; Hist: PIntegers;
                      Count,Percent,Flags,hi1,hi2: Integer): TPoint;

function Gray_vga_channel(icb: PImageParams): Integer;
function Fill_vga_channel(icb: PImageParams): Integer;

function Fill_gray_quads(Quads: PRGBQuads;
                         icb: PImageParams): Integer;

function Color_Quad(cl: TColor): TRGBQuad;
function Quad_Color(qu: TRGBQuad): TColor;
function Quad_Trunc(qu: TRGBQuad): TRGBQuad;
function Quad_Dist(qu1,qu2: TRGBQuad): Double;
function xQuad_Dist(qu1,qu2: TRGBQuad): Double;

function rgb_Indexof(Colors: PColors; Count,Color: Integer): Integer;
function qu_Indexof(Quads: PRGBQuads; Count,Color: Integer): Integer;

function Quads_Gray(Quads: PRGBQuads; Count: int): Boolean;
function Colors_Gray(Colors: PColors; Count: int): Boolean;

function xGray_Quads(quads: PRGBQuads; Count: int): Integer;
function xHist_Quads(quads: PRGBQuads; Count: int): Integer;

function xLevel_Quads(Quads: PRGBQuads; Max: Integer;
                      min_cl,max_cl: TColor): Integer;

procedure tga_palette(Quads: PRGBQuads; Count: int);
procedure tga_colors(Colors: PColors; Count: int);

procedure BGR_Colors(Colors: PColors; Count: Integer);

procedure RGB_to_Quads(Rgbp: PRGB_Tbl; Quads: PRGBQuads; Count: Integer);
procedure Quads_to_RGB(Quads: PRGBQuads; Rgbp: PRGB_Tbl; Count: Integer);

procedure Colors_to_Quads(Colors: PColors; Quads: PRGBQuads; Count: Integer);
procedure Quads_to_Colors(Quads: PRGBQuads; Colors: PColors; Count: Integer);

function Trunc_Quads(Quads: PRGBQuads; Count: Integer;
                     chan: PBytes): Integer;

function Comp_Quads(Quads: PRGBQuads; Chan: PBytes): Integer;

function Trunc_Gray(Quads: PRGBQuads; chan: PBytes): Integer;

function xGetPalette(Pal: HPALETTE; Quads: PRGBQuads): Integer;

function CreateSystemPalette(DC: hDC; Cols,Caps: Integer): HPalette;

function xCreatePalette(Colors: PColors; Count: Integer): HPalette;
function qCreatePalette(Quads: PRgbQuads; Count: Integer): HPalette;
function yRealizePalette(DC: hDC; rgbp: PRGB_Tbl; Cols: Integer): HPalette;

function xQuadsPalette(DC: hDC; Quads: PRGBQuads; Cols: Integer): HPalette;

function CopyPalette(Palette: HPALETTE): HPALETTE;

function RGB_24_16(cl: Integer): Integer;
function RGB_16_24(cl: Integer): Integer;

function RGB_32_20(cl: Integer): Integer;
function RGB_20_32(cl: Integer): Integer;

procedure nearest_gray(Dest: PRgbQuads; Count: Integer;
                       Quads: PRgbQuads; Used: Integer;
                       Chan: PBytes);

function RGB_CMYK(r,g,b: Integer): TCmyk;
function Color_CMYK(cl: TColor): TCmyk;

function CMYK_RGB(const cmyk: TCmyk;
                  out r,g,b: Integer): TColorRef;

function x_RGB_CMYK(r,g,b,hi: Integer): TCmyk;

procedure x_CMYK_RGB(const cmyk: tcmyk; hi: Integer;
                    out r,g,b: Integer);

function CMYK_Int32(cl: Integer): Integer;
procedure CMYK_Buf(buf: pint; Count: Integer);
procedure CMYK_BGR(si: PIntegers; di: PBytes; Count: Integer);

function HSV_to_RGB(const hsv: THSV): TColorRef;
function RGB_to_HSV(cl: TColorRef): THSV;

function HSV_Brightness(cl: TColor; kv: Double): TColor;
procedure HSV_Brightness_arr(Colors: PColors; Count: int; kv: double);

function RGB_HSV_Mixed(p1,p2: PBYtes): Integer;

procedure pf24_YCbCr_to_RGB(Data: PBytes; Count: Integer);

procedure b3_to_b3(A,B: PBytes; Tr: PDoubles);

procedure d3_to_d3(A,B,T: PDoubles);

procedure b3_to_d3(A: PBytes; B,T: PDoubles);
procedure d3_to_b3(A,T: PDoubles; B: PBytes);

procedure rgb_to_d3(R,G,B: Integer; V,T: PDoubles);
function d3_to_rgb(A,T: PDoubles): Integer;

function RGB_YIQ_Mixed(p1,p2: PBytes; k1,k2: Float): Integer;
function BGR_YIQ_Mixed(p1,p2: PBYtes; k1,k2: Float): Integer;
procedure BGR_YIQ_Mixed_in_two(p1,p2: PBYtes);

function RGB_YUV_Mixed(p1,p2: PBytes; k1,k2: Float): Integer;

function RGB_Mixed_array(cl: PColors; n: Integer): TColor;
function YIQ_Mixed_array(cl: PColors; n: Integer): TColor;

procedure RGB_Brightness(Colors: PColors; i1,i2: Integer;
                         kv: Double; is_centre: Boolean);

function RGB_to_Gray(cl: TColor): Integer;

function Get_Bitmap_Size(bit: hBitMap): TSize;

function SelectStockObject(dc: HDC; Stock: Integer): THandle;

function SelectSolidPen(dc: HDC; width: Integer;
                        cl: TColorRef): HPen;

function SelectSolidPen1(dc: HDC; width: Integer;
                         cl: TColorRef; old: HPen): HPen;

function SelectTrackPen(dc: HDC; style: Integer;
                        cl: TColorRef): HPen;

function SelectSolidBrush(dc: HDC; cl: TColorRef): HBrush;

function SelectSolidBrush1(dc: HDC; cl: TColorRef;
                           old: HBrush): HBrush;

function Get_lut(lut: PBytes; brightness,contrast: double): bool;

function AdjustColors(Colors: PColors; Count: int;
                      brightness,contrast: double): bool;

implementation

uses
  ofiles,xdc;

{$ASMMODE INTEL}

function win_to_rgb(Color: TColor): TColor;
begin
  if Color = $FFFFFFFF then Color:=clBlack else
  if Color = $0100003B then Color:=clWhite;
  Result:=Color
end;

procedure gamma_init(var Params: TGammaParams);
begin
  Fillchar(Params,Sizeof(Params),0);
  Params.gamma:=1; Params.curve.N:=2;
  Params.curve.Pol[0]:=_Gauss(0,0);
  Params.curve.Pol[1]:=_Gauss(1,1);
  Init_Gray_Channel(@Params.chan);
end;

procedure Fill_gamma_quads(Quads: PRGBQuads;
                           const Params: TGammaParams);
var
  i,hi: Integer;
begin
  for i:=0 to 255 do begin
    hi:=Params.chan[i];
    Quads[i]:=Color_Quad(RGB(hi,hi,hi));
  end
end;

procedure GetGammaChannels(const Gamma: TImageGamma;
                           out ch0,ch1,ch2,ch3: PBytes);

function GammaChanel(const ch0,ch: TGammaParams): PBytes;
begin
  Result:=@ch.chan;
  if ch.typ = 0 then
  if Abs(ch.gamma-1) < 0.001 then
  Result:=@ch0.chan;
end;

begin
  with Gamma do begin
    ch0:=@Gamma[0].chan;
    ch1:=GammaChanel(Gamma[0],Gamma[1]);
    ch2:=GammaChanel(Gamma[0],Gamma[2]);
    ch3:=GammaChanel(Gamma[0],Gamma[3]);
  end
end;

procedure Init_Channel(chan: PBytes; Count: Integer);
var
  i: Integer;
begin
  for i:=0 to Count-1 do chan[i]:=i
end;

procedure Init_Gray_Channel(chan: PBytes);
var
  i: Integer;
begin
  for i:=0 to 255 do chan[i]:=i
end;

procedure Gray_Channel(chan: PBytes; Hist: PIntegers;
                       Left,Right,Count,Flags,hi1,hi2: Integer);
var
  i,hi,dhi: Integer;
  kz: Extended; eax,ebx: Int64;
begin
  if Left >= 0 then
  if Right < Count then

  if Left < Right then begin

    dhi:=Max(1,hi2-hi1);
    kz:=dhi / Max(1,Right - Left);

    if Flags and icb_invert <> 0 then begin
      for i:=0 to Left-1 do chan[i]:=hi2;
      for i:=Right to Count-1 do chan[i]:=hi1;
    end
    else begin
      for i:=0 to Left-1 do chan[i]:=hi1;
      for i:=Right to Count-1 do chan[i]:=hi2;
    end;

    if Flags and icb_square = 0 then begin

      for i:=Left to Right do begin
        hi:=Trunc((i-Left)*kz);

        if Flags and icb_invert <> 0 then
          hi:=hi2 - hi
        else
          hi:=hi1 + hi; chan[i]:=hi
      end

    end
    else begin

      eax:=xCountersCount(@hist[Left],Right-Left+1);
      ebx:=0; if eax > 0 then

      for i:=Left to Right do begin

        hi:=0; if eax > 0 then begin
          Inc(ebx,hist[i]);
          hi:=Trunc(ebx / eax * dhi)
        end;

        if Flags and icb_invert <> 0 then
          hi:=hi2 - hi
        else
          hi:=hi1 + hi; chan[i]:=hi
      end

    end
  end
end;

function Hist_Channel(chan: PBytes; Hist: PIntegers;
                      Count,Flags,hi1,hi2: Integer;
                      Threshold: Float): TPoint;
var
  i,i1,i2: Integer; ax,bx1,bx2: Int64;
begin
  Result.X:=0; Result.Y:=Count-1;

  if Count > 1 then begin

    i1:=0; i2:=Count-1;

    ax:=0;
    for i:=0 to Count-1 do begin
      chan[i]:=Trunc(255/i2);
      Inc(ax,Hist[i]);
    end;

    if ax > 0 then begin
      bx1:=Round(ax*Threshold); bx2:=bx1;

      while bx1 > 0 do begin
        Dec(bx1,Hist[i1]); Inc(i1)
      end;

      while bx2 > 0 do begin
        Dec(bx2,Hist[i2]); Dec(i2)
      end;

      Gray_Channel(chan,Hist,i1,i2,Count,Flags,hi1,hi2);

      Result.X:=i1; Result.Y:=i2
    end
  end
end;

function Fill_Channel(chan: PBytes; Hist: PIntegers;
                      Count,Percent,Flags,hi1,hi2: Integer): TPoint;
begin
  Result:=Hist_Channel(chan,Hist, Count,Flags,hi1,hi2, Percent/100)
end;

function Gray_vga_channel(icb: PImageParams): Integer;
begin
  with icb^ do begin
    Black:=Max(0,Min(254,Black));
    White:=Max(Black+1,Min(255,White));

    Gray_Channel(@chan,@hist,Black,White,256,
                 Flags,0,Grays-1);

    Result:=Grays
  end;
end;

function Fill_vga_channel(icb: PImageParams): Integer;
var
  r: TPoint;
begin
  with icb^ do begin
    r:=Fill_Channel(@chan,@hist,256,
                    Percent,Flags,0,Grays-1);

    Black:=r.X; White:=r.Y;
    Result:=r.Y - r.X + 1;
  end;
end;

function Fill_gray_quads(Quads: PRGBQuads;
                         icb: PImageParams): Integer;
var
  hi: Integer;
  k1,k2,k3: Double; c1,c2,cl: tlong;
begin
  with icb^ do begin
    Grays:=Max(Grays,1);
    c1.i:=min_cl; c2.i:=max_cl;

    k1:=(c2.b[0] - c1.b[0]) / Grays;
    k2:=(c2.b[1] - c1.b[1]) / Grays;
    k3:=(c2.b[2] - c1.b[2]) / Grays;

    cl:=c1;
    for hi:=0 to Grays-1 do begin
      cl.b[0]:=c1.b[0] + Trunc(hi*k1);
      cl.b[1]:=c1.b[1] + Trunc(hi*k1);
      cl.b[2]:=c1.b[2] + Trunc(hi*k2);
      Quads[hi]:=Color_Quad(cl.i)
    end;

    Result:=Grays
  end
end;

function rgb_Indexof(Colors: PColors; Count,Color: Integer): Integer;
var
  i, e,t, r,g,b,_r,_g,_b, dr,dg,db: Integer; ax: tlong;
begin
  Result:=0; e:=-1;

  ax.i:=Color;
  r:=ax.b[0]; g:=ax.b[1]; b:=ax.b[2];

  for i:=0 to Count-1 do begin
    ax.i:=Colors[i];
    _r:=ax.b[0]; _g:=ax.b[1]; _b:=ax.b[2];

    dr:=Abs(r-_r); dg:=Abs(g-_g); db:=Abs(b-_b);

    t:=dr + dg + db;
    if (e < 0) or (t < e) then begin
      Result:=i; e:=t
    end
  end;
end;

function qu_Indexof(Quads: PRGBQuads; Count,Color: Integer): Integer;
var
  i, e,t, r1,g1,b1, dr,dg,db: Integer;  
begin
  Result:=0; e:=-1;

  with tlong(Color) do begin
    r1:=b[0]; g1:=b[1]; b1:=b[2];
  end;

  for i:=0 to Count-1 do begin
    with Quads[i] do begin
      dr:=Abs(rgbRed-r1);
      dg:=Abs(rgbGreen-g1);
      db:=Abs(rgbBlue-b1);
    end;

    t:=dr + dg + db;
    if (e < 0) or (t < e) then begin
      Result:=i; e:=t
    end
  end;
end;

function Quads_Gray(Quads: PRGBQuads; Count: Integer): Boolean;
var
  c: Integer;
begin
  Result:=false; if Count > 2 then begin
    Result:=true; for c:=0 to 255 do with Quads[c] do
    if (rgbRed <> rgbGreen) or (rgbRed <> rgbBlue) then
    begin Result:=false; Break end
  end
end;

function Colors_Gray(Colors: PColors; Count: int): Boolean;
var
  c: Integer;
begin
  Result:=false; if Count > 2 then begin
    Result:=true; for c:=0 to 255 do
    with tlong(Colors[c]) do
    if (b[0] <> b[1]) or (b[0] <> b[2]) then
    begin Result:=false; Break end
  end
end;

function xGray_Quads(Quads: PRGBQuads; Count: int): int;
var
  i,hi: Integer; kc: double; qu: TRGBQuad;
begin
  Result:=0;

  if Assigned(Quads) then begin
    FillChar(Quads^,SizeOf(TRGBQuads),0);

    if Count = 1 then
      Quads[0]:=Color_Quad(clWhite)
    else
    if Count >= 2 then begin
      kc:=255/(Count-1);
      for i:=0 to Count-1 do begin
        hi:=Round(kc*i);

        qu.rgbBlue:=hi;
        qu.rgbGreen:=hi;
        qu.rgbRed:=hi;
        qu.rgbReserved:=0;

        Quads[i]:=qu
      end
    end;

    Result:=Count
  end
end;

function xLevel_Quads(Quads: PRGBQuads; Max: Integer;
                      min_cl,max_cl: TColor): Integer;
var
  i, r1,g1,b1, r2,g2,b2: Integer; q: TRGBQuad;
  rk,bk,gk: double;
begin
  Result:=0;

  if Assigned(Quads) then begin
    FillChar(Quads^,SizeOf(TRGBQuads),0);

    if Max = 1 then
      Quads[0]:=Color_Quad(max_cl)
    else
    if Max >= 2 then begin

      r1:=tlong(min_cl).b[0];
      g1:=tlong(min_cl).b[1];
      b1:=tlong(min_cl).b[2];

      r2:=tlong(max_cl).b[0];
      g2:=tlong(max_cl).b[1];
      b2:=tlong(max_cl).b[2];

      rk:=(r2-r1)/(Max-1);
      gk:=(g2-g1)/(Max-1);
      bk:=(b2-b1)/(Max-1);

      for i:=0 to Max-1 do begin
        q.rgbBlue:=b1 + Round(bk*i);
        q.rgbGreen:=g1 + Round(gk*i);
        q.rgbRed:=r1 + Round(rk*i);
        q.rgbReserved:=0;

        Quads[i]:=q
      end
    end;

    Result:=Max;
  end
end;

function xHist_Quad(I: Integer): TRGBQuad;
var
  h: Integer;
begin
  Result.rgbBlue:=0;
  Result.rgbGreen:=0;
  Result.rgbRed:=0;
  Result.rgbReserved:=0;

  if I > 0 then
  if I < 64 then begin
    h:=I*(192-64) div 64;
    Result.rgbGreen:=Min(64+h,191)
  end else
  if I < 128 then begin
    h:=(I-64)*192 div 64;
    Result.rgbRed:=Min(h,191);
    Result.rgbGreen:=191;
  end else
  if I < 192 then begin
    h:=(I-128)*192 div 64;
    Result.rgbGreen:=Max(0,191-h);
    Result.rgbRed:=191;
  end else
  if I < 256 then begin
    h:=(I-192)*(192-64) div 64;
    Result.rgbRed:=Max(64,191-h)
  end
end;

function xHist_Quads(quads: PRGBQuads; Count: Integer): Integer;
var
  i: Integer; kc: Double;
begin
  Result:=0;

  if Quads <> nil then begin
    FillChar(Quads^,SizeOf(tRGBQuads),0);

    kc:=255/(Count-1); for i:=0 to Count-1 do
    Quads[I]:=xHist_Quad(Round(i*kc));

    Result:=Count
  end
end;

function Color_Quad(cl: TColor): TRGBQuad;
begin
  with TRGBQuad(Result) do begin
    rgbBlue:=tlong(cl).b[2];
    rgbGreen:=tlong(cl).b[1];
    rgbRed:=tlong(cl).b[0];
    rgbReserved:=0
  end
end;

function Quad_Color(qu: TRGBQuad): TColor;
begin
  with qu do
  Result:=RGB(rgbRed,rgbGreen,rgbBlue)
end;

function Quad_Trunc(qu: TRGBQuad): TRGBQuad;
begin
  Result.rgbBlue:=qu.rgbBlue and $F8;
  Result.rgbGreen:=qu.rgbGreen and $F8;
  Result.rgbRed:=qu.rgbRed and $F8;
end;

function Quad_shr(qu: TRGBQuad): TRGBQuad;
begin
  Result.rgbBlue:=qu.rgbBlue shr 3;
  Result.rgbGreen:=qu.rgbGreen shr 3;
  Result.rgbRed:=qu.rgbRed shr 3;
end;

function Quad_Dist(qu1,qu2: TRGBQuad): Double;
begin
  Result:=Sqrt(Sqr(qu2.rgbRed-qu1.rgbRed)+
               Sqr(qu2.rgbGreen-qu1.rgbGreen)+
               Sqr(qu2.rgbBlue-qu1.rgbBlue))
end;

function xQuad_Dist(qu1,qu2: TRGBQuad): Double;
begin
  Result:=Quad_Dist(Quad_shr(qu1),Quad_shr(qu2))
end;

procedure tga_palette(Quads: PRGBQuads; Count: int);
var
  i,j: int; ip: PIntegers;
begin
  ip:=@Quads[0];
  for i:=1 to Count-1 do
  if Int_Contains(ip,i,ip[i]) >= 0 then

  for j:=0 to 15 do
  if Int_Contains(ip,i,EGA_Quads[j]) < 0 then begin
    ip[i]:=EGA_Quads[j]; Break
  end
end;

procedure tga_colors(Colors: PColors; Count: int);
var
  i,j: int; ip: PIntegers;
begin
  ip:=@Colors[0];
  for i:=1 to Count-1 do
  if Int_Contains(ip,i,ip[i]) >= 0 then

  for j:=0 to 15 do
  if Int_Contains(ip,i,EGA_Colors[j]) < 0 then begin
    ip[i]:=EGA_Colors[j]; Break
  end
end;

procedure BGR_Colors(Colors: PColors; Count: Integer);
var
  i,al: Integer;
begin
  for i:=0 to Count-1 do
  with tlong(Colors[i]) do begin
    al:=b[0]; b[0]:=b[2]; b[2]:=al
  end
end;

procedure RGB_to_Quads(Rgbp: PRGB_Tbl; Quads: PRGBQuads; Count: Integer);
var
  i: Integer;
begin
  if Quads <> nil then
  for i:=0 to Count-1 do with Rgbp[i],Quads[i] do begin
    rgbBlue:=b; rgbGreen:=g; rgbRed:=r; rgbReserved:=0
  end
end;

procedure Quads_to_RGB(Quads: PRGBQuads; Rgbp: PRGB_Tbl; Count: Integer);
var
  i: Integer;
begin
  for i:=0 to Count-1 do with Quads[i],rgbp[i] do begin
    r:=rgbRed; g:=rgbGreen; b:=rgbBlue
  end
end;

procedure Colors_to_Quads(Colors: PColors; Quads: PRGBQuads; Count: Integer);
var
  i: Integer; cl: tlong; qu: TRGBQuad;
begin
  if Count = 0 then Count:=256;
  for i:=0 to Count-1 do begin
    cl.i:=Colors[i]; qu.rgbBlue:=cl.b[2];
    qu.rgbGreen:=cl.b[1]; qu.rgbRed:=cl.b[0];
    qu.rgbReserved:=0; Quads[i]:=qu
  end
end;

procedure Quads_to_Colors(Quads: PRGBQuads; Colors: PColors; Count: Integer);
var
  i: Integer;
begin
  if Count = 0 then Count:=256;
  for i:=0 to Count-1 do with Quads[i] do
  Colors[i]:=RGB(rgbRed,rgbGreen,rgbBlue)
end;

function RGB_Black(const Quad: TRGBQuad): boolean;
begin
  Result:=false; with Quad do if rgbRed = 0 then
  if rgbGreen = 0 then if rgbBlue = 0 then Result:=true
end;

function Trunc_Quads(Quads: PRGBQuads; Count: Integer;
                     Chan: PBytes): Integer;
var
  i: Integer; is_255: Boolean;
begin
  Result:=Count;

  is_255:=false; if Count = 256 then
  if not RGB_Black(Quads[255]) then begin
    Dec(Result); is_255:=true
  end;

  while Result > 16 do begin
    if not RGB_Black(Quads[Result-1]) then
    Break; Dec(Result)
  end;

  if (Result > 16) and is_255 then
  Result:=Count;

  if chan <> nil then for i:=0 to 255 do
  if chan[i] >= Result then chan[i]:=0
end;

function Comp_Quads(Quads: PRGBQuads; Chan: PBytes): Integer;
var
  i,j: Integer; qu: TRGBQuad;
begin
  Result:=1;

  if Assigned(Chan) then
  Chan[0]:=0;

  for i:=1 to 255 do begin qu:=Quads[i];
    j:=Int_Contains(@Quads[0],Result,Integer(qu));
    if j < 0 then begin
      j:=Result; Quads[j]:=qu; Inc(Result);
    end;

    if Assigned(Chan) then
    Chan[i]:=j
  end
end;

function Trunc_Gray(Quads: PRGBQuads; chan: PBytes): Integer;
var
  c: Integer;
begin
  Result:=xGray_Quads(Quads,64);
  for c:=0 to 255 do chan[c]:=c shr 2
end;

function CreateSystemPalette(DC: hDC; Cols,Caps: Integer): HPalette;
var
  Sys: PLogPalette; len: Integer;
begin
  Result:=0;
  if xBitsPerPixel(DC) <= 8 then begin
    if Cols = 0 then Cols:=GetDeviceCaps(DC,Caps);

    if Cols <= 256 then begin
      len:=SizeOf(TLogPalette)+SizeOf(tPaletteEntry)*256;

      sys:=xGetMem(len); if sys <> nil then begin
        sys.palVersion:=$300; sys.palNumEntries:=Cols;
        GetSystemPaletteEntries(DC,0,Cols,sys.palPalEntry);
        Result:=CreatePalette(Sys^); FreeMem(sys,len)
      end
    end
  end
end;

function xGetPalette(Pal: HPALETTE; Quads: PRGBQuads): Integer;
var
  i,n: Integer; log: TMaxLogPalette;
begin
  Result:=0; n:=0; if Pal <> 0 then
  if GetObject(pal,SizeOf(n),@n) > 0 then
  if (n >= 2) and (n <= 256) then begin

    with log do begin
      palVersion:=$0300; palNumEntries:=n;
      GetPaletteEntries(pal, 0,n, palPalEntry);

      for i:=0 to n-1 do with palPalEntry[i] do begin
        Quads[i].rgbBlue:=peBlue;
        Quads[i].rgbGreen:=peGreen;
        Quads[i].rgbRed:=peRed;
        Quads[i].rgbReserved:=0
      end;

      Result:=n
    end
  end
end;

function xCreatePalette(Colors: PColors; Count: Integer): HPalette;
var
  Log: TMaxLogPalette; i: Integer;
begin
  Result:=0; with Log do begin
    palVersion:=$300; palNumEntries:=Count;
    for i:=0 to Count-1 do palPalEntry[i]:=TPaletteEntry(Colors[i]);
    Result:=CreatePalette(PLogPalette(@Log)^)
  end
end;

function qCreatePalette(Quads: PRgbQuads; Count: Integer): HPalette;
var
  Colors: TColors;
begin
  Quads_to_Colors(Quads,@Colors,Count);
  Result:=xCreatePalette(@Colors,Count)
end;

function yCreatePalette(Rgbp: PRGB_Tbl; Count: Integer): HPalette;
var
  Colors: TColors; i: Integer;
begin
  for i:=0 to Count-1 do
  with Rgbp[i] do Colors[i]:=RGB(r,g,b);
  Result:=xCreatePalette(@Colors,Count)
end;

function yRealizePalette(DC: hDC; rgbp: PRGB_Tbl; Cols: Integer): HPalette;
begin
  Result:=yCreatePalette(rgbp,Cols); if Result <> 0 then begin
    SelectPalette(DC,Result,false); RealizePalette(DC)
  end
end;

function xQuadsPalette(DC: hDC; Quads: PRGBQuads; Cols: Integer): HPalette;
var
  i,len: Integer; Log: PLogPalette; ch: PPaletteChanel;
begin
  Result:=0; if Cols > 0 then
  if GetDeviceCaps(DC,SizePalette) > 0 then begin

    len:=SizeOf(TLogPalette)+SizeOf(TPaletteEntry)*Cols;
    Log:=xGetMem(len); if Log <> nil then begin

      Log.palVersion:=$300;
      Log.palNumEntries:=Cols;

      for i:=0 to Cols-1 do
      with Log.palPalEntry[i],Quads[i] do begin
        peRed:=rgbRed; peGreen:=rgbGreen; peBlue:=rgbBlue;
        peFlags:=0
      end;

      Result:=CreatePalette(Log^);
      if Result <> 0 then begin
        SelectPalette(DC,Result,false); RealizePalette(DC);

        ch:=PPaletteChanel(Quads);
        for i:=0 to Cols-1 do ch[i]:=i;
      end
    end; xFreeMem(Log,len)
  end
end;

function CopyPalette(Palette: HPALETTE): HPALETTE;
var
  PaletteSize: int; Log: TMaxLogPalette;
begin
  Result := 0;
  if Palette = 0 then Exit;
  PaletteSize := 0;
  if GetObject(Palette, SizeOf(PaletteSize), @PaletteSize) = 0 then Exit;
  if PaletteSize = 0 then Exit;

  with Log do begin
    palVersion:=$0300;
    palNumEntries:=PaletteSize;
    GetPaletteEntries(Palette, 0, PaletteSize, palPalEntry);
  end;

  Result:=CreatePalette(PLogPalette(@Log)^)
end;

function RGB_24_16(cl: Integer): Integer;
var
  r,g,b: Integer;
begin
  r:=tlong(cl).b[0] shr 3;
  g:=tlong(cl).b[1] shr 3;
  b:=tlong(cl).b[2] shr 3;
  Result:=(b shl 10) or (g shl 5) or r;
end;

function RGB_16_24(cl: Integer): Integer;
var
  ax: tlong;
begin
  ax.i:=0;
  ax.b[0]:=(cl and $1F) shl 3; cl:=cl shr 5;
  ax.b[1]:=(cl and $1F) shl 3; cl:=cl shr 5;
  ax.b[2]:=(cl and $1F) shl 3; Result:=ax.i
end;

function RGB_32_20(cl: Integer): Integer;
var
  r,g,b,a: Integer;
begin
  r:=tlong(cl).b[0] shr 3;
  g:=tlong(cl).b[1] shr 3;
  b:=tlong(cl).b[2] shr 3;
  a:=tlong(cl).b[3] shr 3;
  Result:=(a shl 15) or (b shl 10) or (g shl 5) or r;
end;

function RGB_20_32(cl: Integer): Integer;
var
  ax: tlong;
begin
  ax.i:=0;
  ax.b[0]:=(cl and $1F) shl 3; cl:=cl shr 5;
  ax.b[1]:=(cl and $1F) shl 3; cl:=cl shr 5;
  ax.b[2]:=(cl and $1F) shl 3; cl:=cl shr 5;
  ax.b[3]:=(cl and $1F) shl 3; Result:=ax.i
end;

procedure Nearest_Gray(Dest: PRgbQuads; Count: Integer;
                       Quads: PRgbQuads; Used: Integer;
                       Chan: PBytes);
var
  i,j,hi,dh,ind,tmp: Integer;
begin
  if Count > 0 then
  for i:=0 to Used-1 do begin

    hi:=Quads[i].rgbBlue;

    ind:=0; dh:=Abs(hi - Dest[0].rgbBlue);
    for j:=1 to Count-1 do begin
      tmp:=Abs(hi - Dest[j].rgbBlue);
      if tmp < dh then begin
        dh:=tmp; ind:=j
      end
    end;

    Chan[i]:=ind
  end
end;

function RGB_CMYK(r,g,b: Integer): TCmyk;
var
  c,m,y,t: float;
begin
  c:=1-r/255; m:=1-g/255; y:=1-b/255;
  t:=Min(c,Min(m,y));

  Result.c:=Min(1, Max(0, c-t));
  Result.m:=Min(1, Max(0, m-t));
  Result.y:=Min(1, Max(0, y-t));
  Result.k:=Min(1, Max(0, t));
end;

function Color_CMYK(cl: TColor): TCmyk;
begin
  with tlong(cl) do
  Result:=RGB_CMYK(b[0],b[1],b[2])
end;

function CMYK_RGB(const cmyk: tcmyk; out r,g,b: Integer): TColorRef;
begin
  with cmyk do begin
    r:=Round((1 - Min(1, c+k))*255);
    g:=Round((1 - Min(1, m+k))*255);
    b:=Round((1 - Min(1, y+k))*255);
  end;

  Result:=RGB(r,g,b)
end;

function x_RGB_CMYK(r,g,b,hi: Integer): TCmyk;
var
  c,m,y,t: float;
begin
  c:=1-r/hi; m:=1-g/hi; y:=1-b/hi;
  t:=Min(c,Min(m,y));

  Result.c:=Min(1, Max(0, c-t));
  Result.m:=Min(1, Max(0, m-t));
  Result.y:=Min(1, Max(0, y-t));
  Result.k:=Min(1, Max(0, t));
end;

procedure x_CMYK_RGB(const cmyk: tcmyk; hi: Integer;
                    out r,g,b: Integer);
begin
  with cmyk do begin
    r:=Round((1 - Min(1, c+k))*hi);
    g:=Round((1 - Min(1, m+k))*hi);
    b:=Round((1 - Min(1, y+k))*hi);
  end;
end;

function CMYK_Int32(cl: Integer): Integer;
asm
{$ifdef CPUX86_64}
  mov   RAx,RDi
  push  RBx
{$else}
  push  EBx
{$endif}

  mov   EBx,EAx
  BSWAP EBx
  mov   Dl,Bl
  BSWAP EBx

  mov   EAx,0
  mov   ECx,3
@loop:
  shl   EAx,8
  mov   Al,Bl
  add   Al,Dl
  jnc   @next
  mov   Al,255
@next:
  shr   EBx,8
  sub   Al,255
  neg   Al
  loop  @loop

{$ifdef CPUX86_64}
  pop   RBx
{$else}
  pop   EBx
{$endif}
end;

procedure CMYK_Buf(buf: pint; Count: Integer);
{$ifdef CPUX86_64}
var
  i: int;
begin
  for i:=1 to Count do begin
    buf^:=CMYK_Int32(buf^); Inc(buf)
  end
end;

{$else}
asm
  push  EDi
  mov   EDi,EAx
  mov   ECx,EDx

  cld
@loop:
  mov   EAx,DWORD Ptr [EDi]
  push  ECx
  call  CMYK_Int32
  stosd
  pop   ECx
  loop  @loop

  pop   EDi
end;
{$endif}

function cmykConvert(c,m,y,k: float): int;
var
  k1,nc,nm,ny: float; r,g,b: int;
begin
  k1:=(1-k);
  nc:=(c * k1 + k);
  nm:=(m * k1 + k);
  ny:=(y * k1 + k);

  r:=Trunc( (1-nc) * 255 );
  g:=Trunc( (1-nm) * 255 );
  b:=Trunc( (1-ny) * 255 );

  Result:=RGB(r,g,b)
end;

procedure CMYK_BGR(si: PIntegers; di: PBytes; Count: Integer);
var
  i,c,m,y,k: int; ax: tlong;
begin
  for i:=0 to Count-1 do begin
    ax.i:=si[0]; si:=@si[1];

    with ax do
    i:=cmykConvert(b[0]/255,b[1]/255,b[2]/255,b[3]/255);

    di[0]:=ax.b[2];
    di[1]:=ax.b[1];
    di[2]:=ax.b[0];
    di:=@di[3]
  end
end;

function Middle_V(Colors: PColors; i1,i2: Integer): Double;
var
  i,n: Integer; hsv: thsv;
begin
  Result:=0; n:=i2-i1+1;

  for i:=i1 to i2 do begin
    hsv:=RGB_to_HSV(Colors[i]);
    Result:=Result + hsv.v/n
  end
end;

// h: 0-360; s: 0-1; v: 0-1; r,g,b: 0-255
function HSV_to_RGB(const hsv: thsv): TColorRef;
var
  i, r,g,b: int;
  h,s,v, f,m,n,k, rr,rg,rb: Double;
begin
  h:=hsv.h; s:=hsv.s; v:=hsv.v;
  if v < 0 then v:=0;
  if v > 1 then v:=1;

  if s = 0 then begin
    rr:=v; rg:=v; rb:=v
  end
  else begin
    if h = 360 then h:=0 else h:=h/60;

    i:=Trunc(h); f:=h-i;

    m:=v*(1-s); n:=v*(1-s*f); k:=v*(1-s*(1-f));

    case i+1 of
  1:  begin rr:=v; rg:=k; rb:=m end;
  2:  begin rr:=n; rg:=v; rb:=m end;
  3:  begin rr:=m; rg:=v; rb:=k end;
  4:  begin rr:=m; rg:=n; rb:=v end;
  5:  begin rr:=k; rg:=m; rb:=v end;
  6:  begin rr:=v; rg:=m; rb:=n end;
    end
  end;

  r:=Max(0,Min(255,Round(rr*255)));
  g:=Max(0,Min(255,Round(rg*255)));
  b:=Max(0,Min(255,Round(rb*255)));

  Result:=RGB(r,g,b)
end;

function RGB_to_HSV(cl: TColorRef): THSV;
var
  r,g,b,t, cr,cg,cb, h,s,v: Double;
begin
  r:=tlong(cl).b[0]/255;
  g:=tlong(cl).b[1]/255;;
  b:=tlong(cl).b[2]/255;;

  t:=r; if t > g then t:=g; if t > b then t:=b;
  v:=r; if v < g then v:=g; if v < b then v:=b;

  if v = 0 then s:=0 else s:=(v-t)/v;

  if s = 0 then h:=360 else begin
    t:=v-t; cr:=(v-r)/t; cg:=(v-g)/t; cb:=(v-b)/t;

    if Abs(r-v) < 0.00001 then h:=cb-cg;
    if Abs(g-v) < 0.00001 then h:=2+cr-cb;
    if Abs(b-v) < 0.00001 then h:=4+cg-cr;

    h:=60*h; if h < 0 then h:=h+360
  end;

  Result.h:=h;
  Result.s:=s;
  Result.v:=v
end;

function HSV_Brightness(cl: TColor; kv: Double): TColor;
var
  hsv: THSV;
begin
  hsv:=RGB_to_HSV(cl);
  hsv.v:=hsv.v*kv;
  if hsv.v > 1 then hsv.v:=1;
  Result:=HSV_to_RGB(hsv)
end;

procedure HSV_brightness_arr(Colors: PColors; Count: int; kv: double);
var
  i: int; hsv: THSV;
begin
  for i:=0 to Count-1 do begin
    hsv:=RGB_to_HSV(Colors[i]);
    hsv.v:=hsv.v*kv;
    if hsv.v > 1 then hsv.v:=1;
    Colors[i]:=HSV_to_RGB(hsv)
  end
end;

function RGB_HSV_Mixed(p1,p2: PBYtes): Integer;
var
  hsv1,hsv2,hsv: thsv; cl: tlong;
begin
  cl.i:=0; Move(p1[0],cl,3);
  hsv1:=RGB_to_HSV(cl.i);

  cl.i:=0; Move(p2[0],cl,3);
  hsv2:=RGB_to_HSV(cl.i);

  hsv.h:=hsv1.h/2 + hsv2.h/2;
  hsv.s:=hsv1.s/2 + hsv2.s/2;
  hsv.v:=hsv1.v/2 + hsv2.v/2;

  Result:=HSV_to_RGB(hsv)
end;

function YCbCr_to_RGB(const YCbCr: TYCbCr): TColorRef;
var
  r,g,b: Double;
begin
  with YCbCr do begin
    b:=Cb + Y; r:=Cr + Y;
    g:=(Y - (0.299*r) - (0.114*b)) / 0.587;
  end;

  Result:=RGB(Round(Min(1,r)*255),Round(Min(1,g)*255),Round(Min(1,b)*255))
end;

procedure pf24_YCbCr_to_RGB(Data: PBytes; Count: Integer);
var
  i: int; si: PBytes; Y,Cb,Cr,R,G,B: Double;
begin
  si:=Data;
  for i:=1 to Count do begin

    Y:=si[0]/255; Cb:=si[1]/255; Cr:=si[2]/255;

    B:=Cb + Y; R:=Cr + Y;
    G:=(Y - (0.299*R) - (0.114*B)) / 0.587;

    si[0]:=Round(Min(1,R)*255);
    si[1]:=Round(Min(1,G)*255);
    si[2]:=Round(Min(1,B)*255);

    si:=@si[3]
  end
end;

procedure d3_to_d3(A,B,T: PDoubles);
var
  i: Integer;
begin
  for i:=1 to 3 do begin
    B[0]:=A[0]*T[0] + A[1]*T[1] + A[2]*T[2];
    B:=@B[1]; T:=@T[3]
  end
end;

procedure b3_to_b3(A,B: PBytes; Tr: PDoubles);
var
  ax,i: int; v: IValues8;
begin
  for i:=0 to 2 do begin
    ax:=Round(A[0]*Tr[0] + A[1]*Tr[1] + A[2]*Tr[2]);
    if ax < 0 then ax:=0; if ax > 255 then ax:=255;
    v[i]:=ax; Tr:=@Tr[3]
  end;

  B[0]:=v[0]; B[1]:=v[1]; B[2]:=v[2];
end;

procedure b3_to_d3(A: PBytes; B,T: PDoubles);
var
  i: Integer;
begin
  for i:=1 to 3 do begin
    B[0]:=A[0]*T[0] + A[1]*T[1] + A[2]*T[2];
    B:=@B[1]; T:=@T[3]
  end
end;

procedure d3_to_b3(A,T: PDoubles; B: PBytes);
var
  i: Integer; ax: Double;
begin
  for i:=0 to 2 do begin
    ax:=A[0]*T[0] + A[1]*T[1] + A[2]*T[2];
    if ax < 0 then ax:=0; if ax > 255 then ax:=255;
    B[i]:=Round(ax); T:=@T[3]
  end;
end;

procedure RGB_to_d3(R,G,B: Integer; V,T: PDoubles);
var
  i: Integer;
begin
  for i:=1 to 3 do begin
    V[0]:=R*T[0] + G*T[1] + B*T[2];
    V:=@V[1]; T:=@T[3]
  end
end;

function d3_to_rgb(A,T: PDoubles): Integer;
var
  i: Integer; cl: tlong; eax: Double;
begin
  cl.i:=0;

  for i:=0 to 2 do begin
    eax:=A[0]*T[0] + A[1]*T[1] + A[2]*T[2];
    cl.b[i]:=Max(0,Min(255,Round(eax))); T:=@T[3]
  end;

  Result:=cl.i
end;

function RGB_YIQ_Mixed(p1,p2: PBytes; k1,k2: Float): Integer;
var
  c1,c2,c: TYIQ; ks: Float;
begin
  ks:=k1 + k2;

  b3_to_d3(p1,@c1,@RGB_YIQ_T);
  b3_to_d3(p2,@c2,@RGB_YIQ_T);

  c.Y:=(c1.Y*k1 + c2.Y*k2) / ks;
  c.I:=(c1.I*k1 + c2.I*k2) / ks;
  c.Q:=(c1.Q*k1 + c2.Q*k2) / ks;

  Result:=d3_to_rgb(@c,@YIQ_RGB_T)
end;

function BGR_YIQ_Mixed(p1,p2: PBytes; k1,k2: Float): Integer;
var
  c1,c2,c: TYIQ; ks: Float; al: Byte;
begin
  ks:=k1 + k2;

  RGB_to_d3(p1[2],p1[1],p1[0],@c1,@RGB_YIQ_T);
  RGB_to_d3(p2[2],p2[1],p2[0],@c2,@RGB_YIQ_T);

  c.Y:=(c1.Y*k1 + c2.Y*k2) / ks;
  c.I:=(c1.I*k1 + c2.I*k2) / ks;
  c.Q:=(c1.Q*k1 + c2.Q*k2) / ks;

  Result:=d3_to_rgb(@c,@YIQ_RGB_T);

  with tlong(Result) do begin
    al:=b[0]; b[0]:=b[2]; b[2]:=al
  end
end;

procedure BGR_YIQ_Mixed_in_two(p1,p2: PBYtes);
var
  c1,c2,c: TYIQ; ax: tlong;
begin
  RGB_to_d3(p1[2],p1[1],p1[0],@c1,@RGB_YIQ_T);
  RGB_to_d3(p2[2],p2[1],p2[0],@c2,@RGB_YIQ_T);

  c.Y:=(c1.Y + c2.Y) / 2;
  c.I:=(c1.I + c2.I) / 2;
  c.Q:=(c1.Q + c2.Q) / 2;

  ax.i:=d3_to_rgb(@c,@YIQ_RGB_T);

  p1[0]:=ax.b[2]; p1[1]:=ax.b[1]; p1[2]:=ax.b[0];
end;

function RGB_YUV_Mixed(p1,p2: PBytes; k1,k2: Float): Integer;
var
  c1,c2,c: TYIQ; ks: Float;
begin
  Result:=0; ks:=k1 + k2;

  b3_to_d3(p1,@c1,@RGB_YUV_T);
  b3_to_d3(p2,@c2,@RGB_YUV_T);

  c.Y:=(c1.Y*k1 + c2.Y*k2) / ks;
  c.I:=(c1.I*k1 + c2.I*k2) / ks;
  c.Q:=(c1.Q*k1 + c2.Q*k1) / ks;

  Result:=d3_to_rgb(@c,@YUV_RGB_T)
end;

function RGB_Mixed_array(cl: PColors; n: Integer): TColor;
var
  i: Integer; _r,_g,_b,k: Double;
begin
  k:=1/n; _r:=0; _g:=0; _b:=0;

  for i:=1 to n do begin
    with tlong(cl[0]) do begin
      _r:=_r + b[0]*k;
      _g:=_g + b[1]*k;
      _b:=_b + b[2]*k;
    end;

    cl:=@cl[1]
  end;

  Result:=0;
  tlong(Result).b[0]:=Min(255,Round(_r));
  tlong(Result).b[1]:=Min(255,Round(_g));
  tlong(Result).b[2]:=Min(255,Round(_b));
end;

function YIQ_Mixed_array(cl: PColors; n: Integer): TColor;
var
  i: Integer; k: Double; acc,yiq: TYIQ;
begin
  k:=1/n;

  acc.Y:=0; acc.I:=0; acc.Q:=0;

  for i:=1 to n do begin
    b3_to_d3(@cl[0],@yiq,@RGB_YIQ_T);

    acc.Y:=acc.Y + yiq.Y*k;
    acc.I:=acc.I + yiq.I*k;
    acc.Q:=acc.Q + yiq.Q*k;

    cl:=@cl[1]
  end;

  Result:=d3_to_rgb(@acc,@YIQ_RGB_T)
end;

procedure RGB_Brightness(Colors: PColors; i1,i2: Integer;
                         kv: Double; is_centre: Boolean);
var
  i: integer; sv: Double; hsv: thsv;
begin
  sv:=0; if is_centre then
  sv:=Middle_V(Colors,i1,i2);

  for i:=i1 to i2 do begin
    hsv:=RGB_to_HSV(Colors[i]);
    hsv.v:=sv+(hsv.v-sv)*kv;
    Colors[i]:=HSV_to_RGB(hsv)
  end
end;

function RGB_to_Gray(cl: TColor): Integer;
begin
  with tlong(cl) do
  Result:=Round(0.299*b[0] + 0.587*b[1] + 0.114*b[2]);
end;

function Get_Bitmap_Size(bit: hBitMap): TSize;
var
  Info: tagBITMAP; rc: Integer;
begin
  Result.cx:=0; Result.cy:=0;

  if bit <> 0 then begin
    rc:=GetObject(bit,Sizeof(Info),@Info);

    if rc = Sizeof(Info) then begin
      Result.cx:=Info.bmWidth;
      Result.cy:=Info.bmHeight
    end
  end
end;

function SelectStockObject(dc: HDC; Stock: Integer): THandle;
var
  h: THandle;
begin
  h:=GetStockObject(Stock);
  Result:=SelectObject(dc,h)
end;

function SelectSolidPen(dc: HDC; width: Integer;
                        cl: TColorRef): HPen;
begin
  Result:=CreatePen(ps_SOLID,width,cl);
  SelectObject(dc,Result)
end;

function SelectSolidPen1(dc: HDC; width: Integer;
                        cl: TColorRef; old: HPen): HPen;
begin
  Result:=CreatePen(ps_SOLID,width,cl);
  if SelectObject(dc,Result) = old then
  DeleteObject(old)
end;

function SelectTrackPen(dc: HDC; style: Integer;
                        cl: TColorRef): HPen;
begin
  Result:=CreatePen(style,1,cl);
  SelectObject(dc,Result)
end;

function SelectSolidBrush(dc: HDC; cl: TColorRef): HBrush;
begin
  Result:=CreateSolidBrush(cl);
  SelectObject(dc,Result)
end;

function SelectSolidBrush1(dc: HDC; cl: TColorRef;
                           old: HBrush): HBrush;
begin
  Result:=CreateSolidBrush(cl);
  if SelectObject(dc,Result) = old then
  DeleteObject(old)
end;

function Get_lut(lut: PBytes; brightness,contrast: double): bool;
var
  i,b: int; k,v: double; dLut: doubles256;
begin
  Result:=false;

	for i:=0 to 255 do dLut[i]:=i;

	if Abs(contrast) > 1E-6 then begin
		k:=1 + contrast;
		for i:=0 to 255 do begin
      v:=128 + (dLut[i]-128)*k;
			dLut[i]:=Max(0,Min(v,255))
    end;

    Result:=true
  end;

	if Abs(brightness) > 1E-6 then begin
    k:=1 + brightness;
		for i:=0 to 255 do begin
			v:=dLut[i]*k;
			dLUT[i]:=Max(0,Min(v,255))
    end;

		Result:=true
  end;

  for i:=0 to 255 do begin
    b:=Round(dLut[i]);
    if b > 255 then b:=255;
    lut[i]:=b
  end
end;

function AdjustColors(Colors: PColors; Count: int;
                      brightness,contrast: double): bool;
var
  i: int; lut: TChannel;
begin
  Result:=false;
  if Get_lut(@lut,brightness,contrast) then begin

    for i:=0 to Count-1 do
    with tlong(Colors[i]) do begin
      b[0]:=lut[b[0]];
      b[1]:=lut[b[1]];
      b[2]:=lut[b[2]];
    end;

		Result:=true
  end
end;

end.
