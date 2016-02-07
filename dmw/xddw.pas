unit xddw; interface

uses
  LCLType,LCLIntf,
  Graphics,otypes;

type
  PVGABITMAPINFO = ^TVGABITMAPINFO;
  TVGABITMAPINFO = record
    bmiHeader: TBITMAPINFOHEADER;
    bmiColors: TRGBQUADS
  end;

function __Bitmap(w,h,bits,line: int; buf: Pointer): Bitmap;

function BITMAPINFOHEADER(w,h,bits: int): TBITMAPINFOHEADER;

function bmp_Is_gray(const Info: TVGABITMAPINFO): Boolean;

procedure pushColors;
procedure popColors;

procedure dib_Colors_init;

function dib_Colors_ptr: Pointer;
function dib_Create_Palette(n: Integer): HPalette;
procedure dib_Colors_Reset(cp: PColors);
procedure dib_Colors_cls;

function dib_Color(i: Integer): ColorRef;
function dib_CMYK(i: Integer): TCmyk;

procedure Set_dib_Color(i,cl: Integer);

function dib_Indexof(cl: Integer): Integer;

procedure dib_Colors_Load(Path: PChar);
procedure dib_Colors_SaveAs(Path: PChar);

function dmw_Unpack_cl(cl: longint): ColorRef;

function fill_Color(cl: Integer): ColorRef;
function pen_Color(cl: Integer): ColorRef;

function xCreateDibSection(Width,Height,Bits: Integer;
                           Colors: PColors; used: Integer;
                           out DibBits: Pointer): HBitmap;

function Bitmap_map(bmp: TBitmap; out map: Bitmap): Boolean;

function Bitmap_ppvBits(Bmp: TBitmap): Pointer;
function Bitmap_Depth(Bmp: TBitmap): Integer;

function dib_init_bitmap(bmp: TBitmap; w,h,bits: Integer): Boolean;
function dib_alloc_bitmap(bmp: TBitmap; w,h: Integer): Boolean;
procedure dib_clear_bitmap(bmp: TBitmap);

function dib_alloc_bmp(Width,Height,Bits: Integer;
                       Colors: PColors; used: Integer;
                       out DibBits: Pointer): HBitmap;

function dib_alloc_rgb(Width,Height: Integer;
                       out DibBits: Pointer): HBitmap;

function dib_alloc_rgba(Width,Height: Integer;
                        out DibBits: Pointer): HBitmap;

function dib_push_bitmap(src,dst: TBitmap): Boolean;
function dib_pop_bitmap(src,dst: TBitmap): Boolean;

procedure ddw_Patterns_Init;
function ddw_Patterns_ptr: pointer;

procedure dib_texture(bp: PBytes; cl,fc,pat,alf: Integer);

function BitmapIsGray(const Bmp: Bitmap): Boolean;
function BitmapIsEmpty(const Bmp: Bitmap; fc: int): bool;

function allocBitmap(out bmp: Bitmap; w,h,bits: int): bool;

procedure freeBitmap(var bmp: Bitmap);
function clearBitmap(const bmp: Bitmap): Integer;

function likeBitmap(const src,dst: Bitmap): Integer;
function copyBitmap(const src,dst: Bitmap): Integer;

function pasteBitmap(const dst: Bitmap; dstX,dstY: int;
                     const src: Bitmap; srcX,srcY,srcW,srcH: int): bool;

function alfaBitmap(const src,dst: Bitmap;
                    fc: Integer; alf: Float): Integer;

function mixBitmap(const src,dst: Bitmap; fc: int): int;
function transBitmap(const src,dst: Bitmap; tc: int): int;

function map32_to_map(const src,dst: Bitmap; tc: int): bool;

procedure dib_gamma(const bmp: Bitmap; gp: PBytes);

procedure bmp_fill_rgba(const bmp: Bitmap; w,h,cl: int);
procedure bmp_alfa_swap_rgba(const bmp: Bitmap; w,h,cl: int);

function bmp_rgb_plus_alfa(const bmp: HBitmap; fc: int): HBitmap;

var
  dibPalette32: HPalette;

implementation

uses
  Sysutils,Classes,
  ofiles,xgdi,xdc;

var
  FillMasks: PFillMasks;
  dib_Colors: TColors;
  dib_Colors1: TColors;

function __Bitmap(w,h,bits,line: int; buf: Pointer): Bitmap;
var
  map: Bitmap;
begin
  Fillchar(map,sizeOf(map),0);
  map.bmWidth:=w; map.bmHeight:=h;
  map.bmBitsPixel:=bits; map.bmPlanes:=1;
  map.bmWidthBytes:=line; map.bmBits:=buf;
  Result:=map
end;

function BITMAPINFOHEADER(w,h,bits: int): TBITMAPINFOHEADER;
var
  hr: TBITMAPINFOHEADER;
begin
  Fillchar(hr,Sizeof(hr),0);
  hr.biSize:=Sizeof(hr);
  hr.biWidth:=w;
  hr.biHeight:=h;
  hr.biPlanes:=1;
  hr.biBitCount:=bits;
  Result:=hr
end;

function bmp_Is_gray(const Info: TVGABITMAPINFO): Boolean;
begin
  Result:=false;
  with Info.bmiHeader do

  if biPlanes*biBitCount = 8 then
  if biClrUsed = 0 then
    Result:=true
  else
    Result:=Quads_Gray(@Info.bmiColors,
                       biClrUsed)
end;

procedure PushColors;
begin
  dib_Colors1:=dib_Colors
end;

procedure PopColors;
begin
  dib_Colors:=dib_Colors1
end;

procedure dib_Colors_init;
begin
  FillChar(dib_Colors,SizeOf(dib_Colors),0);
  dib_Colors_Reset(@dib_Colors); PushColors;
end;

function dib_Colors_ptr: pointer;
begin
  Result:=@dib_Colors
end;

function dib_Create_Palette(n: Integer): HPalette;
begin
  if n <= 0 then n:=16;
  Result:=xCreatePalette(@dib_Colors,n)
end;

procedure dib_Colors_reset(cp: PColors);
var
  i: Integer;
begin
  for i:=0 to 15 do begin
    cp[i]:=EGA_Colors[i];
    cp[16+i]:=EGA_Colors[15-i]
  end
end;

procedure dib_Colors_cls;
begin
  FillChar(dib_Colors,SizeOf(dib_Colors),0)
end;

function dib_Color(i: Integer): ColorRef;
begin
  Result:=dib_Colors[i and 31]
end;

function dib_CMYK(i: Integer): TCmyk;
begin
  Result:=Color_CMYK(dib_Color(i))
end;

procedure Set_dib_Color(i,cl: Integer);
begin
  dib_Colors[i and 31]:=cl;
end;

function dib_Indexof(cl: Integer): Integer;
begin
  Result:=rgb_Indexof(@dib_Colors,16,cl)
end;

procedure dib_Colors_Load(Path: PChar);
var
  f: TReadFile; i: int;
begin
  f:=TReadFile.Create;
  try
    if This_Ext(Path,'.RGB') then
    if f.Open(Path) then begin

      dib_Colors_init;

      if f.Size = 64*4 then
        Move(f.Buf^,dib_Colors,64*4)
      else
      if f.Size = 32*4 then
        Move(f.Buf^,dib_Colors[16],32*4);

      for i:=0 to 63 do
      dib_Colors[i]:=SwapColor(dib_Colors[i]);

      if dib_Colors[15] = clWhite then
      for i:=0 to 14 do
      if dib_Colors[i] = clWhite then
      dib_Colors[i]:=EGA_Colors[i];

    end;
  finally
    f.Free
  end
end;

procedure dib_Colors_SaveAs(Path: PChar);
var
  h: Integer;
begin
  h:=FileCreate(StrPas(Path)); if h > 0 then begin
    FileWrite(h,dib_Colors,64*4); FileClose(h)
  end
end;

function dmw_Unpack_cl(cl: longint): ColorRef;
var
  r,g,b: int;
begin
  r:=cl and 31;
  g:=(cl shr 5) and 31;
  b:=(cl shr 16) and 31;
  Result:=RGB(r shl 3,g shl 3,b shl 3)
end;

function fill_Color(cl: Integer): ColorRef;
begin
  if tlong(cl).b[2] and $80 <> 0 then
    Result:=dmw_Unpack_cl(cl)
  else
    Result:=dib_Color(cl)
end;

function pen_Color(cl: Integer): ColorRef;
begin
  if tlong(cl).b[2] and $10 <> 0 then
    Result:=dmw_Unpack_cl(cl)
  else
    Result:=dib_Color(cl)
end;

function xCreateDibSection(Width,Height,Bits: Integer;
                           Colors: PColors; used: Integer;
                           out DibBits: Pointer): HBitmap;
var
  info: TVGABITMAPINFO;
begin
  Result:=0; DibBits:=nil;

  if used = 0 then Colors:=nil;
  if used > 256 then used:=256;

  Fillchar(info,Sizeof(info),0);
  with info,bmiHeader do begin
    biSize:=Sizeof(bmiHeader);
    biWidth:=Width; biHeight:=Height;
    biPlanes:=1; biCompression:=bi_RGB;
    biBitCount:=bits;

    if Assigned(Colors) then begin
      Colors_to_Quads(Colors,@bmiColors,used);
      biClrUsed:=used;
    end
  end;

  Result:=CreateDibSection(0,PBitMapInfo(@info)^,
                           DIB_RGB_COLORS, DibBits, 0,0);
end;

function Bitmap_map(bmp: TBitmap; out map: Bitmap): Boolean;
begin
  Result:=false; if Bmp.HandleAllocated then
  if GetObject(Bmp.Handle,Sizeof(map),@map) > 0 then
  Result:=true
end;

function Bitmap_ppvBits(Bmp: TBitmap): Pointer;
var
  map: Bitmap;
begin
  Result:=nil; if Bmp.HandleAllocated then
  if GetObject(Bmp.Handle,Sizeof(map),@map) > 0 then
  Result:=map.bmBits
end;

function Bitmap_Depth(Bmp: TBitmap): Integer;
begin
  case Bmp.PixelFormat of
pf8bit:
    Result:=8;
pf16bit:
    Result:=16;
pf24bit:
    Result:=24;
pf32bit:
    Result:=32;
pfDevice:
    Result:=xBitsPerPixel(0);
  end
end;

function dib_init_bitmap(bmp: TBitmap; w,h,bits: Integer): Boolean;
var
  p: Pointer;
begin
  Result:=false;
  if not bmp.HandleAllocated
  or (bmp.Width <> w)
  or (bmp.Height <> h)
  or (Bitmap_Depth(bmp) <> bits) then
  dib_clear_bitmap(bmp);

  if not bmp.HandleAllocated then
  bmp.Handle:=dib_alloc_rgb(w,h,p);

  Result:=bmp.HandleAllocated
end;

function dib_alloc_bitmap(bmp: TBitmap; w,h: Integer): Boolean;
var
  p: Pointer;
begin
  Result:=false;
  if not bmp.HandleAllocated
  or (bmp.Width <> w)
  or (bmp.Height <> h) then
  dib_clear_bitmap(bmp);

  if not bmp.HandleAllocated then
  bmp.Handle:=dib_alloc_rgb(w,h,p);

  Result:=bmp.HandleAllocated
end;

procedure dib_clear_bitmap(bmp: TBitmap);
begin
  if bmp.HandleAllocated then
  DeleteObject(bmp.ReleaseHandle);
end;

function dib_alloc_bmp(Width,Height,Bits: Integer;
                       Colors: PColors; used: Integer;
                       out DibBits: Pointer): HBitmap;
var
  info: TVGABITMAPINFO;
begin
  Result:=0; DibBits:=nil;

  if used = 0 then Colors:=nil;
  if used > 256 then used:=256;

  Fillchar(info,Sizeof(info),0);
  with info,bmiHeader do begin
    biSize:=Sizeof(bmiHeader);
    biWidth:=Width; biHeight:=Height;
    biPlanes:=1; biCompression:=bi_RGB;
    biBitCount:=bits;

    if Assigned(Colors) then begin
      Colors_to_Quads(Colors,@bmiColors,used);
      biClrUsed:=used; biBitCount:=8;
    end
  end;

  Result:=CreateDibSection(0,PBitMapInfo(@info)^,
                           DIB_RGB_COLORS, DibBits, 0,0);
end;

function dib_alloc_rgb(Width,Height: Integer;
                       out DibBits: Pointer): HBitmap;
begin
  Result:=dib_alloc_bmp(Width,Height,24,nil,0,DibBits)
end;

function dib_alloc_rgba(Width,Height: Integer;
                        out DibBits: Pointer): HBitmap;
begin
  Result:=dib_alloc_bmp(Width,Height,32,nil,0,DibBits)
end;

function dib_push_bitmap(src,dst: TBitmap): Boolean;
var
  w,h: Integer; DibBits: Pointer; R: TRect;
begin
  Result:=false;

  w:=src.Width; h:=src.Height;

  if src.HandleAllocated then
  if w*h > 0 then begin

    if not dst.HandleAllocated
    or (dst.Width <> w)
    or (dst.Height <> h) then
    dib_clear_bitmap(dst);

    if not dst.HandleAllocated then
    dst.Handle:=dib_alloc_rgb(w,h,DibBits);

    R:=Rect(0,0,w,h);

    if dst.HandleAllocated then begin
      dst.Canvas.CopyRect(R,src.Canvas,R);
      Result:=true
    end
  end
end;

function dib_pop_bitmap(src,dst: TBitmap): Boolean;
var
  w,h: Integer; DibBits: Pointer; R: TRect;
begin
  Result:=false;

  w:=src.Width; h:=src.Height;

  if src.HandleAllocated then
  if w*h > 0 then

  if dst.HandleAllocated then
  if dst.Width = w then
  if dst.Height = h then begin
    R:=Rect(0,0,w,h);
    dst.Canvas.CopyRect(R,src.Canvas,R);
    Result:=true
  end
end;

function ddw_Patterns_ptr: pointer;
begin
  Result:=FillMasks
end;

procedure ddw_Patterns_Init;
var
  i: Integer;
begin
  if FillMasks <> nil then
  for i:=0 to 15 do FillMasks[i]:=cFillMask[i];
end;

procedure dib_texture(bp: PBytes; cl,fc,pat,alf: Integer);
var
  fp: PFillMask;
  i,x,y,ax,bx,cx: Integer;
begin
  fp:=@FillMasks[pat and 63];

  Fillchar(bp^,16*16*4,0);

  for y:=0 to 15 do begin

    bx:=$8000; ax:=fp[y];
    for x:=0 to 15 do begin

      cx:=cl;
      if ax and bx = 0 then cx:=fc;

      for i:=0 to 2 do begin
        bp[0]:=tlong(cx).b[i]; bp:=@bp[1]
      end;

      if ax and bx = 0 then bp[0]:=255 - alf
      else                  bp[0]:=alf;

      bp:=@bp[1]; bx:=bx shr 1
    end
  end
end;

function BitmapIsGray(const Bmp: Bitmap): Boolean;
begin
  Result:=Assigned(Bmp.bmBits) and
          (Bmp.bmWidth > 0) and
          (Bmp.bmHeight > 0) and
          (Bmp.bmPlanes = 1) and
          (Bmp.bmBitsPixel = 8)

end;

function BitmapIsEmpty(const Bmp: Bitmap; fc: int): bool;
var
  x,y,x2,y2,bits,bx: int;
  bp: PBytes; ax: tlong;
begin
  Result:=false;

  bits:=Bmp.bmBitsPixel;
  x2:=Bmp.bmWidth-1;
  y2:=Abs(Bmp.bmHeight)-1;
  bx:=Bmp.bmWidthBytes;

  if bx > 0 then
  if bits in [24] then begin

    Result:=true; ax.i:=0;

    for y:=0 to y2 do begin
      bp:=@PBytes(Bmp.bmBits)[y*bx];

      case bits of
    24: for x:=0 to x2 do begin
          ax.b[0]:=bp[0];
          ax.b[1]:=bp[1];
          ax.b[2]:=bp[2];

          if ax.i <> fc then begin
            Result:=false; Break
          end;

          bp:=@bp[3]
        end;
      end;

      if not Result then Break
    end
  end
end;

function allocBitmap(out bmp: Bitmap; w,h,bits: int): bool;
var
  bx,cx: DWord;
begin
  Fillchar(bmp,Sizeof(bmp),0);

  bx:=(w * bits + 31) div 32;
  bx:=bx * 4; cx:=bx * h;

  if cx > 0 then begin
    bmp.bmWidth:=w;
    bmp.bmHeight:=h;
    bmp.bmWidthBytes:=bx;
    bmp.bmPlanes:=1;
    bmp.bmBitsPixel:=bits;
    bmp.bmBits:=xAllocPtr(cx);
  end;

  Result:=Assigned(bmp.bmBits)
end;

procedure freeBitmap(var bmp: Bitmap);
begin
  bmp.bmBits:=xFreePtr(bmp.bmBits);
  Fillchar(bmp,Sizeof(bmp),0);
end;

function clearBitmap(const bmp: Bitmap): Integer;
begin
  Result:=0; with bmp do
  if Assigned(bmBits) then begin
    Result:=bmWidthBytes * bmHeight;
    Fillchar(bmBits^,Result,0)
  end
end;

function likeBitmap(const src,dst: Bitmap): Integer;
begin
  Result:=0; with src do
  if bmWidth = dst.bmWidth then
  if bmHeight = dst.bmHeight then
  if bmWidthBytes = dst.bmWidthBytes then

  if Assigned(bmBits) then
  if Assigned(dst.bmBits) then
  Result:=bmWidthBytes * bmHeight;
end;

function copyBitmap(const src,dst: Bitmap): Integer;
begin
  Result:=likeBitmap(src,dst);

  if Result > 0 then
  Move(src.bmBits^,dst.bmBits^,Result)
end;

function tif_offs(dx,bits: int): int;
begin
  Result:=(dx * bits + 7) div 8;
end;

function pasteBitmap(const dst: Bitmap; dstX,dstY: int;
                     const src: Bitmap; srcX,srcY,srcW,srcH: int): bool;
var
  x,y,w,h,iw,ih,bits,si_bx,di_bx,cx: int;
  si,di: PBytes;
begin
  bits:=src.bmBitsPixel;
  iw:=src.bmWidth; ih:=src.bmHeight;

  w:=srcW; if w = 0 then w:=iw;
  h:=srcH; if h = 0 then h:=ih;

  if srcX+w > iw then w:=iw-srcX;
  if srcY+h > ih then h:=ih-srcY;

  if dstX+w > dst.bmWidth then w:=dst.bmWidth-dstX;
  if dstY+h > dst.bmHeight then h:=dst.bmHeight-dstY;

  if bits div 8 > 0 then
  if bits = dst.bmBitsPixel then
  if (w > 0) and (h > 0) then begin

    si:=src.bmBits;
    si_bx:=src.bmWidthBytes;
    si:=@si[srcY*si_bx + tif_offs(srcX,bits)];

    di:=dst.bmBits;
    di_bx:=dst.bmWidthBytes;
    di:=@di[dstY*di_bx + tif_offs(dstX,bits)];

    cx:=tif_offs(w,bits);

    for y:=1 to h do begin
      Move(si^,di^,cx);
      si:=@si[si_bx];
      di:=@di[di_bx];
    end;

    Result:=true
  end
end;

function alfaBitmap(const src,dst: Bitmap;
                    fc: Integer; alf: Float): Integer;
var
  x,y,bx,dx,r,g,b: Integer;
  si,di: PBytes; c1,c2: TYIQ;
  a1: Float;
begin
  Result:=likeBitmap(src,dst);

  if Result > 0 then begin

    r:=tlong(fc).b[0];
    g:=tlong(fc).b[1];
    b:=tlong(fc).b[2]; a1:=1 - alf;

    bx:=src.bmWidthBytes;

    for y:=0 to src.bmHeight-1 do begin

      dx:=y*bx;
      TPointer(si):=TPointer(src.bmBits)+dx;
      TPointer(di):=TPointer(dst.bmBits)+dx;

      for x:=0 to src.bmWidth-1 do begin

        if (si[0] <> b)
        or (si[1] <> g)
        or (si[2] <> r) then

        if (di[0] <> b)
        or (di[1] <> g)
        or (di[2] <> r) then begin

          b3_to_d3(di,@c1,@BGR_YIQ_T);
          b3_to_d3(si,@c2,@BGR_YIQ_T);

          c1.Y:=c1.Y*a1 + c2.Y*alf;
          c1.I:=c1.I*a1 + c2.I*alf;
          c1.Q:=c1.Q*a1 + c2.Q*alf;

          d3_to_b3(@c1,@YIQ_BGR_T,di);
        end
        else begin
          di[0]:=si[0];
          di[1]:=si[1];
          di[2]:=si[2];
        end;

        si:=@si[3]; di:=@di[3]
      end
    end
  end
end;

function mixBitmap(const src,dst: Bitmap; fc: int): int;
var
  x,y,bx,dx,r,g,b: int; si,di: PBytes;
begin
  Result:=likeBitmap(src,dst);

  if Result > 0 then begin

    r:=tlong(fc).b[0];
    g:=tlong(fc).b[1];
    b:=tlong(fc).b[2];

    bx:=src.bmWidthBytes;

    for y:=0 to src.bmHeight-1 do begin

      dx:=y*bx;
      TPointer(si):=TPointer(src.bmBits)+dx;
      TPointer(di):=TPointer(dst.bmBits)+dx;

      if src.bmBitsPixel = 8 then
        for x:=0 to src.bmWidth-1 do begin
          if di[0] = r then di[0]:=si[0];
          si:=@si[1]; di:=@di[1]
        end
      else
      if src.bmBitsPixel = 24 then
        for x:=0 to src.bmWidth-1 do begin

          if di[0] = b then
          if di[1] = g then
          if di[2] = r then begin
            di[0]:=si[0];
            di[1]:=si[1];
            di[2]:=si[2];
          end;

          si:=@si[3]; di:=@di[3]
        end
    end
  end
end;

function transBitmap(const src,dst: Bitmap; tc: int): int;
var
  x,y,bx,dx,r,g,b: int; si,di: PBytes;
begin
  Result:=likeBitmap(src,dst);

  if Result > 0 then begin

    r:=tlong(tc).b[0];
    g:=tlong(tc).b[1];
    b:=tlong(tc).b[2];

    bx:=src.bmWidthBytes;

    for y:=0 to src.bmHeight-1 do begin

      dx:=y*bx;
      TPointer(si):=TPointer(src.bmBits)+dx;
      TPointer(di):=TPointer(dst.bmBits)+dx;

      if src.bmBitsPixel = 8 then
        for x:=0 to src.bmWidth-1 do begin
          if si[0] <> r then di[0]:=si[0];
          si:=@si[1]; di:=@di[1]
        end
      else
      for x:=0 to src.bmWidth-1 do begin

        if (si[0] <> b)
        or (si[1] <> g)
        or (si[2] <> r) then begin
          di[0]:=si[0];
          di[1]:=si[1];
          di[2]:=si[2];
        end;

        si:=@si[3]; di:=@di[3]
      end
    end
  end
end;

function map32_to_map(const src,dst: Bitmap; tc: int): bool;
var
  bits,x,y,w,h,line1,line2: int; si,di: pint;
begin
  tc:=tc and $FFFFFF;

  bits:=dst.bmBitsPixel;

  w:=src.bmWidth;
  h:=src.bmHeight;

  line1:=src.bmWidthBytes;
  line2:=dst.bmWidthBytes;

  if w = dst.bmWidth then
  if h = dst.bmHeight then begin

    for y:=0 to h-1 do begin

      si:=@PBytes(src.bmBits)[y*line1];
      di:=@PBytes(dst.bmBits)[y*line2];

      if bits = 32 then
        for x:=1 to w do begin
          if (si^ and $FFFFFF) <> tc then di^:=si^;
          Inc(si); Inc(di)
        end
    end;

    Result:=true
  end
end;

procedure dib_gamma(const bmp: Bitmap; gp: PBytes);
var
  x,y,bx,w,h: int; bp,si: PBytes;
begin
  bp:=bmp.bmBits;
  bx:=bmp.bmWidthBytes;

  w:=bmp.bmWidth;
  h:=bmp.bmHeight;

  if bmp.bmBitsPixel = 8 then
    for y:=0 to h-1 do begin
      si:=@bp[y*bx];
      si[0]:=gp[si[0]];
      si:=@si[1]
    end
  else
    for y:=0 to h-1 do begin
      si:=@bp[y*bx];
      for x:=0 to w-1 do begin
        si[0]:=gp[si[0]];
        si[1]:=gp[si[1]];
        si[2]:=gp[si[2]];
        si:=@si[3]
      end
    end
end;

procedure bmp_fill_rgba(const bmp: Bitmap; w,h,cl: int);
var
  x,y,bx: int; ip: pint; map: Bitmap;
begin
  map:=bmp;
  if map.bmBitsPixel = 32 then begin
    if (w <= 0) or (w > map.bmWidth) then w:=map.bmWidth;
    if (h <= 0) or (h > map.bmHeight) then h:=map.bmHeight;

    bx:=map.bmWidthBytes;
    for y:=0 to h-1 do begin
      ip:=@PBytes(map.bmBits)[y*bx];
      for x:=1 to w do begin
        ip^:=cl; Inc(ip)
      end
    end
  end
end;

procedure bmp_alfa_swap_rgba(const bmp: Bitmap; w,h,cl: int);
var
  x,y,v: int; ip: PIntegers; map: Bitmap;
begin
  map:=bmp;
  if (w <= 0) or (w > map.bmWidth) then w:=map.bmWidth;
  if (h <= 0) or (h > map.bmHeight) then h:=map.bmHeight;

  for y:=0 to h-1 do begin
    ip:=@PBytes(map.bmBits)[y*map.bmWidthBytes];
    for x:=0 to w-1 do begin
      v:=ip[0]; if v = cl then v:=0 else
      v:=(SwapInt(v) shr 8) or $FF000000;
      ip[0]:=v; ip:=@ip[1]
    end
  end
end;

function bmp_rgb_plus_alfa(const bmp: HBitmap; fc: int): HBitmap;
var
  ax: tlong;
  x,y,w,h,bx1,bx2: int; dib: Pointer;
  m1,m2: Bitmap; bmp2: HBitmap;
  si: PBytes; di: PIntegers;
begin
  Result:=0;
  if GetObject(bmp,Sizeof(m1),@m1) > 0 then
  if m1.bmPlanes = 1 then
  if m1.bmBitsPixel = 24 then begin

    w:=Abs(m1.bmWidth);
    h:=Abs(m1.bmHeight);

    bmp2:=dib_alloc_bmp(w,h,32,nil,0,dib);

    if bmp2 <> 0 then
    if GetObject(bmp2,Sizeof(m2),@m2) > 0 then begin

      bx1:=m1.bmWidthBytes;
      bx2:=m2.bmWidthBytes;

      for y:=0 to h-1 do begin

        si:=@PBytes(m1.bmBits)[y*bx1];
        di:=@PBytes(m2.bmBits)[y*bx2];

        for x:=0 to w-1 do begin

          ax.b[0]:=si[0];
          ax.b[1]:=si[1];
          ax.b[2]:=si[2]; ax.b[3]:=0;
          if ax.i <> fc then ax.b[3]:=255;

          di[0]:=ax.i; di:=@di[1];
          si:=@si[3];
        end
      end;

      Result:=bmp2; bmp2:=0
    end
  end
end;

procedure ddw_Init;
var
  fn: TShortStr;
begin
  dib_Colors_init;

  StrUsrPath(fn,'/obj','dmw.rgb');
  dib_Colors_Load(fn);

  dibPalette32:=dib_Create_Palette(32);

  FillMasks:=xAllocPtr(SizeOf(TFillMasks));

  if FillMasks <> nil then
  FillChar(FillMasks^,SizeOf(TFillMasks),0);

  ddw_Patterns_Init;
end;

procedure ddw_Done;
begin
  FillMasks:=xFreePtr(FillMasks);
  DeleteObject(dibPalette32)
end;

initialization ddw_Init;
finalization ddw_Done;

end.