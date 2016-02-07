unit img_x; interface

uses
  Graphics,Classes,
  LCLType,LCLIntf,Math,
  otypes,ofiles,convert,
  xvirts,xgdi;

const
  img_Inline  = 1;
  img_Rotate  = 2;
  img_Index   = 4;
  img_jpeg    = 8;
  img_Pal     = 32;
  img_tiled   = 64;
  img_Relief  = 128;
  img_cmyk    = 256;
  img_fot     = 512;
  img_tiff    = 1024;
  img_tiff1   = 2048;
  img_inv     = 4096;
  img_pf32    = 8192;

  Info_Size   = 4096;

  dem_znil    = -9999;

const
  B_Bit: array[0..7] of Byte =($80,$40,$20,$10,$08,$04,$02,$01);

  L_Msk: array[0..7] of Byte = ($FF,$7F,$3F,$1F,$0F,$07,$03,$01);
  R_Msk: array[0..7] of Byte = ($80,$C0,$E0,$F0,$F8,$FC,$FE,$FF);

  icb_ext    = '.^^^';
  icb_filter = '#^^^';

type
  TImageFormat = (UNK,BMP,JPG,PNG);

  TOnStepIt = function: Boolean of object;
  TOnProgress = procedure(Count: Integer; Capt,Path: PChar) of object;
  TOnReadData = procedure(var buf; len: int) of object;

  THex_RGB = array[0..15] of TRGB_Rec;

  XImage = class
    im_w,im_h: Integer;
    h_res,v_res: Integer;

    im_bits: Integer;
    im_used: Integer;
    im_line: Integer;
    im_loc: Integer;

    is_bmp: Longbool;
    is_ijl: Longbool;
    is_tif: Longbool;
    is_cmyk: Longbool;
    is_alfa: Longbool;
    is_bgr: Longbool;
    is_wb: Longbool;

    constructor Create;
    destructor Destroy; override;

    function im_Release: Pointer;

    function GetMap(out map: Bitmap): Boolean;
    function SetMap(const Map: Bitmap): Boolean;
    function SetBitmap(Bmp: HBitmap): Boolean;

    function im_This(w,h,bits: Integer): Boolean;

    function im_Bitmap(Map: PBitmap): Boolean;

    function im_Buffer(w,h,bits: Integer;
                       Buf: Pointer; tif: Boolean): Boolean;

    function im_Buffer1(w,h,bits,line: int; Buf: Pointer): Boolean;

    function im_Create(w,h,bits: Integer): Boolean;
    function im_Alloc(w,h,bits: Integer): Boolean;
    function im_Realloc(w,h,bits: Integer): Boolean;

    function GetRect(Dest: XImage; const Rect: TRect): Boolean;

    function LoadBitmap(bmp: HBitmap): Boolean;
    function LoadFrame(bmp: HBitmap): Boolean;

    function loadRGB(const map: Bitmap): Boolean;

    function onlyRGB(bmp: HBitmap): Boolean;
    function onlyAlf(bmp: HBitmap): Boolean;

    function addRGB(bmp: HBitmap): Boolean;
    function addAlf(bmp: HBitmap): Boolean;

    function LoadMap(const map: Bitmap): Boolean;
    function LoadMap1(const map: Bitmap): Boolean;
    function LoadMap2(const map: Bitmap; bmp: bool): Boolean;
    function LoadRGB2(const map: Bitmap; bmp: bool): Boolean;

    function Clone(im: XImage): Boolean; 

    function LoadFrom(im: XImage): Boolean; virtual;
    function LoadAsBmp(im: XImage): Boolean;
    function LoadAsTiff(im: XImage): Boolean;
    function LoadPlusAlfa(im: XImage): Boolean;

    procedure CopyTo(bmp: TBitmap); virtual;

    procedure im_Fill(fc: Integer);
    procedure im_Clear(fc: Integer);
    procedure im_Close; virtual;

    procedure Set_Gray(AMax: Integer);

    function This_Colors(AColors: PColors): bool;

    procedure Set_Colors(AColors: PColors);
    procedure Set_Quads(Quads: PRGBQuads);
    procedure Set_palette(pal: HPalette);

    procedure Swap_palette;

    function im_Fill_Hist: Boolean;

    function Get_BitMapInfo(Info: PBitMapInfo): Pointer;

    function bmp_load(bmp: TBitmap): PByte;
    function gif_load(bmp: TBitmap): PByte;
    function bmp_xload(bmp: TBitmap): Boolean;

    procedure im_load(im: XImage);
    procedure im_add(im: XImage);
    procedure im_not(im: XImage);
    procedure im_swap(im: XImage);
    procedure im_copy(im: XImage);

    function im_Zoom(im: XImage; iz: Integer): Boolean;

    function Get_Pixel(x,y: Integer): Integer;

    function Set_Pixel(x,y,val: Integer): Boolean;
    procedure Set_Pixels(x1,x2,y,val: int);

    function off_Pixels(map: PBytes; line,val: int): int;

    procedure Get_Tile(x,y,w,h: Integer; tile: PBytes);
    procedure Get_Tile1(x,y,w,h,line: Integer; tile: PBytes);

    procedure cls_Tile(x,y,w,h: int);

    procedure Set_Tile(x,y,w,h: Integer; tile: PBytes); virtual;
    procedure Set_Tile1(x,y,w,h,line: Integer; tile: PBytes);
    procedure Set_Tile2(x,y: Integer; tile: XImage);

    function Get_Rect(tile: XImage; x,y,w,h: int): bool;

    procedure Set_page(ip,jp: Integer; tile: XImage);

    procedure Put_Tile(x,y,w,h: Integer; tile: PBytes);

    function Get_Dib(Info: PBitMapInfoHeader; Zoom: Integer;
                     var Rect: XRect; out lpBits: PBytes): Boolean;

    procedure Progress(Count: Integer; Capt,Path: PChar);
    function StepIt: Boolean;

    function ContainsPoint(x,y: Integer): Boolean;

    procedure Animate(AColors: PColors;
                      AUsed: Integer;
                      AChan: PBytes);

    procedure Apply_gamma(ch0,ch1,ch2,ch3: PBytes);

    procedure shift_left(dx: Integer);
    procedure shift_up(dy: Integer);

    procedure fill_test(cl: Integer);

    function IsEmpty(fc: Integer): Boolean;

    procedure Swap_24_bits;
    procedure Swap_32_bits;

    procedure Swap_Height;

    function to_pf32: bool;

    procedure Transit(lut: PBytes);

    procedure rgb_to_gray(RGB: XImage);

    function Sharpen(Threshold: Float): bool;

  protected
    fBmp: PBytes;
    
    fColors: TColors;
    fHist: TRGB_hist;
    fIs_Hist: Longbool;
    fReadonly: Longbool;

  private
    fBuffer: PBytes;
    fBufferSize: int;

    fCapacity: Integer;

    fOnlyAlloc: Longbool;

    fOnStepIt: TOnStepIt;
    fOnProgress: TOnProgress;

    procedure SetBmp(ABmp: PBytes);

    function Get_Active: Boolean;
    function Get_Is_rw: Boolean;

    function Get_Line(y: Integer): PBytes;
    function Get_Size: Int64;
    function Get_Pad: Integer;

    function Get_is_x2: Boolean;

    function Get_is_gray: Boolean;

    function Get_Offset(x,y: Integer): PBytes;

    procedure Set_Line(y: Integer; buf: PBytes);
    procedure Set_Color(I,Value: Integer);

  public
    property Active: Boolean read Get_Active;

    property Read_only: Longbool read fReadonly;
    property Is_rw: Boolean read Get_Is_rw;

    property Capacity: Integer write fCapacity;

    property bmpBits: PBytes read fBmp write SetBmp;
    property Lines[y: Integer]: PBytes read Get_Line write Set_Line;

    property Offset[x,y: Integer]: PBytes read Get_Offset;
    property im_Size: Int64 read Get_Size;
    property im_Pad: Integer read Get_Pad;

    property im_Colors: TColors read fColors;
    property im_Color[I: Integer]: Integer write Set_Color;

    property im_Hist: TRGB_Hist read fHist;
    property Is_Hist: Longbool read fIs_Hist;

    property Is_x2: Boolean read Get_is_x2;

    property Is_Gray: Boolean read Get_is_gray;

    property OnProgress: TOnProgress write fOnProgress;
    property OnStepIt: TOnStepIt write fOnStepIt;
  end;

  XBits = class(XImage)
    procedure xor_bits(bits: PBytes;
                       const R: TRect);

    procedure xor_line(x1,x2,y: Integer);
    procedure horz_line(x1,x2,y: Integer);
    function is_Up(x,y: Integer): Boolean;
    procedure Up_pixel(x,y: Integer);
  end;

  TTexture = class
    destructor Destroy; override;

    function is_x2: Boolean;

    procedure Fill(cl: int);

    function GetMap(out map: Bitmap): Boolean;

    function refBmp(Bmp: Pointer;
                    ASize,Acomp: Integer;
                    w,h,dep: Integer): Boolean;

    function Assign(Buf: Pointer;
                    ASize,Acomp: Integer;
                    w,h,dep: Integer): Pointer;

    function Alloc(ASize,Acomp: Integer;
                   w,h,dep: Integer): Boolean;

    function AllocRGB(w,h: Integer): Boolean;

    procedure Clear;

    function Load_BitMap(Bmp: TBitmap): Boolean;
    function Load_rgb(im: XImage): Boolean;

    function Load_map(const map: Bitmap; bgr: bool): Boolean;

    function map_to_rgba(const map: Bitmap; bgr: bool): bool;

    function Load_rgba(im: XImage; cw,ch: Integer): Boolean;

    procedure Opacity(Color: int);

    procedure SetTile(x,y,w,h: int; buf: PBytes);

  protected
    fImage: PBytes;
    fBuffer: PBytes;

    fComp: DWord;
    fWidth: Integer;
    fHeight: Integer;
    fDepth: Integer;

    fCapacity: Integer;
    fSize: Integer;

    function Get_Active: Boolean;

  public
    property Active: Boolean read Get_Active;

    property Comp: DWord read fComp;

    property Image: PBytes read fImage;
    property Width: Integer read fWidth;
    property Height: Integer read fHeight;
    property Size: Integer read fSize;

    property Depth: Integer read fDepth
                            write fDepth;
  end;

function xCreateDibBitMap(Width,Height,Bits: Integer;
                          out ppvBits: Pointer): hBitMap;

function img_Size(Width,Height,Bits: Integer): Integer;
function Bitmap_Size(const Bmp: Bitmap): Integer;

function tif_Line(Width,Bits: Integer): Integer;
function img_Line(Width,Bits: Integer): Integer;
function pad_Line(Width,Bits: Integer): Integer;

function img_Line_ofs(X,Bits: Integer): Integer;

function img_Bits(info: PBitMapInfoHeader): Integer;
function img_Planes(info: PBitMapInfoHeader): Integer;
function img_Square(info: PBitMapInfoHeader): longint;

procedure img_Set_Pixel(buf: PBytes; x,bits,pix: Integer);

procedure hex_Transit(buf: PBytes; chan: PBytes; Count: Integer);
procedure vga_Transit(buf: PBytes; chan: PBytes; Count: Integer);

procedure bmp_Transit(buf: PBytes; chan: PBytes;
                      Info: PBitMapInfoHeader);

procedure plus_gamma(a0,a1,a2,a3: PBytes;
                     b0,b1,b2,b3: PBytes);

procedure bmp_Gamma(buf: PBytes;
                    ch0,ch1,ch2,ch3: PBytes;
                    Info: PBitMapInfoHeader);

procedure bmp_Gamma1(buf: PBytes;
                     w,h,bits,line: Integer;
                     ch0,ch1,ch2,ch3: PBytes);

function bmp_Gamma2(const bmp: Bitmap;
                    ch0,ch1,ch2,ch3: PBytes): Boolean;

procedure mac12_to_pf8(src,dst: pointer; Count: Integer);

procedure pf16_to_pf8a(src,dst: pointer; Count: int);         // x86
procedure pf16_to_pf8af(src,dst: pointer; Count,Factor: int); // x86

procedure pf16_to_pf8b(src,dst: pointer; Count: int);         // mac
procedure pf16_to_pf8bf(src,dst: pointer; Count,Factor: int); // mac

procedure pf16_to_pf8_scale(src,dst: Pointer;
                            Count,MaxValue: int);

procedure pf16_to_pf8_scalei(src,dst: Pointer;
                             Count,MinValue,MaxValue: int);

procedure pf32_to_pf8_scale(src,dst: Pointer;
                            Count,MaxValue: int);

procedure float_to_pf8(src,dst: Pointer; Count: int; v1,v2: float);

procedure pf64_to_pf24(src,dst: Pointer;
                       Count,Planes,MaxValue: int);

procedure pf48_to_pf24(src,dst: pointer; Count: Integer);
procedure pf16_to_pf24(src,dst: pointer; Count: Integer);
procedure pf32_to_pf24(src,dst: pointer; Count: Integer);
procedure pf24_to_pf32(src,dst: pointer; Count: Integer);
procedure pf8_to_pf4(src,dst: pointer; Count: Integer);
procedure pf8_to_pf1(src,dst: pointer; Count: Integer);
procedure pf1_to_pf8(src,dst: pointer; Count: Integer);
procedure pf4_to_pf8(src,dst: pointer; Count: Integer);
procedure pf4_to_pf1(src,dst: PBytes; Count: Integer);

procedure pf4_to_pf24(src,dst: pointer;
                      Count: Integer; Quads: PRgbQuads);

procedure Pack_tile_bytes(buf: Pointer; w,h,x_step,y_step: Integer);
procedure Pack_tile_rgb(buf: Pointer; w,h,x_skip,x_line: Integer);

procedure Pack_tile_bmp(di,si: PBytes;
                        w,h, loc,line: Integer;
                        stepx,stepy: Integer);

procedure xor_image(buf: PBytes; Size: Integer);

procedure swap_rgb(buf: PBytes; Count: Integer);
procedure x_swap_rgb(buf: PBytes; w,h: Integer);

procedure map_swap_rgb(const map: Bitmap);

procedure rgb_to_rgba(Source,Dest: Pointer; Count: Integer);

procedure load_bmp(di,si: PBytes; di_line,si_line,Count: int);

procedure bmp_to_buf(buf: PBytes; w,h,bits: Integer);

procedure Zoom_pf1(si,di: PBytes; Count,Zoom: Integer);
procedure Zoom_pf4(si,di: PBytes; Count,Zoom: Integer);
procedure Zoom_pf8(si,di: PBytes; Count,Zoom: Integer);
procedure Zoom_pf16(si,di: PBytes; Count,Zoom: Integer);
procedure Zoom_pf24(si,di: PBytes; Count,Zoom: Integer);
procedure Zoom_pf32(si,di: PBytes; Count,Zoom: Integer);

procedure Zoom_bmp(buf: PBytes; Zoom: Integer;
                   Width0,Width,Height,Bits: Integer);

procedure x_zoom_bmp(si,di: PBytes; Zoom: Integer;
                     Width,Height,Bits: Integer);
                   
procedure img_swap_height(bmp: PBytes; h,line: Integer);
procedure tile_swap_height(bmp: PBytes; h,line: Integer);
procedure map_swap_height(const map: Bitmap);
function bmp_swap_height(bmp: HBitmap; out sz: Integer): Pointer;

procedure bmp_diff(ds: PBytes; Cols,Rows,Bits,Line: Integer);

function img_Get_Hex(h,pos: Integer; chan: PBytes): longint;

function img_Get_icb(Path: PChar;
                     icb: PImageParams;
                     IsHist: Boolean): Boolean;

function img_Get_gam(Path: PChar;
                     out gam: TImageGamma): Boolean;

procedure app_Gamma(var gam: TImageGamma;
                    var ch0,ch1,ch2,ch3: PBytes);
                    
procedure img_Put_icb(Path: PChar; const icb: TImageParams);
procedure img_Put_gam(Path: PChar; const gam: TImageGamma);

function Hist_sum(const Hist: TRGB_Hist): Int64;
function Hist_okay(const Hist: TRGB_Hist; w,h: Integer): Boolean;

procedure Init_Hist(out icb: TImageParams);

procedure Fill_Hist(var Hist: TRGB_Hist;
                    buf: PBytes; len: Integer);

procedure Order_Hist(const Hist: TRGB_Hist; Chan: PBytes);

procedure Hex_Swap_Quads(Quads: pRGBQuads; chan: PBytes);

procedure Unpack_Planes(dst,src: PBytes; Info: PBitMapInfoHeader);

implementation

uses
  SysUtils,xddw;

function xCreateDibBitMap(Width,Height,Bits: Integer;
                          out ppvBits: Pointer): hBitMap;
var
  inf: TBitmapInfoHeader;
begin
  Fillchar(inf,Sizeof(inf),0);
  with inf do begin
    biSize:=Sizeof(inf);
    biWidth:=Width; biHeight:=Height;
    biPlanes:=1; biCompression:=bi_RGB;
    biBitCount:=Bits
  end;

  Result:=CreateDibSection(0,PBitMapInfo(@inf)^,
                           DIB_RGB_COLORS, ppvBits, 0,0);
end;

function img_Size(Width,Height,Bits: Integer): Integer;
begin
  Result:=img_Line(Width,Bits) * Height
end;

function Bitmap_Size(const Bmp: Bitmap): Integer;
begin
  with Bmp do
  Result:=img_Line(bmWidth,bmBitsPixel) * bmHeight
end;

function tif_Line(Width,Bits: Integer): Integer;
begin
  Result:=(Width * Bits + 7) div 8;
end;

function img_Line(Width,Bits: Integer): Integer;
begin
  Result:=(Width * Bits + 31) div 32;
  Result:=Result * 4
end;

function pad_Line(Width,Bits: Integer): Integer;
begin
  Result:=img_Line(Width,Bits) -
          ((Width * Bits + 7) div 8)
end;

function img_Line_ofs(X,Bits: Integer): Integer;
begin
  Result:=X * Bits div 8
end;

function img_Bits(info: PBitMapInfoHeader): Integer;
begin
  with Info^ do Result:=biBitCount * biPlanes
end;

function img_Planes(info: PBitMapInfoHeader): Integer;
begin
  with Info^ do Result:=biBitCount * biPlanes div 8
end;

function img_Square(info: PBitMapInfoHeader): longint;
begin
  with Info^ do Result:=biWidth*biHeight
end;

procedure img_Set_Pixel(buf: PBytes; x,bits,pix: Integer);
var
  ind: Integer; bit: byte;
begin
  case bits of
1:  begin
      ind:=x div 8; bit:=B_Bit[x and 7];
      if pix = 1 then buf[ind]:=buf[ind] or bit
      else buf[ind]:=buf[ind] and (bit xor $FF)
    end;
8:  buf[x]:=pix
  end
end;

{$ASMMODE INTEL}

procedure hex_Transit(buf: PBytes; chan: PBytes; Count: int);
asm
{$ifdef CPUX86_64}
  xchg RDi,RSi
  mov  RCx,RDx
  shr  RCx,1

  cld
  mov  RDx,0
@loop:
  mov  Ah,BYTE PTR [RDi]
  mov  Dl,Ah
  shr  Dl,4
  mov  Al,BYTE PTR [RSi][RDx]
  shr  Ah,4
  mov  Dl,Ah
  mov  Ah,Al
  shl  Ah,4
  mov  Al,BYTE PTR [RSi][RDx]
  or   Al,Ah
  stosb
  loop @loop

{$else}
  push EBx
  push EDi
  Push ESi

  mov  EDi,EAx
  mov  ESi,EDx

  cld
  shr  ECx,1
  mov  EBx,0
@loop:
  mov  Ah,BYTE PTR [EDi]
  mov  Bl,Ah
  shr  Bl,4
  mov  Al,BYTE PTR [ESi][EBx]
  shr  Ah,4
  mov  Bl,Ah
  mov  Ah,Al
  shl  Ah,4
  mov  Al,BYTE PTR [ESi][EBx]
  or   Al,Ah
  stosb
  loop @loop

  pop  ESi
  pop  EDi
  pop  EBx
{$endif}
end;

procedure vga_Transit(buf: PBytes; chan: PBytes; Count: Integer);
asm
{$ifdef CPUX86_64}
  xchg RDi,RSi
  mov  RCx,RDx
  shr  RCx,1

  cld
  mov  RDx,0
@loop:
  mov  Dl,BYTE PTR [RDi]
  mov  Al,BYTE PTR [RSi][RDx]
  stosb
  loop @loop

{$else}
  push EBx
  push EDi
  Push ESi

  mov  EDi,EAx
  mov  ESi,EDx

  cld
  mov  EBx,0
@loop:
  mov  Bl,BYTE PTR [EDi]
  mov  Al,BYTE PTR [ESi][EBx]
  stosb
  loop @loop

  pop  ESi
  pop  EDi
  pop  EBx
{$endif}
end;

procedure bmp_Transit(buf: PBytes; chan: PBytes;
                      Info: PBitMapInfoHeader);
var
  di: PBytes; bits,x,y,w,pad: Integer;
begin
  bits:=img_Bits(Info);

  with Info^ do
  if bits in [8,24] then begin

    w:=biWidth * (bits div 8);
    pad:=img_Line(biWidth,bits) - w;

    di:=buf;
    for y:=1 to biHeight do begin

      for x:=1 to w do begin
        di[0]:=chan[ di[0] ];  di:=@di[1]
      end;

      di:=@di[pad]
    end
  end
end;

procedure plus_gamma(a0,a1,a2,a3: PBytes;
                     b0,b1,b2,b3: PBytes);
var
  i: Integer;
begin
  if Assigned(a0) then
  if Assigned(b0) then begin

    if b1 = nil then b1:=b0;
    if b2 = nil then b2:=b0;
    if b2 = nil then b3:=b0;

    if a1 = a0 then a1:=nil;
    if a2 = a0 then a2:=nil;
    if a3 = a0 then a3:=nil;

    for i:=0 to 255 do begin
      a0[i]:=b0[a0[i]];
      if Assigned(a1) then a1[i]:=b1[a1[i]];
      if Assigned(a2) then a2[i]:=b2[a2[i]];
      if Assigned(a3) then a3[i]:=b3[a3[i]];
    end
  end
end;

procedure bmp_Gamma(buf: PBytes;
                    ch0,ch1,ch2,ch3: PBytes;
                    Info: PBitMapInfoHeader);
var
  di: PBytes; bits,x,y,pad: Integer;
begin
  bits:=img_Bits(Info);

  if bits = 8 then
    bmp_Transit(buf,ch0,Info)
  else
  if bits = 24 then
  with Info^ do begin

    if ch1 = nil then ch1:=ch0;
    if ch2 = nil then ch2:=ch0;
    if ch3 = nil then ch3:=ch0;

    pad:=pad_Line(biWidth,bits);

    di:=buf;
    for y:=1 to biHeight do begin

      for x:=1 to biWidth do begin
        di[0]:=ch3[di[0]];
        di[1]:=ch2[di[1]];
        di[2]:=ch1[di[2]];
        di:=@di[3]
      end;

      di:=@di[pad]
    end
  end
end;

procedure bmp_Gamma1(buf: PBytes;
                     w,h,bits,line: Integer;
                     ch0,ch1,ch2,ch3: PBytes);
var
  di: PBytes; i,n,x,y,pad: Integer;
begin
  di:=buf;

  if bits = 8 then begin

    n:=line*h;

    for i:=1 to n do begin
      di[0]:=ch0[di[0]]; di:=@di[1]
    end

  end else
  if bits = 24 then begin

    if ch1 = nil then ch1:=ch0;
    if ch2 = nil then ch2:=ch0;
    if ch3 = nil then ch3:=ch0;

    pad:=line - w*3;
    for y:=1 to h do begin

      for x:=1 to w do begin
        di[0]:=ch3[di[0]];
        di[1]:=ch2[di[1]];
        di[2]:=ch1[di[2]];
        di:=@di[3]
      end;

      di:=@di[pad]
    end
  end
end;

function bmp_Gamma2(const bmp: Bitmap;
                    ch0,ch1,ch2,ch3: PBytes): Boolean;
begin
  Result:=false;

  if Assigned(ch0) then

  with bmp do
  if Assigned(bmBits) then
  if bmBitsPixel*bmPlanes = 24 then begin

    bmp_Gamma1(bmBits,bmWidth,bmHeight,
               24,img_Line(bmWidth,24),
               ch0,ch1,ch2,ch3);

    Result:=true
  end
end;

procedure mac12_to_pf8(src,dst: pointer; Count: Integer);
asm
{$ifdef CPUX86_64}
  xchg RSi,RDi
  mov  RCx,RDx
{$else}
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx
{$endif}

  cld
@loop:
  lodsw
  xchg Ah,Al
  shr  Ax,4
  cmp  Ax,255
  jle  @skip
  mov  Ax,255
@skip:
  stosb
  loop @loop

{$ifdef CPU386}
  pop  EDi
  pop  ESi
{$endif}
end;

procedure pf16_to_pf8a(src,dst: pointer; Count: int);  // x86
asm
{$ifdef CPUX86_64}
  xchg RSi,RDi
  mov  RCx,RDx
{$else}
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx
{$endif}

  cld
@loop:
  lodsw
  stosb
  loop @loop

{$ifdef CPU386}
  pop  EDi
  pop  ESi
{$endif}
end;

procedure pf16_to_pf8af(src,dst: pointer; Count,Factor: int);
asm
{$ifdef CPUX86_64}
  push RBx

  xchg RSi,RDi
  xchg RDx,RCx
  mov  RBx,RDx
{$else}
  push ESi
  push EDi
  push EBx

  mov  ESi,EAx
  mov  EDi,EDx

  mov  EBx,[Factor]
{$endif}

  cld
  mov  EAx,0
@loop:
  lodsw
  cdq
  idiv EBx
  stosb
  loop @loop

{$ifdef CPUX86_64}
  pop  RBx
{$else}
  pop  EBx
  pop  EDi
  pop  ESi
{$endif}
end;

procedure pf16_to_pf8b(src,dst: pointer; Count: int); // mac
asm
{$ifdef CPUX86_64}
  xchg RSi,RDi
  mov  RCx,RDx
{$else}
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx
{$endif}

  cld
@loop:
  lodsw
  xchg Ah,Al
  stosb
  loop @loop

{$ifdef CPU386}
  pop  EDi
  pop  ESi
{$endif}
end;

procedure pf16_to_pf8bf(src,dst: pointer; Count,Factor: int);
asm
{$ifdef CPUX86_64}
  pop  RBx

  xchg RSi,RDi
  xchg RDx,RCx
  mov  RBx,RDx
{$else}
  push ESi
  push EDi
  push EBx

  mov  ESi,EAx
  mov  EDi,EDx

  mov  EBx,[Factor]
{$endif}

  shl  EBx,16

  cld
  mov  EAx,0
@loop:
  lodsw
  BSWAP EAx
  cdq
  idiv EBx
  stosb
  loop @loop
@skip:

{$ifdef CPUX86_64}
  pop  RBx
{$else}
  pop  EBx
  pop  EDi
  pop  ESi
{$endif}
end;

procedure pf16_to_pf8_scale(src,dst: Pointer; Count,MaxValue: int);
var
  ax: tlong; kv: int; si: PWords; di: PBytes;
begin
  if MaxValue = 0 then
    pf16_to_pf8a(src,dst,Count)
  else begin
    kv:=255 * $10000 div MaxValue;

    si:=src; di:=dst;
    while Count > 0 do begin
      ax.i:=si[0] * kv; di[0]:=ax.w[1];
      si:=@si[1]; di:=@di[1]; Dec(Count)
    end
  end
end;

procedure pf16_to_pf8_scalei(src,dst: Pointer;
                             Count,MinValue,MaxValue: int);
var
  ax: tlong; kv: int; si: PSmallints; di: PBytes;
begin
  if MinValue > MaxValue then
  if MinValue and $8000 = 0 then
    MaxValue:=0
  else begin
    MinValue:=MinValue or $FFFF0000;
    if MinValue > MaxValue then MaxValue:=0
    else Dec(MaxValue,MinValue)
  end;

  if MaxValue <= 0 then
    pf16_to_pf8a(src,dst,Count)
  else begin
    kv:=255 * $10000 div MaxValue;

    si:=src; di:=dst;
    while Count > 0 do begin
      ax.i:=si[0]; si:=@si[1];
      if ax.i = -32767 then
        ax.i:=0
      else
        ax.i:=(ax.i-MinValue) * kv;
         
      di[0]:=ax.w[1]; di:=@di[1]; Dec(Count)
    end
  end
end;

procedure pf32_to_pf8_scale(src,dst: Pointer;
                            Count,MaxValue: int);
var
  v: int; si: PIntegers; di: PBytes; kv: double;
begin
  si:=src; di:=dst;

  if MaxValue = 0 then
    while Count > 0 do begin
      di[0]:=si[0]; di:=@di[1];
      si:=@si[1]; Dec(Count)
    end
  else begin
    kv:=1/MaxValue;
    while Count > 0 do begin
      v:=Round(si[0]*kv);
      if v > 255 then v:=255;
      di[0]:=v; di:=@di[1];
      si:=@si[1]; Dec(Count)
    end
  end
end;

procedure float_to_pf8(src,dst: Pointer; Count: int; v1,v2: float);
var
  si: PFloats; di: PBytes; i,h: int; v0,kv: float;
begin
  v0:=0; kv:=1;
  if v1+1 < v2 then begin
    v0:=v1; kv:=255/(v2-v1)
  end;

  si:=src; di:=dst;
  for i:=0 to Count-1 do begin
    h:=Round((si[0]-v0)*kv);
    if h < 0 then h:=0;
    if h > 255 then h:=255;
    di[0]:=h; di:=@di[1];
    si:=@si[1];
  end
end;

procedure pf64_to_pf24(src,dst: Pointer;
                       Count,Planes,MaxValue: int);
var
  ax: tlong; kv: int;
  si,si1,si2,si3: PWords; di: PBytes;
begin
  if MaxValue = 0 then MaxValue:=256;
  kv:=255 * $10000 div MaxValue;

  si:=src; di:=dst;

  if Planes = 1 then
    while Count > 0 do begin
      ax.i:=si[0] * kv; di[0]:=ax.w[1];
      ax.i:=si[1] * kv; di[1]:=ax.w[1];
      ax.i:=si[2] * kv; di[2]:=ax.w[1];
      si:=@si[4]; di:=@di[3]; Dec(Count)
    end
  else begin
    si1:=si;
    si2:=@si1[Count];
    si3:=@si2[Count];

    while Count > 0 do begin
      ax.i:=si1[0] * kv; di[0]:=ax.w[1];
      ax.i:=si2[1] * kv; di[1]:=ax.w[1];
      ax.i:=si3[2] * kv; di[2]:=ax.w[1];
      si1:=@si1[1]; si2:=@si2[1]; si3:=@si3[1];
      di:=@di[3]; Dec(Count)
    end
  end
end;

procedure pf48_to_pf24(src,dst: pointer; Count: Integer);
asm
{$ifdef CPUX86_64}
  xchg RSi,RDi
  mov  RCx,RDx
{$else}
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx
{$endif}

  mov  EAx,ECx
  add  ECx,EAx
  add  ECx,EAx

  cld
@loop:
  lodsw
  shr  Ax,8
  stosb
  loop @loop

{$ifdef CPU386}
  pop  EDi
  pop  ESi
{$endif}
end;

procedure pf16_to_pf24(src,dst: pointer; Count: Integer);
asm
{$ifdef CPUX86_64}
  xchg RSi,RDi
  mov  RCx,RDx
{$else}
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx
{$endif}

  cld
@loop:
  lodsw
  mov  Dx,Ax

  shl  Al,3
  stosb

  mov  Ax,Dx
  shr  Ax,5
  shl  Al,3
  stosb

  mov  Al,Dh
  shr  Al,2
  shl  Al,3
  stosb

  loop @loop

{$ifdef CPU386}
  pop  EDi
  pop  ESi
{$endif}
end;

procedure pf32_to_pf24(src,dst: pointer; Count: Integer);
asm
{$ifdef CPUX86_64}
  xchg RSi,RDi
  mov  RCx,RDx
{$else}
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx
{$endif}

  cld
@loop:
  lodsd
  BSWAP EAx

  shr   EAx,8
  stosb
  shr   EAx,8
  stosb
  shr   EAx,8
  stosb

  loop @loop

{$ifdef CPU386}
  pop  EDi
  pop  ESi
{$endif}
end;

procedure pf24_to_pf32(src,dst: pointer; Count: Integer);
asm
{$ifdef CPUX86_64}
  xchg RSi,RDi
  mov  RCx,RDx
{$else}
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx
{$endif}

  cld
@loop:
  mov EAx,0
  lodsb
  shl   EAx,8
  lodsb
  shl   EAx,8
  lodsb

  stosd

  loop @loop

{$ifdef CPU386}
  pop  EDi
  pop  ESi
{$endif}
end;

procedure pf8_to_pf4(src,dst: pointer; Count: Integer);
asm
{$ifdef CPUX86_64}
  xchg RSi,RDi
  mov  RCx,RDx
{$else}
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx
{$endif}

  shr  ECx,1

  cld
@loop:
  lodsb
  mov  Ah,Al
  shl  Ah,4
  lodsb
  and  Al,0Fh
  or   Al,Ah
  stosb
  loop @loop

{$ifdef CPU386}
  pop  EDi
  pop  ESi
{$endif}
end;

procedure pf8_to_pf1(src,dst: pointer; Count: Integer);
asm
{$ifdef CPUX86_64}
  xchg RSi,RDi
  mov  RCx,RDx
{$else}
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx
{$endif}

  mov  Dx,8

  cld
@loop:
  shl  Dh,1
  lodsb
  cmp  Al,0
  jz   @skip
  or   Dh,1
@skip:
  dec  Dl
  jnz  @next
  mov  Al,Dh
  mov  Dx,8
  stosb
@next:
  loop @loop

{$ifdef CPU386}
  pop  EDi
  pop  ESi
{$endif}
end;

procedure pf1_to_pf8(src,dst: pointer; Count: Integer);
var
  i,ax,bit: int; si,di: PBytes;
begin
  si:=src; di:=dst;
  ax:=si[0]; bit:=$80;

  for i:=1 to Count do begin

    if bit = 0 then begin
      si:=@si[1]; ax:=si[0]; bit:=$80
    end;

    if ax and bit <> 0 then
    di[0]:=255 else di[0]:=0;

    bit:=bit shr 1;
    di:=@di[1]
  end
end;

procedure pf4_to_pf8(src,dst: pointer; Count: Integer);
asm
{$ifdef CPUX86_64}
  xchg RSi,RDi
  mov  RCx,RDx
{$else}
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx
{$endif}

  shr  ECx,1

  cld
@loop:
  lodsb
  mov  Ah,Al

  shr  Al,4
  stosb
  mov  Al,Ah
  and  Al,0Fh
  stosb
  loop @loop

{$ifdef CPU386}
  pop  EDi
  pop  ESi
{$endif}
end;

procedure pf4_to_pf1(src,dst: PBytes; Count: Integer);
var
  si,di: PBytes;
  i,cx: Integer; al,bl,bit: Byte;
begin
  si:=src; di:=dst;

  cx:=int_Round(Count,2);
  while cx > 0 do begin
    al:=0; bit:=$80;

    for i:=1 to 4 do
    if cx > 0 then begin
      bl:=si[0]; si:=@si[1]; Dec(cx);

      if bl and $F0 <> 0 then
      Inc(al,bit); bit:=bit shr 1;

      if bl and $0F <> 0 then
      Inc(al,bit); bit:=bit shr 1;
    end;

    di[0]:=al; di:=@di[1]
  end
end;

procedure pf4_to_pf24(src,dst: pointer;
                      Count: Integer; Quads: PRgbQuads);
var
  i: Integer; si,di: PBytes; qu: TRGBQuad;
begin
  i:=Count; si:=src; di:=dst;
  while i > 0 do begin
    qu:=Quads[si[0] shr 4];

    di[0]:=qu.rgbBlue;
    di[1]:=qu.rgbGreen;
    di[2]:=qu.rgbRed; di:=@di[3];

    qu:=Quads[si[0] and $F];
    di[0]:=qu.rgbBlue;
    di[1]:=qu.rgbGreen;
    di[2]:=qu.rgbRed; di:=@di[3];

    si:=@si[1]; Dec(i,2)
  end
end;

procedure Pack_tile_bytes(buf: Pointer; w,h,x_step,y_step: Integer);
var
  x,y: int; si,di: PByte;
begin
  si:=buf; di:=buf;
  for y:=1 to h do begin

    for x:=1 to w do begin
      di^:=si^; Inc(di); Inc(si);
      Inc(si,x_step);
    end;

    Inc(si,y_step)
  end
end;

procedure Pack_tile_rgb(buf: Pointer; w,h,x_skip,x_line: Integer);
var
  x,y: int; si,si1,di: PByte;
begin
  si:=buf; di:=buf;

  for y:=1 to h do begin

    si1:=si;
    for x:=1 to w do begin
      di^:=si1^; Inc(di); Inc(si1);
      di^:=si1^; Inc(di); Inc(si1);
      di^:=si1^; Inc(di); Inc(si1);
      Inc(si,x_skip);
    end;

    Inc(si,x_line)
  end
end;

procedure Pack_tile_bmp(di,si: PBytes;
                        w,h, loc,line: Integer;
                        stepx,stepy: Integer);
var
  _si,_di: PBytes;
  x,y, dx, si_line,di_line: Integer;
begin
  si_line:=line*stepy;
  di_line:=int_Round(w*loc,4);

  dx:=stepx*loc;
  for y:=1 to h do begin

    _si:=si; _di:=di;

    case loc of
  1:  for x:=1 to w do begin
        _di[0]:=_si[0];
        _si:=@_si[dx]; _di:=@_di[1]
      end;
  2:  for x:=1 to w do begin
        PWord(_di)^:=PWord(_si)^;
        _si:=@_si[dx]; _di:=@_di[2]
      end;
  3:  for x:=1 to w do begin
        PWord(_di)^:=PWord(_si)^; _di[2]:=_si[2];
        _si:=@_si[dx]; _di:=@_di[3]
      end;
  4:  for x:=1 to w do begin
        PLongint(_di)^:=PLongint(_si)^;
        _si:=@_si[dx]; _di:=@_di[4]
      end;
    end;

    si:=@si[si_line];
    di:=@di[di_line];
  end
end;

procedure xor_image(buf: PBytes; Size: Integer);
var
  i: Integer;
begin
  for i:=1 to Size do begin
    buf[0]:=buf[0] xor $FF; buf:=@buf[1]
  end
end;

procedure swap_rgb(buf: PBytes; Count: Integer);
asm
{$ifdef CPUX86_64}
  mov  RCx,RSi
  mov  RSi,RDi

  cld
@loop:
  mov  Al,BYTE PTR [RSi]
  inc  RSi
  mov  Ah,BYTE PTR [RSi]
  inc  RSi
  mov  Bl,BYTE PTR [RSi]
  inc  RSi

  mov  BYTE PTR [RDi],Bl
  inc  RDi
  mov  BYTE PTR [RDi],Ah
  inc  RDi
  mov  BYTE PTR [RDi],Al
  inc  RDi
loop @loop

{$else}
  push EBx
  push EDi
  push ESi

  cld
  mov  EDi,EAx
  mov  ESi,EAx

  mov  ECx,EDx
@loop:
  mov  Al,BYTE PTR [ESi]
  inc  ESi
  mov  Ah,BYTE PTR [ESi]
  inc  ESi
  mov  Bl,BYTE PTR [ESi]
  inc  ESi

  mov  BYTE PTR [EDi],Bl
  inc  EDi
  mov  BYTE PTR [EDi],Ah
  inc  EDi
  mov  BYTE PTR [EDi],Al
  inc  EDi

  loop @loop

  pop  ESi
  pop  EDi
  pop  EBx
{$endif}
end;

procedure x_swap_rgb(buf: PBytes; w,h: Integer);
var
  y,line: Integer;
begin
  line:=img_line(w,24);
  for y:=1 to h do begin
    swap_rgb(buf,w); buf:=@buf[line]
  end
end;

procedure map_swap_rgb(const map: Bitmap);
begin
  with map do
  if Assigned(bmBits) then

  if pad_Line(bmWidth,bmBitsPixel) <> 0 then
    x_swap_rgb(bmBits,bmWidth,bmHeight)
  else
    swap_rgb(bmBits,bmWidth*bmHeight)
end;

procedure rgb_to_rgba(Source,Dest: Pointer; Count: Integer);
asm
{$ifdef CPUX86_64}
  push RBx

  xchg RSi,RDi
  mov  RCx,RDx
{$else}
  push EBx
  push EDi
  push ESi

  mov  ESi,EAx
  mov  EDi,EDx
{$endif}

  cld
  mov  Ax,0
@loop:
  mov  Bx,0
  lodsb
  stosb
  add  Bx,Ax
  lodsb
  stosb
  add  Bx,Ax
  lodsb
  stosb
  add  Bx,Ax

  mov  Al,0
  cmp  Bx,0
  je   @alfa
  mov  Al,255
@alfa:
  stosb
  loop @loop

{$ifdef CPUX86_64}
  pop  RBx
{$else}
  pop  ESi
  pop  EDi
  pop  EBx
{$endif}
end;

procedure load_bmp(di,si: PBytes; di_line,si_line,Count: int);
var
  i: int;
begin
  si:=@si[(Count-1) * si_Line];
  for i:=1 to Count do begin
    Move(si[0],di[0],di_Line);
    si:=@si[-si_Line];
    di:=@di[di_Line]
  end
end;

procedure bmp_to_buf(buf: PBytes; w,h,bits: Integer);
var
  si,di: PBytes; si_line,di_line,y: Integer;
begin
  si_line:=(w*bits + 31) div 32;
  si_line:=si_line * 4;

  di_line:=(w*bits + 7) div 8;
  if di_line < si_line then begin

    si:=buf; di:=buf;
    for y:=1 to h do begin
      Move(si^,di^,di_line);
      si:=@si[si_line];
      di:=@di[di_line];
    end;
  end
end;

procedure dib_lines(_Si,_Di: PBytes;
                    Count,Bytes: Integer;
                    si_line,di_line: Integer);
var
  i: Integer;
begin
  for i:=1 to Count do begin
    Move(_Si^,_Di^,Bytes);
    _Si:=@_Si[si_line];
    _Di:=@_Di[di_line]
  end
end;

procedure Zoom_pf1(si,di: PBytes; Count,Zoom: Integer);
var
  i,j,bit1,bit2,k: Integer;
begin
  k:=Count div 8;
  if k*8 < Count then Inc(k);
  Fillchar(di^,k,0);

  bit1:=$80; bit2:=$80;
  for i:=1 to Count do begin

    if si[0] and bit1 <> 0 then
    di[0]:=di[0] or bit2;

    bit2:=bit2 shr 1;
    if bit2 = 0 then begin
      bit2:=$80; di:=@di[1];
    end;

    for j:=1 to Zoom do begin
      bit1:=bit1 shr 1; if bit1 = 0 then
      begin bit1:=$80; si:=@si[1] end
    end
  end
end;

procedure Zoom_pf4(si,di: PBytes; Count,Zoom: Integer);
asm
{$ifdef CPUX86_64}
  xchg RSi,RDi
  xchg RCx,RDx
{$else}
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx

  mov  EDx,[Zoom]
{$endif}

  shr  ECx,1

  shr  EDx,1
  dec  EDx

  cld
@loop:
  lodsb
  add  ESi,EDx

  and  Al,0F0h
  mov  Ah,Al

  lodsb
  add  ESi,EDx

  and  Al,0Fh
  or   Al,Ah

  stosb
  loop @loop

{$ifdef CPU386}
  pop  EDi
  pop  ESi
{$endif}
end;

procedure Zoom_pf8(si,di: PBytes; Count,Zoom: Integer);
asm
{$ifdef CPUX86_64}
  xchg RSi,RDi
  xchg RCx,RDx
{$else}
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx

  mov  EDx,[Zoom]
{$endif}

  dec  EDx

  cld
@loop:
  lodsb
  stosb
  add  ESi,EDx
  loop @loop

{$ifdef CPU386}
  pop  EDi
  pop  ESi
{$endif}
end;

procedure Zoom_pf16(si,di: PBytes; Count,Zoom: Integer);
asm
{$ifdef CPUX86_64}
  xchg RSi,RDi
  xchg RCx,RDx

  dec  EDx
  add  EDx,EDx

  cld
@loop:
  movsw
  add  RSi,RDx
  loop @loop

{$else}
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx

  mov  EDx,[Zoom]
  dec  EDx
  add  EDx,EDx

  cld
@loop:
  movsw
  add  ESi,EDx
  loop @loop

  pop  EDi
  pop  ESi
{$endif}
end;

procedure Zoom_pf24(si,di: PBytes; Count,Zoom: Integer);
asm
{$ifdef CPUX86_64}
  xchg RSi,RDi
  xchg RCx,RDx

  dec  EDx
  mov  EAx,EDx
  add  EDx,EAx
  add  EDx,EAx

  cld
@loop:
  movsb
  movsb
  movsb

  add  RSi,RDx
  loop @loop

{$else}
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx

  mov  EDx,[Zoom]
  dec  EDx
  mov  EAx,EDx
  add  EDx,EAx
  add  EDx,EAx

  cld
@loop:
  movsb
  movsb
  movsb

  add  ESi,EDx
  loop @loop

  pop  EDi
  pop  ESi
{$endif}
end;

procedure Zoom_pf32(si,di: PBytes; Count,Zoom: Integer);
asm
{$ifdef CPUX86_64}
  xchg RSi,RDi
  xchg RCx,RDx

  dec  EDx
  shl  EDx,2

  cld
@loop:
  movsd
  add  RSi,RDx
  loop @loop

{$else}
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx

  mov  EDx,[Zoom]
  dec  EDx
  shl  EDx,2

  cld
@loop:
  movsd
  add  ESi,EDx
  loop @loop

  pop  EDi
  pop  ESi
{$endif}
end;

procedure Zoom_bmp(buf: PBytes; Zoom: Integer;
                   Width0,Width,Height,Bits: Integer);
                   
procedure Zoom_pf1(src,dst: PBytes; Bytes,Zoom: Integer);
asm
{$ifdef CPUX86_64}
  push RBx

  xchg RSi,RDi
  xchg RCx,RDx

  pop  RBx
{$else}
  push EBx
  push ESi
  push EDi

  mov  ESi,EAx
  mov  EDi,EDx

  mov  EDx,[Zoom]

  dec  EDx

  mov  EAx,0
  shr  ECx,3

  cld

  mov  EBx,8
  lodsb

  cmp ECx,0
  jne @loop1
  inc ECx

@loop1:
  push ECx

  xchg ECx,EDx
  mov  EDx,8

@loop2:
  shl  Ax,1
  shl  Al,Cl
  sub  EBx,ECx
  dec  EBx
  jg   @skip

  mov  EBx,8
  lodsb

@skip:

  dec  EDx
  jg   @loop2

  xchg ECx,EDx

  mov  BYTE PTR [EDi],Ah
  inc  EDi

  pop  ECx
  loop @loop1

  pop  EDi
  pop  ESi
  pop  EBx
{$endif}
end;

var
  y,bx,cx,dx: Integer; si,di: PBytes;
begin
  if Width0 = 0 then
  Width0:=Width * Zoom;

  cx:=Width0 * Bits div 8;
  dx:=cx * Zoom;

  si:=buf; di:=buf;

  if Width0 <> Width * Zoom then begin

    bx:=Width * Bits div 8;

    for y:=1 to Height do begin

      if Bits = 1 then
        Zoom_pf1(si,di,Width,Zoom)
      else
      if Bits = 4 then
        Zoom_pf4(si,di,Width,Zoom)
      else
      if Bits = 8 then
        Zoom_pf8(si,di,Width,Zoom)
      else
      if Bits = 24 then
        Zoom_pf24(si,di,Width,Zoom);

      di:=@di[bx]; si:=@si[dx]
    end;
  end
  else begin
    for y:=1 to Height do begin
      Move(si^,di^,cx);
      di:=@di[cx]; si:=@si[dx]
    end;

    si:=buf; di:=buf;
    cx:=Width * Height; if cx > 1 then

    if Bits = 1 then
      Zoom_pf1(si,di,cx,Zoom)
    else
    if Bits = 4 then
      Zoom_pf4(si,di,cx,Zoom)
    else
    if Bits = 8 then
      Zoom_pf8(si,di,cx,Zoom)
    else
    if Bits = 24 then
      Zoom_pf24(si,di,cx,Zoom)
  end
end;

procedure x_zoom_bmp(si,di: PBytes; Zoom: Integer;
                     Width,Height,Bits: Integer);
var
  y,w,h,bx,dx: Integer;
begin
  w:=Width div Zoom;
  h:=Height div Zoom;

  dx:=img_Line(Width,Bits)*Zoom;
  bx:=img_line(w,Bits);

  for y:=1 to h do begin

    if Bits = 1 then
      Zoom_pf1(si,di,w,Zoom)
    else
    if Bits = 4 then
      Zoom_pf4(si,di,w,Zoom)
    else
    if Bits = 8 then
      Zoom_pf8(si,di,w,Zoom)
    else
    if Bits = 24 then
      Zoom_pf24(si,di,w,Zoom);

    di:=@di[bx]; si:=@si[dx]
  end;
end;

procedure img_swap_height(bmp: PBytes; h,line: Integer);
var
  bx,y1,y2: Integer;
  p1,p2,buf: PBytes;
begin
  bx:=line;
  buf:=xAllocPtr(bx);

  y1:=0; y2:=h-1;
  p1:=Bmp; p2:=@Bmp[y2*bx];

  if Assigned(buf) then
  while y1 < y2 do begin
    Move(p1^,buf^,bx);
    Move(p2^,p1^,bx);
    Move(buf^,p2^,bx);

    Inc(y1); Inc(TPointer(p1),bx);
    Dec(y2); Dec(TPointer(p2),bx);
  end;

  xFreePtr(buf)
end;

procedure tile_swap_height(bmp: PBytes; h,line: Integer);
var
  bx,y1,y2: Integer;
  p1,p2,buf: PBytes;
begin
  bx:=line; buf:=@bmp[h*bx];

  y1:=0; y2:=h-1;
  p1:=Bmp; p2:=@Bmp[y2*bx];

  if Assigned(buf) then
  while y1 < y2 do begin
    Move(p1^,buf^,bx);
    Move(p2^,p1^,bx);
    Move(buf^,p2^,bx);

    Inc(y1); Inc(TPointer(p1),bx);
    Dec(y2); Dec(TPointer(p2),bx);
  end;
end;

procedure map_swap_height(const map: Bitmap);
var
  line: Integer;
begin
  with map do
  if Assigned(bmBits) then begin
    line:=img_Line(bmWidth,bmBitsPixel);
    img_swap_height(bmBits,bmHeight,line);
  end
end;

function bmp_swap_height(bmp: HBitmap; out sz: Integer): Pointer;
var
  map: Bitmap; line: Integer;
begin
  Result:=nil; sz:=0;

  if GetObject(bmp,Sizeof(map),@map) > 0 then

  with map do
  if Assigned(bmBits) then begin
    line:=img_Line(bmWidth,bmBitsPixel);
    img_swap_height(bmBits,bmHeight,line);
    sz:=line * bmHeight; Result:=bmBits
  end
end;

procedure bmp_diff(ds: PBytes; Cols,Rows,Bits,Line: Integer);
var
  x,y,x2: Integer; si,si1: PBytes;
begin
  x2:=Cols-2;
  for y:=0 to Rows-1 do begin
    si:=@ds[y*Line];

    if Bits = 8 then begin
      for x:=0 to x2 do begin
        Inc(si[1],si[0]); si:=@si[1]
      end
    end else
    if Bits = 24 then
      for x:=0 to x2 do begin
        si1:=si; si:=@si[3];
        Inc(si[0],si1[0]);
        Inc(si[1],si1[1]);
        Inc(si[2],si1[2]);
      end
  end
end;

function img_Get_Hex(h,pos: Integer; chan: PBytes): longint;
var
  acc,c: Integer;
begin
  acc:=0; if pos > 0 then begin
    FileSeek(h,pos,0); FileRead(h,chan^,16);

    for c:=0 to 15 do begin
      chan[c]:=chan[c] and 15;
      Inc(acc,chan[c])
    end
  end;

  if acc = 0 then
  for c:=0 to 15 do chan[c]:=c;
  Result:=pos
end;

function img_Get_icb(Path: PChar;
                     icb: PImageParams;
                     IsHist: Boolean): Boolean;
var
  buf: TImageParams; fn: TShortstr;
begin
  Result:=false;

  StrUpdateExt(fn,Path,icb_ext);
  if magic_FileRead(fn,'ICB0',buf,Sizeof(buf)) then begin

    if buf.max_cl = 0 then begin
      buf.min_cl:=clBlack;
      buf.max_cl:=clWhite
    end;

    if not IsHist
    or (Hist_sum(buf.hist) > 0) then begin
      if Assigned(icb) then icb^:=buf;
      Result:=true
    end

  end
end;

function img_Get_gam(Path: PChar;
                     out gam: TImageGamma): Boolean;
var
  icb: TImageParams;
begin
  Result:=false;
  Fillchar(gam,Sizeof(gam),0);

  if img_Get_icb(Path,@icb,false) then
  if icb.Flags and icb_gamma <> 0 then begin
    gam:=icb.gamma; Result:=true
  end
end;

procedure app_Gamma(var gam: TImageGamma;
                    var ch0,ch1,ch2,ch3: PBytes);
var
  fn,nm: TShortstr; b0,b1,b2,b3: PBytes;
begin
  Fillchar(gam,Sizeof(gam),0);

  StrPLCopy(nm,ParamStr(0),255);
  StrNameExt(nm,nm);
  StrUpdateExt(nm,nm,icb_ext);

  StrWorkPath(fn,nm);
  if FileExist(fn) then
  if img_Get_gam(fn,gam) then

  if ch0 = nil then
    GetGammaChannels(gam,ch0,ch1,ch2,ch3)
  else begin
    GetGammaChannels(gam,b0,b1,b2,b3);
    Plus_gamma(ch0,ch1,ch2,ch3, b0,b1,b2,b3)
  end
end;

procedure img_Put_icb(Path: PChar; const icb: TImageParams);
var
  buf: TImageParams; fn: TShortstr;
begin
  StrUpdateExt(fn,Path,icb_ext); buf:=icb;
  magic_FileWrite(fn,'ICB0',buf,Sizeof(buf));
end;

procedure img_Put_gam(Path: PChar; const gam: TImageGamma);
var
  buf: TImageParams; fn: TShortstr;
begin
  if not img_Get_icb(Path,@buf,true) then
  Fillchar(buf,Sizeof(buf),0);

  buf.Grays:=256;
  buf.Black:=0;
  buf.White:=clWhite;

  buf.Flags:=icb_gamma;

  buf.gamma:=gam;
  buf.chan:=gam.Gamma[0].chan;

  StrUpdateExt(fn,Path,icb_ext);
  magic_FileWrite(fn,'ICB0',buf,Sizeof(buf));
end;

function Hist_sum(const Hist: TRGB_Hist): Int64;
var
  i: Integer;
begin
  Result:=0;
  for i:=0 to 255 do Inc(Result,hist[i]);
end;

function Hist_okay(const Hist: TRGB_Hist; w,h: Integer): Boolean;
var
  s: Int64;
begin
  s:=w; s:=s * h;
  Result:=Hist_sum(Hist) = s
end;

procedure Init_Hist(out icb: TImageParams);
begin
  FillChar(icb,SizeOf(icb),0);
  icb.Black:=0; icb.White:=255; icb.grays:=64;
  icb.min_cl:=clBlack; icb.max_cl:=clWhite;
  icb.Percent:=3; Init_gray_channel(@icb.chan);
end;

procedure Fill_Hist(var Hist: TRGB_Hist;
                    buf: PBytes; len: Integer);
var
  i: Integer;
begin
  if Assigned(buf) then
  for i:=0 to len-1 do Inc(hist[buf[i]])
end;

procedure Order_Hist(const Hist: TRGB_Hist; Chan: PBytes);
var
  i,j,t, lev1,lev2: Integer;
begin
  Init_Gray_Channel(Chan);
  for i:=0 to 254 do begin
    lev1:=Hist[Chan[i]];

    for j:=i to 255 do begin
      lev2:=Hist[Chan[j]];

      if lev2 > lev1 then begin
        t:=Chan[i]; Chan[i]:=Chan[j];
        Chan[j]:=t; lev1:=lev2
      end
    end
  end
end;

procedure Hex_Swap_Quads(Quads: PRGBQuads; chan: PBytes);
var
  i: Integer;
begin
  for i:=0 to 15 do Quads[i+16]:=Quads[i];
  for i:=0 to 15 do Quads[i]:=Quads[chan[i]+16]
end;

function img_Bits_Planes(Info: PBitMapInfoHeader; Bits,Planes: Integer): Boolean;
begin
  with Info^ do
  Result:=(biBitCount = Bits) and (biPlanes = Planes)
end;

procedure Unpack_planes(dst,src: PBytes; Info: PBitMapInfoHeader);
var
  len, loc, cnt, si_,di_,off, i,j: Integer;
  acc,bit,msk: byte;
begin
  if Info.biPlanes > 1 then begin

    len:=Info.biSize;

    if dst = nil then begin
      dst:=src; src:=@src[len];
      Move(dst^,src^,len)
    end;

    if img_Bits_Planes(Info,1,4) then begin

      cnt:=len+len; loc:=len div 4;

      si_:=0; di_:=0; bit:=$80; acc:=0;
      i:=0; while i < cnt do begin

        acc:=acc shl 4; msk:=1;

        off:=si_; for j:=1 to 4 do begin
          if src[off] and bit <> 0 then
          acc:=acc or msk; Inc(off,loc);
          msk:=msk shl 1
        end;

        bit:=bit shr 1; if bit = 0 then
        begin bit:=$80; Inc(si_) end;

        if Odd(i) then begin
          dst[di_]:=acc; Inc(di_); acc:=0
        end;

        Inc(i)
      end

    end else
    if img_Bits_Planes(Info,8,3) then begin

      loc:=len div 3; si_:=0; di_:=0;
      while si_ < loc do begin

        off:=si_; for i:=1 to 3 do begin
          dst[di_]:=src[off]; Inc(di_);
          Inc(off,loc)
        end;

        Inc(si_)
      end
    end
  end
end;

function DWORD_Ptr(P: Pointer): Pointer;
begin
  Result:=Pointer((TPointer(P)+7) and $FFFFFFF8)
end;

constructor XImage.Create;
begin
  inherited; Set_Gray(256);
  h_res:=96; v_res:=96
end;

destructor XImage.Destroy;
begin
  im_Close; inherited
end;

procedure XImage.im_Close;
begin
  im_w:=0; im_h:=0; is_bgr:=false;

  Fillchar(fHist,Sizeof(fHist),0);
  fIs_Hist:=false; is_bmp:=false;

  is_alfa:=false; fBmp:=nil;

  fBuffer:=xFreePtr(fBuffer);
  fBufferSize:=0;

  fReadonly:=true
end;

function XImage.im_Release: Pointer;
begin
  Result:=nil;
  if Assigned(fBmp) then
  if fBmp = fBuffer then begin
    fBuffer:=nil; Result:=fBmp;
    fBmp:=nil
  end
end;

function XImage.GetMap(out map: Bitmap): Boolean;
begin
  Fillchar(map,Sizeof(map),0);
  map.bmWidth:=im_w; map.bmHeight:=im_h;
  map.bmWidthBytes:=im_line; map.bmPlanes:=1;
  map.bmBitsPixel:=im_bits; map.bmBits:=fBmp;
  Result:=Assigned(fBmp)
end;

function XImage.SetMap(const Map: Bitmap): Boolean;
begin
  im_Close;
  
  if Assigned(Map.bmBits) then begin
    im_w:=Map.bmWidth;
    im_h:=Abs(Map.bmHeight);
    im_bits:=Map.bmBitsPixel;
    im_loc:=im_bits div 8;
    im_line:=Map.bmWidthBytes;
    is_bmp:=Map.bmHeight < 0;
    fBmp:=Map.bmBits
  end;

  Result:=Active
end;

function XImage.SetBitmap(Bmp: HBitmap): Boolean;
var
  map: Bitmap;
begin
  im_Close;
  if GetObject(Bmp,Sizeof(map),@map) > 0 then
  Result:=SetMap(map)
end;

function XImage.im_This(w,h,bits: Integer): Boolean;
begin
  Result:=false; if Active then
  if (w = im_w) and (h = im_h) then
  Result:=bits = im_bits
end;

function XImage.im_Bitmap(Map: PBitmap): Boolean;
begin
  im_Close;

  if Assigned(Map.bmBits) then begin
    im_w:=Map.bmWidth;
    im_h:=Map.bmHeight;
    im_bits:=Map.bmBitsPixel;
    im_loc:=im_bits div 8;
    im_line:=img_Line(im_w,im_bits);
    is_bmp:=true; fBmp:=Map.bmBits
  end;

  Result:=Active
end;

function XImage.im_Buffer(w,h,bits: Integer;
                          Buf: Pointer; tif: Boolean): Boolean;
begin
  im_Close; fBmp:=Buf;

  im_w:=w; im_h:=h; im_bits:=bits;
  im_loc:=bits div 8; is_tif:=tif;

  if is_tif then
    im_line:=(w * bits + 7) div 8
  else
    im_line:=img_Line(w,bits);

  Result:=Assigned(fBmp)
end;

function XImage.im_Buffer1(w,h,bits,line: int; Buf: Pointer): Boolean;
begin
  im_Close; fBmp:=Buf;

  im_w:=w; im_h:=h; im_bits:=bits;
  im_loc:=bits div 8; im_line:=line;
  is_tif:=img_Line(w,bits) = line;

  Result:=Assigned(fBmp)
end;

function XImage.im_Create(w,h,bits: Integer): Boolean;
var
  sz: int;
begin
  im_Close;

  im_w:=w; im_h:=h; im_bits:=bits;
  im_loc:=bits div 8;

  if is_tif then
    im_line:=(w * bits + 7) div 8
  else
    im_line:=img_Line(w,bits);

  sz:=im_line; sz:=sz * h;
  if sz <= $40000000 then

  if is_ijl then begin
    sz:=Max(sz+256,fCapacity);
    fBuffer:=xAllocPtr(sz);
    if Assigned(fBuffer) then begin
      fBmp:=DWORD_Ptr(fBuffer);
      fBufferSize:=sz-8;
      Result:=true
    end
  end
  else begin
    sz:=Max(sz,fCapacity);
    fBuffer:=xAllocPtr(sz);
    if Assigned(fBuffer) then begin
      fBmp:=fBuffer;
      fBufferSize:=sz;
      Result:=true
    end
  end;

  fReadonly:=false;
end;

function XImage.im_Alloc(w,h,bits: Integer): Boolean;
var
  line: Integer; sz: Int64;
begin
  if is_tif then
    line:=(w * bits + 7) div 8
  else
    line:=img_Line(w,bits);

  if not Active then
    im_Create(w,h,bits)
  else begin
    sz:=line; sz:=sz * h;

    if sz > fBufferSize then
      im_Create(w,h,bits)
    else begin
      im_w:=w; im_h:=h; im_bits:=bits;
      im_loc:=bits div 8; im_line:=line;
      if not fOnlyAlloc then
      im_Clear(0);
    end
  end;

  Result:=Active
end;

function XImage.im_Realloc(w,h,bits: Integer): Boolean;
begin
  Result:=true;

  if not Active
  or (bits <> im_bits)
  or (w <> im_w)
  or (h <> im_h) then

  Result:=im_Alloc(w,h,bits)
end;

function XImage.GetRect(Dest: XImage; const Rect: TRect): Boolean;
var
  x,y,w,h,iy,bx,cx: int; si,di: PBytes;
begin
  Result:=false;

  if im_loc > 0 then
  if Active then begin
    x:=Rect.Left; w:=Rect.Right-x+1;
    y:=Rect.Top;  h:=Rect.Bottom-y+1;

    if x+w > im_w then w:=im_w-x;
    if y+h > im_h then h:=im_h-y;

    if (w > 0) and (h > 0) then begin
      Dest.is_bmp:=is_bmp;
      Dest.is_tif:=is_tif;

      if Dest.im_Realloc(w,h,im_bits) then begin

        bx:=tif_line(x,im_bits);
        cx:=tif_line(w,im_bits);

        for iy:=0 to h-1 do begin
          si:=Lines[y+iy];
          di:=Dest.Lines[iy];
          Move(si[bx],di^,cx)
        end;

        Result:=true
      end
    end
  end
end;

function XImage.LoadBitmap(bmp: HBitmap): Boolean;
var
  map: Bitmap;
  y,w,h,bits,line: int;
  si,di: PBytes;
begin
  Result:=false;

  if GetObject(bmp,Sizeof(map),@map) > 0 then
  if Assigned(map.bmBits) then begin

    w:=map.bmWidth;
    h:=map.bmHeight;
    bits:=map.bmBitsPixel;

    if im_Alloc(w,h,bits) then begin

      line:=img_Line(w,bits);
      si:=map.bmBits;
      si:=@si[(h-1)*line];

      di:=fBmp;
      for y:=1 to h do begin
        Move(si^,di^,im_line);
        Dec(TPointer(si),line);
        Inc(TPointer(di),im_line);
      end;

      is_bmp:=false; Result:=true
    end
  end
end;

function XImage.LoadFrame(bmp: HBitmap): Boolean;
var
  dx,dy: int;
  y,w,h,line,bits,loc,maxw,maxh: int;
  si,di: PBytes; map: Bitmap;
begin
  Result:=false;

  if GetObject(bmp,Sizeof(map),@map) > 0 then
  if Assigned(map.bmBits) then begin

    bits:=map.bmBitsPixel;
    loc:=bits div 8;

    if loc in [1,3] then begin

      maxw:=640; maxh:=480;

      w:=map.bmWidth; dx:=0;
      if w > maxw then begin
        dx:=(w-maxw) div 2; w:=maxw;
      end;

      h:=map.bmHeight; dy:=0;
      if h > maxh then begin
        dy:=(h-maxh) div 2; h:=maxh;
      end;

      if im_Alloc(w,h,bits) then begin

        with map do
        line:=img_Line(bmWidth,bmBitsPixel);

        si:=map.bmBits;
        si:=@si[dy*line+dx*loc];

        di:=fBmp;
        for y:=1 to h do begin
          Move(si^,di^,im_line);
          if loc = 3 then swap_rgb(di,w);

          Inc(TPointer(si),line);
          Inc(TPointer(di),im_line);
        end;

        is_bmp:=false; Result:=true
      end
    end
  end
end;

function XImage.loadRGB(const map: Bitmap): Boolean;
var
  src,si,di: PBytes; x,y,w,h,line: int;
begin
  Result:=false;
  if Assigned(map.bmBits) then
  if map.bmBitsPixel = 32 then begin

    src:=map.bmBits;

    w:=map.bmWidth;
    h:=map.bmHeight;
    is_tif:=false;

    if im_Alloc(w,h,24) then begin

      line:=w*4;

      is_bmp:=false;
      for y:=0 to h-1 do begin
        si:=@src[y*line];
        di:=Lines[y];

        for x:=1 to w do begin
          di[0]:=si[0];
          di[1]:=si[1];
          di[2]:=si[2];
          si:=@si[4];
          di:=@di[3];
        end
      end;

      Result:=true
    end
  end
end;

function XImage.onlyRGB(bmp: HBitmap): Boolean;
var
  map: Bitmap;
begin
  Result:=false;
  if GetObject(bmp,Sizeof(map),@map) > 0 then
  if loadRGB(map) then begin
    is_bmp:=true; Result:=true
  end
end;

function XImage.onlyAlf(bmp: HBitmap): Boolean;
var
  si,di: PBytes;
  x,y,w,h,line: int; map: Bitmap;
begin
  Result:=false;
  if GetObject(bmp,Sizeof(map),@map) > 0 then
  if Assigned(map.bmBits) then
  if map.bmBitsPixel = 32 then begin

    w:=map.bmWidth;
    h:=map.bmHeight;
    is_tif:=false;

    if im_Alloc(w,h,8) then begin

      line:=w*4;

      is_bmp:=false;
      for y:=0 to h-1 do begin
        si:=@PBytes(map.bmBits)[y*line];
        di:=Lines[y];

        for x:=1 to w do begin
          di[0]:=si[3];
          si:=@si[4];
          di:=@di[1];
        end
      end;

      is_bmp:=true; Result:=true
    end
  end
end;

function XImage.addRGB(bmp: HBitmap): Boolean;
var
  si,di: PBytes;
  x,y,y1,w,h,line: int;
  map: Bitmap;
begin
  Result:=false;

  w:=im_w; h:=im_h;

  if Active then
  if im_bits = 32 then

  if GetObject(bmp,Sizeof(map),@map) > 0 then
  if Assigned(map.bmBits) then
  if map.bmBitsPixel = 24 then

  if map.bmWidth = im_w then
  if map.bmHeight = im_h then begin

    line:=img_Line(im_w,24);

    y1:=h-1;
    for y:=0 to h-1 do begin

      si:=@PBytes(map.bmBits)[y1*line];
      di:=Lines[y];

      for x:=1 to w do begin
        di[0]:=si[0];
        di[1]:=si[1];
        di[2]:=si[2];
        di:=@di[4]; si:=@si[3];
      end;

      Dec(y1)
    end;

    Result:=true
  end
end;

function XImage.addAlf(bmp: HBitmap): Boolean;
var
  si,di: PBytes;
  x,y,y1,w,h,line: int;
  map: Bitmap;
begin
  Result:=false;

  w:=im_w; h:=im_h;
  
  if Active then
  if im_bits = 32 then

  if GetObject(bmp,Sizeof(map),@map) > 0 then
  if Assigned(map.bmBits) then
  if map.bmBitsPixel = 24 then

  if map.bmWidth = im_w then
  if map.bmHeight = im_h then begin

    line:=img_Line(im_w,24);

    y1:=h-1;
    for y:=0 to h-1 do begin

      si:=@PBytes(map.bmBits)[y1*line];
      di:=Lines[y];

      for x:=1 to w do begin
        di[3]:=si[0];
        di:=@di[4]; si:=@si[3];
      end;

      Dec(y1)
    end;

    Result:=true
  end
end;

function XImage.LoadMap(const map: Bitmap): Boolean;
var
  w,h,bits: int;
begin
  Result:=false;

  if Assigned(map.bmBits) then begin
    w:=map.bmWidth; h:=map.bmHeight;
    bits:=map.bmBitsPixel;

    with map do
    if bmWidthBytes = 0 then
      is_tif:=false
    else
      is_tif:=tif_Line(w,bits) = bmWidthBytes;

    if im_Alloc(w,Abs(h),bits) then begin
      Move(map.bmBits^,fBmp^,im_Size);
      is_bmp:=h < 0; Result:=true
    end

  end
end;

function XImage.LoadMap1(const map: Bitmap): Boolean;
var
  y,w,h,bits,bx,bx1: int; si,di: PByte;
begin
  w:=map.bmWidth;
  h:=Abs(map.bmHeight);
  bits:=map.bmBitsPixel;

  if im_Realloc(w,h,bits) then begin

    bx:=map.bmWidthBytes;
    if bx = 0 then bx:=img_Line(w,bits);

    bx1:=im_line;
    if bx = bx1 then
      Move(map.bmBits^,fBmp^,im_Size)
    else
    for y:=0 to h-1 do begin
      si:=@PBytes(map.bmBits)[y*bx];
      di:=@fBmp[y*bx1]; Move(si^,di^,bx)
    end
  end;

  is_bmp:=map.bmHeight < 0;

  Result:=Active
end;

function XImage.LoadMap2(const map: Bitmap; bmp: bool): Boolean;
var
  y,y1,w,h,bits,bx: int; si,di: PBytes; swap: bool;
begin
  w:=map.bmWidth;
  h:=Abs(map.bmHeight);
  bits:=map.bmBitsPixel;

  if im_Realloc(w,h,bits) then begin

    bx:=map.bmWidthBytes;
    if bx = 0 then bx:=img_Line(w,bits);

    is_bmp:=bmp;
    swap:=is_bmp <> (map.bmHeight < 0);

    Dec(h);
    for y:=0 to h do begin
      si:=@PBytes(map.bmBits)[y*bx];

      y1:=y; if swap then y1:=h-y;
      di:=Lines[y1]; Move(si^,di^,bx)
    end
  end;

  Result:=Active
end;

function XImage.LoadRGB2(const map: Bitmap; bmp: bool): Boolean;
var
  x,y,y1,w,h,bits,bx: int;
  cl: tlong; si,di: PBytes; swap: bool;
begin
  Result:=false;

  if map.bmBitsPixel = 24 then
    Result:=LoadMap2(map,bmp)
  else
  if map.bmBitsPixel = 8 then begin

    w:=map.bmWidth;
    h:=Abs(map.bmHeight);
    bits:=map.bmBitsPixel;

    if im_Realloc(w,h,24) then begin

      bx:=map.bmWidthBytes;
      if bx = 0 then bx:=img_Line(w,bits);

      is_bmp:=bmp;
      swap:=is_bmp <> (map.bmHeight < 0);

      Dec(h);
      for y:=0 to h do begin
        si:=@PBytes(map.bmBits)[y*bx];

        y1:=y; if swap then y1:=h-y;
        di:=Lines[y1];

        for x:=1 to w do begin
          cl.i:=fColors[si[0]];
          di[0]:=cl.b[0];
          di[1]:=cl.b[1];
          di[2]:=cl.b[2];
          di:=@di[3]; si:=@si[1]
        end
      end;

      Result:=true
    end
  end
end;

function XImage.Clone(im: XImage): Boolean;
begin
  Result:=false;

  if im.Active then begin

    fOnlyAlloc:=true;

    is_tif:=im.is_tif;
    if im_Alloc(im.im_w,
                im.im_h,
                im.im_bits) then begin

      is_bmp:=im.is_bmp;
      is_bgr:=im.is_bgr;

      fColors:=im.im_Colors;
      im_used:=im.im_used;

      fHist:=im.im_Hist;
      fIs_Hist:=im.Is_Hist;

      Result:=true
    end;

    fOnlyAlloc:=false
  end;

  if not Result then im_Close;
end;

function XImage.LoadFrom(im: XImage): Boolean;
begin
  Result:=Clone(im);

  if Result then
    Move(im.bmpBits^,fBmp^,im_size)
  else
    im_Close
end;

function XImage.LoadAsBmp(im: XImage): Boolean;
var
  w,h,bits,bx,y: int; si,di: PBytes;
begin
  is_tif:=false;

  w:=im.im_w; h:=im.im_h;
  bits:=im.im_bits;
  bx:=tif_Line(w,bits);

  if not im.Active then
    im_Close
  else
  if im_Alloc(w,h,im.im_bits) then begin
    is_bmp:=true;
    for y:=0 to h-1 do begin
      si:=im.Lines[y]; di:=Lines[y];
      Move(si^,di^,bx)
    end
  end;

  Result:=Active
end;

function XImage.LoadAsTiff(im: XImage): Boolean;
var
  y,line: Integer; si,di: PBytes;
begin
  is_tif:=true; is_bmp:=false;

  if not im.Active then
    im_Close
  else
  if im_Alloc(im.im_w,im.im_h,im.im_bits) then begin

    fColors:=im.im_Colors;
    im_used:=im.im_used;

    fHist:=im.im_Hist;
    fIs_Hist:=im.Is_Hist;

    di:=fBmp;
    for y:=0 to im_h-1 do begin
      si:=im.Lines[y];
      Move(si^,di^,im_line);
      di:=@di[im_line]
    end
  end;

  Result:=Active
end;

function XImage.LoadPlusAlfa(im: XImage): Boolean;
var
  x,y,w,h,bits: int; si,di: PBytes;
begin
  Result:=false;

  w:=im.im_w; h:=im.im_h; bits:=im.im_bits;

  if bits in [8,24] then
  if im_Realloc(w,h,bits+8) then begin

    for y:=0 to h-1 do begin

      si:=im.Lines[y]; di:=Lines[y];

      if bits = 8 then
        for x:=0 to w-1 do begin
          di[0]:=si[0]; di[1]:=255;
          si:=@si[1]; di:=@di[2]
        end
      else
        for x:=0 to w-1 do begin
          di[0]:=si[2]; di[1]:=si[1]; di[2]:=si[0];
          di[3]:=255; si:=@si[3]; di:=@di[4]
        end
    end;

    Result:=true
  end
end;

procedure XImage.CopyTo(bmp: TBitmap);

function CopyToDib(bmp: TBitmap): Boolean;
var
  map: Bitmap; si,di: PBytes;
  si_line,di_line,len,y: Integer;
begin
  Result:=false;

  if GetObject(bmp.Handle,Sizeof(map),@map) > 0 then
  if Assigned(map.bmBits) then
  if map.bmPlanes = 1 then
  if map.bmBitsPixel = im_bits then

  if map.bmWidth = im_w then
  if map.bmHeight = im_h then begin

    di:=map.bmBits; di_line:=map.bmWidthBytes;
    si_line:=im_line; len:=Min(si_line,di_line);

    if is_bmp then begin

      si:=fBmp;

      for y:=1 to im_h do begin
        Move(si^,di^,len);
        si:=@si[si_line];
        di:=@di[di_line];
      end

    end
    else begin

      si:=@fBmp[si_line*(im_h-1)];

      for y:=1 to im_h do begin
        Move(si^,di^,len);
        Dec(TPointer(si),si_line);
        di:=@di[di_line];
      end
    end;

    Result:=true
  end
end;

var
  info: TVGABITMAPINFO;
begin
  if Assigned(fBmp) then
  if not CopyToDib(bmp) then begin

    Get_BitMapInfo(@info);

    StretchDIBits(bmp.Canvas.Handle,
                  0,0,bmp.Width,bmp.Height,

                  0,0,im_w,im_h,fBmp,
                  PBitMapinfo(@info)^,
                  DIB_RGB_COLORS,SRCCOPY);
  end
end;

procedure XImage.im_Clear(fc: Integer);
begin
  if Active then
  FillChar(fBmp^,im_Size,fc);
end;

procedure XImage.im_Fill(fc: Integer);
var
  line: PBytes; x,y,di: Integer;
begin
  if Active then begin

    if im_used > 0 then
    if im_bits <= 8 then begin
      fc:=rgb_Indexof(@im_Colors,im_used,fc);
      if fc <> 0 then
      if im_bits = 1 then fc:=$FF else
      if im_bits = 4 then fc:=fc or (fc shl 4)
    end;

    FillChar(fBmp^,im_Size,fc and 255);

    if fc <> 0 then
    if im_bits = 32 then
      SetInts(Pointer(fBmp),im_w*im_h,fc)
    else
    if im_bits = 24 then begin
      line:=Lines[0]; di:=0;

      for x:=1 to im_w do begin
        Move(fc,line[di],3); Inc(di,3)
      end;

      for y:=1 to im_h-1 do
      Move(Lines[0]^,Lines[y]^,im_line)
    end
  end
end;

function XImage.Get_BitMapInfo(Info: PBitMapInfo): Pointer;
var
  used: Integer;
begin
  with Info^ do begin
    FillChar(bmiHeader,SizeOf(bmiHeader),0);

    bmiHeader.biSize:=SizeOf(bmiHeader);
    bmiHeader.biWidth:=im_w;
    bmiHeader.biHeight:=im_h;
    bmiHeader.biPlanes:=1;
    bmiHeader.biBitCount:=im_bits;
    bmiHeader.biCompression:=bi_RGB;

    bmiHeader.biSizeImage:=im_line * im_h;

    if im_bits in [1,4,8] then begin
      Colors_to_Quads(@fColors,@bmiColors,256);
      used:=im_used; if used = 0 then
      if im_bits = 1 then used:=2 else
      if im_bits = 1 then used:=16 else
                          used:=256;
      bmiHeader.biClrUsed:=used;
    end
  end; Result:=Info
end;

function XImage.This_Colors(AColors: PColors): bool;
var
  i: int;
begin
  Result:=true;
  for i:=0 to 255 do
  if fColors[i] <> AColors[i] then begin
    Result:=false; Break
  end
end;

procedure XImage.Set_Colors(AColors: PColors);
begin
  fColors:=AColors^; im_used:=256
end;

procedure XImage.Set_Quads(Quads: PRGBQuads);
begin
  if Assigned(Quads) then begin
    Quads_to_Colors(Quads,@fColors,0);
    im_used:=256
  end
end;

procedure XImage.Swap_palette;
var
  i,al: int;
begin
  for i:=0 to im_used-1 do
  with tlong(fColors[i]) do begin
    al:=b[0]; b[0]:=b[2]; b[2]:=al
  end
end;

procedure XImage.Set_palette(pal: HPalette);
var
  used: int; quads: TRGBQuads;
begin
  Fillchar(quads,Sizeof(quads),0);
  used:=0; if pal <> 0 then
  used:=xGetPalette(pal,@quads);

  if used = 0 then
    Set_Gray(256)
  else begin
    Quads_to_Colors(@quads,@fColors,used);
    im_used:=256
  end
end;

procedure XImage.Set_Gray(AMax: Integer);
begin
  im_used:=xGray_Quads(@fColors,AMax);
  Quads_to_Colors(@fColors,@fColors,0);
end;

function XImage.im_Fill_Hist: Boolean;
var
  y: Integer; capt: TShortstr;
begin
  Result:=Hist_okay(fHist,im_w,im_h);

  if not Result then
  if im_bits = 8 then begin

    StrCopy(capt,'Histogram');
    if rus_interface then
    StrCopy(capt,'Ãטסעמדנאללא');

    Progress(im_h,capt,nil);

    Fillchar(fHist,Sizeof(fHist),0);

    for y:=0 to im_h-1 do begin
      Fill_hist(fHist,Lines[y],im_w);
      StepIt
    end;

    Progress(0,nil,nil);

    Result:=Hist_okay(fHist,im_w,im_h);
  end;

  fIs_Hist:=Result
end;

function XImage.bmp_load(bmp: TBitmap): PByte;
var
  bits: Integer;
begin
  Result:=nil;

  bits:=Bitmap_Depth(bmp);
  if bits in [8,16,24,32] then begin

    if bits > 8 then bits:=24;

    if (im_w <> bmp.Width)
    or (im_h <> bmp.Height)
    or (im_bits <> bits) then
    im_Create(bmp.Width,bmp.Height,bits);

    if bmp_xload(bmp) then Result:=@fBmp[0]
  end
end;

function XImage.gif_load(bmp: TBitmap): PByte;
begin
  Result:=nil;

  if (im_w <> bmp.Width)
  or (im_h <> bmp.Height)
  or (im_bits <> 24) then
  im_Create(bmp.Width,bmp.Height,24);

  if bmp_xload(bmp) then Result:=@fBmp[0]
end;

function XImage.bmp_xload(bmp: TBitmap): Boolean;
var
  info: TBitmapInfo;
begin
  Result:=false;

  if Active then
  if im_w = bmp.Width then
  if im_h = bmp.Height then begin

    FillChar(info,SizeOf(info),0);
    info.bmiHeader.biSize:=SizeOf(info.bmiHeader);
    info.bmiHeader.biWidth:=im_w;
    info.bmiHeader.biHeight:=-im_h;
    info.bmiHeader.biPlanes:=1;
    info.bmiHeader.biBitCount:=im_bits;
    info.bmiHeader.biCompression:=bi_RGB;
    info.bmiHeader.biSizeImage:=im_Size;

    if GetDIBits(Bmp.Canvas.Handle,Bmp.Handle,0,im_h,
                 fBmp,info,DIB_RGB_COLORS) = im_h then

    Result:=true
  end
end;

procedure XImage.im_load(im: XImage);
begin
  if Active and im.Active then
  if im_Size = im.im_Size then
  Move(im.Lines[0]^,Lines[0]^,im_Size)
end;

procedure XImage.im_add(im: XImage);
var
  x,y: Integer; buf1,buf2: PBytes;
begin
  for y:=0 to im_h-1 do begin
    buf1:=Lines[y];
    buf2:=im.Lines[y];

    for x:=0 to im_line-1 do
    buf1[x]:=buf1[x] or buf2[x]
  end
end;

procedure XImage.im_not(im: XImage);
var
  x,y: Integer; buf1,buf2: PBytes;
begin
  for y:=0 to im_h-1 do begin
    buf1:=Lines[y];
    buf2:=im.Lines[y];

    for x:=0 to im_line-1 do
    buf1[x]:=buf2[x] xor $FF
  end
end;

procedure XImage.im_swap(im: XImage);
var
  x,y: Integer; bp1,bp2: PBytes;
begin
  for y:=0 to im_h-1 do begin
    bp1:=Lines[y];
    bp2:=im.Lines[y];

    for x:=0 to im_line-1 do begin
      bSwap(bp1[0],bp2[0]);
      bp1:=@bp1[1]; bp2:=@bp2[1];
    end
  end
end;

procedure XImage.im_copy(im: XImage);
var
  si,di: PBytes; y,bx: Integer;
begin
  if Active then
  if im.Active then

  if im_w = im.im_w then
  if im_h = im.im_h then
  if im_bits = im.im_bits then begin

    bx:=Min(im_line,im.im_line);
    for y:=0 to im_h-1 do begin
      si:=im.Lines[y]; di:=Lines[y];
      Move(si^,di^,bx)
    end
  end
end;

function XImage.im_Zoom(im: XImage; iz: Integer): Boolean;
var
  y,w,h: Integer; si,di: PBytes; old_bmp: Boolean;
begin
  old_bmp:=is_bmp;

  w:=im.im_w div iz;
  h:=im.im_h div iz;

  if im_Alloc(w,h,im.im_bits) then begin

    fColors:=im.fColors;
    im_used:=im.im_used;
    is_bmp:=old_bmp;

    for y:=0 to h-1 do begin
      si:=im.Lines[y*iz]; di:=Lines[y];

      if im_bits = 4 then
        Zoom_pf4(si,di,w,iz)
      else
      if im_bits = 8 then
        Zoom_pf8(si,di,w,iz)
      else
      if im_bits = 24 then
        Zoom_pf24(si,di,w,iz)
    end;
  end;

  Result:=Active
end;

function XImage.Get_Pixel(x,y: Integer): Integer;
var
  si: PBytes; ind,cl: Integer; bit: byte;
begin
  Result:=0;

  if (x >= 0) and (x < im_w) then
  if (y >= 0) and (y < im_h) then begin

    si:=Lines[y]; if Assigned(si) then

    if im_bits = 1 then begin
      ind:=x div 8; bit:=B_Bit[x and 7];
      if (si[ind] and bit) <> 0 then Result:=1
    end else
    if im_bits = 4 then begin
      cl:=si[x div 2]; if Odd(x) then
      cl:=cl and 15 else cl:=cl shr 4;
      Result:=cl
    end
    else begin cl:=0;
      Move(si[x*im_loc],cl,im_loc);
      Result:=cl
    end
  end
end;

function XImage.Set_Pixel(x,y,val: Integer): Boolean;
var
  bp: PBytes; bx,bit: int;
begin
  Result:=false;

  if (x >= 0) and (x < im_w) then
  if (y >= 0) and (y < im_h) then begin
    bp:=Lines[y];
    if Assigned(bp) then

    case im_bits of
  1:  begin
        bp:=@bp[x div 8];
        bit:=B_Bit[x and 7];

        if val = 1 then bp[0]:=bp[0] or bit
        else bp[0]:=bp[0] and (bit xor $FF)
      end;

  4:  begin
        bp:=@bp[x div 2];

        if Odd(x) then
          bp[0]:=(bp[0] and $F0) or val
        else
          bp[0]:=(bp[0] and $0F) or (val shl 4);
      end;

  8:  bp[x]:=val;

  24: begin
        bp[0]:=tlong(val).b[0];
        bp[1]:=tlong(val).b[1];
        bp[2]:=tlong(val).b[2];
      end;

  32: PInteger(@bp[x*4])^:=val;
    end;

    Result:=true
  end
end;

procedure XImage.Set_Pixels(x1,x2,y,val: int);
const
  H_Bit: array[0..1] of int = (2,1);
var
  x,bit,val1: int; bp: PBytes;
begin
  if x1 < 0 then x1:=0;
  if x2 >= im_w then x2:=im_w-1;

  if x1 <= x2 then
  if (y >= 0) and (y < im_h) then begin

    bp:=Lines[y];
    if Assigned(bp) then

    case im_bits of
  1:  begin
        bp:=@bp[x1 div 8];
        bit:=B_Bit[x1 and 7];

        for x:=x1 to x2 do begin
          if val = 1 then bp[0]:=bp[0] or bit
          else bp[0]:=bp[0] and (bit xor $FF);

          bit:=bit shr 1;
          if bit = 0 then begin
            bit:=$80; bp:=@bp[1]
          end
        end
      end;

  4:  begin
        bp:=@bp[x1 div 2];
        bit:=H_Bit[x1 and 1];

        val1:=val shl 4;

        for x:=x1 to x2 do begin
          if Odd(bit) then
            bp[0]:=(bp[0] and $F0) or val
          else
            bp[0]:=(bp[0] and $0F) or val1;

          bit:=bit shr 1;
          if bit = 0 then begin
            bit:=2; bp:=@bp[1]
          end
        end
      end;

  8:  for x:=x1 to x2 do bp[x]:=val;

  else
      for x:=x1 to x2 do Set_Pixel(x,y,val)
    end
  end
end;

function XImage.off_Pixels(map: PBytes; line,val: int): int;
var
  bp,mp: PBytes; ip:PIntegers;
  i,bit,x,y,w1,h1,val1,val2,val3: int;
begin
  Result:=0; h1:=im_h-1;

  case im_bits of
1:  begin
      w1:=(im_w div 8)-1;

      for y:=0 to h1 do begin

        bp:=Lines[y];
        mp:=@map[y*line];

        for x:=0 to w1 do begin
          bit:=$80;
          for i:=1 to 8 do begin

            if mp[0] <> 0 then begin
              if val = 1 then bp[0]:=bp[0] or bit
              else bp[0]:=bp[0] and (bit xor $FF);
              Inc(Result);
            end;

            mp:=@mp[1]; bit:=bit shr 1
          end;

          bp:=@bp[1]
        end
      end
    end;

2:  begin
      w1:=(im_w div 4)-1;
      val1:=val shl 6;
      val2:=val shl 4;
      val3:=val shl 2;

      for y:=0 to h1 do begin

        bp:=Lines[y];
        mp:=@map[y*line];

        for x:=0 to w1 do begin

          if mp[0] <> 0 then begin
            bp[0]:=(bp[0] and $3F) or val1;
            Inc(Result)
          end; mp:=@mp[1];

          if mp[0] <> 0 then begin
            bp[0]:=(bp[0] and $CF) or val2;
            Inc(Result)
          end; mp:=@mp[1];

          if mp[0] <> 0 then begin
            bp[0]:=(bp[0] and $F3) or val3;
            Inc(Result)
          end; mp:=@mp[1];

          if mp[0] <> 0 then begin
            bp[0]:=(bp[0] and $FC) or val;
            Inc(Result)
          end; mp:=@mp[1];

          bp:=@bp[1]
        end
      end
    end;

4:  begin
      w1:=(im_w div 2)-1;
      val1:=val shl 4;

      for y:=0 to h1 do begin

        bp:=Lines[y];
        mp:=@map[y*line];

        for x:=0 to w1 do begin
          if mp[0] <> 0 then begin
            bp[0]:=(bp[0] and $0F) or val1;
            Inc(Result)
          end;

          mp:=@mp[1];

          if mp[0] <> 0 then begin
            bp[0]:=(bp[0] and $F0) or val;
            Inc(Result)
          end;

          mp:=@mp[1]; bp:=@bp[1]
        end
      end
    end;

8,
24,
32: begin
      w1:=im_w-1;

      for y:=0 to h1 do begin
        bp:=Lines[y];
        mp:=@map[y*line];

        if im_bits = 8 then
          for x:=0 to w1 do begin
            if mp[0] <> 0 then begin
              bp[0]:=val; Inc(Result)
            end;

            bp:=@bp[1]; mp:=@mp[1]
          end
        else
        if im_bits = 24 then
          for x:=0 to w1 do begin
            if mp[0] <> 0 then begin
              bp[0]:=tlong(val).b[0];
              bp[1]:=tlong(val).b[1];
              bp[2]:=tlong(val).b[2];
              Inc(Result)
            end;

            bp:=@bp[3]; mp:=@mp[1]
          end
        else
        if im_bits = 32 then begin
          ip:=Pointer(bp);
          for x:=0 to w1 do begin
            if mp[0] <> 0 then begin
              ip[0]:=val; Inc(Result)
            end;

            ip:=@ip[1]; mp:=@mp[1]
          end
        end
      end
    end
  end
end;

procedure XImage.Set_Line(y: Integer; buf: PBytes);
var
  dst: PBytes;
begin
  if y >= 0 then
  if y < im_h then begin
    if is_bmp then dst:=Lines[im_h-1-y]
    else dst:=Lines[y];

    Move(Buf^,dst^,im_line)
  end
end;

procedure XImage.Set_Color(I,Value: Integer);
begin
  fColors[I]:=Value
end;

procedure XImage.Get_Tile(x,y,w,h: Integer; tile: PBytes);
begin
  Get_Tile1(x,y,w,h,0,tile)
end;

procedure XImage.Set_Tile(x,y,w,h: Integer; tile: PBytes);
begin
  Set_tile1(x,y,w,h,0,tile)
end;

procedure XImage.Get_Tile1(x,y,w,h,line: Integer; tile: PBytes);
var
  i,dw,dh,len,loc,line1: Integer;
  _si: Integer; si,di: PBytes;
begin
  if im_bits > 0 then
  if x < im_w then begin

    dw:=w; dh:=h;
    if x+dw > im_w then dw:=im_w-x;
    if y+dh > im_h then dh:=im_h-y;

    if im_bits >= 8 then begin
      loc:=im_bits div 8; line1:=loc * w;
      _si:=x * loc; len:=dw * loc;
      if is_alfa then Inc(_si,_si)
    end
    else begin
      loc:=8 div im_bits; line1:=w div loc;
      _si:=x div loc; len:=dw div loc;
    end;

    if (line = 0)
    or (line1 > line) then line:=line1; 

    di:=tile;
    Fillchar(di^,line*h,0);

    for i:=1 to dh do
    if y < im_h then begin

      si:=@Lines[y][_si];

      if is_alfa then begin
        for x:=1 to len do begin
          di[0]:=si[0]; di:=@di[1]; si:=@si[2]
        end; di:=@di[line-len]
      end
      else begin
        Move(si^,di^,len);
        di:=@di[line]
      end;

      Inc(y)
    end;
  end
end;

function XImage.Get_Rect(tile: XImage; x,y,w,h: int): bool;
begin
  Result:=false;

  if Active then
  if tile.im_Realloc(w,h,im_bits) then begin
    Get_Tile1(x,y,w,h,tile.im_line,tile.bmpBits);
    Result:=true
  end
end;

procedure XImage.cls_Tile(x,y,w,h: int);
var
  i, len,loc, _di, dw: int; di: PBytes;
begin
  if Active then
  if x < im_w then begin

    loc:=im_bits div 8;

    dw:=w;
    if x < 0 then begin
      x:=-x; Dec(dw,x); x:=0
    end;

    if x+dw > im_w then dw:=im_w-x;

    if im_bits >= 8 then begin
      _di:=x * loc; len:=dw * loc;
    end
    else begin
      loc:=8 div im_bits; _di:=x div loc;
      len:=tif_Line(dw,im_bits);
    end;

    if y < 0 then begin
      y:=-y; Dec(h,y); y:=0
    end;

    if y+h > im_h then h:=im_h-y;

    if len > 0 then
    for i:=1 to h do begin
      di:=Lines[y];
      Fillchar(di[_di],len,0);
      Inc(y)
    end
  end
end;

procedure XImage.Set_Tile1(x,y,w,h,line: Integer; tile: PBytes);
var
  i, len,loc, _di, dw: Integer;
  si,di: PBytes; hist_w: Integer;
begin
  if Active then
  if x < im_w then begin

    loc:=im_bits div 8;

    si:=tile; dw:=w;
    if x < 0 then begin

      x:=-x; if loc > 0 then
        si:=@si[x*loc]
      else
        si:=@si[x*im_bits div 8];

      Dec(dw,x); x:=0
    end;

    if x+dw > im_w then dw:=im_w-x;

    if im_bits >= 8 then begin
      if line = 0 then line:=loc * w;
      _di:=x * loc; len:=dw * loc;
    end
    else begin
      loc:=8 div im_bits; _di:=x div loc;
      if line = 0 then line:=tif_Line(w,im_bits);
      len:=tif_Line(dw,im_bits);
    end;

    hist_w:=0;
    if im_bits = 8 then hist_w:=dw;

    if y < 0 then begin
      y:=-y; si:=@si[y*line];
      Dec(h,y); y:=0
    end;

    if y+h > im_h then h:=im_h-y;

    if len > 0 then
    for i:=1 to h do begin
      di:=Lines[y];

      if hist_w > 0 then
      Fill_hist(fHist,si,hist_w);
      Move(si^,di[_di],len);

      si:=@si[line]; Inc(y);
    end
  end
end;

procedure XImage.Set_Tile2(x,y: Integer; tile: XImage);
begin
  if tile.Active then
  Set_Tile1(x,y,tile.im_w,tile.im_h,
            tile.im_line,tile.bmpBits)
end;

procedure XImage.Set_page(ip,jp: Integer; tile: XImage);
var
  w,h: Integer;
begin
  if tile.Active then begin
    w:=tile.im_w; h:=tile.im_h;
    Set_Tile1(ip*w,jp*h,w,h,
              tile.im_line,
              tile.bmpBits)
  end
end;

procedure XImage.Put_Tile(x,y,w,h: Integer; tile: PBytes);
var
  si,di: PBytes;
  i,bx,line,loc,len: Integer;
begin
  if Active then begin
    si:=tile;

    loc:=im_bits div 8;
    line:=loc * w;

    if x < 0 then begin
      x:=-x; si:=@si[loc*x];
      Dec(w,x); x:=0
    end;

    if y < 0 then begin
      y:=-y; si:=@si[line*y];
      Dec(h,y); y:=0
    end;

    if x < im_w then
    if y < im_h then begin

      if x+w > im_w then w:=im_w-x;
      if y+h > im_h then h:=im_h-y;

      bx:=x*loc; len:=w*loc;

      if len > 0 then
      for i:=1 to h do begin
        di:=Lines[y];
        Move(si^,di[bx],len);
        si:=@si[line]; Inc(y);
      end
    end
  end
end;

function XImage.Get_Dib(Info: PBitMapInfoHeader; Zoom: Integer;
                        var Rect: XRect; out lpBits: PBytes): Boolean;
var
  i,bx,cx,dx: int; si,di: PBytes; r: XRect;
begin
  Result:=false; lpBits:=nil; r:=Rect;

  if r.x < 0 then r.x:=0;
  if r.x+r.w > im_w then r.w:=im_w-r.x;

  if r.y < 0 then r.y:=0;
  if r.y+r.h > im_h then r.h:=im_h-r.y;

  if (r.w > 0) and (r.h > 0) then

  if Is_bmp and (Zoom = 1) then begin
    Rect:=r; Result:=true
  end
  else begin
    bx:=img_Line_ofs(r.x,im_bits);
    cx:=r.x; r.x:=bx * 8 div im_bits;
    r.w:=Rect.w+(cx-r.x);

    if r.x+r.w > im_w then r.w:=im_w-r.x;

    r.x:=r.x div Zoom; r.w:=r.w div Zoom;
    r.y:=r.y div Zoom; r.h:=r.h div Zoom;

    if (r.w > 0) and (r.h > 0) then begin

      dx:=img_Line(r.w,im_Bits);
      di:=xAllocPtr(r.h*dx);

      if Assigned(di) then begin

        cx:=(r.w*im_bits+7) div 8;

        lpBits:=di;
        di:=@di[r.h*dx]; dx:=-dx;

        for i:=0 to r.h-1 do begin

          si:=@Lines[(r.y+i)*Zoom][bx];
          di:=@di[dx];

          if Zoom = 1 then
            Move(si^,di^,cx)
          else
          case im_Bits of
        1:  Zoom_pf1(si,di,r.w,Zoom);
        4:  Zoom_pf4(si,di,r.w,Zoom);
        8:  Zoom_pf8(si,di,r.w,Zoom);
        24: Zoom_pf24(si,di,r.w,Zoom);
          end
        end;

        Info.biSize:=Sizeof(TBitMapInfoHeader);
        Info.biWidth:=r.w;
        Info.biHeight:=r.h;
        Info.biSizeImage:=0;

        Rect:=r; Result:=true
      end
    end
  end
end;

function XImage.ContainsPoint(x,y: Integer): Boolean;
begin
  Result:=(x >= 0) and (x < im_w) and
          (y >= 0) and (y < im_h)
end;

procedure XImage.Animate(AColors: PColors;
                         AUsed: Integer;
                         AChan: PBytes);
const
  max_len = 40000 * 40000;
var
  buf: PBytes; cx,bx: Int64;
begin
  fColors:=AColors^;
  im_used:=AUsed;

  fIs_Hist:=false;

  cx:=im_Size; buf:=bmpBits;

  if Assigned(buf) then
  if im_bits in [4,8] then begin

    while cx > 0 do begin
      bx:=Min(cx,max_len);

      if im_bits = 8 then
        vga_Transit(buf,AChan,bx)
      else
        hex_Transit(buf,AChan,bx);

      Dec(cx,bx); buf:=@buf[bx]
    end

  end
end;

procedure XImage.Apply_gamma(ch0,ch1,ch2,ch3: PBytes);
begin
  if Assigned(bmpBits) then
  bmp_Gamma1(bmpBits,im_w,im_h,im_bits,im_line,
             ch0,ch1,ch2,ch3)
end;

procedure XImage.shift_left(dx: Integer);
var
  si,di,di_: PBytes; y,bx,cx,line: Integer;
begin
  line:=im_line;
  bx:=img_line(dx,im_bits);

  di:=bmpBits; si:=@di[bx];
  cx:=line - bx; di_:=@di[cx];

  for y:=1 to im_h do begin
    Move(si^,di^,cx);
    Fillchar(di_^,bx,0);
    si:=@si[line];
    di:=@di[line];
    di_:=@di_[line];
  end
end;

procedure XImage.shift_up(dy: Integer);
var
  line,cx1,cx2: int; di: PBytes;
begin
  if dy > 0 then

  if dy >= im_h then
    im_Clear(0)
  else begin
    line:=im_line;
    cx1:=dy*line;
    cx2:=(im_h-dy)*line;

    di:=bmpBits;
    Move(di[cx1],di[0],cx2);
    Fillchar(di[cx2],cx1,0)
  end
end;

procedure XImage.fill_test(cl: Integer);
var
  si: PBytes; x,y: Integer;
begin
  if Active then
  if im_bits = 24 then
  for y:=0 to im_h-1 do begin
    si:=Lines[y];
    for x:=0 to im_w-1 do begin
      si[0]:=tlong(cl).b[0];
      si[1]:=tlong(cl).b[1];
      si[2]:=tlong(cl).b[2];
      si:=@si[3]
    end;

    si:=Lines[y];
  end
end;

function XImage.IsEmpty(fc: Integer): Boolean;
var
  i,x,y,w,r,g,b,pad,loc: Integer; bp: PBytes;
begin
  Result:=true;
  if Active then begin

    bp:=bmpBits; w:=im_w;
    pad:=im_line - tif_Line(w,im_bits);

    loc:=im_loc;
    if loc <> 3 then begin w:=im_line;

      if im_used > 0 then
      if im_bits <= 8 then begin
        fc:=rgb_Indexof(@im_Colors,im_used,fc);
        if fc <> 0 then
        if im_bits = 1 then fc:=$FF else
        if im_bits = 4 then fc:=fc or (fc shl 4)
      end;

      fc:=fc and 255;
    end
    else begin
      r:=tlong(fc).b[0];
      g:=tlong(fc).b[1];
      b:=tlong(fc).b[2];
    end;
                                                   
    for y:=1 to im_h do begin

      if loc = 3 then
        for x:=1 to w do begin
          if (bp[0] <> r)
          or (bp[1] <> g)
          or (bp[2] <> b) then begin
            Result:=false; Break
          end; bp:=@bp[3]
        end
      else
        for x:=1 to w do begin
          if bp[0] <> fc then begin
            Result:=false; Break
          end; bp:=@bp[1]
        end;

      if not Result then Break;
      bp:=@bp[pad]
    end
  end
end;

procedure XImage.Swap_24_bits;
var
  y: Integer;
begin
  if im_bits = 24 then
  for y:=0 to im_h-1 do
  swap_rgb(Lines[y],im_w);
end;

procedure XImage.Swap_32_bits;
begin
  if Is_rw then
  if im_bits = 32 then
  SwapInts(Pointer(bmpBits),im_w*im_h);
end;

procedure XImage.Swap_Height;
begin
  if Is_rw then
  img_swap_height(fBmp,im_h,im_line)
end;

function XImage.to_pf32: bool;
var
  buf: XImage;
  x,y,w,h,bits,al: int;
  si,di: PBytes; _bmp: bool;
begin
  Result:=false;

  w:=im_w; h:=im_h; bits:=im_bits;

  if bits in [8,24] then begin

    buf:=XImage.Create;
    try
      _bmp:=is_bmp;

      if buf.LoadFrom(Self) then
      if im_Alloc(w,h,32) then begin

        is_bmp:=_bmp;

        for y:=0 to h-1 do begin
          si:=buf.Lines[y];
          di:=Lines[y];

          if bits = 8 then
            for x:=0 to w-1 do begin
              al:=si[0]; di[0]:=al;
              di[1]:=al; di[2]:=al; di[3]:=0;
              si:=@si[1]; di:=@di[4]
            end
          else
            for x:=0 to w-1 do begin
              di[0]:=si[0]; di[1]:=si[1];
              di[2]:=si[2]; di[3]:=0;
              si:=@si[3]; di:=@di[4]
            end
        end;

        Result:=true
      end
    finally
      buf.Free
    end
  end
end;

procedure XImage.Transit(lut: PBytes);
begin
  if Is_rw then
  if im_bits in [8,24] then
  vga_transit(fBmp,lut,im_Size)
end;

procedure XImage.rgb_to_gray(RGB: XImage);
var
  x,y,w,h: Integer; si,di: PBytes;
begin
  w:=RGB.im_w;
  h:=RGB.im_h;

  if RGB.Active then
  if RGB.im_bits = 24 then

  if Active then
  if im_bits = 8 then

  if im_w = w then
  if im_h = h then

  for y:=0 to h-1 do begin
    si:=RGB.Lines[y]; di:=Lines[y];

    for x:=0 to w-1 do begin
      di[0]:=si[0];
      si:=@si[3]; di:=@di[1];
    end
  end
end;

function XImage.Sharpen(Threshold: Float): bool;
var
  x,y: int; bp: PBytes; tr: TChannel;
begin
  if Active then
  if not fReadonly then
  if im_bits = 8 then
  if im_Fill_Hist then begin

    Hist_Channel(@tr,@fHist,256,
                 icb_square, 0,255,
                 Threshold/100);

    for y:=0 to im_h-1 do begin
      bp:=Lines[y];
      for x:=0 to im_w-1 do
      bp[x]:=tr[bp[x]]
    end
  end
end;

procedure XImage.Progress(Count: Integer; Capt,Path: PChar);
begin
  if Assigned(fOnProgress) then
  fOnProgress(Count,Capt,Path)
end;

function XImage.StepIt: Boolean;
begin
  Result:=true;
  if Assigned(fOnStepIt) then
  Result:=fOnStepIt
end;

procedure XImage.SetBmp(ABmp: PBytes);
begin
  fBmp:=ABmp;
  if fBmp = nil then fBmp:=fBuffer;
end;

function XImage.Get_Active: Boolean;
begin
  Result:=Assigned(fBmp)
end;

function XImage.Get_Is_rw: Boolean;
begin
  Result:=Assigned(fBmp) and not fReadonly
end;

function XImage.Get_is_x2: Boolean;
begin
  Result:=false; if Assigned(fBmp) then
  Result:=this_x2(im_w) and this_x2(im_h)
end;

function XImage.Get_is_gray: Boolean;
begin
  if im_bits = 8 then
  Result:=Colors_Gray(@im_Colors,256)
end;

function XImage.Get_Line(y: Integer): PBytes;
begin
  Result:=nil; if Active then begin
    if is_bmp then y:=im_h-1-y;
    Result:=@fBmp[y * im_line]
  end
end;

function XImage.Get_Size: Int64;
begin
  Result:=Int64(im_line) * im_h
end;

function XImage.Get_Pad: Integer;
begin
  Result:=im_line - (im_w * im_bits div 8)
end;

function XImage.Get_Offset(x,y: Integer): PBytes;
var
  buf: PBytes;
begin
  Result:=nil;

  if (x >= 0) and (x < im_w) then
  if (y >= 0) and (y < im_h) then begin

    buf:=Lines[y]; if Assigned(buf) then
    Result:=@buf[x * im_bits div 8]

  end
end;

procedure XBits.xor_bits(bits: PBytes;
                         const R: TRect);
var
  i,j,bx,cx,dx: Integer;
  si,di,_si,_di: PBytes;
begin
  bx:=R.Left div 8;
  dx:=(R.Right div 8) - bx + 1;

  bx:=R.Top * im_line + bx;
  cx:=R.Bottom - R.Top + 1;

  si:=@bits[bx]; di:=@fBmp[bx];

  for i:=1 to cx do begin

    _si:=si; _di:=di;

    for j:=1 to dx do begin
      _di[0]:=_di[0] xor _si[0];
      _si:=@_si[1]; _di:=@_di[1];
    end;

    si:=@si[im_line];
    di:=@di[im_line];
  end
end;

procedure XBits.xor_line(x1,x2,y: Integer);
var
  di: PBytes; p1,p2: Pbyte; ax: Byte;
begin
  x1:=Max(0,x1); x2:=Min(im_w-1,x2);

  if x1 <= x2 then begin
    di:=@fBmp[y * im_line];

    p1:=@di[x1 div 8];
    p2:=@di[x2 div 8];

    if p1 = p2 then begin
      ax:=L_Msk[x1 and 7] and R_Msk[x2 and 7];
      p1^:=p1^ xor ax
    end
    else begin
      p1^:=p1^ xor L_Msk[x1 and 7]; Inc(p1);
      p2^:=p2^ xor R_Msk[x2 and 7]; Dec(p2);

      while p1 <= p2 do begin
        p1^:=p1^ xor $FF; Inc(p1);
      end
    end
  end
end;

procedure XBits.horz_line(x1,x2,y: Integer);
var
  di: PBytes; p1,p2: PByte; cx: int;
begin
  x1:=Max(0,x1); x2:=Min(im_w-1,x2);

  if x1 <= x2 then begin
    di:=@fBmp[y * im_line];

    p1:=@di[x1 div 8];
    p2:=@di[x2 div 8];

    p1^:=p1^ or L_Msk[x1 and 7];

    if p1 = p2 then
      p1^:=p1^ and R_Msk[x2 and 7]
    else begin
      p2^:=p2^ or R_Msk[x2 and 7];
      Inc(p1); Dec(p2);

      cx:=TPointer(p2)-TPointer(p1)+1;
      if cx > 0 then Fillchar(p1^,cx,$FF);
    end
  end
end;

function XBits.is_Up(x,y: Integer): Boolean;
var
  di: PBytes;
begin
  di:=@fBmp[(y * im_line) + (x div 8)];
  Result:=di[0] and B_Bit[x and 7] <> 0
end;

procedure XBits.Up_pixel(x,y: Integer);
var
  di: PBytes;
begin
  di:=@fBmp[(y * im_line) + (x div 8)];
  di[0]:=di[0] or B_Bit[x and 7]
end;

destructor TTexture.Destroy;
begin
  xFreePtr(fBuffer);
  inherited
end;

function TTexture.Get_Active: Boolean;
begin
  Result:=false; if fSize > 0 then
  Result:=Assigned(fImage)
end;

procedure TTexture.Fill(cl: int);
begin
  if Active then
  Fillchar(fImage^,fSize,cl)
end;

function TTexture.GetMap(out map: Bitmap): Boolean;
begin
  Result:=false;
  Fillchar(map,Sizeof(map),0);

  if Active then begin
    map.bmWidth:=fWidth;
    map.bmHeight:=fHeight;
    map.bmWidthBytes:=fWidth*fDepth;
    map.bmBitsPixel:=fDepth*8;
    map.bmPlanes:=1;
    map.bmBits:=fImage;
    Result:=true
  end
end;

function TTexture.refBmp(Bmp: Pointer;
                         ASize,Acomp: Integer;
                         w,h,dep: Integer): Boolean;
begin
  fImage:=Bmp;

  fSize:=ASize; fComp:=Acomp;
  if fSize = 0 then fSize:=w * h * dep;

  fWidth:=w; fHeight:=h; fDepth:=dep;
  Result:=Active
end;

function TTexture.Assign(Buf: Pointer;
                         ASize,Acomp: Integer;
                         w,h,dep: Integer): Pointer;
begin
  xFreePtr(fBuffer);
  fBuffer:=Buf; fImage:=Buf;

  if ASize = 0 then ASize:=w * h * dep;
  fSize:=ASize; fCapacity:=fSize;

  fComp:=Acomp; fWidth:=w; fHeight:=h;
  fDepth:=dep; Result:=nil
end;

function TTexture.is_x2: Boolean;
begin
  Result:=Active and this_x2(Width) and this_x2(Height)
end;

function TTexture.Alloc(ASize,Acomp: Integer;
                        w,h,dep: Integer): Boolean;
var
  cx: Integer;
begin
  Result:=false;

  cx:=ASize;
  if cx = 0 then begin
    cx:=w * h * dep;
  end;

  if cx > fCapacity then begin
    fBuffer:=xReallocPtr(fBuffer,cx);
    if Assigned(fBuffer) then fCapacity:=cx
  end;

  fImage:=fBuffer;
  if Assigned(fImage) then begin
    fWidth:=w; fHeight:=h; fDepth:=dep;
    fComp:=Acomp; fSize:=cx; Result:=true
  end
end;

function TTexture.AllocRGB(w,h: Integer): Boolean;
begin
  Result:=Alloc(0,0,w,h,3)
end;

function TTexture.Load_BitMap(Bmp: TBitmap): Boolean;
var
  Ind, x,y,cl: Integer;
begin
  Result:=false;

  if AllocRGB(Bmp.Width,Bmp.Height) then begin
    Ind:=0;
    for y:=fHeight-1 downto 0 do
    for x:=0 to fWidth-1 do begin
      cl:=Bmp.Canvas.Pixels[x,y];
      fImage[Ind]:=tlong(cl).b[0]; Inc(Ind);
      fImage[Ind]:=tlong(cl).b[1]; Inc(Ind);
      fImage[Ind]:=tlong(cl).b[2]; Inc(Ind);
    end;

    Result:=true
  end
end;

function TTexture.Load_rgb(im: XImage): Boolean;
var
  y,line,di: Integer;
begin
  Result:=false;
  if AllocRGB(im.im_w,im.im_h) then begin

    di:=0; line:=fWidth*3;

    if im.im_line = line then
      Move(im.Lines[0]^,fImage^,im.im_Size)
    else
    for y:=0 to fHeight-1 do begin
      Move(im.Lines[y]^,fImage[di],line);
      Inc(di,line)
    end;

    Result:=true
  end
end;

function TTexture.Load_map(const map: Bitmap; bgr: bool): Boolean;
var
  x,y,y1,w,h,h1,bx1,bx2: int; si,di: PBytes;
begin
  Result:=false;

  w:=map.bmWidth;
  h:=Abs(map.bmHeight);

  if map.bmBitsPixel = 24 then
  if Assigned(map.bmBits) then

  if AllocRGB(w,h) then begin

    si:=map.bmBits; di:=fImage;

    bx1:=map.bmWidthBytes;
    bx2:=fWidth*3;

    h1:=h-1;
    for y:=0 to h1 do begin
      y1:=y;
      if map.bmHeight < 0 then
      y1:=h1-y;

      Move(si[y1*bx1],di^,bx2);
      di:=@di[bx2]
    end;

    if bgr then swap_rgb(fImage,w*h);

    Result:=true
  end
end;

function TTexture.map_to_rgba(const map: Bitmap; bgr: bool): bool;
var
  x,y,y1,w,h,h1,sline: int;
  si,si1: PBytes; di: PIntegers;
  ax,bx: tlong;
begin
  Result:=false;

  w:=map.bmWidth;
  h:=Abs(map.bmHeight);

  if map.bmBitsPixel = 24 then
  if Assigned(map.bmBits) then

  if Alloc(0,0,w,h,4) then begin

    si:=map.bmBits;
    sline:=map.bmWidthBytes;

    di:=PIntegers(fImage);

    h1:=h-1;
    if bgr then begin
      bx.i:=0;
      bx.b[0]:=si[2];
      bx.b[1]:=si[1];
      bx.b[2]:=si[0];

      for y:=0 to h1 do begin

        y1:=y;
        if map.bmHeight < 0 then y1:=h1-y;
        si1:=@si[y1*sline];

        for x:=0 to w-1 do begin
          ax.b[0]:=si1[2];
          ax.b[1]:=si1[1];
          ax.b[2]:=si1[0];
          si1:=@si1[3];

          ax.b[3]:=0;
          if ax.i <> bx.i then
          ax.b[3]:=255;

          di[0]:=ax.i; di:=@di[1]
        end
      end
    end
    else begin
      bx.i:=0;
      bx.b[0]:=si[0];
      bx.b[1]:=si[1];
      bx.b[2]:=si[2];

      for y:=0 to h1 do begin

        y1:=y;
        if map.bmHeight < 0 then y1:=h1-y;
        si1:=@si[y1*sline];

        for x:=0 to w-1 do begin
          ax.b[0]:=si1[0];
          ax.b[1]:=si1[1];
          ax.b[2]:=si1[2];
          si1:=@si1[3];

          ax.b[3]:=0;
          if ax.i <> bx.i then
          ax.b[3]:=255;

          di[0]:=ax.i; di:=@di[1]
        end
      end
    end;

    Result:=true
  end
end;

function TTexture.Load_rgba(im: XImage; cw,ch: Integer): Boolean;
var
  x,y,iw,ih,line,loc,alf: Integer;
  si,di: PBytes;
begin
  Result:=false;

  iw:=im.im_w; if cw = 0 then cw:=iw;
  ih:=im.im_h; if ch = 0 then ch:=ih;
  loc:=im.im_loc;

  if loc in [1,3] then
  if Alloc(0,0,iw,ih,4) then begin

    di:=fImage; line:=iw*fDepth;

    for y:=0 to ih-1 do begin

      si:=im.Lines[y];
      for x:=0 to iw-1 do begin

        if loc = 1 then begin
          di[0]:=si[0]; di[1]:=si[0]; di[2]:=si[0]
        end
        else begin
          di[0]:=si[0]; di[1]:=si[1]; di[2]:=si[2]
        end;

        alf:=0;
        if (x < cw) and (y < ch) then alf:=255;
        di[3]:=alf;

        di:=@di[4]; si:=@si[loc]
      end
    end;

    Result:=true
  end
end;

procedure TTexture.SetTile(x,y,w,h: int; buf: PBytes);
var
  bx1,bx2,cx,loc: int; si,di: PBytes;
begin
  if Assigned(fImage) then begin

    loc:=fDepth;

    bx1:=fWidth*loc; bx2:=w*loc;

    si:=buf;
    if x < 0 then begin
      Inc(w,x); si:=@si[Abs(x)*loc]; x:=0
    end;

    if y < 0 then begin
      Inc(h,y); si:=@si[Abs(y)*bx2]; y:=0
    end;

    if x+w > fWidth then w:=fWidth-x;
    if y+h > fHeight then h:=fHeight-y;

    cx:=w*loc;
    if (w > 0) and (h > 0) then
    while h > 0 do begin
      di:=@fImage[y*bx1+x*loc];
      Move(si^,di^,cx); si:=@si[bx2];
      Inc(y); Dec(h)
    end
  end
end;

procedure TTexture.Clear;
begin
  fImage:=nil; fSize:=0; fComp:=0
end;

procedure TTexture.Opacity(Color: int);
var
  si,tmp: PBytes; di: PIntegers;
  i,n,hi: Integer; c: tlong;
begin
  if Active then
  if fDepth = 3 then begin

    n:=fWidth*fHeight;
    di:=xAllocPtr(n*4);

    if Assigned(di) then begin

      c.i:=Color; c.b[3]:=255;

      tmp:=fImage; si:=fImage;
      fImage:=Pointer(di); fDepth:=4;

      for i:=0 to n-1 do begin

        hi:=si[0];
        Inc(hi,si[1]);
        Inc(hi,si[2]);

        if hi < 100 then di[0]:=0
        else di[0]:=c.i;

        si:=@si[3]; di:=@di[1]
      end;

      xFreePtr(tmp)
    end
  end
end;

end.
