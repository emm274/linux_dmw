unit img_dll; interface

uses
  Classes,LCLType,Math,
  otypes,img_x,idib;

function im_dll_Open(Path: PChar; info: PBitMapInfoHeader): Boolean;
function im_dll_Update(Path: PChar; info: PBitMapInfoHeader): Boolean;
function im_dll_Stat(Path: PChar; info: PBitMapInfoHeader): Boolean;

procedure im_dll_Done;
procedure im_dll_Free;

function im_dll_data(out Offset: Integer): PBytes;

function im_dll_size(Path: PChar): Integer;
function im_dll_bmp(Path: PChar; out w,h,flags: Integer): Boolean;
function im_dll_This(Path: PChar; info: PBitMapInfoHeader): Boolean;
function im_dll_bits(Path: PChar): Integer;

function im_dll_Gray(Path: PChar): Boolean;

function im_dll_Palette(Quads: PRGBQuads): Integer;

function im_dll_Palette_Gray: Boolean;
function im_dll_Palette_Trunc: Integer;

procedure im_dll_Draw(DC: ICanvas; Path: PChar;
                      dc_x,dc_y,dc_w,dc_h: int;
                      src_x,src_y: int; kz: Float);

function im_dll_Dib(Dib: PBytes; Width,Height: Integer;
                    Path: PChar; xPos,yPos,Zoom: Integer;
                    Quads: PRGBQuads): Integer;

function im_dll_Flags: longint;
function im_dll_Position: longint;
procedure im_dll_GotoLine(y: Integer; buf: PBytes);
procedure im_dll_ReadLine(y: Integer; buf: PBytes);

function im_dll_Tiled(var tile_w,tile_h: Integer): Boolean;
function im_dll_GetTile(buf: PBytes; x,y,w,h,size: longint): Pbytes;

function im_dll_Image(im: XImage): Boolean;

function im_dll_Get_Hex(chan: PBytes): longint;

procedure im_dll_Put_Index(indp: pIndex; len: longint);
procedure im_dll_Put_Hex(chan: PBytes);

procedure im_dll_Put_Palette(Quads: PRGBQuads);

function im_tif_Info(Path: PChar;
                     out Comp: Integer;
                     out tile_w: Integer;
                     out tile_h: Integer): Boolean;

type
  TInline = class

    constructor Create(info: PBitMapInfoHeader);
    destructor Destroy; override;

    function Pixel(x,y: Integer): Integer;

  private
    fcash: pbytes;
    cash_y: Integer;
    cash_h: Integer;

    cash_depth: Integer;
    cash_page: Integer;
    cash_size: Integer;
    page_size: Integer;

    line: Integer;
    width: Integer;
    height: Integer;
    bits: Integer;

  public
    property cash: PBytes read fcash;
  end;

procedure img_Copy_hex(src,dst: PChar);

implementation

uses
  dynlibs,
  SysUtils,
  ofiles,xgdi;

type
  tim_Open = function(Path: PChar; info: PBitMapInfoHeader): Integer; stdcall;
  tim_Update = function(Path: PChar; info: PBitMapInfoHeader): Integer; stdcall;
  tim_Stat = function(Path: PChar; info: PBitMapInfoHeader): Integer; stdcall;
  tim_Done = procedure; stdcall;
  tim_Free = procedure; stdcall;

  tim_Bmp = function(out Offset: Integer): PBytes; stdcall;

  tim_Palette = function(Quads: PRGBQuads): Integer; stdcall;

  tim_Draw = procedure(DC: ICanvas; Path: PChar;
                       dc_x,dc_y,dc_w,dc_h: int;
                       src_x,src_y: int; kz: Float); stdcall;

  tim_Dib = function(Dib: pbytes; Width,Height: Integer;
                     Path: PChar; xPos,yPos,Zoom: Integer;
                     Quads: PRGBQuads): Integer; stdcall;

  tim_Flags = function: longint; stdcall;
  tim_Position = function: longint; stdcall;
  tim_GotoLine = procedure(y: Integer; buf: pbytes); stdcall;
  tim_ReadLine = procedure(y: Integer; buf: pbytes); stdcall;

  tim_Tiled = function(var tile_w,tile_h: Integer): Boolean; stdcall;
  tim_GetTile = function(buf: pBytes; x,y,w,h,size: longint): pbytes; stdcall;

  tim_Get_hex = function(chan: PBytes): longint; stdcall;

  tim_Put_Index = procedure(indp: PIndex; len: longint); stdcall;
  tim_Put_hex = procedure(chan: PBytes); stdcall;

  tim_Put_Palette = procedure(Quads: PRGBQuads); stdcall;

var
  Lib: HModule;

  im_Open: tim_Open;
  im_Update: tim_Update;
  im_Stat: tim_Stat;
  im_Done: tim_Done;
  im_Free: tim_Free;

  im_Bmp: tim_Bmp;

  im_Palette: tim_Palette;
  im_Draw: tim_Draw;
  im_Dib: tim_Dib;

  im_Flags: tim_Flags;
  im_Position: tim_Position;
  im_GotoLine: tim_GotoLine;
  im_ReadLine: tim_ReadLine;

  im_Tiled: tim_Tiled;
  im_GetTile: tim_GetTile;

  im_Get_Hex: tim_Get_Hex;

  im_Put_Index: tim_Put_Index;
  im_Put_Hex: tim_Put_hex;

  im_Put_Palette: tim_Put_Palette;

  im_dll: TShortStr;

procedure Init;
begin
  Lib:=0; StrCopy(im_dll,'');
end;

procedure Done;
begin
  if Lib >= 32 then FreeLibrary(Lib);
  Lib:=0;
end;

function dll_Open(dll,Path: PChar; info: PBitMapInfoHeader): Boolean;
begin
  Result:=false;
  if Lib >= 32 then FreeLibrary(Lib);
  Lib:=0; Lib:=LoadLibrary(dll);

  if Lib >= 32 then begin
    im_Open:=GetProcAddress(Lib,'im_Open');
    im_Update:=GetProcAddress(Lib,'im_Update');
    im_Stat:=GetProcAddress(Lib,'im_Stat');
    im_Done:=GetProcAddress(Lib,'im_Done');
    im_Free:=GetProcAddress(Lib,'im_Free');

    im_Bmp:=GetProcAddress(Lib,'im_Bmp');

    im_Palette:=GetProcAddress(Lib,'im_Palette');
    im_Draw:=GetProcAddress(Lib,'im_Draw');
    im_Dib:=GetProcAddress(Lib,'im_Dib');

    im_Flags:=GetProcAddress(Lib,'im_Flags');
    im_Position:=GetProcAddress(Lib,'im_Position');
    im_GotoLine:=GetProcAddress(Lib,'im_GotoLine');
    im_ReadLine:=GetProcAddress(Lib,'im_ReadLine');

    im_Tiled:=GetProcAddress(Lib,'im_Tiled');
    im_GetTile:=GetProcAddress(Lib,'im_GetTile');

    im_Get_Hex:=GetProcAddress(Lib,'im_Get_Hex');

    im_Put_Index:=GetProcAddress(Lib,'im_Put_Index');
    im_Put_Hex:=GetProcAddress(Lib,'im_Put_Hex');

    im_Put_Palette:=GetProcAddress(Lib,'im_Put_Palette');

    if im_Open(Path, info) = S_OK then begin
      StrCopy(im_dll,dll); Result:=true
    end
    else begin
      im_Done; Done
    end
  end;
end;

function im_dll_Open(Path: PChar; info: PBitMapInfoHeader): Boolean;
var
  mag: Integer; ext: TShortstr;
begin
  Result:=false;

  StrPCopy(ext,ExtractFileExt(Path));

  if Lib >= 32 then
  if im_Open(Path, info) = S_OK then
    Result:=true
  else begin
    im_Done; Done
  end;

  if not Result then

  if StrIComp(ext,'.xml') = 0 then

    Result:=dll_Open('lib_tif.so',Path, info)

  else begin

    mag:=Get_mag2(Path);
    if (mag = $4949) or (mag = $4D4D) then
      Result:=dll_Open('lib_tif.so',Path, info)
    else begin
      Result:=dll_Open('lib_tif.so',Path, info);

      if not Result then
      Result:=dll_Open('dll_bmp.dll',Path, info);
    end
  end
end;

function im_dll_data(out Offset: Integer): PBytes;
begin
  Result:=nil;

  if Lib >= 32 then
  if Assigned(im_Bmp) then
  Result:=im_Bmp(Offset)
end;

function im_dll_Update(Path: PChar; info: PBitMapInfoHeader): Boolean;
begin
  Result:=false;
  if im_dll_Open(Path,info) then
  if Assigned(im_Update) then
  Result:=im_Update(Path,info) > 0
end;

function im_dll_Stat(Path: PChar; info: PBitMapInfoHeader): Boolean;
begin
  Result:=false;

  if Lib >= 32 then
  if Assigned(im_Stat) then
  Result:=im_Stat(Path,info) > 0
end;

procedure im_dll_Done;
begin
  if Lib >= 32 then
  im_Done
end;

procedure im_dll_Free;
begin
  if Lib >= 32 then
  if Assigned(im_Free) then
  im_Free else im_Done
end;

function im_dll_size(Path: PChar): Integer;
var
  info: TBitMapInfoHeader;
  bits,line: Integer;
begin
  Result:=0;

  if im_dll_Open(Path, @info) then begin
    bits:=img_Bits(@info);
    line:=img_Line(info.biWidth,bits);
    Result:=line * info.biHeight
  end; im_dll_Done
end;

function im_dll_bmp(Path: PChar; out w,h,flags: Integer): Boolean;
var
  info: TBitMapInfoHeader;
begin
  Result:=false; w:=0; h:=0; flags:=0;

  if im_dll_Open(Path, @info) then begin
    w:=info.biWidth; h:=info.biHeight;
    flags:=im_Flags; Result:=true
  end; im_dll_Done
end;

function im_dll_This(Path: PChar; info: PBitMapInfoHeader): Boolean;
var
  Quads: TRGBQuads; used: Integer;
begin
  Result:=im_dll_Open(Path, info);

  if Result then begin
    used:=im_Palette(@Quads); if used >= 16 then
    if Quads_Gray(@Quads,used) then used:=0;
    info.biClrUsed:=used
  end;

  im_dll_Done
end;

function im_dll_bits(Path: PChar): Integer;
var
  Info: TBitMapInfoHeader;
begin
  Result:=0;
  if im_dll_This(Path,@Info) then
  Result:=img_Bits(@Info)
end;

function im_dll_Gray(Path: PChar): Boolean;
var
  info: TBitMapInfoHeader;
begin
  Result:=false;
  if im_dll_Open(Path, @info) then
  Result:=im_dll_Palette_Gray;
  im_dll_Done
end;

function im_dll_Palette(Quads: PRGBQuads): Integer;
begin
  Result:=im_Palette(Quads)
end;

function im_dll_Palette_Gray: Boolean;
var
  Quads: TRGBQuads; used: Integer;
begin
  Result:=false;
  used:=im_Palette(@Quads); if used >= 16 then
  Result:=Quads_Gray(@Quads,used)
end;

function im_dll_Palette_Trunc: Integer;
var
  Quads: TRGBQuads;
begin
  Result:=im_Palette(@Quads);
  if Result = 256 then
  Result:=Trunc_Quads(@Quads,256,nil)
end;

procedure im_dll_Draw(DC: ICanvas; Path: PChar;
                      dc_x,dc_y,dc_w,dc_h: int;
                      src_x,src_y: int; kz: Float);
begin
  im_Draw(DC, Path,
          dc_x,dc_y,dc_w,dc_h,
          src_x,src_y, kz)
end;

function im_dll_Dib(Dib: pbytes; Width,Height: Integer;
                    Path: PChar; xPos,yPos,Zoom: Integer;
                    Quads: PRGBQuads): Integer;
begin
  Result:=0; if Assigned(im_Dib) then
  Result:=im_Dib(Dib,Width,Height, Path,xPos,yPos,Zoom, Quads)
end;

function im_dll_Flags: longint;
begin
  Result:=im_Flags
end;

function im_dll_Position: longint;
begin
  Result:=im_Position
end;

procedure im_dll_GotoLine(y: Integer; buf: pBytes);
begin
  im_GotoLine(y,buf)
end;

procedure im_dll_ReadLine(y: Integer; buf: pBytes);
begin
  im_ReadLine(y,buf)
end;

function im_dll_Tiled(var tile_w,tile_h: Integer): Boolean;
begin
  Result:=false;
  if Assigned(im_Tiled) then
  Result:=im_Tiled(tile_w,tile_h)
end;

function im_dll_GetTile(buf: PBytes; x,y,w,h,size: longint): pbytes;
begin
  Result:=im_GetTile(buf,x,y,w,h,size)
end;

function im_dll_Image(im: XImage): Boolean;
begin
  Result:=im_dll_GetTile(im.bmpBits,
    0,0,im.im_w,im.im_h,im.im_Size) <> nil;
end;

function im_dll_Get_Hex(chan: PBytes): longint;
begin
  Result:=im_Get_Hex(chan)
end;

procedure im_dll_Put_Index(indp: pIndex; len: longint);
begin
  im_Put_Index(indp,len)
end;

procedure im_dll_Put_Hex(chan: PBytes);
begin
  im_Put_Hex(chan)
end;

procedure im_dll_Put_Palette(Quads: pRGBQuads);
begin
  im_Put_Palette(Quads)
end;

function im_tif_Info(Path: PChar;
                     out Comp: Integer;
                     out tile_w: Integer;
                     out tile_h: Integer): Boolean;
var
  info: TBitMapInfoHeader;
begin
  Result:=false; Comp:=bi_RGB;
  tile_w:=0; tile_h:=0;

  if StrLen(Path) > 0 then
  if not This_ext(Path,'.FOT') then
  if im_Open(Path,@info) = S_OK then begin
    Result:=im_Tiled(tile_w,tile_h);
    Comp:=info.biCompression;
  end;

  im_dll_Done
end;

constructor TInline.Create(info: PBitMapInfoHeader);
begin
  inherited Create;

  line:=info.biSize;
  width:=info.biWidth;
  height:=info.biHeight;
  bits:=img_Bits(info);

  cash_y:=0; cash_h:=0;
  cash_depth:=Max(64,height div 8);
  cash_page:=Max(1,cash_depth div 8);

  cash_size:=cash_depth*line;
  page_size:=cash_page*line;

  fcash:=xAllocPtr(cash_size);

  if cash <> nil then
  im_dll_GotoLine(0,cash)
end;

destructor TInline.Destroy;
begin
  xFreePtr(fcash);
  inherited
end;

function TInline.Pixel(x,y: Integer): Integer;
const
  B_Bit: array[0..7] of byte =
  ($80,$40,$20,$10,$08,$04,$02,$01);
var
  ofs: Integer;
begin
  Result:=-1;

  if (x >= 0) and (x < width) then
  if (y >= 0) and (y < height) then
  if y >= cash_y then begin

    while y >= cash_y+cash_h do begin

      if cash_h = cash_depth then begin
        Inc(cash_y,cash_page); Dec(cash_h,cash_page);
        Move(cash[page_size],cash^,cash_size-page_size)
      end;

      im_dll_ReadLine(cash_y+cash_h,@cash[cash_h*line]);
      Inc(cash_h)
    end;

    ofs:=(y-cash_y) * line; Result:=0;

    case bits of
  1:  if cash[ofs + (x div 8)] and B_Bit[x and 7] <> 0 then
      Result:=1;
  8:  Result:=cash[ofs+x];
    end

  end
end;

procedure img_Copy_hex(src,dst: PChar);
var
  info: TBitMapInfoHeader; chan: TChannel;
begin
  if im_dll_Open(src,@info) then begin
    Init_Gray_Channel(@chan);
    if im_dll_Get_Hex(@chan) > 0 then
    if im_dll_Update(dst,@info) then
    im_dll_Put_Hex(@chan);
    im_dll_Done;
  end
end;

initialization Init;
finalization Done;

end.


