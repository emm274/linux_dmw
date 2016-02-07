unit tiff_x; interface

uses
  Classes,Graphics,
  LCLType,Math,Windows,
  otypes,xevents,oframes,
  xlist,ogauss,tiff_y,
  ijpg,img_glu,xsid,xecw,
  ijl,img_ijl,
  img_x;

const
  bi_REL = 10;
  bi_VEG = 11;
  bi_GR3 = 13;
  bi_GR4 = 14;
  bi_LZW = 15;
  bi_RLC = 19;
  bi_JPG = 21;
  bi_ZIP = 23;
  bi_ECW = 24;
  bi_SID = 25;
  bi_WMS = 26;
  bi_ERR = 27;

  bi_ARJ = [bi_LZW,bi_GR3,bi_GR4,bi_ZIP];

  Comp_rgb = 1;
  Comp_jpg = 7;
  Comp_zip = 8;
  Comp_jpf = 12346;
  Comp_ecw = 997;
  Comp_zlb = 998;

  Levels_Max = 32;

  def_quality = 96;

type
  TTiff = class
    constructor Create;
    destructor Destroy; override;

    procedure SetRel(fmt: int; min,max: float);

    function file_Create(Dest: PChar; tags: int): THandle;

    function im_Create(Path: PChar;
                       bmp: PBitMapInfoHeader;
                       Quads: PRGBQuads): THandle;

    function Append_hdr(tags: int): int64;

    function Append_dir(tag,typ,len: int; ind: int64): int64;
    function Close_dir: int64;

    function next_layer(tags: int): bool;

    procedure Append_list(List: TTiffData);
    procedure Update_list(List: TTiffData);

    function Continue_dir(tags: int): int64;

    function Update_dir(tag,typ,len: int; ind: int64): int;

    function Update_buf(tag: int;
                        buf: pointer; len: int): int64;

    procedure Update_rgb(rgbp: PRGBQuads; used: int);
    function Update_ind(tag,typ,len: int): int64;

    function buff_Write(var buf; len: int): int64;

    function Get_tile(bmp,buf: PBytes;
                      im_w,im_h,im_bits,zoom: int;
                      pos: int64; len: int): int;

    procedure file_Read(var buf; len: int); virtual;
    procedure file_Write(var buf; len: int); virtual;
    function file_Seek(pos: int64): int64; virtual;
    function file_Size: int64; virtual;
    function file_pos: int64; virtual;

    procedure geo_Scale(kz: Double);

    procedure geo_Append_dir;
    procedure geo_Append_data;

  private
    file_h: THandle;

    fIsBig: longbool;

    dir_pos: int64;
    dir_size: DWord;

    close_pos: int64;

    ftile: TBitMapInfoHeader;
    fijl_props: TJPEG_Tables;

    fgeo: lg_Transit;

    frel: record
      fmt: int; min,max: float
    end;

    fError: int;

    procedure SetIsBig(Value: Boolean); virtual;

    function GetGeoCount: Integer;
    procedure SetGeo(Ageo: lg_Transit);

    procedure Set_tile(const bmp: TBitMapInfoHeader);

  public
    property Handle: THandle read file_h;

    property IsBig: Boolean write SetIsBig;

    property GeoCount: Integer read GetGeoCount;
    property geo: lg_Transit write SetGeo;

    property tile: TBitMapInfoHeader read ftile write Set_tile;
    property ijl_props: TJPEG_Tables write fijl_props;

    property Error: int read fError;
  end;

  TTiffTiled = class(TTiff)

    constructor Create;
    destructor Destroy;

    function kh_Create1(ADest: PChar;
                        nx,ny,xpage,ypage,bits: int;
                        Quads: PRGBQuads; used: int;
                        iw,ih,fc: int; Chan: PBytes): Boolean;

    function kh_Create(ADest: PChar;
                       nx,ny,page,bits: int;
                       Quads: PRGBQuads; used: int;
                       iw,ih,fc: int; Chan: PBytes): Boolean;

    procedure Close_doc;
    function Close_level: Integer;

    procedure Build_pyramid(fc: Integer);

    procedure Put_tile(i,j: Integer;
                       buf: PBytes; len: Integer);

    procedure Pack_tile(i,j: Integer; buf: PBytes);

  private
    fSize: TSize;

    fXTiles: int;
    fYTiles: int;
    fNTiles: int;

    foff_typ: int;
    foff_loc: int;

    foff32p: PDWords;
    foff64p: PInt64s;

    flenp: PDWords;
    foffp_ind: int64;
    flenp_ind: int64;

    fTileCount: Integer;
    fLevelCount: Integer;

    fPalette: HPalette;
    fQuads: TRGBQuads;

    fbufLen: Integer;
    fbuffer: PBytes;

    fBits: Integer;
    fQuality: Integer;

    fOnLevel: TIntegerProc;

    fClearColor: Integer;

    fIsChkBig: bool;
    fIsTga: longbool;
    fIsPng: longbool;

    fPath: TShortstr;

    function GetPath: PChar;

    procedure SetIsBig(Value: Boolean); override;

  public
    property LevelCount: Integer read fLevelCount;
    property TileCount: Integer read fTileCount;

    property XTiles: Integer read fXTiles;
    property YTiles: Integer read fYTiles;
    property NTiles: Integer read fNTiles;
    property Size: TSize read fSize;

    property Palette: HPalette read fPalette;

    property ClearColor: Integer read fClearColor;
    property Quality: Integer write fQuality;

    property IsTga: longbool write fIsTga;
    property IsPng: longbool write fIsPng;
    property IsChkBig: bool write fIsChkBig;

    property Path: PChar read GetPath;

    property OnLevel: TIntegerProc write fOnLevel;
  end;

  PTilePos64 = ^TTilePos64;
  TTilePos64 = record
    pos: int64; len: DWord
  end;

  PTilePosArr64 = ^TTilePosArr64;
  TTilePosArr64 = array[0..1023] of TTilePos64;

  PTilePos32 = ^TTilePos32;
  TTilePos32 = record
    pos,len: DWord
  end;

  PTilePosArr32 = ^TTilePosArr32;
  TTilePosArr32 = array[0..1023] of TTilePos32;

  TTilePosList64 = class(TCustomList)
    constructor Create;
  end;

  PTiff_tile = ^TTiff_tile;
  TTiff_tile = record
    Width,Height,BufSize: Integer;
    XTiles,YTiles,NTiles: Integer;
    BufLine: Integer;

    off_pos,len_pos: Integer;
    offp,lenp: PIntegers;

    bufp: PBytes;
  end;

  TTiff_typ = (typ_tif,typ_jpg,typ_sid,typ_ecw);

  PTiff_level = ^TTiff_level;
  TTiff_level = record
    Lev,Typ: Integer;      // 0 - lines; 1 - tiles
    Width,Height: Integer;

    off_pos: int64;
    off_typ: int;
    len_pos: int64;
    len_typ: int;

    bit_pos,bit_len: Integer;

    tileWidth: Integer;
    tileHeight: Integer;
    tileLine: Integer;
    tileSize: Integer;
    tiffLine: Integer;

    XTiles: Integer;
    YTiles: Integer;
    NTiles: Integer;
    NPlanes: Integer
  end;

  TTiff_levels = array[0..Levels_Max-1] of TTiff_level;

  TTiffView = class

    constructor Create;
    destructor Destroy; override;

    function Open(APath: PChar): Boolean; virtual;
    procedure Close; virtual;

    function GetBitmap(out map: Bitmap): bool;

    procedure jpg_to_tiled(tw,th: Integer);

    function lines_to_tiled: Boolean;

    function Get_tile_buf: Pointer;
    procedure Free_tile_buf;

    function zoom_Enabled: Boolean;

    procedure zoom_out(w,h: Integer);
    function scale_out(iz: Integer): Integer; virtual;
    function scale2_out(x2: Integer): Integer;

    function Next_zoom(var zoom: Integer): Boolean; virtual;
    procedure Next_layer(var iw,ih: int; iz: int); virtual;

    function Get_zoom: Integer;
    function zoom_tiles(iz: Integer): Integer;

    function Get_jpeg_tile(im: PBytes): Boolean;

    function tile_Exist(x,y: Integer): Boolean;

    function Ptr_tile(x,y: Integer): PBytes;

    function Seek_tile(x,y: Integer;
                       out ptr: PBytes;
                       out len: DWord): Boolean;

    function Get_ijl_tile(im: PBytes): Boolean; virtual;
    function Get_tile(x,y: Integer; im: PBytes): Boolean; virtual;
    function Zoom_tile(x,y: Integer; im: PBytes): Boolean; virtual;
    function Pack_tile(x,y,zoom: Integer; im: PBytes): Boolean; virtual;

    procedure Set_tile(x,y: Integer; im: PBytes); virtual;

    procedure Begin_progress(Sender: TObject); virtual;

    function Get_dib(x,y: Integer; im: PBytes): Boolean;
    procedure Set_dib(x,y: Integer; im: PBytes);

    function Get_tex(x,y,zoom: Integer; tex: TTexture): Boolean;

    function Zoom_rect(x,y,w,h,iz: Integer; im: XImage): Boolean;

    function Get_Rect(im: XImage; x,y,w,h,iz: Integer): Boolean;

    function Get_pbuf_size: TSize; virtual;
    function fill_pbuf(ip,jp: int): Boolean; virtual;
    function pbuf_tile(i,j: int; im: PBytes): Boolean; virtual;

  protected
    fInfo: TBitMapInfoHeader;
    ftile: TTiff_tile;
    fQuads: TRGBQuads;

    fTiles: TTilePosList64;

    fData: TTiff_level;

    flevels: TTiff_levels;
    fLevelCount: Integer;

    fQuality: int;
    fPhotometric: int;

    fPlanarConfig: int;
    fNPlanes: int;
    
    fClearColor: int;
    fMinValue: int;
    fMaxValue: int;
    fSampleFormat: int;

    fim_typ: TTiff_typ;

    fis_mac: Longbool;
    fis_big: Longbool;

    fis_cmyk: Longbool;
    fis_Jpg: Longbool;
    fis_upd: Longbool;
    fis_tif: Longbool;

    fis_Swap: Longbool;
    fis_Zoom: Longbool;
    fis_bmp: Longbool;
    fis_ijl: Longbool;
    fis_hdif: Longbool;
    fis_x16: Longbool;
    fis_x32: Longbool;

    fOnStatus: TOnStr;

  private
    fFile: IFile;

    fsid: tltiImage;
    fsidBuf: XImage;
    fsidBuf1: XImage;
    fsidPos: TPoint;
    fsidClip: XRect;

    fecw: TNCSFileView;

    fjpg: IJpeg;
    fjpg_zoom: Integer;
    fjpg_bits: Integer;
    fjpg_size: TPoint;

    rgb_pos: int64;
    rgb_len: Integer;

    fDataBuf: PBytes;
    fDataSize: Integer;
    fDataTop: Integer;

    fjpg_props_pos: int64;
    fjpg_props_len: Integer;
    fjpg_props: TJPEGTables;

    fScanPtr: PBytes;
    fScanTop: Integer;
    fScanRows: Integer;

    fCrypt: DWord;

    fIsChan: Longbool;
    fChan: TChannel;

    fWaitRGB: Longbool;
    fWaitTiled: Longbool;
    fScanColumns: Longbool;

    fis_float: Longbool;
    fSMinSampleValue: float;
    fSMaxSampleValue: float;

    fOnlyData: Longbool;

    fFileSize: int64;

    fPath: TShortstr;

    function Get_Path: PChar;

    function Get_Active: Boolean;

    function jpg_enabled: bool;

    function Get_is_tiled: Boolean;
    function Get_is_striped: Boolean;
    function Get_is_lines: Boolean;

    function GetColor(Ind: Integer): Integer;

    function GetDataBuf(len: Integer): Pointer;

    procedure SetData(const lev: TTiff_level);

    function Verify_tiles(offPos: int64; offTyp: int;
                          lenPos: int64; lenTyp: int;
                          Count: int): Boolean;

    function Verify_tiles1(offPos: int64; offTyp: int;
                           lenPos: int64; lenTyp: int;
                           Count: int): Boolean;

    function Load_tiles(offPos: int64; offTyp: int;
                        lenPos: int64; lenTyp: int;
                        Count: int): Boolean;

    procedure Update_tile(Ind,Pos,Len: DWord);

    procedure Set_status(Str: PChar);

    procedure Set_LevelIndex(Ind: Integer);

    function Get_used_Quads: PRGBQuads;
    function Get_Quads_Used: Integer;

    function Get_im_bits: Integer;
    function Get_is_gray: Boolean;

    procedure swap_dib(im: PBytes);

    function GetLine(y: Integer): PBytes;

    procedure SetSidClip(const R: XRect);

    procedure swap_rgb24(im: PBytes; w,h: Integer);

  public
    property Active: Boolean read Get_Active;

    property Crypt: DWord read fCrypt;

    property FileSize: int64 read fFileSize;

    property Colors[Ind: Integer]: Integer read GetColor;

    property Info: TBitMapInfoHeader read fInfo;

    property tile: TTiff_tile read ftile;

    property LevelIndex: Integer write Set_LevelIndex;
    property levelCount: Integer read fLevelCount;
    property levels: TTiff_levels read flevels;

    property SidClip: XRect write SetSidClip;

    property im_bits: Integer read Get_im_bits;
    property is_gray: Boolean read Get_is_gray;

    property is_tiled: Boolean read Get_is_tiled;
    property is_striped: Boolean read Get_is_striped;
    property is_lines: Boolean read Get_is_lines;

    property im_typ: TTiff_typ read fim_typ;

    property is_Big: Longbool read fIs_Big;
    property is_Jpg: Longbool read fIs_Jpg;
    property is_tif: Longbool read fIs_tif;
    property is_bmp: Longbool read fis_bmp;
    property is_upd: Longbool write fis_upd;
    property is_zoom: Longbool write fis_zoom;
    property is_Swap: Longbool read fis_swap write fis_swap;
    property is_ijl: Longbool read fis_ijl;

    property jpg_props: TJPEGTables read fjpg_props;

    property Quads: TRGBQuads read fQuads write fQuads;
    property used_Quads: PRGBQuads read Get_used_Quads;
    property Quads_Used: Integer read Get_Quads_Used;

    property ClearColor: Integer read fClearColor
                                 write fClearColor;

    property Photometric: Integer read fPhotometric
                                  write fPhotometric;

    property IsChan: Longbool read fIsChan;
    property Chan: TChannel read fChan;

    property OnlyData: Longbool write fOnlyData;

    property MinValue: int read fMinValue;
    property MaxValue: int read fMaxValue write fMaxValue;
    property SampleFormat: int read fSampleFormat;

    property is_float: Longbool read fis_float;
    property SMinSampleValue: float read fSMinSampleValue;
    property SMaxSampleValue: float read fSMaxSampleValue;

    property WaitRGB: Longbool write fWaitRGB;
    property WaitTiled: Longbool write fWaitTiled;

    property ScanColumns: Longbool read fScanColumns
                                   write fScanColumns;

    property Path: PChar read Get_Path;

    property Status: PChar write Set_status;
  end;

  XTiffView = class(TTiffView)
    constructor Create(ABits: tbyte_set);
    function Open(Path: PChar): Boolean; override;
  private
    fBits: tbyte_set;
  end;

function TilePos(pos: int64; len: DWord): TTilePos64;

procedure rlc_Unpack(Inbuf: PBytes; ilen: Integer;
                     Outbuf: PBytes; len: Integer);

function tiff_add_pyramid(Path1,Path2: PChar): Boolean;

function tiff_crypted(Path: PChar): Boolean;

implementation

uses
  LCLIntf,SysUtils,
  convert,ofiles,xline,
  xbl_use,xgdi,xlzw;

var
  cid_key: DWord;

function TilePos(pos: int64; len: DWord): TTilePos64;
var
  r: TTilePos64;
begin
  r.pos:=pos; r.len:=len; Result:=r
end;

function Image_level(const inf: TBitmapInfoHeader;
                     typ,pos,len: Integer): TTiff_level;
var
  lev: TTiff_level; bits: Integer;
begin
  Fillchar(lev,Sizeof(lev),0);

  lev.Typ:=typ;

  lev.Width:=inf.biWidth;
  lev.Height:=inf.biHeight;

  lev.bit_pos:=pos;
  lev.bit_len:=len;

  lev.tileWidth:=inf.biWidth;
  lev.tileHeight:=inf.biHeight;

  lev.tileLine:=inf.biSize;
  if lev.tileLine = 0 then begin
    bits:=img_Bits(@inf);
    lev.tileLine:=img_Line(inf.biWidth,bits)
  end;

  lev.XTiles:=1;
  lev.YTiles:=1;
  lev.NTiles:=1;

  Result:=lev
end;

procedure Level_tiled(var lev: TTiff_level; bits: Integer);
var
  w,h: Integer;
begin
  with lev do begin

    w:=Width; h:=Height;
    if (w > 8192) or (h > 8192) then begin

      w:=256; h:=256;
      while (w < 4096) and (w < Width) do w:=w*2;
      while (h < 4096) and (h < Height) do h:=h*2;

      if w > Width then w:=Width;
      if h > Height then h:=Height;
    end;

    tileWidth:=w;
    tileHeight:=h;
    tileLine:=img_line(w,bits);
    tileSize:=tileLine * h;

    XTiles:=int_Tiles(Width,w);
    YTiles:=int_Tiles(Height,h);
    NTiles:=XTiles * YTiles;
  end
end;

procedure Level_tiled1(var lev: TTiff_level; w,h,bits: Integer);
begin
  with lev do begin
    tileWidth:=w;
    tileHeight:=h;
    tileLine:=img_line(w,bits);
    tileSize:=tileLine * h;

    XTiles:=int_Tiles(Width,w);
    YTiles:=int_Tiles(Height,h);
    NTiles:=XTiles * YTiles;
  end
end;

procedure rlc_Unpack(Inbuf: PBytes; ilen: int;
                     Outbuf: PBytes; len: Int);
var
  si,di: PBytes; acc,col: Byte;
begin
  si:=Inbuf; di:=Outbuf;

  while len > 0 do begin

    if ilen <= 0 then Break;
    acc:=si[0]; si:=@si[1]; Dec(ilen);

    if acc > 128 then begin
      if ilen <= 0 then Break;
      col:=si[0]; si:=@si[1]; Dec(ilen);

      Dec(acc); while acc > 0 do begin
        di[0]:=col; di:=@di[1]; Dec(len);
        if len <= 0 then Break; Inc(acc)
      end
    end else
    if acc < 128 then begin
      Inc(acc); while acc > 0 do begin
        if ilen <= 0 then Break;
        col:=si[0]; si:=@si[1]; Dec(ilen);

        di[0]:=col; di:=@di[1]; Dec(len);
        if len <= 0 then Break; Dec(acc)
      end
    end
  end
end;

constructor TTilePosList64.Create;
begin
  inherited Create(Sizeof(TTilePos64),4096*16)
end;

constructor TTiffView.Create;
begin
  inherited;

  fsid:=tltiImage.Create;

  fsidBuf:=XImage.Create;
  fsidBuf.is_tif:=false;

  fsidBuf1:=XImage.Create;
  fsidBuf1.is_tif:=false;

  fTiles:=TTilePosList64.Create;
  fPhotometric:=1;
end;

destructor TTiffView.Destroy;
begin
  fFile:=nil;

  FreeJpegTables(fjpg_props);

  ftile.bufp:=xFreePtr(ftile.bufp);
  fDataBuf:=xFreePtr(fDataBuf);

  FreeAndNil(fTiles);

  if fecw <> 0 then
  NCScbmCloseFileViewEx(fecw,false);

  FreeAndNil(fsidBuf1);
  FreeAndNil(fsidBuf);
  FreeAndNil(fsid);

  fjpg:=nil;

  inherited
end;

function TTiffView.Get_pbuf_size: TSize;
begin
  Result.cx:=0; Result.cy:=0;
end;

function TTiffView.fill_pbuf(ip,jp: Integer): Boolean;
begin
  Result:=false
end;

function TTiffView.pbuf_tile(i,j: Integer; im: PBytes): Boolean;
begin
  Result:=false
end;

function TTiffView.Next_zoom(var zoom: Integer): Boolean;
begin
  zoom:=zoom * 2; with Info do
  Result:=((biWidth div zoom) > tile.Width div 2) and
          ((biHeight div zoom) > tile.Height div 2)
end;

procedure TTiffView.Next_layer(var iw,ih: int; iz: int);
begin
end;

procedure TTiffView.SetData(const lev: TTiff_level);
begin
  fData:=lev;

  fInfo.biWidth:=fData.Width;
  fInfo.biHeight:=fData.Height;

  ftile.Width:=fData.tileWidth;
  ftile.Height:=fData.tileHeight;
  ftile.XTiles:=fData.XTiles;
  ftile.YTiles:=fData.YTiles;

  with fData do begin
    NTiles:=XTiles * YTiles * Max(1,NPlanes);
    Load_tiles(off_pos,off_typ,
               len_pos,len_typ,
               NTiles);
  end;

  ftile.NTiles:=fData.NTiles;

  fDataTop:=fData.Height;

  with fData do begin
    tileSize:=tileLine * tileHeight;
    tiffLine:=tif_Line(tileWidth,im_bits)
  end;

  ftile.BufSize:=fData.tileSize;
  ftile.BufLine:=fData.tileLine;

  fjpg_size.X:=fData.Width;
  fjpg_size.Y:=fData.Height;
  fjpg_zoom:=1
end;

function TTiffView.Get_Path: PChar;
begin
  Result:=fPath
end;

function TTiffView.Get_Active: Boolean;
begin
  Result:=Assigned(fFile);
  if not Result then

  if Assigned(fsid) then
    Result:=fim_typ = typ_sid
  else
  if fecw <> 0 then
    Result:=fim_typ = typ_ecw
end;

function TTiffView.jpg_enabled: bool;
begin
  if fjpg = nil then begin
    GetJpgInterface(IJPeg,fjpg);
    if Assigned(fjpg) then
    AllocJpegTables(fjpg,fjpg_props)
  end;

  Result:=Assigned(fjpg)
end;

function TTiffView.GetColor(Ind: Integer): Integer;
begin
  Result:=Quad_Color(fQuads[Ind])
end;

function TTiffView.Get_is_tiled: Boolean;
begin
  Result:=fData.Typ = 1
end;

function TTiffView.Get_is_lines: Boolean;
begin
  Result:=fData.Typ = 0
end;

function TTiffView.Get_is_striped: Boolean;
begin
  Result:=(fData.off_pos > 0) and
          (fData.len_pos > 0) and
          (fData.tileHeight > 1)
end;

function TTiffView.GetDataBuf(len: Integer): Pointer;
begin
  Result:=nil;

  if len = 0 then len:=fData.tileSize;

  if len > 0 then begin

    if Assigned(fDataBuf) then
    if fDataSize < len then
    fDataBuf:=xFreePtr(fDataBuf);

    if fDataBuf = nil then begin
      fDataBuf:=xAllocPtr(len);
      fDataSize:=len
    end
  end;

  Result:=fDataBuf
end;

function TTiffView.Get_tile_buf: Pointer;
begin
  with ftile do
  if bufp = nil then
  if BufSize > 0 then
  bufp:=xAllocPtr(BufSize);
  Result:=ftile.bufp
end;

procedure TTiffView.Free_tile_buf;
begin
  with ftile do bufp:=xFreePtr(bufp);
end;

function TTiffView.GetBitmap(out map: Bitmap): bool;
var
  bx: int; cx: int64;
begin
  Result:=false;
  Fillchar(map,Sizeof(map),0);

  if fData.bit_pos > 0 then
  if fData.bit_len > 0 then
  if fInfo.biCompression = bi_RGB then begin

    with fInfo do begin
      bx:=tif_Line(biWidth,biBitCount);
      cx:=bx; cx:=cx*biHeight
    end;

    if cx = fData.bit_len then begin
      map.bmWidth:=fInfo.biWidth;
      map.bmHeight:=fInfo.biHeight;
      map.bmWidthBytes:=bx;
      map.bmPlanes:=1;
      map.bmBitsPixel:=fInfo.biBitCount;
      map.bmBits:=fFile.Seek(fData.bit_pos,fData.bit_len);
      Result:=Assigned(map.bmBits)
    end
  end
end;

procedure TTiffView.jpg_to_tiled(tw,th: Integer);
begin
  if fim_typ =typ_jpg then
  if (tw >= 256) and (tw <= fInfo.biWidth) then
  if (th >= 256) and (th <= fInfo.biHeight) then begin
    Level_tiled1(fData,tw,th,im_bits);
    fData.Typ:=1; SetData(fData);
  end
end;

function TTiffView.lines_to_tiled: Boolean;

function unpack_to_tiled: Boolean;
var
  y,h,len,pos: Integer;
  sz: Int64; lev: TTiff_level;
begin
  Result:=false;

  lev:=fData; lev.tileHeight:=256;

  lev.tileSize:=lev.tileLine * lev.tileHeight;

  pos:=fData.bit_pos;
  sz:=fFile.GetSize;

  lev.XTiles:=1;
  lev.YTiles:=0; y:=0;
  while y < info.biHeight do begin
    h:=lev.tileHeight;
    if y + h > info.biHeight then
    h:=info.biHeight - y;

    len:=h * lev.tileLine;
    if pos + len > sz then begin
      lev.YTiles:=0; Break
    end;

    Inc(lev.YTiles); Inc(y,h)
  end;

  lev.NTiles:=lev.XTiles * lev.YTiles;

  if lev.NTiles > 0 then begin
    lev.Typ:=1; lev.bit_len:=0;
    SetData(lev); Result:=true
  end
end;

function AvailAllocImage: Boolean;
var
  sz: Integer; p: Pointer;
begin
  Result:=false;

  with fData do begin

    sz:=bit_len div 1024 div 1024;
    if fWaitRGB and (im_bits < 24) then
    sz:=sz*3;

    if GetAvailMemorySize > 3*sz then begin
      p:=fFile.Seek(bit_pos,bit_len);
      Result:=Assigned(p)
    end
  end
end;

begin
  if fData.Typ = 1 then begin

    if info.biCompression = bi_RGB then

    with fData do
    if bit_pos > 0 then
    if bit_len > 0 then

    if not AvailAllocImage then
    unpack_to_tiled

  end
  else begin
    if fData.off_pos > 0 then
    if fData.len_pos > 0 then
    if fData.tileHeight > 1 then
    fData.Typ:=1;

    if fData.Typ = 0 then

    with fData do
    if bit_pos = 0 then begin

      if fim_typ = typ_jpg then begin
        Level_tiled(fData,im_bits);
        fData.Typ:=1
      end

    end else
    if bit_pos > 0 then

    if bit_len = 0 then begin

      if info.biCompression = bi_RGB then
      unpack_to_tiled;

    end else

    if AvailAllocImage then fData.Typ:=1
    else                    unpack_to_tiled;

    if Is_Tiled then SetData(fData)
  end;

  Result:=Is_Tiled
end;

function TTiffView.Open(APath: PChar): Boolean;

procedure Set_image(typ,pos,len: Integer);
begin
  fData:=Image_level(fInfo, typ,pos,len);
  SetData(fData)
end;

function Is_image(typ,pos,len: Integer): Boolean;
begin
  Result:=false;

  if fFile.IsData(pos,len) then begin
    Set_image(typ,pos,len);
    Result:=true
  end
end;

function This_bmp: Boolean;
var
  hdr: TBitMapFileHeader;
  bits,cols,pos: Integer;
begin
  Result:=false; fis_bmp:=true;

  if fFile.GetSize > Sizeof(hdr)+Sizeof(fInfo) then begin

    fFile.Load(0,hdr,SizeOf(hdr));
    fFile.Load(SizeOf(hdr),fInfo,SizeOf(fInfo));

    bits:=img_Bits(@fInfo);

    if fInfo.biSize > Sizeof(fInfo) then begin
      pos:=SizeOf(hdr) + Sizeof(fInfo);
      cols:=(fInfo.biSize - Sizeof(fInfo)) div SizeOf(TRGBQuad);
    end
    else begin
      pos:=SizeOf(hdr) + fInfo.biSize;
      cols:=(hdr.bfOffBits - pos) div SizeOf(TRGBQuad);
    end;

    with fInfo do
    if biSizeImage > 0 then
      biSize:=biSizeImage div biHeight
    else begin
      biSize:=(biWidth*bits+7) div 8;
      biSize:=int_Round(biSize,4);
      biSizeImage:=biSize * biHeight
    end;

    fInfo.biClrUsed:=Max(0,Min(256,cols));

    cols:=0;
    FillChar(fQuads,SizeOf(fQuads),0);

    if bits = 1 then cols:=2 else
    if bits = 2 then cols:=4 else
    if bits = 4 then cols:=16 else
    if bits = 8 then cols:=256;

    if cols > 0 then begin
      cols:=xGray_Quads(@Quads,cols);

      if cols >= 16 then with fInfo do
      if biClrUsed > 0 then
      if biClrUsed <= cols then

      fFile.Load(pos,fQuads,biClrUsed * SizeOf(TRGBQuad))
    end;

    Result:=Is_image(0,hdr.bfOffBits,fInfo.biSizeImage)
  end
end;

function Load_Quads: Integer;
var
  i,bits,used,cl: Integer;
  tmp: array[0..256*3] of word;
  isShift: Boolean;
begin
  Result:=0; used:=0;

  bits:=img_bits(@info);

  if bits = 1 then used:=2 else
  if bits = 2 then used:=4 else
  if bits = 4 then used:=16 else
  if bits = 8 then used:=256;

  if rgb_pos > 0 then
  if fFile.IsData(rgb_pos,rgb_len) then begin

    Result:=Min(used,rgb_len div 6);

    if Result > 0 then begin
      used:=Result;
      fFile.Load(rgb_pos,tmp,used*6);

      isShift:=false; for i:=0 to used*3-1 do
      if tmp[i] > 255 then begin isShift:=true; Break end;

      if isShift then for i:=0 to used*3-1 do
      tmp[i]:=tmp[i] shr 8;

      for i:=0 to used-1 do with fQuads[i] do begin
        rgbRed:=tmp[i]; rgbGreen:=tmp[used+i];
        rgbBlue:=tmp[used+used+i]
      end
    end
  end;

  if used > 0 then
  if Result = 0 then begin
    xGray_Quads(@fQuads,used);
    if used = 2 then

    if fPhotometric = 1 then
      fQuads[1]:=Color_Quad($FFFFFF)
    else begin
      fQuads[1]:=fQuads[0];
      fQuads[0]:=Color_Quad($FFFFFF)
    end;

    if used <= 4 then
    if fIsChan then
    for i:=0 to used-1 do begin
      cl:=EGA_Colors[fChan[i]];
      fQuads[i]:=Color_Quad(cl)
    end
  end
end;

function Read_int(pos: int64; typ: int): int;
begin
  Result:=0;
  if typ = 3 then begin
    Result:=fFile.Get_word(pos);
    if fis_mac then Result:=SwapWord(Result);
  end
end;

procedure dir_rational(var dir: tiff_dir64);
var
  c: TRational;
begin
  with dir do
  if typ = 5 then begin
    fFile.Load(ind,c,8);

    if fis_mac then begin
      c.c1:=SwapInt(c.c1);
      c.c2:=SwapInt(c.c2);
    end;

    if c.c2 <> 0 then
      ind:=c.c1 div c.c2
    else
      ind:=0
  end;
end;

type       
  TTiff_dcb = record
    data_w: Integer;
    data_h: Integer;

    data_rows: Integer;
    data_ofs: DWord;
    data_len: DWord;
    bit_pos: DWord;
    bit_len: DWord;

    tile_ofs: int64;
    ofs_typ: int;
    tile_len: int64;
    len_typ: int;

    tile_w: Integer;
    tile_h: Integer;

    NPlanes: int;
    PlanarConfig: int;
  end;

function IsLevel(const Data: TTiff_dcb;
                 out lev: TTiff_level): Boolean;
begin
  Result:=false;

  Fillchar(lev,Sizeof(lev),0);
  lev.Width:=Data.data_w;
  lev.Height:=Data.data_h;
  lev.XTiles:=1; lev.YTiles:=1;
  lev.NTiles:=1; lev.NPlanes:=1;

  with Data do
  if (data_w > 0) and (data_h > 0) then

  if (bit_pos > 0) and (bit_len > 0) then begin
    lev.bit_pos:=bit_pos;
    lev.bit_len:=bit_len;
    lev.tileWidth:=data_w;
    lev.tileHeight:=data_h;
    Result:=true
  end else

  if (data_ofs > 0) and (data_len > 0) then begin
    if data_rows > 0 then
    if data_rows <= data_h then begin

      lev.tileWidth:=data_w;
      lev.tileHeight:=data_rows;

      lev.YTiles:=int_Tiles(data_h,data_rows);
      lev.NTiles:=lev.YTiles;

      lev.off_pos:=data_ofs; lev.off_typ:=ofs_typ;
      lev.len_pos:=data_len; lev.len_typ:=len_typ;

      if PlanarConfig = 2 then
      lev.NPlanes:=Max(1,NPlanes);

      Result:=Verify_tiles1(data_ofs,ofs_typ,
                            data_len,len_typ,
                            lev.NTiles*lev.NPlanes)
    end
  end else

  if (tile_ofs > 0) and (tile_len > 0) then begin

    if (tile_w > 0) and (tile_h > 0) then begin

      lev.Typ:=1;
      lev.tileWidth:=tile_w;
      lev.tileHeight:=tile_h;

      lev.XTiles:=int_Tiles(data_w,tile_w);
      lev.YTiles:=int_Tiles(data_h,tile_h);
      lev.NTiles:=lev.YTiles;

      lev.off_pos:=tile_ofs; lev.off_typ:=ofs_typ;
      lev.len_pos:=tile_len; lev.len_typ:=len_typ;

      Result:=Verify_tiles1(tile_ofs,ofs_typ,
                            tile_len,len_typ,
                            lev.NTiles)
    end
  end;

  if Result then begin
    lev.tileLine:=tif_Line(lev.tileWidth,im_bits);
    lev.tileSize:=lev.tileLine * lev.tileHeight
  end
end;

procedure Add_level(var lev: TTiff_level);
begin
  lev.Lev:=fInfo.biWidth div lev.Width;
  flevels[fLevelCount]:=lev;
  Inc(fLevelCount);
end;

procedure Add_levels(top,ind: int64);
var
  dir: TTiffDirList;
  i,cx: int; pos: int64;
  dp: ptiff_dir_arr64;
  dr: tiff_dir64;
  dcb: TTiff_dcb;
  lev: TTiff_level;
  sz: Int64;
begin
  dir:=TTiffDirList.Create(fIs_Big,fIs_Mac);
  try
    sz:=fFile.GetSize;
    while ind > 0 do begin pos:=ind;

      if fLevelCount = 0 then Break;
      lev:=fLevels[ fLevelCount-1 ];

      if fLevelCount = Levels_Max then Break;
      if not dir.Load(fFile,pos) then Break;

      Fillchar(dcb,Sizeof(dcb),0);
      dcb.data_rows:=1;

      dcb.PlanarConfig:=fPlanarConfig;
      dcb.NPlanes:=fNPlanes;

      dp:=dir.First;
      for i:=0 to dir.Count-1 do begin

        dr:=dp[i]; dir_rational(dr);

        case dr.tag of
      256:dcb.data_w:=dr.ind;      {ImageWidth}
      257:dcb.data_h:=dr.ind;      {ImageHeigth}

      273:                         {StripOffset}
          if dr.len <= 1 then
            dcb.bit_pos:=dr.ind
          else begin
            dcb.data_ofs:=dr.ind;
            dcb.ofs_typ:=dr.typ
          end;

      277:dcb.NPlanes:=dr.ind;     {SamplesPerPixel}

      278:dcb.data_rows:=dr.ind;   {RowsPerStrips}

      279:                         {StripByteCount}
          if dr.len <= 1 then
            dcb.bit_len:=dr.ind
          else begin
            dcb.data_len:=dr.ind;
            dcb.len_typ:=dr.typ
          end;

      284:dcb.PlanarConfig:=dr.ind;

      322:dcb.tile_w:=dr.ind;      {TileWidth}
      323:dcb.tile_h:=dr.ind;      {TileLength}

      324:begin                    {TileOffset}
            dcb.tile_ofs:=dr.ind;
            dcb.ofs_typ:=dr.typ
          end;

      325:begin                    {TileByteCount}
            dcb.tile_len:=dr.ind;
            dcb.len_typ:=dr.typ
          end
        end;
      end;

      if (dcb.data_w/lev.Width > 0.75)
      or (dcb.data_h/lev.Height > 0.75) then
        Break
      else
      if not IsLevel(dcb,lev) then
        Break
      else begin
        Add_level(lev);

        cx:=4;
        if fis_big then cx:=8;

        if pos+cx < sz then begin
          fFile.Load(pos,ind,cx);
          if ind < top then ind:=0
        end
      end
    end;
  finally
    dir.Free
  end
end;

function Load_jpg_pyramid: Integer;
type
  TJpegMarker = record
    id,len: Word
  end;

  TJpegFatItem = record
    ofs,len,width,height: DWord
  end;
var
  si: PBytes; i,n,pos,pos1,ok: Integer;
  mk: TJpegMarker; fr: TJpegFatItem;
  tmp: TBitmapInfoHeader; lev: TTiff_level;
  sz: Int64;
begin
  sz:=fFile.GetSize;
  if sz < MaxLong then begin

    pos:=sz-8;
    if pos > 0 then begin
      si:=fFile.Seek(pos,8);
      if Assigned(si) then begin

        Move(si^,mk,Sizeof(mk));
        mk.id:=SwapWord(mk.id);
        mk.len:=SwapWord(mk.len);

        if mk.id = $FF02 then
        if mk.len = 6 then begin

          Move(si[4],pos1,4);
          if pos1 > 0 then
          if pos1 <= pos-8 then begin
            si:=fFile.Seek(pos1,pos-pos1);
            if Assigned(si) then begin
              Move(si^,mk,Sizeof(mk));
              mk.id:=SwapWord(mk.id);
              mk.len:=SwapWord(mk.len);
              if mk.id = $FF01 then
              if pos1 + mk.len + 2 = pos then begin

                si:=@si[4];
                n:=mk.len div Sizeof(fr);

                for i:=0 to n-1 do begin

                  Move(si^,fr,Sizeof(fr));
                  si:=@si[Sizeof(fr)]; ok:=0;

                  if fFile.IsData(fr.ofs,fr.len) then

                  if fr.width <= fInfo.biWidth then
                  if fr.height <= fInfo.biHeight then begin
                    tmp:=fInfo;
                    tmp.biWidth:=fr.width;
                    tmp.biHeight:=fr.height;
                    tmp.biSize:=img_Line(fr.width,fjpg_bits);

                    lev:=Image_level(tmp, 0,fr.ofs,fr.len);
                    Add_level(lev); ok:=1;

                    if fLevelCount = 1 then SetData(lev);
                  end;

                  if ok = 0 then Break
                end
              end
            end
          end
        end
      end
    end
  end;

  Result:=fLevelCount
end;

function Load_jpg_props: Boolean;
var
  si: Pointer;
begin
  Result:=false;

  if fjpg_props.isValid then
    Result:=true
  else
  if fjpg_props_pos > 0 then
  if fjpg_props_len > 0 then

  if jpg_enabled then begin

    si:=fFile.Seek(fjpg_props_pos,fjpg_props_len);

    if Assigned(si) then
    Result:=fjpg.LoadTables(fjpg_props,si,fjpg_props_len)
  end
end;

function stripes_to_normal(var dcb: TTiff_dcb): bool;
var
  ofs,len: PInteger; sz: int64;
  i,n: int; bx,cx,dx,line,top: uint;
begin
  Result:=false;

  line:=tif_Line(dcb.data_w,fInfo.biBitCount);
  sz:=line; sz:=sz * dcb.data_h;

  if sz < 100000000 then begin

    n:=dcb.data_h; cx:=n*4;

    ofs:=fFile.Seek(dcb.data_ofs,cx);
    if Assigned(ofs) then begin

      Result:=true; top:=ofs^;

      bx:=0;
      for i:=1 to n do begin
        dx:=ofs^; Inc(ofs);
        if dx <= bx then begin
          Result:=false; Break
        end
      end;

      if Result then begin
        Result:=false;
        len:=fFile.Seek(dcb.data_len,cx);
        if Assigned(len) then begin

          Result:=true;
          for i:=1 to n do begin
            if len^ <> line then begin
              Result:=false; Break
            end; Inc(len)
          end
        end
      end
    end;

    if Result then begin
      dcb.bit_pos:=top;
      dcb.bit_len:=sz;
    end
  end
end;

var
  dir: TTiffDirList; si: PBytes;
  i,cx,mag,bits: int; pos,top: int64;
  dp: ptiff_dir_arr64; dr: tiff_dir64;
  hdr: tiff_hdr; dcb: TTiff_dcb;
  sz: Int64; jpg: Boolean;
  ext: TShortstr;

begin
  Result:=false; Close;

  rgb_pos:=0; rgb_len:=0;
  fjpg_props_pos:=0;

  Fillchar(finfo,Sizeof(finfo),0);
  Fillchar(fdata,Sizeof(fdata),0);

  Free_tile_buf;
  Fillchar(ftile,Sizeof(ftile),0);

  Fillchar(flevels,Sizeof(flevels),0);
  fLevelCount:=0; fTiles.Clear;

  fQuality:=95; fjpg_props_len:=0;

  fjpg_props.IsValid:=false;

  fis_cmyk:=false;
  fIs_Jpg:=false;
  fIs_Tif:=true;
  fis_hdif:=false;
  fis_x16:=false;
  fis_x32:=false;

  fis_float:=false;
  fSMinSampleValue:=1;
  fSMaxSampleValue:=0;

  fIs_swap:=false;
  fis_bmp:=false;
  fim_typ:=typ_tif;

  fPhotometric:=1;

  fPlanarConfig:=1;
  fNPlanes:=1;

  fClearColor:=0;
  fMaxValue:=0;

  fIsChan:=false;

  fCrypt:=0;

  fjpg_zoom:=1; fjpg_bits:=24;

  StrLCopy(fPath,APath,Sizeof(fPath)-1);
  StrPCopy(ext,ExtractFileExt(StrPas(APath)));

  fFileSize:=winFileSize(fPath);

  if fis_upd then begin
    if StrIComp(ext,'.TIF') = 0 then
    Result:=GetUpdateFileIntf(APath,fFile)
  end else

  if StrIComp(ext,'.SID') = 0 then begin

    if fsid.Open(APath) then begin

      fInfo.biWidth:=fsid.Width;
      fInfo.biHeight:=fsid.Height;
      fInfo.biPlanes:=1;
      fInfo.biBitCount:=fsid.OutBits;
      fInfo.biCompression:=bi_SID;

      fjpg_bits:=fsid.OutBits;

      if fInfo.biBitCount = 8 then
      xGray_Quads(@Quads,256);

      fim_typ:=typ_sid;
      Set_image(0,0,0);

      Result:=true
    end

  end else

  if StrIComp(ext,'.ECW') = 0 then begin

    if ecw_Init then
    if NCScbmOpenFileView(APath,fecw,nil) = 0 then
    if ecwBitmapInfo(fecw,@fInfo) then begin

      fInfo.biCompression:=bi_ECW;

      fim_typ:=typ_ecw;
      Set_image(0,0,0);

      Result:=true
    end

  end else

  if GetOpenFileIntf(APath,fFile) then begin

    sz:=fFile.GetSize;
    mag:=fFile.Get_word(0);
    jpg:=StrIComp(ext,'.JPG') = 0;

    if jpg then
    jpg:=(mag <> $4949) and (mag <> $4D4D);

    if jpg then begin

      fIs_Tif:=false;

      si:=fFile.Seek(0,-1);
      if sz >= MaxLong then si:=nil;

      if si = nil then begin
        if jpg_Get_info(fPath,@fInfo) then
        with fInfo do begin

          fInfo.biCompression:=bi_JPG;
          fjpg_bits:=img_bits(@fInfo);

          biSize:=img_Line(biWidth,fjpg_bits);
          fis_swap:=fjpg_bits = 24;

          if fjpg_bits = 8 then begin
            fInfo.biClrUsed:=256;
            xGray_Quads(@fQuads,256);
          end;

          fim_typ:=typ_jpg;
          Set_image(0,0,0);
          Level_tiled(fData,im_bits);
          fData.Typ:=1; SetData(fData);
          Result:=true
        end

      end else

      if ijl_Get_info(si,sz,@fInfo) then
      with fInfo do begin

        fInfo.biCompression:=bi_JPG;
        fjpg_bits:=img_bits(@fInfo);

        biSize:=img_Line(biWidth,fjpg_bits);
        fis_swap:=fjpg_bits = 24;

        if fjpg_bits = 8 then begin
          fInfo.biClrUsed:=256;
          xGray_Quads(@fQuads,256);
        end;

        if Load_jpg_pyramid > 0 then
          fim_typ:=typ_jpg
        else
        if (biWidth <= 4096)
        and (biHeight <= 4096) then
          Set_image(1,0,sz)
        else begin
          fim_typ:=typ_jpg;
          Set_image(0,0,sz)
        end;

        Result:=true; 
      end;

      if im_bits = 24 then fClearColor:=clWhite

    end else

    if StrIComp(ext,'.BMP') = 0 then begin
      fIs_tif:=false; Result:=This_bmp
    end else

    if sz > SizeOf(hdr) then begin
      fFile.Load(0,hdr,8);

      Fillchar(dcb,Sizeof(dcb),0);
      dcb.data_rows:=1;

      fis_mac:=false;
      if hdr.cod = $4D4D then begin
        hdr.cod:=$4949; fis_mac:=true;
        hdr.ver:=SwapWord(hdr.ver);
        hdr.ind:=SwapInt(hdr.ind)
      end;

      fis_big:=false;
      pos:=hdr.ind; top:=8;

      if hdr.ver = $2B then
      if hdr.ind <> 8 then
        hdr.cod:=0
      else begin
        fFile.Load(8,pos,8);
        if fis_mac then pos:=SwapInt64(pos);
        fis_big:=true; top:=16;
      end;

      if hdr.cod = $4949 then
      if pos > 0 then begin

        finfo.biPlanes:=1;
        finfo.biBitCount:=1;

        dir:=TTiffDirList.Create(fIs_Big,fIs_Mac);
        try
          if dir.Load(fFile,pos) then begin

            bits:=0;

            dp:=dir.First;
            for i:=0 to dir.Count-1 do begin
              dr:=dp[i]; dir_rational(dr);

              with info do case dr.tag of
            256:begin                     {ImageWidth}
                  biWidth:=dr.ind;
                  dcb.data_w:=dr.ind
                end;

            257:begin                     {ImageHeigth}
                  biHeight:=dr.ind;
                  dcb.data_h:=dr.ind
                end;

            258:if dr.len = 1 then begin  {BitsPerSample}
                  biBitCount:=dr.ind;
                  bits:=dr.ind
                end
                else begin
                  if fis_big and
                    (dr.typ = 3) and
                    (dr.len = 3) then
                    biBitCount:=TINt64(dr.ind).w[0]
                  else
                    biBitCount:=Read_int(dr.ind,dr.typ);
                  bits:=biBitCount * 3
                end;

            259:if dr.ind = 1 then        {Compression}
                  biCompression:=bi_RGB
                else
                if dr.ind = 3 then begin
                  if Hoffman.Count > 0 then
                    biCompression:=bi_GR3
                  else biCompression:=bi_ERR
                end else
                if dr.ind = 4 then begin
                  if Hoffman.Count > 0 then
                    biCompression:=bi_GR4
                  else biCompression:=bi_ERR
                end else
                if dr.ind = 5        then biCompression:=bi_LZW else
                if dr.ind = 7        then biCompression:=bi_JPG else
                if dr.ind = 8        then biCompression:=bi_ZIP else
                if dr.ind = 32773    then biCompression:=bi_RLC else
                if dr.ind = 32946    then biCompression:=bi_ZIP else
                if dr.ind = Comp_Jpf then biCompression:=bi_JPG else
                if dr.ind = comp_zlb then biCompression:=bi_ZIP else
                                       biCompression:=bi_ERR;

            262:begin
                  fPhotometric:=dr.ind;
                  fis_cmyk:=dr.ind = 5;
                end;

            273:                          {StripOffset}
                if dr.len <= 1 then
                  dcb.bit_pos:=dr.ind
                else begin
                  dcb.data_ofs:=dr.ind;
                  dcb.ofs_typ:=dr.typ
                end;

            277:begin                     {SamplesPerPixel}
                  biPlanes:=dr.ind;
                  dcb.NPlanes:=dr.ind;
                  fNPlanes:=dr.ind
                end;

            278:dcb.data_rows:=dr.ind;    {RowsPerStrips}

            279:                          {StripByteCount}
                if dr.len <= 1 then
                  dcb.bit_len:=dr.ind
                else begin
                  dcb.data_len:=dr.ind;
                  dcb.len_typ:=dr.typ
                end;

            280:                          {MinSampleValue}
                if dr.len in [1,2] then
                  fMinValue:=dr.ind
                else
                  fMinValue:=Read_int(dr.ind,dr.typ);

            281:                          {MaxSampleValue}
                if dr.len in [1,2] then
                  fMaxValue:=dr.ind
                else
                  fMaxValue:=Read_int(dr.ind,dr.typ);

            282:biXPelsPerMeter:=dr.ind;
            283:biYPelsPerMeter:=dr.ind;

            284:begin
                  dcb.PlanarConfig:=dr.ind;
                  fPlanarConfig:=dr.ind;
                end;

            317:fis_hdif:=dr.ind = 2;

            320:begin
                  rgb_pos:=dr.ind; rgb_len:=dr.len;
                  if dr.typ = 3 then rgb_len:=dr.len*2;
                end;

            322:dcb.tile_w:=dr.ind;       {TileWidth}
            323:dcb.tile_h:=dr.ind;       {TileLength}

            324:begin                     {TileOffset}
                  dcb.tile_ofs:=dr.ind;
                  dcb.ofs_typ:=dr.typ;
                  fis_swap:=dr.typ in [4,16];
                end;

            325:begin                     {TileByteCount}
                  dcb.tile_len:=dr.ind;
                  dcb.len_typ:=dr.typ
                end;

            339:if dr.typ = 3 then
                if dr.len = 1 then begin
                  fSampleFormat:=dr.ind;

                  if fSampleFormat = 2 then
                  if biBitCount = 16 then
                  if fMinValue and $8000 <> 0 then
                  fMinValue:=fMinValue or $FFFF0000;

                  fis_float:=dr.ind = 3;
                end;

            340:if dr.typ = 11 then
                if dr.len = 1 then
                fSMinSampleValue:=pfloat(@dr.ind)^;

            341:if dr.typ = 11 then
                if dr.len = 1 then
                fSMaxSampleValue:=pfloat(@dr.ind)^;

            347:if dr.len > 0 then
                if fFile.IsData(dr.ind,dr.len) then begin
                  fjpg_props_pos:=dr.ind;
                  fjpg_props_len:=dr.len;
                end;

            900:if dr.typ = 4 then
                if dr.len = 1 then
                fClearColor:=dr.ind;

            901:if dr.typ = 1 then
                if dr.len = 16 then
                if dr.ind > 0 then begin
                  fFile.Load(dr.ind,fChan,dr.len);
                  fIsChan:=true
                end;

            $A0A0:
                if dr.typ = 1 then
                if dr.len = 1 then
                fQuality:=dr.ind;

            999:if dr.typ = 4 then
                if dr.len = 1 then
                if dr.ind = 1 then
                fCrypt:=cid_key;
              end;
            end;

            if finfo.biPlanes = 1 then
            if finfo.biBitCount = 8 then
            if fPhotometric = 2 then
            if bits = 24 then
            finfo.biBitCount:=24;

            with dcb do
            if fInfo.biCompression = bi_RGB then
            if data_rows = 1 then
            if bit_pos = 0 then
            if data_ofs > 0 then
            if data_len > 0 then

            if fOnlyData then
            stripes_to_normal(dcb);

            with dcb do
            if (bit_pos > 0) and (bit_len = 0) then
            bit_len:=tif_line(data_w,bits) * data_h;

            if IsLevel(dcb,fData) then begin

              cx:=4;
              if fis_big then cx:=8;

              if pos+cx < sz then begin
                fFile.Load(pos,pos,cx);

                if pos > top then begin
                  Add_level(fData);
                  Add_levels(top,pos)
                end
              end;

              SetData(fData);

              if fis_swap then
              fis_swap:=im_bits = 24;

              with info do begin
                biBitCount:=biBitCount*biPlanes;
                biPlanes:=1;

                if not (biBitCount in [1,2,4,8,16,24,32,48,64]) then
                  biSize:=0
                else begin
                  if fis_cmyk then
                  fis_cmyk:=biBitCount = 32;

                  if biBitCount = 16 then begin
                    if not fOnlyData then begin
                      fis_x16:=true; biBitCount:=8
                    end
                  end else
                  if biBitCount = 32 then begin

                    if fis_float then begin
                      if not fOnlyData then
                      biBitCount:=8
                    end
                    else begin
                      fis_x32:=true; biBitCount:=24
                    end
                  end else
                  if biBitCount = 48 then begin
                    fis_x16:=true; biBitCount:=24
                  end else
                  if biBitCount = 64 then begin
                    fis_x16:=true; biBitCount:=24
                  end;

                  biSize:=(biWidth * biBitCount + 7) div 8;

                  fIs_Jpg:=biCompression = bi_JPG;

                  if fis_x16 then
                  if biCompression <> bi_RGB then biSize:=0;

                  if biCompression = bi_ERR then biSize:=0
                end
              end;

              with info do
              if biCompression in [bi_GR3,bi_GR4] then begin
                if biBitCount <> 1 then biSize:=0;
              end;

              if info.biSize > 0 then begin

                if fData.Typ = 0 then
                fis_swap:=img_bits(@fInfo) = 24;

                if fWaitTiled then
                with fData do if Typ = 0 then
                if (bit_pos > 0) and (bit_len > 0) then
                Typ:=1;

                finfo.biClrUsed:=Load_Quads;

                if info.biCompression = bi_JPG then
                Load_jpg_props;

                Result:=true
              end
            end
          end;
        finally
          dir.Free
        end
      end
    end
  end;

  if not Result then Close
end;

procedure TTiffView.Close;
begin
  ftile.bufp:=xFreePtr(ftile.bufp);
  fDataBuf:=xFreePtr(fDataBuf);

  if Assigned(fsidBuf1) then fsidBuf1.im_Close;
  if Assigned(fsidBuf) then fsidBuf.im_Close;
  if Assigned(fsid) then fsid.Close;

  fsidPos.X:=-1; fsidPos.Y:=-1;
  Fillchar(fsidClip,Sizeof(fsidClip),0);

  if fecw <> 0 then
  NCScbmCloseFileViewEx(fecw,false);
  fecw:=0;

  fFile:=nil;
end;

function TTiffView.Verify_tiles(offPos: int64; offTyp: int;
                                lenPos: int64; lenTyp: int;
                                Count: int): Boolean;
var
  cx1,cx2: int;
begin
  Result:=false;

  if Count > 0 then begin

    cx1:=4; cx2:=4;
    if fis_big then begin
      if offTyp = 16 then cx1:=8;
      if lenTyp = 16 then cx2:=8;
    end;

    Result:=fFile.IsData(offPos,Count*cx1) and
            fFile.IsData(lenPos,Count*cx2)
  end
end;

function TTiffView.Verify_tiles1(offPos: int64; offTyp: int;
                                 lenPos: int64; lenTyp: int;
                                 Count: int): Boolean;
var
  cx1,cx2: int;
begin
  Result:=false; if Count > 0 then begin

    cx1:=4; cx2:=4;
    if fis_big then begin
      if offTyp = 16 then cx1:=8;
      if lenTyp = 16 then cx2:=8;
    end
    else begin
      if offTyp = 3 then cx1:=2;
      if lenTyp = 3 then cx2:=2;
    end;

    if Count = 1 then
      Result:=fFile.IsData(offPos,lenPos)
    else
      Result:=fFile.IsData(offPos,Count*cx1) and
              fFile.IsData(lenPos,Count*cx2)
  end
end;

function TTiffView.Load_tiles(offPos: int64; offTyp: int;
                              lenPos: int64; lenTyp: int;
                              Count: int): Boolean;
var
  i,cx1,cx2: int; lp: PTilePosArr64;
  xp: PInt64s; ip: PDWords; wp: PWords;
begin
  Result:=false; fTiles.Clear;

  if (offPos > 0) and (lenPos > 0) then
  if Verify_tiles1(offPos,offTyp,lenPos,lenTyp,Count) then
  if fTiles.Resize(Count) then begin

    lp:=fTiles.First;

    if Count = 1 then
      lp[0]:=TilePos(offPos,lenPos)
    else begin

      for i:=0 to Count-1 do begin
        lp[i].pos:=0; lp[i].len:=0
      end;

      cx1:=4; cx2:=4;
      if fis_big then begin
        if offTyp = 16 then cx1:=8;
        if lenTyp = 16 then cx2:=8;
      end
      else begin
        if offTyp = 3 then cx1:=2;
        if lenTyp = 3 then cx2:=2;
      end;

      xp:=fFile.Seek(offPos,Count*cx1);
      if Assigned(xp) then begin

        ip:=Pointer(xp); wp:=Pointer(xp);

        if cx1 = 8 then begin
          for i:=0 to Count-1 do
          if fis_mac then lp[i].pos:=SwapInt64(xp[i])
                     else lp[i].pos:=xp[i]
        end else
        if cx1 = 2 then begin
          for i:=0 to Count-1 do
          if fis_mac then lp[i].pos:=SwapWord(wp[i])
                     else lp[i].pos:=wp[i]

        end
        else begin
          for i:=0 to Count-1 do
          if fis_mac then lp[i].pos:=SwapInt(ip[i])
                     else lp[i].pos:=ip[i]
        end;

        xp:=fFile.Seek(lenPos,Count*cx2);
        if xp = nil then
          for i:=0 to Count-1 do lp[i].pos:=0
        else begin

          ip:=Pointer(xp); wp:=Pointer(xp);

          if cx2 = 8 then begin
            for i:=0 to Count-1 do
            if fis_mac then lp[i].len:=SwapInt64(xp[i])
                       else lp[i].len:=xp[i]
          end else
          if cx2 = 2 then begin
            for i:=0 to Count-1 do
            if fis_mac then lp[i].len:=SwapWord(wp[i])
                       else lp[i].len:=wp[i]
          end
          else begin
            for i:=0 to Count-1 do
            if fis_mac then lp[i].len:=SwapInt(ip[i])
                       else lp[i].len:=ip[i]
          end
        end;

        if fCrypt > 0 then
        if not fis_big then
        Decrypt_buf(Pointer(lp),Count*2,fCrypt)
      end;
    end;

    Result:=true
  end
end;

procedure TTiffView.Update_tile(Ind,Pos,Len: DWord);
var
  di: PDWords;
begin
  with fData do begin
    di:=fFile.Seek(off_pos,NTiles*4);
    if Assigned(di) then di[Ind]:=Pos;

    di:=fFile.Seek(len_pos,NTiles*4);
    if Assigned(di) then di[Ind]:=Len;
  end
end;

procedure TTiffView.Set_status(Str: PChar);
begin
  if Assigned(fOnStatus) then
  fOnStatus(Str)
end;

function TTiffView.Get_zoom: Integer;
var
  i: Integer;
begin
  Result:=1;
  for i:=1 to flevelCount-1 do
  Result:=Max(Result,flevels[i].Lev)
end;

function TTiffView.zoom_Enabled: Boolean;
begin
  Result:=false;
  if fLevelCount > 1 then
    Result:=true
  else
  if Info.biCompression = bi_JPG then
    Result:=true
end;

function TTiffView.zoom_tiles(iz: Integer): Integer;
var
  i,z: Integer;
begin
  Result:=iz;

  if fim_typ = typ_jpg then begin
    z:=iz; if z > 8 then z:=8;
    fData.Width:=fjpg_size.X div z;
    fData.Height:=fjpg_size.Y div z;
    Level_tiled(fData,im_bits);
    SetData(fData); fjpg_zoom:=z;
    Result:=iz div z
  end else

  if flevelCount > 0 then begin

    LevelIndex:=0;

    for i:=1 to flevelCount-1 do begin
      z:=flevels[i].Lev;

      if (z > 1) and (z <= iz) then begin
        z:=iz div z; if z < Result then begin
          LevelIndex:=i; Result:=z;
        end
      end
    end
  end
end;

procedure TTiffView.zoom_out(w,h: Integer);
var
  i,j: Integer; lev: ptiff_level;
begin
  j:=0;
  for i:=0 to fLevelCount-1 do begin
    lev:=@flevels[i];
    if (lev.Width < w)
    or (lev.Height < h) then Break;
    j:=i;
  end; if j > 0 then LevelIndex:=j
end;

function TTiffView.scale_out(iz: Integer): Integer;

procedure LevelInfoHeader(out lev: TBitmapInfoHeader;
                          w,h: Integer);
begin
  lev:=fInfo;
  lev.biWidth:=w;
  lev.biHeight:=h;

  if fis_tif then
    lev.biSize:=tif_Line(w,im_bits)
  else
    lev.biSize:=img_Line(w,im_bits)
end;

var
  i,j,w,h,z: Integer; lev: ptiff_level;
  tmp: TBitmapInfoHeader; tmp1: ttiff_level;
begin
  Result:=iz;

  if iz = 1 then
    LevelIndex:=0
  else
  if Info.biCompression = bi_SID then begin
    fsid.ReadMag:=1 / iz;

    with fsid.ReadSize do
    LevelInfoHeader(tmp,X,Y);

    tmp1:=Image_level(tmp,0,0,0);
    SetData(tmp1); Result:=1
  end
  else begin

    if fLevelCount > 0 then begin
      w:=Info.biWidth div iz;
      h:=Info.biHeight div iz; j:=0;

      for i:=0 to fLevelCount-1 do begin
        lev:=@flevels[i];
        if (lev.Width < w)
        or (lev.Height < h) then Break;

        iz:=Max(1,lev.Width div w);
        if this_x2(iz) then begin
          j:=i; Result:=Max(1,lev.Width div w);
        end

      end;

      if j > 0 then LevelIndex:=j;
      iz:=Result
    end;

    if (fim_typ = typ_jpg) and Is_lines then begin
      z:=iz; if z > 8 then z:=8;

      with fjpg_size do
      LevelInfoHeader(tmp,X div z,Y div z);

      with fData do
      tmp1:=Image_level(tmp,0,bit_pos,bit_len);

      SetData(tmp1); fjpg_zoom:=z;
      Result:=iz div z
    end
  end
end;

function TTiffView.scale2_out(x2: Integer): Integer;
var
  i,iz: Integer;
begin
  iz:=1;
  for i:=1 to x2 do iz:=iz * 2;
  Result:=scale_out(iz)
end;

procedure TTiffView.Set_LevelIndex(Ind: Integer);
begin
  if fLevelCount > 1 then
  if Ind < fLevelCount then
  SetData(flevels[Ind]);
end;

function TTiffView.Seek_tile(x,y: Integer;
                             out ptr: PBytes;
                             out len: DWord): Boolean;
var
  p: PTilePos64; h,bx,cx: int;
begin
  Result:=false; ptr:=nil; len:=0;

  if Assigned(fFile) then

  with fData do
  if (x >= 0) and (x < XTiles) then
  if (y >= 0) and (y < YTiles) then

  if fTiles.Count > 0 then begin
    p:=fTiles[y*XTiles + x];

    if Assigned(p) then
    if p.pos > 0 then
    if p.len > 0 then begin
      ptr:=fFile.Seek(p.pos,p.len);
      len:=p.len
    end
  end else

  if bit_len > 0 then begin
    ptr:=fFile.Seek(bit_pos,bit_len);
    len:=bit_len;
  end else
  if fim_typ <> typ_jpg then begin
    h:=fData.tileHeight; y:=y*h;
    if y+h > info.biHeight then
    h:=info.biHeight-y;

    if h > 0 then begin
      bx:=y; bx:=bx*info.biSize + bit_pos;
      cx:=h * info.biSize; ptr:=fFile.Seek(bx,cx);
      len:=cx
    end
  end;

  if Assigned(ptr) then
  Result:=len > 0
end;

function TTiffView.Get_used_Quads: PRGBQuads;
begin
  Result:=nil;
  if info.biClrUsed > 0 then
  Result:=@fQuads
end;

function TTiffView.Get_Quads_Used: Integer;
begin
  Result:=info.biClrUsed
end;

function TTiffView.Get_im_bits: Integer;
begin
  Result:=img_bits(@info)
end;

function TTiffView.Get_is_gray: Boolean;
begin
  Result:=false;
  if im_bits = 8 then

  if Info.biClrUsed = 0 then
    Result:=true
  else
    Result:=Quads_Gray(@fQuads,
                       Info.biClrUsed)
end;

function TTiffView.Get_jpeg_tile(im: PBytes): Boolean;
var
  dir: PTilePos64; i,j,len: int; pg: TPoint;
begin
  Result:=false;

  pg:=Point(0,0); len:=0;

  if fTiles.Count > 0 then

  for j:=0 to tile.YTiles-1 do
  for i:=0 to tile.XTiles-1 do begin

    dir:=fTiles[j*tile.XTiles + i];

    if dir.pos > 0 then
    if dir.len > len then begin
      pg.X:=i; pg.Y:=j; len:=dir.len
    end
  end;

  Result:=Get_tile(pg.X,pg.Y,im)
end;

function TTiffView.Get_ijl_tile(im: PBytes): Boolean;
begin
  fis_ijl:=true; with tile do
  Result:=Get_tile(XTiles div 2,YTiles div 2,im);
  fis_ijl:=false
end;

function TTiffView.tile_Exist(x,y: Integer): Boolean;
var
  si: PBytes; len: DWord;
begin
  Result:=Seek_tile(x,y,si,len)
end;

function TTiffView.Ptr_tile(x,y: Integer): PBytes;
var
  si: PBytes; len: DWord; 
begin
  Result:=nil; len:=0;
  if info.biCompression = bi_RGB then
  if Seek_tile(x,y,si,len) then
  Result:=si
end;

procedure TTiffView.swap_rgb24(im: PBytes; w,h: Integer);
var
  pad: Integer;
begin
  if fis_swap and (im_bits = 24) then begin
    pad:=0; if not Is_tif then
    pad:=pad_Line(w,24);

    if pad = 0 then swap_rgb(im,w * h)
               else x_swap_rgb(im,w,h)
  end
end;

function TTiffView.Get_tile(x,y: Integer; im: PBytes): Boolean;
var
  lzw: tlzw; si: PBytes;
  len, w,h,cx: DWord;
begin
  Result:=false;

  cx:=fData.tileSize;
  w:=fData.tileWidth;
  h:=fData.tileHeight;

  if not Seek_tile(x,y,si,len) then begin

    if fim_typ = typ_jpg then

    with fData do
    if (x >= 0) and (x < XTiles) then
    if (y >= 0) and (y < YTiles) then

    if ijl_Decompressf(im,w,h,fjpg_bits,
                       x*w,y*h,fjpg_zoom,
                       fPath) then begin
      swap_rgb24(im,w,h); Result:=true
    end

  end else

  if fim_typ = typ_jpg then begin

    if x*w < fData.Width then
    if y*h < fData.Height then

    if ijl_Decompress1(im,w,h,fjpg_bits,
                       x*w,y*h,fjpg_zoom,
                       si,len,nil) then begin
      swap_rgb24(im,w,h); Result:=true
    end

  end else

  case info.biCompression of
bi_JPG:
    if jpg_enabled then
    Result:=fjpg.Decompress(im,w,h,info.biBitCount,
                            si,len,@fjpg_props);

bi_ZIP:
    begin
      Result:=zlb.xDecompress(si,len, im,cx);
      if Result then swap_rgb24(im,w,h);
    end;

bi_LZW:
    begin
      lzw:=tlzw.Create(si,len,im,cx);
      try
        lzw.Unpack; Result:=true;
      finally
        lzw.Free
      end;

      if Result then
      if fis_x32 then
      pf32_to_pf24(im,im,w*h);

      if fis_hdif then
      bmp_diff(im,w,h,im_bits,0);

      if im_bits = 24 then
      swap_rgb24(im,w,h)
    end;

bi_RLC:
    begin
      rlc_Unpack(si,len,im,cx);
      Result:=true
    end;

bi_GR3:
    begin Result:=true;
      Hoffman.Unpack_gr3(si,len,im,
                         fData.tileHeight,
                         fData.tileWidth,
                         fData.tileLine)
    end;

bi_GR4:
    begin Result:=true;
      Hoffman.Unpack_gr4(si,len,im,
                         fData.tileHeight,
                         fData.tileWidth,
                         fData.tileLine);
    end;

bi_RGB:
    if len = cx then begin

      if fIs_bmp then
        load_bmp(im,si,
                 fData.tiffLine,
                 fData.tileLine,
                 fData.tileHeight)
      else
      if fis_x32 then
        pf32_to_pf24(si,im,w*h)
      else
        Move(si^,im^,cx);

      if fis_swap <> fis_bmp then
      swap_rgb24(im,w,h);

      Result:=true
    end;
  end;

  if not Result then Fillchar(im^,cx,0)
end;

function TTiffView.Zoom_tile(x,y: Integer; im: PBytes): Boolean;
begin
  Result:=false
end;

function TTiffView.Pack_tile(x,y,zoom: Integer; im: PBytes): Boolean;
var
  si,buf: PBytes; len,size: DWord;
  iz,tw,th,w,h,lev,bits: Integer;
begin
  if zoom = 1 then
    Result:=Get_tile(x,y,im)
  else begin
    Result:=false; iz:=Zoom;

    tw:=fData.tileWidth;  w:=tw div iz;
    th:=fData.tileHeight; h:=th div iz;
    size:=fData.tileSize div iz div iz;

    lev:=0; while iz > 1 do begin
      iz:=iz div 2; Inc(lev)
    end;

    bits:=im_bits; if size > 0 then

    if Seek_tile(x,y,si,len) then

    if fim_typ = typ_jpg then begin

      if ijl_Decompress1(im,w,h,fjpg_bits,
                         x*w,y*h,fjpg_zoom*zoom,
                         si,len,nil) then begin
        swap_rgb24(im,w,h); Result:=true
      end

    end else

    if info.biCompression = bi_JPG then begin

      if jpg_enabled then
      Result:=fjpg.Decompress(im,w,h,info.biBitCount,
                              si,len,@fjpg_props)

    end else
    if info.biCompression in [bi_ZIP,bi_LZW] then begin

      if Get_tile(x,y,im) then begin
        Zoom_bmp(im,Zoom,tw,w,h,bits);
        Result:=true
      end

    end else

    if info.biCompression = bi_RGB then begin

      if fTiles.Count = 0 then begin
        th:=len div fData.tileLine; h:=th div zoom
      end else len:=fData.tileSize;

      if h > 0 then

      if (im_bits < 8)
      or (Quads_Used > 0) then begin

        buf:=GetDataBuf(len);
        if Assigned(buf) then begin

          if fIs_bmp then
            load_bmp(im,buf,fData.tiffLine,
                     len div th,th)
          else
            Move(si^,buf^,len);

          if not is_tif then
            x_zoom_bmp(buf,im,Zoom,tw,th,bits)
          else begin
            Zoom_bmp(buf,Zoom,tw,w,h,bits);
            len:=tif_line(w,bits)*h;
            Move(buf^,im^,len);
          end;

          Result:=true
        end

      end else

      if (tw > 1024) or (th > 1024)
      or (Pad_Line(tw,bits) > 0) then begin

        buf:=GetDataBuf(len);
        if Assigned(buf) then begin

          Move(si^,buf^,len);

          if not is_tif then
            x_zoom_bmp(buf,im,Zoom,tw,th,bits)
          else begin
            Zoom_bmp(buf,Zoom,tw,w,h,bits);
            len:=tif_line(w,bits)*h;
            Move(buf^,im^,len);
          end;

          swap_rgb24(im,w,h); Result:=true
        end
      end else

      if ijl_Active then begin

        len:=fData.tileSize*2;
        buf:=GetDataBuf(len);

        if Assigned(buf) then begin

          len:=ijl_Compress_image(buf,len,
                                  si,fData.tileWidth,th,
                                  info.biBitCount,
                                  def_quality);

          if len > 0 then

          Result:=ijl_Decompress(im, w,h,info.biBitCount,
                                 buf,len,@fjpg_props)

        end
      end
    end;

    if not Result then
    Fillchar(im^,size,0)
  end
end;

procedure TTiffView.Set_tile(x,y: Integer; im: PBytes);
var
  si,buf: PBytes; p: PTilePos64;
  di,len,cx,size,ind,bufl: DWord;
begin
  if Seek_tile(x,y,si,len) then

  if Is_Tiled then
  if fis_upd then

  with fData do begin

    size:=tileSize; bufl:=size*3;
    buf:=GetDataBuf(bufl);

    if Assigned(buf) then begin cx:=0;

      if info.biCompression = bi_ZIP then
        cx:=zlb.xCompress(im,size, buf,bufl)
      else
      if info.biCompression = bi_JPG then
        cx:=ijl_Compress_entropy(buf,bufl,
                                 im,tileWidth,tileHeight,im_bits,
                                 @fjpg_props,fQuality);

      if cx > 0 then begin

        ind:=y*XTiles + x; di:=0;

        if cx <= len then
          Move(buf^,si^,cx)
        else
          di:=fFile.Append(buf^,cx);

        p:=fTiles[ind];
        if Assigned(p) then begin
          p.pos:=Max(p.pos,di); p.len:=cx;
          Update_tile(ind,p.pos,p.len)
        end
      end
    end

  end
end;

procedure TTiffView.swap_dib(im: PBytes);
var
  line,y1,y2: Integer; si,di,tmp: PBytes;
begin
  tmp:=GetDataBuf(fData.tileSize*3);

  y1:=0; y2:=fData.tileHeight-1;

  line:=fData.tileWidth * im_bits div 8;
  si:=im; di:=@im[line * y2];

  if tmp <> nil then
  while y1 < y2 do begin
    Move(si^,tmp^,line);
    Move(di^,si^,line);
    Move(tmp^,di^,line);

    si:=@si[line]; Inc(y1);
    di:=@di[-line]; Dec(y2)
  end
end;

function TTiffView.Get_dib(x,y: Integer; im: PBytes): Boolean;
begin
  Result:=Get_tile(x,y,im);
  swap_dib(im);
end;

procedure TTiffView.Set_dib(x,y: Integer; im: PBytes);
begin
  swap_dib(im);
  Set_tile(x,y,im);
  swap_dib(im);
end;

function TTiffView.Get_tex(x,y,zoom: Integer; tex: TTexture): Boolean;
var
  i,iz,loc,w,h: Integer;
  buf,si,di: PBytes; cl: tlong;
  swap: Boolean;
begin
  Result:=false; iz:=1;

  if info.biCompression in [bi_JPG] then

  if zoom >= 8 then iz:=8 else
  if zoom >= 4 then iz:=4 else
  if zoom >= 2 then iz:=2;

  w:=tile.Width div iz;
  h:=tile.Height div iz;

  tex.Depth:=3;
  if is_gray then tex.Depth:=1;

  if Is_Tiled then

  if im_bits in [8,16,24] then
  if tex.AllocRGB(w,h) then begin

    buf:=tex.Image;

    swap:=not fis_swap; fis_swap:=false;

    if iz = 1 then
      Result:=Get_tile(x,y,buf)
    else
      Result:=Pack_tile(x,y,iz,buf);

    if swap and (im_bits = 24) then
    swap_rgb(buf,w * h);

    fis_swap:=not swap;

    if Result then
    if tex.Depth = 3 then begin

      loc:=im_bits div 8;
      if loc in [1,2] then begin

        si:=@buf[h * w * loc - loc];
        di:=@buf[h * w * 3 - 3];

        for i:=1 to h*w do begin

          case loc of
        1:  with fQuads[si[0]] do begin
              cl.b[0]:=rgbRed;
              cl.b[1]:=rgbGreen;
              cl.b[2]:=rgbBlue;
            end;
          end;

          di[0]:=cl.b[0];
          di[1]:=cl.b[1];
          di[2]:=cl.b[2];

          Dec(TPointer(si),loc);
          Dec(TPointer(di),3)
        end
      end
    end
  end
end;

function TTiffView.GetLine(y: Integer): PBytes;

procedure plane_rgb(si,di: PBytes; Size: int);
begin
  while Size > 0 do begin
    di[0]:=si[0]; si:=@si[1];
    di:=@di[3]; Dec(Size)
  end
end;

procedure f4_to_b8(bp: PBytes; count: int);
const
  hnil = -32767;
var
  i,k: int; fp: PFloats; v,v1,v2,dv: float;
begin
  if fSMinSampleValue >= fSMaxSampleValue then begin

    fp:=@bp[0]; k:=0;
    for i:=1 to count do begin
      v:=fp[0]; fp:=@fp[1];

      if v > hnil then begin

        if k = 0 then begin
          v1:=v; v2:=v
        end else
        if v < v1 then v1:=v else
        if v > v2 then v2:=v;

        Inc(k)
      end
    end;

    dv:=(v2-v1)/5;

    fSMinSampleValue:=v1-dv;
    fSMaxSampleValue:=v2+dv
  end;

  float_to_pf8(bp,bp,count,
               SMinSampleValue,
               SMaxSampleValue)
end;

var
  lzw: tlzw; si,di,di1: PBytes;
  ind,rows,cols,line,sz,sz1,ip,bx,k: int;
  pos: Int64; p: PTilePos64;
begin
  Result:=nil;

  if fTiles.Count > 0 then begin

    sz:=fData.tileSize; sz1:=sz;
    if fData.NPlanes > 1 then Inc(sz1,sz);
    di:=GetDataBuf(sz1);

    rows:=fData.tileHeight;
    cols:=fData.tileWidth;

    line:=tif_Line(cols,info.biBitCount);

    if (y < fDataTop)
    or (y >= fDataTop+rows) then begin

      ind:=y div rows; fDataTop:=ind*rows;

      p:=fTiles[ind];
      if Assigned(p) then
      if p.len > 0 then begin

        si:=fFile.Seek(p.pos,p.len);
        if Assigned(si) then

        case info.biCompression of
      bi_RGB:
          if p.len <= sz then begin
            di1:=di;
            if fData.NPlanes > 1 then
            di1:=@di[sz];

            bx:=p.len;
            if not fis_x16 then
              Move(si^,di1^,bx)
            else begin bx:=bx div 2;

              k:=fMaxValue div 255;
              if fis_mac then begin
                if k <= 0 then
                  pf16_to_pf8b(si,di1,bx)
                else
                  pf16_to_pf8bf(si,di1,bx,k)
              end
              else begin
                if k <= 0 then
                  pf16_to_pf8a(si,di1,bx)
                else
                  pf16_to_pf8af(si,di1,bx,k)
              end
            end;

            if fData.NPlanes > 1 then begin
              plane_rgb(di1,di,bx);

              for ip:=1 to 2 do begin
                Inc(ind,fData.YTiles);
                p:=fTiles[ind];
                if Assigned(p) then
                if p.len > 0 then
                if p.len <= sz then begin

                  si:=fFile.Seek(p.pos,p.len);
                  if Assigned(si) then begin

                    bx:=p.len;
                    if not fis_x16 then
                      Move(si^,di1^,bx)
                    else begin bx:=bx div 2;
                      pf16_to_pf8_scale(si,di1,bx,fMaxValue)
                    end;

                    plane_rgb(di1,@di[ip],bx)
                  end
                end
              end
            end
          end;

      bi_LZW:
          begin
            lzw:=tlzw.Create(si,p.len,di,sz);
            try
              lzw.Unpack;

              if not fOnlyData then

              if fis_hdif then
                bmp_diff(di,cols,rows,im_bits,line)
              else
              if fis_float then
                f4_to_b8(di,sz div 4)
              else
              if fis_x32 then
                pf32_to_pf24(di,di,sz div 4);
            finally
              lzw.Free
            end
          end;

      bi_GR3:
          Hoffman.Unpack_gr3(si,p.len,di,rows,cols,line);

      bi_GR4:
          Hoffman.Unpack_gr4(si,p.len,di,rows,cols,line);

      bi_RLC:
          rlc_Unpack(si,p.len,di,sz);

      bi_ZIP:
          zlb.xDecompress(si,p.len, di,sz);
        end
      end
    end;

    Dec(y,fDataTop);
    if (y >= 0) and (y < rows) then
    Result:=@di[y*line]

  end
  else begin       
    line:=fData.tileLine;
    if fis_bmp then y:=info.biHeight-1-y;

    if (y < fScanTop)
    or (y >= fScanTop+fScanRows) then
    fScanPtr:=nil;

    if fScanPtr = nil then begin
      fScanTop:=int_Trunc(y,256); fScanRows:=256;
      if fScanTop+fScanRows > info.biHeight then
      fScanRows:=info.biHeight-fScanTop;

      pos:=fScanTop; pos:=pos*line + fData.bit_pos;
      fScanPtr:=fFile.Seek(pos,fScanRows * line)
    end;

    if Assigned(fScanPtr) then
    if y < fScanTop+fScanRows then begin
      Result:=@fScanPtr[(y-fScanTop) * line];

      if fis_x16 then begin
        line:=line div 2;
        di:=GetDataBuf(line);

        if Assigned(di) then
        pf16_to_pf8_scale(Result,di,line,fMaxValue);

        Result:=di
      end else
      if fis_x32 then begin
        di:=GetDataBuf(line);
        bx:=info.biWidth;

        si:=Result;

        if Assigned(di) then begin

          if fis_cmyk then begin
            Move(si^,di^,line); si:=di;
            cmyk_buf(Pointer(di),bx);
          end;

          pf32_to_pf24(si,di,bx)
        end;

        Result:=di
      end
    end
  end
end;

procedure TTiffView.SetSidClip(const R: XRect);
var
  s: TPoint;
begin
  fsidClip:=R; s:=fsid.ReadSize;

  with fsidClip do begin
    if w > s.X then w:=s.X;
    if x+w > s.X then w:=s.X-x;

    if h > s.Y then h:=s.Y;
    if y+h > s.Y then h:=s.Y-y;
  end
end;

function TTiffView.Zoom_rect(x,y,w,h,iz: Integer; im: XImage): Boolean;

function get_ecw(x,y: Integer; im: XImage): Boolean;
var
  di: PBytes;
  ix,iy,w,h,z,nBands: int;
  bands: Array[0..2] of int;
begin
  Result:=true;
  
  w:=im.im_w; h:=im.im_h; z:=fjpg_zoom;

  bands[0]:=0; bands[1]:=1; bands[2]:=2;
  nBands:=fInfo.biBitCount div 8;

  ix:=x*z; iy:=y*z;

  NCScbmSetFileView(fecw,nBands,@bands,
                    ix,iy,ix+w*z-1,iy+h*z-1,w,h);

  for iy:=0 to h-1 do begin
    di:=im.Lines[iy];

    if nBands = 1 then
  		NCScbmReadViewLineBIL(fecw,@di)
    else
      NCScbmReadViewLineRGB(fecw,di)
  end
end;

function get_sid(x,y: Integer; im: XImage): Boolean;
var
  si: PBytes; cx: Int64; w,h: int;
begin
  Result:=false;

  w:=im.im_w; h:=im.im_h;

  case fim_typ of
typ_sid:
    Result:=fsid.Decode(x,y,w,h,im.bmpBits);

typ_ecw:
    Result:=get_ecw(x,y,im);

typ_jpg:
    begin
      cx:=fData.bit_len;
      si:=fFile.Seek(fData.bit_pos,cx);

      if Assigned(si) then
      if ijl_Decompress1(im.bmpBits,
                         w,h,im.im_bits,
                         x,y,fjpg_zoom,
                         si,cx,nil) then begin

        if is_swap then                 
        if fjpg_bits = 24 then
        im.Swap_24_bits;

        Result:=true
      end
    end
  end
end;

function sid_load(x,y,iz: Integer; im: XImage): Boolean;
begin
  Result:=false;

  if iz = 1 then
    Result:=Get_sid(x,y,im)
  else
  if fsidBuf1.im_Alloc(im.im_w*iz,
                       im.im_h*iz,
                       im.im_bits) then

  if Get_sid(x*iz,y*iz,fsidBuf1) then begin
    im.im_Zoom(fsidBuf1,iz);
    Result:=im.Active
  end
end;

function sid_image(iz: int; im: XImage): Boolean;
var
  w,h,k1,k2: int;
begin
  Result:=false;

  fsidBuf1.is_tif:=false;

  if iz = 1 then
    Result:=Get_sid(0,0,im)
  else begin
    w:=fData.Width;
    h:=fData.Height;

    if fim_typ = typ_jpg then
    if fjpg_zoom = 1 then begin
      k1:=Min(8,iz);
      k2:=iz div k1;

      if k1*k2 = iz then begin
        w:=w div k1;
        h:=h div k1;
        fjpg_zoom:=k1;
        iz:=k2
      end
    end;

    if iz = 1 then
      Result:=Get_sid(0,0,im)
    else
    if fsidBuf1.im_Alloc(w,h,im.im_bits) then
    if Get_sid(0,0,fsidBuf1) then begin
      im.im_Zoom(fsidBuf1,iz);
      Result:=im.Active
    end
  end
end;

function sid_Tile(x,y,iz: Integer; im: XImage): Boolean;
var
  iw,ih,w,h,x2,y2,sx,sy: Integer; c: XRect;
begin
  Result:=false;

  c.x:=0; c.w:=fData.Width div iz;
  c.y:=0; c.h:=fData.Height div iz;

  if fsidClip.w > 0 then
  if fsidClip.h > 0 then c:=fsidClip;

  iw:=im.im_w; w:=iw;
  ih:=im.im_h; h:=ih;

  x2:=c.x+c.w; y2:=c.y+c.h;

  if w > c.w then w:=c.w;
  if x+w > x2 then w:=x2-x;

  if h > c.h then h:=c.h;
  if y+h > y2 then h:=y2-y;

  if (w > 0) and (h > 0) then begin

    if fsidPos.X >= 0 then begin
      x2:=fsidPos.X+fsidBuf.im_w;
      if (x < fsidPos.X) or (x+w > x2) then
      fsidPos.X:=-1;

      y2:=fsidPos.Y+fsidBuf.im_h;
      if (y < fsidPos.Y) or (y+h > y2) then
      fsidPos.X:=-1;
    end;

    if fsidPos.X < 0 then begin

      w:=Min(2048,c.w); sx:=x;
      h:=Min(2048,c.h); sy:=y;

      if sx+w > c.w then sx:=c.w-w;
      if sy+h > c.h then sy:=c.h-h;

      if fsidBuf.Active then
      if (w < fsidBuf.im_w)
      or (h < fsidBuf.im_h) then
      fsidBuf.im_Close;

      if not fsidBuf.Active then
      fsidBuf.im_Alloc(w,h,fjpg_bits);

      if fsidBuf.Active then
      if sid_load(sx,sy,iz,fsidBuf) then begin
        fsidPos.X:=sx; fsidPos.Y:=sy
      end
    end;

    if fsidPos.X >= 0 then
    if x >= fsidPos.X then
    if y >= fsidPos.Y then begin
      fsidBuf.Get_Tile1(x-fsidPos.X,y-fsidPos.Y,
                        iw,ih,im.im_line,im.bmpBits);

      Result:=true
    end
  end
end;

function All_image(x,y,w,h,iz: Integer): Boolean;
begin
  Result:=(x = 0) and (w = fData.Width div iz) and
          (y = 0) and (h = fData.Height div iz)
end;

var
  si,di: PBytes;
  iw,ih,bx,cx,dx,w1,h1,i,bits: Integer;
  swap: bool;
begin
  Result:=false; bits:=im_bits;

  bx:=img_Line(w,bits);

  if not im.Active
  or (im.im_w <> w)
  or (im.im_h <> h)
  or (im.im_bits <> bits)
  or (im.im_line <> bx) then begin
    im.is_tif:=false;
    im.im_Alloc(w,h,bits);
  end;

  if im.Active then
  if Is_lines then begin

    im.im_Fill(fClearColor);

    if fim_typ in [typ_jpg,typ_sid,typ_ecw] then begin

      if All_image(x,y,w,h,iz) then
        Result:=sid_image(iz,im)
      else
        Result:=sid_tile(x,y,iz,im)

    end else

    if info.biCompression in [bi_RGB,bi_LZW,
                              bi_GR3,bi_GR4,
                              bi_RLC,bi_ZIP] then begin

      iw:=info.biWidth div iz;
      ih:=info.biHeight div iz;

      if (x >= 0) and (x < iw) then
      if (y >= 0) and (y < ih) then begin

        if bits < 8 then im.im_Clear(0);

        w1:=w; if x+w > iw then w1:=iw-x;
        h1:=h; if y+h > ih then h1:=ih-y;

        bx:=(x*iz * bits) div 8; y:=y*iz;

        cx:=tif_line(w1,im.im_bits);
        dx:=im.im_line;

        di:=im.bmpBits;
        for i:=1 to h1 do begin
          si:=GetLine(y);
          if si = nil then Break;
          si:=@si[bx];

          if iz = 1 then
            Move(si^,di^,cx)
          else
          if bits = 1 then
            Zoom_pf1(si,di,w1,iz)
          else
          if bits = 4 then
            Zoom_pf4(si,di,w1,iz)
          else
          if bits = 8 then
            Zoom_pf8(si,di,w1,iz)
          else
          if bits = 24 then
            Zoom_pf24(si,di,w1,iz);

          di:=@di[dx]; Inc(y,iz)
        end;

        Result:=true; fScanPtr:=nil
      end;

      if bits = 24 then begin

        if is_bmp then
          swap:=not im.is_bgr
        else
          swap:=fis_swap and im.is_bgr;

        if swap then
        if im.is_tif then
          swap_rgb(im.bmpBits,w * h)
        else
          im.Swap_24_bits

      end
    end
  end
end;

function TTiffView.Get_Rect(im: XImage; x,y,w,h,iz: Integer): Boolean;
var
  buf: PBytes; bx,tw,th: Integer;
  i,j,i1,i2,j1,j2,k: Integer;
begin
  Result:=false;

  if Is_lines then
    Result:=Zoom_rect(x,y,w,h,iz,im)
  else begin

    bx:=img_Line(w,im_bits);

    if not im.Active
    or (im.im_w <> w)
    or (im.im_h <> h)
    or (im.im_bits <> im_bits)
    or (im.im_line <> bx) then begin
      im.is_tif:=false; im.im_Alloc(w,h,im_bits);
    end;

    if im.Active then begin

      buf:=Get_tile_buf;
      if Assigned(buf) then begin

        im.im_Fill(ClearColor);

        tw:=ftile.Width div iz;
        th:=ftile.Height div iz;

        i1:=x div tw; i2:=(x+w-1) div tw;
        j1:=y div th; j2:=(y+h-1) div th;

        k:=0;
        for j:=j1 to j2 do
        for i:=i1 to i2 do
        if Pack_tile(i,j,iz,buf) then begin
          im.Set_Tile(i*tw-x,j*th-y,tw,th,buf);
          Inc(k)
        end;

        Result:=k > 0
      end
    end
  end
end;

procedure TTiffView.Begin_progress(Sender: TObject);
begin
end;

constructor XTiffView.Create(ABits: tbyte_set);
begin
  inherited Create; fBits:=ABits
end;

function XTiffView.Open(Path: PChar): Boolean;
begin
  Result:=inherited Open(Path);
  if not (im_bits in fBits) then Close;
  Result:=Active
end;

constructor TTiff.Create;
begin
  inherited;
  Fillchar(fijl_props,Sizeof(fijl_props),0);
  fijl_props.IsValid:=S_FALSE;
  fgeo.s.pps:=-1
end;

destructor TTiff.Destroy;
begin
  if file_h > 0 then
  FileClose(file_h);
end;

procedure TTiff.SetIsBig(Value: Boolean);
begin
  fIsBig:=Value
end;

procedure TTiff.Set_tile(const bmp: TBitMapInfoHeader);
begin
  ftile:=bmp;
end;

procedure TTiff.SetRel(fmt: int; min,max: float);
begin
  frel.fmt:=fmt; frel.min:=min; frel.max:=max
end;

function TTiff.file_Create(Dest: PChar; tags: Integer): THandle;
begin
  fError:=0;
  file_h:=FileCreate(Dest);
  if file_h > 0 then Append_hdr(tags);
  Result:=file_h
end;

function TTiff.im_Create(Path: PChar;
                         bmp: PBitMapInfoHeader;
                         Quads: PRGBQuads): THandle;
var
  bits,tags,used,pos,stripe,comp: Integer;
  SamplesPerPixel,NPlanes: Integer; sz: Int64;
  samples: Array[0..3] of Word;
begin
  fError:=0;

  bits:=img_Bits(bmp); used:=0; tags:=8;

  if bits = 24 then
    Inc(tags)
  else
  if bits <= 8 then
  if Assigned(Quads) then
  if bmp.biClrUsed > 0 then begin
    Inc(tags); used:=bmp.biClrUsed
  end;

  if frel.fmt > 0 then Inc(tags,3);
  Inc(tags,GeoCount);

  sz:=tif_Line(bmp.biWidth,bits);
  sz:=sz * bmp.biHeight;
  if sz > MaxLongint then sz:=-1;;

  stripe:=0; comp:=1;

  if ftile.biWidth = bmp.biWidth then
  if ftile.biHeight > 0 then
  if bmp.biHeight div ftile.biHeight > 1 then begin
    stripe:=ftile.biHeight;
    comp:=ftile.biCompression;
  end;

  if (stripe > 0) or (sz > 0) then Inc(tags);

  Result:=file_Create(Path,tags);

  if Result > 0 then begin
    Append_dir(256,4,1,bmp.biWidth);
    Append_dir(257,4,1,bmp.biHeight);

    Fillchar(samples,Sizeof(samples),0);
    SamplesPerPixel:=0;

    NPlanes:=bmp.biPlanes;

    if bits <> 24 then
      Append_dir(258,3,1,bmp.biBitCount)
    else begin
      Append_dir(258,3,3,0); NPlanes:=3;
      samples[0]:=8; samples[1]:=8; samples[2]:=8;
      SamplesPerPixel:=3
    end;

    Append_dir(259,3,1,comp); // Compression

    if used > 0 then
      Append_dir(262,3,1,3)
    else
    if bits = 24 then
      Append_dir(262,3,1,2)
    else
      Append_dir(262,3,1,1);

    Append_dir(273,4,1,0);
    Append_dir(277,3,1,NPlanes);

    if stripe = 0 then
      Append_dir(278,4,1,bmp.biHeight)
    else
      Append_dir(278,4,1,stripe);

    if (stripe > 0) or (sz > 0) then
    Append_dir(279,4,1,sz);

    if bits = 24 then
    Append_dir(284,3,1,1);

    if used > 0 then Append_dir(320,1,0,0);

    if frel.fmt > 0 then begin
      Append_dir(339,3,1,frel.fmt);
      Append_dir(340,11,1,pint(@frel.min)^);
      Append_dir(341,11,1,pint(@frel.max)^);
    end;

    geo_Append_dir; Close_dir;
    geo_Append_data;

    if SamplesPerPixel > 1 then
    Update_buf(258,@samples,SamplesPerPixel*2);

    if used > 0 then
    Update_rgb(Quads,used);

    pos:=file_size;
    Update_dir(273,4,4,pos);
    file_Seek(pos)
  end
end;

function TTiff.Append_hdr(tags: int): int64;
var
  hdr: tiff_hdr; ax: int64;
begin
  hdr.cod:=$4949; hdr.ver:=42;
  if fIsBig then hdr.ver:=43;
  hdr.ind:=8; file_Write(hdr,8);

  if fIsBig then begin
    dir_pos:=16; file_Write(dir_pos,8);
    ax:=tags; file_Write(ax,8);
    dir_size:=8+SizeOf(tiff_dir64)*tags+8
  end
  else begin
    dir_pos:=8; file_Write(tags,2);
    dir_size:=2+SizeOf(tiff_dir32)*tags+4
  end;

  Result:=dir_pos+dir_size
end;

function TTiff.Append_dir(tag,typ,len: int; ind: int64): int64;
var
  r32: tiff_dir32; r64: tiff_dir64;
begin
  if fIsBig then begin
    r64.tag:=tag; r64.typ:=typ;
    r64.len:=len; r64.ind:=ind;
    file_Write(r64,20);
  end
  else begin
    r32.tag:=tag; r32.typ:=typ;
    r32.len:=len; r32.ind:=ind;
    file_Write(r32,12);
  end;

  Result:=file_Size+4
end;

procedure TTiff.Append_list(List: TTiffData);
var
  i,n: int; lp: ptiff_dir_arr64;
  r32: tiff_dir32; r64: tiff_dir64;
begin
  n:=List.Count;
  if n > 0 then begin
    lp:=List.First;

    if fIsBig then
      file_Write(lp^,n*20)
    else
    for i:=0 to n-1 do begin
      r64:=lp[i];
      r32.tag:=r64.tag;
      r32.typ:=r64.typ;
      r32.len:=r64.len;
      r32.ind:=r64.tag;

      file_Write(r32,12)
    end
  end
end;

procedure TTiff.Update_list(List: TTiffData);
var
  i: int; lp: ptiff_dir_arr64; p: Pointer;
  r: tiff_dir64;
begin
  lp:=List.First;
  for i:=0 to List.Count-1 do begin
    r:=lp[i]; p:=List.xSeek(lp[i]);
    if Assigned(p) then
    Update_buf(r.tag,p,r.len)
  end
end;

function TTiff.Close_dir: int64;
var
  ind: int64; cx: int;
begin
  close_pos:=file_Size;
  ind:=0; cx:=4;
  if fIsBig then cx:=8;
  file_Write(ind,cx);
  Result:=file_Size
end;

function TTiff.next_layer(tags: int): bool;
var
  ax,pos: int64; cx: int;
begin
  Result:=false;

  if close_pos > 0 then begin
    pos:=file_Size; file_Seek(close_pos);
    cx:=4; if fIsBig then cx:=8;
    file_Write(pos,cx); file_Seek(pos);

    ax:=tags; cx:=2;
    dir_size:=2+SizeOf(tiff_dir32)*tags+4;

    if fIsBig then begin cx:=8;
      dir_size:=8+SizeOf(tiff_dir64)*tags+8
    end;

    file_Write(ax,cx); dir_pos:=pos;
    Result:=true
  end
end;

function TTiff.Continue_dir(tags: int): int64;
var
  ax,pos: int64; cx: int;
begin
  Result:=file_Size; pos:=Result;

  cx:=4; if fIsBig then cx:=8;
  file_Seek(dir_pos+dir_size-cx);
  file_Write(pos,cx);

  dir_pos:=file_Size;

  if fIsBig then begin
    ax:=tags; file_Write(ax,8);
    dir_size:=8+SizeOf(tiff_dir64)*tags+8
  end
  else begin
    file_Write(tags,2);
    dir_size:=2+SizeOf(tiff_dir32)*tags+4
  end
end;

function TTiff.Update_dir(tag,typ,len: int; ind: int64): int;
var
  i,dirn,dirl: int;
  dir_: tiff_dir32;
  dir: tiff_dir64;
  ax,pos: int64;
begin
  Result:=-1;

  dirn:=0; pos:=dir_pos;

  file_Seek(pos);

  if fIsBig then begin
    file_Read(ax,8); dirn:=ax;
    Inc(pos,8); dirl:=20
  end
  else begin
    file_Read(dirn,2);
    Inc(pos,2); dirl:=12
  end;

  if dirn > 0 then
  for i:=0 to dirn-1 do begin

    if fIsBig then
      file_Read(dir,20)
    else begin
      file_Read(dir_,12);
      dir.tag:=dir_.tag;
      dir.typ:=dir_.typ;
      dir.len:=dir_.len;
      dir.ind:=dir_.ind;
    end;

    if dir.tag = tag then begin
      if typ > 0 then dir.typ:=typ;

      if dir.typ = 3 then len:=len div 2 else
      if dir.typ = 4 then len:=len div 4 else
      if dir.typ = 5 then len:=len div 8 else
      if dir.typ = 12 then len:=len div 8 else
      if dir.typ = 16 then len:=len div 8;

      dir.len:=len; dir.ind:=ind;

      file_Seek(pos);

      if fIsBig then
        file_Write(dir,20)
      else begin
        dir_.len:=len; dir_.ind:=ind;
        file_Write(dir_,12)
      end;

      Result:=i; Break
    end;

    Inc(pos,dirl)
  end
end;

function TTiff.Update_buf(tag: int;
                          buf: pointer; len: int): int64;
var
  pos: int64;
begin
  Result:=0;
  if (buf <> nil) and (len > 0) then begin
    pos:=file_size;
    if Update_dir(tag,0,len,pos) >= 0 then begin
      file_Seek(pos); file_Write(buf^,len);
      Result:=pos
    end
  end
end;

procedure TTiff.Update_rgb(rgbp: PRGBQuads; used: int);
var
  i: int; buf: TWords;
begin
  for i:=0 to used-1 do with rgbp[i] do begin
    buf[i]:=rgbRed; buf[used+i]:=rgbGreen;
    buf[used+used+i]:=rgbBlue
  end;

  for i:=0 to used*3-1 do buf[i]:=buf[i] shl 8;
  Update_buf(320,@buf,used*3*2)
end;

function TTiff.Update_ind(tag,typ,len: int): int64;
var
  bx: int;
begin
  Result:=file_size;

  if typ = 0 then typ:=4; bx:=4;
  if typ = 16 then bx:=8;

  Update_dir(tag,typ,len,Result);
  file_Seek(Result); Inc(Result,len*bx)
end;

function TTiff.buff_Write(var buf; len: int): int64;
var
  pos: int64;
begin
  Result:=0; pos:=file_pos;

  if len > 0 then
  if pos + len <= pos then
    fError:=-1
  else begin
    file_Write(buf,len);
    Result:=pos
  end
end;

function TTiff.Get_tile(bmp,buf: PBytes;
                        im_w,im_h,im_bits,zoom: int;
                        pos: int64; len: int): int;
var
  comp,im_size,cx: int; old_pos: int64;
begin
  comp:=ftile.biCompression;

  old_pos:=file_pos; file_Seek(pos);
  file_Read(buf^,len); file_Seek(old_pos);

  im_size:=im_h * im_w * im_bits div 8;
  Fillchar(bmp^,im_size,0);

  if len > 0 then
  if (comp = Comp_jpg)
  or (comp = Comp_jpf) then begin

    if ijl_Active then
    ijl_Decompress(bmp, im_w,im_h,im_bits,
                   buf,len,@fijl_props);

  end else
  if (comp = comp_zlb)
  or (comp = comp_zip) then begin
    cx:=im_size * zoom * zoom;
    if zlb.xDecompress(buf,len, bmp,cx) then
    Zoom_bmp(bmp,zoom, 0,im_w,im_h,im_bits);
  end else

  if comp = comp_ecw then begin

    if ecwDecompress(bmp,im_w*zoom,im_h*zoom,im_bits div 8,
                     buf,len) then
    Zoom_bmp(bmp,zoom, 0,im_w,im_h,im_bits);
    
  end else

  if comp = Comp_rgb then begin
    cx:=im_size * zoom * zoom;
    if len = cx then begin
      Zoom_bmp(buf,zoom, 0,im_w,im_h,im_bits);
      Move(buf^,bmp^,im_size)
    end
  end;

  Result:=im_size
end;

procedure TTiff.file_Read(var buf; len: int);
begin
  FileRead(file_h,buf,len)
end;

procedure TTiff.file_Write(var buf; len: int);
begin
  FileWrite(file_h,buf,len)
end;

function TTiff.file_Seek(pos: int64): int64;
begin
  Result:=FileSeek(file_h,pos,0)
end;

function TTiff.file_Size: int64;
begin
  Result:=FileSeek(file_h,0,2)
end;

function TTiff.file_pos: int64;
begin
  Result:=FileSeek(file_h,0,1)
end;

procedure TTiff.SetGeo(Ageo: lg_Transit);
var
  i: int; x0,y0: Double;
begin
  fgeo.s.pps:=-1;

  if Ageo.s.prj in [0,1,2,3,29] then
  if Ageo.s.elp in [0,1,9] then begin

    fgeo:=Ageo;

    if fgeo.s.prj = 29 then begin
      for i:=0 to 3 do
      with fgeo.g[i] do begin
        sys_XY_BL(x,y,fgeo.s,x,y);
        x:=x*180/Pi; y:=y*180/Pi
      end
    end
    else begin
      x0:=fgeo.s.x0;
      y0:=fgeo.s.y0;

      if Abs(x0)+Abs(y0) > 1 then
        fgeo.s.prj:=0
      else
      if fgeo.s.prj > 0 then

      if (Abs(x0) > 0.01)
      or (Abs(y0) > 0.01) then begin

        y0:=y0 - prj_ZoneY(fgeo.s);

        for i:=0 to 3 do
        with fgeo.g[i] do begin
          x:=x - x0; y:=y - y0;
        end
      end
    end;

    fgeo.s.x0:=0;
    fgeo.s.y0:=0;
  end
end;

function TTiff.GetGeoCount: Integer;
begin
  Result:=0;

  with fgeo.s do
  if pps >= 0 then
  if prj <= 0 then
    Result:=2
  else begin
    Result:=3;
    if prj = 29 then Result:=4 else
    if (prj <> 2) or (elp <> 9) then
    Result:=5
  end
end;

procedure TTiff.geo_Scale(kz: Double);
var
  i: Integer;
begin
  for i:=0 to 3 do
  with fgeo.l[i] do begin
    x:=x*kz; y:=y*kz;
  end
end;

procedure TTiff.geo_Append_dir;
begin
  if GeoCount >= 2 then begin
    Append_dir(33550,12,0,0);
    Append_dir(33922,12,0,0);

    if GeoCount >= 3 then begin
      Append_dir(34735,3,0,0);

      if GeoCount = 4 then begin
        if fgeo.s.prj = 29 then
          Append_dir(34737,2,0,0)
        else
          Append_dir(34736,12,0,0)
      end else
      if GeoCount = 5 then begin
        Append_dir(34736,12,0,0);
        Append_dir(34737,2,0,0)
      end
    end
  end
end;

procedure TTiff.geo_Append_data;
const
  magic = 'IMAGINE GeoTIFF Support'#10;

const
  ru_a    = 6378137;
  ru_alfa = 298.257223563;

type
  TParams = Array[0..6] of Double;
var
  scale: txyz; l1,l2,dy: Double; sys: tsys;
  iz,pcs,nk,nd,gc,gprj,gelp,gdat: Integer;
  dir: TGeoSubKeys; dat: TParams; tie: TTiepoint;
  gs: TCmdStr; ts: TShortstr;
begin
  gc:=GeoCount;
  if gc >= 2 then begin

    StrCopy(gs,'');

    with fgeo do begin
      l1:=Gauss_dist(l[0],l[1]);
      l2:=Gauss_dist(g[0],g[1]);
      scale.x:=l2 / Max(1,l1);

      l1:=Gauss_dist(l[0],l[3]);
      l2:=Gauss_dist(g[0],g[3]);
      scale.y:=l2 / Max(1,l1);
      scale.z:=0;
    end;

    dy:=0; pcs:=0; iz:=0; sys:=fgeo.s;

    if gc = 3 then
    if sys.prj in [1,2] then begin
      iz:=iZone(sys.lc);

      if sys.prj = 1 then begin
        if iz in [4..32] then begin
          dy:=iz*1000000; pcs:=28400 + iz
        end
      end else
      if sys.prj = 2 then begin
        Inc(iz,30);
        if iz > 60 then
        iz:=iz mod 60;

        if iz in [1..60] then begin
          dy:=iz*1000000; pcs:=32600 + iz;
        end
      end
    end;

    with fgeo.l[0] do tie.a:=_xyz(x,y,0);
    with fgeo.g[0] do tie.b:=_xyz(y-dy,x,0);

    Update_buf(33550,@scale,3*8);
    Update_buf(33922,@tie,6*8);

    if gc > 2 then begin
      Fillchar(dir,Sizeof(dir),0);

      Fillchar(dat,Sizeof(dat),0);
      nd:=0;

      nk:=3; if pcs > 0 then nk:=4;
      dir[0]:=GeoSubKey(1,1,0,nk);
      dir[1]:=GeoSubKey(1024,0,1,1);
      dir[2]:=GeoSubKey(1025,0,1,1);
      dir[3]:=GeoSubKey(3076,0,1,9001);
      dir[4]:=GeoSubKey(3072,0,1,pcs);

      if gc = 3 then begin
        nk:=3; if pcs > 0 then nk:=4;
        dir[0]:=GeoSubKey(1,1,0,nk);
        dir[1]:=GeoSubKey(1024,0,1,1);
        dir[2]:=GeoSubKey(1025,0,1,1);
        dir[3]:=GeoSubKey(3076,0,1,9001);
        dir[4]:=GeoSubKey(3072,0,1,pcs);
      end else
      if fgeo.s.prj = 29 then begin
        nk:=0;
        dir[0]:=GeoSubKey(1,1,0,nk);
        dir[1]:=GeoSubKey(1024,0,1,2);
        dir[2]:=GeoSubKey(1025,0,1,1);

        StrCopy(ts,magic);
        StrCat(ts,'Projection Name = Geographic (Lat/Lon)'#10);
        StrCat(ts,'Units = degrees'#10+'GeoTIFF Units = dd|');

        dir[3]:=GeoSubKey(1026,34737,Strlen(ts),Strlen(gs));
        StrLCat(gs,ts,Sizeof(gs)-1);

        dir[4]:=GeoSubKey(2048,0,1,4326);
        dir[5]:=GeoSubKey(2054,0,1,9102); nk:=5;

        dir[0].w4:=nk;
      end
      else begin nk:=0;

        dir[0]:=GeoSubKey(1,1,0,nk);
        Inc(nk); dir[nk]:=GeoSubKey(1024,0,1,1);
        Inc(nk); dir[nk]:=GeoSubKey(1025,0,1,1);

        StrCopy(ts,magic);

        if sys.prj = 3 then
          StrCat(ts,'Projection Name = Mercator'#10)
        else
          StrCat(ts,'Projection Name = Transverse Mercator'#10);

        StrCat(ts,'Units = meters'#10+'GeoTIFF Units = meters|');

        Inc(nk); dir[nk]:=GeoSubKey(1026,34737,Strlen(ts),Strlen(gs));
        StrLCat(gs,ts,Sizeof(gs)-1);

        Inc(nk); dir[nk]:=GeoSubKey(2048,0,1,32767);

        if sys.elp <> 9 then begin StrCopy(ts,magic);
          StrCat(ts,'Unable to match Ellipsoid (Datum) to a GeographicTypeGeoKey value'#10+
                    'Ellipsoid = Krasovsky'#10+'Datum = Pulkovo 1942|');

          Inc(nk); dir[nk]:=GeoSubKey(2049,34737,Strlen(ts),Strlen(gs));
          StrLCat(gs,ts,Sizeof(gs)-1);
        end;

        gelp:=32767; gdat:=32767;

        if sys.elp = 1 then begin
          gelp:=7024; gdat:=6284;
        end else
        if sys.elp = 9 then begin
          gelp:=7030; gdat:=6326;
        end;

        Inc(nk); dir[nk]:=GeoSubKey(2050,0,1,gdat);
        Inc(nk); dir[nk]:=GeoSubKey(2051,0,1,8901);
        Inc(nk); dir[nk]:=GeoSubKey(2052,0,1,9001);
        Inc(nk); dir[nk]:=GeoSubKey(2054,0,1,9102);     // degree
        Inc(nk); dir[nk]:=GeoSubKey(2056,0,1,gelp);

        if sys.elp <> 9 then begin
          Inc(nk); dir[nk]:=GeoSubKey(2057,34736,1,5);  // elp_A
          Inc(nk); dir[nk]:=GeoSubKey(2058,34736,1,6);  // elp_B
        end;

        Inc(nk); dir[nk]:=GeoSubKey(3072,0,1,32767);

        gprj:=1; if sys.prj = 3 then gprj:=7;

        StrCopy(ts,magic);

        if sys.prj = 3 then
          StrCat(ts,'Projection = Mercator|')
        else
          StrCat(ts,'Projection = Transverse Mercator|');

        Inc(nk); dir[nk]:=GeoSubKey(3073,34737,Strlen(ts),Strlen(gs));
        StrLCat(gs,ts,Sizeof(gs)-1);

        Inc(nk); dir[nk]:=GeoSubKey(3074,0,1,32767);
        Inc(nk); dir[nk]:=GeoSubKey(3075,0,1,gprj); // Projection Type
        Inc(nk); dir[nk]:=GeoSubKey(3076,0,1,9001);

        if sys.prj = 3 then begin
          Inc(nk); dir[nk]:=GeoSubKey(3078,34736,1,1);
          Inc(nk); dir[nk]:=GeoSubKey(3082,34736,1,2);
          Inc(nk); dir[nk]:=GeoSubKey(3083,34736,1,3);
        end
        else begin
          Inc(nk); dir[nk]:=GeoSubKey(3080,34736,1,1);
          Inc(nk); dir[nk]:=GeoSubKey(3081,34736,1,2);
          Inc(nk); dir[nk]:=GeoSubKey(3082,34736,1,3);
          Inc(nk); dir[nk]:=GeoSubKey(3083,34736,1,4);
        end;

        Inc(nk); dir[nk]:=GeoSubKey(3092,34736,1,0);

        dir[0].w4:=nk; dat[0]:=1;

        if sys.prj = 3 then begin
          dat[1]:=sys.b1*180/Pi; nd:=4;
        end
        else begin
          dat[1]:=sys.lc*180/Pi;
          dat[3]:=prj_ZoneY(fgeo.s);
          nd:=5;
        end;

        if sys.elp <> 9 then begin

          if sys.elp > 1 then
            Get_Ellipsoid(sys.elp,dat[5],dat[6])
          else begin
            dat[5]:=ru_a;
            dat[6]:=ru_a*(1-1/ru_alfa);
          end;

          nd:=7
        end
      end;

      Update_buf(34735,@dir,(nk+1)*4*2);

      if nd > 0 then
      Update_buf(34736,@dat,nd*8);

      if gc >= 5 then
      if Strlen(gs) > 0 then
      Update_buf(34737,@gs,Strlen(gs));
    end
  end
end;

constructor TTiffTiled.Create;
begin
  inherited;
  foff_typ:=4; foff_loc:=4;
  fQuality:=def_quality
end;

destructor TTiffTiled.Destroy;
begin
  xFreePtr(fBuffer);

  if fPalette <> 0 then
  DeleteObject(fPalette);

  foff32p:=xFreePtr(foff32p);
  inherited
end;

function TTiffTiled.GetPath: PChar;
begin
  Result:=fPath
end;

procedure TTiffTiled.SetIsBig(Value: Boolean);
begin
  inherited SetIsBig(Value);
  foff_typ:=4; foff_loc:=4;
  if fIsBig then begin
    foff_typ:=16; foff_loc:=8;
  end
end;

function TTiffTiled.kh_Create1(ADest: PChar;
                               nx,ny,xpage,ypage,bits: int;
                               Quads: PRGBQuads; used: int;
                               iw,ih,fc: int; Chan: PBytes): Boolean;
function begin_level(ADest: PChar; tags: int): Boolean;
var
  ax,pos: int64; cx: int;
begin
  Result:=false;

  if file_h > 0 then
    Result:=next_layer(tags)
  else
  if Assigned(ADest) then begin
    Result:=file_Create(ADest,tags) > 0;
    StrCopy(fPath,ADest)
  end
end;

var
  i,comp,fot,tags,sz,ax: int;
  samples: Array[0..3] of Word;
  SamplesPerPixel: int; v: TInt64;
begin
  Result:=false; fTileCount:=0;

  tags:=10; comp:=comp_jpg; fClearColor:=fc;

  if (bits = 1) and not fIsTga then used:=0;
  if bits > 8 then used:=0;

  if (used > 0) or (bits < 8) or fIsPng then
  comp:=comp_zip;

  if bits > 4 then Chan:=nil;

  if used > 0 then Inc(tags);
  if fc <> clNone then Inc(tags);
  if Assigned(Chan) then Inc(tags);

  if (iw > 0) and (ih > 0) then begin
    nx:=int_Tiles(iw,xpage);
    ny:=int_Tiles(ih,ypage);
  end
  else begin
    iw:=nx*xpage; ih:=ny*ypage;
  end;

  if fIsChkBig then
  if not fIsBig then begin
    ax:=iw div 256; ax:=ax * (ih div 256);
    IsBig:=ax > 65536
  end;

  fSize.cx:=iw; fSize.cy:=ih;

  fXTiles:=nx; fYTiles:=ny;
  fNtiles:=nx * ny; fBits:=bits;

  Fillchar(ftile,Sizeof(ftile),0);
  ftile.biWidth:=xpage;
  ftile.biHeight:=ypage;
  ftile.biBitCount:=bits;
  ftile.biPlanes:=1;
  ftile.biCompression:=comp;
  ftile.biClrUsed:=used;

  foff32p:=xFreePtr(foff32p);
  flenp:=nil;

  if fNtiles > 0 then begin

    with ftile do
    sz:=tif_Line(biWidth,bits) * biHeight;
    if sz <> fbufLen then
    fBuffer:=xFreePtr(fBuffer);
    fbufLen:=sz;

    foff32p:=xAllocDWords(fNtiles*3);
    foff64p:=Pointer(foff32p);

    if Assigned(foff32p) then
    if begin_level(ADest,tags) then begin

      flenp:=@foff64p[fNtiles];

      Fillchar(samples,Sizeof(samples),0);
      SamplesPerPixel:=0;

      if bits = 24 then begin
        samples[0]:=8; samples[1]:=8; samples[2]:=8;
        SamplesPerPixel:=3
      end;

      Append_dir(256,4,1,fSize.cx);
      Append_dir(257,4,1,fSize.cy);

      if SamplesPerPixel <= 1 then
        Append_dir(258,3,1,bits)
      else begin
        v.x:=0; if fIsBig then
        for i:=0 to SamplesPerPixel-1 do
        v.w[i]:=samples[i];

        Append_dir(258,3,SamplesPerPixel,v.x)
      end;

      Append_dir(259,3,1,comp);

      fot:=1;
      if bits = 24 then
        fot:=2
      else
      if bits = 1 then begin

        if Assigned(Quads) then
        if Quad_Color(Quads[1]) = 0 then
        fot:=0

      end else
      if used > 0 then fot:=3;
      Append_dir(262,3,1,fot);

      if SamplesPerPixel > 1 then
        Append_dir(277,3,1,SamplesPerPixel)
      else
        Append_dir(277,3,1,1);

      Append_dir(322,3,1,xpage);
      Append_dir(323,3,1,ypage);
      Append_dir(324,foff_typ,fNtiles,0);
      Append_dir(325,4,fNtiles,0);

      if used > 0 then
      Append_dir(320,3,0,0);

      if fc <> clNone then
      Append_dir(900,4,1,fc);

      if Assigned(Chan) then
      Append_dir(901,1,0,0);

      Close_dir;

      if (SamplesPerPixel > 1) and not fIsBig then
      Update_buf(258,@samples,SamplesPerPixel*2);

      if fNtiles > 1 then begin
        foffp_ind:=Update_buf(324,foff32p,fNtiles*foff_loc);
        flenp_ind:=Update_buf(325,flenp,fNtiles*4);
      end;

      if Assigned(Quads) then
        fQuads:=Quads^
      else begin
        Fillchar(fQuads,Sizeof(fQuads),0);
        xGray_Quads(@fQuads,used);
      end;

      if used > 0 then
      Update_rgb(@fQuads,used);

      if Assigned(Chan) then
      Update_buf(901,Chan,16);

      Result:=true
    end
  end
end;

function TTiffTiled.kh_Create(ADest: PChar;
                              nx,ny,page,bits: int;
                              Quads: PRGBQuads; used: int;
                              iw,ih,fc: int; Chan: PBytes): Boolean;
begin
  Result:=kh_Create1(ADest,
                     nx,ny,page,page,bits,
                     Quads,used, iw,ih,fc, Chan)
end;

procedure TTiffTiled.Close_doc;
begin
  if file_h > 0 then
  FileClose(file_h); file_h:=0
end;

function TTiffTiled.Close_level: Integer;
begin
  Result:=0;

  if file_h > 0 then begin

    if fNtiles = 1 then begin
      Update_dir(324,0,foff_loc,foff64p[0]);
      Update_dir(325,0,4,flenp[0]);
    end
    else begin
      file_Seek(foffp_ind);
      file_Write(foff32p^,fNtiles*foff_loc);

      file_Seek(flenp_ind);
      file_Write(flenp^,fNtiles*4)
    end;

    Inc(fLevelCount); Result:=fTileCount
  end;

  if fPalette <> 0 then
  DeleteObject(fPalette);
  fPalette:=0;
end;

procedure TTiffTiled.Build_pyramid(fc: Integer);
var
  img: XImage; bp1,bp2: PBytes;
  pp32,lp: PDWords; pp64: PInt64s;
  lev,w,h,iz,pg,pg1,ip,jp: int;
  nx,ny,i,j,_i,_j,k: int; bx: int64;
  cx: DWord; sz: TSize;
begin
  img:=XImage.Create;
  try
    if fLevelCount > 0 then begin

      img.is_bmp:=false;
      img.is_tif:=true;

      if fBuffer = nil then
      fBuffer:=xAllocPtr(fbufLen*4);

      pg:=ftile.biWidth;
      pg1:=pg div 2; sz:=fSize;

      if pg = pg1*2 then
      if Assigned(fBuffer) then
      if img.im_Alloc(pg,pg,fBits) then begin

        if fBits = 8 then begin
          img.Set_Quads(@fQuads);
          img.im_used:=ftile.biClrUsed
        end;

        bp1:=fBuffer; bp2:=@bp1[fbufLen];

        iz:=1;
        for lev:=1 to 31 do begin

          if Assigned(fOnLevel) then
          fOnLevel(lev);

          iz:=iz*2; pp32:=nil;
          w:=Int_tiles(sz.cx,iz);
          h:=Int_tiles(sz.cy,iz);

          if Assigned(foff32p) then
          if (w >= pg) and (h >= pg) then begin
            nx:=fXTiles; ny:=fYTiles;
            pp32:=xAllocDWords(fNTiles*3);
            pp64:=Pointer(pp32)
          end;

          if pp32 = nil then Break;

          lp:=@pp64[fNTiles];
          Move(foff32p^,pp32^,fNTiles*foff_loc);
          Move(flenp^,lp^,fNTiles*4);

          if kh_Create(nil, 0,0,
                       pg,fBits,
                       @fQuads,
                       ftile.biClrUsed,
                       w,h,fc,nil) then begin

            for jp:=0 to fYTiles-1 do
            for ip:=0 to fXTiles-1 do begin
              k:=0; img.im_Fill(fc);

              for j:=0 to 1 do
              for i:=0 to 1 do begin
                _i:=ip*2 + i; _j:=jp*2 + j;

                if (_i < nx) and (_j < ny) then begin

                  if fIsBig then bx:=pp64[_j*nx + _i]
                            else bx:=pp32[_j*nx + _i];

                  cx:=lp[_j*nx + _i];

                  if (bx > 0) and (cx > 0) then
                  if Get_tile(bp1,bp2, pg1,pg1,fBits,2, bx,cx) > 0 then begin
                    img.Set_Tile(i*pg1,j*pg1,pg1,pg1,bp1);
                    Inc(k)
                  end
                end
              end;

              if k > 0 then
              Pack_tile(ip,jp,img.bmpBits)
            end;

            if Close_level = 0 then Break
          end;

          xFreePtr(pp32)
        end
      end
    end;
  finally
    img.Free
  end
end;

procedure TTiffTiled.Put_tile(i,j: Integer;
                              buf: PBytes; len: Integer);
var
  ind,pos: uint;
begin
  if len > 0 then
  if (i >= 0) and (i < fXTiles) then
  if (j >= 0) and (j < fYTiles) then

  if Assigned(foff32p) then begin

    ind:=j*fXTiles + i;

    pos:=buff_Write(buf^,len);

    if pos = 0 then len:=0 else
    len:=file_size - pos;

    if len > 0 then begin
      if fIsBig then foff64p[ind]:=pos
                else foff32p[ind]:=pos;

      flenp[ind]:=len;

      Inc(fTileCount)
    end
  end
end;

procedure TTiffTiled.Pack_tile(i,j: Integer; buf: PBytes);
var
  comp,cx,dx: Integer; di: PBytes;
begin
  if (i >= 0) and (i < fXTiles) then
  if (j >= 0) and (j < fYTiles) then begin

    comp:=fTile.biCompression;

    if fbufLen > 0 then

    if comp = comp_rgb then

      Put_tile(i,j,buf,fBufLen)

    else
    with ftile do begin
      if fBuffer = nil then
      fBuffer:=xAllocPtr(fbufLen*4);

      if Assigned(fBuffer) then begin

        cx:=fbufLen; di:=fBuffer; dx:=0;

        if comp = comp_jpg then
  
          dx:=ijl_Compress_image(di,cx*3,buf,
                                 biWidth,biHeight,fBits,
                                 Max(85,fQuality))

        else
        if comp = comp_zip then
          dx:=zlb.xCompress(buf,cx,di,cx*3);

        if dx > 0 then Put_tile(i,j,di,dx)
      end
    end
  end
end;

type
  TDirList32 = class(TCustomList)
    constructor Create;
  end;

  TDirList64 = class(TCustomList)
    constructor Create;
  end;

constructor TDirList32.Create;
begin
  inherited Create(Sizeof(tiff_dir32),32)
end;

constructor TDirList64.Create;
begin
  inherited Create(Sizeof(tiff_dir64),32)
end;

function tiff_add_pyramid(Path1,Path2: PChar): Boolean;

function add_next(fileh: THandle;
                  Path: PChar; big: bool): int64;
var
  h: THandle; pos,sz: int64;
  bx,cx: int; buf: PBytes;
begin
  Result:=0;
  h:=FileOpen(Path,fmOpenRead);
  if h > 0 then begin
    cx:=4096*4;
    buf:=xAllocPtr(cx);
    if Assigned(buf) then begin

      xFileBot(fileh);

      pos:=0; cx:=4;
      if big then cx:=8;
      FileWrite(fileh,pos,cx);

      Result:=xFileBot(fileh);

      sz:=xFileBot(h);
      xFileSeek(h,0);

      while sz > 0 do begin
        bx:=Min(cx,sz);
        FileRead(h,buf^,bx);
        FileWrite(fileh,buf^,bx);
        Dec(sz,bx)
      end;
    end;

    xFreePtr(buf); FileClose(h)
  end
end;

function Shift_dir32(fileh: THandle; top: int): int;

procedure Shift_offs(fileh: THandle; bx,cx,top: int);
var
  ax: int;
begin
  while cx > 0 do begin
    FileSeek(fileh,bx,0);
    FileRead(fileh,ax,4);

    Inc(ax,top);
    FileSeek(fileh,bx,0);
    FileWrite(fileh,ax,4);

    Inc(bx,4); Dec(cx)
  end
end;

var
  list: TDirList32; p: Pointer;
  hdr: tiff_hdr; dir: tiff_dir32;
  nxt,pos,sz: uint; cnt: Word;
begin
  Result:=0;

  list:=TDirList32.Create;
  try
    sz:=xFileBot(fileh);

    xFileSeek(fileh,top);
    FileRead(fileh,hdr,8);

    Inc(hdr.ind,top);

    if hdr.cod = $4949 then
    if hdr.ind < sz-2 then

    while hdr.ind > 0 do begin
      FileSeek(fileh,hdr.ind,0);
      FileRead(fileh,cnt,2); nxt:=0;

      pos:=hdr.ind+2;
      if pos + Sizeof(dir)*cnt <= sz then begin

        if Result = 0 then Result:=hdr.ind;

        list.Clear;
        while cnt > 0 do with dir do begin

          FileRead(fileh,dir,12);
          Inc(pos,12); Dec(cnt);

          if dir.len > 1 then
          Inc(dir.ind,top);

          if dir.tag = 324 then
          if dir.len = 1 then
            Inc(dir.ind,top)
          else begin
            Shift_offs(fileh,dir.ind,dir.len,top);
            FileSeek(fileh,pos,0)
          end;

          list.Add(@dir)
        end;

        if list.Count > 0 then begin
          p:=list.First;
          FileSeek(fileh,hdr.ind+2,0);
          FileWrite(fileh,p^,list.BufferSize);

          if pos <= sz-4 then begin
            FileRead(fileh,nxt,4);
            if nxt > 0 then begin
              Inc(nxt,top);
              if nxt <= hdr.ind then nxt:=0;
              if nxt >= sz-2 then nxt:=0;
              FileSeek(fileh,pos,0);
              FileWrite(fileh,nxt,4)
            end
          end
        end;

        hdr.ind:=nxt
      end
    end;
  finally
    list.Free
  end
end;

function Shift_dir64(fileh: THandle; top: int64): int64;

procedure Shift_offs(fileh: THandle;
                     bx: int64; cx: int;
                     top: int64);
var
  ax: int64;
begin
  while cx > 0 do begin
    xFileSeek(fileh,bx);
    FileRead(fileh,ax,8);

    Inc(ax,top);
    xFileSeek(fileh,bx);
    FileWrite(fileh,ax,8);

    Inc(bx,8); Dec(cx)
  end
end;

var
  list: TDirList64; p: Pointer;
  hdr: tiff_hdr; dir: tiff_dir64;
  hdr_pos,nxt,pos,sz,cnt: int64;
begin
  Result:=0;

  list:=TDirList64.Create;
  try
    sz:=xFileBot(fileh);

    xFileSeek(fileh,top);
    FileRead(fileh,hdr,8);
    FileRead(fileh,hdr_pos,8);

    Inc(hdr_pos,top);

    if hdr.cod = $4949 then
    if hdr_pos < sz-8 then

    while hdr_pos > 0 do begin
      xFileSeek(fileh,hdr_pos);
      FileRead(fileh,cnt,8); nxt:=0;

      pos:=hdr_pos+8;
      if pos + Sizeof(dir)*cnt <= sz then begin

        if Result = 0 then Result:=hdr_pos;

        list.Clear;
        while cnt > 0 do with dir do begin

          FileRead(fileh,dir,20);
          Inc(pos,20); Dec(cnt);

          if dir.len > 1 then
          if (dir.typ = 3) and (dir.len = 3) then
          else Inc(dir.ind,top);

          if dir.tag = 324 then

          if dir.len = 1 then
            Inc(dir.ind,top)
          else begin
            Shift_offs(fileh,dir.ind,dir.len,top);
            xFileSeek(fileh,pos)
          end;

          list.Add(@dir)
        end;

        if list.Count > 0 then begin
          p:=list.First;
          xFileSeek(fileh,hdr_pos+8);
          FileWrite(fileh,p^,list.BufferSize);

          if pos <= sz-8 then begin
            FileRead(fileh,nxt,8);
            if nxt > 0 then begin
              Inc(nxt,top);
              if nxt < 16 then Break;
              if nxt >= sz-8 then nxt:=0;
              xFileSeek(fileh,pos);
              FileWrite(fileh,nxt,8)
            end
          end
        end;

        hdr_pos:=nxt
      end
    end;
  finally
    list.Free
  end
end;

function Link_doc32(fileh: THandle; doc: uint): uint;
var
  hdr: tiff_hdr;
  nxt,pos,len,sz: uint;
  cnt: Word;
begin
  Result:=0;

  sz:=FileSeek(fileh,0,2);

  FileSeek(fileh,0,0);
  FileRead(fileh,hdr,8);

  if hdr.cod = $4949 then
  if hdr.ind < sz-2 then

  while hdr.ind > 0 do begin
    FileSeek(fileh,hdr.ind,0);
    FileRead(fileh,cnt,2); nxt:=0;

    if cnt < 256 then begin
      len:=Sizeof(tiff_dir32)*cnt;
      pos:=hdr.ind + 2 + len;

      if pos <= sz-4 then begin
        Result:=pos;

        FileSeek(fileh,pos,0);
        FileRead(fileh,nxt,4);

        if nxt > 0 then begin
          if nxt <= hdr.ind then nxt:=0;
          if nxt >= sz-2 then nxt:=0;
        end
      end
    end;

    hdr.ind:=nxt
  end;

  if Result > 0 then begin
    FileSeek(fileh,Result,0);
    FileWrite(fileh,doc,4);
  end;
end;

function Link_doc64(fileh: THandle; doc: int64): int64;
var
  hdr: tiff_hdr; len: int;
  hdr_pos,nxt,pos,sz,cnt: int64;
begin
  Result:=0;

  sz:=xFileBot(fileh);

  xFileSeek(fileh,0);
  FileRead(fileh,hdr,8);
  FileRead(fileh,hdr_pos,8);

  if hdr.cod = $4949 then
  if hdr_pos < sz-8 then

  while hdr_pos > 0 do begin
    xFileSeek(fileh,hdr_pos);
    FileRead(fileh,cnt,8); nxt:=0;

    if cnt < 256 then begin
      len:=Sizeof(tiff_dir64)*cnt;
      pos:=hdr_pos + 8 + len;

      if pos <= sz-8 then begin
        Result:=pos;

        xFileSeek(fileh,pos);
        FileRead(fileh,nxt,8);

        if nxt < 16 then nxt:=0;
        if nxt >= sz-8 then nxt:=0;
      end
    end;

    hdr_pos:=nxt
  end;

  if Result > 0 then begin
    xFileSeek(fileh,Result);
    FileWrite(fileh,doc,8);
  end;
end;

var
  tiff: TTiffView; fileh: THandle;
  bot,nxt: int64; big: bool;
begin
  Result:=false;

  tiff:=TTiffView.Create;
  try
    if tiff.Open(Path1) then begin

      big:=tiff.is_Big;
      if tiff.Open(Path2) then
      if tiff.is_Big = big then begin

        tiff.Close;

        fileh:=FileOpen(Path1,fmOpenReadWrite);
        if fileh > 0 then begin

          bot:=add_next(fileh,Path2,big);
          if bot > 0 then begin

            if big then
              nxt:=Shift_dir64(fileh,bot)
            else
              nxt:=Shift_dir32(fileh,bot);

            if nxt > 0 then
            if big then
              Link_doc64(fileh,nxt)
            else
              Link_doc32(fileh,nxt)
          end;

          FileClose(fileh)
        end
      end
    end;
  finally
    tiff.Free
  end
end;

function tiff_crypted(Path: PChar): Boolean;
var
  tif: TTiffView;
begin
  Result:=false;
  if This_ext(Path,'.tif') then begin
    tif:=TTiffView.Create;
    try
      if tif.Open(Path) then
      Result:=tif.Crypt > 0
    finally
      tif.Free
    end
  end
end;

initialization
begin
  cid_key:=key_load('tif.key');
end;

end.
