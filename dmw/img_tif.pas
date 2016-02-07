unit img_tif; interface

uses
  Classes,LCLType,Math,otypes,xgdi,idib;
                                 
function im_Open(Path: PChar; info: PBitMapInfoHeader): HResult; stdcall;
function im_Stat(Path: PChar; info: PBitMapInfoHeader): Integer; stdcall;
procedure im_Done; stdcall;
procedure im_Free; stdcall;

procedure im_ClearColor(Color: Integer); stdcall;

function im_Bmp(out Offset: Integer): PBytes; stdcall;

function im_Palette(Quads: PRGBQuads): Integer; stdcall;

procedure im_Draw(DC: ICanvas; Path: PChar;
                  dc_x,dc_y,dc_w,dc_h: int;
                  src_x,src_y: int; kz: Float); stdcall;

function im_Flags: longint; stdcall;
function im_Position: longint; stdcall;
procedure im_GotoLine(y: Integer; buf: PBytes); stdcall;
procedure im_ReadLine(y: Integer; buf: PBytes); stdcall;

function im_Tiled(var tile_w,tile_h: Integer): boolean; stdcall;
function im_GetTile(buf: PBytes; x,y,w,h,size: longint): PBytes; stdcall;

function im_Get_Hex(chan: PBytes): longint; stdcall;

procedure im_Put_Index(indp: PIndex; len: longint); stdcall;
procedure im_Put_Hex(chan: PBytes); stdcall;

procedure im_Put_Palette(Quads: PRGBQuads); stdcall;

implementation

uses
  SysUtils,
  ofiles,convert,xlog,
  xlzw,ijpg,img_glu,
  XPens,Img_x,xddw,tiff_x,tiff_y,
  img_rel,img_rgb,xecw,xsid,ozf2,
  XIntf1,XInter,img_dib;

const
  B_Bit: array[0..7] of byte =
  ($80,$40,$20,$10,$08,$04,$02,$01);

  cash_size = 4*4096;

var
  ecw: TNCSFileView;
  sid: tltiImage;
  wms: ITileImage;
  ozf: tozf2;
  jpg: IJpeg;

  image: TRGBImage;
  imagePath: TPathStr;

  Frame: TDibFrame;

  im_h: int;
  im_h1: int;

  im_Zoom: Integer;
  fm_Zoom: Integer;
  jpg_Zoom: Integer;

  jpg_pos: Int64;
  jpg_len: Int64;

  Offset: PInt64s;

  only_read: LongBool;
  fileSize: Int64;

  im_what: (im_NIL,
            im_TIF,im_JPG,im_ECW,im_SID,
            im_REL,im_RLZ,im_VEG,im_RGB,
            im_EPS,im_WMS,im_RSW,im_OZF);

  im_View: TFileMemory;
  im_Pos: Int64;

  dir_pos: int64;

  rgb_pos: int64;
  rgb_len: int;

  bit_pos: int64;
  bit_size: int64;
  hex_pos: int64;

  im_info: TBitMapInfoHeader;
  im_Size: TPoint;

  ClearColor: Integer;

  tiff: record
    bufp,bufp1: PBytes;
    size,size1,line,bits,bufy,rows: int;
    offp: int64; offTyp,offLen: int;
    lenp: int64; lenTyp,lenLen: int;
    PlanarConfig,Photometric,Planes: int;
    SampleFormat,MinValue,MaxValue,Pack: int;
  end;

  tile: record
    Width,Height,Size,Size1: int;
    XTiles,YTiles,NTiles: int;

    offp,lenp: int64;
    offTyp,lenTyp: int;

    bufp: PBytes;
    Width1,Height1: int;
    palOfs,palLen: uint;
  end;

  rsw_comp: int;
  rsw_len0: uint;
  rsw_len1: uint;

  im_icb: TImageParams;
  im_chan: TChannel;

  im_pack: Longbool;
  im_float: Longbool;
  im_cmyk: Longbool;
  im_alfa: Longbool;
  im_pred: Longbool;
  im_inv: Longbool;
  im_mac: Longbool;
  im_big: Longbool;
  is_chan: Longbool;
  im_hdif: Longbool;
  is_dem: Longbool;

  rel: TRelHeader;
  rlz: TRlzHeader;

  cmyk_dot: uint;

  jpg_props_pos: int64;
  jpg_props_len: Integer;
  jpg_props: TJPEGTables;

  tif_key: DWord;
  cid_key: DWord;

  tif_rgb: Longbool;

  SMinSampleValue: float;
  SMaxSampleValue: float;

  im_Path: TShortStr;

function im_Bits: Integer;
begin
  with im_info do
  Result:=biBitCount*biPlanes;
end;

procedure Init;
begin
  im_mac:=false;
  im_big:=false;

  Fillchar(im_View,Sizeof(im_View),0);

  Offset:=nil;

  im_h:=0; im_h1:=0; 

  rgb_pos:=0; rgb_len:=0;
  bit_pos:=0; bit_size:=0; hex_pos:=0;
  FillChar(im_info,SizeOf(im_info),0);
  FillChar(tiff,SizeOf(tiff),0);
  tiff.bufp:=nil; tiff.bufp1:=nil;
  dir_pos:=0; im_Size.X:=0; im_Size.Y:=0;

  FillChar(tile,SizeOf(tile),0);

  FillChar(rel,SizeOf(rel),0);

  Fillchar(im_icb,Sizeof(im_icb),0);

  Init_Gray_Channel(@im_chan);
  is_chan:=false;

  fm_Zoom:=0;
  im_what:=im_NIL;
  im_inv:=false;

  im_View.ptr:=nil;
  im_View.size:=0;
  im_Pos:=0;

  im_pack:=false;
  im_float:=false;
  im_cmyk:=false;
  im_alfa:=false;
  im_pred:=false;
  im_hdif:=false;

  jpg_props_pos:=0;
  jpg_props_len:=0;
  FillChar(jpg_props,SizeOf(jpg_props),0);
  jpg_props.IsValid:=false;

  tif_rgb:=true; ecw:=0; ozf.Close;

  sid.Close; wms:=nil; tif_key:=0;

  ClearColor:=$FFFFFF
end;

procedure Free;
begin
  if ecw <> 0 then
  NCScbmCloseFileViewEx(ecw,false);

  FileMemoryFree(im_View);
  if im_h > 0 then FileClose(im_h);
  if im_h1 > 0 then FileClose(im_h1);

  Offset:=xFreePtr(Offset);

  with tiff do begin
    bufp:=xFreePtr(bufp);
    bufp1:=xFreePtr(bufp1);
  end;

  with tile do bufp:=xFreePtr(bufp);

  Init
end;

procedure f4_to_b8(si,di: PBytes; count: int);
const
  hnil = -32767;
var
  i,k: int; fp: PFloats; v,v1,v2,dv: float;
begin
  if SMinSampleValue >= SMaxSampleValue then begin

    fp:=@si[0]; k:=0;
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

    SMinSampleValue:=v1-dv;
    SMaxSampleValue:=v2+dv
  end;

  float_to_pf8(si,di,count,
               SMinSampleValue,
               SMaxSampleValue)
end;

function Read_ind(top: int64; typ,ind: uint): int64;
var
  ax,bx: int64; cx: int;
begin
  ax:=0; cx:=4;
  if typ = 3 then cx:=2 else
  if typ = 16 then cx:=8;

  bx:=top+ind*cx;

  if bx+cx <= fileSize then

  if Assigned(im_View.ptr) then
    Move(im_View.ptr[bx],ax,cx)
  else begin
    xFileSeek(im_h,bx);
    FileRead(im_h,ax,cx)
  end;

  if im_mac then
  if cx = 4 then ax:=SwapInt(ax)
            else ax:=SwapInt64(ax);

  Result:=ax
end;

procedure dump_offs(Path: PChar);
var
  txt: TTextfile;
  i: int; pos: int64; lzw: uint;
begin
  txt:=TTextfile.Create;
  try
    if txt.Make_ext(Path,'.!!!') then

    with tiff do
    for i:=0 to offLen-1 do begin

      if Assigned(Offset) then
        pos:=Offset[i]
      else
        pos:=Read_ind(offp,offTyp,i);

      lzw:=Read_ind(lenp,lenTyp,i);
      txt.WriteStr(Format('%d %d %d',[i,pos,lzw]));
    end;
  finally
    txt.Free
  end
end;

function kfa_normal(info: PBitMapInfoHeader): Boolean;
var
  top,off,nxt,bx: int64; len,y: int;
begin
  Result:=false;

  if im_what = im_TIF then
  if im_info.biCompression = bi_RGB then
  with tiff do if rows = 1 then
  if (offp > 0) and (lenp > 0) then begin
    top:=Read_ind(offp,offTyp,0); off:=top;
    nxt:=Read_ind(offp,offTyp,1); bx:=nxt - off;

    Result:=true;
    for y:=2 to im_info.biHeight-1 do begin
      off:=nxt; nxt:=Read_ind(offp,offTyp,y);
      if nxt - off <> bx then begin
        Result:=false; Break
      end
    end;

    if Result then begin line:=bx;

      if im_pack then begin
        if bx div Pack = im_info.biSize then
        bit_pos:=top;
      end
      else begin
        im_info.biSize:=bx; bit_pos:=top;
        if Assigned(info) then info.biSize:=bx
      end
    end
  end
end;

procedure im_Read(buf: Pointer; pos: int64; len: uint);
begin
  if Assigned(im_View.ptr) then
    Move(im_View.ptr[pos],buf^,len)
  else begin
    xFileSeek(im_h,pos);
    FileRead(im_h,buf^,len);
  end
end;

function jpg_enabled: bool;
begin
  if jpg = nil then begin
    GetJpgInterface(IJPeg,jpg);
    if Assigned(jpg) then
    AllocJpegTables(jpg,jpg_props)
  end;

  Result:=Assigned(jpg)
end;

function jpeg_load(buf: XImage; ix,iy: Integer): Boolean;
var
  bp: PBytes; sz: Int64;
begin
  Result:=false;

  bp:=@im_View.ptr[jpg_pos];
  sz:=jpg_len;

  if Assigned(jpg) then
  if Assigned(bp) then

  Result:=jpg.Decompress1(buf.bmpBits,
                          buf.im_w,buf.im_h,buf.im_bits,
                          ix,iy,jpg_Zoom,
                          bp,sz,nil)
end;

function jpeg_tile(ip,jp: Integer; buf: PBytes): Boolean;
var
  bp: PBytes; w,h,sz: uint;
begin                     
  Result:=false;

  bp:=im_View.ptr; sz:=im_View.Size;

  w:=tile.Width; h:=tile.Height;

  if Assigned(jpg) then
  if Assigned(bp) then

  Result:=jpg.Decompress1(buf,w,h,im_bits,
                          w*ip,h*jp,jpg_Zoom,
                          bp,sz,nil)
end;

function Tile_pos(i,j: int): int64;
var
  ax,bx: int64; cx: int;
begin
  Result:=0;

  if im_what <> im_JPG then

  if tile.NTiles = 1 then
    Result:=tile.offp
  else begin
    cx:=4; if im_big then
    if tile.offTyp = 16 then cx:=8;

    bx:=tile.offp + (j*tile.XTiles+i)*cx;

    if bx+cx <= fileSize then begin
      if Assigned(im_View.ptr) then begin
        if cx = 16 then
          Result:=PInt64(@im_View.ptr[bx])^
        else
          Result:=PInteger(@im_View.ptr[bx])^
      end
      else begin
        xFileSeek(im_h,bx);
        ax:=0; FileRead(im_h,ax,cx);
        Result:=ax
      end;

      if tif_key > 0 then
      Result:=Decrypt(tif_key,Result)
    end
  end
end;

function Tile_len(i,j: int): DWord;
var
  ax,bx: int64; cx: int;
begin
  Result:=0;

  if im_what = im_JPG then
    Result:=im_View.Size
  else
  if tile.NTiles = 1 then
    Result:=tile.lenp
  else begin
    cx:=4; if im_big then
    if tile.lenTyp = 16 then cx:=8;

    bx:=tile.lenp + (j*tile.XTiles+i)*cx;

    if bx+cx <= fileSize then begin
      if Assigned(im_View.ptr) then
        if cx = 8 then
          Result:=PInt64(@im_View.ptr[bx])^
        else
          Result:=PInteger(@im_View.ptr[bx])^
      else begin
        xFileSeek(im_h,bx);
        ax:=0; FileRead(im_h,ax,cx);
        Result:=ax
      end;

      if tif_key > 0 then
      Result:=Decrypt(tif_key,Result)
    end
  end
end;

function Load_jpg_props: Boolean;
begin
  Result:=false;

  if jpg_props.isValid then
    Result:=true
  else
  if jpg_props_pos > 0 then
  if jpg_props_len > 0 then

  if jpg_enabled then

  if Assigned(im_View.ptr) then

    Result:=jpg.LoadTables(jpg_props,
                           @im_View.ptr[jpg_props_pos],
                           jpg_props_len)

  else
  with tile do
  if jpg_props_len < Size*2 then
  if Assigned(bufp) then begin
    im_Read(bufp,jpg_props_pos,jpg_props_len);
    Result:=jpg.LoadTables(jpg_props,bufp,jpg_props_len)
  end
end;

function Seek_stripe(pos: int64; len: DWord): PBytes;
begin
  Result:=nil;

  if pos+len <= fileSize then

  if Assigned(im_View.ptr) then
    Result:=@im_View.ptr[pos]
  else
  with tiff do begin
    if (bufp1 = nil) or (len > Size1) then begin
      xFreePtr(bufp1);
      if len > Size1 then
      Size1:=int_Round(len,4096*4);
      bufp1:=xAllocPtr(Size1);
    end;

    if Assigned(bufp1) then begin
      Result:=bufp1;
      xFileSeek(im_h,pos);
      FileRead(im_h,bufp1^,len)
    end
  end
end;

function tile_scale(Zoom: Integer): Integer;
begin
  Result:=1; with tile do

  if this_x2(Width) then
  if this_x2(Height) then
  if Zoom in [1,2,4,8,16,64] then

  if im_info.biCompression in [bi_JPG] then
  Result:=Min(Zoom,8)
end;

function Seek_tile(pos: int64; len: DWord): PBytes;
begin
  Result:=nil;

  if pos+len <= fileSize then

  if Assigned(im_View.ptr) then
    Result:=@im_View.ptr[pos]
  else
  with tile do
  if Assigned(bufp) then

  if len <= Size1 then begin
    Result:=@bufp[Size];
    xFileSeek(im_h,pos);
    FileRead(im_h,Result^,len)
  end;
end;

function TileRead(ip,jp,zoom: Integer;
                  buf: PBytes; sz: Integer): Boolean;

function tile_jpg(si: Pointer; len,zoom: int; buf: PBytes): bool;
var
  w,h: int;
begin
  Result:=false;

  if len >= 16 then
  with tile do begin

    w:=Width div zoom; h:=Height div zoom;

    if jpg_enabled then
    Result:=jpg.Decompress(buf,w,h,im_Bits,
                           si,len,@jpg_props)
  end
end;

procedure rlz_pack(dst,src: PBytes);
var
  i,j,hi,ax,_ik,_ib: int;

  z1,kz,_k,_b: Double;
  si: PWords; di: PBytes;
begin
  z1:=rel.zmin;
  kz:=255/rel.zmax;

  _k:=kz * rlz.k0;
  _b:=(rlz.a0 + z1) * kz;

  _ik:=Round(_k * $10000);
  _ib:=Round(_b * $10000);

  si:=@src[0]; di:=dst;

  with tile do
  for j:=1 to Height do begin

    for i:=1 to Width do begin

      ax:=si[0];
      if ax = 0 then
        hi:=0
      else begin
        hi:=(ax*_ik - _ib) div $10000;
        if hi < 0 then hi:=0;
        if hi > 255 then hi:=255;
      end;

      di[0]:=hi;
      si:=@si[1]; di:=@di[1];
    end;

    if rlz.tile_nx and 1 <> 0 then
    si:=@si[1]
  end
end;

function rsw_tile(ip,jp: Integer; buf: PBytes): Boolean;

procedure bound_tile(si,di: PBytes; tw,th,ts: int);
var
  bx,cx,dx,y: int;
begin
  bx:=tif_line(tw,tiff.bits);
  dx:=tile.Size div tile.Height;

  Fillchar(di^,0,tile.Size);

  cx:=0;
  for y:=1 to th do begin
    Move(si^,di^,bx);
    si:=@si[bx]; di:=@di[dx];
    Inc(cx,bx)
  end
end;

var
  bx: int64;
  cx,dx,tw,th: uint;
  tp: TPoint; si,di: PBytes;
begin
  Result:=false;

  bx:=tile.offp;
  Inc(bx,(jp*tile.XTiles+ip)*8);

  if bx+8 <= fileSize then begin

    if Assigned(im_View.ptr) then
      tp:=PPoint(@im_View.ptr[bx])^
    else begin
      xFileSeek(im_h,bx);
      FileRead(im_h,tp,8);
    end;

    si:=nil;
    if (tp.X > 0) and (tp.Y > 0) then

    if tp.Y <= tile.Size1 then

    if im_h1 = 0 then
      si:=Seek_tile(tp.X,tp.Y)
    else begin
      bx:=tp.X; bx:=bx shl 8;

      if Assigned(tile.bufp) then
      if tp.Y <= tile.Size1 then

      if bx < rsw_len0 then begin

        if bx+tp.Y <= rsw_len0 then begin
          si:=@tile.bufp[tile.Size];
          xFileSeek(im_h,bx);
          FileRead(im_h,si^,tp.Y)
        end
      end
      else begin
        Dec(bx,rsw_len0);
        if bx+tp.Y <= rsw_len1 then begin
          si:=@tile.bufp[tile.Size];
          xFileSeek(im_h1,bx);
          FileRead(im_h1,si^,tp.Y)
        end
      end
    end;

    if Assigned(si) then begin

      tw:=tile.Width; th:=tile.Height;
      if ip = tile.XTiles-1 then tw:=tile.Width1;
      if jp = tile.YTiles-1 then th:=tile.Height1;

      if im_info.biCompression = bi_JPG then begin

        di:=buf;
        if (tw < tile.Width)
        or (th < tile.Height) then
        di:=tile.bufp;

        if jpg.Decompress(di,tw,th,tiff.bits,
                          si,tp.Y,nil) then begin

          if (tw < tile.Width)
          or (th < tile.Height) then
          bound_tile(di,buf,tw,th,0);

          Result:=true
        end
      end
      else begin
        cx:=tp.Y;
        if cx > tile.Size then
        cx:=tile.Size;

        if (tw = tile.Width) and (th = tile.Height) then
          Move(si^,buf^,cx)
        else
          bound_tile(si,buf,tw,th,tp.Y);

        Result:=true
      end
    end
  end
end;

var
  pos: int64; len,cx,k: DWord;
  si: Pointer; lzw: tlzw;
begin
  Result:=false; len:=0;

  if Assigned(buf) then begin

    pos:=0; len:=tile.Size;

    if (jp >= 0) and (jp < tile.YTiles) then
    if (ip >= 0) and (ip < tile.XTiles) then

    if im_info.biCompression = bi_SID then
      Result:=sid.Get_tile(ip,jp,buf)
    else
    if im_info.biCompression = bi_WMS then begin
      if Assigned(wms) then
      Result:=wms.GetTile(ip,jp,buf) > 0;
    end else

    if im_what = im_JPG then
      Result:=jpeg_tile(ip,jp,buf)
    else
    if im_what = im_RSW then begin
      Result:=rsw_tile(ip,jp,buf);

      if im_bits = 24 then
      if rsw_comp <> 0 then
      with tile do x_swap_rgb(buf,Width,Height)
    end else
    if im_what = im_OZF then begin
      if im_bits = 24 then
        Result:=ozf.getTileRGB(ip,jp,buf)
      else
        Result:=ozf.getTile(ip,jp,buf)
    end
    else begin

      if bit_pos = 0 then begin
        pos:=Tile_pos(ip,jp); cx:=tile_len(ip,jp);
      end;

      if (pos > 0) and (cx > 0) then begin

        if im_info.biCompression = bi_JPG then
        Load_jpg_props;

        si:=Seek_tile(pos,cx);
        if Assigned(si) then

        if im_info.biCompression = bi_JPG then
          Result:=tile_jpg(si,cx,zoom,buf)
        else
        if im_info.biCompression = bi_ZIP then begin

          Result:=zlb.xDecompress(si,cx, buf,tile.Size);

          if Result then
          if im_what = im_RLZ then
            rlz_pack(buf,buf)

        end else
        if im_info.biCompression = bi_LZW then begin

          lzw:=tlzw.Create(si,cx,buf,tile.Size);
          try
            begin
              lzw.Unpack; Result:=true
            end;
          finally
            lzw.Free
          end;

          if Result then begin

            if im_pack then
            if tiff.bits = 32 then begin

              k:=tile.size div 4;

              if im_info.biBitCount = 8 then begin
                if im_float then
                  f4_to_b8(buf,buf,k)
                else
                  pf32_to_pf8_scale(buf,buf,k,0)
              end else
              if im_info.biBitCount = 24 then
                pf32_to_pf24(buf,buf,k);
            end;

            if im_hdif then
            bmp_diff(buf,tile.Width,tile.Height,
                     im_bits,0);

            if im_bits = 24 then
            x_swap_rgb(buf,tile.Width,tile.Height)

          end
        end else
        if im_info.biCompression = bi_ECW then begin

          with tile do
          Result:=ecwDecompress(buf,Width,Height,im_bits div 8,si,cx)

        end else

        if im_info.biCompression = bi_RGB then begin

          if im_pack then begin

            if im_info.biBitCount in [8,24] then
            if tiff.Pack = 2 then begin
              pf16_to_pf8_scale(si,buf,sz,tiff.MaxValue);
              Result:=true
            end else
            if tiff.bits = 32 then begin
              pf32_to_pf24(si,buf,tile.size div 4);
              Result:=true
            end

          end
          else begin
            Move(si^,buf^,cx);
            if im_bits = 24 then
            swap_rgb(buf,cx div 3);
            Result:=true
          end

        end;

      end else
      if im_what = im_JPG then begin
        si:=im_View.ptr; if Assigned(si) then
        Result:=tile_jpg(si,cx,zoom,buf);
      end
    end;

    if len > 0 then
    if not Result then
    FillChar(buf^,sz,0)
  end
end;

function tif_Open(Path: PChar; info: PBitMapInfoHeader): Integer;

function eps_load(Path: PChar): Boolean;

function eps_line(y: Integer; p: PChar; l: Integer): Boolean;
var
  x,w,i,j,al,bl,r,g,b: Integer;
  di: PBytes; cmyk: tcmyk;
  b4: Array[0..3] of Byte; ch: Char;
begin
  Result:=false; w:=Image.im_w;
  if w*8 = l then begin

    di:=Image.Lines[y];
    if Assigned(di) then

    for x:=0 to w-1 do begin

      for i:=0 to 3 do begin

        bl:=0;
        for j:=0 to 1 do begin
          ch:=p[0]; p:=@p[1];

          if ch in ['0'..'9'] then
            al:=ord(ch)-ord('0')
          else
          if ch in ['A'..'F'] then
            al:=10+ord(ch)-ord('A')
          else
            al:=0;

          bl:=(bl shl 4) + al;
        end;

        b4[i]:=bl
      end;

      cmyk.c:=b4[0]/255;
      cmyk.m:=b4[1]/255;
      cmyk.y:=b4[2]/255;
      cmyk.k:=b4[3]/255;
      CMYK_RGB(cmyk, r,g,b);

      di[0]:=b; di[1]:=g; di[2]:=r;
      di:=@di[3]
    end;

    Result:=true
  end
end;

var
  vm: TReadfile; p1,p2: PChar;
  iline,len,w,h,z,l: Integer;
  s: TShortstr;
begin
  Result:=false;

  vm:=TReadfile.Create;
  try
    if vm.Open(Path) then begin

      len:=vm.Size; iline:=0;
      p1:=@vm.buf[0]; p2:=p1;

      while len > 2 do begin
        p2:=@p2[1]; Dec(len);
        if p2[0] = #13 then
        if p2[1] = #10 then begin

          if iline = 0 then begin
            l:=p2-p1;

            if l < 256 then begin
              StrLCopy(s,p1,p2-p1);
              if IntToken(s,w) then
              if IntToken(s,h) then
              if IntToken(s,z) then begin

                if Image.Active then
                if (Image.im_w <> w) or (Image.im_h <> h)
                or (StrComp(imagePath,Path) <> 0) then
                Image.im_Close;

                if Image.Active then begin
                  Result:=true; Break
                end else
                if Image.im_Alloc(w,h,24) then begin
                  StrLCopy(imagePath,Path,Sizeof(imagePath)-1);
                  Image.is_bmp:=false; Result:=true
                end
              end
            end
          end else
          if not eps_line(iline-1,p1,p2-p1) then begin
            Image.im_Close; Result:=false; Break
          end;

          if not Result then Break;
          p2:=@p2[2]; Dec(len,2);
          p1:=p2; Inc(iline)
        end;
      end;
    end;
  finally
    vm.Free
  end;

  Image.is_bmp:=true
end;

procedure Read_buf(h: THandle; pos: int64;
                   var buf; len: Integer);
var
  old: int64;
begin
  old:=xfilePos(h);
  xFileSeek(h,pos);
  FileRead(h,buf,len);
  xFileSeek(h,old);
end;

function Read_str(h: THandle;
                  const dr: tiff_dir64;
                  Str: PChar; MaxLen: int): PChar;
begin
  Result:=nil; StrCopy(Str,'');

  with dr do if typ = 2 then
  if (len > 0) and (len <= MaxLen) then begin
    Read_buf(h,ind,Str[0],len);
    Str[len]:=#0; Result:=Str
  end
end;

function Read_int(h: THandle; pos: int64;
                  typ,len: int; val: PIntegers;
                  _mac: Boolean): Integer;
var
  i,ax,loc: Integer; old: int64;
begin
  Result:=0; loc:=0;
  if typ = 3 then loc:=2;

  if loc > 0 then begin

    old:=xFilePos(h);
    xFileSeek(h,pos);

    for i:=0 to len-1 do begin
      ax:=0; FileRead(h,ax,loc);
      if _mac then ax:=SwapWord(ax);
      val[i]:=ax; Inc(Result);
      if Result = 32 then Break
    end;

    xFileSeek(h,old)
  end
end;

procedure dump_int(txt: TTextfile;
                   h: THandle;
                   const dr: tiff_dir64;
                   cols: Integer);
var
  i,j,n,loc: int; ax,old: int64;
  vals: TIntegers; s: TShortstr;
begin
  n:=0; loc:=0;
  if dr.typ = 3 then loc:=2 else
  if dr.typ = 4 then loc:=4;

  if loc > 0 then
  if dr.len > 1 then
  if dr.len <= 256 then begin
    old:=xFilePos(h);
    xFileSeek(h,dr.ind);

    for i:=0 to dr.len-1 do begin
      ax:=0; FileRead(h,ax,loc);
      if im_mac then
      case loc of
    2:  ax:=SwapWord(ax);
    4:  ax:=SwapInt(ax);
      end;

      vals[i]:=ax; Inc(n)
    end;

    xFileSeek(h,old);
  end;

  for i:=0 to (n div cols)-1 do begin
    StrCopy(s,#9); for j:=0 to cols-1 do
    pStrInt(s,vals[i*cols+j],12);

    txt.WriteStr(Strpas(s));
  end
end;

procedure dump_real(txt: TTextfile;
                    h: THandle;
                    const dr: tiff_dir64;
                    cols: Integer);
var
  i,j,n: int; old: int64;
  f: Float; d: Double;
  vals: TDoubles; s: TShortstr;
begin
  n:=0;

  if dr.typ in [11,12] then
  if dr.len > 1 then
  if dr.len <= 256 then begin
    old:=xFilePos(h);
    xFileSeek(h,dr.ind);

    for i:=0 to dr.len-1 do begin
      if dr.typ = 12 then begin
        FileRead(h,d,8);
        if im_mac then d:=SwapDouble(d)
      end
      else begin
        FileRead(h,f,4);
        if im_mac then f:=SwapSingle(f);
        d:=f
      end;

      vals[i]:=d; Inc(n)
    end;

    xFileSeek(h,old);
  end;

  for i:=0 to (n div cols)-1 do begin
    StrCopy(s,#9); for j:=0 to cols-1 do
    pStrReal(s,vals[i*cols+j],16,6);
    txt.WriteStr(Strpas(s));
  end
end;

function OpenFile(Path: PChar): int;
var
  s: String;
begin
  Result:=FileOpen(Path,fmOpenRead);
  if Result <= 0 then begin
    s:=Format('OpenFile "%s" INVALID_HANDLE_VALUE',[xStrNameExt(Path)]);
    std_err.WriteStr(s)
  end
end;

function Open_jpeg(Path: PChar; info: PBitmapInfoHeader): Boolean;
type
  TJpegMarker = record
    id,len: Word
  end;

  TJpegFatItem = record
    ofs,len,width,height: DWord
  end;
var
  vm: TReadFile; bmp: Bitmap;
  i,n,pos,pos1,w,h,iz: Longint;
  mk: TJpegMarker; fr: TJpegFatItem;
begin
  Result:=false;

  vm:=TReadFile.Create;
  try
    if im_Zoom > 1 then
    if vm.Open(Path) then
    if vm.Size > 8 then begin

      pos:=vm.Size-8;
      vm.Load(pos,mk,Sizeof(mk));
      mk.id:=SwapWord(mk.id);
      mk.len:=SwapWord(mk.len);

      if mk.id = $FF02 then
      if mk.len = 6 then begin

        vm.Load(pos+4,pos1,4);

        if pos1 > 0 then
        if pos1 <= pos-8 then begin

          vm.Load(pos1,mk,Sizeof(mk));
          mk.id:=SwapWord(mk.id);
          mk.len:=SwapWord(mk.len);
          if mk.id = $FF01 then
          if pos1 + mk.len + 2 = pos then begin
            Inc(pos1,4);

            n:=mk.len div Sizeof(fr);

            for i:=0 to n-1 do begin
              pos1:=vm.Load(pos1,fr,Sizeof(fr));
              if vm.IsJpeg(fr.ofs,fr.len) then

              if i = 0 then begin
                w:=fr.width; h:=fr.height
              end else
              if (w > 0) and (fr.width > 0) then
              if (h > 0) and (fr.height > 0) then begin
                iz:=Round(w/fr.width);
                if iz > im_zoom then Break;

                if Assigned(jpg) then
                if jpg.GetInfo(@vm.Buf[fr.ofs],fr.len,nil,@bmp) then begin

                  info^:=BITMAPINFOHEADER(bmp.bmWidth,
                                          bmp.bmHeight,
                                          bmp.bmBitsPixel);

                  im_Size.X:=w; im_Size.Y:=h;
                  jpg_Pos:=fr.ofs; jpg_len:=fr.len;
                  Result:=true
                end;
              end;
            end;
          end
        end
      end
    end;
  finally
    vm.Free
  end;

  if not Result then
  if jpg_enabled then
  if jpg.GetInfo(nil,0,Path,@bmp) then begin

    info^:=BITMAPINFOHEADER(bmp.bmWidth,
                            bmp.bmHeight,
                            bmp.bmBitsPixel);

    im_Size.X:=info.biWidth;
    im_Size.Y:=info.biHeight;
    Result:=true
  end
end;

function rsw_Open(Path: PChar;
                  info: PBitmapInfoHeader;
                  log: TTextfile): int;

procedure dump_fat(log: TTextfile; h: int);
var
  i,xp,yp: int; p: TTilePos32; offs,len: int64;
begin
  xFileSeek(h,tile.offp);

  i:=0;
  for yp:=0 to tile.YTiles-1 do
  for xp:=0 to tile.XTiles-1 do begin
    FileRead(h,p,Sizeof(p));
    Inc(i); offs:=p.pos; len:=p.len;
    log.WriteStr( Format('[%d,%d]'#9'%d'#9'%d',[xp,yp,offs,len]) );
  end
end;

procedure log_header(const rsw: trswHdr; offp: int64);
var
  ntiles: int;
begin
  with rsw do
  ntiles:=rsw.xtiles * rsw.ytiles;

  log.WriteStr('magic'#9+'0x'+HexToStr(rsw.mag,8));
  log.WriteStr('ver'#9+IntToStr(rsw.ver));

  log.WriteStr('thumb'#9+IntToStr(rsw.thumb));
  log.WriteStr('sit'#9+IntToStr(rsw.sit));

  log.WriteStr('');
  log.WriteStr('width'#9+IntToStr(rsw.width));
  log.WriteStr('height'#9+IntToStr(rsw.height));
  log.WriteStr('bits'#9+IntToStr(rsw.bits));

  log.WriteStr('tilew'#9+IntToStr(rsw.tilew));
  log.WriteStr('tileh'#9+IntToStr(rsw.tileh));

  log.WriteStr('tilew1'#9+IntToStr(rsw.tilew1));
  log.WriteStr('tileh1'#9+IntToStr(rsw.tileh1));

  log.WriteStr('xtiles'#9+IntToStr(rsw.xtiles));
  log.WriteStr('ytiles'#9+IntToStr(rsw.ytiles));
  log.WriteStr('ntiles'#9+IntToStr(ntiles));
  log.WriteStr('comp'#9+IntToStr(rsw.comp));

  log.WriteStr('');
  log.WriteStr('palOfs'#9+IntToStr(rsw.palOfs));
  log.WriteStr('palLen'#9+IntToStr(rsw.palLen));

  log.WriteStr('');
  log.WriteStr('tileOfs'#9+IntToStr(offp));
  log.WriteStr('tileLen'#9+IntToStr(rsw.tileLen));

  log.WriteStr('');
  log.WriteStr('len'#9+IntToStr(rsw.len));
  log.WriteStr('len0'#9+IntToStr(rsw.len0));
  log.WriteStr('len1'#9+IntToStr(rsw.len1));

  log.WriteStr('');
  log.WriteStr('res1'#9+IntToStr(rsw.res1));
  log.WriteStr('res2'#9+IntToStr(rsw.res2));
  log.WriteStr('res3'#9+IntToStr(rsw.res3));
  log.WriteStr('res4'#9+IntToStr(rsw.res4));
  log.WriteStr('res5'#9+IntToStr(rsw.res5));

  log.WriteStr('');
  log.WriteStr('typ'#9+IntToStr(rsw.typ));
  log.WriteStr('prj'#9+IntToStr(rsw.prj));
  log.WriteStr('b1'#9+RealToStr(rsw.b1*180/Pi,4));
  log.WriteStr('b2'#9+RealToStr(rsw.b2*180/Pi,4));
  log.WriteStr('lc'#9+RealToStr(rsw.lc*180/Pi,4));

  log.WriteStr('');
  log.WriteStr('scale'#9+RealToStr(rsw.scale,-1));
  log.WriteStr('ppm'#9+RealToStr(rsw.ppm,6));
  log.WriteStr('mpp'#9+RealToStr(rsw.mpp,6));
  log.WriteStr('xpos'#9+RealToStr(rsw.XPos,3));
  log.WriteStr('ypos'#9+RealToStr(rsw.YPos,3));
end;

var
  h,h1,tk,bx,loop: int; sz: int64;
  rsw,rsw1: trswHdr; fn: TShortstr;
begin
  Result:=0; sz:=fileSize; h1:=0;

  h:=winOpenReadFile(Path);
  if h > 0 then
  if sz > Sizeof(rsw) then begin
    xFileSeek(h,0);
    FileRead(h,rsw,Sizeof(rsw));

    tk:=rsw.xtiles * rsw.ytiles;

    if rsw.mag = rsw_magic then
    if rsw.bits in [1,8,24] then

    if rsw.width > 0 then
    if rsw.height > 0 then

    if rsw.xtiles > 0 then
    if rsw.ytiles > 0 then

    if rsw.tilew > 0 then
    if rsw.tileh > 0 then

    if tk > 0 then
    if rsw.tileOfs > 0 then
    if rsw.tileLen = tk*Sizeof(TPoint) then begin

      tile.offp:=rsw.tileOfs;
      tile.lenp:=rsw.tileOfs;

      tile.Width:=rsw.tilew;
      tile.Height:=rsw.tileh;

      tile.Width1:=rsw.tilew1;
      tile.Height1:=rsw.tileh1;

      tile.XTiles:=rsw.xtiles;
      tile.YTiles:=rsw.ytiles;

      tile.NTiles:=tile.XTiles*tile.YTiles;

      tile.Size:=tif_line(rsw.tilew,rsw.bits) * rsw.tileh;

      tile.bufp:=xAllocPtr(tile.Size*4);
      tile.Size1:=tile.Size*3;

      tile.palOfs:=rsw.palOfs;
      tile.palLen:=rsw.palLen;

      rsw_len0:=rsw.len0;
      rsw_len1:=rsw.len1;

      rsw_comp:=rsw.comp;

      if rsw_len1 > 0 then begin
        StrCopy(fn,Path);
        StrLCat(fn,'.01',255);
        h1:=winOpenReadFile(fn);
      end;

      if h1 > 0 then
      tile.offp:=tile.offp shl 8;

      if Assigned(log) then begin

        if Param_Option('/tif') then
        if log.Make_ext(Path,'.###') then begin

          log_header(rsw,tile.offp);

          rsw1:=rsw;
          for loop:=1 to 8 do
          if rsw1.thumb > 0 then begin
            if rsw1.thumb+Sizeof(rsw1) > sz then Break;
            xFileSeek(h,rsw1.thumb);
            FileRead(h,rsw1,Sizeof(rsw1));

            log.WriteStr('');
            log.WriteStr('[thumb'+IntToStr(loop)+']');
            log_header(rsw1,0);
          end;

          if Param_option('/fat') then begin
            log.WriteStr(''); dump_fat(log,h)
          end
        end
      end
    end;

    with im_info do begin
      biWidth      :=rsw.width;
      biHeight     :=rsw.height;
      biPlanes     :=1;
      biBitCount   :=rsw.bits;
      biCompression:=bi_RGB;

      if rsw.comp = 2 then
      biCompression:=bi_JPG;

      biSize:=img_Line(biWidth,biBitCount);
    end;

    tiff.bits:=im_info.biBitCount;

    if Assigned(tile.bufp) then begin
      info^:=im_info; im_h:=h; im_h1:=h1;
      im_what:=im_RSW; im_inv:=true;
      Result:=h
    end
  end;

  if Result = 0 then begin
    if h > 0 then FileClose(h);
    if h1 > 0 then FileClose(h1);
  end
end;

function ozf_Open(Path: PChar;
                  info: PBitmapInfoHeader): int;
var
  bits: int;
begin
  Result:=0;

  if ozf.Open(Path) then begin

    im_Size:=ozf.size;

    if im_zoom > 1 then
    ozf.setZoom(im_zoom);

    tile.Width:=ozf.tileWidth;
    tile.Height:=ozf.tileWidth;

    tile.XTiles:=ozf.xtiles;
    tile.YTiles:=ozf.ytiles;

    tile.NTiles:=tile.XTiles*tile.YTiles;

    bits:=ozf.bits;
    if bits <> 8 then bits:=24;
    tile.Size:=tif_line(tile.Width,bits) * tile.Height;

    tile.bufp:=xAllocPtr(tile.Size*4);
    tile.Size1:=tile.Size*3;

    with im_info do begin
      biWidth      :=ozf.width;
      biHeight     :=ozf.height;
      biPlanes     :=1;
      biBitCount   :=bits;
      biCompression:=bi_RGB;
      biSize:=img_Line(biWidth,biBitCount);
    end;

    tiff.bits:=im_info.biBitCount;

    if Assigned(tile.bufp) then begin
      info^:=im_info;
      im_what:=im_OZF;
      Result:=1
    end
  end
end;

procedure dump_strings(h: Integer;
                       const dr: tiff_dir64;
                       txt: TTextfile);
var
  p,q: PChar; s: TCmdStr; t: TShortstr;
begin
  p:=s;
  if Read_str(h,dr,s,Sizeof(s)-1) <> nil then
  while p[0] <> #0 do begin
    q:=StrScan(p,#10);
    if q = nil then q:=StrScan(p,'|');
    if Assigned(q) then q[0]:=#0;

    StrLCopy(t,p,200);
    txt.WriteStr(#9+Strpas(t));

    if q = nil then Break;
    p:=@q[1]
  end
end;

procedure Append_svalue(h: Integer;
                        const dr: tiff_dir64;
                        Str: PChar);
var
  s: TShortstr;
begin
  if Read_str(h,dr,s,Sizeof(s)-1) <> nil then
  if Strlen(s) > 0 then begin
    StrLCat(Str,' "',255);
    StrLCat(Str,s,255);
    StrLCat(Str,'"',255);
  end
end;

procedure dump_tiles(Dest: PChar; h: THandle);
var
  txt: TTextfile;
  ip,jp,len,_h: int;
  pos: int64;
begin
  _h:=im_h; im_h:=h;

  txt:=TTextfile.Create;
  try
    if txt.Make_ext(Dest,'.$$$') then
    for jp:=0 to tile.YTiles-1 do begin
      txt.WriteStr(Format('[row=%d]',[jp]));

      for ip:=0 to tile.XTiles-1 do begin
        pos:=Tile_pos(ip,jp);
        len:=Tile_len(ip,jp);
        txt.WriteStr(Format('%d %d',[pos,len]));
      end;

    end;
  finally
    txt.Free
  end;

  im_h:=_h
end;

procedure dump_strips(Dest: PChar; h: THandle);
var
  txt: TTextfile;
  i,len,_h: int;
  pos: int64;
begin
  _h:=im_h; im_h:=h;

  txt:=TTextfile.Create;
  try
    if txt.Make_ext(Dest,'.$$$') then
    for i:=0 to tiff.offLen-1 do begin
      pos:=Read_ind(tiff.offp,tiff.offTyp,i);
      len:=Read_ind(tiff.lenp,tiff.lenTyp,i);
      txt.WriteStr(Format('%d: %d %d',[i,pos,len]));
    end;
  finally
    txt.Free
  end;

  im_h:=_h
end;

type
  TSamples = Array[0..31] of int;

var
  txt: TTextfile; ilevel: int;
  h,i,bits,zoom,pos,rc,mag,iw,ih,cx: int;
  dir_top,sz,cnt,hdr_top: Int64; c: TRational;

  hdr: tiff_hdr; dir: tiff_dir64;
  dir_: tiff_dir32;

  tmp: TVGABITMAPINFO; kz,t: Double;
  val: TSamples; valf: RValues;
  img: Longbool; str,ext: TShortstr;
  rlev: TRlzPyrRec;
begin
  Result:=0; Free; img:=false;

  FillChar(info^,SizeOf(TBitMapInfoHeader),0);
  im_Size.X:=0; im_Size.Y:=0; jpg_zoom:=1;

  fileSize:=winFileSize(Path);
  jpg_pos:=0; jpg_len:=fileSize;
  mag:=Get_mag2(Path); tif_key:=0;
  im_pack:=false; im_float:=false;
  is_dem:=false; tiff.Pack:=1;

  SMinSampleValue:=1;
  SMaxSampleValue:=0;

  jpg_props.IsValid:=false;

  StrPCopy(ext,ExtractFileExt(StrPas(Path)));

  txt:=TTextfile.Create;
  try
    if (mag = $4949) or (mag = $4D4D)
    or (StrIComp(ext,'.TIF') = 0) then begin

      if Param_Option('/tif') then
      if txt.Make_ext(Path,'.###') then begin
        StrPCopy(str,'$'+HexToStr(mag,4));
        if mag = $4949 then StrCat(str,' x86')
                       else StrCat(str,' mac');
        txt.WriteStr(Strpas(str));
      end;

      sz:=fileSize;
      h:=winOpenReadFile(Path);

      if h > 0 then
      if sz > 64 then begin

        FileSeek(h,0,0);
        FileRead(h,hdr,8);

        tiff.rows:=1;
        tiff.PlanarConfig:=1;
        tiff.Planes:=1;

        im_mac:=false;
        if hdr.cod = $4D4D then begin
          hdr.cod:=$4949; im_mac:=true;
          hdr.ver:=SwapWord(hdr.ver);
          hdr.ind:=SwapInt(hdr.ind)
        end;

        im_big:=false;
        dir_top:=hdr.ind;
        hdr_top:=8;

        if hdr.ver = $2B then
        if hdr.ind <> 8 then
          hdr.cod:=0
        else begin
          FileRead(h,dir_top,8);
          if im_mac then
          dir_top:=SwapInt64(dir_top);
          im_big:=true; hdr_top:=16;
        end;

        if hdr.cod = $4949 then
        if dir_top < sz then begin

          ilevel:=0; bits:=0;
          FillChar(im_info,SizeOf(im_info),0);
          im_info.biPlanes:=1; im_info.biBitCount:=1;

          if txt.Active then begin

            StrFmt(str,'ver=%s ',[HexToStr(hdr.ver,2)]);
            if im_big then StrCat(str,'[big]');

            txt.WriteStr(Strpas(str));

            txt.WriteStr('');
            txt.WriteStr('                tag   typ   len      ind');
          end;

          while dir_top > 0 do begin
            xFileSeek(h,dir_top);
            dir_pos:=dir_top;

            cnt:=0;
            if im_big then begin
              FileRead(h,cnt,8);
              if im_mac then
              cnt:=SwapInt64(cnt);
              Inc(dir_pos,8)
            end
            else begin
              FileRead(h,cnt,2);
              if im_mac then
              cnt:=SwapWord(cnt);
              Inc(dir_pos,2)
            end;

            if cnt > 256 then Break;
            while cnt > 0 do with dir do begin

              if im_big then begin
                FileRead(h,dir,20);
                Inc(dir_pos,20);

                if im_mac then begin
                  dir.tag:=SwapWord(dir.tag);
                  dir.typ:=SwapWord(dir.typ);
                  dir.len:=SwapInt64(dir.len);
                  dir.ind:=SwapInt64(dir.ind);
                end
              end
              else begin
                FileRead(h,dir_,12);
                Inc(dir_pos,12);

                if im_mac then begin
                  dir_.tag:=SwapWord(dir_.tag);
                  dir_.typ:=SwapWord(dir_.typ);

                  dir_.len:=SwapInt(dir_.len);

                  if (dir_.typ = 3) and (dir_.len = 1) then
                    dir_.ind:=SwapWord(dir_.ind)
                  else
                    dir_.ind:=SwapInt(dir_.ind);
                end;

                dir.tag:=dir_.tag;
                dir.typ:=dir_.typ;
                dir.len:=dir_.len;
                dir.ind:=dir_.ind;
              end;

              Dec(cnt); rc:=0;

              if typ = 5 then begin
                Read_buf(h,ind,c,8);

                if im_mac then begin
                  c.c1:=SwapInt(c.c1);
                  c.c2:=SwapInt(c.c2);
                end;

                if c.c2 <> 0 then
                ind:=c.c1 div c.c2
              end;

              with im_info do case tag of

            0:  begin
                  dir_top:=0; Break;
                end;  

            256:if biWidth > 0 then begin   {ImageWidth}

                  if (ind = 0)
                  or (ind/biWidth > 0.75) then begin
                    dir_top:=0; Break
                  end
                  else begin
                    zoom:=Round(biWidth/ind);

                    if zoom > im_Zoom then begin
                      dir_top:=0; Break
                    end
                    else begin
                      biWidth:=ind; im_Zoom:=im_Zoom div zoom;
                      Inc(ilevel)
                    end
                  end

                end
                else begin
                  biWidth:=ind;
                  im_Size.X:=ind;
                  ilevel:=1
                end;

            257:begin                       {ImageHeight}
                  if biHeight = 0 then
                  im_Size.Y:=ind; biHeight:=ind;
                end;

            258:if len in [1,2] then begin  {BitsPerSample}
                  ind:=ind and $FFFF; biBitCount:=ind
                end else
                if im_big and (typ = 3) and (len = 3) then begin

                  val[0]:=TINt64(ind).w[0];
                  val[1]:=TINt64(ind).w[1];
                  val[2]:=TINt64(ind).w[2];

                  biBitCount:=val[0]; bits:=0;
                  for i:=0 to rc-1 do Inc(bits,val[i]);
                end else

                if len in [3..32] then begin
                  rc:=Read_int(h,ind,typ,len,@val,im_mac);

                  biBitCount:=val[0]; bits:=0;
                  for i:=0 to rc-1 do Inc(bits,val[i])
                end;

            259:begin                       {Compression}
                  ind:=ind and $FFFF;
                  if ind  = 1 then
                    biCompression:=bi_RGB
                  else
                  if ind = 3 then begin
                    if Hoffman.Count > 0 then
                      biCompression:=bi_GR3
                    else biCompression:=bi_ERR
                  end else
                  if ind = 4 then begin
                    if Hoffman.Count > 0 then
                      biCompression:=bi_GR4
                    else biCompression:=bi_ERR
                  end else
                  if ind = 5 then
                    biCompression:=bi_LZW
                  else
                  if ind = 7 then
                    biCompression:=bi_JPG
                  else
                  if ind = 8 then
                    biCompression:=bi_ZIP
                  else
                  if ind = 32773 then
                    biCompression:=bi_RLC
                  else
                  if ind = 32946 then
                    biCompression:=bi_ZIP
                  else
                  if ind = comp_JPF then
                    biCompression:=bi_JPG
                  else
                  if ind = comp_ZLB then
                    biCompression:=bi_ZIP
                  else
                  if ind = comp_ecw then begin
                    biCompression:=bi_ERR;
                    if ecw_init then
                    biCompression:=bi_ECW
                  end else
                    biCompression:=bi_ERR;
                end;

            262:begin
                  tiff.Photometric:=ind;
                  im_cmyk:=ind = 5;
                  im_inv:=ind = 1
                end;

            273:                            {StripOffset}
                if len <= 1 then
                  bit_pos:=ind
                else begin
                  tiff.offp:=ind;
                  tiff.offTyp:=typ;
                  tiff.offLen:=len
                end;

            277:biPlanes:=ind;              {SamplesPerPixel}
            278:tiff.rows:=ind;             {RowsPerStrips}

            279:                            {StripByteCount}
                if len <= 1 then
                  bit_size:=ind
                else begin
                  tiff.lenp:=ind;
                  tiff.lenTyp:=typ;
                  tiff.lenLen:=len
                end;

            280:if len in [1,2] then        {MinSampleValue}
                  tiff.MinValue:=ind 
                else begin
                  rc:=Read_int(h,ind,typ,len,@val,im_mac);
                  tiff.MinValue:=val[0]
                end;

            281:if len in [1,2] then        {MaxSampleValue}
                  tiff.MaxValue:=ind
                else begin
                  rc:=Read_int(h,ind,typ,len,@val,im_mac);
                  tiff.MaxValue:=val[0]
                end;

            282:biXPelsPerMeter:=ind;
            283:biYPelsPerMeter:=ind;

            284:tiff.PlanarConfig:=ind;

            296:im_pred:=ind = 3;

            317:im_hdif:=ind = 2;

            320:begin
                  rgb_pos:=ind; rgb_len:=len;
                  if typ = 3 then rgb_len:=len*2;
                end;

            322:tile.Width :=ind;           {TileWidth}
            323:tile.Height:=ind;           {TileLength}

            324:begin                       {TileOffset}
                  tile.offp:=ind;
                  tile.offTyp:=typ;
                  tif_rgb:=typ in [4,16];
                end;

            325:begin                       {TileByteCount}
                  tile.lenp:=ind;
                  tile.lenTyp:=typ
                end;

            336:cmyk_dot:=ind; 

            339:if typ = 3 then begin

                  tiff.SampleFormat:=ind;

                  if (len = 1) and (ind < 10) then
                    im_float:=ind = 3
                  else
                  if len = biPlanes then begin
                    rc:=Read_int(h,ind,typ,len,@val,im_mac);
                    if rc > 0 then
                    if val[0] = 3 then
                      im_float:=true
                  end
                end;

            340:if typ = 11 then
                if len = 1 then begin
                  valf[0]:=pfloat(@ind)^;
                  SMinSampleValue:=valf[0];
                  rc:=1
                end;

            341:if typ = 11 then
                if len = 1 then begin
                  valf[0]:=pfloat(@ind)^;
                  SMaxSampleValue:=valf[0];
                  rc:=1
                end;

            347:begin
                  jpg_props.isValid:=false;
                  jpg_props_pos:=ind;
                  jpg_props_len:=len;
                end;

            530:if typ = 3 then             {YCbCrSubSampling}
                if len = 2 then begin
                  val[0]:=TInt64(ind).w[0];
                  val[1]:=TInt64(ind).w[1];
                  rc:=2;
                end;

            900:if typ = 4 then
                if len = 1 then
                ClearColor:=ind;

            901:if typ = 1 then
                if len = 16 then
                Hex_pos:=ind;

            999:if typ = 4 then
                if len = 1 then
                if ind = 1 then
                if not im_big then
                tif_key:=cid_key;
              end;

              if txt.Active then begin
                case tag of
              256:StrCopy(str,'ImageWidth       ');
              257:StrCopy(str,'ImageLength      ');
              258:StrCopy(str,'BitsPerSample    ');
              259:StrCopy(str,'Compression      ');
              262:StrCopy(str,'Photometric      ');
              270:StrCopy(str,'ImageDescription ');
              273:StrCopy(str,'StripOffset      ');
              274:StrCopy(str,'Orientation      ');
              277:StrCopy(str,'SamplesPerPixel  ');
              278:StrCopy(str,'RowsPerStrips    ');
              279:StrCopy(str,'StripByteCount   ');
              280:StrCopy(str,'MinSampleValue   ');
              281:StrCopy(str,'MaxSampleValue   ');
              282:StrCopy(str,'XResolution      ');
              283:StrCopy(str,'YResolution      ');
              284:StrCopy(str,'PlanarConfig     ');
              293:StrCopy(str,'Group4           ');
              296:StrCopy(str,'ResolutionUnit   ');
              317:StrCopy(str,'Diff Predicator  ');
              320:StrCopy(str,'RGB              ');
              322:StrCopy(str,'TileWidth        ');
              323:StrCopy(str,'TileHeight       ');
              324:StrCopy(str,'TileOffset       ');
              325:StrCopy(str,'TileByteCount    ');
              347:StrCopy(str,'jpegTables       ');

              339:StrCopy(str,'SampleFormat     ');
              340:StrCopy(str,'SMinSampleValue  ');
              341:StrCopy(str,'SMaxSampleValue  ');

              530:StrCopy(str,'YCbCrSubSampling ');

              900:StrCopy(str,'ClearColor       ');
              901:StrCopy(str,'hex_to           ');
              999:StrCopy(str,'crypt            ');

              33550:
                  StrCopy(str,'ModelPixelScale  ');

              33922:
                  StrCopy(str,'ModelTiepoint    ');

              34735:
                  StrCopy(str,'GeoKeyDirectory  ');
              34736:
                  StrCopy(str,'GeoDoubleParams  ');
              34737:
                  StrCopy(str,'GeoAsciiParams   ');

              $A0A0:
                  StrCopy(str,'Quality          ');

              else
                  StrCopy(str,'                 ');
                end;

                pStrInt(str,tag,6);
                pStrInt(str,typ,6);
                pStrInt64(str,len,10);
                pStrInt64(str,ind,12);

                if rc > 0 then
                if typ = 11 then begin
                  if rc > 4 then rc:=4;
                  for i:=0 to rc-1 do
                  pStrReal(str,valf[i],8,1)
                end else

                if rc in [2,3,4] then begin
                  for i:=0 to rc-1 do
                  pStrInt(str,val[i],6)
                end else
                for i:=0 to rc-1 do begin
                  pStrInt(str,val[i],6);
                  if i = 7 then begin
                    StrCat(str,'...'); Break
                  end
                end;

                txt.WriteStr(Strpas(str));

                case tag of
              34735:
                  if typ = 3 then
                  dump_int(txt, h,dir,4);
              34736:
                  dump_real(txt, h,dir,1);
              34737:
                  dump_strings(h,dir,txt);
              33550:
                  dump_real(txt, h,dir,3);
              33922:
                  dump_real(txt, h,dir,3);
              else
                  case typ of
                2:  if Read_str(h,dir,str,127) <> nil then
                    txt.WriteStr(#9'"'+Strpas(str)+'"');
                  end  
                end
              end;
            end;

            if dir_top = 0 then Break; 

            dir_top:=0; cx:=4;
            if im_big then cx:=8;

            if FileRead(h,dir_top,cx) < cx then
            dir_top:=0;

            if dir_top < hdr_top then
            dir_top:=0;

            if txt.Active then
            if dir_top > 0 then
            txt.WriteStr('')
          end;

          if txt.Active then begin
            txt.WriteStr('');
            with im_info do begin
              txt.WriteStr('biWidth       '+IntToStr(biWidth));
              txt.WriteStr('biHeight      '+IntToStr(biHeight));
              txt.WriteStr('biPlanes      '+IntToStr(biPlanes));
              txt.WriteStr('biBitCount    '+IntToStr(biBitCount));
              txt.WriteStr('biCompression '+IntToStr(biCompression));
            end; txt.WriteStr('');
          end
        end;
                          
        tiff.Planes:=Max(1,im_info.biPlanes);

        with im_info do
        if biPlanes > 0 then

        if biPlanes = 1 then begin
          if bits = 24 then
          if tiff.Photometric = 2 then
          biBitCount:=bits
        end else

        if biPlanes > 3 then begin
          tiff.Planes:=bits div biBitCount;
          biBitCount:=bits; biPlanes:=1
        end
        else begin
          if (biPlanes = 2) and (biBitCount = 8) then
          im_alfa:=true else biBitCount:=im_Bits;
          biPlanes:=1
        end;

        with im_info do begin
          bits:=im_Bits; if im_alfa then Inc(bits,bits);
          biSize:=(biWidth * bits + 7) div 8
        end;

        if im_cmyk then im_cmyk:=im_Bits = 32;

        bits:=im_Bits; rc:=bits div 8;
        if (bits in [1,2,4]) or (bits = rc*8) then
        else begin
          if txt.Active then
          txt.WriteStr('Bits error: '+IntToStr(im_Bits));
          im_info.biSize:=0;
        end;

        if im_info.biCompression = bi_ERR then begin
          if txt.Active then
          txt.WriteStr('Unknown compression.');
          im_info.biSize:=0;
        end;

        if tiff.offp > 0 then

        if tiff.lenp = 0 then begin
          if txt.Active then
          txt.WriteStr('StripByteCount error.');
          im_info.biSize:=0;
        end else
        if (tiff.rows < 1)
        or (tiff.rows > im_info.biHeight) then begin
          if txt.Active then
          txt.WriteStr('RowsPerStrips error.');
          im_info.biSize:=0;
        end;

        if (tile.offp > 0) and (tile.lenp = 0) then begin
          if txt.Active then
          txt.WriteStr('TileByteCount error.');
          im_info.biSize:=0;
        end;

        if (bit_pos = 0) and (tiff.offp = 0)
        and (tile.offp = 0) then begin
          if txt.Active then
          txt.WriteStr('Data offset error.');
          im_info.biSize:=0;
        end;

        if im_info.biSize > 0 then
        with tiff do if offp > 0 then begin

          with im_info do
          if biCompression = bi_JPG then
          biSize:=img_Line(biWidth,im_Bits);

          size:=im_info.biSize * rows;

          if PlanarConfig = 2 then
            bufp:=xAllocPtr(size + size)
          else
            bufp:=xAllocPtr(size);

          Size1:=size*2; // for reading from [big] files stripe

          if bufp = nil then im_info.biSize:=0;
          bufy:=im_info.biHeight;

          if debug_enabled then
          if txt.Active then
          dump_strips(Path,h)
        end;

        with im_info do if biSize > 0 then
        if biCompression in bi_ARJ then

        if (bit_pos > 0) and (bit_size > 0) then
        with tiff do if bufp = nil then begin
          rows:=biHeight; bufy:=biHeight;
          size:=biSize * rows; bufp:=xAllocPtr(size);
        end;

        with im_info,tiff do
        if biCompression in [bi_GR3,bi_GR4] then begin
          if im_Bits <> 1 then biSize:=0;
          if bufp = nil then biSize:=0;
        end;

        tiff.line:=im_info.biSize;
        tiff.bits:=im_info.biBitCount;

        with im_info do
        if biPlanes = 1 then
        if biBitCount > 8 then
        if biBitCount <> 24 then

        if biBitCount = 32 then begin
          if im_float then biBitCount:=8
                      else biBitCount:=24;

          biSize:=tif_Line(biWidth,biBitCount);
          im_pack:=true
        end else
        if biCompression <> bi_RGB then
          biSize:=0
        else begin
          bits:=biBitCount;

          if tiff.Planes >= 3 then
          if tiff.Photometric = 2 then
          if bits = 64 then begin
            biBitCount:=24; tiff.Pack:=2;
            biSize:=img_Line(biWidth,24);
            im_pack:=true
          end;

          if biBitCount = bits then begin

            if biBitCount in [16,48] then begin
              biBitCount:=biBitCount div 2;
              tiff.Pack:=2
            end
            else begin
              tiff.Pack:=biBitCount div 8;
              biBitCount:=8;
            end;

            biSize:=biSize div tiff.Pack;
            im_pack:=true
          end
        end;

        if im_info.biSize = 0 then
          FileClose(h)
        else begin

          with tile do
          if (Width > 0) and (Height > 0) then
          if (offp > 0) and (lenp > 0) then begin
            Size:=Width * Height * tiff.bits div 8;
            XTiles:=(im_Info.biWidth+Width-1) div Width;
            YTiles:=(im_Info.biHeight+Height-1) div Height;
            NTiles:=YTiles*XTiles;

            if txt.Active then begin
              t:=fileSize / NTiles;
              txt.WriteStr('tileSize: '+RealToStr(t/1024,1)+'Kb');
            end;

            if debug_enabled then
            if txt.Active then
            if ilevel > 1 then
            dump_tiles(Path,h)
          end;

          info^:=im_info; im_h:=h;
          im_what:=im_TIF;

          with tile do
          if im_Flags and img_tiled <> 0 then begin
            bufp:=xAllocPtr(Size*4); Size1:=Size*3
          end;

          if tile.bufp = nil then
          if im_info.biCompression in bi_ARJ then
          if (tiff.bufp = nil) and (tile.bufp = nil) then
          im_info.biSize:=0;

          if tiff.bits = 16 then
          if tiff.SampleFormat = 2 then is_dem:=true
          else is_dem:=StrPos(Path,'ASTGTM2') <> nil;

          if im_info.biSize = 0 then
            FileClose(h)
          else
            Result:=h;
        end
      end
    end else

    if StrIComp(ext,'.JPG') = 0 then begin

      if only_read then
      if Open_jpeg(Path,@im_info) then begin

        with im_info do
        jpg_Zoom:=Round(im_Zoom*biWidth/im_Size.X);

        if jpg_Zoom < 1 then jpg_Zoom:=1;
        if jpg_Zoom > 8 then jpg_Zoom:=8;

        with im_info do begin
          biWidth:=biWidth div jpg_Zoom;
          biHeight:=biHeight div jpg_Zoom;
          biCompression:=bi_JPG;
        end;

        with tile do begin
          Width:=256; Height:=256;
          Size:=img_Line(Width,im_Bits)* Height;
          XTiles:=int_Tiles(im_info.biWidth,Width);
          YTiles:=int_Tiles(im_info.biHeight,Height);
          NTiles:=XTiles * YTiles
        end;

        im_what:=im_JPG; info^:=im_info;
        im_h:=OpenFile(Path); Result:=im_h
      end

    end else

    if (StrIComp(ext,'.SID') = 0)
    or (StrIComp(ext,'.JP2') = 0) then begin

      if sid.Open(Path) then begin

        im_Size.X:=sid.Width;
        im_Size.Y:=sid.Height;

        sid.ReadMag:=1 / im_Zoom;

        im_info.biWidth:=sid.ReadSize.X;
        im_info.biHeight:=sid.ReadSize.Y;
        im_info.biPlanes:=1;
        im_info.biBitCount:=sid.OutBits;
        im_info.biCompression:=bi_SID;
        im_info.biSize:=sid.ReadSize.X;

        with tile do begin
          Width:=sid.TileSize;
          Height:=sid.TileSize;

          XTiles:=sid.Tiles.X;
          YTiles:=sid.Tiles.Y;
          NTiles:=XTiles*YTiles;

          Size:=img_Size(Width,Height,im_info.biBitCount);
        end;

        info^:=im_info;
        im_what:=im_SID;
        Result:=1
      end
    end else

    if StrIComp(ext,'.ECW') = 0 then begin

      if ecw_Init then
      if only_read then
      if NCScbmOpenFileView(Path,ecw,nil) = 0 then
      if ecwBitmapInfo(ecw,@im_info) then begin

        with im_info do
        biSize:=img_Line(biWidth,biBitCount);

        if im_info.biBitCount = 32 then begin
          tiff.bits:=32; im_pack:=true;
          im_info.biBitCount:=24
        end;

        im_info.biCompression:=bi_ECW;
        im_what:=im_ECW; info^:=im_info;
        Result:=1

      end

    end else

    if StrIComp(ext,'.RLZ') = 0 then begin

      h:=OpenFile(Path);

      if h > 0 then
      if not this_rlz(h,rlz) then begin
        std_err.WriteStr('this_rlz: false')
      end
      else begin
        with tile do begin
          offp:=rlz.offp; lenp:=rlz.lenp;

          Width:=rlz.tile_nx and $FFFE;
          Height:=rlz.tile_ny and $FFFE;

          XTiles:=rlz.XTiles; YTiles:=rlz.YTiles;
          NTiles:=XTiles*YTiles;

          with rlz do
          Size:=tile_nx * tile_ny * 2;

          bufp:=xAllocPtr(Size*4);
          Size1:=Size*3
        end;

        rel.nx:=tile.Width * tile.XTiles + 1;
        rel.ny:=tile.Height * tile.YTiles + 1;

        im_Size.X:=rel.nx;
        im_Size.Y:=rel.ny;

        if im_Zoom > 1 then
        if rlz.m_offp > 0 then
        if get_rlz_pyr(h,im_Zoom,rlz,rlev) then begin
          rel.nx:=rlev.Width;
          rel.ny:=rlev.Height;

          tile.offp:=rlev.offp;
          tile.lenp:=rlev.lenp;

          tile.XTiles:=rlev.XTiles;
          tile.YTiles:=rlev.YTiles;
          tile.NTiles:=tile.XTiles*tile.YTiles;
        end;

        rel.zmin:=rlz.zmin;
        rel.zmax:=Max(1,rlz.zmax-rlz.zmin);

        with im_info do begin
          biSize       :=rel.nx;
          biWidth      :=rel.nx;
          biHeight     :=rel.ny;
          biPlanes     :=1;
          biBitCount   :=8;
          biCompression:=bi_ZIP;
        end;

        if tile.bufp <> nil then begin
          info^:=im_info; im_h:=h;
          im_what:=im_RLZ; Result:=h
        end
      end;

      if Result = 0 then
      FileClose(h)
    end else

    if StrIComp(ext,'.RSW') = 0 then
      Result:=rsw_Open(Path,info,txt)
    else
    if StrIComp(ext,'.ozf2') = 0 then
      Result:=ozf_Open(Path,info)
    else
    if StrIComp(ext,'.XML') = 0 then begin

      if GetWmsInterface(Path,ITileImage,wms) = S_OK then
      if wms.GetInfo(@val) = 5 then begin

        im_Size.X:=val[0];
        im_Size.Y:=val[1];

        kz:=wms.Zoom(im_Zoom);
        iw:=Round(val[0] / kz);
        ih:=Round(val[1] / kz);

        im_info.biWidth:=iw;
        im_info.biHeight:=ih;
        im_info.biPlanes:=1;
        im_info.biBitCount:=val[2];
        im_info.biCompression:=bi_WMS;

        im_info.biSize:=iw;
        with tile do begin
          Width:=val[3];
          Height:=val[4];

          XTiles:=int_Tiles(iw,Width);
          YTiles:=int_Tiles(ih,Height);
          NTiles:=XTiles*YTiles;

          Size:=img_Size(Width,Height,im_info.biBitCount);
        end;

        info^:=im_info;
        im_what:=im_WMS;
        Result:=1
      end
    end else

    if StrIComp(ext,'.RGB') = 0 then begin

      StrCopy(imagePath,'');
      if Image.Load(Path,false) then begin
        StrLCopy(imagePath,Path,Sizeof(imagePath)-1);

        Image.Get_BitMapInfo(@tmp);
        im_info:=tmp.bmiHeader;

        im_info.biCompression:=bi_RGB;

        im_what:=im_RGB; info^:=im_info;
        img:=true; Result:=1
      end

    end else

    if StrIComp(ext,'.EPS') = 0 then begin

      if eps_load(Path) then begin
        Image.Get_BitMapInfo(@tmp);
        im_info:=tmp.bmiHeader;

        im_info.biCompression:=bi_RGB;

        im_what:=im_RGB; info^:=im_info;
        img:=true; Result:=1
      end

    end else

    if (StrIComp(ext,'.REL') = 0)
    or (StrIComp(ext,'.MRF') = 0) then begin
      h:=OpenFile(Path);

      if this_rlf(h,rel) then begin

        rel.zmax:=Max(1,rel.zmax-rel.zmin);

        bit_pos:=rel.data;
        with im_info do begin
          biSize       :=rel.nx;
          biWidth      :=rel.nx;
          biHeight     :=rel.ny;
          biPlanes     :=1;
          biBitCount   :=8;
          biCompression:=bi_REL;
        end;

        info^:=im_info; im_h:=h;
        im_what:=im_REL; Result:=h
      end;

      if Result = 0 then
      FileClose(h)
    end;

    if Result > 0 then begin
      if tiff.line = 0 then
      tiff.line:=im_info.biSize;

      if im_Size.X = 0 then begin
        im_Size.X:=im_info.biWidth;
        im_Size.Y:=im_info.biHeight;
      end;

      img_Get_icb(Path,@im_icb,false);
      StrCopy(im_Path,Path);
    end;
  finally
    txt.Free
  end;

  if not img then Image.im_Close
end;

function im_Open(Path: PChar; info: PBitMapInfoHeader): HResult;

procedure fillOffset;
var
  bx,dx,hi: int64; i,n: int; ip: PIntegers;
begin
  xFreePtr(Offset);

  bx:=tiff.offp;
  n:=tiff.offLen;

  if bx+n*4 <= fileSize then begin

    Offset:=xAllocInt64(n);
    if Assigned(Offset) then begin

      ip:=Pointer(Offset);
      ip:=@ip[n];

      xFileSeek(im_h,bx);
      FileRead(im_h,ip^,n*4);

      for i:=0 to n-1 do
      Offset[i]:=uint(ip[i]);

      if n > 1 then
      if Offset[0] > 0 then
      if Offset[1] > 0 then
      if Offset[0] < Offset[1] then begin

        hi:=0; bx:=0;
        for i:=0 to n-1 do begin
          dx:=Offset[i];
          if dx > 0 then begin
            Inc(dx,hi);
            if dx < bx then begin
              hi:=hi+$100000000;
              Inc(dx,$100000000);
            end;

            Offset[i]:=dx; bx:=dx
          end
        end

      end
    end
  end
end;

procedure test_jpeg;
var
  ip,jp: int; pos: int64; cx: DWord;
  si: Pointer; fn: TShortstr;
begin
  ip:=tile.XTiles div 2;
  jp:=tile.YTiles div 2;
  pos:=Tile_pos(ip,jp);
  cx:=tile_len(ip,jp);
  if (pos > 0) and (cx > 0) then begin
    si:=Seek_tile(pos,cx);
    if Assigned(si) then begin
      StrWorkPath(fn,'test.jpg');
      xFileDump(fn,si^,cx)
    end
  end
end;

var
  fh: int;
begin
  Result:=S_FALSE;

  fm_Zoom:=im_Zoom;
  fh:=tif_Open(Path,info);

  im_Pos:=0; if fh > 0 then begin

    if im_h > 0 then begin

      if im_What = im_JPG then
        im_View:=FileMemoryReadH(im_h)
      else
      if only_read and (im_h1 = 0) then
      if fileSize < 128 * 1024 * 1024 then begin
        im_View:=FileMemoryReadH(im_h);
        kfa_normal(info);
      end;

      if Assigned(im_View.ptr) then begin
        FileClose(im_h); im_h:=0
      end;

      if tiff.offp > 0 then begin

        if tiff.offTyp = 4 then
        if fileSize > $FFFFFFF0 then
        fillOffset;

        if Param_option('/tif1') then
        dump_offs(Path);
      end;

      if im_h > 0 then
      FileSeek(im_h,0,0);

      if test_enabled then
      if info.biBitCount = 24 then
      if info.biCompression = bi_JPG then
      if tile.NTiles > 0 then test_jpeg;
    end;

    Result:=S_OK
  end;
end;

function im_Stat(Path: PChar; info: PBitMapInfoHeader): Integer;
begin
  im_Zoom:=64;
  Result:=im_Open(Path,info);
  im_Zoom:=1
end;

procedure im_Done; begin Free end;
procedure im_Free; begin Free end;

procedure im_ClearColor(Color: Integer); stdcall;
begin
  ClearColor:=Color
end;

function im_Bmp(out Offset: Integer): PBytes; stdcall;
var
  sz: uint;
begin
  Result:=nil; Offset:=0;

  if Assigned(im_View.ptr) then
  if bit_pos > 0 then

  if not im_pack then
  if not im_cmyk then

  if im_info.biCompression = bi_RGB then begin

    sz:=im_info.biSize * im_info.biHeight;
    if sz <= im_View.size then begin
      Offset:=bit_pos; Result:=@im_View.ptr[bit_pos]
    end
  end
end;

function im_Palette(Quads: PRGBQuads): Integer;

function tif_palette(Quads: PRGBQuads; Count: int): int;
var
  i,bits,used: int; shift: Longbool;
  tmp: array[0..256*3] of word;
begin
  used:=rgb_len div 6;
  Result:=Count;

  if used >= 2 then
  if used <= Count then begin

    FillChar(Quads^,SizeOf(TRGBQuads),0);
    im_Read(@tmp,rgb_pos,used*6);

    shift:=false; for i:=0 to used*3-1 do
    if tmp[i] > 255 then begin
      shift:=true; Break
    end;

    if shift then for i:=0 to used*3-1 do
    tmp[i]:=tmp[i] shr 8;

    for i:=0 to used-1 do with Quads[i] do begin
      rgbRed:=tmp[i]; rgbGreen:=tmp[used+i];
      rgbBlue:=tmp[used+used+i]
    end;

    Result:=used
  end
end;

function rsw_palette(Quads: PRGBQuads; Count: int): int;
var
  used: int; pal: TColors;
begin
  Result:=Count;
  used:=tile.palLen div 4;

  if used >= 2 then begin
    if used > Result then used:=Result;
    im_Read(@pal,tile.palOfs,used*4);

    if used = 2 then
    if pal[0] = pal[1] then begin
      pal[0]:=0; pal[1]:=$FFFFFF
    end;

    Colors_to_Quads(@pal,Quads,used);
    Result:=used
  end
end;

var
  i,bits: int; colors: TColors;
begin
  Result:=0;
  FillChar(Quads^,SizeOf(TRGBQuads),0);

  bits:=im_Bits;

  if bits = 1 then Result:=2 else
  if bits = 2 then Result:=4 else
  if bits = 4 then Result:=16 else
  if bits = 8 then Result:=256;

  if Result > 0 then begin
    if Result <= 16 then
      Move(EGA_Quads,Quads^,SizeOf(EGA_Quads))
    else begin
      Result:=xGray_Quads(Quads,Result);

      if im_what = im_REL then begin
        if rel.fmt in [4,6] then for i:=0 to 255 do
        Quads[i]:=TRGBQuad(EGA_Quads[i and 15])
      end else
      if im_what = im_ozf then
        ozf.GetQuads(Quads)
    end;

    if rgb_pos > 0 then begin
      Result:=tif_palette(Quads,Result);
      if Result = 16 then
      tga_palette(Quads,Result)
    end else

    if tile.palOfs > 0 then
      Result:=rsw_palette(Quads,Result)
    else
    if Result = 2 then begin

      if im_inv then
        Quads[1]:=Quads[15]
      else begin
        Quads[1]:=Quads[0];
        Quads[0]:=Quads[15]
      end

    end;

    if Result <= 16 then
    tga_palette(Quads,16)
  end
end;

function draw_Palette(Quads: PRGBQuads): Integer;
var
  ht: thChannel; std: Integer;
begin
  Init_Gray_Channel(@im_chan);
  is_Chan:=false;

  Result:=im_Palette(Quads);

  if im_bits > 8 then begin
    if im_icb.Grays = 256 then begin
      im_chan:=im_icb.chan; is_Chan:=true;
    end
  end else

  if Result > 16 then
  if Result <= 256 then

  if Quads_Gray(Quads,Result) then begin

    if im_icb.Grays >= 16 then begin
      im_chan:=im_icb.chan; is_Chan:=true;
      Result:=Fill_gray_quads(Quads,@im_icb);
    end

  end
  else begin
    std:=Result;
    Result:=Trunc_Quads(Quads,std,@im_chan);
    is_Chan:=Result < std
  end;

  if im_Get_Hex(@ht) > 0 then
    Hex_Swap_Quads(Quads,@ht)
end;

procedure Unpack_tga(buf: PBytes; len: Integer);
var
  i,j, off, acc,loc: Integer;
  dst: PWords;
begin
  off:=len-1; dst:=PWords(buf);

  for i:=1 to len do begin
    acc:=0; loc:=buf[off];

    for j:=1 to 4 do begin
      acc:=(acc shr 4) or ((loc and 3) shl 12);
      loc:=loc shr 2;
    end;

    dst[off]:=Swap(acc); Dec(off)
  end
end;

procedure pack_alfa(buf: PBytes; Count: Integer);
var
  si,di: PBytes; I: Integer;
begin
  if im_pred then
  if im_info.biCompression = bi_LZW then begin
    si:=buf;
    for I:=1 to Count-1 do begin
      si[2]:=si[0] + si[2];
      si[3]:=si[1] + si[3];
      si:=@si[2]
    end;
  end;

  di:=buf; si:=buf;
  for I:=1 to Count do begin
    di[0]:=si[0]; di:=@di[1]; si:=@si[2]
  end
end;

procedure draw_empty(DC: ICanvas; x1,y1,x2,y2: Integer);
var
  r: TRect;
begin
  if debug_enabled then begin
    DC.MoveTo(x1,y1); DC.LineTo(x2,y2);
    DC.MoveTo(x2,y1); DC.LineTo(x1,y2);
  end
  else begin
    r:=Rect(x1,y1,x2,y2);
    dc.SetBrushColor(ClearColor);
    dc.FillRect(@r);
  end
end;

procedure clip_tile(ip,jp,zoom,bits,fc: Integer; buf: PBytes);
var
  y,w,h,iw,ih,dx,dy,bx,cx,loc,fc1: Integer;
  i,j: Integer; di,di1: PBytes;
begin
  loc:=bits div 8;

  fc1:=fc;
  if loc = 3 then begin
    fc1:=tlong(ClearColor).b[0];

    with tlong(fc) do
    if b[0] = b[1] then
    if b[0] = b[2] then fc:=fc1;
  end;

  if loc in [1,2,3] then
  if (ip = tile.XTiles-1)
  or (jp = tile.YTiles-1) then begin

    w:=im_Info.biWidth div zoom;
    h:=im_Info.biHeight div zoom;

    iw:=tile.Width div zoom;
    ih:=tile.Height div zoom;

    dx:=Max(0,((ip+1)*iw) - w);
    dy:=Max(0,((jp+1)*ih) - h);

    if (dx > 0) or (dy > 0) then begin

      bx:=img_line(iw,bits);

      if dx > 0 then begin
        cx:=(iw-dx)*loc;
        di:=@buf[cx]; cx:=bx-cx;

        for y:=1 to ih-dy do begin

          if fc = fc1 then
            Fillchar(di^,cx,fc1)
          else begin di1:=di;
            for i:=0 to (cx div 3)-1 do begin
              di1[0]:=tlong(fc).b[2];
              di1[1]:=tlong(fc).b[1];
              di1[2]:=tlong(fc).b[0];
              di1:=@di1[3]
            end
          end;

          di:=@di[bx]
        end
      end;

      if dy > 0 then begin
        di:=@buf[(ih-dy)*bx];

        if fc = fc1 then
          FillChar(di^,dy*bx,fc)
        else
        for y:=1 to dy do begin

          di1:=di;
          for i:=0 to iw-1 do begin
            di1[0]:=tlong(fc).b[2];
            di1[1]:=tlong(fc).b[1];
            di1[2]:=tlong(fc).b[0];
            di1:=@di1[3]
          end;

          di:=@di[bx]
        end
      end
    end
  end
end;

procedure im_Draw(DC: ICanvas; Path: PChar;
                  dc_x,dc_y,dc_w,dc_h: Integer;
                  src_x,src_y: Integer; kz: Float);

procedure get_range16(Path: PChar);
var
  fn: TShortstr; r: TPoint;
begin
  StrUpdateExt(fn,Path,'.i16');
  if magic_FileRead(fn,'#i16',r,Sizeof(r)) then begin
    tiff.MinValue:=r.X;
    tiff.MaxValue:=r.Y;
  end
end;

procedure upd_range16(Path: PChar);
var
  fn: TShortstr; r: TPoint;
begin
  StrUpdateExt(fn,Path,'.i16');
  r.X:=tiff.MinValue;
  r.Y:=tiff.MaxValue;
  magic_FileWrite(fn,'#i16',r,Sizeof(r))
end;

var
  inter: TInterImage;
  Info: PBitmapInfo; buf1, si,di: PBytes;
  line,zoom,x_pack,y_pack,w,h,BufLen,fc: int;
  cols,src_w,src_h, ofs, bits, out_bits: int;
  i,j, ix1,iy1,ix2,iy2,i1,j1,i2,j2, ox,oy,dx,dy: int;
  iz,ix,iy,iw,ih,tx,ty,tline: Integer; rc: bool;
  ch0,ch1,ch2,ch3: PBytes; gam: TImageGamma;
  range16: TPoint;
begin
  Info:=xAllocPtr(Info_size);

  im_Zoom:=min_x2(Trunc(kz));

  if Info <> nil then
  if im_Open(Path, @Info.bmiHeader) = S_OK then begin

    if is_dem then begin
      range16.X:=tiff.MinValue;
      range16.Y:=tiff.MaxValue;
      get_range16(Path);
    end;

    iz:=Round(im_Size.X / Info.bmiHeader.biWidth);
    src_x:=src_x div iz; src_y:=src_y div iz; kz:=kz / iz;

    line:=Info.bmiHeader.biSize;

    with Info.bmiHeader do begin
      biSize:=40;
      biCompression:=bi_RGB;
      biSizeImage:=0;
    end;

    buf1:=nil;

    cols:=draw_Palette(@Info.bmiColors);
    Info.bmiHeader.biClrUsed:=cols;

    bits:=im_Bits; out_bits:=bits;

    fc:=ClearColor;
    if (bits = 8) and (cols > 0) then
    fc:=qu_Indexof(@Info.bmiColors,Cols,fc);

    ch0:=nil; if bits in [8,24] then begin

      if im_icb.Flags and icb_gamma <> 0 then
      GetGammaChannels(im_icb.gamma,ch0,ch1,ch2,ch3);

      app_Gamma(gam, ch0,ch1,ch2,ch3)
    end;

    src_w:=int_Size(dc_w,kz);
    src_h:=int_Size(dc_h,kz);

    if im_what in [im_JPG,im_SID] then begin

      Image.is_tif:=false;
      Image.is_bmp:=false; rc:=false;
      StrCopy(imagePath,'');

      if Image.im_Alloc(src_w,src_h,bits) then

      if im_what = im_JPG then
        rc:=jpeg_load(image,src_x,src_y)
      else
      if im_what = im_SID then
        rc:=sid.Decode(src_x,src_y,src_w,src_h,Image.bmpBits);

      if rc then
      with Info.bmiHeader do begin

        biWidth:=src_w;
        biHeight:=src_h;

        dx:=Round(biWidth/kz);
        dy:=Round(biHeight/kz);

        DC.StretchDIBits(dc_x,dc_y,dx,dy,
                         Image.bmpBits,Info,
                         0,0,biWidth,biHeight, 0,0);
      end

    end else

    if tile.NTiles > 0 then begin

      w:=tile.Width; h:=tile.Height;

      if out_bits = 2 then
      Inc(out_bits,out_bits);

      with Info.bmiHeader do begin
        biWidth:=w; biHeight:=h;
        biBitCount:=out_bits
      end;

      tline:=img_Line(w,out_bits);

      BufLen:=tline * h;
      if (im_what = im_RLZ)
      or (BufLen < tile.Size) then
      BufLen:=tile.Size;

      buf1:=xAllocPtr(BufLen+tline);
      if buf1 <> nil then begin

        line:=img_Line(w,bits) * h;

        ix2:=src_x + src_w;
        iy2:=src_y + src_h;

        ix1:=int_Trunc(src_x,w);
        iy1:=int_Trunc(src_y,h);

        i1:=ix1 div w; i2:=ix2 div w;
        j1:=iy1 div h; j2:=iy2 div h;

        dx:=Round(w/kz); dy:=Round(h/kz);

        x_pack:=0; y_pack:=0; zoom:=1;

        iz:=min_x2(Trunc(kz));

        if iz > 1 then
        if w mod 4 = 0 then

        if bits in [1,4,8,24] then

        if (im_info.biCompression = bi_JPG)
        or ((w mod iz = 0) and (h mod iz = 0)) then

        with Info.bmiHeader do begin
          biWidth:=w div iz;
          biHeight:=h div iz; x_pack:=iz-1;

          if im_info.biCompression = bi_JPG then begin
            zoom:=Min(8,iz); x_pack:=(iz div zoom)-1
          end;

          if x_pack > 0 then begin
            y_pack:=w div zoom;

            if bits <> 24 then
              y_pack:=y_pack * x_pack
            else begin
              y_pack:=img_line(y_pack,24) * (x_pack+1);
              x_pack:=x_pack*3;
            end
          end;

          tline:=img_Line(biWidth,bits)
        end;

        with Info.bmiHeader do
        if biWidth > 0 then
        if biHeight > 0 then begin

          inter:=TInterImage.Create(biWidth*(i2-i1+1),
                                    biHeight*(j2-j1+1),
                                    bits,cols,Round(1/kz));

          try
            if ClearColor <> 0 then
            inter.im_Fill(ClearColor);

            for j:=j1 to j2 do begin

              oy:=Trunc((j*h - src_y)/kz);
              dy:=Trunc(((j+1)*h - src_y)/kz);
              Dec(dy,oy); Inc(oy,dc_y);

              for i:=i1 to i2 do begin

                ox:=Trunc((i*w - src_x)/kz);
                dx:=Trunc(((i+1)*w - src_x)/kz);
                Dec(dx,ox); Inc(ox,dc_x);

                if TileRead(i,j,zoom,buf1,bufLen) then begin

                  clip_tile(i,j,zoom,bits,fc,buf1);
                  if bits = 2 then Unpack_tga(buf1,line);

                  with Info.bmiHeader do begin

                    if y_pack > 0 then

                    if bits in [1,4] then
                      Zoom_bmp(buf1,iz,0,biWidth,biHeight,Bits)
                    else
                    if bits = 8 then
                      Pack_tile_bytes(buf1,biWidth,biHeight,x_pack,y_pack)
                    else
                    if bits = 24 then
                      Pack_tile_rgb(buf1,biWidth,biHeight,x_pack,y_pack);

                    if Assigned(ch0) then
                      bmp_Gamma(buf1,ch0,ch1,ch2,ch3,@Info.bmiHeader)
                    else
                    if is_Chan then
                      bmp_Transit(buf1,@im_chan,@Info.bmiHeader);

                    if inter.Active then

                      inter.Set_Tile((i-i1)*biWidth,
                                     (j-j1)*biHeight,
                                     biWidth,biHeight,buf1)

                    else

                      DC.StretchDIBits(ox,oy,dx,dy,
                                       buf1,Info,
                                       0,0,biWidth,biHeight,
                                       0,0);
                  end
                end else
                if (i >= 0) and (i < tile.XTiles) then
                if (j >= 0) and (j < tile.YTiles) then
                draw_empty(DC,ox,oy,ox+dx,oy+dy);
              end
            end;

            if inter.Active then begin

              ix:=i1*w; iw:=inter.im_w;
              iy:=j1*h; ih:=inter.im_h;

              ox:=Round((ix - src_x)/kz);
              oy:=Round((iy - src_y)/kz);

              while ox < 0 do begin
                tx:=Round((ix+1 - src_x)/kz);
                if tx >= 0 then Break; ox:=tx;
                Inc(ix); Dec(iw)
              end;

              while oy < 0 do begin
                ty:=Round((iy+1 - src_y)/kz);
                if ty >= 0 then Break; oy:=ty;
                Inc(iy); Dec(ih)
              end;

              if (iw > 0) and (ih > 0) then

              inter.Draw(DC,
                         dc_x + ox,dc_y + oy,
                         Round(iw/kz),Round(ih/kz),
                         ix - i1*w,iy - j1*h,iw,ih);
            end
          finally
            inter.Free
          end;
        end
      end

    end else
    if Image.Active then begin

      if Frame.BeginDraw(Info,
                         src_x,src_y,src_w,src_h,
                         kz) then begin

        iy:=Frame.SrcY;
        ih:=Frame.Height;
        iz:=Frame.Zoom;

        for i:=0 to ih-1 do begin
          si:=Image.Lines[iy]; Inc(iy,iz);
          Frame.Lines[i]:=si;
        end;

        Frame.Draw(DC,dc_x,dc_y,src_x,src_y,kz);
      end;

    end
    else begin
      if tiff.line > line then
      line:=tiff.Line;

      buf1:=xAllocPtr(line);

      if Frame.BeginDraw(Info,
                         src_x,src_y,src_w,src_h,
                         kz) then begin

        iy:=Frame.SrcY;
        ih:=Frame.Height;
        iz:=Frame.Zoom;

        im_GotoLine(iy,buf1);
        for i:=0 to ih-1 do begin

          im_ReadLine(iy,buf1); Inc(iy);

          if im_alfa then
          pack_alfa(buf1,im_info.biWidth);

          Frame.Lines[i]:=buf1;

          for j:=2 to iz do begin
            im_ReadLine(iy,buf1); Inc(iy);
          end;
        end;

        if is_Chan then Frame.Recode(@im_chan) else
        if im_cmyk then Frame.Cmyk(im_mac);

        Frame.Draw(DC,dc_x,dc_y,src_x,src_y,kz);
      end;
    end;

    if is_dem then
    if (range16.X > tiff.MinValue)
    or (range16.Y < tiff.MaxValue) then
    upd_range16(Path);

    xFreePtr(buf1);
  end;

  xFreePtr(Info);

  im_Done; im_zoom:=1
end;

function im_Flags: longint;
var
  w,h: Integer;
begin
  Result:=0;
  if im_what = im_TIF then begin

    if tile.NTiles = 0 then
      Result:=img_Inline
    else
    if im_Tiled(w,h) then

    if w > 16 then
    if w mod 16 = 0 then

    Inc(Result,img_tiled);

    if tiff.bits = 32 then
    Inc(Result,img_pf32);

    Inc(Result,img_tiff)

  end else
  if im_what = im_JPG then
    Result:=img_tiled
  else
  if im_what = im_ECW then
    Result:=img_Inline
  else
  if im_what = im_REL then
    Result:=img_Inline + img_Relief
  else
  if im_what = im_RSW then
    Inc(Result,img_tiled);

  if im_cmyk then
    Inc(Result,img_cmyk)
  else
  if im_bits = 8 then begin
    if rgb_pos > 0 then
    Inc(Result,img_pal)
  end;

  if im_bits = 1 then
  if im_inv then Inc(Result,img_inv);

  Inc(Result,img_tiff1)
end;

function im_Position: longint;
begin
  Result:=0
end;

procedure im_GotoLine(y: Integer; buf: PBytes);
var
  pos,len: Int64;
  bands: Array[0..2] of Integer;
begin
  with im_info do
  if (y >= 0) and (y < biHeight) then

  if biCompression = bi_ECW then begin

    bands[0]:=0; bands[1]:=1; bands[2]:=2;
		NCScbmSetFileView(ecw,biBitCount div 8,@bands,
                      0,y,biWidth-1,biHeight-1,
                      biWidth,biHeight-y);

  end else
  if bit_pos > 0 then begin

    case biCompression of
  bi_RGB:
      begin
        pos:=y; y:=0; len:=tiff.line;
        im_Pos:=bit_pos; Inc(im_Pos,pos*len)
      end;
  bi_REL:
      begin pos:=y; y:=0;
        im_Pos:=bit_pos;
        Inc(im_Pos,pos*biSize*rel.len)
      end;
    end;

    if not Assigned(im_View.ptr) then
    xFileSeek(im_h,im_Pos);

    while y > 0 do begin
      im_ReadLine(0,buf); Dec(y)
    end

  end
end;

procedure im_ReadLine(y: Integer; buf: PBytes);

function InByte: byte;
begin
  Result:=0;

  if im_Pos < fileSize then

  if Assigned(im_View.ptr) then
    Result:=im_View.ptr[im_Pos]
  else
    FileRead(im_h,Result,1);

  Inc(im_Pos)
end;

procedure InBytes(var buf; len: int);
var
  bx: int64; cx: int;
begin
  bx:=im_Pos; Inc(im_Pos,len);

  cx:=len;
  if im_Pos > fileSize then begin
    Fillchar(buf,len,0);
    cx:=fileSize-bx;
  end;

  if cx > 0 then
  if im_View.ptr = nil then
    FileRead(im_h,buf,cx)
  else
    Move(im_View.ptr[bx],buf,cx);
end;

function stripe_jpg(pos,len: Integer): Boolean;
begin
  Result:=false;

  with tiff do begin

    if Assigned(jpg) then
    if Assigned(im_View.ptr) then begin

      Load_jpg_props;

      Result:=jpg.Decompress(bufp,
                             im_info.biWidth,Rows,im_Bits,
                             @im_View.ptr[pos],len,
                             @jpg_props);
    end;

    if not Result then
    FillChar(bufp^,Size,0);
  end
end;

function stripe_zip(pos,len: Integer): Boolean;
var
  si: Pointer;
begin
  Result:=false;

  with tiff do begin
    si:=Seek_stripe(pos,len);
    if Assigned(si) then
    Result:=zlb.xDecompress(si,len,bufp,size);

    if not Result then
    FillChar(bufp^,Size,0);
  end
end;

procedure ReadBuf(tmp: PBytes; len,pack: Integer);

procedure pfs_to_pf8(src,dst: PBytes; Count,bps: int);
const
  layer = 3;
var
  fp: PFloats; i,h: int; v,v0,kv: float;
begin
  if im_float then begin

    bps:=bps div 4;

    v0:=250; kv:=255/50;

    fp:=@src[4*layer];
    for i:=0 to Count-1 do begin
      v:=fp[0]; fp:=@fp[bps];
      h:=Round((v-v0)*kv);
      if h < 0 then h:=0;
      if h > 255 then h:=255;
      dst[i]:=h
    end
  end
end;

procedure i16_upd_range(si: Pointer; Count: int);
var
  vp: PSmallint; i,v,v1,v2: int;
begin
  v1:=MaxLongint;
  v2:=-MaxLongint;

  vp:=si;
  for i:=1 to Count do begin
    v:=vp^; Inc(vp);
    if v > dem_znil then
    if v < v1 then v1:=v else
    if v > v2 then v2:=v
  end;

  if v1 < tiff.MinValue then tiff.MinValue:=v1;
  if v2 > tiff.MaxValue then tiff.MaxValue:=v2;
end;

var
  lzw: tlzw; si: PBytes; bx: int64;
  i,k,loc,ind,size,ax,len1: int;
  z: TRel_Loc; kx: Double;
begin
  i:=0;

  case im_info.biCompression of

bi_RGB:
    if im_pack then begin

      len1:=len*tiff.Pack;

      bx:=im_Pos; Inc(im_Pos,len1);

      if im_Pos > fileSize then
      Fillchar(tmp^,len1,0); si:=tmp;

      if Assigned(im_View.ptr) then begin
        if im_Pos <= fileSize then
        si:=@im_View.ptr[bx]
      end
      else begin
        xFileSeek(im_h,bx);
        FileRead(im_h,tmp^,len1);
      end;

      if im_info.biBitCount = 8 then begin

        if im_float then
          f4_to_b8(si,tmp,len div 4)
        else
        if tiff.bits <> 16 then
          pfs_to_pf8(si,tmp,len,tiff.Pack)
        else
        if tiff.SampleFormat <> 2 then
          pf16_to_pf8_scale(si,tmp,len,tiff.MaxValue)
        else begin
          if is_dem then i16_upd_range(si,len);
          pf16_to_pf8_scalei(si,tmp,len,tiff.MinValue,tiff.MaxValue);
        end

      end else
      if tiff.Planes = 1 then begin

        k:=(tiff.MaxValue-tiff.MinValue) div 256;

        if im_mac then begin
          if k <= 0 then
            pf16_to_pf8b(si,tmp,len)
          else
            pf16_to_pf8bf(si,tmp,len,k)
        end
        else begin
          if k <= 1 then
            pf16_to_pf8a(si,tmp,len)
          else
            pf16_to_pf8af(si,tmp,len,k)
        end
      end else
      if im_mac then
        mac12_to_pf8(si,tmp,len)
      else
        pf16_to_pf8_scale(si,tmp,len,tiff.MaxValue)
    end
    else begin
      if pack = 0 then pack:=len;
      InBytes(tmp^,pack);

      k:=pack div 4;

      if im_cmyk then begin
        if im_mac then SwapInts(tmp,k);
        CMYK_BGR(Pointer(tmp),tmp,k);
      end else
      if im_info.biBitCount = 8 then
      if im_float then f4_to_b8(tmp,tmp,k)
    end;

bi_RLC:
    begin
      si:=Seek_stripe(im_Pos,pack);
      if si = nil then
        FillChar(tmp^,len,0)
      else
        rlc_Unpack(si,pack,tmp,len);
    end;

bi_GR3:
    if Assigned(im_View.ptr) then

    with im_info do
    Hoffman.Unpack_gr3(@im_View.ptr[im_Pos],pack,
                       tiff.bufp,tiff.rows,
                       biWidth,biSize);

bi_GR4:
    if Assigned(im_View.ptr) then

    with im_info do
    Hoffman.Unpack_gr4(@im_View.ptr[im_Pos],pack,
                       tiff.bufp,tiff.rows,
                       biWidth,biSize);

bi_LZW:
    if pack > 0 then begin

      si:=Seek_stripe(im_Pos,pack);
      if si = nil then
        Fillchar(tmp^,len,0)
      else begin
        lzw:=tlzw.Create(si,pack,tmp,len);
        try
          lzw.Unpack;
        finally
          lzw.Free
        end
      end;

      k:=len div 4;

      if tiff.bits = 32 then
      if im_info.biBitCount = 24 then begin

        if not im_cmyk then
          pf32_to_pf24(tmp,tmp,k)
        else begin
          if im_mac then SwapInts(tmp,k);
          CMYK_BGR(Pointer(tmp),tmp,k);
        end

      end else
      if im_info.biBitCount = 8 then
      if im_float then
        f4_to_b8(tmp,tmp,k)
      else
        pf32_to_pf8_scale(tmp,tmp,k,0)
    end;

bi_JPG:
    stripe_jpg(im_pos,pack);

bi_ZIP:
    stripe_zip(im_pos,pack);

bi_REL:
    while len > 0 do begin
      loc:=len; size:=cash_size div rel.len;
      if loc > size then loc:=size; Dec(len,loc);

      kx:=255/rel.zmax;

      if rel.len = 1 then begin
        InBytes(tmp[i],loc);

        for k:=1 to loc do begin
          tmp[i]:=Round((tmp[i]-rel.zmin)*kx);
          Inc(i)
        end
      end else

      for k:=1 to loc do begin
        InBytes(z,rel.len);

        case rel.fmt of
      1:  z.d:=z.b;
      2:  z.d:=z.i;
      4:  z.d:=z.l;

      5:  begin ax:=Abs(z.l);

            if ax <= max_Single.i then
              z.d:=z.f
            else
              z.d:=rel.zmin
          end;

      6:  z.d:=z.cod;
      8:  z.d:=z.d;
      9:  z.d:=((z.surf and $FFFFF) - 200000)/10;
      else
          z.d:=0;
        end;

        z.d:=Min(Max(z.d-rel.zmin,0),rel.zmax);
        tmp[i]:=Min(Round(z.d*kx),255); Inc(i);
      end
    end;

else
    FillChar(tmp^,len,0)
  end;
end;

procedure Planar_rgb(si,di: PBytes; Size: int);
var
  si1,si2,si3: PBytes;
begin
  si1:=si;
  si2:=@si1[Size];
  si3:=@si2[Size];

  while Size > 0 do begin
    di[0]:=si1[0]; si1:=@si1[1];
    di[1]:=si2[0]; si2:=@si2[1];
    di[2]:=si3[0]; si3:=@si3[1];
    di:=@di[3]; Dec(Size)
  end
end;

var
  ip,ind,len,sz,row: int; di,di1: PBytes;
  pos: int64; lzw: uint;
begin
  len:=im_info.biSize;

  if y >= 0 then
  if y < im_info.biHeight then

  if im_info.biCompression = bi_ECW then begin
        
    if im_info.biBitCount = 8 then
  		NCScbmReadViewLineBIL(ecw,@buf)
    else
    if not im_pack then
      NCScbmReadViewLineRGB(ecw,buf)
    else begin
      NCScbmReadViewLineRGBA(ecw,buf);
      pf32_to_pf24(buf,buf,im_info.biWidth);
    end
  end else

  if bit_pos <> 0 then begin

    if tiff.bufp = nil then
      ReadBuf(buf,len,tiff.line)
    else
    if im_info.biCompression = bi_RGB then
      ReadBuf(buf,len,tiff.line)
    else
    with tiff do begin
      if (y < bufy) or (y >= bufy+rows) then begin
        ind:=y div rows; bufy:=ind*rows;
        im_Pos:=bit_pos + bufy*tiff.line;
        if im_View.ptr = nil then
        xFileSeek(im_h,im_Pos);
        ReadBuf(bufp,size,bit_size)
      end;

      Move(bufp[(y-bufy)*len],buf^,len)
    end

  end else

  with tiff do
  if Assigned(bufp) then begin
    if (y < bufy) or (y >= bufy+rows) then begin

      ind:=y div rows; bufy:=ind*rows;
      Planes:=Max(1,Planes); di:=bufp;

      if PlanarConfig <> 2 then
        Planes:=1
      else
        di:=@bufp[size];

      sz:=size div Planes;

      if im_pack then sz:=sz div tiff.Pack;

      di1:=di;
      for ip:=1 to Planes do begin

        pos:=0; lzw:=0;
        if ind < offLen then begin
          if Assigned(Offset) then
            pos:=Offset[ind]
          else
            pos:=Read_ind(offp,offTyp,ind);

          lzw:=Read_ind(lenp,lenTyp,ind)
        end;

        if (lzw = 0)
        or (pos <= 0)
        or (pos+lzw > fileSize) then
          Fillchar(di1^,sz,0)
        else begin
          im_Pos:=pos;
          if im_View.ptr = nil then
          xFileSeek(im_h,im_Pos);

          ReadBuf(di1,sz,lzw)
        end;

        Inc(ind,offLen div Planes);
        di1:=@di1[sz];
      end;

      if PlanarConfig = 2 then
      if Planes < 3 then
        Move(bufp[size],bufp[0],sz)
      else
        Planar_rgb(di,bufp,sz);

      if im_hdif then
      bmp_diff(bufp,im_info.biWidth,rows,im_bits,len);
    end;

    Move(bufp[(y-bufy)*len],buf^,len)
  end
end;

function im_Tiled(var tile_w,tile_h: Integer): boolean;
begin
  Result:=false;

  if tile.NTiles > 0 then
  if tile.Width > 0 then
  if tile.Height > 0 then begin
    tile_w:=tile.Width;
    tile_h:=tile.Height;
    Result:=true
  end
end;

function im_GetTile(buf: PBytes; x,y,w,h,size: longint): PBytes;
var
  tmp: PBytes; img: XImage; qu: TRGBQuads;
begin
  Result:=nil;

  if tile.NTiles > 0 then
  if tile.Width = w then
  if tile.Height = h then

  if w * im_Bits div 8 * h = size then

  if im_what = im_RLZ then begin
    tmp:=tile.bufp; if Assigned(tmp) then
    if TileRead(x div w,y div h,1,tmp,size) then begin
      Move(tmp^,buf^,size); Result:=buf
    end
  end else
  if TileRead(x div w,y div h,1,buf,Size) then
    Result:=buf
  else begin
    img:=XImage.Create;
    try
      if img.im_Buffer1(w,h,im_bits,size div h,buf) then begin

        if im_Palette(@qu) > 0 then
        img.Set_Quads(@qu);

        img.im_Fill(ClearColor);

      end
    finally
      img.Free
    end
  end
end;

function im_Get_Hex(chan: PBytes): longint;
begin
  Result:=img_Get_Hex(im_h,Hex_pos,chan)
end;

procedure im_Put_Index(indp: PIndex; len: longint);
begin
end;

procedure im_Put_Hex(chan: PBytes);
begin
  if Hex_Pos > 0 then begin
    xFileSeek(im_h,Hex_Pos);
    FileWrite(im_h,chan^,16)
  end
end;

procedure im_Put_Palette(Quads: PRGBQuads);
var
  buf: TWords; i,used: Integer;
begin
  if rgb_pos > 0 then begin
    used:=rgb_len div 6;

    for i:=0 to used-1 do with Quads[i] do begin
      buf[i]:=rgbRed; buf[used+i]:=rgbGreen;
      buf[used+used+i]:=rgbBlue
    end;

    xFileSeek(im_h,rgb_pos);
    FileWrite(im_h,buf,rgb_len)
  end
end;

initialization
begin
  jpg:=nil;
  sid:=tltiImage.Create;
  ozf:=tozf2.Create;
  image:=TRGBImage.Create;

  Frame:=TDibFrame.Create;

  jpg_props.IsValid:=false;
  jpg_props.data:=nil;
  jpg_props.size:=0;

  wms:=nil; im_zoom:=1;
  only_read:=true; Init;
  StrCopy(im_Path,'');

  cid_key:=key_load('tif.key');
end;

finalization Free;
begin
  FreeJpegTables(jpg_props);

  Frame.Free;

  Image.Free;
  ozf.Free;
  sid.Free;

  jpg:=nil
end;

end.
