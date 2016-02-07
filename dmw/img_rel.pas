unit img_rel; interface

uses
  Classes,LCLType,Math,
  otypes,ofiles,xlist,
  oframes,xline,ogauss,
  xinter,img_x,img_glu,
  tiff_x;

const
  tiles_max = 9;

  h_null = -10000;
  h_null1 = -10000+1;

type
  PRelHist = ^TRelHist;
  TRelHist = record
    minv,step,cols: Longint;
    count: Int64; buf: PIntegers
  end;

  prel_array = record case integer of
0: (p: Pointer); 1: (f: PFloats); 2: (w: PWords)
  end;

  TRel_XY = class(TReadFile)
    function Open(Path: PChar): Boolean; override;
  private
    fxy: PGPoly;

    function Get_MinX: Double;
    function Get_MinY: Double;
    function Get_MaxX: Double;
    function Get_MaxY: Double;

  public
    property MinX: Double read Get_MinX;
    property MinY: Double read Get_MinY;
    property MaxX: Double read Get_MaxX;
    property MaxY: Double read Get_MaxY;
  end;

  ttvr_hdr = record
    lx,ly,rx,ry: Longint;
    nx,ny, step: Longint
  end;

  TRelHeader = record
    mark: Char4;
    data,res1,res2: Longint;
    nx,nz,ny,fn: Longint;
    len,fmt,res3,res4: Byte;
    xmin,zmin,ymin: Double;
    xstep,zstep,ystep: Double;
    xmax,zmax,ymax: Double;
  end;

  TRel_Loc = packed record case Integer of
1: (b: Byte);
2: (i: SmallInt);
4: (l: Longint);
5: (f: Single);
6: (rel,cod,loc: Word);
8: (d: Double);
9: (surf: dword; depth: Word);
  end;

  PRlzHeader = ^TRlzHeader;
  TRlzHeader = record
    mark: Char4;
    tile_nx,tile_ny: int;
    XTiles,YTiles: int;
    offp,lenp: int;
    xmin,zmin,ymin: Double;
    xmax,zmax,ymax: Double;
    a0,k0: Float; s: tsys;
    zrange: int;
    m_offp,m_lenp: int;
    c_offp,c_lenp: int;
    ver: int;
  end;

  TRlzDump = class
    constructor Create;
    destructor Destroy; override;

    procedure set_zrange(z1,z2: float);

    function Begin_doc(APath: PChar;
                       nx,ny,page: Integer;
                       gx1,gx2,gy1,gy2,gz1,gz2: Double;
                       const sys: tsys): Boolean;

    function Clone(APath: PChar; Ahdr: TRlzHeader): Boolean;

    procedure Close_doc;

    function Refresh_min_max: Integer;

    procedure verify;

    function g_to_p(gx,gy: double): TPoint;

    function Add_rel(Tile: PChar; pg_i,pg_j: Integer): Boolean;

    function Add_buf(rel: PFloats; rw,rh: Integer;
                     pg_i,pg_j: Integer): Boolean;

    function Add_clu(Tile: PChar; pg_i,pg_j: Integer): Boolean;

    function Add_tile(Tile: PFloats; pg_i,pg_j: Integer): Boolean;
    function Add_tileW(Tile: PWords; pg_i,pg_j: Integer): Boolean;

  private
    fHandle: Integer;
    fHdr: TRlzHeader;

    NTiles: Integer;

    _zrange: PIntegers;
    _offp,_lenp: PIntegers;
    off_len: Integer;

    _m_offp,_m_lenp: PIntegers;
    _c_offp,_c_lenp: PIntegers;

    ibuf: PWords;
    obuf: PBytes;
    buf_len: Integer;

    zlb: tzlb;

    fVertBuf: PWords;
    fHorzBuf: PWords;
    fHorzWidth: Integer;
    fVertWidth: Integer;
    fRelIndex: TPoint;

    fstepx: double;
    fstepy: double;

    flog: TStringList;

    fIs_Micro: longbool;
    fIs_z: longbool;

    fPath: TShortstr;

    function Get_Active: Boolean;

    function GetPath: PChar;

    function Get_TileMeters: TGauss;

    function ReadTile(pg_i,pg_j: Integer): Boolean;

    procedure Pack_tile(pg_i,pg_j: Integer; buf: PWords);

  public
    property Active: Boolean read Get_Active;
    property Is_Micro: Longbool write fIs_Micro;
    property Hdr: TRlzHeader read fHdr write fHdr;
    property TileMeters: TGauss read Get_TileMeters;
    property stepx: double read fstepx;
    property stepy: double read fstepy;
    property log: TStringList write flog;
    property Path: PChar read GetPath;
  end;

  PRlz_tile = ^TRlz_tile; TRlz_tile = record
    Rect: TRect; Index: TPoint; Buf: PWords;
  end;

  TLayer = record
    offp,lenp: PIntegers;
    Buf: PBytes; Index: Integer;
  end;

  PRlzPyrRec = ^TRlzPyrRec;
  TRlzPyrRec = record
    magic: int;
    Width,Height: int;
    XTiles,YTiles: int;
    offp,lenp,zrange: int;
    seek,next: int
  end;

  PRlzPyrArr = ^TRlzPyrArr;
  TRlzPyrArr = Array[0..31] of TRlzPyrRec;

  TRlzPyrList = class(TCustomList)
    constructor Create;
  end;

  TRelief = class
    constructor Create;
    destructor Destroy; override;

    function Open(Path: PChar): Boolean; virtual;
    procedure Close; virtual;

    function GetPyr(List: TRlzPyrList): int;
    procedure SetPyrLevel(lev: PRlzPyrRec);

    function Open_edit: Boolean;

    procedure Get_line(y: int; buf: PFloats);
    procedure Put_line(y: int; buf: PFloats);

    function Get_z(x,y: int; out z: Double): Boolean; virtual;
    function Int_z(x,y: Double; out z: Double): Boolean;

    function Near_z(x,y: int; R: float; out z: Double): Boolean;

    function sm_z(x,y: int; out z: int): Boolean;

    function Get_micro(x,y: Integer): Integer; virtual;
    function Get_clutter(x,y: Integer): Integer;

    procedure Get_rlz(x,y,w,h: int;
                      a0,k0: Float; buf: PWords);

    procedure Get_mrf(x,y,w,h: int; buf: PBytes);

    function tile_Exist(i,j: Integer): Boolean;

    function GetCompRatio: Float;

    function Seek_tile(ip,jp: int; out tp: PBytes): int;

    function Load_tile(ip,jp: int): Boolean;
    function xLoad_tile(x,y: int): Boolean;

    function pyr_tile(pyr: THandle; buf: PWords; out len: int): int;

    function add_pyr(var last,lev: TRlzPyrRec;
                     offp,lenp: PIntegers;
                     pyr: THandle): bool;

    procedure Update_tile(ip,jp: int; buf: PWords);
    procedure Undo_tile(ip,jp: int; si: PBytes; cx: int);
    procedure Delete_tile(ip,jp: int);

    function Get_tile(ip,jp,line: int; buf: PFloats): Boolean;

    function frame_zrange(const lt,rb: TPoint;
                          out zr: TRange): Integer;

    function tile_zrange(ip,jp: int): TRange;
    function tile_wrange(ip,jp: int): int;

    function IsEmpty(h: Float): Boolean;

  private
    fFile: IFile;
    fData: PBytes;

    hdr: TRelHeader;
    frlz: TRlzHeader;

    fVersion: Integer;

    zlb: tzlb;

    coeff: pinter_coeff;

    _offp: PIntegers;
    _lenp: PIntegers;
    _zrange: PIntegers;
    _zrange1: PIntegers;

    fTree: PIntegers;
    fWords: PWords;
    fTile: PRlz_tile;

    fBuffer: Pointer;
    fTileBuf: Pointer;
    fTempBuf: Pointer;

    fByteScale: tgauss;

    fMicro: TLayer;
    fClutter: TLayer;

    tiles: array[0..tiles_max-1] of TRlz_tile;

    fMatrix: Real3x3;
    fReverse: Real3x3;

    fUpdateCount: Integer;

    fExtended: Longbool;
    fDoubleBuffered: Longbool;
    fLeftAlign: Longbool;
    fIs_rlz: Longbool;
    fIs_upd: Longbool;

    fOnActivate: TNotifyEvent;
    fEditPath: TShortStr;

    function Get_Tile_size: Integer;

    function Get_Active: Boolean; virtual;
    function Get_row(Y: int): Pointer; virtual;

    function GetEditPath: PChar;

    function Get_Is_mrf: Boolean;

    function Get_layer(var Layer: TLayer; x,y: int): int;

  public
    property View: IFile read fFile;

    property Active: Boolean read Get_Active;
    property Extended: Longbool write fExtended;
    property DoubleBuffered: Longbool write fDoubleBuffered;
    property LeftAlign: Longbool write fLeftAlign;

    property Is_rlz: Longbool read fIs_rlz;
    property Is_mrf: Boolean read Get_Is_mrf;
    property Is_upd: Longbool write fIs_upd;

    property OnActivate: TNotifyEvent write fOnActivate;

    property Header: TRelHeader read hdr;
    property rlz: TRlzHeader read frlz;

    property Width: Integer read hdr.Nx;
    property Height: Integer read hdr.Ny;

    property Tile_buf: PWords read fWords;
    property Tile_size: Integer read Get_Tile_size;

    property Rows[Y: int]: Pointer read Get_row;

    property EditPath: PChar read GetEditPath;

    property Matrix: Real3x3 read fMatrix;
    property Reverse: Real3x3 read fReverse;
  end;

  trel_prj = record
    sys: tsys; top: tgauss;
    kx,ky: Double;
  end;

  TRelMatrix = class(TRelief)
    function Open(Path: PChar): Boolean; override;

    function Get_mpp: Double;

    function Get_bound(out xmin,ymin,xmax,ymax: double): Integer;
    procedure Get_gbound(out xmin,ymin,xmax,ymax: double);
    
    function Get_lg_Transit(out lg: lg_Transit): Integer;

    function g_to_p(const g: tgauss; out p: TGauss): Boolean;
    function r_to_p(const r: xgeoid; out p: TGauss): Boolean;

    function r_to_g(const r: xgeoid; out g: TGauss): Integer;
    function g_to_r(const g: TGauss; out r: xgeoid): Integer;

    function wgs_to_g(b,l: Double; out g: TGauss): Integer;

    function r_to_v(const r: xgeoid; out v: txyz): Boolean;
    function r_to_l(const r: xgeoid; out p: TPoint): Boolean;
    function g_to_l(const g: tgauss; out p: TPoint): Boolean;

    function l_to_g(x,y: int): TGauss;

    function p_to_g(x,y: Double; out g: tgauss): Boolean;
    function p_to_r(x,y: Double; out r: xgeoid): Boolean;

    function r_get_z(const x: xgeoid; out z: Double): Boolean;
    function g_get_z(const g: tgauss; out z: Double): Boolean;
    function xy_get_z(gx,gy: Double; var z: Double): Boolean;

    function g_int_z(const g: tgauss; out z: Double): Boolean;
    function r_int_z(const x: xgeoid; out z: Double): Boolean;
    function wgs_int_z(b,l: Double; out z: Double): Boolean;

    function elp_z(px,py,pz: Double; const dst: tsys): Double;
                   
  private
    fprj: trel_prj;

    function Get_in_tr: Real3x3;
    function Get_out_tr: Real3x3;

  public
    property prj: trel_prj read fprj;
    property in_tr: Real3x3 read Get_in_tr;
    property out_tr: Real3x3 read Get_out_tr;
  end;

  TRelScene = class(TRelMatrix)
    constructor Create;
    destructor Destroy; override;

    function Open(APath: PChar): Boolean; override;
    procedure Close; override;

    function tiff_Open(Path: PChar; tw,th: int): Boolean;
    procedure tiff_Close;

    function Get_tile_tex(tex: TTexture; zoom: Integer): Boolean;

  private
    ftiff: TTiffView;
    ftex_k: TGauss;

    fParent: TRelScene;
    fIndex: TPoint;

    fFlags: Cardinal;

    fPath: TShortStr;

    function GetPath: PChar;

    function Get_tex_Point(X,Y: Integer): TGauss;

    function Get_tiff_tiled: Boolean;

  protected
    function Get_tiff_width: Integer; virtual;

  public
    property tiff: TTiffView read ftiff;

    property tex_k: TGauss read ftex_k;

    property tex_Points[X,Y: Integer]: TGauss read Get_tex_Point;

    property tiff_tiled: Boolean read Get_tiff_tiled;
    property tiff_width: Integer read Get_tiff_width;

    property Flags: Cardinal read fFlags
                             write fFlags;

    property Path: PChar read GetPath;
  end;

function rlz_round(nx,page: Integer): Integer;

function Init_rel_hist(minv,maxv: Float;
                       out hist: TRelHist): Boolean;

function Trunc_rel_hist(var hist: TRelHist): Integer;

procedure Free_rel_hist(var hist: TRelHist);

function rel_get_hist(rel: TRelief; Hist: PRelHist;
                      var z1,z2: Single): int;

function rlz_tile_hist(rel: TRelief; ip,jp: int;
                       Hist: PRelHist; var z1,z2: Single): DWord;

function this_rlz(h: Integer; out hdr: TRlzHeader): Boolean;

function get_rlz_pyr(h: THandle; zoom: int;
                     const hdr: TRlzHeader;
                     out lev: TRlzPyrRec): bool;

function this_rlf(h: Integer; out hdr: TRelHeader): Boolean;
function file_rel(Path: PChar; out hdr: TRelHeader): Boolean;
function this_rel(Path: PChar; is_rlz: Boolean): Boolean;

procedure rel_after_edit(Path: PChar);

implementation

uses
  SysUtils,
  otypes1,convert,
  xreals,xpoly,xy,
  xbl_use;

const
  pyr_magic = $1234;

var
  triw: Array[0..3] of TTriWeight;

procedure Init;
begin
  TriWeightd(0,0, 0,1, 1,0, triw[0]);
  TriWeightd(0,0, 0,1, 1,1, triw[1]);
  TriWeightd(0,0, 1,1, 1,0, triw[2]);
  TriWeightd(0,1, 1,1, 1,0, triw[3]);
end;

function rlz_round(nx,page: Integer): Integer;
begin
  Result:=int_Round(nx,page)+1;
  if Result + (page div 3) < nx then
  Inc(Result,page)
end;

procedure Clear_layer(var Layer: TLayer);
begin
  Fillchar(Layer,Sizeof(Layer),0);
  Layer.Index:=-1
end;

function ds_get_tat(const ds: IFile;
                    offp: PIntegers; offOffs: int;
                    lenp: PIntegers; lenOffs: int;
                    Count: int; pyr: bool): bool;
var
  i,cx,bx,dx: int; sz: int64;
begin
  Result:=false;

  dx:=Count * 4; sz:=ds.GetSize;

  if offOffs > 0 then
  if lenOffs > 0 then
  if lenOffs = offOffs + dx then

  if lenOffs+dx <= sz then begin

    Result:=true;

    if Assigned(offp) then begin
      ds.Load(offOffs,offp^,dx);
      ds.Load(lenOffs,lenp^,dx);

      dx:=lenOffs + dx;

      for i:=1 to Count do begin

        bx:=offp[0]; cx:=lenp[0];

        if cx > 0 then

        if (bx+cx > sz)
        or (not pyr and (bx < dx)) then
        begin Result:=false; Break end;

        offp:=@offp[1];
        lenp:=@lenp[1]
      end
    end
  end
end;

function tile_zmin_zmax(buf: PWords; nx,ny: Integer): Integer;
var
  z1,z2: Word; i,cx,bx,z: Integer; si: PWords;
begin
  z1:=1; z2:=0; bx:=0;

  si:=buf; cx:=nx * ny;

  for i:=1 to cx do begin

    if si[0] > 0 then begin

      if bx > 0 then begin
        z1:=Min(z1,si[0]);
        z2:=Max(z2,si[0]);
      end
      else begin
        z1:=si[0]; z2:=z1
      end;

      Inc(bx)
    end;

    si:=@si[1]
  end;

  tlong(z).w[0]:=z1;
  tlong(z).w[1]:=z2;
  Result:=z
end;

function is_rlf_hdr(var hdr: TRelHeader; fsize: int): Boolean;
var
  wait: int;
begin
  Result:=false;
  
  if hdr.mark = 'RLF0' then
  if hdr.len <= 6 then begin
    Dec(fsize,hdr.data);
    wait:=hdr.Nx*hdr.Ny*hdr.len;

    if fsize = wait then
      Result:=true
    else
    if fsize > wait then
    if hdr.len = 4 then
    if hdr.fmt = 5 then
    if fsize = hdr.Nx*hdr.Ny*6 then begin
      hdr.len:=6; hdr.fmt:=6;
      Result:=true
    end
  end
end;

function is_rlz_hdr(var hdr: TRlzHeader): Boolean;
var
  ip: PIntegers; n: Integer; s: tsys;
begin
  Result:=false;

  if hdr.mark = 'rlz0' then begin

    if hdr.offp < Sizeof(hdr) then begin

      ip:=@hdr;
      n:=hdr.offp div 4;

      hdr.ver   :=ip[n-1];
      hdr.c_lenp:=ip[n-2];
      hdr.c_offp:=ip[n-3];
      hdr.m_lenp:=ip[n-4];
      hdr.m_offp:=ip[n-5];
      hdr.zrange:=ip[n-6];

      s:=hdr.s;
      hdr.s.lc:=s.b2;
      hdr.s.b1:=s.lc;
      hdr.s.b2:=s.b1;
      hdr.s.b3:=0;
      hdr.s.k0:=1;
      hdr.s.rn:=0;
      hdr.s.dat:=nil_Datum7;
    end;

    with hdr.s do
    if (pps = 1) and (elp = 1) then
    dat:=ru42_Datum;

    Result:=true
  end
end;

function is_rel_ds(const ds: IFile;
                   out hdr: TRelHeader): bool;
var
  p: ^TRelHeader;
begin
  Result:=false;
  p:=ds.Seek(0,Sizeof(TRelHeader));
  if Assigned(p) then begin
    hdr:=p^; Result:=is_rlf_hdr(hdr,ds.GetSize)
  end
end;

function is_rlz_ds(const ds: IFile;
                   out hdr: TRlzHeader): bool;
var
  p: ^TRlzHeader;
begin
  Result:=false;
  p:=ds.Seek(0,Sizeof(TRlzHeader));
  if Assigned(p) then begin
    hdr:=p^; Result:=is_rlz_hdr(hdr)
  end
end;

function TRel_XY.Open(Path: PChar): Boolean;
var
  fn: TShortStr;
begin
  fxy:=nil;

  StrUpdateExt(fn,Path,'.XY');
  Result:=inherited Open(fn);

  if Result then
  if Size = SizeOf(tgauss)*4 then
  fxy:=@Buf[0];

  if fxy = nil then Close;
  Result:=Assigned(fxy)
end;

function TRel_XY.Get_MinX: Double;
begin
  Result:=fxy[3].x
end;

function TRel_XY.Get_MinY: Double;
begin
  Result:=fxy[3].y
end;
                                        
function TRel_XY.Get_MaxX: Double;
begin
  Result:=fxy[1].x
end;

function TRel_XY.Get_MaxY: Double;
begin
  Result:=fxy[1].y
end;

constructor TRlzDump.Create;
begin
  inherited;
  zlb:=tzlb.Create;
end;

destructor TRlzDump.Destroy;
begin
  Close_doc; zlb.Free;
  inherited
end;

function TRlzDump.Get_Active: Boolean;
begin
  Result:=fHandle > 0
end;

function TRlzDump.GetPath: PChar;
begin
  Result:=fPath
end;

function TRlzDump.Get_TileMeters: TGauss;
begin
  Result.x:=0;
  Result.y:=0;
  with fHdr do
  if XTiles > 0 then
  if YTiles > 0 then begin
    Result.x:=(xmax-xmin) / YTiles;
    Result.y:=(ymax-ymin) / XTiles;
  end
end;

procedure TRlzDump.set_zrange(z1,z2: float);
var
  g_dz,a,k: Double;
begin
  if z1 < z2 then begin
    a:=Trunc(z1-100);
    g_dz:=(z2 - z1) + 200;

    k:=0.01;
    if g_dz/k > 64000 then k:=0.02;
    if g_dz/k > 64000 then k:=0.05;
    if g_dz/k > 64000 then k:=0.1;
    if g_dz/k > 64000 then k:=0.2;
    if g_dz/k > 64000 then k:=0.5;
    if g_dz/k > 64000 then k:=1;
    if g_dz/k > 64000 then k:=2;
    if g_dz/k > 64000 then k:=5;

    fHdr.a0:=-a; fHdr.k0:=k;
  end
end;

function TRlzDump.Begin_doc(APath: PChar;
                            nx,ny,page: Integer;
                            gx1,gx2,gy1,gy2,gz1,gz2: Double;
                            const sys: tsys): Boolean;
var
  g_dx,g_dy,g_dz,a,k: Double; cx,cx1,cx2: Integer;
begin
  Result:=false;

  StrUpdateExt(fPath,APath,'.RLZ');

  Fillchar(fHdr,Sizeof(fHdr),0);
  with fHdr do begin
    mark:='rlz0';
    tile_nx:=page;
    tile_ny:=page;

    fHdr.a0:=5000; fHdr.k0:=1;

    if gz1 < gz2 then
      set_zrange(gz1,gz2)
    else begin
      g_dx:=gx2-gx1; g_dy:=gy2-gy1;

      if (g_dx / nx <= 10)
      or (g_dy / ny <= 10) then fHdr.k0:=0.5
      else
      if (g_dx / nx <= 5)
      or (g_dy / ny <= 5) then fHdr.k0:=0.2;
    end;

    buf_len:=page * page * 2;
    ibuf:=xAllocPtr(buf_len * 6);
    obuf:=@ibuf[buf_len div 2];

    XTiles:=nx div (page-1);
    YTiles:=ny div (page-1);

    NTiles:=XTiles * YTiles;

    if NTiles > 0 then begin
      off_len:=NTiles * Sizeof(Integer);

      cx:=3; if fIs_Micro then Inc(cx,4);

      _offp:=xAllocInt(NTiles * cx);

      if Assigned(_offp) then begin
        _lenp:=@_offp[NTiles];
        _zrange:=@_lenp[NTiles];
      end;

      fHorzWidth:=XTiles * page;
      fVertWidth:=YTiles * page;

      cx1:=fHorzWidth * (YTiles+1);
      cx2:=fVertWidth * (XTiles+1);

      fHorzBuf:=xAllocPtr((cx1 + cx2) * SizeOf(Word));

      if Assigned(fHorzBuf) then
      fVertBuf:=@fHorzBuf[cx1]
    end;

    fHdr.offp:=Sizeof(fHdr);
    fHdr.lenp:=fHdr.offp + off_len;
    fHdr.zrange:=fHdr.lenp + off_len;

    if fIs_Micro then begin
      fHdr.m_offp:=fHdr.zrange + off_len;
      fHdr.m_lenp:=fHdr.m_offp + off_len;
      _m_offp:=@_zrange[NTiles];
      _m_lenp:=@_m_offp[NTiles];

      fHdr.c_offp:=fHdr.m_lenp + off_len;
      fHdr.c_lenp:=fHdr.c_offp + off_len;
      _c_offp:=@_m_lenp[NTiles];
      _c_lenp:=@_c_offp[NTiles];
    end;

    xmin:=gx1; xmax:=gx2;
    ymin:=gy1; ymax:=gy2;

    fstepx:=(gy2-gy1) / (XTiles*(page-1)+1);
    fstepy:=(gx2-gx1) / (YTiles*(page-1)+1);

    s:=sys; s.x0:=0; s.y0:=0
  end;

  if zlb.Active then

  if NTiles > 0 then
  if Assigned(ibuf) then
  if Assigned(_offp) then begin
    fHandle:=FileCreate(fPath);
    if fHandle > 0 then begin
      FileWrite(fHandle,fHdr,Sizeof(fHdr));

      fHdr.offp:=FileSeek(fHandle,0,2);
      FileWrite(fHandle,_offp^,off_len);

      fHdr.lenp:=FileSeek(fHandle,0,2);
      FileWrite(fHandle,_lenp^,off_len);

      fHdr.zrange:=FileSeek(fHandle,0,2);
      FileWrite(fHandle,_zrange^,off_len);

      if fHdr.offp > 0 then
      if fHdr.lenp > 0 then
      if fHdr.zrange > 0 then begin

        if fIs_Micro then begin
          fHdr.m_offp:=FileSeek(fHandle,0,2);
          FileWrite(fHandle,_m_offp^,off_len);

          fHdr.m_lenp:=FileSeek(fHandle,0,2);
          FileWrite(fHandle,_m_lenp^,off_len);

          fHdr.c_offp:=FileSeek(fHandle,0,2);
          FileWrite(fHandle,_c_offp^,off_len);

          fHdr.c_lenp:=FileSeek(fHandle,0,2);
          FileWrite(fHandle,_c_lenp^,off_len);
        end;

        Result:=true
      end
    end
  end;

  with fHdr do
  fRelIndex:=Point(XTiles,YTiles)
end;

function TRlzDump.Clone(APath: PChar; Ahdr: TRlzHeader): Boolean;
var
  page,cx1,cx2: int; ext: TShortstr;
begin
  Result:=false;

  StrCopy(fPath,APath);

  StrExt(ext,APath);
  if StrComp(ext,'.rlz1') <> 0 then
  if StrComp(ext,'.rlz2') <> 0 then
  if StrComp(ext,'.rlz3') <> 0 then
  StrUpdateExt(fPath,fPath,'.rlz');

  fHdr:=Ahdr;

  page:=fHdr.tile_nx;

  buf_len:=page * page * 2;
  ibuf:=xAllocPtr(buf_len * 6);
  obuf:=@ibuf[buf_len div 2];

  NTiles:=fHdr.XTiles * fHdr.YTiles;

  if fHdr.tile_nx = fHdr.tile_ny then
  if NTiles > 0 then begin

    off_len:=NTiles * Sizeof(Integer);

    _offp:=xAllocInt(NTiles * 3);

    if Assigned(_offp) then begin
      _lenp:=@_offp[NTiles];
      _zrange:=@_lenp[NTiles];
    end;

    fHorzWidth:=fHdr.XTiles * page;
    fVertWidth:=fHdr.YTiles * page;

    cx1:=fHorzWidth * (fHdr.YTiles+1);
    cx2:=fVertWidth * (fHdr.XTiles+1);

    fHorzBuf:=xAllocPtr((cx1 + cx2) * SizeOf(Word));

    if Assigned(fHorzBuf) then
    fVertBuf:=@fHorzBuf[cx1];

    fHdr.m_offp:=0; _m_offp:=nil;
    fHdr.m_lenp:=0; _m_lenp:=nil;

    fHdr.c_offp:=0; _c_offp:=nil;
    fHdr.c_lenp:=0; _c_lenp:=nil;

    with fHdr do begin
      fstepx:=(ymax-ymin) / (XTiles*(page-1)+1);
      fstepy:=(xmax-xmin) / (YTiles*(page-1)+1);
      fRelIndex:=Point(XTiles,YTiles)
    end;

    if zlb.Active then

    if Assigned(ibuf) then
    if Assigned(_offp) then begin
      fHandle:=FileCreate(fPath);
      if fHandle > 0 then begin
        FileWrite(fHandle,fHdr,Sizeof(fHdr));

        fHdr.offp:=FileSeek(fHandle,0,2);
        FileWrite(fHandle,_offp^,off_len);

        fHdr.lenp:=FileSeek(fHandle,0,2);
        FileWrite(fHandle,_lenp^,off_len);

        fHdr.zrange:=FileSeek(fHandle,0,2);
        FileWrite(fHandle,_zrange^,off_len);

        if fHdr.offp > 0 then
        if fHdr.lenp > 0 then
        if fHdr.zrange > 0 then
        Result:=true
      end
    end
  end
end;

procedure TRlzDump.Close_doc;
begin
  if fHandle > 0 then begin
    FileSeek(fHandle,0,0);
    FileWrite(fHandle,fHdr,Sizeof(fHdr));

    FileSeek(fHandle,fHdr.offp,0);
    FileWrite(fHandle,_offp^,off_len);

    FileSeek(fHandle,fHdr.lenp,0);
    FileWrite(fHandle,_lenp^,off_len);

    FileSeek(fHandle,fHdr.zrange,0);
    FileWrite(fHandle,_zrange^,off_len);

    if fIs_Micro then begin
      FileSeek(fHandle,fHdr.m_offp,0);
      FileWrite(fHandle,_m_offp^,off_len);

      FileSeek(fHandle,fHdr.m_lenp,0);
      FileWrite(fHandle,_m_lenp^,off_len);

      FileSeek(fHandle,fHdr.c_offp,0);
      FileWrite(fHandle,_c_offp^,off_len);

      FileSeek(fHandle,fHdr.c_lenp,0);
      FileWrite(fHandle,_c_lenp^,off_len);
    end;

    FileClose(fHandle); fHandle:=0
  end;

  _offp:=xFreeptr(_offp); _lenp:=nil;
  ibuf:=xFreeptr(ibuf); obuf:=nil;

  fHorzBuf:=xFreeptr(fHorzBuf);
  fVertBuf:=nil; fIs_z:=false
end;

function TRlzDump.Refresh_min_max: Integer;
var
  i,n,z1,z2: Integer; zr: tlong;
begin
  Result:=0; z1:=1; z2:=0;

  n:=fHdr.XTiles * fHdr.YTiles;
  for i:=0 to n-1 do begin
    zr.i:=_zrange[i];
    if zr.i <> 0 then begin

      with zr do
      if z1 > z2 then begin
        z1:=w[0]; z2:=w[1];
      end
      else begin
        if w[0] < z1 then z1:=w[0];
        if w[1] > z2 then z2:=w[1];
      end;

      Inc(Result)
    end
  end;

  fHdr.zmin:=z1 * fHdr.k0 - fHdr.a0;
  fHdr.zmax:=z2 * fHdr.k0 - fHdr.a0;
end;

procedure TRlzDump.verify;

procedure left_verify;
var
  wp1,wp2: PWords;
  pg_i,pg_j,len,ok: Integer;
begin
  len:=buf_len;

  for pg_j:=0 to fHdr.YTiles-1 do begin

    ok:=0;
    for pg_i:=0 to fHdr.XTiles-2 do
    if ReadTile(pg_i,pg_j) then begin

      if ok = 1 then begin
      end;

      Move(obuf^,ibuf^,len); ok:=1
    end
  end
end;

begin
  left_verify;
end;

function TRlzDump.ReadTile(pg_i,pg_j: Integer): Boolean;
var
  ind,bx,cx: Integer; tmp: POinter;
begin
  Result:=false;

  if (pg_i >= 0) and (pg_i < fHdr.XTiles) then
  if (pg_j >= 0) and (pg_j < fHdr.YTiles) then begin

    ind:=pg_j * fHdr.XTiles + pg_i;
    bx:=_offp[ind]; cx:=_lenp[ind];

    if (bx > 0) and (cx > 0) then begin

      tmp:=@obuf[buf_len];
      FileSeek(fHandle,bx,0);
      FileRead(fHandle,tmp^,cx);
      FileSeek(fHandle,0,2);

      Result:=zlb.xDecompress(tmp,cx,obuf,buf_len);
    end
  end
end;

procedure TRlzDump.Pack_tile(pg_i,pg_j: Integer; buf: PWords);

procedure lace_refresh(pg_i,pg_j: Integer; buf: PWords);
var
  i,bx: Integer; si1,si2, di1,di2: PWords;
begin
  if Assigned(fHorzBuf) then
  with fHdr do begin

    si1:=buf;
    si2:=@buf[(tile_ny-1) * tile_nx];

    di1:=@fHorzBuf[pg_j*fHorzWidth +
                   pg_i*(tile_nx-1)];

    di2:=@di1[fHorzWidth];

    for i:=1 to tile_nx do begin
      if di1[0] > 0 then si1[0]:=di1[0]
                    else di1[0]:=si1[0];

      if di2[0] > 0 then si2[0]:=di2[0]
                    else di2[0]:=si2[0];

      si1:=@si1[1]; di1:=@di1[1];
      si2:=@si2[1]; di2:=@di2[1];
    end;

    bx:=tile_nx;
    si1:=buf; si2:=@buf[bx-1];

    di1:=@fVertBuf[pg_i*fVertWidth +
                   pg_j*(tile_ny-1)];

    di2:=@di1[fVertWidth];

    for i:=1 to tile_ny do begin
      if di1[0] > 0 then si1[0]:=di1[0]
                    else di1[0]:=si1[0];

      if di2[0] > 0 then si2[0]:=di2[0]
                    else di2[0]:=si2[0];

      si1:=@si1[bx]; di1:=@di1[1];
      si2:=@si2[bx]; di2:=@di2[1];
    end
  end
end;

function tile_min_max(ind: Integer; buf: PWords): Integer;
var
  z1,z2,z: Word; i,cx: Integer;
  si: PWords; zr: tlong;
begin
  Result:=0; z1:=1; z2:=0;

  si:=buf; with fHdr do
  cx:=tile_nx * tile_ny;

  for i:=1 to cx do begin

    z:=si[0];
    if z > 0 then begin

      if Result > 0 then begin
        if z < z1 then z1:=z;
        if z > z2 then z2:=z;
      end
      else begin
        z1:=z; z2:=z
      end;

      Inc(Result)
    end;

    si:=@si[1]
  end;

  zr.w[0]:=z1; zr.w[1]:=z2;
  _zrange[ind]:=zr.i
end;

procedure error(pg_i,pg_j: Integer; msg: String);
begin
  if Assigned(flog) then
  flog.Add(Format('tile %d_%d: '+msg,[pg_i,pg_j]))
end;

var
  bx,cx: int;
begin
  lace_refresh(pg_i,pg_j,buf);

  bx:=pg_j * fHdr.XTiles + pg_i;
  if tile_min_max(bx,buf) > 0 then begin

    cx:=0;
    if fHdr.ver = 0 then
    cx:=zlb.xCompress(buf,buf_len,obuf,buf_len*2);

    if cx > 0 then begin
      _offp[bx]:=FileSeek(fHandle,0,2);
      FileWrite(fHandle,obuf^,cx);
      _lenp[bx]:=cx;
    end;

    Error(pg_i,pg_j,'ofs='+IntToStr(_offp[bx])+
                    ' ,len='+IntToStr(_lenp[bx]));
  end
end;

function TRlzDump.g_to_p(gx,gy: double): TPoint;
begin
  Result.X:=Round( (gy-Hdr.ymin)/stepx );
  Result.Y:=Round( (Hdr.xmax-gx)/stepy );
end;

function TRlzDump.Add_rel(Tile: PChar; pg_i,pg_j: Integer): Boolean;
var
  rel: TRelief; z1,z2: Float;
  i,j,_i,_j, ni,nj, x,y,w,h: Integer;
  is_left,is_top: Boolean;
begin
  Result:=false;

  rel:=TRelief.Create;
  try
    if rel.Open(Tile) then begin

      is_left:=pg_i = fRelIndex.x+1;
      is_top:=pg_j > 0;

      fRelIndex:=Point(pg_i,pg_j);

      w:=fHdr.tile_nx; h:=fHdr.tile_ny;

      ni:=int_Tiles(rel.Width-1,w-1);
      nj:=int_Tiles(rel.Height-1,h-1);

      pg_i:=pg_i * ni; pg_j:=pg_j * nj;

      if pg_i < fHdr.XTiles then
      if pg_j < fHdr.YTiles then begin

        z1:=rel.hdr.zmin;
        z2:=z1 + rel.hdr.zmax;

        with fHdr do
        if fIs_z then begin
          zmin:=Min(zmin,z1);
          zmax:=Max(zmax,z2);
        end
        else begin
          zmin:=z1; zmax:=z2;
          fIs_z:=true
        end;

        for j:=0 to nj-1 do
        for i:=0 to ni-1 do begin

          _i:=pg_i + i; _j:=pg_j + j;

          if _i < fHdr.XTiles then
          if _j < fHdr.YTiles then begin

            x:=i*w-i; y:=j*h-j; with fHdr do
            rel.Get_rlz(x,y,w,h, a0,k0, ibuf);

            pack_tile(_i,_j,ibuf)
          end
        end;

        Result:=true
      end
    end;
  finally
    rel.Free
  end
end;

function TRlzDump.Add_buf(rel: PFloats; rw,rh: Integer;
                          pg_i,pg_j: Integer): Boolean;
var
  z1,z2: Float;
  i,j,_i,_j, ni,nj, x,y,w,h: Integer;
  is_left,is_top: Boolean;
begin
  Result:=false;

  is_left:=pg_i = fRelIndex.x+1;
  is_top:=pg_j > 0;

  fRelIndex:=Point(pg_i,pg_j);

  w:=fHdr.tile_nx; h:=fHdr.tile_ny;

  ni:=int_Tiles(rw-1,w-1);
  nj:=int_Tiles(rh-1,h-1);

  pg_i:=pg_i * ni; pg_j:=pg_j * nj;

  if pg_i < fHdr.XTiles then
  if pg_j < fHdr.YTiles then

  if r4_min_max(rel,rw*rh,h_null1, z1,z2) then begin

    with fHdr do
    if fIs_z then begin
      zmin:=Min(zmin,z1);
      zmax:=Max(zmax,z2);
    end
    else begin
      zmin:=z1; zmax:=z2;
      fIs_z:=true
    end;

    for j:=0 to nj-1 do
    for i:=0 to ni-1 do begin

      _i:=pg_i + i; _j:=pg_j + j;

      if _i < fHdr.XTiles then
      if _j < fHdr.YTiles then begin

        x:=i*w-i; y:=j*h-j; with fHdr do
        r4_get_rlz(rel,rw,rh, x,y,w,h, a0,k0, ibuf);

        pack_tile(_i,_j,ibuf)
      end
    end;

    Result:=true
  end
end;

function TRlzDump.Add_clu(Tile: PChar; pg_i,pg_j: Integer): Boolean;

procedure Get_clu(clu: TReadFile; clu_w,clu_h: Integer;
                  x,y,w,h: Integer; di: PBytes);
var
  si,_si: PWords; i,j,bx: Integer;
begin
  if (x >= 0) and (x < clu_w) then
  if (y >= 0) and (y < clu_h) then begin

    if y + h > clu_h then h:=clu_h - y;
    si:=@PWords(clu.Buf) [y*clu_w + x];

    for j:=1 to h do begin
      _si:=si; bx:=x;

      for i:=1 to w do begin

        if bx < clu_w then begin
          di[0]:=tword(_si[0]).b[1];
          _si:=@_si[1]; Inc(bx)
        end;

        di:=@di[1]
      end;

      si:=@si[clu_w]
    end
  end
end;

var
  mrf: TRelief; clu: TReadFile;
  i,j,_i,_j, ni,nj, w,h, cx,ind: Integer;
  fn: TShortstr;
begin
  mrf:=TRelief.Create;
  try
    StrUpdateExt(fn,Tile,'.MRF');
    if fIs_Micro and FileExist(fn) then

    if mrf.Open(fn) then
    if mrf.hdr.fmt = 1 then begin

      w:=fHdr.tile_nx - 1;
      h:=fHdr.tile_ny - 1;

      ni:=mrf.Width div w;
      nj:=mrf.Height div h;

      pg_i:=pg_i * ni; pg_j:=pg_j * nj;

      if pg_i < fHdr.XTiles then
      if pg_j < fHdr.YTiles then begin

        for j:=0 to nj-1 do
        for i:=0 to ni-1 do begin

          _i:=pg_i + i; _j:=pg_j + j;

          if _i < fHdr.XTiles then
          if _j < fHdr.YTiles then begin

            mrf.Get_mrf(i*w,j*h,w,h, @ibuf[0]);
            cx:=zlb.xCompress(ibuf,w * h,obuf,buf_len*2);

            if cx > 0 then begin
              ind:=_j * fHdr.XTiles + _i;
              _m_offp[ind]:=FileSeek(fHandle,0,2);
              FileWrite(fHandle,obuf^,cx);
              _m_lenp[ind]:=cx;
            end
          end
        end;

        clu:=TReadFile.Create;
        try
          if clu.Open(StrUpdateExt(fn,Tile,'.GSM')) then
          if clu.Size = mrf.Width * mrf.Height * 2 then

          for j:=0 to nj-1 do
          for i:=0 to ni-1 do begin

            _i:=pg_i + i; _j:=pg_j + j;

            if _i < fHdr.XTiles then
            if _j < fHdr.YTiles then begin

              Get_clu(clu, mrf.Width,mrf.Height,
                      i*w,j*h,w,h, @ibuf[0]);

              cx:=zlb.xCompress(ibuf,w * h,obuf,buf_len*2);

              if cx > 0 then begin
                ind:=_j * fHdr.XTiles + _i;
                _c_offp[ind]:=FileSeek(fHandle,0,2);
                FileWrite(fHandle,obuf^,cx);
                _c_lenp[ind]:=cx;
              end
            end
          end;

        finally
          clu.Free
        end;

        Result:=true
      end
    end;
  finally
    mrf.Free
  end
end;

function TRlzDump.Add_tile(Tile: PFloats; pg_i,pg_j: Integer): Boolean;
var
  cx,rc: Integer; a0,k0,z,z1,z2: Float;
  si: PFloats; di: PWords;
begin
  Result:=false; rc:=0;

  si:=Tile; di:=ibuf;

  a0:=fHdr.a0; k0:=fHdr.k0;
  cx:=fHdr.tile_nx * fHdr.tile_ny;

  while cx > 0 do begin z:=si[0];

    if z <= -a0 then
      di[0]:=0
    else begin
      if rc = 0 then begin
        z1:=z; z2:=z
      end
      else begin
        if z < z1 then z1:=z;
        if z > z2 then z2:=z;
      end;

      z:=Max(-a0,z) + a0; Inc(rc);
      di[0]:=Min($FFFF,Round(z/k0));
    end;

    si:=@si[1]; di:=@di[1];
    Dec(cx)
  end;

  if rc > 0 then begin

    with fHdr do
    if fIs_z then begin
      zmin:=Min(zmin,z1);
      zmax:=Max(zmax,z2);
    end
    else begin
      zmin:=z1; zmax:=z2;
      fIs_z:=true
    end;

    Pack_tile(pg_i,pg_j,ibuf);

    Result:=true
  end
end;

function TRlzDump.Add_tileW(Tile: PWords; pg_i,pg_j: Integer): Boolean;
var
  i,n,z,z1,z2: int; si: PWords; fz1,fz2: Float;
begin
  Result:=false;

  si:=Tile; z1:=$FFFF; z2:=0;

  n:=fHdr.tile_nx * fHdr.tile_ny;
  for i:=0 to n-1 do begin
    z:=si[i]; if z > 0 then
    if z < z1 then z1:=z;
    if z > z2 then z2:=z;
  end;

  if z2 > 0 then
  if z1 <= z2 then

  with fHdr do begin
    fz1:=z1 * k0 - a0;
    fz2:=z2 * k0 - a0;

    if fIs_z then begin
      zmin:=Min(zmin,fz1);
      zmax:=Max(zmax,fz2);
    end
    else begin
      zmin:=fz1; zmax:=fz2;
      fIs_z:=true
    end;

    Pack_tile(pg_i,pg_j,Tile);
    Result:=true
  end
end;

constructor TRlzPyrList.Create;
begin
  inherited Create(Sizeof(TRlzPyrArr),16)
end;

constructor TRelief.Create;
begin
  inherited;
  zlb:=tzlb.Create;
  fTile:=@tiles[0];
end;

destructor TRelief.Destroy;

function Refresh_zmin_zmax: int;
var
  i,n: int; zp: PIntegers;
  z1,z2,_z1,_z2: Word;
  hdr: TRlzHeader;
begin
  Result:=0; z1:=0; z2:=0;

  with frlz do n:=XTiles * YTiles;

  zp:=_zrange;
  for i:=0 to n-1 do
  if _offp[i] > 0 then
  if _lenp[i] > 0 then begin

    _z1:=tlong(zp[i]).w[0];
    _z2:=tlong(zp[i]).w[1];

    if _z1 < _z2 then begin

      if Result = 0 then begin
        z1:=_z1; z2:=_z2;
      end
      else begin
        if _z1 < z1 then z1:=_z1;
        if _z2 > z2 then z2:=_z2;
      end;

      Inc(Result)
    end
  end;

  if Result > 0 then begin
    fFile.Load(0,hdr,Sizeof(TRlzHeader));
    hdr.zmin:=z1 * hdr.k0 - hdr.a0;
    hdr.zmax:=z2 * hdr.k0 - hdr.a0;
    fFile.Store(0,hdr,Sizeof(TRlzHeader))
  end
end;

var
  nt: int;
begin
  if fis_upd then
  if Assigned(fFile) then
  if Assigned(_zrange) then
  if Assigned(_offp) then
  if Assigned(_lenp) then

  if fUpdateCount > 0 then begin
    Refresh_zmin_zmax;
    nt:=rlz.XTiles*rlz.YTiles;
    fFile.Store(rlz.zrange,_zrange^,nt*4)
  end;

  fFile:=nil; fData:=nil;

  coeff:=xFreePtr(coeff);

  fTempBuf:=xFreePtr(fTempBuf);
  fBuffer:=xFreePtr(fBuffer);

  zlb.Free; inherited
end;

function TRelief.Open(Path: PChar): Boolean;
var
  ip,p1,p2: PIntegers;
  i,k1,k2,nt,bx,dx: int; p: PBytes;
  tr: Real3x3; kx,ky: Double;
begin
  Result:=false; fIs_rlz:=false;

  fFile:=nil; fData:=nil; fUpdateCount:=0;

  StrCopy(fEditPath,''); fVersion:=0;

  Fillchar(hdr,Sizeof(hdr),0);
  Fillchar(frlz,Sizeof(frlz),0);

  _offp:=nil; _lenp:=nil;
  _zrange:=nil; _zrange1:=nil;
  fWords:=nil;

  Clear_layer(fMicro);
  Clear_layer(fClutter);

  for i:=0 to tiles_max-1 do begin
    tiles[i].Index:=Point(-1,-1);
    tiles[i].Rect:=Rect(0,0,-1,-1);
  end;

  fTempBuf:=xFreePtr(fTempBuf);

  fTile:=@tiles[0];

  if xStrLen(Path) > 0 then begin

    if fis_upd then
      GetUpdateFileIntf(Path,fFile)
    else
      GetOpenFileIntf(Path,fFile);

    if Assigned(fFile) then
    if is_rel_ds(fFile,hdr) then begin

      frlz.XTiles:=1;
      frlz.YTiles:=1;
      frlz.tile_nx:=hdr.nx;
      frlz.tile_ny:=hdr.ny;

      frlz.a0:=-10000;

      if hdr.len = 1 then hdr.fmt:=1;

      hdr.zmax:=Max(1,hdr.zmax-hdr.zmin);

      if hdr.xstep < 0.001 then begin
        hdr.xstep:=(hdr.xmax - hdr.xmin) / (hdr.nx-1) / 100;
        hdr.ystep:=(hdr.ymax - hdr.ymin) / (hdr.ny-1) / 100
      end;

      frlz.xmin:=hdr.xmin; frlz.ymin:=hdr.ymin;
      frlz.xmax:=hdr.xmax; frlz.ymax:=hdr.ymax;

      fData:=fFile.Seek(hdr.data,fFile.GetSize-hdr.data);

      Result:=true
    end else

    if is_rlz_ds(fFile,frlz) then begin

      Fillchar(hdr,Sizeof(hdr),0);

      with rlz do begin
        hdr.nx:=(tile_nx-1) * XTiles + 1;
        hdr.ny:=(tile_ny-1) * YTiles + 1;
      end;

      fByteScale.x:=(hdr.nx-2)/(hdr.nx-1);
      fByteScale.y:=(hdr.ny-2)/(hdr.ny-1);

      nt:=rlz.XTiles * rlz.YTiles; k1:=2;

      bx:=Tile_size; k2:=1;
      if fDoubleBuffered then k2:=tiles_max;
      Inc(k2,2);

      if rlz.m_offp > 0 then begin Inc(k1,2); Inc(k2) end;
      if rlz.c_offp > 0 then begin Inc(k1,2); Inc(k2) end;
      if rlz.zrange > 0 then Inc(k1);

      fBuffer:=xReAllocPtr(fBuffer,(nt*k1*4) + bx*k2);

      if Assigned(fBuffer) then begin

        ip:=fBuffer;

        fTileBuf:=@ip[nt*k1];

        p:=fTileBuf; if Assigned(p) then
        for i:=0 to tiles_max-1 do begin
          tiles[i].Buf:=Pointer(p); p:=@p[bx];
          if not fDoubleBuffered then Break
        end;

        if rlz.m_offp > 0 then begin
          fMicro.Buf:=p; p:=@p[bx];
        end;

        if rlz.c_offp > 0 then begin
          fClutter.Buf:=p; p:=@p[bx];
        end;

        fTree:=Pointer(p);

        fWords:=fTile.Buf; fIs_rlz:=true;

        if rlz.zmin < -5000 then frlz.zmin:=-5000;
        if rlz.zmax >= 10000 then frlz.zmax:=10000;

        hdr.zmin:=rlz.zmin;
        hdr.zmax:=Max(1,rlz.zmax-rlz.zmin);

        hdr.xmin:=rlz.xmin; hdr.ymin:=rlz.ymin;
        hdr.xmax:=rlz.xmax; hdr.ymax:=rlz.ymax;

        hdr.xstep:=(rlz.xmax - rlz.xmin) / (hdr.ny-1);
        hdr.ystep:=(rlz.ymax - rlz.ymin) / (hdr.nx-1);

        _offp:=ip; ip:=@ip[nt];
        _lenp:=ip; ip:=@ip[nt];

        if ds_get_tat(fFile,
                      _offp,rlz.offp,
                      _lenp,rlz.lenp,
                      nt,false) then begin

          for i:=0 to nt-1 do begin
            dx:=_offp[i]; if dx > 0 then Break
          end;

          bx:=rlz.lenp - rlz.offp;

          if rlz.m_offp > 0 then begin
            p1:=ip; ip:=@ip[nt];
            p2:=ip; ip:=@ip[nt];

            if ds_get_tat(fFile,
                          p1,rlz.m_offp,
                          p2,rlz.m_lenp,
                          nt,false) then begin
              fMicro.offp:=p1; fMicro.lenp:=p2;
              dx:=rlz.lenp + bx + bx
            end
          end;

          if rlz.c_offp > 0 then begin
            p1:=ip; ip:=@ip[nt];
            p2:=ip; ip:=@ip[nt];

            if ds_get_tat(fFile,
                          p1,rlz.c_offp,
                          p2,rlz.c_lenp,
                          nt,false) then begin
              fClutter.offp:=p1; fClutter.lenp:=p2;
              dx:=rlz.lenp + bx + bx
            end
          end;

          if rlz.zrange = rlz.lenp + bx then
          if dx >= rlz.zrange + bx then begin
            fFile.Load(rlz.zrange,ip^,nt*4);
            _zrange:=ip; _zrange1:=ip
          end;

          if _zrange = nil then frlz.zrange:=0;

          Result:=true
        end
      end
    end;

    if not Result then begin
      fFile:=nil; fData:=nil
    end
    else begin

      with hdr do begin
        kx:=1; if ystep > 0.001 then kx:=1/ystep;
        ky:=1; if xstep > 0.001 then ky:=1/xstep;
      end;

      Init_3x3(tr,hdr.xmax,-hdr.ymin,-1,1);
      xy_swap_3x3(tr); xy_Scale_3x3(tr,kx,ky);

      fMatrix:=tr; inverse_3x3(tr,fReverse);
    end
  end;

  if Assigned(fOnActivate) then
  fOnActivate(Self);

  Result:=Active; if Result then
  StrCopy(fEditPath,Path)
end;

procedure TRelief.Close;
begin
  if not Active then
  StrCopy(fEditPath,'');

  fFile:=nil; fData:=nil;

  if Assigned(fOnActivate) then
  fOnActivate(Self);
end;

function TRelief.GetPyr(List: TRlzPyrList): int;
var
  r: TRlzPyrRec; p: PRlzPyrRec;
  bx,nt,iz,iw,ih,tw,th: int;
begin
  Result:=0;

  if Assigned(List) then begin
    List.Clear;

    Fillchar(r,Sizeof(r),0);
    r.magic:=pyr_magic;
    r.Width:=Width;
    r.Height:=Height;
    r.XTiles:=rlz.XTiles;
    r.YTiles:=rlz.YTiles;
    r.offp:=rlz.offp;
    r.lenp:=rlz.lenp;
    r.zrange:=rlz.zrange;

    List.Add(@r); Inc(Result)
  end;

  if Assigned(fFile) then
  if rlz.m_offp > 0 then
  if rlz.m_lenp = 0 then begin

    iw:=Width; ih:=Height; iz:=1;
    tw:=rlz.tile_nx-1; th:=rlz.tile_ny-1;

    bx:=rlz.m_offp;
    while bx > 0 do begin
      p:=fFile.Seek(bx,Sizeof(r));
      if p = nil then Break;

      r:=p^; r.seek:=bx; r.zrange:=0;
      if r.magic <> pyr_magic then Break;

      iz:=iz*2;
      if r.Width <> int_Tiles(iw,iz) then Break;
      if r.Height <> int_Tiles(ih,iz) then Break;

      if r.XTiles <> int_Tiles(r.Width-1,tw) then Break;
      if r.YTiles <> int_Tiles(r.Height-1,th) then Break;

      nt:=r.XTiles*r.YTiles;
      if not ds_get_tat(fFile,
                        nil,r.offp,
                        nil,r.lenp,
                        nt,true) then Break;

      if Assigned(List) then List.Add(@r);
      Inc(Result); bx:=r.next
    end
  end
end;

procedure TRelief.SetPyrLevel(lev: PRlzPyrRec);
var
  i,nt: int;
begin
  hdr.nx:=lev.Width;
  hdr.ny:=lev.Height;

  frlz.XTiles:=lev.XTiles;
  frlz.YTiles:=lev.YTiles;

  frlz.offp:=lev.offp;
  frlz.lenp:=lev.lenp;
  frlz.zrange:=lev.zrange;

  _zrange:=nil;

  if Assigned(fFile) then begin

    nt:=rlz.XTiles*rlz.YTiles;

    fFile.Load(rlz.offp,_offp,nt*4);
    fFile.Load(rlz.lenp,_lenp,nt*4);

    if rlz.zrange > 0 then
    if Assigned(_zrange1) then begin
      fFile.Load(rlz.zrange,_zrange1,nt*4);
      _zrange:=_zrange1
    end
  end;

  for i:=0 to tiles_max-1 do begin
    tiles[i].Index:=Point(-1,-1);
    tiles[i].Rect:=Rect(0,0,-1,-1);
  end
end;

function TRelief.Open_edit: Boolean;
var
  fn: TShortstr;
begin
  StrCopy(fn,fEditPath);
  if FileExist(fn) then Open(fn);
  Result:=Active
end;

function TRelief.IsEmpty(h: Float): Boolean;
begin
  Result:=h < -frlz.a0 + 0.1
end;

function TRelief.Get_Active: Boolean;
begin
  Result:=Assigned(fFile)
end;

function TRelief.GetEditPath: PChar;
begin
  Result:=fEditPath
end;

function TRelief.Get_Tile_size: Integer;
begin
  Result:=0; with rlz do
  Result:=tile_nx * tile_ny * 2
end;

function TRelief.Get_row(Y: int): Pointer;
var
  cx: int;
begin
  Result:=nil;

  if not fIs_rlz then
  if Assigned(fFile) then
  if (y >= 0) and (y < hdr.ny) then begin
    cx:=hdr.nx * hdr.len;

    if cx > 0 then
    if Assigned(fData) then
      Result:=@fData[y*cx]
    else
      Result:=fFile.Seek(hdr.data + y*cx,cx)
  end
end;

procedure TRelief.Get_line(y: int; buf: PFloats);
var
  cx: int;
begin
  if not fIs_rlz then
  if Assigned(fFile) then
  if hdr.fmt = 5 then
  if (y >= 0) and (y < hdr.ny) then begin
    cx:=hdr.nx * Sizeof(float);

    if cx > 0 then
    if Assigned(fData) then
      Move(fData[y*cx],buf^,cx)
    else
      fFile.Load(hdr.data + y*cx,buf^,cx)
  end
end;

procedure TRelief.Put_line(y: int; buf: PFloats);
var
  cx: int;
begin
  if fIs_upd then
  if not fIs_rlz then
  if Assigned(fFile) then
  if hdr.fmt = 5 then
  if (y >= 0) and (y < hdr.ny) then begin
    cx:=hdr.nx * Sizeof(float);
    fFile.Store(hdr.data + y*cx,buf^,cx);

    if Assigned(fData) then
    Move(buf^,fData[y*cx],cx)
  end
end;

function TRelief.Get_z(x,y: int; out z: Double): Boolean;

function RectContainsPoint(R: PRect; x,y: Integer): Boolean;
begin
  Result:=(x >= R.Left) and (x <= R.Right) and
          (y >= R.Top) and (y <= R.Bottom)
end;

function seek_tile(x,y: Integer): Boolean;
var
  i,cx: Integer; p: PRlz_tile; tmp: TRlz_tile;
begin
  Result:=false; p:=fTile;

  if RectContainsPoint(@fTile.Rect, x,y) then
    Result:=true
  else
  if not fDoubleBuffered then
    Result:=xLoad_tile(x,y)
  else begin

    i:=0;
    while i < tiles_max do begin
      p:=@tiles[i]; Inc(i);

      if p.Index.X < 0 then Break else
      if RectContainsPoint(@p.Rect, x,y) then begin
        fTile:=p; Result:=true; Break
      end
    end;

    if not Result then begin

      if i = tiles_max then begin
        tmp:=tiles[0]; cx:=Sizeof(tmp) * (tiles_max-1);
        Move(tiles[1],tiles[0],cx); p^:=tmp;
      end;

      fTile:=p; Result:=xLoad_tile(x,y)
    end
  end;
end;

var
  ax,bx: int; loc: tdouble;
begin
  Result:=false; z:=h_null;

  if Assigned(fFile) then
  if (x >= 0) and (x < Width) then
  if (y >= 0) and (y < Height) then

  if fIs_rlz then begin

    if seek_tile(x,y) then begin

      Dec(x,fTile.Rect.Left);
      Dec(y,fTile.Rect.Top);

      if (x >= 0) and (x < rlz.tile_nx) then
      if (y >= 0) and (y < rlz.tile_ny) then

      with rlz do begin
        ax:=fTile.Buf[y * tile_nx + x];
        if ax = 0 then
          z:=h_null
        else begin
          z:=ax * k0 - a0;
          Result:=true
        end
      end
    end

  end
  else begin
    bx:=(y*hdr.nx+x)*hdr.len;

    if Assigned(fData) then
      Move(fData[bx],loc,hdr.len)
    else
      fFile.Load(hdr.data + bx,loc,hdr.len);

    case hdr.fmt of
  1:  z:=loc.b;
  2:  z:=loc.i;
  4:  z:=loc.l;
  5:  z:=loc.f;
  8:  z:=loc.d;
    end;

    Result:=z > h_null
  end
end;

function TRelief.Int_z(x,y: Double; out z: Double): Boolean;
var
  c: PFloats;
  i,ix,iy,rc,rc1,bit,x1,y1: Integer;
  v: Array[0..3] of Double; l: Double;
begin
  Result:=false;

  ix:=Trunc(x); iy:=Trunc(y); z:=h_null;

  if Assigned(fFile) then
  if (ix >= 0) and (iy >= 0) then begin

    if coeff = nil then begin
      coeff:=xAllocPtr(Sizeof(tinter_coeff));
      if Assigned(coeff) then
      fill_inter_coeff(coeff,bilinear);
    end;

    if coeff = nil then
      Result:=Get_z(Round(x),Round(y),z)
    else
    if (x <= Width-1) and (y <= Height-1) then begin

      if ix = Width-1 then Dec(ix);
      if iy = Height-1 then Dec(iy); rc:=0; rc1:=0;

      if Get_z(ix  ,iy  ,v[0]) then begin Inc(rc,1); Inc(rc1) end;
      if Get_z(ix+1,iy  ,v[1]) then begin Inc(rc,2); Inc(rc1) end;
      if Get_z(ix,iy+1  ,v[2]) then begin Inc(rc,4); Inc(rc1) end;
      if Get_z(ix+1,iy+1,v[3]) then begin Inc(rc,8); Inc(rc1) end;

      if rc = $F then begin
        c:=pixel_inter_coeff(x,y,coeff);
        z:=v[0]*c[0] + v[1]*c[1] + v[2]*c[2] + v[3]*c[3];
        Result:=true
      end else
      if rc1 = 3 then begin
        x:=x - ix; y:=y - iy;

        // Тест принадлежности треугольнику
        // Точка должна быть левее диагонали

                                  // diagonal vector
        if rc = 1+2+4 then begin  // +1,-1
          l:=(+1)*(y-(0)) - (-1)*(x-(1));
          if l <= 0 then begin
            z:=TriValued(x,y, v[0],v[2],v[1], triw[0]);
            Result:=true
          end
        end else
        if rc = 1+4+8 then begin  // -1,-1
          l:=(-1)*(y-(0)) - (-1)*(x-(0));
          if l <= 0 then begin
            z:=TriValued(x,y, v[0],v[2],v[3], triw[1]);
            Result:=true
          end
        end else
        if rc = 1+2+8 then begin  // +1,+1
          l:=(+1)*(y-(1)) - (+1)*(x-(1));
          if l <= 0 then begin
            z:=TriValued(x,y, v[0],v[3],v[1], triw[2]);
            Result:=true
          end
        end else
        if rc = 2+4+8 then begin  // -1,+1
          l:=(-1)*(y-(1)) - (+1)*(x-(0));
          if l <= 0 then begin
            z:=TriValued(x,y, v[2],v[3],v[1], triw[3]);
            Result:=true
          end
        end
      end else
      if rc1 > 0 then begin
        bit:=1; for i:=0 to 3 do begin

          if rc and bit <> 0 then begin
            x1:=ix; y1:=iy;
            if i > 0 then begin
              if i and 1 = 0 then Inc(x1);
              if i >= 2 then Inc(y1);
            end;

            if Abs(x-x1) < 0.01 then
            if Abs(y-y1) < 0.01 then begin
              z:=v[i]; Result:=true; Break;
            end
          end;

          bit:=bit shl 1
        end
      end
    end
  end
end;

function TRelief.Near_z(x,y: int; R: float; out z: Double): Boolean;
var
  cx,cy,dx,dy,ir,dr,ix,iy,x1,y1,x2,y2: int;
  loop: int; _r,_t,_z: double;
begin
  Result:=false; z:=h_null;

  cx:=x; cy:=y;

  if R <= Small then begin
    dx:=100; dy:=100
  end
  else begin
    dx:=Round(R/Abs(hdr.ystep));
    dy:=Round(R/Abs(hdr.xstep));
    dr:=Max(dx,dy);

    for ir:=1 to dr do begin

      x1:=cx-ir; x2:=cx+ir;
      y1:=cy-ir; y2:=cy+ir;

      iy:=y1;
      for loop:=1 to 2 do begin

        for ix:=x1 to x2 do
        if Get_z(ix,iy,_z) then begin
          _t:=Hypot(ix-cx,iy-cy);
          if not Result or (_t < _r) then begin
            _r:=_t; z:=_z; Result:=true
          end
        end;

        iy:=y2
      end;

      Inc(y1); Dec(y2); ix:=x1;

      for loop:=1 to 2 do begin

        for iy:=y1 to y2 do
        if Get_z(ix,iy,_z) then begin
          _t:=Hypot(ix-cx,iy-cy);
          if not Result or (_t < _r) then begin
            _r:=_t; z:=_z; Result:=true
          end
        end;

        ix:=x2
      end;

      if Result then Break
    end
  end
end;

function TRelief.sm_z(x,y: Integer; out z: Integer): Boolean;
var
  _z: Double;
begin
  Result:=Get_z(x,y,_z);
  z:=Round(_z*100)
end;

function TRelief.Get_Is_mrf: Boolean;
begin
  Result:=Assigned(fMicro.offp)
end;

function TRelief.Get_layer(var Layer: TLayer; x,y: int): int;
var
  i,j,w,h,cx,bx,dx: int; si: Pointer;
begin
  Result:=0;

  if Assigned(Layer.offp) then begin

    x:=Round(x * fByteScale.x); w:=rlz.tile_nx-1;
    y:=Round(y * fByteScale.y); h:=rlz.tile_ny-1;

    i:=x div w; j:=y div h;

    if (i >= 0) and (i < rlz.XTiles) then
    if (j >= 0) and (j < rlz.YTiles) then begin

      bx:=j * rlz.XTiles + i;

      if bx <> Layer.Index then begin

        dx:=Layer.offp[bx];
        cx:=Layer.lenp[bx];

        si:=nil;
        if cx > 0 then si:=fFile.Seek(dx,cx);

        Layer.Index:=bx; bx:=w * h;

        if si = nil then
          Fillchar(Layer.Buf^,bx,0)
        else
          zlb.xDecompress(si,cx,Layer.Buf,bx)
      end;

      Result:=Layer.Buf[(y mod h)*w + (x mod w)]
    end
  end
end;

function TRelief.Get_micro(x,y: Integer): Integer;
begin
  Result:=Get_Layer(fMicro, x,y)
end;

function TRelief.Get_clutter(x,y: Integer): Integer;
begin
  Result:=Get_Layer(fClutter, x,y)
end;

function TRelief.tile_Exist(i,j: Integer): Boolean;
var
  bx: Integer;
begin
  Result:=false;

  with frlz do
  if (i >= 0) and (i < XTiles) then
  if (j >= 0) and (j < YTiles) then begin

    bx:=(j * XTiles) + i;

    if Assigned(_offp) then
    if _offp[bx] > 0 then
    if _lenp[bx] > 0 then Result:=true
  end
end;

function TRelief.GetCompRatio: Float;
var
  i,j,k,bx: Integer; sz: Int64;
begin
  Result:=0;
  if Is_rlz then begin

    sz:=0; k:=0;
    for j:=0 to frlz.YTiles-1 do
    for i:=0 to frlz.XTiles-1 do begin

      bx:=(j * frlz.XTiles) + i;

      if _offp[bx] > 0 then
      if _lenp[bx] > 0 then begin
        Inc(k); Inc(sz,_lenp[bx]);
      end
    end;

    if sz > 0 then
    Result:=1 - sz / (k*Tile_size);
  end
end;

function TRelief.Seek_tile(ip,jp: int; out tp: PBytes): int;
var
  bx,cx,dx: int;
begin
  Result:=0; tp:=nil;

  if fIs_rlz then with frlz do
  if (ip >= 0) and (ip < XTiles) then
  if (jp >= 0) and (jp < YTiles) then begin

    bx:=(jp * XTiles) + ip;
    dx:=_offp[bx]; cx:=_lenp[bx];

    if (dx > 0) and (cx > 0) then begin
      tp:=fFile.Seek(dx,cx);
      Result:=cx
    end
  end
end;

function TRelief.Load_tile(ip,jp: int): Boolean;
var
  bx,cx,dx,size, x,y: int; si: PBytes;
begin
  Result:=false;

  if fIs_rlz then with fTile^ do

  if (Index.x = ip) and (Index.y = jp) then
    Result:=true
  else

  if Assigned(Buf) then

  with frlz do
  if (ip >= 0) and (ip < XTiles) then
  if (jp >= 0) and (jp < YTiles) then begin

    bx:=(jp * XTiles) + ip;

    dx:=_offp[bx]; if dx > 0 then begin

      cx:=_lenp[bx]; si:=nil;
      if cx > 0 then si:=fFile.Seek(dx,cx);

      size:=tile_size; if size > 0 then

      if si = nil then begin
        Fillchar(Buf^,tile_size,0);
        Result:=true
      end else
      if frlz.ver = 0 then
        Result:=zlb.xDecompress(si,cx,Buf,size);

      if Result then begin
        Index.x:=ip; Index.y:=jp;
        x:=ip * (tile_nx-1); y:=jp * (tile_ny-1);
        Rect.Left:=x; Rect.Right :=x + tile_nx - 1;
        Rect.Top :=y; Rect.Bottom:=y + tile_ny - 1;

        if fLeftAlign then begin
          Dec(Rect.Right); Dec(Rect.Bottom)
        end
      end
    end
  end
end;

function TRelief.xLoad_tile(x,y: int): Boolean;
var
  ip,jp: int;
begin
  if fLeftAlign then begin
    ip:=x div (rlz.tile_nx-1);
    jp:=y div (rlz.tile_ny-1)
  end
  else begin
    ip:=Max(0,x-1) div (rlz.tile_nx-1);
    jp:=Max(0,y-1) div (rlz.tile_ny-1)
  end;

  Result:=Load_tile(ip,jp)
end;

function TRelief.pyr_tile(pyr: THandle; buf: PWords; out len: int): int;
var
  cx,sz: int; si: Pointer;
begin
  Result:=0; len:=0;

  if Assigned(fFile) then

  if fIs_rlz then
  if fVersion = 0 then begin

    sz:=tile_size; if sz > 0 then begin

      if fTempBuf = nil then
      fTempBuf:=xAllocPtr(sz+sz);

      cx:=zlb.xCompress(buf,sz,fTempBuf,sz+sz);
      if cx > 0 then begin
        Result:=FileSeek(pyr,0,2);
        FileWrite(pyr,fTempBuf^,cx);
        len:=cx
      end
    end
  end
end;

function TRelief.add_pyr(var last,lev: TRlzPyrRec;
                         offp,lenp: PIntegers;
                         pyr: THandle): bool;
var
  upd: THandle;
  i,n,bx,cx,dx,sz: int;
  hdr: TRlzHeader;
  buf: Pointer;
begin
  Result:=false;

  if Assigned(fFile) then

  if fIs_rlz then
  if fVersion = 0 then begin

    sz:=tile_size; if sz > 0 then begin

      Inc(sz,sz);
      if fTempBuf = nil then
      fTempBuf:=xAllocPtr(sz);

      buf:=fTempBuf;
      if Assigned(buf) then begin

        fFile:=nil;
        upd:=FileOpen(fEditPath,fmOpenReadWrite);

        if upd <> 0 then begin

          n:=lev.XTiles*lev.YTiles;
          for i:=0 to n-1 do begin
            bx:=lenp[i]; cx:=0;
            if (bx > 0) and (bx < sz) then begin
              FileSeek(pyr,offp[i],0);
              cx:=FileRead(pyr,buf^,bx);

              if cx <= 0 then
                offp[i]:=0
              else begin
                offp[i]:=FileSeek(upd,0,2);
                FileWrite(upd,buf^,cx)
              end
            end;

            lenp[i]:=cx
          end;

          lev.magic:=pyr_magic;

          lev.offp:=FileSeek(upd,0,2);
          FileWrite(upd,offp^,n*4);

          lev.lenp:=FileSeek(upd,0,2);
          FileWrite(upd,lenp^,n*4);

          lev.seek:=FileSeek(upd,0,2);
          FileWrite(upd,lev,Sizeof(lev));

          if last.seek = 0 then begin
            FileSeek(upd,0,0);
            FileRead(upd,hdr,Sizeof(hdr));
            hdr.m_offp:=lev.seek;
            FileSeek(upd,0,0);
            FileWrite(upd,hdr,Sizeof(hdr));
          end
          else begin
            last.next:=lev.seek;
            FileSeek(upd,last.seek,0);
            FileWrite(upd,last,Sizeof(last));
          end;

          FileClose(upd);

          if fis_upd then
            GetUpdateFileIntf(fEditPath,fFile)
          else
            GetOpenFileIntf(fEditPath,fFile);

          Result:=true
        end
      end
    end
  end
end;

procedure TRelief.Update_tile(ip,jp: int; buf: PWords);
var
  upd: THandle;
  bx,cx,dx,sz: int; zr: tlong; 
begin
  if Assigned(fFile) then

  if fIs_rlz then
  if fVersion = 0 then

  with frlz do
  if (ip >= 0) and (ip < XTiles) then
  if (jp >= 0) and (jp < YTiles) then begin

    bx:=(jp * XTiles) + ip;

    sz:=tile_size; if sz > 0 then begin

      if fTempBuf = nil then
      fTempBuf:=xAllocPtr(sz+sz);

      cx:=zlb.xCompress(buf,sz,fTempBuf,sz+sz);
      if cx > 0 then

      if (_offp[bx] = 0)
      or (cx > _lenp[bx]) then begin

        fFile:=nil;
        upd:=FileOpen(fEditPath,fmOpenReadWrite);

        if upd <> 0 then begin
          dx:=FileSeek(upd,0,2);
          FileWrite(upd,fTempBuf^,cx);
          FileClose(upd);

          FileSeek(upd,rlz.offp+bx*4,0);
          FileWrite(upd,dx,4);

          FileSeek(upd,rlz.lenp+bx*4,0);
          FileWrite(upd,cx,4);

          _offp[bx]:=dx;
          _lenp[bx]:=cx
        end;

        if fis_upd then
          GetUpdateFileIntf(fEditPath,fFile)
        else
          GetOpenFileIntf(fEditPath,fFile);
      end else

      if fIs_upd then begin
        dx:=_offp[bx]; _lenp[bx]:=cx;
        fFile.Store(dx,fTempBuf^,cx);
        fFile.Store(rlz.lenp+bx*4,cx,4)
      end;

      if Assigned(_zrange) then begin
        zr.i:=tile_zmin_zmax(buf,tile_nx,tile_ny);
        _zrange[bx]:=zr.i; Inc(fUpdateCount)
      end
    end
  end
end;

procedure TRelief.Delete_tile(ip,jp: int);
var
  bx,ax: int;
begin
  if Assigned(fFile) then

  with frlz do
  if (ip >= 0) and (ip < XTiles) then
  if (jp >= 0) and (jp < YTiles) then begin
    bx:=(jp * XTiles) + ip;
    _offp[bx]:=0; _lenp[bx]:=0;

    ax:=0; if fis_upd then begin
      fFile.Store(rlz.offp+bx*4,ax,4);
      fFile.Store(rlz.lenp+bx*4,ax,4)
    end
  end
end;

procedure TRelief.Undo_tile(ip,jp: int; si: PBytes; cx: int);
var
  buf: PWords;
begin
  buf:=fTile.Buf; if Assigned(buf) then
  if zlb.xDecompress(si,cx,buf,tile_size) then
  Update_tile(ip,jp,buf)
end;

function TRelief.Get_tile(ip,jp,line: int; buf: PFloats): Boolean;
var
  x,y,bx: int; si: PWords; di: PFloats;
begin
  Result:=Load_tile(ip,jp);

  if Result then begin

    bx:=Max(0,line - fRlz.tile_nx);

    si:=fTile.Buf; di:=buf;

    for y:=1 to fRlz.tile_ny do begin

      for x:=1 to fRlz.tile_nx do begin
        di[0]:=si[0] * fRlz.k0 - fRlz.a0;
        si:=@si[1]; di:=@di[1]
      end;

      di:=@di[bx]
    end
  end
end;

function TRelief.frame_zrange(const lt,rb: TPoint;
                              out zr: TRange): Integer;
var
  i,j,i1,j1,i2,j2,w,h: Integer;
  z: tlong; w1,w2: Word;
begin
  Result:=1;

  zr.min:=hdr.zmin;
  zr.max:=hdr.zmin + hdr.zmax;

  if Is_rlz then
  if Assigned(_zrange) then
  with frlz do begin

    w:=rlz.tile_nx-1;
    h:=rlz.tile_ny-1;

    i1:=lt.X div w; i2:=rb.X div w;
    j1:=lt.Y div h; j2:=rb.Y div h;

    if i1 < 0 then i1:=0;
    if j1 < 0 then j1:=0;
    if i2 >= XTiles then i2:=XTiles-1;
    if j2 >= YTiles then j2:=YTiles-1;

    Result:=0; w1:=$FFFF; w2:=0;

    for j:=j1 to j2 do
    for i:=i1 to i2 do begin
      z.i:=_zrange[(j * XTiles) + i];
      if z.w[0] > 0 then
      if z.w[0] <= z.w[1] then begin
        if z.w[0] < w1 then w1:=z.w[0];
        if z.w[1] > w2 then w2:=z.w[1];
        Inc(Result)
      end
    end;

    if Result > 0 then begin
      zr.min:=w1 * k0 - a0;
      zr.max:=w2 * k0 - a0;
    end
  end
end;

function TRelief.Tile_zrange(ip,jp: int): TRange;
var
  z: tlong;
begin
  Result.min:=hdr.zmin;
  Result.max:=hdr.zmin + hdr.zmax;

  with frlz do
  if Assigned(_zrange) then
  if (ip >= 0) and (ip < XTiles) then
  if (jp >= 0) and (jp < YTiles) then begin

    z.i:=_zrange[(jp * XTiles) + ip];

    Result.min:=z.w[0] * k0 - a0;
    Result.max:=z.w[1] * k0 - a0;
  end
end;

function TRelief.tile_wrange(ip,jp: int): int;
begin
  Result:=0;

  with frlz do
  if Assigned(_zrange) then
  if (ip >= 0) and (ip < XTiles) then
  if (jp >= 0) and (jp < YTiles) then
  Result:=_zrange[(jp * XTiles) + ip];
end;

procedure TRelief.Get_rlz(x,y,w,h: int;
                          a0,k0: Float; buf: PWords);
var
  i,j,bx,cx,dx: int; fp,si: PFloats;
  di,_di: PWords; z: Float;
begin
  di:=buf;
  Fillchar(di^,w*h*2,0);

  if Assigned(fFile) then

  if not fIs_rlz then
  if hdr.fmt = 5 then

  if (x >= 0) and (x < Width) then
  if (y >= 0) and (y < Height) then begin

    fp:=Pointer(fData);
    if Assigned(fp) then fp:=@fp[x];

    cx:=Width; dx:=0;

    if fp = nil then begin
      cx:=cx * 4;
      dx:=hdr.data + x*4;
    end;

    bx:=w;
    if x+w > Width then bx:=Width-x;

    for j:=1 to h do
    if y < Height then begin

      if Assigned(fp) then
        si:=@fp[y*cx]
      else
        si:=fFile.Seek(y*cx+dx,bx*4);

      _di:=di;
      if Assigned(si) then
      for i:=1 to bx do begin
        z:=Max(-a0,si[0]) + a0;
        _di[0]:=Round(z / k0);
        si:=@si[1]; _di:=@_di[1];
      end;

      di:=@di[w]; Inc(y)
    end
  end
end;

procedure TRelief.Get_mrf(x,y,w,h: int; buf: PBytes);
var
  i,bx,cx,dx: int; fp,si,di: PBytes;
begin
  di:=buf;
  Fillchar(di^,w*h,0);

  if Assigned(fFile) then

  if (x >= 0) and (x < Width) then
  if (y >= 0) and (y < Height) then

  if not fIs_rlz then
  if hdr.fmt = 1 then begin

    fp:=fData;
    if Assigned(fp) then fp:=@fp[x];

    cx:=Width; bx:=w;
    if x+w > Width then bx:=Width-x;
    dx:=hdr.data + x;

    if bx > 0 then
    for i:=1 to h do
    if y < Height then begin

      if Assigned(fp) then
        si:=@fp[y*cx]
      else
        si:=fFile.Seek(y*cx+dx,bx);

      if Assigned(si) then
      Move(si[0],di[0],bx);

      di:=@di[w]; Inc(y)
    end
  end
end;

function TRelMatrix.Open(Path: PChar): Boolean;
var
  lg: lg_Transit; xy: TRel_XY;
begin
  Result:=false;

  if FileExist(Path) then
  if inherited Open(Path) then begin

    with prj do begin
      top.x:=rlz.xmax; top.y:=rlz.ymin;

      kx:=(rlz.ymax - rlz.ymin) / (Width-1);
      ky:=(rlz.xmin - rlz.xmax) / (Height-1);
      hdr.xstep:=Abs(ky); hdr.ystep:=Abs(kx);

      sys:=rlz.s; sys.x0:=0; sys.y0:=0;

      if sys.elp = 0 then
      if sys.prj <= 1 then sys.elp:=1;
    end;

    if not fis_rlz then

    if lg_LoadFrom(lg,Path) then begin
      FillChar(fprj,Sizeof(fprj),0);
      fprj.kx:=1; fprj.ky:=1;

      with fprj,lg do begin
        sys:=lg.s; top:=l[0];
        kx:=(l[1].y - l[0].y) / (Width-1);
        ky:=(l[3].x - l[0].x) / (Height-1);

        hdr.xstep:=Abs(ky); hdr.ystep:=Abs(kx);
        hdr.xmin:=l[3].x; hdr.ymin:=l[0].y;
        hdr.xmax:=l[0].x; hdr.ymax:=l[1].y;
        frlz.xmin:=hdr.xmin; frlz.ymin:=hdr.ymin;
        frlz.xmax:=hdr.xmax; frlz.ymax:=hdr.ymax;
      end;

      frlz.s:=lg.s;
    end
    else begin
      xy:=TRel_XY.Create;
      try
        if xy.Open(Path) then begin
          fprj.top.x:=xy.MaxX;
          fprj.top.y:=xy.MinY;
          fprj.kx:=(xy.MaxY-xy.MinY)/(hdr.nx-1);
          fprj.ky:=(xy.MinX-xy.MaxX)/(hdr.ny-1);
        end
      finally
        xy.Free
      end
    end;

    Result:=true
  end;

  if Result then
  with fprj.sys do
  if (pps = 0) and (prj = 0) then
  with fprj.top do
  sys_projection(fprj.sys, x,y)
end;

function TRelMatrix.Get_mpp: Double;
var
  g1,g2: TGauss;
begin
  p_to_g(0,0,g1); p_to_g(Width,Height,g2);
  Result:=Gauss_dist(g1,g2) / Hypot(Width,Height)
end;

function TRelMatrix.Get_bound(out xmin,ymin,xmax,ymax: double): Integer;
var
  i: Integer; r: xgeoid;
  l: LOrient; g: GOrient; lt,rb: TGauss;
begin
  Port_to_LPoly(0,0,Width,Height,@l);

  for i:=0 to 3 do begin
    p_to_r(l[i].X,l[i].Y,r);
    g[i]:=_Gauss(r.x,r.y)
  end;

  Max_Gauss_Bound(@g,4,lt,rb);

  xmin:=lt.x; ymin:=lt.y;
  xmax:=rb.x; ymax:=rb.y;

  Result:=fprj.sys.pps
end;

procedure TRelMatrix.Get_gbound(out xmin,ymin,xmax,ymax: double);
var
  i: Integer; l: LOrient; g: GOrient;
  lt,rb: TGauss;
begin
  Port_to_LPoly(0,0,Width-1,Height-1,@l);

  for i:=0 to 3 do
  p_to_g(l[i].X,l[i].Y,g[i]);

  Max_Gauss_Bound(@g,4,lt,rb);

  xmin:=lt.x; ymin:=lt.y;
  xmax:=rb.x; ymax:=rb.y;
end;

function TRelMatrix.Get_lg_Transit(out lg: lg_Transit): Integer;
var
  i: Integer; r: xgeoid;
begin
  Init_lg_Transit(lg); lg.s:=prj.sys;
  Frame_to_GOrient(0,0,Width,Height,@lg.l);

  for i:=0 to 3 do begin
    with lg.l[i] do p_to_r(x,y,r);
    lg.g[i]:=_Gauss(r.x,r.y);
  end;

  Result:=fprj.sys.pps
end;

function TRelMatrix.Get_in_tr: Real3x3;
var
  tr: Real3x3;
begin
  with fprj do begin
    Begin_3x3(tr,-top.x,-top.y);
    xy_Swap_3x3(tr);
    xy_Scale_3x3(tr,1/kx,1/ky);
    Result:=tr
  end
end;

function TRelMatrix.Get_out_tr: Real3x3;
var
  tr: Real3x3;
begin
  with fprj do begin
    Init_3x3(tr, 0,0, kx,ky);
    xy_Swap_3x3(tr);
    t_Move_3x3(tr,top.x,top.y);
    Result:=tr
  end
end;

function TRelMatrix.g_to_p(const g: TGauss; out p: TGauss): Boolean;
var
  x,y: Double;
begin
  with fprj do begin
    x:=(g.y - top.y) / kx;
    y:=(g.x - top.x) / ky;

    p.x:=x; p.y:=y;

    Result:=(x >= 0) and (x < hdr.nx) and
            (y >= 0) and (y < hdr.ny);
  end
end;

function TRelMatrix.r_to_p(const r: xgeoid; out p: TGauss): Boolean;
var
  g: tgauss;
begin
  r_to_g(r,g);
  Result:=g_to_p(g,p)
end;

function TRelMatrix.r_to_g(const r: xgeoid; out g: TGauss): Integer;
begin
  Result:=prj_to_xy(r,fprj.sys,g)
end;

function TRelMatrix.g_to_r(const g: TGauss; out r: xgeoid): Integer;
begin
  with fprj do begin
    r.x:=g.x; r.y:=g.y; r.s:=sys;

    if sys.pps = 1 then
    prj_XY_BL(g.x,g.y, sys, r.x,r.y);

    Result:=sys.pps
  end
end;

function TRelMatrix.wgs_to_g(b,l: Double; out g: TGauss): Integer;
var
  gx,gy,gh: Double;
begin
  Result:=fprj.sys.pps;

  with fprj do

  if sys.pps = 0 then

    prj_BL_XY(b,l, sys, gx,gy)

  else begin
    if sys.elp <> 9 then
    WGS_GEO(b,l,0, sys.elp,@sys.dat, b,l,gh);
    prj_BL_XY(b,l, fprj.sys, gx,gy);
  end;

  g.x:=gx; g.y:=gy;
end;

function TRelMatrix.r_to_v(const r: xgeoid; out v: txyz): Boolean;
var
  p: tgauss;
begin
  Result:=r_to_p(r,p); v.x:=p.x; v.y:=p.y;
  Get_z(Round(p.x),Round(p.y),v.z);
end;

function TRelMatrix.r_to_l(const r: xgeoid; out p: TPoint): Boolean;
var
  g: TGauss;
begin
  Result:=r_to_p(r,g);
  p.X:=Round(g.x); p.Y:=Round(g.y);
end;

function TRelMatrix.g_to_l(const g: tgauss; out p: TPoint): Boolean;
var
  _p: tgauss;
begin
  Result:=g_to_p(g,_p);
  p:=_LGauss(_p.x,_p.y)
end;

function TRelMatrix.l_to_g(x,y: int): TGauss;
begin
  with fprj do begin
    Result.x:=top.x + y * ky;
    Result.y:=top.y + x * kx;
  end
end;

function TRelMatrix.p_to_g(x,y: Double; out g: tgauss): Boolean;
begin
  with fprj do begin
    g.x:=top.x + y * ky;
    g.y:=top.y + x * kx;

    Result:=(x >= 0) and (x < hdr.nx) and
            (y >= 0) and (y < hdr.ny)
  end
end;

function TRelMatrix.p_to_r(x,y: Double; out r: xgeoid): Boolean;
var
  g: tgauss;
begin
  Result:=p_to_g(x,y,g); g_to_r(g,r)
end;

function TRelMatrix.elp_z(px,py,pz: Double; const dst: tsys): Double;
var
  g: TGauss; v: txyz;
begin
  p_to_g(px,py,g); v.z:=pz;
  prj_XY_BL(g.x,g.y, fprj.sys, v.x,v.y);
  Result:=vGEO_GEO(v,fprj.sys,dst).z
end;

function TRelMatrix.r_get_z(const x: xgeoid; out z: Double): Boolean;
var
  p: TPoint;
begin
  Result:=false; z:=h_null;
  if r_to_l(x,p) then
  if Get_z(p.x,p.y,z) then begin
    if not elp_Equal(x.s.elp,prj.sys.elp) then
    z:=elp_z(p.X,p.Y,z,x.s); Result:=true
  end
end;

function TRelMatrix.g_get_z(const g: tgauss; out z: Double): Boolean;
var
  ix,iy: Integer;
begin
  Result:=false; z:=h_null;

  with fprj do begin
    ix:=Round((g.y - top.y) / kx);
    iy:=Round((g.x - top.x) / ky)
  end;

  if (ix >= 0) and (ix < hdr.nx) then
  if (iy >= 0) and (iy < hdr.ny) then
  Result:=Get_z(ix,iy,z)
end;

function TRelMatrix.xy_get_z(gx,gy: Double; var z: Double): Boolean;
var
  _z: Double;
begin
  Result:=false; with fprj do
  if Get_z(Round((gy - top.y) / kx),
           Round((gx - top.x) / ky),_z) then
  begin z:=_z; Result:=true end;
end;

function TRelMatrix.g_int_z(const g: tgauss; out z: Double): Boolean;
var
  p: tgauss;
begin
  Result:=false; z:=h_null;
  if g_to_p(g,p) then
  Result:=int_z(p.x,p.y,z)
end;

function TRelMatrix.r_int_z(const x: xgeoid; out z: Double): Boolean;
var
  p: tgauss;
begin
  Result:=false; z:=h_null;
  if r_to_p(x,p) then
  if int_z(p.x,p.y,z) then begin
    if not elp_Equal(x.s.elp,prj.sys.elp) then
    z:=elp_z(p.x,p.y,z,x.s); Result:=true
  end
end;

function TRelMatrix.wgs_int_z(b,l: Double; out z: Double): Boolean;
var
  g: TGauss; v: txyz;
begin
  wgs_to_g(b,l,g);
  if g_int_z(g,z) then begin

    if prj.sys.elp <> 9 then begin
     prj_XY_BL(g.x,g.y, fprj.sys, v.x,v.y);
     v.z:=z; z:=vGEO_WGS(v,fprj.sys).z;
    end;

    Result:=true
  end
end;

constructor TRelScene.Create;
begin
  inherited Create;
  ftiff:=TTiffView.Create
end;

destructor TRelScene.Destroy;
begin
  ftiff.Free;
  inherited
end;

function TRelScene.GetPath: PChar;
begin
  Result:=fPath
end;

function TRelScene.Open(APath: PChar): Boolean;
var
  fn: TShortStr;
begin
  Result:=false; Close;

  if Assigned(APath) then
    StrCopy(fn,APath)
  else
    StrCopy(fn,fPath);

  if FileExist(fn) then
  Result:=inherited Open(fn);

  ftex_k.x:=1; ftex_k.y:=1;

  if not hdr.fmt in [0,5] then
  if Result then Close;

  if Result then begin

    StrCopy(fPath,fn);

    ftex_k.x:=1 / (Width-1);
    ftex_k.y:=1 / (Height-1);
  end
end;

procedure TRelScene.Close;
begin
  ftiff.Close;
  inherited
end;

function TRelScene.tiff_Open(Path: PChar; tw,th: int): Boolean;
var
  fn: TShortstr;
begin
  if FileExist(Path) then
    Result:=ftiff.Open(Path)
  else begin
    StrUpdateExt(fn,fEditPath,'.TIF');
    Result:=ftiff.Open(fn)
  end;

  if Result then
  if Active then begin

    Result:=false;

    if ftiff.is_Tiled then begin
      if ftiff.tile.XTiles = rlz.XTiles then
      if ftiff.tile.YTiles = rlz.YTiles then
      Result:=true;

      if not Result then
      if not is_rlz then begin
        frlz.tile_nx:=tw+1;
        frlz.tile_ny:=th+1;
        frlz.XTiles:=int_Tiles(Width,tw);
        frlz.YTiles:=int_Tiles(Height,th)
      end;

      if ftiff.tile.XTiles = rlz.XTiles then
      if ftiff.tile.YTiles = rlz.YTiles then
      Result:=true
    end;

    if not Result then ftiff.Close
  end
end;

procedure TRelScene.tiff_Close;
begin
  ftiff.Close
end;

function TRelScene.Get_tex_Point(X,Y: Integer): TGauss;
begin
  Result.x:=X*ftex_k.x;
  Result.y:=Y*ftex_k.y
end;

function TRelScene.Get_tiff_tiled: Boolean;
begin
  Result:=false;
  if Assigned(fParent) then
  Result:=fParent.ftiff.Active;
end;

function TRelScene.Get_tiff_width: Integer;
begin
  Result:=0;
end;

function TRelScene.Get_tile_tex(tex: TTexture; zoom: Integer): Boolean;
begin
  Result:=false; if tiff_tiled then
  Result:=fParent.ftiff.Get_tex(fIndex.x,fIndex.y,zoom,tex)
end;

function Init_rel_hist(minv,maxv: Float;
                       out hist: TRelHist): Boolean;
var
  dv: Float; cx,st,v1,v2: Integer;
begin
  Result:=false;

  Fillchar(hist,Sizeof(hist),0);

  dv:=maxv - minv; st:=1;
  if dv > 1000 then st:=20 else
  if dv > 100  then st:=10 else
  if dv > 50   then st:=5;

  v1:=Trunc(minv*10);
  v2:=Trunc(maxv*10)+10;

  hist.minv:=v1; hist.step:=st;
  hist.cols:=int_Tiles(v2-v1+1,st);

  if hist.cols > 0 then
  hist.buf:=xAllocInt(hist.cols);

  Result:=Assigned(hist.buf)
end;

function Trunc_rel_hist(var hist: TRelHist): Integer;
var
  z1,z2,iz,dn: Integer;
begin
  with hist do
  if Count > 10000 then begin

    z1:=0; z2:=cols-1; dn:=Count div 10000;

    while z1 < z2 do begin
      if buf[z1] > dn then Break; Inc(z1)
    end;

    while z1 < z2 do begin
      if buf[z2] > dn then Break; Dec(z2)
    end;

    if z1 < z2 then begin

      for iz:=z1 to z2 do buf[iz-z1]:=buf[iz];
      for iz:=z2+1 to cols-1 do buf[iz]:=0;

      Inc(minv,z1*step); cols:=z2-z1+1
    end;
  end;

  Result:=hist.count
end;

procedure Free_rel_hist(var hist: TRelHist);
begin
  hist.buf:=xFreePtr(hist.buf)
end;

function rel_get_hist(rel: TRelief; Hist: PRelHist;
                      var z1,z2: Single): int;
var
  fp: PFloats;
  c,i,n,k: int; v,v1,v2: Float;
begin
  if not rel.Is_rlz then begin
    fp:=rel.Rows[0];
    n:=rel.Width * rel.Height;

    k:=0; v1:=z1; v2:=z2;

    if rel.Header.fmt = 5 then
    for i:=1 to n do begin

      v:=fp[0]; fp:=@fp[1];
      if v < v1 then v1:=v;
      if v > v2 then v2:=v;

      if Assigned(Hist) then
      with Hist^ do begin
        c:=(Round(v*10)-minv) div step;
        if c < 0 then c:=0;
        if c >= cols then c:=cols-1;
        Inc(buf[c]); Inc(k)
      end;
    end;

    if Assigned(Hist) then Hist.count:=k;

    z1:=v1; z2:=v2
  end;

  Result:=Hist.count
end;

function rlz_tile_hist(rel: TRelief; ip,jp: int;
                       Hist: PRelHist; var z1,z2: Single): DWord;
var
  wp: PWords; v,v1,v2,a0,k0: Float;
  w,w1,w2,i,n,k,c: int;
begin
  Result:=0;

  if rel.Is_rlz then
  if rel.Load_tile(ip,jp) then begin

    v1:=z1; v2:=z2;

    wp:=rel.Tile_buf;
    n:=rel.Tile_size div 2;

    a0:=rel.rlz.a0;
    k0:=rel.rlz.k0;

    k:=0;
    for i:=1 to n do begin

      w:=wp[0];
      if w > 0 then begin

        if k = 0 then begin
          w1:=w; w2:=w
        end
        else begin
          if w < w1 then w1:=w;
          if w > w2 then w2:=w;
        end; Inc(k);

        v:=w * k0 - a0;

        if v < v1 then v1:=v;
        if v > v2 then v2:=v;

        if Assigned(Hist) then
        with Hist^ do begin
          c:=(Round(v*10)-minv) div step;
          if c < 0 then c:=0;
          if c >= cols then c:=cols-1;
          Inc(buf[c]); Inc(count)
        end
      end;

      wp:=@wp[1]
    end;

    if k > 0 then begin
      tlong(Result).w[0]:=w1;
      tlong(Result).w[1]:=w2;
    end;

    z1:=v1; z2:=v2
  end
end;

function this_rlz(h: Integer; out hdr: TRlzHeader): Boolean;
begin
  Result:=false;
  FillChar(hdr,SizeOf(hdr),0);

  if h > 0 then
  if xFileBot(h) > SizeOf(hdr) then begin
    FileRead(h,hdr,SizeOf(hdr));
    Result:=is_rlz_hdr(hdr)
  end
end;

function get_rlz_pyr(h: THandle; zoom: int;
                     const hdr: TRlzHeader;
                     out lev: TRlzPyrRec): bool;
var
  bx,sz, iw,ih,iz,tw,th,k: int; r: TRlzPyrRec;
begin
  Result:=false;

  if hdr.m_offp > 0 then
  if hdr.m_lenp = 0 then begin

    tw:=hdr.tile_nx-1;
    th:=hdr.tile_ny-1;

    iw:=tw * hdr.XTiles + 1;
    ih:=th * hdr.YTiles + 1; iz:=1;

    sz:=FileSeek(h,0,2);

    bx:=hdr.m_offp;
    while bx > 0 do begin

      if bx+Sizeof(r) > sz then Break;

      FileSeek(h,bx,0);
      FileRead(h,r,Sizeof(r));

      if r.magic <> pyr_magic then Break;

      iz:=iz*2; if iz > zoom then Break;

      if r.Width <> int_Tiles(iw,iz) then Break;
      if r.Height <> int_Tiles(ih,iz) then Break;

      if r.XTiles <> int_Tiles(r.Width-1,tw) then Break;
      if r.YTiles <> int_Tiles(r.Height-1,th) then Break;

      lev:=r; Result:=true; bx:=r.next
    end
  end
end;

function this_rlf(h: Integer; out hdr: TRelHeader): Boolean;
begin
  Result:=false;
  FillChar(hdr,SizeOf(hdr),0);

  if h > 0 then
  if xFileBot(h) > SizeOf(hdr) then begin
    FileRead(h,hdr,SizeOf(hdr));
    Result:=is_rlf_hdr(hdr,xFileBot(h))
  end
end;

function file_rel(Path: PChar; out hdr: TRelHeader): Boolean;
var
  h: Integer;
begin
  Result:=false;
  Fillchar(hdr,Sizeof(hdr),0);

  h:=FileOpen(Strpas(Path),fmOpenRead);
  if h > 0 then begin
    Result:=this_rlf(h,hdr);
    FileClose(h)
  end
end;

function this_rel(Path: PChar; is_rlz: Boolean): boolean;
var
  rel: TRelief;
begin
  Result:=false;

  rel:=TRelief.Create;
  rel.Extended:=is_rlz;
  try
    Result:=rel.Open(Path)
  finally
    rel.Free
  end
end;

procedure rel_after_edit(Path: PChar);
begin
  xFileErase(Path,'.nxy');
end;

initialization Init;

end.