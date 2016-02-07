unit ozf2; interface

uses
  Classes,otypes,OFrames,XList,img_glu;

type
  tozf2 = class(TCustomList)
    constructor Create;
    destructor Destroy; override;

    function Open(APath: PChar): bool;
    procedure Close;

    procedure setLevel(Ind: int);
    procedure setZoom(iz: int);

    function GetQuads(Quads: PRGBQuads): int;

    function getTile(xp,yp: int; tile: PBytes): bool;
    function getTileRGB(xp,yp: int; tile: PBytes): bool;

    function verifyLevel(Ind: int): bool;
    function LevelStr(Ind: int): String;

  private
    fdata: IFile;
    fsize: TPoint;

    fwidth: int;
    fheight: int;
    fbits: int;
    ftileWidth: int;
    ftileSize: int;
    fXTiles: int;
    fYTiles: int;
    fdataBot: int;
    fTiles: TIntegerList;

    zlb: tzlb;

    fbuf1: PBytes;
    fbuf2: PBytes;
    fbufSize: int;

    fPalette: Array[0..255] of dword;

    fErrorCode: int;
    fErrorMsg: String;

    procedure Error(Code: int; const Msg: String);

    function __getTile(xp,yp: int): PBytes;

  public
    property ErrorCode: int read fErrorCode;
    property ErrorMsg: String read fErrorMsg;

    property size: TPoint read fsize;

    property width: int read fwidth;
    property height: int read fheight;
    property bits: int read fbits;
    property tileWidth: int read ftileWidth;
    property XTiles: int read fXTiles;
    property YTiles: int read fYTiles;
  end;

implementation

uses
  Sysutils,Convert,OFiles;

type
  plevel = ^tlevel;
  tlevel = record
    zoom: float;
    width: int;
    height: int;
    xtiles: int;
    ytiles: int;
    offs: int;
    bot: int
  end;

  tofzLevel = record
    width: int;
    height: int;
    xtiles: short;
    ytiles: short;
  end;

constructor tozf2.Create;
begin
  inherited Create(sizeOf(tlevel),8);
  fTiles:=TIntegerList.Create;
  zlb:=tzlb.Create;
end;

destructor tozf2.Destroy;
begin
  xFreePtr(fbuf1);
  fdata:=nil;
  zlb.Free;
  fTiles.Free;
  inherited
end;

function tozf2.LevelStr(Ind: int): String;
var
  p: ^tlevel;
begin
  Result:=''; p:=Items[Ind];
  if Assigned(p) then
  Result:=RealToStr(p.zoom,1)+'% '+
          Format('[%d x %d]',[p.width,p.height])
end;

procedure tozf2.Error(Code: int; const Msg: String);
begin
  fErrorCode:=Code;
  fErrorMsg:=Msg;
end;

procedure tozf2.Close;
begin
  fdata:=nil; Clear;
end;

procedure tozf2.setLevel(Ind: int);
var
  p: plevel; n: int; ip: Pintegers;
begin
  fTiles.Clear;

  p:=Items[Ind];
  if Assigned(p) then begin
    fwidth:=p.width;
    fheight:=p.height;
    fXTiles:=p.xtiles;
    fYTiles:=p.ytiles;
    fdataBot:=p.bot;

    n:=fXTiles*fYTiles;

    ip:=fdata.Seek(p.offs,(256+n)*4);

    if Assigned(ip) then begin
      Move(ip[0],fPalette,256*4);

      ip:=@ip[256];
      fTiles.LoadBuffer(@ip[0],n)
    end
  end
end;

procedure tozf2.setZoom(iz: int);
var
  i,ilev,v: int; p: plevel;
begin
  ilev:=0;

  p:=First;
  for i:=0 to Count-1 do begin
    v:=Round(fsize.X/p.width);
    if v > iz then Break;
    ilev:=i; Inc(p)
  end;

  setLevel(ilev);
end;

function tozf2.verifyLevel(Ind: int): bool;
var
  i,n,bx,cx,dx: int; ip: pint;
begin
  Result:=false;

  setLevel(Ind);

  ip:=fTiles.First;

  n:=fTiles.Count;
  for i:=1 to n do begin
    bx:=ip^; Inc(ip);
    dx:=fdataBot;
    if i < n then dx:=ip^;

    if bx = 0 then
      Result:=true
    else
    if zlb.Active then begin
      Result:=false;

      cx:=dx-bx;
      if cx >= 16 then
      if cx <= fbufSize then
      if fData.IsData(bx,cx) then begin
        fData.Load(bx,fbuf1^,cx);
        Result:=zlb.xDecompress(fbuf1,cx,fbuf2,ftileSize);
      end
    end;

    if not Result then Break;
  end
end;

function tozf2.GetQuads(Quads: PRGBQuads): int;
begin
  Move(fPalette,Quads[0],256*4);
  Result:=256
end;

function tozf2.__getTile(xp,yp: int): PBytes;
var
  i,bx,cx,dx: int; ip: PIntegers;
begin
  Result:=nil;

  i:=yp*fXTiles+xp;

  ip:=fTiles.Items[i];
  if Assigned(ip) then begin
    bx:=ip[0]; dx:=fdataBot;
    if i < fTiles.Count then dx:=ip[1];

    cx:=dx-bx;
    if bx > 0 then
    if cx >= 16 then
    if cx <= fbufSize then
    if fData.IsData(bx,cx) then
    if zlb.Active then begin
      fData.Load(bx,fbuf1^,cx);
      if zlb.xDecompress(fbuf1,cx,fbuf2,ftileSize) then
      Result:=fbuf2
    end
  end
end;

function tozf2.getTile(xp,yp: int; tile: PBytes): bool;
var
  y,tw: int; img,si,di: PBytes; 
begin
  Result:=false;

  img:=__getTile(xp,yp);
  if Assigned(img) then begin

    tw:=ftileWidth; di:=tile;
    for y:=1 to tw do begin
      si:=@img[ (tw-y)*tw ];
      Move(si^,di^,tw);
      di:=@di[tw]
    end;

    Result:=true
  end
end;

function tozf2.getTileRGB(xp,yp: int; tile: PBytes): bool;
var
  x,y,tw: int; img,si,di: PBytes; cl: tlong;
begin
  Result:=false;

  img:=__getTile(xp,yp);
  if Assigned(img) then begin

    tw:=ftileWidth; di:=tile;
    for y:=1 to tw do begin
      si:=@img[ (tw-y)*tw ];

      for x:=1 to tw do begin
        cl.i:=fPalette[ si[0] ];
        di[0]:=cl.b[2];
        di[1]:=cl.b[1];
        di[2]:=cl.b[0];
        si:=@si[1];
        di:=@di[3];
      end
    end;

    Result:=true
  end
end;

function tozf2.Open(APath: PChar): bool;

const
  separator = $77777777;

type
  thdr1 = record
    magic: short;           // 0x7780 for ozfx3 and 0x7778 for ozf2
    locked: long;           // if set to 1
    tile_width: short;      // set always to 64
    version: short;         // set always to 1
    old_hdr_size: long;     // set always to 0x436
  end;

  thdr2 = record
    header_size: int;       // always 40
	  width: int;	          	// width of image in pixels
  	height: int;        		// height of image in pixels
  	depth: short;   				// set to 1
  	bpp: short;   					// set to 8
    reserved1: int;	    		// set to 0
 	  memory_size: int;		    // height * width
  	reserved2: int;			    // set to 0
    reserved3: int;			    // set to 0
	  unk1: int;						  // set to 0x100
	  unk2: int;						  // set to 0x100
  end;

  thdr = record
    r1: thdr1;
    r2: thdr2;
    sep: dword;
    zoom_level_count: short
  end;

function verify_hdr1(const hdr: thdr1): bool;
begin
  with hdr do
  Result:=(magic = $7778) and
          (tile_width = 64) and
          (version = 1) and
          (old_hdr_size = $436)
end;

function verify_hdr2(const hdr: thdr2): bool;
begin
  Result:=false;
  if hdr.header_size = 40 then
  if hdr.depth = 1 then
  if hdr.width > 0 then
  if hdr.height > 0 then begin
    fsize.X:=hdr.width;
    fsize.Y:=hdr.height;
    fbits:=hdr.bpp;
    Result:=true
  end
end;

procedure fill_levels_offs(top: int);

function verifyLevel(const lev: tofzLevel): bool;
var
  tw: int;
begin
  Result:=false;

  tw:=ftileWidth; if tw > 0 then
  if lev.xtiles = int_Tiles(lev.width,tw) then
  if lev.ytiles = int_Tiles(lev.height,tw) then
  Result:=true
end;

var
  i,n,bx,dx: int; sz: int64;
  lev: tlevel; lev1: ^tofzLevel;
begin
  sz:=fdata.GetSize;
  if sz > top then
  if sz < $FFFFFFFF then begin

    dx:=sz-4;
    bx:=fdata.Get_long(dx);

    n:=(dx-bx) div 4;
    for i:=1 to n do begin
      dx:=fdata.Get_long(bx); Inc(bx,4);

      lev1:=fdata.Seek(dx,sizeof(tofzLevel));
      if Assigned(lev1) then
      if verifyLevel(lev1^) then begin

        Fillchar(lev,Sizeof(lev),0);
        lev.zoom:=lev1.width/fsize.X*100;

        lev.width:=lev1.width;
        lev.height:=lev1.height;
        lev.xtiles:=lev1.xtiles;
        lev.ytiles:=lev1.ytiles;
        lev.offs:=dx + sizeof(tofzLevel);
        lev.bot:=dx; Add(@lev)
      end
    end
  end
end;

var
  hdr: ^thdr;
begin
  Result:=false; Close;

  if not GetOpenFileIntf(APath,fdata) then
    Error(1,'FileOpen error.')
  else begin
    hdr:=fdata.Seek(0,sizeOf(thdr));
    if hdr = nil then
      Error(2,'seek [hdr] false.')
    else
    if not verify_hdr1(hdr.r1) then
      Error(3,'unknown format: [hdr1].')
    else
    if not verify_hdr2(hdr.r2) then
      Error(3,'unknown format: [hdr2].')
    else
    if hdr.sep <> separator then
      Error(4,'zoom levels separator not found.')
    else begin

      ftileWidth:=hdr.r1.tile_width;

      xFreePtr(fbuf1);
      ftileSize:=ftileWidth * ftileWidth;
      fbuf1:=xAllocPtr(ftileSize*3);
      fbufSize:=ftileSize*2;

      if Assigned(fbuf1) then begin
        fbuf2:=@fbuf1[fbufSize];

        fill_levels_offs(sizeOf(thdr));

        if Count = 0 then
        Error(5,'Levels not found.');

        SetLevel(0);
        Result:=fTiles.Count > 0
      end
    end
  end;

  if not Result then Close;
end;

end.
