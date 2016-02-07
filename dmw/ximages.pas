unit ximages; interface

uses
  Classes,Math,LCLType,
  otypes,xlist,idib;

type
  PFotItem = ^TFotItem;
  TFotItem = record
    p: TPoint; img: TNameStr
  end;

  PFotArray = ^TFotArray;
  TFotArray = Array[0..255] of TFotItem;

  TFotList = class(TCustomList)
    constructor Create;
    function Load(APath: PChar): Integer;

    procedure Draw(DC: ICanvas;
                   rcPaint: PRect;
                   XPos,YPos: Integer);

  private
    fSize: TPoint;
    fTile: TPoint;
    fDir: TShortstr;
  public
    property Size: TPoint read fSize;
    property Tile: TPoint read fTile;
  end;

function this_Image(Path: PChar; out w,h,dll: Integer): Boolean;
function rect_Image(Path: PChar; iv: PIntegers): Boolean;

procedure xImagePaint(DC: ICanvas; rcPaint: PRect;
                      xPos,yPos: int; kz: Float;
                      Path,Info: PChar);

function dll_GetMosaicInterface(var Obj): HResult;

implementation

uses
  dynlibs,SysUtils,LCLIntf,
  convert,ofiles,img_dll;

function fot_Path(Path,Fot,Pcx: PChar): PChar;
var
  dir: TShortStr;
begin
  Result:=nil; if StrLen(Pcx) > 0 then

  if StrDirectory(dir,Fot) <> nil then
    Result:=StrPath(Path,Dir,Pcx)
  else
    Result:=StrCopy(Path,Pcx)
end;

function first_FOT(Pcx,Fot: PChar): PChar;
var
  txt: tTextFile; fn: TShortStr;
begin
  Result:=nil;
  txt:=tTextFile.Create;
  try
    if txt.Open(Fot) then

    if not txt.End_of_file then
    if txt.xStrRead <> nil then

    if not txt.End_of_file then
    if txt.xStrRead <> nil then

    if StrToken(fn,txt.str) <> nil then
    Result:=fot_Path(Pcx,Fot,fn);
  finally
    txt.Free
  end
end;

function this_Image(Path: PChar; out w,h,dll: Integer): Boolean;
var
  flags: Integer;
begin
  Result:=false; w:=0; h:=0; dll:=0;

  if im_dll_bmp(Path, w,h,flags) then begin
    dll:=1; Result:=true
  end
end;

function rect_Image(Path: PChar; iv: PIntegers): Boolean;
begin
  Result:=this_Image(Path,iv[0],iv[1],iv[2])
end;

procedure xImage_Draw(DC: ICanvas;
                      rcPaint: PRect;
                      xPos,yPos: int; kz: Float;
                      Path: PChar);
var
  ox,oy,ow,oh, ix1,iy1,ix2,iy2, im_w,im_h,flags: int;
begin
  if im_DLL_Bmp(Path, im_w,im_h,flags) then begin

    with rcPaint^ do begin
      ix1:=xPos + Trunc(Left*kz);
      iy1:=yPos + Trunc(Top*kz);

      ix2:=xPos + Trunc((Right+1)*kz);
      iy2:=yPos + Trunc((Bottom+1)*kz);
    end;

    if ix1 < 0 then ix1:=0;
    if ix2 >= im_w then ix2:=im_w;

    if iy1 < 0 then iy1:=0;
    if iy2 >= im_h then iy2:=im_h;

    ox:=Trunc((ix1 - xPos)/kz);
    oy:=Trunc((iy1 - yPos)/kz);

    ow:=Trunc((ix2 - xPos)/kz)-ox+1;
    oh:=Trunc((iy2 - yPos)/kz)-oy+1;

    if (ow > 0) and (oh > 0) then

    im_dll_Draw(DC,Path,
                ox,oy,ow,oh,
                ix1,iy1, kz)
  end
end;

constructor TFotList.Create;
begin
  inherited Create(Sizeof(TFotItem),32)
end;

function TFotList.Load(APath: PChar): Integer;
var
  txt: TTextfile; s,t: TPoint;
  ph: TFotItem; fn: TShortstr;
begin
  Clear;

  txt:=TTextfile.Create;
  try
    if txt.Open(APath) then
    if txt.xStrLine <> nil then

    if txt.x_this_str('image') then
    if txt.x_Point(s) then
    if txt.x_Point(t) then

    if (t.X <= s.X) and (t.Y <= s.Y) then begin

      fSize:=s; fTile:=t;
      StrDirectory(fDir,APath);

      while txt.xStrLine <> nil do
      if txt.x_str(fn) <> nil then begin
        StrLCopy(ph.img,fn,Sizeof(ph.img)-1);
        if txt.x_Point(ph.p) then Add(@ph)
      end;

    end;
  finally
    txt.Free
  end;

  Result:=Count
end;

procedure TFotList.Draw(DC: ICanvas;
                        rcPaint: PRect;
                        XPos,YPos: Integer);
var
  lp: PFotItem;
  ph: TFotItem; p,w: TPoint;
  i,ix,iy,iw,ih: int; tiles: Boolean;
  fn: TShortstr;
begin
  w:=tile; tiles:=w.X*w.Y > 0;

  lp:=First;
  for i:=1 to Count do begin
    ph:=lp^; Inc(lp);

    p.x:=XPos - ph.p.x;
    p.y:=YPos - ph.p.y;

    StrPath(fn,fDir,ph.img);

    if not tiles then

      xImage_Draw(DC,rcPaint, p.x,p.y, 1, fn)

    else
    with rcPaint^ do begin
      ix:=int_Trunc(p.x + Left - 1,4);
      iy:=int_Trunc(p.y + Top - 1,4);

      iw:=int_Round(p.x + Right + 1,4);
      ih:=int_Round(p.y + Bottom + 1,4);

      if ix < 0 then ix:=0; if iw >= w.x then iw:=w.x;
      if iy < 0 then iy:=0; if ih >= w.y then ih:=w.y;

      if (ix <= iw) and (iy <= ih) then
      xImage_Draw(DC,rcPaint, p.x,p.y, 1, fn)
    end
  end
end;

procedure xImagePaint(DC: ICanvas; rcPaint: PRect;
                      XPos,YPos: int; kz: Float;
                      Path,Info: PChar);
begin
  if Info = nil then
    xImage_Draw(DC,rcPaint, xPos,yPos, kz, Path)
  else begin
    StrCopy(Info,Path);
  end
end;

var
  dll_tif: THandle;

function dll_GetMosaicInterface(var Obj): HResult;
type
  tfunc = function(var Obj): HResult; stdcall;
var
  func: tfunc;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  if dll_tif = 0 then
  dll_tif:=LoadLibrary('dll_tif.dll');

  if dll_tif >= 32 then begin
    @func:=GetProcAddress(dll_tif,'GetMosaicInterface');
    if Assigned(func) then Result:=func(Obj)
  end
end;

initialization
begin
  dll_tif:=0;
end;

finalization
begin
  xFreeLibrary(dll_tif);
end;

end.
