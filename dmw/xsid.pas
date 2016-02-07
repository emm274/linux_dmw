unit xsid; interface

uses
  Classes,LCLType,
  otypes,xline;

const
  LT_STS_CAPI_BASE     = 52000;
  LT_STS_CAPI_BadParam = 52001;
  LT_STS_CAPI_MAX      = 52099;

type
  LT_STATUS  = DWORD;

  LTI_COLORSPACE = (lti_INVALID1,
                    lti_RGB,
                    lti_RGBK,
                    lti_CMYK,
                    lti_GRAYSCALE,
                    lti_PALETTE,
                    lti_YIQ,
                    lti_YIQK,
                    lti_MULTISPECTRAL);

  LTIDataType = (lti_INVALID2,
                 lti_UINT8,lti_SINT8,
                 lti_UINT16,lti_SINT16,
                 lti_UINT32,lti_SINT32,
                 lti_FLOAT32,lti_FLOAT64,
                 lti_UINT64,lti_SINT64,
                 lti_COMPLEX32,lti_COMPLEX64);

const
  sLTI_COLORSPACE: Array[LTI_COLORSPACE] of String[15] =
  ('INVALID','RGB','RGBK','CMYK','GRAYSCALE','PALETTE',
   'YIQ','YIQK','MULTISPECTRAL');

  sLTIDataType: Array[LTIDataType] of String[15] =
  ('INVALID','UINT8','SINT8','UINT16','SINT16',
   'UINT32','SINT32','FLOAT32','FLOAT64',
   'UINT64','SINT64','COMPLEX32','COMPLEX64');

  lLTIDataBits: Array[LTIDataType] of Integer =
  (0,8,8,16,16,32,32,32,64,64,64,32,64);

type
  lti_image = (lti_unk,lti_sid,lti_jp2);

(* get SDK version (C API)
 * Returns the full version number of the SDK, e.g. "4.0.8.673".
 *
 * @param major     address to hold major version number
 * @param minor     address to hold minor version number
 * @param revision  address to hold revision number
 * @param build     address to hold build number
 * @param branch    address to hold string giving the branch name *)
  ltic_getVersion = function(out major,minor,revision,build: DWord;
                             out branch: PChar): LT_STATUS; cdecl;

(* close an image (C API)
 * This function will close the given image.
 *
 * @param image     image to be closed *)
  ltic_closeImage = procedure(image: Pointer); cdecl;

(* open MrSID image via filename (C API)
 * Given the filename of a MrSID image, this function will open the image
 * and return a handle which can be used to access image information and
 * perform decodes.
 *
 * @param image     address to hold image handle
 * @param fileName  name of file to open *)
  ltic_openMrSIDImageFile = function(out image: Pointer;
                                     fileName: PChar): LT_STATUS; cdecl;

(* open JPEG 2000 image via filename (C API)
 *
 * Given the filename of a JPEG 2000 image, this function will open the image
 * and return a handle which can be used to access image information and
 * perform decodes.
 *
 * @param image     address to hold image handle
 * @param fileName  name of file to open
 * @return status code indicating success or failure *)
  ltic_openJP2ImageFile = function(out image: Pointer;
                                   fileName: PChar): LT_STATUS; cdecl;


(* get image width (C API)
 * This function will return the width of the given image.
 *
 * @param   image     image to query
 * @return  width in pixels *)
  ltic_getWidth = function(image: Pointer): DWord; cdecl;

(* get image height (C API)
 * This function will return the height of the given image.
 *
 * @param   image     image to query
 * @return  height of image *)
  ltic_getHeight = function(image: Pointer): DWord; cdecl;

(* get dimensions of an image at a given magnification (C API)
 *
 * This function returns the projected dimensions of an image at a given
 * magnification level. *)
  ltic_getDimsAtMag = function(image: Pointer;
                               magnification: double;
                               out width,height: DWord): LT_STATUS; cdecl;

(* get image colorspace (C API)
 *
 * This function will return the colorspace of the given image.
 *
 * @param   image     image to query
 * @return  colorspace of image *)
  ltic_getColorSpace = function(image: Pointer): LTI_COLORSPACE; cdecl;

(* get number of bands in trhe image (C API)
 *
 * This function will return the numbe rof bands in the given image.
 *
 * @param   image     image to query
 * @return  number of bands in the image *)
  ltic_getNumBands = function(image: Pointer): Word; cdecl;

(* get image datatype (C API)
 *
 * This function will return the datatype of the pixels of the image.
 *
 * @param   image     image to query
 * @return  datatype of the image *)
  ltic_getDataType = function(image: Pointer): LTIDataType; cdecl;

(* get image minimum magnifaction (C API)
 *
 * This function will return the minimum magnification of the image.
 *
 * @param   image     image to query
 * @return  minimum magnification of image *)
  ltic_getMinMagnification = function(image: Pointer): Double; cdecl;

(* get image maximum magnifacation (C API)
 *
 * This function will return the maximum magnification of the image.
 *
 * This method is part of the C API.  It is equivalent to calling
 * LTIImage::getMaxMagnification().
 *
 * @param   image     image to query
 * @return  maximum magnification of image *)
  ltic_getMaxMagnification = function(image: Pointer): Double; cdecl;

(* query if MrSID image is locked (C API)
 *
 * This function will return whether or not the given image is
 * locked, i.e. password protected.  The \a image handle must
 * be a MrSID image.
 *
 * @param   image     MrSID image to query
 * @return  1 if locked, otherwise 0 *)
  ltic_isMrSIDLocked = function(image: Pointer): Byte; cdecl;

(* get geo X position of image (C API)
 * This function will return the upper-left X position of the image. *)
  ltic_getGeoXOrigin = function(image: Pointer): Double; cdecl;

(* get geo Y position of image (C API)
 * This function will return the upper-left Y position of the image. *)
  ltic_getGeoYOrigin = function(image: Pointer): Double; cdecl;

(* get geo X resolution of image (C API)
 * This function will return the X resolution of the image. *)
  ltic_getGeoXResolution = function(image: Pointer): Double; cdecl;

(* get geo Y resolution of image (C API)
 * This function will return the Y resolution of the image. *)
  ltic_getGeoYResolution = function(image: Pointer): Double; cdecl;

(* get geo X rotation term of image (C API)
 * This function will return the X rotation term of the image. *)
  ltic_getGeoXRotation = function(image: Pointer): Double; cdecl;

(* get geo Y rotation term of image (C API)
 * This function will return the Y rotation term of the image. *)
  ltic_getGeoYRotation = function(image: Pointer): Double; cdecl;

(* decode a scene from the image (C API)
 *
 * This function decodes a scene from the image.  The output
 * is written to the given band buffers in packed form.
 *
 * @param   image     image to decode
 * @param   xUpperLeft upperleft x position of scene
 * @param   yUpperLeft upperleft y position of scene
 * @param   width      width of scene
 * @param   height     height of scene
 * @param   magnification manification of scene
 * @param   buffers   array of buffers to write to, one per band
 * @return status code indicating success or failure *)
  ltic_decode = function(image: Pointer;
                         xUpperLeft,yUpperLeft: Double;
                         width,height,magnification: Double;
                         buffers: PPointers): LT_STATUS; cdecl;


 tltic = class
   destructor Destroy; override;

   function Open_image(Path: PChar;
                       out h: Pointer;
                       out typ: lti_image): Boolean;

 private
   fSdk: DWord;
   fmajor: DWord;
   fminor: DWord;
   frevision: DWord;
   fbuild: DWord;

   fdll: THandle;
   fInit: LongBool;
   fActive: LongBool;

   fgetVersion: ltic_getVersion;
   fcloseImage: ltic_closeImage;

   fopenMrSIDImageFile: ltic_openMrSIDImageFile;
   fopenJP2ImageFile: ltic_openJP2ImageFile;

   fgetWidth: ltic_getWidth;
   fgetHeight: ltic_getHeight;

   fgetDimsAtMag: ltic_getDimsAtMag;
   fgetColorSpace: ltic_getColorSpace;

   fgetNumBands: ltic_getNumBands;
   fgetDataType: ltic_getDataType;

   fgetMinMagnification: ltic_getMinMagnification;
   fgetMaxMagnification: ltic_getMaxMagnification;

   fisMrSIDLocked: ltic_isMrSIDLocked;

   fgetGeoXOrigin: ltic_getGeoXOrigin;
   fgetGeoYOrigin: ltic_getGeoYOrigin;

   fgetGeoXResolution: ltic_getGeoXResolution;
   fgetGeoYResolution: ltic_getGeoYResolution;

   fgetGeoXRotation: ltic_getGeoXRotation;
   fgetGeoYRotation: ltic_getGeoYRotation;

   fdecode: ltic_decode;

   function GetActive: bool;

 public
   property Active: bool read GetActive;

   property major: DWord read fmajor;
   property minor: DWord read fminor;
   property revision: DWord read frevision;
   property build: DWord read fbuild;
 end;

 tltiImage = class
   destructor Destroy; override;

   function Open(Path: PChar): Boolean;
   procedure Close;

   function Get_bsq(x,y,w,h: Integer; mag: Double;
                    buf: PPointers): Boolean;

   function Decode(x,y,w,h: Integer; buf: PBytes): Boolean;

   function Get_tile(ip,jp: Integer; buf: PBytes): Boolean;

   function Seek_tile(ip,jp: Integer): Pointer;

   function Get_tile_info: TBitmapInfoHeader;

 private
   fHandle: Pointer;
   fWidth: Integer;
   fHeight: Integer;
   fCOLORSPACE: LTI_COLORSPACE;

   fNumBands: Integer;
   fOutBands: Integer;
   fDataType: LTIDataType;

   fInpBits: Integer;
   fOutBits: Integer;

   fMinMag: Double;
   fMaxMag: Double;

   fXOrg,fYOrg: Double;
   fXRes,fYRes: Double;
   fXRot,fYRot: Double;

   fLocked: Byte;

   fTileSize: Integer;
   fTiles: TPoint;

   fReadMag: Double;
   fReadSize: TPoint;

   fbuf1: Pointer;
   fbuf1Size: TPoint;

   fbuf2: PBytes;

   procedure SetReadMag(Value: Double);

 public
   property Width: Integer read fWidth;
   property Height: Integer read fHeight;
   property COLORSPACE: LTI_COLORSPACE read fCOLORSPACE;
   property NumBands: Integer read fNumBands;
   property DataType: LTIDataType read fDataType;
   property MinMag: Double read fMinMag;
   property MaxMag: Double read fMaxMag;
   property Locked: Byte read fLocked;

   property OutBits: Integer read fOutBits;

   property XOrg: Double read fXOrg;
   property YOrg: Double read fYOrg;
   property XRes: Double read fXRes;
   property YRes: Double read fYRes;
   property XRot: Double read fXRot;
   property YRot: Double read fYRot;

   property ReadMag: Double read fReadMag write SetReadMag;
   property ReadSize: TPoint read fReadSize;

   property TileSize: Integer read fTileSize;
   property Tiles: TPoint read fTiles;
 end;

var
  ltic: tltic;

function Get_sid_geo(Path: PChar;
                     out ImageSize: TPoint;
                     out tr: Real3x3): Boolean;

implementation

uses
  Sysutils,dynlibs,
  convert;

const
  lti_dll = 'lti_dsdk_dll.dll';

function tif_Line(Width,Bits: Integer): Integer;
begin
  Result:=(Width * Bits + 7) div 8;
end;

function img_Line(Width,Bits: Integer): Integer;
begin
  Result:=(Width * Bits + 31) div 32;
  Result:=Result * 4
end;

function img_Size(Width,Height,Bits: Integer): Integer;
begin
  Result:=img_Line(Width,Bits) * Height
end;

function Alloc_bsq_win(w,h,bits,NPlanes: Integer): PPointers;
var
  i,win: Integer; bp: PBytes;
begin
  Result:=nil;

  if NPlanes > 0 then begin
    win:=((w * Bits + 7) div 8) * h;
    win:=int_Round(win,4);

    Result:=xAllocPtr(NPLanes * (win+4));
    if Assigned(Result) then begin
      bp:=@Result[NPLanes];
      for i:=0 to NPLanes-1 do
      Result[i]:=@bp[i*win];
    end
  end
end;

procedure bsq_to_win1(src: PPointers;
                      iw,ih,NPlanes: Integer;
                      dst: PBytes; ow,oh: Integer);
var
  i,x,y,line,iw1,np1: Integer;
  bp: Array[0..7] of PBytes;
  dp: PBytes;
begin
  iw1:=iw-1; np1:=NPlanes-1;

  line:=img_Line(ow,8*NPlanes);
  Fillchar(dst^,line*oh,0);

  for i:=0 to np1 do bp[i]:=src[np1-i];

  for y:=0 to ih-1 do begin

    dp:=@dst[y*line];

    for x:=0 to iw1 do
    for i:=0 to np1 do begin
      dp[0]:=bp[i][0]; dp:=@dp[1];
      bp[i]:=@bp[i][1]
    end
  end;

  bp:=bp
end;

destructor tltic.Destroy;
begin
  if fdll >= 32 then
  FreeLibrary(fdll);
  inherited
end;

function tltic.GetActive: bool;
var
  pc: PChar;
begin
  if not fInit then begin fInit:=true;

    fdll:=LoadLibrary(lti_dll);
    if fdll >= 32 then begin
      fgetVersion:=GetProcAddress(fdll,'ltic_getVersion');
      fcloseImage:=GetProcAddress(fdll,'ltic_closeImage');

      fopenMrSIDImageFile:=GetProcAddress(fdll,'ltic_openMrSIDImageFile');
      fopenJP2ImageFile:=GetProcAddress(fdll,'ltic_openJP2ImageFile');

      fgetWidth:=GetProcAddress(fdll,'ltic_getWidth');
      fgetHeight:=GetProcAddress(fdll,'ltic_getHeight');

      fgetDimsAtMag:=GetProcAddress(fdll,'ltic_getDimsAtMag');
      fgetColorSpace:=GetProcAddress(fdll,'ltic_getColorSpace');

      fgetNumBands:=GetProcAddress(fdll,'ltic_getNumBands');
      fgetDataType:=GetProcAddress(fdll,'ltic_getDataType');

      fgetMinMagnification:=GetProcAddress(fdll,'ltic_getMinMagnification');
      fgetMaxMagnification:=GetProcAddress(fdll,'ltic_getMaxMagnification');

      fisMrSIDLocked:=GetProcAddress(fdll,'ltic_isMrSIDLocked');

      fgetGeoXOrigin:=GetProcAddress(fdll,'ltic_getGeoXOrigin');
      fgetGeoYOrigin:=GetProcAddress(fdll,'ltic_getGeoYOrigin');

      fgetGeoXResolution:=GetProcAddress(fdll,'ltic_getGeoXResolution');
      fgetGeoYResolution:=GetProcAddress(fdll,'ltic_getGeoYResolution');

      fgetGeoXRotation:=GetProcAddress(fdll,'ltic_getGeoXRotation');
      fgetGeoYRotation:=GetProcAddress(fdll,'ltic_getGeoYRotation');

      fdecode:=GetProcAddress(fdll,'ltic_decode');

      if Assigned(fgetVersion) then
      if Assigned(fcloseImage) then

      if Assigned(fopenMrSIDImageFile) then
      if Assigned(fopenJP2ImageFile) then

      if Assigned(fgetWidth) then
      if Assigned(fgetHeight) then

      if Assigned(fgetDimsAtMag) then
      if Assigned(fgetColorSpace) then

      if Assigned(fgetNumBands) then
      if Assigned(fgetDataType) then

      if Assigned(fgetMinMagnification) then
      if Assigned(fgetMaxMagnification) then

      if Assigned(fisMrSIDLocked) then

      if Assigned(fgetGeoXOrigin) then
      if Assigned(fgetGeoYOrigin) then

      if Assigned(fgetGeoXResolution) then
      if Assigned(fgetGeoYResolution) then

      if Assigned(fgetGeoXRotation) then
      if Assigned(fgetGeoYRotation) then

      if Assigned(fdecode) then

      if fgetVersion(fmajor,fminor,frevision,fbuild,pc) = 0 then
      fActive:=true
    end
  end;

  Result:=fActive
end;

function tltic.Open_image(Path: PChar;
                          out h: Pointer;
                          out typ: lti_image): Boolean;
var
  ext: TShortstr;
begin
  Result:=false; h:=0; typ:=lti_unk;

  StrPCopy(ext,ExtractFileExt(StrPas(Path)));

  if StrIComp(ext,'.sid') = 0 then begin
    Result:=fopenMrSIDImageFile(h,Path) = 0;
    typ:=lti_sid
  end else
  if StrIComp(ext,'.jp2') = 0 then begin
    Result:=fopenJP2ImageFile(h,Path) = 0;
    typ:=lti_jp2
  end
end;

destructor tltiImage.Destroy;
begin
  Close; inherited
end;

procedure tltiImage.SetReadMag(Value: Double);
begin
  if Value < fMinMag then Value:=fMinMag;
  if Value > fMaxMag then Value:=fMaxMag;

  fReadSize.X:=Trunc(Width*Value);
  fReadSize.Y:=Trunc(Height*Value);

  fTiles.X:=int_Tiles(fReadSize.X,fTileSize);
  fTiles.Y:=int_Tiles(fReadSize.Y,fTileSize);

  fReadMag:=Value
end;

function tltiImage.Open(Path: PChar): Boolean;
const
  x2 = 1024;
var
  typ: lti_image;
begin
  Result:=false; Close;

  fbuf2:=xFreePtr(fbuf2);
  fbuf1:=xFreePtr(fbuf1);

  if ltic.Active then
  if ltic.Open_image(Path,fHandle,typ) then begin

    fWidth:=ltic.fgetWidth(fHandle);
    fHeight:=ltic.fgetHeight(fHandle);

    fCOLORSPACE:=ltic.fgetColorSpace(fHandle);

    fNumBands:=ltic.fgetNumBands(fHandle);
    fDataType:=ltic.fgetDataType(fHandle);

    fMinMag:=ltic.fgetMinMagnification(fHandle);
    fMaxMag:=ltic.fgetMaxMagnification(fHandle);

    fLocked:=0; if typ = lti_sid then
    fLocked:=ltic.fisMrSIDLocked(fHandle);

    fXOrg:=ltic.fgetGeoXOrigin(fHandle);
    fYOrg:=ltic.fgetGeoYOrigin(fHandle);

    fXRes:=ltic.fgetGeoXResolution(fHandle);
    fYRes:=ltic.fgetGeoYResolution(fHandle);

    fXRot:=ltic.fgetGeoXRotation(fHandle);
    fYRot:=ltic.fgetGeoYRotation(fHandle);

    if fWidth > 0 then
    if fHeight > 0 then begin

      if fCOLORSPACE = lti_RGB then begin
        if fNumBands = 3 then
        if fDataType = lti_UINT8 then
        Result:=true
      end else
      if fCOLORSPACE = lti_GRAYSCALE then begin
        if fNumBands = 1 then
        if fDataType = lti_UINT8 then
        Result:=true
      end;

      if Result then begin

        fInpBits:=lLTIDataBits[fDataType];

        fOutBands:=1;
        if fNumBands > 1 then fOutBands:=3;

        fOutBits:=fOutBands * 8;

        fTileSize:=x2; ReadMag:=fMinMag;

        Result:=true
      end
    end
  end
end;

procedure tltiImage.Close;
begin
  fbuf2:=xFreePtr(fbuf2);
  fbuf1:=xFreePtr(fbuf1);

  if Assigned(fHandle) then
  ltic.fcloseImage(fHandle);
  fHandle:=nil
end;

function tltiImage.Get_bsq(x,y,w,h: Integer; mag: Double;
                           buf: PPointers): Boolean;
begin
  Result:=false;
  if ltic.fdecode(fHandle,x,y,w,h,mag,buf) = 0 then
  Result:=true
end;

function tltiImage.Get_tile_info: TBitmapInfoHeader;
var
  inf: TBitmapInfoHeader;
begin
  Fillchar(inf,Sizeof(inf),0);
  inf.biSize:=Sizeof(TBitMapInfoHeader);
  inf.biWidth:=fTileSize;
  inf.biHeight:=fTileSize;
  inf.biPlanes:=1;
  inf.biCompression:=bi_RGB;
  inf.biBitCount:=fOutBits;
  Result:=inf
end;

function tltiImage.Decode(x,y,w,h: Integer; buf: PBytes): Boolean;
var
  iw,ih: Integer;
begin
  Result:=false;

  if (w > fbuf1Size.X) or (h > fbuf1Size.Y) then
  fbuf1:=xFreePtr(fbuf1);

  if fbuf1 = nil then begin
    fbuf1:=Alloc_bsq_win(w,h,fInpBits,fNumBands);
    fbuf1Size:=Point(w,h);
  end;

  iw:=w; ih:=h;
  if x+iw > fReadSize.X then
  iw:=fReadSize.X - x;

  if y+ih > fReadSize.Y then
  ih:=fReadSize.Y - y;

  if Assigned(fbuf1) then
  if (iw > 0) and (ih > 0) then
  if ltic.fdecode(fHandle,x,y,iw,ih,fReadMag,fbuf1) = 0 then begin
    bsq_to_win1(fbuf1,iw,ih,fOutBands,buf,w,h);
    Result:=true
  end
end;

function tltiImage.Get_tile(ip,jp: Integer; buf: PBytes): Boolean;
begin
  Result:=Decode(ip*fTileSize,jp*fTileSize,
                 fTileSize,fTileSize,buf)
end;

function tltiImage.Seek_tile(ip,jp: Integer): Pointer;
var
  sz: Integer;
begin
  Result:=nil;

  if fbuf2 = nil then begin
    sz:=img_Size(fTileSize,fTileSize,8*fOutBands);
    fbuf2:=xAllocPtr(sz);
  end;

  if Assigned(fbuf2) then
  if Get_tile(ip,jp,fbuf2) then
  Result:=fbuf2
end;

function Get_sid_geo(Path: PChar;
                     out ImageSize: TPoint;
                     out tr: Real3x3): Boolean;
var
  img: tltiImage;
begin
  Result:=false;

  ImageSize:=Point(1000,1000);
  tr:=Identify_3x3;

  img:=tltiImage.Create;
  try
    if img.Open(Path) then begin

      ImageSize.X:=img.Width;
      ImageSize.Y:=img.Height;

      Init_3x3(tr,0,0,img.XRes,img.YRes);
      t_Move_3x3(tr,img.XOrg,img.YOrg);

      Result:=true
    end;
  finally
    img.Free
  end
end;

initialization
  ltic:=tltic.Create;

finalization
  ltic.Free;

end.
