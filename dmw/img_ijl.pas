unit img_ijl; interface

uses
  Classes,LCLType,Graphics,
  otypes,xproj,ofiles1,xlist,
  ijl,img_x;

const
  JPegFatMax = 256;

  ijl_photomod: Longbool = true;

type
  TJpegMarker = record
    id,len: Word
  end;

  TJpegFatItem = record
    ofs,len,width,height: DWord
  end;

  PJpegFatArray = ^TJpegFatArray;
  TJpegFatArray = array[0..JPegFatMax-1] of TJpegFatItem;

  PJpegDecompObj = ^TJpegDecompObj;
  TJpegDecompObj = record
    Data: Pointer;
    DataLen: int;
    Dest: XImage;
    log: TTextWrite;
    Err: int
  end;

  TJpegDecompThread = class(TThread)
    constructor Create(AObj: PJpegDecompObj;
                       ARotate: IRotateImage;
                       AGamma: PBytes);

    procedure Execute; override;

    procedure Takt;
  private
    fObj: PJpegDecompObj;
    fRotate: IRotateImage;
    fGamma: PBytes;
  end;

  TJpegDecompBuf = class(TDataStream)

    constructor Create(Ind: int;
                       ARotate: IRotateImage;
                       AGamma: PBytes);

    destructor Destroy; override;

    procedure Resume;
    procedure Suspend;
    procedure Terminate;

    procedure Unpack(Data: Pointer; len,Id: int);
    procedure Unpackb(Data: TDataStream; Id: int);

    function GetImage(out Err: int): XImage;

  private
    fThread: TJpegDecompThread;
    fImage: XImage;
    flog: TTextWrite;

    fCom: TJpegDecompObj;

    fId: int;
    fActive: longbool;

    function GetReady: bool;

  public
    property Active: longbool read fActive;
    property Ready: bool read GetReady;
    property Id: int read fId write fId;
  end;

var
  ijl_Err: int;

function JpegMarker(id,len: Word): TJpegMarker;

function ijl_load_tables(hdr: Pointer; len: Integer;
                         Tables: PJPEG_Tables): Boolean;

function ijl_Get_info(Buf: Pointer; BufSize: Integer;
                      Info: PBitmapInfoHeader): Boolean;

function jpg_Get_info(jpg: PChar;
                      Info: PBitmapInfoHeader): Boolean;

function ijl_Decompress(dib: Pointer;
                        dib_w,dib_h,dib_bits: Integer;
                        Jpeg: Pointer; JpegLen: Integer;
                        Tables: PJPEG_Tables): Boolean;

function ijl_Decompress1(dib: Pointer;
                         dib_w,dib_h,dib_bits: Integer;

                         posX,posY,Zoom: Integer;

                         Jpeg: Pointer; JpegLen: Integer;
                         Tables: PJPEG_Tables): Boolean;

function ijl_Decompressf(dib: Pointer;
                         dib_w,dib_h,dib_bits: Integer;
                         posX,posY,Zoom: Integer;
                         Jpeg: PChar): Boolean;

function ijl_Compress_header(Dest: Pointer; DestSize: Integer;

                             Dib: Pointer;
                             Dib_w,Dib_h,Dib_bits: Integer;

                             Quality: Integer): Integer;

function ijl_Compress_entropy(Dest: Pointer; DestSize: Integer;

                              Dib: Pointer;
                              Dib_w,Dib_h,Dib_bits: Integer;

                              Tables: PJPEG_Tables;
                              Quality: Integer): Integer;

function ijl_Compress_image(Dest: Pointer; DestSize: Integer;

                            Dib: Pointer;
                            Dib_w,Dib_h,Dib_bits: Integer;

                            Quality: Integer): Integer;

function ijl_Compress_file(Dest: PChar; img: XImage): Boolean;
function ijl_Compress_map(Dest: PChar; const map: Bitmap): Boolean;

function ijl_Decompress_image(Data: Pointer; DataLen: Integer;
                              dst: XImage): int;

function ijl_Decompress_data(Data: Pointer; DataLen: Integer;
                             dst: XImage): Boolean;

function ijl_Decompress_file(jpg: PChar; dst: XImage; iz: int): Boolean;

function ijl_Bitblt(Buf: Pointer; Size: Integer;
                    dst: XImage): Boolean;

function jpg_pyramid(Dest,Path: PChar): Boolean;

function Is_jpg_pyramid(Path: PChar): Boolean;

type
  TJpgView = class
    destructor Destroy; override;

    function Open(Path: PChar): Boolean;
    procedure Close;

    function Get_rgb(Buf: Pointer): Boolean;
    function BitBlt(im: XImage): Boolean;

  private
    jcprops: TJPEG_CORE_PROPERTIES;
    fActive: Boolean;
  public
    property Width: Cardinal read jcprops.JPGWidth;
    property Height: Cardinal read jcprops.JPGHeight;
  end;

implementation

uses
  Sysutils,ofiles;

function JpegMarker(id,len: Word): TJpegMarker;
begin
  Result.id:=SwapWord(id);
  Result.len:=SwapWord(2+len);
end;

function DWORD_Ptr(P: Pointer): Pointer;
begin
  Result:=Pointer((TPointer(P)+7) and $FFFFFFF8)
end;

procedure Copy_Shorts(src,dst: PShort; N: Integer);
begin
  Move(src^,dst^,N*SizeOf(Short))
end;

procedure ijl_YCbCr_to_RGB(dib: Pointer; dib_w,dib_h: int);
var
  i,j,pad,R,G,B: int; si: PBytes;
  Y,Cr,Cb: Single;
begin
  pad:=pad_Line(dib_w,24); si:=dib;
  for j:=1 to dib_h do begin
    for i:=1 to dib_w do begin

      if false then begin
        Y:=si[2]; Cb:=si[1]; Cr:=si[0];
        Cb:=Cb - 128; Cr:=Cr - 128;

        R:=Round(Y + 1.40200*Cr);
        G:=Round(Y - 0.34414*Cb - 0.71414*Cr);
        B:=Round(Y + 1.77200*Cb);
      end
      else begin
        Y:=si[2]; Y:=1.164*(Y - 16);
        Cb:=si[1]; Cb:=Cb-128;
        Cr:=si[0]; Cr:=Cr-128;

        R:=Round(Y + 1.596*Cr);
        G:=Round(Y - 0.813*Cr - 0.392*Cb);
        B:=Round(Y + 2.017*Cb);
      end;

      if R < 0 then R:=0; if R > 255 then R:=255;
      if G < 0 then G:=0; if G > 255 then G:=255;
      if B < 0 then B:=0; if B > 255 then B:=255;

      si:=@si[3]

    end; si:=@si[pad]
  end
end;

function ijl_load_tables(hdr: Pointer; len: Integer;
                         Tables: PJPEG_Tables): Boolean;
var
  jcprops: TJPEG_CORE_PROPERTIES;
  I: Integer;
begin
  Result:=false;
  FillChar(Tables^,SizeOf(TJPEG_Tables),0);
  Tables.IsValid:=S_FALSE;

  if ijl_Active then
  if ijlInit(@jcprops) = ijl_OK then begin

    jcprops.JPGBytes:=hdr;
    jcprops.JPGSizeBytes:=len;

    if ijlRead(@jcprops,IJL_JBUFF_READPARAMS) = IJL_OK then
    with jcprops.jprops do begin
      Tables.IsValid:=S_OK;

      Tables.maxquantindex:=maxquantindex;

      Tables.nhuffActables:=nhuffActables;
      for I:=0 to nhuffActables-1 do
      Tables.jFmtAcHuffman[I]:=jFmtAcHuffman[I];

      Tables.nhuffDctables:=nhuffDctables;
      for I:=0 to nhuffDctables-1 do
      Tables.jFmtDcHuffman[I]:=jFmtDcHuffman[I];

      Tables.nqtables:=nqtables;
      for I:=0 to nqtables-1 do begin
        Tables.jFmtQuant[I].precision:=jFmtQuant[I].precision;
        Tables.jFmtQuant[I].ident:=jFmtQuant[I].ident;

        with Tables.jFmtQuant[I] do elements:=DWORD_Ptr(@elarray);
        Copy_Shorts(jFmtQuant[I].elements,Tables.jFmtQuant[I].elements,77);
      end;

      Result:=true
    end;

    ijlFree(@jcprops);
  end;
end;

function ijl_back_tables(Tables: PJPEG_Tables;
                         jprops: PJPEG_PROPERTIES): Boolean;
var
  I: Integer;
begin
  Result:=false;

  with Tables^ do
  if IsValid = S_OK then begin

    jprops.maxquantindex:=maxquantindex;

    jprops.nhuffActables:=nhuffActables;
    for I:=0 to nhuffActables-1 do
    jprops.jFmtAcHuffman[I]:=jFmtAcHuffman[I];

    jprops.nhuffDctables:=nhuffDctables;
    for I:=0 to nhuffDctables-1 do
    jprops.jFmtDcHuffman[I]:=jFmtDcHuffman[I];

    jprops.nqtables:=nqtables;
    for I:=0 to nqtables-1 do begin

      jprops.jFmtQuant[I].precision:=jFmtQuant[I].precision;
      jprops.jFmtQuant[I].ident:=jFmtQuant[I].ident;

      with jprops.jFmtQuant[I] do elements:=DWORD_Ptr(@elarray);
      Copy_Shorts(jFmtQuant[I].elements,jprops.jFmtQuant[I].elements,77);
    end;

    Result:=true
  end
end;

function ijl_Get_info(Buf: Pointer; BufSize: Integer;
                      Info: PBitmapInfoHeader): Boolean;
var
  jcprops: TJPEG_CORE_PROPERTIES;
begin
  Result:=false;
  Fillchar(Info^,Sizeof(TBitmapInfoHeader),0);

  if ijl_Active then
  if ijlInit(@jcprops) = ijl_OK then begin

    jcprops.JPGBytes:=Buf;
    jcprops.JPGSizeBytes:=BufSize;

    if ijlRead(@jcprops,IJL_JBUFF_READPARAMS) = IJL_OK then
    with Info^ do begin

      biWidth:=jcprops.JPGWidth;
      biHeight:=jcprops.JPGHeight;
      biBitCount:=jcprops.JPGChannels * 8;
      biPlanes:=1;

      Result:=true
    end;

    ijlFree(@jcprops)
  end;
end;

function jpg_Get_info(jpg: PChar;
                      Info: PBitmapInfoHeader): Boolean;
var
  jcprops: TJPEG_CORE_PROPERTIES;
begin
  Result:=false;
  Fillchar(Info^,Sizeof(TBitmapInfoHeader),0);

  if ijl_Active then
  if ijlInit(@jcprops) = ijl_OK then begin

    jcprops.JPGFile:=jpg;
    if ijlRead(@jcprops,IJL_JFILE_READPARAMS) = IJL_OK then
    with Info^ do begin

      biWidth:=jcprops.JPGWidth;
      biHeight:=jcprops.JPGHeight;
      biBitCount:=jcprops.JPGChannels * 8;
      biPlanes:=1;

      Result:=true
    end;

    ijlFree(@jcprops)
  end;
end;

function ijl_Decompress(dib: Pointer;
                        dib_w,dib_h,dib_bits: Integer;
                        Jpeg: Pointer; JpegLen: Integer;
                        Tables: PJPEG_Tables): Boolean;
var
  jcprops: TJPEG_CORE_PROPERTIES;
  IoType: TIJLIOTYPE; channels,zoom,rc: int;
begin
  Result:=false;

  if ijl_Active then
  if ijlInit(@jcprops) = ijl_OK then begin

    if Assigned(Tables) then
    if Tables.isValid = S_OK then
    ijl_back_tables(Tables,@jcprops.jprops);

    jcprops.JPGBytes:=Jpeg;
    jcprops.JPGSizeBytes:=JpegLen;

    channels:=dib_bits div 8;

    if ijlRead(@jcprops,IJL_JBUFF_READPARAMS) = IJL_OK then

    if jcprops.JPGWidth > 0 then
    if jcprops.JPGHeight > 0 then
    if jcprops.JPGChannels = channels then begin

      jcprops.DIBBytes:=dib;
      jcprops.DIBWidth:=dib_w;
      jcprops.DIBHeight:=dib_h;
      jcprops.DIBChannels:=channels;
      jcprops.DIBPadBytes:=pad_Line(dib_w,dib_bits);

      if jcprops.JPGChannels = 1 then
        jcprops.DIBColor:=IJL_G
      else
      if jcprops.JPGChannels = 3 then begin
        jcprops.DIBColor:=IJL_RGB;

        if jcprops.JPGColor = IJL_YCBCR then begin

          if Assigned(Tables) then
          if Tables.isValid <> S_OK then
          Tables:=nil;

          if Tables = nil then
          if ijl_photomod then
          if PInteger(TPointer(Jpeg)+6)^ <> $4649464A then
          jcprops.JPGColor:=IJL_RGB
        end
      end;

      IoType:=IJL_JBUFF_READWHOLEIMAGE;
      zoom:=jcprops.JPGWidth div dib_w;

      if zoom >= 8 then IoType:=IJL_JBUFF_READONEEIGHTH else
      if zoom >= 4 then IoType:=IJL_JBUFF_READONEQUARTER else
      if zoom >= 2 then IoType:=IJL_JBUFF_READONEHALF;

      rc:=ijlRead(@jcprops,IoType);
      Result:=rc = IJL_OK
    end;

    ijlFree(@jcprops)
  end;
end;

function ijl_Decompress1(dib: Pointer;
                         dib_w,dib_h,dib_bits: Integer;

                         posX,posY,Zoom: Integer;

                         Jpeg: Pointer; JpegLen: Integer;
                         Tables: PJPEG_Tables): Boolean;
var
  jcprops: TJPEG_CORE_PROPERTIES;
  IoType: TIJLIOTYPE; channels,rc: Integer;
begin
  Result:=false;

  if ijl_Active then
  if ijlInit(@jcprops) = ijl_OK then begin

    if Assigned(Tables) then
    if Tables.isValid = S_OK then
    ijl_back_tables(Tables,@jcprops.jprops);

    jcprops.JPGBytes:=Jpeg;
    jcprops.JPGSizeBytes:=JpegLen;

    channels:=dib_bits div 8;

    if ijlRead(@jcprops,IJL_JBUFF_READPARAMS) = IJL_OK then

    if jcprops.JPGWidth > 0 then
    if jcprops.JPGHeight > 0 then
    if jcprops.JPGChannels = channels then begin

      jcprops.DIBBytes:=dib;
      jcprops.DIBWidth:=dib_w;
      jcprops.DIBHeight:=dib_h;
      jcprops.DIBChannels:=channels;
      jcprops.DIBPadBytes:=pad_Line(dib_w,dib_bits);

      if jcprops.JPGChannels = 1 then
        jcprops.DIBColor:=IJL_G
      else
      if jcprops.JPGChannels = 3 then
        jcprops.DIBColor:=IJL_RGB;

      IoType:=IJL_JBUFF_READWHOLEIMAGE;

      if zoom = 2 then
        IoType:=IJL_JBUFF_READONEHALF
      else
      if zoom = 4 then
        IoType:=IJL_JBUFF_READONEQUARTER
      else
      if zoom = 8 then
        IoType:=IJL_JBUFF_READONEEIGHTH;

      with jcprops.jprops.roi do begin
        Left:=posX; Top:=posY;
        Right:=posX + dib_w;
        Bottom:=posY + dib_h;
      end;

      rc:=ijlRead(@jcprops,IoType);
      Result:=rc in [IJL_OK,IJL_ROI_OK]
    end;

    ijlFree(@jcprops)
  end;
end;

function ijl_Decompressf(dib: Pointer;
                         dib_w,dib_h,dib_bits: Integer;
                         posX,posY,Zoom: Integer;
                         Jpeg: PChar): Boolean;
var
  jcprops: TJPEG_CORE_PROPERTIES;
  channels,rc: int; IoType: TIJLIOTYPE;
begin
  Result:=false;

  if ijl_Active then
  if ijlInit(@jcprops) = ijl_OK then begin

    jcprops.JPGFile:=Jpeg;

    channels:=dib_bits div 8;

    if ijlRead(@jcprops,IJL_JFILE_READPARAMS) = IJL_OK then

    if jcprops.JPGWidth > 0 then
    if jcprops.JPGHeight > 0 then
    if jcprops.JPGChannels = channels then begin

      jcprops.DIBBytes:=dib;
      jcprops.DIBWidth:=dib_w;
      jcprops.DIBHeight:=dib_h;
      jcprops.DIBChannels:=channels;
      jcprops.DIBPadBytes:=pad_Line(dib_w,dib_bits);

      if jcprops.JPGChannels = 1 then
        jcprops.DIBColor:=IJL_G
      else
      if jcprops.JPGChannels = 3 then
        jcprops.DIBColor:=IJL_RGB;

      IoType:=IJL_JFILE_READWHOLEIMAGE;

      if zoom = 2 then
        IoType:=IJL_JFILE_READONEHALF
      else
      if zoom = 4 then
        IoType:=IJL_JFILE_READONEQUARTER
      else
      if zoom = 8 then
        IoType:=IJL_JFILE_READONEEIGHTH;

      with jcprops.jprops.roi do begin
        Left:=posX; Top:=posY;
        Right:=posX + dib_w;
        Bottom:=posY + dib_h;
      end;

      rc:=ijlRead(@jcprops,IoType);
      Result:=rc in [IJL_OK,IJL_ROI_OK]
    end;

    ijlFree(@jcprops)
  end;
end;

function ijl_Compress_dib(JpgBuff: Pointer;
                          Jpg_w,Jpg_h,MaxSize: Integer;

                          JpgPath: PChar;

                          Dib: Pointer;
                          Dib_w,Dib_h,Dib_bits: Integer;

                          Tables: PJPEG_Tables;

                          Command: TIJLIOType;
                          Quality: Integer): Integer;
var
  jcprops: TJPEG_CORE_PROPERTIES;
begin
  Result:=0;

  if ijl_Active then
  if dib_bits in [8,24] then

  if ijlInit(@jcprops) = ijl_OK then begin

    if Assigned(Tables) then
    if Tables.isValid = S_OK then
    ijl_back_tables(Tables,@jcprops.jprops);

    jcprops.DIBChannels:=1;
    jcprops.DIBColor:=IJL_G;

    if dib_bits = 24 then begin
      jcprops.DIBChannels:=3;
      jcprops.DIBColor:=IJL_RGB;
    end;

    jcprops.DIBHeight:=dib_h;
    jcprops.DIBWidth:=dib_w;
    jcprops.DIBBytes:=dib;

    jcprops.DIBPadBytes:=pad_Line(dib_w,dib_bits);

    jcprops.JPGHeight:=jpg_h;
    jcprops.JPGWidth:=jpg_w;

    if dib_bits = 8 then begin
      jcprops.JPGChannels:=1;
      jcprops.JPGColor:=IJL_G;
      jcprops.JPGSubsampling:=IJL_PAD2;
    end;

    jcprops.jquality:=Quality;

    if JpgPath <> nil then begin

      jcprops.JPGFile:=JpgPath;
      if ijlWrite(@jcprops,Command) = IJL_OK then
      Result:=1

    end
    else begin
      jcprops.JPGBytes:=JpgBuff;
      jcprops.JPGSizeBytes:=MaxSize;

      if ijlWrite(@jcprops,Command) = IJL_OK then
      Result:=jcprops.JPGSizeBytes;
    end;

    ijlFree(@jcprops)
  end
end;

function ijl_Compress_header(Dest: Pointer;
                             DestSize: Integer;

                             Dib: Pointer;
                             Dib_w,Dib_h,Dib_bits: Integer;

                             Quality: Integer): Integer;
begin
  Result:=ijl_Compress_dib(Dest, Dib_w,Dib_h,DestSize,
                           nil, Dib,Dib_w,Dib_h,Dib_bits,
                           nil,IJL_JBUFF_WRITEHEADER,
                           Quality)
end;

function ijl_Compress_entropy(Dest: Pointer; DestSize: Integer;

                              Dib: Pointer;
                              Dib_w,Dib_h,Dib_bits: Integer;

                              Tables: PJPEG_Tables;
                              Quality: Integer): Integer;
begin
  Result:=ijl_Compress_dib(Dest, Dib_w,Dib_h,DestSize,
                           nil, Dib,Dib_w,Dib_h,Dib_bits,
                           Tables,IJL_JBUFF_WRITEENTROPY,
                           Quality)
end;

function ijl_Compress_image(Dest: Pointer;
                            DestSize: Integer;

                            Dib: Pointer;
                            Dib_w,Dib_h,Dib_bits: Integer;

                            Quality: Integer): Integer;
begin
  Result:=ijl_Compress_dib(Dest, Dib_w,Dib_h,DestSize,
                           nil, Dib,Dib_w,Dib_h,Dib_bits,
                           nil,IJL_JBUFF_WRITEWHOLEIMAGE,
                           Quality)
end;

function ijl_Compress_file(Dest: PChar; img: XImage): Boolean;
begin
  with img do
  Result:=ijl_Compress_dib(nil, im_w,im_h,0,
                           Dest, bmpBits,im_w,im_h,im_bits,
                           nil,IJL_JFILE_WRITEWHOLEIMAGE,
                           93) > 0
end;

function ijl_Compress_map(Dest: PChar; const map: Bitmap): Boolean;
begin
  Result:=false;
  with map do if Assigned(bmBits) then
  if (bmPlanes = 1) and (bmBitsPixel in [8,24]) then

  Result:=ijl_Compress_dib(nil, bmWidth,bmHeight,0,
                           Dest, bmBits,bmWidth,bmHeight,bmBitsPixel,
                           nil,IJL_JFILE_WRITEWHOLEIMAGE,
                           93) > 0
end;

function ijl_Decompress_file(jpg: PChar; dst: XImage; iz: int): Boolean;
var
  jcprops: TJPEG_CORE_PROPERTIES;
  w,h: int; IoType: TIJLIOTYPE;
begin
  dst.im_Close;

  if ijl_Active then
  if ijlInit(@jcprops) = ijl_OK then begin

    jcprops.JPGFile:=jpg;

    ijl_Err:=ijlRead(@jcprops,IJL_JFILE_READPARAMS);
    if ijl_Err = IJL_OK then

    with jcprops do
    if JPGWidth > 0 then
    if JPGHeight > 0 then begin

      IoType:=IJL_JFILE_READWHOLEIMAGE;
      w:=JPGWidth; h:=JPGHeight;

      case iz of
    2:  IoType:=IJL_JFILE_READONEHALF;
    4:  IoType:=IJL_JFILE_READONEQUARTER;
    8:  IoType:=IJL_JFILE_READONEEIGHTH;
      end;

      if IoType <> IJL_JFILE_READWHOLEIMAGE then
      begin w:=w div iz; h:=h div iz end;

      if dst.im_Create(w,h,24) then begin
        DIBBytes:=PByte(dst.Lines[0]);
        DIBWidth:=dst.im_w;
        DIBHeight:=dst.im_h;
        DIBColor:=IJL_RGB;
        DIBChannels:=3;
        DIBPadBytes:=dst.im_pad;

        ijl_Err:=ijlRead(@jcprops,IoType);
        Result:=ijl_Err = IJL_OK
      end
    end;

    ijlFree(@jcprops)
  end
end;

function ijl_Decompress_image(Data: Pointer; DataLen: Integer;
                              dst: XImage): int;
var
  jcprops: TJPEG_CORE_PROPERTIES;
begin
  Result:=IJL_RESERVED;

  dst.is_tif:=false;
  dst.is_bmp:=false;

  if ijl_Active then
  if ijlInit(@jcprops) = ijl_OK then begin

    jcprops.JPGBytes:=Data;
    jcprops.JPGSizeBytes:=DataLen;

    Result:=ijlRead(@jcprops,IJL_JBUFF_READPARAMS);
    if Result = IJL_OK then  begin

      Result:=IJL_RESERVED-1;

      with jcprops do
      if JPGWidth > 0 then
      if JPGHeight > 0 then

      if dst.im_Alloc(JPGWidth,JPGHeight,24) then begin
        DIBBytes:=PByte(dst.Lines[0]);
        DIBWidth:=dst.im_w;
        DIBHeight:=dst.im_h;
        DIBColor:=IJL_BGR;
        DIBChannels:=3;
        DIBPadBytes:=dst.im_pad;

        Result:=ijlRead(@jcprops,IJL_JBUFF_READWHOLEIMAGE);
      end
    end;

    ijlFree(@jcprops)
  end;

  if Result <> IJL_OK then dst.im_Close
end;

function ijl_Decompress_data(Data: Pointer; DataLen: Integer;
                             dst: XImage): Boolean;
begin
  Result:=ijl_Decompress_image(Data,DataLen,dst) = ijl_OK
end;

function ijl_Bitblt(Buf: Pointer; Size: Integer;
                    dst: XImage): Boolean;
var
  jcprops: TJPEG_CORE_PROPERTIES;
begin
  if ijl_Active then
  if ijlInit(@jcprops) = ijl_OK then begin

    jcprops.JPGBytes:=Buf;
    jcprops.JPGSizeBytes:=Size;

    if ijlRead(@jcprops,IJL_JBUFF_READPARAMS) = IJL_OK then

    with jcprops do
    if JPGWidth > 0 then
    if JPGHeight > 0 then

    if dst.im_bits = 24 then begin
      DIBBytes:=PByte(dst.Lines[0]);
      DIBWidth:=dst.im_w;
      DIBHeight:=-dst.im_h;
      DIBColor:=IJL_BGR;
      DIBChannels:=3;
      DIBPadBytes:=dst.im_pad;

      if ijlRead(@jcprops,IJL_JBUFF_READWHOLEIMAGE) = IJL_OK then
      Result:=true
    end;

    ijlFree(@jcprops)
  end
end;

destructor TJpgView.Destroy;
begin
  if fActive then
  ijlFree(@jcprops)
end;

function TJpgView.Open(Path: PChar): Boolean;
begin
  Result:=false; Close;

  if ijl_Active then
  if ijlInit(@jcprops) = ijl_OK then begin

    jcprops.JPGFile:=Path;
    if ijlRead(@jcprops,IJL_JFILE_READPARAMS) = IJL_OK then

    with jcprops do
    if JPGWidth > 0 then
    if JPGHeight > 0 then Result:=true;

    fActive:=true
  end
end;

function TJpgView.Get_rgb(Buf: Pointer): Boolean;
begin
  Result:=false;

  if fActive then

  with jcprops do
  if JPGWidth > 0 then
  if JPGHeight > 0 then begin
    DIBBytes:=Buf;
    DIBWidth:=JPGWidth;
    DIBHeight:=JPGHeight;
    DIBColor:=IJL_RGB;
    DIBChannels:=3;
    DIBPadBytes:=0;

    if ijlRead(@jcprops,IJL_JFILE_READWHOLEIMAGE) = IJL_OK then
    Result:=true
  end
end;

function TJpgView.BitBlt(im: XImage): Boolean;
begin
  Result:=false;

  if fActive then
  if im.Active then
  if im.im_bits = 24 then

  with jcprops do
  if JPGWidth > 0 then
  if JPGHeight > 0 then begin
    DIBBytes:=PByte(im.Lines[0]);
    DIBWidth:=im.im_w;
    DIBHeight:=im.im_h;
    DIBColor:=IJL_BGR;
    DIBChannels:=3;
    DIBPadBytes:=im.im_line-(im.im_w*3);

    if ijlRead(@jcprops,IJL_JFILE_READWHOLEIMAGE) = IJL_OK then
    Result:=true
  end
end;

procedure TJpgView.Close;
begin
  if fActive then ijlFree(@jcprops);
  FillChar(jcprops,Sizeof(jcprops),0);
  fActive:=false
end;

function jpg_pyramid(Dest,Path: PChar): Boolean;
var
  vm: TReadFile; dst: THandle;
  i,fatN,w,h,bits,line,bx,cx,iz: Integer;
  buf,buf1: PBytes; sz,sz1: Int64; mk: TJpegMarker;
  inf: TBitmapInfoHeader; fat: TJpegFatArray;
  fr: TJpegFatItem;
begin
  Result:=false;

  vm:=TReadFile.Create;
  try
    if vm.Open(Path) then
    if ijl_Get_info(vm.Buf,vm.Size,@inf) then

    if inf.biWidth <= 32000 then
    if inf.biHeight <= 32000 then begin

      iz:=2;
      w:=inf.biWidth div iz;
      h:=inf.biHeight div iz;
      bits:=img_bits(@inf);

      if (w >= 32) and (h >= 32) then begin

        line:=img_line(w,bits);
        sz:=line; sz:=line*h + 256;

        sz1:=vm.Size;

        buf:=xAllocPtr(sz);
        buf1:=xAllocPtr(sz1);

        if Assigned(buf) then
        if Assigned(buf1) then

        if ijl_Decompress(buf,w,h,bits,
                          vm.Buf,vm.Size,nil) then begin

          fr.ofs:=0; fr.len:=vm.Size;
          fr.width:=inf.biWidth;
          fr.height:=inf.biHeight;

          vm.Close; dst:=0;

          if Assigned(Dest) then
          if xStrThis(Dest,Path) then
          Dest:=nil;

          if Dest = nil then
            dst:=FileOpen(Path,fmOpenReadWrite)
          else
          if FileCopy(Path,Dest) then
            dst:=FileOpen(Dest,fmOpenReadWrite);

          if dst > 0 then begin

            Fillchar(fat,Sizeof(fat),0);
            fat[0]:=fr; fatN:=1;

            cx:=ijl_Compress_image(buf1,sz1, buf,w,h,bits,90);
            while cx > 0 do begin
              fr.ofs:=FileSeek(dst,0,2); fr.len:=cx;
              fr.width:=w; fr.height:=h;
              fat[fatN]:=fr; Inc(fatN);

              FileWrite(dst,buf1^,cx);
              if fatN = 16 then Break;

              iz:=iz * 2;
              w:=inf.biWidth div iz;
              h:=inf.biHeight div iz;
              if (w < 32) or (h < 32) then Break;

              bx:=cx; cx:=0;
              if ijl_Decompress(buf,w,h,bits,
                                buf1,bx,nil) then

              cx:=ijl_Compress_image(buf1,sz, buf,w,h,bits,90);
            end;

            if fatN > 1 then begin
              bx:=FileSeek(dst,0,2);

              cx:=fatN*Sizeof(fr);
              mk:=JpegMarker($FF01,cx);
              FileWrite(dst,mk,Sizeof(mk));
              FileWrite(dst,fat,cx);

              mk:=JpegMarker($FF02,4);
              FileWrite(dst,mk,Sizeof(mk));
              FileWrite(dst,bx,4);
            end;

            FileClose(dst); Result:=fatN > 1
          end
        end;

        xFreeptr(buf1);
        xFreeptr(buf);
      end
    end;
  finally
    vm.Free
  end;

end;

function Is_jpg_pyramid(Path: PChar): Boolean;
var
  vm: TReadFile; i,n,k,pos,pos1: Longint;
  inf,inf1: TBitmapInfoHeader; mk: TJpegMarker;
  fr: TJpegFatItem;
begin
  Result:=false;

  vm:=TReadFile.Create;
  try
    if vm.Open(Path) then
    if vm.Size > 8 then
    if ijl_Get_info(vm.Buf,vm.Size,@inf) then begin

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
            if n > 0 then begin

              k:=0;
              for i:=1 to n do begin
                pos1:=vm.Load(pos1,fr,Sizeof(fr));
                if vm.IsJpeg(fr.ofs,fr.len) then
                if ijl_Get_info(@vm.Buf[fr.ofs],fr.len,@inf1) then
                if fr.width = inf1.biWidth then
                if fr.height = inf1.biHeight then begin
                  Inc(k); inf:=inf1
                end
              end;

              Result:=k > 1
            end
          end
        end
      end
    end;
  finally
    vm.Free
  end
end;

constructor TJpegDecompThread.Create(AObj: PJpegDecompObj;
                                     ARotate: IRotateImage;
                                     AGamma: PBytes);
begin
  inherited Create(true);
  fObj:=AObj; fRotate:=ARotate;
  fGamma:=AGamma
end;

procedure TJpegDecompThread.Execute;
begin
  if Assigned(fRotate) then fRotate.Init(true);

  while not Terminated do begin
    Takt; Sleep(5)
  end;

  if Assigned(fRotate) then fRotate.Done
end;

procedure TJpegDecompThread.Takt;
var
  map: Bitmap;
begin
  with fObj^ do
  if Assigned(Data) then begin

    if Assigned(log) then
    if log.Active then
    log.WriteStr(Format('start len=%d',[DataLen]));

    Err:=ijl_Decompress_image(Data,DataLen,Dest);

    if Assigned(log) then
    if log.Active then
    log.WriteStr(Format('end err=%d',[Err]));

    if Assigned(fRotate) then
    if fRotate.GetActive then
    if Dest.GetMap(map) then
    fRotate.Exec(@map,@map);

    if Assigned(fGamma) then
    Dest.Apply_gamma(fGamma,nil,nil,nil);

    Data:=nil
  end;
end;

constructor TJpegDecompBuf.Create(Ind: int;
                                  ARotate: IRotateImage;
                                  AGamma: PBytes);
var
  fn: TShortstr;
begin
  inherited Create(4096*16);

  fImage:=XImage.Create;
  flog:=TTextWrite.Create;

  if log_enabled then
  flog.NewTemp(StrFmt(fn,'decomp%d',[Ind]));

  fCom.Dest:=fImage;
  fCom.log:=flog;

  fThread:=TJpegDecompThread.Create(@fCom,ARotate,AGamma);
  fThread.FreeOnTerminate:=false
end;

destructor TJpegDecompBuf.Destroy;
begin
  fThread.Free;

  flog.Free;
  fImage.Free;

  inherited
end;

procedure TJpegDecompBuf.Resume;
begin
  if flog.Active then
  flog.WriteStr('Resume');

  if not fActive then
  fThread.Resume; fActive:=true
end;

procedure TJpegDecompBuf.Suspend;
begin
  if flog.Active then
  flog.WriteStr('Suspend');

  if fActive then fThread.Suspend;
  fActive:=false
end;

procedure TJpegDecompBuf.Terminate;
begin
  fThread.Terminate; fActive:=false
end;

function TJpegDecompBuf.GetReady: bool;
begin
  Result:=fCom.Data = nil
end;

procedure TJpegDecompBuf.Unpack(Data: Pointer; len,Id: int);
begin
  if fCom.Data = nil then

  if len > 0 then

  if not fActive then begin
    fCom.DataLen:=len;
    fCom.Data:=Data;
    fThread.Takt;
    fId:=Id
  end else

  if Resize(len) then begin
    Move(Data^,Buffer[0],len);
    Size:=len;

    fCom.DataLen:=len;
    fCom.Data:=Buffer;

    fId:=Id
  end
end;

procedure TJpegDecompBuf.Unpackb(Data: TDataStream; Id: int);
begin
  if Data.Size > 0 then
  Unpack(Data.Buffer,Data.Size,Id);
end;

function TJpegDecompBuf.GetImage(out Err: int): XImage;
begin
  Result:=nil; Err:=0;

  if fCom.Data = nil then begin
    Err:=fCom.Err;

    if fImage.Active then
    Result:=fImage
  end
end;

end.
