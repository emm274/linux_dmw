unit xbmp; interface

uses
  Classes, LCLType, Math, otypes;

function BitmapAsBmp(const map: Bitmap; Dest: PChar): Boolean;
function BitmapAsPng(const map: Bitmap; Dest: PChar): Boolean;

implementation

uses
  SysUtils,png,img_x;

type
  TBmpExport = class
    constructor Create(im: PBitMapInfo; Path: PChar);
    destructor Destroy; override;

    function PutLines(buf: PBytes; N: Integer): Boolean;

  private
    fHandle,fLine,fCount: Integer;
  public
    property Handle: Integer read fHandle;
    property Line: Integer read fLine;
  end;

constructor TBmpExport.Create(im: PBitMapInfo; Path: PChar);
var
  bmp: TBitMapFileHeader;
  h,bits,len,size,cols: Integer;
begin
  inherited Create;

  h:=FileCreate(Path);
  if h > 0 then begin

    with im.bmiHeader do begin
      biXPelsPerMeter:=96;
      biXPelsPerMeter:=96;
    end;

    bits:=img_Bits(@im.bmiHeader);
    fLine:=img_Line(im.bmiHeader.biWidth,bits);
    size:=im.bmiHeader.biHeight * fLine;

    cols:=im.bmiHeader.biClrUsed;
    len:=SizeOf(TBitMapInfoHeader)+cols*4;

    FillChar(bmp,SizeOf(bmp),0);
    with bmp do begin
      bfType:=$4D42; bfSize:=size;
      bfOffBits:=SizeOf(bmp)+len
    end;

    im.bmiHeader.biSize:=SizeOf(TBitMapInfoHeader);

    FileWrite(h,bmp,SizeOf(bmp));
    FileWrite(h,im^,len);

    fCount:=im.bmiHeader.biHeight;
    fHandle:=h
  end
end;

destructor TBmpExport.Destroy;
begin
  if fHandle > 0 then
  FileClose(fHandle)
end;

function TBmpExport.PutLines(buf: PBytes; N: Integer): Boolean;
begin
  Result:=false;
  if fCount > 0 then begin
    N:=Min(N,fCount);
    FileWrite(fHandle,buf^,fLine*N);
    Dec(fCount,N); Result:=true
  end
end;

function BitmapAsBmp(const map: Bitmap; Dest: PChar): Boolean;
var
  bmp: TBmpExport;
  inf: TBitmapInfo;
  y,w,h,line,bx,bits: int;
  buf,si: PBytes;
begin
  Result:=false;

  w:=map.bmWidth;
  h:=map.bmHeight;
  bits:=map.bmBitsPixel;

  if w*h > 0 then
  if bits in [24,32] then begin

    Fillchar(inf,Sizeof(inf),0);

    with inf.bmiHeader do begin
      biSize:=SizeOf(inf.bmiHeader);
      biWidth:=w; biHeight:=h;
      biPlanes:=1; biBitCount:=24;
      biCompression:=bi_RGB;

      line:=img_Line(w,24);
      biSizeImage:=line * h;
    end;

    bmp:=TBmpExport.Create(@inf,Dest);
    try
      if bmp.Handle <> 0 then begin

        buf:=xAllocPtr(line);
        if Assigned(buf) then begin

          for y:=0 to h-1 do begin
            si:=map.bmBits;
            bx:=map.bmWidthBytes;
            si:=@si[(h-1-y)*bx];

            if bits = 24 then
              Move(si^,buf^,bx)
            else
              pf32_to_pf24(si,buf,w);

            bmp.PutLines(buf,1);
          end;

          xFreePtr(buf)
        end;

        Result:=true
      end;
    finally
      bmp.Free;
    end;
  end;
end;

procedure png_write_data(png: png_structp; data: png_bytep; len: png_size_t);cdecl;
var
  p: PLongint;
begin
  p:=png_get_io_ptr(png);
  FileWrite(p^,data^,len)
end;

procedure png_write_flush(png: png_structp);cdecl;
begin
end;

function BitmapAsPng(const map: Bitmap; Dest: PChar): Boolean;
var
  png: png_structp;
  info: png_infop;
  f: Longint; y,bx,typ: int;
  line: Pointer;
begin
  Result:=false;

  if map.bmBitsPixel in [24,32] then begin
    png:=png_create_write_struct(png_get_libpng_ver(nil), nil, nil, nil);
    if Assigned(png) then begin

      info:=png_create_info_struct(png);
      if Assigned(info) then begin

        {$IOCHECKS OFF}
        f:=FileCreate(Dest);
        if f <> 0 then begin

          png_set_write_fn(png,@f,png_write_data,png_write_flush);

          typ:=6;
          if map.bmBitsPixel =  24 then
          typ:=2;

          png_set_IHDR(png,info,map.bmWidth,map.bmHeight,
                       8,    // bit depth
                       typ,  // color type
                       0,    // PNG_INTERLACE_NONE
                       0,    // PNG_COMPRESSION_TYPE_DEFAULT
                       0     //PNG_FILTER_TYPE_DEFAULT
                       );

          png_write_info(png,info);

          bx:=map.bmWidthBytes;
          for y:=0 to  map.bmHeight-1 do begin
            line:=@PBytes(map.bmBits)[y*bx];
            png_write_row(png,line);
          end;

          png_write_end(png,info);
          FileClose(f);

          Result:=true
        end;

        png_destroy_info_struct(png,@info)
      end;

      png_destroy_write_struct(@png, nil);
    end;
  end;
end;

end.

