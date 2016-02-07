unit Img_rgb; interface

uses
  math,otypes,img_x;

type
  TRGBImage = class(XImage)
    destructor Destroy; override;
    function Load(Path: PChar; tex: Boolean): Boolean;
  private
    fBuffer: PBytes;
    fBufSize: Longint;
  end;

implementation

uses
  ofiles;

type
  Char80 = Array[0..79] of Char;

  TRGBHeader = record
    Magic: Word;              // IRIS image file magic number
                              // This should be decimal 474
    Storage: Byte;            // Storage format
                              // 0 for uncompressed
                              // 1 for RLE compression
    BPC: Byte;                // Number of bytes per pixel channel
                              // Legally 1 or 2
    Dimension: Word;          // Number of dimensions
                              // Legally 1, 2, or 3
                              // 1 means a single row, XSIZE long
                              // 2 means a single 2D image
                              // 3 means multiple 2D images
    XSIZE: Word;              // X size in pixels
    YSIZE: Word;              // Y size in pixels
    ZSIZE: Word;              // Number of channels
                              // 1 indicates greyscale
                              // 3 indicates RGB
                              // 4 indicates RGB and Alpha
    PIXMIN: Longint;          // Minimum pixel value
                              // This is the lowest pixel value in the image
    PIXMAX: Longint;          // Maximum pixel value
                              //  This is the highest pixel value in the image
    DUMMY1: Longint;          // Ignored
                              // Normally set to 0
    IMAGENAME: Char80;        // Image name must be null terminated,
                              // therefore at most 79 bytes
    COLORMAP: Longint;        // Colormap ID
                              // 0 - normal mode
                              // 1 - dithered, 3 mits for red and green, 2 for blue, obsolete
                              // 2 - index colour, obsolete
                              // 3 - not an image but a colourmap
    DUMMY2: Array[1..404] of Byte;
  end;

destructor TRGBImage.Destroy;
begin
  fBuffer:=xFreePtr(fBuffer);
  inherited
end;

function TRGBImage.Load(Path: PChar; tex: Boolean): Boolean;

procedure SwapLongs(lp: PIntegers; n: Integer);
begin
  while n > 0 do begin
    lp[0]:=SwapInt(lp[0]);
    lp:=@lp[1]; Dec(n)
  end
end;

procedure UnpackRow(di: PBytes; n: Integer;
                    vm: TReadFile; bx,cx: Integer);
var
  si: PBytes; al,cl: Byte;
begin
  if vm.IsData(bx,cx) then begin
    si:=@vm.Buf[bx];

    while (cx > 1) and (n > 0) do begin
      al:=si[0]; si:=@si[1]; Dec(cx);

      cl:=al and $7F;
      if cl = 0 then Break else

      if al and $80 <> 0 then begin

        if cl > cx then cl:=cx;

        while cl > 0 do begin
          di[0]:=si[0]; Dec(cl);
          di:=@di[1]; Dec(n);
          si:=@si[1]; Dec(cx);
          if n = 0 then Break
        end

      end
      else begin
        al:=si[0]; si:=@si[1]; Dec(cx);
        while cl > 0 do begin
          di[0]:=al; Dec(cl);
          di:=@di[1]; Dec(n);
          if n = 0 then Break
        end
      end
    end

  end
  else Fillchar(di^,n,0)
end;

const
  _plane: Array[0..3] of Integer = (2,1,0,3);

var
  vm: TReadfile; hdr: TRGBHeader;
  iz,x,y,w,h,z,bits,sz,sz1,line,temp: Integer;
  offp,lenp: PIntegers; si,di,si1,di1: PBytes;
begin
  Result:=false; im_Close;

  vm:=TReadfile.Create;
  try
    if vm.Open(Path) then
    if vm.Size > Sizeof(hdr) then begin

      vm.Load(0,hdr,Sizeof(hdr));
      hdr.Magic:=SwapWord(hdr.Magic);
      hdr.Dimension:=SwapWord(hdr.Dimension);
      hdr.XSIZE:=SwapWord(hdr.XSIZE);
      hdr.YSIZE:=SwapWord(hdr.YSIZE);
      hdr.ZSIZE:=SwapWord(hdr.ZSIZE);

      if hdr.Magic = 474 then
      if hdr.Storage = 1 then
      if hdr.BPC = 1 then
      if hdr.XSIZE >= 32 then
      if hdr.YSIZE >= 32 then
      if hdr.ZSIZE in [1..4] then begin

        w:=hdr.XSIZE; h:=hdr.YSIZE; z:=hdr.ZSIZE;

        if tex then begin
          bits:=z*8; line:=w*z
        end
        else begin
          bits:=Min(3,z)*8;
          line:=img_Line(w,bits)
        end;

        sz1:=h*z*2*4;
        if vm.Size > 512 + sz1 then begin

          temp:=w * 8;

          sz:=sz1 + temp + line*h;

          if fBuffer = nil then begin
            fBuffer:=xAllocPtr(sz);
            fBufSize:=sz
          end else
          if sz > fBufSize then begin
            fBuffer:=xReallocPtr(fBuffer,sz);
            fBufSize:=sz
          end;

          if Assigned(fBuffer) then begin

            offp:=@fBuffer[0];
            lenp:=@offp[h*z];

            sz1:=sz1 div 2;
            vm.Load(512,offp[0],sz1);
            vm.Load(512+sz1,lenp[0],sz1);

            SwapLongs(offp,h*z);
            SwapLongs(lenp,h*z);

            si:=@fBuffer[sz1 + sz1];

            z:=Min(3,z);
            for iz:=0 to z-1 do begin

              di:=@si[temp];
              for y:=1 to h do begin

                if lenp[0] <= temp then
                UnpackRow(si,w,vm,offp[0],lenp[0]);

                di1:=@di[_plane[iz]];

                si1:=si;
                for x:=1 to w do begin
                  di1[0]:=si1[1];
                  si1:=@si1[1];
                  di1:=@di1[z]
                end;

                offp:=@offp[1];
                lenp:=@lenp[1];

                di:=@di[line]
              end;
            end;

            di:=@si[temp];
            Result:=im_Buffer(w,h,bits,di,false);

            is_bmp:=true
          end
        end
      end;

    end;
  finally
    vm.Free
  end;

  Result:=Active
end;

end.
