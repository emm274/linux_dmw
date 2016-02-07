unit img_dib; interface

uses
  Windows,Math,LCLType,
  OTypes,XInter,
  idib,xddw,img_x;

type
  TInterImage = class(XImage)
    constructor Create(AWidth,AHeight,ABits,AUsed,ADst: Integer);
    destructor Destroy; override;

    procedure Draw(DC: ICanvas;
                   ox,oy,ow,oh: Integer;
                   sx,sy,sw,sh: Integer);

  private
    fbuf: PBytes;
    fbuf_w: Integer;
    fbuf_h: Integer;
    fbuf_line: Integer;
    fbuf_dst: Integer;

    coeff: pinter_coeff;

    vga: TVGABITMAPINFO;

    function Get_Active: Boolean;
  public
    property Active: Boolean read Get_Active;
  end;

  TDibFrame = class(XImage)

    destructor Destroy; override;

    function BeginDraw(inf: PBitmapInfo;
                       x,y,w,h: int; kz: Float): Boolean;

    procedure Draw(DC: ICanvas;
                   ox,oy,ix,iy: int;
                   kz: Float);

    procedure Recode(chan: PBytes);
    procedure Cmyk(mac: Boolean);

  private
    finter: pinter_coeff;
    fmap: Bitmap; fSize: DWord;
    fSrcX,fSrcY,fZoom: int;
    fSrcOffs,fSrcLen: int;
    fInfo: TVGABITMAPINFO;

    function GetLine(y: Integer): PBytes;
    procedure SetLine(y: Integer; si: PBytes);

  public
    property Zoom: Integer read fZoom;

    property SrcX: Integer read fSrcX;
    property SrcY: Integer read fSrcY;

    property Width: Integer read fmap.bmWidth;
    property Height: Integer read fmap.bmHeight;

    property Lines[y: Integer]: PBytes read GetLine
                                       write SetLine;
  end;

implementation

uses
  Convert,xgdi;
  
constructor TInterImage.Create(AWidth,AHeight,ABits,AUsed,ADst: Integer);
var
  len: Integer;
begin
  inherited Create;

  if ABits in [8,24] then
  if AUsed = 0 then

  if ADst > 1 then
  if im_Alloc(AWidth,AHeight,ABits) then begin

    fbuf_w:=AWidth * ADst;
    fbuf_h:=AHeight * ADst;
    fbuf_line:=img_Line(fbuf_w,ABits);
    fbuf_dst:=ADst;

    len:=fbuf_h*fbuf_line;

    if (len > 0) and (len < 4096*4096*4) then
    fbuf:=xAllocPtr(len + Sizeof(tinter_coeff));

    if Assigned(fbuf) then begin
      coeff:=@fbuf[len];
      fill_inter_coeff(coeff,bilinear);

      with vga.bmiHeader do begin
        biSize:=SizeOf(vga.bmiHeader);
        biWidth:=fbuf_w; biHeight:=fbuf_h;
        biPlanes:=1; biBitCount:=ABits;
        biCompression:=bi_RGB;
      end;

      xGray_Quads(@vga.bmiColors,256);
    end
  end
end;

destructor TInterImage.Destroy;
begin
  xFreeptr(fbuf); inherited
end;

function TInterImage.Get_Active: Boolean;
begin
  Result:=Assigned(fbuf)
end;

procedure TInterImage.Draw(DC: ICanvas;
                           ox,oy,ow,oh: Integer;
                           sx,sy,sw,sh: Integer);
var
  loc,ix,iy,_iy,_ix,iw,k: Integer;
  si_line,di_line,ax0,ax1,ax2: Integer;
  si,_si1,_si2,di,di1: PBytes;
  cp: PFloats; c1,c2: Float;
begin
  if Assigned(fbuf) then begin

    k:=fbuf_dst;

    loc:=im_loc;
    si_line:=im_line;

    si:=Lines[sy];
    si:=@si[sx*loc];

    di_line:=fbuf_Line;
    di1:=@fbuf[sh*k*di_line];
    di1:=@di1[sx*k*loc];

    iw:=sw;
    if sx+sw >= im_w then
    iw:=im_w-1-sx;

    if loc = 1 then

      for iy:=1 to sh do begin
        for _iy:=0 to k-1 do begin

          _si1:=si; _si2:=si;
          if sy+iy < im_h then _si2:=@si[si_line];

          Dec(TPointer(di1),di_line); di:=di1;

          for ix:=1 to iw do begin

            for _ix:=0 to k-1 do begin

              cp:=pixel_inter_coeff(_ix/k,_iy/k,coeff);

              ax0:=Round(_si1[0]*cp[0]+_si1[1]*cp[1]+
                         _si2[0]*cp[2]+_si2[1]*cp[3]);

              if ax0 > 255 then ax0:=255;
              di[0]:=ax0; di:=@di[1]
            end;

            _si1:=@_si1[1]; _si2:=@_si2[1];
          end;

          if iw < sw then begin
            c2:=_iy/k; c1:=1 - c2;
            for _ix:=0 to k-1 do begin
              ax0:=Round(_si1[0]*c1+_si2[0]*c2);
              if ax0 > 255 then ax0:=255;
              di[0]:=ax0; di:=@di[1]
            end
          end
        end;

        si:=@si[si_line]
      end

    else
    if loc = 3 then

      for iy:=1 to sh do begin
        for _iy:=0 to k-1 do begin

          _si1:=si; _si2:=si;
          if sy+iy < im_h then _si2:=@si[si_line];

          Dec(TPointer(di1),di_line); di:=di1;

          for ix:=1 to iw do begin

            for _ix:=0 to k-1 do begin
              cp:=pixel_inter_coeff(_ix/k,_iy/k,coeff);

              ax0:=Round(_si1[0]*cp[0]+_si1[3]*cp[1]+
                         _si2[0]*cp[2]+_si2[3]*cp[3]);

              ax1:=Round(_si1[1]*cp[0]+_si1[4]*cp[1]+
                         _si2[1]*cp[2]+_si2[4]*cp[3]);

              ax2:=Round(_si1[2]*cp[0]+_si1[5]*cp[1]+
                         _si2[2]*cp[2]+_si2[5]*cp[3]);

              if ax0 > 255 then ax0:=255; di[0]:=ax0;
              if ax1 > 255 then ax1:=255; di[1]:=ax1;
              if ax2 > 255 then ax2:=255; di[2]:=ax2;

              di:=@di[3]
            end;

            _si1:=@_si1[3]; _si2:=@_si2[3];
          end;

          if iw < sw then begin
            c2:=_iy/k; c1:=1 - c2;
            for _ix:=0 to k-1 do begin
              ax0:=Round(_si1[0]*c1+_si2[0]*c2);
              ax1:=Round(_si1[1]*c1+_si2[1]*c2);
              ax2:=Round(_si1[2]*c1+_si2[2]*c2);

              if ax0 > 255 then ax0:=255; di[0]:=ax0;
              if ax1 > 255 then ax1:=255; di[1]:=ax1;
              if ax2 > 255 then ax2:=255; di[2]:=ax2;

              di:=@di[3]
            end
          end
        end;

        si:=@si[si_line]
      end;

    sx:=sx*k; sw:=sw*k; sh:=sh*k;
    vga.bmiHeader.biHeight:=sh;

    DC.StretchDIBits(ox,oy,ow,oh, fbuf,@vga, sx,0,sw,sh, 0,0);
  end
end;

destructor TDibFrame.Destroy;
begin
  xFreePtr(finter);
  xFreePtr(fmap.bmBits);
  inherited
end;

function TDibFrame.GetLine(y: Integer): PBytes;
var
  bx: Integer;
begin
  Result:=nil;
  if (y >= 0) and (y < Height) then begin
    bx:=fmap.bmWidthBytes * (Height-1-y);
    Result:=@PBytes(fmap.bmBits)[bx]
  end
end;

procedure TDibFrame.SetLine(y: Integer; si: PBytes);
var
  bx,iw,iz: Integer; di: PBytes;
begin
  if (y >= 0) and (y < Height) then begin
    bx:=fmap.bmWidthBytes * (Height-1-y);
    di:=@PBytes(fmap.bmBits)[bx];

    iw:=Width; iz:=Zoom;

    si:=@si[fSrcOffs];

    if iz = 1 then
      Move(si^,di^,fSrcLen)
    else
    case fmap.bmBitsPixel of
  1:  Zoom_pf1(si,di,iw,iz);
  4:  Zoom_pf4(si,di,iw,iz);
  8:  Zoom_pf8(si,di,iw,iz);
  16: Zoom_pf16(si,di,iw,iz);
  24: Zoom_pf24(si,di,iw,iz);
  32: Zoom_pf32(si,di,iw,iz);
    end
  end
end;

procedure TDibFrame.Recode(chan: PBytes);
var
  cx: DWord;
begin
  with fmap do
  cx:=bmWidthBytes * bmHeight;
  vga_Transit(fmap.bmBits,chan,cx);
end;

procedure TDibFrame.Cmyk(mac: Boolean);
var
  cx: DWord;
begin
  if fInfo.bmiHeader.biBitCount = 32 then begin
    with fmap do cx:=bmWidth * bmHeight;
    if mac then SwapInts(fmap.bmBits,cx);
    cmyk_buf(fmap.bmBits,cx)
  end
end;

function TDibFrame.BeginDraw(inf: PBitmapInfo;
                             x,y,w,h: int; kz: Float): Boolean;
var
  ax,bx,cx,dx,iz,bits: int; bp: PBytes;
begin
  Result:=false;

  fInfo:=PVGABITMAPINFO(inf)^;

  bits:=img_Bits(@fInfo);

  with fInfo.bmiHeader do begin
    if x < 0 then x:=0;

    bx:=img_Line_ofs(x,bits); ax:=x;
    x:=bx * 8 div bits; Inc(w,(ax-x));

    if x + w > biWidth then w:=biWidth-x;

    if y < 0 then y:=0;
    if y + h > biHeight then h:=biHeight-y
  end;

  if (w > 0) and (h > 0) then begin
    cx:=(w * bits+7) div 8;

    iz:=Max(1,Trunc(kz));
    if iz > 8 then iz:=iz div 2;

    w:=w div iz; h:=h div iz;

    fSrcX:=x; fSrcY:=y; fZoom:=iz;
    fSrcOffs:=bx; fSrcLen:=cx;

    dx:=img_Line(w,bits);
    cx:=dx * h;

    bp:=fMap.bmBits;
    if cx > fSize then begin
      fSize:=0; bp:=xFreePtr(bp);
    end;

    if bp = nil then begin
      bp:=xAllocPtr(cx);
      if Assigned(bp) then fSize:=cx
    end;

    fmap.bmWidth:=w;
    fmap.bmHeight:=h;
    fmap.bmWidthBytes:=dx;
    fmap.bmPlanes:=1;
    fmap.bmBitsPixel:=bits;
    fmap.bmBits:=bp;

    fInfo.bmiHeader.biWidth:=w;
    fInfo.bmiHeader.biHeight:=h;

    Result:=Assigned(bp)
  end
end;

procedure TDibFrame.Draw(DC: ICanvas;
                         ox,oy,ix,iy: int;
                         kz: Float);
var
  w,h,dx,dy,iz,bits: int;
  inf: PBitmapInfo; bp: Pointer;
  map2: Bitmap;
begin
  inf:=@fInfo; bp:=fmap.bmBits;

  ox:=ox + Round((SrcX - ix)/kz);
  oy:=oy + Round((SrcY - iy)/kz);

  kz:=Zoom/kz;

  w:=Width;  dx:=Round(w*kz);
  h:=Height; dy:=Round(h*kz);

  iz:=Trunc(kz);
  if iz > 2 then begin
    while (w/4096)*(h/4096)*iz > 4 do iz:=iz div 2;

    if iz > 1 then begin
      bits:=inf.bmiHeader.biBitCount;
      is_tif:=false; if bits in [24] then begin

        if finter = nil then begin
          finter:=xAllocPtr(Sizeof(tinter_coeff));
          fill_inter_coeff(finter,bilinear);
        end;

        if Assigned(finter) then
        if im_Realloc(w*iz,h*iz,bits) then
        if GetMap(map2) then
        if bilinear_bmp(fmap,map2,finter) then begin

          w:=map2.bmWidth;
          h:=map2.bmHeight;
          fInfo.bmiHeader.biWidth:=w;
          fInfo.bmiHeader.biHeight:=h;

          bp:=map2.bmBits
        end
      end
    end
  end;

  DC.StretchDIBits(ox,oy,dx,dy, bp,inf, 0,0,w,h, 0,0);

  fInfo.bmiHeader.biWidth:=fmap.bmWidth;
  fInfo.bmiHeader.biHeight:=fmap.bmHeight;
end;

end.
