unit XInter; interface

uses
  Windows,LCLType,Math,OTypes;

type
  tinterpolation = (nearest,bilinear,bicubic);

  tpixel_coeff = array[0..15] of Float;

  pinter_coeff = ^tinter_coeff;
  tinter_coeff = array[0..255] of tpixel_coeff;

  TInterpolator = class
    constructor Create;
    destructor Destroy; override;

    function pixel_coeff(x,y: Double): PFloats;

  private
    fMethod: tinterpolation;
    coeff: pinter_coeff;
    null: tpixel_coeff;

    procedure SetMethod(Value: tinterpolation);

  public
    property Method: tinterpolation read fMethod
                                    write SetMethod;
  end;

procedure fill_inter_coeff(coeff: pinter_coeff;
                           method: tinterpolation);

function pixel_inter_coeff(x,y: Double;
                           coeff: pinter_coeff): PFloats;

function Mixed_rgb(cl: PIntegers; coeff: PFloats;
                   Count: Integer): Integer;

function bilinear_bmp(const src,dst: Bitmap;
                      coeff: pinter_coeff): bool;

implementation

procedure fill_inter_coeff(coeff: pinter_coeff;
                           method: tinterpolation);

function cubic_func(a,x: Double): Float;
var
  x3,x2: Double;
begin
  Result:=0;

  x:=Abs(x); x2:=x*x; x3:=x2*x;

  if x < 1 then
    Result:=(a+2)*x3 - (a+3)*x2 + 1
  else
  if x < 2 then
    Result:=a*x3 - 5*a*x2 + 8*a*x - 4*a;
end;

function cubic_dist_func(i,j: Integer; a,u,v: Double): Float;
var
  x,y,r: Double;
begin
  x:=i-u; y:=j-v;
  r:=Hypot(x-u,y-v);
  Result:=cubic_func(a,r)
end;

var
  i,j,k, x,y: Integer; a,u,v,s: Double;
  pix: tpixel_coeff;
begin
  Fillchar(pix,Sizeof(pix),0);
  Fillchar(coeff^,Sizeof(tinter_coeff),0);

  for y:=0 to 15 do
  for x:=0 to 15 do begin
    u:=x/15; v:=y/15;

    if method = bilinear then begin
      pix[0]:=(1-u)*(1-v);
      pix[1]:=u*(1-v);
      pix[2]:=(1-u)*v;
      pix[3]:=u*v;
    end else
    if method = bicubic then begin

      a:=-0.5; k:=0; s:=0;

      for j:=-1 to 2 do for i:=-1 to 2 do begin
        pix[k]:=cubic_dist_func(i,j, a,u,v);
        s:=s + pix[k]; Inc(k)
      end;

      for k:=0 to 15 do pix[k]:=pix[k] / s
    end;

    coeff[0]:=pix;
    coeff:=@coeff[1]
  end
end;

function pixel_inter_coeff(x,y: Double;
                           coeff: pinter_coeff): PFloats;
var
  u,v: int;
begin
  u:=Trunc(Frac(Abs(x))*16);
  v:=Trunc(Frac(Abs(y))*16);
  Result:=@coeff[v*16 + u]
end;

constructor TInterpolator.Create;
begin
  inherited;
  coeff:=xAllocPtr(Sizeof(tinter_coeff));
  null[0]:=1; Method:=bilinear
end;

destructor TInterpolator.Destroy;
begin
  xFreeptr(coeff); inherited;
end;

procedure TInterpolator.SetMethod(Value: tinterpolation);
begin
  fMethod:=Value;
  if Assigned(coeff) then
  fill_inter_coeff(coeff,Value);
end;

function TInterpolator.pixel_coeff(x,y: Double): PFloats;
var
  u,v: Integer;
begin
  if coeff = nil then
    Result:=@null
  else begin
    u:=Trunc(Frac(Abs(x))*16);
    v:=Trunc(Frac(Abs(y))*16);
    Result:=@coeff[v*16 + u]
  end;
end;

function Mixed_rgb(cl: PIntegers; coeff: PFloats;
                   Count: Integer): Integer;
var
  ax0,ax1,ax2: Float;
  i,b0,b1,b2: Integer; ax: tlong;
begin
  Result:=0; ax0:=0; ax1:=0; ax2:=0;

  for i:=1 to Count do begin
    ax.i:=cl[0];
    ax0:=ax0 + ax.b[0]*coeff[0];
    ax1:=ax1 + ax.b[1]*coeff[0];
    ax2:=ax2 + ax.b[2]*coeff[0];

    cl:=@cl[1]; coeff:=@coeff[1]
  end;

  b0:=Round(ax0); if b0 > 255 then b0:=255;
  b1:=Round(ax1); if b1 > 255 then b1:=255;
  b2:=Round(ax2); if b2 > 255 then b2:=255;

  tlong(Result).b[0]:=b0;
  tlong(Result).b[1]:=b1;
  tlong(Result).b[2]:=b2;
end;

function bilinear_bmp(const src,dst: Bitmap;
                      coeff: pinter_coeff): bool;
type
  tcolor = Array[0..2] of int;
var
  i,ax,bits,x,y,ix,iy,k,w,h,w1,h1: int;
  si_line,di_line: int; si: PBytes;
  c1,c2,c3,c4: tcolor; cp: PFloats;
  si1,si2,di,di1,p1,p2: PByte;
begin
  Result:=false;

  bits:=src.bmBitsPixel;
  w:=src.bmWidth;
  h:=src.bmHeight;

  k:=dst.bmWidth div w;

  if w*k = dst.bmWidth then
  if h*k = dst.bmHeight then

  if dst.bmBitsPixel = bits then
  if bits in [8,24] then begin

    w1:=w-1; h1:=h-1;

    si:=src.bmBits;
    di:=dst.bmBits;

    si_line:=src.bmWidthBytes;
    di_line:=dst.bmWidthBytes;

    for y:=0 to h1 do begin

      for iy:=0 to k-1 do begin

        si1:=@si[y*si_line]; si2:=si1;
        if y < h1 then Inc(si2,si_line);

        di1:=di;

        for x:=0 to w1 do
        if bits = 8 then begin

          c1[0]:=si1^; c2[0]:=c1[0];
          c3[0]:=si2^; c4[0]:=c3[0];

          p1:=si1; p2:=si2;
          if x < w1 then begin
            Inc(p1); c2[0]:=p1^;
            Inc(p2); c4[0]:=p2^;
          end;

          for ix:=0 to k-1 do begin

            cp:=pixel_inter_coeff(ix/k,iy/k,coeff);

            ax:=Round(c1[0]*cp[0]+c2[0]*cp[1]+
                      c3[0]*cp[2]+c4[0]*cp[3]);

            if ax > 255 then ax:=255;
            di1^:=ax; Inc(di1)
          end;

        end
        else begin
          for i:=0 to 2 do begin
            c1[i]:=si1^; Inc(si1);
            c3[i]:=si2^; Inc(si2);
          end;

          c2:=c1; c4:=c3;

          p1:=si1; p2:=si2;
          if x < w1 then
          for i:=0 to 2 do begin
            c2[i]:=p1^; Inc(p1);
            c4[i]:=p2^; Inc(p2);
          end;

          for ix:=0 to k-1 do begin

            cp:=pixel_inter_coeff(ix/k,iy/k,coeff);

            for i:=0 to 2 do begin
              ax:=Round(c1[i]*cp[0]+c2[i]*cp[1]+
                        c3[i]*cp[2]+c4[i]*cp[3]);

              if ax > 255 then ax:=255;
              di1^:=ax; Inc(di1);
            end
          end;

        end;

        Inc(di,di_line)
      end
    end;

    Result:=true
  end
end;

end.
