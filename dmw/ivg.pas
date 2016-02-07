unit ivg; interface

uses
  Classes,Graphics,
  Math,otypes;

const
  loc_hg  = 101;
  loc_hgt = 104;

  ivg_nn_iv = 1001;
  ivg_nn_ic = 1002;
  ivg_nn_r1 = 1003;
  ivg_nn_r2 = 1004;
  ivg_nn_df = 1005;

function hg_pack_color(alfa,ptyp,pcolor: int): int;
function hg_unpack_color(color: int; out alfa,ptyp: int): int;

function hgr_pack_color(color,thick,len: int): int;
function hgr_unpack_color(color: int; out thick,len: int): int;

function hgl_pack_color(st,w,color: int): int;
function hgl_unpack_color(color: int; out st,w: int): int;

function hgp_pack_color(msk,alfa,color: int): int;
function hgp_unpack_color(color: int; out msk,alfa: int): int;

function hgl_hide_color(color,bc: int): int;
function hgp_hide_color(color,bc: int): int;

function hgt_pack_color(r2,color,blank: int): int;
function hgt_unpack_color(color: int; out r2,blank: int): int;

function hgt_pack_code(dx,dy,r1: int): int;
function hgt_unpack_code(code: int; out dx,dy: int): int;

function IntsToStr(v: PIntegers; n: int): String;
function StrToInts(const S: String; v: PIntegers): int;

implementation

uses
  Sysutils,convert,
  xgdi,xline,xpoly,xpens;

function hg_pack_color(alfa,ptyp,pcolor: int): int;
begin
  Result:=RGB_24_16(pcolor) or
          (alfa shl 15) or (ptyp shl 21);
end;

function hg_unpack_color(color: int; out alfa,ptyp: int): int;
begin
  alfa:=(color shr 15) and $3F;
  ptyp:=(color shr 21) and $F;
  Result:=RGB_16_24(color);
end;

function hgr_pack_color(color,thick,len: int): int;
begin
  Result:=RGB_24_16(color)+
          (thick shl 15) + (len shl 20)

end;

function hgr_unpack_color(color: int; out thick,len: int): int;
begin
  thick:=(color shr 15) and $1F;
  len:=(color shr 20) and $FFF;
  Result:=RGB_16_24(color);
end;

function hgl_pack_color(st,w,color: int): int;
begin
  Result:=RGB_24_16(color)+
          (w shl 16) + (st shl 20)
end;

function hgl_unpack_color(color: int; out st,w: int): int;
begin
  st:=(color shr 20) and $F;
  w:=(color shr 16) and $F;
  Result:=RGB_16_24(color);
end;

function hgp_pack_color(msk,alfa,color: int): int;
begin
  alfa:=alfa shr 3;
  Result:=RGB_24_16(color)+
          (alfa shl 15) + (msk shl 20)
end;

function hgp_unpack_color(color: int; out msk,alfa: int): int;
begin
  msk:=(color shr 20) and $F;
  alfa:=((color shr 15) and $1F) shl 3;
  Result:=RGB_16_24(color);
end;

function hgl_hide_color(color,bc: int): int;
var
  st,w: int;
begin
  color:=hgl_unpack_color(color,st,w);
  Result:=hgl_pack_color(st,w,bc)
end;

function hgp_hide_color(color,bc: int): int;
var
  msk,alf: int;
begin
  color:=hgp_unpack_color(color,msk,alf);
  Result:=hgp_pack_color(msk,alf,bc)
end;

function hgt_pack_color(r2,color,blank: int): int;
var
  ax: tlong;
begin
  ax.i:=RGB_24_16(color);
  if blank > 0 then ax.i:=ax.i or $8000;
  ax.b[2]:=r2; Result:=ax.i
end;

function hgt_unpack_color(color: int; out r2,blank: int): int;
begin
  blank:=0;
  if (color and $8000) <> 0 then blank:=1;
  r2:=(color shr 16) and $FF;
  Result:=RGB_16_24(color);
end;

function pack_ibyte(v: int): int;
begin
  Result:=Abs(v);
  if v < 0 then Inc(Result,$80);
end;

function unpack_ibyte(v: int): int;
begin
  Result:=v and $7F;
  if v and $80 <> 0 then
  Result:=-Result
end;

function hgt_pack_code(dx,dy,r1: int): int;
begin
  Result:=r1 or
          (pack_ibyte(dx) shl 8) or
          (pack_ibyte(dy) shl 16)
end;

function hgt_unpack_code(code: int; out dx,dy: int): int;
begin
  dx:=unpack_ibyte(code shr 8);
  dy:=unpack_ibyte(code shr 16);
  Result:=code and 255
end;

function IntsToStr(v: PIntegers; n: int): String;
var
  i: int; s: String;
begin
  s:='';

  if n > 0 then begin
    s:=IntToStr(v[0]);
    for i:=1 to n-1 do
    if Length(s) < 200 then
    s:=s+' '+IntToStr(v[i]);
  end;

  Result:=s
end;

function StrToInts(const S: String; v: PIntegers): int;
var
  t: TShortstr;
begin
  Result:=0; StrPCopy(t,S);

  while Result < 16 do begin
    if not IntToken(t,v[Result]) then Break;
    Inc(Result)
  end
end;

end.