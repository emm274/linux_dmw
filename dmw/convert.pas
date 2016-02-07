unit convert; interface {$H-}

uses                  
  Classes,
  Math,otypes;

const
  Is_semicolon_Delimiter: Longbool = false;
  Is_Comma_Delimiter: Longbool = true;
  Is_Seconds_Align: Longbool = false;
  Is_Real_Point: Longbool = false;
  Is_Angle_fmt: Longbool = false;
  is_Number: Longbool = false;

  is_Number_code: Longbool = false;

  Angle_fmt: Integer = 0;
                                                                       
  cHex: array[0..15] of Char = '0123456789ABCDEF';

  ms_scale  = 100000000;
  deg_scale = 1000000;
  rad_scale = ms_scale;

  deg_scale1 = Pi/180*ms_scale;
  rad_scale1 = 180/Pi*deg_scale;

  DegToRad = Pi/180;
  RadToDeg = 180/Pi;

  DegToRadi = Pi/180/deg_scale;
  RadToDegi = 180/Pi*deg_scale;

  day_seconds = 24 * 3600;
  week_seconds = 7 * 24 * 3600;

  time_scale  = day_seconds / ms_scale;
  time_scale1 = day_seconds * 1000;

  rus_month: array[1..12] of string[15] =
  ('January','February','March','April','May','June','July',
   'August','September','October','November','December');

var
  max_Single: tlong;

function SetFlag(flags,fl: Integer; up: Boolean): Integer;
function SwapFlag(flags,fl: Integer): Integer;

procedure set_Angle_fmt(fmt: Integer); stdcall;

function IsDigit(Ch: Char): Boolean;

function GarminRadian(v: Double): Double;

function TruncMin(v: Double): Double;

function sec_Time(T: TDateTime): Integer;
function degree_Round(F: Double): Integer;

function xInc(v,dv: Integer): Integer;

function Decimals(i: Integer): Integer;
function MaxDecimal(i: Integer): Integer;
function MaxDigits(i: Integer): Integer;
function RoundDecimal(i: Integer): Integer;

function this_x2(i: Integer): Boolean;
function degree_x2(x: Integer): Integer;
function power_x2(n: Integer): Integer;
function int_x2(i: Integer): Integer;
function min_x2(v: Integer): Integer;
function max_x2(v: Integer): Integer;

function Align_min(t: Integer): Integer;

function val_Align(var v: Integer; dv,per: Integer): Boolean;

function int_Align(i,k: Integer): Integer;
function int_Round(i,k: Integer): Integer;
function int_Trunc(i,k: Integer): Integer;
function int_Even(i: Integer): Integer;
function int_Tiles(i,j: Integer): Integer;

function int_Size(var s: Integer; k: Float): Integer;

function IsGeoid(b,l: Double): bool;

function Double_Correct(r: double): Boolean;
function Short_Correct(r: float): Boolean;

function Short_Assign(r: float): float;

function mRound(x: Double; m: Integer): Double;
function sRound(x: Double; m: Integer): Double;

function m_Real(d: double; max: Integer): Integer;
function bin_Real(d: double; m: Integer): double;
function xRound(x: Double): Integer;

function iCompare(i, i1,i2, cmd: int): Boolean;
function fCompare(f, f1,f2: double; cmd: int): Boolean;

function Str_chk(S: PChar): Integer;
function chk_Str(const S: String): Integer;
function buf_chk(P: PBytes; Len: Integer): Integer;

function xString(s: String; len: Integer): string;

function xStrPas(s: PChar): string;
function xStrLen(s: PChar): Integer;
function StrLPas(s: PChar; MaxLen: Integer): string;

function xStrPCopy(dst: PChar; src: String): PChar;
function xStrCopy(dst,src: PChar): PChar;

function StrQuery(Msg: PChar; const Capt: String): PChar;

function xStrLCat(dst,src: PChar; len: Integer): PChar;

function StrRecode(s: PChar; ch1,ch2: Char): PChar;

function StrPrefix(Prefix,Name: PChar): PChar;
function StrPhoto(Name: PChar; out row,col: Integer): Boolean;

function xStrScan(str,chars: PChar): PChar;
function xStrSkip(str,chars: PChar): PChar;

function StrAnsi(s: PChar): PChar;

function xStrName(dst,src: PChar; Ind: Integer): PChar;

function StrToNnn(const s: String): Integer;
function StrToInt(const s: String): Integer;
function StrToInt64(const s: String): Int64;

function str_icomp(s: PChar; v: Integer): Boolean;

function xHexToStr(v: Integer): string;
function HexToStr(v: int64; len: Integer): string;
function DecToStr(v: int64; len: Integer): string;
function StateToStr(state: uint): String;
function BitToStr(v: int64; len: Integer): string;
function FlagsToStr(flags,bit: Integer): string;

function KByteStr(Bytes: int64): String;

function LongStr3(v: Longint): String;
function LargeToStr(v: Int64): string;
function sIntToStr(v: Integer): string;

function xIntToStr(v,len: Integer; ch: char): string;
function xIdToStr(Id: Cardinal; len: Integer; ch: char): string;
function EpsToStr(r: double; M: Integer): string;
function AxeToStr(r: double; M: Integer): string;
function RealToStr(r: double; M: Integer): string;

function StrToLStr(s: PChar; L: Integer): string;
function IntToLStr(i: Integer; L: Integer): string;
function RealToLStr(r: double; L,M: Integer): string;
function FloatToLStr(r: double; E,L,M: Integer): string;

function RealToStrz(r: Double; m: int): string;

function DistToStr(r: double): string;
function ValToStr(r: double; L: Integer): string;
function mRealToStr(r: double; M: Integer): string;
function xRealToStr(r: double; M: Integer): string;
function FloatToStr(v: double; M: Integer): string;
function ValueToStr(v: double; M: Integer): String;
function TruncRStr(S: String): String;
function LPointStr(const p: TPoint): string;
function xSizeStr(w,h: Integer): string;
function LSizeStr(w,h: Integer): string;
function GaussStr(const g: tgauss; m: Integer): string;
function CoordStr(const g: tgauss; m: Integer): string;
function xyStr(const g: tgauss; m: Integer): String;
function xyzStr(const v: txyz; m: Integer): String;
function MatrixStr(Nx,Ny: Integer): String;

procedure IntToChar(v: Integer; var buf; len: Integer);
procedure xIntToChar(v: Integer; var buf; len: Integer);
procedure xRealToChar(v: Double; m: Integer; var buf; len: Integer);
function StrToChar(s: PChar; var buf; len: Integer): Integer;
function xStrToChar(const s: string; var buf; len: Integer): Integer;
function sStrToChar(const s: string; var buf; len: Integer): Integer;
procedure xAngleToChar(f: Double; var buf; len: Integer);

function StrWide(dst: PChar;
                 const src: WideString;
                 MaxLen: Integer): PChar;

function PStrToWideChar(const Src: String; Dest: PWideChar;
                        DestSize: Integer): PWideChar;

function StrToWideChar(Src: PChar; Dest: PWideChar;
                       DestSize: Integer): PWideChar;

function sCatStr(s1,s2,cs: String): String;

function sStrCat(dst,src: PChar): PChar;
function sTxtCat(dst,src: PChar): PChar;
function pStrCat(dst: PChar; const s: string): PChar;
function cStrCat(dst: PChar; const s: string): PChar;
function iStrCat(s: PChar; i: Integer; last: Boolean): PChar;
function ILStrCat(s: PChar; i,len: Integer; last: Boolean): PChar;
function fStrCat(s: PChar; f: double; m: Integer; last: Boolean): PChar;
function xStrCat(s: PChar; f: double; l,m: Integer; last: Boolean): PChar;
function xyStrCat(s: PChar; x,y: Integer; last: Boolean): PChar;
function polyStrCat(s: PChar; lp: PLLine; len: Integer): PChar;

function pStrLine(dst: PChar;
                  const str: String;
                  len: Integer): PChar;

function pStrInt(dst: PChar;
                 val,len: Integer): PChar;

function pStrInt64(dst: PChar;
                   val: Int64; len: Integer): PChar;

function pStrReal(dst: PChar; val: Double;
                  len,m: Integer): PChar;

function pStrEnd(dst: PChar; len: Integer; const s: string): PChar;
function IStrEnd(dst: PChar; len: Integer; v: Integer): PChar;
function fStrEnd(dst: PChar; len: Integer; v: double; m: Integer): PChar;
function eStrEnd(dst: PChar; len: Integer; v: double; m: Integer): PChar;

procedure StoA(var fa; Len: Integer; S: PChar);

function ItoA(V,Len: Integer; var fa): string;
procedure ItoC(V,Len: Integer; var fa);

function StoI(const fa; Len: Integer; out V: Integer): Boolean;

function AtoI(const fa; Len: Integer): Integer;
function AtoR(const fa; Len: Integer): Double;

function PageToStr(i,j,len: Integer): string;
function iLongStr(d: longint): string;
function DegreeStr(a: double): string;

function TimeStr(t: longint): string;
function xTimeStr(dt: Double): string;
function mTimeStr(dt: Double): string;
function xTimeStr1(dt: Double): string;
function xTimeStr2(dt: Double; m: int): string;

function xTimeName(dt: Double): string;
function iDateStr(year,month,day: int): String;
function sDateStr1(year,month,day: int): String;
function xDateStr(dt: Double): string;
function xDateName(dt: Double): string;
function xDateStr1(dt: Double): string;
function msTimeStr(dt: Double): string;
function DelayStr(t: longint): string;
function BusStr(t: longint): string;

function DateStr(d: longint): string;
function DateStr1(dt: Double): string;

function DateShort(d: longint): string;
function DateMonth(d: longint): string;
function MonthStr(d: longint): string;
function DayRuss(d: longint): string;
function DayStr(d: Integer): string;

function TimeShort(T: Integer): string;

function AviStr(t,fmt: int): string;
function MSecStr(ms,fmt: longint): string;

function GameStr(t: longint): string;
function hGameStr(t: Double): string;
function xGameStr(t: Double): string;
function iGameStr(t: Double): string;

function mkTimeStr(t: Double; len: Integer): string;

function xBusStr(Dt: TDatetime): String;
function xGpsStr(Dt: TDatetime): String;

function FileDateStr(dat: longint): string;

function SVal(var a; len: Integer): string;
function IVal(var a; len: Integer): Integer;
function xIVal(var a; len: Integer): Integer;
function XVal(var a; len: Integer): Int64;
function RVal(var a; len: Integer): double;
function TVal(var fa): Integer;
function DVal(var fa; len: Integer): Integer;

function StrToDate(const s: string): Integer;
function StrToTime(const s: string): Integer;

function xUpCase(ch: Char): Char;

function xStrUpper(p: PChar): PChar;
function sStrComp(s1: PChar; const s2: string): Boolean;
function sRusComp(s1: PChar; const s2: string): Boolean;

function xStrComp(s1,s2: PChar): Boolean;
function xStrLIComp(s1,s2: PChar; len: int): int;

function IsTownShort(Str: PChar; Len: int): bool;
function xTownComp(s1,s2: PChar; len,mode: int): int;

function xStreetComp(s1,s2: PChar): Integer;

function xStrSkipToken(Str: PChar; Ch: Char): PChar;

function is_rus(p: PChar): Boolean;
function is_rus_pas(const s: String): Boolean;
function is_rus_wide(const s: WideString): Boolean;
function is_natf(const s: WideString): Boolean;

function is_pack_wide(s: PWideChar): Boolean;
function is_Ansi_wide(s: PWideChar): Boolean;
function is_dos_wide(Src: PWideChar; out Dst: String): Boolean;

function wide_to_dos(const Src: WideString;
                     out Dst: String): Boolean;

function WideToAnsi(dst: PChar; src: PWideChar; MaxLen: Cardinal): PChar;

function StrRus(s: PChar): PChar;
function StrWin(s: PChar): PChar;
function StrWin1(s: PChar): PChar;
function StrDos(s: PChar): PChar;

function WinChar(ch: Char): Char;

function StrMain(s: PChar): PChar;
function MainString(const s: string): string;

function WinString(s: string): string;
function DosString(s: string): string;
function UpString(s: string): string;

function DosToUTF8(const s: string): string;

function DosToUnicodep(const Src: String; Dest: PWideChar): PWideChar;

procedure WinAlfa(s: PChar; len: Integer);
procedure DosAlfa(s: PChar; len: Integer);

function DbfStr(Dest,Str: PChar; Len: Integer): PChar;

function R_to_S(r: double): Integer;

function RadToDeg1(rad: double; out g,m: Integer): Double;

function R_to_G(r: double; long: Integer;
                out g,m: Integer): Double;

function xRadian(degree: double): double;

function ToDegree(g: Integer; m,s: double): double;
function Radian(g: Integer; m,s: double): double;
function Sec_to_Rad(s: Integer): double;
function Rad_to_Sec(r: double): longint;
function Round_Angle(a: double; m: Integer): double;

function RealStr(r: double; l,m:Integer): string;
function xRealStr(r: double; l,m:Integer): string;

function Str_xy(x,y: double; m: Integer; d,s: PChar): PChar;

function xStrAngle(s: PChar; r: double; m: Integer; ch: Char): PChar;
function StrAngle(s: PChar; a: double; long: Integer): PChar;

function DocAngle(s: PChar; Delimiter: Char;
                  a: double; long: Integer): PChar;

function AngleStr(a: double; long: Integer): string;
function xAngleStr(a: double): string;
function AngleFormat(a: double; m: Integer): string;

function NorthStr(r: double; m: Integer): string;
function EastStr(r: double; m: Integer): string;

function xNorthStr(r: double; m: Integer): string;
function xEastStr(r: double; m: Integer): string;

function GeoidStr(b,l: Double; m,ne: Integer): String;
function EarthStr(f: Double; m: Integer; ns: PChar): String;

function StrTransit(s,ch: PChar): PChar;
function StrReplace(s,ch1,ch2: PChar): PChar;

function StrReplaces(s,s1,s2: PChar): PChar;
function RuToEn(s: PChar): PChar;

function StrToken(dst,src: PChar): PChar;
function lStrToken(dst,src: PChar): Boolean;
function StrWord(dst,src: PChar): PChar;

function FieldToken(dst,src: PChar; Delimiter: Char): string;
function StringToken(src: PChar): string;

function StrMsg(dst,src: PChar; ch: char): PChar;

function IntToken(s: PChar; out v: Integer): Boolean;

function StrInts(Str: PChar; v: PIntegers; N: Integer): PChar;
function IntTokens(s: PChar; v: PIntegers; N: Integer): Integer;

function DWordToken(s: PChar; out v: DWord): Boolean;
function Int64Token(s: PChar; out v: Int64): Boolean;
function RealToken(s: PChar; out v: double): Boolean;
function AngleToken(s: PChar; out r: double): Boolean;

function GeoidToken(s: PChar; ch: Char; out r: Double): Boolean;
function LatitudeToken(s: PChar; out r: Double): Boolean;
function LongitudeToken(s: PChar; out r: Double): Boolean;

function LatitudeToken1(s: PChar; out r: Double): Boolean;
function LongitudeToken1(s: PChar; out r: Double): Boolean;

function xLatitudeToken(s: PChar; out r: Double): Boolean;
function xLongitudeToken(s: PChar; out r: Double): Boolean;

function DegToken(s: PChar; out r: Double): Boolean;
function LatToken(Str: PChar; out r: Double): Boolean;
function LonToken(Str: PChar; out r: Double): Boolean;

function CodeToken(s: PChar; out code: Integer): Boolean;
function PointToken(s: PChar; out p: TPoint): Boolean;
function GaussToken(s: PChar; out g: tgauss): Boolean;
function rgbToken(s: PChar; out cl: tColorRef): Boolean;
function xyzToken(s: PChar; out v: txyz): Boolean;

function sTimeToken(s: PChar; out t: Double): Boolean;

function TimeToken(s: PChar; out dt: Double): Boolean;
function dTimeToken(s: PChar; out dt: Double): Boolean;

function DateToken(s: PChar; fmt: Integer;
                   out dt: Double): Boolean;

function x_RealToken(s: PChar; out v: double;
                     v_min,v_max: Double): Boolean;

function StrInt(s: PChar): Integer;

function xStrToInt(S: PChar; out V: Integer): Boolean;
function StrToReal(S: PChar; out V: Double): Boolean;
function StrToFloat(S: PChar; out v: Double): Boolean;

function StrToValue(S: PChar; out V: Double): Integer;
function StrToValueG(S,NS: PChar; out V: Double): Integer;
function StrToPoint(Str: PChar; LB,RB: Char; out P: TGauss): Integer;

function StrInside(Dst,Str: PChar; LB,RB: Char): PChar;

function StrString(dst,src: PChar): PChar;

function xStrToken(src,cmd: PChar): Boolean;

function StrDeleteChar(s,ch: PChar): PChar;
function StrClearChar(s,ch: PChar): PChar;

function xIntToken(s: PChar): longint;
function xRealToken(s: PChar): double;

function xCmdToken(Str,Cmd: PChar): Boolean;

function dec_Round(d: double): longint;

function ReadPool(buf: pBytes; pos,size: Integer; var s: string): Integer;

function Current_century: Integer;

function Get_Month(date: Integer): Integer;
function Get_WeekDay(date: Integer): Integer;
function PackDate(day,month,year: Integer): Integer;
function Next_Date(date,days: Integer): Integer;
function Date_Dist(d1,d2: Integer): Integer;

function MonthInt(dt: TDateTime): Integer;

function PackTime(h,m,s: Integer): Integer;

procedure Unpack_time(t: Integer; out hh,mm,ss: Integer);
procedure Unpack_date(dt: Integer; out yy,mm,dd: Integer);

function xEncodeTime(t: Integer): TDateTime;
function xEncodeDate(dt: Integer): TDateTime;

function xDecodeTime(dt: Double): Longint;
function xDecodeDate(dt: Double): Longint;

function xFormatDateTime(Dt: TDatetime): String;
function xStrToDate(s: PChar): TDateTime;

function _UpperDos(s: PChar): PChar;
function _UpperRus(s: PChar): PChar;

function Str_Compare(s1,s2,str: PChar; Mode: Integer): Boolean;

function Str_Words_Comp(s1,s2: PChar): Boolean;

function CodeToStr(code: Integer): string;

function xCodeToStr(code,loc: Integer): string;
function CodeToChar(code: Integer): char;

function StrToCode(const s: string): longint;
function StrToCode1(const s: string): longint;

function div_Code(code: longint): longint;
function is_Code(const s: string): Boolean;

function StrCls(s: PChar): PChar;

function LStr(s: PChar): PChar;
function RStr(s: PChar): PChar;

function xRStr(s: PChar; ch: Char): PChar;

function ClsString(const s: string): string;
function AddString(s,add,comma: string): string;

procedure ClsWideString(var w: WideString);

function StrTrunc(Str: PChar; Chr: Char): PChar;

function Trunc_comment(skip,s: PChar): PChar;
function xTrunc_comment(skip,s: PChar): PChar;
function pTrunc_comment(skip,s: PChar): PChar;

function CmdToken(s: PChar; out nn: Integer): string;

procedure DosToAlfa(var a: alfa; const s: string);

function WideStrLen(s: PWideChar): Integer;
function WideStrEnd(s: PWideChar): PWideChar;

function WideStrComp(s1,s2: PWideChar): Integer;
function WideStrLCopy(dst,src: PWideChar; MaxLen: Cardinal): PWideChar;

function WideStrCopy(dst,src: PWideChar): PWideChar;

function WideStrAnsi(dst,src: PWideChar; MaxLen: Cardinal): PWideChar;
function WideStrPack(dst,src: PWideChar; MaxLen: Cardinal): PWideChar;

function WideStrToStr(dst: PChar; src: PWideChar; MaxLen: Cardinal): PChar;

function xStrToWideChar(src: PChar; dst: PWideChar): PWideChar;
function xStringToWideChar(const src: string; dst: PWideChar): PWideChar;

function xWideCharToString(s: PWideChar): string;
function xWideCharToString1(s: PWideChar; MaxLen: int): WideString;
function xWideStrToString(const s: WideString): String;

function StrHex(Dst: PChar; Buf: PBytes; Len: Integer): PChar;
function StrItem(It,Str: PChar; Index: Integer): PChar;

function HexString(Buf: PBytes; Len: Integer): WideString;

function StrToHex(Str: PChar; out v: DWord): Boolean;
function HexValue(Str: PChar): DWord;

function HexPack(const S: WideString;
                 Buf: PBytes; Size: Integer): Integer;

function HexPoly(const S: WideString;
                 lp: PLLine; lp_Max: Integer): Integer;

function HexStrPack(Str: PChar; Dst: PBytes;
                    Size: Integer): PChar;

function StrToArray(Str,Mask: PChar;
                    V: PIntegers; Max: Integer): Integer;

function StrToCrcs(Crc: PChar): Int64;

function PackIP(v1,v2,v3,v4: DWord): DWord;
procedure UnpackIP(ip: DWord; out v1,v2,v3,v4: DWord);
function IPStr(ip: DWord): String;

implementation //{$R xlang.res}

uses
  SysUtils,
  wstrings;

function StrResA(Str: PChar; Id: uint): int;
begin
//  Result:=LoadStringA(hInstance,Id,Str,255)
end;

function SetFlag(flags,fl: Integer; up: Boolean): Integer;
begin
  if up then Result:=flags or fl
  else Result:=flags and (fl xor (-1))
end;

function SwapFlag(flags,fl: Integer): Integer;
begin
  if flags and fl = 0 then
    Result:=flags or fl
  else
    Result:=flags and (fl xor (-1))
end;

procedure set_Angle_fmt(fmt: Integer);
begin
  if fmt in [0..2] then Angle_fmt:=fmt
end;

function IsDigit(Ch: Char): Boolean;
begin
  Result:=Ch in ['0'..'9']
end;

function xInc(v,dv: Integer): Integer;
var
  t: Integer;
begin
  t:=v; Inc(v,dv);
  if (dv > 0) and (v < t) then
    v:=MaxInt
  else
  if (dv < 0) and (v > t) then
    v:=-MaxInt;

  Result:=v
end;

function Decimals(i: Integer): Integer;
begin
  Result:=1; i:=Abs(i); while i > 9 do
  begin i:=i div 10; Inc(Result) end
end;

function MaxDecimal(i: Integer): Integer;
begin
  Result:=9; while i > 1 do begin
    Result:=Result*10+9; Dec(i)
  end
end;

function RoundDecimal(i: Integer): Integer;
begin
  Result:=1; while i > Result do
  Result:=Result*10;
end;

function MaxDigits(i: Integer): Integer;
begin
  Result:=1; while i >= 10 do begin
    Inc(Result); i:=i div 10
  end
end;

function this_x2(i: Integer): Boolean;
var
  acc: Integer;
begin
  acc:=0;
  while i > 0 do begin
    if Odd(i) then Inc(acc);
    i:=i shr 1
  end;

  Result:=acc = 1
end;

function degree_x2(x: Integer): Integer;
begin
  Result:=0;
  while x >= 2 do begin
    Inc(Result); x:=x div 2
  end
end;

function power_x2(n: Integer): Integer;
begin
  Result:=1;
  while n > 0 do begin
    Inc(Result,Result); Dec(n)
  end
end;

function int_x2(i: Integer): Integer;
var
  dist,next,tmp: Integer;
begin
  Result:=1; dist:=Abs(Result-i);
  while true do begin
    next:=Result*2;
    tmp:=Abs(next-i);

    if tmp < dist then begin
      Result:=next; dist:=tmp
    end else Break
  end
end;

function min_x2(v: Integer): Integer;
var
  ax: Integer;
begin
  Result:=1; ax:=1;
  while true do begin
    ax:=ax + ax;
    if ax > v then Break;
    Result:=ax;
  end
end;

function max_x2(v: Integer): Integer;
begin
  Result:=1;
  while Result < v do
  Inc(Result,Result)
end;

function degree_Round(F: Double): Integer;
begin
  Result:=Round(F);
  while Result >= 360 do Dec(Result,360);
  while Result <= -360 do Inc(Result,360);
end;

function GarminRadian(v: Double): Double;
var
  gr: Integer; min: Double;
begin
  gr:=Trunc(v / 100); min:=v - gr*100;
  Result:=(gr + min/60) * Pi / 180;
end;

function TruncMin(v: Double): Double;
begin
  Result:=Trunc(v);
  if Result > v then
  Result:=Result - 1
end;

function sec_Time(T: TDateTime): Integer;
var
  Hour,Min,Sec,MSec: Word;
begin
  DecodeTime(T, Hour,Min,Sec,MSec); Result:=Hour;
  Result:=((Result * 60) + Min) * 60 + Sec
end;

function Align_min(t: Integer): Integer;
begin
  Result:=int_Round(t,60)
end;

function val_Align(var v: Integer; dv,per: Integer): Boolean;
var
  k,eps: Double;
begin
  Result:=false;

  if dv > 0 then

  if per = 0 then begin
    v:=int_Align(v,dv); Result:=true
  end
  else begin
    k:=(Abs(v) mod dv)/dv; eps:=1/per;

    if (k < eps) or ((1-k) < eps) then begin
      v:=int_Align(v,dv); Result:=true
    end
  end
end;

function int_Align(i,k: Integer): Integer;
begin
  Result:=Abs(i) div k;
  if Abs(i) mod k >= k div 2 then
  Inc(Result); Result:=Result * k;
  if i < 0 then Result:=-Result else
  if i = 0 then Result:=0 else
  if k = 1 then Result:=i
end;

function int_Round(i,k: Integer): Integer;
begin
  if i < 0 then
    Result:=-int_Trunc(-i,k)
  else
    Result:=((i+k-1) div k) * k
end;

function int_Trunc(i,k: Integer): Integer;
begin
  if i < 0 then
    Result:=-int_Round(-i,k)
  else
    Result:=(i div k) * k
end;

function int_Even(i: Integer): Integer;
begin
  Result:=i; if Odd(i) then Dec(Result)
end;

function int_Tiles(i,j: Integer): Integer;
begin
  Result:=(i+j-1) div j
end;

function int_Size(var s: Integer; k: Float): Integer;
var
  bs,i: Integer;
begin
  bs:=s;
  for i:=1 to 100 do begin
    Result:=Round(s*k);
    s:=Round(Result / k);
    if s >= bs then Break;
    s:=bs+i
  end;
end;

function IsGeoid(b,l: Double): bool;
begin
  Result:=(Round(b*ms_scale) <> 0) or
          (Round(l*ms_scale) <> 0)
end;

function Double_Correct(r: double): Boolean;
var
  rc: Integer; s: string;
begin
  str(r,s); val(s,r,rc);
  Result:=rc = 0
end;

function Short_Correct(r: float): Boolean;
var
  rc: Integer; s: string;
begin
  str(r,s); val(s,r,rc);
  Result:=rc = 0
end;

function Short_Assign(r: float): float;
begin
  Result:=0;
  if Short_Correct(r) then
  Result:=r
end;

function mRound(x: Double; m: Integer): Double;
const
  Large = 1000000;
var
  i,d: Integer;
begin
  Result:=x;

  if m > 0 then begin
    d:=1; for i:=1 to m do begin
      d:=d * 10; x:=x * 10;
      if x > Large then Break
    end;

    Result:=Round(x)/d
  end
end;

function sRound(x: Double; m: Integer): Double;
var
  i,d: Integer; s: Double;
begin
  Result:=x;

  if m > 0 then

  if m = 1 then
    Result:=mRound(x,1)
  else begin
    x:=Abs(x); s:=Frac(x); x:=Trunc(x);

    d:=1; for i:=1 to m do begin
      d:=d * 10; s:=s * 10;
    end;

    if d-s <= 1.001 then s:=s+1;

    x:=x + s/d;
    if Result < 0 then x:=-x;
    Result:=x
  end
end;

function m_Real(d: double; max: Integer): Integer;
begin
  Result:=0; d:=d-Trunc(d);

  while (d >= 0.1) and (d < 0.999) and (Result < max) do
  begin Inc(Result); d:=d*10; d:=d-Trunc(d) end
end;

function bin_Real(d: double; m: Integer): double;
var
  rc: Integer; s: string; 
begin
  Result:=0;
  str(d:0:m,s); Val(s,d,rc);
  if rc = 0 then Result:=d
end;

function xRound(x: Double): Integer;
begin
  Result:=0;
  if Abs(x) < MaxInt then
  Result:=Round(x)
end;

function iCompare(i, i1,i2, cmd: int): Boolean;
begin
  Result:=false;

  case cmd of
0:  Result:=i > i1;
1:  Result:=i < i1;
2:  Result:=i = i1;
3:  Result:=(i >= i1) and (i <= i2);
4:  Result:=(i > i1) and (i < i2);
  end
end;

function fCompare(f, f1,f2: double; cmd: int): Boolean;
begin
  Result:=false;

  f:=bin_Real(f,8);

  f1:=bin_Real(f1,8);
  f2:=bin_Real(f2,8);

  case cmd of
0:  Result:=f > f1;
1:  Result:=f < f1;
2:  Result:=f = f1;
3:  Result:=(f >= f1) and (f <= f2);
4:  Result:=(f > f1) and (f < f2);
  end
end;

function Str_chk(S: PChar): Integer;
var
  ax,l: Integer; p: PChar; t: TShortstr;
begin
  Result:=0;
  if Assigned(S) then begin

    StrLCopy(t,S,255);
    xStrUpper(t); p:=t;

    l:=Strlen(t);
    while l >= 4 do begin
      Inc(Result,PLongint(p)^);
      p:=@p[4]; Dec(l,4)
    end;

    if l > 0 then begin
      ax:=0; Move(p[0],ax,l);
      Inc(Result,ax)
    end
  end
end;

function chk_Str(const S: String): Integer;
var
  t: TShortstr;
begin
  Result:=Str_chk(StrPCopy(t,S))
end;

function buf_chk(P: PBytes; Len: Integer): Integer;
var
  i: Integer;
begin
  Result:=0;
  for i:=0 to Len-1 do
  Inc(Result,P[i])
end;

function xString(s: String; len: Integer): string;
begin
  Result:=s;
  if Length(s) > len then
  SetLength(Result,len)
end;

function xStrPas(s: PChar): string;
begin
  Result:=''; if s <> nil then
  Result:=StrPas(s)
end;

function xStrLen(s: PChar): Integer;
begin
  Result:=0; if s <> nil then
  Result:=StrLen(s)
end;

function StrLPas(s: PChar; MaxLen: Integer): string;
var
  t: TShortStr;
begin
  Result:=''; if s <> nil then
  Result:=StrPas(StrLCopy(t,s,MaxLen))
end;

function xStrPCopy(dst: PChar; src: String): PChar;
begin
  Result:=nil;
  if Assigned(dst) then
  if StrPLCopy(dst,src,255) <> nil then
  if StrLen(dst) > 0 then Result:=dst
end;

function xStrCopy(dst,src: PChar): PChar;
begin
  Result:=nil; if Assigned(dst) then
  if Assigned(src) then Result:=StrLCopy(dst,src,255)
  else StrCopy(dst,'')
end;

function StrQuery(Msg: PChar; const Capt: String): PChar;
var
  p: PChar;
begin
  Result:=StrPCopy(msg,Capt);

  p:=StrScan(Msg,'&');
  if Assigned(p) then StrCopy(p,@p[1]);

  p:=StrScan(Msg,'.');
  if Assigned(p) then p[0]:=#0;

  StrCat(Msg,'?');
end;

function xStrLCat(dst,src: PChar; len: Integer): PChar;
begin
  Result:=StrLCat(dst,src,StrLen(dst)+len)
end;

function StrRecode(s: PChar; ch1,ch2: Char): PChar;
var
  i: Integer; p: PChar; ch: Char;
begin
  Result:=s; p:=s;
  for i:=1 to 255 do begin
    ch:=p[0]; if ch = #0 then Break;
    if ch = ch1 then p[0]:=ch2;
    p:=@p[1]
  end;
end;

function StrPrefix(Prefix,Name: PChar): PChar;
var
  p1,p2: PChar;
begin
  Result:=Prefix;

  p1:=Prefix; p2:=Name;

  while p1[0] <> #0 do begin
    if p1[0] <> p2[0] then begin
      p1[0]:=#0; Break
    end;

    p1:=@p1[1]; p2:=@p2[1];
  end
end;

function StrPhoto(Name: PChar; out row,col: Integer): Boolean;
var
  p,p1,p2: PChar; l1,l2,rc: Integer; S: TShortstr;
begin
  Result:=false; col:=0; row:=0;

  p:=StrLCopy(S,Name,255);

  while p[0] <> #0 do
  if p[0] in ['0'..'9'] then Break
  else p:=@p[1];

  p1:=StrScan(p,'_'); p2:=StrScan(p,'.');
  if p2 = nil then p2:=StrEnd(p);

  if Assigned(p1) then
  if Assigned(p2) then
  if p1 < p2 then begin
    p1[0]:=#0;
    if Strlen(p) > 0 then begin

      val(p,row,rc); l1:=Strlen(p);

      if (rc = 0) and (row >= 0) then begin
        p2[0]:=#0; StrCopy(p,@p1[1]);
        if Strlen(p) > 0 then begin

          val(p,col,rc); l2:=Strlen(p);

          if rc = 0 then begin

            if l1 <= 2 then
            if l1 < l2 then iSwap(col,row);

            Result:=col > 0
          end
        end
      end
    end
  end
end;

function xStrScan(str,chars: PChar): PChar;
var
  i,len: Integer; p: PChar;
begin
  Result:=nil; len:=Strlen(chars);

  if Assigned(str) then
  for i:=0 to len-1 do begin
    p:=StrScan(str,chars[i]);

    if p <> nil then
    if (Result = nil) or (p < Result) then
    Result:=p
  end
end;

function xStrSkip(str,chars: PChar): PChar;
begin
  Result:=str;
  while Result[0] <> #0 do begin
    if StrScan(chars,Result[0]) = nil then Break;
    Result:=@Result[1]
  end
end;

function xTokenLen(p,q: PChar): Integer;
begin
  Result:=xStrlen(p);
  if (Result > 0) and Assigned(q) then
  Result:=q-p 
end;

function StrAnsi(s: PChar): PChar;
var
  len: Integer;
begin
  len:=byte(s[0]);
  if len = 0 then Result:=StrCopy(s,'')
  else Result:=StrLCopy(s,@s[1],len)
end;

function xStrName(dst,src: PChar; Ind: Integer): PChar;
var
  p,q: PChar; len: Integer;
begin
  p:=src;

  while Ind > 0 do begin
    q:=xStrScan(p,'\;');
    if q = nil then Break;
    if Strlen(@q[1]) = 0 then Break;
    p:=@q[1]; Dec(ind)
  end;

  q:=xStrScan(p,'\;');
  len:=xTokenLen(p,q);
  Result:=StrLCopy(dst,p,len);
end;

function str_icomp(s: PChar; v: Integer): Boolean;
var
  ax,rc: Integer;
begin
  Result:=false; val(s,ax,rc);
  if rc = 0 then Result:=ax = v
end;

function StrToNnn(const s: String): Integer;
var
  i: Integer; ch: Char;
begin
  Result:=0;

  for i:=1 to Length(s) do begin
    ch:=s[i]; if (ch < '0')
    or (ch > '9') then Break;
    Result:=Result*10 + ord(ch) - ord('0')
  end
end;

function StrToInt(const s: String): Integer;
var
  rc: Integer;
begin
  val(s,Result,rc);
  if rc <> 0 then Result:=0
end;

function StrToInt64(const s: String): Int64;
var
  rc: Integer;
begin
  val(s,Result,rc);
  if rc <> 0 then Result:=0
end;

function xHexToStr(v: Integer): string;
begin
  Result:='';
  repeat
    Result:=cHex[v and $F]+Result;
    v:=v shr 4;
  until v = 0
end;

function HexToStr(v: int64; len: Integer): string;
begin
  Result:=''; while len > 0 do begin
    Result:=cHex[v and $F]+Result;
    v:=v shr 4; Dec(len)
  end
end;

function DecToStr(v: int64; len: Integer): string;
var
  t: int64;
begin
  Result:=''; t:=Abs(v);

  while len > 0 do begin
    Result:=cHex[t and $7]+Result;
    t:=t shr 3; Dec(len)
  end;

  if v < 0 then Result:='-'+Result
end;

function StateToStr(state: uint): String;
var
  s: String;
begin
  s:='';
  while state <> 0 do begin
    if state and 1 <> 0 then s:='1'+s
                        else s:='0'+s;
    state:=state shr 1;
  end;

  Result:=s
end;

function BitToStr(v: int64; len: Integer): string;
var
  ch: char; p: Integer;
begin
  Result:='';

  while len > 0 do begin
    ch:='0'; if Odd(v) then ch:='1';
    Result:=ch + Result; Dec(len);
    v:=v shr 1
  end;

  p:=Pos('1',Result); if p > 1 then
  Delete(Result,1,p-1)
end;

function FlagsToStr(flags,bit: Integer): string;
begin
  if flags and bit <> 0 then Result:='1'
  else Result:='0'
end;

function KByteStr(Bytes: int64): String;
var
  v: double; s1,s2: String;
begin
  s2:='bytes';
  if Bytes < 1024 then
    s1:=IntToStr(Bytes)
  else begin
    v:=Bytes/1024; s2:='Kb';
    if v >= 1024 then begin
      v:=v/1024; s2:='Mb';
      if v >= 1024 then begin
        v:=v/1024; s2:='Gb';
      end
    end;

    s1:=RealToStr(v,1)
  end;

  Result:=s1+s2
end;

function LongStr3(v: Longint): String;
var
  s: String;
begin
  s:=''; v:=Abs(v);

  while v >= 1000 do begin
    if length(s) > 0 then s:=' '+s;
    s:=xIntToStr(v mod 1000,3,'0')+s;
    v:=v div 1000
  end;

  if length(s) > 0 then
    s:=IntToStr(v)+' '+s
  else
    s:=IntToStr(v);

  Result:=s
end;

function LargeToStr(v: Int64): string;
var
  i,k,l: Integer; s1,s2: String;
begin
  s1:=IntToStr(v); s2:='';

  while true do begin
    l:=length(s1)+1; if l <= 1 then Break;
    i:=l-3; if i <= 1 then i:=1;
    if length(s2) > 0 then s2:=' '+s2;
    k:=l-i; s2:=Copy(s1,i,k)+s2;
    Delete(s1,i,k);
  end;

  Result:=s2
end;

function sIntToStr(v: Integer): string;
var
  s: String;
begin
  s:=IntToStr(v);
  if v > 0 then s:='+'+s;
  Result:=s
end;

function xIntToStr(v,len: Integer; ch: char): string;
begin
  Result:=IntToStr(v);

  if len > 0 then
  if length(Result) > len then
  Result[0]:=chr(len);

  while length(Result) < Abs(len) do
  Result:=ch+Result
end;

function xIdToStr(Id: Cardinal; len: Integer; ch: char): string;
begin
  Result:=IntToStr(Id);
  while length(Result) < Abs(len) do
  Result:=ch+Result
end;

function EpsToStr(r: double; M: Integer): string;
var
  ax: Double;
begin
  ax:=Abs(r);
  while ax >= 10 do begin
    if M < 0 then Break;
    ax:=ax / 10; Dec(M)
  end;

  if M  = 0 then M:=-1;
  Result:=RealToStr(r,M)
end;

function AxeToStr(r: double; M: Integer): string;
var
  s: String; p,l: Integer;
begin
  s:=EpsToStr(r,M);
  p:=Pos('.',s); if p > 1 then begin
    l:=Length(s); while l > p do
    if s[l] = '0' then Dec(l) else Break;
    if l = p then SetLength(s,p-1);
  end; Result:=s
end;

function RealToStr(r: double; M: Integer): string;
var
  i,p: Integer; ax: Int64;
  sign: Boolean; ch: Char; s: String;
begin
  if M < 0 then
    Result:=IntToStr( Round(r) )
  else begin
    sign:=r < 0; r:=Abs(r);
    if M = 0 then M:=10;
    str(r:0:M,s); s:=Trim(s);

    p:=System.Pos('.',s)+1;
    if p > 1 then begin

      i:=length(s);
      while i > p do begin
        if s[i] <> '0' then
        begin p:=i; Break end;
        Dec(i);
      end;

      s[0]:=chr(p)
    end;

    ax:=0;
    for i:=1 to length(s) do begin
      ch:=s[i]; if ch in ['0'..'9'] then
      ax:=ax * 10 + ord(ch) - ord('0')
    end;

    if ax = 0 then Result:='0' else
    if sign then Result:='-'+s else
    Result:=s
  end
end;

function StrToLStr(s: PChar; L: Integer): string;
begin
  Result:=StrLPas(s,L);
  while Length(Result) < L do
  Result:=Result+' ';
  Result:=Result+' '
end;

function IntToLStr(i: Integer; L: Integer): string;
begin
  Result:=IntToStr(i);
  while Length(Result) < L do
  Result:=Result+' ';
  Result:=Result+' '
end;

function RealToLStr(r: Double; L,M: Integer): string;
begin
  Result:=RealToStr(r,M);
  while Length(Result) < L do
  Result:=Result+' ';
  Result:=Result+' '
end;

function RealToStrz(r: Double; m: int): string;
var
  p,l: int; s: String;
begin
  s:=RealToStr(r,m);
  p:=Pos('.',s);
  if p = 0 then begin
    s:=s+'.'; p:=Pos('.',s);
  end;

  if p > 0 then begin
    l:=Length(s);
    while p+m > l do begin
      s:=s+'0'; Inc(l);
    end
  end;

  Result:=s
end;

function FloatToLStr(r: double; E,L,M: Integer): string;
var
  i: Integer;
begin
  for i:=1 to E do r:=r*10;
  Result:=RealToStr(r,M)+'E-'+IntToStr(E);
  while Length(Result) < L do
  Result:=Result+' ';
  Result:=Result+' '
end;

function DistToStr(r: double): string;
var
  m: Integer;
begin
  m:=3;
  if r >= 1000 then m:=-1 else
  if r >= 100 then m:=1 else
  if r >= 10 then m:=2;
  
  Result:=RealToStr(r,m)
end;

function ValToStr(r: double; L: Integer): string;
begin
  str(r:0:L-2,Result);          
  if Length(Result) > L then begin

    if Pos('.',Result) < L then
    Result[0]:=Chr(L)

  end else

  while Length(Result) < L do
  Result:=Result+' '
end;

function mRealToStr(r: double; M: Integer): string;
var
  p: Integer;
begin
  Result:=RealToStr(r,M);
  while M > 0 do begin
    p:=Pos('.',Result);
    if p = 0 then Result:=Result + '.' else
    if p <= Length(Result) - M then Break;
    Result:=Result + '0'
  end
end;

function xRealToStr(r: double; M: Integer): string;
var
  ax, i,k, i1,i2,rM, v: Integer;
  f1,f2: double; sign: Boolean;
  s: String;
begin
  Result:='';

  if Double_Correct(r) then begin

    sign:=r < 0; r:=Abs(r); rM:=M;
    k:=M; M:=0; if k = 0 then k:=8;

    if r > 200000000 then begin
      str(r:0:3,Result); i:=Pos('.',Result);
      if i > 0 then Result[0]:=chr(i-1)
    end
    else begin
      f1:=1; f2:=Frac(r); i1:=0; i2:=0;

      for i:=1 to k do begin
        f1:=f1*10; f2:=f2*10;

        ax:=Trunc(f2) mod 10; M:=i;

        if ax = 0 then begin
          i2:=0; Inc(i1);
          if i1 >= 3 then
          if i1 >= rM then Break
        end else
        if ax = 9 then begin
          i1:=0; Inc(i2);
          if rM = 0 then
          if i2 = 3 then Break;
        end
        else begin
          i1:=0; i2:=0
        end
      end;

      v:=Trunc(r); f2:=Round(f2);
      if f2 >= f1 then begin
        Inc(v); f2:=f2-f1; M:=0
      end;

      Result:=IntToStr(v);

      if M > 0 then begin
        s:=IntToStr(Trunc(f2));
        while length(s) < M do s:='0'+s;

        i:=Length(s);
        while (i > 0) and (s[i] = '0') do Dec(i);
        s[0]:=chr(i);

        if length(s) > 0 then
        Result:=Result+'.'+s
      end
    end;

    Result:=Trim(Result);
    if sign then Result:='-'+Result;
  end
end;

function FloatToStr(v: double; M: Integer): String;
var
  r: Double; e: Integer;
begin
  r:=v; e:=0;

  if Abs(r) > 1E-32 then begin
    while Abs(r) < 1 do begin
      r:=r*10; Dec(e)
    end;

    while Abs(r) >= 10 do begin
      r:=r/10; Inc(e)
    end;
  end;

  Result:=RealToStr(r,m)+'E'+IntToStr(e)
end;

function ValueToStr(v: double; M: Integer): String;
begin
  if Abs(v) < 1E-32 then
    Result:='0'
  else
  if Abs(v) < 0.01 then
    Result:=FloatToStr(v,M)
  else
    Result:=RealToStr(v,M)
end;

function TruncRStr(S: String): String;
var
  l,p: Integer;
begin
  Result:=S;

  if Length(S) > 0 then begin
    l:=Length(S); p:=Pos('.',S);

    if p > 0 then
    if l > 7 then
    if l-p > 3 then begin

      l:=Min(l,Max(8,p+3));
      Result:=Copy(S,1,l)
    end
  end
end;

function LPointStr(const p: TPoint): string;
begin
  Result:=IntToStr(p.X)+','+IntToStr(p.Y)
end;

function xSizeStr(w,h: Integer): string;
begin
  Result:=IntToStr(w)+'x'+IntToStr(h)
end;

function LSizeStr(w,h: Integer): string;
begin
  Result:='['+xSizeStr(w,h)+']'
end;

function GaussStr(const g: tgauss; m: Integer): string;
begin
  Result:=RealToStr(g.x,m)+':'+RealToStr(g.y,m)
end;

function PosToStr(v: double; m: int): String;
var
  t: double; ax: Int64; i,k1,k2: Int;
  s: String; c: TShortstr;
begin
  t:=Abs(v); ax:=Trunc(t); t:=Frac(t);

  k1:=0; k2:=0;
  while ax > 0 do begin
    c[k1]:=Char(ord('0') + (ax mod 10));
    ax:=ax div 10;

    Inc(k1); Inc(k2); if k2 = 3 then begin
      c[k1]:=' '; Inc(k1); k2:=0
    end
  end;

  s:='';
  for i:=k1-1 downto 0 do s:=s+c[i];

  if m > 0 then begin
    for i:=1 to m do t:=t*10;
    i:=Round(t); if i > 0 then
    s:=s+'.'+IntToStr(i)
  end;

  if Length(s) = 0 then s:='0';
  Result:=s
end;

function CoordStr(const g: tgauss; m: Integer): string;
begin
  Result:=PosToStr(g.x,m)+':'+PosToStr(g.y,m)
end;

function xyStr(const g: tgauss; m: Integer): String;
begin
  Result:=RealToStr(g.x,m)+' '+RealToStr(g.y,m)
end;

function xyzStr(const v: txyz; m: Integer): String;
begin
  Result:=RealToStr(v.x,m)+' '+
          RealToStr(v.y,m)+' '+
          RealToStr(v.z,m)
end;

function MatrixStr(Nx,Ny: Integer): String;
begin
  Result:=Format('[%dx%d]',[Nx,Ny])
end;

procedure IntToChar(v: Integer; var buf; len: Integer);
var
  s: string;
begin
  s:=IntToStr(v); if length(s) > len then
  s[0]:=chr(len); Move(s[1],buf,length(s))
end;

procedure xIntToChar(v: Integer; var buf; len: Integer);
var
  s: string;
begin
  s:=IntToStr(v);
  if length(s) > len then s[0]:=chr(len);
  while length(s) < len do s:=' '+s;
  Move(s[1],buf,length(s))
end;

procedure xRealToChar(v: Double; m: Integer; var buf; len: Integer);
var
  s: string; i: Integer;
begin
  str(v:0:m,s);

  if System.Pos('.',s) > 0 then begin
    i:=length(s);
    while (i > 1) and (s[i] = '0') do Dec(i);
    if (i < length(s)) and (s[i] = '.') then Inc(i);
    s[0]:=chr(i)
  end;

  if length(s) > len then s[0]:=chr(len);
  while length(s) < len do s:=' '+s;
  Move(s[1],buf,length(s))
end;

function StrToChar(s: PChar; var buf; len: Integer): Integer;
begin
  Result:=0;
  if s <> nil then begin
    Result:=StrLen(s); if Result > 0 then begin
      if Result > len then Result:=len;
      Move(s^,buf,Result)
    end
  end
end;

function xStrToChar(const s: string; var buf; len: Integer): Integer;
begin
  Result:=length(s);

  if Result > 0 then begin
    Result:=Min(Result,len);
    if Result < len then
    FillChar(buf,len,' ');
    Move(s[1],buf,Result)
  end
end;

function sStrToChar(const s: string; var buf; len: Integer): Integer;
begin
  Result:=length(s);

  if Result > 0 then begin
    Result:=Min(Result,len);
    if Result < len then
    FillChar(buf,len,#0);
    Move(s[1],buf,Result)
  end
end;

procedure xAngleToChar(f: Double; var buf; len: Integer);
begin
  xStrToChar(AngleStr(f,4),buf,len)
end;

function StrWide(dst: PChar;
                 const src: WideString;
                 MaxLen: Integer): PChar;
begin
  Result:=nil;

  if dst <> nil then

  if length(src) = 0 then
    StrCopy(dst,'')
  else begin
    if MaxLen = 0 then MaxLen:=255;
    if MaxLen > 255 then MaxLen:=255;

    Result:=StrPLCopy(dst,src,MaxLen)
  end
end;

function PStrToWideChar(const Src: String; Dest: PWideChar;
                        DestSize: Integer): PWideChar;
var
  s: TShortstr;
begin
  Result:=StrToWideChar(StrPCopy(s,Src), Dest,DestSize)
end;

function StrToWideChar(Src: PChar; Dest: PWideChar;
                       DestSize: Integer): PWideChar;
begin
  Result:=StrPCopyW(Dest,StrPasAW(Src),DestSize)
end;

function sCatStr(s1,s2,cs: String): String;
begin
  Result:=s1;
  if Length(s2) > 0 then

  if Length(s1) = 0 then
    Result:=s2
  else
  if length(cs) = 0 then
    Result:=s1+' '+s2
  else
    Result:=s1+cs+s2
end;

function sStrCat(dst,src: PChar): PChar;
begin
  Result:=StrCat(StrCat(StrCat(dst,'"'),src),'"')
end;

function sTxtCat(dst,src: PChar): PChar;
begin
  Result:=nil;
  if Strlen(src) > 0 then begin
    if Strlen(dst) > 0 then StrCat(dst,' ');
    Result:=StrCat(dst,src)
  end
end;

function pStrCat(dst: PChar; const s: string): PChar;
var
  t: TShortStr;
begin
  Result:=StrLCat(dst,StrPCopy(t,s),255)
end;

function cStrCat(dst: PChar; const s: string): PChar;
begin
  if dst[0] <> #0 then StrLCat(dst,', ',255);
  Result:=pStrCat(dst,s)
end;

function IStrCat(s: PChar; i: Integer; last: Boolean): PChar;
var
  t: TShortStr;
begin
  Result:=StrCat(s,StrPCopy(t,IntToStr(i)));
  if not last then Result:=StrCat(s,' ')
end;

function ILStrCat(s: PChar; i,len: Integer; last: Boolean): PChar;
var
  t: TShortStr;
begin
  StrPCopy(t, xIntToStr(i,len,'0'));
  Result:=StrCat(s,t);
  if not last then Result:=StrCat(s,' ')
end;

function fStrCat(s: PChar; f: double; m: Integer; last: Boolean): PChar;
var
  l: Integer; t: TShortStr;
begin
  str(f:0:m,t); l:=Strlen(t);
  while (l > 1) and (t[l-1] = '0') do
  Dec(l); t[l]:=#0; Result:=StrCat(s,t);
  if not last then Result:=StrCat(s,' ')
end;

function xStrCat(s: PChar; f: double; l,m: Integer; last: Boolean): PChar;
var
  t: TShortStr;
begin
  str(f:l:m,t); Result:=StrCat(s,t);
  if not last then Result:=StrCat(s,' ')
end;

function xyStrCat(s: PChar; x,y: Integer; last: Boolean): PChar;
begin
  IStrCat(s,x,false); Result:=IStrCat(s,y,last)
end;

function polyStrCat(s: PChar; lp: PLLine; len: Integer): PChar;
var
  i: Integer;
begin
  Result:=s;

  if lp <> nil then
  if lp^.N >= 0 then begin
    iStrCat(s,lp^.N,false);

    with lp^ do for i:=0 to N do
    if StrLen(s)+64 < len then
    with Pol[i] do xyStrCat(s,x,y,false)
    else begin Result:=nil; Break end
  end
end;

function pStrLine(dst: PChar;
                  const str: String;
                  len: Integer): PChar;
var
  i: Integer;
begin
  Result:=pStrCat(dst,str);
  Dec(len,Length(str));
  for i:=1 to len do StrCat(dst,' ')
end;

function pStrInt(dst: PChar;
                 val,len: Integer): PChar;
begin
  Result:=pStrLine(dst,IntToStr(val),len)
end;

function pStrInt64(dst: PChar;
                   val: Int64; len: Integer): PChar;
begin
  Result:=pStrLine(dst,IntToStr(val),len)
end;

function pStrReal(dst: PChar; val: Double;
                  len,m: Integer): PChar;
begin
  Result:=pStrLine(dst,RealToStr(val,m),len)
end;

function pStrEnd(dst: PChar; len: Integer; const s: string): PChar;
begin
  while Strlen(dst)+Length(s) < len do StrCat(dst,' ');
  Result:=pStrCat(dst,s)
end;

function IStrEnd(dst: PChar; len: Integer; v: Integer): PChar;
begin
  Result:=pStrEnd(dst,len,IntToStr(v))
end;

function fStrEnd(dst: PChar; len: Integer; v: double; m: Integer): PChar;
begin
  Result:=pStrEnd(dst,len,RealToStr(v,m))
end;

function eStrEnd(dst: PChar; len: Integer; v: double; m: Integer): PChar;
begin
  Result:=pStrEnd(dst,len,EpsToStr(v,m))
end;

function StoI(const fa; Len: Integer; out V: Integer): Boolean;
var
  p: PChar;
begin
  Result:=false; V:=0; p:=@fa;

  while Len > 0 do begin
    if not (p[0] in ['0'..'9']) then
    begin V:=0; Result:=false; Break end;
    V:=V*10 + ord(p[0]) - ord('0');
    p:=@p[1]; Dec(Len); Result:=true
  end
end;

function AtoI(const fa; Len: Integer): Integer;
var
  a: array[0..31] of char absolute fa;
  i: Integer; sign: Boolean;
begin
  i:=0; Result:=0; sign:=false;

  if Len > 0 then begin
    while (i < Len) and (a[i] = ' ') do Inc(i);

    if a[i] = '+' then Inc(i) else
    if a[i] = '-' then begin Inc(i); sign:=true end;

    while i < Len do begin
      Result:=(Result*10)+ord(a[i])-ord('0'); Inc(i)
    end;

    if sign then Result:=-Result
  end
end;

function AtoR(const fa; Len: Integer): Double;
var
  a: array[0..31] of char absolute fa;
  i,d,m: Integer; sign: Boolean; ch: char;
begin
  Result:=0; i:=0; m:=0; sign:=false;

  while (i < Len) and (a[i] = ' ') do Inc(i);

  if a[i] = '+' then Inc(i) else
  if a[i] = '-' then begin
    Inc(i); sign:=true
  end;

  while i < Len do begin
    ch:=a[i];
    if ch = '.' then
      m:=10
    else begin
      d:=ord(ch)-ord('0');

      if m = 0 then
        Result:=(Result*10)+d
      else begin
        Result:=Result+d/m;
        m:=m*10
      end
    end;

    Inc(i)
  end;

  if sign then Result:=-Result
end;

procedure StoA(var fa; Len: Integer; S: PChar);
begin
  if Len > 0 then begin
    FillChar(fa,Len,0);
    Move(S[0],fa,Min(Len,StrLen(S)))
  end
end;

function ItoA(V,Len: Integer; var fa): String;
var
  i,t: Integer; s: String;
begin
  t:=Abs(V); s[0]:=char(Len);

  for i:=Len downto 1 do begin
    s[i]:=chr(ord('0')+(t mod 10));
    t:=t div 10;
  end;

  if V < 0 then s[1]:='-';

  Move(s[1],fa,Len); Result:=s
end;

procedure ItoC(V,Len: Integer; var fa);
var
  I: Integer; buf: TShortStr;
begin
  ItoA(V,Len,buf); for I:=0 to Len-1 do begin
    if buf[i] = '0' then buf[i]:=' ' else Break
  end; Move(buf,fa,Len)
end;

function PageToStr(i,j,len: Integer): string;
var
  a: alpha;
begin
  Result:=ItoA(i,len,a)+ItoA(j,len,a)
end;

function iLongStr(d: longint): string;
var
  t: longint; s: string[3];
begin
  Result:=''; t:=Abs(d); while t <> 0 do begin
    s[0]:=chr(3); ItoA(t mod 1000,3,s[1]);
    if length(Result) > 0 then Result:=' '+Result;
    Result:=s+Result; t:=t div 1000
  end;

  while (length(Result) > 2) and (Result[1] = '0') do
  System.Delete(Result,1,1);

  if d < 0 then Result:='-'+Result
end;

function DegreeStr(a: double): string;
begin
  Result:=IntToStr(Round(a*180/Pi))
end;

function TimeStr(t: Integer): string;
const
  ts: string[8] = '00:00:00';
var
  h,m,s: Integer;
begin
  Unpack_time(t, h,m,s);

  ItoA(h,2,ts[1]);
  ItoA(m,2,ts[4]);
  ItoA(s,2,ts[7]);
  Result:=ts
end;

function xTimeStr(dt: Double): string;
const
  ts: string[8] = '00:00:00';
var
  hh,mm,ss,ms: Word;
begin
  DecodeTime(dt, hh,mm,ss,ms);

  ItoA(hh mod 24,2,ts[1]);
  ItoA(mm,2,ts[4]);
  ItoA(ss,2,ts[7]);
  Result:=ts
end;

function mTimeStr(dt: Double): string;
const
  ts: string[5] = '00:00';
var
  hh,mm,ss,ms: Word;
begin
  DecodeTime(dt, hh,mm,ss,ms);

  ItoA(hh mod 24,2,ts[1]);
  ItoA(mm,2,ts[4]);
  Result:=ts
end;

function xTimeStr1(dt: Double): string;
const
  ts: string[8] = '00:00:00';
var
  hh,mm,ss,ms: Word; s: String;
begin
  DecodeTime(dt, hh,mm,ss,ms);

  ItoA(hh mod 24,2,ts[1]);
  ItoA(mm,2,ts[4]);
  ItoA(ss,2,ts[7]); s:=ts;

  if ms <> 0 then begin
    if ms mod 10 = 0 then ms:=ms div 10;
    if ms mod 10 = 0 then ms:=ms div 10;
    if ms <> 0 then s:=s+'.'+IntToStr(ms)
  end;

  Result:=s
end;

function xTimeStr2(dt: Double; m: int): string;
const
  ts: string[8] = '00:00:00';
var
  i: int; hh,mm,ss,ms: Word; s: String; c: TShortstr;
begin
  DecodeTime(dt, hh,mm,ss,ms);

  if m = 0 then
  if ms > 500 then begin
    Inc(ss); ms:=0;
    if ss = 60 then begin
      Inc(mm); ss:=0;
      if mm = 60 then begin
        Inc(hh); mm:=0;
        if hh >= 24 then
        hh:=hh mod 24
      end
    end
  end;

  ItoA(hh mod 24,2,ts[1]);
  ItoA(mm,2,ts[4]);
  ItoA(ss,2,ts[7]); s:=ts;

  if m > 0 then begin
    for i:=2 downto 0 do begin
      c[i]:=Char(ord('0')+(ms mod 10));
      ms:=ms div 10
    end;

    if m > 3 then m:=3; s:=s+'.';
    for i:=0 to m-1 do s:=s+c[i]
  end;

  Result:=s
end;

function xTimeName(dt: Double): string;
const
  ts: string[8] = '00-00-00';
var
  Hour,Min,Sec,MSec: Word;
begin
  DecodeTime(dt, Hour,Min,Sec,MSec);

  ItoA(Hour,2,ts[1]);
  ItoA(Min,2,ts[4]);
  ItoA(Sec,2,ts[7]);

  Result:=ts
end;

function AviStr(t,fmt: int): string;
var
  Hour,Min,Sec: Word; s: String;
  st: Longint;
begin
  st:=t; t:=Abs(t);
  Sec:=t mod 60; t:=t div 60;
  Min:=t mod 60; t:=t div 60;
  Hour:=t;

  s:='00:00:00';
  ItoA(Hour,2,s[1]);
  ItoA(Min,2,s[4]);
  ItoA(Sec,2,s[7]);

  if fmt = 1 then
    System.Delete(s,1,3)
  else
  if fmt = 2 then begin
    if Hour = 0 then
    System.Delete(s,1,3)
  end;

  if st < 0 then s:='-'+s;
  Result:=s
end;

function MSecStr(ms,fmt: longint): string;
var
  Hour,Min,Sec,len: Word; i,t: Longint; s: String;
begin
  t:=Abs(ms) div 1000;
  Sec:=t mod 60; t:=t div 60;
  Min:=t mod 60; t:=t div 60;
  Hour:=t;

  s:='00:00:00';
  ItoA(Hour,2,s[1]);
  ItoA(Min,2,s[4]);
  ItoA(Sec,2,s[7]);

  len:=fmt shr 8;
  fmt:=fmt and 255;

  if fmt = 1 then
    System.Delete(s,1,3)
  else
  if fmt = 2 then begin
    if Hour = 0 then
    System.Delete(s,1,3)
  end;

  if ms < 0 then s:='-'+s;

  if len > 0 then begin
    ms:=Abs(ms) mod 1000; s:=s+'.';
    for i:=1 to len do begin
      s:=s+IntToStr((ms div 100) mod 10);
      ms:=ms * 10
    end
  end;

  Result:=s
end;

function GameStr(t: longint): string;
var
  hh,mm,ss,st,l: Integer; s,s1: String;
begin
  st:=t; t:=Abs(t);
  ss:=t mod 60; t:=t div 60; mm:=t;

  if t >= 200 then begin
    mm:=t mod 60; hh:=t div 60;
    s:=ItoA(hh,2,s1)+':'+ItoA(mm,2,s1)
  end
  else begin
    l:=2; if mm >= 100 then l:=3;
    s:=ItoA(mm,l,s1);
  end;

  s:=s+':'+ItoA(ss,2,s1);
  if st < 0 then s:='-'+s;
  Result:=s
end;

function hGameStr(t: Double): string;
var
  h,m,s,ms: Integer; a: alfa;
begin
  ms:=Round(Frac(t)*100); s:=Trunc(t);
  if ms >= 100 then begin Dec(ms,100); Inc(s) end;

  m:=s div 60; s:=s mod 60;
  h:=m div 60; m:=m mod 60;

  Result:=ItoA(h,2,a)+':'+ItoA(m,2,a)+':'+
          ItoA(s,2,a)+'.'+ItoA(ms,2,a)
end;

function mkTimeStr(t: Double; len: Integer): string;
var
  i,l,h,m,s,ms,mk: Integer;
  t0: Double; a: alfa; str: String;
begin
  mk:=1; l:=Abs(len);
  for i:=1 to l do mk:=mk*10;

  t0:=t; t:=Abs(t);

  ms:=Round(Frac(t)*mk); s:=Trunc(t);
  if ms >= mk then begin
    Dec(ms,mk); Inc(s)
  end;

  m:=s div 60; s:=s mod 60;
  h:=m div 60; m:=m mod 60;
  h:=h mod 24;

  if len > 0 then

    Result:=ItoA(h,2,a)+':'+ItoA(m,2,a)+':'+
            ItoA(s,2,a)+'.'+ItoA(ms,l,a)

  else begin str:='';
    if h > 0 then str:=str + ItoA(h,2,a)+':';
    if m > 0 then str:=str + ItoA(m,2,a)+':';
    Result:=str + ItoA(s,2,a)+'.'+ItoA(ms,l,a)
  end;

  if t0 < 0 then Result:='-'+Result
end;

function xGameStr(t: Double): string;
var
  min,sec,ms: Integer; a: alfa;
  sign: Double;
begin
  sign:=t; t:=Abs(t);
  ms:=Round(Frac(t)*100); sec:=Trunc(t);
  if ms >= 100 then begin Dec(ms,100); Inc(sec) end;
  min:=sec div 60; sec:=sec mod 60;

  if min >= 10 then Result:=IntToStr(min)
  else              Result:=ItoA(min,2,a);

  Result:=Result+':'+ItoA(sec,2,a)+'.'+ItoA(ms,2,a);
  if sign < 0 then Result:='-'+Result
end;

function iGameStr(t: Double): string;
var
  p: Integer;
begin
  Result:=xGameStr(Abs(t)); p:=Pos('.',Result);
  if p > 0 then Result[0]:=Char(p-1);
  if t < 0 then Result:='-'+Result
end;

function iDateStr(year,month,day: int): String;
const
  ds: string[8] = '00-00-00';
begin
  ItoA(Day,2,ds[1]);
  ItoA(Month,2,ds[4]);
  ItoA(Year,2,ds[7]);
  Result:=ds
end;

function sDateStr1(year,month,day: int): String;
var
  s1,s2: String; last: Char;
begin
  s1:=IntToStr(month);
  s2:=IntToStr(year);

  if rus_interface then begin
    s1:=AnsiLowerCase(rus_month[month]);
    last:=s1[Length(s1)];
    if last in ['ü','é'] then
      s1[Length(s1)]:='ÿ'
    else
      s1:=s1+'à';

    s2:=s2+'ã.'
  end;

  Result:=Format('%d %s %s',[day,s1,s2])
end;

function xDateStr(dt: Double): string;
const
  ds: string[8] = '00-00-00';
var
  Year,Month,Day: Word;
begin
  DecodeDate(dt, Year,Month,Day);

  ItoA(Day,2,ds[1]);
  ItoA(Month,2,ds[4]);
  ItoA(Year,2,ds[7]);
  Result:=ds
end;

function xDateName(dt: Double): string;
const
  ds: string[8] = '00-00-00';
var
  Year,Month,Day: Word;
begin
  DecodeDate(dt, Year,Month,Day);

  ItoA(Year,2,ds[1]);
  ItoA(Month,2,ds[4]);
  ItoA(Day,2,ds[7]);
  Result:=ds
end;

function xDateStr1(dt: Double): string;
const
  ds: string[8] = '00-00';
var
  Year,Month,Day: Word;
begin
  DecodeDate(dt, Year,Month,Day);

  ItoA(Month,2,ds[1]);
  ItoA(Day,2,ds[4]);
  Result:=ds
end;

function msTimeStr(dt: Double): string;
const
  ts: string[13] = '00:00:00.000';
var
  Hour,Min,Sec,MSec: Word;
begin
  DecodeTime(dt, Hour,Min,Sec,MSec);

  ItoA(Hour,2,ts[1]);
  ItoA(Min,2,ts[4]);
  ItoA(Sec,2,ts[7]);
  ItoA(MSec,3,ts[10]);
  Result:=ts
end;

function DelayStr(t: Integer): string;
var
  sign: string[1]; _t: Integer;
begin
  _t:=Abs(t) - (Abs(t) mod 60);
  if t < 0 then t:=-_t else t:=_t;
  sign:=''; if t > 0 then sign:='+'
  else if t < 0 then sign:='-';
  Result:=sign + BusStr(_t);
end;

function BusStr(t: longint): string;
begin
  if t > 24*3600 then Dec(t,24*3600); 
  Result:=Copy(TimeStr(t),1,5)
end;

function DateStr(d: longint): string;
const
  s: string[10] = '01.01.1996';
begin
  ItoA(d,2,s[1]);
  ItoA(d div 100,2,s[4]);

  d:=d div 10000;
  if d < 100 then Inc(d,2000);

  ItoA(d,4,s[7]);
  Result:=s
end;

function DateStr1(dt: Double): string;
begin
  Result:=DateStr(xDecodeDate(dt))
end;

function DateShort(d: longint): string;
begin
  Result:=DateStr(d);
  Delete(Result,7,2)
end;

function DateMonth(d: longint): string;
begin
  Result:=DateStr(d); Result[0]:=chr(5)
end;

function MonthStr(d: longint): string;
var
  month: Integer;
begin
  Result:=''; month:=Get_Month(d);
  if month in [1..12] then Result:=rus_month[month]
end;

function DayRuss(d: longint): string;
begin
  Result:=IntToStr(d mod 100)+' '+MonthStr(d);
  if Get_Month(d) in [3,8] then Result:=Result+'à'
  else Result[length(Result)]:='ÿ'
end;

function DayStr(d: Integer): string;
const
  s: array[0..1] of char = '00';
begin
  ItoA(d,2,s); Result:=s
end;

function TimeShort(T: Integer): string;
var
  Min,Sec: Integer;
  s: String[7];
begin
  Min:=(T mod 3600) div 60;
  Sec:=T mod 60;

  s:='00:00'; ItoA(Min,2,s[1]);
  ItoA(Sec,2,s[4]); Result:=s
end;

function xBusStr(Dt: TDatetime): String;
begin
  Result:=FormatDateTime('hh:mm',Dt)
end;

function xGpsStr(Dt: TDatetime): String;
begin
  Result:=FormatDateTime('hh:mm:ss',Dt)
end;

function FileDateStr(dat: longint): string;
const
  s: string[10] = '01.01.1996';
var
  dt: TDateTime; Year,Month,Day: word;
begin
  dt:=FileDateToDateTime(dat);
  DecodeDate(dt, Year,Month,Day);

  ItoA(Day,2,s[1]);
  ItoA(Month,2,s[4]);
  ItoA(Year,4,s[7]);

  Result:=s
end;

function SVal(var a; len: Integer): string;
var
  i1,i2: Integer; s: array[1..255] of char;
begin
  Result:=''; if len > 0 then begin
    Move(a,s,len); i1:=1; i2:=len;
    while (i1 <= i2) and (s[i1] in [#0,' ']) do Inc(i1);
    while (i1 <= i2) and (s[i2] in [#0,' ']) do Dec(i2);
    Result:=Copy(s,i1,i2+1-i1)
  end
end;

function IVal(var a; len: Integer): Integer;
var
  v,rc: Integer;
begin
  Result:=0; Val(SVal(a,len),v,rc);
  if rc = 0 then Result:=v
end;

function xIVal(var a; len: Integer): Integer;
var
  s: String; v,rc: Integer; r: Double;
begin
  Result:=0; s:=SVal(a,len);
  Val(s,v,rc); if rc <> 0 then begin
    Val(s,r,rc); if rc = 0 then
    if Abs(r) < Maxlongint then
    v:=Trunc(r)
  end;

  if rc = 0 then Result:=v
end;

function XVal(var a; len: Integer): Int64;
var
  rc: Integer;
begin
  Val(SVal(a,len),Result,rc);
  if rc <> 0 then Result:=0
end;

function RVal(var a; len: Integer): double;
var
  i,rc: int; v: Double; s: string;
begin
  Result:=0; s:=SVal(a,len);

  if Length(s) > 0 then begin
    i:=System.Pos(',',s);
    if i > 0 then s[i]:='.';

    Val(s,v,rc); if rc = 0 then
    Result:=v
  end
end;

function TVal(var fa): Integer;
var
  a: array[0..15] of char absolute fa;
  hour,min,sec,sec100: word;
begin
  if length(SVal(a,8)) = 0 then
    DecodeTime(Time,hour,min,sec,sec100)
  else begin
    hour:=AtoI(a[0],2);
    min:=AtoI(a[3],2);
    sec:=AtoI(a[6],2)
  end;

  Result:=PackTime(hour,min,sec)
end;

function DVal(var fa; len: Integer): Integer;
var
  a: array[0..15] of char absolute fa;
  year,month,day: word; dd: longint;
begin
  if length(SVal(a,len)) = 0 then
    DecodeDate(Date,year,month,day)
  else begin
    day:=AtoI(a[0],2); month:=AtoI(a[3],2);
    year:=AtoI(a[6],len-6); if len = 8 then
    Inc(year,1900)
  end;

  dd:=year; DVal:=(dd * 10000) + (month * 100) + day
end;

function StrToDate(const s: string): Integer;
var
  day,month,year: Integer;
begin
  day:=0; month:=0; year:=0;

  if length(s) >= 2 then begin
    day:=AtoI(s[1],2);

    if length(s) >= 5 then begin
      month:=AtoI(s[4],2);

      if length(s) >= 8 then
      year:=AtoI(s[7],2)
    end
  end;

  Result:=(year * 10000) + (month * 100) + day
end;

function StrToTime(const s: string): Integer;
var
  hour,min,sec: Integer;
begin
  hour:=0; min:=0; sec:=0;

  if length(s) >= 2 then begin
    hour:=AtoI(s[1],2);

    if length(s) >= 5 then begin
      min:=AtoI(s[4],2);

      if length(s) >= 8 then
      sec:=AtoI(s[7],2)
    end
  end;

  Result:=(hour * 3600) + (min * 60) + sec
end;

const
  _UPChar: array[char] of byte =
   (00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {00}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {10}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {20}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {30}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {40}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {50}
    00,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,  {60}
    32,32,32,32,32,32,32,32,32,32,32,00,00,00,00,00,  {70}

    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {80}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {90}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {A0}
    00,00,00,00,00,00,00,00,16,00,00,00,00,00,00,00,  {B0}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {C0}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {D0}
    32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,  {E0}
    32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32); {F0}


function xUpCase(ch: Char): Char;
begin
  if ver_English then Result:=Upcase(ch)
  else Result:=Char(ord(ch)-_UPChar[ch])
end;

function xStrUpper(p: PChar): PChar;
var
  i,l: Integer; ch: char;
begin
  Result:=p;

  if Assigned(p) then

  if ver_English then
    StrUpper(p)
  else begin
    l:=StrLen(p); for i:=0 to l-1 do begin
      ch:=p[i]; p[i]:=Char(ord(ch)-_UPChar[ch])
    end
  end;
end;

function xStrComp(s1,s2: PChar): Boolean;
begin
  StrUpper(s1); StrUpper(s2);
  Result:=StrComp(s1,s2) = 0
end;

function xStrLIComp(s1,s2: PChar; len: int): int;
var
  i: int; c1,c2: Char;
begin
  Result:=0;
  if len = 0 then len:=256;

  if not rus_interface then
    Result:=StrLIComp(s1,s2,len)
  else
  for i:=0 to len-1 do begin
    c1:=s1[0]; s1:=@s1[1];
    c2:=s2[0]; s2:=@s2[1];

    if c1 = #0 then begin
      if c2 <> #0 then Result:=-1; Break
    end else
    if c2 = #0 then begin
      Result:=+1; Break
    end
    else begin
      c1:=Char(ord(c1)-_UPChar[c1]);
      c2:=Char(ord(c2)-_UPChar[c2]);
      if c1 < c2 then begin
        Result:=-1; Break
      end else
      if c1 > c2 then begin
        Result:=+1; Break
      end
    end
  end
end;

function IsTownShort(Str: PChar; Len: int): bool;
begin
  Result:=false;

  case Len of
2:  if (StrLComp(Str,'ÊÐ',2) = 0)
    or (StrLComp(Str,'ÑÐ',2) = 0) then
    Result:=true;

3:  if (StrLComp(Str,'ÁÎË',3) = 0)
    or (StrLComp(Str,'ÌÀË',3) = 0)
    or (StrLComp(Str,'ÍÎÂ',3) = 0)
    or (StrLComp(Str,'ÍÈÆ',3) = 0)
    or (StrLComp(Str,'ËÅÂ',3) = 0) then
    Result:=true;

4:  if (StrLComp(Str,'ÂÅÐÕ',4) = 0)
    or (StrLComp(Str,'ÑÒÀÐ',4) = 0)
    or (StrLComp(Str,'ÑÐÅÄ',4) = 0)
    or (StrLComp(Str,'ÏÐÀÂ',4) = 0) then
    Result:=true;

5:  if (StrLComp(Str,'ÊÐÀÑÍ',5) = 0) then
    Result:=true;
  end;

  if not result then
  len:=len
end;

function xTownComp(s1,s2: PChar; len,mode: int): int;
var
  i,err: int; _s2: PChar; c1,c2: Char; number: bool;
begin
  Result:=0;
  if len = 0 then len:=256;

  number:=false; err:=0;

  i:=0; _s2:=s2;
  while i < len do begin Inc(i);

    c1:=s1[0]; s1:=@s1[1];
    c2:=s2[0]; s2:=@s2[1];

    if c2 in [' ','-'] then _s2:=s2;

    if c1 = #0 then begin
      if mode = 1 then
      if c2 = '(' then
        c2:=#0
      else
      if c2 = ' ' then
      if s2[0] = '(' then
      c2:=#0;

      if c2 <> #0 then Result:=-1; Break
    end else
    if c2 = #0 then begin
      Result:=+1; Break
    end
    else begin
      if rus_interface then begin
        c1:=Char(ord(c1)-_UPChar[c1]);
        c2:=Char(ord(c2)-_UPChar[c2]);
      end
      else begin
        c1:=Char(ord(c1)-ord(Upcase(c1)));
        c2:=Char(ord(c2)-ord(Upcase(c2)));
      end;

      if c1 < c2 then begin
        Result:=-1;

        if number and (c1 = ' ') then begin
          s2:=@s2[-1]; Result:=0
        end else
        if mode = 1 then begin
          if err = 0 then
          if ((c1 = 'È') and (c2 = 'Û'))
          or ((c1 = 'À') and (c2 = 'Î'))
          or ((c1 = ' ') and (c2 = '-')) then
          begin Inc(err); Result:=0 end
        end;

        if Result <> 0 then Break
      end else
      if c1 > c2 then begin
        Result:=+1;

        if number and (c2 = ' ') then begin
          Dec(i); s1:=@s1[-1]; Result:=0
        end else
        if (c1 = 'Ñ') and (c2 = 'C') then
          Result:=0
        else
        if (c2 in [' ','.']) and
           IsTownShort(_s2,s2-_s2-1) then begin

          if s2[0] = ' ' then s2:=@s2[1];

          while i < len do begin
            Inc(i); c1:=s1[0]; s1:=@s1[1];
            if c1 = ' ' then begin
              Result:=0; Break
            end
          end

        end else
        if mode = 1 then begin
          if err = 0 then
          if ((c1 = 'Û') and (c2 = 'È'))
          or ((c1 = 'Î') and (c2 = 'À')) then
          begin Inc(err); Result:=0 end
        end;

        if Result <> 0 then Break
      end else
      if i = 1 then
        number:=c1 in ['0'..'9']
      else
      if number then
        number:=c1 in ['0'..'9']
    end
  end
end;

function xStreetComp(s1,s2: PChar): Integer;
var
  i: Integer; c1,c2: Char; words: Boolean;
begin
  Result:=0; words:=false;

  if not rus_interface then
    Result:=StrIComp(s1,s2)
  else
  for i:=0 to 255 do begin
    c1:=s1[0]; s1:=@s1[1];
    c2:=s2[0]; s2:=@s2[1];

    if c1 = #0 then begin
      if not words then
      if c2 <> #0 then Result:=-1; Break
    end else
    if c2 = #0 then begin
      if not words then Result:=+1; Break
    end
    else begin
      c1:=Char(ord(c1)-_UPChar[c1]);
      c2:=Char(ord(c2)-_UPChar[c2]);
      if c1 < c2 then begin
        if (c1 <> '.') or (s1[0] <> #0)
        or not words then Result:=-1; Break
      end else
      if c1 > c2 then begin
        if (c2 <> '.') or (s2[0] <> #0)
        or not words then Result:=+1; Break
      end else
      if c1 = ' ' then words:=true
    end
  end
end;

function xStrSkipToken(Str: PChar; Ch: Char): PChar;
var
  pc: PChar;
begin
  Result:=nil;
  pc:=StrScan(Str,Ch);
  if Assigned(pc) then begin
    StrCopy(Str,@pc[1]);
    Result:=Str
  end
end;

function sStrComp(s1: PChar; const s2: string): Boolean;
var
  t: TShortStr;
begin
  Result:=false;

  if StrPCopy(t,s2) <> nil then

  if StrUpper(s1) <> nil then
  if StrUpper(t) <> nil then
  if StrComp(s1,t) = 0 then

  Result:=true
end;

function sRusComp(s1: PChar; const s2: string): Boolean;
var
  t1,t2: TShortStr;
begin
  Result:=false;

  if StrCopy(t1,s1) <> nil then
  if StrPCopy(t2,s2) <> nil then

  if xStrUpper(t1) <> nil then
  if xStrUpper(t2) <> nil then
  if StrComp(t1,t2) = 0 then

  Result:=true
end;

function is_rus(p: PChar): Boolean;
var
  i: Integer;
begin
  Result:=false;
  for i:=0 to StrLen(p)-1 do
  if p[i] > #127 then begin
    Result:=true; Break
  end
end;

function is_rus_pas(const s: String): Boolean;
var
  i: Integer;
begin
  Result:=false;
  for i:=1 to Length(s) do
  if s[i] > #127 then begin
    Result:=true; Break
  end
end;

function is_rus_wide(const s: WideString): Boolean;
var
  i: Integer;
begin
  Result:=false;
  for i:=1 to Length(s) do
  if s[i] > #127 then begin
    Result:=true; Break
  end
end;

function is_natf(const s: WideString): Boolean;
var
  i: Integer; ch: WideChar;
begin
  Result:=false;
  for i:=1 to Length(s) do begin
    ch:=s[i]; if ch > #255 then begin
      Result:=true; Break
    end else
    if WideCharLenToString(@ch,1) <> ch then begin
      Result:=true; Break
    end
  end
end;

function is_pack_wide(s: PWideChar): Boolean;
var
  _s: TWideStr;
begin
  StringToWideChar(WideCharToString(s),_s,255);
  Result:=WideStrComp(s,_s) = 0
end;

function is_Ansi_wide(s: PWideChar): Boolean;
begin
  Result:=true;
  while s[0] <> #0 do begin
    if ord(s[0]) > 255 then begin
      Result:=false; Break
    end; s:=@s[1]
  end
end;

function is_dos_wide(Src: PWideChar; out Dst: String): Boolean;
var
  s1,s2: ShortString;
begin
  Result:=false; Dst:='';

  if WideStrLen(Src) < 256 then
  if is_pack_wide(Src) then begin
    s1:=WideCharToString(Src);
    s2:=DosString(s1);
    if WinString(s2) = s1 then begin
      Dst:=s2; Result:=true
    end
  end
end;

function wide_to_dos(const Src: WideString;
                     out Dst: String): Boolean;
var
  w: TWideStr;
begin
  Result:=false; Dst:='';
  if Length(Src) < 256 then 
  if StringToWideChar(Src,w,255) <> nil then
  Result:=is_dos_wide(w,Dst)
end;

function WideToAnsi(dst: PChar; src: PWideChar; MaxLen: Cardinal): PChar;
var
  i: Integer; ch: WideChar;
begin
  Result:=dst;

  for i:=1 to MaxLen do begin
    ch:=src[0]; src:=@src[1];
    if ch = #0 then Break;
    dst[0]:=Char(Ch); dst:=@dst[1]
  end;

  dst[0]:=#0
end;

function StrRus(s: PChar): PChar;
var
  p: PChar;
begin
  if xStrLen(s) > 0 then begin
    p:=@s[1];

    StrRecode(p,'Þ','þ');
    StrRecode(p,'Þ','þ');
    StrRecode(p,'ß','ÿ');
    StrRecode(p,'ß','ÿ');
  end;

  Result:=s
end;

function StrMain(s: PChar): PChar;
var
  p: PChar; c: char;
begin
  Result:=s; p:=s;

  while p^ <> #0 do begin c:=p^;

    if (c >= #$80) and (c <= #$AF) then
      c:=chr(ord(c)+$30);

    p^:=c; p:=@p[1]
  end;
end;

function MainString(const s: string): string;
var
  t: TShortStr;
begin
  Result:=StrPas(StrMain(StrPCopy(t,s)))
end;

function dos_win(ch: Char): Char;
begin
  Result:=ch;
  if ch = #0 then Result:=' ' else
  if ch = #196 then Result:=#150 else
  if ch = #205 then Result:=#151 else
  if ch = #240 then Result:=#168 else
  if ch = #241 then Result:=#184 else
  if ch = #249 then Result:=#177 else
  if ch = #252 then Result:=#185 else
  if ch = #253 then Result:=#167 else
  if ch >= #$F0 then ch:=chr(ord(ch)-$50)
end;

function win_dos(ch: Char): Char;
begin
  Result:=ch;
  if ch = #0 then Result:=' ' else
  if ch = #150 then Result:=#196 else
  if ch = #151 then Result:=#205 else
  if ch = #168 then Result:=#240 else
  if ch = #184 then Result:=#241 else
  if ch = #177 then Result:=#249 else
  if ch = #185 then Result:=#252 else
  if ch = #167 then Result:=#253 else
  if (ch >= #$A0) and (ch <= #$AF) then
  ch:=chr(ord(ch)+$50)
end;

function WinChar(ch: Char): Char;
begin
  if not ver_English then
  
  if (ch >= #$80) and (ch <= #$AF) then
    ch:=chr(ord(ch)+$40)
  else
  if (ch >= #$E0) and (ch <= #$EF) then
    ch:=chr(ord(ch)+$10)
  else
    ch:=dos_win(ch);

  Result:=ch
end;

function StrWin(s: PChar): PChar;
var
  p: PChar; ch: Char;
begin
  Result:=s; p:=s;

  if Assigned(s) then
  if not ver_English then

  while p[0] <> #0 do begin
    ch:=p[0];

    if (ch >= #$80) and (ch <= #$AF) then
      ch:=chr(ord(ch)+$40)
    else
    if (ch >= #$E0) and (ch <= #$EF) then
      ch:=chr(ord(ch)+$10)
    else
      ch:=dos_win(ch);

    p[0]:=ch; p:=@p[1]
  end
end;

function StrWin1(s: PChar): PChar;
var
  p: PChar; ch: Char;
begin
  Result:=s; p:=s;

  if Assigned(s) then
  if not ver_English then

  while p[0] <> #0 do begin
    ch:=p[0];

    if ch = 'o' then ch:='î' else
    if ch = 'O' then ch:='Î' else
    if ch = 'c' then ch:='ñ' else
    if ch = 'C' then ch:='Ñ' else
    if ch = 'M' then ch:='Ì' else

    if (ch >= #$80) and (ch <= #$AF) then
      ch:=chr(ord(ch)+$40)
    else
    if (ch >= #$E0) and (ch <= #$EF) then
      ch:=chr(ord(ch)+$10)
    else
      ch:=dos_win(ch);

    p[0]:=ch; p:=@p[1]
  end
end;

function WinString(s: string): string;
var
  i: int; ch: char;
begin
  if not ver_English then
  for i:=1 to length(s) do begin
    ch:=s[i];

    if (ch >= #$80) and (ch <= #$AF) then
      ch:=chr(ord(ch)+$40)
    else
    if (ch >= #$E0) and (ch <= #$EF) then
      ch:=chr(ord(ch)+$10)
    else
      ch:=dos_win(ch);

    s[i]:=ch
  end;

  Result:=s
end;

procedure WinAlfa(s: PChar; len: Integer);
var
  i: Integer; ch: char;
begin
  if not ver_English then

  for i:=1 to len do begin
    ch:=s[0];

    if (ch >= #$80) and (ch <= #$AF) then
      ch:=chr(ord(ch)+$40)
    else
    if (ch >= #$E0) and (ch <= #$EF) then
      ch:=chr(ord(ch)+$10)
    else
      ch:=dos_win(ch);

    s[0]:=ch; s:=@s[1]
  end
end;

procedure DosAlfa(s: PChar; len: Integer);
var
  t: TShortstr;
begin
  if len > 0 then begin
    StrLCopy(t,s,len); StrDos(t);
    Move(t[0],s[0],len)
  end
end;

function StrDos(s: PChar): PChar;
var
  i,l: int; ch: char;
begin
  Result:=s;

  if not ver_English then begin

    l:=Strlen(s);
    for i:=1 to l do begin
      ch:=s[0];

      if (ch >= #$C0) and (ch <= #$EF) then
        ch:=chr(ord(ch)-$40)
      else
      if ch >= #$F0 then
        ch:=chr(ord(ch)-$10)
      else
        ch:=win_dos(ch);

      s[0]:=ch; s:=@s[1]
    end
  end
end;

function DosString(s: string): string;
var
  i: Integer; ch: char;
begin
  if not ver_English then
  for i:=1 to length(s) do begin
    ch:=s[i];

    if (ch >= #$C0) and (ch <= #$EF) then
      ch:=chr(ord(ch)-$40)
    else
    if ch >= #$F0 then
      ch:=chr(ord(ch)-$10)
    else
      ch:=win_dos(ch);

    s[i]:=ch
  end;

  Result:=s
end;

function DosToUTF8(const s: string): string;
begin
  Result:=AnsiToUTF8(WinString(s))
end;

function DosToUnicodep(const Src: String; Dest: PWideChar): PWideChar;
begin
  if ver_English then
     Result:=WinToUnicodep(Src,Dest)
  else
    Result:=WinToUnicodep(WinString(Src),Dest)
end;

function UpString(s: string): string;
var
  t: TShortStr;
begin
  Result:=StrPas(xStrUpper(StrPCopy(t,s)))
end;

function DbfStr(Dest,Str: PChar; Len: Integer): PChar;
var
  si,di,sp: PChar; i,l: Integer; ch: Char;
begin
  Result:=nil;

  if Len > 255 then Len:=255;
  si:=Str; di:=Dest; sp:=nil; l:=0;

  for i:=1 to Len do begin
    ch:=si[0]; si:=@si[1];

    if ch in [#0,' '] then begin

      if l > 0 then begin
        if sp = nil then sp:=di;
        di[0]:=' '; di:=@di[1]
      end

    end
    else begin sp:=nil;

      if not ver_English then begin

        if (ch >= #$80) and (ch <= #$AF) then
          ch:=chr(ord(ch)+$40)
        else
        if (ch >= #$E0) and (ch <= #$EF) then
          ch:=chr(ord(ch)+$10);

        ch:=char(ord(ch)-_UPChar[ch])
      end;

      di[0]:=ch; di:=@di[1]; Inc(l);
    end;
  end;

  if sp <> nil then sp[0]:=#0
  else di[0]:=#0;

  if l > 0 then begin
    if ver_English then StrUpper(Dest);
    Result:=Dest
  end
end;

function R_to_S(r: double): Integer;
begin
  Result:=Round(r*180*3600/Pi)
end;

function RadToDeg1(rad: double; out g,m: Integer): Double;
var
  r: Double;
begin
  r:=Abs(rad) * 180 / Pi;
  g:=Trunc(r); r:=(r-g)*60; m:=Trunc(r);
  Result:=(r-m)*60; if rad < 0 then g:=-g
end;

function R_to_G(r: double; long: Integer;
                out g,m: Integer): Double;
var
  sign: Boolean; i,k: Integer;
begin
  sign:=r < 0; r:=Abs(r); r:=r * 180 / Pi;
  g:=Trunc(r); r:=(r-g)*60; m:=Trunc(r);

  Result:=(r-m)*60; if long = 0 then begin
    if Result >= 30 then Inc(m); Result:=0
  end
  else begin k:=1;
    for i:=1 to long-2 do k:=k*10;
    Result:=Round(Result*k)/k;

    if Result >= 60 then begin
      Result:=Result-60; Inc(m)
    end
  end;

  if m = 60 then begin Inc(g); m:=0 end;
  if sign then g:=-g
end;

function xRadian(degree: double): double;
begin
  Result:=degree/180*Pi
end;

function ToDegree(g: Integer; m,s: double): double;
begin
  Result:=g + m/60 + s/3600
end;

function Radian(g: Integer; m,s: double): double;
var
  xg,xm: double;
begin
  xg:=g; xm:=(m + s/60)/60;
  if g < 0 then xg:=xg - xm else xg:=xg + xm;
  Result:=xg * Pi / 180
end;

function Sec_to_Rad(s: Integer): double;
begin
  Result:=s/100/3600/180 * Pi
end;

function Rad_to_Sec(r: double): longint;
begin
  Result:=Round(r*180*3600*100/Pi)
end;

function Round_Angle(a: double; m: Integer): double;
var
  gr,min: Integer; sec: double;
begin
  sec:=R_to_G(Abs(a),2,gr,min); if m = 0 then sec:=0
  else sec:=Round(sec*m)/m; Result:=Radian(gr,min,sec);
  if a < 0 then Result:=-Result
end;

function Str_xy(x,y: double; m: Integer; d,s: PChar): PChar;
var
  sx,sy: tShortStr;
begin
  str(x:0:m,sx); str(y:0:m,sy);
  Str_xy:=StrCat(StrCat(StrCopy(s,sx),d),sy)
end;

function RealStr(r: double; l,m: Integer): string;
var
  sign: Boolean; f: Double; i,i1,i2: Integer;
begin
  sign:=r < 0; r:=Abs(r);

  if m = 0 then i1:=Round(r)
  else i1:=Trunc(r); i2:=0;

  if m > 0 then begin
    f:=Frac(r); for i:=1 to m do f:=f*10;
    i2:=Round(f); if Decimals(i2) > m then
    begin Inc(i1); i2:=0 end
  end;

  Result:=xIntToStr(i1,l,'0'); if i2 > 0 then
  Result:=Result+'.'+xIntToStr(i2,m,'0');

  if sign then if Result[1] = '0' then
  Result[1]:='-' else Result:='-'+Result
end;

function xRealStr(r: double; l,m: Integer): string;
var
  p: Integer;
begin
  Result:=RealStr(r,l,m);
  p:=System.Pos('.',Result);

  if (m > 0) and (p = 0) then begin
    if l = 0 then l:=length(Result)+1+m;
    if length(Result)+2 <= l then
    Result:=Result+'.'
  end;

  if p > 0 then
  while (length(Result) < l)
  and   (length(Result)-p < m) do
  Result:=Result+'0';
end;

function xStrAngle(s: PChar; r: double;
                   m: Integer; ch: Char): PChar;
var
  grad,p,len: Integer; min: Double; fmt,sign,smin: String;
begin
  sign:=''; if r < 0 then sign:='-';
  r:=Abs(r); r:=r * 180 / Pi;

  grad:=Trunc(r); min:=Frac(r)*60;
  if Abs(min-60) < 1E-6 then begin
    Inc(grad); min:=0
  end;

  if m > 0 then begin
    fmt:='%0.'+IntToStr(m)+'f';
    smin:=Format(fmt,[min])
  end
  else begin
    smin:=Format('%0.3f',[min]);

    len:=length(smin);
    while (len > 1) and (smin[len] = '0') do
    begin Dec(len); smin[0]:=chr(len) end
  end;

  p:=System.Pos(',',smin);
  if p > 0 then smin[p]:='.';
  if System.Pos('.',smin) < 3 then smin:='0'+smin;

  Result:=StrFmt(s,'%s%d'#176'%s''%s',[sign,grad,smin,ch])
end;
              
function StrAngle(s: PChar; a: double; long: Integer): PChar;
var
  fmt,grad,min, m,d,p: Integer;
  sec: double; s1,s2: String[63];
begin
  sec:=R_to_G(Abs(a),long, grad,min);

  fmt:=0; if Is_Angle_fmt then fmt:=Angle_fmt;

  if fmt = 2 then
    Result:=StrPCopy(s,RealToStr(a*180/Pi,6)+#176)
  else begin
    if fmt = 0 then
      s2:=xIntToStr(min,2,'0')
    else begin
      m:=3; d:=1000; long:=0;

      min:=Round((min + sec/60) * d);
      if min >= 60*d then begin
        Dec(min,60*d); Inc(grad)
      end;

      s2:=RealToStr(min/d,m);
      while true do begin
        p:=Pos('.',s2);
        if p > 2 then Break else
        if p > 0 then s2:='0'+s2 else
        begin s2:='00.'+s2; Break end
      end;

      while length(s2) < 3+m do s2:=s2+'0';
    end;

    s1:=''; if a < 0 then s1:='-';
    s1:=s1 + IntToStr(grad)+#176 + s2+'''';

    Result:=StrPCopy(s,s1);

    if long > 0 then begin
      m:=Max(0,long-2);

      if m = 0 then
        s2:=xIntToStr(Trunc(sec),2,'0')
      else
      if Is_Seconds_Align then
        s2:=xRealStr(sec,2,m)
      else
        s2:=RealStr(sec,2,m);

      PStrCat(s,s2+'"')
    end
  end
end;

function DocAngle(s: PChar; Delimiter: Char;
                  a: double; long: Integer): PChar;
var
  p: PChar;
begin
  Is_Seconds_Align:=true;
  Result:=StrAngle(s,a,long);
  Is_Seconds_Align:=false;

  p:=StrScan(s,#176);
  if p <> nil then p[0]:=Delimiter;

  p:=StrScan(s,'''');
  if p <> nil then p[0]:=Delimiter;

  p:=StrScan(s,'"');
  if p <> nil then p[0]:=#0;
end;

function NorthStr(r: double; m: Integer): string;
var
  s: TShortStr; ch: Char;
begin
  ch:='N'; if r < 0 then ch:='S';
  Result:=StrPas(xStrAngle(s,Abs(r),m,ch))
end;

function EastStr(r: double; m: Integer): string;
var
  s: TShortStr; ch: Char;
begin
  ch:='E'; if r < 0 then ch:='W';
  Result:=StrPas(xStrAngle(s,Abs(r),m,ch))
end;

function xNorthStr(r: double; m: Integer): string;
var
  ch: Char;
begin
  ch:='N'; if r < 0 then ch:='S';
  Result:=AngleStr(Abs(r),2+m)+ch
end;

function xEastStr(r: double; m: Integer): string;
var
  ch: Char;
begin
  ch:='E'; if r < 0 then ch:='W';
  Result:=AngleStr(Abs(r),2+m)+ch
end;

function AngleStr(a: double; long: Integer): string;
var
  s: TShortStr;
begin
  Result:=StrPas(StrAngle(s,a,long))
end;

function xAngleStr(a: double): string;
begin
  str(a*180/Pi:0:6,Result)
end;

function AngleFormat(a: double; m: Integer): string;
var
  gr: Double;
begin
  gr:=a*180/Pi;
  if Abs(gr) >= 100 then Dec(m,2) else
  if Abs(gr) >= 10 then Dec(m);

  Result:=RealToStr(gr,Max(1,m))
end;

function GeoidStr(b,l: Double; m,ne: Integer): String;
var
  n,e: String[1];
begin
  n:=''; e:=''; if ne = 1 then begin
    n:='N'; if b < 0 then begin n:='S'; b:=-b end;
    e:='E'; if l < 0 then begin e:='W'; l:=-l end
  end;

  Result:=AngleStr(b,2+m)+n+':'+AngleStr(l,2+m)+e
end;

function EarthStr(f: Double; m: Integer; ns: PChar): String;
var
  n: String[3];
begin
  n:=ns[0]; if f < 0 then begin n:=ns[1]; f:=-f end;
  Result:=AngleStr(f,2+m)+n
end;

function StrToken(dst,src: PChar): PChar;
var
  p,q,s: PChar; ch: Char;
begin
  Result:=nil; s:=src;
  if dst <> nil then StrCopy(dst,'');

  if src <> nil then
  if src[0] <> #0 then begin

    while src[0] <> #0 do begin
      if (src[0] = #9) or (src[0] = ' ') then
      src:=@src[1] else Break
    end;

    p:=nil; if src[0] <> #0 then begin
      if src[0] = '"' then begin
        src:=@src[1]; p:=StrScan(src,'"')
      end else
      if Is_semicolon_Delimiter then
        p:=StrScan(src,';')
      else begin
        if p = nil then begin
          p:=StrScan(src,' ');
          q:=StrScan(src,#9);

          if p = nil then p:=q else
          if (q <> nil) and (q < p) then
          p:=q
        end;

        if Is_Comma_Delimiter then begin
          q:=StrScan(src,',');
          if p = nil then p:=q else if q <> nil then
          if not Is_Number or (q < p) then p:=q
        end else

        if p = nil then p:=StrScan(src,',')
      end
    end;

    if p <> nil then begin
      ch:=p[0]; p[0]:=#0;
      if dst <> nil then StrLCopy(dst,src,255);

      if Is_semicolon_Delimiter then begin
        if ch = '"' then
        p:=StrScan(@p[1],';')
      end else

      if ch <> ',' then
      while StrLen(@p[1]) > 0 do begin
        ch:=p[1]; if ch in [#9,' ',','] then begin
          p:=@p[1]; if ch = ',' then Break
        end else Break
      end;

      if p = nil then StrCopy(s,'')
                 else StrCopy(s,@p[1])
    end
    else begin
      if dst <> nil then
      StrLCopy(dst,src,255);
      StrCopy(s,'')
    end;

    Result:=dst
  end;
end;

function lStrToken(dst,src: PChar): Boolean;
begin
  Result:=false;
  if StrToken(dst,src) <> nil then
  Result:=Strlen(dst) > 0
end;

function StrWord(dst,src: PChar): PChar;
var
  Is_fl: Boolean;
begin
  is_fl:=Is_Comma_Delimiter;
  Is_Comma_Delimiter:=false;
  Result:=StrToken(dst,src);
  Is_Comma_Delimiter:=is_fl
end;

function FieldToken(dst,src: PChar; Delimiter: Char): string;
var
  p,q: PChar; len: Integer; s: TShortStr;
begin
  StrCopy(s,''); LStr(src);

  q:=src;
  if q[0] = '"' then begin
    p:=StrScan(@q[1],'"');
    if Assigned(p) then q:=@p[1]
  end;

  p:=StrScan(q,Delimiter);
  if (p = nil) and (Delimiter = ' ') then
  p:=StrScan(q,#9);

  if p <> nil then begin
    StrLCopy(s,src,p-src);
    StrCopy(src,@p[1])
  end
  else begin
    StrCopy(s,src);
    StrCopy(src,'')
  end;

  RStr(LStr(s)); len:=StrLen(s);

  if s[0] = '"' then
  if s[len-1] = '"' then

  if len > 2 then
    StrLCopy(s,@s[1],len-2)
  else
    StrCopy(s,'');

  Result:=StrPas(s);
  xStrCopy(dst,s)
end;

function StringToken(src: PChar): string;
var
  s: tShortStr;
begin
  StrToken(s,src); Result:=StrPas(s)
end;

function StrMsg(dst,src: PChar; ch: char): PChar;
var
  p: PChar;
begin
  StrCopy(dst,#0); Result:=nil;

  if (src <> nil) and (StrLen(src) > 0) then begin

    while StrLen(src) > 0 do begin
      if (src^ = #9) or (src^ = ' ') then
      src:=@pbytes(src)^[1] else Break
    end;

    if StrLen(src) > 0 then begin
      if src^ = ch then src:=@pbytes(src)^[1];
      if StrLen(src) > 0 then if ch <> ' ' then begin
        p:=StrScan(src,ch); if p <> nil then p^:=#0;
        Result:=StrCopy(dst,src)
      end
    end
  end
end;

function IntToken(s: PChar; out v: Integer): Boolean;
var
  t: TShortStr; i,rc: Integer;
begin
  Result:=false; v:=0;
  if StrToken(t,s) <> nil then begin
    Val(t,i,rc); if rc = 0 then
    begin v:=i; Result:=true end
  end;
end;

function StrInts(Str: PChar; v: PIntegers; N: Integer): PChar;
var
  i: int; t: TShortstr;
begin
  Result:=nil;

  if N > 0 then begin
    StrCopy(Str,'');
    for i:=0 to N-1 do begin
      if i > 0 then StrLCat(Str,' ',255);
      StrPCopy(t,IntToStr(v[i]));
      StrLCat(Str,t,255)
    end;

    Result:=Str
  end
end;

function IntTokens(s: PChar; v: PIntegers; N: Integer): Integer;
var
  i,t: Integer;
begin
  Result:=0;
  for i:=1 to N do
  if IntToken(s,t) then begin
    v[Result]:=t; Inc(Result)
  end
end;

function DWordToken(s: PChar; out v: DWord): Boolean;
var
  t: TShortStr; rc: Integer;
begin
  Result:=false; v:=0;
  if StrToken(t,s) <> nil then begin
    Val(t,v,rc); if rc <> 0 then v:=0
    else Result:=true
  end
end;

function Int64Token(s: PChar; out v: Int64): Boolean;
var
  t: TShortStr; rc: Integer;
begin
  Result:=false; v:=0;
  if StrToken(t,s) <> nil then begin
    Val(t,v,rc); if rc <> 0 then v:=0
    else Result:=true
  end
end;

function RealToken(s: PChar; out v: double): Boolean;
var
  e,rc: int; p: PChar;
  r: Double; fl: bool; t: TShortStr;
begin
  Result:=false; v:=0;

  fl:=Is_Number; Is_Number:=true;

  if is_Real_Point then
    p:=StrToken(t,s)
  else
    p:=StrWord(t,s);

  Is_Number:=fl;

  if p <> nil then begin
    p:=StrScan(t,',');
    if p <> nil then p[0]:='.';

    e:=0; rc:=0;

    p:=StrScan(t,'E');
    if Assigned(p) then begin
      p[0]:=#0; p:=@p[1];
      val(p,e,rc)
    end;

    if rc = 0 then Val(t,r,rc);

    if rc = 0 then begin

      while e > 0 do begin
        r:=r*10; Dec(e)
      end;

      while e < 0 do begin
        r:=r/10; Inc(e)
      end;

      if Double_Correct(r) then
      begin v:=r; Result:=true end
    end
  end
end;

function GeoidToken(s: PChar; ch: Char; out r: Double): Boolean;
var
  al,ax,bx,m,len: int; gr,min,sec: Double;
  p: PChar; is_sign,is_gr: Boolean;
begin
  Result:=false; r:=0; s:=LStr(s);

  if xStrLen(s) >= 6 then
  if s[StrLen(s)-1] = ch then begin

    is_gr:=false; gr:=0; min:=0; sec:=0;

    is_sign:=false; if s[0] = '-' then
    begin is_sign:=true; s:=@s[1] end;

    ax:=0; bx:=0; m:=0;
    while s[0] <> #0 do begin

      if s[0] in ['0'..'9'] then begin

        al:=ord(s[0]) - ord('0');
        if m = 0 then
          ax:=ax*10 + al
        else begin
          bx:=bx*10 + al;
          m:=m*10
        end;

        is_gr:=true
      end
      else
      if s[0] = '.' then begin
        if m > 0 then begin
          is_gr:=false; Break
        end; m:=1;
      end
      else begin
        s:=@s[1]; Break
      end;

      s:=@s[1]
    end;

    if is_gr then begin

      gr:=ax; if m > 0 then gr:=gr+bx/m;

      len:=StrLen(s)-1;
      if len > 0 then begin
        p:=StrScan(s,'''');
        if p <> nil then len:=p-s;
        min:=AtoR(s[0],len);

        if min < 0 then begin min:=Abs(min);
          if not is_sign then is_sign:=true
        end;

        if p <> nil then begin
          s:=@p[1]; len:=StrLen(s)-1;
          p:=StrScan(s,'"');
          if p <> nil then len:=p-s;

          if len > 0 then sec:=AtoR(s[0],len);
        end
      end;

      r:=(gr + min/60 + sec/3600)*Pi/180;
      
      if is_sign then r:=-r;
      Result:=true
    end
  end
end;

function LatitudeToken(s: PChar; out r: Double): Boolean;
begin
  Result:=false;

  if s[StrLen(s)-1] = 'N' then
    Result:=GeoidToken(s,'N',r)
  else
  if s[StrLen(s)-1] = 'S' then begin
    Result:=GeoidToken(s,'S',r);
    if Result and (r > 0) then
    r:=-r
  end
end;

function LatitudeToken1(s: PChar; out r: Double): Boolean;
var
  v: Double; rc: Integer;
begin
  Result:=LatitudeToken(s,r);
  if not Result then begin
    val(s,v,rc); if rc = 0 then begin
      r:=v/180*Pi; Result:=true
    end
  end
end;

function LongitudeToken(s: PChar; out r: Double): Boolean;
begin
  Result:=false;

  if s[StrLen(s)-1] = 'E' then
    Result:=GeoidToken(s,'E',r)
  else begin
    Result:=GeoidToken(s,'W',r);
    if Result and (r > 0) then
    r:=-r
  end
end;

function LongitudeToken1(s: PChar; out r: Double): Boolean;
var
  v: Double; rc: Integer;
begin
  Result:=LongitudeToken(s,r);
  if not Result then begin
    val(s,v,rc); if rc = 0 then begin
      r:=v/180*Pi; Result:=true
    end
  end
end;

function xLatitudeToken(s: PChar; out r: Double): Boolean;
var
  t: TShortstr;
begin
  Result:=false; r:=0;
  if StrToken(t,s) <> nil then
  Result:=LatitudeToken(t,r)
end;

function xLongitudeToken(s: PChar; out r: Double): Boolean;
var
  t: TShortstr;
begin
  Result:=false; r:=0;
  if StrToken(t,s) <> nil then
  Result:=LongitudeToken(t,r)
end;

function DegToken(s: PChar; out r: Double): Boolean;

function ival(s: PChar; out v: int): int;
var
  i,ax: int; ch: Char;
begin
  Result:=0; ax:=0;
  for i:=1 to 255 do begin
    ch:=s[0]; s:=@s[1];
    if ch = #0 then Break;

    if ch in ['0'..'9'] then
      ax:=ax*10 + ord(ch) - ord('0')
    else begin
      Result:=i; Break
    end
  end;

  v:=ax
end;

function rval(s: PChar; out v: double): int;
var
  i,ax,bx,rc,m: int; ch: Char;
begin
  Result:=0; v:=0;

  rc:=ival(s,ax); bx:=0; m:=1;

  if rc = 0 then
    v:=ax
  else
  if s[rc-1] <> '.' then
    Result:=rc
  else begin
    s:=@s[rc];
    for i:=1 to 255 do begin
      ch:=s[0]; s:=@s[1];

      if ch = #0 then
        Break
      else
      if ch in ['0'..'9'] then begin
        bx:=bx*10 + ord(ch) - ord('0');
        m:=m * 10
      end
      else begin
        Result:=rc+i; Break
      end
    end;
  end;

  v:=ax + bx/m;
end;

var
  rc: int; p: PChar; gr,min,sec: Double;
begin
  Result:=false; r:=0;

  if LStr(s) <> nil then
  if StrLen(s) > 0 then begin

    rc:=rval(s,gr); min:=0; sec:=0;

    if rc > 1 then begin
      StrCopy(s,@s[rc]);

      if Strlen(s) > 0 then begin
        rc:=rval(s,min);
        if rc > 1 then
        if s[rc-1] = '''' then begin
          StrCopy(s,@s[rc]);

          if Strlen(s) > 0 then begin

            p:=StrScan(s,'"');
            if p = nil then
            p:=StrScan(s,'''');

            if Assigned(p) then p[0]:=#0;

            rc:=rval(s,sec);
            if rc = 0 then
            if p = nil then
              StrCopy(s,'')
            else
              StrCopy(s,@p[1])
          end
        end
      end;

      r:=Abs(gr) + min/60 + sec/3600;
      if gr < 0 then r:=-r;

      Result:=Strlen(s) = 0
    end
  end
end;

function LatToken(Str: PChar; out r: Double): Boolean;
var
  len,rc: Integer; ch: Char; s,t: TShortstr;
begin
  Result:=false; StrToken(s,Str);

  len:=StrLen(s);
  if len > 0 then begin

    val(s,r,rc);
    if rc = 0 then
      Result:=true
    else
    if len > 1 then begin
      ch:=s[len-1];

      if ch in ['N','n'] then begin
        StrLCopy(t,s,len-1);
        Result:=DegToken(t,r)
      end else
      if ch in ['S','s'] then begin
        StrLCopy(t,s,len-1);
        Result:=DegToken(t,r);
        if Result and (r > 0) then
        r:=-r
      end
      else Result:=DegToken(s,r);
    end
  end
end;

function LonToken(Str: PChar; out r: Double): Boolean;
var
  len,rc: Integer; ch: Char; s,t: TShortstr;
begin
  Result:=false; StrToken(s,Str);

  len:=StrLen(s);
  if len > 0 then begin

    val(s,r,rc);
    if rc = 0 then
      Result:=true
    else
    if len > 1 then begin
      ch:=s[len-1];

      if ch in ['E','e'] then begin
        StrLCopy(t,s,len-1);
        Result:=DegToken(t,r)
      end else
      if ch in ['W','w'] then begin
        StrLCopy(t,s,len-1);
        Result:=DegToken(t,r);
        if Result and (r > 0) then
        r:=-r
      end
      else Result:=DegToken(s,r);
    end
  end
end;

function StrInt(s: PChar): Integer;
var
  p: PChar; ch: Char;
begin
  Result:=0; p:=s;
  while p[0] <> #0 do begin
    ch:=p[0]; p:=@p[1];
    if not (ch in ['0'..'9']) then Break;
    Result:=Result * 10 + ord(ch) - ord('0')
  end; StrCopy(s,p)
end;

function xStrToInt(S: PChar; out V: Integer): Boolean;
var
  rc: Integer;
begin
  Result:=false; val(S,V,rc);
  if rc = 0 then Result:=true else V:=0;
end;

function StrToReal(S: PChar; out V: Double): Boolean;
var
  rc: Integer;
begin
  Result:=false; val(S,V,rc);
  if rc = 0 then Result:=true else V:=0;
end;

function StrToFloat(s: PChar; out v: Double): Boolean;
var
  i,m, ax,bx,sgn: Integer;
  ch: Char; t: Double;
begin
  Result:=false; sgn:=1; m:=0;

  ax:=0; bx:=0;
  for i:=0 to 255 do begin
    ch:=s[0]; s:=@s[1];

    if ch in ['0'..'9'] then begin

      if m = 0 then
        ax:=ax*10+ord(ch)-ord('0')
      else begin
        bx:=bx*10+ord(ch)-ord('0');
        m:=m*10
      end;

      Result:=true
    end else

    if ch = '.' then begin
      if m > 0 then begin
        Result:=false; Break
      end; m:=1;
    end else

    if ch = '+' then begin
      if i > 0 then begin
        Result:=false; Break
      end
    end else

    if ch = '-' then begin
      if i > 0 then begin
        Result:=false; Break
      end; sgn:=-1;
    end else

    if ch = #0 then
      Break
    else begin
      Result:=false; Break
    end
  end;

  t:=ax; if m > 0 then
  t:=t + bx/m; v:=t * sgn
end;

function StrToValue(S: PChar; out V: Double): Integer;
var
  p: PChar;
begin
  Result:=-1;
  if StrToReal(S,V) then
    Result:=0
  else begin
    p:=StrScan(S,'"'); if p = nil then
    p:=StrScan(S,''''); if p = nil then
    p:=StrScan(S,#176); if p <> nil then

    if GeoidToken(S,p[0],v) then Result:=1
  end
end;

function StrToValueG(S,NS: PChar; out V: Double): Integer;
var
  p: PChar; sign: Double;
begin
  Result:=-1;
  if StrToReal(S,V) then
    Result:=0
  else begin
    p:=StrScan(S,'"'); if p = nil then
    p:=StrScan(S,''''); if p = nil then
    p:=StrScan(S,#176); if p <> nil then begin

      sign:=1;
      if p[1] = NS[0] then
        p:=@p[1]
      else
      if p[1] = NS[1] then begin
        p:=@p[1]; sign:=-1
      end;

      if GeoidToken(S,p[0],v) then begin
        v:=v*sign; Result:=1
      end
    end
  end
end;

function StrInside(Dst,Str: PChar; LB,RB: Char): PChar;
var
  p1,p2: PChar;
begin
  Result:=nil;

  p1:=StrScan(Str,LB); p2:=StrScan(Str,RB);
  if (p1 <> nil) and (p2 <> nil) then begin
    p1:=@p1[1]; if p1 < p2 then
    Result:=StrLCopy(Dst,p1,p2-p1)
  end;

  if Result = nil then StrCopy(Dst,'')
end;

function StrToPoint(Str: PChar; LB,RB: Char; out P: TGauss): Integer;
var
  S: TShortStr; p1,p2: PChar; rc1,rc2: Integer;
begin
  Result:=-1; P.X:=0; P.Y:=0;

  p1:=StrInside(S,Str, LB,RB);

  if p1 <> nil then begin

    p2:=StrScan(p1,',');
    if p2 <> nil then begin
      p2[0]:=#0; p2:=@p2[1];

      rc1:=StrToValue(p1,P.X);
      rc2:=StrToValue(p2,P.Y);

      if (rc1 >= 0) and (rc1 = rc2) then
      Result:=rc1
    end
  end
end;

function StrTransit(s,ch: PChar): PChar;
var
  p: PChar;
begin
  p:=StrPos(s,ch); Result:=p;

  while p <> nil do begin
    p[0]:=' '; p:=StrPos(s,ch)
  end
end;

function StrReplace(s,ch1,ch2: PChar): PChar;
var
  p: PChar;
begin
  p:=StrPos(s,ch1); Result:=p;

  while p <> nil do begin
    p[0]:=ch2^; p:=StrPos(s,ch1)
  end
end;

function StrReplaces(s,s1,s2: PChar): PChar;
var
  i: int; p: PChar; ch: Char;
begin
  Result:=s;
  for i:=0 to 255 do begin
    ch:=s[i]; if ch = #0 then Break;

    p:=StrScan(s1,ch);
    if Assigned(p) then
    ch:=s2[p-s1];

    s[i]:=ch
  end
end;

const
  tr_ru: TShortstr = 'àáâãäåæçèêëìíîïðñòóô'+
                     'ÀÁÂÃÄÅÆÇÈÊËÌÍÎÏÐÑÒÓÔ';
  tr_en: TShortstr = 'abvgdegziklmnoprstuf'+
                     'ABVGDEGZIKLVNOPRSTUF';

function RuToEn(s: PChar): PChar;
var
  i: int; p: PChar; ch: Char;
begin
  Result:=s;
  for i:=0 to 255 do begin
    ch:=s[i]; if ch = #0 then Break;

    p:=StrScan(tr_ru,ch);
    if Assigned(p) then
    ch:=tr_en[p-tr_ru];

    s[i]:=ch
  end
end;

function AngleToken(s: PChar; out r: double): Boolean;
var
  t: TShortStr; g,m,rc: Integer;
begin
  Result:=false; r:=0;

  if StrWord(t,s) <> nil then begin
    Val(t,r,rc); if rc = 0 then
      Result:=true
    else begin
      if StrTransit(t,',') = nil then
      if StrTransit(t,'/') = nil then
      StrTransit(t,'\');

      if IntToken(t,g) then
      if IntToken(t,m) then begin
        r:=0; rc:=0;
        if StrLen(t) > 0 then
        Val(t,r,rc);

        if rc = 0 then begin
          r:=Radian(g,m,r);
          Result:=true
        end
      end
    end
  end
end;

function CodeToken(s: PChar; out code: Integer): Boolean;
var
  rc: int; t: TShortStr;
begin
  Result:=false; code:=0;
  if StrWord(t,s) <> nil then
  if StrLen(t) = 8 then begin
    code:=StrToCode(UpperCase(StrPas(t)));
    Result:=true
  end else

  if is_Number_code then begin
    val(t,code,rc); Result:=rc = 0
  end
end;

function PointToken(s: PChar; out p: TPoint): Boolean;
begin
  Result:=IntToken(s,p.x) and IntToken(s,p.y)
end;

function GaussToken(s: PChar; out g: tgauss): Boolean;
begin
  Result:=RealToken(s,g.x) and RealToken(s,g.y)
end;

function rgbToken(s: PChar; out cl: tColorRef): Boolean;
var
  r,g,b: Integer;
begin
  Result:=false; cl:=0;

  if IntToken(s,r) and (r in [0..255]) then
  if IntToken(s,g) and (g in [0..255]) then
  if IntToken(s,b) and (b in [0..255]) then begin
    cl:=RGB(r,g,b); Result:=true
  end
end;

function xyzToken(s: PChar; out v: txyz): Boolean;
begin
  Result:=false; v.x:=0; v.y:=0; v.z:=0;

  if RealToken(s,v.x) then
  if RealToken(s,v.y) then
  if RealToken(s,v.z) then
  Result:=true
end;

function sTimeToken(s: PChar; out t: Double): Boolean;
var
  hh,mm,ss,ms,rc: Integer;
  p: PChar;
begin
  Result:=false; t:=0;

  hh:=0; mm:=0; ss:=0; ms:=0;

  p:=s; val(p,hh,rc);
  if rc = 1 then hh:=-1 else
  if rc > 0 then begin
    p:=@p[rc]; val(p,mm,rc);
    if rc = 1 then mm:=-1 else
    if rc > 0 then begin
      p:=@p[rc]; val(p,ss,rc);

      if rc > 0 then
      if rc = 1 then ss:=-1
      else begin
        p:=@p[rc]; val(p,ms,rc);
        if rc <> 0 then ss:=-1
      end
    end
  end;

  if hh in [0..23] then
  if mm in [0..59] then
  if ss in [0..59] then begin
    t:=EncodeTime(hh,mm,ss,ms); Result:=true
  end
end;

function TimeToken(s: PChar; out dt: Double): Boolean;
var
  hour,min,sec,msec,mdiv,len: int;
  p: PChar; ts: TShortstr;
begin
  Result:=false; dt:=0;

  p:=StrScan(s,' '); if p = nil then
  p:=StrScan(s,#9);

  if p <> nil then
    StrLCopy(ts,s,p-s)
  else
    StrCopy(ts,s);

  len:=Strlen(ts); msec:=0;

  if ts[1] = ':' then
  len:=Strlen(StrPCopy(ts,'0'+Strpas(ts)));

  if len in [8,10..14] then
  if StoI(ts[0],2,hour) and (ts[2] = ':') then
  if StoI(ts[3],2,min) and (ts[5] = ':') then
  if StoI(ts[6],2,sec) then

  if (len = 8)
  or ((ts[8] = '.') and StoI(ts[9],len-9,msec)) then

  if hour in [0..23] then
  if min in [0..59] then
  if sec in [0..59] then begin

    Dec(len,9);
    if len > 3 then begin
      mdiv:=1000;
      while len > 3 do begin
        mdiv:=mdiv*10; Dec(len)
      end;

      msec:=Trunc(msec/mdiv*1000)
    end;

    dt:=EncodeTime(hour,min,sec,msec);
    Result:=true
  end;

  if Result then

  if p <> nil then
    StrCopy(s,@p[1])
  else
    StrCopy(s,'');
end;

function dTimeToken(s: PChar; out dt: Double): Boolean;
var
  sgn: int;
begin
  Result:=false;

  sgn:=1;
  if s[0] in ['+','-'] then begin
    if s[0] = '-' then sgn:=-1; s:=@s[1]
  end;

  Result:=TimeToken(s,dt);
  if Result and (sgn <0) then dt:=-dt
end;

function DateToken(s: PChar; fmt: Integer;
                   out dt: Double): Boolean;
var
  year,month,day,len,mode,rc: Integer;
  p: PChar; yy,mm,dd: Word; ts: TShortstr;
begin
  Result:=false; dt:=0;

  mode:=fmt shr 8; fmt:=fmt and 255;

  p:=StrScan(s,' ');
  if p = nil then p:=StrScan(s,#9);

  if p <> nil then StrLCopy(ts,s,p-s)
  else             StrCopy(ts,s);

  len:=Strlen(ts) - 6;

  if len in [2,4] then begin rc:=0;

    if ts[2] in ['.','\','/','-'] then begin
      if ts[5] = ts[2] then
      if StoI(ts[0],2,day) then
      if StoI(ts[3],2,month) then
      if StoI(ts[6],len,year) then rc:=1
    end else
    if ts[4] in ['.','\','/','-'] then begin
      if ts[7] = ts[4] then
      if StoI(ts[0],4,year) then
      if StoI(ts[5],2,month) then
      if StoI(ts[8],2,day) then rc:=1
    end;

    if rc = 1 then begin
      if fmt = 1 then iSwap(day,month) else
      if fmt = 2 then iSwap(day,year);

      if (day = 0) and (month = 0) and (year = 0) then
      if mode = 1 then begin
        DecodeDate(Date,yy,mm,dd);
        day:=dd; month:=mm; year:=yy
      end;

      if month in [1..12] then
      if day in [1..31] then begin
        if year < 20 then Inc(year,2000);
        dt:=EncodeDate(year,month,day);
        Result:=true
      end;
    end
  end;

  if Result then

  if p <> nil then
    StrCopy(s,@p[1])
  else
    StrCopy(s,'');
end;

function x_RealToken(s: PChar; out v: double;
                     v_min,v_max: Double): Boolean;
begin
  Result:=false; if RealToken(s,v) then
  if (v >= v_min) and (v <= v_max) then
  Result:=true
end;

function StrString(dst,src: PChar): PChar;
var
  p: PChar; len: Integer;
  tmp: TShortStr;
begin
  Result:=nil; StrCopy(dst,'');

  if StrCopy(tmp,src) <> nil then
  if LStr(tmp) <> nil then
  if RStr(tmp) <> nil then begin
    p:=@tmp; if tmp[0] = '"' then p:=@tmp[1];
    len:=StrLen(p); Result:=StrCopy(dst,p);
    if p[len-1] = '"' then dst[len-1]:=#0
  end
end;

function xStrToken(src,cmd: PChar): Boolean;
var
  dst: TShortStr;
begin
  Result:=false;

  if StrToken(dst,src) <> nil then
  if StrUpper(dst) <> nil then

  Result:=StrComp(dst,cmd) = 0
end;

function StrDeleteChar(s,ch: PChar): PChar;
var
  p: PChar;
begin
  p:=StrPos(s,ch); if p <> nil then
  StrCopy(p,@p[1]); Result:=s
end;

function StrClearChar(s,ch: PChar): PChar;
var
  p: PChar;
begin
  while true do begin
    p:=StrPos(s,ch);
    if p <> nil then
      StrCopy(p,@p[1])
    else Break
  end;

  Result:=s
end;

function xIntToken(s: PChar): longint;
begin
  IntToken(s,Result)
end;

function xRealToken(s: PChar): double;
begin
  RealToken(s,Result)
end;

function xCmdToken(Str,Cmd: PChar): Boolean;
var
  p: PChar;
begin
  Result:=false;
  p:=StrScan(Str,' ');
  if p = nil then p:=StrEnd(Str);

  if StrLIComp(Str,Cmd,p-Str) = 0 then begin
    if p[0] <> #0 then p:=@p[1];
    StrCopy(Str,p); Result:=true
  end
end;

function dec_Round(d: double): longint;
var
  k: longint;
begin
  k:=1; while d >= 10 do
  begin d:=d / 10; k:=k * 10 end;
  dec_Round:=Round(d) * k
end;

function ReadPool(buf: pBytes; pos,size: Integer; var s: string): Integer;
var
  ch: char;
begin
  s:='';

  while pos < size do begin
    ch:=char(buf^[pos]); Inc(pos);
    if ch in [#0,#13] then Break;
    s:=s+ch
  end;

  Result:=pos
end;

function Current_century: Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Date, Year,Month,Day);
  Result:=(Year  div 100) * 100
end;

function Get_Month(date: Integer): Integer;
begin
  Result:=(date div 100) mod 100
end;

function Get_WeekDay(date: Integer): Integer;
begin
  Result:=0; if date >= 19960101 then begin
    Result:=Date_Dist(19960101,date+1) mod 7;
    if Result = 0 then Result:=7
  end
end;

function Get_Month_Days(date: Integer): Integer;
const
  Days: array[1..12] of Integer =
  (31,28,31,30,31,30,31,31,30,31,30,31);
var
  month: Integer;
begin
  Result:=0; month:=Get_Month(date);

  if month in [1..12] then begin
    Result:=Days[month]; if month = 2 then
    if (date div 10000) mod 4 = 0 then
    Inc(Result)
  end
end;

function PackDate(day,month,year: Integer): Integer;
begin
  Result:=year*10000 + month*100 + day
end;

function Next_Date(date,days: Integer): Integer;
var
  m,d,md,y: Integer;
begin
  while days > 0 do begin
    y:=date div 10000; m:=Get_Month(date);
    d:=Succ(date mod 100); md:=Get_Month_Days(date);

    if d > md then begin d:=1; Inc(m);
      if m > 12 then begin m:=1; Inc(y) end
    end;

    date:=PackDate(d,m,y);
    Dec(days)
  end;

  Result:=date
end;

function Date_Dist(d1,d2: Integer): Integer;
var
  t1,t2: Integer;
begin
  Result:=0; if d2 > 19900101 then begin
    if d1 < d2 then begin t1:=d1; t2:=d2 end
    else begin t1:=d2; t2:=d1 end;

    Result:=0; while t1 < t2 do begin
      t1:=Next_Date(t1,1); Inc(Result)
    end;

    if d1 > d2 then Result:=-Result
  end
end;

function MonthInt(dt: tDateTime): Integer;
var
  Year,Month,Day: Word;
begin
 DecodeDate(dt,Year,Month,Day);
 Result:=Year; Result:=Result*12+Month;
end;

function PackTime(h,m,s: Integer): Integer;
begin
  Result:=(h*60 + m)*60 + s
end;

procedure Unpack_time(t: Integer; out hh,mm,ss: Integer);
begin
  hh:=t div 3600; Dec(t,hh*3600);
  mm:=t div 60; ss:=t-mm*60
end;

procedure Unpack_date(dt: Integer; out yy,mm,dd: Integer);
begin
  dd:=dt mod 100; dt:=dt div 100;
  mm:=dt mod 100; dt:=dt div 100;
  yy:=dt
end;

function xEncodeTime(t: Integer): TDateTime;
var
  h,m,s: Integer;
begin
  Unpack_time(t, h,m,s);
  Result:=EncodeTime(h,m,s,0)
end;

function xEncodeDate(dt: Integer): TDateTime;
var
  y,m,d: Integer;
begin
  Unpack_date(dt, y,m,d);
  Result:=EncodeDate(y,m,d)
end;

function xDecodeTime(dt: Double): Longint;
var
  h,m,s,ms: Word;
begin
  DecodeTime(dt,h,m,s,ms);
  Result:=PackTime(h,m,s)
end;

function xDecodeDate(dt: Double): Longint;
var
  y,m,d: Word;
begin
  DecodeDate(dt,y,m,d);
  Result:=PackDate(d,m,y)
end;

function xFormatDateTime(Dt: TDatetime): String;
begin
  Result:=FormatDateTime('mm.dd.yyyy hh:mm:ss',Dt)
end;

function xStrToDate(s: PChar): TDateTime;
var
  p: PChar; ax,cx: Integer;
  dd: array[0..2] of Integer;
  fwd: Boolean; fmt: String;
  year,month,day: Word;
begin
  fmt:=ShortDateFormat;
  fwd:=true; if length(fmt) > 0 then
  if Upcase(fmt[1]) in ['M','Y'] then fwd:=false;

  dd[0]:=0; dd[1]:=0; dd[2]:=0;

  p:=s; ax:=0; cx:=0;

  while p[0] <> #0 do begin
    if p[0] in ['0'..'9'] then
      ax:=(ax*10) + ord(p[0]) - ord('0')
    else begin
      dd[cx]:=ax; ax:=0; Inc(cx);
      if cx = 3 then Break
    end;

    p:=@p[1]
  end;

  if (cx < 3) and (ax > 0) then
  begin dd[cx]:=ax; Inc(cx) end;

  if not fwd then

  if cx = 2 then
    iSwap(dd[0],dd[1])
  else
  if cx = 3 then
    iSwap(dd[0],dd[2]);

  if cx = 2 then begin
    DecodeDate(Date, year,month,day);
    dd[2]:=year
  end;

  if dd[2] < 1900 then
  dd[2]:=Current_century + dd[2];

  Result:=EncodeDate(dd[2],dd[1],dd[0])
end;

function _UpperDos(s: PChar): PChar;
const
  _UPChar: array[0..255] of byte =
   (00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {00}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {10}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {20}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {30}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {40}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {50}
    00,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,  {60}
    32,32,32,32,32,32,32,32,32,32,32,00,00,00,00,00,  {70}

    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {80}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {90}
    32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,  {A0}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {B0}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {C0}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,  {D0}
    80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,  {E0}
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00); {F0}
var
  i,len,ch: Integer;
begin
  Result:=s;

  if s <> nil then begin
    len:=StrLen(s);

    for i:=0 to len-1 do begin
      ch:=ord(s[i]); s[i]:=char(ch-_UPChar[ch])
    end
  end
end;

function _UpperRus(s: PChar): PChar;
begin
  StrRecode(s,'c',chr(ord('Ñ')-$40));
  StrRecode(s,'C',chr(ord('Ñ')-$40));
  Result:=_UpperDos(s)
end;

function StrEol(eol,str: PChar): PChar;
var
  p: PChar;
begin
  p:=eol; if Assigned(str) then

  while p[0] <> #0 do
  if StrScan(str,p[0]) = nil then
  p:=@p[1] else StrCopy(p,@p[1]);

  Result:=eol
end;

function Str_Compare(s1,s2,str: PChar; Mode: Integer): Boolean;

const
  Digits = ['0'..'9'];

function xStrComp(s1,s2: PChar; slen: int): int;
var
  i: int; ch1,ch2: Char;
begin
  Result:=0;

  if slen = 0 then slen:=256;
  for i:=0 to slen-1 do begin
    ch1:=s1[i]; ch2:=s2[i];

    if ch2 <> #0 then
    if (ch1 = #1) then begin
      if ch2 in Digits then ch2:=#1
    end else
    if ch1 = #2 then begin
      if not (ch2 in Digits) then ch2:=#2
    end;

    if ch1 <> #0 then
    if ch2 = #1 then begin
      if ch1 in Digits then ch1:=#1
    end else
    if ch2 = #2 then begin
      if not (ch1 in Digits) then ch1:=#2
    end;

    if ch1 <> ch2 then
    if ch1 < ch2 then begin
      Result:=-1; Break
    end
    else begin
      Result:=+1; Break
    end;

    if ch1 = #0 then Break;
    if ch2 = #0 then Break;
  end
end;

function StrScanDigit(Str: PChar): PChar;
var
  i: int; p: PChar; _ch: Char;
begin
  Result:=nil; p:=Str;
  for i:=1 to 255 do begin
    _ch:=p[0]; if _ch = #0 then Break;
    if _ch in Digits then begin
      Result:=p; Break
    end; p:=@p[1]
  end
end;

function StrScanNoDigit(Str: PChar): PChar;
var
  i: int; p: PChar; _ch: Char;
begin
  Result:=nil; p:=Str;
  for i:=1 to 255 do begin
    _ch:=p[0]; if _ch = #0 then Break;
    if not (_ch in Digits) then begin
      Result:=p; Break
    end; p:=@p[1]
  end
end;

var
  p,q,p1: PChar; s1_len,s2_len: int;
  s,eol: TShortStr; words,ext: bool;
  ch: Char;
begin
  Result:=false; p:=StrCopy(s,str);

  StrCopy(eol,' _/\~'); words:=true;

  ext:=false; q:=s1;
  while true do begin
    q:=StrScan(q,'^');
    if q = nil then Break;

    if q[1] = '#' then begin
      StrCopy(q,@q[1]);
      ext:=true; q[0]:=#1;
    end else
    if q[1] = '$' then begin
      StrCopy(q,@q[1]);
      ext:=true; q[0]:=#2;
    end else
    if s1[0] = '^' then begin
      StrCopy(s1,@q[1]);
      words:=false;
    end
    else q:=@q[1]
  end;

  s1_len:=StrLen(s1); s2_len:=xStrLen(s2);

  StrEol(eol,s1); StrEol(eol,s2);

  if Assigned(p) then

  if (s1_len = 0) and (Mode > 0) then
    Result:=true
  else
  while StrLen(p) >= s1_len do begin

    q:=nil; if words then begin
      q:=xStrScan(p,eol);
      if q <> nil then q[0]:=#0;
    end;

    p1:=p; if p1[0] = #0 then
      q:=nil
    else
    if ext then
      case Mode of
    1,
    4:  Result:=xStrComp(p,s1, s1_len) = 0;
    2:  Result:=xStrComp(@p[StrLen(p)-s1_len],s1,0) = 0;

    3:  while true do begin
          ch:=s1[0];
          if ch = #1 then
            p1:=StrScanDigit(p1)
          else
          if ch = #2 then
            p1:=StrScanNoDigit(p1)
          else
            p1:=StrScan(p1,s1[0]);

          if p1 = nil then Break;
          Result:=xStrComp(p1,s1, s1_len) = 0;

          if Result then Break; p1:=@p1[1]
        end;
      end
    else
      case Mode of
    1,
    4:  Result:=StrLComp(p,s1, s1_len) = 0;
    2:  Result:=StrComp(@p[StrLen(p)-s1_len],s1) = 0;
    3:  Result:=StrPos(p,s1) <> nil;
      end;

    if Result and (Mode = 4) then
    if s2 = nil then Result:=true else
    Result:=StrComp(@p[StrLen(p)-s2_len],s2) = 0;

    if Result or (q = nil) then Break else
    StrPCopy(p, TrimLeft(StrPas(@q[1])) );
  end
end;

function Str_Words_Comp(s1,s2: PChar): Boolean;
const
  divs = ' -^~_';
var
  p1,p2,q: PChar; len: Integer;
begin
  Result:=false;

  if StrLen(s1) > 0 then
  if StrLen(s2) > 0 then begin
    Result:=true;

    p1:=xStrSkip(s1,' ');
    p2:=xStrSkip(s2,' ');

    while p2[0] <> #0 do begin
      q:=xStrScan(p2,divs); len:=StrLen(p2);
      if q <> nil then len:=q-p2;

      if StrLen(p1) < len then begin
        Result:=false; Break
      end;

      if StrLComp(p2,p1,len) <> 0 then begin
        Result:=false; Break
      end;

      p2:=xStrSkip(@p2[len],divs);
      p1:=xStrSkip(@p1[len],divs);
    end;

    if Result then
    Result:=p1[0] = #0
  end
end;

function CodeToStr(code: longint): string;
var
  i: Integer; a: alfa;
begin
  if code < 0 then
    Result:=IntToStr(code)
  else begin
    for i:=8 downto 2 do begin
      a[i]:=Chr(code mod 10 + ord('0'));
      code:=code div 10
    end;

    code:=code + ord('0'); if code > ord('Z') then
    code:=ord('0'); a[1]:=chr(code); Result:=a
  end
end;

function xCodeToStr(code,loc: Integer): string;
begin
  Result:='['+CodeToStr(Code)+'-'+IntToStr(Loc)+']'
end;

function CodeToChar(code: Integer): Char;
begin
  Result:=chr(Min((code div 10000000)+ord('0'),ord('Z')))
end;

function StrToCode(const s: string): longint;
var
  t: string;
begin
  if length(s) > 8 then t:=System.Copy(s,1,8)
  else t:=s; while length(t) < 8 do t:=t+'0';
  Result:=AtoI(t[1],length(t))
end;

function StrToCode1(const s: string): longint;
var
  t: string;
begin
  t:=s; if length(t) > 8 then
  t:=System.Copy(s,1,8);
  Result:=AtoI(t[1],length(t))
end;

function div_Code(code: longint): longint;
var
  i: integer;
begin
  Result:=10; for i:=1 to 6 do
  if code mod Result = 0 then Result:=Result*10
  else Break; Result:=Result div 10
end;

function is_Code(const s: string): Boolean;
var
  i: Integer;
begin
  Result:=false; if length(s) <> 8 then Exit;
  if not (s[1] in ['0'..'9','A'..'Z']) then Exit;
  for i:=2 to 8 do if not (s[i] in ['0'..'9']) then Exit;
  Result:=true
end;

function StrCls(s: PChar): PChar;
var
  p: PChar; l: Integer;
begin
  Result:=nil;
  if s <> nil then begin

    p:=s; while p[0] <> #0 do
    if p[0] in [#9,' '] then
      p:=@p[1]
    else begin
      l:=StrLen(p)-1;
      while l >= 0 do begin
        if p[l] <> ' ' then Break;
        p[l]:=#0; Dec(l)
      end;

      if l >= 0 then
      Result:=StrCopy(s,p); Break
    end;

    if Result = nil then
    Result:=StrCopy(s,'')
  end
end;

function LStr(s: PChar): PChar;
var
  p: PChar;
begin
  Result:=nil;
  if s <> nil then begin

    p:=s; while p[0] <> #0 do
    if p[0] in [#9,' '] then
      p:=@p[1]
    else begin
      Result:=StrCopy(s,p); Break
    end;

    if Result = nil then
    Result:=StrCopy(s,'')
  end
end;

function RStr(s: PChar): PChar;
var
  i,len: Integer;
begin
  Result:=s;
  if Assigned(s) then begin
    len:=StrLen(s);
    for i:=len-1 downto 0 do
    if s[i] = ' ' then s[i]:=#0
    else Break
  end
end;

function xRStr(s: PChar; ch: Char): PChar;
var
  i,len: Integer;
begin
  Result:=s;
  if Assigned(s) then begin
    len:=StrLen(s);
    for i:=len-1 downto 0 do
    if s[i] = ch then s[i]:=#0
    else Break
  end
end;

function ClsString(const s: string): string;
var
  i1,i2: Integer; ch: char;
begin
  i1:=1; i2:=length(s);

  while i2 > 0 do begin
    ch:=s[i2]; if ch in [#0,' '] then
    Dec(i2) else Break
  end;

  while i1 <= i2 do begin
    ch:=s[i1]; if ch in [#0,' '] then
    Inc(i1) else Break
  end;

  if i1 > i2 then Result:='' else
  Result:=System.Copy(s,i1,i2-i1+1)
end;

function AddString(s,add,comma: string): string;
begin
  Result:=s; if length(add) > 0 then
  Result:=s+' '+comma+add
end;

procedure ClsWideString(var w: WideString);
var
  i,k,l: Integer;
begin
  l:=Length(w);
  if l > 0 then begin

    k:=0; for i:=1 to l do
    if w[i] = ' ' then Inc(k)
    else Break;

    if k > 0 then
    if k = l then w:=''
    else Delete(w,1,k);

    l:=Length(w);
    if l > 0 then begin
      i:=l;
      while i > 0 do
      if w[i] = ' ' then Dec(i)
      else Break;

      if i < l then
      if i = 0 then w:=''
      else SetLength(w,i)
    end
  end
end;

function StrTrunc(Str: PChar; Chr: Char): PChar;
var
  pc: PChar;
begin
  Result:=Str; pc:=StrScan(Str,Chr);
  if Assigned(pc) then pc[0]:=#0;
end;

function Trunc_Comment(skip,s: PChar): PChar;
var
  i: integer; p: PChar;
begin
  for i:=0 to StrLen(skip)-1 do begin
    p:=StrScan(s,skip[i]); if p <> nil then
    p[0]:=#0;
  end;

  Result:=RStr(LStr(s))
end;

function xTrunc_Comment(skip,s: PChar): PChar;
var
  i: integer; p: PChar; ch: Char;
begin
  Result:=nil;

  if Assigned(s) then begin

    for i:=0 to StrLen(skip)-1 do begin
      ch:=skip[i]; p:=StrScan(s,ch);

      while p <> nil do begin
        if p[1] = ch then begin
          p[0]:=#0; Break
        end else
        if p[1] = #0 then Break else
          p:=StrScan(@p[2],ch)
      end
    end;

    Result:=RStr(LStr(s))
  end
end;

function pTrunc_comment(skip,s: PChar): PChar;
var
  i: integer; p: PChar; ch: Char;
begin
  Result:=nil;

  if Assigned(s) then begin

    for i:=0 to StrLen(skip)-1 do begin
      ch:=skip[i]; p:=StrScan(s,ch);
      if Assigned(p) then
      if p[1] = ch then p[0]:=#0;
    end;

    Result:=RStr(LStr(s))
  end
end;

function StrSkip(s: PChar): PChar;
begin
  while true do begin
    if not (s[0] in [' ',#9]) then
    Break; s:=@s[1];
  end;

  Result:=s
end;

function CmdToken(s: PChar; out nn: Integer): string;
var
  p: PChar; ch: char;
  sign: Boolean; t: tShortStr;
begin
  nn:=0; Result:=''; sign:=false;

  p:=s; while true do begin
    ch:=p[0]; if ch = #0 then Break else
    if ch = '-' then sign:=true else
    if ch = '+' then else
    if ch in ['0'..'9'] then
      nn:=(nn*10) + ord(ch) - ord('0')
    else
      Break;

    p:=@p[1]
  end;

  if sign then nn:=-nn;

  p:=StrSkip(p);
  if p[0] = '=' then begin
    p:=@p[1]; p:=StrSkip(p);
    if StrToken(t,p) <> nil then
    Result:=StrPas(t)
  end;

  StrCopy(s,p)
end;

procedure DosToAlfa(var a: alfa; const s: string);
var
  t: String;
begin
  t:=DosString(s); if length(t) > 8 then
  t[0]:=chr(8); Move(t[1],a,length(t))
end;

function WideStrLen(s: PWideChar): Integer;
begin
  Result:=0;
  if Assigned(s) then
  while Result < 4096 do
  if s[Result] = #0 then
  Break else Inc(Result)
end;

function WideStrEnd(s: PWideChar): PWideChar;
begin
  Result:=s;
  while Result-s < 256*2 do
  if Result[0] = #0 then Break
  else Result:=@Result[1]
end;

function WideStrComp(s1,s2: PWideChar): Integer;
var
  i: Integer; ch1,ch2: WideChar;
begin
  Result:=0;

  for i:=0 to 255 do begin
    ch1:=s1[i]; ch2:=s2[i];

    if ch1 = #0 then begin
      if ch2 <> #0 then Result:=-1; Break
    end else
    if ch2 = #0 then begin
      Result:=1; Break
    end else
    if ch1 < ch2 then begin
      Result:=-1; Break
    end else
    if ch1 > ch2 then begin
      Result:=1; Break
    end
  end
end;

function WideStrCopy(dst,src: PWideChar): PWideChar;
var
  len: Integer;
begin
  Result:=nil; dst[0]:=#0;

  len:=Min(255,WideStrLen(src));

  if len > 0 then begin
    Move(src^,dst^,len*2);
    dst[len]:=#0; Result:=dst
  end
end;

function WideStrLCopy(dst,src: PWideChar; MaxLen: Cardinal): PWideChar;
var
  len: Integer;
begin
  Result:=nil; dst[0]:=#0;

  len:=Min(MaxLen,WideStrLen(src));

  if len > 0 then begin
    Move(src^,dst^,len*2);
    dst[len]:=#0; Result:=dst
  end
end;

function CharToWideChar(Ch: Char): WideChar;
var
  w: WideString;
begin
  Result:=WideChar(Ch); w:=Ch;
  if Length(w) = 1 then Result:=w[1]
end;

function WideCharPack(Ch: WideChar): WideChar;
var
  _Ch: Char; s: AnsiString;
begin
  Result:=Ch; s:=Ch; if Length(s) = 1 then
  if CharToWideChar(s[1]) = Ch then Result:=WideChar(_Ch)
end;

function WideStrAnsi(dst,src: PWideChar; MaxLen: Cardinal): PWideChar;
var
  Ch: WideChar; I: Integer;
begin
  Result:=dst;

  for I:=1 to MaxLen do begin
    Ch:=src[0]; if Ch = #0 then Break;
    if (Ch >= #128) and (Ch <= #255) then
    Ch:=CharToWideChar(Char(Ch)); dst[0]:=Ch;
    src:=@src[1]; dst:=@dst[1]
  end; dst[0]:=#0
end;

function WideStrPack(dst,src: PWideChar; MaxLen: Cardinal): PWideChar;
var
  i: int; ch: WideChar;
begin
  Result:=dst;

  for i:=1 to MaxLen do begin
    ch:=src^; if ch = #0 then Break;
    dst^:=WideCharPack(ch);
    Inc(src); Inc(dst)
  end; dst^:=#0
end;

function WideStrToStr(dst: PChar; src: PWideChar; MaxLen: Cardinal): PChar;
var
  Ch: WideChar; i: Integer;
begin
  Result:=dst; StrCopy(dst,'');

  for i:=1 to MaxLen do begin
    Ch:=src[0]; src:=@src[1];
    if Ch = #0 then Break;

    Ch:=WideCharPack(Ch);
    if ord(Ch) > 255 then begin
      Result:=nil; Break
    end;

    dst[0]:=Char(Ch); dst:=@dst[1]
  end;

  dst[0]:=#0
end;
                          
function xStrToWideChar(src: PChar; dst: PWideChar): PWideChar;
var
  i,ch: int; si: PBytes; di: PWords;
begin
  Result:=nil;

  if src[0] <> #0 then begin

    si:=Pointer(src); di:=Pointer(dst);

    for i:=1 to 255 do begin
      ch:=si[0]; if ch = 0 then Break;
      di[0]:=ch; si:=@si[1]; di:=@di[1]
    end;

    di[0]:=0; Result:=dst
  end
end;

function xStringToWideChar(const src: string; dst: PWideChar): PWideChar;
var
  i,l: int; si: pbyte; di: pword;
begin
  Result:=nil;

  l:=length(src);
  if l > 0 then begin
    si:=@src[1]; di:=Pointer(dst);
    for i:=1 to l do begin
      di^:=si^; Inc(si); Inc(di);
    end; di^:=0; Result:=dst
  end
end;

function xWideCharToString(s: PWideChar): string;
var
  i: Integer; ch: WideChar;
begin
  Result:='';

  for i:=0 to 255 do begin
    ch:=s[i]; if ch = #0 then Break;
    Result:=Result + chr(ord(ch) and 255)
  end
end;

function xWideCharToString1(s: PWideChar; MaxLen: int): WideString;
var
  i,n: int; ch: WideChar;
begin
  Result:='';

  n:=MaxLen;
  if n = 0 then n:=255;

  for i:=0 to n do begin
    ch:=s[i]; if ch = #0 then Break;
    Result:=Result + ch
  end
end;

function xWideStrToString(const s: WideString): String;
var
  i,len: Integer;
begin
  len:=Min(255,length(s));
  Result:=''; for i:=1 to len do
  Result:=Result + chr(ord(s[i]) and 255)
end;

function StrHex(Dst: PChar; Buf: PBytes; Len: Integer): PChar;
var
  I,Ax: Integer; si: PBytes; di: PChar;
begin
  Result:=nil; StrCopy(Dst,'');

  if Len > 0 then begin
    si:=Buf; di:=Dst;
    for I:=1 to Len do begin
      Ax:=si[0]; si:=@si[1];
      di[0]:=cHex[Ax shr 4]; di:=@di[1];
      di[0]:=cHex[Ax and 15]; di:=@di[1];
    end;

    di[0]:=#0; Result:=Dst
  end
end;

function StrItem(It,Str: PChar; Index: Integer): PChar;
var
  I,Len: Integer; P,Q: PChar;
begin
  Result:=nil; xStrCopy(It,'');

  P:=Str;
  for I:=0 to Index do begin
    Q:=StrScan(P,#10);
    if Q <> nil then Len:=Q-P
    else Len:=StrLen(P);

    Len:=Min(255,Len);

    if I = Index then if Len > 0 then
    Result:=StrLCopy(It,P,Len);

    if Q = nil then P:=StrEnd(P)
    else P:=@Q[1]
  end
end;

function HexString(Buf: PBytes; Len: Integer): WideString;
var
  si,di: Integer; al: Byte;
begin
  SetLength(Result,Len + Len); di:=0;

  if Length(Result) = Len+Len then
  for si:=0 to Len-1 do begin al:=Buf[si];
    Inc(di); Result[di]:=WideChar(cHex[al shr 4]);
    Inc(di); Result[di]:=WideChar(cHex[al and 15]);
  end
end;

function StrToHex(Str: PChar; out v: DWord): Boolean;
var
  al: Integer; p: PChar; ch: Char;
begin
  Result:=false; v:=0;

  p:=Str;
  while p[0] <> #0 do begin ch:=p[0];

    if (ch >= '0') and (ch <= '9') then
      al:=ord(ch)-ord('0')
    else
    if (ch >= 'A') and (ch <= 'F') then
      al:=10+ord(ch)-ord('A')
    else
    if (ch >= 'a') and (ch <= 'f') then
      al:=10+ord(ch)-ord('a')
    else begin
      Result:=false; v:=0; Break;
    end;

    v:=(v shl 4) + al; p:=@p[1];
    Result:=true
  end;
end;

function HexValue(Str: PChar): DWord;
var
  al: Integer; p: PChar; ch: Char;
begin
  Result:=0;

  p:=Str;
  while p[0] <> #0 do begin ch:=p[0];

    if (ch >= '0') and (ch <= '9') then
      al:=ord(ch)-ord('0')
    else
    if (ch >= 'A') and (ch <= 'F') then
      al:=10+ord(ch)-ord('A')
    else
    if (ch >= 'a') and (ch <= 'f') then
      al:=10+ord(ch)-ord('a')
    else
      Break;

    Result:=(Result shl 4) + al;

    p:=@p[1]
  end;
end;

function HexPack(const S: WideString;
                 Buf: PBytes; Size: Integer): Integer;
var
  i,k,al,bl,len: Integer;
  ch: Widechar;
begin
  Result:=0; k:=0; bl:=0;

  len:=Length(S);
  for I:=1 to len do begin
    ch:=S[I];

    if (ch >= '0') and (ch <= '9') then
      al:=ord(ch)-ord('0')
    else
    if (ch >= 'A') and (ch <= 'F') then
      al:=10+ord(ch)-ord('A')
    else
      Break;

    bl:=(bl shl 4) + al;
    Inc(k); if k = 2 then begin
      if Result < Size then begin
        Buf[Result]:=bl; Inc(Result)
      end;

      k:=0; bl:=0
    end
  end
end;

function HexPoly(const S: WideString;
                 lp: PLLine; lp_Max: Integer): Integer;
var
  len: Integer;
begin
  Result:=-1; lp.N:=-1;

  len:=SizeOf(TPoint) * lp_Max;

  if Length(S) div 2 < len then
  with lp^ do begin
    len:=HexPack(S,@Pol,len);
    N:=pred(len div SizeOf(TPoint));
    Result:=N
  end
end;

function HexStrPack(Str: PChar; Dst: PBytes;
                    Size: Integer): PChar;
var
  i,k,al,bl: Integer; p: PChar; ch: Char;
begin
  i:=0; k:=0; bl:=0;

  p:=Str;
  while p[0] <> #0 do begin
    ch:=p[0];

    if ch in ['0'..'9'] then
      al:=ord(ch)-ord('0')
    else
    if ch in ['A'..'F'] then
      al:=10+ord(ch)-ord('A')
    else
      Break;

    bl:=(bl shl 4) + al;
    Inc(k); if k = 2 then begin
      if i < Size then begin
        dst[i]:=bl; Inc(i)
      end;

      k:=0; bl:=0
    end;

    p:=@p[1]
  end;

  Result:=p
end;

function StrToArray(Str,Mask: PChar;
                    V: PIntegers; Max: Integer): Integer;
var
  p1,p2: PChar; ch1,ch2: Char;
  ax,bx: Integer; ok: Boolean;
begin
  Result:=0; if Max > 0 then
  FillChar(V^,Max*Sizeof(Integer),0);

  p1:=Str; p2:=Mask; ok:=true;

  ax:=0; bx:=0;
  while (p1[0] <> #0) and
        (p2[0] <> #0) do begin
    ch1:=p1[0]; p1:=@p1[1];
    ch2:=p2[0]; p2:=@p2[1];

    if ch2 = 'X' then begin
      ok:=ch1 in ['0'..'9'];
      if ok then begin
        ax:=ax*10 + ord(ch1) - ord('0');
        Inc(bx)
      end
    end
    else begin
      if bx > 0 then begin
        if Result < Max then begin
          V[Result]:=ax; Inc(Result)
        end; ax:=0; bx:=0
      end;

      if ch2 = '?' then
      else
        ok:=ch1 = ch2;
    end;

    if not ok then begin
      Result:=0; Break
    end
  end
end;

function StrToCrcs(Crc: PChar): Int64;
var
  i,len,al: Integer; ch: Char;
begin
  Result:=0; len:=Strlen(Crc);

  for i:=0 to len-1 do begin
    ch:=Crc[i];

    if ch in ['0'..'9'] then
      al:=ord(ch)-ord('0')
    else
    if ch in ['A'..'F'] then
      al:=10+ord(ch)-ord('A')
    else
      Break;

    Result:=(Result shl 4) + al;
  end
end;

function PackIP(v1,v2,v3,v4: DWord): DWord;
begin
  Result:=(v1 shl 24) or (v2 shl 16) or (v3 shl 8) or v4
end;

procedure UnpackIP(ip: DWord; out v1,v2,v3,v4: DWord);
begin
  v4:=ip and 255; ip:=ip shr 8;
  v3:=ip and 255; ip:=ip shr 8;
  v2:=ip and 255; ip:=ip shr 8;
  v1:=ip and 255; ip:=ip shr 8;
end;

function IPStr(ip: DWord): String;
var
  v1,v2,v3,v4: DWord;
begin
  UnpackIP(ip, v1,v2,v3,v4);
  Result:=Format('%d.%d.%d.%d',[v1,v2,v3,v4])
end;

procedure Init;
var
  i: int; s: TShortstr;
begin
  max_Single.f:=MaxSingle;

  for i:=1 to 12 do
  if StrResA(s,i) > 0 then
  rus_month[i]:=StrLPas(s,15);
end;

procedure Done;
begin
  max_Single.f:=MaxSingle;
end;

initialization Init;
finalization Done;

end.

