unit wstrings; interface {$H-}

uses
  cwstring,otypes;

function StrCopyW(Dest: PWideChar; const Source: PWideChar; MaxLen: int): PWideChar;
function StrPCopyW(Dest: PWideChar; const Source: WideString; MaxLen: int): PWideChar;

function StrLenW(Str: PWideChar): int;
function StrPasWW(Str: PWideChar): WideString;
function StrPasAW(Str: PAnsiChar): WideString;

function StrCopyWA(Dest: PChar; DstLen: int;
                   Src: PWideChar; SrcLen: int): PChar;

function StrPasUTF8(s: PWideChar): String;

function WinToUnicode(Src: PChar; Dest: PWideChar; MaxLen: int): PWideChar;
function WinToUnicodep(const Src: String; Dest: PWideChar): PWideChar;

implementation

uses
  Math,SysUtils;

function StrCopyW(Dest: PWideChar; const Source: PWideChar; MaxLen: int): PWideChar;
var
  Src: PWideChar;
begin
  Result:=Dest;

  if Assigned(Dest) and (MaxLen > 0) then begin
    Src:=Source;
    if Assigned(Src) then
    while (MaxLen > 0) and (Src^ <> #0) do begin
      Dest^:=Src^; Inc(Src); Inc(Dest); Dec(MaxLen);
    end;

    Dest^:=#0;
  end;
end;

function StrPCopyW(Dest: PWideChar; const Source: WideString; MaxLen: int): PWideChar;
begin
  Result:=StrCopyW(Dest,PWideChar(Source), Min(MaxLen,Length(Source)))
end;

function StrLenW(Str: PWideChar): int;
var
  i: int;
begin
  Result:=0;
  for i:=1 to 1024*8 do begin
    if Str^ = #0 then Break;
    Inc(Str); Inc(Result)
  end
end;

function StrPasWW(Str: PWideChar): WideString;
begin
  Result:=Str
end;

function StrPasAW(Str: PAnsiChar): WideString;
begin
  Result:=Str
end;

function StrCopyWA(Dest: PChar; DstLen: int;
                   Src: PWideChar; SrcLen: int): PChar;
var
  i,l: int;
begin
  Result:=Dest;

  l:=SrcLen;
  if l >= DstLen then l:=DstLen-1;

  for i:=0 to l-1 do begin
    Dest^:=Src^; Inc(Dest); Inc(Src)
  end;

  Dest^:=#0
end;

function StrPasUTF8(s: PWideChar): String;
var
  w: WideString;
begin
  w:=s;

  if Length(w) > 127 then
  SetLength(w,127);

  Result:=UTF8Encode(w)
end;

const
  cp1251: Array[$80..$ff] of int =
   ($0402,                // 80
    $0403,
    $201A,
    $0453,
    $201E,
    $2026,
    $2020,
    $2021,
    $20AC,
    $2030,
    $0409,
    $2039,
    $040A,
    $040C,
    $040B,
    $040F,
    $0452,                // 90
    $2018,
    $2019,
    $201C,
    $201D,
    $2022,
    $2013,
    $2014,
    $98,
    $2122,
    $0459,
    $203A,
    $045A,
    $045C,
    $045B,
    $045F,
    $00A0,                // a0
    $040E,
    $045E,
    $0408,
    $00A4,
    $0490,
    $00A6,
    $00A7,
    $0401,
    $00A9,
    $0404,
    $00AB,
    $00AC,
    $00AD,
    $00AE,
    $0407,
    $00B0,                // b0
    $00B1,
    $0406,
    $0456,
    $0491,
    $00B5,
    $00B6,
    $00B7,
    $0451,
    $2116,
    $0454,
    $00BB,
    $0458,
    $0405,
    $0455,
    $0457,
    $0410,                // c0
    $0411,
    $0412,
    $0413,
    $0414,
    $0415,
    $0416,
    $0417,
    $0418,
    $0419,
    $041A,
    $041B,
    $041C,
    $041D,
    $041E,
    $041F,
    $0420,                // d0
    $0421,
    $0422,
    $0423,
    $0424,
    $0425,
    $0426,
    $0427,
    $0428,
    $0429,
    $042A,
    $042B,
    $042C,
    $042D,
    $042E,
    $042F,
    $0430,                // e0
    $0431,
    $0432,
    $0433,
    $0434,
    $0435,
    $0436,
    $0437,
    $0438,
    $0439,
    $043A,
    $043B,
    $043C,
    $043D,
    $043E,
    $043F,
    $0440,                // f0
    $0441,
    $0442,
    $0443,
    $0444,
    $0445,
    $0446,
    $0447,
    $0448,
    $0449,
    $044A,
    $044B,
    $044C,
    $044D,
    $044E,
    $044F);

function WinToUnicode(Src: PChar; Dest: PWideChar; MaxLen: int): PWideChar;
var
  i,ch: int;
begin
  Result:=Dest;

  if not rus_interface then
    StringToWideChar(Src,Dest,255)
  else
    for i:=1 to MaxLen do begin
      ch:=int(Src^);
      if ch = 0 then Break;
      if ch >= $80 then ch:=cp1251[ch];
      Dest^:=WideChar(ch); Inc(Dest)
    end;

  Dest^:=#0;
end;

function WinToUnicodep(const Src: String; Dest: PWideChar): PWideChar;
var
  i,n,ch: int;
begin
  Result:=Dest;

  n:=Length(Src);

  if not rus_interface then
    StringToWideChar(Src,Dest,255)
  else
    for i:=1 to n do begin
      ch:=int(Src[i]);
      if ch >= $80 then ch:=cp1251[ch];
      Dest^:=WideChar(ch); Inc(Dest)
    end;

  Dest^:=#0;
end;

end.