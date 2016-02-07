unit dmw_fnt; interface {$H-}

uses
  Windows,Classes,
  Contnrs,Math,LCLType,
  otypes,idib,xline,xdib,
  xvirts,dmw_vgm,
  freetypep;

const
  Fnt_Max = 64;

  Fonts_Edit: Boolean = false;

type
  TFntHeights = array[0..63,0..63] of Float;
  TFntCounters = array[0..63,0..63] of Integer;
  TFntIndexes = array[0..63,0..63] of TPoint;
  TFntNames = array[0..63] of String[31];

  TOnTextOut = procedure(const p1,p2: TGauss;
                         h: double; s: PWideChar) of object;

  TFonts = class;

  TTrueTypeFont = class

    constructor Create(AOwner: TFonts;
                       const AFont: TLogFont);

    destructor Destroy; override;
    procedure Clear;

    function This(const AFont: TLogFont): Boolean;

    function OutText(v: PPoly; up: Integer;
                     s: PWideChar): Integer;

    procedure PolyText(lp: PLLine; cl: Integer;
                       s: PWideChar);

    procedure PolyAttr(lp: PLLine; cl: Integer;
                       ds: Double; s: PWideChar);

    function TextExtent(s: PWideChar): TSize;
    function CharExtent(ch: WideChar): TSize;

    function blank_space(dist: Double; s: PWideChar): Double;

    function BeginDraw(ADrawDC: HDC; cl: Integer): Boolean;
    procedure EndDraw;

  private
    fOwner: TFonts;
    fDC,fDrawDC: HDC;
    fFont: TLogFont;

    fHeights: Array[0..3] of Integer;

    fOldFont: THandle;
    fMatrix: Real3x3;
    fg_Rect: GOrient;

    fAngleIndex: Integer;
    fAngles: Array[0..359] of THandle;

    fOnRect: TNotifyEvent;
    fOnText: TOnTextOut;

    function SetAngle(fi: Integer): Boolean;

    function __charSize(ch: WideChar): TSize;

  public
    property DrawDC: HDC write fDrawDC;
    property Font: TLogFont read fFont;
    property g_Rect: GOrient read fg_Rect;
    property OnRect: TNotifyEvent write fOnRect;
    property OnText: TOnTextOut write fOnText;
  end;

  TTrueTypes = class(TObjectList)
    constructor Create(AOwner: TFonts);
    function GetFont(AName: PChar; AHeight: Integer): TTrueTypeFont;
  private
    fOwner: TFonts;
    fttfRatio: Float;
  end;

  TFreeTypeFonts = class
    constructor Create(AOwner: TFonts;
                       Adraw: IDrawText);

    destructor Destroy; override;

    function SetFont(AName: PChar; AHeight: int): Boolean;

    function GetExtent(s: PWideChar): TPoint;

    function OutText(v: PPoly; up: int; s: PWideChar): int;
    procedure PolyText(lp: PLLine; cl: int; s: PWideChar);

    procedure PolyAttr(lp: PLLine; cl: int;
                       ds: Double; s: PWideChar);

  private
    fOwner: TFonts;
    fdraw: IDrawText;
    fttfRatio: float;

    fName: TShortstr;
    fHeight: int;
    lfHeight: int;
    fEnabled: bool;

    fHeights: Array[0..3] of int;

    fCanvas: ICanvas;

    fColor: int;
    fAngle: int;

    fMatrix: Real3x3;
    fg_Rect: GOrient;

    fOnRect: TNotifyEvent;
    fOnText: TOnTextOut;

    function blank_space(dist: double; s: PWideChar): double;

  public
    property g_Rect: GOrient read fg_Rect;

    property Canvas: ICanvas write fCanvas;
    property Color: int write fColor;

    property OnRect: TNotifyEvent write fOnRect;
    property OnText: TOnTextOut write fOnText;
  end;

  TFonts = class(TVgmBank)
    MinH,MaxH,Comp: double;

    constructor Create(cash: int; Adraw: IDrawText);
    destructor Destroy; override;

    procedure sync_folder;

    function Open_font(cl: int): Boolean;

    function StrGraphics(s: PChar; cl: Integer): PChar;

    function Get_char_bound(ch,cl: Integer;
                            out lt,rb: TGauss): TGauss;

    function Scale_Text(cl: Integer; k: Double;
                        h1,h2: Float): Double;

    function Display_text(cl: Integer; k: Double;
                          out h: Float): Boolean;

    function Display_text1(cl: Integer; k: Double;
                           out h: Float; out ansi: int): Boolean;

    function Out_Scale(cl: Integer; k: Double): Double;

    function Font_Aspect: Double;

    function blank_space(s: PWideChar;
                         dist,kz: Double): Double;

    procedure Out_lWide(DC: XCanvas; lp: PLLine;
                        cl: Integer; k: Double;
                        s: PWideChar);

    procedure Out_Attr(DC: XCanvas; lp: PLLine;
                       cl: Integer; k,ds: Double;
                       s: PWideChar);

    procedure Line_Attr(DC: XCanvas; lp: PLLine;
                        cl: Integer; k,dx,ds: Double;
                        s: PWideChar);

    procedure Out_Wide(DC: XCanvas;
                       x,y,h,cl: Integer;
                       s: PWideChar);

    procedure Out_Text(DC: XCanvas;
                       x,y,h,cl: Integer;
                       s: PChar);

    function Text_Extent(cl: Integer; k: double; s: PWideChar): TSize;

    function Height_Text(cl: Integer; k: double): double;

    function Attr_Width(cl: Integer; k: double; s: PWideChar): Double;

    function xText_Width(cl: Integer; k: double; s: PWideChar): double;
    function Text_Width(cl: Integer; k: double; s: PChar): double;

    function xWidth_Text(h: double; s: PWideChar): double;
    function Width_Text(h: double; const s: string): double;
    function Width_Wide(h: double; const s: WideString): double;

    function Up_Text(h: double): double;
    function Dn_Text(h: double): double;

    procedure Vgm_Skip(DC: XCanvas; const p: TPoint; cl,i: int); override;
    procedure Virt_reset; virtual;

  private
    fTrueTypes: TFreeTypeFonts;

    fFntIndex: Integer;

    fxDC: XCanvas;
    fxBlankTyp: Integer;
    fxBlankTest: Integer;
    fxBlankColor: Integer;

    fonlyFreeType: bool;

    fFolder: TShortstr;

    function Open_vgt(Ind: Integer): Boolean;
    function Scale_vgt(cl: Integer; k: Double): Double;

    function Open_ttf(fn: PChar; cl: int; k: double): Boolean;

    procedure ttf_Rect(Sender: TObject);

    procedure vgt_Attr(DC: XCanvas; lp: PLLine;
                       cl: Integer; k,ds: Double;
                       s: PWideChar);

  protected
    fxBlankTest1: Integer;

  public
    property xDC: XCanvas read fxDC;
    property xBlankTyp: Integer read fxBlankTyp
                                write fxBlankTyp;

    property xBlankColor: Integer read fxBlankColor
                                  write fxBlankColor;

    property xBlankTest: Integer write fxBlankTest;
  end;

  TExportFonts = class(TFonts)
    function exp_Symbols(ch: WideChar): Boolean; virtual;

    procedure exp_str(const p1,p2: TGauss;
                      h: double; const s: WideString); virtual;

    procedure exp_point(i: Integer; const p: TGauss); virtual;
    procedure exp_line(const p1,p2: TGauss); virtual;

    procedure exp_lwide(lp: PLLine;
                        cl: Integer; k: Double;
                        s: PWideChar);

    procedure exp_ltext(lp: PLLine;
                        cl: Integer; k: Double;
                        s: PChar);

  private
    fIsMif: Longbool;
    fIsSxf: Longbool;
  public
    property IsMif: Longbool write fIsMif;
    property IsSxf: Longbool write fIsSxf;
  end;

  TDrawFonts = class(TFonts)

    constructor Create(Adraw: IDrawText);
    destructor Destroy; override;

    procedure Vgm_Start(DC: XCanvas); override;

    procedure Vgm_Poly(DC: XCanvas; lp: pLPoly;
                       N, Cl,Tag: Integer); override;

    procedure Vgm_Close(DC: XCanvas); override;

  private
    x_LPoints: PLLine;
    x_Counters: PIntegers;
    x_cnt: Integer;
  end;

  TFontsRect = class(TFonts)
    constructor Create(lp: PLLine; lp_Max: Integer);
    procedure Clear_bound;

    procedure Vgm_Poly(DC: XCanvas; lp: PLPoly;
                       N, Cl,Tag: Integer); override;

    procedure Virt_Rect(Sender: TObject); override;
    procedure Virt_reset; override;

  private
    flt,frb: TPoint;

    fbp: PLLine;
    fPolyInd: Integer;
    fPolyMax: Integer;
  public
    property lt: TPoint read flt;
    property rb: TPoint read frb;
  end;

  TFontsRect1 = class(TFontsRect)
    constructor Create;
  private
    fbuf: TLLine;
  public
    property buf: TLLine read fbuf;
  end;

  TFontsBound = class(TFonts)
    procedure Vgm_Poly(DC: XCanvas; lp: PLPoly;
                       N, Cl,Tag: Integer); override;
  private
    flt,frb: TPoint; Count: Integer;
  public
    property lt: TPoint read flt;
    property rb: TPoint read frb;
  end;

function Get_Font_Index(cl: Integer): Integer;
function Set_Font_Index(cl,ind: Integer): Integer;

function Get_Height_Index(cl: Integer): Integer;
function Set_Height_Index(cl,ind: Integer): Integer;

procedure obj_Load_fonts(obj: TVirtMem);
procedure obj_Save_fonts(obj: TVirtMem);

procedure push_fonts_params(out buf1: TFntHeights;
                            out buf2: TFntNames);

procedure pop_fonts_params(const buf1: TFntHeights;
                           const buf2: TFntNames);

function fnt_obj_to_obj(const oldHeights: TFntHeights;
                        const oldNames: TFntNames;
                        cl: Integer): Integer;

function Fonts_Height(cl: Integer): Float;

function font_HeightOf(fnt: Integer; h: Float): Integer;

function Get_Fnt_Count(chr,ind: Integer): Integer;
function Inc_Fnt_Count(cl: Integer): Integer;
function Dec_Fnt_Count(cl: Integer): Integer;

function Get_Font_Height(chr,i: Integer): float;
procedure Set_Font_Height(chr,i: Integer; h: float);

function Get_Font_FName(chr: Integer): string;
procedure Set_Font_FName(chr: Integer; fn: string);

function Str_font_FName(Path: PChar; cl: int): PChar;

function fnt_cl_Transit(const fnt_Ind: TFntIndexes;
                        cl: Integer): Integer;

function Is_fonts_order(out fnt_Ind: TFntIndexes;
                        pack: Boolean): Boolean;

function Is_wide_vgt(Path: PChar): Boolean;

function TtfFlags(Str: PChar): Integer;

implementation

uses
  wstrings,
  SysUtils,LCLIntf,
  convert,ofiles,xddw,xgdi,
  xpoly,xpoly1;

const
  fnt_lab: alfa = 'fnt_0002';

  LPointsMax = 4096;
  CountersMax = 64;

type
  WideChar2 = array[0..1] of WideChar;

  TVgtNames = array[0..63] of String[15];

var
  Fnt_Heights: TFntHeights;
  Fnt_Counters: TFntCounters;
  Fnt_FNames: TFntNames;

function Get_Font_Index(cl: Integer): Integer;
begin
  Result:=(cl shr 12) and 15;
  Inc(Result,cl and $30)
end;

function Set_Font_Index(cl,ind: Integer): Integer;
begin
  with tlong(cl) do begin
    b[1]:=(b[1] and $0F) or ((ind and $F) shl 4);
    b[0]:=(b[0] and $CF) or (ind and $30);
  end; Result:=cl
end;

function Get_Height_Index(cl: Integer): Integer;
begin
  Result:=(cl shr 8) and 15;
  Inc(Result,(cl shr 2) and $30)
end;

function Set_Height_Index(cl,ind: Integer): Integer;
begin
  with tlong(cl) do begin
    b[1]:=(b[1] and $F0) or (ind and $F);
    b[0]:=(b[0] and $3F) or ((ind and $30) shl 2)
  end; Result:=cl
end;

function h_Index(hp: PFloats; n: Integer; h: Float): Integer;
var
  i: Integer;
begin
  Result:=-1; for i:=0 to n-1 do
  if hp[i] = h then begin
    Result:=i; Break
  end
end;

function fnt_cl_Transit(const fnt_Ind: TFntIndexes;
                        cl: Integer): Integer;
var
  fnt,ind, h_lo,h_hi: Integer; p: TPoint;
begin
  fnt:=Get_Font_Index(cl);
  ind:=Get_Height_Index(cl);

  p:=fnt_Ind[fnt,ind];

  h_lo:=p.y and 15;
  h_hi:=p.y shr 4;

  cl:=cl and $F03F;

  cl:=cl or (h_lo shl 8) or (h_hi shl 6);
  Result:=cl
end;

function Is_fonts_order(out fnt_Ind: TFntIndexes;
                        pack: Boolean): Boolean;
var
  i,j, j1,j2, bx,cx: Integer; h: Float;
  hv: TFntHeights; hp: PFloats; ch: TChannel;
begin
  Result:=false;

  for i:=0 to Fnt_Max-1 do
  for j:=0 to Fnt_Max-1 do
  fnt_Ind[i,j]:=Point(i,j);

  hv:=Fnt_Heights;

  for i:=0 to Fnt_Max-1 do begin

    cx:=0; hp:=@hv[i,0];

    for j:=0 to Fnt_Max-1 do begin
      h:=hv[i,j];

      bx:=h_Index(hp,cx,h);

      if bx < 0 then begin
        bx:=cx; hv[i,cx]:=h; Inc(cx)
      end;

      fnt_Ind[i,j].y:=bx
    end;

    for j:=cx to Fnt_Max-1 do
    hv[i,j]:=0;

    for j:=0 to Fnt_Max-1 do ch[j]:=j;

    for j1:=0 to cx-2 do
    for j2:=j1+1 to cx-1 do

    if hp[j1] > hp[j2] then begin
      fSwap(hp[j1],hp[j2]);
      bSwap(ch[j1],ch[j2]);
    end;

    for j:=0 to cx-1 do
    with fnt_Ind[i,j] do y:=ch[y]
  end;

  for i:=0 to Fnt_Max-1 do
  for j:=0 to Fnt_Max-1 do
  with fnt_Ind[i,j] do
  if (x <> i) or (y <> j) then
  begin Result:=true; Break end;

  if pack then Fnt_Heights:=hv
end;

function Get_Fnt_Count(chr,ind: Integer): Integer;
begin
  Result:=Fnt_Counters[chr and 63,ind and 15]
end;

function Inc_Fnt_Count(cl: Integer): Integer;
var
  Row,Col: Integer;
begin
  Row:=Get_Font_Index(cl);
  Col:=Get_Height_Index(cl);
  Result:=Fnt_Counters[Row,Col]+1;
  Fnt_Counters[Row,Col]:=Result
end;

function Dec_Fnt_Count(cl: Integer): Integer;
var
  Row,Col: Integer;
begin
  Row:=Get_Font_Index(cl);
  Col:=Get_Height_Index(cl);
  Result:=Max(0,Fnt_Counters[Row,Col]-1);
  Fnt_Counters[Row,Col]:=Result
end;

function Get_Font_Height(chr,i: Integer): float;
begin
  Result:=Fnt_Heights[chr and 63,i and 63]
end;

procedure Set_Font_Height(chr,i: Integer; h: float);
begin
  Fnt_Heights[chr and 63,i and 63]:=h;
  Fonts_Edit:=true
end;

function Get_Font_FName(chr: Integer): string;
var
  fn: TShortStr;
begin
  StrPCopy(fn,Fnt_FNames[chr and 63]);
  Result:=StrPas(StrLower(fn))
end;

function font_Indexof(Name: PChar): Integer;
var
  i: Integer; fn: TShortstr;
begin
  Result:=0;
  for i:=0 to 63 do begin
    StrPCopy(fn,Fnt_FNames[i]);
    if StrIComp(fn,Name) = 0 then begin
      Result:=i; Break
    end
  end
end;

procedure Set_Font_FName(chr: Integer; fn: string);
begin
  if length(fn) > 31 then fn[0]:=#8;
  Fnt_FNames[chr and 63]:=fn;
  Fonts_Edit:=true
end;

function Str_font_FName(Path: PChar; cl: int): PChar;
var
  f: int; fn,dir: TShortStr;
begin
  Result:=nil;

  f:=Get_Font_Index(cl);
  StrPCopy(fn,Fnt_FNames[f]);

  if fn[0] = '*' then
    Result:=StrCopy(Path,fn)
  else begin
    if not UsrDir1(dir,'/fonts') then
    UsrDir(dir,'/obj');

    xStrPath(Path,dir,fn,'.vgt');

    if FileExist(Path) then
      Result:=Path
    else
      StrCopy(Path,'')
  end
end;

function Is_wide_vgt(Path: PChar): Boolean;
var
  vgt: TFonts;
begin
  Result:=false;

  vgt:=TFonts.Create(0,nil);
  try
    if vgt.Open(Path) then
    Result:=vgt.t_cnt > 256
  finally
    vgt.Free
  end
end;

procedure Init_Heights; var i,j: Integer;
begin
  for j:=0 to 63 do for i:=0 to 63 do
  Fnt_Heights[j,i]:=1.1 + i*0.1;

  FillChar(Fnt_Counters,SizeOf(Fnt_Counters),0)
end;

procedure Init_FNames;
var
  i: Integer;
begin
  for i:=0 to 63 do
  Fnt_FNames[i]:='rus'+IntToStr(i)
end;

function Fonts_Height(cl: Integer): Float;
begin
  Result:=Fnt_Heights[Get_Font_Index(cl),
                      Get_Height_Index(cl)]
end;

function font_HeightOf(fnt: Integer; h: Float): Integer;
var
  fp: PFloats; i: Integer; r,t: Float;
begin
  Result:=0; fp:=@Fnt_Heights[fnt];

  r:=Abs(h-fp[0]);
  for i:=1 to 63 do begin
    t:=Abs(h-fp[i]); if t < r then
    begin Result:=i; r:=t end
  end
end;

function obj_Load_buf(const fnt_lab: alfa;
                      obj: TVirtMem; ptr: Integer;
                      var buf; BufLen: Integer): Boolean;
var
  ind,nxt: longint; lab: alfa;
begin
  Result:=false;
  ind:=obj.vm_Long(ptr);

  if ind > 0 then begin
    nxt:=obj.vm_Load(ind,lab,Sizeof(lab));

    if lab = fnt_lab then begin
      obj.vm_Load(nxt,buf,BufLen);
      Result:=true
    end else

    obj.vm_Load(ind,buf,BufLen div 4)
  end
end;

procedure obj_Save_buf(obj: TVirtMem; ptr: Integer;
                       var buf; BufLen: Integer);
var
  ind,nxt: Integer; lab: alfa;
begin
  ind:=obj.vm_Long(ptr);
  if ind > 0 then begin
    nxt:=obj.vm_Load(ind,lab,Sizeof(lab));

    if lab = fnt_lab then
      obj.vm_Store(nxt,buf,BufLen)
    else
      ind:=0
  end;

  if ind = 0 then begin
    lab:=fnt_lab;
    obj.vm_Store(ptr,obj.vm_Ind,4);
    obj.vm_Append(lab,Sizeof(lab));
    obj.vm_Append(buf,BufLen)
  end
end;

procedure obj_Load_fonts(obj: TVirtMem);
const
  old_lab: alfa = 'fnt_0001';

var
  i,j: int; h: double;
  old: array[0..63,0..15] of Float;
  s: string; vgt: TVgtNames;
begin
  Init_FNames; Init_Heights;
  FillChar(Fnt_Counters,SizeOf(Fnt_Counters),0);

  if obj.vm_Active then begin

    if not obj_Load_buf(fnt_lab,obj,12,Fnt_Heights,
                        SizeOf(Fnt_Heights)) then begin
      Init_Heights;

      Fillchar(old,Sizeof(old),0);
      if obj_Load_buf(old_lab,obj,12,old,
                      SizeOf(old)) then

      for i:=0 to 63 do for j:=0 to 15 do
      Fnt_Heights[i,j]:=old[i,j]
    end;

    if obj_Load_buf(fnt_lab,obj,8,vgt,SizeOf(vgt))
    or obj_Load_buf(old_lab,obj,8,vgt,SizeOf(vgt)) then

    for i:=0 to Fnt_Max-1 do begin
      s:=vgt[i];
      if Length(s) > 15 then SetLength(s,15);
      Fnt_FNames[i]:=AnsiLowerCase(s)
    end;

    for i:=0 to 63 do
    for j:=0 to 15 do begin
      h:=Fnt_Heights[i,j];
      h:=Max(-64,Min(100,h));
      Fnt_Heights[i,j]:=h
    end
  end
end;

procedure obj_Save_fonts(obj: TVirtMem);
var
  i: Integer; s: TShortStr; vgt: TVgtNames;
begin
  if obj.vm_Active then begin
    obj_Save_buf(obj,12,Fnt_Heights,SizeOf(Fnt_Heights));

    for i:=0 to Fnt_Max-1 do begin
      StrPLCopy(s,Fnt_FNames[i],15);
      vgt[i]:=Strpas(s)
    end;

    obj_Save_buf(obj,8,vgt,SizeOf(vgt));
  end
end;

procedure push_fonts_params(out buf1: TFntHeights;
                            out buf2: TFntNames);
begin
  buf1:=Fnt_Heights; buf2:=Fnt_FNames
end;

procedure pop_fonts_params(const buf1: TFntHeights;
                           const buf2: TFntNames);
begin
  Fnt_Heights:=buf1; Fnt_FNames:=buf2
end;

function fnt_obj_to_obj(const oldHeights: TFntHeights;
                        const oldNames: TFntNames;
                        cl: Integer): Integer;
var
  chr,ind: Integer; h: Float; fn: TShortstr;
begin
  chr:=Get_Font_Index(cl) and 63;
  ind:=Get_Height_Index(cl) and 63;
  h:=oldHeights[chr,ind];

  StrPCopy(fn,oldNames[chr]);
  chr:=font_Indexof(fn);

  ind:=font_HeightOf(chr,h);

  cl:=Set_Height_Index(cl,ind);
  cl:=Set_Font_Index(cl,chr);

  Result:=cl;
end;

function is_mix_wide(s: PWideChar): Boolean;
var
  i: Integer; ch: WideChar;
begin
  Result:=false;
  for i:=1 to 1024 do begin
    ch:=s[0]; s:=@s[1];
    if ch = #0 then Break;

    if ch < #255 then
    if Char(ch) in ['~','^','@','_'] then begin
      Result:=true; Break
    end
  end
end;

function txt_Clip(const lt,rb: TPoint;
                  v: PLPoly; w,h: Double): Boolean;
var
  a,b: TPoint; ih: Integer; dx,dy,r: Double;
begin
  Result:=false; a:=v[0]; b:=v[1];

  dx:=b.X-a.X; dy:=b.Y-a.Y;
  r:=Hypot(dx,dy);
  if r < 0.1 then begin
    dx:=1; dy:=0
  end
  else begin
    dx:=dx/r; dy:=dy/r;
  end;

  b.X:=a.X + Round(dx*w);
  b.Y:=a.Y + Round(dy*w);

  if a.X > b.X then iSwap(a.X,a.Y);
  if a.Y > b.Y then iSwap(a.Y,a.Y);

  ih:=Round(h);
  Dec(a.X,ih); Inc(b.X,ih);
  Dec(a.Y,ih); Inc(b.Y,ih);

  if (a.X < rb.X) and (b.X > lt.X) then
  if (a.Y < rb.Y) and (b.Y > lt.Y) then
  Result:=true
end;

function TtfFlags(Str: PChar): Integer;
var
  p: PChar; v,rc: Integer;
begin
  Result:=0;

  if Str[0] = '*' then
  StrCopy(Str,@Str[1]);

  p:=StrScan(Str,'*');
  if Assigned(p) then begin
    p[0]:=#0; p:=@p[1];
    val(p,v,rc); if rc = 0 then
    Result:=v
  end;
end;

constructor TTrueTypeFont.Create(AOwner: TFonts;
                                 const AFont: TLogFont);
begin
  inherited Create; fFont:=AFont;
  fOwner:=AOwner
end;

procedure TTrueTypeFont.Clear;
var
  i,h: Integer;
begin
  for i:=0 to 359 do begin
    h:=fAngles[i]; if h <> 0 then
    DeleteObject(h);
  end;

  Fillchar(fAngles,Sizeof(fAngles),0);
end;

function TTrueTypeFont.This(const AFont: TLogFont): Boolean;
begin
  Result:=false;
  if StrIComp(AFont.lfFaceName,fFont.lfFaceName) = 0 then
  if AFont.lfHeight = fFont.lfHeight then
  if AFont.lfWeight = fFont.lfWeight then
  if AFont.lfItalic = fFont.lfItalic then
  if AFont.lfPitchAndFamily = fFont.lfPitchAndFamily then
  Result:=true
end;

destructor TTrueTypeFont.Destroy;
begin
  Clear; inherited
end;

function TTrueTypeFont.SetAngle(fi: Integer): Boolean;
var
  fnt,old: HFont; tmp: TLogFont;
  Metrics: TTextMetric; h: Integer;
begin
  Result:=false;

  while fi < 0 do fi:=fi+360;
  while fi >= 360 do fi:=fi-360;
  fMatrix:=Rotate_3x3(-fi/180*Pi);

  fAngleIndex:=fi; fnt:=fAngles[fi];

  if fnt = 0 then begin
    tmp:=fFont;
    tmp.lfEscapement:=fi*10+1;
    tmp.lfOrientation:=fi*10;
    fnt:=CreateFontIndirect(tmp);
    fAngles[fi]:=fnt
  end;

  if fnt <> 0 then begin
    old:=SelectObject(fDC,fnt);

    if fOldFont = 0 then begin
      GetTextMetrics(fDC,Metrics);
      h:=Metrics.tmHeight;

      fHeights[0]:=h;
      fHeights[1]:=Round(h * 0.75);
      fHeights[2]:=Round(h * 0.5);
      fHeights[3]:=Round(h * 0.25);

      fOldFont:=old;
    end;

    Result:=true
  end
end;

function TTrueTypeFont.OutText(v: PPoly; up: Integer;
                               s: PWideChar): Integer;

function X_Plot(x,y: Double; w: Integer): tgauss;
begin
  Result.x:=x + fMatrix[1,1]*w;
  Result.y:=y - fMatrix[2,1]*w
end;

function Y_Plot(x,y,h: Double): tgauss;
begin
  Result.x:=x - h * fMatrix[1,2];
  Result.y:=y + h * fMatrix[2,2]
end;

function Y_Move(x,y,k: Double): TGauss;
begin
  Result:=Y_Plot(x,y,fFont.lfHeight*k)
end;

function IY_Move(x,y,k: Double): TPoint;
var
  p: TGauss;
begin
  p:=Y_Move(x,y,k);
  Result.X:=Round(p.x);
  Result.Y:=Round(p.y);
end;

procedure Vgm_Rect(const a: TGauss; w: Integer; k: Double);
var
  i: Integer; h: Float; b: TGauss;
begin
  h:=fHeights[1]*k;
  b:=X_Plot(a.x,a.y,w);

  fg_Rect[1]:=a;
  fg_Rect[0]:=Y_Plot(a.x,a.y,+h);
  fg_Rect[3]:=Y_Plot(b.x,b.y,+h);
  fg_Rect[2]:=b; fg_Rect[4]:=a;

  if Assigned(fOnRect) then
  fOnRect(Self)
end;

procedure TextEvent(px,py: Double; w: Integer; s: PWideChar);
var
  p1,p2: TGauss;
begin
  p1.x:=px; p1.y:=py;
  p2:=X_Plot(px,py,w);
  fOnText(p1,p2,fHeights[0],s)
end;

function __OutText(px,py: Double; s: PWideChar): Integer;
var
  ix,iy,l: int; sz: TSize;
  pc: PChar; t: String;
begin
  Result:=0;

  ix:=Round(px); iy:=Round(py);

  t:=StrPasUTF8(s);
  l:=Length(t);

  if l > 0 then begin
    pc:=@t[1];

    if fDrawDC <> 0 then
    TextOut(fDrawDC,ix,iy,pc,l);

    GetTextExtentPoint(fDC,pc,l,sz);

    if Assigned(fOnText) then
    TextEvent(px,py,sz.cx,s);

    Result:=sz.cx
  end;
end;

function OutMix(px,py,fi: Integer; s: PWideChar): Integer;
var
  i,dx,w,sp,tw: Integer; dn_k,up_k,k,r,u: Double;
  p,q,sq,org: tgauss; ch: WideChar; a,b: TPoint;
  buf: TWideStr; pop: Boolean;
begin
  Result:=0;

  p.x:=px; p.y:=py; q:=p; org:=p; sp:=0;

  dx:=0; w:=0; dn_k:=0; up_k:=0; k:=0;

  i:=0; while i < 256 do begin

    ch:=s[i]; Inc(i);
    if ch = #0 then Break;

    if ch = '~' then begin k:=k-4/3;
      p:=y_move(p.x,p.y,4/3); Dec(w,dx); dx:=0
    end else
    if ch = '^' then begin
      p:=y_move(q.x,q.y,-2/3);
      k:=k+2/3; dx:=0
    end else
    if ch = '@' then begin
      u:=AtoI(s[i],2)/100; Inc(i,2);
      p:=y_move(q.x,q.y,-u); k:=k+u; dx:=0
    end else
    if ch = '_' then begin
      a:=iy_move(p.x,p.y,4/3);
      b:=iy_move(q.x,q.y,4/3);

      if fDrawDC <> 0 then begin
        LCLIntf.MoveToEx(fDC,a.X,a.Y,nil);
        LCLIntf.LineTo(fDC,b.X,b.Y);
      end;

      if Assigned(fOnText) then
      fOnText(y_move(p.x,p.y,0.1),
              y_move(q.x,q.y,0.1),0,nil)

    end
    else begin
      if sp = 0 then sq:=q;
      buf[sp]:=ch; Inc(sp)
    end;

    if sp > 0 then begin
      if sp = 128 then
        pop:=true
      else begin
        ch:=s[i]; if ch < #255 then
        pop:=Char(ch) in ['~','^','@','_'];
      end;

      if pop then begin buf[sp]:=#0;
        tw:=__OutText(sq.x,sq.y,buf);
        Inc(dx,tw); Inc(w,tw); sp:=0;
        if Result < w then Result:=w
      end
    end;

    if k < dn_k then dn_k:=k;
    if k > up_k then up_k:=k;

    q.x:=p.x + fMatrix[1,1]*dx;
    q.y:=p.y - fMatrix[2,1]*dx
  end;

  if sp > 0 then begin
    buf[sp]:=#0; __OutText(sq.x,sq.y,buf)
  end;

  if dn_k < 0 then
  org:=y_move(org.x,org.y,-dn_k);

  Vgm_Rect(org,Result,up_k-dn_k+1)
end;

var
  dx,dy,fi,px,py,l: int;
  p: TGauss; h: Double; sz: TSize;
  pc: PChar; t: String;
begin
  Result:=0;

  px:=v[0].X; dx:=v[1].X-px;
  py:=v[0].Y; dy:=v[1].Y-py; fi:=0;

  if (dx <> 0) or (dy <> 0) then
    fi:=Round( -ArcTan2(dy,dx)/Pi*180 )
  else
    dx:=1;

  if fDC <> 0 then
  if SetAngle(fi) then begin

    h:=0;
    if up in [0..2] then
    h:=fHeights[up+1];

    if Abs(h) > 0.5 then begin
      h:=h / Hypot(dx,dy);
      Inc(px,Round(h*dy));
      Inc(py,-Round(h*dx));
    end;

    if is_mix_wide(s) then
      Result:=OutMix(px,py,fi,s)
    else begin

      t:=StrPasUTF8(s);
      l:=Length(t);

      if l > 0 then begin
        pc:=@t[1];

        if fDrawDC <> 0 then
        TextOut(fDrawDC,px,py,pc,l);

        GetTextExtentPoint(fDC,pc,l,sz);

        p.x:=px; p.y:=py;
        Vgm_Rect(p,sz.cx,1);

        if Assigned(fOnText) then
        TextEvent(px,py,sz.cx,s);

        Result:=sz.cx
      end
    end
  end
end;

procedure TTrueTypeFont.PolyText(lp: PLLine; cl: Integer;
                                 s: PWideChar);
var
  lt,up,i,ir,jr,sp: Integer; p0: TPoint;
  ax,len,space,top,dx,dx2,vx,vy,vk: Double;
  sz: TSize; L: VLLine; v,vv: LVector;
  dn,txt: TGauss; ch: WideChar2;
  buf: TWideStr;
begin
  ch[1]:=#0;

  up:=tlong(cl).b[2];
  lt:=(up shr 2) and 7; up:=up and 3;

  v[0]:=lp.Pol[0]; v[1]:=v[0];
  if lp.N > 0 then v[1]:=lp.Pol[1];

  if (lp.N <= 1) and (lt in [0,2,3]) then
    OutText(@v,up,s)
  else
  if lt in [2,3] then begin

    v[1]:=lp.Pol[0]; i:=1;

    while i <= lp.N do begin
      if s[0] = #0 then Break;
      v[0]:=v[1]; v[1]:=lp.Pol[i];

      if i = lp.N then
        OutText(@v,up,s)
      else begin
        ch[0]:=s[0]; s:=@s[1];
        OutText(@v,up,ch)
      end;

      Inc(i); if lt = 3 then begin
        v[1]:=lp.Pol[i]; if i < lp.N then Inc(i)
      end
    end

  end
  else begin
    if lp.N = 0 then begin
      L.Pol[0]:=v[0]; L.Pol[1]:=v[1];
      L.N:=1; lp:=@L
    end;

    with lp^ do
    dn:=Stretch_dir(Pol[0],Pol[1],fFont.lfHeight);

    p0:=lp.Pol[0]; space:=0; top:=0;

    if lt > 0 then begin

      len:=PolyLength(@lp.Pol,lp.N);

      if lt = 6 then
        space:=blank_space(len,s)
      else begin
        sz:=TextExtent(s);
        txt.x:=sz.cx; txt.y:=sz.cy;

        case lt of
      1:  begin
            i:=WideStrLen(s);
            if i > 1 then begin
            space:=(len-txt.x)/(i-1);
              if space < 0 then space:=0
            end
          end;
      4,
      5:  begin
            if lt = 5 then begin
              len:=len/2; txt.x:=txt.x/2
            end;

            top:=len-txt.x; if top < 0 then begin
              Get_Next_Point(v[0],v[1],top,lp.Pol[0]);
              top:=0
            end
          end
        end
      end
    end;

    ax:=top; txt.x:=0; txt.y:=0;

    jr:=-1; sp:=0;

    if (lp.N = 1) and (lt in [0,4,5]) then begin
      v[0]:=lp.Pol[0]; v[1]:=lp.Pol[1];
      Move_vector(v,ax); OutText(@v,up,s)
    end else
    for i:=0 to 1023 do begin
      ch[0]:=s[0]; s:=@s[1];
      if ch[0] = #0 then Break;

      if lt = 6 then
      if ch[0] = ' ' then begin
        ax:=ax + space; Continue
      end else
      if ch[0] = '^' then begin
        ax:=0; space:=blank_space(len,s);
        txt.x:=txt.x + dn.x;
        txt.y:=txt.y + dn.y;
        Continue
      end;

      sz:=CharExtent(ch[0]);
      dx:=sz.cx; dx2:=dx/2;

      ir:=Poly_dist_Vector(@lp.Pol,lp.N,ax+dx2,dx,true,v);

      vx:=v[1].X-v[0].X;
      vy:=v[1].Y-v[0].Y;
      len:=Hypot(vx,vy);

      if len < 0.01 then begin
        vx:=-dx2; vy:=0
      end
      else begin vk:=-dx2/len;
        vx:=vx*vk; vy:=vy*vk
      end;

      v[0].x:=v[0].x + Round(vx + txt.x);
      v[0].y:=v[0].y + Round(vy + txt.y);
      v[1].x:=v[1].x + Round(txt.x);
      v[1].y:=v[1].y + Round(txt.y);

      if (ir <> jr) or (sp = 128) then begin
        if sp > 0 then begin
          buf[sp]:=#0; OutText(@vv,up,buf);
        end;

        buf[0]:=ch[0]; sp:=1;
        vv:=v; jr:=ir
      end
      else begin
        buf[sp]:=ch[0]; Inc(sp);
      end;

      ax:=ax + CharExtent(ch[0]).cx;
      if lt = 1 then ax:=ax + space
    end;

    if sp > 0 then begin
      buf[sp]:=#0; OutText(@vv,up,buf)
    end;

    lp.Pol[0]:=p0
  end;
end;

procedure TTrueTypeFont.PolyAttr(lp: PLLine; cl: Integer;
                                 ds: Double; s: PWideChar);
var
  tdc: HDC;
  ax,bx,h,k,len,space,dx,dx2,top,vx,vy: double;
  i,up,ir,jr,sp: Integer; p: PWideChar; sz: TSize;
  lt,rb: TPoint; v,vv: LVector; R: TRect;
  ch: WideChar; buf: TWideStr; blank: Boolean;
begin
  GetClipBox(fDrawDC,@R);

  lt:=Point(-100,-100);
  rb:=Point(R.Right+100,R.Bottom+100);

  up:=tlong(cl).b[2];
  blank:=((up shr 6) and 3) <> 0;

  ax:=ds; p:=s; jr:=-1; sp:=0; bx:=0;

  for i:=0 to 1023 do begin
    ch:=p[0]; p:=@p[1];
    if ch = #0 then Break;

    sz:=CharExtent(ch);
    dx:=sz.cx; dx2:=dx/2;

    ir:=Poly_dist_Vector(@lp.Pol,lp.N,ax+dx2,dx,true,v);

    vx:=v[1].X-v[0].X;
    vy:=v[1].Y-v[0].Y;
    len:=Hypot(vx,vy);

    if len < 0.01 then
      v[0].x:=v[0].x - Round(dx2)
    else begin k:=-dx2/len;
      v[0].x:=v[0].x + Round(vx*k);
      v[0].y:=v[0].y + Round(vy*k);
    end;

    if (ir <> jr) or (sp = 128) then begin
      if sp > 0 then
      if txt_Clip(lt,rb,@vv,bx,sz.cy) then begin
        buf[sp]:=#0;

        if blank then
        if Assigned(fOwner) then begin
          fOwner.xBlankTyp:=1; tdc:=fDrawDC;
          fDrawDC:=0; OutText(@vv,up,buf);
          fOwner.xBlankTyp:=0; fDrawDC:=tdc
        end;

        OutText(@vv,up,buf)
      end;

      buf[0]:=ch; sp:=1;
      vv:=v; jr:=ir; bx:=dx
    end
    else begin
      buf[sp]:=ch; Inc(sp);
      bx:=bx + dx
    end;

    ax:=ax + dx
  end;

  if sp > 0 then
  if txt_Clip(lt,rb,@vv,bx,sz.cy) then begin
    buf[sp]:=#0;

    if blank then
    if Assigned(fOwner) then begin
      fOwner.xBlankTyp:=1; tdc:=fDrawDC;
      fDrawDC:=0; OutText(@vv,up,buf);
      fOwner.xBlankTyp:=0; fDrawDC:=tdc
    end;

    OutText(@vv,up,buf)
  end;
end;

function TTrueTypeFont.TextExtent(s: PWideChar): TSize;
var
  l: int; sz: TSize; pc: PChar; t: String;
begin
  Result.cx:=0; Result.cy:=0;

  if (fOldFont <> 0) or SetAngle(0) then begin

    t:=StrPasUTF8(s);
    l:=Length(t);

    if l > 0 then begin pc:=@t[1];
      if GetTextExtentPoint(fDC,pc,l,sz) then
      Result:=sz
    end
  end
end;

function TTrueTypeFont.__charSize(ch: WideChar): TSize;
var
  l: int; sz: TSize; pc: PChar; t: String;
begin
  Result.cx:=0; Result.cy:=0;

  t:=AnsiToUtf8(ch);
  l:=Length(t);

  if l > 0 then begin pc:=@t[1];
    if GetTextExtentPoint(fDC,pc,l,sz) then
    Result:=sz
  end
end;

function TTrueTypeFont.CharExtent(ch: WideChar): TSize;
begin
  Result.cx:=0; Result.cy:=0;
  if (fOldFont <> 0) or SetAngle(0) then
  Result:=__charSize(ch)
end;

function TTrueTypeFont.blank_space(dist: Double; s: PWideChar): Double;
var
  i,cx: Integer; sz: TSize;
  ch: WideChar; ax: Double; cl: Char;
begin
  Result:=0; ax:=0; cx:=0;

  if (fOldFont <> 0) or SetAngle(0) then

  for i:=0 to 1023 do begin
    ch:=s[0]; s:=@s[1];
    if ch = #0 then Break else
    if ch = '^' then Break else
    if ch = ' ' then Inc(cx) else
    ax:=ax + __charSize(ch).cx
  end;

  if cx > 0 then begin
    dist:=(dist-ax)/cx;
    if dist < 0 then dist:=0;
    Result:=dist
  end
end;

function TTrueTypeFont.BeginDraw(ADrawDC: HDC; cl: Integer): Boolean;
var
  tc: Integer;
begin
  fDrawDC:=ADrawDC; fDC:=fDrawDC;
  if fDC = 0 then fDC:=GetDC(0);
  fOldFont:=0; SetAngle(45);

  if fDrawDC <> 0 then begin
    SetBkMode(fDrawDC,TRANSPARENT);
    tc:=dib_color(cl and 15);
    SetTextColor(fDrawDC,tc);
  end;

  Result:=fDC <> 0
end;

procedure TTrueTypeFont.EndDraw;
begin
  if fDC <> 0 then
  if fOldFont <> 0 then
  SelectObject(fDC,fOldFont);
  fOldFont:=0;

  if fDrawDC = 0 then
  ReleaseDC(0,fDC); fDC:=0
end;

constructor TTrueTypes.Create(AOwner: TFonts);
begin
  inherited Create;
  fttfRatio:=0.75;
  fOwner:=AOwner
end;

function TTrueTypes.GetFont(AName: PChar; AHeight: Integer): TTrueTypeFont;
var
  i,fl,rc,h: Integer; ttf: TTrueTypeFont;
  p: PChar; fnt,old: HFont; lfont: TLogFont;
  sz: TSize;
begin
  Result:=nil;

  h:=Round(AHeight / fttfRatio);

  Fillchar(lFont,Sizeof(lFont),0);
  with lFont do begin
    lfHeight:=h; lfWeight:=FW_NORMAL;

    lfCharSet:=RUSSIAN_CHARSET;
    lfQuality:=Proof_Quality;

    StrPCopy(lfFaceName,AName);

    p:=StrScan(lfFaceName,'*');
    if Assigned(p) then begin
      p[0]:=#0; p:=@p[1];

      val(p,fl,rc);
      if rc = 0 then begin
        lfItalic:=fl and 1;
        if fl and 2 <> 0 then
        lfWeight:=FW_BOLD
      end
    end
  end;

  for i:=0 to Count-1 do begin
    ttf:=Items[i] as TTrueTypeFont;
    if ttf.This(lFont) then begin
      Result:=ttf; Break
    end
  end;

  if Result = nil then begin
    Result:=TTrueTypeFont.Create(fOwner,lFont);
    try
      Add(Result)
    finally
    end
  end
end;

constructor TFreeTypeFonts.Create(AOwner: TFonts;
                                  Adraw: IDrawText);
begin
  inherited Create;
  fOwner:=AOwner; fdraw:=Adraw;
  fttfRatio:=0.75;
end;

destructor TFreeTypeFonts.Destroy;
begin
  fdraw:=nil; inherited
end;

function TFreeTypeFonts.SetFont(AName: PChar; AHeight: Integer): Boolean;
var
  h: int;
begin
  if Assigned(fdraw) then

  if (StrComp(AName,fName) <> 0)
  or (fHeight = AHeight) then begin
    StrCopy(fName,AName);
    fHeight:=AHeight;
    fEnabled:=false;

    if fdraw.SetFont(AName) >= 0 then begin

      h:=Round(AHeight / fttfRatio);
      lfHeight:=h;

      fHeights[0]:=h;
      fHeights[1]:=Round(h * 0.5);
      fHeights[2]:=Round(h * 1.0);;
      fHeights[3]:=Round(h * 0.25);

      fEnabled:=fdraw.SetSize(lfHeight)
    end
  end;

  Result:=fEnabled
end;

function TFreeTypeFonts.GetExtent(s: PWideChar): TPoint;
begin
  Result:=fdraw.GetExtent(s)
end;

function TFreeTypeFonts.blank_space(dist: double; s: PWideChar): double;
var
  i,cx: int; ch: WideChar; ax: double;
begin
  Result:=0; ax:=0; cx:=0;

  for i:=0 to 1023 do begin
    ch:=s[0]; s:=@s[1];
    if ch = #0 then Break else
    if ch = '^' then Break else
    if ch = ' ' then Inc(cx) else
    ax:=ax + fdraw.GetExtentc(ch).X
  end;

  if cx > 0 then begin
    dist:=(dist-ax)/cx;
    if dist < 0 then dist:=0;
    Result:=dist
  end
end;

function TFreeTypeFonts.OutText(v: PPoly; up: int; s: PWideChar): int;

function X_Plot(x,y: Double; w: int): tgauss;
begin
  Result.x:=x + fMatrix[1,1]*w;
  Result.y:=y - fMatrix[2,1]*w
end;

function Y_Plot(x,y,h: Double): tgauss;
begin
  Result.x:=x - h * fMatrix[1,2];
  Result.y:=y + h * fMatrix[2,2]
end;

function Y_Move(x,y,k: Double): TGauss;
begin
  Result:=Y_Plot(x,y,lfHeight*k)
end;

function IY_Move(x,y,k: Double): TPoint;
var
  p: TGauss;
begin
  p:=Y_Move(x,y,k);
  Result.X:=Round(p.x);
  Result.Y:=Round(p.y);
end;

procedure Vgm_Rect(const a: TGauss; w: int; k: Double);
var
  h: Float; b: TGauss;
begin
  h:=fHeights[1]*k;
  b:=X_Plot(a.x,a.y,w);

  fg_Rect[1]:=a;
  fg_Rect[0]:=Y_Plot(a.x,a.y,+h);
  fg_Rect[3]:=Y_Plot(b.x,b.y,+h);
  fg_Rect[2]:=b; fg_Rect[4]:=a;

  if Assigned(fOnRect) then
  fOnRect(Self)
end;

procedure TextEvent(px,py: Double; w: int; s: PWideChar);
var
  p1,p2: TGauss;
begin
  p1.x:=px; p1.y:=py;
  p2:=X_Plot(px,py,w);
  fOnText(p1,p2,fHeights[0],s)
end;

function __OutText(px,py: Double; s: PWideChar): int;
var
  tw: int;
begin
  Result:=0;

  if s[0] <> #0 then begin

    if fCanvas = nil then
      tw:=fdraw.GetExtent(s).X
    else begin
      fdraw.Draw(fCanvas, Round(px),Round(py),fAngle, s);
      tw:=fdraw.GetExtent(nil).X
    end;

    if Assigned(fOnText) then
    TextEvent(px,py,tw,s);

    Result:=tw
  end;
end;

function OutMix(px,py: int; s: PWideChar): int;
var
  i,dx,w,sp,tw: int; dn_k,up_k,k,r,u: Double;
  p,q,sq,org: tgauss; ch: WideChar; a,b: TPoint;
  buf: TWideStr; pop: Boolean;
begin
  Result:=0;

  p.x:=px; p.y:=py; q:=p; org:=p; sp:=0;

  dx:=0; w:=0; dn_k:=0; up_k:=0; k:=0;

  i:=0; while i < 256 do begin

    ch:=s[i]; Inc(i);
    if ch = #0 then Break;

    if ch = '~' then begin k:=k-4/3;
      p:=y_move(p.x,p.y,4/3); Dec(w,dx); dx:=0
    end else
    if ch = '^' then begin
      p:=y_move(q.x,q.y,-2/3);
      k:=k+2/3; dx:=0
    end else
    if ch = '@' then begin
      u:=AtoI(s[i],2)/100; Inc(i,2);
      p:=y_move(q.x,q.y,-u); k:=k+u; dx:=0
    end else
    if ch = '_' then begin
      a:=iy_move(p.x,p.y,4/3);
      b:=iy_move(q.x,q.y,4/3);

      if Assigned(fCanvas) then begin
        fCanvas.MoveTo(a.X,a.Y);
        fCanvas.LineTo(b.X,b.Y);
      end;

      if Assigned(fOnText) then
      fOnText(y_move(p.x,p.y,0.1),
              y_move(q.x,q.y,0.1),0,nil)

    end
    else begin
      if sp = 0 then sq:=q;
      buf[sp]:=ch; Inc(sp)
    end;

    if sp > 0 then begin
      if sp = 128 then
        pop:=true
      else begin
        ch:=s[i]; if ch < #255 then
        pop:=Char(ch) in ['~','^','@','_'];
      end;

      if pop then begin buf[sp]:=#0;
        tw:=__OutText(sq.x,sq.y,buf);
        Inc(dx,tw); Inc(w,tw); sp:=0;
        if Result < w then Result:=w
      end
    end;

    if k < dn_k then dn_k:=k;
    if k > up_k then up_k:=k;

    q.x:=p.x + fMatrix[1,1]*dx;
    q.y:=p.y - fMatrix[2,1]*dx
  end;

  if sp > 0 then begin
    buf[sp]:=#0; __OutText(sq.x,sq.y,buf)
  end;

  if dn_k < 0 then
  org:=y_move(org.x,org.y,-dn_k);

  Vgm_Rect(org,Result,up_k-dn_k+1)
end;

var
  dx,dy,px,py,fi,tw: int;
  p: TGauss; h: Double;
begin
  Result:=0;

  if Assigned(fCanvas) then begin

    px:=v[0].X; dx:=v[1].X-px;
    py:=v[0].Y; dy:=v[1].Y-py; fi:=0;

    if (dx <> 0) or (dy <> 0) then
      fi:=Round( -ArcTan2(dy,dx)/Pi*1800 )
    else
      dx:=1;

    fAngle:=fi;
    fMatrix:=Rotate_3x3(-fi/1800*Pi);

    h:=0;
    if up in [1..2] then
    h:=fHeights[up+1];

    if Abs(h) > 0.5 then begin
      h:=h / Hypot(dx,dy);
      Inc(px,Round(h*dy));
      Inc(py,Round(h*dx));
    end;

    if is_mix_wide(s) then
      Result:=OutMix(px,py,s)
    else
    if s[0] <> #0 then begin

      if fCanvas = nil then
        tw:=fdraw.GetExtent(s).X
      else begin
        fCanvas.SetPenColor(fColor);
        fdraw.Draw(fCanvas,px,py,fAngle,s);
        tw:=fdraw.GetExtent(nil).X
      end;

      p.x:=px; p.y:=py;
      Vgm_Rect(p,tw,1);

      if Assigned(fOnText) then
      TextEvent(px,py,tw,s);

      Result:=tw
    end
  end
end;

procedure TFreeTypeFonts.PolyText(lp: PLLine; cl: int; s: PWideChar);
var
  lt,up,i,ir,jr,sp: int; p0,sz: TPoint;
  ax,len,space,top,dx,dx2,vx,vy,vk: Double;
  L: VLLine; v,vv: LVector; dn,txt: TGauss;
  ch: WideChar2; buf: TWideStr;
begin
  ch[1]:=#0;

  up:=tlong(cl).b[2];
  lt:=(up shr 2) and 7; up:=up and 3;

  v[0]:=lp.Pol[0]; v[1]:=v[0];
  if lp.N > 0 then v[1]:=lp.Pol[1];

  if (lp.N <= 1) and (lt in [0,2,3]) then
    OutText(@v,up,s)
  else
  if lt in [2,3] then begin

    v[1]:=lp.Pol[0]; i:=1;

    while i <= lp.N do begin
      if s[0] = #0 then Break;
      v[0]:=v[1]; v[1]:=lp.Pol[i];

      if i = lp.N then
        OutText(@v,up,s)
      else begin
        ch[0]:=s[0]; s:=@s[1];
        OutText(@v,up,ch)
      end;

      Inc(i); if lt = 3 then begin
        v[1]:=lp.Pol[i]; if i < lp.N then Inc(i)
      end
    end

  end
  else begin
    if lp.N = 0 then begin
      L.Pol[0]:=v[0]; L.Pol[1]:=v[1];
      L.N:=1; lp:=@L
    end;

    with lp^ do
    dn:=Stretch_dir(Pol[0],Pol[1],lfHeight);

    p0:=lp.Pol[0]; space:=0; top:=0;

    if lt > 0 then begin

      len:=PolyLength(@lp.Pol,lp.N);

      if lt = 6 then
        space:=blank_space(len,s)
      else begin
        sz:=fdraw.GetExtent(s);
        txt.x:=sz.X; txt.y:=sz.Y;

        case lt of
      1:  begin
            i:=WideStrLen(s);
            if i > 1 then begin
            space:=(len-txt.x)/(i-1);
              if space < 0 then space:=0
            end
          end;
      4,
      5:  begin
            if lt = 5 then begin
              len:=len/2; txt.x:=txt.x/2
            end;

            top:=len-txt.x; if top < 0 then begin
              Get_Next_Point(v[0],v[1],top,lp.Pol[0]);
              top:=0
            end
          end
        end
      end
    end;

    ax:=top; txt.x:=0; txt.y:=0;

    jr:=-1; sp:=0;

    if (lp.N = 1) and (lt in [0,4,5]) then begin
      v[0]:=lp.Pol[0]; v[1]:=lp.Pol[1];
      Move_vector(v,ax); OutText(@v,up,s)
    end else
    for i:=0 to 1023 do begin
      ch[0]:=s[0]; s:=@s[1];
      if ch[0] = #0 then Break;

      if lt = 6 then
      if ch[0] = ' ' then begin
        ax:=ax + space; Continue
      end else
      if ch[0] = '^' then begin
        ax:=0; space:=blank_space(len,s);
        txt.x:=txt.x + dn.x;
        txt.y:=txt.y + dn.y;
        Continue
      end;

      sz:=fdraw.GetExtentc(ch[0]);
      dx:=sz.X; dx2:=dx/2;

      ir:=Poly_dist_Vector(@lp.Pol,lp.N,ax+dx2,dx,true,v);

      vx:=v[1].X-v[0].X;
      vy:=v[1].Y-v[0].Y;
      len:=Hypot(vx,vy);

      if len < 0.01 then begin
        vx:=-dx2; vy:=0
      end
      else begin vk:=-dx2/len;
        vx:=vx*vk; vy:=vy*vk
      end;

      v[0].x:=v[0].x + Round(vx + txt.x);
      v[0].y:=v[0].y + Round(vy + txt.y);
      v[1].x:=v[1].x + Round(txt.x);
      v[1].y:=v[1].y + Round(txt.y);

      if (ir <> jr) or (sp = 128) then begin
        if sp > 0 then begin
          buf[sp]:=#0; OutText(@vv,up,buf);
        end;

        buf[0]:=ch[0]; sp:=1;
        vv:=v; jr:=ir
      end
      else begin
        buf[sp]:=ch[0]; Inc(sp);
      end;

      ax:=ax + fdraw.GetExtentc(ch[0]).X;
      if lt = 1 then ax:=ax + space
    end;

    if sp > 0 then begin
      buf[sp]:=#0; OutText(@vv,up,buf)
    end;

    lp.Pol[0]:=p0
  end;
end;

procedure TFreeTypeFonts.PolyAttr(lp: PLLine; cl: int;
                                  ds: Double; s: PWideChar);

procedure blank_text(lp: PLPoly; up: int; s: PWideChar);
var
  tdc: TPointer;
begin
  if Assigned(fOwner) then begin
    fOwner.xBlankTyp:=1;

    tdc:=TPointer(fCanvas);
    TPointer(fCanvas):=0;

    OutText(lp,up,s);
    fOwner.xBlankTyp:=0;

    TPointer(fCanvas):=tdc
  end;
end;

var
  ax,bx,h,k,len,space,dx,dx2,top,vx,vy: double;
  i,up,ir,jr,sp: int; p: PWideChar;
  lt,rb,sz: TPoint; v,vv: LVector;
  ch: WideChar; buf: TWideStr;
  blank: Boolean;
begin
  if Assigned(fCanvas) then begin

    lt:=Point(-100,-100);
    rb.X:=fCanvas.GetWidth+100;
    rb.Y:=fCanvas.GetHeight+100;

    up:=tlong(cl).b[2];
    blank:=((up shr 6) and 3) <> 0;

    ax:=ds; p:=s; jr:=-1; sp:=0; bx:=0;

    for i:=0 to 1023 do begin
      ch:=p[0]; p:=@p[1];
      if ch = #0 then Break;

      sz:=fdraw.GetExtentc(ch);
      dx:=sz.X; dx2:=dx/2;

      ir:=Poly_dist_Vector(@lp.Pol,lp.N,ax+dx2,dx,true,v);

      vx:=v[1].X-v[0].X;
      vy:=v[1].Y-v[0].Y;
      len:=Hypot(vx,vy);

      if len < 0.01 then
        v[0].x:=v[0].x - Round(dx2)
      else begin k:=-dx2/len;
        v[0].x:=v[0].x + Round(vx*k);
        v[0].y:=v[0].y + Round(vy*k);
      end;

      if (ir <> jr) or (sp = 128) then begin
        if sp > 0 then
        if txt_Clip(lt,rb,@vv,bx,sz.Y) then begin
          buf[sp]:=#0;

          if blank then
          blank_text(@vv,up,buf);

          OutText(@vv,up,buf)
        end;

        buf[0]:=ch; sp:=1;
        vv:=v; jr:=ir; bx:=dx
      end
      else begin
        buf[sp]:=ch; Inc(sp);
        bx:=bx + dx
      end;

      ax:=ax + dx
    end;

    if sp > 0 then
    if txt_Clip(lt,rb,@vv,bx,sz.Y) then begin
      buf[sp]:=#0;

      if blank then
      blank_text(@vv,up,buf);

      OutText(@vv,up,buf)
    end
  end;
end;

constructor TFonts.Create(cash: int; Adraw: IDrawText);
begin
  inherited Create(cash,0);

  fTrueTypes:=TFreeTypeFonts.Create(Self,Adraw);
  fTrueTypes.OnRect:=ttf_Rect;

  MinH:=6; MaxH:=512; Comp:=1;
  sync_folder; fFntIndex:=-1;

//  fonlyFreeType:=true
end;

destructor TFonts.Destroy;
begin
  fTrueTypes.Free; inherited
end;

procedure TFonts.sync_folder;
var
  dir: TShortstr;
begin
  UsrDir(fFolder,'/fonts');
  if not dir_Exists(fFolder) then begin
    StrDirectory(dir,BinDir);
    StrPath(fFolder,dir,'fonts');
    if not dir_Exists(fFolder) then
    UsrDir(fFolder,'/obj')
  end
end;

function TFonts.Open_vgt(Ind: Integer): Boolean;
var
  fn,fn1,nm: TShortStr;
begin
  if not vm_Active or (Ind <> fFntIndex) then begin

    StrPCopy(nm,'/'+Fnt_FNames[Ind]);
    StrCopy(fn,fFolder); StrLCat(fn,nm,255);

    StrCopy(fn1,fn); StrLCat(fn1,'.vgt',255);

    if not Open(fn1) then begin
      StrCopy(fn1,fn); StrLCat(fn1,'.vgm',255);
      Open(fn1)
    end;

    fFntIndex:=Ind;
  end;

  Result:=vm_Active
end;

function TFonts.Open_font(cl: int): Boolean;
begin
  Result:=Open_vgt( Get_Font_Index(cl) )
end;

function TFonts.Open_ttf(fn: PChar; cl: int; k: double): Boolean;
var
  fh: Float;
begin
  Result:=false;

  if k < 0 then
    fh:=Abs(k)
  else begin
    fh:=Fonts_Height(cl);
    if fh < 0 then fh:=Abs(fh) * pp_k
              else fh:=fh * k;
  end;

  if fh >= MinH then
  if fTrueTypes.SetFont(fn,Round(fh)) then begin
    fTrueTypes.Color:=dib_color(cl and 15);
    Result:=true
  end;
end;

procedure TFonts.ttf_Rect(Sender: TObject);
var
  r: GOrient;
begin
  r:=fTrueTypes.g_Rect;
  Set_Vgm_Rect(@r);
end;

function TFonts.StrGraphics(s: PChar; cl: Integer): PChar;
var
  fnt: Integer; h: Float; p: PChar;
begin
  Result:=nil; StrCopy(s,'');

  fnt:=Get_Font_Index(cl);
  h:=Fonts_Height(cl);

  StrPCopy(s,Fnt_FNames[fnt]);
  p:=StrScan(@s[1],'*');
  if p <> nil then p[0]:=#0;

  StrCat(s,' - '); fStrCat(s,h,2,true);
  Result:=StrCat(s,'mm')
end;

function TFonts.Get_char_bound(ch,cl: Integer;
                               out lt,rb: TGauss): TGauss;
var
  k: Double; _lt,_rb: TPoint;
begin
  Result:=gauss_nil;
  lt:=Result; rb:=lt;

  if Open_Font(cl) then begin

    if rus_interface then
      Vgm_Read(ord('A')+1)
    else
      Vgm_Read(ord('A')+1);

    if hdr[6] > 0 then begin
      k:=Fonts_Height(cl)/hdr[6];

      Inc(ch); Vgm_Read(ch);
      if hdr[6] > 0 then
      if Vgm_Min_Max(_lt,_rb) > 1 then begin
        lt.x:=_lt.X*k; lt.y:=_lt.Y*k;
        rb.x:=_rb.X*k; rb.y:=_rb.Y*k;
        Result.x:=hdr[5]*k;
        Result.y:=hdr[6]*k;
      end
    end
  end
end;

function TFonts.Scale_vgt(cl: Integer; k: Double): Double;
var
  h: Float;
begin
  Result:=0;

  if rus_interface then Vgm_Read(ord('A')+1)
  else                  Vgm_Read(ord('A')+1);

  h:=Fonts_Height(cl);

  if hdr[6] = 0 then
    Result:=0
  else
  if h < -0.0001 then
    Result:=Abs(h)/hdr[6]*pp_k
  else
  if h <= 0.000001 then
    Result:=1
  else begin h:=h*k;
    if h >= MinH then begin
      if h > MaxH then h:=MaxH; k:=h/hdr[6]
    end else k:=0; Result:=k
  end
end;

function TFonts.Display_text(cl: Integer; k: Double;
                             out h: Float): Boolean;
begin
  Result:=true;
  h:=Fonts_Height(cl);
  if h > 0 then h:=h*k; h:=Abs(h);
  if Abs(h) < MinH then h:=0;
end;

function TFonts.Display_text1(cl: Integer; k: Double;
                              out h: Float; out ansi: int): Boolean;
var
  fnt: int; fn: TShortStr;
begin
  Result:=Display_text(cl,k,h); ansi:=0;

  fnt:=Get_Font_Index(cl);
  StrPCopy(fn,Fnt_FNames[fnt]);

  if not fonlyFreeType then
  if fn[0] = '*' then
    ansi:=rus_unicode
  else
  if Open_vgt(fnt) then
  if t_cnt > 256 then ansi:=rus_unicode
                 else ansi:=dos_unicode;
end;

function TFonts.Scale_Text(cl: Integer; k: Double;
                           h1,h2: Float): Double;
var
  h: Float;
begin
  Result:=k;

  h:=Fonts_Height(cl);
  if h > 0.1 then begin h:=h*k;
    if h < h1 then
      Result:=k / h * h1
    else
    if h1 < h2 then
    if h > h2 then
      Result:=k / h * h2
  end
end;

function TFonts.Out_Scale(cl: Integer; k: Double): Double;
begin
  Vgm_Read(ord('A')+1);
  Result:=0; if hdr[6] > 0 then
  Result:=Fonts_Height(cl)*k/hdr[6]
end;

function TFonts.Font_Aspect: Double;
begin
  Vgm_Read(ord('A')+1);
  Result:=hdr[5]/Max(1,hdr[6]);
end;

function TFonts.blank_space(s: PWideChar;
                            dist,kz: Double): Double;
var
  i,cx: Integer;
  ch: Widechar; ax: Double;
begin
  Result:=0; ax:=0; cx:=0;

  for i:=0 to 1023 do begin
    ch:=s[i];
    if ch = #0 then Break else
    if ch = '^' then Break else
    if ch = ' ' then Inc(cx) else
    if Vgm_Read(ord(ch)+1) > 0 then
    ax:=ax + (hdr[5]-hdr[3])*kz;
  end;

  if cx > 0 then
  Result:=Max(0,(dist-ax)/cx)
end;

procedure TFonts.Out_lWide(DC: XCanvas; lp: PLLine;
                           cl: Integer; k: Double;
                           s: PWideChar);

procedure ttf_lwide(DC: XCanvas; lp: PLLine;
                    fn: PChar; cl: int; k: Double;
                    s: PWideChar);
var
  up: int;
begin
  if open_ttf(fn,cl,k) then begin

    fxBlankTest1:=0;
    if fxBlankTest > 0 then begin
      xBlankTyp:=255; fxDC:=DC;
      fTrueTypes.PolyText(lp,cl,s);
      xBlankTyp:=0; fxDC:=nil
    end;

    if fxBlankTest1 = 0 then begin

      up:=tlong(cl).b[2];
      if ((up shr 6) and 3) <> 0 then begin
        xBlankTyp:=1; fxDC:=DC;
        fTrueTypes.PolyText(lp,cl,s);
        xBlankTyp:=0; fxDC:=nil
      end;

      if Assigned(DC) then
      fTrueTypes.Canvas:=DC.Canvas;

      fTrueTypes.PolyText(lp,cl,s);

      fTrueTypes.Canvas:=nil
    end
  end
end;

procedure vgt_lWide(DC: XCanvas; lp: PLLine;
                    fnt,cl: Integer; k: Double;
                    s: PWideChar);
var
  ax,h,len,space,dx,dx2,top: double;
  i,lt,up: Integer; a,b,lp_p: TPoint;
  p: PWideChar; ch: WideChar2; v: LVector;
  dn,txt: TGauss; L: VLLine;
begin
  if Open_vgt(fnt) then begin

    up:=tlong(cl).b[2]; ch[1]:=#0;

    if ((up shr 6) and 3) <> 0 then begin
      xBlankTyp:=1; fxDC:=DC;
      Out_lWide(DC,lp,cl and $3FFFFF,k,s);
      xBlankTyp:=0; fxDC:=nil
    end;

    k:=Scale_vgt(cl,k);

    cl:=cl and 15;
    if DC <> nil then begin
      DC.xPen(1,cl); DC.xBrush(0,cl)
    end;

    lt:=(up shr 2) and 7; up:=up and 3;

    h:=0; if up > 0 then begin
      h:=hdr[6]*k; if Odd(up) then h:=h/2
    end;

    a:=lp.Pol[0]; b:=a;
    if lp.N > 0 then b:=lp.Pol[1];

    if (lp.N <= 1) and (lt in [0,2,3]) then
      Vgm_Wide(DC,a,b,h,k,0,s)
    else
    if lt in [2,3] then begin

      p:=s; b:=lp.Pol[0]; i:=1;

      while i <= lp.N do begin
        if p[0] = #0 then Break;
        a:=b; b:=lp.Pol[i];

        if i = lp.N then
          Vgm_Wide(DC,a,b,h,k,0,p)
        else begin
          ch[0]:=p[0]; p:=@p[1];
          Vgm_Wide(DC,a,b,h,k,0,ch);
        end;

        Inc(i); if lt = 3 then begin
          b:=lp.Pol[i]; if i < lp.N then Inc(i)
        end
      end

    end
    else begin
      if lp.N = 0 then begin
        L.Pol[0]:=a; L.Pol[1]:=b;
        L.N:=1; lp:=@L
      end;

      with lp^ do
      dn:=Stretch_dir(Pol[0],Pol[1],hdr[6]*k);

      lp_p:=lp.Pol[0]; space:=0; top:=0;

      if lt > 0 then begin

        len:=PolyLength(@lp.Pol,lp.N);

        if lt = 6 then
          space:=blank_space(s,len,k)
        else begin
          txt.x:=0; txt.y:=0;
          if Vgm_Wide(nil, a,b, h,k,0, s) > 0 then
          txt:=Vgm_Size; Virt_reset;

          case lt of
        1:  begin
              i:=WideStrLen(s); if i > 1 then
              space:=Max(0,(len-txt.x)/(i-1));
            end;
        4,
        5:  begin
              if lt = 5 then begin
                len:=len/2; txt.x:=txt.x/2
              end;

              top:=len-txt.x; if top < 0 then begin
                Get_Next_Point(a,b,top,lp.Pol[0]);
                top:=0
              end
            end
          end
        end
      end;

      ax:=top; txt.x:=0; txt.y:=0; p:=s;

      if (lp.N = 1) and (lt in [0,4,5]) then begin
        v[0]:=lp.Pol[0]; v[1]:=lp.Pol[1]; Move_vector(v,ax);
        Vgm_Wide(DC, v[0],v[1], h,k,0, s)
      end else
      for i:=0 to 1023 do begin
        ch[0]:=p[0]; p:=@p[1];
        if ch[0] = #0 then Break;

        if lt = 6 then
        if ch[0] = ' ' then begin
          ax:=ax + space; Continue
        end else
        if ch[0] = '^' then begin
          ax:=0; space:=blank_space(p,len,k);
          txt.x:=txt.x + dn.x;
          txt.y:=txt.y + dn.y;
          Continue
        end;

        if Vgm_Read(ord(ch[0])+1) > 0 then begin
          dx:=(hdr[5]-hdr[3])*k; dx2:=dx/2;

          Poly_dist_Vector(@lp.Pol,lp.N,ax+dx2,dx,false,v);
          Get_Next_Point(v[0],v[1],-dx2,a); b:=v[1];

          a.x:=a.x + Round(txt.x);
          a.y:=a.y + Round(txt.y);
          b.x:=b.x + Round(txt.x);
          b.y:=b.y + Round(txt.y);

          ax:=ax + Vgm_Wide(DC,a,b,h,k,0,ch)*k;
          if lt = 1 then ax:=ax + space
        end
      end;

      lp.Pol[0]:=lp_p
    end

  end
end;

var
  fnt: Integer; fn: TShortStr;
begin
  if s[0] <> #0 then
  if lp.N >= 0 then begin

    fnt:=Get_Font_Index(cl);
    StrPCopy(fn,Fnt_FNames[fnt]);

    if fonlyFreeType then
      ttf_lwide(DC,lp,fn,cl,k,s)
    else
    if fn[0] = '*' then
      ttf_lwide(DC,lp,@fn[1],cl,k,s)
    else
      vgt_lWide(DC,lp,fnt,cl,k,s)
  end
end;

procedure TFonts.vgt_Attr(DC: XCanvas; lp: PLLine;
                          cl: Integer; k,ds: Double;
                          s: PWideChar);
var
  ax,bx,th,h,l,space,dx,dx2,top,vx,vy: double;
  i,up,ir,jr,sp: Integer; a,b,lt,rb: TPoint;
  p: PWideChar; v,vv: LVector; ch: WideChar; 
  buf: TWideStr; blank: Boolean;
begin
  if s[0] <> #0 then
  if lp.N >= 0 then begin

    lt:=Point(-100,-100); rb:=Point(-200,-200);

    if Assigned(DC) then
    rb:=Point(DC.Width+100,DC.Height+100);

    up:=tlong(cl).b[2];
    blank:=((up shr 6) and 3) <> 0;

    k:=Scale_vgt(cl,k);

    cl:=cl and 15;
    if DC <> nil then begin
      DC.xPen(1,cl); DC.xBrush(0,cl)
    end;

    Vgm_Read(ord('A')+1);
    th:=hdr[6]*k;

    up:=up and 3; h:=0;
    if up > 0 then begin
      h:=th; if Odd(up) then h:=h/2
    end;

    ax:=ds; p:=s; jr:=-1; sp:=0; bx:=0;

    for i:=0 to 1023 do begin
      ch:=p[0]; p:=@p[1];
      if ch = #0 then Break;

      if Vgm_Read(ord(ch)+1) > 0 then begin
        dx:=(hdr[5]-hdr[3])*k; dx2:=dx/2;

        ir:=Poly_dist_Vector(@lp.Pol,lp.N,ax+dx2,dx,true,v);

        vx:=v[1].X-v[0].X;
        vy:=v[1].Y-v[0].Y;
        l:=Hypot(vx,vy);

        if l < 0.01 then
          v[0].x:=v[0].x - Round(dx2)
        else begin l:=-dx2/l;
          v[0].x:=v[0].x + Round(vx*l);
          v[0].y:=v[0].y + Round(vy*l);
        end;

        if (ir <> jr) or (sp = 128) then begin
          if sp > 0 then
          if txt_Clip(lt,rb,@vv,bx,th) then begin
            buf[sp]:=#0;

            if blank then begin
              xBlankTyp:=1; fxDC:=DC;
              Vgm_Wide(DC,vv[0],vv[1],h,k,0,buf);
              xBlankTyp:=0; fxDC:=nil
            end;

            Vgm_Wide(DC,vv[0],vv[1],h,k,0,buf);
          end;

          buf[0]:=ch; sp:=1;
          vv:=v; jr:=ir; bx:=dx
        end
        else begin
          buf[sp]:=ch; Inc(sp);
          bx:=bx + dx
        end;

        ax:=ax + dx
      end
    end;

    if sp > 0 then
    if txt_Clip(lt,rb,@vv,bx,th) then begin
      buf[sp]:=#0;

      if blank then begin
        xBlankTyp:=1; fxDC:=DC;
        Vgm_Wide(DC,vv[0],vv[1],h,k,0,buf);
        xBlankTyp:=0; fxDC:=nil
      end;

      Vgm_Wide(DC,vv[0],vv[1],h,k,0,buf);
    end;
  end
end;

procedure TFonts.Out_Attr(DC: XCanvas; lp: PLLine;
                          cl: Integer; k,ds: Double;
                          s: PWideChar);

procedure ttf_Attr(DC: XCanvas; lp: PLLine;
                   fn: PChar; cl: int; k,ds: Double;
                   s: PWideChar);
begin
  if open_ttf(fn,cl,k) then begin

    if Assigned(DC) then
    fTrueTypes.Canvas:=DC.Canvas;

    fTrueTypes.PolyAttr(lp,cl,ds,s);

    fTrueTypes.Canvas:=nil
  end
end;

var
  fnt: Integer; fn: TShortStr;
begin
  if s[0] <> #0 then
  if lp.N >= 0 then begin fxDC:=DC;

    fnt:=Get_Font_Index(cl);
    StrPCopy(fn,Fnt_FNames[fnt]);

    if fn[0] = '*' then
      ttf_Attr(DC,lp,@fn[1],cl,k,ds,s)
    else
    if Open_vgt(fnt) then
      vgt_Attr(DC,lp,cl,k,ds,s);

    fxDC:=nil
  end
end;

procedure TFonts.Line_Attr(DC: XCanvas; lp: PLLine;
                           cl: Integer; k,dx,ds: Double;
                           s: PWideChar);

function turn_len(pos,len: Double;
                  tp: PFloats; tn: Integer): Double;
var
  i: Integer;
begin
  Result:=len;

  for i:=1 to tn do begin
    if pos < tp[0] then begin
      Result:=tp[0]; Break
    end; tp:=@tp[1]
  end
end;

var
  len,old,pos,tw: Double; th: float;
  fnt,i,j,tn: Integer; p: PWideChar;
  jl: Array[0..2] of Integer;
  jw: Array[0..2] of Double;
  js: Array[0..2] of TWideStr;
  ch: WideChar; fn: TShortStr;
  turns: TFloats;
begin
  with lp^ do
  if Pol[lp.N].X < Pol[0].X then Swap_Poly(lp);

  if Display_text(cl,k,th) then
  lp.N:=Off_short_ribs(@lp.Pol,lp.N,th);

  len:=PolyLength(@lp.Pol,lp.N); pos:=dx;

  tn:=Get_Poly_turns(@lp.Pol,lp.N,@turns,30/180*Pi);

  fnt:=Get_Font_Index(cl);
  StrPCopy(fn,Fnt_FNames[fnt]);

  j:=0;
  for i:=0 to 2 do jl[i]:=0;

  p:=s;
  for i:=1 to 200 do begin
    ch:=p[0]; p:=@p[1];
    if ch = #0 then Break;

    if ch = '{' then begin
      if p[0] <> #0 then p:=@p[1];
      Inc(j); ch:=' '
    end;

    if j < 1 then begin
      js[1][jl[1]]:=ch; Inc(jl[1]);
    end;

    if j < 2 then begin
      js[0][jl[0]]:=ch; Inc(jl[0]);
    end;

    if j < 3 then begin
      js[2][jl[2]]:=ch; Inc(jl[2]);
    end
  end;

  for i:=0 to 2 do js[i][jl[i]]:=#0;

  if j > 2 then j:=0;

  if j = 0 then begin
    jl[0]:=0; jl[1]:=0; jl[2]:=0;
  end else
  if j = 1 then jl[2]:=0;

  if fn[0] = '*' then begin

    if open_ttf(@fn[1],cl,k) then begin

      fTrueTypes.Canvas:=DC.Canvas;

      tw:=fTrueTypes.GetExtent(s).X;

      if jl[0] > 0 then
      for i:=0 to 2 do begin
        jw[i]:=0; if jl[i] > 0 then
        jw[i]:=fTrueTypes.GetExtent(js[i]).X
      end;

      while pos + dx < len do begin

        old:=pos;
        pos:=turn_len(pos,len,@turns,tn)+dx;

        if jl[0] = 0 then begin
          if old + tw + dx <= pos then begin
            fTrueTypes.PolyAttr(lp,cl,old,s);
            pos:=old + tw + ds;
          end
        end else
        for i:=0 to 2 do begin
          if jl[i] = 0 then Break;
          if old + jw[i] + dx <= pos then begin
            fTrueTypes.PolyAttr(lp,cl,old,js[i]);
            pos:=old + jw[i] + ds; Break
          end
        end;

        if ds < 1 then Break;
      end;

      fTrueTypes.Canvas:=nil;
    end

  end else
  if Open_vgt(fnt) then begin

    tw:=Attr_Width(cl,k,s);
    if jl[0] > 0 then
    for i:=0 to 2 do begin
      jw[i]:=0; if jl[i] > 0 then
      jw[i]:=Attr_Width(cl,k,js[i]);
    end;

    while pos + dx < len do begin

      old:=pos;
      pos:=turn_len(pos,len,@turns,tn)+dx;

      if jl[0] = 0 then begin
        if old + tw + dx <= pos then begin
          vgt_Attr(DC,lp,cl,k,old,s);
          pos:=old + tw + ds
        end
      end else

      for i:=0 to 2 do begin
        if jl[i] = 0 then Break;
        if old + jw[i] + dx <= pos then begin
          vgt_Attr(DC,lp,cl,k,old,js[i]);
          pos:=old + jw[i] + ds; Break
        end
      end;

      if ds < 1 then Break;
    end
  end
end;

procedure TFonts.Out_Wide(DC: XCanvas;
                          x,y,h,cl: Integer;
                          s: PWideChar);
var
  chr: int; v: LVector;
  k: float; fn: TShortstr;
begin
  chr:=Get_Font_Index(cl);
  StrPCopy(fn,Fnt_FNames[chr]);

  v[0].X:=x;   v[1].X:=x+100;
  v[0].Y:=y+h; v[1].Y:=v[0].Y;

  if fn[0] = '*' then begin

    if open_ttf(@fn[1],cl,-h*1.2) then begin
      fTrueTypes.Canvas:=DC.Canvas;
      fTrueTypes.OutText(@v,0,s);
      fTrueTypes.Canvas:=nil
    end

  end else
  if Open_vgt(chr) then begin
    DC.xPen((cl shr 4) and 3 + 1,cl and 15);
    DC.xBrush(0,cl and 15);

    k:=1; if h > 0 then begin
      Vgm_Read(ord('A')+1);
      if hdr[6] > 0 then k:=h/hdr[6]
    end;

    Vgm_Wide(DC,v[0],v[1],0,k,0,s)
  end
end;

procedure TFonts.Out_Text(DC: XCanvas;
                          x,y,h,cl: Integer;
                          s: PChar);
var
  t: TWideStr;
begin
  xStringToWideChar(StrPas(s),t);
  Out_Wide(DC,x,y,h,cl,t)
end;

function TFonts.Text_Extent(cl: Integer; k: double; s: PWideChar): TSize;

function ttf_Extent(fn: PChar; cl: int; k: double; s: PWideChar): TSize;
var
  sz: TPoint;
begin
  Result.cx:=0;
  Result.cy:=0;

  if open_ttf(fn,cl,k) then begin
    sz:=fTrueTypes.GetExtent(s);
    Result.cx:=sz.X;
    Result.cy:=sz.Y
  end
end;

function vgt_Extent(fnt,cl: Integer; k: double; s: PWideChar): TSize;
begin
  Result.cx:=0;
  Result.cy:=0;
end;

var
  fnt: Integer; h: float;
  p: TPoint; fn: TShortStr;
begin
  Result.cx:=0;
  Result.cy:=0;

  if s[0] <> #0 then begin

    fnt:=Get_Font_Index(cl);
    StrPCopy(fn,Fnt_FNames[fnt]);

    if fn[0] = '*' then
      Result:=ttf_Extent(@fn[1],cl,k,s)
    else
    if Open_vgt(fnt) then begin
      h:=Fonts_Height(cl)*k;
      Result.cy:=Round(h);

      Vgm_Read(ord('A')+1);
      if hdr[6] > 0 then begin
        p:=Point(0,0); k:=h / hdr[6];
        if Vgm_Wide(nil, p,p, 0,k,0, s) > 0 then
        Result.cx:=Round(Vgm_Size.x)
      end
    end
  end
end;

function TFonts.Height_Text(cl: Integer; k: double): double;
begin
  Result:=0; cl:=tlong(cl).w[0];

  if Open_Font(cl) then begin
    Result:=Fonts_Height(cl)*k;
    Vgm_Read(ord('A')+1);
  end;
end;

function TFonts.Attr_Width(cl: Integer; k: double; s: PWideChar): Double;
var
  i: Integer; h: double; ch: WideChar;
begin
  Result:=0;

  if vm_Active then begin
    h:=Height_Text(cl,k); k:=0;
    if hdr[6] > 0 then k:=h/hdr[6];

    if k > 0 then
    for i:=0 to 1023 do begin
      ch:=s[0]; s:=@s[1];
      if ch = #0 then Break;

      if Vgm_Read(ord(ch)+1) > 0 then
      Result:=Result + (hdr[5]-hdr[3])*k
    end
  end
end;

function TFonts.xText_Width(cl: Integer; k: double; s: PWideChar): double;
var
  h: double; p: TPoint;
begin
  Result:=0;

  if vm_Active then begin
    h:=Height_Text(cl,k); k:=0;
    if hdr[6] > 0 then k:=h/hdr[6];
    p:=Point(0,0); if k > 0 then
    if Vgm_Wide(nil, p,p, 0,k,0, s) > 0 then
    Result:=Vgm_Size.x
  end
end;

function TFonts.Text_Width(cl: Integer; k: double; s: PChar): double;
var
  t: TWideStr;
begin
  Result:=0;
  if xStringToWideChar(s,t) <> nil then
  Result:=xText_Width(cl,k,t)
end;

function TFonts.xWidth_Text(h: double; s: PWideChar): double;
var
  k: double; p: TPoint;
begin
  Result:=0;

  if vm_Active then begin
    k:=0; Vgm_Read(ord('A')+1);
    if hdr[6] > 0 then k:=h/hdr[6];

    p:=Point(0,0); if k > 0 then
    if Vgm_Wide(nil, p,p, 0,k,0, s) > 0 then
    Result:=Vgm_Size.x
  end
end;

function TFonts.Width_Text(h: double; const s: string): double;
var
  t: TWideStr;
begin
  Result:=0;
  if xStringToWideChar(s,t) <> nil then
  Result:=xWidth_Text(h,t)
end;

function TFonts.Width_Wide(h: double; const s: WideString): double;
var
  t: TWideStr;
begin
  Result:=0;
  if xStringToWideChar(s,t) <> nil then
  Result:=xWidth_Text(h,t)
end;

function TFonts.Up_Text(h: double): double;
begin
  Result:=h; if vm_Active then begin
    Vgm_Read(ord('A')+1);
    Result:=hdr[4]*h/hdr[6]
  end
end;

function TFonts.Dn_Text(h: double): double;
begin
  Result:=0; if vm_Active then begin
    Vgm_Read(ord('A')+1); if hdr[4] < hdr[6] then
    Result:=(hdr[6]-hdr[4])*h/hdr[6]
  end
end;

procedure TFonts.Vgm_Skip(DC: XCanvas; const p: TPoint; cl,i: Integer);
begin
  if i <> ord(' ')+1 then begin
    hdr[3]:=0; hdr[4]:=hdr[6]-1;
    inherited Vgm_Skip(DC,p,cl,i)
  end
end;

procedure TFonts.Virt_reset;
begin
end;

function TExportFonts.exp_Symbols(ch: Widechar): Boolean;
begin
  Result:=false; if ch < 'z' then
  Result:=Char(ch) in ['J','K','L']
end;

procedure TExportFonts.exp_str(const p1,p2: tgauss;
                               h: double; const s: WideString);
begin
end;

procedure TExportFonts.exp_point(i: Integer; const p: tgauss);
begin
end;

procedure TExportFonts.exp_line(const p1,p2: tgauss);
begin
end;

procedure TExportFonts.exp_lWide(lp: PLLine;
                                 cl: Integer; k: Double;
                                 s: PWideChar);

function exp_item(const p1,p2: TPoint;
                  u,k,g: float; s: PWideChar): Integer;

function x_text(const p: tgauss; fi,h: double;
                var s: WideString): tgauss;
begin
  Result:=p;
  if length(s) > 0 then begin
    Result:=prj_gauss(p,Width_Wide(h,s)*Comp,fi);
    if not exp_Symbols(s[1]) then
    exp_str(p,Result, h, s)
  end; s:=''
end;

var
  i,id,dx,w: Integer; a,b: TPoint;
  p,q: tgauss; h,fi: Double;
  ch: WideChar; t: WideString;
begin
  Result:=0; if vm_Active then begin

    Vgm_Read(ord('A')+1); h:=hdr[6]*k;

    a:=p1; b:=p2; fi:=iAngle(p1,p2);

    SetLength(t,1024); t:='';

    if u > 0 then begin
      if (a.x = b.x) and (a.y = b.y) then begin
        Inc(a.y,Round(u)); b.y:=a.y
      end
      else begin
        Up_Point(p1,p2, u, a);
        Inc(b.x,a.x-p1.x); Inc(b.y,a.y-p1.y);
      end
    end;

    t_t[1,1]:=k; t_t[1,2]:=Sin(g)/Cos(g)*k; t_t[1,3]:=0;
    t_t[2,1]:=0; t_t[2,2]:=k;               t_t[2,3]:=0;
    t_t[3,1]:=0; t_t[3,2]:=0;               t_t[3,3]:=1;

    xy_Rotate_3x3(t_t,b.x-a.x,b.y-a.y);

    p:=_Gauss(a.x,a.y); q:=p;
    dx:=0; w:=0; k:=0;

    i:=0; while i < 256 do begin

      ch:=s[i]; Inc(i);
      if ch = #0 then Break;

      if ch = '~' then begin
        x_text(p,fi,h,t); k:=k-4/3;
        p:=y_move(p.x,p.y,4/3);
        Dec(w,dx); dx:=0
      end else
      if ch = '^' then begin
        x_text(p,fi,h,t);
        p:=y_move(q.x,q.y,-2/3);
        k:=k+2/3; dx:=0
      end else
      if ch = '@' then begin
        x_text(p,fi,h,t);
        u:=AtoI(s[i],2)/100; Inc(i,2);
        p:=y_move(q.x,q.y,-u); k:=k+u; dx:=0
      end else
      if ch = '_' then begin
        x_text(p,fi,h,t);
        exp_Line(y_move(p.x,p.y,0.1),
                 y_move(q.x,q.y,0.1))
      end
      else begin
        if exp_Symbols(ch) then begin

          p:=x_text(p,fi,h,t); t:=ch;

          if ch = 'J' then begin
            id:=37; if i <= length(s) then
            if s[i] = 'L' then begin
              id:=39; t:=t+'L'; Inc(i)
            end
          end else
          if ch = 'L' then begin
            id:=38; if i <= length(s) then
            if s[i] = 'J' then begin
              id:=32; t:=t+'J'; Inc(i)
            end
          end else id:=40;

          exp_point(id,p);
          p:=x_text(p,fi,h,t); t:=''
        end

        else t:=t+ch;

        Vgm_Read(ord(ch)+1);
        Dec(hdr[5],hdr[3]); Inc(dx,hdr[5]);
        Inc(w,hdr[5]); Result:=Max(Result,w)
      end;

      q.x:=p.x + t_t[1,1]*dx;
      q.y:=p.y - t_t[2,1]*dx
    end;

    x_text(p, fi,h,t)
  end
end;

function str_width(s: PWidechar; k: Double): Double;
var
  i: Integer; ch: WideChar;
begin
  Result:=0;
  for i:=0 to 255 do begin
    ch:=s[i]; if ch = #0 then Break;
    if Vgm_Read(ord(ch)+1) > 0 then
    Result:=Result + hdr[5]*k;
  end;
end;

procedure exp_by_letter(lp: PLLine; lt: Integer;
                        top,space, h,k,g: Double;
                        s: PWidechar);
var
  i: Integer; len,dx: Double;
  ch: array[0..1] of WideChar;
  v: LVector; a,b: TPoint;
begin
  len:=top; ch[1]:=#0;
  for i:=0 to 255 do begin
    ch[0]:=s[i]; if ch[0] = #0 then Break;

    if Vgm_Read(ord(ch[0])+1) > 0 then begin

      dx:=(hdr[5]-hdr[3])*k/2; with lp^ do
      Poly_dist_Vector(@Pol,N,len+dx,dx+dx,false,v);

      Get_Next_Point(v[0],v[1],-dx,a); b:=v[1];

      len:=len + exp_Item(a,b, h,k,g, ch)*k;

      if lt = 1 then
        len:=len+space
      else
      if lt = 6 then
      if ch[0] = ' ' then
        len:=len+space
    end
  end;
end;

procedure exp_by_poly(lp: PLLine;
                      top, h,k,g: Double;
                      s: PWidechar);
var
  i,j,ind,tlen: Integer; len,dx,dx2: Double;
  ch: array[0..1] of WideChar; v: LVector;
  a,b: TPoint; t: TWideStr;
begin
  t[0]:=#0; tlen:=0; ind:=-1;

  len:=top; ch[1]:=#0;
  for i:=0 to 255 do begin
    ch[0]:=s[i]; if ch[0] = #0 then Break;

    if Vgm_Read(ord(ch[0])+1) > 0 then begin

      dx2:=(hdr[5]-hdr[3])*k; dx:=dx2/2;

      with lp^ do
      j:=Poly_dist_Vector(@Pol,N,len+dx,dx2,false,v);

      if (j <> ind) or (tlen = 0) then begin
        if tlen > 0 then exp_Item(a,b, h,k,g, t);
        Get_Next_Point(v[0],v[1],-dx,a); ind:=j;
        t[0]:=#0; tlen:=0
      end;

      t[tlen]:=ch[0]; Inc(tlen);
      t[tlen]:=#0; b:=v[1];

      len:=len + dx2
    end
  end;

  if tlen > 0 then
  exp_Item(a,b, h,k,g, t)
end;

procedure exp_lp_str(lp: PLLine; top,len,kz,h: Double;
                     const dn: tgauss; const s: WideString);
var
  dx: Double; v: LVector;
  a,b: TGauss;
begin
  if Length(s) > 0 then begin
    Vgm_Read(ord('A')+1);
    dx:=(hdr[5]-hdr[3])*kz/2;

    Poly_dist_Vector(@lp.Pol,lp.N,top+dx,len,false,v);

    a:=_Gauss(v[0].x,v[0].y);
    b:=_Gauss(v[1].x,v[1].y);
    a:=Next_Gauss(a,b, -dx);

    a.x:=a.x + dn.x; a.y:=a.y + dn.y;
    b.x:=b.x + dn.x; b.y:=b.y + dn.y;

    exp_str(a,b, h,s)
  end
end;

procedure str_words(str: PWidechar;
                    lp: PLLine; h,kz: Double);
var
  len,space,top,ax,w,blank: Double;
  i: Integer; s: WideString; p: PWidechar;
  dn,txt: tgauss; ch: Widechar;
begin
  w:=hdr[6]*kz;
  blank:=Width_Text(w,' ');

  len:=PolyLength(@lp.Pol,lp.N); with lp^ do
  dn:=Stretch_dir(Pol[0],Pol[1],h);

  space:=blank_space(str,len,kz);

  top:=0; ax:=0; txt.x:=0; txt.y:=0;

  s:=''; p:=str;

  for i:=0 to 1023 do begin
    ch:=p[0]; p:=@p[1];
    if ch = #0 then Break;

    if ch = ' ' then begin

      if space >= blank then begin
        exp_lp_str(lp, top,ax,kz,h, dn,s);
        top:=top + ax + space; ax:=0;
        s:=''; Continue
      end

    end else
    if ch = '^' then begin
      exp_lp_str(lp, top,ax,kz,h, dn,s);
      top:=0; ax:=0; s:='';

      space:=blank_space(p,len,kz);
      txt.x:=txt.x + dn.x;
      txt.y:=txt.y + dn.y;
      Continue
    end;

    ax:=ax + Letter_Width(ord(ch),kz);
    s:=s + ch
  end;

  exp_lp_str(lp, top,ax,kz,h, dn,s)
end;

procedure exp_vgt(lp: PLLine;
                  cl: Integer; k: Double;
                  s: PWideChar);
var
  h,len,space,top: double;
  i,width,lt,up: Integer; a,b,lp_p: TPoint;
  ch: array[0..1] of WideChar; txt: tgauss;
  p: PWideChar; L: VLLine;
begin
  if s[0] <> #0 then
  if lp.N >= 0 then begin ch[1]:=#0;

    up:=tlong(cl).b[2]; cl:=tlong(cl).w[0];

    k:=Scale_vgt(cl,k);

    width:=(cl shr 4) and 3 + 1; cl:=cl and 15;
    if (cl <> 7) and (k < 1) then width:=1;

    lt:=(up shr 2) and 7; up:=up and 3;

    h:=0; if up > 0 then begin
      h:=hdr[6]*k; if Odd(up) then h:=h/2
    end;

    a:=lp.Pol[0]; b:=a;
    if lp.N > 0 then b:=lp.Pol[1];

    if (lt = 0) and (lp.N > 1) then
    if str_width(s,k) <= Long_Dist(a,b) then
    lp.N:=1;

    if (lp.N <= 1) and (lt in [0,2,3]) then
      exp_Item(a,b, h,k,0, s)
    else
    if lt in [2,3] then begin

      p:=s; b:=lp.Pol[0]; i:=1;

      with lp^ do
      while i <= N do begin
        if p[0] = #0 then Break;
        a:=b; b:=Pol[i];

        if i = N then
          exp_Item(a,b, h,k,0, p)
        else begin
          ch[0]:=p[0]; p:=@p[1];
          exp_Item(a,b, h,k,0, ch);
        end;

        Inc(i); if lt = 3 then begin
          b:=Pol[i]; if i < N then Inc(i)
        end
      end

    end
    else begin
      if lp.N = 0 then begin
        L.Pol[0]:=a; L.Pol[1]:=b;
        L.N:=1; lp:=@L
      end;

      if lt = 6 then
        str_words(s,lp,h,k)
      else begin

        lp_p:=lp.Pol[0]; space:=0; top:=0;

        if lt > 0 then begin

          len:=PolyLength(@lp.Pol,lp.N);

          txt.x:=0; txt.y:=0;
          if Vgm_Wide(nil, a,b, h,k,0, s) > 0 then
          txt:=Vgm_Size;

          case lt of
        1:  begin
              i:=WideStrLen(s); if i > 1 then
              space:=(len-txt.x)/(i-1);

              if space < Width_Text(h,' ') then
              space:=0;
            end;
        4,
        5:  begin
              if lt = 5 then begin
                len:=len/2; txt.x:=txt.x/2
              end;

              top:=len-txt.x; if top < 0 then begin
                Get_Next_Point(a,b,top,lp.Pol[0]); top:=0;
              end
            end
          end
        end;

        if fIsMif then
        if lt in [0,4,5] then begin

          if lt = 0 then begin
            txt.x:=0; txt.y:=0;
            if Vgm_Wide(nil, a,b, h,k,0, s) > 0 then
            txt:=Vgm_Size;
          end;

          line_backup(lp,top,a);
          line_backup(lp,top+txt.x,b);
          exp_Item(a,b, h,k,0, s);
          lt:=-1
        end;

        if lt >= 0 then begin

          a:=lp.Pol[0]; b:=lp.Pol[1];

          if lp.N > 1 then begin
            if not fIsSxf and (lt in [4,5]) then  
              exp_by_poly(lp, top,h,k,0, s)
            else
              exp_by_letter(lp,lt, top,space, h,k,0, s)
          end else
          if space < 1 then
            exp_Item(a,b, h,k,0, s)
          else
          if lt = 1 then
            exp_by_letter(lp,lt, top,space, h,k,0, s);
        end;

        lp.Pol[0]:=lp_p
      end
    end
  end
end;

var
  ttf: TTrueTypeFont;
  fnt: Integer; fn: TShortStr;
begin
  if s[0] <> #0 then
  if lp.N >= 0 then begin

    fnt:=Get_Font_Index(cl);
    StrPCopy(fn,Fnt_FNames[fnt]);

    if fn[0] <> '*' then begin
      if Open_vgt(fnt) then
      exp_vgt(lp,cl,k,s)
    end else

    if open_ttf(@fn[1],cl,k) then
      fTrueTypes.PolyText(lp,cl,s);
  end
end;

procedure TExportFonts.exp_ltext(lp: PLLine;
                                 cl: Integer; k: Double;
                                 s: PChar);
var
  t: TWideStr;
begin
  if xStringToWideChar(Strpas(s),t) <> nil then
  exp_LWide(lp, cl,k, t);
end;

constructor TDrawFonts.Create(Adraw: IDrawText);
begin
  inherited Create(1,Adraw);

  x_LPoints:=Alloc_LLine(LPointsMax);
  x_Counters:=xAllocInt(CountersMax);
  if x_LPoints <> nil then x_LPoints.N:=0;
end;

destructor TDrawFonts.Destroy;
begin
  xFreePtr(x_LPoints);
  xFreePtr(x_Counters);
  inherited
end;

procedure TDrawFonts.Vgm_Start(DC: XCanvas);
begin
  if x_LPoints <> nil then
  x_LPoints.N:=0; x_cnt:=0
end;

procedure TDrawFonts.Vgm_Poly(DC: XCanvas; lp: pLPoly;
                              N, Cl,Tag: Integer);
begin
  if not DC.Fill then begin
    Vgm_Close(DC); DC.PolyLine(pPoly(lp),N)
  end else
  if x_LPoints = nil then
    DC.Polygon(pPoly(lp),N)
  else begin
    Inc(N); if x_cnt = CountersMax then Vgm_Close(DC);
    if x_LPoints.N+N > LPointsMax then Vgm_Close(DC);

    Move(lp^,x_LPoints.Pol[x_LPoints.N],SizeOf(TPoint)*N);
    Inc(x_LPoints.N,N); x_Counters[x_cnt]:=N; Inc(x_cnt);
  end
end;

procedure TDrawFonts.Vgm_Close(DC: XCanvas);
begin
  if x_cnt > 0 then with x_LPoints^ do begin
    DC.PolyPolygon(@Pol,x_Counters,x_cnt); N:=0
  end; x_cnt:=0
end;

constructor TFontsRect.Create(lp: PLLine; lp_Max: Integer);
begin
  inherited Create(0,nil); fbp:=lp;
  if fbp <> nil then fbp.N:=-1;
  fPolyInd:=2; fPolyMax:=lp_Max;
  MinH:=0; MaxH:=MaxInt;
  flt.X:=1; flt.Y:=1;
end;

procedure TFontsRect.Clear_bound;
begin
  flt.X:=1; flt.Y:=1;
  frb.X:=0; frb.Y:=0;
  if fbp <> nil then fbp.N:=-1;
  fPolyInd:=2;
end;

procedure TFontsRect.Virt_reset;
begin
  Clear_bound;
end;

procedure TFontsRect.Vgm_Poly(DC: XCanvas; lp: PLPoly;
                              N, Cl,Tag: Integer);
begin
end;

procedure TFontsRect.Virt_Rect(Sender: TObject);
var
  i,ind: integer; lt1,rb1: TPoint;
begin
  Max_Poly_Bound(@v_Rect,4,lt1,rb1);
  if flt.X > frb.X then begin
    flt:=lt1; frb:=rb1;
  end
  else begin
    flt.X:=Min(flt.X,lt1.X);
    flt.Y:=Min(flt.Y,lt1.Y);
    frb.X:=Max(frb.X,rb1.X);
    frb.Y:=Max(frb.Y,rb1.Y);
  end;

  if fbp <> nil then
  if fPolyMax >= 4 then begin

    with fbp^ do if N = -1 then begin
      N:=4; for i:=0 to 4 do Pol[i]:=v_Rect[i]
    end
    else begin ind:=fPolyInd;
      Inc(fPolyInd,2); for i:=1 to 4 do
      ind:=Poly_Insert(fbp,ind,fPolyMax,v_Rect[i]);
    end

  end
end;

constructor TFontsRect1.Create;
begin
  inherited Create(@fbuf,LPoly_Max)
end;

procedure TFontsBound.Vgm_Poly(DC: XCanvas; lp: PLPoly;
                               N, Cl,Tag: Integer);
var
  _lt,_rb: TPoint;
begin
  if N >= 0 then begin
    Max_Poly_Bound(lp,N+1, _lt,_rb);

    if Count = 0 then begin
      flt:=_lt; frb:=_rb
    end
    else Add_lRect(flt,frb, _lt,_rb);

    Inc(Count)
  end
end;

initialization
begin
  Init_Heights;
  Init_FNames
end;

end.