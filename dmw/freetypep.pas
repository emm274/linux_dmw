unit freetypep; interface

uses
  math,sysutils, classes, contnrs,
  freetypeh, LCLType, otypes,
  idib, ofiles, xlist;

type
  FreeTypeException = class(exception);

  TBitmapType = (btBlackWhite, bt256Gray);

  PFontBitmap = ^TFontBitmap;
  TFontBitmap = record
    height,width,pitch,
    x,y, advanceX,advanceY: int;
    data: int
  end;

  TStringBitMaps = class(TCustomList)
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    procedure push(posX,posY: int; gl: PFT_Glyph);

    procedure Draw(const cv: ICanvas; aX,aY: int);

  private
    fData: TDataStream;

    fBounds : TRect;
    fText: WideString;
    fMode: TBitmapType;

    procedure CalculateGlobals;

  public
    property Text: WideString read fText;
    property Mode: TBitmapType read FMode;
    property Bounds: TRect read fBounds;
  end;

  PMgrGlyph = ^TMgrGlyph;
  TMgrGlyph = record
    Character: WideChar;
    GlyphIndex: FT_UInt;
    Glyph: PFT_Glyph;
  end;

  TFontGlyphList = class(TCustomList)
    constructor Create(aFont: PFT_Face);
    destructor Destroy; override;

    function GetGlyph(c: WideChar): PMgrGlyph;

  private
    fFont: PFT_Face;
  end;

  PMgrSize = ^TMgrSize;
  TMgrSize = record
    res,size: int;
    Glyphs: TFontGlyphList;
  end;

  TFontSizeList = class(TCustomList)
    constructor Create(aFont: PFT_Face);
    destructor Destroy; override;

    function GetSize(aSize,aRes: int): PMgrSize;
  private
    fFont: PFT_Face;
  end;

  TFontManager = class;

  TMgrFont = class
    constructor Create(aMgr: TFontManager;
                       aFilename: PChar; aIndex: int);

    destructor Destroy; override;

    function This(aFilename: PChar; aIndex: int): Boolean;

    function SetSize(aSize,aRes: int): Boolean;

  private
    fMgr: TFontManager;
    fFont: PFT_Face;
    fSizes: TFontSizeList;
    fCurSize: PMgrSize;
    fEnabled: bool;
    fFilename: TShortstr;

  public
    property Font: PFT_Face read fFont;
    property CurSize: PMgrSize read fCurSize;
  end;

  TFontManager = class
    constructor Create;
    destructor destroy; override;

    function GetFreeTypeFont(I: int): PFT_Face;

    function SetFont(aFilename: PChar; aIndex: int): int;

    function SetSize(aSize: int): Boolean;

    function GetExtent(Text: PWideChar): TPoint;

    function GetString(bmp: TStringBitmaps;
                       Text: PWideChar;
                       angle: float): int;

  private
    FTLib: PFT_Library;
    fEnabled: bool;

    fmajor,fminor,fpatch: int;

    fList: TObjectList;
    fResolution: integer;

    CurFont: TMgrFont;
    fRenderMode: FT_Render_Mode;
    UseKerning: bool;

    fSearchPath: TShortStr;
    fExtension: TShortStr;

  protected
    function GetFont(I: int): TMgrFont;

    function GetFontId(aFilename: PChar; aIndex: int): int;
    function CreateFont(aFilename: PChar; aIndex: int): int;

  public
    property Enabled: bool read fEnabled;

    property major: int read fmajor;
    property minor: int read fminor;
    property patch: int read fpatch;

    property Resolution: int read fResolution write fResolution;
  end;

  TDrawText = class(TInterfacedObject,IDrawText)
    constructor Create;
    destructor Destroy; override;

    function SetFont(FileName: PChar): int; stdcall;
    function SetSize(aSize: int): Boolean; stdcall;

    function GetExtent(Text: PWideChar): TPoint; stdcall;
    function GetExtentc(Char: WideChar): TPoint; stdcall;

    function Draw(const cv: ICanvas;
                  X,Y,Fi: int; Text: PWideChar): Boolean; stdcall;

  private
    fonts: TFontManager;
    bmp: TStringBitmaps;
  end;

function GetDrawText(out Obj): Boolean;

procedure dump_face(txt: TTextfile; face: PFT_FACE);

implementation

uses
  convert,wstrings,xlog;

function GetDrawText(out Obj): Boolean;
var
  tobj: TDrawText;
begin
  Result:=false; TPointer(Obj):=0;

  tobj:=TDrawText.Create;
  try
    if tobj.GetInterface(IDrawText,Obj) then begin
      tobj:=nil; Result:=true;
    end;
  finally
    tobj.Free;
  end;
end;

{ TStringBitmaps }
constructor TStringBitmaps.Create;
begin
  inherited Create(Sizeof(TFontBitmap),32);
  fData:=TDataStream.Create(4096);
end;

destructor TStringBitmaps.Destroy;
begin
  fData.Free;
  inherited;
end;

procedure TStringBitmaps.Clear;
begin
  inherited Clear;
  fData.Size:=0;
end;

procedure TStringBitmaps.push(posX,posY: int; gl: PFT_Glyph);
var
  bm: PFT_BitmapGlyph;
  si,di: PBytes; i,bx,cx: int;
  bmp: TFontBitmap;
  reverse: bool;
begin
  // Copy what is needed to record
  bm:=PFT_BitmapGlyph(gl);

  with gl.advance do begin
    bmp.advanceX:=x div 64;
    bmp.advanceY:=y div 64;
  end;

  with bm^ do begin
    bmp.height:=bitmap.rows;
    bmp.width:=bitmap.width;

    bmp.x:=(posX div 64) + left;   // transformed bitmap has correct x,y
    bmp.y:=(posY div 64) - top;    // not transformed has only a relative correction

    si:=PBytes(bitmap.buffer);
    bx:=bitmap.pitch; reverse:=(bx < 0);
    bx:=Abs(bx); cx:=bx*bmp.height;

    bmp.pitch:=bx;
    bmp.data:=fData.Size;
    if fData.Expand(cx) then begin

      di:=fData.Get_Pointer(bmp.data);

      if not reverse then
        Move(si^,di^,cx)
      else begin
        si:=@si[(bmp.height-1)*bx];

        for i:=1 to bmp.height do begin
          move(si^,di^,bx); di:=@di[bx]
        end
      end;

      Add(@bmp);
    end;
  end;
end;

procedure TStringBitmaps.Draw(const cv: ICanvas; aX,aY: int);
var
  i: int; b: PFontBitmap; top,bp: PBytes;
begin
  top:=fData.Buffer;

  b:=First;
  for i:=1 to Count do begin
    bp:=@top[b.data];

    if mode = btBlackWhite then
       cv.DrawCharBW(aX+b.x, aY+b.y, bp, b.width,b.height,b.pitch)
    else
       cv.drawChar(aX+b.x, aY+b.y, bp, b.width,b.height,b.pitch);

    Inc(b)
  end;
end;

procedure TStringBitmaps.CalculateGlobals;
const
  test = false;
type
  PFontBitmaps = ^TFontBitmaps;
  TFontBitmaps = Array[0..127] of TFontBitmap;
var
  i,n1,t: int; bp: PFontBitmaps; r: TRect; s: String;
begin
  n1:=Count-1;
  if n1 >= 0 then begin

    bp:=First;

    if test then begin
      std_log.WriteStr(fText);
      std_log.WriteStr('{');

      for i:=0 to Count-1 do begin
        with bp[i] do
        s:=Format('%dx%d {%d %d} {%d %d}',
          [width,height, x,y, advanceX,advanceY]);
        std_log.WriteStr(s);
      end;

      std_log.WriteStr('}');
    end;

    // check first 2 bitmaps for left side
    // check last 2 bitmaps for right side
    with bp[0] do begin
      r.left:=x;
      r.top:=y + height;
      r.bottom:=y;
    end;

    with bp[n1] do
    r.right:=x + width;

    if n1 > 0 then begin

      t:=bp[1].x;
      if t < r.left then r.left:=t;

      with bp[n1-1] do t:=x + width;

      if t > r.right then r.right:=t;
      end;

    // check top/bottom of other bitmaps
    for i:=1 to n1 do
    with bp[i] do begin
      t:=y+height;
      if r.top < t then r.top:=t;
      if r.bottom > y then r.bottom:=y;
    end;

    if test then begin
      s:=Format('rect: %d %d %d %d',[r.Left,r.Top,r.Right,r.Bottom]);
      std_log.WriteStr(s); std_log.WriteStr('');
    end;

    fBounds:=r
  end;
end;

const
  sErrErrorsInCleanup : string = '%d errors detected while freeing a Font Manager object';
  sErrFontFileNotFound : string = 'Font file "%s" not found';
  sErrFreeType : string = 'Error %d while %s';
  sInitializing : string = 'initializing font engine';
  sDestroying : string = 'destroying font engine';
  sErrErrorInCleanup : string = 'freeing Font Manager object';
  sErrSetPixelSize : string = 'setting pixel size %d (resolution %d)';
  sErrSetCharSize : string = 'setting char size %d (resolution %d)';
  sErrLoadingGlyph : string = 'loading glyph';
  sErrKerning : string = 'determining kerning distance';
  sErrMakingString1 : string = 'making string bitmaps step 1';
  sErrMakingString2 : string = 'making string bitmaps step 2';
  sErrMakingString3 : string = 'making string bitmaps step 3';
  sErrMakingString4 : string = 'making string bitmaps step 4';
  sErrLoadFont : string = 'loading font %d from file %s';
  sErrInitializing : string = 'initializing FreeType';
  sErrDestroying : string = 'finalizing FreeType';

procedure FTError(Event:string; Err:integer);
begin
  raise FreeTypeException.CreateFmt (sErrFreeType, [Err,Event]);
end;

Function FTCheck(Res: Integer; Msg:string) : Integer;

begin
  Result:=Res;
  If (Result<>0) then
    FTError(Msg,Result);
end;

procedure dump_face(txt: TTextfile; face: PFT_FACE);
var
  i: int; s: TShortstr;
begin
  txt.Write_int('num_faces',face.num_faces);
  txt.Write_int('face_index',face.face_index);
  txt.Write_hex('face_flags',face.face_flags,8);
  txt.Write_hex('style_flags',face.style_flags,8);

  txt.Write_int('num_glyphs',face.num_glyphs);

  txt.Write_str2('family_name',face.family_name);
  txt.Write_str2('style_name',face.style_name);

  txt.Write_int('num_fixed_sizes',face.num_fixed_sizes);

  if Assigned(face.available_sizes) then
  for i:=0 to face.num_fixed_sizes-1 do begin
    StrFmt(s,#9'%d',[i+1]);
    with face.available_sizes[i] do
    txt.Write_str(s,Format('%d x %d',[width,height]));
  end;
(*
  writeln(' num_charmaps=',CurFont^.num_charmaps);
  writeln(' charmaps=',CurFont^.charmaps<>nil);
  writeln(' generic.data=',CurFont^.generic.data<>nil);
  //writeln(' generic.finalizer=',CurFont^.generic.finalizer<>nil);
  writeln(' bbox.xMin=',CurFont^.bbox.xMin,
    ' bbox.xMax=',CurFont^.bbox.xMax,
    ' bbox.yMin=',CurFont^.bbox.yMin,
    ' bbox.yMax=',CurFont^.bbox.yMax,
    ' units_per_EM=',CurFont^.units_per_EM,
    ' ascender=',CurFont^.ascender,
    ' descender=',CurFont^.descender,
    ' height=',CurFont^.height,
    ' max_advance_width=',CurFont^.max_advance_width,
    ' max_advance_height=',CurFont^.max_advance_height,
    ' underline_position=',CurFont^.underline_position,
    ' underline_thickness=',CurFont^.underline_thickness,
    ' glyph=',CurFont^.glyph<>nil,
    ' size=',CurFont^.size<>nil,
    ' charmap=',CurFont^.charmap<>nil,
    '');
    *)
end;

constructor TFontGlyphList.Create(aFont: PFT_Face);
begin
  inherited Create(Sizeof(TMgrGlyph),32);
  fFont:=aFont
end;

destructor TFontGlyphList.Destroy;
var
  i: int; g: PMgrGlyph;
begin
  g:=Last;
  for i:=1 to Count do begin
    FT_Done_Glyph(g.Glyph);
    Dec(g)
  end;

  inherited
end;

function TFontGlyphList.GetGlyph(c: WideChar): PMgrGlyph;

function CreateGlyph(c: WideChar): PMgrGlyph;
var
  e: int; g: TMgrGlyph;
begin
  Result:=nil;

  g.character:=c;
  g.GlyphIndex:=FT_Get_Char_Index(fFont, ord(c));
  g.Glyph:=nil;

  e:=FT_Load_Glyph(fFont, g.GlyphIndex, FT_Load_Default);
  if e <> 0 then
    FTError(sErrLoadingGlyph, e)
  else begin
    e:=FT_Get_Glyph(fFont.glyph, g.Glyph);
    if e <> 0 then
      FTError(sErrLoadingGlyph, e)
    else
    if Add(@g) >= 0 then
      Result:=Last
  end
end;

var
  i: int; g: PMgrGlyph;
begin
  Result:=nil;

  g:=Last;
  for i:=1 to Count do begin
    if g.Character = c then begin
      Result:=g; Break
    end; Dec(g)
  end;

  if Result = nil then
  Result:=CreateGlyph(c)
end;

constructor TFontSizeList.Create(aFont: PFT_Face);
begin
  inherited Create(Sizeof(TMgrSize),16);
  fFont:=aFont
end;

destructor TFontSizeList.Destroy;
var
  i: int; s: PMgrSize;
begin
  s:=Last;
  for i:=1 to Count do begin
    s.Glyphs.Free; Dec(s)
  end;

  inherited
end;

function TFontSizeList.GetSize(aSize,aRes: int): PMgrSize;

function IsFixedSize(aSize: int): int;
var
  i: int; s: FT_Bitmap_Size;
begin
  Result:=-1;

  with fFont^ do begin
    for i:=Num_fixed_sizes-1 downto 0 do begin
      s:=available_sizes[i];
      if (s.height = aSize) and
         (s.width = aSize) then begin
        Result:=i; Break
      end;
    end;

    if Result >= 0 then
    raise FreeTypeException.CreateFmt ('Size %d not available for %s %s',
                [aSize, style_name, family_name]);
  end;
end;

function SetPixelSize(aSize,aRes: int): Boolean;
var
  e,s: int;
begin
  Result:=false;

  with fFont^ do
  if (face_flags and FT_Face_Flag_Fixed_Sizes) <> 0 then begin

    if IsFixedSize(aSize) < 0 then begin
      e:=FT_Set_pixel_sizes(fFont, aSize,aSize);
      if e = 0 then Result:=true else
      FTError(format(sErrSetPixelSize,[aSize,aRes]), e)
    end
  end
  else begin
    s:=aSize shl 6;
    e:=FT_Set_char_size(fFont, s,s, aRes,aRes);
    if e = 0 then Result:=true else
    FTError(format(sErrSetCharSize,[aSize,aRes]), e);
  end;
end;

var
  i: int; s,t: PMgrSize; r: TMgrSize;
begin
  s:=nil; t:=Last;
  for i:=1 to Count do begin
    if (t.size = aSize) and
       (t.res = aRes) then begin
      s:=t; Break
    end; Dec(t)
  end;

  if s = nil then
  if SetPixelSize(aSize,aRes) then begin
    r.res:=aRes; r.size:=aSize;
    r.Glyphs:=TFontGlyphList.Create(fFont);
    if Add(@r) >= 0 then s:=Last
  end;

  Result:=s
end;

{ TMgrFont }
constructor TMgrFont.Create(aMgr: TFontManager;
                            aFilename: PChar;
                            aIndex: int);
var
  e: int;
begin
  inherited Create;

  StrLCopy(fFilename,aFilename,255);
  StrLower(fFilename);

  fMgr:=aMgr;
  e:=FT_New_Face(aMgr.FTLib, fFilename, aIndex, fFont);
  if e <> 0 then
    FTError(Format(sErrLoadFont,[aIndex,aFilename]),e)
  else begin
    fSizes:=TFontSizeList.Create(fFont);
    fEnabled:=true
  end;
end;

destructor TMgrFont.Destroy;
begin
  fSizes.Free;
  inherited
end;

function TMgrFont.This(aFilename: PChar; aIndex: int): Boolean;
begin
  Result:=false;
  if StrComp(fFileName,aFilename) = 0 then
  if fFont.face_index = aIndex then
  Result:=true
end;

function TMgrFont.SetSize(aSize,aRes: int): Boolean;
var
  s: PMgrSize;
begin
  Result:=false;

  s:=fCurSize;

  if Assigned(s) then
  if (s.size <> aSize)
  or (s.res <> aRes) then
  s:=nil;

  if s = nil then
  s:=fSizes.GetSize(aSize,aRes);

  if Assigned(s) then begin
    fCurSize:=s; Result:=true
  end
end;

{ TFontManager }

constructor TFontManager.Create;
var
  rc: int;
begin
  inherited;

  fList:=TObjectList.Create;

  rc:=FT_Init_FreeType(FTLib);
  fEnabled:=rc = 0;

  if rc <> 0 then begin
    FTLib:=nil;
    FTError(sErrInitializing, rc);
  end
  else begin
    FT_Library_Version(FTLib,fmajor,fminor,fpatch)
  end;

  UsrDir(fSearchPath,'/fonts');
  StrCopy(fExtension,'.ttf');

  fRenderMode:=FT_RENDER_MODE_NORMAL;

  Resolution:=96
end;

destructor TFontManager.Destroy;
var
  rc: int;
begin
  fList.Free;

  if assigned(FTLib) then begin
    rc:=FT_Done_FreeType(FTlib);
    if rc <> 0 then
    FTError(sErrDestroying,rc);
  end;

  inherited
end;

function TFontManager.GetFont(I: int): TMgrFont;
begin
  Result:=nil;
  if (I >= 0) and (I < fList.Count) then
  Result:=fList[I] as TMgrFont
end;

function TFontManager.GetFreeTypeFont(I: int): PFT_Face;
var
  f: TMgrFont;
begin
  Result:=nil;
  f:=GetFont(I);
  if Assigned(f) then
  Result:=f.font;
end;

function TFontManager.SetFont(aFilename: PChar; aIndex: int): int;

function SearchFont(Path,FName: PChar): Boolean;
var
  fn: TShortstr;
begin
  StrUpdateExt(fn,FName,fExtension);
  StrPath(Path,fSearchPath,fn);
  Result:=FileExist(Path)
end;

var
  fn: TShortstr;
begin
  Result:=-1;

  if Assigned(aFilename) then
  if SearchFont(fn,aFilename) then begin
    Result:=GetFontID(fn,aIndex);
    if Result < 0 then
    Result:=CreateFont(fn,aIndex);
  end;

  if Result >= 0 then
  CurFont:=GetFont(Result);
end;

function TFontManager.GetFontId(aFilename: PChar; aIndex: int): int;
var
  i: int; fn: TShortstr;
begin
  Result:=-1;

  StrCopy(fn,aFileName);
  StrLower(fn);

  for i:=fList.Count-1 downto 0 do
  if (fList[i] as TMgrFont).This(fn,aIndex) then begin
    Result:=i; Break
  end
end;

function TFontManager.CreateFont(aFilename: PChar; aIndex: int): int;
var
  f: TMgrFont;
begin
  f:=TMgrFont.Create (self, aFilename,aIndex);
  result:=fList.Count; fList.Add(f);
end;

function TFontManager.SetSize(aSize: int): Boolean;
var
  f: TMgrFont;
begin
  Result:=false;

  f:=CurFont;
  if Assigned(f) then
  if f.SetSize(aSize,fResolution) then begin
    UseKerning:=(f.font.face_flags and FT_FACE_FLAG_KERNING) <> 0;
    Result:=true
  end
end;

function TFontManager.GetExtent(Text: PWideChar): TPoint;
var
  size: PMgrSize;
  Glyphs: TFontGlyphList;
  g: PMgrGlyph; gl: PFT_Glyph;
  s: TPoint; pos,kern: FT_Vector;
  i,e, prevx,prevIndex: int; ch: WideChar;
begin
  s.X:=0; s.Y:=0;

  size:=nil;
  if Assigned(CurFont) then
  size:=CurFont.CurSize;

  if Assigned(size) then begin

    Glyphs:=size.Glyphs;

    prevIndex:=0;
    prevx:=0;
    pos.x:=0;
    pos.y:=0;

    for i:=1 to 256 do begin
      ch:=Text^; Inc(Text);
      if ch = #0 then Break;

      // retrieve loaded glyph
      g:=Glyphs.GetGlyph(ch);
      if Assigned(g) then begin

        // check kerning
        if UseKerning and (g.glyphindex <> 0) and (PrevIndex <> 0) then begin
          prevx:=pos.x;
          e:=FT_Get_Kerning(Curfont.Font, prevIndex, g.GlyphIndex, ft_kerning_default, kern);
          if e <> 0 then FTError(sErrKerning, e);
          Inc(pos.x,kern.x shl 11);
        end;

        // render the glyph
        FTCheck(FT_Glyph_Copy (g.glyph, gl),sErrMakingString1);

        Inc(pos.x,gl.advance.x);
        if prevx > pos.x then pos.x:=prevx;
        pos.y:=Max(pos.y,gl.advance.y);

        // finish rendered glyph
        FT_Done_Glyph(gl)
      end;

      s.X:=pos.x shr 11;
      s.Y:=pos.y shr 11;
    end;

  end;

  Result:=s
end;

function TFontManager.GetString(bmp: TStringBitmaps;
                                Text: PWideChar;
                                angle: float): int;

procedure MakeTransformation(angle: float; out Transformation: FT_Matrix);
var
  c,s: extended;
begin
  SinCos(angle, s,c);

  with Transformation do begin
    xx:=Round( c*$10000);
    xy:=Round(-s*$10000);
    yx:=Round( s*$10000);
    yy:=Round( c*$10000);
  end;
end;

procedure MakeString0(Glyphs: TFontGlyphList;
                      bmp: TStringBitmaps;
                      Text: PWideChar);
var
  g: PMgrGlyph;
  gl: PFT_Glyph;
  i,e, prevIndex, prevx: int;
  pos,kern: FT_Vector; ch: WideChar;
begin
  prevIndex:=0;
  prevx:=0;
  pos.x:=0;
  pos.y:=0;

  for i:=1 to 256 do begin
    ch:=Text^; Inc(Text);
    if ch = #0 then Break;

    // retrieve loaded glyph
    g:=Glyphs.GetGlyph(ch);
    if Assigned(g) then begin

      // check kerning
      if UseKerning and (g.glyphindex <> 0) and (PrevIndex <> 0) then begin
        prevx:=pos.x;
        e:=FT_Get_Kerning(Curfont.Font, prevIndex, g.GlyphIndex, ft_kerning_default, kern);
        if e <> 0 then FTError(sErrKerning, e);
        Inc(pos.x,kern.x);
      end;

      // render the glyph
      FTCheck(FT_Glyph_Copy (g.glyph, gl),sErrMakingString1);
      FTCheck(FT_Glyph_To_Bitmap(gl, fRenderMode, @pos, true),sErrMakingString4);

      bmp.push(pos.x,pos.y,gl);

      Inc(pos.x,gl.advance.x shr 11);
      if prevx > pos.x then pos.x:=prevx;

      // finish rendered glyph
      FT_Done_Glyph(gl)
    end;
  end;
end;

var
  g: PMgrGlyph;
  gl: PFT_Glyph;
  size: PMgrSize;
  Glyphs: TFontGlyphList;
  e, prevIndex, prevx, i: int;
  pre,adv,pos,kern: FT_Vector;
  tr: FT_Matrix; ch: WideChar;
begin
  bmp.Clear;

  size:=nil;
  if Assigned(CurFont) then
  size:=CurFont.CurSize;

  if Assigned(size) then begin

    Glyphs:=size.Glyphs;

    bmp.fText:=StrPasWW(Text);

    if (fRenderMode = FT_RENDER_MODE_MONO) then
      bmp.fMode:=btBlackWhite
    else
      bmp.fMode:=bt256Gray;

    if  (Angle = 0) or   // no angle asked, or can't work with angles (not scalable)
        ((CurFont.Font.face_flags and FT_FACE_FLAG_SCALABLE)=0) then

      MakeString0(Glyphs,bmp,Text)

    else begin
      MakeTransformation(angle, tr);

      prevIndex := 0;
      prevx := 0;
      pos.x := 0;
      pos.y := 0;
      pre.x := 0;
      pre.y := 0;

      for i:=1 to 256 do begin
        ch:=Text^; Inc(Text);
        if ch = #0 then Break;

        // retrieve loaded glyph
        g:=Glyphs.GetGlyph(ch);
        if Assigned(g) then begin

          // check kerning
          if UseKerning and (g.glyphindex <> 0) and (PrevIndex <> 0) then begin
            prevx:=pre.x;
            e:=FT_Get_Kerning(Curfont.Font, prevIndex, g.GlyphIndex, ft_kerning_default, kern);
            if e <> 0 then FTCheck(e,sErrKerning);
            Inc(pre.x,kern.x);
          end;

          // render the glyph
          Gl:=nil;
          FTCheck(FT_Glyph_Copy(g.glyph, gl),sErrMakingString1);
          // placing the glyph
          FTCheck(FT_Glyph_Transform(gl, nil, @pre),sErrMakingString2);
          adv := gl.advance;
          // rotating the glyph
          FTCheck(FT_Glyph_Transform(gl, @tr, nil),sErrMakingString3);
          // rendering the glyph
          FTCheck(FT_Glyph_To_Bitmap(gl, fRenderMode, nil, true),sErrMakingString4);

          bmp.push(0,0,gl);

          // place position for next glyph
          with gl.advance do begin
            Inc(pos.x,x div 1024);
            Inc(pos.y,y div 1024);
          end;

          Inc(pre.x,adv.x div 1024);
          if prevx > pre.x then pre.x:=prevx;

          // finish rendered glyph
          FT_Done_Glyph(gl);
        end;
      end;
    end;

    bmp.FText:=Text;
    bmp.CalculateGlobals;
  end;

  Result:=bmp.Count;
end;

constructor TDrawText.Create;
begin
  inherited Create;
  fonts:=TFontManager.Create;
  bmp:=TStringBitmaps.Create;
end;

destructor TDrawText.Destroy;
begin
  bmp.Free;
  fonts.Free;
  inherited
end;

function TDrawText.SetFont(FileName: PChar): int;
begin
  Result:=fonts.SetFont(FileName,0);
end;

function TDrawText.SetSize(aSize: int): Boolean;
begin
  Result:=fonts.SetSize(aSize);
end;

function TDrawText.GetExtent(Text: PWideChar): TPoint;
var
  s: TPoint; r: TRect;
begin
  s.X:=0; s.Y:=0;

  if Assigned(Text) then
    s:=fonts.GetExtent(Text)
  else
  if bmp.Count > 0 then begin
    r:=bmp.Bounds;
    s.X:=r.Right-r.Left;
    s.Y:=r.Top-r.Bottom;
  end;

  Result:=s
end;

function TDrawText.GetExtentc(Char: WideChar): TPoint;
var
  s: Array[0..1] of WideChar;
begin
  s[0]:=Char; s[1]:=#0;
  Result:=fonts.GetExtent(s)
end;

function TDrawText.Draw(const cv: ICanvas;
                        X,Y,Fi: int;
                        Text: PWideChar): Boolean;
begin
  Result:=false;

  if fonts.GetString(bmp,Text,Fi/10*DegToRad) > 0 then begin
    bmp.Draw(cv,X,Y);
    Result:=true
  end;
end;

end.