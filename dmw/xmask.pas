unit xmask; interface

uses
  Classes,LCLType,
  Math,otypes,img_x,xplot;

type
  TBitMask = class
    constructor Create(AIsByte: bool);
    destructor Destroy; override;

    procedure Alloc_dc(AWidth,AHeight: int);
    procedure Free_dc;

    procedure Realloc_dc(AWidth,AHeight: Integer);

    procedure Add_mask(Mask: TBitMask);

    procedure bits_Polygon(Bits: XBits; lp: PLLine);

    function PolyPolygon(lp: PPoly; cp: PIntegers; N: Integer): Boolean;
    function Polygon(lp: PPoly; N: Integer): Integer;

    procedure PolyPolyline(lp: PPoly; cp: PIntegers; N: Integer);
    procedure Polyline(lp: PPoly; N: Integer);

    procedure horz_Line(x1,x2,y: Integer);

    function IsPixel(x,y: Integer): Boolean;

    function GetPixel(x,y: int): int;
    procedure SetPixel(x,y: int);

    procedure Pat_Mask(pat: int);
    procedure Off_Mask;

    procedure Fill_Mask;
    procedure Clear_Mask;

    procedure Fill_map32(const map: Bitmap; pat,color: int);

    procedure Fill_map(const map: Bitmap; color: int);

    procedure alfa_Polygon(map: PBitmap;
                           lp: PPoly; cp: PIntegers;
                           N, alfa1,alfa2,cl: int;
                           at: PPoint; ak: float);

    function ThickPolyLine(lp: PPoly; N: int; delta: float): int;

  protected
    bmInfo: Bitmap;

    procedure Set_ClipRect(R: PRect); virtual;

    procedure Line(lp: PPoly);

  private
    fthick: TThickPolyline;

    bmSize: int;

    fMaskRect: TRect;
    fMinX,fMinY,fMaxX,fMaxY: int;

    fPos: TPoint;
    fPosCount: int;
    fPosRect: TRect;
    fPosLocked: bool;

    fIsByte: bool;
    fIsClutter: bool;

    function Get_Active: Boolean;

    function Get_ScanLine(Row: int): Pointer;

    function Get_Enabled: Boolean;

    procedure endRegion(x1,y1,x2,y2: int);

    procedure thickPoint(x,y: int);
    procedure thickBreak;

  public
    property Active: Boolean read Get_Active;

    property Width: int read bmInfo.bmWidth;
    property Height: int read bmInfo.bmHeight;
    property WidthBytes: int read bmInfo.bmWidthBytes;

    property IsClutter: bool write fIsClutter;

    property ScanLine[Row: int]: Pointer read Get_ScanLine;

    property ClipRect: PRect write Set_ClipRect;

    property MaskRect: TRect read fMaskRect write fMaskRect;

    property Enabled: Boolean read Get_Enabled;
  end;

  TTextMask = class
    constructor Create;
    destructor Destroy; override;

    function Alloc(w,h: Integer): Boolean;
    function Polygon(lp: PPoly; N: Integer): Boolean;

  private
    fMask1: TBitMask;
    fMask2: TBitMask;
  end;

procedure dib_Patterns_Init;
procedure dib_Patterns_LoadFrom(Path: PChar);
procedure dib_Patterns_SaveAs(Path: PChar);

implementation

uses
  Graphics,
  SysUtils,
  convert,ofiles,
  xddw,xline;

var
  FillMasks: PFillMasks;

procedure dib_Patterns_Init;
begin
  ddw_Patterns_Init;
end;

procedure dib_Patterns_LoadFrom(Path: PChar);

procedure bmp_LoadFrom(Path: PChar);
var
  bmp: TBitmap;
  x,y,al,bl: Integer;
  fp: PFillMask;
begin
  bmp:=TBitmap.Create;
  try
    bmp.LoadFromFile(Path);
    if bmp.HandleAllocated then
    if bmp.Width = 16*64 then
    if bmp.Height = 16 then begin bl:=0;

      Fillchar(FillMasks^,Sizeof(TFillMasks),0);

      for y:=0 to 15 do
      for x:=0 to 16*64-1 do begin

        al:=bmp.Canvas.Pixels[x,y];
        if (x = 0) and (y = 0) then bl:=al;

        if al = bl then begin
          fp:=@FillMasks[x div 16];
          fp[y]:=fp[y] or (1 shl (x mod 16));
        end
      end;

      dib_Patterns_Init
    end;
  finally
    bmp.Free
  end
end;

var
  h: Integer; fn: TShortstr;
begin
  if StrLen(Path) > 0 then
  if Assigned(FillMasks) then

  if This_ext(Path,'.bmp') then

    bmp_LoadFrom(Path)

  else begin
    StrUpdateExt(fn,Path,'.pat');
    h:=FileOpen(StrPas(fn),fmOpenRead);

    if h > 0 then begin
      FileRead(h,FillMasks^,SizeOf(TFillMasks));
      FileClose(h); dib_Patterns_Init
    end
  end
end;

procedure dib_Patterns_SaveAs(Path: PChar);

procedure pat_saveAs(Path: PChar);
var
  h: Integer; fn: TShortstr;
begin
  if StrUpdateExt(fn,Path,'.pat') <> nil then begin

    h:=FileCreate(fn); if h > 0 then begin
      FileWrite(h,FillMasks^,SizeOf(TFillMasks));
      FileClose(h)
    end
  end
end;

procedure bmp_saveAs(Path: PChar);
begin
end;

begin
  if Assigned(FillMasks) then
  if StrLen(Path) > 0 then

  if This_ext(Path,'.bmp') then bmp_saveAs(Path)
  else                          pat_saveAs(Path)
end;

constructor TBitMask.Create(AIsByte: bool);
begin
  inherited Create;
  fthick:=TThickPolyline.Create(thickPoint,thickBreak);
  fIsByte:=true
end;

destructor TBitMask.Destroy;
begin
  Free_dc;
  fthick.Free;
  inherited
end;

procedure TBitMask.Alloc_dc(AWidth,AHeight: int);
var
  map: Bitmap; cx: int;
begin
  Fillchar(map,Sizeof(map),0); cx:=0;

  if AWidth*AHeight > 0 then
  with map do begin
    bmType:=0;
    bmWidth:=AWidth;
    bmHeight:=AHeight;

    if fIsByte then begin
      bmWidthBytes:=int_Round(AWidth,4);
      bmBitsPixel:=8;
    end
    else begin
      bmWidthBytes:=int_Round(AWidth,32) div 8;
      bmBitsPixel:=1;
    end;

    bmPlanes:=1;
    cx:=bmWidthBytes * AHeight;
    map.bmBits:=bmInfo.bmBits;
  end;

  if (bmInfo.bmBits = nil)
  or (cx = 0) or (cx > bmSize) then begin

    Free_dc;
    if cx > 0 then begin
      map.bmBits:=xAllocPtr(cx);
      bmSize:=cx
    end
  end;

  bmInfo:=map;

  with bmInfo do
  if Assigned(bmBits) then
  FillChar(bmBits^,bmSize,0);

  fMinX:=0; fMaxX:=Width-1;
  fMinY:=0; fMaxY:=Height-1;

  FMaskRect:=Rect(Width,Height,0,0)
end;

procedure TBitMask.Free_dc;
begin
  if Assigned(bmInfo.bmBits) then
  bmInfo.bmBits:=xFreePtr(bmInfo.bmBits);
  FillChar(bmInfo,SizeOf(bmInfo),0);
  bmSize:=0
end;

procedure TBitMask.Realloc_dc(AWidth,AHeight: Integer);
begin
  if (bmInfo.bmBits = nil)
  or (bmInfo.bmWidth <> AWidth)
  or (bmInfo.bmHeight <> AHeight) then
  Alloc_dc(AWidth,AHeight)
end;

function TBitMask.Get_ScanLine(Row: int): Pointer;
begin
  Result:=nil;
  with bmInfo do if bmBits <> nil then
  Result:=@PBytes(bmBits)[Row*bmWidthBytes]
end;

function TBitMask.Get_Active: Boolean;
begin
  Result:=Assigned(bmInfo.bmBits)
end;

procedure TBitMask.Fill_Mask;
begin
  with bmInfo do
  if Assigned(bmBits) then
  FillChar(bmBits^,bmSize,255);
  fMaskRect:=Rect(Width,Height,0,0)
end;

procedure TBitMask.Clear_Mask;
begin
  with bmInfo do
  if Assigned(bmBits) then
  FillChar(bmBits^,bmSize,0);
  fMaskRect:=Rect(Width,Height,0,0)
end;

procedure TBitMask.Add_mask(Mask: TBitMask);
var
  si,di: PBytes; i: Integer;
begin
  if Active then

  if Mask.Active then
  if Mask.fIsByte = fIsByte then
  if Mask.Width = Width then
  if Mask.Height = Height then begin

    si:=Mask.bmInfo.bmBits;
    di:=bmInfo.bmBits;

    for i:=1 to bmSize do begin
      di[0]:=di[0] or si[0];
      di:=@di[1]; si:=@si[1];
    end
  end
end;

{$ASMMODE INTEL}

procedure TBitMask.Pat_Mask(Pat: int);
var
  x1,y1,x2,y2, d_si, dx_cx,dy_cx: int;
  _si: PByte;  mask: TFillMask; _bit,_clu: byte;
begin
  with MaskRect do begin
    x1:=Left; x2:=Right;
    y1:=Top; y2:=Bottom;
  end;

  if x1 <= x2 then
  if y1 <= y2 then begin

    _si:=ScanLine[y1];
    d_si:=WidthBytes;

    if fIsByte then begin
      Inc(_si,x1); _bit:=1
    end
    else begin
      Inc(_si,x1 shr 3);
      _bit:=B_Bit[x1 and 7];
    end;

    dx_cx:=x2-x1+1; dy_cx:=y2-y1+1;
    mask:=FillMasks[pat];

    _clu:=3;
    // только по левая граница
    if fIsClutter then _clu:=1;

    if _si <> nil then

    if fIsByte then
      asm      // byte
{$ifdef CPUX86_64}
{$else}
        push EAx
        push EBx
        push ECx
        push EDx
        push ESi

        mov  ESi,[_si]

        mov  ECx,[dy_cx]

        mov  EBx,[y1]
        and  EBx,15

      @loopy:
        push EBx
        push ECx
        push ESi

        add  EBx,EBx
        mov  Dx,WORD PTR mask[EBx]
        mov  ECx,[x1]
        and  ECx,15
        rol  Dx,Cl

        mov  Ax,0
        mov  ECx,[dx_cx]

      @loopx:
        cmp BYTE PTR [ESi],0
        je   @drawx
        mov  BYTE PTR [ESi],0
        inc  Ax
        and  Ax,[_clu]

      @drawx:

        cmp  Ax,0
        jz   @skipx

        rol  Dx,1
        jnc  @nextx

        mov  BYTE PTR [ESi],1
        jmp  @nextx

      @skipx:
        rol  Dx,1
      @nextx:
        and  Ax,1
        inc  ESi
        loop @loopx

        pop  ESi
        pop  ECx
        pop  EBx

        add  ESi,[d_si]

        inc  EBx
        and  EBx,15

        loop @loopy

        pop  ESi
        pop  EDx
        pop  ECx
        pop  EBx
        pop  EAx
{$endif}
      end

    else
      asm    // bit
{$ifdef CPUX86_64}
{$else}
        push EAx
        push EBx
        push ECx
        push EDx
        push ESi

        mov  ESi,[_si]

        mov  ECx,[dy_cx]

        mov  EBx,[y1]
        and  EBx,15

      @loopy:
        push EBx
        push ECx
        push ESi

        add  EBx,EBx
        mov  Dx,WORD PTR mask[EBx]
        mov  ECx,[x1]
        and  ECx,15
        rol  Dx,Cl

        mov  Ah,0
        mov  Al,[_bit]
        mov  ECx,[dx_cx]

      @loopx:
        test BYTE PTR [ESi],Al
        jz   @drawx
        xor  BYTE PTR [ESi],Al
        inc  Ah
        and  Ah,[_clu]

      @drawx:

        cmp  Ah,0
        jz   @skipx

        rol  Dx,1
        jnc  @nextx

        or   BYTE PTR [ESi],Al
        jmp  @nextx

      @skipx:
        rol  Dx,1
      @nextx:
        and  Ah,1
        ror  Al,1
        jnc  @skipc
        mov  Al,80h
        inc  ESi
      @skipc:
        loop @loopx

        pop  ESi
        pop  ECx
        pop  EBx

        add  ESi,[d_si]

        inc  EBx
        and  EBx,15

        loop @loopy

        pop  ESi
        pop  EDx
        pop  ECx
        pop  EBx
        pop  EAx
{$endif}
      end
  end
end;

procedure TBitMask.Fill_map32(const map: Bitmap; pat,color: int);
var
  x1,y1,x2,y2, d_si,d_di, w,h: int;
  _si: PByte; _di: pint; mask: TFillMask;
  _bit: byte;
begin
  with MaskRect do begin
    x1:=Left; x2:=Right;
    y1:=Top; y2:=Bottom;
  end;

  if map.bmWidth = Width then
  if map.bmHeight = Height then

  if x1 <= x2 then
  if y1 <= y2 then begin

    _si:=ScanLine[y1];
    d_si:=WidthBytes;

    if fIsByte then begin
      Inc(_si,x1); _bit:=1;
    end
    else begin
      Inc(_si,x1 shr 3);
      _bit:=B_Bit[x1 and 7];
    end;

    w:=x2-x1+1; h:=y2-y1+1;

    d_di:=map.bmWidthBytes;
    _di:=@PBytes(map.bmBits)[d_di*y1 + x1*4];
    Dec(d_di,w*4);

    mask:=FillMasks[pat];
    if _si <> nil then

    if fIsByte then

    asm  // byte
{$ifdef CPUX86_64}
      push RAx
      push RBx
      push RCx
      push RDx
      push RSi
      push RDi
      push R8
      push R9
      push R10

      mov  RSi,[_si]
      mov  RDi,[_di]

      xor  R8,R8
      mov  R8d,[d_si]

      xor  R9,R9
      mov  R9d,[d_di]

      mov  R10d,[color]

      xor  RBx,RBx
      xor  RCx,RCx

      mov  ECx,[h]

      mov  EBx,[y1]
      and  EBx,15

    @loopy:
      push RBx
      push RCx
      push RSi

      add  RBx,RBx
      mov  Dx,WORD PTR mask[RBx]
      mov  ECx,[x1]
      and  ECx,15
      rol  Dx,Cl

      mov  Ax,0
      mov  ECx,[w]

    @loopx:
      cmp  BYTE PTR [RSi],0
      je   @drawx
      mov  BYTE PTR [RSi],0
      inc  Ax
      and  Ax,3

   @drawx:
     cmp  Ax,0
     jz   @skipx

     rol  Dx,1
     jnc  @nextx

     mov  DWORD PTR [RDi],R10d
     jmp  @nextx

   @skipx:
     rol  Dx,1
   @nextx:
     and  Ax,1
     inc  RSi
     add  RDi,4
     loop @loopx

     pop  RSi
     pop  RCx
     pop  RBx

     add  RSi,R8
     add  RDi,R9

     inc  EBx
     and  EBx,15

     loop @loopy

     pop  R10
     pop  R9
     pop  R8
     pop  RDi
     pop  RSi
     pop  RDx
     pop  RCx
     pop  RBx
     pop  RAx

{$else}
      push EAx
      push EBx
      push ECx
      push EDx
      push ESi
      push EDi

      mov  ESi,[_si]
      mov  EDi,[_di]

      mov  ECx,[h]

      mov  EBx,[y1]
      and  EBx,15

    @loopy:
      push EBx
      push ECx
      push ESi

      add  EBx,EBx
      mov  Dx,WORD PTR mask[EBx]
      mov  ECx,[x1]
      and  ECx,15
      rol  Dx,Cl

      mov  Ax,0
      mov  ECx,[w]

      mov  EBx,DWORD PTR [color]

    @loopx:
      cmp  BYTE PTR [ESi],0
      je   @drawx
      mov  BYTE PTR [ESi],0
      inc  Ax
      and  Ax,3

    @drawx:

      cmp  Ax,0
      jz   @skipx

      rol  Dx,1
      jnc  @nextx

      mov  DWORD PTR [EDi],EBx
      jmp  @nextx

    @skipx:
      rol  Dx,1
    @nextx:
      and  Ax,1
      inc  ESi
      add  EDi,4
      loop @loopx

      pop  ESi
      pop  ECx
      pop  EBx

      add  ESi,[d_si]
      add  EDi,[d_di]

      inc  EBx
      and  EBx,15

      loop @loopy

      pop  EDi
      pop  ESi
      pop  EDx
      pop  ECx
      pop  EBx
      pop  EAx
{$endif}
    end

    else

      asm  // bit
{$ifdef CPUX86_64}
{$else}
        push EAx
        push EBx
        push ECx
        push EDx
        push ESi
        push EDi

        mov  ESi,[_si]
        mov  EDi,[_di]

        mov  ECx,[h]

        mov  EBx,[y1]
        and  EBx,15

      @loopy:
        push EBx
        push ECx
        push ESi

        add  EBx,EBx
        mov  Dx,WORD PTR mask[EBx]
        mov  ECx,[x1]
        and  ECx,15
        rol  Dx,Cl

        mov  Ah,0
        mov  Al,[_bit]
        mov  ECx,[w]

        mov  EBx,DWORD PTR [color]

      @loopx:
        test BYTE PTR [ESi],Al
        jz   @drawx
        xor  BYTE PTR [ESi],Al
        inc  Ah
        and  Ah,3

      @drawx:

        cmp  Ah,0
        jz   @skipx

        rol  Dx,1
        jnc  @nextx

        mov  DWORD PTR [EDi],EBx
        jmp  @nextx

      @skipx:
        rol  Dx,1
      @nextx:
        and  Ah,1
        ror  Al,1
        jnc  @skipc
        mov  Al,80h
        inc  ESi
      @skipc:
        add  EDi,4
        loop @loopx

        pop  ESi
        pop  ECx
        pop  EBx

        add  ESi,[d_si]
        add  EDi,[d_di]

        inc  EBx
        and  EBx,15

        loop @loopy

        pop  EDi
        pop  ESi
        pop  EDx
        pop  ECx
        pop  EBx
        pop  EAx
{$endif}
      end
  end
end;

procedure TBitMask.Off_Mask;
var
  x,x1,x2,y: Integer; line: PBytes;
begin
  x1:=fMaskRect.Left div 8;
  x2:=fMaskRect.Right div 8;

  with fMaskRect do
  for y:=Top to Bottom do begin

    line:=ScanLine[y];
    if line <> nil then
    for x:=x1 to x2 do line[x]:=0;
  end;

  fMaskRect:=Rect(Width,Height,0,0)
end;

procedure TBitMask.Set_ClipRect(R: PRect);
begin
  if R = nil then begin
    FMinX:=0; FMaxX:=Width-1;
    FMinY:=0; FMaxY:=Height-1
  end else
  with R^ do begin
    FMinX:=Max(0,Left); FMaxX:=Min(Width-1,Right);
    FMinY:=Max(0,Top); FMaxY:=Min(Height-1,Bottom)
  end
end;

procedure TBitMask.endRegion(x1,y1,x2,y2: int);
begin
  if x1 < fminX then x1:=fminX;
  if x2 > fmaxX then x2:=fmaxX;
  if y1 < fminY then y1:=fminY;
  if y2 > fmaxY then y2:=fmaxY;

  with fMaskRect do begin
    Left:=Min(Left,x1); Right:=Max(Right,x2);
    Top:=Min(Top,y1); Bottom:=Max(Bottom,y2)
  end;
end;

function TBitMask.Get_Enabled: Boolean;
begin
  with FMaskRect do
  Result:=(Left < Right) and (Top < Bottom)
end;

procedure TBitMask.bits_Polygon(Bits: XBits; lp: PLLine);
begin
  if not fIsByte then
  with bmInfo do if bmBits <> nil then begin
    with lp^ do Polygon(@Pol,N); Pat_Mask(0);
    with lp^ do Polyline(@Pol,N);

    Bits.xor_bits(bmBits,fMaskRect);
    Clear_Mask
  end
end;

function TBitMask.PolyPolygon(lp: PPoly; cp: PIntegers; N: Integer): Boolean;
var
  i,cx: Integer;
begin
  Result:=false;

  if cp = nil then begin if N > 2 then
    if Points_Equal(lp[0],lp[N]) then begin
      Polygon(lp,N); Result:=true
    end
  end else

  if N > 0 then begin

    for i:=0 to N-1 do begin
      cx:=cp[i]; Polygon(lp,cx-1);
      lp:=@lp[cx]
    end;

    Result:=true
  end
end;

procedure TBitMask.Line(lp: PPoly);
var
  ix,ctg: double; x,y,w: int;
  line: PBytes; a,b: TPoint;
begin
  a:=lp[0]; b:=lp[1];
  if a.Y > b.Y then begin
    b:=a; a:=lp[1];
  end;

  if a.Y < b.Y then
  if a.Y <= fMaxY then
  if b.Y >= fMinY then begin
    ix:=a.X; ctg:=(b.X-a.X)/(b.Y-a.Y);

    if a.Y < fMinY then begin
      ix:=ix + (fMinY-a.Y)*ctg; a.Y:=fMinY
    end;

    Dec(b.Y); if b.Y > fMaxY then b.Y:=fMaxY;

    w:=WidthBytes; line:=ScanLine[a.Y];

    for y:=a.Y to b.Y do begin

      x:=Round(ix); ix:=ix+ctg;

      if x < fMinX then x:=fMinX;
      if x > fMaxX then x:=fMaxX;

      if fIsByte then begin
        line[x]:=line[x] xor 1;
        line:=@line[w]
      end

      else
        asm      // bit
{$ifdef CPUX86_64}
{$else}
          push EAx
          push EBx
          push EDi

          mov  EDi,[line]
          push EDi

          mov  EBx,[x]
          mov  EAx,EBx
          shr  EAx,3
          add  EDi,EAx
          and  EBx,7

          mov  Al,BYTE PTR B_Bit[EBx]
          xor  BYTE PTR [EDi],Al

          pop  EDi
          add  EDi,[w]
          mov  [line],EDi

          pop  EDi
          pop  EBx
          pop  EAx
{$endif}
        end

    end
  end
end;

function TBitMask.Polygon(lp: PPoly; N: int): int;
var
  i: int; lt,rb: TPoint;
begin
  if N > 2 then
  if bmInfo.bmBits <> nil then begin

    lt:=lp[0]; rb:=lt;

    for i:=1 to N do begin
      Line(lp); lp:=@lp[1];

      with lp[0] do begin
        if X < lt.X then lt.X:=X;
        if Y < lt.Y then lt.Y:=Y;
        if X > rb.X then rb.X:=X;
        if Y > rb.Y then rb.Y:=Y;
      end
    end;

    endRegion(lt.X,lt.Y,rb.X,rb.Y);

  end; Result:=N+1
end;

procedure TBitMask.thickPoint(x,y: int);
var
  l: LVector;
begin
  l[1].X:=x; l[1].Y:=y;

  with fPosRect do
  if fPosCount = 0 then begin
    Left:=x; Top:=y; Right:=x; Bottom:=y
  end
  else begin
    if not fPosLocked then begin
      l[0]:=fPos; Line(@l);
    end;

    if x < Left then Left:=x else
    if x > Right then Right:=x;

    if y < Top then Top:=y else
    if y > Bottom then Bottom:=y;
  end;

  fPos:=l[1]; Inc(fPosCount);
  fPosLocked:=false
end;

procedure TBitMask.thickBreak;
begin
  fPosLocked:=true
end;

function TBitMask.ThickPolyLine(lp: PPoly; N: int; delta: float): int;
begin
  fPosCount:=0;
  fPosLocked:=false;

  Result:=fthick.Polyline(lp,N, delta);

  with fPosRect do
  endRegion(Left,Top,Right,Bottom)
end;

procedure TBitMask.PolyPolyline(lp: PPoly; cp: PIntegers; N: Integer);
var
  i,cx,ind: Integer;
begin
  ind:=0;

  if cp = nil then
    Polyline(lp,N)
  else
  for i:=0 to N-1 do begin
    cx:=cp[i]; Polyline(@lp[ind],cx-1); Inc(ind,cx)
  end
end;

procedure TBitMask.Polyline(lp: PPoly; N: Integer);
var
  draw: TFigure; i: Integer; a,b: TPoint;
begin
  draw:=TFigure.Create;
  try
    draw.OnPixel:=SetPixel;

    b:=lp[0];
    for i:=1 to N do begin
      a:=b; b:=lp[i];
      draw.Line(a.X,a.Y,b.X,b.Y)
    end;

  finally
    draw.Free
  end
end;

procedure TBitMask.horz_Line(x1,x2,y: int);
var
  di: PBytes; p1,p2: PByte; cx: int;
begin
  with bmInfo do
  if Assigned(bmBits) then
  if (y >= 0) and (y < bmHeight) then begin

    if x1 < 0 then x1:=0;
    if x2 >= bmWidth then x2:=bmWidth-1;

    if x1 <= x2 then begin
      di:=@PBytes(bmBits)[y*bmWidthBytes];

      if fIsByte then
        Fillchar(di[x1],x2-x1+1,1)
      else begin
        p1:=@di[x1 div 8]; p2:=@di[x2 div 8];

        p1^:=p1^ or L_Msk[x1 and 7];

        if p1 = p2 then
          p1^:=p1^ and R_Msk[x2 and 7]
        else begin
          p2^:=p2^ or R_Msk[x2 and 7];
          Inc(p1); Dec(p2);

          cx:=TPointer(p2)-TPointer(p1)+1;
          if cx > 0 then Fillchar(p1^,cx,$FF)
        end
      end
    end
  end
end;

function TBitMask.IsPixel(x,y: Integer): Boolean;
var
  line: PBytes;
begin
  Result:=true;

  with bmInfo do
  if Assigned(bmBits) then begin

    line:=@PBytes(bmBits)[y*bmWidthBytes];

    if fIsByte then
      Result:=line[x] <> 0
    else
      Result:=line[x div 8] and B_Bit[x and 7] <> 0

  end
end;

function TBitMask.GetPixel(x,y: int): int;
var
  line: PBytes;
begin
  Result:=0;
  with bmInfo do
  if Assigned(bmBits) then
  if (x >= 0) and (x < bmWidth) then
  if (y >= 0) and (y < bmHeight) then begin

    line:=@PBytes(bmBits)[y*bmWidthBytes];

    if fIsByte then
      Result:=line[x]
    else
    if line[x div 8] and B_Bit[x and 7] <> 0 then
      Result:=1
  end
end;

procedure TBitMask.SetPixel(x,y: int);
var
  di: PByte;
begin
  with bmInfo do
  if Assigned(bmBits) then
  if (x >= 0) and (x < bmWidth) then
  if (y >= 0) and (y < bmHeight) then

  if fIsByte then
    PBytes(bmBits)[y*bmWidthBytes + x]:=1
  else begin
    di:=@PBytes(bmBits)[y*bmWidthBytes + (x div 8)];
    di^:=di^ or B_Bit[x and 7]
  end
end;

procedure TBitMask.Fill_map(const map: Bitmap; color: int);
var
  x,y,dx,h,bit,r,g,b,di_line: int;
  si: PByte; dest,di: PBytes; ip: pint;
  rt: TRect;
begin
  rt:=MaskRect;

  dest:=map.bmBits;
  di_line:=map.bmWidthBytes;

  if map.bmWidth = Width then
  if map.bmHeight = Height then

  if map.bmBitsPixel = 24 then begin

    r:=tlong(color).b[0];
    g:=tlong(color).b[1];
    b:=tlong(color).b[2];

    h:=Height;

    for y:=rt.Top to rt.Bottom do begin

      si:=ScanLine[y];
      if si <> nil then begin

        dx:=(h-1-y)*di_line + rt.Left*3;
        di:=@dest[dx];

        if fIsByte then begin
          si:=@si[rt.Left];

          for x:=rt.Left to rt.Right do begin
            if si^ <> 0 then begin
              di[0]:=b; di[1]:=g; di[2]:=r;
            end;

            Inc(si); di:=@di[3]
          end
        end
        else begin
          si:=@si[rt.Left div 8];
          bit:=B_Bit[rt.Left and 7];

          for x:=rt.Left to rt.Right do begin
            if si^ and bit <> 0 then begin
              di[0]:=b; di[1]:=g; di[2]:=r;
            end;

            bit:=bit shr 1; if bit = 0 then
            begin bit:=$80; Inc(si) end;

            di:=@di[3]
          end
        end

      end
    end

  end else
  if map.bmBitsPixel = 32 then begin

    if fIsByte then
    for y:=rt.Top to rt.Bottom do begin

      si:=ScanLine[y];
      if si <> nil then begin

        dx:=y*di_line + rt.Left*4;
        ip:=@dest[dx];

        if fIsByte then begin
          si:=@si[rt.Left];
          for x:=rt.Left to rt.Right do begin
            if si^ <> 0 then ip^:=color;
            Inc(si); Inc(ip)
          end
        end
        else begin
          si:=@si[rt.Left div 8];
          bit:=B_Bit[rt.Left and 7];

          for x:=rt.Left to rt.Right do begin
            if si^ and bit <> 0 then ip^:=color;

            bit:=bit shr 1; if bit = 0 then
            begin bit:=$80; Inc(si) end;

            Inc(ip)
          end
        end

      end
    end

  end
end;

procedure TBitMask.alfa_Polygon(map: PBitmap;
                                lp: PPoly; cp: PIntegers;
                                N, alfa1,alfa2,cl: int;
                                at: PPoint; ak: float);

procedure Fill_map(map: PBitmap; alfa,cl: int;
                   at: PPoint; ak: float);
var
  x,y,bx,dx,h,r,g,b,a,a1,bit: Integer;
  si,di: PBytes; cx,cy,vx,vy: double;
begin
  r:=tlong(cl).b[0];
  g:=tlong(cl).b[1];
  b:=tlong(cl).b[2];

  if at = nil then begin
    r:=alfa * r; g:=alfa * g; b:=alfa * b;
  end
  else begin
    cx:=at.X; cy:=at.Y
  end;

  a1:=256 - alfa;

  bx:=map.bmWidthBytes;
  h:=map.bmHeight;

  if Assigned(map.bmBits) then
  if map.bmWidth = Width then
  if map.bmHeight = Height then
  if map.bmBitsPixel = 24 then

  if at = nil then

    with MaskRect do
    for y:=Top to Bottom do begin

      si:=ScanLine[y];
      if si <> nil then begin

        dx:=(h-1-y)*bx + Left*3;
        di:=@PBytes(map.bmBits)[dx];

        if fIsByte then begin
          si:=@si[Left];

          for x:=Left to Right do begin
            if si[0] <> 0 then begin
              di[0]:=(a1*di[0] + b) shr 8;
              di[1]:=(a1*di[1] + g) shr 8;
              di[2]:=(a1*di[2] + r) shr 8;
            end;

            si:=@si[1]; di:=@di[3]
          end
        end
        else begin
          si:=@si[Left div 8];
          bit:=B_Bit[Left and 7];

          for x:=Left to Right do begin
            if si[0] and bit <> 0 then begin
              di[0]:=(a1*di[0] + b) shr 8;
              di[1]:=(a1*di[1] + g) shr 8;
              di[2]:=(a1*di[2] + r) shr 8;
            end;

            bit:=bit shr 1; if bit = 0 then
            begin bit:=$80; si:=@si[1] end;

            di:=@di[3]
          end
        end
      end
    end

  else

    with MaskRect do
    for y:=Top to Bottom do begin

      si:=ScanLine[y];
      if si <> nil then begin

        dx:=(h-1-y)*bx + Left*3;
        di:=@PBytes(map.bmBits)[dx];

        vy:=cy-y; vy:=vy*vy;

        if fIsByte then begin
          si:=@si[Left];

          for x:=Left to Right do begin

            vx:=cx-x;
            a:=alfa + Round(Sqrt(vx*vx+vy)*ak);
            a1:=256-a;

            if si[0] <> 0 then begin
              di[0]:=(a1*di[0] + a*b) shr 8;
              di[1]:=(a1*di[1] + a*g) shr 8;
              di[2]:=(a1*di[2] + a*r) shr 8;
            end;

            si:=@si[1]; di:=@di[3]
          end
        end
        else begin
          si:=@si[Left div 8];
          bit:=B_Bit[Left and 7];

          for x:=Left to Right do begin

            vx:=cx-x;
            a:=alfa + Round(Sqrt(vx*vx+vy)*ak);
            a1:=256-a;

            if si[0] and bit <> 0 then begin
              di[0]:=(a1*di[0] + a*b) shr 8;
              di[1]:=(a1*di[1] + a*g) shr 8;
              di[2]:=(a1*di[2] + a*r) shr 8;
            end;

            bit:=bit shr 1; if bit = 0 then
            begin bit:=$80; si:=@si[1] end;

            di:=@di[3]
          end
        end
      end
    end
end;

begin
  if PolyPolygon(lp,cp,N) then begin
    Pat_mask(0); if alfa2 < 0 then at:=nil;
    Fill_map(map,alfa1,cl,at,(alfa2-alfa1)*ak);
    Clear_Mask
  end;
end;

constructor TTextMask.Create;
begin
  inherited Create;
  fMask1:=TBitMask.Create(false);
  fMask2:=TBitMask.Create(false);
end;

destructor TTextMask.Destroy;
begin
  fMask2.Free;
  fMask1.Free;
  inherited
end;

function TTextMask.Alloc(w,h: Integer): Boolean;
begin
  fMask1.Realloc_dc(w,h);
  fMask2.Realloc_dc(w,h);
  Result:=fMask1.Active and fMask2.Active
end;

function TTextMask.Polygon(lp: PPoly; N: Integer): Boolean;
var
  x,y,bit,bx: Integer; lt,rb: TPoint;
  p1,p2: PBytes; R: TRect; up: Boolean;
begin
  Result:=true;

  fMask2.Polygon(lp,N);
  lt:=fMask2.MaskRect.TopLeft;
  rb:=fMask2.MaskRect.BottomRight;

  bx:=lt.X div 8;

  for y:=lt.Y to rb.Y do begin
    p1:=fMask1.ScanLine[y];
    p2:=fMask2.ScanLine[y];

    if Assigned(p1) then
    if Assigned(p2) then begin

      p1:=@p1[bx]; p2:=@p2[bx];
      bit:=B_Bit[lt.X and 7]; up:=false;

      for x:=lt.X to rb.X do begin

        if p2[0] and bit <> 0 then up:=not up;

        if up then
        if p1[0] and bit <> 0 then begin
          Result:=false; Break
        end;

        bit:=bit shr 1;
        if bit = 0 then begin
          bit:=$80; p1:=@p1[1]; p2:=@p2[1];
        end
      end;

      if not Result then Break
    end
  end;

  if Result then
  for y:=lt.Y to rb.Y do begin
    p1:=fMask1.ScanLine[y];
    p2:=fMask2.ScanLine[y];

    if Assigned(p1) then
    if Assigned(p2) then begin

      p1:=@p1[bx]; p2:=@p2[bx];
      bit:=B_Bit[lt.X and 7]; up:=false;

      for x:=lt.X to rb.X do begin

        if p2[0] and bit <> 0 then begin
          up:=not up; p2[0]:=p2[0] xor bit;
        end;

        if up then p1[0]:=p1[0] or bit;

        bit:=bit shr 1;
        if bit = 0 then begin
          bit:=$80; p1:=@p1[1]; p2:=@p2[1];
        end
      end;

      if not Result then Break
    end
  end;

  R:=Rect(fMask2.Width,fMask2.Height,0,0);
  fMask2.MaskRect:=R
end;

procedure init;
begin
  FillMasks:=ddw_Patterns_ptr;
  dib_Patterns_Init;
end;

initialization init;

end.
