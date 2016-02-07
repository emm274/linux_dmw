unit xmovie; interface

uses
  Classes,Graphics,Math,
  otypes,xdib,xdibcv,
  xclasses;

const
  movie_lift = 0;
  movie_draw = 1;
  movie_plot = 2;
  movie_show = 3;
  movie_hide = 4;

  movie_odd  = 1;
  movie_even = 2;
  movie_dot  = 4;
  movie_dash = 8;

type
  PMovieData = ^TMovieData;
  TMovieData = record
    DC: XCanvas;
    Bmp: TCanvas;
    Dib: XBitmap;
    Mode: Integer;
    Flags: Integer;
    Acc,Bit,Ind,Size: Integer;
    pen_Color: TColor;
    Buf: PColors;
  end;

  TMovie = class
    constructor Create(Buf: PColors; Size: Integer);

    procedure Reset; virtual;

    procedure Drag(Parent: TMovie;
                   PosX,PosY: Integer;
                   cmdShow: Boolean); virtual;

    procedure Begin_Paint(DC: XCanvas; Mode,Flags: Integer);

    procedure Line(X1,Y1,X2,Y2: Integer);
    procedure xLine(const a,b: TPoint);

    procedure PolyPolyline(lp: PPoly; cp: PIntegers; N: Integer);

    procedure Polyline(lp: PPoly; N: Integer);
    function Polyline1(lp: PPoly; N: Integer): Integer;
    function Polyline2(lp: PLLine): Integer;

    procedure Rectangle(X1,Y1,X2,Y2: Integer);

    procedure xCursor(X,Y,D1,D2,AColor: Integer);
    procedure xPoint(X,Y,D: Integer);
    procedure xCross(X,Y,D: Integer);

    function dc_Contains_Point(X,Y: Integer): Boolean;
    function dc_Contains_Line(X1,Y1,X2,Y2: Integer): Boolean;
    function dc_Clip_Line(X1,Y1,X2,Y2: Integer): Boolean;

  private
    fData: TMovieData;
    fThickLine: Longbool;
    fEnabled: Longbool;
    fVisible: Longbool;

    procedure Set_Bmp(Bmp: TCanvas);

    function Get_Dib: XBitmap;
    procedure Set_Dib(Dib: XBitmap);

    procedure Set_Color(Value: TColor);

  public
    property Data: TMovieData read fData;
    property Flags: Integer write fData.Flags;

    property Bmp: TCanvas write Set_Bmp;

    property Dib: XBitmap read Get_Dib write Set_Dib;

    property Color: TColor write Set_Color;

    property Enabled: Longbool read fEnabled write fEnabled;
    property Visible: Longbool read fVisible write fVisible;
    property ThickLine: Longbool write fThickLine;
  end;

  TMoviePoint = class(TMovie)
    constructor Create;
  private
    fBuf: TColors;
  end;

  TMovieLine = class(TMovie)
    constructor Create;
  private
    fBuf: array[0..4095] of int;
  end;

  TMovieRule = class(TMovieLine)
    procedure Draw(DC: XCanvas; lp: PLLine;
                   Mode,Flags: Integer);
  private
    procedure lp_Draw(lp: PLLine);
  end;

  TMovieDraft = class(TMovieLine)
    procedure Draw(DC: XCanvas;
                   lp: TMovieLPoints;
                   Mode,Flags: Integer);
  private
    procedure lp_Draw(lp: TMovieLPoints);
  end;

  TMovieCursor = class(TMoviePoint)

    procedure BeginPaint;
    procedure AfterPaint;

    procedure Move(X,Y,Cr: Integer);
    procedure Leave;
    procedure Show;
    procedure Hide;

    procedure Reset; override;

    function IsCross: Boolean;

  private
    fChild: TMovie;

    fCard: XCanvas;
    fRight: XCanvas;
    fCursor: Integer;
    fPos: TPoint;

    fxyz_dx: Integer;

    fPainting: Boolean;
    fActive: Boolean;
    fInside: Boolean;
    fStereo: Boolean;

    procedure Draw(Mode: Integer);
    procedure SetCursor(Cr: Integer);

  public
    property Card: XCanvas write fCard;
    property Right: XCanvas write fRight;
    property Cursor: Integer write SetCursor;

    property Child: TMovie write fChild;

    property Enabled: Longbool read fEnabled write fEnabled;
    property Active: Boolean read fActive write fActive;

    property Stereo: Boolean write fStereo;
    property xyz_dx: Integer write fxyz_dx;
  end;

implementation

uses
  LCLIntf,idib,
  xline,xcursors;

procedure dda_proc(X,Y: Integer; Data: PMovieData); stdcall;
var
  cv: ICanvas; i,cl,w,h: int; dup: bool;
begin
  cv:=Data.DC.Canvas;
  w:=cv.GetWidth;
  h:=cv.GetHeight;

  with Data^ do
  if (X >= 0) and (X < w) then begin

    Bit:=(Bit+1) and 1; Inc(Acc);

    if Flags and movie_even <> 0 then Y:=Y*2 else
    if Flags and movie_odd <> 0 then Y:=Y*2+1;

    dup:=Flags and (movie_odd+movie_even) <> 0;

    if (Y >= 0) and (Y < h) then
    for I:=1 to ord(dup)+1 do begin

      case Mode of
    movie_lift:
        if Dib = nil then
        if Ind < Size then begin
          cl:=cv.GetPixel(X,Y); if cl <> -1 then
          Buf[Ind]:=cl; Inc(Ind)
        end;

    movie_draw:
        if (Size = 0) or Assigned(Dib) then
          cv.SetPixel(X,Y,pen_Color)
        else
        if Ind < Size then begin
          cv.SetPixel(X,Y,pen_Color); Inc(Ind)
        end;

    movie_plot:
        if Flags and movie_dash <> 0 then begin
          if Acc mod 16 < 8 then cv.SetPixel(X,Y,pen_Color);
        end else

        if (Flags and movie_dot = 0)
        or Odd(Bit) then cv.SetPixel(X,Y,pen_Color);

    movie_show:
        begin
          if Ind < Size then begin
            cl:=cv.GetPixel(X,Y); if cl <> -1 then
            Buf[Ind]:=cl; Inc(Ind)
          end;

          if Flags and movie_dash <> 0 then begin
            if Acc mod 16 < 8 then cv.SetPixel(X,Y,pen_Color);
          end else

          if (Flags and movie_dot = 0)
          or Odd(Bit) then cv.SetPixel(X,Y,pen_Color);
        end;

    movie_hide:
        if Assigned(Bmp) then
          cv.SetPixel(X,Y,Bmp.Pixels[X,Y])
        else
        if Assigned(Dib) then begin
          if Dib.BackupPixel(X,Y,cl) then
          cv.SetPixel(X,Y,cl)
        end else
        if Ind < Size then begin
          cv.SetPixel(X,Y,Buf[Ind]);
          Inc(Ind)
        end;
      end;

      Inc(X)
    end
  end
end;

constructor TMovie.Create(Buf: PColors; Size: Integer);
begin
  inherited Create;

  fData.Buf:=Buf;
  fData.Size:=Size
end;

procedure TMovie.Drag(Parent: TMovie;
                      PosX,PosY: Integer;
                      cmdShow: Boolean);
begin
end;

procedure TMovie.Begin_Paint(DC: XCanvas; Mode,Flags: Integer);
begin
  fData.DC:=DC;
  fData.Mode:=Mode;
  fData.Flags:=Flags;
  fData.Ind:=0;
  fData.Bit:=0;
  fData.Acc:=0;

  fVisible:=(Mode = movie_show) or
            (Mode = movie_draw) or
            (Mode = movie_plot);

  fEnabled:=true
end;

procedure TMovie.Set_Bmp(Bmp: TCanvas);
begin
  fData.Bmp:=Bmp
end;

function TMovie.Get_Dib: XBitmap;
begin
  Result:=fData.Dib
end;

procedure TMovie.Set_Dib(Dib: XBitmap);
begin
  fData.Dib:=Dib
end;

procedure TMovie.Reset;
begin
  fVisible:=false
end;

procedure TMovie.Set_Color(Value: TColor);
begin
  if Assigned(fData.DC) then
  fData.DC.Canvas.SetPen(0,1,Value,0);
  fData.pen_Color:=Value
end;

procedure TMovie.Line(X1,Y1,X2,Y2: Integer);
begin
  if fData.Flags and (movie_even+movie_odd) <> 0 then
  begin Y1:=Y1 div 2; Y2:=Y2 div 2 end;

//  LineDDA(X1,Y1,X2,Y2, @dda_proc, Integer(@fData));

  if fThickLine then begin

    if Abs(X2-X1) > Abs(Y2-Y1) then begin
      Dec(Y1); Dec(Y2)
    end
    else begin
      Inc(X1); Inc(X2)
    end;

//    LineDDA(X1,Y1,X2,Y2, @dda_proc, Integer(@fData));
  end
end;

procedure TMovie.xLine(const a,b: TPoint);
var
  lt,rb, _a,_b: TPoint;
begin
  if dc_Contains_Line(a.X,a.Y,b.X,b.Y) then

  if dc_Clip_Line(a.X,a.Y,b.X,b.Y) then begin

    lt.X:=0; rb.X:=fData.DC.Width;
    lt.Y:=0; rb.Y:=fData.DC.Height;

    if xClip_Line(lt,rb, a,b, _a,_b) then
    Line(_a.X,_a.Y,_b.X,_b.Y)

  end else

  Line(a.X,a.Y,b.X,b.Y)
end;

procedure TMovie.PolyPolyline(lp: PPoly; cp: PIntegers; N: Integer);
var
  i,ind,cnt: Integer;
begin
  if Assigned(cp) then begin
    ind:=0; for i:=0 to N-1 do begin
      cnt:=cp[i]; PolyLine(@lp[ind],cnt-1);
      Inc(ind,cnt)
    end
  end else

  Polyline(lp,N)
end;

procedure TMovie.Polyline(lp: PPoly; N: Integer);
begin
  Polyline1(lp,N)
end;

function TMovie.Polyline1(lp: PPoly; N: Integer): Integer;
var
  i: Integer; p1,p2: TPoint;
begin
  if N > 0 then begin
    p2:=lp[0]; for i:=1 to N do begin
      p1:=p2; p2:=lp[i]; xLine(p1,p2)
    end
  end; Result:=N
end;

function TMovie.Polyline2(lp: PLLine): Integer;
begin
  Result:=Polyline1(@lp.Pol,lp.N);
end;

procedure TMovie.Rectangle(X1,Y1,X2,Y2: Integer);
begin
  if dc_Contains_Line(X1,Y1,X2,Y2) then begin
    xLine(Point(X1,Y1),Point(X2,Y1));
    xLine(Point(X1,Y2),Point(X2,Y2));
    xLine(Point(X1,Y1+1),Point(X1,Y2-1));
    xLine(Point(X2,Y1+1),Point(X2,Y2-1));
  end
end;

procedure TMovie.xCursor(X,Y,D1,D2,AColor: Integer);
begin
  if dc_Contains_Point(X,Y) then begin
    Color:=AColor;
    Line(X-D1,Y,X-D2,Y);
    Line(X,Y-D1,X,Y-D2);
    Line(X+D1,Y,X+D2,Y);
    Line(X,Y+D1,X,Y+D2);
  end
end;

procedure TMovie.xPoint(X,Y,D: Integer);
begin
  if dc_Contains_Point(X,Y) then begin
    Line(X-D,Y-D,X+D,Y-D);
    Line(X+D,Y-D,X+D,Y+D);
    Line(X-D,Y+D,X+D,Y+D);
    Line(X-D,Y-D,X-D,Y+D);
  end
end;

procedure TMovie.xCross(X,Y,D: Integer);
var
  x1,y1,x2,y2: Integer;
begin
  if dc_Contains_Point(X,Y) then begin
    x1:=x-d; y1:=y-d; x2:=x+d; y2:=y+d;
    xLine(Point(x1,y1),Point(x2+1,y2+1));
    xLine(Point(x1,y2),Point(x2+1,y1-1))
  end
end;

function TMovie.dc_Contains_Point(X,Y: Integer): Boolean;
begin
  Result:=false; with fData.DC do
  if (X > -32) and (X < Width+32) then
  if (Y > -32) and (Y < Height+32) then
  Result:=true
end;

function TMovie.dc_Contains_Line(X1,Y1,X2,Y2: Integer): Boolean;
var
  lt,rb: TPoint;
begin
  lt.x:=Min(X1,X2); lt.y:=Min(Y1,Y2);
  rb.x:=Max(X1,X2); rb.y:=Max(Y1,Y2);

  Result:=false; with fData.DC do
  if (lt.x <= Width+8) and (rb.x >= -8) then
  if (lt.y <= Height+8) and (rb.y >= -8) then
  Result:=true
end;

function TMovie.dc_Clip_Line(X1,Y1,X2,Y2: Integer): Boolean;
var
  lt,rb: TPoint;
begin
  lt.x:=Min(X1,X2); lt.y:=Min(Y1,Y2);
  rb.x:=Max(X1,X2); rb.y:=Max(Y1,Y2);

  Result:=false; with fData.DC do
  if (lt.x < 0) or (rb.x > Width)
  or (lt.y < 0) or (rb.y > Height) then

  Result:=true
end;

constructor TMoviePoint.Create;
begin
  inherited Create(@fBuf,256);
end;

constructor TMovieLine.Create;
begin
  inherited Create(@fBuf,Sizeof(fBuf) div 4)
end;

procedure TMovieRule.Draw(DC: XCanvas; lp: PLLine;
                          Mode,Flags: Integer);
begin
  if not Assigned(Data.Dib) then
  if Mode = movie_show then begin
    Begin_Paint(DC,movie_lift,Flags);
    lp_Draw(lp); Mode:=movie_draw
  end;

  Begin_Paint(DC,Mode,Flags);
  lp_Draw(lp)
end;

procedure TMovieRule.lp_Draw(lp: PLLine);
begin
  with lp^ do
  if N >= 0 then begin Color:=clLime;
    if N = 1 then xLine(Pol[0],Pol[1]);
    with Pol[0] do xCursor(X,Y,2,8,clBlue); if N = 1 then
    with Pol[1] do xCursor(X,Y,2,8,clRed)
  end
end;

procedure TMovieDraft.Draw(DC: XCanvas;
                           lp: TMovieLPoints;
                           Mode,Flags: Integer);
begin
  if Mode = movie_show then begin
    Begin_Paint(DC,movie_lift,Flags);
    lp_Draw(lp); Mode:=movie_draw
  end;

  Begin_Paint(DC,Mode,Flags);
  lp_Draw(lp)
end;

procedure TMovieDraft.lp_Draw(lp: TMovieLPoints);
var
  i: Integer; v: TLLine;
begin
  for i:=0 to lp.x_Count-1 do
  if lp.Get_Item(i,@v,LPoly_Max) > 1 then begin
    Color:=lp.Colors[i]; Polyline2(@v);
  end;

  for i:=0 to lp.x_Count-1 do
  if lp.Get_Item(i,@v,LPoly_Max) = 1 then
  with v.Pol[0] do xCursor(X,Y,2,8,lp.Colors[i])
end;

procedure TMovieCursor.BeginPaint;
begin
  Hide; fPainting:=true;
end;

procedure TMovieCursor.AfterPaint;
begin
  fPainting:=false; Show
end;

procedure TMovieCursor.Move(X,Y,Cr: Integer);
begin
  Hide; fPos.X:=X; fPos.Y:=Y;
  fCursor:=Cr; fInside:=true
end;

procedure TMovieCursor.Show;
begin
  if fActive then
  if fInside then
  if not fVisible then
  if not fPainting then
  Draw(movie_show);

  if Assigned(fChild) then
  fChild.Drag(Self,fPos.X,fPos.Y,true);
end;

procedure TMovieCursor.Hide;
begin
  if fVisible then Draw(movie_hide);

  if Assigned(fChild) then
  fChild.Drag(Self,0,0,false);
end;

procedure TMovieCursor.Leave;
begin
  Hide; fInside:=false
end;

procedure TMovieCursor.Reset;
begin
  fVisible:=false;
  if Assigned(fChild) then
  fChild.Visible:=false;
end;

procedure TMovieCursor.SetCursor(Cr: Integer);
begin
  Hide; fCursor:=Cr
end;

function TMovieCursor.IsCross: Boolean;
begin
  Result:=fCursor in [_cCursor,
                      _cTarget,_pTarget,
                      _vTarget,_zTarget]
end;

procedure TMovieCursor.Draw(Mode: Integer);

const
  c_cCursor: array[0..12] of byte =
  (7,7,
   0,7,14,7,255,
   7,0,7,14,255,255);

  c_cLocator: array[0..30] of byte =
  (7,7,
   0,5,0,0,5,0,255,
   9,0,14,0,14,5,255,
   14,9,14,14,9,14,255,
   5,14,0,14,0,9,255,255);

  c_cTarget: array[0..22] of byte =
  (14,14,
   0,14,11,14,255,
   14,0,14,11,255,
   17,14,28,14,255,
   14,17,14,28,255,255);

procedure OutCursor(Cr: PBytes);
var
  i,n, cx,cy: Integer; p, a,b: TPoint;
begin
  cx:=fPos.X-Cr[0];
  cy:=fPos.Y-Cr[1];

  i:=2; n:=0; Color:=clBlue;
  while true do begin
    p.x:=Cr[i]; Inc(i); p.y:=Cr[i];

    if p.x = 255 then begin
      n:=0; if p.y = 255 then Break
    end
    else begin
      Inc(i); a:=b; Inc(n);
      b.x:=cx+p.x; b.y:=cy+p.y;
      if n = 2 then begin
        Line(a.x,a.y,b.x,b.y); n:=1;
      end
    end
  end
end;

procedure xDraw(Flags: Integer);
begin
  fData.Flags:=Flags;

  case fCursor of
_cCursor:
    OutCursor(@c_cCursor);
_cLocator,
_xLocator:
    OutCursor(@c_cLocator);
_cTarget,
_pTarget,
_vTarget,
_xTarget:
    OutCursor(@c_cTarget);
  end
end;

procedure _Draw(Mode: Integer);
begin
  fCard.Canvas.SetClipRect(nil);
  Begin_Paint(fCard,Mode,0);

  if fStereo then begin
    xDraw(movie_odd); Inc(fPos.x,fxyz_dx);
    xDraw(movie_even); Dec(fPos.x,fxyz_dx)
  end else
  if Assigned(fRight) then begin
    xDraw(0); Inc(fPos.x,fxyz_dx);
    fData.DC:=fRight; xDraw(0);
    Dec(fPos.x,fxyz_dx)
  end
  else xDraw(0)
end;

begin
  if Assigned(fCard) then

  if Mode = movie_show then begin
    _Draw(movie_lift);
    _Draw(movie_draw)
  end
  else _Draw(movie_hide);
end;

end.