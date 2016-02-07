unit dmx_draw; interface {$H-}

uses
  Classes,LCLType,Math,
  otypes,xlist,xdib,dmw_vgm;

const
  what_sign   = 13;
  what_fill   = 14;
  what_edge   = 15;
  what_tube   = 16;
  what_cmyk   = 17;
  what_stroke = 18;
  xpen_cmyk   = 19;

  x_gdi = $80000000;

type
  PSimplePen = ^TSimplePen;
  TSimplePen = record case Integer of
0:  (what,cl: byte; f1,f2,f3,f4,f5: SmallInt);
1:  (i1,i2,i3: Integer)
  end;

  PExtPen = ^TExtPen;
  TExtPen = array[0..3] of TSimplePen;

type
  TExt_Pens = class(TMemoryStream)

    constructor Create;
    destructor Destroy; override;

    procedure Refresh_vgm(tr: TPointList);

  private
    FCount: Integer;

    FPen: TExtPen;
    FPenIndex: Integer;

  public
    property Count: Integer read fCount;

    function Pen(I: Integer): PExtPen;

    function Update(I: Integer;
                    const Pen: TExtPen): Integer;

    procedure Reset;
    procedure LoadFrom(Path: PChar);
    procedure SaveAs(Path: PChar);
  end;

var
  Ext_Pens: TExt_Pens;

function Pens_Equal(const p1,p2: TExtPen): Boolean;

procedure pen_lp_draw(dc: XCanvas;
                      what,cl: Integer;
                      lp: PPoly; N: Integer;
                      f1,f2,f3,f4,f5: Double);

procedure ext_lp_draw(dc: XCanvas; loc: Integer;
                      lp: PPoly; cp: PIntegers; N: Integer;
                      const Pen: TExtPen; def,hide: Integer;
                      Vgm: TVgmBank; mm_k: Double);

function dmx_pack_pen(style,mm,pc: int): uint;
function dmx_pack_bru(style,pc,fc: int): uint;
                      
function dmx_pen_color(cl: Integer): Longint;

function dmx_lp_line(dc: XCanvas;
                     lp: PPoly; lp_N, cl,hide_cl: Integer;
                     mm_k: Double; Vgm: TVgmBank): Integer;

function dmx_line_Width(cl: Longint; mmk: Double): Integer;
function dmx_line_Thick(cl: Longint; mmk: Double): Double;

function dmx_line_scale(cl: Longint; mmk: Double;
                        h1,h2: Float): Double;

function dmx_lp_hide(cl,hide: Integer): Integer;

function dmx_lp_fill(dc: XCanvas;
                     lp: PPoly; cp: pIntegers; N: int;
                     pc,fc: int; Vgm: TVgmBank): int;

function dmx_Polygon(const pen: TExtPen): Boolean;

function dmx_cmyk(cl: Integer;
                  out cmyk: tcmyk): Boolean;

procedure dmx_Sign(DC: XCanvas;
                   const a,b: TPoint;
                   cl,sel: Integer);

implementation

uses
  SysUtils,
  convert,ofiles,
  xline,xpoly,xgdi,xddw,
  img_x,xmask;

const
  StdPens: array[0..7] of TExtPen =
  (((what:1; cl:0; f1:10; f2:0; f3:0; f4:0; f5:0),
    (i1:0;i2:0;i3:0),(i1:0;i2:0;i3:0),(i1:0;i2:0;i3:0)),

   ((what:1; cl:0; f1:10; f2:0; f3:640; f4:320; f5:0),
    (i1:0;i2:0;i3:0),(i1:0;i2:0;i3:0),(i1:0;i2:0;i3:0)),

   ((what:1; cl:0; f1:10; f2:0; f3:320; f4:240; f5:0),
    (i1:0;i2:0;i3:0),(i1:0;i2:0;i3:0),(i1:0;i2:0;i3:0)),

   ((what:1; cl:0; f1:10; f2:0; f3:160; f4:160; f5:0),
    (i1:0;i2:0;i3:0),(i1:0;i2:0;i3:0),(i1:0;i2:0;i3:0)),

   ((what:1; cl:0; f1:10; f2:0; f3:500; f4:300; f5:0),
    (what:1; cl:0; f1:10; f2:600; f3:100; f4:700; f5:0),
    (i1:0;i2:0;i3:0),(i1:0;i2:0;i3:0)),

   ((what:1; cl:0; f1:10; f2:0; f3:500; f4:500; f5:0),
    (what:1; cl:0; f1:10; f2:600; f3:100; f4:900; f5:0),
    (what:1; cl:0; f1:10; f2:800; f3:100; f4:900; f5:0),
    (i1:0;i2:0;i3:0)),

   ((what:1; cl:0; f1:10; f2:0; f3:500; f4:200; f5:0),
    (what:5; cl:0; f1:10; f2:600; f3:0; f4:700; f5:0),
    (i1:0;i2:0;i3:0),(i1:0;i2:0;i3:0)),

   ((what:1; cl:0; f1:10; f2:0; f3:500; f4:300; f5:0),
    (what:5; cl:0; f1:10; f2:600; f3:0; f4:800; f5:0),
    (what:5; cl:0; f1:10; f2:700; f3:0; f4:800; f5:0),
    (i1:0;i2:0;i3:0)));

   prn_scale: double = 1;

function Pens_Equal(const p1,p2: TExtPen): Boolean;
var
  i: Integer;
begin
  Result:=true;

  for i:=0 to 3 do
  if (p1[i].i1 <> p2[i].i1)
  or (p1[i].i2 <> p2[i].i2)
  or (p1[i].i3 <> p2[i].i3) then begin
    Result:=false; Break
  end
end;

constructor TExt_Pens.Create;
begin
  inherited; Reset
end;

destructor TExt_Pens.Destroy;
begin
  inherited
end;

procedure TExt_Pens.Refresh_vgm(tr: TPointList);
var
  i,j,k: Integer; r: TExtPen; s: TSimplePen;
  p: PPoint;
begin
  for i:=0 to fCount-1 do begin
    r:=Pen(i)^; k:=0;

    for j:=0 to 3 do begin
      s:=r[j];

      if s.what = what_sign then begin
        p:=tr.id_Itemof(s.f1);
        if Assigned(p) then begin
          r[j].f1:=p.Y; Inc(k)
        end
      end
    end;

    if k > 0 then Update(i,r)
  end
end;

function TExt_Pens.Pen(I: Integer): PExtPen;
begin
  if I <> fPenIndex then begin
    FPenIndex:=-1; FillChar(FPen,SizeOf(FPen),0);
    FPen[0].what:=1;

    if (I >= 0) and (I < fCount) then begin
      Seek(I*SizeOf(FPen),soFromBeginning);
      Read(FPen,SizeOf(FPen)); FPenIndex:=I
    end
  end;

  Result:=@FPen
end;

function TExt_Pens.Update(I: Integer;
                          const Pen: TExtPen): Integer;
begin
  if I < 0 then begin
    I:=FCount; Inc(FCount)
  end;

  I:=Min(I,FCount-1);

  if I >= 0 then begin
    Seek(I*SizeOf(FPen),soFromBeginning);
    Write(Pen,SizeOf(FPen));

    if I = FPenIndex then
    FPen:=Pen
  end;

  Result:=I
end;

procedure TExt_Pens.Reset;
begin
  fCount:=1; FPenIndex:=-1;
  FillChar(FPen,SizeOf(FPen),0);
  fPen[0].what:=1;

  Clear; Write(fPen,SizeOf(fPen));
end;

procedure TExt_Pens.LoadFrom(Path: PChar);
var
  h: int;
  stm: THandleStream;
  fn: TShortStr;
begin
  SaveAs(nil); Reset;

  if StrUpdateExt(fn,Path,'.ext') <> nil then
  begin
    h:=FileOpen(StrPas(fn),fmOpenRead);

    if h > 0 then begin
      stm:=THandleStream.Create(h);
      try
        LoadFromStream(stm);
        fCount:=Size div SizeOf(TExtPen);
        if fCount = 0 then Reset
      finally
        stm.Free
      end;

      FileClose(h)
    end
  end
end;

procedure TExt_Pens.SaveAs(Path: PChar);
var
  h: int;
  stm: THandleStream;
  fn: TShortStr;
begin
  if Path <> nil then
  if StrUpdateExt(fn,Path,'.EXT') <> nil then begin
    h:=xFileCreate(fn);

    if h > 0 then begin
      stm:=THandleStream.Create(h);
      try
        stm.CopyFrom(Self,0);
      finally
        stm.Free
      end;

      FileClose(h)
    end
  end
end;

type
  TDot = class(XCanvas)
    constructor Create(ADC: XCanvas; ASign: Integer;
                       AStart,ALine,ASkip: Double);

    destructor Destroy; override;

    procedure MoveTo(X,Y: Integer); override;
    procedure LineTo(X,Y: Integer); override;

  private
    DC: XCanvas; IsDown: Boolean;
    XPred,YPred,XPos,YPos,dR,Sign,Count: Integer;
    Start,Dist,Line,Skip: Double;

    procedure plot_line(X,Y: Integer; dX,dY: Double);
    procedure plot_Sign(X,Y: Integer; dX,dY: Double);
  end;

constructor TDot.Create(ADC: XCanvas; ASign: Integer;
                        AStart,ALine,ASkip: Double);
begin
  inherited Create; DC:=ADC;
  Sign:=ASign; Start:=AStart; 

  Line:=ALine; Skip:=ASkip;
  if ASign = 0 then Line:=Abs(ALine);

  if ASign > 0 then begin
    Line:=Line/2; dR:=Round(Line);
    if ASign = 6 then Line:=Line*2
    else if ASign in [7,8,9,10] then
    Line:=Line * Sqrt(2)
  end else
  if ASkip < 0 then Line:=Line/2;

  Dist:=AStart
end;

destructor TDot.Destroy;
var
  d: tgauss;
begin
  if Sign > 0 then
  if Skip < 0 then
  if Abs(Start) < 1 then
  if Count > 0 then begin
    d.X:=XPos-XPred; d.Y:=YPos-YPred;
    xNorm(d); plot_Sign(XPos,YPos,d.X,d.Y)
  end;

  inherited
end;

procedure TDot.MoveTo(X,Y: Integer);
begin
  XPred:=X; YPred:=Y;
  XPos:=X; YPos:=Y; Count:=0
end;

procedure TDot.LineTo(X,Y: Integer);
var
  px,py: Integer;
  d,q: tgauss; len: Double;
begin
  d.X:=X-XPos; d.Y:=Y-YPos;

  if Skip < 0 then begin

    if Sign = 0 then begin
      plot_line(XPos,YPos,d.X,d.Y);
      plot_line(X,Y,-d.X,-d.Y);
    end else
    if Abs(Start) >= 1 then begin
      xNorm(d); px:=XPos; py:=YPos;
      if Start < 0 then begin
        px:=X; py:=Y
      end;

      px:=px+Round(Start*d.X);
      py:=py+Round(Start*d.Y);

      plot_Sign(px,py,d.X,d.Y);
    end
    else begin

      if xNorm(d) then begin
        q.X:=XPos-XPred; q.Y:=YPos-YPred;
        if xNorm(q) then begin
          d.X:=d.X+q.X;
          d.Y:=d.Y+q.Y;
          xNorm(d)
        end
      end;

      plot_Sign(XPos,YPos,d.X,d.Y);
    end;

    XPred:=XPos; YPred:=YPos;
    Inc(Count)
  end
  else begin

    len:=Hypot(d.X,d.Y);

    if len > Small then begin

      d.X:=d.X/len; d.Y:=d.Y/len;
      px:=XPos; py:=YPos;

      while dist < len do begin

        if dist > 0 then begin
          px:=XPos+Round(dist*d.X);
          py:=YPos+Round(dist*d.Y)
        end;

        if Sign = 0 then begin
          if IsDown then DC.LineTo(px,py)
          else DC.MoveTo(px,py);

          IsDown:=not IsDown;
          if IsDown then dist:=dist+line
          else dist:=dist+skip
        end
        else begin
          plot_Sign(px,py,d.X,d.Y);
          dist:=dist+skip
        end
      end
    end;

    if Sign = 0 then begin
      if IsDown then DC.LineTo(X,Y);
    end else
    if dist = len then plot_Sign(X,Y,d.X,d.Y);

    dist:=dist-len;
    if dist < 0 then begin
      IsDown:=not IsDown;
      if IsDown then dist:=dist+line
      else dist:=dist+skip
    end
  end;

  XPos:=X; YPos:=Y;
end;

procedure TDot.plot_line(X,Y: Integer; dX,dY: Double);
var
  len: Double;
begin
  len:=Hypot(dX,dY);
  if len > Small then begin

    DC.MoveTo(X,Y);
    DC.LineTo(X+Round(dX/len*Line),
              Y+Round(dY/len*Line));
  end
end;

procedure TDot.plot_Sign(X,Y: Integer; dX,dY: Double);
var
  i: Integer;
begin
  if dR = 0 then
    DC.SetPixel(X,Y,-1)
  else

  case Sign of
1,
2:  DC.Rectangle(X-dR,Y-dR,X+dR,Y+dR);

3,
4:  DC.Ellipse(X-dR,Y-dR,X+dR,Y+dR);

5:  begin
      DC.MoveTo(X+Round(dY*line),Y-Round(dX*line));
      DC.LineTo(X-Round(dY*line),Y+Round(dX*line));
    end;

6:  begin DC.MoveTo(X,Y);
      DC.LineTo(X+Round(dY*line),Y-Round(dX*line));
    end;

7:  begin
      plot_line(X,Y,dX + dY,dY - dX);
      plot_line(X,Y,dX - dY,dY + dX)
    end;

8:  begin
      plot_line(X,Y,-dX + dY,-dY - dX);
      plot_line(X,Y,-dX - dY,-dY + dX);
    end;

9:  for i:=1 to 2 do begin
      plot_line(X,Y,dX + dY,dY - dX);
      plot_line(X,Y,dX - dY,dY + dX);
      X:=X - Round(dX * line);
      Y:=Y - Round(dY * line);
    end;

10: for i:=1 to 2 do begin
      plot_line(X,Y,-dX + dY,-dY - dX);
      plot_line(X,Y,-dX - dY,-dY + dX);
      X:=X + Round(dX * line);
      Y:=Y + Round(dY * line);
    end;

  end
end;

procedure lp_Up(dc: XCanvas;
                lp: PPoly; N: Integer;
                Up: double);
var
  p1,p2,p3: TPoint; lp_lock: bool;
  i: Integer; v: lvector;
begin
  Up:=-Up; if N > 0 then

  if Abs(Up) < 1 then dc.PolyLine(lp,N)
  else begin

    lp_lock:=false;

    lp_lock:=Points_Equal(lp[0],lp[N]);

    p1:=lp[0]; p2:=lp[1]; p3:=lp[N-1];

    if lp_lock then
      Get_Tube_Points(p3,p1,p2, Up,Up, v[0],v[1])
    else
      Stretch_Point(p1,p2, Up,Up, v[0],v[1]);

    dc.MoveTo(v[0].x,v[0].y);

    i:=1; while i < N do begin

      Inc(i); p3:=lp[i];
      if not Points_Equal(p1,p2) then
      if not Points_Equal(p2,p3) then begin
        Get_Tube_Points(p1,p2,p3, Up,Up, v[0],v[1]);
        p1:=p2; p2:=p3; dc.LineTo(v[0].x,v[0].y)
      end
    end;

    p3:=lp[1]; if lp_lock then
      Get_Tube_Points(p1,p2,p3, Up,Up, v[0],v[1])
    else
      Stretch_Point(p2,p1, Up,Up, v[1],v[0]);

    dc.LineTo(v[0].x,v[0].y)
  end
end;

procedure lp_Edge(dc: XCanvas;
                  lp: PPoly; N: Integer;
                  Up,R,Fi: double);

procedure draw_Edge(dc: XCanvas;
                    const p1,p2: Tpoint;
                    Up,R,Fi: double);
var
  a,b,c: TPoint;
begin
  a:=p1; b:=p2;

  if Abs(Up) >= 1 then
  Get_Two_P_Points(a,b,Up, a,b);
  c:=prj_lpoint(b,R,iAngle(a,b)+Fi);

  dc.MoveTo(b.x,b.y);
  dc.LineTo(c.x,c.y)
end;

begin
  if R > 0 then
  if N > 0 then begin
    draw_Edge(dc,lp[1],lp[0],Up,R,Fi);
    draw_Edge(dc,lp[N-1],lp[N],Up,R,Fi)
  end
end;

procedure lp_dot(dc: XCanvas;
                 lp: PPoly; lp_N,Sign: Integer;
                 Start,Line,Skip,Up: Double);
var
  dot: TDot; i: Integer;
begin
  if lp_N > 0 then begin
    dot:=TDot.Create(dc,Sign,Start,Line,Skip);
    try
      if Abs(Up) < 1 then begin
        dot.MoveTo(lp[0].x,lp[0].y);

        for i:=1 to lp_N do
        dot.LineTo(lp[i].x,lp[i].y);

      end
      else lp_Up(dot,lp,lp_N,Up);

    finally
      dot.Free
    end
  end
end;

procedure lp_line(dc: XCanvas;
                  lp: PPoly; lp_N: Integer;
                  cl: Integer; Width: Double;
                  Start,Line,Skip,Up: Double);
begin
  if (Skip < 0) or (Skip >= 1) then begin
    dc.extPen(Round(Width),cl);
    dc.BeginPath;
    lp_dot(dc,lp,lp_N,0,Start,Line,Skip,Up);
    dc.ClosePath;
  end
  else begin
    dc.xPen(Round(Width),cl);
    lp_Up(dc,lp,lp_N,Up)
  end
end;

procedure Fill_vgm(dc: XCanvas;
                   lp: PPoly; cp: PIntegers; N,Cl: Integer;
                   Vgm: TVgmBank; sign: Integer;
                   f2,f3,f4,f5,mm_k: Double);
var
  Mask: TBitMask; p: TPoint; line: PBytes;
  g: TGauss; lx: Integer; flag,up: Boolean;
  v: VLLine;
begin
  Mask:=dc.BitMask;

  if f2 > 1 then
  if f3 > 1 then

  if Assigned(Vgm) then
  if Vgm.vm_Back then

  if Assigned(Mask) then
  if Mask.ScanLine[0] <> nil then

  if Mask.PolyPolygon(lp,cp,N) then

  if Mask.Enabled then
  with Mask.MaskRect do begin

    dc.ClipPolyPolygon(lp,cp,N);

    g.y:=0; flag:=false;
    while g.y <= Bottom do begin

      p.y:=Round(g.y);
      if (p.y >= Top) and (p.y <= Bottom) then begin

        up:=false; line:=Mask.ScanLine[p.y]; g.x:=f5;
        if flag then g.x:=g.x+f4; flag:=not flag;
        if g.x >= f2 then g.x:=g.x-f2;

        up:=false; lx:=Left-1;

        while g.x <= Right do begin
          p.x:=Round(g.x);

          while lx < p.x do begin
            Inc(lx); if lx <= Right then
            if line[lx div 8] and B_Bit[lx and 7] <> 0 then
            up:=not up
          end;

          if up then begin v.N:=0; v.Pol[0]:=p;
            if not Vgm.Vgm_Sign(dc,@v,mm_k,0,cl,sign,4) then
            dc.SetPixel(p.x,p.y,1)
          end;

          g.x:=g.x+f2
        end;
      end;

      g.y:=g.y+f3
    end;

    dc.ClipPaintRect;
    Mask.Clear_Mask
  end
end;

procedure pen_lp_tube(dc: XCanvas;
                      lp: PPoly; N: Integer;
                      f1,f2,f3: Double);
var
  v: TLLine; tube,buf: PLLine;
  r: array[0..LPoly_Max] of double;
  lr: PDoubles; tube_max: Integer;
  k,len: Double; i,mode: Integer;
begin
  mode:=Round(f1);

  if LPoly_Lock(PLPoly(lp),N)
  or ((f2 < 1) and (f3 < 1)) then

    dc.PolyLine(lp,N)

  else
  if N = 1 then begin

    Stretch_Point(lp[0],lp[1], f2,f2, v.Pol[0],v.Pol[1]);
    Stretch_Point(lp[1],lp[0], f3,f3, v.Pol[2],v.Pol[3]);

    if mode = 1 then begin
      v.Pol[2]:=lp[1]; v.Pol[3]:=lp[0]
    end else
    if mode = 2 then begin
      v.Pol[0]:=lp[0]; v.Pol[1]:=lp[1]
    end;

    v.Pol[4]:=v.Pol[0]; v.N:=4;
    dc.outPolygon(@v.Pol,v.N)
  end else
  if N > 1 then begin
    tube_max:=N+N+2; buf:=nil;
    if tube_max <= LPoly_Max then begin
      tube:=@v; lr:=@r
    end
    else begin
      buf:=Alloc_LLine(tube_max+1+N+1);
      tube:=buf; if Assigned(tube) then
      lr:=@tube.Pol[tube_max]
    end;

    if Assigned(tube) then begin
      len:=PolyLength(lp,N); k:=0;
      if len > 1 then k:=(f3-f2)/len;

      len:=0; lr[0]:=f2;

      for i:=1 to N do begin
        len:=len + Long_Dist(lp[i-1],lp[i]);
        lr[i]:=f2 + len * k;
      end;

      if LPoly_Tube(lp,N,lr,2,mode,tube,tube_max) > 0 then
      dc.outPolygon(@tube.Pol,tube.N);
    end;

    xFreePtr(buf);
  end
end;

procedure pen_lp_draw(dc: XCanvas;
                      what,cl: Integer;
                      lp: PPoly; N: Integer;
                      f1,f2,f3,f4,f5: Double);
begin
  if N > 0 then begin

    case what of
  1:  lp_line(dc,lp,N,cl,f1,f2,f3,f4,f5);

  2:  begin
        lp_line(dc,lp,N,cl,f1,f2,f3,f4,f5/2);
        lp_line(dc,lp,N,cl,f1,f2,f3,f4,-f5/2)
      end;
  3,
  4,
  5,
  6:  if (f4 < 0) or (f4 >= 2) then begin
        dc.xPen(Round(f1),cl);

        if Odd(what) then dc.xBrush(0,cl)
        else dc.xBrush(-1,cl);

        lp_dot(dc,lp,N,what-2,f2,f3,f4,f5)
      end  else

      if Odd(what) then
        lp_line(dc,lp,N,cl,f3,0,0,0,f5)
      else begin
        lp_line(dc,lp,N,cl,1,0,0,0,f5+f3/2);
        lp_line(dc,lp,N,cl,1,0,0,0,f5-f3/2)
      end;

  7,
  8,
  9,
  10,
  11,
  12: begin
        if f4 < 0 then
          f4:=PolyLength(lp,N)*2
        else
        if f2 < 0 then begin
          f4:=PolyLength(lp,N); f2:=f4-0.5
        end;  

        if f4 < 2 then begin
          dc.xPen(Round(f3),cl);
          lp_Up(dc,lp,N,f5)
        end
        else begin
          dc.extPen(Round(f1),cl);
          dc.BeginPath;
          lp_dot(dc,lp,N,what-2,f2,f3,f4,f5);
          dc.ClosePath
        end
      end;

  what_edge:
      begin
        dc.xPen(Round(f2),cl);

        f1:=(30+f1*15)/180*Pi;
        f4:=f4/sin(f1);

        lp_Edge(dc,lp,N,f3/2,f4,f1);
        lp_Edge(dc,lp,N,-f3/2,f4,-f1);
      end;

  what_tube:
      begin
        dc.xPen(1,cl); dc.xBrush(0,cl);
        pen_lp_tube(dc,lp,N,f1,f2,f3)
      end;

    end
  end
end;

function lev(v: Integer): Double;
begin
  Result:=v;
  while Result > 1 do
  Result:=Result/100
end;

function cmyk_ps(const ps: TSimplePen): int;
var
  cmyk: tcmyk; r,g,b: Integer;
begin
  cmyk.c:=lev(ps.f1);
  cmyk.m:=lev(ps.f2);
  cmyk.y:=lev(ps.f3);
  cmyk.k:=lev(ps.f4);
  Result:=CMYK_RGB(cmyk, r,g,b);
end;

procedure ext_lp_draw(dc: XCanvas; loc: Integer;
                      lp: PPoly; cp: PIntegers; N: Integer;
                      const Pen: TExtPen; def,hide: Integer;
                      Vgm: TVgmBank; mm_k: Double);
var
  ip,ic, i,c, cl, ind: Integer;
  f1,f2,f3,f4,f5,k: Double; ps: TSimplePen;
  rc: Boolean;
begin
  k:=mm_k/100;

  for ip:=0 to 3 do begin ps:=Pen[ip];

    if ps.what > 0 then begin

      f1:=ps.f1; f1:=ps.f1*k; f2:=ps.f2*k;
      f3:=ps.f3*k; f4:=ps.f4*k; f5:=ps.f5*k;

      if ps.what = what_tube then f1:=ps.f1;

      ic:=ps.cl; if hide > 0 then
      ic:=hide else if ps.what <> 2 then
      if ic = 0 then ic:=def;

      if ps.what = what_sign then begin

        // f1, f2,f3,f4,f5 - sign, right, down, shift, origin

        if loc = 3 then
          Fill_vgm(dc,lp,cp,N,ic, Vgm, ps.f1,f2,f3,f4,f5,mm_k)
        else begin
          rc:=false; if Assigned(Vgm) then
          rc:=Vgm.Vgm_Line(dc, lp,N, ic,ps.f1,4,
                           mm_k, f5,f3,f4);

          if not rc then begin
            dc.xPen(1,cl); dc.PolyPolyline(lp,cp,N)
          end
        end

      end else
      if ps.what = what_fill then

        dc.dmwPolygon(lp,cp,N,ic)

      else
      if ps.what = what_cmyk then begin

        cl:=cmyk_ps(ps);

        if loc = 2 then begin
          dc.rgb_Pen(Round(f5),cl);
          dc.PolyPolyline(lp,cp,N)
        end
        else begin
          dc.rgb_ResetCanvas(cl,cl);

          if dc.Fill then
            dc.PolyPolygon(lp,cp,N)
          else
            dc.PolyPolyline(lp,cp,N)
        end

      end else

      if ps.what = what_stroke then begin

        if loc = 3 then begin
          dc.xPen(Round(f1),ic);
          dc.PolyStroke(lp,cp,N,f2,Round(f5))
        end

      end else

      if ps.what = xpen_cmyk then begin

        cl:=cmyk_ps(ps);
        dc.rgb_Pen(Round(f1),cl);

        dc.PolyPolyline(lp,cp,N)
      end else

      if cp <> nil then begin

        ind:=0; for i:=0 to N-1 do begin
          c:=cp[i];

          pen_lp_draw(dc,ps.what,ic, @lp[ind],c-1,
                      f1,f2,f3,f4,f5);

          Inc(ind,c)
        end

      end else

      pen_lp_draw(dc,ps.what,ic,lp,N,
                  f1,f2,f3,f4,f5)
    end
  end
end;

function dmx_pack_pen(style,mm,pc: int): uint;
begin
  Result:=$800000 or
          (mm shl 8) or
          (style shl 5) or pc
end;

function dmx_pack_bru(style,pc,fc: int): uint;
begin
  Result:=(style shl 10) or (pc shl 5) or fc;
end;

function dmx_pen_color(cl: Integer): Longint;
var
  ind,ip: Integer; pen: TExtPen;
  ps: TSimplePen;
begin
  Result:=0;

  if tlong(cl).b[2] and $20 = 0 then
    Result:=pen_Color(cl)
  else begin
    Result:=dib_Color(cl and 31);

    ind:=tlong(cl).w[0] shr 5;
    pen:=Ext_Pens.Pen(ind)^;

    for ip:=0 to 3 do begin
      ps:=pen[ip];
      if ps.what > 0 then

      if ps.what in [what_cmyk,xpen_cmyk] then begin
        Result:=cmyk_ps(ps); Break
      end else
      if ps.cl > 0 then begin
        Result:=dib_Color(ps.cl);
        Break
      end

    end
  end
end;

function dmx_lp_line(dc: XCanvas;
                     lp: PPoly; lp_N, cl,hide_cl: Integer;
                     mm_k: Double; Vgm: TVgmBank): Integer;

function RGB_CMYK(r,g,b: Integer): tcmyk;
var
  c,m,y,k: float;
begin
  c:=1-r/31; m:=1-g/31; y:=1-b/31;
  k:=c; if k > m then k:=m;
  if k > y then k:=y;

  c:=c-k; m:=m-k; y:=y-k;

  if c < 0 then c:=0;
  if c > 1 then c:=1;

  if m < 0 then m:=0;
  if m > 1 then m:=1;

  if y < 0 then y:=0;
  if y > 1 then y:=1;

  if k < 0 then k:=0;
  if k > 1 then k:=1;

  Result.c:=c;
  Result.m:=m;
  Result.y:=y;
  Result.k:=k;
end;

var
  pen: TExtPen; w: Double;
  width,rr,gg,bb,g1,b1,typ: int;
  cmyk: tcmyk;
begin
  Result:=-1;

  typ:=tlong(cl).b[2];
  if typ and $40 <> 0 then begin
    dc.xPen(1,0); dc.PolyLine(lp,lp_N)
  end else
  if typ and $20 <> 0 then begin
    Result:=tlong(cl).w[0] shr 5; pen:=Ext_Pens.Pen(Result)^;
    ext_lp_draw(dc,2,lp,nil,lp_N,pen,cl and 31,hide_cl,Vgm,mm_k)
  end
  else begin
    pen:=StdPens[ tlong(cl).b[0] shr 5 ];

    if typ and $10 = 0 then begin
      width:=tlong(cl).b[1] and 63;

      if tlong(cl).b[1] shr 6 = 1 then begin
        pen[3].what:=2; pen[3].f5:=pen[0].f1;
      end;
    end
    else begin
      width:=tlong(cl).b[1] and 31;

      g1:=tlong(cl).b[2] and $C;
      b1:=tlong(cl).b[2] and $3;

      rr:=tlong(cl).b[0] and 31;
      gg:=(tlong(cl).b[0] shr 5) or (g1 shl 1);
      bb:=(tlong(cl).b[1] shr 5) or (b1 shl 3);

      cmyk:=RGB_CMYK(rr,gg,bb);

      pen[0].what:=xpen_cmyk;
      pen[0].f2:=Round(cmyk.c*100);
      pen[0].f3:=Round(cmyk.m*100);
      pen[0].f4:=Round(cmyk.y*100);
      pen[0].f5:=Round(cmyk.k*100);
    end;

    if typ and $80 <> 0 then
      w:=Line_mm[width]
    else begin
      w:=(width+1) * prn_Scale;
      if mm_k > 0 then w:=w / mm_k
      else mm_k:=1
    end;

    mm_k:=mm_k * w * 100 / pen[0].f1;

    ext_lp_draw(dc,2,lp,nil,lp_N,pen,cl and 31,hide_cl,Vgm,mm_k)
  end
end;

function dmx_line_Width(cl: Integer; mmk: Double): Integer;
var
  pen: PExtPen; i: Integer;
begin
  Result:=1;

  if tlong(cl).b[2] and $20 <> 0 then begin

    mmk:=mmk/100;
    pen:=Ext_Pens.Pen(tlong(cl).w[0] shr 5);

    for i:=0 to 3 do with pen[i] do
    if what in [1..8] then
    Result:=Max(Result,Round(f1*mmk));

  end
  else begin
    Result:=tlong(cl).b[1] and 63;
    if tlong(cl).b[2] and $80 <> 0 then
      Result:=Max(Round(Line_mm[Result] * mmk),1)
    else Inc(Result);
  end
end;

function dmx_line_Thick(cl: Integer; mmk: Double): Double;
var
  pen: PExtPen; i: Integer;
begin
  Result:=0;

  if tlong(cl).b[2] and $20 <> 0 then begin

    mmk:=mmk/100;
    pen:=Ext_Pens.Pen(tlong(cl).w[0] shr 5);

    for i:=0 to 3 do with pen[i] do
    if what in [1..8] then
    Result:=Max(Result,f1*mmk);

  end else

  if tlong(cl).b[2] and $80 <> 0 then
  Result:=Line_mm[tlong(cl).b[1] and 63] * mmk
end;

function dmx_line_scale(cl: Longint; mmk: Double;
                        h1,h2: Float): Double;
var
  pen: PExtPen; i,ax: Integer; h: Float;
begin
  Result:=mmk; h:=0;

  if tlong(cl).b[2] and $20 <> 0 then begin

    pen:=Ext_Pens.Pen(tlong(cl).w[0] shr 5);

    ax:=0; for i:=0 to 3 do
    if pen[i].what in [1..8] then
    ax:=Max(ax,pen[i].f1);

    h:=ax;
  end else

  if tlong(cl).b[2] and $80 <> 0 then
    h:=Line_mm[tlong(cl).b[1] and 63] * 100;

  if h >= 1 then begin

    h:=h / 100 * mmk;

    if h < h1 then
      Result:=mmk / h * h1
    else
    if h1 < h2 then
    if h > h2 then
      Result:=mmk / h * h2

  end
end;

function dmx_lp_hide(cl,hide: Integer): Integer;
begin
  hide:=hide and 31;
  if tlong(cl).b[2] and $40 <> 0 then

    cl:=(cl and $FFFFFFE0) or (hide and 31)

  else
  if cl and $800000 <> 0 then
    cl:=(hide shl 5) or hide
  else
    cl:=cl and $03FC00 + (hide shl 5) + hide;

  Result:=cl
end;

function dmx_lp_fill(dc: XCanvas;
                     lp: PPoly; cp: PIntegers; N: int;
                     pc,fc: int; Vgm: TVgmBank): int;
var
  pen: TExtPen;
begin
  Result:=-1;

  if fc and x_gdi <> 0 then
    dc.ivgPolygon(lp,cp,N,pc,fc)
  else
  if tlong(fc).b[2] and $40 = 0 then

    dc.dmwPolygon(lp,cp,N,fc)

  else begin
    Result:=tlong(fc).w[0] shr 5;
    pen:=Ext_Pens.Pen(Result)^;

    ext_lp_draw(dc,3,lp,cp,N,pen,fc and 31,0,
                Vgm,dc.mm_k)
  end
end;

function dmx_Polygon(const pen: TExtPen): Boolean;
var
  i: Integer;
begin
  Result:=false;

  for i:=0 to 3 do with pen[i] do
  if what in [what_sign,what_fill,what_cmyk] then
  begin Result:=true; Break end
end;

function dmx_cmyk(cl: Integer;
                  out cmyk: tcmyk): Boolean;
var
  i,ip: Integer; pen: TExtPen;
begin
  Result:=false;
  cmyk:=_cmyk(1,0,0,0);

  if tlong(cl).b[2] and $40 <> 0 then begin
    ip:=tlong(cl).w[0] shr 5;
    pen:=Ext_Pens.Pen(ip)^;

    for i:=0 to 3 do with pen[i] do
    if what = what_cmyk then begin
      cmyk.c:=lev(f1);
      cmyk.m:=lev(f2);
      cmyk.y:=lev(f3);
      cmyk.k:=lev(f4);

      Result:=true; Break
    end
  end
end;

procedure dmx_Sign(DC: XCanvas;
                   const a,b: TPoint;
                   cl,sel: Integer);
var
  tag,pc,fc,dr: int;
  r,d, f,df: Double;
begin
  tag:=tlong(cl).b[2] and $1F;

  case tag of
0:  begin
      DC.dmw_Line(a,b,cl);

      d:=Long_Dist(a,b); r:=d/6;
      if r < 4 then r:=r*2;

      f:=iAngle(a,b); df:=Pi*3/4;

      DC.dmw_Line(b,prj_lpoint(b,r,f+df),cl);
      DC.dmw_Line(b,prj_lpoint(b,r,f-df),cl);
    end;

1:  begin
      pc:=dib_Color(cl shr 5);
      fc:=dib_Color(cl);
      dr:=Max(1,tlong(cl).w[0] shr 10);

      dc.rgb_ResetCanvas(fc,pc); with a do
      dc.Ellipse(X-dr,Y-dr,X+dr+1,Y+dr+1);
    end;

2:  begin
      dr:=(tlong(cl).b[2] shr 5) and 3;
      Inc(dr); fc:=RGB_16_24(cl);

      if sel <> 0 then fc:=dib_Color(sel);

      dc.rgb_ResetCanvas(fc,fc); with a do
      dc.Rectangle(X-dr,Y-dr,X+dr+1,Y+dr+1)
    end;

  end
end;

initialization
  Ext_Pens:=TExt_Pens.Create;

finalization
  Ext_Pens.Free

end.


