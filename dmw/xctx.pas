unit xctx; interface {$H-}

uses
  Classes,SysUtils,
  otypes,xclasses,xmask,
  xdib,xmovie,xclip,xcurve,
  dmw_vgm,dmw_fnt,dmw_prj,
  idib;

const
  x_Draw  = 0;
  x_Hide  = 1;
  x_Undo  = 2;
  x_Mark  = 3;
  x_Group = 4;
  x_Show  = 5;
  x_Card  = 6;
  x_Push  = 7;
  x_Clear = 8;
  x_Lock  = 9;
  x_Free  = 10;

type
  tx_context = class
    constructor Create(Avgm: TVgmBank);
    destructor Destroy; override;

    function Get_Mark_Color: int;

    function pen_Active: Integer;
    function pen_Group: Integer;
    function pen_Width: Integer;

    function rgb_Ground_Color: int;
    function rgb_Active_Color: int;
    function rgb_Group_Color: int;
    function rgb_Mark_Color: int;

    procedure beginPaint(prj: TMapProject);
    procedure endPaint;

    procedure setMode(prj: TMapProject; mode: int);

    procedure x_Start(dc: XCanvas;
                      pc,fc: int;
                      x_vgm: bool);

    procedure x_Close;

    procedure DrawPoly(dc: XCanvas;
                       lp: PPoly; N,cl: int;
                       mm_k: double);

    procedure FillPoly(dc: XCanvas; lp: PPoly;
                       cp: PIntegers; N,pc,fc: int);

    procedure MarkPoly(dc: XCanvas;
                       lp: PLPoly; lp_N,dR: int;
                       Curve: Boolean);

  private
    fpoly: TPolyLPoints;
    fvgm: TVgmBank;

    feye_draw: TMovie;

    fx_Mode: int;
    fx_Thin: bool;

    fx_dc: XCanvas;
    fx_pc,fx_fc: int;

    fx_lock: bool;
    fx_vgm: bool;

    fTextFill: bool;

    fGround: int;
    fPencil: int;
    fGrid: int;

  public
    property x_lock: bool write fx_lock;
    property x_Mode: int read fx_Mode;

    property eye_draw: TMovie read feye_draw;
    property poly: TPolyLPoints read fpoly;
  end;

type
  XDrawFonts = class(TFonts)
    constructor Create(Actx: tx_context; Adraw: IDrawText);

    procedure Vgm_Start(DC: XCanvas); override;
    procedure Vgm_Poly(DC: XCanvas; lp: PLPoly; N, Cl,Tag: Integer); override;
    procedure Vgm_Close(DC: XCanvas); override;
    procedure Virt_Rect(Sender: TObject); override;
  private
    fctx: tx_context;
    fpoly: TPolyLPoints;
    fMask: TTextMask;
  public
    property Mask: TTextMask write fMask;
  end;

type
  tproj_lp = function(lp: PLLine): integer of object;

  TClipDraw = class(TClipPoly)

    constructor Create(Actx: tx_context;
                       Afont: TFonts);

    procedure plot_begin(ADC: XCanvas;
                         Aloc,Apc,Afc: int;
                         Ammk: double);

    procedure OutLine(dc: XCanvas; lp: PLLine; pw,pc: int);

    procedure Grid_line(DC: XCanvas; bp,lp: PLLine);

    function plot_Polyline(lp: PPoly; lp_N: Integer): Integer;
    function plot_Polygon(lp: PLLine): Integer;
    function plot_TPoints(lp: PLLine): Integer;

    function plot_Attr(lp: PLLine; Aleft: Double;
                       AText: PWideChar): Integer;

    function Curve_Polyline(lp: PLPoly; n: Integer): Integer;
    function Curve_Polygon(lp: PLLine): Integer;

    function beginCurve(DC: XCanvas;
                        loc,pc,fc: int;
                        mm_k: double;
                        lp_max: int): TDrawCurve;

  private
    fctx: tx_context;
    fpoly: TPolyLPoints;
    ffont: TFonts;
    fDC: XCanvas;

    fproj_lp: tproj_lp;

    fLoc,fpc,ffc: int;
    fCount: Integer;

    fmmk,fleft: double;

    fText: PWideChar;

    function plot_lp(plot: TLLineProc;
                     lp: PLPoly; lp_N: Integer): Integer;

    procedure lp_Polyline(lp: PLLine);
    procedure lp_Polygon(lp: PLLine);
    procedure lp_Attr(lp: PLLine);
    procedure lp_Grid(lp: PLLine);

    procedure Out_lp(lp: PLPoly; n: Integer);

  public
    property proj_lp: tproj_lp write fproj_lp;
  end;

implementation

uses
  Graphics,
  XPoly,xline1,
  xddw,xgdi,ivg,
  dmx_draw;

const
  x_TPoints_Max = 128000;
  x_Counters_Max = 4096;

constructor tx_context.Create(Avgm: TVgmBank);
begin
  inherited Create; fvgm:=Avgm;

  fpoly:=TPolyLPoints.Create;
  fpoly.Points_Max:=x_TPoints_Max;
  fpoly.Counters_Max:=x_Counters_Max;

  feye_draw:=TMovie.Create(nil,0);

  fx_Mode:=x_Draw;
  fx_lock:=true
end;

destructor tx_context.Destroy;
begin
  feye_draw.Free;
  fpoly.Free;
  inherited
end;

function tx_context.Get_Mark_Color: int;
begin
  Result:=0;

  case fx_Mode of
x_Hide,
x_Undo:
    Result:=fGround;
x_Mark:
    Result:=fPencil and 31;
x_Group:
    Result:=(fPencil shr 5) and 31
   end;
end;

function tx_context.pen_Active: int; begin Result:=fPencil and 31 end;
function tx_context.pen_Group: int; begin Result:=(fPencil shr 5) and 31 end;
function tx_context.pen_Width: int; begin Result:=succ((fPencil shr 10) and 3) end;

function tx_context.rgb_Ground_Color: int; begin Result:=dib_Color(fGround) end;
function tx_context.rgb_Active_Color: int; begin Result:=dib_Color(pen_Active) end;
function tx_context.rgb_Group_Color: int; begin Result:=dib_Color(pen_Group) end;
function tx_context.rgb_Mark_Color: int; begin Result:=dib_Color(fPencil and 31) end;

procedure tx_context.beginPaint(prj: TMapProject);
begin
  fpoly.x_Alloc;
  fx_mode:=x_DRAW;
  fTextFill:=prj.x_Disp.TextFill;
end;

procedure tx_context.endPaint;
begin
  if not fx_lock then
  fpoly.x_Free;
end;

procedure tx_context.setMode(prj: TMapProject; mode: int);
begin
  fx_mode:=mode;
  if mode > x_DRAW then begin
    fGround:=prj.dm_Ground;
    fPencil:=prj.dm_Pencil;
    fGrid:=prj.cl_Grid;
  end;
end;

procedure tx_context.x_Start(dc: XCanvas;
                             pc,fc: int;
                             x_vgm: bool);
begin
  fx_dc:=dc;
  fx_pc:=pc; fx_fc:=fc;
  fx_vgm:=x_vgm; fpoly.Clear
end;

procedure tx_context.x_Close;
begin
  with fpoly do
  if x_Count > 0 then

  if Assigned(x_Points) then
  with x_Points^ do

  if fx_vgm then
    fx_dc.PolyPolygon(@Pol,x_Counters,x_Count)
  else
  if fx_Mode = x_Draw then
    dmx_lp_fill(fx_dc,@Pol,x_Counters,x_Count,fx_pc,fx_fc,fvgm)
  else
    FillPoly(fx_dc,@Pol,x_Counters,x_Count,fx_pc,fx_fc);

  fpoly.Clear;
end;

procedure tx_context.DrawPoly(dc: XCanvas;
                              lp: PPoly; N,cl: int;
                              mm_k: double);

procedure OutPoly(DC: XCanvas;
                  lp: PPoly; N,cl: int;
                  mm_k: double);
var
  fc,st,w,typ: int; pal: PColors;
begin
  typ:=tlong(cl).b[2];

  if fx_Thin then begin
    if fx_Mode = x_Hide then
      DC.xPen(1,fGround)
    else
    if (cl and x_gdi <> 0)
    or (typ and $08 <> 0) then
      DC.rgb_Pen(1,RGB_16_24(cl))
    else
      DC.xPen(1,cl);

    DC.PolyLine(lp,N)
  end else

  if cl and x_gdi <> 0 then begin
    cl:=hgl_unpack_color(cl,st,w);
    DC.rgb_xPen(w,st,cl);
    DC.PolyLine(lp,N)
  end else

  if typ and $08 <> 0 then begin
    cl:=hgl_unpack_color(cl and $7FFFFF,st,w);
    DC.rgb_xPen(w,st,cl);
    DC.PolyLine(lp,N)
  end else

  if typ and $40 <> 0 then begin

    if fx_Mode = x_Hide then begin
      DC.xPen(1,fGround);
      DC.PolyLine(lp,N)
    end else
    if fVgm.vm_Active then
      fVgm.Poly_Line(DC,lp,N,tlong(cl).w[0],0,mm_k)
    else begin
      DC.xPen(1,0);
      DC.PolyLine(lp,N)
    end

  end else
  if fx_Mode = x_Hide then begin
    pal:=dib_Colors_ptr;
    fc:=pal[1]; pal[1]:=pal[fGround];
    dmx_lp_line(dc,lp,N,cl,1,mm_k,fVgm);
    pal[1]:=fc
  end
  else dmx_lp_line(dc,lp,N,cl,0,mm_k,fVgm)
end;

begin
  case fx_Mode of
x_Draw,
x_Hide:
    OutPoly(dc,lp,N,cl,mm_k);

x_Undo,
x_Mark,
x_Group:
    begin
      dc.xPen(pen_Width,Get_Mark_Color);

      if feye_draw.Enabled then
        feye_draw.Polyline(lp,N)
      else
        dc.PolyLine(lp,N)
    end
  end
end;

procedure tx_context.FillPoly(dc: XCanvas; lp: PPoly;
                              cp: PIntegers; N,pc,fc: int);
var
  bc: int;
begin
  case fx_Mode of
x_Draw:
    dmx_lp_fill(dc,lp,cp,N,pc,fc,fvgm);

x_Hide:
    if fc and x_gdi <> 0 then begin
      bc:=rgb_Ground_Color;
      pc:=hgl_hide_color(pc,bc);
      fc:=hgp_hide_color(pc,bc);
      dmx_lp_fill(dc,lp,cp,N,pc,fc,fvgm)
    end
    else begin
      fc:=dmx_lp_hide(fc,fGround);
      dmx_lp_fill(dc,lp,cp,N,0,fc,fvgm)
    end;

x_Undo,
x_Mark,
x_Group:
    begin
      dc.xPen(pen_Width,Get_Mark_Color);

      if feye_draw.Enabled then
        feye_draw.PolyPolyline(lp,cp,N)
      else
        dc.PolyPolyLine(lp,cp,N)
    end;

  end
end;

procedure tx_context.MarkPoly(dc: XCanvas;
                              lp: PLPoly; lp_N,dR: int;
                              Curve: Boolean);
var
  i,n,r,r1,x2,y2: int; p: TPoint;
begin
  if fx_Mode in [x_Hide,x_Undo] then
    dc.ResetCanvas(fGround,fGround)
  else
    dc.ResetCanvas(15,0);

  r:=dR; r1:=r div 2;
  if r1 < 1 then begin r1:=1; r:=2 end;

  if Assigned(lp) then begin
    n:=lp_N;
    if Curve then begin
      if LCurve_Lock(lp,n) then Dec(n,2)
    end else

    if LPoly_Lock(lp,n) then Dec(n);

    x2:=dc.Width+8; y2:=dc.Height+8;

    for i:=0 to n do begin p:=lp[i];

      if (p.x >= -8) and (p.x <= x2) and
         (p.y >= -8) and (p.y <= y2) then

      if not Curve or not Odd(i) then

      if dR > 0 then
        dc.Ellipse(p.x-r,p.y-r,p.x+r,p.y+r)
      else
        dc.Rectangle(p.x-r,p.y-r,p.x+r,p.y+r);

      if not Curve then r:=r1 else
      if i = 1 then dc.rgb_Brush(clLtGray)
    end
  end
end;

constructor XDrawFonts.Create(Actx: tx_context; Adraw: IDrawText);
begin
  inherited Create(1,Adraw);
  fctx:=Actx; fpoly:=fctx.fpoly;
  is_Skip:=true;
end;

procedure XDrawFonts.Vgm_Start(DC: XCanvas);
begin
  if xBlankTyp = 0 then
  fctx.x_Start(DC,0,0,true)
end;

procedure XDrawFonts.Vgm_Poly(DC: XCanvas; lp: PLPoly; N, Cl,Tag: Integer);
begin
  if xBlankTyp = 0 then

  if (Tag = 0) and
     not fctx.fTextFill then begin
    Vgm_Close(DC); DC.PolyLine(lp,N)
  end else
  if fpoly.add_LPoly(lp,N) <= 0 then
    DC.PolyLine(PPoly(lp),N)
end;

procedure XDrawFonts.Vgm_Close(DC: XCanvas);
begin
  if xBlankTyp = 0 then
  fctx.x_Close;
end;

procedure XDrawFonts.Virt_Rect(Sender: TObject);
var
  l: LOrient; fl: Boolean;
begin
  if xBlankTyp = 1 then begin
    if Assigned(xDC) then begin
      l:=v_Rect; Blank_rect(@l,1/8);
      fl:=xDC.Fill; xDC.Fill:=true;
      xDC.dmwPolygon(@l,nil,4,xBlankColor);
      xDC.Fill:=fl
    end
  end else
  if xBlankTyp = 255 then begin
    if Assigned(fMask) then begin
      l:=v_Rect; Blank_rect(@l,1/8);
      if not fMask.Polygon(@l,4) then
      fxBlankTest1:=1
    end
  end
end;

constructor TClipDraw.Create(Actx: tx_context;
                             Afont: TFonts);
begin
  inherited Create;
  fctx:=Actx; fpoly:=fctx.fpoly;
  ffont:=Afont
end;

procedure TClipDraw.plot_begin(ADC: XCanvas;
                               Aloc,Apc,Afc: int;
                               Ammk: double);
begin
  fDC:=ADC; fLoc:=Aloc; fpc:=Apc; ffc:=Afc;
  fmmk:=Ammk; fleft:=0; fCount:=0; fText:=nil;
end;

function TClipDraw.plot_lp(plot: TLLineProc;
                           lp: PLPoly; lp_N: Integer): Integer;
begin
  fCount:=0; On_lp_Dump:=plot;

  if Assigned(fDC) then
  Draw_LPoly(lp,lp_N,
             fDC.clip_lt,
             fDC.clip_rb,
             fLoc = 3);

  Result:=fCount; fCount:=0; On_lp_Dump:=nil
end;

procedure TClipDraw.OutLine(dc: XCanvas; lp: PLLine; pw,pc: int);
var
  lt,rb: TPoint;
begin
  with lp^ do if N > 0 then begin

    dc.xPen(pw,pc);
    plot_begin(dc,0,0,0,0);

    with lp^ do
    Max_Poly_Bound(@Pol,N+1, lt,rb);

    if (rb.x >= 0) and (lt.x <= dc.Width) then
    if (rb.y >= 0) and (lt.y <= dc.Height) then

    if dc.lp_Clip then

      if (lt.x < -64) or (rb.x > dc.Width+64)
      or (lt.y < -64) or (rb.y > dc.Height+64) then
        plot_Polyline(@lp.Pol,lp.N)
      else
        dc.PolyLine(@Pol,N)
    else
      dc.PolyLine(@Pol,N)
  end
end;

procedure TClipDraw.Grid_line(DC: XCanvas; bp,lp: PLLine);
var
  i: int;
begin
  fpoly.Clear;
  On_lp_Dump:=lp_Grid;
  Clip_Polyline(lp,bp,nil);
  On_lp_Dump:=nil;

  for i:=0 to fpoly.x_Count-1 do
  if fpoly.Get_Item(i,lp,LPoly_Max) > 0 then
  OutLine(dc,lp,1,fctx.fGrid);

  fpoly.Clear
end;

function TClipDraw.plot_Polyline(lp: PPoly; lp_N: Integer): Integer;
begin
  Result:=plot_lp(lp_Polyline,lp,lp_N)
end;

function TClipDraw.plot_Polygon(lp: PLLine): Integer;
begin
  Result:=plot_lp(lp_Polygon,@lp.Pol,lp.N);
end;

function TClipDraw.plot_TPoints(lp: PLLine): Integer;
begin
  fCount:=0; On_lp_Dump:=lp_Polygon;

  if Assigned(fDC) then
  Draw_LPoly(@lp.Pol,lp.N,
             fDC.clip_lt,
             fDC.clip_rb,
             false);

  Result:=fCount; fCount:=0; On_lp_Dump:=nil
end;

function TClipDraw.plot_Attr(lp: PLLine; Aleft: Double;
                             AText: PWideChar): Integer;
begin
  fleft:=Aleft; fText:=AText;
  Result:=plot_lp(lp_Attr,@lp.Pol,lp.N);
end;

function TClipDraw.Curve_Polyline(lp: PLPoly; n: Integer): Integer;
var
  p: TPoint;
begin
  if Assigned(fDC) then

  if n = 0 then begin p:=lp[0];
    if fDC.Clip_Figure(p,p) then begin
      fDC.SetPixel(p.x,p.y,fpc);
      Inc(fCount)
    end
  end else

  if n > 0 then

  if fDC.Clip_Polyline(lp,n) then
    Result:=plot_lp(lp_Polyline,lp,n)
  else begin
    out_lp(lp,n); Result:=n+1
  end
end;

function TClipDraw.Curve_Polygon(lp: PLLine): Integer;
begin
  if Assigned(fDC) then
  if fDC.Clip_Polyline(@lp.Pol,lp.N) then
    plot_lp(lp_Polygon,@lp.Pol,lp.N)
  else
    lp_Polygon(lp);

  Result:=fCount
end;

procedure TClipDraw.lp_Polyline(lp: PLLine);
begin
  Out_lp(@lp.Pol,lp.N)
end;

procedure TClipDraw.Out_lp(lp: PLPoly; n: Integer);
begin
  if Assigned(fDC) then
  if fLoc = 0 then fDC.PolyLine(lp,N) else
  if fLoc = 2 then fctx.DrawPoly(fDC,lp,N,fpc,fmmk)
  else fctx.FillPoly(fDC,lp,nil,N,fpc,ffc);
  Inc(fCount,N+1)
end;

procedure TClipDraw.lp_Attr(lp: PLLine);
var
  len,tw: Double;
begin
  if Assigned(fText) then begin

    with lp^ do
    if Pol[lp.N].X < Pol[0].X then Swap_Poly(lp);

    len:=PolyLength(@lp.Pol,lp.N);
    tw:=ffont.Attr_Width(fpc,fmmk,fText);

    if fleft+tw+fleft <= len then
    ffont.Out_Attr(fDC,lp,fpc,fmmk,fleft,fText);
  end
end;

procedure TClipDraw.lp_Grid(lp: PLLine);
begin
  if Assigned(fproj_lp) then fproj_lp(lp);
  fpoly.add_LPoly(@lp.Pol,lp.N)
end;

procedure TClipDraw.lp_Polygon(lp: PLLine);
begin
  Inc(fCount,fpoly.Add_LPoly(@lp.Pol,lp.N))
end;

function TClipDraw.beginCurve(DC: XCanvas;
                              loc,pc,fc: int;
                              mm_k: double;
                              lp_max: int): TDrawCurve;
begin
  plot_begin(DC,loc,pc,fc,mm_k);
  Result:=TDrawCurve.Create(curve_polyline,lp_max,loc = 3)
end;

end.

