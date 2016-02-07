unit cardbox; interface

uses
  Classes,LCLType,
  Controls,StdCtrls,ExtCtrls,
  Math,otypes,xintf2;

type
  TCardBox = class(TInterfacedObject,ICardBox)

    constructor Create(APaint: TPaintBox;
                       hbar,vbar: TScrollBar;
                       ApaZoom: TPanel);

    function GetDC: HDC; stdcall;

    function GetWidth: int; stdcall;
    function GetHeight: int; stdcall;

    function GetVisible: bool; stdcall;

    function win_Ratio: double; stdcall;

    procedure xClear(fc: int); stdcall;

    procedure xRepeat(const cashBmp: PBitmap;
                      const cashDC: HDC); stdcall;

    procedure SetHorzBar(pos,range,line,page: int); stdcall;
    procedure SetVertBar(pos,range,line,page: int); stdcall;

    function GetHorz(out pos,line,page: int): int; stdcall;
    function GetVert(out pos,line,page: int): int; stdcall;

    procedure SetZoomStr(const Str: PChar); stdcall;

    function GetCursorPos(out p: TPoint): bool; stdcall;
    procedure SetCursorPos(const p: TPoint); stdcall;

    function ClientToScreen(const p: TPoint): TPoint; stdcall;

    procedure Invalidate; stdcall;

    function GetLocked: bool; stdcall;
    procedure SetLocked(v: bool); stdcall;

    function GetCapture: bool; stdcall;
    procedure SetCapture(v: bool); stdcall;

    function GetCursor: int; stdcall;
    procedure SetCursor(cr: Integer); stdcall;

    procedure ProcessMessages; stdcall;
    procedure HelpContext(Id: int); stdcall;

    procedure SetScreenCursor(cr: int); stdcall;
    procedure CursorToScreen; stdcall;

    function GetSau: bool; stdcall;

    function GetSoft: bool; stdcall;
    procedure SetSoft(v: bool); stdcall;

  private
    fdraw: ICardBoxDraw;

    fPaint: TPaintBox;
    fhorz,fvert: TScrollBar;
    fpaZoom: TPanel;

    fCursor: int;
    fCapture: bool;
    fLocked: bool;
    fSoft: bool;
    fSau: bool;

    procedure CardMouseDown(Sender: TObject; Button: TMouseButton;
                            Shift: TShiftState; X, Y: Integer);

    procedure CardMouseMove(Sender: TObject; Shift: TShiftState;
                            X,Y: Integer);

    procedure CardMouseUp(Sender: TObject; Button: TMouseButton;
                            Shift: TShiftState; X, Y: Integer);

    procedure CardPaint(Sender: TObject);

    procedure horzScroll(Sender: TObject; ScrollCode: TScrollCode;
                         var ScrollPos: Integer);

    procedure vertScroll(Sender: TObject; ScrollCode: TScrollCode;
                         var ScrollPos: Integer);

  public
    property draw: ICardBoxDraw write fdraw;
  end;

implementation

uses
  LCLIntf,Graphics,
  Forms,XPens,xddw;

constructor TCardBox.Create(APaint: TPaintBox;
                            hbar,vbar: TScrollBar;
                            ApaZoom: TPanel);
begin
  inherited Create;
  fPaint:=APaint;
  fhorz:=hbar; fvert:=vbar;
  fpaZoom:=ApaZoom;

  if Assigned(fPaint) then begin
    fPaint.OnPaint:=CardPaint;
    fPaint.OnMouseDown:=CardMouseDown;
    fPaint.OnMouseMove:=CardMouseMove;
    fPaint.OnMouseUp:=CardMouseUp;
  end;

  if Assigned(fhorz) then
  fhorz.OnScroll:=horzScroll;

  if Assigned(fvert) then
  fvert.OnScroll:=vertScroll;
end;

function TCardBox.GetDC: HDC;
begin
  Result:=0;
  if fPaint.Canvas.HandleAllocated
  or fPaint.Visible then
  Result:=fPaint.Canvas.Handle
end;

function TCardBox.GetWidth: int;
begin
  Result:=Max(1,fPaint.Width);
end;

function TCardBox.GetHeight: int;
begin
  Result:=Max(1,fPaint.Height);
end;

function TCardBox.GetVisible: bool;
var
  up: TControl;
begin
  Result:=fPaint.Visible;
  up:=fPaint.Parent;

  while Result and Assigned(up) do begin
    Result:=up.Visible;
    if up is TForm then Break;
    up:=up.Parent
  end
end;

function TCardBox.GetLocked: bool;
begin
  Result:=fLocked
end;

procedure TCardBox.SetLocked(v: bool);
begin
  fLocked:=v
end;

function TCardBox.GetCapture: bool;
begin
  Result:=fCapture
end;

procedure TCardBox.SetCapture(v: bool);
begin
  if not fSau then begin

    if v then
      SetCaptureControl(fPaint)
    else begin
      SetCaptureControl(nil);
      Screen.Cursor:=crDefault;
    end;

    fCapture:=v
  end
end;

function TCardBox.GetSau: bool;
begin
  Result:=fSau
end;

function TCardBox.GetSoft: bool;
begin
  Result:=fSoft
end;

procedure TCardBox.SetSoft(v: bool);
begin
  fSoft:=v
end;

function TCardBox.win_Ratio: double;
begin
  Result:=Max(Screen.Width/GetWidth,
              Screen.Height/GetHeight)
end;

procedure TCardBox.xClear(fc: int);
var
  dc: TCanvas;
begin
  dc:=fPaint.Canvas;
  xResetCanvas(dc,0,dib_color(fc))
end;

procedure TCardBox.xRepeat(const cashBmp: PBitmap;
                           const cashDC: HDC);
begin
  if Assigned(cashBmp) then
  else
  if cashDC <> 0 then
    BitBlt(fPaint.Canvas.Handle,
           0,0,GetWidth,GetHeight,
           cashDC,0,0,SRCCOPY)
end;

procedure TCardBox.ProcessMessages;
begin
  Application.ProcessMessages;
end;

procedure TCardBox.HelpContext(Id: int);
begin
  Application.HelpContext(id)
end;

procedure TCardBox.SetScreenCursor(cr: int);
begin
  Screen.Cursor:=cr;
end;

procedure TCardBox.CursorToScreen;
var
  cr: Integer; p: TPoint;
begin
  if GetCursorPos(p) then begin
    cr:=fCursor;
    if fSoft then cr:=crNone;
    if fSau then cr:=crArrow;

    Screen.Cursor:=cr;
    fPaint.Cursor:=cr;
    Screen.Cursor:=crDefault
  end
end;

function TCardBox.GetCursor: int;
begin
  Result:=fPaint.Cursor
end;

procedure TCardBox.SetCursor(cr: Integer);
begin
  fCursor:=cr;
  if fSoft then cr:=crNone;
  if fSau then cr:=crArrow;
  fPaint.Cursor:=cr;
end;

procedure TCardBox.SetHorzBar(pos,range,line,page: int);
begin
  if Assigned(fhorz) then
  if fhorz.Enabled then

  with fhorz do

  if range < 0 then
    Position:=pos
  else begin
    Position:=0; Min:=0; Max:=range;
    SmallChange:=line; LargeChange:=page;
    Position:=pos;
  end;
end;

procedure TCardBox.SetVertBar(pos,range,line,page: int);
begin
  if Assigned(fvert) then
  if fvert.Enabled then

  with fvert do

  if range < 0 then
    Position:=pos
  else begin
    Position:=0; Min:=0; Max:=range;
    SmallChange:=line; LargeChange:=page;
    Position:=pos;
  end;
end;

function TCardBox.GetHorz(out pos,line,page: int): int;
begin
  Result:=0; pos:=0; line:=0; page:=0;
  if Assigned(fhorz) then begin
    pos:=fhorz.Position;
    line:=fhorz.SmallChange;
    page:=fhorz.LargeChange;
    Result:=fhorz.Max;
  end;
end;

function TCardBox.GetVert(out pos,line,page: int): int;
begin
  Result:=0; pos:=0; line:=0; page:=0;
  if Assigned(fvert) then begin
    pos:=fvert.Position;
    line:=fvert.SmallChange;
    page:=fvert.LargeChange;
    Result:=fvert.Max;
  end;
end;

procedure TCardBox.SetZoomStr(const Str: PChar);
begin
  if fpaZoom <> nil then
  fpaZoom.Caption:=Str
end;

function TCardBox.GetCursorPos(out p: TPoint): bool;
var
  t1,t2: TPoint;
begin
  Result:=false;
  if LCLIntf.GetCursorPos(t1) then begin
    t2:=fPaint.ScreenToClient(t1);
    p:=t2; with t2 do
    if (X >= 0) and (X < GetWidth) then
    if (Y >= 0) and (Y < GetHeight) then
    Result:=true
  end;
end;

procedure TCardBox.SetCursorPos(const p: TPoint);
begin
  with ClientToScreen(p) do
  LCLIntf.SetCursorPos(X,Y);
end;

function TCardBox.ClientToScreen(const p: TPoint): TPoint;
begin
  Result:=fPaint.ClientToScreen(p);
end;

procedure TCardBox.Invalidate;
begin
  fPaint.Invalidate;
end;

procedure TCardBox.CardMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TCardBox.CardMouseMove(Sender: TObject; Shift: TShiftState;
                                 X,Y: Integer);
begin

end;

procedure TCardBox.CardMouseUp(Sender: TObject; Button: TMouseButton;
                               Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TCardBox.CardPaint(Sender: TObject);
begin
  if Assigned(fdraw) then
  fdraw.CardDraw
end;

procedure TCardBox.horzScroll(Sender: TObject;
                              ScrollCode: TScrollCode;
                              var ScrollPos: Integer);
begin
  if Assigned(fhorz) then
  if fhorz.Enabled then begin
    fhorz.Enabled:=ScrollCode = scTrack;
    if Assigned(fdraw) then
    ScrollPos:=fdraw.Horz_Scroll( TCardScrollCode(ScrollCode),ScrollPos );
    fhorz.Enabled:=true
  end
end;

procedure TCardBox.vertScroll(Sender: TObject;
                              ScrollCode: TScrollCode;
                              var ScrollPos: Integer);
begin
  if Assigned(fvert) then
  if fvert.Enabled then begin
    fvert.Enabled:=ScrollCode = scTrack;
    if Assigned(fdraw) then
    ScrollPos:=fdraw.Vert_Scroll( TCardScrollCode(ScrollCode),ScrollPos );
    fvert.Enabled:=true
  end
end;

end.

