unit xcards; interface {$H-}

uses
  Classes, LCLType,
  LMessages, Controls, Graphics,
  Math, Windows, otypes, xclasses,
  xintf, xintf2, xdib, xdibx, xdibcv,
  xline, xmovie, xlist1;

const
  but_Hand = $FFFF;

  wm_Invalidate = wm_USER + 512;
  wm_Refresh = wm_USER + 513;
  wm_Centre = wm_USER + 514;

  image_power2 = False;

type
  TOnDibDraw = procedure(Sender: TDib; PaintR, ClipR: PRect) of object;

  TDrawEvent = procedure(Bmp: XCanvas) of object;

  TOnPreciseCursor = function(P: PGauss; dX, dY: int): bool of object;

  TPlastic = class(XBitMap)
    constructor Create;

    procedure Release;
    procedure Clear;

    procedure xAlloc(Dib: TDib; bits: integer);

    function xLoad(XPos, XRange, XMax: integer;
      YPos, YRange, YMax: integer; Asrc: Float;
      Adst: integer): boolean;
  private
    fOnDraw: TOnDibDraw;

    fActive: boolean;
    fEnabled: boolean;

    fPath: TShortStr;

    function GetImage: PChar;
    procedure SetImage(Path: PChar);

    procedure Owner_Draw(dib: TDIB; PaintR, ClipR: PRect);

  public
    property Active: boolean read fActive;

    property OnDraw: TOnDibDraw read fOnDraw write fOnDraw;

    property Image: PChar read GetImage write SetImage;
  end;

  TOnPoint = procedure(x, y: integer) of object;
  TOnRect = procedure(x1, y1, x2, y2: integer) of object;
  TOnCardPort = procedure(out a, b: TPoint) of object;
  TOnCardPlus = procedure(const a, b: TPoint) of object;

  TCustomCard = class(TNotifyObj)
    procedure Owner_Draw(dc: XCanvas); virtual;
    procedure CaptureMove(X, Y: integer); virtual;
    function StrPosition(X, Y: integer): string; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); virtual; abstract;
    procedure MouseDown(Button: TMouseButton; X, Y: integer); virtual; abstract;
    procedure MouseUp(Button: TMouseButton; X, Y: integer); virtual; abstract;
    procedure KeyDown(var Key: word; Shift: TShiftState); virtual;
    procedure MarkPencil(mov: boolean); virtual;
    procedure Card_Escape; virtual;

  protected
    procedure Draw_cd_frame(xdc: XCanvas); virtual;
  end;

  timCard = class(TCustomCard)
    Cash: TDib;
    Cash1: TDib;
    Cash2: TDib;
    Cash3: TDib;

    Child: TCustomCard;
    vk_Chars: set of char;

    XPos, xLine, xPage, XRange, xMaxValue: int;
    YPos, yLine, yPage, YRange, yMaxValue: int;
    Centre: TPoint;

    CardDrawTime: int;

    isHourGlass: longbool;
    IsHourGlass1: longbool;
    isInvalidate: longbool;
    isMoving: longbool;
    isTrackCard: longbool;
    Escape_Enabled: longbool;
    Zoom_Enabled: longbool;

    isGray: longbool;
    isScroll: longbool;
    isWheel: longbool;
    isZoom: longbool;
    isRepaint: longbool;
    IsAnimate: longbool;
    isZoomDraw: longbool;

    isPlus: int;
    Plus_p: TPoint;

    Capture: record
      R: LRect;
      Butt, Cursor: int;
      ab_Aspect, isTrace, isVector: longbool;
      isStart: longbool;
      pen_dot: TPoint
    end;

    Key_Skip: word;

    Plastic: TPlastic;

    Stereo_a: TPlastic;
    Stereo_b: TPlastic;

    Right_dx: integer;

    loc_lt, loc_rb, dos_dot: TPoint;

    Card_Wnd: HWnd;

    imWidth, imHeight: integer;
    imTrack, im_dll: longbool;

    z_src: Float;
    z_dst: integer;
    zoom: integer;

    z_dst_round: longbool;

    cd_Frame: record
      lt, rb: TPoint;
      Active: longbool
    end;

    constructor Create(ACard: ICardBox);
    destructor Destroy; override;

    function Cash1_Active: boolean;
    function Cash1_Enabled: boolean;

    function begin_cash1(out xdc: XCanvas): bool;
    procedure end_cash1(xdc: XCanvas);

    procedure Set_loc_dot(const p: TPoint); virtual;

    function msg_Escape: boolean;

    function Open_Image(x, y, w, h: integer; im: PIntegers;
      Path: PChar): boolean;

    procedure Close_image;

    function Set_Image(Path: PChar): boolean;

    procedure Set_Plastic(Path: PChar);

    procedure image_full_card(w, h: integer);

    procedure Get_win(iv: PIntegers; rv: PDoubles); virtual;
    procedure Set_win(iv: PIntegers; rv: PDoubles); virtual;

    function get_view(out c: TPoint): double;
    procedure set_view(cx, cy: integer; kz: double);

    function CardScale(sc: double): double;

    procedure Card_zoom(cls: boolean); virtual;
    procedure pcx_Card_zoom(cls: boolean); virtual;
    procedure Setup_src_dst; virtual;
    procedure Setup_units;

    function win_Ratio: double;

    function prj_Resolution: double;

    function xy_to_pix(x, y: double): TGauss;

    procedure Project(x, y: integer; out p: TPoint); virtual;
    procedure Backup(x, y: integer; out p: TPoint); virtual;

    procedure Backupf(x, y: double; out p: TPoint); virtual;

    procedure pcx_Project(x, y: integer; out p: TPoint);
    procedure pcx_Backup(x, y: double; out p: TPoint);
    procedure pcx_Backupt(x, y: double; out p: TPoint);

    function img_Project(x, y: double): TPoint;
    function img_Backup(x, y: double): TGauss;

    function card_to_img(x, y: integer): TPoint;
    function card_to_img1(x, y: double): TGauss;
    function img_to_card(x, y: double): TGauss;
    function bmp_to_card(x, y: integer): TPoint;

    function lp_Project(lp: PLLine): integer;

    function xyz_Project(x, y, z: integer): TPoint; virtual;
    function xyz_Backup(x, y: double): VPoint; virtual;

    procedure ProjectLink(const g: TGauss; out p: TPoint); virtual;
    procedure BackupCentre(x, y: integer; out c: TPoint); virtual;
    procedure Backup_plus(x, y: integer; out p: TPoint); virtual;

    procedure Card_wheel(x,y,plus: int);

    function Wheel_centre(x,y,plus: int; out c: TPoint): double; virtual;

    procedure Wheel_card(x,y,plus: int);

    function Card_Project(x,y,d: int; out p: TPoint): boolean;

    procedure BackupCapture(out a, b: TPoint);
    procedure BackupCapture_Dir(out d: TPoint);

    procedure BackupPort(x1, y1, x2, y2: integer; out lt, rb: TPoint);
    function BackupSize(x1, y1, x2, y2: integer; out s: TSize): integer;
    procedure BackupRect(w, h, d: int; out lt, rb: TPoint);

    procedure Backup_dc(dc: XCanvas; d: int; out lt, rb: TPoint);

    function Backup_Card_Bound(lp: PLPoly; dx, dy: integer): integer;
    procedure Backup_Centre_Card(out c: TPoint);
    procedure Backup_Card(out lt, rb: TPoint);

    procedure BackupCardCentre;

    function Card_Position(d: integer; out p: TPoint): boolean;

    procedure Card_Draw(R: PRect); virtual;

    procedure xdc_to_Card(xdc: XCanvas);
    procedure Cash_to_Card;

    procedure Cash_Release;
    procedure Cash_Clear;

    procedure Cash_Paint(dib: TDIB; PaintR, ClipR: PRect);

    procedure Card_Refresh(Sender: TObject); virtual;

    procedure end_Draw_Card(Sender: TObject); virtual;

    procedure Owner_Draw(dc: XCanvas); override;
    procedure After_Track; virtual;

    procedure Card_Update(cls: boolean);
    procedure Card_Clear(Sender: TObject);
    procedure Card_Redraw(Sender: TObject);
    procedure Card_Repaint(Sender: TObject);
    procedure Card_xRepaint(Sender: TObject);
    procedure Image_Repaint(Sender: TObject);

    procedure Card_MoveTo(Posx, Posy: integer; Code: TCardScrollCode);

    function Horz_Scroll(Code: TCardScrollCode; pos: integer): integer;
    function Vert_Scroll(Code: TCardScrollCode; pos: integer): integer;

    procedure Card_Scroll(var X, Y: integer);

    procedure Card_click(const P: TPoint);

    function PreciseLocator: bool;
    procedure SetLocator(x, y: double; d: int); virtual;
    function Locate_point(const P: TPoint): boolean;
    function locate_poly(lp: PLLine): integer;

    procedure FocusedPoint(const p: TPoint; but, cr: integer);
    procedure Focused_dos(const p: TPoint; but, cr: integer);
    procedure Focused_dos_dot(const p: TPoint; cr: integer);

    procedure dos_CardCapture(X, Y, but, cr: integer; ab: boolean);
    procedure dos_FocusedCard(const p: TPoint; but, cr: integer);
    procedure FocusedCard(const p: TPoint; but: integer);

    function Frame_Visible(const a, b: TPoint): boolean;

    procedure Get_Window(out a, b: TPoint);
    procedure Set_Window(const lt, rb: TPoint);

    function Point_Visible(const p: TPoint; dr: int): boolean;
    function Point_Cursor(const p: TPoint): boolean;

    function Card_Track(const p: TPoint): boolean;
    function Card_Track1(const p: TPoint): boolean;
    function Card_Track2(const p: TPoint): boolean;
    procedure Card_Review(const p: TPoint);

    function lp_Plus(lp: PLLine; r: integer): boolean;

    procedure Card_plus_zoom(const a, b: TPoint);

    procedure Card_Centre(const c: TPoint); virtual;
    procedure Card_Move(const a, b: TPoint); virtual;
    procedure Card_Plus(const a, b: TPoint); virtual;
    function Char_Minus(x, y: integer): boolean; virtual;
    function Char_Plus(x, y: integer): boolean; virtual;
    procedure Card_Escape; override;

    procedure Card_Minus(k: Float);

    function Locator_ContainsPoint(X, Y: integer): boolean;

    function Card_ContainsPoint(x, y, dx, dy: integer): boolean;
    function Card_ContainsRect(x1, y1, x2, y2: integer): boolean;
    function Card_Contains_lp(lp: PLLine; d: integer): boolean;
    function Card_Contains_Mouse(x, y: integer): boolean;

    function StrZoom: string;

    procedure CaptureMove(X, Y: integer); override;
    function StrPosition(X, Y: integer): string; override;

    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; X, Y: integer); override;
    procedure MouseMoveTo(var X, Y: integer); virtual;

    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure ctrl_KeyDown(var Key: word); virtual;

    procedure ChangeCapture(Value: boolean); virtual;

    procedure CardCapture(X, Y, But, Cr: integer; ab: boolean); virtual;
    procedure UpdateCapture(X, Y, But, Cr: integer); virtual;

    procedure begin_XOR_Mode(mov: bool);
    procedure end_XOR_Mode(mov: bool);

    procedure BeginCapture(Start: boolean);
    procedure ReleaseCapture; virtual;

    function CancelCapture: boolean;
    procedure ReturnCapture(capt: boolean);
    procedure xReturnCapture(cr: integer; capt: boolean);

    procedure Capture_Plus(AMode: integer);
    procedure Capture_Drag;

    procedure MovePencil(X, Y: integer);
    procedure MarkPencil(mov: boolean); override;
    procedure xMarkPencil(mov: boolean);
    procedure SwapPencil; virtual;

    procedure MarkCaptureRect;
    procedure MarkCaptureVector(dist: boolean);

    procedure Screen_Capture_Cursor;

    function MoveCursor(dX, dY: integer): integer;

    function Cursor_soft: boolean;
    procedure Cursor_change(Cr: integer);
    procedure Cursor_move(X, Y: integer);
    procedure Cursor_leave;
    procedure Cursor_show;
    procedure Cursor_hide;

    procedure Cursor_BeginPaint;
    procedure Cursor_AfterPaint;

    function Cursor_stereo(out dx: integer): boolean; virtual;

    procedure run_Point(const p: TPoint); virtual;
    procedure bmp_Info;

  protected
    function Get_Active: boolean; virtual;
    procedure Draw_cd_frame(xdc: XCanvas); override;

    procedure pen_Capture(X, Y: integer); virtual;
    procedure pen_MoveTo(X, Y: integer); virtual;
    procedure pen_Dn_Left(X, Y: integer); virtual;
    procedure pen_Dn_Right(X, Y: integer); virtual;
    procedure pen_Up_Left(X, Y: integer); virtual;
    procedure pen_Up_Right(X, Y: integer); virtual;
    procedure pen_mark(mov: boolean); virtual;

    procedure SetIsCapture(Value: longbool); virtual;

  private
    fCard: ICardBox;

    fLeft: timCard;
    fRight: timCard;

    fMovieCursor: TMovieCursor;

    fImages: TPcxList;

    fCaptureCV: XCanvas;
    fIsCapture: longbool;

    floc_dot: TPoint;

    fShiftState: TShiftState;

    fClickTick: longint;
    fIsDblClick: longbool;

    z_dx, z_dy, z_cx, z_cy: integer;

    fIsProcess: longbool;

    fImage: TShortStr;

    fOnBmpInfo: TNotifyEvent;
    fOnCashDraw: TOnDibDraw;

    fOnCardPort: TOnCardPort;
    fOnChangeDepth: TNotifyEvent;
    fOnBeginDraw: TDrawEvent;
    fOnEndDraw: TDrawEvent;
    fOnNewFrame: TLPolyFunc;
    fOnAfterDraw: TNotifyEvent;
    fOnDrawRight: TNotifyEvent;
    fOnDispRight1: TNotifyEvent;
    fOnDispRight2: TNotifyEvent;
    fOnRepaint: TNotifyEvent;
    fOnZoom: TNotifyEvent;

    fOnMoveCursor: TOnPoint;
    fOnHideCursor: TNotifyEvent;
    fOnPreciseCursor: TOnPreciseCursor;

    fOnCentre: TOnPoint;
    fOnGamma: TNotifyEvent;

    function GetCaption: string;
    function GetImage: PChar;

    procedure Set_Cursor(cr: integer);
    procedure Set_Right(im: timCard);
    function Get_IsStereo: boolean;

    procedure Set_Card_Locked(Value: boolean);
    procedure Set_IsProcess(Value: longbool);

  public
    property Active: boolean read Get_Active;

    property Card: ICardBox read fCard write fCard;

    property Card_Locked: boolean write Set_Card_Locked;

    property IsProcess: longbool read fIsProcess write Set_IsProcess;

    property MovieCursor: TMovieCursor read fMovieCursor;

    property CaptureCV: XCanvas read fCaptureCV;

    property IsCapture: longbool read fIsCapture write SetIsCapture;

    property loc_dot: TPoint read floc_dot write Set_loc_dot;

    property cd_Cursor: integer write Set_Cursor;

    property Left: timCard read fLeft write fLeft;
    property Right: timCard read fRight write Set_Right;

    property ShiftState: TShiftState read fShiftState write fShiftState;

    property IsDblClick: longbool read fIsDblClick;

    property IsStereo: boolean read Get_IsStereo;

    property Caption: string read GetCaption;

    property Image: PChar read GetImage;
    property Images: TPcxList read fImages;

    property OnBmpInfo: TNotifyEvent write fOnBmpInfo;

    property OnCashDraw: TOnDibDraw read fOnCashDraw write fOnCashDraw;

    property OnBeginDraw: TDrawEvent write fOnBeginDraw;
    property OnEndDraw: TDrawEvent write fOnEndDraw;

    property OnNewFrame: TLPolyFunc write fOnNewFrame;

    property OnAfterDraw: TNotifyEvent write fOnAfterDraw;

    property OnDrawRight: TNotifyEvent write fOnDrawRight;

    property OnDispRight1: TNotifyEvent read fOnDispRight1 write fOnDispRight1;

    property OnDispRight2: TNotifyEvent read fOnDispRight2 write fOnDispRight2;

    property OnRepaint: TNotifyEvent read fOnRepaint write fOnRepaint;

    property OnCardPort: TOnCardPort read fOnCardPort write fOnCardPort;

    property OnChangeDepth: TNotifyEvent write fOnChangeDepth;

    property OnMoveCursor: TOnPoint write fOnMoveCursor;
    property OnHideCursor: TNotifyEvent write fOnHideCursor;
    property OnPreciseCursor: TOnPreciseCursor write fOnPreciseCursor;

    property OnZoom: TNotifyEvent write fOnZoom;
    property OnCentre: TOnPoint write fOnCentre;

    property OnGamma: TNotifyEvent write fOnGamma;
  end;

  TCardBoxDraw = class(TInterfacedObject,ICardBoxDraw)
    constructor Create(ACard: timCard);

    procedure CardDraw; stdcall;

    function Horz_Scroll(Code: TCardScrollCode; pos: int): int; stdcall;
    function Vert_Scroll(Code: TCardScrollCode; pos: int): int; stdcall;

  private
    fCard: timCard;
  end;

function image_zoom(k: double; out src: Float): integer;

implementation

uses
  SysUtils,
  convert, ofiles, xkeys,
  img_dll, ximages, xpoly,
  xutils, xcursors, xpens,
  xddw, xdc, ipens;

function log2_zoom(k: double; out src: Float): integer;
var
  dst,z: int; d,t: double;
begin
  k:=Log2(k); src:=1; dst:=1;
  d:=Abs(Log2(1) - k);
  z:=1;

  if k > 1 then
    while z < 1024 do begin
      z:=z * 2;
      t:=Abs(Log2(z) - k);
      if t < d then begin
        src:=z; d:=t;
      end;
    end
  else
    while z < 10 do begin
      Inc(z);
      t:=Abs(Log2(1 / z) - k);
      if t < d then begin
        dst:=z; d:=t;
      end;
    end;

  Result:=dst
end;

function image_zoom(k: double; out src: Float): integer;
begin
  Result:=1;
  src:=1;

  if image_power2 then
    Result:=log2_zoom(k, src)
  else
  begin
    src:=Min(1024, k);
    Result:=1;
  end;
end;

constructor TPlastic.Create;
begin
  inherited Create;
  Transparent:=clYellow;
end;

procedure TPlastic.Release;
begin
  Clear_dc(True);
end;

procedure TPlastic.Clear;
begin
  Clear_dc(False);
end;

function TPlastic.GetImage: PChar;
begin
  Result:=fPath;
end;

procedure TPlastic.SetImage(Path: PChar);
begin
  Release;

  Depth:=0;
  fEnabled:=True;

  if Path = nil then
    StrCopy(FPath, '')
  else
    StrCopy(FPath, Path);
end;

procedure TPlastic.Owner_Draw(dib: TDIB; PaintR, ClipR: PRect);
begin
  xPaint(PaintR, 0, 0, 1, fPath);
  if Assigned(fOnDraw) then
  fOnDraw(Self, PaintR, ClipR);
end;

procedure TPlastic.xAlloc(Dib: TDib; bits: integer);
begin
  if StrLen(fPath) > 0 then
    Alloc(Dib.Width, Dib.Height, bits);
end;

function TPlastic.xLoad(XPos, XRange, XMax: integer;
  YPos, YRange, YMax: integer; Asrc: Float;
  Adst: integer): boolean;
begin
  fActive:=False;

  if StrLen(FPath) > 0 then begin
    Pix_src:=Asrc; Pix_dst:=Adst;
    xDraw(XPos, XRange, XMax, YPos, YRange, YMax, Owner_Draw);
    fActive:=True
  end;

  Result:=fActive
end;

procedure TCustomCard.Owner_Draw(dc: XCanvas);
begin
end;

procedure TCustomCard.CaptureMove(X, Y: integer);
begin
end;

function TCustomCard.StrPosition(X, Y: integer): string;
begin
  Result:='';
end;

procedure TCustomCard.KeyDown(var Key: word; Shift: TShiftState);
begin
end;

procedure TCustomCard.MarkPencil(mov: boolean);
begin
end;

procedure TCustomCard.Card_Escape;
begin
end;

procedure TCustomCard.Draw_cd_frame(xdc: XCanvas);
begin
end;

constructor timCard.Create(ACard: ICardBox);
begin
  inherited Create;
  fCard:=ACard;

  imWidth:=10000;
  imHeight:=10000;
  zoom:=-1;
  z_src:=1;
  z_dst:=1;

  Cash:=CreateDib;
  Cash1:=CreateDib;
  Cash2:=CreateDib;
  Cash3:=CreateDib;

  fCaptureCV:=Cash2;

  vk_Chars:=['k', 'm', char(187), char(189)];

  Plastic:=TPlastic.Create;

  Stereo_a:=TPlastic.Create;
  Stereo_b:=TPlastic.Create;

  fMovieCursor:=TMovieCursor.Create;

  fImages:=TPcxList.Create;

  if fCard <> nil then
    Card_zoom(True);

  IsHourGlass:=True;
  IsHourGlass1:=True;
  Escape_Enabled:=True;
  Zoom_Enabled:=True;

  z_dst_round:=True;
end;

destructor timCard.Destroy;
begin
  fImages.Free;
  fMovieCursor.Free;

  Stereo_b.Free;
  Stereo_a.Free;

  Plastic.Free;

  Cash3.Free;
  Cash2.Free;
  Cash1.Free;
  Cash.Free;

  fCard:=nil;

  inherited;
end;

function timCard.GetCaption: string;
var
  dir: TShortstr;
begin
  Result:=xStrNameExt(fImage);
  if This_ext(fImage, '.xml') then
  begin
    StrDirectory(dir, fImage);
    Result:='[' + xStrNameExt(dir) + ']';
  end;
end;

function timCard.GetImage: PChar;
begin
  Result:=fImage;
end;

function timCard.Get_Active: boolean;
begin
  Result:=True;
end;

function timCard.Cash1_Active: boolean;
begin
  Cash1.Realloc_dc(Card.Width, Card.Height, 24);
  Cash1.Ground:=Cash.Ground;
  Result:=Cash1.Active;
end;

function timCard.Cash1_Enabled: boolean;
begin
  Result:=False;
  if Cash1.Active then
  if Cash1.Width = Card.Width then
  if Cash1.Height = Card.Height then
  Result:=True;
end;

function timCard.begin_cash1(out xdc: XCanvas): bool;
begin
  xdc:=Cash1;
  Result:=Cash1_Enabled
end;

procedure timCard.end_cash1(xdc: XCanvas);
begin
  xdc_to_card(xdc)
end;

procedure timCard.Set_loc_dot(const p: TPoint);
begin
  floc_dot:=p;
end;

function timCard.msg_Escape: boolean;
begin
  Result:=False;
  if Escape_Enabled then
    if xEscape(Card_Wnd, Key_Skip) then
    begin
      Cls_KeyDown(Card_Wnd, vk_Space);
      Result:=True;
    end;
end;

function timCard.Open_Image(x,y,w,h: integer; im: PIntegers;
                            Path: PChar): boolean;
var
  iw,ih,dll: integer;
begin
  Result:=False;
  im_dll:=False;
  Set_Plastic(nil);
  Right:=nil;

  cd_Frame.Active:=False;
  fImages.Clear;

  z_dx:=0; z_dy:=0; z_cx:=0; z_cy:=0;

  if Path = nil then begin
    StrCopy(fImage, '');
    if Zoom_Enabled then
    zoom:=-1;
    imWidth:=1000;
    imHeight:=1000;
    pcx_Card_zoom(True);
  end else

  if StrLen(Path) > 0 then begin

    if im = nil then
      Result:=this_Image(Path, iw, ih, dll)
    else begin
      iw:=im[0];
      ih:=im[1];
      dll:=im[2];
      Result:=True;
    end;

    if Result then begin
      if Zoom_Enabled then
      zoom:=-1;

      z_dx:=x;
      z_cx:=x + iw div 2;
      z_dy:=y;
      z_cy:=y + ih div 2;

      imWidth:=Max(iw, w);
      imHeight:=Max(ih, h);
      im_dll:=dll > 0;

      StrCopy(fImage, Path);

      Cash.IsGray:=False;
      Cash.IsContrast:=True;

      pcx_Card_zoom(True);
    end;
  end;
end;

procedure timCard.Close_image;
begin
  im_dll:=False;
  Set_Plastic(nil);
  Right:=nil;

  cd_Frame.Active:=False;

  StrCopy(fImage, '');
  z_dx:=0;
  imWidth:=1000;
  z_dy:=0;
  imHeight:=1000;
  zoom:=-1;
  Card_zoom(True);
end;

function timCard.Set_Image(Path: PChar): boolean;
begin
  Result:=Open_image(0, 0, 0, 0, nil, Path);
end;

procedure timCard.Set_Plastic(Path: PChar);
var
  w, h, flags: integer;
begin
  Cash.Clear_dc(False);
  Plastic.Image:=nil;

  if Path <> nil then
    if StrLen(Image) > 0 then
      if im_dll_bmp(Path, w, h, flags) then
        if (w = imWidth) and (h = imHeight) then

          Plastic.Image:=Path;
end;

procedure timCard.Set_Right(im: timCard);
var
  x, y: integer;
begin
  Cash_Clear;
  Plastic.Image:=nil;

  Stereo_a.Image:=nil;
  Stereo_b.Image:=nil;

  if im <> nil then
  begin
    x:=Card.Width div 4;
    y:=Card.Height div 2;
    BackupCentre(x, y, centre);
    fRight:=im;
  end
  else
  if fRight <> nil then
    if StrLen(fRight.Image) > 0 then
    begin
      Stereo_a.Image:=Image;
      Stereo_b.Image:=fRight.Image;

      if Zoom_Enabled then
        if z_dst > 1 then
        begin
          z_src:=1;
          z_dst:=1;
          Card_Zoom(True);
        end;
    end;

  fRight:=im;
end;

function timCard.Get_IsStereo: boolean;
begin
  Result:=FileExist(Stereo_a.Image) and FileExist(Stereo_b.Image);
end;

procedure timCard.pcx_Card_zoom(cls: boolean);
begin
  Card_Zoom(cls);
end;

procedure timCard.image_full_card(w, h: integer);
var
  dst: integer;
  k, k1, k2, src: Float;
begin
  if (w = 0) or (h = 0) then
  begin
    w:=fCard.Width;
    h:=fCard.Height;
  end;

  if (w > 0) and (h > 0) then
  begin

    k:=Max(imWidth / w, imHeight / h);
    dst:=image_zoom(k, src);

    k1:=src / dst;
    k2:=z_src / z_dst;
    if Abs(k1 / k2 - 1) > 0.001 then
    begin
      zoom:=-1;
      Card_zoom(True);
      zoom:=1;
      z_src:=src;
      z_dst:=dst;
      Card_zoom(False);
    end;
  end;
end;

procedure timCard.Get_win(iv: PIntegers; rv: PDoubles);
begin
  iv[0]:=zoom;
  iv[1]:=centre.X;
  iv[2]:=centre.Y;

  rv[0]:=z_src / z_dst;
end;

procedure timCard.Set_win(iv: PIntegers; rv: PDoubles);
begin
  zoom:=iv[0];
  z_src:=1;
  z_dst:=1;

  if Strlen(fImage) > 0 then
  begin
    centre:=Point(iv[1], iv[2]);

    if Double_Correct(rv[0]) then
      if rv[0] <= 1000 then

        if rv[0] > 1 then
          z_src:=Round(rv[0])
        else
        if rv[0] > 0.001 then
          z_dst:=Round(1 / rv[0]);

    Card_zoom(True);
  end;
end;

function timCard.get_view(out c: TPoint): double;
begin
  with centre do
    c:=card_to_img(X, Y);

  Result:=z_src / z_dst;
end;

procedure timCard.set_view(cx, cy: integer; kz: double);
begin
  if kz > 0 then
  begin
    zoom:=1;
    z_src:=1;
    z_dst:=1;
    centre:=bmp_to_card(cx, cy);

    if kz > 1 then
      z_src:=Round(kz)
    else
    if kz > 0.001 then
      z_dst:=Round(1 / kz);
    z_dst:=Min(10, z_dst);

    Card_zoom(True);
  end;
end;

function timCard.CardScale(sc: double): double;
begin
  Result:=xDeviceScale(sc)
end;

procedure timCard.Card_zoom(cls: boolean);
var
  px,py,pg: int; s: TShortstr;
begin
  if Assigned(fLeft) then begin
    zoom:=fLeft.zoom;
    z_src:=fLeft.z_src;
    z_dst:=fLeft.z_dst;

    XPos:=fLeft.XPos + Round(Right_dx * z_src);
    YPos:=fLeft.YPos;

    XRange:=fLeft.XRange;
    YRange:=fLeft.YRange;
    xMaxValue:=fLeft.xMaxValue;
    yMaxValue:=fLeft.yMaxValue;
  end
  else begin

    if not Active then begin
      imWidth:=0;
      imHeight:=0;
    end;

    if zoom = -1 then begin
      centre.X:=z_cx;
      centre.Y:=z_cy;
      z_src:=1;
      z_dst:=1;
      zoom:=1;
    end;

    if z_dst_round then
      if z_dst > z_src then begin
        z_dst:=Round(z_dst / z_src);
        z_src:=1;
      end;

    Setup_src_dst;

    pg:=fCard.Width div z_dst;
    xLine:=pg div 2;
    xPage:=pg * 7 div 8;
    xMaxValue:=Round(imWidth / z_src + 0.5);
    XRange:=Max(0, xMaxValue - pg);

    pg:=fCard.Height div z_dst;
    yLine:=pg div 2;
    yPage:=pg * 7 div 8;
    yMaxValue:=Round(imHeight / z_src + 0.5);
    YRange:=Max(0, yMaxValue - pg);

    px:=Round(centre.X / z_src) - fCard.Width div 2 div z_dst;
    py:=Round(centre.Y / z_src) - fCard.Height div 2 div z_dst;

    XPos:=Min(Max(0, px), XRange);
    YPos:=Min(Max(0, py), YRange);

    Setup_units;

    if Assigned(fCard) then begin
      StrPCopy(s,StrZoom);
      fCard.SetZoomStr(s);
    end
  end;

  if cls then
    Cash_Clear;

  if Assigned(fOnZoom) then
    fOnZoom(nil);
end;

procedure timCard.Setup_src_dst;
begin
end;

procedure timCard.Setup_units;
begin
  if Assigned(fCard) then
  begin
    fCard.SetHorzBar(XPos, XRange, XLine, XPage);
    fCard.SetVertBar(YPos, YRange, YLine, YPage);
  end;
end;

function timCard.win_Ratio: double;
begin
  Result:=1;
  if Assigned(fCard) then
    Result:=fCard.win_Ratio;
end;

function timCard.prj_Resolution: double;
var
  p1, p2: TPoint;
  w, h: integer;
begin
  w:=100;
  h:=100;
  if Assigned(Card) then
  begin
    w:=Card.Width;
    h:=Card.Height;
  end;

  Backup(0, 0, p1);
  Backup(w, h, p2);
  Result:=Long_Dist(p1, p2) / Hypot(w, h);
end;

function timCard.xy_to_pix(x, y: double): TGauss;
begin
  Result.x:=((x + z_dx) / z_src - XPos) * z_dst;
  Result.y:=((y + z_dy) / z_src - YPos) * z_dst;
end;

procedure timCard.Project(x, y: integer; out p: TPoint);
var
  r: integer;
begin
  r:=z_dst div 2;
  p.X:=Round(x / z_src - XPos) * z_dst + r;
  p.Y:=Round(y / z_src - YPos) * z_dst + r;
end;

procedure timCard.Backup(x, y: integer; out p: TPoint);
begin
  p.X:=Round((XPos + x div z_dst) * z_src);
  p.Y:=Round((YPos + y div z_dst) * z_src);
end;

procedure timCard.Backupf(x, y: double; out p: TPoint);
begin
  p.X:=Round((XPos + x / z_dst) * z_src);
  p.Y:=Round((YPos + y / z_dst) * z_src);
end;

procedure timCard.pcx_Project(x, y: integer; out p: TPoint);
var
  r: integer;
begin
  Inc(x, z_dx);
  Inc(y, z_dy);
  r:=z_dst div 2;
  p.X:=Round(x / z_src - XPos) * z_dst + r;
  p.Y:=Round(y / z_src - YPos) * z_dst + r;
end;

procedure timCard.pcx_Backup(x, y: double; out p: TPoint);
begin
  p.X:=Round((XPos + x / z_dst) * z_src) - z_dx;
  p.Y:=Round((YPos + y / z_dst) * z_src) - z_dy;
end;

procedure timCard.pcx_Backupt(x, y: double; out p: TPoint);
begin
  p.X:=Trunc((XPos + x / z_dst) * z_src) - z_dx;
  p.Y:=Trunc((YPos + y / z_dst) * z_src) - z_dy;
end;

function timCard.img_Project(x, y: double): TPoint;
begin
  x:=x + z_dx;
  y:=y + z_dy;
  Result.X:=Round((x / z_src - XPos) * z_dst);
  Result.Y:=Round((y / z_src - YPos) * z_dst);
end;

function timCard.img_Backup(x, y: double): TGauss;
begin
  Result.x:=(XPos + x / z_dst) * z_src - z_dx;
  Result.y:=(YPos + y / z_dst) * z_src - z_dy;
end;

function timCard.card_to_img(x, y: integer): TPoint;
begin
  Result.X:=x - z_dx;
  Result.Y:=y - z_dy;
end;

function timCard.card_to_img1(x, y: double): TGauss;
begin
  Result.x:=x - z_dx;
  Result.y:=y - z_dy;
end;

function timCard.img_to_card(x, y: double): TGauss;
begin
  Result.x:=x + z_dx;
  Result.y:=y + z_dy;
end;

function timCard.bmp_to_card(x, y: integer): TPoint;
begin
  Result.X:=x + z_dx;
  Result.Y:=y + z_dy;
end;

function timCard.lp_Project(lp: PLLine): integer;
var
  i: integer;
begin
  Result:=lp.N;
  with lp^ do
    for i:=0 to Result do
      with Pol[i] do
        Project(x, y, Pol[i]);
end;

function timCard.xyz_Project(x, y, z: integer): TPoint;
begin
  Project(x, y, Result);
end;

function timCard.xyz_Backup(x, y: double): VPoint;
var
  p: TPoint;
begin
  Backupf(x, y, p);
  Result:=_VPoint(p.x, p.y, 0);
end;

procedure timCard.ProjectLink(const g: TGauss; out p: TPoint);
begin
  p:=img_Project(g.x, g.y);
end;

function timCard.Card_Project(x, y, d: integer; out p: TPoint): boolean;
begin
  Project(x, y, p);
  Result :=
    (p.x >= -d) and (p.x <= Card.Width + d) and (p.y >= -d) and (p.y <= Card.Height + d);
end;

procedure timCard.BackupCentre(x, y: integer; out c: TPoint);
begin
  Backup(x, y, c);
end;

procedure timCard.Backup_plus(x, y: integer; out p: TPoint);
begin
  Backup(x, y, p);
end;

procedure timCard.Card_wheel(x, y, plus: integer);
var
  p: TPoint;
  capt: boolean;
begin
  if not isWheel then
    if not isInvalidate then
    begin
      isWheel:=True;
      capt:=False;

      if isCapture then
      begin
        with Capture.R do
          Backup(Left, Top, p);
        capt:=CancelCapture;
      end;

      if plus > 0 then
        Char_Plus(x, y)
      else
        Char_Minus(x, y);

      if capt then
      begin
        Project(p.X, p.Y, p);
        Capture.R.lt:=p;
        ReturnCapture(capt);
      end;

      isWheel:=False;
    end;
end;

procedure timCard.Wheel_card(x, y, plus: integer);
var
  k: Float;
begin
  isWheel:=True;
  k:=Wheel_centre(x, y, plus, centre);

  z_dst:=image_zoom(k, z_src);
  Card_Update(True);
  isWheel:=False;
end;

function timCard.Wheel_centre(x, y, plus: integer; out c: TPoint): double;
var
  cx, cy, dst: integer;
  px, py, src, k: Float;
  p: TPoint;
begin
  BackupCentre(x, y, c);

  src:=z_src;
  dst:=z_dst;

  k:=1.2;
  if image_power2 then
    k:=2;

  if plus > 0 then
  begin
    if src > 1 then
      src:=Max(1, src / k)
    else
      dst:=Min(10, dst + 1);
  end
  else
  if plus < 0 then
  begin
    if dst > 1 then
      Dec(dst)
    else
      src:=Min(1024, src * k);
  end;

  if IsWheel then
  begin
    p:=c;

    cx:=Card.Width div 2;
    cy:=Card.Height div 2;
    px:=p.X / src - x div dst;
    py:=p.Y / src - y div dst;
    c.x:=Round((px + cx div dst) * src);
    c.y:=Round((py + cy div dst) * src);
  end;

  Result:=src / dst;
end;

procedure timCard.BackupCapture(out a, b: TPoint);
begin
  with Capture.R do
  begin
    Backup(Left, Top, a);
    Backup(Right, Bottom, b);
  end;
end;

procedure timCard.BackupCapture_Dir(out d: TPoint);
var
  a, b: TPoint;
begin
  BackupCapture(a, b);
  d.x:=b.x - a.x;
  d.y:=b.y - a.y;
end;

procedure timCard.BackupPort(x1, y1, x2, y2: integer; out lt, rb: TPoint);
var
  L: LOrient;
begin
  Backup(x1, y1, L[0]);
  Backup(x2, y1, L[1]);
  Backup(x2, y2, L[2]);
  Backup(x1, y2, L[3]);
  Max_Poly_Bound(@L, 4, lt, rb);
end;

function timCard.BackupSize(x1, y1, x2, y2: integer; out s: TSize): integer;
var
  lt, rb: TPoint;
begin
  BackupPort(x1, y1, x2, y2, lt, rb);
  s.cx:=rb.x - lt.x + 1;
  s.cy:=rb.y - lt.y + 1;
  Result:=Max(s.cx, s.cy);
end;

procedure timCard.BackupRect(w, h, d: int; out lt, rb: TPoint);
var
  x1, y1, x2, y2: integer;
begin
  x1:=-d;
  x2:=w + d - 1;
  y1:=-d;
  y2:=h + d - 1;
  BackupPort(x1, y1, x2, y2, lt, rb);
end;

procedure timCard.Backup_dc(dc: XCanvas; d: int; out lt, rb: TPoint);
begin
  BackupRect(dc.Width, dc.Height, d, lt, rb);
end;

procedure timCard.Backup_Centre_Card(out c: TPoint);
begin
  Backup(fCard.Width div 2, fCard.Height div 2, c);
end;

function timCard.Backup_Card_Bound(lp: PLPoly; dx, dy: integer): integer;
var
  x1, y1, x2, y2: integer;
begin
  x1:=-dx;
  x2:=fCard.Width + dx;
  y1:=-dy;
  y2:=fCard.Height + dy;
  Backup(x1, y1, lp[0]);
  Backup(x2, y1, lp[1]);
  Backup(x2, y2, lp[2]);
  Backup(x1, y2, lp[3]);
  lp[4]:=lp[0];
  Result:=4;
end;

procedure timCard.Backup_Card(out lt, rb: TPoint);
begin
  BackupRect(fCard.Width, fCard.Height, 0, lt, rb);
end;

procedure timCard.BackupCardCentre;
begin
  BackupCentre(fCard.Width div 2,
    fCard.Height div 2,
    centre);
end;

function timCard.Card_Position(d: integer; out p: TPoint): boolean;
var
  t: TPoint;
begin
  Result:=False; p.X:=0; p.Y:=0;

  if Card.GetCursorPos(t) then
  if Card_ContainsPoint(t.X,t.Y, -d, -d) then begin
    p:=t; Result:=True;
  end;
end;

procedure timCard.Card_Draw(R: PRect);
var
  xdc: TDib;
  cd: ICardBox;
  w, h, d: int;

begin
  if Assigned(Card) then
  if Card.Visible then
  if not IsInvalidate then

  if not Active then
    Card.xClear(7)
  else begin
    CardDrawTime:=GetTickCount;

    if Assigned(fLeft) then
      Card_Zoom(False);

    if isGray then
      Cash_Clear;

    Card.SetHorzBar(XPos, -1, 0, 0);
    Card.SetVertBar(YPos, -1, 0, 0);

    d:=1024;
    cd:=fCard;
    if Assigned(fLeft) then
      if not cd.Visible then
        cd:=fLeft.Card;

    w:=int_Round(cd.Width + 1, d);
    h:=int_Round(cd.Height + 1, d);

    Cash.Alloc(w, h, 24);

    Plastic.xAlloc(Cash, 1);
    Stereo_a.xAlloc(Cash, 24);
    Stereo_b.xAlloc(Cash, 24);

    if R = nil then
      fMovieCursor.Reset;

    Cursor_BeginPaint;

    if isTrackCard then
      Card.ScreenCursor:=_xDrag
    else
    if IsHourGlass1 then
      if not IsAnimate then
        Card.ScreenCursor:=crHourGlass;

    isInvalidate:=True;

    if Cash1_Active then begin
      xdc:=Cash1;
      Owner_Draw(xdc);

      if Assigned(fOnGamma) then
        fOnGamma(xdc);

      isInvalidate:=False;

      if Assigned(fOnBeginDraw) then
        fOnBeginDraw(xdc);

      Draw_cd_frame(xdc);

      if Assigned(child) then
        child.Draw_cd_frame(xdc);

      if Assigned(fOnEndDraw) then
        fOnEndDraw(xdc);

      xdc_to_Card(xdc)
    end;

    if Assigned(fRight) then
      fRight.Card_Draw(nil);

    if Assigned(fOnDrawRight) then
      fOnDrawRight(nil);

    Card.ScreenCursor:=crDefault;
    Cursor_AfterPaint;

    BackupCardCentre;

    CardDrawTime:=GetTickCount - CardDrawTime;

    end_Draw_Card(Self);
  end;
end;

procedure timCard.After_Track;
begin
  Card_Draw(nil);
end;

procedure timCard.Cash_Paint(dib: TDIB; PaintR, ClipR: PRect);
var
  p: TPoint;
  i,fc: integer;
  lp: PPcxArray;
begin
  if Stereo_a.Active then begin
    p:=Point(0, 0); if Assigned(Card) then
    p:=Card.ClientToScreen(p);

    Cash.xStereo(Stereo_a, Stereo_b, p.y);
  end
  else begin
    if Strlen(fImage) > 0 then  begin

      if Cash.IsGray then
      if xFileExist(fImage, '.gam') then
      Cash.IsGray:=False;

      fc:=dib_Color(dib.Ground);
      dib.rgb_Pen(1, fc xor clWhite);
      dib.rgb_Brush(fc);

      dib.xPaint(PaintR, z_dx,z_dy,1, fImage);

      lp:=fImages.First;
      for i:=1 to fImages.Count-1 do  with lp[i] do
      dib.xPaint(PaintR, x, y, 1 / scale, fn);

      dib.XGray;
    end;

    if Plastic.Active then
    if Plastic.fEnabled then
    Cash.xPlastic(Plastic);

    if Assigned(fOnCashDraw) then
    fOnCashDraw(dib, PaintR, ClipR);
  end;
end;

procedure timCard.Set_Card_Locked(Value: boolean);
begin
  Card.Locked:=Value;
  if not Value then
    Card_Refresh(nil);
end;

procedure timCard.Set_IsProcess(Value: longbool);
begin
  fIsProcess:=Value;

  if Value then
    fIsProcess:=Cash1.Active
  else
  if Cash1.Active then
    xdc_to_card(Cash1);
end;

procedure timCard.Card_Refresh(Sender: TObject);
var
  card2: IDrawCard2;
begin
  if cash.Active then
    if cash1_Active then

      if cash.xCopy(Cash1) then
      begin

        if Assigned(fOnGamma) then
          fOnGamma(Cash1);

        if Assigned(fOnBeginDraw) then
          fOnBeginDraw(Cash1);

        Draw_cd_frame(Cash1);

        if Assigned(Child) then
          Child.Draw_cd_frame(Cash1);

        if Assigned(fOnEndDraw) then
          fOnEndDraw(Cash1);

        xdc_to_Card(Cash1);
      end;
end;

procedure timCard.end_Draw_Card(Sender: TObject);
begin
end;

procedure timCard.Owner_Draw(dc: XCanvas);
var
  XRange,YRange: int; k1,k2: Float;
begin
  k1:=Cash.Scale;
  k2:=z_src / z_dst;
  if Abs(k1 / k2 - 1) > 0.001 then
  Cash_Clear;

  if Assigned(dc) then
  if Cash.Active then begin

    Cash.Pix_src:=z_src;
    Cash.Pix_dst:=z_dst;
    Cash.Ground:=15;

    XRange:=dc.Width div z_dst;
    YRange:=dc.Height div z_dst;

    if not isTrackCard then begin

      Plastic.xLoad(XPos, XRange, xMaxValue,
                    YPos, YRange, yMaxValue,
                    z_src, z_dst);

      Stereo_a.xLoad(XPos, XRange, xMaxValue,
                     YPos, YRange, yMaxValue,
                     z_src, z_dst);

      Stereo_b.xLoad(XPos, XRange, xMaxValue,
                     YPos, YRange, yMaxValue,
                     z_src, z_dst);

      Cash.xDraw(XPos, XRange, xMaxValue,
                 YPos, YRange, yMaxValue,
                 Cash_Paint);
    end;

    Cash.xMove(XPos, YPos);
    Cash.xCopy(dc);
  end;

  if Child <> nil then
  Child.Owner_Draw(dc);
end;

procedure timCard.xdc_to_Card(xdc: XCanvas);
var
  map: Bitmap; dc: HDC;
begin
  if Assigned(xdc) then
  if Assigned(fCard) then begin

    dc:=fCard.GetDC;
    if dc <> 0 then
      xdc.Canvas.CopyToDC(dc)
    else
    if xdc.GetMap(map) then
      fCard.xRepeat(@map,0)
    else begin
      dc:=xdc.GetDC;
      if dc <> 0 then
      fCard.xRepeat(nil,dc)
    end
  end
end;

procedure timCard.Cash_to_Card;
begin
  Cash.xCopy(Cash1);
  xdc_to_Card(Cash1)
end;

procedure timCard.Cash_Release;
begin
  Cash.Clear_dc(True);
end;

procedure timCard.Cash_Clear;
begin
  Cash.Clear_dc(False);

  Plastic.Clear;

  Stereo_a.Clear;
  Stereo_b.Clear;
end;

procedure timCard.Card_Update(cls: boolean);
var
  capt: boolean;
begin
  capt:=CancelCapture;
  while xEscape(Card_Wnd, 0) do ;

  Card_zoom(cls);

  if Card.Visible then
    if not IsMoving then
      Card_Draw(nil);

  ReturnCapture(capt);
end;

procedure timCard.Card_Clear(Sender: TObject);
begin
  Cash.Clear_dc(False);

  if Stereo_a.Active then
  begin
    Stereo_a.Clear_dc(False);
    Stereo_b.Clear_dc(False);
  end;

  if Assigned(fRight) then
    fRight.Cash.Clear_dc(False);
end;

procedure timCard.Card_Redraw(Sender: TObject);
var
  capt: boolean;
begin
  capt:=CancelCapture;

  if Card.Visible then

    if capt then
      Card_Draw(nil)
    else
      Card.Invalidate;

  ReturnCapture(capt);
end;

procedure timCard.Card_Repaint(Sender: TObject);
var
  capt: boolean;
begin
  capt:=CancelCapture;
  Card_Clear(Sender);

  if capt or isRepaint then
    Card_Draw(nil)
  else
    Card.Invalidate;

  ReturnCapture(capt);
end;

procedure timCard.Card_xRepaint(Sender: TObject);
begin
  isRepaint:=True;
  Card_Repaint(nil);

  if Assigned(fOnRepaint) then
    fOnRepaint(Self);

  isRepaint:=False;
end;

procedure timCard.Image_Repaint(Sender: TObject);
begin
  Cash_Release;
  Card_Repaint(nil);
end;

procedure timCard.Draw_cd_frame(xdc: XCanvas);
var
  a, b: TPoint;
  v: VLLine;
begin
  if Assigned(xdc) then begin

    if cd_Frame.Active then begin

      with cd_Frame.lt do Project(x,y, a);
      with cd_Frame.rb do Project(x,y, b);

      v.N:=Bound_to_LPoly(a, b, @v.Pol);
      iDisp_frame(xdc.Canvas, @v.Pol,v.N);
    end
  end
end;

procedure timCard.Card_MoveTo(Posx, Posy: integer; Code: TCardScrollCode);
var
  lt, rb: TPoint;
begin
  if isCapture then
    BackupCapture(lt, rb)
  else
    isTrackCard:=Code = _scTrack;

  isHourGlass:=Code <> _scTrack;
  XPos:=Posx;
  YPos:=Posy;

  if Code = _scTrack then
    Card_Draw(nil)
  else
  if Cash.Imaging then
    Card_Draw(nil)
  else
    Card.Invalidate;

  isTrackCard:=False;
  isHourGlass:=True;

  if isCapture then
  begin
    Project(lt.x, lt.y, Capture.R.lt);
    Project(rb.x, rb.y, Capture.R.rb);
  end;
end;

function timCard.Horz_Scroll(Code: TCardScrollCode; pos: longint): integer;
begin
  if not isInvalidate then begin

    if pos < 0 then
      pos:=0;
    if pos > XRange then
      pos:=XRange;

    if pos <> XPos then
      Card_MoveTo(pos, YPos, Code)
    else
    if Code = _scPosition then
      After_Track;
  end;

  Result:=XPos;
end;

function timCard.Vert_Scroll(Code: TCardScrollCode; pos: longint): integer;
begin
  if not isInvalidate then begin

    if pos < 0 then
      pos:=0;
    if pos > YRange then
      pos:=YRange;

    if pos <> YPos then
      Card_MoveTo(XPos, pos, Code)
    else
    if Code = _scPosition then
      After_Track;
  end;

  Result:=YPos;
end;

procedure timCard.Card_Scroll(var X, Y: integer);

  function Drag_scroll: boolean;
  begin
    Result:=False;
    if IsCapture then
      if (IsPlus = 2) or (Capture.butt = but_Hand) then
        Result:=True;
  end;

function horz(dir: int): bool;
var
  v1,v2,v3: int;
begin
  Result:=false;
  if Card.GetHorz(v1,v2,v3) > 0 then begin
    v1:=Horz_Scroll(_scTrack,v1+v2*dir);
    Card.SetHorzBar(v1,-1,0,0);
    Result:=True;
  end
end;

function vert(dir: int): bool;
var
  v1,v2,v3: int;
begin
  Result:=false;
  if Card.GetVert(v1,v2,v3) > 0 then begin
    v1:=Vert_Scroll(_scTrack,v1+v2*dir);
    Card.SetVertBar(v1,-1,0,0);
    Result:=True;
  end
end;

var
  p,t: TPoint; v: VPoint; mov: bool;
begin
  if Assigned(Card) then
  if not Drag_scroll then
  if Card.GetCursorPos(p) then begin

    isScroll:=True; mov:=False;
    v:=xyz_Backup(p.x, p.y);

    if x < 0 then
      mov:=horz(-1)
    else
    if x >= Card.Width then
      mov:=horz(+1);

    if y < 0 then
      mov:=vert(-1)
    else
      mov:=vert(+1);

    if mov then begin

      if Card.Sau then
        Project(loc_dot.x, loc_dot.y, t)
      else begin
        t:=xyz_Project(v.x, v.y, v.z);
        Card.SetCursorPos(t);
      end;

      X:=t.x;
      Y:=t.y;
    end;

    isScroll:=False;
  end;
end;

procedure timCard.Card_click(const P: TPoint);
begin
  if Assigned(Card) then Card.SetCursorPos(P);
  MouseDown(mbLeft, p.X, p.Y);
  MouseUp(mbLeft, p.X, p.Y);
end;

function timCard.PreciseLocator: bool;
var
  p: TPoint; t: TGauss;
begin
  Result:=false;

  if Assigned(Card) then
  if Assigned(fOnPreciseCursor) then
  if fOnPreciseCursor(@t, 0, 0) then
  if Card.GetCursorPos(p) then

  if Trunc(t.x) = p.X then
  if Trunc(t.y) = p.Y then begin
    SetLocator(t.x, t.y, 6);
    Result:=True;
  end;
end;

procedure timCard.SetLocator(x, y: double; d: int);
var
  lt, rb, p: TPoint; v: LOrient;
begin
  Backupf(X, Y, p);
  loc_dot:=p;

  Backupf(X - d, Y - d, v[0]);
  Backupf(X + d, Y - d, v[1]);
  Backupf(X + d, Y + d, v[2]);
  Backupf(X - d, Y + d, v[3]);

  Max_Poly_Bound(@v, 4, lt, rb);

  if lt.X = rb.X then
  begin
    Dec(lt.X);
    Inc(rb.X);
  end;

  if lt.Y = rb.Y then
  begin
    Dec(lt.Y);
    Inc(rb.Y);
  end;

  loc_lt:=lt;
  loc_rb:=rb;
end;

function timCard.Locate_point(const P: TPoint): boolean;
begin
  Result:=PortContainsPoint(loc_lt, loc_rb, P.X, P.Y);
end;

function timCard.locate_poly(lp: PLLine): integer;
var
  i: integer;
  lt, rb: TPoint;
begin
  Result:=-1;
  lt:=loc_lt;
  rb:=loc_rb;

  for i:=0 to lp.N do
    with lp.Pol[i] do
      if PortContainsPoint(lt, rb, X, Y) then
      begin
        Result:=i;
        Break;
      end;
end;

procedure timCard.FocusedPoint(const p: TPoint; but, cr: integer);
var
  t: TPoint;
begin
  Project(p.X, p.Y, t);
  CardCapture(t.X, t.Y, but, cr, False);
end;

procedure timCard.Focused_dos(const p: TPoint; but, cr: integer);
begin
  if not isCapture then
  begin
    if dos_interface then
      FocusedPoint(p, 0, cr)
    else
      FocusedPoint(p, but, cr);
    dos_dot:=p;
  end;
end;

procedure timCard.Focused_dos_dot(const p: TPoint; cr: integer);
begin
  if not isCapture then
  begin
    FocusedPoint(p, 0, cr);
    dos_dot:=p;
  end;
end;

procedure timCard.dos_CardCapture(X, Y, but, cr: integer; ab: boolean);
begin
  if not isCapture then
  begin
    if dos_interface then
      CardCapture(X, Y, 0, cr, ab)
    else
      CardCapture(X, Y, but, cr, ab);
  end;
end;

procedure timCard.dos_FocusedCard(const p: TPoint; but, cr: integer);
begin
  if dos_interface then
    FocusedPoint(p, but, cr);
end;

procedure timCard.FocusedCard(const p: TPoint; but: integer);
begin
  FocusedPoint(p, but, Card.Cursor);
end;

function timCard.Frame_Visible(const a, b: TPoint): boolean;
var
  L: LOrient; lt,rb: TPoint;
begin
  Result:=False;

  Project(a.x, a.y, L[0]);
  Project(b.x, a.y, L[1]);
  Project(b.x, b.y, L[2]);
  Project(a.x, b.y, L[3]);

  Max_Poly_Bound(@L, 4, lt, rb);

  if (rb.x - lt.x >= 32) or (rb.y - lt.y >= 32) then

    Result:=True;
end;

procedure timCard.Get_Window(out a, b: TPoint);
var
  w, h: int;
  l: LOrient;
begin
  if Assigned(fOnCardPort) then
    fOnCardPort(a, b)
  else
  begin
    w:=Card.Width - 1;
    h:=Card.Height - 1;

    Backup(0, 0, l[0]);
    Backup(w, 0, l[1]);
    Backup(w, h, l[2]);
    Backup(0, h, l[3]);

    Max_Poly_Bound(@l, 4, a, b);
  end;
end;

procedure timCard.Set_Window(const lt, rb: TPoint);
begin
  Cash.Clear_dc(False);
  Card_Plus(lt, rb);
end;

function timCard.Point_Visible(const p: TPoint; dr: int): boolean;
var
  t: TPoint;
begin
  Result:=False;
  Project(p.X, p.Y, t);
  with t do
    if (x >= -dr) and (x < Card.Width + dr) then
      if (y >= -dr) and (y < Card.Height + dr) then
        Result:=True;
end;

function timCard.Point_Cursor(const p: TPoint): boolean;
var
  t: TPoint;
begin
  Result:=False;
  Project(p.X, p.Y, t);
  if Card_ContainsPoint(t.X, t.Y, -8, -8) then begin
    Card.SetCursorPos(t); Result:=True;
  end;
end;

function timCard.Card_Track(const p: TPoint): boolean;
var
  t: TPoint;
begin
  Result:=True;
  Project(p.x, p.y, t);
  if not Card_ContainsPoint(t.X, t.Y, -16, -16) then
  begin
    Card_Centre(p);
    Result:=False;
  end;
end;

function timCard.Card_Track1(const p: TPoint): boolean;
var
  r: int;
  t: TPoint;
begin
  Result:=True;
  Project(p.x, p.y, t);

  r:=Min(Card.Width, Card.Height) div 8;
  if not Card_ContainsPoint(t.X, t.Y, -r, -r) then
  begin
    Card_Centre(p);
    Result:=False;
  end;
end;

function timCard.Card_Track2(const p: TPoint): boolean;
begin
  Result:=Card_Track1(p);
  Point_Cursor(p);
end;

procedure timCard.Card_Review(const p: TPoint);
var
  t: TPoint;
begin
  Project(p.x, p.y, t);
  with t do
    if not Card_ContainsPoint(X, Y, -8, -8) then
      if Card_ContainsPoint(X, Y, Card.Width shr 1, Card.Height shr 1) then
        Card_Scroll(X, Y)
      else
        Card_Centre(p);
end;

function timCard.lp_Plus(lp: PLLine; r: integer): boolean;
var
  i, w, h: integer;
  lt, rb, p: TPoint;
  L: LOrient;
begin
  Result:=False;

  with lp^ do
    if N > 0 then
    begin

      for i:=0 to N do
      begin
        with Pol[i] do
          Project(x, y, p);

        if i = 0 then
        begin
          lt:=p;
          rb:=p;
        end
        else

          Max_lPort(lt, rb, p);
      end;

      w:=rb.x - lt.x;
      h:=rb.y - lt.y;

      if r <= 0 then
        r:=32;

      if (lt.x < 8) or (w < r) or (lt.y < 8) or (h < r) then
      begin

        Dec(lt.x, w);
        Inc(rb.x, w);
        Dec(lt.y, h);
        Inc(rb.y, h);

        Bound_to_LPoly(lt, rb, @L);

        for i:=0 to 3 do
          with L[i] do
            Backup(x, y, L[i]);

        Max_Poly_Bound(@L, 4, lt, rb);
        Card_Plus(lt, rb);
        Result:=True;
      end;

    end;
end;

procedure timCard.Card_Centre(const c: TPoint);
begin
  centre:=c;
  Card_Update(False);
end;

procedure timCard.Card_Move(const a, b: TPoint);
begin
  if not Points_Equal(a, b) then
  begin
    Inc(centre.X, a.x - b.x);
    Inc(centre.Y, a.y - b.y);
    Card_Update(False);
  end;
end;

procedure timCard.Card_plus_zoom(const a, b: TPoint);
var
  k, d1, d2: double;
begin
  if Zoom_enabled then
    if a.x <> b.x then
      if a.y <> b.y then
      begin

        d1:=Hypot(Card.Width, Card.Height);
        d2:=Long_dist(a, b);

        k:=Max(imWidth / Card.Width, imHeight / Card.Height);

        k:=Min(k, d2 / d1);
        z_dst:=image_zoom(k, z_src);
        zoom:=1;
      end;
end;

procedure timCard.Card_Plus(const a, b: TPoint);
var
  k1, k2: Float;
  c: TPoint;
  changed: boolean;
begin
  c:=centre;
  zoom:=1;
  Middle_Point(a, b, centre);

  k1:=z_src / z_dst;
  if Zoom_Enabled then
    Card_plus_zoom(a, b);
  k2:=z_src / z_dst;
  Zoom_Enabled:=True;

  changed:=Abs(k1 / k2 - 1) > 0.001;

  if changed then
    imTrack:=False;

  if imTrack then
  begin
    imTrack:=False;
    Card_zoom(False);
    if not Points_Equal(c, centre) then
      Card_Draw(nil);
  end
  else
    Card_Update(changed);
end;

procedure timCard.Card_Minus(k: Float);
var
  w, h: integer; k1, k2, kz: double;
begin
  w:=fCard.Width; h:=fCard.Height;

  if (w > 0) and (h > 0) then begin

    k1:=z_src / z_dst;

    kz:=Max(imWidth / w, imHeight / h);
    if k > 0 then
      kz:=k1 * k;

    z_dst:=image_zoom(kz, z_src);

    k2:=z_src / z_dst;

    if Abs(k1 / k2 - 1) > 0.001 then
      Card_Update(True);
  end;
end;

function timCard.Char_Minus(x, y: integer): boolean;
begin
  Result:=False;
  if z_src < 1024 then
  begin
    BackupCentre(x, y, centre);

    if z_dst > 1 then
      Dec(z_dst)
    else
      z_src:=Min(1024, z_src / 0.8);

    Card_Update(True);
    Result:=True;
  end;
end;

function timCard.Char_Plus(x, y: integer): boolean;
begin
  Result:=False;
  if z_dst < 10 then
  begin
    BackupCentre(x, y, centre);

    if z_src > 1 then
      z_src:=Max(1, z_src * 0.8)
    else
      z_dst:=Min(10, z_dst + 1);

    Card_Update(True);
    Result:=True;
  end;
end;

procedure timCard.Card_Escape;
begin
end;

function timCard.Locator_ContainsPoint(X, Y: integer): boolean;
begin
  Result:=PortContainsPoint(loc_lt, loc_rb, x, y);
end;

function timCard.Card_ContainsPoint(x, y, dx, dy: integer): boolean;
var
  w,h: int;
begin
  Result:=false;
  if Assigned(Card) then begin
    w:=Card.Width; h:=Card.Height;
    if (x >= -dx) and (x <= w+dx) then
    if (y >= -dy) and (y <= h+dy) then
    Result:=true
  end
end;

function timCard.Card_ContainsRect(x1, y1, x2, y2: integer): boolean;
begin
  Result:=False;
  if (x1 <= Card.Width) and (x2 >= 0) then
    if (y1 <= Card.Height) and (y2 >= 0) then
      Result:=True;
end;

function timCard.Card_Contains_lp(lp: PLLine; d: integer): boolean;
var
  lt, rb: TPoint;
begin
  lt:=Point(d, d);
  rb:=Point(Card.Width - d, Card.Height - d);
  Result:=PortContainsPolyLine(lt, rb, lp) >= 0;
end;

function timCard.Card_Contains_Mouse(X, Y: integer): boolean;
begin
  Result:=False;
  if (X >= 0) and (X < Card.Width) then
    if (Y >= 0) and (Y < Card.Height) then
      Result:=True;
end;

function timCard.StrPosition(X, Y: integer): string;
var
  p: TPoint;
begin
  Result:='';
  if Child <> nil then
    Result:=Child.StrPosition(X, Y);

  if length(Result) = 0 then
  begin
    Backup(X, Y, p);
    loc_dot:=p;
    Result:=Format('%d : %d', [p.X, p.Y]);
  end;
end;

function timCard.StrZoom: string;
var
  src: integer;
  s: string;
begin
  s:=IntToStr(z_dst) + ':';

  src:=Round(z_src * 10);

  if src mod 10 = 0 then
    Result:=s + IntToStr(src div 10)
  else
    Result:=s + RealToStr(z_src, 1);
end;

procedure timCard.CaptureMove(X, Y: integer);
begin
end;

procedure timCard.MouseMove(Shift: TShiftState; X, Y: integer);
var
  p: TPoint;
begin
  if not isScroll then
  begin

    if isCapture then
    begin

      if Assigned(fOnHideCursor) then
        fOnHideCursor(Self);

      MovePencil(X, Y);

      if (isPlus = 2) or (Capture.Butt = but_Hand) then
      begin
        if Drag_Enabled then
          Capture_Drag;
      end
      else
      begin
        SetLocator(X, Y, 6);

        if Child <> nil then
          Child.CaptureMove(X, Y)
        else
          pen_Capture(X, Y);
      end;

    end
    else
    if isPlus > 0 then
    begin
      if isPlus = 1 then
        cd_Cursor:=_xZoom
      else
      if isPlus = 2 then
        cd_Cursor:=_xHand
      else
        cd_Cursor:=_cRegion;
    end
    else
    begin
      SetLocator(X, Y, 6);

      if Child <> nil then
        Child.MouseMove(Shift, X, Y)
      else
        pen_MoveTo(X, Y);
    end;

    Backup(X, Y, p);
    if Assigned(fOnMoveCursor) then
      fOnMoveCursor(p.X, p.Y);
  end;
end;

procedure timCard.MouseDown(Button: TMouseButton; X, Y: integer);
begin
  if not isScroll then

    if isPlus > 0 then
    begin
      Backup_plus(X, Y, plus_p);

      if Button = mbLeft then
      begin
        if isPlus = 2 then
          CardCapture(X, Y, 1, _xDrag, False)
        else
          dos_CardCapture(X, Y, 1, Card.Cursor, isPlus = 1);
      end
      else
      if Button = mbRight then
      begin
        if not isCapture then
          CardCapture(X, Y, 2, Card.Cursor, False);
      end;
    end
    else
    begin
      SetLocator(X, Y, 6);

      if Button = mbMiddle then
      begin
        if not isCapture then
        begin
          Backup_Plus(X, Y, plus_p);
          CardCapture(X, Y, but_Hand, _xDrag, False);
        end;
      end
      else
      if Child <> nil then
        Child.MouseDown(Button, X, Y)
      else
      if Button = mbLeft then
        pen_Dn_Left(X, Y)
      else
      if Button = mbRight then
        pen_Dn_Right(X, Y);

    end;
end;

procedure timCard.MouseUp(Button: TMouseButton; X, Y: integer);
var
  dt: longint;
begin
  dt:=GetTickCount - fClickTick;
  fIsDblClick:=(dt > 0) and (dt < 200);
  Inc(fClickTick, dt);

  if not isScroll then

    if isCapture then
    begin

      ReleaseCapture;

      if Button = mbLeft then
      begin

        if isPlus > 0 then
        begin
          if Capture.Butt = 1 then
            Capture_Plus(-1)
          else
          if dos_interface then
            CardCapture(X, Y, 1, Card.Cursor, isPlus = 1);
        end
        else
        begin
          SetLocator(X, Y, 6);

          if Child <> nil then
            Child.MouseUp(Button, X, Y)
          else
            pen_Up_Left(X, Y);

        end;
      end
      else
      if Button = mbRight then
      begin

        if isPlus > 0 then
        begin
          if Capture.Butt = 2 then
            isPlus:=0;
        end
        else
        begin
          SetLocator(X, Y, 6);

          if Child <> nil then
            Child.MouseUp(Button, X, Y)
          else
            pen_Up_Right(X, Y);
        end;
      end
      else

      if Button = mbMiddle then
        Capture_Plus(2);

    end;
end;

procedure TimCard.MouseMoveTo(var X, Y: integer);
begin
end;

procedure TimCard.pen_Capture(X, Y: integer);
begin
end;

procedure TimCard.pen_MoveTo(X, Y: integer);
begin
  cd_Cursor:=_cCursor;
end;

procedure TimCard.pen_Dn_Left(X, Y: integer);
begin
end;

procedure TimCard.pen_Dn_Right(X, Y: integer);
begin
end;

procedure TimCard.pen_Up_Left(X, Y: integer);
begin
end;

procedure TimCard.pen_Up_Right(X, Y: integer);
begin
  Char_Minus(X, Y);
end;

procedure TimCard.pen_mark(mov: boolean);
begin
  if Capture.butt = 1 then
    MarkCaptureRect;
end;

procedure TimCard.KeyDown(var Key: word; Shift: TShiftState);

  function Return_Centre: TPoint;
  var
    p: TPoint;
  begin
    Result.X:=Card.Width div 2;
    Result.Y:=Card.Height div 2;
    if Card_Position(-4, p) then
      Result:=p;
  end;

  function Cursor_to_Centre: boolean;
  var
    a, b, c, p: TPoint;
    capt: boolean;
  begin
    Result:=False;

    if Card_Position(-4, p) then
    begin

      capt:=CancelCapture;
      if capt then
        BackupCapture(a, b);

      if Assigned(fOnCentre) then
        fOnCentre(p.X, p.Y);

      BackupCentre(p.X, p.Y, c);
      centre:=c;
      IsZoom:=capt;
      Card_Update(False);

      if capt then
        with Capture.R do
        begin
          Project(a.X, a.Y, lt);
          Project(b.X, b.Y, rb);
          ReturnCapture(capt);
          MovePencil(p.X, p.Y);
        end;

      isZoom:=False;
      Result:=True;
    end;
  end;

  function Change_Depth(Shift: TShiftState; d: integer): integer;
  var
    capt: boolean;
  begin
    Result:=vk_Clear;

    if IsStereo then
    begin
      capt:=CancelCapture;
      fMovieCursor.Visible:=False;

      if ssShift in Shift then
        d:=d * 4;
      Stereo_b.Depth:=Stereo_b.Depth + d;
      Cash.Clear_dc(False);
      Card_Draw(nil);
      if Assigned(fOnChangeDepth) then
        fOnChangeDepth(Self);

      ReturnCapture(capt);
    end
    else
    if Assigned(Right) then begin

      capt:=CancelCapture;

      Inc(Right.Right_dx, d);
      Right.Card_Draw(nil);

      ReturnCapture(capt);
    end;
  end;

  procedure xHorz_Scroll(Code: TCardScrollCode);
  var
    p,d,t: int;
  begin
    if Assigned(Card) then
    if Card.GetHorz(p,d,t) > 0 then begin
      if Code = _scLineUp then d:=-d;
      Horz_Scroll(Code,p+d);
    end
  end;

  procedure xVert_Scroll(Code: TCardScrollCode);
  var
    p,d,t: int;
  begin
    if Assigned(Card) then
    if Card.GetVert(p,d,t) > 0 then begin
      if Code = _scLineUp then d:=-d;
      Vert_Scroll(Code,p+d);
    end
  end;

  procedure Large_Scroll(Posx, Posy: integer);
  var
    a, b: TPoint;
  begin
    if not isInvalidate then begin

      posx:=Min(Max(0, posx), XRange);
      posy:=Min(Max(0, posy), YRange);

      if (posx <> XPos) or (posy <> YPos) then begin

        if isCapture then
        BackupCapture(a, b);

        isHourGlass:=True;
        XPos:=posx;
        YPos:=posy;
        Card_Draw(nil);
        isHourGlass:=True;

        if isCapture then
        with Capture.R do begin
          Project(a.x, a.y, a);
          Left:=a.x; Top:=a.y;
          Project(b.x, b.y, b);
          Right:=b.x; Bottom:=b.y;
        end;
      end;
    end;
  end;

var
  p: TPoint;
  capt: boolean;
begin
  if ssCtrl in Shift then
    ctrl_KeyDown(Key)
  else
  if Key = vk_Space then
  begin
    if Cursor_to_Centre then
      Key:=vk_Clear;
  end
  else

  if Key = vk_Left then
    Key:=MoveCursor(-1, 0)
  else
  if Key = vk_Right then
    Key:=MoveCursor(1, 0)
  else
  if Key = vk_Up then
    Key:=MoveCursor(0, -1)
  else
  if Key = vk_Down then
    Key:=MoveCursor(0, 1)
  else

  if Key = vk_Home then
    Key:=Change_Depth(Shift, 1)
  else
  if Key = vk_End then
    Key:=Change_Depth(Shift, -1)
  else

  if Key in [97..105] then
  begin

    if Key = 97 then
      Large_Scroll(0, YRange)
    else
    if Key = 98 then
      xVert_Scroll(_scLineDown)
    else
    if Key = 99 then
      Large_Scroll(XRange, YRange)
    else
    if Key = 100 then
      xHorz_Scroll(_scLineUp)
    else
    if Key = 101 then
      Large_Scroll(XRange div 2, YRange div 2)
    else
    if Key = 102 then
      xHorz_Scroll(_scLineDown)
    else
    if Key = 103 then
      Large_Scroll(0, 0)
    else
    if Key = 104 then
      xVert_Scroll(_scLineUp)
    else
    if Key = 105 then
      Large_Scroll(XRange, 0);

    Key:=vk_Clear;

  end
  else

  if Key = vk_Escape then
  begin
    if isCapture then
    begin
      ReleaseCapture;
      Card_Escape;
      if Child <> nil then
        Child.Card_Escape;
      Key:=vk_Clear;
    end;
  end
  else
  if Key = 191 then
  begin // ?
    bmp_Info;
    Key:=vk_Clear;
  end
  else
  if only_Char(Key, Shift, 'H') then begin
    if Plastic.Active then begin
      Plastic.fEnabled:=not Plastic.fEnabled;
      Card_Repaint(nil);
      Key:=vk_Clear;
    end;
  end else

  if Key < 255 then
    if char(Key) in vk_Chars then begin

      capt:=False;
      isZoom:=True;

      if isCapture then
      begin
        with Capture.R do
          Backup(Left, Top, p);
        capt:=CancelCapture;
      end;

      if only_Char(Key, Shift, 'D') then begin
        Key_Skip:=Ord('D');
        Card_Repaint(nil);
      end else
      if Minus_Pressed(Key) then begin
        Key_Skip:=Key;
        with Return_Centre do
          Char_Minus(x, y);
      end else
      if Plus_Pressed(Key) then begin
        Key_Skip:=Key;
        with Return_Centre do
          Char_Plus(x, y);
      end;

      if capt then begin
        Project(p.X, p.Y, p);
        Capture.R.lt:=p;
        ReturnCapture(capt);
      end;

      isZoom:=False;
      Key:=vk_Clear;
    end;

  if Child <> nil then
    if Key <> 0 then
      if Key <> vk_Clear then
        Child.KeyDown(Key, Shift);
end;

procedure TimCard.ctrl_KeyDown(var Key: word);
begin
  KeyDown(Key, []);
end;

procedure TimCard.ChangeCapture(Value: boolean);
begin
end;

procedure TimCard.CardCapture(X, Y, But, Cr: integer; ab: boolean);
begin
  Cursor_hide;

  if cr = 0 then
    cr:=Card.Cursor;

  with Capture do
  begin
    R.Left:=X;
    R.Top:=Y;
    R.Right:=X;
    R.Bottom:=Y;
    Butt:=But;
    ab_Aspect:=ab;
    cursor:=Cr;
    isTrace:=False;
    Backup(X, Y, pen_dot);
  end;

  BeginCapture(True);
end;

procedure TimCard.UpdateCapture(X, Y, But, Cr: integer);
begin
  xMarkPencil(False);

  with Capture do begin
    R.Left:=X;
    R.Top:=Y;
    Butt:=but;
    Cursor:=cr;
  end;

  MarkPencil(True);
  Cursor_Move(X, Y);
end;

procedure TimCard.SetIsCapture(Value: longbool);
begin
  fIsCapture:=Value;

  if not Value then
    xdc_to_card(Cash1)
  else
  if Cash1.Active then begin
    Cash2.Realloc_rgb(Cash1.Width,Cash1.Height);
    fCaptureCV:=Cash2
  end;
end;

procedure TimCard.begin_XOR_Mode(mov: bool);
begin
  if mov then
  fCaptureCV.xRepeat(Cash1);
end;

procedure TimCard.end_XOR_Mode(mov: bool);
var
  map: Bitmap;
begin
  if mov then
  if fCaptureCV.GetMap(map) then
  Card.xRepeat(@map,0);
end;

procedure TimCard.BeginCapture(Start: boolean);
begin
  IsCapture:=true;
  Capture.isStart:=true;

  ChangeCapture(true);
  Capture.isStart:=False;

  Card.Capture:=true;

  MarkPencil(true);
  with Capture.R do
    if Start then
      Cursor_move(Left, Top)
    else
      Cursor_show;
end;

procedure TimCard.ReleaseCapture;
begin
  if isCapture then
  begin
    xMarkPencil(False);
    Card.Capture:=False;
    IsCapture:=False;
    ChangeCapture(False);
    Cursor_show;
  end;
end;

function TimCard.CancelCapture: boolean;
begin
  Result:=False;

  if isCapture then
  begin
    xMarkPencil(False);
    Card.Capture:=False;
    IsCapture:=False;
    ChangeCapture(False);
    Result:=True;
  end;
end;

procedure TimCard.ReturnCapture(capt: boolean);
begin
  if capt then begin
    Card.ProcessMessages;
    BeginCapture(False);
  end
  else begin
    Cursor_show;
    Card.CursorToScreen;
  end;
end;

procedure TimCard.xReturnCapture(cr: integer; capt: boolean);
begin
  if cr <> 0 then
  begin
    cd_Cursor:=cr;
    if capt then
      Capture.Cursor:=cr;
  end;

  ReturnCapture(capt);
end;

procedure TimCard.Capture_Plus(AMode: integer);
var
  a, b: TPoint;
  fr: LVector;
  n: integer;
begin
  if AMode < 0 then
    AMode:=IsPlus;

  BackupCapture(a, b);
  Swap_lRect(a, b, fr[0], fr[1]);

  case AMode of

    1:
    begin
      Card_plus(fr[0], fr[1]);
      isPlus:=0;
    end;

    2: if Drag_Enabled then
        After_Track
      else
        Card_move(a, b);

    3:
    begin
      n:=2;
      if Assigned(fOnNewFrame) then
        n:=fOnNewFrame(@fr, 2);

      if n = 2 then
      begin
        cd_Frame.lt:=a;
        cd_Frame.rb:=b;
        cd_Frame.Active:=True;
        Card_Refresh(nil);
      end;
    end;
  end;
end;

procedure TimCard.Capture_Drag;
var
  p: TPoint;
begin
  if not isInvalidate then
  begin

    with Capture.R do
      Backup_plus(Right, Bottom, p);

    if (Capture.butt = 1) or (Capture.butt = but_Hand) then
      if not Points_Equal(plus_p, p) then
      begin
        CancelCapture;

        Inc(centre.x, plus_p.x - p.x);
        Inc(centre.y, plus_p.y - p.y);

        isTrackCard:=True;
        Card_zoom(False);
        Card_Draw(nil);
        isTrackCard:=False;

        BeginCapture(False);
      end;
  end;
end;

procedure TimCard.MovePencil(X, Y: integer);
var
  w, h: integer;
  kx, ky: double;
  p: TPoint;
begin
  with Capture do
  begin
    isVector:=False;

    if isTrace then
    begin
      Backup(X, Y, p);
      if Points_Equal(p, pen_dot) then
        Exit;
      pen_dot:=p;
    end;

    xMarkPencil(False);
    SwapPencil;
    p.X:=X;
    p.Y:=Y;

    if ab_Aspect then
    begin
      w:=Card.Width;
      h:=Card.Height;

      if (w > 0) and (h > 0) then
      begin

        kx:=Abs(x - R.Left + 1) / w;
        ky:=Abs(y - R.Top + 1) / h;

        if kx > ky then
        begin
          h:=Round(kx * h) - 1;
          if p.Y > R.Top then
            p.Y:=R.Top + h
          else
            p.Y:=R.Top - h;
        end
        else
        begin
          w:=Round(ky * w) - 1;
          if p.X > R.Left then
            p.X:=R.Left + w
          else
            p.X:=R.Left - w;
        end;
      end;
    end
    else
      Card_Scroll(X, Y);

    R.rb:=p;
    MarkPencil(True);
    Cursor_move(X, Y);
  end;
end;

procedure TimCard.MarkPencil(mov: boolean);
begin
  begin_Xor_Mode(mov);

  if Capture.butt = but_Hand then
  begin
    if not Drag_Enabled then
      MarkCaptureVector(True);
  end
  else

  if isPlus > 0 then
  begin
    if Capture.butt = 1 then
    begin
      if isPlus in [1, 3] then
        MarkCaptureRect
      else
      if isPlus = 2 then
        if not Drag_Enabled then
          MarkCaptureVector(True);
    end;
  end
  else
  begin
    if Child <> nil then
      Child.MarkPencil(mov)
    else
      pen_Mark(mov);
  end;

  end_Xor_Mode(mov)
end;

procedure TimCard.xMarkPencil(mov: boolean);
begin
  if mov then begin
    MarkPencil(True);
    Cursor_show;
  end
  else begin
    fMovieCursor.Hide;
    fMovieCursor.Active:=False;
    MarkPencil(False);
  end;
end;

procedure TimCard.SwapPencil;
begin
end;

procedure TimCard.MarkCaptureRect;
var
  R: TRect;
begin
  with Capture.R do
  R:=xSwapRect(Rect(Left, Top, Right, Bottom));
  fCaptureCV.Canvas.DrawFocusRect(R);
end;

procedure TimCard.MarkCaptureVector(dist: boolean);
begin
  with Capture.R do
  iDrawLine(fCaptureCV.Canvas, Left, Top, Right, Bottom);
  Capture.isVector:=dist;
end;

procedure timCard.run_Point(const p: TPoint);
begin
end;

procedure timCard.bmp_Info;
var
  capt: boolean;
begin
  if Assigned(fOnBmpInfo) then begin
    capt:=CancelCapture;
    fOnBmpInfo(Self);
    ReturnCapture(capt);
  end;
end;

procedure timCard.Screen_Capture_Cursor;
begin
  if Assigned(Card) then
  if not Card.Sau then
  if not Card.Soft then
  if isCapture then
  Card.ScreenCursor:=Capture.Cursor;
end;

procedure timCard.Set_Cursor(Cr: integer);
begin
  if Assigned(Card) then

  if Card.Cursor <> cr then begin
    fMovieCursor.Cursor:=cr;
    Card.SetCursor(cr);
    Cursor_show;
    Card.CursorToScreen;
  end;
end;

function timCard.MoveCursor(dX, dY: integer): integer;
var
  p: TPoint; t: TGauss;
begin
  if Assigned(Card) then
  if Assigned(fOnPreciseCursor) then
  if Card.GetCursorPos(p) then begin

    Inc(p.X,dX); Inc(p.Y,dY);
    if Card_ContainsPoint(p.X,p.Y,0,0) then
    if fOnPreciseCursor(@t, dx, dy) then begin
      P.X:=Trunc(t.x);
      P.Y:=Trunc(t.y);
      Card.SetCursorPos(p)
    end
  end;

  Result:=vk_Clear;
end;

function timCard.Cursor_soft: boolean;
begin
  if Card.Sau then
    Result:=True
  else begin
    Result:=IsStereo;
    if not Result then
    if Assigned(Right) then
    Result:=fMovieCursor.isCross;
  end;
end;

procedure timCard.Cursor_change(Cr: integer);
begin
  Set_Cursor(Cr);
  if IsCapture then
    Capture.Cursor:=Cr;
end;

procedure timCard.Cursor_move(X, Y: integer);
var
  cr: integer;
begin
  cr:=Card.Cursor;
  if isCapture then
    cr:=Capture.Cursor;

  fMovieCursor.Move(X, Y, Cr);
  Cursor_show;
end;

procedure timCard.Cursor_show;
var
  dx: integer; xdc: XCanvas;
begin
  fMovieCursor.Active:=Cursor_soft;
  Card.Soft:=fMovieCursor.Active;

  xdc:=nil;
  if Assigned(Right) then
  xdc:=Right.Cash2;

  fMovieCursor.Right:=xdc;

  fMovieCursor.Stereo:=Cursor_Stereo(dx);
  fMovieCursor.xyz_dx:=dx;

  fMovieCursor.Show;
  Screen_Capture_Cursor;
end;

procedure timCard.Cursor_leave;
begin
  fMovieCursor.Leave;
end;

procedure timCard.Cursor_hide;
begin
  fMovieCursor.Hide;
end;

procedure timCard.Cursor_BeginPaint;
begin
  fMovieCursor.BeginPaint;
end;

procedure timCard.Cursor_AfterPaint;
var
  dx: integer;
begin
  fMovieCursor.Stereo:=Cursor_Stereo(dx);
  fMovieCursor.xyz_dx:=dx;

  fMovieCursor.AfterPaint;
end;

function timCard.Cursor_stereo(out dx: integer): boolean;
begin
  if Assigned(fRight) then
    dx:=(XPos - fRight.XPos) * z_dst
  else
    dx:=Stereo_a.Depth * z_dst;

  Result:=IsStereo;
end;

constructor TCardBoxDraw.Create(ACard: timCard);
begin
  inherited Create; fCard:=ACard
end;

procedure TCardBoxDraw.CardDraw;
begin
  fCard.Card_Draw(nil);
end;

function TCardBoxDraw.Horz_Scroll(Code: TCardScrollCode; pos: int): int;
begin
  Result:=fCard.Horz_Scroll(Code,pos);
end;

function TCardBoxDraw.Vert_Scroll(Code: TCardScrollCode; pos: int): int;
begin
  Result:=fCard.Vert_Scroll(Code,pos);
end;

end.
