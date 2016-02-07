unit xintf2; interface

uses
  Classes,LCLType,otypes;

type
  TCardScrollCode = (
    _scLineUp,
    _scLineDown,
    _scPageUp,
    _scPageDown,
    _scPosition,
    _scTrack,
    _scTop,
    _scBottom,
    _scEndScroll
    );

  ICardBox = interface(IUnknown)
    ['{E6FF4F0B-E324-4BC9-8488-6F896B408B8A}']

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

    function GetSoft: bool; stdcall;
    procedure SetSoft(v: bool); stdcall;

    function GetSau: bool; stdcall;

    property Width: int read GetWidth;
    property Height: int read GetHeight;
    property Visible: bool read GetVisible;

    property Cursor: int read GetCursor write SetCursor;

    property ScreenCursor: int write SetScreenCursor;

    property Capture: bool read GetCapture write SetCapture;

    property Locked: bool read GetLocked write SetLocked;

    property Soft: bool read GetSoft write SetSoft;
    property Sau: bool read GetSau;
  end;

  ICardBoxDraw = interface(IUnknown)
    ['{0F0466F8-E26F-4D46-A135-CF3DA825906C}']
    procedure CardDraw; stdcall;

    function Horz_Scroll(Code: TCardScrollCode; pos: int): int; stdcall;
    function Vert_Scroll(Code: TCardScrollCode; pos: int): int; stdcall;
  end;

implementation

end.