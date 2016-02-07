unit idib; interface

{$mode delphi}

uses
  Classes,SysUtils,
  LCLType,otypes;

type
  IDrawText = interface;

  ICanvas = interface(IUnknown)
    ['{EA71E858-DCFE-428E-9E9D-C20A1763D732}']

    function GetWidth: int; stdcall;
    function GetHeight: int; stdcall;

    function GetDC: HDC; stdcall;
    function GetMap(out map: Bitmap): Boolean; stdcall;

    function GetBitmap: Pointer; stdcall;
    procedure BitmapChanged; stdcall;

    procedure SetDrawText(const ADrawText: IDrawText); stdcall;

    procedure SetPen(ps,w,cl,qu: int); stdcall;
    procedure SetBrush(bs,cl: int); stdcall;

    procedure SetPenColor(cl: int); stdcall;
    procedure SetBrushColor(cl: int); stdcall;

    procedure MoveTo(x,y: int); stdcall;
    procedure LineTo(x,y: int); stdcall;

    procedure Clear(cl: int); stdcall;

    procedure FillRect(const R: PRect); stdcall;

    procedure Rectangle(x1,y1,x2,y2: int); stdcall;
    procedure Ellipse(x1,y1,x2,y2: int); stdcall;

    procedure PolyPolyline(lp: PPoly; cp: PIntegers; N: int); stdcall;
    procedure Polyline(lp: PPoly; N: int); stdcall;

    procedure PolyPolygon(lp: PPoly; cp: PIntegers; N: int); stdcall;
    procedure Polygon(lp: PPoly; N: int); stdcall;

    function GetPixel(x,y: int): int; stdcall;
    procedure SetPixel(x,y,cl: int); stdcall;

    procedure DrawFocusRect(const R: TRect); stdcall;

    procedure SetTextColor(fc: int); stdcall;
    procedure TextOut(x,y: int; Str: PWideChar); stdcall;
    function TextExtent(Str: PWideChar; out ts: TPoint): int; stdcall;

    function MetersPerPixel: double; stdcall;

    procedure Set_Xor_Mode(Ground: int); stdcall;
    procedure Set_Copy_Mode; stdcall;

    function BeginPaint(R: PRect; maskX,maskY: int): Boolean; stdcall;
    procedure EndPaint; stdcall;

    function GetClipRect(out R: TRect): Boolean; stdcall;
    procedure SetClipRect(const R: PRect); stdcall;

    procedure ClipPolygon(lp: PPoly; cp: PIntegers; N: int); stdcall;

    procedure SetBkMode(Mode: int); stdcall;

    procedure dmwPolygon(lp: PPoly; cp: PIntegers; N,  alfa,msk,fc, pw,pc: int); stdcall;
    procedure fillPolygon(lp: PPoly; cp: PIntegers; N, msk,fc: int); stdcall;

    procedure alfaPolygon(lp: PPoly; cp: PIntegers;
                          N, alfa1,alfa2,fc: int;
                          at: PPoint; ak: float); stdcall;

    procedure CopyRect(Dst_R: PRect; Source: ICanvas; Src_R: PRect); stdcall;

    procedure CopyToDC(Dest: HDC); stdcall;

    procedure drawChar(x,y: int; data: PBytes; width,height,pitch: int); stdcall;
    procedure drawCharBW(x,y: int; data: PBytes; width,height,pitch: int); stdcall;

    procedure StretchDIBits(ox,oy,ow,oh: int;
                            bp: PBytes; inf: PBitmapInfo;
                            ix,iy,iw,ih: int;
                            alfa,blend: int); stdcall;
  end;

  IDrawText = interface(IUnknown)
    ['{DDC5A0B1-97AB-46BA-8701-BCA683F7597C}']

    function SetFont(FileName: PChar): int; stdcall;
    function SetSize(v: int): Boolean; stdcall;

    function GetExtent(Text: PWideChar): TPoint; stdcall;
    function GetExtentc(Char: WideChar): TPoint; stdcall;

    function Draw(const cv: ICanvas;
                  X,Y,Fi: int; Text: PWideChar): Boolean; stdcall;
  end;

implementation

end.

