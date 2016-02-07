unit tstu3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, freetypep;

type

  { TDfmTst3 }

  TDfmTst3 = class(TForm)
    btTest: TButton;
    lbLog: TListBox;
    paCard1: TPanel;
    paCard2: TPanel;
    paCard3: TPanel;
    pbCard3: TPaintBox;
    paTop: TPanel;
    pbCard1: TPaintBox;
    pbCard2: TPaintBox;
    Splitter1: TSplitter;
    procedure btTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lbLogClick(Sender: TObject);
    procedure pbCard1Click(Sender: TObject);
    procedure pbCard1Paint(Sender: TObject);
    procedure pbCard2Paint(Sender: TObject);
    procedure pbCard3Paint(Sender: TObject);
  private
    map1: TObject;
    map2: TObject;
    map3: TObject;
    fcolor: Integer;

    drawtext: TDrawText;

    procedure log(const Msg: String);

  public
  end;

var
  DfmTst3: TDfmTst3;

implementation {$R *.lfm}

uses
  LCLType,
  LCLIntf,

  GraphType,
  IntfGraphics,
  LazCanvas,
  FPImage,
  FPCanvas,

  Windows,
  OTypes,
  wstrings,
  xini,
  xdc,
  idib,

  GR32,
  GR32_Polygons,
  GR32_Backends_LCL_Gtk,

  InterfaceBase,
  gtk2int,
  gdk2,
  gtk2def,

  gdk2pixbuf,
  glib2,

  xdib,
  xdib32,
  xutils;

type
  TAbstractMap = class
    function AllocRGB(w,h: Integer): bool; virtual; abstract;
    procedure Clear(Color: Integer); virtual; abstract;

    procedure SetBrush(Color: int); virtual; abstract;
    procedure SetPen(Width,Color: int); virtual; abstract;

  protected
    function GetWidth: int; virtual; abstract;
    function GetHeight: int; virtual; abstract;
    function GetDepth: int; virtual; abstract;

    function GetBitmap: TBitmap; virtual; abstract;
    function GetCanvas: TCanvas; virtual; abstract;

  public
    property Width: int read GetWidth;
    property Height: int read GetHeight;
    property Depth: int read GetDepth;

    property Bitmap: TBitmap read GetBitmap;
    property Canvas: TCanvas read GetCanvas;
  end;

  TLazIntfMap = class(TAbstractMap)

    constructor Create;
    destructor Destroy; override;

    function AllocRGB(w,h: Integer): bool; override;
    procedure Clear(Color: Integer); override;

    procedure SetBrush(AColor: int); override;
    procedure SetPen(AWidth,AColor: int); override;

 protected
    function GetWidth: int; override;
    function GetHeight: int; override;
    function GetDepth: int; override;

    function GetBitmap: TBitmap; override;
    function GetCanvas: TCanvas; override;

  private
    fImage: TLazIntfImage;
    fCanvas: TLazCanvas;
    fBitmap: TBitmap;
  end;

constructor TLazIntfMap.Create;
begin
  inherited Create;

  fImage:=TLazIntfImage.Create(0,0);
  fCanvas:=TLazCanvas.Create(fImage);

  fBitmap:=TBitmap.Create;
end;

destructor TLazIntfMap.Destroy;
begin
  fBitmap.Free;

  fCanvas.Free;
  fImage.Free;

  inherited
end;

function TLazIntfMap.GetWidth: int;
begin
  Result:=fImage.Width
end;

function TLazIntfMap.GetHeight: int;
begin
  Result:=fImage.Height;
end;

function TLazIntfMap.GetDepth: int;
begin
  Result:=fImage.DataDescription.BitsPerPixel;
end;

function TLazIntfMap.GetBitmap: TBitmap;
begin
  Result:=nil;
  fBitmap.LoadFromIntfImage(fImage);
  if fBitmap.HandleAllocated then
  Result:=fBitmap
end;

function TLazIntfMap.GetCanvas: TCanvas;
var
  bmp: TBitmap;
begin
  Result:=nil;
  bmp:=Bitmap;
  if Assigned(bmp) then
  Result:=bmp.Canvas;
end;

function TLazIntfMap.AllocRGB(w,h: Integer): bool;
var
  raw: TRawImage;
begin
  if (fImage.PixelData = nil)
  or (fImage.Width <> w)
  or (fImage.Height <> h)
  or (Depth <> 32) then begin

    raw.Init;
    raw.Description.Init_BPP32_A8R8G8B8_BIO_TTB(w,h);
    raw.CreateData(true);
    fImage.SetRawImage(raw);

    Clear(clWhite);
  end;

  Result:=fImage.PixelData <> nil;
end;

procedure TLazIntfMap.Clear(Color: Integer);
var
  i,n: int; di: PInteger;
begin
  Color:=(Color shl 8) or $FF;

  di:=PInteger(fImage.PixelData);
  if Assigned(di) then begin
    n:=fImage.DataDescription.BytesPerLine *
       fImage.DataDescription.Height div 4;
    for i:=1 to n do begin
      di^:=Color; Inc(di)
    end;
  end;
end;

procedure TLazIntfMap.SetBrush(AColor: int);

begin
  fCanvas.Brush.FPColor:=ColorToFPColor(AColor)
end;

procedure TLazIntfMap.SetPen(AWidth,AColor: int);
begin
  fCanvas.Pen.FPColor:=ColorToFPColor(AColor);
  fCanvas.Pen.Width:=AWidth
end;

type
  TPaintMap = class(TAbstractMap)

    constructor Create;
    destructor Destroy; override;

    function AllocRGB(w,h: Integer): bool; override;
    procedure Clear(Color: Integer); override;

    procedure SetBrush(AColor: int); override;
    procedure SetPen(AWidth,AColor: int); override;

  protected
     function GetWidth: int; override;
     function GetHeight: int; override;
     function GetDepth: int; override;

     function GetBitmap: TBitmap; override;
     function GetCanvas: TCanvas; override;

  private
    fBitmap: TBitmap;
  end;

constructor TPaintMap.Create;
begin
  inherited Create;
  fBitmap:=TBitmap.Create;
end;

destructor TPaintMap.Destroy;
begin
  fBitmap.Free;
  inherited
end;

function TPaintMap.GetBitmap: TBitmap;
begin
  Result:=nil;
  if fBitmap.HandleAllocated then
  Result:=fBitmap;
end;

function TPaintMap.GetCanvas: TCanvas;
begin
  Result:=fBitmap.Canvas
end;


function TPaintMap.GetWidth: int;
begin
  Result:=fBitmap.Width
end;

function TPaintMap.GetHeight: int;
begin
  Result:=fBitmap.Height;
end;

function TPaintMap.GetDepth: int;
begin
  Result:=24
end;

function TPaintMap.AllocRGB(w,h: Integer): bool;
var
  map: BITMAP;
begin
  Result:=false;

  if not fBitmap.HandleAllocated
  or (fBitmap.Width <> w)
  or (fBitmap.Height <> h)
  or (Depth <> 24) then begin
    fBitmap.FreeImage;
    fBitmap.Handle:=CreateBitmap(w,h,1,24,nil);

    if fBitmap.HandleAllocated then begin
      Clear(clWhite);
      Canvas.Rectangle(10,10,100,100);
    end;
  end;

  if fBitmap.HandleAllocated then
  if GetObject(fBitmap.Handle,Sizeof(map),@map) > 0 then
  Result:=Assigned(map.bmBits);

  Result:=true
end;

procedure TPaintMap.Clear(Color: Integer);
var
  cv: TCanvas;
begin
  cv:=Bitmap.Canvas;
  cv.Brush.Color:=Color;
  cv.FillRect(0,0,Bitmap.Width,Bitmap.Height);
end;

procedure TPaintMap.SetBrush(AColor: int);
begin
  Bitmap.Canvas.Brush.Color:=AColor;
end;

procedure TPaintMap.SetPen(AWidth,AColor: int);
begin
  Bitmap.Canvas.Pen.Color:=AColor;
  Bitmap.Canvas.Pen.Width:=AWidth
end;

type
  TPaintMap32 = class(TAbstractMap)

    constructor Create;
    destructor Destroy; override;

    function AllocRGB(w,h: Integer): bool; override;
    procedure Clear(Color: Integer); override;

    procedure SetBrush(AColor: int); override;
    procedure SetPen(AWidth,AColor: int); override;

  protected
     function GetWidth: int; override;
     function GetHeight: int; override;
     function GetDepth: int; override;

     function GetBitmap: TBitmap; override;
     function GetCanvas: TCanvas; override;

  private
    fBitmap32: TBitmap32;
  end;

constructor TPaintMap32.Create;
begin
  inherited Create;
  fBitmap32:=TBitmap32.Create;
end;

destructor TPaintMap32.Destroy;
begin
  fBitmap32.Free;
  inherited
end;

function TPaintMap32.GetBitmap: TBitmap;
begin
  Result:=nil;
end;

function TPaintMap32.GetCanvas: TCanvas;
begin
  Result:=fBitmap32.Canvas
end;


function TPaintMap32.GetWidth: int;
begin
  Result:=fBitmap32.Width
end;

function TPaintMap32.GetHeight: int;
begin
  Result:=fBitmap32.Height;
end;

function TPaintMap32.GetDepth: int;
begin
  Result:=32
end;

function TPaintMap32.AllocRGB(w,h: Integer): bool;
begin
  Result:=false;

  if (fBitmap32.Bits = nil)
  or (fBitmap32.Width <> w)
  or (fBitmap32.Height <> h) then
  if fBitmap32.SetSize(w,h) then
  fBitmap32.Clear(clWhite32);

  Result:=not fBitmap32.Empty
end;

procedure TPaintMap32.Clear(Color: Integer);
begin
  fBitmap32.Clear(Color);
end;

procedure TPaintMap32.SetBrush(AColor: int);
begin
  fBitmap32.Canvas.Brush.Color:=AColor;
end;

procedure TPaintMap32.SetPen(AWidth,AColor: int);
begin
  fBitmap32.Canvas.Pen.Color:=AColor;
  fBitmap32.Canvas.Pen.Width:=AWidth
end;

procedure TDfmTst3.FormCreate(Sender: TObject);
begin
  map1:=TLazIntfMap.Create;
  map2:=TPaintMap.Create;
  map3:=TPaintMap32.Create;

  drawtext:=TDrawText.Create;

  ini.GetForm(Self);
  fcolor:=100
end;

procedure TDfmTst3.FormDestroy(Sender: TObject);
begin
  ini.PutForm(Self);

  drawtext.Free;
  map3.Free;
  map2.Free;
  map1.Free;
end;

procedure TDfmTst3.FormResize(Sender: TObject);
begin
  paCard1.Width:=ClientWidth div 3;
  paCard3.Width:=paCard1.Width;
end;

procedure TDfmTst3.lbLogClick(Sender: TObject);
begin

end;

procedure TDfmTst3.pbCard1Click(Sender: TObject);
begin
  Inc(fcolor,10);
  pbCard1.Repaint;
end;

procedure TDfmTst3.log(const Msg: String);
begin
  lbLog.Items.Add(Msg);
end;

procedure TDfmTst3.pbCard1Paint(Sender: TObject);
var
  w,h: int;
  _map: TLazIntfMap;
  bmp: TBitmap; r: TRect;
begin
  w:=pbCard1.Width;
  h:=pbCard1.Height;

  _map:=TLazIntfMap(map1);
  if _map.AllocRGB(w,h) then begin
    bmp:=_map.Bitmap;
    if Assigned(bmp) then
    pbCard1.Canvas.Draw(0,0,bmp);
  end;
end;

procedure TDfmTst3.pbCard2Paint(Sender: TObject);
var
  w,h: int;
  _map: TPaintMap;
  src: TCanvas; r: TRect;
begin
  w:=pbCard2.Width;
  h:=pbCard2.Height;

  _map:=TPaintMap(map2);
  if _map.AllocRGB(w,h) then begin
    src:=_map.Bitmap.Canvas;
    r:=Rect(0,0,w,h);
    pbCard2.Canvas.CopyRect(r,src,r);
  end;
end;

procedure TDfmTst3.pbCard3Paint(Sender: TObject);

procedure test1;
var
  w,h,line: int;
  _map: TPaintMap32;
  backend: TLCLBackend;
  cv: TCanvas; rt: TRect;
begin
  w:=pbCard3.Width;
  h:=pbCard3.Height;

  _map:=TPaintMap32(map3);
  if _map.AllocRGB(w,h) then begin

    backend:=_map.fBitmap32.Backend as TLCLBackend;

    cv:=backend.Canvas;

    gdk_draw_line(TGtkDeviceContext(cv.Handle).Drawable,
                  TGtkDeviceContext(cv.Handle).GC,
                  0,0,1000,1000);

    cv.TextOut(10,10,'Hello');

    cv.Pen.Color:=clBlack;
    cv.Line(0,100,100,0);

    rt:=cv.ClipRect;

    _map.fBitmap32.PenColor:=clred32;
    _map.fBitmap32.MoveTo(0,0);
    _map.fBitmap32.LineToAS(100,100);

     _map.fBitmap32.SaveToFile('/home/emm/tol/1.bmp',false);

    backend.DoPaint(_map.fBitmap32,nil,
                    pbCard3.Canvas,nil);
  end;
end;

procedure test2;

function outText(const cv: ICanvas; x,y,size: int; ttf: PChar): int;
var
  s: TWideStr;
begin
  StrPCopyW(s,Format('%s_%d',[ttf,size]),255);

  drawtext.SetFont(ttf);
  drawtext.setsize(size);
  drawtext.Draw(cv,x,y,0,s);
  Result:=y + drawtext.GetExtent(nil).Y;
end;

var
  w,h,y: int;
  _map: TPaintMap32;
  bits: Pointer; dest: HDC;
  bmp32: TCanvas32;
begin
  w:=pbCard3.Width;
  h:=pbCard3.Height;

  _map:=TPaintMap32(map3);
  if _map.AllocRGB(w,h) then begin

    _map.fBitmap32.FillRect(0,0,w,h,clLightGray32);

    bits:=_map.fBitmap32.Bits;

    bmp32:=TCanvas32.Create(_map.fBitmap32);
    try
      bmp32.SetPenColor(clNavy);

      y:=40;
      y:=outText(bmp32, 10,y, 20,'bm431n');
      y:=outText(bmp32, 10,y, 20,'bm431n');
      y:=outText(bmp32, 10,y, 12,'bm431n');
      y:=outText(bmp32, 10,y, 12,'bm431n');
  (*
      y:=outText(bmp32, 10,y, 20,'r151n');
      y:=outText(bmp32, 10,y, 12,'r151n');
      y:=outText(bmp32, 10,y, 20,'r151n');
      y:=outText(bmp32, 10,y, 12,'r151n');
      *)
    finally
      bmp32.Free;
    end;

    dest:=pbCard3.Canvas.Handle;

    gdk_draw_rgb_32_image(
      TGtkDeviceContext(Dest).Drawable,
      TGtkDeviceContext(Dest).GC,
      0,0,w,h,
      GDK_RGB_DITHER_NORMAL,
      Pguchar(bits),w*4)

  end;
end;

begin
  test2
end;

procedure TDfmTst3.btTestClick(Sender: TObject);

procedure log_test(const Capt: String; dt1,dt2,dt3: int);
begin
  if dt2 = 0 then dt2:=1; if dt3 = 0 then dt3:=1;
  log(Format('%s: %d / %d / %d = %0.3f %0.3f',
      [Capt,dt1,dt2,dt3, dt1/dt2,dt1/dt3]))
end;

procedure test_fill(map1: TLazIntfMap;
                    map2: TPaintMap;
                    map3: TPaintMap32);
const
  max = 10000;
var
  i,t1,t2,t3: int;
begin
  t1:=GetTickCount;
  for i:=1 to max do map1.Clear(RGB(i,i,i));

  t1:=GetTickCount-t1;

  t2:=GetTickCount;
  for i:=1 to max do map2.Clear(RGB(i,i,i));
  t2:=GetTickCount-t2;

  t3:=GetTickCount;
  for i:=1 to max do map3.Clear(RGB(i,i,i));
  t3:=GetTickCount-t3;

  log_test('test fill',t1,t2,t3);
end;

procedure test_lines(map1: TLazIntfMap;
                     map2: TPaintMap;
                     map3: TPaintMap32);

const
  max = 10000;

procedure draw_lines(map: TAbstractMap);
var
  cv: TFPCustomCanvas;
  i,w,h: int; a,b: TPoint;
begin
  cv:=map.Canvas;
  w:=map.Width;
  h:=map.Height;

  for i:=1 to max do begin
    a.X:=Random(w);
    a.Y:=Random(h);
    b.X:=Random(w);
    b.Y:=Random(h);
    cv.Line(a,b);
  end;
end;

procedure draw_lines32(map: TPaintMap32);
var
  bmp: TBitmap32;
  i,w,h: int; a,b: TPoint;
begin
  bmp:=map.fBitmap32;
  w:=map.Width;
  h:=map.Height;

  for i:=1 to max do begin
    a.X:=Random(w);
    a.Y:=Random(h);
    b.X:=Random(w);
    b.Y:=Random(h);
    bmp.LineS(a.X,a.Y,b.X,b.Y,0,false)
  end;
end;

var
  t1,t2,t3: int;
begin
  map1.SetPen(1,RGB(0,255,0));

  Randomize;

  t1:=GetTickCount;
  draw_lines(map1);
  t1:=GetTickCount-t1;

  map2.SetPen(1,RGB(0,0,255));

  t2:=GetTickCount;
  draw_lines(map2);
  t2:=GetTickCount-t2;

  t3:=GetTickCount;
  draw_lines32(map3);
  t3:=GetTickCount-t3;

  log_test('test line',t1,t2,t3);
end;

procedure test_tris(map1: TLazIntfMap;
                    map2: TPaintMap;
                    map3: TPaintMap32);

const
  max = 1000;

procedure draw_tris(map: TAbstractMap);
var
  cv: TFPCustomCanvas;
  i,w,h: int; p: Array of TPoint;
begin
  cv:=map.Canvas;
  w:=map.Width;
  h:=map.Height;

  SetLength(p,4);

  for i:=1 to max do begin
    p[0].X:=Random(w);
    p[0].Y:=Random(h);
    p[1].X:=Random(w);
    p[1].Y:=Random(h);
    p[2].X:=Random(w);
    p[2].Y:=Random(h);
    p[3]:=p[0];
    cv.Polygon(p);
  end;
end;

procedure draw_tris32(map: TPaintMap32);
var
  bmp: TBitmap32;
  Polygon32: TPolygon32;
  tri: Array[0..3] of TFixedPoint;
  i,w,h: int; cr: TRect;
begin
  Polygon32:=TPolygon32.Create;
  try
    bmp:=map.fBitmap32;
    w:=map.Width;
    h:=map.Height;

    cr:=bmp.ClipRect;

    for i:=1 to max do begin
      tri[0]:=FixedPoint( Random(w),Random(h) );
      tri[1]:=FixedPoint( Random(w),Random(h) );
      tri[2]:=FixedPoint( Random(w),Random(h) );
      tri[3]:=tri[0];
      Polygon32.Clear;
      Polygon32.AddPoints(tri[0],4);
      Polygon32.Draw(bmp,clBlack32,clGreen32);
    end;
  finally
    Polygon32.Free;
  end;
end;

var
  t1,t2,t3: int;
begin
  Randomize;

  map1.SetPen(1,RGB(0,255,0));
  t1:=GetTickCount;
  draw_tris(map1);
  t1:=GetTickCount-t1;

  map2.SetPen(1,RGB(0,0,255));
  t2:=GetTickCount;
  draw_tris(map2);
  t2:=GetTickCount-t2;

  t3:=GetTickCount;
  draw_tris32(map3);
  t3:=GetTickCount-t3;

  log_test('test tri',t1,t2,t3);
end;

var
  _map1: TLazIntfMap;
  _map2: TPaintMap;
  _map3: TPaintMap32;
begin
  btTest.Enabled:=false;
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;

  _map1:=TLazIntfMap(map1);
  _map2:=TPaintMap(map2);
  _map3:=TPaintMap32(map3);

  lbLog.Items.BeginUpdate;
  lbLog.Items.Clear;

  log('LazIntf / Bitmap / Bitmap32');
  test_fill(_map1,_map2,_map3);
  test_lines(_map1,_map2,_map3);
  test_tris(_map1,_map2,_map3);

  lbLog.Items.EndUpdate;
  pbCard1.Repaint;
  pbCard2.Repaint;
  pbCard3.Repaint;

  btTest.Enabled:=true;
  Screen.Cursor:=crDefault;
end;

end.

