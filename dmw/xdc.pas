unit xdc; interface

uses
  Classes,
  LCLType,LCLIntf,Math,
  otypes,FPimage;

var
  sys_CYCaption: Integer;
  sys_CXVScroll: Integer;
  sys_CYHScroll: Integer;

function ColorToFPColor(Color: int): TFPColor;

function xGetDC(Wnd: HWND): HDC;
function xReleaseDC(Wnd: HWND; DC: HDC): HDC;

function xDeviceRatio(DC: HDC): Double;
function xDevice_dpi(DC: HDC): Integer;
function xDeviceAspect(DC: HDC): Double;

function xMetersPerPixel(DC: HDC): Double;
function xPixelsPerMeter(DC: HDC): TGauss;
function xDeviceScale(sc: double): double;

function xDeviceDpi: Integer;

function xDeviceStep(Dev: HDC; st: Integer): Integer;

function xBitsPerPixel(DC: hDC): Integer;
function xPlanesPerPixel(DC: hDC): Integer;

function xPixels(dist,scale: Double): Longint;
function xPenThick(DC: HDC; mm: Double): Integer;

function xResolution(w,h: Double): Double;

function xCreateCompatibleBitmap(Width, Height: Integer): HBITMAP;

procedure SetBrushOrgEx(DC: HDC; X,Y: int);

implementation

function ColorToFPColor(Color: int): TFPColor;
begin
  Result:=FPColor( (Color and 255) shl 8,
                   ((Color shr 8) and 255) shl 8,
                   ((Color shr 16) and 255) shl 8)
end;

function xGetDC(Wnd: HWND): HDC;
begin
  Result:=GetDC(Wnd)
end;

function xReleaseDC(Wnd: HWND; DC: HDC): HDC;
begin
  if DC <> 0 then ReleaseDC(Wnd,DC);
  Result:=0
end;

function xDeviceRatio(DC: HDC): Double;
var
  dev_ppi,dc_ppi: Integer;
begin
  Result:=1;
  if DC <> 0 then begin
    dev_ppi:=GetDeviceCaps(DC,LogPixelSx);
    dc_ppi:=GetDeviceCaps(DC,LogPixelSx);
    if (dev_ppi > 0) and (dc_ppi > 0) then
    Result:=dev_ppi / dc_ppi
  end
end;

function xDevice_dpi(DC: HDC): Integer;
begin
  Result:=GetDeviceCaps(DC,LogPixelSx)
end;

function xDeviceAspect(DC: HDC): Double;
var
  mpp,mpp0: Double;
begin
  Result:=1;
  if DC <> 0 then begin
    mpp:=xMetersPerPixel(DC);
    mpp0:=xMetersPerPixel(0);
    if mpp > 1E-8 then
    Result:=mpp0 / mpp
  end
end;

function xMetersPerPixel(DC: HDC): Double;
var
  _DC: HDC; ppi: Integer;
begin
  Result:=0;

  _DC:=0; if DC = 0 then begin
    _DC:=xGetDC(0); DC:=_DC
  end;

  if DC <> 0 then begin
    ppi:=GetDeviceCaps(DC,LogPixelSx);
    if ppi > 0 then Result:=0.0254 / ppi;
  end;

  if _DC <> 0 then xReleaseDC(0,_DC)
end;

function xDeviceScale(sc: double): double;
begin
  Result:=sc; if sc > 0 then
  Result:=1/(xMetersPerPixel(0)*sc)
end;

function xDeviceDpi: Integer;
var
  DC: hDC;
begin
  DC:=GetDC(0);
  Result:=GetDeviceCaps(DC,LogPixelSx);
  ReleaseDC(0,DC)
end;

function xPixelsPerMeter(DC: HDC): TGauss;
var
  _DC: HDC; s: TGauss;
begin
  s.x:=0; s.y:=0;

  _DC:=0; if DC = 0 then begin
    _DC:=xGetDC(0); DC:=_DC
  end;

  if DC <> 0 then begin
    s.x:=GetDeviceCaps(DC,LogPixelSx) / 0.0254;
    s.y:=GetDeviceCaps(DC,LogPixelSy) / 0.0254;
  end;

  if _DC <> 0 then xReleaseDC(0,_DC);

  Result:=s
end;

function xDeviceStep(Dev: HDC; st: Integer): Integer;
var
  ppm1,ppm2: TGauss;
begin
  Result:=st;

  ppm1:=xPixelsPerMeter(0);
  ppm2:=xPixelsPerMeter(Dev);

  if ppm1.x >= 1 then
  if ppm2.x >= 1 then
  Result:=Round(st / ppm1.x * ppm2.x);
end;

function xBitsPerPixel(DC: hDC): Integer;
var
  _DC: HDC;
begin
  Result:=0;

  _DC:=0; if DC = 0 then begin
    _DC:=xGetDC(0); DC:=_DC
  end;

  if DC <> 0 then
  Result:=GetDeviceCaps(DC, BITSPIXEL) *
          GetDeviceCaps(DC, PLANES);

  if _DC <> 0 then xReleaseDC(0,_DC);
end;

function xPlanesPerPixel(DC: hDC): Integer;
var
  _DC: HDC;
begin
  Result:=0;

  _DC:=0; if DC = 0 then begin
    _DC:=xGetDC(0); DC:=_DC
  end;

  if DC <> 0 then
  Result:=GetDeviceCaps(DC, PLANES);

  if _DC <> 0 then xReleaseDC(0,_DC);
end;

function xPixels(dist,scale: Double): Longint;
begin
  Result:=0; if scale > 0.1 then
  Result:=Round(dist / scale / xMetersPerPixel(0) + 0.5)
end;

function xPenThick(DC: HDC; mm: Double): Integer;
var
  dpi: Integer;
begin
  dpi:=GetDeviceCaps(DC,LogPixelSx);
  Result:=Round(dpi / 25.4 * mm);
end;

function xResolution(w,h: Double): Double;
var
  DC: hDC; kx,ky: Double;
begin
  DC:=GetDC(0);
  kx:=w/GetDeviceCaps(DC,HorzRes);
  ky:=h/GetDeviceCaps(DC,VertRes);
  Result:=Max(kx,ky);
  ReleaseDC(0,DC);
end;

function xCreateCompatibleBitmap(Width, Height: Integer): HBITMAP;
var
  DC: HDC;
begin
  Result:=0;

  DC:=GetDC(0);
  if DC <> 0 then begin
    Result:=CreateCompatibleBitmap(DC, Width, Height);
    ReleaseDC(0,DC);
  end;
end;

procedure SetBrushOrgEx(DC: HDC; X,Y: int);
begin
end;

procedure Init;
begin
  sys_CYCaption:=GetSystemMetrics(sm_CYCaption);
  sys_CXVScroll:=GetSystemMetrics(sm_CXVScroll);
  sys_CYHScroll:=GetSystemMetrics(sm_CYHScroll);
end;

begin
  Init;
end.

