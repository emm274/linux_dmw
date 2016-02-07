unit xintf1; interface

uses
  LCLType,otypes,xproj;

type
  IMosaic = interface(IUnknown)
    ['{E1F13CA0-1770-4D93-836A-ACEA7226A8EB}']

    function GetParam(Id: int): int; stdcall;
    procedure SetParam(Id,Value: int); stdcall;
    // 0= SetWnd
    // 1= SetClearColor
    // 2= SetTransparent
    // 3= SetPlastic
    // 4= IsPcx
    // 5= SetAlfa
    // 6= SetClipTiff

    procedure SetFrame(Left,Top,Scale: Double); stdcall;

    function BeginPaint(Amap: PBitmap): Boolean; stdcall;
    procedure EndPaint; stdcall;

    procedure Paint(Path: PChar; l: PGPoly;
                    const Proj: IProject2); stdcall;

    procedure OutBmp(Bmp: PBitmap;
                     const Proj: IProject2); stdcall;

    procedure ImagePaint(AWidth,AHeight: Integer; Path: PChar); stdcall;

    function GetImageInfo(Path: PChar; inf: PIntegers): HResult; stdcall;

    procedure Google(const sas: IGoogle;
                     const Proj: IProject2); stdcall;

    procedure Kml(const doc: IKmlImage;
                  const Proj: IProject2); stdcall;

    procedure Video(Amap: Pbitmap;
                    const Proj: IProject2;
                    avi: int; Frame: PBitmap;
                    Frag: PGPoly); stdcall;
  end;

  ITileImage = interface(IUnknown)
    ['{4B8614F8-281A-45D3-9000-0C052B46F5A1}']

    function GetSys(sys: psys): Double; stdcall;

    //Info: Width,Height,Bits,tileWidth,tileHeight
    function GetInfo(Info: PIntegers): Integer; stdcall;

    function Zoom(kz: Double): Double; stdcall;

    function GetTile(Col,Row: Integer; Bits: Pointer): Integer; stdcall;
  end;

  TSyncDataFunc = function(Owner: Pointer;
                           Name: PChar;
                           Buf: Pointer;
                           Size: Integer): HResult; stdcall;

  ISyncData = interface(IUnknown)
    ['{07EF8046-6BAD-4806-9E19-CDCAD6A40052}']

    procedure SetCallback(AOwner: Pointer;
                          AProc: TSyncDataFunc); stdcall;

    function Connect: HResult; stdcall;
    procedure Disconnect; stdcall;

    procedure dump(APath: PChar;
                   ABuf: Pointer;
                   ASize: Integer); stdcall;

    function Load(APath: PChar): Integer; stdcall;

    function OpenHist(APath: PChar): Integer; stdcall;
    function GetHistData(Ind: Integer; Path: PChar): HResult; stdcall;
    function GetHistCapt(Ind: Integer; Capt: PChar): HResult; stdcall;

    procedure Idle; stdcall;
  end;

function GetWmsInterface(Path: PChar;
                         const IID: TGUID;
                         var Obj): HResult; stdcall;

function GetSyncInterface(const IID: TGUID;
                          var Obj): HResult; stdcall;

implementation

uses
  dynlibs;

var
  dll_wms: THandle;
  dll_sync: THandle;

function GetWmsInterface(Path: PChar;
                         const IID: TGUID;
                         var Obj): HResult; stdcall;
type
  tfunc = function(Path: PChar;
                   const IID: TGUID;
                   var Obj): HResult; stdcall;
var
  func: tfunc;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  if dll_wms = 0 then
  dll_wms:=LoadLibrary('dll_wms.dll');

  if dll_wms >= 32 then begin
    @func:=GetProcAddress(dll_wms,'DllGetInterface');
    if Assigned(func) then Result:=func(Path,IID,Obj)
  end
end;

function GetSyncInterface(const IID: TGUID;
                          var Obj): HResult; stdcall;
type
  tfunc = function(const IID: TGUID;
                   var Obj): HResult; stdcall;
var
  func: tfunc;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  if dll_sync = 0 then
  dll_sync:=LoadLibrary('dll_sync.dll');

  if dll_sync >= 32 then begin
    @func:=GetProcAddress(dll_sync,'DllGetInterface');
    if Assigned(func) then Result:=func(IID,Obj)
  end
end;

initialization
begin
  dll_wms:=0;
  dll_sync:=0;
end;

finalization
begin
  if dll_wms >= 32 then FreeLibrary(dll_wms);
  if dll_sync >= 32 then FreeLibrary(dll_sync);
end;

end.