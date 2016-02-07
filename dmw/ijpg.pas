unit ijpg; interface

uses
  Windows,LCLType,OTypes;
      
type
  IImage = interface(IUnknown)
    ['{028D299D-AD2A-41F7-A779-DD7F23D4A9F9}']
    function GetWidth: int; stdcall;
    function GetHeight: int; stdcall;
    function GetBits: int; stdcall;

    function GetRect(x,y,w,h: int): PBytes; stdcall;
  end;

  pjpegTables = ^tjpegTables;
  tjpegTables = record
    IsValid: longbool;
    data: Pointer;
    size: int
  end;

  IJpeg = interface(IUnknown)
    ['{E4D2736C-44DD-4DD3-ADA0-323A13B1C715}']

    function GetTablesSize(var Tables: tjpegTables): int; stdcall;

    function LoadTables(var Tables: tjpegTables;
                        Data: Pointer; DataLen: int): Boolean; stdcall;

    function GetInfo(Jpeg: Pointer; JpegLen: int;
                     Path: PChar; Info: PBitmap): Boolean; stdcall;

    function Decompress(dib: Pointer;
                        dib_w,dib_h,dib_bits: int;
                        Jpeg: Pointer; JpegLen: int;
                        Tables: Pointer): Boolean; stdcall;

    function Decompress1(dib: Pointer;
                         dib_w,dib_h,dib_bits: int;
                         posX,posY,zoom: int;
                         Jpeg: Pointer; JpegLen: int;
                         Tables: Pointer): Boolean; stdcall;

    function Compress(Dest: Pointer; DestSize: int;

                      Dib: Pointer;
                      Dib_w,Dib_h,Dib_bits: int;
                      Quality: int): int; stdcall;

    function Compressf(Dest: PChar;
                       const img: IImage;
                       Quality: int): int64; stdcall;
  end;

function GetJpgInterface(const IID: TGUID; var Obj): HResult; stdcall;

procedure AllocJpegTables(jpg: IJpeg; var Tables: TJpegTables);
procedure FreeJpegTables(var Tables: TJpegTables);

implementation

uses
  dynlibs;

var
  dll_jpg: THandle;

function GetJpgInterface(const IID: TGUID; var Obj): HResult; stdcall;
type
  tfunc = function(const CLSID,IID: TGUID; var Obj): HResult; stdcall;
var
  func: tfunc;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  if dll_jpg = 0 then
  dll_jpg:=LoadLibrary('lib_jpg.so');

  if dll_jpg >= 32 then begin
    @func:=GetProcAddress(dll_jpg,'DllGetInterface');
    if Assigned(func) then Result:=func(IID,IID,Obj)
  end
end;

procedure AllocJpegTables(jpg: IJpeg; var Tables: TJpegTables);
begin
  if Tables.data = nil then
  if jpg.GetTablesSize(Tables) > 0 then
  Tables.data:=GetMem(Tables.size)
end;

procedure FreeJpegTables(var Tables: TJpegTables);
begin
  if Assigned(Tables.data) then
  FreeMem(Tables.data,Tables.size);
end;

initialization
begin
  dll_jpg:=0;
end;

finalization
begin
  if dll_jpg >= 32 then FreeLibrary(dll_jpg);
end;

end.
