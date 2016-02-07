library dll_jpg;

uses
  Classes,
  SysUtils,
  Interfaces,
  otypes,ijpg,
  jpeg62;

function DllGetInterface(const CLSID,IID: TGUID; var Obj): HResult; stdcall;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  if IsEqualGUID(IID,IJpeg) then

  with tjpegObj.Create do
  if GetInterface(IID,OBJ) then
  Result:=S_OK
end;

exports
  DllGetInterface;

begin
end.

