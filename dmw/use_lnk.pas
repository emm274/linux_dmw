unit use_lnk; interface

uses
  Classes,otypes,xproj;
  
type
  ILink2 = interface(IUnknown)
    ['{147182FD-27F7-4592-B202-86FEF081D480}']

    function Open(Path: PChar): Integer; stdcall;
    procedure SaveAs(Path: PChar); stdcall;

    function GetCount: Integer; stdcall;

    function ImageWidth: Integer; stdcall;
    function ImageHeight: Integer; stdcall;

    procedure GetSys(out sys: tsys); stdcall;
    procedure SetSys(sys: psys); stdcall;

    procedure GetPoint(Ind: Integer; out a,b: TGauss); stdcall;
    procedure SetPoint(Ind: Integer; a,b: PGauss); stdcall;

    procedure AddPoint(a,b: PGauss); stdcall;

    procedure l_to_g(ix,iy: Double; out ox,oy: Double); stdcall;
    function l_to_r(ix,iy: Double; out ox,oy: Double): Integer; stdcall;

    procedure g_to_l(ix,iy: Double; out ox,oy: Double); stdcall;
    procedure r_to_l(ix,iy: Double; pps: Integer; out ox,oy: Double); stdcall;

    function Containsg(x,y: double): Boolean; stdcall;
  end;

function dll_calc_tri(lp: PLPoly; ndp: Integer;
                      var Tri: PTriWords): Integer;

function dll_bound_tri(lp: PLPoly; ndp: Integer;
                       var Bound: PIntegers): Integer;

function dll_calc_ribs(lp: PLPoly; ndp: Integer;
                       var Buf: PIntegers): Integer;

function dll_tri_cline(lp: PLPoly; n1,n2: Integer;
                       var Buf: PLPoly; tin: PChar): Integer;

procedure dll_create_img_link(Dest: PChar; l,g: PGPoly; sys: psys);
function dll_lnk_restore(src,dst,old,new: PChar): Integer;

function GetLink2Intf(const IID: TGUID; var Obj): HResult;

function dll_lnk_Exist(Path: PChar): Boolean;

function GetJpgProj(pos: pxyz;      // [WGS-84] Lat,Lon,Alt
                    rot: pxyz;      // Gam,Tet,Psi

                    cam_in: pxyz;   // Ширина, Высота, Focus
                    cam_rot: pxyz;
                    cam_swap: Longbool;

                    ground: double;

                    out_sys: psys;  // система координат
                    var Obj         // IProject2
                    ): Boolean;

implementation

uses
  dynlibs;

const
  dll_tri = 'dll_tri.dll';
  dll_lnk = 'dll_lnk.dll';

function dll_calc_tri(lp: PLPoly; ndp: Integer;
                      var Tri: PTriWords): Integer;
type
  tfunc = function(lp: PLPoly; ndp: Integer;
                   var Tri: PTriWords): Integer; stdcall;
var
  dll: THandle; func: tfunc;
begin
  Result:=0; Tri:=xFreePtr(Tri);

  if ndp > 3 then begin
    dll:=LoadLibrary(dll_tri);
    if dll >= 32 then begin
      @func:=GetProcAddress(dll,'Calc_tri');
      Result:=func(lp,ndp,Tri);
      FreeLibrary(dll)
    end;
  end
end;

function dll_bound_tri(lp: PLPoly; ndp: Integer;
                       var Bound: PIntegers): Integer;
type
  tfunc = function(lp: PLPoly; ndp: Integer;
                   var Bound: PIntegers): Integer; stdcall;
var
  dll: THandle; func: tfunc;
begin
  Result:=0; Bound:=xFreePtr(Bound);

  if ndp > 3 then begin
    dll:=LoadLibrary(dll_tri);
    if dll >= 32 then begin
      @func:=GetProcAddress(dll,'Bound_tri');
      Result:=func(lp,ndp,Bound);
      FreeLibrary(dll)
    end;
  end
end;

function dll_calc_ribs(lp: PLPoly; ndp: Integer;
                       var Buf: PIntegers): Integer;
type
  tfunc = function(lp: PLPoly; ndp: Integer;
                   var Buf: PIntegers): Integer; stdcall;
var
  dll: THandle; func: tfunc;
begin
  Result:=0; Buf:=nil;

  if ndp > 3 then begin
    dll:=LoadLibrary(dll_tri);
    if dll >= 32 then begin
      @func:=GetProcAddress(dll,'Calc_ribs');
      Result:=func(lp,ndp,Buf);
      FreeLibrary(dll)
    end;
  end
end;

function dll_tri_cline(lp: PLPoly; n1,n2: Integer;
                       var Buf: PLPoly; tin: PChar): Integer;
type
  tfunc = function(lp: PLPoly; n1,n2: Integer;
                   var Buf: PLPoly; tin: PChar): Integer; stdcall;
var
  dll: THandle; func: tfunc;
begin
  Result:=0; Buf:=nil;

  if n1+n2 > 3 then begin
    dll:=LoadLibrary(dll_tri);
    if dll >= 32 then begin
      @func:=GetProcAddress(dll,'tri_cline');
      Result:=func(lp,n1,n2,Buf,tin);
      FreeLibrary(dll)
    end;
  end
end;

var
  lnk_dll: THandle;

procedure dll_create_img_link(Dest: PChar; l,g: PGPoly; sys: psys);
type
  tproc = procedure(Dest: PChar; l,g: PGPoly; sys: psys); stdcall;
var
  proc: tproc;
begin
  if lnk_dll = 0 then
  lnk_dll:=LoadLibrary('dll_lnk.dll');

  if lnk_dll >= 32 then begin
    @proc:=GetProcAddress(lnk_dll,'create_img_link');
    if Assigned(proc) then proc(Dest, l,g, sys);
  end;
end;

function dll_lnk_restore(src,dst,old,new: PChar): Integer;
type
  tfunc = function(src,dst,old,new: PChar): Integer; stdcall;
var
  func: tfunc;
begin
  Result:=0;

  if lnk_dll = 0 then
  lnk_dll:=LoadLibrary('dll_lnk.dll');

  if lnk_dll >= 32 then begin
    @func:=GetProcAddress(lnk_dll,'lnk_restore');
    if Assigned(func) then Result:=func(src,dst,old,new);
  end;
end;

function GetLink2Intf(const IID: TGUID; var Obj): HResult;
type
  tfunc = function(const CLSID,IID: TGUID; var Obj): HResult; stdcall;
var
  func: tfunc;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  if lnk_dll = 0 then
  lnk_dll:=LoadLibrary('dll_lnk.dll');

  if lnk_dll >= 32 then begin
    @func:=GetProcAddress(lnk_dll,'DllGetInterface');
    if Assigned(func) then Result:=func(IID,IID,Obj)
  end
end;

function dll_lnk_Exist(Path: PChar): Boolean;
var
  lnk: ILink2;
begin
  Result:=false;

  if GetLink2Intf(ILink2,lnk) = S_OK then
  Result:=lnk.Open(Path) >= 3;

  lnk:=nil;
end;

function GetJpgProj(pos,rot, cam_in,cam_rot: pxyz;
                    cam_swap: Longbool; ground: double;
                    out_sys: psys; var Obj): Boolean;
type
  tfunc = function(pos,rot, cam_in,cam_rot: pxyz;
                   cam_swap: Longbool; ground: double;
                   out_sys: psys; var Obj): Boolean; stdcall;
var
  func: tfunc;
begin
  Result:=false; TPointer(Obj):=0;

  if lnk_dll = 0 then
  lnk_dll:=LoadLibrary('dll_lnk.dll');

  if lnk_dll >= 32 then begin
    @func:=GetProcAddress(lnk_dll,'GetJpgProj');
    if Assigned(func) then
    Result:=func(pos,rot, cam_in,cam_rot,cam_swap,ground,out_sys,Obj)
  end
end;

initialization
begin
  lnk_dll:=0;
end;

finalization
begin
  if lnk_dll >= 32 then
  FreeLibrary(lnk_dll);
end;

end.
