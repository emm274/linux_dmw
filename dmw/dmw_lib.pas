unit dmw_lib; interface

uses
  Classes,otypes;
  
type
  tdm_lib = class
    constructor Create;
    destructor Destroy; override;
    function Active: Boolean;

    function GetProc(Name: PChar): Pointer;

    function Open(Path: PChar; rw: Boolean): Boolean;

    function Update: Boolean;
    procedure Close;

    function dm_New1(path,obj: PChar; sys: psys;
                     xmin,ymin,xmax,ymax: double;
                     ed: Integer): Boolean;

    function bl_Make(path,obj,nom: PChar): Boolean;

    function wgs_Make(path,obj: PChar;
                      const lt,rb: TGauss): Boolean;

    function Copy(src,dst,obj: PChar): Boolean;
    function add_grid(code,mm: int): int;

    function lp_fill(Items: PInt64s; Count: Integer;
                     lp: PLLine; lp_max: Integer;
                     const p, lt,rb: TPoint;
                     cut: Boolean): Integer;

    function geo_fill(lp: PLLine; lp_max: Integer;
                      const p, lt,rb: TPoint): Integer;

  private
    hlib: HModule;
  end;

function dm_new_Frame(Dest: PChar; dx,dy: double): bool;

function dmw_nom_prj(files: TStrings): Boolean;

implementation

uses
  SysUtils,dynlibs,
  convert,ofiles,ogauss;

constructor tdm_lib.Create;
begin
  inherited;
  hlib:=LoadLibrary('dll_dm.dll');
end;

destructor tdm_lib.Destroy;
begin
  if hlib >= 32 then begin
    Close; FreeLibrary(hlib);
  end; inherited;
end;

function tdm_lib.Active: Boolean;
begin
  Result:=hlib >= 32
end;

function tdm_lib.GetProc(Name: PChar): Pointer;
begin
  Result:=GetProcAddress(hlib,Name)
end;

function tdm_lib.Open(Path: PChar; rw: Boolean): Boolean;
type
  tfunc = function(dm: PChar; edit: boolean): word; stdcall;
var
  func: tfunc;
begin
  Result:=false;
  if hlib >= 32 then begin
    set_dll_env_dir(hlib);
    @func:=GetProcAddress(hlib,'dm_Open');
    if Assigned(func) then
    Result:=func(Path,rw) > 0
  end
end;

function tdm_lib.Update: Boolean;
type
  tfunc = function: Integer; stdcall;
var
  func: tfunc;
begin
  Result:=false;
  if hlib >= 32 then begin
    @func:=GetProcAddress(hlib,'dm_Update');
    if Assigned(func) then Result:=func > 0
  end
end;

procedure tdm_lib.Close;
type
  tproc = procedure; stdcall;
var
  proc: tproc;
begin
  if hlib >= 32 then begin
    @proc:=GetProcAddress(hlib,'dm_Done');
    if Assigned(proc) then proc
  end
end;

function tdm_lib.dm_New1(path,obj: PChar; sys: psys;
                         xmin,ymin,xmax,ymax: double;
                         ed: Integer): Boolean;
type
  tfunc = function(path,obj: PChar; sys: psys;
                   xmin,ymin,xmax,ymax: double;
                   ed: Integer): Integer; stdcall;
var
  func: tfunc;
begin
  Result:=false;

  if hlib >= 32 then begin
    @func:=GetProcAddress(hlib,'dm_New1');
    if Assigned(func) then
    if func(path,obj,sys, xmin,ymin,xmax,ymax, ed) > 0 then
    Result:=true
  end
end;

function tdm_lib.bl_Make(path,obj,nom: PChar): Boolean;
type
  tfunc = function(path,obj,nom: PChar): word; stdcall;
var
  func: tfunc;
begin
  Result:=false;

  if hlib >= 32 then begin
    @func:=GetProcAddress(hlib,'bl_Make');
    if Assigned(func) then
    Result:=func(path,obj,nom) > 0;
    Close;
  end
end;

function tdm_lib.wgs_Make(path,obj: PChar;
                          const lt,rb: TGauss): Boolean;
type
  tfunc = function(path,obj: PChar;
                   const lt,rb: TGauss): Boolean; stdcall;
var
  func: tfunc;
begin
  Result:=false;

  if hlib >= 32 then begin
    @func:=GetProcAddress(hlib,'wgs_Make');
    if Assigned(func) then
    Result:=func(path,obj,lt,rb);
  end
end;

function tdm_lib.Copy(src,dst,obj: PChar): Boolean;
type
  tfunc = function(src,dst,obj: PChar): Boolean; stdcall;
var
  func: tfunc;
begin
  Result:=false;

  if hlib >= 32 then begin
    @func:=GetProcAddress(hlib,'dm_Copy');
    if Assigned(func) then
    Result:=func(src,dst,obj);
  end
end;

function tdm_lib.add_grid(code,mm: int): int;
type
  tfunc = function(code,mm: int): int; stdcall;
var
  func: tfunc;
begin
  Result:=0;
  if hlib >= 32 then begin
    @func:=GetProcAddress(hlib,'dm_add_grid');
    if Assigned(func) then Result:=func(code,mm);
  end
end;

function tdm_lib.lp_fill(Items: PInt64s;
                         Count: Integer;

                         lp: PLLine; lp_max: Integer;
                         const p, lt,rb: TPoint;
                         cut: Boolean): Integer;

type
  tfunc = function(Items: PInt64s;
                   Count: Integer;

                   lp: PLLine; lp_max: Integer;
                   const p, lt,rb: TPoint;

                   cut: Boolean): Integer; stdcall;
var
  func: tfunc;
begin
  Result:=-1; lp.N:=-1;

  if hlib >= 32 then begin
    @func:=GetProcAddress(hlib,'dm_lp_fill');
    if Assigned(func) then
    Result:=func(Items,Count, lp,lp_max, p,lt,rb, cut);
  end
end;

function tdm_lib.geo_fill(lp: PLLine; lp_max: Integer;
                          const p, lt,rb: TPoint): Integer;
type
  tfunc = function(lp: PLLine; lp_max: Integer;
                   const p, lt,rb: TPoint): Integer; stdcall;
var
  func: tfunc;
begin
  Result:=-1; lp.N:=-1;

  if hlib >= 32 then begin
    @func:=GetProcAddress(hlib,'dm_geo_fill');
    if Assigned(func) then
    Result:=func(lp,lp_max, p,lt,rb);
  end
end;

function dm_new_Frame(Dest: PChar; dx,dy: double): bool;
var
  lib: tdm_lib;
begin
  Result:=false;

  lib:=tdm_lib.Create;
  try
    Result:=lib.dm_New1(Dest,nil,nil, 0,0,dx,dy, 1000);
  finally
    lib.Free
  end
end;

function dmw_nom_prj(files: TStrings): Boolean;
var
  lib: tdm_lib; sc: Integer;
  txt: TTextfile; g: GOrient;
  fn,dir,nm,nom: TShortstr;
begin
  Result:=false;

  if files.Count = 1 then
  if StrPLCopy(fn,files[0],255) <> nil then

  if This_Ext(fn,'.txt') then
  if This_Text(fn,dir) then begin

    StrDirectory(dir,fn);

    txt:=TTextfile.Create;
    lib:=tdm_lib.Create;
    try
      if txt.Open(fn) then
      while txt.xStrLine <> nil do
      if txt.x_str(nom) <> nil then begin

        dm_Nom_Scan(StrPas(nom),true, g, sc);
        if sc > 0 then begin

          StrCopy(nm,nom);
          StrReplace(nm,'.','_');

          StrUpdateExt(nm,nm,'.dm');
          StrPath(fn,dir,nm);

          if lib.bl_Make(fn,'100',nom) then begin
            if not Result then files.Clear;
            files.Add(fn); Result:=true
          end
        end
      end;
    finally
      lib.Free;
      txt.Free
    end
  end
end;

end.