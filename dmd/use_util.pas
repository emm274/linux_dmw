unit use_util; interface

uses
//  Windows,
  otypes{,XIntf};

type
  TDmwIntfProc = procedure(dll: THandle); stdcall;

var
  DmwIntfProc: TDmwIntfProc;

procedure dll_Angle_fmt(dll: THandle);

function Load_dm_dll(Name: PChar): THandle;
function Load_dm_util: THandle;
               (*
procedure dmd_Edit_filter(Path,Capt: PChar);

procedure dmd_rel_build(dm,prj,rel: PChar; Fr: PLPoly; sm: Integer);
function dmd_to_clu(dm,clu: PChar): int;

function dmd_Import(DllName,Path: PChar): PChar;

function dmd_Copy_List(Src,Dest: PChar;
                       List: PInt64s; Count: Integer): Boolean;
                       
function dmd_Paste(Dest,Path: Pchar; dlg: Integer): Boolean;
                 *)
implementation

uses
  dynlibs,
//  Sysutils,
  convert,
  ofiles,
  xapp
  //,XFiles
  ;

procedure dll_Angle_fmt(dll: THandle);
type
  tproc = procedure(fmt: Integer); stdcall;
var
  proc: tproc;
begin
  @proc:=GetProcAddress(dll,'set_Angle_fmt');
  if Assigned(proc) then proc(Angle_fmt);
end;

function Load_dm_dll(Name: PChar): THandle;
type
  TBoolProc = procedure(rus: Boolean); stdcall;
  TDirProc = procedure(dir: PChar); stdcall;
var
  dll: THandle;
  BoolProc: TBoolProc;
  DirProc: TDirProc;
begin
  dll_DumpIcon(nil);

  dll:=LoadLibrary(Name);
  if dll >= 32 then begin

    set_dll_app_handle(dll);
    set_dll_intf_dir(dll);

    if Strlen(env_ini) > 0 then
    set_dll_env_dir(dll);

    if not rus_interface then begin
      @BoolProc:=GetProcAddress(dll,'dmw_English');
      if Assigned(BoolProc) then BoolProc(false)
    end;

    if view_interface then begin
      @BoolProc:=GetProcAddress(dll,'set_view_intf');
      if Assigned(BoolProc) then BoolProc(true)
    end;

    @DirProc:=GetProcAddress(dll,'dmw_WorkDir');
    if Assigned(DirProc) then DirProc(WorkDir);

    @DirProc:=GetProcAddress(dll,'SetSpaceDir');
    if Assigned(DirProc) then DirProc(SpaceDir);

    dll_Angle_fmt(dll);

    if Assigned(DmwIntfProc) then
    DmwIntfProc(dll);
  end;

  Result:=dll
end;

function Load_dm_util: THandle;
begin
  Result:=Load_dm_dll('dm_util.dll');
end;
                    (*
procedure dmd_Edit_filter(Path,Capt: PChar);
type
  tproc = procedure(Path,Capt: PChar); stdcall;
var
  dll: THandle; proc: tproc;
begin
  dll:=Load_dm_dll('Filter.dll');
  if dll >= 32 then begin
    @Proc:=GetProcAddress(dll,'dm_Filter_Dial');
    Proc(Path,Capt); wFreeLibrary(dll)
  end;
end;

procedure dmd_rel_build(dm,prj,rel: PChar; Fr: PLPoly; sm: Integer);
type
  tproc = procedure(dm,prj,rel: PChar;
                   x1,y1,x2,y2,ed: Integer;
                   dmw: Boolean); stdcall;
var
  dll: THandle; proc: tproc;
begin
  dll:=Load_dm_util; if dll >= 32 then begin
    @proc:=GetProcAddress(dll,'dm_Relief');
    if Assigned(proc) then
    proc(dm,prj,rel,Fr[0].X,Fr[0].Y,Fr[1].X,Fr[1].Y,sm,false);
    wFreeLibrary(dll)
  end
end;

function dmd_to_clu(dm,clu: PChar): int;
type
  tfunc = function(dm,clu: PChar): int; stdcall;
var
  dll: HModule; func: tfunc;
begin
  Result:=-1;

  dll:=Load_dm_util; if dll >= 32 then begin
    @func:=GetProcAddress(dll,'dmw_to_clu');
    if Assigned(func) then
    Result:=func(dm,clu);
    wFreeLibrary(dll)
  end
end;

function dmd_Import(DllName,Path: PChar): PChar;
type
  TFunc = function(Path: PChar): PChar; stdcall;
var
  dll: THandle; Func: TFunc;
begin
  Result:=nil; StrCopy(Path,'');

  dll:=Load_dm_dll(DllName);
  if dll >= 32 then begin
    @Func:=GetProcAddress(dll,'dm_Import');
    if Assigned(Func) then Result:=Func(Path);
    wFreeLibrary(dll)
  end
end;

function dmd_Copy_List(Src,Dest: PChar;
                       List: PInt64s; Count: Integer): Boolean;
type
  tproc = procedure(src_fn,dst_fn: PChar;
                    List: PInt64s; Count: Integer); stdcall;
var
  dll: THandle; proc: tproc;
begin
  Result:=false; FileErase(Dest);

  dll:=Load_dm_util; if dll >= 32 then begin
    @proc:=GetProcAddress(dll,'dm_Copy_List');
    if Assigned(proc) then begin
      proc(Src,Dest,List,Count);
      Result:=FileExist(Dest)
    end;

    wFreeLibrary(dll)
  end
end;

function dmd_Paste(Dest,Path: Pchar; dlg: Integer): Boolean;
type
  TFunc = function(mapPath,addPath: PChar;
                   dlg: Integer; bin: Boolean): Integer;
  stdcall;
var
  dll: THandle; func: TFunc;
begin
  Result:=false;

  dll:=Load_dm_util;
  if dll >= 32 then begin
    @func:=GetProcAddress(dll,'dm_Add');
    if Assigned(func) then
    Result:=func(Dest,Path,dlg,false) >= 0;
    wFreeLibrary(dll)
  end;
end;
                      *)
begin
  DmwIntfProc:=nil
end.