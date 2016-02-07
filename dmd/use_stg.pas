unit use_stg; interface

uses
  Classes,otypes;
                          (*
function dlg_Choose_dir(Dir,Def,Capt: PChar): PChar;
function dlg_Open_dir(Dir,Def,Capt: PChar): PChar;
                            *)
function dm_Dial_Gauss(var g: TGauss): Boolean;
function dm_dial_xy1(v: PDoubles): Boolean;

function dm_Dial_Geoid(var b,l: double;
                       const sys: tsys): Boolean;
                              (*
function dm_Dial_elp(var s: tsys): Boolean;
function dm_Dial_geo(var s: tsys): Boolean;
function dm_Dial_prj(var s: tsys): Boolean;

procedure dm_dlg_About(Capt,Product,Version,
                       Company,Phone,Bitmap: PChar);

function dm_Enter_str(dst,src,capt: PChar): Boolean;

function dm_dial_Scale(sc,min,win,max: double;
                       lev: Integer; capt: PChar): double;

function dm_dial_ra(buf: PGPoly; N: Integer;
                    en_abs: Boolean; var is_abs: Boolean;
                    out Ed: Integer): Integer;

function dm_Dial_Size(def_w,def_h: double;
                      out w,h: double): Boolean;

function dm_Dial_Size1(def_w,def_h: double;
                       out w,h: double): int;
                       
procedure Set_val_font_size(v: Integer);
                      
function dial_int(def: Integer; out val: Integer;
                  capt,msg: PChar): Boolean;

function dial_Real(def: double; out val: Double;
                   capt,msg: PChar): Boolean;

function dial_time(var t: Double; Flags: Integer;
                   capt,msg: PChar): Boolean;
                   
function dm_Dial_Ort(Dir: Integer; var R: Double;
                     lp: PGPoly; lp_n: Integer;
                     out Ed: Integer): Integer;

function stg_dial_List(Items,Hints: TStrings;
                       Def: Integer; Capt: PChar): Integer;

function stg_dial_CList(Dest: PIntegers; Def: Integer;
                        Items,Hints, Capt,Lang: PChar): Integer;

function stg_dlg_Color(var cl: TColorRef): Boolean;

function dm_Dial_Hot(Actions: PHotActions;
                     Msg: PChar; Id: Integer;
                     OnDefault: TNotifyEvent): Boolean;

function stg_mdb_enum(Value,Mdb,Dbf,Range,Capt: PChar;
                      RangeSelect: Boolean): PChar;

function stg_mdb_code(var Code: int;
                      Mdb,Dbf,Range,Capt: PChar;
                      fld: int): Boolean;

procedure dialAdoConString;
                              *)
implementation

uses
  dynlibs,
  ofiles,
  use_util;

function Load_dlg_stg: THandle;
begin
  Result:=Load_dm_dll('dlg_stg.dll')
end;

function Load_dlg_stg_proc(Name: PChar; out proc: Pointer): THandle;
begin
  proc:=nil; Result:=Load_dlg_stg;
  if Result >= 32 then begin
    proc:=GetProcAddress(Result,Name);
    if proc = nil then wFreeLibrary(Result)
  end
end;
    (*
function dlg_dir(Name,Dir,Def,Capt: PChar): PChar;
type
  tdlg = function(Dir,Def,Capt: PChar): PChar; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=nil;

  dll:=Load_dlg_stg; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,Name);
    if Assigned(dlg) then
    Result:=dlg(Dir,Def,Capt);
    wFreeLibrary(dll)
  end
end;

function dlg_Choose_dir(Dir,Def,Capt: PChar): PChar;
begin
  Result:=dlg_dir('Choose_dir',Dir,Def,Capt)
end;

function dlg_Open_dir(Dir,Def,Capt: PChar): PChar;
begin
  Result:=dlg_dir('Open_dir',Dir,Def,Capt)
end;

procedure dm_dlg_About(Capt,Product,Version,
                       Company,Phone,Bitmap: PChar);
type
  tdlg = procedure(Capt,Product,Version,
                   Company,Phone,Bitmap: PChar); stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  dll:=Load_dlg_stg; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dlg_About');
    dlg(Capt,Product,Version,Company,Phone,Bitmap);
    wFreeLibrary(dll)
  end
end;

function dm_Enter_str(dst,src,capt: PChar): Boolean;
type
  tdlg = function(dst,src,capt: PChar): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_stg; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'Enter_str');
    Result:=dlg(dst,src,capt); wFreeLibrary(dll)
  end
end;
                  *)
function dm_Dial_Gauss(var g: tgauss): Boolean;
type
  tdlg = function(var g: tgauss): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_stg; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'Dial_Gauss');
    Result:=dlg(g); wFreeLibrary(dll)
  end
end;

function dm_dial_xy1(v: PDoubles): Boolean;
type
  tdlg = function(v: PDoubles): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_stg; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dial_xy1');
    Result:=dlg(v); wFreeLibrary(dll)
  end
end;

function dm_Dial_Geoid(var b,l: double;
                       const sys: tsys): Boolean;
type
  tdlg = function(var b,l: double; sys: psys): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_stg; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'Dial_Geoid');

    Result:=dlg(b,l, @sys);

    wFreeLibrary(dll)
  end

end;
                      (*
function dm_Dial_elp(var s: tsys): Boolean;
type
  tdlg = function(var elp: Integer;
                  var geo: TDatum7): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_stg;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'Dial_elp');
    with s do Result:=dlg(elp,dat);
    wFreeLibrary(dll)
  end
end;

function dm_Dial_geo(var s: tsys): Boolean;
type
  tdlg = function(var s: tsys): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_stg;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'Dial_geo');
    Result:=dlg(s); wFreeLibrary(dll)
  end
end;

function dm_Dial_prj(var s: tsys): Boolean;
type
  tdlg = function(var s: tsys): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_stg;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'Dial_prj');
    Result:=dlg(s); wFreeLibrary(dll)
  end
end;

function dm_dial_Scale(sc,min,win,max: double;
                       lev: Integer; capt: PChar): double;
type
  tdlg = function(sc,min,win,max: double;
                  lev: Integer; capt: PChar): double;
  stdcall;

var
  dll: THandle; dlg: tdlg;
begin
  Result:=-1;

  dll:=Load_dlg_stg;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'Dial_Scale');
    if Assigned(dlg) then
    Result:=dlg(sc,min,win,max, lev,capt);
    wFreeLibrary(dll)
  end
end;
       *)
var
  rr_ed: Integer;
         (*
function dm_dial_ra(buf: PGPoly; N: Integer;
                    en_abs: Boolean; var is_abs: Boolean;
                    out Ed: Integer): Integer;
type
  tdlg = function(buf: PGPoly; N: Integer;
                  en_abs: Boolean; var is_abs: Boolean;
                  var Ed: Integer): Integer; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=-1; Ed:=0;

  dll:=Load_dlg_stg;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'Dial_ra');
    if Assigned(dlg) then
    Result:=dlg(buf,N, en_abs,is_abs,rr_ed);
    wFreeLibrary(dll);
  end;

  Ed:=rr_ed;
end;

function dm_Dial_Size(def_w,def_h: double;
                      out w,h: double): Boolean;
type
  tdlg = function(def_w,def_h: double;
                  out w,h: double; Capt: PChar): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_stg;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'Dial_Size');
    if Assigned(dlg) then
    Result:=dlg(def_w,def_h,w,h,nil);
    wFreeLibrary(dll)
  end
end;

function dm_Dial_Size1(def_w,def_h: double;
                       out w,h: double): int;
type
  tdlg = function(def_w,def_h: double;
                  out w,h: double; Capt: PChar): int; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=-1;

  dll:=Load_dlg_stg;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'Dial_Size1');
    if Assigned(dlg) then
    Result:=dlg(def_w,def_h,w,h,nil);
    wFreeLibrary(dll)
  end
end;

procedure Set_val_font_size(v: Integer);
type
  tproc = procedure(v: Integer); stdcall;
var
  dll: THandle; proc: tproc;
begin
  dll:=Load_dlg_stg;
  if dll >= 32 then begin
    @proc:=GetProcAddress(dll,'Set_val_font_size');
    if Assigned(proc) then proc(v);
    wFreeLibrary(dll)
  end
end;

function dial_int(def: Integer; out val: Integer;
                  capt,msg: PChar): Boolean;
type
  tdlg = function(def: Integer; out val: Integer;
                  capt,msg: PChar): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_stg;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dial_int');
    if Assigned(dlg) then
    Result:=dlg(def,val,Capt,Msg);
    wFreeLibrary(dll)
  end
end;

function dial_Real(def: double; out val: Double;
                   Capt,Msg: PChar): Boolean;
type
  tdlg = function(def: double; out val: Double;
                  Capt,Msg: pChar): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_stg;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'Dial_Real');
    if Assigned(dlg) then
    Result:=dlg(def,val,Capt,Msg);
    wFreeLibrary(dll)
  end
end;

function dial_time(var t: Double; Flags: Integer;
                   capt,msg: PChar): Boolean;
type
  tdlg = function(var t: Double; Flags: Integer;
                  capt,msg: PChar): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_stg;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dial_time');
    if Assigned(dlg) then
    Result:=dlg(t,Flags,capt,msg);
    wFreeLibrary(dll)
  end
end;

function dm_Dial_Ort(Dir: Integer; var R: Double;
                     lp: PGPoly; lp_n: Integer;
                     out Ed: Integer): Integer;
type
  tdlg = function(Dir: Integer; var R: Double;
                  lp: PGPoly; lp_n: Integer;
                  var Ed: Integer): Integer;
         stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=-1; Ed:=0;
  if rr_ed = 4 then Ed:=1;

  dll:=Load_dlg_stg;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'Dial_Ort');
    if Assigned(dlg) then
    Result:=dlg(Dir,R,lp,lp_n,Ed);
    wFreeLibrary(dll)
  end;

  if Ed = 1 then Ed:=4
end;

function stg_dial_List(Items,Hints: TStrings;
                       Def: Integer; Capt: PChar): Integer;
type
  tdlg = function(Items,Hints: TStrings;
                  Def: Integer; Capt: PChar): Integer;
stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=-1;

  dll:=Load_dlg_stg; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dial_List');
    Result:=dlg(Items,Hints, Def,Capt);
    wFreeLibrary(dll)
  end
end;

function stg_dial_CList(Dest: PIntegers; Def: Integer;
                        Items,Hints, Capt,Lang: PChar): Integer;
type
  tfunc = function(Dest: PIntegers; Def: Integer;
                   Items,Hints, Capt,Lang: PChar): Integer; stdcall;
var
  dll: THandle; dlg: tfunc;
begin
  Result:=0;

  dll:=Load_dlg_stg; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dial_CList');
    Result:=dlg(Dest,Def, Items,Hints, Capt,Lang);
    wFreeLibrary(dll)
  end
end;

function stg_dlg_Color(var cl: TColorRef): Boolean;
type
  tdlg = function(var cl: TColorRef): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_stg; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dlg_win_Color');
    Result:=dlg(cl); wFreeLibrary(dll)
  end
end;

function dm_Dial_Hot(Actions: PHotActions;
                     Msg: PChar; Id: Integer;
                     OnDefault: TNotifyEvent): Boolean;
type
  tdlg = function(Actions: PHotActions;
                  Msg: PChar; Id: Integer;
                  OnDefault: TNotifyEvent): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_stg; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'Dial_Hot');
    Result:=dlg(Actions,Msg,Id,OnDefault);
    wFreeLibrary(dll)
  end

end;

function stg_mdb_enum(Value,Mdb,Dbf,Range,Capt: PChar;
                      RangeSelect: Boolean): PChar;
type
  tdlg = function(Value,Mdb,Dbf,Capt: PChar;
                  RangeSelect: Boolean): PChar; stdcall;

  tdlg1 = function(Value,Mdb,Dbf,Range,Capt: PChar;
                   RangeSelect: Boolean): PChar; stdcall;
var
  dll: THandle; dlg: tdlg; dlg1: tdlg1;
begin
  Result:=nil;
  dll:=Load_dlg_stg; if dll >= 32 then begin

    @dlg1:=GetProcAddress(dll,'Dial_mdb_enum1');

    if Assigned(dlg1) then
      Result:=dlg1(Value,Mdb,Dbf,Range,Capt, RangeSelect)
    else begin
      @dlg:=GetProcAddress(dll,'Dial_mdb_enum');
      if Assigned(dlg) then
      Result:=dlg(Value,Mdb,Dbf,Capt, RangeSelect)
    end;

    wFreeLibrary(dll)
  end
end;

function stg_mdb_code(var Code: int;
                      Mdb,Dbf,Range,Capt: PChar;
                      fld: int): Boolean;
type
  tdlg = function(var Code: int;
                  Mdb,Dbf,Capt: PChar;
                  fld: int): Boolean; stdcall;

  tdlg1 = function(var Code: int;
                   Mdb,Dbf,Range,Capt: PChar;
                   fld: int): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg; dlg1: tdlg1;
begin
  Result:=false;
  dll:=Load_dlg_stg; if dll >= 32 then begin

    @dlg1:=GetProcAddress(dll,'Dial_mdb_code1');
    if Assigned(dlg1) then
      Result:=dlg1(Code,Mdb,Dbf,Range,Capt,fld)
    else begin
      @dlg:=GetProcAddress(dll,'Dial_mdb_code');
      if Assigned(dlg) then
      Result:=dlg(Code,Mdb,Dbf,Capt,fld);
    end;

    wFreeLibrary(dll)
  end
end;

procedure dialAdoConString;
type
  tdlg = procedure; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  dll:=Load_dlg_stg; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dialAdoConString');
    if Assigned(dlg) then dlg; wFreeLibrary(dll)
  end
end;
     *)
begin
  rr_ed:=0
end.