unit prj_com; interface

uses
  Classes,OTypes,LCLType;

function dllGetInterface(IID: PGUID; var Obj): HResult; stdcall;

function prj_init: Pointer; cdecl;
procedure prj_done(prj: Pointer); cdecl;

function prj_GetMapCount(prj: Pointer): int; cdecl;

function prj_AddMap(prj: Pointer; path: PChar): int; cdecl;

procedure prj_set_proj(prj: Pointer; elp,proj: int; b1,b2,lc: double); cdecl;

procedure prj_GetBound(prj: Pointer; pps: int; bp: pdoubles); cdecl;

function prj_XY_BL(prj: Pointer; x,y: double; out b,l: double): int; cdecl;
function prj_BL_XY(prj: Pointer; b,l: double; out x,y: double): int; cdecl;

function prj_to_tile(prj: Pointer; map: PBitmap; left,top,pix: double; alfa: int): int; cdecl;
function prj_to_tile1(prj: Pointer; map: PBitmap; left,top,pixx,pixy: double; alfa: int): int; cdecl;
function prj_bmp_SaveAs(prj: Pointer; Dest: PChar; map: PBitmap): int; cdecl;

function jprj_XY_BL(prj: Pointer; x,y: double; bl: pdoubles): int; cdecl;
function jprj_BL_XY(prj: Pointer; b,l: double; xy: pdoubles): int; cdecl;

function jprj_to_tile(prj: Pointer;
                      tw,th,tbits,tline: int; buf: Pointer;
                      left,top,pix: double; alfa: int): int; cdecl;

function jprj_to_tile1(prj: Pointer;
                       tw,th,tbits,tline: int; buf: Pointer;
                       left,top,pixx,pixy: double; alfa: int): int; cdecl;

function jprj_bmp_SaveAs(prj: Pointer; Dest: PChar;
                         tw,th,tbits,tline: int; buf: Pointer): int; cdecl;

procedure prj_SetBinDir(Path: PChar); cdecl;

implementation

uses
  SysUtils,

  ofiles,
  dmw_prj,
  xbl_use,
  idmw,
  xdib32,
  xddw,
  xbmp,
  dm_draw,
  xlog;

type
  tdmwProject = class(TInterfacedObject,idmwProject)
    constructor Create;
    destructor Destroy; override;

    procedure Clear; stdcall;

    function LoadFrom(path: PChar): int; stdcall;

    function SaveAs(path: PChar): PChar; stdcall;

    function GetMapCount: int; stdcall;

    function GetActiveMapIndex: int; stdcall;

    function AddMap(path: PChar): int; stdcall;

    function DeleteMap(fname: PChar): int; stdcall;

    function GetActiveMap(path: PChar): int; stdcall;
    function SetActiveMap(path: PChar): int; stdcall;

    function GetMapPath(i: int; path: PChar): PChar; stdcall;

    function Contains_Map(path: PChar): int; stdcall;

    procedure set_proj(elp,prj: int; b1,b2,lc: double); stdcall;

    procedure GetBound(pps: int; bp: pdoubles); stdcall;

    function XY_BL(x,y: double; out b,l: double): int; stdcall;
    function BL_XY(b,l: double; out x,y: double): int; stdcall;

    function to_tile(map: PBitmap; left,top,pix: double; alfa: int): int; stdcall;
    function to_tile1(map: PBitmap; left,top,pixx,pixy: double; alfa: int): int; stdcall;

    function bmp_saveAs(Dest: PChar; map: PBitmap): int; stdcall;

    procedure SetBinDir(Path: PChar); stdcall;

  private
    fMaps: TMapProject;
    fcv: XBitmap32;
  end;

constructor tdmwProject.Create;
begin
  inherited Create;
  fMaps:=TMapProject.Create;
  fcv:=XBitmap32.Create;

  std_log.WriteStr('dmwProject.Create');
end;

destructor tdmwProject.Destroy;
begin
  std_log.WriteStr('dmwProject.Destroy');

  fcv.Free;
  fMaps.Free;
  inherited
end;

procedure tdmwProject.Clear;
begin
  std_log.WriteStr('prj_Clear');
  fMaps.Reset_Project(false)
end;

function tdmwProject.LoadFrom(path: PChar): int;
begin
  if Assigned(path) then
    fMaps.LoadFrom(path,true)
  else
    fMaps.Reset_Project(false);

  fMaps.Map.Close_Map;

  Result:=fMaps.Count;
end;

function tdmwProject.SaveAs(path: PChar): PChar;
begin
  Result:=fMaps.SaveAs(Path,true)
end;

function tdmwProject.GetMapCount: int;
begin
  std_log.WriteStr(Format('GetMapCount=%d',[fMaps.Count]));
  Result:=fMaps.Count
end;

function tdmwProject.GetActiveMapIndex: int;
begin
  Result:=fMaps.ActiveMap
end;

function tdmwProject.AddMap(path: PChar): int;
begin
  Result:=-1;
  if fMaps.xInsert_Map(path) then begin
    std_log.WriteStr('prj_AddMap: '+xStrNameExt(path));
    fMaps.auto_Resize; Result:=fMaps.Count
  end;
end;

function tdmwProject.DeleteMap(fname: PChar): int;
var
  i: int;
begin
  Result:=-1; i:=fMaps.Contains_Map(fname);
  if i >= 0 then if fMaps.Delete_Map(i) then
  Result:=i
end;

function tdmwProject.GetActiveMap(path: PChar): int;
var
  i: int;
begin
  Result:=-1; i:=fMaps.ActiveMap;
  if fMaps.Get_dm_Path(i,path) then
  Result:=i
end;

function tdmwProject.SetActiveMap(path: PChar): int;
var
  i: int;
begin
  Result:=-1;
  i:=fMaps.Contains_Map(path);

  if i >= 0 then with fMaps do begin
    if i <> ActiveMap then ActiveMap:=i;
    if i = ActiveMap then Result:=i
  end
end;

function tdmwProject.GetMapPath(i: int; path: PChar): PChar;
begin
  Result:=nil;
  if fMaps.Get_dm_Path(i,path) then
  Result:=path
end;

function tdmwProject.Contains_Map(path: PChar): int;
begin
  Result:=fMaps.Contains_Map(path)
end;

procedure tdmwProject.set_proj(elp,prj: int; b1,b2,lc: double);
var
  sys: tsys; s: String;
begin
  s:=Format('set_proj: %d %d %0.5f %0.5f %0.5f',[elp,prj, b1,b2,lc]);
  std_log.WriteStr(s);

  sys:=sys7(1,prj,elp, b1,b2,lc);
  fMaps.prj_Resize(sys);
end;

procedure tdmwProject.GetBound(pps: int; bp: pdoubles);
var
  lt,rb: TGauss; s: String;
begin
  lt:=fMaps.px_lt;
  rb:=fMaps.px_rb;

  if pps = 1 then begin
    fMaps.Get_Bound(lt,rb);
  end;

  s:=Format('prj_GetBound: %0.3f %0.3f %0.3f %0.3f',[lt.x,lt.y,rb.x,rb.y]);
  std_log.WriteStr(s);

  bp[0]:=lt.x; bp[1]:=lt.y;
  bp[2]:=rb.x; bp[3]:=rb.y;
end;

function tdmwProject.XY_BL(x,y: double; out b,l: double): int;
begin
  Result:=0; b:=x; l:=y;
  with fMaps do if p_lg.s.pps = 1 then begin
    sys_XY_BL(x,y, p_lg.s, b,l); Result:=1
  end
end;

function tdmwProject.BL_XY(b,l: double; out x,y: double): int;
begin
  Result:=1; x:=b; y:=l;
  with fMaps do if p_lg.s.pps = 1 then begin
    sys_BL_XY(b,l, p_lg.s, x,y); Result:=0
  end
end;

function tdmwProject.to_tile(map: PBitmap; left,top,pix: double; alfa: int): int;
begin
  Result:=to_tile1(map, left,top,pix,pix, alfa)
end;

function tdmwProject.to_tile1(map: PBitmap; left,top,pixx,pixy: double; alfa: int): int;
var
  draw: TDrawTile; w,h: int; map1: Bitmap; s: String;
begin
  Result:=0;

  w:=map.bmWidth;
  h:=map.bmHeight;

  if fMaps.Count <= 0 then
    Result:=-1
  else
  if w*h <= 0 then
    Result:=-2
  else
  if not (map.bmBitsPixel in [24,32]) then
    Result:=-3
  else
  if not fcv.Alloc_dc(w,h,0) then
    Result:=-4
  else begin
    draw:=TDrawTile.Create(fMaps);
    try
      draw.Draw(fcv, left,top,pixx,pixy);
      if fcv.GetMap(map1) then
      if map32_to_map(map1,map^,dib_Color(15)) then
      Result:=1
    finally
      draw.Free;
    end;
  end;

  s:=Format('prj_tile=%d %0.3f %0.3f %0.3f %0.3f',
    [Result,left,top,pixx,pixy]);

  std_log.WriteStr(s);
end;

function tdmwProject.bmp_saveAs(Dest: PChar; map: PBitmap): int;
begin
  Result:=0;

  if This_ext(Dest,'.png') then begin
    if BitmapAsPng(map^,Dest) then
    Result:=1
  end
  else begin
    if BitmapAsBmp(map^,Dest) then
    Result:=1
  end
end;

procedure tdmwProject.SetBinDir(Path: PChar);
begin
  SetBinDir(Path);
end;

function dllGetInterface(IID: PGUID; var Obj): HResult;
var
  prj: tdmwProject;
begin
  Result:=S_FALSE; OTypes.TPointer(Obj):=0;

  if Assigned(IID) then
  if IsEqualGUID(IID^,idmwProject) then
  IID:=nil;

  if IID = nil then begin
    prj:=tdmwProject.Create;
    try
      if prj.GetInterface(idmwProject,Obj) then begin
        prj:=nil; Result:=S_OK
      end;
    finally
      prj.Free
    end;
  end
end;

function prj_init: Pointer;
var
  prj: tdmwProject;
begin
  Result:=nil;

  prj:=tdmwProject.Create;
  try
    Result:=prj; prj:=nil;
  finally
    prj.Free
  end;
end;

procedure prj_done(prj: Pointer);
begin
  TObject(prj).Free;
end;

function prj_GetMapCount(prj: Pointer): int; cdecl;
begin
  Result:=tdmwProject(prj).GetMapCount;
end;

function prj_AddMap(prj: Pointer; path: PChar): int;
begin
  Result:=tdmwProject(prj).AddMap(path);
end;

procedure prj_set_proj(prj: Pointer; elp,proj: int; b1,b2,lc: double);
begin
  tdmwProject(prj).set_proj(elp,proj, b1,b2,lc);
end;

procedure prj_GetBound(prj: Pointer; pps: int; bp: pdoubles);
begin
  tdmwProject(prj).GetBound(pps, bp);
end;

function prj_XY_BL(prj: Pointer; x,y: double; out b,l: double): int;
begin
  Result:=tdmwProject(prj).XY_BL(x,y, b,l);
end;

function prj_BL_XY(prj: Pointer; b,l: double; out x,y: double): int;
begin
  Result:=tdmwProject(prj).BL_XY(b,l, x,y);
end;

function prj_to_tile(prj: Pointer; map: PBitmap; left,top,pix: double; alfa: int): int;
begin
  Result:=tdmwProject(prj).to_tile(map, left,top,pix, alfa);
end;

function prj_to_tile1(prj: Pointer; map: PBitmap; left,top,pixx,pixy: double; alfa: int): int;
begin
  Result:=tdmwProject(prj).to_tile1(map, left,top,pixx,pixy, alfa);
end;

function prj_bmp_saveAs(prj: Pointer; Dest: PChar; map: PBitmap): int;
begin
  Result:=tdmwProject(prj).bmp_saveAs(Dest,map);
end;

procedure prj_SetBinDir(Path: PChar);
begin
  SetBinDir(Path);
  std_log.Write_str2('BinDir',BinDir);
end;

function jprj_XY_BL(prj: Pointer; x,y: double; bl: pdoubles): int;
begin
  Result:=tdmwProject(prj).XY_BL(x,y, bl[0],bl[1]);
end;

function jprj_BL_XY(prj: Pointer; b,l: double; xy: pdoubles): int;
begin
  Result:=tdmwProject(prj).BL_XY(b,l, xy[0],xy[1]);
end;

function jprj_to_tile(prj: Pointer;
                      tw,th,tbits,tline: int; buf: Pointer;
                      left,top,pix: double; alfa: int): int;
var
  map: Bitmap;
begin
  map:=__Bitmap(tw,th,tbits,tline,buf);
  Result:=tdmwProject(prj).to_tile(@map, left,top,pix, alfa);
end;

function jprj_to_tile1(prj: Pointer;
                       tw,th,tbits,tline: int; buf: Pointer;
                       left,top,pixx,pixy: double; alfa: int): int; cdecl;
var
  map: Bitmap;
begin
  map:=__Bitmap(tw,th,tbits,tline,buf);
  Result:=tdmwProject(prj).to_tile1(@map, left,top,pixx,pixy, alfa);
end;

function jprj_bmp_SaveAs(prj: Pointer; Dest: PChar;
                         tw,th,tbits,tline: int; buf: Pointer): int;
var
  map: Bitmap;
begin
  map:=__Bitmap(tw,th,tbits,tline,buf);
  Result:=tdmwProject(prj).bmp_saveAs(Dest,@map);
end;

end.

