unit xproj; interface

uses
  LCLType,otypes,
  xintf,xline,pmath;

type
  IProject2 = interface(IUnknown)
    ['{31F58C49-CF91-421E-89F8-3DBEE31F5ACB}']
    function Project(x,y: Double): TGauss; stdcall;
    function Backup(x,y: Double): TGauss; stdcall;

    function Get_d2(out tr,bt: Real3x3): HResult; stdcall;
    function Get_poly(out tr,bt: TPolynom): HResult; stdcall;

    function Push: HResult; stdcall;
    procedure Pop; stdcall;

    procedure MoveTo(dx,dy: Double); stdcall;
    procedure Scale(kz: Double); stdcall;
  end;

  THttpCallback = procedure(Obj: Pointer; key,path: PChar); stdcall;

  IHttpGet = interface(IUnknown)
    ['{C7C6D0AB-FBC7-4FDF-A88A-C3205C4B595D}']
    procedure SaveToFile(key,url,dest: PChar); stdcall;

    function GetReceiveCount: int; stdcall;
    function GetWaitCount: int; stdcall;

    procedure Stop; stdcall;

    procedure SetCallback(Obj: Pointer; cb: THttpCallback); stdcall;

    procedure Reset; stdcall;

    function GetReceived: int; stdcall;

    function IsStop: bool; stdcall;
    function IsReady: bool; stdcall;
  end;

  IGoogle = interface(IUnknown)
    ['{796DD18C-0B52-4273-B1F8-CB5CE3828B6C}']

    function GetNLayers: Integer; stdcall;
    function GetLayerCapt(Ind: Integer; Str: PChar): HResult; stdcall;
    function SetLayer(Ind: Integer): Integer; stdcall;

    function GetTileSize: Integer; stdcall;

    function GetNLevels: Integer; stdcall;

    function GetTile(Dest: PChar; X,Y,Z: Integer): PChar; stdcall;

    // World [X,Y] -> Image [X(0..1),Y(0..1)]
    function Project(x,y: Double): TGauss; stdcall;

    // Image [X(0..1),Y(0..1)] -> World [X,Y]
    function Backup(x,y: Double): TGauss; stdcall;

    function GetSys(out sys: tsys): Integer; stdcall;

    function Paint(Frame: PGPoly; mpp: Double): Integer; stdcall;

    function GetDraw(Ind: Integer; Dest: PChar;
                     Frame: PDoubles): HResult; stdcall;

    function SaveAs(Dest: PChar; Frame: PGPoly;
                    lev: int; wait: IProgress): HResult; stdcall;

    procedure SetHttp(const AHttp: IHttpGet); stdcall;

    function GetParam(Key: int): int; stdcall;
    procedure SetParam(Key,Val: int); stdcall;

    function GetParams(Key: int; Val: PIntegers): int; stdcall;

    function GetMapCount: Integer; stdcall;
    function GetMapName(Ind: int; Name: PChar): Integer; stdcall;
    function GetMapCapt(Ind: int; Capt: PChar): Integer; stdcall;

    function GetSklCount: Integer; stdcall;
    function GetSklName(Ind: int; Name: PChar): Integer; stdcall;
    function GetSklCapt(Ind: int; Capt: PChar): Integer; stdcall;

    function SelectMap(Id: int): Integer; stdcall;
    function SelectSkl(Id,Up: int): Integer; stdcall;

    function Upload(Frame: PGPoly; Z: int): int; stdcall;

    function SelectChart(mapId: int;
                         sklId: PIntegers;
                         sklN: int): int; stdcall;
  end;

  IKmlImage = interface(IUnknown)
    ['{EEC56ED0-7159-4D86-B58F-C1C158951933}']
    function GetTileSize: Integer; stdcall;

    procedure GetBound(Frame: PGPoly); stdcall;

    function Paint(Frame: PGPoly): Integer; stdcall;

    function GetDraw(Ind: Integer; Dest: PChar;
                     Frame: PGPoly): HResult; stdcall;

    function SaveAs(Dest: PChar; Frame: PGPoly;
                    wait: IProgress): HResult; stdcall;
  end;

  ISputnik3D = interface(IUnknown)
    ['{BCB9C058-32B9-4042-80A3-ADB0A45A73B2}']
    procedure Inspect(lat,lon,alt,rad: double); stdcall;
    function Stop: HResult; stdcall;

    function GetEye(eye,pos: pxyz): HResult; stdcall;
  end;

  IRotateImage = interface(IUnknown)
    ['{C60701D5-4A55-49A0-97AD-604A1904FABC}']
    procedure Init(Thread: Boolean); stdcall;
    procedure Done; stdcall;

    procedure SetActive(Value: Boolean); stdcall;
    function GetActive: Boolean; stdcall;

    procedure SetFlags(Value: int); stdcall;

    procedure SetProj(Mat: PDoubles); stdcall;
    function Exec(map_in,map_out: PBitmap): Boolean; stdcall;
  end;

  TProject2d = class(TInterfacedObject,IProject2)
    constructor Create;
    function Project(x,y: Double): TGauss; stdcall;
    function Backup(x,y: Double): TGauss; stdcall;

    function Get_d2(out tr,bt: Real3x3): HResult; stdcall;
    function Get_poly(out tr,bt: TPolynom): HResult; stdcall;

    function Push: HResult; stdcall;
    procedure Pop; stdcall;

    procedure MoveTo(dx,dy: Double); stdcall;
    procedure Scale(kz: Double); stdcall;

    function Assign(Atr: Real3x3): Boolean;
    function Solve(p,q: PGPoly): Boolean;
  private
    ftr,fbt: Real3x3;
    str: Array[0..7] of Real3x3;
    sbt: Array[0..7] of Real3x3;
    sp: Integer;
  end;

  TProject2p = class(TInterfacedObject,IProject2)
    constructor Create(const AProj: IProject2);
    function Project(x,y: Double): TGauss; stdcall;
    function Backup(x,y: Double): TGauss; stdcall;

    function Get_d2(out tr,bt: Real3x3): HResult; stdcall;
    function Get_poly(out tr,bt: TPolynom): HResult; stdcall;

    function Push: HResult; stdcall;
    procedure Pop; stdcall;

    procedure MoveTo(dx,dy: Double); stdcall;
    procedure Scale(kz: Double); stdcall;
  private
    fProj: IProject2;
    fdX,fdY,fScale: Double;
  end;

  TKmlProject = class(TInterfacedObject,IProject2)

    constructor Create(const AProj: IProject2;
                       Frame: PGPoly; tile: int);

    function Project(x,y: Double): TGauss; stdcall;
    function Backup(x,y: Double): TGauss; stdcall;

    function Get_d2(out tr,bt: Real3x3): HResult; stdcall;
    function Get_poly(out tr,bt: TPolynom): HResult; stdcall;

    function Push: HResult; stdcall;
    procedure Pop; stdcall;

    procedure MoveTo(dx,dy: Double); stdcall;
    procedure Scale(kz: Double); stdcall;
  private
    fProj: IProject2;
    ftr,fbt: Real3x3;
    fScale: Double;
  end;

function Get_proj2d(const Atr,Abt: Real3x3; var Obj): HResult;
function Solve_proj2d(p,q: PGPoly; var Obj): HResult;

function Get_proj2p(const AProj: IProject2; var Obj): HResult;

function GetKmlProject(const Proj: IProject2;
                       Frame: PGPoly; tile: int;
                       out Obj): HResult;

function DllKmlInterface(Path: PChar; out Obj): HResult;

function DllSputnikInterface(out Obj): HResult;

implementation

uses
  dynlibs,
  ofiles,xmath;

constructor TProject2d.Create;
begin
  inherited Create;
  ftr:=Identify_3x3;
  fbt:=Identify_3x3;
  sp:=0
end;

function TProject2d.Project(x,y: Double): TGauss;
begin
  Result:=Projective_3x3(x,y,ftr)
end;

function TProject2d.Backup(x,y: Double): TGauss;
begin
  Result:=Projective_3x3(x,y,fbt);
end;

function TProject2d.Get_d2(out tr,bt: Real3x3): HResult;
begin
  tr:=ftr; bt:=fbt;
  Result:=S_OK
end;

function TProject2d.Get_poly(out tr,bt: TPolynom): HResult;
begin
  Result:=S_FALSE
end;

function TProject2d.Push: HResult;
begin
  Result:=S_FALSE;
  if sp < 8 then begin
    str[sp]:=ftr; sbt[sp]:=fbt;
    Inc(sp); Result:=S_OK
  end
end;

procedure TProject2d.Pop;
begin
  if sp > 0 then begin Dec(sp);
    ftr:=str[sp]; fbt:=sbt[sp];
  end
end;

procedure TProject2d.MoveTo(dx,dy: Double);
var
  tr: Real3x3;
begin
  Init_3x3(tr,dx,dy,1,1);
  Forw_3x3(tr,ftr); ftr:=tr;

  t_Move_3x3(fbt,-dx,-dy);
  norm_3x3(fbt);
end;

procedure TProject2d.Scale(kz: Double);
var
  tr: Real3x3;
begin
  Init_3x3(tr,0,0,kz,kz);
  Forw_3x3(tr,ftr); ftr:=tr;

  t_Scale_3x3(fbt,1/kz);
  norm_3x3(fbt);
end;

function TProject2d.Assign(Atr: Real3x3): Boolean;
begin
  ftr:=Atr;
  Result:=Inverse_3x3(ftr,fbt)
end;

function TProject2d.Solve(p,q: PGPoly): Boolean;
begin
  Result:=Solve_projective1(p,q,ftr,fbt)
end;

constructor TProject2p.Create(const AProj: IProject2);
begin
  inherited Create;
  fProj:=AProj; fScale:=1
end;

function TProject2p.Project(x,y: Double): TGauss;
begin
  Result:=fProj.Project((x+fdX)*fScale,
                        (y+fdY)*fScale)
end;

function TProject2p.Backup(x,y: Double): TGauss;
var
  t: TGauss;
begin
  t:=fProj.Backup(x,y);
  Result.x:=t.x/fScale - fdX;
  Result.y:=t.y/fScale - fdY;
end;

function TProject2p.Get_d2(out tr,bt: Real3x3): HResult;
begin
  Result:=S_FALSE
end;

function TProject2p.Get_poly(out tr,bt: TPolynom): HResult;
begin
  Result:=S_FALSE
end;

function TProject2p.Push: HResult;
begin
  Result:=S_FALSE
end;

procedure TProject2p.Pop;
begin
end;

procedure TProject2p.MoveTo(dx,dy: Double);
begin
  fdX:=fdX + dx; fdY:=fdY + dy;
end;

procedure TProject2p.Scale(kz: Double);
begin
  fScale:=kz
end;

function Get_proj2d(const Atr,Abt: Real3x3; var Obj): HResult;
var
  proj: TProject2d;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  proj:=TProject2d.Create;
  try
    proj.ftr:=Atr;
    proj.fbt:=Abt;

    if proj.GetInterface(IProject2,OBJ) then
    begin proj:=nil; Result:=S_OK end
  finally
    proj.Free
  end
end;

function Solve_proj2d(p,q: PGPoly; var Obj): HResult;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  with TProject2d.Create do
  if Solve(p,q) then

  if GetInterface(IProject2,OBJ) then
  Result:=S_OK
end;

function Get_proj2p(const AProj: IProject2; var Obj): HResult;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  with TProject2p.Create(AProj) do
  if GetInterface(IProject2,OBJ) then
  Result:=S_OK
end;

constructor TKmlProject.Create(const AProj: IProject2;
                               Frame: PGPoly; tile: int);
var
  kx,ky: Double; a,b: TGauss;
begin
  inherited Create;
  fProj:=AProj; fScale:=1;

  a:=Frame[0]; b:=Frame[1];

  kx:=tile/(b.x-a.x);
  ky:=tile/(b.y-a.y);

  ftr:=nil_3x3;
  ftr[1,2]:=+ky; ftr[1,3]:=-a.y*ky;
  ftr[2,1]:=-kx; ftr[2,3]:=b.x*kx;
  ftr[3,3]:=1;

  fbt:=nil_3x3;
  fbt[1,2]:=-1/kx; fbt[1,3]:=b.x;
  fbt[2,1]:=+1/ky; fbt[2,3]:=a.y;
  fbt[3,3]:=1;
end;

function TKmlProject.Project(x,y: Double): TGauss;
var
  g: TGauss;
begin
  g:=Transit_3x3(x*fScale,y*fScale,fbt);
  g:=fProj.Project(g.x,g.y);
  Result:=g
end;

function TKmlProject.Backup(x,y: Double): TGauss;
var
  g: TGauss;
begin
  g:=fProj.Backup(x,y);
  g:=Transit_3x3(g.x,g.y,ftr);
  Result.x:=g.x/fScale;
  Result.y:=g.y/fScale
end;

function TKmlProject.Get_d2(out tr,bt: Real3x3): HResult;
begin
  Result:=S_FALSE
end;

function TKmlProject.Get_poly(out tr,bt: TPolynom): HResult;
begin
  Result:=S_FALSE
end;

function TKmlProject.Push: HResult;
begin
  Result:=S_FALSE
end;

procedure TKmlProject.Pop;
begin
end;

procedure TKmlProject.MoveTo(dx,dy: Double);
begin
end;

procedure TKmlProject.Scale(kz: Double);
begin
  fScale:=fScale*kz
end;

function GetKmlProject(const Proj: IProject2;
                       Frame: PGPoly; tile: int;
                       out Obj): HResult;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  with TKmlProject.Create(Proj,Frame,tile) do
  if GetInterface(IProject2,OBJ) then
  Result:=S_OK
end;

var
  dll_kml: HModule;

function DllKmlInterface(Path: PChar; out Obj): HResult;
type
  tfunc = function(Path: PChar; out Obj): HResult; stdcall;
var
  func: tfunc;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  if dll_kml = 0 then
  dll_kml:=LoadLibrary('dll_kml.dll');

  if dll_kml >= 32 then begin
    @func:=GetProcAddress(dll_kml,'GetKmlInterface');
    if Assigned(func) then Result:=func(Path,Obj);
  end
end;

var
  dll_pipe: HModule;

function DllSputnikInterface(out Obj): HResult;
type
  tfunc = function(out Obj): HResult; stdcall;
var
  func: tfunc;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  if dll_pipe = 0 then
  dll_pipe:=LoadLibrary('dll_pipe.dll');

  if dll_pipe >= 32 then begin
    @func:=GetProcAddress(dll_pipe,'GetSputnikInterface');
    if Assigned(func) then Result:=func(Obj);
  end
end;

procedure Init;
begin
  dll_kml:=0;
  dll_pipe:=0;
end;

procedure Done;
begin
  xFreeLibrary(dll_kml);
  xFreeLibrary(dll_pipe);
end;

initialization Init;
finalization Done;

end.