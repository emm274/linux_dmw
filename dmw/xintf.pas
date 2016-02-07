unit xintf; interface

uses          
  Classes,LCLType,
  otypes,xlist;

const
  flag_visible  = 1;
  flag_readonly = 2;
  flag_enabled  = 4;

type
  TIntfEvent = function(out Obj): Boolean of object;

  INotify = interface(IUnknown)
    ['{E0044AAB-85B4-4715-81AE-3D563E8DB28C}']
    procedure Command(Cmd: Integer); stdcall;
  end;

  IProgress = interface(IUnknown)
    ['{4A6DC49F-875C-4D1B-8790-81C03D0228A7}']
    procedure Init(Capt,Msg: PChar; Count,Brk: Integer); stdcall;
    procedure Repeat_for(Count: Integer; Msg: PChar); stdcall;
    function Step: HResult; stdcall;
    function Break_Process: HResult; stdcall;
    procedure Done; stdcall;
  end;

  IStrings = interface(IUnknown)
    ['{1DEE5036-A795-4313-A1A8-91FDE8B86CC6}']
    function GetCount: Integer; stdcall;
    function GetItem(Ind: Integer; Str: PChar): Integer; stdcall;
    function SetItem(Ind: Integer; Str: PChar): Integer; stdcall;
    function Add(Str: PChar): Integer; stdcall;
    function Indexof(Str: PChar): Integer; stdcall;
  end;

  IStrings1 = interface(IUnknown)
    ['{0D1989EE-4B7B-4674-86B9-72DEAF964A89}']
    function GetCount: Integer; stdcall;

    function GetItem(Ind: Integer;
                     Str: PChar; MaxLen: Cardinal;
                     out Flags: Cardinal): HResult; stdcall;
  end;

  IIntegers1 = interface(IUnknown)
    ['{DC18EFF5-601A-41D9-B909-862087E645AD}']
    function GetCount: Integer; stdcall;
    function GetItem(Ind: Integer): Integer; stdcall;
  end;

  IPointList = interface(IUnknown)
    ['{3516AAF3-9EFF-4821-96F5-535D6D5F32B9}']

    procedure Clear; stdcall;

    function GetCount: Integer; stdcall;
    function GetPoint(Ind: Integer; P: PPoint): Integer; stdcall;
    function SetPoint(Ind: Integer; P: PPoint): Integer; stdcall;
    function AddPoint(Ind,X,Y: Integer): Integer; stdcall;
  end;

  ILog = interface(IUnknown)
    ['{3A4D4B8C-FFC6-4494-908A-59A2399A57B0}']
    procedure Add(Msg: PChar); stdcall;
    procedure Add_time(Msg: PChar; dt: Longint); stdcall;

    procedure Begin_section(Msg,Alt: PChar); stdcall;
    procedure End_section(Msg,Alt: PChar); stdcall;
    procedure Ok_section(Msg: PChar; ok: LongBool); stdcall;

    procedure sAdd(const Msg: ShortString); stdcall;
  end;

  IRelief = interface(IUnknown)
    ['{A8E6DCEF-D7DC-413A-ACB1-74BBED636DA9}']
    function GetValue(g: PGeoPoint; out zv: Double): HResult; stdcall;
  end;

  IRelief1 = interface(IRelief)
    ['{8D7561FF-05D2-4EE1-8288-D125C0759037}']

    function Open(Path: PChar): HResult; stdcall;
    procedure Close; stdcall;

    function GetBound(gl,gb: PGPoly; out sys: tsys): Integer; stdcall;

    function get_mpp: Double; stdcall;

    function Get_NLayers: Integer; stdcall;
    function Set_Layer(Ind: Integer): Integer; stdcall;

    procedure geo_to_xy(g: PGeoPoint; out p: TGauss); stdcall;
    procedure xy_to_geo(p: PGauss; out g: TGeoPoint); stdcall;

    function GetValue1(g: PGauss; out zv: Double): HResult; stdcall;

    function vec_zrange(gp: PGPoly; zp: PDoubles): Integer; stdcall;

    function frame_zrange(gp: PGPoly; zp: PDoubles): HResult; stdcall;

    procedure Draw_xz(DC: HDC;
                      dc_w,dc_h,cl,mode: Integer;
                      gp: PGPoly; Tr: PDoubles); stdcall;
  end;

  IRelief2 = interface(IRelief1)
    ['{2695FED7-CA6D-43C1-AB1C-EF4F5A7FCBC4}']
    function Draw_xz1(DC: HDC;
                      iv: PIntegers;
                      dv: PDoubles;
                      out lp: PLPoly): Integer; stdcall;
  end;

  TTinTrigOp = function(vp: PIntegers): HResult of object;

  ITin = interface(IUnknown)
    ['{D390E980-63D4-4DB0-A065-7E3F42554951}']
    function GetTriCount: Cardinal; stdcall;
    function GetPointCount: Cardinal; stdcall;

    function GetPointi(I: Integer; vp: PIntegers): HResult; stdcall;
    function GetTri(I: Integer; tp: PIntegers): HResult; stdcall;
    function GetTri2i(I: Integer; tp: PIntegers): HResult; stdcall;
    function GetBound(Buf: PLPoly; BufMax: Integer): Integer; stdcall;

    procedure Draw_tin(ATrigOp: TTinTrigOp); stdcall;
  end;

  ITriangulator = interface(IUnknown)
    ['{6730D708-79CB-4575-80F7-9DE76C8D7073}']

    procedure SetParams(Params: PIntegers); stdcall;

    function BeginRegion(Rgn: PGPoly; sys: psys): HResult; stdcall;
    function EndRegion: Integer; stdcall;

    function Clean(baseCount: Integer;
                   dist: Double; distK: Integer): Integer; stdcall;

    function Clip(bp: PLPoly; n: Integer): Integer; stdcall;

    procedure InsNode(v: pxyz); stdcall;

    procedure SaveAs(Dest: PChar); stdcall;
  end;

  ITriangulator2 = interface(IUnknown)
    ['{FC4EFF7D-FE42-4BEB-AB5C-2D4E33513409}']

    function BeginRegion(const lt,rb: TPoint;
                         bp: PVPoly; ndp: int): Boolean; stdcall;

    function EndRegion(pack: int): int; stdcall;

    function GetTriCount: int; stdcall;
    function GetTri(i: int; V: PVPoly): int; stdcall;
    function PickupTri(p: PPoint): int; stdcall;

    function Ins_node(const v: VPoint; it: int): int; stdcall;

    function GetTri2(i: int; tp: PIntegers): int; stdcall;

    procedure print(Dest: PChar); stdcall;
  end;

  IPrint = interface(IUnknown)
    ['{38B6310C-5FB0-420F-9072-57A91A3E9E1A}']

    procedure Draw(ADC: HDC; ABmp: HBitMap;
                   dp: PDoubles; np: Integer); stdcall;

    function Backup(X,Y: Integer;
                    out g: TGauss;
                    out s: tsys): Integer; stdcall;

    procedure ShowPage(Page: PPort2d); stdcall;

    function GetClearColor: Integer; stdcall;
    function GetPalette(Pal: PIntegers): Integer; stdcall;
  end;

  IPrint1 = interface(IPrint)
    ['{37EEF1EB-5F66-47FB-AE64-5DBFACFAB9D1}']

    procedure prj_to_prn(X1,Y1: Double; out X2,Y2: Double); stdcall;
  end;

  IDrawCard = interface(IUnknown)
    ['{58D6E76C-1123-4D55-BB95-28EE62BE6B5E}']
    function Get_gauss_bound(Frame: PGPoly): HResult; stdcall;
    function gProject(g: pxyz): TPoint; stdcall;
    function gBackup(X,Y: Integer): TGauss; stdcall;
    function wgs_to_xy(g: pxyz): txyz; stdcall;
    function xy_to_wgs(g: pxyz): txyz; stdcall;
    function GetResolution(cx,cy: Integer): Double; stdcall;
    function Get_sys(out s: tsys): HResult; stdcall;
  end;

  IDrawCard2 = interface(IUnknown)
    ['{1C35BF2C-D301-43E9-8243-9E566B361492}']
    function Project(x,y: Double): TPoint; stdcall;
    function Backup(X,Y: Integer): TGauss; stdcall;
  end;

  IVectorData = interface(IUnknown)
    ['{CA1D5268-88C7-46C8-BE46-462986676049}']
    function Open(Path: PChar): HResult; stdcall;

    procedure Draw(ADC: HDC; Card: IDrawCard2); stdcall;
    procedure Disp(ADC: HDC; Card: IDrawCard2); stdcall;

    function Locate(Rect: PGPoly; Pt: PGauss): Integer; stdcall;

    procedure ClearSelection; stdcall;
    procedure Select(Id: Integer); stdcall;
  end;

  IGpsTrack = interface(IUnknown)
    ['{DDA01BEF-85E7-40C6-A341-804EC123769B}']

    function GetDim: Integer; stdcall;

    function GetCount: Integer; stdcall;
    function GetPoint(Ind: Integer; Pos: PDoubles): Integer; stdcall;

    function GetBound(out lt,rb: txyz): Integer; stdcall;
    function GetStart(out dt: Integer): Integer; stdcall;

    procedure SetSys(sys: psys); stdcall;

    procedure SetPen(w,cl,pix: int); stdcall;

    procedure Draw(ADC: HDC; Card: IDrawCard); stdcall;
    procedure Disp(ADC: HDC; Card: IDrawCard); stdcall;

    function GetPos(out v,r: txyz): Integer; stdcall;
    function SetPos(T: Integer): Integer; stdcall;

    function MoveTo(Loc: PDoubles): Integer; stdcall;

    function Push(T,B,L,H: Double): Integer; stdcall;
  end;

  IPlanTracks = interface(IUnknown)
    ['{06C2D379-1E57-4FB1-903A-54972BF2A020}']

    function GetCount: Integer; stdcall;

    function GetFlags(Ind: Integer): Integer; stdcall;

    procedure SetSys(sys: psys); stdcall;

    function Get_org(Ind: Integer; out v: txyz): Integer; stdcall;
    function Set_org(Ind: Integer; gb,gl: Double): HResult; stdcall;

    function Get_pos(Ind: Integer; out v,r: txyz): Integer; stdcall;
    function Set_pos(Path: PChar; pos,cl: Integer): Integer; stdcall;

    procedure Draw(ADC: HDC; Card: IDrawCard); stdcall;
    procedure Disp(ADC: HDC; Card: IDrawCard); stdcall;

    function Move(Ind: Integer; Loc: PDoubles): Integer; stdcall;
  end;

  IPolyI = interface(IUnknown)
    ['{72481711-FC85-4D72-B2EB-DCF69D561FF5}']
    function GetDim: Integer; stdcall;
    procedure SetDim(Dim: Integer); stdcall;

    function GetCount: Integer; stdcall;
    function GetPoint(Ind: Integer; V: PIntegers): Integer; stdcall;
    function SetPoint(Ind: Integer; V: PIntegers): Integer; stdcall;
    function Delete(Ind: Integer): Integer; stdcall;
    function Insert(Ind: Integer; v: PIntegers): Integer; stdcall;
    procedure Clear; stdcall;
  end;

  IPolyF = interface(IUnknown)
    ['{F99C3734-FBDE-4C3F-A1ED-CBA39F2181F0}']
    function GetCount: Integer; stdcall;
    function GetPoint(Ind: Integer; V: PDoubles; N: Integer): Integer; stdcall;
    function SetPoint(Ind: Integer; V: PDoubles; N: Integer): Integer; stdcall;
    function Delete(Ind: Integer): Integer; stdcall;
    function Insert(Ind: Integer; v: pxyz): Integer; stdcall;
    procedure Clear; stdcall;
  end;

  IPolyList = interface(IUnknown)
    ['{548C3687-9567-44FA-88C8-5C5B08ECDE3A}']
    function SeekPoly(Ind: int; out lp: PLPoly): int; stdcall;
    function GetParam(Ind1,Ind2: int): int; stdcall;
  end;

  TIntfPointList = class(TInterfacedObject,IPointList)

    constructor Create(AList: TPointList);

    procedure Clear; stdcall;
    
    function GetCount: Integer; stdcall;
    function GetPoint(Ind: Integer; P: PPoint): Integer; stdcall;
    function SetPoint(Ind: Integer; P: PPoint): Integer; stdcall;
    function AddPoint(Ind,X,Y: Integer): Integer; stdcall;

  private
    fList: TPointList;
  end;

  TIntfStrings = class(TInterfacedObject,IStrings1)

    constructor Create;
    destructor Destroy; override;

    function GetCount: Integer; stdcall;

    function GetItem(Ind: Integer;
                     Str: PChar; MaxLen: Cardinal;
                     out Flags: Cardinal): HResult; stdcall;

  private
    fList: TStringList;
  public
    property List: TStringList read fList;
  end;

  TStringsObj = class(TInterfacedObject,IStrings)

    constructor Create(AList: TStrings);

    function GetCount: Integer; stdcall;
    function GetItem(Ind: Integer; Str: PChar): Integer; stdcall;
    function SetItem(Ind: Integer; Str: PChar): Integer; stdcall;
    function Add(Str: PChar): Integer; stdcall;
    function Indexof(Str: PChar): Integer; stdcall;

  private
    fList: TStrings;
  end;

  TNotify = class(TInterfacedObject,INotify)
    constructor Create(AProc: TIntegerProc);
    procedure Command(Cmd: Integer); stdcall;
  private
    fProc: TIntegerProc;
  end;

function StrIntf(Str: PChar; out Obj: IStrings1): Boolean;

function dllGetExtInterface(dll: HModule; Path: PChar;
                            const IID: TGUID; var Obj): HResult;

function GetPointListIntf(List: TPointList; var Obj): Boolean;

function get_view_list(list: TStrings;
                       const prj: IStrings1;
                       Ext: Pchar): Integer;

function GetNotifyIntf(Proc: TIntegerProc; var Obj): Boolean;

function dllGetRelInteface(Path: PChar;
                           const IID: TGUID;
                           var Obj): Boolean;
                           
implementation

uses
  SysUtils,dynlibs,
  convert,ofiles;

constructor TIntfPointList.Create(AList: TPointList);
begin
  inherited Create;
  fList:=TPointList.Create(1024);
end;

procedure TIntfPointList.Clear;
begin
  fList.Clear
end;

function TIntfPointList.GetCount: Integer;
begin
  Result:=fList.Count
end;

function TIntfPointList.GetPoint(Ind: Integer; P: PPoint): Integer;
var
  pp: PPoint;
begin
  Result:=-1;
  pp:=fList.Items[Ind];
  if Assigned(pp) then begin
    P^:=pp^; Result:=Ind
  end
end;

function TIntfPointList.SetPoint(Ind: Integer; P: PPoint): Integer;
var
  pp: PPoint;
begin
  Result:=-1;
  pp:=fList.Items[Ind];
  if Assigned(pp) then begin
    pp^:=P^; Result:=Ind
  end
end;

function TIntfPointList.AddPoint(Ind,X,Y: Integer): Integer;
var
  p: TPoint;
begin
  p.X:=X; p.Y:=Y;
  if Ind < 0 then
    Result:=fList.Add(@p)
  else
    Result:=fList.Insert_point(@p,Ind)
end;

constructor TIntfStrings.Create;
begin
  inherited Create;
  fList:=TStringList.Create;
end;

destructor TIntfStrings.Destroy;
begin
  fList.Free; inherited
end;

function TIntfStrings.GetCount: Integer;
begin
  Result:=fList.Count
end;

function TIntfStrings.GetItem(Ind: Integer;
                              Str: PChar; MaxLen: Cardinal;
                              out Flags: Cardinal): HResult;
begin
  Result:=S_FALSE; Flags:=0;

  if (Ind >= 0) and (Ind < fList.Count) then begin
    if MaxLen = 0 then MaxLen:=255;
    StrPLCopy(Str,fList[Ind],MaxLen);
    Result:=S_OK
  end
end;

constructor TStringsObj.Create(AList: TStrings);
begin
  inherited Create; fList:=AList
end;

function TStringsObj.GetCount: Integer;
begin
  Result:=fList.Count
end;

function TStringsObj.GetItem(Ind: Integer; Str: PChar): Integer;
begin
  Result:=-1; if Ind >= 0 then
  if Ind < fList.Count then begin
    StrPCopy(Str,fList[Ind]);
    Result:=Ind;
  end
end;

function TStringsObj.SetItem(Ind: Integer; Str: PChar): Integer;
begin
  Result:=-1; if Ind >= 0 then
  if Ind < fList.Count then begin
    fList[Ind]:=Str; Result:=Ind;
  end
end;

function TStringsObj.Add(Str: PChar): Integer;
begin
  Result:=fList.Add(Str)
end;

function TStringsObj.Indexof(Str: PChar): Integer;
begin
  Result:=fList.Indexof(Str)
end;

constructor TNotify.Create(AProc: TIntegerProc);
begin
  inherited Create; fProc:=AProc
end;

procedure TNotify.Command(Cmd: Integer);
begin
  if Assigned(fProc) then fProc(Cmd)
end;

function StrIntf(Str: PChar; out Obj: IStrings1): Boolean;
var
  list: TIntfStrings;
begin
  Result:=false; TPointer(Obj):=0;

  list:=TIntfStrings.Create;
  try
    list.List.SetText(Str);
    if list.List.Count > 0 then
    if list.GetInterface(IStrings1,Obj) then begin
      Result:=true; list:=nil
    end;
  finally
    list.Free
  end
end;

function GetPointListIntf(List: TPointList; var Obj): Boolean;
var
  iobj: TIntfPointList;
begin
  Result:=false; TPointer(Obj):=0;

  iobj:=TIntfPointList.Create(List);
  try
    if iobj.GetInterface(IPointList,Obj) then begin
      Result:=true; iobj:=nil
    end
  finally
    iobj.Free
  end
end;

function GetNotifyIntf(Proc: TIntegerProc; var Obj): Boolean;
begin
  Result:=false; TPointer(Obj):=0;
  with TNotify.Create(Proc) do
  Result:=GetInterface(INotify,Obj)
end;

function get_view_list(list: TStrings;
                       const prj: IStrings1;
                       Ext: Pchar): Integer;
var
  i,n: Integer; fl: Cardinal;
  fn: TShortStr;
begin
  list.Clear;

  if Assigned(prj) then begin
    n:=prj.GetCount; for i:=0 to n-1 do
    if prj.GetItem(i,fn,0,fl) = S_OK then
    if fl and flag_visible = 0 then

    if (Ext = nil)
    or This_ext(fn,Ext) then
    list.Add(fn);
  end;

  Result:=list.Count
end;

function dllGetExtInterface(dll: HModule; Path: PChar;
                            const IID: TGUID; var Obj): HResult;
type
  tfunc = function(Path: PChar;
                   const IID: TGUID;
                   var Obj): HResult; stdcall;
var
  func: tfunc;
begin
  Result:=S_FALSE; TPointer(Obj):=0;
  @func:=GetProcAddress(dll,'GetExtInterface');
  if Assigned(func) then Result:=func(Path,IID,Obj)
end;

var
  dll_rel: THandle;

function dllGetRelInteface(Path: PChar;
                           const IID: TGUID;
                           var Obj): Boolean;
begin
  Result:=false; TPointer(Obj):=0;

  if dll_rel < 32 then
  dll_rel:=LoadLibrary('dll_rel.so');

  if dll_rel >= 32 then
  Result:=dllGetExtInterface(dll_rel,Path,IID,Obj) = S_OK
end;

initialization
begin
  dll_rel:=0;
end;

finalization
begin
  xFreeLibrary(dll_rel);
end;

end.
