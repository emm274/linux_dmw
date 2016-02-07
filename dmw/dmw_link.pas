unit dmw_link; interface

uses
  Classes,contnrs,Math,
  otypes,xlist,xline,ogauss,
  tri_link,use_lnk,pmath;

const
  wait_gk: Longbool = false;

type
  PLinkPoint = ^TLinkPoint;
  TLinkPoint = record
    a,b: TGauss; tag: Integer
  end;

  PLinkArray = ^TLinkArray;
  TLinkArray = Array[0..1023] of TLinkPoint;

  ULinkPoint = record
    Ind,Cmd: Integer;
    p: TLinkPoint
  end;

  tdmwLink = class;

  udmwLink = class(TCustomList)
    constructor Create(ALink: tdmwLink);
    procedure Push(Ind,Cmd: Integer);
    function Undo(out Cmd: Integer): Integer;
  private
    fLink: tdmwLink;
  end;

  pdmwLink = class
    function Open(Path: PChar; nx,ny: Integer): Integer; virtual;
  protected
    fOwner: tdmwLink;
  end;

  tdmwLink = class(TCustomList)
    constructor Create;
    destructor Destroy; override;

    procedure Plugin(Obj: pdmwLink);

    function IsPhotoplan: bool;

    function Assign(link: tdmwLink): Integer;

    function Add_pp(pp: XY_XY_List): Integer;
    function Get_pp(pp: XY_XY_List): Integer;

    function Default_sys: Integer;

    procedure Set_dat(const dat: TDatum7);

    function Change_prj(const s: tsys): Boolean;
    function Change_sys(const s: tsys): Boolean;

    procedure sm_Transit(var sm_T: AB_Orient);

    function Get_lg_Transit(w,h: Integer;
                            out lg: lg_Transit;
                            jgw: Boolean): Integer;

    procedure Clear; override;
    procedure Reset_link;

    procedure Changed;

    function link_frame(nx,ny: Integer;
                        Frame: PGPoly): Boolean;

    procedure link_bmp(w,h: int; G: PGPoly);
    procedure link_bmpc(w,h: int; G: PGPoly);

    function Open_link(Path: PChar; nx,ny: Integer): Boolean;

    function SaveAs_link(Path: PChar): Boolean;

    function SaveAs_work(Dest: PChar): Boolean;

    procedure SaveAs_text(Dest,Pcx: PChar; im_sys: psys);
    procedure SaveAs_tab(Dest,Pcx: PChar);

    function Save_link: Boolean;
    procedure Close_link;

    procedure bit_scale(k: Integer);

    procedure Transit_geo(Asys: psys; const tr: Real3x3);

    procedure sm_transform(const T: AB_Orient);

    function Refresh: Boolean;
    procedure xRefresh;

    function Get_tri_list(List: TTriList): Integer;

    procedure Assign_bit_fast(const T: Real3x3);
    procedure Assign_map_fast(const T: Real3x3);

    function Set_projective_3x3: Boolean;

    function Set_xml(w,h: int; gb: PGPoly; const gs: tsys): Boolean;

    function bit_IndexOf(const p: TGauss; eps: Float): Integer;

    function add_ab(const a,b: tgauss): Integer;
    function add_xy(x,y: Integer; const g: tgauss): Integer;

    procedure add_sm(const v: LVector);
    function add_2g(const g: GVector): Boolean;

    function New_ab(const a,b: tgauss): Boolean;
    function Mov_ab(Ind: Integer; const a,b: tgauss): Boolean;
    function Del_ab(Ind: Integer): Boolean; // [1..Count]

    function bit_to_sm(const a: TPoint; out b: TPoint): Boolean;
    function sm_to_bit(const a: TPoint; out b: TPoint): Boolean;

    function pix_to_xy(x,y: Double; out g: tgauss): Boolean;
    function xy_to_pix(const g: tgauss; out p: tgauss): Boolean;

    function l_to_xy(const l: TPoint; out g: tgauss): Boolean;
    function xy_to_l(const g: tgauss; out l: TPoint): Boolean;

    function l_to_g(x,y: Double; out g: xgeoid): Boolean;

    function g_to_p(const g: xgeoid; out p: tgauss): Boolean;
    function g_to_l(const g: xgeoid; out l: TPoint): Boolean;

    function bit_Bound_Contains(const p: TPoint): Boolean;
    function sm_Bound_Contains(const p: TPoint): Boolean;

    function ContainsGauss(const g: xgeoid): Boolean;

    function This_Pair(const lt,rb: TGauss;
                       who: Integer; out p: TGauss): Integer;

    function Move_Pair(i,who: Integer; const p: TGauss): Boolean;

    function Optimize(const lt,rb: TGauss): Boolean;

    function Verify(is_ab: Boolean): Boolean;

    function Get_bound(w,h: Integer; G: PGPoly): Double;
    function Get_resolution(w,h: Integer): Double;

    function Get_geo_mpp(const g: TGeoPoint): double;
    function Get_pix_mpp(x,y: double): double;

    function Pickup_res(x,y, w,h: Integer): Double;

    function Get_clip(lp: PLPoly; Scale: Integer): Integer;

    function add_bound(x1,y1,x2,y2,k: Double): Boolean;

  private
    ftri: TTriLink;

    fUndo: udmwLink;
    fPlugin: TObjectList;

    fBit_fast: Boolean;
    fMap_fast: Boolean;
    fEnabled: Boolean;
    fis_3x3: Boolean;

    fMatrix: Real3x3;
    fForw_t: Real3x3;
    fBack_t: Real3x3;

    forg,fsys,fsys1: tsys;

    fPointIndex: Integer;

    fbit_lt,fbit_rb: TPoint;
    fsm_lt,fsm_rb: TPoint;
    fsm_ed: Integer;

    fim_eps: Double;
    fsm_eps: Double;

    fEditCount: int;

    fImageSize: TPoint;

    fis_pcx: Boolean;
    fEditFlag: Boolean;
    fIsBak: Boolean;

    fOnChange: TNotifyEvent;

    vm_Path: TShortStr;
    vm_Tiff: TShortStr;

    procedure Push_sys(const s: tsys);

    function GetFileName: String;
    function GetPath: PChar;
    procedure SetPath(APath: PChar);

    function Get_im_Point(I: Integer): TPoint;

    function Get_im_Geoid(I: Integer): XGeoid;
    function Get_sm_Geoid(I: Integer): XGeoid;

    function get_mpp: Double;
    function get_res: Double;

    function Get_sys: tsys;
    procedure Set_sys(const s: tsys);

    function Get_Matrix: Real3x3;

    procedure Change;

  public
    property Enabled: Boolean read fEnabled;

    property mpp: Double read get_mpp;
    property im_res: Double read get_res;

    property Bit_fast: boolean read fBit_fast write fBit_fast;
    property Map_fast: boolean read fMap_fast write fMap_fast;

    property is_pcx: Boolean read fis_pcx write fis_pcx;
    property is_3x3: Boolean read fis_3x3;

    property im_eps: Double read fim_eps write fim_eps;
    property sm_eps: Double read fsm_eps write fsm_eps;

    property im_Points[I: Integer]: TPoint read Get_im_Point;

    property im_Geoids[I: Integer]: XGeoid read Get_im_Geoid;
    property sm_Geoids[I: Integer]: XGeoid read Get_sm_Geoid;

    property tri: TTriLink read ftri;

    property PointIndex: Integer read fPointIndex
                                 write fPointIndex;

    property bit_lt: TPoint read fbit_lt;
    property bit_rb: TPoint read fbit_rb;
    property sm_lt: TPoint read fsm_lt;
    property sm_rb: TPoint read fsm_rb;

    property sm_ed: Integer read fsm_ed;

    property sys: tsys read fsys write Set_sys;
    property org: tsys read forg write forg;
    property geo_sys: tsys read Get_sys;

    property forw_t: Real3x3 read fforw_t;
    property back_t: Real3x3 read fback_t;

    property Matrix: Real3x3 read Get_Matrix;

    property FileName: String read GetFileName;
    property Path: PChar read GetPath write SetPath;

    property EditFlag: Boolean write fEditFlag;
    property IsBak: Boolean write fIsBak;

    property Undo: udmwLink read fUndo;

    property OnChange: TNotifyEvent write fOnChange;
  end;

  txy_to_xy = class(tdmwLink)
    function LoadFrom(Path: PChar): Integer;
  end;

  idmwLink = class(TInterfacedObject,ILink2)

    constructor Create(Alnk: tdmwLink; Bmp: PChar);

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
  private
    flnk: tdmwLink;
    fWidth: Integer;
    fHeight: Integer;
  end;

function this_FFT(Path: PChar): Boolean;

function open_fft(Pcx,Fft: PChar; const g: xgeoid;
                  scale: Integer; more: Boolean): PChar;

function Strings_fft(List: TStrings; Fft: PChar): Integer;

function load_jgw(Path: PChar;
                  out tr: Real3x3;
                  sys: psys): Boolean;

procedure Create_link(l,g: PGPoly; sys: psys; Dest: PChar);

function get_lg_link(Path: PChar; out lg: lg_Transit): Boolean;
function get_lg_link1(Path: PChar; out lg: lg_Transit): Boolean;

function cut_lg_link(Path: PChar; iw,ih,cx,cy,cw,ch: Integer;
                     out lg: lg_Transit): Boolean;

procedure create_img_link(Dest: PChar; l,g: PGPoly; sys: psys); stdcall;

procedure create_lg_link(Dest: PChar; const lg: lg_Transit);

function copy_lg_link(Src,Dest: PChar): bool;

procedure create_tiff_link(Dest: PChar;
                           w,h: Double; g: PGPoly;
                           const sys: tsys);

procedure cut_link(Dest,Path: PChar;
                   iw,ih,cx,cy,cw,ch: Integer);

procedure Create_jgw(Dest,Ext: PChar; const tr: Real3x3);

function Get_img_res(l,g: PGPoly): Double;
function Get_img_res1(l,g: PGPoly): TGauss;

function lnk_to_tab(Dest,Path: PChar): Boolean;

procedure copy_link(Dest,Path: PChar; Zoom: Integer);

function copy_tab_link(src,dst: PChar): bool;

implementation

uses
  SysUtils,
  convert,ofiles,ofiles1,
  xpoly,xy,xmath,xintf1,
  img_dll,img_rel,xbl_use,
  tiff_y,xsid;

function verify_val(v,ed: Double): Boolean;
var
  d: Double;
begin
  if Abs(v) >= 10000000 then
    Result:=false
  else begin
    d:=Round(v*ed) / ed;
    Result:=Abs(d-v) < 0.1
  end
end;

constructor udmwLink.Create(ALink: tdmwLink);
begin
  inherited Create(Sizeof(ULinkPoint),256);
  fLink:=ALink
end;

procedure udmwLink.Push(Ind,Cmd: Integer);
var
  r: ULinkPoint; p: PLinkPoint;
begin
  p:=fLink[Ind];
  if Assigned(p) then begin
    r.Ind:=Ind; r.Cmd:=Cmd;
    r.p:=p^; Add(@r)
  end
end;

function udmwLink.Undo(out Cmd: Integer): Integer;
var
  fp: ^ULinkPoint; p: PLinkPoint; bx: Integer;
begin
  Result:=-1; Cmd:=0;

  fp:=Last;
  if Assigned(fp) then begin

    bx:=fp.Ind;
    p:=fLink[bx]; Cmd:=fp.Cmd;

    case fp.Cmd of

  0:  if Assigned(p) then begin
        p^:=fp.p; Result:=bx
      end;

  1:  if Assigned(p) then begin
        Result:=flink.Delete(bx);
      end;

  2:  if bx >= 0 then
      if bx <= fLink.Count then begin
        fLink.Insert_range(@fp.p,bx,1);
        Result:=bx
      end
    end;

    Delete_last
  end
end;

function pdmwLink.Open(Path: PChar; nx,ny: Integer): Integer;
begin
  Result:=0
end;

constructor tdmwLink.Create;
begin
  inherited Create(Sizeof(TLinkPoint),256);

  ftri:=TTriLink.Create;

  fUndo:=udmwLink.Create(Self);
  fUndo.Enabled:=false;

  fPlugin:=TObjectList.Create;

  fIsBak:=true; fsm_ed:=10; Clear;
end;

destructor tdmwLink.Destroy;
begin
  Close_link;

  fPlugin.Free;
  fUndo.Free;

  ftri.Free;

  inherited
end;

procedure TDmwLink.Plugin(Obj: pdmwLink);
begin
  Obj.fOwner:=Self;
  fPlugin.Add(Obj)
end;

function TDmwLink.GetFileName: String;
begin
  Result:=ExtractFileName(StrPas(vm_Path))
end;

function TDmwLink.GetPath: PChar;
begin
  Result:=vm_Path
end;

procedure TDmwLink.SetPath(APath: PChar);
begin
  StrCopy(vm_Path,'');
  if Assigned(APath) then
  StrUpdateExt(vm_Path,APath,'.@@@');
end;

function TDmwLink.Get_im_Point(I: Integer): TPoint;
var
  p: PLinkPoint;
begin
  p:=Items[I];
  if p = nil then
    Result:=Point(0,0)
  else
    with p.a do
    Result:=_LGauss(x,y)
end;

function TDmwLink.Get_im_Geoid(I: Integer): XGeoid;
var
  p: PLinkPoint;
begin
  p:=Items[I];
  if p = nil then
    Result:=GeoPoint_nil
  else
  if forg.pps < 0 then
    Result:=GeoPoint(p.a.x,p.a.y,nil)
  else
    Result:=prj_xy_to_x(p.a,org)
end;

function TDmwLink.Get_sm_Geoid(I: Integer): XGeoid;
var
  p: PLinkPoint;
begin
  p:=Items[I];
  if p = nil then
    Result:=GeoPoint_nil
  else
    Result:=prj_xy_to_x(p.b,sys)
end;

function TDmwLink.get_mpp: Double;
var
  r: Double; g1,g2: tgauss;
begin
  Result:=1;

  r:=Long_Dist(fbit_lt,fbit_rb);
  if r > 0.1 then begin

    l_to_xy(fbit_lt,g1);
    l_to_xy(fbit_rb,g2);

    Result:=Gauss_Dist(g1,g2)/r
  end
end;

function TDmwLink.get_res: Double;
var
  p1,p2: TLinkPoint;
  i: Integer; r1,r2: Double;
  lp: PLinkArray;
begin
  Result:=1;

  if Count > 1 then begin lp:=First;

    p1:=lp[0]; p2:=lp[1]; lp:=@lp[2];
    r1:=Gauss_Dist(p1.a,p2.a);

    for i:=3 to Count do begin

      r2:=Gauss_Dist(p1.a,lp[0].a);
      if r2 > r1 then begin
        r1:=r2; p2:=lp[0]
      end;

      lp:=@lp[1]
    end;

    r2:=Gauss_Dist(p1.b,p2.b);
    if r1 > 1 then Result:=r2/r1
  end
end;

procedure TDmwLink.Set_dat(const dat: TDatum7);
begin
  fsys.dat:=dat
end;

function TDmwLink.Get_sys: tsys;
var
  s: tsys; p: PLinkPoint;
begin
  s:=fsys; p:=First;
  if Assigned(p) then with p.b do
  sys_projection(s,x,y); Result:=s
end;

procedure TDmwLink.Push_sys(const s: tsys);
begin
  fsys:=s; fsys1:=s
end;

procedure TDmwLink.Set_sys(const s: tsys);
var
  i: Integer; lp: PLinkArray;
  g: tgauss; r: tgeoid;
begin
  if Count = 0 then
    Push_sys(s)
  else
  if fsys1.prj = 0 then begin
    Push_sys(s); fEditFlag:=true
  end else

  if fsys.pps = 1 then
  if s.pps = 1 then begin

    lp:=First;
    for i:=1 to Count do begin
      g:=lp[0].b;
      prj_XY_BL(g.x,g.y, sys, r.b,r.l);
      prj_BL_XY(r.b,r.l, s, g.x,g.y);
      lp[0].b:=g; lp:=@lp[1]
    end;

    Push_sys(s); fEditFlag:=true
  end
end;

function TDmwLink.Get_Matrix: Real3x3;
var
  pp: ITransit2;
  i: Integer; lp: PLinkArray;
  tr,bt: Real3x3;
begin
  Result:=Identify_3x3;

  if Count >= 3 then
  if GetPMathIntf(ITransit2,pp) = S_OK then begin

    lp:=First;
    for i:=0 to Count-1 do
    with lp[i] do pp.Add(@a,@b);

    if pp.CalcAffine(xy_Affine4,@tr,@bt) then
    Result:=tr
  end
end;

procedure TDmwLink.Change;
begin
  if Assigned(fOnChange) then
  fOnChange(Self)
end;

function TDmwLink.IsPhotoplan: bool;
var
  i: int; lp: PLinkArray; p: TGauss;
begin
  Result:=false;
  if Count = 4 then begin
    lp:=First; Result:=true;
    for i:=0 to 3 do begin
      p:=lp[i].a;
      if ((p.x > 0.1) and (p.x < fImageSize.X-1))
      or ((p.y > 0.1) and (p.y < fImageSize.Y-1)) then
      begin Result:=false; Break end
    end
  end
end;

function TDmwLink.Assign(link: tdmwLink): Integer;
var
  fn: TShortstr;
begin
  StrCopy(fn,vm_Path);
  LoadList(link); fEditFlag:=true;
  StrCopy(vm_Path,fn);

  forg:=link.org;
  fsys:=link.sys;
  fsys1:=link.fsys1;

  fMatrix:=link.fMatrix;
  fis_3x3:=link.fis_3x3;

  Refresh; Result:=Count
end;

function TDmwLink.Add_pp(pp: XY_XY_List): Integer;
var
  i: Integer; lp: pxy_xy_array;
  r: TLinkPoint;
begin
  Fillchar(r,Sizeof(r),0);

  lp:=pp.First;
  for i:=1 to pp.Count do begin

    r.a:=lp[0].p1;
    r.b:=lp[0].p2; Add(@r);

    lp:=@lp[1]
  end;

  Result:=pp.Count
end;

function TDmwLink.Get_pp(pp: XY_XY_List): Integer;
var
  i: Integer; lp: PLinkArray;
begin
  pp.Clear; lp:=First;

  for i:=0 to Count-1 do
  with lp[i] do pp.xAdd(i+1,0,0,0,0,a,b);

  Result:=pp.Count
end;

function TDmwLink.Change_prj(const s: tsys): Boolean;
var
  s1: tsys;
begin
  Result:=false; s1:=s;

  if Count = 0 then begin
    Push_sys(s1); Result:=true
  end
  else begin
    if fsys.elp <> s1.elp then
    if fsys.elp in [0..1] then
    if s1.elp in [0..1] then
    s1.elp:=fsys.elp;

    if fsys1.prj = 0 then
      Result:=true
    else
      Result:=sys_Equal(s1,sys)
  end
end;

function TDmwLink.Default_sys: Integer;
var
  lp: PLinkArray;
begin
  if fsys.prj = 0 then begin
    lp:=First; if Assigned(lp) then begin
      with lp[0].b do sys_projection(fsys,x,y);
      if fsys.prj > 0 then fsys.pps:=1;
    end;
  end; Result:=fsys.pps
end;

function TDmwLink.Change_sys(const s: tsys): Boolean;
var
  i: Integer; lp: PLinkArray;
  s1: tsys;
begin
  Result:=false;

  s1:=s; if s.prj > 0 then s1.pps:=1;

  if Count <= 0 then begin
    Push_sys(s); Result:=true
  end else

  if s1.pps = 1 then
  if Default_sys = 1 then

  if not elp_Equal(fsys.elp,s1.elp)
  or not prj_Equal(fsys,s) then begin

    lp:=First;
    for i:=1 to Count do begin

      with lp[0].b do
      lp[0].b:=prj_to_prj(x,y,fsys,s1);

      lp:=@lp[1]
    end;

    Push_sys(s1); Refresh;
    
    Result:=true
  end
end;

procedure TDmwLink.sm_Transit(var sm_T: AB_Orient);
var
  p1: PLinkPoint; s1,s2: tsys;
begin
  s1:=sm_T.sys;
  if Count > 0 then
  if s1.prj > 0 then begin

    if fsys.prj = 0 then begin
      fsys:=s1; fsys.pps:=0;

      p1:=First;
      if s1.x0 = 0 then
      if s1.y0 = 0 then
      if s1.prj = 2 then
      if Trunc(p1.b.y / 1000000) = 0 then
      fsys.y0:=500000;
    end;

    s2:=fsys; if fsys.prj > 0 then s2.pps:=1;
    sm_T.Change_Transit(s2,true)
  end
end;

function tdmwLink.Get_lg_Transit(w,h: Integer;
                                 out lg: lg_Transit;
                                 jgw: Boolean): Integer;
var
  i: Integer; g: TGauss; r: xgeoid;
  lp: PLinkArray; lc: Double;
begin
  if jgw then
  if fsys.prj = 0 then
  if Count > 0 then begin
    lp:=First; g:=lp[0].b;
    sys_projection(fsys,g.x,g.y)
  end;

  if w*h <= 0 then begin
    w:=fbit_rb.X; h:=fbit_rb.Y;
  end;

  Init_lg_Transit(lg); lg.s:=fsys;
  Frame_to_GOrient(0,0,w,h,@lg.l);

  for i:=0 to 3 do begin

    with lg.l[i] do
    pix_to_xy(x,y,g);
    r:=prj_xy_to_x(g,fsys);
    lg.g[i]:=_Gauss(r.x,r.y);
  end;

  Result:=fsys.pps
end;

procedure tdmwLink.Clear;
begin
  inherited Clear; fUndo.Clear;

  fBit_fast:=true; fForw_t:=Identify_3x3;
  fMap_fast:=true; fBack_t:=fForw_t;

  fEditFlag:=false; StrCopy(vm_Path,'');

  Fillchar(forg,Sizeof(forg),0);

  fImageSize:=Point(0,0);

  Reset_link; fEnabled:=false;
end;

procedure tdmwLink.Reset_link;
begin
  ftri.Clear; fEditCount:=0;
  if fPointIndex > Count then
  fPointIndex:=0
end;

procedure tdmwLink.Changed;
begin
  fEditFlag:=true;
  Reset_Link; Refresh;
  Change;
end;

function tdmwLink.link_frame(nx,ny: Integer;
                             Frame: PGPoly): Boolean;
begin
  add_xy(0 ,ny,Frame[0]);
  add_xy(0 , 0,Frame[1]);
  add_xy(nx, 0,Frame[2]);
  add_xy(nx,ny,Frame[3]);
  Result:=Refresh;
end;

procedure tdmwLink.link_bmp(w,h: int; G: PGPoly);
begin
  add_xy(0,0,G[0]);
  add_xy(w,0,G[1]);
  add_xy(w,h,G[2]);
  add_xy(0,h,G[3]);
end;

procedure tdmwLink.link_bmpc(w,h: int; G: PGPoly);
var
  x1,y1,x2,y2,dx2,dy2: double;
  i: int; t: GOrient;
begin
  x1:=G[2].x; x2:=G[0].x;
  y1:=G[0].y; y2:=G[1].y;

  if (G[1].x = x2) and
     (G[3].x = x1) and
     (G[3].y = y1) and
     (G[2].y = y2) then begin

    dx2:=(x2-x1)/h/2;
    dy2:=(y2-y1)/w/2;

    for i:=0 to 3 do with G[i] do
    t[i]:=_Gauss(x+dx2,y-dy2);

    add_xy(0,0,t[0]);
    add_xy(w,0,t[1]);
    add_xy(w,h,t[2]);
    add_xy(0,h,t[3])
  end
  else begin
    add_xy(0,0,G[0]);
    add_xy(w,0,G[1]);
    add_xy(w,h,G[2]);
    add_xy(0,h,G[3])
  end
end;

function tdmwLink.Open_link(Path: PChar; nx,ny: Integer): Boolean;

function link_lg(Path: PChar; nx,ny: Integer): Boolean;
var
  lg: lg_Transit;
begin
  Result:=false;
  if lg_LoadFrom(lg,Path) then begin

    if This_ext(Path,'.rel') then
      link_bmp(nx,ny,@lg.l)
    else
      link_bmp(nx,ny,@lg.l);

    fsys:=lg.s; Result:=true
  end
end;

function link_xy(Path: PChar; nx,ny: Integer): Boolean;
var
  h: Integer; g: GOrient;
  fn: TShortStr;
begin
  Result:=false;

  StrUpdateExt(fn,Path,'.XY');
  h:=FileOpen(StrPas(fn),fm_OpenRead);
  if h > 0 then begin
    FileRead(h,G,4*SizeOf(tgauss));
    link_bmp(nx,ny,@G); FileClose(h);
    Result:=true
  end
end;

function link_fot(Path: PChar; nx,ny: Integer): Boolean;
var
  txt: TTextfile; lev,w,h: Integer;
  s: tsys; lt,rb: TGauss; g: GOrient;
begin
  Result:=false;

  txt:=TTextfile.Create;
  try
    if txt.Open(Path) then
    if txt.xStrLine <> nil then

    if sysToken(txt.str,s) >= 0 then
    if txt.xStrLine <> nil then

    if txt.x_Gauss(lt) then
    if txt.x_Gauss(rb) then

    if txt.xStrLine <> nil then

    if txt.x_Int(lev) then
    if txt.x_Int(w) then
    if txt.x_Int(h) then

    if lev in [1,2,4,8,16,64] then
    if (w > 0) and (w < 32000) then
    if (h > 0) and (h < 32000) then begin

      g[0].x:=rb.x; g[0].y:=lt.y;
      g[1].x:=rb.x; g[1].y:=rb.y;
      g[2].x:=lt.x; g[2].y:=rb.y;
      g[3].x:=lt.x; g[3].y:=lt.y;

      link_bmp(w * lev,h * lev,@g);
      forg:=sys_nil; fsys:=s;

      Result:=true
    end;
  finally
    txt.Free
  end
end;

function link_tab(Path: PChar; nx,ny: Integer): Boolean;

function Get_xy(s: PChar; out x,y: Double): Boolean;
var
  p: PChar;
begin
  Result:=false; p:=StrScan(s,'(');

  if p <> nil then begin
    StrCopy(s,@p[1]);

    p:=StrScan(s,')');
    if p <> nil then begin p^:=#0;

      if RealToken(s,x) then
      Result:=RealToken(s,y);

      StrCopy(s,@p[1]);
    end
  end
end;

var
  tab: TTextFile; g1,g2: tgauss; r: tgeoid;
  List: TGaussList; s: tsys; ax,dx,dy,mk: Double;
  i,v1,v2,v: Integer; fn,key: TShortstr;
  fl,deg: Boolean;
begin
  Result:=false;

  StrUpdateExt(fn,Path,'.tab');
  if FileExist(fn) then begin

    Is_Real_Point:=true;

    fl:=Is_Comma_Delimiter;
    Is_Comma_Delimiter:=true;

    s:=sys7(-1,0,0, 0,0,0);
    dx:=0; dy:=0; mk:=1; deg:=false;

    tab:=TTextFile.Create;
    List:=TGaussList.Create(256);
    try
      if tab.Open(fn) then

      if (tab.xStrLine <> nil) and tab.x_cmd('!table') then
      if (tab.xStrLine <> nil) and tab.x_cmd('!version') then
      if (tab.xStrLine <> nil) and tab.x_cmd('!charset') then

      if (tab.xStrLine <> nil) then
      if tab.x_cmd('Definition') and tab.x_cmd('Table') then

      if (tab.xStrLine <> nil) and tab.x_cmd('File') then begin

        tab.x_str(vm_Tiff);

        if (tab.xStrLine <> nil) then
        if tab.x_cmd('Type') and tab.x_cmd('"Raster"') then

        while tab.xStrLine <> nil do

        if tab.x_cmd('CoordSys') then begin

          if tab.x_cmd('NonEarth') then
            s.pps:=0
          else
          if tab.x_cmd('Earth') then
          if tab.x_cmd('Projection') then begin

            if tab.x_Int(v1) then
            if tab.x_Int(v2) then begin

              s.pps:=1; s.elp:=1;

              if v2 in [104] then s.elp:=9
              else
              if v2 = 1001 then s.elp:=1
              else
              if v2 in [62,70] then s.elp:=6;

              if s.elp = 1 then s.dat:=ru42_Datum;

              if v1 = 1 then
                s.prj:=3
              else
              if tab.x_str(key) <> nil then begin

                if StrIComp(key,'mi') = 0 then mk:=1609.344;

                case v1 of
              3:  if tab.x_Double(s.lc) then
                  if tab.x_Double(s.b1) then
                  if tab.x_Double(s.b2) then
                  if tab.x_Double(s.b3) then
                  if tab.x_Double(s.y0) then
                  if tab.x_Double(s.x0) then begin
                    s.prj:=28; cuba_sys(s)
                  end;

              8:  if tab.x_double(s.lc) then begin
                    s.prj:=1;

                    if tab.x_Double(ax) then
                    if tab.x_Double(ax) then begin

                      if s.elp = 9 then
                      if Abs(ax - 0.9996) < 1E-6 then
                      begin s.prj:=2; ax:=1 end;

                      mk:=mk / ax;
                      tab.x_Double(s.y0);
                      tab.x_Double(s.x0);
                    end
                  end
                end
              end;

              s.b1:=s.b1/180*Pi;
              s.b2:=s.b2/180*Pi;
              s.b3:=s.b3/180*Pi;
              s.lc:=s.lc/180*Pi;

              if s.prj = 0 then s.pps:=0
            end
          end

        end else
        if tab.x_cmd('Units') then begin

          if s.pps >= 0 then
          if tab.x_cmd('"degree"') then begin
            if s.pps <> 1 then s.pps:=-1;
            deg:=true
          end
        end else

        if get_xy(tab.str,g1.x,g1.y) then
        if get_xy(tab.str,g2.x,g2.y) then

        if (g2.x >= 0) and ((nx = 0) or (g2.x <= nx)) then
        if (g2.y >= 0) and ((ny = 0) or (g2.y <= ny)) then begin
          List.AddItem(g1.y,g1.x);
          List.AddItem(g2.x,g2.y);
        end
      end;

      if s.pps >= 0 then
      if List.Count > 1 then begin

        i:=0; dx:=s.x0; dy:=s.y0;
        s.x0:=0; s.y0:=0;

        while i < List.Count-1 do begin
          g1:=List.Points[i]; Inc(i);
          g2:=List.Points[i]; Inc(i);

          with s do if pps = 1 then

          if deg then begin
            g1.x:=g1.x/180*Pi; g1.y:=g1.y/180*Pi;
            BL_to_XY(g1.x,g1.y, 0,0,0, elp,prj, g1.x,g1.y);
          end
          else begin
            g1.x:=(g1.x - dx)*mk;
            g1.y:=(g1.y - dy)*mk;
            sys_XY_BL(g1.x,g1.y, s, r.b,r.l);
            prj_BL_XY(r.b,r.l, s, g1.x,g1.y)
          end;

          Add_ab(g2,g1)
        end;

        forg:=sys_nil; fsys:=s;
        Result:=Count > 0
      end;

    finally
      List.Free;
      tab.Free
    end;

    Is_Comma_Delimiter:=fl;
    Is_Real_Point:=false
  end
end;

function link_ozi(Path: PChar; nx,ny: Integer): Boolean;

function get_angle(txt: TTextfile;
                   out v: Double; ns: PChar): Boolean;
var
  rc: Integer; deg,min: Double; s: TShortstr;
begin
  Result:=false; v:=0; rc:=0;

  if txt.x_Double(deg) then Inc(rc);
  if txt.x_Double(min) then Inc(rc);
  if txt.x_str(s) <> nil then  Inc(rc);

  if rc = 3 then
  if Strlen(s) = 1 then begin

    v:=(deg + min/60)/180 * Pi;

    if s[0] = ns[0] then
      Result:=true
    else
    if s[0] = ns[1] then begin
      v:=-v; Result:=true
    end
  end
end;

var
  mmp: tdmwLink;
  txt: TTextfile;
  msf,v,k,dy,dx,tx,ty: Double;
  i,id,rc: Integer; lp: PLinkArray;
  g1,g2,g: TGauss; pp: PLinkPoint;
  s,prj: tsys; fl: Boolean;
  str: TShortstr;
begin
  fl:=Is_Comma_Delimiter;
  Is_Comma_Delimiter:=true;
  is_Real_Point:=true;

  msf:=1;
  s:=sys7(1,prj_deg,9, 0,0,0);
  prj:=s; prj.pps:=0; prj.prj:=-1;

  txt:=TTextfile.Create;
  mmp:=tdmwLink.Create;
  try
    if txt.Open_ext(Path,'.map') then
    while txt.xStrLine <> nil do
    if txt.x_str(str) <> nil then
    if StrIComp(str,'OziExplorer') = 0 then begin

      while txt.xStrLine <> nil do
      if txt.x_str(str) <> nil then

      if StrIComp(str,'WGS 84') = 0 then begin
        s.elp:=9; prj.elp:=9
      end else
      if StrIComp(str,'Map Projection') = 0 then begin
        if txt.x_str(str) <> nil then
        if StrIComp(str,'Transverse Mercator') = 0 then
          prj.prj:=2
        else
        if StrIComp(str,'Mercator') = 0 then begin
          prj.pps:=1; prj.prj:=3
        end
      end else

      if StrIComp(str,'Projection Setup') = 0 then begin

        if txt.x_Double(v) then
        if txt.x_Double(prj.lc) then
        if txt.x_Double(k) then
        if txt.x_Double(dx) then
        if txt.x_Double(dy) then begin
          prj.pps:=1; prj.lc:=prj.lc/180*Pi;
          if eps_Equal(k,1,1E-6) then prj.prj:=1
        end;

      end else

      if StrIComp(str,'MSF') = 0 then begin
        if txt.x_Double(v) then msf:=v
      end else

      if StrLIComp(str,'Point',5) = 0 then begin

        if txt.x_str(str) <> nil then
        if StrIComp(str,'xy') = 0 then
        if txt.x_Gauss(g1) then

        if txt.x_str(str) <> nil then
        if StrIComp(str,'in') = 0 then begin

          g1.x:=g1.x / msf;
          g1.y:=g1.y / msf;

          if txt.x_str(str) <> nil then
          if StrIComp(str,'deg') = 0 then begin

            rc:=0;
            if get_angle(txt,g2.x,'NS') then Inc(rc);
            if get_angle(txt,g2.y,'EW') then Inc(rc);

            if rc = 2 then begin
              with s,g2 do
              BL_to_XY(x,y,lc,b1,b2,elp,prj,x,y);

              pp:=Items[ Add_ab(g1,g2) ];
              if Assigned(pp) then pp.tag:=1
            end
          end
        end
      end else

      if Count = 0 then
      if StrIComp(str,'MMPXY') = 0 then begin
        if txt.x_Int(id) then
        if txt.x_Gauss(g1) then
        mmp.add_ab(g1,g1);
      end else
      if StrIComp(str,'MMPLL') = 0 then begin
        if txt.x_Int(id) then
        if txt.x_Gauss(g2) then begin

          g2.x:=g2.x/180*Pi;
          g2.y:=g2.y/180*Pi;

          pp:=mmp[id-1];
          if Assigned(pp) then begin
            with s,g2 do
            BL_to_XY(y,x,lc,b1,b2,elp,prj,x,y);
            pp.b:=g2; pp.tag:=1
          end

        end
      end;

      if prj.pps = 1 then
      if prj.prj > 0 then begin

        if Count = 0 then LoadList(mmp);

        if prj.prj <= 2 then
        dx:=dx - prj_ZoneY(prj);

        lp:=First;

        if lp[0].tag = 1 then
        if prj.prj in [1,2] then
        if Count > 0 then begin

          with lp[0].b do
          prj_XY_BL(x,y, s, g1.x,g1.y); g2:=g1;

          for i:=1 to Count-1 do begin
            with lp[i].b do
            prj_XY_BL(x,y, s, g.x,g.y);
            Max_GPort(g1,g2,g);
          end;

          g.y:=(g1.y+g2.y)/2;
          ty:=zLongitude(g.y);

          if Abs(g.y-ty) < Abs(g.y-prj.lc) then
          prj.lc:=ty
        end;

        for i:=1 to Count do begin

          if lp[0].tag = 1 then begin
            with lp[0].b do begin
              prj_XY_BL(x,y, s, tx,ty);
              prj_BL_XY(tx,ty, prj, x,y);
            end;

            lp[0].tag:=0
          end
          else begin
            g2:=lp[0].b;
            lp[0].b.x:=g2.y - dy;
            lp[0].b.y:=g2.x - dx;
          end;

          lp:=@lp[1]
        end;

        s:=prj
      end;

      if Count > 0 then begin
        forg:=sys_nil; fsys:=s;
      end
    end;

  finally
    mmp.Free;
    txt.Free
  end;

  Is_Comma_Delimiter:=fl;
  is_Real_Point:=false;

  Result:=Count > 0
end;

function link_rlz(Path: PChar; nx,ny: Integer): Boolean;
var
  h: Integer; g: GOrient;
  hdr: TRlzHeader;
begin
  Result:=false;

  if this_ext(Path,'.RLZ') then begin
    h:=FileOpen(StrPas(Path),fm_OpenRead);
    if h > 0 then begin
      if this_rlz(h,hdr) then

      if hdr.s.pps in [0..1] then
      if hdr.s.prj in [0..prj_max] then
      if hdr.s.elp in [0..elp_max] then begin

        G[0].x:=hdr.xmax; G[0].y:=hdr.ymin;
        G[1].x:=hdr.xmax; G[1].y:=hdr.ymax;
        G[2].x:=hdr.xmin; G[2].y:=hdr.ymax;
        G[3].x:=hdr.xmin; G[3].y:=hdr.ymin;

        link_bmp(nx,ny,@G);
        fsys:=hdr.s; Result:=true
      end;

      FileClose(h)
    end
  end
end;

function link_txt(Path: PChar; nx,ny: Integer): Boolean;
var
  txt: TTextfile; G,_G: GOrient;
  ed: Double; rc: Integer;
begin
  Result:=false; rc:=0;

  txt:=TTextfile.Create;
  try
    if txt.Open_ext(Path,'.TXT') then
    if txt.xStrLine <> nil then
    if txt.x_Double(ed) then

    while txt.xStrLine <> nil do
    if txt.x_Gauss(G[rc]) then begin
      Inc(rc); if rc = 4 then begin
        _G[0]:=G[1]; _G[1]:=G[2];
        _G[2]:=G[3]; _G[3]:=G[0];
        link_bmp(Nx,Ny,@_G);
        Result:=true; Break
      end
    end
  finally
    txt.Free
  end
end;

function link_rel(Path: PChar): Boolean;
var
  h: Integer; g: GOrient;
  hdr: TRelHeader;
begin
  Result:=false;

  if this_ext(Path,'.REL') then begin
    h:=FileOpen(StrPas(Path),fm_OpenRead);
    if h > 0 then begin
      if this_rlf(h,hdr) then
      with hdr do begin
        G[0].x:=xmax; G[0].y:=ymin;
        G[1].x:=xmax; G[1].y:=ymax;
        G[2].x:=xmin; G[2].y:=ymax;
        G[3].x:=xmin; G[3].y:=ymin;

        link_bmp(Nx,Ny,@G); Result:=true
      end;

      FileClose(h)
    end
  end
end;

function link_rsw(Path: PChar): Boolean;
var
  i,h,iw,ih: int; l,g: GOrient; rsw: trswHdr;
  mpp,x1,y1,x2,y2: double;
begin
  Result:=false;

  if this_ext(Path,'.RSW') then begin
    h:=winOpenReadFile(Path);
    if h > 0 then begin
      xFileSeek(h,0);
      FileRead(h,rsw,Sizeof(rsw));

      if rsw.width > 1 then
      if rsw.height > 1 then begin

        mpp:=rsw.mpp;
        iw:=rsw.width;
        ih:=rsw.height;

        x1:=rsw.XPos; x2:=x1+ih*mpp;
        y1:=rsw.YPos; y2:=y1+iw*mpp;

        l[0]:=_Gauss(0,0);   g[0]:=_Gauss(x2,y1);
        l[1]:=_Gauss(iw,0);  g[1]:=_Gauss(x2,y2);
        l[2]:=_Gauss(iw,ih); g[2]:=_Gauss(x1,y2);
        l[3]:=_Gauss(0,ih);  g[3]:=_Gauss(x1,y1);

        for i:=0 to 3 do
        add_ab(l[i],g[i]);

        if rsw.prj = 1 then begin
          fsys:=sys_ru42;
          fsys.prj:=1;
          fsys.lc:=rsw.lc;

          if eps_Equal(fsys.lc,0,1E-6) then
          fsys.lc:=Gauss_Longitude(g[0].y)
        end;

        Result:=true
      end;

      FileClose(h)
    end
  end
end;

function link_jgw(Path: PChar; nx,ny: Integer): Boolean;
var
  i: int; L: LOrient; G: GOrient;
  t: TGauss; kp: double; tr: Real3x3;
  IsGeo: bool; fn: TShortstr;
begin
  Result:=false; StrCopy(fn,'');

  if This_Ext(Path,'.jpg') then
    StrUpdateExt(fn,Path,'.jgw')
  else
  if This_Ext(Path,'.tif') then
    StrUpdateExt(fn,Path,'.tfw');

  if Strlen(fn) > 0 then
  if FileExist(fn) then

  if load_jgw(fn,tr,@fsys) then begin
    Port_to_LPoly(0,0,nx+1,ny+1, @L);

    kp:=Pi/180; IsGeo:=false;
    if fsys.prj = prj_geo then begin
      fsys.prj:=3; IsGeo:=true
    end;

    for i:=0 to 3 do begin
      with L[i] do t:=Transit_3x3(x,y,tr);

      if IsGeo then
      sys_BL_XY(t.x*kp,t.y*kp, fsys, t.x,t.y);

      G[i]:=t
    end;

    for i:=0 to 3 do with L[i] do
    add_xy(x,y,G[i]);

    fis_3x3:=true; fMatrix:=tr;
    Result:=true
  end
end;

function link_geotiff(Path: PChar): Boolean;
var
  tiff: TGeoTiff; tr: Real3x3;
  i: int; tp: PTiepoints; kp: double;
  a,b: TGauss; L: GOrient; IsGeo: bool;
begin
  Result:=false; 

  tiff:=TGeoTiff.Create;
  try
    if tiff.Open(Path) then begin

      fsys:=tiff.sys;
      kp:=Pi/180; IsGeo:=false;
      if fsys.prj = prj_geo then begin
        fsys.prj:=3; IsGeo:=true
      end;

      if tiff.Points.Count = 0 then begin
        tr:=tiff.Matrix; xy_swap_3x3(tr);

        fis_3x3:=not IsGeo; fMatrix:=tr;

        Frame_to_GOrient(0,0,tiff.Width,tiff.Height,@L);

        for i:=0 to 3 do begin
          a:=L[i]; b:=Transit_3x3(a.x,a.y,tr);
          if IsGeo then
          sys_BL_XY(b.x*kp,b.y*kp, fsys, b.x,b.y);
          add_ab(a,b)
        end;

        Result:=true
      end
      else begin
        tp:=tiff.Points.First;
        for i:=0 to tiff.Points.Count-1 do begin
          with tp[0].a do a:=_Gauss(x,y);
          with tp[0].b do b:=_Gauss(y,x);

          if IsGeo then
          sys_BL_XY(b.x*kp,b.y*kp, fsys, b.x,b.y);

          add_ab(a,b); tp:=@tp[1]
        end;

        Result:=true
      end;

    end

  finally
    tiff.Free
  end
end;

function link_sid(Path: PChar): Boolean;
var
  i: Integer;
  sz: TPoint; tr: Real3x3;
  L: LOrient; g: TGauss;
begin
  Result:=false;

  if Get_sid_geo(Path,sz,tr) then begin

    xy_Swap_3x3(tr);

    Port_to_LPoly(0,0,sz.X,sz.Y,@L);

    for i:=0 to 3 do
    with L[i] do begin
      g:=Transit_3x3(x,y,tr);
      add_xy(x,y,g);
    end;

    fis_3x3:=true; fMatrix:=tr;
    Result:=true
  end
end;

function link_wms(Path: PChar): Boolean;
var
  wms: ITileImage; res: Double;
  inf: ZOrient; tr: Real3x3;
  i: Integer; sz: TPoint;
  L: LOrient; g: TGauss;
begin
  Result:=false;

  if GetWmsInterface(Path,ITileImage,wms) = S_OK then
  if wms.GetInfo(@inf) = 5 then begin

    res:=wms.GetSys(@fsys);
    if fsys.prj > 0 then fsys.pps:=1; 

    tr:=nil_3x3;
    tr[1,2]:=-res; tr[1,3]:=fsys.x0;
    tr[2,1]:=+res; tr[2,3]:=fsys.y0;
    tr[3,3]:=1; fsys.x0:=0; fsys.y0:=0;

    Port_to_LPoly(0,0,inf[0],inf[1],@L);

    for i:=0 to 3 do
    with L[i] do begin
      g:=Transit_3x3(x,y,tr);
      add_xy(x,y,g);
    end;

    fis_3x3:=true; fMatrix:=tr;
    Result:=true
  end
end;

function open_txt: Boolean;
var
  txt: TTextfile; s1,s2: tsys; p: TPoint;
  r: TLinkPoint; kp: Double; key: TShortstr;
begin
  Result:=false; kp:=Pi/180;

  txt:=TTextfile.Create;
  try
    if txt.Open(vm_Path) then

    if txt.xStrLine <> nil then
    if sysToken(txt.str,s1) >= 0 then

    if txt.xStrLine <> nil then
    if sysToken(txt.str,s2) >= 0 then begin

      forg:=s1; fsys:=s2;

      if (fsys.pps = 0) and (fsys.prj > 0) then
      fsys.pps:=1;

      while txt.xStrLine <> nil do

      if txt.str[0] = '%' then begin

        if txt.x_str(key) <> nil then

      end
      else begin
        Fillchar(r,Sizeof(r),0);

        if txt.x_Gauss(r.a) then
        if txt.x_Gauss(r.b) then begin

          if fsys.pps = 1 then
          if fsys.prj in [prj_deg,prj_geo] then
          with r.b do sys_BL_XY(x*kp,y*kp,fsys,x,y);

          txt.x_Int(r.tag); Add(@r);
        end;
      end;

      Result:=Count > 0
    end;
  finally
    txt.Free
  end
end;

procedure Reset_off;
var
  i: Integer; lp: PLinkArray;
begin
  lp:=First;
  for i:=0 to Count-1 do lp[i].tag:=0
end;

procedure sys_to_gk;
var
  p: PLinkPoint; lc: double;
begin
  if fsys.prj = 0 then
  if fsys.elp <= 1 then begin
    p:=First; if Assigned(p) then
    if gk_projection(p.b.x,p.b.y,1,lc) then begin
      fsys.prj:=1; fsys.elp:=1; fsys.lc:=lc;
      fsys.dat:=ru42_Datum
    end
  end
end;

var
  i,flags: int; obj: pdmwLink;
  ext: TShortstr; en: Boolean;
begin
  Result:=false; Close_link;

  en:=fUndo.Enabled; fUndo.Enabled:=false;

  Fillchar(fsys,Sizeof(fsys),0);

  fMatrix:=Identify_3x3;
  fis_3x3:=false;

  StrCopy(vm_Tiff,'');

  if StrLen(Path) > 0 then begin

    fImageSize:=Point(nx,ny);

    StrUpdateExt(vm_Path,Path,'.@@@');
    StrPCopy(ext,ExtractFileExt(StrPas(Path)));

    if StrIComp(ext,'.tab') = 0 then
      Result:=link_tab(Path, 0,0)
    else
    if StrIComp(ext,'.map') = 0 then
      Result:=link_ozi(Path, 0,0)
    else
    if StrIComp(ext,'.rsw') = 0 then
      Result:=link_rsw(Path)
    else begin
      if (nx = 0) or (ny = 0) then
      im_dll_bmp(Path,nx,ny,flags);

      if (nx = 0) or (ny = 0) then begin

        if FileExist(vm_Path) then
        Result:=open_txt

      end else

      if StrIComp(ext,'.RLZ') = 0 then
        Result:=link_rlz(Path, nx,ny)
      else begin

        if FileExist(vm_Path) then
        Result:=open_txt;

        if not Result then begin

          for i:=0 to fPlugin.Count-1 do begin
            obj:=fPlugin[i] as pdmwLink;
            if obj.Open(Path, nx,ny) > 0 then begin
              Result:=true; Break
            end
          end;

          if not Result then begin
            Result:=link_tab(Path, nx,ny);

            if not Result then
            if StrIComp(ext,'.TIF') = 0 then 
              Result:=link_geotiff(Path)
            else
            if StrIComp(ext,'.SID') = 0 then
              Result:=link_sid(Path)
            else
            if StrIComp(ext,'.XML') = 0 then
              Result:=link_wms(Path)
          end;

          if not Result then begin
            if (nx > 0) and (ny > 0) then begin

              if StrIComp(ext,'.txt') = 0 then
                Result:=link_fot(Path, nx,ny)
              else
                Result:=link_lg(Path, nx,ny) or
                        link_xy(Path, nx,ny) or
                        link_txt(Path, nx,ny) or
                        link_rel(Path) or
                        link_jgw(Path, nx,ny) or
                        link_ozi(Path,nx,ny);
            end else

            Result:=link_tab(Path, 0,0) or
                    link_jgw(Path, 0,0) or
                    link_ozi(Path, 0,0);
          end;

          Reset_off;
        end
      end
    end;

    Result:=Refresh
  end;

  if wait_gk then sys_to_gk;

  fsys1:=fsys;

  fEditFlag:=false; Change;
  fUndo.Enabled:=en
end;

function tdmwLink.SaveAs_link(Path: PChar): Boolean;
var
  upd: Boolean; fn: TShortStr;
begin
  upd:=fEditFlag; StrCopy(fn,vm_Path);
  fEditFlag:=true; StrCopy(vm_Path,Path);
  Result:=Save_link; fEditFlag:=upd;
  StrCopy(vm_Path,fn);
end;

function tdmwLink.SaveAs_work(Dest: PChar): Boolean;
begin
  StrWorkPath(Dest,'lnk.@@@');
  Result:=SaveAs_link(Dest)
end;

procedure tdmwLink.SaveAs_text(Dest,Pcx: PChar; im_sys: psys);

procedure SaveAs_lnk(Dest: PChar; sys: psys; ab: Boolean);
var
  txt: TTextWrite;
  i,m: Integer; lp: PLinkArray;
  p,g: TGauss; r: TGeoid; pt: TLinkPoint;
  sx1,sy1, sx2,sy2, s: String;
begin
  txt:=TTextWrite.Create;
  try
    if Count > 0 then
    if txt.New(Dest) then begin

      with sys^ do begin
        s:=Format('%d %d %d %d',[Count,pps,prj,elp]);
        sx1:=xAngleStr(b1); sx2:=xAngleStr(b2); sy1:=xAngleStr(lc);
        if (pps = 1) and (prj > 2) then
        s:=s+' '+sx1+' '+sx2+' '+sy1
      end;

      txt.WriteStr(s);

      m:=2; if sys.pps = 1 then m:=7;

      lp:=First;
      for i:=0 to Count-1 do begin pt:=lp[i];

        if ab then begin
          p:=pt.a; g:=pt.b
        end
        else begin
          p:=pt.b; g:=pt.a
        end;

        str(p.x:0:2,sx1);
        str(p.y:0:2,sy1);

        if sys.pps = 1 then begin
          prj_XY_BL(g.x,g.y, sys^, r.b,r.l);
          g.x:=r.b*180/Pi; g.y:=r.l*180/Pi
        end;

        str(g.x:0:m,sx2);
        str(g.y:0:m,sy2);

        s:=sx1+' '+sy1+' '+sx2+' '+sy2;
        if pt.tag <> 0 then s:=s+' '+IntToStr(pt.tag);

        txt.WriteStr(sx1+' '+sy1+' '+sx2+' '+sy2);
      end
    end
  finally
    txt.Free
  end
end;

begin
  if Count > 0 then

  if This_Ext(Dest,'.TAB') then
    SaveAs_tab(Dest,Pcx)
  else
  if Assigned(im_sys) then
    SaveAs_lnk(Dest,im_sys,false)
  else
    SaveAs_lnk(Dest,@fsys,true);
end;

procedure tdmwLink.SaveAs_tab(Dest,Pcx: PChar);
var
  txt: TTextWrite;
  i,m: Integer; lp: PLinkArray;
  g: tgauss; r: tgeoid; g_mk,g_dy: double;
  g_sys: tsys; is_bl,is_gk: Boolean;
  sx1,sy1, sx2,sy2, s: String;
begin
  txt:=TTextWrite.Create;
  try
    if Count > 0 then
    if txt.New(Dest) then begin

      is_bl:=fsys.pps = 1; is_gk:=false;
      if is_bl then is_gk:=fsys.prj in [0,1,2];
      if is_gk then is_bl:=false;

      g_mk:=1; g_dy:=0; g_sys:=fsys;
      if not g_sys.elp in [1,9] then begin
        g_sys.elp:=9; g_sys.dat:=nil_Datum7
      end;

      if is_gk then begin
        g_sys.lc:=zLongitude(g_sys.lc);
        g_dy:=xZone(g_sys.lc);

        if g_sys.elp = 9 then begin
          g_sys.prj:=2; g_mk:=0.9996; g_dy:=500000
        end
      end;

      txt.WriteStr('!table');
      txt.WriteStr('!version 300');
      txt.WriteStr('!charset WindowsCyrillic');
      txt.WriteStr('');

      if Assigned(Pcx) then s:=xStrNameExt(Pcx) else
      if Strlen(vm_Tiff) > 0 then s:=StrLPas(vm_Tiff,255)
      else s:=ChangeFileExt(xStrNameExt(vm_Path),'.TIF');

      txt.WriteStr('Definition Table');
      txt.WriteStr(Format('  File "%s"',[s]));
      txt.WriteStr('Type "RASTER"');

      lp:=First;
      for i:=1 to Count do begin

        g:=lp[0].b; m:=2;

        if is_gk then begin
          g:=prj_to_lg(g.x,g.y, fsys,g_sys);
          g.y:=g.y + g_dy;
        end else

        if fsys.pps = 1 then begin
          prj_XY_BL(g.x,g.y, fsys, g.x,g.y);
          g.x:=g.x*180/Pi; g.y:=g.y*180/Pi;

          if fsys.elp <> g_sys.elp then begin
            r:=tgeoid(g); s_BL_BL(r, fsys,g_sys, tgeoid(g))
          end; m:=6
        end;

        str(g.x:0:m,sx1);
        str(g.y:0:m,sy1);

        sx2:=IntToStr(Round(lp[0].a.x));
        sy2:=IntToStr(Round(lp[0].a.y));

        s:=Format('  (%s,%s) (%s,%s) Label "Pt %d"',
                  [sy1,sx1,sx2,sy2,i]);

        if i < Count then s:=s+',';
        txt.WriteStr(s);

        lp:=@lp[1]
      end;

      if g_sys.elp = 1 then g_sys.elp:=1001
      else g_sys.elp:=104;

      if is_bl then begin
        s:=Format('CoordSys Earth Projection 1, %d',[g_sys.elp]);
        txt.WriteStr(s); txt.WriteStr('Units "degree"')
      end else
      if is_gk then begin
        m:=4; if g_sys.elp = 1001 then m:=0;
        s:=Format('CoordSys Earth Projection 8, %d, "m", %s, 0, %s, %s, 0',
        [g_sys.elp,DegreeStr(g_sys.lc),RealToStr(g_mk,m),RealToStr(g_dy,-1)]);
        txt.WriteStr(s); txt.WriteStr('Units "m"')
      end
      else begin
        txt.WriteStr('CoordSys NonEarth Units "m"');
        txt.WriteStr('Units "m"');
      end
    end
  finally
    txt.Free
  end
end;

procedure tdmwLink.Close_link;
begin
  Save_link; Clear; Change
end;

function tdmwLink.Save_link: Boolean;
var
  txt: TTextWrite;
  i,m: Integer; lp: PLinkArray;
  p1,p2: TGauss; str: TShortstr;
  s: String;
begin
  Result:=true;

  if fEditFlag then
  if StrLen(vm_Path) > 0 then begin

    if fIsBak then
    if FileExist(vm_Path) then begin
      StrBackupFName(str,vm_Path);
      FileCopy(vm_Path,str)
    end;

    Result:=false;

    txt:=TTextWrite.Create;
    try
      if txt.New(vm_Path) then begin

        if forg.pps < 0 then forg.pps:=0;
        StrSys(str,-1,forg); txt.WriteLn(str);
        StrSys(str,-1,fsys); txt.WriteLn(str);

        lp:=First;
        for i:=1 to Count do begin

          p1:=lp[0].a; p2:=lp[0].b; m:=3;

          if fsys.pps = 1 then
          if fsys.prj in [prj_deg,prj_geo] then begin
            with p2 do sys_XY_BL(x,y,fsys,x,y);
            p2.x:=p2.x*180/Pi; p2.y:=p2.y*180/Pi;
            m:=6
          end;

          s:=RealToLStr(p1.x,15,3)+
             RealToLStr(p1.y,15,3)+
             RealToLStr(p2.x,15,m)+
             RealToLStr(p2.y,15,m);

          txt.WriteStr(s); lp:=@lp[1]
        end;

        fEditFlag:=false; Result:=true
      end;
    finally
      txt.Free
    end
  end
end;

procedure tdmwLink.bit_scale(k: Integer);
var
  i: Integer; lp: PLinkArray;
begin
  lp:=First;
  for i:=1 to Count do begin

    with lp[0].a do begin
      x:=x * k; y:=y * k;
    end;

    lp:=@lp[1]
  end;

  Refresh
end;

procedure tdmwLink.Transit_geo(Asys: psys; const tr: Real3x3);
var
  i: Integer; lp: PLinkArray;
begin
  if Asys = nil then fsys:=sys_nil
  else fsys:=Asys^; fsys1:=fsys;

  lp:=First; for i:=0 to Count-1 do
  with lp[i] do b:=Transit_3x3(b.x,b.y,tr);

  fEditFlag:=true
end;

procedure tdmwLink.sm_transform(const T: AB_Orient);
var
  i: Integer; lp: PLinkArray; g: tgauss;
begin
  if Count > 0 then begin

    Push_sys(T.sys);

    lp:=First;
    for i:=0 to Count-1 do begin

      g:=lp[i].b;

      if T.x_fast then
        g:=T.x_ab.Transit(g.x,g.y)
      else
        g:=T.q_ab.Transit(g.x,g.y);

      lp[i].b:=g
    end;

    fEditFlag:=true
  end
end;

procedure tdmwLink.xRefresh;
begin
  if fEditCount > 0 then Refresh;
end;

function tdmwLink.Refresh: Boolean;

function Verify_sm_ed: Integer;

function ed_verify(ed: Integer): Boolean;
var
  i: Integer; lp: PLinkArray;
begin
  Result:=true; lp:=First;
  for i:=1 to Count do begin
    with lp[0] do
    if not verify_val(a.x,ed)
    or not verify_val(a.y,ed)
    or not verify_val(b.x,ed)
    or not verify_val(b.y,ed) then
    begin Result:=false; Break end;

    lp:=@lp[1]
  end
end;

var
  i: Integer; lp: PLinkArray;
  p1,p2: TPoint;
begin
  fsm_ed:=1; fEditCount:=0;

  if ed_verify(10) then fsm_ed:=10;

  if Count > 0 then begin

    lp:=First;
    for i:=0 to Count-1 do begin
      with lp[i] do begin
        p1.X:=Round(a.x);
        p1.Y:=Round(a.y);
        p2.X:=Round(b.x*sm_ed);
        p2.Y:=Round(b.y*sm_ed)
      end;

      if i = 0 then begin
        fbit_lt:=p1; fbit_rb:=p1;
        fsm_lt:=p2; fsm_rb:=p2
      end
      else begin
        Max_LPort(fbit_lt,fbit_rb,p1);
        Max_LPort(fsm_lt,fsm_rb,p2)
      end
    end
  end;

  Result:=Count
end;

function im_Indexof(Ind: Integer;
                    const p: TGauss): Integer;
var
  lp: PLinkArray;
begin
  Result:=-1; lp:=Items[Ind];

  while Ind < Count do begin

    if Gauss_Dist(p,lp[0].a) < 2 then begin
      Result:=Ind; Break
    end;

    Inc(Ind); lp:=@lp[1]
  end
end;

function cls_points: Integer;
var
  i,j: Integer; lp: PLinkArray;
begin
  i:=0; lp:=First;
  while i < Count-1 do begin

    j:=im_Indexof(i+1,lp[0].a);

    if j > i then begin
      Delete(j); fEditFlag:=true
    end
    else begin
      Inc(i); lp:=@lp[1]
    end
  end;

  if fPointIndex > 0 then
  fPointIndex:=0;

  Result:=Count
end;

procedure Affine_min_sqr;
var
  tr: AB_Orient; lp: PLinkArray;
  ag,bg: PGPoly; a,b,a1,b1: tgauss;
  i,len: Integer; s_bit,s_map: double;
  av,bv: GOrient;
begin
  lp:=First;
  if Count >= 2 then begin

    if Count <= 3 then begin

      av[0]:=lp[0].a;
      bv[0]:=lp[0].b;

      av[1]:=lp[1].a;
      bv[1]:=lp[1].b;

      if Count = 3 then begin
        av[2]:=lp[2].a;
        bv[2]:=lp[2].b;
      end;

      tr.x_ab.ab_Open(@av,@bv,Count);
      tr.x_ba.ab_Open(@bv,@av,Count);
      tr.x_ab.Get_Matrix(fForw_t);
      tr.x_ba.Get_Matrix(fBack_t);
    end
    else begin

      len:=Count * SizeOf(tgauss);

      ag:=xAllocPtr(len+len);
      if Assigned(ag) then bg:=@ag[Count];

      for i:=0 to Count-1 do begin

        a:=lp[i].a; b:=lp[i].b;

        if i <= 3 then begin
          tr.ag[i]:=a; tr.bg[i]:=b
        end;

        if Assigned(ag) then begin
          ag[i]:=a; bg[i]:=b;
        end
      end;

      tr.x_Prepare;
      tr.x_ab.Get_Matrix(fForw_t);
      tr.x_ba.Get_Matrix(fBack_t);

      if ag <> nil then
      if bg <> nil then
      if Count > 3 then begin

        tr.x_ab.Open_Min_Sqr(ag,bg,Count);
        tr.x_ba.Open_Min_Sqr(bg,ag,Count);
        tr.x_ab.Get_Matrix(fForw_t);
        tr.x_ba.Get_Matrix(fBack_t);

        if ftri.Count > 0 then begin

          s_bit:=0; s_map:=0;

          for i:=0 to Count-1 do begin
            a:=lp[i].a; b:=lp[i].b;

            b1:=Transit_3x3(a.x,a.y,fForw_t);
            s_map:=Max(s_map,Gauss_Dist(b,b1));

            a1:=Transit_3x3(b.x,b.y,fBack_t);
            s_bit:=Max(s_bit,Gauss_Dist(a,a1))
          end;

          fBit_fast:=s_map <= sm_eps;
          fMap_fast:=s_bit <= im_eps
        end
      end;

      xFreePtr(ag);
    end
  end
end;

function Link_pcx: Boolean;
var
  pp: ITransit2;
  i: Integer; lp: PLinkArray;
  tr,bt: Real3x3; m: ZValues;
begin
  Result:=false; fEnabled:=false;

  fForw_t:=Identify_3x3;
  fBack_t:=fForw_t;

  fBit_fast:=true; fMap_fast:=true;

  lp:=First;
  if Count > 0 then begin

    with lp[0] do begin
      Init_3x3(tr,-a.x,a.y,1,-1);
      xy_swap_3x3(tr); t_Move_3x3(tr,b.x,b.y);
      fForw_t:=tr; Inverse_3x3(tr,fBack_t)
    end;

    if Count > 1 then

    if Count <= 3 then
      Affine_min_sqr
    else
    if GetPMathIntf(ITransit2,pp) = S_OK then begin

      for i:=0 to Count-1 do
      with lp[i] do pp.Add(@a,@b);

      if pp.CalcAffine(xy_Affine6,@tr,@bt) then begin

        fForw_t:=tr; fBack_t:=bt; pp.GetError(@m);

        if ftri.Count > 0 then begin
          fBit_fast:=m[0] <= sm_eps;
          fMap_fast:=m[0] <= im_eps;
        end
      end
    end
    else Affine_min_sqr; pp:=nil;

    fEnabled:=true; Result:=true
  end;

  ftri.SetAffine(fForw_t,fBack_t);
end;

var
  i: Integer; lp: PLinkArray;
begin
  Result:=false;

  cls_points; Verify_sm_ed;

  ftri.Clear; lp:=First;

  for i:=0 to Count-1 do
  with lp[i] do if tag >= 0 then
  ftri.xAdd(a,b);

  ftri.Calc_tri; Result:=Link_pcx
end;

function tdmwLink.Get_tri_list(List: TTriList): Integer;
begin
  List.LoadLink(ftri);
  List.Assign_3x3(fForw_t,fBack_t);
  Result:=List.Count
end;

procedure tdmwLink.Assign_bit_fast(const T: Real3x3);
begin
  fBit_fast:=true; fForw_t:=T
end;

procedure tdmwLink.Assign_map_fast(const T: Real3x3);
begin
  fMap_fast:=true; fBack_t:=T
end;

function tdmwLink.bit_IndexOf(const p: TGauss; eps: Float): Integer;
var
  i: Integer; lp: PLinkArray;
begin
  Result:=-1; lp:=First;

  for i:=1 to Count do begin
    with lp[0].a do
    if Abs(x-p.x) <= eps then
    if Abs(y-p.y) <= eps then begin
      Result:=i; Break
    end; lp:=@lp[1]
  end
end;

function tdmwLink.add_ab(const a,b: tgauss): Integer;
var
  r: TLinkPoint;
begin
  Fillchar(r,Sizeof(r),0);
  r.a:=a; r.b:=b; Result:=Add(@r);
  fEditFlag:=true;

  if Result >= 0 then
  if fUndo.Enabled then
  fUndo.Push(Result,1);
end;

function tdmwLink.add_xy(x,y: Integer; const g: tgauss): Integer;
var
  p: TGauss;
begin
  p.x:=x; p.y:=y; Result:=Add_ab(p,g)
end;

procedure tdmwLink.add_sm(const v: LVector);
var
  a,b: TGauss;
begin
  a.x:=v[0].X; a.y:=v[0].Y;
  b.x:=v[1].X / sm_ed;
  b.y:=v[1].Y / sm_ed;
  Add_ab(a,b); Reset_link;
end;

function tdmwLink.add_2g(const g: GVector): Boolean;
begin
  Result:=false;
  if bit_IndexOf(g[0],1) <= 0 then
  Result:=Add_ab(g[0],g[1]) >= 0
end;

function tdmwLink.New_ab(const a,b: TGauss): Boolean;
begin
  Add_ab(a,b); Reset_link;
  Result:=Refresh; Change;
end;

function tdmwLink.Mov_ab(Ind: Integer; const a,b: tgauss): Boolean;
var
  p: PLinkPoint;
begin
  Result:=false;

  p:=Items[Ind];
  if Assigned(p) then begin

    if fUndo.Enabled then
    fUndo.Push(Ind,0);

    p.a:=a; p.b:=b; Changed;
    Result:=true
  end
end;

function tdmwLink.Del_ab(Ind: Integer): Boolean;
begin
  Result:=false; Dec(Ind);
  if (Ind >= 0) and (Ind < Count) then begin

    if fUndo.Enabled then
    fUndo.Push(Ind,2);

    if Delete(Ind) >= 0 then begin
      Changed; Result:=true
    end
  end
end;

function tdmwLink.Set_projective_3x3: Boolean;
var
  i: Integer; lp: PLinkArray;
  a,b: GOrient; t1,t2: Real3x3;
begin
  Result:=false;

  if Count > 3 then begin

    if Count = 4 then begin
      lp:=First;
      for i:=0 to 3 do begin
        a[i]:=lp[i].a; b[i]:=lp[i].b;
      end;
    end
    else begin
      Frame_to_GOrient(fbit_lt.X,fbit_lt.Y,
                       fbit_rb.X-fbit_lt.X+1,
                       fbit_rb.Y-fbit_lt.Y+1,
                       @a);

      for i:=0 to 3 do
      pix_to_xy(a[i].x,a[i].y,b[i]);
    end;

    if Solve_projective1(@a,@b,t1,t2) then begin
      fForw_t:=t1; fBit_fast:=true;
      fBack_t:=t2; fMap_fast:=true;
      Result:=true
    end
  end
end;

function tdmwLink.Set_xml(w,h: int; gb: PGPoly; const gs: tsys): Boolean;
var
  i: int; l,g: GOrient; t1,t2: Real3x3;
begin
  Result:=false; Dec(w); Dec(h);

  l[0].x:=0; l[0].y:=0;
  l[1].x:=w; l[1].y:=0;
  l[2].x:=w; l[2].y:=h;
  l[3].x:=0; l[3].y:=h;

  for i:=0 to 3 do g[i]:=gb[i];

  if Solve_projective1(@l,@g,t1,t2) then begin
    fForw_t:=t1; fBit_fast:=true;
    fBack_t:=t2; fMap_fast:=true;
    Result:=true
  end
end;

function tdmwLink.bit_to_sm(const a: TPoint; out b: TPoint): Boolean;
var
  g: tgauss;
begin
  Result:=pix_to_xy(a.X,a.Y,g);
  b.X:=Round(g.x*sm_ed);
  b.Y:=Round(g.y*sm_ed)
end;

function tdmwLink.sm_to_bit(const a: TPoint; out b: TPoint): Boolean;
var
  p,g: tgauss;
begin
  g.x:=a.X / sm_ed;
  g.y:=a.Y / sm_ed;
  Result:=xy_to_pix(g,p);
  b.X:=Round(p.x); b.Y:=Round(p.y)
end;

function tdmwLink.pix_to_xy(x,y: Double; out g: tgauss): Boolean;
begin
  Result:=true;

  if fBit_fast then
    g:=Projective_3x3(x,y,fForw_t)
  else
    Result:=ftri.a_to_b(x,y,g)
end;

function tdmwLink.xy_to_pix(const g: tgauss; out p: tgauss): Boolean;
begin
  Result:=true;
  if fMap_fast then
    p:=Projective_3x3(g.x,g.y,fBack_t)
  else
    Result:=ftri.b_to_a(g.x,g.y,p)
end;

function tdmwLink.l_to_xy(const l: TPoint; out g: tgauss): Boolean;
begin
  Result:=pix_to_xy(l.X,l.Y,g)
end;

function tdmwLink.xy_to_l(const g: tgauss; out l: TPoint): Boolean;
var
  p: TGauss;
begin
  Result:=xy_to_pix(g,p);
  l.X:=Round(p.x); l.Y:=Round(p.y)
end;

function tdmwLink.l_to_g(x,y: Double; out g: xgeoid): Boolean;
var
  p,t: tgauss;
begin
  Result:=false;
  Fillchar(g,Sizeof(g),0);

  if fEnabled then begin
    Result:=pix_to_xy(x,y,t);
    g:=prj_xy_to_x(t,fsys);
    Result:=true
  end
end;

function tdmwLink.g_to_p(const g: xgeoid; out p: tgauss): Boolean;
var
  t: tgauss; tz: double;
begin
  Result:=false; p.x:=0; p.y:=0;

  if fEnabled then begin
    t.x:=g.x; t.y:=g.y;

    if g.s.pps = 1 then begin
      if fsys.prj = 0 then Default_sys;

      if g.s.elp <> fsys.elp then
      GEO_GEO(t.x,t.y,0,g.s.elp,fsys.elp,
              @g.s.dat,@fsys.dat, t.x,t.y,tz);

      prj_BL_XY(t.x,t.y, fsys, t.x,t.y)
    end;

    Result:=xy_to_pix(t,p)
  end
end;

function tdmwLink.g_to_l(const g: xgeoid; out l: TPoint): Boolean;
var
  p: tgauss;
begin
  Result:=g_to_p(g,p);
  l.X:=Round(p.x); l.Y:=Round(p.y)
end;

function tdmwLink.bit_Bound_Contains(const p: TPoint): Boolean;
begin
  Result:=false;
  if (p.x >= bit_lt.x) and (p.y >= bit_lt.y) then
  if (p.x <= bit_rb.x) and (p.y <= bit_rb.y) then
  Result:=true
end;

function tdmwLink.sm_Bound_Contains(const p: TPoint): Boolean;
begin
  Result:=false;
  if (p.x >= sm_lt.x) and (p.y >= sm_lt.y) then
  if (p.x <= sm_rb.x) and (p.y <= sm_rb.y) then
  Result:=true
end;

function tdmwLink.ContainsGauss(const g: xgeoid): Boolean;
var
  p: TPoint;
begin
  Result:=false; if g_to_l(g,p) then
  Result:=PortContainsPoint(bit_lt,bit_rb, p.x,p.y)
end;

function tdmwLink.This_Pair(const lt,rb: TGauss;
                            who: Integer; out p: TGauss): Integer;
var
  i: Integer; lp: PLinkArray; t: TGauss;
begin
  Result:=0; p.x:=0; p.y:=0;

  lp:=First;
  for i:=1 to Count do begin

    if who = 0 then t:=lp[0].a
               else t:=lp[0].b;

    if (t.x >= lt.x) and (t.x <= rb.x) then
    if (t.y >= lt.y) and (t.y <= rb.y) then
    begin Result:=i; p:=t; Break end;

    lp:=@lp[1]
  end
end;

function tdmwLink.Move_Pair(i,who: Integer; const p: TGauss): Boolean;
var
  pp: PLinkPoint;
begin
  Result:=false; pp:=Items[i-1];

  if Assigned(pp) then begin
    if who = 0 then pp.a:=p
               else pp.b:=p;

    Inc(fEditCount);

    fEditFlag:=true;
    Change; Result:=true
  end
end;

function tdmwLink.Optimize(const lt,rb: TGauss): Boolean;
var
  tmp: tdmwLink;
  i: Integer; lp: PLinkArray;
  _lt,_rb: TGauss;
begin
  Result:=false;

  tmp:=tdmwLink.Create;
  try
    Mult_GRect(lt,rb,4,_lt,_rb);

    lp:=First;
    for i:=1 to Count do begin
      if GaussContainsPoint(_lt,_rb, lp[0].b) then
      tmp.Add(@lp[0]); lp:=@lp[1]
    end;

    if tmp.Count >= 4 then begin
      LoadList(tmp); Result:=Refresh
    end
  finally
    tmp.Free
  end
end;

function tdmwLink.Verify(is_ab: Boolean): Boolean;
var
  i: Integer; lp: PLinkArray;
  p1,p2: TGauss;
begin
  Result:=true; lp:=First;

  for i:=1 to Count do begin

    with lp[0] do
    if is_ab then begin
      pix_to_xy(a.x,a.y,p1); p2:=b;
    end
    else begin
      xy_to_pix(b,p1); p2:=a
    end;

    if Gauss_Dist(p1,p2) > 0.9 then begin
      Result:=false; Break
    end;

    lp:=@lp[1]
  end
end;

function tdmwLink.Get_bound(w,h: Integer; G: PGPoly): Double;
var
  i: Integer; pp: PLinkArray;
  L: GOrient; lt,rb: TGauss;
begin
  Result:=-1;

  if Count > 0 then begin

    lt:=_Gauss(0,0); rb:=_Gauss(w,h);

    if (w = 0) or (h = 0) then begin
      pp:=First; lt:=pp[0].a; rb:=lt;

      for i:=1 to Count-1 do
      with pp[i].a do begin
        if x < lt.x then lt.x:=x;
        if x > rb.x then rb.x:=x;
        if y < lt.y then lt.y:=y;
        if y > rb.y then rb.y:=y
      end
    end;

    if rb.x+1 <= lt.x then rb.x:=lt.x+1;
    if rb.y+1 <= lt.y then rb.y:=lt.y+1;

    Bound_to_GOrient(lt,rb,@L);
    for i:=0 to 3 do
    pix_to_xy(L[i].x,L[i].y,G[i]);

    Result:=Get_img_res(@L,G)
  end;
end;

function tdmwLink.Get_resolution(w,h: Integer): Double;
var
  G: GOrient;
begin
  Result:=Get_bound(w,h,@G)
end;

function tdmwLink.Get_geo_mpp(const g: TGeoPoint): double;
var
  b: GOrient; p,g1,g2: TGauss;
begin
  Result:=-1;
  if g_to_p(g,p) then
  Result:=Get_pix_mpp(p.x,p.y);

  if Result < 0 then
  Result:=Get_bound(0,0,@b)
end;

function tdmwLink.Get_pix_mpp(x,y: double): double;
var
  b: GOrient; g1,g2: TGauss;
begin
  Result:=-1;

  if pix_to_xy(x,y,g1) then
  if pix_to_xy(x,y+1,g2) then
  Result:=Gauss_Dist(g1,g2);

  if Result < 0 then
  Result:=Get_bound(0,0,@b)
end;

function tdmwLink.Pickup_res(x,y, w,h: Integer): Double;
var
  g1,g2: TGauss;
begin
  Result:=Get_resolution(w,h);
  if PortContainsPoint(bit_lt,bit_rb, x,y) then
  if pix_to_xy(x-1,y-1,g1) then
  if pix_to_xy(x+1,y+1,g2) then
  Result:=Gauss_Dist(g1,g2) / Hypot(2,2)
end;

function tdmwLink.Get_clip(lp: PLPoly; Scale: Integer): Integer;
var
  i: Integer; pp: PLinkArray;
begin
  Result:=0;

  if Count = 4 then begin

    pp:=First;
    for i:=0 to Count-1 do
    with pp[i].a do begin
      lp[i].X:=Round(x/Scale);
      lp[i].Y:=Round(y/Scale);
    end;

    if poly_quad(lp,4) then begin
      lp[4]:=lp[0]; Result:=4
    end
  end
end;

function tdmwLink.add_bound(x1,y1,x2,y2,k: Double): Boolean;

procedure add_point(x,y: Double);
var
  a,b: TGauss;
begin
  a.x:=x; a.y:=y;
  b:=Transit_3x3(x,y,fForw_t);
  add_ab(a,b); fEditFlag:=false;
end;

var
  dx,dy,xc,yc: Double;
begin
  dx:=(x2-x1)*k;
  dy:=(y2-y1)*k;

  x1:=x1-dx; x2:=x2+dx; xc:=(x1+x2)/2;
  y1:=y1-dy; y2:=y2+dy; yc:=(y1+y2)/2;

  add_point(x1,y1); add_point(xc,y1);
  add_point(x2,y1); add_point(x2,yc);
  add_point(x2,y2); add_point(xc,y2);
  add_point(x1,y2); add_point(yc,x1);
  Result:=Refresh
end;

function txy_to_xy.LoadFrom(Path: PChar): Integer;
var
  txt: TTextFile; v: LVector;
  s: tsys; i,n,max_n: Integer;
  av,bv: PGPoly; c: TGauss;
begin
  Clear;

  Fillchar(s,Sizeof(s),0); max_n:=256;
  av:=xAllocPtr(max_n*2*SizeOf(tgauss));

  txt:=TTextFile.Create;
  try
    if av <> nil then

    if txt.Open(Path) then
    if not txt.End_of_file then

    if txt.Get_int(n) then
    if txt.x_int(s.pps) then
    if txt.x_int(s.prj) then
    if txt.x_int(s.elp) then begin

      txt.x_Double(s.b1); s.b1:=s.b1/180*Pi;
      txt.x_Double(s.b2); s.b2:=s.b2/180*Pi;
      txt.x_Double(s.lc); s.lc:=s.lc/180*Pi;

      if n > 0 then
      if n <= max_n then begin
        if s.pps in [0..1] then
        if s.prj in [0..8] then
        if s.elp >= 0 then begin

          bv:=@av[max_n]; i:=0;

          while not txt.End_of_file do
          if txt.Get_gauss(av[i]) then
          if txt.x_Geoid(s.pps,bv[i]) then begin
            Inc(i); if i = max_n then Break
          end;

          n:=Min(n,i);
          if n >= 2 then begin

            if (s.pps = 1) and (s.prj = 1) then
            s.dat:=ru42_Datum;

            if s.pps = 1 then begin
              for i:=0 to n-1 do with bv[i] do
              begin x:=x*Pi/180; y:=y*Pi/180 end;

              c:=gauss_xCentre(bv,n);

              if s.lc = 0 then s.lc:=zLongitude(c.y);

              for i:=0 to n-1 do with s,bv[i] do
              BL_to_XY(x,y, lc,b1,b2, elp,prj, x,y);
            end;

            for i:=0 to n-1 do
            Add_ab(av[i],bv[i]);

            Refresh
          end
        end
      end
    end;

  finally
    txt.Free
  end;

  xFreePtr(av); Result:=Count
end;

constructor idmwLink.Create(Alnk: tdmwLink; Bmp: PChar);
var
  flags: Integer;
begin
  inherited Create; flnk:=Alnk;
  if Assigned(Bmp) then
  im_dll_bmp(Bmp, fWidth,fHeight,flags);
end;

function idmwLink.Open(Path: PChar): Integer;
var
  flags: Integer;
begin
  Result:=0; flnk.Clear;
  im_dll_bmp(Path, fWidth,fHeight,flags);
  if flnk.Open_link(Path,fWidth,fHeight) then
  Result:=flnk.Count
end;

procedure idmwLink.SaveAs(Path: PChar);
begin
  if This_ext(Path,'.lnk') then
    flnk.SaveAs_text(Path,Path,nil)
  else
    flnk.SaveAs_link(Path)
end;

function idmwLink.GetCount: Integer;
begin
  Result:=flnk.Count
end;

function idmwLink.ImageWidth: Integer;
begin
  Result:=fWidth
end;

function idmwLink.ImageHeight: Integer;
begin
  Result:=fHeight
end;

procedure idmwLink.GetSys(out sys: tsys);
begin
  sys:=flnk.sys
end;

procedure idmwLink.SetSys(sys: psys);
begin
  flnk.sys:=sys^
end;

procedure idmwLink.GetPoint(Ind: Integer; out a,b: TGauss);
var
  p: PLinkPoint;
begin
  p:=flnk[Ind];
  if Assigned(p) then begin
    a:=p.a; b:=p.b
  end
end;

procedure idmwLink.SetPoint(Ind: Integer; a,b: PGauss);
var
  p: PLinkPoint;
begin
  p:=flnk[Ind];
  if Assigned(p) then begin
    p.a:=a^; p.b:=b^; flnk.EditFlag:=true
  end
end;

procedure idmwLink.AddPoint(a,b: PGauss);
begin
  flnk.Add_ab(a^,b^);
end;

procedure idmwLink.l_to_g(ix,iy: Double; out ox,oy: Double);
var
  g: TGauss;
begin
  ox:=0; oy:=0;
  if flnk.pix_to_xy(ix,iy,g) then begin
    ox:=g.x; oy:=g.y
  end
end;

function idmwLink.l_to_r(ix,iy: Double; out ox,oy: Double): Integer;
var
  g: xgeoid;
begin
  Result:=0; ox:=0; oy:=0;
  if flnk.l_to_g(ix,iy,g) then begin

    if g.s.pps = 0 then
    if g.s.prj > 0 then begin
      prj_XY_BL(g.x,g.y,g.s,g.x,g.y);
      g.s.pps:=1;
    end;

    Result:=g.s.pps; ox:=g.x; oy:=g.y;
  end
end;

procedure idmwLink.g_to_l(ix,iy: Double; out ox,oy: Double);
var
  g,p: TGauss;
begin
  ox:=0; oy:=0; g.x:=ix; g.y:=iy;
  flnk.xy_to_pix(g,p); ox:=p.x; oy:=p.y
end;

procedure idmwLink.r_to_l(ix,iy: Double; pps: Integer; out ox,oy: Double);
var
  g: xgeoid; p: tgauss;
begin
  ox:=0; oy:=0;

  g.s:=flnk.sys; g.s.pps:=pps;
  g.x:=ix; g.y:=iy;

  if flnk.g_to_p(g,p) then begin
    ox:=p.x; oy:=p.y
  end
end;

function idmwLink.Containsg(x,y: double): Boolean;
var
  ix,iy: int;
begin
  Result:=false;
  with flnk do
  if (x >= sm_lt.X/sm_ed) then
  if (x <= sm_rb.X/sm_ed) then
  if (y >= sm_lt.Y/sm_ed) then
  if (y <= sm_rb.Y/sm_ed) then
  Result:=true
end;

function load_jgw(Path: PChar;
                  out tr: Real3x3;
                  sys: psys): Boolean;

function txt_double(txt: TTextfile; out v: Double): Boolean;
begin
  Result:=false;
  if txt.xStrLine <> nil then
  Result:=txt.x_Double(v)
end;

var
  txt: TTextfile; s,s1: tsys; g,a,b: tgauss; dy: Double;
begin
  Result:=false;

  tr:=Identify_3x3; s:=sys_nil;

  txt:=TTextfile.Create;
  try
    if txt.Open(Path) then
    if txt_double(txt,tr[2,1]) then
    if txt_double(txt,tr[1,1]) then
    if txt_double(txt,tr[2,2]) then
    if txt_double(txt,tr[1,2]) then
    if txt_double(txt,tr[2,3]) then
    if txt_double(txt,tr[1,3]) then begin

      if txt_double(txt,g.x) and
         txt_double(txt,g.y) and

         (Abs(g.x) < 89) and
         (Abs(g.y) < 170) then begin

        g.x:=g.x/180*Pi;
        g.y:=g.y/180*Pi;

        a.x:=tr[1,3];
        a.y:=tr[2,3] - 500000;

        s1:=sys7(0,2,9,0,0,0);
        s1.lc:=zLongitude(g.y);
        dy:=prj_ZoneY(s1);

        sys_BL_XY(g.x,g.y,s1,b.x,b.y);

        if eps_Equal(a.x,b.x,0.01) then
        if eps_Equal(a.y,b.y,0.01) then begin
          a.y:=a.y + dy; tr[2,3]:=a.y; s:=s1
        end
      end else

      if (Abs(tr[1,3]) < 89) and
         (Abs(tr[2,3]) < 170) and
         (Abs(tr[2,1]) < 1E-3) and
         (Abs(tr[1,2]) < 1E-3) then
        s:=sys7(1,prj_geo,9,0,0,0);

      if Assigned(sys) then sys^:=s;
      Result:=true
    end
  finally
    txt.Free
  end
end;

procedure create_jgw(Dest,Ext: PChar; const tr: Real3x3);
var
  txt: TTextFile; fn: TShortstr;
begin
  StrCopy(fn,Dest);
  if Assigned(Ext) then
  StrUpdateExt(fn,Dest,Ext);

  txt:=TTextFile.Create;
  try
    if txt.Make(fn) then begin
      txt.WriteReal(tr[2,1],6);
      txt.WriteReal(tr[1,1],6);
      txt.WriteReal(tr[2,2],6);
      txt.WriteReal(tr[1,2],6);
      txt.WriteReal(tr[2,3],6);
      txt.WriteReal(tr[1,3],6);
    end
  finally
    txt.Free
  end
end;

function this_FFT(Path: PChar): Boolean;
var
  fft: TShortStr;
begin
  Result:=false;

  if StrLen(Path) > 0 then
  if This_Ext(Path,'.FFT') then
  if This_Text(Path,fft) then
  Result:=StrComp(fft,'FFT') = 0
end;

function open_fft(Pcx,Fft: PChar; const g: xgeoid;
                  scale: Integer; more: Boolean): PChar;
var
  txt: tTextFile; link: tdmwLink;
  dist,sc,tmp: Integer;
  dir,fn,path: TShortStr;
begin
  Result:=nil; dist:=0;

  txt:=tTextFile.Create;
  try
    if FileExist(fft) then
    if this_fft(fft) then

    if txt.Open(Fft) then begin

      link:=tdmwLink.Create;
      try
        StrDirectory(dir,Fft);

        if txt.KeyRead('FFT') then

        while txt.xStrLine <> nil do

        if txt.x_str(fn) <> nil then
        if StrPath(Path,dir,fn) <> nil then begin

          txt.x_Int(sc);
          if (sc > 0) or not more then begin

            tmp:=Abs(sc-scale); if (Result = nil)
            or (sc = 0) or (tmp < dist) then

            if link.Open_link(Path,0,0) then
            if link.Count > 3 then
            if link.ContainsGauss(g) then begin
              Result:=StrCopy(Pcx,Path);
              dist:=tmp; if sc = 0 then Break
            end

          end
        end
      finally
        link.Free
      end
    end;
  finally
    txt.Free
  end
end;

function Strings_fft(List: TStrings; Fft: PChar): Integer;
var
  txt: tTextFile; dir,fn,pcx: TShortStr;
begin
  List.Clear;

  txt:=tTextFile.Create;
  try
    if FileExist(fft) then
    if this_fft(fft) then

    if txt.Open(fft) then
    if txt.KeyRead('FFT') then begin

      StrDirectory(dir,Fft);

      while not txt.End_of_file do
      if txt.StrRead(fn) <> nil then

      if RStr(fn) <> nil then
      if StrPath(pcx,dir,fn) <> nil then
      if FileExist(pcx) then

      List.Add(StrPas(fn));
    end;
  finally
    txt.Free
  end;

  Result:=List.Count
end;

procedure Create_link(l,g: PGPoly; sys: psys; Dest: PChar);
var
  lnk: tdmwLink; fn: TShortstr;
  i: Integer; a,b: TGauss;
begin
  StrUpdateExt(fn,Dest,'.@@@');
  FileErase(fn);

  lnk:=tdmwLink.Create;
  try
    if Assigned(sys) then
    lnk.Push_sys(sys^);

    for i:=0 to 3 do begin
      a:=l[i]; b:=g[i];

      if Assigned(sys) then
      if sys.pps = 1 then
      prj_BL_XY(b.x,b.y, sys^, b.x,b.y);

      lnk.add_ab(a,b)
    end;

    StrCopy(lnk.vm_Path,fn);
    lnk.Save_link;
  finally
    lnk.Free
  end
end;

function get_lg_link(Path: PChar; out lg: lg_Transit): Boolean;
var
  link: TDmwLink; i,w,h,fl: int;
begin
  Result:=false;

  Fillchar(lg,Sizeof(lg),0);
  lg.s.pps:=-1;

  link:=TDmwLink.Create;
  try
    if im_dll_bmp(Path, w,h,fl) then
    if link.Open_link(Path,w,h) then begin
      lg.s:=link.sys;
      lg.l[0]:=_Gauss(0,0);
      lg.l[1]:=_Gauss(w,0);
      lg.l[2]:=_Gauss(w,h);
      lg.l[3]:=_Gauss(0,h);

      for i:=0 to 3 do with lg.l[i] do
      link.pix_to_xy(x,y, lg.g[i]);

      if This_Ext(Path,'.rsw') then
      with lg.g[0] do rsw_projection(lg.s,x,y);

      Result:=true
    end
  finally
    link.Free
  end
end;

function get_lg_link1(Path: PChar; out lg: lg_Transit): Boolean;
var
  link: TDmwLink; i,w,h,fl: int;
  lp: PLinkArray;
begin
  Result:=false;

  Fillchar(lg,Sizeof(lg),0);
  lg.s.pps:=-1;

  link:=TDmwLink.Create;
  try
    if im_dll_bmp(Path, w,h,fl) then
    if link.Open_link(Path,w,h) then begin
      lg.s:=link.sys;

      if link.Count = 4 then begin

        lp:=link.First;
        for i:=0 to 3 do begin
          lg.l[i]:=lp[i].a;
          lg.g[i]:=lp[i].b;
        end;
      end
      else begin
        lg.l[0]:=_Gauss(0,0);
        lg.l[1]:=_Gauss(w,0);
        lg.l[2]:=_Gauss(w,h);
        lg.l[3]:=_Gauss(0,h);

        for i:=0 to 3 do with lg.l[i] do
        link.pix_to_xy(x,y, lg.g[i]);
      end;

      if This_Ext(Path,'.rsw') then
      with lg.g[0] do rsw_projection(lg.s,x,y);

      Result:=true
    end
  finally
    link.Free
  end
end;

function cut_lg_link(Path: PChar; iw,ih,cx,cy,cw,ch: Integer;
                     out lg: lg_Transit): Boolean;
var
  link: TDmwLink; i: Integer;
begin
  Result:=false;

  Fillchar(lg,Sizeof(lg),0);
  lg.s.pps:=-1;

  link:=TDmwLink.Create;
  try
    if link.Open_link(Path,iw,ih) then begin
      lg.s:=link.sys;
      lg.l[0]:=_Gauss(0 ,0);
      lg.l[1]:=_Gauss(cw,0);
      lg.l[2]:=_Gauss(cw,ch);
      lg.l[3]:=_Gauss(0 ,ch);

      for i:=0 to 3 do with lg.l[i] do
      link.pix_to_xy(cx+x,cy+y, lg.g[i]);

      Result:=true
    end
  finally
    link.Free
  end
end;

procedure create_img_link(Dest: PChar; l,g: PGPoly; sys: psys);
var
  link: TDmwLink;
  i: Integer; fn: TShortstr;
begin
  link:=TDmwLink.Create;
  try
    if Assigned(sys) then
    link.sys:=sys^;

    for i:=0 to 3 do
    link.add_ab(l[i],g[i]);

    StrUpdateExt(fn,Dest,'.@@@');
    link.SaveAs_link(fn)
  finally
    link.Free
  end
end;

procedure create_lg_link(Dest: PChar; const lg: lg_Transit);
var
  i: Integer; g: GOrient; s: tsys;
begin
  if lg.s.prj <> prj_deg then
    create_img_link(Dest,@lg.l,@lg.g,@lg.s)
  else begin
    s:=lg.s; s.prj:=prj_geo; g:=lg.g;

    for i:=0 to 3 do with g[i] do begin
      sys_XY_BL(x,y, lg.s, x,y);
      sys_BL_XY(x,y, s, x,y);
    end;

    create_img_link(Dest,@lg.l,@g,@s)
  end
end;

function copy_lg_link(Src,Dest: PChar): bool;
var
  lg: lg_Transit;
begin
  Result:=false;
  if get_lg_link(Src,lg) then begin
    create_lg_link(Dest,lg);
    Result:=xFileExist(Dest,'.@@@')
  end
end;

procedure create_tiff_link(Dest: PChar;
                           w,h: Double; g: PGPoly;
                           const sys: tsys);
var
  lg: lg_Transit;
begin
  Fillchar(lg,Sizeof(lg),0); lg.s:=sys;

  lg.l[0]:=_Gauss(0,0); lg.g[0]:=g[0];
  lg.l[1]:=_Gauss(w,0); lg.g[1]:=g[1];
  lg.l[2]:=_Gauss(w,h); lg.g[2]:=g[2];
  lg.l[3]:=_Gauss(0,h); lg.g[3]:=g[3];

  create_lg_link(Dest,lg)
end;

procedure cut_link(Dest,Path: PChar; iw,ih,cx,cy,cw,ch: Integer);
var
  lnk: tdmwLink; g: GOrient;
begin
  lnk:=tdmwLink.Create;
  try
    if lnk.Open_link(Path,iw,ih) then begin
      lnk.pix_to_xy(cx   ,cy   ,g[0]);
      lnk.pix_to_xy(cx+cw,cy   ,g[1]);
      lnk.pix_to_xy(cx+cw,cy+ch,g[2]);
      lnk.pix_to_xy(cx   ,cy+ch,g[3]);
      create_tiff_link(Dest,cw,ch,@g,lnk.sys);
    end;
  finally
    lnk.Free
  end
end;

function Get_img_res(l,g: PGPoly): Double;
var
  r1,r2: Double;
begin
  r1:=Gauss_dist(g[0],g[2]) / Gauss_dist(l[0],l[2]);
  r2:=Gauss_dist(g[1],g[3]) / Gauss_dist(l[1],l[3]);
  Result:=r1/2 + r2/2
end;

function Get_img_res1(l,g: PGPoly): TGauss;
var
  r1,r2: double;
begin
  r1:=Gauss_dist(g[0],g[1]) / Gauss_dist(l[0],l[1]);
  r2:=Gauss_dist(g[2],g[3]) / Gauss_dist(l[2],l[3]);
  Result.x:=r1/2 + r2/2;

  r1:=Gauss_dist(g[0],g[3]) / Gauss_dist(l[0],l[3]);
  r2:=Gauss_dist(g[1],g[2]) / Gauss_dist(l[1],l[2]);
  Result.y:=r1/2 + r2/2;
end;

function lnk_to_tab(Dest,Path: PChar): Boolean;
var
  lnk: tdmwLink;
begin
  Result:=false;

  lnk:=tdmwLink.Create;
  try
    if lnk.Open_link(Path,0,0) then
    lnk.SaveAs_text(Dest,Path,nil);
  finally
    lnk.Free
  end
end;

procedure copy_link(Dest,Path: PChar; Zoom: Integer);
var
  link: tdmwLink;
  i,w,h: Integer; lp: PLinkArray;
  fn: TShortstr;
begin
  link:=tdmwLink.Create;
  try
    if link.Open_link(Path,0,0) then

    if link.Count >= 3 then begin

      if Zoom > 1 then begin
        lp:=link.First;
        for i:=1 to link.Count do begin

          with lp[0].a do begin
            x:=x / Zoom; y:=y / Zoom;
          end;

          lp:=@lp[1]
        end
      end;

      if xFileExist(Path,'.tab') then begin
        StrUpdateExt(fn,Dest,'.tab');
        link.SaveAs_tab(fn,Dest)
      end
      else begin
        StrUpdateExt(fn,Dest,'.@@@');
        link.SaveAs_link(fn)
      end
    end;

  finally
    link.Free
  end
end;

function copy_tab_link(src,dst: PChar): bool;

function xCopy_tab(tif,jpg: PChar): bool;
var
  src,dst: TTextfile; s: TShortstr;
begin
  Result:=false;

  src:=TTextfile.Create;
  dst:=TTextfile.Create;
  try
    if src.Open_ext(tif,'.tab') then
    if dst.Make_ext(jpg,'.tab') then begin

      while src.xStrLine <> nil do begin
        StrCopy(s,src.str);
        if src.x_key('File') then
        StrFmt(s,'File "%s"',[xStrNameExt(jpg)]);

        if Strlen(s) > 0 then
        dst.WriteStr(Strpas(s));
      end;

      Result:=true
    end
  finally
    dst.Free;
    src.Free
  end
end;

function xCopy_lnk(tif,jpg: PChar): bool;
var
  lnk: tdmwLink; tab,pcx: TShortstr;
begin
  Result:=false;

  StrUpdateExt(tab,jpg,'.tab');
  StrNameExt(pcx,jpg);

  lnk:=tdmwLink.Create;
  try
    if lnk.Open_link(tif,0,0) then
    lnk.SaveAs_tab(tab,pcx);
  finally
    lnk.Free
  end
end;

begin
  Result:=xCopy_tab(src,dst);
  if not Result then xCopy_lnk(src,dst)
end;

end.
