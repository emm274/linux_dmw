unit dmw_prj; interface

uses
  Classes,LCLType,Math,
  activeX,otypes,ofiles,
  xintf,xproj,xlist,xlist1,
  xvirts,xrings,xline,ogauss,
  xstereo,dmw_link,dmx_dm,use_sas;

const
  def_Pencil = 12 + (14 shl 5) + (3 shl 10);

  tx_opt_Childs  = 1;
  tx_opt_Childs1 = 2;
  tx_opt_Delete  = 4;
  tx_opt_bak     = 8;
  tx_opt_mark    = 16;

  prj_folder_Id = -999999;
  prj_readonly  = 1;
  prj_map_dll   = 2;
  prj_map_tmp   = 4;
  prj_tif_kml   = 8;

  prj_disp_vec1 = 1;
  prj_disp_vec2 = 2;
  prj_disp_rel  = 4;
  prj_disp_tif  = 8;

  brkModeMax    = 5;

type
  tdm_to_pcx = procedure(x,y: Integer; out b: TGauss) of object;
  tpcx_to_dm = procedure(x,y: Double; out b: TPoint) of object;

  TXYZ_to_XY = procedure(x,y,z: Integer; out b: TPoint) of object;

  tx_Disp = record
    AreaFill,Mark,Info,Objs,Maps,Dup: Boolean;
    Calc,Tick,Grid,Thin,Dos,dm_Frame: Boolean;
    Cut,Clip,First: Boolean; Points: Byte;
    Reserv1,TextFill,AttrDisp,TextMask: Boolean;
    xxx,rlf,Movie,Reserv3,Ground,Math: Boolean;
    im_Frame,inf_Repeat,is_cn: Boolean;
    is_lotsia,mm_paste,hf_unk: Boolean;
  end;

  TMapRec = record
    scale1,scale2,dm_scale,flags: Longint;
    l_lt,l_rb: TPoint; x_lt,x_rb, kz: TGauss;
    lg: lg_Transit; x_mm,y_mm,w_mm,h_mm: Double;
    Index,Color,Rank,Key,Bak: Integer;
    Res: Array[0..7] of Integer;
    Visible,Enabled: Boolean;
    Path,Idt: TPathStr;
  end;

  PMapArray = ^TMapArray;
  TMapArray = Array[0..255] of TMapRec;

  TMapList = class(TCustomList)
    constructor Create;
  end;

  TMapFunc = function(Sender: TObject; ptr: Longint;
                      var rec: TMapRec): Integer of object;

  TMapBank = class(TInterfacedObject,IStrings1)

    constructor Create;
    destructor Destroy; override;

    function GetCount: Integer; stdcall;

    function GetItem(Ind: Integer;
                     Str: PChar; MaxLen: Cardinal;
                     out Flags: Cardinal): HResult; stdcall;

    procedure Clear;

    procedure ResetAllBak;

    procedure Assign(ABank: TMapBank);

    function LoadFrom(const stg: IStorage;
                      APath,Name: PChar): Integer;

    function LoadBank(ABank: TMapBank): Integer;

    procedure CopyTo(const stg: IStorage);

    function GetList(List: TStrings; Skip: Integer): Integer;

    function Dump(ADir: PChar;
                  AText: TTextfile;
                  AMode: Integer): Integer;

    procedure ForEach(Func: TMapFunc);

    function GetFolder(Name: PChar): Longint;
    function InsFolder(Name: PChar): Longint;
    function AddFolder(Name: PChar): Longint;

    function IsFolder(Ptr: Integer): Boolean;

    function NameOf(APath: PChar): Integer;
    function IdtOf(Str: PChar): Integer;
    function HashOf(AId: Integer): Integer;

    procedure Get_rec(Ptr: Integer; var rec: TMapRec);
    procedure Put_rec(Ptr: Integer; var rec: TMapRec);

    function Get_color(Ind: int): int;

    function Get_map(Ind: Integer; out rec: TMapRec): Boolean;
    procedure Put_map(Ind: Integer; const rec: TMapRec);

    function Get_trk(Ind: Integer; out Obj): Boolean;

    procedure visible_map(Ind,Cmd: Integer);

    procedure color_map(Ind,Color: Integer);
    procedure flags_map(Ind,Flags: Integer);

    function disp_map(Ind: Integer; out rec: TMapRec): Boolean;

    function Add_map(par: Longint;
                     var rec: TMapRec): Integer;

    procedure trk_map(Ind: Integer; trk: IGpsTrack);

    function Active_map(var rec: TMapRec): Boolean;

    function Exclude_map(Ind: Integer): Boolean;

    function Delete_path(Path: PChar): Boolean;
    function Delete_name(Name: PChar): Boolean;
    function Delete_map(Ind: Integer): Boolean;

    procedure Delete_node(Ptr: Longint); virtual;

    function Move_map(I,J: Integer): Boolean;

    procedure get_position(out pos: TPoint);
    procedure set_position(const pos: TPoint);

    function Swap_maps(i1,i2: Integer): Boolean;

    function Pickup(skip,rw: Integer;
                    const r: xgeoid;
                    out map: TMapRec): Integer;

  protected
    procedure ClearData; virtual;

  private
    fMem: TVirtMem;
    fTree: TVirtTree;
    fList: TIntegerList;

    fTracks: TKeyIntfList;
    fTrkKey: uint;

    ftdm_Nnn: Integer;

    fActive: Integer;
    fVideo: Integer;
    fItemIndex: Integer;

    fDumpDir: PChar;
    fDumpText: TTextfile;
    fDumpMode: Integer;
    fDumpCount: Integer;

    fBankScale1: Integer;
    fBankScale2: Integer;

    fOnData: TNotifyEvent;
    fOnVisible: TNotifyEvent;
    fOnDelete: TCharProc;

    function Get_tdm_Nnn: Integer;

    function GetEnabled: Boolean;

    procedure SetActive(Ind: Integer); virtual;
    procedure SetVideo(Ind: Integer);

    procedure SetItemIndex(Ind: Integer);

    function Refresh_list: Integer;

    function ResetBak(Sender: TObject; ptr: Longint;
                      var rec: TMapRec): Integer;

    function IndexMap(Sender: TObject; ptr: Longint;
                      var rec: TMapRec): Integer;

    function DumpMap(Sender: TObject; ptr: Longint;
                     var rec: TMapRec): Integer;

  public
    property Tree: TVirtTree read fTree;
    property Mem: TVirtMem read fMem;
    property List: TIntegerList read fList;

    property tdm_Nnn: Integer read Get_tdm_Nnn; 

    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled;

    property Active: Integer read fActive
                             write SetActive;

    property Video: Integer read fVideo
                            write SetVideo;

    property ItemIndex: Integer read fItemIndex
                                write SetItemIndex;

    property BankScale1: Integer read fBankScale1
                                 write fBankScale1;

    property BankScale2: Integer read fBankScale2
                                 write fBankScale2;

    property OnData: TNotifyEvent write fOnData;
    property OnVisible: TNotifyEvent write fOnVisible;
    property OnDelete: TCharProc write fOnDelete;
  end;

  TRelBank = class(TMapBank,IRelief)
    constructor Create;
    destructor Destroy; override;

    function Add(Path: PChar): Boolean;

    function xAdd(Path: PChar; list: TStrings;
                  Color,Flags: Integer): Boolean;

    function Active_data: Boolean;

    function Open_data: HResult;
    procedure Close_data;

    function get_mpp: Double;

    function Get_zval(g: PGeoPoint;
                      out zv: Double): Boolean;

    function GetValue(g: PGeoPoint;
                      out zv: Double): HResult; stdcall;

    function Get_Intf1(Path: PChar; var Obj): HResult;
    function Get_Intf2(Path: PChar; var Obj): HResult;

  protected
    procedure ClearData; override;

  private
    fdll_rel: THandle;
    fRelief: IRelief1;

    fNodeIndex: TPoint;

    fLocked: Longbool;

    procedure SetActive(Ind: Integer); override;

  public
    property Locked: Longbool write fLocked;

    property NodeIndex: TPoint read fNodeIndex
                               write fNodeIndex;
  end;

  TRelBankObj = class(TInterfacedObject,IRelief)
    constructor Create(ABank: TRelBank);
    destructor Destroy; override;

    function GetValue(g: PGeoPoint; out zv: Double): HResult; stdcall;
  private
    fBank: TRelBank;
  end;

  TCardFrame = record
    iv: ZOrient; rv: ZValues;
  end;

  TCardCentre = record
    pos: TGeoPoint; sc: Double
  end;

  TPrjFrame = record
    Count: Integer; sys: tsys;
    lt,rb: TGauss; dm: TShortstr
  end;

  TEditParams = record
    mark_dr: int;

    m_step: Double;
    m_ed: int;

    j_dist: Double;
    j_ed: int;

    brkLen: int;
    brkAuto: int;
    brkMode: int;
    brkMode1: int;
    brkMax: int;
    brkPull: int;
    brkPer: int;
    brkSharp: int;
    brkThick: int;

    joinrMode: int;

    cls: Array[0..2] of float;

    coRad: int;
    coMode: int;

    p_eps: int;
    p_ed: int;

    mf_nn: int;
    sqr_nn: int;

    find_nn: int;

    blankText: int;
  end;

  TMapProject = class

    Movie_wnd: HWnd;

    isEditor: Longbool;
    isPlan: Longbool;
    isDmw: Longbool;

    px_Count: Integer;
    px_lt,px_rb: tgauss;
    px_lt1,px_rb1: tgauss;
    px_centre: tgauss;
    px_ed,px_ppm: Double;
    px_north: Double;

    p_lg: lg_Transit;
    sm_T: AB_Orient;
    px_zone: TGauss;

    win_edit: TCardFrame;
    win_plan: TCardFrame;
    win_tiff: TCardFrame;

    win_gc: TCardCentre;

    mm_w,mm_h: Double;
    is_mm: Longbool;

    z_pps: byte;
    z_lt,z_rb: tgauss;

    Params: TEditParams;

    bak_dt: int;

    d3_on: Integer;
    d3_fov: Integer;
    d3_teta: Integer;

    dm_Ground: int;
    dm_Pencil: int;
    dm_Pencil1: int;

    cl_Edge: int;
    cl_Grid: int;

    MosaicColor: int;
    MosaicAlfa: int;
    MosaicClip: int;

    x_Disp: tx_Disp;
    x_Disp1: int64;

    wgs_mirror: bool;

    Image,Movie: TShortStr;

    constructor Create;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Changed;

    function Get_frame: TPrjFrame;

    function prj_Scale: double;
    function px_Width: double;
    function px_Height: double;
    function sm_Width: longint;
    function sm_Height: longint;

    procedure Get_prj_3x3(out T: Real3x3);

    procedure Push_DispAttrs;
    procedure Pop_DispAttrs;

    procedure GetWorkPath(Dest,Name: PChar);
    procedure GetIntfPath(Dest,Name: PChar);

    function GetDmwFolder(Dir: PChar): Boolean;
    function GetAlxFolder(Dir: PChar): Boolean;
    function GetRelFolder(Dir: PChar): Boolean;
    function GetTifFolder(Dir: PChar): Boolean;

    procedure Load_Options(APath: PChar);

    procedure Refresh_project;

    function LoadFrom(APath: PChar; Opt: Boolean): Integer;
    function SaveAs(APath: PChar; IsTxt: Boolean): PChar;

    procedure Reset_Project(im: Boolean);

    procedure Assign(Prj: TMapProject; flags: uint);

    procedure Hide_relief;
    function Back_relief: Boolean;

    procedure Update_relief(Sender: TObject; Path: PChar);

    function Relief_add(Path: PChar): bool;

    function tif_add(Path: PChar;
                     Color,Flags,Scale1,Scale2: int;
                     upd: Boolean): Boolean;

    function SwapImage1(i,j: Integer): Boolean;

    function DeleteImage1(i: Integer): Integer;

    function Open_image1(List: TStrings; Path: PChar;
                         Id: Integer; swap: Boolean): Boolean;

    function Open_pcx(Path: PChar): Boolean;

    procedure Close_pcx;

    procedure Stereo_delta_plan(Sender: TObject);

    procedure StereoEye(Value: Boolean);
    procedure StereoLeft(Value: Boolean);
    procedure StereoPair(Id: Integer);

    function Locate_link(const lt,rb: TPoint): Boolean;
    function Change_link(lnk: tdmwLink): tdmwLink;

    function IsImage: Boolean;
    function IsPcxLinked: Boolean;
    function Is_Stereo: Boolean;

    function Movie_map(dm: tdm_edit): Integer;
    procedure Return_map(dm: tdm_edit);

    function Get_rel_Path(Path: PChar): Boolean;

    function Get_alx_Path(Path: PChar): Boolean;

    function Get_alx_list(List: TStrings;
                          fr: PGPoly; vs: Boolean): Integer;

    function Get_all_alx(List: TStrings; vs: Boolean): Integer;

    function Get_alx_prj(Prj: TPlanList): Integer;

    function clear_alx_folder: Boolean;
    function clear_map_folder: Boolean;

    function Insert_Map(Path: PChar;
                        par,flags: Integer;
                        scale1,scale2: Integer): Integer;

    function xInsert_Map(Path: PChar): Boolean;

    function Delete_Map(Ind: Integer): Boolean;
    procedure Delete_Node(Ptr: Integer);

    function SwapMaps(i1,i2: Integer): Boolean;

    function Refresh_Map(Ind: Integer): Boolean;
    function Refresh_Active_Map: Boolean;

    function Refresh_Map1(Path: PChar): Boolean;

    function Open_Active_Map: Boolean;
    function Enabled_Active_Map: Boolean;

    function Backup_Active_Map: Boolean;

    procedure Swap_Visible_Map(Ind,dll: Integer);

    function Get_dm_list(list: TStrings): Integer;

    function dm_list(all: Boolean): TStringList;
    function Get_list(all: Boolean): TCharArray;

    function Alt_list(alt: TCharArray): Integer;

    function Alt_Project(alt: PChar): PChar;
    function xAlt_Project(alt: PChar): PChar;

    function Get_first_dm_map: Integer;

    function Get_dm_Path(Ind: Integer; Path: PChar): Boolean;
    function Get_Active_Path(Path: PChar): Boolean;

    function Project_Contains(const g: tgauss): Boolean;

    procedure Set_Display_dm(Ind: Integer; up: Boolean);

    function Str_dm_Path(Path,FName: PChar): PChar;

    function Open_dm(Ind: Integer): Boolean;

    function Open_Active_dm: Boolean;

    function Disp_map(const rec: TMapRec;
                      const lt,rb: TGauss;
                      Scale: Integer): Boolean;

    function Disp_scale(const rec: TMapRec;
                        Scale: Integer): Boolean;

    function Disp_dm(Ind,Scale: Integer;
                     const lt,rb: tgauss): Boolean;

    function Map_Contains_Gauss(Ind,Lev: Integer;
                                const p: tgauss): Boolean;

    function Map_Contains_Geoid(Ind,Lev: Integer;
                                const x: xgeoid): Boolean;

    function Map_Contains_Geoidb(Ind,Lev: int;
                                 bp: PGPoly;
                                 const s: tsys): bool;

    function Contains_Geoid(const x: xgeoid): Integer;
    function Contains_Gauss(const p: tgauss): Integer;

    function Contains_alx(const lt,rb: tgauss): Boolean;
    function Contains_rgn(const lt,rb: tgauss): Boolean;

    function Contains_Map(Path: PChar): Integer;
    function Map_ExpandPath(Path,fn: PChar): PChar;

    procedure Set_map_sm_T(Ind: Integer);

    function Active_enabled(Ind: Integer): Boolean;

    function Get_map_lg_t(const rec: TMapRec;
                          out T: AB_Orient): Boolean;

    function Get_dm_lg_t(Ind: Integer;
                         out T: AB_Orient): Boolean;

    function Get_Active_lg_t(out T: AB_Orient): Boolean;

    function prj_Resize(const sys: tsys): Boolean;

    function Resize(auto: Boolean): Boolean;
    function auto_Resize: Boolean;

    function Get_Bound(out lt,rb: TGauss): Integer;

    function SaveToMpg(Path: PChar): Integer;

    procedure Clip_Rect(a,b: tgauss; out c,d: tgauss);
    procedure prj_GOrient(var R: GOrient);

    function geo_Get_zval(const g: XGeoid;
                          out zv: Double): Boolean;

    function dm_Get_zval(const p: TPoint;
                         out zv: Double): Boolean;

    function xy_Get_zval(const g: TGauss;
                         out zv: Double): Boolean;

    function xy_Get_zmin(const c,v: TGauss;
                         dr: Double; out zv: Double): Boolean;

    procedure Get_xy_port(const lt,rb: TPoint;
                          out g_lt,g_rb: TGauss);

    procedure xy_to_sm(g: tgauss; out p: TPoint);
    procedure sm_to_xy(p: TPoint; out g: tgauss);

    procedure dm_to_pcx(x,y: Integer; out b: TGauss);
    procedure pcx_to_dm(x,y: Double; out b: TPoint);

    procedure dm_to_bmp(const a: TPoint; out b: TPoint);
    procedure bmp_to_dm(const a: TPoint; out b: TPoint);

    function xyz_to_pair(const v: vpoint;
                         out p: TPoint): Integer;

    procedure xyz_to_scb(x,y,z: Integer; out b: TPoint);
    procedure scb_to_xyz(x1,x2,y: Integer; out b: vpoint);

    procedure dm_to_scb(x,y: Integer; out b: tgauss);
    procedure scb_to_dm(x,y: Double; out b: TPoint);

    function rpc_to_dm(x,y: Double; out v: lxyz): Boolean;

    function get_rpc_h1(const v: VPoint;
                        const cp: TGauss;
                        dm_t_rpc: Boolean): Float;

    procedure mm_rec_x_lt_rb(var rec: TMapRec);

    function get_pcx_plan(out x,y,w,h: Integer): Integer;

    function get_prj_t(const gc: TGauss; fi: Double;
                       out tr,bt: Real3x3): TGauss;

    function GetSasInterface(out Obj: IGoogle): bool;

    function SetSasMapLayer(Name: PChar): int;
    function SetSasSklLayer(Name: PChar; Up: int): int;

    procedure pushSasCurrentChart;

    function GetBrkThick: int;

    procedure SetBrkLen(code,val: int);
    procedure NextMarkRadius;

  private
    fStorage: IStorage;

    fBank: TMapBank;

    fBank1: TRelBank;
    fRelief: IRelief;

    fBank2: TMapBank;
    fList2: TStringList;

    fMap: tdm_Edit;

    fPcx_link: tdmwLink;

    fAlxList: TPlanList;

    fImages: TStringList;

    fDispAttrs: TDispAttrList;

    fCards: TList;

    fUserData: TStringList;

    fBrkLenList: TPointList;

    xxx_lg: AB_Matrix;
    xxx_gl: AB_Matrix;

    fpcx_mixed: Longbool;
    fpcx_tiled: Longbool;
    fpcx_rel: Longbool;

    ftif_active_last: Longbool;

    fjpg_cut: TPoint;

    fUpdated: Integer;

    fsasLevel: int;

    fsasChart: TSasChart;
    fsasUser: TSasChartList;
    fsasInit: longbool;

    fHttp: IHttpGet;

    fOnLoadFrom: TTextfileEvent;
    fOnSaveAs: TNotifyEvent;

    fOnAlx: TNotifyEvent;
    fOnChanged: Classes.TNotifyEvent;
    fOnChangeMap: TNotifyEvent;
    fOnStatusMap: TNotifyEvent;
    fOnDestroy: TNotifyEvent;
    fOnImage: TNotifyEvent;
    fOnDBase: TCharProc;

    fOnRpcPos: TSetInteger;

    fOnVisible: TNotifyEvent;

    fOnUpdateRel: TCharProc;

    fOnSasLevel: TNotifyEvent;

    fStereo: TStereo;

    fEditPath: TShortStr;
    fProfile: TShortStr;

    fPhoto: TShortStr;
    fTemp: TShortStr;
    fSas: TShortstr;

    procedure VisibleEvent(Sender: TObject);

    function GetEditPath: PChar;

    function GetProfile: PChar;
    procedure SetProfile(Dir: PChar);

    function GetCount: Integer;
    function GetCount2: Integer;
    function GetVideo: Integer;

    function GetSas: PChar;
    procedure SetSas(Dir: PChar);

    procedure txt_Load_Options(txt: TTextfile);
    procedure txt_Load_env(txt: TTextfile);
    procedure txt_Load_sas(txt: TTextfile);
    procedure txt_user_data(txt: TTextfile; key: PChar);

    procedure xLoad_Options(const stg: IStorage);

    function Get_alx_project: Boolean;

    function GetPhoto: PChar;
    procedure SetPhoto(Path: PChar);

    function GetActiveMap: Integer;
    procedure SetActiveMap(Ind: Integer);

    function GetEnabled: Boolean;

    procedure SetStereo(const scb: TStereo);

    procedure SetImageMirror(Value: Boolean);

    procedure SetSasLevel(v: int);

    function map_x_lt_rb(Sender: TObject; ptr: Longint;
                         var rec: TMapRec): Integer;

  public
    property Bank: TMapBank read fBank;
    property Bank1: TRelBank read fBank1;
    property Bank2: TMapBank read fBank2;
    property List2: TStringList read fList2;

    property AlxList: TPlanList read fAlxList;

    property Count: Integer read GetCount;
    property Count2: Integer read GetCount2;
    property Video: Integer read GetVideo;

    property ActiveMap: Integer read GetActiveMap
                                write SetActiveMap;

    property alx_project: Boolean read Get_alx_project;

    property Stereo: TStereo read fStereo write SetStereo;

    property Photo: PChar read GetPhoto write SetPhoto;

    property Enabled: Boolean read GetEnabled;

    property Map: tdm_Edit read fMap;

    property Pcx_link: tdmwLink read fPcx_link;

    property pcx_mixed: Longbool read fpcx_mixed;
    property pcx_tiled: Longbool read fpcx_tiled;
    property pcx_rel: Longbool read fpcx_rel write fpcx_rel;

    property jpg_cut: TPoint read fjpg_cut write fjpg_cut;

    property Images: TStringList read fImages;

    property DispAttrs: TDispAttrList read fDispAttrs;

    property Cards: TList read fCards;

    property UserData: TStringList read fUserData;

    property BrkLenList: TPointList read fBrkLenList;

    property EditPath: PChar read GetEditPath;

    property Profile: PChar read GetProfile
                            write SetProfile;

    property ImageMirror: Boolean write SetImageMirror;

    property tif_active_last: Longbool read ftif_active_last
                                       write ftif_active_last;

    property sasLevel: int read fsasLevel
                           write SetSasLevel;

    property sasChart: TSasChart read fsasChart;
    property sasUser: TSasChartList read fsasUser;

    property Sas: PChar read GetSas write SetSas;

    property Http: IHttpGet read fHttp write fHttp;

    property OnLoadFrom: TTextfileEvent write fOnLoadFrom;
    property OnSaveAs: TNotifyEvent write fOnSaveAs;

    property OnAlx: TNotifyEvent write fOnAlx;
    property OnChanged: TNotifyEvent write fOnChanged;
    property OnChangeMap: TNotifyEvent write fOnChangeMap;
    property OnStatusMap: TNotifyEvent write fOnStatusMap;
    property OnDestroy: TNotifyEvent write fOnDestroy;
    property OnImage: TNotifyEvent write fOnImage;
    property OnDBase: TCharProc read fOnDBase write fOnDBase;
    property OnRpcPos: TSetInteger write fOnRpcPos;

    property OnUpdateRel: TCharProc write fOnUpdateRel;

    property OnVisible: TNotifyEvent write fOnVisible;

    property OnSasLevel: TNotifyEvent write fOnSasLevel;
  end;

  PTabPos = ^TTabPos;
  TTabPos = record
    g: xgeoid; sc: double;
    msg: string[63]
  end;

  TTabList = class(TCustomList)
    constructor Create;

    function Add_Item(const g: xgeoid;
                      sc: double; s: PChar): Integer;

    function LoadFrom(Path: PChar): Integer; virtual;
    procedure SaveAs(Path: PChar); virtual;
  end;

function CardFrameIdentify: TCardFrame;

function CardFrameStr(const win: TCardFrame): String;

function GetCardFrame(txt: TTextfile; out fr: TCardFrame): Boolean;

function FrameContains(const fr1,fr2: TPrjFrame): Boolean;

function dmw_get_prj_dir(Dir: PChar): Boolean;
procedure dmw_put_prj_dir(Dir: PChar);

function dmw_get_dm_dir(Dir: PChar): Boolean;
procedure dmw_put_dm_dir(Dir: PChar);

function dmw_get_lnk_dir(Dir: PChar): Boolean;
procedure dmw_put_lnk_dir(Dir: PChar);

function dmw_get_pcx_dir(Dir: PChar): Boolean;
procedure dmw_put_pcx_dir(Dir: PChar);

function dmw_get_tif_dir(Dir: PChar): Boolean;
procedure dmw_put_tif_dir(Dir: PChar);

function dmw_get_trk_dir(Dir: PChar): Boolean;
procedure dmw_put_trk_dir(Dir: PChar);

function dmw_get_prn_dir(Dir: PChar): Boolean;
procedure dmw_put_prn_dir(Dir: PChar);

function dmw_get_txt_dir(Dir: PChar): Boolean;
procedure dmw_put_txt_dir(Dir: PChar);

function dmw_get_rel_dir(Dir: PChar): Boolean;
procedure dmw_put_rel_dir(Dir: PChar);

function dmw_get_dbf_dir(Dir: PChar): Boolean;
procedure dmw_put_dbf_dir(Dir: PChar);

function dmw_get_gps(fn: PChar): Boolean;
procedure dmw_put_gps(fn: PChar);

function dmw_get_fi_fmt: Integer;

function dmw_text_prj(Path: PChar): Boolean;

function gport_contains_map(Frame: PGPoly;
                            const sys: tsys;
                            const map: TMapRec): Boolean;

function Get_map_transit(const map: TMapRec;
                         const sys: tsys;
                         out tr,bt: Real3x3): Boolean;

implementation

uses
  dynlibs,
  SysUtils,
  Graphics,
  Convert,xini,xutils,
  xclasses,xbl_use,xpoly,
  xy,xmath,dmw_dm,storage,xdc,
  img_dll,ximages,img_rel,
  img_x,use_lnk,dmw_pcx;

const
  pcx_dir = 'pcx_Directory';
  tif_dir = 'tif_Directory';
  trk_dir = 'trk_Directory';
  prn_dir = 'prn_Directory';
  lnk_dir = 'lnk_Directory';
  prj_dir = 'prj_Directory';
  dm_dir = 'dm_Directory';
  txt_dir = 'txt_Directory';
  rel_dir = 'rel_Directory';
  dbf_dir = 'dbf_Directory';

  x_Mark: longint = $34303021;

  TFlags_Max = 31;

type
  TFlags = array[0..TFlags_Max] of Boolean;

type
  TPrjOptions = record
    Options: int;
    dm_Ground: int;
    dm_Pencil: int;
    cl_Grid: int;
    cl_Edge: int;
    x_Disp1: Int64;
    MosaicColor: int;
    MosaicAlfa: int;
    bak_dt: int;
    dm_Pencil1: int;
    MosaicClip: int;
    Reserv: Array[0..9] of int
  end;

  TPrjEnv = record
    edit: TCardFrame;
    plan: TCardFrame;
    tiff: TCardFrame;

    mm_w,mm_h: double;
    mm_on: Integer;

    Params: TEditParams;

    stereoId: Integer;

    Image: TShortstr;
    Photo: TShortstr;

    d3_on: Integer;
    d3_fov: Integer;
    d3_teta: Integer;

    BankScale1: Integer;
    BankScale2: Integer;

    Bank1Scale1: Integer;
    Bank1Scale2: Integer;

    Bank2Scale1: Integer;
    Bank2Scale2: Integer;

    Reserv: Array[0..987] of Byte
  end;

function CardFrameIdentify: TCardFrame;
var
  fr: TCardFrame;
begin
  Fillchar(fr,Sizeof(fr),0);
  fr.iv[0]:=-1; fr.rv[0]:=1;
  Result:=fr
end;

function CardFrameStr(const win: TCardFrame): String;
begin
  with win do
  Result:=IntToStr(iv[0])+' '+
          IntToStr(iv[1])+' '+
          IntToStr(iv[2])+' '+
          IntToStr(iv[3])+' '+
          RealToStr(rv[0],8)+' '+
          IntToStr(iv[4])+' '+
          RealToStr(rv[1],8)+' '+
          RealToStr(rv[2],8)+' '+
          RealToStr(rv[3],8)
end;

function GetCardFrame(txt: TTextfile; out fr: TCardFrame): Boolean;
var
  i: int;
begin
  Result:=false;

  Fillchar(fr,Sizeof(fr),0);
  if txt.x_Int(fr.iv[0]) then
  if txt.x_Int(fr.iv[1]) then
  if txt.x_Int(fr.iv[2]) then
  if txt.x_Int(fr.iv[3]) then
  if txt.x_Double(fr.rv[0]) then begin

    txt.x_Int(fr.iv[4]);

    for i:=1 to 3 do
    txt.x_Double(fr.rv[i]);

    Result:=true
  end;

  if not Result then fr.iv[0]:=0
end;

function FrameContains(const fr1,fr2: TPrjFrame): Boolean;
begin
  Result:=false;

  if (fr1.Count > 0) and (fr2.Count > 0) then
  if sys_Equal(fr1.sys,fr2.sys) then
  Result:=GaussContainsPort(fr1.lt,fr1.rb,fr2.lt,fr2.rb)
end;

function frame_contains_map(Frame: PGPoly;
                            const map: TMapRec): Boolean;
begin
  Result:=true; if Assigned(Frame) then with map do
  Result:=GaussContainsPort(Frame[0],Frame[1],x_lt,x_rb)
end;

function gport_contains_map(Frame: PGPoly;
                            const sys: tsys;
                            const map: TMapRec): Boolean;
var
  i: Integer; g1,g2: GOrient; r: xgeoid;
  p,lt1,rb1,lt2,rb2: TGauss; gs: tsys;
begin
  Result:=false;

  gs:=map.lg.s; g2:=map.lg.g; p:=g2[0];
  if (sys.pps = 1) or (sys.prj > 0) then
  sys_projection(gs, p.x,p.y);

  r.s:=sys;
  for i:=0 to 3 do begin
    r.x:=Frame[i].x; r.y:=Frame[i].y;
    g1[i]:=prj_inside(r,gs);

    if gs.pps = 1 then with g2[i] do
    sys_BL_XY(x,y,gs,x,y)
  end;

  Max_Gauss_Bound(@g1,4,lt1,rb1);
  Max_Gauss_Bound(@g2,4,lt2,rb2);
  Result:=GaussContainsPort(lt1,rb1,lt2,rb2)
end;

function Get_map_transit(const map: TMapRec;
                         const sys: tsys;
                         out tr,bt: Real3x3): Boolean;
var
  i: Integer; r: xgeoid;
  p: TGauss; gs: tsys;
  g1,g2: GOrient;
begin
  gs:=map.lg.s; p:=map.lg.g[0];
  if (sys.pps = 1) or (sys.prj > 0) then
  sys_projection(gs, p.x,p.y);

  r.s:=gs;
  for i:=0 to 3 do begin
    p:=map.lg.g[i];
    r.x:=p.x; r.y:=p.y;

    if gs.pps = 1 then
    prj_BL_XY(p.x,p.y,gs,p.x,p.y);

    g1[i]:=p; prj_to_xy(r,sys,g2[i])
  end;

  Result:=Solve_projective1(@g1,@g2,tr,bt)
end;

function Pack_x_Disp(disp: tx_Disp): Cardinal;
var
  i,bit: Integer;
begin
  bit:=1; Result:=0;
  for i:=0 to TFlags_Max do begin
    if TFlags(disp)[i] then
    Result:=Result or bit;
    bit:=bit shl 1
  end
end;

function Unpack_x_Disp(pack: Cardinal): tx_Disp;
var
  i,bit: Cardinal; v: tx_Disp;
begin
  Fillchar(v,Sizeof(v),0);

  bit:=1;
  for i:=0 to TFlags_Max do begin
    TFlags(v)[i]:=pack and bit > 0;
    bit:=bit shl 1
  end;

  dos_interface:=v.Dos;
  Result:=v
end;

function dmw_get_prj_dir(Dir: PChar): Boolean;
begin
  Result:=ini.GetDir(prj_dir,Dir)
end;

procedure dmw_put_prj_dir(Dir: PChar);
begin
  if StrLen(Dir) > 0 then
  ini.PutDir(prj_dir,Dir)
end;

function dmw_get_dm_dir(Dir: PChar): Boolean;
begin
  Result:=ini.GetDir(dm_dir,Dir)
end;

procedure dmw_put_dm_dir(Dir: PChar);
begin
  if StrLen(Dir) > 0 then
  ini.PutDir(dm_dir,Dir)
end;

function dmw_get_lnk_dir(Dir: PChar): Boolean;
begin
  Result:=ini.GetDir(lnk_dir,Dir)
end;

procedure dmw_put_lnk_dir(Dir: PChar);
begin
  if StrLen(Dir) > 0 then
  ini.PutDir(lnk_dir,Dir)
end;

function dmw_get_pcx_dir(Dir: PChar): Boolean;
begin
  Result:=ini.GetDir(pcx_dir,Dir)
end;

procedure dmw_put_pcx_dir(Dir: PChar);
begin
  if StrLen(Dir) > 0 then
  ini.PutDir(pcx_dir,Dir)
end;

function dmw_get_tif_dir(Dir: PChar): Boolean;
begin
  Result:=ini.GetDir(tif_dir,Dir)
end;

procedure dmw_put_tif_dir(Dir: PChar);
begin
  if StrLen(Dir) > 0 then
  ini.PutDir(tif_dir,Dir)
end;

function dmw_get_trk_dir(Dir: PChar): Boolean;
begin
  Result:=ini.GetDir(trk_dir,Dir)
end;

procedure dmw_put_trk_dir(Dir: PChar);
begin
  if StrLen(Dir) > 0 then
  ini.PutDir(trk_dir,Dir)
end;

function dmw_get_prn_dir(Dir: PChar): Boolean;
begin
  Result:=ini.GetDir(prn_dir,Dir)
end;

procedure dmw_put_prn_dir(Dir: PChar);
begin
  if StrLen(Dir) > 0 then
  ini.PutDir(prn_dir,Dir)
end;

function dmw_get_txt_dir(Dir: PChar): Boolean;
begin
  Result:=ini.GetDir(txt_dir,Dir)
end;

procedure dmw_put_txt_dir(Dir: PChar);
begin
  if StrLen(Dir) > 0 then
  ini.PutDir(txt_dir,Dir)
end;

function dmw_get_rel_dir(Dir: PChar): Boolean;
begin
  Result:=ini.GetDir(rel_dir,Dir)
end;

procedure dmw_put_rel_dir(Dir: PChar);
begin
  if StrLen(Dir) > 0 then
  ini.PutDir(rel_dir,Dir)
end;

function dmw_get_dbf_dir(Dir: PChar): Boolean;
begin
  Result:=ini.GetDir(dbf_dir,Dir)
end;

procedure dmw_put_dbf_dir(Dir: PChar);
begin
  if StrLen(Dir) > 0 then
  ini.PutDir(dbf_dir,Dir)
end;

function dmw_get_gps(fn: PChar): Boolean;
begin
  Result:=ini.GetFile('gps_xy_bl',fn)
end;

procedure dmw_put_gps(fn: PChar);
begin
  if StrLen(fn) > 0 then
  ini.PutFile('gps_xy_bl',fn)
end;

function dmw_get_fi_fmt: Integer;
var
  ini: tini; v: Integer;
begin
  ini:=tini.Create('dlg_dmw');
  try
    if ini.GetInt('Angle_fmt',v) then
    Set_Angle_fmt(v);
  finally
   ini.Free
  end;

  Result:=Angle_fmt
end;

function dmw_text_prj(Path: PChar): Boolean;
var
  magic: TShortstr;
begin
  Result:=false;
  if This_Text(Path,magic) then
  Result:=StrIComp(magic,'#prj') = 0
end;

procedure Precise_x_lt_rb(dm: tdm_map; var rec: TMapRec);
var
  i: Integer; ln: TLLine; g: TGauss;
begin
  if dm.Get_dm_Bound(@ln,LPoly_Max) > 0 then

  if (ln.N >= 4) and (ln.N <= 16) then
  for i:=0 to ln.N-1 do begin

    dm.LG_T.L_to_G(ln.Pol[i],g);

    with rec do
    if i = 0 then begin
      x_lt:=g; x_rb:=g
    end
    else Max_GPort(x_lt,x_rb, g);
  end
end;

function This_map(Path: PChar; out rec: TMapRec): Boolean;
var
  dm: tdm_Map; hdr: dm_Hdr;
begin
  Result:=false;

  Fillchar(rec,Sizeof(rec),0);

  dm:=tdm_Map.Create(1);
  try
    if dm.Open_Map(Path,false) then begin

      if dm.dm_ver = 0 then
      dm_Update_Version(dm);

      if dm.Doctor_Map then
      if dm.Update_Map then
      dm.Doctor_Map;

      dm.Link_Map(true);

      if dm.Enabled_Map then begin

        dm.Get_dm_Hdr(hdr);
        StrPCopy(rec.Idt,hdr.idt);
        StrWin(rec.Idt);

        rec.Visible:=true;
        rec.Enabled:=true;

        rec.l_lt:=dm.dm_lt;
        rec.l_rb:=dm.dm_rb;
        rec.dm_scale:=dm.dm_scale;

        with rec do
        dm.LG_t.Get_Gauss_Bound(l_lt,l_rb,x_lt,x_rb);

        Precise_x_lt_rb(dm,rec);

        dm.Get_lg_Transit(rec.lg);
        StrPCopy(rec.Path,dm.Tree.vm_Path);

        with rec do
        dm.Get_mm_Rect(x_mm,y_mm,w_mm,h_mm);

        Result:=true
      end
    end;
  finally
    dm.Free
  end
end;

function Is_tree(tree: TVirtTree): Boolean;
begin
  Result:=false; if tree.Root = 32 then
  if tree.vm_Virt.vm_Long(12) = x_Mark then
  if tree._Length(tree.Root) = SizeOf(TMapRec) then
  Result:=true
end;

const
  flag_prj_dll = $100;
  flag_prj_tmp = $200;

function Get_map_flags(const rec: TMapRec): Cardinal;
begin
  Result:=0;
  if not rec.Visible then Inc(Result,flag_visible);
  if rec.flags and prj_readonly <> 0 then Inc(Result,flag_readonly);
  if rec.flags and prj_map_dll <> 0 then Inc(Result,flag_prj_dll);
  if rec.flags and prj_map_tmp <> 0 then Inc(Result,flag_prj_tmp);
  if not rec.Enabled then Inc(Result,flag_enabled);
end;

procedure Set_map_flags(var rec: TMapRec; fl: Cardinal);
begin
  rec.Visible:=fl and flag_visible = 0;
  rec.Enabled:=fl and flag_enabled = 0;

  rec.flags:=rec.flags and (prj_readonly xor (-1));

  if fl and flag_readonly <> 0 then
  rec.flags:=rec.flags or prj_readonly;

  if fl and flag_prj_dll <> 0 then
  rec.flags:=rec.flags or prj_map_dll;

  if fl and flag_prj_tmp <> 0 then
  rec.flags:=rec.flags or prj_map_tmp;
end;

constructor TMapList.Create;
begin
  inherited Create(Sizeof(TMapRec),256)
end;

constructor TMapBank.Create;
begin
  inherited;

  fMem:=TVirtMem.Create(1,32);
  fTree:=TVirtTree.Create(fMem,16);
  fList:=TIntegerList.Create;

  fTracks:=TKeyIntfList.Create
end;

destructor TMapBank.Destroy;
begin
  ClearData;

  fTracks.Free;

  fList.Free;
  fTree.Free;
  fMem.Free;

  inherited
end;

function TMapBank.Get_tdm_Nnn: Integer;
begin
  Inc(ftdm_Nnn);
  Result:=ftdm_Nnn
end;

function TMapBank.GetCount: Integer;
begin
  Result:=fList.Count
end;

function TMapBank.GetItem(Ind: Integer;
                     Str: PChar; MaxLen: Cardinal;
                     out Flags: Cardinal): HResult;
var
  rec: TMapRec;
begin
  Result:=S_FALSE; Flags:=0;

  if Get_map(Ind,rec) then begin
    if MaxLen = 0 then MaxLen:=255;
    StrLCopy(Str,rec.Path,MaxLen);
    Flags:=Get_map_flags(rec);
    Result:=S_OK
  end
end;

procedure TMapBank.ClearData;
begin
  fTracks.Clear;
  fTrkKey:=0
end;

procedure TMapBank.Clear;
var
  rec: TMapRec;
begin
  ClearData;
  fList.Clear;

  fActive:=0; fVideo:=0;
  fItemIndex:=0;

  if fMem.vm_Active then
  with fTree do begin
    Root:=0; TopP:=0; RunP:=0;
    fMem.vm_Startup;

    FillChar(rec,SizeOf(rec),0);
    Ins_Root(rec,SizeOf(rec))
  end;

  if Assigned(fOnDelete) then
  fOnDelete(Self,nil)
end;

function TMapBank.Refresh_list: Integer;
begin
  fList.Clear;

  ForEach(IndexMap);

  if ItemIndex >= Count then
    ItemIndex:=Count-1
  else
    ItemIndex:=ItemIndex;

  Result:=fList.Count
end;

function TMapBank.ResetBak(Sender: TObject; ptr: Longint;
                           var rec: TMapRec): Integer;
begin
  rec.Bak:=-1; Result:=1
end;

function TMapBank.IndexMap(Sender: TObject; ptr: Longint;
                           var rec: TMapRec): Integer;
begin
  rec.Index:=-1;
  if rec.dm_scale <> prj_folder_Id then
  if rec.flags and prj_readonly = 0 then
  rec.Index:=fList.AddItem(ptr);

  Result:=1
end;

procedure TMapBank.ResetAllBak;
begin
  ForEach(ResetBak)
end;

procedure TMapBank.Assign(ABank: TMapBank);
var
  i,ptr: Integer; rec: TMapRec;
begin
  ClearData;
  fList.Clear;

  Abank.Mem.CopyTo(fMem);
  fMem.vm_Activate;

  Refresh_list;

  Active:=ABank.Active;
  fVideo:=0;
end;

function TMapBank.LoadBank(ABank: TMapBank): Integer;
var
  i: Integer; rec: TMapRec;
begin
  Clear;

  for i:=0 to ABank.Count-1 do
  if ABank.Get_map(i,rec) then
  Add_map(0,rec);

  Result:=Refresh_list
end;

function TMapBank.LoadFrom(const stg: IStorage;
                           APath,Name: PChar): Integer;

procedure Verify_Project(APath: PChar);

function dm_Exist(dm,fld: PChar): Boolean;
var
  dir,sub,fn: TShortstr;
begin
  Result:=false;

  StrDirectory(dir,dm);
  StrNameExt(sub,dir);

  StrNameExt(fn,dm);
  StrPath(dm,fld,fn);

  if FileExist(dm) then
    Result:=true
  else begin
    StrPath(dir,fld,sub);
    StrPath(dm,dir,fn);
    Result:=FileExist(dm)
  end
end;

procedure scan_childs(par: Longint;
                      txt: TTextfile;
                      Dir: PChar);
var
  top,run,prd,nxt: longint;
  rec: TMapRec; fn,msg: TShortstr;
begin
  with fTree do begin

    top:=_Child(par);
    if top > Root then
    if top < Mem.vm_Ind then begin

      prd:=top; run:=_Link(top);

      while run <> top do begin

        nxt:=_Link(run);
        _Info(run,rec,SizeOf(rec));

        StrLCopy(fn,rec.Path,255);
        StrCopy(msg,'ok.');

        if rec.dm_scale = prj_folder_Id then begin
          scan_childs(run,txt,Dir);
          prd:=run
        end
        else begin
          if FileExist(rec.Path) then
            prd:=run
          else
          if dm_Exist(rec.Path,Dir) then begin
            Tree.NodeInfo(run,rec,SizeOf(rec));
            prd:=run
          end
          else begin
            NodeLink(prd,nxt);
            StrCopy(msg,'not found.');
          end;

          if txt.Active then
          txt.WriteStr(Strpas(fn)+' '+Strpas(msg));
        end;

        if nxt < Root then begin
          NodeLink(prd,top); Break;
        end;

        run:=nxt
      end
    end
  end
end;

var
  txt: TTextfile; fld: TShortstr;
begin
  StrDirectory(fld,APath);

  txt:=TTextfile.Create;
  try
    if false then
    txt.Make_ext(APath,'.txt');

    if Mem.vm_Active then
    scan_childs(fTree.Root,txt,fld);
  finally
    txt.Free
  end;
end;

var
  tmp: TVirtMem; tr: TVirtTree;
  copy: TCopyTree; act: Integer;
begin
  Clear;

  tmp:=TVirtMem.Create(0,0);
  tr:=TVirtTree.Create(tmp,16);
  try
    if tmp.stm_Open(stg,Name,ole_Read) then
    if Is_Tree(tr) then

    if fMem.vm_Active then begin

      if false then begin
        tmp.CopyTo(fMem); fMem.vm_Activate;
        act:=fMem.vm_Word(30);
      end else

      with fTree do begin
        Root:=0; TopP:=0; RunP:=0;
        fMem.vm_Startup;
        fMem.vm_Store(12,x_Mark,4);

        act:=tmp.vm_Word(30);

        copy:=TCopyTree.Create(tr,fTree,0);
        try
          copy.Copy_Tree
        finally
          copy.Free
        end;
      end;

      if not Is_Tree(fTree) then Clear
    end
  finally
    tr.Free;
    tmp.Free
  end;

  Verify_Project(APath);

  Result:=Refresh_list;
  Active:=act
end;

procedure TMapBank.CopyTo(const stg: IStorage);
begin
  fMem.vm_Store(12,x_Mark,4);
  fMem.vm_Store(30,fActive,2);
  xCopyStream(stg,fMem.vm_Stream)
end;

function TMapBank.GetList(List: TStrings; Skip: Integer): Integer;
var
  i: Integer; rec: TMapRec;
begin
  for i:=0 to Count-1 do
  if Get_map(i,rec) then
  if rec.flags and Skip = 0 then
  List.Add(rec.Path);

  Result:=List.Count
end;

function TMapBank.Dump(ADir: PChar;
                       AText: TTextfile;
                       AMode: Integer): Integer;
var
  dir: TShortstr;
begin
  fDumpDir:=nil;

  if Assigned(ADir) then begin
    StrCopy(dir,ADir);
    xStrUpper(dir);
    fDumpDir:=dir
  end;

  fDumpText:=AText;
  fDumpCount:=0;
  fDumpMode:=AMode;
  ForEach(DumpMap);
  Result:=fDumpCount
end;

function TMapBank.DumpMap(Sender: TObject; ptr: Longint;
                          var rec: TMapRec): Integer;
var
  p: PChar; fl: Cardinal;
  dir,fn: TShortStr; s: TPathStr;
  ok: Boolean;
begin
  if rec.dm_scale <> prj_folder_Id then begin

    ok:=true;
    if fDumpMode = 1 then
      ok:=This_ext(rec.Path,'.alx')
    else
    if fDumpMode = 2 then
      ok:=This_ext(rec.Path,'.dm')
    else
    if fDumpMode = 3 then
      ok:=rec.Key > 0;

    if ok then begin
      Inc(fDumpCount);
      if Assigned(fDumpText) then begin
        fl:=Get_map_flags(rec);

        StrCopy(fn,rec.Path);
        if This_ext(fn,'.tdm') then
        if FileExist(rec.Idt) then
        StrCopy(fn,rec.Idt);

        if Assigned(fDumpDir) then begin
          StrDirectory(dir,fn); xStrUpper(dir);
          if StrIComp(dir,fDumpDir) = 0 then
          StrNameExt(fn,rec.Path)
        end;

        with rec do
        StrLFmt(s,255,'"%s" %d %d %d %d',
          [fn,Color,fl,scale1,scale2]);
        fDumpText.WriteStr(Strpas(s));
      end
    end
  end;

  Result:=0
end;

procedure TMapBank.ForEach(Func: TMapFunc);

procedure ForEachChilds(par: Longint; Func: TMapFunc);
var
  top,run: Longint; rec: TMapRec;
begin
  with fTree do begin

    top:=_Child(par);
    if top > Root then
    if top < Mem.vm_Ind then begin

      run:=_Link(top);
      while run <> top do begin

        if not _Verify(run) then Break;
        _Info(run,rec,SizeOf(rec));

        if Func(Self,run,rec) = 1 then
        NodeInfo(run,rec,SizeOf(rec));

        if _Child(run) > 0 then
        ForEachChilds(run,Func);

        run:=_Link(run)
      end
    end
  end
end;

begin
  if Enabled then
  ForEachChilds(Tree.Root,Func);
end;

function TMapBank.GetFolder(Name: PChar): Longint;
var
  top,run: Longint; rec: TMapRec;
begin
  Result:=0;

  with Tree do begin

    top:=Tree._Child(Root);
    if top > Root then begin
      run:=_Link(top);
      while run > top do begin

        if not _Verify(run) then Break;
        _Info(run,rec,Sizeof(rec));

        if rec.dm_scale = prj_folder_Id then
        if StrIComp(rec.Path,'#alx') = 0 then
        begin Result:=run; Break end;

        run:=_Link(run)
      end

    end
  end
end;

function TMapBank.InsFolder(Name: PChar): Longint;
var
  rec: TMapRec;
begin
  Fillchar(rec,Sizeof(rec),0);
  rec.dm_scale:=prj_folder_Id;
  rec.Visible:=true; StrCopy(rec.Path,Name);
  Result:=Tree.Ins_Node(rec,SizeOf(rec));
end;

function TMapBank.AddFolder(Name: PChar): Longint;
begin
  with Tree do begin
    Ring_Down(Root); Last_RunP;
    Result:=InsFolder(Name)
  end;
end;

function TMapBank.IsFolder(Ptr: Integer): Boolean;
var
  rec: TMapRec;
begin
  fTree._Info(Ptr,rec,SizeOf(rec));
  Result:=rec.dm_scale = prj_folder_Id
end;

function TMapBank.NameOf(APath: PChar): Integer;
var
  i: Integer; rec: TMapRec; nm1,nm2: TShortstr;
begin
  Result:=-1; StrNameExt(nm1,APath);

  for i:=0 to Count-1 do
  if Get_map(i,rec) then begin
    StrNameExt(nm2,rec.Path);
    if StrIComp(nm1,nm2) = 0 then begin
      Result:=i; Break
    end
  end
end;

function TMapBank.IdtOf(Str: PChar): Integer;
var
  i: Integer; rec: TMapRec;
begin
  Result:=-1;

  for i:=0 to Count-1 do
  if Get_map(i,rec) then
  if StrIComp(rec.Idt,Str) = 0 then
  begin Result:=i; Break end
end;

function TMapBank.HashOf(AId: Integer): Integer;
var
  i: Integer; rec: TMapRec; nm: TShortstr;
begin
  Result:=-1;

  for i:=0 to Count-1 do
  if Get_map(i,rec) then begin
    StrNameExt(nm,rec.Path);
    if Str_chk(nm) = AId then begin
      Result:=i; Break
    end
  end
end;

procedure TMapBank.Get_rec(Ptr: Integer; var rec: TMapRec);
begin
  fTree._Info(Ptr,rec,SizeOf(rec))
end;

procedure TMapBank.Put_rec(Ptr: Integer; var rec: TMapRec);
begin
  fTree.NodeInfo(Ptr,rec,SizeOf(rec))
end;

function TMapBank.Get_color(Ind: int): int;
var
  rec: TMapRec;
begin
  Result:=0;
  if Ind < 0 then Ind:=fActive;
  if Get_map(Ind,rec) then
  Result:=rec.Color
end;

function TMapBank.Get_map(Ind: Integer; out rec: TMapRec): Boolean;
var
  ptr: Longint;
begin
  Result:=false;

  FillChar(rec,SizeOf(rec),0);

  if Ind >= 0 then begin
    ptr:=fList[Ind];
    if ptr > 0 then begin
      fTree._Info(ptr,rec,SizeOf(rec));
      Result:=true
    end
  end
end;

function TMapBank.Get_trk(Ind: Integer; out Obj): Boolean;
var
  rec: TMapRec; p: PKeyIntfRec;
begin
  Result:=false; TPointer(Obj):=0;

  if Get_map(Ind,rec) then
  if rec.Key > 0 then begin

    p:=fTracks.id_Itemof(rec.Key);
    if Assigned(p) then begin
      TPointer(Obj):=p.Intf;
      Result:=true
    end
  end
end;

procedure TMapBank.Put_map(Ind: Integer; const rec: TMapRec);
var
  ptr: Longint; tmp: TMapRec;
begin
  tmp:=rec;
  ptr:=fList[Ind]; if ptr > 0 then
  fTree.NodeInfo(ptr,tmp,SizeOf(tmp));
end;

procedure TMapBank.visible_map(Ind,Cmd: Integer);
var
  rec: TMapRec;
begin
  if Get_map(Ind,rec) then begin
    if Cmd = 0 then rec.Visible:=false else
    if Cmd = 1 then rec.Visible:=true else
    if Cmd = 2 then rec.Visible:=not rec.Visible;
    Put_map(Ind,rec)
  end
end;

procedure TMapBank.color_map(Ind,Color: Integer);
var
  rec: TMapRec;
begin
  if Get_map(Ind,rec) then begin
    rec.Color:=Color; Put_map(Ind,rec)
  end
end;

procedure TMapBank.flags_map(Ind,Flags: Integer);
var
  rec: TMapRec;
begin
  if Get_map(Ind,rec) then begin
    Set_map_flags(rec,Flags);
    Put_map(Ind,rec)
  end
end;

function TMapBank.disp_map(Ind: Integer; out rec: TMapRec): Boolean;
begin
  Result:=false; if Get_map(Ind,rec) then
  Result:=rec.Visible and rec.Enabled
end;

procedure TMapBank.trk_map(Ind: Integer; trk: IGpsTrack);
var
  rec: TMapRec; p: TKeyIntfRec;
begin
  if Get_map(Ind,rec) then begin
    Inc(fTrkKey);
    rec.Key:=fTrkKey;
    Put_map(Ind,rec);

    p.Key:=rec.Key;
    p.Intf:=TPointer(trk);
    fTracks.Add(@p);
    trk._AddRef
  end
end;

function TMapBank.Add_map(par: Longint;
                          var rec: TMapRec): Integer;
var
  ptr: Longint; pos: TPoint;
begin
  Result:=-1;

  with Tree do
  if par <= 0 then begin
    if (TopP <> _Child(Root))
    or (RunP = TopP)
    or (_Link(RunP) <> TopP) then begin
      Ring_Down(Root); Last_RunP
    end
  end else
  if (TopP = 0) or (TopP = Root) then begin
    Ring_Down(Root); Last_RunP; par:=0
  end;

  rec.Index:=-1;
  ptr:=Tree.Ins_Node(rec,SizeOf(rec));
  if ptr > 0 then begin

    if par <= 0 then begin
      rec.Index:=fList.Insert(-1,ptr);
      Tree.NodeInfo(ptr,rec,Sizeof(rec));
    end
    else begin
      get_position(pos); Refresh_list;
      Tree._Info(ptr,rec,Sizeof(rec));
      set_position(pos)
    end;

    Result:=rec.Index
  end
end;

function TMapBank.Active_map(var rec: TMapRec): Boolean;
begin
  Result:=Get_map(fActive,rec)
end;

function TMapBank.GetEnabled: Boolean;
begin
  Result:=false;
  if Mem.vm_Active then
  if Tree.Root > 0 then
  Result:=true
end;

procedure TMapBank.get_position(out pos: TPoint);
begin
  pos.X:=fList[fActive];
  pos.Y:=0; if fVideo > 0 then
  pos.Y:=fList[fVideo-1];
end;

procedure TMapBank.set_position(const pos: TPoint);
begin
  Active:=fList.IndexOf(pos.X);
  fVideo:=0; if pos.Y > 0 then
  fVideo:=fList.IndexOf(pos.Y)+1
end;

function TMapBank.Swap_maps(i1,i2: Integer): Boolean;
var
  r1,r2: TMapRec;
begin
  Result:=false;

  if Get_map(i1,r1) then
  if Get_map(i2,r2) then begin
    Put_map(i1,r2); Put_map(i2,r1);

    if i1 = Active then Active:=i2 else
    if i2 = Active then Active:=i1;

    Result:=true
  end
end;

function TMapBank.Pickup(skip,rw: Integer;
                         const r: xgeoid;
                         out map: TMapRec): Integer;
var
  i,j,n,mode: Integer; rec: TMapRec; g: TGauss;
begin
  Result:=-1;
  Fillchar(map,Sizeof(map),0);

  n:=Count; mode:=0;
  if rw = 1 then Inc(mode,prj_readonly);

  j:=-1; if skip >= 0 then j:=skip;

  for i:=1 to n do begin
    Inc(j); if j = n then j:=0;

    if j <> skip then
    if Get_map(j,rec) then

    if rec.Visible then
    if rec.Enabled or (n = 1) then

    if rec.flags and mode = 0 then begin
      g:=prj_inside(r,rec.lg.s);
      if GaussContainsPoint(rec.x_lt,rec.x_rb,g) then
      begin map:=rec; Result:=j; Break end
    end
  end
end;

procedure TMapBank.SetItemIndex(Ind: Integer);
begin
  fItemIndex:=0;
  if Ind >= 0 then
  if Ind < fList.Count then
  fItemIndex:=Ind
end;

procedure TMapBank.SetActive(Ind: Integer);
begin
  fActive:=0;
  if Ind >= 0 then
  if Ind < fList.Count then
  fActive:=Ind
end;

procedure TMapBank.SetVideo(Ind: Integer);
begin
  fVideo:=0;
  if Ind > 0 then
  if Ind <= fList.Count then
  fVideo:=Ind
end;

procedure TMapBank.Delete_node(Ptr: Longint);
var
  pos: TPoint; rec: TMapRec;
begin
  Get_rec(Ptr,rec);
  fTracks.deleteKey(rec.Key);

  get_position(pos);

  fTree._Goto(ptr);
  fTree.Del_Node(false);
  Refresh_list;

  set_position(pos)
end;

function TMapBank.Delete_path(Path: PChar): Boolean;
var
  ind: Integer;
begin
  Result:=false; ind:=NameOf(Path);
  if ind >= 0 then Result:=Delete_map(ind)
end;

function TMapBank.Delete_name(Name: PChar): Boolean;
var
  ind: Integer;
begin
  Result:=false; ind:=NameOf(Name);
  if ind >= 0 then Result:=Delete_map(ind)
end;

function TMapBank.Delete_map(Ind: Integer): Boolean;
var
  rec: TMapRec;
begin
  if Assigned(fOnDelete) then
  if Get_map(Ind,rec) then
  fOnDelete(Self,rec.Path);

  Result:=Exclude_map(Ind)
end;

function TMapBank.Exclude_map(Ind: Integer): Boolean;
var
  ptr: Longint;
begin
  Result:=false;

  ptr:=fList[Ind];
  if ptr > 0 then begin
    Delete_node(ptr);
    Result:=true;
  end
end;

function TMapBank.Move_map(I,J: Integer): Boolean;
var
  p1,p2: longint;
begin
  Result:=false;

  p1:=List[I]; p2:=List[J];

  if p1 <> p2 then
  if (p1 > 0) and (p2 > 0) then begin

    p2:=Tree._Pred(p2); if p2 > 0 then
    if Tree.Mov_Node(0,p1,p2) > 0 then begin

      if fActive >= Min(i,j) then
      if fActive <= Max(i,j) then
      if fActive = i then fActive:=j else
      if i > j then Inc(fActive) else Dec(fActive);
      fActive:=Max(0,Min(Count-1,fActive));

      Result:=true
    end
  end

end;

constructor TRelBank.Create;
begin
  inherited;
  fdll_rel:=LoadLibrary('dll_rel.dll');
end;

destructor TRelBank.Destroy;
begin
  fRelief:=nil;

  xFreeLibrary(fdll_rel);
  fdll_rel:=0;

  fNodeIndex:=Point(-1,-1);

  inherited
end;

procedure TRelBank.SetActive(Ind: Integer);
var
  old: Integer; r1,r2: TMapRec;
begin
  old:=Active;
  if not Active_map(r1) then
  ind:=-1;

  inherited SetActive(Ind);

  if not Active_map(r2)
  or (old <> Ind) then old:=-1;

  if old < 0 then
  fNodeIndex:=Point(-1,-1)
end;

procedure TRelBank.ClearData;
begin
  inherited; fRelief:=nil
end;

function TRelBank.Get_Intf1(Path: PChar; var Obj): HResult;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  if fdll_rel >= 32 then
  Result:=dllGetExtInterface(fdll_rel,Path,IRelief1,Obj);
end;

function TRelBank.Get_Intf2(Path: PChar; var Obj): HResult;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  if fdll_rel >= 32 then
  Result:=dllGetExtInterface(fdll_rel,Path,IRelief2,Obj);
end;

function TRelBank.Add(Path: PChar): Boolean;
var
  list: TStringList; n: Integer;
begin
  Result:=false;

  n:=Count;
  list:=TStringList.Create;
  try
    list.Add(Path);
    xAdd(nil,list,clNone,0);
    Result:=Count > n
  finally
    list.Free
  end;
end;

function TRelBank.xAdd(Path: PChar; list: TStrings;
                       Color,Flags: Integer): Boolean;

function this_data(Path: PChar; out rec: TMapRec): Boolean;
var
  rel: IRelief1; i: Integer;
  l,g: GOrient; s: tsys; lt,rb: TGauss;
begin
  Result:=false;
  Fillchar(rec,Sizeof(rec),0);

  if Get_Intf1(Path,rel) = S_OK then
  if rel.GetBound(@l,@g,s) > 0 then begin

    rec.Visible:=true; rec.Enabled:=true;

    Max_Gauss_Bound(@l,4,lt,rb);

    rec.l_lt:=_LGauss(lt.x,lt.y);
    rec.l_rb:=_LGauss(rb.x,rb.y);
    rec.dm_scale:=1000;

    rec.lg.s:=s;
    rec.lg.l:=l; rec.lg.g:=g;

    with rec do begin

      if s.pps = 1 then
      for i:=0 to 3 do with g[i] do
      sys_BL_XY(x,y, s, x,y);
      Max_Gauss_Bound(@g,4,rec.x_lt,rec.x_rb);
    end;

    StrPCopy(rec.Path,Path);

    rec.x_mm:=0; rec.w_mm:=100;
    rec.y_mm:=0; rec.h_mm:=100;

    rec.Color:=clGray; Result:=true
  end
end;

procedure rel_color(var rec: TMapRec; Color: Integer);
var
  l: Integer; fn: TShortstr;
begin
  if Color <> clNone then
    rec.Color:=Color
  else begin
    rec.Color:=clMaroon;

    StrNameExt(fn,rec.Path);
    StrUpdateExt(fn,fn,'');
    l:=Strlen(fn); if l > 0 then
    if fn[l-1] in ['G','g'] then
    rec.Color:=clGreen
  end
end;

var
  i,k: int; rec: TMapRec;
  fn: TShortstr;
begin
  Result:=false; k:=0;

  if Assigned(Path) then begin
    Clear;
    if this_data(Path,rec) then begin
      rel_color(rec,Color);
      Set_map_flags(rec,Flags);
      Add_map(0,rec); Inc(k)
    end
  end;

  if Assigned(list) then
  for i:=0 to list.Count-1 do
  if StrPCopy(fn,list[i]) <> nil then
  if NameOf(fn) < 0 then

  if this_data(fn,rec) then begin
    rel_color(rec,Color);
    Set_map_flags(rec,Flags);
    Add_map(0,rec); Inc(k)
  end;

  if k > 0 then
  Result:=Open_data = S_OK
end;

function TRelBank.Open_data: HResult;
var
  rec: TMapRec;
begin
  Result:=S_FALSE; fRelief:=nil;

  if Active_map(rec) then
  Result:=Get_Intf1(rec.Path,fRelief)
end;

procedure TRelBank.Close_data;
begin
  fRelief:=nil
end;

function TRelBank.Active_data: Boolean;
begin
  Result:=Assigned(fRelief)
end;

function TRelBank.get_mpp: Double;
begin
  Result:=1;
  if Assigned(fRelief) then
  Result:=fRelief.get_mpp
end;

function TRelBank.Get_zval(g: PGeoPoint;
                           out zv: Double): Boolean;
begin
  Result:=false;
  if Assigned(fRelief) then
  Result:=fRelief.GetValue(g,zv) = S_OK;
end;

function TRelBank.GetValue(g: PGeoPoint;
                           out zv: Double): HResult;
var
  rec: TMapRec; i,i1,i2: Integer;
begin
  Result:=S_FALSE; zv:=0;

  if not fLocked then
  if Count > 0 then begin

    i1:=fActive;

    if Assigned(fRelief) then
    Result:=fRelief.GetValue(g,zv);

    if Result <> S_OK then begin
      i:=Pickup(i1,0,g^,rec); i2:=i;
      while i >= 0 do begin
        fActive:=i;
        if Open_data = S_OK then begin

          if Assigned(fRelief) then
          Result:=fRelief.GetValue(g,zv);

          if Result = S_OK then Break;
        end;

        i:=Pickup(i,0,g^,rec);
        if (i = i1) or (i = i2) then Break;
      end
    end;

    if i1 <> fActive then
    if Assigned(fOnData) then
    fOnData(Self)
  end
end;

constructor TRelBankObj.Create(ABank: TRelBank);
begin
  inherited Create; fBank:=ABank
end;

destructor TRelBankObj.Destroy;
begin
  inherited
end;

function TRelBankObj.GetValue(g: PGeoPoint; out zv: Double): HResult;
begin
  Result:=fBank.GetValue(g,zv)
end;

constructor TMapProject.Create;
begin
  inherited Create; isEditor:=true;

  fBank:=TMapBank.Create;
  fMap:=tdm_Edit.Create;

  fBank1:=TRelBank.Create;
  fBank1.GetInterface(IRelief,fRelief);

  fBank2:=TMapBank.Create;
  fList2:=TStringList.Create;

  fBank.OnVisible:=VisibleEvent;
  fBank1.OnVisible:=VisibleEvent;
  fBank2.OnVisible:=VisibleEvent;

  fPcx_link:=tdmwLink.Create;
  Pcx_link.is_pcx:=true;

  fAlxList:=TPlanList.Create;

  fDispAttrs:=TDispAttrList.Create;

  fImages:=TStringList.Create;
  fImages.CaseSensitive:=false;

  fCards:=TList.Create;

  fUserData:=TStringList.Create;
  fUserData.CaseSensitive:=false;

  fBrkLenList:=TPointList.Create(16);

  fsasChart:=TSasChart.Create;
  fsasUser:=TSasChartList.Create(4);

  xStrTempFileName(fTemp,'prj');
  if Strlen(fTemp) > 0 then
  if xCreateStorage(fTemp,fStorage) = S_OK then begin
    Bank.Mem.stm_Create(fStorage,'dm');
    Bank1.Mem.stm_Create(fStorage,'rel');
    Bank2.Mem.stm_Create(fStorage,'img');
  end
  else begin
    Bank.Mem.vm_Make(fTemp);
  end;

  dm_Ground:=7;
  dm_Pencil:=def_Pencil;
  dm_Pencil1:=(10 + (3 shl 5)) +
              ((13 + (3 shl 5)) shl 8);

  cl_Edge:=1;

  with x_Disp do begin
    AreaFill:=true; TextFill:=true; Info:=true;
    Dup:=true; Calc:=true; Tick:=true; Dos:=true;
    dm_Frame:=true; im_Frame:=true;
  end;

  MosaicColor:=clNone;

  Params.joinrMode:=2;
  Params.coRad:=2;

  Reset_Project(false);
end;

destructor TMapProject.Destroy;
begin
  Pcx_link.OnChange:=nil;

  if Assigned(fOnDestroy) then
  fOnDestroy(nil);

  fOnImage:=nil;

  Map.OnOccupe_a:=nil;
  Map.OnEditObject:=nil;
  Map.Group.OnExplore:=nil;
  Map.OnExplore:=nil;

  fsasUser.Free;
  fsasChart.Free;
  
  fBrkLenList.Free;

  fUserData.Free;

  fCards.Free;

  fImages.Free;

  fDispAttrs.Free;

  fAlxList.Free;

  fPcx_link.Free;

  fList2.Free;
  fBank2.Free;

  fRelief:=nil;

  fMap.Free; fBank.Free;

  fStorage:=nil; FileErase(fTemp);

  inherited
end;

function TMapProject.Get_frame: TPrjFrame;
begin
  Result.Count:=Count; Result.sys:=p_lg.s;
  Result.lt:=px_lt; Result.rb:=px_rb;
  StrCopy(Result.dm,Map.Tree.vm_Path);
end;

procedure TMapProject.BeginUpdate;
begin
  Inc(fUpdated)
end;

procedure TMapProject.EndUpdate;
begin
  fUpdated:=Max(0,fUpdated-1)
end;

procedure TMapProject.Changed;
begin
  if Assigned(fOnChanged) then
  fOnChanged(Self)
end;

procedure TMapProject.VisibleEvent(Sender: TObject);
begin
  if Assigned(fOnVisible) then
  fOnVisible(Self)
end;

function TMapProject.GetEditPath: PChar;
begin
  Result:=fEditPath
end;

function TMapProject.GetProfile: PChar;
begin
  Result:=fProfile
end;

procedure TMapProject.SetProfile(Dir: PChar);
begin
  StrCopy(fProfile,'');
  if Strlen(Dir) > 0 then
  if Dir_Exists(Dir) then
  StrCopy(fProfile,Dir)
end;

function TMapProject.GetCount: Integer;
begin
  Result:=Bank.Count
end;

function TMapProject.GetCount2: Integer;
begin
  Result:=Bank.Count + Bank2.Count
end;

function TMapProject.GetVideo: Integer;
begin
  Result:=Bank.Video
end;

function TMapProject.GetEnabled: Boolean;
begin
  Result:=Bank.Enabled
end;

function TMapProject.GetSas: PChar;
begin
  Result:=fSas
end;

procedure TMapProject.SetSas(Dir: PChar);
begin
  StrCopy(fSas,'');
  
  if Assigned(Dir) then
  if Dir_Exists(Dir) then
  StrCopy(fSas,Dir)
end;

procedure TMapProject.SetSasLevel(v: int);
begin
  fsasLevel:=v; Notify(Self,fOnSasLevel)
end;

function TMapProject.GetSasInterface(out Obj: IGoogle): bool;
begin
  Result:=false; Obj:=nil;

  if fsasChart.Active then

  if sasGetInterface(fSas,Obj) = S_OK then begin
    fsasChart.Apply(Obj);
    Obj.SetParam(0,fsasLevel);
    Result:=true
  end
end;

function TMapProject.SetSasMapLayer(Name: PChar): int;
begin
  Result:=fsasChart.SetMapLayer(fSas,Name)
end;

function TMapProject.SetSasSklLayer(Name: PChar; Up: int): int;
begin
  Result:=fsasChart.SetSklLayer(fSas,Name,Up)
end;

procedure TMapProject.pushSasCurrentChart;
begin
  fsasChart.RefreshCaption(fSas);
  if Length(fsasChart.Caption) > 0 then
  sasUser.addChart(sasChart)
end;

function TMapProject.GetBrkThick: int;
begin
  Result:=0;
  if Params.brkMode1 and 4 <> 0 then
  Result:=Params.brkThick
end;

procedure TMapProject.SetBrkLen(code,val: int);
var
  p: PPoint;
begin
  p:=fBrkLenList.id_Itemof(code);

  if val > 0 then begin
    Params.brkLen:=val; if code > 0 then
    if Assigned(p) then p.Y:=val else
    fBrkLenList.AddItem(code,val)
  end else
  if Assigned(p) then
    Params.brkLen:=p.Y
end;

procedure TMapProject.NextMarkRadius;
begin
  with Params do
  if mark_dr = 0 then mark_dr:=7
                 else mark_dr:=0
end;

function TMapProject.prj_Scale: Double;
begin
  Result:=xResolution(px_Width,px_Height) / xMetersPerPixel(0)
end;

function TMapProject.px_Width: Double;
begin
  Result:=px_rb.y-px_lt.y
end;

function TMapProject.px_Height: Double;
begin
  Result:=px_rb.x-px_lt.x
end;

function TMapProject.sm_Width: longint;
begin
  Result:=Round(px_Width * px_ed)
end;

function TMapProject.sm_Height: longint;
begin
  Result:=Round(px_Height * px_ed)
end;

procedure TMapProject.Get_prj_3x3(out T: Real3x3);
begin
  T[1,1]:=0;  T[1,2]:=1; T[1,3]:=-px_lt.y;
  T[2,1]:=-1; T[2,2]:=0; T[2,3]:=px_rb.x;
  T[3,1]:=0;  T[3,2]:=0; T[3,3]:=1;
end;

procedure TMapProject.Push_DispAttrs;
begin
  if Assigned(fStorage) then
  DispAttrs.doc_SaveAs(fStorage,'DispAttrs')
end;

procedure TMapProject.Pop_DispAttrs;
begin
  if Assigned(fStorage) then
  DispAttrs.doc_LoadFrom(fStorage,'DispAttrs')
end;

procedure TMapProject.GetWorkPath(Dest,Name: PChar);
begin
  StrWorkPath(Dest,Name);
  if Strlen(fProfile) > 0 then
  if Dir_Exists(fProfile) then
  StrPath(Dest,fProfile,Name);
end;

procedure TMapProject.GetIntfPath(Dest,Name: PChar);
begin
  StrIntfPath(Dest,Name);
  if Strlen(fProfile) > 0 then
  if Dir_Exists(fProfile) then
  StrPath(Dest,fProfile,Name);
end;

function TMapProject.GetDmwFolder(Dir: PChar): Boolean;
var
  fn: TShortstr;
begin
  dmw_get_dm_dir(dir);

  if Count > 0 then begin
    Get_Active_Path(fn);
    StrDirectory(dir,fn)
  end;

  Result:=StrLen(Dir) > 0
end;

function TMapProject.GetAlxFolder(Dir: PChar): Boolean;
var
  fn: TShortstr;
begin
  dmw_get_dm_dir(dir);

  if Get_alx_Path(fn) then
    StrDirectory(dir,fn)
  else
  if Count > 0 then begin
    Get_Active_Path(fn);
    StrDirectory(dir,fn)
  end else
  if Get_rel_Path(fn) then
    StrDirectory(dir,fn);

  Result:=StrLen(Dir) > 0
end;

function TMapProject.GetRelFolder(Dir: PChar): Boolean;
var
  fn: TShortstr;
begin
  dmw_get_dm_dir(dir);

  if Get_rel_Path(fn) then
    StrDirectory(dir,fn)
  else
  if Get_alx_Path(fn) then
    StrDirectory(dir,fn)
  else
  if Count > 0 then begin
    Get_Active_Path(fn);
    StrDirectory(dir,fn)
  end;

  Result:=StrLen(Dir) > 0
end;

function TMapProject.GetTifFolder(Dir: PChar): Boolean;
var
  rec: TMapRec; fn: TShortstr;
begin
  StrCopy(dir,'');

  if Bank2.Get_map(0,rec) then
    StrDirectory(dir,rec.Path)
  else
  if Get_alx_Path(fn) then
    StrDirectory(dir,fn)
  else
  if Count > 0 then begin
    Get_Active_Path(fn);
    StrDirectory(dir,fn)
  end;

  Result:=StrLen(Dir) > 0
end;

function TMapProject.GetActiveMap: Integer;
begin
  Result:=Bank.Active
end;

procedure TMapProject.SetActiveMap(Ind: Integer);
var
  i: int; rec: TMapRec; fn: TShortStr;
begin
  if Ind = Bank.Active then

  if Bank.Get_map(Ind,rec) then
  if This_ext(rec.Path,'.alx') then

  for i:=0 to Bank.Count-1 do
  if Bank.Get_map(i,rec) then
  if not This_ext(rec.Path,'.alx') then
  begin Ind:=i; Break end;

  Bank.Active:=Ind;

  if Bank.Active_map(rec) then begin
    Str_dm_Path(fn,rec.Path);

    if not xStrThis(fn,Map.Tree.vm_Path) then begin
      Map.Active_Map(fn); Map.Close_Map;
    end;

    Map.dm_Index:=Bank.Active;
  end
  else Map.Close_Edit_Map;

  auto_Resize;

  if Assigned(fOnChangeMap) then
  fOnChangeMap(Self)
end;

procedure TMapProject.txt_Load_Options(txt: TTextfile);
var
  v: int; x: Int64;
begin
  while txt.xStrLine <> nil do begin
    if StrLIComp(txt.str,'[end_',5) = 0 then Break;

    if txt.x_key_int('Options',v) then Unpack_x_Disp(v) else
    if txt.x_key_int('Ground',v) then dm_Ground:=v else
    if txt.x_key_int('Pencil',v) then dm_Pencil:=v else
    if txt.x_key_int('Pencil1',v) then dm_Pencil1:=v else
    if txt.x_key_int('Grid',v) then cl_Grid:=v else
    if txt.x_key_int('Edge',v) then cl_Edge:=v else
    if txt.x_key_int64('Disp1',x) then x_Disp1:=x else
    if txt.x_key_int('Mosaic',v) then MosaicColor:=v else
    if txt.x_key_int('MosaicAlf',v) then MosaicAlfa:=v else
    if txt.x_key_int('MosaicClip',v) then MosaicClip:=v else
    if txt.x_key_int('bak_dt',v) then bak_dt:=v*60 else
    if txt.x_key_int('bak_dt1',v) then bak_dt:=v
  end
end;

procedure TMapProject.txt_Load_env(txt: TTextfile);
var
  v,v1: int; x: Int64; r: Double; f: float; fn: TShortstr;
begin
  fBrkLenList.Clear;

  while txt.xStrLine <> nil do begin
    if StrLIComp(txt.str,'[end_',5) = 0 then Break;

    if txt.x_key_int('BankScale1',v) then Bank.BankScale1:=v else
    if txt.x_key_int('BankScale2',v) then Bank.BankScale2:=v else

    if txt.x_key_int('Bank1Scale1',v) then Bank1.BankScale1:=v else
    if txt.x_key_int('Bank1Scale2',v) then Bank1.BankScale2:=v else

    if txt.x_key_int('Bank2Scale1',v) then Bank2.BankScale1:=v else
    if txt.x_key_int('Bank2Scale2',v) then Bank2.BankScale2:=v else

    if txt.x_key_int('BankActive',v) then Bank.Active:=v else

    if txt.x_key_double('mm_w',r) then mm_w:=r else
    if txt.x_key_double('mm_h',r) then mm_h:=r else
    if txt.x_key_int('mm_on',v) then is_mm:=v <> 0 else

    if txt.x_key_int('mark_dr',v) then begin
      if v in [0..7] then Params.mark_dr:=v
    end else

    if txt.x_key_int('brkLen',v) then Params.brkLen:=v else
    if txt.x_key_int('brkAuto',v) then Params.brkAuto:=v else
    if txt.x_key_int('brkMode',v) then Params.brkMode:=v else
    if txt.x_key_int('brkMode1',v) then Params.brkMode1:=v else
    if txt.x_key_int('brkMax',v) then Params.brkMax:=v else
    if txt.x_key_int('brkPull',v) then Params.brkPull:=v else
    if txt.x_key_int('brkPer',v) then Params.brkPer:=v else
    if txt.x_key_int('brkSharp',v) then Params.brkSharp:=v else
    if txt.x_key_int('brkThick',v) then Params.brkThick:=v else

    if txt.x_key_int2('brkLen1',v,v1) then fBrkLenList.AddItem(v,v1) else

    if txt.x_key_int('joinrMode',v) then Params.joinrMode:=v else

    if txt.x_key_float('cls1',f) then Params.cls[0]:=f else
    if txt.x_key_float('cls2',f) then Params.cls[1]:=f else
    if txt.x_key_float('cls3',f) then Params.cls[2]:=f else

    if txt.x_key_int('coRad',v) then Params.coRad:=v else

    if txt.x_key_int('p_eps',v) then Params.p_eps:=v else
    if txt.x_key_int('p_ed',v) then Params.p_ed:=v else
    if txt.x_key_int('mf_nn',v) then Params.mf_nn:=v else
    if txt.x_key_int('sqr_nn',v) then Params.sqr_nn:=v else
    if txt.x_key_int('find_nn',v) then Params.find_nn:=v else
    if txt.x_key_int('blankText',v) then Params.blankText:=v else

    if txt.x_key_str('pcx2',fn) then begin
      if FileExist(fn) then StrCopy(fPhoto,fn)
    end else

    if txt.x_key_int('pcx_rel',v) then
      fpcx_rel:=v = 1
    else
    if txt.x_key_int('jpg_cutx',v) then
      fjpg_cut.X:=v
    else
    if txt.x_key_int('jpg_cuty',v) then
      fjpg_cut.Y:=v

  end
end;

procedure TMapProject.txt_Load_sas(txt: TTextfile);
var
  v: int; s: TShortstr;
begin
  fsasLevel:=0;
  fsasChart.Clear;

  while txt.xStrLine <> nil do begin
    if StrLIComp(txt.str,'[end_',5) = 0 then Break;

    if txt.x_key_str('cash',s) then begin
      if dir_Exists(s) then StrCopy(fSAS,s)
    end else
    if txt.x_key_str('chart',s) then
      fsasChart.StrToChart(s)
    else
    if txt.x_key_int('level',v) then
      fsasLevel:=v
  end;

  if not fsasInit then
  fsasChart.Enabled:=IsOcx;

  fsasInit:=true
end;

procedure TMapProject.txt_user_data(txt: TTextfile; key: PChar);
begin
  fUserData.Add(key);
  while txt.xStrLine <> nil do begin
    fUserData.Add(txt.str);
    if StrLIComp(txt.str,'[end_',5) = 0 then Break;
  end
end;

procedure TMapProject.xLoad_Options(const stg: IStorage);
var
  opt: TPrjOptions;
begin
  if xGetFromStream(stg,'opt',opt,Sizeof(opt)) then begin
    Unpack_x_Disp(opt.Options);

    dm_Ground:=opt.dm_Ground;
    dm_Pencil:=opt.dm_Pencil;
    dm_Pencil1:=opt.dm_Pencil1;
    cl_Grid:=opt.cl_Grid;
    cl_Edge:=opt.cl_Edge;

    x_Disp1:=opt.x_Disp1;
    MosaicColor:=opt.MosaicColor;
    MosaicAlfa:=opt.MosaicAlfa;
    MosaicClip:=opt.MosaicClip;
    bak_dt:=opt.bak_dt  
  end
end;

procedure TMapProject.Load_Options(APath: PChar);
var
  stg: IStorage;
  txt: TTextfile;
  str: TShortstr;
begin
  if xStgIsStorageFile(APath) then begin
    if xOpenStorage(APath,ole_Read,stg) = S_OK then
    xLoad_Options(stg); stg:=nil
  end else

  if This_Text(APath,str) then
  if StrIComp(str,'#prj') = 0 then begin
    txt:=TTextfile.Create;
    try
      if txt.Open(APath) then
      while txt.xStrLine <> nil do
      if txt.x_str(str) <> nil then

      if str[0] = '[' then
      if StrIComp(str,'[options]') = 0 then
        txt_load_Options(txt)
      else
      if StrIComp(str,'[env]') = 0 then
        txt_Load_env(txt)
      else
      if StrIComp(str,'[sas]') = 0 then
        txt_Load_sas(txt)
      else
      if StrLIComp(str,'[end_',5) <> 0 then begin
        while txt.xStrLine <> nil do
        if StrLIComp(txt.str,'[end_',5) = 0 then Break;
      end
    finally
      txt.Free
    end
  end
end;

procedure TMapProject.Refresh_project;
var
  rec: TMapRec;
begin
  if Bank.Get_map(0,rec) then
  p_lg:=rec.lg; ActiveMap:=Bank.Active;
end;

function TMapProject.LoadFrom(APath: PChar; Opt: Boolean): Integer;

procedure verify_filelist(list: TStrings);
var
  i: Integer; fn: TShortstr;
begin
  i:=0;
  while i < list.Count do begin
    StrPLCopy(fn,list[i],255);
    if FileExist(fn) then Inc(i)
    else list.Delete(0)
  end
end;

function load_prj(APath: PChar;
                  list1: TStrings;
                  out env: TPrjEnv;
                  Opt: Boolean): Boolean;
var
  stg: IStorage; p: PChar; fn: TShortstr;
begin
  Result:=false;

  Fillchar(env,Sizeof(env),0);

  if xStgIsStorageFile(APath) then
  if xOpenStorage(APath,ole_Read,stg) = S_OK then begin

    Bank.LoadFrom(stg,APath,'dm');
    Bank1.LoadFrom(stg,APath,'rel');
    Bank2.LoadFrom(stg,APath,'img');

    if Opt then xLoad_Options(stg);

    DispAttrs.doc_LoadFrom(stg,'DispAttrs');
    DispAttrs.doc_SaveAs(fStorage,'DispAttrs');

    if xGetFromStream(stg,'env1',env,Sizeof(env)) then begin
      p:=StrFromStream(stg,'Images',nil,0);
      if Assigned(p) then list1.SetText(p);
      xFreePtr(p); verify_filelist(list1);
    end
    else begin
      Fillchar(env,Sizeof(env),0);
      env.edit:=CardFrameIdentify;
      env.plan:=CardFrameIdentify;
      env.tiff:=CardFrameIdentify;
    end;

    Back_relief; Result:=true

  end; stg:=nil
end;

const
  ext: Array[0..4] of Char5 =
  ('\alx'#0,'\map'#0,'\rlz'#0,'\img'#0,'\img'#0);

var
  txt: TTextfile;
  list1,tmp: TStringList;
  rec: TMapRec; env: TPrjEnv;
  ind,tag,cl,fl, v1,v2: int;
  dir,dir1,fn,nm,mag: TShortstr;
  fr: TCardFrame; gc: TCardCentre;
begin
  Reset_Project(false);

  Fillchar(win_gc,Sizeof(win_gc),0);
  win_gc.sc:=-1; win_gc.pos.s.pps:=-1;
  gc:=win_gc;

  list1:=TStringList.Create;
  tmp:=TStringList.Create;
  try
    list1.CaseSensitive:=false;

    Fillchar(env,Sizeof(env),0);
    fUserData.Clear;

    if Enabled then

    if This_Text(APath,mag) then begin

      BeginUpdate;

      txt:=TTextfile.Create;
      try
        StrDirectory(dir,APath); tag:=0;

        if txt.Open(APath) then

        if StrIComp(mag,'#prj') <> 0 then begin

          while txt.xStrLine <> nil do
          if txt.x_str(fn) <> nil then
          if FileExist(fn) then

          if this_ext(fn,'.dm')
          or this_ext(fn,'.alx') then
          Insert_Map(fn,0,0,0,0)

        end
        else begin
          while txt.xStrLine <> nil do
          if txt.x_str(fn) <> nil then begin

            if fn[0] = '[' then begin tag:=255;
              if StrIComp(fn,'[alx]') = 0 then  tag:=0 else
              if StrIComp(fn,'[dm]') = 0 then   tag:=1 else
              if StrIComp(fn,'[rlz]') = 0 then  tag:=2 else
              if StrIComp(fn,'[tif]') = 0 then  tag:=3 else
              if StrIComp(fn,'[trk]') = 0 then tag:=4 else
              if StrIComp(fn,'[img]') = 0 then tag:=5 else
              if StrIComp(fn,'[frames]') = 0 then tag:=6 else
              if StrIComp(fn,'[centre]') = 0 then tag:=7 else

              if StrIComp(fn,'[options]') = 0 then begin
                if Opt then txt_load_Options(txt)
              end else

              if StrIComp(fn,'[env]') = 0 then
                txt_load_env(txt)
              else
              if StrIComp(fn,'[sas]') = 0 then
                txt_load_sas(txt)
              else

              if StrLIComp(fn,'[end_',5) <> 0 then

              if Assigned(fOnLoadFrom) then
                fOnLoadFrom(Self,txt,fn)
              else
                txt_user_data(txt,fn)

            end else

            if tag in [0..5] then begin

              if StrIComp(fn,'#prj') <> 0 then begin

                if FileExist(fn) then begin
                  StrPCopy(dir1,ExtractFileDir(fn));
                  if Strlen(dir1) = 0 then
                  StrPath(fn,dir,fn)
                end
                else begin
                  StrNameExt(nm,fn);
                  StrPath(fn,dir,nm);
                  if not FileExist(fn) then begin
                    StrCopy(dir1,dir);
                    StrLCat(dir1,@ext[tag],255);
                    StrPath(fn,dir1,nm);
                  end
                end;

                if FileExist(fn) then begin

                  cl:=clNone; fl:=0;
                  if tag in [2,3,4] then begin
                    if not txt.x_Int(cl) then cl:=clNone;
                    if not txt.x_Int(fl) then fl:=0;
                  end;

                  case tag of
                0,
                1:  begin
                      ind:=Insert_Map(fn,0,0,0,0);
                      if ind >= 0 then begin

                        if txt.x_Int(cl) then
                        if tag = 0 then Bank.color_map(ind,cl);

                        if txt.x_Int(fl) then
                        Bank.flags_map(ind,fl);

                        if txt.x_Int(v1) then
                        if txt.x_Int(v2) then begin
                          Bank.Get_map(ind,rec);
                          rec.scale1:=v1; rec.scale2:=v2;
                          Bank.Put_map(ind,rec);
                        end
                      end
                    end;

                2:  begin tmp.Add(fn);
                      Bank1.xAdd(nil,tmp,cl,fl);
                      tmp.Clear
                    end;

                3:  begin
                      txt.x_Int(v1); txt.x_Int(v2);
                      tif_add(fn,cl,fl,v1,v2,false);
                    end;

                5:  list1.Add(fn);

                  end
                end
              end

            end else
            if tag = 6 then begin
              if GetCardFrame(txt,fr) then

              if StrIComp(fn,'edit_win') = 0 then
                win_edit:=fr
              else
              if StrIComp(fn,'plan_win') = 0 then
                win_plan:=fr
              else
              if StrIComp(fn,'tiff_win') = 0 then
                win_tiff:=fr

            end else
            if tag = 7 then begin
              if txt.x_Int(gc.pos.s.pps) then
              if txt.x_Double(gc.pos.x) then
              if txt.x_Double(gc.pos.y) then
              txt.x_Double(gc.sc)
            end
          end;

          if This_ext(APath,'.prj') then
          StrCopy(fEditPath,APath);
        end;
      finally
        txt.Free
      end;

      EndUpdate; Refresh_project;

      if gc.sc > 0 then begin
        win_gc:=gc; win_gc.pos.s:=p_lg.s;
        win_gc.pos.s.pps:=gc.pos.s.pps;
      end;

      if list1.Count > 0 then
      Open_Image1(list1,nil,0,false);

      StrUpdateExt(fn,APath,'.atr');
      fDispAttrs.LoadFrom(fn);

      Push_DispAttrs;

    end else

    if not load_prj(APath,list1,env,Opt) then

      Reset_Project(false)

    else begin

      if This_ext(APath,'.prj') then
      StrCopy(fEditPath,APath);

      win_edit:=env.edit;
      win_plan:=env.plan;
      win_tiff:=env.tiff;

      Params:=env.Params;

      mm_w:=env.mm_w;
      mm_h:=env.mm_h;
      is_mm:=env.mm_on = 1;

      d3_on:=env.d3_on;
      d3_fov:=env.d3_fov;
      d3_teta:=env.d3_teta;

      Bank.BankScale1:=env.BankScale1;
      Bank.BankScale2:=env.BankScale2;

      Bank1.BankScale1:=env.Bank1Scale1;
      Bank1.BankScale2:=env.Bank1Scale2;

      Bank2.BankScale1:=env.Bank2Scale1;
      Bank2.BankScale2:=env.Bank2Scale2;

      fStereo.Id:=env.stereoId;
      StrCopy(fPhoto,env.Photo);

      Refresh_project;

      if FileExist(env.Image) then

      if list1.Count > 0 then
        Open_Image1(list1,nil,env.stereoId,false)
      else
        Open_Image1(nil,env.Image,env.stereoId,false);

      if win_edit.iv[3] = 1 then
      win_edit.iv[3]:=ord( FileExist(Image) );
    end;

  finally
    tmp.Free;
    list1.Free
  end;

  Get_alx_prj(fAlxList);
  Bank.ResetAllBak;

  Result:=Count
end;

function TMapProject.SaveAs(APath: PChar; IsTxt: Boolean): PChar;

function SaveAs_txt(APath: PChar;
                    const opt: TPrjOptions): Boolean;
var
  txt: TTextfile; i,m: int;
  lp: PLPoly; g: TGauss;
  fn,s2: TShortstr; s1: String;
begin
  Result:=false;

  txt:=TTextfile.Create;
  try
    if txt.Make(APath) then begin
      txt.WriteStr('#prj');

      StrDirectory(fn,APath);

      if Bank.Dump(fn,nil,1) > 0 then begin
        txt.WriteStr('');
        txt.WriteStr('[alx]');
        Bank.Dump(fn,txt,1);
        txt.WriteStr('[end_alx]');
      end;

      txt.WriteStr('');
      txt.WriteStr('[dm]');
      Bank.Dump(fn,txt,2);
      txt.WriteStr('[end_dm]');

      if Bank.Dump(fn,nil,3) > 0 then begin
        txt.WriteStr('');
        txt.WriteStr('[trk]');
        Bank.Dump(fn,txt,3);
        txt.WriteStr('[end_trk]');
      end;

      if Bank1.Count > 0 then begin
        txt.WriteStr('');
        txt.WriteStr('[rlz]');
        Bank1.Dump(fn,txt,0);
        txt.WriteStr('[end_rlz]');
      end;

      if Bank2.Count > 0 then begin
        txt.WriteStr('');
        txt.WriteStr('[tif]');
        Bank2.Dump(fn,txt,0);
        txt.WriteStr('[end_tif]');
      end;

      if fImages.Count > 0 then begin
        txt.WriteStr('');
        txt.WriteStr('[img]');
        txt.WriteFiles(fImages);
        txt.WriteStr('[end_img]');
      end;

      txt.WriteStr('');
      txt.WriteStr('[frames]');

      if win_gc.sc > 0 then
      if win_gc.pos.s.pps >= 0 then

      with win_gc do begin

        m:=2; g.x:=pos.x; g.y:=pos.y;
        if pos.s.pps = 1 then begin
          g.x:=g.x*180/Pi;
          g.y:=g.y*180/Pi;
          m:=6
        end;

        s1:='edit_gc '+
            RealToStr(sc,6)+' '+
            RealToStr(g.x,m)+' '+
            RealToStr(g.y,m);

        StrSys(s2,-1,pos.s);

        txt.WriteStr(s1+' '+Strpas(s2))
      end;

      if win_edit.iv[0] > 0 then
      txt.WriteStr('edit_win '+CardFrameStr(win_edit));

      if win_plan.iv[0] > 0 then
      txt.WriteStr('plan_win '+CardFrameStr(win_plan));

      if win_tiff.iv[0] > 0 then
      txt.WriteStr('tiff_win '+CardFrameStr(win_tiff));
      txt.WriteStr('[end_frames]');

      txt.WriteStr('');
      txt.WriteStr('[options]');
        txt.Write_int('Options',opt.Options);
        txt.Write_int('Ground',opt.dm_Ground);
        txt.Write_int('Pencil',opt.dm_Pencil);
        txt.Write_int('Pencil1',opt.dm_Pencil1);
        txt.Write_int('Grid',opt.cl_Grid);
        txt.Write_int('Edge',opt.cl_Edge);
        txt.Write_int64('Disp1',opt.x_Disp1);
        txt.Write_int('Mosaic',opt.MosaicColor);
        txt.Write_int('MosaicAlfa',opt.MosaicAlfa);
        txt.Write_int('MosaicClip',opt.MosaicClip);
        txt.Write_int('bak_dt1',opt.bak_dt);
      txt.WriteStr('[end_options]');

      txt.WriteStr('');
      txt.WriteStr('[env]');

        txt.Write_int('BankScale1',Bank.BankScale1);
        txt.Write_int('BankScale2',Bank.BankScale2);

        txt.Write_int('Bank1Scale1',Bank1.BankScale1);
        txt.Write_int('Bank1Scale2',Bank1.BankScale2);

        txt.Write_int('Bank2Scale1',Bank2.BankScale1);
        txt.Write_int('Bank2Scale2',Bank2.BankScale2);

        txt.Write_int('BankActive',Bank.Active);

        txt.Write_real('mm_w',mm_w,1);
        txt.Write_real('mm_h',mm_h,1);
        txt.Write_int('mm_on',ibool[is_mm]);

        txt.Write_int('mark_dr',Params.mark_dr);

        txt.Write_int('brkLen',Params.brkLen);
        txt.Write_int('brkAuto',Params.brkAuto);
        txt.Write_int('brkMode',Params.brkMode);
        txt.Write_int('brkMode1',Params.brkMode1);
        txt.Write_int('brkMax',Params.brkMax);
        txt.Write_int('brkPull',Params.brkPull);
        txt.Write_int('brkPer',Params.brkPer);
        txt.Write_int('brkSharp',Params.brkSharp);
        txt.Write_int('brkThick',Params.brkThick);

        txt.Write_int('joinrMode',Params.joinrMode);

        txt.Write_real('cls1',Params.cls[0],3);
        txt.Write_real('cls2',Params.cls[1],3);
        txt.Write_real('cls3',Params.cls[2],3);

        txt.Write_int('coRad',Params.coRad);

        txt.Write_int('p_eps',Params.p_eps);
        txt.Write_int('p_ed',Params.p_ed);
        txt.Write_int('mf_nn',Params.mf_nn);
        txt.Write_int('sqr_nn',Params.sqr_nn);
        txt.Write_int('find_nn',Params.find_nn);
        txt.Write_int('blankText',Params.blankText);

        lp:=fBrkLenList.First;
        for i:=0 to fBrkLenList.Count-1 do
        txt.Write_int2('brkLen1',lp[i].X,lp[i].Y);

        if Strlen(fPhoto) > 0 then
        txt.Write_str2('pcx2',fPhoto);

        txt.Write_int('pcx_rel',ibool[fpcx_rel]);

        txt.Write_int('jpg_cutx',fjpg_cut.X);
        txt.Write_int('jpg_cuty',fjpg_cut.Y);

      txt.WriteStr('[end_env]');

      if dir_Exists(fSAS) then begin
        txt.WriteStr('');
        txt.WriteStr('[sas]');
          txt.Write_str2('cash',fSAS);

          if sasChart.StrChart(s2) then
          txt.Write_str2('chart',s2);

          txt.Write_int('level',fSASLevel);
        txt.WriteStr('[end_sas]');
      end;

      OTypes.Notify(txt,fOnSaveAs);

      if fUserData.Count > 0 then begin
        txt.WriteStr('');
        txt.WriteStrings(fUserData);
      end; fUserData.Clear;

      Result:=true
    end;
  finally
    txt.Free
  end;

  StrUpdateExt(fn,APath,'.atr');
  fDispAttrs.SaveAs(fn);
end;

var
  stg: IStorage;
  opt: TPrjOptions;
  env: TPrjEnv;
begin
  Result:=nil;

  Fillchar(opt,Sizeof(opt),0);
  opt.Options:=Pack_x_Disp(x_Disp);
  opt.dm_Ground:=dm_Ground;
  opt.dm_Pencil:=dm_Pencil;
  opt.dm_Pencil1:=dm_Pencil1;
  opt.cl_Grid:=cl_Grid;
  opt.cl_Edge:=cl_Edge;
  opt.x_Disp1:=x_Disp1;
  opt.MosaicColor:=MosaicColor;
  opt.MosaicAlfa:=MosaicAlfa;
  opt.MosaicClip:=MosaicClip;
  opt.bak_dt:=bak_dt;

  if not IsTxt then
  IsTxt:=Bank.Count <= 32;

  if IsTxt then begin
    if SaveAs_txt(APath,opt) then begin
      if This_ext(APath,'.prj') then
      StrCopy(fEditPath,APath);

      Result:=APath;
    end
  end else
  if xCreateStorage(APath,stg) = S_OK then begin

    Bank.CopyTo(stg);  // dm
    Bank1.CopyTo(stg); // rlz
    Bank2.CopyTo(stg); // tif

    xSaveAsStream(stg,'opt',@opt,Sizeof(opt));

    FillChar(env,SizeOf(env),0);
    env.edit:=win_edit;
    env.plan:=win_plan;
    env.tiff:=win_tiff;

    env.Params:=Params;

    env.mm_w:=mm_w;
    env.mm_h:=mm_h;
    env.mm_on:=ibool[is_mm];

    env.stereoId:=fStereo.Id;

    env.d3_on:=d3_on;
    env.d3_fov:=d3_fov;
    env.d3_teta:=d3_teta;

    env.BankScale1:=Bank.BankScale1;
    env.BankScale2:=Bank.BankScale2;

    env.Bank1Scale1:=Bank1.BankScale1;
    env.Bank1Scale2:=Bank1.BankScale2;

    env.Bank2Scale1:=Bank2.BankScale1;
    env.Bank2Scale2:=Bank2.BankScale2;

    StrCopy(env.Image,Image);
    StrCopy(env.Photo,fPhoto);

    xSaveAsStream(stg,'env1',@env,Sizeof(env));

    if fImages.Count > 0 then
    xStrAsStream(stg,'Images',fImages.GetText);

    if DispAttrs.Count > 0 then
    DispAttrs.doc_SaveAs(stg,'DispAttrs');

    if This_ext(APath,'.prj') then
    StrCopy(fEditPath,APath);

    Result:=APath;
  end;

  stg:=nil
end;

procedure TMapProject.Reset_Project(im: Boolean);
var
  id: Integer; fn: TShortStr;
begin
  StrCopy(fn,Image); id:=fStereo.Id;

  Close_pcx; im_dll_Free;

  Bank2.Clear;
  Bank1.Clear;

  StrCopy(fEditPath,'');

  px_lt.x:=0; px_rb.x:=10000;
  px_lt.y:=0; px_rb.y:=10000;
  px_ed:=100;

  FillChar(p_lg,SizeOf(p_lg),0);

  StrCopy(Image,'');
  StrCopy(fPhoto,''); sm_T.Init;

  pcx_link.Close_Link;

  win_edit:=CardFrameIdentify;
  win_plan:=CardFrameIdentify;
  win_tiff:=CardFrameIdentify;

  is_mm:=false;

  if im then Open_image1(nil,fn,id,true);

  Map.Close_Edit_Map; Bank.Clear;

  fAlxList.Clear; auto_Resize
end;

procedure TMapProject.Assign(Prj: TMapProject; flags: uint);
begin
  Bank.Assign(Prj.Bank);

  if flags and 1 <> 0 then
  Bank1.Assign(Prj.Bank1);

  if flags and 2 <> 0 then
  Bank2.Assign(Prj.Bank2);

  if flags and 4 <> 0 then
  StrCopy(Image,Prj.Image);

  if flags and 8 <> 0 then begin
    fsasLevel:=Prj.sasLevel;
    fsasChart.Enabled:=Prj.sasChart.Enabled;
    fsasChart.Assign(Prj.sasChart);
  end;

  Refresh_project;
  fAlxList.Clear;

  is_mm:=Prj.is_mm;
  mm_w:=Prj.mm_w;
  mm_h:=Prj.mm_h;

  Changed
end;

function TMapProject.Get_alx_project: Boolean;
var
  rec: TMapRec;
begin
  Result:=false;
  if Bank.Get_map(0,rec) then
  Result:=This_ext(rec.Path,'.ALX')
end;

function TMapProject.GetPhoto: PChar;
begin
  Result:=nil;
  if (fPhoto[0] = '$')
  or FileExist(fPhoto) then
  Result:=fPhoto
end;

procedure TMapProject.SetPhoto(Path: PChar);
begin
  StrCopy(fPhoto,'');
  if Assigned(Path) then

  if (Path[0] = '$')
  or FileExist(Path) then
    StrCopy(fPhoto,Path)
end;

procedure TMapProject.Hide_relief;
begin
  Bank1.Close_data;
  Bank1.Locked:=true;
  im_dll_Done;
end;

function TMapProject.Back_relief: Boolean;
begin
  Bank1.Locked:=false;
  Result:=Bank1.Open_data = S_OK;
end;

procedure TMapProject.Update_relief(Sender: TObject; Path: PChar);
begin
  if Assigned(fOnUpdateRel) then
    fOnUpdateRel(Sender,Path)
  else
    rel_after_edit(Path)
end;

function TMapProject.Relief_add(Path: PChar): bool;
begin
  Result:=Bank1.xAdd(Path,nil,clNone,0)
end;

function TMapProject.tif_add(Path: PChar;
                             Color,Flags,Scale1,Scale2: int;
                             upd: Boolean): Boolean;

function this_tif(Path: PChar; out rec: TMapRec): Boolean;
var
  lnk: tdmwLink; kml: IKmlImage;
  g,fr: GOrient; a,b: TGauss;
  i,iw,ih,dll: int; jgw: bool;
  ext: TShortstr;
begin
  Result:=false;
  Fillchar(rec,Sizeof(rec),0);

  StrExt(ext,Path);
  if (StrIComp(ext,'.kml') = 0)
  or (StrIComp(ext,'.kmz') = 0) then begin

    if DllKmlInterface(Path,kml) = S_OK then begin

      kml.GetBound(@fr);

      rec.lg.s:=sys7(1,3,9,0,0,0);

      Fillchar(g,Sizeof(g),0);
      Bound_to_GOrient(fr[0],fr[1],@g);

      rec.lg.g:=g;

      for i:=0 to 3 do with g[i] do
      sys_BL_XY(x,y,rec.lg.s,x,y);

      rec.lg.l:=g;

      Max_Gauss_Bound(@g,4,a,b);

      rec.Visible:=true;
      rec.Enabled:=true;
      rec.l_lt:=_LGauss(a.x,a.y);
      rec.l_rb:=_LGauss(b.x,b.y);
      rec.flags:=prj_tif_kml;

      with rec do
      Max_Gauss_Bound(@g,4,x_lt,x_rb);

      StrPCopy(rec.Path,Path);

      rec.x_mm:=0; rec.w_mm:=100;
      rec.y_mm:=0; rec.h_mm:=100;

      rec.Color:=clGray;
      rec.Rank:=4;

      Result:=true
    end;

    kml:=nil
  end
  else begin
    lnk:=tdmwLink.Create;
    try
      if im_dll_bmp(Path, iw,ih,flags) then
      if lnk.Open_link(Path,iw,ih) then begin

        rec.Visible:=true;
        rec.Enabled:=true;

        rec.l_lt.X:=0; rec.l_rb.X:=iw;
        rec.l_lt.Y:=0; rec.l_rb.Y:=ih;
        rec.dm_scale:=1000;

        with p_lg.s do
        jgw:=(pps = 1) or (prj > 0);

        lnk.Get_lg_Transit(iw,ih,rec.lg,jgw);

        g:=rec.lg.g;
        if rec.lg.s.pps = 1 then begin
          for i:=0 to 3 do with g[i] do
          sys_BL_XY(x,y, rec.lg.s, x,y);
        end;

        with rec do
        Max_Gauss_Bound(@g,4,x_lt,x_rb);

        StrPCopy(rec.Path,Path);

        rec.x_mm:=0; rec.w_mm:=100;
        rec.y_mm:=0; rec.h_mm:=100;

        rec.Color:=clGray;
        rec.Rank:=lnk.Count;

        Result:=true
      end;
    finally
      lnk.Free
    end
  end
end;

var
  i: Integer; rec: TMapRec;
begin
  Result:=false;

  if this_tif(Path,rec) then begin
    i:=Bank2.NameOf(Path);

    if i >= 0 then begin
      if upd then begin
        Bank2.Put_map(i,rec);
        Result:=true
      end
    end
    else begin
      rec.Color:=Color;
      Set_map_flags(rec,Flags);
      rec.scale1:=Scale1;
      rec.scale2:=Scale2;
      Bank2.Add_map(0,rec);
      Bank2.ItemIndex:=Bank2.Count-1;
      Result:=true
    end;

    if Result then
    if Count = 0 then
    p_lg:=rec.lg;
  end;
end;

function TMapProject.DeleteImage1(i: Integer): Integer;
var
  w,h,dll: int; fn: TShortstr;
begin
  Result:=-1;
  if (i >= 0) and (i < fImages.Count) then

  if fImages.Count = 1 then begin
    Close_pcx; Result:=0
  end else
  if i > 0 then begin
    fImages.Delete(i); Result:=i
  end
  else begin
    StrPCopy(fn,fImages[1]);
    if this_image(fn, w,h,dll) then begin
      fImages.Delete(i);
      Pcx_link.Open_link(fn,w,h);
      StrCopy(Image,fn); Result:=0
    end
  end
end;

function TMapProject.Open_image1(List: TStrings; Path: PChar;
                                 Id: Integer; swap: Boolean): Boolean;
var
  w,h,dll,Comp,tile_w,tile_h: Integer;
  scb: TStereo; info: TBitMapInfoHeader;
  fn: TShortstr;
begin
  Result:=false;

  scb:=fStereo; Close_pcx;
  if swap then fStereo.Eye:=scb.Eye;

  StrCopy(fn,'');
  if FileExist(Path) then begin
    fImages.Add(Strpas(Path));
    StrCopy(fn,Path)
  end else
  if Assigned(List) then
  if List.Count > 0 then begin
    StrPCopy(fn,List[0]);
    fImages.AddStrings(List);
  end;

  if FileExist(fn) then

  if This_ext(fn,'.XXX') then begin
    if xxx_Load_Pair(fn,Id,scb) then begin
      if swap then scb.Eye:=fStereo.Eye;
      Stereo:=scb; Result:=true
    end
  end else

  if this_Image(fn, w,h,dll) then begin

    if dll = 1 then
    if im_tif_Info(fn,Comp,tile_w,tile_h) then begin
      fpcx_tiled:=true;
      if im_dll_This(fn,@info) then
      if img_bits(@info) = 8 then
      fpcx_mixed:=true
    end;

    Pcx_link.Open_link(fn,w,h);

    Set_map_sm_T(ActiveMap);

    if isDmw and fpcx_rel then
    if This_ext(fn,'.rel')
    or This_ext(fn,'.rlz') then
    Bank1.xAdd(fn,nil,clNone,0);

    Result:=true
  end;

  if Result then StrCopy(Image,fn);

  if Assigned(fOnImage) then
  fOnImage(Self); Changed
end;

function TMapProject.SwapImage1(i,j: Integer): Boolean;
var
  w,h,dll: Integer; fn: TShortstr;
begin
  Result:=false;

  if i <> j then
  if (i >= 0) and (j >= 0) then
  if i < fImages.Count then
  if j < fImages.Count then begin

    StrPCopy(fn,fImages[i]);
    fImages[i]:=fImages[j];
    fImages[j]:=Strpas(fn);

    if Min(i,j) > 0 then
      Result:=true
    else begin
      StrPCopy(fn,fImages[0]);

      if this_Image(fn, w,h,dll) then begin
        Pcx_link.Open_link(fn,w,h);
        StrCopy(Image,fn); Result:=true
      end
      else begin
        StrPCopy(fn,fImages[i]);
        fImages[i]:=fImages[j];
        fImages[j]:=Strpas(fn);
      end
    end
  end
end;

function TMapProject.Open_pcx(Path: PChar): Boolean;
begin
  Result:=Open_image1(nil,Path,-1,false)
end;

procedure TMapProject.Close_pcx;
var
  fn: TShortstr;
begin
  if isDmw and fpcx_rel then
  if Get_rel_Path(fn) then
  if xStrThis(fn,Image) then
  Bank1.Clear;

  FillChar(fStereo,SizeOf(fStereo),0);
  StrCopy(Image,''); Pcx_link.Close_link;
  fpcx_mixed:=false; fpcx_tiled:=false;
  fImages.Clear;

  if Assigned(fOnImage) then
  fOnImage(Self); Changed
end;

procedure TMapProject.SetImageMirror(Value: Boolean);
var
  tlink: idmwLink; ilink: ILink2;
  Ind: int; tdm: TShortstr; rec: TMapRec;
begin
  if IsLibrary then begin

    StrExeName(tdm,'pcx.tdm');
    Ind:=Bank.NameOf(tdm);

    if not FileExist(Image) then
    Value:=false;

    if Value then begin

      if Ind < 0 then
      if Count = 0 then begin

        tlink:=idmwLink.Create(Pcx_link,Image);
        try
          if tlink.GetInterface(ILink2,ilink) then begin

            StrWorkPath(tdm,tdm);

            if tif_lnk_dm(tdm,Image,'tif',ilink,wgs_mirror) then
            if xInsert_Map(tdm) then begin

              if Bank.Get_map(0,rec) then begin
                rec.Visible:=false;
                Bank.Put_map(0,rec);
              end;
            end;

            ilink:=nil; tlink:=nil;
          end;
        finally
          tlink.Free
        end
      end

    end else
    if Ind >= 0 then Delete_Map(Ind)
  end
end;

procedure TMapProject.SetStereo(const scb: TStereo);
begin
  xxx_lg.Open(@scb.l_xy,@scb.g_xy);
  xxx_gl.Open(@scb.g_xy,@scb.l_xy);
  fStereo:=scb;
end;

procedure TMapProject.StereoLeft(Value: Boolean);
begin
  fStereo.Left:=Value
end;

procedure TMapProject.StereoEye(Value: Boolean);
begin
  fStereo.Eye:=Value
end;

procedure TMapProject.StereoPair(Id: Integer);
var
  scb: TStereo;
begin
  if xxx_Load_Pair(Image,Id,scb) then begin
    scb.Eye:=fStereo.Eye; Stereo:=scb
  end
end;

procedure TMapProject.Stereo_delta_plan(Sender: TObject);
begin
  xxx_Delta_Plan(Image,fStereo)
end;

function TMapProject.Locate_link(const lt,rb: TPoint): Boolean;
var
  g_lt,g_rb: TGauss;
begin
  Result:=false;
  Pcx_link.Open_link(Image,0,0);

  if Pcx_link.Count > 0 then begin
    Get_xy_port(lt,rb, g_lt,g_rb);
    Result:=Pcx_link.Optimize(g_lt,g_rb)
  end
end;

function TMapProject.Change_link(lnk: tdmwLink): tdmwLink;
begin
  Result:=Pcx_link; fPcx_link:=lnk
end;

function TMapProject.IsImage: Boolean;
begin
  Result:=FileExist(Image)
end;

function TMapProject.IsPcxLinked: Boolean;
begin
  Result:=false;

  if Count > 0 then
  if FileExist(Image) then

  if This_ext(Image,'.XXX') then
    Result:=fStereo.Id > 0
  else
    Result:=Pcx_link.Count >= 1
end;

function TMapProject.Is_Stereo: Boolean;
begin
  Result:=FileExist(Image) and
          This_ext(Image,'.XXX') and
          (fStereo.Id > 0)
end;

function TMapProject.Movie_map(dm: tdm_edit): Integer;
var
  ptr: Longint; rec: TMapRec;
begin
  Result:=-1;

  if Bank.Count > 0 then begin

    Fillchar(rec,Sizeof(rec),0); with rec do
    dm.LG_t.Get_Gauss_Bound(l_lt,l_rb,x_lt,x_rb);

    dm.Get_lg_Transit(rec.lg);
    StrPCopy(rec.Path,dm.Tree.vm_Path);

    Result:=Bank.Add_map(0,rec);
    if Result >= 0 then fMap:=dm
  end
end;

procedure TMapProject.Return_map(dm: tdm_edit);
begin
  fMap:=dm;
  if Bank.Count > 1 then
  Bank.Delete_map(Bank.Count-1)
end;

function TMapProject.Get_rel_Path(Path: PChar): Boolean;
var
  rec: TMapRec;
begin
  Result:=false; StrCopy(Path,'');

  if Bank1.Count > 0 then
  if Bank1.Active_map(rec) then
  if StrCopy(Path,rec.Path) <> nil then
  Result:=FileExist(Path)
end;

function TMapProject.Get_alx_Path(Path: PChar): Boolean;
var
  rec: TMapRec;
begin
  Result:=false; StrCopy(Path,'');

  if Bank.Count > 0 then
  if Bank.Get_map(0,rec) then
  if This_ext(rec.Path,'.ALX') then begin
    StrCopy(Path,rec.Path);
    Result:=true
  end
end;

function TMapProject.Get_alx_list(List: TStrings;
                                  fr: PGPoly; vs: Boolean): Integer;
var
  top,run: Longint; rec: TMapRec;
  fn: TShortstr;
begin
  List.Clear;

  with Bank.Tree do begin
    top:=Bank.GetFolder('#alx');
    if top > 0 then top:=_Child(top);

    if top > 0 then begin
      run:=_Link(top);
      while run > top do begin
        Bank.Get_rec(run,rec);
        if This_ext(rec.Path,'.alx') then
        if not vs or rec.Visible then
        if frame_contains_map(fr,rec) then
        list.Add(Strpas(rec.Path));
        run:=_Link(run)
      end
    end
  end;

  if List.Count = 0 then
  if Bank.Get_map(0,rec) then
  if This_ext(rec.Path,'.ALX') then
  if not vs or rec.Visible then
  if frame_contains_map(fr,rec) then
  list.Add(Strpas(rec.Path));

  Result:=list.Count
end;

function TMapProject.Get_all_alx(List: TStrings; vs: Boolean): Integer;
begin
  Result:=Get_alx_list(List,nil,vs)
end;

function TMapProject.Get_alx_prj(Prj: TPlanList): Integer;
var
  i: Integer; rec: TMapRec; r: TPlanItem;
begin
  Prj.Clear;

  for i:=0 to Bank.Count-1 do begin
    Bank.Get_map(i,rec);
    if This_ext(rec.Path,'.alx') then  begin
      r.Id:=i; r.lt:=rec.x_lt; r.rb:=rec.x_rb;
      StrCopy(r.fn,rec.Path); Prj.Add(@r)
    end
  end;

  if Prj.Count > 0 then Prj.ItemIndex:=0;
  Result:=Prj.Count;

  if Assigned(fOnAlx) then fOnAlx(Prj)
end;

function TMapProject.clear_alx_folder: Boolean;
var
  fld: Longint; fn: TShortStr; pos: TPoint;
begin
  Result:=false;

  fld:=Bank.GetFolder('#alx');
  if fld > 0 then
  if Bank.Tree.Is_Childs(fld) > 0 then begin

    Bank.get_position(pos);

    Bank.Tree.NodeChild(fld,0);
    Bank.Refresh_list;

    Bank.set_position(pos);
    Refresh_project;

    Result:=true
  end;

  if not Result then
  if Get_alx_Path(fn) then begin
    Delete_Map(0); Result:=true
  end;

  Get_alx_prj(fAlxList);
end;

function TMapProject.clear_map_folder: Boolean;
var
  alx: TMapList; i,n,par: Integer;
  rec: TMapRec; lp: PMapArray;
begin
  Result:=false; n:=Bank.Count;

  alx:=TMapList.Create;
  try
    for i:=0 to n-1 do
    if Bank.Get_map(i,rec) then
    if This_ext(rec.Path,'.alx') then
    alx.Add(@rec);

    Bank.Clear;

     with Bank.Tree do Ring_Down(Root);
     par:=Bank.InsFolder('#alx');

     if par > 0 then
     with Bank.Tree do begin
       Ring_Down(par); Last_RunP;

       lp:=alx.First;
       for i:=1 to alx.Count do begin
         Ins_Node(lp[0],SizeOf(rec));
         lp:=@lp[1]
       end
     end;
  finally
    alx.Free
  end;

  Bank.Refresh_list;

  if Bank.Count < n then begin
    Refresh_project; Result:=true
  end
end;

function TMapProject.Insert_Map(Path: PChar;
                                par,flags: Integer;
                                scale1,scale2: Integer): Integer;

function mm_Correct(var rec: TMapRec): Boolean;
begin
  Result:=true; if is_mm then begin

    with rec do begin
      if x_mm >= mm_w then x_mm:=0;
      if y_mm >= mm_h then y_mm:=0;

      if (w_mm < 1) and (dm_scale >= 1) then
      w_mm:=(x_rb.y-x_lt.y)/dm_scale*1000;

      if (h_mm < 1) and (dm_scale >= 1) then
      h_mm:=(x_rb.x-x_lt.x)/dm_scale*1000;

      Result:=(w_mm >= 1) and (h_mm >= 1)
    end
  end
end;

function Gauss_xy(G: PGPoly): Boolean;
var
  z: Integer;
begin
  z:=Round(G[0].y) div 1000000;
  Result:=z in [1..30]
end;

var
  rec: TMapRec; ind: Integer;
  pos: TPoint; mm: Boolean;
begin
  Result:=-1;

  if Enabled then begin

    mm:=is_mm;

    if Count = 0 then
    if Bank1.Count = 0 then
    if Bank2.Count = 0 then
    Reset_Project(true);

    is_mm:=mm;

    if Bank.NameOf(Path) < 0 then
    if This_map(Path,rec) then begin

      rec.flags:=flags;
      rec.scale1:=scale1;
      rec.scale2:=scale2;

      if (Count > 0) and (p_lg.s.prj = 0) then
      if rec.lg.s.pps = 0 then rec.lg.s.prj:=0;

      if mm_Correct(rec) then begin

        if par = 0 then
        if This_ext(Path,'.alx') then begin

          par:=Bank.GetFolder('#alx');
          if par = 0 then begin
            with Bank.Tree do Ring_Down(Root);
            par:=Bank.InsFolder('#alx');
          end;

          if par > 0 then begin
            Bank.Tree.Ring_Down(par);
            Bank.Tree.Last_RunP
          end;

          rec.flags:=rec.flags or prj_map_dll;
          rec.Color:=clFuchsia
        end;

        Bank.Delete_name('tmp.tdm');

        Bank.get_position(pos);
        Result:=Bank.Add_map(par,rec);

        if Result >= 0 then

        if Bank.Count = 1 then begin
          p_lg:=rec.lg;
          if fUpdated = 0 then
          ActiveMap:=0
        end
        else begin
          Bank.set_position(pos);
          if fUpdated = 0 then
          Refresh_project
        end
      end
    end
  end
end;

function TMapProject.xInsert_Map(Path: PChar): Boolean;
begin
  Result:=Insert_Map(Path,0,0,0,0) >= 0
end;

function TMapProject.Delete_Map(Ind: Integer): Boolean;
begin
  Result:=false;

  if Bank.Delete_map(Ind) then begin
    Refresh_project; Result:=true
  end
end;

procedure TMapProject.Delete_Node(Ptr: Integer);
begin
  Bank.Delete_node(Ptr);
  Refresh_project
end;

function TMapProject.SwapMaps(i1,i2: Integer): Boolean;
var
  r1,r2: TMapRec;
begin
  Result:=false;

  with Bank do

  if Get_map(i1,r1) then
  if Get_map(i2,r2) then begin
    Put_map(i1,r2); Put_map(i2,r1);

    if (i1 = 0) or (i2 = 0) then
    if Get_map(0,r1) then begin
      p_lg:=r1.lg; auto_Resize
    end;

    if i1 = ActiveMap then ActiveMap:=i2 else
    if i2 = ActiveMap then ActiveMap:=i1;

    Result:=true
  end
end;

function TMapProject.Refresh_Map(Ind: Integer): Boolean;
var
  dm: tdm_map; rec: TMapRec;
begin
  Result:=false;

  dm:=tdm_map.Create(0);
  try
    if Bank.Get_map(Ind,rec) then
    if dm.Open_Map(rec.Path,false) then begin

      rec.l_lt:=dm.dm_lt;
      rec.l_rb:=dm.dm_rb;
      rec.dm_scale:=dm.dm_scale;

      with rec do
      dm.LG_t.Get_Gauss_Bound(l_lt,l_rb,x_lt,x_rb);

      Precise_x_lt_rb(dm,rec);
      dm.Get_lg_Transit(rec.lg);

      Bank.Put_map(Ind,rec);

      if Ind = 0 then p_lg:=rec.lg;

      dm.Close_Map; Resize(true);
      Result:=true
    end;
  finally
    dm.Free
  end
end;

function TMapProject.Refresh_Map1(Path: PChar): Boolean;
var
  Ind: Integer;
begin
  Result:=false;
  Ind:=Bank.NameOf(Path);
  if Ind >= 0 then
  Result:=Refresh_Map(Ind)
end;

function TMapProject.Refresh_Active_Map: Boolean;
begin
  Result:=Refresh_Map(Bank.Active)
end;

function TMapProject.Open_Active_Map: Boolean;
begin
  Result:=false;
  if Count > 0 then
  Result:=Map.Enabled_Map
end;

function TMapProject.Enabled_Active_Map: Boolean;
begin
  Result:=false;
  if Count > 0 then begin
    Result:=Map.Enabled_Map;
    Map.Tree.vm_Close
  end
end;

function TMapProject.Backup_Active_Map: Boolean;
const
  min = 1000 * 60;
var
  rec: TMapRec; t: Longint;
begin
  Result:=false;

  if (x_Disp1 and tx_opt_bak) <> 0 then
  if Bank.Get_map(Bank.Active,rec) then begin

    t:=GetTickCount;
    if rec.Bak <= 0 then begin
      rec.Bak:=t;
      Bank.Put_map(Bank.Active,rec);
    end else
    if (t - rec.Bak) / min >= bak_dt then begin
      rec.Bak:=t;
      Bank.Put_map(Bank.Active,rec);
      Result:=true
    end
  end
end;

procedure TMapProject.Swap_Visible_Map(Ind,dll: Integer);
var
  rec: TMapRec;
begin
  if Bank.Get_map(Ind,rec) then
  if rec.flags and dll = 0 then begin
    rec.Visible:=not rec.Visible;
    Bank.Put_map(Ind,rec);

    if Assigned(fOnStatusMap) then
    fOnStatusMap(nil)
  end
end;

function TMapProject.Get_dm_list(list: TStrings): Integer;
var
  i: Integer; rec: TMapRec;
begin
  list.Clear;

  for i:=0 to Bank.Count-1 do
  if Bank.Get_map(i,rec) then
  List.Add(xStrNameExt(rec.Path));

  Result:=list.Count
end;

function TMapProject.dm_list(all: Boolean): TStringList;
var
  i: Integer; rec: TMapRec;
begin
  Result:=TStringList.Create;
  try
    if all then begin
      for i:=0 to Bank.Count-1 do
      if Bank.Get_map(i,rec) then
      Result.Add(StrPas(rec.Path))
    end else
    if Bank.Active_map(rec) then
      Result.Add(StrPas(rec.Path))
  finally
  end
end;

function TMapProject.Get_list(all: Boolean): TCharArray;
var
  i: Integer; rec: TMapRec;
begin
  Result:=TCharArray.Create;
  try
    if all then begin
      for i:=0 to Bank.Count-1 do
      if Bank.Get_map(i,rec) then
      if (rec.flags and prj_map_dll) = 0 then
      Result.AddLine(rec.Path);
    end else
    if Bank.Active_map(rec) then
    if (rec.flags and prj_map_dll) = 0 then
      Result.AddLine(rec.Path)
  finally
  end
end;

function TMapProject.Alt_list(alt: TCharArray): Integer;
var
  i: integer; rec: TMapRec;
begin
  Result:=0; alt.Clear;

  if Bank.Count > 1 then
  for i:=0 to Bank.Count-1 do
  if i <> Bank.Active then
  if Bank.disp_map(i,rec) then begin
    alt.AddLine(rec.Path); Inc(Result)
  end
end;

function TMapProject.Alt_Project(alt: PChar): PChar;
var
  txt: TTextFile; i: integer;
  rec: TMapRec; fn: TShortStr;
begin
  Result:=nil;

  if StrWorkPath(fn,'###.###') <> nil then begin
    DeleteFile(fn);

    txt:=TTextFile.Create;
    try
      if Bank.Count > 1 then
      if txt.Make(fn) then begin

        for i:=0 to Bank.Count-1 do
        if i <> Bank.Active then
        if Bank.disp_map(i,rec) then
        txt.WriteStr(StrPas(rec.Path));

        Result:=StrCopy(alt,fn)
      end;
    finally
      txt.Free
    end
  end
end;

function TMapProject.xAlt_Project(alt: PChar): PChar;
begin
  Result:=nil; StrCopy(alt,'');
  if (Bank.Count > 1) and x_Disp.Maps then
  Result:=Alt_Project(alt)
end;

function TMapProject.Get_first_dm_map: Integer;
var
  i: Integer; rec: TMapRec;
begin
  Result:=-1;
  for i:=0 to Bank.Count-1 do
  if Bank.Get_map(i,rec) then
  if This_ext(rec.Path,'.dm') then
  begin Result:=i; Break end
end;

function TMapProject.Get_dm_Path(Ind: Integer; Path: PChar): Boolean;
var
  rec: TMapRec;
begin
  Result:=false; if Bank.Get_map(Ind,rec) then
  Result:=Str_dm_Path(Path,rec.Path) <> nil
end;

function TMapProject.Get_Active_Path(Path: PChar): Boolean;
begin
  Result:=Get_dm_Path(ActiveMap,Path)
end;

function TMapProject.Project_Contains(const g: TGauss): Boolean;
begin
  Result:=false;
  if (g.x >= px_lt.x) and (g.y >= px_lt.y) then
  if (g.x <= px_rb.x) and (g.y <= px_rb.y) then
  Result:=true
end;

procedure TMapProject.Set_Display_dm(Ind: Integer; up: Boolean);
var
  rec: TMapRec;
begin
  if Bank.Get_map(Ind,rec) then begin
    rec.Visible:=up; Bank.Put_map(Ind,rec);
  end
end;

function TMapProject.Str_dm_Path(Path,FName: PChar): PChar;
var
  Dir: tShortStr;
begin
  StrPCopy(Dir,ExtractFileDir(FName));
  Result:=StrCopy(Path,FName);

  if StrLen(Dir) = 0 then begin
    if StrLen(fEditPath) > 0 then
    StrPCopy(Dir,ExtractFileDir(fEditPath));

    if StrLen(Dir) > 0 then begin
      StrCat(StrCopy(Path,Dir),'\');
      Result:=StrCat(Path,FName)
    end
  end
end;

function TMapProject.Open_dm(Ind: Integer): Boolean;
var
  fn: TShortStr; rec: TMapRec;
begin
  Result:=false;
  if Bank.Get_map(Ind,rec) then
  if Str_dm_Path(fn,rec.Path) <> nil then
  Result:=Map.Open_Map(fn,false)
end;

function TMapProject.Open_Active_dm: Boolean;
begin
  Result:=Open_dm(ActiveMap)
end;

function TMapProject.Disp_map(const rec: TMapRec;
                              const lt,rb: TGauss;
                              Scale: Integer): Boolean;
var
  tmp: TMapRec;
begin
  Result:=false; tmp:=rec;

  if tmp.Enabled and tmp.Visible then with tmp do

  if x_Disp.Maps or
  ((scale >= scale1) and ((scale1 >= scale2) or (scale <= scale2))) then begin
    if is_mm then mm_rec_x_lt_rb(tmp);
    Result:=GaussContainsPort(lt,rb, x_lt,x_rb)
  end
end;

function TMapProject.Disp_scale(const rec: TMapRec;
                                Scale: Integer): Boolean;
begin
  Result:=false;

  if rec.Enabled and rec.Visible then with rec do

  if x_Disp.Maps or (Scale = 0) or
  ((scale >= scale1) and ((scale1 >= scale2) or (scale <= scale2))) then
  Result:=true
end;

function TMapProject.Disp_dm(Ind,Scale: Integer;
                             const lt,rb: tgauss): Boolean;
var
  rec: TMapRec;
begin
  Result:=false;
  if Bank.Get_map(Ind,rec) then
  Result:=Disp_map(rec,lt,rb,Scale)
end;

function TMapProject.Map_Contains_Gauss(Ind,Lev: Integer;
                                        const p: tgauss): Boolean;
var
  rec: TMapRec;
begin
  Result:=false;
  if Bank.Get_map(Ind,rec) then begin
    if is_mm then mm_rec_x_lt_rb(rec);

    with rec do
    if Disp_dm(Ind,Lev,x_lt,x_rb) then
    GaussContainsPoint(x_lt,x_rb, p)
  end
end;

function TMapProject.Map_Contains_Geoid(Ind,Lev: Integer;
                                        const x: xgeoid): Boolean;
var
  g: tgauss; rec: TMapRec;
begin
  Result:=false;

  if Bank.Get_map(Ind,rec) then begin
    if is_mm then mm_rec_x_lt_rb(rec);

    with rec do
    if Disp_dm(Ind,Lev,x_lt,x_rb) then

    if is_mm
    or ((p_lg.s.pps+x.s.pps) = 0) then begin
      g.x:=x.x; g.y:=x.y;
      Result:=GaussContainsPoint(x_lt,x_rb, g)
    end else

    if lg_Contains_Point(lg, x) then
      Result:=true
    else
    if Bank.Count <= 8 then begin
      g:=geo_to_xy(x,p_lg.s);
      Result:=GaussContainsPoint(x_lt,x_rb, g)
    end
  end
end;

function TMapProject.Map_Contains_Geoidb(Ind,Lev: int;
                                         bp: PGPoly;
                                         const s: tsys): bool;
var
  i: int; x: xgeoid;
begin
  Result:=false; x.s:=s;
  for i:=0 to 3 do begin
    x.x:=bp[i].x; x.y:=bp[i].y;
    if Map_Contains_Geoid(Ind,Lev,x) then
    begin Result:=true; Break end
  end
end;

function TMapProject.Contains_Geoid(const x: xgeoid): Integer;
var
  i,j: Integer; rec: TMapRec;
begin
  Result:=-1; j:=Bank.Active;

  for i:=1 to Bank.Count do begin
    Inc(j); if j >= Bank.Count then j:=0;
    if Bank.Get_map(j,rec) then
    if lg_Contains_Point(rec.lg,x) then
    begin Result:=j; Break end
  end
end;

function TMapProject.Contains_Gauss(const p: TGauss): Integer;
var
  i,j: Integer; rec: TMapRec;
begin
  Result:=-1; j:=Bank.Active;

  for i:=1 to Bank.Count do begin
    Inc(j); if j >= Bank.Count then j:=0;
    if Bank.Get_map(j,rec) then
    if GaussContainsPoint(rec.x_lt,rec.x_rb, p) then
    begin Result:=j; Break end
  end
end;

function TMapProject.Contains_alx(const lt,rb: tgauss): Boolean;
var
  i: Integer; g: GOrient; r: xgeoid; _lt,_rb: TGauss;
begin
  Result:=false;
  if Count > 0 then begin

    r.s:=sys7(0,2,9,0,0,0);
    r.s.lc:=gLongitude(lt.y-30000000);

    Bound_to_GOrient(lt,rb, @g);

    for i:=0 to 3 do begin
      r.x:=g[i].x; r.y:=g[i].y;
      g[i]:=prj_inside(r,p_lg.s);
    end;

    Max_Gauss_Bound(@g,4,_lt,_rb);

    Result:=GaussContainsPort(px_lt,px_rb,_lt,_rb)
  end
end;

function TMapProject.Contains_rgn(const lt,rb: tgauss): Boolean;
var
  _lt,_rb: TGauss; dy: Double;
begin
  Result:=false;
  if Count > 0 then begin _lt:=lt; _rb:=rb;

    if p_lg.s.pps = 1 then begin
      dy:=prj_ZoneY(p_lg.s);
      _lt.y:=_lt.y - dy;
      _rb.y:=_rb.y - dy;
    end;

    Result:=GaussContainsPort(px_lt,px_rb,_lt,_rb)
  end
end;

function TMapProject.Contains_Map(Path: PChar): integer;
var
  i: int; rec: TMapRec;
  fn,name,ext: TShortStr;
begin
  Result:=-1;

  StrNameExt(fn,Path);

  StrExt(ext,fn);
  if StrComp(ext,'.') = 0 then
    StrLCat(fn,'.dm',255)
  else
    xStrUpdateExt(fn,'.dm');

  for i:=0 to Bank.Count-1 do
  if Bank.Get_map(i,rec) then begin
    StrNameExt(name,rec.Path);
    if StrComp(name,fn) = 0 then
    begin Result:=i; Break end
  end
end;

function TMapProject.Map_ExpandPath(path,fn: PChar): PChar;
var
  i: Integer; rec: TMapRec;
begin
  Result:=nil;
  i:=Contains_Map(fn);
  if Bank.Get_map(i,rec) then
  Result:=StrCopy(path,rec.Path)
end;

procedure TMapProject.Set_map_sm_T(Ind: Integer);
var
  rec: TMapRec; img: LG_Transit;
begin
  sm_T.Init;

  if Bank.Get_map(Ind,rec) then begin

    if not is_mm then
    if Pcx_link.sys.pps = 1 then begin
      Pcx_link.Get_lg_Transit(0,0,img,false);
      East_lg_Transit(rec.lg,img);
    end;

    sm_T.Assign(rec.lg,-1);

    if is_mm then
      Get_dm_lg_t(Ind,sm_T)
    else
      Pcx_link.sm_Transit(sm_T)
  end
end;

function TMapProject.Active_enabled(Ind: Integer): Boolean;
var
  rec: TMapRec; lt,rb: TGauss;
  lc,l1,l2,dl: Double;
begin
  Result:=false;
  if Bank.Get_map(Ind,rec) then
  with rec.lg do begin
    Result:=true;

    if s.pps = 1 then
    if (s.prj > 2) and (s.prj <> 3) then
    if p_lg.s.pps = 1 then
    if p_lg.s.prj in [1,2] then begin

      lc:=p_lg.s.lc;

      dl:=3/180*Pi;
      l1:=lc-dl; l2:=lc+dl;

      Max_Gauss_Bound(@g,4, lt,rb);
      if (lt.y < l1) or (rb.y > l2) then
      Result:=false
    end
  end
end;

function TMapProject.Get_map_lg_t(const rec: TMapRec;
                                  out T: AB_Orient): Boolean;

function trunc_lg(var lg: lg_Transit;
                  const base: lg_Transit;
                  lc: Double): Boolean;
var
  l1,l2,dl: Double; lt,rb,t: TGauss;
  ab: AB_Orient; i: Integer;
begin
  Result:=true;

  if lg.s.pps = 1 then
  if lg.s.prj in [1,2] then
  if base.s.pps = 1 then
  if base.s.prj > 2 then begin

    dl:=3/180*Pi;
    l1:=lc-dl; l2:=lc+dl;

    Max_Gauss_Bound(@lg.g,4, lt,rb);
    if (lt.y < l1) or (rb.y > l2) then begin
      lt.y:=Max(lt.y,l1);
      rb.y:=Min(rb.y,l2);

      if lt.y+0.01/180*Pi <= rb.y then begin

        ab.Init;
        ab.Assign(base,-1);

        Bound_to_GOrient(lt,rb, @lg.g);

        for i:=0 to 4 do begin
          with lg.g[i] do ab.BL_XY(x,y,t);
          ab.G_to_P(t,lg.l[i]);
        end;

        Result:=true
      end
    end
  end
end;

var
  lg_t: lg_Transit; g: TGauss;
begin
  Result:=false; T.Init;

  if is_mm then begin

    lg_t:=rec.lg; with rec do
    Update_mm_transit(lg_t,x_mm,y_mm,w_mm,h_mm);

    T.Assign(lg_t,-1); Result:=true

  end
  else begin
    lg_t:=rec.lg;
    Link_lg_Transit(lg_t,p_lg);

    if trunc_lg(lg_t,rec.lg,p_lg.s.lc) then
    if T.Assign(lg_t,p_lg.s.lc) then begin

      if p_lg.s.prj = 0 then begin

        if lg_t.s.pps = 0 then
        if lg_t.s.prj = 0 then begin
          T.sys:=sys_nil; T.zoneY:=0;
        end

      end
      else begin
        with px_centre do T.BL_XY(x,y,g);
        if T.sys.pps = 0 then g.y:=g.y + T.zoneY;

        T.c_Prepare(g);
      end;

      Result:=true
    end
  end
end;

function TMapProject.Get_dm_lg_t(Ind: Integer; out T: AB_Orient): Boolean;
var
  rec: TMapRec;
begin
  Result:=false; T.Init;
  if Bank.Get_map(Ind,rec) then
  Result:=Get_map_lg_t(rec,T)
end;

function TMapProject.Get_Active_lg_t(out T: AB_Orient): Boolean;
begin
  Result:=Get_dm_lg_t(Bank.Active,T)
end;

function TMapProject.map_x_lt_rb(Sender: TObject; ptr: Longint;
                                 var rec: TMapRec): Integer;

function Align_projection(G: PGPoly): Double;
var
  v: TGauss;
begin
  Sort_lg_Transit(nil,G);
  v:=Gauss_dir(G[1],G[2]);
  Result:=ArcTan2(v.x,v.y)*180/Pi
end;

function trunc_lg(const T: AB_Orient;
                  G: PGPoly; L: PLPoly;
                  lc: Double): Boolean;
var
  i: Integer;
  l1,l2,dl: Double; lt,rb: TGauss;
begin
  Result:=false;

  dl:=3/180*Pi;
  l1:=lc-dl; l2:=lc+dl;

  Max_Gauss_Bound(G,4, lt,rb);
  if (lt.y < l1) or (rb.y > l2) then begin
    lt.y:=Max(lt.y,l1);
    rb.y:=Min(rb.y,l2);

    if lt.y+0.01/180*Pi <= rb.y then begin

      Bound_to_GOrient(lt,rb, G);

      for i:=0 to 3 do
      T.R_to_L(TGeoid(G[i]),L[i]);

      Result:=true
    end
  end
end;

var
  i,pps: Integer; d1,d2: Double;
  lt,rb: TGauss; x: xgeoid; T: AB_Orient;
  lg: lg_Transit; L: LOrient; G: GOrient;
  act: TMapRec;
begin
  Result:=1; rec.Enabled:=false;

  if rec.dm_scale <> prj_folder_Id then begin

    rec.Enabled:=Get_map_lg_t(rec,T);

    if p_lg.s.prj > 0 then begin

      pps:=T.Get_pps_Bound(lt,rb);

      if px_Count = 0 then begin
        z_pps:=pps; z_lt:=lt; z_rb:=rb
      end else
      if pps = z_pps then
        Add_GRect(z_lt,z_rb, lt,rb);

      lg:=rec.lg; with lg.g[0] do
      sys_Projection(lg.s,x,y);

      East_lg_Transit(lg,p_lg);

      T.Assign(lg,-1);
      Bound_to_LPoly(rec.l_lt,rec.l_rb,@L);

      if p_lg.s.pps = 1 then
      if p_lg.s.prj in [1,2] then

      if lg.s.pps = 1 then
      if lg.s.prj > 2 then begin
        G:=lg.g; Trunc_lg(T,@G,@L,p_lg.s.lc)
      end;

      for i:=0 to 3 do
      if T.This_lg(p_lg) then
        T.L_to_G(L[i],G[i])
      else begin
        T.x_L_to_R(L[i],x);
        G[i]:=prj_inside(x,p_lg.s);
      end;

      d1:=Max(1,Gauss_Dist(G[0],G[2]));
      d2:=Max(1,Gauss_Dist(G[1],G[3]));

      rec.kz.x:=Long_Dist(L[0],L[2]) / d1;
      rec.kz.y:=Long_Dist(L[1],L[3]) / d2;

      with rec.kz do
      px_ppm:=Max(px_ppm,Max(x,y));

      Max_Gauss_Bound(@G,4,rec.x_lt,rec.x_rb);

      if p_lg.s.pps = 1 then

      if Sender = Bank then
      if rec.Index = Bank.Active then

      if p_lg.s.prj in [1..2,prj_cuba,prj_cuba+1] then
      px_North:=Align_projection(@G)
    end
    else begin
      lt.x:=rec.x_lt.x;
      lt.y:=rec.x_lt.y;
      rb.x:=rec.x_rb.x;
      rb.y:=rec.x_rb.y;

      if px_Count = 0 then begin
        z_lt:=lt; z_rb:=rb
      end else
      Add_gRect(z_lt,z_rb, lt,rb);

      rec.kz.x:=T.lg_ppm;
      rec.kz.y:=T.lg_ppm;

      with rec.kz do
      px_ppm:=Max(px_ppm,Max(x,y));

      z_pps:=0
    end;

    if rec.Enabled then begin

      with rec do
      if px_Count > 0 then
        Add_gRect(px_lt,px_rb,x_lt,x_rb)
      else begin
        px_lt:=x_lt; px_rb:=x_rb;
      end;

      Inc(px_Count)
    end
  end
end;

function TMapProject.prj_Resize(const sys: tsys): Boolean;
var
  i: Integer; z: Double;
begin
  Result:=false;
  if p_lg.s.pps = 1 then
  if sys.pps = 1 then begin

    if p_lg.s.elp <> sys.elp then

    for i:=0 to 3 do with p_lg.g[i] do
    GEO_GEO(x,y,0, p_lg.s.elp,sys.elp,
            @p_lg.s.dat,@sys.dat, x,y,z);

    p_lg.s:=sys; Resize(false);
    Result:=true
  end
end;

function TMapProject.Resize(auto: Boolean): Boolean;

procedure Get_lc_Bound(const lg,act: lg_Transit;
                       out lt,rb: tgauss);
var
  i: int; t: lg_Transit; r: TGeoid;
begin
  t:=lg; Link_lg_Transit(t,p_lg);

  if p_lg.s.pps = 0 then
  for i:=0 to 3 do begin
    r:=x_XY_BL(t.g[i],p_lg.s);
    t.g[i]:=TGauss(r)
  end;

  Max_Gauss_Bound(@t.g,4,lt,rb);
end;

procedure Update_prj_plan_prj;

procedure bank_get_prj(Bank: TMapBank);
var
  i: Integer; rec: TMapRec;
begin
  if p_lg.s.prj = 0 then
  for i:=0 to Bank.Count-1 do
  if Bank.Get_map(i,rec) then
  if rec.lg.s.prj > 0 then begin
    Link_lg_prj(p_lg.s,rec.lg.s);
    Break
  end
end;

procedure bank_upd_prj(Bank: TMapBank);
var
  i: Integer; rec: TMapRec;
begin
  if p_lg.s.prj > 0 then
  for i:=0 to Bank.Count-1 do
  if Bank.Get_map(i,rec) then
  if rec.lg.s.prj = 0 then begin

    with rec.lg.g[0] do
    sys_projection(rec.lg.s,x,y);

    Bank.Put_map(i,rec)
  end
end;

var
  i: Integer; rec: TMapRec;
begin
  if p_lg.s.pps = 1 then
  if p_lg.s.prj = 0 then
  p_lg.s.prj:=1;

  if p_lg.s.prj = 0 then begin
    bank_get_prj(Bank);
    bank_get_prj(Bank2);
    bank_get_prj(Bank1);
  end;

  if p_lg.s.prj > 0 then
  if Bank.Count > 0 then
  if Bank.Get_map(0,rec) then
  if rec.lg.s.prj = 0 then begin

    with rec.lg.g[0] do
    sys_projection(rec.lg.s,x,y);

    if rec.lg.s.prj > 0 then begin
      p_lg.s:=sys_nil;
      Link_lg_prj(p_lg.s,rec.lg.s);
    end
  end;

  bank_upd_prj(Bank);
  bank_upd_prj(Bank1);
  bank_upd_prj(Bank2);
end;

procedure Update_prj_lc(auto: Boolean);
var
  i: Integer; lc: double;
  lt,rb,_lt,_rb, g1,g2: tgauss;
  act,rec: TMapRec; tmp: tsys;
begin
  if p_lg.s.prj > 0 then
  if Bank.Count > 0 then begin

    lc:=p_lg.s.lc;

    Bank.Active_map(act);
    Get_lc_Bound(act.lg,act.lg, lt,rb);

    if act.lg.s.prj in prj_lc then begin
      tmp:=act.lg.s;
      if tmp.pps = 1 then
        lc:=xLongitude(@act.lg.g)
      else begin
        with act.lg.g[0] do
        sys_projection(tmp,x,y);
        lc:=tmp.lc
      end;

      if p_lg.s.pps = 1 then
      lc:=prj_Longitude(tmp.lc,
                        lt.y/2 + rb.y/2,
                        p_lg.s.prj);
    end;

    for i:=0 to Bank.Count-1 do
    if Bank.Get_map(i,rec) then begin
      Get_lc_Bound(rec.lg,act.lg, _lt,_rb);
      Add_GRect(lt,rb, _lt,_rb);
    end;

    Middle_Gauss(lt,rb,px_centre);

    if x_Disp.Maps and (Count > 1) then
    with p_lg.s do if pps = 1 then begin
      BL_to_XY(rb.x,lt.y, lc,b1,b2, elp,prj, g1.x,g1.y);
      BL_to_XY(rb.x,rb.y, lc,b1,b2, elp,prj, g2.x,g2.y);
      px_North:=ArcTan2(g2.x-g1.x,g2.y-g1.y)*180/Pi
    end;

    if IsPlan then
    if p_lg.s.prj in [0..2] then
    if Map.LG_T.sys.prj in [0..2] then
    lc:=Map.LG_T.sys.lc;

    if auto then
    if p_lg.s.pps = 1 then
    p_lg.s.lc:=lc;

    if p_lg.s.pps = 1 then begin
      px_zone.y:=prj_zoneY(p_lg.s);

      lc:=p_lg.s.lc;
      if px_zone.y <> 0 then
      if Abs(lc - zLongitude(lc)) > Small then
      px_zone.y:=0
    end
  end
end;

function Update_folder(Tree: TVirtTree; ptr: Longint): Integer;
var
  top,run,rc: Longint; rec,tmp: TMapRec;
begin
  Result:=0;

  top:=Tree._Child(ptr);
  if top > 0 then begin
    Tree._Info(ptr,rec,Sizeof(rec));
    rec.Enabled:=false;

    run:=Tree._Link(top);
    while run <> top do begin

      if not Tree._Verify(run) then Break;
      Tree._Info(run,tmp,SizeOf(tmp));

      if tmp.dm_scale <> prj_folder_Id then
        rc:=ord(tmp.Enabled)
      else begin
        rc:=Update_folder(Tree,run);
        Tree._Info(run,tmp,SizeOf(tmp));
      end;

      if rc > 0 then begin

        with rec do
        if Result > 0 then
          Add_gRect(x_lt,x_rb,tmp.x_lt,tmp.x_rb)
        else begin
          x_lt:=tmp.x_lt; x_rb:=tmp.x_rb;
        end;

        rec.Enabled:=true; Inc(Result)
      end;

      run:=Tree._Link(run);
    end;

    Tree.NodeInfo(ptr,rec,Sizeof(rec));
  end
end;

const
  Max_Range = 1000000000;

var
  i: Integer; old_lt,old_rb: TGauss;
begin
  Result:=true;

  Fillchar(win_gc,Sizeof(win_gc),0);
  win_gc.sc:=-1; win_gc.pos.s.pps:=-1;

  px_Count:=0; px_North:=0;
  px_centre:=_Gauss(0,0);

  old_lt:=px_lt; old_rb:=px_rb;
  px_lt.x:=0; px_rb.x:=10000;
  px_lt.y:=0; px_rb.y:=10000;

  px_ed:=100; px_ppm:=1;
  px_zone:=_Gauss(0,0);

  if Enabled and (Count2 > 0) then

  if is_mm then begin
    px_rb.x:=mm_h; px_rb.y:=mm_w;
    p_lg.s.lc:=0;
  end
  else begin
    Update_prj_plan_prj;
    Update_prj_lc(auto);
    Bank.ForEach(map_x_lt_rb);

    if not IsOcx then
    Bank1.ForEach(map_x_lt_rb);

    Bank2.ForEach(map_x_lt_rb);
    Result:=false;
  end;

  Update_folder(Bank.Tree,Bank.Tree.Root);

  px_lt1:=px_lt; px_rb1:=px_rb;
  
  if isEditor then
  Mult_gRect(px_lt,px_rb,32, px_lt,px_rb);

  px_rb.x:=Max(px_lt.x+10,px_rb.x);
  px_rb.y:=Max(px_lt.y+10,px_rb.y);

  while px_ed < px_ppm do px_ed:=px_ed*10;

  while (px_Width * px_ed > Max_Range) and
        (px_ed > 0.01) do px_ed:=px_ed/10;

  while (px_Height * px_ed > Max_Range) and
        (px_ed > 0.01) do px_ed:=px_ed/10;

  if not Result then

  if Gauss_Dist(old_lt,px_lt) > 0.1 then Result:=true else
  if Gauss_Dist(old_rb,px_rb) > 0.1 then Result:=true;

  for i:=0 to fCards.Count-1 do
  TNotifyObj(fCards[i]).notify(Self,0);
end;

function TMapProject.auto_Resize: Boolean;
begin
  Get_alx_prj(fAlxList);
  Result:=Resize(true)
end;

function TMapProject.Get_Bound(out lt,rb: TGauss): Integer;
var
  G: GOrient; i: Integer;
begin
  Result:=p_lg.s.pps;

  if Result = 0 then begin
    lt:=px_lt; rb:=px_rb
  end
  else begin
    Bound_to_GOrient(px_lt,px_rb,@G);

    for i:=0 to 3 do with G[i] do
    sys_XY_BL(x,y,p_lg.s,x,y);

    Max_Gauss_Bound(@G,4,lt,rb)
  end
end;

function TMapProject.SaveToMpg(Path: PChar): Integer;
var
  txt: TTextfile; kz,mpp,dy: Double;
  i: Integer; lt,rb: TGauss; rec: TMapRec;
  fl: Boolean; s: TShortstr;
begin
  Result:=0;

  if Count > 0 then begin

    fl:=isEditor; isEditor:=false;
    if Map.LG_T.sys.pps = 1 then
    if Map.LG_T.sys.prj = 1 then

    if p_lg.s.pps = 1 then
    if p_lg.s.prj = 1 then
    p_lg.s.lc:=Map.LG_T.sys.lc;

    Resize(false);
    lt:=px_lt; rb:=px_rb;

    if p_lg.s.pps = 1 then
    if p_lg.s.prj = 1 then begin
      dy:=prj_ZoneY(p_lg.s);
      lt.y:=lt.y + dy;
      rb.y:=rb.y + dy;
    end;

    isEditor:=fl; Resize(true);

    txt:=TTextfile.Create;
    try
      if txt.Make(Path) then begin

        txt.WriteStr(RealToStr(lt.x,-1)+#9+
                     RealToStr(lt.y,-1));

        txt.WriteStr(RealToStr(rb.x,-1)+#9+
                     RealToStr(rb.y,-1));

        for i:=0 to Bank.Count-1 do
        if Bank.Get_map(i,rec) then begin

          StrNameExt(s,rec.Path);

          txt.WriteStr(IntToStr(rec.scale1)+#9+
                       IntToStr(rec.scale2)+#9+
                       Strpas(s));
        end;
      end;
    finally
      txt.Free
    end
  end
end;

procedure TMapProject.Clip_Rect(a,b: tgauss; out c,d: tgauss);
begin
  Clip_GRect(a,b, px_lt,px_rb, c,d)
end;

procedure TMapProject.prj_GOrient(var R: GOrient);
begin
  Bound_to_GOrient(px_lt,px_rb,@R)
end;

function TMapProject.geo_Get_zval(const g: XGeoid;
                                  out zv: Double): Boolean;
begin
  Result:=Bank1.GetValue(@g,zv) = S_OK
end;

function TMapProject.dm_Get_zval(const p: TPoint;
                                 out zv: Double): Boolean;
var
  g: xgeoid;
begin
  Result:=false; zv:=0;
  if Bank1.Count > 0 then begin
    Map.LG_t.x_L_to_R(p,g);
    Result:=Bank1.GetValue(@g,zv) = S_OK
  end
end;

function TMapProject.xy_Get_zval(const g: TGauss;
                                 out zv: Double): Boolean;
var
  r: XGeoid; bl: TGeoid;
begin
  r.s:=Map.LG_T.sys;
  r.x:=g.x; r.y:=g.y;
  if r.s.pps = 1 then begin
    Map.LG_T.z_XY_BL(g,bl);
    r.x:=bl.b; r.y:=bl.l;
  end;

  Result:=geo_Get_zval(r,zv)
end;

function TMapProject.xy_Get_zmin(const c,v: TGauss;
                                 dr: Double; out zv: Double): Boolean;
var
  i,k: Integer; gz: Double; g: TGauss;
begin
  Result:=false; zv:=0;

  k:=4;
  for i:=-k to k do begin
    g:=gauss_forw(c,v,-i/k*dr);
    if xy_Get_zval(g,gz) then
    if not Result or (gz < zv) then
    begin zv:=gz; Result:=true end
  end
end;

procedure TMapProject.Get_xy_port(const lt,rb: TPoint;
                                  out g_lt,g_rb: TGauss);
var
  i: Integer;
  L: LOrient; G: GOrient;
begin
  Bound_to_LPoly(lt,rb,@L);

  for i:=0 to 3 do
  Map.LG_t.dm_to_xy(L[i],G[i]);

  Max_Gauss_Bound(@G,4, g_lt,g_rb)
end;

procedure TMapProject.xy_to_sm(g: tgauss; out p: TPoint);
begin
  p.x:=Round((g.y - px_lt.y)*px_ed);
  p.y:=Round((px_rb.x - g.x)*px_ed)
end;

procedure TMapProject.sm_to_xy(p: TPoint; out g: tgauss);
begin
  g.x:=px_rb.x - p.y/px_ed;
  g.y:=px_lt.y + p.x/px_ed
end;

procedure TMapProject.dm_to_pcx(x,y: Integer; out b: tgauss);
var
  p: TPoint; g: TGauss;
begin
  p.x:=x; p.y:=y;
  sm_T.dm_to_xy(p,g);
  Pcx_link.xy_to_pix(g,b);
end;

procedure TMapProject.pcx_to_dm(x,y: Double; out b: TPoint);
var
  p,g: tgauss;
begin
  Pcx_link.pix_to_xy(x,y,g);
  sm_T.xy_to_dm(g,b);
end;

procedure TMapProject.dm_to_bmp(const a: TPoint; out b: TPoint);
var
  p: tgauss;
begin
  dm_to_pcx(a.x,a.y,p);
  b.X:=Round(p.x); b.Y:=Round(p.y);
end;

procedure TMapProject.bmp_to_dm(const a: TPoint; out b: TPoint);
begin
  pcx_to_dm(a.x,a.y,b)
end;

function TMapProject.xyz_to_pair(const v: vpoint;
                                 out p: TPoint): Integer;
var
  p1,p2: TPoint;
begin
  with v do xyz_to_scb(x,y,z,p1); StereoLeft(false);
  with v do xyz_to_scb(x,y,z,p2); StereoLeft(true);
  Result:=p2.x-p1.x; p:=p1
end;

procedure TMapProject.xyz_to_scb(x,y,z: Integer; out b: TPoint);
var
  g,p: tgauss; r: xgeoid; v: xxyz;
begin
  if fStereo.sys_Enabled then begin
    Map.LG_T.x_L_to_R(Point(x,y),r);
    xStereo_Project(fStereo, r,z/Map.z_res, p)
  end
  else begin
    with Map.LG_T do begin
      if x_fast then g:=x_ab.Transit(x,y) else
      g:=q_ab.Transit(x,y); g.y:=g.y+zoneY;
    end;

    v.g:=xxx_gl.Transit(g.x,g.y);
    v.v.z:=z/Map.z_res - fStereo.lg_h;

    Stereo_Project(fStereo, v.v, p)
  end;

  b.x:=Round(p.x); b.y:=Round(p.y)
end;

procedure TMapProject.scb_to_xyz(x1,x2,y: Integer; out b: vpoint);
var
  v: txyz; g: tgauss; p: TPoint; r: xgeoid;
begin
  Fillchar(r,Sizeof(r),0);
  r.s.pps:=Stereo_Backup(fStereo,x1,x2,y, v);

  if fStereo.sys_Enabled then begin
    r.x:=v.x; r.y:=v.y;
    Map.LG_T.xxx_to_L(r,p)
  end
  else begin
    g:=xxx_lg.Transit(v.x,v.y);
    v.z:=v.z+Round(fStereo.lg_h);
    Map.LG_T.Z_to_L(g,p);
  end;

  b.x:=p.x; b.y:=p.y;
  b.z:=Round(v.z*Map.z_res);
end;

procedure TMapProject.dm_to_scb(x,y: Integer; out b: tgauss);
var
  p: TPoint;
begin
  xyz_to_scb(x,y,0, p); b.x:=p.x; b.y:=p.y
end;

procedure TMapProject.scb_to_dm(x,y: Double; out b: TPoint);
var
  v: lxyz; p: TPoint;
begin
  p.x:=Round(x); p.y:=Round(y);
  scb_to_xyz(p.x,p.x,p.y, v.v); b:=v.p
end;

function TMapProject.rpc_to_dm(x,y: Double; out v: lxyz): Boolean;

var
  p: TPoint;
begin
  Result:=false;
  pcx_to_dm(x,y,p);
  v.p:=p; v.v.z:=0
end;

function TMapProject.get_rpc_h1(const v: VPoint;
                                const cp: TGauss;
                                dm_t_rpc: Boolean): Float;
begin
  Result:=0
end;

procedure TMapProject.mm_rec_x_lt_rb(var rec: TMapRec);
begin
  if is_mm then with rec do begin
    x_lt.x:=y_mm; x_rb.x:=y_mm+h_mm;
    x_lt.y:=x_mm; x_rb.y:=x_mm+w_mm
  end
end;

function TMapProject.get_pcx_plan(out x,y,w,h: Integer): Integer;
var
  i,j,k: Integer; rec: TMapRec;
  l: LOrient; lt,rb,lt1,rb1: TPoint;
begin
  x:=0; y:=0; w:=0; h:=0;

  Result:=Pcx_link.Count;
  if Result >= 3 then begin k:=0;

    for i:=0 to Bank.Count-1 do
    if Bank.Get_map(i,rec) then begin
      Set_map_sm_T(i); with rec do
      Bound_to_LPoly(l_lt,l_rb,@l);

      for j:=0 to 3 do dm_to_bmp(l[j],l[j]);
      Max_Poly_Bound(@l,4,lt1,rb1);

      if k = 0 then begin
        lt:=lt1; rb:=rb1
      end
      else Add_LRect(lt,rb,lt1,rb1);

      Inc(k)
    end;

    Set_map_sm_T(ActiveMap);

    if k > 0 then begin

      if lt.X > 0 then
        w:=rb.X + 1
      else begin
        w:=rb.X - lt.X + 1; x:=-lt.X
      end;

      if lt.Y > 0 then
        h:=rb.Y + 1
      else begin
        h:=rb.Y - lt.Y + 1; y:=-lt.Y
      end;

    end
  end
end;

function TMapProject.get_prj_t(const gc: TGauss; fi: Double;
                               out tr,bt: Real3x3): TGauss;
var
  i: Integer; G: GOrient;
  c,lt,rb: TGauss; t,_t: Real3x3;
begin
  c:=gc;

  t[1,1]:=0;  t[1,2]:=1; t[1,3]:=-c.y;
  t[2,1]:=-1; t[2,2]:=0; t[2,3]:=c.x;
  t[3,1]:=0;  t[3,2]:=0; t[3,3]:=1;

  _t[1,1]:=0; _t[1,2]:=-1; _t[1,3]:=c.x;
  _t[2,1]:=1; _t[2,2]:=0;  _t[2,3]:=c.y;
  _t[3,1]:=0; _t[3,2]:=0;  _t[3,3]:=1;

  fi:=-fi/180*Pi;
  if fi <> 0 then
  fi_Rotate_3x3(t,fi);

  Bound_to_GOrient(px_lt,px_rb,@G);

  for i:=0 to 3 do with G[i] do
  G[i]:=Transit_3x3(x,y, t);

  Max_Gauss_Bound(@G,4, lt,rb);

  t_Move_3x3(t, -lt.x,-lt.y); tr:=t;

  Begin_3x3(t, lt.x,lt.y);

  if fi <> 0 then
  fi_Rotate_3x3(t,-fi);

  Next_3x3(t,_t); bt:=t;

  Result.x:=Max(1,rb.x - lt.x);
  Result.y:=Max(1,rb.y - lt.y);
end;

constructor TTabList.Create;
begin
  inherited Create(Sizeof(TTabPos),32)
end;

function TTabList.Add_Item(const g: xgeoid;
                           sc: double; s: PChar): Integer;
var
  Pos: TTabPos;
begin
  Pos.g:=g; Pos.sc:=sc;
  if StrLen(s) > 63 then s[63]:=#0;
  Pos.msg:=StrPas(s);

  Result:=Add(@Pos)
end;

function TTabList.LoadFrom(Path: PChar): Integer;
var
  fn: TShortStr;
begin
  StrWorkPath(fn,'tabs.###');
  if xStrlen(Path) > 0 then
  StrCopy(fn,Path);

  Result:=inherited LoadFrom(fn)
end;

procedure TTabList.SaveAs(Path: PChar);
var
  fn: TShortStr;
begin
  StrWorkPath(fn,'tabs.###');
  if xStrlen(Path) > 0 then
  StrCopy(fn,Path);

  inherited SaveAs(fn)
end;

procedure Init;
begin
  xStrTempClear('prj',nil);
end;

initialization Init;

end.