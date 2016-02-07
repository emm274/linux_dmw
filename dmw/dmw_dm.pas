unit dmw_dm; interface {$H-} {$A1}

uses
  Classes,Math,activeX,
  otypes,xdmw,xlist,xclasses,
  xvirts,xrings,ogauss,
  dmw_obj,dmw_idc,dmw_bln;

const
  ObjectsMax = 63;
  LPolyMax  = 32000-1;

  is_str_anl: Boolean = false;

  Hole_Code: Integer = 0;
                                         
  dm_Polygon_Square: Boolean = true;

  dm_Char_Is_Words: Boolean = false;

  dm_Auto_Max: Integer = 8;

  dm_min: array[0..11] of Integer = (0,0,1,2,0,1,3,5,2,0,0,0);

  dm_ppm: array[0..3] of Integer = (1,10,100,1000);

  dmw_mark: char4 = '#dmw';

  fl_del  = 1;
  fl_attr = 2;
  fl_mf   = 4;
  fl_hf   = 8;
  fl_draw = 16;
  fl_long = 32;
  fl_skip = 64;
  fl_xyz  = 128;

  tx_edit_sel = 0;
  tx_edit_add = 1;
  tx_edit_del = 2;
  tx_edit_mf  = 4;
  tx_edit_hf  = 8;
  tx_show_mf  = 16;
  tx_edit_cn  = 32;
  tx_link_cn  = 64;
  tx_undo_cn  = 128;

  tx_select   = 256;
  tx_editing  = 512;
  tx_edited   = 1024;
  tx_repaint  = 2048;

  tx_edit_any = 1+2+4+8;

  undo_mov    = 0;
  undo_add    = 1;
  undo_del    = 2;
  undo_own    = 3;

  cn_object   = 0;
  cn_ident    = 1;
  cn_offs     = 99;
  cn_node     = 120;
  cn_edge     = 130;
  cn_aggr     = 100;

  cn_point3   = 249;
  cn_point2   = 250;
  cn_point1   = 251;
  cn_point    = 252;
  cn_legend   = 253;
  cn_vector   = 254;
  cn_vertex   = 255;

  cn_geo      = $FFFFFFFF;

  _cn: array[0..4] of Integer =
        (cn_object,cn_ident,cn_node,cn_edge,cn_aggr);

  nn_grid_db = 971;
  nn_grid_dl = 972;

  nn_grid_dx = 973;
  nn_grid_dy = 974;

  nn_grid_b0 = 975;
  nn_grid_l0 = 976;

  nn_grid_x0 = 977;
  nn_grid_y0 = 978;

  nn_mm = 980;

type
  TOnShowObject = procedure(Sender: TObject; P: Int64) of object;
  TOnEditObject = procedure(P: Int64; Id,Cmd: Integer) of object;
  TOnUndoObject = procedure(P: Int64; Cmd: Integer) of object;
  TOnIdentify = function(Id: Cardinal): Cardinal of object;

  TOnORefs = procedure(cn,id: Cardinal;
                       fe: PIntegers; fe_n: Integer) of object;

  TGetValue = function(p,n: int; out v: Double): Boolean of object;

  dm_Hdr = record
    ver,scale,zzz,fot,srct: byte;
    nil1,nil2: byte; sys: tsys; r_div: Double;
    y_get,y_upd,y_mag,y_pab,dm_scale,dm_nnn: Integer;
    nom,idt,obj,loc,st_loc,own,access: string;
    create_hf_16: longbool
  end;

  dm_Rec = record
    Code,mind,hind: Longint;
    Tag,View: byte; Color: word;

    case Integer of
  0:  (ox1,oy1,ox2,oy2: longint);
  1:  (o_lt,o_rb: TPoint);
  end;

  dm_Node = record
    Len: word; Flags,Tag: byte;
    Link,Child: Longint; dRec: dm_Rec
  end;

  pdm_Nodes = ^tdm_Nodes;
  tdm_Nodes = Array[0..255] of dm_Node;

  pcn_rec = ^tcn_rec;
  tcn_rec = record
    rcid: Cardinal;
    rver: Word;
    rcnm: Byte;
    bits: Byte;

    fe: Cardinal;

    case Integer of
  0:  (Pos: TPoint;
       ZPos: Integer);

  1:  (VPos: VPoint);

  2:  (vc1: Cardinal;
       vc2: Cardinal;
       sgp: Integer);

  3:  (OBJL: Cardinal;
       mfp: Integer;
       hfp: Integer);
  end;

  TInsObject = record
    Code,Loc,Color: int;
    Tree: TPoint
  end;

  tdmNodeList = class(TCustomList)
    constructor Create;
  end;

const
  fe_max = 256;

type
  tfe_id = array[0..fe_max-1] of Cardinal;

  pmf_buff = ^tmf_buff;
  TCallback_draw = function(mf: pmf_buff): Integer of object;

  TCallback_edge = function(lp: PLLine; hp: PIntegers;
                            out lp_lt,lp_rb: TPoint;
                            pack,mark: Boolean): integer of object;

  TCallback_node = function(X,Y,Z: Integer): TPoint of object;

  tmf_buff = record
    lp: PLLine;
    hp: PIntegers;
    lp_Max: Integer;
    loc: Integer;

    is_mark: Boolean;
    is_break: Boolean;
  end;

  PClickData = ^TClickData;
  TClickData = record
    loc,dist,mf,lev: int
  end;

  PClickDataArray = ^TClickDataArray;
  TClickDataArray = Array[0..255] of TClickData;

  TDmwClickList = class(TInt64List)
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

    function xAdd(Ind: int; Ptr: Int64;
                  loc,dist,mf,lev: int): int;

    function xPop(Ind: int): bool;

    procedure Pair(p1,p2: Int64; lock: bool);

    function IncLevel(ptr: int64; up: int): int;
    function GetLevel(ptr: int64): int;

  private
    fData: TCustomList;

    fPos: TPoint;

  public
    property Pos: TPoint read fPos write fPos;
    property Data: TCustomList read fData;
  end;

  tcn_list = class(TVirtMem)

    constructor Create;
    destructor Destroy; override;

    function Add_rec(rec: pcn_rec): Cardinal;
    function Get_rec(I: Integer; rec: pcn_rec): Cardinal;

    procedure Reindex(I: Integer; Id: Cardinal);

    function Return_item(Id: Cardinal): Boolean;
    function Doctor(is_max: Boolean): Boolean;

  private
    fvm_Index: TIndexList;
  public
    property vm_Index: TIndexList read fvm_Index;
  end;

  tdm = class(TVirtTree)

    constructor Create(cash: Integer);
    destructor Destroy; override;

    function Get_info(magic: PChar;
                      buf: Pointer;
                      len: Integer): Boolean;

    procedure put_info(magic: PChar;
                       buf: Pointer;
                       len: Integer);

    function Begin_cn: Boolean;

    function vm_mem(out sf,mf,hf: TVirtMem): Boolean;
    function vm_Back(out sf,mf,hf: TVirtMem): Boolean;

    function vm_This(APath: PChar): Boolean;

    function vm_Open(APath: PChar; rw: Boolean;
                     out sf,mf,hf: TVirtMem): Boolean;

    function vm_Make(APath: PChar; fmt: int;
                     out sf,mf,hf: TVirtMem): Boolean;

    procedure vm_Close;

    function vm_ReadOnly(out sf,mf,hf: TVirtMem): Boolean;
    function vm_Update(out sf,mf,hf: TVirtMem): Boolean;
    function vm_Recycle(out sf,mf,hf: TVirtMem): Boolean;

    procedure vm_Rename(Path: PChar);

    function vm_FileName: String;

    function Add_vc(vc: pcn_rec): Cardinal;
    function Add_ve(ve: pcn_rec): Cardinal;
    function Add_fc(fc: pcn_rec): Cardinal;

    function Get_vc_Max_id: Cardinal;
    function Get_ve_Max_id: Cardinal;

    function vc_IndexOf(Id: Cardinal): Integer;
    function ve_IndexOf(Id: Cardinal): Integer;
    function fc_IndexOf(Id: Cardinal): Integer;

    function get_vc(Id: Cardinal; vc: pcn_rec): Boolean;
    function get_ve(Id: Cardinal; ve: pcn_rec): Boolean;
    function get_fc(Id: Cardinal; fc: pcn_rec): Boolean;

    function get_vc_rec(I: Integer; vc: pcn_rec): Cardinal;
    function get_ve_rec(I: Integer; ve: pcn_rec): Cardinal;
    function get_fc_rec(I: Integer; fc: pcn_rec): Cardinal;

    procedure fc_reinfex(I: Integer; Id: Cardinal);

    procedure Put_ve(Id: Cardinal; ve: pcn_rec);
    procedure Put_vc(Id: Cardinal; vc: pcn_rec);
    procedure Put_fc(Id: Cardinal; fc: pcn_rec);

    function Delete_ve(Id: Cardinal): Boolean;
    function Delete_vc(Id: Cardinal): Boolean;
    function Delete_fc(Id: Cardinal): Boolean;

    function Return_ve(Id: Cardinal): Boolean;
    function Return_vc(Id: Cardinal): Boolean;

    procedure Link_fe_ve(ve_id,fe_id: Cardinal; is_link: Boolean);
    procedure Link_fe_vc(vc_id,fe_id: Cardinal; is_link: Boolean);

    function Get_vc_ref(Vc: Cardinal; buf: PIntegers;
                        BufMax: Integer): Integer;

    function Get_vc_xref(Vc: Cardinal; List: TInt64List): Integer;

    function Get_fe(ve_Id,vc_Id: Cardinal;
                    fe: PIntegers; BufMax: Integer): Integer;

    procedure vc_Put_fe(Id: Cardinal;
                        fe: PIntegers; Count: Integer);

    procedure ve_Put_fe(Id: Cardinal;
                        fe: PIntegers; Count: Integer);

    function Add_fe(ve_Id,vc_Id: Cardinal;
                    List: TInt64List): Integer;

    function ve_Contains_vc(ve_Id,vc_Id: Integer): Boolean;
    function Is_free_node(vc_Id: Integer): Boolean;

    function vc_Lookup(const V: VPoint): Cardinal;

    procedure fe_backup;

    function fe_doctor(vc_fe,ve_fe: TIndexList): Boolean;
    procedure fe_restore(vc_fe,ve_fe: TIndexList);

    function cn_Doctor(is_max: Boolean): Boolean;

    procedure fe_fc_Update(fc: Cardinal;
                           lp: PLPoly; lp_n: Integer;
                           IsUpdate: Boolean);

    procedure fe_fc_Exclude(fe: Cardinal);

    function Get_fe_fc(fe_Id: Cardinal;
                       out fc: tfe_id): Integer;

    procedure vc_update_attv(id,attv: Cardinal;
                             IsUpdate: Boolean);

    procedure ve_update_attv(id,attv: Cardinal;
                             IsUpdate: Boolean);

    function Get_vc_attv(id: Cardinal): Cardinal;
    function Get_ve_attv(id: Cardinal): Cardinal;

  private
    fdoc: IStorage;
    sf_,mf_,hf_,fe_: TVirtMem;
    fe_fc,vc_hf,ve_hf: TIndexMem;
    vc_,ve_,fc_: tcn_list;

    fdll_alx: THandle;
    fdll_rel: THandle;
    fdll_rd: THandle;

    fdm_lib: THandle;
    fvm_dmw: IDmwDraw;

    fvm_fmt: int;

    vc_index: TIndexList;
    ve_index: TIndexList;
    fc_index: TIndexList;

    fe_stg: IStorage;
    vc_stg: IStorage;
    ve_stg: IStorage;
    fc_stg: IStorage;

    fhf_x16: Longbool;
    fold_neva: Longbool;
    fhf_only_x16: Longbool;

    fOn_fe_doctor: TOnORefs;

    function Get_mf_x32: bool;

    function Get_Autosave: Boolean;

    function Get_is_fe_temp: Boolean;
    procedure Set_is_fe_temp(Value: Boolean);

    procedure Set_is_vc_temp(Value: Boolean);
    procedure Set_is_ve_temp(Value: Boolean);

    function Alloc_mf_hf: Boolean;
    procedure Free_mf_hf;

    function Get_Active: Boolean;

    function Get_is_cn: Boolean;
    procedure Set_is_cn(Value: Boolean);

    procedure Init_mf_fmt(sf,mf: TVirtMem);

    function fe_Update(Ptr,Id: Cardinal;
                       is_link: Boolean): Cardinal;

  protected
    fIntList1: TIntegerList;

  public
    vm_Path: TShortStr;

    property doc: IStorage read fdoc;

    property dm_lib: THandle read fdm_lib;
    property vm_dmw: IDmwDraw read fvm_dmw;

    property vm_Active: Boolean read Get_Active;
    property Autosave: Boolean read Get_Autosave;

    property is_cn: Boolean read Get_is_cn
                            write Set_is_cn;

    property old_neva: Longbool write fold_neva;

    property is_fe_temp: Boolean read Get_is_fe_temp
                                 write Set_is_fe_temp;

    property is_vc_temp: Boolean write Set_is_vc_temp;
    property is_ve_temp: Boolean write Set_is_ve_temp;

    property dm_fmt: int read fvm_fmt;
    property mf_x32: bool read Get_mf_x32;
    property hf_x16: bool read fhf_x16;

    property hf_only_x16: Longbool write fhf_only_x16;

    property On_fe_doctor: TOnORefs write fOn_fe_doctor;
  end;

  tdm_Map = class
    dm_Index: Integer;
    dm_scale: Integer;
    dm_ver: Integer;
    dm_Auto: Integer;

    dm_lt,dm_rb: TPoint;
    dm_lt1,dm_rb1: TPoint;
    dm_ab,win_ab: TPoint;

    LG_Tr: LG_Transit;
    LG_T: ab_Orient;

    sf,mf,hf: TVirtMem;
    Obj: TFastObjects;
    Idc: TIdcStg;

    lt_g,rb_g: TGauss; g_ed: Integer;

    dm_AutoSave,dm_Pred: Longbool;
    dm_doctor,mf_lock,mf_curve: Longbool;

    pack_enabled: Longbool;

    constructor Create(cash: Integer);
    destructor Destroy; override;

    function StrExcept(Str: PChar): int;

    procedure Close_Map;
    procedure Auto_Map(ok: Boolean);

    function GetPolyMax: int;

    function doc_get_info(magic: PChar;
                          buf: Pointer;
                          len: Integer): Boolean;

    procedure doc_put_info(magic: PChar;
                           buf: Pointer;
                           len: Integer);

    function dm_Create(Path: PChar; fmt: int): Boolean;

    function clip_Open(Path: PChar): Boolean;

    function dm_Objects_std_nn: Boolean;
    function dm_Backup: Boolean;

    function Open_Map(Path: PChar; rw: Boolean): Boolean;
    function Return_Map(Path: PChar; rw: Boolean): Boolean;

    function Enabled_Map: Boolean;
    function Update_Map: Boolean;
    function Edit_Map: Boolean;

    function Begin_Update: Boolean;
    procedure End_Update;

    function Doctor_Map: Boolean;

    function Offset_by_Id(Id: Integer): Integer;
    function Ptr_by_Id(P: Int64): Int64;
    function x_Ptr_by_Id(P: Int64): Int64;

    function GetParent(P: int64): int64;

    function Get_childs(Ptr: int;
                        list1: TIntegerList;
                        list2: TInt64List): int;

    function Get_dm_LRect(lp: PLPoly): Integer;
    function Get_dm_LOrient(lp: PLPoly): Integer;
    function Get_dm_Bound(lp: PLLine; lp_Max: Integer): Integer;

    procedure Get_dm_clip(Frame: PGPoly;
                          const sys: tsys;
                          out lt,rb: TPoint);

    function Get_lg_orient(out T: LG_Transit): Integer;
    function Get_LG_Transit(out T: LG_Transit): Boolean;

    function Update_dm_lt_rb: Boolean;
    function dm_Resolution: double;

    function mmpp(is_mm: Boolean): double;

    function dm_mm_k: double;
    function dm_ed: Integer;

    function dm_dist(r: Double; ed: Integer): Double;

    function chk_ppm(mm: Double): Double;

    function Align_grid(var p: TPoint): Boolean;

    function ms_unit: Integer;

    function StrNom(s: PChar): PChar;

    function Get_Stat(Stat: PIntegers;
                      StatMax: Integer): Integer;

    procedure Put_Stat(Stat: PIntegers; Count: Integer);

    function Points_to_Metre: double;
    function Points_to_mm: double;

    function dm_to_xyz(x,y,z: Integer): txyz;
    function xyz_to_dm(x,y,z: Double): VPoint;

    function dm_to_wgs(x,y,z: Integer): txyz;
    function wgs_to_dm(x,y,z: Double): VPoint;

    function trf_to_dm(const g: txyz; pps: Integer): VPoint;
    function dm_to_trf(x,y,z: Integer; out v: txyz): Integer;

    function Contains_Point(x,y: longint): Boolean;
    function Contains_Gauss(x,y: double): Boolean;
    function Contains_lp(lp: PLLine; loc: Integer): Boolean;
    function xContains_Point(const p: TPoint): Boolean;

    function Is_Clip_lp(lp: PLLine): Boolean;

    function Contains_Locator(lp: PLLine; lp_Max: Integer;
                              const lt,rb: TPoint): Boolean;

    function Link_Map(edit: Boolean): Boolean;
    function Link_update(backup: Boolean): Boolean;
    function Link_update_gauss: int;

    procedure Link_to_Bound(update: Boolean);
    function Link_Clear: Boolean;

    function Link_Point(x,y: longint;
                        const g: tgauss;
                        const f: tgeoid): longint;

    function x_Link_Point(x,y,cl: longint;
                          const g: xgeoid): longint;

    function Link_Sign: Integer;

    function g_Width: double;
    function g_Height: double;
    function bl_Grid: Boolean;

    function Open_Objects: Boolean;
    procedure ObjectsEvent(Sender: TObject);

    function Open_Blanks(Blanks: TBlnBank): Boolean;

    function Objects_Path(Path,Ext: PChar): PChar;
    function Objects_Name: string;

    function Map_Caption(capt: PChar): string;
    function Info_Path(dst,ext: PChar): PChar;

    function Is_Objects: longint;
    function Update_Cnt(dn: Integer): Integer;

    function Seek_Layer(Ind: Integer): longint;
    function Layer_by_Code(Code: Integer): longint;
    function Locate_Layer(Code: Integer): Boolean;

    function Get_layers(List: TPOintList): int;

    function Empty_Layers_Exist: bool;
    function Clean_Layers: int;

    procedure Undo_Object(p: Int64; cmd: Integer);

    function Init_Ins_Object(Code,Loc: int): TInsObject;

    function Auto_Object(var Ins: TInsObject;
                         lp: PLLine; hp: PIntegers;
                         Info: TInfoList): int;

    function Ins_Object(var dRec: dm_Rec;
                        lp: PLLine; hp: PIntegers;
                        Info: TInfoList): longint;

    function Pred_Object(prd,run: longint): longint;

    function xDel_Object(prd,run: longint): longint;
    function Del_Object(prd,run: longint): longint;

    function ext_Del_Object(P: Int64): Boolean;

    procedure delete_list(list: TIntegerList);

    function Return_Object(P: Int64): Int64;

    function lcut_Object(p: longint; lp: PLLine;
                         hp: PIntegers): longint;

    function Repeat_Object(p: int;
                           buf_lp: PLLine;
                           buf_hp: PIntegers): int;

    function Dup_Object(p: int;
                        buf_lp: PLLine;
                        buf_hp: PIntegers): int;

    function Dup_Object1(p: int; lp: PLLine;
                         hp: PIntegers): longint;

    procedure Owner_Object(p, Code,Owner: int);

    function Add_Layer(Code,Lev: Integer): longint;

    function Add_Object(Layer,Owner: int;
                        var dRec: dm_Rec;
                        lp: PLLine; hp: PIntegers): int;

    function Add_Hole(p: longint; lp: PLLine; hp: PIntegers): longint;

    function Is_Hole(run,par: longint): Boolean;

    function z_axe_Exist(p: Int64): Boolean;

    function Get_zrange(ptr: Int64; clip: PLPoly;

                        lp: PLLine; hp: PIntegers;
                        mesh: TPolyMesh;

                        out zr: TRange): Boolean;

    procedure hf_Trunc_z_axe(p: longint);
    procedure Trunc_z_axe(p: longint);

    function Get_Poly_Cnt(p: Int64): Integer;
    function Get_Poly_Count(p: Int64): Integer;

    function Draw_HF(p, n: int; id: Id_Tag;
                     ansi: int; s: PWideChar): PWideChar;

    function Hint_HF(p, n: int; s: PChar; len: int): bool;

    function xWide_Text(p: longint): Boolean;
    function xDraw_Text(ptr,ansi: int; s: PWideChar): PWideChar;

    function xLoad_str(p,n: longint; s: PChar): PChar;
    function xLoad_Ansi(p: longint; s: PChar): PChar;

    function Get_Bound(p: longint; out lt,rb: lxyz): Integer;

    function Get_house(offs: int; lp: PLLine;
                       out lt,rb: TPoint): int;

    function Get_Poly(p: longint; lp: PLLine; lp_max: Integer): Integer;
    function xGet_Poly(p: longint; lp: PLLine; hp: PIntegers; lp_max: Integer): Integer;
    function xGet_mf(p: Int64; lp: PLLine; hp: PIntegers): Integer;
    function xGet_mf1(p: Int64; lp: PLLine; hp: PIntegers): Integer;

    function mf_poly(p: Int64; poly: TPolyList;
                     lp: PLLine; hp: PIntegers): Integer;

    function Get_Vertex(p: Int64; lp_i: Integer;
                        out v: VPoint): Boolean;

    procedure Put_Vertex(p: Int64; lp_i: longint;
                         const v: VPoint);

    function Update_Profile(p: Int64; xy: PGPoly;
                            xy_n,xy_ind: Integer): bool;

    function Get_Polybuf(p: longint; lp: PLLine): Integer;
    function xGet_Polybuf(p: longint; lp: PLLine; hp: PIntegers): Integer;
    function Get_Polygon(p: longint; lp: PLLine; hp: PIntegers): Integer;

    function Get_xyz(p: Int64; out v: lxyz): Boolean;
    function wgs_centre(p: Int64; out c: txyz): double;

    procedure Centre_Object(p: Int64; out c: TPoint);

    function Ind_Blank(p: Int64): Integer;
    function obj_Ind_Blank(code,loc: Integer): Integer;
    function Capt_Object(p: Int64; status: Boolean): string;

    function Get_hf(p, n: Integer; id: Id_Tag; var equ): Boolean;

    function Get_sval(p, n: Integer; Str: PChar): Boolean;

    procedure Put_hf(p, n: Integer; id: Id_Tag; var equ);
    procedure Put_xy(p, n: Integer; x,y: Double);

    procedure Put_rvec(p, n: Integer; id: Id_Tag;
                       fp: PDoubles; Count: Integer);

    procedure Put_ivec(p, n: Integer; id: Id_Tag;
                       ip: PIntegers; Count: Integer);

    procedure Del_hf(p, n: Integer; id: Id_Tag);

    function Get_Int(p, n: Integer): longint;
    function Get_Real(p, n: Integer): float;
    function Get_Double(p, n: Integer): double;

    function Get_id(p: Int64): uint;

    function GetValue(p, n: Integer; out v: Double): Boolean;

    function xGet_Int(p, n: Integer; out i: longint): Boolean;
    function xGet_Real(p, n: Integer; out r: float): Boolean;
    function xGet_Double(p, n: Integer; out r: double): Boolean;
    function Get_Gauss(p, n: Integer; out g: tgauss): Boolean;
    function xGet_Str(p, n: Integer; s: PChar): PChar;

    procedure xPut_Int(p, n: Integer; v: longint);
    procedure xPut_Real(p, n: Integer; v: float);
    procedure xPut_Double(p, n: Integer; v: double);
    procedure xPut_Str(p, n: Integer; s: PChar);

    procedure xAssign_Int(p, n: Integer; v: longint);
    procedure xAssign_Real(p, n: Integer; v: float);
    procedure xAssign_Double(p, n: Integer; v: double);

    function xGet_z(p: Integer; out z: double): Integer;
    function xDelete_z(p: Integer): Boolean;

    function xGet_Relief(p: Integer; out h: double): Boolean;

    function xGet_Height(p: Integer; out h: double): Boolean;
    procedure xPut_Height(p: Integer; h: double);

    function Get_hg(p: Integer;
                    iv,ic: PIntegers;
                    out r1,r2: int;
                    Hint: PChar): int;

    function Get_hgt(p: Integer;
                     Name,Font,Style: PChar): int;

    function Get_mm_Org: TGauss;
    procedure Get_mm_Rect(out x,y,w,h: Double);

    procedure Set_mm_Org(x,y: Double);
    procedure Set_mm_Extent(w,h: Double);
    procedure Set_mm_Rect(x,y,w,h: Double);

    procedure dm_Local_Bound(out lt,rb: TPoint; k: float);
    function dm_pps_Bound(out lt,rb: tgauss): Integer;
    function dm_pps_Bound1(out lt,rb: tgauss): Integer;
    procedure dm_Gauss_Bound(out lt,rb: tgauss);

    procedure Get_dm_Hdr(out hdr: dm_Hdr);
    procedure Put_dm_Hdr(const hdr: dm_Hdr);

    function dm_ReadOnly: Boolean;
    function Get_Password: string;
    procedure Put_Password(pwd: string);

    function xTag_Object(p: Int64): int;
    function xLoc_Object(p: Int64): int;
    function xCode_Object(p: Int64; out code: int): int;

    function Is_Link_Object(p: longint): Boolean;

    function Code_Object(p: Int64): longint;
    function Tag_Object(p: Int64): Integer;
    function Color_Object(p: Int64): Integer;
    function Level_Object(p: Int64): Integer;
    function Text_up(p: longint): Integer;

    function Style_Object(p: longint): Integer;

    function Height_Object(p: longint): Double;

    function Tag_Parent(p: longint): Integer;

    function Get_Node(p: longint; out Node: dm_Node): Integer;
    function xGet_Node(p: Int64; out Node: dm_Node): Integer;

    function Get_Object(p: longint; out dRec: dm_Rec): Integer;
    procedure Put_Object(p: longint; var dRec: dm_Rec);

    function xGet_Object(p: Int64; out dRec: dm_Rec): Integer;
    procedure xPut_Object(p: Int64; var dRec: dm_Rec);

    function Port_Object(p: Int64; out lt,rb: TPoint): Integer;

    function Port_Contains(const lt,rb: TPoint;
                           p: Int64; d: Integer): Boolean;

    function Get_Flags(p: Int64): Integer;
    function xGet_Flags(p: longint): Integer;
    function Up_Flag(p,fl: longint): Boolean;
    function Set_Flag(p,bit: longint; up: Boolean): Integer;
    function Xor_Flag(p,bit: longint): Integer;
    procedure Put_Flags(p,flags: longint);

    procedure xPut_Flags(p: Int64; flags: Integer);

    function flag_del(p: Int64): Boolean;
    function flag_attr(p: Int64): Boolean;
    function edit_mf(p: Int64): Boolean;
    function edit_hf(p: Int64): Boolean;
    function flag_draw(p: longint): Boolean;

    function Is_delete(p: Int64): Boolean;

    function Editing_attr(p: Int64): Boolean;
    function Editing_mf(p: Int64): Boolean;
    function Editing_hf(p: Int64): Boolean;

    function Editing_geo(p: Int64): Integer;

    function Ok_Object(p: Int64): Boolean;
    function Identify(p: Int64; id: uint): Int64;
    function Identify1(p: Int64; id: uint): Int64;
    function Identify2(p: Int64; id: uint): Int64;

    function Rect_Object(p: Int64; out lt,rb: TPoint): Boolean;
    function xRect_Object(p: Int64; out lt,rb: TPoint): Boolean;

    function Object_contains(p: Int64; X,Y: int): Boolean;

    function Update_Code1(ptr: Int64; code: int): bool;

    procedure Update_Code(p, Code: int);
    function Update_Loc(p, Loc: int): bool;
    procedure Update_Code_Color(p, code, cl: Integer);
    procedure Update_Color(p, cl: Integer);
    procedure Update_cl(p, cl: Integer);
    procedure Update_Level(p, View: Integer);
    procedure Update_Up(p, Up: Integer);

    function xRestore_Color(p: int;
                            out oColor: int;
                            only_idc: longbool): bool;

    function Restore_Color(p: int): int;

    procedure Update_Ansi(p: longint; s: PChar);

    procedure Update_mf(p,loc: Integer; lp: PLLine);

    function xUpdate_mf(p: Int64; loc: Integer;
                        lp: PLLine; hp: PIntegers): Boolean;

    function old_mf(p: longint;
                    var dRec: dm_Rec;
                    lp: PLLine; hp: PIntegers): Boolean;

    function Update_xyz(p,loc: Integer;
                        lp: PLLine; hp: PIntegers): Boolean;

    function Get_mesh(p: Integer; mesh: TPolyMesh): Boolean;
    function Update_mesh(p: Integer; mesh: TPolyMesh): Boolean;

    procedure Update_Text(p: Integer; str: PWideChar);

    function Get_next_nnnn: Cardinal;
    function Gen_next_nnnn: Cardinal;

    function IntLink(p: longint): longint;
    function StrLink(p: longint; s: PChar): PChar;
    function Assign_Link(p,id: longint): longint;
    function Update_Link(p: longint): longint;
    function Release_link(id: Integer): Integer;

    function Str_Object(s,cmd: PChar; p: longint): PChar;

    function Poly_Calc(p: Int64;
                       lp: PLLine; hp: PIntegers;
                       lp_max: Integer): txyz;

    function Poly_Calci(loc: Integer;
                        lp: PLLine; hp: PIntegers): txyz;

    function Inside_Object(par,run: Integer): Boolean;

    function Up_Object(p: longint): longint;
    procedure Cls_Parent(par: longint);

    function Load_hf(p: Int64; stm: thfStream): Integer;
    procedure Update_hf(p: Int64; stm: TStream);

    function Get_Info(p: Int64; Info: TInfoList): Integer;
    function Update_Info(p: Int64; Info: TInfoList): Boolean;
    procedure Clear_Info(p: Int64);

    procedure Copy_Info(dst_p,src_p: longint;
                        src_dm: tdm_Map; id_gen: Boolean);

    procedure Free_Object(p: longint);

    procedure Edit_Object(p: Int64; id,cmd: longint); virtual;

    function cn_add(RCNM,RCID,RVER,RUIN, P1,P2: Cardinal;
                    lp: PLLine; hp: PIntegers): Cardinal;

    function add_vc(const p: TPoint; z: PInteger): Cardinal;
    function add_vc1(const p: TPoint; z: PInteger): Cardinal;

    function vc_add(lp: PLLine; hp: PIntegers): Int64;

    function ve_add(vc1,vc2: Cardinal;
                    lp: PLLine; hp: PIntegers): Int64;

    function Add_holes(ptr: int; poly: TPolyList;
                       lp: PLLine; hp: PIntegers): Integer;

    function Del_holes(ptr: int): Integer;
    function Undo_holes(ptr,cmd: int): Integer;

    function Get_holes(ptr: int;
                       list: TIntegerList;
                       poly: TPolylist;
                       buf: PLLine): int;

    function recode_holes(ptr,code1: int): int;

    function Nameof(Name: PChar): int;

    procedure cn_attv(cn,id,attv: Cardinal; IsUpdate: Boolean);

    function cn_draw(p: Integer;
                     mf_buf: pmf_buff;
                     draw: TCallback_draw): Integer;

    function cn_xdraw(p: Integer;
                      poly: TPolyList;
                      mf_buf: pmf_buff;
                      node: TCallback_node;
                      edge: TCallback_edge): Integer;

    function cn_poly(p: Integer; poly: TPolyList;
                     lp: PLLine; hp: PIntegers): Integer;

    function fe_contains(p,ref: Cardinal): Integer;

    procedure fe_bound(p: Integer);

    procedure fe_ornt(p: Integer; lp,buf: PLLine; lp_Max: Integer);

    function Link_fe_cn(ptr: int; link: bool): bool;

    procedure Update_fe(p: Integer; lp,buf: PLLine);
    procedure After_fe(p: Integer; lp,buf: PLLine);

    function fe_ref(p, id1,id2: Integer;
                    tags: tbyte_set): Boolean;

    function Get_ve_vc(const P: TPoint; out vc1,vc2: Cardinal): Boolean;
    function Get_lp_vc(lp: PLLine; out vc1,vc2: Cardinal): Boolean;
    function fe_lock(lp: PLLine): Boolean;

    function Get_ve_next(Vc,Ve: Cardinal; lp: PLLine): Cardinal;

    function ve_vc_Used(ve,vc: Cardinal): Integer;

    function Get_ve_mf(ve: pcn_rec;
                       lp: PLLine; hp: PIntegers;
                       lp_Max: Integer): Integer;

    function Get_ve_Poly(Id: Cardinal;
                         lp: PLLine; hp: PIntegers;
                         lp_Max: Integer): Integer;

    function More_ve_Poly(const Pt: TPoint;
                          lp: PLLine; hp: PIntegers;
                          lp_Max: Integer): Integer;

    procedure Update_vc(Id: Cardinal;
                        const P: TPoint;
                        ZPos: PInteger);

    procedure Update_ve(Id, Vc1,Vc2: Cardinal;
                        lp: PLLine; hp: PIntegers);

    function Split_ve_lp(ve_Id,vc1_Id,vc2_Id: Cardinal;
                         lp: PLLine; lp_Max: Integer;
                         Alt: TIntegerList): Boolean;

    procedure Split_ve(ve,vc1,vc2: Cardinal;
                       buf: PLLine; lp_Max: Integer;
                       Alt: TIntegerList);

    function IsEqualNodes(vc1,vc2: Cardinal): Boolean;

    function is_ve_merge(ve1,ve2: Cardinal;
                         out vc: Cardinal): Boolean;

    function ve_fe_join(ve1,ve2: Cardinal; buf: PLLine;
                        is_edit: Boolean): Boolean;

    function fc_add(RCNM,RCID,RVER,RUIN, OBJL: Cardinal;
                    lp: PLLine; hf: TInfoList): Cardinal;

    function fc_Get_list(Id: Cardinal;
                         lp: PLLine; lp_Max: Integer): Integer;

    function fc_Contains_fe(fc,fe: Cardinal): Boolean;

    procedure fc_Update(Id, OBJL: Cardinal; lp: PLLine);

    function fc_Tools(fc_Id,fe_Id: Cardinal;
                      Include,fe_fc: Boolean): Boolean;

    procedure fc_fe_Exclude(fe_Id: Cardinal);

    function fc_Get_Bound(Id: Cardinal;
                          out lt,rb: lxyz): Boolean;

  private
    fTree: tdm;
    fEditMode: Integer;

    fOnEditObject: TOnEditObject;
    fOnUndoObject: TOnUndoObject;
    fve_Identify: TOnIdentify;
    fOnGetValue: TGetValue;
    fOnExpr: TOnIdcExpr;

    fnn_4,fnn_1,fnn_7,fnn_9: Integer;

    fz_res,fobj_index: Integer;

    fIndex: TIndexList;

    fExceptCode: int;

    fmf_x32: Longbool;
    fhf_x16: Longbool;

    fIsDoctor: Longbool;
    fIsEnabled: Longbool;
    fIsEditor: Longbool;
    fIsStorage: Longbool;
    fIsNotify: Longbool;
    frel_s57: Longbool;
    fIs_s57: Longbool;

    procedure Set_EditMode(Value: Boolean);

    function Get_FileName: String;

    function Get_is_dm: Boolean;
    function Get_CanModify: Boolean;

    procedure Fill_hp(p: Integer; hp: PIntegers; N: Integer);

    function Load_Height(hp: PIntegers;
                         Ind,Count,Flags: Integer): Integer;

    function Load_Poly(mind: longint;
                       lp: PLPoly; hp: PIntegers;
                       lp_Max,Flags: Integer): Integer;

    function Update_Poly(var mf_ind,flags: longint;
                         lp: PLLine; hp: PIntegers): Boolean;

    function Bound_Poly(ind,Flags: longint;
                        out lt,rb: VPoint): Integer;

    function Bound_ve(Id: Integer;
                      out lt,rb: VPoint): Boolean;

    function Get_vc_Count: Integer;
    function Get_ve_Count: Integer;
    function Get_fc_Count: Integer;

    function Get_vc_Id(Ind: Integer): Cardinal;
    function Get_ve_Id(Ind: Integer): Cardinal;
    function Get_fc_Id(Ind: Integer): Cardinal;

    function Load_ve_mf(ve: pcn_rec;
                        lp: PLPoly; hp: PIntegers;
                        lp_Max: Integer): Integer;

  protected
    fintList1: TIntegerList;

  public
    property Tree: tdm read FTree;
    property Is_dm: Boolean read Get_is_dm;

    property IndexList: TIndexList read fIndex;

    property FileName: String read Get_FileName;

    property Is_Enabled: Longbool read fIsEnabled write fIsEnabled;
    property Is_Editor: Longbool write fIsEditor;

    property Is_Storage: Longbool read fIsStorage;
    property Rel_s57: Longbool read fRel_s57 write fRel_s57;
    property Is_s57: Longbool read fIs_s57;

    property IsNotify: Longbool write fIsNotify;

    property mf_x32: Longbool read fmf_x32;
    property hf_x16: Longbool read fhf_x16;

    property CanModify: Boolean read Get_CanModify;

    property nn_4: Integer read fnn_4;
    property nn_1: Integer read fnn_1;
    property nn_7: Integer read fnn_7;

    property z_res: Integer read fz_res write fz_res;

    property obj_index: Integer read fobj_index;

    property id_Index: TIndexList read fIndex;

    property vc_Count: Integer read Get_vc_Count;
    property ve_Count: Integer read Get_ve_Count;
    property fc_Count: Integer read Get_fc_Count;

    property vc_Id[Ind: Integer]: Cardinal read Get_vc_Id;
    property ve_Id[Ind: Integer]: Cardinal read Get_ve_Id;
    property fc_Id[Ind: Integer]: Cardinal read Get_fc_Id;

    property EditMode: Boolean write Set_EditMode;
    property IsDoctor: Longbool read fIsDoctor write fIsDoctor;

    property ExceptCode: int write fExceptCode;

    property OnEditObject: TOnEditObject read fOnEditObject
                                         write fOnEditObject;

    property OnUndoObject: TOnUndoObject read fOnUndoObject
                                         write fOnUndoObject;

    property ve_Identify: TOnIdentify write fve_Identify;

    property OnGetValue: TGetValue write fOnGetValue;
    property OnExpr: TOnIdcExpr write fOnExpr;
  end;

  tdm_Dest = class(tdm_Map)
    constructor Create;
    destructor Destroy; override;

    function Open_dst(Path: PChar): Boolean;

    function dst_Object(src_dm: tdm_Map; p: Integer;
                        lp: PLLine; hp: PIntegers): Integer;

    function dst_fc(fc: pcn_rec; lp: PLLine;
                    hf: TInfoList): Cardinal;

    function dst_seek_Layer(Code: int): bool;

  private
    Info: TInfoList;
    fmax_NNNN: Cardinal;
    dst_Count: Integer;

    fControl: TIndexControl;
    fTransit: TIndexList;

    fLayers: VPointList;
    fLayer: PVPoint;

    fIndexing: Boolean;
    fIsTransit: Boolean;

    procedure Set_max_NNNN(Value: Cardinal);

    function dst_Index(id,ptr: Cardinal): Cardinal;

  public
    property Indexing: Boolean write fIndexing;
    property IsTransit: Boolean write fIsTransit;
    property max_NNNN: Cardinal write Set_max_NNNN;

    property Transit: TIndexList read fTransit;
  end;

  tdm_cn = class(tdm_Dest)
    destructor Destroy; override;
  end;

  tdm_Copy = class(TCopyTree)
    constructor Create(Src,Dst: tdm_Map);
    destructor Destroy; override;

    procedure Update_Info(Inp_P,Dst_P: longint); override;
  public
    src_dm,dst_dm: Tdm_Map;
    fhf: TInfoList;
  end;

  TObjectFunc = function(p: Longint;
                         const dRec: dm_Rec): Boolean of object;

  TObjectFunc1 = function(p,lev: Longint;
                          const dRec: dm_Rec): Boolean of object;

  TObjectFunc2 = function(p,id,lev: Longint): Boolean; stdcall;

  tdmScan = class
    constructor Create(dm: tdm_Map);

    function Scan_tree(frst,up: Boolean): longint;

    function Scan_Childs(p: Int64): longint;
    function Edit_Childs(p: Int64): longint;

    function This_Object(p: longint): Boolean; virtual;
    function Goto_Down(p: longint): Boolean; virtual;
    procedure Back_Down(p: longint); virtual;

    function Restore_Object(p: longint;
                            is_mf: Boolean): Boolean; virtual;

    function That_Object(p: longint): Boolean;

  private
    fLevel: Integer;

    fRun_Parent: longint;
    fPred_Run: longint;

    ffind_pos: TPoint;

    fOnObject: TObjectFunc;
    fOnObject1: TObjectFunc1;
    fOnObject2: TObjectFunc2;

  public
    dm_: tdm_Map;
    dmNode: dm_Node;
    Start: longint;
    is_Break: Longbool;

    property Level: Integer read fLevel
                            write fLevel;

    property Run_Parent: longint read fRun_Parent
                                 write fRun_Parent;

    property Pred_Run: longint read fPred_Run;

    property find_pos: TPoint read ffind_pos
                              write ffind_pos;

    property OnObject: TObjectFunc write fOnObject;
    property OnObject1: TObjectFunc1 write fOnObject1;
    property OnObject2: TObjectFunc2 write fOnObject2;

    function This_Hole(p: longint): Boolean;
  end;

  tdmList = class(tdmScan)
    constructor Create(dm: tdm_Map;
                       AList1: TIntegerList;
                       AList2: TInt64List);

    function This_Object(p: longint): Boolean; override;
  private
    fList1: TIntegerList;
    fList2: TInt64List;
  end;

  vdmScan = class(tdmScan)
    function Goto_Down(p: longint): Boolean; override;
  end;

  TUserScan = class(tdmScan)
    constructor Create(dm: tdm_Map);
    destructor Destroy; override;
  private
    fTopp: Longint;
    fRunp: Longint;
  end;

  VUserScan = class(vdmScan)
    constructor Create(dm: tdm_Map);
    destructor Destroy; override;
  private
    fTopp: Longint;
    fRunp: Longint;
  end;

  tdmEnum = class(tdmScan)
    constructor Create(dm: tdm_Map);
    destructor Destroy; override;
    function This_Object(p: longint): Boolean; override;
    function Release(out Buf: PIntegers): Integer;
  private
    fList: TIntegerList;
  end;

  tdm_3d = class(tdmEnum)
    constructor Create(dm: tdm_Map;
                       AFlags: Integer;
                       ARgn: PLPoly);

    function This_Object(p: longint): Boolean; override;

    function Goto_Down(p: longint): Boolean; override;
    procedure Back_Down(p: longint); override;

  private
    fFlags: Integer;
    fRgn: PLPoly;
    fStack: VLLine;
  end;

  xScanLink = class(tdmScan)
    function This_Object(p: longint): Boolean; override;
  end;

  xLookLayer = class(tdmScan)
    constructor Create(dm: tdm_Map; Code: Integer);
    function This_Object(p: longint): Boolean; override;
    function Goto_Down(p: longint): Boolean; override;
  public
    dmCode: Integer;
  end;

  xLookCode = class(tdmScan)
    constructor Create(dm: tdm_Map; Code,Loc: int);
    function This_Object(p: longint): Boolean; override;
  private
    dmCode,dmLoc: int;
  end;

  xLookObject = class(xLookCode)
    function This_Object(p: longint): Boolean; override;
  end;

  xLookChar = class(xLookCode)
    constructor Create(dm: tdm_Map; Code,Loc: Integer;
                       n: Integer; t: Id_Tag; i: longint;
                       f: Double; s: PChar);

    function This_Object(p: longint): Boolean; override;

  private
    nn: Integer; id: Id_Tag;
    ii: longint; ff: Double;
    ss: TShortStr;
  end;

  xLookName = class(xLookCode)
    constructor Create(dm: tdm_Map;
                       AId,ACode,ALoc,Ann: int;
                       AName: PChar);

    function This_Object(ptr: longint): Boolean; override;

  private
    fName: TShortStr;
    fId: uint;  fnn: int;
  end;

  tdmTool = class(tdmScan)
    procedure Tool(p: longint);
  end;

  tdmCalc = class(tdmTool)
    constructor Create(dm: tdm_Map);
    destructor Destroy; override;
    function This_Object(p: longint): Boolean; override;
  private
    buf: PLLine; max: Integer;
    s: double;
  end;

  xdmScan = class(tdmScan)
    constructor Create(dm: tdm_Map);
    destructor Destroy; override;

    function Goto_Down(p: longint): Boolean; override;
    procedure Back_Down(p: longint); override;

    function Owner_run: longint;
    function Owner_Code: longint;
    function Owner_Loc: Integer;
    function Owner_Lev: Integer;

  private
    Owner_: TList;
  end;

  tdm_cl = class(tdm_map)

    procedure Init_graphics;
    function Object_cl(Code,loc: Integer): Integer;

  private
    fast: TObjFast;
    cash: Obj_Rec;
    _code,_loc: Integer;
  end;

  tdm_zrange = class(tdmScan)

    constructor Create(dm: tdm_Map; AClip: PLPoly;
                       Abuf: PLLine; Ahp: PIntegers;
                       Amesh: TPolyMesh);

    function Get_zrange(out zr: TRange): Boolean;

    function This_Object(p: longint): Boolean; override;

  private
    fClip: PLPoly;

    fbuf: PLLine;
    fhp: PIntegers;
    fmesh: TPolyMesh;

    fRange: TRange;
    flt,frb: TPoint;
  end;

  tdm_rebound = class(tdmScan)

    function This_Object(p: longint): Boolean; override;

  private
    fCount: int;
    flt,frb: TPoint;

  public
    property Count: int read fCount;
    property lt: TPoint read flt;
    property rb: TPoint read frb;
  end;

var
  dmScaleList: TIntegerList;
  attvStrings: TStringList;
  attv402: TStringList;

function dm_Transit_loc(loc: int): int;

function Unpack_mf(mf: PBytes; len: Integer;
                   lp: PLPoly; hp: PIntegers;
                   lp_Max,Flags: Integer): Integer;

function Get_scale_list(scale: Integer;
                        out lp: PIntegers): Integer;

function cn_Id_str(cn,id: Integer): String;
function Str_to_cn_Id(def_cn: Integer; const S: String): Int64;

function cn_ptr(rcid,rcnm,ornt,usag,mask: Integer): TPoint;
function fc_ptr(rcid,rcnm,rind: Integer): TPoint;

procedure swap_ve_oref(lp: PLPoly; N: Integer);
procedure x_swap_ve_oref(lp: PLLine; lp_i: Integer);

function dm_mpp(ppm: Double; out mpp: Double): Integer;

function Alloc_LPolyBuf: PLLine;
function Alloc_ZPolyBuf(out lp_z: PIntegers): PLLine;

procedure Init_dm_Hdr(out hdr: dm_Hdr);
function is_Hole_Code(Code: Integer): Boolean;

function this_dm(Path: PChar): Boolean;

function Id_to_Offset(dm: tdm_Map; id: longint): longint;

function dm_Indexing(dm: tdm_Map; AIndex: TIndexList;
                     only_id: Boolean): Integer;

function dm_Index_range(dm: tdm_Map;
                        out minv,maxv: Integer): Integer;

function dm_Index_list(dm: tdm_Map; AIndex: TIntegerList): Integer;
procedure dm_Index_ctrl(dm: tdm_Map; ACtrl: TIndexControl);

function dm_Square_Childs(dm: tdm_Map; p: longint): double;

procedure dm_Update_Version(dm: tdm_Map);

implementation

uses
  dynlibs,SysUtils,
  convert,ofiles,xdos,
  Storage,xbl_use,ivg,
  xline,xpoly,xpoly1,xy;

function Get_scale_list(scale: Integer;
                        out lp: PIntegers): Integer;
begin
  Result:=dmScaleList.Count;
  lp:=dmScaleList.First;

  if scale > 0 then
  while Result > 1 do begin
    if scale*2 >= lp[0] then Break;
    lp:=@lp[1]; Dec(Result);
  end
end;

function dm_Transit_loc(loc: int): int;
begin
  if loc > 100 then begin
    loc:=loc mod 100;
    if loc = 4 then loc:=1;
  end; Result:=loc
end;

{$ASMMODE INTEL}

procedure Unpack_ipoly(LPoly,WPoly: Pointer; Count: Integer);
asm
{$ifdef CPUX86_64}      // RDi,RSi,RDx

  push  RBx
  mov   RCx,RDx

  cld
  lodsd
  mov   EBx,EAx         // [X]
  lodsd
  mov   EDx,EAx         // [Y]

@loop:
  movsx EAx,WORD PTR [RSi]
  add   EBx,EAx
  add   RSi,2
  movsx EAx,WORD PTR [RSi]
  add   EDx,EAx
  add   RSi,2

  mov   DWORD PTR [RDi],EBx
  add   RDi,4
  mov   DWORD PTR [RDi],EDx
  add   RDi,4

  loop  @loop

  pop   RBx

{$else}
  push EBx
  push ESi
  push EDi

  cld
  mov  EDi,EAx
  mov  ESi,EDx

  lodsd
  mov  EBx,EAx
  lodsd
  mov  EDx,EAx

@loop:
  movsx EAx,WORD PTR [ESi]
  add   EBx,EAx
  add   ESi,2
  movsx EAx,WORD PTR [ESi]
  add   EDx,EAx
  add   ESi,2

  mov   DWORD PTR [EDi],EBx
  add   EDi,4
  mov   DWORD PTR [EDi],EDx
  add   EDi,4

  loop @loop

  pop  EDi
  pop  ESi
  pop  EBx
{$endif}
end;

function Unpack_mf(mf: PBytes; len: Integer;
                   lp: PLPoly; hp: PIntegers;
                   lp_Max,Flags: Integer): Integer;
var
  i,n, loc, z: Integer;
  wp: PIPoly; p: TPoint;
  hp_: PSmallInts;
begin
  Result:=-1;

  loc:=SizeOf(TPoint);
  if flags and fl_xyz <> 0 then
  loc:=SizeOf(VPoint);

  if flags and fl_long <> 0 then begin

    n:=len div loc;

    if n > 0 then

    if n <= lp_Max+1 then begin
      len:=n*SizeOf(TPoint);

      Move(mf^,lp^,len);
      mf:=@mf[len];

      len:=n*SizeOf(Integer);
      if Assigned(hp) then

      if flags and fl_xyz <> 0 then
        Move(mf^,hp^,len)
      else
        Fillchar(hp^,len,0);

      Result:=n-1
    end

  end else
  if len > loc then begin
    Dec(len,loc);

    loc:=SizeOf(IPoint);
    if flags and fl_xyz <> 0 then
    Inc(loc,SizeOf(SmallInt));

    n:=len div loc;

    if n > 0 then

    if n <= lp_Max+1 then begin

      Unpack_ipoly(lp,mf,n);
      Result:=n-1;

      len:=n*SizeOf(IPoint)+SizeOf(TPoint);
      mf:=@mf[len];

      if Assigned(hp) then

      if flags and fl_xyz <> 0 then begin

        z:=PInteger(mf)^; hp_:=@mf[4];

        for i:=0 to Result do begin
          z:=z + hp_[i]; hp[i]:=z
        end
      end
    end
  end
end;

constructor TDmwClickList.Create;
begin
  inherited Create;
  fData:=TCustomList.Create(Sizeof(TClickData),16)
end;

destructor TDmwClickList.Destroy;
begin
  fData.Free;
  inherited
end;

procedure TDmwClickList.Clear;
begin
  if not Locked then begin
    fData.Clear; inherited Clear
  end
end;

function TDmwClickList.xAdd(Ind: int; Ptr: Int64;
                            loc,dist,mf,lev: int): int;
var
  r: TClickData;
begin
  r.loc:=loc; r.dist:=dist;
  r.mf:=mf; r.lev:=lev;

  if Ind < 0 then begin
    fData.Add(@r);
    Result:=Add(Ptr)
  end
  else begin
    fData.Insert_range(@r,Ind,1);
    Result:=Insert(Ind,Ptr)
  end
end;

function TDmwClickList.xPop(Ind: int): bool;
begin
  Result:=Pop(Ind);
  if Result then begin
    fData.MoveTop(Ind)
  end
end;

procedure TDmwClickList.Pair(p1,p2: Int64; lock: bool);
begin
  Clear;
  xAdd(-1,p1,0,0,0,0);
  xAdd(-1,p2,0,0,0,0);
  Locked:=lock
end;

function TDmwClickList.IncLevel(ptr: int64; up: int): int;
var
  p: PClickData;
begin
  Result:=IndexOf(ptr);
  if Result >= 0 then begin

    p:=fData[Result];
    if Assigned(p) then
    Inc(p.lev,up);

    Changed;
  end
end;

function TDmwClickList.GetLevel(ptr: int64): int;
var
  i: int; p: PClickData;
begin
  Result:=-1;

  i:=IndexOf(ptr);
  if i >= 0 then begin
    p:=fData[i];
    if Assigned(p) then
    Result:=p.lev
  end
end;

constructor tdmNodeList.Create;
begin
  inherited Create(Sizeof(dm_Node),256)
end;

destructor tdm_cn.Destroy;
begin
  Tree.fe_backup; inherited
end;

constructor tdm_Dest.Create;
begin
  inherited Create(1);
  Info:=TInfoList.Create;
  fControl:=TIndexControl.Create;
  fTransit:=TIndexList.Create(false);
  fIndexing:=true;

  fLayers:=VPointList.Create(256);
end;

destructor tdm_Dest.Destroy;
begin
  xPut_int(Tree.Root,1000,fmax_NNNN);

  fLayers.Free;
  fTransit.Free;
  fControl.Free;

  Info.Free;

  inherited
end;

function tdm_Dest.Open_dst(Path: PChar): Boolean;
var
  id: Cardinal;
begin
  Result:=false; fLayers.Clear;

  if Open_Map(Path,true) then begin

    id:=Get_next_nnnn;
    fIndexing:=Tree.is_cn;

    if fIndexing then begin
      Offset_by_Id(0);
      id:=Max(id, fIndex.Max_id);
    end;

    fmax_NNNN:=id-1;
    Result:=true
  end
end;

procedure tdm_Dest.Set_max_NNNN(Value: Cardinal);
begin
  fmax_NNNN:=Max(fmax_NNNN,Value);

  if not fIndexing then begin
    fControl.max_NNNN:=fmax_NNNN;
    dm_Index_ctrl(Self,fControl)
  end
end;

function tdm_Dest.dst_Index(id,ptr: Cardinal): Cardinal;
begin
  if fIndexing then begin

    if id > fmax_NNNN then
      fmax_NNNN:=id
    else
    if fIndex.id_IndexOf(id) >= 0 then begin
      Inc(fmax_NNNN); id:=fmax_NNNN;
    end;

    fIndex.id_Add(id,ptr);

  end else
  if id > fmax_NNNN then
    fmax_NNNN:=id
  else
  if not fControl.Control(id) then begin
    Inc(fmax_NNNN); id:=fmax_NNNN;
  end;

  Result:=id
end;

function tdm_Dest.dst_seek_Layer(Code: int): bool;
var
  i,run: int; vp: PVPoly; v: VPoint;
begin
  Result:=false;

  vp:=fLayers.First;
  for i:=0 to fLayers.Count-1 do begin

    v:=vp[i];
    if v.z = Code then begin
      Tree.TopP:=v.x;
      Tree.RunP:=v.y;
      fLayer:=@vp[i];

      Result:=true; Break
    end
  end;

  if not Result then

  with Tree do
  if Ring_Down(Root) then begin
    run:=_Link(TopP); RunP:=run;

    while run <> TopP do begin
      if Code_Object(run) = Code then begin

        Ring_Down(run); Last_RunP;
        v.x:=TopP; v.y:=RunP; v.z:=Code;
        fLayers.Add(@v); fLayer:=fLayers.Last;

        Result:=true; Break
      end;

      run:=_Link(run)
    end
  end
end;

function tdm_Dest.dst_Object(src_dm: tdm_Map; p: Integer;
                             lp: PLLine; hp: PIntegers): Integer;
var
  dRec: dm_Rec; id,cl: Cardinal; x: TInt64;
begin
  Result:=0; Inc(dst_Count);

  src_dm.Get_Object(p,dRec);
  dRec.mind:=0; dRec.hind:=0;
  cl:=src_dm.Color_Object(p);

  Result:=Tree.Ins_Node(dRec,SizeOf(dm_Rec));

  if Result > 0 then begin

    if Assigned(fLayer) then
    if fLayer.x = Tree.TopP then
    fLayer.y:=Result;

    Update_cl(Result,cl);

    if Assigned(lp) then
    Update_xyz(Result,0,lp,hp);

    src_dm.Get_Info(p,Info);
    Info.x16:=hf_x16;

    if p = src_dm.Tree.Root then begin
      Info.xUpdate_int(999,1);
      Info.Delete_nn(808);
    end else
    if Info.xGet_int(1000,Integer(id)) then begin

      x.x:=id; id:=dst_Index(id,Result);
      if id <> x.id then Info.xUpdate_int(1000,id);

      if fIsTransit then begin
        x.cn:=id; fTransit.Add(x.x)
      end
    end;

    Update_Info(Result,Info);

    if dm_Transit_loc(dRec.Tag) > 20 then begin
      fe_bound(Result);
      Link_fe_cn(Result,true);
    end;

    Update_Cnt(1)
  end
end;

function tdm_Dest.dst_fc(fc: pcn_rec; lp: PLLine;
                         hf: TInfoList): Cardinal;
var
  x: TInt64;
begin
  x.id:=fc.rcid;

  fc.rcid:=dst_Index(fc.rcid,0); with fc^ do
  Result:=fc_add(RCNM,RCID,1,1, OBJL, lp,hf);

  x.cn:=fc.rcid; if fIsTransit then
  fTransit.Add(x.x)
end;

function cn_Id_str(cn,id: Integer): String;
var
  len: Integer;
begin
  Result:='';

  if (cn <> 0) or (id <> 0) then begin
    if cn = cn_object then Result:='DM' else
    if cn = cn_ident then Result:='FE' else
    if cn = cn_node then Result:='VC' else
    if cn = cn_edge then Result:='VE' else
    if cn = cn_aggr then Result:='FE';

    len:=5; if cn = cn_object then len:=8;

    if not is_str_anl then Result:=Result+'-';
    Result:=Result+xIdToStr(id,len,'0');
  end
end;

function Str_to_cn_Id(def_cn: Integer; const S: String): Int64;
var
  l,p,rc: Integer; x: TInt64;
  cn,id: String;
begin
  x.cn:=def_cn;
  val(S,x.id,rc);

  if rc <> 0 then begin

    x.id:=0; cn:=''; id:='';

    p:=System.Pos('-',S);
    l:=length(S);

    if p = 0 then begin
      if l > 2 then begin
        cn:=System.Copy(S,1,2);
        id:=System.Copy(S,3,l-2);
      end
    end else
    if (p > 1) and (p < l) then begin
      cn:=System.Copy(S,1,p-1);
      id:=System.Copy(S,p+1,l-p);
    end;

    if length(id) = 0 then
      x.x:=0
    else begin
      val(id,x.id,rc);
      if rc <> 0 then x.x:=0 else
      if cn = 'DM' then x.cn:=cn_object else
      if cn = 'FE' then x.cn:=cn_ident else
      if cn = 'VC' then x.cn:=cn_node else
      if cn = 'VE' then x.cn:=cn_edge else x.x:=0;
    end
  end;

  Result:=x.x
end;

function cn_ptr(rcid,rcnm,ornt,usag,mask: Integer): TPoint;
begin
  Result.x:=rcid;
  Result.y:=Long_bytes(rcnm,ornt,usag,mask)
end;

function fc_ptr(rcid,rcnm,rind: Integer): TPoint;
begin
  Result.x:=rcid;
  Result.y:=Long_bytes(rcnm,rind,0,0)
end;

procedure swap_ve_oref(lp: PLPoly; N: Integer);
var
  I,Ornt: Integer; p: TPoint;
begin
  Swap_LPoly(lp,nil,N);

  for I:=0 to N do begin
    p:=lp[I]; Ornt:=tlong(p.y).b[1];
    if Ornt = 2 then Ornt:=1 else Ornt:=2;
    tlong(p.y).b[1]:=Ornt; lp[I]:=p;
  end;
end;

procedure x_swap_ve_oref(lp: PLLine; lp_i: Integer);
begin
  with lp^ do
  swap_ve_oref(@Pol[lp_i],N-lp_i)
end;

function dm_mpp(ppm: Double; out mpp: Double): Integer;
var
  ppm_: Double;
begin
  Result:=0;

  if Abs(ppm-1000) < 1 then Result:=3 else
  if Abs(ppm-100) < 0.1 then Result:=2 else
  if Abs(ppm-10) < 0.01 then Result:=1;

  mpp:=100; ppm_:=dm_ppm[Result];

  if ppm > ppm_/100 then
  mpp:=ppm_/ppm
end;

function Alloc_LPolyBuf: PLLine;
begin
  Result:=Alloc_LLine(LPolyMax+1);
  if Assigned(Result) then Result.N:=-1
end;

function Alloc_ZPolyBuf(out lp_z: PIntegers): PLLine;
var
  max: Integer;
begin
  max:=LPolyMax+1; lp_z:=nil;
  Result:=Alloc_LLine(max + (max div 2));
  if Assigned(Result) then begin
    lp_z:=@Result.Pol[max];
    Result.N:=-1;
  end
end;

function is_Hole_Code(Code: Integer): Boolean;
begin
  Result:=(Code = 0) or (Code = Hole_Code)
end;

procedure Init_dm_Hdr(out hdr: dm_Hdr);
begin
  FillChar(hdr,SizeOf(dm_Hdr),0)
end;

function this_dm(Path: PChar): Boolean;
var
  h: Integer; size,Root: longint;
  mark: char4; len: word;
begin
  Result:=false;

  if This_Ext(Path,'.DM')
  or This_Ext(Path,'.TDM') then begin

    h:=FileOpen(StrPas(Path),fm_OpenRead);
    if h > 0 then begin

      size:=FileSeek(h,0,2);
      if size > 32+SizeOf(dm_Node) then begin
        FileSeek(h,16,0); FileRead(h,Root,4);
        FileSeek(h,24,0); FileRead(h,mark,4);

        if mark <> dmw_mark then

        if Root = 32 then begin
          FileSeek(h,Root,0); FileRead(h,len,2);
          if len = SizeOf(dm_Rec) then
          Result:=true
        end;
      end;

      FileClose(h)
    end
  end
end;

constructor tcn_list.Create;
begin
  inherited Create(0,0);
  fvm_Index:=TIndexList.Create(true);
  fvm_Index.Duplicates:=false;
  vm_ole:=true;
end;

destructor tcn_list.Destroy;
begin
  fvm_Index.Free;
  fvm_Index:=nil;
  inherited
end;

function tcn_list.Add_rec(rec: pcn_rec): Cardinal;
var
  ind,pos: Integer;
begin
  Result:=0;

  if rec.rcid = 0 then
  rec.rcid:=fvm_Index.Max_id;

  if fvm_Index.id_IndexOf(rec.rcid) >= 0 then
    Result:=rec.rcid
  else begin
    pos:=vm_Append(rec^,Sizeof(tcn_rec));

    if pos < vm_Ind then begin
      ind:=pos div Sizeof(tcn_rec);
      fvm_Index.id_Add(rec.rcid,ind);
      Result:=rec.rcid
    end
  end
end;

function tcn_list.Get_rec(I: Integer; rec: pcn_rec): Cardinal;
var
  p: TInt64; Ind: Integer;
begin
  Result:=0; p.x:=fvm_Index.Items[I];
  Ind:=p.c[1] * Sizeof(tcn_rec);

  if p.c[0] > 0 then
  if vmx_Load(Ind,rec^,SizeOf(tcn_rec)) then
  if rec.rcid = p.c[0] then Result:=p.c[0]
end;

procedure tcn_list.Reindex(I: Integer; Id: Cardinal);
var
  p: TInt64; Ind: Integer;
  rec: tcn_rec;
begin
  if Get_rec(I,@rec) > 0 then begin
    p.x:=fvm_Index.Items[I];

    p.c[0]:=Id; rec.rcid:=Id;

    fvm_Index.Items[I]:=p.x;

    Ind:=p.c[1] * Sizeof(tcn_rec);
    vm_Store(Ind,rec,Sizeof(rec))
  end
end;

function tcn_list.Return_item(Id: Cardinal): Boolean;
var
  Ind,Pos: Integer; v: tcn_rec;
begin
  Result:=false; Pos:=0;
  if vm_Index.id_IndexOf(Id) < 0 then
  while Pos+SizeOf(v) <= vm_Ind do begin
    Pos:=vm_Load(Pos,v,SizeOf(v));

    if v.rcid = Id then begin
      Ind:=Pos div SizeOf(v);
      vm_index.id_Add(Id,Ind-1);
      Result:=true; Break
    end
  end
end;

function tcn_list.Doctor(is_max: Boolean): Boolean;
var
  I,Ind,MaxInd: Integer; It1,It2: TInt64;
  P: PInt64s; v: tcn_rec;
begin
  Result:=false;

  MaxInd:=vm_Ind div Sizeof(v);

  if is_max then
    Result:=vm_Index.id_Clear(MaxInd-1,true)
  else begin
    Fillchar(v,Sizeof(v),0);

    I:=0; P:=vm_Index.Buffer;
    while I < vm_Index.Count do begin
      It2.x:=P[0]; v.rcid:=0;

      if It2.c[0] > 0 then
      if It2.c[1] < MaxInd then begin
        Ind:=It2.c[1] * Sizeof(v);
        if vmx_Load(Ind,v,Sizeof(v)) then
        if v.rcid <> It2.c[0] then v.rcid:=0
      end;

      if (I > 0) and (v.rcid > 0) then
      if It1.id = It2.id then
      v.rcid:=0;

      if v.rcid > 0 then begin
        Inc(I); P:=@P[1]; It1:=It2
      end else

      vm_Index.Delete(I)
    end;
  end;

  if vm_Edit then begin
    Ind:=vm_Index.Max_value;
    vm_Ind:=(Ind+1)  * Sizeof(v)
  end
end;

constructor tdm.Create(cash: Integer);
begin
  sf_:=TVirtMem.Create(cash,32);
  inherited Create(sf_,16);

  vc_:=tcn_list.Create;
  ve_:=tcn_list.Create;
  fc_:=tcn_list.Create;
  fe_:=TVirtMem.Create(0,0);

  fe_fc:=TIndexMem.Create(0,0);
  vc_hf:=TIndexMem.Create(0,0);
  ve_hf:=TIndexMem.Create(0,0);

  vc_index:=vc_.vm_Index;
  ve_index:=ve_.vm_Index;
  fc_index:=fc_.vm_Index;
end;

destructor tdm.Destroy;
begin
  vm_Close; vm_Virt:=nil;

  ve_hf.Free;
  vc_hf.Free;
  fe_fc.Free;

  fe_.Free; fc_.Free;
  ve_.Free; vc_.Free;

  sf_.Free; Free_mf_hf;

  xFreeLibrary(fdll_alx);
  xFreeLibrary(fdll_rel);
  xFreeLibrary(fdll_rd);

  inherited
end;

function tdm.Get_mf_x32: bool;
var
  mf: TVirtMem;
begin
  Result:=false; mf:=sf_;
  if Assigned(mf_) then mf:=mf_;
  if Assigned(mf) then
  Result:=mf.vm_x32
end;

function tdm.Get_Autosave: Boolean;
begin
  Result:=true;
  if Assigned(fe_stg) then
  Result:=false
end;

function tdm.Get_is_fe_temp: Boolean;
begin
  Result:=Assigned(fe_stg)
end;

procedure tdm.Set_is_fe_temp(Value: Boolean);
begin
  if Value then
  with TMemoryStorage.Create do
  GetInterface(IStorage,fe_stg);
end;

procedure tdm.Set_is_vc_temp(Value: Boolean);
begin
  if Value then
  with TMemoryStorage.Create do
  GetInterface(IStorage,vc_stg);
end;

procedure tdm.Set_is_ve_temp(Value: Boolean);
begin
  if Value then
  with TMemoryStorage.Create do
  GetInterface(IStorage,ve_stg);
end;

function tdm.Alloc_mf_hf: Boolean;
begin
  mf_:=TVirtMem.Create(1,0);
  hf_:=TVirtMem.Create(1,0);
  mf_.vm_Edit:=sf_.vm_Edit;
  hf_.vm_Edit:=sf_.vm_Edit;
  Result:=true
end;

procedure tdm.Free_mf_hf;
begin
  mf_.Free; mf_:=nil;
  hf_.Free; hf_:=nil;
end;

function tdm.vm_mem(out sf,mf,hf: TVirtMem): Boolean;
begin
  sf:=sf_; mf:=sf_; hf:=sf_;
  if Assigned(mf_) then mf:=mf_;
  if Assigned(hf_) then hf:=hf_;
  Result:=vm_Active
end;

function tdm.vm_Back(out sf,mf,hf: TVirtMem): Boolean;
begin
  Result:=vm_Open(vm_Path,sf.vm_Edit,sf,mf,hf)
end;

function tdm.Get_Active: Boolean;
begin
  if Assigned(fvm_dmw) then
    Result:=true
  else
    Result:=sf_.vm_Active
end;

procedure tdm.Init_mf_fmt(sf,mf: TVirtMem);
begin
  sf.vm_x32:=false;
  mf.vm_x32:=false;

  if Assigned(fdoc) then
  if xStorageFlags(fdoc) and $F = 5 then
  mf.vm_x32:=true;
end;

function tdm.Begin_cn: Boolean;
begin
  if not is_cn then is_cn:=true;
  Result:=is_cn
end;

function tdm.Get_is_cn: Boolean;
begin
  Result:=false;

  if vc_.vm_Active
  or ve_.vm_Active then

  Result:=true
end;

procedure tdm.Set_is_cn(Value: Boolean);
begin
  if Value and sf_.vm_Edit then
  if Assigned(fdoc) then begin

    if not vc_.vm_Active then

    if Assigned(vc_stg) then
      vc_.stm_Create(vc_stg,'x_vc')
    else
      vc_.stm_Create(fdoc,'x_vc');

    if not ve_.vm_Active then

    if Assigned(ve_stg) then
      ve_.stm_Create(ve_stg,'x_ve')
    else
      ve_.stm_Create(fdoc,'x_ve');

    if not fe_.vm_Active then begin

      if Assigned(fe_stg) then
        fe_.stm_Create(fe_stg,'x_fe')
      else
        fe_.stm_Create(fdoc,'x_fe');

      fe_.vm_Append(dmw_mark,4)
    end;

    if not fc_.vm_Active then
    fc_.stm_Create(fdoc,'x_fc')

  end
end;

function tdm.vm_This(APath: PChar): Boolean;
begin
  Result:=StrComp(APath,vm_Path) = 0
end;

function tdm.vm_Open(APath: PChar; rw: Boolean;
                     out sf,mf,hf: TVirtMem): Boolean;

function stm_dm(stm: IStream): Boolean;
var
  h: Integer; size,Root: longint;
  mark: char4; len: word;
begin
  Result:=false;
  size:=xSize(stm);

  if size > 32+SizeOf(dm_Node) then

  if xSeek(stm,16) then
  if xRead(stm,Root,4) then

  if xSeek(stm,24) then
  if xRead(stm,mark,4) then

  if mark <> dmw_mark then
  if Root = 32 then

  if xSeek(stm,Root) then
  if xRead(stm,len,2) then

  if len = SizeOf(dm_Rec) then
  Result:=true
end;

procedure Open_cn(Mode: Integer);
begin
  if vc_.stm_Open(fdoc,'x_vc',Mode) then
  vc_index.stm_LoadFrom(fdoc,'vc_index');

  if ve_.stm_Open(fdoc,'x_ve',Mode) then
  ve_index.stm_LoadFrom(fdoc,'ve_index');

  fe_.stm_Open(fdoc,'x_fe',Mode);

  if fc_.stm_Open(fdoc,'x_fc',Mode) then
  fc_index.stm_LoadFrom(fdoc,'fc_index');
end;

var
  mode: Integer;
  sf_stm,mf_stm,hf_stm: IStream;
begin
  Result:=false; vm_Close;
  fvm_fmt:=0; sf_.vm_Edit:=rw;
  fhf_x16:=false;

  if APath[0] <> #0 then
  if xStgIsStorageFile(APath) then begin

    mode:=ole_Read; if rw then
    mode:=ole_ReadWrite;

    if xOpenStorage(APath,mode, fdoc) = S_OK then
    if fdoc.OpenStream('dm',nil,Mode,0,sf_stm) = S_OK then begin
      sf_.vm_Assign(fdoc,sf_stm,false); Open_cn(Mode)
    end else

    if fdoc.OpenStream('sf',nil,Mode,0,sf_stm) = S_OK then begin

      if fdoc.OpenStream('mf',nil,Mode,0,mf_stm) = S_OK then
      if fdoc.OpenStream('hf',nil,Mode,0,hf_stm) = S_OK then

      if Alloc_mf_hf then begin
        sf_.vm_Assign(fdoc,sf_stm,false);
        mf_.vm_Assign(fdoc,mf_stm,false);
        hf_.vm_Assign(fdoc,hf_stm,false);

        fhf_x16:=xStorageFlags(fdoc) and $F0 = $10
      end;

      Open_cn(Mode);

    end

  end else

  if this_dm(APath) then
    sf_.vm_Open(APath);

  if vm_mem(sf,mf,hf) then begin
    Init_mf_fmt(sf,mf);
    StrLCopy(vm_Path,APath,255);
    Result:=true
  end
  else begin
    sf_stm:=nil;
    mf_stm:=nil;
    hf_stm:=nil;
    fdoc:=nil
  end
end;

function tdm.vm_Make(APath: PChar; fmt: int;
                     out sf,mf,hf: TVirtMem): Boolean;

var
  sf_stm,mf_stm,hf_stm: IStream;
  bits: int;
begin
  Result:=false; vm_Close;
  fvm_fmt:=0; sf_.vm_Edit:=true;
  fhf_x16:=false;

  case fmt of
0:  sf_.vm_Make(APath);

1:  if xCreateStorage(APath,fdoc) = S_OK then
    if fdoc.CreateStream('sf',ole_CREATE,0,0,sf_stm) = s_OK then
    if fdoc.CreateStream('mf',ole_CREATE,0,0,mf_stm) = s_OK then
    if fdoc.CreateStream('hf',ole_CREATE,0,0,hf_stm) = s_OK then

    if Alloc_mf_hf then begin
      sf_.vm_Assign(nil,sf_stm,true);

      mf_.vm_Assign(nil,mf_stm,true);
      mf_.vm_Append(dmw_mark,4);

      hf_.vm_Assign(nil,hf_stm,true);
      hf_.vm_Append(dmw_mark,4);

      if not fold_neva then begin
        bits:=5;

        if fhf_only_x16
        or Param_Option('/hf16') then begin
          bits:=bits or $10;
          fhf_x16:=true
        end;

        fdoc.SetStateBits(bits,$FF); // x32
      end
    end;

  end;

  if vm_mem(sf,mf,hf) then begin

    Init_mf_fmt(sf,mf); fvm_fmt:=fmt;

    StrCopy(vm_Path,'');

    if Assigned(APath) then
    StrLCopy(vm_Path,APath,255);

    Result:=true
  end;
end;

procedure tdm.vm_Close;
var
  rc: Integer;
begin
  if is_cn and sf_.vm_Edit then begin
    vc_index.stm_SaveAs(fdoc,'vc_index');
    ve_index.stm_SaveAs(fdoc,'ve_index');
    fc_index.stm_SaveAs(fdoc,'fc_index');
  end;

  vc_.vm_Close; vc_index.Clear;
  ve_.vm_Close; ve_index.Clear;
  fc_.vm_Close; fc_index.Clear;

  fe_.vm_Close; fe_stg:=nil;
  vc_stg:=nil; ve_stg:=nil;

  vc_hf.vm_Close;
  ve_hf.vm_Close;
  fe_fc.vm_Close;

  if Assigned(fvm_dmw) then begin
    fvm_dmw:=nil; if fdm_lib >= 32 then
    FreeLibrary(fdm_lib); fdm_lib:=0;
  end
  else begin
    if Assigned(mf_) then mf_.vm_Close;
    if Assigned(hf_) then hf_.vm_Close;
    sf_.vm_Close; fdoc:=nil
  end;

  Free_mf_hf;
end;

function tdm.Get_info(magic: PChar;
                      buf: Pointer;
                      len: Integer): Boolean;
begin
  Result:=false; if Assigned(fdoc) then
  Result:=xGetFromStream(fdoc,magic,buf^,len)
end;

procedure tdm.put_info(magic: PChar;
                       buf: Pointer;
                       len: Integer);
begin
  if Assigned(fdoc) and sf_.vm_Edit then
  xSaveAsStream(fdoc,magic,buf,len)
end;

function tdm.vm_ReadOnly(out sf,mf,hf: TVirtMem): Boolean;
begin
  if Strlen(vm_Path) > 0 then
  if not vm_Active or sf.vm_Edit then begin
    vm_Close; vm_Open(vm_Path,false,sf,mf,hf)
  end;

  Result:=vm_mem(sf,mf,hf)
end;

function tdm.vm_Update(out sf,mf,hf: TVirtMem): Boolean;
begin
  if Strlen(vm_Path) > 0 then
  if not vm_Active or not sf.vm_Edit then begin
    vm_Close; vm_Open(vm_Path,true,sf,mf,hf)
  end;

  Result:=vm_mem(sf,mf,hf)
end;

function tdm.vm_Recycle(out sf,mf,hf: TVirtMem): Boolean;
var
  tmp: TShortstr;
begin
  Result:=false; vm_mem(sf,mf,hf);

  if dm_Lib = 0 then
  if sf_.vm_Active then
  if Assigned(fdoc) then

  if xStrTempFileName(tmp,'dm') <> nil then
  if xCopyStorage(fdoc,tmp) then begin
    vm_Close; FileErase(vm_Path);
    xRenameFile(tmp,vm_Path);

    vm_Open(vm_Path,sf_.vm_Edit,sf,mf,hf);
    Result:=sf_.vm_Active
  end;

  FileErase(tmp);
end;

procedure tdm.vm_Rename(Path: PChar);
begin
  if dm_lib = 0 then begin
    vm_Close; FileErase(Path);
    if StrLen(vm_Path) > 0 then
    xRenameFile(vm_Path,Path)
  end
end;

function tdm.vm_FileName: String;
begin
  Result:=ExtractFileName(vm_Path)
end;

function tdm.Add_vc(vc: pcn_rec): Cardinal;
begin
  Result:=vc_.Add_rec(vc)
end;

function tdm.Add_ve(ve: pcn_rec): Cardinal;
begin
  Result:=ve_.Add_rec(ve)
end;

function tdm.Add_fc(fc: pcn_rec): Cardinal;
begin
  Result:=fc_.Add_rec(fc)
end;

function tdm.Get_vc_Max_id: Cardinal;
begin
  Result:=vc_index.Max_id;
end;

function tdm.Get_ve_Max_id: Cardinal;
begin
  Result:=ve_index.Max_id;
end;

function tdm.vc_IndexOf(Id: Cardinal): Integer;
begin
  Result:=vc_index.id_IndexOf(Id)
end;

function tdm.ve_IndexOf(Id: Cardinal): Integer;
begin
  Result:=ve_index.id_IndexOf(Id)
end;

function tdm.fc_IndexOf(Id: Cardinal): Integer;
begin
  Result:=fc_index.id_IndexOf(Id)
end;

function tdm.get_vc(Id: Cardinal; vc: pcn_rec): Boolean;
var
  Ind: Integer;
begin
  Result:=false;
  Ind:=vc_index.id_Value(Id) * Sizeof(tcn_rec);
  if vc_.vmx_Load(Ind,vc^,Sizeof(tcn_rec)) then
  Result:=vc.rcid = Id
end;

function tdm.get_ve(Id: Cardinal; ve: pcn_rec): Boolean;
var
  Ind: Integer;
begin
  Result:=false;
  Ind:=ve_index.id_Value(Id) * Sizeof(tcn_rec);
  if ve_.vmx_Load(Ind,ve^,Sizeof(tcn_rec)) then
  Result:=ve.rcid = Id
end;

function tdm.get_fc(Id: Cardinal; fc: pcn_rec): Boolean;
var
  Ind: Integer;
begin
  Result:=false;
  Ind:=fc_index.id_Value(Id) * Sizeof(tcn_rec);
  if fc_.vmx_Load(Ind,fc^,Sizeof(tcn_rec)) then
  Result:=fc.rcid = Id
end;

function tdm.get_vc_rec(I: Integer; vc: pcn_rec): Cardinal;
begin
  Result:=vc_.Get_rec(I,vc)
end;

function tdm.get_ve_rec(I: Integer; ve: pcn_rec): Cardinal;
begin
  Result:=ve_.Get_rec(I,ve)
end;

function tdm.get_fc_rec(I: Integer; fc: pcn_rec): Cardinal;
begin
  Result:=fc_.Get_rec(I,fc)
end;

procedure tdm.fc_reinfex(I: Integer; Id: Cardinal);
begin
  fc_.Reindex(I,Id);
end;

procedure tdm.Put_ve(Id: Cardinal; ve: pcn_rec);
var
  Ind: Integer;
begin
  Ind:=ve_index.id_Value(Id) * Sizeof(tcn_rec);
  ve_.vm_Store(Ind,ve^,Sizeof(tcn_rec));
end;

procedure tdm.Put_vc(Id: Cardinal; vc: pcn_rec);
var
  Ind: Integer;
begin
  Ind:=vc_index.id_Value(Id) * Sizeof(tcn_rec);
  vc_.vm_Store(Ind,vc^,Sizeof(tcn_rec));
end;

procedure tdm.Put_fc(Id: Cardinal; fc: pcn_rec);
var
  Ind: Integer;
begin
  Ind:=fc_index.id_Value(Id) * Sizeof(tcn_rec);
  fc_.vm_Store(Ind,fc^,Sizeof(tcn_rec));
end;

function tdm.Delete_vc(Id: Cardinal): Boolean;
begin
  Result:=vc_index.id_Delete(Id,0) > 0
end;

function tdm.Delete_ve(Id: Cardinal): Boolean;
begin
  Result:=ve_index.id_Delete(Id,0) > 0
end;

function tdm.Delete_fc(Id: Cardinal): Boolean;
begin
  Result:=fc_index.id_Delete(Id,0) > 0;
  if Result then fe_fc_Update(Id,nil,0,true)
end;

function tdm.Return_vc(Id: Cardinal): Boolean;
begin
  Result:=vc_.Return_item(Id)
end;

function tdm.Return_ve(Id: Cardinal): Boolean;
begin
  Result:=ve_.Return_item(Id)
end;

function tdm.fe_Update(Ptr,Id: Cardinal;
                       is_link: Boolean): Cardinal;
var
  fe: tfe_id; i,n,ind,len,size: Integer;
begin
  Result:=Ptr;

  if fe_.vm_Active then begin

    n:=0; if Ptr > 0 then begin
      len:=fe_.vm_LoadBuf(Ptr,fe,SizeOf(fe));
      n:=len div SizeOf(Integer);
    end;

    ind:=-1; if n > 0 then
    ind:=Int_Contains(@fe,n,Id);

    if is_link then begin
      if ind < 0 then
      if n < fe_max then begin
        fe[n]:=Id; Inc(n)
      end
    end else
    if ind >= 0 then begin
      Dec(n); for i:=ind to n-1 do
      fe[i]:=fe[i+1]
    end;

    if n = 0 then
      Result:=fe_.vm_FreeBuf(Ptr)
    else begin
      len:=int_Round(n,4) * SizeOf(Integer);
      size:=0; if Ptr > 0 then size:=fe_.vm_Word(Ptr);
      if len > size then Ptr:=fe_.vm_AllocBuf(len);

      len:=n * SizeOf(Integer);
      Result:=fe_.vm_UpDateBuf(Ptr,fe,len)
    end
  end
end;

procedure tdm.Link_fe_ve(ve_id,fe_id: Cardinal; is_link: Boolean);
var
  v: tcn_rec;
begin
  if fe_id <> 0 then
  if Get_ve(ve_id,@v) then begin
    v.fe:=fe_Update(v.fe,fe_id,is_link);
    Put_ve(ve_id,@v)
  end
end;

procedure tdm.Link_fe_vc(vc_id,fe_id: Cardinal; is_link: Boolean);
var
  v: tcn_rec;
begin
  if fe_id <> 0 then
  if Get_vc(vc_id,@v) then begin
    v.fe:=fe_Update(v.fe,fe_id,is_link);
    Put_vc(vc_id,@v)
  end
end;

function tdm.Get_vc_ref(Vc: Cardinal; buf: PIntegers;
                        BufMax: Integer): Integer;
var
  i: Integer; ve: tcn_rec;
begin
  Result:=0;

  if BufMax = 0 then BufMax:=fe_max;

  for i:=0 to ve_index.Count-1 do
  if get_ve_rec(I,@ve) > 0 then

  if (ve.vc1 = Vc) or (ve.vc2 = Vc) then

  if buf = nil then Inc(Result) else

  if Result < BufMax then begin
    buf[Result]:=ve.rcid; Inc(Result)
  end;
end;

function tdm.Get_vc_xref(Vc: Cardinal; List: TInt64List): Integer;
var
  i: Integer; ve: tcn_rec; p: TInt64;
begin
  List.Clear; p.cn:=cn_edge;

  for I:=0 to ve_index.Count-1 do
  if get_ve_rec(I,@ve) > 0 then
  if (ve.vc1 = Vc) or (ve.vc2 = Vc) then begin
    p.id:=ve.rcid; List.Add(p.x);
  end;

  Result:=List.Count
end;

function tdm.Get_fe(ve_Id,vc_Id: Cardinal;
                    fe: PIntegers; BufMax: Integer): Integer;
var
  v: tcn_rec; p,len: Integer;
begin
  Result:=0; p:=0;

  if ve_Id > 0 then begin
    if get_ve(ve_Id,@v) then

    if (vc_Id = 0)
    or (v.vc1 = vc_Id)
    or (v.vc2 = vc_Id) then

    p:=v.fe
  end else
  if vc_Id > 0 then begin
    if get_vc(vc_Id,@v) then p:=v.fe
  end;

  if BufMax = 0 then BufMax:=fe_Max;
  len:=BufMax * Sizeof(Integer);

  if p > 0 then begin
    len:=fe_.vm_LoadBuf(p,fe^,len);
    Result:=len div Sizeof(Integer)
  end
end;

procedure tdm.vc_Put_fe(Id: Cardinal;
                        fe: PIntegers; Count: Integer);
var
  v: tcn_rec; cx: Integer;
begin
  if not is_fe_temp then
  if Get_vc(Id,@v) then begin
    cx:=Count * SizeOf(Integer);
    if cx = 0 then v.fe:=0 else
    v.fe:=fe_.vm_UpDateBuf(v.fe,fe^,cx);
    Put_vc(Id,@v)
  end
end;

procedure tdm.ve_Put_fe(Id: Cardinal;
                        fe: PIntegers; Count: Integer);
var
  v: tcn_rec; cx: Integer;
begin
  if not is_fe_temp then
  if Get_ve(Id,@v) then begin
    cx:=Count * SizeOf(Integer);
    if cx = 0 then v.fe:=0 else
    v.fe:=fe_.vm_UpDateBuf(v.fe,fe^,cx);
    Put_ve(Id,@v)
  end
end;

function tdm.Add_fe(ve_Id,vc_Id: Cardinal;
                    List: TInt64List): Integer;
var
  i,n: Integer; fe: tfe_id; p: TInt64;
begin
  Result:=0;

  n:=Get_fe(ve_Id,vc_Id,@fe,0);
  for i:=0 to n-1 do begin
    p.cn:=cn_ident; p.id:=fe[i];
    List.Add(p.x); Inc(Result)
  end
end;

function tdm.ve_Contains_vc(ve_Id,vc_Id: Integer): Boolean;
var
  ve: tcn_rec;
begin
  Result:=false;
  if get_ve(ve_Id,@ve) then
  if ve.vc1 = vc_Id then Result:=true else
  if ve.vc2 = vc_Id then Result:=true
end;

function tdm.Is_free_node(vc_Id: Integer): Boolean;
var
  I,ve_Id: Integer;
begin
  Result:=true;

  for I:=0 to ve_index.Count-1 do begin
    ve_Id:=ve_index.Get_id(I);
    if ve_Contains_vc(ve_Id,vc_Id) then begin
      Result:=false; Break
    end
  end
end;

function tdm.vc_Lookup(const V: VPoint): Cardinal;
var
  I,N: Integer; vc: tcn_rec;
begin
  Result:=0; N:=vc_Index.Count;

  for I:=0 to N-1 do
  if Get_vc_rec(I,@vc) > 0 then
  if VPoints_Equal(V,vc.VPos) then begin
    Result:=vc.rcid; Break
  end
end;

procedure tdm.fe_backup;

procedure backup_vm(vm,dst: TVirtMem);
var
  ind,len,n: Integer; v: tcn_rec;
  fe: tfe_id;
begin
  ind:=0;
  while ind+SizeOf(v) <= vm.vm_Ind do begin
    vm.vm_Load(ind,v,SizeOf(v));

    if v.fe > 0 then begin
      len:=fe_.vm_LoadBuf(v.fe,fe,SizeOf(fe));
      n:=len div SizeOf(Integer);

      if n > 0 then begin
        len:=n * SizeOf(Integer);
        v.fe:=dst.vm_UpDateBuf(0,fe,len);
        vm.vm_Store(ind,v,SizeOf(v));
      end
    end;

    Inc(ind,SizeOf(v))
  end
end;

procedure used_vc(Id: Cardinal);
var
  v: tcn_rec;
begin
  if get_vc(Id,@v) then begin
    v.bits:=v.bits or fl_skip;
    put_vc(Id,@v)
  end
end;

var
  tmp: TVirtMem; index: TIndexList;
  i,id,pos: Integer; v: tcn_rec;
  only_vc: Boolean;
begin
  tmp:=TVirtMem.Create(0,0);
  index:=TIndexList.Create(false);
  try
    if sf_.vm_Edit then
    if Assigned(fdoc) then begin

      if Assigned(fe_stg) then begin
        tmp.stm_Create(fdoc,'x_fe');
        tmp.vm_Append(dmw_mark,4);

        backup_vm(vc_,tmp);
        backup_vm(ve_,tmp)
      end;

      if Assigned(vc_stg) then begin

        only_vc:=ve_stg = nil;

        for i:=0 to ve_index.Count-1 do begin
          id:=ve_index.Get_id(i);

          if get_ve(id,@v) then
          if only_vc or (v.fe > 0) then begin
            used_vc(v.vc1); used_vc(v.vc2);
          end
        end;

        if tmp.stm_Create(fdoc,'x_vc') then begin

          index.Clear;
          for i:=0 to vc_Index.Count-1 do begin
            id:=vc_Index.Get_id(i);

            if get_vc(id,@v) then
            if (v.bits and fl_skip <> 0)
            or (v.fe > 0) then begin
              v.bits:=v.bits and ($FF xor fl_skip);
              pos:=tmp.vm_Ind div SizeOf(v);
              tmp.vm_Append(v,Sizeof(v));
              Index.id_Add(id,pos)
            end
          end;

          vc_Index.Assign(Index)
        end;
      end;

      if Assigned(ve_stg) then
      if tmp.stm_Create(fdoc,'x_ve') then begin

        Index.Clear;
        for i:=0 to ve_index.Count-1 do begin
          id:=ve_index.Get_id(i);

          if get_ve(id,@v) then
          if v.fe > 0 then begin
            pos:=tmp.vm_Ind div SizeOf(v);
            tmp.vm_Append(v,Sizeof(v));
            index.id_Add(id,pos)
          end
        end;

        ve_Index.Assign(Index)
      end;

      fe_stg:=nil;
      vc_stg:=nil;
      ve_stg:=nil
    end;

  finally
    index.Free;
    tmp.Free
  end
end;

function tdm.fe_doctor(vc_fe,ve_fe: TIndexList): Boolean;

procedure fe_ref_backup(cn: tcn_list; cn_fe: TIndexList);
var
  v: tcn_rec; x: TInt64;
  Ind: Integer;
begin
  cn_fe.Clear; Ind:=0;
  while Ind+Sizeof(v) <= cn.vm_Ind do begin
    Ind:=cn.vm_Load(Ind,v,Sizeof(v));
    x.c[0]:=v.rcid; x.c[1]:=v.fe;
    cn_fe.Add(x.x)
  end
end;

procedure fe_ref_clear(vm: TVirtMem);
var
  v: tcn_rec; Ind: Integer;
begin
  Ind:=0;
  while Ind+Sizeof(v) <= vm.vm_Ind do begin
    vm.vm_Load(Ind,v,Sizeof(v)); v.fe:=0;
    Ind:=vm.vm_Store(Ind,v,Sizeof(v));
  end
end;

begin
  Result:=false;

  fe_ref_backup(vc_,vc_fe);
  fe_ref_backup(ve_,ve_fe);

  fe_.vm_Close;
  is_fe_temp:=true;
  is_cn:=true;

  if fe_.vm_Active then begin
    fe_ref_clear(vc_);
    fe_ref_clear(ve_);
    Result:=true
  end
end;

procedure tdm.fe_restore(vc_fe,ve_fe: TIndexList);

function verify(cn: tcn_list; upd: TVirtMem;
                cn_fe: TIndexList): Boolean;

function vm_Get_fe(vm: TVirtMem; Ptr: Integer;
                   var fe: tfe_id): Integer;
var
  len: Integer;
begin
  Result:=0;

  if Ptr > 0 then begin
    len:=fe_Max * Sizeof(Integer);
    len:=vm.vm_LoadBuf(Ptr,fe,len);
    Result:=len div Sizeof(Integer)
  end
end;

function fe_Compare(fe1: PIntegers; fe1_n: Integer;
                    fe2: PIntegers; fe2_n: Integer): Boolean;
var
  i: Integer;
begin
  Result:=fe1_n = fe2_n;

  if Result then

  if not CompareMem(fe1,fe2,fe1_n * 4) then

  for i:=0 to fe1_n-1 do
  if Int_Contains(fe2,fe2_n,fe1[i]) < 0 then
  begin Result:=false; Break end
end;

function fe_Restore(fe1: PIntegers; fe1_n: Integer;
                    fe2: PIntegers; fe2_n: Integer): Integer;
var
  i,j: Integer;
begin
  i:=0;
  while i < fe1_n do
  if Int_Contains(fe2,fe2_n,fe1[i]) < 0 then
    Inc(i)
  else begin
    Dec(fe1_n);
    for j:=i to fe1_n-1 do
    fe1[j]:=fe1[j+1]
  end;

  Result:=fe1_n
end;

var
  i,ind,fe1_n,fe2_n,len: Integer;
  v: tcn_rec; x: TInt64; fe1,fe2: tfe_id;
begin
  Result:=true; ind:=0;

  for i:=0 to cn_fe.Count-1 do
  if ind+SizeOf(v) <= cn.vm_Ind then begin

    ind:=cn.vm_Load(ind,v,SizeOf(v));
    x.x:=cn_fe.Items[i];

    if x.c[0] = v.rcid then begin
      fe1_n:=vm_Get_fe(fe_,v.fe,fe1);
      fe2_n:=vm_Get_fe(upd,x.c[1],fe2);

      if not fe_Compare(@fe1,fe1_n, @fe2,fe2_n) then
      begin
        if Assigned(fOn_fe_doctor) then begin
          fe1_n:=fe_Restore(@fe1,fe1_n,@fe2,fe2_n);

          if fe1_n > 0 then
          fOn_fe_doctor(v.rcnm,v.rcid,@fe1,fe1_n)
        end;

        Result:=false
      end
    end
    else begin
      x.c[1]:=0;
      cn_fe.Items[i]:=x.x;
      Result:=false
    end;
  end
end;

procedure restore(cn: tcn_list; upd: TVirtMem;
                  cn_fe: TIndexList);
var
  i,ind: Integer;
  v: tcn_rec; x: TInt64;
begin
  ind:=0;
  for i:=0 to cn_fe.Count-1 do
  if ind+SizeOf(v) <= cn.vm_Ind then begin
    cn.vm_Load(ind,v,SizeOf(v));
    x.x:=cn_fe.Items[i]; v.fe:=x.c[1];
    Ind:=cn.vm_Store(ind,v,Sizeof(v))
  end
end;

var
  upd: TVirtMem;
begin
  upd:=TVirtMem.Create(0,0);
  try
    if fe_.vm_Active then
    if Assigned(fe_stg) then

    if upd.stm_Open(fdoc,'x_fe',ole_ReadWrite) then

    if not verify(vc_,upd,vc_fe)
    or not verify(ve_,upd,ve_fe) then

      fe_.CopyTo(upd)

    else begin
      restore(vc_,upd,vc_fe);
      restore(ve_,upd,ve_fe)
    end;

  finally
    upd.Free
  end
end;

function tdm.cn_Doctor(is_max: Boolean): Boolean;
begin
  Result:=vc_.Doctor(is_max) or
          ve_.Doctor(is_max) or
          fc_.Doctor(is_max)
end;

procedure tdm.fe_fc_Update(fc: Cardinal;
                           lp: PLPoly; lp_n: Integer;
                           IsUpdate: Boolean);
var
  fe: TIntegers; i: Integer;
begin
  if lp_n < Vector_Max then begin

    if fe_fc.Open(fdoc,'fe_fc',true) then

    if lp = nil then
      fe_fc.fc_Update(nil,0,fc,IsUpdate)
    else begin
      for i:=0 to lp_n-1 do fe[i]:=lp[i].x;
      fe_fc.fc_Update(@fe,lp_n,fc,IsUpdate)
    end
  end
end;

procedure tdm.fe_fc_Exclude(fe: Cardinal);
begin
  if fe_fc.Open(fdoc,'fe_fc',true) then
  fe_fc.fe_Exclude(fe)
end;

function tdm.Get_fe_fc(fe_Id: Cardinal; out fc: tfe_id): Integer;
begin
  Result:=0; Fillchar(fc,Sizeof(fc),0);
  if fe_fc.Open(fdoc,'fe_fc',false) then
  Result:=fe_fc.Get_fc(@fc,fe_Max,fe_Id)
end;

procedure tdm.vc_update_attv(id,attv: Cardinal;
                             IsUpdate: Boolean);
begin
  if vc_hf.Open(fdoc,'vc_hf',true) then
  vc_hf.attv_update(id,attv,IsUpdate)
end;

procedure tdm.ve_update_attv(id,attv: Cardinal;
                             IsUpdate: Boolean);
begin
  if ve_hf.Open(fdoc,'ve_hf',true) then
  ve_hf.attv_update(id,attv,IsUpdate)
end;

function tdm.Get_vc_attv(id: Cardinal): Cardinal;
begin
  Result:=Cardinal(-1);
  if vc_hf.Open(fdoc,'vc_hf',false) then
  Result:=vc_hf.get_attv(id)
end;

function tdm.Get_ve_attv(id: Cardinal): Cardinal;
begin
  Result:=Cardinal(-1);
  if ve_hf.Open(fdoc,'ve_hf',false) then
  Result:=ve_hf.get_attv(id)
end;

constructor tdm_Copy.Create(Src,Dst: tdm_Map);
begin
  inherited Create(Src.Tree,Dst.Tree,SizeOf(dm_Rec));

  src_dm:=Src; dst_dm:=Dst;
  dst_dm.dm_AutoSave:=false;

  fhf:=TInfoList.Create
end;

destructor tdm_Copy.Destroy;
begin
  fhf.Free; inherited
end;

procedure tdm_Copy.Update_Info(Inp_P,Dst_P: longint);
var
  dRec: dm_Rec;
begin
  if src_dm.hf_x16 = dst_dm.hf_x16 then begin

    src_dm.Get_Object(Inp_P,dRec);

    with dRec do begin
      hind:=src_dm.hf.vm_CopyBuf(hind,0,dst_dm.hf);
      mind:=src_dm.mf.vm_CopyBuf(mind,0,dst_dm.mf);
    end;

    dst_dm.Put_Object(Dst_P,dRec)
  end
  else begin

    src_dm.Get_Object(Inp_P,dRec);

    with dRec do
    mind:=src_dm.mf.vm_CopyBuf(mind,0,dst_dm.mf);
    dRec.hind:=0;

    dst_dm.Put_Object(Dst_P,dRec);

    src_dm.Get_Info(Inp_P,fhf);
    dst_dm.Update_Info(Dst_P,fhf)
  end;

  if dRec.Tag > 20 then
  dst_dm.Link_fe_cn(Dst_P,true);
end;

constructor tdm_Map.Create(cash: Integer);
begin
  inherited Create;

  FTree:=Tdm.Create(cash);
  FTree.vm_mem(sf,mf,hf);

  Obj:=TFastObjects.Create;
  Idc:=TIdcStg.Create;

  fIndex:=TIndexList.Create(true);

  fintList1:=TIntegerList.Create;

  LG_T.Init; dm_scale:=0;
  dm_lt.X:=0; dm_rb.X:=99999; dm_ab.X:=10000;
  dm_lt.Y:=0; dm_rb.Y:=99999; dm_ab.Y:=10000;
  dm_ver:=0; g_ed:=0; LG_T.Init;

  fnn_4:=4; fnn_1:=1; fnn_7:=7; fnn_9:=9;

  fz_res:=100; dm_AutoSave:=true;

  pack_enabled:=true; fIsNotify:=true;

end;

destructor tdm_Map.Destroy;
begin
  fintList1.Free;

  fIndex.Free;

  Obj.Free;
  Idc.Free;

  fTree.Free;

  inherited
end;

function tdm_Map.Get_is_dm: Boolean;
begin
  Result:=FTree.dm_lib = 0
end;

function tdm_Map.Get_FileName: String;
begin
  Result:=xStrNameExt(Tree.vm_Path)
end;

function tdm_Map.Get_CanModify: Boolean;
begin
  Result:=false;
  if Enabled_Map then
  Result:=not dm_ReadOnly
end;

procedure tdm_Map.Set_EditMode(Value: Boolean);
begin
  if Value then Inc(fEditMode) else
  fEditMode:=Max(0,fEditMode-1)
end;

function tdm_Map.StrExcept(Str: PChar): int;
begin
  Result:=0; StrCopy(Str,'');
  case fExceptCode of
1:  StrCopy(Str,'ѕревышение размера метрики');
  end
end;

procedure tdm_Map.Close_Map;
begin
  if fEditMode <= 0 then begin
    Tree.vm_Close; Obj.vm_Close;
    if not fIsEditor then fIndex.Clear
  end
end;

function tdm_Map.doc_get_info(magic: PChar;
                              buf: Pointer; len: Integer): Boolean;
begin
  Result:=Tree.get_info(magic,buf,len)
end;

procedure tdm_Map.doc_put_info(magic: PChar;
                               buf: Pointer; len: Integer);
begin
  Tree.put_info(magic,buf,len)
end;

function tdm_Map.GetPolyMax: int;
const
  LPolyMax1 = 8000-1;
begin
  Result:=LPolyMax1;
  if mf_x32 then Result:=LPolyMax;
end;

procedure tdm_Map.Auto_Map(ok: Boolean);
var
  top,run: longint;
begin
  if fEditMode = 0 then

  if dm_AutoSave then
  if dm_Auto >= 0 then begin Inc(dm_Auto);
    if dm_Auto >= dm_Auto_max then ok:=true
  end;

  if ok and Tree.Autosave then begin
    top:=Tree.TopP; run:=Tree.RunP;
    Tree.vm_Close; dm_Backup; dm_Auto:=0;
    Tree.RunP:=run; Tree.TopP:=top;
  end             
end;

function tdm_Map.dm_Create(Path: PChar; fmt: int): Boolean;
begin
  Result:=Tree.vm_Make(Path, fmt, sf,mf,hf);
  fhf_x16:=Tree.hf_x16; dm_Autosave:=false
end;

function tdm_Map.clip_Open(Path: PChar): Boolean;
begin
  Result:=false;
  if Tree.vm_Open(Path,false, sf,mf,hf) then begin

    fhf_x16:=Tree.hf_x16;

    if Tree.Root > 0 then
    if Tree.Is_Childs(Tree.Root) > 0 then
    Result:=true
  end
end;              

function tdm_Map.dm_Objects_std_nn: Boolean;
begin
  Result:=false;
  fnn_4:=4; fnn_1:=1; fnn_7:=7; fnn_9:=9;

  if Open_Objects then begin
    fnn_4:=Obj.nn_4; fnn_1:=Obj.nn_1;
    fnn_7:=Obj.nn_7; fnn_9:=Obj.nn_9;
  end
end;

function tdm_Map.dm_Backup: Boolean;
begin
  Result:=Tree.vm_Back(sf,mf,hf)
end;

function tdm_Map.Open_Map(Path: PChar; rw: Boolean): Boolean;

function xGet_nn(nn: Integer): Integer;
begin
  if Obj.vm_Active then begin
    if nn = 4 then Result:=Obj.nn_4 else
    if nn = 1 then Result:=Obj.nn_1 else
    if nn = 7 then Result:=Obj.nn_7 else
    if nn = 9 then Result:=Obj.nn_9;
  end else

  Result:=Get_int(0,1000+nn);

  if Result = 0 then Result:=nn
end;

var
  obj_Active: Boolean;
  ed: Integer; fn: TShortStr;
begin
  Result:=false;

  dm_Pred:=SF.vm_Edit;

  Close_Map; dm_ver:=0;

  fIsStorage:=false;
  fIs_s57:=false;

  fmf_x32:=false;
  fhf_x16:=false;

  if not fIsEditor then fIndex.Clear;

  fnn_4:=4; fnn_1:=1; fnn_7:=7; fnn_9:=9;

  if Path <> nil then StrCopy(fn,Path)
  else StrCopy(fn,Tree.vm_Path);

  if StrLen(fn) > 0 then
  if Tree.vm_Open(fn,rw,sf,mf,hf) then begin

    fmf_x32:=mf.vm_x32;
    fhf_x16:=Tree.hf_x16;

    dm_Auto:=0; Update_dm_lt_rb;
    if not dm_doctor then Link_Map(false);

    obj_Active:=Obj.vm_Active; Open_Objects;

    fnn_4:=xGet_nn(4);
    fnn_1:=xGet_nn(1);
    fnn_7:=xGet_nn(7);
    fnn_9:=xGet_nn(9);

    if not obj_Active then
    Obj.vm_Close;

    fz_res:=100;

    if Tree.dm_lib = 0 then
    if Get_hf(0,990,_long,ed) then
    fz_res:=dm_ppm[ed];

    fobj_index:=Get_int(0,980);

    fIsStorage:=Assigned(Tree.doc);
    fIs_s57:=vc_Count + ve_Count > 0;

    Result:=true
  end;
end;

function tdm_Map.Return_Map(Path: PChar; rw: Boolean): Boolean;
begin
  Result:=false;

  if xStrThis(Path,Tree.vm_Path) then
    if rw then Result:=Update_Map
    else Result:=Enabled_Map
  else

  Result:=Open_Map(Path,rw)
end;

function tdm_Map.Enabled_Map: Boolean;
begin
  if Tree.vm_Active then
    Result:=true
  else begin
    SF.vm_Edit:=false;
    Result:=dm_Backup
  end
end;

function tdm_Map.Update_Map: Boolean;
begin
  if sf.vm_Active and sf.vm_Edit then
    Result:=true
  else
  if dm_ReadOnly then
    Result:=false
  else
    Result:=Tree.vm_Update(sf,mf,hf);
end;

function tdm_Map.Edit_Map: Boolean;
begin
  Result:=false;
  if Enabled_Map then
  Result:=not dm_ReadOnly
end;

function tdm_Map.Begin_Update: Boolean;
begin
  EditMode:=true;
  Result:=Update_Map;
end;

procedure tdm_Map.End_Update;
begin
  EditMode:=false; Close_Map;
end;

function tdm_Map.Doctor_Map: Boolean;

function xPut_nn(nn,val: Integer): Boolean;
begin
  Result:=false;
  if Get_int(0,1000+nn) <> val then begin
    if sf.vm_Active and sf.vm_Edit then
    xPut_Int(0,1000+nn,val); Result:=true
  end
end;

begin
  Result:=false;

  if Enabled_Map then
  if Tree.is_cn then
  Result:=Tree.cn_Doctor(true);

  if xPut_nn(4,fnn_4)
  or xPut_nn(1,fnn_1)
  or xPut_nn(7,fnn_7)
  or xPut_nn(9,fnn_9) then

  Result:=true
end;

function tdm_Map.Offset_by_Id(Id: Integer): Integer;
var
  maxId: longint;
begin
  Result:=0;

  if Id < 0 then fIndex.Clear;

  if fIndex.Count = 0 then begin

    maxId:=Get_Int(Tree.Root,1000);
    fIndex.IsSorted:=maxId < 100000;

    dm_Indexing(Self,fIndex,true);

    if not fIndex.IsSorted then
    fIndex.id_Sort;
  end;

  if Id > 0 then
  if fIndex.Count > 0 then
  Result:=fIndex.id_Value(Id)
end;

function tdm_Map.Ptr_by_Id(P: Int64): Int64;
var
  x: TInt64; pt: Integer;
begin
  x.x:=P;

  if x.cn = cn_ident then begin
    pt:=Offset_by_Id(x.id);
    if pt > 0 then begin
      x.cn:=cn_object; x.id:=pt
    end else
    if Tree.fc_IndexOf(x.id) >= 0 then
      x.cn:=cn_aggr
    else
      x.x:=0
  end;

  Result:=x.x
end;

function tdm_Map.x_Ptr_by_Id(P: Int64): Int64;
var
  lt,rb: TPoint;
begin
  Result:=0; P:=Ptr_by_Id(P);

  if P > 0 then
  if Rect_Object(P,lt,rb) then
  Result:=P
end;

function tdm_Map.GetParent(P: int64): int64;
begin
  Result:=-1;
  if __int64(P).cn = 0 then
  if Enabled_Map then begin
    P:=Tree._Parent(P);
    if P <> Tree.Root then
    Result:=P
  end
end;  

function tdm_Map.Get_childs(Ptr: int;
                            list1: TIntegerList;
                            list2: TInt64List): int;
var
  top,run: int;
begin
  if Assigned(list1) then list1.Clear;
  if Assigned(list2) then list2.Clear;

  if Enabled_Map then begin
    top:=Tree._Child(ptr);
    if top > 0 then begin
      run:=Tree._Link(top);

      while run <> top do begin
        if Assigned(list1) then list1.AddItem(run);
        if Assigned(list2) then list2.Add(run);
        run:=Tree._Link(run)
      end
    end
  end;

  if Assigned(list1) then
    Result:=list1.Count
  else
  if Assigned(list2) then
    Result:=list2.Count
end;

function tdm_Map.Get_dm_LRect(lp: PLPoly): Integer;
begin
  lp[0].X:=dm_lt.X; lp[0].Y:=dm_rb.Y;
  lp[1].X:=dm_lt.X; lp[1].Y:=dm_lt.Y;
  lp[2].X:=dm_rb.X; lp[2].Y:=dm_lt.Y;
  lp[3].X:=dm_rb.X; lp[3].Y:=dm_rb.Y;
  lp[4]:=lp[0]; Result:=4
end;

function tdm_Map.Get_dm_LOrient(lp: PLPoly): Integer;
begin
  Result:=Get_dm_LRect(lp);
  Middle_Point(dm_lt,dm_rb,lp[4])
end;

function tdm_Map.Get_dm_Bound(lp: PLLine; lp_Max: Integer): Integer;
begin
  lp.N:=-1;
  Get_Poly(Tree.Root,lp,lp_Max);
  if not PolyLock(lp) then lp.N:=-1;

  if lp.N < 0 then
  lp.N:=Get_dm_LRect(@lp.Pol);

  Result:=lp.N
end;

procedure tdm_Map.Get_dm_clip(Frame: PGPoly;
                              const sys: tsys;
                              out lt,rb: TPoint);
var
  i: Integer; l: LOrient; x: XGeoid;
begin
  x.s:=sys;

  for i:=0 to 3 do begin
    x.x:=Frame[i].x; x.y:=Frame[i].y;
    LG_T.x_R_to_L(x,l[i]);
  end;

  Max_Poly_Bound(@l,4,lt,rb)
end;

function tdm_Map.Get_lg_orient(out T: LG_Transit): Integer;
var
  link: xScanLink;
  i,k,nn,p: int; r: Double;
  hdr: dm_Hdr;
begin
  Result:=0; Init_LG_Transit(T);

  if Enabled_map then begin

    Get_dm_Hdr(hdr); dm_ver:=hdr.ver;

    if hdr.sys.pps = 1 then
    if hdr.sys.prj = 0 then
    hdr.sys.prj:=1;

    dm_scale:=hdr.dm_scale;

    if Tree.dm_lib > 0 then begin
      hdr.sys.pps:=Tree.vm_dmw.Get_Link(@T.l,@T.g);
      k:=4;
    end
    else begin
      nn:=901; if hdr.sys.pps = 1 then
      nn:=91; k:=0;

      link:=xScanLink.Create(Self);
      try
        repeat
          p:=link.Scan_tree(k = 0,true);
          if p = 0 then Break;

          T.l[k].x:=link.dmNode.dRec.ox1;
          T.l[k].y:=link.dmNode.dRec.oy1;

          Get_Gauss(p,nn,T.g[k]); Inc(k)
        until k = 4
      finally
        link.Free
      end;

      if k = 4 then begin
        r:=1; if hdr.sys.pps = 1 then r:=Small/10;
        if not Is_GBound(@t.l,1) then k:=0 else
        if not Is_GBound(@t.g,r) then k:=0
      end
    end;

    T.s:=hdr.sys;
    if T.s.pps = 1 then
    if T.s.prj <= 2 then
    if T.s.y0 = 500000 then
    T.s.y0:=0;

    // проверка перевода местных координат
    // в геодезические, если они с проекцией
    if k > 0 then with T.s do
    if (pps = 0) and (prj > 0) then
    if Abs(x0)+Abs(y0) > 1 then begin
      for i:=0 to k-1 do with T.g[i] do
      sys_XY_BL(x-x0,y-y0,T.s,x,y);
      pps:=1; x0:=0; y0:=0
    end;

    Result:=k
  end
end;

function tdm_Map.Get_LG_Transit(out T: LG_Transit): Boolean;
var
  i,iz,k,w,h: Integer;
  ed,lc,tmp: double; lt,rb: tgauss;
  b: LOrient; fr: GOrient;
begin
  Result:=false; g_ed:=0;
  lt_g.x:=0; lt_g.y:=0; rb_g.x:=0; rb_g.y:=0;

  k:=Get_lg_orient(T);

  if k in [2..3] then begin
    Get_dm_LRect(@b); for i:=0 to 3 do
    fr[i]:=_Gauss(b[i].X,b[i].Y);
    k:=Restore_lg_Transit(T,@fr,k);
  end;

  if k = 4 then begin

    Max_Gauss_Bound(@T.g,4, lt_g,rb_g);

    if t.s.pps = 0 then begin

      if t.s.prj = 0 then
      if t.s.lc <> 0 then
      with t.g[0] do sys_projection(t.s,x,y);

      if t.s.prj in [1..2] then
      if Abs(t.s.y0) <= 1 then begin

        lc:=t.s.lc;

        for i:=0 to 3 do begin
          iz:=Trunc(T.g[i].y / 1000000);
          if t.s.prj = 2 then Dec(iz,30);
          tmp:=gLongitude(iz * 1000000);

          if i = 0 then
            t.s.lc:=tmp
          else
          if Abs(tmp-lc)*180/Pi < 0.1 then begin
            t.s.lc:=lc; Break
          end
        end
      end;

      Result:=true
    end
    else begin
      lc:=0; if T.s.prj in prj_lc then
      lc:=prj_Longitude(T.s.lc,lt_g.y/2 + rb_g.y/2,T.s.prj);
      if not (T.s.prj in [10..prj_deg]) then
      T.s.lc:=lc; Result:=true
    end
  end else

  if Enabled_map then begin T.s:=sys_nil;

    ed:=1; w:=dm_ab.x; h:=dm_ab.y;
    while (w > 1000000) or (h > 1000000) do begin
      ed:=ed/10; w:=w div 10; h:=h div 10
    end;

    Get_dm_LRect(@b);
    for i:=0 to 4 do with b[i] do begin
      T.l[i].x:=X; T.g[i].x:=X*ed;
      T.l[i].y:=Y; T.g[i].y:=Y*ed;
    end
  end
end;

function tdm_Map.Update_dm_lt_rb: Boolean;
var
  i: int; lg: LG_Transit;
  dRec: dm_Rec; s,t: TPoint;
begin
  Result:=false;

  if Enabled_Map then begin
    s:=dm_ab; Get_Object(Tree.Root,dRec);
    dm_lt:=dRec.o_lt; dm_rb:=dRec.o_rb;

    if Get_LG_Transit(lg) then
    for i:=0 to 3 do begin
      with lg.l[i] do t:=_LGauss(x,y);
      Max_LPort(dm_lt,dm_rb,t);
    end;

    Mult_LRect(dm_lt,dm_rb, 8, dm_lt1,dm_rb1);

    dm_ab.X:=dm_rb.X-dm_lt.X;
    dm_ab.Y:=dm_rb.Y-dm_lt.Y;
    win_ab:=dm_ab;

    Result:=not Points_Equal(s,dm_ab)
  end
end;

function tdm_Map.dm_Resolution: double;
var
  len,f: double; a,b: TPoint;
begin
  Result:=LG_T.lg_res;

  with LG_T do if sys.prj = 4 then begin

    len:=Gauss_Dist(ag[0],ag[2]);
    if len > 1 then begin
      a.x:=Round(ag[0].x); a.y:=Round(ag[0].y);
      b.x:=Round(ag[2].x); b.y:=Round(ag[2].y);
      Result:=xLength(a,b, f)/len
    end

  end
end;

function tdm_Map.mmpp(is_mm: Boolean): double;
var
  res: double;
begin
  if is_mm then begin
    Result:=0; res:=dm_Resolution;
    if res > 0 then Result:=1/res
  end
  else Result:=dm_mm_k
end;

function tdm_Map.dm_mm_k: double;
var
  res: double;
begin
  Result:=0; res:=dm_Resolution;
  if res < 0.001 then res:=0.001;
  Result:=dm_scale/1000/res
end;

function tdm_Map.dm_ed: Integer;
var
  ppm: double;
begin
  Result:=0;
  ppm:=Round(Points_to_Metre*10)/10;

  if ppm >= 1000 then Result:=3 else
  if ppm >= 100 then  Result:=2 else
  if ppm >= 10 then   Result:=1
end;

function tdm_Map.dm_dist(r: Double; ed: Integer): Double;
var
  res: Double;
begin
  Result:=0;
  if ed = 1 then r:=r * dm_scale / 1000;
  res:=dm_Resolution; if res > 0 then
  Result:=Round(r / res);
end;

function tdm_Map.Align_grid(var p: TPoint): Boolean;
var
  b,l,db,dl: int; r,r1: tgeoid;
  s_open: Boolean;
begin
  Result:=false; db:=0; dl:=0;

  s_open:=sf.vm_Active;

  if LG_T.sys.pps = 1 then
  if Enabled_Map then begin
    db:=R_to_S(Get_Double(0,nn_grid_db));
    dl:=R_to_S(Get_Double(0,nn_grid_dl));
    if not s_open then Close_Map
  end;

  if (db > 0) and (dl > 0) then begin
    LG_T.L_to_R(p,r);
    b:=R_to_S(r.b); l:=R_to_S(r.l);

    if val_Align(b,db,8) then
    if val_Align(l,dl,8) then begin
      r.b:=b/3600/180*Pi;
      r.l:=l/3600/180*Pi;
      LG_T.R_to_L(r,p);
    end
  end
  else begin
    LG_T.L_to_R(p,r);

    b:=int_Round( R_to_S(r.b),5 );
    l:=int_Round( R_to_S(r.l),5 );

    r.b:=b/3600/180*Pi;
    r.l:=l/3600/180*Pi;
    LG_T.R_to_L(r,p)
  end
end;

function tdm_Map.chk_ppm(mm: Double): Double;
var
  g1,g2: TGauss; mm_ppm,d: Double;
  r1,r2: tgeoid; p1,p2: TPoint;
begin
  d:=0.01; if mm > 0 then
  if mm <= 1 then d:=mm;

  g1:=gauss_Centre(@LG_T.bg, 4);
  d:=d * dm_scale / 1000;
  g2:=g1; g2.y:=g2.y + d;

  LG_T.G_to_L(g1,p1);
  LG_T.G_to_L(g2,p2);
  mm_ppm:=Long_Dist(p1,p2);
  Result:=Max(0.7,mm_ppm);

  if mm < 0 then
  if LG_T.sys.pps = 1 then begin
    LG_T.XY_BL(g1,r1); r2:=r1;

    r1.l:=Round(r1.l * 180 * 10000000 / Pi);
    r2.l:=Round(r2.l * 180 * 10000000 / Pi)+1;

    r1.l:=r1.l / 10000000 / 180 * Pi;
    r2.l:=r2.l / 10000000 / 180 * Pi;

    LG_T.R_to_L(r1,p1);
    LG_T.R_to_L(r2,p2);

    Result:=Max(0.7,Long_Dist(p1,p2));
  end
end;

function tdm_Map.ms_unit: Integer;
var
  p1,p2: TPoint; g1,g2: TGeoid; d: Double;
begin
  Result:=1;
  
  Middle_Point(dm_lt,dm_rb,p1);
  p2:=Point(p1.X+1,p1.Y);
  LG_T.L_to_ms(p1,g1);
  LG_T.L_to_ms(p2,g2);

  d:=Hypot(g2.b-g1.b,g2.l-g1.l);
  if d < 1 then Result:=3 else
  if d < 10 then Result:=2
end;

function tdm_Map.StrNom(s: PChar): PChar;
var
  hdr: dm_Hdr;
begin
  Result:=nil; Get_dm_Hdr(hdr);
  Result:=StrPCopy(s,hdr.nom)
end;

function tdm_Map.Get_Stat(Stat: PIntegers;
                          StatMax: Integer): Integer;
var
  stm: IStream; n: Longint;
begin
  Result:=0;
  if Assigned(Tree.doc) then
  if xOpenStream(Tree.doc,'stat',false,stm) then begin

    n:=xSize(stm) div 4;
    if (n > 0) and (n <= StatMax) then
    if xRead(stm,Stat^,n*4) then
    Result:=n;

    stm:=nil
  end
end;

procedure tdm_Map.Put_Stat(Stat: PIntegers; Count: Integer);
begin
  if Assigned(Tree.doc) then
  xSaveAsStream(Tree.doc,'stat',Stat,Count*4)
end;

function tdm_Map.Points_to_Metre: double;
begin
  Result:=LG_T.lg_ppm
end;

function tdm_Map.Points_to_mm: double;
begin
  Result:=Points_to_Metre * dm_Scale / 1000
end;

function tdm_Map.dm_to_xyz(x,y,z: Integer): txyz;
var
  g: TGauss;
begin
  LG_T.L_to_Z(Point(x,y),g);
  Result.x:=g.x; Result.y:=g.y;
  Result.z:=z / z_res
end;

function tdm_Map.xyz_to_dm(x,y,z: Double): VPoint;
var
  p: TPoint;
begin
  LG_T.Z_to_L(_Gauss(x,y),p);
  Result.x:=p.X; Result.y:=p.Y;
  Result.z:=Round(z * z_res)
end;

function tdm_Map.dm_to_wgs(x,y,z: Integer): txyz;
var
  r: TGeoid; g: TGauss; p: TPoint; v: txyz;
begin
  p:=Point(x,y);
  LG_T.L_to_G(p,g); LG_T.XY_BL(g,r);
  v.x:=r.b; v.y:=r.l; v.z:=z / z_res;
  with LG_T.sys do if elp <> 9 then
  GEO_WGS(v.x,v.y,v.z, elp,@dat, v.x,v.y,v.z);
  Result:=v
end;

function tdm_Map.wgs_to_dm(x,y,z: Double): VPoint;
var
  g: TGauss; p: TPoint;
begin
  with LG_T.sys do if elp <> 9 then
  WGS_GEO(x,y,z, elp,@dat, x,y,z);
  LG_T.BL_XY(x,y,g); LG_T.G_to_L(g,p);
  Result.x:=p.X; Result.y:=p.Y;
  Result.z:=Round(z * z_res)
end;

function tdm_Map.trf_to_dm(const g: txyz; pps: Integer): VPoint;
var
  t: TGauss; v: lxyz;
begin
  t.x:=g.x; t.y:=g.y;

  if pps = 0 then
    LG_T.Z_to_L(t,v.p)
  else begin
    LG_T.WGS_XY(t.x,t.y,t);
    LG_T.G_to_L(t,v.p);
  end;

  v.v.z:=Round(g.z * z_res);
  Result:=v.v
end;

function tdm_Map.dm_to_trf(x,y,z: Integer; out v: txyz): Integer;
var
  p: TPoint; g: XGeoid; gz: Double;
begin
  p.X:=x; p.Y:=y;
  LG_T.x_L_to_R(p,g);
  gz:=z / z_res;

  if g.s.pps = 0 then
  if g.s.prj > 0 then begin
    prj_XY_BL(g.x,g.y,g.s,g.x,g.y);
    g.s.pps:=1
  end;

  if g.s.pps = 1 then
  with g.s do if elp <> 9 then
  GEO_WGS(g.x,g.y,gz,elp,@dat,g.x,g.y,gz);

  v:=_xyz(g.x,g.y,gz); Result:=g.s.pps
end;

function tdm_Map.Contains_Point(x,y: longint): Boolean;
begin
  Result:=PortContainsPoint(dm_lt,dm_rb, x,y)
end;

function tdm_Map.Contains_Gauss(x,y: double): Boolean;
var
  lt,rb, p: tgauss;
begin
  dm_Gauss_Bound(lt,rb); p.x:=x; p.y:=y;
  Result:=GaussContainsPoint(lt,rb, p)
end;

function tdm_Map.Contains_lp(lp: PLLine; loc: Integer): Boolean;
var
  i,lp_N: int; lt,rb: TPoint; buf: TLLine;
begin
  Result:=false;

  if loc in [21,22,23] then
    Result:=true
  else begin
    with lp^ do
    Max_Poly_Bound(@Pol,N, lt,rb);

    if PortContainsRect(dm_lt,dm_rb, lt.X,lt.Y,rb.X,rb.Y) then
    begin
      Result:=true;

      if Get_Poly(Tree.Root,@buf,LPoly_Max) > 0 then
      if PolyLock(@buf) then begin

        lp_N:=lp.N; if loc in [1,4] then lp_N:=0;

        Result:=false; for i:=0 to lp_N do
        if xPolyGonContainsPoint(@buf,lp.Pol[i]) >= 0 then
        begin Result:=true; Break end
      end
    end
  end
end;

function tdm_Map.xContains_Point(const p: TPoint): Boolean;
var
  buf: TLLine;
begin
  if Get_Poly(Tree.Root,@buf,LPoly_Max) > 0 then
    Result:=xPolyGonContainsPoint(@buf,p) >= 0
  else
    Result:=Contains_Point(p.x,p.y)
end;

function tdm_Map.Is_Clip_lp(lp: PLLine): Boolean;
var
  i,rc: int; p: TPoint; b: TLLine;
begin
  Result:=false;

  if Enabled_map then begin

    Get_Poly(Tree.Root,@b,LPoly_Max);
    if not PolyLock(@b) then

    b.N:=Bound_to_LPoly(dm_lt,dm_rb,@b.Pol);

    for i:=0 to lp.N do begin p:=lp.Pol[i];
      rc:=_rPolygonContainsPixel(@b.Pol,b.N,p.X,p.Y,nil);
      if rc < 0 then begin Result:=true; Break end
    end
  end
end;

function tdm_Map.Contains_Locator(lp: PLLine; lp_Max: Integer;
                                  const lt,rb: TPoint): Boolean;
var
  buf: TLLine; p: TPoint;
begin
  Result:=false;

  if lp = nil then begin
    lp:=@buf; lp_Max:=LPoly_Max;
  end;

  Middle_Point(lt,rb, p);

  if Get_dm_Bound(lp,lp_Max) > 0 then

  if xPolyGonContainsPoint(lp,p) >= 0 then
    Result:=true
  else
  if LocatePolyLine(@lp.Pol,lp.N, lt,rb, p) >= 0 then
    Result:=true
end;

function tdm_Map.Link_Map(edit: Boolean): Boolean;
var
  i,sc: Integer;
  lp: PLLine; r: tgeoid; g: tgauss;
  g_lc,ed: double; l: LOrient;
  t: LG_Transit; hdr: dm_Hdr;
begin
  g_ed:=0;
  Result:=Get_LG_Transit(t);

  LG_Tr:=t; LG_T.Init;
  LG_T.Assign(t,-1);

  ed:=dm_Resolution;
  if ed <= 0.01 then g_ed:=2 else
  if ed <= 0.1 then g_ed:=1;

  if edit then
  if not Result then
  if Update_Map then begin
    Get_dm_Hdr(hdr);

    if hdr.sys.pps = 1 then
    if hdr.sys.prj = 1 then
    if length(hdr.nom) > 0 then begin

      dm_Nom_Scan(hdr.nom,true, t.g, sc);

      Get_dm_LRect(@L);
      lp:=Alloc_LPolyBuf;

      if sc > 0 then
      if lp <> nil then
      if Get_Poly(Tree.Root,lp,LPolyMax) > 0 then
      if Poly_Vertex(lp,l[0],l[0]) >= 0 then
      if Poly_Vertex(lp,l[1],l[1]) >= 0 then
      if Poly_Vertex(lp,l[2],l[2]) >= 0 then
      if Poly_Vertex(lp,l[3],l[3]) >= 0 then

      if Link_Clear then begin
        g_lc:=xLongitude(@t.g); LG_T.sys.pps:=1;

        for i:=0 to 3 do begin
          r:=tgeoid(t.g[i]); with hdr.sys do
          BL_to_XY(r.b,r.l, g_lc,b1,b2, elp,prj, g.x,g.y);
          Link_Point(l[i].x,l[i].y, g,r)
        end;

        Result:=Link_Map(false)
      end;

      xFreePtr(lp)
    end
  end
end;

function tdm_Map.Link_update(backup: Boolean): Boolean;
var
  link: xScanLink; i: Integer;
  g: tgauss; r: TGeoid;
begin
  Result:=false;

  link:=xScanLink.Create(Self);
  try
    if link.Scan_childs(0) = 0 then begin

      with Tree do begin
        Ring_Down(Root); Ring_Down(_Link(TopP))
      end;

      with LG_T do for i:=0 to 3 do begin
        r.b:=0; r.l:=0; g:=bg[i]; if sys.pps = 1 then
        if backup then XY_BL(g,r) else begin
          r:=tgeoid(g); BL_XY(r.b,r.l,g)
        end;

        with ag[i] do
        Link_Point(Round(x),Round(y), g,r)
      end;

      Result:=true
    end;
  finally
    link.Free
  end
end;

function tdm_Map.Link_update_gauss: int;
var
  link: xScanLink; p: int; g: TGauss;
begin
  Result:=0;

  if Tree.dm_lib = 0 then
  if LG_T.sys.pps = 1 then
  if LG_T.sys.prj > 0 then begin
    link:=xScanLink.Create(Self);
    try
      while Result < 256 do begin
        p:=link.Scan_tree(Result = 0,true);
        if p = 0 then Break; Inc(Result);

        if Get_Gauss(p,91,g) then begin
          prj_BL_XY(g.x,g.y,LG_T.sys,g.x,g.y);
          xPut_Double(p,901,g.x);
          xPut_Double(p,902,g.y)
        end
      end
    finally
      link.Free
    end
  end
end;

function tdm_Map.Link_Point(x,y: Integer;
                            const g: tgauss;
                            const f: tgeoid): longint;
var
  hf: TInfoList;
  dRec: dm_Rec; cl: int;
  _g: tgauss;
begin
  hf:=TInfoList.Create;
  try
    hf.x16:=hf_x16;
    Obj.Obj_Color(0,1, cl); _g:=g;

    Fillchar(dRec,Sizeof(dRec),0);
    dRec.Tag:=1; dRec.Color:=cl;
    dRec.ox1:=x; dRec.oy1:=y;
    dRec.ox2:=x; dRec.oy2:=y;

    if LG_T.sys.pps = 1 then begin
      hf.Push_equ(91,_double,f.b);
      hf.Push_equ(92,_double,f.l);
      LG_T.z_BL_XY(f,_g);
    end;

    hf.Push_equ(901,_double,_g.x);
    hf.Push_equ(902,_double,_g.y);

    Result:=Ins_Object(dRec,nil,nil,hf);

    if Result > 0 then begin
      Get_Object(Tree.Root,dRec);
      dRec.ox1:=Min(dRec.ox1,x);
      dRec.oy1:=Min(dRec.oy1,y);
      dRec.ox2:=Max(dRec.ox2,x);
      dRec.oy2:=Max(dRec.oy2,y);
      Put_Object(Tree.Root,dRec);

      Set_Flag(Result,fl_del,true);
      Set_Flag(Result,fl_mf,true)
    end;
  finally
    hf.Free
  end
end;

function tdm_Map.x_Link_Point(x,y,cl: longint;
                              const g: xgeoid): longint;
var
  hf: TInfoList;
  dRec: dm_Rec; nn: int;
begin
  Result:=0;

  hf:=TInfoList.Create;
  try
    hf.x16:=hf_x16;

    nn:=901; if g.s.pps = 1 then nn:=91;
    hf.Push_equ(nn,_double,g.x);
    hf.Push_equ(nn+1,_double,g.y);

    FillChar(dRec,SizeOf(dm_Rec),0);
    dRec.Tag:=1; dRec.Color:=cl;
    dRec.ox1:=x; dRec.oy1:=y;
    dRec.ox2:=x; dRec.oy2:=y;

    Result:=Ins_Object(dRec,nil,nil,hf);

    Set_Flag(Result,fl_del,true);
    Set_Flag(Result,fl_mf,true)
  finally
    hf.Free
  end
end;

function tdm_Map.Link_Sign: Integer;
var
  link: xScanLink;
begin
  Result:=0;
  link:=xScanLink.Create(Self);
  if link.Scan_Childs(0) > 0 then
  Result:=link.dmNode.dRec.Color;
  link.Free;
end;

procedure tdm_Map.Link_To_Bound(update: Boolean);

function verify_bound(b: PLLine): bool;
var
  i: int; l: TLLine;
begin
  Result:=false;

  Get_Poly(Tree.Root,@l,LPoly_Max);
  if not PolyLock(@l) then l.N:=-1;

  if Polylock(@l) then

  if not PolyLock(b) then
    Result:=true
  else
  if l.N = b.N then begin

    i:=LLine_IndexOf(b,l.Pol[0],false);

    if i >= 0 then begin
      if i > 0 then
      Lock_Rotate(@l,nil,i);
                                   
      Result:=true; for i:=0 to l.N do
      if not Points_Equal(l.Pol[i],b.Pol[i]) then
      begin Result:=false; Break end
    end
  end
end;

var
  i,cnt,sc,ptr: int; p: TPoint;
  xy: tgauss; bl: tgeoid; G: GOrient;
  hdr: dm_Hdr; b: TLLine; s: TShortstr;
begin
  if Update_Map then begin

    b.N:=4;
    for i:=0 to 3 do begin
      b.Pol[i].x:=Round(LG_T.ag[i].x);
      b.Pol[i].y:=Round(LG_T.ag[i].y)
    end; b.Pol[4]:=b.Pol[0];

    if (LG_T.sys.pps = 0)
    or (LG_T.sys.prj in prj_tilted) then begin

      if not update then
      Update_mf(Tree.Root,2,@b)

    end
    else begin
      sc:=0; Get_dm_Hdr(hdr);

      if LG_T.sys.prj in [1..2] then
      if length(hdr.nom) > 0 then
      dm_Nom_Scan(hdr.nom,true, g,sc);

      if LG_T.Geoid_Port(lt_g,rb_g,sc,dm_scale,
                         @b,LPoly_Max,true) > 0 then

      if update then begin
        if not verify_bound(@b) then
        Update_mf(Tree.Root,2,@b)
      end
      else begin

        Update_mf(Tree.Root,2,@b);

        with Tree do begin Ring_Down(Root);
          Ring_Down(_Link(TopP)); Last_RunP
        end;

        if LG_T.sys.prj in [1..2] then begin
          Middle_Gauss(lt_g,rb_g,tgauss(bl));
          LG_T.BL_XY(bl.b,bl.l,xy); LG_T.G_to_L(xy,p);
          ptr:=Link_Point(p.x,p.y,xy,bl);

          if xGet_Str(0,901,s) <> nil then
          if Strlen(s) > 0 then xPut_Str(ptr,901,s)
        end
      end
    end
  end
end;

function tdm_Map.Link_Clear: Boolean;
var
  link: xScanLink;
  list: TIntegerList;
  i,p: Integer;
begin
  Result:=false;

  list:=fIntList1;
  list.Clear;

  link:=xScanLink.Create(Self);
  try
    p:=0;
    while true do begin
      p:=link.Scan_tree(p = 0,true);
      if p = 0 then Break;

      List.AddItem(p)
    end;
  finally
    link.Free
  end;

  for i:=0 to List.Count-1 do
  Del_Object(0,List[i]);

  with Tree do
  if Ring_Down(Root) then begin
    RunP:=_Link(TopP);
    if RunP <> TopP then
    Result:=Ring_Down(Tree.RunP)
  end;
end;

function tdm_Map.g_Width: double;
var
  p1,p2: TPoint; g1,g2: tgauss;
begin
  p1.x:=dm_lt.x; p1.y:=dm_lt.y; LG_T.L_to_G(p1,g1);
  p2.x:=dm_rb.x; p2.y:=dm_lt.y; LG_T.L_to_G(p2,g2);
  Result:=Gauss_Dist(g1,g2)
end;

function tdm_Map.g_Height: double;
var
  p1,p2: TPoint; g1,g2: tgauss;
begin
  p1.x:=dm_lt.x; p1.y:=dm_lt.y; LG_T.L_to_G(p1,g1);
  p2.x:=dm_lt.x; p2.y:=dm_rb.y; LG_T.L_to_G(p2,g2);
  Result:=Gauss_Dist(g1,g2)
end;

function tdm_Map.bl_Grid: Boolean;
var
  hdr: dm_Hdr;
begin
  Get_dm_Hdr(hdr);
  Result:=hdr.sys.pps = 1;

  if Result then
  if hdr.sys.prj in [1..2] then
  Result:=hdr.scale in [1..2]
end;

function tdm_Map.Open_Objects: Boolean;
var
  dir,nm,fn: TShortStr;
begin
  if not Obj.vm_Active then
  if Enabled_Map then begin
    Obj.Open_Obj(Objects_Path(fn,'.obj'));

    if not Obj.vm_Active then
    if StrDirectory(dir,fn) <> nil then
    if not dir_Exists(dir) then

    if StrNameExt(nm,fn) <> nil then
    if UsrDir(fn,'/obj/') <> nil then
    if StrCat(fn,nm) <> nil then

    Obj.Open_Obj(fn)
  end;

  Result:=Obj.vm_Active
end;

procedure tdm_Map.ObjectsEvent(Sender: TObject);
begin
  Open_Objects
end;

function tdm_Map.Open_Blanks(Blanks: TBlnBank): Boolean;
var
  fn: TShortStr;
begin
  Result:=Blanks.Open(Objects_Path(fn,'.bln'))
end;

function tdm_Map.Objects_Path(Path,Ext: PChar): PChar;
var
  hdr: dm_Hdr;
  dir,fn: TShortStr;
begin
  Result:=StrCopy(Path,'');

  if Enabled_Map then begin
    Get_dm_Hdr(hdr);

    StrPCopy(dir,ExtractFileDir(hdr.obj));
    StrPCopy(fn,ExtractFileName(hdr.obj));

    if not Dir_Exists(dir) then UsrDir(dir,'/obj');
    StrPath(Path,dir,fn); StrChangeExt(Path,Ext)
  end
end;

function tdm_Map.Objects_Name: string;
var
  hdr: dm_Hdr;
begin
  Get_dm_Hdr(hdr);
  Result:=ChangeFileExt(ExtractFileName(hdr.obj),'')
end;

function tdm_Map.Map_Caption(capt: PChar): string;
begin
  Result:=StrPas(capt)+' - '+Tree.vm_FileName
end;

function tdm_Map.Info_Path(dst,ext: PChar): PChar;
begin
  Result:=StrUpdateExt(dst,Tree.vm_Path,ext)
end;

function tdm_Map.Is_Objects: longint;
var
  wait: longint;
begin
  if Tree.is_cn then
    wait:=SF.vm_Ind div SizeOf(dm_Rec)
  else
    wait:=SF.vm_Ind div 64;

  if wait <= 0 then wait:=1;
  Result:=1; SF.vm_Load(28,Result,4);

  if Result < 0 then Result:=wait else
  if Result > wait then Result:=wait
end;

function tdm_Map.Update_Cnt(dn: Integer): Integer;
begin
  Result:=Is_Objects+dn;
  SF.vm_Store(28,Result,4)
end;

function tdm_Map.Get_layers(List: TPOintList): int;
var
  i,top,run: int; dRec: dm_Rec;
begin
  Result:=0;

  if Assigned(List) then List.Clear;

  if Tree.dm_lib = 0 then
  if Tree.Ring_Down(Tree.Root) then begin

    top:=Tree.TopP; run:=top;

    for i:=1 to 256 do begin
      run:=Tree._Link(run);
      if run <= top then Break;

      if Assigned(List) then begin
        Get_Object(run,dRec);
        List.AddItem(dRec.Code,dRec.View)
      end;

      Inc(Result)
    end
  end
end;

function tdm_Map.Empty_Layers_Exist: bool;
var
  i,top,run: int;
begin
  Result:=false;

  if Tree.dm_lib = 0 then
  if Tree.Ring_Down(Tree.Root) then begin

    Tree.CleanMode:=true;

    top:=Tree.TopP; run:=top;

    for i:=1 to 256 do begin
      run:=Tree._Link(run);
      if run <= top then Break;

      if Tree.Is_Childs(run) = 0 then begin
        Result:=true; Break
      end
    end;

    Tree.CleanMode:=false;
  end
end;

function tdm_Map.Clean_Layers: int;
var
  i,top,prd,run: int;
begin
  Result:=0;

  if Tree.dm_lib = 0 then
  if Tree.Ring_Down(Tree.Root) then begin

    Tree.CleanMode:=true;

    top:=Tree.TopP; prd:=top;

    for i:=1 to 256 do begin

      run:=Tree._Link(prd);
      if run <= top then Break;

      if Tree.Is_Childs(run) = 0 then
        xDel_Object(prd,run)
      else begin
        prd:=run; Inc(Result)
      end
    end;

    Tree.CleanMode:=false;
  end
end;

function tdm_Map.Seek_Layer(Ind: Integer): longint;
begin
  with Tree do
  Result:=_RunP(_Child(Root),Ind+1)
end;

function tdm_Map.Layer_by_Code(Code: Integer): Integer;
var
  p,p_code: longint;
begin
  Result:=0;

  if Tree.dm_lib = 0 then
  with Tree do
  if Ring_Down(Root) then begin

    RunP:=_Link(TopP);
    if RunP <> TopP then begin

      Result:=RunP; p:=Result;

      while p <> TopP do begin
        p_code:=Code_Object(p);
        if Layer_Contains(p_code,Code) then begin
          RunP:=p; Result:=p;
          if (p_code > 0) and (p_code = Code) then Break
        end;

        p:=_Link(p)
      end

    end
  end
end;

function tdm_Map.Locate_Layer(Code: Integer): Boolean;
begin
  Result:=false;
  if Layer_by_Code(Code) > 0 then
  with Tree do if Ring_Down(RunP) then
  Result:=Last_RunP > 0
end;

procedure tdm_Map.Undo_Object(p: Int64; cmd: Integer);
begin
  if Assigned(fOnUndoObject) then
  fOnUndoObject(P,cmd)
end;

function tdm_Map.Init_Ins_Object(Code,Loc: int): TInsObject;
var
  r: TInsObject; o: Obj_Rec; cl: tlong; layer,owner: int;
begin
  Fillchar(r,Sizeof(r),0);
  r.Code:=Code; r.Loc:=Loc;

  if Open_Objects then begin
    Obj.Obj_Index(Code,Loc, o,nil);

    cl.i:=0;
    cl.w[0]:=o.Color;
    cl.b[2]:=o.Pen;

    if (o.Tag = 0) and (Loc = 3) then
    cl.i:=7 shl 6;

    r.Color:=cl.i;
  end;

  if Update_map then begin

    owner:=o.Owner;
    Layer:=Layer_by_Code(Owner * 1000000);

    if Layer > 0 then begin
      Tree.Ring_Down(layer);
      Tree.Last_RunP;

      Tree.Get_Position(r.Tree);
    end
  end;

  Result:=r
end;

function tdm_Map.Auto_Object(var Ins: TInsObject;
                             lp: PLLine; hp: PIntegers;
                             Info: TInfoList): int;
var
  dRec: dm_Rec;
begin
  Result:=0;
  if Update_Map then begin

    if Ins.Tree.X = 0 then
    Ins:=Init_Ins_Object(Ins.Code,Ins.Loc);

    Fillchar(dRec,Sizeof(dRec),0);
    dRec.Code:=Ins.Code;
    dRec.Tag:=Ins.Loc;

    if Ins.Tree.X > 0 then
    Result:=Ins_Object(dRec,lp,hp,Info);
    
    if Result > 0 then begin
      Tree.Get_Position(Ins.Tree);
      Update_cl(Result,Ins.Color)
    end
  end
end;

function tdm_Map.Ins_Object(var dRec: dm_Rec;
                            lp: PLLine; hp: PIntegers;
                            Info: TInfoList): longint;
begin
  Result:=Tree.Ins_Node(dRec,SizeOf(dm_Rec));

  if Result = 0 then
  if Tree.vm_Recycle(sf,mf,hf) then
  Result:=Tree.Ins_Node(dRec,SizeOf(dm_Rec));

  if Result > 0 then begin
    if lp <> nil then Update_xyz(Result,0,lp,hp);
    if Assigned(Info) then Update_Info(Result,Info);
    if Get_Object(Result,dRec) <> 0 then
    Update_Cnt(1); Auto_Map(false)
  end
end;

function tdm_Map.Pred_Object(prd,run: longint): longint;
var
  cod1,cod2: Longint;
begin
  if prd > 0 then begin
    cod1:=Code_Object(prd) div 10000000;
    cod2:=Code_Object(run) div 10000000;

    if cod1 = cod2 then
      Result:=Tree._fPred(prd,run)
    else
      Result:=Tree._xPred(prd,run)
  end else

  Result:=Tree._Pred(run)
end;

function tdm_Map.xDel_Object(prd,run: longint): longint;
var
  nxt: longint; Node: dm_Node;
begin
  Result:=run;

  if run > 0 then begin
    Get_Node(run,Node);
    if Node.Flags and fl_skip = 0 then

    with Tree do begin

      if Node.Child = 0 then Update_Cnt(-1)
      else Update_Cnt(-Is_Childs(run)-1);

      if prd > 0 then
        prd:=_fPred(prd,run)
      else
        prd:=_Pred(run);

      if prd > 0 then begin
        EditMode:=true;
        Link_fe_cn(run,false);
        EditMode:=false;

        nxt:=Node.Link; NodeLink(prd,nxt);

        NodeLink(run,prd);
        Set_Flag(run,fl_skip,true);

        if run = RunP then begin
          RunP:=nxt; if RunP = TopP then RunP:=prd;
          if RunP = TopP then _Goto(_Child(TopP))
        end;

        Result:=prd
      end
    end
  end
end;

function tdm_Map.Del_Object(prd,run: longint): longint;
begin
  Result:=xDel_Object(prd,run);
  Result:=Tree.RunP
end;

procedure tdm_Map.delete_list(list: TIntegerList);
var
  i,prd: Integer; ip: PIntegers;
begin
  prd:=0; ip:=list.First;
  for i:=0 to list.Count-1 do
  prd:=Del_Object(prd,ip[i])
end;

function tdm_Map.ext_Del_Object(P: Int64): Boolean;
var
  prd,id: Integer; x,vc: TInt64; ve: tcn_rec;
begin
  Result:=false; x.x:=P;

  if Begin_Update then

  case x.cn of
cn_object:
    begin
      x.id:=0;
      if xGet_int(P,1000,id) then
      x.id:=id;

      if xDel_Object(0,P) <> P then
        Result:=true
      else
      if Get_Flags(P) and fl_skip <> 0 then begin
        prd:=Tree._Pred(P);
        if Tree._Link(prd) = P then begin
          Set_Flag(P,fl_skip,false);
          if xDel_Object(prd,P) <> P then
          Result:=true
        end
      end
    end;

cn_node:
    Result:=Tree.Delete_vc(x.id);

cn_edge:
    if Tree.get_ve(x.id,@ve) then
    if Tree.Delete_ve(x.id) then begin
      vc.cn:=cn_node; vc.id:=ve.vc1;
      Edit_Object(vc.x,vc.id,tx_edit_cn);
      vc.cn:=cn_node; vc.id:=ve.vc2;
      Edit_Object(vc.x,vc.id,tx_edit_cn);
      Result:=true
    end;

cn_aggr:
    Result:=Tree.Delete_fc(x.id);
  end;

  if Result then
  Edit_Object(P,x.id,tx_edit_del);

  EditMode:=false;
end;

function tdm_Map.Return_Object(p: Int64): Int64;

function link_object(prd,ptr: Integer): Integer;
begin
  Result:=0;

  if Tree.This_top(prd)
  or Ok_Object(prd) then begin
    Tree.Lnk_Node(prd,p);
    Set_Flag(p,fl_skip,false);
    Update_Cnt(1); Result:=p;

    if Tag_Object(Result) > 0 then
    Link_fe_cn(Result,true)
  end
end;

var
  x: TInt64; prd: Integer; dRec: dm_Rec;
begin
  Result:=0; x.x:=p;

  EditMode:=true;

  if x.cn = cn_object then begin

    Result:=link_object(Tree._Link(p),p);

    if Result = 0 then
    if Get_Object(p,dRec) > 0 then
    if Locate_Layer(dRec.Code) then
    Result:=link_object(Tree.RunP,p);

  end else

  if x.cn = cn_node then begin
    if Tree.Return_vc(x.Id) then
    Result:=p
  end else

  if x.cn = cn_edge then begin
    if Tree.Return_ve(x.Id) then
    Result:=p
  end;

  EditMode:=false
end;

function tdm_Map.lcut_Object(p: longint; lp: PLLine;
                             hp: PIntegers): longint;
var
  dRec: dm_Rec;
begin
  Result:=0;

  if Assigned(lp) then
  if Update_Map then begin

    EditMode:=true;

    FillChar(dRec,SizeOf(dRec),0);
    dRec.Code:=Code_Object(p);
    dRec.Tag:=Tag_Object(p);

    Result:=Tree.Ins_Node(dRec,SizeOf(dm_Rec));

    if Result > 0 then begin

      Update_xyz(Result,0,lp,hp);

      Copy_Info(Result,p,Self,true);

      Update_cl(Result,Color_Object(p));

      fe_bound(Result);
      Link_fe_cn(Result,true);

      EditMode:=false;
      Update_Cnt(1);
    end
  end
end;

function tdm_Map.Repeat_Object(p: int;
                               buf_lp: PLLine;
                               buf_hp: PIntegers): int;
var
  lp: PLLine; hp: PIntegers;
  dRec: dm_Rec;
begin
  Result:=0;

  lp:=buf_lp; hp:=buf_hp;

  if lp = nil then
  lp:=Alloc_ZPolyBuf(hp);

  if Assigned(lp) then
  if Update_Map then begin

    EditMode:=true;

    FillChar(dRec,SizeOf(dRec),0);
    dRec.Code:=Code_Object(p);
    dRec.Tag:=Tag_Object(p);

    Result:=Tree.Ins_Node(dRec,SizeOf(dm_Rec));

    if Result > 0 then begin

      if not z_axe_Exist(p) then hp:=nil;

      if xGet_Poly(p,lp,hp,LPolyMax) >= 0 then
      Update_xyz(Result,0,lp,hp);

      Copy_Info(Result,p,Self,true);

      Update_cl(Result,Color_Object(p));

      fe_bound(Result);
      Link_fe_cn(Result,true);

      Update_Cnt(1);
    end;

    EditMode:=false;
  end;

  if buf_lp = nil then
  xFreePtr(lp)
end;

function tdm_Map.Dup_Object(p: int;
                            buf_lp: PLLine;
                            buf_hp: PIntegers): int;
begin
  Result:=0;

  if Enabled_Map then
  if Tree._Goto(p) > 0 then begin
    Result:=Repeat_Object(p,buf_lp,buf_hp);
    if Result > 0 then Auto_Map(false)
  end
end;

function tdm_Map.Dup_Object1(P: int; lp: PLLine;
                             hp: PIntegers): longint;
var
  dRec: dm_Rec;
begin
  Result:=0;

  if Enabled_Map then
  if Tree._Goto(p) > 0 then

  if Update_Map then begin

    EditMode:=true;

    FillChar(dRec,SizeOf(dRec),0);
    dRec.Code:=Code_Object(p);
    dRec.Tag:=Tag_Object(p);

    Result:=Tree.Ins_Node(dRec,SizeOf(dm_Rec));

    if Result > 0 then begin

      xUpdate_mf(Result,0, lp,hp);

      Copy_Info(Result,p,Self,true);
      Update_cl(Result,Color_Object(p));

      fe_bound(Result);
      Link_fe_cn(Result,true);

      Update_Cnt(1);
    end;

    EditMode:=false;

    if Result > 0 then Auto_Map(false)
  end
end;

procedure tdm_Map.Owner_Object(p, Code,Owner: int);
begin
  if Owner > 0 then
  Code:=Owner * 1000000;

  if Update_Map then
  if Layer_by_Code(Code) > 0 then
  Tree.Dn_Node(p,Tree.RunP)
end;

function tdm_Map.Add_Layer(Code,Lev: Integer): longint;
var
  dRec: dm_Rec;
begin
  FillChar(dRec,SizeOf(dRec),0);
  dRec.Code:=Code; dRec.View:=Lev;
  Result:=Tree.Ins_Node(dRec,SizeOf(dm_Rec));

  if Result > 0 then 
  Put_Flags(Result,fl_del)
end;

function tdm_Map.Add_Object(Layer,Owner: int;
                            var dRec: dm_Rec;
                            lp: PLLine; hp: PIntegers): int;
begin
  if (Layer = 0) and (Owner >= 0) then
  Layer:=Layer_by_Code(Owner * 1000000);

  if Layer > 0 then begin
    Tree.Ring_Down(layer);
    Tree.Last_RunP
  end;

  Result:=Ins_Object(dRec,lp,hp,nil)
end;

function tdm_Map.Add_Hole(p: longint; lp: PLLine; hp: PIntegers): longint;
var
  dRec: dm_Rec;
begin
  Result:=0;
  if Update_Map then

  if Tag_Object(p) = 3 then

  if Tree.Ring_Down(p) then
  if Tree.Last_RunP > 0 then begin

    FillChar(dRec,SizeOf(dRec),0);

    dRec.Code:=Code_Object(p);
    dRec.Tag:=Tag_Object(p);

    Result:=Tree.Ins_Node(dRec,SizeOf(dm_Rec));

    if Result > 0 then begin
      Update_xyz(Result,0,lp,hp);
      Update_cl(Result,Color_Object(p));
      Update_Cnt(1); Auto_Map(false)
    end
  end
end;

function tdm_Map.Is_Hole(run,par: longint): Boolean;
var
  Code: Integer;
begin
  Result:=false;

  if par = 0 then
  par:=Tree._Parent(run);

  if par > 0 then
  if Tag_Object(par) = 3 then
  if Tag_Object(run) = 3 then begin
    Code:=Code_Object(run);

    if (Code = Code_Object(par))
    or Is_Hole_Code(Code) then

    Result:=true
  end
end;

function tdm_Map.z_axe_Exist(p: Int64): Boolean;
var
  x: TInt64; Node: dm_Node; v: tcn_rec;
begin
  Result:=false;

  if dm_ver <> 2 then
  if Tree.dm_lib = 0 then begin

    x.x:=p; Fillchar(v,SizeOf(v),0);

    if x.cn = cn_object then begin
      if Get_Node(p,Node) in [1..4,6,7,11] then
      if Node.dRec.mind > 0 then
      v.bits:=Node.Flags
    end else
    if x.cn = cn_node then
      Tree.get_vc(x.Id,@v)
    else
    if x.cn = cn_edge then
      Tree.get_ve(x.Id,@v);

    Result:=v.bits and fl_xyz <> 0
  end
end;

function tdm_Map.Get_zrange(ptr: Int64; clip: PLPoly;

                            lp: PLLine; hp: PIntegers;
                            mesh: TPolyMesh;

                            out zr: TRange): Boolean;
var
  i,k,h,h1,h2,loc: Integer;
  vp: PVPoly; p,lt,rb: TPoint;
  v,vlt,vrb: VPoint; z: Double;
begin
  Result:=false;

  h1:=0; h2:=0; k:=0;

  loc:=xTag_Object(ptr);

  if loc < 20 then begin

    if not z_axe_Exist(ptr) then
    if not xGet_Double(ptr,4,z) then hp:=nil else
    begin h1:=Round(z * z_res); h2:=h1 end;

    if Assigned(hp) or (h1 <= h2) then
    if xGet_Poly(ptr,lp,hp,LPolyMax) >= 0 then begin

      if Assigned(clip) then begin
        lt:=clip[0]; rb:=clip[1];
      end;

      if loc = 1 then lp.N:=0;

      for i:=0 to lp.N do begin p:=lp.Pol[i];

        if (clip = nil)

        or ((p.X >= lt.X) and (p.X <= rb.X) and
            (p.Y >= lt.Y) and (p.Y <= rb.Y)) then

        if hp = nil then
          Inc(k)
        else begin
          h:=hp[i];

          if k = 0 then begin
            h1:=h; h2:=h; k:=1
          end
          else begin
            if h < h1 then h1:=h;
            if h > h2 then h2:=h;
            Inc(k)
          end
        end
      end
    end
  end else

  if loc = 33 then begin

    if Assigned(mesh) then
    if Get_mesh(ptr,mesh) then

    if clip = nil then begin
      mesh.Points.Get_min_max(vlt,vrb);
      h1:=vlt.z; h2:=vrb.z;
      k:=mesh.Points.Count;
    end
    else begin
      vp:=mesh.Points.First;
      for i:=0 to mesh.Points.Count-1 do begin
        v:=vp[i]; h:=v.z;

        if (v.x >= lt.X) and (v.x <= rb.X) then
        if (v.y >= lt.Y) and (v.y <= rb.Y) then

        if k = 0 then begin
          h1:=h; h2:=h; k:=1
        end
        else begin
          if h < h1 then h1:=h;
          if h > h2 then h2:=h;
          Inc(k)
        end
      end
    end
  end;

  if k > 0 then
  if (h1 <> 0) or (h1 < h2) then begin
    zr.min:=h1 / z_res;
    zr.max:=h2 / z_res;
    Result:=true
  end
end;

procedure tdm_Map.hf_Trunc_z_axe(p: longint);
var
  h: Double;
begin
  if xGet_Height(p,h) then
  if Get_Flags(p) and fl_xyz <> 0 then
  Trunc_z_axe(p);
end;

procedure tdm_Map.Trunc_z_axe(p: longint);
var
  Node: dm_Node; len,loc,xyz: Integer;
begin
  if dm_ver <> 2 then
  if Tree.dm_lib = 0 then

  if Get_Node(p,Node) in [1..4] then
  if Node.Flags and fl_xyz <> 0 then begin

    with Node.dRec do
    if mind > 0 then begin
      if Tag = 1 then Node.Flags:=fl_long;
      len:=mf.vmx_BufLen(mind);

      if Node.Flags and fl_long <> 0 then
        len:=(len div 12) * 8
      else begin
        len:=len - 12;
        len:=(len div 6) * 4;
        len:=len + 8
      end;

      mind:=mf.vmx_TruncBuf(mind,len)
    end;

    Set_Flag(p,fl_xyz,false);
  end
end;

function tdm_Map.Get_Poly_Cnt(p: Int64): Integer;
var
  len,loc,xyz,typ: int; Node: dm_Node;
  x: TInt64; v: tcn_rec;
begin
  Result:=-1; x.x:=p;

  if Tree.dm_lib > 0 then
    Result:=Tree.vm_dmw.Get_Poly_Cnt(p)
  else

  if x.cn = cn_object then begin

    typ:=dm_Transit_loc(Get_Node(p,Node));

    len:=0;
    with Node.dRec do if mind > 0 then
    len:=mf.vmx_BufLen(mind);

    case typ of

  1:  begin
        if len > 0 then begin
          loc:=8;
          if Node.Flags and fl_xyz <> 0 then
          loc:=12;

          Result:=(len div loc)-1
        end;

        if Result <> 3 then begin
          Result:=0; with Node.dRec do
          if (ox1 <> ox2) or (oy1 <> oy2) then
          Result:=1
        end
      end;

  2,
  3,
  4,
  11: if len > 0 then begin xyz:=2;

        if dm_ver = 2 then Node.Flags:=fl_long else
        Inc(xyz, ord(Node.Flags and fl_xyz <> 0));

        if Node.Flags and fl_long <> 0 then
          loc:=4
        else begin
          Dec(len,xyz*4);
          loc:=2
        end;

        Result:=pred(len div (loc*xyz))
      end else

      if Node.dRec.Tag = 3 then Result:=4;

  5,
  8:  if len > 0 then begin
        Result:=(len div 8)-1;
        if Result > 2 then Result:=2
      end;
  6,
  7,

  21,
  22,
  23: if len > 0 then
      Result:=(len div 8)-1;
    end

  end else
  if x.cn = cn_node then begin
    if Tree.get_vc(x.Id,@v) then
    Result:=0
  end else
  if x.cn = cn_edge then
    Result:=Get_ve_Poly(x.Id,nil,nil,LPolyMax)
end;

function tdm_Map.Get_Poly_Count(p: Int64): Integer;
var
  code,up_code, top,run: longint;
begin
  Result:=Get_Poly_Cnt(p)+1;

  if __int64(p).cn = 0 then
  if Tag_Object(p) = 3 then begin
    top:=Tree._Child(p);
    up_code:=Code_Object(p);

    if top > 0 then begin
      run:=Tree._Link(top);

      if run > 0 then
      while run <> top do begin
        if run = 0 then Break;

        if Tag_Object(run) = 3 then begin
          code:=Code_Object(run);
          if code = up_code then code:=0;

          if is_Hole_Code(code) then
          Inc(Result,Get_Poly_Cnt(run)+1);
        end;

        run:=Tree._Link(run)
      end
    end
  end
end;

function tdm_Map.Draw_HF(p, n: Integer; id: Id_Tag;
                         ansi: int; s: PWideChar): PWideChar;
var
  t: string;
begin
  Result:=nil;

  if id < _unicode then
  if Get_HF(p, n,id, t) then

  if id <> _string then
    Result:=StringToWideChar(IdToStr(id,@t),s,255)
  else

  if ansi = 0 then
    DosToUnicodep(t,s)
  else begin
    if ansi = rus_unicode then
    t:=WinString(t);

    xStringToWideChar(t,s)
  end
end;

function tdm_Map.Hint_HF(p, n: int; s: PChar; len: int): bool;
var
  stm: thfStream; dRec: dm_Rec;
begin
  Result:=false;

  if Tree.dm_lib = 0 then begin

    if p = 0 then p:=Tree.Root;
    Get_Object(p,dRec);

    if dRec.hind > 0 then begin

      stm:=TInfoStream.Create(hf_x16);
      try
        if hf.Stream_Copy(dRec.hind,stm) > 0 then begin
          if len = 0 then len:=255;
          Result:=Get_hint(stm,n,s,len);
        end
      finally
        stm.Free
      end

    end
  end
end;

function tdm_Map.xWide_Text(p: longint): Boolean;
var
  stm: thfStream; dRec: dm_Rec;
  id: Id_Tag; info:  TString;
begin
  Result:=false;

  if p > 0 then
  if Tree.dm_lib = 0 then begin

    Get_Object(p,dRec);
    if dRec.hind > 0 then begin
      stm:=thfStream.Create(hf_x16);
      try
        if hf.Stream_Copy(dRec.hind,stm) > 0 then
        if Get_equ_nn(stm,fnn_9,id,info) then
        Result:=id = _unicode
      finally
        stm.Free
      end
    end

  end;
end;

function tdm_Map.xDraw_Text(ptr,ansi: int; s: PWideChar): PWideChar;
var
  stm: thfStream; o: dm_Rec;
begin
  Result:=nil;

  if ptr > 0 then
  if Tree.dm_lib = 0 then begin

    Get_Object(ptr,o);
    if o.hind > 0 then begin

      stm:=thfStream.Create(hf_x16);
      try
        if hf.Stream_Copy(o.hind,stm) > 0 then
        if Get_equ(stm, fnn_9,_unicode,ansi, s^) then
        Result:=s
      finally
        stm.Free
      end
    end

  end;

  if Result = nil then
  Result:=StringToWideChar('',s,255);
end;

function tdm_Map.xLoad_str(p,n: longint; s: PChar): PChar;
var
  stm: TInfoStream; t: string;
begin
  t:='';

  if p > 0 then begin

    stm:=TInfoStream.Create(hf_x16);
    try
      if Load_hf(p,stm) > 0 then
      stm.Get_HF(n,_string,t)
    finally
      stm.Free
    end

  end;

  Result:=StrPCopy(s,t)
end;

function tdm_Map.xLoad_Ansi(p: longint; s: PChar): PChar;
begin
  Result:=xLoad_str(p,9, s)
end;

procedure tdm_Map.Fill_hp(p: Integer; hp: PIntegers; N: Integer);
var
  z: Double; i,h: Integer;
begin
  if Assigned(hp) then

  if xGet_Relief(p,z) then begin
    h:=Round(z*z_res);
    for i:=0 to N do hp[i]:=h
  end else

  FillChar(hp^,(N+1)*SizeOf(Integer),0)
end;

function tdm_Map.Load_Height(hp: PIntegers;
                             Ind,Count,Flags: Integer): Integer;
var
  loc: Integer;
begin
  Result:=-1;

  if Assigned(hp) then
  if Flags and fl_xyz <> 0 then begin

    if Flags and fl_long <> 0 then
      loc:=SizeOf(TPoint)
    else begin
      Inc(ind,SizeOf(TPoint));
      loc:=SizeOf(IPoint)
    end;

    ind:=mf.vmx_Buffer(ind) + Count * loc;
    mf.vm_Load(ind,hp^,SizeOf(Integer));
    Result:=0
  end
end;

function tdm_Map.Bound_Poly(ind,Flags: longint;
                            out lt,rb: VPoint): Integer;
var
  n, len,loc,alt, z,z1,z2: int;
  p,p1,p2: TPoint;
begin
  Result:=-1; lt:=_VPoint(0,0,0); rb:=lt;

  ind:=mf.vmx_Block(ind,len);

  loc:=SizeOf(TPoint);
  if flags and fl_xyz <> 0 then
  loc:=SizeOf(VPoint);

  if flags and fl_long <> 0 then begin

    n:=len div loc;
    if n > 0 then begin
      Ind:=mf.min_max_xy(ind,n, p1,p2);

      if flags and fl_xyz <> 0 then
      mf.min_max_long(ind,n, z1,z2);

      Result:=n-1
    end

  end
  else begin
    alt:=loc; loc:=SizeOf(IPoint);
    if flags and fl_xyz <> 0 then
    Inc(loc,SizeOf(SmallInt));

    n:=(len - alt) div loc;

    if n > 0 then begin

      ind:=mf.vm_Load(ind,p,SizeOf(p));
      ind:=mf.min_max_dxy(ind,n, p1,p2);
      Inc(p1.x,p.x); Inc(p1.y,p.y);
      Inc(p2.x,p.x); Inc(p2.y,p.y);

      if flags and fl_xyz <> 0 then begin
        ind:=mf.vm_Load(ind,z,SizeOf(z));
        mf.min_max_inc(ind,n, z1,z2);
        Inc(z1,z); Inc(z2,z)
      end;

      Result:=n-1
    end
  end;

  if Result >= 0 then begin
    lt:=_VPoint(p1.x,p1.y,z1);
    rb:=_VPoint(p2.x,p2.y,z2)
  end
end;

function tdm_Map.Load_Poly(mind: longint;
                           lp: PLPoly; hp: PIntegers;
                           lp_Max,Flags: Integer): Integer;
var
  wp: PIPoly; hp_: PSmallInts;
  i,n, len,loc, z, si: int; p: TPoint;
begin
  Result:=-1;

  si:=mf.vmx_Block(mind,len);

  loc:=SizeOf(TPoint);
  if flags and fl_xyz <> 0 then
  loc:=SizeOf(VPoint);

  if flags and fl_long <> 0 then begin

    n:=len div loc;

    if n > 0 then

    if n <= lp_Max+1 then begin
      len:=n*SizeOf(TPoint);

      if Assigned(lp) then
        si:=mf.vm_Load(si,lp^,len)
      else
        Inc(si,len);

      len:=n*SizeOf(Integer);

      if Assigned(hp) then

      if flags and fl_xyz <> 0 then
        mf.vm_Load(si,hp^,len)
      else
        Fillchar(hp^,len,0);

      Result:=n-1
    end else

    Result:=Load_Height(hp,mind,n,flags)

  end else
  if len > loc then begin
    Dec(len,loc);

    loc:=SizeOf(IPoint);
    if flags and fl_xyz <> 0 then
    Inc(loc,SizeOf(SmallInt));

    n:=len div loc;

    if n > 0 then

    if n <= lp_Max+1 then begin

      Result:=n-1;
      len:=n*SizeOf(IPoint)+SizeOf(TPoint);

      if Assigned(lp) then begin
        wp:=@PIPoly(lp)[n-1];
        si:=mf.vm_Load(si,wp^,len);
        Unpack_ipoly(lp,wp, Result+1);
      end
      else Inc(si,len);

      if Assigned(hp) then

      if flags and fl_xyz <> 0 then begin

        hp_:=@PSmallInts(hp)[n];

        si:=mf.vm_Load(si,z,SizeOf(Integer));
        si:=mf.vm_Load(si,hp_^,n*SizeOf(SmallInt));

        for i:=0 to Result do begin
          z:=z + hp_[i]; hp[i]:=z
        end
      end else

      Fillchar(hp^,n*SizeOf(Integer),0);

    end else

    Result:=Load_Height(hp,mind,n,flags)

  end
end;

function tdm_Map.Get_house(offs: int; lp: PLLine;
                           out lt,rb: TPoint): int;
begin
  Result:=0;

  lp.N:=Load_Poly(offs,@lp.Pol,nil,LPolyMax,0);
  if PolyLock(lp) then begin

    Max_Poly_Bound(@lp.Pol,lp.N, lt,rb);

    if lt.X >= dm_lt1.X then
    if lt.Y >= dm_lt1.Y then
    if rb.X <= dm_rb1.X then
    if rb.Y <= dm_rb1.Y then
    Result:=lp.N
  end;

  if Result = 0 then begin
    lp.N:=Load_Poly(offs,@lp.Pol,nil,LPolyMax,fl_long);
    if PolyLock(lp) then begin

      Max_Poly_Bound(@lp.Pol,lp.N, lt,rb);

      if lt.X >= dm_lt1.X then
      if lt.Y >= dm_lt1.Y then
      if rb.X <= dm_rb1.X then
      if rb.Y <= dm_rb1.Y then
      Result:=lp.N;
    end
  end
end;

function tdm_Map.Get_Bound(p: longint; out lt,rb: lxyz): Integer;
var
  Node: dm_Node;
begin
  Result:=-1; lt.v:=_VPoint(0,0,0); rb:=lt;

  if Get_Node(p,Node) in [1..7,11,21..23] then

  with Node.dRec do begin

    lt.p:=o_lt; rb.p:=o_rb;

    case Tag of
  1:  if mind = 0 then begin
        Result:=ord(not Points_Equal(o_lt,o_rb));
        lt.p:=o_lt; rb.p:=o_rb;
      end else

      Result:=Bound_Poly(mind,Node.Flags or fl_long, lt.v,rb.v);

  2,
  3,
  4:  Result:=Bound_Poly(mind,Node.Flags, lt.v,rb.v);

  21,
  22,
  23: Result:=Get_Poly_Cnt(p)
    end
  end
end;

function tdm_Map.Get_Poly(p: longint; lp: PLLine; lp_Max: Integer): Integer;
begin
  Result:=xGet_Poly(p,lp,nil,lp_Max)
end;

function tdm_Map.xGet_Poly(p: longint; lp: PLLine; hp: PIntegers; lp_max: Integer): Integer;

function Load_Unpack(Ptr,Ofs,Flags: Integer;
                     lp: PLLine; hp: PIntegers;
                     lp_Min,lp_Max: Integer): Integer;
var
  ind,len,loc,n: int;
begin
  Result:=-1;

  if Ofs > 0 then begin
    Ofs:=mf.vmx_Block(Ofs,len);

    loc:=SizeOf(TPoint);
    if Flags and fl_xyz <> 0 then
    Inc(loc,Sizeof(Integer));

    n:=len div loc; Result:=pred(n);
    if Result > lp_max then Result:=lp_min;

    len:=(Result+1)*SizeOf(TPoint);

    if len > 0 then begin
      if Assigned(lp) then begin
        mf.vm_Load(Ofs,lp.Pol,len);
        lp.N:=Result; mf_lock:=true
      end;

      if Assigned(hp) then

      if loc = Sizeof(TPoint) then
        Fill_hp(Ptr,hp,Result)
      else begin
        len:=(Result+1)*Sizeof(Integer);
        ind:=Ofs+n*Sizeof(TPoint);
        mf.vm_Load(ind,hp^,len)
      end

    end
  end;
end;
                            
function Load_Signs(Ind: Integer;
                    lp: PLLine; hp: PIntegers;
                    lp_Max: Integer): Integer;
var
  i,len,n: int; p: TPoint; vc: tcn_rec;
begin
  Result:=-1;

  if Ind > 0 then begin
    Ind:=mf.vmx_Block(Ind,len);
    n:=len div SizeOf(TPoint);

    if n <= lp_Max+1 then begin

      for i:=1 to n do begin
        Ind:=mf.vm_Load(Ind,p,SizeOf(p));

        if Tree.get_vc(p.x,@vc) then begin
          Inc(Result); if Assigned(lp) then
          lp.Pol[Result]:=vc.Pos;
          if Assigned(hp) then hp[Result]:=vc.zpos
        end
      end;

      if Assigned(lp) then lp.N:=Result
    end
  end
end;

var
  loc: int; _lp: PLPoly; Node: dm_Node;
begin
  Result:=-1;

  mf_lock:=false;
  mf_curve:=false;

  _lp:=nil; if Assigned(lp) then
  begin lp.N:=-1; _lp:=@lp.Pol end;

  loc:=dm_Transit_loc(Get_Node(p,Node));
  if loc in [1..8,11,21..23] then begin

    if Tree.dm_lib > 0 then begin
      if Assigned(lp) then begin
        Result:=Tree.vm_dmw.Get_mf(p,@lp.Pol,hp,lp_max);
        lp.N:=Result
      end
    end

    else with Node,dRec do

    case loc of

  1:  begin
        if mind > 0 then begin
          Flags:=Flags or fl_long;
          Result:=Load_Poly(mind,_lp,hp,lp_max,Flags);
          if Result > 2 then Result:=-1
        end;

        if Result < 0 then begin
          Result:=ord( (ox1 <> ox2) or (oy1 <> oy2) );

          if Assigned(lp) then begin
            lp.Pol[0].x:=ox1; lp.Pol[0].y:=oy1;
            if Result = 1 then begin
              lp.Pol[1].x:=ox2; lp.Pol[1].y:=oy2
            end
          end;

          Flags:=0
        end;

        if Assigned(lp) then lp.N:=Result;

        if Assigned(hp) then
        if Flags and fl_xyz = 0 then
        Fill_hp(p,hp,Result)
      end;
  2,
  3,
  4,
  11: if mind > 0 then begin
        if dm_ver = 2 then Flags:=fl_long;
        Result:=Load_Poly(mind,_lp,hp,lp_max,Flags);

        if Assigned(lp) then begin
          lp.N:=Result; mf_lock:=PolyLock(lp)
        end;

        if Assigned(hp) then
        if Flags and fl_xyz = 0 then
        Fill_hp(p,hp,Result);
      end else
      if Tag = 3 then begin

        if Assigned(lp) then
        with lp^ do begin
          Pol[0].x:=ox1; Pol[0].y:=oy1;
          Pol[1].x:=ox2; Pol[1].y:=oy1;
          Pol[2].x:=ox2; Pol[2].y:=oy2;
          Pol[3].x:=ox1; Pol[3].y:=oy2;
          Pol[4]:=Pol[0]; N:=4
        end;

        Result:=4; mf_lock:=true;
        Fill_hp(p,hp,Result)
      end;

  5,
  8:  Result:=Load_Unpack(p,mind,flags, lp,hp, 1,2);

  6,
  7:  begin
        Result:=Load_Unpack(p,mind,flags, lp,hp, lp_max,lp_max);
        if Assigned(lp) then mf_lock:=CurveLock(lp);
        mf_curve:=true
      end;

  21: if fIsDoctor then
        Result:=Load_Unpack(p,mind,0, lp,nil, lp_max,lp_max)
      else
        Result:=Load_Signs(mind, lp,hp, lp_Max);

  22,
  23: Result:=Load_Unpack(p,mind,0, lp,nil, lp_max,lp_max);

    end

  end
end;

function tdm_Map.xGet_mf(p: Int64; lp: PLLine; hp: PIntegers): Integer;
var
  x: TInt64; v: tcn_rec;
begin
  Result:=-1; mf_lock:=false;
  if Assigned(lp) then lp.N:=-1;

  x.x:=p;

  if x.cn = cn_object then
    Result:=xGet_Poly(x.id,lp,hp,LPolyMax)
  else
  if x.cn = cn_node then begin
    if Tree.get_vc(x.Id,@v) then begin
      if Assigned(lp) then begin
        lp.N:=0; lp.Pol[0]:=v.Pos
      end;

      if Assigned(hp) then hp[0]:=v.zpos;
      Result:=0
    end
  end else
  if x.cn = cn_edge then begin
    if Tree.get_ve(x.Id,@v) then begin
      mf_lock:=v.vc1 = v.vc2;
      Result:=Get_ve_mf(@v,lp,hp,LPolyMax)
    end
  end
end;

function tdm_Map.xGet_mf1(p: Int64; lp: PLLine; hp: PIntegers): Integer;
begin
  Result:=-1; lp.N:=-1;
  if Enabled_Map then
  Result:=xGet_mf(p,lp,hp)
end;

function tdm_Map.mf_poly(p: Int64; poly: TPolyList;
                         lp: PLLine; hp: PIntegers): Integer;
begin
  poly.Clear;

  Result:=xGet_mf(p,lp,hp);

  if xTag_Object(p) in [22,23] then
    cn_poly(p,poly,lp,hp)
  else
    poly.add_poly(@lp.Pol,hp,lp.N+1);

  Result:=poly.Count
end;

function tdm_Map.Get_xyz(p: Int64; out v: lxyz): Boolean;
var
  l: VLLine; z: ZOrient;
begin
  Result:=false; v.v:=_VPoint(0,0,0);

  if xTag_Object(p) = 1 then
  if xGet_poly(p,@l,@z,3) >= 0 then begin
    v.p:=l.Pol[0]; v.v.z:=z[0];
    Result:=true
  end
end;

function tdm_Map.Get_Polybuf(p: longint; lp: PLLine): Integer;
begin
  Result:=xGet_Poly(p,lp,nil,LPolyMax)
end;

function tdm_Map.xGet_Polybuf(p: longint; lp: PLLine; hp: PIntegers): Integer;
begin
  Result:=xGet_Poly(p,lp,hp,LPolyMax)
end;

function tdm_Map.Get_Polygon(p: longint; lp: PLLine; hp: PIntegers): Integer;
var
  loc: Integer;
begin
  Result:=-1;
  loc:=Tag_Object(p);

  if loc in [3,5] then
  if Get_Poly(p,lp,LPolyMax) > 0 then

  case loc of
3:  Result:=lp.N;
5:  Result:=Ellipse_LLine(lp,LPolyMax,0.8);
  end
end;

function tdm_Map.Get_Vertex(p: Int64; lp_i: Integer;
                            out v: VPoint): Boolean;
var
  lp,_lp: PLLine; hp,_hp: PIntegers;
  buf1,buf2: TLLine; n: Integer;
begin
  Result:=false; v:=_VPoint(0,0,0);

  if p > 0 then
  if lp_i >= 0 then

  if Enabled_Map then begin

    n:=Get_Poly_Cnt(p); if n < 0 then
    if Tree.dm_lib > 0 then n:=LPolyMax;

    if n >= 0 then begin

      lp:=@buf1; hp:=@buf2; _lp:=nil;

      if n > LPoly_Max then begin
        _lp:=Alloc_ZPolyBuf(_hp);
        lp:=_lp; hp:=_hp;
      end;

      if Assigned(lp) then
      if xGet_mf(p,lp,hp) >= 0 then
      if lp_i <= lp.N then begin
        v.x:=lp.Pol[lp_i].x; v.y:=lp.Pol[lp_i].y;
        v.z:=hp[lp_i]; Result:=true
      end;

      xFreePtr(_lp)
    end
  end
end;

procedure tdm_Map.Put_Vertex(p: Int64; lp_i: longint;
                             const v: VPoint);
var
  lp,_lp: PLLine; hp,_hp: PIntegers;
  buf1,buf2: TLLine; x: TInt64;
  n: Integer;
begin
  x.x:=p;

  if x.cn = 0 then

  if p > 0 then
  if lp_i >= 0 then

  if Update_Map then begin

    n:=Get_Poly_Cnt(p); if n < 0 then
    if Tree.dm_lib > 0 then n:=LPolyMax;

    if n >= 0 then begin

      lp:=@buf1; hp:=@buf2; _lp:=nil;

      if n > LPoly_Max then begin
        _lp:=Alloc_ZPolyBuf(_hp);
        lp:=_lp; hp:=_hp;
      end;

      if Assigned(lp) then
      if xGet_Poly(p,lp,hp,LPolyMax) >= 0 then
      if lp_i <= lp.N then begin
        lp.Pol[lp_i].x:=v.x; lp.Pol[lp_i].y:=v.y;
        hp[lp_i]:=v.z; Update_xyz(p,0,lp,hp);
      end;

      xFreePtr(_lp)
    end
  end
end;

function tdm_map.Update_Profile(p: Int64; xy: PGPoly;
                                xy_n,xy_ind: Integer): bool;
var
  lp: PLLine; hp: PIntegers;
  i: Integer; p1,p2: TPoint; v: lxyz;
  len1,len2,rmu: Double;
begin
  Result:=false;

  if p > 0 then
  if Update_Map then begin

    lp:=Alloc_ZPolyBuf(hp);
    if Assigned(lp) then
    if xGet_Poly(p,lp,hp,LPolyMax) > 0 then begin

      if xy_ind > 0 then
      if xy_ind < xy_n-1 then

      if lp.N = xy_n-2 then
      if lp.N < LPolyMax then begin

        p1:=lp.Pol[xy_ind-1];
        p2:=lp.Pol[xy_ind];

        len1:=xy[xy_ind].x - xy[xy_ind-1].x;
        len2:=xy[xy_ind+1].x - xy[xy_ind-1].x;

        if len2 > Small then
        if len1 < len2 then begin
          rmu:=len1 / len2;
          v.v.x:=p1.X + Round((p2.X-p1.X)*rmu);
          v.v.y:=p1.Y + Round((p2.Y-p1.Y)*rmu);
          v.v.z:=0;

          LPoly_Insert(lp,nil,xy_ind-1,LPolyMax,v.v)
        end
      end;

      if lp.N = xy_n-1 then begin

        for i:=0 to lp.N do
        hp[i]:=Round(xy[i].y * z_res);

        Update_xyz(p,0,lp,hp);
        Result:=true
      end;
    end;

    xFreePtr(lp)
  end
end;

procedure tdm_Map.Centre_Object(p: Int64; out c: TPoint);
var
  dRec: dm_Rec;
begin
  xGet_Object(p,dRec);

  if dRec.Tag in [1,4] then
    c:=dRec.o_lt
  else begin
    c.X:=Round(dRec.ox1/2+dRec.ox2/2);
    c.Y:=Round(dRec.oy1/2+dRec.oy2/2)
  end
end;

function tdm_Map.wgs_centre(p: Int64; out c: txyz): double;
var
  dRec: dm_Rec; a,b,pc: TPoint;
begin
  Result:=-1;

  xGet_Object(p,dRec);
  if dRec.Tag in [1,3] then begin

    a:=dRec.o_lt; b:=a;
    if dRec.Tag = 3 then begin
      a:=dRec.o_lt; b:=dRec.o_rb
    end;

    Middle_Point(a,b,pc);
    c:=dm_to_wgs(pc.X,pc.Y,0); c.z:=0;

    Result:=Long_Dist(a,b) * dm_Resolution
  end
end;

function tdm_Map.Ind_Blank(p: Int64): Integer;
var
  dRec: dm_Rec;
begin
  xGet_Object(p,dRec); with dRec do
  Result:=obj_Ind_Blank(Code,Tag)
end;

function tdm_Map.obj_Ind_Blank(code,loc: Integer): Integer;
var
  oRec: Obj_Rec; is_close: Boolean;
begin
  Result:=0;

  is_close:=not Obj.vm_Active;

  if Open_Objects then begin
    if loc = 255 then loc:=0;
    Obj.Obj_Index(code,loc,oRec,nil);
    Result:=oRec.bln;

    if is_close then Obj.vm_Close
  end else
  if Tree.dm_lib > 0 then Result:=1
end;

function tdm_Map.Capt_Object(p: Int64; status: Boolean): string;
var
  code,loc,lev: Integer;
  v1,v2,v3,v4,v5,v6,cl: int;
  p_: TInt64; dRec: dm_Rec;
  fn: TShortStr; s: String;
begin
  s:=''; p_.x:=p;

  if Tree.dm_lib > 0 then begin
    if Tree.vm_dmw.Capt_Object(p,fn) <> nil then
    s:=StrPas(fn)
  end else

  if p_.cn = cn_object then begin

    loc:=xGet_Object(p_.id,dRec);
    code:=dRec.Code; cl:=Color_Object(p);

    if ((code and $80000000) <> 0) and (loc = 1) then
      s:=Format('sign #%d, H=%d',[cl and $FFF,code and $FFF])
    else
    if loc = 101 then begin
      if code = 1 then s:='диаграмма' else
      if code = 2 then s:='гистограмма'
    end else
    if loc = 104 then begin
      hgt_unpack_color(cl,v2,v3);
      v1:=hgt_unpack_code(dRec.Code,v4,v5);
      s:=Format('text: H=(%dm,%dp), dXY=%d,%d',[v1,v2,v4,v5])
    end
    else begin
      if status then
      s:=xCodeToStr(code,loc);

      if Open_Objects then
      if Obj.StrCapt(code,loc,fn) <> nil then begin
        xStrName(fn,fn,fobj_index);
        if Strlen(fn) > 0 then begin
          if status then s:=s+' - ';
          s:=s+StrPas(fn)
        end
      end;
    end;

    if Length(s) = 0 then
    s:=xCodeToStr(code,loc);

    Obj.vm_Close
  end
  else begin

    if p_.cn = cn_node then
      s:='узел'
    else
    if p_.cn = cn_edge then
      s:='ребро';

    if length(s) > 0 then
    s:=s+' #'+IntToStr(p_.id)
  end;

  Result:=s
end;

function tdm_Map.Get_HF(p, n: Integer; id: Id_Tag; var equ): Boolean;
var
  stm: thfStream; dRec: dm_Rec;
begin
  Result:=false; if n > 0 then

  if Tree.dm_lib > 0 then begin
  end
  else begin
    if p = 0 then p:=Tree.Root;
    Get_Object(p,dRec);

    if dRec.hind > 0 then begin

      stm:=thfStream.Create(hf_x16);
      try
        if hf.Stream_Copy(dRec.hind,stm) > 0 then
        Result:=Get_equ(stm, n,id,0, equ);
      finally
        stm.Free
      end

    end
  end
end;

function tdm_Map.Get_sval(p, n: Integer; Str: PChar): Boolean;
var
  stm: thfStream; dRec: dm_Rec;
begin
  Result:=false; if n > 0 then

  if Tree.dm_lib > 0 then begin
  end
  else begin
    if p = 0 then p:=Tree.Root;
    Get_Object(p,dRec);

    if dRec.hind > 0 then begin

      stm:=thfStream.Create(hf_x16);
      try
        if hf.Stream_Copy(dRec.hind,stm) > 0 then
        Result:=Get_hint(stm,n,Str,255)
      finally
        stm.Free
      end

    end
  end
end;

procedure tdm_Map.Put_hf(p, n: Integer; id: Id_Tag; var equ);
var
  stm: TInfoStream;
begin
  stm:=TInfoStream.Create(hf_x16);
  try
    if p = 0 then p:=Tree.Root;

    Load_hf(p,stm);
    stm.Put_HF(n,id,equ);
    Update_hf(p,stm);

    if (n = nn_4) or (n = nn_7) then
    Trunc_z_axe(p);

  finally
    stm.Free
  end
end;

procedure tdm_Map.Put_xy(p, n: Integer; x,y: Double);
var
  stm: TInfoStream;
begin
  stm:=TInfoStream.Create(hf_x16);
  try
    if p = 0 then p:=Tree.Root;

    Load_hf(p,stm);
    stm.Put_HF(n,_double,x);
    stm.Put_HF(n+1,_double,y);
    Update_hf(p,stm);

  finally
    stm.Free
  end
end;

procedure tdm_Map.Put_rvec(p, n: Integer; id: Id_Tag;
                           fp: PDoubles; Count: Integer);
var
  stm: TInfoStream; i: Integer; f: Float;
begin
  stm:=TInfoStream.Create(hf_x16);
  try
    if p = 0 then p:=Tree.Root;
    Load_hf(p,stm);
    for i:=0 to Count-1 do

    if fp = nil then
      stm.Del_HF(n+i,_double)
    else
    if id = _double then
      stm.Put_HF(n+i,id,fp[i])
    else begin
      f:=fp[i];
      stm.Put_HF(n+i,id,f)
    end;

    Update_hf(p,stm);
  finally
    stm.Free
  end
end;

procedure tdm_Map.Put_ivec(p, n: Integer; id: Id_Tag;
                           ip: PIntegers; Count: Integer);
var
  stm: TInfoStream; i: Integer;
begin
  stm:=TInfoStream.Create(hf_x16);
  try
    if p = 0 then p:=Tree.Root;
    Load_hf(p,stm);

    for i:=0 to Count-1 do
    if ip = nil then
      stm.Del_HF(n+i,_long)
    else
      stm.Put_HF(n+i,id,ip[i]);

    Update_hf(p,stm);
  finally
    stm.Free
  end
end;

procedure tdm_Map.Del_hf(p, n: Integer; id: Id_Tag);
var
  stm: TInfoStream;
begin
  stm:=TInfoStream.Create(hf_x16);
  try
    if p = 0 then p:=Tree.Root;

    Load_hf(p,stm);
    stm.Del_HF(n,id);
    Update_hf(p,stm)
  finally
    stm.Free
  end
end;

function tdm_Map.Get_Int(p, n: Integer): longint;
begin
  Result:=0; Get_hf(p,n,_long,Result)
end;

function tdm_Map.Get_Real(p, n: Integer): float;
begin
  Result:=0; Get_hf(p,n,_real,Result)
end;

function tdm_Map.Get_Double(p, n: Integer): double;
begin
  Result:=0; Get_hf(p,n,_double,Result)
end;

function tdm_Map.Get_id(p: Int64): uint;
var
  x: TInt64; id: Integer; v: tcn_rec;
begin
  Result:=0; x.x:=p;

  if x.cn = cn_object then begin
    if xGet_Int(p,1000,id) then
    Result:=id
  end else
  if x.cn = cn_node then begin
    if Tree.Get_vc(x.id,@v) then
    Result:=v.rcid
  end else
  if x.cn = cn_edge then begin
    if Tree.Get_ve(x.id,@v) then
    Result:=v.rcid
  end else
  if x.cn = cn_aggr then begin
    if Tree.Get_fc(x.id,@v) then
    Result:=v.rcid
  end
end;

function tdm_Map.GetValue(p, n: Integer; out v: Double): Boolean;
begin
  Result:=false; v:=0;
  if Assigned(fOnGetValue) then
  Result:=fOnGetValue(p,n,v) 
end;

function tdm_Map.xGet_Int(p, n: Integer; out i: longint): Boolean;
begin
  i:=0; Result:=Get_hf(p,n,_long, i)
end;

function tdm_Map.xGet_Real(p, n: Integer; out r: float): Boolean;
begin
  r:=0; Result:=Get_hf(p,n,_real, r)
end;

function tdm_Map.xGet_Double(p, n: Integer; out r: double): Boolean;
begin
  Result:=false;
  if Get_hf(p,n,_double, r) then
  Result:=not IsNAN(r)
end;

function tdm_Map.Get_Gauss(p, n: Integer; out g: tgauss): Boolean;
begin
  Result:=false; g.x:=0; g.y:=0;
  if Get_hf(p,n,_double, g.x) then
  if Get_hf(p,n+1,_double, g.y) then
  if not IsNAN(g.x) then
  if not IsNAN(g.y) then
  Result:=true
end;

function tdm_Map.xGet_Str(p, n: Integer; s: PChar): PChar;
var
  s_: string;
begin
  Result:=nil; StrCopy(s,'');
  if Get_hf(p,n,_string, s_) then
  Result:=StrPCopy(s,s_)
end;

procedure tdm_Map.xPut_Int(p, n: Integer; v: longint);
begin
  Put_hf(p,n,_long,v)
end;

procedure tdm_Map.xPut_Real(p, n: Integer; v: float);
begin
  Put_hf(p,n,_real,v)
end;

procedure tdm_Map.xPut_Double(p, n: Integer; v: double);
begin
  Put_hf(p,n,_double,v)
end;

procedure tdm_Map.xPut_Str(p, n: Integer; s: PChar);
var
  t: string;
begin
  if StrLen(s) = 0 then
    Del_hf(p,n,_string)
  else begin
    t:=StrPas(s);
    Put_hf(p,n,_string,t)
  end
end;

procedure tdm_Map.xAssign_Int(p, n: Integer; v: longint);
begin
  if v <> 0 then
  Put_hf(p,n,_long,v)
end;

procedure tdm_Map.xAssign_Real(p, n: Integer; v: float);
begin
  if v <> 0 then
  Put_hf(p,n,_real,v)
end;

procedure tdm_Map.xAssign_Double(p, n: Integer; v: double);
begin
  if v <> 0 then
  Put_hf(p,n,_double,v)
end;

function tdm_Map.xGet_z(p: Integer; out z: double): Integer;
begin
  Result:=0; z:=0;

  if Get_hf(p,nn_4,_double, z) then
    Result:=nn_4
  else
  if Get_hf(p,nn_7,_double, z) then
    Result:=nn_7
  else
  if Get_hf(p,1007,_double, z) then
    Result:=1007
end;

function tdm_Map.xDelete_z(p: Integer): Boolean;
var
  z: Double;
begin
  Result:=false;

  if Get_hf(p,nn_4,_double, z) then begin
    Del_hf(p,nn_4,_double); Result:=true
  end else
  if Get_hf(p,nn_7,_double, z) then begin
    Del_hf(p,nn_7,_double); Result:=true
  end else
  if Get_hf(p,1007,_double, z) then begin
    Del_hf(p,1007,_double); Result:=true
  end
end;

function tdm_Map.xGet_Relief(p: Integer; out h: double): Boolean;
var
  stm: thfStream;
  h1,h2: Double;
  dRec: dm_Rec;
begin
  Result:=false; h:=0;

  if Tree.dm_lib = 0 then begin
    Get_Object(p,dRec);
    if dRec.hind > 0 then begin

      stm:=thfStream.Create(hf_x16);
      try
        if hf.Stream_Copy(dRec.hind,stm) > 0 then

        if Get_equ(stm, nn_4,_double,0, h) then
          Result:=true
        else
        if fRel_s57 then begin
          if Get_equ(stm, nn_7,_double,0, h)
          or Get_equ(stm, 1007,_double,0, h) then
          begin Result:=true; h:=-h end;

          if not Result then
          if Get_equ(stm, 87,_double,0, h1) then begin
            if not Get_equ(stm, 88,_double,0, h2) then
            h2:=h1; if eps_Equal(h1,h2,0.001) then
            begin Result:=true; h:=-h1 end
          end
        end;

      finally
        stm.Free
      end

    end
  end
end;

function tdm_Map.xGet_Height(p: Integer; out h: double): Boolean;
begin
  h:=0; Result:=Get_hf(p,nn_4,_double, h);
end;

procedure tdm_Map.xPut_Height(p: Integer; h: double);
begin
  if fnn_4 = 0 then fnn_4:=4;
  Put_hf(p,nn_4,_double,h)
end;

function tdm_Map.Get_hg(p: Integer;
                        iv,ic: PIntegers;
                        out r1,r2: int;
                        Hint: PChar): int;
var
  n1,n2: int; stm: thfStream;
  dRec: dm_Rec; s: String;
begin
  n1:=0; n2:=0; r1:=0; r2:=0;

  if Assigned(Hint) then
  StrCopy(Hint,'');

  if Tree.dm_lib = 0 then begin
    Get_Object(p,dRec);
    if dRec.hind > 0 then begin

      stm:=thfStream.Create(hf_x16);
      try
        if hf.Stream_Copy(dRec.hind,stm) > 0 then begin

          if Assigned(iv) then
          if Get_equ(stm, ivg_nn_iv,_string,0, s) then
          n1:=StrToInts(s,iv);

          if Assigned(ic) then
          if Get_equ(stm, ivg_nn_ic,_string,0, s) then
          n2:=StrToInts(s,ic);

          Get_equ(stm, ivg_nn_r1,_int,0, r1);
          Get_equ(stm, ivg_nn_r2,_int,0, r2);

          if Assigned(Hint) then
          if Get_equ(stm, 9,_string,0, s) then
          StrPCopy(Hint,WinString(s))
        end;
      finally
        stm.Free
      end

    end
  end;

  Result:=Min(n1,n2)
end;

function tdm_Map.Get_hgt(p: Integer;
                         Name,Font,Style: PChar): int;
var
  stm: thfStream;
  dRec: dm_Rec; s: String;
begin
  Name[0]:=#0; Font[0]:=#0; Style[0]:=#0;

  if Tree.dm_lib = 0 then begin
    Get_Object(p,dRec);
    if dRec.hind > 0 then begin

      stm:=thfStream.Create(hf_x16);
      try
        if hf.Stream_Copy(dRec.hind,stm) > 0 then begin
          if Get_equ(stm, 9,_string,0, s) then
          StrPCopy(Name,WinString(s));

          if Get_equ(stm, 10,_string,0, s) then
          StrPCopy(Font,s);

          if Get_equ(stm, 11,_string,0, s) then
          StrPCopy(Style,s);
        end;
      finally
        stm.Free
      end

    end
  end;

  Result:=Strlen(Name)
end;

function tdm_Map.Get_mm_Org: TGauss;
begin
  Result.x:=Get_Double(0,nn_mm+1);
  Result.y:=Get_Double(0,nn_mm+2);
end;

procedure tdm_Map.Get_mm_Rect(out x,y,w,h: Double);
var
  p: TGauss; a,b: TPoint; s,f: Double;
begin
  p:=Get_mm_Org; x:=p.x; y:=p.y;

  s:=Get_Double(0,nn_mm+3);
  if (s < 1) and (dm_scale > 0) then begin
    a:=dm_lt; b:=dm_rb; b.y:=dm_lt.y;
    s:=LG_T.xy_dist(a,b, f)*1000/dm_scale
  end; w:=Max(0,s);

  s:=Get_Double(0,nn_mm+4);
  if (s < 1) and (dm_scale > 0) then begin
    a:=dm_lt; b:=dm_rb; b.x:=dm_lt.x;
    s:=LG_T.xy_dist(a,b, f)*1000/dm_scale;
  end; h:=Max(0,s);
end;

procedure tdm_Map.Set_mm_Org(x,y: Double);
begin
  xPut_Double(0,nn_mm+1,x);
  xPut_Double(0,nn_mm+2,y)
end;

procedure tdm_Map.Set_mm_Extent(w,h: Double);
begin
  xPut_Double(0,nn_mm+3,w);
  xPut_Double(0,nn_mm+4,h)
end;

procedure tdm_Map.Set_mm_Rect(x,y,w,h: Double);
begin
  Set_mm_Org(x,y);
  Set_mm_Extent(w,h)
end;

procedure tdm_Map.dm_Local_Bound(out lt,rb: TPoint; k: float);
var
  dx,dy: longint;
begin
  dx:=Round((dm_rb.x-dm_lt.x)*k);
  dy:=Round((dm_rb.y-dm_lt.y)*k);

  lt.x:=dm_lt.x-dx; rb.x:=dm_rb.x+dx;
  lt.y:=dm_lt.y-dy; rb.y:=dm_rb.y+dy;
end;

function tdm_Map.dm_pps_Bound(out lt,rb: tgauss): Integer;
var
  link: xScanLink;
  nn,cnt,p: Integer;
  g: tgauss;
begin
  Result:=LG_T.Get_pps_Bound(lt,rb);

  nn:=901; if Result = 1 then
  nn:=91; cnt:=0;

  link:=xScanLink.Create(Self);
  try
    while cnt < 16 do begin
      p:=link.Scan_tree(cnt = 0,true);
      if p = 0 then Break; Inc(cnt);
      if Get_Gauss(p,nn,g) then
      Max_GPort(lt,rb, g)
    end;
  finally
    link.Free
  end
end;

function tdm_Map.dm_pps_Bound1(out lt,rb: tgauss): Integer;
var
  i: int; b: TLLine; g: TGauss;
begin
  Result:=-1;
  if xGet_Poly(Tree.Root,@b,nil,LPoly_Max) > 0 then
  if PolyLock(@b) then begin

    for i:=0 to b.N do begin
      LG_T.L_to_R(b.Pol[i],TGeoid(g));

      if i > 0 then
        Max_GPort(lt,rb, g)
      else begin
        lt:=g; rb:=g
      end
    end;

    Result:=LG_T.sys.pps
  end;

  if Result < 0 then
  Result:=dm_pps_Bound(lt,rb)
end;

procedure tdm_Map.dm_Gauss_Bound(out lt,rb: tgauss);
var
  i: Integer; l: lOrient; g: gOrient;
begin
  Get_dm_LRect(@l);
  for i:=0 to 3 do LG_T.L_to_Z(l[i],g[i]);
  Max_Gauss_Bound(@g,4,lt,rb)
end;

procedure tdm_Map.Get_dm_Hdr(out hdr: dm_Hdr);
var
  Spool: TInfoStream;
  is_dat: Boolean; fn: TShortStr;
begin
  Init_dm_Hdr(hdr);

  if Tree.vm_Active then

  if Tree.dm_lib > 0 then begin
    hdr.ver:=1;
    hdr.dm_scale:=Tree.vm_dmw.Get_Scale;
    hdr.scale:=dm_Scale_Index(hdr.dm_scale);

    Tree.vm_dmw.Get_sys(hdr.sys);

    if Tree.vm_dmw.Get_Legend(fn) <> nil then
    hdr.obj:=StrPas(fn)

  end else
  if hf.vm_Active then begin

    Spool:=TInfoStream.Create(hf_x16);
    try
      if load_hf(Tree.Root,Spool) > 0 then
      with hdr do begin

        ver:=Spool.Get_int(999);

        y_get:=Spool.Get_int(921);
        y_upd:=Spool.Get_int(922);
        y_mag:=Spool.Get_int(923);
        y_pab:=Spool.get_int(924);

        sys.pps:=Spool.Get_int(901) and 1;

        scale:=Spool.Get_int(903);
        if scale > 11 then scale:=0;

        sys.elp:=Spool.Get_int(911);
        sys.prj:=Spool.Get_int(913);
        zzz:=Spool.Get_int(912);
        fot:=Spool.Get_int(917);
        srct:=Spool.Get_int(925);

        nil1:=Spool.Get_int(816); // depth nil
        nil2:=Spool.Get_int(832); // height nil

        dm_scale:=Spool.Get_int(904);
        if dm_scale = 0 then begin
          dm_scale:=10000; if scale > 0 then
          dm_scale:=dmScale[scale]
        end;

        dm_nnn:=Spool.Get_int(1011);

        sys.b1:=Spool.Get_Real(991);
        sys.b2:=Spool.Get_Real(992);
        sys.lc:=Spool.Get_Real(993);

        sys.Rn:=Spool.Get_Real(994);

        sys.x0:=Spool.Get_real(1019);
        sys.y0:=Spool.Get_real(1020);
        sys.b3:=Spool.Get_real(1021);
        sys.k0:=Spool.Get_real(1022);

        r_div:=Spool.Get_real(961);

        is_dat:=false; with sys.dat do

        if Spool.xGet_real(995,dX) then
        if Spool.xGet_real(996,dY) then
        if Spool.xGet_real(997,dZ) then begin

          Spool.xGet_real(1015,wX);
          Spool.xGet_real(1016,wY);
          Spool.xGet_real(1017,wZ);
          Spool.xGet_real(1018,m);

          if (Abs(wX) >= 1)
          or (Abs(wY) >= 1)
          or (Abs(wZ) >= 1)
          or (Abs(m) >= 1) then begin
            wX:=0; wY:=0; wZ:=0; m:=0;
          end;

          Datum_norm(sys.dat); is_dat:=true
        end;

        if (Abs(sys.dat.dX) >= 10000)
        or (Abs(sys.dat.dY) >= 10000)
        or (Abs(sys.dat.dZ) >= 10000) then begin
          Fillchar(sys.dat,Sizeof(sys.dat),0);
          is_dat:=false
        end;

        if not is_dat then
        if sys.pps = 1 then
        if sys.elp in [0,1] then
        sys.dat:=ru42_Datum;

        Spool.Get_HF(901,_string,nom);
        Spool.Get_HF(902,_string,idt);
        Spool.Get_HF(903,_string,obj);

        Spool.Get_HF(904,_string,loc);
        Spool.Get_HF(905,_string,st_loc);
        Spool.Get_HF(906,_string,own);
        Spool.Get_HF(603,_string,access);

        obj:=AnsiLowerCase(obj)
      end;
    finally
      Spool.Free
    end
  end
end;
           
procedure tdm_Map.Put_dm_Hdr(const hdr: dm_Hdr);
var
  Spool: TInfoStream;
  loop,v: Integer; dat: TDatum7;
begin
  if Tree.dm_lib > 0 then begin
  end
  else with hdr do begin

    Spool:=TInfoStream.Create(hf_x16);
    try
      load_hf(Tree.Root,Spool);

      v:=ver;
      Spool.Put_hf(999,_word,v);

      Spool.Put_hf(921,_long,y_get);
      Spool.Put_hf(922,_long,y_upd);
      Spool.Put_hf(923,_long,y_mag);
      Spool.Put_hf(924,_long,y_pab);

      Spool.Put_hf(901,_byte,sys.pps);
      Spool.Put_hf(903,_byte,scale);

      Spool.Put_hf(911,_byte,sys.elp);
      Spool.Put_hf(912,_byte,zzz);
      Spool.Put_hf(913,_byte,sys.prj);
      Spool.Put_hf(917,_byte,fot);
      Spool.Put_hf(925,_byte,srct);

      Spool.Put_hf(816,_byte,nil1);
      Spool.Put_hf(832,_byte,nil2);

      Spool.Put_hf(904,_long,dm_scale);
      Spool.Put_hf(1011,_long,dm_nnn);

      Spool.Put_hf(991,_double,sys.b1);
      Spool.Put_hf(992,_double,sys.b2);
      Spool.Put_hf(993,_double,sys.lc);

      if sys.Rn >= 6000000 then
      if sys.Rn <= 7000000 then
      Spool.Put_hf(994,_double,sys.Rn);

      if Abs(sys.x0) >= 1 then
        Spool.Put_hf(1019,_double,sys.x0)
      else
        Spool.Del_HF(1019,_double);

      if Abs(sys.y0) >= 1 then
        Spool.Put_hf(1020,_double,sys.y0)
      else
        Spool.Del_HF(1020,_double);

      if sys.b3 > sys.b2 then
        Spool.Put_hf(1021,_double,sys.b3)
      else
        Spool.Del_HF(1021,_double);

      if (sys.k0 >= 0.9)
      and (sys.k0 >= 1.1) then
        Spool.Put_hf(1022,_double,sys.k0)
      else
        Spool.Del_HF(1022,_double);

      if r_div >= 0.01 then
        Spool.Put_hf(961,_double,r_div)
      else
        Spool.Del_HF(961,_double);

      if sys.elp = 9 then begin
        Spool.Del_HF(995,_double);
        Spool.Del_HF(996,_double);
        Spool.Del_HF(997,_double);
        Spool.Del_HF(1015,_double);
        Spool.Del_HF(1016,_double);
        Spool.Del_HF(1017,_double);
        Spool.Del_HF(1018,_double);
      end
      else begin
        dat:=sys.dat; with dat do
        if (dX > 1) or (dY > 1) or (dZ > 1) then begin
          Datum_disp(dat);
          Spool.Put_hf(995,_double,dX);
          Spool.Put_hf(996,_double,dY);
          Spool.Put_hf(997,_double,dZ);

          Spool.Put_hf(1015,_double,wX);
          Spool.Put_hf(1016,_double,wY);
          Spool.Put_hf(1017,_double,wZ);
          Spool.Put_hf(1018,_double,m);
        end
      end;

      Spool.Put_str(901,nom);
      Spool.Put_str(902,idt);
      Spool.Put_str(903,obj);

      Spool.Put_str(904,loc);
      Spool.Put_str(905,st_loc);
      Spool.Put_str(906,own);
      Spool.Put_str(603,access);

      update_hf(Tree.Root,Spool);
    finally
      Spool.Free
    end;

  end
end;

function tdm_Map.dm_ReadOnly: Boolean;
var
  fl: longint;
begin
  fl:=0; Result:=false;
  if xGet_Int(Tree.Root,988,fl) then
  if fl = 1 then Result:=true
end;

function tdm_Map.Get_Password: string;
var
  pwd: string;
begin
  Result:='';
  if Get_hf(Tree.Root,989,_string, pwd) then
  Result:=pwd
end;

procedure tdm_Map.Put_Password(pwd: string);
begin
  if Tree.vm_Active then if hf.vm_Edit then
  Put_hf(Tree.Root,989,_string,pwd)
end;

function tdm_Map.xTag_Object(p: Int64): Integer;
var
  dRec: dm_Rec;
begin
  Result:=xGet_Object(p,dRec)
end;

function tdm_Map.xLoc_Object(p: Int64): Integer;
begin
  Result:=dm_Transit_loc(xTag_Object(p))
end;

function tdm_Map.xCode_Object(p: Int64; out code: int): int;
var
  dRec: dm_Rec;
begin
  xGet_Object(p,dRec); code:=dRec.Code;
  Result:=dm_Transit_loc(dRec.Tag)
end;


function tdm_Map.Is_Link_Object(p: longint): Boolean;
var
  dRec: dm_Rec;
begin
  Result:=false;
  if xGet_Object(p,dRec) = 1 then
  Result:=dRec.Code = 0
end;

function tdm_Map.Code_Object(p: Int64): longint;
var
  dRec: dm_Rec;
begin
  xGet_Object(p,dRec); Result:=dRec.Code
end;

function tdm_Map.Tag_Object(p: Int64): Integer;
var
  dRec: dm_Rec;
begin
  Result:=xGet_Object(p,dRec)
end;

function tdm_Map.Color_Object(p: Int64): Integer;
var
  Node: dm_Node; x: TInt64;
begin
  x.x:=p; Result:=0;

  if x.cn = cn_object then begin
    Get_Node(p,Node); with Node do
    Result:=Long_words(dRec.Color,Node.Tag)
  end
end;

function tdm_Map.Level_Object(p: Int64): Integer;
var
  dRec: dm_Rec; x: TInt64;
begin
  Result:=0; x.x:=p; if x.cn = 0 then
  if Get_Object(x.id,dRec) <> 0 then
  Result:=dRec.View
end;

function tdm_Map.Text_up(p: longint): Integer;
begin
  Result:=Tree._Tag(p)
end;

function tdm_Map.Style_Object(p: longint): Integer;
var
  oRec: Obj_Rec;
begin
  Result:=0;

  if Open_Objects then
  if Obj.Obj_Index(Code_Object(p),
                   Tag_Object(p),
                   oRec,nil) > 0 then
  Result:=oRec.Style;

  Obj.vm_Close
end;

function tdm_Map.Height_Object(p: longint): Double;
var
  v: VLLine; z: Double;
  h: array[0..7] of Integer;
begin
  Result:=0;

  if xGet_Height(p,z) then
    Result:=z
  else
  if Tag_Object(p) in [1..3] then
  if Get_Flags(p) and fl_xyz <> 0 then
  if xGet_Poly(p,@v,@h,1) >= 0 then
  Result:=h[0]/z_res
end;

function tdm_Map.Tag_Parent(p: longint): Integer;
begin
  Result:=0; p:=Tree._Parent(p);
  if p > 0 then Result:=Tag_Object(p)
end;

function tdm_Map.Get_Node(p: longint; out Node: dm_Node): Integer;
var
  rec: tdmRec; tmp: dm_Node;
begin
  Result:=-1;

  if Tree.dm_lib > 0 then begin

    Tree.vm_dmw.Get_Object(p,rec,nil);

    FillChar(Node,SizeOf(Node),0);
    with Node do begin
      dRec.Code:=rec.Code; dRec.Tag:=rec.Loc;
      dRec.View:=rec.Level; dRec.Color:=rec.Color;
      dRec.ox1:=rec.ox1; dRec.oy1:=rec.oy1;
      dRec.ox2:=rec.ox2; dRec.oy2:=rec.oy2;
      dRec.mind:=p; dRec.hind:=p;
      Tag:=tlong(rec.Color).b[2]
    end;

    Result:=rec.Loc
  end else
  if p <= SF.vm_Ind-SizeOf(Node) then begin
    SF.vm_Load(p,Node,SizeOf(Node));
    Result:=Node.dRec.Tag
  end
end;

function tdm_Map.xGet_Node(p: Int64; out Node: dm_Node): Integer;
var
  x: TInt64; v: tcn_rec;
begin
  x.x:=p;

  Fillchar(Node,SizeOf(Node),0);

  if x.cn = cn_object then
    Get_Node(x.id,Node)
  else
  if x.cn = cn_node then begin
    if xGet_Object(p,Node.dRec) > 0 then
    if Tree.get_vc(x.Id,@v) then
    Node.Flags:=v.bits
  end else
  if x.cn = cn_edge then begin
    if xGet_Object(p,Node.dRec) > 0 then
    if Tree.get_ve(x.Id,@v) then
    Node.Flags:=v.bits
  end;

  Result:=Node.dRec.Tag
end;

function tdm_Map.Get_Object(p: longint; out dRec: dm_Rec): Integer;
var
  rec: tdmRec;
begin
  if Tree.dm_lib > 0 then begin

    Tree.vm_dmw.Get_Object(p,rec,nil);

    FillChar(dRec,SizeOf(dRec),0);
    dRec.Code:=rec.Code; dRec.Tag:=rec.Loc;
    dRec.View:=rec.Level; dRec.Color:=rec.Color;
    dRec.ox1:=rec.ox1; dRec.oy1:=rec.oy1;
    dRec.ox2:=rec.ox2; dRec.oy2:=rec.oy2;
    dRec.mind:=p; dRec.hind:=p;
  end else

  if p = 0 then
    Tree._Info(Tree.Root,dRec,SizeOf(dm_Rec))
  else
    Tree._Info(p,dRec,SizeOf(dm_Rec));

  Result:=dRec.Tag
end;

procedure tdm_Map.Put_Object(p: longint; var dRec: dm_Rec);
begin
  if p > 0 then
  if Tree.dm_lib = 0 then
  Tree.NodeInfo(p,dRec,SizeOf(dm_Rec))
end;

function tdm_Map.xGet_Object(p: Int64; out dRec: dm_Rec): Integer;
var
  x: TInt64; v: tcn_rec; lt,rb: lxyz;
begin
  x.x:=p;

  Fillchar(dRec,SizeOf(dRec),0);

  if x.cn = cn_object then
    Result:=Get_Object(x.id,dRec)
  else
  if x.cn = cn_node then begin

    if Tree.get_vc(x.Id,@v) then begin
      dRec.o_lt:=v.Pos; dRec.o_rb:=v.Pos;
      dRec.Tag:=1;
    end

  end else
  if x.cn = cn_edge then begin

    if Tree.get_ve(x.Id,@v) then
    if Bound_ve(x.Id, lt.v,rb.v) then begin
      dRec.o_lt:=lt.p; dRec.o_rb:=rb.p;
      dRec.Tag:=2; dRec.mind:=v.sgp
    end

  end else
  if x.cn = cn_aggr then begin

    if Tree.get_fc(x.Id,@v) then begin
      dRec.Code:=v.OBJL; dRec.Tag:=255;
      dRec.mind:=v.mfp; dRec.hind:=v.hfp;
    end

  end;

  Result:=dRec.Tag
end;

procedure tdm_Map.xPut_Object(p: Int64; var dRec: dm_Rec);
var
  x: TInt64; v: tcn_rec; lt,rb: VPoint;
begin
  x.x:=p;

  if x.cn = cn_object then
    Put_Object(x.id,dRec)
  else
  if x.cn = cn_node then begin

    if Tree.get_vc(x.Id,@v) then begin
      v.Pos.x:=dRec.ox1;
      v.Pos.y:=dRec.oy1;
      Tree.put_vc(x.Id,@v)
    end

  end else
  if x.cn = cn_edge then begin

    if Tree.get_ve(x.Id,@v) then begin
      v.sgp:=dRec.mind;
      Tree.put_ve(x.Id,@v)
    end

  end else
  if x.cn = cn_aggr then begin

    if Tree.get_fc(x.Id,@v) then begin
      v.OBJL:=dRec.Code;
      v.mfp:=dRec.mind;
      v.hfp:=dRec.hind;
      Tree.put_fc(x.Id,@v)
    end

  end;
end;

function tdm_Map.Port_Object(p: Int64; out lt,rb: TPoint): Integer;
var
  dRec: dm_Rec;
begin
  Result:=xGet_Object(p,dRec);
  lt.x:=Min(dRec.ox1,dRec.ox2);
  lt.y:=Min(dRec.oy1,dRec.oy2);
  rb.x:=Max(dRec.ox1,dRec.ox2);
  rb.y:=Max(dRec.oy1,dRec.oy2)
end;

function tdm_Map.Port_Contains(const lt,rb: TPoint;
                               p: Int64; d: Integer): Boolean;
var
  dRec: dm_Rec; dx,dy: Integer;
begin
  Result:=false; dx:=d; dy:=d;

  if xGet_Object(p,dRec) in [1..7,21..23] then
  with dRec do begin

    if Tag = 1 then
      o_rb:=o_lt
    else
    if Tag = 4 then
    if (o_lt.X = o_rb.X)
    or (o_lt.Y = o_rb.Y) then
      dx:=d * 32;

    if (o_lt.X <= rb.X + dx) and (o_rb.X >= lt.X-dx) then
    if (o_lt.Y <= rb.Y + dy) and (o_rb.Y >= lt.Y-dy) then
    Result:=true
  end
end;

function tdm_Map.Get_Flags(p: Int64): Integer;
var
  Node: dm_Node; x: TInt64; v: tcn_rec;
begin
  Result:=0; x.x:=p;

  if x.cn = cn_object then begin
    Get_Node(p,Node); Result:=Node.Flags
  end else
  if x.cn = cn_node then begin
    if Tree.get_vc(x.id,@v) then
    Result:=v.bits and fl_xyz
  end else
  if x.cn = cn_edge then begin
    if Tree.get_ve(x.id,@v) then
    Result:=v.bits and (fl_xyz + fl_long)
  end
end;

function tdm_Map.xGet_Flags(p: longint): Integer;
begin
  Result:=(Get_Flags(Tree.Root) and 15) or Get_Flags(p)
end;

function tdm_Map.Up_Flag(p,fl: longint): Boolean;
begin
  Result:=Get_Flags(p) and fl <> 0
end;

function tdm_Map.Set_Flag(p,bit: longint; up: Boolean): Integer;
begin
  Result:=Get_Flags(p);

  if Update_Map then begin
    if up then Result:=Result or bit else
    Result:=Result and (bit xor $FF);
    Put_Flags(p,Result)
  end
end;

function tdm_Map.Xor_Flag(p,bit: longint): Integer;
begin
  Result:=Get_Flags(p) xor bit;
  Put_Flags(p,Result)
end;

procedure tdm_Map.Put_Flags(p,flags: longint);
begin
  Tree.NodeFlags(p,flags)
end;

procedure tdm_Map.xPut_Flags(p: Int64; flags: Integer);
var
  x: TInt64; v: tcn_rec;
begin
  x.x:=p;

  if x.cn = cn_object then
    Tree.NodeFlags(p,flags)
  else
  if x.cn = cn_node then begin
    if Tree.get_vc(x.id,@v) then begin
      v.bits:=flags; Tree.Put_vc(x.id,@v)
    end
  end else
  if x.cn = cn_edge then begin
    if Tree.get_ve(x.id,@v) then begin
      v.bits:=flags; Tree.Put_ve(x.id,@v)
    end
  end
end;

function tdm_Map.flag_del(p: Int64): Boolean;
var
  x: TInt64; fe: tfe_id;
begin
  Result:=false; x.x:=p;

  if Enabled_Map then

  if x.cn = cn_object then
    Result:=(xGet_Flags(p) and fl_del) = 0
  else
  if x.cn = cn_edge then
    Result:=Tree.Get_fe(x.id,0,@fe,0) = 0
  else
  if x.cn = cn_node then
    Result:=Tree.Is_free_node(x.id)
end;

function tdm_Map.Is_delete(p: Int64): Boolean;
begin
  Result:=false; if flag_del(p) then
  Result:=Update_Map
end;

function tdm_Map.flag_attr(p: Int64): Boolean;
begin
  Result:=false; if Update_Map then
  if __int64(p).cn = cn_object then
  Result:=(xGet_Flags(p) and fl_attr) = 0
end;

function tdm_Map.edit_mf(p: Int64): Boolean;
begin
  Result:=false; if Update_Map then
  if __int64(p).cn <> cn_object then Result:=true
  else Result:=(xGet_Flags(p) and fl_mf) = 0
end;

function tdm_Map.edit_hf(p: Int64): Boolean;
var
  x: TInt64;
begin
  Result:=false; x.x:=p;

  if Update_Map then

  if x.cn = cn_object then
    Result:=(xGet_Flags(p) and fl_hf) = 0
  else
    Result:=x.cn = cn_aggr
end;

function tdm_Map.Editing_attr(p: Int64): Boolean;
begin
  Result:=false; if CanModify then
  if __int64(p).cn = cn_object then
  Result:=(xGet_Flags(p) and fl_attr) = 0
end;

function tdm_Map.Editing_mf(p: Int64): Boolean;
begin
  Result:=false; if CanModify then
  if __int64(p).cn <> cn_Object then Result:=true
  else Result:=(xGet_Flags(p) and fl_mf) = 0
end;

function tdm_Map.Editing_hf(p: Int64): Boolean;
var
  x: TInt64;
begin
  Result:=false; x.x:=p;

  if CanModify then
  if x.cn = cn_object then
    Result:=(xGet_Flags(p) and fl_hf) = 0
  else
    Result:=x.cn = cn_aggr
end;

function tdm_Map.Editing_geo(p: Int64): Integer;
begin
  Result:=0; if Editing_mf(p) then
  Result:=xLoc_Object(p)
end;

function tdm_Map.flag_draw(p: longint): Boolean;
begin
  Result:=(xGet_Flags(p) and fl_draw) = 0
end;

function tdm_Map.Ok_Object(p: Int64): Boolean;
var
  dRec: dm_Rec; loc: int; v: tcn_rec; x: TInt64;
begin
  Result:=false; x.x:=p;

  if Enabled_Map then

  if Tree.dm_lib > 0 then
    Result:=true
  else
  if x.cn = cn_object then begin

    if p > 0 then
    if p >= Tree.Root then
    if p+SizeOf(dm_Rec) < SF.vm_Ind then

    if Tree._Length(p) = SizeOf(dm_Rec) then begin
      loc:=dm_Transit_loc(Get_Object(p,dRec));
      if loc in [0..8,10..13,21..23] then
      if (Get_Flags(p) and fl_skip) = 0 then
      Result:=true
    end
  end else

  if x.cn = cn_node then
    Result:=Tree.get_vc(x.Id,@v)
  else
  if x.cn = cn_edge then
    Result:=Tree.get_ve(x.Id,@v)
  else
  if x.cn = cn_aggr then
    Result:=Tree.get_fc(x.Id,@v)
end;

function tdm_Map.Identify(p: Int64; id: uint): Int64;
begin
  Result:=0;

  if Ok_Object(p) then begin

    if id = 0 then
      Result:=p
    else
    if Get_id(p) = id then
      Result:=p
    else
      Result:=Id_to_Offset(Self,id)

  end else

  if id > 0 then
    Result:=Id_to_Offset(Self,id)
end;

function tdm_Map.Identify1(p: Int64; id: uint): Int64;
begin
  Result:=0;

  if Ok_Object(p) then begin
    if id = 0 then
      Result:=p
    else
    if Get_id(p) = id then
      Result:=p
  end else

  if id > 0 then
    Result:=Offset_by_Id(id)
end;

function tdm_Map.Identify2(p: Int64; id: uint): Int64;
begin
  Result:=0;

  if Ok_Object(p) then
    Result:=p
  else
  if id > 0 then
    Result:=Offset_by_Id(id)
end;

function tdm_Map.Rect_Object(p: Int64; out lt,rb: TPoint): Boolean;
var
  x: TInt64; lt1,rb1: lxyz; dRec: dm_Rec; a,b: TPoint;
begin
  Result:=false; x.x:=p;

  if Ok_Object(p) then

  if (x.cn <> cn_object)
  or (x.id > Tree.Root) then

  if x.cn = cn_aggr then begin

    if fc_Get_Bound(x.Id, lt1,rb1) then begin
      lt:=lt1.p; rb:=rb1.p; Result:=true
    end

  end else

  if xGet_Object(p,dRec) > 0 then
  if Get_Poly_Cnt(p) >= 0 then begin

    a:=dRec.o_lt; b:=dRec.o_rb;

    if dRec.Tag = 1 then b:=a else begin
      if a.X > b.X then iSwap(a.X,b.X);
      if a.Y > b.Y then iSwap(a.Y,b.Y);
    end;

    lt:=a; rb:=b; Result:=true
  end
end;

function tdm_Map.xRect_Object(p: Int64; out lt,rb: TPoint): Boolean;
begin
  Result:=false; if p > 0 then
  Result:=Rect_Object(p,lt,rb)
end;

function tdm_Map.Object_contains(p: Int64; X,Y: int): Boolean;
var
  lt,rb: TPoint; r: int;
begin
  Result:=false; if p > 0 then
  if Rect_Object(p, lt,rb) then begin
    r:=Round(dm_dist(1,1));
    Result:=FrameContainsPoint(lt,rb, X,Y,r)
  end
end;

function tdm_Map.xRestore_Color(p: int;
                                out oColor: int;
                                only_idc: longbool): bool;
var
  inf: TInfoList;
  i,code,loc,nn,v,rc: int; cl,old: tlong;
  dNode: dm_Node; oRec: Obj_Rec;
  iRec: TIdcObjRec; vRec: TIdcValRec;
  s: TShortstr;
begin
  Result:=false; oColor:=0;

  if p > 0 then
  if Open_Objects then begin

    Get_Node(p,dNode);

    code:=dNode.dRec.Code;
    loc:=Transit_loc(dNode.dRec.Tag);

    if Obj.Obj_Index(code,loc,oRec,nil) > 0 then

    if (oRec.idcInd > 0)
    or not only_idc then begin

      old.i:=dNode.dRec.Color;
      cl.i:=oRec.Color;

      if loc in [1,2,3] then begin
        cl.w[1]:=oRec.Pen;
        old.w[1]:=dNode.Tag
      end;

      if oRec.idcInd > 0 then begin

        inf:=TInfoList.Create;
        try
          if Get_Info(p,inf) > 0 then
          if Idc.Open(Obj.vm_Path) > 0 then begin

            if Idc.seekObj(oRec.idcInd-1,code,loc,iRec) >= 0 then begin

              Fillchar(vRec,Sizeof(vRec),0);

              for i:=0 to 3 do begin
                nn:=iRec.nn[i];
                if nn = 0 then Break;
                if inf.xGet_int(nn,v) then
                  vRec.val[i].v1:=v
                else
                if inf.xGet_str(nn,s) then begin
                  val(s,v,rc);
                  if rc = 0 then
                  vRec.val[i].v1:=v
                end
              end;

              if Idc.List.seek(@iRec.nn,vRec,
                               Self,inf,p,fOnExpr) >= 0 then
              cl.i:=vRec.color
            end;

            Idc.Close
          end
        finally
          inf.Free
        end
      end;

      oColor:=cl.i; Result:=true;
      if only_idc then Result:=cl.i <> old.i
    end;

    Obj.vm_Close
  end
end;

function tdm_Map.Restore_Color(p: int): int;
var
  cl: int;
begin
  Result:=0;
  if xRestore_Color(p,cl,false) then begin
    Update_Color(p,cl);
    Result:=cl
  end
end;

function tdm_Map.Update_Code1(ptr: Int64; code: int): bool;
begin
  Result:=false;
  if __int64(ptr).cn = 0 then
  if Update_Map then begin
    Update_Code(ptr,code);
    Restore_Color(ptr);
    Result:=true
  end
end;

procedure tdm_Map.Update_Code(p, Code: Integer);
var
  dRec: dm_Rec;
begin
  Get_Object(p,dRec);
  dRec.Code:=Code;
  Put_Object(p,dRec)
end;

function tdm_Map.Update_Loc(p, Loc: int): bool;
var
  dRec: dm_Rec;
begin
  Result:=false;

  if Loc > 0 then
  if Update_Map then
  if Get_Object(p,dRec) <> Loc then begin
    dRec.Tag:=Loc; Put_Object(p,dRec);
    Result:=true
  end
end;

procedure tdm_Map.Update_Code_Color(p, code, cl: Integer);
var
  dRec: dm_Rec;
begin
  Update_Code(p,code);

  Get_Object(p,dRec);
  dRec.Color:=tlong(cl).w[0];
  Put_Object(p,dRec);

  if dRec.Tag <> 4 then
  Tree.NodeTag(p,tlong(cl).b[2])
end;

procedure tdm_Map.Update_Color(p, cl: Integer);
var
  dRec: dm_Rec; up: Integer;
begin
  if Tag_Object(p) = 4 then begin
    Get_Object(p,dRec);
    dRec.Color:=tlong(cl).w[0];
    Put_Object(p,dRec);

    up:=(Tree._Tag(p) and $3F) or
        (tlong(cl).b[2] and $C0);

    Tree.NodeTag(p,up)
  end
  else Update_cl(p,cl)
end;

procedure tdm_Map.Update_cl(p, cl: Integer);
var
  dRec: dm_Rec;
begin
  Get_Object(p,dRec);
  dRec.Color:=tlong(cl).w[0];
  Put_Object(p,dRec);

  Tree.NodeTag(p,tlong(cl).b[2])
end;

procedure tdm_Map.Update_Level(p, View: Integer);
var
  dRec: dm_Rec;
begin
  Get_Object(p,dRec);
  dRec.View:=View;
  Put_Object(p,dRec)
end;

procedure tdm_Map.Update_Up(p, Up: Integer);
begin
  Tree.NodeTag(p,Up)
end;

procedure tdm_Map.Update_Text(p: longint; str: PWideChar);
var
  stm: TInfoStream; s: ShortString;
begin
  if str <> nil then
  if Update_Map then begin

    stm:=TInfoStream.Create(hf_x16);
    try
      load_hf(p,stm);

      if is_dos_wide(str,s) then
          stm.Put_hf(9,_string,s)
      else
        stm.Put_hf(9,_unicode,str^);

      update_hf(p,stm)
    finally
      stm.Free
    end;

  end
end;

procedure tdm_Map.Update_Ansi(p: longint; s: PChar);
var
  stm: TInfoStream; t: string;
begin
  if s <> nil then
  if Update_Map then begin

    stm:=TInfoStream.Create(hf_x16);
    try
      load_hf(p,stm);

      t:=StrPas(s);
      stm.Put_hf(9,_string,t);

      update_hf(p,stm)
    finally
      stm.Free
    end;

  end
end;

function tdm_Map.Update_Poly(var mf_ind,flags: longint;
                             lp: PLLine; hp: PIntegers): Boolean;

function pack_Poly(var mf_ind: longint; lp: PLLine): Boolean;
var
  i,lp_n, di,len, step, dx,dy,dk: Integer;
  p1,p2: TPoint; dp: IPoint;
begin
  Result:=false; di:=mf_ind; len:=0;

  for step:=1 to 2 do
  if len <= 64000 then begin

    if len > 0 then
    if mf.vmx_UpdateBuf(di,lp.Pol,len) then begin
      mf_ind:=di; Result:=true;
      di:=mf.vmx_Buffer(di)+8;
    end else Break;

    i:=0; p2:=lp.Pol[0]; lp_n:=lp.N;

    while i <= lp.N do begin
      p1:=p2; p2:=lp.Pol[i]; Inc(i);

      dx:=Abs(p2.X-p1.X); dy:=Abs(p2.Y-p1.Y);

      if (dx >= $7FFF) or (dy >= $7FFF) then begin
        dk:=Max(dx div $7FFF,dy div $7FFF)+1;
        p2.X:=p1.X+(p2.X-p1.X) div dk;
        p2.Y:=p1.Y+(p2.Y-p1.Y) div dk;
        Dec(i); Inc(lp_n)
      end;

      if step = 2 then begin
        dp.X:=p2.X-p1.X; dp.Y:=p2.Y-p1.Y;
        di:=mf.vm_Store(di,dp,4)
      end
    end;

    len:=(lp_n+3)*4
  end
end;

var
  i,h1,h2,h, ind,len,xyz,maxLen: int;
  p1,p2: TPoint; dp: IPoint; pack: Boolean;
begin
  Result:=false;

  ind:=mf_ind; flags:=flags and $1F;

  pack:=false; p2:=lp.Pol[0]; h2:=0;
  if Assigned(hp) then h2:=hp[0];

  for i:=1 to lp.N do begin
    h1:=h2; if Assigned(hp) then h2:=hp[i];
    p1:=p2; p2:=lp.Pol[i]; pack:=true;

    if (Abs(h2-h1) >= $7FFF)
    or (Abs(p2.x-p1.x) >= $7FFF)
    or (Abs(p2.y-p1.y) >= $7FFF) then
    begin pack:=false; Break end
  end;

  maxLen:=64000;
  if mf.vm_x32 then maxLen:=64000*8;

  with lp^ do

  if pack then begin
    xyz:=0; if Assigned(hp) then
    xyz:=SizeOf(Integer) + (N+1)*SizeOf(SmallInt);
    len:=SizeOf(TPoint) + (N+1)*SizeOf(IPoint);

    if len+xyz > maxLen then xyz:=0;

    if len <= maxLen then
    if mf.vmx_ResizeBuf(ind, len+xyz) then begin

      mf_ind:=ind; Result:=true;

      ind:=mf.vmx_Buffer(ind);
      ind:=mf.vm_Store(ind,Pol[0],8);

      p2:=Pol[0];
      for i:=0 to N do begin
        p1:=p2; p2:=Pol[i];
        dp.X:=p2.X-p1.X; dp.Y:=p2.Y-p1.Y;
        ind:=mf.vm_Store(ind,dp,4)
      end;

      if xyz > 0 then begin h2:=hp[0];
        ind:=mf.vm_Store(ind,h2,SizeOf(h2));
        for i:=0 to N do begin
          h1:=h2; h2:=hp[i]; h:=h2-h1;
          ind:=mf.vm_Store(ind,h,2)
        end; flags:=flags or fl_xyz
      end
    end
  end
  else begin
    len:=(N+1) * SizeOf(TPoint);
    xyz:=0; if Assigned(hp) then
    xyz:=(N+1) * SizeOf(Integer);

    if len+xyz > maxLen then begin
      if pack_enabled then
      Result:=pack_Poly(mf_ind,lp);
      fExceptCode:=1
    end else
    if mf.vmx_ResizeBuf(ind,len+xyz) then begin

      mf_ind:=ind; Result:=true;

      ind:=mf.vmx_Buffer(ind);
      ind:=mf.vm_Store(ind,Pol,len);

      flags:=flags or fl_long;

      if xyz > 0 then begin
        mf.vm_Store(ind,hp^,xyz);
        flags:=flags or fl_xyz
      end
    end
  end
end;

function tdm_Map.old_mf(p: longint;
                        var dRec: dm_Rec;
                        lp: PLLine; hp: PIntegers): Boolean;
var
  lt,rb: TPoint; flags: int;
begin
  Result:=false;

  if dRec.Tag in [2,3] then
  if mf.vm_x32 then begin

    mf.vm_x32:=false;
    if mf.vm_Domain(dRec.mind) then
    if xGet_Poly(p,lp,hp,LPolyMax) > 0 then begin

      mf.vm_x32:=true;

      with lp^ do
      Max_Poly_Bound(@Pol,N+1, lt,rb);

      if Points_Equal(lt,dRec.o_lt) then
      if Points_Equal(rb,dRec.o_rb) then begin

        flags:=Get_Flags(p) and $1F;

        if flags and fl_xyz <> 0 then
        if hp = nil then flags:=flags xor fl_xyz;

        dRec.mind:=0; Put_Object(p,dRec);

        if Update_Poly(dRec.mind,flags, lp,hp) then begin
          Put_Object(p,dRec);
          Put_Flags(p,flags);
          Result:=true
        end
      end
    end;

    mf.vm_x32:=true
  end
end;

function tdm_Map.Update_xyz(p,loc: Integer;
                            lp: PLLine; hp: PIntegers): Boolean;

function hp_empty(hp: PIntegers; n: Integer): Boolean;
var
  z1,z2: Integer;
begin
  Result:=true;
  if Assigned(hp) then begin
    int_MinMax(hp,n+1, @z1,@z2);
    Result:=z1 = z2
  end
end;

function Update_curve(var mf_ind,flags: longint;
                      lp: PLLine; hp: PIntegers): Boolean;
var
  ind,len,xyz: Integer;
begin
  Result:=false; ind:=mf_ind;

  with lp^ do begin
    len:=(N+1)*Sizeof(TPoint);

    xyz:=0; if not hp_empty(hp,N) then
    xyz:=(N+1)*Sizeof(Integer);

    if mf.vmx_ResizeBuf(ind,len+xyz) then begin

      mf_ind:=ind; Result:=true;

      ind:=mf.vmx_Buffer(ind);
      ind:=mf.vm_Store(ind,Pol,len);

      flags:=flags or fl_long;

      if xyz > 0 then begin
        mf.vm_Store(ind,hp^,xyz);
        flags:=flags or fl_xyz
      end
    end
  end
end;

var
  i,di,flags: int; dRec: dm_Rec; R: LOrient;
begin
  Result:=false;

  if Assigned(lp) then
  if lp.N >= 0 then begin

    Get_Object(p,dRec);
    if loc > 0 then dRec.Tag:=loc;
    loc:=dm_transit_loc(dRec.Tag);

    if loc in [1..5,8,11] then
    lp.N:=dup_LPoly(@lp.Pol,hp,lp.N,false);

    flags:=Get_Flags(p) and $1F;

    with dRec do case loc of

  1:  with lp^ do begin

        i:=0;
        if N = 1 then i:=1 else
        if N = 3 then i:=2;

        o_lt:=Pol[0]; o_rb:=Pol[i];

        if Assigned(hp) then begin
          if mf.vmx_UpDateBuf(mind,Pol,(N+1)*12) then begin
            di:=mf.vmx_Buffer(mind)+(N+1)*8;
            mf.vm_Store(di,hp^,(N+1)*4);
            flags:=flags or fl_xyz; Result:=true
          end
        end else
        if N < 3 then begin
          mind:=mf.vmx_FreeBuf(mind); Result:=true
        end else
          Result:=mf.vmx_UpDateBuf(mind,Pol,4*8)

      end;
  2,
  3,
  4,
  11: with lp^ do
      if N >= dm_min[loc] then
      if (loc <> 3) or PolyLock(lp) then begin

        Max_Poly_Bound(@Pol,N+1, o_lt,o_rb);

        if Tag = 3 then
        if dm_Polygon_Square then
        if Square_Poly(@Pol,N) > 0 then
        Swap_LPoly(@Pol,hp,N);

        Result:=Update_Poly(mind,flags, lp,hp)
      end;

  5:  with lp^ do
      if Ellipse_Bound(lp,R) then begin
        if N > 1 then N:=2;
        Max_Poly_Bound(@R,4,o_lt,o_rb);
        Result:=mf.vmx_UpdateBuf(mind,Pol,(N+1)*8)
      end;

  6,
  7,
  8:  with lp^ do
      if N >= dm_min[loc] then
      if (loc <> 7) or CurveLock(lp) then begin
        Max_Poly_Bound(@Pol,N+1, o_lt,o_rb);
        Result:=Update_curve(mind,flags,lp,hp)
      end;

  21,
  22,
  23: with lp^ do
      Result:=mf.vmx_UpdateBuf(mind,Pol,(N+1)*8);

    end;

    if Result then begin
      Put_Object(p,dRec);
      Put_Flags(p,flags);
      if dm_AutoSave then
      Auto_Map(false)
    end
  end
end;

function tdm_Map.xUpdate_mf(p: Int64; loc: Integer;
                            lp: PLLine; hp: PIntegers): Boolean;
var
  x: TInt64;
begin
  Result:=false; x.x:=p;

  if Update_Map then

  if x.cn = 0 then
    Result:=Update_xyz(x.id,loc, lp,hp)
  else
  if x.cn = cn_node then begin
    if lp.N = 0 then
    Update_vc(x.id,lp.Pol[0],Pointer(hp))
  end else
  if x.cn = cn_edge then
    Update_ve(x.Id,0,0, lp,hp)
end;

procedure tdm_Map.Update_mf(p,loc: Integer; lp: PLLine);
begin
  Update_xyz(p,loc, lp,nil)
end;

function tdm_Map.Get_mesh(p: Integer; mesh: TPolyMesh): Boolean;
var
  dRec: dm_Rec; bx,cx,cx1: Longint;
  s: TPoint; vp: PBytes; fp: PIntegers;
begin
  Result:=false; mesh.Clear;

  if Get_Object(p,dRec) = 33 then
  if dRec.mind > 0 then begin

    bx:=mf.vmd_Buffer(dRec.mind);
    cx:=mf.vmd_BufLen(dRec.mind);

    if cx > 8 then begin
      bx:=mf.vm_Load(bx,s,8); Dec(cx,8);

      if s.X >= 3 then
      if s.Y <= cx then
      if s.Y <= s.X*12 then

      if mesh.Points.Resize(s.X) then begin
        vp:=mesh.Points.First;

        cx1:=s.X*12;
        if s.Y = cx1 then
          bx:=mf.vm_Load(bx,vp^,s.Y)
        else begin
          vp:=@vp[cx1-s.Y];
          bx:=mf.vm_Load(bx,vp^,s.Y);
          mesh.Points.Unpack(vp)
        end;

        Dec(cx,s.Y);

        bx:=mf.vm_Load(bx,s,8); Dec(cx,8);

        if s.X >= 3 then
        if s.Y <= cx then
        if s.Y = s.X*4 then

        if mesh.Faces.Resize(s.X) then begin
          fp:=mesh.Faces.First;
          mf.vm_Load(bx,fp^,s.Y);

          Result:=mesh.Verify_faces >= 3
        end
      end
    end
  end
end;

function tdm_Map.Update_mesh(p: Integer; mesh: TPolyMesh): Boolean;
var
  dRec: dm_Rec;
  vp: PVPoly; vn: Integer;
  fp: PIntegers; fn: Integer;
  bx,cx,cx1,cx2,flags: Integer;
  lt,rb: lxyz;
begin
  Result:=false;

  if mf.vm_Active then

  if mesh.Points.Count >= 3 then
  if mesh.Verify_faces >= 3 then begin

    Get_Object(p,dRec);
    flags:=Get_Flags(p) and $1F;

    if dRec.Tag = 33 then begin

      mesh.Points.Get_min_max(lt.v,rb.v);
      dRec.o_lt:=lt.p; dRec.o_rb:=rb.p;

      vp:=mesh.Points.First;
      vn:=mesh.Points.Count;

      fp:=mesh.Faces.First;
      fn:=mesh.Faces.Count;

      cx1:=vn*12; cx2:=fn*4;

      if mesh.Points.IsPack then
        cx1:=mesh.Points.Pack
      else
        flags:=flags or fl_long;

      flags:=flags or fl_xyz;

      cx:=(4+4+cx1)+(4+4+cx2);
      bx:=mf.vmd_ResizeBuf(dRec.mind,cx);
      if bx > 0 then begin
        dRec.mind:=bx;
        bx:=mf.vmd_Buffer(bx);

        bx:=mf.vm_Store(bx,vn,4);
        bx:=mf.vm_Store(bx,cx1,4);
        bx:=mf.vm_Store(bx,vp^,cx1);

        bx:=mf.vm_Store(bx,fn,4);
        bx:=mf.vm_Store(bx,cx2,4);
        bx:=mf.vm_Store(bx,fp^,cx2);

        Put_Object(p,dRec);
        Put_Flags(p,flags);
        Auto_Map(false);

        Result:=true
      end
    end
  end
end;

function tdm_Map.Get_next_nnnn: Cardinal;
begin
  Result:=Get_id(Tree.Root)+1;
  Result:=Max(Result, Tree.fc_index.Max_id);
end;

function tdm_Map.Gen_next_nnnn: Cardinal;
begin
  Result:=Get_next_nnnn;
  xPut_Int(Tree.Root,1000,Result)
end;

function tdm_Map.IntLink(p: Integer): Integer;
var
  id: Integer;
begin
  if not xGet_Int(p,1000,id) then begin
    id:=Assign_Link(p,0); if id > 0 then
    if fIndex.Count > 0 then fIndex.id_Add(id,p);
  end; Result:=id
end;

function tdm_Map.StrLink(p: Integer; s: PChar): PChar;
var
  dRec: dm_Rec; id: Integer;
begin
  Get_Object(p,dRec); id:=IntLink(p); with dRec do
  Result:=StrFmt(s,'%d %s %d',[id,CodeToStr(Code),Tag])
end;

function tdm_Map.Assign_link(p,id: Integer): Integer;
var
  _p: Integer;
begin
  Result:=0;

  if Enabled_Map then begin

    if id > 0 then
    if Get_id(p) = id then
    Result:=id;

    if Result = 0 then
    if Update_Map then begin

      if id <> 0 then
      if Offset_by_Id(id) > 0 then
      id:=Release_link(id);

      if id = 0 then id:=Gen_next_nnnn;

      if p > 0 then begin
        xPut_Int(p,1000,id);

        if Get_id(p) = id then
        if fIndex.Count > 0 then
        fIndex.id_Add(id,p)
      end;

      Result:=id
    end
  end
end;

function tdm_Map.Update_link(p: longint): longint;
begin
  if xGet_Int(p,1000,Result) then
  Result:=Assign_Link(p,0)
end;

function tdm_Map.Release_link(id: Integer): Integer;
var
  p: Integer;
begin
  if Enabled_Map then begin
    p:=Tree.Root;

    if Get_id(p) = id then
    if Update_Map then
    xPut_Int(p,1000,Max(0,id-1));
  end;

  Result:=0
end;

function tdm_Map.Str_Object(s,cmd: PChar; p: longint): PChar;
var
  dRec: dm_Rec; fn: TShortStr;
begin
  Result:=nil;

  if Ok_Object(p) then
  if Get_Object(p,dRec) >= 0 then with dRec do begin

    Result:=StrFmt(s,'%s %d %s %d %d %d %d %d',
                     [xStrPas(cmd),p,
                     CodeToStr(Code),Tag,
                     ox1,oy1,ox2,oy2]);

    if Open_Objects then
    if Obj.Obj_Str(Code,Tag,fn) <> nil then
    StrCat(StrCat(StrCat(s,' "'),fn),'"');

    Obj.vm_Close
  end
end;

function tdm_Map.Poly_Calci(loc: Integer;
                            lp: PLLine; hp: PIntegers): txyz;
var
  s: Double; l: TGauss;
begin
  Result:=_xyz(-1,-1,-1);

  if lp.N > 0 then begin

    if loc in [6..7] then begin
      l.x:=LG_T.Curve_Length(lp,s);

      if LG_T.poly_gk(@lp.Pol,lp.N) then
      l.x:=LG_T.Poly_Length(lp);

      Result.x:=l.x;

      if CurveLock(lp) then
      Result.y:=Abs(s)
    end
    else begin
      l.x:=LG_T.Poly_Length(lp);

      if LG_T.poly_gk(@lp.Pol,lp.N) then
      l.x:=LG_T.Poly_Length(lp);

      Result.x:=l.x;

      if loc = 2 then
      if Assigned(hp) then begin
        l:=LG_T.xyz_Length(lp,hp,z_res);
        Result.x:=l.y; Result.z:=l.x;
      end;

      s:=LG_T.Poly_Square(lp);

      if PolyLock(lp) then
      Result.y:=Abs(s)
    end;

    LG_T.gk_s.pps:=-1;
  end
end;

function tdm_Map.Poly_Calc(p: Int64;
                           lp: PLLine; hp: PIntegers;
                           lp_max: Integer): txyz;
var
  loc: Integer;
begin
  Result:=_xyz(-1,-1,-1);
  loc:=xTag_Object(p);

  if loc in [2..7] then
  if xGet_Poly(p,lp,hp,lp_max) > 0 then begin

    if loc = 5 then
    Ellipse_LLine(lp,lp_max,0.8);

    if not z_axe_Exist(p) then hp:=nil;
    Result:=Poly_Calci(loc,lp,hp);
  end
end;

function tdm_Map.Inside_Object(par,run: Integer): Boolean;
var
  Code: Integer;
begin
  Result:=false;
  if Tag_Object(par) = 3 then
  if Tag_Object(run) = 3 then begin
    Code:=Code_Object(run);
    if (Code = Code_Object(par))
    or is_Hole_Code(Code) then
    Result:=true
  end
end;

function tdm_Map.Up_Object(p: longint): longint;
var
  up: longint;
begin
  Result:=0;

  up:=Tree._Parent(p); if up > 0 then
  if up <> Tree.Root then begin

    up:=Tree._Parent(up); if up > 0 then
    if up <> Tree.Root then begin
      up:=Layer_by_Code(Code_Object(p));
      Tree.Dn_Node(p,up); Result:=up
    end
  end
end;

procedure tdm_Map.Cls_Parent(par: longint);
var
  up_code,code, top,run,nxt: longint;
begin
  if par > 0 then with Tree do begin
    top:=_Child(par); if top <> 0 then begin

      up_code:=Code_Object(par);

      run:=_Link(top); while run <> top do begin
        nxt:=_Link(run); code:=Code_Object(run);

        if (code = up_code) or is_Hole_Code(code) then
        else Up_Object(run); run:=nxt
      end;

      Up_Childs(par)
    end
  end
end;

function tdm_Map.Load_hf(p: Int64; stm: thfStream): Integer;
var
  dRec: dm_Rec;
begin
  Result:=0;
  if p > 0 then begin
    xGet_Object(p,dRec); stm.x16:=hf_x16;
    Result:=hf.Stream_Copy(dRec.hind,stm)
  end;
end;

procedure tdm_Map.Update_hf(p: Int64; stm: TStream);
var
  dRec: dm_Rec;
begin
  if p > 0 then
  if Update_Map then begin
    xGet_Object(p,dRec);

    if stm = nil then
      dRec.hind:=hf.vm_FreeBuf(dRec.hind)
    else
      dRec.hind:=hf.Stream_Paste(dRec.hind,stm);

    xPut_Object(p,dRec)
  end;
end;

function tdm_Map.Get_Info(p: Int64; Info: TInfoList): Integer;

procedure inf_attv(Info: TInfoList; attv: Cardinal);
var
  i,v: Integer;
begin
  for i:=0 to 3 do begin
    v:=tlong(attv).b[i]; if v <> 255 then
    Info.Push_equ(400+i,_long,v)
  end
end;

var
  buf: thfStream; x: TInt64;
begin
  Info.Clear;
  Info.x16:=hf_x16;

  x.x:=p;
  if x.cn = cn_node then
    inf_attv(Info,Tree.Get_vc_attv(x.id))
  else
  if x.cn = cn_edge then
    inf_attv(Info,Tree.Get_ve_attv(x.id))
  else begin
    buf:=thfStream.Create(hf_x16);
    try
      if Load_hf(p,buf) > 0 then
      Info.LoadFrom(buf)
    finally
      buf.Free
    end
  end;

  Result:=Info.Count
end;

function tdm_Map.Update_Info(p: Int64; Info: TInfoList): Boolean;
var
  buf: thfStream;
begin
  Result:=false;
  buf:=thfStream.Create(hf_x16);
  try
    if p > 0 then
    if Update_Map then begin
      Info.SaveTo(buf);
      Update_hf(p,buf);
      Result:=true
    end;
  finally
    buf.Free
  end;
end;

procedure tdm_Map.Clear_Info(p: Int64);
var
  hf: TInfoList; id: int;
begin
  hf:=TInfoList.Create;
  try
    if p > 0 then
    if Update_Map then begin
      id:=Get_id(p);
      hf.Push_equ(1000,_long,id);
      Update_Info(p,hf)
    end;
  finally
    hf.Free
  end;
end;

procedure tdm_Map.Copy_Info(dst_p,src_p: longint;
                            src_dm: tdm_Map; id_gen: Boolean);
var
  hf: TInfoList; i,id: int;
begin
  hf:=TInfoList.Create;
  try
    if Update_Map then begin

      src_dm.Get_Info(src_p,hf);

      if Self <> src_dm then begin

        if dst_p = Tree.Root then begin
          hf.xUpdate_int(999,1);
          hf.Delete_nn(808);
        end

      end else
      if id_gen then begin
        i:=hf.Contains_nn(1000);
        id:=Gen_next_nnnn;

        if i >= 0 then
          hf.Update_int(i,id)
        else
          hf.Push_equ(1000,_long,id)
      end;

      Update_Info(dst_p,hf);
    end;

   finally
    hf.Free
  end
end;

procedure tdm_Map.Free_Object(p: longint);
var
  dRec: dm_Rec;
begin
  Get_Object(p,dRec);
  hf.vm_FreeBuf(dRec.hind);
  mf.vmx_FreeBuf(dRec.mind);
  sf.vm_FreeMem(p,SizeOf(dm_Node));
end;

procedure tdm_Map.Edit_Object(p: Int64; id,cmd: longint);
var
  x: TInt64;
begin
  if Assigned(fOnEditObject) then begin
    if (id = 0) and (cmd <> tx_edit_del) then
    if Enabled_Map then id:=Get_id(p);
    fOnEditObject(p,id,cmd)
  end;

  x.x:=p; if cmd = tx_edit_del then
  if x.cn in [cn_object,cn_aggr] then begin
    if x.cn = cn_object then x.id:=Get_id(p);
    fc_fe_Exclude(x.id);
  end
end;

function tdm_Map.cn_add(RCNM,RCID,RVER,RUIN, P1,P2: Cardinal;
                        lp: PLLine; hp: PIntegers): Cardinal;
var
  r: tcn_rec; flags: Integer; x: TInt64;
begin
  Result:=0;

  FillChar(r,SizeOf(r),0);
  r.rcid:=RCID; r.rver:=RVER;
  r.rcnm:=RCNM; r.bits:=RUIN;

  if Assigned(hp) then
  r.bits:=r.bits or $80;

  if not Tree.is_cn then
  Tree.is_cn:=true;

  if Tree.is_cn then

  case RCNM of
110,
120:if lp.N = 0 then begin

      r.Pos:=lp.Pol[0];
      if Assigned(hp) then
      r.zpos:=hp[0];

      if Tree.Add_vc(@r) > 0 then
      Result:=r.rcid;
    end;

130:if p1 <> 0 then
    if p2 <> 0 then begin

      if r.rcid = 0 then
      if Assigned(fve_Identify) then
      r.rcid:=fve_Identify(0);

      if r.rcid = 0 then
      r.rcid:=Tree.ve_index.Max_id;

      r.vc1:=p1; r.vc2:=p2; flags:=0;

      r.bits:=r.bits and $0F;

      if Assigned(lp) and (lp.N >= 0) then
      if Update_Poly(r.sgp,flags, lp,hp) then
      r.bits:=r.bits or flags;

      if Tree.Add_ve(@r) > 0 then
      Result:=r.rcid;

      if Result > 0 then
      if Assigned(fve_Identify) then
      fve_Identify(Result)

    end;
  end;

  if Result > 0 then
  if Assigned(FOnEditObject) then begin
    x.id:=r.rcid; x.cn:=r.rcnm;
    fOnEditObject(x.x,0,tx_Edit_add)
  end
end;

function tdm_Map.add_vc(const p: TPoint; z: PInteger): Cardinal;
var
  v: VLLine; x: TInt64;
begin
  v.N:=0; v.Pol[0]:=p;

  Result:=cn_add(cn_node,0, 1,1, 0,0, @v,PIntegers(z));

  if Result > 0 then begin
    x.cn:=cn_node; x.id:=Result;
    Undo_Object(x.x,undo_add);
  end
end;

function tdm_Map.add_vc1(const p: TPoint; z: PInteger): Cardinal;
var
  v: VPoint;
begin
  v.x:=p.X; v.y:=p.Y; v.z:=0;
  if Assigned(z) then v.z:=z^;

  Result:=Tree.vc_Lookup(v);
  if Result = 0 then
  Result:=add_vc(p,z)
end;

function tdm_Map.vc_add(lp: PLLine; hp: PIntegers): Int64;
var
  p: TInt64;
begin
  Result:=0; p.cn:=cn_node;
  p.id:=cn_add(cn_node,0, 1,1, 0,0, lp,hp);

  if p.id > 0 then begin
    Undo_Object(p.x,undo_add); Result:=p.x
  end
end;

function tdm_Map.ve_add(vc1,vc2: Cardinal;
                        lp: PLLine; hp: PIntegers): Int64;
var
  p: TInt64;
begin
  Result:=0; p.cn:=cn_edge;
  p.id:=cn_add(cn_edge,0, 1,1,vc1,vc2, lp,hp);

  if p.id > 0 then begin
    Undo_Object(p.x,undo_add); Result:=p.x
  end
end;

function tdm_Map.Add_holes(ptr: int; poly: TPolyList;
                           lp: PLLine; hp: PIntegers): int;
var
  code,top,run: uint; dRec: dm_Rec;
begin
  Result:=0;

  Code:=Code_Object(ptr);

  top:=Tree._Child(ptr);
  if top > 0 then begin
    run:=Tree._Link(top);

    while run <> top do begin

      if Get_Object(run,dRec) = 3 then

      if (dRec.Code = Code)
      or Is_Hole_Code(dRec.Code) then

      if xGet_mf(run,lp,hp) > 0 then begin
        poly.add_poly(@lp.Pol,hp,lp.N+1);
        Inc(Result)
      end;

      run:=Tree._Link(run)
    end
  end
end;

function tdm_Map.Del_holes(ptr: int): Integer;
var
  code,top,run,nxt,prd: uint; dRec: dm_Rec;
  del: bool;
begin
  Result:=0;

  Code:=Code_Object(ptr);

  top:=Tree._Child(ptr);
  if top > 0 then begin
    run:=Tree._Link(top);

    prd:=top;
    while run <> top do begin

      nxt:=Tree._Link(run); del:=false;

      if Get_Object(run,dRec) = 3 then

      if (dRec.Code = Code)
      or Is_Hole_Code(dRec.Code) then begin
        Undo_Object(run,undo_del);
        xDel_Object(prd,run);
        Inc(Result); del:=true
      end;

      if not del then prd:=run;
      run:=nxt
    end
  end
end;

function tdm_Map.Undo_holes(ptr,cmd: int): Integer;
var
  code,top,run: uint; dRec: dm_Rec;
begin
  Result:=0;

  Code:=Code_Object(ptr);

  top:=Tree._Child(ptr);
  if top > 0 then begin
    run:=Tree._Link(top);

    while run <> top do begin

      if Get_Object(run,dRec) = 3 then

      if (dRec.Code = Code)
      or Is_Hole_Code(dRec.Code) then begin
        Undo_Object(run,cmd); Inc(Result);
      end;

      run:=Tree._Link(run)
    end
  end
end;

function tdm_Map.Get_holes(ptr: int;
                           list: TIntegerList;
                           poly: TPolylist;
                           buf: PLLine): int;
var
  code,top,run: int; dRec: dm_Rec;
begin
  list.Clear;

  if Assigned(poly) then poly.Clear;

  Code:=Code_Object(ptr);

  top:=Tree._Child(ptr);
  if top > 0 then begin
    run:=Tree._Link(top);

    while run <> top do begin

      if Get_Object(run,dRec) = 3 then

      if (dRec.Code = Code)
      or Is_Hole_Code(dRec.Code) then

      if poly = nil then
        list.AddItem(run)
      else
      if Get_Polybuf(run,buf) > 0 then begin
        poly.add_poly(@buf.Pol,nil,buf.N+1);
        list.AddItem(run)
      end;

      run:=Tree._Link(run)
    end
  end;

  Result:=list.Count
end;

function tdm_Map.recode_holes(ptr,code1: int): int;
var
  code,code2,loc,top,run,loop: int;
begin
  Result:=0;

  if Enabled_Map then begin

    loc:=xCode_Object(ptr,code);

    if Transit_loc(loc) = 3 then begin

      top:=Tree._Child(ptr);
      if top > 0 then

      for loop:=1 to 2 do begin

        run:=Tree._Link(top);
        while run <> top do begin

          loc:=xCode_Object(run,code2);
          if Transit_loc(loc) = 3 then

          if (code2 = code)
          or (code2 = code1)
          or Is_Hole_Code(code2) then begin
            if loop = 2 then
            Update_code(run,code);
            Inc(Result)
          end;

          run:=Tree._Link(run)
        end;

        if Result = 0 then Break;
        if loop = 2 then Break;

        Result:=0;
        if not Update_Map then Break
      end
    end
  end
end;

function tdm_Map.Nameof(Name: PChar): int;
var
  look: xLookChar;
begin
  Result:=0;

  look:=xLookChar.Create(Self, 0,0, 9,_string, 0,0,Name);
  try
    Result:=look.Scan_tree(true,true);
  finally
    look.Free
  end
end;

procedure tdm_Map.cn_attv(cn,id,attv: Cardinal; IsUpdate: Boolean);
begin
  if cn = cn_node then
    Tree.vc_update_attv(id,attv,IsUpdate)
  else
  if cn = cn_edge then
    Tree.ve_update_attv(id,attv,IsUpdate)
end;

function tdm_Map.cn_draw(p: Integer;
                         mf_buf: pmf_buff;
                         draw: TCallback_draw): Integer;

function mf_break(mf_buf: pmf_buff;
                  proc: TCallback_draw): Integer;
var
  p: TPoint;
begin
  with mf_buf^ do begin
    with lp^ do p:=Pol[N];
    Result:=proc(mf_buf);
    lp.N:=0; lp.Pol[0]:=p
  end
end;

function mf_vc(mf_buf: pmf_buff;
               proc: TCallback_draw;
               id: Integer): Integer;
var
  p: TPoint; vc: tcn_rec;
begin
  Result:=0;

  if Tree.get_vc(id,@vc) then

  with mf_buf^ do begin
    if lp.N = lp_Max then
    Result:=mf_break(mf_buf,proc);

    with lp^ do
    if N < lp_Max then begin
      Inc(N); Pol[N]:=vc.Pos;
      if Assigned(hp) then
      hp[N]:=vc.zpos
    end
  end
end;

var
  i,j,k,n,ind,len, ornt,rib_n: int;
  qc,pc,vc1,vc2: Cardinal; ve: tcn_rec;
  rib_lp: PLPoly; rib_hp: PIntegers;
  buf: TLPoly; dRec: dm_Rec; pp: TPoint;
begin
  Result:=0;

  Tree._Info(p,dRec,SizeOf(dRec));
  if dRec.mind > 0 then begin

    ind:=mf.vmx_Block(dRec.mind,len);
    n:=len div SizeOf(TPoint);

    i:=0; mf_buf.lp.N:=-1;

    qc:=0; pc:=0;
    while i < n do begin

      k:=Min(LPoly_Max+1,n-i); Inc(i,k);
      ind:=mf.vm_Load(ind,buf,k * SizeOf(TPoint));

      for j:=0 to k-1 do begin
        pp:=buf[j]; ornt:=tlong(pp.y).b[1];

        if Tree.get_ve(pp.x,@ve) then begin

          vc1:=ve.vc1; vc2:=ve.vc2; if ornt = 2 then
          begin vc1:=ve.vc2; vc2:=ve.vc1 end;

          if vc1 <> pc then
          Inc(Result,mf_vc(mf_buf,draw,vc1));

          if qc = 0 then qc:=vc1; pc:=vc2;

          with mf_buf^ do begin
            if lp.N = lp_Max then
            Result:=mf_Break(mf_buf,draw);

            if lp.N >= 0 then
            if lp.N < lp_Max then
            if ve.sgp > 0 then begin

              with lp^ do rib_lp:=@Pol[N+1];
              rib_hp:=hp; if Assigned(hp) then
              rib_hp:=@hp[N+1];

              rib_n:=Load_Poly(ve.sgp, rib_lp,rib_hp,
                               lp_max-lp.N-1,ve.bits);

              if rib_n < 0 then begin
                Inc(Result,mf_Break(mf_buf,draw));

                with lp^ do rib_lp:=@Pol[N+1];
                rib_hp:=hp; if Assigned(hp) then
                rib_hp:=@hp[N+1];

                rib_n:=Load_Poly(ve.sgp, rib_lp,rib_hp,
                                 lp_max-lp.N-1,ve.bits)
              end;

              if (ornt = 2) and (rib_n > 0) then
              Swap_LPoly(rib_lp,rib_hp,rib_n);

              Inc(lp.N,rib_n+1)
            end
          end;

          Inc(Result,mf_vc(mf_buf,draw,vc2));
        end;

        if (qc = vc2) or (ornt = 255) then begin
          Inc(Result,draw(mf_buf));
          qc:=0; pc:=0; mf_buf.lp.N:=-1;
          if mf_buf.is_break then Break;
        end
      end;

      if mf_buf.is_break then Break;
    end;

    if mf_buf.lp.N >= 0 then
    Inc(Result,draw(mf_buf))
  end
end;

function tdm_Map.cn_xdraw(p: Integer;
                          poly: TPolyList;
                          mf_buf: pmf_buff;
                          node: TCallback_node;
                          edge: TCallback_edge): Integer;

procedure mf_vc(poly: TPolyList;
                node: TCallback_node;
                id: Integer);
var
  p: TPoint; vc: tcn_rec; zp: PIntegers;
begin
  if Tree.get_vc(id,@vc) then begin

    if Assigned(node) then begin
      with vc.Pos do p:=node(X,Y,vc.ZPos);
      zp:=nil
    end
    else begin
      p:=vc.Pos; zp:=@vc.ZPos;
    end;

    poly.next_poly(@p,zp,1);
  end
end;

var
  i,j,k,n,ind,len, ornt: int; qc,pc,vc1,vc2: uint;
  ve: tcn_rec; dRec: dm_Rec; pp,lt,rb: TPoint;
  buf: TLPoly;
begin
  Result:=0; poly.Clear;

  Tree._Info(p,dRec,SizeOf(dRec));
  if dRec.mind > 0 then begin

    ind:=mf.vmx_Block(dRec.mind,len);
    n:=len div SizeOf(TPoint);

    i:=0; qc:=0; pc:=0;
    while i < n do begin

      k:=Min(LPoly_Max+1,n-i); Inc(i,k);
      ind:=mf.vm_Load(ind,buf,k * SizeOf(TPoint));

      for j:=0 to k-1 do begin
        pp:=buf[j]; ornt:=tlong(pp.y).b[1];

        if Tree.get_ve(pp.x,@ve) then begin

          vc1:=ve.vc1; vc2:=ve.vc2; if ornt = 2 then
          begin vc1:=ve.vc2; vc2:=ve.vc1 end;

          if vc1 <> pc then begin
          
            if pc <> 0 then begin
              poly.End_contour;
              qc:=0; pc:=0; mf_buf.lp.N:=-1;
            end;

            mf_vc(poly,node,vc1);
          end;

          if qc = 0 then qc:=vc1; pc:=vc2;

          if ve.sgp > 0 then
          with mf_buf^ do begin

            lp.N:=Load_Poly(ve.sgp, @lp.Pol,hp,
                            lp_Max,ve.bits);

            if lp.N >= 0 then begin
              if (ornt = 2) and (lp.N > 0) then
              Swap_LPoly(@lp.Pol,hp,lp.N);

              if Assigned(edge) then begin
                edge(lp,hp,lt,rb,true,false);
                poly.next_poly(@lp.Pol,nil,lp.N+1);
              end else

              poly.next_poly(@lp.Pol,hp,lp.N+1);
            end
          end;

          mf_vc(poly,node,vc2)
        end;

        if (qc = vc2) or (ornt = 255) then begin
          poly.End_contour;
          qc:=0; pc:=0; mf_buf.lp.N:=-1;
        end
      end;
    end;

    poly.End_contour;
  end;

  if poly.PartCount = 0 then
  poly.Clear;

  Result:=poly.Count
end;

function tdm_Map.cn_poly(p: Integer; poly: TPolyList;
                         lp: PLLine; hp: PIntegers): Integer;
var
   mf: tmf_buff;
begin
  Fillchar(mf,SizeOf(mf),0);
  mf.lp:=lp; mf.hp:=hp;
  mf.lp_Max:=LPolyMax;
  Result:=cn_xdraw(p,poly,@mf,nil,nil)
end;

function tdm_Map.fe_contains(p,ref: Cardinal): Integer;
var
  i,k,n,ind,len,top: int;
  dRec: dm_Rec; buf: TLPoly;
begin
  Result:=-1;

  Tree._Info(p,dRec,SizeOf(dRec));
  if dRec.mind > 0 then begin

    ind:=mf.vmx_Block(dRec.mind,len);
    n:=len div SizeOf(TPoint);

    top:=0;
    while n > 0 do begin

      k:=Min(n,LPoly_Max+1); Dec(n,k);

      len:=k * SizeOf(TPoint);
      ind:=mf.vm_Load(ind,buf,len);

      for i:=0 to k-1 do
      if buf[i].x = ref then begin
        Result:=top+i; Break
      end; Inc(top,k)
    end
  end
end;

procedure tdm_Map.fe_bound(p: Integer);
var
  buf: TLPoly; dRec: dm_Rec;
  i,j,k, pp_n, ind,len: int;

  lt,rb, pp: TPoint;
  _lt,_rb: lxyz;

  vc,ve,v1,v2: tcn_rec;

  is_init: Boolean;

begin
  if Get_Object(p,dRec) in [21,22,23] then

  if dRec.mind > 0 then begin

    ind:=mf.vmx_Block(dRec.mind,len);
    pp_n:=len div SizeOf(TPoint);

    i:=0; is_init:=false;

    while i < pp_n do begin

      k:=Min(LPoly_Max+1,pp_n-i); Inc(i,k);
      ind:=mf.vm_Load(ind,buf,k * SizeOf(TPoint));

      for j:=0 to k-1 do begin
        pp:=buf[j];

        if dRec.Tag = 21 then begin
          if Tree.get_vc(pp.x,@vc) then

          if not is_init then begin
            lt:=vc.Pos; rb:=lt; is_init:=true
          end
          else Max_lPort(lt,rb,vc.Pos);

        end else
        if Bound_ve(pp.x, _lt.v,_rb.v) then begin
          if not is_init then begin
            lt:=_lt.p; rb:=_rb.p; is_init:=true
          end
          else Add_lRect(lt,rb, _lt.p,_rb.p);
        end
      end
    end;

    if is_init then begin
      dRec.o_lt:=lt; dRec.o_rb:=rb;
      Tree.NodeInfo(p,dRec,SizeOf(dRec));
    end
  end
end;

procedure tdm_Map.fe_ornt(p: Integer; lp,buf: PLLine; lp_Max: Integer);

const
  s_Ornt: array[1..2] of Byte = (2,1);

var
  ip,iq,ic,loc: Integer;
  qc,pc,ornt,usag,_usag,vc1,vc2: Integer;
  pp: TPoint; ve: tcn_rec; sqr: Double;
  is_ornt,is_inner,is_swap: Boolean;

begin
  qc:=0; pc:=0; iq:=0; sqr:=0;
  is_ornt:=false; is_inner:=false;

  if Assigned(lp) then
  if Assigned(buf) then

  if Tag_Object(p) in [22,23] then
  if xGet_Poly(p,lp,nil,lp_Max) >= 0 then
  for ip:=0 to lp.N do begin
    pp:=lp.Pol[ip];

    if Tree.get_ve(pp.x,@ve) then
    if Get_ve_mf(@ve,buf,nil,lp_Max) > 0 then
    begin
      ornt:=tlong(pp.y).b[1];
      usag:=tlong(pp.y).b[2];

      if loc = 22 then _usag:=255 else
      if is_inner then _usag:=2 else _usag:=1;

      if usag <> _usag then begin
        usag:=_usag; tlong(pp.y).b[2]:=usag;
        lp.Pol[ip]:=pp; is_ornt:=true
      end;

      if not (ornt in [1,2]) then begin
        ornt:=1; tlong(pp.y).b[1]:=ornt;
        lp.Pol[ip]:=pp; is_ornt:=true
      end;

      vc1:=ve.vc1; vc2:=ve.vc2;
      if ornt = 2 then begin
        vc1:=ve.vc2; vc2:=ve.vc1;
        Swap_Poly(buf)
      end;

      if qc = 0 then begin
        qc:=vc1; pc:=vc1
      end;

      if pc <> vc1 then
      if pc = vc2 then begin
        iSwap(vc1,vc2);
        if ornt = 2 then ornt:=1 else
        ornt:=2; Swap_Poly(buf);

        tlong(pp.y).b[1]:=ornt;
        lp.Pol[ip]:=pp; is_ornt:=true
      end;

      if pc = vc1 then begin

        with buf^ do
        sqr:=sqr + Square_Poly(@Pol,N);

        pc:=vc2; if pc = qc then begin

          is_swap:=false;

          if is_inner then is_swap:=sqr > 0
          else is_swap:=sqr < 0;

          if is_swap then begin
            is_ornt:=true; with lp^ do
            Swap_LPoly(@Pol[iq],nil,ip-iq);

            for ic:=iq to ip do begin
              pp:=lp.Pol[ic];
              ornt:=tlong(pp.y).b[1];

              ornt:=s_Ornt[ornt];
              tlong(pp.y).b[1]:=ornt;

              lp.Pol[ic]:=pp;
            end;
          end;

          qc:=0; pc:=0; sqr:=0;
          iq:=ip+1; is_inner:=true
        end
      end
    end
  end;

  if is_ornt then
  Update_xyz(p,0,lp,nil);
end;

function tdm_Map.Link_fe_cn(ptr: int; link: bool): bool;
var
  buf: TLPoly; dRec: dm_Rec;
  id, i,j,k,n, ind,len, cmd: int;
  pp: TPoint;
begin
  Result:=false;

  if Get_Object(ptr,dRec) in [21,22,23] then
  if dRec.mind > 0 then

  if Tree.fe_.vm_Active then
  if Tree.fe_.vm_Edit then begin

    id:=IntLink(ptr);

    if id <> 0 then begin

      ind:=mf.vmx_Block(dRec.mind,len);
      n:=len div SizeOf(TPoint);

      i:=0;
      while i < n do begin
        k:=Min(LPoly_Max+1,n-i); Inc(i,k);
        ind:=mf.vm_Load(ind,buf,k * SizeOf(TPoint));

        for j:=0 to k-1 do begin pp:=buf[j];

          if dRec.Tag = 21 then
            Tree.Link_fe_vc(pp.X,id,link)
          else
            Tree.Link_fe_ve(pp.X,id,link);
        end
      end;

      if fIsNotify then begin
        cmd:=tx_link_cn;
        if not link then cmd:=tx_undo_cn;
        Edit_Object(ptr,id,cmd)
      end;

      Result:=true
    end
  end
end;

procedure tdm_Map.Update_fe(p: Integer; lp,buf: PLLine);
begin
  EditMode:=true;

  Link_fe_cn(p,false);
  xUpdate_mf(p,0,lp,nil);
  After_fe(p, lp,buf);

  EditMode:=false;
end;

procedure tdm_Map.After_fe(p: Integer; lp,buf: PLLine);
begin
  EditMode:=true;

  fe_bound(p);

  if Assigned(buf) then
  fe_ornt(p,lp,buf,LPolyMax);

  Link_fe_cn(p,true);

  EditMode:=false;
end;

function tdm_Map.fe_ref(p, id1,id2: Integer;
                        tags: tbyte_set): Boolean;
var
  i,n,k, ind,len, lp_max: int;
  pt: TPoint; dRec: dm_Rec; lp: TLPoly;
begin
  Result:=false;

  if p > 0 then
  if Get_Object(p,dRec) in tags then
  if dRec.mind > 0 then begin

    ind:=mf.vmx_Block(dRec.mind,len);
    n:=len div Sizeof(pt);

    lp_max:=Sizeof(lp) div Sizeof(pt);

    while n > 0 do begin
      k:=Min(n,lp_max); Dec(n,k);
      mf.vm_Load(ind,lp,k * Sizeof(pt));

      for i:=0 to k-1 do begin
        pt:=lp[i]; if pt.x = id1 then begin
          pt.x:=id2; Result:=true;
          mf.vm_Store(ind,pt,Sizeof(pt));
        end;

        Inc(ind,Sizeof(pt))
      end
    end
  end
end;

function tdm_Map.Get_ve_vc(const P: TPoint; out vc1,vc2: Cardinal): Boolean;
var
  ve: tcn_rec; ornt: Integer;
begin
  Result:=false; vc1:=0; vc2:=0;

  ornt:=tlong(P.y).b[1];
  if Tree.get_ve(P.x,@ve) then begin

    if ornt = 2 then begin
      vc1:=ve.vc2; vc2:=ve.vc1
    end
    else begin
      vc1:=ve.vc1; vc2:=ve.vc2
    end;

    Result:=true
  end
end;

function tdm_Map.Get_lp_vc(lp: PLLine; out vc1,vc2: Cardinal): Boolean;
var
  vc: Cardinal;
begin
  Result:=false; vc1:=0; vc2:=0;

  if Enabled_Map then

  with lp^ do if N >= 0 then
  if Get_ve_vc(Pol[0], vc1,vc) then
  if Get_ve_vc(Pol[N], vc,vc2) then

  Result:=true
end;

function tdm_Map.fe_lock(lp: PLLine): Boolean;
var
  vc1,vc2: Cardinal;
begin
  Result:=false;
  if Get_lp_vc(lp, vc1,vc2) then
  Result:=vc1 = vc2
end;

function tdm_Map.Get_ve_next(Vc,Ve: Cardinal; lp: PLLine): Cardinal;
var
  fe: tfe_id; cn: tcn_rec;
begin
  Result:=0; if Vc > 0 then
  if Tree.Get_vc_ref(Vc,@fe,fe_Max) = 2 then
  if fe[0] = Ve then Result:=fe[1] else
  if fe[1] = Ve then Result:=fe[0];

  if Result = Ve then Result:=0 else

  if Result > 0 then
  if Tree.Get_ve(Result,@cn) then
  if cn.vc1 = cn.vc2 then Result:=0;

  if Result <> 0 then
  if Assigned(lp) then with lp^ do
  if Index_Contains(@Pol,N+1,Result) >= 0 then
  Result:=0
end;

function tdm_Map.ve_vc_Used(ve,vc: Cardinal): Integer;
var
  cn: tcn_rec;
begin
  Result:=-1;
  if Tree.get_ve(ve,@cn) then
  if cn.vc1 = vc then Result:=0 else
  if cn.vc2 = vc then Result:=1
end;

function tdm_Map.Load_ve_mf(ve: pcn_rec;
                            lp: PLPoly; hp: PIntegers;
                            lp_Max: Integer): Integer;
var
  v1,v2: tcn_rec; di: PChar;
  rib_lp: PLPoly; rib_hp: PIntegers;
begin
  Result:=-1;

  if Tree.Get_vc(ve.vc1,@v1) then
  if Tree.Get_vc(ve.vc2,@v2) then begin

    rib_lp:=nil; rib_hp:=nil;
    if Assigned(lp) then rib_lp:=@lp[1];
    if Assigned(hp) then rib_hp:=@hp[1];

    Result:=Load_Poly(ve.sgp, rib_lp,rib_hp,
                      lp_max-2,ve.bits);

    if Assigned(lp) then lp[0]:=v1.Pos;
    if Assigned(hp) then hp[0]:=v1.zpos;

    Inc(Result,2);

    if Assigned(lp) then lp[Result]:=v2.Pos;
    if Assigned(hp) then hp[Result]:=v2.zpos;
  end
end;

function tdm_Map.Get_ve_mf(ve: pcn_rec;
                           lp: PLLine; hp: PIntegers;
                           lp_Max: Integer): Integer;
var
  _lp: PLPoly;
begin
  Result:=-1; _lp:=nil;
  if Assigned(lp) then _lp:=@lp.Pol;
  Result:=Load_ve_mf(ve,_lp,hp,lp_Max);
  if Assigned(lp) then lp.N:=Result
end;

function tdm_Map.Get_ve_Poly(Id: Cardinal;
                             lp: PLLine; hp: PIntegers;
                             lp_Max: Integer): Integer;
var
  ve: tcn_rec;
begin
  Result:=-1;
  if Assigned(lp) then lp.N:=-1;
  if lp_Max = 0 then lp_Max:=LPolyMax;

  if Tree.get_ve(Id,@ve) then
  Result:=Get_ve_mf(@ve, lp,hp, lp_Max)
end;

function tdm_Map.More_ve_Poly(const Pt: TPoint;
                              lp: PLLine; hp: PIntegers;
                              lp_Max: Integer): Integer;
var
  ve: tcn_rec; i: Integer;
  _lp: PLPoly; _hp: PIntegers;
begin
  Result:=-1;

  if Tree.get_ve(Pt.x,@ve) then begin
    if lp_Max = 0 then lp_Max:=LPolyMax;

    i:=lp.N+1; _lp:=@lp.Pol[i];
    _hp:=nil; if Assigned(hp) then _hp:=@hp[i];

    Result:=Load_ve_mf(@ve,_lp,_hp,lp_Max - i);

    if Result > 0 then
    if tlong(Pt.y).b[1] = 2 then
    Swap_LPoly(_lp,_hp,Result);

    Inc(lp.N,Result+1)
  end;
end;

procedure tdm_Map.Update_vc(Id: Cardinal;
                            const P: TPoint;
                            ZPos: PInteger);
var
  vc: tcn_rec;
begin
  if Tree.get_vc(Id,@vc) then begin
    vc.Pos:=P; vc.zpos:=0;
    if vc.bits and fl_xyz <> 0 then
    vc.bits:=vc.bits xor fl_xyz;

    if Assigned(ZPos) then begin
      vc.bits:=vc.bits or fl_xyz;
      vc.zpos:=ZPos^;
    end;

    Tree.Put_vc(Id,@vc)
  end
end;

procedure tdm_Map.Update_ve(Id, Vc1,Vc2: Cardinal;
                            lp: PLLine; hp: PIntegers);
var
  ve: tcn_rec; _lp: PLLine;
  p: PChar; n,fl: Integer;
begin
  if Tree.get_ve(Id,@ve) then begin

    if Vc1 > 0 then ve.vc1:=Vc1;
    if Vc2 > 0 then ve.vc2:=Vc2;

    if Assigned(lp) then begin
      dup_LPoly(@lp.Pol,hp,lp.N,false);

      if lp.N > 1 then begin
        p:=@lp.Pol[1]; _lp:=PLLine(p - Sizeof(lp.N));
        n:=_lp.N; _lp.N:=lp.N-2; fl:=ve.bits;
        if Update_Poly(ve.sgp,fl, _lp,hp) then
        ve.bits:=fl; _lp.N:=n
      end
      else begin
        ve.sgp:=0; ve.bits:=ve.bits and $1F;
      end;
    end;

    Tree.Put_ve(Id,@ve)
  end
end;

function tdm_Map.Split_ve_lp(ve_Id,vc1_Id,vc2_Id: Cardinal;
                             lp: PLLine; lp_Max: Integer;
                             Alt: TIntegerList): Boolean;

function ve_Ornt(vc_Id: Cardinal; var pp: TPoint): Integer;
var
  ve: tcn_rec; Ornt: Integer;
begin
  Result:=vc_Id; Ornt:=1;

  if Tree.get_ve(pp.x,@ve) then

  if vc_Id = ve.vc1 then begin
    Ornt:=1; Result:=ve.vc2
  end else
  if vc_Id = ve.vc2 then begin
    Ornt:=2; Result:=ve.vc1
  end
  else Ornt:=255;

  tlong(pp.y).b[1]:=Ornt
end;

var
  i,dn,top,ind,cx,ornt,vc_id: Integer;
  pp: TPoint; ve: tcn_rec;
begin
  Result:=false;

  top:=0; dn:=Alt.Count-1;

  if Alt.Count > 1 then
  if Tree.get_ve(ve_Id,@ve) then

  with lp^ do
  while N + dn <= lp_Max do begin

    ind:=-1; cx:=N+1-top;  if cx > 0 then
    ind:=Index_Contains(@Pol[top],N+1-top,ve_Id);
    if ind < 0 then Break; Inc(ind,top);

    pp:=Pol[ind];
    ornt:=tlong(pp.y).b[1];
    if ornt = 255 then begin
      tlong(pp.y).b[1]:=1;
      Pol[ind]:=pp; ornt:=1;
    end;

    for i:=N downto ind+1 do
    Pol[i+dn]:=Pol[i];

    if ornt = 2 then begin
      vc_id:=vc2_id;
      for i:=dn downto 0 do begin
        pp.x:=Alt[i];
        vc_id:=ve_Ornt(vc_id,pp);
        Pol[ind]:=pp; Inc(ind)
      end
    end
    else begin
      vc_id:=vc1_id;
      for i:=0 to dn do begin
        pp.x:=Alt[i];
        vc_id:=ve_Ornt(vc_id,pp);
        Pol[ind]:=pp; Inc(ind)
      end;
    end;

    Inc(N,dn); Result:=true;
    top:=ind; if top > N then Break
  end
end;

procedure tdm_Map.Split_ve(ve,vc1,vc2: Cardinal;
                           buf: PLLine; lp_max: Integer;
                           Alt: TIntegerList);
var
  i,j,n, id,p: Integer;
  fe: tfe_id; pp: TPoint;
begin
  n:=Tree.Get_fe(ve,0, @fe,0);
  for i:=0 to n-1 do begin

    id:=fe[i]; p:=Offset_by_Id(id);

    if p > 0 then
    if Tag_Object(p) in [22,23] then
    if xGet_Poly(p,buf,nil,lp_Max) >= 0 then

    if Split_ve_lp(ve,vc1,vc2,
                   buf,lp_Max, Alt) then begin

      Update_xyz(p,0,buf,nil);

      for j:=0 to Alt.Count-1 do begin
        pp.x:=Alt[j]; if pp.x <> ve then
        Tree.Link_fe_ve(pp.x,id,true);
      end;

      Edit_Object(p,id,tx_edit_mf)
    end
  end
end;

function tdm_Map.IsEqualNodes(vc1,vc2: Cardinal): Boolean;
var
  v1,v2: tcn_rec;
begin
  Result:=false;

  if vc1 = vc2 then
    Result:=true
  else
  if Tree.get_vc(vc1,@v1) then
  if Tree.get_vc(vc2,@v2) then

  if PointsEqual(v1.Pos,v2.Pos) then
  Result:=v1.ZPos = v2.ZPos
end;

function tdm_Map.is_ve_merge(ve1,ve2: Cardinal;
                             out vc: Cardinal): Boolean;
var
  v1,v2: tcn_rec;
begin
  Result:=false;

  if ve1 <> ve2 then

  if Tree.get_ve(ve1,@v1) then
  if Tree.get_ve(ve2,@v2) then

  if v1.vc1 <> v1.vc2 then
  if v2.vc1 <> v2.vc2 then

  if (v1.vc1 = v2.vc1) or (v1.vc1 = v2.vc2) then
    begin vc:=v1.vc1; Result:=true end
  else
  if (v1.vc2 = v2.vc1) or (v1.vc2 = v2.vc2) then
    begin vc:=v1.vc2; Result:=true end;

  if Result then
  Result:=Tree.Get_vc_ref(vc,nil,0) = 2
end;

function tdm_Map.ve_fe_join(ve1,ve2: Cardinal; buf: PLLine;
                            is_edit: Boolean): Boolean;

function ve_fe(ve1,ve2: Integer; buf: PLLine;
               is_edit: Boolean): Boolean;
var
  i,j,jn,fe_n,id,p: Integer;
  fe: tfe_id; p1,p2: TPoint;
  skip: Boolean;
begin
  Result:=true;

  fe_n:=Tree.Get_fe(ve1,0,@fe,0);
  for i:=0 to fe_n do begin
    id:=fe[i]; p:=Offset_by_Id(id);

    if p > 0 then
    if Tag_Object(p) in [22,23] then
    if xGet_Poly(p,buf,nil,LPolyMax) >= 0 then

    if buf.N = 0 then begin
      Result:=false; Break
    end
    else begin

      p2:=buf.Pol[0]; jn:=0; skip:=false;

      for j:=1 to buf.N do begin
        p1:=p2; p2:=buf.Pol[j];
        Inc(jn); buf.Pol[jn]:=p2;

        if skip then skip:=false else

        if p1.x = ve1 then begin

          if p2.x = ve2 then begin
            Dec(jn); skip:=true
          end
          else begin
            Result:=false; Break
          end

        end else
        if p1.x = ve2 then begin

          if p2.x = ve1 then begin
            Dec(jn); buf.Pol[jn]:=p2; skip:=true
          end
          else begin
            Result:=false; Break
          end

        end
      end; buf.N:=jn;

      if not Result then Break;

      if is_edit then begin
        Update_xyz(p,0,buf,nil);
        Edit_Object(p,id,tx_edit_mf)
      end
    end
  end
end;

begin
  if is_edit then
    Result:=ve_fe(ve1,ve2,buf,true)
  else
    Result:=ve_fe(ve1,ve2,buf,false) and
            ve_fe(ve2,ve1,buf,false)
end;

function tdm_Map.Bound_ve(Id: Integer;
                          out lt,rb: VPoint): Boolean;
var
  ve,vc1,vc2: tcn_rec; lt1,rb1,lt2,rb2: VPoint;
begin
  Result:=false; lt:=_VPoint(0,0,0); rb:=lt;

  if Tree.get_ve(Id,@ve) then
  if Tree.get_vc(ve.vc1,@vc1) then
  if Tree.get_vc(ve.vc2,@vc2) then begin

    lt1:=vc1.VPos; rb1:=lt1;
    Max_VPort(lt1,rb1, vc2.VPos);

    if Bound_Poly(ve.sgp,ve.bits, lt2,rb2) >= 0 then
    Add_VRect(lt1,rb1, lt2,rb2);

    lt:=lt1; rb:=rb1; Result:=true
  end
end;

function tdm_Map.Get_vc_Count: Integer;
begin
  Result:=Tree.vc_index.Count
end;

function tdm_Map.Get_ve_Count: Integer;
begin
  Result:=Tree.ve_index.Count
end;

function tdm_Map.Get_fc_Count: Integer;
begin
  Result:=Tree.fc_index.Count
end;

function tdm_Map.Get_vc_Id(Ind: Integer): Cardinal;
begin
  Result:=Tree.vc_index.Get_id(Ind)
end;

function tdm_Map.Get_ve_Id(Ind: Integer): Cardinal;
begin
  Result:=Tree.ve_index.Get_id(Ind)
end;

function tdm_Map.Get_fc_Id(Ind: Integer): Cardinal;
begin
  Result:=Tree.fc_index.Get_id(Ind)
end;

function tdm_Map.fc_add(RCNM,RCID,RVER,RUIN, OBJL: Cardinal;
                        lp: PLLine; hf: TInfoList): Cardinal;
var
  r: tcn_rec; x: TInt64;
begin
  Result:=0; x.x:=0;

  FillChar(r,SizeOf(r),0);
  r.rcid:=RCID; r.rver:=RVER;
  r.rcnm:=RCNM; r.bits:=RUIN;

  if not Tree.is_cn then
  Tree.is_cn:=true;

  if Tree.is_cn then begin

    if hf = nil then begin
      if r.rcid < Get_next_nnnn then
      r.rcid:=0; if r.rcid = 0 then
      r.rcid:=Gen_next_nnnn;
    end;

    x.id:=r.RCID; x.cn:=r.RCNM; r.OBJL:=OBJL;

    with lp^ do if N >= 0 then
    mf.vmx_UpdateBuf(r.mfp,Pol,(N+1)*8);

    Tree.Add_fc(@r); Result:=r.rcid;

    if Assigned(hf) then
    if hf.Count > 0 then
    Update_Info(x.x,hf)
  end;

  if Result > 0 then begin

    with lp^ do
    Tree.fe_fc_Update(Result,@Pol,N+1,false);

    if Assigned(FOnEditObject) then
    fOnEditObject(x.x,0,tx_Edit_add)
  end
end;

function tdm_Map.fc_Get_list(Id: Cardinal;
                             lp: PLLine; lp_Max: Integer): Integer;
var
  fc: tcn_rec; len,ofs,n: Integer;
begin
  Result:=0;
  if Assigned(lp) then lp.N:=-1;

  if Tree.get_fc(Id,@fc) then
  if fc.mfp > 0 then begin

    ofs:=mf.vmx_Block(fc.mfp,len);
    n:=len div SizeOf(TPoint);

    if lp = nil then
      Result:=n
    else
    if n <= lp_Max then begin
      len:=n*SizeOf(TPoint);

      if len > 0 then begin
        mf.vm_Load(ofs,lp.Pol,len);
        lp.N:=n-1;
      end;

      Result:=n
    end
  end
end;

function tdm_Map.fc_Contains_fe(fc,fe: Cardinal): Boolean;
var
  buf: TLLine;
begin
  Result:=false;
  if fc_Get_list(fc,@buf,LPoly_Max) > 0 then
  Result:=lp_Contains_Index(@buf,fe) >= 0
end;

procedure tdm_Map.fc_Update(Id, OBJL: Cardinal; lp: PLLine);
var
  x: TInt64; ofs,len: Integer; fc: tcn_rec;
begin
  if Begin_Update then
  if Tree.get_fc(Id,@fc) then begin

    if Assigned(lp) then

    if lp.N >= 0 then begin
      len:=(lp.N+1)*8; ofs:=fc.mfp;
      if mf.vmx_UpDateBuf(ofs,lp.Pol,len) then
      fc.mfp:=ofs
    end
    else fc.mfp:=0;

    if OBJL <> 0 then fc.OBJL:=OBJL;

    Tree.Put_fc(Id,@fc);

    x.cn:=cn_aggr; x.id:=Id;

    if Assigned(lp) then with lp^ do
    Tree.fe_fc_Update(Id,@Pol,N+1,true);

    if Assigned(FOnEditObject) then
    fOnEditObject(x.x,0,tx_Edit_mf)
  end;

  End_Update
end;

function tdm_Map.fc_Tools(fc_Id,fe_Id: Cardinal;
                          Include,fe_fc: Boolean): Boolean;
var
  fc: tcn_rec; i,n,cn, ind,len: int;
  lp,hp: PLPoly; buf: TLPoly; x: TInt64;
begin
  Result:=false;

  if Begin_Update then
  if Tree.get_fc(fc_Id,@fc) then begin

    ind:=mf.vmx_Block(fc.mfp,len);
    n:=len div SizeOf(TPoint);

    hp:=nil; lp:=@buf;
    if n+1 > LPoly_Max then begin
      hp:=xAllocPtr((n+1) * Sizeof(TPoint)); lp:=hp
    end;

    if Assigned(lp) then begin

      if n > 0 then begin
        len:=n * SizeOf(TPoint);
        mf.vm_Load(ind,lp^,len);

        i:=Index_Contains(Pointer(lp),n,fe_Id);

        if i >= 0 then

        if Include then
          Include:=false
        else begin
          while i < n do begin
            lp[i]:=lp[i+1]; Inc(i)
          end;

          Dec(n); len:=n * SizeOf(TPoint);

          with fc do if n = 0 then begin
            mfp:=0; Result:=true
          end else

          Result:=mf.vmx_UpDateBuf(mfp,lp^,len);
        end
      end;

      if Include then begin cn:=0;
        if Tree.fc_IndexOf(fe_Id) >= 0 then
        cn:=cn_aggr;

        lp[n]:=fc_ptr(fe_Id,cn,3);
        Inc(n); len:=n * SizeOf(TPoint);

        Result:=mf.vmx_UpDateBuf(fc.mfp,lp^,len);
      end
    end;

    if Result then begin
      Tree.Put_fc(fc_Id,@fc);

      if fe_fc then
      Tree.fe_fc_Update(fc_Id,lp,n,true);

      x.cn:=cn_aggr; x.id:=fc_Id;
      if Assigned(FOnEditObject) then
      fOnEditObject(x.x,0,tx_Edit_mf)
    end;

    xFreePtr(hp)
  end;

  End_Update
end;

procedure tdm_Map.fc_fe_Exclude(fe_Id: Cardinal);
var
  fc: tfe_id; fc_i,fc_n: Integer;
begin
  if Enabled_Map then begin
    fc_n:=Tree.Get_fe_fc(fe_Id,fc);

    if fc_n > 0 then begin

      if Begin_update then begin
        Tree.fe_fc_Exclude(fe_Id);

        for fc_i:=0 to fc_n-1 do
        fc_Tools(fc[fc_i],fe_Id,false,false)
      end;

      End_update
    end
  end
end;

function tdm_Map.fc_Get_Bound(Id: Cardinal;
                              out lt,rb: lxyz): Boolean;
var
  fc: tcn_rec; p: TPoint;
  len,ofs, i,n: Integer; _lt,_rb: lxyz;
begin
  Result:=false;

  if Tree.get_fc(Id,@fc) then
  if fc.mfp > 0 then begin

    ofs:=mf.vmx_Block(fc.mfp,len);
    n:=len div SizeOf(TPoint);

    for i:=1 to n do begin
      ofs:=mf.vm_Load(ofs,p,Sizeof(p));

      if tlong(p.y).b[0] = 0 then begin

        p.y:=Offset_by_Id(p.x);
        if Get_Bound(p.y, _lt,_rb) >= 0 then

        if Result then
          Add_VRect(lt.v,rb.v, _lt.v,_rb.v)
        else begin
          lt:=_lt; rb:=_rb; Result:=true
        end

      end
    end
  end
end;

constructor tdmScan.Create(dm: tdm_Map);
begin
  inherited Create; dm_:=dm;
  Start:=dm.Tree.Root
end;

function tdmScan.Scan_tree(frst,up: Boolean): longint;
var
  top,run,owner,bot,id: Longint;
  pos: TLLine; this: Boolean;
begin
  Result:=0; fLevel:=0;

  if dm_.Enabled_Map then

  with dm_.Tree do
  if dm_lib = 0 then begin

    bot:=vm_Virt.vm_Ind - (12 + Sizeof(dm_Rec));

    owner:=Root; Start:=Root; this:=false;

    if frst then begin
      top:=Root; run:=Root
    end
    else begin
      top:=TopP; run:=RunP;
      if not up then owner:=run;
      start:=RunP;
    end;

    dm_.Get_Node(run,dmNode);

    Fillchar(pos,Sizeof(pos),0); 

    pos.N:=-1; repeat
      if dmNode.Child > 0 then
      if Goto_Down(run) then begin

        with pos do begin
          if N < LPoly_Max then Inc(N) else
          Move(Pol[1],Pol[0],N*Sizeof(TPoint));
          Pol[N].x:=top; Pol[N].y:=run
        end;

        top:=dmNode.Child;
        run:=dmNode.Child;
        Inc(fLevel);
      end;

      while run <> owner do begin

        FPred_run:=0;
        if run <> top then
        FPred_run:=run;

        run:=_Link(run);

        if run <= Root then
        begin run:=owner; Break end;

        if run < Root then run:=top else
        if run > bot then run:=top;

        if run <> top then begin
          dm_.Get_Node(run,dmNode);
          if dmNode.Len <> Sizeof(dm_Rec) then
          run:=top
        end;

        if run = top then begin
          run:=_Child(top); Back_Down(run);
          Dec(fLevel);

          if run <> owner then

          if pos.N >= 0 then
          with Pos do begin
            run:=Pol[N].y; top:=Pol[N].x; Dec(N)
          end else

          if run = 0 then begin
            top:=owner; run:=owner
          end else
          if run = ffind_pos.Y then
            top:=ffind_pos.X
          else
            top:=_TopP(run)

        end
        else begin
          fRun_Parent:=_Child(top);

          if Assigned(fOnObject) then begin
            TopP:=top; RunP:=run;
            this:=fOnObject(run,dmNode.dRec)
          end else
          if Assigned(fOnObject1) then begin
            TopP:=top; RunP:=run;
            this:=fOnObject1(run,fLevel,dmNode.dRec);
          end else
          if Assigned(fOnObject2) then begin
            TopP:=top; RunP:=run;
            id:=dm_.Get_id(run);
            this:=fOnObject2(run,id,fLevel);
          end
          else this:=This_Object(run);

          if vm_Virt.vm_Edit then
          bot:=vm_Virt.vm_Ind - (12 + Sizeof(dm_Rec));

          Break
        end
      end;

    until this or (run = owner) or is_Break;

    if this then begin

      if pos.N >= 0 then
      ffind_pos:=pos.Pol[pos.N];

      TopP:=top; RunP:=run;
      Result:=run
    end

  end
end;

function tdmScan.Scan_Childs(p: Int64): longint;
var
  x: TInt64;
begin
  Result:=0; x.x:=p;

  if x.cn = cn_object then

  if p = 0 then
    Result:=Scan_tree(true,true)
  else
  if dm_.Tree._Goto(p) > 0 then
    Result:=Scan_tree(false,false)

end;

function tdmScan.Edit_Childs(p: Int64): longint;
begin
  Result:=0;
  if dm_.Update_Map then
  Result:=Scan_Childs(p)
end;

function tdmScan.This_Object(p: longint): Boolean;
begin
  Result:=false;
end;

function tdmScan.Goto_Down(p: longint): Boolean;
begin Result:=true end;

procedure tdmScan.Back_Down(p: longint);
begin
end;

function tdmScan.Restore_Object(p: longint;
                                is_mf: Boolean): Boolean;
begin
  Result:=false
end;

function tdmScan.That_Object(p: longint): Boolean;
begin
  fRun_Parent:=0;
  dm_.Get_Node(p,dmNode);
  Result:=This_Object(p)
end;

function tdmScan.This_Hole(p: longint): Boolean;
var
  code: Integer;
begin
  Result:=false;

  if fRun_Parent <> 0 then

  with dm_ do
  if Tag_Object(p) = 3 then
  if Tag_Object(fRun_Parent) = 3 then begin

    code:=Code_Object(p);

    Result:=is_Hole_Code(code) or
            (code = Code_Object(fRun_Parent))
  end
end;

constructor tdmList.Create(dm: tdm_Map;
                           AList1: TIntegerList;
                           AList2: TInt64List);
begin
  inherited Create(dm);
  fList1:=AList1;
  fList2:=AList2;
end;

function tdmList.This_Object(p: longint): Boolean;
begin
  Result:=false;
  if Assigned(fList1) then fList1.AddItem(p);
  if Assigned(fList2) then fList2.Add(p)
end;

constructor TUserScan.Create(dm: tdm_Map);
begin
  inherited Create(dm);
  fTopp:=dm.Tree.TopP;
  fRunp:=dm.Tree.RunP;
end;

destructor TUserScan.Destroy;
begin
  dm_.Tree.TopP:=fTopp;
  dm_.Tree.RunP:=fRunp;
  inherited
end;

constructor VUserScan.Create(dm: tdm_Map);
begin
  inherited Create(dm);
  fTopp:=dm.Tree.TopP;
  fRunp:=dm.Tree.RunP;
end;

destructor VUserScan.Destroy;
begin
  dm_.Tree.TopP:=fTopp;
  dm_.Tree.RunP:=fRunp;
  inherited
end;

constructor tdmEnum.Create(dm: tdm_Map);
begin
  inherited Create(dm);
  fList:=TIntegerList.Create;
  fList.Capacity:=1024*64
end;

destructor tdmEnum.Destroy;
begin
  fList.Free; inherited
end;

function tdmEnum.Release(out Buf: PIntegers): Integer;
begin
  Result:=fList.Release(Pointer(Buf))
end;

function tdmEnum.This_Object(p: longint): Boolean;
begin
  Result:=false; fList.AddItem(p)
end;

constructor tdm_3d.Create(dm: tdm_Map;
                          AFlags: Integer;
                          ARgn: PLPoly);
begin
  inherited Create(dm);
  fFlags:=AFlags; fRgn:=ARgn;
  fStack.N:=-1
end;

function tdm_3d.This_Object(p: longint): Boolean;
const
  _loc: array[1..6] of Integer = (1,2,4,8,0,2);
  _typ: array[1..6] of Integer = ($100,$200,$400,$800,0,$200);
var
  loc,typ,id: Integer; lt,rb: TPoint;
begin
  Result:=false; typ:=0;

  loc:=dmNode.dRec.Tag;
  if loc in [1..4,6,33] then begin

    if loc = 3 then with fStack do
    if (N >= 0) and (Pol[N].Y = 3) then
    if Pol[N].X = dmNode.dRec.Code then
    loc:=-1;

    if loc > 0 then begin

      loc:=loc mod 10;

      if Assigned(fRgn) then begin
        with dmNode.dRec do begin
          lt:=o_lt; rb:=o_rb;
          if loc = 1 then rb:=o_lt
        end;

        if (rb.X <= fRgn[0].X)
        or (lt.X >= fRgn[1].X)
        or (rb.Y <= fRgn[0].Y)
        or (lt.Y >= fRgn[1].Y) then loc:=0
      end;

      if loc in [1..6] then begin
        if _loc[loc] and fFlags <> 0 then begin

          if fFlags and $40000000 <> 0 then
            typ:=1
          else
          if fFlags and $20000000 <> 0 then begin
            if dm_.xGet_Int(p,1000,id) then typ:=1
          end else

          if fFlags and $10000000 <> 0 then begin
            if dmNode.Flags and fl_xyz <> 0 then
            fList.AddItem(p)
          end
          else begin
            typ:=dm_.Get_int(p, 700); if typ = 0 then
            if fFlags and $80000000 <> 0 then typ:=1
          end;

          if typ in [1..4,6] then
          if _typ[typ] and fFlags <> 0 then
          fList.AddItem(p)
        end;

        if not Result and (loc = 1) then
        if typ = -1 then fList.AddItem(p)
      end
    end
  end
end;

function tdm_3d.Goto_Down(p: longint): Boolean;
var
  dRec: dm_Rec;
begin
  Result:=true;

  if fFlags and $10000000 <> 0 then

  with dmNode do
  Result:=(p = dm_.Tree.Root) or
  (dRec.Tag <> 0) or (Flags and fl_draw = 0);

  if Result then

  with fStack do
  if N < 4 then begin
    dm_.Get_Object(p,dRec); Inc(N);
    Pol[N].X:=dRec.Code; Pol[N].Y:=dRec.Tag;
  end
end;

procedure tdm_3d.Back_Down(p: longint);
begin
  with fStack do if N >= 0 then Dec(N)
end;

function vdmScan.Goto_Down(p: longint): Boolean;
begin
  with dmNode do
  Result:=(p = dm_.Tree.Root) or
  (dRec.Tag <> 0) or (Flags and fl_draw = 0)
end;

function xScanLink.This_Object(p: longint): Boolean;
begin
  Result:=false; with dmNode.dRec do
  Result:=(Code = 0) and (Tag = 1)
end;

constructor xLookLayer.Create(dm: tdm_Map; Code: Integer);
begin
  inherited Create(dm);
  dmCode:=Code;
end;

function xLookLayer.This_Object(p: longint): Boolean;
begin
  Result:=false;

  with dmNode.dRec do
  if p <> dm_.Tree.Root then
  Result:=(Code = dmCode) and (Tag = 0)
end;

function xLookLayer.Goto_Down(p: longint): Boolean;
begin
  Result:=p = dm_.Tree.Root
end;

constructor xLookCode.Create(dm: tdm_Map; Code,Loc: Integer);
begin
  inherited Create(dm);
  dmCode:=Code; dmLoc:=Loc;
end;

function xLookCode.This_Object(p: longint): Boolean;

function IsBadCode(code: int): Boolean;
begin
  Result:=StrToCode(CodeToStr(code)) = dmCode
end;

begin
  Result:=false;
  if p <> dm_.Tree.Root then
  with dmNode.dRec do

  if dmLoc = 0 then
    Result:=Layer_Contains(dmCode,Code)
  else
  if (Code = dmCode) or (dmCode = 0)
  or ((dmCode < 10000000) and IsBadCode(code)) then begin
    if Tag = dmLoc then Result:=true else
    if Transit_loc(Tag) = dmLoc then Result:=true
  end
end;

function xLookObject.This_Object(p: longint): Boolean;
begin
  with dmNode.dRec do
  Result:=(Code = dmCode) and (Tag = dmLoc)
end;

constructor xLookChar.Create(dm: tdm_Map; Code,Loc: Integer;
                             n: Integer; t: Id_Tag; i: longint;
                             f: Double; s: PChar);
begin
  inherited Create(dm,Code,Loc);
  nn:=n; id:=t; ii:=i; ff:=f;
  StrCopy(ss,''); if s <> nil then
  _UpperRus(StrCopy(ss,s))
end;

function xLookChar.This_Object(p: longint): Boolean;
var
  equ: TWideStr;
  b: byte absolute equ;
  w: word absolute equ;
  i: SmallInt absolute equ;
  l: longint absolute equ;
  r: float absolute equ;
  d: double absolute equ;
  s: string absolute equ;

  t: TShortStr;

begin
  Result:=false;

  if inherited This_Object(p) then
  if dm_.Get_hf(p,nn,id,equ) then

  case id of
_byte,
_bool:
    Result:=b = ii;

_word,
_enum,
_dbase:
    Result:=w = ii;

_int:
    Result:=i = ii;

_long,
_time,
_date:
    Result:=l = ii;

_float,
_real,
_angle:
    Result:=r = ff;

_double:
    Result:=d = ff;

_string:
    if StrPCopy(t,s) <> nil then
    if _UpperRus(t) <> nil then

    if StrComp(t,ss) = 0 then
      Result:=true
    else
    if dm_Char_Is_Words then
      Result:=Str_Words_Comp(t,ss)
  end
end;

constructor xLookName.Create(dm: tdm_Map;
                             AId,ACode,ALoc,Ann: int;
                             AName: PChar);
begin
  inherited Create(dm,ACode,ALoc);

  StrCopy(fName,''); if AName <> nil then
  xStrUpper(StrCopy(fName,AName));

  fId:=AId; fnn:=Ann;
  if fnn = 0 then fnn:=9
end;

function xLookName.This_Object(ptr: longint): Boolean;

function xThis_Object(ptr: longint): Boolean;

function IsBadCode(code: int): Boolean;
begin
  Result:=StrToCode(CodeToStr(code)) = dmCode
end;

begin
  Result:=false;
  if ptr <> dm_.Tree.Root then
  with dmNode.dRec do

  if dmLoc = 0 then
    Result:=Layer_Contains(dmCode,Code)
  else
  if (Code = dmCode)
  or ((dmCode < 10000000) and IsBadCode(code)) then

  if Tag = dmLoc then Result:=true else
  if Transit_loc(Tag) = dmLoc then Result:=true
end;

var
  s: TShortStr;
begin
  Result:=false;

  if fId <> 0 then
    Result:=dm_.Get_Int(ptr,1000) = fId
  else begin
    Result:=xThis_Object(ptr);

    if Result then
    if fName[0] <> #0 then begin
      Result:=false;

      if dm_.Get_sval(ptr,fnn,s) then
      Result:=Str_Compare(fName,nil, xStrUpper(s),1)
    end
  end
end;

procedure tdmTool.Tool(p: longint);
begin
  if p > 0 then Scan_Childs(p)
end;

constructor tdmCalc.Create(dm: tdm_Map);
begin
  inherited Create(dm); s:=0;
  buf:=Alloc_LPolyBuf
end;

destructor tdmCalc.Destroy;
begin
  xFreePtr(buf); inherited
end;

function tdmCalc.This_Object(p: longint): Boolean;
var
  t: txyz;
begin
  if Assigned(buf) then
  t:=dm_.Poly_Calc(p,buf,nil,LPolyMax);
  if t.y > 0 then s:=s+t.y;
  Result:=false
end;

function dm_Square_Childs(dm: tdm_Map; p: longint): double;
var
  scan: tdmCalc;
begin
  Result:=0;
  scan:=tdmCalc.Create(dm);
  try
    begin
      scan.Tool(p);
      Result:=scan.s
    end;
  finally
    scan.Free;
  end;
end;

type
  xLookLink = class(tdmScan)
    constructor Create(dm: tdm_Map; nnn: Integer);
    function This_Object(p: longint): Boolean; override;
  private
    fnnn: Integer;
  end;

constructor xLookLink.Create(dm: tdm_Map; nnn: Integer);
begin
  inherited Create(dm); fnnn:=nnn
end;

function xLookLink.This_Object(p: longint): Boolean;
begin
  Result:=dm_.Get_Int(p,1000) = fnnn
end;

function Id_to_Offset(dm: tdm_Map; id: longint): longint;
var
  look: xLookLink;
begin
  look:=xLookLink.Create(dm,id);
  Result:=look.Scan_childs(0);
  look.Free
end;

type
  TIndexing = class(tdmScan)
    constructor Create(dm: tdm_Map;
                       AIndex: TIndexList;
                       only_id: Boolean);

    function This_Object(p: longint): Boolean; override;
  private
    fIndex: TIndexList;
    fonly_id: Boolean;
  end;

  XIndexing = class(tdmScan)
    constructor Create(dm: tdm_Map;
                       AIndex: TIntegerList);

    function This_Object(p: longint): Boolean; override;
  private
    fIndex: TIntegerList;
  end;

  TIndexCtrl = class(tdmScan)
    constructor Create(dm: tdm_Map;
                       ACtrl: TIndexControl);

    function This_Object(p: longint): Boolean; override;
  private
    fCtrl: TIndexControl;
  end;

constructor TIndexing.Create(dm: tdm_Map;
                             AIndex: TIndexList;
                             only_id: Boolean);
begin
  inherited Create(dm);
  AIndex.Clear; fIndex:=AIndex;
  fonly_id:=only_id
end;

function TIndexing.This_Object(p: longint): Boolean;
var
  id: Integer;
begin
  if not dm_.xGet_Int(p,1000,id) then id:=0;
  if fonly_id or (id > 0) then fIndex.id_Add(id,p);
  Result:=false
end;

constructor XIndexing.Create(dm: tdm_Map;
                             AIndex: TIntegerList);
begin
  inherited Create(dm);
  AIndex.Clear; fIndex:=AIndex;
end;

function XIndexing.This_Object(p: longint): Boolean;
var
  id: Integer;
begin
  if dm_.xGet_Int(p,1000,id) then
  fIndex.Add(@id); Result:=false
end;

constructor TIndexCtrl.Create(dm: tdm_Map;
                             ACtrl: TIndexControl);
begin
  inherited Create(dm);
  fCtrl:=ACtrl; if fCtrl.Bits = nil then
  fCtrl.max_NNNN:=dm.Get_next_nnnn
end;

function TIndexCtrl.This_Object(p: longint): Boolean;
var
  id: Integer;
begin
  if dm_.xGet_Int(p,1000,id) then
  fCtrl.Control(id); Result:=false
end;

function dm_Indexing(dm: tdm_Map; AIndex: TIndexList;
                     only_id: Boolean): Integer;
var
  scan: TIndexing;
begin
  scan:=TIndexing.Create(dm,AIndex,only_id);
  scan.Scan_childs(0); scan.Free;
  Result:=AIndex.Count
end;

function dm_Index_list(dm: tdm_Map; AIndex: TIntegerList): Integer;
var
  scan: XIndexing;
begin
  scan:=XIndexing.Create(dm,AIndex);
  scan.Scan_childs(0); scan.Free;
  Result:=AIndex.Count
end;

procedure dm_Index_ctrl(dm: tdm_Map; ACtrl: TIndexControl);
var
  scan: TIndexCtrl;
begin
  scan:=TIndexCtrl.Create(dm,ACtrl);
  scan.Scan_childs(0); scan.Free;
end;

type
  tdmIndexRange = class(tdmScan)
    function This_Object(run: longint): Boolean; override;
  private
    fminv,fmaxv,fcount: Integer;
  end;

function tdmIndexRange.This_Object(run: longint): Boolean;
var
  id: Longint;
begin
  Result:=false;

  if dm_.Get_hf(run,1000,_long,id) then begin

    if fcount = 0 then begin
      fminv:=id; fmaxv:=id;
    end
    else begin
      fminv:=Min(fminv,id);
      fmaxv:=Max(fmaxv,id);
    end;

    Inc(fcount)
  end
end;

function dm_Index_range(dm: tdm_Map;
                        out minv,maxv: Integer): Integer;
var
  scan: tdmIndexRange;
begin
  Result:=0; minv:=0; maxv:=0;

  if dm.Enabled_Map then begin

    scan:=tdmIndexRange.Create(dm);
    try
      scan.Scan_childs(0);

      if scan.fcount > 0 then begin
        minv:=scan.fminv;
        maxv:=scan.fmaxv;
        Result:=scan.fcount
      end;

    finally
      scan.Free;
    end;
  end
end;

type
  tdmVersion = class(tdmScan)
    function This_Object(run: longint): Boolean; override;
  end;

function tdmVersion.This_Object(run: longint): Boolean;
var
  ind,len,loc: int; v: VLLine; s: string;
begin
  Result:=false;

  with dmNode.dRec do
  if Tag = 4 then with dm_ do
  if not Get_HF(run,9,_string,s) then begin

    ind:=mf.vmx_Block(mind,len);

    if len > 8+1 then begin
      v.Pol[0]:=o_lt;
      ind:=mf.vm_Load(ind,v.Pol[1],8); v.N:=1;
      if Points_Equal(v.Pol[0],v.Pol[1]) then v.N:=0;

      loc:=mf.vm_Byte(ind);

      if loc = len-8-1 then
      if loc < 255 then begin
        mf.vm_Load(ind,s,loc+1);

        Update_mf(run,0,@v);
        Put_hf(run,9,_string,s)
      end
    end
  end
end;

procedure dm_Update_Version(dm: tdm_Map);
var
  scan: tdmVersion; hdr: dm_Hdr;
begin
  if dm.Update_Map then begin

    scan:=tdmVersion.Create(dm);
    try
      scan.Scan_childs(0);
    finally
      scan.Free;
    end;

    dm.Get_dm_Hdr(hdr); hdr.ver:=1;
    dm.Put_dm_Hdr(hdr)
  end
end;

constructor xdmScan.Create(dm: tdm_Map);
begin
  inherited Create(dm);
  Owner_:=TList.Create
end;

destructor xdmScan.Destroy;
begin
  Owner_.Free; inherited
end;

function xdmScan.Goto_Down(p: longint): Boolean;
begin
  if p <> dm_.Tree.Root then
  if Assigned(Owner_) then
  Owner_.Add(Pointer(p));

  Result:=true
end;

procedure xdmScan.Back_Down(p: longint);
begin
  if Assigned(Owner_) then
  if Owner_.Count > 0 then
  Owner_.Delete(Owner_.Count-1)
end;

function xdmScan.Owner_run: longint;
begin
  Result:=0;
  if Assigned(Owner_) then
  if Owner_.Count > 0 then begin
    Result:=Integer(Owner_.Last);
    if Result = dm_.Tree.Root then
    Result:=0
  end
end;

function xdmScan.Owner_Code: longint;
var
  p: longint;
begin
  Result:=-1; p:=Owner_run;
  if p > 0 then Result:=dm_.Code_Object(p)
end;

function xdmScan.Owner_Loc: Integer;
var
  p: longint;
begin
  Result:=-1; p:=Owner_run;
  if p > 0 then Result:=dm_.Tag_Object(p)
end;

function xdmScan.Owner_Lev: Integer;
begin
  Result:=0;
  if Assigned(Owner_) then
  Result:=Owner_.Count
end;

procedure tdm_cl.Init_graphics;
begin
  Obj.Fill_fast(@fast);
  FillChar(cash,SizeOf(cash),$FF);
end;

function tdm_cl.Object_cl(Code,loc: Integer): Integer;
var
  ok: Boolean; oRec: obj_rec;
begin
  Result:=0; ok:=true;

  if (Code <> cash.Code) or (loc <> cash.Tag) then begin

    ok:=false; if (Code <> _Code) or (loc <> _loc) then
    if Obj.Obj_Index(Code,loc,oRec,@fast) > 0 then
    begin Cash:=oRec; ok:=true end

  end; _code:=Code; _loc:=loc;

  if ok then begin
    Result:=Cash.Color;
    if loc in [2,3] then
    tlong(Result).w[1]:=Cash.Pen
  end
end;

constructor tdm_zrange.Create(dm: tdm_Map; AClip: PLPoly;
                              Abuf: PLLine; Ahp: PIntegers;
                              Amesh: TPolyMesh);
begin
  inherited Create(dm);fClip:=AClip;
  fbuf:=Abuf; fhp:=Ahp; fmesh:=Amesh;

  fRange.min:=+1E+9; fRange.max:=-1E+9;
  if Assigned(fClip) then begin
    flt:=fClip[0]; frb:=fClip[1]
  end
end;

function tdm_zrange.Get_zrange(out zr: TRange): Boolean;
begin
  Scan_childs(0);
  zr:=fRange; Result:=zr.min <= zr.max
end;

function tdm_zrange.This_Object(p: longint): Boolean;
var
  zr: TRange;
begin
  Result:=false;

  with dmNode.dRec do
  if Tag > 0 then

  if (fClip = nil)
  or ((o_lt.x <= frb.x) and (o_lt.x <= frb.x) and
      (o_lt.y <= frb.y) and (o_lt.y <= frb.y)) then

  if Tag > 20 then

  else
  if dm_.Get_zrange(p,fClip,fbuf,fhp,fmesh, zr) then begin
    if zr.min < fRange.min then fRange.min:=zr.min;
    if zr.max > fRange.max then fRange.max:=zr.max;
  end
end;

function tdm_rebound.This_Object(p: longint): Boolean;
begin
  Result:=false;
  if p <> dm_.Tree.Root then

  with dmNode.dRec do
  if Tag > 0 then
  if (Code <> 0) or (Tag <> 1) then begin

    if fCount > 0 then
      Add_LRect(flt,frb,o_lt,o_rb)
    else begin
      flt:=o_lt; frb:=o_rb
    end;

    Inc(fCount)
  end
end;

procedure Init;
var
  sc: Integer;
begin
  Hole_Code:=StrToCode('H0000000');
  dmScaleList:=TIntegerList.Create;

  dmScaleList.AddItem(20000000);
  dmScaleList.AddItem(15000000);

  sc:=10000000;
  while sc >= 100 do begin
    dmScaleList.AddItem(sc);
    dmScaleList.AddItem(Round(sc * 0.75));
    dmScaleList.AddItem(Round(sc * 0.5));
    dmScaleList.AddItem(Round(sc * 0.3));
    dmScaleList.AddItem(Round(sc * 0.2));
    sc:=sc div 10
  end;

  dmScaleList.AddItem(10);

  attvStrings:=TStringList.Create;
  attvStrings.Add('');
  attvStrings.Add('“очность');
  attvStrings.Add(' ачество');
  attvStrings.Add('ћетка');

  if not rus_interface then begin

    attvStrings.Clear;
    xLoadStrings(attvStrings,'dmw.msg',2700,2703);

    if attvStrings.Count = 0 then begin
      attvStrings.Add('');
      attvStrings.Add('Exactness');
      attvStrings.Add('Quality');
      attvStrings.Add('Label');
    end
  end;

  attv402:=TStringList.Create;

  if rus_interface then begin
    xLoadStrings(attv402,'rus.msg',2800,2850);

    if attv402.Count = 0 then
    with attv402 do begin
      Add('-');
      Add('достоверное');
      Add('не достоверное');
      Add('малообследованное');
      Add('приближенное');
      Add('положение сомнительное');
      Add('не надежное');
      Add('по донесению (не достоверное)');
      Add('по донесению (не подтвержденное)');
      Add('полученное в результате оценки');
      Add('точно известное');
      Add('вычисленное');
    end
  end
  else begin
    xLoadStrings(attv402,'dmw.msg',2800,2850);

    if attv402.Count = 0 then
    with attv402 do begin
      Add('-');
      Add('reliable');
      Add('not reliable');
      Add('small investigate');
      Add('nearly');
      Add('doubt position');
      Add('not hope');
      Add('by message (not reliable)');
      Add('by message (not confirm)');
      Add('estimation');
      Add('exactly known');
      Add('calculate');
    end
  end
end;

initialization Init;

finalization
begin
  dmScaleList.Free
end;

end.