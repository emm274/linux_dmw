unit dmw_vars; interface

uses
  Classes,
  Math,LCLType,Controls,
  otypes,xcursors,xclasses,
  xlist,xlist1,xmovie,
  dmw_obj,dmw_dm,dmw_prj;

const
  only_any    = 0;
  only_geo    = 1;
  only_code   = 2;
  only_color  = 3;
  only_delete = 4;
  only_move   = 5;
  only_copy   = 6;
  only_info   = 7;
  only_text   = 8;                        
  only_calc   = 9;
  only_pmov   = 10;
  only_pdel   = 11;
  only_lmov   = 12;
  only_ladd   = 13;
  only_ldel   = 14;
  only_lock   = 15;
  only_ldiv   = 16;
  only_Import = 17;
  only_join   = 18;
  only_cls    = 19;
  only_pline  = 20;
  only_scale  = 21;
  only_lrun   = 22;
  only_next   = 23;
  only_point  = 24;
  only_cut    = 25;
  only_clip   = 26;
  only_hole   = 27;
  only_align  = 28;
  only_up     = 29;
  only_forest = 30;
  only_signs  = 31;
  only_pull   = 32;
  only_ltop   = 33;
  only_link   = 34;

  only_vc     = 35;
  only_ve     = 36;
  only_x_vc   = 37;
  only_x_ve   = 38;

  only_xmov   = 39;
  only_xedit  = 41;
  only_xpull  = 42;
  only_xdiv   = 43;
  only_odiv   = 44;

  only_swap   = 45;
  only_pcut   = 46;

  only_gpol   = 47;

  only_child  = 48;
  only_joinp  = 49;
  only_joinr  = 50;
  only_joino  = 51;
  only_nextc  = 52;

  only_rmov   = 53;
  only_smov   = 54;

  only_mirror = 55;
  only_rotate = 56;

type
  x_Edit_State =
  (st_Tool,st_Input,st_Rule,st_Import,
   st_Dup,st_Rotate,st_Mirror,st_Join,
   st_lmov,st_Scale,st_Object,st_Move,
   st_Points,st_Dial,st_Hole,st_Pull,
   st_Height,st_Ground,st_Punkt,st_Draft,
   st_Draft_move,st_Frame,st_Drag,st_Curve,
   st_Joinp,st_Joinr,st_Objectc,st_rmov);

const
  st_Cursor: array[x_Edit_State] of integer =
    (
     _cLocator, // st_Tool
     _cTarget,  // st_Input
     _cTarget,  // st_Rule
     _cLocator, // st_Import
     _xTarget,  // st_Dup
     _cTarget,  // st_Rotote
     _cTarget,  // st_Mirror
     _cLocator, // st_Join
     _cLocator, // st_lmov
     _cLocator, // st_Scale
     _cLocator, // st_Object
     _cTarget,  // st_Move
     crNone,    // st_Points
     crNone,    // st_Dial
     _cLocator, // st_Hole
     _cLocator, // st_Pull
     _cTarget,  // st_Height
     _cGround,  // st_Ground
     _cLocator, // st_Sign
     _cTarget,  // st_Draft
     _cTarget,  // st_Draft_move
     _cRegion,  // st_Frame
     _cRegion,  // st_Drag
     _cTarget,  // st_Curve
     _cLocator, // st_Joinp
     _cLocator, // st_Joinr
     _cLocator, // st_Objectc
     _cLocator  // st_rmov
    );

type
  tdm_Strings = class(TStringList)
    function iMsg(i: int; s: PChar): PChar;
    function iAdd(s: PChar): Boolean;
  private
    find: int; fmsg: TShortStr;
  end;

  TMoveList = class(TInt64List)
    function xAdd(P: Int64): Integer;
  private
    fEnabled: Boolean;
  public
    property Enabled: Boolean read fEnabled write fEnabled;
  end;

  pvc_disp = ^tvc_disp;
  tvc_disp = record
    Id: uint; Pos: lxyz
  end;

  pvc_disp_arr = ^tvc_disp_arr;
  tvc_disp_arr = Array[0..1023] of tvc_disp;

  tvc_List = class(TCustomList)
    constructor Create;
    function xAdd(const vc: tcn_rec): int;
    procedure xUpdate(const vc: tcn_rec);
  end;

  tve_List = class(TIntegerList)
  end;

  pfe_disp = ^tfe_disp;
  tfe_disp = record
    Ptr,Code: int;
    Loc,Lev: Word;

    case Integer of
  0:  (ox1,oy1,ox2,oy2: int);
  1:  (o_lt,o_rb: TPoint);
  end;

  tfe_ptr = int;
  pfe_arr = ^tfe_arr;
  tfe_arr = Array[0..1023] of tfe_ptr;

  tfe_List = class(TIntegerList)
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    function oAdd(Ptr,Code,Loc: int; const lt,rb: TPoint): int;

    function xAdd(Ptr,Lev: int; const dRec: dm_Rec): int;
    function xDel(Ptr: int): int;

    function Get_data(Ind,Ptr: int; var dRec: dm_Rec): bool;
    function Set_data(Ind,Ptr: int; const dRec: dm_Rec): bool;

    function Get_level(Ind,Ptr: int): int;
    function up_level(Ptr,up: int): int;

  private
    fData: TCustomList;
    fMaxDataCount: int;
  end;

  tdmw_List = class
    constructor Create(Adm: tdm_Map);
    destructor Destroy; override;

    procedure Clear;

    function Get_disp_list: TInt64List;

    function x_Display_Enabled: Boolean;

    function x_Enabled: Boolean;
    function x_Count: int;

    function x_Append(p: Int64): bool;
    procedure x_Remove(p: Int64);
    function x_Display(cn,id: uint): bool;

    function x_Update(p: Int64): int;
    function fe_Update(p: int): int;

    function x_IndexOf(cn,id: uint): int;

    procedure vc_Update(p: Int64);

    procedure grid_Display(const p: TPoint);

  private
    fdm_: tdm_Map;

    vc_: tvc_List;
    ve_: tve_List;
    fe_: tfe_List;

    grid_: TPointList;

    fDisp: TInt64List;
    fCtrl: TInt64List;

    fIsDisplay: Boolean;
    fIsEnabled: Boolean;
    fIsUpdated: Boolean;
    fIsScan: Boolean;

    fPath: TShortStr;

    procedure Set_IsDisplay(disp: Boolean);

    function fe_add(Ptr,Lev: int): bool;
    function vc_add(Id: uint): bool;

  public
    property dm_: tdm_Map read fdm_;
    property Ctrl: TInt64List write fCtrl;

    property Scan: Boolean read fIsScan write fIsScan;

    property IsDisplay: Boolean read fIsDisplay
                                write Set_IsDisplay;

    property IsEnabled: Boolean write fIsEnabled;
    property IsUpdated: Boolean write fIsUpdated;

    property x_vc: tvc_list read vc_;
    property x_ve: tve_list read ve_;
    property x_fe: tfe_list read fe_;

    property x_grid: TPointList read grid_;
  end;

const
  ClickMax = 16;

type
  TClickObject = record
    Code,Loc: Integer;
    nn: array[0..3] of Integer
  end;

  PClickArray = ^TClickArray;
  TClickArray = array[0..ClickMax-1] of TClickObject;

type
  PDraft = ^TDraft; TDraft = record
    N: Integer; Pol: VOrient;
    Id,Code,Flags: Integer;
    RF: GOrient; bufp: PLLine;
    mov_p: VPoint; movInd: Integer;
    str: TShortstr
  end;

  TDraftProc = procedure(lp: PDraft) of object;

  TDraftList = class(TXyzList)
  private
    fColor: Integer;
    ftxt_on: Integer;
  public
    property Color: Integer read fColor write fColor;
    property txt_on: Integer read ftxt_on write ftxt_on;
  end;

  tx_Edit = class
    Cursor,Tool: int;
    lp_Cross,lp_mode,input_mode: int;

    this_lock,this_curve,only_ellipse,lp_page,
    lp_edit,lp_update,lp_Vector,lp_Curve,lp_ellipse,
    lp_lock,lp_ins,lp_Pencil,is_Pencil,lp_Draw,
    lp_ort,lp_obj,lp_text,lp_sign,bl_Port: Longbool;

    IsCurvePoly: Longbool;
    ToolHint: int;

    lp_loc,lp_cnt: Integer;
    lp_ind,lp_max,vc_loc,lp_dir: Integer;

    lp_top: int; lp_topt: float;
    lp_bot: int; lp_bott: float;

    curve_t: float; curve_ind: TPoint;

    lmov_i1,lmov_i2,lmov_i3,lmov_k,pmov_i: int;
    lmov_t1: float;

    lmov_lock,pmov_ins: Longbool;
    pmov_dup,pmov_xyz: Longbool;
    pmov_ext,lmov_ext: Longbool;

    lmov_v1,lmov_v2,lmov_v3,ort_v: VPoint;

    rr_func: Integer; rr_p: TPoint;

    ve_p1,ve_p2: TInt64;

    VecBuf: VVector; LVecInd: Integer;

    z_Rule: TVBound; is_Depth: Longbool;
    z_Rule_a,z_Rule_b: TMovieRule;

    z_Ground: VPoint;
    is_Ground: Longbool;
    h4_hint: Longbool;

    Draft: TDraft;
    DraftList: TDraftList;

    Draft_a,Draft_b: TMovieDraft;

    DPolyBuf: PLLine;
    DHeights: PIntegers;

    DPolyBuf1: PLLine;
    DHeights1: PIntegers;

    DPolyBuf2: PLLine;
    DHeights2: PIntegers;

    XPolyBuf: PLLine;
    LDemoBuf: PLLine;

    LPolyBuf: PLLine;
    LHeights: PIntegers;
    LPolyBuf_Lock: Longbool;
    LPolyBuf_Xyz: int;
    LPolyBuf_cn: int;

    State: x_Edit_State;
    State1: x_Edit_State;
    ext_State: x_Edit_State;

    ext_Tool: Integer;
    ext_Tool1: Integer;

    plan_k,page_k: double;
    mpp,line_w,line_r: double;

    Port_db,Port_dl: Double;

    link_Enabled: Longbool;
    Link_v: GVector;
    link_i: Integer;

    dmObj: Obj_Rec;
    Obj_Ind: Integer;

    CurveBuf: VLLine;

    rule_k: Double;
    rule_h: Double;
    rule_op: Integer;
    rule_nn: Integer;

    Print: GVector;
 
    constructor Create;
    destructor Destroy; override;

    procedure xLog(const Str: String);

    function Change_Tool(New: Integer): Integer;

    function Input_Curve: Boolean;
    function Input_Ellipse: Boolean;
    function Input_Cursor: hCursor;

    function Curve_Continue: Boolean;
    function Sector_Continue: Boolean;

    function Swap_LPolybuf(curve: Boolean): PIntegers;
    function Swap_DPolybuf(curve: Boolean): PIntegers;

    function Copy_LPolybuf(out lp_z: PIntegers): PLLine;

    function Load_LPolybuf(lp: PLLine; lp_z: PIntegers): Integer;

    function LPolybuf_to_DPolybuf: Integer;
    function LPolybuf_to_DPolybuf1: Integer;
    function DPolybuf_to_DPolybuf1: Integer;
    function DPolybuf_to_LPolybuf: Integer;

    function LPolybuf_Cut(Ind,Count: int): int;
    function DPolybuf_Cut(Ind,Count: int): int;

    procedure LPolybuf_Clear_all;
    procedure LPolybuf_Clear;

    function LPolybuf_Insert(ind: Integer;
                             const v: vpoint): Integer;

    function LPolybuf_Append(lp: PLLine; lp_z: PIntegers;
                             dist: Double): bool;

    function LPolybuf_Swap(lp: PLLine; curve: Boolean): Boolean;

    function lock_LPolybuf: bool;

    function Is_curve_LPolybuf: bool;

    function poly_LPolybuf(eps: double): bool;
    function curve_LPolybuf: bool;

    function Is_LPolybuf_xyz: Boolean;

    function lp_loc_Edge: Boolean;
    function lp_loc_ins: Integer;

  private
    fPolyBuf1: TPolyList;
    fPolyBuf2: TPolyList;

    fOccuped: Integer;
    fdm_Loc: VPoint;

    function Get_Xyz_Points: Boolean;
    procedure Set_Xyz_Points(Value: Boolean);

    procedure Set_Occuped(Value: Integer);

    function Get_LVecBuf(I: Integer): TPoint;
    procedure Set_LVecBuf(I: Integer; const P: TPoint);

    function Get_LPoint(I: Integer): VPoint;
    procedure Set_LPoint(I: Integer; const V: VPoint);

    function Get_DPoint(I: Integer): VPoint;
    procedure Set_DPoint(I: Integer; const V: VPoint);

    function Get_DPoint1(I: Integer): VPoint;

    function Get_LPoints_Last: VPoint;
    procedure Set_LPoints_Last(const v: VPoint);

    function Get_LPoints_lock: Boolean;

    function Get_lp_LPoints(I: Integer): TPoint;

    function Get_dmLoc: TPoint;
    procedure Set_dmLoc(const P: TPoint);

    function Set_dm_Loc(const V: VPoint;
                        i: Integer): Integer;

    function Get_lmov_p1: TPoint;
    function Get_lmov_p2: TPoint;

    procedure Set_lmov_p1(const p: TPoint);
    procedure Set_lmov_p2(const p: TPoint);

  public
    property Xyz_Points: Boolean read Get_Xyz_Points
                                 write Set_Xyz_Points;

    property Occuped: Integer read fOccuped write Set_Occuped;

    property PolyBuf1: TPolyList read fPolybuf1;
    property PolyBuf2: TPolyList read fPolybuf2;

    property LVecBuf[I: int]: TPoint read Get_LVecBuf
                                     write Set_LVecBuf;

    property LPoints[I: int]: VPoint read Get_LPoint
                                     write Set_LPoint;

    property DPoints[I: int]: VPoint read Get_DPoint
                                     write Set_DPoint;

    property DPoints1[I: int]: VPoint read Get_DPoint1;

    property LPoints_lock: Boolean read Get_LPoints_lock;

    property LPoints_Last: VPoint read Get_LPoints_Last
                                  write Set_LPoints_Last;

    property lp_LPoints[I: int]: TPoint read Get_lp_LPoints;

    property dmLoc: TPoint read Get_dmLoc write Set_dmLoc;
    property dm_Loc: VPoint read fdm_Loc write fdm_Loc;

    property lmov_p1: TPoint read Get_lmov_p1 write Set_lmov_p1;
    property lmov_p2: TPoint read Get_lmov_p2 write Set_lmov_p2;

    procedure LPoints_Restore_z_axe(var V: VPoint; Ind: Integer);
    procedure DPoints_Restore_z_axe(var V: VPoint; Ind: Integer);
  end;

  TObjectsFunc = function(P1,P2: Int64): Boolean of object;

  tdmQuery = class(tdmScan)

    constructor Create(dm: tdm_Map;
                       AList: tdmw_List;
                       AEdit: tx_Edit);

    procedure SetOccupe(p,p1: Int64);

    procedure SetLocator(const lt,rb,w_lt,w_rb: TPoint; lev: Integer);
    procedure xxxLocator(const lt,rb: TPoint; dm_to_bmp: TXYZ_to_XY);

    procedure MoveLocator(const lt,rb: TPoint);

    function First(Top,Only: int;
                   AClickList: TDmwClickList): Int64;

    function Second(Top,Only: int): Int64;

    function Next(P: Int64; Only: int;
                  AClickList: TDmwClickList): Int64;

    function That(P: Int64; Only: int): Boolean;

    function This_Object(p: longint): Boolean; override;

    function Only_Object(p: Integer): Boolean; virtual;
    function Only_Node(id: Cardinal): Boolean; virtual;
    function Only_Edge(id: Cardinal): Boolean; virtual;

    function Pickup_vc(out V: VPoint): uint;
    function Pickup_grid(out V: VPoint): Boolean;

    function This_poly(lp: PLLine; scan: Boolean): Boolean;

    function That_poly(lp: PLLine; loc: Integer): Boolean;

    function dm_ThisPoly(p: Int64;
                         scan: Boolean): Integer;

    function dm_This_LPoint(p: Int64; lp_i: Integer): Boolean;

    function this_Occupe_run(p: Int64): Boolean;
    function this_Occupe_run1(p: Int64): Boolean;

    function this_Occupe_Object(Code,Loc: Integer): Boolean;

    function Get_that_dist: Integer;

    procedure disp_List_Clear;

  private
    x_Edit: tx_Edit;

    fDispList: tdmw_list;
    fClickList: TDmwClickList;

    fHints: THintList;

    child: longint;

    fIsDown,fIsTick: bool;
    fIsThat,fInSide: bool;
    fIsJoin: bool;

    fOccupe_p: Int64;
    fOccupe_p1: Int64;

    fOccupe_dm: Integer;

    fOccupe_Code: Integer;
    fOccupe_Loc: Integer;

    this_lt,this_rb,this_loc,win_lt,win_rb: TPoint;
    this_Level,fThis_Only,fThis_id,this_d: int;

    that_loc: TPoint;
    this_xyz: Longbool;

    fdm_to_bmp: TXYZ_to_XY;
    fHeights: PIntegers;

    fBreakCount: int;

    fx_Disp: tx_Disp;

    fOnCompare: TObjectsFunc;

    procedure log(const Msg: String);

    function Goto_Down(p: Integer): Boolean; override;

    function Scan_List(prd,nxt: Int64): Int64;

    function click_Object(cn,id,lev,mf: int; pt: PPoint): Integer;

    function ext_This_Object(cn,id: uint): Boolean;

    function This_LPoint(const v: TPoint): Boolean;

    function dm_GetPoly(p: Int64): PLLine;
    function dm_GetPoint(p: Int64; lp_i: Integer): VPoint;

    function Occupe_lp_loc(p: Int64; lp_i: Integer): Integer;
    function Occupe_this_loc: Boolean;

  public
    property DispList: tdmw_List read fDispList
                                  write fDispList;

    property Hints: THintList write fHints;

    property This_Only: Integer read FThis_Only;
    property This_Id: Integer read FThis_Id;
    property IsTick: bool read FIsTick;

    property Occupe_p: Int64 read fOccupe_p;

    property InSide: bool write fInSide;
    property IsJoin: bool write fIsJoin;

    property x_Disp: tx_Disp write fx_Disp;

    property OnCompare: TObjectsFunc write fOnCompare;
  end;

var
  dmw_Strings: tdm_Strings;

function Draft_identify: TDraft;

function dmw_rus_msg(Name,Def: PChar): PChar;

function dmw_Capt(capt: PChar): PChar;
function dmw_Prompt(Msg: PChar; id: Integer): PChar;

function dmw_Message(Msg,Def: PChar; Id: Integer): PChar;
function dmw_Message1(Def: PChar; Id: Integer): String;

procedure dmw_About(Msg: PChar; Id: Integer);
function dmw_Answer(Msg: PChar; Id: Integer): integer;
function dmw_Warning(Msg: PChar; Id: Integer): Boolean;
function dmx_Warning(Msg,List: PChar; Id: Integer): Boolean;

function dmw_Warning_Delete_Object: Boolean;

function Init_ClickArray(Click: PClickArray;
                         ini: PChar): Integer;

function This_click_object(dm: tdm_map; p: Integer;
                           Click: PClickArray;
                           Count: Integer): Boolean;

implementation

uses
  SysUtils,
  convert,ofiles,xini,
  xline,xpoly,xcurve,
  xdlg,xkeys,dmw_bln,
  xlog;

const
  log_query = false;

function Draft_identify: TDraft;
var
  r: TDraft;
begin
  Fillchar(r,Sizeof(r),0);
  r.N:=-1; Result:=r
end;

function tdm_Strings.iMsg(i: Integer; s: PChar): PChar;
var
  iv,it: int; ts: tShortStr;
begin
  Result:=nil;

  if i = find then
    Result:=StrCopy(s,fmsg)
  else begin
    Is_Comma_Delimiter:=false;

    for iv:=0 to Count-1 do
    if StrLen(StrPCopy(ts,Strings[iv])) > 0 then
    if IntToken(ts,it) then if it = i then begin
      find:=i; StrMsg(fmsg,ts,'"');
      Result:=StrCopy(s,fmsg); Break
    end;

    Is_Comma_Delimiter:=true
  end;
end;

function tdm_Strings.iAdd(s: PChar): Boolean;
var
  i: Integer; t: TShortStr;
begin
  Result:=true; if StrCopy(t,s) <> nil then
  if IntToken(t,i) and (i > 0) then

  if i < 900 then
    Add(s)
  else
    Result:=false
end;

function TMoveList.xAdd(P: Int64): Integer;
begin
  Result:=-1; if fEnabled then
  Result:=Add(P)
end;

constructor tvc_List.Create;
begin
  inherited Create(SizeOf(tvc_disp),64)
end;

function tvc_List.xAdd(const vc: tcn_rec): int;
var
  r: tvc_disp;
begin
  r.Id:=vc.rcid;
  r.Pos.v:=vc.VPos;
  Result:=Add(@r)
end;

procedure tvc_List.xUpdate(const vc: tcn_rec);
var
  p: pvc_disp;
begin
  p:=id_Itemof(vc.rcid);

  if Assigned(p) then
    p.Pos.v:=vc.VPos
  else
    xAdd(vc)
end;

constructor tfe_List.Create;
begin
  inherited Create; Increment:=4096;
  fData:=TCustomList.Create(Sizeof(tfe_disp),4096);
  fMaxDataCount:=4096*4
end;

destructor tfe_List.Destroy;
begin
  fData.Free; inherited
end;

procedure tfe_List.Clear;
begin
  inherited Clear;
  fData.Clear
end;

function tfe_List.oAdd(Ptr,Code,Loc: int; const lt,rb: TPoint): int;
var
  r: tfe_disp;
begin
  Result:=AddItem(Ptr);

  if Result >= 0 then
  if Count <= fMaxDataCount then begin
    r.Ptr:=Ptr;
    r.Code:=Code; r.Loc:=Loc;
    r.o_lt:=lt; r.o_rb:=rb;
    fData.Add(@r)
  end
end;

function tfe_List.xAdd(Ptr,Lev: int; const dRec: dm_Rec): int;
var
  r: tfe_disp;
begin
  Result:=AddItem(Ptr);

  if Result >= 0 then
  if Count <= fMaxDataCount then begin
    r.Ptr:=Ptr;
    with dRec do begin
      r.Code:=Code; r.Loc:=Tag;
      r.o_lt:=o_lt; r.o_rb:=o_rb;
      r.Lev:=dRec.View;
    end;

    if Lev >= 0 then r.Lev:=Lev;
    fData.Add(@r)
  end
end;

function tfe_List.xDel(Ptr: int): int;
begin
  Result:=DeleteItem(Ptr);
  if Result >= 0 then
  fData.Delete(Result)
end;

function tfe_List.Get_data(Ind,Ptr: int; var dRec: dm_Rec): bool;
var
  p: pfe_disp;
begin
  Result:=false; p:=fData[Ind];
  if Assigned(p) then
  if p.Ptr = Ptr then begin

    with dRec do begin
      Code:=p.Code; Tag:=p.Loc;
      o_lt:=p.o_lt; o_rb:=p.o_rb;
      View:=p.Lev
    end;

    Result:=true
  end
end;

function tfe_List.Set_data(Ind,Ptr: int; const dRec: dm_Rec): bool;
var
  p: pfe_disp;
begin
  Result:=false; p:=fData[Ind];
  if Assigned(p) then
  if p.Ptr = Ptr then begin

    with dRec do begin
      p.Code:=Code; p.Loc:=Tag;
      p.o_lt:=o_lt; p.o_rb:=o_rb;
    end;

    Result:=true
  end
end;

function tfe_List.Get_level(Ind,Ptr: int): int;
var
  p: pfe_disp;
begin
  Result:=0;

  if Ind < 0 then
  Ind:=Indexof(Ptr);

  if Ind >= 0 then begin
    p:=fData[Ind];
    if Assigned(p) then
    if p.Ptr = Ptr then
    Result:=p.Lev
  end
end;

function tfe_List.up_level(Ptr,up: int): int;
var
  p: pfe_disp;
begin
  Result:=IndexOf(Ptr);
  if Result >= 0 then begin
    p:=fData[Result];
    if Assigned(p) then
    Inc(p.Lev,up)
  end
end;

constructor tdmw_List.Create(Adm: tdm_Map);
begin
  inherited Create;

  vc_:=tvc_List.Create;
  ve_:=tve_List.Create;
  fe_:=tfe_List.Create;

  grid_:=TPointList.Create(256);

  fDisp:=TInt64List.Create;

  fdm_:=Adm;
end;

destructor tdmw_List.Destroy;
begin
  fDisp.Free;
  
  grid_.Free;

  fe_.Free;
  ve_.Free;
  vc_.Free;

  inherited
end;

procedure tdmw_List.Clear;
begin
  grid_.Clear;

  fe_.Clear;
  ve_.Clear;
  vc_.Clear;

  inherited
end;

function tdmw_List.x_Display_Enabled: Boolean;
begin
  Result:=fIsDisplay and fIsEnabled;
end;

function tdmw_List.x_Enabled: Boolean;
begin
  Result:=false;
  if dm_.Tree.vm_This(fPath) then
  Result:=true else Clear
end;

function tdmw_List.x_Count: int;
begin
  Result:=vc_.Count + ve_.Count + fe_.Count
end;

function tdmw_List.Get_disp_list: TInt64List;
var
  i: int; ip: PIntegers; p: TInt64;
  vp: pvc_disp_arr;
begin
  fDisp.Clear;

  if x_Enabled then begin
    p.cn:=cn_node; vp:=vc_.First;
    for i:=0 to vc_.Count-1 do begin
      p.id:=vp[i].Id; fDisp.Add(p.x)
    end;

    p.cn:=cn_edge; ip:=ve_.First;
    for i:=0 to ve_.Count-1 do begin
      p.id:=ip[i]; fDisp.Add(p.x)
    end;

    p.cn:=0; ip:=fe_.First;
    for i:=0 to fe_.Count-1 do begin
      p.id:=ip[i]; fDisp.Add(p.x)
    end
  end;

  Result:=fDisp
end;

function tdmw_List.fe_add(Ptr,Lev: int): bool;
var
  dRec: dm_Rec;
begin
  Result:=false;
  if dm_.Enabled_Map then begin
    dm_.Get_Object(Ptr,dRec);
    fe_.xAdd(Ptr,Lev,dRec);
    Result:=true
  end
end;

function tdmw_List.vc_add(Id: uint): bool;
var
  vc: tcn_rec;
begin
  Result:=false;

  if dm_.Enabled_Map then
  if dm_.Tree.get_vc(Id,@vc) then
  Result:=vc_.xAdd(vc) >= 0
end;

function tdmw_List.x_Append(p: Int64): bool;
var
  x: TInt64;
begin
  Result:=false; x.x:=p;

  if x.id > 0 then
  if x_Enabled then

  if x.cn = 0 then
    Result:=fe_add(x.id,0)
  else
  if x.cn = cn_node then
    Result:=vc_add(x.id)
  else
  if x.cn = cn_edge then begin
    ve_.AddItem(x.id); Result:=true
  end;

  if Result then
  if Assigned(fCtrl) then fCtrl.Add(p);
end;

procedure tdmw_List.x_Remove(p: Int64);
var
  x: TInt64;
begin
  x.x:=p;
  if x_Enabled then

  if x.cn = 0 then
    fe_.xDel(x.id)
  else
  if x.cn = cn_node then
    vc_.id_Delete(x.id)
  else
  if x.cn = cn_edge then
    ve_.DeleteItem(x.id)
end;

function tdmw_List.x_Display(cn,id: uint): bool;
var
  x: TInt64;
begin
  Result:=fIsDisplay and fIsEnabled;

  x.cn:=cn; x.id:=id;
  if Result then
    x_Append(x.x)
  else
  if fIsUpdated then
  if x_IndexOf(cn,id) < 0 then
    x_Append(x.x)
end;

function tdmw_List.x_Update(p: Int64): int;
var
  x: TInt64;
begin
  Result:=-1;

  if p > 0 then
  if x_Enabled then begin x.x:=p;
    Result:=x_IndexOf(x.cn,x.id);
    if Result < 0 then x_Append(p)
  end
end;

function tdmw_List.fe_Update(p: int): int;
var
  dRec: dm_Rec;
begin
  Result:=-1;
  if x_Enabled then begin
    Result:=fe_.IndexOf(p);
    if Result >= 0 then
    if dm_.Enabled_Map then begin
      dm_.Get_Object(p,dRec);
      fe_.Set_data(Result,p,dRec)
    end
  end
end;

procedure tdmw_List.Set_IsDisplay(disp: Boolean);
begin
  fIsDisplay:=disp; if disp then
  StrCopy(fPath,dm_.Tree.vm_Path);
  fIsEnabled:=true
end;

function tdmw_List.x_IndexOf(cn,id: uint): int;
begin
  Result:=-1;
  if cn = 0 then
    Result:=fe_.IndexOf(id)
  else
  if cn = cn_node then
    Result:=vc_.id_Indexof(id)
  else
  if cn = cn_edge then
    Result:=ve_.IndexOf(id);
end;

procedure tdmw_List.vc_Update(p: Int64);
var
  x: TInt64; vc: tcn_rec;
begin
  x.x:=p; if x.cn = cn_node then
  if dm_.Enabled_Map then
  if dm_.Tree.get_vc(x.id,@vc) then
  vc_.xUpdate(vc)
end;

procedure tdmw_List.grid_Display(const p: TPoint);
begin
  grid_.AddItem(p.X,p.Y)
end;

constructor tx_Edit.Create;
begin
  inherited Create;

  DraftList:=TDraftList.Create(256);

  LPolyBuf:=Alloc_LPolyBuf;
  DPolyBuf:=Alloc_LPolyBuf;
  DPolyBuf1:=Alloc_LPolyBuf;
  DPolyBuf2:=Alloc_ZPolyBuf(DHeights2);
  XPolyBuf:=Alloc_LPolyBuf;

  LDemoBuf:=nil; lp_ind:=-1;
  CurveBuf.N:=-1; line_w:=-1; line_r:=-1;

  z_Rule.N:=-1;
  z_Rule_a:=TMovieRule.Create;
  z_Rule_b:=TMovieRule.Create;

  Draft.N:=-1;
  Draft_a:=TMovieDraft.Create;
  Draft_b:=TMovieDraft.Create;

  fPolyBuf1:=TPolyList.Create;
  fPolyBuf2:=TPolyList.Create;

  IsCurvePoly:=ini.DefBool(false,'IsCurvePoly');

  Change_Tool(0)
end;

destructor tx_Edit.Destroy;
begin
  ini.PutBool('IsCurvePoly',IsCurvePoly);

  fPolyBuf2.Free;
  fPolyBuf1.Free;

  Draft_b.Free;
  Draft_a.Free;

  z_Rule_b.Free;
  z_Rule_a.Free;

  xFreePtr(LDemoBuf);
  xFreePtr(DHeights);

  xFreePtr(XPolyBuf);
  xFreePtr(DPolyBuf2);
  xFreePtr(DPolyBuf1);
  xFreePtr(DPolyBuf);
  xFreePtr(LPolyBuf);

  DraftList.Free;

  inherited
end;

procedure tx_Edit.xLog(const Str: String);
begin
  if log_enabled then
  std_log.Write_str('x_Edit',Str)
end;

function tx_Edit.Get_Xyz_Points: Boolean;
begin
  Result:=Assigned(DHeights)
end;

procedure tx_Edit.Set_Xyz_Points(Value: Boolean);
var
  max: Integer;
begin
  if not Value then begin
    DHeights:=xFreePtr(DHeights);
    LHeights:=nil; DHeights1:=nil
  end else
  if not Assigned(DHeights) then begin
    max:=LPolyMax+1;
    DHeights:=xAllocInt(max+max+max);
    if Assigned(DHeights) then begin
      LHeights:=@DHeights[max];
      DHeights1:=@LHeights[max];
    end
  end
end;

procedure tx_Edit.Set_Occuped(Value: Integer);
begin
  fOccuped:=Value;
  is_depth:=false
end;

function tx_Edit.Get_LVecBuf(I: Integer): TPoint;
begin
  Result.x:=VecBuf[I].x;
  Result.y:=VecBuf[I].y;
end;

procedure tx_Edit.Set_LVecBuf(I: Integer; const P: TPoint);
begin
  VecBuf[I].x:=P.x;
  VecBuf[I].y:=P.y;
  VecBuf[I].z:=0
end;

function tx_Edit.Get_LPoint(I: Integer): VPoint;
begin
  Result.x:=LPolyBuf.Pol[I].x;
  Result.y:=LPolyBuf.Pol[I].y;
  Result.z:=0; if Assigned(LHeights) then
  Result.z:=LHeights[I]
end;

procedure tx_Edit.Set_LPoint(I: Integer; const V: VPoint);
begin
  LPolyBuf.Pol[I].x:=V.x;
  LPolyBuf.Pol[I].y:=V.y;
  if Assigned(LHeights) then
  LHeights[I]:=V.z
end;

function tx_Edit.Get_DPoint(I: Integer): VPoint;
begin
  Result.x:=DPolyBuf.Pol[I].x;
  Result.y:=DPolyBuf.Pol[I].y;
  Result.z:=0; if Assigned(DHeights) then
  Result.z:=DHeights[I]
end;

function tx_Edit.Get_DPoint1(I: Integer): VPoint;
begin
  Result.x:=DPolyBuf1.Pol[I].x;
  Result.y:=DPolyBuf1.Pol[I].y;
  Result.z:=0; if Assigned(DHeights1) then
  Result.z:=DHeights1[I]
end;

procedure tx_Edit.Set_DPoint(I: Integer; const V: VPoint);
begin
  DPolyBuf.Pol[I].x:=V.x;
  DPolyBuf.Pol[I].y:=V.y;
  if Assigned(DHeights) then
  DHeights[I]:=V.z
end;

function tx_Edit.Get_LPoints_Last: VPoint;
var
  i: int; v: VPoint;
begin
  v.x:=0; v.y:=0; v.z:=0;

  with LPolyBuf^ do begin
    i:=N; if lp_curve then Dec(i);
    if (i >= 0) and (i <= N) then
    Result:=LPoints[i]
  end
end;

procedure tx_Edit.Set_LPoints_Last(const v: VPoint);
var
  i: int;
begin
  with LPolyBuf^ do begin
    i:=N; if lp_curve then Dec(i);
    if (i >= 0) and (i <= N) then
    LPoints[i]:=v
  end
end;

function tx_Edit.Get_LPoints_lock: Boolean;
begin
  if lp_Curve then
    Result:=CurveLock(LPolybuf)
  else
  if lp_Ellipse then
    Result:=LPolybuf.N > 0
  else
    Result:=PolyLock(LPolybuf)
end;

function tx_Edit.Get_lp_LPoints(I: Integer): TPoint;
var
  p: TPoint;
begin
  p.x:=0; p.y:=0;

  if lp_ind >= 0 then I:=lp_ind-I;

  with LPolybuf^ do
  if (I >= 0) and (I <= N) then
  p:=Pol[I];

  Result:=p
end;

function tx_Edit.Get_dmLoc: TPoint;
begin
  Result.x:=fdm_loc.x; Result.y:=fdm_loc.y;
end;

procedure tx_Edit.Set_dmLoc(const P: TPoint);
begin
  fdm_loc.x:=p.x; fdm_loc.y:=p.y; fdm_loc.z:=0;
end;

function tx_Edit.Set_dm_Loc(const V: VPoint;
                            i: Integer): Integer;
begin
  fdm_loc:=V; lp_loc:=i;
  curve_t:=-1; Result:=i
end;

function tx_Edit.Get_lmov_p1: TPoint;
begin
  Result.X:=lmov_v1.x;
  Result.Y:=lmov_v1.y;
end;

function tx_Edit.Get_lmov_p2: TPoint;
begin
  Result.X:=lmov_v2.x;
  Result.Y:=lmov_v2.y;
end;

procedure tx_Edit.Set_lmov_p1(const p: TPoint);
begin
  lmov_v1.x:=p.X; lmov_v1.y:=p.Y; lmov_v1.z:=0;
end;

procedure tx_Edit.Set_lmov_p2(const p: TPoint);
begin
  lmov_v2.x:=p.x; lmov_v2.y:=p.y; lmov_v2.z:=0;
end;

procedure tx_Edit.LPoints_Restore_z_axe(var V: VPoint; Ind: Integer);
begin
  LPoly_Restore_z_axe(LPolybuf,LHeights, V,Ind)
end;

procedure tx_Edit.DPoints_Restore_z_axe(var V: VPoint; Ind: Integer);
begin
  LPoly_Restore_z_axe(DPolybuf,DHeights, V,Ind)
end;

function tx_Edit.Change_Tool(New: Integer): Integer;
begin
  state:=st_Tool; ext_Tool:=0; lp_page:=false;

  if New <> Tool then Link_i:=0;
  Tool:=New; Occuped:=0; curve_ind:=Point(-1,-1);

  lp_Vector:=false; lp_Pencil:=false;
  is_Pencil:=false; bl_Port:=false;

  LVecInd:=-1; lp_ind:=-1;
  lp_ort:=false; lp_obj:=false;

  LPolyBuf.N:=-1; XPolyBuf.N:=-1;
  CurveBuf.N:=-1; Input_mode:=lp_mode;

  LPolybuf_xyz:=0; LPolybuf_cn:=0;

  lp_edit:=false; lp_draw:=false;
  lp_Curve:=false; lp_ellipse:=false;
  only_ellipse:=false;

  Cursor:=_cLocator; Result:=Tool
end;

function tx_Edit.Input_Curve: Boolean;
begin
  Result:=false;
  if LPolyBuf.N = -1 then lp_Curve:=false;

  if lp_Curve then Result:=true else

  if LPolyBuf.N = -1 then
  if lp_mode = 1 then Result:=true
end;

function tx_Edit.Input_Ellipse: Boolean;
begin
  Result:=false;
  if LPolyBuf.N = -1 then lp_Ellipse:=false;

  if lp_Ellipse then Result:=true else

  if not lp_text then
  if Abs(lp_max) > 1 then if LPolyBuf.N = -1 then
  if lp_mode = 3 then Result:=true else
  Result:=only_ellipse
end;

function tx_Edit.Input_Cursor: hCursor;
begin
  if lp_Vector then Result:=_cCursor else
  if lp_Pencil then Result:=_pTarget
  else Result:=_cTarget
end;

function tx_Edit.Curve_Continue: Boolean;
begin
  Result:=false;
  if not lp_curve then if Input_Mode = 1 then
  if lp_ind >= 0 then if lp_ind = LPolyBuf.N then
  if Abs(lp_max) > 1 then Result:=true
end;

function tx_Edit.Sector_Continue: Boolean;
begin
  Result:=false;
  if not lp_curve then if not lp_ellipse then
  if input_Mode = 3 then

  if lp_ind < 0 then
    Result:=LPolyBuf.N > 0
  else
  if lp_ind >= 1 then

  if lp_ind = LPolyBuf.N then
    Result:=true
  else
  if lp_lock then
  if lp_ind = LPolyBuf.N-1 then
    Result:=true
end;

function tx_Edit.Swap_LPolybuf(curve: Boolean): PIntegers;
begin
  Result:=nil; if curve then
    Swap_Curve(LPolybuf)
  else begin
    with LPolyBuf^ do
    Swap_LPoly(@Pol,LHeights,N);
    Result:=LHeights
  end
end;

function tx_Edit.Swap_DPolybuf(curve: Boolean): PIntegers;
begin
  Result:=nil; if curve then
    Swap_Curve(DPolybuf)
  else begin
    with DPolyBuf^ do
    Swap_LPoly(@Pol,DHeights,N);
    Result:=DHeights
  end
end;

function tx_Edit.Copy_LPolybuf(out lp_z: PIntegers): PLLine;
begin
  Result:=nil; lp_z:=nil;

  if Assigned(LHeights) then
    Result:=Alloc_ZPolyBuf(lp_z)
  else
    Result:=Alloc_LPolyBuf;

  if Assigned(Result) then

  Copy_LPoly(LPolyBuf,Result,
             LHeights,lp_z,LPolyMax)
end;

function tx_Edit.Load_LPolybuf(lp: PLLine; lp_z: PIntegers): Integer;
begin
  LPolyBuf.N:=-1; if Assigned(lp) then
  Copy_LPoly(lp,LPolyBuf,lp_z,LHeights,LPolyMax);
  Result:=LPolyBuf.N
end;

function tx_Edit.LPolybuf_to_DPolybuf: Integer;
begin
  Result:=Copy_LPoly(LPolyBuf,DPolyBuf,
                     LHeights,DHeights,
                     LPolyMax)
end;

function tx_Edit.LPolybuf_to_DPolybuf1: Integer;
begin
  Result:=Copy_LPoly(LPolyBuf,DPolyBuf1,
                     LHeights,DHeights1,
                     LPolyMax)
end;

function tx_Edit.DPolybuf_to_DPolybuf1: Integer;
begin
  Result:=Copy_LPoly(DPolyBuf,DPolyBuf1,
                     DHeights,DHeights1,
                     LPolyMax)
end;

function tx_Edit.DPolybuf_to_LPolybuf: Integer;
begin
  Result:=Copy_LPoly(DPolyBuf,LPolyBuf,
                     DHeights,LHeights,
                     LPolyMax)
end;

function tx_Edit.LPolybuf_Cut(Ind,Count: int): int;
begin
  Result:=LPoly_Cut(LPolyBuf,LHeights,Ind,Count)
end;

function tx_Edit.DPolybuf_Cut(Ind,Count: int): int;
begin
  Result:=LPoly_Cut(DPolyBuf,DHeights,Ind,Count)
end;

procedure tx_Edit.LPolybuf_Clear_all;
begin
  LPolybuf_Clear; lp_ind:=-1;
  lp_Curve:=false; lp_ellipse:=false;
  lp_Draw:=false; lp_ort:=false;
end;

procedure tx_Edit.LPolybuf_Clear;
begin
  LPolybuf.N:=-1; LPolybuf_cn:=0;
  LPolybuf_xyz:=0
end;

function tx_Edit.LPolybuf_Insert(ind: Integer;
                                 const v: vpoint): Integer;
begin
  Result:=LPoly_Insert(LPolyBuf,LHeights,
                       Ind,LPolyMax,v)
end;

function tx_Edit.LPolybuf_Append(lp: PLLine; lp_z: PIntegers;
                                 dist: Double): bool;
begin
  if lp = nil then begin
    lp:=DPolybuf; lp_z:=DHeights
  end;

  Result:=LPoly_Append(LPolybuf,lp,
                       LHeights,lp_z,
                       LPolyMax,dist)
end;

function tx_Edit.LPolybuf_Swap(lp: PLLine; curve: Boolean): Boolean;
var
  p1,p2,q1,q2: TPoint;
begin
  Result:=false;

  if lp.N >= 1+ord(curve)*2 then
  if LPolybuf.N >= 1+ord(curve)*2 then begin

    p1:=LPolybuf.Pol[0];
    q1:=lp.Pol[0];

    if curve then begin
      with LPolybuf^ do p2:=Pol[N-1];
      with lp^ do q2:=Pol[N-1];
    end
    else begin
      with LPolybuf^ do p2:=Pol[N];
      with lp^ do q2:=Pol[N];
    end;

    if Long_dist(q2,p1) < Long_dist(q1,p1) then begin
      Swap_LLine(LPolybuf,LHeights,curve);
      Result:=true
    end
  end
end;

function tx_Edit.lock_LPolybuf: bool;
begin
  Result:=LPoints_lock;

  if not Result then
  if not lp_ellipse then

  if lp_curve then begin

    if Lock_Curve(LPolybuf,LPolyMax) then
    if Assigned(LHeights) then
    with LPolybuf^ do begin
      LHeights[N-1]:=LHeights[0];
      LHeights[N]:=LHeights[0];
    end

  end else

  with LPolybuf^ do

  if N >= 2 then
  if N < LPolyMax then begin
    Inc(N); Pol[N]:=Pol[0];
    if Assigned(LHeights) then
    LHeights[N]:=LHeights[0];
  end;

  Result:=LPoints_lock
end;

function tx_Edit.Is_curve_LPolybuf: bool;
begin
  Result:=false;
  if LPolybuf.N > 0 then
  Result:=lp_curve or lp_ellipse
end;

function tx_Edit.poly_LPolybuf(eps: double): bool;
var
  c: TCurveToPoly;
begin
  Result:=false;

  if Is_curve_LPolybuf
  or (lp_mode = 11) then begin

    DPolybuf.N:=-1;
    c:=TCurveToPoly.Create(DPolybuf,-1,LPolyMax,eps*2,eps);
    try
      with LPolybuf^ do

      if lp_curve then
        c.Draw_lcurve(@Pol,N)
      else
      if lp_ellipse then
        c.Draw_ellipse(@Pol,N)
      else
      if lp_mode = 11 then
        c.Draw_bspline(@Pol,N,0,N,-1);

      if DPolybuf.N > 0 then begin
        DPolybuf_to_LPolybuf;
        lp_curve:=false;
        lp_ellipse:=false;
        Result:=true
      end
    finally
      c.Free
    end
  end;

  Result:=not Is_curve_LPolybuf
end;

function tx_Edit.curve_LPolybuf: bool;
begin
  if not lp_curve then
  if not lp_ellipse then
  if lp_mode = 11 then
  if BsplineToLCurve(LPolybuf,LPolyBuf,LPolyMax) > 0 then
  begin lp_curve:=true; Result:=true end;
  Result:=lp_curve
end;

function tx_Edit.Is_LPolybuf_xyz: Boolean;
begin
  Result:=false;

  if Xyz_Points then
  if State = st_Input then
  Result:=LPolyBuf_cn = 0
end;

function tx_Edit.lp_loc_ins: Integer;
begin
  Result:=lp_loc;
  if lp_ins then
  Result:=-Result
end;

function tx_Edit.lp_loc_Edge: Boolean;
begin
  Result:=false;
  if not this_lock then
  if not lp_ins then
  if (lp_loc = 0)
  or (lp_loc+ibool[this_curve] = lp_cnt) then
  Result:=true
end;

function dmw_rus_msg(Name,Def: PChar): PChar;
begin
  if rus_interface then
    Result:=StrCopy(Name,'rus.msg')
  else begin
    Result:=StrCopy(Name,Def);
    xStrUpdateExt(Name,'.MSG')
  end
end;

function dmw_Capt(capt: PChar): PChar;
begin
  Result:=nil; if dmw_Strings <> nil then
  Result:=dmw_Strings.iMsg(500,capt);
  if Result = nil then
  Result:=StrCopy(capt,'(dm) ')
end;

function dmw_Prompt(Msg: PChar; id: Integer): PChar;
begin
  Result:=nil; if id > 0 then
  if dmw_Strings <> nil then
  Result:=dmw_Strings.iMsg(id,Msg)
end;

function dmw_Message(Msg,Def: PChar; Id: Integer): PChar;
begin
  Result:=nil;

  if rus_interface or (Id = 0) then
    Result:=StrCopy(Msg,Def)
  else begin
    if dmw_Strings <> nil then
    Result:=dmw_Strings.iMsg(Id,Msg);
    if Result = nil then
    Result:=StrCopy(Msg,Def)
  end
end;

function dmw_Message1(Def: PChar; Id: Integer): String;
var
  s: TShortstr;
begin
  dmw_Message(s,Def,Id);
  Result:=Strpas(s)
end;

procedure dmw_About(Msg: PChar; Id: Integer);
var
  c,s: TShortStr;
begin
  xMessage( dmw_Capt(c),dmw_Message(s,Msg,Id) );
end;

function dmw_Answer(Msg: PChar; Id: Integer): integer;
var
  c,s: TShortStr;
begin
  Result:=xAnswer(dmw_Capt(c),dmw_Message(s,Msg,Id))
end;

function dmw_Warning(Msg: PChar; Id: Integer): Boolean;
var
  capt,quest: tShortStr;
begin
  dmw_Capt(Capt);
  dmw_Message(quest,Msg,Id);
  Result:=xWarning(Capt,quest)
end;

function dmx_Warning(Msg,List: PChar; Id: Integer): Boolean;
var
  capt,quest: tShortStr;
begin
  dmw_Capt(Capt); if Assigned(List) then
  StrCat(StrCat(Capt,' - '),List);

  dmw_Message(quest,Msg,Id);
  Result:=xWarning(Capt,quest)
end;

function dmw_Warning_Delete_Object: Boolean;
begin
  Result:=dmw_Warning(' ?',503)
end;

procedure x_Vars_Init;
var
  txt: TTextFile; fn: tShortStr;
begin
  dmw_Strings:=nil;

  if rus_interface then
    StrPath(fn,BinDir,'rus.msg')
  else
    StrPath(fn,BinDir,'dmw.msg');

  txt:=tTextFile.Create;
  try
    if txt.Open(fn) then begin

      Is_Comma_Delimiter:=false;
      dmw_Strings:=tdm_Strings.Create;

      while not txt.end_of_file do
      if txt.xStrRead <> nil then
      if not dmw_Strings.iAdd(txt.str) then
      Break;

      Is_Comma_Delimiter:=true
    end;
  finally
    txt.Free;
  end
end;

procedure x_Vars_Done;
begin
  dmw_Strings.Free
end;

constructor tdmQuery.Create(dm: tdm_Map;
                            AList: tdmw_List;
                            AEdit: tx_Edit);
begin
  inherited Create(dm);
  fDispList:=AList;
  x_Edit:=AEdit
end;

procedure tdmQuery.log(const Msg: String);
begin
  if log_query then
  std_log.WriteStr(msg);
end;

procedure tdmQuery.SetOccupe(p,p1: Int64);
var
  dRec: dm_Rec;
begin
  fOccupe_p:=p;
  fOccupe_p1:=p1;

  fOccupe_dm:=dm_.dm_Index;
  fOccupe_Code:=0;
  fOccupe_Loc:=0;

  if p > 0 then begin
    dm_.xGet_Object(p,dRec);
    fOccupe_Code:=dRec.Code;
    fOccupe_Loc:=dRec.Tag
  end
end;

procedure tdmQuery.SetLocator(const lt,rb,w_lt,w_rb: TPoint; lev: Integer);
begin
  this_lt:=lt; this_rb:=rb;
  Middle_Point(lt,rb,this_loc);
  this_d:=(rb.x-lt.x) div 2;
  win_lt:=w_lt; win_rb:=w_rb;
  this_Level:=lev; this_xyz:=false;
  fHeights:=x_Edit.DHeights;
end;

procedure tdmQuery.xxxLocator(const lt,rb: TPoint; dm_to_bmp: TXYZ_to_XY);
var
  L: LOrient; i: Integer;
  w_lt,w_rb: TPoint;
begin
  Bound_to_LPoly(win_lt,win_rb,@L);

  for i:=0 to 3 do
  with L[i] do dm_to_bmp(x,y,0, L[i]);
  Max_Poly_Bound(@L,4, w_lt,w_rb);

  SetLocator(lt,rb, w_lt,w_rb, this_level);
  this_xyz:=Assigned(fHeights);

  fdm_to_bmp:=dm_to_bmp;
end;

procedure tdmQuery.MoveLocator(const lt,rb: TPoint);
begin
  this_lt:=lt; this_rb:=rb;
  Middle_Point(lt,rb,this_loc);
  this_d:=(rb.x-lt.x) div 2
end;

function tdmQuery.First(Top,Only: int;
                        AClickList: TDmwClickList): Int64;
var
  that_p: Int64; p: TInt64; frst: Boolean;
begin
  Result:=0;

  if log_query then
  log(Format('Query.First top=%d only=%d',[Top,Only]));

  fClickList:=AClickList;
  if Assigned(fClickList) then begin
    fClickList.Pos:=this_loc;
    fClickList.Clear;
  end;

  frst:=true; if top > 0 then
  if dm_.Is_dm then with dm_.Tree do begin
    frst:=false; if RunP <> top then
    begin RunP:=top; TopP:=_TopP(top) end
  end;

  fIsDown:=true; child:=0;
  FThis_Only:=Only;
  FIsTick:=fx_Disp.Tick;

  Start:=top; if top <= 0 then begin
    Start:=dm_.Tree.Root;
    if top = 0 then top:=Start
  end;

  if top < 0 then begin
    Log('Scan_tree [top < 0]');
    Result:=Scan_tree(frst,false)
  end else
  if fDispList = nil then begin
    Log('Scan_tree [DispList = nil]');
    Result:=Scan_tree(frst,false)
  end else
  if (top = dm_.Tree.Root) and
     (fDispList.x_Count > 0) then begin
    Log(Format('Scan_List=%d',[fDispList.x_Count]));
    Result:=Scan_List(0,0);
    fDispList.Scan:=true
  end
  else begin
    Log('Scan_tree');

    fDispList.Scan:=false;

    if fDispList.x_Count = 0 then
    if top = dm_.Tree.Root then begin
      dm_.Get_Node(top,dmNode);
      if This_Object(top) then
      Result:=top;
    end;

    if Result = 0 then
    Result:=Scan_tree(frst,false)
  end;

  if Assigned(fClickList) then begin

    Log(Format('ClickCount=%d',[fClickList.Count]));
    
    that_p:=fOccupe_p; Result:=0;

    if fClickList.Count > 0 then begin
      p.x:=AClickList.Items[0];
      fClickList:=nil;

      if Ext_This_Object(p.cn,p.id) then
      Result:=p.x
    end;

    Log(Format('Result=%d',[Result]));

    if Result = 0 then
    Result:=First(Top,Only,nil);

    if Result > 0 then
    if Result = that_p then

    if AClickList.Count > 1 then
    if AClickList[0] = Result then

    if Only = only_join then
    Result:=AClickList[1]
  end;

  fClickList:=nil; fOccupe_p:=0;
end;

function tdmQuery.Second(Top: longint; Only: Integer): Int64;
begin
  fOccupe_Code:=0;
  Result:=First(Top,Only,nil);
end;

function tdmQuery.Next(P: Int64; Only: Integer;
                       AClickList: TDmwClickList): Int64;

function xNext(p: Int64): Integer;
begin
  Result:=0;
  with dm_ do if Is_dm then begin

    if TInt64(p).cn <> 0 then
    p:=Tree.Root;

    if Tree._Goto(p) > 0 then
    Result:=Scan_tree(false,true)
  end
end;

var
  i,ind,up: Integer; x: TInt64;
begin
  Result:=0; fIsDown:=true; child:=0;
  FThis_Only:=Only; FIsTick:=fx_Disp.Tick;

  if P > 0 then
  if dm_.Enabled_Map then begin

    Start:=dm_.Tree.Root;
    if TInt64(P).cn = 0 then Start:=P;

    if fDispList = nil then
      Result:=xNext(P)
    else
    if (fDispList.x_Count > 0)
    and fDispList.Scan then begin

      ind:=-1;

      if Assigned(AClickList) then begin
        ind:=AClickList.IndexOf(P); if ind >= 0 then
        for i:=ind+1 to AClickList.Count-1 do begin
          x.x:=AClickList[i];
          if ext_This_Object(x.cn,x.id) then
          begin Result:=x.x; Break end
        end
      end;

      if ind < 0 then begin

        if TInt64(P).cn = 0 then begin

          up:=dm_.Tree._Parent(P);
          if up > dm_.Tree.Root then begin
            Result:=Scan_List(P,up);

            ind:=fDispList.x_IndexOf(0,up);

            if ind >= 0 then
            if ind < fDispList.x_IndexOf(0,P) then P:=up;

            if Result = 0 then
            if dm_.Get_Node(up,dmNode) in [1,3,5,7] then

            if This_Object(up) then begin
              if P = up then Result:=up
            end
            else begin
              fInSide:=true;
              if This_Object(up) then
              Result:=up; fInSide:=false
            end
          end

        end;

        if Result = 0 then
        Result:=Scan_List(P,0);
      end
    end else

    Result:=xNext(P)
  end
end;

function tdmQuery.That(P: Int64; Only: longint): Boolean;
begin
  Result:=false;
  if dm_.Enabled_Map then begin

    Start:=dm_.Tree.Root;
    if TInt64(P).cn = 0 then Start:=P;

    fThis_Only:=Only; fIsTick:=false;
    fIsDown:=true; fIsThat:=true; child:=0;

    with TInt64(P) do
    Result:=ext_This_Object(cn,id);

    fIsThat:=false
  end
end;

function tdmQuery.Scan_List(prd,nxt: Int64): Int64;

function scan_vc(list: tvc_List; prd,nxt: Int64): Int64;
var
  i: int; p: TInt64; ip: pvc_disp_arr;
begin
  Result:=0;

  i:=list.Count-1;

  p.x:=prd;
  if p.cn <> cn_node then
    prd:=-1
  else
  if p.id > 0 then
    i:=list.id_Indexof(p.id)-1;

  p.cn:=cn_node; ip:=list.First;

  while i >= 0 do begin
    p.id:=ip[i].Id; Dec(i);

    if (p.x = nxt) or (p.x = prd) then begin
      Result:=-1; Break
    end;

    if ext_This_Object(p.cn,p.id) then
    begin Result:=p.x; Break end
  end
end;

function scan_ve(list: tve_List; prd,nxt: Int64): Int64;
var
  i: int; p: TInt64; ip: PIntegers;
begin
  Result:=0;

  i:=list.Count-1; p.x:=prd;

  if p.cn <> cn_edge then
    prd:=-1
  else
  if p.id > 0 then
    i:=list.Indexof(p.id)-1;

  p.cn:=cn_edge; ip:=list.First;

  while i >= 0 do begin
    p.id:=ip[i]; Dec(i);

    if (p.x = nxt) or (p.x = prd) then begin
      Result:=-1; Break
    end;

    if ext_This_Object(p.cn,p.id) then
    begin Result:=p.x; Break end
  end
end;

function scan_fe(list: tfe_List; prd,nxt: Int64): Int64;
var
  i: int; p: TInt64; ip: PIntegers; node: dm_Node;
begin
  Result:=0;

  i:=list.Count-1; p.x:=prd;

  if p.cn <> 0 then
    prd:=-1
  else
  if p.id > 0 then
    i:=list.id_Indexof(p.id)-1;

  p.cn:=0; ip:=list.First;

  while i >= 0 do begin p.id:=ip[i];

    if (p.x = nxt) or (p.x = prd) then begin
      Result:=-1; Break
    end;

    Level:=255;
    FillChar(node,Sizeof(node),0);

    if list.Get_data(i,p.id,node.dRec) then
      Level:=node.dRec.View
    else
      dm_.Get_Node(p.id,node);

    dmNode:=node;
    if This_Object(p.id) then
    begin Result:=p.x; Break end;

    Dec(i)
  end
end;

var
  cn: int;
begin
  Result:=0;

  is_Break:=false;
  this_Level:=255;

  if dm_.Enabled_Map then begin

    cn:=TInt64(prd).cn;
    if cn = 0 then begin
      Result:=scan_fe(fDispList.x_fe,prd,nxt);
      cn:=cn_edge
    end;

    if Result = 0 then
    if cn = cn_edge then begin
      Result:=scan_ve(fDispList.x_ve,prd,nxt);
      cn:=cn_node
    end;

    if Result = 0 then
    if cn = cn_node then
    Result:=scan_vc(fDispList.x_vc,prd,nxt);

    if Result < 0 then Result:=0
  end
end;

function tdmQuery.click_Object(cn,id,lev,mf: int; pt: PPoint): Integer;
var
  sort: TCustomList;
  i,ind,code,loc,dist,root: int; p,q: TInt64;
  sp: PClickDataArray; s: TClickData;
  dRec: dm_Rec;
begin
  Result:=-1;
  if Assigned(fClickList) then begin

    p.cn:=cn; p.id:=id;
    loc:=dm_.xGet_Object(p.x,dRec);
    loc:=Transit_loc(loc);
    code:=dRec.Code;

    dist:=-1; if Assigned(pt) then
    dist:=Round(Long_Dist(pt^,this_loc));

    ind:=-1; root:=dm_.Tree.Root;

    sort:=fClickList.Data;

    if sort.Count > 0 then begin
      sp:=sort.First;
      for i:=0 to sort.Count-1 do begin

        if Assigned(fOnCompare) then begin
          q.x:=fClickList[i];
          if fOnCompare(q.x,p.x) then begin
            ind:=i; Break
          end
        end;

        s:=sp[i];

        if p.x = root then begin

          if fOccupe_loc > 0 then
          if s.loc <> fOccupe_loc then
          if fThis_Only = only_point then begin
            ind:=i; Break
          end

        end
        else begin
          if fThis_Only = only_joinp then
          if (loc = 2) and (s.loc <> 2) then begin
            ind:=i; Break
          end;

          if loc = s.loc then begin

            if (fThis_Only in [only_child,only_joino])
            and (code = fOccupe_Code) then begin
              ind:=0; Break
            end else
            if dist < s.dist then begin
              ind:=i; Break
            end else

            if (fThis_Only in [only_clip]) and
               (mf < s.mf) then begin
              ind:=i; Break
            end
          end else
          if loc <> 3 then
          if loc < s.loc then begin
            ind:=i; Break
          end
        end
      end
    end;

    Result:=fClickList.xAdd(ind,p.x,loc,dist,mf,lev)
  end
end;

function tdmQuery.ext_This_Object(cn,id: uint): Boolean;

function This_Node(Id: Cardinal): Boolean;
var
  vc: tcn_rec; p: TPoint;
begin
  Result:=false;
  if dm_.Tree.get_vc(Id,@vc) then begin

    p:=vc.Pos; if this_xyz then
    fdm_to_bmp(p.x,p.y,vc.zpos, p);

    if This_LPoint(p) then begin
      x_Edit.Set_dm_Loc(vc.VPos,0);

      if Only_Node(Id) then
      Result:=click_Object(cn_node,Id,0,0,@p) < 0
    end
  end
end;

function This_Rib(Id: Cardinal): Boolean;
var
  p: TInt64; Ind: Integer;
begin
  Result:=false; p.id:=Id; p.cn:=cn_edge;

  if dm_ThisPoly(p.x,fx_Disp.Points <> 2) >= 0 then

  with x_Edit do
  if Only_Edge(Id) then begin
    Ind:=click_Object(cn_edge,Id,0,DPolybuf.N,nil);

    if Ind < 0 then
      Result:=true
    else
    if LPolyBuf_cn = 2 then
    if lp_Contains_Index(LPolybuf,id) >= 0 then
    fClickList.xPop(Ind)
  end
end;

begin
  Result:=false;

  if cn = cn_object then begin

    if id > 0 then begin
      Level:=255;
      dm_.Get_Node(id,dmNode);
      Result:=This_Object(id)
    end

  end else
  if cn = cn_node then Result:=This_Node(id) else
  if cn = cn_edge then Result:=This_Rib(id)
end;

function tdmQuery.This_Object(p: longint): Boolean;

function ThisSign(p,loc: Integer): Integer;
var
  i: Integer; lp: PLLine; v: TPoint;
begin
  Result:=-1;

  lp:=dm_GetPoly(p);
  if Assigned(lp) then with x_Edit do begin
    lp_cnt:=lp.N; lp_ins:=false; this_lock:=false;
    this_curve:=(loc = 1) and (lp.N = 3);

    for i:=0 to lp.N do begin
      v:=lp.Pol[i];
      if This_LPoint(v) then begin
        Result:=Occupe_lp_loc(p,i);
        Result:=i; that_loc:=v; Break
      end
    end
  end
end;                   

function ThisPoly(p, x1,y1,x2,y2: longint): Boolean;
var
  lt,rb: TPoint; n,min,max: Integer;
  L: array[0..7] of TPoint;
begin
  if this_xyz then begin

    n:=dm_.xGet_Poly(p, nil,fHeights,LPolyMax);
    min:=0; max:=0;

    if n = 0 then begin
      min:=fHeights[0]; max:=min
    end else

    if n > 0 then
    int_MinMax(fHeights,n+1, @min,@max);

    fdm_to_bmp(x1,y1,min, L[0]);
    fdm_to_bmp(x2,y1,min, L[1]);
    fdm_to_bmp(x2,y2,min, L[2]);
    fdm_to_bmp(x1,y2,min, L[3]);

    n:=4; if max > min then begin
      fdm_to_bmp(x1,y1,max, L[4]);
      fdm_to_bmp(x2,y1,max, L[5]);
      fdm_to_bmp(x2,y2,max, L[6]);
      fdm_to_bmp(x1,y2,max, L[7]);
      n:=8
    end;

    Max_Poly_Bound(@L,n, lt,rb);
    x1:=lt.X; y1:=lt.Y; x2:=rb.X; y2:=rb.Y
  end;

  with this_loc do Result:=
  (x >= x1-this_d) and (x <= x2+this_d) and
  (y >= y1-this_d) and (y <= y2+this_d)
end;

function ThisEllipse(p: longint): Boolean;
var
  i: int; lp: PLLine; b: LOrient;
begin
  Result:=false;

  lp:=dm_GetPoly(p);
  if Assigned(lp) then
  with x_Edit do begin
    lp_ins:=false; lp_cnt:=lp.N;

    i:=LPoly_Locate(lp,this_lt,this_rb);
    if i >= 0 then begin
      Occupe_lp_loc(p,i); Result:=true
    end else
    if Ellipse_Bound(lp,b) then
    if PolyGonContainsPoint(@b,4,this_loc) > 0 then
    Result:=Occupe_this_loc

  end
end;

function ThisCurve(p: longint;
                   scan: Boolean): Boolean;
var
  curve: TLocateCurve;
  lp: PLLine; lp_i,i,j: int;
  lp_p,p1,p2,p3: TPoint;
  t: Float; v: lxyz;
begin
  Result:=false;

  lp:=dm_GetPoly(p);
  if Assigned(lp) then
  with x_Edit do begin
    lp_cnt:=lp.N; lp_ins:=false;
    this_lock:=CurveLock(lp);
    this_curve:=true;

    curve:=TLocateCurve.Create(this_lt,this_rb);
    try
      t:=-1;
      if fIsThat then begin
        i:=lp_loc; if Odd(i) then Dec(i);

        for j:=i-2 to i+2 do
        if (j >= 0) and (j < lp.N) then begin

          p1:=lp.Pol[j]; p2:=lp.Pol[j+1];
          Mirror_Point(p2,p1,p3);

          if (j <= i) and This_LPoint(p2) then begin
            lp_i:=j+1; lp_p:=p2; t:=0;
            x_Edit.lp_dir:=0; Break
          end else

          if (j >= i) and This_LPoint(p3) then begin
            lp_i:=j+1; lp_p:=p3; t:=0;
            x_Edit.lp_dir:=1; Break
          end
        end
      end;

      if t < 0 then
      t:=curve.This_curve(lp,scan,lp_i,lp_p);

      if t >= 0 then begin

        if this_xyz then begin
          dm_.xGet_mf(p,lp,nil);

          if Odd(lp_i) then

          with lp^ do begin
            lp_p:=Pol[lp_i]; if x_Edit.lp_dir = 1 then
            Mirror_Point(Pol[lp_i],Pol[lp_i-1],lp_p)
          end
          else begin
            curve.lBezier(@lp.Pol[lp_i]);
            lp_p:=curve.lPlot(t);
          end
        end;

        v.p:=lp_p; v.v.z:=0;

        with x_Edit do begin
          Set_dm_Loc(v.v,lp_i);
          curve_t:=t; lp_ins:=t > Small;
        end;

        Result:=true
      end;
    finally
      curve.Free
    end
  end
end;

function ThisSector(p: longint): Boolean;
var
  lp: PLLine; i: Integer;
begin
  Result:=false;

  lp:=dm_GetPoly(p);
  if Assigned(lp) then
  with x_Edit do begin
    lp_ins:=false; lp_cnt:=lp.N;

    i:=LPoly_Locate(lp,this_lt,this_rb);
    if i = 0 then begin
      Occupe_lp_loc(p,i); Result:=true
    end
    else begin
      lp.N:=LPoly_Sectorp(@lp.Pol,lp.N);

      if lp.N > 0 then
      if xPolyGonContainsPoint(lp,this_loc) > 0 then
      Result:=Occupe_this_loc
    end
  end
end;

function This_fe(p,loc: Integer): Boolean;
var
  i,ax,bx,ornt: Integer; fe: PLPoly; x: TInt64;
begin
  Result:=false; with x_Edit do

  if dm_.xGet_Poly(p,XPolybuf,nil,LPolyMax) >= 0 then
  begin

    if fx_Disp.Points > 0 then
    loc:=22; bx:=0;

    fe:=@XPolybuf.Pol;
    for i:=0 to XPolybuf.N do begin
      x.id:=fe[0].x; x.cn:=cn_edge;
      ornt:=tlong(fe[0].y).b[1]; ax:=-1;

      if dm_ThisPoly(x.x,fx_Disp.Points <> 2) >= 0 then
        ax:=0
      else
      if loc = 23 then begin

        if ornt = 2 then
        Swap_Poly(DPolybuf);

        with DPolybuf^,this_loc do
        ax:=PolyGonContainsPixel(@Pol,N, x,y);
        if ax = 1 then bx:=(bx + 1) and 1;
      end;

      if ax = 0 then begin
        bx:=1; ax:=i; Break
      end;

      fe:=@fe[1];
    end;

    if bx = 1 then begin
      vc_loc:=-1; if lp_loc = 0 then vc_loc:=ax else
      if lp_loc = DPolybuf.N then vc_loc:=ax+1;

      this_lock:=false; this_curve:=false;
      lp_loc:=ax; lp_cnt:=XPolybuf.N;
      FThis_Id:=0; Result:=true
    end
  end;
end;

function This_hg(p: longint): Boolean;
var
  lp: PLPoly; n: int;
begin
  Result:=false;

  if Assigned(fHints) then begin
    n:=fHints.GetPoly(p,lp);

    if n > 0 then with this_loc do
    if rPolygonContainsPixel(lp,n-1,X,Y) >= 0 then begin
      Load_Poly(x_Edit.DPolybuf,lp,n);
      Occupe_lp_loc(p,0); Result:=true
    end
  end
end;

var
  up,dx,dy,loc,mf: int;
  tmp: dm_Rec; pp: PPoint;
  vv: VPoint; o: dm_Node;
begin
  Result:=false; fIsDown:=false;

  o:=dmNode; with o.dRec do
  if View <= this_Level then begin

    fIsDown:=true; dx:=ox2-ox1; dy:=oy2-oy1;

    vv:=x_Edit.dm_Loc; loc:=Tag mod 100; mf:=0;

    case Tag of

  0:  if (o.Flags and (fl_draw+fl_long)) <> 0 then
      fIsDown:=false;

  1:  Result:=ThisSign(p,1) >= 0;

  11,
  21: if ThisPoly(p, ox1,oy1,ox2,oy2) then
      Result:=ThisSign(p,11) >= 0;

  22,
  23: if ThisPoly(p, ox1,oy1,ox2,oy2) then
      Result:=This_fe(p,Tag);

  2,
  3,
  4,

  102,
  103,
  104:if ThisPoly(p, ox1,oy1,ox2,oy2) then
      with x_Edit do begin
        Result:=dm_ThisPoly(p,fx_Disp.Points <> 2) >= 0;
        mf:=DPolybuf.N;

        if fx_Disp.Points = 0 then
        if not (This_Only in [only_import,only_joinp]) then
        if not Result and (loc = 3) and (lp_cnt >= 0) then
        if this_lock and (p <> dm_.Tree.Root) then

        if xPolyGonContainsPoint(DPolyBuf,this_loc) > 0 then

        if (LPoly_Locate(DPolyBuf,win_lt,win_rb) >= 0)
        or fx_Disp.AreaFill or fInSide then
        Result:=Occupe_this_loc
      end;

  5,
  105:if ThisPoly(p, ox1,oy1,ox2,oy2) then
      Result:=ThisEllipse(p);

  6,
  7:  if ThisPoly(p, ox1-dx,oy1-dy,ox2+dx,oy2+dy) then
      Result:=ThisCurve(p,fx_Disp.Points <> 2);

  108:if ThisPoly(p, ox1,oy1,ox2,oy2) then
      Result:=ThisSector(p);

  101:
      Result:=this_hg(p);

    end;

    if Result then begin
      Result:=Only_Object(p);
      if not Result then x_Edit.dm_Loc:=vv
    end;

    if Result then begin pp:=nil;
      if Tag in [1,2] then pp:=@that_loc;
      if click_Object(0,p,Level,mf,pp) >= 0 then
      Result:=false
    end
  end;

  Inc(fBreakCount);
  if fBreakCount mod 10 = 0 then
  is_Break:=xEscape(0,0)
end;

function tdmQuery.Only_Object(p: Integer): Boolean;
begin
  Result:=p <> dm_.Tree.Root
end;

function tdmQuery.Goto_Down(p: Integer): Boolean;
begin
  Result:=fIsDown
end;

function tdmQuery.Only_Node(id: Cardinal): Boolean;
begin
  Result:=false
end;

function tdmQuery.Only_Edge(id: Cardinal): Boolean;
begin
  Result:=false
end;

function tdmQuery.Pickup_vc(out V: VPoint): uint;
var
  i,vn: int; vp: pvc_disp_arr;
  vc: tvc_disp; p: TPoint;
begin
  Result:=0; V.x:=0; V.y:=0; V.z:=0;

  if Assigned(DispList) then begin

    vp:=DispList.x_vc.First;
    vn:=DispList.x_vc.Count;

    for i:=0 to vn-1 do begin
      vc:=vp[i];

      p:=vc.Pos.p; if this_xyz then
      fdm_to_bmp(p.x,p.y,vc.Pos.v.z, p);

      if This_LPoint(p) then begin
        Result:=vc.Id; V:=vc.Pos.v; Break
      end
    end
  end
end;

function tdmQuery.Pickup_grid(out V: VPoint): Boolean;
var
  x_pt: TPointList; I: Integer;
  pt: PPoint; p: TPoint;
begin
  Result:=false;

  V.x:=0; V.y:=0; V.z:=0;

  if Assigned(DispList) then begin
    x_pt:=dispList.x_grid;

    for I:=0 to x_pt.Count-1 do begin
      pt:=x_pt.Items[I];

      p:=pt^; if this_xyz then
      fdm_to_bmp(p.x,p.y,0, p);

      if This_LPoint(p) then begin
        V.x:=pt.x; V.y:=pt.y;
        Result:=true; Break
      end
    end
  end
end;

function tdmQuery.This_LPoint(const v: TPoint): Boolean;
begin
  Result:=PortContainsPoint(this_lt,this_rb, v.x,v.y)
end;

function tdmQuery.dm_GetPoly(p: Int64): PLLine;
var
  i: Integer;
begin
  Result:=nil; with x_Edit do
  if dm_.xGet_mf(p,DPolyBuf,fHeights) >= 0 then begin
    if this_xyz then with DPolyBuf^ do
    for i:=0 to N do with Pol[i] do
    fdm_to_bmp(x,y,fHeights[i], Pol[i]);

    Result:=DPolyBuf
  end
end;

function tdmQuery.dm_This_LPoint(p: Int64; lp_i: Integer): Boolean;
var
  lp: PLLine;
begin
  Result:=false;

  if this_xyz then
    lp:=dm_GetPoly(p)
  else
    lp:=x_Edit.DPolyBuf;

  if lp <> nil then
  if lp_i >= 0 then
  if lp_i <= lp.N then

  Result:=This_LPoint(lp.Pol[lp_i])
end;

function tdmQuery.dm_GetPoint(p: Int64; lp_i: Integer): VPoint;
var
  lp: PLLine; t: TPoint; z: Integer;
begin
  lp:=x_Edit.DPolyBuf;
  t:=lp.Pol[lp_i]; z:=0; if this_xyz then
  if dm_.xGet_mf(p,lp,nil) >= 0 then begin
    t:=lp.Pol[lp_i]; z:=fHeights[lp_i]
  end;

  Result.x:=t.x; Result.y:=t.y; Result.z:=z
end;

function tdmQuery.Occupe_lp_loc(p: Int64; lp_i: Integer): Integer;
begin
  with x_Edit do
  Result:=Set_dm_Loc(dm_GetPoint(p,lp_i),lp_i)
end;

function tdmQuery.Occupe_this_loc: Boolean;
var
  v: vpoint;
begin
  v.x:=this_loc.x; v.y:=this_loc.y; v.z:=0;
  x_Edit.Set_dm_Loc(v,-1); Result:=true
end;

function tdmQuery.Get_that_dist: Integer;
begin
  Result:=Round(Long_Dist(that_loc,this_loc))
end;

function tdmQuery.This_poly(lp: PLLine; scan: Boolean): Boolean;
var
  i: Integer; v1,v2,vc,lt,rb: TPoint;
begin
  Result:=false;

  if lp.N >= 0 then begin

    if this_xyz then with lp^ do
    for i:=0 to N do with Pol[i] do
    fdm_to_bmp(X,Y,0,Pol[i]);

    v2:=lp.Pol[0];
    for i:=0 to lp.N do begin
      v1:=v2; v2:=lp.Pol[i];

      lt.x:=Min(v1.x,v2.x); rb.x:=Max(v1.x,v2.x);
      lt.y:=Min(v1.y,v2.y); rb.y:=Max(v1.y,v2.y);

      if (this_lt.x < rb.x) and (this_rb.x > lt.x) then
      if (this_lt.y < rb.y) and (this_rb.y > lt.y) then begin

        if (v2.x >= this_lt.x) and (v2.x <= this_rb.x) then
        if (v2.y >= this_lt.y) and (v2.y <= this_rb.y) then
        begin that_loc:=v2; Result:=true; Break end;

        if scan and (i > 0) then
        if Locate_Line(v1,v2, this_lt,this_rb, vc) then
        begin that_loc:=vc; Result:=true; Break end;
      end
    end
  end
end;

function tdmQuery.That_poly(lp: PLLine; loc: Integer): Boolean;
begin
  Result:=This_poly(lp,loc > 1);
  if not Result and (loc = 3) then

  with this_loc do if Polylock(lp) then
  Result:=rPolygonContainsPixel(@lp.Pol,lp.N,X,Y) >= 0
end;

function tdmQuery.dm_ThisPoly(p: Int64;
                              scan: Boolean): Integer;

function dm_GetScan(p: Int64; lp: PLLine;
                    i: Integer; const _v: TPoint): VPoint;
var
  v1,v2: TPoint; h1,h2: Integer; k: Double;
begin
  Result.x:=_v.x;
  Result.y:=_v.y;
  Result.z:=0;

  v1:=lp.Pol[i-1]; v2:=lp.Pol[i];
  k:=0; if not Points_Equal(v1,v2) then
  k:=Long_Dist(v1,_v)/Long_Dist(v1,v2);

  if this_xyz then
  if dm_.xGet_mf(p,lp,nil) >= 0 then
  begin
    v1:=lp.Pol[i-1]; v2:=lp.Pol[i];
    Result.x:=v1.x+Round((v2.x-v1.x)*k);
    Result.y:=v1.y+Round((v2.y-v1.y)*k);

    h1:=fHeights[i-1]; h2:=fHeights[i];
    Result.z:=h1+Round((h2-h1)*k)
  end
end;

var
  i: Integer; lp: PLLine; v: vpoint;
  v1,v2, lt,rb, _v: TPoint;
begin
  Result:=-1; lp:=dm_GetPoly(p);

  if lp <> nil then with x_Edit do begin

    lp_cnt:=lp.N; lp_ins:=false;
    this_lock:=PolyLock(lp);
    this_curve:=false; v2:=lp.Pol[0];

    for i:=0 to lp.N do begin
      v1:=v2; v2:=lp.Pol[i];

      lt.x:=Min(v1.x,v2.x); rb.x:=Max(v1.x,v2.x);
      lt.y:=Min(v1.y,v2.y); rb.y:=Max(v1.y,v2.y);

      if (this_lt.x < rb.x) and (this_rb.x > lt.x) then
      if (this_lt.y < rb.y) and (this_rb.y > lt.y) then begin

        if (v2.x >= this_lt.x) and (v2.x <= this_rb.x) then
        if (v2.y >= this_lt.y) and (v2.y <= this_rb.y) then
        begin
          v:=dm_GetPoint(p,i); that_loc:=v2;
          lp_ins:=false; Result:=i; Break
        end;

        if scan then
        if i > 0 then if Result < 0 then
        if Locate_Line(v1,v2, this_lt,this_rb, _v) then begin
          that_loc:=_v;
          v:=dm_GetScan(p,lp,i,_v);
          lp_ins:=true; Result:=i
        end

      end
    end
  end;

  if Result >= 0 then
  x_Edit.Set_dm_Loc(v,Result)
end;

function tdmQuery.this_Occupe_run(p: Int64): Boolean;
begin
  Result:=false;
  if dm_.dm_Index = fOccupe_dm then
  if p = fOccupe_p then Result:=true
end;

function tdmQuery.this_Occupe_run1(p: Int64): Boolean;
begin
  Result:=false;
  if dm_.dm_Index = fOccupe_dm then
  if p = fOccupe_p1 then Result:=true
end;

function tdmQuery.this_Occupe_Object(Code,Loc: Integer): Boolean;
begin
  Result:=false; if Loc = fOccupe_Loc then
  if (fOccupe_Code = 0) or (Code = fOccupe_Code)
  or fIsJoin then Result:=true
end;

procedure tdmQuery.Disp_List_Clear;
begin
  if Assigned(fDispList) then begin
    fDispList.Clear; fDispList.Scan:=false;
  end
end;

function Init_ClickArray(Click: PClickArray;
                         ini: PChar): Integer;
var
  p,q,s: PChar; r: TClickObject;
begin
  Result:=0;
  Fillchar(Click^,SizeOf(TClickArray),0);

  s:=xAllocPtr(Strlen(ini)+1);
  if Assigned(s) then begin
    p:=StrCopy(s,ini);

    while Strlen(p) > 0 do begin
      q:=StrScan(p,#10); if q = nil then
      q:=StrScan(p,';'); if q = nil then
      q:=StrEnd(p); if q = nil then Break;

      if q[0] <> #0 then begin
        q[0]:=#0; q:=@q[1]
      end;

      Fillchar(r,Sizeof(r),0);
      if CodeToken(p,r.Code) then begin
        IntToken(p,r.Loc);
        IntToken(p,r.nn[0]);
        IntToken(p,r.nn[1]);
        IntToken(p,r.nn[2]);
        IntToken(p,r.nn[3]);

        if Result < ClickMax then begin
          Click[Result]:=r; Inc(Result)
        end
      end;

      p:=q
    end
  end;

  xFreePtr(s)
end;

function This_click_object(dm: tdm_map; p: Integer;
                           Click: PClickArray;
                           Count: Integer): Boolean;
var
  i,j: Integer; r: TClickObject;
  dRec: dm_Rec; Info: TInfoList;
begin
  Result:=true;

  Info:=TInfoList.Create;
  try
    dm.Get_Object(p,dRec);
    dm.Get_Info(p,Info);

    for i:=0 to Count-1 do begin
      r:=Click[i]; with dRec do
      Result:=Transit_Object(r.Code,r.Loc,Code,Tag);

      if Result then
      for j:=0 to 3 do
      if r.nn[j] > 0 then
      if Info.Contains_nn(r.nn[j]) < 0 then
      begin Result:=false; Break end;

      if Result then Break
    end;
  finally
    Info.Free
  end
end;

initialization x_Vars_Init;
finalization x_Vars_Done;

end.