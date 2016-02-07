unit dm_draw; interface {$H-}

uses
  Windows,Classes,LCLType,
  Controls,Contnrs,Math,
  otypes,xcursors,xline,ogauss,xyz,
  xclasses,xintf,xintf1,xintf2,xproj,
  pmath,xlist,xlist1,xdib,xmask,xctx,
  dmw_link,dmw_obj,dmw_bln,dmw_dm,dmw_vars,
  dmw_prj,dmw_fnt,dmw_vgm,dmw_pgm,
  xmovie,xcards,cd_pcx,idib;

const
  xxx_mode = true;

  pen_Object  = 0;
  pen_Objects = 100;

  ext_Tools   = 100;

  ext_Point   = 101;
  ext_Vector  = 102;
  ext_Rect    = 103;
  ext_Ring    = 104;
  ext_Port    = 105;
  ext_Poly    = 106;
  ext_Gauss   = 107;
  ext_Object  = 108;
  ext_Image   = 109;
  ext_Page    = 110;
  ext_Geoid   = 111;

  ext_Rule    = 112;
  sm_Port     = 113;

  ext_Locator = 114;
  ext_Cursor  = 115;
  ext_Import  = 116;
  ext_Punkt   = 117;

  ext_Drag    = 118;
  ext_Print   = 119;

  ext_Cursors: Array[ext_Point..ext_Port] of Integer =
  (_xTarget,_xTarget,_cRegion,_cCursor,_xTarget);

type
  TCode_set = array[Char] of Boolean;

  TUtilProc = procedure(Topic: PChar; pen,lock, w,h: Integer) of object;

  TOnVector = procedure(x1,y1,x2,y2: Integer) of object;
  TOnPort = procedure(x1,y1,x2,y2: Integer; lp: PLLine) of object;

  TOnShape = procedure(cn: Integer; lp: PGPoly) of object;
  TOnDispObject = procedure(P: Int64; Cmd: Integer) of object;
  TOnObject = procedure(p: Int64) of object;
  TOnPair = procedure(p1,p2: Int64) of object;
  TOnLine = procedure(lp: PLLine) of object;
  TOnLine1 = procedure(lp: PLLine; hp: PIntegers) of object;
  TOnScale = function(cx,cy,sc: double; pps: Integer): Boolean of object;
  TOnLocate = function(cx,cy: double; pps: Integer): Boolean of object;
  TOnZoom = procedure(sc: Double) of object;

  TOnRefresh = procedure(dc: XCanvas) of object;

  TOnSetFrame = procedure(Sender: TObject; G: PGPoly) of object;

  TFrameEvent = function(G: PGPoly): Double of object;

  TOnObjects = procedure(cn,pps: int; lp: PGPoly;
                         p1,p2: Int64; flags: int) of object;

  TOnMouseMove = procedure(const p: TPoint) of object;
  TOnMoveDown = function(var p: TPoint): Boolean of object;

  TOnXY_BL = procedure(X,Y: Double;
                       out B,L: Double;
                       out pps: Integer) of object;

  TOnStatus = procedure(Id: Integer; Msg: PChar) of object;

  TOnStereoPos = procedure(h: Double; abs,up: Boolean) of object;

  TOnWhere = procedure(Sender: TObject; const V: VPoint) of object;

  TOnSwapPcx = function(Sender: TObject;
                        const g: xgeoid;
                        scale: int): Boolean of object;

  TOnRotate = procedure(var T: Real3x3) of object;

  TOnDrawObject = procedure(dc: XCanvas; p,loc: Integer) of object;

  TOnRuleDist = function(lp: PLLine; hp: PIntegers; curve: Boolean): Boolean of object;

  TDrawCard = class;

  TNavCustom = class
    constructor Create(ACard: TDrawCard);
    function MouseMove(X,Y: int): bool; virtual; abstract;
    function MouseDown(Button: TMouseButton; X,Y: int): bool; virtual; abstract;
    function MouseUp(Button: TMouseButton; X,Y: int): bool; virtual; abstract;
    procedure Draw(dc: XCanvas); virtual; abstract;
  protected
    fCard: TDrawCard;
  end;

  TDrawCard = class(TLinkCard)

    dispFlags: Integer;

    dm_Tag: Integer;
    dm_Coord: Integer;

    dm_t_init: Longbool;
    dm_t_xyz: Longbool;
    dm_t_mode: Integer;

    kz,mm_k,p_step: double;

    v_lt,v_rb, s_lt,s_rb, t_lt,t_rb: TPoint;
    map_lt,map_rb, run_p: TPoint;
    win_lt,win_rb: TPoint;

    xyz_dx: Integer;
    prn_scale: Integer;
    pgm_scale: float;

    lp_View,dm_View: int;
    cd_View,ocx_View: int;
    cd_Scale: int;
    cd_dpmm: Double;
    cd_kz: Double;

    isCash,isCross,isDraw: Longbool;
    isPaste,isCurve,isRefresh: Longbool;

    Preview: TDrawCard;

    lg_p: ab_Orient;
    dm_lg_t: ab_Orient;

    dm_t,dm_bt: Real3x3;
    lg_t,lg_bt: Real3x3;
    grid_t,gauss_t: Real3x3;

    xyz_t: TMatrix;
    xyz_p: TMatrix;

    xyz_bt: TMatrix;

    xyz_c: TGauss;
    xyz_focus: Double;

    Code_Chars: TCode_set;
    Is_Code_Chars: Longbool;

    prn_Page: record
      Rect: VLLine;
      Nx,Ny,Typ: Integer;
    end;

    Azimuth: record
      g: TGauss; Enabled: Longbool;
    end;

    draw_Wnd: hWnd;
    draw_Msg: Integer;

    sys_coord: int;

    toolButton: int;

    skip_frames: longbool;

    constructor Create(ACard: ICardBox);

    destructor Destroy; override;

    function InitGlobe: TMapProject;
    function FreeGlobe: TMapProject;

    procedure notify(Sender: TObject; Cmd: Integer); override;

    function Movie_Flags: Integer;

    function Cursor_stereo(out dx: Integer): Boolean; override;

    procedure Set_loc_dot(const p: TPoint); override;
    procedure Set_loc_xyz(const v: VPoint);

    function Get_loc_xxx: VPoint;
    procedure Set_loc_xxx(const v: VPoint);

    function GeoToMeter(const g: TGeoPoint): TGauss;
    
    function Set_prj_Bound(ANorth: Double): TGauss;

    function Get_min_scale: Longint;
    function Get_max_scale: Longint;

    function Get_prj_size: TSize;

    function Get_prj_tr(out tr,bt: Real3x3): Boolean;

    procedure clip_centre(var c: TPoint);

    function prn_Get_win(out wp,pp: TPort2d): Boolean;

    function prn_Get_frag(lp: PLLine): TPort2d;
    function prn_Get_poly(const fr: TPort2d): VLLine;

    procedure prn_Show_frag(const fr: TPort2d);

    procedure Get_win(iv: PIntegers; rv: PDoubles); override;
    procedure Set_win(iv: PIntegers; rv: PDoubles); override;

    procedure Card_road(gx,gy,fi,dr: Double; pps,dx,dy: Integer);

    procedure pcx_Card_zoom(cls: Boolean); override;
    procedure Card_Zoom(cls: Boolean); override;

    procedure Set_Code_Chars(mask: Int64);

    function clip_poly(bp: PLPoly; bn: Integer): Boolean;

    function xProject(x,y,z: Integer): TPoint;

    function xyz_Project(x,y,z: integer): TPoint; override;
    function xyz_Backup(x,y: double): VPoint; override;

    function xxx_Project(const v: VPoint): TPoint;
    function xxx_Backup(x,y: Integer): VPoint;

    function xxx_to_xyz(const v: VPoint): VPoint;

    function gProject(const g: TGauss): TPoint;

    function depth_Backup(X,Y: integer;
                          const v: VPoint): VPoint;

    function depth_Stereo(X,Y: integer;
                          const v: VPoint): VPoint;

    procedure pcx_Centre(out c: TPoint);
    procedure pcx_Port(out a,b: TPoint);

    function Get_kz_mpp: Double;

    function pcx_ed(sm: Boolean): double;                        
    function pcx_mpp: double;
    function pcx_kz: double;

    function dm_to_bmp(const p: TPoint): TPoint;
    function dm_to_img(const p: TPoint; k: Double): TGauss;
    function dm_to_dc(px,py: Double): TGauss;

    function prn_to_dm(const p: TGauss): TPoint;

    function xy_lnk_dm(const g: tgauss): TPoint;
    function dm_lnk_xy(const p: TPoint): TGauss;

    procedure Project(x,y: longint; out p: TPoint); override;
    procedure Backup(x,y: integer; out p: TPoint); override;
    procedure Backupf(x,y: double; out p: TPoint); override;

    procedure ProjectLink(const g: TGauss; out p: TPoint); override;
    procedure BackupCentre(x,y: integer; out c: TPoint); override;
    procedure Backup_plus(x,y: integer; out p: TPoint); override;

    function BackupCentreMeter(x,y: integer): TGauss;

    procedure ProjectCentre(const p: TPoint; out c: TPoint);
    function GaussToCentre(x,y: Double): TPoint;

    function Wheel_centre(x,y,plus: integer;
                          out c: TPoint): Double; override;

    function Backup_ext_Centre(out g: xgeoid): Double;
    function xBackup_ext_Centre(out g: xgeoid): Double;

    function Project_lp(lp: PLLine): integer;

    procedure pps_Backup(out g: xgeoid);
    procedure gBackup(x,y: integer; out g: tgauss);
    procedure pBackup(x,y: integer; out g: tgauss);
    function pBackup_Centre: TGauss;

    function Backup_gcentre: TGauss;
    function Get_gview1(P,G: PGPoly): Boolean;
    function Get_gview(G: PGPoly): Boolean;
    function Get_xview(G: PGPoly): Integer;

    function Backup_ed(k: double): double;

    procedure xy_to_sm(const g: TGauss; out l: TPoint);
    procedure sm_to_xy(const l: TPoint; out g: TGauss);

    procedure x_Gauss(const l: TPoint; out p: TPoint);
    procedure x_Local(const p: TPoint; out l: TPoint);

    procedure p_Gauss(const l: TPoint; out p: TGauss);
    procedure p_Local(const p: TGauss; out l: TPoint);

    procedure Align_prj_page(lp: PLLine);

    function Get_gauss_step(cx,cy: Integer): double;
    function dc_gauss_step(dc: XCanvas): double;

    function dc_Resolution(dc: XCanvas): double;
    function kz_Scale(dc: XCanvas; kz: Double): double;
    function dc_Scale(dc: XCanvas): double;

    procedure Cash_Paint(dib: TDIB; PaintR,ClipR: PRect);

    procedure Draw_Event(Sender: TObject);
    procedure Card_Refresh(Sender: TObject); override;
    procedure Card_Reground(Sender: TObject);

    procedure nav_draw(dc: XCanvas);

    procedure Owner_Draw(dc: XCanvas); override;

    procedure Cash_Draw(Sender: TDIB; PaintR,ClipR: PRect);

    function Pickup_Enabled: Boolean;
    procedure Pickup_fe(Ptr,Code,Loc: int; const lt,rb: TPoint);
    procedure Pickup_list(list: TInt64List);
    procedure Pickup_childs(p: Longint);

    procedure Owner_Disp_Object(xdc: XCanvas; Ptr: Int64; mode: int);
    procedure Owner_Mark_Object(xdc: XCanvas; Ptr: Int64);
    procedure Mark_Occupe_Point(xdc: XCanvas; Ptr: Int64);

    procedure Owner_Draw_Begin(dc: XCanvas); virtual;
    procedure Owner_Draw_End(dc: XCanvas); virtual;

    procedure BeforePaint(Sender: TObject; PaintR: PRect);
    procedure AfterPaint(Sender: TObject);

    function Setup_Map_Display(dc: XCanvas; P: PRect;
                               dm_i: Integer): Boolean;

    function Prepare_Map_draw: Boolean;
    function Setup_Edit_Map_Display: Boolean;

    procedure Page_PolyBuf(dc: XCanvas; lp: PLLine);

    procedure Draw_PolyBuf(dc: XCanvas; lp: PLLine; loc,cl: Integer);
    function Clip_PolyBuf(dc: XCanvas; lp: PLLine; loc,cl: Integer): Boolean;

    function Draw_frame(dc: XCanvas; lp: PLLine; loc,cl: Integer): Boolean;
    procedure Draw_frame1(dc: XCanvas; lp: PLLine; cl: Integer);

    procedure card_lpoly(lp: PLPoly; n,loc,cl: int);
    procedure draw_list(dc: XCanvas; mf: TPolyList; loc,cl: int);
    procedure Out_lline(dc: XCanvas; lp: PLLine; loc,cl: int);
    procedure Out_lpoly(dc: XCanvas; lp: PLPoly; n,loc,cl: int);

    procedure Out_LPolyLine(dc: XCanvas; lp: PLPoly; n,cl: int);
    procedure Out_PolyLine(dc: XCanvas; lp: PLLine; cl: int);

    procedure draw_mf(dc: XCanvas; mf: TPolyList; loc,cl: int);

    procedure mark_Points(dc: XCanvas; lp: PLPoly; n,r,cl: int);

    function print_init_mosaic: bool;

    function Print_Maps(dc: XCanvas;
                        dc_x1,dc_y1,dc_x2,dc_y2: Integer;
                        px_x1,px_y1,px_x2,px_y2,sc: Double;
                        bmp: Boolean): Double;

    procedure Print_page(xdc: XCanvas; px,py,mpp: Double);

    procedure Draw_Maps(dc: XCanvas; p: PRect); virtual;
    procedure Draw_Grid(dc: XCanvas; prj,grid: Boolean);
    procedure Draw_map(dc: XCanvas; dm_i: Integer);

    procedure begin_draw_cn(dc: XCanvas);

    function draw_vc(dc: XCanvas; Id: uint): Boolean;
    function draw_ve(dc: XCanvas; Id: uint): Boolean;

    procedure Draw_Objects(dc: XCanvas; p,p_lev,p_code: longint);
    function Draw_Object(dc: XCanvas; p,p_lev,p_code: longint): longint;
    function Draw_attr(dc: XCanvas; p,p_code,p_loc: Integer): Boolean;

    function Display_model(p: longint): Boolean; virtual;

    function x_Draw_Object(dc: XCanvas; p: Int64): Boolean; virtual;
    procedure Stereo_Object(dc: XCanvas; p: Int64);

    procedure x_dc_ctrl(dc: XCanvas; mode: Integer);
    procedure x_disp_ctrl(mode: Integer);
    procedure xc_disp_ctrl(mode: Integer);

    procedure x_Disp_Object(dm_i: int; ptr: Int64; mode: int);
    procedure xc_Disp_Object(dm_i: int; ptr: Int64; mode: int);

    function Disp_Object(p: Int64; Mode: Integer): Integer;

    procedure Disp_list(List: TInt64List; Mode: Integer);

    function Free_Occupe_Point(P: Int64): Boolean;

    function ReleaseObject(p: Int64): Integer; virtual;

    procedure OccupeA_Object(p: Int64);

    function Unmark_Object(p: Int64): Integer;
    function Draw_Mark_Object(p: Int64): Int64;

    procedure ext_Edit_Hide(p: Int64);

    function ext_Draw_Object(p: Int64): Boolean; virtual;

    procedure ext_Disp_Object(p: Int64; Mode: Integer);

    procedure ext_Hide_Object(p: Int64);
    function Hide_geo(p: Integer): Integer;

    procedure ocx_Hide_Object(p: Int64);

    procedure Disp_Polyline(dc: XCanvas; lp: PLLine);

    procedure Draw_VPoint(dc: XCanvas;
                          const v: VPoint; d,i: Integer;
                          curve,cross: Boolean);

    procedure Draw_LPoint(dc: XCanvas;
                          const p: TPoint; d,i: Integer;
                          curve,cross: Boolean);

    procedure Draw_lcurve(dc: XCanvas; lp: PLLine);
    procedure Draw_ellipse(dc: XCanvas; lp: PLLine);
    procedure Draw_bspline(dc: XCanvas; lp: PLLine; i1,i2: int);

    procedure Draw_LLine(dc: XCanvas; lp: PLLine);
    procedure Draw_VVector(dc: XCanvas; const a,b: VPoint);
    procedure Draw_LVector(dc: XCanvas; const a,b: TPoint);
    procedure Draw_LSector(dc: XCanvas; lp: PLPoly);

    function LPolyBuf_Movie: Boolean;

    procedure Disp_LPolyBuf_Begin(dc: XCanvas);
    procedure Disp_LPolyBuf_End(dc: XCanvas);

    procedure LPolybuf_update(Value: Boolean);

    procedure LPolybufEvent(Sender: TObject);

    procedure xMark_LPolyBuf(dc: XCanvas); virtual;
    procedure Disp_LPolyBuf(dc: XCanvas);

    function Track_LPolyBuf: Boolean;

    function This_LPolybuf(cX,cY: Integer): Integer;
    function This_LPolyCurve(cX,cY: Integer): TPoint;

    procedure Mark_LPolyBuf(dc: XCanvas);
    procedure Clear_LPolyBuf;

    function LPolybuf_undo: Boolean;

    procedure Mark_LVecBuf(dc: XCanvas);
    procedure Mark_XPolyBuf(dc: XCanvas);

    procedure Clear_LVecBuf;
    procedure begin_LVecBuf(const v: VPoint);
    procedure Back_LVecBuf;

    procedure MouseMove(Shift: TShiftState; X,Y: integer); override;
    procedure MouseDown(Button: TMouseButton; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; X,Y: Integer); override;
    procedure MouseMoveTo(var X,Y: Integer); override;

    function GetHint(Str: PChar; smax: int; var X,Y: int): bool;

    function Tools_Pick(X,Y: integer; out v: VPoint): Int64; virtual;

    procedure Tools_Move(X,Y: integer); virtual;
    procedure Tools_Dn_Left(X,Y: integer); virtual;
    procedure Tools_Dn_Right(X,Y: integer); virtual;
    procedure Tools_Up_Left(X,Y: integer); virtual;
    procedure Tools_Up_Right(X,Y: integer); virtual;
    function Tools_Up_Right1(X,Y: integer): Boolean; virtual;

    procedure plus_Gauss(const lt,rb: TGauss; k: double);
    function Card_xPlus(const a,b: TPoint): Boolean;

    procedure Set_dm_Centre(const v: VPoint; sc,k: double);
    procedure Set_ext_Centre(const g: xgeoid; sc: double);

    procedure Set_geo_Centre(const g: xgeoid; sc: double);

    procedure Set_Gauss_Centre(x,y,z,sc: double);
    procedure Set_wgs_Centre(x,y,z,sc: double);

    function Rebound_prj: Boolean;

    function Get_mpp_centre(out c: TGeoPoint): Double;
    function Set_mpp_centre(const c: TGeoPoint; mpp: Double): bool;

    function Get_prj_centre(out c: xgeoid): Double;
    procedure Set_prj_centre(const c: xgeoid; sc: Double);

    procedure win_frame(const fr: TPrjFrame;
                        sc: Double; const g: XGeoid);

    function Visible_VPoint(const v: VPoint; sc: double): Boolean;
    function Present_VPoint(const v: VPoint; sc: double): Boolean;
    function Up_Local_Centre(const p: TPoint; sc: double): Boolean;

    function Present_LPoly(lp: PLPoly; hp: PIntegers;
                           lp_N: Integer; const v: VPoint;
                           sc: double): Boolean;

    procedure Set_Local_Centre(const c: TPoint; k: double);

    procedure x_Window(const g: xgeoid; sc: double;
                       out lt,rb: tgauss);

    function Get_Gauss_Centre(out g: tgauss): double;

    procedure dm_Zoom_Centre(k: double; z: int;
                             const c: TPoint);

    procedure dm_Zoom_Scale(k: float);

    procedure dm_Set_scale(sc: Double);
    procedure dm_Set_ppm(v: Double);

    procedure dm_Centre(const p: TPoint);
    procedure geo_Centre(const g: TGeoPoint);

    function pcx_Swap(const p,lt,rb: TPoint): Boolean;
    function pcx_Zoom(iz: Integer): Boolean;

    procedure pcx_plus(const a,b: TPoint);

    procedure Show_marker(const p: TPoint; fi: Integer);

    procedure Card_Centre(const c: TPoint); override;
    procedure Card_move(const a,b: TPoint); override;
    procedure Card_plus(const a,b: TPoint); override;
    function Char_Minus(X,Y: integer): Boolean; override;
    function Char_Plus(X,Y: integer): Boolean; override;

    function StrPosition(X,Y: Integer): string; override;
    function StrWhere(const V: VPoint): string;

    procedure Status(Id: Integer; Msg: PChar);

    procedure Show_Status;
    procedure Tool_Status(P: Int64); virtual;

    procedure Card_Help(id: Integer);

    procedure sau_Show; virtual;
    procedure sau_Hide; virtual;

    procedure Change_Rule_pos;
    procedure Change_Rule_dist;

    procedure Begin_show_port;
    procedure Close_show_port;

    procedure MosaicClose(Sender: TObject; Path: PChar);

    procedure Video(avi: int;
                    Frame: PBitmap;
                    Frag: PGPoly);

  protected
    fHints: THintList;

    fcashChanged: bool;

  private
    Pgm: TPgmBank;
    Vgm: TVgmBank;

    fctx: tx_context;
    fpoly: TPolyLPoints;
    fDrawText: IDrawText;
    Fonts: XDrawFonts;
    clip_master: TClipDraw;
    eye_draw: TMovie;

    fdc_mode: int;
    fmarkColor: int;
    fGround: int;
    fGrid: int;

    fx_Mark: bool;
    fx_all: bool;

    fx_Disp: tx_Disp;
    fx_Disp1: int64;

    fCtrl_list: TInt64List;

    fPickup: tdmw_List;
    fQuery: tdmQuery;

    fDrawBuf: TPolyList;
    fScaleList: TObjScaleList;
    fDispAttrs: TDispAttrList;

    fAttrList: VPointList;

    fDrawLink: tdmwLink;
    fmesh: TPolyMesh;

    fMosaic: IMosaic;
    fMosaicBmp: longbool;
    fTextMask: TTextMask;

    fnavList: TObjectList;

    fdm_to_pcx: tdm_to_pcx;
    fpcx_to_dm: tpcx_to_dm;

    fDrawPort: TPoint;
    fDrawCount: Integer;
    fDrawTick: Integer;

    floc_xyz: VPoint;
    floc_xxx: VPoint;

    fPattern_c: TPoint;

    fpoly_min: Integer;
    fpoly_min1: Integer;
    fsign_min: Double;

    fIs_xyz: bool;
    fIs_Ext_Disp: bool;
    fIs_North: bool;
    fIs_Zoom: bool;

    fNorth: Double;
    fNorth_gc: TGauss;
    fNorth_kz: Double;
    fNorthShift: TPoint;

    fMainGlobe: TMapProject;
    fGlobe_c: XGeoid;

    fLPolybuf_update: bool;

    fIs_x2: bool;
    fx2_kz: TGauss;

    fport_lt,fport_rb: TPoint;

    fmin_kz: Double;
    fmax_kz: Double;

    fprj_tr: Real3x3;
    fprj_bt: Real3x3;

    fwin_tr: Real3x3;
    fwin_bt: Real3x3;

    fMapIndex: Integer;

    fdraw_scale: int;

    fdraw_dc: XCanvas;
    fdraw_id: Integer;
    fdraw_lev: int;
    fdraw_node: dm_Node;

    fblankPen: int;
    fblankBru: int;

    fIsRightHand: longbool;
    fIsPcxWhere: longbool;
    fIsGKWhere: longbool;

    fIsScaleMPP: longbool;

    fOnPoint: TOnPoint;
    fOnVector: TOnVector;
    fOnRect: TOnLine;
    fOnPort: TOnPort;
    fOnRing: TOnVector;
    fOnPage: TOnVector;
    fOnGeoid: TOnVector;
    fOnImage: TOnVector;
    fOnPoly: TOnLine;
    fOnPoly1: TOnLine1;
    fOnGauss: TOnLine;
    fOnObject: TOnObject;
    fOnOccupe: TOnObject;
    fOnPair: TOnPair;
    fOnCancel: TNotifyEvent;
    fOnScale: TOnScale;
    fOnDraw: TNotifyEvent;
    fOnRefresh: TOnRefresh;
    fOnBackground: TOnRefresh;
    fOnInvalidate: TOnRefresh;
    fOnTrack: TOnRefresh;
    fOnZoom: TOnZoom;
    fOnXY_BL: TOnXY_BL;

    fOnStatus: TOnStatus;
    fOnChangeState: TNotifyEvent;

    fOnRulePaint: TNotifyEvent;

    fOnRulePos: TLPolyProc;
    fOnRuleDist: TOnRuleDist;
    fOnRuleClose: TLLineFunc;

    fOnStereoPos: TOnStereoPos;
    fOnWhere: TOnWhere;

    fOnSetPcx: TNotifyEvent;
    fOnSwapPcx: TOnSwapPcx;
    fOnRotate: TOnRotate;

    fOnPosition: TNotifyEvent;
    fOnLPolybuf: TNotifyEvent;

    fOnMaster: TFrameEvent;

    fOnDrawObject: TOnDrawObject;

    procedure done_Signs;

    procedure x_Mark_lp(dc: XCanvas; lp: PLLine; Curve: Boolean);
    procedure x_Mark_DPolyBuf(dc: XCanvas; Curve: Boolean);

    function Get_prj_North: Integer;
    procedure Set_prj_North(Value: Integer);

    function Get_Coord_Azimuth: Boolean;

    procedure Get_win_tr(out tr,bt: Real3x3);

    function Get_IsPcx: Boolean;
    procedure Set_IsPcx(Value: Boolean);

    function Get_Is_Eye: Boolean;
    procedure Set_IsXyz(Value: Longbool);

    function Get_IsXyz1: Boolean;

    function Get_Stereo_dx: Integer;

    function Get_Capture_lt: TPoint;
    function Get_Capture_rb: TPoint;

    function prj_Vertex(X,Y,Z: Integer): TPoint;

    function mf_Project(lp: PLLine; hp: PIntegers;
                        out lp_lt,lp_rb: TPoint;
                        pack,mark: Boolean): integer;

    function Draw_trig(vp: PIntegers): HResult;

    procedure Show_Port(out lt,rb: TPoint);

  protected
    function Get_Is_Alx: Boolean; virtual;
    function Get_Is_Rel: Boolean; virtual;

    function rule_close: int;

  public
    property Mosaic: IMosaic read fMosaic write fMosaic;
    property MosaicBmp: longbool write fMosaicBmp;

    property navList: TObjectList read fnavList;

    property dm_to_pcx: tdm_to_pcx read fdm_to_pcx;
    property pcx_to_dm: tpcx_to_dm read fpcx_to_dm;

    property loc_xyz: VPoint read floc_xyz write Set_loc_xyz;
    property loc_xxx: VPoint read Get_loc_xxx write Set_loc_xxx;

    property Pattern_c: TPoint read fPattern_c write fPattern_c;

    property IsPcx: Boolean read Get_IsPcx write Set_IsPcx;
    property IsXyz: Longbool read fIs_xyz write Set_IsXyz;
    property IsXyz1: Boolean read Get_IsXyz1;
    property IsAlx: Boolean read Get_Is_Alx;
    property IsRel: Boolean read Get_Is_Rel;

    property Is_Eye: Boolean read Get_Is_Eye;

    property Stereo_dx: Integer read Get_Stereo_dx;

    property Capture_lt: TPoint read Get_Capture_lt;
    property Capture_rb: TPoint read Get_Capture_rb;

    property Query: tdmQuery read fQuery write fQuery;
    property Pickup: tdmw_List read fPickup write fPickup;

    property Ctrl_list: TInt64List read fCtrl_list;

    property Drawlink: tdmwLink read fDrawlink;

    property North: Double read fNorth write fNorth;
    property North_gc: TGauss read fNorth_gc write fNorth_gc;
    property NorthShift: TPoint write fNorthShift;

    property prj_North: Integer read Get_prj_North
                                write Set_prj_North;

    property prj_tr: Real3x3 read fprj_tr;
    property prj_bt: Real3x3 read fprj_bt;

    property min_kz: Double read fmin_kz;
    property max_kz: Double read fmax_kz;

    property MainGlobe: TMapProject write fMainGlobe;

    property Globe_c: XGeoid write fGlobe_c;

    property poly_min: Integer read fpoly_min
                               write fpoly_min;

    property DrawBuf: TPolyList read fDrawBuf;
    property mesh: TPolyMesh read fmesh;

    property ScaleList: TObjScaleList read fScaleList;

    property MapIndex: Integer read fMapIndex;

    property IsRightHand: bool write fIsRightHand;
    property IsScaleMPP: longbool write fIsScaleMPP;

    property OnPoint: TOnPoint read fOnPoint write fOnPoint;
    property OnVector: TOnVector read fOnVector write fOnVector;
    property OnRect: TOnLine read fOnRect write fOnRect;
    property OnPort: TOnPort read fOnPort write fOnPort;
    property OnRing: TOnVector read fOnRing write fOnRing;
    property OnPage: TOnVector read fOnPage write fOnPage;
    property OnGeoid: TOnVector read fOnGeoid write fOnGeoid;
    property OnImage: TOnVector read fOnImage write fOnImage;
    property OnPoly: TOnLine read fOnPoly write fOnPoly;
    property OnPoly1: TOnLine1 read fOnPoly1 write fOnPoly1;
    property OnGauss: TOnLine read fOnGauss write fOnGauss;
    property OnObject: TOnObject read fOnObject write fOnObject;
    property OnOccupe: TOnObject read fOnOccupe write fOnOccupe;
    property OnPair: TOnPair read fOnPair write fOnPair;
    property OnCancel: TNotifyEvent read fOnCancel write fOnCancel;

    property OnScale: TOnScale read fOnScale write fOnScale;

    property OnDraw: TNotifyEvent read fOnDraw write fOnDraw;
    property OnRefresh: TOnRefresh write fOnRefresh;
    property OnBackground: TOnRefresh write fOnBackground;
    property OnInvalidate: TOnRefresh write fOnInvalidate;
    property OnTrack: TOnRefresh write fOnTrack;

    property OnZoom: TOnZoom read fOnZoom write fOnZoom;
    property OnXY_BL: TOnXY_BL read fOnXY_BL write fOnXY_BL;

    property OnStatus: TOnStatus read fOnStatus write fOnStatus;

    property OnChangeState: TNotifyEvent read fOnChangeState
                                         write fOnChangeState;

    property OnRulePaint: TNotifyEvent write fOnRulePaint;

    property OnRulePos: TLPolyProc write fOnRulePos;
    property OnRuleDist: TOnRuleDist write fOnRuleDist;
    property OnRuleClose: TLLineFunc write fOnRuleClose;

    property OnStereoPos: TOnStereoPos write fOnStereoPos;
    property OnWhere: TOnWhere read fOnWhere write fOnWhere;

    property OnSetPcx: TNotifyEvent write fOnSetPcx;
    property OnSwapPcx: TOnSwapPcx write fOnSwapPcx;

    property OnRotate: TOnRotate write fOnRotate;

    property OnPosition: TNotifyEvent write fOnPosition;
    property OnLPolybuf: TNotifyEvent write fOnLPolybuf;

    property OnMaster: TFrameEvent read fOnMaster write fOnMaster;

    property OnDrawObject: TOnDrawObject write fOnDrawObject;

    property Coord_Azimuth: Boolean read Get_Coord_Azimuth;

    procedure StereoPos(z: Integer; abs,up: Boolean);
  end;

  TDrawTile = class
    constructor Create(Globe: TMapProject);
    destructor Destroy; override;

    procedure Draw(DC: XCanvas; left,top,pixx,pixy: double);
  private
    fcard: TDrawCard;
  end;

  TDrawProc = procedure(DC: THandle; Card: IDrawCard) of object;

  TProjMosaic = class(TInterfacedObject,IProject2)

    constructor Create(ACard: TDrawCard;
                       const Amap: TMapRec);

    function Project(x,y: Double): TGauss; stdcall;
    function Backup(x,y: Double): TGauss; stdcall;

    function Get_d2(out tr,bt: Real3x3): HResult; stdcall;
    function Get_poly(out tr,bt: TPolynom): HResult; stdcall;

    function Push: HResult; stdcall;
    procedure Pop; stdcall;

    procedure MoveTo(dx,dy: Double); stdcall;
    procedure Scale(kz: Double); stdcall;
  private
    fCard: TDrawCard;
    fLink: tdmwLink;

    fmap_tr: Real3x3;
    fmap_bt: Real3x3;

    fprj_tr: Real3x3;
    fprj_bt: Real3x3;

    fScale: Double;

    fsys1: tsys;
    fsys2: tsys;

    fIsPcx: Longbool;

  public
    property sys1: tsys read fsys1;
  end;

  TWgsMosaic = class(TInterfacedObject,IProject2)

    constructor Create(ACard: TDrawCard);

    function Project(x,y: Double): TGauss; stdcall;
    function Backup(x,y: Double): TGauss; stdcall;

    function Get_d2(out tr,bt: Real3x3): HResult; stdcall;
    function Get_poly(out tr,bt: TPolynom): HResult; stdcall;

    function Push: HResult; stdcall;
    procedure Pop; stdcall;

    procedure MoveTo(dx,dy: Double); stdcall;
    procedure Scale(kz: Double); stdcall;
  private
    fCard: TDrawCard;
    fLink: tdmwLink;
    fIsPcx: Longbool;

    fsys: tsys;

    fprj_tr: Real3x3;
    fprj_bt: Real3x3;

  public
    property sys: tsys read fsys;
  end;

implementation

uses
  Sysutils,Graphics,
  convert,ofiles,xlog,
  xddw,xgdi,ivg,xutils,

  xline1,xpoly,xy,
  xcurve,xmath,
  xcross,xkeys,xdc,

  img_x,ximages,
  xbl_use,dmx_draw,
  freetypep;

const
  _small = 1E-9;

constructor TNavCustom.Create(ACard: TDrawCard);
begin
  inherited Create; fCard:=ACard
end;

constructor TDrawCard.Create(ACard: ICardBox);
begin
  inherited Create(ACard);

  Link_index:=1; p_step:=1;
  OnCashDraw:=Cash_Draw;

  Stereo_a.OnDraw:=Cash_Draw;
  Stereo_b.OnDraw:=Cash_Draw;

  Pgm:=TPgmBank.Create(0,0);
  Vgm:=TVgmBank.Create(1,0);
  Vgm.is_Skip:=true;

  fctx:=tx_context.Create(Vgm);
  fpoly:=fctx.poly;

  GetDrawText(fDrawText);

  Cash.SetDrawText(fDrawText);
  Cash1.SetDrawText(fDrawText);
  Cash2.SetDrawText(fDrawText);
  Cash3.SetDrawText(fDrawText);

  Fonts:=XDrawFonts.Create(fctx,fDrawText);
  clip_master:=TClipDraw.Create(fctx,Fonts);
  clip_master.proj_lp:=lp_Project;

  eye_draw:=fctx.eye_draw;

  fCtrl_List:=TInt64List.Create;
  fCtrl_List.Duplicates:=false;

  fDrawBuf:=TPolyList.Create;
  fScaleList:=TObjScaleList.Create;

  fDrawlink:=tdmwLink.Create;
  fDispAttrs:=TDispAttrList.Create;
  fHints:=THintList.Create;

  fTextMask:=TTextMask.Create;

  fAttrList:=VPointList.Create(1024);

  fmesh:=TPolyMesh.Create;

  fnavList:=TObjectList.Create;

  x_Edit:=tx_Edit.Create;

  fIsPcxWhere:=Param_Option('/pcx');
  fIsGKWhere:=Param_Option('/gk');

  z_dst_round:=false;
  lp_View:=3;
end;

destructor TDrawCard.Destroy;
begin
  Cash.SetDrawText(nil);
  Cash1.SetDrawText(nil);
  Cash2.SetDrawText(nil);
  Cash3.SetDrawText(nil);

  fQuery.Free;
  fPickup.Free;

  x_Edit.Free;

  fnavList.Free;

  fmesh.Free;

  fAttrList.Free;

  fTextMask.Free;

  fHints.Free;
  fDispAttrs.Free;

  fDrawLink.Free;

  fScaleList.Free;
  fDrawBuf.Free;

  fCtrl_List.Free;

  clip_master.Free;
  Fonts.Free;

  fDrawText:=nil;

  fctx.Free;

  Pgm.Free;
  Vgm.Free;

  inherited
end;

function TDrawCard.InitGlobe: TMapProject;
begin
  Globe:=TMapProject.Create;
  Globe.Cards.Add(Self);

  Pickup:=tdmw_List.Create(Globe.Map);
  Query:=tdmQuery.Create(Globe.Map,Pickup,x_Edit);

  Result:=Globe
end;

function TDrawCard.FreeGlobe: TMapProject;
begin
  Globe.Free; Result:=nil
end;

procedure TDrawCard.notify(Sender: TObject; Cmd: Integer);
begin
  with Globe do begin
    fNorth_gc.x:=px_lt.x/2 + px_rb.x/2;
    fNorth_gc.y:=px_lt.y/2 + px_rb.y/2;
    fNorth:=px_north
  end
end;

procedure TDrawCard.done_Signs;
begin
  Pgm.vm_Close; Vgm.vm_Close
end;

procedure TDrawCard.x_Mark_lp(dc: XCanvas; lp: PLLine; Curve: Boolean);
var
  dr: int;
begin
  dr:=Globe.Params.mark_dr;
  fctx.MarkPoly(dc,@lp.Pol,lp.N,dr,Curve)
end;

procedure TDrawCard.x_Mark_DPolyBuf(dc: XCanvas;  Curve: Boolean);
begin
  if not eye_draw.Enabled then
  x_Mark_lp(dc,x_Edit.DPolyBuf,Curve)
end;

function TDrawCard.Movie_Flags: Integer;
begin
  Result:=0; if is_Eye then begin
    Result:=movie_odd;
    if not Globe.Stereo.Left then
    Result:=movie_even
  end
end;

function TDrawCard.Cursor_stereo(out dx: Integer): Boolean;
begin
  inherited Cursor_stereo(dx);
  Inc(dx,Round(xyz_dx / z_src * z_dst));
  Result:=Is_Eye
end;

procedure TDrawCard.Set_loc_dot(const p: TPoint);
begin
  inherited Set_loc_dot(p);
  floc_xyz.x:=p.x;
  floc_xyz.y:=p.y;
  floc_xyz.z:=0
end;

procedure TDrawCard.Set_loc_xyz(const v: VPoint);
var
  p: TPoint;
begin
  p.x:=v.x; p.y:=v.y; loc_dot:=p;
  floc_xyz.z:=v.z
end;

function TDrawCard.Get_loc_xxx: VPoint;
begin
  Result:=floc_xyz; if xxx_mode then
  if IsXYZ then Result:=floc_xxx
end;

procedure TDrawCard.Set_loc_xxx(const v: VPoint);
begin
  floc_xxx:=v
end;

procedure TDrawCard.pcx_Card_zoom(cls: Boolean);
begin
  if IsPcx then Card_Zoom(cls)
end;

procedure TDrawCard.Get_win(iv: PIntegers; rv: PDoubles);
begin
  inherited Get_win(iv,rv);
  if not IsPcx then rv[0]:=kz;
  iv[3]:=ibool[IsPcx]; iv[4]:=dispFlags
end;

procedure TDrawCard.Set_win(iv: PIntegers; rv: PDoubles);
begin
  if IsPcx then
    inherited Set_win(iv,rv)
  else begin
    zoom:=iv[0];
    centre:=Point(iv[1],iv[2]);
    kz:=rv[0]; Card_zoom(true);
  end;

  dispFlags:=iv[4]
end;

procedure TDrawCard.Get_win_tr(out tr,bt: Real3x3);
var
  kx,ky: Double;
begin
  kx:=kz; ky:=kz; if fis_x2 then
  begin kx:=fx2_kz.x; ky:=fx2_kz.y end;

  Init_3x3(tr, -XPos,-YPos, kx,ky);

  Init_3x3(bt, +XPos,+YPos, 1,1);
  xy_Scale_3x3(bt,1/kx,1/ky);
end;

procedure TDrawCard.Card_zoom(cls: Boolean);

function Model_Matrix(fov2,teta: Float;
                      ox,oy,pw,ph: Double;
                      out itr: TMatrix): TMatrix;
var
  l,x,h,t1,t2,vx,vy,px,py: Double;
  tr: TMatrix;
begin
  vx:=0; vy:=1; l:=ph/2;

  t1:=Tan((90 - (fov2-teta))/180*Pi);
  t2:=Tan((90 - teta)/180*Pi);

  x:=l*t2 / (t1+t2); h:=x * t1;

  px:=ox + pw/2;
  py:=oy + ph - x;

  tr:=Matrix_Identify;
  tr[1,1]:=-1;

  Translate_Matrix(tr,-px,py,-h);
  RotateX_Matrix(tr,-teta/180*Pi);

  itr:=Matrix_RotateX(teta/180*Pi);
  Translate_Matrix(itr,px,py,h);

  Result:=tr
end;

procedure Set_perspective;
var
  fov2,teta,w,h: Integer;
  x0,y0,gw,gh: Double;
begin
  fov2:=0; teta:=0;
  if Globe.d3_on = 1 then begin
    fov2:=Globe.d3_fov div 2;
    teta:=Globe.d3_teta
  end;

  if fov2 in [10..60] then
  if teta in [0..60] then begin

    x0:=XPos / kz; y0:=YPos / kz;

    w:=Card.Width;  gw:=w / kz;
    h:=Card.Height; gh:=h / kz;

    xyz_t:=Model_Matrix(fov2,teta,x0,y0,gw,gh,xyz_bt);

    xyz_focus:=h/2 * Tan( (90-fov2) / 180 * Pi);

    xyz_bt[0,2]:=-xyz_bt[0,2]*xyz_focus;
    xyz_bt[1,2]:=-xyz_bt[1,2]*xyz_focus;
    xyz_bt[2,2]:=-xyz_bt[2,2]*xyz_focus;

    xyz_c.x:=w/2; xyz_c.y:=h/2;

    Cash.IsRoad:=true;
  end
end;

function map_mm_scale: Double;
var
  a,b: TGauss; r: Double;
begin
  Result:=1;
  with Globe.Map do begin
    a.x:=dm_lt.X; a.y:=dm_lt.Y;
    b.x:=dm_rb.X; b.y:=dm_rb.Y;
  end;

  r:=MaxInt/1.2;
  Result:=Min(1,Min(r/(b.x-a.x+1),r/(b.y-a.y+1)));
end;

const
  MaxRange = 256 * 256 * 256 * 4;
var
  rb,ab,p: TPoint; s,t: TGauss; G: GOrient;
  k,kx,ky,ed,res: double; lg: AB_Orient;
begin
  fprj_tr:=Identify_3x3;
  fprj_bt:=prj_tr;

  Cash.IsRoad:=false;

  if Card <> nil then
  if Globe <> nil then

  if isPcx then begin
    inherited Card_zoom(cls);
    Set_prj_Bound(0); kz:=1
  end
  else begin

    if Globe.Get_Active_lg_t(lg) then

    if Assigned(fOnMaster) then begin
      res:=fOnMaster(@G);

      if res > 0 then begin
        s:=Gauss_centre(@G,4);
        Globe.Map.LG_T.Z_to_L(s,p);
        lg.L_to_G(p,fNorth_gc);
        fNorth:=-xAzimuth(G[0],G[1])*180/Pi;
        fIs_North:=true; kz:=res;
      end
    end;

    s:=Set_prj_Bound(fNorth);
    ed:=Globe.px_ed;

    kx:=Card.Width/s.x;
    ky:=Card.Height/s.y;
    k:=Min(kx,ky) / map_mm_scale;
    fmin_kz:=k;

    fmax_kz:=Globe.px_ppm;
    if rus_interface then
    fmax_kz:=Globe.Map.Points_to_Metre*32;

    z_dst:=1;

    p.X:=Round(s.x * ed);
    p.Y:=Round(s.y * ed);

    if fGlobe_c.s.pps = 1 then
    if Globe.p_lg.s.pps = 1 then begin
      t:=prj_inside(fGlobe_c,Globe.p_lg.s);
      xy_to_sm(t,centre); fGlobe_c.s.pps:=0
    end;

    with centre do begin
      if (X < 0) or (X > Max(p.X,p.Y)) then zoom:=-1;
      if (Y < 0) or (Y > Max(p.X,p.Y)) then zoom:=-1;
    end;

    if zoom = -1 then begin
      centre.X:=p.X div 2;
      centre.Y:=p.Y div 2;
      zoom:=1; kz:=fmin_kz;
      fIs_North:=false;
    end;

    kz:=Max(kz,fmin_kz);
    if not Globe.is_mm then
    kz:=Min(kz,fmax_kz);

    if fIs_North then begin
      xy_to_sm(fNorth_gc,centre);

      if fNorth_kz > fmin_kz then
      if fNorth_kz < fmax_kz then
      kz:=fNorth_kz;

      fIs_North:=false; cls:=true
    end;

    while true do begin
      t.x:=Round(s.x*kz+0.5);
      t.y:=Round(s.y*kz+0.5);
      if t.x < MaxRange then
      if t.y < MaxRange then
      Break; kz:=kz-0.01
    end;

    p.X:=Card.Width; p.Y:=Card.Height;

    xLine:=p.X div 3; xPage:=p.X * 7 div 8;
    xMaxValue:=Round(t.x); xRange:=Max(0,xMaxValue-p.X);

    yLine:=p.Y div 3; yPage:=p.Y * 7 div 8;
    yMaxValue:=Round(t.y); yRange:=Max(0,yMaxValue-p.Y);

    xPos:=Round(centre.X / ed * kz) - (p.X div 2);
    yPos:=Round(centre.Y / ed * kz) - (p.Y div 2);

    Dec(xPos,p.X * fNorthShift.X div 100);
    Dec(yPos,p.Y * fNorthShift.Y div 100);

    xPos:=Min(Max(0, xPos), xRange);
    yPos:=Min(Max(0, yPos), yRange);

    if not isMoving then Setup_units;

    dm_t_init:=false;

    xyz_focus:=-1; Cash.IsRoad:=false;
    if Globe.Count > 0 then Set_perspective;

    if cls then Cash.Clear_dc(false);

    Get_win_tr(fwin_tr,fwin_bt);
  end;

  fIs_North:=false;
  fNorth_kz:=-1;
  fNorthShift:=Point(0,0);

  fis_Zoom:=true
end;

function TDrawCard.Get_min_scale: Longint;
begin
  Result:=int_Round(Round(kz_Scale(nil,Globe.px_ppm)),100);
end;

function TDrawCard.Get_max_scale: Longint;
begin
  Result:=int_Round(Round(kz_Scale(nil,fmin_kz)),100);
end;

function TDrawCard.Get_prj_size: TSize;
var
  s: TGauss; p: TPoint; ed: Double;
begin
  Result.cx:=0; Result.cy:=0;

  if Card <> nil then
  if Globe <> nil then begin

    s:=Set_prj_Bound(fNorth);
    ed:=Globe.px_ed;

    Result.cx:=Round(s.x * ed);
    Result.cy:=Round(s.y * ed);
  end
end;

function TDrawCard.Get_prj_tr(out tr,bt: Real3x3): Boolean;
var
  k: TGauss; o: TPoint; wt: Real3x3;
begin
  Result:=false;
  tr:=Identify_3x3; bt:=tr;

  if not IsPcx then begin
    k.x:=kz; k.y:=k.x;
    if fis_x2 then k:=fx2_kz;

    if Abs(k.x) > Small then
    if Abs(k.y) > Small then begin

      o.X:=XPos; o.Y:=YPos;

      Init_3x3(wt, -o.X,-o.Y, k.x,k.y);
      tr:=fprj_tr; Forw_3x3(tr,wt);

      Init_3x3(wt, +o.X,+o.Y, 1,1);
      xy_Scale_3x3(wt,1/k.x,1/k.y);
      Forw_3x3(wt,fprj_bt); bt:=wt;

      Result:=true
    end
  end
end;

procedure TDrawCard.clip_centre(var c: TPoint);
var
  s: TSize;
begin
  s:=Get_prj_Size;
  if (c.X < 0) or (c.X >= s.cx) then c.X:=s.cx div 2;
  if (c.Y < 0) or (c.Y >= s.cy) then c.Y:=s.cy div 2;
end;

function TDrawCard.prn_Get_win(out wp,pp: TPort2d): Boolean;
var
  i,w,h: Integer; g: GOrient;
  tr,bt: Real3x3; mpp: Double;
  lt,rb: TPoint; lt1,rb1: TGauss;
begin
  Result:=false;

  Fillchar(wp,Sizeof(wp),0);
  Fillchar(pp,Sizeof(pp),0);

  if dm_t_init then

  if IsPcx then begin
    pcx_Port(lt,rb); mpp:=pcx_mpp;
    pp.w:=imWidth * mpp; pp.h:=imHeight * mpp;
    wp.x:=lt.X*mpp; wp.y:=lt.Y*mpp;
    wp.w:=(rb.X-lt.X+1)*mpp;
    wp.h:=(rb.Y-lt.Y+1)*mpp;
    Result:=true
  end
  else begin
    Get_win_tr(tr,bt);

    w:=Card.Width; h:=Card.Height;
    g[0]:=Transit_3x3(0,0,bt);
    g[1]:=Transit_3x3(w,0,bt);
    g[2]:=Transit_3x3(0,h,bt);

    wp.o:=g[0];
    wp.w:=Gauss_dist(g[0],g[1]);
    wp.h:=Gauss_dist(g[0],g[2]);

    pp.s:=Globe.get_prj_t(fNorth_gc,fNorth,tr,bt);

    with Globe do
    Bound_to_GOrient(px_lt1,px_rb1,@G);

    for i:=0 to 3 do with G[i] do
    G[i]:=Transit_3x3(x,y,tr);

    Max_Gauss_Bound(@G,4,lt1,rb1); pp.o:=lt1;
    pp.w:=Max(1,rb1.x - lt1.x);
    pp.h:=Max(1,rb1.y - lt1.y);

    Result:=true
  end
end;

function TDrawCard.prn_Get_frag(lp: PLLine): TPort2d;
var
  g: GOrient; mpp: Double;
begin
  if IsPcx then begin
    mpp:=pcx_mpp;
    g[0]:=dm_to_img(lp.Pol[0],mpp);
    g[1]:=dm_to_img(lp.Pol[1],mpp);
    g[3]:=dm_to_img(lp.Pol[3],mpp);
  end
  else begin
    p_Gauss(lp.Pol[0],g[0]);
    p_Gauss(lp.Pol[1],g[1]);
    p_Gauss(lp.Pol[3],g[3]);
  end;

  Result.o:=g[0];
  Result.s.x:=Gauss_dist(g[0],g[1]);
  Result.s.y:=Gauss_dist(g[0],g[3]);
end;

function TDrawCard.prn_Get_poly(const fr: TPort2d): VLLine;
var
  i: Integer; mpp: Double;
  g: GOrient; l: VLLine; t: TGauss;
begin
  with fr do
  Frame_to_GOrient(x,y,w,h,@g);

  if IsPcx then begin
    mpp:=pcx_mpp;
    for i:=0 to 3 do with g[i] do begin
      t:=card_to_img1(x/mpp,y/mpp);
      Globe.pcx_to_dm(t.x,t.y,l.Pol[i])
    end
  end
  else begin
    for i:=0 to 3 do
    p_Local(g[i],l.Pol[i]);
  end;

  l.N:=4; l.Pol[4]:=l.Pol[0];
  Result:=l
end;

procedure TDrawCard.prn_Show_frag(const fr: TPort2d);
var
  lt,rb: TPoint; l: VLLine;
begin
  l:=prn_Get_poly(fr);
  if l.N = 4 then begin
    Max_Poly_Bound(@l.Pol,4,lt,rb);
    Card_xPlus(lt,rb);
  end
end;

function TDrawCard.GeoToMeter(const g: TGeoPoint): TGauss;
var
  t: TGauss;
begin
  t:=geo_to_xy(g,Globe.p_lg.s);
  Result:=Transit_3x3(t.x,t.y,fprj_tr)
end;

function TDrawCard.Set_prj_Bound(ANorth: Double): TGauss;
begin
  Result:=Globe.get_prj_t(fNorth_gc,ANorth,
                          fprj_tr,fprj_bt);

  if fIs_North then xy_to_sm(fNorth_gc,centre);
end;

function TDrawCard.Get_prj_North: Integer;
begin
  Result:=Round(fNorth)
end;

procedure TDrawCard.Set_prj_North(Value: Integer);
begin
  fIs_North:=true;
  sm_to_xy(centre,fNorth_gc);
  fNorth:=Value;

  if not IsMoving then
  Card_Update(true)
end;

procedure TDrawCard.Card_road(gx,gy,fi,dr: Double; pps,dx,dy: Integer);
var
  p: TPoint;
begin
  if not IsPcx then
  if Globe.Count > 0 then begin

    with Globe.Map.LG_T do
    if pps = 0 then Z_to_L(_Gauss(gx,gy),p)
    else            R_to_L(_Geoid(gx,gy),p);

    lg_p.L_to_G(p,fNorth_gc);
    fIs_North:=true; fNorth:=fi;
    fNorthShift:=Point(dx,dy);
    fNorth_kz:=-1;

    if dr >= 0.01 then
    fNorth_kz:=Card.Width / dr;

    Card_Update(true);
  end
end;

procedure TDrawCard.Set_Code_Chars(mask: Int64);
var
  i: Integer; bit: Int64;
begin
  if mask = 0 then begin
    FillChar(Code_Chars,SizeOf(Code_Chars),true);
    Is_Code_Chars:=false
  end
  else begin
    FillChar(Code_Chars,SizeOf(Code_Chars),false);
    Is_Code_Chars:=true;

    bit:=1; for i:=0 to 63 do begin

      if mask and bit <> 0 then

      if i < 10 then
        Code_Chars[chr(ord('0')+i)]:=true
      else
        Code_Chars[chr(ord('A')+i-10)]:=true;

      bit:=bit shl 1
    end
  end
end;

function TDrawCard.Get_IsPcx: Boolean;
begin
  Result:=Assigned(dm_to_pcx) and                             
          Assigned(pcx_to_dm)
end;

procedure TDrawCard.Set_IsPcx(Value: Boolean);
var
  x,y,w,h: Integer;
begin
  Set_Image(nil); fIs_xyz:=false;
  fdm_to_pcx:=nil; fpcx_to_dm:=nil;

  if Value then begin
    Globe.get_pcx_plan(x,y,w,h);
    Globe.Pcx_link.xRefresh;

    if Open_image(x,y,w,h,nil,Globe.Image) then begin
      fdm_to_pcx:=Globe.dm_to_pcx;
      fpcx_to_dm:=Globe.pcx_to_dm;

      Globe.ImageMirror:=false;
      Globe.ImageMirror:=true;

      if Globe.Count > 0 then
      Globe.Set_map_sm_T(Globe.ActiveMap);
    end
  end;

  OTypes.Notify(Self,fOnSetPcx)
end;

function TDrawCard.Get_Is_Alx: Boolean;
begin
  Result:=IsXyz
end;

function TDrawCard.Get_Is_Rel: Boolean;
begin
  Result:=false
end;

function TDrawCard.Get_Is_Eye: Boolean;
begin
  Result:=IsPcx and Globe.Stereo.Eye
end;

procedure TDrawCard.Set_IsXyz(Value: Longbool);
begin
  fdm_to_pcx:=nil; fpcx_to_dm:=nil;

  if Value then begin
    fpcx_to_dm:=Globe.scb_to_dm;
    fdm_to_pcx:=Globe.dm_to_scb;
  end;

  fIs_xyz:=Value
end;

function TDrawCard.Get_IsXyz1: Boolean;
begin
  Result:=IsXyz;
end;

function TDrawCard.Get_Stereo_dx: Integer;
begin
  Result:=Stereo_a.Depth + Round(xyz_dx / z_src * z_dst)
end;

function TDrawCard.Get_Capture_lt: TPoint;
begin
  Result:=Capture.R.lt; if is_Eye then
  if not Globe.Stereo.Left then
  Inc(Result.x,Stereo_dx)
end;

function TDrawCard.Get_Capture_rb: TPoint;
begin
  Result:=Capture.R.rb; if is_Eye then
  if not Globe.Stereo.Left then
  Inc(Result.x,Stereo_dx)
end;

procedure TDrawCard.StereoPos(z: Integer; abs,up: Boolean);
begin
  if Assigned(fOnStereoPos) then
  fOnStereoPos(z/Globe.Map.z_res,abs,up)
end;

function TDrawCard.clip_poly(bp: PLPoly; bn: Integer): Boolean;
var
  i: Integer; lt,rb: TPoint;
begin
  lt:=win_lt; rb:=win_rb;

  for i:=0 to bn-1 do
  with bp[i] do
  if (X < lt.X) or (X > rb.X)
  or (Y < lt.Y) or (Y > rb.Y) then
  begin Result:=true; Break end
end;

function TDrawCard.xProject(x,y,z: Integer): TPoint;
var
  p: TPoint;
begin
  if IsXyz then begin
    Globe.xyz_to_scb(x,y,z, p);

    if Is_Eye then
    if not Globe.Stereo.Left then
    Inc(p.x,Round(Stereo_a.Depth * z_src));

    Result:=img_Project(p.X,p.Y)
  end
  else Project(x,y, Result)
end;

function TDrawCard.xyz_Project(x,y,z: integer): TPoint;
begin
  Result:=xProject(x,y,z)
end;

function TDrawCard.xyz_Backup(x,y: double): VPoint;
var
  p: TPoint;
begin
  if IsXyz then begin pcx_Backup(x,y,p);
    Globe.scb_to_xyz(p.x,p.x+xyz_dx,p.y, Result);
  end
  else begin
    Backupf(x,y,p);
    Result.x:=p.x; Result.y:=p.y;
    Result.z:=0
  end
end;

function TDrawCard.xxx_Project(const v: VPoint): TPoint;
var
  p: TPoint;
begin
  if IsXyz and xxx_mode then begin
    p.x:=v.x; p.y:=v.y;
    if not Globe.Stereo.Left then begin
      Inc(p.x,v.z); if Is_Eye then
      Inc(p.x,Round(Stereo_a.Depth * z_src))
    end;

    Result:=img_Project(p.X,p.Y)
  end else

  Result:=xProject(v.x,v.y,v.z)
end;

function TDrawCard.xxx_Backup(x,y: Integer): VPoint;
var
  p: TPoint;
begin
  if IsXyz and xxx_mode then begin
    pcx_Backup(x,y,p);
    Result.x:=p.x; Result.y:=p.y;
    Result.z:=xyz_dx;
  end else

  Result:=xyz_Backup(x,y)
end;

function TDrawCard.xxx_to_xyz(const v: VPoint): VPoint;
begin
  Result:=v; if IsXyz and xxx_mode then
  Globe.scb_to_xyz(v.x,v.x+v.z,v.y, Result)
end;

function TDrawCard.gProject(const g: TGauss): TPoint;
var
  p: TPoint;
begin
  Globe.Map.LG_T.Z_to_L(g,p);
  Project(p.x,p.y,Result);
end;

function TDrawCard.depth_Backup(X,Y: integer;
                                const v: VPoint): VPoint;
var
  p1,p2: TPoint;
begin
  pcx_Backup(X,Y,p2);

  if Is_Eye then
  if not Globe.Stereo.Left then
  Dec(p2.x,Round(Stereo_a.Depth * z_src));

  Globe.StereoLeft(true);
  Globe.xyz_to_scb(v.x,v.y,v.z,p1);
  Globe.scb_to_xyz(p1.x,p2.x,p1.y,Result)
end;

function TDrawCard.depth_Stereo(X,Y: integer;
                                const v: VPoint): VPoint;
var
  p: TPoint;
begin
  if xxx_mode then begin
    pcx_Backup(X,Y,p);

    if Is_Eye then
    if not Globe.Stereo.Left then
    Dec(p.x,Round(Stereo_a.Depth * z_src));

    Result:=v;
    Result.z:=p.x-v.x
  end else

  Result:=depth_Backup(X,Y, v)
end;

procedure TDrawCard.pcx_Centre(out c: TPoint);
begin
  inherited Backup(Card.Width div 2,Card.Height div 2,c)
end;

procedure TDrawCard.pcx_Port(out a,b: TPoint);
begin
  inherited Backup(0,0,a); inherited
  Backup(Card.Width-1,Card.Height-1,b);
end;

function TDrawCard.Get_kz_mpp: Double;
begin
  Result:=kz; if isPcx then Result:=pcx_kz;
  Result:=CardScale(Result)
end;

function TDrawCard.pcx_ed(sm: Boolean): double;
var
  p1,p2: TPoint; g1,g2: TGauss; dist: Double;
begin
  p1.x:=(imWidth shr 1)-16; p2.x:=p1.x+32;
  p1.y:=(imHeight shr 1)-16; p2.y:=p1.y+32;

  pcx_to_dm(p1.x,p1.y,p1);
  pcx_to_dm(p2.x,p2.y,p2);
  dist:=Long_Dist(p1,p2);

  if sm then begin
    lg_p.L_to_G(p1,g1); lg_p.L_to_G(p2,g2);
    dist:=Gauss_Dist(g1,g2) * Globe.px_ed
  end;

  Result:=Max(dist/Hypot(32,32),1)
end;

function TDrawCard.pcx_kz: double;
var
  g1,g2: tgauss; d: double;
begin
  Result:=1; pBackup(0,0,g1);
  pBackup(Card.Width,Card.Height,g2);
  d:=Gauss_Dist(g1,g2); if d > 0 then
  Result:=Hypot(Card.Width,Card.Height)/d
end;

function TDrawCard.pcx_mpp: double;
begin
  Result:=Globe.Pcx_link.Get_resolution(imWidth,imHeight)
end;

function TDrawCard.dm_to_bmp(const p: TPoint): TPoint;
var
  t: tgauss;
begin
  dm_to_pcx(p.x,p.y,t);
  t:=img_to_card(t.x,t.y);
  Result.X:=Round(t.x);
  Result.Y:=Round(t.y)
end;

function TDrawCard.dm_to_img(const p: TPoint; k: Double): TGauss;
var
  t: tgauss;
begin
  dm_to_pcx(p.x,p.y,t);
  t:=img_to_card(t.x,t.y);
  Result.X:=t.x * k;
  Result.Y:=t.y * k
end;

function TDrawCard.xy_lnk_dm(const g: tgauss): TPoint;
begin
  with Globe.Map.LG_T do
  Result:=xy_sys_dm(g,Link_T.sys)
end;

function TDrawCard.dm_lnk_xy(const p: TPoint): TGauss;
begin
  with Globe.Map.LG_T do
  Result:=dm_sys_xy(p,Link_T.sys);
end;

function TDrawCard.prn_to_dm(const p: TGauss): TPoint;
var
  t: TGauss; mpp: Double;
begin
  if not IsPcx then
    p_Local(p,Result)
  else begin
    mpp:=pcx_mpp;
    t:=card_to_img1(p.x/mpp,p.y/mpp);
    Globe.pcx_to_dm(t.x,t.y,Result);
  end
end;

function TDrawCard.dm_to_dc(px,py: Double): TGauss;
var
  g,t: tgauss; f: Double; v: txyz;
begin
  if Assigned(dm_to_pcx) then begin
    dm_to_pcx(Round(px),Round(py),g);
    Result:=inherited xy_to_pix(g.x,g.y)
  end
  else begin
    if dm_t_mode = 0 then begin
      f:=dm_t[3,1]*px + dm_t[3,2]*py + 1;
      t.x:=(dm_t[1,1]*px + dm_t[1,2]*py + dm_t[1,3]) / f;
      t.y:=(dm_t[2,1]*px + dm_t[2,2]*py + dm_t[2,3]) / f;
    end else
    if dm_t_mode = 254 then begin
      v.x:=px*xyz_p[0,0] + py*xyz_p[0,1] + xyz_p[0,3];
      v.y:=px*xyz_p[1,0] + py*xyz_p[1,1] + xyz_p[1,3];
      v.z:=px*xyz_p[2,0] + py*xyz_p[2,1] + xyz_p[2,3];

      t.x:=xyz_c.x - xyz_focus*v.x/v.z;
      t.y:=xyz_c.y + xyz_focus*v.y/v.z;
    end else

    if dm_t_mode = 255 then
      fDrawLink.pix_to_xy(px,py,t)
    else begin
      g:=ab_Project(px,py, dm_lg_t,lg_p);
      f:=lg_t[3,1]*g.x + lg_t[3,2]*g.y + 1;
      t.x:=(lg_t[1,1]*g.x+lg_t[1,2]*g.y+lg_t[1,3]) / f;
      t.y:=(lg_t[2,1]*g.x+lg_t[2,2]*g.y+lg_t[2,3]) / f
    end;

    Result:=t
  end
end;

procedure TDrawCard.Project(x,y: longint; out p: TPoint);
var
  g,t: tgauss; px,py,f: Double; v: txyz;
begin
  if Assigned(dm_to_pcx) and
    Assigned(pcx_to_dm) then begin
    dm_to_pcx(x,y,g);
    p:=img_Project(g.x,g.y)
  end
  else begin
    px:=x; py:=y;

    if dm_t_mode = 0 then begin
      f:=dm_t[3,1]*px + dm_t[3,2]*py + 1;
      t.x:=(dm_t[1,1]*px + dm_t[1,2]*py + dm_t[1,3]) / f;
      t.y:=(dm_t[2,1]*px + dm_t[2,2]*py + dm_t[2,3]) / f;
    end else
    if dm_t_mode = 254 then begin
      v.x:=px*xyz_p[0,0] + py*xyz_p[0,1] + xyz_p[0,3];
      v.y:=px*xyz_p[1,0] + py*xyz_p[1,1] + xyz_p[1,3];
      v.z:=px*xyz_p[2,0] + py*xyz_p[2,1] + xyz_p[2,3];

      if v.z < -0.1 then begin
        t.x:=xyz_c.x - xyz_focus*v.x/v.z;
        t.y:=xyz_c.y + xyz_focus*v.y/v.z;
      end
      else begin
        if v.x < 0 then t.x:=-16
        else t.x:=xyz_c.x*2+16;

        if v.y > 0 then t.y:=-16
        else t.y:=xyz_c.y*2+16;
      end

    end else

    if dm_t_mode = 255 then
      fDrawLink.pix_to_xy(x,y,t)
    else begin
      g:=ab_Project(px,py, dm_lg_t,lg_p);
      f:=lg_t[3,1]*g.x + lg_t[3,2]*g.y + 1;
      t.x:=(lg_t[1,1]*g.x+lg_t[1,2]*g.y+lg_t[1,3]) / f;
      t.y:=(lg_t[2,1]*g.x+lg_t[2,2]*g.y+lg_t[2,3]) / f
    end;

    p.X:=Round(t.x); p.Y:=Round(t.y);
  end
end;

procedure TDrawCard.Backup(x,y: integer; out p: TPoint);
begin
  Backupf(x,y,p)
end;

procedure TDrawCard.Backupf(x,y: double; out p: TPoint);
var
  g,t: tgauss; v: lxyz; kx,ky,f: Double; u: txyz;
begin
  if IsPcx then begin
    g:=img_Backup(x,y);
    pcx_to_dm(g.x,g.y,p)
  end else

  if not dm_t_init then

    p:=Point(0,0)

  else begin
    t.x:=x; t.y:=y;

    if dm_t_mode = 0 then
      p:=iProjective_3x3(t.x,t.y,dm_bt)
    else
    if dm_t_mode = 254 then begin
      t.x:=t.x - xyz_c.x;
      t.y:=xyz_c.y - t.y;

      u.x:=xyz_bt[0,0]*t.x+xyz_bt[0,1]*t.y+xyz_bt[0,2];
      u.y:=xyz_bt[1,0]*t.x+xyz_bt[1,1]*t.y+xyz_bt[1,2];
      u.z:=xyz_bt[2,0]*t.x+xyz_bt[2,1]*t.y+xyz_bt[2,2];

      f:=-xyz_bt[2,3] / u.z;
      t.x:=u.x*f + xyz_bt[0,3];
      t.y:=-u.y*f + xyz_bt[1,3];

      p:=iProjective_3x3(t.x,t.y,dm_bt);
    end else
    if dm_t_mode = 255 then begin
      fDrawLink.xy_to_pix(t,t);
      p.X:=Round(t.x); p.Y:=Round(t.y);
    end
    else begin
      g:=Projective_3x3(t.x,t.y,lg_bt);
      p:=AB_Backup(g.x,g.y, lg_p,dm_lg_t)
    end
  end
end;

procedure TDrawCard.ProjectLink(const g: TGauss; out p: TPoint);
var
  t: TGauss; q: TPoint;
begin
  if isPcx then begin
    Link_T.xy_to_pix(g,t);
    p:=img_Project(t.x,t.y)
  end
  else begin
    q:=xy_lnk_dm(g);
    Project(q.X,q.Y,p)
  end
end;

function TDrawCard.Backup_ext_Centre(out g: xgeoid): Double;
var
  p: TPoint;
begin
  Result:=0;
  Fillchar(g,Sizeof(g),0);

  if dm_t_init then begin
    Backup_Centre_Card(p);
    Globe.Map.LG_t.x_L_to_R(p,g);
    Result:=Get_kz_mpp
  end
end;

function TDrawCard.xBackup_ext_Centre(out g: xgeoid): Double;
var
  c,p: TPoint;
begin
  Result:=0;
  Fillchar(g,Sizeof(g),0);

  if dm_t_init then begin

    Backup_Centre_Card(p);
    if Card_Position(8,c) then
    Backup(c.X,c.Y,p);

    Globe.Map.LG_t.x_L_to_R(p,g);
    Result:=Get_kz_mpp
  end
end;

function TDrawCard.Project_lp(lp: PLLine): integer;
var
  i: integer; p1,p2, d1,d2: TPoint;
begin
  Result:=-1;

  with lp^ do if N >= 0 then begin
    with Pol[0] do Project(x,y,p1);
    Result:=0; Pol[0]:=p1; d1.x:=0; d1.y:=0;

    for i:=1 to N do begin
      with Pol[i] do Project(x,y,p2);
      d2.x:=p2.x-p1.x; d2.y:=p2.y-p1.y;

      if (d2.x <> 0) or (d2.y <> 0) then begin
        if (d1.x <> d2.x) or (d1.y <> d2.y) then
        Inc(Result); Pol[Result]:=p2; p1:=p2; d1:=d2
      end
    end;

    N:=Result
  end
end;

function TDrawCard.mf_Project(lp: PLLine; hp: PIntegers;
                              out lp_lt,lp_rb: TPoint;
                              pack,mark: Boolean): integer;
var
  p1,p2, lt,rb, p: TPoint;
  i,dx1,dy1,dx2,dy2: int; v: VPoint;
begin
  Result:=-1;

  with lp^ do if N >= 0 then begin

    p1:=Pol[0]; lt:=p1; rb:=p1;

    if dm_t_xyz then begin
      Globe.xyz_to_scb(p1.x,p1.y,hp[0], p);
      p1:=img_Project(p.X,p.Y)
    end
    else Project(p1.X,p1.Y, p1); Pol[0]:=p1;

    Result:=0; dx1:=0; dy1:=0;

    for i:=1 to N do begin p2:=Pol[i];

      if p2.X < lt.X then lt.X:=p2.X;
      if p2.X > rb.X then rb.X:=p2.X;
      if p2.Y < lt.Y then lt.Y:=p2.Y;
      if p2.Y > rb.Y then rb.Y:=p2.Y;

      if dm_t_xyz then begin
        Globe.xyz_to_scb(p2.X,p2.Y,hp[i], p);
        inherited Project(p.X,p.Y, p2)
      end
      else Project(p2.X,p2.Y, p2);

      if pack then begin
        dx2:=p2.X-p1.X; dy2:=p2.Y-p1.Y;
        if (dx2 <> 0) or (dy2 <> 0) then begin

          if mark then Inc(Result) else
          if (dx1 <> dx2) or (dy1 <> dy2) then
          Inc(Result); Pol[Result]:=p2;

          p1:=p2; dx1:=dx2; dy1:=dy2
        end;

        if isPcx and (i >= 512) then
        if not isDraw then Break
      end
      else begin
        Inc(Result); Pol[Result]:=p2;
      end
    end;

    lp_lt:=lt; lp_rb:=rb; N:=Result
  end
end;

function TDrawCard.prj_Vertex(X,Y,Z: Integer): TPoint;
var
  p: TPoint;
begin
  if dm_t_xyz then begin
    Globe.xyz_to_scb(X,Y,Z, p);
    inherited Project(X,Y, p)
  end else

  Project(X,Y, p); Result:=p
end;

procedure TDrawCard.BackupCentre(x,y: integer; out c: TPoint);
begin
  if isPcx then
    inherited Backup(x,y,c)
  else
  with Globe do begin
    c.X:=Round((XPos+x) / kz * px_ed);
    c.Y:=Round((YPos+y) / kz * px_ed)
  end
end;

function TDrawCard.BackupCentreMeter(x,y: integer): TGauss;
begin
end;

procedure TDrawCard.Backup_plus(x,y: integer; out p: TPoint);
begin
  if isPcx then
    inherited Backup(x,y,p)
  else begin
    BackupCentre(x,y,p);
    p.x:=p.x and $FFFFFFFE;
    p.y:=p.y and $FFFFFFFE;
  end;
end;

procedure TDrawCard.ProjectCentre(const p: TPoint; out c: TPoint);
begin
  if isPcx then c:=dm_to_bmp(p) else x_Gauss(p,c)
end;

function TDrawCard.GaussToCentre(x,y: Double): TPoint;
var
  p: TPoint;
begin
  Globe.Map.LG_T.Z_to_L(_Gauss(x,y),p);
  ProjectCentre(p,Result)
end;

function TDrawCard.Wheel_centre(x,y,plus: integer;
                                out c: TPoint): Double;
var
  cx,cy: Integer; p: TPoint; k,pk: Double;
begin
  BackupCentre(x,y,c); k:=kz;

  if plus > 0 then k:=kz*(5/4) else
  if plus < 0 then k:=kz/(5/4);

  if IsWheel then begin p:=c;

    pk:=Globe.px_ed / k;
    if k > Small then begin

      cx:=Card.Width div 2;
      cy:=Card.Height div 2;

      c.x:=Round((p.X / pk - x + cx) * pk);
      c.y:=Round((p.Y / pk - y + cy) * pk);
    end
  end;

  Result:=k
end;

procedure TDrawCard.pps_Backup(out g: xgeoid);
var
  p: TPoint; t: TGauss; pcx: longbool;
begin
  p.X:=Card.Width div 2;
  p.Y:=Card.Height div 2;

  pcx:=IsPcx;

  if not pcx and (dm_t_mode = 1) then begin
    t:=Projective_3x3(p.X,p.Y,lg_bt);
    g:=lg_p.g_to_geo(t.x,t.y)
  end else
  if pcx and not IsXyz then begin
    t:=img_Backup(p.X,p.Y);
    Globe.Pcx_link.l_to_g(t.x,t.y,g)
  end
  else begin
    Backup(p.X,p.Y,p);
    lg_p.x_L_to_R(p,g)
  end
end;

procedure TDrawCard.gBackup(x,y: integer; out g: TGauss);
var
  p: TPoint;
begin
  Backup(x,y,p);
  Globe.Map.LG_t.L_to_Z(p,g)
end;

procedure TDrawCard.pBackup(x,y: integer; out g: TGauss);
var
  p: TPoint;
begin
  Backup(x,y,p); lg_p.L_to_G(p,g)
end;

function TDrawCard.pBackup_Centre: TGauss;
begin
  pBackup(Card.Width div 2,Card.Height div 2,Result)
end;

function TDrawCard.Backup_gcentre: TGauss;
var
  p: TPoint;
begin
  Backup_Centre_Card(p);
  Globe.Map.LG_t.L_to_Z(p,Result);
end;

function TDrawCard.Get_gview1(P,G: PGPoly): Boolean;
var
  i: Integer; s: TPoint; L: LOrient;
begin
  Result:=false;

  if Globe.Count > 0 then
  if dm_t_init then begin

    s:=fDrawPort;
    if Assigned(Card) then
    s:=Point(Card.Width,Card.Height);

    Port_to_LPoly(0,0,s.X,s.Y,@L);

    for i:=0 to 4 do with L[i] do begin
      if Assigned(P) then
      P[i]:=_Gauss(X,Y);
      gBackup(X,Y,G[i]);
    end;

    Result:=true
  end
end;

function TDrawCard.Get_gview(G: PGPoly): Boolean;
begin
  Result:=Get_gview1(nil,G)
end;

function TDrawCard.Get_xview(G: PGPoly): Integer;
var
  i: Integer; L: LOrient; p: TPoint; r: TGeoid;
begin
  Result:=-1;

  if Globe.Count > 0 then
  if dm_t_init then

  with Globe.Map.LG_T do begin

    p:=fDrawPort;
    if Assigned(Card) then
    p:=Point(Card.Width,Card.Height);

    Port_to_LPoly(0,0,p.X,p.Y,@L);

    for i:=0 to 4 do begin
      p:=L[i]; Backup(p.X,p.Y,p);
      L_to_R(p,r); G[i].x:=r.b; G[i].y:=r.l
    end;

    Result:=sys.pps
  end
end;

function TDrawCard.Backup_ed(k: double): double;
var
  a,b: TPoint;
begin
  Backup(0,0,a); Backup(Card.Width-1,Card.Height-1,b);
  Result:=Long_Dist(a,b)/Hypot(Card.Width,Card.Height)*k
end;

procedure TDrawCard.xy_to_sm(const g: tgauss; out l: TPoint);
var
  p: TGauss;
begin
  p:=Projective_3x3(g.x,g.y, prj_tr);
  l.X:=Round(p.x * Globe.px_ed);
  l.Y:=Round(p.y * Globe.px_ed)
end;

procedure TDrawCard.sm_to_xy(const l: TPoint; out g: tgauss);
begin
  with Globe do
  g:=Projective_3x3(l.x/px_ed,l.y/px_ed, prj_bt);
end;

procedure TDrawCard.x_Gauss(const l: TPoint; out p: TPoint);
var
  g: tgauss;
begin
  lg_p.L_to_G(l,g); xy_to_sm(g,p)
end;

procedure TDrawCard.x_Local(const p: TPoint; out l: TPoint);
var
  g: tgauss;
begin
  sm_to_xy(p,g); lg_p.G_to_L(g,l)
end;

procedure TDrawCard.p_Gauss(const l: TPoint; out p: TGauss);
var
  g: TGauss;
begin
  lg_p.L_to_G(l,g);
  p:=Projective_3x3(g.x,g.y, prj_tr);
end;

procedure TDrawCard.p_Local(const p: TGauss; out l: TPoint);
var
  g: TGauss;
begin
  g:=Projective_3x3(p.x,p.y, prj_bt);
  lg_p.G_to_L(g,l)
end;

procedure TDrawCard.Align_prj_page(lp: PLLine);
var
  i: Integer; l: GOrient; a,b: TGauss;
begin
  if not IsPcx then
  with lp^ do if N = 4 then begin

    for i:=0 to 3 do p_Gauss(Pol[i],l[i]);

    a:=l[0];
    b.x:=a.x + Gauss_dist(a,l[1]);
    b.y:=a.y + Gauss_dist(a,l[3]);

    l[1]:=_Gauss(b.x,a.y);
    l[2]:=_Gauss(b.x,b.y);
    l[3]:=_Gauss(a.x,b.y);

    for i:=1 to 3 do p_Local(l[i],Pol[i]);

    Pol[4]:=Pol[0]
  end
end;

function TDrawCard.Get_gauss_step(cx,cy: Integer): double;
var
  g1,g2: TGauss;
begin
  gBackup(cx,cy,g1);
  gBackup(cx+8,cy+8,g2);
  Result:=Gauss_Dist(g1,g2) / Hypot(8,8)
end;

function TDrawCard.dc_gauss_step(dc: XCanvas): double;
begin
  Result:=Get_gauss_step(dc.Width div 2,dc.Height div 2);
end;

function TDrawCard.dc_Resolution(dc: XCanvas): double;
var
  w,h: Integer; g1,g2: TGauss;
begin
  Result:=1;

  if isPcx or Globe.is_mm then begin

    w:=100; h:=100;
    if Assigned(dc) then begin
      w:=dc.Width; h:=dc.Height;
    end;

    gBackup(0,0,g1); gBackup(w,h,g2);
    Result:=Gauss_Dist(g1,g2)/Hypot(w,h)
  end

  else
  if kz > 0 then Result:=1/kz
end;

function TDrawCard.kz_Scale(dc: XCanvas; kz: Double): double;
var
  ax: Double;
begin
  Result:=0;

  if dc = nil then ax:=xMetersPerPixel(0)
              else ax:=dc.MetersPerPixel;

  ax:=ax*kz;

  if ax > 0 then Result:=1/ax
end;

function TDrawCard.dc_Scale(dc: XCanvas): double;
var
  mpp: Double;
begin
  Result:=0;

  if dc = nil then mpp:=xMetersPerPixel(0)
              else mpp:=dc.MetersPerPixel;

  if mpp > 0 then Result:=dc_Resolution(dc)/mpp
end;

procedure TDrawCard.Cash_Paint(dib: TDIB; PaintR,ClipR: PRect);
var
  ox,oy: Integer;
begin
  isCash:=true; ox:=xPos; oy:=yPos;
  xPos:=dib.v_lt.x; yPos:=dib.v_lt.y;
  Draw_Maps(dib,ClipR); xPos:=ox; yPos:=oy;
  isCash:=false; fcashChanged:=true
end;

procedure TDrawCard.Draw_Event(Sender: TObject);
begin
end;

procedure TDrawCard.Card_Refresh(Sender: TObject);
var
  ox,oy: Integer;
begin
  if not isInvalidate then

  if Cash.Active then
  if Cash1_Active then begin

    if Assigned(OnDispRight1) then
    OnDispRight1(Self);

    IsInvalidate:=true;

    if IsCapture
    or not IsAnimate then
    Cursor_BeginPaint;

    if Assigned(fOnTrack) then begin
      ox:=xPos; xPos:=Cash.v_lt.X;
      oy:=yPos; yPos:=Cash.v_lt.Y;
      Setup_Map_Display(Cash,nil,Globe.ActiveMap);
      fOnTrack(Cash); xPos:=ox; yPos:=oy
    end;

    if Cash.xCopy(Cash1) then begin

      fctx.beginPaint(Globe);
      Cash1.BeginPaint(nil);

      Owner_Draw_End(Cash1);

      Cash1.EndPaint;
      fctx.endPaint;

      if Assigned(fOnRefresh) then
      fOnRefresh(Cash1);

      xdc_to_Card(Cash1);

      if not Assigned(fOnRefresh) then
      if Assigned(OnDraw) then OnDraw(Self)
      else Draw_Event(nil);

      Cursor_AfterPaint
    end;

    IsInvalidate:=false;

    if Right <> nil then
    (Right as TDrawCard).Card_Refresh(nil);

    if Assigned(OnDispRight2) then
    OnDispRight2(Self);

    Globe.Map.Close_Map;
  end;

  IsRefresh:=false
end;

procedure TDrawCard.nav_draw(dc: XCanvas);
var
  i: int;
begin
  if Assigned(dc) then begin
    if Cash2.Realloc_rgb(dc.Width,dc.Height) then
    Cash2.xRepeat(dc);

    for i:=0 to fnavList.Count-1 do
    TNavCustom(fnavList[i]).Draw(dc);
  end else
  if Cash2.Active then begin
    Cash1.xRepeat(Cash2);
    for i:=0 to fnavList.Count-1 do
    TNavCustom(fnavList[i]).Draw(Cash1);
    xdc_to_Card(Cash1);
  end
end;

procedure TDrawCard.Card_Reground(Sender: TObject);
var
  ox,oy: int; map,map3: Bitmap;
begin
  if not isInvalidate then
  if Assigned(fOnBackground) then

  if Cash.Active then
  if Cash3.Realloc_rgb(Cash.Width,Cash.Height) then begin

    Cash3.xRepeat(Cash);
    IsInvalidate:=true;

    if IsCapture
    or not IsAnimate then
    Cursor_BeginPaint;

    ox:=xPos; xPos:=Cash.v_lt.X;
    oy:=yPos; yPos:=Cash.v_lt.Y;
    Setup_Map_Display(Cash3,nil,Globe.ActiveMap);

    Cash3.rgb_Brush(fctx.rgb_Ground_Color);
    Cash3.FillRect(nil);
    fOnBackground(Cash3);
    xPos:=ox; yPos:=oy;

    if Cash.GetMap(map) then
    if Cash3.GetMap(map3) then
    mixBitmap(map3,map,fctx.rgb_Ground_Color);

    if Cash.xCopy(Cash1) then begin
      fctx.beginPaint(Globe);
      Cash1.BeginPaint(nil);

      Owner_Draw_End(Cash1);

      Cash1.EndPaint;
      fctx.endPaint;

      if Assigned(fOnRefresh) then
      fOnRefresh(Cash1);

      xdc_to_Card(Cash1);

      if not Assigned(fOnRefresh) then
      if Assigned(OnDraw) then OnDraw(Self)
      else Draw_Event(nil);

      Cursor_AfterPaint;
    end;

    IsInvalidate:=false;

    if Right <> nil then
    (Right as TDrawCard).Card_Refresh(nil);

    if Assigned(OnDispRight2) then
    OnDispRight2(Self);

    Globe.Map.Close_Map;
  end;

  IsRefresh:=false
end;

procedure TDrawCard.Owner_Draw(dc: XCanvas);
var
  mpp,sc: Double;
begin
  is_pcx_link:=isPcx;

  if Assigned(dc) then begin

    fctx.beginPaint(Globe);
    fctx.x_lock:=true;

    if not Cash.Active then
    isTrackCard:=false;

    fGround:=Globe.dm_Ground;
    fGrid:=Globe.cl_Grid;

    Owner_Draw_Begin(dc);

    if isPcx then begin

      if isXyz then
      Globe.Stereo_delta_plan(nil);

      if IsLibrary then
      Globe.ImageMirror:=true;

      inherited Owner_Draw(dc);
    end
    else begin
      dc.Ground:=fGround;

      if Cash.Active then begin

        Cash.Ground:=fGround;
        Cash.pix_src:=1;
        Cash.pix_dst:=1;

        if not isTrackCard then begin

          Cash.OnBeginPaint:=BeforePaint;
          Cash.OnEndPaint:=AfterPaint;

          Cash.xDraw(xPos,Card.Width,xMaxValue,
                     yPos,Card.Height,yMaxValue,
                     Cash_Paint);

          Cash.OnBeginPaint:=nil;
          Cash.OnEndPaint:=nil;
        end;

        Cash.xMove(XPos,YPos);
        Setup_Edit_Map_Display;

        Cash.xCopy(dc)
      end
    end;

    if not isTrackCard then begin

      if Assigned(fOnTrack) then
      fOnTrack(dc);

      dc.BeginPaint(nil);
      Owner_Draw_End(dc);
      dc.EndPaint;

      if Assigned(fOnPosition) then
      fOnPosition(Self);

      if Assigned(fOnRulePaint) then
      fOnRulePaint(nil);

      Globe.Map.Close_Map;
      done_Signs;

      if Assigned(fOnRefresh) then
      fOnRefresh(Cash1) else
      if Assigned(OnDraw) then OnDraw(Self) else
      Draw_Event(nil);

      if fis_Zoom then
      if Assigned(fOnZoom) then begin

        if IsPcx then
          sc:=dc_Scale(dc)
        else begin
          sc:=0; mpp:=dc.MetersPerPixel;
          if mpp > 0 then begin sc:=1;
            if kz > 0 then sc:=1/kz/mpp
          end
        end;

        fOnZoom(sc);
      end;

      fis_Zoom:=false
    end;

    fctx.endPaint;
  end
end;

procedure TDrawCard.Cash_Draw(Sender: TDIB; PaintR,ClipR: PRect);

procedure zoomDraw(xdc: XCanvas);
var
  dm: tdm_map;
  i,p: int; ip: PIntegers;
begin
  dm:=Globe.Map;
  if Globe.Count > 0 then

  if Assigned(Pickup) then
  if Pickup.x_fe.Count > 0 then

  if Setup_Map_Display(xdc,nil,Globe.ActiveMap) then begin

    ip:=Pickup.x_fe.First;
    for i:=0 to Pickup.x_fe.Count-1 do begin
      p:=ip[i];
      if dm.Tag_Object(p) in [1,4] then
      Draw_Object(xdc,p,0,-1)
    end;

    Setup_Edit_Map_Display
  end
end;

var
  ox,oy: Integer;
begin
  if not isTrackCard then
  if Sender.Active then begin
    BeforePaint(Sender,ClipR);
    fGround:=Globe.dm_Ground;

    Sender.Ground:=fGround;
    ox:=xPos; xPos:=Sender.v_lt.X;
    oy:=yPos; yPos:=Sender.v_lt.Y;
    isCash:=true; fcashChanged:=true;

    if isZoomDraw then
      zoomDraw(Sender)
    else
      Draw_Maps(Sender,ClipR);

    xPos:=ox; yPos:=oy; isCash:=false;

    AfterPaint(Sender)
  end
end;

function TDrawCard.Pickup_Enabled: Boolean;
begin
  Result:=false;
  if Assigned(Pickup) then
  Result:=Pickup.IsDisplay
end;

procedure TDrawCard.Pickup_fe(Ptr,Code,Loc: int; const lt,rb: TPoint);
begin
  if Assigned(Pickup) then
  if Pickup.x_Display_Enabled then
  Pickup.x_fe.oAdd(Ptr,Code,Loc,lt,rb)
end;

procedure TDrawCard.Pickup_list(list: TInt64List);
var
  i: int; p: TInt64;
begin
  if Assigned(Pickup) then
  for i:=0 to list.Count-1 do begin
    p.x:=list[i]; Pickup.x_Display(p.cn,p.id)
  end
end;

procedure TDrawCard.Pickup_childs(p: Longint);
var
  list: TInt64List;
begin
  if Assigned(Pickup) then begin

    Pickup.IsUpdated:=true;

    list:=TInt64List.Create;
    try
      if Globe.Map.Get_childs(p,nil,list) > 0 then
      Pickup_list(list);
    finally
      list.Free
    end;

    Pickup.IsUpdated:=false
  end
end;

procedure TDrawCard.Mark_Occupe_Point(xdc: XCanvas; Ptr: Int64);
var
  c: VPoint; p: TPoint;
begin
  if Left = nil then
  if Globe.Map.Get_Occupe_Point(Ptr,c) then begin

    p:=xProject(c.x,c.y,c.z);
    if xdc.Contains_point(p.X,p.Y, 8,8) then
    xdc.MarkPoint(p.X,p.Y,8,0);
  end
end;

procedure TDrawCard.Owner_Disp_Object(xdc: XCanvas; Ptr: Int64; mode: int);
var
  lp: PLLine; i,x2,y2: int;
begin
  if Ptr > 0 then begin
    fdc_Mode:=mode;
    fctx.setMode(Globe,mode);
    fmarkColor:=fctx.Get_Mark_Color;

    fx_Mark:=true;
    isDraw:=Ptr <> Globe.Map.Tree.Root;
    Stereo_Object(xdc,Ptr);

    fdc_Mode:=x_DRAW;
    fctx.SetMode(Globe,x_DRAW);
    fx_Mark:=fx_Disp.Mark;
    isDraw:=false;

    if Left = nil then begin

      lp:=x_Edit.DPolyBuf;
      if Globe.Map.Get_Occupe_Range(Ptr,lp) then begin

        xdc.rgb_Pen(3,clFuchsia);
        Disp_Polyline(xdc,lp);

        xdc.rgb_Pen(1,clBlack);
        xdc.rgb_Brush(clFuchsia);

        x2:=xdc.Width+8; y2:=xdc.Height+8;

        for i:=1 to lp.N-2 do
        with lp.Pol[i] do
        if (X >= -8) and (X <= x2) and
           (Y >= -8) and (Y <= y2) then
        xdc.Rectangle(X-2,Y-2,X+3,Y+3);

        with lp.Pol[0] do xdc.RectPoint(X,Y,4);
        with lp.Pol[lp.N] do xdc.RectPoint(X,Y,4)
      end;

      Mark_Occupe_Point(xdc,Ptr)
    end
  end
end;

procedure TDrawCard.Owner_Mark_Object(xdc: XCanvas; Ptr: Int64);
begin
  Owner_Disp_Object(xdc,Ptr,x_Mark);
end;

procedure TDrawCard.Owner_Draw_Begin(dc: XCanvas);
begin
  sau_Hide;
end;

procedure TDrawCard.Owner_Draw_End(dc: XCanvas);
begin
end;

function TDrawCard.rule_close: int;
begin
  Result:=-1;
  if Assigned(fOnRuleClose) then
  Result:=fOnRuleClose(x_Edit.LPolyBuf)
end;

function TDrawCard.Setup_Map_Display(dc: XCanvas; p: PRect;
                                     dm_i: Integer): Boolean;

function Orient_dm_t(dc: XCanvas; dm_i: Integer): Boolean;

procedure prj_dm_LOrient(out L: LOrient);
var
  i: Integer; cx,cy: double;
begin
  Globe.Map.Get_dm_LOrient(@L);

  xCentre_Polygon(@L,3,cx,cy);
  L[4]:=_LGauss(cx,cy);

  for i:=0 to 4 do with L[i] do
  Project(X,Y,L[i]);
end;

var
  i,j,w,h: Integer;
  wt,wt_,tr1,tr2: Real3x3; dw: Double;
  L1,L2: LOrient; G1,G2: GOrient;
  lg: lg_Transit; p: TPoint;
begin
  Result:=false;

  if dm_i < 0 then Result:=true else

  with Globe do
  if dm_i < Count then begin fMapIndex:=dm_i;

    Get_win_tr(wt,wt_);

    lg:=Map.LG_Tr;

    if p_lg.s.prj > 0 then
    with lg.g[0] do
    sys_Projection(lg.s,x,y);

    East_lg_Transit(lg,p_lg);
    dm_lg_t.Assign(lg,-1);

    Result:=Get_dm_lg_t(dm_i,lg_p);

    lg_p.x_ab.Get_Matrix(tr1);
    lg_p.x_ba.Get_Matrix(tr2);

    dm_t:=tr1;
    Forw_3x3(dm_t,prj_tr);

    lg_t:=prj_tr;

    if xyz_focus >= 1 then begin

      xyz_p:=Real3x3_Matrix(dm_t);
      Matrix_forw(xyz_p,xyz_t);

      xyz_c.x:=Card.Width/2;
      xyz_c.y:=Card.Height/2;

      if dc is TDib then begin
        (dc as TDib).Backup(0,0,p.X,p.Y);
        xyz_c.x:=xyz_c.x + p.X;
        xyz_c.y:=xyz_c.y + p.Y;
      end;

      lg_bt:=prj_bt; dm_t_mode:=254;
    end
    else begin
      Forw_3x3(dm_t,wt);
      Forw_3x3(lg_t,wt);

      lg_bt:=wt_;
      Forw_3x3(lg_bt,prj_bt);

      dm_t_mode:=0;
    end;

    dm_bt:=lg_bt;
    Forw_3x3(dm_bt,tr2);

    if Assigned(fOnRotate) then fOnRotate(dm_t);

    if dm_t_mode = 0 then
    if lg_p.sys.prj > 0 then begin

      if not Assigned(fOnRotate) then
      if not isPcx and not is_mm then begin

        if not sys_Equal(lg_p.sys,dm_lg_t.sys) then begin
          dm_t_mode:=0; prj_dm_LOrient(L1);
          dm_t_mode:=2; prj_dm_LOrient(L2);

          dm_t_mode:=0; for i:=0 to 4 do
          if Long_Dist(L1[i],L2[i]) > 1 then begin

            dm_t_mode:=1;

            Globe.Map.Get_dm_LOrient(@L1);
            prj_dm_LOrient(L2);

            for j:=0 to 3 do begin
              with L1[j] do G1[j]:=_Gauss(x,y);
              with L2[j] do G2[j]:=_Gauss(x,y);
            end;

            tr1:=dm_t; tr2:=dm_bt;

            if Solve_projective1(@G1,@G2,dm_t,dm_bt) then begin
              dm_t_mode:=0; Project(L1[4].X,L1[4].Y,p);
              if Long_Dist(p,L2[4]) > 1 then dm_t_mode:=1
            end;

            if dm_t_mode = 1 then begin
              dm_t:=tr1; dm_bt:=tr2;
            end;

            Break
          end;
        end
      end
    end;

    if isPcx then Set_map_sm_T(dm_i)
  end;

  dm_t_xyz:=false;
  if Assigned(x_Edit.DHeights) then
  dm_t_xyz:=fIs_xyz;

  dm_t_init:=Result
end;

procedure Pattern_p(var p: TPoint;
                    dm_i: Integer;
                    const c: TPoint);
begin
  if dm_i < 0 then begin
    p.x:=p.x-fPattern_c.x+c.x;
    p.y:=p.y-fPattern_c.y+c.y;
  end
end;

procedure Pattern_ab(var a,b: TPoint;
                    dm_i: Integer;
                    const c: TPoint);
begin
  if dm_t_mode <> 255 then
  if dm_i < 0 then begin
    Pattern_p(a,dm_i,c);
    Pattern_p(b,dm_i,c);
  end
end;

procedure conus_lt_rb(const R: TRect; var lt,rb: TPoint);
var
  L: LOrient; i: Integer; p: TPoint;
begin                     
  Globe.Map.Get_dm_LOrient(@L);
  for i:=0 to 3 do begin
    with L[i] do Project(x,y,p);

    if RectContainsPoint(R,p) then
    Max_lPort(lt,rb, L[i]);
  end
end;

function Get_cd_kz(dc: XCanvas): double;
var
  w,h: Integer; g1,g2: TGauss;
begin
  Result:=kz;

  if isPcx or Globe.is_mm then begin
    w:=dc.Width; h:=dc.Height;
    gBackup(0,0,g1); gBackup(w,h,g2);
    Result:=Gauss_Dist(g1,g2)/Hypot(w,h)
  end
end;

var
  dm: tdm_map;
  i,n,d, x1,y1,x2,y2, sc, v: int;
  lp: PIntegers; a,b,dm_c: TPoint;
  k,res: double; R: TRect; fn: TShortStr;
begin
  if dc <> nil then begin

    dm:=Globe.Map;

    dm_c:=Point(0,0); if dm_i < 0 then
    Middle_Point(dm.dm_lt,dm.dm_rb,dm_c);

    if Orient_dm_t(dc,dm_i) then begin

      if dm.Enabled_Map then begin

        dm.Objects_Path(fn,'.obj');

        if dm.Open_Objects then begin
          fScaleList.LoadFrom(dm.Obj,fn);
          obj_Load_Fonts(dm.Obj);
          dm.Obj.vm_Close;
        end
        else fScaleList.Clear;

        fDispAttrs.LoadList(Globe.DispAttrs);

        if Strlen(fn) > 0 then begin
          Pgm.ext_Open(fn,'.pgm');
          Vgm.ext_Open(fn,'.vgm');

          StrUpdateExt(fn,fn,'.pen');
          Ext_Pens.LoadFrom(fn);

          StrUpdateExt(fn,fn,'.pat');
          dib_Patterns_LoadFrom(fn);

          if fDispAttrs.Count = 0 then begin
            StrUpdateExt(fn,fn,'.atr');
            if FileExist(fn) then
            fDispAttrs.LoadFrom(fn)
          end
        end;

        fblankPen:=hgl_pack_color(0,1,RGB(255,255,255));
        fblankBru:=hgp_pack_color(0,128,RGB(255,255,255));

        if dm.xGet_Int(0,899,v) then fblankPen:=v;
        if dm.xGet_Int(0,898,v) then fblankBru:=v;

        dm_View:=255; res:=dc_Resolution(dc);

        if lg_p.sys.pps = 1 then
        if lg_p.sys.prj = prj_deg then
        if not isPcx and not Globe.is_mm then
        res:=res / lg_p.prj_res;

        mm_k:=0; if res > Small then
        mm_k:=dm.dm_Scale / (res*1000);

        dc.mm_k:=mm_k;

        res:=dc.MetersPerPixel;
        cd_dpmm:=dc.Get_Scale(1)/1000;

        if res > Small then
        Fonts.MinH:=Min(16,0.0015 / res);

        Fonts.xBlankColor:=(fGround shl 5) or
                            fGround;

        cd_scale:=Round(dc_Scale(dc));
        if cd_scale <= 0 then cd_Scale:=1;

        cd_View:=0; sc:=dm.dm_scale;

        n:=Get_scale_list(sc,lp);
        if n = 0 then
        for i:=1 to 255 do begin
          if cd_scale > sc div i then Break;
          cd_View:=i
        end else
        for i:=0 to n-1 do begin
          if cd_scale > lp[i] then Break;
          Inc(cd_View)
        end;

        if Preview <> nil then cd_View:=Preview.cd_View
        else if ocx_View > 0 then cd_View:=ocx_View;

        if not fx_Disp.Objs then dm_View:=cd_View
      end;

      R:=dc.ClipRect; if p <> nil then R:=p^;
      BackupPort(R.Left,R.Top,R.Right,R.Bottom,v_lt,v_rb);
      conus_lt_rb(R, v_lt,v_rb);

      Pattern_ab(v_lt,v_rb, dm_i,dm_c);

      Backup(0,0,a); Backup(32,32,b);
      Pattern_ab(a,b, dm_i,dm_c);
      d:=Round(Long_Dist(a,b));

      s_lt.X:=xInc(v_lt.X,-d); s_lt.Y:=xInc(v_lt.Y,-d);
      s_rb.X:=xInc(v_rb.X,+d); s_rb.Y:=xInc(v_rb.Y,+d);

      Backup(0,0,a); Backup(1024,1024,b);
      d:=Round(Long_Dist(a,b));

      t_lt.X:=xInc(v_lt.X,-d); t_lt.Y:=xInc(v_lt.Y,-d);
      t_rb.X:=xInc(v_rb.X,+d); t_rb.Y:=xInc(v_rb.Y,+d);
                                
      Backup(0,0,a); Backup(100,0,b);
      Pattern_ab(a,b, dm_i,dm_c);

      fpoly_min:=Round( Long_Dist(a,b)/100 * 4 );
      fpoly_min1:=fpoly_min * 3;

      fsign_min:=Long_Dist(a,b)/100 * 8;

      if dc.lp_Clip then begin
        win_lt.X:=-64; win_rb.X:=dc.Width+64;
        win_lt.Y:=-64; win_rb.Y:=dc.Height+64;

        Backup_dc(dc,64, map_lt,map_rb);
        Pattern_ab(map_lt,map_rb, dm_i,dm_c);
      end
      else begin
        with Globe do
        lg_p.Get_Local_Bound(px_lt,px_rb,
                             map_lt,map_rb);

        Pattern_ab(map_lt,map_rb, dm_i,dm_c);

        Project(map_lt.X,map_lt.Y,a);
        Project(map_rb.X,map_rb.Y,b);
        Swap_lRect(a,b,win_lt,win_rb)
      end;

      cd_kz:=Get_cd_kz(dc)
    end;

    dc.SetBkMode_TRANSPARENT
  end;

  Result:=dm_t_init
end;

function TDrawCard.Prepare_Map_draw: Boolean;
begin
  Result:=Globe.Map.Enabled_Map and
          Setup_Edit_Map_Display
end;

function TDrawCard.Setup_Edit_Map_Display: Boolean;
begin
  Result:=Setup_Map_Display(Cash1,nil,Globe.ActiveMap)
end;

procedure TDrawCard.BeforePaint(Sender: TObject; PaintR: PRect);
var
  xdc: XCanvas;
begin
  fctx.beginPaint(Globe);

  Fonts.Mask:=nil;
  Fonts.xBlankTest:=0;

  if Sender is XCanvas then begin
    xdc:=Sender as XCanvas;
    xdc.Thin:=fx_Disp.Thin;

    if fx_Disp.TextMask then
    if fTextMask.Alloc(xdc.Width,xdc.Height) then begin
      Fonts.Mask:=fTextMask; Fonts.xBlankTest:=1;
    end
  end;

  if (Left <> nil)
  or (Sender = Stereo_b) then
  Globe.StereoLeft(false)
end;

procedure TDrawCard.AfterPaint(Sender: TObject);
begin
  fctx.endPaint; Globe.StereoLeft(true)
end;

procedure TDrawCard.Page_PolyBuf(dc: XCanvas; lp: PLLine);
var
  i,cl: Integer;
begin
  cl:=$0500 or fGround;
  if Project_lp(lp) > 0 then

  for i:=1 to 2 do begin
    clip_master.plot_begin(dc,2,cl,cl,0);
    clip_master.plot_Polyline(@lp.Pol,lp.N);
    cl:=$020D
  end
end;

procedure TDrawCard.Draw_PolyBuf(dc: XCanvas; lp: PLLine; loc,cl: int);
var
  i: int;
begin
  with lp^ do for i:=0 to N do
  with Pol[i] do Project(x,y,Pol[i]);
  Clip_PolyBuf(dc,lp,loc,cl)
end;

function TDrawCard.Clip_PolyBuf(dc: XCanvas; lp: PLLine; loc,cl: Integer): Boolean;
var
  p: LVector;
begin
  Result:=false;
  if lp.N > 0 then begin
    Max_Poly_Bound(@lp.Pol,lp.N+1,p[0],p[1]);

    if dc.Contains_port(@p,8,8) then begin
      clip_master.plot_begin(dc,loc,cl,cl,mm_k);
      clip_master.plot_Polyline(@lp.Pol,lp.N);
      Result:=true
    end
  end
end;

function TDrawCard.Draw_frame(dc: XCanvas; lp: PLLine; loc,cl: Integer): Boolean;
var
  r: Integer; thin: Boolean;
begin
  thin:=fx_Disp.Thin;
  fx_Disp.Thin:=false;
  Result:=Clip_PolyBuf(dc,lp,loc,cl);
  fx_Disp.Thin:=thin
end;

procedure TDrawCard.Draw_frame1(dc: XCanvas; lp: PLLine; cl: Integer);
var
  i,r: Integer; thin: Boolean;
begin
  thin:=fx_Disp.Thin;
  fx_Disp.Thin:=false;
  Clip_PolyBuf(dc,lp,2,cl);

  r:=((cl div 256) and 7) div 2;
  if r > 0 then begin

    cl:=cl and 31;
    dc.xBrush(0,cl);
    dc.xPen(1,cl);

    for i:=0 to lp.N-1 do with lp.Pol[i] do
    if dc.Contains_point(X,Y,8,8) then
    dc.Rectangle(X-r-1,Y-r-1,X+r,Y+r);
  end;

  fx_Disp.Thin:=thin
end;

procedure TDrawCard.card_lpoly(lp: PLPoly; n,loc,cl: int);
var
  i: int; xdc: XCanvas;
begin
  for i:=0 to n do
  with lp[i] do Project(X,Y,lp[i]);

  if begin_cash1(xdc) then begin
    xdc.BeginPaint(nil); xdc.Fill:=true;
    Out_lpoly(xdc,lp,n,loc,cl);
    xdc.EndPaint
  end
end;

procedure TDrawCard.draw_list(dc: XCanvas; mf: TPolyList; loc,cl: int);
var
  i,n: int; lp: PLPoly; hp: PIntegers;
begin
  lp:=mf.First;
  for i:=0 to mf.Count-1 do
  with lp[i] do Project(X,Y,lp[i]);

  for i:=0 to mf.PartCount-1 do begin
    n:=mf.seek_poly(i,lp,hp)-1;
    Out_lpoly(dc,lp,n,loc,cl)
  end;
end;

procedure TDrawCard.Out_lline(dc: XCanvas; lp: PLLine; loc,cl: int);
begin
  Out_lpoly(dc,@lp.Pol,lp.N,loc,cl)
end;

procedure TDrawCard.Out_lpoly(dc: XCanvas; lp: PLPoly; n,loc,cl: int);
var
  p: LVector;
begin
  Max_Poly_Bound(lp,n+1, p[0],p[1]);

  if (p[0].X < win_lt.X) or (p[0].Y < win_lt.Y) or
     (p[1].X > win_rb.X) or (p[1].Y > win_rb.Y) then begin

    if dc.Contains_port(@p,8,8) then begin
      clip_master.plot_begin(dc,loc,cl,cl,mm_k);
      clip_master.plot_Polyline(lp,n)
    end

  end else
  if loc = 2 then
    fctx.DrawPoly(dc,lp,n,cl,mm_k)
  else
    fctx.FillPoly(dc,lp,nil,n,0,cl)
end;
      
procedure TDrawCard.Out_LPolyLine(dc: XCanvas; lp: PLPoly; n,cl: int);
var
  lt,rb: TPoint;
begin
  if n > 0 then begin
    Max_Poly_Bound(lp,n+1, lt,rb);

    if (lt.x < win_lt.x) or (lt.y < win_lt.y) or
       (rb.x > win_rb.x) or (rb.y > win_rb.y) then begin
      clip_master.plot_begin(dc,2,cl,cl,mm_k);
      clip_master.plot_Polyline(lp,n)
    end
    else begin
      dc.xPen(cl shr 5,cl and 31);
      dc.PolyLine(lp,n)
    end
  end
end;

procedure TDrawCard.Out_PolyLine(dc: XCanvas; lp: PLLine; cl: int);
begin
  Out_LPolyLine(dc,@lp.Pol,lp.N,cl)
end;

procedure TDrawCard.draw_mf(dc: XCanvas; mf: TPolyList; loc,cl: int);
var
  i,ln,cn,dr: int;
  lp: PLPoly; cp: PIntegers;
begin
  lp:=mf.First;
  ln:=mf.Count;
  cp:=mf.Parts.First;
  cn:=mf.PartCount;

  for i:=0 to ln-1 do
  with lp[i] do Project(X,Y,lp[i]);

  dc.Clip:=clip_poly(lp,ln);

  if loc = 2 then begin

    for i:=0 to cn-1 do begin
      ln:=cp[i]-1;

      if dc.Clip then begin
        clip_master.plot_begin(dc,2,cl,0,mm_k);
        clip_master.plot_polyline(lp,ln)
      end
      else fctx.DrawPoly(dc,lp,ln,cl,mm_k);

      if fx_Mark then begin
        dr:=Globe.Params.mark_dr;
        fctx.MarkPoly(dc,lp,ln,dr,false)
      end;

      lp:=@lp[ln+1]
    end

  end else
  if loc = 3 then begin
    dc.Poly:=true;
    dmx_lp_fill(dc,lp,cp,cn,0,cl,Vgm);
    dc.Poly:=false;
  end;

  dc.Clip:=false
end;

procedure TDrawCard.mark_Points(dc: XCanvas; lp: PLPoly; n,r,cl: int);
var
  i: int; p: TPoint;
begin
  if n >= 0 then begin
    dc.xPen(1,0); dc.xBrush(0,cl);

    for i:=0 to n do begin
      p:=lp[i]; Project(p.X,p.Y,p);
      if dc.Contains_point(p.X,p.Y,r,r) then
      dc.Ellipse(p.X-r,p.Y-r,p.X+r,p.Y+r);
    end
  end
end;

function TDrawCard.print_init_mosaic: bool;
begin
  Result:=false;
  if (Globe.Bank2.Count > 0)
  or (Strlen(Globe.Sas) > 0) then
  if fMosaic = nil then begin
    dll_GetMosaicInterface(fMosaic);
    Result:=Assigned(fMosaic)
  end
end;

function TDrawCard.Print_Maps(dc: XCanvas;
                              dc_x1,dc_y1,dc_x2,dc_y2: Integer;
                              px_x1,px_y1,px_x2,px_y2,sc: Double;
                              bmp: Boolean): Double;
var
  px1,py1,px2,py2,kx,ky: double;
  p: PRect; R: TRect; o: TPoint;
  tr: Real3x3; oy: Integer; g: TGauss;
  fl: bool;
begin
  Result:=Globe.px_rb.x;

  R:=Rect(dc_x1,dc_y1,dc_x2,dc_y2);
  p:=@R; Set_prj_Bound(0);

  if dc_x1 > dc_x2 then begin
    dc_x1:=0; dc_x2:=dc.Width;
    dc_y1:=0; dc_y2:=dc.Height;
    p:=nil
  end;

  px1:=Min(px_x1,px_x2); px2:=Max(px_x1,px_x2);
  py1:=Min(px_y1,px_y2); py2:=Max(px_y1,px_y2);

  if sc > 0 then begin
    kz:=dc.Get_Scale(sc); kx:=kz; ky:=kz
  end else
  if px2 > px1 then
  if py2 > py1 then begin
    kx:=(dc_x2-dc_x1)/(py2-py1);
    ky:=(dc_y2-dc_y1)/(px2-px1);
    kz:=kx; sc:=1;

    if not fis_x2 then begin
      kx:=kz; ky:=kz
    end
  end;

  fx2_kz.x:=kx; fx2_kz.y:=ky;

  if sc > 0 then begin

    tr:=prj_tr; xy_Scale_3x3(tr,kx,ky);
    norm_3x3(tr);

    if bmp then begin
      o:=iTransit_3x3(px1,py1,tr);
      yPos:=o.Y-dc_y2
    end
    else begin
      o:=iTransit_3x3(px2,py1,tr);
      yPos:=o.Y-dc_y1
    end;

    xPos:=o.X-dc_x1;

    Vgm.pp_k:=pgm_Scale;

    fl:=print_init_mosaic;

    dc.BeginPaint(p); BeforePaint(dc,p);
    Draw_Maps(dc,nil); dc.EndPaint;
    AfterPaint(dc);

    if fl then fMosaic:=nil;

    Setup_Map_Display(dc,nil,Globe.ActiveMap);

    dm_t_init:=true; Vgm.pp_k:=1;

    Init_3x3(tr, XPos,YPos, 1,1);
    xy_Scale_3x3(tr,1/kx,1/ky);
    Forw_3x3(tr,prj_bt);

    oy:=dc.Height; if bmp then oy:=0;
    Result:=Transit_3x3(0,oy,tr).x;
  end
end;

procedure TDrawCard.Print_page(xdc: XCanvas; px,py,mpp: Double);
var
  k,res: Double; p: TPoint; R: TRect; dc: ICanvas;
begin
  if mpp > Small then begin

    xdc.BeginPaint(nil);
    BeforePaint(xdc,nil);

    if IsPcx then begin
      res:=pcx_mpp; k:=mpp / res;

      z_dst:=image_zoom(k,z_src);
      XPos:=Round(px/res/z_src);
      YPos:=Round(py/res/z_src);

      pcx_Backup(0,0,p);

      R:=Rect(0,0,xdc.Width,xdc.Height);

      dc:=xdc.Canvas;
      if Assigned(dc) then
      xImagePaint(dc,@R,p.X,p.Y,z_src/z_dst,Image,nil);

    end
    else begin
      Set_prj_Bound(fNorth);
      kz:=1/mpp; fx2_kz.x:=kz; fx2_kz.y:=kz;
      XPos:=Round(px*kz); YPos:=Round(py*kz);
    end;

    Draw_Maps(xdc,nil);

    xdc.EndPaint; AfterPaint(xdc);
  end
end;

procedure TDrawCard.Draw_Maps(dc: XCanvas; p: PRect);

procedure Mosaic_draw(dc: XCanvas; Scale: int);

procedure draw_tif(Ind,Scale: Integer);

function GetClipTif(const rec: TMapRec): IPolyList;
var
  dm: tdm_map;
  ptr,top,run: int; lp: PLLine;

  obj: TPolyListIntf; c: TPoint;
  intf: IPolyList; _lp: PLPoly;

  i,ip,v,cx,cy,iw,ih: int; t1,t2: TGauss;
  l,g: GOrient; s1,s2: tsys; tr,bt: Real3x3;
  rad,ppm: double; nm,nm1: TShortstr;
begin
  Result:=nil;

  dm:=Globe.Map;
  lp:=x_Edit.DPolyBuf;

  StrNameExt(nm,rec.Path);
  StrDos(nm);

  ptr:=dm.Nameof(nm);
  if ptr > 0 then
  if dm.Enabled_Map then
  if dm.xGet_poly(ptr,lp,nil,LPolyMax) > 0 then
  if PolyLock(lp) then begin

    l:=rec.lg.l; g:=rec.lg.g; s2:=rec.lg.s;

    if s2.pps = 1 then
    for i:=0 to 3 do with g[i] do
    prj_BL_XY(x,y, s2, x,y);

    if Solve_projective1(@g,@l,tr,bt) then begin

      dm.LG_T.L_to_Z(lp.Pol[0],t1);
      t2:=t1; t2.y:=t2.y+1;

      t1:=Projective_3x3(t1.x,t1.y,tr);
      t2:=Projective_3x3(t2.x,t2.y,tr);

      ppm:=Gauss_Dist(t1,t2);

      obj:=TPolyListIntf.Create;
      try
        // BlendTyp
        if dm.xGet_Int(ptr,12,v) then
        fMosaic.SetParam(6,v);

        obj.AddPoly(@lp.Pol,lp.N+1);
        ip:=obj.Poly.PartCount;

        // alfa
        if not dm.xGet_Int(ptr,10,v) then v:=100;
        obj.SetParam(ip,0,v);

        // radius
        rad:=dm.Get_Real(ptr,11); if rad > 0 then
        obj.SetParam(ip,1,Round(rad*ppm));

        top:=dm.Tree._Child(ptr);
        if top > 0 then begin
          run:=dm.Tree._Link(top);

          while run <> top do begin

            if dm.xGet_poly(run,lp,nil,LPolyMax) > 0 then
            if PolyLock(lp) then begin

              obj.AddPoly(@lp.Pol,lp.N+1);
              ip:=obj.Poly.PartCount;

              // alfa
              if not dm.xGet_Int(run,10,v) then v:=0;
              if dm.xGet_Str(run,9,nm1) <> nil then v:=100-v;

              obj.SetParam(ip,0,v);

              // radius
              rad:=dm.Get_Real(run,11); if rad > 0 then
              obj.SetParam(ip,1,Round(rad*ppm));
            end;

            run:=dm.Tree._Link(run)
          end
        end;

        s1:=dm.LG_T.sys; s1.pps:=0;

        _lp:=obj.Poly.First;
        for i:=0 to obj.Poly.Count-1 do begin
          dm.LG_T.L_to_Z(_lp[i],t1);
          t2:=gp_to_prj(t1,s1,s2);
          _lp[i]:=iProjective_3x3(t2.x,t2.y,tr)
        end;

        if obj.GetInterface(IPolyList,intf) then begin
          obj:=nil; Result:=intf
        end
      finally
        obj.Free
      end
    end
  end;

  c:=Globe.jpg_cut;

  if Result = nil then
  if c.X+c.Y > 0 then
  if This_ext(rec.Path,'.jpg') then begin

    iw:=rec.l_rb.X;
    ih:=rec.l_rb.Y;

    cx:=iw * c.X div 100;
    cy:=ih * c.Y div 100;

    Dec(iw,(cx+cx)); Dec(ih,(cy+cy));

    if log_enabled then begin
      StrFmt(nm,'"%s" %d %d %d %d',
        [xStrNameExt(rec.Path),cx,cy,iw,ih]);
      std_log.WriteAnsi(nm)
    end;

    if (iw >= 32) and (ih >= 32) then begin
      lp.N:=Port_to_LPoly(cx,cy,iw,ih,@lp.Pol);

      obj:=TPolyListIntf.Create;
      try
        obj.AddPoly(@lp.Pol,lp.N+1);
        obj.SetParam(1,0,100);

        if obj.GetInterface(IPolyList,intf) then begin
          obj:=nil; Result:=intf
        end
      finally
        obj.Free
      end

    end
  end
end;

var
  rec: TMapRec;
  proj: IProject2;
  proj1: TProjMosaic;
  proj2: TWgsMosaic;
  kml: IKmlImage;
  clip: IPolyList;
begin
  if Globe.Bank2.Get_map(Ind,rec) then
  if Globe.Disp_scale(rec,Scale) then

  if (rec.flags and prj_tif_kml) <> 0 then begin

    if DllKmlInterface(rec.Path,kml) = S_OK then begin

      proj2:=TWgsMosaic.Create(Self);
      try
        if proj2.sys.pps >= 0 then
        if proj2.GetInterface(IProject2,proj) then begin
          fMosaic.Kml(kml,proj); proj1:=nil
        end
      finally
        proj1.Free
      end;

      proj:=nil;
    end;

    kml:=nil;
  end
  else begin

    proj:=nil;
    East_lg_Transit(rec.lg,Globe.p_lg);

    proj1:=TProjMosaic.Create(Self,rec);
    try
      if proj1.sys1.pps >= 0 then
      if proj1.GetInterface(IProject2,proj) then begin
        fMosaic.SetParam(3,rec.Color); // SetPlastic

        if Globe.MosaicClip <> 0 then begin
          clip:=GetClipTif(rec);
          if Assigned(clip) then
          fMosaic.SetParam(7,int(clip));
        end;

        fMosaic.Paint(rec.Path,@rec.lg.l,proj);

        fMosaic.SetParam(6,0);
        fMosaic.SetParam(7,0);
        proj1:=nil
      end
    finally
      proj1.Free
    end;

    proj:=nil;
  end;
end;

procedure draw_sas;
var
  sas: IGoogle;
  proj: IProject2;
  proj1: TWgsMosaic;
begin
  if Globe.sasChart.Enabled then
  if Globe.GetSasInterface(sas) then begin

    if Assigned(Globe.Http) then begin
      Globe.Http.Reset;
      sas.SetHttp(Globe.Http);
    end;

    proj:=nil;

    proj1:=TWgsMosaic.Create(Self);
    try
      if proj1.sys.pps >= 0 then
      if proj1.GetInterface(IProject2,proj) then begin
        fMosaic.Google(sas,proj);
        proj1:=nil
      end
    finally
      proj1.Free
    end;

    proj:=nil; sas:=nil
  end
end;

procedure draw_video;
var
  proj: IProject2;
  proj1: TWgsMosaic;
begin
  proj1:=TWgsMosaic.Create(Self);
  try
    if proj1.sys.pps >= 0 then
    if proj1.GetInterface(IProject2,proj) then begin
      fMosaic.Video(nil,proj,-1,nil,nil);
      proj:=nil; proj1:=nil
    end
  finally
    proj1.Free
  end;
end;

var
  i,j,sc,alf: int; tr: Real3x3;
  rec: TMapRec; map,map2: Bitmap;
  pmap: PBitmap; sas: bool;
begin
  sc:=Globe.Bank2.BankScale1;

  if Assigned(fMainGlobe) then
  Globe.Sas:=fMainGlobe.Sas;

  sas:=false;
  if not IsPcx then
  if dir_Exists(Globe.Sas) then
  sas:=Globe.sasChart.Active;

  if Assigned(fMosaic) then
  if dispFlags and prj_disp_tif = 0 then begin

    if (Globe.Bank2.Count > 0) or sas
    or (fMosaic.GetParam(10) > 0) then

    if (sc = 0) or (dc_Scale(dc) <= sc) then
    if dc.GetMap(map) then begin

      pmap:=@map; alf:=Globe.MosaicAlfa;
      if alf > 100 then alf:=100;

      if alf > 0 then
      if Cash2.Realloc_rgb(map.bmWidth,map.bmHeight) then
      if Cash2.GetMap(map2) then begin
        Cash2.FillCanvas(15); pmap:=@map2
      end;

      Globe.Bank2.OnDelete:=MosaicClose;

      if Globe.Count > 0 then
        Setup_Map_Display(dc,nil,Globe.ActiveMap)
      else
        lg_p.Assign(Globe.p_lg,-1);

      fMosaic.SetParam(1,fctx.rgb_Ground_Color);  // SetClearColor
      fMosaic.SetParam(2,Globe.MosaicColor);      // SetTransparent
      fMosaic.SetParam(4,ibool[IsPcx]);

      if fMosaic.BeginPaint(pmap) then begin

        if sas then draw_sas;

        j:=-1;
        if Globe.tif_active_last then
        j:=Globe.Bank2.ItemIndex;

        alf:=255 - Globe.Bank2.Count;

        for i:=0 to Globe.Bank2.Count-1 do
        if i <> j then begin
          fMosaic.SetParam(5,alf);
          draw_tif(i,Scale); Inc(alf)
        end;

        fMosaic.SetParam(5,255);
        if j >= 0 then draw_tif(j,Scale);

        draw_video; fMosaic.EndPaint;

        if pmap = @map2 then
        alfaBitmap(map2,map,clWhite,alf/100);

        if fMosaicBmp then
        img_swap_height(map.bmBits,map.bmHeight,map.bmWidthBytes)
      end
    end
  end
end;

procedure Get_g_lt_rb(dc: XCanvas; p: PRect; var lt,rb: tgauss);
var
  R: TRect; t: GOrient;
begin
  if p <> nil then R:=p^ else R:=dc.CliPRect;

  with R do begin
    pBackup(Left,Top,t[0]);     pBackup(Right,Top,t[1]);
    pBackup(Right,Bottom,t[2]); pBackup(Left,Bottom,t[3])
  end;

  Max_Gauss_Bound(@t,4,lt,rb)
end;

procedure pickup_push(dc: XCanvas; tmp: tdmw_List);

procedure push_vc(src,dest: tvc_List);
var
  i: int; p: TInt64; ip: pvc_disp_arr;
begin
  p.cn:=cn_node;

  ip:=src.First;
  for i:=0 to src.Count-1 do begin
    p.id:=ip[i].Id;
    if x_Draw_Object(nil,p.id) then
    dest.Add(@ip[i])
  end
end;

procedure push_ve(src,dest: tve_List);
var
  i: int; p: TInt64; ip: PIntegers;
begin
  p.cn:=cn_edge;

  ip:=src.First;
  for i:=0 to src.Count-1 do begin
    p.id:=ip[i];
    if x_Draw_Object(nil,p.x) then
    dest.AddItem(p.id)
  end
end;

procedure push_fe(src,dest: tfe_List);
var
  i,p: int; ip: PIntegers; dRec,dRec1: dm_Rec;
begin
  ip:=src.First;
  for i:=0 to src.Count-1 do begin
    p:=ip[i];
    if x_Draw_Object(nil,p) then begin

      dRec:=fdraw_node.dRec;
      dRec.View:=255;

      if not Pickup.x_fe.Get_data(i,p,dRec1) then
      dRec.View:=dRec1.View;

      dest.xAdd(p,-1,dRec)
    end
  end
end;

var
  r: TRect;
begin
  if Pickup.x_Count > 0 then begin
    r:=Rect(0,0,dc.Width,dc.Height);
    Setup_Map_Display(dc,@r,Globe.ActiveMap);

    push_vc(Pickup.x_vc,tmp.x_vc);
    push_ve(Pickup.x_ve,tmp.x_ve);
    push_fe(Pickup.x_fe,tmp.x_fe)
  end;

  Pickup.Clear;
end;

procedure pickup_pop(tmp: tdmw_List);

procedure pop_vc(src,dest: tvc_List);
var
  i: int; ip: pvc_disp_arr;
begin
  ip:=src.First;
  for i:=0 to src.Count-1 do
  if dest.id_Itemof(ip[i].Id) = nil then
  dest.Add(@ip[i])
end;

procedure pop_ve(src,dest: tve_List);
var
  i,p: int; ip: PIntegers;
begin
  ip:=src.First;
  for i:=0 to src.Count-1 do begin
    p:=ip[i];
    if dest.IndexOf(p) < 0 then
    dest.AddItem(p)
  end
end;

procedure pop_fe(src,dest: tfe_List);
var
  i,p: int; ip: PIntegers; dRec: dm_Rec;
begin
  ip:=src.First;
  for i:=0 to src.Count-1 do begin

    p:=ip[i];
    if dest.IndexOf(p) < 0 then begin
      if not src.Get_data(i,p,dRec) then begin
        Globe.Map.Get_Object(p,dRec);
        dRec.View:=255
      end;

      dest.xAdd(p,-1,dRec)
    end
  end
end;

begin
  if Globe.Map.Enabled_Map then begin
    pop_vc(tmp.x_vc,Pickup.x_vc);
    pop_ve(tmp.x_ve,Pickup.x_ve);
    pop_fe(tmp.x_fe,Pickup.x_fe);
  end
end;

function init_drawLink: Boolean;
var
  lnk: tdmwLink;
  i: Integer; lp: PLinkArray;
  r: TLinkPoint; p: TGauss;
begin
  Result:=false; fdrawLink.Clear;

  lnk:=Globe.Pcx_link;

  if FileExist(Background) then
  if lnk.Count > 2 then begin

    lp:=lnk.First;
    for i:=1 to lnk.Count do begin
      r:=lp[0]; lp:=@lp[1];
      p:=Globe.Map.LG_T.z_xy_to_pix(r.b);
      r.b:=dm_to_dc(p.x,p.y); fdrawLink.Add(@r)
    end;

    if Globe.Map.Open_Map(Background,false) then begin

      lp:=fdrawLink.First;
      for i:=1 to fdrawLink.Count do begin
        p:=Globe.Map.LG_T.z_xy_to_pix(lp[0].a);
        lp[0].a:=p; lp:=@lp[1];
      end;

      fdrawLink.Refresh;
      Result:=true
    end
  end
end;

procedure draw_prj(dc: XCanvas;
                   Bank: TMapBank; par: int;
                   const lt,rb: TGauss;
                   Scale: int);
var
  top,run,dm_i,vs,pg,sc,sc1: int;
  rec: TMapRec; sc_: ZOrient;
begin
  vs:=dispFlags; 

  sc_[0]:=Bank.BankScale1;
  sc_[1]:=Bank.BankScale2;
  sc:=Round(dc_Scale(dc));
  if prn_scale > 0 then
  sc:=prn_scale;

  if not IsOcx or (vs <> 3) then
  with Bank.Tree do begin

    top:=_Child(Par);
    if top > 0 then begin

      run:=_Link(top);
      while run <> top do begin
        _Info(run,rec,SizeOf(rec));
        pg:=ibool[ rec.flags and prj_map_dll <> 0 ];
        sc1:=sc_[pg];

        if rec.dm_scale = prj_folder_Id then begin
          if Globe.Disp_map(rec,lt,rb,Scale) then
          draw_prj(dc,Bank,run,lt,rb,Scale)
        end else

        if (vs and (1 shl pg)) = 0 then

        if Globe.Disp_map(rec,lt,rb,Scale) then begin
          dm_i:=Bank.List.IndexOf(run);
          if dm_i >= 0 then

          if (Card = nil)
          or (dm_i <> Bank.Video-1) then
          if (sc1 = 0) or (sc1 >= sc) then

          if Globe.Open_dm(dm_i) then begin
            Setup_Map_Display(dc,p,dm_i);
            Draw_map(dc,dm_i);
            if not IsDraw then Break
          end;
        end else

        if not rec.Visible and (pg = 1) then begin
          rec.Visible:=true;
          if Globe.Disp_map(rec,lt,rb,Scale) then begin

            dm_i:=Bank.List.IndexOf(run);
            if dm_i >= 0 then

            if Globe.Open_dm(dm_i) then
            if not Globe.Map.Is_dm then begin
              Setup_Map_Display(dc,p,dm_i);

              with Globe.Map.Tree do
              vm_dmw.Draw_bound(dc.Canvas.GetDC,
                                @dm_t,rec.Color);
            end;

            Globe.Map.Close_Map
          end
        end;

        run:=_Link(run)
      end
    end
  end
end;

var
  tmp1: THintList;
  tmp: tdmw_List; lt,rb: TGauss;
  mode,scale: int; g: XGeoid;
  grid: Boolean;
begin
  scale:=Round(kz_Scale(dc,kz));
  if prn_scale > 0 then scale:=prn_scale;
  Mosaic_draw(dc,scale);

  isDraw:=true; fDrawCount:=0;
  fDrawTick:=GetTickCount;

  tmp:=tdmw_List.Create(Globe.Map);
  tmp1:=THintList.Create;
  try
    fx_Disp:=Globe.x_Disp;
    fx_Mark:=fx_Disp.Mark;
    fx_all:=fx_Disp.Objs;

    if Assigned(fQuery) then begin
      fQuery.Hints:=fHints;
      fQuery.x_Disp:=Globe.x_Disp;
    end;

    if Assigned(p) then
    tmp1.LoadList(fHints);
    fHints.Clear;

    with Globe do
    if Open_Active_Map then begin

      Fonts.sync_folder;

      if Card <> nil then
      if Assigned(Pickup) then begin
        if p <> nil then Pickup_push(dc,tmp);
        Pickup.Clear;
      end;

      Setup_Map_Display(dc,nil,ActiveMap);
      fhints.BeginDraw(16,16,map_lt,map_rb);

      scale:=Round(dc_Scale(dc));
      if Preview <> nil then
      scale:=Round(Preview.dc_Scale(nil));
      if prn_scale > 0 then scale:=prn_scale;
      fdraw_scale:=scale;

      Get_g_lt_rb(dc,p,lt,rb);

      if init_drawLink then begin

        if fdrawLink.Count > 2 then begin
          mode:=dm_t_mode; dm_t_mode:=255;
          Setup_Map_Display(dc,nil,-1);
          Draw_map(dc,-1); dm_t_mode:=mode;
        end;

        Open_Active_dm;
        Setup_Map_Display(dc,nil,ActiveMap);
      end;

      grid:=x_Disp.Grid; if Map.bl_grid then
      if grid then x_Disp.Grid:=false;

      if not isMoving then begin
        if Assigned(fOnBackground) then
        fOnBackground(dc);

        draw_prj(dc,Bank,Bank.Tree.Root,lt,rb,scale)
      end;

      if Assigned(Pickup) then pickup_pop(tmp);

      Open_Active_dm;

      Setup_Map_Display(dc,nil,ActiveMap);

      if Assigned(fOnInvalidate) then begin
        Map.Close_Map; fOnInvalidate(dc);
      end;

      if grid then begin
        Draw_Grid(dc,true,true);
        x_Disp.Grid:=true
      end;

      fHints.Pop(tmp1)
    end
    else begin
      fHints.Clear
    end
  finally
    tmp1.Free;
    tmp.Free
  end;

  isDraw:=false;
end;

procedure TDrawCard.Draw_Grid(dc: XCanvas; prj,grid: Boolean);

procedure Orient_grid_t;
var
  tr: Real3x3;
begin
  Begin_3x3(grid_t,0,0);

  if not isPcx then begin
    Init_3x3(tr, -XPos,-YPos, kz,kz);
    grid_t:=prj_tr; Forw_3x3(grid_t,tr);
  end;

  Globe.Map.LG_t.x_ba.Get_Matrix(gauss_t);
  lg_p.x_ab.Get_Matrix(tr); Forw_3x3(gauss_t,tr)
end;

function xSeconds(r: Double): double;
begin
  Result:=r*180*3600/Pi
end;

function xRadian(s: Double): double;
begin
  Result:=s*Pi/180/3600
end;

function Align_left(r: Double; ds: Integer): Integer;
begin
  Result:=int_Trunc(Trunc(r),ds);
  while Result > r do Dec(Result,ds);
end;

function Align_right(r: Double; ds: Integer): Integer;
begin
  Result:=int_Trunc(Trunc(r),ds);
  while Result < r do Inc(Result,ds);
end;

function Next_ds(s: Integer): Integer;

function Next_d(d: Integer): Integer;
begin
  if d <= 1 then Inc(d) else
  if d = 2 then d:=5 else
  if d < 15 then Inc(d,5) else d:=d*2;
  Result:=d
end;

var
  g,m: Integer;
begin
  g:=s div 3600; m:=(s div 60) mod 60;

  if s > 0 then s:=Next_d(s) else
  if m > 0 then m:=Next_d(m) else begin
    g:=Next_d(g); if g > 15 then g:=15
  end;

  Result:=g*3600 + m*60 + s;
end;

function grid_Project(x,y: double): TPoint;
begin
  Result.x:=Round(grid_t[1,1]*x+grid_t[1,2]*y+grid_t[1,3]);
  Result.y:=Round(grid_t[2,1]*x+grid_t[2,2]*y+grid_t[2,3])
end;

function gauss_Project(x,y: double): TPoint;
var
  g: tgauss; p: TPoint;
begin
  if IsPcx then begin
    g.x:=x; g.y:=y;
    Globe.Map.LG_t.G_to_L(g,p);

    dm_to_pcx(p.x,p.y,g);
    Result:=img_Project(g.x,g.y)

  end
  else begin
    g:=Projective_3x3(x,y,gauss_t);
    Result:=grid_Project(g.x,g.y)
  end
end;

function geoid_Project(const r: tgeoid): TPoint;
var
  p: TPoint; g: tgauss;
begin
  if isPcx then begin
    Globe.Map.LG_t.R_to_L(r,p);
    dm_to_pcx(p.x,p.y,g);
    Result:=img_Project(g.x,g.y)
  end
  else begin
    lg_p.BL_XY(r.b,r.l,g);
    Result:=grid_Project(g.x,g.y)
  end;
end;

procedure Insert_bl(lp: PLLine; b,l: Double);
var
  r: TGeoid;
begin
  r.b:=xRadian(b); r.l:=xRadian(l);
  LPoly_Next(lp,LPolyMax,geoid_Project(r))
end;

function Grid_Latitude(l1,l2: Double; b,dl: Integer;
                       lp: PLLine): Boolean;
var
  l: Integer;
begin
  l:=dl div 8;
  if l*8 = dl then dl:=l;

  l:=int_Trunc(Trunc(l1),dl);

  lp.N:=-1;
  while l < l2 do begin
    if l < l1 then Insert_bl(lp,b,l1)
    else           Insert_bl(lp,b,l);
    Inc(l,dl)
  end;

  Insert_bl(lp,b,l2); Result:=lp.N > 0
end;

function Grid_Longitude(b1,b2: Double; l,db: Integer;
                        lp: PLLine): Boolean;
begin
  lp.N:=-1; while b1 < b2 do
  begin Insert_bl(lp,b1,l); b1:=b1+db end;
  Insert_bl(lp,b2,l); Result:=lp.N > 0
end;

function grid_Line(dc: XCanvas; bp,lp: PLLine;
                   bl,draw: Boolean): Boolean;
var
  i: Integer; poly: TPolylist;
begin
  Result:=false;

  if Assigned(bp) and (bp.N > 0) then

  if bl then begin
    poly:=TPolylist.Create;
    try
      if xClipPolyLine1(poly,bp,lp) > 0 then

      for i:=0 to poly.PartCount-1 do begin
        poly.get_line(i,lp,nil,LPolyMax);
        clip_master.OutLine(dc,lp,1,fGrid)
      end

    finally
      poly.Free
    end
  end else
  if not xClipPolyline(bp,lp) then lp.N:=-1;

  if draw then clip_master.OutLine(dc,lp,1,fGrid);

  Result:=lp.N > 0
end;

function Up_Grid(b,l,dl: Integer): Boolean;
var
  p1,p2: TPoint; r: tgeoid;
  g: tgauss; w: Integer;
begin
  Result:=false;

  r.b:=xRadian(b);
  r.l:=xRadian(l);
  lg_p.BL_XY(r.b,r.l,g);
  p1:=grid_Project(g.x,g.y);

  r.l:=xRadian(l+dl);
  lg_p.BL_XY(r.b,r.l,g);
  p2:=grid_Project(g.x,g.y);

  w:=Round(xPixelsPerMeter(0).y*0.01);
  Result:=w < Long_Dist(p1,p2)
end;

procedure Up_Latitude(dc: XCanvas; lp: PLLine; cb: Integer);
var
  i,h,x,y,dx: int; a,b,sz: TPoint;
  s,s1,s2: string;
begin
  x:=2; y:=-1;

  with lp^ do
  if Pol[0].x < dc.Width then
  if Pol[N].x > 0 then begin

    if Pol[0].x >= 0 then begin
      x:=Pol[0].x+2; y:=Pol[0].y
    end
    else begin
      i:=1; while Pol[i].x <= 0 do Inc(i);
      a:=Pol[i-1]; b:=Pol[i];

      dx:=b.x-a.x; if dx > 0 then
      y:=a.y - Round((b.y-a.y)/dx*a.x)
    end
  end;

  if y > 4 then
  if y < dc.Height-4 then begin
    s:=AngleStr(xRadian(cb),0);
    s1:=''; s2:=''; i:=SysTem.Pos(#176,s);

    if i > 0 then begin
      s1:=System.Copy(s,1,i);
      s2:=System.Copy(s,i+1,length(s)-i+1)
    end;

    sz:=dc.TextExtents(s1); h:=sz.Y;

    if x > 2 then Dec(x,sz.X);
    if x < 2 then x:=2;

    dc.TextColor:=clBlack;
    dc.rgb_Brush(clLtGray);

    if length(s2) > 0 then begin
      dc.TextOuts(x,y-h,s1);
      dc.TextOuts(x,y,s2);
    end
    else dc.TextOuts(x,y-h-2,s)
  end
end;

procedure Up_Longitude(dc: XCanvas; lp: PLLine; cl: Integer);
var
  i,h,x,y,dy: int; a,b: TPoint;
  s: string;
begin
  if lp.N = 1 then begin
    a:=lp.Pol[1]; b:=lp.Pol[0];

    x:=-1; y:=2;

    if a.y < dc.Height then
    if b.y > 0 then begin

      if a.y >= 0 then begin
        x:=a.x; y:=a.y+2
      end
      else begin
        dy:=b.y-a.y; if dy > 0 then
        x:=a.x - Round((b.x-a.x)/dy*a.y)
      end
    end;

    if x > 4 then
    if x < dc.Width-4 then begin
      s:=AngleStr(xRadian(cl),0);

      dc.TextColor:=clBlack;
      dc.rgb_Brush(clLtGray);

      dc.TextOuts(x+1,y,s)
    end
  end
end;

procedure Gauss_Step(dc: XCanvas; var g_dx,g_dy: Integer);

function Local_Gauss(const p: TPoint; g_dx,g_dy: Integer): TPoint;
var
  g: tgauss;
begin
  Globe.Map.LG_T.L_to_G(p,g);
  Result.x:=Round(g.x/g_dx);
  Result.y:=Round(g.y/g_dy)
end;

var
  a,b,c: TPoint; i: Integer;
  g1,g2: tgauss;
begin
  if Card <> nil then Backup_Card(a,b)
  else Backup_dc(dc,0,a,b);

  Middle_point(a,b,c);

  Globe.Map.LG_T.L_to_G(c,g1);
  a:=gauss_Project(g1.x,g1.y);

  for i:=1 to 256 do begin
    b:=gauss_Project(g1.x+g_dx,g1.y);
    if Long_Dist(a,b) > 16 then Break;
    g_dx:=g_dx * 2
  end;

  for i:=1 to 256 do begin
    b:=gauss_Project(g1.x,g1.y+g_dy);
    if Long_Dist(a,b) > 16 then Break;
    g_dy:=g_dy * 2
  end;
end;

procedure Gauss_Line(dc: XCanvas; Clip: PLLine;
                     x1,y1,x2,y2, dx,dy, g_dx,g_dy: Integer);
var
  lp: PLLine; p,p1,p2, q1,q2, lt,rb: TPoint;
begin
  p1:=gauss_Project(x1*g_dx,y1*g_dy);
  p2:=gauss_Project(x2*g_dx,y2*g_dy);

  lt.x:=Min(p1.x,p2.x); lt.y:=Min(p1.y,p2.y);
  rb.x:=Max(p1.x,p2.x); rb.y:=Max(p1.y,p2.y);

  if dc.Contains_rect(lt.x,lt.y,rb.x,rb.y, 8,8) then
  begin
    lp:=x_Edit.DPolybuf;
    if Assigned(lp) then

    if ClipLine(Clip, p1,p2, q1,q2) then
    with lp^ do begin

      Pol[0]:=q1; Pol[1]:=q2; N:=1;
      p1:=q1; p2:=q2; Swap_lPort(p1,p2);

      while lp.N < LPolyMax do begin
        Inc(x1,dx); if x1 > x2 then Break;
        Inc(y1,dy); if y1 > y2 then Break;

        p:=gauss_Project(x1*g_dx,y1*g_dy);
        if PortContainsPoint(p1,p2, p.x,p.y) then begin
          Pol[N]:=p; Inc(N); Pol[N]:=q2
        end
      end;

      grid_Line(dc,nil,lp,false,true)
    end
  end
end;

function fi_step(nn: Integer): Integer;
begin
  with Globe.Map do
  Result:=Round(xSeconds(Get_Double(0,nn)))
end;

function dg_step(nn,def: Integer): Integer;
begin
  Result:=Globe.Map.Get_Int(0,nn);
  if Result <= 0 then Result:=def
end;

function bl_clip(bp: PLLine; var lt,rb: TGauss): Boolean;
var
  i: Integer; p1,p2,pc: TPoint;
  g: tgeoid; eps: Double;
begin
  Result:=false;

  eps:=Radian(0,0,1);

  p2:=bp.Pol[0];
  for i:=1 to bp.N do begin
    p1:=p2; p2:=bp.Pol[i];
    Middle_Point(p1,p2,pc);

    Globe.Map.LG_T.L_to_R(pc,g);

    if Abs(g.b-lt.x) > eps then Result:=true else
    if Abs(g.b-rb.x) > eps then Result:=true else
    if Abs(g.l-lt.y) > eps then Result:=true else
    if Abs(g.l-rb.y) > eps then Result:=true;

    lt.x:=Min(lt.x,g.b); rb.x:=Max(rb.x,g.b);
    lt.y:=Min(lt.y,g.l); rb.y:=Max(rb.y,g.l);
  end
end;

procedure grid_ellipse(dc: XCanvas; bp: PLLine; b0,l0,db,dl: Integer);
const
  pi_2 = 3600 * 90;
  pi2  = 3600 * 360;

function Insert_bl(lp: PLLine; b,l: Double; is_sp: Boolean): Boolean;
var
  r: tgeoid; g: tgauss; p: TPoint;
begin
  Result:=true;
  r.b:=xRadian(b); r.l:=xRadian(l);
  if is_sp then r.b:=-r.b;

  with Globe.Map do begin
    LG_T.BL_XY(r.b,r.l,g);

    if IsNAN(g.x) or IsNAN(g.y) then
      Result:=false
    else begin
      LG_T.G_to_L(g,p);
      LPoly_Next(lp,LPolyMax,p)
    end
  end
end;

procedure out_line(dc: XCanvas; bp,lp: PLLine);
begin
  if lp.N > 0 then
  clip_master.Grid_line(dc,bp,lp);
  lp.N:=-1
end;

procedure bound_extend(var lt,rb: TGauss; bp: PLLine);
var
  i: Integer; r: tgeoid;
begin
  with Globe.Map.LG_T do
  for i:=0 to bp.N do begin
    L_to_R(bp.Pol[i],r);
    if IsNan(r.b) then begin
      lt.x:=-Pi/2; rb.x:=+Pi/2; Break
    end
  end
end;

var
  b1,b2,l1,l2: Integer;
  lp: PLLine; lt,rb: TGauss;
  b,l,st: Double; sp: Boolean;
begin
  lp:=x_Edit.DPolyBuf;

  clip_master.plot_begin(dc,0,fGrid,0,0);

  db:=Max(3600,db); dl:=Max(3600,dl);

  with Globe.Map do
  if LG_t.Get_pps_Bound(lt,rb) = 1 then begin

    if Get_dm_Bound(lp,LPolyMax) > 0 then
    bound_extend(lt,rb,lp);

    sp:=false; if rb.x < 0 then begin
      sp:=true; lt.x:=Abs(lt.x); rb.x:=Abs(rb.x);
      if lt.x > rb.x then xSwap(lt.x,rb.x);
    end;

    if rb.y - lt.y >= Pi then begin
      lt.y:=-Pi; rb.y:=Pi;
      if rb.x > 0 then rb.x:=Pi/2 else
      if lt.x < 0 then lt.x:=-Pi/2
    end;

    b1:=Align_left(xSeconds(lt.x), db);
    l1:=Align_left(xSeconds(lt.y), dl);

    b2:=Align_right(xSeconds(rb.x), db);
    l2:=Align_right(xSeconds(rb.y), dl);

    if (b0 > 0) and (b0 < db) then begin
      Inc(b1,b0); Inc(b2,b0)
    end;

    if (l0 > 0) and (l0 < dl) then begin
      Inc(l1,l0); Inc(l2,l0)
    end;

    b:=b1; while b <= b2 do begin

      lp.N:=-1; l:=l1; st:=dl/8;
      while l <= l2 do begin
        if not Insert_bl(lp, b,l,sp) then
        out_line(dc,bp,lp); l:=l + st
      end;

      out_line(dc,bp,lp); b:=b + db
    end;

    if (l1 = 0) and (l2 = pi2) then Dec(l2,dl);

    if l1+Pi2 = l2 then Dec(l2,dl);

    l:=l1; while l <= l2 do begin

      lp.N:=-1; b:=b1; st:=db/8;
      while b <= b2 do begin
        if not Insert_bl(lp, b,l,sp) then
        out_line(dc,bp,lp); b:=b+st
      end;

      out_line(dc,bp,lp); l:=l+dl
    end

  end
end;

const
  grid_dg: array[0..11] of Integer =
  (1000,16000,8000,4000,2000,1000,1000,100,50,20,10,5);

var
  b0,l0,b1,b2,l1,l2,db,dl: Integer;
  i,cx,delta, px,py, g_dx,g_dy: integer;
  lt,rb,_lt,_rb, g: tgauss; r: tgeoid;
  p, a,b, t1,t2: TPoint; hdr: dm_Hdr;
  Rect: LOrient; bp,lp: PLLine;
  is_bl: Boolean;

begin
  bp:=x_Edit.DPolyBuf1;
  if bp <> nil then

  with Globe.Map do
  if Enabled_Map then

  if Get_dm_Bound(bp,LPolyMax) > 0 then
  if Polylock(bp) then begin

    dc.ResetCanVas(0,15);
    Get_dm_Hdr(hdr);

    is_bl:=false;

    if hdr.sys.pps = 1 then begin
      b0:=fi_step(nn_grid_b0);
      l0:=fi_step(nn_grid_l0);

      db:=fi_step(nn_grid_db);
      dl:=fi_step(nn_grid_dl);

      case hdr.sys.prj of
    1,
    2:  if (db > 0) and (dl > 0) then
          is_bl:=true
        else
          is_bl:=hdr.scale in [1..2];

    3,
    4,
    13,      
    prj_cuba,
    prj_cuba+1:
        is_bl:=(db > 0) and (dl > 0);
      end
    end;

    if is_bl then begin
      Orient_grid_t;

      if hdr.sys.prj in prj_tilted then

        grid_ellipse(dc,bp, b0,l0,db,dl)

      else begin
        if (db <= 0) or (dl <= 0) then

        if hdr.sys.prj in [1,2] then begin
          db:=20*60; dl:=30*60; if hdr.scale = 1 then
          begin db:=db*2; dl:=dl*2 end;
        end
        else begin
          db:=15; dl:=15
        end;

        lp:=x_Edit.DPolybuf; lp.N:=-1;

        lt:=lt_g; rb:=rb_g; if prj then begin
          lt:=Globe.z_lt; rb:=Globe.z_rb
        end;

        bl_clip(bp, lt,rb); Project_lp(bp);

        lt.x:=xSeconds(lt.x); lt.y:=xSeconds(lt.y);
        rb.x:=xSeconds(rb.x); rb.y:=xSeconds(rb.y);

        b1:=Align_left(lt.x, db);
        l1:=Align_left(lt.y, dl);

        b2:=Align_right(rb.x, db);
        l2:=Align_right(rb.y, dl);

        if (b0 > 0) and (b0 < db) then begin
          Inc(b1,b0); Inc(b2,b0)
        end;

        if (l0 > 0) and (l0 < dl) then begin
          Inc(l1,l0); Inc(l2,l0)
        end;

        _lt:=lt; _rb:=rb;
        if fx_Disp.dm_Frame and grid then begin
          _lt.x:=lt.x + 0.0001;
          _lt.y:=lt.y + 0.0001;
          _rb.x:=rb.x - 0.0001;
          _rb.y:=rb.y - 0.0001;
        end;

        delta:=8; if not grid then
        delta:=dc.TextExtent('9999').Y*4;

        while true do begin cx:=(b2-b1) div db;
          if cx <= 1 then Break else if cx < 1024 then

          with lp^ do
          if Grid_Longitude(b1,b1+db, l1,db, lp) then
          if Long_Dist(Pol[0],Pol[1]) >= delta then Break;

          db:=Next_ds(db)
        end;

        while true do begin cx:=(l2-l1) div dl;
          if cx <= 1 then Break else if cx < 1024 then

          with lp^ do
          if Grid_Latitude(l1,l1+dl, b1,dl, lp) then
          if Long_Dist(Pol[0],Pol[1]) >= delta then Break;
          dl:=Next_ds(dl)
        end;

        if grid or Up_Grid(b1,l1,dl) then begin

          if Assigned(Pickup) then begin
            px:=b1; while px <= b2 do begin

              py:=l1; while py <= l2 do begin
                r.b:=xRadian(px); r.l:=xRadian(py);
                p:=geoid_Project(r);

                if dc.Contains_point(p.x,p.y,8,8) then begin
                  LG_t.R_to_L(r,p); Pickup.grid_Display(p);
                end;

                py:=py+dl;
              end;

              px:=px+db;
            end
          end;
                           
          while b1 <= b2 do begin

           if (b1 >= _lt.x) and (b1 <= _rb.x) then begin
              grid_Latitude(lt.y,rb.y, b1,dl, lp);
              if grid_Line(dc,bp,lp,true,grid) then
              if not grid then Up_Latitude(dc,lp,b1);
            end;

            b1:=b1+db;
          end;

          while l1 <= l2 do begin

            if (l1 >= _lt.y) and (l1 <= _rb.y) then begin
              grid_Longitude(lt.x,rb.x, l1,db, lp);
              if grid_Line(dc,bp,lp,true,grid) then
              if not grid then Up_Longitude(dc,lp,l1)
            end;

            l1:=l1+dl;
          end                
        end
      end
    end else
    if grid then begin
      Orient_grid_t;

      if hdr.scale > 11 then g_dx:=10
      else g_dx:=grid_dg[hdr.scale];

      g_dx:=dg_step(nn_grid_dx,g_dx);
      g_dy:=dg_step(nn_grid_dy,g_dx);

      Gauss_Step(dc,g_dx,g_dy);

      Max_Poly_Bound(@bp.Pol,bp.N, a,b);
      Bound_to_LPoly(a,b, @Rect);

      for i:=0 to 3 do begin
        LG_T.L_to_G(Rect[i],g);
        Rect[i].X:=Round(g.x/g_dx);
        Rect[i].Y:=Round(g.y/g_dy)
      end;

      Max_Poly_Bound(@Rect,4, a,b);
      Dec(a.X); Dec(a.Y); Inc(b.X); Inc(b.Y);

      with bp^ do
      for i:=0 to N do begin
        LG_t.L_to_G(Pol[i],g);
        Pol[i]:=gauss_Project(g.x,g.y)
      end;

      for px:=a.x to b.x do
      Gauss_Line(dc,bp, px,a.y,px,b.y, 0,1, g_dx,g_dy);

      for py:=a.y to b.y do
      Gauss_Line(dc,bp, a.x,py,b.x,py, 1,0, g_dx,g_dy);

      if Assigned(Pickup) then
      for px:=a.x to b.x do
      for py:=a.y to b.y do begin
        g.x:=px * g_dx; g.y:=py * g_dy;
        p:=gauss_Project(g.x,g.y);

        if dc.Contains_point(p.x,p.y,8,8) then begin
          LG_t.G_to_L(g,p); Pickup.grid_Display(p);
        end
      end
    end
  end
end;

function TDrawCard.Draw_trig(vp: PIntegers): HResult;
var
  i: int; lp: PLPoly; l: LOrient;
  lt,rb,p: TPoint; 
begin
  Result:=S_FALSE; lp:=@vp[0];

  p:=lp[0]; lt:=p; rb:=p;
  Project(p.X,p.Y,l[0]);

  for i:=1 to 2 do begin p:=lp[i];
    if p.X < lt.X then lt.X:=p.X;
    if p.X > rb.X then rb.X:=p.X;
    if p.Y < lt.Y then lt.Y:=p.Y;
    if p.Y > rb.Y then rb.Y:=p.Y;
    Project(p.X,p.Y,l[i]);
  end;

  l[3]:=l[0];

  if (lt.X < v_rb.X) and (rb.X > v_lt.X) then
  if (lt.Y < v_rb.Y) and (rb.Y > v_lt.Y) then begin

    if (lt.X < map_lt.X) or (rb.X > map_rb.X)
    or (lt.Y < map_lt.Y) or (rb.Y > map_rb.Y) then begin
      clip_master.plot_begin(fdraw_dc,0,0,0,0);
      clip_master.plot_polyline(@l,3);
    end
    else fdraw_dc.PolyLine(@l,3);

    if fdraw_id > 0 then

    if Assigned(Pickup) then
    if Pickup.x_Display_Enabled then
    Pickup.x_fe.oAdd(fdraw_id,0,2,lt,rb)
  end
end;

procedure TDrawCard.Draw_map(dc: XCanvas; dm_i: Integer);
var
  i,cl: int; rec: TMapRec; vp: PVPoly;
  fill: bool; fn: TShortstr;
begin
  dc.Fill:=fx_Disp.AreaFill;

  if dm_i >= 0 then
  if Card <> nil then
  if Pickup <> nil then
  if dm_i = Globe.ActiveMap then begin
    Pickup.IsDisplay:=true;
    fHints.Enabled:=true
  end;

  with Globe.Map do
  if Is_dm then begin
    Objects_Path(fn,'.BLN');

    isDraw:=false; fill:=dc.Fill;
    dc.Fill:=Get_Int(Tree.Root,800) = 1;

    if not skip_frames then
    if fx_Disp.dm_Frame or dc.Fill then
    Draw_Object(dc,Tree.Root,0,-1);

    dc.Fill:=fill; isDraw:=true;

    if fx_Disp.is_cn then begin
      begin_draw_cn(dc);

      with Globe.Map do
      if Tree.is_cn then begin
        for i:=0 to vc_Count-1 do
        draw_vc(dc,vc_Id[i]);

        for I:=0 to ve_Count-1 do
        draw_ve(dc,ve_Id[i]);
      end
    end;

    fAttrList.Clear;
    fAttrList.Enabled:=true;

    Draw_Objects(dc,Tree.Root,0,-1);

    vp:=fAttrList.First;
    for i:=0 to fAttrList.Count-1 do
    with vp[i] do Draw_attr(dc,-x,y,z);

    fAttrList.Clear;
    fAttrList.Enabled:=false;
  end;

  if dm_i >= 0 then
  if fx_Disp.Grid then
  Draw_Grid(dc,false,true);

  if Pickup <> nil then
  Pickup.IsDisplay:=false;
  fHints.Enabled:=false;

  done_Signs
end;

procedure TDrawCard.begin_draw_cn(dc: XCanvas);
var
 cl,w: int;
begin
  cl:=Globe.cl_Edge and 31; w:=1;

  if fdc_mode > x_Draw then begin
    if fdc_mode <> x_Group then w:=fctx.pen_Width;
    cl:=fMarkColor
  end;

  dc.xPen(w,cl); dc.xBrush(0,fGround);
end;

function TDrawCard.draw_vc(dc: XCanvas; Id: uint): Boolean;
var
  vc: tcn_rec; p: TPoint;
begin
  Result:=false; with vc do

  if Globe.Map.Tree.get_vc(Id,@vc) then

  if (Pos.X >= s_lt.X) and (Pos.X <= s_rb.X) then
  if (Pos.Y >= s_lt.Y) and (Pos.Y <= s_rb.Y) then
  begin
    if dc <> nil then begin
      if dm_t_xyz then begin with VPos do
        Globe.xyz_to_scb(x,y,z, p);
        inherited Project(p.x,p.y, p)
      end else

      Project(Pos.x,Pos.y, p);

      dc.Rectangle(p.X-3,p.Y-3,p.X+3,p.Y+3);

      if Assigned(Pickup) then
      if Pickup.x_Display_Enabled then
      Pickup.x_vc.xAdd(vc)
    end;

    Result:=true
  end
end;

function TDrawCard.draw_ve(dc: XCanvas; Id: uint): Boolean;
var
  i: int; lt,rb: TPoint;
begin
  Result:=false;

  with x_Edit do
  if Globe.Map.Get_ve_Poly(Id,DPolyBuf,DHeights,LPolyMax) > 0 then
  if mf_Project(DPolyBuf,DHeights,lt,rb,true,fx_Mark) > 0 then

  if (lt.X <= map_rb.X) and (rb.X >= map_lt.X) then
  if (lt.Y <= map_rb.Y) and (rb.Y >= map_lt.Y) then begin

    if dc <> nil then
    with DPolyBuf^ do begin

      if (lt.X < map_lt.X) or (lt.Y < map_lt.Y) or
         (rb.X > map_rb.X) or (rb.Y > map_rb.Y) then begin
        clip_master.plot_begin(dc,0,0,0,0);
        clip_master.plot_polyline(@Pol,N);
      end else

      dc.PolyLine(@Pol,N);

      if fx_Mark then
      for i:=1 to N-1 do with Pol[i] do
      if dc.Contains_point(X,Y, 8,8) then
      dc.Rectangle(X-1,Y-1,X+1,Y+1);

      if Assigned(Pickup) then
      if Pickup.x_Display_Enabled then
      Pickup.x_ve.AddItem(Id)
    end;

    Result:=true
  end
end;

procedure TDrawCard.Draw_Objects(dc: XCanvas; p,p_lev,p_code: int);
var
  top,run: int;
begin
  if isDraw then

  with Globe.Map do begin
    top:=Tree._Child(p);
    if top > 0 then begin

      if debug_enabled1 then
      std_log.WriteStr(Format('[begin] %d %d',[p,p_lev]));

      run:=Tree._Link(top); Inc(p_lev);
      while isDraw and (run > 0) and (run <> top) do begin

        if debug_enabled1 then
        std_log.WriteStr(Format('%d',[run]));

        run:=Draw_Object(dc,run,p_lev,p_code);
      end;

      if debug_enabled1 then
      std_log.WriteStr(Format('[end] %d',[p]));
    end
  end
end;

function TDrawCard.Draw_Object(dc: XCanvas; p,p_lev,p_code: longint): longint;

var
  Node: dm_Node;

function x_Disp_Sign: Boolean;
begin
  Result:=fx_Mark or (fdc_Mode > x_Draw)
end;

function Display_Object(Code: Longint): Boolean;
begin
  Result:=true; if Is_Code_Chars then
  Result:=Code_Chars[ CodeToChar(Code) ];
end;

function Display_Layer(p: Longint): Boolean;
var
  v1,v2: Longint;
begin
  Result:=false;

  if Is_Code_Chars then
    Result:=true
  else
  if Node.Flags and fl_draw = 0 then begin
    v1:=Globe.Map.Get_Int(p,101);
    v2:=Globe.Map.Get_Int(p,102);

    if fdraw_scale >= v1 then
    Result:=(v1 >= v2) or (fdraw_scale <= v2)
  end
end;

function Display_Sign: Boolean;
begin
  Result:=false;
  with Node.dRec do if View <= dm_View then
  if (ox1 >= s_lt.X) and (ox1 <= s_rb.X) then
  if (oy1 >= s_lt.Y) and (oy1 <= s_rb.Y) then
  Result:=true
end;

function Display_Poly(is_signs: Boolean): Boolean;
begin
  Result:=false;

  with Node.dRec do
  if View <= dm_View then
  if (ox1 < v_rb.X) and (ox2 > v_lt.X) then
  if (oy1 < v_rb.Y) and (oy2 > v_lt.Y) then

  if cd_View >= lp_View then
    Result:=true
  else

  if is_signs then
    Result:=true
  else
  if ((ox2-ox1) > fpoly_min)
  or ((oy2-oy1) > fpoly_min) then
    Result:=true
end;

function Get_DPolyBuf(p: longint): integer;
var
  lt,rb: TPoint;
begin
  Result:=-1; with x_Edit,Globe.Map do
  if xGet_Poly(p,DPolyBuf,DHeights,LPolyMax) >= 0 then
  Result:=mf_Project(DPolyBuf,DHeights,lt,rb,false,false)
end;

function Get_DPolyBufv(p: longint): integer;
var
  a,b,lt,rb: TPoint; vx,vy,l: double;
begin
  Result:=-1; with x_Edit,Globe.Map do
  if xGet_Poly(p,DPolyBuf,DHeights,LPolyMax) >= 0 then begin

    DPolybuf1.N:=-1;
    if DPolyBuf.N = 1 then begin
      a:=DPolyBuf.Pol[0];
      b:=DPolyBuf.Pol[1];
      vx:=b.X-a.X; vy:=b.Y-a.Y;
      l:=Abs(vx)+Abs(vy);
      if (l > 0) and (l < fsign_min) then begin
        l:=fsign_min / Hypot(vx,vy);
        b.X:=a.X + Round(vx*l);
        b.Y:=a.Y + Round(vy*l);
        DPolyBuf1.Pol[0]:=b;
        DPolybuf1.N:=0;

        mf_Project(DPolyBuf1,DHeights,lt,rb,false,false)
      end
    end;

    Result:=mf_Project(DPolyBuf,DHeights,lt,rb,false,false)
  end
end;

function Get_DPolyBufb(p: longint; out lt,rb: TPoint): integer;
begin
  Result:=-1; with x_Edit,Globe.Map do
  if xGet_Poly(p,DPolyBuf,DHeights,LPolyMax) >= 0 then
  Result:=mf_Project(DPolyBuf,DHeights,lt,rb,false,false)
end;

function Load_DPolyBuf(p: Integer; mark: Boolean;
                       out _lt,_rb: TPoint): Integer;
begin
  Result:=-1; with x_Edit,Globe.Map do
  if xGet_Poly(p,DPolyBuf,DHeights,LPolyMax) >= 0 then
  Result:=mf_Project(DPolyBuf,DHeights,_lt,_rb,true,mark)
end;

procedure Load_Mark_DPolybuf(dc: XCanvas; p: int; Curve: bool);
var
  lt,rb: TPoint;
begin
  if Load_DPolyBuf(p,true, lt,rb) >= 0 then
  x_Mark_DPolyBuf(dc,Curve)
end;

function draw_DPolyBuf(dc: XCanvas; p,cl: int; mmk: Double): Integer;
var
  lt,rb: TPoint; dp: PLLine;
begin
  Result:=0;

  dp:=x_Edit.DPolyBuf;
  if Load_DPolyBuf(p,false, lt,rb) >= 0 then begin

    if (lt.X < map_lt.X) or (lt.Y < map_lt.Y) or
       (rb.X > map_rb.X) or (rb.Y > map_rb.Y) then
    begin
      clip_master.plot_begin(dc,2,cl,0,mmk);
      Result:=clip_master.plot_polyline(@dp.Pol,dp.N)
    end
    else begin
      fctx.DrawPoly(dc,@dp.Pol,dp.N,cl,mmk);
      Result:=dp.N+1
    end;

    if fx_Mark then
      Load_Mark_DPolyBuf(dc,p,false)
    else
    with Globe.Map do
    if p = Tree.Root then
    if xGet_Poly(p,dp,nil,LPolyMax) >= 0 then
    mark_Points(dc,@dp.Pol,dp.N,4,13)
  end
end;

procedure pickup_only(p,lev: int; const dRec: dm_Rec);
begin
  if Assigned(Pickup) then
  if Pickup.x_Display_Enabled then
  Pickup.x_fe.xAdd(p,fdraw_lev+lev,dRec)
end;

procedure pickup_draw_node(p: int);
begin
  if Assigned(Pickup) then
  if Pickup.x_Display_Enabled then
  Pickup.x_fe.xAdd(p,fdraw_lev,Node.dRec);
end;

procedure After_disp(dc: XCanvas; p, _cod,_loc: uint);
begin
  if fdc_Mode = x_DRAW then
  if Assigned(Pickup) then
  if Pickup.x_Display_Enabled then
  Pickup.x_fe.xAdd(p,fdraw_lev,Node.dRec);

  if Assigned(fOnDrawObject) then
  fOnDrawObject(dc,p,_loc);

  if fdc_Mode in [x_DRAW,x_HIDE,x_UNDO] then
  if not fx_Disp.AttrDisp then
  Draw_attr(dc,p,_cod,_loc)
end;

procedure after_ivg1(p,code,color: uint);
var
  i: int; b: TLLine; s: TCmdStr;
begin
  pickup_draw_node(p);

  if fHints.Enabled then
  if Globe.Map.Hint_HF(p,9,s,Sizeof(s)-1) then
  if Vgm.Loc.Ind > 0 then begin

    for i:=0 to 3 do
    with Vgm.v_Rect[i] do
    Backup(X,Y,b.Pol[i]);

    b.Pol[4]:=b.Pol[0]; b.N:=4;
    fHints.AddPoly(p,3,@b.Pol,b.N,s)
  end
end;

procedure after_ivg(p,loc: uint);
var
  lp,lp1: PLLine; s: TShortstr;
begin
  pickup_draw_node(p);

  lp:=x_Edit.DPolyBuf;
  lp1:=x_Edit.DPolyBuf1;

  if fHints.Enabled then
  if Globe.Map.Hint_HF(p,9,s,0) then
  if Globe.Map.xGet_Poly(p,lp,nil,LPolyMax) >= 0 then

  if loc = 1 then
    fHints.AddSign(p,lp.Pol[0],s)
  else
  if lp.N > 0 then begin

    case loc of
  3:  if not PolyLock(lp) then lp.N:=-1;

  5:  begin
        Ellipse_LLine(lp,LPolyMax,1); loc:=3
      end;

  6,
  7:  begin
        CurveToLLine(lp1,lp,LPolyMax,2,1);
        Dec(loc,4)
      end;

  8:  if lp.N <> 2 then lp.N:=-1 else begin
        lp.N:=LPoly_Sectorp(@lp.Pol,lp.N);
        loc:=3
      end;
    end;

    if lp.N > 0 then
    if loc in [2,3] then
    fHints.AddPoly(p,loc,@lp.Pol,lp.N,s)
  end
end;

function fill_bp(dc: XCanvas; lp: PLLine; pc,fc: int): Integer;
var
  lt,rb: TPoint;
begin
  Result:=0;

  Max_Poly_Bound(@lp.Pol,lp.N+1,lt,rb);

  if (lt.X < win_lt.X) or (rb.X > win_rb.X)
  or (lt.Y < win_lt.Y) or (rb.Y > win_rb.Y) then
  begin
    clip_master.plot_begin(dc,3,pc,fc,0);
    Result:=clip_master.plot_Polyline(@lp.Pol,lp.N);
  end else

  with lp^ do begin
    fctx.FillPoly(dc,@Pol,nil,N,pc,fc);
    Result:=N+1
  end
end;

function fill_Polygon(dc: XCanvas; P,Code, pc,fc: int): Integer;

function fill_DPolyBuf(const R: LVector): Integer;
begin
  Result:=0;

  if (R[0].X < map_lt.X) or (R[1].X > map_rb.X)
  or (R[0].Y < map_lt.Y) or (R[1].Y > map_rb.Y) then

    Result:=clip_master.plot_Polygon(x_Edit.DPolyBuf)

  else

    with x_Edit.DPolyBuf^ do
    Result:=fpoly.Add_LPoly(@Pol,N);

  Inc(fDrawCount,Result)
end;

function fill_lp(dc: XCanvas;
                 lp: PLLine; pc,fc: int;
                 const R: LVector): Integer;
begin
  Result:=0;

  if (R[0].X < map_lt.X) or (R[1].X > map_rb.X)
  or (R[0].Y < map_lt.Y) or (R[1].Y > map_rb.Y) then
  begin
    clip_master.plot_begin(dc,3,pc,fc,0);
    Result:=clip_master.plot_Polyline(@lp.Pol,lp.N);
  end else

  with lp^ do begin
    fctx.FillPoly(dc,@Pol,nil,N,pc,fc);
    Result:=N+1
  end;

  if Result > 0 then begin
    if fx_Mark then
    x_Mark_lp(dc,lp,false);

    Inc(fDrawCount,Result);
  end
end;

function fill_childs(par,up_code: Integer;
                     mark: Boolean): Boolean;
var
  top,run: int; node: dm_Node; R: LVector;
begin
  Result:=false;

  with Globe.Map,x_Edit do
  if run <> Tree.Root then begin

    top:=Tree._Child(par);
    if top > 0 then begin

      run:=Tree._Link(top);
      while run <> top do begin

        DPolyBuf.N:=-1;

        if Get_Node(run,node) in [3,5] then begin
        
          with node.dRec do
          if (ox1 < v_rb.X) and (ox2 > v_lt.X) then
          if (oy1 < v_rb.Y) and (oy2 > v_lt.Y) then

          if (Code = up_Code)
          or is_Hole_Code(Code) then begin

            case Tag of
          3:  Load_DPolyBuf(run,mark, R[0],R[1]);

          5:  if Get_DPolyBuf(run) > 0 then begin
                Ellipse_LLine(DpolyBuf,LPolyMax,0.7);
                with DPolyBuf^ do
                Max_Poly_Bound(@Pol,N, R[0],R[1]);
              end
            end;

            if DPolyBuf.N > 0 then
            if PolyLock(DpolyBuf) then

            if mark then
              x_Mark_DPolyBuf(dc,false)
            else
            if fill_DPolyBuf(R) > 0 then
              pickup_only(run,1,node.dRec)
          end
          else Result:=true
        end
        else Result:=true;

        if run = node.Link then Break;
        run:=node.Link;
      end
    end
  end
end;

var
  R: LVector;
begin
  Result:=-1;

  clip_master.plot_begin(dc,3,pc,fc,0);

  if Load_DPolyBuf(P,false, R[0],R[1]) > 0 then begin

    with Globe.Map,x_Edit do
    if fpoly.Active then begin

      fctx.x_Start(dc,pc,fc,false);

      if fill_DPolyBuf(R) > 0 then begin

        if not fx_all then
        if fx_Disp.AreaFill then
        if (R[1].X-R[0].X < fpoly_min1)
        or (R[1].Y-R[0].Y < fpoly_min1) then
        dc.Poly:=true;

        fill_childs(P,Code,false);
        fctx.x_Close;

        if (fc and x_gdi) = 0 then
          After_disp(dc,P,Code,3)
        else
          After_ivg(P,3);

        dc.Poly:=false;

        if fx_Mark then begin
          Load_Mark_DPolyBuf(dc,P,false);
          fill_childs(P,Code,true)
        end;

        Result:=Code
      end
    end else

    if fill_lp(dc,x_Edit.DPolyBuf,pc,fc,R) > 0 then
    After_disp(dc,P,Code,3);

  end else

  if fpoly.Active then
  with Globe.Map,fpoly do

  if Get_Poly(P,x_LPoints,Points_Max) > 0 then
  if Project_lp(x_LPoints) > 0 then begin

    R[0]:=Node.dRec.o_lt;
    R[1]:=Node.dRec.o_rb;

    if fill_lp(dc,x_LPoints,pc,fc,R) > 0 then
    After_disp(dc,P,Code,3)
  end
end;

function disp_Text(dc: XCanvas; ptr,code,cl: int): Boolean;

procedure text_DPolyBuf(ptr: int; s: PWideChar; h,k: Double; cl: int);
const
  _dxy: Array[0..7,0..1] of int =
    ((-1,0),(-1,-1),(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1));
var
  pen: TDrawCurve;
  up,ft,pw,pc,fc,fig,r: int;
  i,x1,y1,x2,y2,x3,y3,dx,dy: int;
  dp,lp: PLLine; a,b,c: TPoint;
  v: VLLine; clip: longbool;
begin
  dp:=x_Edit.DPolyBuf;

  if h >= Fonts.MinH then begin

    lp:=dp;

    up:=tlong(cl).b[2] and $3F;
    if up shr 2 = 7 then
    if lp.N > 0 then begin

      c:=lp.Pol[lp.N];

      pc:=0; fig:=0;
      if Globe.Map.xGet_Int(ptr,999,ft) then begin

        pw:=(ft shr 10) and 7; // 

        pc:=(pw shl 8) + ((ft shr 5) and 31);
        tlong(fc).w[1]:=pw;
        tlong(fc).w[0]:=ft and $3FF;

        fig:=(ft shr 13) and 7;
      end;

      with Node.dRec do
      clip:=(ox1 < map_lt.X) or (ox2 > map_rb.X) or
            (oy1 < map_lt.Y) or (oy2 > map_rb.Y);

      if pw > 0 then
      if not clip then
        fctx.DrawPoly(dc,@dp.Pol,dp.N,pc,k)
      else begin
        clip_master.plot_begin(dc,2,pc,0,k);
        clip_master.plot_polyline(@dp.Pol,dp.N)
      end;

      if fig > 0 then begin
        r:=Round( ((ft shr 16) and $FFF)/100*k);

        if r > 0 then
        case fig of
      1:  begin
            v.N:=1; v.Pol[0]:=c;
            v.Pol[1]:=c; Inc(v.Pol[1].X,r);

            pen:=clip_master.beginCurve(dc,3,0,fc,0,LPolyMax);
            try
              pen.Draw_Ellipse(@v.Pol,v.N)
            finally
              pen.Free;
            end;
          end;

      2,
      3:  begin
            x1:=c.X-r; y1:=c.Y-r;
            x2:=c.X+r; y2:=c.Y+r;

            if fig = 2 then begin
              v.Pol[0]:=Point(x1,y1);
              v.Pol[1]:=Point(x2,y1);
              v.Pol[2]:=Point(x2,y2);
              v.Pol[3]:=Point(x1,y2);
            end
            else begin
              x3:=(x1+x2) div 2;
              y3:=(y1+y2) div 2;

              v.Pol[0]:=Point(x1,y3);
              v.Pol[1]:=Point(x3,y1);
              v.Pol[2]:=Point(x2,y3);
              v.Pol[3]:=Point(x3,y2);
            end;

            v.Pol[4]:=v.Pol[0]; v.N:=4;

            if not clip then
              fctx.FillPoly(dc,@v.Pol,nil,v.N,pc,fc)
            else begin
              clip_master.plot_begin(dc,3,pc,fc,k);
              clip_master.plot_polyline(@v.Pol,v.N)
            end
          end

        end
      end;

      up:=(5 shl 2) or 1; // 
      cl:=(cl and $FFC0FFFF) or (up shl 16);
      v.N:=0; v.Pol[0]:=c; lp:=@v;
    end;

    if fdc_Mode > x_Draw then
    cl:=(cl and $FFFFFFF0) or (fMarkColor and 15);

    fc:=Globe.Params.blankText-1;

    if fc >= 0 then begin

      if fdc_Mode > x_Draw then
      fc:=fMarkColor and 15;

      fc:=(cl and $FFFFFFF0) or fc;

      for i:=0 to 7 do begin
        dx:=_dxy[i,0]; dy:=_dxy[i,1];
        move_poly(lp,dx,dy);
        Fonts.Out_lWide(dc,lp,fc,k, s);
        move_poly(lp,-dx,-dy);
      end
    end;

    Fonts.Out_lWide(dc,lp,cl,k, s)
  end else
  if x_Disp_Sign then begin
    cl:=cl and 15;
    if fdc_Mode > x_Draw then
    cl:=fMarkColor;

    fctx.DrawPoly(dc,@dp.Pol,dp.N,cl,0)
  end
end;

var
  up,ansi,ay: int;
  h: Float; k: Double; a,b: TPoint;
  sp: PObjScaleRec; s: TWideStr;
begin
  Result:=false;

  if Node.dRec.View <= dm_View then begin

    a:=Node.dRec.o_lt;
    b:=Node.dRec.o_rb;

    sp:=ScaleList.xIndexof(Code,4);
    if (sp = nil) or (sp.scale = 0)
    or (sp.scale+1 >= cd_scale) then begin

      k:=mm_k; if Assigned(sp) then
      k:=Fonts.Scale_Text(cl,k,
                          cd_dpmm * sp.sz1 / 10,
                          cd_dpmm * sp.sz2 / 10);

      Fonts.Display_Text1(cl,k,h,ansi);

      if h < 1 then begin

        if x_Disp_Sign then
        if (a.X < v_rb.X) and (b.X > v_lt.X) then
        if (a.Y < v_rb.Y) and (b.Y > v_lt.Y) then

        if Get_DPolyBuf(ptr) >= 0 then
        if Globe.Map.xDraw_Text(ptr,ansi,s) <> nil then begin
          text_DPolyBuf(ptr,s,h,k,cl); Result:=true
        end

      end else

      if (h > 0) or fx_all then

      if (a.X < s_rb.X) and (a.Y < s_rb.Y) and
         (b.X > s_lt.X) and (b.Y > s_lt.Y) then begin

        if Get_DPolyBuf(ptr) >= 0 then
        if Globe.Map.xDraw_Text(ptr,ansi,s) <> nil then begin
          text_DPolyBuf(ptr,s,h,k,cl); Result:=true
        end

      end else

      if (a.X < t_rb.X) and (a.Y < t_rb.Y) and
         (b.X > t_lt.X) and (b.Y > t_lt.Y) then

      if Globe.Map.Get_Poly_Cnt(ptr) <= 1 then
      if Globe.Map.xDraw_Text(ptr,ansi,s) <> nil then begin

        Project(a.X,a.Y,a);
        b.X:=a.X+Round(Fonts.xWidth_Text(h,s));

        up:=tlong(cl).w[0] and 3;
        if up = 0 then begin
          b.Y:=a.Y; a.Y:=b.Y-Round(h);
        end else
        if up = 1 then begin
          ay:=a.Y;
          a.Y:=ay-Round(h/2);
          b.Y:=ay+Round(h/2);
        end
        else b.Y:=a.Y+Round(h);

        if (a.X < win_rb.X) and (b.X > win_lt.X) then
        if (a.Y < win_rb.Y) and (b.Y > win_lt.Y) then

        if Get_DPolyBuf(ptr) >= 0 then begin
          text_DPolyBuf(ptr,s,h,k,cl); Result:=true
        end
      end
    end
  end
end;

function draw_Sign(dc: XCanvas; ptr,code: int;
                   lp,vp: PLLine; cl: int): bool;
var
  fon,fon1,len,r,h: int; k: Double;
  sp: PObjScaleRec; v: VLLine; p: TPoint;
  rc: bool;
begin
  Result:=false;

  if lp.N >= 0 then begin

    v.N:=Min(1,lp.N);
    len:=(v.N+1) * SizeOf(TPoint);
    Move(lp.Pol,v.Pol,len);

    if v.N = 0 then
      v.Pol[1]:=v.Pol[0]
    else
    if v.N = 1 then begin

      if Assigned(vp) then
      if vp.N = 0 then v.Pol[1]:=vp.Pol[0];

      if cl and $10000 <> 0 then

      if cl and $20000 <> 0 then begin
        v.N:=0; v.Pol[0]:=v.Pol[1]
      end
      else begin
        Mirror_Point(v.Pol[0],v.Pol[1],v.Pol[2]);
        v.Pol[0]:=v.Pol[1]; v.Pol[1]:=v.Pol[2]
      end
    end;

    fon:=0; fon1:=0; rc:=false;

    if fdc_Mode > x_Draw then begin
      fon:=fMarkColor; Vgm.IsColor:=true;
      fon1:=fon
    end;

    if cl and $8000 = 0 then

      if cl and $800000 <> 0 then begin
        if cl and $400000 <> 0 then
          ivg_draw_Sign(dc,v.Pol[0],v.Pol[1],cl,code)
        else
          dmx_Sign(dc,v.Pol[0],v.Pol[1],cl,fon1);
        Inc(fDrawCount); Result:=true
      end else
      with v.Pol[0] do begin
        k:=1; if pgm_Scale > 0 then k:=pgm_Scale;
        if not Pgm.Draw_Pgm(dc,k,X,Y,fGround,fon,cl) then
        if fdc_Mode in [x_Mark,x_Group] then begin
          dc.xPen(2,fon); r:=6;
          if fdc_Mode = x_Group then r:=3;
          dc.Frame(X-r,Y-r,X+r+1,Y+r+1);
        end; Inc(fDrawCount); Result:=true
      end

    else begin
      sp:=nil;
      if cl and $800000 = 0 then
      sp:=ScaleList.xIndexof(code,1);

      if fx_all
      or (sp = nil) or (sp.scale = 0)
      or (sp.scale+1 >= cd_scale) then begin

        h:=0;
        if cl and $800000 <> 0 then
        h:=code and $FFF;

        if h > 0 then
          rc:=Vgm.Out_vgm(dc,@v,cl and $7FFF,fon,-h)
        else begin
          k:=mm_k; if Assigned(sp) then
          k:=Vgm.Vgm_disp(cl, k,
                          cd_dpmm * sp.sz1 / 10,
                          cd_dpmm * sp.sz2 / 10);

          rc:=Vgm.Vgm_Sign(dc,@v,k, 0,fon,cl,0)
              or Display_model(ptr)
        end;

        if rc then begin
          Inc(fDrawCount); Result:=true
        end else
        if fx_all then begin
          Vgm.Vgm_Mark(DC,@v,fon,cl);
          Inc(fDrawCount); Result:=true
        end else
        if x_Disp_Sign then Result:=true;
      end
    end;

    if Result then

    if fx_Mark then begin

      if lp.N = 1 then begin
        Out_Polyline(dc,lp,fon);
        x_Mark_DPolyBuf(dc,false)
      end else

      x_Mark_DPolyBuf(dc,lp.N = 3);

      if fx_Disp1 and tx_opt_mark <> 0 then
      if fdc_Mode = x_Mark then begin
        p:=lp.Pol[0];
        if dc.Contains_sign(p) then begin
          dc.Canvas.SetBrush(-1,clWHITE);

          dc.rgb_Pen(fctx.pen_Width,
                     fctx.rgb_Active_Color);

          r:=Max(8,Round(dc.ppmm*4));
          dc.Ellipse(p.X-r,p.Y-r,p.X+r,p.Y+r);

          dc.Canvas.SetBrush(0,clWHITE)
        end
      end
    end;

    Vgm.IsColor:=false
  end
end;

function draw_Signs(dc: XCanvas; p,code: Longint;
                    lp: PLLine; cl: Integer): Boolean;
var
  i: int; v: VLLine; smark: bool;
begin
  Result:=false;

  if x_Edit.pmov_i > 0 then begin

    i:=x_Edit.pmov_i-1;
    if i <= lp.N then begin
      v.N:=0; v.Pol[0]:=lp.Pol[i];
      Result:=draw_Sign(dc,p,code,@v,nil,cl)
    end

  end
  else begin
    smark:=fx_Mark;
    fx_Mark:=false;

    for i:=0 to lp.N do begin
      v.N:=0; v.Pol[0]:=lp.Pol[i];
      if draw_Sign(dc,p,code,@v,nil,cl) then
      Result:=true
    end;

    if smark then begin
      fctx.MarkPoly(dc,nil,-1,0,false);

      if fdc_Mode > x_Draw then
      dc.xPen(1,fMarkColor);

      with x_Edit.DPolybuf^ do
      for i:=0 to N do with Pol[i] do

      if (x >= -8) and (x <= dc.Width+8) and
         (y >= -8) and (y <= dc.Height+8) then begin
        dc.Rectangle(x-1,y-1,x+1,y+1);
        Result:=true
      end;

      fx_Mark:=fx_Disp.Mark
    end
  end
end;

function draw_Curve(dc: XCanvas; p,loc,pc,fc: int): Boolean;

procedure curve_vector(dc: XCanvas; lp: PLLine; ind: int);

procedure draw_vector(dc: XCanvas; const p1,p2: TPoint; cl: int);
begin
  if not Points_equal(p1,p2) then begin
    dc.rgb_xPen(1,ord(psDOT),cl);
    dc.SetBkMode_TRANSPARENT;
    dc.Draw_Line(p1,p2);
    dc.rgb_xPen(1,0,0)
  end;
end;

var
  p1,p2,p3,q1,q2: TPoint; cl: Integer;
begin
  cl:=fctx.rgb_Mark_Color;

  with lp^ do
  if (Ind >= 0) and (Ind < N) then begin

    p1:=Pol[Ind]; p2:=Pol[Ind+1]; q1:=p1; q2:=p1;

    if Ind <= x_Edit.lp_loc then q1:=p2;

    if Ind - x_Edit.lp_loc >= -1 then begin
      Mirror_Point(p2,p1,p3); q2:=p3
    end;

    draw_vector(dc,q1,q2,cl);
    dc.ResetCanvas(clBlack,cl);

    if not Points_equal(q1,p1) then
    dc.RectPoint(q1.X,q1.Y,2);

    if not Points_equal(q2,p1) then
    dc.RectPoint(q2.X,q2.Y,2);
  end else
  if Ind < 0 then
  if CurveLock(lp) then begin
    Ind:=N-1-2; p1:=Pol[Ind]; p2:=Pol[Ind+1];
    draw_vector(dc,p1,p2,cl);

    dc.ResetCanvas(clBlack,cl);

    if not Points_equal(p1,p2) then
    dc.RectPoint(p2.X,p2.Y,2);
  end
end;

var
  pen: TDrawCurve;
  lp: PLLine; ind: Integer;
begin
  Result:=false;

  if Get_DPolyBuf(p) >= dm_min[loc] then begin

    if loc = 6 then pc:=fc;

    lp:=x_Edit.DPolyBuf;
    pen:=clip_master.beginCurve(dc,loc-4,pc,fc,mm_k,LPolyMax);
    try
      if pen.Draw_lcurve(@lp.Pol,lp.N) > 0 then begin

        if fx_Mark then begin

          if dc <> Cash then
          if p = Globe.Map.Occupe_a then begin
            ind:=x_Edit.lp_loc;
            if Odd(ind) then Dec(ind);
            curve_vector(dc,lp,ind-2);
            curve_vector(dc,lp,ind);
            curve_vector(dc,lp,ind+2);
          end;

          x_Mark_DPolyBuf(dc,true)
        end;

        Inc(fDrawCount,lp.N+1);
        Result:=true
      end;
    finally
      pen.Free;
    end
  end
end;

function draw_Ellipse(dc: XCanvas; p,pc,fc: int): Boolean;
var
  alf,loc: int; pen: TDrawCurve; lp: PLLine;
begin
  Result:=false;

  if Get_DPolyBuf(p) > 0 then begin

    loc:=3;
    if fdc_Mode in [x_Mark,x_Undo] then loc:=2;

    lp:=x_Edit.DPolyBuf;

    if fc and x_gdi <> 0 then
    if Globe.Map.xGet_Int(p,1001,alf) then
    dc.SetAlfaCenter(@lp.Pol[0],@lp.Pol[1],Round(alf/100*255));

    pen:=clip_master.beginCurve(dc,loc,pc,fc,0,LPolyMax);
    try
      if pen.Draw_Ellipse(@lp.Pol,lp.N) > 0 then begin

        if fx_Mark then
        x_Mark_DPolyBuf(dc,false);

        Inc(fDrawCount,lp.N+1);
        Result:=true
      end
    finally
      pen.Free;
    end;

    dc.SetAlfaCenter(nil,nil,-1);
  end
end;

function draw_Sector(dc: XCanvas; p,pc,fc: int): int;
var
  lp: PLLine; lt,rb: TPoint; alf: int;
begin
  Result:=0;

  if Get_DPolyBufb(p,lt,rb) = 2 then begin

    lp:=x_Edit.DPolyBuf;

    if fc and x_gdi <> 0 then
    if Globe.Map.xGet_Int(p,1001,alf) then
    dc.SetAlfaCenter(@lp.Pol[0],@lp.Pol[1],Round(alf/100*255));

    lp.N:=LPoly_Sectorp(@lp.Pol,lp.N);

    if lp.N > 0 then
    if (lt.X < map_lt.X) or (rb.X > map_rb.X)
    or (lt.Y < map_lt.Y) or (rb.Y > map_rb.Y) then
    begin
      clip_master.plot_begin(dc,3,pc,fc,0);
      Result:=clip_master.plot_Polyline(@lp.Pol,lp.N);
    end else

    with lp^ do begin
      fctx.FillPoly(dc,@Pol,nil,N,pc,fc);
      Result:=N+1
    end;

    if Result > 0 then begin
      if fx_Mark then
      x_Mark_lp(dc,lp,false);

      After_ivg(p,8);
      Inc(fDrawCount,Result)
    end;

    dc.SetAlfaCenter(nil,nil,-1);
  end
end;

function cn_draw(dc: XCanvas; p,loc,cl: Integer): Integer;
var
  lp: PLPoly; cp: PIntegers;
  i,n,lp_n,thick: int;
  mf: tmf_buff;
begin
  Fillchar(mf,SizeOf(mf),0);

  mf.lp:=x_Edit.DPolyBuf;
  mf.hp:=x_Edit.DHeights;
  mf.lp_Max:=LPolyMax;

  Result:=Globe.Map.cn_xdraw(p,fDrawBuf,@mf,
                             prj_Vertex,mf_Project);

  if Result > 0 then begin

    lp:=fDrawbuf.First;
    cp:=fDrawbuf.Parts.First;
    n:=fDrawbuf.PartCount;

    dc.Clip:=clip_poly(lp,fDrawbuf.Count);

    if loc = 23 then begin

      dc.Poly:=true;

      if fdc_Mode = x_Draw then
        dmx_lp_fill(dc,lp,cp,n,0,cl,Vgm)
      else
        fctx.FillPoly(dc,lp,cp,n,0,cl);

      dc.Poly:=false;

      thick:=succ(tlong(cl).b[2] and 3);
      if dc.Thin then thick:=1;

      cl:=((cl shr 5) and 31) or (thick shl 8)
    end;

    for i:=1 to n do begin
      lp_n:=cp[0]-1; cp:=@cp[1];

      if dc.Clip then begin
        clip_master.plot_begin(dc,2,cl,0,mm_k);
        clip_master.plot_polyline(lp,lp_n)
      end
      else fctx.DrawPoly(dc,lp,lp_n,cl,mm_k);

      lp:=@lp[lp_n+1]
    end;

    dc.Clip:=false
  end
end;

function Get_draw_Node(p: longint): Integer;
var
  tmp: dm_Node;
begin
  Result:=-1; with Globe.Map do

  if p <= SF.vm_Ind-SizeOf(tmp) then begin
    Get_Node(p,tmp);

    if not is_dm then begin
      Node:=tmp;
      Result:=tmp.dRec.Tag
    end else

    if tmp.Link > 0 then
    if tmp.Len = SizeOf(dm_Rec) then begin
      Node:=tmp;
      Result:=tmp.dRec.Tag
    end
  end
end;

function draw_mesh(dc: XCanvas; p,cl: Integer): Boolean;
const
  vmax = 2048;
  cmax = 1024;

var
  i,j1,j2,_j1,_j2,k,vn,fn,np,nc: Integer;
  fp,cp,cp1: PIntegers; lp,bp,bp1,bp2: PLPoly;
  vp: PVPoly;
begin
  Result:=false; fDrawbuf.Clear;

  if Globe.Map.Get_mesh(p,fmesh) then

  if fDrawbuf.Resize(vmax) then
  if fDrawbuf.Parts.Resize(cmax) then begin

    cl:=cl and 31; cl:=cl or (cl shl 5);

    bp:=fDrawbuf.First;
    cp:=fDrawbuf.Parts.First;

    bp1:=bp; np:=0; cp1:=cp; nc:=0;

    vp:=fmesh.Points.First;
    vn:=fmesh.Points.Count;

    fp:=fmesh.Faces.First;
    fn:=fmesh.Faces.Count;

    lp:=Pointer(vp);
    for i:=0 to vn-1 do with vp[i] do
    Project(x,y,lp[i]);

    i:=0;
    while i < fn do begin
      j1:=fp[i]; _j1:=j1;
      j1:=j1 and $FFFFFF; Inc(i);

      if _j1 and mesh_brk = 0 then begin

        bp2:=bp1;
        bp1[0]:=lp[j1]; bp1:=@bp1[1];
        Inc(np); k:=1;

        while i < fn do begin
          j2:=fp[i]; _j2:=j2;
          j2:=j2 and $FFFFFF; Inc(i);

          if np < vmax-5 then begin
            bp1[0]:=lp[j2]; bp1:=@bp1[1];
            Inc(np); Inc(k)
          end;

          if _j2 and mesh_brk <> 0 then Break
        end;

        if (k < 3) or (np >= vmax) then begin
          bp1:=bp2; Dec(np,k)
        end
        else begin
          bp1[0]:=bp2[0]; bp1:=@bp1[1]; Inc(np);
          cp1[0]:=k+1; cp1:=@cp1[1]; Inc(nc);
        end
      end;

      if (np > vmax-20)
      or (nc > cmax-10) then begin
        dc.Clip:=clip_poly(bp,np);
        dc.dmwPolygon(bp,cp,nc,cl);
        bp1:=bp; np:=0; cp1:=cp; nc:=0;
      end
    end;

    if nc > 0 then begin
      dc.Clip:=clip_poly(bp,np);
      dc.dmwPolygon(bp,cp,nc,cl);
      bp1:=bp; np:=0; cp1:=cp; nc:=0;
    end;

    dc.Clip:=false; Result:=true
  end;
end;

function draw_hg(dc: XCanvas; p,cl: int): Boolean;
var
  i,k,r1,r2,typ,ptyp,alfa: int;
  lp: PLLine; c,a,b: TPoint;
  iv,ic: IValues32; eps: Double;
  s: TShortstr;
begin
  Result:=false;

  with Node.dRec do
  if View <= dm_View then begin

    typ:=Code;

    k:=Globe.Map.Get_hg(p,@iv,@ic,r1,r2,s);
    kz:=cd_kz;

    if k > 0 then
    if typ in [1..2] then begin

      c:=o_lt;
      Project(c.X,c.Y, c);

      r1:=Round(r1*cd_kz)+r2;

      if r1 > 0 then begin

        a:=c; b:=c; r2:=r1 div 2;

        if typ = 1 then begin
          Dec(a.X,r1); Inc(b.X,r1);
          Dec(a.Y,r1); Inc(b.Y,r1)
        end
        else begin
          Dec(a.Y,r1);
          Dec(a.X,r2); Inc(b.X,r2);
        end;

        if (a.X < win_rb.X) and (b.X > win_lt.X) then
        if (a.Y < win_rb.Y) and (b.Y > win_lt.Y) then

        if dc = nil then
          Result:=true
        else begin

          eps:=-1;
          if fdc_Mode = x_Mark then begin
            Inc(r1,6); eps:=0.7;
            Dec(a.X,6); Inc(b.X,6);
            Dec(a.Y,6); Inc(b.Y,6);
          end else
          if fdc_Mode = x_HIDE then begin
            for i:=0 to k-1 do
            ic[i]:=fctx.rgb_Ground_Color;

            cl:=hg_unpack_color(cl, alfa,ptyp);
            cl:=hg_pack_color(0,0,cl)
          end;

          lp:=x_Edit.DPolyBuf;
          if typ = 1 then
            lp.N:=CircleToLine(@lp.Pol,c.X,c.Y,r1,eps)
          else
            lp.N:=Bound_to_LPoly(a,b, @lp.Pol);

          if fdc_Mode = x_Mark then
            Clip_PolyBuf(dc,lp,2,cl)  
          else
          if ivg_draw_hg(dc,
                         c.X,c.Y, typ,cl,
                         @iv,@ic,k,r1) then begin

            if fHints.Enabled then
            if s[0] <> #0 then begin

              for i:=0 to lp.N do
              with lp.Pol[i] do
              Backup(X,Y,lp.Pol[i]);

              fHints.AddPoly(p,3,@lp.Pol,lp.N,s)
            end;

            pickup_draw_node(p);

            Result:=true
          end
        end
      end
    end
  end
end;

function draw_hgt(xdc: XCanvas; p,code,cl: Integer): Boolean;
begin
  Result:=false;
end;

var
  cl,cod,loc,ic,t: Integer;
  sp: PObjScaleRec; k: Double;
  dn,fl: Boolean;
begin
  Result:=0; fdraw_lev:=p_lev;

  if p > 0 then
  if Get_Draw_Node(p) >= 0 then begin

    loc:=Node.dRec.Tag;

    if dc = nil then begin

      case loc mod 100 of
    1:  if Display_Sign then Result:=p;

    2,
    3,
    4,
    5,
    6,
    7,
    8,

   11,
   21,
   22,
   23:  if Display_Poly(false) then Result:=p;

   loc_hg:
        if draw_hg(nil,p,0) then Result:=p;
      end;

      fdraw_node:=Node
    end
    else begin
      with Node.dRec do

      if (loc < loc_hg)
      or Display_Object(Code) then

      if (Node.Flags and fl_draw = 0)
      or (fdc_Mode <> x_Draw) or fx_all

      then begin cod:=-1;

        dn:=false; if loc <> 0 then
        dn:=(fx_Disp1 and tx_opt_Childs1) = 0;

        tlong(cl).w[1]:=Node.Tag;
        tlong(cl).w[0]:=Color;
   
        case loc of
      0:  if Display_Layer(p) then begin
            dn:=true; if Pickup <> nil then
            Pickup.IsEnabled:=Node.Flags and fl_long = 0;
          end;

      1:  if Display_Sign then
          if Get_DPolyBufv(p) >= 0 then
          if draw_Sign(dc,p,Code,
                       x_Edit.DPolybuf,
                       x_Edit.DPolybuf1,
                       cl) then begin

            if (cl and $800000) = 0 then // ivg
              After_disp(dc,p,Code,1)
            else
            if cl and $8000 <> 0 then
              after_ivg1(p,Code,cl);

            dn:=true
          end;

      2:  if Display_Poly(false) then begin

            sp:=nil; if not fx_all then
            sp:=fScaleList.xIndexof(Code,2);

            if (sp = nil) or (sp.scale = 0)
            or (sp.scale+1 >= cd_scale) then begin

              k:=mm_k; if Assigned(sp) then
              k:=dmx_line_scale(cl,k,
                                cd_dpmm * sp.sz1 / 10,
                                cd_dpmm * sp.sz2 / 10);

              ic:=Draw_DPolyBuf(dc,p,cl,k);
              if ic > 0 then begin
                Inc(fDrawCount,ic);

                if (cl and $80000) = 0 then
                  After_disp(dc,p, Code,2)
                else begin
                  fl:=fx_Disp.AttrDisp;
                  fx_Disp.AttrDisp:=true;
                  After_disp(dc,p, Code,2);
                  fx_Disp.AttrDisp:=fl
                end;

                dn:=true
              end
            end

          end;
 
      3:  begin
            cod:=Code; if (p_Code = -1)
            or ((cod <> 0) and (cod <> p_code)) then

            if Display_Poly(false) then begin
              if mind = 0 then cod:=0 else
              cod:=Fill_Polygon(dc,p,cod,0,cl);
              dn:=true
            end
          end;
 
      4:  if Disp_Text(dc,p,Code,cl) then begin
            Inc(fDrawCount,x_Edit.DPolyBuf.N+1);
            pickup_draw_node(p); dn:=true;

            if fx_Mark then
            Draw_DPolyBuf(dc,p,0,0)
          end;

       5: if Display_Poly(false) then
          if Draw_Ellipse(dc,p,cl,cl) then begin
            After_disp(dc,p,Code,3); dn:=true
          end;

      6,
      7:  if Display_Poly(false) then
          if Draw_Curve(dc,p,Tag,cl,cl) then begin
            After_disp(dc,p,Code,Tag-4); dn:=true
          end;

      8:  if Display_Poly(false) then
          dn:=draw_Sector(dc,p,Code,cl) > 0;

      11,
      21: if Display_Poly(true) then
          if Get_DPolyBuf(p) >= 0 then
          if draw_Signs(dc,p,Code,x_Edit.DPolybuf,cl) then begin

            if x_Edit.DPolyBuf.N = 0 then
              After_disp(dc,p,Code,1)
            else
              After_disp(dc,p,Code,11);

            dn:=true
          end;

      22,
      23: if Display_Poly(false) then
          if cn_draw(dc,p,Tag,cl) > 0 then begin
            pickup_draw_node(p); dn:=true
          end;

      33: if Display_Poly(false) then
          if draw_mesh(dc,p,cl) then begin
            pickup_draw_node(p); dn:=true
          end;

      loc_hg:
          begin
            draw_hg(dc,p,cl); dn:=true
          end;

      102:if Display_Poly(false) then begin
            ic:=draw_DPolyBuf(dc,p,cl or x_gdi,1);
            if ic > 0 then begin
              Inc(fDrawCount,ic);
              After_ivg(p,2); dn:=true
            end
          end;

      103:if Display_Poly(false) then begin
            cod:=Fill_Polygon(dc,p,3,Code,cl or x_gdi);
            dn:=true
          end;

      104:if draw_hgt(dc,p,Code,cl) then begin;
            Inc(fDrawCount,x_Edit.DPolyBuf.N+1);
            pickup_draw_node(p); dn:=true;

            if fx_Mark then
            Draw_DPolyBuf(dc,p,0,0)
          end;

      105:if Display_Poly(false) then
          if Draw_Ellipse(dc,p,Code,cl or x_gdi) then begin
            After_ivg(p,5); dn:=true
          end;

      106,
      107:if Display_Poly(false) then
          if Draw_Curve(dc,p,Tag-100,Code,cl or x_gdi) then begin
            After_ivg(p,Tag-100); dn:=true
          end;

      108:if Display_Poly(false) then
          dn:=draw_Sector(dc,p,Code,cl or x_gdi) > 0;
          
        end;

        if fDrawCount >= 4096 then begin
          t:=GetTickCount;
          if t - fDrawTick > 1000 then begin
            if not isAnimate then
            if isCash then Cash_to_Card;
            if msg_Escape then IsDraw:=false;
            fDrawCount:=0; fDrawTick:=t;
          end
        end;

        if dn then
        if (p > 0) and (p_lev <= 32) then
        Draw_Objects(dc,p,p_lev,cod)
      end;

      Result:=Node.Link
    end
  end
end;

function TDrawCard.Draw_attr(dc: XCanvas; p,p_code,p_loc: Integer): Boolean;

procedure shift_lp(lp: PLLine; dx,dy: Float);
var
  i,_dx,_dy: Integer;
begin
  _dx:=Round(dx);
  _dy:=Round(dy);

  for i:=0 to lp.N do
  with lp.Pol[i] do begin
    Inc(X,_dx); Dec(Y,_dy)
  end
end;

function Load_DPolyBuf(p: Integer): Integer;
var
  lt,rb: TPoint;
begin
  Result:=-1; with x_Edit,Globe.Map do
  if xGet_Poly(p,DPolyBuf,DHeights,LPolyMax) >= 0 then
  Result:=mf_Project(DPolyBuf,DHeights,lt,rb,true,false)
end;

function Objects_Equal(code1,loc1, code2,loc2: int): bool;
var
  i: int;
begin
  Result:=false;

  if (loc1 = 0) or (loc1 = Transit_Loc(loc2)) then

  if code1 = code2 then
    Result:=true
  else
  if loc1 = 0 then begin
    if code1 mod 100 = 0 then begin
      code1:=code1 div 100;
      code2:=code2 div 100;

      for i:=1 to 5 do
      if code1 mod 10 = 0 then begin
        code1:=code1 div 10;
        code2:=code2 div 10;
      end
    end;

    Result:=code1 = code2
  end
end;

var
  i,j,cl,iv,fc,tc,ansi: int;
  lp: PLLine; hp: PIntegers;
  k,z,fi,df,dr,dc_k: Double;
  R: TDispAttr; p1,p2: TPoint; sz: TSize;
  g1,g2: TGauss; th,h1,h2,dx,dy,ds: Float;
  v: VLLine; pc: PChar; s: TWideStr; 
begin
  Result:=false;

  if p > 0 then
  if fAttrList.Enabled then
    fAttrList.AddItem(p,p_code,p_loc)
  else
    p:=-p;

  if p < 0 then begin p:=-p;

    dc_k:=1/1000/dc.MetersPerPixel;
    fc:=Fonts.xBlankColor;

    lp:=x_Edit.DPolyBuf;

    with Globe.Map,fDispAttrs do
    for i:=0 to Count-1 do begin

      R:=PDispAttr(Items[i])^;

      if R.Flags and attr_Active <> 0 then
      if Objects_Equal(R.Code,R.Loc, p_code,p_loc) then

      if R.typ = 3 then begin

        pc:=@s;
        if xGet_Poly(p,lp,nil,LPolyMax) >= 0 then
        if Hint_hf(p, R.nn, pc,0) then

        case p_loc of
      1:  fHints.AddSign(p,lp.Pol[0],pc);
      2,
      3:  fHints.AddPoly(p,p_loc,@lp.Pol,lp.N,pc)
        end

      end
      else begin
        k:=dc_k; h1:=0; h2:=0;
        if R.Flags and attr_Scaled <> 0 then begin
          h1:=cd_dpmm * R.size1/10;
          h2:=cd_dpmm * R.size2/10;
          k:=mm_k;
        end;

        cl:=R.Color;

        if fdc_Mode in [x_HIDE,x_UNDO] then
        cl:=(cl and $FFFFF0) or (fMarkColor and 15);

        if tlong(R.die).b[3] = 1 then begin
          with tlong(cl) do b[2]:=b[2] or $40;

          tc:=R.die and $FFFFFF;
          if fdc_Mode = x_Hide then
          tc:=fctx.rgb_Mark_Color;

          Fonts.xBlankColor:=tc
        end;

        case R.typ of
      0:  begin
            if R.Flags and attr_Scaled <> 0 then begin
              th:=Fonts_Height(cl);
              if th > 0.1 then begin
                th:=th*k;
                if th < h1 then begin
                  if R.Flags and attr_House = 0 then
                    k:=k / th * h1
                  else
                    k:=-1
                end else
                if h1 < h2 then
                if th > h2 then
                  k:=k / th * h2
              end
            end;

            if k > 0 then begin
              dx:=R.dx/10 * k;
              dy:=R.dy/10 * k;
              ds:=R.ds/10 * k;

              if Fonts.Display_Text1(cl,k,th,ansi) then
              if Load_DPolyBuf(p) >= 0 then

              if Draw_hf(p, R.nn, id_Tag(R.id), ansi,s) <> nil then begin

                case p_loc of
              1:  begin
                    if R.flags and (attr_cx+attr_cy) <> 0 then begin
                      sz:=Fonts.Text_Extent(cl,k,s);
                      if R.flags and attr_cx <> 0 then dx:=-sz.cx/2;
                      if R.flags and attr_cy <> 0 then dy:=-sz.cy/2;
                    end;

                    v.N:=0; v.Pol[0]:=lp.Pol[0];
                    shift_lp(@v,dx,dy);
                    Fonts.Out_lWide(dc,@v,cl,k, s)
                  end;

              2:  begin
                    with tlong(cl) do b[2]:=b[2] or 1;

                    if true then
                      Fonts.Line_Attr(dc,lp,cl,k,dx,ds,s)
                    else begin
                      clip_master.plot_begin(dc,2,cl,0,k);
                      clip_master.plot_Attr(lp,dx,s)
                    end
                  end;

              3:  begin
                    p1:=Polygon_Centre(lp);
                    Inc(p1.X,Round(R.dx/10 * k));
                    Inc(p1.Y,Round(R.dy/10 * k));

                    v.N:=0; v.Pol[0]:=p1;
                    Fonts.Out_lWide(dc,@v,cl,k, s);
                  end
                end

              end else

              if p_loc in [1,11] then
              if (R.nn = nn_4) or (R.nn = nn_7) then

              if z_axe_Exist(p) then begin

                x_Edit.Xyz_Points:=true;
                hp:=x_Edit.DHeights;
                if Assigned(hp) then
                if xGet_Poly(p,nil,hp,LPolyMax) >= 0 then

                if p_loc = 1 then lp.N:=0;

                for j:=0 to lp.N do begin
                  v.N:=0; v.Pol[0]:=lp.Pol[j];
                  shift_lp(@v,dx,dy);

                  with v.Pol[0] do
                  if dc.Contains_point(X,Y,8,8) then begin
                    z:=hp[j]/z_res;
                    StringToWideChar(xRealToStr(z,0),s,Sizeof(s)-1);
                    Fonts.Out_lWide(dc,@v,cl,k, s)
                  end
                end
              end
            end
          end;

      1:  if xGet_int(p,R.nn,iv) then
          if (R.val = 0) or (iv = R.val) then begin

            if R.Flags and attr_Scaled <> 0 then
            k:=Vgm.Vgm_disp(cl,k,h1,h2);

            dx:=R.dx/10 * k;
            dy:=R.dy/10 * k;
            ds:=R.ds/10 * k;

            Load_DPolyBuf(p);
            if lp.N >= 0 then

            case p_loc of

          1:  begin
                v.N:=0; v.Pol[0]:=lp.Pol[0];
                if lp.N > 0 then begin
                  v.N:=1; v.Pol[1]:=lp.Pol[1];
                end;

                Shift_lp(@v,dx,dy);
                Vgm.Vgm_Sign(dc,@v, k, 0,0,cl,0)
              end;

          2:  Vgm.Vgm_Line(dc, @lp.Pol,lp.N, 0,cl,0,k,dx,dy,ds);

            end
          end;

      2:  if xGet_Double(p,R.nn,fi) then
          if xGet_Double(p,R.nn+1,df) then
          if xGet_Double(p,R.nn+2,dr) then begin

            fi:=fi/180*Pi; df:=df/180*Pi;

            Centre_Object(p, p1);

            LG_t.L_to_G(p1,g1);
            g2:=prj_gauss(g1,dr,fi);
            LG_t.G_to_L(g2,p2);

            if Sector_LLine(lp,p1,p2,df) > 0 then
            if lp_Project(lp) > 0 then

            fctx.FillPoly(dc,@lp.Pol,nil,lp.N,0,cl);
          end;
        end;

        Fonts.xBlankTyp:=0
      end
    end;

    Fonts.xBlankColor:=fc
  end
end;

function TDrawCard.Display_model(p: longint): Boolean;
begin
  Result:=false
end;

function TDrawCard.x_Draw_Object(dc: XCanvas; p: Int64): Boolean;
var
  x: TInt64;
begin
  Result:=false;

  if not Globe.Map.Is_dm then begin
    if dc = nil then Result:=true
  end
  else begin

    x.x:=p;
    if x.cn = 0 then
      Result:=Draw_Object(dc,x.id,0,-1) > 0
    else begin
      if dc <> nil then
      begin_draw_cn(dc);

      if x.cn = cn_node then
        Result:=draw_vc(dc,x.id)
      else
      if x.cn = cn_edge then
        Result:=draw_ve(dc,x.id)
    end

  end
end;

procedure TDrawCard.Stereo_Object(dc: XCanvas; p: Int64);
begin
  if is_Eye then begin
    eye_draw.Begin_Paint(dc,movie_draw,Movie_Flags);
    x_Draw_Object(dc,p); Globe.StereoLeft(false);
    eye_draw.Flags:=movie_Even; x_Draw_Object(dc,p);
    Globe.StereoLeft(true); eye_draw.Enabled:=false
  end else

  x_Draw_Object(dc,p)
end;

procedure TDrawCard.x_dc_ctrl(dc: XCanvas; mode: Integer);
var
  dm: tdm_map; i: int; p: TInt64;
begin
  fGround:=Globe.dm_Ground;
  dc.Ground:=fGround;

  fdc_Mode:=mode;
  fctx.setMode(Globe,mode);
  fMarkColor:=fctx.Get_Mark_Color;

  dc.BeginPaint(nil);
  BeforePaint(dc,nil);

  if mode = X_MARK then
  dc.Thin:=false;

  dm:=Globe.Map;

  for i:=0 to ctrl_List.Count-1 do begin
    p.x:=ctrl_list.Items[i];

    if p.cn = cn_ident then begin
      p.id:=dm.Offset_by_Id(p.id);
      p.cn:=cn_object;
    end;

    isDraw:=p.x <> dm.Tree.Root;

    if p.x > 0 then begin

      if not dm.Is_dm then
      else
      if fdc_Mode = x_Mark then
        Stereo_Object(dc,p.x)
      else begin
        if p.cn = 0 then
        if mode = x_DRAW then
        if Assigned(Pickup) then
        Pickup.fe_Update(p.id);

        x_Draw_Object(dc,p.x)
      end
    end
  end;

  dc.EndPaint; isDraw:=false;

  fctx.setMode(Globe,x_DRAW);
  fdc_Mode:=x_DRAW;
end;

procedure TDrawCard.x_disp_ctrl(mode: Integer);
var
  xdc: XCanvas; p: Int64; v,ox,oy: int;
begin
  xdc:=Cash1;

  if Cash1.Active then
  if ctrl_list.Count > 0 then
  if Prepare_Map_draw then begin

    Escape_Enabled:=false;
    dm_View:=255;
    xdc.Fill:=false;

    v:=fpoly_min; fpoly_min:=-1;

    case mode of
  x_Mark:
      begin
        fx_Mark:=true;
        x_dc_ctrl(xdc,x_MARK);
        fx_Mark:=fx_Disp.Mark;

        p:=ctrl_list[0];
        Mark_Occupe_Point(xdc,p);
      end;

  x_Hide:
      if not Card.Locked then begin
        fx_Mark:=true;
        x_dc_ctrl(xdc,x_UNDO);
        fx_Mark:=false;
      end;

  x_Undo:
      begin
        fx_Mark:=true;
        x_dc_ctrl(xdc,x_UNDO);
        fx_Mark:=fx_Disp.Mark;
        x_dc_ctrl(xdc,x_DRAW)
      end;

  x_Group:
      x_dc_ctrl(xdc,x_Group);
    end;

    fx_Mark:=fx_Disp.Mark;
    if mode <= x_HIDE then begin

      xdc.Fill:=fx_Disp.AreaFill;
      if not Card.Locked then
      x_dc_ctrl(xdc,mode);

      if Cash.Active then begin
        ox:=xPos; xPos:=Cash.v_lt.X;
        oy:=yPos; yPos:=Cash.v_lt.Y;
        Setup_Map_Display(Cash,nil,Globe.ActiveMap);
        Cash.Fill:=fx_Disp.AreaFill;
        x_dc_ctrl(Cash,mode);
        xPos:=ox; yPos:=oy
      end;

      Setup_Edit_Map_Display;
    end;

    if not IsProcess then
    if not Card.Locked then
    xdc_to_Card(xdc);

    done_Signs;
    Escape_Enabled:=true;

    if Right <> nil then begin
      (Right as TDrawCard).Ctrl_list.Assign(ctrl_list);
      (Right as TDrawCard).x_disp_ctrl(mode);
    end;

    fpoly_min:=v; AfterPaint(nil)
  end
end;

procedure TDrawCard.xc_disp_ctrl(mode: Integer);
begin
  x_disp_ctrl(mode); ctrl_List.Clear;
end;

procedure TDrawCard.x_Disp_Object(dm_i: int; ptr: Int64; mode: int);
var
  ox,oy: int;
begin
  if ptr > 0 then
  ctrl_List.Add(ptr);

  if ctrl_list.Count > 0 then begin

    if Cash.Active then begin
      ox:=xPos; xPos:=Cash.v_lt.x;
      oy:=yPos; yPos:=Cash.v_lt.y;
      Setup_Map_Display(Cash,nil,dm_i);
      Cash.Fill:=fx_Disp.AreaFill;
      x_dc_ctrl(Cash,mode);
      xPos:=ox; yPos:=oy
    end;

    if not Card.Locked then
    if Cash1.Active then begin
      Cash1.Fill:=fx_Disp.AreaFill;
      Setup_Map_Display(Cash1,nil,dm_i);
      x_dc_ctrl(Cash1,mode);
      xdc_to_Card(Cash1)
    end
  end;

  ctrl_List.Clear;
end;

procedure TDrawCard.xc_Disp_Object(dm_i: int; ptr: Int64; mode: int);
begin
  if dm_i < Globe.Count then begin
    Globe.Open_dm(dm_i);
    x_Disp_Object(dm_i,ptr,mode);

    Globe.Open_Active_dm;
    Setup_Edit_Map_Display;
    done_Signs
  end
end;

function TDrawCard.Disp_Object(p: Int64; mode: Integer): Integer;
var
  x: TInt64;
begin
  Result:=0; if p > 0 then
  if Globe.Map.Enabled_Map then begin

    x.x:=p; if x.cn = 0 then
    Result:=Globe.Map.Tag_Object(p);

    if x.cn <> cn_aggr then begin
      ctrl_List.Add(p); x_disp_ctrl(mode);
      ctrl_List.Clear;
    end
  end
end;

procedure TDrawCard.Disp_list(List: TInt64List; Mode: Integer);
var
  i: Integer; p: Int64;
begin
  if Assigned(List) then begin
    ctrl_list.AddList(List);

    if Mode = x_Show then
      x_disp_ctrl(x_Draw)
    else
      x_disp_ctrl(Mode);

    ctrl_List.Clear;

    if not Card.Locked then
    for i:=0 to List.Count-1 do begin
      p:=List.Items[i]; with Globe.Map do
      if (p = Occupe_a) or (p = Occupe_b) then
      Disp_Object(p,x_Mark)
    end;
  end
end;

function TDrawCard.Free_Occupe_Point(P: Int64): Boolean;
var
  c: VPoint;
begin
  Result:=false;

  if Globe.Map.Get_Occupe_Point(P,c) then begin

    Globe.Map.Free_Occupe_Point(P);

    if not IsRefresh then
    with xProject(c.x,c.y,c.z) do
    if Card_ContainsPoint(x,y,8,8) then
    Card_Refresh(nil);

    Result:=true
  end;
end;

function TDrawCard.ReleaseObject(p: Int64): Integer;
begin
  Free_Occupe_Point(p);
  Result:=Unmark_Object(p)
end;

procedure TDrawCard.OccupeA_Object(p: Int64);
begin
  Globe.Map.Occupe_a:=p;
  IsRefresh:=true;
  Card_Locked:=false;
end;

function TDrawCard.Unmark_Object(p: Int64): Integer;
begin
  if p > 0 then
  if not IsRefresh then
  Disp_Object(p,x_Undo);
  Result:=0
end;

function TDrawCard.Draw_Mark_Object(p: Int64): Int64;
begin
  Disp_Object(p,x_Draw);
  Disp_Object(p,x_Mark);
  Result:=p
end;

procedure TDrawCard.ext_Edit_Hide(p: Int64);
var
  x: TInt64;
begin
  x.x:=p;
  fIs_Ext_Disp:=x.cn <> cn_object;
  ext_Disp_Object(p,x_Hide);
end;

function TDrawCard.ext_Draw_Object(p: Int64): Boolean;
begin
  Result:=false
end;

procedure TDrawCard.ext_Disp_Object(p: Int64; Mode: Integer);
var
  x,q: TInt64; i,n: Integer;
begin
  x.x:=p;

  if x.cn = cn_object then
    Disp_Object(p,Mode)
  else

  with Globe.Map do
  if Enabled_Map then begin

    if x.cn = cn_node then begin
      n:=Tree.Get_vc_xref(x.id,ctrl_List);
      Tree.Add_fe(0,x.Id,ctrl_List);

      for i:=0 to n-1 do begin
        q.x:=ctrl_List.Items[i];
        Tree.Add_fe(q.Id,0,ctrl_List)
      end
    end else
    if x.cn = cn_edge then
      Tree.Add_fe(x.Id,0,ctrl_List);

    ctrl_list.Add(p);

    x_disp_ctrl(Mode);
    ctrl_List.Clear
  end
end;

procedure TDrawCard.ext_Hide_Object(p: Int64);
var
  x: TInt64;
begin
  x.x:=p; if x.cn = cn_object then Hide_geo(p)
  else ext_Disp_Object(p,x_HIDE)
end;

function TDrawCard.Hide_geo(p: Integer): Integer;
var
  i,loc: Integer; x: TInt64; ve: tcn_rec;
begin
  Result:=0; Disp_Object(p,x_HIDE);

  with Globe.Map do
  if Enabled_Map then begin
    loc:=Tag_Object(p);

    if loc in [21..23] then begin
      IsDoctor:=true; with x_Edit do
      if xGet_mf(p,DPolybuf,nil) >= 0 then begin

        for i:=0 to DPolybuf.N do begin
          x.id:=DPolybuf.Pol[i].x;
          if loc = 21 then x.cn:=cn_node
          else x.cn:=cn_edge;

          ctrl_list.Add(x.x);
        end;

        if loc in [22..23] then
        for i:=0 to DPolybuf.N do begin
          x.id:=DPolybuf.Pol[i].x;
          x.cn:=cn_node;

          if Tree.get_ve(x.id,@ve) then begin
            x.id:=ve.vc1; ctrl_list.Add(x.x);
            x.id:=ve.vc2; ctrl_list.Add(x.x)
          end;
        end;

        if ctrl_list.Count > 0 then
        x_disp_ctrl(x_Draw);

        ctrl_List.Clear
      end;

      IsDoctor:=false;
    end
  end;

  Globe.Map.Close_Map
end;

procedure TDrawCard.ocx_Hide_Object(p: Int64);
begin
  if p > 0 then begin

    if Globe.Count > 0 then begin

      if p = Globe.Map.Occupe_a then
      Globe.Map.Occupe_a:=0;

      if Globe.Open_Active_Map then
      if Globe.Map.Ok_Object(p) then

      if Globe.ActiveMap = Globe.Video-1 then
        Card_Refresh(nil)
      else
        Disp_Object(p,x_HIDE);
    end;

    Globe.Map.Close_Map;

    Query.DispList.x_Remove(p);
    fHints.RemoveObject(p)
  end
end;

procedure TDrawCard.Disp_Polyline(dc: XCanvas; lp: PLLine);
var
  lt,rb: TPoint;
begin
  if mf_Project(lp,nil, lt,rb, true,false) > 0 then

  if (lt.X <= map_rb.X) and (rb.X >= map_lt.X) then
  if (lt.Y <= map_rb.Y) and (rb.Y >= map_lt.Y) then

  with lp^ do

  if (lt.X < map_lt.X) or (lt.Y < map_lt.Y) or
     (rb.X > map_rb.X) or (rb.Y > map_rb.Y) then
  begin
    clip_master.plot_begin(dc,0,0,0,0);
    clip_master.plot_polyline(@lp.Pol,lp.N);
  end else

  dc.PolyLine(@Pol,N);
end;

procedure TDrawCard.Draw_LPoint(dc: XCanvas;
                                const p: TPoint; d,i: Integer;
                                curve,cross: Boolean);
var
  v: VPoint;
begin
  v.x:=p.x; v.y:=p.y; v.z:=0;
  Draw_VPoint(dc,v, d,i, curve,cross)
end;

procedure TDrawCard.Draw_VPoint(dc: XCanvas;
                                const v: VPoint; d,i: Integer;
                                curve,cross: Boolean);
var
  p: TPoint;
begin
  p:=xProject(v.x,v.y,v.z);

  if dc.Contains_point(p.x,p.y, 8,8) then

  if curve and Odd(i) then
    dc.DrawPoint(p.x,p.y,3)
  else begin
    dc.DrawPoint(p.x,p.y,d);

    if cross then
    dc.CrossPoint(p.x,p.y,d+1)
  end
end;

procedure TDrawCard.Draw_lcurve(dc: XCanvas; lp: PLLine);
var
  pen: TDrawCurve;
begin
  pen:=clip_master.beginCurve(dc,0,0,0,0,LPolyMax);
  try
    pen.Draw_lcurve(@lp.Pol,lp.N);
  finally
    pen.Free;
  end;
end;

procedure TDrawCard.Draw_ellipse(dc: XCanvas; lp: PLLine);
var
  pen: TDrawCurve;
begin
  pen:=clip_master.beginCurve(dc,0,0,0,0,LPolyMax);
  try
    pen.Draw_ellipse(@lp.Pol,lp.N);
  finally
    pen.Free;
  end
end;

procedure TDrawCard.Draw_bspline(dc: XCanvas; lp: PLLine; i1,i2: int);
var
  pen: TDrawCurve;
begin
  pen:=clip_master.beginCurve(dc,0,0,0,0,LPolyMax);
  try
    pen.Draw_bspline(@lp.Pol,lp.N, i1,i2, -1);
  finally
    pen.Free;
  end
end;

procedure TDrawCard.Draw_LSector(dc: XCanvas; lp: PLPoly);
var
  pen: TDrawCurve;
begin
  pen:=clip_master.beginCurve(dc,0,0,0,0,LPolyMax);
  try
    pen.Draw_sector(lp);
  finally
    pen.Free;
  end
end;     

procedure TDrawCard.Draw_LLine(dc: XCanvas; lp: PLLine);
begin
  with lp^ do
  if dc.Clip_Polyline(@Pol,N) then begin
    clip_master.plot_begin(dc,0,0,0,0);
    clip_master.plot_Polyline(@lp.Pol,lp.N)
  end else

  if eye_draw.Enabled then
    eye_draw.Polyline2(lp)
  else
    dc.PolyLine(@Pol,N)
end;

procedure TDrawCard.Draw_VVector(dc: XCanvas; const a,b: VPoint);
var
  _a,_b: TPoint;
begin
  _a:=xProject(a.x,a.y,a.z);
  _b:=xProject(b.x,b.y,b.z);

  if eye_draw.Enabled then
    eye_draw.xLine(_a,_b)
  else
    dc.Draw_Line(_a,_b)
end;

procedure TDrawCard.Draw_LVector(dc: XCanvas; const a,b: TPoint);
var
  _a,_b: TPoint;
begin
  Project(a.x,a.y,_a);
  Project(b.x,b.y,_b);
  dc.Draw_Line(_a,_b)
end;

function TDrawCard.LPolyBuf_Movie: Boolean;
begin
  Result:=fx_Disp.Movie or Is_Eye or
          (x_Edit.LPolyBuf_cn > 0)
end;

procedure TDrawCard.Disp_LPolyBuf_Begin(dc: XCanvas);
var
  flags: Integer;
begin
  Cursor_BeginPaint;

  if x_Edit.Tool = ext_Port then
    dc.rgb_Pen(2,clFuchsia)
  else begin
    if LPolyBuf_Movie then begin
      dc.xPen(1,fctx.pen_Active);
      flags:=Movie_Flags
    end
    else begin
      dc.Set_Xor_Mode; flags:=0
    end;

    eye_draw.Begin_Paint(dc,movie_draw,flags);
    eye_draw.Color:=fctx.rgb_Active_Color
  end
end;

procedure TDrawCard.Disp_LPolyBuf_End(dc: XCanvas);
begin
  dc.Canvas.Set_Copy_Mode;
  eye_draw.Enabled:=false;
  Cursor_AfterPaint;
end;

procedure TDrawCard.LPolybuf_update(Value: Boolean);
begin
  fLPolybuf_update:=Value;
  LPolybufEvent(nil)
end;

procedure TDrawCard.LPolybufEvent(Sender: TObject);
begin
  if not fLPolybuf_update then
  if Assigned(fOnLPolybuf) then
  fOnLPolybuf(Self);
end;

procedure TDrawCard.xMark_LPolyBuf(dc: XCanvas);
begin
  Mark_LVecBuf(dc); Mark_LPolyBuf(dc);
end;

procedure TDrawCard.Disp_LPolyBuf(dc: XCanvas);
begin
  xMark_LPolyBuf(dc);

  Globe.StereoLeft(false);

  if Is_Eye then
    xMark_LPolyBuf(dc)
  else
  if Right <> nil then
  if not isInvalidate then

  (Right as TDrawCard).xMark_LPolyBuf(dc);

  Globe.StereoLeft(true);

  LPolybufEvent(nil)
end;

function TDrawCard.Track_LPolyBuf: Boolean;
var
  I: Integer;
begin
  Result:=false;
  with x_Edit,LPolyBuf^ do
  if N >= 0 then begin
    i:=Min( Max(lp_ind,0), N);
    Result:=Card_Track(Pol[i])
  end
end;

function TDrawCard.This_LPolybuf(cX,cY: Integer): Integer;
var
  i: Integer; p: TPoint;
begin
  Result:=-1;
  with x_Edit.LPolyBuf^ do
  for i:=0 to N do begin
    with Pol[i] do Project(X,Y,p);

    if (p.X >= cX-4) and (p.X <= cX+4) then
    if (p.Y >= cY-4) and (p.Y <= cY+4) then begin
      Result:=i; Break
    end
  end
end;

function TDrawCard.This_LPolyCurve(cX,cY: Integer): TPoint;

function This_point(const P: TPoint; cX,cY: Integer): Boolean;
var
  q: TPoint;
begin
  Result:=false; Project(P.X,P.Y,q);
  if (q.X >= cX-4) and (q.X <= cX+4) then
  if (q.Y >= cY-4) and (q.Y <= cY+4) then
  Result:=true
end;

function curve_vector(cX,cY, Ind: Integer): TPoint;
var
  p1,p2,p3: TPoint;
begin
  Result:=Point(-1,-1);

  with x_Edit.LPolyBuf^ do
  if (Ind >= 0) and (Ind < N) then begin

    p1:=Pol[Ind]; p2:=Pol[Ind+1];
    Mirror_Point(p2,p1,p3);

    if Ind <= x_Edit.lp_Ind then
    if This_point(p2,cX,cY) then
    Result:=Point(Ind+1,0);

    if Result.X < 0 then
    if Ind - x_Edit.lp_Ind >= -1 then
    if This_point(p3,cX,cY) then
    Result:=Point(Ind+1,1);
  end
end;

var
  Ind: Integer;
begin
  Result:=Point(-1,-1);

  Ind:=This_LPolybuf(cX,cY);

  if Ind >= 0 then
  if Odd(Ind) then Ind:=-1;

  if Ind >= 0 then
    Result:=Point(Ind,-1)
  else begin
    Ind:=x_Edit.lp_Ind;
    if Odd(Ind) then Dec(Ind);

    Result:=curve_vector(cX,cY, Ind+2);

    if Result.X < 0 then
    Result:=curve_vector(cX,cY, Ind);

    if Result.X < 0 then
    Result:=curve_vector(cX,cY, Ind-2);
  end
end;

procedure TDrawCard.Mark_LPolyBuf(dc: XCanvas);

procedure curve_point(dc: XCanvas; const P: TPoint; R: Integer);
begin
  if eye_draw.Enabled then
    eye_draw.xPoint(P.X,P.Y,R)
  else
    dc.DrawPoint(P.X,P.Y,R)
end;

procedure curve_vector(xdc: XCanvas; Ind: Integer);
var
  p1,p2,p3,q1,q2: TPoint; cl: int;
begin
  with x_Edit.DPolyBuf^ do
  if (Ind >= 0) and (Ind < N) then begin

    p1:=Pol[Ind]; p2:=Pol[Ind+1]; q1:=p1; q2:=p1;

    if Ind <= x_Edit.lp_Ind then q1:=p2;

    if Ind - x_Edit.lp_Ind >= -1 then begin
      Mirror_Point(p2,p1,p3); q2:=p3
    end;

    if not Points_equal(q1,q2) then begin

      cl:=clNavy;
      if LPolyBuf_Movie then
      cl:=clBlue;

      xdc.rgb_xPen(1,1,cl);
      xdc.Draw_Line(q1,q2);
    end;

    xdc.rgb_xPen(1,0,clNavy);
    if not Points_equal(q1,p1) then
    curve_point(xdc,q1,2);

    if not Points_equal(q2,p1) then
    curve_point(xdc,q2,2);
  end
end;

procedure draw_CurveBuf(dc: XCanvas; d: Integer);
begin
  with x_Edit,CurveBuf do begin
    Draw_lVector(dc,Pol[0],Pol[1]);
    Draw_LPoint(dc,Pol[0],d,0,lp_Curve,false);
    Draw_LPoint(dc,Pol[1],d,1,lp_Curve,false)
  end
end;

procedure draw_cn(xdc: XCanvas);
var
  dm: tdm_map; i,j: int; x: TInt64;
  p1,p2: TPoint; is_lp: Boolean;
begin
  dm:=Globe.Map;

  if dm.Enabled_Map then

  with x_Edit do
  if LPolybuf_cn = 1 then begin

    xdc.xPen(0,14);
    xdc.xBrush(0,fGround);

    for i:=0 to LPolyBuf.N do
    draw_vc(xdc,LPolybuf.Pol[i].x)

  end
  else begin
    eye_draw.Color:=clYellow;

    is_lp:=false;

    for i:=0 to LPolyBuf.N do begin
      x.id:=LPolyBuf.Pol[i].x; x.cn:=cn_edge;

      if dm.xGet_mf(x.x,DPolyBuf,DHeights) > 0 then
      with DPolyBuf^ do begin
        for j:=0 to N do with DPoints[j] do
        Pol[j]:=xProject(x,y,z);

        if i = 0 then p1:=Pol[0];
        p2:=Pol[N]; is_lp:=true;

        Draw_LLine(xdc,DPolyBuf)
      end
    end;

    if is_lp then begin
      xdc.DrawPoint(p1.X,p1.Y,2);
      xdc.CrossPoint(p2.X,p2.Y,3)
    end
  end
end;

var
  i,i1,i2,d,_n: int; p1,p2: TPoint;
begin
  with x_Edit do
  if LPolyBuf.N >= 0 then begin

    Disp_LPolyBuf_Begin(dc);

    if LPolyBuf_cn > 0 then
      draw_cn(dc)
    else

    with DPolyBuf^ do begin

      if Tool = ext_Port then
      if lp_page then Align_prj_page(LPolybuf);

      N:=LPolyBuf.N; for i:=0 to N do
      with LPoints[i] do Pol[i]:=xProject(x,y,z);

      if Tool <> ext_Port then
      with Pol[0] do if not lp_Curve then
      if eye_draw.Enabled then eye_draw.xCross(X,Y,4)
      else dc.CrossPoint(X,Y,4);

      if N > 0 then begin
        if lp_Ellipse then
          Draw_ellipse(dc,DPolyBuf)
        else
        if lp_Curve then begin

          _n:=DPolyBuf.N;
          if lp_lock then
          Lock_Curve(DPolyBuf,LPolyMax);
          Draw_lcurve(dc,DPolyBuf);
          DPolyBuf.N:=_n;

          if LPolyBuf_Movie then
          dc.Canvas.SetPenColor(clNavy);
          eye_draw.Color:=clNavy;

          i:=0; while i < N do begin
            curve_point(dc,Pol[i],3); Inc(i,2)
          end;

          if lp_Ind >= 0 then begin
            i:=lp_Ind; if Odd(i) then Dec(i);
            curve_vector(dc,i-2); curve_vector(dc,i);
            curve_vector(dc,i+2);
          end

        end else

        if lp_mode = 11 then begin
          i1:=0; i2:=N;

          if lp_Ind = N then begin
            if lp_lock and (N > 0) then Inc(i1);
            if State <> st_lmov then Dec(i2);
          end;

          Draw_bspline(dc,DPolyBuf, i1,i2)
        end
        else Draw_LLine(dc,DPolyBuf)
      end;
    end;

    with LPolyBuf^ do begin d:=3;
      if Tool in [ext_Port,ext_Geoid] then d:=7;

      i:=0; if lp_Ellipse then
      else
      if lp_Curve then begin
        if CurveLock(LPolyBuf) then i:=2
      end else
      if PolyLock(LPolyBuf) then i:=1;

      if not eye_draw.Enabled then
      while i <= N do begin
        Draw_VPoint(dc,LPoints[i],d,i,lp_curve,false);
        Inc(i)
      end;

      if not lp_Ellipse then
      if not (Tool in [ext_Port,ext_Geoid]) then begin

        if CurveBuf.N > 0 then
          draw_CurveBuf(dc,d)
        else
        if not eye_draw.Enabled then begin
          i:=lp_ind; if i < 0 then i:=0;
          Draw_VPoint(dc,LPoints[i],d,i,lp_Curve,true);

          if lp_ind >= 0 then begin Inc(i);
            if (i <> 0) and (i <= N) then
            Draw_VPoint(dc,LPoints[i],d,i,lp_Curve,true)
          end
        end
      end
    end;

    Disp_LPolyBuf_End(dc)
  end
end;

procedure TDrawCard.Clear_LPolyBuf;
begin
  x_Edit.LPolyBuf_Clear;
  Card_Refresh(nil)
end;

function TDrawCard.LPolybuf_undo: Boolean;
begin
  Result:=false;

  with x_Edit.LPolyBuf^ do
  if N >= 0 then begin Dec(N);
    if N < 0 then x_Edit.LPolybuf_cn:=0;
    Card_Refresh(nil); Result:=true
  end
end;

procedure TDrawCard.Clear_LVecBuf;
begin
  x_Edit.LVecInd:=-1;
  Card_Refresh(nil)
end;

procedure TDrawCard.begin_LVecBuf(const v: VPoint);
begin
  x_Edit.LVecInd:=0;
  x_Edit.VecBuf[0]:=v;
  Card_Refresh(nil);
  LPolybufEvent(nil)
end;

procedure TDrawCard.Back_LVecBuf;
begin
  Dec(x_Edit.LVecInd);
  Card_Refresh(nil)
end;

procedure TDrawCard.Mark_LVecBuf(dc: XCanvas);
begin
  with x_Edit do
  if State <> st_Frame then
  if LVecInd >= 0 then begin

    Disp_LPolyBuf_Begin(dc);

    Draw_VPoint(dc,VecBuf[0],3,0,false,true);

    if LVecInd = 1 then
    Draw_VPoint(dc,VecBuf[1],3,0,false,true);

    Disp_LPolyBuf_End(dc)
  end
end;

procedure TDrawCard.Mark_XPolyBuf(dc: XCanvas);
var
  i: int; lp: PLPoly; p: TPoint;
begin
  if x_Edit.State = st_Input then
  if x_Edit.is_Pencil then

  with x_Edit.XPolyBuf^ do
  if N > 0 then begin

    dc.Set_Xor_Mode;

    lp:=@Pol;
    with lp[0] do Project(X,Y,p);
    dc.MoveTo(p.X,p.Y);

    for i:=1 to N do begin lp:=@lp[1];
      with lp[0] do Project(X,Y,p);
      dc.LineTo(p.X,p.Y);
    end;

    dc.Set_Copy_Mode
  end
end;

procedure TDrawCard.MouseMove(Shift: TShiftState; X,Y: integer);

function navMouseMove(X,Y: int): Boolean;
var
  i: int;
begin
  Result:=false;
  for i:=0 to fnavList.Count-1 do
  if TNavCustom(fnavList[i]).MouseMove(X,Y) then
  begin Result:=true; Break end
end;

var
  v: lxyz; l: LOrient;
begin
  if not IsCapture then
  toolButton:=0;

  if dm_t_init then
  if not isInvalidate then
  if not isScroll then
  if not navMouseMove(X,Y) then begin

    SetLocator(X,Y,6); 

    if isCapture then begin

      if isPlus = 0 then Tools_Pick(X,Y,v.v);

      MovePencil(X,Y);

      if (isPlus = 2)
      or (Capture.Butt = but_Hand) then
      if Drag_Enabled then Capture_Drag;

      with Capture do if IsVector then
      if Assigned(fOnRulePos) then begin
        Backup(R.Left,R.Top,l[0]);
        l[1]:=loc_dot; fOnRulePos(@l,2)
      end

    end else

    if isPlus > 0 then begin
      if isPlus = 2 then cd_Cursor:=_xHand
      else cd_Cursor:=_xZoom
    end
    else begin
      Cursor_move(X,Y); Tools_Move(X,Y);
    end
  end
end;

procedure TDrawCard.MouseDown(Button: TMouseButton; X,Y: Integer);

function navMouseDown(Button: TMouseButton; X,Y: int): Boolean;
var
  i: int;
begin
  Result:=false;
  for i:=0 to fnavList.Count-1 do
  if TNavCustom(fnavList[i]).MouseDown(Button,X,Y) then
  begin Result:=true; Break end
end;

begin
  toolButton:=ord(Button)+1;

  if dm_t_init then
  if not isScroll then
  if not isInvalidate then
  if not navMouseDown(Button,X,Y) then

  if Card_Contains_Mouse(X,Y) then begin

    SetLocator(X,Y,6);

    if Button = mbLeft then begin

      Backup_Plus(X,Y,plus_p);

      if isPlus > 0 then begin
        if isPlus = 2 then CardCapture(X,Y,1,_xDrag,false)
        else dos_CardCapture(X,Y,1,Card.Cursor,isPlus <> 2)
      end
      else Tools_Dn_Left(X,Y);

    end else
    if Button = mbRight then begin

      if not isCapture then begin

        if fIsRightHand then begin
          Backup_Plus(X,Y,plus_p);
          CardCapture(X,Y,but_Hand,_xDrag,false);
        end else

        if isPlus > 0 then
          CardCapture(X,Y,2,Card.Cursor,false)
        else
          Tools_Dn_Right(X,Y)

      end
    end else
    if Button = mbMiddle then begin
      if not isCapture then begin
        Backup_Plus(X,Y,plus_p);
        CardCapture(X,Y,but_Hand,_xDrag,false);
      end
    end;

  end; Show_Status
end;

procedure TDrawCard.MouseUp(Button: TMouseButton; X,Y: Integer);

function navMouseUp(Button: TMouseButton; X,Y: int): Boolean;
var
  i: int;
begin
  Result:=false;
  for i:=0 to fnavList.Count-1 do
  if TNavCustom(fnavList[i]).MouseUp(Button,X,Y) then
  begin Result:=true; Break end
end;

begin
  toolButton:=ord(Button)+1+8;

  if dm_t_init then
  if not isScroll then
  if not isInvalidate then
  if not navMouseUp(Button,X,Y) then

  if not Card_Contains_Mouse(X,Y) then begin
    if isCapture then ReleaseCapture;

    if (Button = mbMiddle)
    or (Capture.Butt = but_Hand) then
      Capture_Plus(2)
    else
    if Capture.Butt = 1 then
    if isPlus = 2 then Capture_Plus(-1)

  end else

  if isCapture then with x_Edit do begin

    SetLocator(X,Y,6); ReleaseCapture;

    if Button = mbLeft then begin

      if isPlus > 0 then begin
        if Capture.Butt = 1 then Capture_Plus(-1)
        else
        if dos_interface then
          CardCapture(X,Y,1,Card.Cursor,isPlus = 1)
      end
      else Tools_Up_Left(X,Y)

    end else
    if Button = mbRight then begin

      if isPlus > 0 then begin
        if Capture.Butt = 2 then
        if x_Edit.Tool <> ext_Drag then
        isPlus:=0
      end else
      if Capture.Butt = but_Hand then
        Capture_Plus(2)
      else
      if Capture.Butt <> 1 then
        Tools_Up_Right(X,Y)
      else
      if not Tools_Up_Right1(X,Y) then
        Tools_Up_Right(X,Y);

    end else
    if Button = mbMiddle then
      Capture_Plus(2);

    Show_Status
  end
end;

procedure TDrawCard.MouseMoveTo(var X,Y: Integer);
begin
  if isCapture then
  if x_Edit.State = st_Input then

  if key_Pressed(vk_F9) then
    Y:=Capture.R.Top
  else
  if key_Pressed(vk_F10) then
    X:=Capture.R.Left;
end;

function TDrawCard.GetHint(Str: PChar; smax: int; var X,Y: int): bool;
var
  p: TPoint;
begin
  Result:=false; StrCopy(Str,'');

  if dm_t_init then
  if not isInvalidate then
  if not isScroll then
  if fHints.Pickup(loc_lt,loc_rb,Str,smax,p) then begin
    Project(p.X,p.Y,p);
    X:=p.X; Y:=p.Y; Result:=true
  end
end;

function TDrawCard.Tools_Pick(X,Y: integer; out v: VPoint): Int64;
begin
  Result:=0; v.x:=0; v.y:=0; v.z:=0
end;

procedure TDrawCard.Tools_Move(X,Y: integer);
begin
end;

procedure TDrawCard.Tools_Dn_Left(X,Y: integer);
begin
end;

procedure TDrawCard.Tools_Dn_Right(X,Y: integer);
begin
  CardCapture(X,Y,2,Card.Cursor,false)
end;

procedure TDrawCard.Tools_Up_Left(X,Y: integer);
begin
end;

procedure TDrawCard.Tools_Up_Right(X,Y: integer);
begin
end;

function TDrawCard.Tools_Up_Right1(X,Y: integer): Boolean;
begin
  Result:=false
end;

procedure TDrawCard.dm_Zoom_Centre(k: double; z: int;
                                   const c: TPoint);
var
  p: TPoint; g: xgeoid;
begin
  if Assigned(OnScale) then begin
    x_Local(c,p); lg_p.x_L_to_R(p,g);
    if not OnScale(g.x,g.y,k,g.s.pps) then Exit
  end;

  kz:=k; centre:=c; zoom:=z;
  Card_Update(true)
end;

procedure TDrawCard.dm_Zoom_Scale(k: float);
var
  _kz: double;
begin
  if k <> 0 then
  if IsPcx then begin
    if k > small then
    Card_Minus(1/k)
  end else
  if k < 0 then
    dm_Zoom_Centre(min_kz,-1,centre)
  else
  if k > small then
    dm_Zoom_Centre(kz*k,1,centre)
end;

procedure TDrawCard.dm_Set_scale(sc: Double);
begin
  dm_Set_ppm( CardScale(sc) )
end;

procedure TDrawCard.dm_Set_ppm(v: Double);
var
  g: TGeoPoint; mpp,mpp1,sc: Double;
begin
  if v > small then begin

    sc:=Backup_ext_Centre(g);

    if sc > small then
    if Assigned(OnScale) then
    if not OnScale(g.x,g.y,v,g.s.pps) then Exit;

    kz:=v; zoom:=1;

    if IsPcx then begin mpp:=1/v;

      if sc < small then
        mpp1:=Globe.Pcx_link.Get_resolution(imWidth,imHeight)
      else begin
        mpp:=Get_sys_mpp(mpp,g,Globe.Pcx_link.sys);
        mpp1:=Globe.Pcx_link.Get_geo_mpp(g)
      end;

      if mpp1 > small then
      z_dst:=image_zoom(mpp/mpp1,z_src)
    end;

    Card_Update(true)
  end
end;

procedure TDrawCard.dm_Centre(const p: TPoint);
begin
  ProjectCentre(p,centre);
  Card_Update(true)
end;

procedure TDrawCard.geo_Centre(const g: TGeoPoint);
var
  p: TPoint; t: TGauss;
begin
  if IsPcx then begin

    if IsXyz then begin
      lg_p.x_R_to_L(g,p);
      centre:=dm_to_bmp(p)
    end
    else begin
      Globe.Pcx_link.g_to_p(g,t);
      t:=img_to_card(t.x,t.y);
      centre.X:=Round(t.x);
      centre.Y:=Round(t.y);
    end

  end
  else begin
    lg_p.x_R_to_G(g,t);
    xy_to_sm(t,centre);
  end;

  Card_Update(true);
end;

procedure TDrawCard.plus_Gauss(const lt,rb: tgauss; k: double);
var
  dx,dy,d: double; a,b,c: TPoint;
begin
  lg_p.G_to_L(lt,a);
  lg_p.G_to_L(rb,b);
  Middle_Point(a,b,c);

  if isPcx then begin
    a:=dm_to_bmp(a); b:=dm_to_bmp(b);
    inherited Card_plus(a,b)
  end
  else begin
    dx:=Abs(rb.x-lt.x);
    dy:=Abs(rb.y-lt.y);

    if k > 1 then begin
      d:=Max(dx,dy)*2/k;
      dx:=dx+d; dy:=dy+d
    end;

    if dx > 0.01 then begin
      k:=Card.Height/dx;

      if dy > 0.01 then
      k:=Min(k, Card.Width/dy)
    end else

    if dy > 0.01 then
      k:=Card.Width/dy
    else
      k:=kz*2;

    x_Gauss(c,c);
    dm_Zoom_Centre(k,1,c)
  end
end;

function TDrawCard.Card_xPlus(const a,b: TPoint): Boolean;
var
  _a,_b: TPoint;
begin
  Result:=false;
  if a.x <= b.x then
  if a.y <= b.y then begin
    Mult_LRect(a,b,256,_a,_b);
    Card_Plus(_a,_b); Result:=true
  end
end;

procedure TDrawCard.Set_dm_Centre(const v: VPoint; sc,k: double);
var
  mpp,mpp1: Double; g: TGeoPoint;
  c,p,q: TPoint; s: TSize;
begin
  p:=Point(v.x,v.y);

  if isPcx then begin

    if k > 0 then begin
      k:=z_src / z_dst * k;
      z_dst:=image_zoom(k,z_src);
    end
    else begin mpp:=-1;

      if sc < 0 then begin // mpp
        if fIsScaleMPP then mpp:=Abs(sc)
      end else
      if sc > 0 then
        mpp:=sc * xMetersPerPixel(0);

      if mpp > 0 then begin
        if Backup_ext_Centre(g) < Small then
          mpp1:=Globe.Pcx_link.Get_resolution(imWidth,imHeight)
        else begin
          mpp:=Get_sys_mpp(mpp,g,Globe.Pcx_link.sys);
          mpp1:=Globe.Pcx_link.Get_geo_mpp(g)
        end;

        if mpp1 > _small then
        z_dst:=image_zoom(mpp/mpp1,z_src);
      end
    end;

    zoom:=1; Card_Zoom(false);

    if IsXyz then begin
      q:=xProject(p.x,p.y,v.z);
      inherited Backup(q.X,q.Y,Centre)
    end
    else begin
      Globe.dm_to_bmp(p,q);
      centre:=bmp_to_card(q.X,q.Y);
    end;

    zoom:=1
  end
  else begin
    s:=Get_prj_size;
    Setup_Edit_Map_Display; x_Gauss(p,c);

    centre.X:=Max(0,Min(s.cx-1,c.X));
    centre.Y:=Max(0,Min(s.cy-1,c.Y));

    if k > 0 then  kz:=kz / k else
    if sc > 0 then kz:=CardScale(sc) else

    if fIsScaleMPP then
    if sc < 0 then kz:=1/Abs(sc)
  end;

  if (sc >= 0) or IsCapture then
    Card_Update(true)
  else begin
    Card_Zoom(true);
    if not IsMoving then begin
      IsAnimate:=false;
      Card_Draw(nil);
      IsAnimate:=true
    end
  end
end;

procedure TDrawCard.Set_ext_Centre(const g: xgeoid; sc: double);
var
  v: lxyz;
begin
  lg_p.x_R_to_L(g,v.p); v.v.z:=0;
  Set_dm_Centre(v.v,sc,0)
end;

procedure TDrawCard.Set_geo_Centre(const g: xgeoid; sc: double);
var
  v: lxyz;
begin
  if Globe.Count > 0 then
  with Globe.Map do begin
    lg_T.x_R_to_L(g,v.p); v.v.z:=0;
    if xContains_Point(v.p) then
    Set_dm_Centre(v.v,sc,0)
  end
end;

procedure TDrawCard.Set_Gauss_Centre(x,y,z,sc: double);
var
  v: lxyz;
begin
  if Globe.Count > 0 then begin
    v.v:=Globe.Map.xyz_to_dm(x,y,z);
    Set_dm_Centre(v.v,sc,0)
  end
end;

procedure TDrawCard.Set_wgs_Centre(x,y,z,sc: double);
var
  v: lxyz;
begin
  if Globe.Count > 0 then begin
    v.v:=Globe.Map.wgs_to_dm(x,y,z);
    Set_dm_Centre(v.v,sc,0)
  end
end;

function TDrawCard.Rebound_prj: Boolean;
var
  sc: Double; g: XGeoid;
begin
  Result:=false;
  if Globe.Count2 > 0 then begin
    sc:=Get_prj_centre(g);
    if sc > 0 then begin
      Globe.auto_Resize;
      Card_Zoom(true);
      Set_prj_centre(g,sc);
      Result:=true
    end
  end
end;

function TDrawCard.Get_mpp_centre(out c: TGeoPoint): Double;
var
  q,p: TPoint; g: tgauss; ppm: double;
begin
  Result:=-1;

  Fillchar(c,Sizeof(c),0);
  c.s.pps:=-1;

  if dm_t_init then

  with Globe do
  if Count > 0 then begin

    ppm:=1 / dc_Resolution(nil);
    Get_Active_lg_t(lg_p);

    if Card.Visible then begin

      if not IsPcx then
        x_Local(centre,p)
      else
      if IsXyz then begin
        with Centre do
        inherited Project(X,Y,q);
        Backup(q.X,q.Y,p)
      end
      else begin
        with centre do
        q:=card_to_img(X,Y);
        Globe.bmp_to_dm(q,p);
      end

    end else
    with win_edit do begin
      ppm:=rv[0]; q:=Point(iv[1],iv[2]);

      if IsPcx then
        Globe.bmp_to_dm(q,p)
      else begin
        sm_to_xy(q,g); lg_p.G_to_L(g,p)
      end
    end;

    lg_p.x_L_to_R(p,c);

    if ppm > _small then
    Result:=1 / ppm
  end
end;

function TDrawCard.Set_mpp_centre(const c: TGeoPoint; mpp: Double): bool;
var
  p,t: TPoint; mpp1: double; s: TSize;
begin
  Result:=false;

  if mpp > _small then begin

    Globe.Get_Active_lg_t(lg_p);

    lg_p.x_R_to_L(c,p);

    if IsPcx then begin
      mpp:=Get_sys_mpp(mpp,c,Globe.Pcx_link.sys);
      mpp1:=Globe.Pcx_link.Get_geo_mpp(c);
      if mpp1 > _small then begin
        z_dst:=image_zoom(mpp/mpp1,z_src);
        zoom:=1
      end;

      if isXyz then begin
        t:=xProject(p.X,p.Y,0);
        inherited Backup(t.X,t.Y,Centre)
      end
      else begin
        Globe.dm_to_bmp(p,t);
        centre:=bmp_to_card(t.X,t.Y);
      end

    end
    else begin
      mpp:=Get_sys_mpp(mpp,c,lg_p.sys);
      if mpp > _small then kz:=1/mpp;

      s:=Get_prj_size;
      Setup_Edit_Map_Display; x_Gauss(p,t);

      centre.X:=Max(0,Min(s.cx-1,t.X));
      centre.Y:=Max(0,Min(s.cy-1,t.Y));
    end;

    if IsCapture then
      Card_Update(true)
    else begin
      Card_Zoom(true);
      if not IsMoving then begin
        IsAnimate:=false;
        Card_Draw(nil);
        IsAnimate:=true
      end
    end;

    Result:=true
  end
end;

function TDrawCard.Get_prj_centre(out c: xgeoid): Double;
var
  mpp: double;
begin
  Result:=-1;
  mpp:=Get_mpp_centre(c);
  if mpp > 0 then
  Result:=1 /lg_p.prj_res * mpp
end;

procedure TDrawCard.Set_prj_centre(const c: xgeoid; sc: Double);
var
  v: lxyz; k,k1,sc1: Double; c1: XGeoid;
begin
  Globe.Get_Active_lg_t(lg_p);

  lg_p.x_R_to_L(c,v.p); v.v.z:=0;
  k:=1 /lg_p.prj_res / sc;
  k:=CardScale(k);

  if IsPcx then begin
    sc1:=Get_prj_centre(c1);
    k1:=sc1/sc; if k1 < 2 then
    if Abs(1-k1) < 1E-3 then k:=0
  end;

  Set_dm_Centre(v.v,k,0)
end;

procedure TDrawCard.win_frame(const fr: TPrjFrame;
                              sc: Double; const g: XGeoid);
var
  fr2: TPrjFrame;
begin
  fr2:=Globe.Get_frame;
  if not FrameContains(fr,fr2) then

  if IsPcx then
    Card_xRepaint(nil)
  else
  if fr.sys.pps <> fr2.sys.pps then begin
    zoom:=-1; Card_Update(true)
  end
  else Set_prj_centre(g,sc);
end;

function TDrawCard.Visible_VPoint(const v: VPoint; sc: double): Boolean;
begin
  Result:=true; if sc >= 0 then
  Result:=Abs(kz-(sc)) <= 0.0001;

  if Result then with xProject(v.x,v.y,v.z) do
  Result:=Card_ContainsPoint(X,Y,-8,-8);
end;

function TDrawCard.Present_VPoint(const v: VPoint; sc: double): Boolean;
begin
  Result:=false;

  if not Visible_VPoint(v,sc) then begin
    Set_dm_Centre(v,sc,0); Result:=true
  end
end;

function TDrawCard.Present_LPoly(lp: PLPoly; hp: PIntegers;
                                 lp_N: Integer; const v: VPoint;
                                 sc: double): Boolean;
var
  i: int; p: lxyz;
begin
  Result:=true; if sc >= 0 then begin
    Result:=Abs(dc_scale(nil)-sc) <= 1;
    if Result then sc:=0
  end;

  if Result then begin
    Result:=false;
    for i:=0 to lp_N do begin
      p.p:=lp[i]; p.v.z:=0;
      if Assigned(hp) then p.v.z:=hp[i];

      with xProject(p.v.x,p.v.y,p.v.z) do
      if Card_ContainsPoint(X,Y,-32,-32) then
      begin Result:=true; Break end
    end;
  end;

  if not Result then Set_dm_Centre(v,sc,0)
end;

function TDrawCard.Up_Local_Centre(const p: TPoint; sc: double): Boolean;
begin
  Result:=Present_VPoint(_VPoint(p.x,p.y,0),sc)
end;

procedure TDrawCard.Set_Local_Centre(const c: TPoint; k: double);
begin
  if isPcx then begin
    centre:=dm_to_bmp(c);
    Card_Update(false)
  end
  else begin
    x_Gauss(c,centre); kz:=kz*k;
    Card_Update(true)
  end
end;
                     
procedure TDrawCard.x_Window(const g: xgeoid; sc: double;
                             out lt,rb: tgauss);
var
  c: tgauss; dx,dy: double;
begin
  lg_p.x_R_to_G(g,c);

  if sc > 0 then
    sc:=CardScale(sc)
  else
    sc:=kz;

  dx:=Card.Height/sc/2;
  dy:=Card.Width/sc/2;

  lt.x:=c.x-dx; rb.x:=c.x+dx;
  lt.y:=c.y-dy; rb.y:=c.y+dy;
end;

function TDrawCard.Get_Gauss_Centre(out g: TGauss): double;
begin
  g.x:=0; g.y:=0; Result:=-1;
  if dm_t_init then begin
    gBackup(Card.Width div 2,Card.Height div 2,g);
    Result:=Get_kz_mpp
  end
end;

function TDrawCard.pcx_Swap(const p,lt,rb: TPoint): Boolean;
var
  mpp,res: Double; scale,w,h: Integer;
  x: xgeoid; g1,g2: tgauss;
begin
  Result:=false;

  mpp:=xMetersPerPixel(0);

  Globe.Map.LG_T.L_to_G(lt,g1);
  Globe.Map.LG_T.L_to_G(rb,g2);

  mpp:=xMetersPerPixel(0);
  w:=Card.Width; h:=Card.Height;
  res:=Gauss_Dist(g1,g2)/Hypot(w,h);
  if mpp < Small then mpp:=Small;
  scale:=Round(res/mpp);

  if Assigned(FOnSwapPcx) then begin
    Globe.Map.LG_t.x_L_to_R(p,x);

    OnCardPort:=Show_Port;
    Swap_LRect(lt,rb, fport_lt,fport_rb);
    Result:=FOnSwapPcx(Self,x,scale);
    OnCardPort:=nil
  end
end;

function TDrawCard.pcx_Zoom(iz: Integer): Boolean;
var
  p,lt,rb: TPoint; w,h: Integer;
begin
  p:=loc_dot; Backup_Card(lt,rb);

  if iz > 0 then begin
    w:=Abs(rb.x-lt.x)* iz div 20;
    h:=Abs(rb.y-lt.y)* iz div 20;
    lt.x:=p.x-w; rb.x:=p.x+w;
    lt.y:=p.y-h; rb.y:=p.y+h;
  end;

  Result:=pcx_Swap(loc_dot,lt,rb)
end;

procedure TDrawCard.pcx_plus(const a,b: TPoint);
begin
  inherited Card_plus(a,b)
end;

procedure TDrawCard.Show_marker(const p: TPoint; fi: Integer);
begin
  fIs_North:=true; x_Gauss(p,centre);
  sm_to_xy(centre,fNorth_gc);
  fNorth:=fi; Card_Update(true)
end;

procedure TDrawCard.Card_Centre(const c: TPoint);
var
  p: TPoint;
begin
  if isPcx then p:=dm_to_bmp(c) else
  x_Gauss(c,p); inherited Card_Centre(p)
end;

procedure TDrawCard.Card_move(const a,b: TPoint);
var
  _a,_b: TPoint;
begin
  if isPcx then begin
    _a:=dm_to_bmp(a);
    _b:=dm_to_bmp(b);
  end
  else begin
    x_Gauss(a,_a);
    x_Gauss(b,_b);
  end;

  inherited Card_move(_a,_b)
end;

procedure TDrawCard.Card_plus(const a,b: TPoint);
var
  lt,rb: tgauss; _a,_b: TPoint;
begin
  if not dm_t_init then
    Card_Update(true)
  else
  if isPcx then begin
    _a:=dm_to_bmp(a);
    _b:=dm_to_bmp(b);
    inherited Card_plus(_a,_b)
  end
  else begin
    lg_p.Get_Gauss_Bound(a,b, lt,rb);
    plus_Gauss(lt,rb, 0)
  end
end;

function TDrawCard.Char_Minus(X,Y: integer): Boolean;
var
  c,p: TPoint; k: Double;
begin
  Result:=false;

  if isPcx then begin
    if not Result then
    inherited Char_Minus(X,Y)
  end
  else begin
    k:=Wheel_centre(X,Y,-1,c);
    clip_centre(c);

    dm_Zoom_Centre(k,1,c);
    Result:=true;
  end
end;

function TDrawCard.Char_Plus(X,Y: integer): Boolean;
var
  c: TPoint; k: Double;
begin
  Result:=false;

  if isPcx then begin
    if not Result then
    Result:=inherited Char_Plus(X,Y)
  end
  else begin
    k:=Wheel_centre(X,Y,2,c);
    clip_centre(c);

    dm_Zoom_Centre(k,1,c);
    Result:=true
  end
end;

function TDrawCard.StrPosition(X,Y: Integer): string;

function pcx_link(x,y: Integer): string;
var
  a,p,d: TPoint; b: TGauss;
begin
  pcx_backup(x,y,a);

  pcx_to_dm(a.x,a.y,p);
  dm_to_pcx(p.X,p.Y,b);

  d.x:=Abs(Round(b.x-a.x));
  d.y:=Abs(Round(b.y-a.y));

  Result:=Format('%d_%d:%d_%s',
    [d.x,d.y,Link_T.tri.triIndex,
     RealToStr(Link_t.tri.GetTriTested,1)]);
end;

function pcx_pos(x,y: Integer): string;
var
  p: TPoint;
begin
  pcx_backupt(X,Y, p);
  Result:=Format('%d:%d',[p.X,p.Y]);
end;

var
  coord: int; g: TGauss; s: tsys; sx,sy: String;
begin
  Result:='';

  if dm_t_init then begin

    SetLocator(X,Y,6);
    Result:=StrWhere(floc_xyz);

    if IsPcx then

    if fIsPcxWhere then
      Result:=pcx_pos(X,Y)
    else
    if debug_enabled then
      Result:=pcx_link(X,Y);

    Change_Rule_pos

  end else

  if not isPcx then begin

    if Globe.Count2 > 0 then begin
      g:=Projective_3x3(X,Y,fwin_bt);
      g:=Projective_3x3(g.x,g.y,fprj_bt);
      g.y:=g.y + lg_p.zoneY;

      coord:=dm_Coord; s:=lg_p.sys;
      if s.prj = 0 then coord:=0;

      if coord = 0 then
        Result:=GaussStr(g,1)
      else begin
        prj_XY_BL(g.x,g.y,s,g.x,g.y);
        sx:=xNorthStr(g.x,1);
        sy:=xEastStr(g.y,1);
        Result:=sx+':'+sy
      end
    end

  end else

  if Globe.Pcx_link.Count > 0 then begin

    coord:=dm_Coord; s:=Globe.Pcx_link.sys;
    if s.prj = 0 then coord:=0;

    g:=img_Backup(x,y);
    Globe.Pcx_link.pix_to_xy(g.x,g.y,g);

    if coord = 0 then
      Result:=GaussStr(g,1)
    else begin
      prj_XY_BL(g.x,g.y,s,g.x,g.y);
      sx:=xNorthStr(g.x,1);
      sy:=xEastStr(g.y,1);
      Result:=sx+':'+sy
    end
  end
  else Result:=pcx_pos(X,Y)
end;

function TDrawCard.StrWhere(const V: VPoint): string;

function gk_test(const g: TGauss): TGauss;
var
  r: TGeoid; t: TGauss;
begin
  Globe.Map.LG_T.z_XY_BL(g,r);
  Globe.Map.LG_T.z_BL_XY(r,t);
  Result.x:=t.x-g.x;
  Result.y:=t.y-g.y
end;

var
  dm: tdm_map;
  coord,ed,geo,m: int; h,gz: Double;
  p: TPoint; g,s: tgauss; r: XGeoid;
  sx,sy: String; s1,s2: tsys;
begin
  dm:=Globe.Map;

  geo:=lg_p.sys.prj;
  p.X:=v.x; p.Y:=v.y;

  if Assigned(OnXY_BL) then begin
    lg_p.L_to_G(p,g); geo:=0;
    OnXY_BL(g.x,g.y, g.x,g.y, geo)
  end;

  if dm_Coord in [1,2] then
  if geo = 0 then dm_Coord:=0;
  coord:=dm_Coord; if coord > 3 then
  coord:=0;

  if coord in [1..2] then
  if lg_p.sys.pps = 0 then begin
    if dm.LG_T.sys.prj = 0 then coord:=0
  end
  else begin
    if lg_p.sys.prj = 0 then coord:=0;
  end;

  if coord = 3 then begin

    coord:=0; s:=Azimuth.g;

    if Azimuth.Enabled then
    if Globe.Project_Contains(s) then
    coord:=3;

    if coord = 0 then
    if x_Edit.State = st_Input then
    with x_Edit.LPolyBuf^ do
    if N >= 0 then begin
      lg_p.L_to_G(Pol[N],s);
      coord:=3
    end
  end;

  ed:=dm.dm_ed;
  if ed = 0 then ed:=1;

  if Globe.is_mm then begin
    lg_p.L_to_G(p,g);
    Result:=GaussStr(g,1);
  end else

  case coord of
0:  begin
      g:=dm.LG_t.L_to_prj(p,sys_coord);

      if fIsGKWhere then
      if Globe.Map.LG_T.sys.prj in [1,2] then
      g:=gk_test(g);

      if ver_english then xSwap(g.x,g.y);
      Result:=CoordStr(g,ed);
    end;
1,
2:  begin
      if lg_p.sys.pps = 0 then

        dm.LG_T.L_to_bl(p,tgeoid(g))

      else begin
        lg_p.L_to_bl(p,tgeoid(g));

        if dm.LG_T.sys.pps = 1 then
        if dm.Contains_Point(p.x,p.y) then
        dm.LG_T.L_to_bl(p,tgeoid(g));

        if sys_coord > 0 then begin
          s1:=dm.LG_T.sys;
          if s1.elp > 0 then
          if s1.elp <> sys_coord then begin

            if sys_coord = 1 then s2:=sys_ru95
                             else s2:=sys_wgs84;

            GEO_GEO(g.x,g.y,0,s1.elp,s2.elp,@s1.dat,@s2.dat,g.x,g.y,gz)
          end
        end
      end;

      if not IsNan(g.x) and not IsNan(g.y) then begin

        if g.y < -Pi then g.y:=g.y + 2*Pi;
        if g.y > Pi then g.y:=g.y - 2*Pi;

        Is_Angle_fmt:=true;

        m:=Globe.Map.ms_unit;
        sx:=xNorthStr(g.x,m);
        sy:=xEastStr(g.y,m);

        if coord = 1 then
          Result:=sx+':'+sy
        else
          Result:=sy+':'+sx;

        Is_Angle_fmt:=false
      end
    end;

3:  begin
      lg_p.L_to_G(p,g);
      g.x:=lg_p.gLength(s,g, g.y);
      Result:=RealStr(g.x,0,ed)+':'+AngleStr(g.y,ed)
    end
  end;

  if coord in [0..2] then

  if IsXyz then begin
    h:=v.z/Globe.Map.z_res;
    Result:=Result+':'+RealToStr(h,1)
  end else
  if IsRel then begin
    if Globe.dm_Get_zval(p,h) then
    Result:=Result+':'+RealToStr(h,2);
  end
end;
    
procedure TDrawCard.Status(Id: Integer; Msg: PChar);
begin
  if Assigned(OnStatus) then
  OnStatus(Id,Msg)
end;

procedure TDrawCard.Show_Status;
var
  id: Integer;
begin
  if Assigned(OnStatus) then

  if isPlus > 0 then begin id:=0;

    if isPlus = 1 then
      id:=1
    else begin id:=3;
      if isCapture then
      if Capture.butt = 1 then
      Inc(id)
    end;

    OnStatus(Id,nil)
  end
  else Tool_Status(0)
end;

procedure TDrawCard.Tool_Status(P: Int64);
begin
end;

procedure TDrawCard.Card_Help(id: Integer);
begin
  CancelCapture;
  if Assigned(Card) then
  Card.HelpContext(id);
end;

procedure TDrawCard.sau_Show; begin end;
procedure TDrawCard.sau_Hide; begin end;

procedure TDrawCard.Change_Rule_pos;
var
  ln: int; l: LOrient;
begin
  if not IsCapture
  or  not Capture.isVector then

  if Assigned(fOnRulePos) then begin

    ln:=1;

    with x_Edit do
    if not lp_curve then
    with LPolyBuf^ do
    if N >= 0 then begin
      ln:=2; if N > 0 then begin
        ln:=3; l[0]:=Pol[N-1]
      end; l[ln-2]:=Pol[N]
    end;

    l[ln-1]:=loc_dot; fOnRulePos(@l,ln)
  end
end;

procedure TDrawCard.Change_Rule_dist;
begin
  with x_Edit do
  if Assigned(fOnRuleDist) then
  lp_lock:=fOnRuleDist(LPolyBuf,LHeights,lp_curve)
end;

function TDrawCard.Get_Coord_Azimuth: Boolean;
begin
  Result:=false; if Azimuth.Enabled then
  Result:=Globe.Project_Contains(Azimuth.g)
end;

procedure TDrawCard.Begin_show_port;
begin
  Backup_Card(fport_lt,fport_rb);
  OnCardPort:=Show_Port
end;

procedure TDrawCard.Show_Port(out lt,rb: TPoint);
begin
  lt:=fport_lt; rb:=fport_rb;
end;

procedure TDrawCard.Close_show_port;
begin
  OnCardPort:=nil
end;

procedure TDrawCard.MosaicClose(Sender: TObject; Path: PChar);
begin
  if Assigned(fMosaic) then
  fMosaic.Paint(nil,nil,nil)
end;

procedure TDrawCard.Video(avi: int;
                          Frame: PBitmap;
                          Frag: PGPoly);
var
  proj: IProject2;
  proj1: TWgsMosaic;
  ox,oy,fc: int; map: Bitmap;
begin
  if not isInvalidate then
  if Cash.Active then

  if Assigned(fMosaic) then

  if Cash.GetMap(map) then begin

    ox:=xPos; xPos:=Cash.v_lt.x;
    oy:=yPos; yPos:=Cash.v_lt.y;
    Setup_Map_Display(Cash,nil,Globe.ActiveMap);

    fc:=fctx.rgb_Ground_Color;
    fMosaic.SetParam(1,fc);  // SetClearColor
    fMosaic.SetParam(2,fc);  // SetTransparent

    proj1:=TWgsMosaic.Create(Self);
    try
      if proj1.sys.pps >= 0 then
      if proj1.GetInterface(IProject2,proj) then begin
        fMosaic.Video(@map,proj,avi,Frame,Frag);
        proj1:=nil
      end;

      proj:=nil;

    finally
      proj1.Free
    end;

    xPos:=ox; yPos:=oy;

    Card_Refresh(nil)
  end
end;

constructor TDrawTile.Create(Globe: TMapProject);
begin
  inherited Create;
  fcard:=TDrawCard.Create(nil);
  fcard.Globe:=Globe;
end;

destructor TDrawTile.Destroy;
begin
  fcard.Free;
  inherited
end;

procedure TDrawTile.Draw(DC: XCanvas; left,top,pixx,pixy: double);
var
  tr,bt: Real3x3;
begin
  tr:=affine_3x3(0,1,-left,
                 -1,0,top);

  if Inverse_3x3(tr,bt) then begin
    dc.BeginPaint(nil);
    fcard.BeforePaint(dc,nil);

    fcard.XPos:=0; fcard.YPos:=0;

    fcard.kz:=1/pixx;
    fcard.fx2_kz.x:=1/pixx;
    fcard.fx2_kz.y:=1/pixy;
    fcard.fis_x2:=pixx <> pixy;

    fcard.fprj_tr:=tr;
    fcard.fprj_bt:=bt;

    fcard.Draw_Maps(dc,nil);
    fcard.AfterPaint(dc);

    dc.EndPaint;
  end;

end;

constructor TProjMosaic.Create(ACard: TDrawCard;
                               const Amap: TMapRec);
var
  t1,t2: Real3x3;
  i: Integer; l,g: GOrient;
begin
  inherited Create;

  fCard:=ACard;
  fLink:=fCard.Globe.Pcx_link;
  fIsPcx:=fCard.IsPcx;

  fmap_tr:=Identify_3x3;
  fmap_bt:=fmap_tr;

  fScale:=1; fsys1:=Amap.lg.s;

  if not fIsPcx then
    fsys2:=fCard.lg_p.sys
  else begin
    fsys2:=fLink.sys; fsys2.pps:=0
  end;

  l:=Amap.lg.l; g:=Amap.lg.g;

  if fsys1.pps = 1 then
  for i:=0 to 3 do with g[i] do
  sys_BL_XY(x,y, fsys1, x,y);

  if prj_equal(fsys1,fsys2) then begin

    fsys1.y0:=prj_ZoneY(fsys1);

    if fsys1.pps = 0 then begin

      if not fIsPcx then
      if fsys2.pps = 1 then
      for i:=0 to 3 do with g[i] do
      y:=y - fsys1.y0;

    end else
    if fCard.IsPcx then
    for i:=0 to 3 do with g[i] do
    y:=y + fsys1.y0;

    fsys1.prj:=-1
  end;

  if not Solve_projective1(@l,@g,fmap_tr,fmap_bt) then
  fsys1.pps:=-1;

  fCard.Get_prj_tr(fprj_tr,fprj_bt)
end;

function TProjMosaic.Project(x,y: Double): TGauss;
var
  g,p: TGauss; r: XGeoid; l: TPoint;
begin
  p.x:=x*fScale; p.y:=y*fScale;

  g:=Projective_3x3(p.x,p.y,fmap_tr);

  if fsys1.prj > 0 then begin

    if fsys1.pps = 1 then
    sys_XY_BL(g.x,g.y, fsys1, g.x,g.y);
    r.x:=g.x; r.y:=g.y; r.s:=fsys1;

    if fIsPcx then
      prj_to_xy(r,fsys2,g)
    else
      g:=prj_inside(r,fsys2)
  end;

  if fIsPcx then begin
    fLink.xy_to_pix(g,p);
    l:=fCard.img_Project(p.x,p.y);
    p.x:=l.X; p.y:=l.Y;
  end
  else p:=Projective_3x3(g.x,g.y,fprj_tr);

  Result:=p
end;

function TProjMosaic.Backup(x,y: Double): TGauss;
var
  g,p: TGauss; r: XGeoid;
begin
  if fIsPcx then begin
    p:=fCard.img_Backup(Round(x),Round(y));
    fLink.pix_to_xy(p.x,p.y,g);
  end
  else g:=Projective_3x3(x,y,fprj_bt);

  if fsys1.prj > 0 then begin
    r.x:=g.x; r.y:=g.y; r.s:=fsys2;
    if r.s.pps = 1 then
    sys_XY_BL(r.x,r.y, r.s, r.x,r.y);

    if fIsPcx then
      g:=prj_inside(r,fsys1)
    else
      g:=prj_inside(r,fsys1)
  end;

  p:=Projective_3x3(g.x,g.y,fmap_bt);
  p.x:=p.x/fScale; p.y:=p.y/fScale;
  Result:=p
end;

function TProjMosaic.Get_d2(out tr,bt: Real3x3): HResult;
begin
  Result:=S_FALSE
end;

function TProjMosaic.Get_poly(out tr,bt: TPolynom): HResult;
begin
  Result:=S_FALSE
end;

function TProjMosaic.Push: HResult;
begin
  Result:=S_FALSE
end;

procedure TProjMosaic.Pop;
begin
end;

procedure TProjMosaic.MoveTo(dx,dy: Double);
begin
end;

procedure TProjMosaic.Scale(kz: Double);
begin
  fScale:=kz
end;

constructor TWgsMosaic.Create(ACard: TDrawCard);
begin
  inherited Create; fCard:=ACard;
  fLink:=fCard.Globe.Pcx_link;
  fIsPcx:=fCard.IsPcx;

  fsys:=fCard.lg_p.sys;
  fsys.y0:=0;

  if fIsPcx then begin
    fsys:=fLink.sys;
    fsys.y0:=prj_zoneY(fsys)
  end;

  fCard.Get_prj_tr(fprj_tr,fprj_bt);
  if fsys.prj = 0 then fsys.pps:=-1
end;

function TWgsMosaic.Project(x,y: Double): TGauss;
var
  g,p: TGauss; l: TPoint; h: Double;
begin
  with fsys do if elp <> 9 then
  WGS_GEO(x,y,0, elp,@dat, x,y,h);

  sys_BL_XY(x,y,fsys,g.x,g.y);

  if fIsPcx then begin
    g.y:=g.y + fsys.y0;
    fLink.xy_to_pix(g,p);
    l:=fCard.img_Project(p.x,p.y);
    p.x:=l.X; p.y:=l.Y;
  end
  else p:=Projective_3x3(g.x,g.y,fprj_tr);

  Result:=p
end;

function TWgsMosaic.Backup(x,y: Double): TGauss;
var
  g,p: TGauss; h: Double;
begin
  if fIsPcx then begin
    p:=fCard.img_Backup(Round(x),Round(y));
    fLink.pix_to_xy(p.x,p.y,g);
    g.y:=g.y - fsys.y0;
  end
  else g:=Projective_3x3(x,y,fprj_bt);

  sys_XY_BL(g.x,g.y,fsys,g.x,g.y);

  with fsys do if elp <> 9 then
  GEO_WGS(g.x,g.y,0, elp,@dat, g.x,g.y,h);

  Result:=g
end;

function TWgsMosaic.Get_d2(out tr,bt: Real3x3): HResult;
begin
  Result:=S_FALSE
end;

function TWgsMosaic.Get_poly(out tr,bt: TPolynom): HResult;
begin
  Result:=S_FALSE
end;

function TWgsMosaic.Push: HResult;
begin
  Result:=S_FALSE
end;

procedure TWgsMosaic.Pop;
begin
end;

procedure TWgsMosaic.MoveTo(dx,dy: Double);
begin
end;

procedure TWgsMosaic.Scale(kz: Double);
begin
end;

end.

