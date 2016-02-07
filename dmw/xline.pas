unit xline; interface

uses
  Classes,Math,Windows,otypes;

const
  line_eps: Double = 0.5;
  line_comp_stop: Boolean = false;

type
  PReal3x3  = ^Real3x3;
  Real3x3 = array[1..3,1..3] of double;
  Real2x2 = array[1..2,1..2] of double;
  Long2x2 = array[1..2,1..2] of longint;

  PLine_coeff = ^TLine_coeff;
  TLine_coeff = record
    x0,y0, a,b,c,ab,ca,cb, len: double;
  end;

  TFigure = class
    procedure Pixel(x,y: Integer); virtual;
    procedure Line(x1,y1,x2,y2: Integer);
  private
    fOnPixel: TPixelProc;
  public
    property OnPixel: TPixelProc write fOnPixel;
  end;

  TRotate = class
    constructor Create;

    function Rotate(Fi: Double): Double;

    function Only_Rotate(X,Y: Double): TGauss;

    function IForw(X,Y: Integer): TPoint;
    function IBack(X,Y: Integer): TPoint;

    function Forw(X,Y: Double): TGauss; virtual;
    function Back(X,Y: Double): TGauss; virtual;

  private
    FCx,FCy,FAngle: Double;
    s,c, _s,_c: Extended;

    function Get_Inverse: Real3x3;

  protected
    procedure Set_Angle(f: Double); virtual;
    function Get_Matrix: Real3x3; virtual;

  public
    property Angle: Double read FAngle write Set_Angle;
    property Cx: Double read FCx write FCx;
    property Cy: Double read FCy write FCy;

    property Matrix: Real3x3 read Get_Matrix;
    property Inverse: Real3x3 read Get_Inverse;
  end;

  TProject = class(TRotate)
    constructor Create;

    function Forw(X,Y: Double): TGauss; override;
    function Back(X,Y: Double): TGauss; override;

  private
    FOrgX: Double;
    FOrgY: Double;
    FScale: Double;
    FGauss: Boolean;
    FWinX: Integer;
    FWinY: Integer;

    FEnabled: Boolean;

  protected
    procedure Set_Scale(AScale: Double); virtual;
    function Get_Matrix: Real3x3; override;

  public
    property OrgX: Double read FOrgX write FOrgX;
    property OrgY: Double read FOrgY write FOrgY;
    property Scale: Double read FScale write Set_Scale;
    property Gauss: Boolean write FGauss;

    property WinX: Integer write FWinX;
    property WinY: Integer write FWinY;

    property Enabled: Boolean read FEnabled write FEnabled;
  end;

function nil_3x3: Real3x3;
function Identify_3x3: Real3x3;

function swap_xy_3x3: Real3x3;

function affine_3x3(a1,a2,a3: Double;
                    b1,b2,b3: Double): Real3x3;

function assign_3x3(a1,a2,a3: Double;
                    b1,b2,b3: Double;
                    c1,c2,c3: Double): Real3x3;

function inverse_3x3(const Tr: Real3x3; out Tr_: Real3x3): Boolean;

procedure Init_3x3(out T: Real3x3; x,y,kx,ky: Double);
procedure Gauss_3x3(out T: Real3x3; x,y,kx,ky: Double);
procedure Begin_3x3(out T: Real3x3; x,y: Double);

function norm_3x3(var T: Real3x3): Double;

procedure Denorm_3x3(const A: Real3x3; out B: Real3x3);

function axe_3x3(x,y: Double): Real3x3;
function Rotate_3x3(fi: Double): Real3x3;
function Right_3x3(fi: Double): Real3x3;

function Transit_3x3(x,y: Double; const T: Real3x3): TGauss;
function Projective_3x3(x,y: Double; const T: Real3x3): TGauss;

function qProjective_3x3(x,y: Double;
                         const T: Real3x3;
                         out p: TGauss): Boolean;

function iTransit_3x3(x,y: Double; const T: Real3x3): TPoint;
function iProjective_3x3(x,y: Double; const T: Real3x3): TPoint;

function gTransit_3x3(const g: TGauss; const T: Real3x3): TGauss;

function Is_Projective_3x3(const T: Real3x3): Boolean;

procedure Mult_3x3(const A,B: Real3x3; out C: Real3x3);
procedure Next_3x3(var T: Real3x3; const B: Real3x3);
procedure Forw_3x3(var T: Real3x3; const B: Real3x3);
procedure Scale_3x3(var T: Real3x3; k: double);
procedure t_Scale_3x3(var T: Real3x3; k: Double);
procedure t_Move_3x3(var T: Real3x3; x,y: Double);
procedure fi_Rotate_3x3(var T: Real3x3; fi: Double);
procedure xy_Rotate_3x3(var T: Real3x3; x,y: Double);
procedure xy_Scale_3x3(var T: Real3x3; kx,ky: Double);

function Minus_2x2(var T: Real2x2): Boolean;

procedure y_swap_3x3(var T: Real3x3);
procedure xy_swap_3x3(var T: Real3x3);

function swap_3x3(const T: Real3x3): Real3x3;

function Get_scale_3x3(const T: Real3x3): Double;

procedure vec_scale(X: PDoubles; N: Integer; k: Double);

function Norm_Degree(Fi: Integer): Integer;
function xNorm_Degree(Fi: Double): Double;
function xPi_Degree(Fi: Double): Double;

function Norm_Angle(r: Double): Double;
function pi_Angle(r: Double): Double;

function df_Angle(f1,f2: Double): Double;
function abs_Angle(f1,f2: Double): Double;

function xSin(degree: double): double;
function xCos(degree: double): double;
function xTan(degree: double): double;
function xCotan(degree: double): double;

function xArcTan(x1,y1,x2,y2: double): double;
function iArcTan(x1,y1,x2,y2: longint): double;

function xAngle(x1,y1,x2,y2,x3,y3: double): double;
function sAngle(x1,y1,x2,y2,x3,y3: double): double;

function xAngled(const a,b,c: TGauss): double;
function sAngled(const a,b,c: TGauss): double;

function iAngle(const a,b: TPoint): double;
function nAngle(const a,b: TPoint): double;

function gAngle(const a,b: TGauss): double;
function xy_Angle(G: PGPoly): double;
function gk_Angle(G: PGPoly): double;

function Azimuth(f: Double): Double;
function xAzimuth(const a,b: TGauss): Double;
function iAzimuth(const a,b: TPoint): Double;

function gauss_length(const g: TGauss): Double;
function gauss_inc(const p,d: TGauss): TGauss;
function gauss_dec(const p,d: TGauss): TGauss;
function gauss_dir(const a,b: TGauss): TGauss;

procedure inc_gauss(var p: TGauss; const d: TGauss);

function step_xy(x1,y1,x2,y2, r,df: Double): TGauss;
function inter_v(x1,y1,x2,y2, x: Double): Double;
function inter_i(x1,y1,x2,y2, x: Double): Integer;

function inter_point(const p1,p2: TPoint;
                     r1,r2,r: Double): TPoint;

procedure inter_vpoint(const v1,v2: VPoint; var v: VPoint);

function inter_2i(const a,b: TPoint; k: Double): TPoint;
function inter_3i(const a,b: VPoint; k: Double): VPoint;

function prj_xy(x,y, a,b,f: Double): tgauss;
function prj_gauss(const c: tgauss; r,f: double): tgauss;
function prj_LPoint(const c: TPoint; r,f: double): TPoint;

function gauss_forw(const p,n: tgauss; r: double): tgauss;
function point_forw(const p,d: TPoint; r: double): TPoint;

function point_dir(const a,b: TPoint): TPoint;

procedure Get_F_Point(x,y: Integer; a,b,f: double; out p: TPoint);

procedure Get_tube(ax,ay,bx,by,cx,cy, r1,r2: Double;
                   out lx,ly,rx,ry: Double);

procedure Get_Tube_Points(a,b,c: TPoint; r1,r2: Double;
                          out b1,b2: TPoint);

function Stretch_dir(const a,b: TPoint; r: double): TGauss;

function Stretch_Point(const a,b: TPoint; r1,r2: double;
                       out p1,p2: TPoint): Boolean;

function Stretch_Gauss(const a,b: TGauss; r1,r2: double;
                       out p1,p2: TGauss): Boolean;

procedure Get_Two_P_Points(const a,b: TPoint; r: double; out p1,p2: TPoint);
procedure Up_Point(const a,b: TPoint; r: double; out p: TPoint);

function Get_Next_Point(const a,b: TPoint; r: double;
                        out p: TPoint): Boolean;

function Next_LPoint(const p, a,b: TPoint; k: double): TPoint;

procedure Move_Vector(var v: LVector; r: double);
procedure Step_Next_Point(const a,b: TPoint; out p: TPoint);

procedure Get_Norm_Point(const a,b: TPoint; d,h: double;
                         var p: TPoint);

procedure Get_Thick_Points(const p,q: TPoint; a,b: double;
                           var p1,p2: TPoint);

function Middle_Point(const a,b: TPoint; out p: TPoint): Boolean;
procedure Middle_Gauss(const a,b: tgauss; out p: tgauss);

function Next_Gauss(const a,b: tgauss; r: double): tgauss;
function Move_Gauss(const p,a,b: tgauss; r: double): tgauss;

function xCentre(x1,x2: double): double;
function iCentre(x1,x2: longint): longint;
function iMult(a,b: longint): Double;
function xModule(x,y: double): double;
function xNorm(var g: tgauss): Boolean;          

function d2_scalar(x1,x2,y1,y2: double): double;
function d2_vect(x1,x2,y1,y2: double): double;
function d2_angle(x1,x2,y1,y2: double): double;

function i2_scalar(x1,x2,y1,y2: longint): double;
function i2_vect(x1,x2,y1,y2: longint): double;

function g2_scalar(const v1,v2: TGauss): double;

function iSquare(const a,b: TPoint): double;

function Int_cos(const a,b, c,d: TPoint;
                 minLen: double; out cos: double): bool;

function Gauss_cos(const a,b, c,d: TGauss;
                   minLen: double; out cos: double): bool;

function Gauss_cos3(const a,b, c: TGauss;
                   minLen: double; out cos: double): bool;

function Int_cos3(const a,b, c: TPoint;
                  minLen: double; out cos: double): bool;

function Int_cos2(const v1, v2: TPoint;
                  minLen: double; out cos: double): bool;

function Int_Dist(const p1,p2: IPoint): double;
function Long_Dist(const p1,p2: TPoint): double;
function Points_Dist(const p1,p2: TPoint): double;
function VPoints_Dist(const p1,p2: VPoint): double;
function Gauss_Dist(const g1,g2: TGauss): double;

function Sizes_Equal(const s1,s2: TSize): Boolean;
function Points_Equal(const p1,p2: TPoint): Boolean;
function VPoints_Equal(const p1,p2: VPoint): Boolean;
function Gauss_Equal(const p1,p2: tgauss; eps: Double): Boolean;
function xy_equal(const a,b: tgauss; eps: Double): Boolean;

function Gauss_res(const g1,g2, p1,p2: tgauss): Double;

function dir_Equal(const p1,p2, q1,q2: TPoint): Boolean;

procedure Swap_LPoints(var a,b: TPoint);
procedure Swap_VPoints(var a,b: vpoint);
procedure Swap_TGauss(var a,b: tgauss);
procedure Swap_LPort(var lt,rb: TPoint);
procedure Swap_GPort(var lt,rb: tgauss);

function Get_Normal(const p1,p2: TPoint): TPoint;
function Get_Direction(const p1,p2: TPoint): TPoint;

function z_axe_line(const p1,p2,p: TPoint;
                    z1,z2: Double): Double;

function OnLine(ax,ay,bx,by, px,py: double): double;

function gOnLine(const a,b, p: TGauss): double;

function iOnLine(const a,b, p: TPoint): double;
function iOnLine1(const a,b, p: TPoint): Integer;

function InTriangle(lp: PGPoly; x,y: Double;
                    out dist: double): Integer;

function TriangleCentre(const a,b,c: TPoint): TPoint;

function Triangle_Verify(lp: PLPoly; eps: Float): Boolean;
function xTriangle_Verify(lp: PGPoly; eps: Float): Boolean;

function Dir_to_Line(x,y: Double; const p1,p2: tgauss): Double;
function iDir_to_Line(const p, q1,q2: TPoint): Double;

function Get_line_coeff(x1,y1,x2,y2: Double;
                        out line: TLine_coeff): Double;

function Get_line_coeffp(const a,b: TPoint;
                         out line: TLine_coeff): Double;

function Get_line_coeffg(const a,b: tgauss;
                         out line: TLine_coeff): Double;

function Get_line_tk(const l: TLine_coeff;
                     x,y: Double; out tk: double): Boolean;

function c_prj_to_line(const l: TLine_coeff;
                       x,y: Double; out p: TGauss): Boolean;

function c_prj_to_line1(const l: TLine_coeff;
                        x,y: Double; out p: TGauss): int;

function c_dist_to_line(const l: TLine_coeff; x,y: Double): Double;

function p_dist_to_line(const l: TLine_coeff;
                        x,y: Double; out dist: Double): bool;

function c_plot_to_line(const l: TLine_coeff;
                        x,y,eps: Double; dist: PDouble;
                        out p: TGauss): Boolean;

function c_plot_to_line1(const l: TLine_coeff;
                         x,y,eps: Double; dist: PDouble;
                         out p: TGauss): Boolean;
                        
function c_pull_to_line(const l: TLine_coeff;
                        var p: TGauss; eps: Double): Boolean;

function c_line_get_x(const l: TLine_coeff;
                      y: Double; out x: Double): bool;

function xDist_to_Line(x,y, x1,y1,x2,y2: Double): double;
function gDist_to_Line(const p, q1,q2: TGauss): double;
function Dist_to_Line(const p, q1,q2: TPoint): double;

function xLine_Contains_pt(x1,y1,x2,y2, x,y, eps: Double): Boolean;

function prj_to_Line(const p ,q1,q2: TPoint; out pc: TGauss): Boolean;
function xprj_to_Line(const p ,q1,q2: TPoint; out pc: TGauss): Boolean;

function x_prj_to_Line(const p ,q1,q2: TGauss; out pc: TGauss): Boolean;

function Project_to_Line(const p ,q1,q2: TPoint; out pc: TPoint): Boolean;

function Plot_to_Line(const p ,q1,q2: TPoint; out pc: TPoint): Boolean;

function Plot_to_Line1(const p ,q1,q2: TPoint; out pc: TPoint): Integer;

function Plot_to_Line2(const p ,q1,q2,q3: TPoint): Boolean;

function Plot_to_XLine(const p,q1,q2: TGauss;
                       out cp: TGauss): Boolean;

function xPlot_to_Line(const p ,q1,q2: TPoint;
                       dr: double; out pc: TPoint;
                       out dist: double): Boolean;

function t_plot(const p1,p2: TPoint; t: Double): TPoint;
function t_back(const p1,p2, p: TPoint): Double;

function Ort_to_Line(const p ,q1,q2: TPoint; out c: TPoint): Boolean;
function xOrt_to_Line(const p ,q1,q2: TPoint; out c: TPoint): Boolean;

function OrtToLine(const p ,q1,q2: TGauss; out c: TGauss): Boolean;
function xOrtToLine(const p ,q1,q2: TGauss; out c: TGauss): Boolean;

function Locate_Line(const a,b, lt,rb: TPoint; out c: TPoint): Boolean;

function Ribs_Equal(const p1,p2, q1,q2: TPoint): Boolean;

function xLL2(a1,b1,c1, a2,b2,c2: double; out p: tgauss): Boolean;

function x_LL2(const p1: tgauss; k1: Double;
               const p2: tgauss; k2: Double;
               out p: tgauss): Boolean;

function LL2(x1,y1,x2,y2, x3,y3,x4,y4, k1,k2: double; out c: tgauss): Boolean;

function vLL2(const p1,v1, p2,v2: TGauss; out c: tgauss): Boolean;

function gLL2(const p1,p2, p3,p4: TGauss;
              k1,k2: Double; out c: tgauss): Boolean;

function LL2gk(const p1,p2, p3,p4: TGauss;
               out c: TGauss; out k: double): Boolean;

function LL2ik(const p1,p2, p3,p4: TPoint;
               out c: TGauss; out k: double): Boolean;

function LL2i(const p1,p2, p3,p4: TPoint;
              k1,k2: double; out c: TGauss): Boolean;

function LL2ip(const p1,p2, p3,p4: TPoint;
               k1,k2: double; out c: TPoint): Boolean;

function LL2icg(const a1,a2, b1,b2: TPoint; out c: TGauss): Boolean;
function LL2ic(const a1,a2, b1,b2: TPoint; out c: TPoint): Boolean;

function LL2icg1(const a1,a2, b1,b2: TPoint; out c: TGauss): Boolean;
function LL2ic1(const a1,a2, b1,b2: TPoint; out c: TPoint): Boolean;

function ILL2(const p1,p2, p3,p4: TPoint; out c: TPoint): Boolean;
function ILLD(const p1,p2, p3,p4: TPoint; out c: TPoint): Boolean;
function i_Cross(const p1,p2, q1,q2: TPoint; out c: TPoint): Boolean;
function g_Cross(const p1,p2, q1,q2: tgauss; out c: tgauss): Boolean;

function LL_FF(x1,y1, x2,y2, f1,f2: Double;
               out cx,cy: Double): Boolean;

function GLL_FF(g1,g2: tgauss; f1,f2: Double;
                out g: tgauss): Boolean;

function LL_RR(x1,y1, x2,y2, r1,r2: Double;
               out cx1,cy1,cx2,cy2: Double): Boolean;

function GLL_RR(const c1,c2: tgauss; r1,r2: Double;
                out p1,p2: tgauss): Boolean;

function ILL_RR(const c1,c2: TPoint; r1,r2: Double;
                out p1,p2: TPoint): Boolean;

function LR_Sector(lp: PGPoly; r: Double;
                   out fi: Double): Integer;

function LF_Sector(lp: PGPoly; fi: Double;
                   out r: Double): Integer;

function CutLine(qx,qy,px,py, cx: Double): Double;

procedure Rotate_LPoint(const a,b,c, p: TPoint;
                        out q: TPoint);

function Rotate_VPoint(const a,b,c: TPoint;
                       const v: vpoint): vpoint;

function Azimuth_Point(const p,c: TPoint; fi: Double): TPoint;
function fi_Rotate_Point(const p,c: TPoint; fi: Double): TPoint;

function Double_Point(const a,b,c,d: TPoint; r: double;
                      out p: TPoint): Boolean;

function Join_Points(const a,b,c,d: TPoint; r: double;
                     out p: TPoint): Boolean;

procedure Mirror_Point(const p,c: TPoint; var q: TPoint);

procedure Rotate_Gauss(a,b,c, p: tgauss; var q: tgauss);
procedure Mirror_Gauss(const p,c: tgauss; var q: tgauss);

function LineContainsPoint(const a,b, p: TPoint): Boolean;
function LineContainsPoint1(const a,b, p: TPoint): Boolean;
function LineContainsLine(const a,b, q,p: TPoint): Boolean;

function LineContainsEdge(const a,b: TPoint;
                          const max_r: Double;
                          lp: PPoly; lp_n: Integer;
                          out pc: TPoint): Integer;

function LineContainsPoly(const a,b: TPoint;
                          const max_r: Double;
                          lp: PLLine; lp_i: Integer;
                          dir: Integer): Integer;

function PortContainsPoint(const lt,rb: TPoint; x,y: Integer): Boolean;
function FrameContainsPoint(const lt,rb: TPoint; x,y,r: Integer): Boolean;

function PortContainsRect(const lt,rb: TPoint; x1,y1,x2,y2: Integer): Boolean;
function PortContainsPort(const lt,rb, _lt,_rb: TPoint): Boolean;
function PortContainsLine(const lt,rb, a,b: TPoint): Boolean;

function BoundContainsRect(const lt,rb: TPoint; x1,y1,x2,y2: Integer): Boolean;
function BoundContainsPort(const lt,rb, _lt,_rb: TPoint): Boolean;

function IsInnerPort(const lt,rb, _lt,_rb: TPoint): Boolean;

function RectContainsRect(const Owner,Inner: TRect): Boolean;
function RectContainsPoint(const R: TRect; const P: TPoint): Boolean;
function RectClipRect(const Owner,Inner: TRect; Delta: Integer): Boolean;
function IncClipRect(const R: TRect; d: Integer): TRect;

function RingContainsPoint(cx,cy, r, px,py: Double): Boolean;

function GaussContainsPoint(const lt,rb, p: tgauss): Boolean;

function GaussContainsRect(const lt,rb: tgauss;
                           x1,y1,x2,y2: double): Boolean;

function GaussContainsPort(const lt,rb, _lt,_rb: tgauss): Boolean;

function GaussContainsLine(const lt,rb, a,b: tgauss): Boolean;

function Rect_Contains(const R: TRect; x,y: integer): Boolean;
procedure Swap_Rect(var R: TRect);

function Point_ContainsPoint(cx,cy,d, x,y: integer): Boolean;

procedure Swap_lRect(const a,b: TPoint; out c,d: TPoint);
procedure Mult_lRect(const a,b: TPoint; k: float; out c,d: TPoint);
procedure Scale_lPort(var a,b: TPoint; k: double);
procedure View_lPort(var a,b: TPoint; k: double);
procedure Clip_lRect(const a,b, lt,rb: TPoint; out c,d: TPoint);

function Locator(X,Y,R: int): LVector;

function Clip_lPort(const R: TRect; var lt,rb: TPoint): Boolean;
function Clip_gPort(const R: TRect; var lt,rb: TGauss): Boolean;

procedure Inc_lRect(var a,b: TPoint; dx,dy: int);
procedure move_lRect(var a,b: TPoint; dx,dy: int);
procedure Trunc_lRect(var a,b: TPoint; x1,y1,x2,y2: longint);
procedure max_LRect(var a,b: TPoint; w,h, x1,y1,x2,y2: longint);

procedure Max_LPort(var lt,rb: TPoint; const p: TPoint);
procedure Max_VPort(var lt,rb: vpoint; const p: vpoint);
procedure Add_LRect(var lt,rb: TPoint; const a,b: TPoint);
procedure Add_VRect(var lt,rb: vpoint; const a,b: vpoint);
procedure Centre_lRect(const lt,rb: TPoint; var a,b: TPoint);

function Track_lRect(var R: TRect; const p: TPoint): Boolean;

procedure Locate_Centre(const Port: TRect; var P: TPoint);

procedure Inc_GRect(var a,b: tgauss; dx,dy: double);
procedure Max_GPort(var lt,rb: tgauss; const p: tgauss);
procedure Mult_GRect(const a,b: tgauss; k: double; var c,d: tgauss);
procedure Scale_GPort(var a,b: TGauss; k: double);
procedure Clip_GRect(const a,b, lt,rb: tgauss; var c,d: tgauss);
procedure Swap_GRect(const a,b: tgauss; var c,d: tgauss);
procedure Add_GRect(var lt,rb: tgauss; const a,b: tgauss);
procedure Centre_GRect(const lt,rb: tgauss; var a,b: tgauss);

procedure Up_Line(const a,b: TPoint; h: double; var c,d: TPoint);

function Get_Regular(x1,y1,x2,y2, n,lp_max: Integer; lp: PLPoly): Integer;

function Clip_GPoly(lp: PGPoly; n: Integer; lx,rx: double): Integer;
function Clip_Line(const lt,rb, a,b: TPoint; out c,d: TPoint): Boolean;

function xClip_Line(const lt,rb, a,b: TPoint; out c,d: TPoint): Boolean;

function rClip_Line(const R: TRect;
                    const a,b: TPoint;
                    out c,d: TPoint): Boolean;

function yClip_Line(w,h: Integer; const a,b: TPoint;
                    out c,d: TPoint): Boolean;

implementation

uses
  Sysutils;

const
  Pi2 = Pi*2;

var
  Pi_2: double;

procedure TFigure.Pixel(x,y: Integer);
begin
  if Assigned(fOnPixel) then
  fOnPixel(x,y)
end;

procedure TFigure.Line(x1,y1,x2,y2: Integer);
var
  x,y,dx,dy,d,inc1,inc2: Integer;
  done,_xy,x_sign,y_sign: Boolean;
begin
  _xy:=true; repeat done:=true;

    dx:=x2 - x1; x_sign:=dx < 0; dx:=Abs(dx);
    dy:=y2 - y1; y_sign:=dy < 0; dy:=Abs(dy);

    if dx < dy then begin
      _xy:=false; done:=false;
      x:=x1; x1:=y1; y1:=x; x:=x2; x2:=y2; y2:=x
    end;
  until done;

  inc1:=dy shl 1; inc2:=(dy - dx) shl 1; d:=inc1 - dx;

  x:=x1; if x_sign then dx:=-1 else dx:=1;
  y:=y1; if y_sign then dy:=-1 else dy:=1;

  if _xy then Pixel(x,y) else Pixel(y,x);

  while x <> x2 do begin Inc(x,dx);

    if d < 0 then Inc(d,inc1) else
    begin Inc(d,inc2); Inc(y,dy) end;

    if _xy then Pixel(x,y) else Pixel(y,x)
  end
end;

constructor TRotate.Create;
begin
  inherited; Angle:=0
end;

function TRotate.Rotate(Fi: Double): Double;
begin
  Result:=FAngle + Fi;

  while Result < 0 do
  Result:=Result + 2*Pi;

  while Result >= 2*Pi  do
  Result:=Result - 2*Pi;

  Angle:=Result
end;

function TRotate.Only_Rotate(X,Y: Double): TGauss;
begin
  x:=x-FCx; y:=y-FCy;
  Result.x:=FCx + c*x + s*y;
  Result.y:=FCy - s*x + c*y;
end;

function TRotate.IForw(X,Y: Integer): TPoint;
var
  p: tgauss;
begin
  p:=Forw(X,Y);
  Result.x:=Round(p.x);
  Result.y:=Round(p.y)
end;

function TRotate.IBack(X,Y: Integer): TPoint;
var
  p: tgauss;
begin
  p:=Back(X,Y);
  Result.x:=Round(p.x);
  Result.y:=Round(p.y)
end;

function TRotate.Forw(x,y: Double): TGauss;
begin
  x:=x-FCx; y:=y-FCy;
  Result.x:=FCx + c*x + s*y;
  Result.y:=FCy - s*x + c*y;
end;

function TRotate.Get_Matrix: Real3x3;
var
  T: Real3x3;
begin
  Begin_3x3(T,-fCx,-fCy);
  fi_Rotate_3x3(T,fAngle);
  t_Move_3x3(T,fCx,fCy);
  Result:=T
end;

function TRotate.Get_Inverse: Real3x3;
begin
  Inverse_3x3(Matrix,Result)
end;

function TRotate.Back(x,y: Double): TGauss;
begin
  x:=x-FCx; y:=y-FCy;
  Result.x:=FCx + _c*x + _s*y;
  Result.y:=FCy - _s*x + _c*y;
end;

procedure TRotate.Set_Angle(f: Double);
begin
  FAngle:=f;
  SinCos(f, s,c);
  SinCos(-f, _s,_c)
end;

constructor TProject.Create;
begin
  inherited; FScale:=1;
  FEnabled:=false
end;

procedure TProject.Set_Scale(AScale: Double);
begin
  FScale:=AScale
end;

function TProject.Get_Matrix: Real3x3;
var
  T: Real3x3;
begin
  T:=inherited Get_Matrix;
  xy_swap_3x3(T); y_swap_3x3(T);
  t_Move_3x3(T,-fOrgY,fOrgX);
  t_Scale_3x3(T,fScale);
  t_Move_3x3(T,fWinX,fWinY);
  Result:=T
end;

function TProject.Forw(X,Y: Double): TGauss;
var
  p: tgauss;
begin
  p:=inherited Forw(X,Y);

  if FGauss then begin
    Result.x:=(p.y - FOrgY) * FScale + FWinX;
    Result.y:=(FOrgX - p.x) * FScale + FWinY
  end
  else begin
    Result.x:=(p.x - FOrgX) * FScale + FWinX;
    Result.y:=(p.y - FOrgY) * FScale + FWinY;
  end
end;

function TProject.Back(X,Y: Double): TGauss;
var
  p: tgauss;
begin
  if FGauss then begin
    p.x:=FOrgX - (Y-FWinY)/FScale;
    p.y:=FOrgY + (X-FWinX)/FScale;
  end
  else begin
    p.x:=FOrgX + (X-FWinX)/FScale;
    p.y:=FOrgY + (Y-FWinY)/FScale;
  end;

  Result:=inherited Back(p.x,p.y)
end;

function nil_3x3: Real3x3;
var
  T: Real3x3;
begin
  Fillchar(T,Sizeof(T),0);
  T[3,3]:=0; Result:=T
end;

function swap_xy_3x3: Real3x3;
var
  T: Real3x3;
begin
  T[1,1]:=0; T[1,2]:=1; T[1,3]:=0;
  T[2,1]:=1; T[2,2]:=0; T[2,3]:=0;
  T[3,1]:=0; T[3,2]:=0; T[3,3]:=1;
  Result:=T
end;

function affine_3x3(a1,a2,a3: Double;
                    b1,b2,b3: Double): Real3x3;
var
  T: Real3x3;
begin
  T:=Identify_3x3;
  T[1,1]:=a1; T[1,2]:=a2; T[1,3]:=a3;
  T[2,1]:=b1; T[2,2]:=b2; T[2,3]:=b3;
  Result:=T
end;

function assign_3x3(a1,a2,a3: Double;
                    b1,b2,b3: Double;
                    c1,c2,c3: Double): Real3x3;
var
  T: Real3x3;
begin
  T[1,1]:=a1; T[1,2]:=a2; T[1,3]:=a3;
  T[2,1]:=b1; T[2,2]:=b2; T[2,3]:=b3;
  T[3,1]:=c1; T[3,2]:=c2; T[3,3]:=c3;
  Result:=T
end;

function inverse_3x3(const Tr: Real3x3; out Tr_: Real3x3): Boolean;
const
  Small = 1E-8;
type
  Double3x3 = Array[0..2,0..2] of Extended;
var
  i,j: Integer;
  T: Double3x3; det: Extended;
begin
  Result:=true; Tr_:=Identify_3x3;

  for i:=0 to 2 do
  for j:=0 to 2 do T[i,j]:=Tr[i+1,j+1];

  det:=-T[0,2]*T[1,1]*T[2,0] + T[0,1]*T[1,2]*T[2,0]
       +T[0,2]*T[1,0]*T[2,1] - T[0,0]*T[1,2]*T[2,1]
       -T[0,1]*T[1,0]*T[2,2] + T[0,0]*T[1,1]*T[2,2];

  if Abs(det) < Small then

    Result:=false

  else begin
    Tr_[1,1]:=(-T[1,2]*T[2,1] + T[1,1]*T[2,2]) / det;
    Tr_[1,2]:=(+T[0,2]*T[2,1] - T[0,1]*T[2,2]) / det;
    Tr_[1,3]:=(-T[0,2]*T[1,1] + T[0,1]*T[1,2]) / det;
    Tr_[2,1]:=(+T[1,2]*T[2,0] - T[1,0]*T[2,2]) / det;
    Tr_[2,2]:=(-T[0,2]*T[2,0] + T[0,0]*T[2,2]) / det;
    Tr_[2,3]:=(+T[0,2]*T[1,0] - T[0,0]*T[1,2]) / det;
    Tr_[3,1]:=(-T[1,1]*T[2,0] + T[1,0]*T[2,1]) / det;
    Tr_[3,2]:=(+T[0,1]*T[2,0] - T[0,0]*T[2,1]) / det;
    Tr_[3,3]:=(-T[0,1]*T[1,0] + T[0,0]*T[1,1]) / det;
    norm_3x3(Tr_);
  end
end;

function Identify_3x3: Real3x3;
var
  T: Real3x3;
begin
  T[1,1]:=1; T[1,2]:=0; T[1,3]:=0;
  T[2,1]:=0; T[2,2]:=1; T[2,3]:=0;
  T[3,1]:=0; T[3,2]:=0; T[3,3]:=1;
  Result:=T
end;

procedure Init_3x3(out T: Real3x3; x,y,kx,ky: Double);
begin
  T[1,1]:=kx; T[1,2]:=0;  T[1,3]:=x;
  T[2,1]:=0;  T[2,2]:=ky; T[2,3]:=y;
  T[3,1]:=0;  T[3,2]:=0;  T[3,3]:=1;
end;

procedure Gauss_3x3(out T: Real3x3; x,y,kx,ky: Double);
begin
  T[1,1]:=0;  T[1,2]:=ky; T[1,3]:=y;
  T[2,1]:=kx; T[2,2]:=0;  T[2,3]:=x;
  T[3,1]:=0;  T[3,2]:=0;  T[3,3]:=1;
end;

procedure Begin_3x3(out T: Real3x3; x,y: Double);
begin
  T[1,1]:=1; T[1,2]:=0; T[1,3]:=x;
  T[2,1]:=0; T[2,2]:=1; T[2,3]:=y;
  T[3,1]:=0; T[3,2]:=0; T[3,3]:=1;
end;

function norm_3x3(var T: Real3x3): Double;
var
  i: Integer; lp: PDoubles;
begin
  Result:=T[3,3];
  if Abs(Result) >= 1E-8 then begin

    lp:=@T; for i:=1 to 9 do begin
      lp[0]:=lp[0]/Result; lp:=@lp[1]
    end;

    T[3,3]:=1
  end
end;

procedure Denorm_3x3(const A: Real3x3; out B: Real3x3);
var
  x0,y0,k: Double;
begin
  k:=1 / A[1,1];

  x0:=-A[1,3] * k;
  y0:=-A[2,3] * k;

  Init_3x3(B, x0,y0, k,k);
end;

function axe_3x3(x,y: Double): Real3x3;
begin
  Result[1,1]:=x; Result[1,2]:=0; Result[1,3]:=0;
  Result[2,1]:=0; Result[2,2]:=y; Result[2,3]:=0;
  Result[3,1]:=0; Result[3,2]:=0; Result[3,3]:=1;
end;

function Rotate_3x3(fi: Double): Real3x3;
var
  c,s: Extended;
begin
  SinCos(fi, s,c);
  Result[1,1]:=c;  Result[1,2]:=s; Result[1,3]:=0;
  Result[2,1]:=-s; Result[2,2]:=c; Result[2,3]:=0;
  Result[3,1]:=0;  Result[3,2]:=0; Result[3,3]:=1;
end;

function Right_3x3(fi: Double): Real3x3;
var
  c,s: Extended;
begin
  SinCos(fi, s,c); s:=-s;
  Result[1,1]:=c;  Result[1,2]:=s; Result[1,3]:=0;
  Result[2,1]:=-s; Result[2,2]:=c; Result[2,3]:=0;
  Result[3,1]:=0;  Result[3,2]:=0; Result[3,3]:=1;
end;

function Transit_3x3(x,y: Double; const T: Real3x3): TGauss;
begin
  Result.x:=T[1,1]*x + T[1,2]*y + T[1,3];
  Result.y:=T[2,1]*x + T[2,2]*y + T[2,3]
end;

function Projective_3x3(x,y: Double; const T: Real3x3): TGauss;
var
  f: Double;
begin
  f:=1 / (T[3,1]*x + T[3,2]*y + 1);
  Result.x:=(T[1,1]*x + T[1,2]*y + T[1,3]) * f;
  Result.y:=(T[2,1]*x + T[2,2]*y + T[2,3]) * f
end;

function qProjective_3x3(x,y: Double;
                         const T: Real3x3;
                         out p: TGauss): Boolean;
var
  f: Double;
begin
  Result:=false; p.x:=0; p.y:=0;

  f:=T[3,1]*x + T[3,2]*y + 1;
  if Abs(f) > Small then begin
    p.x:=(T[1,1]*x + T[1,2]*y + T[1,3]) / f;
    p.y:=(T[2,1]*x + T[2,2]*y + T[2,3]) / f;
    Result:=true
  end
end;

function iTransit_3x3(x,y: Double; const T: Real3x3): TPoint;
var
  p: TGauss;
begin
  p:=Transit_3x3(x,y,T);
  Result.X:=Round(p.x);
  Result.Y:=Round(p.y);
end;

function iProjective_3x3(x,y: Double; const T: Real3x3): TPoint;
var
  p: TGauss;
begin
  p:=Projective_3x3(x,y,T);
  Result.X:=Round(p.x);
  Result.Y:=Round(p.y);
end;

function gTransit_3x3(const g: TGauss; const T: Real3x3): TGauss;
begin
  Result:=Transit_3x3(g.x,g.y,T)
end;

function Is_Projective_3x3(const T: Real3x3): Boolean;
begin
  Result:=Abs(T[3,1]) + Abs(T[3,2]) > 1E-10
end;

procedure Mult_3x3(const A,B: Real3x3; out C: Real3x3);
var
  i,j,k: Integer; Ax,Bx,Cx: Extended;
begin
  for i:=1 to 3 do
  for j:=1 to 3 do begin
    Cx:=0; for k:=1 to 3 do begin
      Ax:=A[i,k]; Bx:=B[k,j];
      Cx:=Cx + Ax*Bx;
    end;

    C[i,j]:=Cx
  end
end;

procedure Next_3x3(var T: Real3x3; const B: Real3x3);
var
  A: Real3x3;
begin
  A:=T; Mult_3x3(B,A,T)
end;

procedure Forw_3x3(var T: Real3x3; const B: Real3x3);
var
  A: Real3x3;
begin
  A:=T; Mult_3x3(B,A,T); norm_3x3(T)
end;

procedure Scale_3x3(var T: Real3x3; k: double);
var
  i,j: Integer;
begin
  for i:=1 to 3 do for j:=1 to 3 do
  T[i,j]:=T[i,j]*k; T[3,3]:=1
end;

procedure t_Scale_3x3(var T: Real3x3; k: Double);
var
  A,B: Real3x3;
begin
  B[1,1]:=k; B[1,2]:=0; B[1,3]:=0;
  B[2,1]:=0; B[2,2]:=k; B[2,3]:=0;
  B[3,1]:=0; B[3,2]:=0; B[3,3]:=1;
  A:=T; Mult_3x3(B,A,T)
end;

procedure xy_Scale_3x3(var T: Real3x3; kx,ky: Double);
var
  A,B: Real3x3;
begin
  B[1,1]:=kx; B[1,2]:=0;  B[1,3]:=0;
  B[2,1]:=0;  B[2,2]:=ky; B[2,3]:=0;
  B[3,1]:=0;  B[3,2]:=0;  B[3,3]:=1;
  A:=T; Mult_3x3(B,A,T)
end;

procedure t_Move_3x3(var T: Real3x3; x,y: Double);
var
  A,B: Real3x3;
begin
  Begin_3x3(B,x,y);
  A:=T; Mult_3x3(B,A,T)
end;

procedure cs_Rotate_3x3(var T: Real3x3; c,s: Double);
var
  A,B: Real3x3;
begin
  B[1,1]:=c;  B[1,2]:=s; B[1,3]:=0;
  B[2,1]:=-s; B[2,2]:=c; B[2,3]:=0;
  B[3,1]:=0;  B[3,2]:=0; B[3,3]:=1;
  A:=T; Mult_3x3(B,A,T)
end;

procedure fi_Rotate_3x3(var T: Real3x3; fi: Double);
var
  c,s: Extended;
begin
  SinCos(fi, s,c);
  cs_Rotate_3x3(T, c,s);
end;

procedure xy_Rotate_3x3(var T: Real3x3; x,y: Double);
var
  l: double;
begin
  l:=Hypot(x,y); if l > 0 then
  cs_Rotate_3x3(T, x/l,y/l);
end;

procedure y_swap_3x3(var T: Real3x3);
begin
  Next_3x3(T,axe_3x3(1,-1))
end;

procedure xy_swap_3x3(var T: Real3x3);
begin
  Forw_3x3(T,swap_xy_3x3);
end;

function Minus_2x2(var T: Real2x2): Boolean;
var
  a,b,c,d,k: double;
begin
  a:=T[1,1]; b:=T[1,2];
  c:=T[2,1]; d:=T[2,2];

  Result:=false; k:=a*d-b*c;

  if Abs(k) > Small then begin
    T[1,1]:=d/k;  T[1,2]:=-b/k;
    T[2,1]:=-c/k; T[2,2]:=a/k;
    Result:=true;
  end
end;

function swap_3x3(const T: Real3x3): Real3x3;
var
  i,j: Integer; S: Real3x3;
begin
  for i:=1 to 3 do for j:=1 to 3 do
  S[i,j]:=T[j,i]; Result:=S
end;

function Get_scale_3x3(const T: Real3x3): Double;
begin
  Result:=Hypot(T[1,1],T[1,2])
end;

procedure vec_scale(X: PDoubles; N: Integer; k: Double);
var
  i: Integer;
begin
  for i:=1 to N do begin
    X[0]:=X[0] * k; X:=@X[1]
  end
end;

function Norm_Degree(Fi: Integer): Integer;
begin
  while Fi < 0 do Inc(Fi,360);
  while Fi >= 360 do Dec(Fi,360);
  Result:=Fi
end;

function xNorm_Degree(Fi: Double): Double;
begin
  while Fi < 0 do Fi:=Fi + 360;
  while Fi >= 360 do Fi:=Fi - 360;
  Result:=Fi
end;

function xPi_Degree(Fi: Double): Double;
begin
  while Fi < -180 do Fi:=Fi + 360;
  while Fi > 180 do Fi:=Fi - 360;
  Result:=Fi
end;

function Norm_Angle(r: Double): Double;
var
  pi2: Double;
begin
  pi2:=2*Pi;
  while r < 0 do r:=r + pi2;
  while r > pi2 do r:=r - pi2;
  Result:=r
end;

function pi_Angle(r: Double): Double;
var
  pi2: Double;
begin
  pi2:=2*Pi;
  while r < -Pi do r:=r + pi2;
  while r > +Pi do r:=r - pi2;
  Result:=r
end;

function df_Angle(f1,f2: Double): Double;
var
  df: Double;
begin
  df:=f2-f1;
  while df > Pi do df:=df-Pi2;
  while df < -Pi do df:=df+Pi2;
  Result:=df
end;

function abs_Angle(f1,f2: Double): Double;
begin
  Result:=Abs(df_Angle(f1,f2))
end;

function xSin(degree: double): double;
begin
  Result:=Sin(degree/180*Pi)
end;

function xCos(degree: double): double;
begin
  Result:=Cos(degree/180*Pi)
end;

function xTan(degree: double): double;
begin
  Result:=Tan(degree/180*Pi)
end;

function xCotan(degree: double): double;
begin
  Result:=Cotan(degree/180*Pi)
end;

function xArcTan(x1,y1,x2,y2: double): double;
begin
  Result:=ArcTan2(y2-y1,x2-x1)
end;

function iArcTan(x1,y1,x2,y2: longint): double;
begin
  Result:=ArcTan2(y2-y1,x2-x1)
end;

function xAngle(x1,y1,x2,y2,x3,y3: double): double;
begin
  Result:=Abs(ArcTan2(y2-y1,x2-x1)-ArcTan2(y3-y1,x3-x1))
end;

function sAngle(x1,y1,x2,y2,x3,y3: double): double;
begin
  Result:=ArcTan2(y2-y1,x2-x1)-ArcTan2(y3-y1,x3-x1);
  Result:=Abs(pi_Angle(Result));
end;

function xAngled(const a,b,c: TGauss): double;
begin
  Result:=ArcTan2(b.y-a.y,b.x-a.x)-
          ArcTan2(c.y-a.y,c.x-a.x);
end;

function sAngled(const a,b,c: TGauss): double;
begin
  Result:=xAngled(a,b,c);
  Result:=Abs(pi_Angle(Result));
end;

function iAngle(const a,b: TPoint): double;
begin
  Result:=ArcTan2(b.Y-a.Y,b.X-a.X);
  if Result < 0 then Result:=Result+2*Pi
end;

function nAngle(const a,b: TPoint): double;
begin
  Result:=pi_Angle( ArcTan2(b.Y-a.Y,b.X-a.X) )
end;

function gAngle(const a,b: TGauss): double;
begin
  Result:=ArcTan2(b.y-a.y,b.x-a.x);
  if Result < 0 then Result:=Result+2*Pi
end;

function xy_Angle(G: PGPoly): double;
begin
  Result:=Abs(ArcTan2(G[1].y-G[0].y,G[1].x-G[0].x)-
              ArcTan2(G[2].y-G[0].y,G[2].x-G[0].x))
end;

function gk_Angle(G: PGPoly): double;
begin
  Result:=Abs(ArcTan2(G[1].x-G[0].x,G[1].y-G[0].y)-
              ArcTan2(G[2].x-G[0].x,G[2].y-G[0].y))
end;

function Azimuth(f: Double): Double;
begin
  Result:=Norm_Angle(f)
end;

function xAzimuth(const a,b: TGauss): Double;
begin
  Result:=ArcTan2(b.y - a.y,b.x - a.x);
  Result:=Norm_Angle(Result)
end;

function iAzimuth(const a,b: TPoint): Double;
begin
  Result:=ArcTan2(b.y - a.y,b.x - a.x);
  Result:=Norm_Angle(Result)
end;

function gauss_length(const g: TGauss): Double;
begin
  Result:=Hypot(g.x,g.y)
end;

function gauss_inc(const p,d: TGauss): TGauss;
begin
  Result.x:=p.x + d.x; Result.y:=p.y + d.y;
end;

function gauss_dec(const p,d: TGauss): TGauss;
begin
  Result.x:=p.x - d.x; Result.y:=p.y - d.y;
end;

function gauss_dir(const a,b: TGauss): TGauss;
begin
  Result.x:=b.x - a.x; Result.y:=b.y - a.y;
end;

procedure inc_gauss(var p: TGauss; const d: TGauss);
begin
  p.x:=p.x + d.x; p.y:=p.y + d.y;
end;

function step_xy(x1,y1,x2,y2, r,df: Double): tgauss;
var
  f: Double;
begin
  f:=xArcTan(x1,y1,x2,y2) + df;
  Result:=prj_xy(x1,y1, r,r,f)
end;

function inter_v(x1,y1,x2,y2, x: Double): Double;
var
  dx: Double;
begin
  Result:=y1;
  dx:=x2-x1; if dx > Small then
  Result:=y1 + (x-x1)*(y2-y1)/dx
end;

function inter_i(x1,y1,x2,y2, x: Double): Integer;
begin
  Result:=Round(inter_v(x1,y1,x2,y2, x))
end;

function inter_point(const p1,p2: TPoint;
                     r1,r2,r: Double): TPoint;
begin
  Result.X:=inter_i(r1,p1.X,r2,p2.X,r);
  Result.Y:=inter_i(r1,p1.Y,r2,p2.Y,r);
end;

procedure inter_vpoint(const v1,v2: VPoint; var v: VPoint);
var
  r1,r2: Double;
begin
  v.z:=v1.z;
  r1:=Hypot(v2.x-v1.x,v2.y-v1.y);
  r2:=Hypot(v.x-v1.x,v.y-v1.y);

  if r1 > 0.1 then
  v.z:=v1.z + Round(r2/r1*(v2.z-v1.z))
end;

function inter_2i(const a,b: TPoint; k: Double): TPoint;
begin
  Result.X:=a.X + Round((b.X - a.X)*k);
  Result.y:=a.Y + Round((b.Y - a.Y)*k);
end;

function inter_3i(const a,b: VPoint; k: Double): VPoint;
begin
  Result.x:=a.x + Round((b.x - a.x)*k);
  Result.y:=a.y + Round((b.y - a.y)*k);
  Result.z:=a.z + Round((b.z - a.z)*k);
end;

function prj_xy(x,y, a,b,f: Double): tgauss;
var
  s,c: Extended;
begin
  SinCos(f, s,c);
  Result.x:=x + (c * a);
  Result.y:=y + (s * b)
end;

function prj_gauss(const c: tgauss; r,f: double): tgauss;
begin
  Result:=prj_xy(c.x,c.y, r,r,f)
end;

function prj_LPoint(const c: TPoint; r,f: double): TPoint;
var
  g: tgauss;
begin
  g:=prj_xy(c.X,c.Y, r,r,f);
  Result.X:=Round(g.x);
  Result.Y:=Round(g.y)
end;

function gauss_forw(const p,n: tgauss; r: double): tgauss;
begin
  Result.x:=p.x + n.x*r;
  Result.y:=p.y + n.y*r;
end;

function point_forw(const p,d: TPoint; r: double): TPoint;
var
  n: TGauss; l: Double;
begin
  n.x:=0; n.y:=0; l:=Hypot(d.X,d.Y);
  if l > Small then begin
    n.x:=d.X / l; n.y:=d.Y / l;
  end;

  Result.x:=p.x + Round(n.x*r);
  Result.y:=p.y + Round(n.y*r);
end;

function point_dir(const a,b: TPoint): TPoint;
begin
  Result.X:=b.X-a.X;
  Result.Y:=b.Y-a.Y;
end;

procedure Get_F_Point(x,y: Integer; a,b,f: double; out p: TPoint);
var
  g: tgauss;
begin
  g:=prj_xy(x,y, a,b,f);
  p.x:=Round(g.x); p.y:=Round(g.y);
end;

function Stretch_line(x1,y1,x2,y2: Double; r: double;
                      out _x1,_y1,_x2,_y2: Double): Boolean;
var
  nx,ny,len: double;
begin
  Result:=false;
  _x1:=x1; _y1:=y1; _x2:=x2; _y2:=y2;

  nx:=y1-y2; ny:=x2-x1; len:=Hypot(nx,ny);

  if len > 0.001 then begin
    nx:=nx*r/len; ny:=ny*r/len;
    _x1:=x1 + nx; _y1:=y1 + ny;
    _x2:=x2 + nx; _y2:=y2 + ny;
    Result:=true
  end
end;

procedure Get_tube_side(ax,ay,bx,by,cx,cy, r: Double;
                        out _x,_y: Double);
var
  x1,y1,x2,y2, x3,y3,x4,y4: Double; c: tgauss;
begin
  _x:=bx; _y:=by;

  if Stretch_line(ax,ay,bx,by, r, x1,y1,x2,y2) then

    if Stretch_line(bx,by,cx,cy, r, x3,y3,x4,y4) then begin

      if LL2(x1,y1,x2,y2, x3,y3,x4,y4, 1,0, c) then

        begin _x:=c.x; _y:=c.y end

      else begin
        _x:=x2/2 + x3/2;
        _y:=y2/2 + y3/2;
      end

    end
    else begin _x:=x2; _y:=y2 end

  else
  if Stretch_line(bx,by,cx,cy, r, x3,y3,x4,y4) then

    begin _x:=x3; _y:=y3 end
end;

procedure Get_tube(ax,ay,bx,by,cx,cy, r1,r2: Double;
                   out lx,ly,rx,ry: Double);
begin
  Get_tube_side(ax,ay,bx,by,cx,cy, r1, lx,ly);
  Get_tube_side(ax,ay,bx,by,cx,cy, -r2, rx,ry)
end;

procedure Get_Tube_Points(a,b,c: TPoint; r1,r2: Double;
                          out b1,b2: TPoint);
var
  lx,ly,rx,ry: Double;
begin
  Get_tube(a.x,a.y,b.x,b.y,c.x,c.y, r1,r2, lx,ly,rx,ry);
  b1.x:=Round(lx); b1.y:=Round(ly);
  b2.x:=Round(rx); b2.y:=Round(ry);
end;

function Stretch_dir(const a,b: TPoint; r: double): TGauss;
var
  nx,ny,len: double; d: TGauss;
begin
  d.x:=0; d.y:=0;

  nx:=a.y-b.y; ny:=b.x-a.x;
  len:=Hypot(nx,ny);

  if len > 0.001 then begin
    d.x:=nx * r/len;
    d.y:=ny * r/len
  end;

  Result:=d
end;

function Stretch_Point(const a,b: TPoint; r1,r2: double;
                        out p1,p2: TPoint): Boolean;
var
  nx,ny,len: double;
begin
  Result:=false; p1:=a; p2:=a;

  nx:=a.y-b.y; ny:=b.x-a.x;
  len:=Hypot(nx,ny); p1:=a; p2:=a;

  if len > 0.001 then begin
    nx:=nx / len; ny:=ny / len;

    p1.x:=a.x + Round(nx*r1);
    p1.y:=a.y + Round(ny*r1);

    p2.x:=a.x - Round(nx*r2);
    p2.y:=a.y - Round(ny*r2);

    Result:=true
  end
end;

function Stretch_Gauss(const a,b: TGauss; r1,r2: double;
                        out p1,p2: TGauss): Boolean;
var
  nx,ny,len: double;
begin
  Result:=false; p1:=a; p2:=a;

  nx:=a.y-b.y; ny:=b.x-a.x;
  len:=Hypot(nx,ny); p1:=a; p2:=a;

  if len > Small then begin
    nx:=nx / len; ny:=ny / len;

    p1.x:=a.x + nx*r1;
    p1.y:=a.y + ny*r1;

    p2.x:=a.x - nx*r2;
    p2.y:=a.y - ny*r2;

    Result:=true
  end
end;

procedure Get_Two_P_Points(const a,b: TPoint; r: double; out p1,p2: TPoint);
var
  dx,dy,nx,ny,len: double;
begin
  p1:=a; p2:=b;

  nx:=a.y-b.y; ny:=b.x-a.x;
  len:=Hypot(nx,ny);

  if len > 0.001 then begin
    dx:=nx*r/len; dy:=ny*r/len;

    p1.x:=a.x + Round(dx);
    p1.y:=a.y + Round(dy);

    p2.x:=b.x + Round(dx);
    p2.y:=b.y + Round(dy);
  end
end;

procedure Up_Point(const a,b: TPoint; r: double; out p: TPoint);
var
  nx,ny,len: double;
begin
  nx:=a.y-b.y; ny:=b.x-a.x;
  len:=Hypot(nx,ny); p:=a;

  if len > 0.001 then begin
    p.x:=a.x + Round(nx*r/len);
    p.y:=a.y + Round(ny*r/len)
  end
end;

function Next_Gauss(const a,b: tgauss; r: double): tgauss;
var
  dx,dy, len,k: double;
begin
  dx:=b.x-a.x; dy:=b.y-a.y; k:=0;
  len:=Hypot(dx,dy); if len > 0 then k:=r/len;
  Result.x:=a.x+dx*k; Result.y:=a.y+dy*k
end;

function Move_Gauss(const p,a,b: tgauss; r: double): tgauss;
var
  dx,dy, len,k: double;
begin
  dx:=b.x-a.x; dy:=b.y-a.y; k:=0;
  len:=Hypot(dx,dy); if len > 0 then k:=r/len;
  Result.x:=p.x+dx*k; Result.y:=p.y+dy*k
end;

function Get_Next_Point(const a,b: TPoint; r: double;
                        out p: TPoint): Boolean;
var
  x,y: int; dx,dy,len,k: double;
begin
  Result:=false;

  x:=a.X; dx:=b.X-x;
  y:=a.Y; dy:=b.Y-y;
  len:=Hypot(dx,dy);

  if len < 0.01 then begin
    p.X:=x+Round(r); p.Y:=y
  end
  else begin k:=r/len;
    p.X:=x + Round(dx*k);
    p.Y:=y + Round(dy*k);
    Result:=true
  end
end;

function Next_LPoint(const p, a,b: TPoint; k: double): TPoint;
var
  dx,dy: int;
begin
  dx:=b.X-a.X; dy:=b.Y-a.Y;
  Result.X:=p.X + Round(dx*k);
  Result.Y:=p.Y + Round(dy*k);
end;

procedure Move_Vector(var v: LVector; r: double);
var
  a,b: TPoint; dx,dy: Longint;
  k1,k2,l: Double;
begin
  if r > 0 then begin
    a:=v[0]; b:=v[1];

    dx:=b.X-a.X; dy:=b.Y-a.Y;
    l:=Hypot(dx,dy);

    if (dx = 0) and (dy = 0) then
    begin dx:=1; l:=1 end;

    k1:=r/l; k2:=(l+r)/l;
    v[0].X:=a.X + Round(k1*dx);
    v[0].Y:=a.Y + Round(k1*dy);
    v[1].X:=a.X + Round(k2*dx);
    v[1].Y:=a.Y + Round(k2*dy);
  end
end;

procedure Step_Next_Point(const a,b: TPoint; out p: TPoint);
var
  dx,dy,ed: longint;
begin
  dx:=b.x-a.x; dy:=b.y-a.y;

  if Abs(dx) >= Abs(dy) then begin
    if dx <> 0 then begin
      ed:=1; if dx < 0 then ed:=-1;
      p.x:=a.x+ed; p.y:=a.y+Round(dy/dx)*ed
    end else p:=a
  end
  else begin
    ed:=1; if dy < 0 then ed:=-1;
    p.x:=a.x+Round(dx/dy)*ed; p.y:=a.y+ed;
  end
end;

procedure Get_Norm_Point(const a,b: TPoint; d,h: double;
                         var p: TPoint);
var
  x,y,k,dx,dy,nx,ny,len: double;
begin
  dx:=b.x-a.x; dy:=b.y-a.y;
  len:=Hypot(dx,dy); x:=a.x; y:=a.y;

  if len > 0.001 then begin
    k:=d/len; x:=a.x+dx*k; y:=a.y+dy*k;

    nx:=-dy/len; ny:=dx/len;

    x:=x + Round(nx*h);
    y:=y + Round(ny*h)
  end;

  p.x:=Round(x);
  p.y:=Round(y)
end;

procedure Get_Thick_Points(const p,q: TPoint; a,b: double;
                           var p1,p2: TPoint);
var
  f: double;
begin
  f:=iArcTan(p.x,p.y,q.x,q.y);

  Get_F_Point(p.x,p.y,a,a,f+Pi_2,p1);
  Get_F_Point(p.x,p.y,b,b,f-Pi_2,p2)
end;

function Middle_Point(const a,b: TPoint; out p: TPoint): Boolean;
begin
  Result:=true;
  p.X:=Round(a.X/2 + b.X/2);
  p.Y:=Round(a.Y/2 + b.Y/2);
  if Points_Equal(a,p)
  or Points_Equal(b,p) then
  Result:=true
end;

procedure Middle_Gauss(const a,b: tgauss; out p: tgauss);
begin
  p.x:=a.x/2 + b.x/2;
  p.y:=a.y/2 + b.y/2
end;

function xCentre(x1,x2: double): double;
begin
  Result:=x1/2 + x2/2
end;

function iCentre(x1,x2: longint): longint;
begin
  Result:=Round(x1/2 + x2/2)
end;

function iMult(a,b: longint): Double;
begin
  Result:=a; Result:=Result*b
end;

function xModule(x,y: double): double;
begin
  Result:=Sqr(x)+Sqr(y)
end;

function xNorm(var g: tgauss): Boolean;
var
  r: Extended;
begin
  r:=Hypot(g.x,g.y);

  if r > Small then begin
    g.x:=g.x/r; g.y:=g.y/r;
    Result:=true
  end
  else begin
    g.x:=1; g.y:=0;
    Result:=false
  end
end;

function d2_scalar(x1,x2,y1,y2: double): double;
begin
  Result:=x1*x2 + y1*y2
end;

function g2_scalar(const v1,v2: TGauss): double;
begin
  Result:=v1.x*v2.x + v1.y*v2.y
end;

function d2_vect(x1,x2,y1,y2: double): double;
begin
  Result:=x1*y2 - y1*x2
end;

function d2_angle(x1,x2,y1,y2: double): double;
var
  sp,vp: Double;
begin
  sp:=x1*x2 + y1*y2; vp:=x1*y2 - y1*x2;
  Result:=ArcTan2(vp,sp)
end;

function i2_scalar(x1,x2,y1,y2: longint): double;
begin
  Result:=d2_Scalar(x1,x2,y1,y2)
end;

function i2_vect(x1,x2,y1,y2: longint): double;
begin
  Result:=d2_vect(x1,y1,x2,y2)
end;

function iSquare(const a,b: TPoint): double;
begin
  Result:=d2_vect(a.X,a.Y,b.X,b.Y)/2
end;

function Int_cos(const a,b, c,d: TPoint;
                 minLen: double; out cos: double): bool;
var
  dx1,dy1,dx2,dy2,l1,l2: double;
begin
  Result:=false; cos:=0;

  dx1:=b.X-a.X; dy1:=b.Y-a.Y; l1:=Hypot(dx1,dy1);
  dx2:=d.X-c.X; dy2:=d.Y-c.Y; l2:=Hypot(dx2,dy2);

  if (l1 >= minLen) and (l2 >= minLen) then begin
    cos:=d2_scalar(dx1,dx2,dy1,dy2)/l1/l2;
    Result:=true
  end
end;

function Gauss_cos(const a,b, c,d: TGauss;
                   minLen: double; out cos: double): bool;
var
  dx1,dy1,dx2,dy2,l1,l2: double;
begin
  Result:=false; cos:=0;

  dx1:=b.x-a.x; dy1:=b.y-a.y; l1:=Hypot(dx1,dy1);
  dx2:=d.x-c.x; dy2:=d.y-c.y; l2:=Hypot(dx2,dy2);

  if (l1 >= minLen) and (l2 >= minLen) then begin
    cos:=d2_scalar(dx1,dx2,dy1,dy2)/l1/l2;
    Result:=true
  end
end;

function Gauss_cos3(const a,b, c: TGauss;
                   minLen: double; out cos: double): bool;
var
  dx1,dy1,dx2,dy2,l1,l2: double;
begin
  Result:=false; cos:=0;

  dx1:=b.x-a.x; dy1:=b.y-a.y; l1:=Hypot(dx1,dy1);
  dx2:=c.x-b.x; dy2:=c.y-b.y; l2:=Hypot(dx2,dy2);

  if (l1 >= minLen) and (l2 >= minLen) then begin
    cos:=d2_scalar(dx1,dx2,dy1,dy2)/l1/l2;
    Result:=true
  end
end;

function Int_cos2(const v1,v2: TPoint;
                  minLen: double; out cos: double): bool;
var
  dx1,dy1,dx2,dy2,l1,l2: double;
begin
  Result:=false; cos:=0;

  dx1:=v1.X; dy1:=v1.Y; l1:=Hypot(dx1,dy1);
  dx2:=v2.X; dy2:=v2.Y; l2:=Hypot(dx2,dy2);

  if (l1 >= minLen) and (l2 >= minLen) then begin
    cos:=d2_scalar(dx1,dx2,dy1,dy2)/l1/l2;
    Result:=true
  end
end;

function Int_cos3(const a,b,c: TPoint;
                  minLen: double; out cos: double): bool;
var
  dx1,dy1,dx2,dy2,l1,l2: double;
begin
  Result:=false; cos:=0;

  dx1:=b.X-a.X; dy1:=b.Y-a.Y; l1:=Hypot(dx1,dy1);
  dx2:=c.X-b.X; dy2:=c.Y-b.Y; l2:=Hypot(dx2,dy2);

  if (l1 >= minLen) and (l2 >= minLen) then begin
    cos:=d2_scalar(dx1,dx2,dy1,dy2)/l1/l2;
    Result:=true
  end
end;

function Long_Dist(const p1,p2: TPoint): double;
begin
  Result:=Hypot(p2.X-p1.X,p2.Y-p1.Y)
end;

function Int_Dist(const p1,p2: IPoint): double;
begin
  Result:=Hypot(p2.x-p1.x,p2.y-p1.y)
end;

function Points_Dist(const p1,p2: TPoint): double;
begin
  Result:=Hypot(p2.X-p1.X,p2.Y-p1.Y)
end;

function VPoints_Dist(const p1,p2: VPoint): double;
begin
  Result:=Hypot(p2.z-p1.z,Hypot(p2.x-p1.x,p2.y-p1.y))
end;

function Gauss_Dist(const g1,g2: tgauss): double;
begin
  Result:=Hypot(g2.x-g1.x,g2.y-g1.y)
end;

function Sizes_Equal(const s1,s2: TSize): Boolean;
begin
  Result:=(s1.cx=s2.cx) and (s1.cy=s2.cy)
end;

function Points_Equal(const p1,p2: TPoint): Boolean;
begin
  Result:=(p1.X=p2.X) and (p1.Y=p2.Y)
end;

function VPoints_Equal(const p1,p2: VPoint): Boolean;
begin
  Result:=(p1.x=p2.x) and (p1.y=p2.y) and (p1.z=p2.z)
end;

function Gauss_Equal(const p1,p2: tgauss; eps: Double): Boolean;
begin
  Result:=false;
  if Abs(p2.x-p1.x) < eps then
  if Abs(p2.y-p1.y) < eps then
  Result:=true
end;

function xy_equal(const a,b: tgauss; eps: Double): Boolean;
begin
  Result:=(Round(a.x*eps) = Round(b.x*eps)) and
          (Round(a.y*eps) = Round(b.y*eps))
end;

function Gauss_res(const g1,g2, p1,p2: TGauss): Double;
var
  dist: double;
begin
  Result:=1; dist:=Gauss_Dist(p1,p2);
  if dist >= 1 then Result:=Gauss_Dist(g1,g2)/dist
end;

function dir_Equal(const p1,p2, q1,q2: TPoint): Boolean;
var
  dx1,dy1,dx2,dy2: Integer;
begin
  dx1:=p2.X-p1.X; dy1:=p2.Y-p1.Y;
  dx2:=q2.X-q1.X; dy2:=q2.Y-q1.Y;

  if Abs(dx1) > Abs(dy1) then
    Result:=(dx1 > 0) = (dx2 > 0)
  else
    Result:=(dy1 > 0) = (dy2 > 0)
end;

procedure Swap_LPoints(var a,b: TPoint);
var
  t: TPoint;
begin
  t:=a; a:=b; b:=t
end;

procedure Swap_VPoints(var a,b: vpoint);
var
  t: vpoint;
begin
  t:=a; a:=b; b:=t
end;

procedure Swap_TGauss(var a,b: tgauss);
var
  t: tgauss;
begin
  t:=a; a:=b; b:=t
end;

procedure Swap_lPort(var lt,rb: TPoint);
begin
  if lt.X > rb.X then ISwap(lt.X,rb.X);
  if lt.Y > rb.Y then ISwap(lt.Y,rb.Y);
end;

procedure Swap_GPort(var lt,rb: tgauss);
var
  t: double;
begin
  if lt.x > rb.x then begin t:=lt.x; lt.x:=rb.x; rb.x:=t end;
  if lt.y > rb.y then begin t:=lt.y; lt.y:=rb.y; rb.y:=t end
end;

function Get_Normal(const p1,p2: TPoint): TPoint;
begin
  Result.X:=p1.Y-p2.Y;
  Result.Y:=p2.X-p1.X
end;

function Get_Direction(const p1,p2: TPoint): TPoint;
begin
  Result.X:=p2.X-p1.X;
  Result.Y:=p2.Y-p1.Y
end;

function z_axe_line(const p1,p2,p: TPoint;
                    z1,z2: Double): Double;
var
  k: Double;
begin
  if Points_Equal(p1,p2) then
    Result:=z2
  else begin
    k:=(z2-z1)/Long_Dist(p1,p2);
    Result:=z1 + Long_Dist(p1,p) * k;
  end
end;

function OnLine(ax,ay,bx,by, px,py: double): double;
var
  vx,vy: double;
begin
  vx:=bx-ax; vy:=by-ay;
  Result:=vx*(py-by) - vy*(px-bx);
  if Abs(Result) <= 1E-8 then Result:=0
end;

function gOnLine(const a,b, p: TGauss): double;
var
  ax,ay: double;
begin
  ax:=b.x-a.x; ay:=b.y-a.y;
  Result:=ax*(p.y-b.y) - ay*(p.x-b.x);
  if Abs(Result) <= 1E-8 then Result:=0
end;

function iOnLine(const a,b, p: TPoint): double;
var
  ax,ay: double;
begin
  ax:=b.X-a.X; ay:=b.Y-a.Y;
  Result:=ax*(p.Y-b.Y) - ay*(p.X-b.X);
  if Abs(Result) <= Small then Result:=0
end;

function iOnLine1(const a,b, p: TPoint): Integer;
var
  ax,ay,dx: double;
begin
  ax:=b.X-a.X; ay:=b.Y-a.Y;
  dx:=ax*(p.Y-b.Y) - ay*(p.X-b.X);
  if Abs(dx) <= Small then
    Result:=0
  else
  if dx < 0 then Result:=-1
            else Result:=+1
end;

function InTriangle(lp: PGPoly; x,y: Double;
                    out dist: double): Integer;
var
  cx,cy, x1,y1,x2,y2, r1,r2,r3: double;
begin
  Result:=0;

  cx:=lp[0].x/3 + lp[1].x/3 + lp[2].x/3;
  cy:=lp[0].y/3 + lp[1].y/3 + lp[2].y/3;
  dist:=Hypot(x - cx,y - cy);

  x1:=lp[0].x; y1:=lp[0].y; x2:=lp[1].x; y2:=lp[1].y;
  r1:=(y2-y1)*x + (x1-x2)*y + (y1*x2-x1*y2);

  if Abs(r1) < Small then begin
    if (x >= Min(x1,x2)) and (x <= Max(x1,x2)) then
    if (y >= Min(y1,y2)) and (y <= Max(y1,y2)) then
    Result:=1
  end
  else begin
    x1:=lp[1].x; y1:=lp[1].y; x2:=lp[2].x; y2:=lp[2].y;
    r2:=(y2-y1)*x + (x1-x2)*y + (y1*x2-x1*y2);

    if Abs(r2) < Small then begin
      if (x >= Min(x1,x2)) and (x <= Max(x1,x2)) then
      if (y >= Min(y1,y2)) and (y <= Max(y1,y2)) then
      Result:=2
    end
    else begin
      x1:=lp[2].x; y1:=lp[2].y; x2:=lp[0].x; y2:=lp[0].y;
      r3:=(y2-y1)*x + (x1-x2)*y + (y1*x2-x1*y2);

      if Abs(r3) < Small then begin
        if (x >= Min(x1,x2)) and (x <= Max(x1,x2)) then
        if (y >= Min(y1,y2)) and (y <= Max(y1,y2)) then
        Result:=3
      end else

      if r1 < 0 then begin
        if (r2 < 0) and (r3 < 0) then Result:=4
      end
      else begin
        if (r2 > 0) and (r3 > 0) then Result:=4
      end

    end
  end
end;

function TriangleCentre(const a,b,c: TPoint): TPoint;
begin
  Result.x:=Round(a.x/3+b.x/3+c.x/3);
  Result.y:=Round(a.y/3+b.y/3+c.y/3);
end;

function Triangle_Verify(lp: PLPoly; eps: Float): Boolean;
var
  d1,d2,d3, k: Double;
begin
  Result:=false;

  d1:=Long_Dist(lp[0],lp[1]);
  d2:=Long_Dist(lp[1],lp[2]);
  d3:=Long_Dist(lp[2],lp[0]);

  if d3 < d2 then xSwap(d2,d3);
  if d2 < d1 then xSwap(d1,d2);
  if d3 < d2 then xSwap(d2,d3);

  if d3 > 1 then begin
    k:=Abs((d1+d2-d3)/d3);
    Result:=k >= eps
  end
end;

function xTriangle_Verify(lp: PGPoly; eps: Float): Boolean;
var
  d1,d2,d3, k: Double;
begin
  Result:=false;

  d1:=Gauss_Dist(lp[0],lp[1]);
  d2:=Gauss_Dist(lp[1],lp[2]);
  d3:=Gauss_Dist(lp[2],lp[0]);

  if d3 < d2 then xSwap(d2,d3);
  if d2 < d1 then xSwap(d1,d2);
  if d3 < d2 then xSwap(d2,d3);

  if d3 > 1 then begin
    k:=Abs((d1+d2-d3)/d3);
    Result:=k >= eps
  end
end;

function Ribs_Equal(const p1,p2, q1,q2: TPoint): Boolean;
begin
  Result:=false;

  if CompareMem(@p1,@q1,Sizeof(TPoint)) then
    Result:=CompareMem(@p2,@q2,Sizeof(TPoint))
  else
  if CompareMem(@p1,@q2,Sizeof(TPoint)) then
    Result:=CompareMem(@p2,@q1,Sizeof(TPoint))
end;

function xLL2(a1,b1,c1, a2,b2,c2: double; out p: tgauss): Boolean;
var
  d: Double;
begin
  Result:=false; p.x:=0; p.y:=0;

  d:=b1*a2 - b2*a1;
  if d > Small then begin
    p.x:=(c1*b2 - c2*b1)/d;
    p.y:=(c2*a1 - c1*a2)/d;
    Result:=true
  end
end;

function x_LL2(const p1: tgauss; k1: Double;
               const p2: tgauss; k2: Double;
               out p: tgauss): Boolean;
begin
  Result:=xLL2(k1,-1,p1.y-k1*p1.x, k2,-1,p2.y-k2*p2.x, p)
end;

function LL2(x1,y1,x2,y2, x3,y3,x4,y4, k1,k2: double; out c: tgauss): Boolean;
var
  d1,d2: tgauss; rmu: double;
begin
  Result:=false; c.x:=x1; c.y:=y1;

  d1.x:=x2-x1; d1.y:=y2-y1;
  d2.x:=x4-x3; d2.y:=y4-y3;

  rmu:=d1.x*d2.y-d1.y*d2.x;

  if Abs(rmu) > 0.001 then begin
    rmu:=((x4-x1)*d2.y-(y4-y1)*d2.x)/rmu;

    if (k1 > k2) or ((rmu >= k1) and (rmu <= k2)) then begin
      c.x:=x1+rmu*d1.x; c.y:=y1+rmu*d1.y; Result:=true
    end
  end
end;

function vLL2(const p1,v1, p2,v2: TGauss; out c: tgauss): Boolean;
var
  d1,d2: tgauss; x1,y1,rmu: double;
begin
  Result:=false;

  x1:=p1.x; y1:=p1.y;

  d1:=v1; d2:=v2;
  rmu:=d1.x*d2.y-d1.y*d2.x;

  if Abs(rmu) > 0.001 then begin
    rmu:=((p2.x-x1)*d2.y-(p2.y-y1)*d2.x)/rmu;
    x1:=x1+rmu*d1.x; y1:=y1+rmu*d1.y;
    Result:=true
  end;

  c.x:=x1; c.y:=y1
end;
  
function gLL2(const p1,p2, p3,p4: TGauss;
              k1,k2: Double; out c: tgauss): Boolean;
begin
  Result:=LL2(p1.x,p1.y,p2.x,p2.y, p3.x,p3.y,p4.x,p4.y, k1,k2, c)
end;

function LL2gk(const p1,p2, p3,p4: TGauss;
               out c: TGauss; out k: double): Boolean;
var
  d1,d2: tgauss; rmu: double;
begin
  Result:=false;

  d1.x:=p2.x-p1.x; d1.y:=p2.y-p1.y;
  d2.x:=p4.x-p3.x; d2.y:=p4.y-p3.y;

  rmu:=d1.x*d2.y-d1.y*d2.x;

  if Abs(rmu) > 0.001 then begin
    rmu:=((p4.x-p1.x)*d2.y - (p4.y-p1.y)*d2.x)/rmu;

    c.x:=p1.x + rmu*d1.x;
    c.y:=p1.y + rmu*d1.y;
    k:=rmu; Result:=true
  end
end;

function LL2ik(const p1,p2, p3,p4: TPoint;
               out c: TGauss; out k: double): Boolean;
var
  d1,d2: tgauss; rmu: double;
begin
  Result:=false;

  d1.x:=p2.X-p1.X; d1.y:=p2.Y-p1.Y;
  d2.x:=p4.X-p3.X; d2.y:=p4.Y-p3.Y;

  rmu:=d1.x*d2.y-d1.y*d2.x;

  if Abs(rmu) > 0.001 then begin
    rmu:=((p4.X-p1.X)*d2.y - (p4.Y-p1.Y)*d2.x)/rmu;

    c.x:=p1.X + rmu*d1.x;
    c.y:=p1.Y + rmu*d1.y;
    k:=rmu; Result:=true
  end
end;

function LL2i(const p1,p2, p3,p4: TPoint;
              k1,k2: double; out c: TGauss): Boolean;
var
  d1,d2: tgauss; rmu: double;
begin
  Result:=false; c.x:=p1.X; c.y:=p1.Y;

  d1.x:=p2.X-p1.X; d1.y:=p2.Y-p1.Y;
  d2.x:=p4.X-p3.X; d2.y:=p4.Y-p3.Y;

  rmu:=d1.x*d2.y-d1.y*d2.x;

  if Abs(rmu) > 0.001 then begin
    rmu:=((p4.X-p1.X)*d2.y - (p4.Y-p1.Y)*d2.x)/rmu;

    if (k1 > k2) or ((rmu >= k1) and (rmu <= k2)) then begin
      c.x:=p1.X + rmu*d1.x;
      c.y:=p1.Y + rmu*d1.y;
      Result:=true
    end
  end
end;

function LL2ip(const p1,p2, p3,p4: TPoint;
               k1,k2: double; out c: TPoint): Boolean;
var
  t: TGauss;
begin
  Result:=LL2i(p1,p2, p3,p4, k1,k2, t);
  if Result then begin
    c.X:=Round(t.x);
    c.Y:=Round(t.y);
  end
end;

function LL2icg(const a1,a2, b1,b2: TPoint; out c: TGauss): Boolean;
var
  d1,d2: tgauss; k1,k2: double;
begin
  Result:=false;

  d1.x:=a2.X-a1.X; d1.y:=a2.Y-a1.Y;
  d2.x:=b2.X-b1.X; d2.y:=b2.Y-b1.Y;

  k1:=d1.x*d2.y-d1.y*d2.x;
  k2:=d2.x*d1.y-d2.y*d1.x;

  if Abs(k1) > 0.001 then
  if Abs(k2) > 0.001 then begin

    k1:=((b2.X-a1.X)*d2.y - (b2.Y-a1.Y)*d2.x)/k1;
    k2:=((a2.X-b1.X)*d1.y - (a2.Y-b1.Y)*d1.x)/k2;

    if (k1 >= 0) and (k1 <= 1) then
    if (k2 >= 0) and (k2 <= 1) then begin
      c.X:=a1.X + k1*d1.x;
      c.Y:=a1.Y + k1*d1.y;
      Result:=true
    end
  end
end;

function LL2icg1(const a1,a2, b1,b2: TPoint; out c: TGauss): Boolean;
const
  eps = 1E-3;
var
  d1,d2: tgauss; k1,k2: double;
begin
  Result:=false;

  d1.x:=a2.X-a1.X; d1.y:=a2.Y-a1.Y;
  d2.x:=b2.X-b1.X; d2.y:=b2.Y-b1.Y;

  k1:=d1.x*d2.y-d1.y*d2.x;
  k2:=d2.x*d1.y-d2.y*d1.x;

  if Abs(k1) > 0.001 then
  if Abs(k2) > 0.001 then begin

    k1:=((b2.X-a1.X)*d2.y - (b2.Y-a1.Y)*d2.x)/k1;
    k2:=((a2.X-b1.X)*d1.y - (a2.Y-b1.Y)*d1.x)/k2;

    if (k1 >= 0) and (k1 <= 1) then
    if (k2 >= -eps) and (k2 <= 1+eps) then begin
      c.X:=a1.X + k1*d1.x;
      c.Y:=a1.Y + k1*d1.y;
      Result:=true
    end
  end
end;

function LL2ic(const a1,a2, b1,b2: TPoint; out c: TPoint): Boolean;
var
  t: TGauss;
begin
  Result:=LL2icg(a1,a2, b1,b2, t);
  if Result then begin
    c.X:=Round(t.x);
    c.Y:=Round(t.y);
  end
end;

function LL2ic1(const a1,a2, b1,b2: TPoint; out c: TPoint): Boolean;
var
  t: TGauss;
begin
  Result:=LL2icg1(a1,a2, b1,b2, t);
  if Result then begin
    c.X:=Round(t.x);
    c.Y:=Round(t.y);
  end
end;

function ILL2(const p1,p2, p3,p4: TPoint; out c: TPoint): Boolean;
var
  g: tgauss;
begin
  Result:=false;

  if not Points_Equal(p1,p2) then
  if LL2(p1.X,p1.Y, p2.X,p2.Y, p3.X,p3.Y, p4.X,p4.Y,1,0,g) then

  if Abs(g.x) < MaxLongint then
  if Abs(g.y) < MaxLongint then begin
    c.x:=Round(g.x); c.y:=Round(g.y);
    Result:=true
  end
end;

function ILLD(const p1,p2, p3,p4: TPoint; out c: TPoint): Boolean;
var
  d1,d2,pc: TPoint;
begin
  Result:=false;

  if (Long_Dist(p1,p2) > 0) and (Long_Dist(p3,p4) > 0) then begin
    d1:=Get_Normal(p1,p2); Inc(d1.x,p1.x); Inc(d1.y,p1.y);
    d2:=Get_Normal(p3,p4); Inc(d2.x,p4.x); Inc(d2.y,p4.y);

    if ILL2(p1,d1, d2,p4,pc) then begin
      if (Long_Dist(p1,pc)+Long_Dist(p4,pc))/2 < Long_Dist(p1,p4) then
      begin c:=pc; Result:=true end
    end
  end
end;

function ILLC(p1,p2,p3: TPoint; var pp: TPoint): Boolean;
var
  p4,p5,pc: TPoint;
begin
  Result:=false;

  if (Long_Dist(p1,p2) > 0) and
     (Long_Dist(p1,p3) > 0) and
     (Long_Dist(p2,p3) > 0) then begin
    p4:=Get_Normal(p1,p2); Inc(p4.x,p2.x); Inc(p4.y,p2.y);
    p5.x:=p3.x+p2.x-p1.x; p5.y:=p3.y+p2.y-p1.y;

    if ILL2(p2,p4, p3,p5, pc) and
    (Long_Dist(pc,p3) < Long_Dist(p2,p3)) then
    begin Result:=true; pp:=pc end
  end
end;

function i_Cross(const p1,p2, q1,q2: TPoint; out c: TPoint): Boolean;
var
  q_lt,q_rb: TPoint; t: tgauss;
begin
  Result:=false; Swap_lRect(q1,q2, q_lt,q_rb);

  if PortContainsLine(q_lt,q_rb, p1,p2) then
  if LL2(p1.X,p1.Y,p2.X,p2.Y, q1.X,q1.Y,q2.X,q2.Y, 0,1, t) then
  begin
    if q_lt.X = q_rb.X then t.x:=q_lt.X;
    if q_lt.Y = q_rb.Y then t.y:=q_lt.Y;

    if (t.x >= q_lt.X) and (t.x <= q_rb.X) then
    if (t.y >= q_lt.Y) and (t.y <= q_rb.Y) then
    begin
      c.X:=Round(t.x); c.Y:=Round(t.y); Result:=true
    end
  end
end;

function g_Cross(const p1,p2, q1,q2: tgauss; out c: tgauss): Boolean;
var
  p: tgauss;
begin
  Result:=false; c:=p1;

  if LL2(p1.x,p1.y,p2.x,p2.y, q1.x,q1.y,q2.x,q2.y, 0,1, p) then

  if p.x >= Min(q1.x,q2.x) then
  if p.x <= Max(q1.x,q2.x) then
  if p.y >= Min(q1.y,q2.y) then
  if p.y <= Max(q1.y,q2.y) then begin
    Result:=true; c:=p
  end
end;

function Delta(a1,b1, a2,b2: Double): Double;
begin
  Result:=a1*b2 - a2*b1
end;

function abc_xy(a1,b1,c1, a2,b2,c2: Double;
                out x,y: Double): Boolean;
var
  d: Double;
begin
  Result:=false;

  d:=Delta(a1,b1, a2,b2);
  if d > 0.001 then begin
    x:=Delta(c1,b1, c2,b2)/d;
    y:=Delta(a1,c1, a2,c2)/d;
    Result:=true
  end
end;

function LL_FF(x1,y1, x2,y2, f1,f2: Double;
               out cx,cy: Double): Boolean;
var
  c1,s1,c2,s2: Extended;
begin
  cx:=x1; cy:=y1;

  SinCos(f1, s1,c1); SinCos(f2, s2,c2);

  Result:=abc_xy(s1,-c1,x1*s1 - y1*c1,
                 s2,-c2,x2*s2 - y2*c2, cx,cy)
end;

function GLL_FF(g1,g2: tgauss; f1,f2: Double;
                out g: tgauss): Boolean;
begin
  Result:=LL_FF(g1.x,g1.y, g2.x,g2.y, f1,f2, g.x,g.y)
end;

function LL_RR(x1,y1, x2,y2, r1,r2: Double;
               out cx1,cy1,cx2,cy2: Double): Boolean;
var
  dx,dy, d1,d2,k, cx,cy,nx,ny, a,b: Double;
begin
  Result:=false;
  cx1:=x1; cy1:=y1; cx2:=x2; cy2:=y2;

  dx:=x2-x1; dy:=y2-y1;
  d1:=Hypot(dx,dy); d2:=r1+r2;

  if (d1 > 0) and (d2 > d1) then

  if (r1+d1) > r2 then
  if (r2+d1) > r1 then begin

    a:=(d1 - (r2-r1)*d2/d1) / 2;

    k:=a/d1;

    cx:=x1 + k*dx;
    cy:=y1 + k*dy;

    b:=Sqrt(r1*r1 - a*a);

    nx:=-dy/d1; ny:=dx/d1;
    cx1:=cx + b * nx;
    cy1:=cy + b * ny;
    cx2:=cx - b * nx;
    cy2:=cy - b * ny;

    Result:=true
  end
end;

function GLL_RR(const c1,c2: tgauss; r1,r2: Double;
                out p1,p2: tgauss): Boolean;
begin
  Result:=LL_RR(c1.x,c1.y,c2.x,c2.y, r1,r2,
                p1.x,p1.y,p2.x,p2.y)
end;

function ILL_RR(const c1,c2: TPoint; r1,r2: Double;
                out p1,p2: TPoint): Boolean;
var
  x1,y1,x2,y2: Double;
begin
  Result:=LL_RR(c1.x,c1.y,c2.x,c2.y, r1,r2, x1,y1,x2,y2);
  p1.x:=Round(x1); p1.y:=Round(y1);
  p2.x:=Round(x2); p2.y:=Round(y2);
end;

function LR_Sector(lp: PGPoly; r: Double;
                   out fi: Double): Integer;
var
  c,v: TGauss; a,b: Double; g: GOrient;
begin
  Result:=0; fi:=0;

  v.x:=lp[1].X-lp[0].X;
  v.y:=lp[1].Y-lp[0].Y;

  c.x:=lp[0].X + v.x/2;
  c.y:=lp[0].Y + v.y/2;

  a:=Hypot(v.x,v.y) / 2;

  if xNorm(v) then
  if r > a then begin

    b:=Sqrt(r*r - a*a);
    if b > Small then begin
      lp[2].X:=c.x - v.y*b;
      lp[2].Y:=c.y + v.x*b;

      g[0]:=lp[2]; g[1]:=lp[0]; g[2]:=lp[1];
      fi:=xy_Angle(@g); Result:=2
    end
  end
end;

function LF_Sector(lp: PGPoly; fi: Double;
                   out r: Double): Integer;
var
  c,v: TGauss; a,b,s: Double;
begin
  Result:=0; r:=0;

  v.x:=lp[1].X-lp[0].X;
  v.y:=lp[1].Y-lp[0].Y;

  c.x:=lp[0].X + v.x/2;
  c.y:=lp[0].Y + v.y/2;

  a:=Hypot(v.x,v.y) / 2;

  s:=Abs(Sin(fi/2));

  if s > Small then
  if xNorm(v) then begin

    r:=a/s;
    if r > a then begin

      b:=Sqrt(r*r - a*a);
      if b > Small then begin
        lp[2].X:=c.x - v.y*b;
        lp[2].Y:=c.y + v.x*b;
        Result:=2
      end
    end
  end
end;

function CutLine(qx,qy,px,py, cx: Double): Double;
var
  dx,dy: Double;
begin
  Result:=py;

  dx:=px-qy; dy:=py-qy;

  if Abs(dx) > Small then
  Result:=qy + (cx-qx) * dy / dx
end;

function xDir_to_Line(x,y, x1,y1,x2,y2: Double): Double;
begin
  Result:=(y2-y1)*x + (x1-x2)*y + (y1*x2 - x1*y2)
end;

function Dir_to_Line(x,y: Double; const p1,p2: tgauss): Double;
begin
  Result:=xDir_to_Line(x,y, p1.x,p1.y,p2.x,p2.y)
end;

function iDir_to_Line(const p, q1,q2: TPoint): Double;
begin
  Result:=xDir_to_Line(p.x,p.y, q1.x,q1.y,q2.x,q2.y)
end;

function Get_line_coeff(x1,y1,x2,y2: Double;
                        out line: TLine_coeff): Double;
begin
  with line do begin x0:=x1; y0:=y1;
    a:=y2-y1; b:=x1-x2; c:=y1*x2 - x1*y2;
    ab:=a*a + b*b; ca:=c*a; cb:=c*b;
    len:=Hypot(a,b); Result:=len
  end
end;

function Get_line_coeffp(const a,b: TPoint;
                         out line: TLine_coeff): Double;
begin
  Result:=Get_line_coeff(a.X,a.Y,b.X,b.Y, line)
end;

function Get_line_coeffg(const a,b: tgauss;
                         out line: TLine_coeff): Double;
begin
  Result:=Get_line_coeff(a.x,a.y,b.x,b.y, line)
end;

function Get_line_tk(const l: TLine_coeff;
                     x,y: Double; out tk: double): Boolean;
var
  c2: double;
begin
  Result:=false; tk:=0;

  with l do
  if ab > Small then begin

    c2:=b*x - a*y;

    if Abs(b) > Abs(a) then
      tk:=(((c2*b-c*a)/ab) - x0) / (-b)
    else
      tk:=((-(c2*a+c*b)/ab) - y0) / a;

    Result:=true
  end
end;

function c_prj_to_line(const l: TLine_coeff;
                       x,y: Double; out p: TGauss): Boolean;
var
  c2: Double; t: TGauss;
begin
  Result:=false; p.x:=x; p.y:=y;

  with l do
  if ab > Small then begin

    c2:=b*x - a*y;

    if Abs(b) > Abs(a) then begin
      t.x:=(c2*b-c*a)/ab; t.y:=-(a*t.x+c)/b
    end
    else begin
      t.y:=-(c2*a+c*b)/ab; t.x:=-(b*t.y+c)/a
    end;

    p:=t; Result:=true
  end
end;

function c_prj_to_line1(const l: TLine_coeff;
                        x,y: Double; out p: TGauss): int;
var
  x1,y1,x2,y2,cos,l1,l2: Double; t: TGauss;
begin
  Result:=0;

  if c_prj_to_line(l, x,y, t) then begin

    x1:=l.x0; x2:=x1-l.b;
    y1:=l.y0; y2:=y1+l.a;

    x1:=x1-t.x; y1:=y1-t.y;
    x2:=x2-t.x; y2:=y2-t.y;

    cos:=x1*x2 + y1*y2;

    if cos > 0 then begin
      l1:=x1*x1 + y1*y1;
      l2:=x2*x2 + y2*y2;

      if l1 > l2 then Result:=+1 else
      if l1 < l2 then Result:=-1
    end
  end;

  p:=t
end;

function c_dist_to_line(const l: TLine_coeff; x,y: Double): Double;
begin
  Result:=0;
  with l do if len > 0.001 then
  Result:=(a*x + b*y + c) / len
end;

function p_dist_to_line(const l: TLine_coeff;
                        x,y: Double; out dist: Double): bool;
var
  c2,tk: double;
begin
  Result:=false; dist:=0;

  with l do
  if ab > Small then begin

    c2:=b*x - a*y;

    if Abs(b) > Abs(a) then
      tk:=(((c2*b-c*a)/ab) - x0) / (-b)
    else
      tk:=((-(c2*a+c*b)/ab) - y0) / a;

    dist:=(a*x + b*y + c) / len;

    if (tk >= 0) and (tk <= 1) then
    Result:=true
  end
end;

function c_plot_to_line(const l: TLine_coeff;
                        x,y,eps: Double; dist: PDouble;
                        out p: TGauss): Boolean;
var
  c2,tk,dr: double;
begin
  Result:=false; p.x:=x; p.y:=y;
  if Assigned(dist) then dist^:=0;

  with l do
  if ab > Small then begin

    c2:=b*x - a*y;

    if Abs(b) > Abs(a) then
      tk:=(((c2*b-c*a)/ab) - x0) / (-b)
    else
      tk:=((-(c2*a+c*b)/ab) - y0) / a;

    dr:=(a*x + b*y + c) / len;

    if (tk >= 0) and (tk <= 1) then
    if (Eps < 0) or (Abs(dr) <= Eps) then begin
      p.x:=x0 + tk*(-b); p.y:=y0 + tk*a;
      if Assigned(dist) then dist^:=dr;
      Result:=true
    end
  end
end;

function c_plot_to_line1(const l: TLine_coeff;
                         x,y,eps: Double; dist: PDouble;
                         out p: TGauss): Boolean;
var
  c2,tk,dr: double;
begin
  Result:=false; p.x:=x; p.y:=y;
  if Assigned(dist) then dist^:=0;

  with l do
  if ab > Small then begin

    c2:=b*x - a*y;

    if Abs(b) > Abs(a) then
      tk:=(((c2*b-c*a)/ab) - x0) / (-b)
    else
      tk:=((-(c2*a+c*b)/ab) - y0) / a;

    dr:=(a*x + b*y + c) / len;

    if (tk < 0) and (tk > -Eps/len) then
      tk:=0
    else
    if (tk > 1) and (tk < 1+Eps/len) then
      tk:=1;

    if (tk >= 0) and (tk <= 1) then
    if (Eps < 0) or (Abs(dr) <= Eps) then begin
      p.x:=x0 + tk*(-b); p.y:=y0 + tk*a;
      if Assigned(dist) then dist^:=dr;
      Result:=true
    end
  end
end;

function c_pull_to_line(const l: TLine_coeff;
                        var p: TGauss; eps: Double): Boolean;
var
  x,y,c2,tk,dr: double;
begin
  Result:=false;

  with l do
  if ab > Small then begin

    x:=p.x; y:=p.y; c2:=b*x - a*y;

    if Abs(b) > Abs(a) then
      tk:=(((c2*b-c*a)/ab) - x0) / (-b)
    else
      tk:=((-(c2*a+c*b)/ab) - y0) / a;

    dr:=(a*x + b*y + c) / len;
    if Abs(dr) <= eps then begin

      eps:=eps/len;

      if Abs(tk) < eps then tk:=0 else
      if Abs(1-tk) < eps then tk:=1;

      if (tk >= 0) and (tk <= 1) then begin
        p.x:=x0 + tk*(-b); p.y:=y0 + tk*a;
        Result:=true
      end
    end
  end
end;

function c_line_get_x(const l: TLine_coeff;
                      y: Double; out x: Double): bool;
begin
  Result:=false; x:=0; with l do
  if Abs(a) > Small then begin
    x:=-(b*y+c)/a; Result:=true
  end
end;

function xDist_to_Line(x,y, x1,y1,x2,y2: Double): double;
var
  a,b,c, len: double;
begin
  a:=y2-y1; b:=x1-x2; c:=y1*x2 - x1*y2;

  len:=Hypot(a,b); if len < 0.1 then
    Result:=Hypot(x-x1,y-y1)
  else
    Result:=(a*x + b*y + c) / len
end;

function gDist_to_Line(const p, q1,q2: TGauss): double;
var
  a,b,c, len: double;
begin
  a:=q2.y-q1.y; b:=q1.x-q2.x;
  c:=q1.y*q2.x - q1.x*q2.y;

  len:=Hypot(a,b); if len < 0.1 then
    Result:=Hypot(p.x-q1.x,p.y-q1.y)
  else
    Result:=(a*p.x + b*p.y + c) / len
end;

function Dist_to_Line(const p ,q1,q2: TPoint): double;
var
  a,b,c, tx,ty,len: Double;
begin
  a:=q2.y-q1.y; b:=q1.x-q2.x;
  tx:=q1.x; ty:=q1.y; c:=ty*q2.x - tx*q2.y;

  len:=Hypot(a,b); if len < 0.1 then
    Result:=Hypot(p.x-q1.x,p.y-q1.y)
  else
    Result:=(a*p.x + b*p.y + c) / len;
end;

function xLine_Contains_pt(x1,y1,x2,y2, x,y, eps: Double): Boolean;
var
  _x1,_y1,_x2,_y2: Double;
begin
  Result:=false;

  _x1:=Min(x1,x2); _y1:=Min(y1,y2);
  _x2:=Max(x1,x2); _y2:=Max(y1,y2);

  if (x >= _x1-eps) and (x <= _x2+eps) then
  if (y >= _y1-eps) and (y <= _y2+eps) then

  Result:=xDist_to_Line(x,y, x1,y1,x2,y2) <= eps
end;

function x_prj_to_Line(const p, q1,q2: TGauss;
                       out pc: TGauss): Boolean;
var
  a,b,ab,c1,c2: double; c: tgauss;
begin
  Result:=false; pc:=q1;

  a:=q2.y-q1.y; b:=q1.x-q2.x; ab:=a*a+b*b;
  c1:=-a*q1.x-b*q1.y; c2:=b*p.x-a*p.y;

  if ab > Small then begin

    if q1.x <> q2.x then begin
      c.x:=(c2*b-c1*a)/ab; c.y:=-(a*c.x+c1)/b
    end
    else begin
      c.y:=-(c2*a+c1*b)/ab; c.x:=-(b*c.y+c1)/a
    end;

    pc:=c; Result:=true
  end
end;

function prj_to_Line(const p, q1,q2: TPoint; out pc: TGauss): Boolean;
var
  x1,y1,x2,y2, a,b,ab,c1,c2: double; c: TGauss;
begin
  Result:=false; pc.x:=p.x; pc.y:=p.y;

  x1:=q1.X; y1:=q1.Y; x2:=q2.X; y2:=q2.Y;

  a:=y2-y1; b:=x1-x2; ab:=a*a+b*b;

  if Abs(ab) < 0.1 then begin
    pc.x:=x1; pc.y:=y1;
  end
  else begin
    c1:=-a*x1-b*y1;
    c2:=b*p.X-a*p.Y;

    if Abs(b) > Abs(a) then begin
      c.x:=(c2*b-c1*a)/ab; c.y:=-(a*c.x+c1)/b
    end
    else begin
      c.y:=-(c2*a+c1*b)/ab; c.x:=-(b*c.y+c1)/a
    end;

    pc:=c; Result:=true
  end
end;

function xprj_to_Line(const p, q1,q2: TPoint; out pc: TGauss): Boolean;
var
  x1,x2,y1,y2, dx,dy: Integer;
begin
  Result:=false;
  pc.x:=p.x; pc.y:=p.y;

  dx:=Abs(q2.x-q1.x);
  dy:=Abs(q2.y-q1.y);

  x1:=Min(q1.x,q2.x) - dy;
  x2:=Max(q1.x,q2.x) + dy;

  y1:=Min(q1.y,q2.y) - dx;
  y2:=Max(q1.y,q2.y) + dx;

  if (p.x >= x1) and (p.x <= x2) then
  if (p.y >= y1) and (p.y <= y2) then

  Result:=prj_to_Line(p ,q1,q2, pc)
end;

function Project_to_Line(const p, q1,q2: TPoint; out pc: TPoint): Boolean;
var
  c: TGauss;
begin
  Result:=false; pc:=p;

  if prj_to_Line(p ,q1,q2, c) then begin
    pc.x:=Round(c.x); pc.y:=Round(c.y);
    Result:=true
  end
end;

function Plot_to_Line(const p, q1,q2: TPoint; out pc: TPoint): Boolean;
var
  c: TGauss;
begin
  Result:=false; pc:=q1;

  if prj_to_Line(p ,q1,q2, c) then

  if c.x >= Min(q1.X,q2.X)-0.01 then
  if c.y >= Min(q1.Y,q2.Y)-0.01 then
  if c.x <= Max(q1.X,q2.X)+0.01 then
  if c.y <= Max(q1.Y,q2.Y)+0.01 then begin
    pc.X:=Round(c.x); pc.Y:=Round(c.y);
    Result:=true
  end
end;

function Plot_to_Line1(const p ,q1,q2: TPoint; out pc: TPoint): Integer;
var
  x1,y1,x2,y2, dx1,dy1,dx2,dy2, cos, l1,l2: Double; c: TGauss;
begin
  Result:=-2; pc:=q2;
  if prj_to_Line(p ,q1,q2, c) then begin

    pc.X:=Round(c.x); pc.Y:=Round(c.y);
    Result:=0;

    x1:=q1.X; y1:=q1.Y; x2:=q2.X; y2:=q2.Y;

    dx1:=x1-c.x; dy1:=y1-c.y;
    dx2:=x2-c.x; dy2:=y2-c.y;

    cos:=dx1*dx2 + dy1*dy2;

    if cos > 0 then begin
      l1:=dx1*dx1 + dy1*dy1;
      l2:=dx2*dx2 + dy2*dy2;

      if l1 > l2 then Result:=+1 else
      if l1 < l2 then Result:=-1
    end
  end
end;

function Plot_to_Line2(const p, q1,q2,q3: TPoint): Boolean;
var
  ax,ay,cx,cy,d1,d2: Double;
begin
  Result:=true;

  cx:=q2.X; cy:=q2.Y;
  ax:=p.X - cx; ay:=p.Y - cy;

  d1:=ax*(q1.Y-cy) - ay*(q1.X-cx);
  d2:=ax*(q3.Y-cy) - ay*(q3.X-cx);

  if Abs(d1) > Small then
  if Abs(d2) > Small then
  if d1 < 0 then Result:=d2 > 0
  else           Result:=d2 < 0
end;

function Plot_to_XLine(const p,q1,q2: TGauss;
                       out cp: TGauss): Boolean;
var
  c: TGauss;
begin
  Result:=false; cp:=q1;

  if x_prj_to_Line(p ,q1,q2, c) then begin
    if c.x >= Min(q1.x,q2.x) then
    if c.y >= Min(q1.y,q2.y) then
    if c.x <= Max(q1.x,q2.x) then
    if c.y <= Max(q1.y,q2.y) then
    Result:=true; cp:=c;
  end
end;

function xPlot_to_Line(const p ,q1,q2: TPoint;
                       dr: Double; out pc: TPoint;
                       out dist: double): Boolean;
var
  pX,pY,r: int; lt,rb: TPoint; c: TGauss;
begin
  Result:=false; pc:=q1; dist:=0;

  lt:=q1; rb:=q2;
  if lt.X > rb.X then iSwap(lt.X,rb.X);
  if lt.Y > rb.Y then iSwap(lt.Y,rb.Y);

  if dr < 0 then r:=1 else r:=Trunc(dr+1);

  pX:=p.X; pY:=p.Y;

  if ((pX >= lt.X-r) and (pY >= lt.Y-r))
  or ((pX <= rb.X+r) and (pY <= rb.Y+r)) then

  if prj_to_Line(p ,q1,q2, c) then

  if (c.x >= lt.X) and (c.y >= lt.Y) then
  if (c.x <= rb.X) and (c.y <= rb.Y) then begin

    pc.X:=Round(c.x); pc.Y:=Round(c.y);
    dist:=Hypot(pX-c.x,pY-pc.y);
                             
    if (dr < 0) or (dist <= dr) then
    Result:=true
  end
end;

function t_plot(const p1,p2: TPoint; t: Double): TPoint;
begin
  Result.X:=p1.X + Round((p2.X-p1.X)*t);
  Result.Y:=p1.Y + Round((p2.Y-p1.Y)*t);
end;

function t_back(const p1,p2, p: TPoint): Double;
var
  dx,dy: Integer;
begin
  Result:=0;
  dx:=p2.X - p1.X;
  dy:=p2.Y - p1.Y;

  if Abs(dx) > Abs(dy) then
    Result:=(p.X - p1.X)/dx
  else
  if dy <> 0 then
    Result:=(p.Y - p1.Y)/dy
end;

function Ort_to_Line(const p ,q1,q2: TPoint; out c: TPoint): Boolean;
begin
  Result:=Project_to_Line(p, q1,q2, c); if Result then
  begin c.x:=p.x+q2.x-c.x; c.y:=p.y+q2.y-c.y end
end;

function xOrt_to_Line(const p ,q1,q2: TPoint; out c: TPoint): Boolean;
begin
  Result:=false; if Ort_to_Line(p ,q1,q2, c) then
  if Dist_to_Line(c ,q1,q2) <> 0 then Result:=true
end;

function OrtToLine(const p ,q1,q2: TGauss; out c: TGauss): Boolean;
begin
  Result:=x_prj_to_Line(p ,q1,q2, c); if Result then
  begin c.x:=p.x+q2.x-c.x; c.y:=p.y+q2.y-c.y end
end;

function xOrtToLine(const p ,q1,q2: TGauss; out c: TGauss): Boolean;
begin
  Result:=false; if OrtToLine(p ,q1,q2, c) then
  if gDist_to_Line(c ,q1,q2) <> 0 then Result:=true
end;

function Locate_Line(const a,b, lt,rb: TPoint; out c: TPoint): Boolean;
var
  p1,p2: TPoint; dx,dy,r: Integer;
  kx,ky: double; t1,t2,t: tgauss;
begin
  Result:=false; Middle_Point(lt,rb,c);

  p1.X:=Min(a.X,b.X); p1.Y:=Min(a.Y,b.Y);
  p2.X:=Max(a.X,b.X); p2.Y:=Max(a.Y,b.Y);

  if (p1.X < rb.X) and (p2.X > lt.X) then
  if (p1.Y < rb.Y) and (p2.Y > lt.Y) then begin

    r:=Max(1,(rb.X-lt.X) div 2);

    dx:=b.X-a.X; dy:=b.Y-a.Y;

    if dx = 0 then begin
      if (lt.Y >= p1.Y-r) and (rb.Y <= p2.Y+r) then
      begin c.X:=a.X; Result:=true end
    end else
    if dy = 0 then begin
      if (lt.X >= p1.X-r) and (rb.X <= p2.X+r) then
      begin c.Y:=a.Y; Result:=true end
    end
    else begin
      kx:=dx/dy; ky:=dy/dx;

      t1.x:=lt.X; t1.y:=a.Y+(lt.X-a.X)*ky;
      t2.x:=rb.X; t2.y:=a.Y+(rb.X-a.X)*ky;

      if t1.y > t2.y then begin
        t:=t1; t1:=t2; t2:=t
      end;

      if t1.y < rb.y then
      if t2.y > lt.y then begin

        if t1.y < lt.y then begin
          t1.x:=a.X+(lt.Y-a.Y)*kx; t1.y:=lt.Y
        end;
        if t2.y > rb.y then begin
          t2.x:=a.X+(rb.Y-a.Y)*kx; t2.y:=rb.Y
        end;

        c.X:=Round(t1.x/2+t2.x/2);
        c.Y:=Round(t1.y/2+t2.y/2);

        Result:=true
      end
    end
  end
end;

procedure Rotate_LPoint(const a,b,c, p: TPoint;
                        out q: TPoint);
var
  r,f: double; _s,_c: Extended;
begin
  r:=Long_Dist(a,p);
  f:=iArcTan(a.x,a.y,p.x,p.y) +
     iArcTan(a.x,a.y,c.x,c.y) -
     iArcTan(a.x,a.y,b.x,b.y);

  SinCos(f, _s,_c);
  q.x:=a.x + Round(_c * r);
  q.y:=a.y + Round(_s * r)
end;

function Rotate_vpoint(const a,b,c: TPoint;
                       const v: vpoint): vpoint;
var
  p: TPoint;
begin
  Rotate_LPoint(a,b,c, Point(v.x,v.y), p);
  Result.x:=p.x; Result.y:=p.y; Result.z:=v.z;
end;

function Azimuth_Point(const p,c: TPoint; fi: Double): TPoint;
var
  d: double; _s,_c: Extended;
begin
  SinCos(fi, _s,_c); d:=Long_Dist(c,p);

  Result.x:=c.x + Round(_c * d);
  Result.y:=c.y + Round(_s * d)
end;

function fi_Rotate_Point(const p,c: TPoint; fi: Double): TPoint;
var
  d: double; _s,_c: Extended;
begin
  fi:=iArcTan(c.X,c.Y,p.X,p.Y) - fi;
  SinCos(fi, _s,_c); d:=Long_Dist(c,p);

  Result.X:=c.X + Round(_c * d);
  Result.Y:=c.Y + Round(_s * d)
end;

function Double_Point(const a,b,c,d: TPoint; r: double;
                      out p: TPoint): Boolean;
var
  t: TPoint; d1,d2,r_2: Double;
begin
  Result:=false;

  Middle_Point(b,c,t); p:=t;

  d1:=Dist_to_Line(b, a,t);
  d2:=Dist_to_Line(c, t,d);

  r_2:=Max(0.75,r/2);

  if Abs(d1) <= r_2 then
  if Abs(d2) <= r_2 then

  if ((d1 >= 0) and (d2 <= 0))
  or ((d1 <= 0) and (d2 >= 0)) then

  if Long_Dist(b,c) < 2*r then
    Result:=true
  else
  if Long_Dist(a,b) <= Long_Dist(a,t) then
  if Long_Dist(c,d) <= Long_Dist(t,d) then

  Result:=true
end;

function Join_Points(const a,b,c,d: TPoint; r: double;
                     out p: TPoint): Boolean;
var
  t: TPoint; d1,d2,r1,r2: Double;
begin
  Result:=false;

  Middle_Point(b,c,t); p:=t;

  d1:=Dist_to_Line(b, a,t);
  d2:=Dist_to_Line(c, t,d);

  if Abs(d1) <= r then
  if Abs(d2) <= r then

  if ((d1 >= 0) and (d2 <= 0))
  or ((d1 <= 0) and (d2 >= 0)) then begin

    r1:=Long_Dist(b,t)*3;
    r2:=Long_Dist(c,t)*3;

    if Long_Dist(a,b) >= r1 then
    if Long_Dist(a,t) >= r1 then

    if Long_Dist(d,c) >= r2 then
    if Long_Dist(d,t) >= r2 then

    Result:=true
  end
end;

procedure Mirror_Point(const p,c: TPoint; var q: TPoint);
begin
  q.X:=c.X+c.X-p.X; q.Y:=c.Y+c.Y-p.Y
end;

procedure Rotate_Gauss(a,b,c, p: tgauss; var q: tgauss);
var
  d,f: double; _s,_c: Extended;
begin
  d:=Gauss_Dist(a,p);
  f:=xArcTan(a.x,a.y,p.x,p.y) +
     xArcTan(a.x,a.y,c.x,c.y) -
     xArcTan(a.x,a.y,b.x,b.y);

  SinCos(f, _s,_c);
  q.x:=a.x + (_c * d);
  q.y:=a.y + (_s * d)
end;

procedure Mirror_Gauss(const p,c: tgauss; var q: tgauss);
begin
  q.x:=c.x+c.x-p.x; q.y:=c.y+c.y-p.y
end;

function LineContainsPoint(const a,b, p: TPoint): Boolean;
var
  ax,ay,bx,by, px,py, x1,y1,x2,y2, dx,dy: int; d1,d2: Double;
begin
  Result:=false;

  ax:=a.X; ay:=a.Y; bx:=b.X; by:=b.Y; px:=p.X; py:=p.Y;

  x1:=ax; x2:=bx; if x1 > x2 then begin x1:=bx; x2:=ax end;
  y1:=ay; y2:=by; if y1 > y2 then begin y1:=by; y2:=ay end;

  if (px >= x1) and (px <= x2) then
  if (py >= y1) and (py <= y2) then begin

    dx:=bx-ax; dy:=by-ay;

    if (dx = 0) or (dy = 0) then
      Result:=true
    else begin

      if Abs(dx) > Abs(dy) then begin
        d1:=ay + (px-ax)/dx*dy - py;
        d2:=by + (px-bx)/dx*dy - py;
      end
      else begin
        d1:=ax + (py-ay)/dy*dx - px;
        d2:=bx + (py-by)/dy*dx - px;
      end;

      Result:=Min(Abs(d1),Abs(d2)) < line_eps
    end
  end
end;

function LineContainsPoint1(const a,b, p: TPoint): Boolean;
var
  eps: double;
begin
  eps:=line_eps; line_eps:=0.7;
  Result:=LineContainsPoint(a,b, p);
  line_eps:=eps
end;

function LineContainsLine(const a,b, q,p: TPoint): Boolean;
begin
  Result:=false;
  if LineContainsPoint(a,b, q) then
  if LineContainsPoint(a,b, p) then
  Result:=true
end;

function LineContainsEdge(const a,b: TPoint;
                          const max_r: Double;
                          lp: PPoly; lp_n: Integer;
                          out pc: TPoint): Integer;
var
  lt,rb, p,q: TPoint;
  i, dx,dy: Integer;
  d,d1,d2: Double;
begin
  Result:=0; pc:=p;

  if lp_n > 0 then begin

    lt.x:=Min(a.x,b.x); rb.x:=Max(a.x,b.x);
    lt.y:=Min(a.y,b.y); rb.y:=Max(a.y,b.y);

    dx:=b.x-a.x; dy:=b.y-a.y;

    for i:=0 to lp_n-1 do begin

      p:=lp[i];

      if (p.x < lt.x) or (p.x > rb.x)
      or (p.y < lt.y) or (p.y > rb.y) then
        Break
      else
      if (p.x = b.x) and (p.y = b.y) then begin
        pc:=p; Result:=i+1;
        if line_comp_stop then Break
      end else
      if (dx = 0) or (dy = 0) then begin
        pc:=p; Result:=i+1
      end
      else begin q:=p;

        if Abs(dx) > Abs(dy) then begin
          d1:=a.y + (p.x-a.x)/dx*dy - p.y;
          d2:=b.y + (p.x-b.x)/dx*dy - p.y;
          d:=Min(Abs(d1),Abs(d2));
          q.y:=Round(p.y + d)
        end
        else begin
          d1:=a.x + (p.y-a.y)/dy*dx - p.x;
          d2:=b.x + (p.y-b.y)/dy*dx - p.x;
          d:=Min(Abs(d1),Abs(d2));
          q.x:=Round(p.x + d)
        end;

        if d < line_eps then begin
          pc:=q; Result:=i+1
        end else
        if d > max_r then Break
      end
    end
  end
end;

function LineContainsPoly(const a,b: TPoint;
                          const max_r: Double;
                          lp: PLLine; lp_i: Integer;
                          dir: Integer): Integer;
var
  lt,rb, p: TPoint;
  n, dx,dy: Integer;
  d,d1,d2: Double;
begin
  Result:=0; n:=0;

  lt.x:=Min(a.x,b.x); rb.x:=Max(a.x,b.x);
  lt.y:=Min(a.y,b.y); rb.y:=Max(a.y,b.y);

  dx:=b.x-a.x; dy:=b.y-a.y;

  while (lp_i >= 0) and (lp_i <= lp.N) do begin

    p:=lp.Pol[lp_i]; Inc(n);

    if (p.x < lt.x) or (p.x > rb.x)
    or (p.y < lt.y) or (p.y > rb.y) then
      Break
    else
    if (p.x = b.x) and (p.y = b.y) then begin
      Result:=n; if line_comp_stop then Break
    end else
    if (dx = 0) or (dy = 0) then
      Result:=n
    else begin
      if Abs(dx) > Abs(dy) then begin
        d1:=a.y + (p.x-a.x)/dx*dy - p.y;
        d2:=b.y + (p.x-b.x)/dx*dy - p.y;
      end
      else begin
        d1:=a.x + (p.y-a.y)/dy*dx - p.x;
        d2:=b.x + (p.y-b.y)/dy*dx - p.x;
      end;

      d:=Min(Abs(d1),Abs(d2));
      if d < line_eps then
        Result:=n
      else
      if d > max_r then Break
    end;

    Inc(lp_i,dir)
  end
end;

function PortContainsPoint(const lt,rb: TPoint; x,y: Integer): Boolean;
begin
  Result:=false;
  if (x >= lt.X) and (y >= lt.Y) then
  if (x <= rb.X) and (y <= rb.Y) then
  Result:=true
end;

function FrameContainsPoint(const lt,rb: TPoint; x,y,r: Integer): Boolean;
begin
  Result:=false;
  if (x >= lt.X-r) and (y >= lt.Y-r) then
  if (x <= rb.X+r) and (y <= rb.Y+r) then
  Result:=true
end;

function PortContainsRect(const lt,rb: TPoint; x1,y1,x2,y2: Integer): Boolean;
begin
  Result:=
  (lt.x <= x2) and (x1 <= rb.x) and
  (lt.y <= y2) and (y1 <= rb.y)
end;

function BoundContainsRect(const lt,rb: TPoint; x1,y1,x2,y2: Integer): Boolean;
begin
  Result:=
  (x1 < rb.x) and (x2 > lt.x) and
  (y1 < rb.y) and (y2 > lt.y)
end;

function BoundContainsPort(const lt,rb, _lt,_rb: TPoint): Boolean;
begin
  Result:=
  (_lt.x < rb.x) and (_rb.x > lt.x) and
  (_lt.y < rb.y) and (_rb.y > lt.y)
end;

function PortContainsPort(const lt,rb, _lt,_rb: TPoint): Boolean;
begin
  Result:=
  (lt.X <= _rb.X) and (_lt.X <= rb.X) and
  (lt.Y <= _rb.Y) and (_lt.Y <= rb.Y)
end;

function IsInnerPort(const lt,rb, _lt,_rb: TPoint): Boolean;
begin
  Result:=
  (_lt.X >= lt.X) and (_rb.X <= rb.X) and
  (_lt.Y >= lt.Y) and (_rb.Y <= rb.Y)
end;

function PortContainsLine(const lt,rb, a,b: TPoint): Boolean;
var
  a_x,a_y,b_x,b_y: Integer;
begin
  Result:=false;

  a_x:=Min(a.X,b.X); a_y:=Min(a.Y,b.Y);
  b_x:=Max(a.X,b.X); b_y:=Max(a.Y,b.Y);

  if (a_x <= rb.X) and (b_x >= lt.X) then
  if (a_y <= rb.Y) and (b_y >= lt.Y) then
  Result:=true
end;

function RectContainsRect(const Owner,Inner: TRect): Boolean;
begin
  Result:=(Owner.Left <= Inner.Right) and
          (Inner.Left <= Owner.Right) and
          (Owner.Top <= Inner.Bottom) and
          (Inner.Top <= Owner.Bottom)
end;

function RectContainsPoint(const R: TRect; const P: TPoint): Boolean;
begin
  Result:=(P.X >= R.Left) and
          (P.X <= R.Right) and
          (P.Y >= R.Top) and
          (P.Y <= R.Bottom)
end;

function RectClipRect(const Owner,Inner: TRect; Delta: Integer): Boolean;
begin
  Result:=false;

  if (Inner.Left < Owner.Left-Delta)
  or (Inner.Top < Owner.Top-Delta)
  or (Inner.Right > Owner.Right+Delta)
  or (Inner.Bottom > Owner.Bottom+Delta)

  then Result:=true
end;

function IncClipRect(const R: TRect; d: Integer): TRect;
begin
  Result:=Rect(R.Left-d,R.Top-d,R.Right+d,R.Bottom+d)
end;

function RingContainsPoint(cx,cy, r, px,py: Double): Boolean;
begin
  Result:=Hypot(px-cx,py-cy) <= r
end;

function GaussContainsPoint(const lt,rb, p: tgauss): Boolean;
begin
  Result:=false;
  if (p.x >= lt.x) and (p.y >= lt.y) then
  if (p.x <= rb.x) and (p.y <= rb.y) then
  Result:=true
end;

function GaussContainsRect(const lt,rb: tgauss;
                           x1,y1,x2,y2: double): Boolean;
begin
  Result:=
  (lt.x <= x2) and (x1 <= rb.x) and
  (lt.y <= y2) and (y1 <= rb.y)
end;

function GaussContainsPort(const lt,rb, _lt,_rb: tgauss): Boolean;
begin
  Result:=
  (lt.x <= _rb.x) and (_lt.x <= rb.x) and
  (lt.y <= _rb.y) and (_lt.y <= rb.y)
end;

function GaussContainsLine(const lt,rb, a,b: tgauss): Boolean;
var
  minx,miny,maxx,maxy: Double;
begin
  Result:=false;
  minx:=Min(a.x,b.x); miny:=Min(a.y,b.y);
  maxx:=Max(a.x,b.x); maxy:=Max(a.y,b.y);

  if (minx <= rb.x) and (maxx >= lt.x) then
  if (miny <= rb.y) and (maxy >= lt.y) then
  Result:=true
end;

function Rect_Contains(const R: TRect; x,y: integer): Boolean;
begin
  Result:=false;
  if (X > R.Left) and (Y > R.Top) and
  (X < R.Right) and (Y < R.Bottom) then
  Result:=true
end;

procedure Swap_Rect(var R: TRect);
var
  T: TRect;
begin
  T:=R;
  if T.Left > T.Right then begin
    R.Left:=T.Right; R.Right:=T.Left
  end;

  if T.Top > T.Bottom then begin
    R.Top:=T.Bottom; R.Bottom:=T.Top
  end
end;

function Point_ContainsPoint(cx,cy,d, x,y: integer): Boolean;
begin
  Result:=Rect_Contains(Rect(cx-d,cy-d,cx+d,cy+d), x,y)
end;

procedure Swap_lRect(const a,b: TPoint; out c,d: TPoint);
begin
  c:=a; d:=b;
  if a.X > b.X then iSwap(c.X,d.X);
  if a.Y > b.Y then iSwap(c.Y,d.Y)
end;

procedure Mult_LRect(const a,b: TPoint; k: float; out c,d: TPoint);
var
  dx,dy: longint;
begin
  dx:=Round((b.X-a.X)/k);
  dy:=Round((b.Y-a.Y)/k);
  if dy > dx then dx:=dy;
  c.X:=a.X-dx; c.Y:=a.Y-dy;
  d.X:=b.X+dx; d.Y:=b.Y+dy
end;

procedure Scale_LPort(var a,b: TPoint; k: double);
var
  dx,dy: longint;
begin
  dx:=Round((b.x-a.x)/k);
  dy:=Round((b.y-a.y)/k);
  if dy > dx then dx:=dy;
  a.x:=a.x-dx; a.y:=a.y-dy;
  b.x:=b.x+dx; b.y:=b.y+dy
end;

procedure View_lPort(var a,b: TPoint; k: double);
var
  dx,dy: int;
begin
  dx:=Round((b.x-a.x)*k);
  dy:=Round((b.y-a.y)*k);
  if dx > dy then dx:=dy;
  a.X:=a.X+dx; a.Y:=a.Y+dy;
  b.X:=b.X-dx; b.Y:=b.Y-dy
end;

procedure Clip_LRect(const a,b, lt,rb: TPoint; out c,d: TPoint);
begin
  c.X:=Max(a.X,lt.X); c.Y:=Max(a.Y,lt.Y);
  d.X:=Min(b.X,rb.X); d.Y:=Min(b.Y,rb.Y)
end;

function Locator(X,Y,R: int): LVector;
var
  v: LVector;
begin
  v[0]:=Point(X-R,Y-R);
  v[1]:=Point(X+R,Y+R);
  Result:=v
end;

function Clip_LPort(const R: TRect; var lt,rb: TPoint): Boolean;
begin
  Result:=false;

  if lt.X < R.Left   then lt.X:=R.Left;
  if lt.Y < R.Top    then lt.Y:=R.Top;
  if rb.X > R.Right  then rb.X:=R.Right;
  if rb.Y > R.Bottom then rb.Y:=R.Bottom;

  if lt.X <= rb.X then
  if lt.Y <= rb.Y then Result:=true
end;

function Clip_gPort(const R: TRect; var lt,rb: TGauss): Boolean;
begin
  Result:=false;

  if lt.X < R.Left   then lt.X:=R.Left;
  if lt.Y < R.Top    then lt.Y:=R.Top;
  if rb.X > R.Right  then rb.X:=R.Right;
  if rb.Y > R.Bottom then rb.Y:=R.Bottom;

  if lt.X <= rb.X then
  if lt.Y <= rb.Y then Result:=true
end;

procedure Inc_lRect(var a,b: TPoint; dx,dy: int);
begin
  Dec(a.X,dx); Dec(a.Y,dy); Inc(b.X,dx); Inc(b.Y,dy)
end;

procedure move_lRect(var a,b: TPoint; dx,dy: int);
begin
  Inc(a.X,dx); Inc(a.Y,dy); Inc(b.X,dx); Inc(b.Y,dy)
end;

procedure Trunc_lRect(var a,b: TPoint; x1,y1,x2,y2: longint);
begin
  if a.X < x1 then a.X:=x1; if b.X > x2 then b.X:=x2;
  if a.Y < y1 then a.Y:=y1; if b.Y > y2 then b.Y:=y2
end;

procedure max_lRect(var a,b: TPoint; w,h, x1,y1,x2,y2: longint);
var
  c,t1,t2: TPoint;
begin
  Trunc_lRect(a,b, x1,y1,x2,y2);

  if (b.x-a.x+1) < w then
  if (b.y-a.y+1) < h then begin

    Middle_Point(a,b,c);
    t1.x:=c.x-(w div 2); if t1.x < x1 then t1.x:=x1;
    t1.y:=c.y-(h div 2); if t1.y < y1 then t1.y:=y1;

    t2.x:=t1.x+w-1; if t2.x > x2 then t2.x:=x2;
    t2.y:=t1.y+h-1; if t2.y > y2 then t2.y:=y2;
    a:=t1; b:=t2

  end
end;

procedure Max_lPort(var lt,rb: TPoint; const p: TPoint);
begin
  lt.X:=Min(lt.X,p.X); rb.X:=Max(rb.X,p.X);
  lt.Y:=Min(lt.Y,p.Y); rb.Y:=Max(rb.Y,p.Y);
end;

procedure Max_VPort(var lt,rb: vpoint; const p: vpoint);
begin
  lt.x:=Min(lt.x,p.x); rb.x:=Max(rb.x,p.x);
  lt.y:=Min(lt.y,p.y); rb.y:=Max(rb.y,p.y);
  lt.z:=Min(lt.z,p.z); rb.z:=Max(rb.z,p.z);
end;

procedure Add_LRect(var lt,rb: TPoint; const a,b: TPoint);
begin
  lt.X:=Min(lt.X,a.X); rb.X:=Max(rb.X,b.X);
  lt.Y:=Min(lt.Y,a.Y); rb.y:=Max(rb.Y,b.Y);
end;

procedure Add_VRect(var lt,rb: vpoint; const a,b: vpoint);
begin
  lt.x:=Min(lt.x,a.x); rb.x:=Max(rb.x,b.x);
  lt.y:=Min(lt.y,a.y); rb.y:=Max(rb.y,b.y);
  lt.z:=Min(lt.z,a.z); rb.z:=Max(rb.z,b.z);
end;

procedure Centre_lRect(const lt,rb: TPoint; var a,b: TPoint);
var
  c: TPoint; w,h: int;
begin
  Middle_Point(lt,rb,c);
  w:=b.X-a.X; h:=b.Y-a.Y;

  if w >= rb.X-lt.X then begin
    w:=(rb.X-lt.X) div 2;
    a.X:=c.X-w; b.X:=c.X+w
  end else
  if b.X > rb.X then begin
    b.X:=rb.X; a.X:=rb.X-w
  end else
  if a.X < lt.X then begin
    a.X:=lt.X; b.X:=lt.X+w
  end;

  if h >= rb.Y-lt.Y then begin
    h:=(rb.Y-lt.Y) div 2;
    a.Y:=c.Y-h; b.Y:=c.Y+h
  end else
  if b.Y > rb.Y then begin
    b.Y:=rb.Y; a.Y:=rb.Y-h
  end else
  if a.Y < lt.Y then begin
    a.Y:=lt.Y; b.Y:=lt.Y+h
  end;
end;

function Track_lRect(var R: TRect; const p: TPoint): Boolean;
var
  w,h,d: Integer;
begin
  w:=R.Right-R.Left; h:=R.Bottom-R.Top;
  d:=Min(w,h) div 16; Result:=false;

  if (p.x < R.Left+d) or (p.x > R.Right-d)
  or (p.y < R.Top+d) or (p.y > R.Bottom+d) then begin
    R.Left:=p.x - (w div 2); R.Right:=R.Left + w;
    R.Top:=p.y - (h div 2); R.Bottom:=R.Top + h;
    Result:=true
  end
end;

procedure Locate_Centre(const Port: TRect; var P: TPoint);
begin
  with Port do begin
    P.X:=Min(Max(P.X,Left),Right);
    P.Y:=Min(Max(P.Y,Top),Bottom)
  end
end;

procedure Inc_GRect(var a,b: tgauss; dx,dy: double);
begin
  a.x:=a.x-dx; b.x:=b.x+dx;
  a.y:=a.y-dy; b.y:=b.y+dy;
end;

procedure Max_GPort(var lt,rb: tgauss; const p: tgauss);
begin
  lt.x:=Min(lt.x,p.x); rb.x:=Max(rb.x,p.x);
  lt.y:=Min(lt.y,p.y); rb.y:=Max(rb.y,p.y);
end;

procedure Mult_GRect(const a,b: tgauss; k: double; var c,d: tgauss);
var
  dx,dy: Double;
begin
  dx:=(b.x-a.x)/k;
  dy:=(b.y-a.y)/k;
  if dy > dx then dx:=dy;
  c.x:=a.x-dx; c.y:=a.y-dy;
  d.x:=b.x+dx; d.y:=b.y+dy
end;

procedure Scale_GPort(var a,b: TGauss; k: double);
var
  dx,dy: Double;
begin
  dx:=(b.x-a.x)/k;
  dy:=(b.y-a.y)/k;
  if dy > dx then dx:=dy;
  a.x:=a.x-dx; a.y:=a.y-dy;
  b.x:=b.x+dx; b.y:=b.y+dy
end;

procedure Add_GRect(var lt,rb: tgauss; const a,b: tgauss);
begin
  if a.x < lt.x then lt.x:=a.x;
  if a.y < lt.y then lt.y:=a.y;
  if b.x > rb.x then rb.x:=b.x;
  if b.y > rb.y then rb.y:=b.y;
end;

procedure Swap_GRect(const a,b: tgauss; var c,d: tgauss);
begin
  c.x:=Min(a.x,b.x); c.y:=Min(a.y,b.y);
  d.x:=Max(a.x,b.x); d.y:=Max(a.y,b.y)
end;

procedure Centre_GRect(const lt,rb: tgauss; var a,b: tgauss);
var
  c: tgauss; w,h: Double;
begin
  Middle_Gauss(lt,rb,c);
  w:=b.x-a.x; h:=b.y-a.y;

  if w >= rb.x-lt.x then begin
    w:=(rb.x-lt.x) / 2;
    a.x:=c.x-w; b.x:=c.x+w
  end else
  if b.x > rb.x then begin
    b.x:=rb.x; a.x:=rb.x-w
  end else
  if a.x < lt.x then begin
    a.x:=lt.x; b.x:=lt.x+w
  end;

  if h >= rb.y-lt.y then begin
    h:=(rb.y-lt.y) / 2;
    a.y:=c.y-h; b.y:=c.y+h
  end else
  if b.y > rb.y then begin
    b.y:=rb.y; a.y:=rb.y-h
  end else
  if a.y < lt.y then begin
    a.y:=lt.y; b.y:=lt.y+h
  end;
end;

procedure Clip_gRect(const a,b, lt,rb: tgauss; var c,d: tgauss);
begin
  c.x:=Max(a.x,lt.x);
  c.y:=Max(a.y,lt.y);
  d.x:=Min(b.x,rb.x);
  d.y:=Min(b.y,rb.y)
end;

procedure Up_Line(const a,b: TPoint; h: double; var c,d: TPoint);
var
  nx,ny,len: double;
begin
  nx:=a.y-b.y; ny:=b.x-a.x;
  len:=Hypot(nx,ny); c:=a; d:=b;

  if len > 0.001 then begin
    c.x:=a.x + Round(nx*h/len);
    c.y:=a.y + Round(ny*h/len);

    d.x:=c.x - a.x + b.x;
    d.y:=c.y - a.y + b.y
  end
end;

function Get_Regular(x1,y1,x2,y2, n,lp_max: Integer; lp: PLPoly): Integer;
var
  i,k, x,y,a,b: Integer;
  f,df,min: double; _s,_c: Extended;
begin
  k:=n; if n = 0 then n:=lp_max;
  if n > lp_max then n:=lp_max;

  x:=(x1+x2) div 2; a:=x-x1;
  y:=(y1+y2) div 2; b:=y-y1;

  min:=Max(2, Hypot(x2-x1,y2-y1)/16);

  while true do begin
    f:=0; df:=2*Pi / n;

    for i:=0 to n-1 do begin
      SinCos(f, _s,_c); f:=f+df;
      lp[i].x:=x + Round(a * _c);
      lp[i].y:=y + Round(b * _s);
    end;

    if (k > 0) or (n <= 8) then Break;
    if Long_Dist(lp[0],lp[1]) >= min then Break;
    Dec(n,4); if n < 8 then n:=8
  end;

  lp[n]:=lp[0]; Result:=n
end;

function Clip_GPoly(lp: pGPoly; n: Integer; lx,rx: double): Integer;

function Clip_Point(x, lx,rx: double): Integer;
begin
  if x < lx then Result:=1 else
  if x > rx then Result:=2 else
  Result:=0
end;

function Clip_Line(a,b: tgauss; x: double): tgauss;
var
  dx,dy, k: double;
begin
  dx:=b.x-a.x; dy:=b.y-a.y;
  k:=0; if dx <> 0 then k:=(x-a.x)/dx;
  Result.x:=a.x+dx*k;
  Result.y:=a.y+dy*k
end;

var
  p1,p2: tgauss; x: double;
  i,rc1,rc2: Integer;

begin
  Result:=-1; if n > 0 then begin

    p2:=lp^[0]; rc2:=Clip_Point(p2.x, lx,rx);
    if rc2 = 0 then Inc(Result);

    for i:=1 to n do begin p1:=p2; rc1:=rc2;
      p2:=lp^[i]; rc2:=Clip_Point(p2.x, lx,rx);

      if rc2 = 0 then begin

        if rc1 <> 0 then begin Inc(Result);
          if rc1 = 1 then x:=lx else x:=rx;
          lp[Result]:=Clip_Line(p1,p2,x)
        end;

        Inc(Result); lp[Result]:=p2

      end else
      if rc1 = 0 then begin Inc(Result);
        if rc2 = 1 then x:=lx else x:=rx;
        lp[Result]:=Clip_Line(p1,p2,x)
      end
    end

  end
end;

function Clip_Line(const lt,rb, a,b: TPoint; out c,d: TPoint): Boolean;
var
  _a,_b,t: TPoint; rc: Integer;
begin
  Result:=false; rc:=0;

  if PortContainsPoint(lt,rb, a.x,a.y) then Inc(rc,1);
  if PortContainsPoint(lt,rb, b.x,b.y) then Inc(rc,2);

  _a:=a; _b:=b; if rc = 2 then begin _a:=b; _b:=a end;

  if rc > 0 then begin

    if rc <> 3 then

    while true do begin
      Middle_Point(_a,_b, t);
      if PortContainsPoint(lt,rb, t.x,t.y) then
      Break else _b:=t
    end;

    c:=_a; d:=_b; Result:=true
  end
end;

function xClip_Line(const lt,rb, a,b: TPoint; out c,d: TPoint): Boolean;
var
  _a,_b,_c,_d: TPoint;
begin
  Result:=false; c:=a; d:=b;

  _a.x:=a.x-lt.x; _b.x:=b.x-lt.x;
  _a.y:=a.y-lt.y; _b.y:=b.y-lt.y;

  if yClip_Line(rb.x-lt.x,rb.y-lt.y, _a,_b, _c,_d) then begin
    c.x:=_c.x + lt.x;
    c.y:=_c.y + lt.y;
    d.x:=_d.x + lt.x;
    d.y:=_d.y + lt.y;
    Result:=true
  end
end;

function rClip_Line(const R: TRect;
                    const a,b: TPoint;
                    out c,d: TPoint): Boolean;
var
  lt,rb, _a,_b,_c,_d: TPoint;
begin
  Result:=false; c:=a; d:=b;

  lt:=R.TopLeft; rb:=R.BottomRight;

  _a.x:=a.x-lt.x; _b.x:=b.x-lt.x;
  _a.y:=a.y-lt.y; _b.y:=b.y-lt.y;

  if yClip_Line(rb.x-lt.x,rb.y-lt.y, _a,_b, _c,_d) then begin
    c.x:=_c.x + lt.x;
    c.y:=_c.y + lt.y;
    d.x:=_d.x + lt.x;
    d.y:=_d.y + lt.y;
    Result:=true
  end
end;

function yClip_Line(w,h: Integer; const a,b: TPoint;
                    out c,d: TPoint): Boolean;

function Point_Code(X,Y, W,H: Integer): Integer;
begin
  Result:=0;
  if X < 0 then Inc(Result,1);
  if X > W then Inc(Result,2);
  if Y < 0 then Inc(Result,4);
  if Y > H then Inc(Result,8);
end;

var
  _a,_b, p: TPoint; v: lvector;
  ac,bc, dx,dy,ic, x,y: Integer;
  m: Double; swap: Boolean;
begin
  Result:=false; c:=a; d:=b;

  _a:=a; _b:=b;

  ac:=Point_Code(_a.x,_a.y, w,h);
  bc:=Point_Code(_b.x,_b.y, w,h);

  v[0]:=_a; v[1]:=_b; ic:=0; swap:=false;

  if ac + bc = 0 then
    ic:=2
  else
  if ac and bc = 0 then begin

    ic:=0;

    if ac = 0 then
      ic:=1
    else
    if bc = 0 then begin
      v[0]:=_b; v[1]:=_a; ic:=1; swap:=true
    end;

    dx:=_b.x-_a.x; dy:=_b.y-_a.y;
    m:=0; if dx <> 0 then m:=dy/dx;

    while ic < 2 do begin
      p:=v[ic];

      if dx = 0 then begin
        if p.y < 0 then p.y:=0 else p.y:=h;
        v[ic]:=p; Inc(ic); Continue
      end
      else begin
        if p.x < 0 then begin
          y:=p.y + Round((0 - p.x) * m);
          if (y >= 0) and (y <= h) then begin
            p.x:=0; p.y:=y; v[ic]:=p;
            Inc(ic); Continue
          end
        end;

        if p.x > w then begin
          y:=p.y + Round((w - p.x) * m);
          if (y >= 0) and (y <= h) then begin
            p.x:=w; p.y:=y; v[ic]:=p;
            Inc(ic); Continue
          end
        end;
      end;

      if dy <> 0 then begin
        if p.y < 0 then begin
          x:=p.x + Round((0 - p.y) / m);
          if (x >= 0) and (x <= w) then begin
            p.x:=x; p.y:=0; v[ic]:=p;
            Inc(ic); Continue
          end
        end;

        if p.y > h then begin
          x:=p.x + Round((h - p.y) / m);
          if (x >= 0) and (x <= w) then begin
            p.x:=x; p.y:=h; v[ic]:=p;
            Inc(ic); Continue
          end
        end;
      end;

      Break
    end

  end;

  if ic = 2 then begin

    if swap then begin
      p:=v[0]; v[0]:=v[1]; v[1]:=p
    end;

    c.x:=v[0].x; c.y:=v[0].y;
    d.x:=v[1].x; d.y:=v[1].y;

    Result:=true
  end
end;

begin
  Pi_2:=Pi/2
end.
