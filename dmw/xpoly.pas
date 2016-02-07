unit xpoly; interface {$A1}

uses
  Windows,Classes,Math,otypes,xline;

type                            
  TLineMarker = record
    Ind,Res: int; pc: TPoint
  end;

  PLineMarker1 = ^TLineMarker1;
  TLineMarker1 = record                      
    Ind: Integer; pc: TPoint;
    dist,err: Double;
  end;
                                           
  PLineMarkers1 = ^TLineMarkers1;
  TLineMarkers1 = Array[0..1023] of TLineMarker1;

  TFilter = object
    a1,a2,a3,al: double;

    constructor Init;

    function Dist_to_Line(const p, a,b: TPoint): double;
    function Next_Dist(const p: TPoint): double;

    function Collinear(lp: PLPoly; n: Integer; dl: double): Boolean;

    function Cls(lp: PLLine; i1,i2,o: Integer; dl: double): Integer;
    procedure Norm(lp: PLLine; i1,i3,o: Integer); virtual;

    function x_bound(lp: PLPoly; N: Integer; dl: double): Integer;
    function y_bound(lp: PLLine; o: Integer; dl: double): Integer;
  end;

  TOrtPoly = object(TFilter) nv: TGPoly;
    procedure Norm(lp: PLLine; i1,i3,o: Integer); virtual;
    procedure z_bound(lp: PLLine; dl: double);
  end;

function LineMarker(Ind: Integer; const pc: TPoint): TLineMarker;

function LineMarker1(Ind: Integer; const pc: TPoint;
                     dist: Double): TLineMarker1;

function LineMarker1Of(mk: PLineMarkers1; n: Integer;
                       const p: TPoint): Integer;

function DistMarker1(lp: PLPoly; lp_n,Ind: int;
                     const pc: TPoint): TLineMarker1;

function top_LineMarker(lp: PLLine; const mk: TLineMarker): Boolean;
function bot_LineMarker(lp: PLLine; const mk: TLineMarker): Boolean;
function edge_LineMarker(lp: PLLine; const mk: TLineMarker): Boolean;

function zaxe_LineMarker(lp: PLLine; hp: PIntegers;
                         const mk: TLineMarker): VPoint;

function PolyEqual(lp1: PLPoly; n1: int;
                   lp2: PLPoly; n2: int): bool;

function Backup_lp(lp: PLLine; hp: PIntegers;
                   lp_i: Integer): lxyz;

procedure Restore_lp(lp: PLLine; hp: PIntegers;
                     lp_i: Integer; const v: lxyz);

function Poly_cpos(lp: PLLine;
                   lp_ind: Integer;
                   lp_rmu: Double;
                   out p: TPoint): Boolean;

function Poly_range(lp: PLLine;
                    i1: int; k1: float;
                    i2: int; k2: float): int;

function Poly_cls(lp: PLLine): Integer;
function Poly_Empty(lp: PLLine): Boolean;

function LPoly_Last(lp: PLLine): TPoint;

function LLine_IndexOf(lp: PLLine; const P: TPoint;
                       is_back: Boolean): Integer;

function LLine_RibOf(lp: PLLine; vp: PLPoly): Integer;

function Copy_PolyBuf(lp: PLLine; dn: Integer): PLLine;

function LLine_to_VLine(lp: PLLine; hp: PIntegers;
                        vp: PVLine; Max: Integer): Integer;

function VLine_to_LLine(vp: PVLine;
                        lp: PLLine; z: PIntegers;
                        Max: Integer): Integer;

function VLine_Lock(lp: PVLine): Boolean;

function LPoly_Lock(lp: PLPoly; lp_N: Integer): Boolean;
function LCurve_Lock(lp: PLPoly; lp_N: Integer): Boolean;

function PolyLock(lp: PLLine): Boolean;
function CurveLock(lp: PLLine): Boolean;

function Poly_Pack(lp: PLPoly; hp: PIntegers; lp_N: Integer): Boolean;
procedure Pack_Poly(lp: PLPoly; hp: PIntegers; lp_N: Integer);

function xPoly_Pack(lp: PLLine; hp: PIntegers): Boolean;
procedure xPack_Poly(lp: PLLine; hp: PIntegers);

procedure Unpack_Poly(dst,src: PLPoly; Count: Integer);

function PolyLength(lp: PLPoly; n: Integer): double; stdcall;

procedure Dump_Poly(lp: PLLine; fn: PChar); stdcall;

procedure Swap_LPoly(lp: PLPoly; hp: PIntegers; N: Integer);
procedure Swap_LCurve(lp: PLPoly; hp: PIntegers; N: Integer);

procedure Swap_LLine(lp: PLLine; hp: PIntegers; Curve: Boolean);

procedure xSwap_Poly(lp: PLPoly; N: Integer);

procedure Swap_VLine(lp: PVLine);

procedure Swap_Poly(lp: PLLine); stdcall;
procedure Swap_Curve(lp: PLLine); stdcall;

function Square_Polyi(lp: PLPoly; N: int): double;

function Square_Poly(lp: PLPoly; n: Integer): double; stdcall;
function Centre_Poly(lp: PLLine; var cx,cy: double): double; stdcall;

function xCentre_Polygon(lp: PLPoly; lp_N: Integer;
                         out cx,cy: double): double;

function Centre_Polygon(lp: PLLine; out cx,cy: double): double;

function Polygon_Centre(lp: PLLine): TPoint;
function Poly_Centre(lp: PLLine): TPoint;

function LPoly_Length(lp: PLLine): double;

procedure Move_LPoly(lp: PLPoly; lp_n,dx,dy: int);

procedure Link_Poly(lp: PLLine; x,y: longint); stdcall;
procedure Move_Poly(lp: PLLine; dx,dy: longint); stdcall;
procedure Scale_Poly(lp: PLLine; kx,ky: Double);

procedure zoom_poly(lp: PLPoly; n,iz: int);

procedure polygon_Scale(lp: PLLine;
                        const c: TPoint;
                        kx,ky: double);

procedure Max_Poly_Bound(lp: PLPoly; n: Integer; var a,b: TPoint); stdcall;
function Max_Poly_Rect(lp: PLPoly; n: Integer): TRect;

function Bound_to_LPoly(const lt,rb: TPoint; lp: PLPoly): Integer;
function Port_to_LPoly(x,y,w,h: Integer; lp: PLPoly): Integer;

function Romb_to_LPoly(x,y,r: Integer; lp: PLPoly): Integer;

function Radius_to_LPoly(x,y,r: Integer; lp: PLPoly): Integer;

function Copy_LPoly(src_lp,dst_lp: PLLine;
                    src_z,dst_z: PIntegers;
                    lp_max: Integer): Integer;

function Load_Poly(dst: PLLine; lp: PLPoly; Count: int): int;

function Copy_Poly(src,dst: PLLine; lp_max: Integer): Integer;

function Unlock_Curve(lp,buf_lp: PLLine;
                      i: Integer): Boolean;

function Unlock_LPoly(lp,buf_lp: PLLine;
                      z,buf_z: PIntegers;
                      i: Integer): Boolean;

procedure Smooth_Poly(lp: PLLine; sk: double);
procedure Widen_Poly(lp: PLLine; dl: double);

function dup_LTurn(lp: PLPoly; lp_N: int): int;

function dup_LPlan(lp: PLPoly; hp: PIntegers;
                   lp_N: Integer): Integer;

function dup_LPoly(lp: PLPoly; hp: PIntegers;
                   lp_N: Integer; is_dir: Boolean): Integer;

function peak_LPoly(lp: PLPoly; lp_N: Integer): Integer;

function cls_LPoly(lp: PLPoly; lp_N: Integer;
                   D,R: double): Integer;

function dist_Poly(lp: PLLine; cls: double): bool;
function cls_Poly(lp: PLLine; R: double): Integer;

function eps_LPoly(lp: PLPoly; n: int; eps: double): int;

function line_LPoly(lp: PLPoly; N: Integer; R: double): Integer;
function line_Poly(lp: PLLine; R: double): Integer;

procedure ort_Poly(lp: PLLine; R: double);
function dir_Poly(lp: PLLine; min: double): Boolean;
procedure rect_Poly(lp: PLLine);

function Backup_Polyline(lp: PLLine; k,r: Double;
                         out p: TPoint): Integer;

function Polyline_locate(lp: PLPoly; N: int;
                         const p: TPoint; dist: Double): int;

function Polyline_Contains(lp: PLPoly; lp_N: Integer;
                           const p: TPoint; max_r: Double;
                           out C: TPoint; out R: Double): Double;

function Polyline_Contains1(lp: PLLine;
                            const p: TPoint; max_r: Double;
                            out C: TPoint; out R: Double): Double;

function ProjectToPolyLine(lp: PLPoly; lp_N: Integer;
                           const p: TPoint; d1,d2: Double;
                           out Ind: Integer; out c: TPoint): Boolean;

function Project_to_PolyLine(lp: PLLine;
                             const p: TPoint; d1,d2: Double;
                             out Ind: Integer; out c: TPoint): Boolean;

function xProject_to_LPoly(lp: PLPoly; lp_N: Integer;
                           const p: TPoint; d1,d2: Double;
                           out Ind: Integer; out c: TPoint): Double;

function xProject_to_PolyLine(lp: PLLine; const p: TPoint;
                              out c: TPoint): Double;

function ippl(lp: PLLine; const p: TPoint;
              maxr: Double; out mk: TLineMarker1): Boolean;

function Plot_to_Polyline(lp: PLLine; const p: TPoint;
                          maxr: Double; out ic: Integer;
                          out c: TPoint): Boolean;

function Poly_Azimuth(lp: PLLine; ic: Integer): Double;
                          
function LPoly_Contains_pt(lp: PLPoly; lp_N: Integer;
                           const p: TPoint; out c: TPoint;
                           out dist: Double): Integer;

function lp_Contains_Point(lp: PLLine; const p: TPoint;
                           r: Double; out dist: Double;
                           out c: TPoint): Integer;

function Plot_to_Polygon(lp: PLLine; const p: TPoint;
                         r: Double; out ic: Integer;
                         out c: TPoint): Boolean;

function LocatePolyLine(lp: PLPoly; N: Integer;
                        const lt,rb: TPoint;
                        out c: TPoint): Integer;

procedure Set_rPolygon_eps(v: float);

function _rPolygonContainsTest(lp: PLPoly; lp_n: int): int;

function _rPolygonContainsPixel(lp: PLPoly; lp_n: Integer;
                                px,py: Double; mk: PLineMarker1): Integer;

function rPolygonContainsPixel(lp: PLPoly; lp_n: Integer;
                               px,py: Double): Integer;

function xPolygonContainsPixel(lp: PLPoly; lp_n: Integer;
                               px,py: Double): Integer;

function PolygonContainsPixel(lp: PLPoly; lp_n: Integer;
                              px,py: Double): Integer;

function PolygonContainsPoint(lp: PLPoly; lp_n: Integer;
                              const p: TPoint): Integer;

function xPolygonContainsPoint(lp: PLLine;
                               const p: TPoint): Integer;

function xPolygonContainsContour(lp: PLPoly; lp_N: Integer;
                                 cp: PLPoly; Count: Integer): Boolean;

function xLPoly_Locate(lp: PLPoly; lp_n: Integer;
                       const lt,rb: TPoint; scan: Boolean;
                       out ind: Integer; out lp_p: TPoint): Boolean;

function xLLine_Locate(lp: PLLine;
                       const lt,rb: TPoint; scan: Boolean;
                       out ind: Integer; out lp_p: TPoint): Boolean;

function LPoly_Locate(lp: PLLine; const lt,rb: TPoint): Integer;

function LPoly_Indexof(lp: PLPoly; lp_n: Integer;
                       const p: TPoint): Integer;

function xPortContainsPolyLine(const lt,rb: TPoint;
                               lp: PLPoly; lp_N: Integer): Integer;

function PortContainsPolyLine(const lt,rb: TPoint; lp: PLLine): Integer;

function RingContainsPolyLine(c: TPoint; r: Double; lp: PLLine): Integer;

function PolyLineContainsPort(lp: PLLine; const lt,rb: TPoint): Boolean;

function IsInnerPoly(lp1: PLPoly; n1: int;
                     lp2: PLPoly; n2: int): bool;

function PolygonContainsPoly(lp1: PLPoly; n1: Integer;
                             lp2: PLPoly; n2: Integer): Integer;

function PolygonContainsPoints(lp1: PLLine;
                               lp2: PLPoly; n2: Integer): Integer;

function PolygonContainsPolyLine(lp1,lp2: PLLine): Integer;

function lPolygonContainsPolyLine(lp1: PLPoly; n1: Integer;
                                  lp2: PLPoly; n2: Integer): Boolean;

function xPolygonContainsPolyLine(lp1,lp2: PLLine): Boolean;

function PolygonContainsPolygon(lp1: PLPoly; n1: int;
                                lp2: PLPoly; n2: int;
                                bound: int): bool;

function xPolygonContainsPolygon(lp1,lp2: PLLine; bound: int): bool;

function PolygonClipPolygon(lp1: PLPoly; n1: int;
                            lp2: PLPoly; n2: int): bool;

function IsHolePolygon(lp1: PLPoly; n1: Integer;
                       lp2: PLPoly; n2: Integer): Boolean;

function LPoly_Insert(lp: PLLine;
                      lp_z: PIntegers;
                      ind,lp_max: Integer;
                      const v: VPoint): Integer;

function zPoly_Insert(lp: PLLine;
                      lp_z: PIntegers;
                      ind,lp_max: Integer;
                      const p: TPoint;
                      z: Integer): Integer;

function Poly_Insert(lp: PLLine;
                     ind,lp_max: Integer;
                     const p: TPoint): Integer;

function xLPoly_Insert(lp: PLLine;
                       lp_z: PIntegers;
                       ind,lp_max: Integer;
                       const v: Vpoint): Integer;

function xPoly_Insert(lp: PLLine;
                       ind,lp_max: Integer;
                       const p: TPoint): Integer;

procedure LPoly_push(lp: PLLine; lp_Max: int; const p: TPoint);

function LPoly_Continue(lp: PLLine; lp_Max: Integer;
                        const p: TPoint): Integer;

function xLPoly_Continue(lp: PLLine; lp_Max: Integer;
                         const p: TPoint): Integer;
                        
function LPoly_Next(lp: PLLine; lp_Max: Integer;
                    const p: TPoint): Integer;

function VPoly_Next(lp: PVLine; lp_Max: Integer;
                    x,y,z: Integer): Integer;

function LPoly_edge(lp: PLLine;
                    hp: PIntegers): Integer;
                    
function LPoly_Delete(lp: PLLine;
                      lp_z: PIntegers;
                      Ind: int): int;

function Poly_Delete(lp: PLLine; ind: Integer): Integer;

function Poly_Cut(lp: PLLine; Ind,Count: Integer): Integer;

function cut_lpoly(lp: PLPoly; hp: PIntegers;
                   lp_n,Ind,Count: int): int;

function LPoly_Cut(lp: PLLine; hp: PIntegers;
                   Ind,Count: Integer): Integer;

function Poly_Paste(dst,lp: PLLine;
                    Ind,Count: Integer;
                    dst_Max: Integer): Boolean;

function Poly_Add(dst,lp: PLLine;
                  dst_Max: Integer): Boolean;

function LPoly_Append(dst_lp,src_lp: PLLine;
                      dst_z,src_z: PIntegers;
                      lp_max: Integer; eps: Double): bool;

function Poly_Append(dst,src: PLLine; max: Integer;
                     dist: Double): bool;

function LPoly_cat(dst: PLLine; dst_Max: Integer;
                   src: PLPoly; Count: Integer;
                   dst_hp,src_hp: PIntegers): Boolean;

procedure LPoly_add(dst,src: PLLine);

function lp_trunc_top(lp: PLLine; hp: PIntegers;
                      Ind: int; const v: VPoint): bool;

function lp_trunc_bot(lp: PLLine; hp: PIntegers;
                      Ind: int; const v: VPoint): bool;

function lp_unlock(lp: PLLine; hp: PIntegers;
                   buf: PLPoly; buf_hp: PIntegers;
                   Ind: int; const v: VPoint): bool;

function lp_copy(src: PLLine; hp1: PIntegers;
                 dst: PLLine; hp2: PIntegers;
                 i1,i2: int; const v1,v2: VPoint): bool;

function xPoly_Append(dst,src: PLLine; ind,max: Integer): Integer;

function Poly_Push(lp: PLLine; lp_max: Integer;
                   const p: TPoint; dist,cls: double): Integer;

function LPoly_Road(lp: PLPoly; lp_N: int;
                    lr: PDoubles; R1,R2: double;
                    Tube: int; lp1,lp2: PLLine): Integer;

function LPoly_Tube(lp: PLPoly; lp_N: Integer;
                    lr: PDoubles; R: double; Tube: Integer;
                    dst: PLLine; dst_Max: Integer): Integer;

function LLine_Tube(lp: PLLine; R: Double; Tube: Integer;
                    dst: PLLine; dst_Max: Integer): Integer; stdcall;

function ClipLine(clip: PLLine; const a,b: TPoint;
                  out c,d: TPoint): Boolean;

function xClipLine(clip,lp: PLLine): Boolean;

//   [i]  [k] 
function LPoly_ldel(lp: PLLine;
                    hp: PIntegers;
                    i,k: int): bool;

function Poly_ldel(lp: PLLine; i,k: int): bool;

procedure Rotate_Poly(lp: PLLine; const a,b,c: TPoint);
procedure Azimuth_Poly(lp: PLLine; const c: TPoint; fi: Double);
procedure fi_Rotate_Poly(lp: PLLine; const c: TPoint; fi: Double);
procedure Mirror_Poly(lp: PLLine; const a,b: TPoint);

function Lock_enabled(lp: PLLine): Boolean;

function Lock_Poly(lp: PLLine; lp_Max: Integer): Boolean;
function Lock_Curve(lp: PLLine; lp_Max: Integer): Boolean;

function Cross_PolyLines1(lp1: PLPoly; n1: Integer;
                          lp2: PLPoly; n2: Integer; lp_pc: PPoint;
                          out mk1,mk2: TLineMarker): Boolean;

function Cross_PolyLines(lp1,lp2: PLLine; lp_pc: PPoint;
                         out mk1,mk2: TLineMarker): Boolean;

function CircleToLine(lp: PLPoly; cx,cy,r,eps: Double): Integer;
                         
function Ellipse_Bound(lp: PLLine; out R: LOrient): Boolean;

function Ellipse_LLine(lp: PLLine; lp_Max: Integer; eps: Double): Integer;

function xEllipse_LLine(const p: TPoint; r: Integer;
                        lp: PLLine; lp_Max: Integer;
                        eps: Double): Boolean;

function Ellipse_Curve(lp,curve: PLLine): Integer;

function Sector_LLine(lp: PLLine;
                      const p1,p2: TPoint;
                      df: Double): Integer;

procedure move_Ellipse(lp: PLLine; i: Integer; const p: TPoint);

function LPoly_Sector(lp: PLPoly;
                      xc,yc: Integer;
                      r,f1,f2: Double): Integer;

function LPoly_Sectorc(lp: PLPoly;
                       xc,yc: Integer;
                       r,f1,f2: Double): Integer;
                      
function LPoly_Sectorp(lp: PLPoly; lp_n: int): int;

function LPoly_Triangle(const a,b: TPoint; lp: PLLine): Integer;

function Poly_dist_Vector(lp: PLPoly; n: Integer;
                          dist,loc: double; dir: Boolean;
                          out v: lvector): Integer;

function Get_Poly_turns(lp: PLPoly; n: Integer;
                        turns: PFloats; rot: double): Integer;

function Off_short_ribs(lp: PLPoly; n: int; len: double): int;

function Poly_Vertex(lp: PLLine; const p: TPoint; out v: TPoint): int;

function Edge_Pull(lp: PLPoly; dst: PLLine; r: Double): Boolean;

function Poly_Pull(lp: PLLine; lp_Max: Integer;
                   dst: PLLine; r: Double): Boolean;

function Poly_Bound_Dist(lp1,lp2: PLLine): Double;

function Project_to_Road(const p: TPoint;
                         lp: PLPoly; N: Integer; R: Double;
                         out q: TPoint; out Dist: Double): Integer;

function Poly_Bilding(lp: PLLine): Boolean;

function Sign_Polygon(lp: PLPoly; lp_N: int;
                      out pc: TPoint): Boolean;

function xSign_Polygon(lp: PLLine; out pc: TPoint): Boolean;

procedure LPoly_Restore_z_axe(lp: PLLine; hp: PIntegers;
                              var V: VPoint; Ind: Integer);

function LPoly_Nearest_Vertex(lp: PLPoly; lp_N: Integer;
                              const p: TPoint): Integer;

function Min_Poly_Bound(lp: PLPoly; N: Integer;
                        out lt,rb: TPoint): Boolean;

function Get_Poly_Diam(lp: PLPoly; lp_N: Integer;
                       out cp1,cp2: TPoint): Integer;

function Get_Poly_Bound(lp: PLPoly; lp_N: Integer;
                        Bound: PLPoly; MinR: Integer): Integer;

function poly_quad(lp: PLPoly; lp_n: Integer): Boolean;

function Is_poly_image(lp: PLLine; W,H: Integer): Boolean;

implementation {$R-}

uses
  Sysutils;

function LineMarker(Ind: Integer; const pc: TPoint): TLineMarker;
var
  r: TLineMarker;
begin
  r.Ind:=Ind; r.Res:=0; r.pc:=pc;
  Result:=r
end;

function LineMarker1(Ind: Integer; const pc: TPoint;
                     dist: Double): TLineMarker1;
begin
  Result.Ind:=Ind;
  Result.pc:=pc;
  Result.dist:=dist
end;

function LineMarker1Of(mk: PLineMarkers1; n: Integer;
                       const p: TPoint): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to n-1 do
  if Points_equal(mk[i].pc,p) then begin
    Result:=i; Break
  end
end;

function DistMarker1(lp: PLPoly; lp_n,Ind: int;
                     const pc: TPoint): TLineMarker1;
var
  i,n: int; m: TLineMarker1; p1,p2: TPoint;
begin
  m:=LineMarker1(Ind,pc,0);

  n:=Abs(Ind);
  if n > 0 then
  if lp_n > 0 then begin
    if Ind < 0 then Dec(n);
    if n > lp_n then n:=lp_n;

    p2:=lp[0];
    for i:=1 to n do begin
      p1:=p2; p2:=lp[i];
      m.dist:=m.dist + Long_Dist(p1,p2);
    end;

    if Ind < 0 then
    if Abs(Ind) = n+1 then
    m.dist:=m.dist + Long_Dist(p2,pc);
  end;

  Result:=m
end;

function top_LineMarker(lp: PLLine; const mk: TLineMarker): Boolean;
begin
  Result:=false; with lp^ do
  if (N > 0) and (mk.Ind = 1) then
  Result:=Points_Equal(Pol[0],mk.pc)
end;

function bot_LineMarker(lp: PLLine; const mk: TLineMarker): Boolean;
begin
  Result:=false; with lp^ do
  if (N > 0) and (mk.Ind = N) then
  Result:=Points_Equal(Pol[N],mk.pc)
end;

function edge_LineMarker(lp: PLLine; const mk: TLineMarker): Boolean;
begin
  Result:=top_LineMarker(lp,mk) or bot_LineMarker(lp,mk)
end;

function zaxe_LineMarker(lp: PLLine; hp: PIntegers;
                         const mk: TLineMarker): VPoint;
var
  Ind: Integer;
begin
  Result.x:=mk.pc.x;
  Result.y:=mk.pc.y;
  Result.z:=0;

  Ind:=mk.Ind;

  if Assigned(hp) then with lp^ do
  if (Ind > 0) and (Ind <= N) then

  Result.z:=Round(z_axe_line(Pol[Ind-1],Pol[Ind],
                  mk.pc,hp[Ind-1],hp[Ind]))
end;

function PolyEqual(lp1: PLPoly; n1: int;
                   lp2: PLPoly; n2: int): bool;
var
  i: int;
begin
  Result:=false;

  if n1 >= 0 then
  if n1 = n2 then begin
    Result:=true;

    for i:=0 to n1 do
    if not Points_Equal(lp1[i],lp2[i]) then
    begin Result:=false; Break end
  end
end;

function Backup_lp(lp: PLLine; hp: PIntegers;
                   lp_i: Integer): lxyz;
var
  v: lxyz;
begin
  v.p:=lp.Pol[lp_i]; v.v.z:=0;
  if Assigned(hp) then v.v.z:=hp[lp_i];
  Result:=v
end;

procedure Restore_lp(lp: PLLine; hp: PIntegers;
                     lp_i: Integer; const v: lxyz);
begin
  lp.Pol[lp_i]:=v.p;
  if Assigned(hp) then hp[lp_i]:=v.v.z
end;

function Poly_cpos(lp: PLLine;
                   lp_ind: Integer;
                   lp_rmu: Double;
                   out p: TPoint): Boolean;
var
  Ind: int; q: TPoint;
begin
  Result:=false;

  Ind:=Abs(lp_ind);
  if Ind >= 0 then
  if Ind <= lp.N then begin
    p:=lp.Pol[Ind];

    if lp_ind < 0 then begin
      q:=lp.Pol[Ind-1];
      p:=inter_2i(q,p,lp_rmu);
    end;

    Result:=true
  end
end;

function Poly_range(lp: PLLine;
                    i1: int; k1: float;
                    i2: int; k2: float): int;
var
  j,j1,j2,k: int; swap: bool; a,b,p: TPoint;
begin
  Result:=-1;

  j1:=Abs(i1);
  j2:=Abs(i2); swap:=false;

  if j2 < j1 then swap:=true else
  if (j1 = j2) and (i2 < 0) then
  swap:=true;

  if swap then begin
    iSwap(i1,i2);
    iSwap(j1,j2);
    fSwap(k1,k2)
  end;

  if Poly_cpos(lp,i1,k1,a) then
  if Poly_cpos(lp,i2,k2,b) then begin

    if i2 < 0 then Dec(j2);

    k:=0; lp.Pol[0]:=a;
    for j:=j1 to j2 do begin
      p:=lp.Pol[j];

      if not Points_Equal(a,p) then begin
        Inc(k); lp.Pol[k]:=p
      end;

      a:=p
    end;

    if not Points_Equal(a,b) then begin
      Inc(k); lp.Pol[k]:=b
    end;

    lp.N:=k; Result:=k 
  end
end;

function Poly_cls(lp: PLLine): Integer;
begin
  if Assigned(lp) then lp.N:=-1;
  Result:=-1
end;

function Poly_Empty(lp: PLLine): Boolean;
begin
  Result:=true;
  if Assigned(lp) then
  Result:=lp.N < 0
end;

function LPoly_Last(lp: PLLine): TPoint;
begin
  Result.x:=0; Result.y:=0;
  with lp^ do if N >= 0 then
  Result:=Pol[N]
end;

function LLine_IndexOf(lp: PLLine; const P: TPoint;
                       is_back: Boolean): Integer;
var
  i: Integer;
begin
  Result:=-1;

  if is_back then begin
    for i:=lp.N downto 0 do
    if Points_Equal(lp.Pol[i],P) then
    begin Result:=i; Break end
  end
  else begin
    for i:=0 to lp.N do
    if Points_Equal(lp.Pol[i],P) then
    begin Result:=i; Break end
  end
end;

function LLine_RibOf(lp: PLLine; vp: PLPoly): Integer;
var
  i: int; a,b, p1,p2: TPoint;
begin
  Result:=-1; a:=vp[0]; b:=vp[1];

  p2:=lp.Pol[0];
  for i:=1 to lp.N do begin
    p1:=p2; p2:=lp.Pol[i];
    if (Points_Equal(p1,a) and Points_Equal(p2,b))
    or (Points_Equal(p1,b) and Points_Equal(p2,a))
    then begin Result:=i; Break end
  end
end;

function Copy_PolyBuf(lp: PLLine; dn: Integer): PLLine;
begin
  Result:=nil;

  if lp <> nil then
  if lp.N >= 0 then begin
    Result:=Alloc_LLine(lp.N+1+dn);
    if Result <> nil then with lp^ do begin
      Move(Pol,Result.Pol,(N+1)*SizeOf(TPoint));
      Result.N:=N
    end
  end
end;

function LLine_to_VLine(lp: PLLine; hp: PIntegers;
                        vp: PVLine; Max: Integer): Integer;
var
  i: Integer; v: vpoint;
begin
  Result:=-1; vp.N:=-1;

  if lp.N >= 0 then
  if lp.N <= Max then begin

    for i:=0 to lp.N do begin
      v.x:=lp.Pol[i].x; v.y:=lp.Pol[i].y;
      v.z:=0; if Assigned(hp) then v.z:=hp[i];
      Inc(Result); vp.Pol[Result]:=v
    end;

    vp.N:=Result
  end
end;

function VLine_to_LLine(vp: PVLine;
                        lp: PLLine; z: PIntegers;
                        Max: Integer): Integer;
var
  i: Integer; v: vpoint;
begin
  Result:=-1; lp.N:=-1;

  if vp.N >= 0 then
  if vp.N <= Max then begin

    for i:=0 to vp.N do begin
      v:=vp.Pol[i]; Inc(Result);

      lp.Pol[Result].x:=v.x;
      lp.Pol[Result].y:=v.y;

      if Assigned(z) then
      z[Result]:=v.z
    end;

    lp.N:=Result
  end
end;

function VLine_Lock(lp: PVLine): Boolean;
begin
  Result:=false; with lp^ do if N > 2 then
  if VPoints_Equal(Pol[0],Pol[N]) then
  Result:=true
end;

function LPoly_Lock(lp: PLPoly; lp_N: Integer): Boolean;
begin
  Result:=false; if lp_N > 2 then
  if Points_Equal(lp[0],lp[lp_N]) then
  Result:=true
end;

function LCurve_Lock(lp: PLPoly; lp_N: Integer): Boolean;
begin
  Result:=false; if lp_N >= 5 then
  if Points_Equal(lp[0],lp[lp_N-1]) then
  if Points_Equal(lp[1],lp[lp_N]) then
  Result:=true
end;

function PolyLock(lp: PLLine): Boolean;
begin
  Result:=false; with lp^ do if N > 2 then
  Result:=CompareMem(@Pol[0],@Pol[N],Sizeof(TPoint))
end;

function CurveLock(lp: PLLine): Boolean;
begin
  Result:=false; with lp^ do if N >= 5 then
  if Points_Equal(Pol[0],Pol[N-1]) then
  if Points_Equal(Pol[1],Pol[N]) then
  Result:=true
end;

function Poly_Pack(lp: PLPoly; hp: PIntegers; lp_N: Integer): Boolean;
var
  i,h1,h2: Integer; p1,p2: TPoint;
begin
  Result:=false;

  if lp_N > 0 then begin
    p2:=lp[0]; h2:=0;
    if Assigned(hp) then h2:=hp[0];

    for i:=1 to lp_N do begin
      h1:=h2; if Assigned(hp) then h2:=hp[i];
      p1:=p2; p2:=lp[i]; Result:=true;

      if (Abs(h2-h1) >= $7FFF)
      or (Abs(p2.X-p1.X) >= $7FFF)
      or (Abs(p2.Y-p1.Y) >= $7FFF) then
      begin Result:=false; Break end
    end
  end
end;

procedure Pack_Poly(lp: PLPoly; hp: PIntegers; lp_N: Integer);
var
  i,h1,h2: Integer; p1,p2: TPoint;
  dst_lp: PIPoly; dst_hp: PSmallInts;
begin
  dst_lp:=@lp[1];
  p1:=lp[0]; p2:=lp[1];
  dst_lp[0].x:=0; dst_lp[0].y:=0;

  if Assigned(hp) then begin
    dst_hp:=@hp[1];
    h1:=hp[0]; h2:=hp[1];
    dst_hp[0]:=0;
  end;

  for i:=1 to lp_N do begin
    if i > 1 then begin
      p1:=p2; p2:=lp[i];
    end;

    dst_lp:=@dst_lp[1];
    dst_lp[0].x:=p2.x-p1.x;
    dst_lp[0].y:=p2.y-p1.y;

    if Assigned(hp) then begin
      if i > 1 then begin
        h1:=h2; h2:=hp[i];
      end;

      dst_hp:=@dst_hp[1];
      dst_hp[0]:=h2-h1
    end
  end
end;

function xPoly_Pack(lp: PLLine; hp: PIntegers): Boolean;
begin
  Result:=Poly_Pack(@lp.Pol,hp,lp.N)
end;

procedure xPack_Poly(lp: PLLine; hp: PIntegers);
begin
  Pack_Poly(@lp.Pol,hp,lp.N)
end;

procedure Unpack_Poly(dst,src: PLPoly; Count: Integer);
var
  i: Integer; p: TPoint;
  si: PIPoly; di: PLPoly;
begin
  p:=src[0]; si:=@src[1]; di:=dst;

  for i:=1 to Count do begin
    Inc(p.x,si[0].x); Inc(p.y,si[0].y);
    di[0]:=p; di:=@di[1]; si:=@si[1]
  end
end;

const
  dll_poly = '_poly';

function PolyLength(lp: PLPoly; n: Integer): double;
stdcall; external dll_poly;

procedure Dump_Poly(lp: PLLine; fn: pChar);
stdcall; external dll_poly;

procedure Swap_LPoly(lp: PLPoly; hp: PIntegers; N: Integer);
var
  i1,i2: Integer; si,di: PLPoly; p: TPoint;
begin
  i1:=0; i2:=N; si:=lp;

  while i1 < i2 do begin
    di:=@lp[i2]; p:=si[0];
    si[0]:=di[0]; di[0]:=p;

    if Assigned(hp) then iSwap(hp[i1],hp[i2]);
    Inc(i1); Dec(i2); si:=@si[1]
  end
end;

procedure Swap_LCurve(lp: PLPoly; hp: PIntegers; N: Integer);
var
  i: Integer; p1,p2: lxyz;
  _lp: PLPoly; _hp: PIntegers;
begin
  _lp:=lp; _hp:=hp;

  i:=0; while i < N do begin
    p1.p:=_lp[0]; p2.p:=_lp[1];
    if Assigned(_hp) then begin
      p1.v.z:=_hp[0]; p2.v.z:=_hp[1];
    end;

    with p1.v do begin
      p2.v.x:=x + x - p2.v.x;
      p2.v.y:=y + y - p2.v.y;
      p2.v.z:=z + z - p2.v.z;
    end;

    _lp[0]:=p2.p; _lp[1]:=p1.p; _lp:=@_lp[2];

    if Assigned(_hp) then begin
      _hp[0]:=p2.v.z; _hp[1]:=p1.v.z; _hp:=@_hp[2];
    end;

    Inc(i,2);
  end;

  Swap_LPoly(lp,hp,N)
end;

procedure Swap_LLine(lp: PLLine; hp: PIntegers; Curve: Boolean);
begin
  if curve then
    Swap_LCurve(@lp.Pol,hp,lp.N)
  else
    Swap_LPoly(@lp.Pol,hp,lp.N);
end;

procedure xSwap_Poly(lp: PLPoly; N: Integer);
begin
  Swap_LPoly(lp,nil,N)
end;

procedure Swap_VLine(lp: PVLine);
var
  i1,i2: Integer; v: vpoint;
begin
  i1:=0; i2:=lp.N;

  with lp^ do while i1 < i2 do begin
    v:=Pol[i1]; Pol[i1]:=Pol[i2]; Pol[i2]:=v;
    Inc(i1); Dec(i2)
  end
end;

procedure Swap_Poly(lp: PLLine);
stdcall; external dll_poly;

procedure Swap_Curve(lp: PLLine);
stdcall; external dll_poly;

function Square_Polyi(lp: PLPoly; N: int): double;
var
  i,x1,y1,x2,y2: int; ax: Int64;
begin
  Result:=0; if N > 0 then begin

    ax:=0; x2:=lp[0].x; y2:=lp[0].y;

    for i:=1 to N do begin
      x1:=x2; y1:=y2; x2:=lp[i].X; y2:=lp[i].Y;
      Inc(ax,x1*y2 - x2*y1)
    end;

    Result:=ax/2
  end;
end;

function Square_Poly(lp: PLPoly; n: Integer): double;
stdcall; external dll_poly;

function Centre_Poly(lp: PLLine; var cx,cy: double): double;
stdcall; external dll_poly;

function xCentre_Polygon(lp: PLPoly; lp_N: Integer;
                         out cx,cy: double): double;
var
  i,k: Integer; ax,ay: Extended;
begin
  Result:=Abs(Square_Poly(lp,lp_N));

  ax:=0; ay:=0; k:=lp_N+1;
  if LPoly_Lock(lp,lp_N) then Dec(k);

  for i:=1 to k do begin
    ax:=ax + lp[0].X/k;
    ay:=ay + lp[0].Y/k;
    lp:=@lp[1]
  end;

  cx:=ax; cy:=ay;
end;

function Centre_Polygon(lp: PLLine; out cx,cy: double): double;
begin
  Result:=xCentre_Polygon(@lp.Pol,lp.N, cx,cy)
end;

function Polygon_Centre(lp: PLLine): TPoint;
var
  cx,cy: Double;
begin
  Centre_Polygon(lp, cx,cy);
  Result.x:=Round(cx);
  Result.y:=Round(cy);
end;

function Poly_Centre(lp: PLLine): TPoint;
var
  a,b: TPoint;
begin
  Max_Poly_Bound(@lp.Pol,lp.N+1,a,b);
  Middle_Point(a,b,Result)
end;

function LPoly_Length(lp: PLLine): double;
begin
  with lp^ do
  Result:=PolyLength(@Pol,N)
end;

procedure Move_LPoly(lp: PLPoly; lp_n,dx,dy: int);
var
  i: int;
begin
  for i:=0 to lp_n do begin
    Inc(lp[0].X,dx); Inc(lp[0].Y,dy);
    lp:=@lp[1]
  end
end;

procedure Link_Poly(lp: PLLine; x,y: longint);
stdcall; external dll_poly;

procedure Move_Poly(lp: PLLine; dx,dy: longint);
stdcall; external dll_poly;

procedure Get_Poly_Min_Max(lp: PLLine; var min,max: double);
stdcall; external dll_poly;

function Get_Poly_Max_Rib(lp: PLLine): Integer;
stdcall; external dll_poly;

procedure Max_Poly_Bound(lp: PLPoly; n: Integer; var a,b: TPoint);
stdcall; external dll_poly;

procedure Scale_Poly(lp: PLLine; kx,ky: Double);
var
  i: Integer;
begin
  for i:=0 to lp.N do
  with lp.Pol[i] do begin
    X:=Round(X*kx); Y:=Round(Y*ky);
  end
end;

procedure zoom_poly(lp: PLPoly; n,iz: int);
var
  i: int;
begin
  for i:=0 to n do
  with lp[i] do begin
    X:=X div iz; Y:=Y div iz
  end
end;

procedure polygon_Scale(lp: PLLine;
                        const c: TPoint;
                        kx,ky: double);
var
  i: Integer;
begin
  with lp^ do
  for i:=0 to N do with Pol[i] do begin
    x:=c.X + Round((X - c.X)*kx);
    y:=c.Y + Round((Y - c.Y)*ky);
  end;
end;

function Max_Poly_Rect(lp: PLPoly; n: Integer): TRect;
var
  lt,rb: TPoint;
begin
  Max_Poly_Bound(lp,n, lt,rb);
  Result:=Rect(lt.x,lt.y,rb.x,rb.y)
end;

function Bound_to_LPoly(const lt,rb: TPoint; lp: PLPoly): Integer;
begin
  lp[0].X:=lt.X; lp[0].Y:=lt.Y;
  lp[1].X:=rb.X; lp[1].Y:=lt.Y;
  lp[2].X:=rb.X; lp[2].Y:=rb.Y;
  lp[3].X:=lt.X; lp[3].Y:=rb.Y;
  lp[4]:=lp[0]; Result:=4
end;

function Port_to_LPoly(x,y,w,h: Integer; lp: PLPoly): Integer;
var
  lt,rb: TPoint;
begin
  lt.X:=x; lt.Y:=y; rb.X:=x+w-1; rb.Y:=y+h-1;
  Result:=Bound_to_LPoly(lt,rb, lp);
end;

function Romb_to_LPoly(x,y,r: Integer; lp: PLPoly): Integer;
begin
  lp[0]:=Point(x-r,y);
  lp[1]:=Point(x,y-r);
  lp[2]:=Point(x+r,y);
  lp[3]:=Point(x,y+r);
  lp[4]:=lp[0];
  Result:=4
end;

function Radius_to_LPoly(x,y,r: Integer; lp: PLPoly): Integer;
var
  ix,iy,r1: Integer;
begin
  Result:=-1; r1:=r-1;

  if r <= 0 then begin
    lp[0]:=Point(x,y); Result:=0
  end
  else begin
    for ix:=x-r to x+r do begin
      lp[0]:=Point(ix,y-r); lp:=@lp[1];
    end;

    for iy:=y-r1 to y+r1 do begin
      lp[0]:=Point(x+r,iy); lp:=@lp[1];
    end;

    for ix:=x+r downto x-r do begin
      lp[0]:=Point(ix,y+r); lp:=@lp[1];
    end;

    for iy:=y+r1 downto y-r1 do begin
      lp[0]:=Point(x-r,iy); lp:=@lp[1];
    end;

    Result:=r*8-1
  end
end;

function Copy_LPoly(src_lp,dst_lp: PLLine;
                    src_z,dst_z: PIntegers;
                    lp_max: Integer): Integer;
var
  n: Integer;
begin
  Result:=src_lp.N; if lp_max > 0 then
  Result:=Min(Result,lp_Max); dst_lp.N:=Result;

  n:=Result+1;
  Move(src_lp.Pol,dst_lp.Pol,n * SizeOf(TPoint));
  if Assigned(src_z) and Assigned(dst_z) then
  Move(src_z^,dst_z^,n * SizeOf(Integer))
end;

function Load_Poly(dst: PLLine; lp: PLPoly; Count: int): int;
begin
  Result:=-1; dst.N:=-1;

  if Count > 0 then begin
    Move(lp[0],dst.Pol,Count*Sizeof(TPoint));
    dst.N:=Count-1
  end;

  Result:=dst.N
end;

function Copy_Poly(src,dst: PLLine; lp_max: Integer): Integer;
begin
  Result:=Copy_LPoly(src,dst, nil,nil, lp_max)
end;

function Unlock_LPoly(lp,buf_lp: PLLine;
                      z,buf_z: PIntegers;
                      i: Integer): Boolean;
var
  j: Integer; tmp: PLLine;
begin
  Result:=false;

  if PolyLock(lp) then

  if i = 0 then begin
    Dec(lp.N); Result:=true
  end
  else begin
    tmp:=nil; if buf_lp = nil then begin
      tmp:=Alloc_LLine(lp.N+lp.N+1);
      buf_lp:=tmp; buf_z:=@tmp.Pol[lp.N+1]
    end;

    if Assigned(buf_lp) then begin

      if buf_z = nil then z:=nil;
      Copy_LPoly(lp,buf_lp, z,buf_z, lp.N);

      with lp^ do begin

        Dec(N); for j:=0 to N do begin
          if i > N then i:=0;
          Pol[j]:=buf_lp.Pol[i];

          if Assigned(z) then
          z[i]:=buf_z[i];

          Inc(i);
        end;

        Result:=true
      end
    end;

    xFreePtr(tmp)
  end
end;

function Unlock_Curve(lp,buf_lp: PLLine;
                      i: Integer): Boolean;
var
  j: Integer; tmp: PLLine;
begin
  Result:=false; if not Odd(i) then
  if CurveLock(lp) then begin

    if i = 0 then begin
      Dec(lp.N,2); Result:=true
    end
    else begin tmp:=nil;

      if buf_lp = nil then begin
        tmp:=Copy_PolyBuf(lp,0);
        buf_lp:=tmp
      end
      else Copy_Poly(lp,buf_lp,lp.N);

      if buf_lp <> nil then with lp^ do begin

        Dec(N,2); for j:=0 to N do begin
          Pol[j]:=buf_lp.Pol[i]; Inc(i);
          if i > N then i:=0
        end;

        xFreePtr(tmp); Result:=true
      end
    end
  end
end;

function Edge_Vertex(c: TPoint; lp: PLPoly; N: int): int;
var
  i,j: Integer; l,t: double;
begin
  j:=0; l:=0;
  for i:=0 to N do begin
    t:=Long_Dist(c,lp[i]);
    if t > l then begin j:=i; l:=t end
  end; Result:=j
end;

function Near_Vertex(c: TPoint; lp: PLPoly; n: Integer): Integer;
var
  i: Integer; l,t: Double;
begin
  Result:=0; l:=0;

  for i:=0 to n do begin
    t:=Long_Dist(c,lp[i]);
    if i = 0 then l:=t else
    if t < l then begin
      Result:=i; l:=t
    end
  end
end;

procedure Order_Poly(lp: PLLine; Ind: Integer);
begin
  with lp^ do begin
    Unlock_LPoly(@Pol,nil,nil,nil,Ind);
    Inc(N); Pol[N]:=Pol[0]
  end
end;

procedure Order_LPoly(lp: PLPoly; N,I: Integer);
var
  bp,tmp: PLPoly; k,l: Integer; buf: TLPoly;
begin
  if I > 0 then begin
    l:=I*Sizeof(TPoint);

    bp:=nil; tmp:=@buf;
    if l > Sizeof(buf) then begin
      bp:=xAllocPtr(l); tmp:=bp
    end;

    if Assigned(tmp) then begin
      Move(lp[0],tmp^,l); k:=N+1-I;
      Move(lp[I],lp[0],k*Sizeof(TPoint));
      Move(tmp^,lp[k],l);
    end;

    if Assigned(bp) then
    xFreePtr(bp);
  end
end;

procedure Smooth_Poly(lp: PLLine; sk: double);
var
  i: Integer; p1,p2,p3, t, t1,t2: TPoint;
  l1,l2,l3, min,max: double;
begin
  with lp^ do if N > 7 then begin
    Get_Poly_Min_Max(lp,min,max);
    if sk > 0.5 then sk:=0.5;

    for i:=0 to N do begin
      p1:=p2; p2:=p3; p3:=Pol[i]; if i >= 2 then begin
        l1:=Long_Dist(p1,p2); Get_Next_Point(p1,p2,l1*sk,t1);
        l2:=Long_Dist(p2,p3); Get_Next_Point(p2,p3,l2*sk,t2);
        l3:=l1+l2; if l3 > 0 then begin
          Get_Next_Point(t1,t2,Long_Dist(t1,t2)*l1/l3,t);
          if Long_Dist(p2,t) > min then Get_Next_Point(p2,t,min,t);
          Pol[i-1]:=t
        end
      end
    end
  end
end;

procedure Widen_Poly(lp: PLLine; dl: double);
var
  i: Integer; a,b,c,p, t1,t2,t3,t4: TPoint;
begin
  if PolyLock(lp) then with lp^ do begin

    b:=Pol[N-1]; c:=Pol[0];

    for i:=1 to N do begin
      a:=b; b:=c; c:=Pol[i];

      Get_Two_P_Points(a,b,dl,t1,t2);
      Get_Two_P_Points(b,c,dl,t3,t4);

      if ILL2(t1,t2, t3,t4,p) then
      Pol[i-1]:=p
    end;

    Pol[N]:=Pol[0]
  end
end;

function Norm_Poly(lp: PLLine; var dir: tgauss): Boolean;
var
  i: Integer; p,q: TPoint;
  nx,ny,dx,dy,tmp,len, f,v,t: double;
begin
  Result:=false; with lp^ do

  if not Odd(N) and (N >= 4) then begin
    len:=PolyLength(@Pol,N);
    p:=Pol[0]; nx:=0; ny:=0; v:=0;

    for i:=1 to N do begin
      q:=Pol[i]; f:=iArcTan(p.x,p.y,q.x,q.y);
      if f < 0 then f:=f+2*Pi; if i > 1 then begin
        t:=Abs(f-v); while t > Pi do t:=t-Pi;
        if Abs(t-Pi/2) > Pi/4 then Exit
      end;

      v:=f; dx:=q.x-p.x; dy:=q.y-p.y; p:=q;
      tmp:=Hypot(dx,dy)/len; dx:=dx*tmp; dy:=dy*tmp;
      if Odd(i) then begin tmp:=dx; dx:=-dy; dy:=tmp end;
      tmp:=Hypot(nx,ny); nx:=nx+dx; ny:=ny+dy;
      if Hypot(nx,ny) < tmp then begin
        nx:=nx-dx-dx; ny:=ny-dy-dy
      end
    end; len:=Hypot(nx,ny);

    if len > Small then begin
      dir.x:=nx/len; dir.y:=ny/len;
      Result:=true
    end
  end
end;

constructor TFilter.Init;
begin
end;

function TFilter.Dist_to_Line(const p, a,b: TPoint): double;
var
  tx,ty: double;
begin
  a1:=(b.y-a.y); a2:=(a.x-b.x); tx:=a.x; ty:=a.y;
  a3:=ty*b.x-b.y*tx; al:=Sqrt(a1*a1+a2*a2);
  Result:=Next_Dist(p)
end;

function TFilter.Next_Dist(const p: TPoint): double;
begin
  Result:=0; if al > Small then
  Result:=(a1*p.x+a2*p.y+a3)/al
end;

function TFilter.Collinear(lp: PLPoly; n: Integer; dl: double): Boolean;
var
  i: Integer;
begin
  Result:=true; if n > 1 then begin

    Dist_to_Line(lp[0], lp[0],lp[n]);

    i:=1; while i < n do begin
      if Abs(Next_Dist(lp[i])) > dl then
      begin Result:=false; Break end; Inc(i)
    end
  end
end;

function TFilter.Cls(lp: PLLine; i1,i2,o: Integer; dl: double): Integer;
var
  i,j, i3: Integer; l,t: double;
begin
  with lp^ do while i1 < i2 do begin

    i3:=i2; while i1+1 < i3 do begin j:=i1+1;

      l:=Abs(Dist_to_Line(Pol[j], Pol[i1],Pol[i3]));

      if al > dl then begin
        for i:=i1+2 to i3-1 do begin
          t:=Abs(Next_Dist(Pol[i]));
          if t > l then begin j:=i; l:=t end;
        end;

        if l <= dl then Break else i3:=j;

      end else Break
    end;

    Norm(lp,i1,i3,o); Inc(o);

    Pol[o]:=Pol[i3]; i1:=i3

  end; Result:=o
end;

procedure TFilter.Norm(lp: PLLine; i1,i3,o: Integer);
begin
end;

function TFilter.x_bound(lp: PLPoly; N: Integer; dl: double): Integer;
var
  i,j,k: Integer; lt,rb,c: TPoint;
  cx,cy,len: double; lock: Boolean;
begin                
  if N > 1 then begin

    lock:=LPoly_Lock(lp,N);

    if lock then
    if xCentre_Polygon(lp,N,cx,cy) > 0 then begin

      Dec(N);

      c.x:=Round(cx); c.y:=Round(cy);
      len:=Long_Dist(c,lp[0]);

      if (Long_Dist(c,lp[1]) > len)
      or (Long_Dist(c,lp[N]) > len) then begin
        i:=Edge_Vertex(c,lp,N);
        if i > 0 then Order_LPoly(lp,N,i)
      end;

      Inc(N); lp[N]:=lp[0]
    end;

    if dl <= 1 then begin
      Max_Poly_Bound(lp,N, lt,rb);
      dl:=(rb.x-lt.x)/16 + (rb.y - lt.y)/16
    end;

    i:=0; j:=0; while i+1 < N do begin

      k:=2; while i+k <= N do begin
        if not Collinear(@lp[i],k,dl) then
        Break; Inc(k)
      end;

      Inc(i,k-1);
      if i > N then i:=N;
      Inc(j); lp[j]:=lp[i]
    end;

    if i <= N then begin
      Inc(j); lp[j]:=lp[N]
    end; N:=j;

    if lock then
    if not Points_Equal(lp[0],lp[N]) then begin
      Inc(N); lp[N]:=lp[0]
    end
  end;

  Result:=N
end;

function TFilter.y_bound(lp: PLLine; o: Integer; dl: double): Integer;
var
  i,i1,i2,k: Integer; dw,dh, cx,cy,len: double;
  lock: Boolean; a,b,c,pc: TPoint; p: LOrient;
begin
  Result:=0; with lp^ do

  if N >= 2 then if o < N then begin i1:=o; i2:=N;

    lock:=PolyLock(lp); if lock and (o = 0) then begin

      i1:=0; if dl = 0 then begin
        Max_Poly_Bound(@Pol,N,a,b); dw:=(b.x-a.x)/20;
        dh:=(b.y-a.y)/20; dl:=dw; if dh < dw then dl:=dh
      end;

      if Centre_Poly(lp,cx,cy) > 0 then begin

        c.x:=Round(cx); c.y:=Round(cy);
        len:=Long_Dist(c,Pol[0]);

        if (Long_Dist(c,Pol[1]) > len)
        or (Long_Dist(c,Pol[N-1]) > len) then
        Order_Poly(lp,Edge_Vertex(c,@Pol,N))
      end;

      i2:=Edge_Vertex(Pol[0],@Pol,N)
    end;

    if dl = 0 then dl:=1; i:=o;

    if not lock and (o = 0) and (N > 63) then begin
      i2:=0; while i2 < N do begin
        i1:=i2; i2:=i1+64; if i2 > N then i2:=N;
        if i1 > 0 then begin Dec(i1); Dec(o) end;
        o:=Cls(lp,i1,i2,o,dl)
      end
    end
    else while i1 < i2 do begin
      o:=Cls(lp,i1,i2,o,dl); i1:=i2; i2:=N
    end;

    N:=o; if o-i > 2 then begin

      o:=i; k:=0; while i <= N do begin
        p[k]:=Pol[i]; Inc(k); Inc(i);

        if k = 4 then begin
          if Double_Point(p[0],p[1],p[2],p[3],dl,pc) then
          begin p[1]:=pc; p[2]:=p[3]; k:=3 end;

          if k = 4 then begin
            p[0]:=p[1]; p[1]:=p[2]; p[2]:=p[3];
            k:=3; Inc(o); Pol[o]:=p[0]
          end
        end
      end;

      for i:=1 to k-1 do begin
        Inc(o); Pol[o]:=p[i]
      end;

      N:=o;
    end;

    Result:=Round(dl)
  end
end;

procedure TOrtPoly.Norm(lp: PLLine; i1,i3,o: Integer);
var
  i,di: Integer; gx,gy: double;
begin
  with lp^ do if o <= lPoly_Max then begin
    gx:=0; gy:=0; di:=i3-i1+1;

    for i:=i1 to i3 do begin
      gx:=gx + Pol[i].x; gy:=gy + Pol[i].y
    end;

    nv[o].x:=gx/di; nv[o].y:=gy/di
  end
end;

procedure TOrtPoly.z_bound(lp: PLLine; dl: double);

function norm_lp(lp: PLLine): Boolean;
var
  i: Integer;
begin
  Result:=false; with lp^ do
  if N <= lPoly_Max then begin
    for i:=0 to N-1 do Norm(lp,i,i+1,i);
    Result:=true
  end
end;

function ort_lp(lp: PLLine): Boolean;
var
  i: Integer; vx,vy,dx,dy,rmu: double;
  nd: array[0..1] of tgauss; v1,v2,d1,d2: tgauss;
begin
  Result:=false;

  with lp^ do if N <= lPoly_Max then
  if Norm_Poly(lp,nd[0]) then begin

    nd[1].x:=-nd[0].y; nd[1].y:=nd[0].x;

    nv[N]:=nv[0]; for i:=1 to N do begin

      d1:=nd[i and 1]; d2:=nd[(i+1) and 1];
      dx:=d1.x; dy:=d1.y; rmu:=dx*d2.y - dy*d2.x;

      if Abs(rmu) > Small then begin
        v1:=nv[i-1]; v2:=nv[i];
        vx:=v2.x-v1.x; vy:=v2.y-v1.y;

        rmu:=(vx*d2.y - vy*d2.x) / rmu;

        v1.x:=v1.x+rmu*dx;
        v1.y:=v1.y+rmu*dy; nv[i-1]:=v1
      end
      else Exit
    end;

    for i:=1 to N do with Pol[i] do begin
      x:=Round(nv[i-1].x); y:=Round(nv[i-1].y);
    end; Pol[0]:=Pol[N];

    Result:=true
  end
end;

begin
  if PolyLock(lp) then

  if Norm_lp(lp) and ort_lp(lp) then
  else

  if y_bound(lp,0,0) > 0 then
  if not ort_lp(lp) then
  
  Rect_Poly(lp)
end;

function dist_Poly(lp: PLLine; cls: double): bool;
var
  i,o,k: int; p1,p2: TPoint; len: double;
begin
  Result:=false;

  with lp^ do if N > 0 then begin

    i:=0; o:=0; k:=0; len:=0; p1:=Pol[i];

    while i < N do begin
      Inc(i); p2:=Pol[i]; Inc(k);
      len:=len+Long_Dist(p1,p2);

      if len > cls then begin
        Inc(o); Pol[o]:=p2; len:=0; k:=0
      end; p1:=p2
    end;

    if k > 0 then begin
      Inc(o); Pol[o]:=p2;
    end;

    Result:=o < N; N:=o
  end
end;

function dup_LTurn(lp: PLPoly; lp_N: int): int;
var
  i,j: int; p,q: TPoint;
begin
  Result:=-1;

  i:=2;
  while i <= lp_N do begin
    q:=lp[i-2]; p:=lp[i];
    if (q.X = p.X) and (q.Y = p.Y) then begin
      Dec(lp_N,2); for j:=i to lp_N do lp[j]:=lp[j+2];
      Dec(i)
    end;

    Inc(i)
  end;

  Result:=lp_N
end;

function dup_LPlan(lp: PLPoly; hp: PIntegers;
                   lp_N: Integer): Integer;
var
  i: int; p,q: TPoint;
begin
  Result:=-1;
  if lp_N >= 0 then begin

    Result:=0; p:=lp[0];

    for i:=1 to lp_N do begin q:=p; p:=lp[i];

      if (p.X <> q.X)
      or (p.Y <> q.Y) then begin
        Inc(Result); lp[Result]:=p;
        if Assigned(hp) then hp[Result]:=hp[i];
      end

    end
  end
end;

function dup_LPoly(lp: PLPoly; hp: PIntegers;
                   lp_N: Integer; is_dir: Boolean): Integer;
var
  i: Integer; p,q: lxyz;
begin
  Result:=-1;
  if lp_N >= 0 then begin Result:=0;

    q.p:=lp[0]; q.v.z:=0;
    if Assigned(hp) then q.v.z:=hp[0];

    for i:=1 to lp_N do begin

      p.p:=lp[i]; p.v.z:=0;
      if Assigned(hp) then p.v.z:=hp[i];

      if not CompareMem(@p,@q,Sizeof(p)) then begin
        Inc(Result); lp[Result]:=p.p;
        if Assigned(hp) then hp[Result]:=p.v.z;

        if is_dir and (Result >= 2) then
        if LineContainsPoint(lp[Result-2],p.p,q.p) then
        begin
          Dec(Result); lp[Result]:=p.p;
          if Assigned(hp) then hp[Result]:=p.v.z;
        end
      end;

      q:=p
    end
  end
end;

function peak_LPoly(lp: PLPoly; lp_N: Integer): Integer;
begin
  Result:=lp_N;
  if lp_N > 1 then begin
  end
end;

function cls_LPoly(lp: PLPoly; lp_N: Integer;
                   D,R: double): Integer;

function xRib(const p1,p2,p3: TPoint): Boolean;
begin
  Result:=false;
  if LineContainsPoint(p1,p2, p3)
  or LineContainsPoint(p2,p3, p1) then
  Result:=true
end;

function lp_cls3(lp: PLPoly; lp_N: Integer; R: Double): Integer;
var
  i,j,k: Integer; p: LOrient;
begin
  if lp_N > 1 then
  if R > 0 then begin
    R:=Max(0.6,R);

    p[0]:=lp[0]; p[1]:=lp[1];
    j:=0; i:=2; k:=2;

    while i <= lp_N do begin
      p[k]:=lp[i]; Inc(k); Inc(i);

      if k = 3 then begin
        if Abs(Dist_to_Line(p[1], p[0],p[2])) <= R then
        begin p[1]:=p[2]; k:=2 end;

        if k = 3 then begin
          p[0]:=p[1]; p[1]:=p[2];
          k:=2; Inc(j); lp[j]:=p[0];
        end
      end
    end;

    for i:=1 to k-1 do begin
      Inc(j); lp[j]:=p[i]
    end; lp_N:=j;

    if (lp_N > 3) and LPoly_Lock(lp,lp_N) then
    if Abs(Dist_to_Line(lp[0], lp[lp_N-1],lp[1])) <= R then begin
      Dec(lp_N); for i:=0 to lp_N do lp[i]:=lp[i+1];
      lp[lp_N]:=lp[0]
    end
  end;

  Result:=lp_N
end;

function Wave_LPoly(lp: PLPoly; lp_N: Integer;
                    R: double): Integer;
var
  i,j,k: Integer; p: TPoint;
  pp: LOrient; lp_lock: Boolean;
begin
  if R > 0 then
  if lp_N > 1 then begin

    lp_lock:=LPoly_Lock(lp,lp_N);

    pp[0]:=lp[0]; pp[1]:=lp[1];
    j:=0; i:=2; k:=2;

    while i <= lp_N do begin
      pp[k]:=lp[i]; Inc(k); Inc(i);

      if (j = 0) and (k = 3) and
         (i < lp_N) and lp_lock then begin

        if Double_Point(lp[lp_N-1],pp[0],pp[1],pp[2],R,p) then begin
          lp[j]:=p; if lp_lock then lp[lp_N]:=p;
          pp[0]:=p; pp[1]:=pp[2]; k:=2
        end

      end else

      if k = 4 then begin
        if Double_Point(pp[0],pp[1],pp[2],pp[3],R,p) then
        begin pp[1]:=p; pp[2]:=pp[3]; k:=3 end;

        if k = 4 then begin
          pp[0]:=pp[1]; pp[1]:=pp[2]; pp[2]:=pp[3];
          k:=3; Inc(j); lp[j]:=pp[0]
        end
      end
    end;

    if (k = 3) and (lp_lock) then
    if Double_Point(pp[0],pp[1],pp[2],lp[1],R,p) then
    begin pp[1]:=p; lp[0]:=p; k:=2 end;

    for i:=1 to k-1 do begin
      Inc(j); lp[j]:=pp[i]
    end; lp_N:=j
  end;

  Result:=lp_N
end;

function lp_cls5(lp: PLPoly; lp_N: Integer;
                 R: Double): Integer;
var
  i,j,k: Integer; p: LOrient;
  d1,d2,d3,d_1,d_3: Double; pc: TPoint;
begin
  if R > 0 then
  if lp_N >= 4 then begin
    R:=Max(1,R);

     j:=0; p[0]:=lp[0]; i:=1; k:=1;

    while i <= lp_N do begin
      p[k]:=lp[i]; Inc(k); Inc(i);

      if k = 5 then begin
        d1:=Dist_to_Line(p[1], p[0],p[2]);
        d2:=Dist_to_Line(p[2], p[1],p[3]);
        d3:=Dist_to_Line(p[3], p[2],p[4]);

        if Abs(d1) <= R then
        if Abs(d2) <= R then
        if Abs(d3) <= R then

        if (d1 <= 0) = (d3 <= 0) then
        if (d1 <= 0) <> (d2 <= 0) then begin
          pc.x:=Round(p[1].x/4+p[2].x/2+p[3].x/4);
          pc.y:=Round(p[1].y/4+p[2].y/2+p[3].y/4);

          d_1:=Dist_to_Line(p[1], p[0],pc);
          d_3:=Dist_to_Line(p[3], pc,p[4]);

          if (d1 <= 0) = (d_1 <= 0) then
          if (d3 <= 0) = (d_3 <= 0) then begin
            p[1]:=pc; p[2]:=p[4]; k:=3
          end
        end;

        if k = 5 then begin
          p[0]:=p[1]; p[1]:=p[2];
          p[2]:=p[3]; p[3]:=p[4];
          k:=4; Inc(j); lp[j]:=p[0]
        end

      end
    end;

    for i:=1 to k-1 do begin
      Inc(j); lp[j]:=p[i]
    end; lp_N:=j
  end;

  Result:=lp_N
end;

var
  i,j: int; p1,p2,p3: TPoint; 
begin
  if lp_N > 0 then begin

    R:=Max(R,0.7); if D < R then D:=R*2;

    j:=0; p1:=lp[0];
    for i:=1 to lp_N-1 do begin
      p2:=lp[i]; if Long_Dist(p1,p2) >= D then
      begin Inc(j); lp[j]:=p2; p1:=p2 end
    end;

    p1:=lp[lp_N]; Inc(j); lp[j]:=p1;

    while (j > 1) and (Long_Dist(p1,lp[j-1]) < R) do
    begin Dec(j); lp[j]:=p1 end; lp_N:=j;

    if lp_N > 1 then begin
      j:=0; p1:=lp[0]; p2:=lp[1];

      for i:=2 to lp_N do begin p3:=lp[i];

        if not Points_Equal(p2,p3) then begin
          if not xRib(p1,p2,p3) then begin
            Inc(j); lp[j]:=p2; p1:=p2;
          end;

          p2:=p3
        end
      end;

      Inc(j); lp[j]:=p2; lp_N:=j;

      while LPoly_Lock(lp,lp_N) do begin
        if xRib(lp[lp_N-1],lp[0],lp[1]) then else
        if Points_Equal(lp[0],lp[1]) then else
        Break; Dec(lp_N); lp[0]:=lp[lp_N]
      end;

      if R > 0 then begin
        lp_N:=lp_cls3(lp,lp_N,R/3);
        lp_N:=Wave_LPoly(lp,lp_N,R);
        lp_N:=lp_cls5(lp,lp_N,R);
      end
    end
  end;

  Result:=lp_N
end;

function cls_Poly(lp: PLLine; R: double): Integer;
begin
  if lp.N > 1 then
  lp.N:=cls_LPoly(@lp.Pol,lp.N,0,R);
  Result:=lp.N
end;

type
  teps_lpoly = class
    function Execute(lp: PLPoly; n: int; eps: double): int;
  private
    flp: PLPoly;
    fsp: int;
    feps: double;
  end;

function teps_lpoly.Execute(lp: PLPoly; n: int; eps: double): int;

procedure part(lp: PLPoly; n: int);
var
  i,j: int; r,t: double; lc: TLine_coeff;
  a,b: TPoint;
begin
  if n > 0 then begin j:=-1;

    if n > 1 then begin
      a:=lp[0]; b:=lp[n]; j:=1; r:=-1;

      if Points_Equal(a,b) then begin

        r:=Long_dist(a,lp[1]);
        for i:=2 to n-1 do begin
          t:=Long_Dist(a,lp[i]);
          if t > r then begin j:=i; r:=t end
        end
      end else

      if Get_line_coeffp(a,b,lc) > 1 then begin
        with lp[1] do
        r:=Abs(c_dist_to_line(lc,X,Y));

        for i:=2 to n-1 do begin
          with lp[i] do
          t:=Abs(c_dist_to_line(lc,X,Y));
          if t > r then begin j:=i; r:=t end
        end;
      end;

      if r <= eps then j:=-1
    end;

    if j <= 0 then begin
      Inc(fsp); flp[fsp]:=lp[n]
    end
    else begin
      part(lp,j);
      part(@lp[j],n-j)
    end
  end
end;

begin
  flp:=lp; fsp:=0; feps:=eps;
  part(lp,n); Result:=fsp
end;

function eps_LPoly(lp: PLPoly; n: int; eps: double): int;
var
  cls: teps_lpoly;
begin
  Result:=n;

  cls:=teps_lpoly.Create;
  try
    Result:=cls.Execute(lp,n,eps)
  finally
    cls.Free
  end
end;

function line_LPoly(lp: PLPoly; N: Integer; R: double): Integer;
var
  cls: TFilter;   
begin
  if N > 1 then begin
    N:=cls_LPoly(lp,N,0,R/4);
    cls.Init; N:=cls.x_bound(lp,N,R);
  end; Result:=N
end;

function line_Poly(lp: PLLine; R: double): Integer;
begin
  lp.N:=line_LPoly(@lp.Pol,lp.N,R);
  Result:=lp.N
end;

procedure ort_Poly(lp: PLLine; R: double);
var
  cls: TOrtPoly;
begin
  lp.N:=cls_LPoly(@lp.Pol,lp.N,0,R);
  cls.Init; cls.z_bound(lp,R)
end;

function dir_Poly(lp: PLLine; min: double): Boolean;
var
  i,o: Integer; p1,p2,p3: TPoint;
  dir,tmp: Double;
begin
  Result:=false;

  with lp^ do
  if N > 0 then begin

    p2:=Pol[0]; i:=0; o:=0;

    while i < N do begin

      p1:=p2; Inc(i); p2:=Pol[i];

      while Points_Equal(p1,p2) and (i < N) do
      begin Inc(i); p2:=Pol[i] end;

      if not Points_Equal(p1,p2) then begin
        dir:=iAngle(p1,p2);
        while i < N do begin
          p3:=Pol[i+1]; tmp:=iAngle(p1,p3);
          if Abs(tmp-dir) > min then Break;
          p2:=p3; Inc(i)
        end;

        Inc(o); Pol[o]:=p2
      end
    end;

    Result:=N > o;
    N:=o
  end
end;

function dist_to_Polygon(p: TPoint; lp: PLLine): double;
var
  i: Integer; t: double;
begin
  Result:=-1; with lp^ do

  for i:=0 to N-1 do begin
    t:=Abs( Dist_to_Line(p, Pol[i],Pol[i+1]) );
    if (Result < 0) or (t < Result) then
    Result:=t;
  end
end;

procedure rect_Poly(lp: PLLine);

function x_rect(lp,dst: PLLine; cod: Integer): double;
var
  cx,cy, r,a,b, fi,s,t: double;
  c,p1,p2,p3,p4: TPoint; i,dx,dy: Integer;
begin
  Result:=-1;

  s:=Centre_Polygon(lp, cx,cy);
  if s > 0 then with lp^ do begin

    c.x:=Round(cx); c.y:=Round(cy);

    case cod of
  0:  p1:=Pol[0];
  1:  Middle_Point(Pol[0],Pol[1],p1);
    end;

    r:=Long_Dist(p1,c);

    for i:=1 to N-1 do begin

      p2:=Pol[i];
      case cod of
    0:  p2:=Pol[i];
    1:  Middle_Point(Pol[i],Pol[i+1],p2);
      end;

      t:=Long_Dist(p2,c);
      if t > r then begin
        r:=t; p1:=p2
      end
    end;

    a:=-1; if r > 0 then

    for i:=0 to N-1 do begin

      case cod of
    0:  p3:=Pol[i];
    1:  Middle_Point(Pol[i],Pol[i+1],p3);
      end;

      t:=Long_Dist(p1,p3);
      if (a < 0) or (a < t) then
      begin a:=t; p2:=p3 end
    end;

    if a > 0 then begin b:=s/a/2;
      fi:=iArcTan(p1.x,p1.y,p2.x,p2.y);

      dx:=p2.x-p1.x; dy:=p2.y-p1.y;

      p1:=prj_LPoint(p1,b,fi+Pi/2);
      p4:=prj_LPoint(p1,b+b,fi-Pi/2);

      p2.x:=p1.x+dx; p2.y:=p1.y+dy;
      p3.x:=p4.x+dx; p3.y:=p4.y+dy;

      dst.Pol[0]:=p1; dst.Pol[1]:=p2;
      dst.Pol[2]:=p3; dst.Pol[3]:=p4;
      dst.Pol[4]:=p1; dst.N:=4;

      Result:=0; for i:=0 to N-1 do
      Result:=Result+dist_to_Polygon(Pol[i],dst)/N

    end
  end
end;

var
  k,t: double; i,j: Integer; dst: VLLine;
begin
  if PolyLock(lp) then
  if lp.N >= 4 then begin

    j:=0; k:=x_rect(lp,@dst,j);

    for i:=1 to 1 do begin
      t:=x_rect(lp,@dst,i);
      if t > 0 then if t < k then
      begin k:=t; j:=i end
    end;

    if x_rect(lp,@dst,j) > 0 then with lp^ do
    begin Move(dst.Pol,Pol,5*8); N:=4 end

  end
end;

function Backup_Polyline(lp: PLLine; k,r: Double;
                         out p: TPoint): Integer;

procedure prj_left(var p: TPoint;
                   const p1,p2: TPoint;
                   r: Double);
var
  dx,dy: Integer; nx,ny,rib: Double;
begin
  dx:=p2.X-p1.X; dy:=p2.Y-p1.Y;
  rib:=Hypot(dx,dy);

  if rib >= 0.001 then begin
    nx:=-dy/rib; ny:=dx/rib;
    Inc(p.X,Round(nx*r));
    Inc(p.Y,Round(ny*r))
  end
end;

var
  dist,len,rib,nx,ny: Double;
  i,dx,dy: Integer; p1,p2: TPoint;
begin
  Result:=0;

  with lp^ do
  if N < 0 then p:=Point(0,0) else
  if N = 0 then p:=Pol[0] else
  if k <= 0 then begin
    p:=Pol[0]; if N > 0 then
    prj_left(p,Pol[0],Pol[1],r)
  end else
  if k >= 1 then begin
    p:=Pol[N]; if N > 0 then
    prj_left(p,Pol[N-1],Pol[N],r);
    Result:=N;
  end
  else begin p:=Pol[0];

    len:=PolyLength(@Pol,N);

    if N > 0 then
    prj_left(p,Pol[0],Pol[1],r);

    dist:=len * k; p2:=Pol[0];
    if dist > 0 then
    for i:=1 to N do begin
      p1:=p2; p2:=Pol[i];

      dx:=p2.X-p1.X; dy:=p2.Y-p1.Y;
      rib:=Hypot(dx,dy); Result:=i;

      if rib >= 0.001 then begin

        nx:=-dy/rib; ny:=dx/rib;

        if dist > rib then begin
          p:=p2; dist:=dist-rib;
          if Abs(r) >= 0.01 then begin
            Inc(p.X,Round(nx*r));
            Inc(p.Y,Round(ny*r));
          end
        end
        else begin
          p:=p1; if dist > 0 then
          Get_Next_Point(p1,p2,dist,p);
          if Abs(r) >= 0.01 then begin
            Inc(p.X,Round(nx*r));
            Inc(p.Y,Round(ny*r));
          end; Break
        end
      end
    end
  end
end;

function Polyline_locate(lp: PLPoly; N: int;
                         const p: TPoint; dist: Double): int;
var
  i: Integer;
begin
  Result:=-1;

  if dist <= 0 then begin
    for i:=0 to N do
    if Points_Equal(lp[i],p) then
    begin Result:=i; Break end
  end else
  for i:=0 to N do
  if Long_Dist(lp[i],p) <= dist then
  begin Result:=i; Break end
end;

function Polyline_Contains(lp: PLPoly; lp_N: Integer;
                           const p: TPoint; max_r: Double;
                           out C: TPoint; out R: Double): Double;
var
  i: Integer; q1,q2,q: TPoint;
  dist,loc,tmp: Double;
begin
  Result:=-1; c:=Point(0,0); R:=0;

  if lp_N >= 0 then begin

    q2:=lp[0]; dist:=0;

    loc:=-1; if max_r >= 0 then begin
      tmp:=Long_Dist(p,q2);
      if tmp <= max_r then begin
        loc:=tmp; c:=q2; Result:=0
      end
    end;

    for i:=1 to lp_N do begin
      q1:=q2; q2:=lp[i]; tmp:=-1;

      if not Plot_to_Line(p ,q1,q2, q) then
      if max_r > 0 then q:=q2; tmp:=Long_Dist(p,q);

      if tmp >= 0 then
      if (loc < 0) or (tmp < loc) then

      if (max_r < 0) or (tmp <= max_r) then begin
        loc:=tmp; c:=q; Result:=dist+Long_Dist(q1,q)
      end;

      dist:=dist+Long_Dist(q1,q2)
    end;

    if Result >= 0 then R:=loc
  end
end;

function Polyline_Contains1(lp: PLLine;
                            const p: TPoint; max_r: Double;
                            out C: TPoint; out R: Double): Double;
begin
  Result:=Polyline_Contains(@lp.Pol,lp.N, p,max_r, C,R)
end;

function ProjectToPolyLine(lp: PLPoly; lp_N: Integer;
                           const p: TPoint; d1,d2: Double;
                           out Ind: Integer; out c: TPoint): Boolean;
var
  i: Integer; q1,q2,q: TPoint;
  dist,tmp: Double;
begin
  Result:=false; Ind:=0;

  if lp_N = 0 then begin
    c:=lp[0]; dist:=Long_Dist(p,c);
    if d1 > d2 then Result:=true else
    Result:=(dist >= d1) and (dist <= d2)
  end else

  if lp_N > 0 then begin

    q2:=lp[0]; dist:=0;

    tmp:=Long_Dist(p,q2); if (d1 > d2)
    or ((tmp >= d1) and (tmp <= d2)) then begin
      dist:=tmp; c:=q2; Result:=true; Ind:=0
    end;

    for i:=1 to lp_N do begin
      q1:=q2; q2:=lp[i];

      if Plot_to_Line(p ,q1,q2, q) then begin

        tmp:=Long_Dist(p,q);

        if not Result or (tmp < dist) then

        if (d1 > d2) or
           ((tmp >= d1) and (tmp <= d2)) then begin
          dist:=tmp; c:=q; Result:=true;

          Ind:=-i;
          if Points_Equal(q,q1) then Ind:=i-1 else
          if Points_Equal(q,q2) then Ind:=i else

          if (d1 < 0.1) and (d1 < d2) then
          if Long_dist(q,q1) <= d2 then Ind:=i-1 else
          if Long_dist(q,q2) <= d2 then Ind:=i
        end
      end
      else begin
        tmp:=Long_Dist(p,q2);

        if not Result or (tmp < dist) then
        if (d1 > d2)
        or ((tmp >= d1) and (tmp <= d2)) then begin
          dist:=tmp; c:=q2; Result:=true; Ind:=i
        end
      end
    end

  end
end;

function Project_to_PolyLine(lp: PLLine;
                             const p: TPoint; d1,d2: Double;
                             out Ind: Integer; out c: TPoint): Boolean;
begin
  Result:=ProjectToPolyLine(@lp.Pol,lp.N, p,d1,d2, Ind,c)
end;

function xProject_to_LPoly(lp: PLPoly; lp_N: Integer;
                           const p: TPoint; d1,d2: Double;
                           out Ind: Integer; out c: TPoint): Double;
var
  i: Integer; q1,q2,q: TPoint;
  r,t,dist,lab: Double;
begin
  Result:=-1; Ind:=0;

  if lp_N >= 0 then begin
    c:=lp[0]; r:=Long_Dist(p,c);
    dist:=0; lab:=0;

    q2:=c;
    for i:=1 to lp_N do begin
      q1:=q2; q2:=lp[i];

      if not Plot_to_Line(p ,q1,q2, q) then
      q:=q2;

      t:=Long_Dist(p,q); if t < r then begin

        Ind:=-i;
        if Points_Equal(q,q1) then Ind:=i-1 else
        if Points_Equal(q,q2) then Ind:=i else

        if (d1 < 0.1) and (d1 < d2) then
        if Long_dist(q,q1) <= d2 then begin
          q:=q1; Ind:=i-1
        end else
        if Long_dist(q,q2) <= d2 then begin
          Ind:=i; q:=q2
        end;

        lab:=dist + Long_Dist(q1,q);
        c:=q; r:=t;
      end;

      dist:=dist + Long_Dist(q1,q2)
    end;

    if (d1 > d2)
    or ((r >= d1) and (r <= d2)) then
    Result:=lab
  end
end;

function xProject_to_PolyLine(lp: PLLine; const p: TPoint;
                              out c: TPoint): Double;
var
  Ind: Integer;
begin
  Result:=xProject_to_LPoly(@lp.Pol,lp.N, p,1,0, Ind,c)
end;

function ippl(lp: PLLine; const p: TPoint;
              maxr: Double; out mk: TLineMarker1): Boolean;
var
  i,dx,dy,px,py,rc,dr: int;
  q1,q2,q,lt,rb: TPoint;
  r1,r2,len,rib: Double;
begin
  Result:=false;

  Fillchar(mk,Sizeof(mk),0); mk.pc:=p;

  with lp^ do if N >= 0 then begin

    px:=p.X; py:=p.Y; r2:=0; len:=0;

    q2:=Pol[0];
    if Points_Equal(p,q2) then
      Result:=true
    else begin

      dr:=-1; if maxr >= 0 then
      dr:=Trunc(maxr)+1;

      r1:=Hypot(q2.X-px,q2.Y-py);
      if (maxr < 0) or (r1 <= maxr) then
      if not Result or (r1 < r2) then begin
        mk.Ind:=0; mk.pc:=q2; mk.dist:=len;
        r2:=r1; Result:=true;
      end;

      for i:=1 to N do begin
        q1:=q2; q2:=Pol[i];
        dx:=q2.X-q1.X; dy:=q2.Y-q1.Y;

        if (dx <> 0) or (dy <> 0) then begin

          lt:=q1; rb:=q2;
          if lt.X > rb.X then begin lt.X:=q2.X; rb.X:=q1.X end;
          if lt.Y > rb.Y then begin lt.Y:=q2.Y; rb.Y:=q1.Y end;

          rib:=Hypot(dx,dy); len:=len + rib;

          if (dr < 0) or
             ((px >= lt.X-dr) and (px <= rb.X+dr) and
              (py >= lt.Y-dr) and (py <= rb.Y+dr)) then

          if Points_Equal(p,q2) then begin
            mk.Ind:=i; mk.dist:=len;
            mk.pc:=p; Result:=true; Break
          end
          else begin
            rc:=Plot_to_Line1(p ,q1,q2, q);

            if rc >= 0 then begin
              if rc <> 0 then q:=q2;

              r1:=Hypot(q.X-px,q.Y-py);
              if (maxr < 0) or (r1 <= maxr) then
              if not Result or (r1 < r2) then begin

                r2:=r1; mk.pc:=q; Result:=true;

                if Points_Equal(q1,q) then begin
                  mk.Ind:=i-1; mk.dist:=len-rib;
                end else
                if Points_Equal(q2,q) then begin
                  mk.Ind:=i; mk.dist:=len
                end
                else begin
                  mk.Ind:=-i;
                  rib:=Hypot(q2.X-q.X,q2.Y-q.Y);
                  mk.dist:=len - rib
                end
              end
            end
          end
        end
      end
    end
  end
end;

function Plot_to_Polyline(lp: PLLine; const p: TPoint;
                          maxr: Double; out ic: Integer;
                          out c: TPoint): Boolean;
var
  m: TLineMarker1;
begin
  ic:=0; c:=p;

  Result:=ippl(lp,p,maxr,m);
  if Result then begin
    ic:=m.Ind; c:=m.pc
  end
end;

function Poly_Azimuth(lp: PLLine; ic: Integer): Double;
var
  i1,i2: Integer; fi1,fi2: Double;
begin
  Result:=0;

  if ic < 0 then begin
    i1:=Abs(ic)-1; i2:=Abs(ic)
  end
  else begin
    i1:=Max(0,ic-1);
    i2:=Min(lp.N,ic+1)
  end;

  with lp^ do
  if (i1 >= 0) and (i2 <= N) then

  if i1 < i2 then begin

    fi1:=iAngle(Pol[i1],Pol[i1+1]);

    fi2:=fi1; if i1+1 < i2 then
    fi2:=iAngle(Pol[i1+1],Pol[i1+2]);

    Result:=fi1/2 + fi2/2
  end
end;

function LPoly_Contains_pt(lp: PLPoly; lp_N: Integer;
                           const p: TPoint; out c: TPoint;
                           out dist: Double): Integer;
var
  i, dx,dy: Integer; d,d1,d2: Double;
  a,b, lt,rb: TPoint; pc: TGauss;
begin
  Result:=-1; dist:=-1; c:=p;

  if lp_N > 0 then begin
    b:=lp[0];
    if Points_Equal(p,b) then begin
      Result:=0; dist:=0
    end else

    for i:=1 to lp_N do begin
      a:=b; b:=lp[i];

      if Points_Equal(p,b) then begin
        Result:=i; dist:=0; Break
      end
      else begin
        lt.x:=Min(a.x,b.x); rb.x:=Max(a.x,b.x);
        lt.y:=Min(a.y,b.y); rb.y:=Max(a.y,b.y);

        if (p.x >= lt.x) and (p.x <= rb.x) then
        if (p.y >= lt.y) and (p.y <= rb.y) then begin

          dx:=b.x-a.x; dy:=b.y-a.y; d1:=0; d2:=0;

          if (dx <> 0) and (dy <> 0) then

          if Abs(dx) > Abs(dy) then begin
            d1:=a.y + (p.x-a.x)/dx*dy - p.y;
            d2:=b.y + (p.x-b.x)/dx*dy - p.y;
          end
          else begin
            d1:=a.x + (p.y-a.y)/dy*dx - p.x;
            d2:=b.x + (p.y-b.y)/dy*dx - p.x;
          end;

          d:=Min(Abs(d1),Abs(d2));
          if d < line_eps then begin
            dist:=Long_Dist(a,p);
            Result:=i-1; Break
          end else
          if d < 1 then
          if prj_to_Line(p,a,b,pc) then
          if (p.x = Round(pc.x)) then
          if (p.y = Round(pc.y)) then begin
            dist:=Long_Dist(a,p);
            Result:=i-1; Break
          end
        end
      end
    end
  end
end;

function lp_Contains_Point(lp: PLLine; const p: TPoint;
                           r: Double; out dist: Double;
                           out c: TPoint): Integer;
var
  lp_i: Integer;
begin
  Result:=-1; dist:=-1; c:=p;

  if lp.N >= 0 then

  if r > 0 then begin
    if Plot_to_Polyline(lp,p,r,lp_i,c) then

    if lp_i < 0 then begin
      lp_i:=Abs(lp_i)-1; Result:=lp_i;
      dist:=Long_Dist(lp.Pol[lp_i],c);
    end
    else begin
      Result:=lp_i; dist:=0
    end
  end else

  Result:=LPoly_Contains_pt(@lp.Pol,lp.N,p, c,dist)
end;

function Plot_to_Polygon(lp: PLLine; const p: TPoint;
                         r: Double; out ic: Integer;
                         out c: TPoint): Boolean;
begin
  Result:=false;
  c.X:=0; c.Y:=0; ic:=0;

  if PolyLock(lp) then
  Result:=Plot_to_Polyline(lp,p,r, ic,c)
end;

function LocatePolyLine(lp: PLPoly; N: Integer;
                        const lt,rb: TPoint;
                        out c: TPoint): Integer;
var
  a,b: TPoint; i,x1,y1,x2,y2: Integer;
begin
  Result:=-1;

  if N > 0 then begin
    x1:=lt.X; y1:=lt.Y; x2:=rb.X; y2:=rb.Y;

    b:=lp[0];
    for i:=0 to N do begin
      a:=b; b:=lp[i];

      if x1 < Max(a.X,b.X) then
      if x2 > Min(a.X,b.X) then
      if y1 < Max(a.Y,b.Y) then
      if y2 > Min(a.Y,b.Y) then begin

        if (b.X >= x1) and (b.X <= x2) then
        if (b.Y >= y1) and (b.Y <= y2) then
        begin c:=b; Result:=i; Break end;

        if i > 0 then
        if Locate_Line(a,b, lt,rb, c) then 
        begin Result:=i-1; Break end
      end
    end
  end
end;

var
  _rPolygon_eps: float;

procedure Set_rPolygon_eps(v: float);
begin
  if v < 0 then v:=0.5;
  _rPolygon_eps:=v
end;

function _rPolygonContainsPixel(lp: PLPoly; lp_n: Integer;
                                px,py: Double; mk: PLineMarker1): Integer;
var
  dx,dy,dist,x,y,r,t,eps,min_y,max_y: Double;
  i,j,acc: int; p,p1,p2: TPoint; a,b: TGauss;
begin
  Result:=-1; eps:=_rPolygon_eps;

  if Assigned(mk) then begin
    mk^:=LineMarker1(0,p,0);
    mk.err:=100
  end;

  p2:=lp[0]; p.X:=Round(px); p.Y:=Round(py);

  if Int64(p) = Int64(p2) then
    Result:=0
  else begin
    acc:=0; dist:=0;

    b.x:=p2.X; b.y:=p2.Y;

    for i:=1 to lp_n do begin

      p1:=p2; p2:=lp[i]; a:=b;
      b.x:=p2.X; b.y:=p2.Y;

      dx:=b.x-a.x; dy:=b.y-a.y;

      min_y:=a.y; max_y:=b.y;
      if min_y > max_y then begin
        min_y:=b.y; max_y:=a.y;
      end;

      if Int64(p) = Int64(p2) then
        Result:=0
      else

      if a.y = b.y then begin
        if Abs(py-a.y) <= 0.5 then

        if px >= Min(a.x,b.x) then
        if px <= Max(a.x,b.x) then
        Result:=0

      end else

      if py >= min_y then
      if py <= max_y then begin

        x:=(py-a.y)/dy*dx + a.x;
        t:=Abs(x-px);

        if t <= 0.5 then
          Result:=0
        else begin
          if Abs(dx) > Abs(dy) then begin
            y:=(px-a.x)/dx*dy + a.y; t:=Abs(y-py);
            if t <= 0.5 then Result:=0;
          end;

          if t <= 1 then begin
            r:=Abs( xDist_to_Line(px,py, a.x,a.y,b.x,b.y) );

            if r <= eps then
              Result:=0
            else
            if Assigned(mk) then
            if r < mk.err then begin
              mk.Ind:=i-1; mk.err:=r
            end
          end;

          if py < max_y then
          if px < x then Inc(acc)
        end
      end;

      if Result = 0 then begin

        if Assigned(mk) then begin

          j:=i;
          if Int64(p) = Int64(p1) then Dec(j) else
          if Int64(p) <> Int64(p2) then j:=-j;
          dist:=dist + Long_Dist(p1,p);

          mk^:=LineMarker1(j,p,dist)
        end;

        Break
      end;

      if Assigned(mk) then
      dist:=dist + Hypot(dx,dy);
    end;

    if Result < 0 then
    if Odd(acc) then Result:=1
  end
end;

function _rPolygonContainsTest(lp: PLPoly; lp_n: int): int;
var
  i,rc: int; a,b: TPoint; cx,cy: double;
begin
  Result:=0; b:=lp[0];
  for i:=1 to lp_n do begin
    a:=b; b:=lp[i];
    cx:=a.x/2+b.x/2; cy:=a.y/2+b.y/2;
    rc:=_rPolygonContainsPixel(lp,lp_n, cx,cy, nil);
    if rc <> 0 then begin
      rc:=_rPolygonContainsPixel(lp,lp_n, cx,cy, nil);
      Result:=-i; Break
    end
  end
end;

function rPolygonContainsPixel(lp: PLPoly; lp_n: Integer;
                               px,py: Double): Integer;
begin
  Result:=_rPolygonContainsPixel(lp,lp_n, px,py, nil)
end;

function xPolygonContainsPixel(lp: PLPoly; lp_n: Integer;
                               px,py: Double): Integer;
var
  p, p1,p2: TPoint; x,y: Double;
  i,acc, dx,dy, min_y,max_y: Integer;
begin
  Result:=-1;

  p.X:=Round(px); p.Y:=Round(py);

  acc:=0; p2:=lp[0];

  if Int64(p) = Int64(p2) then begin
    Result:=0; Exit
  end else

  for i:=1 to lp_n do begin
    p1:=p2; p2:=lp[i];

    min_y:=Min(p1.Y,p2.Y);
    max_y:=Max(p1.Y,p2.Y);

    dx:=p2.X-p1.X; dy:=p2.Y-p1.Y;

    if Int64(p) = Int64(p2) then begin
      Result:=0; Exit
    end else

    if p1.Y = p2.Y then begin
      if p.Y = p1.Y then

      if p.X >= Min(p1.X,p2.X) then
      if p.X <= Max(p1.X,p2.X) then begin
        Result:=0; Exit
      end
    end else

    if p.Y >= min_y then

    if p.Y < max_y then begin

      x:=(py-p1.Y)/dy*dx + p1.x;
      if Round(x) = p.X then begin
        Result:=0; Exit
      end else
      if p.x >= Min(p1.X,p2.X) then
      if p.x <= Max(p1.X,p2.X) then
      if dx <> 0 then begin
        y:=(px-p1.X)/dx*dy + p1.Y;
        if Round(y) = p.Y then begin
          Result:=0; Exit
        end
      end;

      if p.X < x then Inc(acc)

    end else

    if p.Y = max_y then

    if p.Y <> p1.Y then begin

      x:=(p.Y-p1.Y)/dy*dx + p1.X;
      if Round(x) = p.X then begin
        Result:=0; Exit
      end
      else
      if dx <> 0 then begin
        y:=(p.X-p1.X)/dx*dy + p1.Y;
        if Round(y) = p.Y then begin
          Result:=0; Exit
        end
      end

    end else
    if dx <> 0 then begin
      y:=(p.X-p1.X)/dx*dy + p1.Y;
      if Round(y) = p.Y then begin
        Result:=0; Exit
      end
    end

  end;

  if Odd(acc) then Result:=1
end;

function PolygonContainsPixel(lp: PLPoly; lp_n: Integer;
                              px,py: Double): Integer;
var
  lt,rb: TPoint;
begin
  Result:=-1;

  Max_Poly_Bound(lp,lp_n+1,lt,rb);

  if (py >= lt.Y) and (py <= rb.Y) then
  if (px >= lt.X) and (px <= rb.X) then

  Result:=xPolygonContainsPixel(lp,lp_n, px,py)
end;

function PolygonContainsPoint(lp: PLPoly; lp_n: Integer;
                              const p: TPoint): Integer;
begin
  Result:=PolygonContainsPixel(lp,lp_n, p.x,p.y)
end;

function xPolygonContainsPoint(lp: PLLine;
                               const p: TPoint): Integer;
begin
  with lp^ do
  Result:=PolygonContainsPixel(@Pol,N,p.X,p.Y)
end;

function xPolygonContainsContour(lp: PLPoly; lp_N: Integer;
                                 cp: PLPoly; Count: Integer): Boolean;
var
  i,rc: int; lt,rb,p: TPoint;
begin
  Result:=false;

  if lp_N > 0 then
  if LPoly_Lock(lp,lp_N) then

  if Count > 0 then begin Result:=false;

    Max_Poly_Bound(lp,lp_N+1,lt,rb);

    for i:=0 to Count-1 do begin

      p:=cp[i];
      if (p.X < lt.X) or (p.X > rb.X)
      or (p.Y < lt.Y) or (p.Y > rb.Y) then begin
        Result:=false; Break
      end
      else begin
        rc:=_rPolygonContainsPixel(lp,lp_N,p.X,p.Y,nil);
        if rc < 0 then begin Result:=false; Break end else
        if rc = 1 then Result:=true
      end
    end
  end
end;

function xLPoly_Locate(lp: PLPoly; lp_n: Integer;
                       const lt,rb: TPoint; scan: Boolean;
                       out ind: Integer; out lp_p: TPoint): Boolean;
var
  i: Integer; p1,p2,a,b,p: TPoint;
begin
  Result:=false;

  if Assigned(lp) then

  for i:=0 to lp_N do begin
    p1:=p2; p2:=lp[i];

    if PortContainsPoint(lt,rb, p2.X,p2.Y) then begin
      ind:=i; lp_p:=p2; Result:=true; Break
    end else
    if scan and (i > 0) then begin
      a:=p1; b:=p2;
      if a.X > b.X then iSwap(a.X,b.X);
      if a.Y > b.Y then iSwap(a.Y,b.Y);

      if (a.X <= rb.X) and (b.X >= lt.X) then
      if (a.Y <= rb.Y) and (b.Y >= lt.Y) then

      if Locate_Line(p1,p2, lt,rb, p) then begin
        ind:=-i; lp_p:=p; Result:=true
      end
    end
  end
end;

function xLLine_Locate(lp: PLLine;
                       const lt,rb: TPoint; scan: Boolean;
                       out ind: Integer; out lp_p: TPoint): Boolean;
begin
  Result:=xLPoly_Locate(@lp.Pol,lp.N,
                        lt,rb,scan, ind,lp_p)
end;

function LPoly_Locate(lp: PLLine; const lt,rb: TPoint): Integer;
var
  i: Integer; p: TPoint;
begin
  Result:=-1;
  if xLPoly_Locate(@lp.Pol,lp.N, lt,rb,false, i,p) then
  Result:=i
end;

function LPoly_Indexof(lp: PLPoly; lp_n: Integer;
                       const p: TPoint): Integer;
var
  i: Integer;
begin
  Result:=-1;
  if Assigned(lp) then
  for i:=0 to lp_N do
  if Points_Equal(lp[i],p) then
  begin Result:=i; Break end
end;

function xPortContainsPolyLine(const lt,rb: TPoint;
                               lp: PLPoly; lp_N: Integer): Integer;
var
  i: Integer; p,q,c: TPoint; isCross: Boolean;
begin
  Result:=-1; isCross:=false;

  if lp_N >= 0 then begin
    p:=lp[0]; for i:=0 to lp_N do begin
      q:=p; p:=lp[i];

      if PortContainsPoint(lt,rb, p.X,p.Y) then
        Inc(Result)
      else
      if Result < 0 then
      if (i > 0) and not isCross then
      isCross:=Locate_Line(p,q, lt,rb, c)
    end
  end;

  if Result < 0 then
  if isCross then
  Result:=0
end;

function PortContainsPolyLine(const lt,rb: TPoint; lp: PLLine): Integer;
begin
  Result:=xPortContainsPolyLine(lt,rb,@lp.Pol,lp.N)
end;

function RingContainsPolyLine(c: TPoint; r: Double; lp: PLLine): Integer;
var
  i: Integer;
begin
  Result:=-1; with lp^ do

  for i:=0 to N do with Pol[i] do
  if RingContainsPoint(c.x,c.y, r, x,y) then
  Inc(Result)
end;

function PolyLineContainsPort(lp: PLLine; const lt,rb: TPoint): Boolean;
var
  i: Integer; p,q,c: TPoint;
begin
  Result:=false;

  with lp^ do begin
    p:=Pol[0]; for i:=0 to N do begin
      q:=p; p:=Pol[i];

      if PortContainsPoint(lt,rb, p.x,p.y) then begin
        Result:=true; Break
      end else
      if i > 0 then
      if Locate_Line(p,q, lt,rb, c) then begin
        Result:=true; Break
      end
    end
  end;
end;

function IsInnerPoly(lp1: PLPoly; n1: int;
                     lp2: PLPoly; n2: int): bool;
var
  i: int;
begin
  Result:=LPoly_Lock(lp1,n1);
  if Result then for i:=0 to n2 do

  with lp2[i] do
  if _rPolygonContainsPixel(lp1,n1,X,Y,nil) < 0 then
  begin Result:=false; Break end
end;

function PolygonContainsPoly(lp1: PLPoly; n1: Integer;
                             lp2: PLPoly; n2: Integer): Integer;
var
  i: Integer;
begin
  Result:=-1;
  if LPoly_Lock(lp1,n1) then for i:=0 to n2 do
  if PolygonContainsPoint(lp1,n1,lp2[i]) >= 0 then
  Inc(Result)
end;

function PolygonContainsPoints(lp1: PLLine;
                               lp2: PLPoly; n2: Integer): Integer;
var
  i: Integer;
begin
  Result:=-1;
  if PolyLock(lp1) then for i:=0 to n2 do
  if PolygonContainsPoint(@lp1.Pol,lp1.N,lp2[i]) >= 0 then
  Inc(Result)
end;

function PolygonContainsPolyLine(lp1,lp2: PLLine): Integer;
var
  i: Integer;
begin
  Result:=-1;
  if PolyLock(lp1) then
  for i:=0 to lp2.N do with lp1^ do
  if PolygonContainsPoint(@Pol,N,lp2.Pol[i]) >= 0 then
  Inc(Result)
end;

function lPolygonContainsPolyLine(lp1: PLPoly; n1: int;
                                  lp2: PLPoly; n2: int): Boolean;
var
  i,rc,k: int; p,c,lt,rb: TPoint; InSide: Boolean;
begin
  Result:=false;

  if LPoly_Lock(lp1,n1) then begin

    k:=-1; InSide:=false;

    Max_Poly_Bound(lp1,n1+1,lt,rb);

    for i:=0 to n2 do begin

      p:=lp2[i]; rc:=-1;

      if (p.Y >= lt.Y) and (p.Y <= rb.Y) then
      if (p.X >= lt.X) and (p.X <= rb.X) then
      rc:=_rPolygonContainsPixel(lp1,n1,p.X,p.Y,nil);

      if rc >= 0 then Inc(k); if rc = 1 then InSide:=true
    end;

    if k = n2 then

    if InSide then
      Result:=true
    else
    if not LPoly_Lock(lp2,n2) then begin

      Result:=true; for i:=0 to n2-1 do
      if Middle_Point(lp2[i],lp2[i+1],c) then
      if PolygonContainsPoint(lp1,n1,c) < 0 then
      Result:=false

    end else
    for i:=0 to n2-2 do begin
      c:=TriangleCentre(lp2[i],lp2[i+1],lp2[i+2]);
      if PolygonContainsPoint(lp2,n2,c) > 0 then
      if PolygonContainsPoint(lp1,n1,c) > 0 then
      begin Result:=true; Break end
    end
  end
end;

function xPolygonContainsPolyLine(lp1,lp2: PLLine): Boolean;
begin
  Result:=lPolygonContainsPolyLine(@lp1.Pol,lp1.N,
                                   @lp2.Pol,lp2.N)
end;

function PolygonContainsPolygon(lp1: PLPoly; n1: int;
                                lp2: PLPoly; n2: int;
                                bound: int): bool;
var
  i: int; c: TPoint;
begin
  Result:=false;

  if LPoly_Lock(lp1,n1) then
  if LPoly_Lock(lp2,n2) then begin

    for i:=0 to n2 do with lp2[i] do
    if _rPolygonContainsPixel(lp1,n1, X,Y,nil) >= bound then
      Result:=true
    else begin
      Result:=false; Break
    end;

    if Result then begin
      Result:=false;
      for i:=0 to n2-2 do begin
        c:=TriangleCentre(lp2[i],lp2[i+1],lp2[i+2]);
        if PolygonContainsPoint(lp2,n2,c) > 0 then
        if PolygonContainsPoint(lp1,n1,c) > 0 then
        begin Result:=true; Break end
      end
    end
  end
end;

function xPolygonContainsPolygon(lp1,lp2: PLLine; bound: int): bool;
begin
  Result:=PolygonContainsPolygon(@lp1.Pol,lp1.N, @lp2.Pol,lp2.N, bound)
end;

function PolygonClipPolygon(lp1: PLPoly; n1: int;
                            lp2: PLPoly; n2: int): bool;
var
  i: int; p,c,lt,rb: TPoint;
begin
  Result:=false;

  if LPoly_Lock(lp1,n1) then
  if LPoly_Lock(lp2,n2) then begin

    Max_Poly_bound(lp1,n1,lt,rb);

    for i:=0 to n2 do begin p:=lp2[i];
      if _rPolygonContainsPixel(lp1,n1, p.X,p.Y,nil) = 1 then begin
        Result:=true; Break
      end;

      if i > 1 then begin
        c:=TriangleCentre(lp2[i-2],lp2[i-1],p);

        if PortContainsPoint(lt,rb, c.X,c.Y) then
        if _rPolygonContainsPixel(lp1,n1, c.X,c.Y,nil) = 1 then
        if _rPolygonContainsPixel(lp2,n2, c.X,c.Y,nil) = 1 then begin
          Result:=true; Break
        end
      end
    end
  end
end;

function IsHolePolygon(lp1: PLPoly; n1: Integer;
                       lp2: PLPoly; n2: Integer): Boolean;
var
  i,rc: int; c: TPoint; InSide: Boolean;
begin
  Result:=false;

  if LPoly_Lock(lp1,n1) then
  if LPoly_Lock(lp2,n2) then begin

    Result:=true; InSide:=false;
    for i:=0 to n2 do begin
      rc:=PolygonContainsPoint(lp1,n1,lp2[i]);
      if rc < 0 then begin Result:=false; Break end;
      if rc = 1 then InSide:=true
    end;

    if Result and not Inside then begin
      Result:=false;
      for i:=0 to n2-2 do begin
        c:=TriangleCentre(lp2[i],lp2[i+1],lp2[i+2]);
        if PolygonContainsPoint(lp2,n2,c) > 0 then
        if PolygonContainsPoint(lp1,n1,c) > 0 then
        begin Result:=true; Break end
      end
    end
  end
end;

function LPoly_Insert(lp: PLLine;
                      lp_z: PIntegers;
                      ind,lp_max: Integer;
                      const v: vpoint): Integer;
var
  i: Integer; p: TPoint;
begin
  if Assigned(lp) then

  with lp^ do
  if N < Abs(lp_max) then begin

    Inc(ind); p.X:=v.x; p.Y:=v.y;

    Inc(N); if ind < N then
    for i:=N downto ind+1 do begin
      Pol[i]:=Pol[i-1];
      if Assigned(lp_z) then
      lp_z[i]:=lp_z[i-1]
    end;

    Pol[ind]:=p;
    if Assigned(lp_z) then lp_z[ind]:=v.z;
    if lp_max < 0 then Dec(ind);
  end;

  Result:=ind
end;

function zPoly_Insert(lp: PLLine;
                      lp_z: PIntegers;
                      ind,lp_max: Integer;
                      const p: TPoint;
                      z: Integer): Integer;
var
  v: VPoint;
begin
  v.x:=p.x; v.y:=p.y; v.z:=z;
  Result:=LPoly_Insert(lp,lp_z,ind,lp_max,v)
end;

function Poly_Insert(lp: PLLine;
                     ind,lp_max: Integer;
                     const p: TPoint): Integer;
var
  v: VPoint;
begin
  v.x:=p.x; v.y:=p.y; v.z:=0;
  Result:=LPoly_Insert(lp,nil,ind,lp_max,v)
end;

function xLPoly_Insert(lp: PLLine;
                       lp_z: PIntegers;
                       ind,lp_max: Integer;
                       const v: vpoint): Integer;
var
  i: integer; p: TPoint;
begin
  Result:=ind;

  with lp^ do begin
    p.x:=v.x; p.y:=v.y;

    i:=ind; if i >= 0 then
    if Points_Equal(Pol[i],p) then Exit;

    if lp_max > 0 then Inc(i) else Dec(i);
    if i >= 0 then if i <= N then
    if Points_Equal(Pol[i],p) then Exit;

    Result:=LPoly_Insert(lp,lp_z,ind,lp_max,v)
  end
end;

function xPoly_Insert(lp: PLLine;
                       ind,lp_max: Integer;
                       const p: TPoint): Integer;
var
  v: VPoint;
begin
  v.x:=p.x; v.y:=p.y; v.z:=0;
  Result:=xLPoly_Insert(lp,nil,ind,lp_max,v)
end;

function LPoly_Next(lp: PLLine; lp_Max: Integer;
                    const p: TPoint): Integer;
begin
  Result:=-1;

  with lp^ do begin

    if N < 0 then begin
      Pol[0]:=p; N:=0;
    end else
    if N < lp_Max then
    if not Points_Equal(Pol[N],p) then
    begin Inc(N); Pol[N]:=p end;

    Result:=N
  end
end;

function VPoly_Next(lp: PVLine; lp_Max: Integer;
                    x,y,z: Integer): Integer;
var
  v: VPoint;
begin
  with lp^ do begin

    if N < 0 then begin
      Pol[0]:=_VPoint(x,y,z); N:=0;
    end else
    if N < lp_Max then begin v:=Pol[N];

      if (x <> v.x) or (y <> v.y) or (z <> v.z) then
      begin Inc(N); Pol[N]:=_VPoint(x,y,z) end
    end;

    Result:=N
  end
end;

procedure LPoly_push(lp: PLLine; lp_Max: int; const p: TPoint);
begin
  with lp^ do if (N < 0)
  or not Points_Equal(Pol[N],p) then
  if N < lp_Max then begin
    Inc(N); Pol[N]:=p
  end
end;

function LPoly_Continue(lp: PLLine; lp_Max: Integer;
                        const p: TPoint): Integer;
var
  p1,p2: TPoint;
begin
  with lp^ do begin

    if N < 0 then begin
      Pol[0]:=p; N:=0;
    end else
    if N < lp_Max then begin

      p2:=Pol[N];
      if not Points_Equal(p2,p) then

      if N = 0 then begin
        Inc(N); Pol[N]:=p
      end
      else begin
        p1:=Pol[N-1];

        if ((p2.X-p1.X) <> (p.X-p2.X))
        or ((p2.Y-p1.Y) <> (p.Y-p2.Y)) then

          begin Inc(N); Pol[N]:=p end

        else Pol[N]:=p
      end
    end;

    Result:=N
  end
end;

function xLPoly_Continue(lp: PLLine; lp_Max: Integer;
                         const p: TPoint): Integer;

function LineContinue(const p1,p2,p: TPoint): Boolean;
begin
  Result:=((p2.x-p1.x) = (p.x-p2.x)) and
          ((p2.y-p1.y) = (p.y-p2.y))
end;

var
  p1,p2: TPoint;
begin
  with lp^ do begin

    if N < 0 then begin
      Pol[0]:=p; N:=0;
    end else
    if N < lp_Max then begin

      p2:=Pol[N];

      if (p.X <> p2.X)
      or (p.Y <> p2.Y) then

      if N = 0 then begin
        Inc(N); Pol[N]:=p
      end
      else begin
        p1:=Pol[N-1];

        if LineContainsPoint(p2,p,p1)
        or LineContinue(p1,p2,p) then
          Pol[N]:=p
        else
        if N < 2 then
          begin Inc(N); Pol[N]:=p end
        else
        if Points_Equal(Pol[N-2],p) then
          Dec(N,2)
        else
          begin Inc(N); Pol[N]:=p end
      end
    end;

    Result:=N
  end
end;

function LPoly_edge(lp: PLLine;
                    hp: PIntegers): Integer;
var
  I: Integer;
begin
  with lp^ do
  if N >= 0 then begin
    N:=dup_LPoly(@Pol,hp,N,false);

    for I:=0 to N-1 do begin
      Pol[I]:=Pol[I+1];
      if Assigned(hp) then hp[I]:=hp[I+1]
    end; N:=Max(-1,N-2);
  end;

  Result:=lp.N
end;

function LPoly_Delete(lp: PLLine;
                      lp_z: PIntegers;
                      Ind: int): int;
var
  i: int; lock: bool;
begin
  if Ind < 0 then Ind:=0;
  Result:=Ind-1;

  lock:=PolyLock(lp);
  with lp^ do if N >= 0 then begin

    if lock and (Ind = N) then Ind:=0;

    Dec(N);
    for i:=Ind to N do begin
      Pol[i]:=Pol[i+1];
      if Assigned(lp_z) then
      lp_z[i]:=lp_z[i+1]
    end;

    if lock then begin
      Pol[N]:=Pol[0];
      if Assigned(lp_z) then
      lp_z[N]:=lp_z[0]
    end;

    if lock then
    if N < 3 then Dec(N)
  end
end;

function Poly_Delete(lp: PLLine; ind: Integer): Integer;
begin
  Result:=LPoly_Delete(lp,nil,ind)
end;

function cut_lpoly(lp: PLPoly; hp: PIntegers;
                   lp_n,Ind,Count: int): int;
var
  i: int;
begin
  if Ind >= 0 then
  if Ind <= lp_n then
  if Count > 0 then begin
    Count:=Min(Count,lp_n-Ind+1);
    Dec(lp_n,Count);

    for i:=Ind to lp_n do begin
      lp[i]:=lp[i+Count];
      if Assigned(hp) then
      hp[i]:=hp[i+Count];
    end
  end;

  Result:=lp_n
end;

function LPoly_Cut(lp: PLLine; hp: PIntegers;
                   Ind,Count: Integer): Integer;
var
  i: Integer;
begin
  if Ind >= 0 then
  if Ind <= lp.N then
  if Count > 0 then begin
    Count:=Min(Count,lp.N-Ind+1);
    Dec(lp.N,Count);

    for i:=Ind to lp.N do begin
      lp.Pol[i]:=lp.Pol[i+Count];
      if Assigned(hp) then
      hp[i]:=hp[i+Count];
    end
  end;

  Result:=lp.N
end;

function Poly_Cut(lp: PLLine; Ind,Count: Integer): Integer;
begin
  Result:=LPoly_Cut(lp,nil,Ind,Count)
end;

function Poly_Paste(dst,lp: PLLine;
                    Ind,Count: Integer;
                    dst_Max: Integer): Boolean;
var
  i,dN,dst_N: Integer;
begin
  Result:=false;

  dst_N:=dst.N;
  dN:=lp.N + 1 - Count;

  if Count >= 0 then
  if dst_N + dN <= dst_Max then

  if ((Ind = dst_N+1) and (Count = 0))
  or ((Ind >= 0) and (Ind+Count <= dst_N+1)) then

  with dst^ do begin Inc(N,dN);

    if dN < 0 then
      for i:=Ind+Count to dst_N do
      Pol[i+dN]:=Pol[i]
    else
      for i:=dst_N downto Ind+Count do
      Pol[i+dN]:=Pol[i];

    for i:=0 to lp.N do begin
      Pol[Ind]:=lp.Pol[i]; Inc(Ind)
    end;

    Result:=true
  end
end;

function Poly_Add(dst,lp: PLLine;
                  dst_Max: Integer): Boolean;
begin
  Result:=Poly_Paste(dst,lp,dst.N+1,0,dst_Max)
end;

function LPoly_Append(dst_lp,src_lp: PLLine;
                      dst_z,src_z: PIntegers;
                      lp_max: int; eps: Double): bool;
var
  i,j: Integer;
  p1,p2,p3,p4,pc: TPoint;
  lock: Boolean;
begin
  Result:=false;

  if src_lp.N >= 0 then
  with dst_lp^ do if N < lp_max then begin

    if src_z = nil then dst_z:=nil;

    lock:=PolyLock(dst_lp); if lock then
    begin Dec(N); Dec(lp_max) end;

    j:=0;
    if eps > 0 then begin

      p2:=Pol[N]; p3:=src_lp.Pol[0];
      Middle_Point(p2,p3,pc);

      if Long_Dist(p2,p3) <= eps then j:=1;

      if eps > 0 then
      if (j = 0) and (N > 0) then
      if src_lp.N > 0 then begin
        p1:=Pol[N-1]; p4:=src_lp.Pol[1];
        if Join_Points(p1,p2,p3,p4,eps/2,pc) then
        j:=1
      end;

      if j = 1 then begin Pol[N]:=pc;

        if Assigned(dst_z) then
        dst_z[N]:=Round(dst_z[N]/2) +
                  Round(src_z[0]/2)
      end
    end;

    for i:=j to src_lp.N do
    if N < lp_max then begin
      Inc(N); Pol[N]:=src_lp.Pol[i];

      if Assigned(dst_z) then
      dst_z[N]:=src_z[i]
    end;

    if lock then begin
      Inc(N); Pol[N]:=Pol[0];
      if Assigned(dst_z) then
      dst_z[N]:=dst_z[0]
    end;

    Result:=true
  end
end;

function Poly_Append(dst,src: PLLine; max: Integer;
                     dist: Double): bool;
begin
  Result:=LPoly_Append(dst,src,nil,nil,max,dist)
end;

function xPoly_Append(dst,src: PLLine; ind,max: Integer): Integer;
var
  i: Integer;
begin
  Result:=ind; with src^ do for i:=0 to N do
  Result:=xPoly_Insert(dst,Result,max,Pol[i])
end;

function LPoly_cat(dst: PLLine; dst_Max: Integer;
                   src: PLPoly; Count: Integer;
                   dst_hp,src_hp: PIntegers): Boolean;
var
  cx: Integer;
begin
  Result:=false;

  if Count > 0 then if (dst_Max = 0)
  or (dst.N + Count <= dst_Max) then

  with dst^ do begin

    if dst_hp = nil then src_hp:=nil;
    if src_hp = nil then dst_hp:=nil;

    if N >= 0 then
    if Points_Equal(Pol[N],src[0]) then

    if (dst_hp = nil)
    or (dst_hp[N] <> src_hp[0]) then begin
      src:=@src[1]; Dec(Count)
    end;

    if Count > 0 then begin
      cx:=Count * Sizeof(TPoint);
      Move(src^,Pol[N+1],cx);

      if Assigned(dst_hp) then
      if Assigned(src_hp) then begin
        cx:=Count * Sizeof(Integer);
        Move(src_hp^,dst_hp[N+1],cx);
      end
    end;

    Inc(N,Count); Result:=true
  end
end;

procedure LPoly_add(dst,src: PLLine);
begin
  with src^ do
  LPoly_cat(dst,0,@Pol,N+1,nil,nil)
end;

function lp_trunc_top(lp: PLLine; hp: PIntegers;
                      Ind: int; const v: VPoint): bool;
var
  p: TPoint; i,j: int;
begin
  Result:=false;

  with lp^ do
  if N > 0 then begin p:=Point(v.x,v.y);

    if Ind = 1 then
    if Points_Equal(p,Pol[0]) then
    Dec(Ind);

    if Ind > 0 then begin
      j:=0; Pol[0]:=p;
      if Assigned(hp) then hp[0]:=v.z;
      if Points_Equal(p,Pol[Ind]) then Inc(Ind);

      for i:=Ind to N do begin
        Inc(j); Pol[j]:=Pol[i];
        if Assigned(hp) then hp[j]:=hp[i]
      end; lp.N:=j;

      Result:=true
    end
  end
end;

function lp_trunc_bot(lp: PLLine; hp: PIntegers;
                      Ind: int; const v: VPoint): bool;
var
  p: TPoint;
begin
  Result:=false;

  with lp^ do
  if N > 0 then begin p:=Point(v.x,v.y);

    if Ind = N then
    if Points_Equal(p,Pol[N]) then
    Inc(Ind);

    if Ind > 0 then
    if Ind <= N then begin
      N:=Ind; Pol[Ind]:=p;
      if Assigned(hp) then hp[Ind]:=v.z;
      Result:=true
    end
  end
end;

function lp_unlock(lp: PLLine; hp: PIntegers;
                   buf: PLPoly; buf_hp: PIntegers;
                   Ind: int; const v: VPoint): bool;
var
  p: TPoint; i,o: int;
begin
  Result:=false;

  with lp^ do
  if N > 0 then
  if (Ind >= 0) and (Ind <= N) then begin

    if buf_hp = nil then hp:=nil;

    p:=Point(v.x,v.y);

    o:=0; buf[o]:=p;
    if Assigned(hp) then
    buf_hp[o]:=v.z;

    if Ind = N then Ind:=0;
    if Points_Equal(p,Pol[Ind]) then Inc(Ind);
    Dec(N); if Ind > N then Ind:=0;

    i:=Ind; repeat
      Inc(o); buf[o]:=lp.Pol[i];
      if Assigned(hp) then buf_hp[o]:=hp[i];
      Inc(i); if i > N then i:=0;
    until i = Ind;

    for i:=0 to o do begin
      lp.Pol[i]:=buf[i];
      if Assigned(hp) then hp[i]:=buf_hp[o]
    end;

    lp.N:=o; Result:=o > 0
  end
end;

function lp_copy(src: PLLine; hp1: PIntegers;
                 dst: PLLine; hp2: PIntegers;
                 i1,i2: int; const v1,v2: VPoint): bool;
var
  i,j: int; p1,p2,p: TPoint;
begin
  Result:=false; dst.N:=-1;

  if src.N > 0 then begin

    if hp1 = nil then hp2:=nil;

    p1:=Point(v1.x,v1.y);
    p2:=Point(v2.x,v2.y);

    if (i1 >= 0) and (i1 <= i2) and (i2 <= src.N) then begin

      j:=0; dst.Pol[0]:=p1;
      if Assigned(hp2) then hp2[0]:=v1.z;

      for i:=i1 to i2 do begin
        p:=src.Pol[i];
        if not Points_Equal(p1,p) then begin
          Inc(j); dst.Pol[j]:=p;
          if Assigned(hp2) then hp2[j]:=hp1[i];
        end; p1:=p
      end;

      dst.Pol[j]:=p2; if j > 0 then
      if Points_Equal(dst.Pol[j-1],p2) then Dec(j);

      if Assigned(hp2) then hp2[j]:=v2.z;
      dst.N:=j; Result:=j > 0
    end
  end
end;

function Poly_Push(lp: PLLine; lp_max: Integer;
                   const p: TPoint; dist,cls: double): Integer;
var
  p1,p2: TPoint;
begin
  with lp^ do if N = -1 then begin
    N:=0; Pol[0]:=p
  end else
  if N < lp_Max then
  if not Points_Equal(Pol[N],p) then begin

    Inc(N); Pol[N]:=p; if N > 1 then begin

      p1:=Pol[N-2]; p2:=Pol[N-1];

      if (dist > 0)
      and (Long_Dist(p1,p2) < dist) then

        begin Pol[N-1]:=p; Dec(N) end

      else

      if (cls > 0)
      and (Abs(Dist_to_Line(p2,p1,p)) < cls) then

        begin Pol[N-1]:=p; Dec(N) end
    end
  end;

  Result:=lp.N
end;

function LPoly_Road(lp: PLPoly; lp_N: int;
                    lr: PDoubles; R1,R2: double;
                    Tube: int; lp1,lp2: PLLine): Integer;

procedure lp_continue(lp: PLLine; const p: TPoint);
begin
  if Assigned(lp) then
  with lp^ do begin Inc(N); Pol[N]:=p end
end;

function xcos(const a1,a2, b1,b2: TPoint): double;
begin
  Result:=d2_scalar(a2.X-a1.X,b2.X-b1.X,
                    a2.Y-a1.Y,b2.Y-b1.Y)
end;

function lp_next(lp: PLLine; var lpp: TPoint;
                 const a,b, p: TPoint): Boolean;
begin
  Result:=false;
  if Assigned(lp) then with lp^ do
  if xcos(lpp,a, Pol[N],p) > 0.1 then begin
    lp_Continue(lp,p); lpp:=a; Result:=true
  end
end;

var
  i: int; a,b,c,d, p1,p2, l1,l2: TPoint;
  r,rk,dist,len: double; lock: bool;
begin
  Result:=-1;

  if Assigned(lp1) then lp1.N:=-1;
  if Assigned(lp2) then lp2.N:=-1;

  if lp_N > 0 then begin
    b:=lp[0];
    while lp_N > 0 do begin
      a:=b; b:=lp[1];
      if (a.X <> b.X) or (a.Y <> b.Y) then Break;
      lp:=@lp[1]; Dec(lp_N)
    end
  end;

  if lp_N > 0 then begin

    lock:=LPoly_Lock(lp,lp_N);
    b:=lp[0]; c:=lp[1];

    rk:=0; len:=-1;
    if not IsNAN(R2) then begin
      len:=PolyLength(lp,lp_N);
      if len > 1 then rk:=(R2-R1)/len
    end;

    r:=R1;
    if Assigned(lr) then r:=lr[0];

    if lock then
      Get_Tube_Points(lp[lp_N-1],b,c, r,r, p1,p2)
    else
      Stretch_Point(b,c, r,r, p1,p2);

    if Tube > 0 then begin
      if Tube = 2 then p2:=p1; p1:=b
    end;

    lp_Continue(lp1,p1); l1:=b;
    lp_Continue(lp2,p2); l2:=b;

    dist:=0; i:=2;
    while i <= lp_N do begin
      d:=lp[i]; 
      if (d.X <> c.X) or (d.Y <> c.Y) then begin
        a:=b; b:=c; c:=d;

        r:=R1;
        if Assigned(lr) then r:=lr[i-1] else
        if len > 0 then begin
          dist:=dist+Long_Dist(a,b);
          r:=R1+dist*rk
        end;

        Get_Tube_Points(a,b,c, r,r, p1,p2);

        if Tube = 0 then
          lp_next(lp1,l1, b,c,p1)
        else begin
          if Tube = 2 then p2:=p1;
          lp_Continue(lp1,b); l1:=b
        end;

        lp_next(lp2,l2, b,c,p2)
      end;

      Inc(i)
    end;

    if lock then begin
      lp_Continue(lp1,lp1.Pol[0]);
      lp_Continue(lp2,lp2.Pol[0]);
    end
    else begin
      r:=R1;
      if Assigned(lr) then r:=lr[lp_N] else
      if len > 0 then begin
        dist:=dist+Long_Dist(b,c);
        r:=R1+dist*rk
      end;

      Stretch_Point(c,b, r,r, p2,p1);

      if Tube = 0 then
        lp_next(lp1,l1, c,c,p1)
      else begin
        if Tube = 2 then p2:=p1;
        lp_Continue(lp1,c);
      end;

      lp_next(lp2,l2, c,c,p2)
    end;

    if Assigned(lp1) then Result:=lp1.N else
    if Assigned(lp2) then Result:=lp2.N
  end;
end;

function LPoly_Tube(lp: PLPoly; lp_N: Integer;
                    lr: PDoubles; R: double; Tube: Integer;
                    dst: PLLine; dst_Max: Integer): Integer;

procedure Append_lp(lp, dst: PLLine; dst_Max: Integer);
var
  i: Integer;
begin
  for i:=0 to lp.N do
  LPoly_Continue(dst,dst_max,lp.Pol[i]);
end;

function Inside_lp(lp: PLPoly; lp_N: Integer; lp_: PLLine;
                   dst: PLLine; dst_Max: Integer): Boolean;
begin
  Result:=false;

  if PolygonContainsPoint(lp,lp_N,lp_.Pol[0]) > 0 then
  begin
    Append_lp(lp_, dst,dst_Max);
    Result:=dst.N > 0
  end
end;

function lp_Continue_lock(lp: PLLine): Boolean;
var
  c: TPoint;
begin
  with lp^ do if N >= 3 then
  if ILL2(Pol[1],Pol[0],Pol[N-1],Pol[N],c) then

  begin Pol[0]:=c; Pol[N]:=c end;

  Result:=PolyLock(lp)
end;

var
  lp1,lp2: PLLine; i,mode: Integer;
begin
  Result:=-1; dst.N:=-1;

  if Tube = 0 then R:=R/2;

  lp_N:=cls_LPoly(lp,lp_N,0,0.7);
  lp1:=Alloc_LLine(lp_N+1+lp_N+1+1);

  if lp_N > 0 then

  if Assigned(lr) or (R >= 1) then
  if lp_N+lp_N+2 <= dst_Max then

  if Assigned(lp1) then begin

    lp2:=@lp1.Pol[lp_N+1]; mode:=Tube and 3;

    if LPoly_Road(lp,lp_N, lr,R,NAN,mode, lp1,lp2) > 0 then
    begin

      if LPoly_Lock(lp,lp_N) then begin

        if lp_Continue_lock(lp1) then
        if lp_Continue_lock(lp2) then

        if Tube > 0 then
          Append_lp(lp2, dst,dst_max)
        else
        if Inside_lp(lp,lp_N,lp1,dst,dst_max) then
        else Inside_lp(lp,lp_N,lp2,dst,dst_max);

        if dst.N > 0 then
        LPoly_Continue(dst,dst_max,dst.Pol[0]);

      end else
      if Tube and 4 <> 0 then

        Append_lp(lp2, dst,dst_max)

      else begin
        for i:=0 to lp1.N do
        LPoly_Continue(dst,dst_max,lp1.Pol[i]);

        for i:=lp2.N downto 0 do
        LPoly_Continue(dst,dst_max,lp2.Pol[i]);

        if dst.N > 0 then
        LPoly_Continue(dst,dst_max,dst.Pol[0]);
      end;

      Result:=dst.N
    end
  end;

  xFreePtr(lp1);
end;

function LLine_Tube(lp: PLLine; R: double; Tube: Integer;
                    dst: PLLine; dst_Max: Integer): Integer;
begin
  Result:=LPoly_Tube(@lp.Pol,lp.N, nil,R,Tube, dst,dst_max)
end;

function Line_in_Poly(const a,b: TPoint; lp: PLLine;
                      out p: TPoint): Boolean;

function cross_Line(const a,b: TPoint; lp: PLLine;
                    out p: TPoint): Boolean;
var
  c: TPoint;
begin
  Result:=false; p:=a; c:=a;

  while true do begin
    c.x:=Round(c.x/2 + b.x/2);
    c.y:=Round(c.y/2 + b.y/2);

    if Abs(b.x-c.x) <= 1 then
    if Abs(b.y-c.y) <= 1 then Break;

    with lp^ do if PolygonContainsPoint(@Pol,N,c) >= 0 then
    begin p:=c; Result:=true; Break end
  end
end;

begin
  Result:=cross_Line(a,b,lp,p) or
          cross_Line(b,a,lp,p)
end;

function ClipLine(clip: PLLine; const a,b: TPoint;
                  out c,d: TPoint): Boolean;
var
  t1,t2,tc, p1,p2: TPoint; ic,i: integer;
begin
  Result:=false;

  t1:=a; t2:=b; ic:=0; c:=a; d:=b;

  if Line_in_Poly(t1,t2,clip,tc) then
  with clip^ do begin

    p1:=Pol[0]; for i:=1 to N do begin p2:=Pol[i];
      if i_Cross(tc,t1, p1,p2, c) then ic:=ic or 1;
      if i_Cross(tc,t2, p1,p2, d) then ic:=ic or 2;
      p1:=p2
    end
  end;

  if ic = 3 then
  Result:=not Points_Equal(c,d)
end;

function xClipLine(clip,lp: PLLine): Boolean;
var
  i,o: Integer; p1,p2,q1,q2,p: TPoint;
begin
  Result:=false;

  with lp^ do
  if N > 0 then begin
    p1:=Pol[0]; p2:=Pol[N];
    if ClipLine(Clip, p1,p2, q1,q2) then begin

      Pol[0]:=q1; o:=0;
      p1:=q1; p2:=q2; Swap_lPort(p1,p2);

      for i:=1 to lp.N-1 do begin p:=Pol[i];
        if xPolygonContainsPoint(clip,p) > 0 then
        begin Inc(o); Pol[o]:=p end
      end;

      Inc(o); Pol[o]:=q2
    end;

    lp.N:=o; Result:=o > 0
  end
end;

function LPoly_ldel(lp: PLLine;
                    hp: PIntegers;
                    i,k: Integer): bool;
var
  j: Integer;
begin
  Result:=false;

  if k > 0 then
  if k <= i then
  with lp^ do begin
    for j:=i to N do begin
      Pol[j-k]:=Pol[j];

      if Assigned(hp) then
      hp[j-k]:=hp[j];
    end;

    Dec(N,k); Result:=true
  end
end;

function Poly_ldel(lp: PLLine; i,k: int): bool;
begin
  Result:=LPoly_ldel(lp,nil, i,k)
end;

procedure Rotate_Poly(lp: PLLine; const a,b,c: TPoint);
var
  i: Integer;
begin
  with lp^ do for i:=0 to N do
  Rotate_LPoint(a,b,c, Pol[i],Pol[i])
end;

procedure Azimuth_Poly(lp: PLLine; const c: TPoint; fi: Double);
var
  i: Integer;
begin
  with lp^ do for i:=0 to N do
  Pol[i]:=Azimuth_Point(Pol[i], c,fi)
end;

procedure fi_Rotate_Poly(lp: PLLine; const c: TPoint; fi: Double);
var
  i: Integer;
begin
  with lp^ do for i:=0 to N do
  Pol[i]:=fi_Rotate_Point(Pol[i], c,fi)
end;

procedure Mirror_Poly(lp: PLLine; const a,b: TPoint);
var
  mirror: TFilter; dist: double;
  i: Integer; p: TPoint;
begin
  with mirror,lp^ do begin
    Dist_to_Line(p, a,b); if al >= 1 then begin

      for i:=0 to N do begin p:=Pol[i];
        dist:=Next_Dist(Pol[i]); dist:=dist+dist;
        Pol[i].x:=p.x - Round(dist*a1/al);
        Pol[i].y:=p.y - Round(dist*a2/al)
      end
    end
  end
end;

function Lock_enabled(lp: PLLine): Boolean;
var
  i: int; a,b,c,lt,rb, q,p,lt1,rb1: TPoint; cos: double;
begin
  Result:=true;
  with lp^ do if N > 1 then begin
    a:=Pol[0]; b:=Pol[N]; lt:=a; rb:=b;
    if lt.X > rb.X then iSwap(lt.X,rb.X);
    if lt.Y > rb.Y then iSwap(lt.Y,rb.Y);

    p:=Pol[1];
    for i:=2 to N-1 do begin
      q:=p; p:=Pol[i]; lt1:=q; rb1:=p;
      if lt1.X > rb1.X then iSwap(lt1.X,rb1.X);
      if lt1.Y > rb1.Y then iSwap(lt1.Y,rb1.Y);

      if (lt.X <= rb1.X) and (lt1.X <= rb.X) then
      if (lt.Y <= rb1.Y) and (lt1.Y <= rb.Y) then
      if LL2ic(a,b, q,p,c) then begin
        Result:=false; Break
      end
    end;

    if Result then begin
      Result:=int_cos3(Pol[N-1],Pol[N],Pol[0],1,cos);
      if Result then Result:=cos > 0
    end
  end
end;

function Lock_Poly(lp: PLLine; lp_Max: Integer): Boolean;
begin
  if not PolyLock(lp) then
  with lp^ do if N > 1 then begin
    if N < lp_Max then Inc(N); Pol[N]:=Pol[0]
  end; Result:=PolyLock(lp)
end;

function Lock_Curve(lp: PLLine; lp_Max: Integer): Boolean;
begin
  with lp^ do if N >= 3 then
  if not Points_Equal(Pol[0],Pol[N-1])
  or not Points_Equal(Pol[1],Pol[N]) then

  if N+2 <= lp_Max then begin
    Inc(N); Pol[N]:=Pol[0];
    Inc(N); Pol[N]:=Pol[1];
  end;

  Result:=CurveLock(lp)
end;

function Cross_PolyLines1(lp1: PLPoly; n1: Integer;
                          lp2: PLPoly; n2: Integer; lp_pc: PPoint;
                          out mk1,mk2: TLineMarker): Boolean;
var
  i,j,_i: int; r,_r: Double;
  p1,p2,q1,q2, pp: TPoint; c: TGauss;
begin
  Result:=false;

  Fillchar(mk1,Sizeof(mk1),0);
  Fillchar(mk2,Sizeof(mk2),0);

  if n1 > 0 then
  if n2 > 0 then begin

    p1:=lp1[0];
    for i:=1 to n1 do begin p2:=lp1[i];

      q1:=lp2[0];
      for j:=1 to n2 do begin q2:=lp2[j];

        if LL2icg(p1,p2, q1,q2, c) then begin

          pp.X:=Round(c.x); pp.Y:=Round(c.y);

          _i:=-i;
          if Abs(c.x-p1.X) < 0.7 then
          if Abs(c.y-p1.Y) < 0.7 then Dec(_i);
          if Abs(c.x-p2.X) < 0.7 then
          if Abs(c.y-p2.Y) < 0.7 then _i:=i;

          if Result then begin
            _r:=Long_Dist(pp,lp_pc^);
            if _r < r then begin
              mk1:=LineMarker(_i,pp);
              mk2:=LineMarker(j,pp);
              r:=_r
            end
          end
          else begin
            Result:=true;
            mk1:=LineMarker(_i,pp);
            mk2:=LineMarker(j,pp);
            if lp_pc = nil then Exit;
            r:=Long_Dist(pp,lp_pc^);
          end
        end;

        q1:=q2
      end;

      p1:=p2
    end
  end
end;

function Cross_PolyLines(lp1,lp2: PLLine; lp_pc: PPoint;
                         out mk1,mk2: TLineMarker): Boolean;
begin
  Result:=Cross_PolyLines1(@lp1.Pol,lp1.N,
                           @lp2.Pol,lp2.N,
                           lp_pc,mk1,mk2)
end;

function Ellipse_Bound(lp: PLLine; out R: LOrient): Boolean;
var
  c,t, p1,p2,p3: TPoint; dx,dy: Integer; a: double;
begin
  Result:=false; with lp^ do

  if N >= 0 then begin
    c:=Pol[0]; Result:=true;
    p1:=c; Dec(p1.x); dx:=0;
    p2:=c; Inc(p2.x); dy:=1;

    if N > 0 then begin
      a:=Long_Dist(c,Pol[1]); if a > 0 then begin

        p1:=c; Dec(p1.x,Round(a));
        p2:=c; Inc(p2.x,Round(a));
        p3:=c; Dec(p3.y,Round(a));

        if N > 1 then
        if Ort_to_Line(Pol[2] ,Pol[1],Pol[0], t) then
        if Dist_to_Line(t ,Pol[1],Pol[0]) <> 0 then begin
          p2:=Pol[1]; p3:=t; Mirror_Point(p2,c,p1)
        end;

        dx:=p3.x-c.x; dy:=p3.y-c.y
      end
    end;

    R[0].x:=p1.x-dx; R[0].y:=p1.y-dy;
    R[1].x:=p1.x+dx; R[1].y:=p1.y+dy;
    R[2].x:=p2.x+dx; R[2].y:=p2.y+dy;
    R[3].x:=p2.x-dx; R[3].y:=p2.y-dy;
    R[4]:=R[0]
  end
end;

function Sector_LLine(lp: PLLine;
                      const p1,p2: TPoint;
                      df: Double): Integer;
var
  i,dn: Integer; p,q: TPoint; fi,dr: Double;
begin
  lp.N:=-1;

  fi:=iAngle(p1,p2);
  dr:=Long_Dist(p1,p2);

  if dr > 1 then
  with lp^ do begin
    N:=0; Pol[0]:=p1; q:=p1;

    dn:=Round(df/Pi*180); fi:=fi - df/2;
    if dn > 128 then n:=n div 2;

    for i:=0 to dn do begin
      p:=prj_LPoint(p1,dr,fi + i*df/dn);
      if (p.x <> q.x) or (p.y <> q.y) then
      begin Inc(N); Pol[N]:=p end; q:=p
    end;

    Inc(N); Pol[N]:=p1;
    if N = 1 then N:=-1;
  end;

  Result:=lp.N
end;

function CircleToLine(lp: PLPoly; cx,cy,r,eps: Double): Integer;

function x_prj(cx,cy,r,f: Double): TGauss;
var
  sin,cos: Extended;
begin
  SinCos(f, sin,cos);
  Result.x:=Round(cx + r*cos);
  Result.y:=Round(cy - r*sin);
end;

var
  i,k: int; df: Double; p: TPoint;
  p1,p2,p3: TGauss;
begin
  k:=16; df:=2*Pi/k;

  if eps > 0 then
  while true do begin
    Inc(k,k); df:=2*Pi/k;
    if k = 128 then Break;

    p1:=x_prj(cx,cy,r,0);
    p2:=x_prj(cx,cy,r,df/2);
    p3:=x_prj(cx,cy,r,df);

    if gDist_to_Line(p2, p1,p3) <= eps then Break;

    if k = 128 then Break
  end;

  for i:=0 to k-1 do begin

    with x_prj(cx,cy,r,df*i) do begin
      p.X:=Round(x); p.Y:=Round(y);
    end;

    lp[i]:=p;
  end;

  lp[k]:=lp[0]; Result:=k
end;

function Ellipse_LLine(lp: PLLine; lp_Max: Integer; eps: Double): Integer;

function x_prj(cx,cy: Integer; a,b,s,c,f: Double): TGauss;
var
  sin,cos: Extended; ix,iy: Double;
begin
  SinCos(f, sin,cos);
  ix:=a * cos; iy:=b * sin;

  Result.x:=cx + Round( c*ix + s*iy);
  Result.y:=cy - Round(-s*ix + c*iy);
end;

function x_step(cx,cy: Integer; a,b,s,c, eps: Double): Integer;
var
  p1,p2,p3: tgauss; df,r: Double;
begin
  Result:=16;

  eps:=Max(0.7,Min(a/16,eps));

  while Result < 256 do begin
    df:=Pi*2 / Result;

    p1:=x_prj(cx,cy, a,b,s,c,0);
    p2:=x_prj(cx,cy, a,b,s,c,df);
    p3:=x_prj(cx,cy, a,b,s,c,df/2);

    r:=xDist_to_Line(p3.x,p3.y, p1.x,p1.y,p2.x,p2.y);

    if Abs(r) <= eps then Break;
    Inc(Result,Result)
  end;
end;

var
  a,b, df, c,s: double;
  p1,p2,p: TPoint; i,k: Integer;
begin
  Result:=-1;

  with lp^ do if N > 0 then begin
    p1:=Pol[0]; p2:=Pol[1];
    a:=Long_Dist(p1,p2);

    if a > 0 then begin
      b:=a; if N = 2 then
      b:=Abs(Dist_to_Line(Pol[2], p1,p2));

      if b = 0 then begin
        Mirror_Point(p2,p1,p);
        Pol[0]:=p; Pol[1]:=p2;
        Result:=1
      end
      else begin
        c:=(p2.X-p1.X)/a; s:=(p2.Y-p1.Y)/a;

        while true do begin
          k:=x_step(p1.x,p1.y, a,b,s,c, eps);
          if k < lp_Max then Break; eps:=eps*2
        end;

        df:=2*Pi/k;

        for i:=0 to k-1 do begin

          with x_prj(p1.x,p1.y, a,b,s,c,df*i) do begin
            p.x:=Round(x); p.y:=Round(y);
          end;

          Inc(Result); lp.Pol[Result]:=p;
        end;

        Inc(Result);
        lp.Pol[Result]:=lp.Pol[0]
      end
    end
  end;

  lp.N:=Result;
  if not PolyLock(lp) then begin
    lp.N:=-1; Result:=-1
  end
end;

function xEllipse_LLine(const p: TPoint; r: Integer;
                        lp: PLLine; lp_Max: Integer;
                        eps: Double): Boolean;
begin
  Result:=false;

  lp.N:=1; lp.Pol[0]:=p; lp.Pol[1]:=p;
  Inc(lp.Pol[1].X,r);

  if Ellipse_LLine(lp,lp_Max,eps) > 0 then
  if PolyLock(lp) then Result:=true
end;

function Ellipse_Curve(lp,curve: PLLine): integer;
var
  i: integer; R: LOrient; a,b: TPoint;
begin
  Result:=-1;
  if Ellipse_Bound(lp,R) then
  with curve^ do begin

    a:=R[0]; for i:=1 to 4 do begin b:=R[i];
      Inc(Result); Middle_Point(a,b, Pol[Result]);
      Inc(Result); Pol[Result]:=b; a:=b
    end;

    Inc(Result); Pol[Result]:=Pol[0];
    Inc(Result); Pol[Result]:=Pol[1];

    curve.N:=Result
  end
end;

procedure move_Ellipse(lp: PLLine; i: Integer; const p: TPoint);
var
  j: Integer; dx,dy: longint; q: TPoint;
begin
  with lp^ do begin
    dx:=p.x-Pol[0].x; dy:=p.y-Pol[0].y;

    Pol[i]:=p; if i = 0 then
      for j:=1 to N do with Pol[j] do
      begin Inc(x,dx); Inc(y,dy) end
    else
    if N > 1 then
    if xOrt_to_Line(Pol[2] ,Pol[1],Pol[0], q) then
    Pol[2]:=q else Dec(N)
  end
end;

function LPoly_Sector(lp: PLPoly;
                      xc,yc: Integer;
                      r,f1,f2: Double): Integer;
var
  i,n,x,y: Integer; df,kf: Double; s,c: Extended;
begin
  df:=f2-f1;

  n:=Round(Abs(df)*r / 8);
  if n < 4 then n:=4;
  if n > 256 then n:=256;

  kf:=df/n;

  for i:=0 to n do begin
    SinCos(f1 + i*kf, s,c);

    x:=Round(xc + (c * r));
    y:=Round(yc + (s * r));

    lp[i]:=Point(x,y)
  end;

  Result:=n+1
end;

function LPoly_Sectorc(lp: PLPoly;
                       xc,yc: Integer;
                       r,f1,f2: Double): Integer;
begin
  lp[0]:=Point(xc,yc);
  Result:=LPoly_Sector(@lp[1],xc,yc,r,f1,f2);
  Inc(Result); lp[Result]:=lp[0]
end;

function LPoly_Sectorp(lp: PLPoly; lp_n: int): int;
var
  a,b,c: TPoint; r,f1,f2: Double;
begin
  Result:=-1;

  if lp_n = 2 then begin

    c:=lp[0]; a:=lp[1]; b:=lp[2];
    r:=Hypot(a.X-c.X,a.Y-c.Y);
    if r >= 4 then begin
      f1:=ArcTan2(a.Y-c.Y,a.X-c.X);
      f2:=ArcTan2(b.Y-c.Y,b.X-c.X);
      if f2 < f1 then f2:=f2 + 2*Pi;
      Result:=LPoly_Sectorc(lp, c.X,c.Y,r,f1,f2);
    end
  end
end;

function LPoly_Triangle(const a,b: TPoint; lp: PLLine): Integer;
var
  dx,x1,x2: longint;
begin
  dx:=(b.x-a.x) div 2;
  x1:=a.x+dx; x2:=b.x-dx;

  with lp^ do begin
    N:=0; Pol[0].x:=a.x; Pol[0].y:=b.y;
    N:=1; Pol[1].x:=x1; Pol[1].y:=a.y;

    if x2 > x1 then begin
      Inc(N); Pol[N].x:=x2; Pol[N].y:=a.y;
    end;

    Inc(N); Pol[N].x:=b.x; Pol[N].y:=b.y;
    Inc(N); Pol[N].x:=a.x; Pol[N].y:=b.y;
    Result:=N
  end
end;

function Poly_dist_Vector(lp: PLPoly; n: Integer;
                          dist,loc: double; dir: Boolean;
                          out v: lvector): Integer;
var
  i,dx,dy: Integer;
  k1,k2,rib: double;
  p1,p2,v1,v2: TPoint;
begin
  Result:=-1;

  v1:=lp[0]; v2:=v1;

  p1:=lp[0]; p2:=p1;
  dx:=0; dy:=0; rib:=0;
  if loc < 1 then loc:=10;

  for i:=1 to n do begin
    p1:=p2; p2:=lp[i];
    dx:=p2.X - p1.X;
    dy:=p2.Y - p1.Y;
    rib:=Hypot(dx,dy);

    dist:=dist-rib;

    if dist < 0 then begin
      Result:=i-1; Break
    end
  end;

  dist:=dist+rib;

  if (dx = 0) and (dy = 0) then
  begin dx:=1; rib:=1 end;

  k1:=dist/rib;
  k2:=(dist+loc)/rib;
  if dir and (k2 < 1) then k2:=1;

  v1.X:=p1.X + Round(k1*dx);
  v1.Y:=p1.Y + Round(k1*dy);
  v2.X:=p1.X + Round(k2*dx);
  v2.Y:=p1.Y + Round(k2*dy);

  if Result < 0 then Result:=n;

  if (v1.X = v2.X) and (v1.Y = v2.Y) then
  Inc(v2.X,10); v[0]:=v1; v[1]:=v2
end;

function Get_Poly_turns(lp: PLPoly; n: Integer;
                        turns: PFloats; rot: double): Integer;
var
  i,k,dx,dy: Integer;
  dist,rib,f1,f2: double;
  p1,p2: TPoint;
begin
  Result:=0;

  p1:=lp[0]; p2:=p1; k:=0; dist:=0;

  for i:=1 to n do begin
    p1:=p2; p2:=lp[i];
    dx:=p2.X - p1.X;
    dy:=p2.Y - p1.Y;

    if (dx <> 0) or (dy <> 0) then begin

      rib:=Hypot(dx,dy);

      f2:=Arctan2(dy,dx);
      while f2 < 0 do f2:=f2 + 2*Pi;

      Inc(k); if k >= 2 then
      if Abs(f2-f1) > rot then
      if Result <= 256 then begin
        turns[Result]:=dist;
        Inc(Result)
      end;

      f1:=f2; dist:=dist + rib
    end
  end;
end;

function Off_short_ribs(lp: PLPoly; n: int; len: double): int;
var
  i,j: int; rb: Double;
begin
  i:=0;
  while i < n do begin
    rb:=Long_dist(lp[i],lp[i+1]);
    if rb >= len then Inc(i) else
    if i = n-1 then begin
      lp[i]:=lp[n]; Dec(n); Break
    end
    else begin Dec(n);
      for j:=i+1 to n do lp[j]:=lp[j+1];
    end
  end; Result:=n
end;

function Poly_Vertex(lp: PLLine; const p: TPoint; out v: TPoint): int;
var
  i: int; dist,min: double;
begin
  Result:=-1; v:=Point(0,0);

  with lp^ do
  if N >= 0 then begin v:=Pol[0];

    min:=Long_Dist(Pol[0],p); Result:=0;

    for i:=1 to N do begin
      dist:=Long_Dist(Pol[i],p);
      if dist < min then begin
        min:=dist; v:=Pol[i]; Result:=i
      end
    end

  end
end;

type
  TPullMarker = record
    i,j: int; d: Double; p: TPoint
  end;

function xPlot_to_lp(lp: PLLine; i: int;
                     dst: PLLine; r: Double;
                     out m: TPullMarker): Boolean;

function lp_Cross(lp: PLLine; i: Integer;
                  const p_: TPoint): Boolean;
var
  j: Integer; p,p1,p2,c: TPoint;
begin
  Result:=false;

  p:=lp.Pol[i]; p2:=lp.Pol[0];

  for j:=1 to lp.N do begin
    p1:=p2; p2:=lp.Pol[j];

    if (i < j-1) and (i > j) then
    if i_Cross(p1,p2, p,p_, c) then begin
      Result:=true; Break
    end
  end
end;

var
  j: int; p,a,b,c,c1: TPoint;
  dist,len,tmp,cos: Double; lock: bool;
begin
  Result:=false; p:=lp.Pol[i];

  lock:=PolyLock(dst);

  m.i:=i; m.j:=-1; len:=0;
  m.d:=0; m.p:=dst.Pol[0];

  dist:=0; b:=dst.Pol[0];
  for j:=1 to dst.N do begin
    a:=b; b:=dst.Pol[j];

    if xPlot_to_Line(p ,a,b,r, c,tmp) then begin

      if Long_dist(a,c) < r then begin
        if lock or not Points_Equal(p,c) then
        c:=a; m.j:=j-1;
      end
      else begin
        if Long_dist(c,b) < r then
        if lock or not Points_Equal(p,c) then c:=b;
        dist:=dist+Long_Dist(a,c); m.j:=j;
      end;

      if (i = 0) and (lp.N > 0) then begin
        if Int_cos(p,lp.Pol[1], a,b, 1,cos) then
        if Abs(cos) < 0.9 then
        if LL2ic(p,lp.Pol[1], a,b, c1) then c:=c1;
      end else
      if (i = lp.N) and (lp.N > 0) then begin
        if Int_cos(p,lp.Pol[lp.N-1], a,b, 1,cos) then
        if Abs(cos) < 0.9 then
        if LL2ic(p,lp.Pol[lp.N-1], a,b, c1) then c:=c1;
      end;

      m.p:=c; m.d:=dist;
      Result:=true; Break
    end else
    
    if j > 1 then begin
      tmp:=Long_Dist(p,a); if tmp < r then
      if not Result or (tmp < len) then begin
        m.j:=j; m.p:=a; m.d:=dist;
        len:=tmp; Result:=true
      end
    end;

    dist:=dist+Long_Dist(a,b)
  end;

  if not Result then
  if Long_Dist(p,dst.Pol[0]) <= r then begin
    m.j:=1; m.p:=dst.Pol[0];
    m.d:=0; Result:=true
  end;

  if Result then begin
    Result:=not lp_Cross(lp,i,m.p);
  end
end;

function Edge_Pull(lp: PLPoly; dst: PLLine; r: Double): Boolean;
var
  m: TPullMarker; v: VLLine;
begin
  Result:=false;
  v.N:=1; v.Pol[0]:=lp[0]; v.Pol[1]:=lp[1];
  if xPlot_to_lp(@v,0,dst,r,m) then begin
    lp[0]:=m.p; Result:=true
  end
end;

function Poly_Pull(lp: PLLine; lp_Max: Integer;
                   dst: PLLine; r: Double): Boolean;

type
  tdir = (_unknown,_forw,_back);

procedure lp_Edit(lp: PLLine; i: Integer; const p: TPoint);
begin
  lp.Pol[i]:=p; if PolyLock(lp) then
  if i = 0 then lp.Pol[lp.N]:=p else
  if i = lp.N then lp.Pol[0]:=p
end;

function dst_Project(dst: PLLine; forw: Boolean;
                     const m1,m2: TPullMarker; r: Double;
                     lp: PLLine; lp_i: Integer): Boolean;

function xProject(p ,a,b: TPoint; r: Double): Boolean;
var
  c: TPoint;
begin
  Result:=false;
  if Abs(Dist_to_Line(p ,a,b)) <= r then

  if Plot_to_Line(p ,a,b, c)
  or (Long_Dist(p,a) <= r)
  or (Long_Dist(p,b) <= r) then

  Result:=true;
end;

var
  a,b: TPoint; j1,j2: Integer;
begin
  Result:=false;

  j1:=m1.j; j2:=m2.j;

  if j1 = j2 then
    Result:=(m1.d < m2.d) = forw
  else

  if PolyLock(dst)
  or ((j1 < j2) = forw) then begin

    a:=lp.Pol[lp_i-1]; b:=lp.Pol[lp_i];

    if not forw then begin
      Dec(j1); if j1 = 0 then j1:=dst.N;
      Dec(j2); if j2 = 0 then j2:=dst.N;
    end;

    Result:=true;
    while j1 <> j2 do begin

      if not xProject(dst.Pol[j1] ,a,b,r) then
      begin Result:=false; Break end;

      if forw then begin
        Inc(j1); if j1 > dst.N then j1:=1
      end
      else begin
        Dec(j1); if j1 = 0 then j1:=dst.N
      end;

      if j1 = j2 then Break;
    end
  end
end;

function Prolong(lp: PLLine; i: int;
                 dst: PLLine; r: Double;
                 var m1,m2: TPullMarker;
                 var dir: tdir): Boolean;
var
  m: TPullMarker;
begin
  Result:=false;

  if xPlot_to_lp(lp,i, dst,r, m) then

  if m1.i < 0 then begin
    m1:=m; Result:=true
  end else

  case dir of
_unknown:
    if Points_Equal(m1.p,m.p) then begin
      m2:=m1; m2.i:=i; Result:=true
    end else
    if m1.j = m.j then begin
      if m1.d < m.d then dir:=_forw else
      if m1.d > m.d then dir:=_back;
      m2:=m; Result:=true
    end else
    if dst_Project(dst,true, m1,m,r, lp,i) then
      begin m2:=m; dir:=_forw; Result:=true end
    else
    if dst_Project(dst,false, m1,m,r, lp,i) then
      begin m2:=m; dir:=_back; Result:=true end;

_forw:
    if Points_Equal(m2.p,m.p) then begin
      m2.i:=i; Result:=true
    end else
    if dst_Project(dst,true, m2,m,r, lp,i) then
    begin m2:=m; Result:=true end;


_back:
    if Points_Equal(m2.p,m.p) then begin
      m2.i:=i; Result:=true
    end else
    if dst_Project(dst,false, m2,m,r, lp,i) then
    begin m2:=m; Result:=true end
  end
end;

function dup_Point(dst: PLLine; lp_Max: Integer;
                   const m: TPullMarker): Integer;
begin
  Result:=m.j; with dst^ do

  if not Points_Equal(m.p,Pol[Result-1]) then
  if not Points_Equal(m.p,Pol[Result]) then

  Result:=Poly_Insert(dst,Result-1,lp_Max,m.p)+1
end;

procedure dup_Points(dst: PLLine; lp_Max: Integer;
                     const m1,m2: TPullMarker);
var
  t1,t2: TPullMarker; j: Integer;
begin
  t1:=m1; t2:=m2;

  if t1.d > t2.d then begin
    t1:=m2; t2:=m1
  end;

  j:=dup_Point(dst,lp_Max,t1);
  t2.j:=t2.j + j - t1.j;
  dup_Point(dst,lp_Max,t2)
end;

function Import(lp: PLLine; lp_i,lp_Max: Integer;
                const m1,m2: TPullMarker; dir: tdir;
                dst: PLLine; bound: bool): Integer;
var
  i, di, i1,i2, j1,j2: Integer;
begin
  i1:=m1.i; i2:=m2.i; j1:=m1.j; j2:=m2.j;

  lp_Edit(lp,i1,m1.p);
  lp_Edit(lp,i2,m2.p);

  if i1+1 < i2 then begin
    di:=i2-i1-1; for i:=i2 to lp.N do
    lp.Pol[i-di]:=lp.Pol[i]; Dec(lp.N,di);
  end else
  if i1 > i2 then begin
    for i:=i2 to i1 do lp.Pol[i-i2]:=lp.Pol[i];
    i1:=i1-i2; lp.N:=i1+1; lp.Pol[i1+1]:=lp.Pol[0]
  end;

  if dir = _back then begin
    Dec(j1); if j1 = 0 then j1:=dst.N;
    Dec(j2); if j2 = 0 then j2:=dst.N;
  end;

  if dir > _unknown then begin

    if j1 = j2 then
    if PolyLock(dst) then

    case dir of
  _back:
      if m1.d < m2.d then begin
        i1:=Poly_Insert(lp,i1,lp_Max,dst.Pol[j1]);
        Dec(j1); if j1 = 0 then j1:=dst.N
      end;
    end;

    while j1 <> j2 do begin

      i1:=Poly_Insert(lp,i1,lp_Max,dst.Pol[j1]);

      if dir = _forw then begin
        Inc(j1); if j1 > dst.N then j1:=1
      end
      else begin
        Dec(j1); if j1 = 0 then j1:=dst.N
      end;

      if j1 = j2 then Break;
    end
  end;

  if not bound then
  dup_Points(dst,lp_Max, m1,m2);

  Result:=lp_i + i1 + 1 - m2.i
end;

const
  swap_dir: array[tdir] of tdir = (_unknown,_back,_forw);

var
  i,j,_i,di: int; lock,bound: bool;
  m1,m2: TPullMarker; dir: tdir;
begin
  Result:=false;

  if lp.N >= 0 then
  if dst.N > 0 then begin

    bound:=lp_Max and $10000000 <> 0;
    lp_Max:=lp_Max and $fffffff;

    m1.i:=-1; m2.i:=-1; dir:=_unknown;

    lock:=PolyLock(lp);
    di:=0; if lock then di:=1;

    i:=0;
    while i <= lp.N-di do begin

      if not Prolong(lp,i,dst,r, m1,m2,dir) then begin
        if m1.i = 0 then
        if m2.i > 0 then
        if lock then begin
          _i:=Max(m1.i,m2.i)+1;

          dir:=swap_dir[dir];
          for j:=lp.N-1 downto _i do
          if not Prolong(lp,j,dst,r, m2,m1,dir) then
          Break; dir:=swap_dir[dir];
        end;

        if m2.i > 0 then begin
          i:=Import(lp,i,lp_Max, m1,m2,dir, dst,bound);
          Result:=true; dir:=_unknown;
          m1.i:=-1; m2.i:=-1;
        end else
        if m1.i >= 0 then begin
          lp.Pol[m1.i]:=m1.p;
          m1.i:=-1
        end
      end;

      Inc(i)
    end;

    if m2.i > 0 then begin
      i:=Import(lp,i,lp_Max, m1,m2,dir, dst,bound);
      Result:=true
    end else
    if m1.i >= 0 then begin
      lp.Pol[m1.i]:=m1.p;
      Result:=true
    end;

    if lock then
    with lp^ do Pol[N]:=Pol[0]
  end
end;

function Poly_Bound_Dist(lp1,lp2: PLLine): Double;
var
  i,j: Integer; dist: Double;
  p, q1,q2: TPoint;
begin
  Result:=-1;

  if lp1.N >= 0 then
  if lp2.N > 0 then

  for i:=0 to lp1.N do begin

    p:=lp1.Pol[i]; q2:=lp2.Pol[0];

    for j:=1 to lp2.N do begin
      q1:=q2; q2:=lp2.Pol[j];
      dist:=Abs(Dist_to_Line(p, q1,q2));

      if Result < 0 then Result:=dist else
      if dist < Result then Result:=dist
    end
  end
end;

function Project_to_Road(const p: TPoint;
                         lp: PLPoly; N: Integer; R: Double;
                         out q: TPoint; out Dist: Double): Integer;
var
  i: Integer; q1,q2,_p: TPoint;
  len, min_r,t: Double;
begin
  Result:=0;

  if N > 0 then begin
    q2:=lp[0]; len:=0;

    q:=lp[0]; Dist:=0;
    min_r:=Long_Dist(p,q2);

    for i:=1 to N do begin
      q1:=q2; q2:=lp[i];

      if not Points_Equal(q1,q2) then begin

        if not Plot_to_Line(p ,q1,q2, _p) then
        _p:=q2; t:=Long_Dist(p,_p);

        if t < min_r then begin
          min_r:=t; q:=_p;
          Dist:=len+Long_Dist(q1,_p);
          Result:=i;
        end;

        len:=len+Long_Dist(q1,q2)
      end
    end;

    if min_r <= R then begin

      if (Result = 0) and (Dist = 0) then begin
        if Project_to_Line(p ,lp[0],lp[1], _p) then
        begin Dist:=-Long_Dist(_p,lp[0]); q:=_p end;
        Result:=-1
      end else
      if (Result = N) and (Dist = len) then begin
        if Project_to_Line(p ,q1,q2, _p) then
        begin Dist:=len+Long_Dist(q2,_p); q:=_p end;
        Result:=-2;
      end

    end
    else Result:=0

  end
end;

function Poly_Bilding(lp: PLLine): Boolean;
var
  i: Integer; a,b,c: TPoint;
  dir,nxt: Double;
begin
  Result:=true;
  with lp^ do if N > 2 then begin
    b:=Pol[0]; c:=Pol[1]; i:=2;

    while i <= N do begin
      a:=b; b:=c; c:=Pol[i]; Inc(i);
      dir:=iOnLine(a,b,c);

      if Abs(dir) > Small then Break;
    end;

    while i <= N do begin
      a:=b; b:=c; c:=Pol[i]; Inc(i);
      nxt:=iOnLine(a,b,c);

      if Abs(nxt) > Small then
      if (nxt > 0) <> (dir > 0) then begin
        Result:=false; Break
      end
    end
  end
end;

function Sign_Polygon(lp: PLPoly; lp_N: int;
                      out pc: TPoint): Boolean;
var
  i: int; cx,cy: Double;
  p,c, p1,p2,p3: TPoint;
begin
  Result:=false; pc:=Point(0,0);

  if LPoly_Lock(lp,lp_N) then begin
    xCentre_Polygon(lp,lp_N, cx,cy);
    p:=Point(Round(cx),Round(cy));

    if lp_N <= 4 then
      Result:=true
    else

    if PolygonContainsPoint(lp,lp_N,p) > 0 then
      Result:=true
    else begin
      p2:=lp[0]; p3:=lp[1];

      for i:=2 to lp_N do begin
        p1:=p2; p2:=p3; p3:=lp[i];
        Middle_Point(p1,p3,c);

        if PolygonContainsPoint(lp,lp_N,c) > 0 then
        begin p:=c; Result:=true; Break end
      end
    end;

    pc:=p
  end
end;

function xSign_Polygon(lp: PLLine; out pc: TPoint): Boolean;
begin
  Result:=Sign_Polygon(@lp.Pol,lp.N,pc)
end;

procedure LPoly_Restore_z_axe(lp: PLLine; hp: PIntegers;
                              var V: VPoint; Ind: Integer);
var
  p: TPoint;
begin
  V.z:=0;

  if Assigned(hp) then
  if Abs(Ind) <= lp.N then

  if Ind >= 0 then
    V.z:=hp[Ind]
  else
  with lp^ do begin
    Ind:=Abs(Ind); p:=Point(V.x,V.y);
    V.z:=Round(z_axe_line(Pol[Ind-1],Pol[Ind],p,
                          hp[Ind-1],hp[Ind]))
  end
end;

function LPoly_Nearest_Vertex(lp: PLPoly; lp_N: Integer;
                              const p: TPoint): Integer;
var
  i: Integer; r,_r: Double;
begin
  Result:=0; r:=Long_Dist(p,lp[0]);

  for i:=1 to lp_N do begin
    _r:=Long_Dist(p,lp[i]); if _r < r then
    begin r:=_r; Result:=i end
  end
end;

function Min_Poly_Bound(lp: PLPoly; N: Integer;
                        out lt,rb: TPoint): Boolean;
var
  _lt,_rb,p: TPoint; L: LOrient;
  i,r,d,cod, dx,dy: Integer;
begin
  Max_Poly_Bound(lp,N, _lt,_rb);

  L[0]:=lp[LPoly_Nearest_Vertex(lp,N,Point(_lt.X,_rb.Y))];
  L[1]:=lp[LPoly_Nearest_Vertex(lp,N,Point(_lt.X,_lt.Y))];
  L[2]:=lp[LPoly_Nearest_Vertex(lp,N,Point(_rb.X,_lt.Y))];
  L[3]:=lp[LPoly_Nearest_Vertex(lp,N,Point(_rb.X,_rb.Y))];

  dx:=(_rb.X - _lt.X) div 16;
  dy:=(_rb.Y - _lt.Y) div 16;

  if (L[0].X-_lt.X) <= dx then
  if (L[1].X-_lt.X) <= dx then

  if (_rb.X-L[2].X) <= dx then
  if (_rb.X-L[3].X) <= dx then

  if (L[1].Y-_lt.Y) <= dy then
  if (L[2].Y-_lt.Y) <= dy then

  if (_rb.Y-L[0].Y) <= dy then
  if (_rb.Y-L[3].Y) <= dy then begin

    _lt.X:=Max(L[0].X,L[1].X);
    _rb.X:=Min(L[2].X,L[3].X);
    _lt.Y:=Max(L[1].Y,L[2].Y);
    _rb.Y:=Min(L[0].Y,L[3].Y)

  end;

  for i:=0 to N do begin p:=lp[i];

    if (p.X >= _lt.X) and (p.X <= _rb.X) then
    if (p.Y >= _lt.Y) and (p.Y <= _rb.Y) then begin

      r:=p.X-_lt.X; cod:=0;

      d:=_rb.X-p.X; if d < r then
      begin cod:=1; r:=d end;

      d:=p.Y-_lt.Y; if d < r then
      begin cod:=2; r:=d end;

      d:=_rb.Y-p.Y; if d < r then
      begin cod:=3; r:=d end;

      case cod of
    0:  _lt.X:=p.X;
    1:  _rb.X:=p.X;
    2:  _lt.Y:=p.Y;
    3:  _rb.Y:=p.Y;
      end
    end;
  end;

  lt:=_lt; rb:=_rb;

  Result:=(_lt.X < _rb.X) and
          (_lt.Y < _rb.Y)
end;

function Get_Poly_Diam(lp: PLPoly; lp_N: Integer;
                       out cp1,cp2: TPoint): Integer;
var
  i: Integer; si: PLPoly;
  cx,cy,r,t: Double; p1,p2,p: TPoint;
begin
  Result:=-1; cp1:=Point(0,0); cp2:=cp1;

  if lp_N > 0 then begin
    xCentre_Polygon(lp,lp_N, cx,cy);

    p1:=lp[0]; Result:=0;
    r:=Hypot(p1.x-cx,p1.y-cy);

    si:=@lp[1];
    for i:=1 to lp_N do begin
      p:=si[0]; si:=@si[1];
      t:=Hypot(p.x-cx,p.y-cy);
      if t > r then begin
        p1:=p; r:=t; Result:=i
      end;
    end;

    p2:=lp[0];
    r:=Hypot(p2.x-p1.x,p2.y-p1.y);

    si:=@lp[1];
    for i:=1 to lp_N do begin
      p:=si[0]; si:=@si[1];
      t:=Hypot(p.x-p1.x,p.y-p1.y);
      if t > r then begin p2:=p; r:=t end;
    end;

    cp1:=p1; cp2:=p2
  end
end;

function Get_Poly_Bound(lp: PLPoly; lp_N: Integer;
                        Bound: PLPoly; MinR: Integer): Integer;
var
  i: Integer; si: PLPoly;
  r,r1,r2: Double; p1,p2: TPoint;
begin
  Result:=0;

  if lp_N >= 0 then begin p1:=lp[0];

    if lp_N = 0 then
      Result:=Port_to_LPoly(p1.X-MinR,p1.Y-MinR,
                            MinR*2,MinR*2,Bound)
    else
    if Get_Poly_Diam(lp,lp_N, p1,p2) >= 0 then begin

      r1:=0; r2:=0; si:=lp;

      for i:=0 to lp_N do begin
        r:=Dist_to_Line(si[0],p1,p2);

        if r < 0 then r1:=Max(r1,Abs(r)) else
        if r > 0 then r2:=Max(r2,r);

        si:=@si[1]
      end;

      r1:=Max(MinR,r1+MinR/2);
      r2:=Max(MinR,r2+MinR/2);

      Get_Two_P_Points(p1,p2,r1,Bound[1],Bound[2]);
      Get_Two_P_Points(p1,p2,-r2,Bound[0],Bound[3]);
      Bound[4]:=Bound[0]; Result:=4
    end
  end
end;

function poly_quad(lp: PLPoly; lp_n: Integer): Boolean;
var
  p: TPoint;
begin
  Result:=false;

  if lp_N = 4 then begin
    if i_Cross(lp[0],lp[1],lp[2],lp[3],p) then
      Swap_LPoints(lp[1],lp[2])
    else
    if i_Cross(lp[0],lp[3],lp[1],lp[2],p) then
      Swap_LPoints(lp[2],lp[3]);

    if not i_Cross(lp[0],lp[1],lp[2],lp[3],p) then
    if not i_Cross(lp[0],lp[3],lp[1],lp[2],p) then
    Result:=true
  end
end;

function Is_poly_image(lp: PLLine; W,H: Integer): Boolean;
var
  i: Integer;
begin
  Result:=true;

  for i:=0 to lp.N do
  with lp.Pol[i] do
  if ((X > 0) and (X < W))
  or ((Y > 0) and (Y < H)) then begin
    Result:=false; Break
  end
end;

initialization
  _rPolygon_eps:=0.75;

end.

