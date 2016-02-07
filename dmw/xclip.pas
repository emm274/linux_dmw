unit xclip; interface

uses
  Classes,Math,otypes;

type
  TClipPoly = class
    constructor Create;
    destructor Destroy; override;

    function dst_Point(const p: TPoint): Boolean; virtual;
    procedure xyz_Point(const v: lxyz);

    procedure Draw_LPoly(lp: PLPoly; lp_n: Integer;
                         const lt,rb: TPoint;
                         fill: boolean);

    procedure Draw_LLine(lp: PLLine;
                         const lt,rb: TPoint;
                         fill: Boolean);

    procedure Cut_Polygon(lp,cut,clip,sum: PLLine; lp_max: Integer);

    procedure Clip_Polyline(lp,clip: PLLine; hp: PIntegers);

    function Cut_Polyline(lp,cut: PLLine; lp_max: Integer;
                          only_div: Boolean): Boolean;

    procedure dst_Dump(lp: PLLine);

  protected
    procedure lp_Dump(lp: PLLine); virtual;
    procedure lp_Clip(lp: PLLine); virtual;
    procedure lp_Cut(lp: PLLine); virtual;

    procedure Open_Child; virtual;
    procedure Close_Child; virtual;
    procedure Clear_Child; virtual;

  private
    cut_lp: PLPoly;
    cut_i: Integer;
    cut_n: Integer;

    dst_lp: PLLine;
    dst_hp: PIntegers;

    dst_lp1: PLLine;

    Clip_lt,Clip_rb: TPoint;
    Clip_R: array[0..4] of TPoint;

    fStack: array[1..16] of Integer;
    fStack_sp: integer;

    fEditFlag: Boolean;
    fFillFlag: Boolean;
    fIsXyz: Boolean;

    fOn_lp_Dump: TLLineProc;
    fOn_lp_Dump1: TLLineProc1;

    fOn_lp_Clip: TLLineProc;

    function Get_Plane(const p: TPoint;
                       old: Integer): Integer;

    procedure Stack_push(v: Integer);

    function cut_lp_Contains(const p: TPoint): Boolean;
    function Clip_v(i: Integer): TPoint;

    procedure lp_Dump1(lp: PLLine);

    function cut_polygon1(lp,cut: PLLine): bool;
    function add_polygon1(lp1,lp2: PLLine): bool;

  public
    property On_lp_Dump: TLLineProc write fOn_lp_Dump;
    property On_lp_Dump1: TLLineProc1 write fOn_lp_Dump1;
    property On_lp_Clip: TLLineProc write fOn_lp_Clip;

    property Stack_sp: Integer read FStack_sp
                               write FStack_sp;

    property EditFlag: Boolean read FEditFlag;
  end;

  TCutPoly = class(TClipPoly)
    constructor Create;
    destructor Destroy; override;

    function dst_Point(const p: TPoint): Boolean; override;

    procedure Open_Child; override;
    procedure Close_Child; override;
    procedure Clear_Child; override;

  private
    old_Stack_sp: Integer;

    Child_Enabled: Boolean;
    Child_lp: PLLine;
  end;

  TAlignPoly = class
    constructor Create(d,w: double);
    function Align_lp(Line,lp: PLLine; Max: Integer): Boolean;
    procedure Stretch(src_lp,dst_lp: PLLine); virtual;

  private
    _Line: PLLine; _r,_w: double;
    function mov_Point(p1,p2,pc: TPoint; var p: TPoint): Integer;
  end;

procedure Get_Max_PolyLine(lp: PLLine; w,h: longint);

function Dist1_to_PolyLine(lp: PLPoly; lp_N: Integer;
                           const p: TPoint): Double;

function Dist2_To_PolyLine(const p: TPoint; lp: PLLine;
                           var dist: double): Integer;

function Dist_To_Road(const p: TPoint; lp: PLLine;
                      var dist: double): Integer;

function Allign_PolyLine(lp,line: PLLine; d,w: double; max: Integer): Boolean;

function Dup_Polyline(lp,cut: PLLine; Max: Integer): boolean;

function Clip_Signs(lp,clip: PLLine; hp: PIntegers;
                    const lt,rb: TPoint): Integer;

implementation

uses
  xline,xpoly,xpoly1,xcutc;

const
  Cross_Max = 15;

  LPolyMax = 32000-1;

type
  TCross = record
    ic,rc: Integer;
    dist: double; p: TPoint
  end;

  TCrossArray = array[0..Cross_Max] of TCross;

function iCross(ic,rc: Integer;
                const p1,p: TPoint;
                var c: TCrossArray): boolean;
var
  i,j: int; dist: double; 
begin
  Result:=false; dist:=Long_Dist(p1,p);

  for i:=0 to Cross_Max do
  if dist < c[i].dist then begin
    for j:=Cross_Max downto i+1 do c[j]:=c[j-1];
    c[i].ic:=ic; c[i].rc:=rc; c[i].dist:=dist;
    c[i].p:=p; if i = 0 then Result:=true;
    Break
  end
end;

function back_Cross(const p2: TPoint;
                    const c: TCrossArray;
                    out p: TPoint): Integer;
var
  i: Integer; _p: TPoint;
  dist,temp: Double;
begin
  Result:=c[0].ic; p:=c[0].p;
  dist:=Long_Dist(p2,p);

  for i:=Cross_Max downto 1 do begin
    _p:=c[i].p; temp:=Long_Dist(p2,_p);
    if temp < dist then begin
      Result:=c[i].ic; p:=_p;
      dist:=temp;
    end
  end
end;

function Poly_Insert(lp: PLLine;
                     ind,lp_max: Integer;
                     const p: TPoint): Integer;
var
  i: Integer;
begin
  if Assigned(lp) then

  with lp^ do
  if N < lp_max then begin

    Inc(ind); i:=ind; if i > 0 then
    if Points_equal(Pol[i-1],p) then
    begin Dec(ind); i:=-1 end;

    if i >= 0 then begin

      if i <= N then
      if Points_equal(Pol[i],p) then i:=-1;

      if i >= 0 then begin

        Inc(N); if ind < N then
        for i:=N downto ind+1 do begin
          Pol[i]:=Pol[i-1];
        end;

        Pol[ind]:=p
      end
    end
  end;

  Result:=ind;
end;

function xCutContains(lp: PLLine; rc: Integer;
                      const p1,p2: TPoint): Integer;
var
  dx,dy: Integer; p: TPoint; k: Double;
begin
  dx:=p2.x-p1.x; dy:=p2.y-p1.y;

  k:=1/2; Result:=rc;

  if Result < 0 then begin
    Result:=xPolygonContainsPoint(lp,p2);

    if Result < 0 then begin
      rc:=xPolygonContainsPoint(lp,p1);
      if rc > 0 then Result:=1; Exit
    end
  end;

  while Result <= 0 do begin
    p.x:=p1.x + Round(dx*k);
    p.y:=p1.y + Round(dy*k);

    if Long_Dist(p1,p) < 2 then Break;

    rc:=xPolygonContainsPoint(lp,p);
    if rc <> 0 then Result:=rc;
    k:=k/2
  end
end;

// p1 - cross point; (i+1) - cross rib
function into_Cross(lp: PLLine; i: Integer; p1,p2: TPoint): Boolean;
var
  q1,q2,q3: TPoint; d1,d2: Double;
begin
  Result:=false; q2:=lp.Pol[i];

  if not Points_Equal(q2,p1) then
    Result:=true
  else
  if (i > 0) or (i < lp.N)
  or PolyLock(lp) then begin

    if i = 0 then q1:=lp.Pol[lp.N-1] else
    q1:=lp.Pol[i-1]; q3:=lp.Pol[i+1];

    d1:=Dist_to_Line(p2, q1,q2);
    d2:=Dist_to_Line(p2, q2,q3);

    if Abs(d1) > 0.5 then
    if Abs(d2) > 0.5 then

    if d2 < 0 then Result:=d1 < 0 else
    if d2 > 0 then Result:=d1 > 0
  end
end;

// 0 - касание [p2->]; 1 - пересечение [p1-p2]; 2 - вход [p1->]

function iPolyCross(lp: PLLine; const p1,p2: TPoint;
                    out c: TCrossArray; Inside: Boolean): Integer;
var
  lt,rb, p,q, t: TPoint; d: double;
  i,j,ic,rc, pred_i: int; c_: TCross;
begin
  Result:=-1; FillChar(c,SizeOf(c),0);

  Get_Next_Point(p1,p2,Long_Dist(p1,p2)*2, p);
  for i:=0 to Cross_Max do begin
    c[i].p:=p; c[i].rc:=-1
  end;

  with lp^ do if N > 0 then
  if not Points_Equal(p1,p2) then begin

    Swap_lRect(p1,p2, lt,rb); ic:=0; pred_i:=-1;

    for i:=0 to Cross_Max do
    c[i].dist:=Long_Dist(p1,p2)*2;

    p:=Pol[0]; for i:=1 to N do begin

      q:=p; p:=Pol[i]; if not Points_Equal(q,p) then
      if PortContainsLine(lt,rb, q,p) then begin

        if LineContainsPoint(q,p, p2) then begin

          if Points_Equal(q,p2) then
            j:=i-1
          else begin j:=-i;
            if Points_Equal(p,p2) then j:=i
          end;

          if LineContainsPoint(q,p, p1) then begin
            iCross(j,0,p1,p2,c); Result:=0; Break
          end else
          if not Points_Equal(p,p2) then begin
            if iCross(j,0,p1,p2,c) then begin

              if LineContainsPoint(p1,p2, p) then begin
                iCross(-i,0,p1,p,c); Inc(ic)
              end;

              Result:=0;
            end;

            Inc(ic)
          end
        end

        else
        if LineContainsPoint(p1,p2, p) then

        else
        if LineContainsPoint(q,p, p1) then begin
          if Points_Equal(q,p1) then j:=i-1 else
          if Points_Equal(p,p1) then j:=i else j:=-i;
          if iCross(j,2,p1,p1,c) then Result:=2;
          Inc(ic)
        end else

        if LineContainsPoint(p1,p2, q) then begin
          if into_Cross(lp,i-1, q,p2) then begin
            j:=i-1; if iCross(j,1,p1,q,c) then
            Result:=1; Inc(ic)
          end
        end
        else begin
          j:=-1;

          if i_Cross(p1,p2, q,p, t) then begin
            if Points_Equal(p,t) then pred_i:=i
            else j:=i
          end else
          if i = pred_i+1 then
            j:=pred_i;

          if j > 0 then
          if into_Cross(lp,j-1, t,p2) then begin
            if not Points_Equal(q,t) then j:=-i;
            if iCross(j,1,p1,t,c) then Result:=1;
            Inc(ic)
          end
        end

      end
    end
  end;

  if Result = 2 then begin

    if ic = 1 then
      rc:=xCutContains(lp,-1, p1,p2)
    else
      rc:=xCutContains(lp,0, p1,c[1].p);

    if Inside then begin
      if rc > 0 then Result:=1
    end
    else begin
      if rc < 0 then Result:=1
    end;

    if ic > 1 then
    if Result <> 1 then begin
      for i:=0 to Cross_Max-1 do c[i]:=c[i+1];
      FillChar(c_,SizeOf(c_),0); c_.rc:=-1;
      c[Cross_Max]:=c_; Result:=c[0].rc
    end;

    if Result > 0 then
    Result:=Result and 1
  end;
end;

constructor TClipPoly.Create;
var
  n: Integer;
begin
  inherited Create;

  n:=LPolyMax+1; dst_hp:=nil;
  dst_lp:=Alloc_LLine(n + (n div 2));
  if Assigned(dst_lp) then begin
    dst_hp:=@dst_lp.Pol[n];
    dst_lp.N:=-1;
  end;

  dst_lp1:=Alloc_LLine(n)
end;

destructor TClipPoly.Destroy;
begin
  dst_lp1:=xFreePtr(dst_lp1);
  dst_lp:=xFreePtr(dst_lp);
  inherited
end;
          
function TClipPoly.Dst_Point(const p: TPoint): Boolean;
begin
  Result:=false;

  with dst_lp^ do if (N = -1)
  or not Points_Equal(Pol[N],p) then

  if (N >= 1) and Points_Equal(Pol[N-1],p) then
    Dec(N)
  else begin
    if N < LPolyMax then Inc(N); Pol[N]:=p;

    if (N = LPolyMax) and not fFillFlag then begin
      lp_Dump1(dst_lp); Pol[0]:=Pol[N]; N:=0
    end;

    Result:=true
  end
end;

procedure TClipPoly.xyz_Point(const v: lxyz);
begin
  if Dst_Point(v.p) then
  dst_hp[dst_lp.N]:=v.v.z
end;

procedure TClipPoly.lp_Dump1(lp: PLLine);
begin
  if fIsXyz and Assigned(fOn_lp_Dump1) then
    fOn_lp_Dump1(lp,dst_hp)
  else
    lp_Dump(lp)
end;

procedure TClipPoly.lp_Dump(lp: PLLine);
begin
  if Assigned(fOn_lp_Dump) then
  fOn_lp_Dump(lp)
end;

procedure TClipPoly.lp_Clip(lp: PLLine);
begin
  if Assigned(fOn_lp_Clip) then
  fOn_lp_Clip(lp)
end;

procedure TClipPoly.lp_Cut(lp: PLLine); begin end;

procedure TClipPoly.Open_Child; begin end;
procedure TClipPoly.Close_Child; begin end;
procedure TClipPoly.Clear_Child; begin end;

function TClipPoly.cut_lp_Contains(const p: TPoint): Boolean;
begin
  Result:=false; if cut_lp <> nil then
  Result:=PolyGonContainsPoint(cut_lp,cut_n,p) > 0
end;

function TClipPoly.Clip_v(i: Integer): TPoint;
begin
  Result:=Clip_R[i]
end;

function TClipPoly.Get_Plane(const p: TPoint;
                             old: Integer): Integer;
begin
  Result:=-1;
  if old >= 0 then begin
    if old = 0 then begin
      if (p.x >= clip_lt.x) and (p.x <= clip_rb.x) and
         (p.y >= clip_lt.y) and (p.y <= clip_rb.y) then
      Result:=0
    end
    else begin
      if (p.x > clip_lt.x) and (p.x < clip_rb.x) and
         (p.y > clip_lt.y) and (p.y < clip_rb.y) then
      Result:=0
    end
  end;

  if Result < 0 then begin
    Result:=1; if p.x > clip_lt.x then begin
      Inc(Result); if p.y > clip_lt.y then begin
        Inc(Result); if p.x < clip_rb.x then Inc(Result)
      end
    end
  end
end;

procedure TClipPoly.Stack_push(v: Integer);
begin
  if v > 0 then

  if (FStack_SP = 0)
  or (FStack[FStack_SP] <> v) then begin
    Inc(FStack_SP); FStack[FStack_SP]:=v;
    dst_Point(clip_R[v])
  end
  else begin
    Dec(FStack_SP);
    Dec(dst_lp.N)
  end
end;

procedure TClipPoly.Draw_LPoly(lp: PLPoly; lp_n: Integer;
                               const lt,rb: TPoint;
                               fill: boolean);

function Alt_Plane(p: TPoint; alt: Integer): boolean;
begin
  Result:=false;
  case alt of
2:  Result:=p.y <= lt.y;
3:  Result:=p.x >= rb.x;
4:  Result:=p.y >= rb.y
  end
end;

function Get_Cross(p1,p2: TPoint; plane: Integer): TPoint;

function CrossX(x,y,dx,dy: longint): longint;
var
  t: double;
begin
  Result:=y;

  if dx <> 0 then begin t:=y + x * (dy/dx);
    if Abs(t) < MaxLongint then Result:=Round(t) else
    if t < 0 then Result:=-MaxLongint else Result:=MaxLongint
  end else
  if dy < 0 then Result:=-MaxLongint else
  if dy > 0 then Result:=MaxLongint
end;

function CrossY(x,y,dx,dy: longint): longint;
var
  t: double;
begin
  Result:=x;

  if dy <> 0 then begin t:=x + y * (dx/dy);
    if Abs(t) < MaxLongInt then Result:=Round(t) else
    if t < 0 then Result:=-MaxLongint else Result:=MaxLongint
  end else
  if dx < 0 then Result:=-MaxLongint else
  if dx > 0 then Result:=MaxLongint
end;

var
  c: TPoint; dx,dy: longint;

begin
  dx:=p2.x-p1.x; dy:=p2.y-p1.y;

  if plane and 1 > 0 then begin
    if plane = 1 then c.x:=lt.x
    else c.x:=rb.x;

    c.y:=CrossX(c.x-p1.x,p1.y,dx,dy);

    if c.y < lt.y then begin
      c.y:=lt.y; c.x:=CrossY(p1.x,c.y-p1.y,dx,dy)
    end else
    if c.y > rb.y then begin
      c.y:=rb.y; c.x:=CrossY(p1.x,c.y-p1.y,dx,dy)
    end
  end
  else begin
    if plane = 2 then c.y:=lt.y
    else c.y:=rb.y;

    c.x:=CrossY(p1.x,c.y-p1.y,dx,dy);

    if c.x < lt.x then begin
      c.x:=lt.x; c.y:=CrossX(c.x-p1.x,p1.y,dx,dy)
    end else
    if c.x > rb.x then begin
      c.x:=rb.x; c.y:=CrossX(c.x-p1.x,p1.y,dx,dy)
    end
  end;

  Result:=c;
end;

procedure Pop_Object;
begin
  with dst_lp^ do begin
    if N >= ord(fFillflag)*2+1 then
    lp_Dump(dst_lp); N:=-1
  end
end;

procedure Stack_add(old,new: Integer; forw: boolean);
const
  Box_Ind: array[1..4,1..4,boolean] of Integer =
     (((0,0),(1,1),(4,1),(4,4)),
      ((1,1),(0,0),(2,2),(0,2)),
      ((2,3),(2,2),(0,0),(3,3)),
      ((4,4),(3,3),(3,3),(0,0)));
var
  v: Integer;
begin
  if old <> new then
  Stack_push( Box_Ind[old,new,forw] )
end;

procedure Stack_cls(old,new: Integer; const p: TPoint);
begin
  Stack_add(old,Get_Plane(p,-1),false);
  if new = 0 then Open_Child; // Input
  FStack_SP:=0; Dst_Point(p)
end;

function Close_dst_child(const p1,p3: TPoint): Boolean;

function RibContainsPoint(const q1,q2, p: TPoint): Boolean;
begin
  Result:=false;

  if (q1.x = q2.x) and (q1.x = p.x) then begin
    if p.y >= Min(q1.y,q2.y) then
    if p.y <= Max(q1.y,q2.y) then
    Result:=true
  end else
  if (q1.y = q2.y) and (q1.y = p.y) then begin
    if p.x >= Min(q1.x,q2.x) then
    if p.x <= Max(q1.x,q2.x) then
    Result:=true
  end
end;

var
  q1,q2: TPoint; i,j,lp_n: Integer;
  lp,buf: PLLine;
begin
  Result:=false;

  with dst_lp^ do
  if N > 1 then begin
    q2:=Pol[N-1]; i:=N-2;

    while i >= 0 do begin
      q1:=q2; q2:=Pol[i];
      if RibContainsPoint(q1,q2, p3) then begin

        buf:=nil; lp_n:=N-i+4;

        if N + lp_n + 4 < LPolyMax then
          lp:=@Pol[N+1]
        else begin
          buf:=Alloc_LLine(lp_n); lp:=buf
        end;

        if Assigned(lp) then begin
          lp.N:=0; lp.Pol[0]:=p3;

          for j:=i+1 to N do begin
            Inc(lp.N); lp.Pol[lp.N]:=Pol[j];
          end;

          Inc(lp.N); lp.Pol[lp.N]:=p1;
          Inc(lp.N); lp.Pol[lp.N]:=p3;

          lp_Dump(lp);

        end; xFreePtr(buf);

        N:=i; Result:=true; Break
      end;

      Dec(i);
    end

  end

end;

procedure Lock_Rib(p1,p2: TPoint; old,new: Integer);

procedure Stack_rib(py, old,new,nxt: Integer);
var
  loop: Integer; forw: Boolean;
begin
  if nxt > Max(old,new) then begin
    forw:=new < old; nxt:=old;

    repeat old:=nxt;

      if forw then begin
        Inc(nxt); if nxt = 5 then nxt:=1
      end
      else begin
        Dec(nxt); if nxt = 0 then nxt:=4
      end;

      Stack_add(old,nxt,forw);
    until nxt = new

  end else

  if abs(new-old) <> 2 then

    Stack_add(old,new,false)

  else begin
    case old of
  1:  forw:=py = clip_lt.y;
  2:  forw:=true;
  3:  forw:=py = clip_rb.y;
  4:  forw:=false
    end;

    for loop:=1 to 2 do begin
      Stack_add(old,new,forw);

      if forw then Inc(old) else begin
        Dec(old); if old = 0 then old:=4
      end
    end
  end
end;

var
  p3: TPoint; plane: Integer;
begin
  if new = 0 then begin
    if old <> 0 then begin
      p1:=Get_Cross(p2,p1,old);
      Stack_cls(old,new,p1)
    end;

    Dst_Point(p2)
  end else

  if old = 0 then begin          // Output
    p1:=Get_Cross(p1,p2,new);
    Dst_Point(p1); Close_Child;

    Lock_Rib(p1,p2,Get_Plane(p1,-1),new)
  end else

  if new <> old then begin
    if Alt_Plane(p1,new) or Alt_Plane(p2,old) then
      Stack_add(old,new,false)
    else begin
      p1:=Get_Cross(p1,p2,old);
      plane:=Get_Plane(p1,0);

      if plane > 0 then
        Stack_rib(p1.y,old,new,plane)
      else begin
        p3:=Get_Cross(p1,p2,new);
        if (p1.x = p3.x) and (p1.y = p3.y) then
          Stack_rib(p1.y,old,new,0)
        else begin
          Stack_add(old,Get_Plane(p1,-1),false);
          FStack_SP:=0;

          if not Close_dst_child(p1,p3) then begin
            Open_Child; Dst_Point(p1);
            Dst_Point(p3); Close_Child;
            Lock_Rib(p3,p2,Get_Plane(p3,-1),new)
          end
        end
      end
    end
  end
end;

procedure Line_Rib(p1,p2: TPoint; old,new: word);
begin
  if new = 0 then begin
    if old <> 0 then begin
      p1:=Get_Cross(p2,p1,old); Dst_Point(p1)
    end; Dst_Point(p2)
  end else

  if old = 0 then begin
    p1:=Get_Cross(p1,p2,new);
    Dst_Point(p1); Pop_Object
  end else

  if new <> old then
  if not Alt_Plane(p1,new) then
  if not Alt_Plane(p2,old) then begin
    p1:=Get_Cross(p1,p2,old);
    if Get_Plane(p1,0) = 0 then begin
      p2:=Get_Cross(p1,p2,new);
      if (p1.x <> p2.x) or (p1.y <> p2.y) then begin
        Dst_Point(p1); Dst_Point(p2); Pop_Object
      end
    end
  end
end;

var
  p1,p2: TPoint; i,org,new,old: Integer;

begin
  if lp_n >= 0 then
  if dst_lp <> nil then begin

    fFillflag:=fill;

    clip_lt:=lt; clip_rb:=rb;
    clip_R[0].x:=0; clip_R[0].y:=0;

    clip_R[1].x:=lt.x; clip_R[1].y:=lt.y;
    clip_R[2].x:=rb.x; clip_R[2].y:=lt.y;
    clip_R[3].x:=rb.x; clip_R[3].y:=rb.y;
    clip_R[4].x:=lt.x; clip_R[4].y:=rb.y;

    FStack_SP:=0; cut_lp:=lp; cut_n:=lp_n;

    p2:=lp[0]; new:=Get_Plane(p2,1);
    org:=new; if new = 0 then Dst_Point(p2);

    for i:=1 to lp_N do begin
      p1:=p2; p2:=lp[i]; cut_i:=i;
      old:=new; new:=Get_Plane(p2,old);

      if fill then
        Lock_Rib(p1,p2,old,new)
      else
        Line_Rib(p1,p2,old,new)
    end;

    Clear_Child; cut_lp:=nil;

    if org > 0 then if fill then
    with dst_lp^ do if N > 0 then begin
      p1:=Pol[0]; p2:=Pol[N];

      if Points_Equal(p1,p2) then begin
        if Points_Equal(p1,Pol[1]) then begin
          Dec(N); Pol[0]:=Pol[N]
        end
      end
      else Dst_Point(p1)
    end;

    Pop_Object
  end
end;

procedure TClipPoly.Draw_LLine(lp: PLLine;
                               const lt,rb: TPoint;
                               fill: boolean);
begin
  Draw_LPoly(@lp.Pol,lp.N, lt,rb, fill);
end;

procedure TClipPoly.Cut_Polygon(lp,cut,clip,sum: PLLine; lp_max: Integer);

function this_prd(prd,nxt: Integer; c1,c2: TPoint; lp: PLLine): Integer;
var
  p: TPoint;
begin
  Result:=prd;

  if prd >= 0 then if prd <> nxt then
  if not ((prd = 0) and (nxt = lp.N)) then begin
    Middle_Point(c1,c2,p);
    if xPolyGonContainsPoint(lp, p) <= 0 then
    Result:=-1
  end
end;

function lp_Contains_lp(lp,cut: PLLine; i1,i2: int): int;
var
  k,rc: int; a,b,c: TPoint;
begin
  Result:=-1;

  with cut^ do begin
    if i1 > N then i1:=1;
    if i2 = 0 then i2:=N;

    k:=0;
    while i1 <> i2 do begin
      b:=Pol[i1];
      rc:=_rPolygonContainsPixel(@lp.Pol,lp.N,b.X,b.Y,nil);

      if rc < 0 then begin
        Result:=-1; Break
      end else
      if rc > 0 then
        Result:=1
      else begin
        if Result < 0 then Result:=0;

        if rc = 0 then
        if k > 0 then begin
          Middle_point(a,b,c);
          rc:=_rPolygonContainsPixel(@lp.Pol,lp.N, c.X,c.Y, nil);
          if rc = 1 then Result:=1
        end else
        if i1+1 = i2 then begin
          Middle_point(b,Pol[i2],c);
          rc:=_rPolygonContainsPixel(@lp.Pol,lp.N, c.X,c.Y, nil);
          if rc = 1 then Result:=1
        end
      end;

      Inc(i1); if i1 > N then i1:=1;
      a:=b; Inc(k)
    end
  end
end;

function lp_dst(lp: PLLine;
                prd,nxt: int; lp_forw: Boolean;
                cut: PLLine; i1,i2,cut_forw: int;
                const c1,c2: TPoint; clip,sum: PLLine): int;

procedure dst_pt(const p: TPoint);
var
  i: int;
begin
  i:=dst_lp.N;
  if Dst_Point(p) then
  with dst_lp^ do
  if (N = i+1) and (N >= 2) then
  if LineContainsPoint(Pol[N-2],Pol[N-1],Pol[N]) then begin
    Dec(N); Pol[N]:=Pol[N+1]
  end
end;

var
  i,rc: int;
begin
  dst_lp.N:=-1; Dst_Point(c1);
  if nxt = 0 then nxt:=lp.N;

  if lp_forw then begin
    i:=prd; repeat
      Dst_Point(lp.Pol[i]); Inc(i);
      if i > lp.N then i:=1
    until i = nxt
  end
  else begin i:=prd;
    if i = 0 then i:=lp.N;

    while i <> nxt do begin
      Dec(i); if i = 0 then i:=lp.N
      else if i < 0 then i:=lp.N-1;
      Dst_Point(lp.Pol[i])
    end
  end;

  Dst_Point(c2);

  if i1 <> i2 then begin i:=i2;

    if cut_forw = 1 then begin
      if i1 = 0 then i1:=cut.N;
      while i <> i1 do begin
        dst_pt(cut.Pol[i]); Inc(i);
        if i > cut.N then i:=1
      end
    end
    else begin
      if i = 0 then i:=cut.N; Dec(i);
      if i1 = cut.N then i1:=0;

      while i <> i1 do begin
        dst_pt(cut.Pol[i]);
        if i = 0 then i:=cut.N;
        Dec(i);
      end
    end
  end;

  Dst_Point(c1);

  if not lp_forw then begin
    i:=prd; repeat
      rc:=xPolyGonContainsPoint(dst_lp, lp.Pol[i]);
      if sum <> nil then rc:=-rc;

      if rc > 0 then begin
        dst_lp.N:=-1; Break
      end;

      Inc(i); if i > lp.N then i:=1
    until i = nxt
  end
  else begin i:=prd;
    if i = 0 then i:=lp.N;

    while i <> nxt do begin
      Dec(i); if i = 0 then i:=lp.N
      else if i < 0 then i:=lp.N-1;

      rc:=xPolyGonContainsPoint(dst_lp, lp.Pol[i]);
      if sum <> nil then rc:=-rc;

      if rc > 0 then begin
        dst_lp.N:=-1; Break
      end;
    end
  end;

  if PolyLock(dst_lp) then begin

    with dst_lp^ do while N > 2 do
    if Points_Equal(Pol[1],Pol[N-1]) then begin
      Dec(N,2); for i:=0 to N do Pol[i]:=Pol[i+1]
    end else Break;

    with dst_lp^ do if N > 2 then
    if LineContainsPoint(Pol[0],Pol[1],Pol[N-1]) then
    begin Pol[0]:=Pol[N-1]; Dec(N) end;

    with dst_lp^ do if N > 4 then
    if Long_dist(Pol[0],Pol[1]) <= 1 then
    if LineContainsPoint(Pol[0],Pol[N-1],Pol[2]) then begin
      Dec(N,2); for i:=0 to N do
      Pol[i]:=Pol[i+2]; Pol[N]:=Pol[0]
    end
  end;

  Result:=dst_lp.N
end;

function only_p_p(lp: PLLine; prd,nxt: Integer): Boolean;
begin
  Result:=false;

  if prd = 0 then begin
    if nxt = lp.N then Result:=true else
    if nxt = lp.N-1 then Result:=true
  end else

  if prd = nxt + 1 then
    Result:=true
  else
  if prd = nxt - 1 then
    Result:=true
end;

function lp_Contains_lp1(lp,cut: PLLine): Boolean;
var
  i,rc: int;
begin
  Result:=false;

  for i:=0 to cut.N do begin

    with cut.Pol[i] do
    rc:=_rPolygonContainsPixel(@lp.Pol,lp.N, X,Y, nil);

    if rc < 0 then begin Result:=false; Break end;
    Result:=true
  end;
end;

function lp_Contains_cut(lp,cut: PLLine): Boolean;
var
  i,rc: int; a,b,c: TPoint;
begin
  Result:=false;

  for i:=0 to cut.N do begin b:=cut.Pol[i];
    rc:=_rPolygonContainsPixel(@lp.Pol,lp.N, b.X,b.Y, nil);

    if (rc = 0) and (i > 0) then begin
      Middle_point(a,b,c);
      rc:=_rPolygonContainsPixel(@lp.Pol,lp.N, c.X,c.Y, nil);
    end;

    if rc > 0 then
    begin Result:=true; Break end;

    a:=b
  end
end;

function cut_lp(lp,cut,clip,sum: PLLine;
                prd,nxt, i1,i2, lp_max: Integer;
                const c1,c2: TPoint;
                InSide: boolean): Integer;
var
  i,_i1,_i2,j1,j2,di, forw,rc1,rc2: Integer;
  ct,p: TPoint; lp_forw: boolean;
begin
  Result:=-1; dst_lp1.N:=-1;

  if i1 = i2 then
  if Points_Equal(c1,c2) then
  if only_p_p(lp, prd,nxt) then
  begin Result:=2; Exit end;

  _i1:=i1; _i2:=i2;
  di:=Abs(Abs(i2)-Abs(i1));

  if di = 1 then if i1 >= 0 then
  if i2 >= 0 then di:=0;

  if (di = 0) or (di = cut.N) then begin

    if prd <> nxt then begin
      Middle_Point(c1,c2,ct);

      if i1 <> i2 then
        forw:=ord(i1 > i2)
      else begin
        forw:=0; p:=cut.Pol[Abs(i2)];
        if Long_Dist(p,c1) < Long_Dist(p,c2) then
        forw:=1
      end;

      if not InSide and (clip <> nil) then
        Result:=forw
      else begin
        rc1:=_rPolygonContainsPixel(@lp.Pol,lp.N,ct.X,ct.Y,nil);

        if rc1 > 0 then Result:=forw else
        if lp_Contains_lp(lp,cut,0,cut.N) >= 0 then
          Result:=(forw+1) and 1
        else
        if rc1 = 0 then Result:=forw
      end
    end
  end
  else begin
    j1:=Abs(i1); j2:=Abs(i2);

    forw:=0; if j2 < j1 then forw:=1;
    if Abs(j2-j1) > cut.N shr 1 then
    forw:=(forw+1) and 1;

    rc1:=lp_Contains_lp(lp,cut,j1,j2);

    if rc1 > 0 then
      Result:=0
    else begin
      rc2:=lp_Contains_lp(lp,cut,j2,j1);

      if (rc1 = 0) and (rc2 = 0) then
      if Abs(j2-j1) = 1 then begin
        Middle_Point(c1,c2,ct);

        rc1:=xPolyGonContainsPoint(lp,ct);
        if rc1 > 0 then Result:=ord(j1 > j2)
      end;

      if Result < 0 then
      if rc2 > 0 then Result:=1 else
      if rc1 = 0 then Result:=0 else
      if rc2 = 0 then Result:=1
    end
  end;

  if Result >= 0 then begin

    if i2 < 0 then if i1 = i2 then begin
      i:=-i2-1; p:=cut.Pol[i];

      if Long_Dist(p,c1) < Long_Dist(p,c2) then begin
        i1:=Poly_Insert(cut,i,lp_max,c1);
        i2:=Poly_Insert(cut,i1,lp_max,c2)
      end
    end;

    if i2 < 0 then begin
      i2:=Poly_Insert(cut,-i2-1,lp_max,c2);
      if i2 <= Abs(i1) then if i1 < 0 then
      Dec(i1) else Inc(i1)
    end;

    if i1 < 0 then begin
      i1:=Poly_Insert(cut,-i1-1,lp_max,c1);
      if i1 <= i2 then Inc(i2)
    end;

    lp_forw:=clip = nil;

    if sum <> nil then
    Result:=(Result+1) and 1;

    if lp_dst(lp,prd,nxt,not lp_forw,
              cut,i1,i2,Result,c1,c2,clip,nil) < 0 then
      Result:=-1
    else begin

      if sum = nil then
      if clip = nil then
      if InSide = lp_forw then
      if dst_lp.N > 0 then

      if lp_Contains_cut(dst_lp,cut) then
      Copy_Poly(dst_lp,dst_lp1,LPolyMax);

      if lp_dst(lp,prd,nxt,lp_forw,
                cut,i1,i2,Result,c1,c2,clip,sum) < 0 then
        Result:=-1
      else
      if sum = nil then
      if InSide <> lp_forw then begin
        if dst_lp.N > 2 then dst_Dump(dst_lp);

        lp_dst(lp,prd,nxt,not lp_forw,
               cut,i1,i2,Result,c1,c2,clip,nil)
      end
    end;

    if Result < 0 then

    if _i1 < 0 then begin
      Poly_Delete(cut,i1);
      if _i2 < 0 then begin
        if i2 < i1 then Poly_Delete(cut,i2) else
        if i2 > i1 then Poly_Delete(cut,i2-1)
      end
    end else
    if _i2 < 0 then
      Poly_Delete(cut,i2)
  end
end;

function Cut_Contains_lp(cut,lp: PLLine;
                         const cut_c: TPoint): Boolean;
var
  i,cnt,rc: Integer;
begin
  Result:=false; cnt:=0;

  for i:=0 to lp.N do with cut^ do begin
    rc:=PolygonContainsPoint(@Pol,N,lp.Pol[i]);
    if rc = 0 then Inc(cnt) else if rc > 0 then
    begin Result:=true; Break end
  end;

  if not Result then
  if xPolyGonContainsPoint(lp,cut_c) > 0 then
  Result:=true
end;

function cut3(lp,cut: PLLine): bool;
var
  obj: TDivPolygon; i: int;
begin
  Result:=false;

  obj:=TDivPolygon.Create;
  try
    obj.beginCut(lp);
    if obj.Cut2(cut,-1) > 0 then begin

      for i:=0 to obj.parts.PartCount-1 do
      if obj.parts.get_line(i,dst_lp,nil,LPolyMax) > 0 then
      if PolyLock(dst_lp) then

      if xPolygonContainsPolygon(lp,dst_lp,1) then
      dst_Dump(dst_lp);

      Result:=true
    end
  finally
    obj.Free
  end
end;

function cut_on_rib(lp,cut,clip,sum: PLLine;
                    nxt: int; const c: TCrossArray): bool;
var
  i: int; c1,c2: TCross;
begin
  Result:=false;

  for i:=Cross_Max downto 1 do begin
    c2:=c[i];
    if c2.rc >= 0 then begin
      c1:=c[i-1];

      if cut_lp(lp,cut,clip,sum, nxt,nxt,
                c2.ic,c1.ic,lp_max,
                c2.p,c1.p,true) >= 0 then

      if PolyEqual(@lp.Pol,lp.N,
                   @dst_lp.Pol,dst_lp.N) then
        dst_lp.N:=-1
      else begin
        Result:=true; Break
      end
    end
  end
end;

var
  i,j,_j, i1,i2,_i2,i3,i4, rc,rc1,rc_nxt: int;
  top,_top, nxt,_nxt, prd,_prd, old, j1,j2, loop: int;
  l_lt,l_rb,c_lt,c_rb, p1,p2,p: TPoint;
  c1,c2,_c2,c3,c4,cut_c,top_p: TPoint;

  cq,cr1,cr2: TCross; c,_c: TCrossArray;
  mk: TLineMarker1; d1,d2: double;
  is_Edit,is_cut,InSide: bool;
begin
  fEditFlag:=false;

  with lp^ do N:=dup_LPlan(@Pol,nil,N);
  with cut^ do N:=dup_LPlan(@Pol,nil,N);
  with cut^ do N:=dup_LTurn(@Pol,N);

  if dst_lp <> nil then
  if PolyLock(cut) then begin

    is_Edit:=false; is_cut:=true;
    cut_c:=Polygon_Centre(cut);

    with lp^ do Max_Poly_Bound(@Pol,N, l_lt,l_rb);
    with cut^ do Max_Poly_Bound(@Pol,N, c_lt,c_rb);

    loop:=0;
    while PortContainsPort(c_lt,c_rb, l_lt,l_rb) do begin

      dst_lp.N:=-1; _top:=0; top:=-1;
      while (_top >= 0) and (_top < lp.N) do begin

        if _top <= top then Break;

        top:=-1; nxt:=-1; prd:=-1;

        for i:=_top to lp.N-1 do begin

          p:=lp.Pol[i]; rc:=-1;
          if PortContainsPoint(c_lt,c_rb,p.X,p.Y) then
          rc:=_rPolygonContainsPixel(@cut.Pol,cut.N,p.X,p.Y,nil);

          if rc < 0 then begin
            top_p:=p; top:=i; Break
          end
        end;

        if top < 0 then
        if lp.N < lp_Max then begin

          p2:=lp.Pol[_top];
          for i:=_top to lp.N do begin
            p1:=p2; p2:=lp.Pol[i+1];
            Middle_Point(p1,p2,p);

            rc:=_rPolygonContainsPixel(@lp.Pol,lp.N,p.X,p.Y,nil);

            if rc = 1 then begin
              rc:=-1;
              if PortContainsPoint(c_lt,c_rb,p.X,p.Y) then
              rc:=_rPolygonContainsPixel(@cut.Pol,cut.N,p.X,p.Y,nil);

              if rc < 0 then
              if zPoly_Insert(lp,nil,i,lp_Max,p,0) > i then begin
                top_p:=p; top:=i+1; Break
              end
            end else
            if rc = 0 then begin
              rc:=_rPolygonContainsPixel(@cut.Pol,cut.N,p.X,p.Y,@mk);

              if rc < 0 then
              if mk.err < 1 then begin
                c1:=cut.Pol[mk.Ind];
                c2:=cut.Pol[mk.Ind+1];

                d1:=Dist_to_Line(p1, c1,c2);
                d2:=Dist_to_Line(p2, c1,c2);

                if d1 < 0.5 then
                if d2 < 0.5 then rc:=0
              end;

              if rc < 0 then
              if zPoly_Insert(lp,nil,i,lp_Max,p,0) > i then
              begin top_p:=p; top:=i+1; Break end
            end
          end
        end;

        _top:=-1;
        if top >= 0 then begin
          j:=top; p1:=lp.Pol[j];
          _nxt:=-1; j1:=-1; j2:=-1;

          for i:=1 to lp.N do begin
            Inc(j); if j > lp.N then j:=1;
            p2:=lp.Pol[j];

if _rPolygonContainsPixel(@cut.pol,cut.N,p2.X,p2.Y,nil) = 1 then
d1:=_rPolygonContainsPixel(@cut.pol,cut.N,p1.X,p1.Y,nil);

//
if j=lp.N-6 then
j:=j;
if j=lp.N-7 then
j:=j;

            rc:=iPolyCross(cut, p1,p2, c,true);

            if rc > 0 then begin
              nxt:=j;
              i2:=c[0].ic; c2:=c[0].p;
              i3:=c[1].ic; c3:=c[1].p;
              j1:=-1; j2:=-1; Break
            end else
            if rc = 0 then
            if clip <> nil then begin
              if _nxt = -1 then begin
                i4:=c[0].ic; c4:=c[0].p;
                _nxt:=j;
              end
            end
            else begin _nxt:=-1;
              if c[0].rc = 2 then begin

                if c[1].rc = 0 then
                if (j1 >= 0) and (j2 < 0) then
                if _c[0].ic = c[0].ic then
                if Points_Equal(_c[0].p,c[0].p) then
                begin j2:=j; _c[1]:=c[1] end

              end
              else begin
                if j1 < 0 then begin
                  j1:=j; _c[0]:=c[0];
                  if c[1].rc = 0 then begin
                    j2:=j; _c[1]:=c[1]
                  end
                end
                else begin
                  j2:=j; _c[1]:=c[0]
                end
              end
            end;

            p1:=p2
          end;

          if nxt >= 0 then begin

            rc_nxt:=-1;
            if nxt <> top then begin
              _top:=nxt; p2:=lp.Pol[nxt];
              rc_nxt:=xPolyGonContainsPoint(cut, p2)
            end;

            rc:=rc_nxt;
            if rc = 0 then begin
              if c[0].rc = 1 then
              if c[1].rc = 0 then rc:=1;
            end;

            if rc > 0 then begin
              j:=nxt; _c:=c;

              repeat p1:=p2;
                Inc(j); if j > lp.N then
                if top = 0 then Break else
                j:=1; p2:=lp.Pol[j]; _top:=j;

                rc:=iPolyCross(cut, p1,p2, c,false);

                if rc = 1 then begin
                  i1:=c[0].ic; c1:=c[0].p;

                  if rc_nxt = 0 then begin
                    _i2:=_c[0].ic; _c2:=_c[0].p
                  end else

                  _i2:=Back_Cross(lp.Pol[nxt],_c,_c2);

                  rc:=cut_lp(lp,cut,clip,sum, j,nxt,
                             i1,_i2,lp_max, c1,_c2, true);

                  if rc >= 0 then begin
                    _top:=-1; p:=lp.Pol[nxt];
                    if (rc = 2) and Assigned(clip) then

                    with clip^ do
                    if PolygonContainsPixel(@Pol,N,p.X,p.Y) >= 0 then
                    begin Poly_Delete(lp,nxt); _top:=nxt; rc_nxt:=1 end;

                    is_Edit:=false; nxt:=-1; _nxt:=-1
                  end else

                  if cut_on_rib(lp,cut,clip,sum,j,c) then begin
                    _top:=-1; is_Edit:=false;
                    nxt:=-1; _nxt:=-1
                  end;

                  Break
                end

              until j = top;
            end;

            if _top >= 0 then
            if rc_nxt <= 0 then begin
              if c[0].rc in [1,2] then
              if c[1].rc in [0,1] then

              if cut_lp(lp,cut,clip,sum, nxt,nxt,
                        i3,i2,lp_max,c3,c2,true) >= 0 then
              begin
                _top:=-1; is_Edit:=false;
                nxt:=-1; _nxt:=-1
              end;

              if nxt >= 0 then
              if nxt = top then
              nxt:=-1
            end else

            if cut_on_rib(lp,cut,clip,sum,nxt,_c) then begin
              _top:=-1; is_Edit:=false;
              nxt:=-1; _nxt:=-1
            end
          end else

          if Assigned(clip) then begin
            if xPolygonContainsPolyLine(lp,clip) then begin
              dst_Dump(clip); is_edit:=false; Break
            end
          end
          else begin

            if (j1 > 0) and (j2 > 0) then

            if Assigned(sum)
            or xPolygonContainsPolyLine(lp,cut) then begin
              i1:=_c[1].ic; c1:=_c[1].p;
              i2:=_c[0].ic; c2:=_c[0].p;

              if cut_lp(lp,cut,clip,sum, j2,j1,
                        i1,i2,lp_max, c1,c2, true) >= 0 then begin

                if sum = nil then begin
                  with lp^ do
                  rc1:=xPolygonContainsPixel(@Pol,N, top_p.X,top_p.Y);
                  if rc1 < 0 then dst_lp.N:=-1
                end;

                is_Edit:=false;
                if dst_lp1.N > 0 then begin
                  dst_dump(dst_lp);
                  Copy_Poly(dst_lp1,dst_lp,LPolyMax)
                end
              end
            end;

            if is_Edit then begin
              dst_Dump(lp); is_Edit:=false
            end
          end;

          if (nxt < 0) and (_nxt >= 0) then

          if Cut_Contains_lp(cut,lp,cut_c) then begin
            nxt:=_nxt; i2:=i4; c2:=c4;
            _nxt:=-1
          end;

          if nxt >= 0 then begin
            j:=top; p1:=lp.Pol[j]; _prd:=-1;

            InSide:=false; repeat
              _j:=j; Dec(j); if j < 0 then
              j:=lp.N-1; p2:=lp.Pol[j];

              rc:=iPolyCross(cut, p1,p2, c,true);

              if rc > 0 then begin
                prd:=_j; i1:=c[0].ic; c1:=c[0].p;

                if j+1 = nxt then begin
                  i3:=c[1].ic; c3:=c[1].p;
                  if i3 <> 0 then

                  if Points_Equal(p2,c3) then begin
                    i2:=i3; c2:=c3; InSide:=true;
                    nxt:=j; Break
                  end else
                  if not Points_Equal(c3,c2) then
                  if Long_Dist(c3,p2) < Long_Dist(c1,p2) then begin
                    i1:=i3; c1:=c3; InSide:=true; Break
                  end
                end;

                if _prd >= 0 then begin
                  prd:=_prd; i1:=cq.ic; c1:=cq.p
                end;

                InSide:=false; Break

              end else
              if rc = 0 then begin
                if _prd = -1 then begin
                  _prd:=_j; cq:=c[0]
                end
              end
              else _prd:=-1;

              p1:=p2

            until j = nxt-1;

            if prd < 0 then
            if _prd >= 0 then begin
              prd:=_prd; i1:=cq.ic; c1:=cq.p
            end;

            if prd >= 0 then begin

              if _nxt >= 0 then
              if cut_lp(lp,cut,clip,sum,
                        prd,_nxt, i1,i4,lp_max,
                        c1,c4, InSide) >= 0 then begin
                if is_Edit and (clip = nil) then dst_Dump(lp);
                _top:=-1; is_Edit:=false
              end;

              if _top >= 0 then begin
                if cut_lp(lp,cut,clip,sum,
                          prd,nxt, i1,i2,lp_max,
                          c1,c2, InSide) >= 0 then begin

                  if is_Edit then
                  if dst_lp.N <= 2 then // добавлено для обрезки
                  dst_Dump(lp);

                  _top:=-1; is_Edit:=false
                end else
                if Assigned(clip) then begin

                  p1:=lp.Pol[prd];
                  j:=prd-1; if j < 0 then
                  j:=lp.N-1; p2:=lp.Pol[j];

                  if _rPolyGonContainsPixel(@cut.Pol,cut.N, p1.X,p1.Y,nil) < 0 then
                  if _rPolyGonContainsPixel(@cut.Pol,cut.N, p2.X,p2.Y,nil) > 0 then

                  repeat
                    p1:=p2; _j:=j;
                    Dec(j); if j < 0 then
                    j:=lp.N-1; p2:=lp.Pol[j];

                    rc:=iPolyCross(cut, p1,p2, c,true);

                    if rc = 1 then
                    if c[0].rc = 1 then begin
                      i2:=c[0].ic; c2:=c[0].p;

                      rc:=cut_lp(lp,cut,clip,sum, prd,_j,
                                 i1,i2,lp_max, c1,c2, true);

                      if rc >= 0 then _top:=-1;
                      Break
                    end
                  until j = nxt-1
                end
              end else

              if Assigned(cut) then
              if prd = top then

              if (dst_lp.N < 0)
              or (clip = nil) then begin

                if cut_lp(lp,cut,clip,sum,
                          prd,nxt, i1,i2,lp_max,
                          c1,c2, InSide) >= 0 then
                is_Edit:=false

              end

            end
          end

        end else

        if clip = nil then
          is_Edit:=false
        else
        if is_Edit then begin
          if PolygonClipPolygon(@cut.Pol,cut.N,
                                @lp.Pol,lp.N) then
          dst_Dump(lp); is_Edit:=false
        end
        else lp_Clip(lp)

      end; // while (top >= 0) ...

      if dst_lp.N > 2 then begin

        if dst_lp.N >= lp_max then
          Break
        else
        if Assigned(sum) then begin
          Copy_Poly(dst_lp,sum,lp_max);
          Max_Poly_Bound(@sum.Pol,sum.N, c_lt,c_rb);
          fEditFlag:=true
        end else

        if PolyEqual(@lp.Pol,lp.N,
                     @dst_lp.Pol,dst_lp.N) then
          Break
        else begin
          Copy_Poly(dst_lp,lp,lp_max);
          Max_Poly_Bound(@lp.Pol,lp.N, l_lt,l_rb);
          is_Edit:=true; is_cut:=false;
          fEditFlag:=true
        end

      end
      else begin
        if Assigned(clip) then begin
          if is_Edit then
          if PolygonClipPolygon(@cut.Pol,cut.N,
                                @lp.Pol,lp.N) then
          dst_Dump(lp); is_Edit:=false

        end else
        if is_Edit then
          dst_Dump(lp)
        else
        if Assigned(sum) then
          add_polygon1(sum,lp)
        else
        if xPolygonContainsPolyLine(lp,cut) then
          cut_polygon1(lp,cut)
        else begin
          rc:=PolygonContainsPolyLine(lp,cut);
          if (rc >= 0) and (rc < cut.N) then
          cut3(lp,cut)
        end;

        is_Edit:=false; Break
      end

    end; // PortContainsRect

    if clip = nil then
    if is_edit or is_cut then
    if not xPolygonContainsPolyLine(cut,lp) then
    lp_cut(lp)

  end // PolyLock
end;

function TClipPoly.cut_polygon1(lp,cut: PLLine): bool;

function out_edge1(lp,lp1: PLLine; i1,i2,di: int): bool;
var
  k,rc: int; a,b,p: TPoint;
begin
  Result:=false; k:=0;

  a:=lp1.Pol[i1]; b:=lp1.Pol[i2];

  if di < 0 then begin
    if i2 = lp1.N then i2:=0
  end
  else begin
    if i2 = 0 then i2:=lp1.N
  end;

  while true do begin

    Inc(i1,di);
    if i1 < 0 then i1:=lp1.N-1;

    if i1 = i2 then Break;

    Inc(k); p:=lp1.Pol[i1];

    if lp = nil then
      Dst_Point(p)
    else begin
      rc:=xPolygonContainsPoint(lp,p);
      Result:=true; if rc <= 0 then begin
        Result:=false; Break
      end
    end
  end
end;

function out_edge2(lp,lp1: PLLine;
                   const m1,m2: TLineMarker1;
                   di: int): bool;
var
  i1,i2,k,rc,dir: int; a,b,p: TPoint; m: TLineMarker1;
begin
  Result:=false;

  dir:=0;
  if m1.dist < m2.dist then dir:=1 else
  if m1.dist > m2.dist then dir:=-1;

  i1:=Abs(m1.Ind); i2:=Abs(m2.Ind);

  if m1.Ind < 0 then
  if m2.Ind < 0 then
  if (i1 = i2) and (dir = di) then
  dir:=0;

  if dir <> 0 then begin

    k:=0; a:=m1.pc; b:=m2.pc;

    if di > 0 then begin
      if m1.Ind >= 0 then Inc(i1);

      if i1 = lp1.N then i1:=0;
      if i2 = lp1.N then i2:=0;

      repeat
        Inc(k); p:=lp1.Pol[i1];

        if lp = nil then
          Dst_Point(p)
        else begin
          rc:=xPolygonContainsPoint(lp,p);
          if rc < 0 then Result:=true else begin
            Result:=false; Break
          end
        end;

        Inc(i1);
        if i1 = lp1.N then i1:=0 else
        if i1 > lp1.N then begin
          if i2 = 0 then Break; i1:=1
        end
      until i1 = i2
    end
    else begin
      if m2.Ind >= 0 then Dec(i2);

      if i1 = lp1.N then i1:=0;
      if i2 = lp1.N then i2:=0;

      repeat
        Dec(i1); if i1 < 0 then i1:=lp1.N-1;

        Inc(k); p:=lp1.Pol[i1];

        if lp = nil then
          Dst_Point(p)
        else begin
          rc:=xPolygonContainsPoint(lp,p);
          if rc < 0 then Result:=true else begin
            Result:=false; Break
          end
        end
      until i1 = i2
    end;

    if Assigned(lp) then
    if k = 0 then begin
      Middle_Point(a,b,p);
      rc:=xPolygonContainsPoint(lp,p);

      if rc <> 0 then
      if ippl(lp,p,1,m) then rc:=0;

      Result:=rc < 0
    end
  end
end;

function RibContains(const a,b, p: TPoint): bool;
var
  pc: TPoint;
begin
  Result:=false;
  if Plot_to_Line1(p, a,b, pc) = 0 then
  Result:=Long_Dist(p,pc) <= 1
end;

function cut1(lp,cut: PLLine): bool;
var
  i, di, j,dj, i1,i2: int;
  m,m1,m2: TLineMarker1;
  p1,p2,p: TPoint;
begin
  Result:=false;

  i1:=-1; i2:=-1;

  for i:=0 to cut.N-1 do begin
    p:=cut.Pol[i];
    if ippl(lp,p,1,m) then

    if i1 < 0 then begin
      i1:=i; m1:=m; p1:=p
    end else
    if i2 < 0 then begin
      i2:=i; m2:=m; p2:=p
    end else
    if RibContains(p1,p, p2) then begin
      i2:=i; m2:=m; p2:=p
    end else
    if RibContains(p,p2, p1) then begin
      i1:=i; m1:=m; p1:=p
    end else
    if RibContains(p1,p2, p) then
    else begin
      i1:=-1; Break
    end
  end;

  if i2 <= i1 then i1:=-1;

  if i1 >= 0 then begin

    di:=0;
    if out_edge1(lp,cut,i1,i2,1) then di:=1 else
    if out_edge1(lp,cut,i1,i2,-1) then di:=-1;

    if di <> 0 then begin

      dj:=0;
      if out_edge2(cut,lp,m2,m1,1) then dj:=1 else
      if out_edge2(cut,lp,m2,m1,-1) then dj:=-1;

      if dj <> 0 then begin
        Dst_Point(cut.Pol[i1]);
        out_edge1(nil,cut,i1,i2,di);
        Dst_Point(cut.Pol[i2]);
        out_edge2(nil,lp,m2,m1,dj);
        Dst_Point(cut.Pol[i1]);

        if PolyLock(dst_lp) then begin
          dst_Dump(dst_lp);
          Result:=true
        end
      end
    end
  end
end;

function cut2(lp,cut: PLLine): bool;
var
  obj: TCutObjc; i: int;
begin
  Result:=false;

  obj:=TCutObjc.Create;
  try
    obj.beginCut(lp);
    if obj.off_child(cut) > 0 then
    if obj.endCut > 0 then begin

      for i:=0 to obj.parts.PartCount-1 do
      if obj.parts.get_line(i,dst_lp,nil,LPolyMax) > 0 then
      if PolyLock(dst_lp) then dst_Dump(dst_lp);

      Result:=true
    end
  finally
    obj.Free
  end
end;

begin
  Result:=cut2(lp,cut)
end;

function TClipPoly.add_polygon1(lp1,lp2: PLLine): bool;
var
  i,j: int;
begin
  Result:=false;

  if PolyLock(lp1) then
  if PolyLock(lp2) then

  if lp2.N < LPolyMax then begin

    dst_lp.N:=-1; for i:=0 to lp2.N do
    if xPolygonContainsPoint(lp1,lp2.Pol[i]) < 0 then begin

      with dst_lp^ do begin
        for j:=i to lp2.N do begin
          Inc(N); Pol[N]:=lp2.Pol[j];
        end;

        for j:=1 to i do begin
          Inc(N); Pol[N]:=lp2.Pol[j];
        end;

        Load_Poly(lp2,@Pol,N+1); Break
      end
    end;

    if PolyLock(dst_lp) then begin


    end
  end
end;

procedure TClipPoly.Clip_Polyline(lp,clip: PLLine; hp: PIntegers);

procedure dst_inter(const p1,p2: lxyz; const pc: TPoint);
var
  v: lxyz;
begin
  v.p:=pc; v.v.z:=0;
  inter_vpoint(p1.v,p2.v,v.v);
  xyz_Point(v);
end;

function push_out_point(clip: PLLine;
                        const v1,v2: lxyz): Boolean;
var
  i: Integer; p1,p2,pc,q1,q2: TPoint;
begin
  Result:=false;

  p1:=v1.p; p2:=v2.p; pc:=p2;

  q2:=clip.Pol[0];
  for i:=1 to clip.N do begin
    q1:=q2; q2:=clip.Pol[i];

    if LineContainsPoint(q1,q2, p1) then

    if LineContainsPoint(p1,p2, q2)
    and not Points_Equal(p1,q2) then begin
      pc:=q2; Result:=true; Break
    end else

    if LineContainsPoint(p1,p2, q1)
    and not Points_Equal(p1,q1) then begin
      pc:=q1; Result:=true; Break
    end
  end;

  if Result then dst_inter(v1,v2,pc)
end;

function x_dst: bool;
var
  i: int; l: double; p1,p2: TPoint;
begin
  Result:=false;

  with dst_lp^ do if N > 0 then begin
    p1:=Pol[0]; l:=0;
    for i:=1 to N do begin
      p2:=Pol[i];
      l:=l+Long_Dist(p1,p2);

      if l > 1 then begin
        dst_Dump(dst_lp);
        Result:=true; Break
      end;

      p1:=p2
    end
  end;
              
  dst_lp.N:=-1
end;

var
  i,j, prd,nxt: Integer;
  p1,p2,pc: lxyz; c: TCrossArray;
begin
  if lp.N > 0 then
  if dst_lp <> nil then begin

    fIsXyz:=Assigned(hp);

    i:=0; if PolyLock(lp) then
    for j:=lp.N-1 downto 0 do
    if xPolyGonContainsPoint(clip, lp.Pol[j]) < 0 then
    begin i:=j; Break end;

    p2.p:=lp.Pol[i]; p2.v.z:=0;
    if Assigned(hp) then p2.v.z:=hp[i];

    dst_lp.N:=-1; nxt:=0;

    for j:=0 to lp.N do begin
      p1:=p2; p2.p:=lp.Pol[i]; p2.v.z:=0;
      if Assigned(hp) then p2.v.z:=hp[i];
      Inc(i); if i > lp.N then i:=1;

      prd:=nxt;
      nxt:=xPolyGonContainsPoint(clip, p2.p);

      if nxt = 0 then begin
        if prd < 0 then
        push_out_point(clip, p2,p1);
        xyz_Point(p2)
      end else
      if nxt > 0 then begin // InSide

        if prd < 0 then
        if iPolyCross(clip, p1.p,p2.p, c,true) > 0 then
        dst_inter(p1,p2,c[0].p); xyz_Point(p2)

      end else
      if j > 0 then
      if nxt < 0 then begin // OutSide

        if prd >= 0 then begin
          if iPolyCross(clip, p1.p,p2.p, c,true) >= 0 then
            dst_inter(p1,p2,c[0].p)
          else
            push_out_point(clip, p1,p2);

          x_dst
        end else

        while iPolyCross(clip, p1.p,p2.p, c,true) = 1 do
        if (c[0].rc = 1) and (c[1].rc = 1) then begin
          dst_inter(p1,p2,c[0].p);
          dst_inter(p1,p2,c[1].p);
          x_dst; pc.p:=c[1].p;
          inter_vpoint(p1.v,p2.v,pc.v);
          p1:=pc
        end
        else Break
      end
    end;

    x_dst; fIsXyz:=false
  end
end;

function TClipPoly.Cut_Polyline(lp,cut: PLLine; lp_max: Integer;
                                only_div: Boolean): boolean;

function dst_piece(mk: TPolyMarkerList; Ind: Integer): Integer;
var
  P1,P2: PPolyMarker; I: Integer;
begin
  Result:=0;

  P1:=mk.Items[Ind];
  P2:=mk.Items[Ind+1];

  if Assigned(P1) then
  if Assigned(P2) then begin

    Dst_Point(P1.P); Inc(Result);

    if Assigned(P2) then
    for I:=P1.Ind+1 to P2.Ind do
    dst_Point(mk.Ctrl.Pol[I]);

    Dst_Point(P2.P); Inc(Result);
  end
end;

function x_dst_piece(mk: TPolyMarkerList; Ind: Integer;
                     cut: PLLine; only_div: Boolean): Integer;
var
  P1,P2: PPolyMarker; pc: TPoint;
begin
  Result:=0;

  P1:=mk.Items[Ind];
  P2:=mk.Items[Ind+1];

  if Assigned(P1) then
  if Assigned(P2) then begin

    if P1.Ind = P2.Ind then
      Middle_Point(P1.P,P2.P,pc)
    else begin
      pc:=mk.Ctrl.Pol[P1.Ind+1];

      if Points_Equal(pc,P2.P) then
      Middle_Point(P1.P,P2.P,pc)
    end;

    if only_div
    or (xPolyGonContainsPoint(cut, pc) < 0) then begin
      Result:=dst_piece(mk,Ind); dst_Dump(dst_lp)
    end
  end
end;

var
  mk,mk1: TPolyMarkerList;
  mp: PPolyMarkers; i,j,rc: Integer;
  p1,p2: TPoint; c: TCrossArray;
  is_lock: Boolean;
begin
  Result:=false;

  mk:=TPolyMarkerList.Create(lp,16);
  mk1:=TPolyMarkerList.Create(nil,16);
  try
    if lp.N > 0 then
    if dst_lp <> nil then begin

      dst_lp.N:=-1; p2:=lp.Pol[0];

      with lp.Pol[0] do mk.AddItem(0,x,y);

      for i:=1 to lp.N do begin
        p1:=p2; p2:=lp.Pol[i];

        if iPolyCross(cut, p1,p2, c,true) >= 0 then

        for j:=0 to Cross_Max do
        if c[j].rc < 0 then Break
        else with c[j] do begin
          mk.AddItem(i-1,p.X,p.Y);
          if ic < 0 then
          mk1.AddItem(Abs(ic)-1,p.X,p.Y)
        end
      end;

      mk1.Sort_up(1); mp:=mk1.First;
      for i:=0 to mk1.Count-1 do
      if cut.N < lp_max then with mp[i] do
      Poly_Insert(cut,Ind,lp_max,P);

      with lp^,Pol[N] do mk.AddItem(N,x,y);

      is_lock:=PolyLock(lp); if is_lock then
      if Odd(mk.Count) then begin
        mp:=mk.First;
        if mp[mk.Count-1].Ind = lp.N then
        for i:=0 to mk.Count-2 do
        x_dst_piece(mk,i,cut,only_div);
        mk.Clear;
      end;

      if mk.Count > 2 then begin

        i:=0; if is_lock then
        if xPolyGonContainsPoint(cut, lp.Pol[0]) < 0 then begin
          dst_piece(mk,mk.Count-2); dst_piece(mk,0);
          mk.Delete_last; i:=1; dst_Dump(dst_lp)
        end;

        while i < mk.Count-1 do begin
          x_dst_piece(mk,i,cut,only_div); Inc(i)
        end;

        Result:=true
      end;
    end;
  finally
    mk1.Free;
    mk.Free
  end
end;

procedure TClipPoly.dst_Dump(lp: PLLine);
var
  min: Integer;
begin
  if not Poly_Empty(lp) then begin
    min:=1; if PolyLock(lp) then min:=3;

    if lp.N >= min then
    if lp = dst_lp then lp_Dump1(lp)
                   else lp_Dump(lp);

    if lp = dst_lp then dst_lp.N:=-1
  end
end;

type
  TMaxPoly = class(TClipPoly)
    constructor Create;
    procedure lp_Dump(lp: PLLine); override;
  private
    fline: TLLine;
  end;

constructor TMaxPoly.Create;
begin
  inherited Create; fline.N:=-1
end;

procedure TMaxPoly.lp_Dump(lp: PLLine);
begin
  if lp.N > fline.N then begin
    Copy_Poly(lp,@fline,lPoly_Max); with fline do
    if Pol[0].x > Pol[N].x then Swap_Poly(@fline)
  end
end;

procedure Get_Max_PolyLine(lp: PLLine; w,h: longint);
var
  a,b: TPoint; clip: TMaxPoly;
begin
  a.x:=0; a.y:=0; b.x:=w-1; b.y:=h-1;

  clip:=TMaxPoly.Create;
  try
    begin
      clip.Draw_LLine(lp,a,b,false);
      Copy_Poly(@clip.fline,lp,lPoly_Max)
    end;
  finally
    clip.Free;
  end;
end;

function Dist1_to_PolyLine(lp: PLPoly; lp_N: Integer;
                           const p: TPoint): Double;
var
  i: int; p1,p2,p3: TPoint; dist,temp: Double;
begin
  Result:=0; 

  if lp_N = 0 then
    Result:=Long_Dist(p,lp[0])
  else
  if lp_N > 0 then begin

    p1:=lp[0]; p2:=lp[1];
    Result:=Dist_to_Line(p, p1,p2);
    Middle_Point(p1,p2, p3);
    dist:=Long_Dist(p,p3);

    for i:=2 to lp_N do begin
      p1:=p2; p2:=lp[i];

      Middle_Point(p1,p2, p3);
      temp:=Long_Dist(p,p3);

      if temp < dist then begin
        Result:=Dist_to_Line(p, p1,p2);
        dist:=temp
      end

    end
  end
end;

function Dist2_To_PolyLine(const p: TPoint; lp: PLLine;
                           var dist: double): Integer;
var
  i: Integer; t: double; p1,p2,c: TPoint;
begin
  Result:=-1; if lp <> nil then
  with lp^ do if N > 0 then begin

    p2:=Pol[0]; for i:=1 to N do begin
      p1:=p2; p2:=Pol[i];

      if Plot_to_Line(p ,p1,p2, c) then begin

        t:=Abs(Dist_to_Line(p, p1,p2));

        if t <= Small then
          begin Result:=0; Break end
        else
        if (Result < 0) or (t < dist) then
          begin dist:=t; Result:=i end

      end
    end
  end
end;

function Dist_To_Road(const p: TPoint; lp: PLLine;
                      var dist: double): Integer;
var
  i: Integer; t1,t2: double; p1,p2,c: TPoint;
begin
  Result:=-1; if lp <> nil then
  with lp^ do if N > 0 then begin

    p2:=Pol[0]; for i:=1 to N do begin
      p1:=p2; p2:=Pol[i];

      if Plot_to_Line(p ,p1,p2, c) then begin

        t1:=Dist_to_Line(p, p1,p2);
        t2:=Abs(t1);

        if t2 <= Small then
          begin Result:=0; Break end
        else
        if (Result < 0) or (t2 < Abs(dist)) then
          begin dist:=t1; Result:=i end

      end
    end
  end
end;

function lp_ContainsPoint(lp: PLLine; r: double; p: TPoint): Integer;
var
  i: Integer; lt,rb,c, p1,p2: TPoint;
  d,d1,d2, t1,t2: double;
begin
  Result:=-1; with lp^ do

  if N > 0 then begin

    i:=0; p1:=Pol[0]; t1:=-1; t2:=-1;

    for i:=1 to N do begin p2:=Pol[i];

      if not Points_Equal(p1,p2) then begin

        d:=Abs(Dist_to_Line(p ,p1,p2));

        if d <= r then
        if Project_to_Line(p ,p1,p2, c) then begin

          Swap_lRect(p1,p2, lt,rb);

          if PortContainsPoint(lt,rb, c.x,c.y) then begin
            if (t1 < 0) or (d < t1) then begin
              t1:=d; Result:=i
            end
          end else

          if t1 < 0 then begin
            d1:=Long_Dist(c,p1); d2:=Long_Dist(c,p2);
            d:=d1; if d2 < d1 then d:=d2;

            if (t2 < 0) or (t2 > d) then
            begin t2:=d; Result:=i end
          end
        end
      end;

      p1:=p2
    end

  end;

  if Result > 0 then if t1 < 0 then
  if t2 > r/2 then Result:=-1
end;

function lp_ContainsCentre(lp: PLLine; r: double; p1,p2: TPoint): Integer;
var
  c: TPoint;
begin
  Middle_Point(p1,p2, c);
  Result:=lp_ContainsPoint(lp,r,c)
end;

procedure Allign_Point(c: TPoint; r,f,d: double; var p: TPoint);
begin
  if d > 0 then f:=f-Pi/2 else f:=f+Pi/2;
  Get_F_Point(c.x,c.y,r,r,f,p)
end;

procedure Allign_Vertex(p1, q1,q2: TPoint; r: double; var p: TPoint);
var
  d,f: double; c: TPoint;
begin
  p:=p1;

  if Project_to_Line(p1 ,q1,q2, c) then begin

    f:=iArcTan(q1.x,q1.y,q2.x,q2.y);
    d:=Dist_to_Line(p1, q1,q2);

    Allign_Point(c, r,f,d, p)
  end
end;

procedure Allign_rib(p1,p2, pc, q1,q2: TPoint; r: double; var p: TPoint);
var
  c1,c2,t: TPoint; f, d1,d2,d3: double;
begin
  if Project_to_Line(p2 ,q1,q2, c1) then begin

    f:=iArcTan(q1.x,q1.y,q2.x,q2.y);

    d1:=Dist_to_Line(p1, q1,q2);
    d2:=Dist_to_Line(p2, q1,q2);
    d3:=Dist_to_Line(pc, q1,q2);

    if (d1 > 0) <> (d3 > 0) then
    if (d1 > 0) = (d2 > 0) then
    d3:=d1;

    Allign_Point(c1, r,f,d3, p);
    d2:=Dist_to_Line(p, q1,q2);

    if (d1 > 0) = (d2 > 0) then
    if Project_to_Line(p1 ,q1,q2, c2) then
    if Long_Dist(c1,c2) < Abs(d2)/4 then
    if Long_Dist(c1,c2) < r/2 then begin

      d1:=Abs(d1); if d1 > r then
      if iLL2(p1,p2, q1,q2, c2) then
      if Long_Dist(c1,c2) < r then

      Get_Next_Point(p1,c2, Long_Dist(p1,c2)/d1*(d1-r), p)
    end
  end
  else p:=p2
end;

function lp_Break(lp,line: PLLine; i,j1,j2: Integer; r: double): boolean;
var
  p1,p2,p,c: TPoint;
begin
  Result:=false;

  if Abs(j2-j1) > 1 then
  with line^ do begin

    p1:=lp.Pol[i]; p2:=lp.Pol[i+1];

    while j1 <> j2 do begin
      if j1 < j2 then Inc(j1) else Dec(j1);
      if j1 = j2 then Break; p:=Pol[j1];

      if Abs(Dist_to_Line(p, p1,p2)) > r then
        Result:=true
      else
      if not Project_to_Line(p ,p1,p2, c) then
        Result:=true;

      if Result then Break
    end
  end
end;

function Insert_rib(lp,line: PLLine; i,max, j1,j2: Integer; r: double): Integer;
var
  p1,p2,p3, a1,a2,b1,b2,c1,c2,d1,d2, c: TPoint; d: double;
begin
  if i < lp.N then
  if i >= 0 then with line^ do
  if j1 > 0 then if j1 <= N then
  if j2 > 0 then if j2 <= N then begin

    if j1 < j2 then begin
      p1:=Pol[j1-1]; p2:=Pol[j1]
    end else begin
      p1:=Pol[j1]; p2:=Pol[j1-1]
    end;

    d:=Dist_to_Line(lp.Pol[i], p1,p2);

    if Abs(Abs(d)-r) <= 1.1 then

    while j1 <> j2 do begin

      if j1 < j2 then begin
        Inc(j1); p3:=Pol[j1]
      end
      else begin
        Dec(j1); p3:=Pol[j1-1]
      end;

      Stretch_Point(p1,p2, r,r, a1,a2);
      Stretch_Point(p2,p1, r,r, b2,b1);
      Stretch_Point(p2,p3, r,r, c1,c2);
      Stretch_Point(p3,p2, r,r, d2,d1);

      if d > 0 then begin
        a1:=a2; b1:=b2; c1:=c2; d1:=d2
      end;

      if Abs(xAngle(p2.x,p2.y,p1.x,p1.y,p3.x,p3.y)-Pi) <= Pi/180 then
        i:=xPoly_Insert(lp,i,max,b1)
      else
      if ILL2(a1,b1, c1,d1, c) then
      i:=xPoly_Insert(lp,i,max,c);

      p1:=p2; p2:=p3
    end

  end; Result:=i
end;

function lmov_lp(lp,line: PLLine; i1,i2,max, j1,j2: Integer; r: double): Integer;
var
  i,k: Integer;
begin
  Result:=i2; if i1 > 0 then if i1+1 < i2 then begin

    k:=i2-i1-1; if k > 0 then with lp^ do begin
      Dec(N,k); for i:=i1 to N do Pol[i]:=Pol[i+k];
    end;

    Result:=Insert_rib(lp,line, i1-1,max, j1,j2, r)+2
  end
end;

function Enabled_rib(p1,p2: TPoint; lp: PLLine; j1,j2: Integer; w: double): boolean;
var
  d1,d2: double; c: TPoint;
begin
  Result:=false; with lp^ do begin

    Middle_Point(p1,p2,c); w:=w*1.2;

    d1:=Dist_to_Line(c,Pol[j1-1],Pol[j1]);
    d2:=Dist_to_Line(c,Pol[j2-1],Pol[j2]);

    if (d1 > 0) = (d2 > 0) then
    if Abs(d1) <= w then if Abs(d2) <= w then
    Result:=true
  end
end;

function Allign_PolyLine(lp,line: PLLine; d,w: double; max: Integer): boolean;
var
  i,j,j1,j2, i_1,j_1: Integer; lock: boolean;
  p1,p2,p3,pc, q1,q2: TPoint; cx,cy,r,d1,d2: double;
begin
  if d < 0 then d:=0;

  Result:=false; with lp^ do

  if N > 0 then begin
    Centre_Polygon(lp, cx,cy);
    pc.x:=Round(cx); pc.y:=Round(cy);

    lock:=PolyLock(lp); r:=d/2; i_1:=-1;
    p1:=Pol[0]; j1:=lp_ContainsPoint(line,w,p1);

    i:=0; while i < N do begin Inc(i);
      p2:=Pol[i]; j2:=lp_ContainsPoint(line,w,p2);

      if (j1 > 0) and (j2 > 0) and
         not lp_Break(lp,line, i-1,j1,j2, w) and
        (lp_ContainsCentre(line,w,p1,p2) > 0) then
      begin
        if i_1 < 0 then begin i_1:=i; j_1:=j1;

          p3:=p1; if i > 1 then p3:=Pol[i-2] else
          if lock then p3:=Pol[N-1]; if not lock then pc:=p2;

          Allign_rib(p3,p1,pc, line.Pol[j1-1],line.Pol[j1],r, p1);
        end;

        p3:=p2; if i < N then p3:=Pol[i+1] else
        if lock then p3:=Pol[1];

        Allign_rib(p3,p2,p1, line.Pol[j2],line.Pol[j2-1],r, p2);

        if lock then if i = 1 then Pol[N]:=p1;

        Pol[i-1]:=p1; Pol[i]:=p2;

        if j1 = j2 then begin
          q1:=line.Pol[j1-1]; q2:=line.Pol[j1];
          d1:=Dist_to_Line(p1, q1,q2);
          d2:=Dist_to_Line(p2, q1,q2);

          if (d1 > 0) <> (d2 > 0) then begin
            i:=lmov_lp(lp,line,i_1,i,max, j_1,j1,r);
            i_1:=i+1; j_1:=j2
          end
        end;

        if lock then if i = N then Pol[0]:=Pol[N];

        Result:=true
      end else
      if i_1 >= 0 then begin
        i:=lmov_lp(lp,line,i_1,i,max, j_1,j1,r);
        i_1:=-1
      end else
      if j1 > 0 then begin
        Allign_Vertex(p1, line.Pol[j1-1],line.Pol[j1],r, p1);
        Pol[i-1]:=p1
      end;

      p1:=p2; j1:=j2
    end;

    lmov_lp(lp,line,i_1,i+1,max, j_1,j1,r);
  end
end;

constructor TCutPoly.Create;
begin
  inherited;
  Child_lp:=Alloc_LLine(LPolyMax+1);
  Child_lp.N:=-1
end;

destructor TCutPoly.Destroy;
begin
  xFreePtr(Child_lp);
  inherited
end;

function TCutPoly.dst_Point(const p: TPoint): Boolean;
begin
  Result:=false;

  if not Child_Enabled then

    Result:=inherited dst_Point(p)

  else
  with Child_lp^ do begin
    if N < LPolyMax then
    Inc(N); Pol[N]:=p
  end
end;

procedure TCutPoly.Open_Child;
begin
  old_Stack_sp:=Stack_sp;
  Child_Enabled:=true;
  Child_lp.N:=-1
end;

procedure TCutPoly.Close_Child;

function Child_Contains_cut_lp: Boolean;
var
  i,n: Integer;
begin
  Result:=false; n:=cut_n;
  if Points_Equal(cut_lp[0],cut_lp[n]) then Dec(n);

  for i:=0 to n do
  if xPolyGonContainsPoint(Child_lp,cut_lp[i]) > 0 then
  begin Result:=true; Break end
end;

function Lock_Child: Boolean;
var
  lp_N: Integer;
begin
  Result:=false; lp_N:=Child_lp.N;

  with Child_lp^ do begin
    if N < LPolyMax then Inc(N);
    Pol[N]:=Pol[0];
  end;

  if Child_Contains_cut_lp then
    Child_lp.N:=lp_N
  else begin
    lp_Dump(Child_lp);
    Child_Enabled:=false;
    Stack_sp:=old_Stack_sp;
    Child_lp.N:=-1;
    Result:=true
  end
end;

function Lock_planes(plane1,plane2: Integer;
                     forw,put: Boolean): Boolean;
var
  iplane,v,sp: Integer; p: TPoint;
begin
  Result:=true; sp:=0;

  iplane:=plane1;
  repeat
    v:=iplane; if forw then begin
      Inc(iplane); if iplane > 4 then
      iplane:=1
    end
    else begin
      Dec(v); if v = 0 then v:=4;
      Dec(iplane); if iplane = 0 then
      iplane:=4
    end;

    p:=Clip_v(v);
    if cut_lp_Contains(p) then begin

      if put then
        with Child_lp^ do begin
          if N < LPolyMax then begin
            Inc(sp); Inc(N); Pol[N]:=p
          end
        end
      else Stack_push(v);

    end
    else begin
      Result:=false; Break
    end

  until iplane = plane2;

  if put then begin

    if Result then
    Result:=Lock_Child;

    if Result then
      Lock_planes(plane2,plane1,not forw,false)
    else
    while sp > 0 do begin
      Dec(sp); Dec(Child_lp.N)
    end

  end  
end;

var
  p1,p2,c: TPoint;
  plane1,plane2: Integer;
begin
  if Child_Enabled then
  with Child_lp^ do begin

    if N > 0 then begin
      p1:=Pol[0]; plane1:=Get_Plane(p1,-1);
      p2:=Pol[N]; plane2:=Get_Plane(p2,-1);

      if plane1 = plane2 then begin
        Middle_Point(p1,p2,c);

        if cut_lp_Contains(c) then
          Lock_Child
      end else
      if not Lock_planes(plane2,plane1,true,true) then
      Lock_planes(plane2,plane1,false,true)

    end;

    Clear_Child;
  end
end;

procedure TCutPoly.Clear_Child;
var
  i: Integer;
begin
  if Child_Enabled then begin
    Child_Enabled:=false;
    with Child_lp^ do
    for i:=0 to N do
    dst_Point(Pol[i]);
  end;

  Child_lp.N:=-1;
end;

constructor TAlignPoly.Create(d,w: double);
begin
  inherited Create;

  if d < 0 then d:=0; if w < d then w:=d;
  if w < 1 then w:=1; _r:=d/2; _w:=w
end;

function TAlignPoly.Align_lp(Line,lp: PLLine; max: Integer): boolean;
var
  i, j1,j2: Integer; cx,cy,r: double;
  pc,p1,p2,q1,q2: TPoint; src_lp: PLLine;
  lp_lock: Boolean;
begin
  Result:=false; _Line:=Line;

  src_lp:=nil; if PolyLock(lp) then
  src_lp:=Copy_PolyBuf(lp,0);

  lp_lock:=PolyLock(lp);

  if lp_lock then
  with lp^ do if N > 0 then begin

    Centre_Polygon(lp, cx,cy);
    pc.x:=Round(cx); pc.y:=Round(cy);

    p1:=Pol[0]; i:=0;

    while i < N-1 do begin
      Inc(i); p2:=Pol[i];

      j1:=mov_Point(p1,p2,pc, q1);
      j2:=mov_Point(p2,p1,pc, q2);

      if j1 > 0 then
      if j2 > 0 then begin
        Pol[i-1]:=q1; if i = 1 then
        if lp_lock then Pol[N]:=q1;
        Pol[i]:=q2; Result:=true
      end;

      p1:=p2
    end
  end;

  if src_lp <> nil then
  Stretch(src_lp,lp)
end;

function TAlignPoly.mov_Point(p1,p2,pc: TPoint; var p: TPoint): Integer;
var
  d1,d2,d3, t1,t2,t, len: double;
  i: Integer; q1,q2,q3,q,c, lt,rb: TPoint;

begin
  Result:=-1; with _Line^ do
  if N > 0 then begin

    i:=0; q1:=Pol[0]; t1:=-1; t2:=-1;

    for i:=1 to N do begin q2:=Pol[i];

      if not Points_Equal(q1,q2) then begin

        d1:=Dist_to_Line(p1 ,q1,q2);
        d2:=Dist_to_Line(p2 ,q1,q2);
        d3:=Dist_to_Line(pc ,q1,q2);

        if Abs(d3) > _r then
        if Abs(d1-d2) < _r then

        if Abs(d1) < _w then
        if Abs(d2) < _w then

        if ILL2(pc,p2, q1,q2, c) then
        if ILL2(pc,p1, q1,q2, c) then

        if Project_to_Line(p1 ,q1,q2, q) then
        begin
          Swap_lRect(q1,q2, lt,rb);

          if PortContainsPoint(lt,rb, c.x,c.y) then begin
            if (t1 < 0) or (Abs(d1) < t1) then begin
              Result:=i; t1:=Abs(d1); q3:=q
            end
          end else

          if t1 < 0 then begin
            t:=Min(Long_Dist(q,q1),Long_Dist(q,q2));
            if (t2 < 0) or (t < t2) then begin
              Result:=i; t2:=t; q3:=q
            end
          end;

          if Result > 0 then begin
            len:=Long_Dist(pc,c)/Abs(d3)*(Abs(d3)-_r);
            Get_Next_Point(pc,c,len, p)
          end

        end
      end;

      q1:=q2
    end;

    if Result > 0 then begin
      q1:=Pol[Result-1]; q2:=Pol[Result];
      t1:=Long_Dist(q1,q3); t2:=Long_Dist(q2,q3);

      if t1 < t2 then begin
        i:=Result-2; Swap_LPoints(q1,q2)
      end else i:=Result+1;

      if i >= 0 then
      if i <= N then begin
        q3:=Pol[i];

        d1:=Dist_to_Line(p ,q1,q2);
        d2:=Dist_to_Line(p ,q2,q3);
        d3:=Dist_to_Line(pc ,q2,q3);

        if Abs(d2) < _w then
        if Abs(d3) > _r then

        if (d1 < 0) = (d2 < 0) then
        if (d1 < 0) = (d3 < 0) then begin
          q.x:=p.x - q2.x + q1.x;
          q.y:=p.y - q2.y + q1.y;

          if ILL2(p,q, q2,q3, c) then begin
            len:=Long_Dist(c,p)/Abs(d2)*_r;
            Get_Next_Point(c,p,len, p)
          end
        end
      end
    end
  end
end;

procedure TAlignPoly.Stretch(src_lp,dst_lp: PLLine);
begin
end;

function Dup_Polyline(lp,cut: PLLine; Max: Integer): boolean;
var
  i,j: Integer; p,_p, q1,q2: TPoint;
begin
  Result:=false;

  if lp.N > 0 then
  if lp.N < Max then

  for i:=0 to cut.N do begin
    p:=cut.Pol[i]; q2:=lp.Pol[0];

    j:=1; while j <= lp.N do begin
      q1:=q2; q2:=lp.Pol[j]; Inc(j);

      if not Points_Equal(p,q1) then
      if not Points_Equal(p,q2) then

      if LineContainsPoint(q1,q2, p) then begin
        Poly_Insert(lp,j-2,Max,p);
        Result:=true; Break
      end
    end
  end
end;

function Clip_Signs(lp,clip: PLLine;  hp: PIntegers;
                    const lt,rb: TPoint): Integer;
var
  i,o,h: Integer; p: TPoint;
begin
  o:=-1;
  for i:=0 to lp.N do begin
    p:=lp.Pol[i]; h:=0;
    if Assigned(hp) then h:=hp[i];

    if PortContainsPoint(lt,rb, p.x,p.y) then

    if clip = nil then begin
      Inc(o); lp.Pol[o]:=p;
    if Assigned(hp) then hp[o]:=h;
    end else

    with clip^ do
    if xPolygonContainsPoint(clip,p) >= 0 then begin
      Inc(o); lp.Pol[o]:=p;
      if Assigned(hp) then hp[o]:=h;
    end

  end; lp.N:=o
end;

end.