unit pmath; interface

uses
  Classes,
  otypes,ofiles,
  xlist,xline,xintf;
  
const
  xy_Affine4    = 0;
  xy_Affine5    = 1;
  xy_Affine6    = 2;
  xy_Projective = 3;
  xy_Polynom    = 4;

  Polynom_Max = 1+2+3+4+5;

  polynom_N: Array[0..4] of Integer = (1,3,6,10,15);
  polynom_N1: Array[0..4] of Integer = (1,3,5,9,14);

  xy_flag_geo   = 1;
  xy_flag_ctrl  = 2;

type
  TPolynomCoeff  = Array[0..Polynom_Max-1] of Double;
  TPolynomCoeff2 = Array[0..Polynom_Max*2-1] of Double;

  TPolynomDiff = record
    kx,ky: TPolynomCoeff2
  end;

  PPolynom = ^TPolynom;
  TPolynom = record
    Order: Integer;
    a,b: TPolynomCoeff;
    x0,y0,kz: Double;
  end;

  ITransit2 = interface(IUnknown)
    ['{6B8188DC-E361-48AD-A38C-8CA8A0FF6EC4}']
    procedure Clear; stdcall;
    procedure Add(p1,p2: PGauss); stdcall;
    procedure IsFloat(Value: Boolean); stdcall;
    function CalcAffine(Typ: Integer; tr,bt: PDoubles): Boolean; stdcall;
    function CalcPolynom(Rank: Integer; tr: PPolynom): Boolean; stdcall;
    procedure GetError(m: PDoubles); stdcall;
  end;

type
  pxy_xy = ^txy_xy;
  txy_xy = record
    Id, Ind1,Ind2,Ph1,Ph2, Grp,Blk: Integer;
    Weight: Int16; Flags: Word;
    p1,p2,pv: TGauss; W: Double
  end;

  pxy_xy_array = ^txy_xy_array;
  txy_xy_array = array[0..255] of txy_xy;

  XY_XY_List = class(TCustomList)
    constructor Create;

    function Indexof1(Id,Ind1: int): int;

    function rAdd(const r: txy_xy): Integer;

    function xAdd(Id,Ind1,Ind2,Ph1,Ph2: Integer;
                  const p1,p2: TGauss): Integer;

    function wAdd(Id,Ind1,Ind2,Ph1,Ph2,W: Integer;
                  const p1,p2: TGauss): Integer;

    function pAdd(const p1,p2: TGauss): Integer;

    procedure SetBlock(Blk: Integer);
    procedure SetFlag(Flag: Integer);

    function pack3(const c: TGauss): int;

  protected
    function Up_item(p1,p2: Pointer): Boolean; override;
  end;

function Identify_poly: TPolynom;
function poly_nil: TPolynom;

procedure xy_poly_Begin(out P: TPolynom; dx,dy,kx,ky: Double);
function xy_poly_xy(x,y: Double; const P: TPolynom): TGauss;
function xy_poly_xy1(x,y: Double; const P: TPolynom): TGauss;
function xy_poly_pix(x,y: Double; const P: TPolynom): TPoint;

procedure xy_poly_swap(var P: TPolynom);

function xy_poly_t3x3(const P: TPolynom): Real3x3;
function t3x3_xy_poly(const T: Real3x3): TPolynom;
function proj_xy_poly(const T: Real3x3): TPolynom;

procedure xy_poly_scale(var P: TPolynom; kx,ky: Double);
procedure xy_poly_Move(var P: TPolynom; dx,dy: Double);

procedure ixy_poly_scale(var P: TPolynom; kz: Double);
procedure ixy_poly_Move(var P: TPolynom; dx,dy: Double);

function ArrayStr(P: PDoubles; N: Integer): String;
function ArrayFloatStr(P: PDoubles; N,M: Integer): String;

procedure log_3x3(log: ILog; const T: Real3x3);
procedure log_Polynom(log: ILog; const P: TPolynom);

procedure txt_Put_3x3(txt: TTextfile; const T: Real3x3);
function txt_Get_3x3(txt: TTextfile; out T: Real3x3): Boolean;

procedure txt_Put_Polynom(txt: TTextfile; const P: TPolynom);
function txt_Get_Polynom(txt: TTextfile; out P: TPolynom): Boolean;

function GetPMathIntf(const IID: TGUID; var Obj): HResult;

function pp_pack3(const c: TGauss;
                  const ag,bg: PGPoly;
                  Count,Inner: int): int;

function pp_solve_sau(sp: PGPoly; sn: Integer; eps: Float;
                      out tr,bt: TPolynom; out sys: tsys): Boolean;

implementation

uses
  Sysutils,dynlibs,convert;

function Identify_poly: TPolynom;
var
  P: TPolynom;
begin
  Fillchar(P,Sizeof(P),0); P.Order:=1;
  P.kz:=1; P.a[1]:=1; P.b[2]:=1;
  Result:=P
end;

function poly_nil: TPolynom;
var
  P: TPolynom;
begin
  Fillchar(P,Sizeof(P),0); Result:=P
end;

procedure xy_poly_Begin(out P: TPolynom; dx,dy,kx,ky: Double);
begin
  Fillchar(P,Sizeof(P),0); P.Order:=1;
  P.a[0]:=dx; P.a[1]:=kx;
  P.b[0]:=dy; P.b[2]:=ky;
end;

function xy_poly_xy(x,y: Double; const P: TPolynom): TGauss;
var
  x_,y_,x2,y2,xy,x3,x2y,xy2,y3: Double;
  x4,x3y,x2y2,xy3,y4,f: Double;
begin
  x_:=x; y_:=y;

  with P do

  if Order = -8 then begin

    f:=1 / (x*a[3] + y*a[4] + 1);
    x_:=(a[0] + x*a[1] + y*a[2]) * f;
    y_:=(b[0] + x*b[1] + y*b[2]) * f

  end
  else begin
    x_:=a[0]; y_:=b[0];

    x:=(x-x0)*kz; y:=(y-y0)*kz;

    if Order > 0 then begin
      x_:=x_ + x*a[1] + y*a[2];
      y_:=y_ + x*b[1] + y*b[2];

      if Order > 1 then begin
        x2:=x*x; xy:=x*y; y2:=y*y;

        x_:=x_ + x2*a[3] + xy*a[4] + y2*a[5];
        y_:=y_ + x2*b[3] + xy*b[4] + y2*b[5];

        if Order > 2 then begin
          x3:=x2*x; x2y:=x2*y; xy2:=x*y2; y3:=y2*y;

          x_:=x_ + x3*a[6] + x2y*a[7] + xy2*a[8] + y3*a[9];
          y_:=y_ + x3*b[6] + x2y*b[7] + xy2*b[8] + y3*b[9];

          if Order > 3 then begin
            x4:=x3*x; x3y:=x3*y; x2y2:=x2*y2; xy3:=x*y3; y4:=y3*y;

            x_:=x_ + x4*a[10] + x3y*a[11] + x2y2*a[12] + xy3*a[13] + y4*a[14];
            y_:=y_ + x4*b[10] + x3y*b[11] + x2y2*b[12] + xy3*b[13] + y4*b[14];
          end
        end
      end
    end;
  end;

  Result.x:=x_; Result.y:=y_;
end;

function xy_poly_xy1(x,y: Double; const P: TPolynom): TGauss;
var
  i,j,k: Integer;
  xk,yk: Array[0..7] of Double;
  x_,y_,f: Double; ax,ay,vk: Extended;
begin
  x_:=x; y_:=y;

  with P do

  if Order = -8 then begin

    f:=1 / (x*a[3] + y*a[4] + 1);
    x_:=(a[0] + x*a[1] + y*a[2]) * f;
    y_:=(b[0] + x*b[1] + y*b[2]) * f

  end
  else begin
    x:=(x-x0)*kz; y:=(y-y0)*kz;

    ax:=1; ay:=1;
    for i:=0 to Order do begin
      xk[i]:=ax; ax:=ax*x;
      yk[i]:=ay; ay:=ay*y;
    end;

    ax:=0; ay:=0;

    k:=0;
    for i:=0 to Order do
    for j:=0 to i do begin
      vk:=xk[i-j]*yk[j];
      ax:=ax + vk*a[k];
      ay:=ay + vk*b[k];
      Inc(k)
    end;

    x_:=ax; y_:=ay;
  end;

  Result.x:=x_; Result.y:=y_;
end;

function xy_poly_pix(x,y: Double; const P: TPolynom): TPoint;
var
  t: TGauss;
begin
  t:=xy_poly_xy(x,y,P);
  Result.x:=Round(t.x);
  Result.y:=Round(t.y);
end;

procedure xy_poly_swap(var P: TPolynom);
var
  T: TPolynom;
begin
  T:=P; P.a:=T.b; P.b:=T.a
end;

function xy_poly_t3x3(const P: TPolynom): Real3x3;
var
  T: Real3x3;
begin
  with P do begin
    T[1,1]:=a[1]*kz; T[1,2]:=a[2]*kz;
    T[1,3]:=a[0] - a[1]*x0*kz - a[2]*y0*kz;

    T[2,1]:=b[1]*kz; T[2,2]:=b[2]*kz;
    T[2,3]:=b[0] - b[1]*x0*kz - b[2]*y0*kz;

    T[3,1]:=0; T[3,2]:=0; T[3,3]:=1;
  end; Result:=T
end;

function t3x3_xy_poly(const T: Real3x3): TPolynom;
var
  P: TPolynom;
begin
  P:=Identify_poly; P.Order:=1;
  P.a[0]:=T[1,3]; P.a[1]:=T[1,1]; P.a[2]:=T[1,2];
  P.b[0]:=T[2,3]; P.b[1]:=T[2,1]; P.b[2]:=T[2,2];
  if Is_Projective_3x3(T) then P:=proj_xy_poly(T);
  Result:=P;
end;

function proj_xy_poly(const T: Real3x3): TPolynom;
var
  P: TPolynom;
begin
  P:=Identify_poly; P.Order:=-8;
  P.a[0]:=T[1,3]; P.a[1]:=T[1,1]; P.a[2]:=T[1,2];
  P.b[0]:=T[2,3]; P.b[1]:=T[2,1]; P.b[2]:=T[2,2];
  P.a[3]:=T[3,1]; P.a[4]:=T[3,2]; Result:=P;
end;

function poly_xy_proj(const P: TPolynom): Real3x3;
var
  T: Real3x3;
begin
  T:=Identify_3x3;
  T[1,3]:=P.a[0]; T[1,1]:=P.a[1]; T[1,2]:=P.a[2];
  T[2,3]:=P.b[0]; T[2,1]:=P.b[1]; T[2,2]:=P.b[2];
  T[3,1]:=P.a[3]; T[3,2]:=P.a[4]; Result:=T
end;

procedure xy_poly_scale(var P: TPolynom; kx,ky: Double);
var
  i: Integer; tr: Real3x3;
begin
  if P.Order = -8 then begin
    tr:=poly_xy_proj(P);
    xy_Scale_3x3(tr, kx,ky);
    P:=proj_xy_poly(tr)
  end else

  for i:=0 to Polynom_Max-1 do begin
    P.a[i]:=P.a[i] * kx;
    P.b[i]:=P.b[i] * ky;
  end
end;

procedure xy_poly_Move(var P: TPolynom; dx,dy: Double);
var
  tr: Real3x3;
begin
  if P.Order = -8 then begin
    tr:=poly_xy_proj(P);
    t_Move_3x3(tr, dx,dy);
    P:=proj_xy_poly(tr)
  end
  else begin
    P.a[0]:=P.a[0] + dx;
    P.b[0]:=P.b[0] + dy;
  end
end;

procedure ixy_poly_scale(var P: TPolynom; kz: Double);
var
  k: Double;
begin
  k:=1 / kz;
  P.x0:=P.x0 * k;
  P.y0:=P.y0 * k;
  P.kz:=P.kz * kz;
end;

procedure ixy_poly_Move(var P: TPolynom; dx,dy: Double);
begin
  P.x0:=P.x0 + dx;
  P.y0:=P.y0 + dy;
end;

function ArrayStr(P: PDoubles; N: Integer): String;
var
  i,l: Integer; s: String;
begin
  s:=''; l:=16;

  while N*(l+1) > 240 do Dec(l);

  for i:=1 to N do begin
    if length(s) > 0 then s:=s+' ';
    s:=s + ValToStr(P[0],l);
    P:=@P[1]
  end;

  Result:=s
end;

function ArrayFloatStr(P: PDoubles; N,M: Integer): String;
var
  i: Integer; s,s1: String;
begin
  s:=''; s1:='%0.'+IntToStr(M)+'e';
  for i:=0 to N-1 do begin
    if length(s) > 0 then s:=s+' ';
    s:=s + Format(s1,[P[i]]);
  end; Result:=s
end;

procedure log_3x3(log: ILog; const T: Real3x3);
var
  i: Integer; s: TShortstr;
begin
  for i:=1 to 3 do begin
    StrPCopy(s,ArrayStr(@T[i,1],3));
    log.Add(s);
  end
end;

procedure log_Polynom(log: ILog; const P: TPolynom);
var
  n: Integer; s: TShortstr;
begin
  if (P.Order <= 0)
  or (P.Order > 4) then
    log.Add('-1')
  else begin

    StrPCopy(s,ValToStr(P.x0,16)+' '+
               ValToStr(P.y0,16)+' '+
               ValToStr(P.kz,16));

    log.Add(s);

    n:=polynom_N[P.Order];
    StrPCopy(s, IntToStr(P.Order)+' '+ArrayStr(@P.a,n) );
    log.Add(s);

    StrPCopy(s, IntToStr(P.Order)+' '+ArrayStr(@P.b,n) );
    log.Add(s);
  end
end;

procedure txt_Put_3x3(txt: TTextfile; const T: Real3x3);
var
  i: Integer;
begin
  for i:=1 to 3 do
  txt.WriteStr( ArrayStr(@T[i,1],3) );
end;

function txt_Get_3x3(txt: TTextfile; out T: Real3x3): Boolean;
var
  i,rc: Integer;
begin
  rc:=0; T:=identify_3x3;

  for i:=1 to 3 do
  if txt.xStrLine > nil then
  if txt.x_Double(T[i,1]) then
  if txt.x_Double(T[i,2]) then
  if txt.x_Double(T[i,3]) then
  Inc(rc);

  Result:=rc in [2,3];
end;

procedure txt_Put_Polynom(txt: TTextfile; const P: TPolynom);
var
  n: Integer; s: String;
begin
  if (P.Order <= 0)
  or (P.Order > 4) then
    txt.WriteStr('-1')
  else begin

    s:=ValToStr(P.x0,16)+' '+
       ValToStr(P.y0,16)+' '+
       ValToStr(P.kz,16);

    txt.WriteStr(s);

    n:=polynom_N[P.Order];
    txt.WriteStr( IntToStr(P.Order)+' '+ArrayStr(@P.a,n) );
    txt.WriteStr( IntToStr(P.Order)+' '+ArrayStr(@P.b,n) );
  end
end;

function txt_Get_Polynom(txt: TTextfile; out P: TPolynom): Boolean;
var
  r1,r2,n: Integer;
begin
  Result:=false; P:=Identify_poly;

  if txt.xStrLine > nil then
  if txt.x_Double(P.x0) then
  if txt.x_Double(P.y0) then
  if txt.x_Double(P.kz) then

  if txt.xStrLine > nil then
  if txt.x_Int(r1) then
  if r1 in [1..4] then begin

    P.Order:=r1; n:=polynom_N[r1];

    if txt.x_Array(@P.a,n) then

    if txt.xStrLine <> nil then
    if txt.x_Int(r2) and (r1 = r2) then
    Result:=txt.x_Array(@P.b,n)
  end
end;

constructor XY_XY_List.Create;
begin
  inherited Create(Sizeof(txy_xy),64)
end;

function XY_XY_List.Up_item(p1,p2: Pointer): Boolean;
var
  pp1,pp2: pxy_xy;
begin
  pp1:=p1; pp2:=p2;
  Result:=Abs(pp2.W) > Abs(pp1.W);
end;

function XY_XY_List.Indexof1(Id,Ind1: int): int;
var
  i: Integer; lp: pxy_xy_array;
begin
  Result:=-1; lp:=First;

  for i:=0 to Count-1 do
  if lp[i].Id = Id then
  if lp[i].Ind1 = Ind1 then begin
    Result:=i; Break
  end
end;

function XY_XY_List.rAdd(const r: txy_xy): Integer;
var
  i: Integer; lp: pxy_xy_array;
begin
  Result:=-1; lp:=First;

  for i:=0 to Count-1 do begin
    if r.pv.x < lp[0].pv.x then begin
      Result:=Insert_range(@r,i,1); Break
    end;
    lp:=@lp[1]
  end;

  if Result < 0 then Result:=Add(@r)
end;

function XY_XY_List.xAdd(Id,Ind1,Ind2,Ph1,Ph2: Integer;
                         const p1,p2: TGauss): Integer;
begin
  Result:=wAdd(Id,Ind1,Ind2,Ph1,Ph2,0, p1,p2)
end;

function XY_XY_List.wAdd(Id,Ind1,Ind2,Ph1,Ph2,W: Integer;
                         const p1,p2: TGauss): Integer;
var
  pp: txy_xy;
begin
  Fillchar(pp,Sizeof(pp),0);
  pp.Id:=Id; pp.Ind1:=Ind1; pp.Ind2:=Ind2;
  pp.Ph1:=Ph1; pp.Ph2:=Ph2; pp.p1:=p1; pp.p2:=p2;
  pp.Weight:=W; Result:=Add(@pp)
end;

function XY_XY_List.pAdd(const p1,p2: TGauss): Integer;
var
  pp: txy_xy;
begin
  Fillchar(pp,Sizeof(pp),0);
  pp.p1:=p1; pp.p2:=p2;
  Result:=Add(@pp)
end;

procedure XY_XY_List.SetBlock(Blk: Integer);
var
  i: Integer; lp: pxy_xy_array;
begin
  lp:=First;
  for i:=0 to Count-1 do
  lp[i].Blk:=Blk
end;

procedure XY_XY_List.SetFlag(Flag: Integer);
var
  i: Integer; lp: pxy_xy_array;
begin
  lp:=First;
  for i:=0 to Count-1 do
  with lp[i] do Flags:=Flags or Flag
end;

function XY_XY_List.pack3(const c: TGauss): int;
var
  i,n: int; lp: pxy_xy_array;
  a,b: TGPoly;
begin
  n:=Count;
  if n < 64 then begin

    lp:=First;
    for i:=0 to n-1 do begin
      a[i]:=lp[i].p1;
      b[i]:=lp[i].p2;
    end;

    n:=pp_pack3(c, @a,@b,n,1);

    if n >= 3 then begin

      for i:=0 to n-1 do begin
        lp[i].p1:=a[i];
        lp[i].p2:=b[i];
      end;

      Truncate(n);
    end
  end;

  Result:=Count
end;

function Trianlge_better(const p1,p2, q1,q2: TGauss): Boolean;
var
  r11,r12,r21,r22: Double;
begin
  Result:=false;

  r11:=Gauss_Dist(q1,p1);
  r12:=Gauss_Dist(q1,p2);

  r21:=Gauss_Dist(q2,p1);
  r22:=Gauss_Dist(q2,p2);

  Result:=r21 + r22 < r11 + r12
end;

function IsTriangle(const p1,p2,p3: TGauss): Boolean;
var
  sp,vp,x21,x31,y21,y31: Double;
begin
  x21:=p2.x-p1.x; x31:=p3.x-p1.x;
  y21:=p2.y-p1.y; y31:=p3.y-p1.y;

  sp:=x21*x31 + y21*y31;
  vp:=y31*x21 - x31*y21;

  Result:=Abs(vp) > Abs(sp) * Small
end;

function spdt(const p1,p2,p3: TGauss): Double;
begin
  Result:=(p2.x-p1.x)*(p3.x-p1.x)+(p2.y-p1.y)*(p3.y-p1.y)
end;

function vpdt(const p1,p2,p3: TGauss): Double;
begin
  Result:=(p3.y-p1.y)*(p2.x-p1.x)-(p3.x-p1.x)*(p2.y-p1.y)
end;

function iOnLine(const a,b, p: TGauss): Integer;
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

function TriangleContainsPoint(const a,b,c, p: TGauss): bool;
var
  d1,d2,d3: int;
begin
  Result:=false;
  d1:=iOnLine(a,b, p);
  if d1 <> 0 then begin
    d2:=iOnLine(b,c, p);
    if d1*d2 > 0 then begin
      d3:=iOnLine(c,a, p);
      Result:=d1*d3 > 0
    end
  end
end;

function pp_pack3(const c: TGauss;
                  const ag,bg: PGPoly;
                  Count,Inner: int): int;
var
  i,j1,j2,j3: int; r1,r2,r: Double;
  p,q, p1,p2,p3, q1,q2,q3: TGauss;
begin
  Result:=0;

  if Count = 3 then Result:=3
  else
  if Count > 3 then begin

    j1:=0; r1:=Gauss_Dist(c,ag[0]);
    j2:=-1; r2:=-1; j3:=-1;

    for i:=1 to Count-1 do begin

      p:=ag[i]; r:=Gauss_Dist(c,p);

      if r < r1 then begin
        j2:=j1; r2:=r1; j1:=i; r1:=r
      end else
      if (j2 < 0) or (r < r2) then begin
        j2:=i; r2:=r
      end
    end;

    if (j2 >= 0) and (j1 <> j2) then begin

      p1:=ag[j1]; q1:=bg[j1];
      p2:=ag[j2]; q2:=bg[j2];

      for i:=0 to Count-1 do
      if (i <> j1) and (i <> j2) then begin
        p:=ag[i]; q:=bg[i];

        if IsTriangle(p1,p2,p) then
        if IsTriangle(q1,q2,q) then

        if (j3 < 0)
        or Trianlge_better(p1,p2, p3,p) then
        begin j3:=i; p3:=p; q3:=q end
      end;

      if (j3 >= 0) then
      if (j1 <> j3) and (j2 <> j3) then

      if (Inner = 0)
      or TriangleContainsPoint(p1,p2,p3, c) then begin
        ag[0]:=p1; bg[0]:=q1;
        ag[1]:=p2; bg[1]:=q2;
        ag[2]:=p3; bg[2]:=q3;
        Result:=3
      end
    end
  end
end;

var
  dll_pp2: THandle;

function GetPMathIntf(const IID: TGUID; var Obj): HResult;
type
  tfunc = function(const CLSID,IID: TGUID; var Obj): HResult; stdcall;
var
  func: tfunc;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  if dll_pp2 = 0 then
  dll_pp2:=LoadLibrary('dll_pp2.dll');

  if dll_pp2 >= 32 then begin
    @func:=GetProcAddress(dll_pp2,'DllGetInterface');
    if Assigned(func) then Result:=func(IID,IID,Obj)
  end
end;

function pp_solve_sau(sp: PGPoly; sn: Integer; eps: Float;
                      out tr,bt: TPolynom; out sys: tsys): Boolean;

type
  tfunc = function(sp: PGPoly; sn: Integer; eps: Float;
                   out tr,bt: TPolynom; out sys: tsys): Boolean; stdcall;
var
  func: tfunc;
begin
  Result:=false;

  if dll_pp2 = 0 then
  dll_pp2:=LoadLibrary('dll_pp2.dll');

  if dll_pp2 >= 32 then begin
    @func:=GetProcAddress(dll_pp2,'solve_sau');
    if Assigned(func) then Result:=func(sp,sn,eps, tr,bt, sys)
  end
end;

initialization
begin
  dll_pp2:=0;
end;

finalization
begin
  if dll_pp2 >= 32 then
  FreeLibrary(dll_pp2);
end;

end.