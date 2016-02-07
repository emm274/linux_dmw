unit tri_link; interface

uses
  Windows,Math,
  OTypes,OTypes1,XLine,XList,
  pmath,OGauss;

type
  TLinkPoint1 = record a,b: TGauss end;

  PLinkArray1 = ^TLinkArray1;
  TLinkArray1 = Array[0..1023] of TLinkPoint1;

  TLinkList1 = class(TCustomList)
    constructor Create;
  end;

  TTriLink = class

    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function GetTriTested: Float;

    procedure SetAffine(const tr,bt: Real3x3);

    function xAdd(const a,b: TGauss): Integer;

    function Calc_tri: Integer;

    function Get_tri(i: Integer; out t1,t2: GOrient): Integer;

    function a_to_b(x,y: Double; out b: TGauss): Boolean;
    function b_to_a(x,y: Double; out a: TGauss): Boolean;

  private
    fverts: TLinkList1;
    ftemp: TPointList;

    fTops: PLinkArray1;
    fTris: PTriArray2;
    fTris1: PTriArray2;

    ftriCount: Integer;
    ftriIndex: Integer;

    ftri_a0,ftri_b0: TGauss;

    fTriTested1: Int64;
    fTriTested2: Integer;

    t_ab: ab_Matrix;
    t_ba: ab_Matrix;

    ftri_a,ftri_b: GOrient;

    fTriBuffer: TTriArray2;

    function Seek_tri(px,py: Double; map: Boolean): Boolean;

  public
    property Tris: PTriArray2 read fTris;
    property Count: Integer read ftriCount;

    property triIndex: Integer read ftriIndex;
  end;

  PTriangle = ^TTriangle;
  TTriangle = record
    x0,y0,x1,y1,x2,y2, cx,cy: Double;
    a1,b1,c1, a2,b2,c2, a3,b3,c3: Double;
    min1,max1, min2,max2, min3,max3: TGauss
  end;

  PTriItem = ^TTriItem;
  TTriItem = record
    A,B: TTriangle; T1,T2: Real3x3;
    Ind1,Dir1,Ind2,Dir2,Ind3,Dir3: Integer;
  end;

  PTriArray = ^TTriArray;
  TTriArray = array[0..255] of TTriItem;

  TTriList = class(TCustomList)
    constructor Create;

    procedure Clear; override;

    function Assign(List: TTriList): Integer;

    function LoadLink(Link: TTriLink): Integer;

    procedure Assign_3x3(const AForw,ABack: Real3x3);
    function xAdd(A,B: PGPoly): Integer;

    function a_to_b(x,y: Double): TGauss;
    function b_to_a(x,y: Double): TGauss;

    procedure Get_B_Triangle(I: Integer; G: PGPoly);

  private
    fforw_t,fback_t: Real3x3;
    fforw_p,fback_p: TPolynom;

    OccupeIndex: Integer;
    Occupe: TTriangle;

  public
    property forw_t: Real3x3 read fforw_t;
    property back_t: Real3x3 read fback_t;

    property forw_p: TPolynom read fforw_p
                              write fforw_p;

    property back_p: TPolynom read fback_p
                              write fback_p;
 end;

implementation

uses
  dynlibs;

constructor TLinkList1.Create;
begin
  inherited Create(Sizeof(TLinkPoint1),1024);
end;

constructor TTriLink.Create;
begin
  inherited Create;

  fverts:=TLinkList1.Create;
  ftemp:=TPointList.Create(1024)
end;

destructor TTriLink.Destroy;
begin
  fTris1:=xFreePtr(fTris1);

  ftemp.Free;
  fverts.Free;

  inherited
end;

procedure TTriLink.Clear;
begin
  ftemp.Clear;
  fverts.Clear;

  ftriIndex:=0; ftriCount:=0;
  fTris1:=xFreePtr(fTris1);
  fTris:=nil;

  t_ab.Reset; t_ba.Reset;
end;

function TTriLink.GetTriTested: Float;
begin
  Result:=0;
  if fTriTested2 > 0 then
  Result:=fTriTested1 / fTriTested2
end;

procedure TTriLink.SetAffine(const tr,bt: Real3x3);
begin
  t_ab.Set_Matrix(tr);
  t_ba.Set_Matrix(bt);
end;

function TTriLink.xAdd(const a,b: TGauss): Integer;
var
  r: TLinkPoint1;
begin
  r.a:=a; r.b:=b; with r.a do
  ftemp.AddItem(Round(x),Round(y));
  Result:=fverts.Add(@r)
end;

function TTriLink.Calc_tri: Integer;

const
  cls = 0.001; 

type
  tfunc = function(var tp2: PTriArray2;
                   lp: PLPoly; ndp: int;
                   cls: Float): int; stdcall;

  tfunca = function(var tp1: PTriArray2;
                    tp1_size: int;

                    var tp2: PTriArray2;
                    lp: PLPoly; ndp: int;
                    cls: Float): int;  stdcall;

var
  lp: PLinkArray1;
  dll: THandle;
  func: tfunc;
  funca: tfunca;
begin
  fTops:=fverts.First;

  ftriIndex:=0; ftriCount:=0;

  fTris1:=xFreePtr(fTris1);
  fTris:=nil;

  if fverts.Count > 3 then begin
    lp:=fverts.First;
    ftri_a0:=lp[0].a;
    ftri_b0:=lp[0].b;

    dll:=LoadLibrary('lib_tri.so');
    if dll >= 32 then begin
      @funca:=GetProcAddress(dll,'Calc_tri2a');

      if Assigned(funca) then begin
        fTris:=@fTriBuffer;
        ftriCount:=funca(fTris,MaxTriCount,fTris1,
                         fTemp.First,fTemp.Count,cls)
      end
      else begin
        @func:=GetProcAddress(dll,'Calc_tri2');
        ftriCount:=func(fTris1,fTemp.First,fTemp.Count,cls);
        fTris:=fTris1
      end;

      FreeLibrary(dll)
    end;

    if ftriCount < 0 then ftriCount:=0;
    fTriTested1:=1; fTriTested2:=1
  end;

  Result:=Count
end;

function TTriLink.Get_tri(i: Integer; out t1,t2: GOrient): Integer;
var
  lp: PLinkArray1; tr: TTri2;
begin
  Result:=-1; Dec(i);
  if (i >= 0) and (i < Count) then begin
    tr:=fTris[i]; lp:=fTops;
    t1[0]:=lp[tr.p1].a; t2[0]:=lp[tr.p1].b;
    t1[1]:=lp[tr.p2].a; t2[1]:=lp[tr.p2].b;
    t1[2]:=lp[tr.p3].a; t2[2]:=lp[tr.p3].b;
    t1[3]:=t1[0];       t2[3]:=t2[0];
    Result:=i
  end
end;

function TTriLink.a_to_b(x,y: Double; out b: TGauss): Boolean;
begin
  Result:=Seek_tri(x,y,false);
  b:=t_ab.Transit(x,y);
end;

function TTriLink.b_to_a(x,y: Double; out a: TGauss): Boolean;
begin
  Result:=Seek_tri(x,y,true);
  a:=t_ba.Transit(x,y)
end;

{$DEFINE METHOD1}

function TTriLink.Seek_tri(px,py: Double; map: Boolean): Boolean;

procedure Link_occupe;
begin
  t_ab.Open(@ftri_a,@ftri_b);
  t_ba.Open(@ftri_b,@ftri_a);
end;

{$IFDEF METHOD1}
var
  i,old,nxt: Integer; dist,tmp: double;
begin
  Result:=false;

  if ftriCount > 0 then begin

    old:=ftriIndex; nxt:=old; dist:=-1;

    if old > 0 then

    if map then
      Result:=InTriangle(@ftri_b, px,py, dist) <> 0
    else
      Result:=InTriangle(@ftri_a, px,py, dist) <> 0;

    if not Result then begin

      old:=ftriIndex; nxt:=old;

      for i:=1 to ftriCount do
      if i <> old then begin
        ftriIndex:=i; Get_tri(i,ftri_a,ftri_b);

        if map then
          Result:=InTriangle(@ftri_b, px,py, tmp) <> 0
        else
          Result:=InTriangle(@ftri_a, px,py, tmp) <> 0;

        if Result then begin
          Link_occupe; Exit
        end;

        if (tmp < dist) or (nxt = 0) then
        begin nxt:=i; dist:=tmp end
      end;

      ftriIndex:=nxt;
      Get_tri(nxt,ftri_a,ftri_b);
      Link_occupe
    end
  end
end;

{$ELSE}
function GetPoint(Ind: Integer; map: Boolean): TPoint;
begin
  with fTops[Ind] do
  if map then begin
    Result.X:=Round((b.x-ftri_b0.x)*fsm_ed);
    Result.Y:=Round((b.y-ftri_b0.y)*fsm_ed);
  end
  else begin
    Result.X:=Round((a.x-ftri_a0.x)*fsm_ed);
    Result.Y:=Round((a.y-ftri_a0.y)*fsm_ed);
  end;
end;

var
  it,it1,it2,rc,k: Integer;
  p,p1,p2,p3: TPoint; tr: TTri2;
begin
  Result:=false;

  fTops:=fVerts.First;

  if Assigned(fTops) then
  if tri_cnt > 0 then begin

    if map then begin
      p.X:=Round((px-ftri_b0.x)*fsm_ed);
      p.Y:=Round((py-ftri_b0.y)*fsm_ed);
    end
    else begin
      p.X:=Round((px-ftri_a0.x)*fsm_ed);
      p.Y:=Round((py-ftri_a0.y)*fsm_ed);
    end;

    it:=ftri_ind-1;
    if it < 0 then it:=0;
    if it >= ftri_cnt then
    it:=ftri_cnt-1;

    it1:=-2; it2:=it; k:=1;
    while it >= 0 do begin

      tr:=fTris[it]; it2:=it; it:=-1;

      p1:=GetPoint(tr.p1,map);
      p2:=GetPoint(tr.p2,map);
      p3:=GetPoint(tr.p3,map);

      rc:=-1; if tr.t3 <> it1 then
      rc:=iline(@p, @p1,@p2);

      if rc = 1 then
        it:=tr.t3
      else begin

        if rc = 2 then begin
          Result:=true; Break;
        end;

        rc:=-1; if tr.t1 <> it1 then
        rc:=iline(@p, @p2,@p3);

        if rc = 1 then
          it:=tr.t1
        else begin
          if rc = 2 then begin
            Result:=true; Break;
          end;

          rc:=-1; if tr.t2 <> it1 then
          rc:=iline(@p, @p3,@p1);

          if rc = 1 then
            it:=tr.t2
          else begin
            Result:=true; Break;
          end
        end
      end;

      it1:=it2; Inc(k)
    end;

    Inc(fTriTested1,k);
    Inc(fTriTested2);

    if it2 >= 0 then
    if it2 <> ftri_ind-1 then begin
      ftri_ind:=it2+1;
      Get_tri(ftri_ind,ftri_a,ftri_b);
      Link_occupe
    end
  end
end;

{$ENDIF}

procedure line_to_abc(x1,y1,x2,y2: Double;
                      out a,b,c: Double; out lt,rb: TGauss);
begin
  a:=y2-y1; b:=x1-x2; c:=y1*x2-x1*y2;
  lt.x:=Min(x1,x2); rb.x:=Max(x1,x2);
  lt.y:=Min(y1,y2); rb.y:=Max(y1,y2);
end;

function Triangle(P: PGPoly): TTriangle;
var
  tr: TTriangle;
begin
  tr.x0:=P[0].x;
  tr.y0:=P[0].y;
  tr.x1:=P[1].x;
  tr.y1:=P[1].y;
  tr.x2:=P[2].x;
  tr.y2:=P[2].y;

  with tr do begin
    cx:=x0/3 + x1/3 + x2/3;
    cy:=y0/3 + y1/3 + y2/3;

    line_to_abc(x0,y0, x1,y1, a1,b1,c1, min1,max1);
    line_to_abc(x1,y1, x2,y2, a2,b2,c2, min2,max2);
    line_to_abc(x2,y2, x0,y0, a3,b3,c3, min3,max3);
  end;

  Result:=tr
end;

function In_Triangle(tri: PTriangle; x,y: Double;
                     out dist: double): Integer;
var
  r1,r2,r3: double;
begin
  Result:=0;

  with tri^ do begin
    dist:=Hypot(x - tri.cx,y - tri.cy);

    r1:=a1*x + b1*y + c1;

    if Abs(r1) < Small then begin
      if (x >= min1.x) and (x <= max1.x) then
      if (y >= min1.y) and (y <= max1.y) then
      Result:=1
    end
    else begin
      r2:=a2*x + b2*y + c2;

      if Abs(r2) < Small then begin
        if (x >= min2.x) and (x <= max2.x) then
        if (y >= min2.y) and (y <= max2.y) then
        Result:=2
      end
      else begin
        r3:=a3*x + b3*y + c3;

        if Abs(r3) < Small then begin
          if (x >= min3.x) and (x <= max3.x) then
          if (y >= min3.y) and (y <= max3.y) then
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
  end
end;

function rib_Direction(x0,y0,x1,y1,x2,y2: Double): Integer;
var
  r: Double;
begin
  Result:=0;
  r:=xDist_to_Line(x2,y2, x0,y0,x1,y1);
  if r < 0 then Result:=-1 else
  if r > 0 then Result:=1
end;

constructor TTriList.Create;
begin
  inherited Create(Sizeof(TTriItem),32);
  fforw_p:=poly_nil; fback_p:=poly_nil;
end;

procedure TTriList.Clear;
begin
  inherited;
  fforw_p:=poly_nil;
  fback_p:=poly_nil;
end;

function TTriList.Assign(List: TTriList): Integer;
begin
  Result:=inherited LoadList(List);
  fforw_t:=List.forw_t;
  fback_t:=List.back_t;
  OccupeIndex:=List.OccupeIndex;
  Occupe:=List.Occupe
end;

function TTriList.LoadLink(Link: TTriLink): Integer;
var
  i,j: Integer; p: PTriItem;
  a,b: GOrient; tr: TTri2;
begin
  Clear;

  for i:=0 to Link.Count-1 do begin
    tr:=Link.Tris[i];
    Link.Get_tri(i+1,a,b);

    Swap_TGauss(a[1],a[2]);
    Swap_TGauss(b[1],b[2]);
    iSwap(tr.t2,tr.t3);

    p:=Items[ xAdd(@a,@b) ];
    if Assigned(p) then begin
      p.Ind1:=tr.t3;
      p.Ind2:=tr.t1;
      p.Ind3:=tr.t2;
    end
  end;

  Result:=Count
end;

procedure TTriList.Assign_3x3(const AForw,ABack: Real3x3);
begin
  fforw_t:=AForw; fback_t:=ABack;
end;

function TTriList.xAdd(A,B: PGPoly): Integer;
var
  r: TTriItem; ab: AB_Matrix;
begin
  Result:=-1;

  if xTriangle_Verify(A,0.001) then
  if xTriangle_Verify(B,0.001) then begin

    Fillchar(r,SizeOf(r),0);

    r.A:=Triangle(A); r.T1:=identify_3x3;
    r.B:=Triangle(B); r.T2:=identify_3x3;

    if ab.xOpen(A[0],A[1],A[2], B[0],B[1],B[2]) then
    ab.Get_Matrix(r.T1);

    if ab.xOpen(B[0],B[1],B[2], A[0],A[1],A[2]) then
    ab.Get_Matrix(r.T2);

    with r.A do begin
      r.Dir1:=rib_Direction(x0,y0,x1,y1,x2,y2);
      r.Dir2:=rib_Direction(x1,y1,x2,y2,x0,y0);
      r.Dir3:=rib_Direction(x2,y2,x0,y0,x1,y1);
    end;

    Result:=Add(@r); OccupeIndex:=0
  end
end;

function TTriList.a_to_b(x,y: Double): TGauss;

function link_Triangle(px,py: Double): Boolean;
var
  i,old: Integer;
  dist,tmp: double; lp: PTriArray;
begin
  Result:=false;

  if Count > 0 then begin

    old:=OccupeIndex; dist:=-1;

    if old > 0 then
    Result:=In_Triangle(@Occupe, px,py, dist) <> 0;

    if not Result then begin

      old:=OccupeIndex;

      lp:=First;
      for i:=1 to Count do begin

        if i <> old then begin

          if In_Triangle(@lp[0].A, px,py, tmp) <> 0 then begin
            OccupeIndex:=i; Occupe:=lp[0].A;
            fforw_t:=lp[0].T1; Result:=true; Break
          end;

          if (OccupeIndex = 0)
          or (tmp < dist) then begin
            OccupeIndex:=i; Occupe:=lp[0].A;
            fforw_t:=lp[0].T1; dist:=tmp;
          end
        end;

        lp:=@lp[1]
      end
    end
  end
end;

begin
  if fforw_p.Order > 0 then
    Result:=xy_poly_xy(x,y,fforw_p)
  else begin
    link_Triangle(x,y);
    Result:=Transit_3x3(x,y,forw_t);
  end
end;

function TTriList.b_to_a(x,y: Double): TGauss;
begin
  if fback_p.Order > 0 then
    Result:=xy_poly_xy(x,y,fback_p)
  else
    Result:=Transit_3x3(x,y,back_t)
end;

procedure TTriList.Get_B_Triangle(I: Integer; G: PGPoly);
var
  p: PTriItem;
begin
  G[0]:=gauss_nil;
  G[1]:=G[0]; G[2]:=G[0];

  p:=Items[I];

  if Assigned(p) then with p.B do begin
    G[0]:=_Gauss(x0,y0);
    G[1]:=_Gauss(x1,y1);
    G[2]:=_Gauss(x2,y2);
  end;

  G[3]:=G[0];
end;

end.
