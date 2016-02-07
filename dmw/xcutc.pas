unit xcutc; interface

uses
  Math,Classes,
  otypes,xlist,xpoly,xclasses;

type
  TCutEdgeList = class(TCustomList)
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    function beginCut(ALine: PLLine): bool;

  protected
    function Up_item(p1,p2: Pointer): Boolean; override;

    function AllocPolybuf1: PLLine;
    function AllocPolybuf2: PLLine;

    function xAdd(const m1,m2: TLineMarker1;
                  lp: PLPoly; N: int): int;

  private
    fedges: TPolyList;
    fparts: TPolyList;
    fLine: PLLine;
    fbuf1: PLLine;
    fbuf2: PLLine;

    function GetPartCount: int;

  public
    property parts: TPolyList read fparts;
    property partCount: int read GetPartCount;
  end;

  TCutObjc = class(TCutEdgeList)
    function off_child(lp: PLLine): int;
    function endCut: int;
  end;

  TDivPolygon = class(TCutEdgeList)
    function Cut1(lp: PLLine): int;
    function Cut2(lp: PLLine; r: float): int;

  private
    function Plot(lp,lp0: PLLine): int;
    procedure Pop(eInd1,eInd2: int; frst: bool);

    procedure endDiv;
  end;

implementation

uses
  xline,xcross;

const
  PolyMax = 32000-1;

type
  PEdgeRec = ^TEdgeRec;
  TEdgeRec = record
    Ind: int; m1,m2: TLineMarker1;
  end;

constructor TCutEdgeList.Create;
begin
  inherited Create(Sizeof(TEdgeRec),16);
  fedges:=TPolyList.Create;
  fparts:=TPolyList.Create;
  fparts.MinCounter:=4
end;

destructor TCutEdgeList.Destroy;
begin
  xFreePtr(fbuf2);
  xFreePtr(fbuf1);
  fparts.Free;
  fedges.Free;
  inherited
end;

function TCutEdgeList.AllocPolybuf1: PLLine;
begin
  if fbuf1 = nil then
  fbuf1:=Alloc_LLine(PolyMax+1);
  Result:=fbuf1
end;

function TCutEdgeList.AllocPolybuf2: PLLine;
begin
  if fbuf2 = nil then
  fbuf2:=Alloc_LLine(PolyMax+1);
  Result:=fbuf2
end;

procedure TCutEdgeList.Clear;
begin
  inherited Clear;
  fedges.Clear;
  fparts.Clear;
end;

function TCutEdgeList.GetPartCount: int;
begin
  Result:=fparts.PartCount
end;

function TCutEdgeList.Up_item(p1,p2: Pointer): Boolean;
var
  d1,d2: Double;
begin
  with PEdgeRec(p1)^ do d1:=Min(m1.dist,m2.dist);
  with PEdgeRec(p2)^ do d2:=Min(m1.dist,m2.dist);
  Result:=d2 < d1
end;

function TCutEdgeList.beginCut(ALine: PLLine): bool;
begin
  Clear; fLine:=ALine;

  if Assigned(fLine) then
  if not PolyLock(fLine) then
  fLine:=nil;

  Result:=Assigned(fLine)
end;

function TCutEdgeList.xAdd(const m1,m2: TLineMarker1;
                           lp: PLPoly; N: int): int;
var
  i: int; r: TEdgeRec;
begin
  Result:=-1;

  if Abs(N) >= 1 then begin

    r.Ind:=fedges.PartCount;

    if N > 0 then begin
      r.m1:=m1; r.m2:=m2;

      for i:=0 to N do
      fedges.next_point(lp[i],nil);
    end
    else begin
      r.m1:=m2; r.m2:=m1;

      for i:=-N downto 0 do
      fedges.next_point(lp[i],nil);
    end;

    fedges.End_contour;

    if fedges.PartCount > Count then
    Result:=Add(@r)
  end
end;

function TCutObjc.off_child(lp: PLLine): int;

type
  pedge = ^tedge; tedge = record
    i1: Integer; m1: TLineMarker1;
    i2: Integer; m2: TLineMarker1;
  end;

  pedges = ^tedges;
  tedges = Array[0..15] of tedge;

function verifyEdges(list: TCustomList): Boolean;
var
  i,n: int; lp: pedges;
  d1,d2,t1,t2: Double; e: tedge;
begin
  n:=list.Count;
  if n > 1 then begin
  
    lp:=list.First; e:=lp[0];

    d1:=Min(e.m1.dist,e.m2.dist);
    d2:=Max(e.m1.dist,e.m2.dist);

    for i:=1 to n-1 do begin
      e:=lp[i];
      t1:=Min(e.m1.dist,e.m2.dist);
      t2:=Max(e.m1.dist,e.m2.dist);

      if (t1 <= d1) and (t2 >= d2) then begin
        lp[i]:=lp[0]; lp[0]:=e;
        d1:=t1; d2:=t2
      end else
      if ((t1 < d2) and (t2 > d2))
      or ((t1 < d1) and (t2 > d1)) then
      begin list.Clear; Break end
    end
  end;

  Result:=list.Count > 0
end;

function is_flat_rib(const p1,p2, q1,q2: TPoint): bool;
var
  l1,l2: double;
begin
  Result:=false;

  l1:=Min(Long_Dist(p1,p2),
          Long_Dist(q1,q2));

  l2:=Max(Long_Dist(p1,q1),
          Long_Dist(p2,q2));

  if (l1 < 1) or (l2/l1 > 1/50) then
  Result:=true
end;

var
  edges: TCustomList;
  i,di,k,_n,rc: int;
  ep: pedge; e: tedge; _lp: PLPoly;
  p: TPoint; mk: TLineMarker1;
begin
  Result:=0;

  if Assigned(fLine) then
  if PolyLock(lp) then begin

    edges:=TCustomList.Create(Sizeof(tedge),16);
    try
      k:=0;

      for i:=0 to lp.N do begin

        p:=lp.Pol[i]; rc:=0;
        if not ippl(fLine,p,1,mk) then

        with fLine^ do
        rc:=_rPolygonContainsPixel(@Pol,N, p.X,p.Y, @mk);

        if rc < 0 then begin
          edges.Clear; Break
        end else
        if rc > 0 then Inc(k)
        else begin
          if k > 0 then begin
            e.i2:=i; e.m2:=mk; rc:=-1;

            if e.m1.Ind < 0 then
            if e.m2.Ind < 0 then rc:=0;

            if rc = 0 then
              edges.Add(@e)
            else begin
              edges.Clear; Break
            end
          end;

          e.i1:=i; e.m1:=mk; k:=0
        end
      end;

      if verifyEdges(edges) then begin

        Result:=Count;

        ep:=edges.First;
        for i:=0 to edges.Count-1 do begin
          ep:=edges[i]; e:=ep^;
          _lp:=@lp.Pol[e.i1]; _n:=e.i2 - e.i1;
          if e.m1.dist > e.m2.dist then _n:=-_n;
          xAdd(e.m1,e.m2,_lp,_n)
        end;

        Result:=Count-Result
      end;
    finally
      edges.Free
    end
  end
end;

function TCutObjc.endCut: int;

function x_bound(lev: int): Boolean;
var
  ip,sp,j,k,_n: int; pp: PEdgeRec;
  _lp: PLPoly; _hp: PIntegers;
  dist,dist1: Double; p1: TEdgeRec;
begin
  Result:=false; 

  if Count > 0 then begin

    ip:=0; pp:=First; p1:=pp^;

    dist1:=p1.m1.dist-1; sp:=-1; k:=1;

    if lev > 0 then begin
      sp:=p1.m1.Ind; k:=0;
      if sp < 0 then sp:=-sp-1;
    end;

    while ip < Count do begin
      pp:=Items[ip];
      dist:=pp.m1.dist;

      if lev > 0 then
      if dist >= p1.m2.dist then Break;

      if dist < dist1 then
        Inc(ip)
      else begin
        j:=pp.m1.Ind;
        if j < 0 then j:=-j-1;

        while sp < j do begin Inc(sp);
          fparts.Continue(fLine.Pol[sp],nil)
        end;

        _n:=fedges.Seek_poly(pp.Ind,_lp,_hp)-1;

        if k > 0 then begin
          for j:=0 to _n do
          fparts.Continue(_lp[j],nil);
          sp:=pp.m2.Ind;
        end
        else begin
          for j:=_n downto 0 do
          fparts.Continue(_lp[j],nil);
          sp:=pp.m1.Ind;
        end;

        sp:=pp.m2.Ind;
        if sp < 0 then sp:=-sp-1;

        if k > 0 then
        dist1:=pp.m2.dist;

        Delete(ip); Inc(k)
      end
    end;

    j:=fLine.N;
    if lev > 0 then begin
      j:=p1.m2.Ind;
      if j < 0 then j:=-j-1
    end;

    while sp < j do begin Inc(sp);
      fparts.Continue(fLine.Pol[sp],nil)
    end;

    Result:=fparts.End_lock > 0
  end
end;

var
  lev,k: int;
begin
  if Count > 0 then
  if Count = fedges.PartCount then begin

    if Count > 1 then Sort_up(1);

    lev:=0;
    while Count > 0 do begin
      k:=Count;

      if not x_bound(lev) then
      if k = Count then Break;

      Inc(lev)
    end
  end;
       
  Result:=fparts.PartCount
end;

function TDivPolygon.Plot(lp,lp0: PLLine): int;

function lp_lp0_plot(vp: PLPoly;
                     const p: TPoint;
                     out ic: Integer;
                     out pc: TPoint): bool;
var
  j: int; q: TPoint;
  l: VLLine; r: double;
  cp: TPoint32; iv,jv: IValues32;
begin
  Result:=false;

  if Plot_to_Polygon(fLine,p,1, j,q) then begin

    r:=Long_Dist(vp[0],vp[1]) + Long_Dist(vp[0],q);
    if Get_Next_Point(vp[1],vp[0],r,vp[0]) then begin

      l.N:=1;
      l.Pol[0]:=vp[0];
      l.Pol[1]:=vp[1];

      if lp_Get_Cross(@l.Pol,l.N,
                      @fLine.Pol,fLine.N,
                      @cp,4,@iv,@jv,true) = 1 then begin
        ic:=jv[0]; pc:=cp[0]; Result:=true
      end
    end
  end
end;

var
  i,k,k1,j,i1,i2,_n: int; _lp: PLPoly;
  cp: TPoint32; iv,jv: IValues32;
  p1,p2,q,t1,t2: TPoint; v: LVector;
  m1,m2: TLineMarker1;
begin
  if lp.N > 0 then begin
    p1:=lp.Pol[0];
    p2:=lp.Pol[lp.N];

    k:=lp_Get_Cross(@lp.Pol,lp.N,
                    @fLine.Pol,fLine.N,
                    @cp,32,@iv,@jv,true);

    k1:=k;
    if (k > 0) and (iv[0] <> 0) then

    if Plot_to_Polygon(fLine,p1,1, j,q) then
      Inc(k1)
    else
    if Assigned(lp0) then
    if rPolygonContainsPixel(@fLine.Pol,fLine.N,p1.X,p1.Y) = 1 then
    begin
      v[0]:=p1; v[1]:=lp.Pol[1];
      p1:=lp0.Pol[0];

      if lp_lp0_plot(@v,p1, j,q) then begin
        lp.Pol[0]:=q; Inc(k1)
      end
    end;

    if k1 > k then begin
      k:=k1; if k > 32 then k:=32;
      for i:=k-1 downto 1 do begin
        cp[i]:=cp[i-1];
        iv[i]:=iv[i-1];
        jv[i]:=jv[i-1];
      end;

      cp[0]:=q; iv[0]:=0; jv[0]:=j
    end;

    k1:=k;
    if (k > 0) and (iv[k-1] <> lp.N) then
    if Plot_to_Polygon(fLine,p2,1, j,q) then
      Inc(k1)
    else
    if Assigned(lp0) then
    if rPolygonContainsPixel(@fLine.Pol,fLine.N,p2.X,p2.Y) = 1 then
    begin
      v[0]:=p2; v[1]:=lp.Pol[lp.N-1];
      p2:=lp0.Pol[lp0.N];

      if lp_lp0_plot(@v,p2, j,q) then begin
        lp.Pol[lp.N]:=q; Inc(k1)
      end
    end;

    if k1 > k then
    if k < 32 then begin
      cp[k]:=q; iv[k]:=lp.N; jv[k]:=j;
      Inc(k)
    end;

    i:=0;
    if k > 0 then
    if not Odd(k) then
    while i+2 <= k do begin
      p1:=cp[i]; i1:=iv[i];
      m1:=DistMarker1(@fLine.Pol,fLine.N,jv[i],p1);
      Inc(i);

      p2:=cp[i]; i2:=iv[i];
      m2:=DistMarker1(@fLine.Pol,fLine.N,jv[i],p2);
      Inc(i);

      if i1 < 0 then i1:=Abs(i1)-1;
      t1:=lp.Pol[i1]; lp.Pol[i1]:=p1;

      i2:=Abs(i2);
      t2:=lp.Pol[i2]; lp.Pol[i2]:=p2;

      _lp:=@lp.Pol[i1]; _n:=i2 - i1;
      if m1.dist > m2.dist then _n:=-_n;

      xAdd(m1,m2,_lp,_n);

      lp.Pol[i1]:=t1;
      lp.Pol[i2]:=t2;
    end
  end;

  Sort_up(1); Result:=Count
end;

// [frst] e1=nil & импорт границы от m1 до m2
procedure TDivPolygon.Pop(eInd1,eInd2: int; frst: bool);
var
  i,i1,i2,_n: int;
  _lp: PLPoly; _hp: PIntegers;
  e1,e2: PEdgeRec; _e1: TEdgeRec;
  m2: TLineMarker1;
begin
  e1:=Items[eInd1];
  e2:=Items[eInd2];

  if Assigned(e1)
  or Assigned(e2) then begin

    if Assigned(e1) then
      _e1:=e1^
    else
    with fLine^ do begin
      _e1.Ind:=-1;
      _e1.m1:=LineMarker1(0,Pol[0],0);
      _e1.m2:=LineMarker1(N,Pol[N],0);
    end;

    if Assigned(e1) or frst then begin
      fparts.Continue(_e1.m1.pc,nil);
      i1:=_e1.m1.Ind;
      if i1 >= 0 then Inc(i1)
                 else i1:=Abs(i1);

      if e2 = nil then m2:=_e1.m2
                  else m2:=e2.m1;

      i2:=Abs(m2.Ind);
      if i2 = 0 then i2:=fLine.N;

      while i1 <> i2 do begin
        fparts.Continue(fLine.Pol[i1],nil);
        Inc(i1); if i1 > fLine.N then
        Dec(i1,fLine.N)
      end;

      if e2 = nil then
        fparts.Continue(m2.pc,nil)
      else begin
        _n:=fedges.Seek_poly(e2.Ind,_lp,_hp)-1;

        for i:=0 to _n do
        fparts.Continue(_lp[i],nil);

        i2:=e2.m2.Ind;
        if i2 >= 0 then Inc(i2)
                   else i2:=Abs(i2);

        i1:=Abs(_e1.m2.Ind)-1;

        for i:=i2 to i1 do
        fparts.Continue(fLine.Pol[i],nil);
      end;

      if e1 = nil then
        fparts.Continue(_e1.m2.pc,nil)
      else begin
        _n:=fedges.Seek_poly(e1.Ind,_lp,_hp)-1;
        for i:=_n downto 0 do
        fparts.Continue(_lp[i],nil)
      end
    end
    else begin
      i1:=e2.m1.Ind;
      if i1 >= 0 then Inc(i1)
                 else i1:=Abs(i1);

      i2:=Abs(e2.m2.Ind)-1;

      for i:=i2 downto i1 do
      fparts.Continue(fLine.Pol[i],nil);

      _n:=fedges.Seek_poly(e2.Ind,_lp,_hp)-1;

      for i:=0 to _n do
      fparts.Continue(_lp[i],nil);
    end;

    fparts.End_lock
  end
end;

function TDivPolygon.Cut1(lp: PLLine): int;
var
  i: int;
begin
  Clear;

  if Assigned(fLine) then
  if Plot(lp,nil) > 0 then begin
    for i:=0 to Count-1 do Pop(i-1,i,true);
    Pop(Count-1,Count,true)
  end;

  Result:=fparts.PartCount
end;

procedure TDivPolygon.endDiv;
var
  i: int; e1,e2: PEdgeRec; frst: bool;
begin
  if Count > 0 then
  if not Odd(Count) then begin
    for i:=0 to Count-1 do
    if (i and 1) = 0 then begin
      frst:=true;
      if i = 0 then begin
        e1:=Items[0];
        e2:=Items[1];

        if e1.m1.dist < e1.m2.dist then
        if e1.m2.dist < e2.m1.dist then
        if e2.m1.dist < e2.m2.dist then
        frst:=false
      end;

      Pop(i-1,i,frst);
    end;

    Pop(Count-1,Count,true)
  end
end;

function TDivPolygon.Cut2(lp: PLLine; r: float): int;
var
  lp1,lp2: PLLine; i: int; e1,e2: PEdgeRec; frst: bool;
begin
  Clear;

  if r < 0 then begin
    if Plot(lp,nil) > 0 then endDiv;
  end
  else begin
    lp1:=AllocPolybuf1;
    lp2:=AllocPolybuf2;

    if Assigned(fLine) then

    if Assigned(lp1) then
    if Assigned(lp2) then
    if lp.N < PolyMax then

    if LPoly_Road(@lp.Pol,lp.N,nil,r,NAN,0,lp1,lp2) > 0 then
    if Plot(lp1,lp) > 0 then
    if Plot(lp2,lp) > 0 then endDiv;
  end;

  Result:=fparts.PartCount
end;

end.