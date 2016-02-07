unit xpoly1; interface

uses
  Classes,otypes,xlist;

const
  lpoly_dir_solve: bool = false;

type
  TRoadPos = record
    pc: TPoint; lp_i: int;
    dist,rib,eps,vx,vy: Double;
  end;

  PPolyMarker = ^TPolyMarker;
  TPolyMarker = record
    Ind: Integer; P: TPoint;
  end;

  PPolyMarkers = ^TPolyMarkers;
  TPolyMarkers = Array[0..255] of TPolyMarker;

  TPolyMarkerList = class(TCustomList)
    constructor Create(ACtrl: PLLine; ACapacity: Integer);
    function AddItem(Ind,X,Y: Integer): Integer;
  private
    fCtrl: PLLine;
  protected
    function Up_item(p1,p2: Pointer): Boolean; override;
  public
    property Ctrl: PLLine read fCtrl;
  end;

procedure lp_push(lp: PLLine; const p: TPoint);

function lp_get_dist(lp: PLPoly; n,lp_i: int;
                     const p: TPoint): double;

function lpoly_backup1(lp: PLPoly; n: int;
                       dist: Double; out c: TPoint;
                       out v: TGauss): int;

function lpoly_backup(lp: PLPoly; n: int;
                      dist: Double; out c: TPoint): int;

function line_backup(lp: PLLine; dist: Double;
                     out c: TPoint): Integer;

function Plot_sign(poly: TPointList; lp: PLLine;
                   const v: txyz): Integer;

function polygonWeight(wp: PDoubles;
                       lp: PLPoly; n: Integer;
                       px,py: Double): Integer;

function lroad_trunc(lp: PLLine; const m: TRoadPos): int;

function lroad_trunc_top(lp: PLLine; const p: TPoint; dR: int): bool;

function lroad_next_range(lp,buf: PLLine;
                          const m1,m2,m3: TRoadPos;
                          out a,b: TRoadPos): bool;

function lroad_cut(lp: PLLine; const a,b: TRoadPos): int;

procedure stripe_cut(lp1,lp2,buf: PLLine;
                     const a1,a2,b1,b2: TRoadPos);

procedure poly_xmov(lp: PLPoly; lp_n, i,di: int;
                    const v: TPoint; dr: double);

function lroad_move(lp: PLLine; lpMax: int;
                    const m: TRoadPos;
                    const v: TPoint;
                    dr: double): bool;

function RoadContainsPoint(lp: PLPoly; lp_N: Integer;
                           pX,pY,dR: Integer;
                           out pos: TRoadPos): Boolean;

function xRoadContainsPointi(lp: PLPoly; lp_N: int;
                             const p: TPoint; dR: int;
                             out pos: TRoadPos): Boolean;

function xRoadContainsPoint(lp: PLLine;
                            const p: TPoint; dR: Integer;
                            out pos: TRoadPos): Boolean;

function PolyContainsBound(lp: PLPoly; lp_n: Integer;
                           pg: PLPoly; pg_n: Integer): Boolean;

function PolyContainsPage(lp: PLPoly; lp_n: Integer;
                          pg: PLPoly; pg_n: Integer): Boolean;

function PickupTrk(lp: PLPoly; lp_n: int; hp: PIntegers;
                   ms: int; out v: lxyz; out d: TGauss): Boolean;

function PickupPoints(pX,pY: int;
                      lp: PLPoly; lp_n: int;

                      ip: PIntegers;
                      dp: PDoubles;
                      MaxCount: int;

                      rad: double): int;

function GetPolySkeleton(dst,src: PLPoly; N: int; tan,eps: double): int;

function GetPolyNearestPoint(lp: PLLine; const c: TPoint; out p: TPoint): bool;

function Lock_Rotate(lp,buf: PLLine; Ind: int): bool;
function lock_frst(Dest,Src: PLPoly; lp_n,Ind: int): int;

function lp_lock_frst(lp,buf: PLLine; Ind: int): bool;

function off_sharp(lp: PLPoly; n: int; minCos: double): int;

function lpoly_dist(lp,buf: PLLine;
                    lpMax: int; dist: double): bool;

function lpoly_dir(lp: PLPoly; lp_n,Ind: int;
                   dr1,dr2: double; vp: PLPoly): int;

function lpoly_dir1(lp: PLPoly; lp_n,Ind: int;
                    dr1,dr2,eps: double; vp: PLPoly): int;

function rv_poly_contains(lp: PLPoly; lp_n: int;
                          const p: TPoint; dR: int;
                          ic: pint; out c: TPoint): Double;

function rv_line_contains(lp: PLLine;
                          const p: TPoint; dR: int;
                          ic: pint; out c: TPoint): Double;

function rv_poly_online(lp: PLPoly; lp_n: int;
                        ic: int; const p: TPoint): int;

function rv_co_x(bp: PLPoly; bn: int;
                 lp: PLPoly; ln: int;
                 out c: TPoint): bool;

function rv_poly_Indexof(lp: PLPoly; lp_n: int;
                         const p: TPoint;
                         eps: Double): int;

function rv_line_Indexof(lp: PLLine;
                         const p: TPoint;
                         eps: Double): int;

function rv_get_bline(bp,lp: PLLine;
                      const e1,e2: TPoint;
                      eps: double): int;

function Is_clip_garbage(lp: PLPoly; lp_n: int): bool;

procedure squeeze_rect(lp: PLLine; dr: double);

function rv_poly_lock(lp: PLLine; lpMax: int;
                      bp: PLPoly; bn,dR: int): Boolean;

function PolyDistPoly(lp1,lp2: PLLine;
                      out dist: double;
                      ip: PIntegers): Boolean;

procedure lp_inter_z(lp: PLPoly; hp: PIntegers; N,z1,z2: int);

function lp_join_trunc(lp1,lp2: PLLine; Eps: double): bool;
function lp_lock_trunc(lp: PLLine; Eps: double): bool;

function lp_cls_ins(lp: PLPoly; n,i: int; eps: double): int;

function eps_lp_z(lp: PLPoly; hp: PIntegers;
                  n: int; eps: double): int;
                  
implementation

uses
  Math,xline,xpoly;

procedure lp_push(lp: PLLine; const p: TPoint);
begin
  with lp^ do if (N < 0)
  or not Points_Equal(Pol[N],p) then
  begin Inc(N); Pol[N]:=p end
end;

constructor TPolyMarkerList.Create(ACtrl: PLLine; ACapacity: Integer);
begin
  inherited Create(Sizeof(TPolyMarker),ACapacity);
  fCtrl:=ACtrl
end;

function TPolyMarkerList.Up_item(p1,p2: Pointer): Boolean;
begin
  Result:=PPolyMarker(p2).Ind > PPolyMarker(p1).Ind
end;

function TPolyMarkerList.AddItem(Ind,X,Y: Integer): Integer;
var
  mk: PPolyMarker; r: TPolyMarker; p: TPoint;
begin
  Result:=-1;
  if Ind >= 0 then begin

    if Assigned(fCtrl) then begin
      if Ind < fCtrl.N then begin
        p:=fCtrl.Pol[Ind+1];
        if (p.x = X) and (p.Y = Y) then Inc(Ind)
      end;

      if Count > 0 then begin
        mk:=Last; if Assigned(mk) then

        if Ind > mk.Ind then else
        if Ind < mk.Ind then Ind:=-1 else
        if (X = mk.P.X) and (Y = mk.P.Y) then Ind:=-1
      end
    end;

    if Ind >= 0 then begin
      r.Ind:=Ind; r.P.X:=X; r.P.Y:=Y;
      Result:=Add(@r)
    end
  end
end;

function lp_get_dist(lp: PLPoly; n,lp_i: int;
                     const p: TPoint): double;
var
  i,i2: int; p1,p2: TPoint; dist: double;
begin
  dist:=0;

  i2:=Abs(lp_i);
  if lp_i < 0 then Dec(i2);

  if i2 > n then i2:=n;
  if n > 0 then begin

    p2:=lp[0];
    for i:=0 to i2 do begin
      p1:=p2; p2:=lp[i];
      dist:=dist+Long_dist(p1,p2)
    end;

    if lp_i < 0 then
    dist:=dist+Long_dist(p2,p)
  end;

  Result:=dist
end;

function lpoly_backup1(lp: PLPoly; n: int;
                       dist: Double; out c: TPoint;
                       out v: TGauss): int;
var
  i,dx,dy: int; a,b,p,pv: TPoint; k,rib: Double;
begin
  Result:=0; p:=lp[0]; pv:=Point(0,0);

  b:=lp[0];
  for i:=1 to n do begin
    a:=b; b:=lp[i];
    dx:=b.X-a.X; dy:=b.Y-a.Y;

    if (dx <> 0) or (dy <> 0) then begin

      rib:=Hypot(dx,dy);

      if (dist <= rib) or (i = n) then begin
        k:=dist / rib;
        p.X:=a.X + Round(dx*k);
        p.Y:=a.Y + Round(dy*k);

        pv.X:=dx; pv.Y:=dy; Result:=i;

        if Points_Equal(p,a) then begin
          if i > 1 then pv:=point_dir(lp[i-2],b);
          Result:=i-1
        end else
        if Points_Equal(p,b) then begin
          if i < n then pv:=point_dir(a,lp[i+1])
        end else
        if dist > rib then Result:=i+1;

        Break
      end;

      dist:=dist - rib
    end
  end;

  c:=p; v.x:=pv.X; v.y:=pv.Y; xNorm(v)
end;

function lpoly_backup(lp: PLPoly; n: int;
                      dist: Double; out c: TPoint): int;
var
  i,dx,dy: int; p1,p2: TPoint; k,rib: Double;
begin
  Result:=1; c:=lp[0];

  p2:=lp[0];
  for i:=1 to n do begin
    p1:=p2; p2:=lp[i]; Result:=i;
    dx:=p2.X-p1.X; dy:=p2.Y-p1.Y;

    if (dx <> 0) or (dy <> 0) then begin

      c:=p2; rib:=Hypot(dx,dy);
      if (dist < rib) or (i = n) then begin
        k:=dist / rib;
        c.X:=p1.X + Round(dx*k);
        c.Y:=p1.Y + Round(dy*k);
        Break
      end;

      dist:=dist - rib
    end
  end
end;

function line_backup(lp: PLLine; dist: Double;
                     out c: TPoint): Integer;
begin
  Result:=lpoly_backup(@lp.Pol,lp.N,dist,c)
end;

function Plot_sign(poly: TPointList; lp: PLLine;
                   const v: txyz): Integer;
var
  i,dx,dy: Integer;
  dr,rib,k,nx,ny: Double;
  p1,p2,p: TPoint;
begin
  poly.Clear;

  if lp.N > 0 then begin

    p2:=lp.Pol[0]; dr:=v.x;

    for i:=1 to lp.N do begin
      p1:=p2; p2:=lp.Pol[i];
      dx:=p2.X - p1.X; dy:=p2.Y - p1.Y;
      rib:=Hypot(dx,dy);

      if rib >= 0.001 then begin

        nx:=+dy/rib*v.y;
        ny:=-dx/rib*v.y;

        while dr <= rib do begin

          k:=dr / rib;
          p.X:=p1.X + Round(k*dx+nx);
          p.Y:=p1.Y + Round(k*dy+ny);
          poly.Add(@p);

          k:=10 / rib;
          p.X:=p.X + Round(k*dx);
          p.Y:=p.Y + Round(k*dy);
          poly.Add(@p);

          if v.z < 1 then begin
            dr:=-1; Break
          end;

          dr:=dr + v.z
        end;

        if dr < 0 then Break;
      end;

      dr:=dr - rib
    end
  end;

  Result:=poly.Count
end;

function polygonWeight(wp: PDoubles;
                       lp: PLPoly; n: Integer;
                       px,py: Double): Integer;
var
  i: Integer; r: Double; cx: Extended;
begin
  cx:=0;
  for i:=0 to n-1 do begin
    r:=Hypot(px-lp[0].X,py-lp[0].Y);
    wp[i]:=r; cx:=cx+r; lp:=@lp[1]
  end;

  for i:=0 to n-1 do wp[i]:=wp[i] / cx;
  Result:=n
end;

function lroad_cut(lp: PLLine; const a,b: TRoadPos): int;
var
  i: int; _a,_b: TRoadPos; p: TPoint; swap: bool;
begin
  Result:=lp.N;

  _a:=a; _b:=b; swap:=false;
  if _a.dist > _b.dist then begin
    _a:=b; _b:=a; swap:=true;
  end;

  with lp^ do begin
    i:=Abs(_b.lp_i); p:=_b.pc;
    if (i >= 0) and (i <= N) then begin
      if Points_Equal(p,Pol[i]) then Dec(i);
      N:=i; if i >= 0 then Pol[i]:=p
    end;

    i:=Abs(_a.lp_i); p:=_a.pc;
    if (i > 0) and (i <= N) then begin
      if not Points_Equal(p,Pol[i]) then Dec(i);
      Poly_cut(lp,0,i); Pol[0]:=p
    end;
  end;

  if swap then Swap_Poly(lp);
  Result:=lp.N;
end;

function lroad_trunc(lp: PLLine; const m: TRoadPos): int;
var
  len: double; v: lxyz;
begin
  Result:=-1;
  if not PolyLock(lp) then begin

    v.p:=m.pc;

    len:=PolyLength(@lp.Pol,lp.N);
    if m.dist < len/2 then
      lp_trunc_top(lp,nil,Abs(m.lp_i),v.v)
    else
      lp_trunc_bot(lp,nil,Abs(m.lp_i),v.v);

    Result:=lp.N
  end
end;

function lroad_trunc_top(lp: PLLine; const p: TPoint; dR: int): bool;
var
  m: TRoadPos; v: lxyz;
begin
  Result:=false;
  if xRoadContainsPoint(lp,p,dr,m) then begin
    v.p:=m.pc; v.v.z:=0;
    Result:=lp_trunc_top(lp,nil,Abs(m.lp_i),v.v)
  end
end;

function lroad_next_range(lp,buf: PLLine;
                          const m1,m2,m3: TRoadPos;
                          out a,b: TRoadPos): bool;

function mk_dist(d1,d2,len: double): double;
begin
  Result:=Abs(d2-d1); if d2 > d1 then
  Result:=Min(Result,d1+len-d2);
end;

var
  d1,d2,len: double;
begin
  Result:=false;
  if PolyLock(lp) then begin
    len:=PolyLength(@lp.Pol,lp.N);

    d1:=mk_dist(m1.dist,m3.dist,len);
    d2:=mk_dist(m2.dist,m3.dist,len);

    if Min(d1,d2) > 1 then
    if d1 < d2 then begin


    end
    else begin

    end
  end else

  if m1.dist < m2.dist then begin

    if m3.dist < m1.dist then begin
      Result:=lroad_cut(lp,m3,m1) > 0;
      a:=m3; b:=m1;
    end else
    if m3.dist > m2.dist then begin
      Result:=lroad_cut(lp,m2,m3) > 0;
      a:=m2; b:=m3;
    end

  end else

  if m1.dist > m2.dist then begin

    if m3.dist < m2.dist then begin
      Result:=lroad_cut(lp,m2,m3) > 0;
      a:=m2; b:=m3;
    end else
    if m3.dist > m1.dist then begin
      Result:=lroad_cut(lp,m3,m1) > 0;
      a:=m3; b:=m1;
    end

  end
end;

procedure stripe_cut(lp1,lp2,buf: PLLine;
                     const a1,a2,b1,b2: TRoadPos);

const
  _dir: Array[Boolean] of int = (-1,1);

function backupc(lp: PLLine; const a,b: TRoadPos;
                 out c: TPoint): double;
begin
  Result:=(a.dist+b.dist)/2;
  line_backup(lp,Result,c);
end;

procedure backup2(lp: PLLine; const a,b: TRoadPos;
                  out d1,d2: double; out c1,c2: TPoint);
var
  len: double;
begin
  len:=PolyLength(@lp.Pol,lp.N);
  d1:=(a.dist+b.dist)/2; d2:=d1+len/2;
  if d2 > len then d2:=d2-len;
  line_backup(lp,d1,c1);
  line_backup(lp,d2,c2);
end;

procedure lp_cut(lp,buf: PLLine; const a1,a2: TRoadPos; c: double);
var
  dir,i1,i2: int;
begin
  dir:=0;
  if a1.dist < a2.dist then begin
    if (c > a1.dist) and (c < a2.dist) then dir:=1 else
    if (c < a1.dist) or (c > a2.dist) then dir:=-1
  end else
  if a1.dist > a2.dist then begin
    if (c > a2.dist) and (c < a1.dist) then dir:=-1 else
    if (c < a2.dist) or (c > a1.dist) then dir:=+1
  end;

  Load_Poly(buf,@lp.Pol,lp.N+1); lp.N:=-1;

  if dir <> 0 then begin
    lp.N:=0; lp.Pol[0]:=a1.pc;
    i1:=Abs(a1.lp_i); i2:=Abs(a2.lp_i);

    if PolyLock(buf) then begin
      if i2 = buf.N then i2:=0;
      if i1 = buf.N then i1:=0;
    end;

    if i1 <= buf.N then
    if i2 <= buf.N then

    if dir > 0 then begin
      while i1 <> i2 do begin
        lp_push(lp,buf.Pol[i1]);

        Inc(i1);
        if i1 > buf.N then begin
          if i2 = 0 then Break; i1:=1;
        end
      end
    end
    else begin
      while i1 <> i2 do begin
        Dec(i1); if i1 < 0 then i1:=buf.N-1;
        lp_push(lp,buf.Pol[i1])
      end
    end;

    lp_push(lp,a2.pc)
  end
end;

procedure x_lock_lock(lp1,lp2,buf: PLLine;
                      const a1,a2,b1,b2: TRoadPos);

function x_dist(const a,b: TRoadPos; c,len: double): double;
var
  d1,d2,t: double;
begin
  d1:=a.dist; d2:=b.dist; t:=Abs(d2-d1);
  if (c >= Min(d1,d2)) and
     (c <= Max(d1,d2)) then
    Result:=t
  else
    Result:=len-t
end;

var
  l,t,l1,l2,d1,d2: double;
  i,j, i1,j1,i2,j2: int;
  _d1,_d2: Array[0..1] of double;
  _c1,_c2: LVector;
begin
  backup2(lp1, a1,a2, _d1[0],_d1[1], _c1[0],_c1[1]);
  backup2(lp2, b1,b2, _d2[0],_d2[1], _c2[0],_c2[1]);

  l:=-1;

  for i:=0 to 1 do
  for j:=0 to 1 do begin
    t:=Long_Dist(_c1[i],_c2[j]);

    if (l < 0) or (t < l) then begin
      i1:=i; j1:=j; l:=t
    end
  end;

  i2:=(i1+1) and 1;
  j2:=(j1+1) and 1;

  l1:=PolyLength(@lp1.Pol,lp1.N);
  l2:=PolyLength(@lp2.Pol,lp2.N);

  d1:=x_dist(a1,a2, _d1[i1],l1)+
      x_dist(b1,b2, _d2[i1],l2);

  d2:=x_dist(a1,a2, _d1[i2],l1)+
      x_dist(b1,b2, _d2[i2],l2);

  if d2 < d1 then begin
    i1:=i2; j1:=j2
  end;

  lp_cut(lp1,buf, a1,a2,_d1[i1]);
  lp_cut(lp2,buf, b1,b2,_d2[j1]);
end;

procedure x_lock_unlock(lp1,lp2: PLLine; const a1,a2,b1,b2: TRoadPos);
var
  d1,d2: double;
  _d1: Array[0..1] of double;
  _c1: LVector; c2: TPoint;
begin
  backup2(lp1, a1,a2, _d1[0],_d1[1], _c1[0],_c1[1]);
  d2:=backupc(lp2, b1,b2, c2);

  d1:=_d1[0];
  if Long_Dist(_c1[1],c2) <
     Long_Dist(_c1[0],c2) then
  d1:=_d1[1];

  lp_cut(lp1,buf, a1,a2,d1);
  lp_cut(lp2,buf, b1,b2,d2);
end;

procedure x_unlock_unlock(lp1,lp2: PLLine; const a1,a2,b1,b2: TRoadPos);
begin
  lp_cut(lp1,buf, a1,a2,(a1.dist+a2.dist)/2);
  lp_cut(lp2,buf, b1,b2,(b1.dist+b2.dist)/2);
end;

begin
  if PolyLock(lp1) then

    if PolyLock(lp2) then
      x_lock_lock(lp1,lp2,buf, a1,a2,b1,b2)
    else
      x_lock_unlock(lp1,lp2, a1,a2,b1,b2)

  else

    if PolyLock(lp2) then
      x_lock_unlock(lp2,lp1, b1,b2,a1,a2)
    else
      x_unlock_unlock(lp1,lp2, a1,a2,b1,b2)
end;

procedure poly_xmov(lp: PLPoly; lp_n, i,di: int;
                    const v: TPoint; dr: double);
var
  p1,p2: TPoint; vx,vy,len,k: double;
begin
  if i >= 0 then
  if i <= lp_n then begin

    p2:=lp[i];

    vx:=v.X; vy:=v.Y; len:=dr;
    while true do begin
      Inc(i,di);
      if i < 0 then Break;
      if i > lp_n then Break;

      p1:=p2; p2:=lp[i];
      len:=len - Long_dist(p1,p2);
      if len < 0 then Break;

      k:=len/dr;
      with lp[i] do begin
        Inc(X,Round(vx*k));
        Inc(Y,Round(vy*k));
      end
    end
  end
end;

function lroad_move(lp: PLLine; lpMax: int;
                    const m: TRoadPos; const v: TPoint;
                    dr: double): bool;
var
  ic: int; pc: TPoint;
begin
  Result:=false;

  ic:=m.lp_i; pc:=m.pc;

  if ic >= 0 then
  if ic <= lp.N then begin

    if ic > 0 then
    if Points_Equal(pc,lp.Pol[ic-1]) then
    Dec(ic);

    if ic > 0 then
    if not Points_Equal(pc,lp.Pol[ic]) then
    Poly_Insert(lp,ic-1,lpMax,pc);

    with lp.Pol[ic] do begin
      Inc(X,v.X); Inc(Y,v.Y)
    end;

    poly_xmov(@lp.Pol,lp.N,ic,-1,v,dr);
    poly_xmov(@lp.Pol,lp.N,ic,+1,v,dr);

    Result:=true
  end
end;

function RoadContainsPoint(lp: PLPoly; lp_N: Integer;
                           pX,pY,dR: Integer;
                           out pos: TRoadPos): Boolean;
var
  i,rc: Integer;
  p,p1,p2,lt,rb,a,b,c: TPoint; g: TGauss;
  dx,dy,eps,tmp,dist,rib,len,_dr: Double;
begin
  Result:=false;

  if lp_N > 0 then begin

    p.X:=pX; p.Y:=pY;
    lt.X:=pX-dR; lt.Y:=pY-dR;
    rb.X:=pX+dR; rb.Y:=pY+dR;

    p2:=lp[0]; dist:=0; _dr:=dR;

    tmp:=Hypot(p2.X-p.X,p2.Y-p.Y);
    if tmp < _dr then begin
      pos.pc:=p2; pos.lp_i:=0;
      pos.dist:=0; pos.rib:=0;
      pos.eps:=tmp;

      dx:=lp[1].X-p2.X;
      dy:=lp[1].Y-p2.Y;

      if (dx = 0) and (dy = 0) then begin
        pos.vx:=1; pos.vy:=0;
      end
      else begin
        rib:=Hypot(dx,dy);
        pos.vx:=dx/rib; pos.vy:=dy/rib;
      end
    end;

    for i:=1 to lp_N do begin
      lp:=@lp[1]; p1:=p2; p2:=lp[0];

      a:=p1; b:=p2;
      if a.X > b.X then begin
        a.X:=p2.X; b.X:=p1.X;
      end;
      if a.Y > b.Y then begin
        a.Y:=p2.Y; b.Y:=p1.Y;
      end;

      dx:=p2.X-p1.X; dy:=p2.Y-p1.Y;
      rib:=Hypot(dx,dy);

      if (dx <> 0) or (dy <> 0) then

      if (a.X < rb.X) and (b.X > lt.X) then
      if (a.Y < rb.Y) and (b.Y > lt.Y) then

      if prj_to_Line(p ,p1,p2, g) then begin

        c.X:=Round(g.x); c.Y:=Round(g.y); rc:=-1;

        if (c.X < a.X) or (c.X > b.X)
        or (c.Y < a.Y) or (c.Y > b.Y) then

        if Long_dist(c,p1) < Long_Dist(c,p2) then
          begin c:=p1; rc:=i-1 end
        else
          begin c:=p2; rc:=i end;

        tmp:=Hypot(c.X-p.X,c.Y-p.Y);

        if tmp < _dr then
        if not Result or (tmp < eps) then begin

          if rc >= 0 then

            pos.lp_i:=rc

          else begin
            pos.lp_i:=i-1;
            if Int64(c) <> Int64(p1) then begin
              pos.lp_i:=i;
              if Int64(c) <> Int64(p2) then
              pos.lp_i:=-i;
            end
          end;

          len:=Hypot(c.X-p1.X,c.Y-p1.Y);
          pos.pc:=c; pos.dist:=dist + len;
          pos.vx:=dx/rib; pos.vy:=dy/rib;
          pos.rib:=len; pos.eps:=tmp;
          eps:=tmp; Result:=true;
        end
      end;

      dist:=dist + rib
    end
  end
end;

function xRoadContainsPointi(lp: PLPoly; lp_N: int;
                             const p: TPoint; dR: int;
                             out pos: TRoadPos): Boolean;
var
  a,b,c,d: double; mk: TRoadPos;
begin
  Result:=RoadContainsPoint(lp,lp_N, p.X,p.Y,dR, pos);
  if Result then begin
    mk:=pos; a:=mk.vy; b:=-mk.vx;
    with mk.pc do c:=Y*mk.vx - X*mk.vy;
    d:=a*p.X + b*p.Y + c;
    if d < 0 then pos.eps:=-mk.eps
  end
end;

function xRoadContainsPoint(lp: PLLine;
                            const p: TPoint; dR: Integer;
                            out pos: TRoadPos): Boolean;
begin
  Result:=RoadContainsPoint(@lp.Pol,lp.N,p.X,p.Y,dR,pos)
end;

function PolyContainsBound(lp: PLPoly; lp_n: Integer;
                           pg: PLPoly; pg_n: Integer): Boolean;
var
  i: Integer;
begin
  Result:=false;

  for i:=0 to pg_n do with pg[i] do
  if rPolygonContainsPixel(lp,lp_n, X,Y) > 0 then
  begin Result:=true; Break end
end;

function PolyContainsPage(lp: PLPoly; lp_n: Integer;
                          pg: PLPoly; pg_n: Integer): Boolean;
begin
  Result:=PolyContainsBound(lp,lp_n,pg,pg_n) or
          PolyContainsBound(pg,pg_n,lp,lp_n)
end;

function PickupTrk(lp: PLPoly; lp_n: int; hp: PIntegers;
                   ms: int; out v: lxyz; out d: TGauss): Boolean;
var
  i1,i2,ii,it, t1,dt: int; p1,p2: TPoint; kt: Double;
begin
  Result:=false;

  if lp_n > 0 then
  if ms >= hp[0] then begin

    i1:=0; i2:=lp_n;
    while i1 <= i2 do begin
      ii:=(i1+i2) div 2; it:=hp[ii];

      if ms = it then begin
        v.p:=lp[ii]; v.v.z:=it;
        Result:=true; Break
      end else
      if ms > it then begin
        i1:=ii+1;
        if ms <= hp[i1] then begin
          Dec(i1); Break
        end
      end
      else begin i2:=ii-1;
        if ms >= hp[i2] then begin
          i1:=i2; Break
        end
      end
    end;

    if not Result then
    if i1 <= i2 then
    if i1 < lp_n then begin

      p1:=lp[i1]; p2:=lp[i1+1];

      t1:=hp[i1];
      dt:=hp[i1+1] - t1;
      if dt > 0 then begin
        kt:=(ms-t1)/dt;
        p1.X:=p1.X + Round((p2.X-p1.X)*kt);
        p1.Y:=p1.Y + Round((p2.Y-p1.Y)*kt);
      end;

      v.p:=p1; v.v.z:=ms;

      d.x:=p2.X-p1.X;
      d.y:=p2.Y-p1.Y;
      xNorm(d);
      
      Result:=true;
    end
  end
end;

function PickupPoints(pX,pY: int;
                      lp: PLPoly; lp_n: int;
                      ip: PIntegers;
                      dp: PDoubles;
                      MaxCount: int;

                      rad: double): int;
var
  i,j,j1,k: int; t: double;
begin
  k:=0;

  for i:=0 to lp_n do begin
    with lp[i] do t:=Hypot(X-pX,Y-pY);

    if (rad < 0) or (t <= rad) then begin

      if k = 0 then begin
        ip[k]:=i; dp[k]:=t; Inc(k)
      end else
      if t > dp[k-1] then begin
        if k < MaxCount then begin
          ip[k]:=i; dp[k]:=t; Inc(k)
        end
      end
      else begin
        j:=0;
        while j < k do begin
          if t < dp[j] then Break; Inc(j)
        end;

        if j < k then begin
          if k < MaxCount then Inc(k);

          for j1:=k-1 downto j do begin
            ip[j1]:=ip[j1-1];
            dp[j1]:=dp[j1-1];
          end;

          ip[j]:=i; dp[j]:=t
        end else

        if k < MaxCount then begin
          ip[k]:=i; dp[k]:=t; Inc(k)
        end
      end
    end
  end;

  Result:=k
end;

// фильтрация или по [eps] или по [tan]
// обязательно что-нибудь нужно задать
function GetPolySkeleton(dst,src: PLPoly; N: int; tan,eps: double): int;

function rib(dst: PLPoly; dst_n: int;
             src: PLPoly; src_n: int;
             tan,eps: double): int;
var
  i,j,o: int; lc: TLine_coeff;
  a,b: TPoint; r,t: double;
begin
  o:=dst_n;
  if src_n > 0 then begin

    a:=src[0]; b:=src[src_n];

    if src_n > 1 then
    if Get_line_coeffp(a,b, lc) > 1 then begin

      r:=0; j:=0;
      for i:=1 to src_n-1 do begin
        with src[i] do t:=Abs(c_dist_to_line(lc, X,Y));
        if t > r then begin r:=t; j:=i end
      end;

      if ((eps > 0) and (r > eps))
      or ((tan > 0) and (r/lc.len > tan)) then begin
        o:=rib(dst,o, src,j, tan,eps);
        Inc(o); dst[o]:=src[j];
        o:=rib(dst,o, @src[j],src_n-j, tan,eps);
      end
    end
  end;

  Result:=o
end;

var
  i,j,o: int; p: TPoint; r,t: double;
begin
  o:=-1; tan:=tan/2;

  if N > 0 then begin
    o:=0; dst[0]:=src[0];

    p:=src[0];
    if not Points_Equal(p,src[N]) then
      o:=rib(dst,o, src,N, tan,eps)
    else
    if N <= 1 then
      o:=-1
    else begin
      r:=Long_dist(p,src[1]); j:=1;

      for i:=2 to N-1 do begin
        t:=Long_dist(p,src[i]);
        if t > r then begin
          r:=t; j:=i
        end
      end;

      o:=rib(dst,o, src,j, tan,eps);
      Inc(o); dst[o]:=src[j];
      o:=rib(dst,o, @src[j],N-j, tan,eps);
    end;

    Inc(o); dst[o]:=src[N]
  end;

  Result:=o
end;

function GetPolyNearestPoint(lp: PLLine; const c: TPoint; out p: TPoint): bool;
var
  i: int; q: TPoint; r,t: double;
begin
  Result:=false;
  if lp.N >= 0 then begin
    p:=lp.Pol[0]; r:=Long_dist(c,p);

    for i:=1 to lp.N do begin
      q:=lp.Pol[i]; t:=Long_dist(c,q);
      if t < r then begin
        p:=q; r:=t
      end
    end;

    Result:=true
  end
end;

function Lock_Rotate(lp,buf: PLLine; Ind: int): bool;
var
  i,n,cx: int;
begin
  Result:=false; n:=lp.N;

  if (Ind > 0) and (Ind < n) then
  if Points_Equal(lp.Pol[0],lp.Pol[n]) then

  if Assigned(buf) then begin

    for i:=0 to n do buf.Pol[i]:=lp.Pol[i];

    for i:=0 to n-1 do begin
      lp.Pol[i]:=buf.Pol[Ind]; Inc(Ind);
      if Ind = n then Ind:=0
    end;

    lp.Pol[n]:=lp.Pol[0];
    Result:=true
  end
  else begin
    cx:=n * Sizeof(TPoint);
    while Ind > 0 do begin
      Move(lp.Pol[1],lp.Pol[0],cx);
      lp.Pol[n]:=lp.Pol[0]; Dec(Ind)
    end; Result:=true
  end
end;

function lock_frst(Dest,Src: PLPoly; lp_n,Ind: int): int;
var
  i,j,o: int;
begin
  if (Ind <= 0)
  or (Ind >= lp_n) then begin
    for i:=0 to lp_n do Dest[i]:=Src[i]
  end
  else begin
    o:=-1; j:=Ind;
    for i:=1 to lp_n do begin
      Inc(o); Dest[o]:=Src[j];
      Inc(j); if j > lp_n then j:=1
    end
  end;

  Dest[lp_n]:=Dest[0];
  Result:=lp_n
end;

function lp_lock_frst(lp,buf: PLLine; Ind: int): bool;
var
  i: int;
begin
  if Ind > 0 then
  if Ind < lp.N then begin
    buf.N:=lock_frst(@buf.Pol,@lp.Pol,lp.N,Ind);

    lp.N:=buf.N;
    for i:=0 to lp.N do
    lp.Pol[i]:=buf.Pol[i]
  end;

  Result:=PolyLock(lp)
end;

function off_sharp(lp: PLPoly; n: int; minCos: double): int;
var
  i,o: int; a,b: TPoint;
  dx1,dy1,dx2,dy2,l1,l2,cos: double;
begin
  Result:=n;

  if n >= 2 then begin
    o:=0; i:=1;

    a:=lp[0]; b:=lp[1];
    dx2:=b.X-a.X; dy2:=b.Y-a.Y;
    l2:=Hypot(dx2,dy2);

    while i+1 <= n do begin
      a:=b; b:=lp[i+1];
      dx1:=dx2; dy1:=dy2; l1:=l2;

      dx2:=b.X-a.X; dy2:=b.Y-a.Y;
      l2:=Hypot(dx2,dy2);

      cos:=1; if (l1 > 1) and (l2 > 1) then
      cos:=d2_scalar(dx1,dx2,dy1,dy2)/l1/l2;

      if cos > minCos then begin
        Inc(o); lp[o]:=a
      end;

      Inc(i)
    end;

    while i <= n do begin
      Inc(o); lp[o]:=lp[i]; Inc(i)
    end;

    Result:=o
  end
end;

function lpoly_dist(lp,buf: PLLine;
                    lpMax: int; dist: double): bool;

function buf_next(buf: PLLine; lpMax: int;
                  const p: TPoint): bool;
begin
  Result:=false;
  with buf^ do
  if N < lpMax then begin
    Inc(N); Pol[N]:=p; Result:=true
  end
end;

var
  i,j,k,dx,dy: int; a,b,p: TPoint; r,tt,d1: double;
begin
  Result:=false; d1:=dist*1.5;

  if lp.N > 0 then begin

    b:=lp.Pol[0];
    buf.N:=0; buf.Pol[0]:=b;

    for i:=1 to lp.N do begin
      a:=b; b:=lp.Pol[i];
      dx:=b.X-a.X; dy:=b.Y-a.Y;
      r:=Hypot(dx,dy);
      if r > 1 then
      if r > d1 then begin
        k:=Round(r/dist);
        if k > 1 then
        for j:=1 to k-1 do begin
          p.X:=a.X + Round(dx*j/k);
          p.Y:=a.Y + Round(dy*j/k);

          if not buf_next(buf,lpMax,p) then begin
            Result:=false; Break
          end;

          Result:=true
        end
      end;

      if not buf_next(buf,lpMax,b) then begin
        Result:=false; Break
      end
    end;

    if Result then
    Load_Poly(lp,@buf.Pol,buf.N+1)
  end
end;

type
  tdir = object
    o,q,v: TGauss; len: double;
    sx,sy: double; cx: int;

    procedure init(const p: TPoint);
    procedure solve(x,y: double);

    function next(const p: TPoint; maxLen: double): bool;
    function GetVec(vp: PLPoly): double;
  end;

procedure tdir.init(const p: TPoint);
begin
  o.x:=p.X; o.y:=p.Y; q:=o;
  v.x:=0; v.y:=0; len:=0;
  sx:=0; sy:=0; cx:=0;
  solve(o.x,o.y)
end;

procedure tdir.solve(x,y: double);
begin
  sx:=sx + x; sy:=sy + y;
  Inc(cx)
end;

function tdir.next(const p: TPoint; maxLen: double): bool;
var
  d,t: TGauss; k1,k2,r,r1: double;
begin
  t.x:=p.X; t.y:=p.Y;

  d.x:=t.x-q.x;
  d.y:=t.y-q.y;
  r:=Hypot(d.x,d.y);

  if r > 0.1 then begin

    r1:=len+r;
    if r1 > maxLen then begin
      r:=(maxLen-r1)/r;
      t.x:=q.x+d.x*r;
      t.y:=q.y+d.y*r;
      r:=Hypot(d.x,d.y);
    end;

    solve(t.x,t.y);

    if r > 0.1 then begin
      d.x:=d.x/r;
      d.y:=d.y/r;

      if len < 0.1 then begin
        len:=0; v:=d
      end
      else begin
        r1:=len+r;
        k1:=len/r1; k2:=r/r1;

        v.x:=v.x*k1 + d.x*k2;
        v.y:=v.y*k1 + d.y*k2;
      end
    end;

    q:=t; len:=len+r;
  end;

  Result:=len >= maxLen
end;

function tdir.GetVec(vp: PLPoly): double;
var
  d,k,b,l,l2: double; t,t1,t2: TGauss;
begin
  t.x:=o.x + v.x*len;
  t.y:=o.y + v.y*len;

  vp[0].X:=Round(o.x);
  vp[0].Y:=Round(o.y);

  vp[1].X:=Round(t.x);
  vp[1].Y:=Round(t.y);

  if lpoly_dir_solve then
  if cx > 2 then begin

    sx:=sx/cx; sy:=sy/cx;

    l:=Gauss_dist(q,o); l2:=l/2;

    t1.x:=sx-v.x*l2; t2.x:=t1.x+v.x*l;
    t1.y:=sy-v.y*l2; t2.y:=t1.y+v.y*l;

    vp[0].X:=Round(t1.x);
    vp[0].Y:=Round(t1.y);

    vp[1].X:=Round(t2.x);
    vp[1].Y:=Round(t2.y)
  end;

  Result:=len
end;

function lpoly_dir(lp: PLPoly; lp_n,Ind: int;
                   dr1,dr2: double; vp: PLPoly): int;
var
  i: int; dir: tdir; dr: double;
begin
  Result:=-1; if lp_n > 0 then
  if (Ind = 0) or (Ind = lp_n) then begin

    i:=Ind; dir.init(lp[i]);

    if Ind = 0 then
      while i < lp_n do begin Inc(i);
        if dir.next(lp[i],dr1) then Break
      end
    else
      while i > 0 do begin Dec(i);
        if dir.next(lp[i],dr1) then Break
      end;

    dr:=dir.GetVec(vp); if (dr >= dr1)
    or ((dr2 > 0) and (dr >= dr2)) then
    Result:=i
  end
end;

function lpoly_dir1(lp: PLPoly; lp_n,Ind: int;
                    dr1,dr2,eps: double; vp: PLPoly): int;
var
  i,i1,i2: int; lc: TLine_coeff; l,r,t: double;
begin
  Result:=lpoly_dir(lp, lp_n,Ind, dr1,dr2, vp);

  if Result >= 0 then begin
    i1:=Ind; i2:=Result;
    if i1 > i2 then iSwap(i1,i2);

    if Get_line_coeffp(vp[0],vp[1],lc) < 1 then
      Result:=-1
    else begin
      r:=0; for i:=i1+1 to i2-1 do begin
        with lp[i] do t:=c_dist_to_line(lc,X,Y);
        if t > r then r:=t
      end;

      if r > 1 then
      if r/lc.len > eps then
        Result:=-1
      else begin
        l:=PolyLength(@lp[i1],i2-i1);
        if l+0.1 < lc.len then
          Result:=-1
        else
        if lc.len/l < 0.5 then
          Result:=-1
      end;

      if Result < 0 then
      Result:=lpoly_dir(lp, lp_n,Ind, dr2,dr2, vp)
    end
  end
end;

function rv_poly_contains(lp: PLPoly; lp_n: int;
                          const p: TPoint; dR: int;
                          ic: pint; out c: TPoint): Double;
var
  i,j,px,py,rc: int;
  p1,p2,lt,rb,pc: TPoint;
  dist,_dx,_dr,eps: Double;
begin
  Result:=-1; c:=p;

  _dr:=dR; _dx:=_dr/10; 

  px:=p.X; py:=p.Y; dist:=0;

  p2:=lp[0];
  for i:=1 to lp_n do begin
    p1:=p2; p2:=lp[i];
    lt:=p1; rb:=p2;

    if lt.X > rb.X then
    begin lt.X:=p2.X; rb.X:=p1.X end;

    if lt.Y > rb.Y then
    begin lt.Y:=p2.Y; rb.Y:=p1.Y end;

    if (px >= lt.X-dR) and (px <= rb.X+dR) then
    if (py >= lt.Y-dR) and (py <= rb.Y+dR) then begin

      rc:=Plot_to_Line1(p, p1,p2, pc);

      eps:=Long_dist(p,pc);
      if eps < _dr then begin

        if (rc = -1) and (i = 1) and
           (Long_dist(pc,p1) <= _dx) then begin
          pc:=p1; rc:=0
        end else
        if (rc = +1) and (i = lp_n) and
           (Long_dist(pc,p2) <= _dx) then begin
          pc:=p2; rc:=0
        end else

        if (rc = 0) and (Long_dist(pc,p1) < dR) then
          pc:=p1

        else
        if (rc = 0) and (Long_dist(pc,p2) < dR) then
          pc:=p2
        else
        if (rc = +1) and (i < lp_n) then begin
          if eps < 4 then
          if Plot_to_Line1(p ,p2,lp[i+1], pc) = -1 then
          begin pc:=p2; rc:=0 end
        end;

        if rc = 0 then begin
          j:=-i;
          if Points_Equal(pc,p1) then j:=i-1 else
          if Points_Equal(pc,p2) then j:=i;

          Result:=dist + Long_dist(p1,pc);
          if Assigned(ic) then ic^:=j;
          c:=pc; Break
        end
      end
    end;

    dist:=dist + Long_Dist(p1,p2)
  end
end;
                 
function rv_line_contains(lp: PLLine;
                          const p: TPoint; dR: int;
                          ic: pint; out c: TPoint): Double;
begin
  Result:=rv_poly_contains(@lp.Pol,lp.N, p,dR, ic,c)
end;

function rv_poly_online(lp: PLPoly; lp_n: int;
                        ic: int; const p: TPoint): int;
var
  d1,d2: int; i: int;
begin
  Result:=0;

  if ic < 0 then begin ic:=Abs(ic);
    Result:=iOnLine1(lp[ic-1],lp[ic], p)
  end else
  if ic = 0 then
    Result:=iOnLine1(lp[0],lp[1], p)
  else
  if ic = lp_n then
    Result:=iOnLine1(lp[ic-1],lp[ic], p)
  else begin
    d1:=iOnLine1(lp[ic-1],lp[ic], p);
    d2:=iOnLine1(lp[ic],lp[ic+1], p);
    if d1*d2 > 0 then Result:=d1
  end
end;

function rv_co_x(bp: PLPoly; bn: int;
                 lp: PLPoly; ln: int;
                 out c: TPoint): bool;

function Is_break(bp: PLPoly; bn, i1,i2: int): bool;
var
  j,j1,j2: int; a,b,c: TPoint;
begin
  Result:=false;

  if Abs(i1) > Abs(i2) then iSwap(i1,i2);

  if bn >= 3 then
  if (Abs(i1) = Abs(i2))
  or ((i1 >= 0) and ((i1+1) = Abs(i2)))
  or ((i1 = 0) and (Abs(i2) = bn)) then

  else begin
    if i1 < 0 then j1:=Abs(i1)
              else j1:=i1+1;

    j2:=Abs(i2)-1; if i2 <> 0 then
    for j:=j1 to j2 do begin
      c:=bp[j]; a:=bp[j-1]; b:=bp[j+1];
      if Dist_to_Line(c, a,b) >
         Long_Dist(c,a)/8 then begin
        Result:=true; Break
      end
    end;

    if Result then begin

      Result:=false;

      if i1 = 0 then j1:=bn-1
                else j1:=Abs(i1)-1;

      if i2 < 0 then j2:=Abs(i2)
                else j2:=i2+1;

      if j2 = bn then j2:=0;
      while true do begin
        c:=bp[j1];
        if j1 = 0 then a:=bp[bn-1]
                  else a:=bp[j1-1];

        if j1 = bn then b:=bp[1]
                  else b:=bp[j1+1];

        if Dist_to_Line(c, a,b) >
           Long_Dist(c,a)/8 then begin
          Result:=true; Break
        end;

        if j1 = j2 then Break;
        Dec(j1); if j1 = 0 then j1:=bn-1
      end
    end
  end
end;

function repeat_bp(bp: PLPoly; bn: int;
                   i1,i2: int; l1,l2: double;
                   var c: TPoint): bool;
begin
  Result:=false;

  if l1 > l2 then begin
    iSwap(i1,i2); xSwap(l1,l2);
  end;

  c:=bp[Abs(i1)];
  if i1 = bn then i1:=0;
  if i2 = 0 then i2:=bn;

  if Abs(i1) = Abs(i2) then
    Result:=true
  else
  if i1 < 0 then
    Result:=Abs(i1)+1 = i2
  else begin
    Result:=i1+1 = Abs(i2);

    if not Result then begin
      c:=bp[i1+1]; i2:=Abs(i2);

      if i1 = 0 then
      if i2 = bn then
        Result:=true
      else
      if bn-i2 < i2 then
        c:=bp[bn-1]
    end
  end
end;

var
  i,_i, j,j1,j2, k, eps: int;
  p,p1,p2, _p,_p1,_p2: TPoint;
  l1,l2,t: double;
begin
  Result:=false; c:=lp[0]; eps:=1;

  j:=-1;
  for i:=0 to ln-1 do begin p:=lp[i];
    if rv_poly_contains(bp,bn, p,eps, nil,_p) < 0 then
    begin j:=i; Break end
  end;

  if j >= 0 then begin

    i:=0; if j > 0 then i:=j;

    p2:=lp[i]; k:=0;
    while k < ln do begin
      p1:=p2; Inc(k); Inc(i);
      if i > ln then i:=1;
      p2:=lp[i];

      l1:=rv_poly_contains(bp,bn,p1,eps,@j1,_p1);
      if l1 >= 0 then begin

        if not Points_Equal(p1,_p1) then
        if rPolygonContainsPixel(lp,ln,
                                 _p1.X,_p1.Y) > 0 then
        l1:=-1;

        if l1 >= 0 then begin
          l2:=rv_poly_contains(bp,bn,p2,eps,@j2,_p2);
          if l2 >= 0 then

          if Is_break(bp,bn, j1,j2) then begin
          end else

          if Abs(l1-l2) < 1 then begin
            c:=p2; Result:=true; Break
          end else

          if not repeat_bp(bp,bn, j1,j2,l1,l2,c) then begin
            Result:=true; Break
          end
          else begin
            while k < ln do begin
              _i:=i+1; if _i > ln then _i:=1;
              p:=lp[_i];

              t:=rv_poly_contains(bp,bn,p,eps,@j,_p);
              if t < 0 then begin
                p2:=p; i:=_i; Inc(k); Break;
              end;

              if Is_break(bp,bn, j2,j) then begin
                p2:=p; i:=_i; Inc(k); Break
              end;

              if Abs(j2) < 0 then begin
                c:=p2; Result:=true; Break
              end else
              if not repeat_bp(bp,bn, j2,j,l2,t,c) then begin
                Result:=true; Break
              end;

              l1:=l2; p1:=p2;
              j2:=j; l2:=t; p2:=p;
              i:=_i; Inc(k)
            end;

            if Result then Break
          end
        end
      end
    end
  end
end;

function rv_poly_Indexof(lp: PLPoly; lp_n: int;
                         const p: TPoint;
                         eps: Double): int;
var
  i,j: int; t,r: Double;
begin
  Result:=-1;

  j:=0; r:=Long_dist(p,lp[0]);

  for i:=1 to lp_N do begin
    t:=Long_dist(p,lp[i]);
    if t < r then begin
      r:=t; j:=i
    end
  end;

  if r < eps then Result:=j
end;

function rv_line_Indexof(lp: PLLine;
                         const p: TPoint;
                         eps: Double): int;
begin
  Result:=rv_poly_Indexof(@lp.Pol,lp.N-1,p,eps)
end;

function rv_get_bline(bp,lp: PLLine;
                      const e1,e2: TPoint;
                      eps: double): int;
var
  i,i1,i2: int; swap: bool;
begin
  Result:=-1;

  i1:=rv_line_Indexof(lp,e1,eps);

  if i1 >= 0 then begin

    i2:=rv_line_Indexof(lp,e2,eps);
    if i2 < 0 then
      Result:=-2
    else begin
      swap:=false;
      if i1 > i2 then begin
        iSwap(i1,i2); swap:=true
      end;

      if i1 = 0 then
      if i2 > lp.N-i2 then begin
        i1:=i2; i2:=lp.N;
        swap:=not swap
      end;

      for i:=i1 to i2 do
      bp.Pol[i-i1]:=lp.Pol[i];
      Result:=i2 - i1;

      if swap then
      Swap_LPoly(@bp.Pol,nil,Result)
    end
  end;

  bp.N:=Result
end;

function Is_clip_garbage(lp: PLPoly; lp_n: int): bool;
var
  i: int; a,b,p: TPoint; l,t: double;
  lc: TLine_coeff; pc: TGauss;
begin
  Result:=false;

  if LPoly_Lock(lp,lp_n) then
  if lp_n <= 63 then begin

    a:=lp[0]; b:=a; l:=0;
    for i:=1 to lp_n-1 do begin
      p:=lp[i]; t:=Long_dist(a,p);
      if t > l then begin l:=t; b:=p end
    end;

    p:=lp[lp_n-1];
    t:=Long_dist(p,b);
    if t > l then a:=p;

    if Get_line_coeffp(a,b, lc) > 1 then begin
      Result:=true;
      for i:=0 to lp_n-1 do begin p:=lp[i];
        if not c_plot_to_line1(lc,p.X,p.Y,2,@t,pc) then
        begin Result:=false; Break end
      end
    end
  end
end;

procedure squeeze_rect(lp: PLLine; dr: double);
var
  l1,l2,l3,l4: double; p: TPoint;
begin
  if lp.N = 4 then
  if PolyLock(lp) then
  with lp^ do begin
    l1:=Long_dist(Pol[0],Pol[1]);
    l2:=Long_dist(Pol[1],Pol[2]);
    l3:=Long_dist(Pol[2],Pol[3]);
    l4:=Long_dist(Pol[3],Pol[4]);

    if l1 > dr*4 then
    if l2 > dr*4 then
    if l3 > dr*4 then
    if l4 > dr*4 then begin

      p:=Pol[0];
      Get_Next_Point(p,Pol[3],dr,Pol[0]);
      Get_Next_Point(Pol[3],p,dr,Pol[3]);

      p:=Pol[1];
      Get_Next_Point(p,Pol[2],dr,Pol[1]);
      Get_Next_Point(Pol[2],p,dr,Pol[2]);

      p:=Pol[0];
      Get_Next_Point(p,Pol[1],dr,Pol[0]);
      Get_Next_Point(Pol[1],p,dr,Pol[1]);

      p:=Pol[3];
      Get_Next_Point(p,Pol[2],dr,Pol[3]);
      Get_Next_Point(Pol[2],p,dr,Pol[2]);
    end;

    Pol[4]:=Pol[0];
  end
end;

function rv_poly_lock(lp: PLLine; lpMax: int;
                      bp: PLPoly; bn,dR: int): Boolean;
var
  p1,p2,c: TPoint; i1,i2: int; d1,d2: double;
begin
  Result:=PolyLock(lp);
  if not Result then begin

    p1:=lp.Pol[0];
    p2:=lp.Pol[lp.N];

    d2:=rv_poly_contains(bp,bn,p1,dR,@i1,c);

    if d2 >= 0 then begin
      d1:=rv_poly_contains(bp,bn,p2,dR,@i2,c);

      if d1 >= 0 then begin

        if i1 <> i2 then
        if d1 < d2 then begin

          if i1 < 0 then i1:=-i1
                    else Inc(i1);
          if i1 >= bn then Dec(i1,bn);

          i2:=Abs(i2);
          if i2 = bn then i2:=0;

          while i1 <> i2 do begin
            LPoly_Continue(lp,lpMax,bp[i1]);
            Inc(i1); if i1 >= bn then Dec(i1,bn);
          end

        end else
        if d1 > d2 then begin

          i1:=Abs(i1)-1;
          if i1 < 0 then Inc(i1,bn);

          if i2 < 0 then i2:=-i2
                    else Inc(i2);
          if i2 >= bn then Dec(i2,bn);

          while i1 <> i2 do begin
            LPoly_Continue(lp,lpMax,bp[i1]);
            Dec(i1); if i1 < 0 then Inc(i1,bn);
          end
        end;

        Result:=Lock_Poly(lp,lpMax)
      end
    end
  end
end;

// rhf_dm Process_unlock
function PolyDistPoly(lp1,lp2: PLLine;
                      out dist: double;
                      ip: PIntegers): Boolean;

function ProjToRib(lp: PLLine; vp: PLPoly;
                   out dist: double): int;
var
  i,dr,rc: int; lt,rb,q,p, v1,v2: TPoint;
  px,py, r,r1, t,t1, tk,tk1, w,w1,w2, c2, tx,ty: double;
  lc,lc1: TLine_coeff; xy: bool;
begin
  Result:=-1; v1:=vp[0]; v2:=vp[1];

  if Get_line_coeffp(v1,v2, lc) > 1 then
  if lc.ab > Small then begin

    xy:=Abs(lc.b) > Abs(lc.a);

    lt:=v1; rb:=v2; Swap_lPort(lt,rb);
    dr:=Max(rb.X-lt.X,rb.Y-lt.Y);
    Inc_LRect(lt,rb,dr,dr);

    r:=0; r1:=lc.len*8; tk1:=0; t1:=0;

    for i:=0 to lp.N do begin
      q:=p; p:=lp.Pol[i];

      if ((p.X >= lt.X) and (p.X <= rb.X))
      or ((p.Y >= lt.Y) and (p.Y <= rb.Y)) then begin

        px:=p.X; py:=p.Y;

        with lc do begin c2:=b*px - a*py;

          if xy then begin
            tx:=(c2*b-ca)/ab; ty:=-(a*tx+c)/b;
            tk:=(tx - x0) / (-b)
          end
          else begin
            ty:=-(c2*a+cb)/ab; tx:=-(b*ty+c)/a;
            tk:=(ty - y0) / a
          end
        end;

        with lc do
        t:=(a*px + b*py + c) / len;

        if (tk >= 0) and (tk <= 1) then begin

          if Abs(t) < r1 then begin
            r:=t; r1:=Abs(r); Result:=i
          end

        end else

        if i > 0 then
        if ((tk1 < 0) and (tk > 1))
        or ((tk1 > 1) and (tk < 0)) then

        if t*t1 > 0 then
        if Get_line_coeffp(q,p, lc1) > 1 then begin

          rc:=0;
          if p_dist_to_line(lc1, v1.X,v1.Y, w1) then Inc(rc);
          if p_dist_to_line(lc1, v2.X,v2.Y, w2) then Inc(rc);

          if rc > 0 then
          if w1*w2 > 0 then begin
            w:=Min(Abs(w1),Abs(w2));
            if t < 0 then w:=-w; t:=w;
            if Abs(t) < r1 then begin
              r:=t; r1:=Abs(r); Result:=i
            end
          end
        end;

        tk1:=tk; t1:=t
      end
    end
  end;

  dist:=r
end;

var
  i,i1,i2,rc: int; r,r1,t: Double;
begin
  Result:=false;

  r:=0; i2:=-1;
  for i:=0 to lp2.N-1 do begin
    rc:=ProjToRib(lp1,@lp2.Pol[i],t);
    if rc >= 0 then
    if (i2 < 0) or (Abs(t) < r1) then begin
      i1:=rc; i2:=i; r:=t; r1:=Abs(r);
      Result:=true
    end
  end;

  if Result then
  if Assigned(ip) then begin
    ip[0]:=i1; ip[1]:=i2
  end;

  dist:=r
end;

procedure lp_inter_z(lp: PLPoly; hp: PIntegers; N,z1,z2: int);
var
  i: int; r,l,kz: double;
begin
  if N >= 0 then
  if Assigned(hp) then begin
    hp[0]:=z1; hp[N]:=z2;

    if N > 1 then begin

      l:=PolyLength(lp,N); Dec(z2,z1);

      if l < 0.1 then begin
        kz:=z2/N; for i:=1 to N-1 do
        hp[i]:=z1 + Round(i*kz)
      end
      else begin
        kz:=z2/l; r:=0;
        for i:=1 to N-1 do begin
          r:=r+Long_Dist(lp[i-1],lp[i]);
          hp[i]:=z1 + Round(r*kz)
        end
      end
    end
  end
end;

function lp_join_trunc(lp1,lp2: PLLine; Eps: double): bool;
var
  a,b,c,d: TPoint; t: TGauss; k: double; rc: bool;
begin
  Result:=false;

  if lp1.N > 0 then begin

    with lp1^ do begin
      a:=Pol[N-1]; b:=Pol[N];
    end;

    while lp2.N > 0 do begin

      c:=lp2.Pol[0]; d:=lp2.Pol[1];

      rc:=false;
      if Long_dist(b,c) < Eps then

      if LL2ik(c,d, a,b, t,k) then
      if k <= 0 then Break else
      if k <= 1 then begin

        c.X:=Round(t.x);
        c.Y:=Round(t.y);

        if Points_Equal(c,d) then
          Poly_cut(lp2,0,1)
        else
          lp2.Pol[0]:=c;

        Result:=true; Break
      end
      else begin
        Poly_cut(lp2,0,1);
        Result:=true; rc:=true;
      end;

      if not rc then Break
    end
  end
end;

function lp_lock_trunc(lp: PLLine; Eps: double): bool;
var
  a,b,c,d: TPoint; t: TGauss; k: double; rc: bool;
begin
  Result:=false;

  if PolyLock(lp) then
  while lp.N > 3 do begin

    with lp^ do begin
      a:=Pol[N-2]; b:=Pol[N-1];
      c:=Pol[0]; d:=Pol[1];
    end;

    rc:=false;
    if Long_dist(b,c) < Eps then

    if LL2ik(c,d, a,b, t,k) then
    if k <= 0 then Break else
    if k <= 1 then begin

      c.X:=Round(t.x);
      c.Y:=Round(t.y);

      if Points_Equal(c,d) then
        Poly_cut(lp,0,1)
      else
        lp.Pol[0]:=c;

      lp.Pol[lp.N]:=lp.Pol[0];
      Result:=true; Break
    end
    else begin
      Poly_cut(lp,0,1);
      lp.Pol[lp.N]:=lp.Pol[0];

      Result:=true; rc:=true;
    end;

    if not rc then Break
  end;

  if PolyLock(lp) then
  while lp.N > 3 do begin

    with lp^ do begin
      a:=Pol[1]; b:=Pol[0];
      c:=Pol[N-1]; d:=lp.Pol[N-2];
    end;

    rc:=false;
    if Long_dist(b,c) < Eps then

    if LL2ik(c,d, a,b, t,k) then
    if k <= 0 then Break else
    if k <= 1 then begin

      c.X:=Round(t.x);
      c.Y:=Round(t.y);

      if Points_Equal(c,d) then
        Poly_cut(lp,lp.N-1,1)
      else
        lp.Pol[lp.N-1]:=c;

      Result:=true; Break
    end
    else begin
      Poly_cut(lp,lp.N-1,1);
      Result:=true; rc:=true;
    end;

    if not rc then Break
  end
end;

function lp_cls_ins(lp: PLPoly; n,i: int; eps: double): int;
var
  p: TPoint;
begin
  p:=lp[i];

  if i+2 <= n then
  if Abs(Dist_to_Line(lp[i+1],p,lp[i+2])) < eps then
  n:=cut_lpoly(lp,nil,n,i+1,1);

  if i >= 2 then
  if Abs(Dist_to_Line(lp[i-1],lp[i-2],p)) < eps then
  n:=cut_lpoly(lp,nil,n,i-1,1);

  Result:=n
end;

type
  teps_z = class
    function Execute(lp: PLPoly;
                     hp: PIntegers;
                     n: int; eps: double): int;
  private
    flp: PLPoly;
    fhp: PIntegers;
    fsp: int; feps: double;
  end;

function teps_z.Execute(lp: PLPoly;
                        hp: PIntegers;
                        n: int; eps: double): int;

procedure part(lp: PLPoly; hp: PIntegers; n: int);
var
  i,j: int; r,t,len,zk, z,z1,z2: double; a,b: TPoint;
begin
  if n > 0 then begin j:=-1;

    if n > 1 then begin
      a:=lp[0]; b:=lp[n]; j:=1; r:=-1;
      z1:=hp[0]; z2:=hp[n];

      len:=Long_dist(a,b);
      if len > 0 then begin
        zk:=(z2-z1)/len;
        for i:=1 to n-1 do begin

          z:=z1 + Long_dist(lp[i],a)*zk;

          t:=Abs(hp[i]-z);
          if (r < 0) or (t > r) then begin
            j:=i; r:=t
          end
        end;
      end;
      
      if r <= eps then j:=-1
    end;

    if j <= 0 then begin
      Inc(fsp);
      flp[fsp]:=lp[n];
      fhp[fsp]:=hp[n];
    end
    else begin
      part(lp,hp,j);
      part(@lp[j],@hp[j],n-j)
    end
  end
end;

begin
  flp:=lp; fhp:=hp;
  fsp:=0; feps:=eps;
  part(lp,hp,n); Result:=fsp
end;

function eps_lp_z(lp: PLPoly; hp: PIntegers;
                  n: int; eps: double): int;
var
  cls: teps_z;
begin
  Result:=n;

  cls:=teps_z.Create;
  try
    Result:=cls.Execute(lp,hp,n,eps)
  finally
    cls.Free
  end
end;

end.