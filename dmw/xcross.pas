unit xcross; interface

uses
  Classes,
  otypes,xlist,xclasses;

(*
  0 - Совмещение метрическое
  1 - Совмещение граничное
  2 - Примыкание метрическое
  3 - Примыкание граничное
  4 - Принадлежность
  5 - Пересечение
  6 - Смежность
  7 - Согласование [x]
  8 - Пересечение [x]
  9 - Принадлежность [2 -> 1]
*)

const
  crossMax = 32;

type
  tcrossRec = record
    p: TPoint; dir,lp_i: int; dist: double
  end;

  pcrossArray = ^tcrossArray;
  tcrossArray = Array[0..crossMax-1] of tcrossRec;

function lp_cross_vec(lp: PLPoly; n: int;
                      vp: PLPoly; edges: Boolean;
                      cp: pcrossArray; cn: int): int;

function lp_cross_vec1(lp: PLPoly; n: int;
                       vp: PLPoly; edges: bool;
                       out c: tcrossRec): bool;

function lp_Get_Cross(lp1: PLPoly; n1: int;
                      lp2: PLPoly; n2: int;
                      cp: PLPoly; cn: int;
                      ip1,ip2: PIntegers;
                      cpoints: bool): int;

function lp_Dir_Cross(lp1: PLPoly; n1,i1: int;
                      lp2: PLPoly; n2,i2: int): int;

function lp_Cross_Comp(lp1: PLPoly; n1: Integer;
                       lp2: PLPoly; n2: Integer;
                       out c: TPoint): Boolean;

procedure lp_sort_cpoints(lp,cp: PLPoly; ip: PIntegers; k: int);

function lp_ins_cpoints(lp: PLLine; hp: PIntegers; lp_max: int;
                        cp: PLPoly; ip: PIntegers; k: int): int;

function lp_co_points(list: TPointList;
                      lp1: PLPoly; n1: int;
                      lp2: PLPoly; n2: int): int;

function mf_co_points(list, mf1,mf2: TPointList): int;

function lp_co_ordinationX(lp1: PLPoly; n1: int;
                           lp2: PLPoly; n2: int;
                           eps: double; var c: TPoint): Boolean;

function lp_Comp(lp1: PLPoly; n1: int;
                 lp2: PLPoly; n2: int;
                 hp1,hp2: PIntegers;

                 cmd,opt: DWord;

                 iparams: PIntegers;
                 rparams: PDoubles;

                 pc: PPoint; out rc: int): Boolean;

function Allign_Level_Line(lp1,lp2: PLLine): Boolean;

function Middle_lline(lp1,lp2: PLLine; max_r: Double;
                      dst: TLLineProc): Double;

function lp_pickup_p(lp: PLLine;
                     const p: TPoint;
                     out lp_i: Integer): Boolean;

function lp_xmov(lp,cut: PLLine;
                 out i1,i2,i3: Integer;
                 out p3: TPoint): Boolean;

function House_to_Road(const a,b: TPoint; lp: PLLine;
                       out a_,b_: TPoint): Boolean;

function xClipPolyLine(clip,lp: PLLine): Boolean;

function xClipPolyLine1(dest: TPolyList;
                        clip,lp: PLLine): Integer;

function align_wire(lp: PLLine): Boolean;

function xArterPoly(lp,lp1,lp2: PLLine): Integer;

function lp_Pickup_z(lp: PLLine; hp: PIntegers;
                     const p: TPoint; out pz: Integer): Boolean;

function lp_sharp(lp: PLPoly; lp_n,fi: int): int;

function lp_off_sharp(lp: PLPoly;
                      hp: PIntegers;
                      lp_n,fi: int): int;

implementation

uses
  Math,
  xline,xpoly;

function lp_Vertex_Comp(lp1: PLPoly; n1: Integer;
                        lp2: PLPoly; n2: Integer;
                        hp1,hp2: PIntegers; opt: DWord;
                        d1,d2: Double; var pc: TPoint): Boolean;
var
  i,j, j1,j2,dj,dh: Integer;
  p: TPoint; xyz_not: Boolean;
begin
  Result:=true; j2:=0; pc:=lp1[0];

  xyz_not:=tlong(opt).b[3] = 2;

  for i:=0 to n1 do begin
    p:=lp1[i]; j1:=j2; j2:=-1;

    for j:=0 to n2 do begin
      if Long_Dist(p,lp2[j]) <= d1 then

      if hp1 = nil then begin
        j2:=j; Break
      end
      else begin
        dh:=Abs(hp1[i]-hp2[j]);

        if (xyz_not and (dh > d2))
        or (not xyz_not and (dh <= d2)) then
        begin j2:=j; Break end
      end
    end;

    if j2 < 0 then begin
      pc:=p; Result:=false; Break
    end else
    if i > 0 then begin
      dj:=Abs(j2-j1);

      if dj <= 1 then
      else
      if not LPoly_Lock(lp2,n2)
      or (n2-dj > 1) then begin
        pc:=p; Result:=false; Break
      end
    end
  end
end;

function lp_Rib_Comp(lp1: PLPoly; n1: Integer;
                     lp2: PLPoly; n2: Integer;
                     d: Double): Boolean;
var
  i,j, j1,j2, _j1,_j2,_j: Integer; ok: Boolean;
  p1,p2, q1,q2, q,c: TPoint; v: LVector;
begin
  Result:=false;

  if n1 >= 0 then
  if n2 >= 0 then begin

    Result:=true;

    for i:=0 to n1 do begin
      p2:=lp1[i]; ok:=false;
      q2:=lp2[0]; j2:=0;

      if n2 = 0 then
        ok:=Long_Dist(p2,q2) <= d
      else
      for j:=1 to n2 do begin
        q1:=q2; q2:=lp2[j];

        if Plot_to_Line(p2 ,q1,q2, c) then
        if Long_Dist(p2,c) <= d then begin

          j2:=j; ok:=true;

          if LineContainsPoint(q1,q2, p2) then begin

            if i > 0 then
            if Abs(j1-j2) > 1 then begin
              v[0]:=p1; v[1]:=p2;
              ok:=lp_Rib_Comp(lp2,n2,@v,1,d)
            end

          end else

          if i > 0 then
          if j1 <> j2 then begin

            _j1:=Min(j1,j2); _j2:=Max(j1,j2);

            if LPoly_Lock(lp2,n2) then
            if Abs(_j2-_j1) > n2 div 2 then

            if _j2 = n2 then begin
              _j2:=_j1; _j1:=0
            end;

            for _j:=_j1 to _j2-1 do begin
              q:=lp2[_j];
              if Plot_to_Line(q ,p1,p2, c) then begin
                if Long_Dist(q,c) > d then ok:=false
              end else
              if Long_Dist(q,p1) <= d then else
              if Long_Dist(q,p2) > d then ok:=false;
              if not ok then Break
            end
          end;

          if ok then Break
        end
      end;

      if not ok then begin
        Result:=false; Break
      end;

      j1:=j2; p1:=p2
    end
  end
end;

function lp_Edge_Comp(lp1: PLPoly; n1: Integer;
                      lp2: PLPoly; n2: Integer;
                      hp1,hp2: PIntegers;
                      Params: PDoubles; opt: DWord;
                      mf: Boolean;

                      out pc: TPoint;
                      out rc: int): Boolean;

function dz_ok(dz,opt: Integer; dp: PDoubles): Boolean;
begin
  Result:=false;

  case opt of
0:  Result:=true;

1:  if dp[3] >= 1 then Result:=true else
    Result:=(dz >= dp[2]) and (dz <= dp[3]);

2:  Result:=(dz < dp[2]) or (dz > dp[3]);
  end
end;

function l_xyz(lp: PLPoly; hp: PIntegers; i,n: Integer): lxyz;
var
  v: lxyz;
begin
  if i < 0 then i:=0;
  if i > n then i:=n;

  v.p:=lp[i]; Result.v.z:=0;
  if Assigned(hp) then v.v.z:=hp[i];

  Result:=v
end;

function p_to_l(const v: lxyz;
                lp: PLPoly; hp: PIntegers;
                n,opt1,xyz: int; dp: PDoubles;
                mf: Boolean): Boolean;
var
  i,i1,i2,z: int; p: TPoint;
  d,d1,d2: Double; v1,v2,c: lxyz;
begin
  Result:=false; p:=v.p;

  d1:=dp[0]; d2:=dp[1]; z:=0;

  if mf then begin

    i1:=0; i2:=n;
    if opt1 = 1 then begin
      i1:=1; i2:=n-1;
    end;

    if opt1 = 2 then begin
      i:=0; d:=Long_Dist(p,lp[i]);
      if (d >= d1) and (d <= d2) then
        Result:=true
      else begin
        i:=n; d:=Long_Dist(p,lp[i]);
        if (d >= d1) and (d <= d2) then
        Result:=true
      end;

      if Result then
      if Assigned(hp) then z:=hp[i];
    end else

    if d1 > Small then
      for i:=i1 to i2 do begin
        d:=Long_Dist(p,lp[i]);
        if d < d1 then begin
          Result:=false; Break
        end else
        if d <= d2 then begin
          if not Result then
          if Assigned(hp) then z:=hp[i];
          Result:=true;
        end
      end

    else
      for i:=i1 to i2 do begin
        d:=Long_Dist(p,lp[i]);
        if (d >= d1) and (d <= d2) then begin
          if Assigned(hp) then z:=hp[i];
          Result:=true; Break
        end
      end

  end else
  if xProject_to_LPoly(lp,n,p,d1,d2,i,c.p) >= 0 then begin

    case opt1 of
  0:  Result:=true;
  1:  Result:=(i <> 0) and (i < n);
  2:  Result:=(i = 0) or (i = n);
  3:  Result:=(i < 0) or not Points_Equal(p,lp[i]);
    end;

    if Result then begin
      d:=Long_dist(p,c.p);
      v1:=l_xyz(lp,hp,i,n);
      v2:=l_xyz(lp,hp,i+1,n);
      inter_vpoint(v1.v,v2.v,c.v);
      z:=c.v.z
    end

  end else
  if not LPoly_Lock(lp,n) then begin
    d:=Long_Dist(p,lp[0]);

    if (d >= d1) and (d <= d2) then begin
      if Assigned(hp) then z:=hp[0];
      Result:=true
    end else
    if n > 0 then begin
      d:=Long_Dist(p,lp[n]);
      if (d >= d1) and (d <= d2) then begin
        if Assigned(hp) then z:=hp[n];
        Result:=true
      end
    end
  end;

  if Result and (xyz = 1) then begin

    z:=Abs(z-v.v.z);
    if (dp[3] >= 1) and (xyz = 1) then begin
      d:=Hypot(d,z/dp[3]*dp[1]);
      Result:=(d >= d1) and (d <= d2)
    end;

    if Result then
    Result:=dz_ok(z,xyz,dp)
  end
end;

function This_Point(const p1,p2: lxyz;
                    dp: PDoubles; xyz: Integer): Boolean;
var
  dz: Integer; d: Double;
begin
  Result:=false;

  d:=Hypot(p2.v.x-p1.v.x,p2.v.y-p1.v.y);
  dz:=Abs(p2.v.z-p1.v.z);

  if (dp[3] >= 1) and (xyz = 1) then
  d:=Hypot(d,dz/dp[3]*dp[1]);

  if (d >= dp[0]) and (d <= dp[1]) then
  Result:=dz_ok(dz,xyz,dp)
end;

var
  i,opt1,xyz: Integer; q,q1,q2,p2: lxyz;
  dv: ZValues;
begin
  Result:=false; rc:=0;

  xyz:=tlong(opt).b[3]; // - ,+3D,-3D
  opt1:=tlong(opt).b[2];
  opt:=tlong(opt).b[0];

  dv[0]:=Params[0]; dv[1]:=Params[1];
  dv[2]:=Params[2]; dv[3]:=Params[3];

  if dv[0] > dv[1] then begin
    xSwap(dv[0],dv[1]);
    xSwap(dv[2],dv[3]);
  end;

  if n1 >= 0 then
  if not LPoly_Lock(lp1,n1) or (opt > 3) then begin

    q1.p:=lp1[0]; q1.v.z:=0;
    if Assigned(hp1) then q1.v.z:=hp1[0];

    q2.p:=lp1[n1]; q2.v.z:=0;
    if Assigned(hp1) then q2.v.z:=hp1[n1];

    pc:=q1.p;

    if n2 = 0 then begin rc:=-1;

      p2.p:=lp2[0]; p2.v.z:=0;
      if Assigned(hp2) then p2.v.z:=hp2[0];

      case opt of
    0:  if This_Point(q1,p2,@dv,xyz) then rc:=0;
    1:  if This_Point(q2,p2,@dv,xyz) then rc:=n1;

    2,
    3:  if This_Point(q1,p2,@dv,xyz) then rc:=0 else
        if This_Point(q2,p2,@dv,xyz) then rc:=n1;

    4:  for i:=0 to n1 do begin
          q.p:=lp1[i]; q.v.z:=0;
          if Assigned(hp1) then q.v.z:=hp1[i];

          if This_Point(q,p2,@dv,xyz) then
          begin rc:=i; Break end
        end
      end;

      if rc >= 0 then begin
        pc:=lp1[rc]; Result:=true;

        if hp1 = nil then begin
          if Assigned(hp2) then Result:=xyz = 2
        end else
        if hp2 = nil then Result:=xyz = 2
      end

    end else

    case opt and 7 of
  0:  Result:=p_to_l(q1,lp2,hp2,n2,opt1,xyz,@dv,mf);
  1:  begin
        Result:=p_to_l(q2,lp2,hp2,n2,opt1,xyz,@dv,mf); pc:=q2.p
      end;

  2:  begin
        if p_to_l(q1,lp2,hp2,n2,opt1,xyz,@dv,mf) then rc:=1;
        if p_to_l(q2,lp2,hp2,n2,opt1,xyz,@dv,mf) then Inc(rc,2);
        if rc <> 0 then begin
          if (rc and 1) = 0 then pc:=q2.p;
          Result:=true
        end
      end;

  3:  begin
        if p_to_l(q1,lp2,hp2,n2,opt1,xyz,@dv,mf) then rc:=1;
        if p_to_l(q2,lp2,hp2,n2,opt1,xyz,@dv,mf) then Inc(rc,2);
        if opt and 8 <> 0 then Result:=rc <> 0 else Result:=rc = 3;
        if Result then begin
          if (rc and 1) = 0 then pc:=q2.p
        end else
        if (opt and 8) = 0 then begin
          if (rc and 1) <> 0 then pc:=q2.p
        end
      end;

  4:  for i:=0 to n1 do begin

        q1.p:=lp1[i]; q1.v.z:=0;
        if Assigned(hp1) then q1.v.z:=hp1[i];

        if p_to_l(q1,lp2,hp2,n2,opt1,xyz,@dv,mf) then begin
          pc:=q1.p; Result:=true; Break
        end
      end;

  5:  for i:=0 to n1 do begin
        q1.p:=lp1[i]; q1.v.z:=0;
        if Assigned(hp1) then q1.v.z:=hp1[i];

        if p_to_l(q1,lp2,hp2,n2,opt1,xyz,@dv,mf) then
          Result:=true
        else begin
          Result:=false; Break
        end
      end
    end

  end
end;

function lp_cross_vec(lp: PLPoly; n: int;
                      vp: PLPoly; edges: Boolean;
                      cp: pcrossArray; cn: int): int;

function Ribs_Cross(const p1,p2,p3: TPoint;
                    vp,vb: PLPoly; edges: Boolean;
                    out c: TPoint): int;
var
  q1,q2,qc: TPoint; t: tgauss; d1,d2: Double;
begin
  Result:=0; q1:=vp[0]; q2:=vp[1];

  if LL2(p1.X,p1.Y,p2.X,p2.Y, q1.X,q1.Y,q2.X,q2.Y, 0,1, t) then
  begin
    if q1.X = q2.X then t.x:=q1.X;
    if q1.Y = q2.Y then t.y:=q1.Y;

    if (t.x >= vb[0].X) and (t.x <= vb[1].X) then
    if (t.y >= vb[0].Y) and (t.y <= vb[1].Y) then begin
      qc.X:=Round(t.x); qc.Y:=Round(t.y);

      if edges or not Points_Equal(qc,q1) then
      if edges or not Points_Equal(qc,q2) then

      if Points_Equal(qc,p2) then begin
        if not Points_Equal(p2,p3) then
        if not Ribs_Equal(p2,p3, q1,q2) then begin

          d1:=Dist_to_Line(p1, q1,q2);
          d2:=Dist_to_Line(p3, q1,q2);

          if (d1 <> 0) and (d2 <> 0) then
          if (d1 > 0) <> (d2 > 0) then begin
            if d1 < 0 then Result:=-1
                      else Result:=+1;
            c:=qc
          end
        end
      end else

      if Points_Equal(qc,p1) then begin
        d2:=Dist_to_Line(p2, q1,q2);
        if d2 > 0 then Result:=-1 else
        if d2 < 0 then Result:=+1;
        c:=qc
      end
      else begin
        d1:=Dist_to_Line(p1, q1,q2);
        if d1 < 0 then Result:=-1 else
        if d1 > 0 then Result:=+1;
        c:=qc
      end
    end
  end
end;

var
  i: int; lp_lock: Longbool;
  p1,p2,p3, _q,q1,q2, qc: TPoint;
  c: tcrossRec; p_lt,p_rb: TPoint;
  vb: LVector; dist: double;
begin
  Result:=0;

  if n > 0 then begin

    lp_lock:=LPoly_Lock(lp,n);

    q1:=vp[0]; q2:=vp[1];
    Swap_lRect(q1,q2, vb[0],vb[1]);

    p2:=lp[0]; dist:=0;
    for i:=1 to n do begin

      p1:=p2; p2:=lp[i]; p3:=p2;
      if i < n then p3:=lp[i+1] else
      if lp_lock then p3:=lp[1];

      Swap_lRect(p1,p2, p_lt,p_rb);

      if (p_lt.X <= vb[1].X) and (p_rb.X >= vb[0].X) then
      if (p_lt.Y <= vb[1].Y) and (p_rb.Y >= vb[0].Y) then

      if Ribs_Equal(p1,p2, q1,q2) then
      else begin
        c.dir:=Ribs_Cross(p1,p2,p3, vp,@vb,edges, c.p);
        if c.dir <> 0 then

        if (Result = 0)
        or not Points_equal(c.p,qc) then begin

          c.lp_i:=i; c.dist:=dist;
          if Points_Equal(p1,c.p) then Dec(c.lp_i)
          else c.dist:=dist+Long_Dist(p1,c.p);

          cp[0]:=c; cp:=@cp[1]; qc:=c.p; Dec(cn);

          Inc(Result); if cn <= 0 then Break
        end
      end;

      dist:=dist+Long_Dist(p1,p2)
    end
  end
end;

function lp_cross_vec1(lp: PLPoly; n: int;
                       vp: PLPoly; edges: bool;
                       out c: tcrossRec): bool;
var
  i: int; b,p: TPoint; cv: tcrossArray;
begin
  Result:=false;
  if lp_cross_vec(lp,n,vp,edges,@cv,32) = 1 then begin
    c:=cv[0]; i:=cv[0].lp_i; p:=cv[0].p;
    if not Points_Equal(p,lp[i]) then i:=-i;
    c.lp_i:=i; Result:=true
  end
end;

function lp_Get_Cross(lp1: PLPoly; n1: int;
                      lp2: PLPoly; n2: int;
                      cp: PLPoly; cn: int;
                      ip1,ip2: PIntegers;
                      cpoints: bool): int;

function xLL2(const p1,p2, q1,q2: TPoint; out c: TGauss): bool;
var
  dx1,dy1,dx2,dy2,a2,b2,c2,len2: double;
  x1,y1,x2,y2,x3,y3,x4,y4,rmu: double;
begin
  Result:=false;

  x1:=p1.X; y1:=p1.Y;
  x2:=p2.X; y2:=p2.Y;
  x3:=q1.X; y3:=q1.Y;
  x4:=q2.X; y4:=q2.Y;

  c.x:=x1; c.y:=y1;

  dx1:=x2-x1; dy1:=y2-y1;
  dx2:=x4-x3; dy2:=y4-y3;

  rmu:=dx1*dy2-dy1*dx2;

  if Abs(rmu) > 0.001 then begin
    rmu:=((x4-x1)*dy2-(y4-y1)*dx2)/rmu;

    if (rmu >= 0) and (rmu <= 1) then begin

      c.x:=x1+rmu*dx1; c.y:=y1+rmu*dy1;

      a2:=dy2; b2:=-dx2;
      c2:=y3*x4 - x3*y4;

      len2:=Hypot(dx2,dy2);
      if Abs(a2*x1 + b2*y1 + c2) / len2 < 1 then
        begin c.x:=x1; c.y:=y1 end
      else
      if Abs(a2*x2 + b2*y2 + c2) / len2 < 1 then
        begin c.x:=x2; c.y:=y2 end;

      Result:=true
    end
  end
end;

function Ribs_Cross(const p1,p2,p3, q1,q2: TPoint;
                    pb,qb: PLPoly; out c: TPoint): Boolean;

function xPoints_Equal(const p1: tgauss; const p2: TPoint): bool;
begin
  Result:=(Abs(p1.x-p2.X) < 0.8) and (Abs(p1.y-p2.Y) < 0.8)
end;

var
  p_lt,p_rb, q_lt,q_rb, q: TPoint;
  t: TGauss; d1,d2: Double; rc: bool;
begin
  Result:=false;

  p_lt:=pb[0]; p_rb:=pb[1];
  q_lt:=qb[0]; q_rb:=qb[1];

  if xLL2(p1,p2, q1,q2, t) then begin
    if q_lt.X = q_rb.X then t.x:=q_lt.X;
    if q_lt.Y = q_rb.Y then t.y:=q_lt.Y;

    if (t.x >= q_lt.X) and (t.x <= q_rb.X) then
    if (t.y >= q_lt.Y) and (t.y <= q_rb.Y) then begin
      q.X:=Round(t.x); q.Y:=Round(t.y);

      if xPoints_Equal(t,p2) then begin
        if not Points_Equal(p2,p3) then
        if not Ribs_Equal(p2,p3, q1,q2) then

        if not xLL2(p2,p3, q1,q2, t) then begin
          d1:=Dist_to_Line(p1, q1,q2);
          if Abs(d1) < 0.7 then d1:=0;

          d2:=Dist_to_Line(p3, q1,q2);
          if Abs(d2) < 0.7 then d2:=0;

          if (d1 <> 0) and (d2 <> 0) then
          if (d1 > 0) <> (d2 > 0) then
          begin c:=q; Result:=true end
        end
      end
      else begin
        rc:=Points_Equal(q,q2);
        if rc then
        if (t.x > qb[0].X) or (t.x < qb[0].X)
        or (t.y > qb[0].Y) or (t.y < qb[0].Y) then
        rc:=false;

        if Points_Equal(q,p1) then
          begin c:=q; Result:=true end
        else

        if (q.X >= p_lt.X) and (q.X <= p_rb.X) and
           (q.Y >= p_lt.Y) and (q.Y <= p_rb.Y) then

        begin c:=q; Result:=true end
      end
    end
  end
end;

function lp_contains_rib(lp: PLPoly; lp_N: Integer;
                         const p1,p2: TPoint): Integer;
var
  cx,cy: Double;
begin
  Result:=_rPolygonContainsPixel(lp,lp_N,p2.X,p2.Y,nil);
  if Result = 0 then begin
    cx:=p1.X/2 + p2.X/2; cy:=p1.Y/2 + p2.Y/2;
    Result:=_rPolygonContainsPixel(lp,lp_N,cx,cy,nil)
  end
end;

function icross_p1(const p1,p2, q1,q2: TPoint): bool;
var
  t: TGauss;
begin
  Result:=false;
  if xLL2(p1,p2, q1,q2, t) then
  if Round(t.x) = p1.X then
  if Round(t.y) = p1.Y then
  Result:=true
end;

function icross1(lp1: PLPoly; n1: int; lock1: bool;
                 lp2: PLPoly; n2: int; lock2: bool;
                 i,j: int; var _i1: int): bool;
var
  p1,p2,q1,q2,t1,t2,a,b,c: TPoint;
  i1,i2,InSide1,InSide2,_j: int;
  d1,d2,cx,cy: Double;
begin
  Result:=false;

  if (j > 0) or lock2 then
  if (i > 1) or lock1 then begin

    if i > 0 then p1:=lp1[i-1]
    else p1:=lp1[n1-1]; p2:=lp1[i];

    if j > 0 then q1:=lp2[j-1] else
    q1:=lp2[n2-1]; q2:=lp2[j];

    InSide1:=0; _j:=-1;

    if lock2 then
      InSide1:=lp_Contains_rib(lp2,n2,p1,p2)
    else
    if LineContainsPoint(q1,q2,p1)
    or icross_p1(p1,p2, q1,q2) then

    if Points_Equal(p1,q1) then begin
      d1:=0; _j:=j-1; InSide1:=1
    end
    else begin
      d1:=Dist_to_Line(p2,q1,q2);
      if d1 <> 0 then InSide1:=1;
    end;

    if InSide1 <> 0 then begin

      i1:=i-1; i2:=i; t2:=p1;
      if i2 = n1 then
      if lock1 then i2:=0 else i2:=-1;

      while i1 <> i2 do begin
        Dec(i1); if i1 < 0 then begin
          if not lock1 then Break; i1:=n1-1;
        end; t1:=t2; t2:=lp1[i1];

        if lock2 then begin

          cx:=t1.X/2+t2.X/2;
          cy:=t1.Y/2+t2.Y/2;

          Inside2:=_rPolygonContainsPixel(lp2,n2,cx,cy,nil);
          if Inside2 = 0 then
          Inside2:=_rPolygonContainsPixel(lp2,n2,t2.X,t2.Y,nil);

          if InSide1 = InSide2 then Break else
          if Abs(InSide1) = Abs(InSide2) then begin
            if Inside2 = 1 then _i1:=i1+1;
            Result:=true; Break
          end

        end else
        if not LineContainsPoint(q1,q2,t2) then

        if Points_Equal(t1,q2)
        or LineContainsPoint(t1,t2,q2) then begin

          Inc(j);
          while j <= n2 do begin
            q1:=q2; q2:=lp2[j];

            if Points_Equal(q2,t2)
            or LinecontainsPoint(q1,q2,t2) then Break;

            if not LinecontainsPoint(t1,t2,q2) then begin
              d2:=Dist_to_Line(t2,q1,q2);
              if d2 <> 0 then begin

                if _j >= 0 then begin
                  j:=_j-1; if j < 0 then Inc(j,n2);
                  d1:=Dist_to_Line(p2,lp2[j],lp2[_j])
                end;

                Result:=(d1 > 0) <> (d2 > 0);
              end;

              j:=n2+1; Break;
            end;

            Inc(j)
          end;

          if j > n2 then Break

        end else

        if Points_Equal(t1,q1)
        or LineContainsPoint(t1,t2,q1) then begin

          Dec(j);
          while j > 0 do begin
            q2:=q1; q1:=lp2[j-1];

            if Points_Equal(q1,t2)
            or LinecontainsPoint(q1,q2,t2) then Break;

            if not LinecontainsPoint(t1,t2,q1) then begin
              d2:=Dist_to_Line(t2,q1,q2);
              if d2 <> 0 then begin
                if _j >= 0 then
                d1:=Dist_to_Line(p2,lp2[_j],lp2[_j+1]);

                Result:=(d1 > 0) <> (d2 > 0);
              end;

              j:=0; Break;
            end; Dec(j)
          end;

          if j <= 0 then Break

        end
        else begin
          d2:=Dist_to_Line(t2,q1,q2);
          if d2 <> 0 then
          Result:=(d1 > 0) <> (d2 > 0);
          Break
        end;

        if not lock1 then
        if i1 = 0 then Break;
      end
    end
  end
end;

var
  i,j,_i,_j: int;
  p1,p2,p3, _q,q1,q2, c,qc: TPoint;
  lt2,rb2: TPoint; pb,qb: LVector;
  lock1,lock2,rc,rc1,rc2: bool;
begin
  Result:=0;

  if n1 > 0 then
  if n2 > 0 then begin

    lock1:=LPoly_Lock(lp1,n1);
    lock2:=LPoly_Lock(lp2,n2);

    Max_Poly_Bound(lp2,n2+1, lt2,rb2);

    p2:=lp1[0];
    for i:=1 to n1 do begin

      p1:=p2; p2:=lp1[i]; p3:=p2;
      if i < n1 then p3:=lp1[i+1] else
      if lock1 then p3:=lp1[1];

      pb[0]:=p1; pb[1]:=p2;
      if p1.X > p2.X then begin pb[0].X:=p2.X; pb[1].X:=p1.X end;
      if p1.Y > p2.Y then begin pb[0].Y:=p2.Y; pb[1].Y:=p1.Y end;

      if (pb[0].X <= rb2.X) and (pb[1].X >= lt2.X) then
      if (pb[0].Y <= rb2.Y) and (pb[1].Y >= lt2.Y) then
      begin
        q1:=lp2[0]; if lock2 then
        q1:=lp2[n2-1]; q2:=lp2[0];

        for j:=1 to n2 do begin
          _q:=q1; q1:=q2; q2:=lp2[j];

          qb[0]:=q1; qb[1]:=q2;
          if q1.X > q2.X then begin qb[0].X:=q2.X; qb[1].X:=q1.X end;
          if q1.Y > q2.Y then begin qb[0].Y:=q2.Y; qb[1].Y:=q1.Y end;

          rc:=false;
          rc1:=false; rc2:=false;
          _i:=i; _j:=j;

          if (qb[0].X <= pb[1].X) and (qb[1].X >= pb[0].X) then
          if (qb[0].Y <= pb[1].Y) and (qb[1].Y >= pb[0].Y) then

          if Ribs_Equal(p1,p2, q1,q2) then
          else
          if Ribs_Equal(p1,p2, _q,q1) then
          else
          if Ribs_Cross(p1,p2,p3, q1,q2, @pb,@qb, c) then

          if Points_Equal(p1,c) then begin
            _i:=i-1;
            rc1:=icross1(lp1,n1,lock1,
                         lp2,n2,lock2, i,j, _i);
            if cpoints then _i:=i-1
          end else
          if Points_Equal(q1,c) then begin
            _j:=j-1;
            rc2:=icross1(lp2,n2,lock2,
                         lp1,n1,lock1, j,i, _j);
            if cpoints then _j:=j-1
          end
          else rc:=true;

          if rc or rc1 or rc2 then
          if (Result = 0)
          or not Points_equal(c,qc) then begin

            if rc1 then
              c:=lp1[_i]
            else begin _i:=-i;
              if Points_Equal(c,p1) then _i:=i-1 else
              if Points_Equal(c,p2) then _i:=i;
            end;

            if rc2 then
              c:=lp2[_j]
            else begin _j:=-j;
              if Points_Equal(c,q1) then _j:=j-1 else
              if Points_Equal(c,q2) then _j:=j;
            end;

            if Assigned(ip1) then begin
              ip1[0]:=_i; ip1:=@ip1[1]
            end;

            if Assigned(ip2) then begin
              ip2[0]:=_j; ip2:=@ip2[1]
            end;

            cp[0]:=c; cp:=@cp[1]; Inc(Result);
            Dec(cn); if cn <= 0 then Break;
            qc:=c;
          end;
        end
      end;

      if (Result > 0) and (cn <= 0) then Break
    end
  end
end;

function lp_Dir_Cross(lp1: PLPoly; n1,i1: int;
                      lp2: PLPoly; n2,i2: int): int;

function get_v(vp,lp: PLPoly; n,i: int): int;
begin
  Result:=-1;
  if i < 0 then begin
    i:=Abs(i);
    vp[0]:=lp[i-1];
    vp[1]:=lp[i];
    Result:=1
  end else
  if i = 0 then begin
    vp[0]:=lp[0];
    vp[1]:=lp[1];
    Result:=1
  end else
  if i = n then begin
    vp[0]:=lp[n-1];
    vp[1]:=lp[n];
    Result:=1
  end else
  if i < n then begin
    vp[0]:=lp[i-1];
    vp[1]:=lp[i];
    vp[2]:=lp[i+1];
    Result:=2
  end
end;

function Onlinev(vp: PLPoly; vn: int;
                 const p: TPoint): int;
var
  rc1,rc2: int;
begin
  rc1:=iOnline1(vp[0],vp[1],p); 

  if vn > 1 then begin
    rc2:=iOnline1(vp[1],vp[2],p);
    if rc1*rc2 <= 0 then rc1:=0
  end;

  Result:=rc1
end;

var
  rc1,rc2: int; v1,v2: LOrient;
begin
  Result:=0;

  n1:=get_v(@v1,lp1,n1,i1);

  if n1 > 0 then
  if Abs(i2) > 0 then
  if i2 < n2 then begin
    n2:=get_v(@v2,lp2,n2,i2);
    if n2 = 1 then begin

      rc1:=Onlinev(@v1,n1,v2[0]);
      rc2:=Onlinev(@v1,n1,v2[1]);

      if rc1*rc2 < 0 then
      Result:=rc1
    end
  end
end;

function lp_Cross_Comp(lp1: PLPoly; n1: Integer;
                       lp2: PLPoly; n2: Integer;
                       out c: TPoint): Boolean;
begin
  Result:=lp_Get_Cross(lp1,n1, lp2,n2, @c,1, nil,nil,false) > 0
end;

procedure lp_sort_cpoints(lp,cp: PLPoly; ip: PIntegers; k: int);
var
  i,j, i1,i2: int; p: TPoint; swap: bool;
begin
  for i:=0 to k-2 do
  for j:=i+1 to k-1 do begin
    i1:=ip[i]; i2:=ip[j]; swap:=false;

    if Abs(i2) < Abs(i1) then
      swap:=true
    else
    if Abs(i1) = Abs(i2) then

    if (i1 >= 0) and (i2 < 0) then
      swap:=true
    else
    if (i1 < 0) and (i2 < 0) then begin
      p:=lp[Abs(i1)-1];
      swap:=Long_Dist(p,cp[i]) >
            Long_Dist(p,cp[j])
    end;

    if swap then begin
      iSwap(ip[i],ip[j]);
      Swap_LPoints(cp[i],cp[j])
    end
  end
end;

function lp_ins_cpoints(lp: PLLine; hp: PIntegers; lp_max: int;
                        cp: PLPoly; ip: PIntegers; k: int): int;
var
  i,j: int; v: lxyz;
begin
  Result:=0;

  lp_sort_cpoints(@lp.Pol,cp,ip,k);

  for i:=k-1 downto 0 do
  if ip[i] < 0 then begin
    j:=ip[i]; v.p:=cp[i]; v.v.z:=0;

    if Assigned(hp) then
    LPoly_Restore_z_axe(lp,hp, v.v,j);

    LPoly_Insert(lp,hp,Abs(j)-1,lp_max,v.v);
    Inc(Result)
  end
end;

function lp_co_points(list: TPointList;
                      lp1: PLPoly; n1: int;
                      lp2: PLPoly; n2: int): int;
var
  lt,rb,p: TPoint; i: int;
begin
  list.Clear;

  Max_Poly_Bound(lp2,n2, lt,rb);

  for i:=0 to n1 do begin p:=lp1[i];
    if PortContainsPoint(lt,rb,p.X,p.Y) then
    if Polyline_locate(lp2,n2,p,-1) >= 0 then
    list.Add(@p)
  end;

  Result:=list.Count
end;

function mf_co_points(list, mf1,mf2: TPointList): int;
var
  lt,rb,p: TPoint; i,n1: int; lp: PLPoly;
begin
  list.Clear;

  mf1.Get_bound(lt,rb);

  lp:=mf2.First; n1:=mf2.Count-1;

  for i:=0 to n1 do begin p:=lp[i];
    if PortContainsPoint(lt,rb,p.X,p.Y) then
    if mf1.IndexOf(p.X,p.Y) >= 0 then
    list.Add(@p)
  end;

  Result:=list.Count
end;

function lp_contiguity(lp1: PLPoly; n1: int;
                       lp2: PLPoly; n2: int;
                       var c: TPoint): bool;

function lines_equal(lp1: PLPoly; n1: int;
                     lp2: PLPoly; n2: int;
                     i2,dir: int): bool;
var
  i: int; p1,p2: TPoint; lock2: bool;
begin
  Result:=true;

  lock2:=LPoly_Lock(lp2,n2);

  for i:=2 to n1 do begin
    p1:=lp1[i];

    if dir < 0 then begin
      Dec(i2); if i2 < 0 then
      if lock2 then i2:=n2-1 else begin
        Result:=false; Break
      end
    end
    else begin
      Inc(i2); if i2 > n2 then
      if lock2 then i2:=1 else begin
        Result:=false; Break
      end
    end;

    p2:=lp2[i2];
    if not Points_Equal(p1,p2) then begin
      Result:=false; Break
    end
  end
end;

var
  i1,i2: int; a1,b1, a2,b2, lt1,rb1, lt2,rb2: TPoint;
begin
  Result:=false;

  i1:=0;
  while i1 < n1 do begin
    a1:=lp1[i1]; Inc(i1); b1:=lp1[i1];
    if Points_Equal(a1,b1) then Continue;

    lt1:=a1; rb1:=b1;
    if a1.X > b1.X then begin
      lt1.X:=b1.X; rb1.X:=a1.X;
    end;
    if a1.Y > b1.Y then begin
      lt1.Y:=b1.Y; rb1.Y:=a1.Y;
    end;

    i2:=0;
    while i2 < n2 do begin
      a2:=lp2[i2]; Inc(i2); b2:=lp2[i2];
      if Points_Equal(a2,b2) then Continue;

      lt2:=a2; rb2:=b2;
      if a2.X > b2.X then begin
        lt2.X:=b2.X; rb2.X:=a2.X;
      end;

      if a2.Y > b2.Y then begin
        lt2.Y:=b2.Y; rb2.Y:=a2.Y;
      end;

      if PortContainsPort(lt1,rb1, lt2,rb2) then

      if Points_Equal(b1,a2) then begin
        if LineContainsPoint(a2,b2, a1) then begin
          if (i1 > 0) or not Points_Equal(a1,b2) then begin
            c:=a1; Result:=true; Break
          end
          else begin
            Result:=lines_equal(lp1,n1, lp2,n2,i2-1,-1);
            i1:=n1; Break
          end
        end
      end else

      if Points_Equal(b1,b2) then begin
        if LineContainsPoint(a2,b2, a1) then begin
          if (i1 > 0) or not Points_Equal(a1,a2) then begin
            c:=a1; Result:=true; Break
          end
          else begin
            Result:=lines_equal(lp1,n1, lp2,n2,i2,+1);
            i1:=n1; Break
          end
        end
      end
    end;

    if Result then Break
  end
end;

function lp_co_ordinationX(lp1: PLPoly; n1: int;
                           lp2: PLPoly; n2: int;
                           eps: double; var c: TPoint): Boolean;

function mk_continue(lp: PLPoly; n: int; lock: bool;
                     const m1,m2: TLineMarker;
                     const p1,p2: TPoint;
                     eps: double): Boolean;
var
  i,i1,i2,j: int; a,b: TPoint;
begin
  Result:=true;

  if Points_Equal(m1.pc,m2.pc) then
    Result:=Points_Equal(p1,p2)
  else begin
    i1:=m1.Ind; i2:=m2.Ind; a:=m1.pc; b:=m2.pc;

    if (i2 < i1) and lock then
    if (i1-i2) > n div 2 then Inc(i1,n);

    if i1 > i2 then iSwap(i1,i2);

    for i:=i1 to i2-1 do begin
      j:=i; if j > n then Dec(j,n);
      if Abs(Dist_to_Line(lp[j], a,b)) > eps then begin
        Result:=false; Break
      end
    end
  end
end;

var
  i,j,i1,n,di,Ind1,dr: int;
  p1,p2, t1,t2, lt1,rb1: TPoint;
  m1,m: TLineMarker; d: double;
  lock1,dot,dot1,rc: bool;
begin
  Result:=false;

  if eps < 0.1 then eps:=1;

  Fillchar(m1,Sizeof(m1),0); i1:=-1;

  lock1:=LPoly_Lock(lp1,n1);
  n:=n2; if LPoly_Lock(lp2,n2) then Inc(n);

  dr:=Round(eps)+2;
  Max_Poly_Bound(lp1,n1+1, lt1,rb1);
  Inc_LRect(lt1,rb1, dr,dr);

  for i:=0 to n do begin p1:=p2;

    if i > n2 then p2:=lp2[n-n2]
              else p2:=lp2[i];

    rc:=false;
    if (p2.X >= lt1.X) and (p2.X <= rb1.X) then
    if (p2.Y >= lt1.Y) and (p2.Y <= rb1.Y) then
    rc:=ProjectToPolyLine(lp1,n1,p2,0,eps,m.Ind,m.pc);

    if not rc then begin

      if (i1 >= 0) and (i1 = i-1) then
      if Ind1 < 0 then begin
        j:=Abs(Ind1); t1:=lp2[i1];
        if xPlot_to_Line(lp1[j-1], t1,p2, eps,t2,d)
        or xPlot_to_Line(lp1[j], t1,p2, eps,t2,d) then begin
          c:=t1; Result:=true; Break
        end
      end;

      i1:=-1
    end
    else begin
      Ind1:=m.Ind; m.Ind:=Abs(m.Ind);

      dot:=Points_Equal(p2,m.pc) and
           Points_Equal(lp1[m.Ind],m.pc);

      if (n1 = 0) and dot then begin
        Result:=false; Break
      end;

      if i1 >= 0 then
      if not mk_continue(lp1,n1,lock1, m1,m, p1,p2, eps) then
        i1:=-1
      else
      if not dot1 then begin
        c:=lp2[i1]; Result:=true; Break
      end else
      if not dot then begin
        c:=lp2[i]; Result:=true; Break
      end
      else begin
        di:=Abs(m.Ind-m1.Ind);

        if (di > 1) and lock1 then
        if m1.Ind < m.Ind then begin
          if m1.Ind = 0 then
          if m.Ind = n1-1 then di:=1;
        end else
        if m1.Ind = n1-1 then
        if m.Ind = 0 then di:=1;

        if di > 1 then begin

          j:=m1.Ind+1;
          if (j > n1) and lock1 then j:=1;

          if j <= n1 then begin t1:=lp1[j];
            if xPlot_to_Line(t1 ,m1.pc,m.pc,eps, t2,d) then
            begin c:=t1; Result:=true end
          end;

          if not Result then begin
            j:=m1.Ind-1;
            if (j < 0) and lock1 then j:=n1-1;

            if j >= 0 then begin t1:=lp1[j];
              if xPlot_to_Line(t1 ,m1.pc,m.pc,eps, t2,d) then
              begin c:=t1; Result:=true end
            end
          end;

          if Result then Break
        end
      end;

      if (i1 < 0) or dot then begin
        i1:=i; dot1:=dot;

        if not dot then
        if (i > 0) and (Ind1 < 0) then begin
          j:=Abs(Ind1); t1:=lp2[i-1];
          if xPlot_to_Line(lp1[j-1], t1,p2, eps,t2,d)
          or xPlot_to_Line(lp1[j], t1,p2, eps,t2,d) then begin
            c:=t1; Result:=true; Break
          end
        end
      end;

      m1:=m
    end
  end
end;

function lp_Comp(lp1: PLPoly; n1: int;
                 lp2: PLPoly; n2: int;
                 hp1,hp2: PIntegers;

                 cmd,opt: DWord;

                 iparams: PIntegers;
                 rparams: PDoubles;

                 pc: PPoint; out rc: int): Boolean;
var
  c: TPoint;
  cv: TPoints16;
  i1,i2: IValues16;
  i,k: int;
begin
  Result:=false; rc:=0;

  c.X:=INT_NAN; c.Y:=0;

  if hp2 = nil then hp1:=nil;
  if cmd = 6 then tlong(opt).b[0]:=4;

  case cmd of
0:  Result:=lp_Vertex_Comp(lp1,n1,lp2,n2,
                           hp1,hp2,opt,
                           rparams[1],    // <= dxy
                           rparams[3],    // <= dz
                           c);

1:  Result:=lp_Rib_Comp(lp1,n1,lp2,n2,rparams[1]);

2,
3:  Result:=lp_Edge_Comp(lp1,n1, lp2,n2,
                         hp1,hp2, rparams,
                         opt,cmd = 2, c,rc);

4:  Result:=lPolygonContainsPolyLine(lp2,n2, lp1,n1);

5:  begin
      k:=lp_Get_Cross(lp1,n1, lp2,n2, @cv,16, nil,nil,false);
      if k > 0 then begin
        c:=cv[0];

        if iparams = nil then
          Result:=k > 0
        else begin
          Result:=k > iparams[0];

          case iparams[1] of
        1:  Result:=k < iparams[0];
        2:  Result:=k = iparams[0];
          end
        end
      end
    end;

6:  Result:=lp_contiguity(lp1,n1, lp2,n2, c);

7:  begin
      Result:=lp_co_ordinationX(lp1,n1, lp2,n2, rparams[0], c);
      if not Result then
      Result:=lp_co_ordinationX(lp2,n2, lp1,n1, rparams[0], c)
    end;

8:  begin
      k:=lp_Get_Cross(lp1,n1, lp2,n2, @cv,16, @i1,@i2,true);
      for i:=0 to k-1 do
      if (i1[i] < 0) or (i2[i] < 0) then begin
         c:=cv[i]; Result:=true; Break
      end
    end;

9:  Result:=lPolygonContainsPolyLine(lp1,n1, lp2,n2);

  end;

  if Assigned(pc) then pc.X:=INT_NAN;
  if cmd = 2 then // примыкание метрическое
  if opt = 1 then c:=lp1[n1] else
  if opt in [0,3] then c:=lp1[0];

  if Assigned(pc) then
  if c.X <> INT_NAN then
  pc^:=c
end;

// lp1 - river; lp2 - horizontal
function Allign_Level_Line(lp1,lp2: PLLine): Boolean;

function rib_Project(const q1,q2, p1,p2: TPoint): Integer;
var
  p3, _q1,_q2: TPoint;
begin
  p3.X:=Round(Long_Dist(p1,p2)); p3.Y:=p1.Y;
  Rotate_lpoint(p1,p2,p3, q1, _q1);
  Rotate_lpoint(p1,p2,p3, q2, _q2);
  Result:=_q2.x - _q1.x
end;

function Back_lp(lp: PLLine; jc: Integer;
                 max: Double; out dist: Double): Integer;
var
  p1,p2: TPoint; len: Double;
begin
  Result:=jc-1; with lp^ do begin
    p2:=Pol[Result]; len:=Long_Dist(p2,Pol[jc]);
    while (len < max) and (Result > 0) do begin
      p1:=p2; Dec(Result); p2:=Pol[Result];
      len:=len+Long_Dist(p1,p2);
    end; dist:=len
  end
end;

function Forw_lp(lp: PLLine; jc: Integer;
                 max: Double; out dist: Double): Integer;
var
  p1,p2: TPoint; len: Double;
begin
  Result:=jc+1; with lp^ do begin
    p2:=Pol[Result]; len:=Long_Dist(p2,Pol[jc]);
    while (len < max) and (Result < N) do begin
      p1:=p2; Inc(Result); p2:=Pol[Result];
      len:=len+Long_Dist(p1,p2);
    end; dist:=len
  end
end;

var
  i,j,_j, j1,j2,jc: Integer; lp_lt,lp_rb: TPoint;
  p0,p1,p2, _p1,_p2, q1,q2,qc, _q1,_q2, c: TPoint;
  d1,d2,d3, r1,r2, dist: Double; dir,tmp: Integer;
begin
  Result:=false;

  dir_Poly(lp1,4*Pi/180);
  dir_Poly(lp2,3*Pi/180);

  if lp1.N > 0 then
  if lp2.N > 0 then begin
    Max_Poly_Bound(@lp2.Pol,lp2.N+1, lp_lt,lp_rb);

    p1:=lp1.Pol[0]; p2:=lp1.Pol[0]; i:=0;
    if PolyLock(lp1) then p1:=lp1.Pol[lp1.N-1];

    while i < lp1.N do begin

      p0:=p1; p1:=p2; Inc(i); p2:=lp1.Pol[i]; j:=0;

      if not Points_Equal(p1,p2) then
      if PortContainsLine(lp_lt,lp_rb, p1,p2) then

      while j < lp2.N do begin
        q1:=lp2.Pol[j]; Inc(j); q2:=lp2.Pol[j];

        if not Points_Equal(q1,q2) then
        if i_Cross(p1,p2, q1,q2, c) then

        if not Points_Equal(q1,c) then
        if not Points_Equal(q2,c) then begin

          _p1:=p1; _p2:=p2;

          if Points_Equal(_p1,c) then
            _p1:=p0
          else
          if Points_Equal(_p2,c) then begin
            if i < lp1.N then _p2:=lp1.Pol[i+1] else
            if PolyLock(lp1) then _p2:=lp1.Pol[1]
          end;

          d1:=Dist_to_Line(q1, _p1,_p2);
          d2:=Dist_to_Line(q2, _p1,_p2);

          jc:=-1; dir:=rib_Project(q1,q2, _p1,_p2);

          if Abs(d1) > 0 then
          if Abs(d2) > 0 then

          if dir <> 0 then
          if (d1 > 0) <> (d2 > 0) then begin

            r1:=Long_Dist(q1,q2); dist:=r1 * 3;
            j1:=j-1; j2:=j; _q1:=q1; r2:=0;

            while (j1 > 0) and (r2 < dist) do begin
              _q2:=_q1; _q1:=lp2.Pol[j1-1];

              d3:=Dist_to_Line(_q1, _p1,_p2);

              if (d3 > 0) = (d1 > 0) then begin
                tmp:=rib_Project(_q1,_q2, _p1,_p2);

                if dir > 0 then begin
                  if tmp < 0 then begin
                    jc:=j1; Break
                  end
                end
                else begin
                  if tmp > 0 then begin
                    jc:=j1; Break
                  end
                end
              end;

              r2:=r2+Long_Dist(_q1,_q2);
              Dec(j1)
            end;

            if jc > 0 then begin
              r1:=r1+r2; j1:=Back_lp(lp2,jc, r1,r2);
              if r2/r1 > 1.5 then j2:=Forw_lp(lp2,jc, r2,r1)
            end
            else begin // right side

              j1:=j-1; j2:=j; _q2:=q2; r2:=0;

              while (j2 < lp2.N) and (r2 < dist) do begin
                _q1:=_q2; _q2:=lp2.Pol[j2+1];

                d3:=Dist_to_Line(_q2, _p1,_p2);

                if (d3 > 0) = (d2 > 0) then begin
                  tmp:=rib_Project(_q1,_q2, _p1,_p2);

                  if dir > 0 then begin
                    if tmp < 0 then begin
                      jc:=j2; Break
                    end
                  end
                  else begin
                    if tmp > 0 then begin
                      jc:=j2; Break
                    end
                  end
                end;

                r2:=r2+Long_Dist(_q1,_q2);
                Inc(j2)
              end;

              if jc > 0 then begin
                r1:=r1+r2; j2:=Forw_lp(lp2,jc, r1,r2);
                if r2/r1 > 1.5 then j1:=Back_lp(lp2,jc, r2,r1)
              end

            end; // right side

            if jc > 0 then begin
              Middle_Point(lp2.Pol[j1],lp2.Pol[j2], qc);
              q1:=lp2.Pol[jc];
              if Project_to_Line(q1 ,_p1,_p2, q2) then

              for _j:=j1+1 to j2-1 do begin
                Rotate_lpoint(qc,q1,q2, lp2.Pol[_j],lp2.Pol[_j]);
                Result:=true
              end
            end

          end
        end
      end;

      if Result then Break
    end
  end

end;

function dist_lp(lp: PLLine; dist: Double): TPoint;
var
  p1,p2: TPoint; i: Integer; rib: Double;
begin
  Result.x:=0; Result.y:=0;

  with lp^ do if N >= 0 then begin

    Result:=Pol[0]; p2:=Pol[0];

    if dist > 0 then
    for i:=1 to N do begin
      p1:=p2; p2:=Pol[i];
      rib:=Long_Dist(p1,p2);
      if rib < dist then begin
        Result:=p2; dist:=dist-rib
      end
      else begin
        Get_Next_Point(p1,p2,dist,Result);
        dist:=0; Break
      end
    end
  end
end;

function Middle_lline(lp1,lp2: PLLine; max_r: Double;
                      dst: TLLineProc): Double;

type
  tdir = (_unknown,_forw,_back);

  XPoint = record
    p: TPoint; r,d: Double;
    i,j: Integer
  end;

  XPoints = array[0..1] of XPoint;

function Plot_lp(lp1,lp2: PLLine; const a,b: XPoints;
                 dst: PLLine; dst_Max: Integer): Integer;
var
  i,n: Integer; d1,d2, k1,k2: Double;
  p1,p2,p: TPoint;
begin
  n:=Max( Abs(a[1].i-a[0].i),
          Abs(b[1].i-b[0].i) );

  n:=Max(1,n);

  k1:=(a[1].d-a[0].d)/n;
  k2:=(b[1].d-b[0].d)/n;

  for i:=0 to n do begin
    d1:=a[0].d + k1 * i;
    d2:=b[0].d + k2 * i;

    p1:=dist_lp(lp1,d1);
    p2:=dist_lp(lp2,d2);

    Middle_point(p1,p2,p);

    LPoly_Continue(dst,dst_Max,p)
  end;

  Result:=dst.N
end;

function prj_line(const p, q1,q2: TPoint;
                  out x: XPoint): Boolean;
var
  lt,rb, q: TPoint;
  d,r, d1,d2: Double;
begin
  Result:=false; FillChar(x,SizeOf(x),0);

  if not Points_Equal(q1,q2) then
  if Project_to_Line(p ,q1,q2, q) then begin

    lt.x:=Min(q1.x,q2.x); rb.x:=Max(q1.x,q2.x);
    lt.y:=Min(q1.y,q2.y); rb.y:=Max(q1.y,q2.y);

    if Points_Equal(p,q) then begin
      if PortContainsPoint(lt,rb, q.x,q.y) then begin
        x.p:=q; x.d:=Long_Dist(q1,q); Result:=true;
      end else
      if Long_Dist(q1,q) > Long_Dist(q2,q) then
        begin x.p:=q2; x.i:=1 end
      else
        begin x.p:=q1; x.i:=-1 end
    end
    else begin
      d:=Dist_to_Line(p, q1,q2);
      if d < 0 then x.j:=-1 else x.j:=1;

      if not PortContainsPoint(lt,rb, q.x,q.y) then

      if Long_Dist(q1,q) > Long_Dist(q2,q) then begin
        q:=q2; x.i:=1
      end
      else begin
        q:=q1; x.i:=-1
      end;

      r:=Long_Dist(p,q);
      if d < 0 then r:=-r;

      x.p:=q; x.d:=Long_Dist(q1,q);
      x.r:=r; Result:=true
    end
  end
end;

function prj_frst(const p: TPoint; lp: PLLine;
                  max_r: Double; plus: Boolean;
                  out x: XPoint): Boolean;
var
  lp_i: Integer; q1,q2: TPoint;
  dist, r: Double; t: XPoint;
  lock: Boolean;
begin
  Result:=false; FillChar(x,SizeOf(x),0);

  with lp^ do

  if N > 0 then begin
    lock:=PolyLock(lp);

    dist:=0; q2:=Pol[0];
    for lp_i:=1 to N do begin
      q1:=q2; q2:=Pol[lp_i];

      if not Points_Equal(q1,q2) then
      if prj_line(p, q1,q2, t) then

      if Points_Equal(p,t.p) then begin
        x.p:=t.p; x.i:=lp_i; x.d:=dist + t.d;
        Result:=true; Break
      end else

      if plus = (t.j = 1) then begin

        if lp_i > 1 then
        if t.i = -1 then t.i:=-2;

        if Abs(t.i) <= 1 then begin
          r:=Abs(t.r); if r <= max_r then

          if not Result or (r < x.r) then begin
            x.p:=t.p; x.i:=lp_i; x.r:=r;
            x.d:=dist + t.d; Result:=true
          end
        end
      end;

      dist:=dist+Long_Dist(q1,q2)
    end
  end
end;

function prj_run(const a: XPoints; lp: PLLine;
                 max_r: Double; plus: Boolean;
                 fwd: Boolean; var b: XPoints): Boolean;
var
  i,lp_i: Integer; lock,rc: Boolean;
  p1,p2, q1,q2: TPoint; x: XPoint;
  dist,rib, r,x_d: Double;
begin
  Result:=false;

  p1:=a[0].p; p2:=a[1].p;
  lock:=PolyLock(lp); lp_i:=b[0].i;

  if lp_i > 0 then
  if lp_i <= lp.N then begin

    x_d:=Long_Dist(lp.Pol[lp_i-1],b[0].p);
    dist:=b[0].d - x_d;

    for i:=1 to lp.N do begin
      q1:=lp.Pol[lp_i-1]; q2:=lp.Pol[lp_i];
      rib:=Long_Dist(q1,q2);

      if not fwd and (i > 1) then begin
        dist:=dist - rib; x_d:=rib
      end;

      if not Points_Equal(q1,q2) then

      if i_Cross(p1,p2, q1,q2, x.p) then
        Break
      else begin
        rc:=prj_line(p2, q1,q2, x);

        if x.i = 0 then begin
          if not rc then Break;

          if (x.d >= x_d) and fwd then else
          if (x.d <= x_d) and not fwd then
          else Break;

          if plus <> (x.j = 1) then Break;

          r:=Abs(x.r); if r > max_r then
            Break
          else begin
            x.d:=dist + x.d; x.i:=lp_i;
            b[1]:=x; Result:=true; Break
          end
        end else
        if (x.i = 1) <> fwd then
          Break
        else begin
          r:=Long_Dist(x.p, p2);
          if r > max_r then Break;
          x.d:=dist + x.d; x.i:=lp_i;
          b[1]:=x; Result:=true
        end
      end;

      if fwd then begin
        Inc(lp_i); if lp_i > lp.N then
        if lock then lp_i:=1 else Break;
        dist:=dist+rib; x_d:=0
      end
      else begin
        Dec(lp_i); if lp_i <= 0 then
        if lock then lp_i:=lp.N else Break;
      end
    end
  end
end;

function prj_next(const a: XPoints; lp: PLLine;
                  max_r: Double; plus: Boolean;
                  dir: tdir; var b: XPoints): Boolean;
begin
  Result:=false; b[1]:=b[0];

  if b[0].i > 0 then
  if b[0].i <= lp.N then

  case dir of
_unknown:
    if prj_run(a,lp, max_r,plus, true,b) then
      Result:=true
    else
    if prj_run(a,lp, max_r,plus, false,b) then
      Result:=true;
_forw:
    Result:=prj_run(a,lp, max_r,plus, true,b);
_back:
    Result:=prj_run(a,lp, max_r,plus, false,b);
  end
end;

function run_lp(lp1,lp2: PLLine; dst: TLLineProc;
                max_r: Double; plus: Boolean): Double;
var
  lp_i,buf_max: Integer; a,b: XPoints;
  dir: tdir; buf: PLLine;
begin
  Result:=0; dir:=_unknown;

  buf:=nil; buf_max:=Max(lp1.N,lp2.N);
  buf_max:=Max(buf_max,16000-1);

  if Assigned(dst) then
  buf:=Alloc_LLine(buf_max+1);

  if Assigned(buf) then buf.N:=-1;

  FillChar(a,SizeOf(a),0);
  FillChar(b,SizeOf(b),0);

  a[1].p:=lp1.Pol[0]; a[1].d:=0;

  for lp_i:=0 to lp1.N do begin
    a[0]:=a[1]; a[1].p:=lp1.Pol[lp_i];
    a[1].d:=a[0].d + Long_Dist(a[0].p,a[1].p);
    a[1].i:=lp_i;

    if b[0].i = 0 then
      prj_frst(a[1].p,lp2, max_r,plus, b[0])
    else

    if not Points_Equal(a[0].p,a[1].p) then

    if prj_next(a,lp2, max_r,plus,dir, b) then begin
      if Assigned(buf) then
      Plot_lp(lp1,lp2, a,b, buf,buf_max);

      if dir = _unknown then
      if b[1].d > b[0].d then dir:=_forw else
      if b[1].d < b[0].d then dir:=_back;

      Result:=Result + a[1].d - a[0].d;
      b[0]:=b[1];
    end

    else begin
      if Assigned(buf) then begin
        if buf.N > 0 then dst(buf);
        buf.N:=-1
      end;

      FillChar(b,SizeOf(b),0);
      dir:=_unknown;
    end
  end;

  if Assigned(buf) then
  if buf.N > 0 then dst(buf);

  xFreePtr(buf)
end;

var
  d1,d2: Double; plus,swap: Boolean;
begin
  Result:=-1;

  if lp1.N > 0 then
  if lp2.N > 0 then begin

    swap:=false;
    d1:=Long_Dist(lp1.Pol[0],lp2.Pol[0]);
    d2:=Long_Dist(lp1.Pol[0],lp2.Pol[lp2.N]);

    if (d2 < d1) and (d2 <= max_r) then
    begin Swap_Poly(lp2); swap:=true end;

    plus:=run_lp(lp1,lp2, nil, max_r,true) >=
          run_lp(lp1,lp2, nil, max_r,false);

    Result:=run_lp(lp1,lp2, dst, max_r,plus);
    if swap then Swap_Poly(lp2)
  end
end;

function lp_pickup_p(lp: PLLine;
                     const p: TPoint;
                     out lp_i: Integer): Boolean;
var
  i: int; a,b: TPoint;
begin
  Result:=false;

  if lp.N >= 0 then

  if Points_Equal(p,b) then begin
    lp_i:=0; Result:=true
  end else

  for i:=1 to lp.N do begin
    a:=b; b:=lp.Pol[i];

    if Points_Equal(p,b) then begin
      lp_i:=i; Result:=true; Break
    end else
    if LineContainsPoint(a,b, p) then begin
      lp_i:=-i; Result:=true; Break
    end
  end
end;

function lp_xmov(lp,cut: PLLine;
                 out i1,i2,i3: Integer;
                 out p3: TPoint): Boolean;

function lp_next(lp: PLLine; const p1,p2: TPoint;
                 i1: Integer; forw,lock: Boolean;
                 out i2: Integer): Boolean;
var
  i: Integer; q1,q2: TPoint;
begin
  Result:=false; i2:=0; q1:=p1;

  if forw then begin
    if i1 < 0 then begin
      i1:=Abs(i1); Result:=true
    end else
    if i1 < lp.N then begin
      Inc(i1); Result:=true
    end
  end
  else begin
    if i1 < 0 then begin
      i1:=Abs(i1)-1; Result:=true
    end else
    if i1 > 0 then begin
      Dec(i1); Result:=true
    end else
    if lock then begin
      i1:=lp.N-1; Result:=true
    end
  end;

  if Result then begin
    Result:=false;

    i:=i1; repeat
      q2:=lp.Pol[i];

      if Points_Equal(p2,q2) then begin
        if forw and (i = lp.N) then
        if lock then i:=0; i2:=i;
        Result:=true; Break
      end else
      if LineContainsPoint(q1,q2, p2) then begin
        if not forw then Inc(i);
        i2:=-i; Result:=true; Break
      end else
      if LineContainsPoint(q1,p2, q2) then begin
        q1:=q2; if forw then begin
          Inc(i); if i > lp.N then
          if lock then i:=1 else Break
        end
        else begin
          Dec(i); if i < 0 then
          if lock then i:=lp.N-1 else Break
        end
      end
      else Break

    until i = i1
  end
end;

function lp_xcomp(lp,cut: PLLine;
                  i1: Integer; forw: Boolean;
                  out i2,i3: Integer;
                  out p3: TPoint): Boolean;
var
  i: Integer; lock: Boolean;
  p1,p2: TPoint;
begin
  Result:=true; i2:=i1; i3:=i1;

  lock:=PolyLock(lp); p2:=cut.Pol[0]; p3:=p2;

  for i:=1 to cut.N do begin
    p1:=p2; p2:=cut.Pol[i];
    if lp_next(lp, p1,p2, i1, forw,lock, i1) then begin
      i2:=i1; if i < cut.N then begin
        i3:=i1; p3:=p2
      end
    end
    else begin
      Result:=false; Break
    end
  end
end;

var
  i: int; p, a,b: TPoint;
begin
  Result:=false;

  if lp.N > 0 then
  if cut.N > 0 then begin

    p:=cut.Pol[0]; b:=lp.Pol[0];

    if Points_Equal(p,b) then begin
      i1:=0; Result:=true
    end else

    for i:=1 to lp.N do begin
      a:=b; b:=lp.Pol[i];

      if Points_Equal(p,b) then begin
        i1:=i; Result:=true; Break
      end else
      if LineContainsPoint(a,b, p) then begin
        i1:=-i; Result:=true; Break
      end
    end;

    if Result then begin
      Result:=false;

      if lp_xcomp(lp,cut, i1,true, i2,i3, p3) then
        Result:=true
      else
      if lp_xcomp(lp,cut, i1,false, i2,i3, p3) then
        Result:=true
    end
  end
end;

function House_to_Road(const a,b: TPoint; lp: PLLine;
                       out a_,b_: TPoint): Boolean;

function x_swap(var p2: TPoint; const b,p: TPoint): Boolean;
begin
  Result:=false;
  if Long_Dist(b,p) < Long_Dist(b,p2) then
  begin p2:=p; Result:=true end
end;

var
  i: Integer; q1,q2,q: TPoint;
  r,t: Double; p1,p2: TPoint;

begin
  Result:=false;

  with lp^ do
  if N > 0 then begin
    q2:=Pol[0];

    for i:=1 to N do begin
      q1:=q2; q2:=Pol[i];

      if not Points_Equal(q1,q2) then
      if Plot_to_Line(a ,q1,q2, q) then begin

        t:=Long_Dist(a,q);
        if not Result or (t < r) then begin
          p1:=q; r:=t; p2:=q2;

          if Points_Equal(q1,q) and (i > 1) then
          x_swap(p2, b,Pol[i-2]);

          if Points_Equal(q2,q) and (i < N) then
          x_swap(p2, b,Pol[i+1]);

          if not Points_Equal(p1,p2) then begin
            a_:=p1; b_:=p2;
            Result:=true
          end
        end
      end
    end;
  end
end;

function xCrossline(lp: PLLine; const a,b: TPoint;
                    out p: TPoint): Boolean;
var
  i: Integer; t: tgauss;
  p1,p2, lt,rb,_lt,_rb: TPoint;
begin
  Result:=false; p:=a;

  with lp^ do
  if N > 0 then begin

    _lt.x:=Min(a.x,b.x); _lt.y:=Min(a.y,b.y);
    _rb.x:=Max(a.x,b.x); _rb.y:=Max(a.y,b.y);

    p2:=Pol[0];
    for i:=1 to lp.N do begin
      p1:=p2; p2:=Pol[i];

      lt.x:=Min(p1.x,p2.x); lt.y:=Min(p1.y,p2.y);
      rb.x:=Max(p1.x,p2.x); rb.y:=Max(p1.y,p2.y);

      if (lt.x < _rb.x) and (rb.x > _lt.x) then
      if (lt.y < _rb.y) and (rb.y > _lt.y) then

      if LL2(p1.x,p1.y,p2.x,p2.y, a.x,a.y,b.x,b.y, 0,1, t) then
      begin
        p.x:=Round(t.x); p.y:=Round(t.y);
        Result:=true; Break
      end
    end
  end
end;

function xClipPolyLine(clip,lp: PLLine): Boolean;
var
  i,o: Integer; p1,p2,p,q1,q2: TPoint;
begin
  Result:=false;

  with lp^ do
  if N > 0 then begin
    p2:=Pol[0]; o:=-1;
    if xPolygonContainsPoint(clip,p2) >= 0 then
    o:=0;

    for i:=1 to lp.N do begin
      p1:=p2; p2:=Pol[i];

      if xPolygonContainsPoint(clip,p2) >= 0 then begin
        if o < 0 then
        if xCrossLine(Clip, p1,p2, p) then
        begin Pol[0]:=p; o:=0 end;

        Inc(o); Pol[o]:=p2
      end
      else begin
        if o >= 0 then begin
          if xCrossLine(Clip, p1,p2, p) then
          begin Inc(o); Pol[o]:=p end;
        end else
        if ClipLine(Clip, p1,p2, q1,q2) then begin
          Pol[0]:=q1; Pol[1]:=q2; o:=1
        end;

        if o >= 0 then Break
      end
    end;

    lp.N:=o; Result:=o > 0
  end
end;

function xClipPolyLine1(dest: TPolyList;
                        clip,lp: PLLine): Integer;
var
  i,o: Integer; p1,p2,p,q1,q2: TPoint;
begin
  dest.Clear;

  with lp^ do
  if N > 0 then begin p2:=Pol[0]; o:=-1;
    if xPolygonContainsPoint(clip,p2) >= 0 then begin
      dest.next_point(p2,nil); o:=0
    end;

    for i:=1 to lp.N do begin
      p1:=p2; p2:=Pol[i];

      if xPolygonContainsPoint(clip,p2) >= 0 then begin
        if o < 0 then
        if xCrossLine(Clip, p1,p2, p) then begin
          o:=0; dest.next_point(p,nil);
        end;

        Inc(o); dest.next_point(p2,nil)
      end
      else begin
        if o >= 0 then begin
          if xCrossLine(Clip, p1,p2, p) then
          dest.next_point(p,nil)
        end else
        if ClipLine(Clip, p1,p2, q1,q2) then begin
          dest.next_point(q1,nil);
          dest.next_point(q2,nil)
        end;

        dest.End_contour; o:=-1
      end
    end;

    dest.End_contour
  end;

  Result:=dest.PartCount
end;

function align_wire(lp: PLLine): Boolean;
var
  a,b,p,q: TPoint; i: Integer;
begin
  Result:=true;
  if lp.N > 1 then begin
    a:=lp.Pol[0]; b:=lp.Pol[lp.N];

    Result:=true;
    for i:=1 to lp.N-1 do begin
      p:=lp.Pol[i];
      if Plot_to_Line(p ,a,b, q) then
        lp.Pol[i]:=q
      else begin
        Result:=false; Break
      end
    end
  end
end;

function xArterPoly(lp,lp1,lp2: PLLine): Integer;
var
  i1,i2: Integer; r1,r2: Double;
  p1,p2,q1,q2,p: TPoint; pc: TGauss;
begin
  lp.N:=-1;

  if lp1.N > 0 then
  if lp2.N > 0 then begin
    p1:=lp1.Pol[0]; p2:=lp1.Pol[1];
    q1:=lp2.Pol[0]; q2:=lp2.Pol[1];

    Middle_Point(p1,q1,p);
    lp.N:=0; lp.Pol[0]:=p;

    i1:=1; i2:=1;

    while (i1 < lp1.N) or (i2 < lp2.N) do begin

      r1:=Long_dist(p,p2);
      r2:=Long_dist(p,q2);
      if i2 = lp2.N then r2:=r1+100;
      if i1 = lp1.N then r1:=r2+100;

      if r2 < r1 then begin
        if prj_to_Line(q2,p1,p2,pc) then begin
          p.X:=Round(q2.X/2 + pc.x/2);
          p.Y:=Round(q2.Y/2 + pc.y/2);
          Inc(lp.N); lp.Pol[lp.N]:=p;
        end;

        Inc(i2); q1:=q2; q2:=lp2.Pol[i2];
      end
      else begin
        if prj_to_Line(p2,q1,q2,pc) then begin
          p.X:=Round(p2.X/2 + pc.x/2);
          p.Y:=Round(p2.Y/2 + pc.y/2);
          Inc(lp.N); lp.Pol[lp.N]:=p;
        end;

        Inc(i1); p1:=p2; p2:=lp1.Pol[i1];
      end
    end;

    Middle_Point(p2,q2,p);
    Inc(lp.N); lp.Pol[lp.N]:=p;
  end;

  Result:=lp.N
end;

function lp_Pickup_z(lp: PLLine; hp: PIntegers;
                     const p: TPoint; out pz: Integer): Boolean;
var
  i,dx,dy,z1,dz: Integer;
  p1,p2,a,b,pc: TPoint;
  dist,tmp: Double;
begin
  Result:=false;

  p2:=lp.Pol[0];

  dist:=Long_Dist(p,p2);
  pz:=hp[0]; Result:=true;

  for i:=1 to lp.N do begin
    p1:=p2; p2:=lp.Pol[i];

    tmp:=Long_Dist(p,p2);
    if tmp < dist then begin
      pz:=hp[i]; dist:=tmp; Result:=true;
    end;

    dx:=p2.X-p1.X; dy:=p2.Y-p1.Y;

    if (dx <> 0) or (dy <> 0) then
    if Plot_to_Line(p ,p1,p2,pc) then begin

      tmp:=Long_Dist(p,pc);
      if tmp < dist then begin
        z1:=hp[i-1]; dz:=hp[i]-z1;
        pz:=z1 + Round(dz*Long_Dist(p1,pc)/Hypot(dx,dy));
        dist:=tmp; Result:=true;
      end
    end
  end
end;

function lp_sharp(lp: PLPoly; lp_n,fi: int): int;
var
  i,ax,ay,bx,by: int; minCos,cs,cs1,l1,l2: Double;
begin
  Result:=-1;

  if lp_n > 1 then begin

    minCos:=Cos(fi/180*Pi); cs1:=minCos;

    if LPoly_Lock(lp,lp_N) then begin
      bx:=lp[0].X-lp[lp_N-1].X;
      by:=lp[0].Y-lp[lp_N-1].Y; i:=0
    end
    else begin
      bx:=lp[1].X-lp[0].X;
      by:=lp[1].Y-lp[0].Y; i:=1
    end;

    l2:=Hypot(bx,by);

    while i < lp_n do begin
      ax:=bx; ay:=by; l1:=l2;
      bx:=lp[i+1].X-lp[i].X;
      by:=lp[i+1].Y-lp[i].Y;
      l2:=Hypot(bx,by);

      if (l1 > 0.1) and (l2 > 0.1) then begin
        cs:=(Int64(ax)*bx + Int64(ay)*by) / (l1*l2);
        if cs < cs1 then begin
          Result:=i; cs1:=cs;
          if minCos < 0 then Break
        end
      end;

      Inc(i)
    end
  end
end;

function lp_off_sharp(lp: PLPoly;
                      hp: PIntegers;
                      lp_n,fi: int): int;
var
  i,j,ax,ay,bx,by,cx,cy,dx,dy,_n: int;
  minCos,minCos1,cs,cs1,l1,l2,l3,l4: Double;
  lock,b_on: longbool;
begin
  Result:=lp_n;

  if lp_n > 1 then begin

    minCos:=Cos(fi/180*Pi);
    minCos1:=Cos(170/180*Pi);

    lock:=LPoly_Lock(lp,lp_N);
    i:=1; if lock then i:=0;

    b_on:=false;

    while i < lp_n do begin

      if b_on then begin
        ax:=bx; ay:=by; l1:=l2;
      end
      else begin
        j:=i-1; if j < 0 then j:=lp_N-1;
        ax:=lp[i].X-lp[j].X;
        ay:=lp[i].Y-lp[j].Y;
        l1:=Hypot(ax,ay)
      end;

      bx:=lp[i+1].X-lp[i].X;
      by:=lp[i+1].Y-lp[i].Y;
      l2:=Hypot(bx,by); b_on:=true;

      _n:=lp_n;

      if (l1 > 0.1) and (l2 > 0.1) then begin
        cs:=(Int64(ax)*bx + Int64(ay)*by) / (l1*l2);
        if cs < minCos then
        if not lock or (lp_n > 3) then

        if not lock and (i = 1) then begin
          if lp_n = 2 then
            Dec(lp_n)
          else begin
            Dec(lp_n); Dec(i);
            for j:=i to lp_n do
            lp[j]:=lp[j+1];
          end;

          b_on:=false;
        end else

        if not lock and (i+1 = lp_n) then
          Dec(lp_n)
        else
        if cs < minCos1 then begin
          b_on:=false; Dec(lp_n);
          for j:=i to lp_n do lp[j]:=lp[j+1];
          if lock then lp[lp_n]:=lp[0];
        end
        else begin
          j:=i+2; if j > lp_n then j:=1;

          cx:=lp[j].X-lp[i+1].X;
          cy:=lp[j].Y-lp[i+1].Y;
          l3:=Hypot(cx,cy);

          if l3 > 0.1 then begin
            cs:=(Int64(ax)*cx + Int64(ay)*cy) / (l1*l3);

            if cs > 0.7 then begin
              b_on:=false; Dec(lp_n);
              if i+1 <= lp_n then
                for j:=i+1 to lp_n do
                lp[j]:=lp[j+1]
              else begin
                for j:=0 to lp_n do lp[j]:=lp[j+1];
                lp[lp_n]:=lp[0]
              end
            end else
            if i > 1 then begin
              dx:=lp[i-1].X-lp[i-2].X;
              dy:=lp[i-1].Y-lp[i-2].Y;

              l4:=Hypot(dx,dy);
              if l4 > 0.1 then begin
                cs:=(Int64(dx)*cx + Int64(dy)*cy) / (l4*l3);

                if cs > 0.8 then begin
                  b_on:=false; Dec(lp_n);
                  for j:=i to lp_n do
                  lp[j]:=lp[j+1]
                end
                else begin
                  cs:=(Int64(dx)*ax + Int64(dy)*ay) / (l4*l1);
                  cs1:=(Int64(bx)*cx + Int64(by)*cy) / (l2*l3);

                  if cs > 0.6 then
                  if cs1 > 0.6 then begin
                    b_on:=false; Dec(lp_n);
                    for j:=i to lp_n do
                    lp[j]:=lp[j+1]
                  end
                end
              end
            end
          end
        end
      end;

      Inc(i)
    end
  end;

  Result:=lp_n;
end;

end.