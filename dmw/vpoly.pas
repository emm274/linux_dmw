unit VPoly; interface

uses
  otypes;
  
procedure Max_VPoly_Bound(lp: PVPoly; n: Integer; out a,b: VPoint);

function VPolyIsPack(vp: PVPoly; vn: Integer): Boolean;
function VPolyPack(vp: PVPoly; vn: Integer): Integer;
procedure VPolyUnpack(vp: PVPoly; vn: Integer; bp: PBytes);

implementation

uses
  Math;
  
procedure Max_VPoly_Bound(lp: PVPoly; n: Integer; out a,b: VPoint);
var
  i: Integer; v1,v2: VPoint;
begin
  if n <= 0 then begin
    a:=_VPoint(0,0,0); b:=a
  end
  else begin
    v1:=lp[0]; v2:=v1;
    for i:=1 to n do begin
      with lp[0] do begin
        v1.x:=Min(v1.x,x); v2.x:=Max(v2.x,x);
        v1.y:=Min(v1.y,y); v2.y:=Max(v2.y,y);
        v1.z:=Min(v1.z,z); v2.z:=Max(v2.z,z);
      end; lp:=@lp[1]
    end;

    a:=v1; b:=v2
  end
end;

function VPolyIsPack(vp: PVPoly; vn: Integer): Boolean;
var
  i: integer; v1,v2: VPoint;
begin
  Result:=false;

  if vn >= 8 then begin

    Result:=true; v2:=vp[0];
    for i:=1 to vn-1 do begin
      v1:=v2; v2:=vp[i];
      if (Abs(v2.x-v1.x) >= $7FFF)
      or (Abs(v2.y-v1.y) >= $7FFF)
      or (Abs(v2.z-v1.z) >= $7FFF) then
      begin Result:=false; Break end
    end
  end
end;

function VPolyPack(vp: PVPoly; vn: Integer): Integer;
var
  i: Integer; v1,v2: VPoint; di: PSmallInts;
begin
  Result:=0;

  if vn > 0 then begin

    v2:=vp[0]; Result:=Sizeof(v2);

    di:=@vp[1];
    for i:=1 to vn-1 do begin
      v1:=v2; v2:=vp[i];
      di[0]:=v2.x-v1.x; di:=@di[1];
      di[0]:=v2.y-v1.y; di:=@di[1];
      di[0]:=v2.z-v1.z; di:=@di[1];
      Inc(Result,2*3)
    end
  end
end;

procedure VPolyUnpack(vp: PVPoly; vn: Integer; bp: PBytes);
var
  i: Integer; v: VPoint; di: PSmallInts;
begin
  if vn > 0 then begin
    v:=PVPoint(bp)^; vp[0]:=v;

    if vn > 1 then begin
      di:=@bp[12];
      for i:=1 to vn-1 do begin
        Inc(v.x,di[0]); di:=@di[1];
        Inc(v.y,di[0]); di:=@di[1];
        Inc(v.z,di[0]); di:=@di[1];
        vp[i]:=v;
      end
    end
  end
end;

end.
