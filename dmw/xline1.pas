unit xline1; interface

uses
  Math,otypes,xline;

procedure Blank_rect(R: PLPoly; k: Float);

function Get_romb_angle(const tr: Real3x3; w,h: Double): Double;

implementation

procedure Blank_rect(R: PLPoly; k: Float);
var
  v,n: TGauss; l: Double;
begin
  v.x:=R[1].X-R[0].X;
  v.y:=R[1].Y-R[0].Y;

  l:=Hypot(v.x,v.y);
  if l >= 0.01 then begin
    v.x:=v.x/l; v.y:=v.y/l;
    n.x:=-v.y;  n.y:=v.x; l:=l/16;

    Inc(R[0].X,Round((-n.x-v.x)*l));
    Inc(R[0].Y,Round((-n.y-v.y)*l));

    Inc(R[1].X,Round((-n.x+v.x)*l));
    Inc(R[1].Y,Round((-n.y+v.y)*l));

    Inc(R[2].X,Round((+n.x+v.x)*l));
    Inc(R[2].Y,Round((+n.y+v.y)*l));

    Inc(R[3].X,Round((+n.x-v.x)*l));
    Inc(R[3].Y,Round((+n.y-v.y)*l));

    R[4]:=R[0]
  end
end;

function Get_romb_angle(const tr: Real3x3; w,h: Double): Double;
var
  p1,p2,p3: TGauss;
begin
  p1:=Transit_3x3(0,0,tr);
  p2:=Transit_3x3(0,w,tr);
  p3:=Transit_3x3(0,h,tr);
  Result:=sAngle(p1.x,p1.y,p2.x,p2.y,p3.x,p3.y)
end;

end.