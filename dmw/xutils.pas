unit xutils; interface

uses
  Classes,otypes;

function GetTickCount: int64;

procedure time_beg(var dt: Integer);
procedure time_end(var dt: Integer);

procedure stat_inc(var P: TPoint; t: int);
procedure stat_dt(var P: TPoint; dt: int);

implementation

uses
  lazutf8sysutils;

function GetTickCount: int64;
begin
  Result:=lazutf8sysutils.GetTickCount64;
end;

procedure time_beg(var dt: Integer);
begin
  Dec(dt,GetTickCount)
end;

procedure time_end(var dt: Integer);
begin
  Inc(dt,GetTickCount)
end;

procedure stat_inc(var P: TPoint; t: int);
begin
  Inc(P.X,GetTickCount-t); Inc(P.Y)
end;

procedure stat_dt(var P: TPoint; dt: int);
begin
  Inc(P.X,dt); Inc(P.Y)
end;

end.

