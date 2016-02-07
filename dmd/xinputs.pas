unit xinputs;

{$mode delphi}

interface

uses
  Classes, Controls, SysUtils, otypes;

function ControlContainsCursor(c: TControl; lp: PPoint): Boolean;

implementation

uses
  LCLIntf;

function ControlContainsCursor(c: TControl; lp: PPoint): Boolean;
var
  p: TPoint;
begin
  Result:=false;

  if c.Visible then
  if GetCursorPos(p) then begin
    p:=c.ScreenToClient(p);

    if (p.X >= 0) and (p.X < c.Width) then
    if (p.Y >= 0) and (p.Y < c.Height) then begin
      if Assigned(lp) then lp^:=p;
      Result:=true
    end
  end
end;

end.

