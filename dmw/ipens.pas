unit ipens; interface

{$mode delphi}

uses
  Classes,Math, otypes, idib;

procedure iDrawLine(dc: ICanvas; x1,y1,x2,y2: Integer);

procedure iDisp_thin(dc: ICanvas; lp: PLPoly; n,w,cl: Integer);
procedure iDisp_frame(dc: ICanvas; lp: PLPoly; n: Integer);

procedure iRectangle(dc: ICanvas; x1,y1,x2,y2, cl: integer);

procedure iLinkMarker(dc: ICanvas; x,y,d,cl: integer);

procedure iDrawArrow(dc: ICanvas; const a,b: TPoint; r: Integer);
procedure iDrawPunkt(dc: ICanvas; x1,x2,y1,y2, cl: integer);

implementation

uses
  Graphics,xline;

procedure iDrawLine(dc: ICanvas; x1,y1,x2,y2: Integer);
begin
  with dc do begin
    MoveTo(x1,y1); LineTo(x2,y2)
  end
end;

procedure iDisp_thin(dc: ICanvas; lp: PLPoly; n,w,cl: Integer);
var
  i: Integer; R: TRect;
  lt,rb, a,b, p1,p2: TPoint;
begin
  if n > 0 then
  if dc.GetClipRect(R) then begin

    lt:=R.TopLeft; rb:=R.BottomRight;

    if cl <> clNone then
    dc.SetPenColor(cl);

    p2:=lp[0]; for i:=1 to n do begin
      p1:=p2; p2:=lp[i];
      if xClip_Line(lt,rb, p1,p2, a,b) then begin
        dc.MoveTo(a.X,a.Y);
        dc.LineTo(b.X,b.Y);
      end
    end
  end
end;

procedure iDisp_frame(dc: ICanvas; lp: PLPoly; n: Integer);
begin
  if n > 0 then begin
    iDisp_thin(dc,lp,n,3,clLtGray);
    iDisp_thin(dc,lp,n,1,clYellow);
  end
end;

procedure iRectangle(dc: ICanvas; x1,y1,x2,y2,cl: integer);
begin
  with dc do begin
    if cl <> clNone then dc.SetPenColor(cl);
    MoveTo(x1,y1); LineTo(x2,y1); LineTo(x2,y2);
    LineTo(x1,y2); LineTo(x1,y1)
  end
end;

procedure iLinkMarker(dc: ICanvas; x,y,d,cl: integer);
begin
  iRectangle(dc,x-d+1,y-d+1,x+d-1,y+d-1,clBlack);
  iRectangle(dc,x-d  ,y-d  ,x+d  ,y+d  ,cl);
  iRectangle(dc,x-d-1,y-d-1,x+d+1,y+d+1,clBlack)
end;

procedure iDrawArrow(dc: ICanvas; const a,b: TPoint; r: Integer);
var
  fi,df: Double; p1,p2: TPoint;
begin
  fi:=ArcTan2(b.Y-a.Y,b.X-a.X); df:=4/5*Pi;

  p1:=prj_LPoint(b,r,fi-df);
  p2:=prj_LPoint(b,r,fi+df);

  dc.MoveTo(p1.X,p1.Y);
  dc.LineTo(b.X,b.Y);
  dc.LineTo(p2.X,p2.Y);
end;

procedure iDrawPunkt(dc: ICanvas; x1,x2,y1,y2, cl: integer);

procedure xDraw(dc: ICanvas; x1,x2,y1,y2, cl: integer);
var
  p: TOrient;
begin
  p[0].x:=x1; p[0].y:=y2; p[1].x:=x2; p[1].y:=y2;
  p[2].x:=Round(x1/2 + x2/2); p[2].y:=y1; p[3]:=p[0];
  dc.SetPen(0,1,cl,0); dc.PolyLine(@p,4);
end;

begin
  xDraw(dc,x1,x2,y1,y2,clBlack);
  xDraw(dc,x1+4,x2-4,y1+4,y2-2,clBlack);
  xDraw(dc,x1+2,x2-2,y1+2,y2-1,cl);
end;

end.

