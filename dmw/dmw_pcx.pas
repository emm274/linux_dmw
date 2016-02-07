unit dmw_pcx; interface

uses
  use_lnk;

function tif_Mirror_dm(dm,tif,obj: PChar): Boolean;

function tif_lnk_dm(dm,tif,obj: PChar;
                    lnk: ILink2; wgs: Boolean): Boolean;

implementation

uses
  Math,
  otypes,xpoly,xy,
  xbl_use,dmw_lib;

function tif_lnk_dm(dm,tif,obj: PChar;
                    lnk: ILink2; wgs: Boolean): Boolean;
var
  lib: tdm_lib;
  i,ed,w,h: int; s: tsys;
  l: LOrient; g: GOrient;
  lt,rb: TGauss; dr,dx,dy: Double;
begin
  Result:=false;

  w:=lnk.ImageWidth;
  h:=lnk.ImageHeight;

  if (w > 0) and (h > 0) then begin

    s:=sys_nil; dr:=0;
    lt:=_Gauss(0,0); rb:=_Gauss(w,h);
    ed:=10;

    if lnk.GetCount > 0 then begin
      Port_to_LPoly(0,0,w,h,@l);

      lnk.GetSys(s); for i:=0 to 3 do
      s.pps:=lnk.l_to_r(l[i].X,l[i].Y,g[i].x,g[i].y);

      Max_Gauss_Bound(@g,4,lt,rb);
      dr:=Max(rb.x-lt.x,rb.y-lt.y)/8;

      dr:=Max(dr,1/60/180*Pi);

      ed:=100; if s.pps = 1 then
      if dr*180/Pi > 6 then ed:=10 else
      if dr*180/Pi > 3 then ed:=1;

      if s.prj = prj_geo then begin
        s.prj:=3; s.b1:=lt.x
      end;

      if wgs then begin
        s.elp:=9; s.dat:=nil_Datum7
      end
    end;

    lib:=tdm_lib.Create;
    try
      Result:=lib.dm_New1(dm,obj, @s,
                          lt.x-dr,lt.y-dr,
                          rb.x+dr,rb.y+dr, ed)
    finally
      lib.Free
    end
  end
end;

function tif_Mirror_dm(dm,tif,obj: PChar): Boolean;
var
  lnk: ILink2;
begin
  Result:=false;
  if GetLink2Intf(ILink2,lnk) = S_OK then begin
    lnk.Open(tif);
    Result:=tif_lnk_dm(dm,tif,obj,lnk,false);
  end; lnk:=nil;
end;

end.