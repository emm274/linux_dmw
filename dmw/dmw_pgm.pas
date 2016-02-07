unit dmw_pgm; interface

uses
  Classes,
  otypes,vbanks,xdib;

type
  Pgm_Hdr = record cx,cy,cw,ch: SmallInt end;

  TPgmBank = class(TIndBank)
    up: Integer; hdr: Pgm_Hdr; sign: tbytes;
    function Read_Pgm(s: Integer): Integer;

    function Draw_Pgm(dc: xCanVas; k: float;
                      x,y,bc,cl,s: int): Boolean;
  end;

implementation

function tPgmBank.Read_Pgm(s: Integer): Integer;
var
  ind: longint; len: integer;
begin
  if (Loc.Pos = 0) or (s <> up) then begin

    if Ind_Load(s) > 0 then begin up:=s;
      ind:=vm_Load(Loc.Ind+4,hdr,SizeOf(hdr));
      len:=vm_BufLen(Loc.Ind)-SizeOf(hdr);
      FillChar(sign,SizeOf(sign),0);
      if len <= SizeOf(sign) then
      vm_Load(ind,sign,len)
    end

  end; Result:=Loc.Ind
end;

function TPgmBank.Draw_Pgm(dc: xCanVas; k: float;
                           x,y,bc,cl,s: int): Boolean;
var
  i,ix,ind, b,c,d: integer; R: TRect;
begin
  Result:=false;

  if Read_Pgm(s) > 0 then begin

    ind:=0; d:=Round(k);
    Dec(x,hdr.cx*d); Dec(y,hdr.cy*d);

    for i:=1 to hdr.ch do begin
      ix:=x; while true do begin
        if ind >= SizeOf(sign) then Exit;
        b:=sign[ind]; Inc(ind);
        if b = $FF then Break;

        Inc(ix,b); b:=sign[ind]; Inc(ind);
        if b = 0 then Exit;

        while b > 0 do begin
          c:=cl; if c = 0 then begin
            c:=sign[ind]; if c = bc then
            c:=c xor $F
          end;

          if d = 1 then
            dc.SetPixel(ix,y,c)
          else begin
            dc.xBrush(0,c);
            R:=Rect(ix,y,ix+d-1,y+d-1);
            dc.FillRect(@R)
          end;

          Inc(ix,d); Inc(ind); Dec(b)
        end
      end;

      Inc(y,d)
    end;

    Result:=true
  end
end;

end.