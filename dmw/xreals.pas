unit xreals; interface

uses
  Math,otypes;

function r4_min_max(fp: PFloats; n: Integer;
                    v0: Float; out v1,v2: Float): Boolean;

procedure r4_get_rlz(fp: PFloats; fw,fh: Integer;
                     tx,ty,tw,th: Integer;
                     a0,k0: Float; wp: PWords);

implementation

function r4_min_max(fp: PFloats; n: Integer;
                    v0: Float; out v1,v2: Float): Boolean;
var
  i: Integer; v: Float;
begin
  Result:=false; v1:=0; v2:=0;

  if n > 0 then

  if IsNAN(v0) then begin
    v1:=fp[0]; v2:=v1;
    for i:=1 to n do begin
      v:=fp[0]; fp:=@fp[1];
      if v < v1 then v1:=v;
      if v > v2 then v2:=v;
    end; Result:=true
  end
  else begin
    for i:=1 to n do begin
      v:=fp[0]; fp:=@fp[1];

      if v > v0 then

      if Result then begin
        if v < v1 then v1:=v;
        if v > v2 then v2:=v;
      end
      else begin
        v1:=v; v2:=v; Result:=true
      end
    end
  end
end;

procedure r4_get_rlz(fp: PFloats; fw,fh: Integer;
                     tx,ty,tw,th: Integer;
                     a0,k0: Float; wp: PWords);
var
  i,ix,iy,fx,fy: Integer;
  fp1: PFloats; v: Float;
begin
  for i:=0 to tw*th-1 do wp[i]:=0;

  for iy:=0 to th-1 do begin

    fy:=ty+iy;
    if (fy < 0) or (fy >= fh) then

      wp:=@wp[tw]

    else begin

      fp1:=@fp[fy*fw];
      for ix:=0 to tw-1 do begin

        fx:=tx+ix;
        if (fx >= 0) and (fx < fw) then begin
          v:=fp1[fx]; if v >= -a0 then
          wp[0]:=Round( (fp1[fx]+a0)/k0 );
        end;
        
        wp:=@wp[1]
      end
    end
  end
end;

end.