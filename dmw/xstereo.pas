unit xstereo; interface

uses
  otypes,xyz,xxx;

const
  xxx_Math: Boolean = true;

type
  txy = tgauss;

  xxyz = record case integer of
0: (g: tgauss); 1: (v: txyz)
  end;

  txxx_R1 = array[0..1] of TReper;
  txxx_c = array[0..1] of tgauss;
  txxx_R2 = array[0..2] of TReper;

  TStereo = record
    Left,Eye: Boolean;
    Id,Width,Height,Tag: Integer;
    Focus,ppmm: Double;

    e: TSpace;

    R1: txxx_R1;
    R2: txxx_R2;
    bmp_c: txxx_c;

    l_xy,g_xy: GOrient;
    lg_h: Double; sys: XSpace;

    geo: tsys; plan_dx,plan_dy: Double;

    sys_Enabled: Boolean;
    pp_Enabled: Boolean;

    l_bmp,r_bmp: TNameStr;
    Folder: TShortStr;
  end;

function xxx_Load_Pair(Path: PChar; id: Integer;
                       out scb: TStereo): Boolean;

function xxx_Delta_Plan(Path: PChar;
                        var scb: TStereo): Boolean;

function Stereo_Project(const scb: TStereo;
                        const in_v: txyz;
                        out out_p: txy): Boolean;

procedure xStereo_Project(const scb: TStereo;
                          const p: xgeoid; h: Double;
                          out out_p: txy);

function Stereo_Backup(const scb: TStereo;
                       x1,x2, y: Double;
                       out v: txyz): Integer;

implementation

function xxx_Load_Pair(Path: PChar; id: Integer;
                       out scb: TStereo): Boolean;
begin
  Result:=false
end;

function xxx_Delta_Plan(Path: PChar;
                        var scb: TStereo): Boolean;
begin
  Result:=false
end;

function Stereo_Project(const scb: TStereo;
                        const in_v: txyz;
                        out out_p: txy): Boolean;
begin
  Result:=false;
  out_p.x:=0; out_p.y:=0;
end;

procedure xStereo_Project(const scb: TStereo;
                          const p: xgeoid; h: Double;
                          out out_p: txy);
begin
  out_p.x:=0; out_p.y:=0;
end;

function Stereo_Backup(const scb: TStereo;
                       x1,x2, y: Double;
                       out v: txyz): Integer;
begin
  Result:=0; v:=_xyz(0,0,0)
end;

end.