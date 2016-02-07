unit dm_dial; interface

uses
//  Windows,
  Classes,
//  ,Menus,
  otypes,
  ogauss
//  XInputs,
//  XIntf,dmw_dm,dmw_prj
  ;

function Dial_xy(const lg_T: AB_Orient;
                 var p: TPoint; def: Boolean): boolean;
              (*
function dm_dlg_Flags(var flags: Integer; Capt: PChar): Boolean;

function dm_dlg_Level(lev: Integer;
                      scalep: PIntegers;
                      scalen: Integer;
                      var isAll: Boolean): Integer;

function dm_Edit_Text(str: PWideChar; vgt: PChar;
                      up: Integer): integer;

function dm_Dial_Sign(Sign: Integer; Path: PChar;
                      line: boolean): Integer;

function dm_Dial_mf(dm: tdm_Map;
                    lp: PLLine; hp: PIntegers;
                    sp: PVLine; sMax: Integer;
                    loc: pxyz): Boolean;

function dlg_Dist(var d: double;
                  scale: Integer;
                  Capt: PChar): Boolean;

function dm_scale_range(var v1,v2: Longint): Boolean;

function dm_dial_Cls(dm: tdm_Map; var dist: double): Boolean;

function dm_dial_Dist(dm: tdm_Map;
                      var dist: double;
                      Capt: PChar): Boolean;

procedure dm_About_Sizes(l,a,b: double;
                         scale: Integer;
                         Capt: PChar);

function dm_dial_z(def: Double; out z: Double): Boolean;

function dm_dial_scale_k(out k: Double): Boolean;

function dial_dotz(vp: pfloats; vn: int): bool;

function dial_fft(Pcx,Fft: PChar): PChar;

function dm_Edit_Menu(Inst: THandle; Path: PChar): Boolean;

procedure dm_Popup_Active_map(Menu: TPopupMenu;
                              Prj: TMapProject;
                              OnClick: TNotifyEvent;
                              cX,cY: Integer);

function dm_dial_Active_map(Prj: TMapProject): Integer;

function dm_Edit_Project(Prj: TMapProject): Integer;

procedure dm_Dial_Options(Prj: TMapProject;
                          var Disp: tx_Disp;
                          var Disp1: Int64);

function dm_Dial_idx(Prj: TMapProject): Boolean;

function dm_dial_attv(var attv: Cardinal): Boolean;

function Dial_Object(var Id,Code: uint;
                     var Loc,Ptr,nn: int;
                     nm: PChar): int;

function dial_rr2(dp: PDoubles): Boolean;

function dm_dial_grid(var dx,dy: Double;
                      const top,dir: TGauss;
                      scale,show: Integer): Boolean;

function dm_dial_print1(pd: PDoubles;
                        pi: PIntegers;
                        const ADrawOp: IPrint;
                       folder: PChar): Integer;

function dm_dlg_legend(Legend: PChar; iCode,iLoc: Integer;
                       out Code,Loc: Integer; Name: PChar): Boolean;
                     *)
implementation

uses
//  Math,SysUtils,Controls,Dialogs,
//  Convert,OFiles,XPoly,XList,XList1,
//  dmw_fnt,dmw_vars,dmw_link,dmw_rus,
  use_stg,use_util;

function Load_dlg_dmw: THandle;
begin
  Result:=Load_dm_dll('dlg_dmw.dll')
end;

function Dial_xy(const lg_T: AB_Orient;
                 var p: TPoint; def: Boolean): boolean;
const
  is_bl: Boolean = false;
  is_xy: Boolean = false;

  xy: tgauss = (x:0; y:0);
  bl: tgeoid = (b:0; l:0);

var
  r: tgeoid; g: tgauss;
begin
  Result:=false;

  if lg_T.sys.pps = 0 then begin
    lg_T.L_to_G(p,g);
    if is_xy and def then g:=xy;

    if dm_Dial_Gauss(g) then begin
      is_xy:=true; xy:=g;
      lg_T.G_to_L(g,p); Result:=true
    end
  end
  else begin
    lg_T.L_to_R(p,r);
    if is_bl and def then r:=bl;

    with lg_T.sys do
    if dm_Dial_Geoid(r.b,r.l, lg_T.sys) then begin
      is_bl:=true; bl:=r;
      r.l:=lg_T.Align_longitude(r.l);
      lg_T.R_to_L(r,p); Result:=true
    end
  end
end;
                           (*
function dm_dlg_Flags(var flags: Integer; Capt: PChar): Boolean;
type
  tdlg = function(var flags: Integer; Capt: PChar): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_dmw;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dmw_dlg_Flags');
    Result:=dlg(flags,Capt); wFreeLibrary(dll)
  end
end;

function dm_dlg_Level(lev: Integer;
                      scalep: PIntegers;
                      scalen: Integer;
                      var isAll: boolean): Integer;
type
  tdlg = function(lev: Integer;
                  scalep: PIntegers;
                  scalen: Integer;
                  var isAll: boolean): Integer; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=-1;

  dll:=Load_dlg_dmw;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dmw_dlg_Level');
    Result:=dlg(lev,scalep,scalen,isAll);
    wFreeLibrary(dll)
  end
end;

function dm_Edit_Text(str: PWideChar; vgt: PChar;
                      up: Integer): integer;
type
  tdlg_text = function(up: Integer; s: PWideChar): integer;
  stdcall;

  tdlg_wide = function(s: PWideChar; vgt: PChar;
                       up: Integer): integer; stdcall;

var
  dll: THandle;
  dlg_text: tdlg_text;
  dlg_wide: tdlg_wide;
  wide: Boolean;
begin
  Result:=-1; wide:=false;

  if txt_english then wide:=FileExist(vgt)
  else                wide:=Is_wide_vgt(vgt);

  dll:=Load_dlg_dmw;
  if dll >= 32 then begin

    if vgt[0] = '*' then begin
      @dlg_wide:=GetProcAddress(dll,'dmw_Edit_ttf');
      if Assigned(dlg_wide) then
      Result:=dlg_wide(str,@vgt[1],up);
    end else
    if wide then begin
      @dlg_wide:=GetProcAddress(dll,'dmw_Edit_Wide');
      if Assigned(dlg_wide) then
      Result:=dlg_wide(str,vgt,up);
    end
    else begin
      @dlg_wide:=GetProcAddress(dll,'vgt_Edit_Text');
      if Assigned(dlg_wide) then
        Result:=dlg_wide(str,vgt,up)
      else begin
        @dlg_text:=GetProcAddress(dll,'dmw_Edit_Text');
        if Assigned(dlg_text) then Result:=dlg_text(up,str);
      end
    end;

    wFreeLibrary(dll)
  end
end;

function dm_Dial_Sign(Sign: Integer; Path: PChar;
                      line: boolean): Integer;
type
  tdlg = function(Sign: Integer; Path: PChar;
                  line: boolean): Integer; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=Sign;

  dll:=Load_dlg_dmw;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dmw_Dial_Sign');
    if Assigned(dlg) then
    Result:=dlg(Sign,Path,line);
    wFreeLibrary(dll)
  end
end;

function dm_Dial_mf(dm: tdm_Map;
                    lp: PLLine; hp: PIntegers;
                    sp: PVLine; sMax: integer;
                    loc: pxyz): Boolean;
type
  tdlg1 = function(lp: PVLine; lMax: Integer;
                   sp: PVLine; sMax: Integer): Boolean; stdcall;

  tdlg2 = function(mf: IPolyF; loc: pxyz): Boolean; stdcall;

function exec_dlg1(dlg: tdlg1;
                   lp: PLLine; hp: PIntegers;
                   sp: PVLine; sMax: Integer): Boolean;
var
  vp: PVLine;
begin
  Result:=false;

  vp:=Alloc_VLLine(LPolyMax+1);

  if Assigned(vp) then begin
    LLine_to_VLine(lp,hp, vp,LPolyMax);
    Result:=dlg(vp,LPolyMax,sp,sMax);
    VLine_to_LLine(vp, lp,hp, LPolyMax)
  end;

  xFreePtr(vp)
end;

function exec_dlg2(dlg: tdlg2; dm: tdm_Map;
                   lp: PLLine; hp: PIntegers;
                   loc: pxyz): Boolean;
var
  mf: IPolyF; i,n,z: Integer; v: txyz; l: lxyz;
begin
  Result:=false;

  if GetIntfPolyF(mf) = S_OK then begin

    for i:=0 to lp.N do begin
      z:=0; if Assigned(hp) then z:=hp[i];
      with lp.Pol[i] do v:=dm.dm_to_xyz(X,Y,z);
      mf.Insert(-1,@v)
    end;

    if dlg(mf,loc) then begin

      n:=mf.GetCount;
      if n < LPolyMax then begin

        for i:=0 to n-1 do begin
          mf.GetPoint(i,@v,3);
          l.v:=dm.xyz_to_dm(v.x,v.y,v.z);
          lp.Pol[i]:=l.p; if Assigned(hp) then
          hp[i]:=l.v.z
        end;

        lp.N:=n-1
      end;

      Result:=true

    end; mf:=nil
  end
end;

var
  dll: THandle; dlg1: tdlg1; dlg2: tdlg2;
begin
  Result:=false;

  dll:=Load_dlg_dmw;
  if dll >= 32 then begin

    @dlg2:=GetProcAddress(dll,'dial_mf');
    if Assigned(dlg2) then
      Result:=exec_dlg2(dlg2,dm, lp,hp,loc)
    else begin
      @dlg1:=GetProcAddress(dll,'Edit_Line');
      if Assigned(dlg1) then
        Result:=exec_dlg1(dlg1, lp,hp, sp,sMax);
    end;

    wFreeLibrary(dll)
  end
end;

function dlg_Dist(var d: double;
                  scale: Integer;
                  Capt: PChar): Boolean;
type
  tdlg = function(var d: double;
                  scale: Integer;
                  Capt: PChar): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_dmw;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'Dial_Dist');
    if Assigned(dlg) then
    Result:=dlg(d,scale,Capt);
    wFreeLibrary(dll)
  end
end;

function dm_scale_range(var v1,v2: Longint): Boolean;
type
  tdlg = function(var v1,v2: Longint): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_dmw;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dmw_scale_range');
    if Assigned(dlg) then Result:=dlg(v1,v2);
    wFreeLibrary(dll)
  end
end;

function dm_dial_Cls(dm: tdm_Map; var dist: double): Boolean;
var
  d,res: Double; Capt: TShortStr;
begin
  Result:=false;
  dmw_Message(Capt,'(dm) Шаг прореживания',636);

  res:=Max(dm.dm_Resolution,0.0001);
  d:=dist * res; 

  if dlg_Dist(d,dm.dm_scale,Capt) then begin
    dist:=d/res; Result:=true
  end
end;

function dm_dial_Dist(dm: tdm_Map;
                      var dist: double;
                      Capt: PChar): Boolean;
var
  d,res: Double;
begin
  Result:=false; if dist <= 0 then dist:=1;
  res:=Max(dm.dm_Resolution,0.0001); d:=dist * res;

  if dlg_Dist(d,dm.dm_scale,Capt) then begin
    dist:=d/res; Result:=true
  end
end;

procedure dm_About_Sizes(l,a,b: double;
                         scale: Integer;
                         Capt: pChar);
type
  tdlg = procedure(l,a,b: double; scale: Integer;
                   Capt: pChar); stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  dll:=Load_dlg_dmw;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'About_Sizes');
    if Assigned(dlg) then dlg(l,a,b,scale,Capt);
    wFreeLibrary(dll)
  end
end;

function dm_dial_z(def: Double; out z: Double): Boolean;
var
  capt,msg: TShortStr;
begin
  dmw_Capt(capt);

  if rus_interface then
    StrCopy(msg,'Z - Координата,м')
  else
    StrCopy(msg,'Z - Coordinate,m');

  Result:=Dial_Real(def,z,capt,msg)
end;

function dm_dial_scale_k(out k: Double): Boolean;
var
  capt,msg: TShortStr;
begin
  dmw_Capt(capt);

  if rus_interface then
    StrCopy(msg,'Масштабировать')
  else
    StrCopy(msg,'Scale value');

  Result:=Dial_Real(1,k,capt,msg)
end;

function dial_dotz(vp: pfloats; vn: int): bool;
type
  tdlg = function(vp: pfloats; vn: int): bool; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_dmw;
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dial_dotz');
    if Assigned(dlg) then
    Result:=dlg(vp,vn);
    wFreeLibrary(dll)
  end
end;

function dial_fft(Pcx,Fft: PChar): PChar;
var
  Items: TStringList; ind: Integer;
  dir,fn,capt: TShortStr;
begin
  Result:=nil; StrCopy(Pcx,'');

  Items:=TStringList.Create;
  try
    if Strings_fft(Items,Fft) > 0 then begin
      StrPCopy(Capt,'(fft) '+xStrNameExt(Fft));
      ind:=stg_dial_List(Items,nil,-1,Capt);

      if ind >= 0 then begin
        StrDirectory(dir,Fft);
        StrPCopy(fn,Items[ind]);
        Result:=StrPath(Pcx,dir,fn)
      end
    end;
  finally
    Items.Free
  end
end;

function dm_Edit_Menu(Inst: THandle; Path: PChar): boolean;
type
  tdlg = function(Inst: THandle; Path: PChar): boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_dmw; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dmw_Edit_Menu');
    Result:=dlg(Inst,Path); wFreeLibrary(dll);
  end

end;

procedure dm_Popup_Active_map(Menu: TPopupMenu;
                              Prj: TMapProject;
                              OnClick: TNotifyEvent;
                              cX,cY: Integer);
var
  dm: TStringlist; it: TMenuItem;
  i: Integer; fn: TShortstr;
begin
  Menu.Items.Clear;

  dm:=TStringlist.Create;
  try
    if Prj.Get_dm_list(dm) > 1 then
    for i:=0 to dm.Count-1 do begin
      StrPCopy(fn,dm[i]);
      it:=popup_add_Item(Menu,OnClick,fn,nil,i);
      if i = Prj.ActiveMap then it.Checked:=true
    end;
  finally
    dm.Free
  end;

  if Menu.Items.Count > 1 then
  Menu.Popup(cX,cY);
end;

function dm_dial_Active_map(Prj: TMapProject): Integer;
var
  dm: TStringlist; capt: TShortstr;
begin
  Result:=-1;

  if rus_interface then
    StrCopy(capt,'(dm) Редактор')
  else
    StrCopy(capt,'(dm) Editor');

  dm:=TStringList.Create;
  try
    if Prj.Get_dm_list(dm) > 1 then
    Result:=stg_dial_List(dm,nil,Prj.ActiveMap,capt);
  finally
    dm.Free
  end
end;

function dm_Edit_Project(Prj: TMapProject): Integer;
type
  tdlg = function(Prj: PChar): Integer; stdcall;
var
  dll: THandle; dlg: tdlg;
  tmp: TMapProject; fn: TShortStr;
begin
  Result:=-1;

  StrWorkPath(fn,'prj.tmp');

  if Prj.Enabled then
  if Prj.SaveAs(fn,false) <> nil then begin

    dll:=Load_dlg_dmw; if dll >= 32 then begin
      @dlg:=GetProcAddress(dll,'dmw_Edit_Project');
      if Assigned(dlg) then Result:=dlg(fn);
      wFreeLibrary(dll)
    end;

    if Result >= 0 then begin
      tmp:=TMapProject.Create;
      try
        tmp.LoadFrom(fn,false);
        Prj.Assign(tmp,0);
      finally
        tmp.Free
      end
    end;

    FileErase(fn)
  end
end;

procedure dm_Dial_Options(Prj: TMapProject;
                          var Disp: tx_Disp;
                          var Disp1: Int64);
type
  tdlg = function(IParams: PIntegers;
                  IParamsN: Integer;

                  FParams: PFloats;
                  FParamsN: Integer;

                  var Disp: tx_Disp;
                  var Disp1: Int64): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
  iv: IValues32; fv: floats8;
begin
  Disp.Math:=true;

  dll:=Load_dlg_dmw; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dmw_Dial_Options');
    if Assigned(dlg) then begin

      fv[0]:=Prj.Params.m_step;
      iv[0]:=Prj.Params.m_ed;

      fv[1]:=Prj.Params.j_dist;
      iv[1]:=Prj.Params.j_ed;

      iv[2]:=x_Edit.rule_nn;
      iv[3]:=Prj.bak_dt;

      with Prj.Params do begin
        iv[4]:=brkLen;
        iv[5]:=brkAuto;
        iv[6]:=brkMode or (brkMode1*8);
        iv[7]:=brkMax;
        iv[8]:=brkPull;
        iv[9]:=brkPer;
        iv[10]:=mark_dr;

        iv[11]:=p_eps;
        iv[12]:=p_ed;

        iv[13]:=brkSharp;
        iv[14]:=brkThick;

        iv[15]:=mf_nn;
        iv[16]:=sqr_nn;
        iv[17]:=blankText
      end;

      if dlg(@iv,18,@fv,2, Disp,Disp1) then begin
        Prj.Params.m_step:=fv[0];
        Prj.Params.m_ed:=iv[0];

        Prj.Params.j_dist:=fv[1];
        Prj.Params.j_ed:=iv[1];

        x_Edit.rule_nn:=iv[2];
        Prj.bak_dt:=iv[3];

        Prj.Params.brkLen:=iv[4];
        Prj.Params.brkAuto:=iv[5];

        Prj.Params.brkMode:=iv[6] and 7;
        Prj.Params.brkMode1:=iv[6] div 8;

        Prj.Params.brkMax:=iv[7];
        Prj.Params.brkPull:=iv[8];
        Prj.Params.brkPer:=iv[9];

        Prj.Params.mark_dr:=iv[10];

        Prj.Params.p_eps:=iv[11];
        Prj.Params.p_ed:=iv[12];

        Prj.Params.brkSharp:=iv[13];
        Prj.Params.brkThick:=iv[14];
        Prj.Params.mf_nn:=iv[15];
        Prj.Params.sqr_nn:=iv[16];

        Prj.Params.blankText:=iv[17]
      end
    end;

    wFreeLibrary(dll)
  end
end;

function dm_Dial_idx(Prj: TMapProject): Boolean;
type
  tdlg = function(var Buf: TAllocBuffer;
                  Objects: PChar): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
  buf: TAllocBuffer; obj: TShortStr;
begin
  Prj.Map.Objects_Path(obj,'.OBJ');
  Prj.DispAttrs.Get_ctrl(buf);

  dll:=Load_dlg_dmw; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dmw_Dial_idx');
    if Assigned(dlg) then Result:=dlg(buf,obj);
    wFreeLibrary(dll)
  end;

  if Result then begin
    Prj.DispAttrs.Set_ctrl(Buf);
    Prj.Push_DispAttrs
  end
end;

function dm_dial_attv(var attv: Cardinal): Boolean;
type
  tdlg = function(var attv: Cardinal): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_dmw; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dial_attv');
    Result:=dlg(attv); wFreeLibrary(dll)
  end
end;

function Dial_Object(var Id,Code: uint;
                     var Loc,Ptr,nn: int;
                     nm: PChar): int;
type
  tdlg = function(var Id,Code: uint;
                  var Loc,Ptr: int;
                  nm: PChar): int; stdcall;

  tdlg1 = function(var Id,Code: uint;
                   var Loc,Ptr,nn: int;
                   nm: PChar): int; stdcall;

var
  dll: THandle; dlg: tdlg; dlg1: tdlg1;
begin
  Result:=mrCancel;

  dll:=Load_dlg_dmw;
  if dll >= 32 then begin

    @dlg1:=GetProcAddress(dll,'dm_Dial_Object1');
    if Assigned(dlg1) then
      Result:=dlg1(Id,Code,Loc,Ptr,nn,nm)
    else begin
      @dlg:=GetProcAddress(dll,'dm_Dial_Object');
      if Assigned(dlg) then
      Result:=dlg(Id,Code,Loc,Ptr,nm)
    end;
      
    wFreeLibrary(dll)
  end
end;

function dial_rr2(dp: PDoubles): Boolean; 
type
  tfunc = function(dp: PDoubles): Boolean; stdcall;
var
  dll: THandle; func: tfunc;
begin
  Result:=false;
  dll:=Load_dlg_dmw; if dll >= 32 then begin
    @func:=GetProcAddress(dll,'dial_rr2');
    if Assigned(func) then Result:=func(dp);
    wFreeLibrary(dll)
  end
end;

function dm_dial_grid(var dx,dy: Double;
                      const top,dir: TGauss;
                      scale,show: Integer): Boolean;
type
  tdlg = function(var dx,dy: Double;
                  const top,dir: TGauss;
                  scale,show: Integer): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dm_dll('dll_misc.dll');
  if dll >= 32 then begin
    dlg:=GetProcAddress(dll,'dm_dial_Align');
    Result:=dlg(dx,dy,top,dir,scale,show);
    wFreeLibrary(dll)
  end
end;

function dm_dial_print1(pd: PDoubles;
                        pi: PIntegers;
                        const ADrawOp: IPrint;
                       folder: PChar): Integer;
type
  tdlg = function(pd: PDoubles;
                  pi: PIntegers;
                  const ADrawOp: IPrint;
                  folder: PChar): Integer; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=-1;

  dll:=Load_dm_dll('dll_prn.dll');
  if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dial_print1');
    if Assigned(dlg) then
    Result:=dlg(pd,pi,ADrawOp,folder);
    wFreeLibrary(dll)
  end
end;

function dm_dlg_legend(Legend: PChar; iCode,iLoc: Integer;
                       out Code,Loc: Integer; Name: PChar): Boolean;
type
  tdlg = function(Legend: PChar; iCode,iLoc: Integer;
                  out Code,Loc: Integer; Name: PChar): Boolean; stdcall;
var
  dll: THandle; dlg: tdlg;
begin
  Result:=false;

  dll:=Load_dlg_dmw; if dll >= 32 then begin
    @dlg:=GetProcAddress(dll,'dm_dlg_legend');
    if Assigned(dlg) then
    Result:=dlg(Legend, iCode,iLoc, Code,Loc, Name);
    wFreeLibrary(dll)
  end
end;
                             *)
end.