unit tstu6; interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, otypes, dmw_prj, dm_draw, types;

type

  { TDfmTstu6 }

  TDfmTstu6 = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    MenuItem1: TMenuItem;
    ExitItem: TMenuItem;
    CloseMap: TMenuItem;
    MenuItem2: TMenuItem;
    CloseRelief: TMenuItem;
    itemBmp: TMenuItem;
    dll_prj_bmp: TMenuItem;
    dll_prj_png: TMenuItem;
    closeImage: TMenuItem;
    itemTest: TMenuItem;
    MiscMenu: TMenuItem;
    openImage: TMenuItem;
    MenuItem4: TMenuItem;
    OpenRelief: TMenuItem;
    OpenMap: TMenuItem;
    paCard: TPanel;
    paDown: TPanel;
    pbCard: TPaintBox;
    paPaint: TPanel;
    paVert: TPanel;
    paRB: TPanel;
    paHorz: TPanel;
    sbHorz: TScrollBar;
    sbVert: TScrollBar;
    procedure closeImageClick(Sender: TObject);
    procedure CloseMapClick(Sender: TObject);
    procedure dll_prj_bmpClick(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure FileMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure itemBmpClick(Sender: TObject);
    procedure dll_prj_pngClick(Sender: TObject);
    procedure itemTestClick(Sender: TObject);
    procedure openImageClick(Sender: TObject);
    procedure OpenMapClick(Sender: TObject);
    procedure OpenReliefClick(Sender: TObject);

  private
    fmaps: TMapProject;
    fdmCard: TDrawCard;
    fProject: TShortstr;

    procedure Refresh_card(cls: Boolean);
    procedure PrjChanged(Sender: TObject);

    procedure ChangeZoom(cls: Boolean);
    procedure ResetZoom;
    function GetWin(out gc: TGeoPoint): Double;
    procedure SetWin(const gc: TGeoPoint; sc: Double);

    procedure dll_prj_tile(bmp: bool);

  public
  end;

var
  DfmTstu6: TDfmTstu6;

implementation  {$R *.lfm}

{ TDfmTstu6 }

uses
  LCLType,
  convert,
  ofiles,
  xini,
  xintf2,
  cardbox,
  xcards,
  xfiles,
  xddw,
  idib,
  xinputs,
  xutils,
  xwin,
  img_rel,
  tiff_x,
  xbmp,
  idmw,
  prj_com;

procedure TDfmTstu6.FormCreate(Sender: TObject);
var
  cardBox: TCardBox;
  tmp1: ICardBox;
  tmp2: ICardBoxDraw;
begin
  fdmCard:=TDrawCard.Create(nil);
  fmaps:=fdmCard.InitGlobe;
  fmaps.OnChangeMap:=PrjChanged;
  fmaps.OnChanged:=PrjChanged;

  cardBox:=TCardBox.Create(pbCard,sbHorz,sbVert,nil);
  try
    with TCardBoxDraw.Create(fdmCard) do
    if GetInterface(ICardBoxDraw,tmp2) then
    cardBox.draw:=tmp2; tmp2:=nil;

    if cardBox.GetInterface(ICardBox,tmp1) then begin
      fdmCard.Card:=tmp1;
      cardBox:=nil; tmp1:=nil
    end;

  finally
    cardBox.Free;
  end;

  paDown.ClientHeight:=sbHorz.Height;
  paVert.ClientWidth:=sbVert.Width;
  paRB.Width:=paVert.Width;
  ini.GetForm(Self);
end;

procedure TDfmTstu6.FormDestroy(Sender: TObject);
begin
  ini.PutForm(Self);

  fmaps:=fdmCard.FreeGlobe;
  fdmCard.Free;
end;

procedure TDfmTstu6.FormHide(Sender: TObject);

function SaveProject(Dest: PChar): int;
begin
  Result:=-1;
  if StrLen(Dest) > 0 then
  with fmaps do begin
    with win_edit do fdmCard.Get_win(@iv,@rv);
    SaveAs(Dest,true); Result:=Count
  end
end;

var
  fn: TShortstr;
begin
  StrWorkPath(fn,'tstu6.###');
  SaveProject(fn)
end;

procedure TDfmTstu6.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  fdmCard.KeyDown(Key,Shift);
end;

procedure TDfmTstu6.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  p: TPoint; ct: uint;
begin
  if ControlContainsCursor(pbCard,@p) then begin
    ct:=GetTickCount;
    fdmCard.Card_wheel(p.X,p.Y,-1);
    if Abs(GetTickCount-ct) > 100 then
    Cls_MouseWheel(Handle);
    Handled:=true
  end
end;

procedure TDfmTstu6.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  p: TPoint; ct: uint;
begin
  if ControlContainsCursor(pbCard,@p) then begin
    ct:=GetTickCount;
    fdmCard.Card_wheel(p.X,p.Y,+1);
    if Abs(GetTickCount-ct) > 100 then
    Cls_MouseWheel(Handle);
    Handled:=true
  end
end;

procedure TDfmTstu6.Refresh_card(cls: Boolean);
begin
  fdmCard.Card_zoom(cls);
  if fdmCard.isMoving then
    fdmCard.Setup_Edit_Map_Display
  else
    fdmCard.Card_Draw(nil)
end;

procedure TDfmTstu6.FormShow(Sender: TObject);

procedure ShowCentre(cx,cy,scale: Double; pps: Integer);
var
  r: TGeoid; g: TGauss; p: TPoint;
begin
  if (pps = 0)
  or (fmaps.Map.LG_T.sys.pps = 1) then begin

    if pps = 1 then begin
      r.b:=cx; r.l:=cy;
      fmaps.Map.LG_T.z_BL_XY(r,g);
      cx:=g.x; cy:=g.y
    end;

    g.x:=cx; g.y:=cy;
    fmaps.Map.LG_T.Z_to_L(g,p);
    fdmCard.lg_p.L_to_G(p,g);

    if not fmaps.Project_Contains(g) then begin
      if scale = -3 then fdmCard.Card_Refresh(nil)
    end else

    if scale = -4 then begin
      fdmCard.IsAnimate:=true;
      fdmCard.Card_Track(p);
      fdmCard.IsAnimate:=false;
    end
    else begin
      fdmCard.isZoom:=true;
      fdmCard.isAnimate:=scale = -3;
      fdmCard.Set_Gauss_Centre(cx,cy,0, scale);
      fdmCard.isZoom:=false;
      fdmCard.isAnimate:=false
    end
  end
end;

procedure ReliefChange(Sender: TObject);
var
  is_z: Boolean;
begin     (*
  is_z:=fmaps.Bank1.Count > 0;
  if pa_z.Visible <> is_z then begin
    pa_z.Visible:=is_z; paHorzResize(nil)
  end       *)
end;

function OpenProject(Path: PChar): int;
var
  gc: TCardCentre;
  fr: TCardFrame;
begin
  fmaps.LoadFrom(Path,true);
  StrCopy(fProject,Path);

  fmaps.Pop_DispAttrs;

  gc:=fmaps.win_gc;
  if gc.sc > 0 then begin
    fdmCard.Card_Zoom(true);
    fdmCard.Setup_Edit_Map_Display;

    with gc.pos do
    ShowCentre(x,y,gc.sc, gc.pos.s.pps)
  end
  else begin
    fr:=fmaps.win_edit;
    fdmCard.isPcx:=fr.iv[3] = 1; fr.iv[4]:=0;
    fdmCard.Set_win(@fr.iv,@fr.rv);
    fmaps.win_edit:=fr
  end;

  Refresh_card(true);
  fmaps.Map.Close_Map;

  ReliefChange(nil)
end;

var
  fn: TShortstr;
begin
  StrWorkPath(fn,'tstu6.###');
  OpenProject(fn)
end;

procedure TDfmTstu6.itemBmpClick(Sender: TObject);
var
  dir,capt,fn: TShortstr;
  map: Bitmap;
begin
  ini.GetDir('bmp_dir',dir);
  StrCopy(capt,'Сохранить копию окна как');
  if cOpenfile(dir,'#bmp',capt,fn) then begin
    ini.PutDir('bmp_dir',dir);

    if fdmCard.Cash.GetMap(map) then
    BitmapAsBmp(map,fn)
  end;
end;

procedure TDfmTstu6.ChangeZoom(cls: Boolean);
begin
  fdmCard.Card_zoom(not fdmCard.isMoving);
  if fdmCard.isMoving then fdmCard.Setup_Edit_Map_Display
  else fdmCard.Card_xRepaint(nil)
end;

procedure TDfmTstu6.ResetZoom;
begin
  fdmCard.Zoom:=-1; ChangeZoom(false)
end;

function TDfmTstu6.GetWin(out gc: TGeoPoint): Double;
begin
  Result:=-1;
  Fillchar(gc,Sizeof(gc),0);

  if not fdmCard.isMoving then
  if fmaps.Count2 > 0 then
  Result:=fdmCard.Get_prj_centre(gc);
end;

procedure TDfmTstu6.SetWin(const gc: TGeoPoint; sc: Double);
begin
  if fmaps.auto_Resize then sc:=-1 else

  if fmaps.Count = 0 then
  if fmaps.Bank2.Count = 1 then
  sc:=-1;

  if sc < 0 then ResetZoom else
  fdmCard.Set_prj_centre(gc,sc)
end;

procedure TDfmTstu6.OpenMapClick(Sender: TObject);

function InsertMap(Path: PChar): int;
var
  g: xgeoid; sc: Double;
begin
  Result:=-1;

  if fmaps.Contains_Map(Path) < 0 then begin
    sc:=GetWin(g);
    Result:=fmaps.Insert_Map(Path,0,0,0,0);

    if Result >= 0 then

    if sc > 0 then
      SetWin(g,sc)
    else
    if fmaps.Count > 1 then
      fdmCard.Card_Update(false)
    else begin
      fdmCard.zoom:=-1;
      Refresh_card(true);
    end
  end
end;

var
  dir,capt,dm: TShortstr;
begin
  ini.GetDir('dm_dir',dir);
  StrCopy(capt,'Открыть карту');
  if cOpenfile(dir,'#dm',capt,dm) then begin
    ini.PutDir('dm_dir',dir);
    InsertMap(dm)
  end
end;

procedure TDfmTstu6.OpenReliefClick(Sender: TObject);
var
  dir,capt,fn: TShortstr;
  rel: TRelief; z: double;
begin
  ini.GetDir('dm_dir',dir);
  StrCopy(capt,'Открыть рельеф');
  if cOpenfile(dir,'#rlz',capt,fn) then begin

    rel:=TRelief.Create;
    try
      if rel.Open(fn) then
      if rel.Get_z(rel.Width div 2,
                   rel.Height div 2,z) then
      Caption:=RealToStr(z,1)+'m';

    finally
      rel.Free;
    end;

  end;
end;

procedure TDfmTstu6.openImageClick(Sender: TObject);

procedure tiff_test(Path: PChar);
var
  tiff: TTiffView; i,j,k,nx,ny: int; buf: Pointer;
begin
  tiff:=TTiffView.Create;
  try
    if tiff.Open(Path) then begin

      nx:=tiff.tile.XTiles;
      ny:=tiff.tile.YTiles;
      k:=0;

      buf:=tiff.Get_tile_buf;

      for j:=0 to ny-1 do
      for i:=0 to nx-1 do
      if tiff.Get_tile(i,j,buf) then
      Inc(k);

      Caption:=Format('ntiles=%d used=%d',[nx*ny,k])
    end;
  finally
    tiff.Free;
  end;
end;

var
  dir,capt,fn: TShortstr;
  g: xgeoid; sc: Double;
begin
  ini.GetDir('dm_dir',dir);
  StrCopy(capt,'Открыть растр');
  if cOpenfile(dir,'#tif#jpg',capt,fn) then

  if false then
    tiff_test(fn)
  else begin
    fmaps.Photo:=fmaps.Image;
    if fmaps.Open_image1(nil,fn,-1,false) then begin

      sc:=GetWin(g);
      fdmCard.IsPcx:=true;

      if fmaps.Count > 0 then
        SetWin(g,sc)
      else
        Refresh_card(true)
    end;
  end;
end;

procedure TDfmTstu6.closeImageClick(Sender: TObject);
var
  g: xgeoid; sc: Double;
begin
  if fdmCard.IsPcx then begin

    sc:=GetWin(g);

    fmaps.Close_pcx;
    fdmCard.IsPcx:=false;

    if fmaps.Count > 0 then
      SetWin(g,sc)
    else
      Refresh_card(true)
  end;
end;

procedure TDfmTstu6.CloseMapClick(Sender: TObject);
var
  g: xgeoid; sc: Double; ind: int;
begin
  if fmaps.Count > 0 then begin
    ind:=fmaps.ActiveMap;
    sc:=GetWin(g);
    fmaps.Delete_Map(ind);
    SetWin(g,sc)
  end
end;

procedure TDfmTstu6.dll_prj_bmpClick(Sender: TObject);
begin
  dll_prj_tile(true)
end;

procedure TDfmTstu6.dll_prj_pngClick(Sender: TObject);
begin
  dll_prj_tile(false)
end;

procedure prj_tile(prj: idmwProject; pix: double; dm,png: PChar);
const
  tw = 1024; th = 1024;
var
  b: TRectd; xc,yc: double;
  map: Bitmap; fn: TShortstr;
begin
  if allocBitmap(map,tw,th,32) then begin

    bmp_fill_rgba(map,0,0,$FFFFFF);

    prj.AddMap(dm);
    if prj.GetMapCount > 0 then begin

      prj.set_proj(9,3,47/180*Pi,0,0);

      prj.GetBound(0, @b);
      xc:=(b.x1+b.x2)/2;
      yc:=(b.y1+b.y2)/2;

      xc:=xc+th/2*pix;
      yc:=yc-tw/2*pix;

      if prj.to_tile1(@map,yc,xc,pix,pix*2,0) = 1 then begin

        StrCopy(fn,'/home/emm/temp/');
        StrCat(fn,png);

        prj.bmp_saveAs(fn,@map)
      end
    end;

    freeBitmap(map)
  end
end;

procedure TDfmTstu6.itemTestClick(Sender: TObject);
var
  prj1: tdll_prj;
  prj2: tdll_prj;
  prj3: tdll_prj;
  prj4: tdll_prj;
begin
  prj1:=tdll_prj.Create;
  prj2:=tdll_prj.Create;
  prj3:=tdll_prj.Create;
  prj4:=tdll_prj.Create;
  try
    if prj1.Enabled then
    if prj2.Enabled then
    if prj3.Enabled then
    if prj4.Enabled then  begin
      prj_tile(prj1.Intf,10,'/home/emm/neva/data/l-37/l-37-006.dm','1.png');
      Caption:='1.png';
      prj_tile(prj1.Intf,10,'/home/emm/neva/data/l-37/l-37-007.dm','2.png');
      Caption:='2.png';
      prj_tile(prj1.Intf,10,'/home/emm/neva/data/l-37/l-37-008.dm','3.png');
      Caption:='3.png';
      prj_tile(prj1.Intf,10,'/home/emm/neva/data/l-37/l-37-009.dm','4.png');
      Caption:='4.png';
    end;
  finally
    prj4.Free;
    prj3.Free;
    prj2.Free;
    prj1.Free;
  end;
end;

procedure TDfmTstu6.dll_prj_tile(bmp: bool);

procedure prj_tile(prj: idmwProject; bmp: bool);
const
  tw = 1024; th = 1024;
var
  b: TRectd; xc,yc,pix: double; g: xgeoid;
  map: Bitmap; fn: TShortstr;
begin
  if fmaps.Count > 0 then
  if allocBitmap(map,tw,th,32) then begin

    bmp_fill_rgba(map,0,0,$FFFFFF);

    pix:=GetWin(g);

    fmaps.Get_dm_Path(0,fn);
    prj.AddMap(fn);

    if prj.GetMapCount > 0 then begin

      prj.set_proj(9,3,g.y,0,0);

      prj.GetBound(0, @b);
      prj.BL_XY(g.x,g.y,xc,yc);

      xc:=xc+th/2*pix;
      yc:=yc-tw/2*pix;

      if prj.to_tile1(@map,yc,xc,pix,pix*2,0) = 1 then begin

        StrCopy(fn,'/home/emm/temp/2.');
        if bmp then StrCat(fn,'bmp')
               else StrCat(fn,'png');

        if prj.bmp_saveAs(fn,@map) = 1 then
        Caption:=fn
      end
    end;

    freeBitmap(map)
  end
end;

procedure prj_tile1(prj: Pointer; bmp: bool);
const
  tw = 1024; th = 1024;
var
  b: TRectd; xc,yc,pix: double; g: xgeoid;
  map: Bitmap; fn: TShortstr;
begin
  if fmaps.Count > 0 then
  if allocBitmap(map,tw,th,32) then begin

    bmp_fill_rgba(map,0,0,$FFFFFF);

    pix:=GetWin(g);

    fmaps.Get_dm_Path(0,fn);
    prj_AddMap(prj, fn);

    if prj_GetMapCount(prj) > 0 then begin

      prj_set_proj(prj, 9,3,g.y,0,0);

      prj_GetBound(prj, 0, @b);
      prj_BL_XY(prj, g.x,g.y,xc,yc);

      xc:=xc+th/2*pix;
      yc:=yc-tw/2*pix;

      if prj_to_tile1(prj, @map,yc,xc,pix,pix*2,0) = 1 then begin

        StrCopy(fn,'/home/emm/temp/2.');
        if bmp then StrCat(fn,'bmp')
               else StrCat(fn,'png');

        if prj_bmp_saveAs(prj, fn,@map) = 1 then
        Caption:=fn
      end
    end;

    freeBitmap(map)
  end
end;

{$define dll_prj1}

{$ifdef dll_prj}
var
  prj: tdll_prj;
begin
  prj:=tdll_prj.Create;
  try
    if prj.Enabled then
    prj_tile(prj.Intf,bmp);
  finally
    prj.Free;
  end;
end;
{$endif}

{$ifdef dll_prj1}
var
  prj: Pointer;
begin
  prj:=prj_init;
  if Assigned(prj) then begin
    prj_tile1(prj,bmp);
    prj_done(prj)
  end;
end;
{$else}
var
  prj: idmwProject; guid: TGUID;
begin
  guid:=idmwProject;
  if dllGetInterface(@guid,prj) = S_OK then
  prj_tile(prj,bmp); prj:=nil;
end;
{$endif}

procedure TDfmTstu6.PrjChanged(Sender: TObject);
var
  fn: TShortstr;
begin
  if not fmaps.Get_Active_Path(fn) then
    Caption:='Map viewer'
  else
    Caption:=xStrNameExt(fn)
end;

procedure TDfmTstu6.ExitItemClick(Sender: TObject);
begin
  Close
end;

procedure TDfmTstu6.FileMenuClick(Sender: TObject);
begin
  CloseMap.Enabled:=fmaps.Count > 0;
end;

end.

