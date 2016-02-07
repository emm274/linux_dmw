unit cd_pcx; interface

uses
  Classes,
  otypes,xdib,xcards,
  dmw_link,dmw_prj,
  dmw_vars;

type
  TLinkCard = class(timCard)

    procedure link_Refresh(Sender: TObject);
    procedure link_Changed(Sender: TObject);

  protected
    x_Edit: tx_Edit;

  private
    fGlobe: TMapProject;

    fis_pcx_link: Boolean;
    fBackground: TShortstr;

    procedure SetBackground(APath: PChar);
    function GetBackground: PChar;

    procedure Set_Globe(AGlobe: TMapProject);

    procedure Link_delete(Ind: Integer);

  public
    link_T: tdmwLink;
    cd_Link: TLinkCard;
    Link_index: Integer;

    link_ctrl_Enabled: Longbool;
    link_ctrl_eps: Float;

    property Globe: TMapProject read fGlobe
                                write Set_Globe;

    property Background: PChar read GetBackground
                               write SetBackground;

    property is_pcx_link: Boolean write fis_pcx_link;

    procedure DrawOrients(dc: XCanvas); virtual;

    function Link_add(const g: TGauss; p_gauss: Integer): Boolean;

    function Link_del_loc(const lt,rb: TGauss; ind: Integer): Boolean;

    function Link_del_qu(Ind: Integer): Boolean;

    procedure Link_move(const p: TGauss);
    function Link_Escape: Boolean;
  end;

implementation

uses
  SysUtils,Graphics,
  ofiles,xpens,ipens,
  dm_dial;

procedure TLinkCard.SetBackground(APath: PChar);
begin
  StrCopy(fBackground,'');
  if Assigned(APath) then
  StrLCopy(fBackground,APath,255)
end;

function TLinkCard.GetBackground: PChar;
begin
  Result:=nil;
  if FileExist(fBackground) then
  Result:=fBackground
end;

procedure TLinkCard.Set_Globe(AGlobe: TMapProject);
begin
  fGlobe:=AGlobe;
  link_T:=fGlobe.Pcx_link;
  zoom:=-1; Card_Zoom(true);
end;

procedure TLinkCard.link_Refresh(Sender: TObject);
begin
  Card_Refresh(nil);
  if cd_Link <> nil then
  cd_Link.Card_Refresh(nil);
end;

procedure TLinkCard.link_Changed(Sender: TObject);
begin
  if Background = nil then
    Card_Refresh(nil)
  else begin
    if link_T.Count < 3 then
    Background:=nil;
    Card_Repaint(nil)
  end
end;

procedure TLinkCard.DrawOrients(dc: XCanvas);

procedure DrawPunkt(dc: XCanvas; i,d: Integer;
                    const g: TGauss);
var
  cl: Integer; p: TPoint;
begin
  ProjectLink(g,p);
  if dc.Contains_Point(p.x,p.y,8,8) then begin
    cl:=clYellow;
    if i = link_T.PointIndex then cl:=clRed;
    iLinkMarker(dc.Canvas,p.x,p.y,d,cl)
  end
end;

var
  i: Integer; lp: PLinkArray; g: TGauss;
begin
  if Assigned(dc) then
  if x_Edit.link_Enabled then begin

    lp:=link_T.First;
    for i:=1 to link_T.Count do begin

      if link_index = 0 then g:=lp[0].a
      else                   g:=lp[0].b;

      DrawPunkt(dc,i,2,g);

      if i mod 100 = 0 then
      if msg_Escape then Break;
      lp:=@lp[1]
    end;

    with x_Edit do
    if Link_i and (link_index+1) > 0 then
    DrawPunkt(dc,link_T.Count+1,4,Link_v[link_index])
  end
end;

function TLinkCard.Link_add(const g: TGauss; p_gauss: Integer): Boolean;
var
  i: int; p: TPoint;
begin
  Result:=false;

  with x_Edit do begin
    Link_v[link_index]:=g;
    Link_i:=Link_i or (link_index+1);

    if Link_i = 1 then
    if link_index = 0 then
    if p_gauss >= 0 then begin

      with fGlobe,Map do
      if Pcx_link.Change_prj(LG_T.sys) then begin

        if Pcx_link.Count < 2 then
          LG_T.Get_Link_Point(p_gauss,p)
        else begin
          pcx_to_dm(g.x,g.y,p);
          Align_grid(p)
        end;

        if Dial_xy(LG_T,p,false) then begin
          LG_t.L_to_Z(p,Link_v[1]); Link_i:=3
        end;
      end;

      if Link_i = 1 then Link_i:=0
    end;

    if Link_i = 3 then begin Link_i:=0;

      i:=link_T.add_ab(Link_v[0],Link_v[1]);
      if i >= 0 then begin
        link_T.Undo.Push(i,1);
        link_T.Refresh;
      end;

      cd_Link.Link_changed(nil);
      Result:=true
    end
  end;

  if Result then Link_changed(nil)
  else           Card_Refresh(nil)
end;

procedure TLinkCard.Link_delete(Ind: Integer);
begin
  link_T.Undo.Push(Ind-1,2);
  link_T.Del_ab(Ind);
  Card_Refresh(nil);

  if cd_Link <> nil then
  cd_Link.link_Changed(nil);
end;

function TLinkCard.Link_del_loc(const lt,rb: TGauss; ind: Integer): Boolean;
var
  i: Integer; p: TGauss;
begin
  Result:=false;
  i:=link_T.This_Pair(lt,rb,ind,p);

  if i > 0 then begin
    Link_delete(i);
    Result:=true
  end
end;

function TLinkCard.Link_del_qu(Ind: Integer): Boolean;
begin
  Result:=false;

  if Ind > 0 then
  if Ind <= link_T.Count then

  if dmw_Warning('Удалить точку?',691) then begin
    Link_delete(Ind); Result:=true
  end
end;

procedure TLinkCard.Link_move(const p: TGauss);
var
  ind: Integer;
begin
  x_Edit.link_i:=0;

  ind:=link_T.PointIndex-1;
  if Assigned(link_T[ind]) then begin

    link_T.Undo.Push(ind,0);

    if link_T.Move_Pair(ind+1,Link_index,p) then
    Link_changed(nil)
  end
end;

function TLinkCard.Link_Escape: Boolean;
begin
  Result:=false;
  if x_Edit.Link_i = link_Index+1 then begin
    x_Edit.Link_i:=0; Card_Refresh(nil);
    Result:=true
  end
end;

end.