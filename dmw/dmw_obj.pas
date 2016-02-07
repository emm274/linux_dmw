unit dmw_obj; interface

uses
  Classes,
  SysUtils,
  otypes,
  xlist,
  vbanks,
  xrings,
  dmw_idc;

const
  Obj_Version: char4 = '0005'; hot_max = 32;

  Pgm_w = 24; Pgm_h = 24; Pgm_Len = (Pgm_w+3) * Pgm_h;

  obj_Tag_Max = 6;
  obj_Tag_Str: array[0..9] of string[7] =
  ('','','','','','','','','','');

  ObjFast_Max = 64;

type
  TObjIcon = array[0..Pgm_Len-1] of Byte;

  Obj_Rec = record
    Code: Longint; Pen,Style: Word; Flags,Tag: Byte;
    Color: Word; MsgP,PgmP: Longint; bln,Owner: Word;
    size1,size2,scale,idcInd,hfP: int;

    Color2,Color3: int;
    loc2,loc3,typ2,res: byte;
    Style2,Style3: Word
  end;

  PObjFast = ^TObjFast;
  TObjFast = array[0..ObjFast_Max-1,1..2] of Integer;

  TObjList = class(TVirtBank)

    function GetRecord(i: Integer; out buf): longint; override;
    function Get_Record(i: Integer; out Rec: Obj_Rec): Boolean;
    function vgm_Object(i: Integer): Boolean;

    procedure Fill_fast(fast: PObjFast);
    function Fill_Vgm_Index(Vgm: TIntegerList): int;

    function Obj_Index(code,loc: Integer;
                       out Rec: Obj_Rec;
                       fast: PObjFast): Integer;

    function Object_Blank(code,loc: Integer): Integer;
    procedure Shift_Blank(ind,delta: int);

    function Object_by_sign(Sign: Integer; out Code: Integer): Boolean;

    function Str_Object(i: Integer; out Rec: Obj_Rec; ps: PChar): PChar;
    function Get_Object(i: Integer; out Rec: Obj_Rec; ps: PChar): Boolean;

    function Str_Prompt(Str: PChar; i: Integer): PChar;
    function Ind_Prompt(Str: PChar; i: Integer): PChar;
    function Obj_Prompt(Str: PChar; Code,Loc: Integer): PChar;

    function Obj_Str(code,loc: Integer; ps: PChar): PChar;

    function Obj_String(code,loc: Integer): string;

    function Obj_Color(code,loc: int; out cl: int): int;

    function dde_Str(i: Integer; s: PChar): PChar;

    function Look_by_Name(it: Integer; ns: PChar): Integer;
    function Look_by_Code(it, code,loc: Integer): Integer;
  end;

  TObjMenu = class(TVirtTree)
    List: TObjList; Level: Integer;

    constructor Create(AList: TObjList);
    procedure Activate; override;

    function Create_Root: Boolean;

    function Ind_Child: Integer;
    function Get_Info(run: Longint): Integer;
    function Get_Item(out Rec: Obj_Rec; fn: PChar): Integer;
    procedure Update_Menu(ind,di: integer);
    procedure Delete_ind(ind: Integer);

    function ind_Indexof(Par,Ind: Longint): Longint;

    function Get_level(Par: int;
                       Items: TIntegerList;
                       dn: bool): int;

    procedure Goto_Root;
    procedure Goto_Down;

    function Locate_Object(ind: Integer): Integer;
    procedure Save_as_txt(Path: PChar);
    procedure Load_from_txt(Path: PChar);
  end;

  TMapObjects = class(TObjList)

    Menu: TObjMenu;

    constructor Create;
    destructor Destroy; override;

    function Version_Ok: Boolean;

    function Open_Obj(Path: PChar): Boolean;
    function Make_Obj(Path: PChar): Boolean;
    function Load_Obj(Path: PChar): Boolean;
    procedure Reset_Obj;

    function Load_hots(Hots: TIntegerList): Integer;
    function Save_hots(Hots: TIntegerList): Integer;

  private
    fctrl_rec: TSearchRec;

    function Get_nn_4: Integer;
    procedure Set_nn_4(nn: Integer);

    function Get_nn_1: Integer;
    procedure Set_nn_1(nn: Integer);

    function Get_nn_7: Integer;
    procedure Set_nn_7(nn: Integer);

    function Get_nn_9: Integer;
    procedure Set_nn_9(nn: Integer);

  public
    property ctrl_rec: TSearchRec read fctrl_rec;

    property nn_4: Integer read Get_nn_4
                           write Set_nn_4;

    property nn_9: Integer read Get_nn_9
                           write Set_nn_9;

    property nn_1: Integer read Get_nn_1
                               write Set_nn_1;

    property nn_7: Integer read Get_nn_7
                               write Set_nn_7;
  end;

  TFastObjects = class(TMapObjects)
    function StrCapt(code,loc: Integer; s: PChar): PChar;

    function Find_Object(code,loc: Integer;
                         out oRec: Obj_Rec): Integer;

    function Owner_Object(code,loc: Integer): Integer;

  private
    fast: TObjFast;
    fast_rec: TSearchRec;
  end;

  TEditObjects = class(TMapObjects)
  private
    fEditPath: TShortStr;

    function Get_EditPath: PChar;
    procedure Set_EditPath(Path: PChar);

  public
    property EditPath: PChar read Get_EditPath
                             write Set_EditPath;
  end;

  PObjScaleRec = ^TObjScaleRec;
  TObjScaleRec = record
    Id: Integer; sz1,sz2: SmallInt;
    scale: Integer
  end;

  TObjScaleList = class(TCustomList)
    constructor Create;
    procedure Clear; override;
    function LoadFrom(Obj: TObjList; APath: PChar): Integer;
    function xIndexof(Code,Loc: Integer): PObjScaleRec;
  private
    fPath: TShortstr;
  end;

  PLegendItem = ^TLegendItem;
  TLegendItem = record
    Code,Loc: Integer; Name: TNameStr1
  end;

  PLegendArray = ^TLegendArray;
  TLegendArray = array[0..31] of TLegendItem;

  TLegendList = class(TCustomList)
    constructor Create;

    function xAdd(Code,Loc: Integer; Name: PChar): Integer;

    function LoadFrom(Path: PChar): Integer;
    procedure SaveAs(Path: PChar);

    function Indexof(Code,Loc: Integer): Integer;
  end;

function is_s57_code(code: Integer): Boolean;

function obj_Vgm_Sign(Tag,Up,Color: Integer): Integer;

function obj_Str_Long(str: PChar): Boolean;
function obj_Str_Name(dst,str: PChar; bits: Integer): PChar;
function obj_Str_Skip(dst,str: PChar; Skip: Integer): PChar;

function obj_HtmlRef(const oRec: Obj_Rec): String;

function Layer_Contains(Layer,Code: Integer): Boolean;
function Objects_Equal(cod,loc, _cod,_loc: Integer): Boolean;

function OwnerToStr(own: Integer): string;
function StrToOwner(s: string): Integer;

function Transit_Loc(loc: Integer): Integer;

function Transit_Object(def_code,def_loc: Integer;
                        code,loc: Integer): Boolean;

function LocToStr(Loc,Len: Integer): string;

function dm_Change_Code(iLoc,oLoc: Integer): Integer;

procedure New_Icon(var ico: tObjIcon);

function StrToLoc(loc: string): Integer;
procedure dmw_Obj_Translate;

implementation

uses
  Math,convert,ofiles;

function is_s57_code(code: Integer): Boolean;
begin
  Result:=StrToCode('S5700000') div 100000 =
          code div 100000
end;

function obj_Vgm_Sign(Tag,Up,Color: Integer): Integer;
begin
  Result:=0; if Tag = 1 then begin
    if Color and $8000 <> 0 then
    Result:=Color and $7FFF
  end else
  if Tag in [2,6] then begin
    if Up and $40 <> 0 then
    Result:=Color
  end
end;

function obj_Str_Long(str: PChar): Boolean;
var
  p1,p2: PChar;
begin
  Result:=false;

  p1:=str; p2:=StrScan(p1,'\');
  if p2 <> nil then begin
    p1:=@p2[1]; p2:=StrScan(p1,'\');

    if p2 <> nil then
    Result:=Strlen(@p2[1]) > 0
  end
end;

function obj_Str_Name(dst,str: PChar; bits: Integer): PChar;
var
  p1,p2: PChar; i: Integer; buf: TShortstr;
begin
  Result:=nil;

  p1:=StrCopy(buf,str);
  p2:=StrScan(p1,'\');

  StrCopy(dst,'');

  for i:=1 to 4 do
  if p2 = nil then begin
    if bits and 1 <> 0 then
    Result:=sTxtCat(dst,p1);
    p1:=StrEnd(p1); Break
  end else

  if p2 <> nil then begin
    p2[0]:=#0;
    if bits and 1 <> 0 then
    Result:=sTxtCat(dst,p1);

    bits:=bits shr 1;
    p1:=@p2[1]; p2:=StrScan(p1,'\')
  end;

  if bits and 1 <> 0 then
  Result:=sTxtCat(dst,p1)
end;

function obj_Str_Skip(dst,str: PChar; Skip: Integer): PChar;
var
  p1,p2: PChar; i: Integer;
begin
  p1:=str; p2:=nil;
  for i:=1 to Skip do begin
    p2:=StrScan(p1,'\');
    if p2 = nil then Break;
    p1:=@p2[1];
  end;

  LStr(StrCopy(dst,p1));
  Result:=dst
end;

function obj_HtmlRef(const oRec: Obj_Rec): String;
begin
  with oRec do
  Result:=CodeToStr(Code)+'_'+IntToStr(Tag)
end;

function Layer_Contains(Layer,Code: Integer): Boolean;
var
  i,d_: Integer; 
begin
  Result:=false; d_:=1;

  for i:=1 to 7 do
  if Layer mod 10 = 0 then begin
    Layer:=Layer div 10;
    d_:=d_ * 10
  end;

  if Layer = 0 then Result:=true
  else
  if Layer = (Code div d_) then
  Result:=true
end;

function Objects_Equal(cod,loc, _cod,_loc: Integer): Boolean;
begin
  Result:=false; loc:=Transit_Loc(loc);

  if _loc = 0 then
    Result:=Layer_Contains(_cod,cod)
  else
    Result:=(cod = _cod) and (loc = _loc)
end;

function OwnerToStr(own: Integer): string;
begin
  Result:=System.Copy(CodeToStr(own*1000000),1,2)
end;

function StrToOwner(s: string): Integer;
begin
  Result:=StrToCode(s+'000000') div 1000000
end;

function dm_Change_Code(iLoc,oLoc: Integer): Integer;
begin
  Result:=0;

  if Transit_Loc(iLoc) = oLoc then
    Result:=iLoc
  else
  case iLoc of
1:  if oLoc in [2,4] then
    Result:=oLoc;

2:  if oLoc in [1,3] then
    Result:=oLoc;

3:  if oLoc in [1,2] then
    Result:=oLoc;

4:  if oLoc in [1,2] then
    Result:=oLoc;

  end
end;

function Transit_Loc(loc: Integer): Integer;
begin
  if loc > 100 then loc:=loc mod 100;

  Result:=loc mod 10;
  if loc in [6..7] then Dec(Result,4)
  else if loc in [5,8] then Result:=3
end;

function Transit_Object(def_Code,def_Loc: Integer;
                        Code,Loc: Integer): Boolean;
begin
  Result:=false;

  if def_Loc = 0 then
    Result:=Layer_Contains(def_Code,Code)
  else
  if (Code = def_Code) or (def_Code = 0) then

  if Loc = def_Loc then Result:=true else
  if Transit_loc(Loc) = def_Loc then Result:=true
end;

function LocToStr(Loc,Len: Integer): string;
begin
  Loc:=Transit_Loc(Loc); Result:=IntToStr(Loc);
  if Loc <= obj_Tag_Max then Result:=obj_Tag_Str[Loc];
  if Len > 0 then while length(Result) < 8 do
  Result:=Result+' ';
end;

procedure New_Icon(var ico: tObjIcon);
var
  i,k: Integer;
begin
  FillChar(ico,SizeOf(ico),7);

  i:=0; for k:=1 to 24 do begin
    ico[i]:=0; ico[i+1]:=24; ico[i+26]:=255;
    Inc(i,27)
  end
end;

function TObjList.GetRecord(i: Integer; out buf): longint;
var
  p: ^Obj_Rec;
begin
  Result:=t_ind + i*t_len;
  vm_Load(Result,buf,t_len);
  if t_len < Sizeof(Obj_Rec) then begin
    p:=@buf; p.size1:=0; p.size2:=0;
    p.scale:=0; p.IdcInd:=0;
  end
end;

function TObjList.Get_Record(i: Integer; out Rec: Obj_Rec): Boolean;
begin
  Result:=false; if vm_Active then
  if (i > 0) and (i <= t_cnt) then begin
    GetRecord(i,Rec); Result:=true
  end
end;

function TObjList.vgm_Object(i: Integer): Boolean;
var
  Rec: Obj_Rec;
begin
  Result:=false;
  if Get_Record(i,Rec) then with Rec do
  Result:=(Tag = 1) and (Color and $8000 <> 0)
end;

procedure TObjList.Fill_fast(fast: PObjFast);
var
  i,c: Integer; oRec: Obj_Rec;
begin
  for c:=0 to ObjFast_Max-1 do begin
    fast[c,1]:=t_cnt+1; fast[c,2]:=0
  end;

  if vm_Active then
  for i:=1 to t_cnt do begin
    GetRecord(i,oRec);

    c:=oRec.Code div 10000000;
    if c < ObjFast_Max then begin
      if fast[c,1] > i then fast[c,1]:=i;
      if fast[c,2] < i then fast[c,2]:=i
    end
  end
end;

function TObjList.Obj_Index(code,loc: Integer;
                            out Rec: Obj_Rec;
                            fast: PObjFast): Integer;
var
  i,top,bot: Integer; r: Obj_Rec;
begin
  Result:=0;

  loc:=Transit_loc(loc);

  FillChar(Rec,SizeOf(Obj_Rec),0);
  Rec.Code:=code; Rec.Tag:=loc;
  Rec.Owner:=code div 1000000;

  if vm_Active then begin

    top:=1; bot:=t_cnt;
    if fast <> nil then begin
      i:=code div 10000000;
      if (i >= 0) and (i < ObjFast_Max) then begin
        top:=fast[i,1]; bot:=fast[i,2]
      end
    end;

    for i:=top to bot do begin
      GetRecord(i,r); if r.Code = code then
      if (loc = 0) or (r.Tag = loc) then begin
        Rec:=r; Result:=i; Break
      end
    end

  end
end;

function TObjList.Object_Blank(code,loc: Integer): Integer;
var
  i: Integer; Rec: Obj_Rec;
begin
  Result:=0;
  loc:=Transit_loc(loc);

  if vm_Active then

  for i:=1 to t_cnt do begin
    GetRecord(i,Rec);

    if Rec.Code = code then
    if Rec.bln > 0 then

    if (loc = 0) or (Rec.Tag = loc) then
    begin Result:=Rec.bln; Break end
  end
end;

procedure TObjList.Shift_Blank(ind,delta: int);
var
  i,j: int; Rec: Obj_Rec;
begin
  if vm_Active then
  for i:=1 to t_cnt do begin
    GetRecord(i,Rec);

    if Rec.bln >= ind then begin
      j:=Rec.bln; j:=Max(0,j+delta);
      Rec.bln:=j; PutRecord(i,Rec)
    end
  end
end;

function TObjList.Object_by_sign(Sign: Integer; out Code: Integer): Boolean;
var
  i: Integer; Rec: Obj_Rec;
begin
  Result:=false; Code:=0;

  if vm_Active then
  for i:=1 to t_cnt do begin
    GetRecord(i,Rec);

    if Rec.Tag = 1 then
    if Rec.Color and $7FFF = Sign then begin
      Code:=Rec.Code; Result:=true; Break
    end
  end
end;

function TObjList.Fill_Vgm_Index(Vgm: TIntegerList): int;
var
  i,j: int; oRec: Obj_Rec;
begin
  Vgm.Clear;

  for i:=1 to t_cnt do begin
    GetRecord(i,oRec); j:=0;
    with oRec do if Tag = 1 then
    if Color and $8000 <> 0 then
    j:=Color and $7FFF;

    Vgm.AddItem(j)
  end;

  Result:=Vgm.Count
end;

function TObjList.Get_Object(i: Integer; out Rec: Obj_Rec; ps: PChar): Boolean;
begin
  FillChar(Rec,SizeOf(Rec),0);
  xStrCopy(ps,'');

  Result:=Get_Record(i,Rec);

  if Result and (Rec.MsgP > 0) then
  xStrPCopy(ps,WinString(vm_String(Rec.MsgP)))
end;

function TObjList.Str_Object(i: Integer; out Rec: Obj_Rec; ps: PChar): PChar;
begin
  Result:=nil; if Get_Object(i,Rec,ps) then
  if xStrLen(ps) > 0 then Result:=ps
end;

function TObjList.Str_Prompt(Str: PChar; i: Integer): PChar;
var
  oRec: Obj_Rec; name: string;
begin
  Result:=nil; StrCopy(Str,'');

  if Get_Record(i,oRec) then begin

    if oRec.MsgP > 0 then begin
      name:=WinString(vm_String(oRec.MsgP));

      if length(name) > 0 then
      Result:=StrPCopy(Str,name)
    end;

    if Strlen(Str) = 0 then
    Result:=StrPCopy(Str,xCodeToStr(oRec.Code,oRec.Tag))
  end
end;

function TObjList.Ind_Prompt(Str: PChar; i: Integer): PChar;
var
  oRec: Obj_Rec; name: string;
begin
  Result:=nil; StrCopy(Str,'');

  if Get_Record(i,oRec) then begin
    Result:=StrPCopy(Str,xCodeToStr(oRec.Code,oRec.Tag));

    if oRec.MsgP > 0 then begin
      name:=WinString(vm_String(oRec.MsgP));

      if length(name) > 0 then
      pStrCat(StrCat(Str,' - '),name)
    end
  end
end;

function TObjList.Obj_Prompt(Str: PChar; Code,Loc: Integer): PChar;
var
  name: TShortStr;
begin
  Result:=StrPCopy(Str,xCodeToStr(Code,Loc));

  if vm_Active then
  if Obj_Str(Code,Loc, name) <> nil then
  if StrLen(name) > 0 then

  StrCat(StrCat(Str,' - '),name)
end;

function TObjList.Obj_Str(code,loc: Integer; ps: PChar): PChar;
var
  oRec: Obj_Rec;
begin
  Result:=nil; xStrCopy(ps,''); if vm_Active then
  Result:=Str_Object(Obj_Index(code,loc,oRec,nil),oRec,ps)
end;

function TObjList.Obj_String(code,loc: Integer): string;
var
  s: TShortStr;
begin
  Result:=''; if vm_Active then
  if Obj_Str(code,loc,s) <> nil then
  Result:=StrPas(s)
end;

function TObjList.Obj_Color(code,loc: int; out cl: int): int;
var
  oRec: Obj_Rec;
begin
  Result:=0; cl:=0;
  loc:=Transit_loc(loc);
  Result:=Obj_Index(code,loc,oRec,nil);

  if Result > 0 then begin
    cl:=oRec.Color; if loc in [2,3] then
    tlong(cl).w[1]:=oRec.Pen
  end
end;

function TObjList.dde_Str(i: Integer; s: PChar): PChar;
var
  name: TShortStr; oRec: Obj_Rec;
begin
  Result:=nil;

  if Get_Record(i,oRec) then with oRec do begin
    Result:=StrPCopy(s, CodeToStr(Code)+' '+IntToStr(Tag));

    StrPCopy(name,WinString(vm_String(MsgP)));
    if StrLen(name) > 0 then StrLCat( StrCat(s,' '),name,255)
  end
end;

function TObjList.Look_by_Name(it: Integer; ns: PChar): Integer;
var
  i,cnt: Integer; oRec: Obj_Rec;
  s,t: TShortStr;
begin
  Result:=0;

  if xStrCopy(s,ns) <> nil then
  if xStrUpper(s) <> nil then begin

    if it > t_cnt then it:=0;
    cnt:=t_cnt; if it > 0 then Dec(cnt);

    for i:=1 to cnt do begin
      Inc(it); if it > t_cnt then it:=1;

      if Str_Object(it,oRec,t) <> nil then
      if xStrUpper(t) <> nil then

      if Str_Compare(s,nil, t,1) then
      begin Result:=it; Break end
    end

  end
end;

function TObjList.Look_by_Code(it, code,loc: Integer): Integer;
var
  i,cnt: Integer; oRec: Obj_Rec;
begin
  Result:=0;

  if it > t_cnt then it:=0;
  cnt:=t_cnt; if it > 0 then Dec(cnt);

  for i:=1 to cnt do begin
    Inc(it); if it > t_cnt then it:=1;

    if Get_Record(it,oRec) then begin
      if oRec.Code = code then
      if (loc = 0) or (oRec.Tag = loc) then
      begin Result:=it; Break end
    end
  end
end;

constructor TObjMenu.Create(AList: TObjList);
begin
  inherited Create(AList,20);
  List:=AList; Level:=0
end;

procedure TObjMenu.Activate;
begin
  Level:=0;

  if Root > 0 then begin
    Ring_Down(Root); if TopP <> 0 then
    RunP:=_Link(TopP)
  end
end;

function TObjMenu.Create_Root: Boolean;
var
  i: Integer;
begin
  Result:=false;
  if List.vm_Update then begin
    i:=0; Ins_Root(i,2);
    Ring_Down(Root); Level:=0;
    Result:=true
  end
end;

function TObjMenu.Ind_Child: Integer;
begin
  Result:=0; if RunP <> TopP then
  Result:=Get_Info(RunP)
end;

function TObjMenu.Get_Info(run: Longint): Integer;
begin
  Result:=0; _Info(run,Result,2)
end;

function TObjMenu.Get_Item(out Rec: Obj_Rec; fn: PChar): Integer;
begin
  FillChar(Rec,Sizeof(Rec),0);
  Result:=Ind_Child; if Result > 0 then
  List.Str_Object(Result,Rec,fn)
end;

procedure TObjMenu.Update_Menu(ind,di: Integer);

procedure Look_Childs(top, ind,di, lev: Integer);
var
  id,run: Integer;
begin
  top:=_Child(top);
  if top > 0 then begin

    run:=_Link(top);
    while run <> top do begin
      id:=Get_Info(run);
      if id >= ind then begin
        id:=iRange(id+di,1,List.t_cnt);
        NodeInfo(run,id,2)
      end;

      if lev < 32 then
      Look_Childs(run, ind,di, lev+1);

      run:=_Link(run)
    end

  end
end;

begin
  if Root > 0 then
  Look_Childs(Root, ind,di, 0)
end;

function TObjMenu.ind_Indexof(Par,Ind: Longint): Longint;
var
  top,run: Longint;
begin
  Result:=0;

  if (Ind > 0) and (Par > 0) then begin

    top:=_Child(Par);
    if top > 0 then begin

      run:=_Link(top);
      while run <> top do begin
        if Get_Info(run) = Ind then begin
          Result:=run; Break
        end;

        run:=_Link(run)
      end

    end
  end
end;

function TObjMenu.Get_level(Par: int;
                            Items: TIntegerList;
                            dn: bool): int;

function xlevel(sp,Par: int;
                Items: TIntegerList;
                dn: bool): int;
var
  top,run,ind: Longint;
begin
  Result:=0;
  if sp < 32 then
  if Par > 0 then begin
    top:=Get_Child(Par);

    if top > 0 then begin
      run:=_Link(top);
      while run <> top do begin
        if run < Root then Break;
        ind:=Get_Info(run);

        if ind > 0 then
        if ind <= List.t_cnt then begin
          Items.AddItem(ind);
          Inc(Result)
        end;

        if dn then
        xlevel(sp+1,run,Items,true);

        run:=_Link(run)
      end
    end
  end;
end;

begin
  Items.Clear;
  xlevel(0,Par,Items,dn);
  Result:=Items.Count
end;

procedure TObjMenu.Delete_ind(ind: Integer);

procedure Look_Childs(top, ind, lev: Integer);
var
  prd,run,nxt: Integer;
begin
  top:=_Child(top);
  if top > 0 then begin

    run:=_Link(top); prd:=top;
    while run <> top do begin
      nxt:=_Link(run);

      if Get_Info(run) = ind then
        NodeLink(prd,nxt)
      else begin
        prd:=run; if lev < 32 then
        Look_Childs(run, ind, lev+1)
      end;

      run:=nxt
    end

  end
end;

begin
  if Root > 0 then
  Look_Childs(Root, ind, 0);
  Update_Menu(ind+1,-1);
end;

procedure TObjMenu.Goto_Root;
begin
  while Level > 0 do begin Dec(Level);
    RunP:=_Child(TopP); TopP:=_TopP(RunP)
  end
end;

procedure TObjMenu.Goto_Down;
begin
  Ring_Down(RunP); RunP:=_Link(RunP);
  Inc(Level);
end;

function TObjMenu.Locate_Object(ind: Integer): Integer;

function Look_Childs(top,ind,lev: Integer): Integer;
var
  run: Longint;
begin
  Result:=0; top:=_Child(top);

  if top <> 0 then if lev <= 16 then begin
    run:=_Link(top); while run <> top do begin
      if Get_Info(run) = ind then begin
        Result:=lev; TopP:=top; RunP:=run; Break
      end;

      Result:=Look_Childs(run,ind,lev+1);
      if Result > 0 then Break;

      run:=_Link(run)
    end
  end
end;

begin
  Result:=0; if Root > 0 then
  Result:=Look_Childs(Root,ind,1)
end;

procedure TObjMenu.Save_as_txt(Path: PChar);

procedure dump_Childs(txt: TTextFile;
                      top,lev: Integer);
var
  run: Longint; ind: Integer;
  dn: Boolean; oRec: Obj_Rec;
begin
  top:=_Child(top); dn:=true;

  if top <> 0 then if lev <= 16 then begin
    run:=_Link(top); while run <> top do begin

      ind:=Get_Info(run);
      if List.Get_Record(ind,oRec) then begin

        if dn then begin
          dn:=false; txt.WriteStr('#')
        end;

        with oRec do
        txt.WriteStr(IntToStr(ind)+' '+
                     CodeToStr(Code)+' '+
                     IntToStr(Tag))
      end;

      dump_Childs(txt,run,lev+1);
      run:=_Link(run)
    end;

    if not dn then
    txt.WriteStr('@')
  end
end;

var
  txt: TTextFile;
begin
  if Root > 0 then begin
    txt:=TTextFile.Create;
    try
      if txt.Make(Path) then
      dump_Childs(txt,Root,1);
    finally
      txt.Free
    end
  end else

  FileErase(Path)
end;

procedure TObjMenu.Load_from_txt(Path: PChar);
var
  txt: TTextFile; oRec: Obj_Rec;
  ind,code,loc,rc: Integer; s: tShortStr;
begin
  txt:=TTextFile.Create;
  try
    if txt.Open(Path) then
    if Create_Root then begin

      TopP:=Root; RunP:=Root;

      while not txt.End_of_file do
      if txt.xStrRead <> nil then

      if StrToken(s,txt.str) <> nil then

      if StrComp(s,'#') = 0 then
        Ring_Down(RunP)
      else
      if StrComp(s,'@') = 0 then begin
        RunP:=_Child(TopP); TopP:=_TopP(RunP)
      end
      else begin
        val(s,ind,rc);

        if rc = 0 then
        if StrToken(s,txt.str) <> nil then
        if StrLen(s) = 8 then

        if IntToken(txt.str,loc) then begin

          code:=StrToCode(StrPas(s));
          if ind > List.t_cnt then ind:=0;

          if ind > 0 then begin
            List.Str_Object(ind,oRec,nil);
            if (oRec.Code <> code) or (oRec.Tag <> loc)
            then ind:=0
          end;

          if ind = 0 then
          ind:=List.Obj_Index(code,loc,oRec,nil);

          if ind > 0 then Ins_Node(ind,2)
        end
      end
    end;
  finally
    txt.Free
  end
end;

constructor TMapObjects.Create;
begin
  inherited Create(0,32,128,SizeOf(Obj_Rec));
  Menu:=TObjMenu.Create(Self);
end;

destructor TMapObjects.Destroy;
begin
  Menu.Free; inherited
end;

function TMapObjects.Version_Ok: Boolean;
var
  ver: char4;
begin
  t_len:=Sizeof(Obj_Rec);

  vm_Load(28,ver,4);
  Result:=ver = Obj_Version;

  if not Result then
  if ver = '0004' then begin
    t_len:=44; Result:=true
  end else
  if ver = '0003' then begin
    t_len:=40; Result:=true
  end else
  if ver = '0002' then begin
    t_len:=24; Result:=true
  end;

  if Result then Get_Status
end;

function TMapObjects.Open_Obj(Path: PChar): Boolean;
begin
  Result:=false;

  if Open(Path) then
  if not Version_Ok then vm_Close;

  if vm_Active then begin

    if FindFirst(Path,faArchive,fctrl_rec) <> 0 then
      Fillchar(fctrl_rec,Sizeof(fctrl_rec),0)
    else
      FindClose(fctrl_rec);

    Result:=true
  end
end;

function TMapObjects.Load_Obj(Path: PChar): Boolean;
begin
  Result:=false; vm_Edit:=true;

  if vm_LoadFrom(Path) > 0 then begin
    Get_Status; if not Version_Ok then
    Make_Obj(vm_Path); Result:=true
  end
  else Make_Obj(vm_Path);
end;

function TMapObjects.Make_Obj(Path: PChar): Boolean;
begin
  if Make(Path) then
  vm_Store(28,Obj_Version,4);
  Result:=vm_Active
end;

procedure TMapObjects.Reset_Obj;
begin
  vm_Close;
  if StrLen(vm_Path) > 0 then
  Make_Obj(vm_Path)
end;

function TMapObjects.Get_nn_4: Integer;
begin
  Result:=vm_Word(24);
  if Result = 0 then Result:=4
end;

procedure TMapObjects.Set_nn_4(nn: Integer);
begin
  vm_Store(24,nn,2)
end;

function TMapObjects.Get_nn_9: Integer;
begin
  Result:=vm_Word(26);
  if Result = 0 then Result:=9
end;

procedure TMapObjects.Set_nn_9(nn: Integer);
begin
  vm_Store(26,nn,2)
end;

function TMapObjects.Get_nn_1: Integer;
begin
  Result:=vm_Word(4);
  if Result = 0 then Result:=1
end;

procedure TMapObjects.Set_nn_1(nn: Integer);
begin
  vm_Store(4,nn,2)
end;

function TMapObjects.Get_nn_7: Integer;
begin
  Result:=vm_Word(6);
  if Result = 0 then Result:=7
end;

procedure TMapObjects.Set_nn_7(nn: Integer);
begin
  vm_Store(6,nn,2)
end;

function TMapObjects.Load_hots(Hots: TIntegerList): Integer;
var
  i,n,ind: int; ip: PIntegers;
  vm: TReadfile; fn: TShortStr;
begin
  Hots.Clear;

  xStrPath(fn,WorkDir,vm_Path,'.###');

  vm:=TReadfile.Create;
  try
    if vm.Open(fn) then begin
      n:=vm.Size div SizeOf(Integer);
      ip:=Pointer(vm.Buf);

      for i:=0 to n-1 do begin
        ind:=ip[i];

        if ind > 0 then
        if ind <= t_cnt then
        Hots.AddItem(ind);

        if Hots.Count = hot_max then Break
      end
    end;
  finally
    vm.Free
  end;

  Result:=Hots.Count
end;

function TMapObjects.Save_hots(Hots: TIntegerList): Integer;
var
  h,i,ind: Integer; fn: TShortStr;
begin
  Result:=0;
  xStrPath(fn,WorkDir,vm_Path,'.###');

  h:=FileCreate(StrPas(fn));
  if h > 0 then begin
    for i:=0 to Hots.Count-1 do begin
      ind:=Hots[i];

      if ind > 0 then
      if ind <= t_cnt then begin
        FileWrite(h,ind,SizeOf(Integer));
        Inc(Result)
      end
    end;

    FileClose(h)
  end;

  if Result = 0 then
  FileErase(fn)
end;

function TFastObjects.StrCapt(code,loc: Integer; s: PChar): PChar;
var
  Ind: Integer; oRec: Obj_Rec;
begin
  Result:=nil; if vm_Active then begin

    if (fast_rec.Time <> ctrl_rec.Time)
    or (fast_rec.Size <> ctrl_rec.Size)
    or (fast_rec.Name <> ctrl_rec.Name) then begin
      fast_rec:=ctrl_rec; Fill_fast(@fast);
    end;

    Ind:=Obj_Index(code,loc,oRec,@fast);
    if Ind >= 0 then
    Result:=Str_Object(Ind,oRec,s)
  end
end;

function TFastObjects.Find_Object(code,loc: Integer;
                                  out oRec: Obj_Rec): Integer;
begin
  Result:=0;
  Fillchar(oRec,Sizeof(oRec),0);

  if vm_Active then begin

    if (fast_rec.Time <> ctrl_rec.Time)
    or (fast_rec.Size <> ctrl_rec.Size)
    or (fast_rec.Name <> ctrl_rec.Name) then begin
      fast_rec:=ctrl_rec; Fill_fast(@fast);
    end;

    Result:=Obj_Index(code,loc,oRec,@fast)
  end
end;

function TFastObjects.Owner_Object(code,loc: Integer): Integer;
var
  oRec: Obj_Rec;
begin
  Result:=-1; if vm_Active then
  if Find_Object(code,loc,oRec) > 0 then
  Result:=oRec.owner;
end;

function TEditObjects.Get_EditPath: PChar;
begin
  Result:=fEditPath
end;

procedure TEditObjects.Set_EditPath(Path: PChar);
begin
  StrCopy(fEditPath,Path)
end;

constructor TObjScaleList.Create;
begin
  inherited Create(Sizeof(TObjScaleRec),256)
end;

procedure TObjScaleList.Clear;
begin
  inherited Clear;
  StrCopy(fPath,'')
end;

function TObjScaleList.LoadFrom(Obj: TObjList; APath: PChar): Integer;
var
  i,bx,cx: Integer; rec: Obj_Rec;
  r: TObjScaleRec;
begin
  if not xStrThis(fPath,APath) then begin
    Clear; StrCopy(fPath,APath);

    cx:=Obj.t_len;
    if cx = Sizeof(rec) then begin

      bx:=Obj.t_ind + cx;
      for i:=1 to Obj.t_cnt do begin
        bx:=Obj.vm_Load(bx,rec,cx);

        if rec.Tag in [1,2,4] then
        if (rec.size1 > 0) or (rec.size2 > 0)
        or (rec.scale > 0) then begin
          r.Id:=rec.Code * 8 + rec.Tag;
          r.sz1:=rec.size1; r.sz2:=rec.size2;
          r.scale:=rec.scale; Add(@r)
        end
      end
    end;

  end; Result:=Count
end;

function TObjScaleList.xIndexof(Code,Loc: Integer): PObjScaleRec;
begin
  Result:=id_Itemof(Code * 8 + Loc)
end;

constructor TLegendList.Create;
begin
  inherited Create(Sizeof(TLegendItem),256)
end;

function TLegendList.xAdd(Code,Loc: Integer; Name: PChar): Integer;
var
  r: TLegendItem;
begin
  r.Code:=Code; r.Loc:=Loc;
  StrLCopy(r.Name,Name,Sizeof(r.Name)-1);
  Result:=Add(@r)
end;

function TLegendList.LoadFrom(Path: PChar): Integer;
var
  txt: TTextfile;
  r: TLegendItem;
  s: TShortstr;
begin
  Clear;

  txt:=TTextfile.Create;
  try
    if txt.Open(Path) then
    while txt.xStrLine <> nil do
    if txt.x_Code(r.code) then
    if txt.x_Int(r.Loc) then begin

      txt.x_str(s);
      StrLCopy(r.Name,s,Sizeof(r.Name)-1);
      Add(@r)
    end
  finally
    txt.Free
  end;

  Result:=Count
end;

procedure TLegendList.SaveAs(Path: PChar);
var
  txt: TTextfile; I: Integer; P: PLegendItem;
begin
  txt:=TTextfile.Create;
  try
    if txt.Make(Path) then
    for I:=0 to Count-1 do begin P:=Items[I]; with P^ do
      txt.WriteStr(Format('%s %d "%s"',[CodeToStr(Code),Loc,Name]));
    end;

  finally
    txt.Free
  end
end;

function TLegendList.Indexof(Code,Loc: Integer): Integer;
var
  i: Integer; lp: PLegendArray;
begin
  Result:=-1; lp:=First;

  for i:=0 to Count-1 do begin
    if Objects_Equal(Code,loc, lp[0].Code,lp[0].Loc) then
    begin Result:=i; Break end; lp:=@lp[1]
  end
end;

function StrToLoc(loc: string): Integer;
var
  i: Integer;
begin
  Result:=0; for i:=1 to 4 do
  if obj_Tag_Str[i] = loc then begin
    Result:=i; Break
  end
end;

procedure dmw_Obj_Translate;
begin
  if not rus_interface then begin
    obj_Tag_Str[0]:='Menu';
    obj_Tag_Str[1]:='Sign';
    obj_Tag_Str[2]:='Line';
    obj_Tag_Str[3]:='Area';
    obj_Tag_Str[4]:='Text';
    obj_Tag_Str[5]:='Ellipse';
    obj_Tag_Str[6]:='Group';
  end
end;

initialization
  dmw_Obj_Translate;

end.