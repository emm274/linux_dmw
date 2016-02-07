unit dmx_dm; interface

uses
  Classes,Math,activeX,
  otypes,xlist,xclasses,xintf,
  dmw_dm,dmw_cn,dmw_obj;

const
  undo_act1 = 1;
  undo_act2 = 2;

type
  tdm_Group = class
    constructor Create(dm: tdm_Map);
    destructor Destroy; override;

    function Get_cpos(i: int; out p: TPoint): bool;

    function Clear(upd: Boolean): Boolean;

    function Tools(p1,p2: Int64; pt: PPoint; cmd: int): Int64;

    function xDelete(I: Integer): Integer;
    function xIndexof(p: Int64): Integer;
    procedure xAppend(p1,p2: Int64);
    procedure xAdd(p1,p2: Int64; pc: PPoint);

    function Delete(i: Integer): Int64;
    function Exclude(i: Integer): Int64;

    function Cls_frame(const a,b: TPoint; lev: int): Boolean;

    function Contains_Point(const p: TPoint): Boolean;
    function Get_LLine(var v: VLLine): Integer;
    function Calc_Bound(exp,upd: Boolean): Boolean;

    function PredRecord: Integer;
    function NextRecord: Integer;

    procedure win_Update;
    procedure win_Refresh;
    procedure win_Next;
    procedure win_Pred;

    function CopyTo(List: TInt64List): Integer;
    function xCopyTo(List: TIntegerList): Integer;

  private
    dm_: tdm_Map;

    fFirst: TInt64List;
    fSecond: TInt64List;
    fPoints: TPointList;
    fItemIndex: Integer;

    Flt,Frb: TPoint;

    fActive: Boolean;
    fVisible: Boolean;

    fIsSecond: Boolean;
    fIsExplore: Boolean;

    fOnExplore: TOnUndoObject;

    fOnClear: TNotifyEvent;
    fUpdateEvent: TNotifyEvent;
    fRefreshEvent: TNotifyEvent;
    fNextEvent: TNotifyEvent;
    fPredEvent: TNotifyEvent;

    function Get_ItemIndex: Integer;
    procedure Set_ItemIndex(Ind: Integer);

    function Get_Object(I,J: Integer): Int64;
    function Get_Count: Integer;

    procedure Set_IsSecond(Value: Boolean);

    procedure Explore(p: Int64);

  public
    property Active: Boolean read fActive;
    property Visible: Boolean read fVisible write fVisible;

    property lt: TPoint read Flt;
    property rb: TPoint read Frb;

    property Objects[I,J: Integer]: Int64 read Get_Object;
    property Count: Integer read Get_Count;

    property Items: TInt64List read FFirst;

    property ItemIndex: Integer read Get_ItemIndex
                                write Set_ItemIndex;

    property IsSecond: Boolean read fIsSecond
                               write Set_IsSecond;

    property IsExplore: Boolean read fIsExplore
                                write fIsExplore;

    property OnExplore: TOnUndoObject write fOnExplore;

    property OnClear: TNotifyEvent write fOnClear;
    property UpdateEvent: TNotifyEvent write fUpdateEvent;
    property RefreshEvent: TNotifyEvent write fRefreshEvent;
    property NextEvent: TNotifyEvent write fNextEvent;
    property PredEvent: TNotifyEvent write fPredEvent;
  end;

  TObjectProc = procedure(P: Int64) of object;
  TShowObject = procedure(P: Int64; Cmd: Integer) of object;

  TOccupe = record
    P: Int64; Lev,Flags: int;
    Ind1: int; rmu1: float;
    Ind2: int; rmu2: float;
    C: VPoint
  end;

  TUndoRec = record
    Ptr: TInt64; Cmd: Word; Flags,Up: Byte;
    dRec: dm_Rec; vc1,vc2,own: Cardinal;
  end;

  PUndoAction = ^TUndoAction;
  TUndoAction = record
    Ptr: Int64; Act: int;
    Count: int; Items: LValues4;

    code1: int;
    i2: int; v2: VPoint;
    ctrl: Longbool
  end;

  tdm_Undo = class
    constructor Create(dm: tdm_Map; dll: Boolean);
    destructor Destroy; override;

    procedure Changed;
    procedure Clear;

    procedure xPush(p: Int64; cmd: Integer; undo: Boolean);
    procedure Push(p: Int64; cmd: Integer);
    procedure cn_Push(cn,id, cmd: Integer);

    function Pop(undo,ins: Boolean): Int64;
    function Delete(undo: Boolean): Int64;
    function Hide(undo: Boolean): Int64;

    function Empty(undo: Boolean): Boolean;

    function Get_Object(out dRec: dm_Rec): Int64;
    function pop_Object(p: Int64): Boolean;

  private
    dm_: tdm_Map;

    doc: IStorage;
    sf,mf: IStream;

    fdll_dm: Longbool;
    fEnabled: Longbool;
    fMoreFlag: Longbool;
    fPopFlag: Longbool;
    fMoreFlag1: Longbool;
    fMoreFlag2: Longbool;

    fAction: TUndoAction;

    fOnChanged: TNotifyEvent;

    function GetAction: TUndoAction;

    procedure SetMoreFlag(fl: Longbool);

    function x_Cmd(Cmd: Byte): Byte;

    function doc_Open(undo,push: Boolean): Boolean;
    procedure doc_Close;

    function doc_Object(undo: Boolean; out Rec: TUndoRec): Boolean;

    function doc_Path(fn: PChar): PChar;

  public
    property dll_dm: Longbool write fdll_dm;

    property Enabled: Longbool read FEnabled write FEnabled;
    property MoreFlag: Longbool read fMoreFlag write SetMoreFlag;
    property MoreFlag2: Longbool write fMoreFlag2;
    property PopFlag: Longbool read fPopFlag;

    property Action: TUndoAction read GetAction
                                 write fAction;

    property OnChanged: TNotifyEvent write fOnChanged;
  end;

  tdm_Edit = class(tdmw_Map)

    occupe_c,last_p,new_p: longint;
    update_p,forest_p: Int64;

    Undo: tdm_Undo; Group: tdm_Group;

    ClickList: TDmwClickList;

    SPolyBuf: TVLine;

    constructor Create;
    destructor Destroy; override;

    procedure Update_stat;

    procedure Close_Edit_Map;

    function Active_Map(Path: PChar): Boolean;

    function ClickDispObject(p: int64): int64;

    procedure Release_Object(p: Int64);

    procedure coObjects(p1,p2: Int64; typ: int);

    function Set_Occupe_a_Level: Boolean;

    procedure Lock_Occupe_c(fl: bool);
    procedure Set_Occupe_a_c(const C: VPoint);
    procedure Reseet_Occupe_a_c;

    procedure Set_Occupe_b_c(const C: VPoint);

    procedure Set_Occupe_a_r(i1: int; k1: float;
                             i2: int; k2: float);

    function Get_Occupe_Point(P: Int64;
                              out C: VPoint): Boolean;

    function Get_Occupe_Range(P: Int64; lp: PLLine): Boolean;

    procedure Free_Occupe_Point(P: Int64);

    function Get_Contour(p: Int64; lp: PLLine; hp: PIntegers): Integer;

    function obj_Record(i: Integer; out oRec: Obj_Rec): Integer;

    function mf_Object(p: Int64; loc: Integer;
                       lp: PLLine; hp: PIntegers): Integer;

    procedure For_all_layers(tool: TObjectProc);

    procedure Layer_up(p: Int64);
    procedure Layer_not(p: Int64);

    function Clear_fe(p: Int64; lp: PLLine): Boolean;

  private
    fOccupeList: TInt64List;

    fcoOccupeA: int64;
    fcoOccupeB: int64;
    fcoType: int;

    fOccupe_a: TOccupe;
    fOccupe_b: TOccupe;

    fOccupeLocked: Longbool;

    fLastTime: Longint;
    fStat: Array[1..6] of Longint;

    fOnOccupe_a: TObjectProc;
    fOnShowObject: TShowObject;

    fOnExplore: TNotifyEvent;

    fOnMoveObject: TObjectProc;

    function Get_Occupe_a: Int64;
    function Get_Occupe_b: Int64;

    function Get_Occupe_a_cn: Integer;

    procedure Set_Occupe_a(P: Int64);
    procedure Set_Occupe_b(P: Int64);

    procedure Undo_Object(P: Int64);
    procedure Click_Remove(P: Int64);

    function cn_callback(mf: pmf_buff): Integer;

  public
    property Undo_p: Int64 write Undo_Object;

    property Occupe_a: Int64 read Get_Occupe_a
                               write Set_Occupe_a;

    property Occupe_b: Int64 read Get_Occupe_b
                             write Set_Occupe_b;

    property Occupe_a_cn: Integer read Get_Occupe_a_cn;

    property Occupe_a_lev: Integer read FOccupe_a.Lev;

    property OccupeList: TInt64List read fOccupeList;

    property coOccupeA: int64 read fcoOccupeA;
    property coOccupeB: int64 read fcoOccupeB;
    property coType: int read fcoType;

    property OnOccupe_a: TObjectProc read fOnOccupe_a
                                     write fOnOccupe_a;

    property OnShowObject: TShowObject write fOnShowObject;

    property OnExplore: TNotifyEvent write fOnExplore;

    property OnMoveObject: TObjectProc write fOnMoveObject;
  end;

procedure UndoAction_off(var act: TUndoAction; ptr: int);

function Contains_Child(dm: tdm_Map; par: Int64;
                        lp,buf: PLLine): Boolean;

implementation

uses
  SysUtils,
  ofiles,storage,
  xline,xpoly;

procedure UndoAction_off(var act: TUndoAction; ptr: int);
var
  i,j: int;
begin
  for i:=0 to act.Count-1 do
  if act.Items[i] = ptr then begin
    Dec(act.Count);
    for j:=i to act.Count-1 do
    act.Items[j]:=act.Items[j+1]
  end
end;

constructor tdm_Group.Create(dm: tdm_Map);
begin
  inherited Create; dm_:=dm;

  fFirst:=TInt64List.Create;
  fFirst.Capacity:=1024*16;

  fSecond:=TInt64List.Create;
  fSecond.Capacity:=1024*16;

  fPoints:=TPointList.Create(4024*8);
  FItemIndex:=-1; FVisible:=true;
  fIsExplore:=true
end;

destructor tdm_Group.Destroy;
begin
  FSecond.Free;
  FFirst.Free;
  inherited
end;

function tdm_Group.Clear(upd: Boolean): Boolean;
begin
  Result:=Count > 0;

  fPoints.Clear;
  fFirst.Clear;
  fSecond.Clear;
  
  fItemIndex:=-1; fActive:=false;

  if upd and Result then begin
    win_Update; Explore(0)
  end;

  Notify(Self,fOnClear)
end;

function tdm_Group.Get_cpos(i: int; out p: TPoint): bool;
var
  pc: PPoint;
begin
  Result:=false;

  if fPoints.Count <> Count then
  fPoints.Clear;

  pc:=fPoints.Items[i];
  if Assigned(pc) then
  if pc.X <> INT_NAN then begin
    p:=pc^; Result:=true
  end
end;

function tdm_Group.Tools(p1,p2: Int64; pt: PPoint; cmd: int): Int64;
var
  i: int;
begin
  Result:=0;

  if cmd > 0 then begin
    i:=fFirst.Remove(p1);
    if i >= 0 then begin

      if i < fSecond.Count then
      if fFirst.Count = fSecond.Count-1 then
      fSecond.Delete(i);

      if i < fPoints.Count then
      if fFirst.Count = fPoints.Count-1 then
      fPoints.Delete(i);

      p1:=0;

      if fItemIndex = i then
      Dec(fItemIndex)
    end
  end;

  if (cmd <= 1) and (p1 > 0) then begin
    if fFirst.Count = 0 then
    FIsSecond:=p2 > 0;

    fFirst.Add(p1);
    fSecond.Add(p2);

    if pt = nil then
      fPoints.AddItem(INT_NAN,INT_NAN)
    else
      fPoints.Add(pt);

    Result:=p1
  end;

  fItemIndex:=Min(fItemIndex,fFirst.Count-1);
  
  if cmd > 0 then Calc_Bound(false,cmd = 3);
  if fIsExplore then Explore(p1)
end;

function tdm_Group.xDelete(I: Integer): Integer;
var
  p: Int64;
begin
  Result:=I;

  if I >= 0 then
  if I < FFirst.Count then begin
    p:=FFirst.Items[I];

    while (I >= 0) and (I < FFirst.Count) do begin
      Delete(I); I:=FFirst.IndexOf(p);
      if I < 0 then I:=FSecond.IndexOf(p);

      if I >= 0 then
      if I < Result then Dec(Result)
    end;

    if fIsExplore then Explore(p)
  end
end;

function tdm_Group.xIndexof(p: Int64): Integer;
begin
  Result:=FFirst.IndexOf(p)
end;

procedure tdm_Group.xAdd(p1,p2: Int64; pc: PPoint);
var
  c: TPoint;
begin
  fFirst.Add(p1); fSecond.Add(p2);

  if Assigned(pc) then c:=pc^
  else c:=Point(INT_NAN,0);
  fPoints.Add(@c)
end;

procedure tdm_Group.xAppend(p1,p2: Int64);
begin
  if FActive then begin
    if FFirst.Remove(p1) >= 0 then p1:=0
  end else
  if p2 = 0 then begin
    if FFirst.IndexOf(p1) >= 0 then p1:=0
  end;

  if p1 > 0 then begin
    FFirst.Add(p1);
    FSecond.Add(p2)
  end
end;

function tdm_Group.Cls_frame(const a,b: TPoint; lev: int): Boolean;
var
  i: int; p: Int64;
begin
  Result:=false; i:=0;

  while i < FFirst.Count do begin
    p:=FFirst.Items[i]; if p > 0 then
    if dm_.Level_Object(p) <= lev then
    if dm_.Port_Contains(a,b, p,0) then begin
      Delete(i); Result:=true
    end
    else Inc(i)
  end;

  if Result then Calc_Bound(true,true)
end;

function tdm_Group.Calc_Bound(exp,upd: Boolean): boolean;
var
  i: Integer; p: Int64; a,b: TPoint;
begin
  Result:=false;

  if dm_.Enabled_Map then
  for i:=0 to FFirst.Count-1 do begin
    p:=FFirst.Items[i]; if p > 0 then

    if Result then begin
      dm_.Port_Object(p, a,b);
      Add_lRect(Flt,Frb, a,b)
    end
    else begin
      dm_.Port_Object(p, Flt,Frb);
      Result:=true
    end
  end;

  fActive:=Result; fIsExplore:=true;
  if upd then win_Update else win_Refresh;
  if exp then Explore(0);
end;

function tdm_Group.Contains_Point(const p: TPoint): Boolean;
begin
  Result:=false; if FActive then
  Result:=PortContainsPoint(Flt,Frb, p.x,p.y)
end;

function tdm_Group.Get_LLine(var v: VLLine): Integer;
begin
  Result:=-1; if FVisible then
  if FActive then with v do begin
    Result:=Bound_to_LPoly(Flt,Frb,@Pol);
    N:=Result
  end
end;

function tdm_Group.Delete(i: Integer): Int64;
begin
  Result:=0;

  if i >= 0 then begin
    if i < fFirst.Count then begin
      Result:=fFirst.Items[i];
      fFirst.Delete(i);
    end;

    if i < fSecond.Count then
    fSecond.Delete(i);

    if i < fItemIndex then Dec(fItemIndex);

    if fItemIndex >= fFirst.Count then
    fItemIndex:=fFirst.Count-1;
  end
end;

function tdm_Group.Exclude(i: Integer): Int64;
begin
  if i < 0 then i:=FItemIndex;
  Result:=Delete(i);

  if Result <> 0 then begin
    Explore(Result); Calc_Bound(false,true)
  end
end;

function tdm_Group.PredRecord: Integer;
var
  Ind: Integer;
begin
  Ind:=FItemIndex-1;
  if Ind < 0 then Ind:=Count-1;
  ItemIndex:=Ind; Result:=FItemIndex
end;

function tdm_Group.NextRecord: Integer;
var
  Ind: Integer;
begin
  Ind:=FItemIndex+1;
  if Ind = Count then Ind:=0;
  ItemIndex:=Ind; Result:=FItemIndex
end;

function tdm_Group.Get_ItemIndex: Integer;
begin
  if FItemIndex >= FFirst.Count then
  FItemIndex:=-1; Result:=FItemIndex
end;

procedure tdm_Group.Set_ItemIndex(Ind: Integer);
begin
  FItemIndex:=Min( Max(Ind,-1),Count-1 )
end;

function tdm_Group.Get_Object(I,J: Integer): Int64;
begin
  Result:=0; if I >= 0 then

  case J of
0:  if I < FFirst.Count then
    Result:=FFirst[I];
1:  if I < FSecond.Count then
    Result:=FSecond[I];
  end
end;

function tdm_Group.Get_Count: Integer;
begin
  Result:=FFirst.Count
end;

procedure tdm_Group.Set_IsSecond(Value: Boolean);
begin
  FIsSecond:=false;
  if FSecond.Count > 0 then
  if FSecond.Items[0] <> 0 then
  FIsSecond:=true
end;

procedure tdm_Group.Explore(p: Int64);
begin
  if Assigned(fOnExplore) then
  fOnExplore(p,tx_select)
end;

procedure tdm_Group.win_Update;
begin
  if Assigned(FUpdateEvent) then
  FUpdateEvent(nil)
end;

procedure tdm_Group.win_Refresh;
begin
  if Assigned(FRefreshEvent) then
  FRefreshEvent(nil)
end;

procedure tdm_Group.win_next;
begin
  if Assigned(FNextEvent) then
  FNextEvent(nil)
end;

procedure tdm_Group.win_pred;
begin
  if Assigned(FPredEvent) then
  FPredEvent(nil)
end;

function tdm_Group.CopyTo(List: TInt64List): Integer;
var
  i: Integer;
begin
  for i:=0 to FFirst.Count-1 do
  List.Add(Objects[i,0]);

  Result:=List.Count
end;

function tdm_Group.xCopyTo(List: TIntegerList): Integer;
var
  i: Integer; p: TInt64;
begin
  for i:=0 to FFirst.Count-1 do begin
    p.x:=Objects[i,0]; if p.cn = 0 then
    List.AddItem(p.x)
  end; Result:=List.Count
end;

constructor tdm_Undo.Create(dm: tdm_Map; dll: Boolean);
begin
  inherited Create; dm_:=dm;
  fdll_dm:=dll; fEnabled:=true
end;

destructor tdm_Undo.Destroy;
begin
  fOnChanged:=nil;
  if not Fdll_dm then Clear;
  inherited Destroy
end;

procedure tdm_Undo.Changed;
begin
  if Assigned(fOnChanged) then
  fOnChanged(Self)
end;

procedure tdm_Undo.Clear;
var
  fn: TShortStr;
begin
  doc_Close;
  FileErase(doc_Path(fn));
  fAction.Ptr:=0
end;

function tdm_Undo.GetAction: TUndoAction;
begin
  Result:=fAction;
  fAction.Ptr:=0
end;

function tdm_Undo.doc_Open(undo,push: Boolean): Boolean;
var
  ic,sz: int; rec: TUndoRec;
  doc_fn,sf_fn,mf_fn,dm_fn: TShortStr;
begin
  Result:=false; doc_Close;

  if doc_Path(doc_fn) <> nil then begin

    if undo then begin
      StrPCopy(sf_fn,'undo.sf');
      StrPCopy(mf_fn,'undo.mf');
    end
    else begin
      StrPCopy(sf_fn,'indo.sf');
      StrPCopy(mf_fn,'indo.mf');
    end;

    if xStgIsStorageFile(doc_fn) then
    if xOpenStorage(doc_fn,ole_ReadWrite,doc) = S_OK then

    if StrFromStream(doc,'dm',dm_fn,0) <> nil then
    if xStrThis(dm_.Tree.vm_Path,dm_fn) then

    if xOpenStream(doc,sf_fn,true,sf) then
    if xOpenStream(doc,mf_fn,true,mf) then begin
      ic:=xSize(sf) div SizeOf(TUndoRec);
      sz:=ic * SizeOf(TUndoRec);
      xSeek_Bottom(sf);

      if sz = xSize(sf) then

      if push then
        Result:=true
      else
      if ic > 0 then begin
        Dec(ic);
        xSeek(sf,ic * SizeOf(TUndoRec));

        if fMoreFlag1 then begin
          while ic > 0 do begin
            if not xRead(sf,rec,SizeOf(rec)) then Break;
            if rec.Cmd and $80 = 0 then Break; Dec(ic);
            xSeek(sf,ic * SizeOf(TUndoRec));
          end;

          xSeek(sf,ic * SizeOf(TUndoRec))
        end;

        Result:=true
      end
    end;

    if not Result then
    if push then

    if undo then begin doc_Close;

      if xCreateStorage(doc_fn,doc) = S_OK then

      if xOpenStream(doc,sf_fn,true,sf) then
      if xOpenStream(doc,mf_fn,true,mf) then begin
        xStrAsStream(doc,'dm',dm_.Tree.vm_Path);
        Result:=true
      end
    end else

    if Assigned(doc) then begin
      sf:=nil; xDeleteElement(doc,sf_fn);
      mf:=nil; xDeleteElement(doc,mf_fn);

      if xOpenStream(doc,sf_fn,true,sf) then
      if xOpenStream(doc,mf_fn,true,mf) then

      Result:=true
    end
  end
end;

procedure tdm_Undo.doc_Close;
begin
  sf:=nil; mf:=nil; doc:=nil
end;

procedure tdm_Undo.SetMoreFlag(fl: Longbool);
begin
  fMoreFlag:=fl;
  fMoreFlag2:=false
end;

function tdm_Undo.x_Cmd(Cmd: Byte): Byte;
begin
  Result:=Cmd and $7F
end;

function tdm_Undo.doc_Object(undo: Boolean; out Rec: TUndoRec): Boolean;
begin
  Result:=false;
  FillChar(Rec,SizeOf(Rec),0);

  if doc_Open(undo,false) then
  Result:=xRead(sf,Rec,SizeOf(Rec));

  doc_Close
end;

function tdm_Undo.doc_Path(fn: PChar): PChar;
begin
  Result:=StrPath(fn,WorkDir,'undo.###')
end;

procedure tdm_Undo.xPush(p: Int64; cmd: Integer; undo: Boolean);
var
  x: TInt64; Node: dm_Node;
  Rec: TUndoRec; ve: tcn_rec;
begin
  x.x:=p;

  if FEnabled then
  if dm_.Is_dm then

  if dm_.Enabled_Map then
  if doc_Open(undo,true) then begin

    dm_.xGet_Node(p,Node);

    Fillchar(Rec,Sizeof(Rec),0);

    Rec.Ptr.x:=p; Rec.Cmd:=cmd;
    if undo and fMoreFlag then
    Rec.Cmd:=cmd or $80;

    Rec.Flags:=Node.Flags;
    Rec.Up:=Node.Tag;
    Rec.dRec:=Node.dRec;

    if x.cn = 0 then
    if cmd = undo_own then
    Rec.own:=dm_.Tree._Parent(p);

    if x.cn = cn_edge then
    if dm_.Tree.get_ve(x.id,@ve) then begin
      Rec.vc1:=ve.vc1; Rec.vc2:=ve.vc2;
    end;

    with Rec.dRec do begin
      if mind > 0 then
        mind:=dm_.mf.Stm_Copy(mind,mf)
      else
        mind:=-1;

      if hind > 0 then
        hind:=dm_.hf.Stm_Copy(hind,mf)
      else
        hind:=-1;
    end;

    xWrite(sf,Rec,SizeOf(Rec));

    if fMoreFlag2 then
    MoreFlag:=true
  end;

  doc_Close; Changed
end;

procedure tdm_Undo.Push(p: Int64; cmd: Integer);
begin
  if p > 0 then xPush(p,cmd,true)
end;

procedure tdm_Undo.cn_Push(cn,id, cmd: Integer);
var
  p: TInt64;
begin
  p.cn:=cn; p.id:=id; xPush(p.x,cmd,true)
end;

function tdm_Undo.Pop(undo,ins: Boolean): Int64;
var
  Rec: TUndoRec; dRec,tmp: dm_Rec;
  cmd: Integer; pos: Int64; x: TInt64;
begin
  Result:=0;

  if dm_.Is_dm then
  if dm_.Update_Map then

  if doc_Open(undo,false) then begin

    pos:=xPos(sf);
    if xRead(sf,Rec,SizeOf(Rec)) then begin

      dm_.EditMode:=true;

      cmd:=x_Cmd(Rec.Cmd); dRec:=Rec.dRec;

      if ins then begin
        dRec.mind:=0; dRec.hind:=0;
        if Rec.Ptr.cn = cn_object then
        Result:=dm_.Tree.Ins_Node(dRec,SizeOf(dRec))
      end else
      if cmd <> undo_add then begin
        Result:=Rec.Ptr.x;
        if Result > 0 then

        if cmd = undo_del then begin
          Result:=dm_.Return_Object(Result);
          dm_.Edit_Object(Result,0,tx_edit_add);
        end else
        if cmd = undo_own then begin
          if dm_.Ok_Object(Rec.own) then
          dm_.Tree.Mov_Child(Result,Rec.own)
        end
        else begin
          dm_.xGet_Object(Result,tmp);
          dRec.mind:=tmp.mind;
          dRec.hind:=tmp.hind
        end
      end;

      if cmd <> undo_del then
      if cmd <> undo_own then
      if Result > 0 then begin
        x.x:=Result;

        if x.cn = cn_object then begin
          dm_.Link_fe_cn(x.id,false);
          dm_.Tree.NodeTag(x.id,Rec.Up)
        end;

        dm_.xPut_Flags(Result,Rec.Flags);

        with dRec do begin
          mind:=dm_.mf.Stm_Paste(mind, mf,Rec.dRec.mind);
          hind:=dm_.hf.Stm_Paste(hind, mf,Rec.dRec.hind)
        end;

        dm_.xPut_Object(Result,dRec);

        if x.cn = cn_object then
          dm_.Link_fe_cn(x.id,true)
        else
        if x.cn = cn_edge then
          dm_.Update_ve(x.Id, Rec.vc1,Rec.vc2, nil,nil);

        if ins then cmd:=tx_edit_add else
        cmd:=tx_edit_mf+tx_edit_hf;

        dm_.Edit_Object(Result,0,cmd);
      end;

      sf.SetSize(pos); pos:=Rec.dRec.mind;
      if pos < 0 then pos:=Rec.dRec.hind;
      if pos >= 0 then mf.SetSize(pos);

      dm_.EditMode:=false
    end;
  end;

  doc_Close; Changed;
end;

function tdm_Undo.Delete(undo: Boolean): Int64;
var
  Rec: TUndoRec;
begin
  Result:=0; fPopFlag:=false;
  if doc_Object(undo,Rec) then begin
    fPopFlag:=Rec.Cmd and $80 <> 0;
    if x_Cmd(Rec.Cmd) = undo_add then
    Result:=Rec.Ptr.x
  end
end;

function tdm_Undo.Hide(undo: Boolean): Int64;
var
  Rec: TUndoRec;
begin
  Result:=0;
  if doc_Object(undo,Rec) then
  if x_Cmd(Rec.Cmd) <> undo_del then
  Result:=Rec.Ptr.x
end;

function tdm_Undo.Get_Object(out dRec: dm_Rec): Int64;
var
  Rec: TUndoRec;
begin
  Result:=0; FillChar(dRec,SizeOf(dRec),0);

  if doc_Object(true,Rec) then
  if x_Cmd(Rec.Cmd) <> undo_del then begin
    dRec:=Rec.dRec; Result:=Rec.Ptr.x
  end
end;

function tdm_Undo.pop_Object(p: Int64): Boolean;
var
  dRec: dm_Rec;
begin
  fMoreFlag1:=true;
  Result:=Get_Object(dRec) = p;
  fMoreFlag1:=false;
end;

function tdm_Undo.Empty(undo: Boolean): Boolean;
begin
  Result:=not doc_Open(undo,false);
  doc_Close
end;

constructor tdm_Edit.Create;
begin
  inherited Create(1);

  group:=tdm_Group.Create(Self);
  undo:=tdm_Undo.Create(Self,false);

  OnUndoObject:=undo.Push;

  ClickList:=TDmwClickList.Create;
  ClickList.Duplicates:=true;

  fOccupeList:=TInt64List.Create;

  sPolyBuf.N:=-1; 

  pack_enabled:=false;
  Is_Editor:=true
end;

destructor tdm_Edit.Destroy;
begin
  fOccupeList.Free;
  ClickList.Free;

  undo.Free;
  group.Free;

  inherited
end;

procedure tdm_Edit.Update_stat;
var
  i,k: Integer;
  tmp: Array[1..6] of Longint;
begin
  if Enabled_Map then begin

    if Get_stat(@tmp,6) > 0 then
    fStat[6]:=tmp[6];

    k:=0; for i:=1 to 4 do
    if fStat[i] <> tmp[i] then Inc(k);

    if k > 0 then
    if Update_Map then 
    Put_stat(@fStat,6);

    Close_Map; 
  end
end;

procedure tdm_Edit.Close_Edit_Map;
begin
  Update_stat; 

  Is_Enabled:=false; fLastTime:=0;
  Close_Map; undo.Clear; id_Index.Clear;
  occupe_a:=0; occupe_b:=0; occupe_c:=0;
  last_p:=0; new_p:=0; update_p:=0; forest_p:=0;
  Group.Clear(true);

  ClickList.Clear;
  ClickList.Changed;

  if Assigned(fOnExplore) then
  fOnExplore(Self); Is_Enabled:=true;
end;

function tdm_Edit.Active_Map(Path: PChar): Boolean;
var
  rc: int; tmp: Array[1..6] of int;
begin
  Close_Edit_Map;
  Result:=Open_Map(Path,false);

  if Result then
  if Assigned(Path) then begin

    Fillchar(fStat,Sizeof(fStat),0);
    fStat[6]:=120; rc:=Get_stat(@tmp,6);
    if rc > 0 then Move(tmp,fStat,rc*4);
    fLastTime:=0;

    if Assigned(fOnExplore) then
    fOnExplore(Self)
  end
end;

procedure tdm_Edit.Click_Remove(p: Int64);
begin
  if ClickList.Remove(p) >= 0 then
  ClickList.Changed;
end;

function tdm_Edit.ClickDispObject(p: int64): int64;
begin
  Result:=p;
  if TInt64(p).cn = 0 then
  if ClickList.GetLevel(p) = 3 then begin
    p:=GetParent(p); if p > 0 then
    if Tag_Object(p) = 3 then
    Result:=p
  end
end;

procedure tdm_Edit.Release_Object(p: Int64);
begin
  if occupe_a = p then occupe_a:=0;
  if occupe_b = p then occupe_b:=0;
  if occupe_c = p then occupe_c:=0;
  if update_p = p then update_p:=0;
  if forest_p = p then forest_p:=0;
end;

function tdm_Edit.Get_Occupe_a: Int64;
begin
  Result:=Abs(FOccupe_a.P)
end;

function tdm_Edit.Get_Occupe_b: Int64;
begin
  Result:=Abs(FOccupe_b.P)
end;

procedure tdm_Edit.Set_Occupe_a(P: Int64);
var
  o,x: TInt64;
begin
  OccupeList.Clear;

  x.x:=P; new_P:=0;
  o.x:=FOccupe_a.P;

  fOccupe_a.P:=0;
  fOccupe_a.Lev:=-1;

  if not fOccupeLocked then
  fOccupe_a.Flags:=0;

  if x.cn <> cn_aggr then
  fOccupe_a.P:=P;

  if P > 0 then
  if P <> fcoOccupeA then
  if P <> fcoOccupeB then begin
    fcoOccupeA:=0;
    fcoOccupeB:=0;
  end;

  if Assigned(fOnOccupe_a) then
  if (o.x > 0) or (x.x > 0) then
  fOnOccupe_a(P)
end;

procedure tdm_Edit.Set_Occupe_b(P: Int64);
var
  x: TInt64;
begin
  x.x:=P; new_P:=0;

  fOccupe_b.P:=0;
  fOccupe_b.Lev:=-1;
  fOccupe_b.Flags:=0;

  if x.cn <> cn_aggr then
  FOccupe_b.P:=P;
end;

function tdm_Edit.Get_Occupe_a_cn: Integer;
var
  p: TInt64;
begin
  p.x:=Occupe_a; Result:=p.cn
end;

procedure tdm_Edit.Undo_Object(P: Int64);
begin
  if P = Occupe_a then Occupe_a:=0 else
  if P = Occupe_b then Occupe_b:=0
end;

procedure tdm_Edit.coObjects(p1,p2: Int64; typ: int);
begin
  if p1 < 0 then begin
    p1:=Occupe_a; p2:=Occupe_b
  end;

  fcoOccupeA:=p1;
  fcoOccupeB:=p2;
  fcoType:=typ
end;

function tdm_Edit.Set_Occupe_a_Level: Boolean;
begin
  Result:=false;
  if Occupe_a > 0 then
  if Enabled_Map then begin
    FOccupe_a.Lev:=Tree._Level(Occupe_a);
    Result:=true
  end
end;

procedure tdm_Edit.Lock_Occupe_c(fl: bool);
begin
  fOccupeLocked:=fl;
  if fl then fOccupe_a.Flags:=0;
end;

procedure tdm_Edit.Set_Occupe_a_c(const C: VPoint);
begin
  fOccupe_a.Flags:=fOccupe_a.Flags or 1;
  fOccupe_a.C:=C
end;

procedure tdm_Edit.Reseet_Occupe_a_c;
begin
  with fOccupe_a do
  if Flags and 1 <> 0 then
  Flags:=Flags xor 1
end;

procedure tdm_Edit.Set_Occupe_b_c(const C: VPoint);
begin
  fOccupe_b.Flags:=fOccupe_b.Flags or 1;
  FOccupe_b.C:=C
end;

procedure tdm_Edit.Set_Occupe_a_r(i1: int; k1: float;
                                  i2: int; k2: float);
var
  ax: int;
begin
  ax:=fOccupe_a.Flags and 1;
  fOccupe_a.Ind1:=i1;
  fOccupe_a.rmu1:=k1;
  fOccupe_a.Ind2:=i2;
  fOccupe_a.rmu2:=k2;
  ax:=ax or 2; fOccupe_a.Flags:=ax
end;

function tdm_Edit.Get_Occupe_Point(P: Int64;
                                   out C: VPoint): Boolean;
begin
  Result:=false;

  if (P = Occupe_a) and ((fOccupe_a.Flags and 1) <> 0) then
    begin C:=FOccupe_a.C; Result:=true end
  else
  if (P = Occupe_b) and ((fOccupe_b.Flags and 1) <> 0) then
    begin C:=FOccupe_b.C; Result:=true end
end;

function tdm_Edit.Get_Occupe_Range(P: Int64; lp: PLLine): Boolean;
var
  r: TOccupe;
begin
  Result:=false;

  if (P = Occupe_a) and ((fOccupe_a.Flags and 2) <> 0) then
    begin r:=fOccupe_a; Result:=true end
  else
  if (P = Occupe_b) and ((fOccupe_b.Flags and 2) <> 0) then
    begin r:=fOccupe_b; Result:=true end;

  if Result then begin
    Result:=false;

    if Enabled_Map then
    if xGet_mf(P,lp,nil) > 0 then
    if Poly_range(lp,r.Ind1,r.rmu1,r.Ind2,r.rmu2) > 0 then
    Result:=true
  end
end;

procedure tdm_Edit.Free_Occupe_Point(P: Int64);
begin
  if P = Occupe_a then fOccupe_a.P:=P else
  if P = Occupe_b then fOccupe_b.P:=P
end;

function tdm_Edit.cn_callback(mf: pmf_buff): Integer;
begin
  if mf.lp.N > 0 then mf.is_break:=true
end;

function tdm_Edit.Get_Contour(p: Int64; lp: PLLine; hp: PIntegers): Integer;
var
  x: TInt64; mf: TPolylist;
begin
  Result:=-1; x.x:=p; lp.N:=-1;

  if (x.cn = cn_object)
  and (Tag_Object(p) > 21) then begin

    mf:=TPolylist.Create;
    try
      if cn_poly(p,mf,lp,hp) > 0 then begin
        lp.N:=mf.get_line(0,lp,hp,LPolyMax);
        Result:=lp.N
      end;
    finally
      mf.Free
    end
  end
  else Result:=xGet_mf(p,lp,hp)
end;

function tdm_Edit.obj_Record(i: Integer; out oRec: Obj_Rec): Integer;
var
  dRec: dm_Rec;
begin
  Result:=0; FillChar(oRec,SizeOf(oRec),0);

  if Enabled_Map then
  if Open_Objects then begin
    if Obj.Get_Record(i,oRec) then Result:=i;
    if Occupe_a > 0 then with dRec do begin
      Tag:=Transit_Loc(Get_Object(Occupe_a,dRec));
      i:=Obj.Obj_Index(Code,Tag,oRec,nil);
      if i > 0 then Result:=i
    end
  end; Obj.vm_Close
end;

function tdm_Edit.mf_Object(p: Int64; loc: Integer;
                            lp: PLLine; hp: PIntegers): Integer;
begin
  Result:=-1;

  if Assigned(lp) then
  if lp.N >= 0 then

  if edit_mf(p) then begin
    undo.Push(p,undo_mov);
    xUpdate_mf(p,loc,lp,hp);
    Edit_Object(p,0,tx_edit_mf);
  end;

  Result:=lp.N
end;

procedure tdm_Edit.For_all_layers(tool: TObjectProc);
var
  top,run: longint;
begin
  if Enabled_Map then begin

    top:=Tree._Child(Tree.Root);
    if top > 0 then begin
      run:=Tree._Link(top);
      while run <> top do begin
        tool(run); run:=Tree._Link(run)
      end
    end

  end
end;

procedure tdm_Edit.Layer_up(p: Int64);
begin
  Set_Flag(p,fl_draw,false)
end;

procedure tdm_Edit.Layer_not(p: Int64);
begin
  Xor_Flag(p,fl_draw)
end;

function tdm_Edit.Clear_fe(p: Int64; lp: PLLine): Boolean;
var
  i,id,loc: Integer; v: tcn_rec;
begin
  Result:=false;

  if Enabled_Map then begin
    loc:=Tag_Object(p);
    if loc in [21..23] then begin

      i:=0;
      while i <= lp.N do begin
        id:=lp.Pol[i].x;

        if loc = 21 then begin

          if not Tree.get_vc(id,@v) then begin
            Poly_Cut(lp,i,1); Dec(i); Result:=true
          end
        end else
        if not Tree.get_ve(id,@v) then begin
          Poly_Cut(lp,i,1); Dec(i); Result:=true
        end;

        Inc(i)
      end
    end;

    if Result then
    if Update_Map then begin
      EditMode:=true;
      xUpdate_mf(p,0,lp,nil);
      Edit_Object(p,0,tx_edit_mf);
      EditMode:=false;
    end
  end
end;

function Contains_Child(dm: tdm_Map; par: Int64;
                        lp,buf: PLLine): Boolean;
var
  top,run: int; tmp: PLLine; x: TInt64;
begin
  Result:=false;

  x.x:=par;
  if x.cn = 0 then begin
    top:=dm.Tree._Child(par);
    if top > 0 then with dm do begin

      run:=Tree._Link(top);
      if run <> top then begin

        tmp:=nil;
        if buf = nil then begin
          tmp:=Alloc_LPolyBuf; buf:=tmp
        end;

        if Assigned(buf) then
        while run <> top do begin

          if Inside_Object(par,run) then
          if Get_Poly(run,buf,LPolyMax) > 0 then

          if xPolygonContainsPolyLine(buf,lp) then
          begin Result:=true; Break end;

          run:=Tree._Link(run)
        end;

        xFreePtr(tmp)
      end
    end
  end
end;

end.