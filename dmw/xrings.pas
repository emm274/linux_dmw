unit xrings; interface

uses
  Classes,activeX,otypes,xvirts;

const
  RING_FIRST = $80;

type
  NodeRec = record
    Len: word; Flag,Tag: byte;
    Link,Child: Longint; Acc: word
  end;

  TVirtTree = class

    Root,TopP,RunP: Longint;

    constructor Create(Mem: TVirtMem; top: Integer);
    destructor Destroy; override;

    procedure ActivateEvent(Sender: TObject);
    procedure CloseEvent(Sender: TObject);

    procedure Activate; virtual;

    procedure cls_Childs;

    procedure Get_Position(out P: TPoint);
    procedure Set_Position(const P: TPoint);

    procedure NodeFlags(P: Longint; F: byte);
    procedure NodeTag  (P: Longint; T: byte);
    procedure NodeLink (P: Longint; Next: Longint);
    procedure NodeChild(P: Longint; Dn: Longint);
    procedure NodeInfo (P: Longint; var buf; len: Integer);

    procedure SetFlags(P: Longint; bit: byte; up: Boolean);

    function _Verify(P: Longint): Boolean;

    function _Length(P: Longint): Integer;
    function _Flags (P: Longint): byte;
    function _Tag   (P: Longint): byte;
    function _Link  (P: Longint): Longint;
    function _Child (P: Longint): Longint;
    function _Parent(P: Longint): Longint;
    procedure _Info (P: Longint; var buf; len: Integer);
    procedure _Node (P: Longint; var Node: NodeRec);

    function This_top(P: Longint): Boolean;

    function _Level(P: Longint): Integer;

    function _Pred(Run: Longint): Longint;
    function _TopP(Run: Longint): Longint;
    function _xPred(Prd,Run: Longint): Longint;
    function _fPred(Prd,Run: Longint): Longint;

    function _RunP(Top,Ind: Longint): Longint;

    function Get_child(P: Longint): Longint;

    function Get_RunP(Top,Ind: Longint;
                      out Ptr: Longint): Boolean;

    function _Jump(Run: Longint): Longint;
    function _Goto(Run: Longint): Longint;
    function _Down(Run: Longint): Longint;
    function Last_RunP: Longint;

    procedure Ins_Frst(Up: Longint);
    procedure Del_Frst(P: Longint);
    function Ins_Root(var info; size: Integer): Longint;
    function Ins_Node(var info; size: Integer): Longint;
    function Del_Node(up: Boolean): Boolean;

    function Pop_Node(prd,run: Longint): Longint;
    procedure Mov_Last(run: Longint);
    procedure Up_Node(par,run: Longint);
    procedure Dn_Node(run,par: Longint);

    function Mov_Node(prd,mov,run: Longint): Longint;
    function Lnk_Node(run,lnk: Longint): Longint;

    procedure Mov_Child(run,dst: Longint);

    procedure Mov_Childs(dst,src: Longint);
    procedure Up_Childs(par: Longint);

    function Ring_Down(Run: Longint): Boolean;
    function Ring_Upper: Longint;

    function Ring_Dn: Boolean;
    function Ring_Up: Boolean;

    function Get_RunI: Integer;

    function Is_Childs(par: Longint): Integer;

    function IsChild(par,child: Longint): Boolean;

    function Get_Childs(par: Longint;
                        Childs: PIntegers;
                        MaxLen: Integer): Integer;

    function group_pline(p1,p2: Longint): Integer;

  private
    vm: TVirtMem;
    vm_Root: Integer;

    fCleanMode: Boolean;

    function GetEnabled: Boolean;

    procedure InitNode(P: Longint;
                       ALen,AFlags,ATag: Integer;
                       Nxt,Dn: Longint);

  public
    property Enabled: Boolean read GetEnabled;
    property CleanMode: Boolean write fCleanMode;
    property vm_Virt: TVirtMem read vm write vm;
  end;

type
  TCopyTree = class
    constructor Create(Src,Dst: TVirtTree;
                       ALen: Integer);

    function Copy_Node(Inp_P,SP: Longint): Longint;
    procedure Copy_Ring(Inp_P,SP: Longint);
    procedure Copy_Tree;

    function Enabled_Node(Inp_P,SP: Longint): Boolean; virtual;
    procedure Update_Info(Inp_P,Dst_P: Longint); virtual;

  private
    FSrc,FDst: TVirtTree;
    fLevel: Integer;
    Len: Integer;
    Wait_Len: Integer;
    buf: TLongBuf;

  public
    property Level: Integer read fLevel;
  end;

implementation

constructor TVirtTree.Create(Mem: TVirtMem; top: Integer);
begin
  inherited Create;
  vm:=Mem; vm_Root:=top;
  vm.OnActivate:=ActivateEvent;
  vm.OnClose:=CloseEvent;
end;

destructor TVirtTree.Destroy;
begin
  if Assigned(vm) then begin
    vm.OnActivate:=nil;
    vm.OnClose:=nil;
  end
end;

function TVirtTree.GetEnabled: Boolean;
begin
  Result:=vm.vm_Active
end;

procedure TVirtTree.ActivateEvent(Sender: TObject);
begin
  Root:=0; TopP:=0; RunP:=0;
  Root:=vm.vm_Long(vm_Root);
  Activate
end;

procedure TVirtTree.CloseEvent(Sender: TObject);
begin
  Root:=0; TopP:=0; RunP:=0
end;

procedure TVirtTree.Activate;
begin
end;

procedure TVirtTree.cls_Childs;
begin
  if Ring_Down(Root) then begin
    NodeLink(TopP,TopP); RunP:=TopP
  end
end;

procedure TVirtTree.Get_Position(out P: TPoint);
begin
  P.X:=TopP; P.Y:=RunP
end;

procedure TVirtTree.Set_Position(const P: TPoint);
begin
  TopP:=P.X; RunP:=P.Y
end;

procedure TVirtTree.InitNode(P: Longint;
                             ALen,AFlags,ATag: Integer;
                             Nxt,Dn: Longint);
var
  Node: NodeRec;
begin
  with Node do begin
    Len:=ALen; Flag:=AFlags; Tag:=ATag;
    Link:=Nxt; Child:=Dn
  end;

  vm.vm_Store(P,Node,12)
end;

procedure TVirtTree.NodeFlags(P: Longint; F: byte);
begin
  vm.vm_Store(P+2,F,1)
end;

procedure TVirtTree.NodeTag(P: Longint; T: byte);
begin
  vm.vm_Store(P+3,T,1)
end;

procedure TVirtTree.NodeLink(P: Longint; Next: Longint);
begin
  vm.vm_Store(P+4,Next,4)
end;

procedure TVirtTree.NodeChild(P: Longint; Dn: Longint);
begin
  vm.vm_Store(P+8,Dn,4)
end;

procedure TVirtTree.NodeInfo(P: Longint; var buf; len: Integer);
begin
  if len > 0 then
  vm.vm_Store(P+12,buf,len)
end;

procedure TVirtTree.SetFlags(P: Longint; bit: byte; up: Boolean);
var
  ax: byte;
begin
  ax:=_Flags(P); if up then ax:=ax or bit else
  ax:=ax and (bit xor $FF); NodeFlags(P,ax)
end;

function TVirtTree._Verify(P: Longint): Boolean;
begin
  Result:=false;
  if P > Root then
  if P+12 <= vm.vm_Ind then
  if P+12+_Length(P) <= vm.vm_Ind then
  Result:=true
end;

function TVirtTree._Length(P: Longint): Integer;
begin
  Result:=vm.vm_Word(P)
end;

function TVirtTree._Flags(P: Longint): byte;
begin
  vm.vm_Load(P+2,Result,1)
end;

function TVirtTree._Tag(P: Longint): byte;
begin
  vm.vm_Load(P+3,Result,1)
end;

function TVirtTree._Link(P: Longint): Longint;
begin
  Result:=vm.vm_Long(P+4);
  if Result > vm.vm_Ind-12 then
  Result:=0
end;

function TVirtTree._Child(P: Longint): Longint;
begin
  Result:=vm.vm_Long(P+8);
  if Result > vm.vm_Ind-12 then
  Result:=0
end;

function TVirtTree._Parent(P: Longint): Longint;
begin
  Result:=_Child(_TopP(P));
  if Result = 0 then Result:=Root
end;

procedure TVirtTree._Info(P: Longint; var buf; len: Integer);
begin
  vm.vm_Load(P+12,buf,len)
end;

procedure TVirtTree._Node(P: Longint; var Node: NodeRec);
begin
  vm.vm_Load(P,Node,SizeOf(Node))
end;

function TVirtTree.This_top(P: Longint): Boolean;
var
  Node: NodeRec;
begin
  Result:=false;
  vm.vm_Load(P,Node,12);

  if Node.Flag and RING_FIRST <> 0 then
  Result:=Node.Len = 0
end;

function TVirtTree._Level(P: Longint): Integer;
begin
  Result:=0;
  while (P <> Root) and (P > 0) do begin
    P:=_Parent(P); Inc(Result)
  end
end;

function TVirtTree._Pred(Run: Longint): Longint;
var
  nxt,top: Longint;
begin
  nxt:=Run;

  top:=0; repeat
    Result:=nxt; nxt:=_Link(nxt);
    if (nxt <= 0) or (nxt = top) then
    begin Result:=0; Break end;
    if top = 0 then top:=nxt
  until nxt = Run
end;

function TVirtTree._TopP(Run: Longint): Longint;
var
  tmp: Longint;
begin
  if (Run > 0) and (Run <> Root) then begin
    tmp:=Run; while not This_top(Run) do begin
      if _Length(Run) = 0 then Break; Run:=_Link(Run);
      if Run = tmp then Break
    end
  end; Result:=Run
end;

function TVirtTree._xPred(Prd,Run: Longint): Longint;
begin
  if (Prd > 0) and (_Link(Prd) <> Run) then
  Prd:=0; if Prd <= 0 then Prd:=_Pred(Run);
  Result:=Prd
end;

function TVirtTree._fPred(Prd,Run: Longint): Longint;
var
  top,nxt: Longint;
begin
  Result:=0;

  if Prd > 0 then begin
    nxt:=_Link(Prd); top:=Prd;

    while (nxt > 0) and (nxt <> top) do begin
      if nxt = Run then begin Result:=Prd; Break end;
      Prd:=nxt; nxt:=_Link(Prd)
    end
  end;

  if Result <= 0 then
  Result:=_Pred(Run);
end;

function TVirtTree._RunP(Top,Ind: Longint): Longint;
begin
  Result:=Top;
  while (Result > 0) and (Ind > 0) do begin
    Result:=_Link(Result); Dec(Ind);
    if Result = Top then Break
  end
end;

function TVirtTree.Get_child(P: Longint): Longint;
var
  top,up: Longint;
begin
  Result:=0; if P > 0 then
  if P <= vm.vm_Ind+8+4 then begin
    top:=_Child(P); if top > 0 then
    if top < vm.vm_Ind+8+4 then
    if This_top(top) then begin
      up:=_Child(top); if up = P then
      Result:=top
    end
  end
end;

function TVirtTree.Get_RunP(Top,Ind: Longint;
                            out Ptr: Longint): Boolean;
begin
  Ptr:=_RunP(Top,Ind); Result:=Ptr <> Top
end;

function TVirtTree._Jump(Run: Longint): Longint;
begin
  if RunP <> Run then begin
    RunP:=Run; TopP:=Run
  end
end;

function TVirtTree._Goto(Run: Longint): Longint;
begin
  if RunP <> Run then begin
    RunP:=Run; TopP:=_TopP(Run)
  end else
  if TopP = Run then begin
    RunP:=Run; TopP:=_TopP(Run)
  end; Result:=Run
end;

function TVirtTree._Down(Run: Longint): Longint;
begin
  Result:=_Child(Run);
  if Result > 0 then begin
    TopP:=Result; RunP:=Result
  end
end;

function TVirtTree.Last_RunP: Longint;
var
  nxt: Longint;
begin
  Result:=RunP;

  if RunP > 0 then
  if TopP > 0 then begin
    nxt:=TopP; repeat

      Result:=nxt; nxt:=_Link(nxt);
      if nxt = 0 then begin
        NodeLink(Result,TopP); nxt:=TopP
      end

    until nxt = TopP; RunP:=Result
  end;
end;

procedure TVirtTree.Ins_Frst(Up: Longint);
begin
  vm.vm_GetMem(TopP,12);

  NodeChild(Up,TopP);
  InitNode(TopP,0,RING_FIRST,0,TopP,Up);
  RunP:=TopP
end;

procedure TVirtTree.Del_Frst(P: Longint);
begin
  vm.vm_FreeMem(P,12)
end;

function TVirtTree.Ins_Root(var info; Size: Integer): Longint;
begin
  vm.vm_GetMem(Root,Size+12);
  InitNode(Root,Size,0,0,Root,0);
  vm.vm_Store(vm_Root,Root,4);
  NodeInfo(Root,info,Size);
  TopP:=Root; RunP:=Root;
  Result:=Root
end;

function TVirtTree.Ins_Node(var info; Size: Integer): Longint;
begin
  vm.vm_GetMem(Result,Size+12);
  if Result > 0 then begin
    InitNode(Result,Size,0,0,_Link(RunP),0);
    NodeLink(RunP,Result); RunP:=Result;
    NodeInfo(Result,info,Size)
  end
end;

function TVirtTree.Del_Node(up: Boolean): Boolean;

procedure x_Del(p: Integer);
var
  top,run,nxt: Longint;
begin
  if p > 0 then begin

    top:=_Child(p);
    if top > 0 then
    if _Length(top) = 0 then begin
      run:=_Link(top);
      while run <> top do
      if run <= 0 then run:=top else begin
        nxt:=_Link(run); x_del(run); run:=nxt
      end;

      vm.vm_FreeMem(top,12);
    end;

    vm.vm_FreeMem(p,_Length(p)+12);
  end
end;

var
  pred,next: Longint;
begin
  Result:=false;
  if (RunP <> 0) and (RunP <> TopP) then begin
    pred:=_Pred(RunP); next:=_Link(RunP);

    NodeLink(Pred,next); NodeLink(RunP,0);
    x_Del(RunP);

    if (pred = next) and up then begin
      RunP:=_Child(TopP); NodeChild(RunP,0);
      TopP:=_TopP(RunP); vm.vm_FreeMem(pred,12);
      Result:=true
    end else
    
    if next = TopP then
    RunP:=pred else RunP:=next
  end
end;

function TVirtTree.Pop_Node(prd,run: Longint): Longint;
begin
  Result:=0; if prd = 0 then
  prd:=_Pred(run); if prd > 0 then begin

    Result:=run; NodeLink(prd,_Link(run));

    if prd = _Link(prd) then begin
      NodeChild(_Child(prd),0); Del_Frst(prd)
    end
  end
end;

procedure TVirtTree.Mov_Last(run: Longint);
begin
  Last_RunP; NodeLink(RunP,run);
  NodeLink(run,TopP); RunP:=run
end;

procedure TVirtTree.Up_Node(par,run: Longint);
begin
  if par = 0 then
  par:=_Parent(run);
  Mov_Node(0,run,par)
end;

procedure TVirtTree.Dn_Node(run,par: Longint);
begin
  if par > 0 then
  if run <> par then begin
    if IsChild(run,par) then
    Up_Childs(run);

    if Ring_Down(par) then
    if Last_RunP > 0 then
    Mov_Node(0,run,RunP)
  end
end;

function TVirtTree.Mov_Node(prd,mov,run: Longint): Longint;
begin
  Result:=0;
  if mov <> run then
  if prd <> run then
  if mov <> _Link(run) then

  if Pop_Node(prd,mov) > 0 then
  if Lnk_Node(run,mov) = mov then
  Result:=mov
end;

procedure TVirtTree.Mov_Child(run,dst: Longint);
begin
  if Ring_Down(dst) then
  if Last_RunP > 0 then
  Mov_Node(0,run,RunP)
end;

procedure TVirtTree.Mov_Childs(dst,src: Longint);
var
  top,run,nxt: Longint;
begin
  top:=_Child(src);
  if top > 0 then begin
    nxt:=_Link(top);
    while nxt <> top do begin
      run:=nxt; nxt:=_Link(nxt);
      Dn_Node(run,dst)
    end
  end
end;

procedure TVirtTree.Up_Childs(par: Longint);
var
  top,frst,last,nxt: Longint;
begin
  top:=_Child(par);
  if top > 0 then begin

    frst:=_Link(top);
    if frst <> top then begin

      last:=frst; while true do begin
        nxt:=_Link(last); if nxt <= Root then Break
        else if nxt = top then Break; last:=nxt
      end;

      NodeLink(last,_Link(par));
      NodeLink(par,frst)
    end;

    NodeChild(par,0)
  end
end;

function TVirtTree.Lnk_Node(run,lnk: Longint): Longint;
var
  nxt: Longint;
begin
  nxt:=_Link(run); NodeLink(run,lnk);
  NodeLink(lnk,nxt); Result:=_Link(run)
end;

function TVirtTree.Ring_Down(Run: Longint): Boolean;
var
  top: Longint;
begin
  Result:=false;
  if Run > 0 then begin

    top:=_Child(Run);
    if top > 0 then begin
      TopP:=top; RunP:=top;
      Result:=true
    end else
    if vm.vm_Edit then begin
      Ins_Frst(Run); Result:=true
    end

  end
end;

function TVirtTree.Ring_Upper: Longint;
begin
  Result:=_Child(TopP);
  if Result > 0 then begin
    RunP:=Result; TopP:=_TopP(Result)
  end
end;

function TVirtTree.Ring_Dn: Boolean;
begin
  Result:=false;

  if Ring_Down(RunP) then begin
    Last_RunP; Result:=true
  end
end;

function TVirtTree.Ring_Up: Boolean;
var
  p: Longint;
begin
  Result:=false;
  p:=_Child(TopP);

  if p > 0 then
  if p <> Root then begin
    RunP:=p; TopP:=_TopP(p)
  end
end;

function TVirtTree.Get_RunI: Integer; var run: Longint;
begin
  run:=TopP; Result:=0; while run <> RunP do
  begin Inc(Result); run:=_Link(run) end
end;

function TVirtTree.Is_Childs(par: Longint): Integer;
var
  top,run,nxt,min: Longint; len: Integer;
begin
  Result:=0; if par > 0 then begin
    top:=_Child(par); if top > 0 then

    if _Child(top) = par then begin

      min:=Root+_Length(Root);

      Result:=-1; run:=top; repeat
        Inc(Result);
        if Result > 0 then
        if fCleanMode then Break;

        nxt:=_Link(run);
        len:=_Length(nxt) + 12;

        if (nxt < min)
        or (nxt > vm.vm_Ind-len) then begin
          if vm.vm_Update then
          NodeLink(run,top);
          Break
        end;

        run:=nxt

      until run = top
    end
  end;
end;

function TVirtTree.IsChild(par,child: Longint): Boolean;
var
  top,run,nxt: Longint;
begin
  Result:=false;

  top:=_Child(par);
  if top > 0 then begin

    run:=_Link(top);
    while run <> top do begin
      if run = child then begin
        Result:=true; Break
      end;

      nxt:=_Link(run);
      if nxt = run then Break;
      run:=nxt
    end

  end
end;

function TVirtTree.Get_Childs(par: Longint;
                              Childs: PIntegers;
                              MaxLen: Integer): Integer;
var
  top,run,min,max: Longint;
begin
  Result:=0;

  if par > 0 then begin
    top:=_Child(par); if top > 0 then
    if _Child(top) = par then begin

      min:=Root+_Length(Root);
      max:=vm.vm_Ind - _Length(par) - 12;

      run:=_Link(top);
      while run <> top do begin
        if (run < min) or (run > max) then Break;
        Childs[Result]:=run; Inc(Result);
        if Result = MaxLen then Break;
        run:=_Link(run);
      end
    end
  end;
end;

function TVirtTree.group_pline(p1,p2: Longint): Integer;
var
  q,p,_top,_run: Longint;
begin
  Result:=0;
  if (p1 > 0) and (p1 < p2) then begin

    _top:=TopP; _run:=RunP;

    q:=p1;
    while true do begin
      p:=_Link(q); if p <= q then Break;
      Inc(Result); if p = _run then _run:=p1;
      if p = p2 then Break; q:=p
    end;

    if Result > 0 then
    if Ring_Down(p1) then begin

      p:=_Link(p1);
      NodeLink(p1,_Link(p2));

      Last_RunP;
      NodeLink(RunP,p);
      NodeLink(p2,TopP);

      TopP:=_top; RunP:=_run
    end
  end
end;

constructor TCopyTree.Create(Src,Dst: TVirtTree;
                             ALen: Integer);
begin
  inherited Create;
  fSrc:=Src; fDst:=Dst;
  Wait_Len:=ALen
end;

function TCopyTree.Copy_Node(Inp_P,SP: Longint): Longint;
var
  tags: Integer;
begin
  Result:=0;

  if Inp_P > 0 then begin
    len:=FSrc._Length(Inp_P);

    if len > 0 then
    if (Wait_Len = 0)
    or (len = Wait_Len) then

    if Enabled_Node(Inp_P,SP) then begin

      fSrc._Info(Inp_P,buf,len);

      if fDst.Root = 0 then
        Result:=fDst.Ins_Root(buf,len)
      else
        Result:=fDst.Ins_Node(buf,len);

      if Result > 0 then begin
        fSrc.vm.vm_Load(Inp_P+2,tags,2);
        fDst.vm.vm_Store(fDst.RunP+2,tags,2);

        fLevel:=SP;
        Update_Info(Inp_P,fDst.RunP);
        Copy_Ring(Inp_P,SP+1)
      end
    end
  end
end;

procedure TCopyTree.Copy_Ring(Inp_P,SP: Longint);

function Skip_Node(Inp_P: Longint): Boolean;
begin
  Result:=false;

  if Inp_P > 0 then begin
    len:=FSrc._Length(Inp_P);

    if len > 0 then
    if (Wait_Len = 0)
    or (len = Wait_Len) then

    Result:=true
  end
end;

var
  itop,irun,otop,orun: int;
begin
  if Inp_P > 0 then
  if SP <= 32 then begin

    itop:=fSrc._Child(Inp_P);
    if itop > 0 then begin
      irun:=fSrc._Link(itop);

      if irun > 0 then
      if irun <> itop then begin

        otop:=fDst.TopP; orun:=fDst.RunP;

        if fDst.Ring_Down(orun) then
        while irun <> itop do begin
          if Copy_Node(irun,SP) = 0 then
          if not Skip_Node(irun) then Break;
          irun:=FSrc._Link(irun)
        end;

        fDst.TopP:=otop; fDst.RunP:=orun
      end
    end
  end
end;

procedure TCopyTree.Copy_Tree;
begin
  Copy_Node(fSrc.Root,0)
end;

function TCopyTree.Enabled_Node(Inp_P,SP: Longint): Boolean;
begin
  Result:=true
end;

procedure TCopyTree.Update_Info(Inp_P,Dst_P: Longint);
begin
end;

end.