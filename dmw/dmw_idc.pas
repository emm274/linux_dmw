unit dmw_idc; interface

uses
  Windows,otypes,xlist,xvirts;

const
  IdcOpMax = 6;
  IdcOpStr: array[0..IdcOpMax] of String[3] =
    ('=','>','<','>=','<=','[]','<>');

type
  TIdcValue = record
    op,v1,v2: int16
  end;

  PIdcValRec = ^TIdcValRec;
  TIdcValRec = record
    color,style,size1,size2,scale: int;
    val: Array[0..3] of TIdcValue;
    expr: TShortStr
  end;

  PIdcValArr = ^TIdcValArr;
  TIdcValArr = Array[0..255] of TIdcValRec;

  PIdcObjRec = ^TIdcObjRec;
  TIdcObjRec = record
    code,loc,fmt,vind: int;
    nn: Array[0..3] of int;
  end;

  PIdcObjArr = ^TIdcObjArr;
  TIdcObjArr = Array[0..255] of TIdcObjRec;

  TOnIdcExpr = function(dm,hf: TObject; Ptr: int;
                        Expr: PChar): Boolean; stdcall;

  TVirtMemList = class
    constructor Create(aRecLen: int; aMagic: PChar);

    destructor Destroy; override;

    function Open(stg: TVirtMem;
                  top: int; var bot: int): int;

    function New: bool; virtual;

    function LoadFrom(stg: TVirtMem;
                      top: int; var bot: int): int; virtual;

    function DumpTo(stg: TVirtMem): int;

    function GetArray(Buf: Pointer): int;

    function Append(var rec; len: int): int;

    function GetBufLen(p: int): int;
    function GetBuf(p: int; buf: Pointer): int;
    function PutBuf(p: int; buf: Pointer; len: int): int;
    function CopyBuf(si,di: int; Dest: TVirtMemList): int;

  private
    fvm: TVirtMem;
    ftemp: TVirtMem;

    fTop: int;
    fRecLen: int;
    fDefRecLen: int;
    fCount: int;

    iMagic: int;
    fMagic: TNameStr;

    function GetActive: Boolean;

    function GetSize: int;

    procedure Reset;

    function SeekList(stg: TVirtMem; top,bot: int;
                      out oTop,oRecLen: int): int;
  public
    property Active: Boolean read GetActive;

    property Top: int read fTop;
    property RecLen: int read fRecLen;

    property Count: int read fCount;
    property Size: int read GetSize;
  end;

  TIdcVarList = class(TCustomList)
    constructor Create;
    function seek(nn: PIntegers;
                  var v: TIdcValRec;
                  dm,hf: TObject; Ptr: int;
                  logic: TOnIdcExpr): int;
  end;

  TIdcValStg = class(TVirtMemList)
    constructor Create;
    destructor Destroy; override;

    function New: bool; override;

    function LoadFrom(stg: TVirtMem;
                      top: int; var bot: int): int; override;

    function GetList(p: int; List: TIdcVarList): int;
    function PutList(p: int; List: TIdcVarList): int;

  private
    fbuf: TDataStream;
  end;

  TIdcObjList = class;

  TIdcObjStg = class(TVirtMemList)
    constructor Create;
    function AddList(List: TIdcObjList): int;
    function GetItem(i: int; out r: TIdcObjRec): int;
  end;

  TIdcObjList = class(TCustomList)
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function pIndexof(code,loc: int): PIdcObjRec;

    function LoadFrom(stg: TVirtMem; top,bot: int): int;
    function DumpTo(stg: TVirtMem; top: int): int;

    function LoadFromFile(Path: PChar): int;
    function SaveToFile(Path: PChar): int;

    function CopyTo(Dest: TIdcObjList;
                    list: TIntegerFunci): int;

    function GetVarList(code,loc: int; list: TIdcVarList): int;
    procedure PutObj(var R: TIdcObjRec; list: TIdcVarList);

  private
    fValStg: TIdcValStg;
    fEditFlag: longbool;

    function GetActive: bool;

  public
    property Active: bool read GetActive;
    property EditFlag: longbool read fEditFlag;
    property ValStg: TIdcValStg read fValStg;
  end;

  TIdcStg = class(TVirtMem)

    constructor Create;
    destructor Destroy; override;

    function Open(Path: PChar): int;
    procedure Close;

    function seekObj(Ind,Code,Loc: int;
                     out oRec: TIdcObjRec): int;

  private
    fObjStg: TIdcObjStg;
    fValStg: TIdcValStg;
    fList: TIdcVarList;

  public
    property List: TIdcVarList read fList;
  end;

function IdcValueStr(const v: TIdcValue): String;

implementation

uses
  Math,Sysutils,ofiles,xlog;

function IdcValueStr(const v: TIdcValue): String;
begin
  Result:='';
  if v.op in [0..IdcOpMax] then

  if v.op = 0 then begin
    if v.v1 <> 0 then Result:=IntToStr(v.v1)
  end else
  if v.op < 5 then
    Result:=IdcOpStr[v.op]+' '+IntToStr(v.v1)
  else begin
    Result:=IdcOpStr[v.op][1]+
            IntToStr(v.v1)+'..'+
            IntToStr(v.v2)+
            IdcOpStr[v.op][2]
  end
end;

constructor TVirtMemList.Create(aRecLen: int; aMagic: PChar);
begin
  inherited Create;
  ftemp:=TVirtMem.Create(0,0);
  fvm:=ftemp; fDefRecLen:=aRecLen;
  fRecLen:=aRecLen;
  StrCopy(fMagic,aMagic);
  iMagic:=PInteger(@fMagic)^
end;

destructor TVirtMemList.Destroy;
begin
  ftemp.vm_Erase;
  ftemp.Free;

  inherited
end;

function TVirtMemList.GetActive: Boolean;
begin
  Result:=fvm.vm_Active
end;

function TVirtMemList.GetSize: int;
begin
  Result:=fCount * fRecLen
end;

procedure TVirtMemList.Reset;
begin
  ftemp.vm_Erase;
  fvm:=ftemp; fTop:=0; fCount:=0
end;

function TVirtMemList.SeekList(stg: TVirtMem; top,bot: int;
                               out oTop,oRecLen: int): int;
var
  bx,cx,dx,n,sz,sz1: int;
begin
  Result:=-1; oTop:=0; oRecLen:=0;

  if stg.vm_Active then

  if top > 0 then begin

    bx:=top;
    if bx+16 <= stg.vm_Ind then
    if stg.vm_Long(bx) = iMagic then begin
      Inc(bx,4); n:=stg.vm_Long(bx);
      Inc(bx,4); cx:=stg.vm_Long(bx);

      if n >= 0 then

      if cx > 0 then
      if cx <= fDefRecLen then begin

        sz:=n*cx; Inc(bx,4); dx:=bx+sz;
        if dx+4 <= stg.vm_Ind then
        if sz = stg.vm_Long(dx) then begin
          oTop:=bx;

          oRecLen:=fDefRecLen;
          if cx > 0 then oRecLen:=cx;

          Result:=n
        end
      end
    end
  end else
  if bot > 8 then begin
    dx:=bot-4;
    if stg.vm_Long(dx) = iMagic then begin
      Dec(dx,4); sz:=stg.vm_Long(dx);
      bx:=dx-sz-12;

      if sz >= 0 then

      if bx >= 0 then
      if stg.vm_Long(bx) = iMagic then begin
        Inc(bx,4); n:=stg.vm_Long(bx);
        Inc(bx,4); cx:=stg.vm_Long(bx);

        if n >= 0 then

        if cx > 0 then
        if cx <= fDefRecLen then
        if cx*n = sz then begin
          oTop:=bx+4;

          oRecLen:=fDefRecLen;
          if cx > 0 then oRecLen:=cx;

          Result:=n
        end
      end
    end
  end
end;

function TVirtMemList.Open(stg: TVirtMem;
                           top: int; var bot: int): int;
var
  bx,cx,rc: int;
begin
  Reset;

  rc:=SeekList(stg,top,bot, bx,cx);

  if rc >= 0 then begin
    fvm:=stg; fTop:=bx; fRecLen:=cx;
    fCount:=rc; bot:=bx-12
  end;

  Result:=fCount
end;

function TVirtMemList.New: bool;
begin
  fvm:=ftemp; fTop:=0; fCount:=0;
  Result:=fvm.vm_Temp(fMagic);
  fRecLen:=fDefRecLen
end;

function TVirtMemList.LoadFrom(stg: TVirtMem;
                               top: int; var bot: int): int;
var
  bx,cx, rc,sz: int;
begin
  Reset;

  if New then begin

    rc:=SeekList(stg,top,bot, bx,cx);
    if rc >= 0 then
    if cx = fDefRecLen then begin
      fRecLen:=fDefRecLen;

      if rc > 0 then begin
        sz:=rc * fRecLen;
        if fvm.vmx_Append(stg,bx,sz) = sz then
        fCount:=rc;
      end;

      bot:=bx-12
    end
  end;

  Result:=fCount
end;

function TVirtMemList.DumpTo(stg: TVirtMem): int;
var
  cx: int;
begin
  Result:=-1;

  if fCount > 0 then begin

    Result:=stg.vm_Ind;

    stg.vm_Append(iMagic,4);
    stg.vm_Append(fCount,4);
    stg.vm_Append(fRecLen,4);

    cx:=fCount*fRecLen;

    stg.vmx_Append(fvm,fTop,cx);

    stg.vm_Append(cx,4);
    stg.vm_Append(iMagic,4)
  end
end;

function TVirtMemList.GetArray(Buf: Pointer): int;
begin
  Result:=fCount;
  if fCount > 0 then
  fvm.vm_Load(fTop,Buf^,Size)
end;

function TVirtMemList.Append(var rec; len: int): int;
begin
  Result:=-1;
  if fvm.vm_Active then
  Result:=fvm.vm_Append(rec,len) - fTop
end;

function TVirtMemList.GetBufLen(p: int): int;
var
  len: int;
begin
  Result:=0;
  if fvm.vm_Active then
  if fvm.vmx_Block(fTop+p,len) > 0 then
  Result:=len
end;

function TVirtMemList.GetBuf(p: int; buf: Pointer): int;
var
  len: int;
begin
  Result:=0;
  if fvm.vm_Active then begin
    p:=fvm.vmx_Block(fTop+p,len);
    if (p > 0) and (len > 0) then begin
      fvm.vm_Load(p,buf^,len);
      Result:=len
    end
  end
end;

function TVirtMemList.PutBuf(p: int; buf: Pointer; len: int): int;
var
  p1: int;
begin
  Result:=0; p1:=p;
  if p > 0 then p1:=fTop+p;

  if fvm.vm_Active then
  if fvm.vmx_UpdateBuf(p1,buf^,len) then
  Result:=p1-fTop;

  fCount:=fvm.vm_Ind
end;

function TVirtMemList.CopyBuf(si,di: int; Dest: TVirtMemList): int;
begin
  Result:=0;
  if fvm.vm_Active then
  if Dest.fvm.vm_Active then begin
    Result:=fvm.vm_CopyBuf(fTop+si,di,Dest.fvm);
    Dest.fCount:=Dest.fvm.vm_Ind
  end
end;

constructor TIdcValStg.Create;
begin
  inherited Create(1,'idcv');
  fbuf:=TDataStream.Create(4096)
end;

destructor TIdcValStg.Destroy;
begin
  fbuf.Free; inherited
end;

function TIdcValStg.New: bool;
begin
  Result:=inherited New;
  if Result then Append(fMagic,4)
end;

function TIdcValStg.LoadFrom(stg: TVirtMem;
                             top: int; var bot: int): int;
var
  bx,cx, rc: int;
begin
  Reset;

  if New then begin

    rc:=SeekList(stg,top,bot, bx,cx);
    if rc >= 0 then begin

      if rc > 4 then
      if cx = 1 then begin
        fvm.vm_Ind:=0;
        fvm.vmx_Append(stg,bx,rc);
        fCount:=fvm.vm_Ind
      end;

      bot:=bx-12
    end
  end;

  Result:=fCount
end;

function TIdcValStg.GetList(p: int; List: TIdcVarList): int;
var
  bp: PBytes; bx,cx,dx: int; r: TIdcValRec;
begin
  List.Clear;

  cx:=GetBufLen(p);
  if cx > 0 then
  if fbuf.Resize(cx) then begin
    bp:=fbuf.Buffer;
    cx:=GetBuf(p,bp);

    while cx >= 1 do begin
      Fillchar(r,Sizeof(r),0);

      bx:=bp[0]; bp:=@bp[1]; Dec(cx);
      if not (bx in [1..5]) then Break;
      bx:=bx*4; if bx > cx then Break;

      Move(bp[0],r.color,bx); bp:=@bp[bx];
      Dec(cx,bx); if cx <= 0 then Break;

      bx:=bp[0]; bp:=@bp[1]; Dec(cx);
      if not (bx in [0..4]) then Break;
      bx:=bx * Sizeof(TIdcValue);
      if bx > cx then Break;

      if bx > 0 then begin
        Move(bp[0],r.val,bx);
        bp:=@bp[bx]; Dec(cx,bx)
      end;

      if cx > 0 then begin
        bx:=bp[0]; bp:=@bp[1]; Dec(cx);

        if bx > 0 then begin
          if bx > cx then Break;

          dx:=Min(bx,Sizeof(r.expr)-1);
          Move(bp[0],r.expr,dx);
          bp:=@bp[bx]; Dec(cx,bx)
        end
      end;

      List.Add(@r)
    end
  end;

  Result:=List.Count
end;

function TIdcValStg.PutList(p: int; List: TIdcVarList): int;
var
  i,j,k: int; lp: PIdcValArr; ip: PIntegers;
  r: TIdcValRec; v: TIdcValue;
begin
  Result:=0; fbuf.Size:=0;

  lp:=List.First;
  for i:=0 to List.Count-1 do begin
    r:=lp[i];

    ip:=@r; k:=1;
    for j:=1 to 4 do  // style,size1,size2,scale
    if ip[j] <> 0 then k:=j+1;

    fbuf.Append(k,1);
    fbuf.Append(r,k*4);

    k:=0;
    for j:=0 to 3 do begin
      v:=r.val[j];
      if (v.op <> 0) or (v.v1 <> 0) then
      k:=j+1
    end;

    fbuf.Append(k,1); if k > 0 then
    fbuf.Append(r.val,k * Sizeof(TIdcValue));

    k:=0;
    if r.expr[0] <> #0 then
    k:=Strlen(r.expr);

    fbuf.Append(k,1); if k > 0 then
    fbuf.Append(r.expr,k);
  end;

  if fbuf.Size > 0 then
  Result:=PutBuf(p,fbuf.Buffer,fbuf.Size);

  fCount:=fvm.vm_Ind
end;

constructor TIdcObjStg.Create;
begin
  inherited Create(Sizeof(TIdcObjRec),'idco');
end;

function TIdcObjStg.AddList(List: TIdcObjList): int;
begin
  Result:=0;

  if List.Count > 0 then
  if List.ItemLen = fRecLen then begin
    fvm.vm_Append(List.First^,List.BufferSize);
    Inc(fCount,List.Count);
    Result:=List.Count
  end
end;

function TIdcObjStg.GetItem(i: int; out r: TIdcObjRec): int;
begin
  Result:=-1;
  if (i >= 0) and (i < fCount) then begin
    Fillchar(r,Sizeof(r),0);
    fvm.vm_Load(fTop+i*fRecLen,r,fRecLen);
    Result:=i
  end
end;
                    
constructor TIdcVarList.Create;
begin
  inherited Create(Sizeof(TIdcValRec),256);
end;

function TIdcVarList.seek(nn: PIntegers;
                          var v: TIdcValRec;
                          dm,hf: TObject; Ptr: int;
                          logic: TOnIdcExpr): int;

function verifyValue(const v1: TIdcValue; v2: int): bool;
begin
  Result:=false;
  case v1.op of
0:  Result:=v2 = v1.v1;
1:  Result:=v2 > v1.v1;
2:  Result:=v2 < v1.v1;
3:  Result:=v2 >= v1.v1;
4:  Result:=v2 <= v1.v1;
5:  Result:=(v2 >= v1.v1) and (v2 <= v1.v2);
6:  Result:=(v2 < v1.v1) or (v2 > v1.v2);
  end
end;

var
  i,rc: int; vp: PIdcValArr; r,t: TIdcValRec;
begin
  Result:=-1; t:=v;

  vp:=First;
  for i:=0 to Count-1 do begin
    r:=vp[i]; rc:=-1;

    if nn[0] = 0 then rc:=i else
    if ((r.val[0].op = 0) and (r.val[0].v1 = 0))
    or verifyValue(r.val[0],t.val[0].v1) then

    if nn[1] = 0 then rc:=i else
    if ((r.val[1].op = 0) and (r.val[1].v1 = 0))
    or verifyValue(r.val[1],t.val[1].v1) then

    if nn[2] = 0 then rc:=i else
    if ((r.val[2].op = 0) and (r.val[2].v1 = 0))
    or verifyValue(r.val[2],t.val[2].v1) then

    if nn[3] = 0 then rc:=i else
    if ((r.val[3].op = 0) and (r.val[3].v1 = 0))
    or verifyValue(r.val[3],t.val[3].v1) then
      rc:=i;

    if rc = i then
    if r.expr[0] <> #0 then
    if Assigned(logic) then
    if not logic(dm,hf,Ptr,r.expr) then
    rc:=-1;

    if rc = i then begin
      v:=r; Result:=i; Break
    end
  end
end;

constructor TIdcObjList.Create;
begin
  inherited Create(Sizeof(TIdcObjRec),256);
  fValStg:=TIdcValStg.Create;
end;

destructor TIdcObjList.Destroy;
begin
  fValStg.Free;
  inherited
end;

procedure TIdcObjList.Clear;
begin
  inherited Clear;
  fValStg.New;
  fEditFlag:=false
end;

function TIdcObjList.GetActive: bool;
begin
  Result:=fValStg.Active
end;

function TIdcObjList.pIndexof(code,loc: int): PIdcObjRec;
var
  i: int; lp: PIdcObjArr;
begin
  Result:=nil; lp:=First;

  for i:=0 to Count-1 do
  if lp[i].code = code then
  if lp[i].loc = loc then begin
    Result:=@lp[i]; Break
  end
end;

function TIdcObjList.CopyTo(Dest: TIdcObjList;
                            list: TIntegerFunci): int;
var
  i,key: int; lp: PIdcObjArr; o: TIdcObjRec;
begin
  Dest.Clear;
  if Dest.Active then begin

    lp:=First;
    for i:=0 to Count-1 do begin
      o:=lp[i];

      if Assigned(list) then begin
        key:=o.code*10 + o.loc;
        if list(key) < 0 then o.vind:=0
      end;

      if o.vind > 0 then begin
        o.vind:=fValStg.CopyBuf(o.vind,0,Dest.ValStg);
        if o.vind > 0 then Dest.Add(@o)
      end
    end
  end;

  Dest.fEditFlag:=false;
  Result:=Dest.Count
end;

function TIdcObjList.LoadFrom(stg: TVirtMem; top,bot: int): int;
var
  tmp: TIdcObjStg; p1,p2,rc: int;
begin
  Clear;

  tmp:=TIdcObjStg.Create;
  try
    p1:=-1; p2:=-1;
    if top > 0 then begin
      top:=stg.vm_Load(top,p1,4);
      top:=stg.vm_Load(top,p2,4);

      tmp.Open(stg,p1,bot);
      fValStg.LoadFrom(stg,p2,bot);
    end
    else begin
      rc:=fValStg.LoadFrom(stg,-1,bot);
      if rc >= 0 then tmp.Open(stg,-1,bot)
    end;

    if tmp.RecLen = Sizeof(TIdcObjRec) then
    if Extend(tmp.Count) then
    tmp.GetArray(First);
  finally
    tmp.Free
  end;

  if not fValStg.Active then Clear;
  Result:=Count
end;

function TIdcObjList.DumpTo(stg: TVirtMem; top: int): int;
var
  tmp: TIdcObjStg; p1,p2: int;
begin
  Result:=0;

  if Count > 0 then begin

    tmp:=TIdcObjStg.Create;
    try
      if tmp.New then
      if tmp.AddList(Self) > 0 then begin

        p1:=tmp.DumpTo(stg);
        p2:=fValStg.DumpTo(stg);

        if top > 0 then begin
          top:=stg.vm_Store(top,p1,4);
          top:=stg.vm_Store(top,p2,4);
        end;

        Result:=Count
      end
    finally
      tmp.Free
    end

  end
end;

function TIdcObjList.LoadFromFile(Path: PChar): int;
var
  stg: TVirtMem; top: int; fn,mag: TShortstr;
begin
  Clear;

  StrUpdateExt(fn,Path,'.idc');

  stg:=TVirtMem.Create(0,0);
  try
    if stg.vm_Open(fn) then begin

      top:=-1;
      if stg.vm_Ind >= 16 then begin
        Fillchar(mag,Sizeof(mag),0);
        stg.vm_Load(0,mag,4);
        if StrComp(mag,'#idc') = 0 then
        top:=4
      end;

      Result:=LoadFrom(stg,top,stg.vm_Ind)
    end
  finally
    stg.Free
  end;

  Result:=Count
end;

function TIdcObjList.SaveToFile(Path: PChar): int;
var
  stg: TVirtMem; fn,mag: TShortstr;
begin
  Result:=0;

  StrUpdateExt(fn,Path,'.idc');
  if Count = 0 then
    FileErase(fn)
  else begin
    stg:=TVirtMem.Create(0,0);
    try
      if not stg.vm_Make(fn) then begin
        StrFmt(mag,'*** create file "%s" false.',[fn]);
        std_err.WriteAnsi(mag)
      end
      else begin

        Fillchar(mag,Sizeof(mag),0);
        StrCopy(mag,'#idc');
        stg.vm_Append(mag,Sizeof(mag));

        Result:=DumpTo(stg,4)
      end
    finally
      stg.Free
    end
  end
end;

function TIdcObjList.GetVarList(code,loc: int; list: TIdcVarList): int;
var
  p: PIdcObjRec;
begin
  Result:=0; list.Clear;

  p:=pIndexof(code,loc);
  if Assigned(p) then
  Result:=fValStg.GetList(p.vind,list);

  list.IsChanged:=false
end;

procedure TIdcObjList.PutObj(var R: TIdcObjRec; list: TIdcVarList);
var
  p: PIdcObjRec; 
begin
  R.vind:=fValStg.PutList(R.vind,list);

  p:=pIndexof(R.code,R.loc);
  if Assigned(p) then p^:=R else
  Add(@R)
end;

constructor TIdcStg.Create;
begin
  inherited Create(0,0);
  fObjStg:=TIdcObjStg.Create;
  fValStg:=TIdcValStg.Create;
  fList:=TIdcVarList.Create;
end;

destructor TIdcStg.Destroy;
begin
  fList.Free;
  fValStg.Free;
  fObjStg.Free;
  inherited
end;

function TIdcStg.Open(Path: PChar): int;
var
  top,bot,p1,p2: int; mag: TShortstr;
begin
  fObjStg.Reset;
  fValStg.Reset;

  if vm_Open_ext(Path,'.idc') then begin

    bot:=vm_Ind;

    top:=-1;
    if bot >= 16 then begin
      Fillchar(mag,Sizeof(mag),0);
      vm_Load(0,mag,4);
      if StrComp(mag,'#idc') = 0 then
      top:=4
    end;

    if top > 0 then begin
      top:=vm_Load(top,p1,4);
      top:=vm_Load(top,p2,4);
      fObjStg.Open(Self,p1,bot);
      fValStg.Open(Self,p2,bot)
    end else

    if fValStg.Open(Self,-1,bot) > 0 then
    fObjStg.Open(Self,-1,bot)
  end;

  Result:=fObjStg.Count
end;

procedure TIdcStg.Close;
begin
  fValStg.Reset;
  fObjStg.Reset;
  vm_Close
end;

function TIdcStg.seekObj(Ind,Code,Loc: int;
                         out oRec: TIdcObjRec): int;
var
  i: int; o: TIdcObjRec; v: TIdcValRec;
begin
  Result:=0; fList.Clear;

  if Ind >= 0 then
  if fObjStg.GetItem(Ind,o) >= 0 then
  if o.code = Code then
  if o.loc = Loc then begin
    Result:=fValStg.GetList(o.vind,fList);
    oRec:=o
  end
end;

end.