unit xlist1; interface

uses
  Classes,Contnrs,Math,LCLType,
  otypes,xintf,xlist,xpens1;

type
  TIntfPolyI = class(TInterfacedObject,IPolyI)
    constructor Create;
    destructor Destroy; override;

    function GetDim: Integer; stdcall;
    procedure SetDim(Dim: Integer); stdcall;

    function GetCount: Integer; stdcall;
    function GetPoint(Ind: Integer; V: PIntegers): Integer; stdcall;
    function SetPoint(Ind: Integer; V: PIntegers): Integer; stdcall;
    function Delete(Ind: Integer): Integer; stdcall;
    function Insert(Ind: Integer; v: PIntegers): Integer; stdcall;
    procedure Clear; stdcall;

  private
    fData: TCustomList;
  end;

  TIntfPolyF = class(TInterfacedObject,IPolyF)
    constructor Create;
    destructor Destroy; override;

    function GetCount: Integer; stdcall;
    function GetPoint(Ind: Integer; V: PDoubles; N: Integer): Integer; stdcall;
    function SetPoint(Ind: Integer; V: PDoubles; N: Integer): Integer; stdcall;
    function Delete(Ind: Integer): Integer; stdcall;
    function Insert(Ind: Integer; v: pxyz): Integer; stdcall;
    procedure Clear; stdcall;

  private
    flist: TXyzList;
    fscale: Double;
  end;

  TGeoDatumList = class(TCustomList)
    constructor Create;
    function Indexof(dX,dY,dZ,Eps: Double): Integer;
  end;

  PCoordRec = ^TCoordRec;
  TCoordRec = record
    Id1,Id2: Integer;
    sys: tsys; name: TNameStr
  end;

  PCoordArray = ^TCoordArray;
  TCoordArray = Array[0..255] of TCoordRec;

  TCoordList = class(TCustomList)
    constructor Create;
    function LoadFrom(Path: PChar): Integer;

    function GetList(Id: Integer;
                     List: TCoordList;
                     Names: TStrings): Integer;
  end;

  TScaleList = class(TIntegerList)
    constructor Create;
    function scaleOf(scale: Integer): Integer;
  end;

  PPcxItem = ^TPcxItem;
  TPcxItem = record
    x,y,iw,ih,ow,oh: Integer;
    scale: Float; fn: TShortstr
  end;

  PPcxArray = ^TPcxArray;
  TPcxArray = Array[0..255] of TPcxItem;

  TPcxList = class(TCustomList)
    constructor Create;

    function xAdd(x,y,iw,ih,ow,oh: Integer;
                  scale: Float; img: PChar): Integer;

    function GetBound(out lt,rb: TPoint): Integer;
    procedure Shift(dx,dy: Integer);
  protected
    function Up_item(p1,p2: Pointer): Boolean; override;
  end;

  PPlanItem = ^TPlanItem;
  TPlanItem = record
    Id: Integer; lt,rb: TGauss;
    fn: TShortstr
  end;

  PPlanArray = ^TPlanArray;
  TPlanArray = Array[0..255] of TPlanItem;

  TPlanList = class(TCustomList)
    constructor Create;
    function GetList(List: TStrings; Frame: PGPoly): Integer;
    function PickupList(List: TStrings; const g: TGauss): Integer;
    function Pickup(Path: PChar; const g: TGauss): Integer;
    function GetActive(Path: PChar): Integer;
  end;

const
  attr_Active = 1;
  attr_Scaled = 2;
  attr_House  = 4;
  attr_cx     = 8;
  attr_cy     = 16;

type
  PDispAttr = ^TDispAttr; TDispAttr = record
    code,loc, nn,id, color, dx,dy,ds: Integer;
    typ,flags,size1,size2,val,die: Integer;
  end;

  PDispAttrArr = ^TDispAttrArr;
  TDispAttrArr = Array[0..255] of TDispAttr;

  TDispAttrList = class(TCustomList)
    constructor Create;
  end;

  PHintItem = ^THintItem;
  THintItem = record
    Ptr,Loc,hf,mf,ln: int;
    lt,rb: TPoint; Str: TNameStr1
  end;

  PHintArray = ^THintArray;
  THintArray = array[0..1023] of THintItem;

  THintList = class(TCustomList)
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    procedure ResetDraw;

    function BeginDraw(ACols,ARows: int;
                       const lt,rb: TPoint): int;

    function AddSign(Ptr: int;
                     const Pos: TPoint;
                     Str: PChar): int;

    function AddPoly(Ptr,Loc: int;
                     lp: PLPoly; N: int;
                     Str: PChar): int;

    procedure Pop(List: THintList);

    function Pickup(const Alt,Arb: TPoint;
                    Str: PChar; smax: int;
                    out Pos: TPoint): bool;

    function GetPoly(Ptr: int; out bp: PLPoly): int;

    function RemoveObject(Ptr: int): int;

  private
    fZones: TObjectList;
    fmf: TPointList;
    fhf: TDataStream;
    fCols,fRows: int;

    fdc: TMaskMap;
    fmap: Bitmap;

    flt,frb: TPoint;
    fkx,fky: Double;

    function Zoneof(pX,pY: int): TIntegerList;

    function MapToZones(Ind: Integer): int;
  end;

  PVgcItem = ^TVgcItem;
  TVgcItem = record
    obj: Cardinal;
    nn,val,color: Integer
  end;

  PVgcArray = ^TVgcArray;
  TVgcArray = Array[0..1023] of TVgcItem;

  TVgcList = class(TCustomList)
    constructor Create;
    function LoadFrom(Path: PChar): Integer;
    procedure SaveAs(Path: PChar);

  protected
    function Up_item(p1,p2: Pointer): Boolean; override;
  end;

  prumb = ^trumb;
  trumb = record
   r,a,z: Double; az: Integer
  end;

  prumb_array = ^trumb_array;
  trumb_array = Array[0..1023] of trumb;

  TRumbList = class(TCustomList)
    constructor Create;
  end;

  PUpTextRec = ^TUpTextRec;
  TUpTextRec = record
    code,loc,codeT,nn,dec: int;
    act,code1,code2: int; mm: float
  end;

  PUpTextArray = ^TUpTextArray;
  TUpTextArray = Array[0..255] of TUpTextRec;

  TUpTextList = class(TCustomList)
    constructor Create(AName: PChar);
    function Objectof(code,loc: int): PUpTextRec;
  end;

  PKeyIntfRec = ^TKeyIntfRec;
  TKeyIntfRec = record
    Key: uint; Intf: TPointer;
  end;

  TKeyIntfList = class(TCustomList)
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

    procedure deleteKey(Key: uint);
  end;

function GetIntfPolyI(out Obj): HResult; stdcall;
function GetIntfPolyF(out Obj): HResult; stdcall;

function rumb(r,a,z: Double; az: Integer): trumb;

implementation

uses
  Sysutils,
  convert,ofiles,
  xline,xpoly,ogauss,
  xddw;

function GetIntfPolyI(out Obj): HResult; stdcall;
var
  poly: TIntfPolyI;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  poly:=TIntfPolyI.Create;
  try
    if poly.GetInterface(IPolyI,Obj) then begin
      Result:=S_OK; poly:=nil
    end;
  finally
    poly.Free
  end
end;

function GetIntfPolyF(out Obj): HResult; stdcall;
var
  poly: TIntfPolyF;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  poly:=TIntfPolyF.Create;
  try
    if poly.GetInterface(IPolyF,Obj) then begin
      Result:=S_OK; poly:=nil
    end;
  finally
    poly.Free
  end
end;

constructor TIntfPolyI.Create;
begin
  inherited Create;
  fData:=TCustomList.Create(8,1024);
end;

destructor TIntfPolyI.Destroy;
begin
  fData.Free; inherited
end;

function TIntfPolyI.GetDim: Integer;
begin
  Result:=fData.ItemLen div 4
end;

procedure TIntfPolyI.SetDim(Dim: Integer);
begin
  if Dim in [1..3] then begin
    fData.Clear; fData.ItemLen:=Dim*4
  end
end;

function TIntfPolyI.GetCount: Integer;
begin
  Result:=fData.Count
end;

function TIntfPolyI.GetPoint(Ind: Integer; V: PIntegers): Integer;
var
  i,k: Integer; p: PIntegers;
begin
  Result:=-1; p:=fData[Ind];
  if Assigned(p) then begin
    k:=GetDim;
    for i:=0 to k-1 do V[i]:=p[i];
    Result:=Ind
  end
end;

function TIntfPolyI.SetPoint(Ind: Integer; V: PIntegers): Integer;
var
  i,k: Integer; p: PIntegers;
begin
  Result:=-1; p:=fData[Ind];
  if Assigned(p) then begin
    k:=GetDim;
    for i:=0 to k-1 do p[i]:=V[i];
    Result:=Ind
  end
end;

function TIntfPolyI.Delete(Ind: Integer): Integer;
begin
  Result:=fData.Delete(Ind)
end;

function TIntfPolyI.Insert(Ind: Integer; v: PIntegers): Integer;
begin
  Result:=fData.Insert_range(v,Ind,1)
end;

procedure TIntfPolyI.Clear;
begin
  fData.Clear
end;

constructor TIntfPolyF.Create;
begin
  inherited Create;
  flist:=TXyzList.Create(1024);
  fscale:=-1
end;

destructor TIntfPolyF.Destroy;
begin
  flist.Free; inherited
end;

function TIntfPolyF.GetCount: Integer;
begin
  Result:=flist.Count
end;

function TIntfPolyF.GetPoint(Ind: Integer; V: PDoubles; N: Integer): Integer;
var
  q,p: pxyz; dx,dy: Double;
begin
  Result:=-1; p:=flist[Ind];
  if Assigned(p) then begin
    if N > 0 then V[0]:=p.x;
    if N > 1 then V[1]:=p.y;
    if N > 2 then V[2]:=p.z;

    if N > 3 then

    if Ind = 0 then begin
      V[3]:=0; if N > 4 then V[4]:=0
    end
    else begin
      q:=flist[Ind-1];
      dx:=p.x-q.x; dy:=p.y-q.y;
      V[3]:=Hypot(dx,dy); if N > 4 then
      V[4]:=Norm_Angle(ArcTan2(dy,dx))*180/Pi;
    end;

    Result:=Ind
  end
end;

function TIntfPolyF.SetPoint(Ind: Integer; V: PDoubles; N: Integer): Integer;
var
  p: pxyz;
begin
  Result:=-1;

  if Ind = -1 then
    fscale:=V[0]
  else begin
    p:=flist[Ind];
    if Assigned(p) then begin
      if N > 0 then p.x:=V[0];
      if N > 1 then p.y:=V[1];
      if N > 2 then p.z:=V[2];
      Result:=Ind
    end
  end
end;

function TIntfPolyF.Delete(Ind: Integer): Integer;
begin
  Result:=-1; if Ind >= 0 then
  Result:=flist.Delete(Ind)
end;

function TIntfPolyF.Insert(Ind: Integer; v: pxyz): Integer;
begin
  if Ind < 0 then Result:=flist.Add(v)
  else Result:=flist.Insert_range(v,Ind,1)
end;

procedure TIntfPolyF.Clear;
begin
  flist.Clear
end;

constructor TGeoDatumList.Create;
begin
  inherited Create(Sizeof(TDatum7),16)
end;

function TGeoDatumList.Indexof(dX,dY,dZ,Eps: Double): Integer;
var
  i: Integer; lp: PDatums;
begin
  Result:=-1; lp:=First;

  for i:=0 to Count-1 do begin

    if Abs(lp[0].dX-dX) < Eps then
    if Abs(lp[0].dY-dY) < Eps then
    if Abs(lp[0].dZ-dZ) < Eps then begin
      Result:=i; Break
    end;

    lp:=@lp[1]
  end
end;

constructor TCoordList.Create;
begin
  inherited Create(Sizeof(TCoordRec),16)
end;

function TCoordList.LoadFrom(Path: PChar): Integer;
var
  txt: TTextfile;
  r: TCoordRec; s: TShortstr;
begin
  Clear;

  txt:=TTextfile.Create;
  try
    if txt.Open(Path) then
    while txt.xStrLine <> nil do begin

      Fillchar(r,Sizeof(r),0);
      if txt.x_str(s) <> nil then begin
        StrLCopy(r.name,s,31);
        txt.x_Int(r.Id1);
        txt.x_Int(r.Id2);

        if txt.xStrLine <> nil then
        if geoToken(txt.str,r.sys) then begin
          r.sys.pps:=1; Add(@r)
        end
      end;
    end;
  finally
    txt.Free
  end
end;

function TCoordList.GetList(Id: Integer;
                            List: TCoordList;
                            Names: TStrings): Integer;
var
  i: Integer; lp: PCoordArray;
begin
  List.Clear; Names.Clear;

  lp:=First;
  for i:=1 to Count do begin
    if lp[0].Id1 = Id then begin
      List.Add(@lp[0]);
      Names.Add(Strpas(lp[0].Name));
    end; lp:=@lp[1]
  end;

  Result:=List.Count
end;

constructor TScaleList.Create;
begin
  inherited Create;
  AddItem(5000000);
  AddItem(3000000);
  AddItem(2000000);
  AddItem(1000000);
  AddItem(750000);
  AddItem(500000);
  AddItem(300000);
  AddItem(200000);
  AddItem(100000);
  AddItem(75000);
  AddItem(50000);
  AddItem(30000);
  AddItem(25000);
  AddItem(20000);
  AddItem(10000);
  AddItem(7500);
  AddItem(5000);
  AddItem(3000);
  AddItem(2000);
  AddItem(1000);
  AddItem(750);
  AddItem(500);
  AddItem(300);
  AddItem(200);
  AddItem(100);
end;

function TScaleList.scaleOf(scale: Integer): Integer;
var
  i: Integer; lp: PIntegers;
begin
  Result:=-1; lp:=First;

  for i:=0 to Count-1 do begin
    if scale > lp[0] then Break; Result:=i;
    if scale/lp[0] <= 0.9 then Inc(Result);
    lp:=@lp[1]
  end;

end;

constructor TPcxList.Create;
begin
  inherited Create(Sizeof(TPcxItem),32)
end;

function TPcxList.xAdd(x,y,iw,ih,ow,oh: Integer;
                       scale: Float; img: PChar): Integer;
var
  ph: TPcxItem;
begin
  ph.x:=x; ph.y:=y;
  ph.iw:=iw; ph.ih:=ih;
  ph.ow:=ow; ph.oh:=oh;
  ph.scale:=scale;
  StrPLCopy(ph.fn,img,255);
  Result:=Add(@ph)
end;

function TPcxList.GetBound(out lt,rb: TPoint): Integer;
var
  i,x2,y2: Integer; lp: PPcxArray;
begin
  lt:=Point(0,0); rb:=lt;

  lp:=First;
  for i:=0 to Count-1 do begin

    with lp[0] do begin
      x2:=x+ow; y2:=y+oh;
      if x < lt.X then lt.X:=x;
      if y < lt.Y then lt.Y:=y;
      if x2 > rb.X then rb.X:=x2;
      if y2 > rb.Y then rb.Y:=y2;
    end;

    lp:=@lp[1]
  end;

  Result:=Count
end;

procedure TPcxList.Shift(dx,dy: Integer);
var
  i,x2,y2: Integer; lp: PPcxArray;
begin
  lp:=First;
  for i:=0 to Count-1 do begin
    with lp[0] do begin
      Inc(x,dx); Inc(y,dy)
    end; lp:=@lp[1]
  end;
end;

function TPcxList.Up_item(p1,p2: Pointer): Boolean;
var
  ph1,ph2: PPcxItem;
begin
  Result:=false; ph1:=p1; ph2:=p2;

  if ph2.y < ph1.y then
    Result:=true
  else
  if ph2.y = ph1.y then
  if ph2.x < ph1.x then
    Result:=true
end;

constructor TPlanList.Create;
begin
  inherited Create(Sizeof(TPlanItem),16)
end;

function TPlanList.GetList(List: TStrings; Frame: PGPoly): Integer;
var
  i: Integer; lp: PPlanArray;
  flt,frb: TGauss;
begin
  List.Clear; lp:=First;

  if Frame = nil then

    for i:=0 to Count-1 do
    List.Add(lp[i].fn)

  else begin
    flt:=Frame[0]; frb:=Frame[1];

    for i:=0 to Count-1 do with lp[i] do
    if GaussContainsPort(flt,frb,lt,rb) then
    List.Add(fn);
  end;

  Result:=List.Count
end;

function TPlanList.PickupList(List: TStrings; const g: TGauss): Integer;
var
  i: Integer; lp: PPlanArray;
begin
  List.Clear; lp:=First;

  for i:=0 to Count-1 do with lp[i] do
  if GaussContainsPoint(lt,rb,g) then
  List.Add(fn);

  Result:=List.Count
end;

function TPlanList.Pickup(Path: PChar; const g: TGauss): Integer;
var
  i: Integer; lp: PPlanArray;
begin
  Result:=-1; StrCopy(Path,'');

  lp:=First;
  for i:=0 to Count-1 do with lp[i] do
  if GaussContainsPoint(lt,rb,g) then begin
    StrCopy(Path,fn); Result:=i
  end;
end;

function TPlanList.GetActive(Path: PChar): Integer;
var
  pp: PPlanItem;
begin
  Result:=-1; StrCopy(Path,'');

  if Count > 0 then begin
    if (ItemIndex < 0)
    or (ItemIndex >= Count) then
    ItemIndex:=0; pp:=ActiveItem;

    if Assigned(pp) then begin
      StrCopy(Path,pp.fn);
      Result:=ItemIndex
    end
  end
end;

constructor TDispAttrList.Create;
begin
  inherited Create(SizeOf(TDispAttr),64)
end;

constructor THintList.Create;
begin
  inherited Create(SizeOf(THintItem),1024);
  fZones:=TObjectList.Create;
  fmf:=TPointList.Create(4096*16);

  fhf:=TDataStream.Create(4096);

  fdc:=TMaskMap.Create
end;

destructor THintList.Destroy;
begin
  freeBitmap(fmap);
  fdc.Free;

  fhf.Free; fmf.Free;

  fZones.Free;
  inherited
end;

procedure THintList.Clear;
begin
  fzones.Clear;
  fhf.Size:=0; fmf.Clear;
  inherited Clear;
end;

procedure THintList.ResetDraw;
var
  i: int; 
begin
  for i:=0 to fzones.Count-1 do
  TIntegerlist(fzones[i]).Clear;

  fhf.Size:=0; fmf.Clear;

  inherited Clear;
end;

function THintList.BeginDraw(ACols,ARows: int;
                             const lt,rb: TPoint): int;
var
  c,r: Integer;
  zone: TIntegerlist;
begin
  Clear; flt:=lt; frb:=rb;

  fCols:=ACols; fRows:=ARows;

  fkx:=fCols / (rb.X - lt.X + 1);
  fky:=fRows / (rb.Y - lt.Y + 1);

  for r:=1 to fRows do
  for c:=1 to fCols do begin
    zone:=TIntegerlist.Create;
    try
      fzones.Add(zone); zone:=nil;
    finally
      zone.Free
    end
  end;

  allocBitmap(fmap,fCols,fRows,8);
  fdc.BeginDraw(fmap);

  fhf.Size:=0; fmf.Clear;

  if fzones.Count <> fCols * fRows then
  fzones.Clear;

  Result:=fzones.Count
end;

function THintList.Zoneof(pX,pY: int): TIntegerList;
var
  c,r,iz: Integer;
begin
  Result:=nil;

  c:=Trunc((pX - flt.X)*fkx);
  r:=Trunc((pY - flt.Y)*fky);

  if fzones.Count > 0 then
  if (c >= 0) and (c < fCols) then
  if (r >= 0) and (r < fRows) then
  Result:=fzones[r * fCols + c] as TIntegerlist;
end;

function THintList.MapToZones(Ind: Integer): int;
var
  x,y,bx: int; bp,bp1: PBytes;
  z: TIntegerList; r: TRect;
begin
  Result:=0;

  bp:=fmap.bmBits;
  bx:=fmap.bmWidthBytes;

  r:=fdc.Rect;
  r.Left:=Max(0,r.Left);
  r.Top:=Max(0,r.Top);
  r.Right:=Min(fCols-1,r.Right);
  r.Bottom:=Min(fRows-1,r.Bottom);

  for y:=r.Top to r.Bottom do begin

    bp1:=@bp[y*bx];
    for x:=r.Left to r.Right do
    if bp1[x] <> 0 then begin
      z:=fzones[y * fCols + x] as TIntegerlist;
      z.AddItem(Ind)
    end
  end
end;

function THintList.AddSign(Ptr: int;
                           const Pos: TPoint;
                           Str: PChar): int;
var
  z: TIntegerList; r: THintItem;
begin
  Result:=-1;

  z:=Zoneof(Pos.X,Pos.Y);
  if Assigned(z) then begin

    Fillchar(r,Sizeof(r),0);
    r.Ptr:=Ptr; r.Loc:=1; r.lt:=Pos; r.rb:=Pos;
    StrLCopy(r.Str,Str,Sizeof(r.Str)-1);

    Result:=Add(@r); if Result >= 0 then
    z.AddItem(Result)
  end
end;

function THintList.AddPoly(Ptr,Loc: int;
                           lp: PLPoly; N: int;
                           Str: PChar): int;
var
  i,l: int; r: THintItem; a,b: TPoint;
begin
  Result:=-1;

  if fzones.Count > 0 then
  if fmf.Count < fmf.Capacity*8 then
  if clearBitmap(fmap) > 0 then begin

    fdc.Count:=0;

    for i:=0 to N do begin

      with lp[i] do begin
        b.X:=Trunc((X - flt.X)*fkx);
        b.Y:=Trunc((Y - flt.Y)*fky);
      end;

      if i > 0 then
      if (a.X = b.X) and (a.Y = b.Y) then
        fdc.Pixel(b.X,b.Y)
      else begin
        fdc.xLine(a,b);
        if Loc = 3 then fdc.xRib(a,b)
      end;

      a:=b
    end;

    if fdc.Count > 0 then begin

      if Loc = 3 then fdc.Fill;

      Fillchar(r,Sizeof(r),0);
      r.Ptr:=Ptr; r.Loc:=Loc;

      Max_Poly_Bound(lp,N+1,r.lt,r.rb);

      r.mf:=fmf.Count; r.ln:=N+1;
      fmf.Add_poly(lp,r.ln);

      l:=Strlen(Str);
      if l > Sizeof(r.Str)-1 then begin
        if fhf.Size = 0 then fhf.Append(l,4);

        if l > 1023 then l:=1023;
        r.hf:=fhf.Size; fhf.Append(l,2);
        fhf.Append(Str^,l)
      end;

      StrLCopy(r.Str,Str,Sizeof(r.Str)-1);

      Result:=Add(@r);
      if Result >= 0 then
      MapToZones(Result)
    end
  end
end;

procedure THintList.Pop(List: THintList);
var
  i: int; hp: PHintArray;
  hr: THintItem;
begin
  hp:=List.First;
  for i:=0 to List.Count-1 do begin
    hr:=hp[i];

    case hr.Loc of
  1:  with hr.lt do
      if (X >= flt.X) and (X <= frb.X) then
      if (Y >= flt.Y) and (Y <= frb.Y) then
      if id_Itemof(hr.Ptr) = nil then
      AddSign(hr.Ptr,hr.lt,hr.Str);
    end
  end
end;

function THintList.Pickup(const Alt,Arb: TPoint;
                          Str: PChar; smax: int;
                          out Pos: TPoint): bool;
var
  wp: PWords;
  hp: PHintArray; d,t,dx,dy: Int64;
  z: TIntegerList; zp: PIntegers;
  i,j,c,r,c1,c2,r1,r2,ok,bx: int; lp: PLPoly;
  _lt,_rb,pc,p: TPoint; hr: THintItem;
begin
  Result:=false;

  StrCopy(Str,''); Pos:=Point(0,0);

  r:=0; hp:=First;
  if fZones.Count > 0 then begin

    _lt:=Alt; _rb:=Arb;
    Middle_point(_lt,_rb,pc);

    c1:=Trunc((_lt.X - flt.X)*fkx);
    r1:=Trunc((_lt.Y - flt.Y)*fky);
    if c1 < 0 then c1:=0;
    if r1 < 0 then r1:=0;

    c2:=Round((_rb.X - flt.X)*fkx);
    r2:=Round((_rb.Y - flt.Y)*fky);
    if c2 >= fCols then c2:=fCols-1;
    if r2 >= fRows then r2:=fRows-1;

    for r:=r1 to r2 do
    for c:=c1 to c2 do begin
      z:=fzones[r * fCols + c] as TIntegerlist;
      zp:=z.First; for i:=0 to z.Count-1 do begin
        j:=zp[i]; hr:=hp[j];

        if hr.Ptr > 0 then begin

          p:=hr.lt; ok:=0;
          case hr.Loc of

        1:  if (p.X >= _lt.X) and (p.X <= _rb.X) then
            if (p.Y >= _lt.Y) and (p.Y <= _rb.Y) then
            ok:=1;
        2,
        3:  if (_lt.X < hr.rb.X) and (_rb.X > hr.lt.X) then
            if (_lt.Y < hr.rb.Y) and (_rb.Y > hr.lt.Y) then begin
              lp:=fmf.Items[hr.mf];
              if Assigned(lp) then begin
                if hr.mf+hr.ln > fmf.Count then
                hr.ln:=fmf.Count - hr.mf;

                p:=pc;
                if LocatePolyLine(lp,hr.ln-1,_lt,_rb,p) >= 0 then
                  ok:=1
                else
                if hr.Loc = 3 then
                if rPolygonContainsPixel(lp,hr.ln-1,pc.X,pc.Y) >= 0 then
                  ok:=1;
              end
            end;
          end;

          if ok = 1 then begin
            dx:=p.X-pc.X; dy:=p.Y-pc.Y;
            t:=dx*dx + dy*dy;

            if not Result or (t < d) then begin
              StrCopy(Str,hr.Str);

              if hr.hf > 0 then
              if smax > Sizeof(hr.Str)-1 then begin
                wp:=fhf.Get_Pointer(hr.hf); bx:=wp[0];
                if hr.hf+2+bx <= fhf.Size then begin
                  if bx > smax then bx:=smax;
                  StrLCopy(Str,PChar(@wp[1]),bx);
                end
              end;

              Pos:=p; Result:=true; d:=t;
              if d < 1 then Break
            end
          end
        end
      end
    end
  end
end;

function THintList.GetPoly(Ptr: int; out bp: PLPoly): int;
var
  hp: PHintItem; bx,cx: int;
begin
  Result:=0; bp:=nil;

  hp:=id_Itemof(Ptr);
  if Assigned(hp) then
  if hp.ln > 0 then begin
    bx:=hp.mf;

    bp:=fmf.Items[bx];
    if Assigned(bp) then begin
      cx:=hp.ln;
      if bx+cx > fmf.Count then
      cx:=fmf.Count - bx;
      Result:=cx
    end
  end
end;

function THintList.RemoveObject(Ptr: int): int;
var
  hp: PHintItem;
begin
  Result:=-1;
  hp:=id_Itemof(Ptr);
  if Assigned(hp) then begin
    hp.Ptr:=-Ptr; Result:=Ptr
  end
end;

constructor TVgcList.Create;
begin
  inherited Create(Sizeof(TVgcItem),256)
end;

function TVgcList.Up_item(p1,p2: Pointer): Boolean;
begin
  Result:=PVgcItem(p2).obj < PVgcItem(p1).obj
end;

function TVgcList.LoadFrom(Path: PChar): Integer;
var
  txt: TTextfile;
  r: TVgcItem; ax,bx: Integer;
begin
  Clear;

  txt:=TTextfile.Create;
  try
    if txt.Open_ext(Path,'.vgc') then
    while txt.xStrLine <> nil do
    if txt.x_Code(ax) then
    if txt.x_Int(bx) then
    if txt.x_Int(r.nn) then
    if txt.x_Int(r.val) then
    if txt.x_Int(r.color) then begin
      r.obj:=Cardinal(ax)*10 + bx;
      if bx = 1 then Inc(r.color,$8000);
      Add(@r);
    end;
  finally
    txt.Free
  end;

  Result:=Count
end;

procedure TVgcList.SaveAs(Path: PChar);
var
  txt: TTextfile;
  i,bx,loc: Integer; lp: PVgcArray;
  r: TVgcItem; s: String;
begin
  xFileErase(Path,'.vgc');

  txt:=TTextfile.Create;
  try
    if Count > 0 then
    if txt.Make_ext(Path,'.vgc') then begin

      lp:=First;
      for i:=0 to Count-1 do begin

        r:=lp[i];
        loc:=r.obj mod 10; bx:=r.color;
        if loc = 1 then bx:=bx and $7FFF;

        s:=Format('%s %d %d %d %d',
          [CodeToStr(r.obj div 10),loc,r.nn,r.val,bx]);

        txt.WriteStr(s);
      end
    end;
  finally
    txt.Free
  end
end;

function rumb(r,a,z: Double; az: Integer): trumb;
var
  v: trumb;
begin
  v.r:=r; v.a:=a; v.z:=z; v.az:=az;
  Result:=v
end;

constructor TRumbList.Create;
begin
  inherited Create(Sizeof(trumb),256)
end;

constructor TUpTextList.Create(AName: PChar);

function load_ini(AName: PChar): int;
var
  txt: TTextfile; r: TUpTextRec; loc: int;
begin
  txt:=TTextfile.Create;
  try
    if txt.Open_bin(AName) then
    while txt.xStrLine <> nil do begin

      Fillchar(r,Sizeof(r),0);

      if txt.x_Code(r.code) then
      if txt.x_int(r.loc) then
      if txt.x_Code(r.codeT) then
      if txt.x_int(r.nn) then
      if txt.x_int(r.dec) then begin

        if txt.x_Int(r.act) then
        if txt.x_Int(r.code1) then
        if txt.x_Int(r.code2) then
        txt.x_Single(r.mm);

        Add(@r)
      end
    end
  finally
    txt.Free
  end;

  Result:=Count
end;

begin
  inherited Create(Sizeof(TUpTextRec),64);
  load_ini(AName)
end;

function TUpTextList.Objectof(code,loc: int): PUpTextRec;
var
  i: int; lp: PUpTextArray;
begin
  Result:=nil; lp:=First;

  for i:=0 to Count-1 do
  if lp[i].code = code then
  if lp[i].loc = loc then begin
    Result:=@lp[i]; Break
  end
end;

constructor TKeyIntfList.Create;
begin
  inherited Create(sizeOf(TKeyIntfRec),16)
end;

destructor TKeyIntfList.Destroy;
begin
  Clear; inherited
end;

procedure TKeyIntfList.Clear;
var
  i: int; p: PKeyIntfRec;
begin
  p:=First;
  for i:=1 to Count-1 do begin
    IUnknown(p.Intf):=nil; Inc(p)
  end;

  inherited Clear
end;

procedure TKeyIntfList.deleteKey(Key: uint);
var
  p: PKeyIntfRec;
begin
  p:=id_Itemof(Key);
  if Assigned(p) then begin
    IUnknown(p.Intf):=nil;
    Delete(PtrToIndex(p))
  end
end;

end.