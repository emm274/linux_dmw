unit xclasses; interface

uses
  Classes,
  activeX,Math,LCLType,LCLIntf,
  otypes,ofiles,xlist,xintf,xvirts;

type
  TNotifyObj = class
    procedure notify(Sender: TObject; Cmd: Integer); virtual;
  end;

  TReadBuf = class
    constructor Create(ABuf: Pointer; ALen: Integer);

    function Skip(Alen: Integer): Integer;

    function Get_byte(out v: Byte): Boolean;
    function Get_word(out v: Word): Boolean;
    function Get_uint24(out v: Integer): Boolean;
    function Get_int(out v: Integer): Boolean;

    function ivalue(len: Integer): Integer;

  private
    fsi: PBytes;
    fcx: Integer;

  public
    property Buf: PBytes read fsi;
    property Len: Integer read fcx;
  end;

  XReadStream = class(TStream)

    constructor Create(Buf: Pointer; Size: Integer);

    procedure Assign(Buf: Pointer; Size: Integer);

    function Align_pos(pad: int): Boolean;

    function Get_pointer(offset: Integer): Pointer;

    function Get_data(var buf; ALen: Integer): Boolean;

    function Get_byte(out v: Byte): Boolean;
    function Get_word(out v: Word): Boolean; virtual;
    function Get_long(out v: Longint): Boolean; virtual;
    function Get_dword(out v: DWord): Boolean; virtual;
    function Get_float(out v: float): Boolean; virtual;

    function Get_wmac(out v: Word): Boolean; virtual;

    function Get_rational(out v: TRational): Boolean;
    function Get_single(out v: Single): Boolean;
    function Get_Double(out v: Single): Boolean;

    function get_utf(s: PChar; Maxlen: int): Boolean;

    function x_Byte(v: Byte): Boolean;
    function x_Word(v: Word): Boolean;

    function x_str(v: PChar; len: Integer): Boolean;

  private
    fMemory: Pointer;
    fSize,fPosition: Longint;
    fMemoryPtr: Pointer;

  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  XMemoryStream = class(XReadStream)
    destructor Destroy; override;
    procedure Clear;

  private
    fCapacity: Longint;

    procedure SetCapacity(AValue: Longint);

  public
    property Memory: Pointer read fMemory;
    property Capacity: Longint read fCapacity write SetCapacity; 
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure SetSize(NewSize: Longint); override;
  end;

  TFileDocument = class
    destructor Destroy; override;
    function doc_Create(Path: PChar; Max: Integer): Boolean;
    procedure doc_Close;

    function load_stm(const stm: IStream): Boolean;
    function copy_stm(const stg: IStorage; fn: PChar): Boolean;

  private
    fHandle: Integer;
    fCapacity: Integer;
    fCount: Integer;
    fReadOnly: Boolean;

    function add_stm(fn: PChar; len: Integer): Boolean;
    procedure hdr_Change;
  end;

  TPolyLPoints = class
    constructor Create;
    destructor Destroy; override;

    function x_Alloc: Boolean;
    procedure x_Free; virtual;

    function Realloc_Counters: Boolean; virtual;
    function Realloc_Points: Boolean; virtual;

    procedure x_Close; virtual;

    procedure Clear;
    function add_LPoly(lp: PLPoly; lp_N: Integer): Integer;
    function more_LPoly(lp: PLPoly; lp_N: Integer): Integer;
    function add_LLine(lp: PLLine): Integer;

    function Get_Item(It: Integer; lp: PLLine;
                      lp_Max: Integer): Integer;

    function Close_input(lp_i: Integer): Boolean;

  private
    fPoints_Max: Integer;
    fCounters_Max: Integer;

    fx_Points: PPolyLine;
    fx_Counters: PIntegers;
    fx_Count: Integer;

    fOnNewItem: TNotifyEvent;

    function Get_Active: Boolean;
    function Get_LPoints: PLLine;

    function Get_Line(It: Integer): PLPoly;
    function Get_Counter(It: Integer): Integer;

  public
    property Active: Boolean read Get_Active;

    property x_Points: PPolyLine read fx_Points;
    property x_LPoints: PLLine read Get_LPoints;

    property x_Counters: PIntegers read fx_Counters;
    property x_Count: Integer read fx_Count;

    property Points_Max: Integer read fPoints_Max
                                 write fPoints_Max;

    property Counters_Max: Integer write fCounters_Max;

    property Lines[I: Integer]: PLPoly read Get_Line;
    property Counters[I: Integer]: Integer read Get_Counter;
  end;

  XPolyLPoints = class(TPolyLPoints)
    function Realloc_Counters: Boolean; override;
    function Realloc_Points: Boolean; override;
  end;

  TMovieLPoints = class(TPolyLPoints)
    constructor Create;
    destructor Destroy; override;

  private
    fColors: TList;
    fColor: ColorRef;

    procedure NewItem(Sender: TObject);
    function GetColor(I: Integer): ColorRef;

  public
    property Color: ColorRef write fColor;
    property Colors[I: Integer]: ColorRef read GetColor;
  end;

  TPolyList = class(TPointList)
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    function SwapList: Boolean; override;

    procedure ClearParts;

    procedure Collapse;

    procedure Truncate(NewCount: Integer); override;

    function IsLarge(MaxCount: int): Boolean;

    procedure next_poly(lp: PLPoly;
                        hp: PIntegers;
                        lp_n: Integer);

    procedure next_point(const p: TPoint; z: PInteger);
    function Continue(const p: TPoint; z: PInteger): bool;

    function pop_point: Integer;

    procedure Cancel_contour;
    function End_contour: Integer;
    function End_lock: Integer;

    function Eps_contour(eps: double): Integer;

    procedure Filter_contour(d,r: float);

    procedure add_gpoly(gp: PGPoly;
                        gn: Integer;
                        gk: Float);

    function add_poly(lp: PLPoly;
                      hp: PIntegers;
                      lp_n: Integer): int; virtual;

    function Break_fix(lp: PLLine; vp: PVPoly; vn: int): Integer;

    function Swap_poly(Ind: Integer): Integer;

    function delete_poly(Ind: Integer): Integer;

    function seek_poly(Ind: Integer; out lp: PLPoly;
                       out hp: PIntegers): Integer;

    function get_poly(Ind: Integer;
                      lp: PLPoly; hp: PIntegers;
                      lp_Max: Integer): Integer;

    function get_line(Ind: Integer;
                      lp: PLLine; hp: PIntegers;
                      lp_Max: Integer): Integer;

    function Add_part(poly: TPointList;
                      Ind: Integer): Integer;

    function Get_part(poly: TPointList;
                      Ind: Integer): Integer;

    procedure MoveTo(X,Y: Integer);
    procedure LineTo(X,Y: Integer);

    procedure Draw_Poly(DC: HDC);

    procedure Fill_Walls(DC: HDC; Typ,dY: Integer);
    procedure Draw_Walls(DC: HDC; Typ,dY: Integer);
    procedure Draw_Edges(DC: HDC; Typ,dY: Integer);

    procedure xy_shift(dX,dY: Integer);

    function ContainsPoint(pX,pY: Double): bool;

    function Verify: bool;

    procedure cls1(d,r: float);

    function PartsEqual(Ind1,Ind2: int): bool;

  protected
    procedure Polygon(DC: HDC; lp: PPoly; N: Integer); virtual;

  private
    fHeights: TIntegerList;

    fCounters: TIntegerList;
    fCounter: Integer;

    fMinCounter: Integer;

    flt,frb: TPoint;

    function GetIsXyz: Boolean;

    procedure SetCounter(AValue: Integer);

    function Get_Vertex(I: Integer): VPoint;

    function Get_PartCount: Integer;

  public
    property Counter: Integer read fCounter write SetCounter;
    property MinCounter: Integer write fMinCounter;

    property PartCount: Integer read Get_PartCount;
    property Parts: TIntegerList read fCounters;

    property Heights: TIntegerList read fHeights;
    property Vertexes[I: Integer]: VPoint read Get_Vertex;

    property IsXyz: Boolean read GetIsXyz;
  end;

  TPolyListIntf = class(TInterfacedObject,IPolyList)
    constructor Create;
    destructor Destroy; override;

    function SeekPoly(Ind: int; out lp: PLPoly): int; stdcall;
    function GetParam(Ind1,Ind2: int): int; stdcall;

    procedure SetParam(Ind1,Ind2,Val: int);
    procedure AddPoly(lp: PLPoly; n: int);

  private
    fPoly: TPolyList;
    fData: TPointList;

  public
    property Poly: TPolyList read fPoly;          
  end;

  TPolyListb = class(TPolyList)
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

    function add_poly(lp: PLPoly;
                      hp: PIntegers;
                      lp_n: Integer): int; override;

    function seek_polyb(Ind: int;
                        out lp: PLPoly;
                        out hp: PIntegers;
                        bp: PLPoly): int;

    function get_polyb(Ind: int;
                       lp,bp: PLPoly;
                       hp: PIntegers;
                       lpMax: Int): int;

  private
    fBounds: TPointList;

    procedure _GetBound(Ind: int; bp: PLPoly;
                        lp: PLPoly; n: int);
  end;

  TInt64List = class
    constructor Create;
    destructor Destroy; override;

    procedure Clear; virtual;

    procedure Changed;

    function LoadFromTxt(Path: PChar): int;

    function Extend(NewCount: int): Boolean;
    function Truncate(NewCount: int): int;

    function Assign(List: TInt64List): Boolean;
    function Compare(List: TInt64List): Boolean;

    function Add(Item: Int64): int;
    function xAdd(cn,id: int): int;
    
    function Insert(Ind: Integer; Item: Int64): Integer;

    function Add_first(Item: Int64): Integer;

    function LoadBuffer(AItems: PInt64s;
                        ACount: Integer): Integer;

    function AddList(List: TInt64List): Integer;
    function AddList32(List: TIntegerList): Integer;

    procedure Delete(Index: Integer);
    function IndexOf(Item: Int64): Integer;
    function Remove(Item: Int64): Integer;

    function Pop(Ind: Integer): bool;

  private
    fCapacity: Integer;
    fCount: Integer;
    fbuf: PInt64s;

    fLocked: Longbool;
    fDuplicates: Longbool;
    fIsChanged: Longbool;

    fOnChanged: TNotifyEvent;

    function Realloc(NewCount: Integer): Boolean;

    function GetItem(I: Integer): Int64;
    procedure SetItem(I: Integer; V: Int64);

  public
    property Buffer: PInt64s read fbuf;

    property Locked: Longbool read fLocked
                              write fLocked;

    property Duplicates: Longbool read fDuplicates
                                  write fDuplicates;

    property IsChanged: Longbool read fIsChanged;

    property Capacity: Integer read fCapacity
                               write fCapacity;

    property Count: Integer read fCount;

    property Items[I: int]: Int64 read GetItem write SetItem; default;

    property OnChanged: TNotifyEvent write fOnChanged;
  end;

  TIndexList = class(TInt64List)
    constructor Create(AIsSorted: Boolean);

    function LoadForm(List: TIndexList): Integer;

    function id_Add(Id,Val: Cardinal): Integer;
    function id_Value(Id: Cardinal): Integer;

    function id_IndexOf(Id: Cardinal): Integer;

    function Get_id(Ind: Integer): Cardinal;

    function Max_id: Cardinal;
    function Max_value: Integer;

    function id_Update(Id,Val: Cardinal): Integer;
    function id_Update1(Id,Val: uint): int;

    function id_Delete(Id,Val: Cardinal): Integer;

    function stm_LoadFrom(const stg: IStorage;
                          Name: PChar): Integer;

    procedure stm_SaveAs(const stg: IStorage; Name: PChar);

    function id_Clear(MaxIndex: Integer;
                      Indexing: Boolean): Boolean;

    function id_Sort: Boolean;

  private
    fIsSorted: Longbool;

    function Get_Value(Ind: Integer): Cardinal;
    procedure Set_Value(Ind: Integer; V: Cardinal);

  public
    property IsSorted: Longbool read fIsSorted
                                write fIsSorted;

    property Values[Ind: Integer]: Cardinal read Get_Value
                                            write Set_Value;
  end;

  TIndexMem = class(TVirtMem)
    function Open(const doc: IStorage;
                  Name: PChar; rw: Boolean): Boolean;

    procedure fc_Update(fe: PIntegers; Count: Integer;
                        fc: Integer; IsUpdate: Boolean);

    procedure fe_Exclude(fe: Integer);

    function Get_fc(fc: PIntegers; Max: Integer;
                    fe: Integer): Integer;

    procedure attv_update(id,attv: Integer; IsUpdate: Boolean);

    function Get_attv(id: Integer): Integer;

  private
    function DeleteItem(ind,bot: longint): longint;
  end;

const
  mesh_brk = $80000000;
  mesh_inv = $40000000;

type
  TPolyMesh = class
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure AddFace(fp: PIntegers; fn: Integer);
    function Verify_faces: Integer;

  private
    fPoints: VPointList;
    fFaces: TIntegerList;
  public
    property Points: VPointList read fPoints;
    property Faces: TIntegerList read fFaces;
  end;

implementation

uses
  SysUtils,Graphics,
  convert,storage,
  xline,xpoly,xpens;

procedure TNotifyObj.notify(Sender: TObject; Cmd: Integer);
begin
end;

type
  tdoc_rec = record
    Name: array[0..7] of char;
    pos,len: Integer
  end;

constructor TReadBuf.Create(ABuf: Pointer; ALen: Integer);
begin
  inherited Create;
  fsi:=ABuf; fcx:=ALen
end;

function TReadBuf.Skip(Alen: Integer): Integer;
begin
  Alen:=Min(Alen,fcx);
  fsi:=@fsi[Alen]; Dec(fcx,Alen);
  Result:=fcx
end;

function TReadBuf.Get_byte(out v: Byte): Boolean;
begin
  Result:=false; v:=0;
  if fcx >= 1 then begin
    v:=fsi[0]; fsi:=@fsi[1]; Dec(fcx);
    Result:=true
  end
end;

function TReadBuf.Get_word(out v: Word): Boolean;
begin
  Result:=false; v:=0;
  if fcx >= 2 then begin
    v:=PWord(fsi)^;
    Dec(fcx,2); Result:=true
  end
  else fcx:=0
end;

function TReadBuf.Get_uint24(out v: Integer): Boolean;
begin
  Result:=false; v:=0;
  if fcx >= 3 then begin
    v:=0;
    tlong(v).b[0]:=fsi[0];
    tlong(v).b[1]:=fsi[1];
    tlong(v).b[2]:=fsi[2];
    Dec(fcx,3); Result:=true
  end
  else fcx:=0
end;

function TReadBuf.Get_int(out v: Integer): Boolean;
begin
  Result:=false; v:=0;
  if fcx >= 4 then begin
    v:=PInteger(fsi)^;
    Dec(fcx,4); Result:=true
  end
  else fcx:=0
end;

function TReadBuf.ivalue(len: Integer): Integer;
var
  i,v: Integer;
begin
  v:=0;
  if len <= fcx then begin
    for i:=0 to len-1 do begin
      tlong(v).b[i]:=fsi[0];
      fsi:=@fsi[1]; Dec(fcx);
    end
  end
  else fcx:=0;

  Result:=v
end;

constructor XReadStream.Create(Buf: Pointer; Size: Integer);
begin
  inherited Create; Assign(Buf,Size);
end;

procedure XReadStream.Assign(Buf: Pointer; Size: Integer);
begin
  fMemory:=Buf; fSize:=Size;
  fPosition:=0
end;

function XReadStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result:=0;

  if Assigned(fMemory) then
  if (FPosition >= 0) and (Count >= 0) then begin
    Result:=fSize - FPosition;
    if Result > 0 then begin
      if Result > Count then Result:=Count;
      Move(Pointer(TPointer(FMemory) + FPosition)^, Buffer, Result);
      Inc(FPosition, Result);
    end;
  end;
end;

function XReadStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
soFromBeginning: FPosition := Offset;
soFromCurrent: Inc(FPosition, Offset);
soFromEnd: FPosition := FSize + Offset;
  end;
  Result:=fPosition;
end;

function XReadStream.Align_pos(pad: int): Boolean;
begin
  fPosition:=int_Round(fPosition,pad);
  Result:=fPosition <= fSize
end;

function XReadStream.Get_pointer(offset: Integer): Pointer;
begin
  Result:=nil; if Assigned(fMemory) then

  if offset < 0 then
    Result:=@PBytes(fMemory)[fPosition]
  else
    Result:=@PBytes(fMemory)[offset]
end;

function XReadStream.Get_data(var buf; ALen: Integer): Boolean;
begin
  Result:=Read(buf,ALen) = ALen
end;

function XReadStream.Get_byte(out v: Byte): Boolean;
begin
  v:=0; Result:=Read(v,Sizeof(v)) = Sizeof(v)
end;

function XReadStream.Get_word(out v: Word): Boolean;
begin
  v:=0; Result:=Read(v,Sizeof(v)) = Sizeof(v)
end;

function XReadStream.Get_long(out v: Longint): Boolean;
begin
  v:=0; Result:=Read(v,Sizeof(v)) = Sizeof(v)
end;

function XReadStream.Get_dword(out v: DWord): Boolean;
begin
  v:=0; Result:=Read(v,Sizeof(v)) = Sizeof(v)
end;

function XReadStream.Get_float(out v: float): Boolean;
begin
  v:=0; Result:=Read(v,Sizeof(v)) = Sizeof(v)
end;

function XReadStream.Get_wmac(out v: Word): Boolean;
begin
  v:=0; Result:=Read(v,Sizeof(v)) = Sizeof(v);
  v:=SwapWord(v)
end;

function XReadStream.Get_rational(out v: TRational): Boolean;
begin
  v.c1:=0; v.c2:=1;
  Result:=Read(v,Sizeof(v)) = Sizeof(v)
end;

function XReadStream.Get_single(out v: Single): Boolean;
begin
  v:=0; Result:=Read(v,Sizeof(v)) = Sizeof(v)
end;

function XReadStream.Get_Double(out v: Single): Boolean;
begin
  v:=0; Result:=Read(v,Sizeof(v)) = Sizeof(v)
end;

function XReadStream.get_utf(s: PChar; Maxlen: int): Boolean;
var
  len: word; cx: int;
begin
  Result:=false; StrCopy(s,'');

  if get_word(len) then
  if Position+len <= fSize then begin

    cx:=MaxLen; if cx = 0 then cx:=255;
    if cx > len then cx:=len;

    if cx > 0 then Read(s^,cx);
    s[cx]:=#0;

    cx:=len-cx; if cx > 0 then
    Seek(cx,soFromCurrent);

    Result:=true
  end
end;

function XReadStream.x_Byte(v: Byte): Boolean;
var
  _v: Byte;
begin
  Result:=Get_byte(_v) and (_v = v)
end;

function XReadStream.x_Word(v: Word): Boolean;
var
  _v: Word;
begin
  Result:=Get_word(_v) and (_v = v)
end;

function XReadStream.x_str(v: PChar; len: Integer): Boolean;
var
  s: TShortstr;
begin
  Result:=false;
  if Get_data(s,len) then
  Result:=StrLIComp(s,v,len) = 0
end;

const
  MemoryDelta = $2000; { Must be a power of 2 }

destructor XMemoryStream.Destroy;
begin
  xFreePtr(fMemoryPtr); inherited
end;

procedure XMemoryStream.Clear;
begin
  fSize:=0; fPosition:=0;
end;

function XMemoryStream.Write(const Buffer; Count: Longint): Longint;
var
  Pos: Longint;
begin
  Result:=0;

  if (fPosition >= 0) and (Count >= 0) then begin
    Pos:=fPosition + Count;

    if Pos > fSize then SetSize(Pos);

    if Assigned(fMemory) then begin
      Move(Buffer,Pointer(TPointer(FMemory) + FPosition)^,Count);
      fPosition:=Pos; fSize:=Max(fSize,Pos); Result:=Count;
    end
  end;
end;

procedure XMemoryStream.SetCapacity(AValue: Longint);
begin
  if AValue > fCapacity then begin
    fCapacity:=int_Round(AValue,MemoryDelta);
    fMemoryPtr:=xReallocPtr(fMemoryPtr,fCapacity);

    fMemory:=fMemoryPtr;
    if fMemory = nil then begin
      fSize:=0; fCapacity:=0
    end
  end
end;

procedure XMemoryStream.SetSize(NewSize: Longint);
begin
  if NewSize < fCapacity then
    fSize:=NewSize
  else begin
    fCapacity:=int_Round(NewSize,MemoryDelta);
    fMemoryPtr:=xReallocPtr(fMemoryPtr,fCapacity);

    if Assigned(fMemoryPtr) then begin
      fMemory:=fMemoryPtr; fSize:=NewSize;
    end
    else begin
      fMemory:=nil; fSize:=0; fCapacity:=0
    end
  end;

  fPosition:=Min(fPosition,fSize)
end;

destructor TFileDocument.Destroy;
begin
  doc_Close
end;

function TFileDocument.doc_Create(Path: PChar; Max: Integer): Boolean;
var
  rec: tdoc_rec; i: Integer;
begin
  Result:=false; doc_Close;

  fHandle:=FileCreate(StrPas(Path));
  if fHandle > 0 then begin
    fCount:=0; fCapacity:=Max;
    hdr_Change; FillChar(rec,SizeOf(rec),0);
    for i:=1 to Max do
    FileWrite(fHandle,rec,SizeOf(rec));
    Result:=true
  end
end;

procedure TFileDocument.doc_Close;
begin
  if fHandle > 0 then FileClose(fHandle);
  fHandle:=0; fReadOnly:=false;
  fCount:=0; fCapacity:=0
end;

function TFileDocument.load_stm(const stm: IStream): Boolean;
const
  buf_size = 4096 * 4;
var
  len,loc: Integer; buf: PBytes; fn: TShortStr;
begin
  Result:=false;

  buf:=xAllocPtr(buf_size);
  if Assigned(buf) then begin

    len:=xSize(stm); if len > 0 then
    if stm_FileName(stm,fn) <> nil then
    if add_stm(fn,len) then begin

      xSeek(stm,0);
      FileSeek(fHandle,0,2);

      while len > 0 do begin
        loc:=Min(len,buf_size);
        xRead(stm,buf^,loc);
        FileWrite(fHandle,buf^,loc);
        Dec(len,loc)
      end

    end
  end; xFreePtr(buf)
end;

function TFileDocument.copy_stm(const stg: IStorage; fn: PChar): Boolean;
var
   stm: IStream;
begin
  Result:=false;
  if xOpenStream(stg,fn,false,stm) then
  Result:=load_stm(stm)
end;

function TFileDocument.add_stm(fn: PChar; len: Integer): Boolean;
var
  rec: tdoc_rec;
begin
  Result:=false;

  if fHandle > 0 then
  if not fReadOnly then
  if fCount < fCapacity then begin

    FillChar(rec,SizeOf(rec),0);
    StrCopy(rec.Name,fn);
    rec.pos:=xFileBot(fHandle);
    rec.len:=len;

    Inc(fCount);
    FileSeek(fHandle,fCount*SizeOf(rec),0);
    FileWrite(fHandle,rec,SizeOf(rec));

    hdr_Change; Result:=true
  end
end;

procedure TFileDocument.hdr_Change;
var
  hdr: tdoc_rec;
begin
  if fHandle > 0 then begin
    if not fReadOnly then begin
      FillChar(hdr,SizeOf(hdr),0);
      StrCopy(hdr.Name,'Document');
      hdr.pos:=fCount; hdr.len:=fCapacity;
      FileSeek(fHandle,0,0);
      FileWrite(fHandle,hdr,SizeOf(hdr))
    end
  end
end;

constructor TPolyLPoints.Create;
begin
  inherited;
  fPoints_Max:=32000;
  fCounters_Max:=4096;
end;

destructor TPolyLPoints.Destroy;
begin
  x_Free
end;

function TPolyLPoints.x_Alloc: Boolean;
begin
  if not Assigned(fx_Points) then begin
    fx_Points:=Alloc_PolyLine(fPoints_Max);
    fx_Counters:=xAllocInt(fCounters_Max);
  end;

  Clear; Result:=Active
end;

procedure TPolyLPoints.x_Free;
begin
  fx_Counters:=xFreePtr(fx_Counters);
  fx_Points:=xFreePtr(fx_Points)
end;

function TPolyLPoints.Realloc_Counters: Boolean;
begin
  Result:=false;
end;

function TPolyLPoints.Realloc_Points: Boolean;
begin
  Result:=false;
end;

procedure TPolyLPoints.x_Close;
begin
end;

procedure TPolyLPoints.Clear;
begin
  fx_Count:=0;
  if Assigned(fx_Points) then
  fx_Points.N:=0
end;

function TPolyLPoints.Get_Active: Boolean;
begin
  Result:=Assigned(fx_Points) and
          Assigned(fx_Counters)
end;

function TPolyLPoints.Get_LPoints: PLLine;
begin
  Result:=nil; if Assigned(fx_Points) then
  Result:=@PLLine(fx_Points).Pol
end;

function TPolyLPoints.Add_LPoly(lp: PLPoly; lp_N: Integer): Integer;
var
  cnt: Integer;
begin
  Result:=0; cnt:=lp_N+1;

  if cnt > 0 then begin

    if Assigned(fOnNewItem) then
    fOnNewItem(nil);

    if Assigned(fx_Points) then
    if Assigned(fx_Counters) then begin

      if fx_Count = fCounters_Max then
      if not Realloc_Counters then x_Close;

      if fx_Points.N+cnt > fPoints_Max then
      if not Realloc_Points then x_Close;

      if fx_Count < fCounters_Max then

      with fx_Points^ do
      if N+cnt <= fPoints_Max then begin
        fx_Counters[fx_Count]:=cnt; Inc(fx_Count);
        Move(lp^,Pol[N],cnt*SizeOf(TPoint));
        Inc(N,cnt); Result:=cnt
      end
    end
  end
end;

function TPolyLPoints.more_LPoly(lp: PLPoly; lp_N: Integer): Integer;
var
  i: Integer;
begin
  Result:=0; if fx_Count > 0 then
  for i:=0 to lp_N do with fx_Points^ do
  if N < fPoints_Max then begin
    Inc(fx_Counters[fx_Count-1]);
    Pol[N]:=lp[i]; Inc(N);
  end
end;

function TPolyLPoints.add_LLine(lp: PLLine): Integer;
begin
  Result:=Add_LPoly(@lp.Pol,lp.N)
end;

function TPolyLPoints.Close_input(lp_i: Integer): Boolean;
var
  cnt: Integer;
begin
  Result:=false;

  if lp_i >= 0 then
  if lp_i < fx_Points.N then begin
    cnt:=fx_Points.N-lp_i;

    if fx_Count < fCounters_Max then begin
      fx_Counters[fx_Count]:=cnt; Inc(fx_Count);
      Result:=true
    end;

    if not Result then
    fx_Points.N:=lp_i
  end
end;

function TPolyLPoints.Get_Item(It: Integer; lp: PLLine;
                               lp_Max: Integer): Integer;
var
  i,cnt,ind: Integer;
begin
  Result:=0; lp.N:=-1;

  if It >= 0 then
  if It < fx_Count then begin
    cnt:=fx_Counters[It];

    ind:=0; for i:=0 to It-1 do
    Inc(ind,fx_Counters[i]);

    with fx_Points^ do
    if ind+cnt <= N then begin
      if cnt > lp_Max+1 then cnt:=lp_Max;
      Move(Pol[ind],lp.Pol,cnt*Sizeof(TPoint));
      lp.N:=cnt-1; Result:=cnt
    end
  end
end;

function TPolyLPoints.Get_Line(It: Integer): PLPoly;
var
  i,cnt,ind: Integer;
begin
  Result:=nil;

  if It >= 0 then
  if It < fx_Count then begin
    cnt:=fx_Counters[It];

    ind:=0; for i:=0 to It-1 do
    Inc(ind,fx_Counters[i]);

    with fx_Points^ do
    if ind+cnt <= N then
    Result:=@Pol[ind]
  end
end;

function TPolyLPoints.Get_Counter(It: Integer): Integer;
begin
  Result:=0;
  if It >= 0 then
  if It < fx_Count then
  Result:=fx_Counters[It]
end;

function XPolyLPoints.Realloc_Counters: Boolean;
var
  buf: Pointer; cnt,len: Integer;
begin
  Result:=false;

  cnt:=fCounters_Max+1024;
  len:=cnt * SizeOf(Integer);

  buf:=xReAllocPtr(fx_Counters,len);
  if Assigned(buf) then begin
    fCounters_Max:=cnt;
    fx_Counters:=buf;
    Result:=true
  end
end;

function XPolyLPoints.Realloc_Points: Boolean;
var
  buf: Pointer; cnt,len: Integer;
begin
  Result:=false;

  cnt:=fPoints_Max+1024;
  len:=(cnt+1) * SizeOf(TPoint);

  buf:=xReAllocPtr(fx_Points,len);
  if Assigned(buf) then begin
    fPoints_Max:=cnt;
    fx_Points:=buf;
    Result:=true
  end
end;

constructor TMovieLPoints.Create;
begin
  inherited Create;
  fColors:=TList.Create;
  fOnNewItem:=NewItem;
  fColor:=clBlue
end;

destructor TMovieLPoints.Destroy;
begin
  fColors.Free; inherited
end;

procedure TMovieLPoints.NewItem(Sender: TObject);
begin
  fColors.Add(Pointer(fColor))
end;

function TMovieLPoints.GetColor(I: Integer): ColorRef;
begin
  Result:=fColor; if I >= 0 then
  if I < fColors.Count then
  Result:=TColor(fColors[I])
end;

constructor TPolyList.Create;
begin
  inherited Create(1024);
  fHeights:=TIntegerList.Create;
  fCounters:=TIntegerList.Create;
  fMinCounter:=2
end;

destructor TPolyList.Destroy;
begin
  fCounters.Free;
  fHeights.Free;
  inherited
end;

procedure TPolyList.Clear;
begin
  fCounters.Clear; fCounter:=0;
  fHeights.Clear; inherited Clear;
end;

function TPolyList.SwapList: Boolean;
var
  cn: int;
begin
  cn:=fCounters.Count;
  End_contour;

  Result:=inherited SwapList;
  if Result then begin
    fHeights.SwapList;
    fCounters.SwapList;

    if cn = 0 then begin
      fCounters.Clear;
      fCounter:=Count
    end
  end
end;

function TPolyList.GetIsXyz: Boolean;
begin
  Result:=fHeights.Count = Count
end;

procedure TPolyList.ClearParts;
begin
  fCounters.Clear; fCounter:=Count
end;

procedure TPolyList.Collapse;
begin
  if fCounters.Count > 1 then begin
    fCounters.Truncate(1);
    fCounters[0]:=Count
  end
end;

procedure TPolyList.Truncate(NewCount: Integer);
var
  i,dn,cx: int; cp: PIntegers;
begin
  if NewCount < Count then

  if NewCount <= 0 then
    Clear
  else begin
    dn:=Count-NewCount;
    inherited Truncate(NewCount);
    fHeights.Truncate(NewCount);

    if fCounter >= dn then
      Dec(fCounter,dn)
    else begin
      Dec(dn,fCounter);
      fCounter:=0;

      cp:=fCounters.First;
      i:=fCounters.Count-1;
      while i >= 0 do begin
        cx:=cp[i];
        if cx > dn then begin
          cp[i]:=cx-dn; Break
        end;

        Dec(dn,cx);
        fCounters.Delete(i);
        Dec(i)
      end
    end
  end
end;

procedure TPolyList.SetCounter(AValue: Integer);
begin
  if AValue >= 0 then
  while AValue < fCounter do begin

    if fHeights.Count = Count then
    fHeights.Delete_last;

    Delete_last; Dec(fCounter)
  end
end;

function TPolyList.Get_PartCount: Integer;
begin
  Result:=fCounters.Count
end;

function TPolyList.Get_Vertex(I: Integer): VPoint;
var
  v: VPoint; p: PPoint;
begin
  v.x:=0; v.y:=0; v.z:=0;
  p:=Items[I]; if Assigned(p) then begin
    v.x:=p.X; v.y:=p.Y;
    if fHeights.Count = Count then
    v.z:=fHeights[I]
  end; Result:=v
end;

function TPolyList.IsLarge(MaxCount: int): Boolean;
var
  i,n,loc,len: int; cp: PIntegers;
  lp: PLPoly; hp: PIntegers;
begin
  Result:=false; cp:=fCounters.First;
  for i:=0 to fCounters.Count-1 do begin
    n:=cp[i];

    if MaxCount > 0 then begin
      if n > MaxCount then begin
        Result:=true; Break
      end
    end else

    if n > 8000 then begin

      n:=Seek_Poly(i,lp,hp);

      loc:=8; if Assigned(hp) then loc:=12;
      len:=loc * n;

      if Poly_Pack(lp,hp,n-1) then
      len:=loc + (len div 2);

      if len > 64000 then begin
        Result:=true; Break
      end
    end
  end
end;

function TPolyList.Swap_poly(Ind: Integer): Integer;
var
  lp: PLPoly; hp: PIntegers;
begin
  Result:=Seek_poly(Ind,lp,hp);
  if Result > 1 then Swap_LPoly(lp,hp,Result-1)
end;

function TPolyList.delete_poly(Ind: Integer): Integer;
var
  i,j: Integer; cp: PIntegers;
begin
  Result:=-1;

  if Ind >= 0 then
  if Ind < fCounters.Count then begin

    Result:=Ind; j:=0;

    cp:=fCounters.First;
    for i:=0 to Ind-1 do begin
      Inc(j,cp[0]); cp:=@cp[1];
    end;

    Delete_range(j,cp[0]);
    fHeights.Delete_range(j,cp[0]);
    fCounters.Delete(Ind)
  end
end;

function TPolyList.seek_poly(Ind: Integer; out lp: PLPoly;
                             out hp: PIntegers): Integer;
var
  i,n: Integer; cp: PIntegers;
  lp1: PLPoly; hp1: PIntegers;
begin
  Result:=-1; lp:=nil; hp:=nil;

  if Ind < 0 then begin

    if fCounter > 0 then begin

      lp1:=First; lp:=@lp1[Count - fCounter];

      if fHeights.Count = Count then begin
        hp1:=fHeights.First; hp:=@hp1[Count - fCounter];
      end;

      Result:=fCounter
    end

  end else

  if Ind < fCounters.Count then begin

    if fHeights.Count <> Count then
    fHeights.Clear; hp1:=fHeights.First;

    lp1:=First; cp:=fCounters.First;

    for i:=0 to Ind-1 do begin
      n:=cp[0]; lp1:=@lp1[n];
      if hp1 <> nil then hp1:=@hp1[n];
      cp:=@cp[1]
    end;

    lp:=lp1; hp:=hp1; Result:=cp[0]
  end
end;

function TPolyList.get_poly(Ind: Integer;
                            lp: PLPoly; hp: PIntegers;
                            lp_Max: Integer): Integer;
var
  n: Integer;
  lp1: PLPoly; hp1: PIntegers;
begin
  Result:=-1;

  n:=Seek_poly(Ind,lp1,hp1);

  if n > 0 then
  if n <= lp_Max then begin

    if Assigned(lp) then
    Move(lp1^,lp^,n*Sizeof(TPoint));

    if Assigned(hp) then
    if Assigned(hp1) then
    Move(hp1^,hp^,n*Sizeof(Integer));

    Result:=n-1
  end
end;

function TPolyList.get_line(Ind: Integer;
                            lp: PLLine; hp: PIntegers;
                            lp_Max: Integer): Integer;
begin
  lp.N:=get_poly(Ind,@lp.Pol,hp,lp_Max);
  Result:=lp.N
end;

function TPolyList.Get_part(poly: TPointList;
                            Ind: Integer): Integer;
var
  n: Integer;
  lp: PLPoly; hp: PIntegers;
begin
  poly.Clear;

  n:=Seek_poly(Ind,lp,hp);
  if n > 1 then
  poly.Insert_range(lp,-1,n);

  Result:=poly.Count
end;

function TPolyList.Add_part(poly: TPointList;
                            Ind: Integer): Integer;
var
  n: Integer;
  lp: PLPoly; hp: PIntegers;
begin
  Result:=0;

  n:=Seek_poly(Ind,lp,hp);
  if n > 1 then begin
    poly.Insert_range(lp,-1,n);
    Result:=n
  end
end;

procedure TPolyList.next_poly(lp: PLPoly;
                              hp: PIntegers;
                              lp_n: Integer);
begin
  if lp_n > 0 then begin
    if Assigned(hp) then
    fHeights.Insert_range(hp,-1,lp_n);

    Insert_range(lp,-1,lp_n);
    Inc(fCounter,lp_n)
  end
end;

procedure TPolyList.next_point(const p: TPoint; z: PInteger);
begin
  Add(@p); if Assigned(z) then
  fHeights.Add(z); Inc(fCounter)
end;

function TPolyList.Continue(const p: TPoint; z: PInteger): bool;
var
  lp: PPoint; zp: PInteger;
begin
  Result:=true;

  if fCounter > 0 then begin
    lp:=Last;

    zp:=nil;
    if Assigned(z) then
    zp:=fHeights.Last;

    if Assigned(lp) then
    if lp.X = p.X then
    if lp.Y = p.Y then
    if zp = nil then
      Result:=false
    else
    if zp^ = z^ then
      Result:=false
  end;

  if Result then begin
    Add(@p); if Assigned(z) then
    fHeights.Add(z); Inc(fCounter)
  end
end;

function TPolyList.pop_point: Integer;
begin
  if fCounter > 0 then begin
    Delete_last;
    if fHeights.Count > Count then
    fHeights.Delete_last;

    Dec(fCounter)
  end; Result:=fCounter
end;

procedure TPolyList.Cancel_contour;
var
  n: int;
begin
  if fCounter > 0 then begin
    n:=Count-fCounter;
    if fHeights.Count = Count then
      fHeights.Truncate(n)
    else
      fHeights.Clear;

    Truncate(n)
  end;

  fCounter:=0;
end;

function TPolyList.End_contour: Integer;
begin
  Result:=0;

  if fCounter > 0 then

  if fCounter < fMinCounter then
    Cancel_contour
  else begin
    fCounters.Add(@fCounter);
    Result:=fCounter;
  end;

  fCounter:=0;
end;

function TPolyList.End_lock: Integer;
var
  lp: PLPoly; zp: PInteger; n: int;
begin
  Result:=0; n:=fCounter;
  if n < 3 then
    Cancel_contour
  else begin
    lp:=Items[Count-n];
    if not Points_Equal(lp[0],lp[n-1]) then begin
      Add(@lp[0]);

      zp:=fHeights.Items[Count-n];
      if Assigned(zp) then
      fHeights.Add(zp);

      Inc(fCounter);
    end;

    Result:=End_contour
  end
end;

function TPolyList.Eps_contour(eps: double): Integer;
var
  lp: PLPoly; n1,n2,dn: int;
begin
  n1:=fCounter;
  if n1 > 1 then begin
    lp:=Items[Count-n1];
    n2:=eps_LPoly(lp,n1-1,eps)+1;
    dn:=n1-n2; if dn > 0 then begin
      n2:=Count-dn;
      inherited Truncate(n2);
      fHeights.Truncate(n2);
      Dec(fCounter,dn)
    end
  end;

  Result:=fCounter
end;

procedure TPolyList.Filter_contour(d,r: float);
var
  lp: PLPoly; n,dn: int;
begin
  n:=fCounter;
  if n > 1 then begin
    lp:=Items[Count - n];
    n:=cls_LPoly(lp,n-1, d,r)+1;

    dn:=fCounter - n;
    if dn > 0 then begin

      if fHeights.Count = Count then
      fHeights.Truncate(Count-dn);

      Truncate(Count-dn);
      fCounter:=n
    end
  end
end;

procedure TPolyList.add_gpoly(gp: PGPoly;
                              gn: Integer;
                              gk: Float);
var
  i: Integer; lp: PLPoly;
begin
  End_contour;

  if gn > 1 then begin

    i:=Count;
    if not Extend(i + gn) then
      Clear
    else begin
      fHeights.Clear;

      lp:=Items[i];
      for i:=0 to gn-1 do begin
        lp[i].X:=Round(gp[i].x*gk);
        lp[i].Y:=Round(gp[i].y*gk);
      end;

      fCounters.AddItem(gn)
    end
  end
end;

function TPolyList.add_poly(lp: PLPoly;
                            hp: PIntegers;
                            lp_n: Integer): int;
begin
  Result:=-1; End_contour;

  if lp_n > 0 then begin
    if Assigned(hp) then
    fHeights.Insert_range(hp,-1,lp_n);

    Insert_range(lp,-1,lp_n);
    Result:=fCounters.AddItem(lp_n)
  end
end;

function TPolyList.Break_fix(lp: PLLine; vp: PVPoly; vn: int): Integer;
var
  i: int; p: TPoint; v: VPoint;
begin
  Clear; fMinCounter:=2;

  if lp.N > 0 then begin

    next_point(lp.Pol[0],nil);

    v:=_VPoint(0,0,0);
    while v.z = 0 do begin
      if vn = 0 then Break;
      v:=vp[0]; vp:=@vp[1]; Dec(vn)
    end;

    for i:=1 to lp.N do begin

      if i = Abs(v.z) then begin
        p:=Point(v.x,v.y);
        next_point(p,nil);
        End_contour;

        if v.z < 0 then
        next_point(p,nil);

        if vn > 0 then begin
          v:=vp[0]; vp:=@vp[1]; Dec(vn)
        end
      end;

      next_point(lp.Pol[i],nil)
    end;

    End_contour
  end;

  Result:=PartCount
end;

function TPolyList.ContainsPoint(pX,pY: Double): bool;
var
  lp: PLPoly; cp: PIntegers; i: Integer;
begin
  Result:=false;

  lp:=First; cp:=fCounters.First;

  if Assigned(lp) then
  if Assigned(cp) then

  if _rPolygonContainsPixel(lp,cp[0]-1,pX,pY,nil) >= 0 then begin

    Result:=true;
    for i:=1 to fCounters.Count-1 do begin
      lp:=@lp[cp[0]]; cp:=@cp[1];
      if _rPolygonContainsPixel(lp,cp[0]-1,pX,pY,nil) >= 0 then
      begin Result:=false; Break end
    end
  end
end;

procedure TPolyList.MoveTo(X,Y: Integer);
var
  p: TPoint;
begin
  if fCounter > 0 then
  if fCounter = 1 then Delete(Count-1)
  else fCounters.Add(@fCounter);

  p.X:=X; p.Y:=Y; Add(@p);
  fCounter:=1;
end;

procedure TPolyList.LineTo(X,Y: Integer);
var
  p: TPoint;
begin
  p.X:=X; p.Y:=Y; Add(@p); Inc(fCounter)
end;

procedure TPolyList.Draw_poly(DC: HDC);
begin
  if fCounter > 0 then
  if fCounter = 1 then Delete(Count-1)
  else fCounters.Add(@fCounter);
  fCounter:=0;

  if DC <> 0 then begin

    if Count > 0 then
    if fCounters.Count > 0 then

    dc_PolyPolyLine(DC,First,Count,
                    fCounters.First,
                    fCounters.Count)
  end
end;

procedure TPolyList.Fill_Walls(DC: HDC; Typ,dY: Integer);
var
  lp: PLPoly; cp: PIntegers;
  i,n: Integer; r: LOrient;
begin
  Draw_poly(0);

  cp:=fCounters.First; lp:=First;
  for i:=1 to fCounters.Count do begin
    n:=cp[0]; cp:=@cp[1];

    while n > 1 do begin
      r[0]:=lp[0]; r[1]:=lp[1];
      r[2]:=lp[1]; r[3]:=lp[0];
      r[4]:=r[0]; Dec(n); lp:=@lp[1];

      if Typ = 2 then begin
        Inc(r[2].Y,dY); Inc(r[3].Y,dY)
      end
      else begin
        r[2].Y:=dY; r[3].Y:=dY
      end;

      Polygon(DC,@r,4);
    end;

    if n > 0 then lp:=@lp[n]
  end;
end;

procedure TPolyList.Draw_Walls(DC: HDC; Typ,dY: Integer);
var
  i: Integer; lp: PLPoly;
begin
  Draw_poly(DC);
  Draw_Edges(DC,Typ,dY);

  if Typ = 2 then
    xy_shift(0,dY)
  else begin
    lp:=First;
    for i:=1 to Count do begin
      lp[0].Y:=dY; lp:=@lp[1]
    end
  end;

  Draw_poly(DC)
end;

procedure TPolyList.Draw_Edges(DC: HDC; Typ,dY: Integer);
var
  i: Integer; lp: PLPoly;
begin
  lp:=First;
  for i:=1 to Count do begin

    with lp[0] do begin
      MoveToEx(DC,X,Y,nil);
      if Typ = 2 then
        LCLIntf.LineTo(DC,X,Y+dY)
      else
        LCLIntf.LineTo(DC,X,dY)
    end;

    lp:=@lp[1]
  end
end;

procedure TPolyList.xy_shift(dX,dY: Integer);
var
  i: Integer; lp: PLPoly;
begin
  lp:=First;
  for i:=1 to Count do begin
    Inc(lp[0].X,dX);
    Inc(lp[0].Y,dY);
    lp:=@lp[1]
  end
end;

procedure TPolyList.Polygon(DC: HDC; lp: PPoly; N: Integer);
begin
  LCLIntf.Polygon(DC,lp^,N+1,false)
end;

function TPolyList.Verify: bool;
var
  i,n: int; cp: PIntegers;
begin
  n:=fCounter; cp:=fCounters.First;
  for i:=0 to fCounters.Count-1 do
  Inc(n,cp[i]); Result:=n = Count
end;

procedure TPolyList.cls1(d,r: float);
var
  cn,i,n1,n2,dn,p: int;
  cp,hp: PIntegers; lp: PLPoly;
begin
  cn:=fCounters.Count;
  End_contour; fHeights.Clear;

  lp:=First; dn:=0;

  cp:=fCounters.First; p:=0;
  for i:=0 to fCounters.Count-1 do begin

    n1:=cp[i]; n2:=n1;
    if n2 > 1 then begin

      n2:=cls_LPoly(@lp[p-dn],n2+dn-1,d,r)-dn+1;

      Dec(n1,n2);
      if n1 > 0 then begin
        cp[i]:=n2;
        Delete_range(p+n2,n1);
      end
    end;

    dn:=1; Inc(p,n2)
  end;

  if cn = 0 then begin
    fCounters.Clear;
    fCounter:=Count
  end;

  Verify
end;

function TPolyList.PartsEqual(Ind1,Ind2: int): bool;
var
  i,n1,n2: int; lp1,lp2: PLPoly; hp1,hp2: PIntegers;
begin
  Result:=false;

  n1:=Seek_poly(Ind1,lp1,hp1);
  n2:=Seek_poly(Ind2,lp2,hp2);

  if (n1 > 0) and (n1 = n2) then begin
    Result:=true; for i:=0 to n1-1 do
    if not Points_Equal(lp1[i],lp2[i]) then
    begin Result:=false; Break end
  end
end;

constructor TPolyListIntf.Create;
begin
  inherited;
  fPoly:=TPolyList.Create;
  fData:=TPointList.Create(256)
end;

destructor TPolyListIntf.Destroy;
begin
  fData.Free;
  fPoly.Free;
  inherited
end;

function TPolyListIntf.SeekPoly(Ind: int; out lp: PLPoly): int;
var
  hp: PIntegers;
begin
  Result:=fPoly.seek_poly(Ind,lp,hp)
end;

function TPolyListIntf.GetParam(Ind1,Ind2: int): int;
var
  id: int; p: PPoint;
begin
  Result:=0;

  if Ind1 = -1 then
    Result:=fPoly.PartCount
  else begin
    id:=Ind1*10000 + Ind2;
    p:=fData.id_Itemof(id);
    if Assigned(p) then Result:=p.Y
  end
end;

procedure TPolyListIntf.SetParam(Ind1,Ind2,Val: int);
var
  id: int; p: PPoint;
begin
  id:=Ind1*10000 + Ind2;
  p:=fData.id_Itemof(id);
  if Assigned(p) then p.Y:=Val
  else fData.AddItem(id,Val)
end;

procedure TPolyListIntf.AddPoly(lp: PLPoly; n: int);
begin
  fPoly.add_poly(lp,nil,n)
end;

constructor TPolyListb.Create;
begin
  inherited Create;
  fBounds:=TPointList.Create(1024)
end;

destructor TPolyListb.Destroy;
begin
  fBounds.Free;
  inherited
end;

procedure TPolyListb.Clear;
begin
  fBounds.Clear;
  inherited
end;

function TPolyListb.add_poly(lp: PLPoly;
                             hp: PIntegers;
                             lp_n: Integer): int;
var
  lt,rb: TPoint;
begin
  Result:=inherited add_poly(lp,hp,lp_n);
  if Result >= 0 then begin
    Max_Poly_Bound(lp,lp_n,lt,rb);
    fBounds.Add(@lt); fBounds.Add(@rb);
  end
end;

procedure TPolyListb._GetBound(Ind: int; bp: PLPoly;
                               lp: PLPoly; n: int);
var
  pp: PLPoly;
begin
  if n > 0 then
  if Assigned(bp) then

  if fBounds.Count div 2 <> Parts.Count then
    Max_Poly_Bound(lp,n,bp[0],bp[1])
  else begin
    pp:=fBounds.Items[Ind+Ind];
    bp[0]:=pp[0]; bp[1]:=pp[1];
  end
end;

function TPolyListb.seek_polyb(Ind: int;
                               out lp: PLPoly;
                               out hp: PIntegers;
                               bp: PLPoly): int;
begin
  Result:=Seek_poly(Ind,lp,hp);
  if Result > 0 then
  if Assigned(bp) then
  _GetBound(Ind,bp,lp,Result)
end;

function TPolyListb.get_polyb(Ind: int;
                              lp,bp: PLPoly;
                              hp: PIntegers;
                              lpMax: Int): int;
begin
  Result:=get_poly(Ind,lp,hp,lpMax);
  if Result > 0 then
  if Assigned(bp) then
  _GetBound(Ind,bp,lp,Result)
end;

constructor TInt64List.Create;
begin
  inherited;
  fCapacity:=1024;
  fDuplicates:=true
end;

destructor TInt64List.Destroy;
begin
  xFreePtr(fbuf); inherited
end;

procedure TInt64List.Clear;
begin
  fCount:=0; fIsChanged:=false
end;

function TInt64List.Extend(NewCount: int): Boolean;
begin
  Result:=Realloc(NewCount);
  if Result then fCount:=NewCount
end;

function TInt64List.Truncate(NewCount: int): int;
begin
  if NewCount < fCount then
  fCount:=NewCount; Result:=fCount;
end;

procedure TInt64List.Changed;
begin
  if Assigned(fOnChanged) then
  fOnChanged(nil)
end;

function TInt64List.LoadFromTxt(Path: PChar): int;
var
  txt: TTextFile; v: Int64;
begin
  Clear;

  txt:=TTextFile.Create;
  try
    if txt.Open(Path) then
    while txt.xStrLine <> nil do
    if txt.x_Int64(v) then Add(v)
  finally
    txt.Free
  end;

  Result:=Count
end;

function TInt64List.Assign(List: TInt64List): Boolean;
var
  i: Integer;
begin
  Result:=false; Clear;
  if Realloc(List.Count) then begin
    for i:=0 to List.Count-1 do
    fbuf[i]:=List.Items[i];
    fCount:=List.Count;
    Result:=true
  end;

  fIsChanged:=true
end;

function TInt64List.Compare(List: TInt64List): Boolean;
var
  cx: Integer;
begin
  Result:=false;
  if fCount = List.Count then begin
    cx:=fCount * Sizeof(Int64);
    Result:=CompareMem(fbuf,List.fbuf,cx)
  end
end;

function TInt64List.Realloc(NewCount: Integer): Boolean;
var
  cnt,len: Integer;
begin
  Result:=false;

  cnt:=int_Round(NewCount,1024);

  if fbuf = nil then begin
    fCapacity:=cnt; fbuf:=xAllocInt64(cnt);
  end else
  if NewCount > fCapacity then begin
    len:=cnt * Sizeof(Int64);
    fbuf:=xReAllocPtr(fbuf,len);
    if Assigned(fbuf) then
    fCapacity:=cnt;
  end;

  if Assigned(fbuf) then
  Result:=fCapacity >= NewCount
end;

function TInt64List.GetItem(I: Integer): Int64;
begin
  Result:=0; if I >= 0 then
  if I < fCount then Result:=fbuf[I]
end;

procedure TInt64List.SetItem(I: Integer; V: Int64);
begin
  if (I >= 0) and (I < fCount) then fbuf[I]:=V;
  fIsChanged:=true
end;

function TInt64List.Pop(Ind: Integer): bool;
var
  I: Integer; V: Int64;
begin
  Result:=false;

  if Ind > 0 then
  if Ind < fCount then begin
    V:=fbuf[Ind];
    for I:=Ind downto I+1 do
    fbuf[I]:=fbuf[I-1];

    fbuf[0]:=V; Result:=true;
  end
end;

function TInt64List.Add(Item: Int64): int;
begin
  Result:=-1;

  if not fDuplicates then
  Result:=IndexOf(Item);

  if Result < 0 then
  if Realloc(fCount+1) then begin
    Result:=fCount; fbuf[Result]:=Item;
    Inc(fCount); fIsChanged:=true
  end
end;

function TInt64List.xAdd(cn,id: int): int;
var
  t: TInt64;
begin
  t.cn:=cn; t.id:=id; Result:=Add(t.x)
end;

function TInt64List.Insert(Ind: Integer; Item: Int64): Integer;
var
  i: Integer;
begin
  Result:=-1;

  if not fDuplicates then
  Result:=IndexOf(Item);

  if Result < 0 then

  if (Ind < 0) or (Ind >= fCount) then
    Result:=Add(Item)
  else
  if Realloc(fCount+1) then begin
    Inc(fCount);
    for i:=fCount downto Ind+1 do
    fbuf[i]:=fbuf[i-1];

    Result:=Ind; fbuf[Ind]:=Item;
    fIsChanged:=true;
  end
end;

function TInt64List.Add_first(Item: Int64): Integer;
begin
  Clear; Result:=Add(Item)
end;

function TInt64List.LoadBuffer(AItems: PInt64s;
                               ACount: Integer): Integer;
var
  n: Integer;
begin
  Clear; n:=ACount;
  if (n > 0) and Realloc(n) then begin
    Move(AItems^,fbuf^,n * Sizeof(Int64));
    fCount:=n; fIsChanged:=true
  end
end;

function TInt64List.AddList(List: TInt64List): Integer;
var
  I: Integer;
begin
  Result:=0;
  for I:=0 to List.Count-1 do
  if Add(List.Items[I]) >= 0 then
  Inc(Result)
end;

function TInt64List.AddList32(List: TIntegerList): Integer;
var
  i: Integer; lp: PIntegers;
begin
  Result:=0; lp:=List.First;
  for I:=1 to List.Count do begin
    if Add(lp[0]) >= 0 then Inc(Result);
    lp:=@lp[1]
  end
end;

procedure TInt64List.Delete(Index: Integer);
var
  cx: Integer;
begin
  if Index >= 0 then
  if Index < fCount then begin
    cx:=(fCount-Index-1)*SizeOf(Int64);
    if cx > 0 then Move(fbuf[Index+1],fbuf[Index],cx);
    Dec(fCount); fIsChanged:=true
  end
end;

function TInt64List.IndexOf(Item: Int64): Integer;
var
  I: Integer; P: PInt64s;
begin
  Result:=-1; P:=fbuf;
  for I:=0 to fCount-1 do begin
    if P[0] = Item then begin
      Result:=I; Break
    end; P:=@P[1]
  end
end;

function TInt64List.Remove(Item: Int64): Integer;
begin
  Result:=IndexOf(Item);
  if Result >= 0 then Delete(Result)
end;

constructor TIndexList.Create(AIsSorted: Boolean);
begin
  inherited Create;
  fIsSorted:=AIsSorted;
end;

function TIndexList.LoadForm(List: TIndexList): Integer;
var
  len: Integer;
begin
  Clear;

  if Realloc(List.Count) then begin
    len:=Sizeof(Int64) * List.Count;
    Move(List.fbuf^,fbuf^,len);
    fCount:=List.Count
  end;

  Result:=Count
end;

function TIndexList.id_Add(Id,Val: Cardinal): Integer;
var
  v,t: TInt64; i,i1,i2,ii,cx: Integer; p: PInt64s;
begin
  v.c[0]:=Id; v.c[1]:=Val;
  Result:=Add(v.x);

  if (Result > 0) and fIsSorted then

  if true then begin

    i1:=0; i2:=Result-1;

    while i1 <= i2 do begin
      ii:=(i1 + i2) div 2; t.x:=fbuf[ii];
      if t.c[0] < Id then i1:=ii+1 else
      if t.c[0] > Id then i2:=ii-1 else
      begin i2:=ii; Break end
    end;

    Inc(i2); if i2 < Result then begin
      p:=@fbuf[i2]; cx:=(Result-i2) * Sizeof(Int64);
      xmove(@p[0],@p[1],cx); p[0]:=v.x
    end

  end
  else begin
    p:=@fbuf[Result-1];
    for i:=1 to Result do begin
      t.x:=p[0];
      if t.c[0] > Id then
      begin p[1]:=p[0]; p[0]:=v.x end;
      Dec(TPointer(p),Sizeof(Int64))
    end
  end
end;

function TIndexList.id_IndexOf(Id: Cardinal): Integer;
begin
  Result:=-1;
  if Assigned(fbuf) then
  if fCount > 0 then begin
           
    if fIsSorted then begin
      Result:=x_Index_Contains(fbuf,fCount,Id);
      if Result < 0 then
      Result:=Index_Contains(fbuf,fCount,Id);
    end else

    Result:=Index_Contains(fbuf,fCount,Id)
  end
end;

function TIndexList.id_Value(Id: Cardinal): Integer;
var
  Ind: Integer; v: TInt64; 
begin
  Result:=0;
  if Assigned(fbuf) then
  if fCount > 0 then begin

    if fIsSorted then begin
      Ind:=x_Index_Contains(fbuf,fCount,Id);

      if Ind < 0 then
      Ind:=Index_Contains(fbuf,fCount,Id);
    end else

    Ind:=Index_Contains(fbuf,fCount,Id);

    if Ind >= 0 then begin
      v.x:=fbuf[Ind]; Result:=v.c[1]
    end
  end
end;

function TIndexList.Get_id(Ind: Integer): Cardinal;
var
  v: TInt64;
begin
  Result:=0;
  if (Ind >= 0) and (Ind < fCount) then begin
    v.x:=fbuf[Ind]; Result:=v.c[0]
  end
end;

function TIndexList.Get_Value(Ind: Integer): Cardinal;
var
  v: TInt64;
begin
  Result:=0;
  if (Ind >= 0) and (Ind < fCount) then begin
    v.x:=fbuf[Ind]; Result:=v.c[1]
  end
end;

procedure TIndexList.Set_Value(Ind: Integer; V: Cardinal);
var
  ax: TInt64;
begin
  if (Ind >= 0) and (Ind < fCount) then begin
    ax.x:=fbuf[Ind]; ax.c[1]:=V; fbuf[Ind]:=ax.x
  end
end;

function TIndexList.Max_id: Cardinal;
var
  I: Integer; v: TInt64;
begin
  Result:=0;

  for I:=0 to fCount-1 do begin
    v.x:=fbuf[I]; Result:=Max(Result,v.c[0])
  end;

  Inc(Result)
end;

function TIndexList.Max_value: Integer;
var
  I: Integer; v: TInt64;
begin
  Result:=-1;

  for I:=0 to fCount-1 do begin
    v.x:=fbuf[I];
    Result:=Max(Result,v.c[1])
  end
end;

function TIndexList.id_Update(Id,Val: Cardinal): Integer;
var
  v: TInt64;
begin
  Result:=id_IndexOf(Id);

  if Result >= 0 then begin
    v.x:=fbuf[Result]; v.c[1]:=Val;
    fbuf[Result]:=v.x
  end
end;

function TIndexList.id_Update1(Id,Val: uint): int;
var
  v: TInt64;
begin
  Result:=id_IndexOf(Id);

  if Result >= 0 then begin
    v.x:=fbuf[Result];
    if v.c[1] < Val then begin
      v.c[1]:=Val;
      fbuf[Result]:=v.x
    end
  end
end;

function TIndexList.id_Delete(Id,Val: Cardinal): Integer;
var
  I: Integer; v: TInt64;
begin
  Result:=0;

  if Val = 0 then begin
    I:=id_IndexOf(Id); if I >= 0 then
    begin Delete(I); Result:=1 end
  end
  else begin
    v.c[0]:=Id; v.c[1]:=Val;

    I:=0; while I < fCount do
    if fbuf[I] = v.x then Delete(I)
    else Inc(I)

  end
end;

function TIndexList.stm_LoadFrom(const stg: IStorage;
                             Name: PChar): Integer;
var
  cnt,len: Integer;
begin
  Clear;

  cnt:=xSizeStream(stg,Name) div Sizeof(Int64);
  len:=cnt * Sizeof(Int64);

  if (cnt > 0) and Realloc(cnt) then
  if xLoadFromStream(stg,Name,fbuf^,len) then
  fCount:=cnt;

  Result:=fCount
end;

procedure TIndexList.stm_SaveAs(const stg: IStorage; Name: PChar);
var
  len: Integer;
begin
  if fIsChanged then

  if fCount = 0 then
    xDeleteElement(stg,Name)
  else begin
    len:=fCount * Sizeof(Int64);
    xSaveAsStream(stg,Name,fbuf,len);
  end;

  fIsChanged:=false
end;

function TIndexList.id_Clear(MaxIndex: Integer;
                             Indexing: Boolean): Boolean;
var
  I: Integer; v1,v2: TInt64;
begin
  Result:=false; I:=0; v2.x:=0;

  while I < fCount do begin
    v1.x:=v2.x; v2.x:=fbuf[I]; Inc(I);

    if v2.c[1] > MaxIndex then begin
      v2:=v1; Dec(I); Delete(I); Result:=true
    end else
    if Indexing and (I > 1) then
    if v1.c[0] = v2.c[0] then begin
      Dec(I); Delete(I); Result:=true
    end
  end
end;

function TIndexList.id_Sort: Boolean;
const
  buf_max = 512;
type
  TBuffer = Array[0..buf_max-1] of Int64;
var
  top,bot,off: Integer;
  cx,bx, ind,tmp,len,blk: Integer;
  big: TInt64List; si,di: Pointer;
  r1,r2: TInt64; small,mov: TBuffer;
begin
  big:=TInt64list.Create;
  try
    off:=1;

    if not fIsSorted then
    if Count < 2000000 then

    while off < Count do begin bot:=0;

      while bot < Count do begin

        ind:=bot; Inc(bot,off);
        top:=bot; Inc(bot,off);

        while (ind < top) and (ind < Count) do begin

          r1.x:=fbuf[ind]; tmp:=top;

          while top < bot do begin
            if top >= Count then Break; r2.x:=fbuf[top];
            if r1.id <= r2.id then Break; Inc(top)
          end;

          if top > tmp then begin

            len:=top-tmp;
            cx:=len*Sizeof(Int64);

            if len <= buf_max then
              Move(fbuf[tmp],small,cx)
            else
              big.LoadBuffer(@fbuf[tmp],len);

            blk:=(tmp-ind)*Sizeof(Int64);

            si:=@fbuf[tmp]; di:=@fbuf[top];

            while blk > 0 do begin {move block}
              bx:=Sizeof(mov);
              if bx > blk then bx:=blk;

              Dec(blk,bx);
              Dec(TPointer(si),bx);
              Dec(TPointer(di),bx);

              Move(si^,mov,bx);
              Move(mov,di^,bx)
            end;

            if len <= buf_max then
              Move(small,fbuf[ind],cx)
            else begin
              si:=big.fbuf;
              if Assigned(si) then
              Move(si^,fbuf[ind],cx)
            end;

            Inc(ind,len)
          end;

          Inc(ind)
        end
      end;

      Inc(off,off)
    end;

    fIsSorted:=true;

  finally
    big.Free
  end;

  Result:=fIsSorted
end;

function TIndexMem.Open(const doc: IStorage;
                        Name: PChar; rw: Boolean): Boolean;
begin
  Result:=false;

  if vm_Active then
    Result:=true
  else

  if Assigned(doc) then

  if rw then begin

    if stm_Open(doc,Name,ole_ReadWrite) then
      Result:=true
    else
      Result:=stm_Create(doc,Name)

  end else

  Result:=stm_Open(doc,Name,ole_Read)
end;

function TIndexMem.DeleteItem(ind,bot: longint): longint;
var
  p: TPoint;
begin
  Result:=bot - Sizeof(p);
  if ind < Result then begin
    vm_Load(Result,p,Sizeof(p));
    vm_Store(ind,p,Sizeof(p));
  end
end;

procedure TIndexMem.fc_Update(fe: PIntegers; Count: Integer;
                              fc: Integer; IsUpdate: Boolean);
var
  p: TPoint; i,cx,ind,bot,len: Integer;
begin
  cx:=vm_Ind div Sizeof(p);
  bot:=cx * Sizeof(p); ind:=0;

  if IsUpdate then
  while ind < bot do begin
    vm_Load(ind,p,Sizeof(p));

    if p.y <> fc then
      Inc(ind,Sizeof(p))
    else
    if p.y = fc then

    if Count = 0 then
      bot:=DeleteItem(ind,bot)
    else begin
      i:=Int_Contains(fe,Count,p.x);

      if i < 0 then begin
        i:=0; p.x:=fe[0];
        vm_Store(ind,p,Sizeof(p))
      end;

      Count:=Int_Delete(fe,Count,i);
      Inc(ind,Sizeof(p))
    end
  end;

  ind:=bot + Count*Sizeof(p);
  len:=ind - vm_Ind;

  if len > 0 then vm_Expand(len);

  if ind <= vm_Ind then begin

    for i:=0 to Count-1 do begin
      p.x:=fe[i]; p.y:=fc;
      bot:=vm_Store(bot,p,Sizeof(p))
    end;

    vm_Ind:=ind
  end
end;

procedure TIndexMem.fe_Exclude(fe: Integer);
var
  p: TPoint; cx,ind,bot: Integer;
begin
  cx:=vm_Ind div Sizeof(p);
  bot:=cx * Sizeof(p); ind:=0;

  while ind < bot do begin
    vm_Load(ind,p,Sizeof(p));

    if p.x = fe then
      bot:=DeleteItem(ind,bot)
    else
      Inc(ind,Sizeof(p))
  end;

  vm_Ind:=bot
end;

function TIndexMem.Get_fc(fc: PIntegers; Max: Integer;
                          fe: Integer): Integer;
var
  p: TPoint; cx,ind,bot: Integer;
begin
  Result:=0;

  cx:=vm_Ind div Sizeof(p);
  bot:=cx * Sizeof(p); ind:=0;

  while ind < bot do begin
    ind:=vm_Load(ind,p,Sizeof(p));

    if p.x = fe then
    if Result < Max then begin
      fc[Result]:=p.y; Inc(Result)
    end
  end
end;

procedure TIndexMem.attv_update(id,attv: Integer; IsUpdate: Boolean);
var
  p: TPoint; cx,ind,bot,len: Integer;
begin
  cx:=vm_Ind div Sizeof(p);
  bot:=cx * Sizeof(p); ind:=0;

  if IsUpdate then
  while ind < bot do begin
    vm_Load(ind,p,Sizeof(p));

    if p.x = id then begin

      if attv = -1 then
        bot:=DeleteItem(ind,bot)
      else begin
        p.y:=attv;
        vm_Store(ind,p,Sizeof(p))
      end;

      Break
    end;

    Inc(ind,Sizeof(p))
  end;

  if attv = -1 then
    vm_Ind:=bot
  else begin
    ind:=bot + Sizeof(p);
    len:=ind - vm_Ind;

    if len > 0 then vm_Expand(len);

    if ind <= vm_Ind then begin
      p.x:=id; p.y:=attv;
      vm_Store(bot,p,Sizeof(p));
      vm_Ind:=ind
    end
  end
end;

function TIndexMem.Get_attv(id: Integer): Integer;
var
  p: TPoint; cx,ind,bot: Integer;
begin
  Result:=-1;

  cx:=vm_Ind div Sizeof(p);
  bot:=cx * Sizeof(p); ind:=0;

  while ind < bot do begin
    ind:=vm_Load(ind,p,Sizeof(p));

    if p.x = id then begin
      Result:=p.y; Break
    end
  end;
end;

constructor TPolyMesh.Create;
begin
  inherited Create;
  fPoints:=VPointList.Create(4096*16);
  fFaces:=TIntegerList.Create
end;

destructor TPolyMesh.Destroy;
begin
  fFaces.Free;
  fPoints.Free;
  inherited;
end;

procedure TPolyMesh.Clear;
begin
  fPoints.Clear;
  fFaces.Clear
end;

procedure TPolyMesh.AddFace(fp: PIntegers; fn: Integer);
var
  i,i1,k,ind: Integer;
begin
  k:=0;
  for i:=0 to fn-1 do begin
    ind:=fp[i]; if ind = 0 then Break;
    Inc(k)
  end;

  i1:=k-1; if k >= 2 then
  for i:=0 to fn-1 do begin
    ind:=fp[i]; if ind = 0 then Break;

    if ind > 0 then Dec(ind) else
    ind:=(Abs(ind)-1) or mesh_inv;
    if i = i1 then Inc(ind,mesh_brk);

    fFaces.AddItem(ind)
  end
end;

function TPolyMesh.Verify_faces: Integer;
var
  i,bx,np: Integer; fp: PIntegers;
begin
  np:=fPoints.Count;

  i:=0; fp:=fFaces.First;
  while i < fFaces.Count do begin

    bx:=fp[i] and $FFFFFF;
    if (bx >= 0) and (bx < np) then
      Inc(i)
    else
      fFaces.Delete(i)
  end;

  if fFaces.Count < 3 then
  fFaces.Clear;

  Result:=fFaces.Count
end;

end.

