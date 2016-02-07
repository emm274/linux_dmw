unit xlist; interface

uses
  Windows,Sysutils,Classes,Math,
  activeX,otypes,
  xyz;

type
  TAllocBuffer = record
    Ptr: Pointer; Capacity,Size: Integer
  end;

  ICustomList = interface(IUnknown)
    ['{2FA100FE-6B39-49E9-867A-5E294931A3EE}']
    function GetCount: Integer; stdcall;
    function GetItemLen: Integer; stdcall;
    function GetItem(I: Integer; var rec): Integer; stdcall;
  end;

  TCustomList = class(TInterfacedObject,ICustomList)

    constructor Create(AItemLen,ACapacity: Integer);
    destructor Destroy; override;

    function GetCount: Integer; stdcall;
    function GetItemLen: Integer; stdcall;
    function GetItem(I: Integer; var rec): Integer; stdcall;

    procedure Draw(Sender: TObject); virtual;

    procedure Refresh;

    function CheckSum: Integer;

    function ActiveItem: Pointer;

    function ItemPtr(Ind: Integer): Pointer;
    function PtrToIndex(Ptr: Pointer): Integer;

    procedure AssignBuf(ABuffer: Pointer; ACount: Integer);

    function Get_ctrl(out Ctrl: TAllocBuffer): Integer;
    function Set_ctrl(const Ctrl: TAllocBuffer): Integer;

    function Release(out Ptr: Pointer): Integer;

    function Realloc(NewCount: Integer): Boolean;
    function Resize(NewCount: Integer): Boolean;
    function Extend(NewCount: Integer): Boolean;

    function Add(Item: Pointer): Integer;
    function Delete(Ind: Integer): Integer; virtual;

    function IsItem(Ind: Integer): Boolean;

    procedure Remove(Item: Pointer);

    procedure Truncate(NewCount: Integer); virtual;
    procedure Delete_last;

    function Indexof(Item: Pointer;
                     FromInd: Integer): Integer;

    function id_Indexof(Id: Integer): Integer;
    function id_Itemof(Id: Integer): Pointer;

    function id_Itemof_from(Top: Pointer; Id: int): Pointer;

    function id_Delete(Id: Integer): Integer;

    function Min_id: Integer;
    function Max_id: Integer;

    function Insert_range(AItems: Pointer;
                          Ind,ACount: Integer): Integer;

    function Delete_range(Ind,ACount: Integer): Integer;

    function Update_range(AItems: Pointer;
                          Ind,ACount: Integer): Integer;

    function Copy_range(ADest: TCustomList;
                        Ind,ACount: Integer): Integer;

    function Exchange(Ind1,Ind2: Integer): Boolean;
    function MoveTo(Ind1,Ind2: Integer): Boolean; virtual;

    function MoveTop(Ind: int): Boolean;

    procedure SwapRange(I1,I2: Integer);

    procedure Exclude(Ind,ACount: Integer); virtual;
    procedure Clear; virtual;

    procedure Update(Ind: Integer; Item: Pointer);

    procedure Change; virtual;

    function AddList(List: TCustomList): Integer;

    function LoadList(List: TCustomList): Integer; virtual;

    function LoadBuffer(Items: Pointer;
                        ACount: Integer): Integer;

    function Load_ctrl(const Ctrl: TAllocBuffer): Integer;
    function Backup_ctrl(var Ctrl: TAllocBuffer): Integer;

    function LoadFrom(Path: PChar): Integer; virtual;
    procedure SaveAs(Path: PChar); virtual;

    function doc_LoadFrom(const stg: IStorage; AName: PChar): Integer; virtual;
    function doc_SaveAs(const stg: IStorage; AName: PChar): Boolean; virtual;

    function Sort_up(percent: Float): Integer; virtual;

    function SwapList: Boolean; virtual;

    function sAdd(Item: Pointer): int;
    function sIndexof(Id: int): Pointer;

    function s64_Add(Item: Pointer): int;
    function s64_Indexof(Id: int64): Pointer;

  protected
    fItemIndex: Integer;

    function Up_item(p1,p2: Pointer): Boolean; virtual; abstract;
    procedure Swap_items(Ind1,Ind2: Integer); virtual;
    procedure Set_ItemIndex(Ind: Integer); virtual;
    function Get_Marker: Integer; virtual;

  private
    fItemLen: Integer;
    fCapacity: Integer;
    fIncrement: Integer;
    fCount: Integer;
    fBuffer: PBytes;

    fSortIndex: Integer;
    
    fIsChanged: Boolean;
    fEnabled: Boolean;

    fOnChange: TNotifyEvent;
    fOnRefresh: TNotifyEvent;

    procedure SetItemLen(Len: Integer);

    function Get_crc: DWord;

    function Get_BufferSize: Integer;

    function Get_Item(I: Integer): Pointer;
    procedure Set_Item(I: Integer; It: Pointer);

    function Get_First: Pointer;
    function Get_Last: Pointer;

  public
    property crc: DWord read Get_crc;

    property Enabled: Boolean read fEnabled
                              write fEnabled;

    property IsChanged: Boolean read fIsChanged
                                write fIsChanged;

    property Buffer: PBytes read fBuffer;
    property BufferSize: Integer read Get_BufferSize;

    property Capacity: Integer read fCapacity write fCapacity;
    property Increment: Integer read fIncrement write fIncrement;

    property ItemLen: Integer read fItemLen write SetItemLen;
    property Count: Integer read fCount;

    property ItemIndex: Integer read fItemIndex write Set_ItemIndex;
    property ActiveIndex: Integer write fItemIndex;

    property Items[I: Integer]: Pointer read Get_Item
                                        write Set_Item; default;

    property First: Pointer read Get_First;
    property Last: Pointer read Get_Last;

    property SortIndex: Integer read fSortIndex
                                write fSortIndex;

    property OnChange: TNotifyEvent write fOnChange;
    property OnRefresh: TNotifyEvent write fOnRefresh;
  end;

  TBoolList = class(TCustomList)
    constructor Create;
    function AddValue(V: Boolean): Integer;
  private
    function Get_Value(I: Integer): Boolean;
    procedure Set_Value(I: Integer; V: Boolean);
  public
    property Values[I: Integer]: Boolean read Get_Value
                                         write Set_Value;
  end;

  TWordList = class(TCustomList)
    constructor Create;
  end;

  TPointerList = class(TCustomList)
    constructor Create;

    procedure Free_items;

    function AddItem(Item: Pointer): Integer;
    function IndexOf(Item: Pointer): Integer;

    function Load_sort_list(List: TCustomList): Integer;

    function Sort_up(percent: Float): Integer; override;

  protected
    function Compare(p1,p2: Pointer): Integer; virtual; abstract;

  private
    function Get_Value(I: Integer): Pointer;
    procedure Set_Value(I: Integer; V: Pointer);
  public
    property Values[I: Integer]: Pointer read Get_Value
                                         write Set_Value; default;
  end;

  TIntfList = class(TPointerList)
    procedure Exclude(Ind,ACount: Integer); override;
  end;

  TIntegerList = class(TCustomList)
    constructor Create;

    function GetRange(out v1,v2: int): int;

    function Assign(List: TIntegerList): Integer;

    function Indexing(ACount: Integer): Integer;

    function AddItem(Item: Integer): Integer;

    function IndexOf(Item: Integer): Integer;
    function ValToIndex(V: int): int;

    function DeleteItem(Item: Integer): Integer;

    function ContainsList(List: TIntegerList): Boolean;

    function Insert(Ind,Val: Integer): Integer;

    procedure Enumerate(MinValue: Integer);

    function SwapItems(i1,i2: int): bool;

    function SwapList: Boolean; override;

    function GetStrings(List: TStrings): Integer;

  protected
    function Up_item(p1,p2: Pointer): Boolean; override;

  private
    fDuplicates: Longbool;
    fSorted: Longbool;

    function Get_Edge(I: Integer): Integer;

    function Get_Value(I: Integer): Integer;
    procedure Set_Value(I,Val: Integer);

    function Get_Max_Value: Integer;
    function Get_Max_Id: Cardinal;

  public
    property Duplicates: Longbool read fDuplicates
                                  write fDuplicates;

    property Sorted: Longbool read fSorted
                              write fSorted;

    property Max_Id: Cardinal read Get_Max_Id;

    property Edges[I: Integer]: Integer read Get_Edge;

    property Values[I: Integer]: Integer read Get_Value
                                         write Set_Value; default;
  end;

  TCustomGrid = class(TCustomList)

    procedure Clear; override;

    function Clear_values: Integer;

    function LoadGrid(Grid: TCustomGrid): Integer;

    function Get_row(Row: Integer): Pointer;
    function Get_cell(Row,Col: Integer): Pointer;

  private
    fCols,fRows: Integer;

    procedure Set_cols(ACols: Integer);
    procedure Set_rows(ARows: Integer);

  public
    property Rows: Integer read fRows write Set_rows;
    property Cols: Integer read fCols write Set_cols;
    property Cells[Row,Col: Integer]: Pointer read Get_cell;
  end;

  TByteGrid = class(TCustomGrid)

    constructor Create;

  private
    function Get_Value(Row,Col: Integer): Integer;
    procedure Set_Value(Row,Col,Val: Integer);
  public
    property Values[Row,Col: Integer]: Integer read Get_Value
                                               write Set_Value; default;
  end;

  TIntegerGrid = class(TCustomGrid)

    constructor Create;

    function Indexof(Val: Integer): TPoint;

    procedure ClearCol(Col: Integer);

  private
    function Get_Value(Row,Col: Integer): Integer;
    procedure Set_Value(Row,Col,Val: Integer);
  public
    property Values[Row,Col: Integer]: Integer read Get_Value
                                               write Set_Value; default;
  end;

  TPointerGrid = class(TCustomGrid)

    constructor Create;

    function val_row_Count(row: Integer): Integer;
    function val_col_Count(col: Integer): Integer;

    procedure delete_row(row: Integer);
    procedure delete_col(col: Integer);

    procedure pack_col(col: Integer);

  private
    function Get_Value(Row,Col: Integer): Pointer;
    procedure Set_Value(Row,Col: Integer; Val: Pointer);
  public
    property Values[Row,Col: Integer]: Pointer read Get_Value
                                               write Set_Value;
  end;

  TPointGrid = class(TCustomGrid)
    constructor Create;
    procedure Fill(vX,vY: int);
  end;

  TGaussGrid = class(TCustomGrid)
    constructor Create;
  private
    function Get_Value(Row,Col: Integer): TGauss;
    procedure Set_Value(Row,Col: Integer; const Val: TGauss);
  public
    property Values[Row,Col: Integer]: TGauss read Get_Value
                                              write Set_Value; default;
  end;

  TSingleGrid = class(TCustomGrid)

    constructor Create;

  private
    function Get_Value(Row,Col: Integer): Single;
    procedure Set_Value(Row,Col: Integer; Val: Single);
  public
    property Values[Row,Col: Integer]: Single read Get_Value
                                              write Set_Value; default;
  end;

  TSingleList = class(TCustomList)
    constructor Create;
    function AddItem(Val: Single): Integer;
    procedure Fill(Val: Single);
    procedure Inc_val(Ind: Integer; dVal: Single);
    function GetSum: Double;
    function GetAve: Double;
    function GetMax: Single;

    function Get_min_max(out vmin,vmax: Single): Integer;

    procedure stat(AList: TSingleList);

    function SwapList: Boolean; override;

    function GetStrings(List: TStrings; m: Integer): Integer;

  private
    function GetValue(Ind: Integer): Single;
    procedure SetValue(Ind: Integer; V: Single);
  public
    property Values[Ind: Integer]: Single read GetValue
                                          write SetValue;
  end;

  TFloatList = TSingleList;
  
  TDoubleList = class(TCustomList)
    constructor Create;
    function AddItem(Val: Double): Integer;

    function Insert(Ind: Integer; Val: Double): Integer;

    function Get_Range(Ind: Integer): TRange;

  private
    function Get_Value(I: Integer): Double;
    procedure Set_Value(I: Integer; Val: Double);

  protected
    function Up_item(p1,p2: Pointer): Boolean; override;
  public
    property Values[I: Integer]: Double read Get_Value
                                        write Set_Value;
  end;

  TPointList = class(TCustomList)
    constructor Create(ACapacity: Integer);

    function IsLock: Boolean;
    function Lock: Boolean;

    function GetLength: Double;

    function Get_bound(out lt,rb: TPoint): int;
    function Get_line(lp: PLLine; Max: int): int;

    function AddItem(X,Y: Integer): Integer;
    function Add_idt(X,Y: Integer): Integer;

    function ExcludeItem(X,Y: Integer): Integer;

    function IndexOf(X,Y: Integer): Integer;
    function rIndexOf(X,Y,R: Integer): Integer;

    function id_IndexOf(Id: Integer): Integer;
    function id_xIndexOf(Id,Top: Integer): Integer;

    function id_Insert(id,val: Integer): Integer;

    function Push(p: PPoint): Integer;
    function Insert_point(p: PPoint; Ind: Integer): Integer;

    function sAdd(X,Y: int): bool;
    function sIndexOf(X,Y: Integer): Integer;

    function Load_line(lp: PLLine): Integer;
    function Load_poly(lp: PLPoly; N: Integer): Integer;

    function Add_poly(lp: PLPoly; N: Integer): Integer;

    function AddNext(X,Y: Integer): Integer;
    function Continue(const p: TPoint): Integer;
    function Continue1(const p: TPoint): Integer;

    function xAppend(lp: PLPoly; N: int): int;

    function LoadFromText(Path: PChar): Integer;
    procedure SaveAsText(Path: PChar);

    function off_duplicates: Integer;
    function Filter(R: double): Integer;

    function SwapList: Boolean; override;

    procedure beginUpdate;
    function endUpdate: int;

    procedure AddPoint(x,y: int);

  protected
    flast: PPoint;
    fsp: int;

    function Up_item(p1,p2: Pointer): Boolean; override;

  private
    function Get_Point(Ind: Integer): TPoint;
    procedure Set_Point(Ind: Integer; const V: TPoint);

    function Get_Edge(Ind: Integer): TPoint;
    procedure Set_Edge(Ind: Integer; const V: TPoint);

  public
    property Points[Ind: Integer]: TPoint read Get_Point
                                          write Set_Point; default;

    property Edges[Ind: Integer]: TPoint read Get_Edge
                                         write Set_Edge;
  end;

  VPointList = class(TCustomList)
    constructor Create(ACapacity: Integer);
    function AddItem(_X,_Y,_Z: Integer): Integer;
    function IndexOf(_X,_Y,_Z: Integer): Integer;
    function xy_IndexOf(_X,_Y: Integer): Integer;
    function y_IndexOf(_Y: Integer): Integer;
    function z_IndexOf(_Z: Integer): Integer;

    function xy_ItemOf(_X,_Y: Integer): Pointer;

    function id_Insert(id,v1,v2: Integer): Integer;

    function Get_min_max(out lt,rb: VPoint): Integer;

    function IsPack: Boolean;

    function Pack: Integer;
    procedure Unpack(bp: PBytes);

    function Load_poly(lp: PLPoly;
                       hp: PIntegers;
                       n: int): int;

    function Get_poly(lp: PLPoly;
                      hp: PIntegers;
                      lpMax: int): int;

    function ins_v(i: int; rmu: double): int;

  protected
    function Up_item(p1,p2: Pointer): Boolean; override;

  private
    function Get_Point(Ind: Integer): VPoint;
    procedure Set_Point(Ind: Integer; const V: VPoint);

  public
    property Points[Ind: Integer]: VPoint read Get_Point write Set_Point;
  end;

  PRectArray = ^TRectArray;
  TRectArray = array[0..255] of TRect;

  TRectList = class(TCustomList)
    constructor Create(ACapacity: Integer);
    function xAdd(ALeft,ATop,ARight,ABottom: Integer): Integer;
    function xIndexof(X,Y: Integer): Integer;
  private
    function Get_lt(I: Integer): TPoint;
    function Get_rb(I: Integer): TPoint;
    function Get_Rect(I: Integer): TRect;
    function Get_Centre(I: Integer): TPoint;

  protected
    function Up_item(p1,p2: Pointer): Boolean; override;

  public
    property lt[I: Integer]: TPoint read Get_lt;
    property rb[I: Integer]: TPoint read Get_rb;
    property Rects[I: Integer]: TRect read Get_Rect;
    property Centres[I: Integer]: TPoint read Get_Centre;
  end;

  TGaussList = class(TCustomList)
    constructor Create(ACapacity: Integer);
    function AddItem(X,Y: Double): Integer;

    function AddArray(XY: PDoubles; ACount,AStride: Integer): Integer;

    function LoadFromText(Path: PChar): Integer;

    function Load_LPoly(lp: PLPoly; N: Integer): Integer;

    procedure Get_Min_Max(out lt,rb: TGauss);

    function Get_Centre: TGauss;
    function Get_Square: Double;
    function Get_max_dim: Double;

    function Locate(w: PGPoly;
                    out dist: Double;
                    out c: TGauss): Integer;

    function Backup(pos: Double;
                    out dpos: Double;
                    out c,v: TGauss): Integer;

    function Normalize(centre,aspect: Boolean): Integer;

    function eps_Indexof(X,Y,Eps: Double): Integer;

    function eps_sync(var p: TGauss; Eps: Double): int;

    function SwapList: Boolean; override;

  protected
    function Up_item(p1,p2: Pointer): Boolean; override;
  private
    fsys: tsys;

    function Get_Point(Ind: Integer): TGauss;
    procedure Set_Point(Ind: Integer; const P: TGauss);

    function Get_First_Point: TGauss;
    function Get_Last_Point: TGauss;

    function GetLength: Double;

  public
    property Points[Ind: Integer]: TGauss read Get_Point
                                          write Set_Point;

    property First_Point: TGauss read Get_First_Point;
    property Last_Point: TGauss read Get_Last_Point;

    property Length: Double read GetLength;

    property sys: tsys read fsys write fsys;
  end;

  TXyzList = class(TCustomList)
    constructor Create(ACapacity: Integer);

    function xAdd(const v: txyz): Integer;

    function AddItem(X,Y,Z: Double): Integer;
    function xIndexof(X,Y,Z,Eps: Double): Integer;

    procedure Get_min_max(out lt,rb: txyz);

    procedure Transit(const tr: TMatrix);

    function IsLock(eps: Double): Boolean;
    function Lock(eps: Double): Boolean;

    function SwapList: Boolean; override;

    function Sum_values: txyz;

    procedure up_z(dz: float);

  private
    fcube: tcube;
    fsys: tsys;

    function Get_Point(I: Integer): txyz;
    procedure Set_Point(I: Integer; const v: txyz);

    function Get_Length: Double;

  public
    property Points[I: Integer]: txyz read Get_Point
                                      write Set_Point;

    property cube: tcube read fcube;
    property Length: Double read Get_Length;

    property sys: tsys write fsys;
  end;

  PKeyRecord = ^TKeyRecord;
  TKeyRecord = record Id,Val: Int64 end;

  PKeyArray = ^TKeyArray;
  TKeyArray = array[0..1023] of TKeyRecord;

  TKeyList = class(TCustomList)
    constructor Create;
    function AddItem(Id,Val: Int64): Integer;
    function Id_Indexof(Id: Int64): Integer;
    function Val_Indexof(Val: Int64): Integer;

  protected
    function Up_item(p1,p2: Pointer): Boolean; override;

  private
    fminId: int64;
    fmaxId: int64;
    fSorted: Longbool;

    function Get_Name(Val: Int64): Int64;
    function Get_Value(Id: Int64): Int64;

    function Get_Left(I: Integer): Int64;
    function Get_Right(I: Integer): Int64;
  public
    property minId: int64 read fminId;
    property maxId: int64 read fmaxId;

    property Sorted: Longbool read fSorted
                              write fSorted;
  
    property Names[Val: Int64]: Int64 read Get_Name;
    property Values[Id: Int64]: Int64 read Get_Value;

    property Left[I: Integer]: Int64 read Get_Left;
    property Right[I: Integer]: Int64 read Get_Right;
  end;

  TCharArray = class(TCustomList)
    constructor Create;

    function AddLine(S: PChar): Integer;
    function AddString(const S: String): Integer;
    function AddReal(V: Double; M: Integer): Integer;
    function AddInteger(V: Integer): Integer;

    function AddStrings(Lines: TStrings): Integer;

  private
    function Get_Strings: PChar;
  public
    property Strings: PChar read Get_Strings;
  end;

  TIdList = class(TCustomList)
    function id_IndexOf(Id: Integer): Integer;
    function id_Ptr(Id: Integer): Pointer;

    function Id_Delete(Id: Integer): Boolean;
  end;

  TIndexControl = class(TIntegerList)

    destructor Destroy; override;

    function Control(Id: Cardinal): Boolean;

  private
    fBits: PIntegers;
    fmax_NNNN: Cardinal;

    procedure Set_max_NNNN(Value: Cardinal);

  public
    property Bits: PIntegers read fBits;
    property max_NNNN: Cardinal write Set_max_NNNN;
  end;

  TChannelList = class(TCustomList)
    constructor Create(ACapacity: Integer);
  end;

  THChannelList = class(TCustomList)
    constructor Create(ACapacity: Integer);
  end;

  TDataStream = class
    constructor Create(AIncrement: Integer);
    destructor Destroy; override;

    function Resize(NewSize: Integer): Boolean;
    function Expand(len: Integer): Boolean;

    function Pop(len: Integer): Integer;

    function LoadFromBuf(Data: Pointer; DataLen: int): bool;

    function doc_LoadFrom(const stg: IStorage;
                          AName: PChar): Integer;

    function LoadStream(stm: TStream): Integer;
    function LoadData(stm: TDataStream): Integer;

    function LoadFile(APath: PChar): Integer;

    function LoadFromFile(APath: PChar): Integer;
    function SaveToFile(APath: PChar): Boolean;
    function SaveToBin(APath: PChar): Boolean;

    function SaveAs(APath: PChar): Boolean;

    function Append(var rec; len: Integer): Integer;
    function Appendi1(v: Integer): Integer;
    function Appendi2(v: Integer): Integer;
    function Appendi4(v: Integer): Integer;
    function Appendi8(v: Int64): Integer;
    function Appendf(v: float): Integer;

    function AppendStream(stm: TStream): Integer;

    function Load(pos: Integer; var rec; len: Integer): Integer;
    function Store(pos: Integer; var rec; len: Integer): Integer;

    function Seek(Offset,Origin: Integer): Boolean;
    function Get(var rec; len: Integer): Boolean;
    function Get_long(out v: Integer): Boolean;

    function Get_data(offs,len: int): Pointer;

    function Get_Pointer(Offset: Integer): Pointer;

    function Get_Ptr(len: Integer): Pointer;

    function Append_poly(lp: PLPoly; n: Integer): Integer;
    function Get_poly(lp: PLPoly; lp_Max: Integer): Integer;
    function Skip_poly: Integer;

    function Cut(pos,len: Integer): Integer;

    procedure Changed;

  private
    fBuffer: PBytes;
    fCapacity: Integer;
    fIncrement: Integer;
    fPosition: Integer;
    fSize: Integer;

    fIsChanged: Longbool;

    fOnChanged: TNotifyEvent;

  public
    property Buffer: PBytes read fBuffer;
    property Position: Integer read fPosition;
    property Size: Integer read fSize write fSize;

    property Increment: Integer read fIncrement
                                write fIncrement;

    property IsChanged: Longbool read fIsChanged
                                 write fIsChanged;

    property OnChanged: TNotifyEvent write fOnChanged;
  end;

  TDataStreamId = class(TDataStream)

  private
    fId: int64;
  public
    property Id: int64 read fId write fId;
  end;

  TUndoStream = class(TDataStream)
    procedure Push(Buf: PBytes; BufLen: Integer);
    function Pop(Buf: PBytes; BufSize: Integer): Integer;
  end;

function CheckSumXor(bp: PBytes; len: Integer): Integer;

implementation

uses
  convert,ofiles,storage,
  xline,xpoly,vpoly,xy,
  xcrc;

function CheckSumXor(bp: PBytes; len: Integer): Integer;
var
  i,al: Integer;
begin
  al:=0;
  for i:=1 to len do begin
    al:=al xor bp[0]; bp:=@bp[1]
  end; Result:=al
end;

{$ASMMODE INTEL}

function id_Item_of(Items: Pointer; Count: Integer;
                    ItemLen,Id: Integer): Pointer;
asm
{$ifdef CPUX86_64}  // Items,Count,ItemLen,Id - RDi,RSi,RDx,RCx
   xchg RCx,RSi     // Count,Id

   cmp  RCx,0
   je   @not_found

   cld
@loop:
   cmp  DWORD PTR [RDi],RSi
   je   @exit
   add  RDi,RDx
   loop @loop

@not_found:
   mov  RDi,0
@exit:
   mov  RAx,RDi
{$else}
  push ESi

  mov  ESi,EAx
  xchg ECx,EDx

  mov  EAx,[Id]

  cmp  ECx,0
  je   @not_found

  cld
@loop:
  cmp  DWORD PTR [ESi],EAx
  je   @exit
  add  ESi,EDx
  loop @loop

@not_found:
  mov  ESi,0
@exit:
  mov  EAx,ESi

  pop  ESi
{$endif}
end;

constructor TCustomList.Create(AItemLen,ACapacity: Integer);
begin
  inherited Create;
  fItemLen:=AItemLen;
  fCapacity:=ACapacity;
  if ACapacity = 0 then
  fCapacity:=64;
  fIncrement:=fCapacity;
  fItemIndex:=-1;
  fEnabled:=true
end;

destructor TCustomList.Destroy;
begin
  Exclude(0,fCount);
  xFreePtr(fBuffer);
  inherited
end;

function TCustomList.GetCount: Integer;
begin
  Result:=Count
end;

function TCustomList.GetItemLen: Integer;
begin
  Result:=ItemLen
end;

procedure TCustomList.Draw(Sender: TObject);
begin
end;

function TCustomList.GetItem(I: Integer; var rec): Integer;
var
  p: Pointer;
begin
  Result:=-1; p:=Items[I];
  if Assigned(p) then begin
    Move(p^,rec,ItemLen);
    Result:=I
  end
end;

function TCustomList.CheckSum: Integer;
begin
  Result:=CheckSumXor(fBuffer,BufferSize)
end;

procedure TCustomList.AssignBuf(ABuffer: Pointer; ACount: Integer);
begin
  Exclude(0,fCount);
  xFreePtr(fBuffer); fBuffer:=ABuffer;
  fCount:=ACount; fCapacity:=ACount
end;

procedure TCustomList.Refresh;
begin
  if Assigned(fOnRefresh) then
  fOnRefresh(Self)
end;

function TCustomList.Get_Marker: Integer;
begin
  Result:=0;
end;

procedure TCustomList.SetItemLen(Len: Integer);
begin
  if Len > 0 then begin
    Clear; fItemLen:=Len
  end
end;

function TCustomList.Get_crc: DWord;
begin
  Result:=0; if Count > 0 then
  Result:=crc_check(fBuffer,BufferSize)
end;

function TCustomList.Get_BufferSize: Integer;
begin
  Result:=Count * fItemLen
end;

function TCustomList.Get_ctrl(out Ctrl: TAllocBuffer): Integer;
begin
  Ctrl.Ptr:=fBuffer;
  Ctrl.Capacity:=fCapacity;
  Ctrl.Size:=BufferSize;
  Result:=Ctrl.Size
end;

function TCustomList.Set_ctrl(const Ctrl: TAllocBuffer): Integer;
begin
  fBuffer:=Ctrl.Ptr;
  fCapacity:=Ctrl.Capacity;
  fCount:=Ctrl.Size div fItemLen;
  Result:=fCount
end;

function TCustomList.Release(out Ptr: Pointer): Integer;
begin
  Ptr:=First; Result:=Count;
  if Assigned(Ptr) then fBuffer:=nil
end;

function TCustomList.Realloc(NewCount: Integer): Boolean;
var
  cnt,len: Integer;
begin
  cnt:=int_Round(NewCount,Max(4,fIncrement));

  if fBuffer = nil then begin
    fCapacity:=cnt; len:=cnt * fItemLen;
    fBuffer:=xAllocPtr(len)
  end else
  if NewCount > fCapacity then begin
    fCapacity:=cnt; len:=cnt * fItemLen;
    fBuffer:=xReAllocPtr(fBuffer,len)
  end;

  Result:=Assigned(fBuffer);
  if not Result then fCount:=0
end;

function TCustomList.Resize(NewCount: Integer): Boolean;
begin
  Result:=false;
  if Realloc(NewCount) then begin
    Fillchar(fBuffer^,fItemLen * NewCount,0);
    fCount:=NewCount; ItemIndex:=ItemIndex;
    Result:=true
  end
end;

function TCustomList.Extend(NewCount: Integer): Boolean;
begin
  Result:=false;
  if Realloc(NewCount) then begin
    fCount:=NewCount; ItemIndex:=ItemIndex;
    Result:=true
  end
end;

procedure TCustomList.Truncate(NewCount: Integer);
begin
  if NewCount < fCount then begin
    if NewCount < 0 then NewCount:=0;
    Exclude(NewCount,fCount-NewCount);
    fCount:=NewCount
  end;

  ItemIndex:=ItemIndex
end;

procedure TCustomList.Delete_last;
begin
  if fCount > 0 then begin
    Exclude(fCount-1,1); Dec(fCount);
  end; ItemIndex:=ItemIndex
end;

function TCustomList.ActiveItem: Pointer;
begin
  Result:=Items[fItemIndex]
end;

function TCustomList.ItemPtr(Ind: Integer): Pointer;
begin
  Result:=@fBuffer[Ind * fItemLen]
end;

function TCustomList.PtrToIndex(Ptr: Pointer): Integer;
var
  Ind: Integer;
begin
  Result:=-1;
  if Assigned(Ptr) and (Count > 0) then
  if TPointer(Ptr) >= TPointer(fBuffer) then begin
    Ind:=(TPointer(Ptr) - TPointer(fBuffer)) div fItemLen;
    if (Ind >= 0) and (Ind < fCount) then Result:=Ind
  end
end;

function TCustomList.Get_Item(I: Integer): Pointer;
begin
  Result:=nil;
  if (I >= 0) and (I < fCount) then
  Result:=@fBuffer[I * fItemLen]
end;

procedure TCustomList.Set_Item(I: Integer; It: Pointer);
begin
  if (I >= 0) and (I < fCount) then begin
    Move(It^,fBuffer[I * fItemLen],fItemLen);
    fIsChanged:=true
  end
end;

function TCustomList.Get_First: Pointer;
begin
  Result:=Items[0]
end;

function TCustomList.Get_Last: Pointer;
begin
  Result:=Items[fCount-1]
end;

procedure TCustomList.Set_ItemIndex(Ind: Integer);
begin
  if Ind = Int_NAN then
    fItemIndex:=-1
  else begin
    if Ind >= fCount-1 then Ind:=fCount-1;
    if Ind < 0 then Ind:=0; fItemIndex:=Ind
  end
end;

function TCustomList.Add(Item: Pointer): Integer;
var
  p: Pointer;
begin
  Result:=-1;

  if Realloc(fCount+1) then begin
    Result:=fCount; Inc(fCount);
    p:=@fBuffer[Result * fItemLen];

    if Assigned(Item) then
      Move(Item^,p^,fItemLen)
    else
      Fillchar(p^,fItemLen,0);

    fIsChanged:=true;
  end
end;

function TCustomList.Delete(Ind: Integer): Integer;
var
  Len: Integer;
begin
  Result:=-1;

  if Ind >= 0 then
  if Ind < fCount then begin

    Exclude(Ind,1); Result:=Ind; Inc(Ind);

    if Ind < fCount then begin
      Len:=(fCount-Ind) * fItemLen;
      Move(Items[Ind]^,Items[Result]^,Len)
    end;

    Dec(fCount); fIsChanged:=true;

    if ItemIndex <= Ind then
      ItemIndex:=ItemIndex
    else
      ItemIndex:=ItemIndex-1
  end
end;

function TCustomList.IsItem(Ind: Integer): Boolean;
begin
  Result:=(Ind >= 0) and (Ind < fCount)
end;

procedure TCustomList.Remove(Item: Pointer);
var
  i,cx: Integer; Next: Pointer;
begin
  if fCount > 0 then
  if Assigned(Item) then begin

    i:=PtrToIndex(Item);

    if (i >= 0) and (i < fCount) then begin

      Exclude(i,1); Dec(fCount);
      fIsChanged:=true;

      cx:=(fCount - i) * ItemLen;
      if cx > 0 then begin
        Next:=@fBuffer[(i+1)*ItemLen];
        Move(Next^,Item^,cx)
      end
    end
  end
end;

function TCustomList.Indexof(Item: Pointer;
                             FromInd: Integer): Integer;
var
  ind: Integer; p: PBytes;
begin
  Result:=-1;

  ind:=Max(0,FromInd);
  if ind < fCount then begin
    p:=@fBuffer[ind*fItemLen];

    while ind < fCount do begin
      if CompareMem(p,Item,fItemLen) then
      begin Result:=ind; Break end;
      Inc(ind); p:=@p[fItemLen]
    end
  end
end;

function TCustomList.id_Indexof(Id: Integer): Integer;
var
  I: Integer; P: PBytes;
begin
  Result:=-1; P:=fBuffer;

  for I:=0 to Count-1 do begin

    if PInteger(P)^ = Id then begin
      Result:=I; Break
    end;

    P:=@P[fItemLen]
  end
end;

function TCustomList.id_Itemof(Id: Integer): Pointer;
begin
  Result:=id_Item_of(fBuffer,Count,ItemLen,Id)
end;

function TCustomList.id_Itemof_from(Top: Pointer; Id: int): Pointer;
var
  i: int;
begin
  i:=PtrToIndex(Top);
  Result:=id_Item_of(Top,Count-i,ItemLen,Id)
end;

function TCustomList.id_Delete(Id: Integer): Integer;
begin
  Result:=id_Indexof(Id);
  if Result >= 0 then
  Result:=Delete(Result)
end;

function TCustomList.Min_id: Integer;
var
  p: PInteger; i: Integer;
begin
  Result:=0;

  if Count > 0 then begin

    p:=@fBuffer[0]; Result:=p^;

    for i:=2 to Count do begin
      Inc(TPointer(p),ItemLen);
      Result:=Min(Result,p^)
    end
  end
end;

function TCustomList.Max_id: Integer;
var
  p: PInteger; i: Integer;
begin
  Result:=0;

  if Count > 0 then begin
    p:=@fBuffer[0];
    for i:=1 to Count do begin
      Result:=Max(Result,p^);
      Inc(TPointer(p),ItemLen)
    end
  end
end;

function TCustomList.Insert_range(AItems: Pointer;
                                  Ind,ACount: Integer): Integer;
var
  i,cx: Integer; si,di: PBytes;
begin
  Result:=-1;

  if Ind < 0 then Ind:=Count;

  if Ind >= 0 then
  if Ind <= fCount then

  if Realloc(fCount+ACount) then begin

    cx:=fItemLen * ACount;
    for i:=fCount-1 downto Ind do begin
      si:=@fBuffer[i * fItemLen];
      Move(si[0],si[cx],fItemLen)
    end;

    if Assigned(AItems) then begin
      di:=@fBuffer[Ind * fItemLen];
      Move(AItems^,di^,cx);
    end;

    Inc(fCount,ACount); fIsChanged:=true;

    Result:=Ind
  end
end;

function TCustomList.Delete_range(Ind,ACount: Integer): Integer;
var
  Len: Integer;
begin
  Result:=-1;

  if Ind >= 0 then
  if Ind + ACount <= fCount then begin

    Exclude(Ind,ACount);
    Result:=Ind; Inc(Ind,ACount);

    if Ind < fCount then begin
      Len:=(fCount-Ind) * fItemLen;
      Move(Items[Ind]^,Items[Result]^,Len)
    end;

    Dec(fCount,ACount); fIsChanged:=true;
    ItemIndex:=ItemIndex
  end
end;

function TCustomList.Update_range(AItems: Pointer;
                                  Ind,ACount: Integer): Integer;
var
  cx: Integer; di: Pointer;
begin
  Result:=-1;

  if Ind >= 0 then
  if Ind + ACount <= fCount then begin

    cx:=ACount * fItemLen; di:=Items[Ind];
    Move(AItems^,di^,cx); Result:=Ind;

    fIsChanged:=true;
  end
end;

function TCustomList.Copy_range(ADest: TCustomList;
                                Ind,ACount: Integer): Integer;
var
  p: PBytes; n: Integer;
begin
  ADest.Clear; p:=Items[Ind];
  n:=Min(ACount,Count - Ind);

  if Assigned(p) and (n > 0) then
  ADest.Insert_range(p,-1,n);

  Result:=ADest.Count
end;

function TCustomList.Exchange(Ind1,Ind2: Integer): Boolean;
var
  p1,p2: PBytes; buf: TBytes; len,loc: Integer;
begin
  Result:=false;

  p1:=Items[Ind1];
  p2:=Items[Ind2];

  len:=fItemLen;

  if Assigned(p1) then
  if Assigned(p2) then begin

    while len > 0 do begin
      loc:=Min(Sizeof(buf),len);
      Move(p1^,buf,loc); Move(p2^,p1^,loc);
      Move(buf,p2^,loc); Dec(len,loc);
      p1:=@p1[loc]; p2:=@p2[loc];
    end;

    Result:=true
  end
end;

function TCustomList.MoveTo(Ind1,Ind2: Integer): Boolean;
var
  p1,p2,pp,sp: PBytes; cx,dx: Integer;
begin
  Result:=false;

  if Ind1 <> Ind2 then
  if (Ind1 >= 0) and (Ind1 < fCount) then
  if (Ind2 >= 0) and (Ind2 < fCount) then

  if Realloc(Count+1) then begin

    p1:=Items[Ind1]; p2:=Items[Ind2];
    if Assigned(p1) then
    if Assigned(p2) then begin
      cx:=fItemLen;
      sp:=@fBuffer[Count*cx];

      Move(p1^,sp^,cx); dx:=cx;
      if Ind1 > Ind2 then dx:=-cx;

      pp:=@p1[dx];

      while Ind1 <> Ind2 do begin
        Move(pp^,p1^,cx); p1:=pp; pp:=@p1[dx];
        if Ind1 < Ind2 then Inc(Ind1) else Dec(Ind1)
      end;

      Move(sp^,p2^,cx); Result:=true
    end
  end
end;

function TCustomList.MoveTop(Ind: int): Boolean;
var
  bx: int; si,di: PBytes; buf: tbytes;
begin
  Result:=false; bx:=ItemLen;
  if bx < Sizeof(buf) then
  if (Ind > 0) and (Ind < Count) then begin

    di:=Items[Ind]; si:=di;

    Move(di^,buf,bx);

    while Ind > 0 do begin
      si:=@si[-bx]; Move(si^,di^,bx);
      di:=si; Dec(Ind)
    end;

    Move(buf,si^,bx)
  end
end;

procedure TCustomList.SwapRange(I1,I2: Integer);
var
  p1,p2: PBytes; cx: Integer; buf: TLongBuf;
begin
  cx:=fItemLen;
  if cx <= Sizeof(buf) then

  if (I1 >= 0) and (I1 < I2) then
  if I2 < fCount then begin

    while I1 < I2 do begin
      p1:=@fBuffer[I1* cx];
      p2:=@fBuffer[I2* cx];

      Move(p1^,buf,cx);
      Move(p2^,p1^,cx);
      Move(buf,p2^,cx);

      Inc(I1); Dec(I2)
    end
  end
end;

procedure TCustomList.Exclude(Ind,ACount: Integer);
begin
end;

procedure TCustomList.Clear;
begin
  Exclude(0,Count); fCount:=0;
  fItemIndex:=-1; fIsChanged:=false
end;

procedure TCustomList.Update(Ind: Integer; Item: Pointer);
var
  P: PBytes;
begin
  P:=Items[Ind]; if Assigned(P) then
  Move(Item^,P^,fItemLen)
end;

procedure TCustomList.Change;
begin
  fIsChanged:=true;
  if Assigned(fOnChange) then
  fOnChange(Self)
end;

function TCustomList.AddList(List: TCustomList): Integer;
begin
  Result:=-1; if List.Count > 0 then
  Result:=Insert_range(List.fBuffer,-1,List.Count)
end;

function TCustomList.LoadList(List: TCustomList): Integer;
begin
  Clear; if List.Count > 0 then
  LoadBuffer(List.fBuffer,List.Count);
  Result:=Count
end;

function TCustomList.Load_ctrl(const Ctrl: TAllocBuffer): Integer;
begin
  Result:=Ctrl.Size div fItemLen;
  Result:=LoadBuffer(Ctrl.Ptr,Result)
end;

function TCustomList.Backup_ctrl(var Ctrl: TAllocBuffer): Integer;
var
  cx: Integer;
begin
  Result:=0; cx:=BufferSize;

  if cx = 0 then
    Ctrl.Size:=0
  else begin
    if Ctrl.Ptr = nil then
    Ctrl.Capacity:=0;

    if cx > Ctrl.Capacity then begin
      Ctrl.Ptr:=xReallocPtr(Ctrl.Ptr,cx);
      if Assigned(Ctrl.Ptr) then
      Ctrl.Capacity:=cx
    end;

    if Assigned(Ctrl.Ptr) then begin
      Move(fBuffer^,Ctrl.Ptr^,cx);
      Ctrl.Size:=cx; Result:=fCount
    end
  end
end;

function TCustomList.LoadBuffer(Items: Pointer;
                                ACount: Integer): Integer;
var
  cx: Integer;
begin
  Clear;

  if Assigned(Items) then
  if Realloc(ACount) then begin
    cx:=fItemLen * ACount;
    Move(Items^,fBuffer^,cx);
    fCount:=ACount; fItemIndex:=0
  end;

  Result:=Count
end;

function TCustomList.LoadFrom(Path: PChar): Integer;
var
  vm: TReadFile;
  i,mk,ax,cx,si: Integer;
begin
  Clear;

  vm:=TReadFile.Create;
  try
    if vm.Open(Path) then
    if vm.Size > 4*3 then begin

      Move(vm.Buf[0],mk,4);
      Move(vm.Buf[4],ax,4);
      Move(vm.Buf[8],cx,4);

      if mk = Get_Marker then
      if ax = fItemLen then

      if cx > 0 then
      if cx*ax = vm.Size-12 then begin

        si:=12; for i:=1 to cx do begin
          Add(@vm.Buf[si]); Inc(si,ItemLen)
        end;
      end
    end;

  finally
    vm.Free
  end;

  Result:=Count
end;

procedure TCustomList.SaveAs(Path: PChar);
var
  h,mk,ax,cx: Integer;
begin
  FileErase(Path);
  if Count > 0 then begin
    h:=FileCreate(Strpas(Path));
    if h > 0 then begin
      mk:=Get_Marker;
      ax:=fItemLen;
      cx:=fCount;

      FileWrite(h,mk,4);
      FileWrite(h,ax,4);
      FileWrite(h,cx,4);

      FileWrite(h,Buffer^,ax * cx);
      FileClose(h)
    end
  end
end;

function TCustomList.doc_LoadFrom(const stg: IStorage; AName: PChar): Integer;
var
  stm: IStream;
  mk,ax,cx,dx: Integer;
begin
  Clear;
  if xOpenStream(stg,AName,false,stm) then begin

    dx:=xSize(stm);
    if dx > 4*3 then begin

      xSeek(stm,0);
      xRead(stm,mk,4);
      xRead(stm,ax,4);
      xRead(stm,cx,4);

      if mk = Get_Marker then
      if ax = ItemLen then

      if cx > 0 then
      if cx*ax = dx-12 then
      if Resize(cx) then

      xRead(stm,Buffer^,cx*ax);
    end;

    stm:=nil
  end;

  Result:=Count
end;

function TCustomList.doc_SaveAs(const stg: IStorage; AName: PChar): Boolean;
var
  mk,ax,cx: Integer; stm: IStream;
begin
  Result:=false;

  if Count = 0 then
    xDeleteElement(stg,AName)
  else begin
    Result:=xOpenStream(stg,AName,true,stm);

    if Result then begin
      stm.SetSize(0); xSeek(stm,0);

      mk:=Get_Marker;
      ax:=ItemLen;
      cx:=Count;

      xWrite(stm,mk,4);
      xWrite(stm,ax,4);
      xWrite(stm,cx,4);

      xWrite(stm,Buffer^,ax * cx);
      stm:=nil
    end
  end
end;

function TCustomList.Sort_up(percent: Float): Integer;
var
  I,J: Integer; p1,p2,tmp: PBytes;
begin
  Result:=0;

  if Count > 1 then
  if Realloc(Count+1) then begin

    Result:=Max(1,Round(Count * percent));
    Result:=Min(Result,Count);

    p1:=Buffer;
    tmp:=@Buffer[Count * ItemLen];

    for I:=1 to Result do begin

      p2:=@p1[ItemLen];
      for J:=I+1 to Count do begin

        if Up_item(p1,p2) then begin
          Move(p1^,tmp^,ItemLen);
          Move(p2^,p1^,ItemLen);
          Move(tmp^,p2^,ItemLen);
          Swap_items(I-1,J-1)
        end;

        p2:=@p2[ItemLen];
      end;

      p1:=@p1[ItemLen]
    end

  end
end;

procedure TCustomList.Swap_items(Ind1,Ind2: Integer);
begin
end;

function TCustomList.SwapList: Boolean;
var
  i1,i2,bx: Integer;
  si,di: Pointer; v: tbytes;
begin
  Result:=false;

  if fItemLen < Sizeof(v) then begin

    if Count > 1 then begin
      i1:=0; i2:=Count-1;
      si:=First; di:=Last; bx:=fItemLen;

      while i1 < i2 do begin
        Move(si^,v,bx);    // v=si
        Move(di^,si^,bx);  // si=di
        Move(v,di^,bx);    // di=v

        Inc(TPointer(si),bx); Inc(i1);
        Dec(TPointer(di),bx); Dec(i2);
      end
    end;

    Result:=true
  end
end;

function TCustomList.sAdd(Item: Pointer): int;
var
  lp: PBytes; Id,bx,ii,i1,i2,n,t: int;
begin
  Result:=-1;

  n:=Count;
  if n = 0 then
    Add(Item)
  else begin
    Id:=PInteger(Item)^;

    lp:=First; bx:=fItemLen;
    i1:=0; i2:=n-1;

    while i1 <= i2 do begin
      ii:=(i1+i2) div 2;
      t:=PInteger(@lp[bx*ii])^;

      if Id < t then i2:=ii-1 else
      if Id > t then i1:=ii+1 else
      begin Result:=ii; Break end
    end;

    if Result < 0 then begin
      Inc(i2);
      if i2 >= n then
        Result:=Add(Item)
      else
        Result:=Insert_range(Item,i2,1)
    end
  end
end;

function TCustomList.sIndexof(Id: int): Pointer;
var
  lp: PBytes; bx,i1,i2,ii,t: int;
begin
  Result:=nil;

  lp:=First; bx:=fItemLen;
  i1:=0; i2:=Count-1;

  while i1 <= i2 do begin
    ii:=(i1+i2) div 2;
    t:=PInteger(@lp[bx*ii])^;

    if Id < t then i2:=ii-1 else
    if Id > t then i1:=ii+1 else begin
      Result:=@lp[bx*ii]; Break
    end
  end
end;

function TCustomList.s64_Add(Item: Pointer): int;
var
  lp: PBytes; id,t: int64; bx,ii,i1,i2,n: int;
begin
  Result:=-1;

  n:=Count;
  if n = 0 then
    Add(Item)
  else begin
    Id:=PInt64(Item)^;

    lp:=First; bx:=fItemLen;
    i1:=0; i2:=n-1;

    while i1 <= i2 do begin
      ii:=(i1+i2) div 2;
      t:=PInt64(@lp[bx*ii])^;

      if Id < t then i2:=ii-1 else
      if Id > t then i1:=ii+1 else
      begin Result:=ii; Break end
    end;

    if Result < 0 then begin
      Inc(i2);
      if i2 >= n then
        Result:=Add(Item)
      else
        Result:=Insert_range(Item,i2,1)
    end
  end
end;

function TCustomList.s64_Indexof(Id: int64): Pointer;
var
  lp: PBytes; bx,i1,i2,ii: int; t: int64;
begin
  Result:=nil;

  lp:=First; bx:=fItemLen;
  i1:=0; i2:=Count-1;

  while i1 <= i2 do begin
    ii:=(i1+i2) div 2;
    t:=PInt64(@lp[bx*ii])^;

    if Id < t then i2:=ii-1 else
    if Id > t then i1:=ii+1 else begin
      Result:=@lp[bx*ii]; Break
    end
  end
end;

constructor TBoolList.Create;
begin
  inherited Create(Sizeof(Boolean),1024)
end;

function TBoolList.AddValue(V: Boolean): Integer;
begin
  Result:=Add(@V)
end;

function TBoolList.Get_Value(I: Integer): Boolean;
var
  P: PBoolean;
begin
  Result:=false; P:=Items[I];
  if Assigned(P) then Result:=P^
end;

procedure TBoolList.Set_Value(I: Integer; V: Boolean);
var
  P: PBoolean;
begin
  P:=Items[I];
  if Assigned(P) then P^:=V
end;

constructor TWordList.Create;
begin
  inherited Create(SizeOf(Word),1024)
end;

constructor TPointerList.Create;
begin
  inherited Create(SizeOf(Pointer),1024)
end;

function TPointerList.AddItem(Item: Pointer): Integer;
begin
  Result:=Add(@Item)
end;

procedure TPointerList.Free_items;
var
  i: Integer; lp: PPointerArray;
begin
  lp:=First;
  for i:=1 to Count do begin
    lp[0]:=xFreePtr(lp[0]); lp:=@lp[1]
  end
end;

function TPointerList.IndexOf(Item: Pointer): Integer;
var
  i: Integer; lp: PPointerArray;
begin
  Result:=-1; lp:=First;

  for i:=0 to Count-1 do begin

    if lp[0] = Item then begin
      Result:=i; Break
    end;

    lp:=@lp[1]
  end
end;

function TPointerList.Get_Value(I: Integer): Pointer;
var
  P: PPointer;
begin
  Result:=0; P:=Items[I];
  if Assigned(P) then Result:=P^
end;

procedure TPointerList.Set_Value(I: Integer; V: Pointer);
var
  P: PPointer;
begin
  P:=Items[I];
  if Assigned(P) then P^:=V
end;

function TPointerList.Sort_up(percent: Float): Integer;
var
  I,J: Integer; tmp,up: Pointer;
  p1,p2: PPointerArray;
begin
  Result:=0;

  if Count = 1 then
    Result:=1
  else
  if Count > 1 then begin

    Result:=Max(1,Round(Count * percent));
    Result:=Min(Result,Count);

    p1:=@Buffer[0];

    for I:=1 to Result do begin

      up:=p1[0]; p2:=@p1[1];

      for J:=I+1 to Count do begin
        if Compare(up,p2[0]) = 1 then begin
          tmp:=up; up:=p2[0]; p2[0]:=tmp
        end;

        p2:=@p2[1];
      end;

      p1[0]:=up; p1:=@p1[1]
    end
  end
end;

function TPointerList.Load_sort_list(List: TCustomList): Integer;
var
  i,bx: Integer; p: Pointer; lp: PBytes;
begin
  Clear; lp:=List.fBuffer;

  bx:=List.ItemLen;
  for i:=1 to List.Count do begin
    p:=@lp[0]; Add(@p); lp:=@lp[bx]
  end;

  Result:=Count
end;

procedure TIntfList.Exclude(Ind,ACount: Integer);
var
  i: Integer; lp: PPointerArray;
begin
  lp:=First;
  for i:=Ind to Ind+ACount-1 do
  if i < fCount then IUnknown(lp[i]):=nil
end;

constructor TIntegerList.Create;
begin
  inherited Create(SizeOf(Integer),1024)
end;

function TIntegerList.GetRange(out v1,v2: int): int;
var
  i,v,_v1,_v2: int; ip: PIntegers;
begin
  _v1:=0; _v2:=0;
  if Count > 0 then begin
    ip:=First;
    for i:=0 to Count-1 do begin
      v:=ip[i];
      if v < _v1 then _v1:=v;
      if v > _v2 then _v2:=v;
    end
  end;

  v1:=_v1; v2:=_v2;
  Result:=_v2-_v1
end;

function TIntegerList.Indexing(ACount: Integer): Integer;
var
  i: Integer; ip: PIntegers;
begin
  if Resize(ACount) then begin
    ip:=First; for i:=0 to Count-1 do
    begin ip[0]:=i; ip:=@ip[1] end
  end; Result:=Count
end;

function TIntegerList.Up_item(p1,p2: Pointer): Boolean;
begin
  if fSortIndex = 0 then
    Result:=PInteger(p2)^ < PInteger(p1)^
  else
    Result:=PInteger(p2)^ > PInteger(p1)^
end;

function TIntegerList.Assign(List: TIntegerList): Integer;
var
  cx,bx: Integer;
begin
  Clear; bx:=List.Count;

  if (bx > 0) and Realloc(bx) then begin
    cx:=bx * Sizeof(Integer);
    Move(List.Buffer^,Buffer^,cx);
    fCount:=bx
  end;

  Result:=Count
end;

function TIntegerList.AddItem(Item: Integer): Integer;
var
  di: PIntegers; i: Integer;
begin
  Result:=-1;

  if fDuplicates then
  Result:=IndexOf(Item);

  if Result < 0 then
  if Realloc(fCount+1) then begin
    di:=Pointer(fBuffer); di[fCount]:=Item;
    if not fDuplicates then Result:=fCount;
    Inc(fCount);

    if fSorted then
    for i:=fCount-2 downto 0 do begin
      if di[i] <= Item then Break;
      di[i+1]:=di[i]; di[i]:=Item;
      if not fDuplicates then Result:=i
    end
  end
end;

function TIntegerList.IndexOf(Item: Integer): Integer;
begin
  Result:=Int_Contains(PIntegers(Buffer),Count,Item)
end;

function TIntegerList.ValToIndex(V: int): int;
var
  ip: PIntegers; i,r,t: int;
begin
  ip:=First; r:=-1;
  Result:=Int_Contains(ip,Count,V);
  if (Result < 0) and (Count > 1) then
  for i:=0 to Count-1 do begin
    t:=Abs(V-ip[i]);
    if (r < 0) or (t < r) then begin
      r:=t; Result:=i
    end
  end
end;

function TIntegerList.DeleteItem(Item: Integer): Integer;
begin
  Result:=Indexof(Item);
  if Result >= 0 then Delete(Result)
end;

function TIntegerList.Get_Edge(I: Integer): Integer;
begin
  if I = 0 then Result:=Values[0]
  else          Result:=Values[Count-1]
end;

function TIntegerList.Get_Value(I: Integer): Integer;
var
  P: PInteger;
begin
  Result:=0; P:=Items[I];
  if Assigned(P) then Result:=P^
end;

procedure TIntegerList.Set_Value(I,Val: Integer);
var
  P: PInteger;
begin
  P:=Items[I];
  if Assigned(P) then P^:=Val
end;

function TIntegerList.Get_Max_Value: Integer;
var
  i: Integer; lp: PIntegers;
begin
  Result:=0; lp:=First;
  for i:=1 to Count do begin
    Result:=Max(Result,lp[0]); lp:=@lp[1]
  end
end;

function TIntegerList.Get_Max_Id: Cardinal;
var
  I: Integer; P: PIntegers;
begin
  Result:=0; P:=First;
  for I:=1 to Count do begin
    Result:=Max(Result,Cardinal(P[0])); P:=@P[1]
  end
end;

function TIntegerList.Containslist(List: TIntegerList): Boolean;
var
  I: Integer; P: PIntegers;
begin
  Result:=false; P:=First;

  for I:=1 to Count do begin
    if List.IndexOf(P[0]) >= 0 then begin
      Result:=true; Break
    end;

    P:=@P[1]
  end
end;

function TIntegerList.Insert(Ind,Val: Integer): Integer;
var
  i: Integer; lp: PIntegers;
begin
  Result:=-1;

  if Ind < 0 then Ind:=Count;

  if Ind >= 0 then
  if Ind <= fCount then

  if Realloc(fCount+1) then begin

    lp:=@fBuffer[fCount*Sizeof(Integer)];
    for i:=fCount-1 downto Ind do begin
      Dec(TPointer(lp),Sizeof(Integer));
      lp[1]:=lp[0]
    end;

    lp[0]:=Val; Inc(fCount);

    fIsChanged:=true;
    Result:=Ind;
  end
end;

function TIntegerList.SwapItems(i1,i2: int): bool;
var
  ip: PIntegers; t: int;
begin
  Result:=false;
  if (i1 >= 0) and (i1 < Count) then
  if (i2 >= 0) and (i2 < Count) then
  if i1 <> i2 then begin
    ip:=First; t:=ip[i1];
    ip[i1]:=ip[i2]; ip[i2]:=t;
    Result:=true
  end
end;

procedure TIntegerList.Enumerate(MinValue: Integer);
var
  i,v: Integer; lp: PIntegers;
begin
  lp:=First; v:=MinValue;
  for i:=1 to Count do begin
    lp[0]:=v; Inc(v); lp:=@lp[1]
  end
end;

function TIntegerList.SwapList: Boolean;
var
  i1,i2,v: int; si,di: PInteger;
begin
  Result:=true;

  if Count > 1 then begin
    i1:=0; i2:=Count-1;
    si:=First; di:=Last;

    while i1 < i2 do begin
      v:=si^; si^:=di^; di^:=v;
      Inc(si); Inc(i1);
      Dec(di); Dec(i2);
    end
  end
end;

function TIntegerList.GetStrings(List: TStrings): Integer;
var
  i: Integer; ip: PIntegers;
begin
  List.BeginUpdate;
  List.Clear;

  ip:=First; for i:=0 to Count-1 do
  List.Add(IntToStr(ip[i]));

  List.EndUpdate;
  Result:=List.Count
end;

function TCustomGrid.Clear_values: Integer;
begin
  if Count > 0 then
  if Assigned(fBuffer) then
  Fillchar(fBuffer^,BufferSize,0);
  Result:=Count
end;

function TCustomGrid.LoadGrid(Grid: TCustomGrid): Integer;
begin
  Clear;
  if Grid.Cols * Grid.Rows = Grid.Count then begin
    LoadList(Grid); fCols:=Grid.Cols; fRows:=Grid.Rows;
  end; Result:=Count
end;

procedure TCustomGrid.Clear;
begin
  inherited; fCols:=0; fRows:=0
end;

function TCustomGrid.Get_row(Row: Integer): Pointer;
begin
  Result:=Items[Row*Cols]
end;

function TCustomGrid.Get_cell(Row,Col: Integer): Pointer;
begin
  Result:=nil;
  if (Col >= 0) and (Col < fCols) then
  if (Row >= 0) and (Row < fRows) then
  Result:=Items[Row*Cols + Col]
end;

procedure TCustomGrid.Set_cols(ACols: Integer);
begin
  fCols:=ACols; Resize(fRows * fCols)
end;

procedure TCustomGrid.Set_rows(ARows: Integer);
begin
  fRows:=ARows; Resize(fRows * fCols)
end;

constructor TByteGrid.Create;
begin
  inherited Create(1,1024)
end;

function TByteGrid.Get_Value(Row,Col: Integer): Integer;
var
  p: PByte;
begin
  Result:=0; p:=Cells[Row,Col];
  if Assigned(p) then Result:=p^
end;

procedure TByteGrid.Set_Value(Row,Col,Val: Integer);
var
  p: PByte;
begin
  p:=Cells[Row,Col];
  if Assigned(p) then p^:=Val
end;

constructor TIntegerGrid.Create;
begin
  inherited Create(SizeOf(Integer),1024)
end;

procedure TIntegerGrid.ClearCol(Col: Integer);
var
  row: Integer;
begin
  for row:=0 to Rows-1 do
  Values[Col,row]:=0
end;

function TIntegerGrid.Indexof(Val: Integer): TPoint;
var
  Ind: Integer;
begin
  Result:=Point(-1,-1);
  if (Count > 0) and (fCols > 0) then begin
    Ind:=Int_Contains(PIntegers(Buffer),Count,Val);
    if Ind >= 0 then begin
      Result.X:=Ind mod fCols;
      Result.Y:=Ind div fCols;
    end
  end
end;

function TIntegerGrid.Get_Value(Row,Col: Integer): Integer;
var
  p: PInteger;
begin
  Result:=0;
  if (Col >= 0) and (Col < fCols) then
  if (Row >= 0) and (Row < fRows) then begin
    p:=Items[ Row * fCols + Col ];
    if Assigned(p) then Result:=p^
  end
end;

procedure TIntegerGrid.Set_Value(Row,Col,Val: Integer);
var
  p: PInteger;
begin
  if (Col >= 0) and (Col < fCols) then
  if (Row >= 0) and (Row < fRows) then begin
    p:=Items[ Row * fCols + Col ];
    if Assigned(p) then p^:=Val
  end
end;

constructor TPointerGrid.Create;
begin
  inherited Create(SizeOf(Pointer),1024)
end;

function TPointerGrid.Get_Value(Row,Col: Integer): Pointer;
var
  p: PPointer;
begin
  Result:=nil;
  if (Col >= 0) and (Col < fCols) then
  if (Row >= 0) and (Row < fRows) then begin
    p:=Items[ Row * fCols + Col ];
    if Assigned(p) then Result:=p^
  end
end;

procedure TPointerGrid.Set_Value(Row,Col: Integer; Val: Pointer);
var
  p: ^PPointer;
begin
  if (Col >= 0) and (Col < fCols) then
  if (Row >= 0) and (Row < fRows) then begin
    p:=Items[ Row * fCols + Col ];
    if Assigned(p) then p^:=Val
  end
end;

function TPointerGrid.val_row_Count(row: Integer): Integer;
var
  col: Integer;
begin
  Result:=0; for col:=0 to fCols-1 do
  if Values[row,col] <> nil then Inc(Result)
end;

function TPointerGrid.val_col_Count(col: Integer): Integer;
var
  row: Integer;
begin
  Result:=0; for row:=0 to fRows-1 do
  if Values[row,col] <> nil then Inc(Result)
end;

procedure TPointerGrid.delete_row(row: Integer);
var
  i,j: Integer;
begin
  if row < fRows-1 then
  for j:=0 to fCols-1 do
  for i:=row to fRows-2 do
  Values[i,j]:=Values[i+1,j];

  Dec(fRows); Truncate(fRows * fCols)
end;

procedure TPointerGrid.delete_col(col: Integer);
var
  i,j: Integer; di: PPointers;
begin
  if col < fCols-1 then
  for i:=0 to fRows-1 do
  for j:=col to fCols-2 do
  Values[i,j]:=Values[i,j+1];

  di:=First;
  for i:=0 to fRows-1 do
  for j:=0 to fCols-2 do begin
    di[0]:=Values[i,j]; di:=@di[1]
  end;

  Dec(fCols); Truncate(fRows * fCols)
end;

procedure TPointerGrid.pack_col(col: Integer);
var
  r,_r: Integer; p: Pointer;
begin
  _r:=0;
  for r:=0 to fRows-1 do begin
    p:=Values[r,col]; if Assigned(p) then
    begin Values[_r,col]:=p; Inc(_r) end;
  end;

  for r:=_r to fRows-1 do
  Values[r,col]:=nil
end;

constructor TPointGrid.Create;
begin
  inherited Create(SizeOf(TPoint),256)
end;

procedure TPointGrid.Fill(vX,vY: int);
var
  i: int; lp: PLPoly; v: TPoint;
begin
  v:=Point(vX,vY); lp:=First;
  for i:=0 to Count-1 do lp[i]:=v
end;

constructor TGaussGrid.Create;
begin
  inherited Create(SizeOf(TGauss),256)
end;

function TGaussGrid.Get_Value(Row,Col: Integer): TGauss;
var
  p: PGauss;
begin
  Result.x:=0; Result.y:=0;
  if (Col >= 0) and (Col < fCols) then
  if (Row >= 0) and (Row < fRows) then begin
    p:=Items[ Row * fCols + Col ];
    if Assigned(p) then Result:=p^
  end
end;

procedure TGaussGrid.Set_Value(Row,Col: Integer; const Val: TGauss);
var
  p: PGauss;
begin
  if (Col >= 0) and (Col < fCols) then
  if (Row >= 0) and (Row < fRows) then begin
    p:=Items[ Row * fCols + Col ];
    if Assigned(p) then p^:=Val
  end
end;

constructor TSingleGrid.Create;
begin
  inherited Create(SizeOf(Single),256)
end;

function TSingleGrid.Get_Value(Row,Col: Integer): Single;
var
  p: PSingle;
begin
  Result:=0; p:=Cells[Row,Col];
  if Assigned(p) then Result:=p^
end;

procedure TSingleGrid.Set_Value(Row,Col: Integer; Val: Single);
var
  p: PSingle;
begin
  p:=Cells[Row,Col];
  if Assigned(p) then p^:=Val
end;

constructor TSingleList.Create;
begin
  inherited Create(Sizeof(Single),256)
end;

function TSingleList.AddItem(Val: Single): Integer;
begin
  Result:=Add(@Val)
end;

procedure TSingleList.Fill(Val: Single);
var
  i: Integer; fp: PSingles;
begin
  fp:=First; for i:=1 to Count do
  begin fp[0]:=Val; fp:=@fp[1] end
end;

procedure TSingleList.Inc_val(Ind: Integer; dVal: Single);
var
  p: PSingle;
begin
  p:=Items[Ind];
  if Assigned(p) then
  p^:=p^ + dVal
end;

function TSingleList.GetValue(Ind: Integer): Single;
var
  p: PSingle;
begin
  Result:=0; p:=Items[Ind];
  if Assigned(p) then Result:=p^
end;

procedure TSingleList.SetValue(Ind: Integer; V: Single);
var
  p: PSingle;
begin
  p:=Items[Ind];
  if Assigned(p) then p^:=V
end;

function TSingleList.GetSum: Double;
var
  i: Integer; fp: PSingles; ax: Double;
begin
  ax:=0; fp:=First;
  for i:=1 to Count do begin
    ax:=ax + fp[0]; fp:=@fp[1]
  end; Result:=ax
end;

function TSingleList.GetAve: Double;
var
  i: Integer; fp: PSingles; ax: Double;
begin
  ax:=0; fp:=First;
  for i:=1 to Count do begin
    ax:=ax + fp[0]; fp:=@fp[1]
  end;

  if Count > 0 then ax:=ax/Count;
  Result:=ax
end;

function TSingleList.GetMax: Single;
var
  i: Integer; fp: PSingles; ax,v: Single;
begin
  Result:=0;
  if Count > 0 then begin
    fp:=First; ax:=fp[0];
    for i:=0 to Count-1 do begin
      v:=fp[i]; if v > ax then ax:=v;
    end; Result:=ax
  end
end;

function TSingleList.Get_min_max(out vmin,vmax: Single): Integer;
var
  i: Integer; fp: PSingles; v,v1,v2: Single;
begin
  v1:=0; v2:=1;

  if Count > 0 then begin
    fp:=First; v1:=fp[0]; v2:=v1;
    for i:=0 to Count-1 do begin
      v:=fp[i];
      if v < v1 then v1:=v;
      if v > v2 then v2:=v;
    end;
  end;

  vmin:=v1; vmax:=v2; Result:=Count
end;

procedure TSingleList.stat(AList: TSingleList);
var
  i,n: Integer; fp1,fp2: PSingles;
begin
  fp1:=First; fp2:=AList.First;

  n:=Min(Count,AList.Count);
  for i:=1 to Count do begin
    fp1[0]:=fp1[0] + fp2[0];
    fp1:=@fp1[1]; fp2:=@fp2[1];
  end
end;

function TSingleList.SwapList: Boolean;
var
  i1,i2: int; si,di: PSingle; v: Single;
begin
  Result:=true;

  if Count > 1 then begin
    i1:=0; i2:=Count-1;
    si:=First; di:=Last;

    while i1 < i2 do begin
      v:=si^; si^:=di^; di^:=v;
      Inc(si); Inc(i1);
      Dec(di); Dec(i2);
    end;
  end
end;

function TSingleList.GetStrings(List: TStrings; m: Integer): Integer;
var
  i: Integer; fp: PSingles;
begin
  List.BeginUpdate;
  List.Clear;

  fp:=First; for i:=0 to Count-1 do
  List.Add(RealToStr(fp[i],m));

  List.EndUpdate;
  Result:=List.Count
end;

constructor TDoubleList.Create;
begin
  inherited Create(Sizeof(Double),1024)
end;

function TDoubleList.Up_item(p1,p2: Pointer): Boolean;
begin
  Result:=false;
  if fSortIndex = 0 then
    Result:=PDouble(p2)^ < PDouble(p1)^
  else
    Result:=PDouble(p2)^ > PDouble(p1)^
end;

function TDoubleList.AddItem(Val: Double): Integer;
begin
  Result:=Add(@Val)
end;

function TDoubleList.Insert(Ind: Integer; Val: Double): Integer;
begin
  Result:=Insert_range(@Val,Ind,1)
end;

function TDoubleList.Get_Value(I: Integer): Double;
var
  P: PDouble;
begin
  Result:=0; P:=Items[I];
  if Assigned(P) then Result:=P^
end;

procedure TDoubleList.Set_Value(I: Integer; Val: Double);
var
  P: PDouble;
begin
  P:=Items[I];
  if Assigned(P) then P^:=Val
end;

function TDoubleList.Get_Range(Ind: Integer): TRange;
var
  I: Integer; P: PDoubles;
begin
  Result.min:=0;
  Result.max:=0;

  if Ind < 0 then Ind:=0;

  if Ind < Count then
  if Count > 0 then begin

    P:=Items[Ind];
    Result.min:=P[0];
    Result.max:=P[0];

    for I:=Ind+1 to Count-1 do begin
      P:=@P[1];
      Result.min:=Min(Result.min,P[0]);
      Result.max:=Max(Result.max,P[0]);
    end
  end
end;

constructor TPointList.Create(ACapacity: Integer);
begin
  inherited Create(SizeOf(TPoint),ACapacity);
end;

function TPointList.GetLength: Double;
var
  lp: PLPoly;
begin
  Result:=0; lp:=First;
  if Count > 0 then
  Result:=PolyLength(lp,Count-1)
end;

function TPointList.IsLock: Boolean;
var
  lp: PLPoly;
begin
  Result:=false; lp:=First; if Count >= 3 then
  Result:=CompareMem(@lp[0],@lp[Count-1],Sizeof(TPoint))
end;

function TPointList.Lock: Boolean;
begin
  Result:=false;

  if IsLock then
    Result:=true
  else
  if Count >= 3 then begin
    Add(First); Result:=IsLock
  end
end;

function TPointList.Get_bound(out lt,rb: TPoint): int;
begin
  lt:=Point(0,0); rb:=lt; if Count > 0 then
  Max_Poly_Bound(First,Count, lt,rb);
  Result:=Count
end;

function TPointList.Get_line(lp: PLLine; Max: int): int;
var
  k: int;
begin
  lp.N:=-1; k:=Count;
  if k > 0 then begin
    k:=Min(k,Max); lp.N:=k-1;
    Move(fBuffer[0],lp.Pol,k*Sizeof(TPoint));
  end;

  Result:=lp.N
end;

procedure TPointList.beginUpdate;
begin
  Extend(Increment);
  flast:=First; fsp:=0;
end;

function TPointList.endUpdate: int;
begin
  Truncate(fsp); Result:=Count
end;

procedure TPointList.AddPoint(x,y: int);
begin
  if fsp = Count then begin
    Extend(fsp+Increment);
    flast:=Items[fsp];
  end;

  if fsp < Count then begin
    flast.X:=x;
    flast.Y:=y;
    Inc(flast);
    Inc(fsp)
  end
end;

function TPointList.AddItem(X,Y: Integer): Integer;
var
  p: TPoint;
begin
  p.X:=X; p.Y:=Y;
  Result:=inherited Add(@p)
end;

function TPointList.Add_idt(X,Y: Integer): Integer;
begin
  Result:=-1;
  if IndexOf(X,Y) < 0 then
  Result:=AddItem(X,Y)
end;

function TPointList.ExcludeItem(X,Y: Integer): Integer;
begin
  Result:=IndexOf(X,Y);
  if Result >= 0 then Delete(Result)
end;

function TPointList.Push(p: PPoint): Integer;
var
  lp: PPoint;
begin
  Result:=-1; lp:=Last;
  if (lp = nil) or (lp.X <> p.X) or (lp.Y <> p.Y) then
  Result:=Add(p)
end;

function TPointList.Insert_point(p: PPoint; Ind: Integer): Integer;
var
  i: Integer; lp: PLPoly;
begin
  Result:=-1;

  if Ind < 0 then Ind:=Count;

  if Ind >= 0 then
  if Ind <= fCount then

  if Realloc(fCount+1) then begin

    lp:=@fBuffer[fCount*Sizeof(TPoint)];
    for i:=fCount-1 downto Ind do begin
      Dec(TPointer(lp),Sizeof(TPoint));
      lp[1]:=lp[0]
    end;

    lp[0]:=p^; Inc(fCount);

    fIsChanged:=true;
    Result:=Ind;
  end
end;

function TPointList.sAdd(X,Y: int): bool;
var
  lp: PLPoly; i,j,i1,i2,n: int; p,t: TPoint;
begin
  Result:=false; p.X:=X; p.Y:=Y;

  n:=Count;
  if n = 0 then
    Add(@p)
  else begin
    lp:=First; i1:=0; i2:=n-1; j:=-1;

    while i1 <= i2 do begin
      i:=(i1+i2) div 2; t:=lp[i];

      if p.Y < t.Y then i2:=i-1 else
      if p.Y > t.Y then i1:=i+1 else

      if p.X < t.X then i2:=i-1 else
      if p.X > t.X then i1:=i+1 else begin
        Result:=true; j:=i; Break
      end
    end;

    Inc(i2); if j < 0 then begin
      if i2 >= n then Add(@p)
      else Insert_range(@p,i2,1);
    end
  end
end;

function TPointList.sIndexOf(X,Y: Integer): Integer;
var
  lp: PLPoly; i1,i2,i: int; p,t: TPoint;
begin
  Result:=-1; p.X:=X; p.Y:=Y;

  lp:=First; i1:=0; i2:=Count-1;

  while i1 <= i2 do begin
    i:=(i1+i2) div 2; t:=lp[i];

    if p.Y < t.Y then i2:=i-1 else
    if p.Y > t.Y then i1:=i+1 else

    if p.X < t.X then i2:=i-1 else
    if p.X > t.X then i1:=i+1 else begin
      Result:=i; Break
    end
  end
end;

function TPointList.Load_line(lp: PLLine): Integer;
begin
  Clear;

  with lp^ do if N >= 0 then
  if Realloc(N+1) then begin
    Move(Pol,fBuffer^,(N+1)*SizeOf(TPoint));
    fCount:=lp.N+1
  end;

  Result:=fCount
end;

function TPointList.Load_poly(lp: PLPoly; N: Integer): Integer;
begin
  Clear;

  if N >= 0 then
  if Realloc(N+1) then begin
    Move(lp^,fBuffer^,(N+1)*SizeOf(TPoint));
    fCount:=N+1
  end;

  Result:=fCount
end;

function TPointList.Add_poly(lp: PLPoly; N: Integer): Integer;
begin
  Insert_Range(lp,-1,N);
  Result:=Count
end;

function TPointList.AddNext(X,Y: Integer): Integer;
var
  lp: PPoint;
begin
  Result:=-1; lp:=Last;
  if lp = nil then
    Result:=AddItem(X,Y)
  else
  if (lp.X <> X) or (lp.Y <> Y) then
    Result:=AddItem(X,Y)
end;

function TPointList.Continue(const p: TPoint): Integer;
var
  lp: PPoint;
begin
  Result:=-1; lp:=Last;
  if lp = nil then
    Result:=Add(@p)
  else
  if (lp.X <> p.X) or (lp.Y <> p.Y) then
    Result:=Add(@p)
end;

function TPointList.Continue1(const p: TPoint): Integer;
var
  lp: PPoint;
begin
  Result:=-1; lp:=First;

  if lp = nil then
    Result:=Add(@p)
  else
  if (lp.X <> p.X) or (lp.Y <> p.Y) then
    Result:=Insert_range(@p,0,1)
end;

function TPointList.xAppend(lp: PLPoly; N: int): int;
var
  lp_: PLPoly; a_,b_,a,b: TPoint;
begin
  Result:=-1; lp_:=First;

  Dec(N); if N > 0 then
  if Count >= 0 then
  if not IsLock then begin

    a_:=lp_[0]; b_:=lp_[Count-1];
    a:=lp[0]; b:=lp[N];

    if Points_Equal(a_,a) then begin
      Swap_LPoly(lp,nil,N);
      Insert_range(lp,0,N);
      Result:=0
    end else
    if Points_Equal(a_,b) then begin
      Insert_range(lp,0,N);
      Result:=0
    end else

    if Points_Equal(b_,a) then begin
      Insert_range(@lp[1],-1,N);
      Result:=0
    end else
    if Points_Equal(b_,b) then begin
      Swap_LPoly(lp,nil,N);
      Insert_range(@lp[1],-1,N);
      Result:=0
    end
  end
end;

function TPointList.LoadFromText(Path: PChar): Integer;
var
  txt: TTextfile; p: TPoint;
begin
  Clear;

  txt:=TTextfile.Create;
  try
    if txt.Open(Path) then
    while txt.xStrLine <> nil do
    if txt.x_Point(p) then Add(@p)
  finally
    txt.Free
  end;

  Result:=Count
end;

procedure TPointList.SaveAsText(Path: PChar);
var
  txt: TTextfile;
  i: Integer; lp: PLPoly;
begin
  txt:=TTextfile.Create;
  try
    lp:=First;
    if txt.Make(Path) then
    for i:=0 to Count-1 do with lp[i] do
    txt.WriteStr(IntToStr(X)+' '+IntToStr(Y));
  finally
    txt.Free
  end;
end;

function TPointList.off_duplicates: Integer;
begin
  if Count > 1 then
  fCount:=dup_LPlan(First,nil,fCount-1)+1;
  Result:=Count
end;

function TPointList.Filter(R: double): Integer;
begin
  if Count > 1 then
  fCount:=cls_LPoly(First,fCount-1,0,R)+1;
  Result:=Count
end;

function TPointList.SwapList: Boolean;
var
  i1,i2: int; si,di: PPoint; v: TPoint;
begin
  Result:=true;

  if Count > 1 then begin
    i1:=0; i2:=Count-1;
    si:=First; di:=Last;

    while i1 < i2 do begin
      v:=si^; si^:=di^; di^:=v;
      Inc(si); Inc(i1);
      Dec(di); Dec(i2);
    end
  end
end;

function TPointList.IndexOf(X,Y: Integer): Integer;
var
  i: Integer; lp: PLPoly;
begin
  Result:=-1; lp:=First;
  for I:=0 to Count-1 do begin
    if (lp[0].X = X) and (lp[0].Y = Y) then
    begin Result:=I; Break end; lp:=@lp[1]
  end
end;

function TPointList.rIndexOf(X,Y,R: Integer): Integer;
var
  i: Integer; lp: PLPoly; p: TPoint;
begin
  Result:=-1; lp:=First;
  for I:=0 to Count-1 do begin

    p:=lp[0]; lp:=@lp[1];

    if Abs(p.X - X) <= R then
    if Abs(p.Y - Y) <= R then

    if Hypot(p.X-X,p.Y-Y) <= R then
    begin Result:=I; Break end;
  end
end;

function TPointList.id_IndexOf(Id: Integer): Integer;
begin
  Result:=-1; if Count > 0 then
  Result:=Index_Contains(First,Count,Id);
end;

function TPointList.id_xIndexOf(Id,Top: Integer): Integer;
var
  lp: PLPoly;
begin
  Result:=-1; lp:=First; if Top < Count then
  Result:=Index_Contains(@lp[Top],Count-Top,Id);
  if Result >= 0 then Inc(Result,Top)
end;

function TPointList.id_Insert(id,val: Integer): Integer;
var
  i: Integer; lp: PLPoly; p: TPoint;
begin
  Result:=-1;

  if id_Indexof(id) < 0 then begin
    lp:=First; p.x:=id; p.y:=val;

    for i:=0 to Count-1 do begin
      if id < lp[0].x then begin
        Result:=i; Break
      end; lp:=@lp[1]
    end;

    if Result < 0 then Result:=Add(@p)
    else Result:=Insert_range(@p,Result,1)
  end
end;

function TPointList.Get_Point(Ind: Integer): TPoint;
var
  P: PPoint;
begin
  Result:=Point(0,0); P:=Items[Ind];
  if Assigned(P) then Result:=P^
end;

procedure TPointList.Set_Point(Ind: Integer; const V: TPoint);
var
  P: PPoint;
begin
  P:=Items[Ind];
  if Assigned(P) then P^:=V
end;

function TPointList.Get_Edge(Ind: Integer): TPoint;
var
  P: PPoint;
begin
  Result:=Point(0,0);
  if Ind = 0 then P:=First else P:=Last;
  if Assigned(P) then Result:=P^
end;

procedure TPointList.Set_Edge(Ind: Integer; const V: TPoint);
var
  P: PPoint;
begin
  if Ind = 0 then P:=First else P:=Last;
  if Assigned(P) then P^:=V
end;

function TPointList.Up_item(p1,p2: Pointer): Boolean;
var
  r1,r2: PPoint;
begin
  Result:=false; r1:=p1; r2:=p2;

  case fSortIndex of

0:  if r2.Y < r1.Y then
      Result:=true
    else
    if r1.Y = r2.Y then
      Result:=r2.X < r1.X;

1:  Result:=r2.X < r1.X;

2:  if r2.X < r1.X then
      Result:=true
    else
    if r2.X = r1.X then
      Result:=r2.Y < r1.Y
  end
end;

constructor VPointList.Create(ACapacity: Integer);
begin
  inherited Create(Sizeof(VPoint),ACapacity)
end;

function VPointList.Up_item(p1,p2: Pointer): Boolean;
var
  v1,v2: VPoint;
begin
  Result:=false;

  v1:=PVPoint(p1)^;
  v2:=PVPoint(p2)^;

  case fSortIndex of
0:  Result:=v1.z > v2.z;
1:  Result:=v1.z < v2.z;
2:  Result:=v2.x > v1.x;
3:  Result:=v2.x < v1.x;
  end
end;

function VPointList.AddItem(_X,_Y,_Z: Integer): Integer;
var
  V: VPoint;
begin
  V.X:=_X; V.Y:=_Y; V.Z:=_Z;
  Result:=inherited Add(@V)
end;

function VPointList.IndexOf(_X,_Y,_Z: Integer): Integer;
var
  I: Integer; lp: PVPoly;
begin
  Result:=-1; lp:=First;

  for I:=0 to Count-1 do begin with lp[0] do
    if (X = _X) and (Y = _Y) and (Z = _Z) then
    begin Result:=I; Break end; lp:=@lp[1]
  end
end;

function VPointList.xy_IndexOf(_X,_Y: Integer): Integer;
var
  I: Integer; lp: PVPoly;
begin
  Result:=-1; lp:=First;

  for I:=0 to Count-1 do begin
    with lp[0] do if (X = _X) and (Y = _Y) then
    begin Result:=I; Break end; lp:=@lp[1]
  end
end;

function VPointList.y_IndexOf(_Y: Integer): Integer;
var
  I: Integer; lp: PVPoly;
begin
  Result:=-1; lp:=First;

  for I:=0 to Count-1 do begin
    with lp[0] do if Y = _Y then
    begin Result:=I; Break end; lp:=@lp[1]
  end
end;

function VPointList.z_IndexOf(_Z: Integer): Integer;
var
  I: Integer; lp: PVPoly;
begin
  Result:=-1; lp:=First;

  for I:=0 to Count-1 do begin
    with lp[0] do if Z = _Z then
    begin Result:=I; Break end; lp:=@lp[1]
  end
end;

function VPointList.id_Insert(id,v1,v2: Integer): Integer;
var
  i: Integer; lp: PVPoly; p: VPoint;
begin
  Result:=-1;

  if id_Indexof(id) < 0 then begin

    p.x:=id; p.y:=v1; p.z:=v2;

    lp:=First;
    for i:=0 to Count-1 do begin
      if id < lp[0].x then begin
        Result:=i; Break
      end; lp:=@lp[1]
    end;

    if Result < 0 then Result:=Add(@p)
    else Result:=Insert_range(@p,Result,1)
  end
end;

function VPointList.xy_ItemOf(_X,_Y: Integer): Pointer;
begin
  Result:=Items[ xy_IndexOf(_X,_Y) ]
end;

function VPointList.Get_Point(Ind: Integer): VPoint;
var
  P: ^VPoint;
begin
  Result:=_VPoint(0,0,0); P:=Items[Ind];
  if Assigned(P) then Result:=P^
end;

procedure VPointList.Set_Point(Ind: Integer; const V: VPoint);
var
  P: ^VPoint;
begin
  P:=Items[Ind];
  if Assigned(P) then P^:=V
end;

function VPointList.Get_min_max(out lt,rb: VPoint): Integer;
var
  i: Integer; lp: PVPoly; v1,v2: VPoint;
begin
  if Count > 0 then begin

    lp:=First; v1:=lp[0]; v2:=v1;

    for i:=2 to Count do begin
      lp:=@lp[1]; with lp[0] do begin
        v1.x:=Min(v1.x,x);
        v1.y:=Min(v1.y,y);
        v1.z:=Min(v1.z,z);
        v2.x:=Max(v2.x,x);
        v2.y:=Max(v2.y,y);
        v2.z:=Max(v2.z,z);
      end
    end;

    lt:=v1; rb:=v2
  end;

  Result:=Count
end;

function VPointList.IsPack: Boolean;
begin
  Result:=VPolyIsPack(First,Count)
end;

function VPointList.Pack: Integer;
begin
  Result:=VPolyPack(First,Count)
end;

procedure VPointList.Unpack(bp: PBytes);
begin
  VPolyUnpack(First,Count,bp)
end;

function VPointList.Load_poly(lp: PLPoly;
                              hp: PIntegers;
                              n: int): int;
var
  i: int; vp: PVPoly; v: lxyz;
begin
  Result:=0;

  if Resize(n) then begin
    vp:=First;
    for i:=0 to Count-1 do begin
      v.p:=lp[i]; v.v.z:=0;
      if Assigned(hp) then v.v.z:=hp[i];
      vp[i]:=v.v
    end;

    Result:=Count
  end
end;

function VPointList.Get_poly(lp: PLPoly;
                             hp: PIntegers;
                             lpMax: int): int;
var
  i: int; vp: PVPoly; v: lxyz;
begin
  Result:=0;

  if Count > 0 then
  if Count < lpMax then begin

    vp:=First;
    for i:=0 to Count-1 do begin
      v.v:=vp[i]; lp[i]:=v.p;
      if Assigned(hp) then hp[i]:=v.v.z
    end;

    Result:=Count
  end
end;

function VPointList.ins_v(i: int; rmu: double): int;
var
  v: PVPoly; p: VPoint;
begin
  Result:=-1;
  if i >= 0 then
  if i < Count-1 then

  if rmu > 0 then
  if rmu < 1 then begin

    v:=Items[i];
    p.x:=v[0].x + Round((v[1].x-v[0].x)*rmu);
    p.y:=v[0].y + Round((v[1].y-v[0].y)*rmu);
    p.z:=v[0].z + Round((v[1].z-v[0].z)*rmu);

    if not VPoints_Equal(p,v[0]) then
    if not VPoints_Equal(p,v[1]) then 
    Result:=Insert_range(@p,i+1,1)
  end
end;

constructor TRectList.Create(ACapacity: Integer);
begin
  inherited Create(Sizeof(TRect),ACapacity)
end;

function TRectList.xAdd(ALeft,ATop,ARight,ABottom: Integer): Integer;
var
  R: TRect;
begin
  R:=Rect(ALeft,ATop,ARight,ABottom);
  Result:=Add(@R)
end;

function TRectList.Get_lt(I: Integer): TPoint;
var
  P: PRect;
begin
  Result:=Point(0,0);
  P:=Items[I]; if Assigned(P) then
  Result:=Point(P.Left,P.Top);
end;

function TRectList.Get_rb(I: Integer): TPoint;
var
  P: PRect;
begin
  Result:=Point(0,0);
  P:=Items[I]; if Assigned(P) then
  Result:=Point(P.Right,P.Bottom);
end;

function TRectList.Get_Rect(I: Integer): TRect;
var
  P: PRect;
begin
  Result:=Rect(0,0,0,0);
  P:=Items[I]; if Assigned(P) then
  Result:=P^
end;

function TRectList.Get_Centre(I: Integer): TPoint;
var
  P: PRect;
begin
  Result:=Point(0,0);
  P:=Items[I]; if Assigned(P) then begin
    Result.X:=Round(P.Left/2 + P.Right/2);
    Result.Y:=Round(P.Top/2 + P.Bottom/2);
  end
end;

function TRectList.Up_item(p1,p2: Pointer): Boolean;
var
  r1,r2: PRect;
begin
  Result:=false; r1:=p1; r2:=p2;

  if r2.Top < r1.Top then
    Result:=true
  else
  if r1.Top = r2.Top then
    Result:=r2.Left < r1.Left
end;

function TRectList.xIndexof(X,Y: Integer): Integer;
var
  i: Integer; rp: PRectArray;
begin
  Result:=-1; rp:=First;

  for i:=0 to Count-1 do begin

    with rp[0] do
    if (X >= Left) and (X <= Right) then
    if (Y >= Top) and (Y <= Bottom) then
    begin Result:=i; Break end;

    rp:=@rp[1]
  end
end;

constructor TGaussList.Create(ACapacity: Integer);
begin
  inherited Create(SizeOf(TGauss),ACapacity)
end;

function TGaussList.Up_item(p1,p2: Pointer): Boolean;
begin
  Result:=PGauss(p2).x < PGauss(p1).x
end;

function TGaussList.LoadFromText(Path: PChar): Integer;
var
  txt: TTextfile; g: TGauss;
begin
  Clear;

  txt:=TTextfile.Create;
  try
    if txt.Open(Path) then
    while txt.xStrLine <> nil do
    if txt.x_Gauss(g) then Add(@g);
  finally
    txt.Free
  end;

  Result:=Count
end;

function TGaussList.AddArray(XY: PDoubles; ACount,AStride: Integer): Integer;
var
  i,bx: Integer; lp: PDoubles; p: TGauss;
begin
  lp:=XY; bx:=Max(2,AStride);

  for i:=1 to ACount do begin
    p.x:=lp[0]; p.y:=lp[1];
    Add(@p); lp:=@lp[bx]
  end;

  Result:=Count
end;

function TGaussList.Load_LPoly(lp: PLPoly; N: Integer): Integer;
var
  i: Integer; p,q: TPoint;
begin
  Clear;

  for i:=1 to N do begin
    p:=lp[0]; lp:=@lp[1];

    if (i = 1)
    or (p.X <> q.X)
    or (p.Y <> q.Y) then
    AddItem(p.X,p.Y);

    q:=p
  end;

  Result:=Count
end;

procedure TGaussList.Get_Min_Max(out lt,rb: TGauss);
begin
  lt:=gauss_nil; rb:=lt;
  if Count > 0 then
  Max_Gauss_Bound(First,Count, lt,rb);
end;

function TGaussList.Get_Centre: TGauss;
var
  i: Integer; lp: PGPoly;
  cx,cy: Extended;
begin
  cx:=0; cy:=0;
  if Count > 0 then begin
    lp:=First;
    for i:=1 to Count do begin
      cx:=cx + lp[0].x;
      cy:=cy + lp[0].y;
      lp:=@lp[1]
    end;

    cx:=cx/Count;
    cy:=cy/Count;
  end;

  Result.x:=cx;
  Result.y:=cy;
end;

function TGaussList.Get_Square: Double;
begin
  Result:=0; if Count > 3 then
  Result:=gauss_Square(First,Count)
end;

function TGaussList.Get_max_dim: Double;
var
  lt,rb: TGauss;
begin
  Result:=0;
  if Count > 3 then begin
    Get_Min_Max(lt,rb);
    Result:=Max(rb.x-lt.x,rb.y-lt.y)
  end
end;

function TGaussList.Locate(w: PGPoly;
                           out dist: Double;
                           out c: TGauss): Integer;
var
  i: Integer; a,b: TGauss; gp: PGPoly; d,r: Double;
  p1,p2,lt,rb,pc: TPoint;
begin
  Result:=-1; gp:=First;

  d:=0; p1:=Point(0,0);

  if Count > 0 then
  if GaussContainsPoint(w[0],w[1],gp[0]) then begin
    dist:=0; c:=gp[0]; Result:=0;
  end else
  for i:=0 to Count-2 do begin
    a:=gp[i]; b:=gp[i+1];
    r:=Gauss_dist(a,b);

    if Min(a.x,b.x) < w[1].x then
    if Min(a.y,b.y) < w[1].y then
    if Max(a.x,b.x) > w[0].x then
    if Max(a.y,b.y) > w[0].y then

    if GaussContainsPoint(w[0],w[1],b) then begin
      dist:=d+r; c:=b; Result:=i+1; Break
    end
    else begin
      p2.X:=Round((b.x-a.x)*100);
      p2.Y:=Round((b.y-a.y)*100);

      lt.X:=Round((w[0].x-a.x)*100);
      lt.Y:=Round((w[0].y-a.y)*100);

      rb.X:=Round((w[1].x-a.x)*100);
      rb.Y:=Round((w[1].y-a.y)*100);

      if Locate_Line(p1,p2, lt,rb, pc) then begin
        c.x:=a.x + pc.X/100;
        c.y:=a.y + pc.Y/100;
        dist:=d+Gauss_dist(a,c);
        Result:=i; Break
      end
    end;

    d:=d+r
  end
end;

function TGaussList.Backup(pos: Double;
                           out dpos: Double;
                           out c,v: TGauss): Integer;
var
  i: Integer; a,b,d,d1: TGauss;
  gp: PGPoly; len,rib: Double;
begin
  Result:=-1; dpos:=0;

  c:=_Gauss(0,0); v:=_Gauss(0,1);

  if Count > 0 then begin

    gp:=First; b:=gp[0]; len:=0; 

    for i:=1 to Count-1 do begin
      a:=b; b:=gp[i];
      d.x:=b.x - a.x;
      d.y:=b.y - a.y;

      rib:=Hypot(d.x,d.y);
      len:=len + rib;

      if rib > Small then begin

        d.x:=d.x/rib; d.y:=d.y/rib;
        c:=b; v:=d; dpos:=0; Result:=i;

        if Abs(len-pos) < Small then begin
          if i < Count-1 then begin
            d1:=gp[i+1];
            d1.x:=d1.x - b.x;
            d1.y:=d1.y - b.y;
            if xNorm(d1) then begin
              v.x:=(d.x + d1.x)/2;
              v.y:=(d.y + d1.y)/2;
              xNorm(v)
            end
          end;

          Break
        end else
        if len > pos then begin
          dpos:=pos + rib - len;
          c:=Next_Gauss(a,b,dpos);
          Result:=i-1; Break
        end
      end
    end
  end
end;

function TGaussList.Normalize(centre,aspect: Boolean): Integer;
var
  i: Integer; lp: PGPoly;
  lt,rb: TGauss; kx,ky: Double;
begin
  Get_Min_Max(lt,rb);

  if centre then begin
    if (lt.x < 0) and (rb.x > 0) then begin
      lt.x:=-Max(-lt.x,rb.x); rb.x:=-lt.x
    end;

    if (lt.y < 0) and (rb.y > 0) then begin
      lt.y:=-Max(-lt.y,rb.y); rb.y:=-lt.y
    end;
  end;

  kx:=1/Max(1,rb.x-lt.x);

  if aspect then ky:=kx else
  ky:=1/Max(1,rb.y-lt.y);

  lp:=First;
  for i:=1 to Count do begin

    with lp[0] do begin
      x:=(x-lt.x)*kx;
      y:=(y-lt.y)*ky;
    end;

    lp:=@lp[1]
  end;

  Result:=Count
end;

function TGaussList.eps_Indexof(X,Y,Eps: Double): Integer;
var
  i: Integer; lp: PGPoly;
begin
  Result:=-1; lp:=First;

  for i:=0 to Count-1 do begin
    if Abs(lp[0].x-X) < Eps then
    if Abs(lp[0].y-Y) < Eps then
    begin Result:=i; Break end;
    lp:=@lp[1]
  end
end;

function TGaussList.eps_sync(var p: TGauss; Eps: Double): int;
var
  i: int; t,_p: TGauss; lp: PGPoly;
begin
  Result:=-1; lp:=First; _p:=p;

  for i:=0 to Count-1 do begin
    t:=lp[i];
    if Abs(t.x-_p.X) < Eps then
    if Abs(t.y-_p.Y) < Eps then begin
      p:=t; Result:=i; Break
    end;
  end
end;

function TGaussList.AddItem(X,Y: Double): Integer;
var
  P: TGauss;
begin
  P.X:=X; P.Y:=Y;
  Result:=inherited Add(@P)
end;

function TGaussList.Get_Point(Ind: Integer): TGauss;
var
  P: ^TGauss;
begin
  Result.X:=0; Result.Y:=0; P:=Items[Ind];
  if Assigned(P) then Result:=P^
end;

function TGaussList.Get_First_Point: TGauss;
begin
  Result:=Points[0]
end;

function TGaussList.Get_Last_Point: TGauss;
begin
  Result:=Points[Count-1]
end;

procedure TGaussList.Set_Point(Ind: Integer; const P: TGauss);
begin
  Update(Ind,@P)
end;

function TGaussList.GetLength: Double;
var
  i: Integer; lp: PGPoly;
  dx,dy: Double; ax: Extended;
begin
  ax:=0; lp:=First;
  for i:=0 to Count-2 do begin
    dx:=lp[1].x-lp[0].x;
    dy:=lp[1].y-lp[0].y;
    ax:=ax + Hypot(dx,dy);
    lp:=@lp[1]
  end;

  Result:=ax
end;

function TGaussList.SwapList: Boolean;
var
  i1,i2: int; si,di: PGauss; v: TGauss;
begin
  Result:=true;

  if Count > 1 then begin
    i1:=0; i2:=Count-1;
    si:=First; di:=Last;

    while i1 < i2 do begin
      v:=si^; si^:=di^; di^:=v;
      Inc(si); Inc(i1);
      Dec(di); Dec(i2);
    end
  end
end;

constructor TXyzList.Create(ACapacity: Integer);
begin
  inherited Create(SizeOf(txyz),ACapacity)
end;

function TXyzList.Get_Point(I: Integer): txyz;
var
  v: txyz; p: pxyz;
begin
  v.x:=0; v.y:=0; v.z:=0; p:=Items[I];
  if Assigned(p) then v:=p^; Result:=v
end;

procedure TXyzList.Set_Point(I: Integer; const v: txyz);
var
  p: pxyz;
begin
  p:=Items[I]; if Assigned(p) then p^:=v
end;

function TXyzList.xAdd(const v: txyz): Integer;
begin
  with fcube do
  if Count = 0 then begin
    vmin:=v; vmax:=v;
  end
  else begin
    if v.x < vmin.x then vmin.x:=v.x;
    if v.y < vmin.y then vmin.y:=v.y;
    if v.z < vmin.z then vmin.z:=v.z;

    if v.x > vmax.x then vmax.x:=v.x;
    if v.y > vmax.y then vmax.y:=v.y;
    if v.z > vmax.z then vmax.z:=v.z;
  end;

  Result:=Add(@v)
end;

function TXyzList.AddItem(X,Y,Z: Double): Integer;
var
  v: txyz;
begin
  v.x:=X; v.y:=Y; v.z:=Z;
  Result:=Add(@v)
end;

function TXyzList.xIndexof(X,Y,Z,Eps: Double): Integer;
var
  i: Integer; lp: pxyz_array; dx,dy,dz: Double;
begin
  Result:=-1; lp:=First;

  for i:=0 to Count-1 do begin
    dx:=X-lp[0].x; dy:=Y-lp[0].y; dz:=Z-lp[0].z;
    if Sqrt(dx*dx + dy*dy + dz*dz) <= Eps then
    begin Result:=i; Break end; lp:=@lp[1]
  end;
end;

procedure TXyzList.Get_min_max(out lt,rb: txyz);
var
  v1,v2: txyz;
begin
  v1:=xyz_nil; v2:=lt;

  if Count > 0 then
  xyzv_min_max(First,Count,v1,v2);

  fcube.vmin:=v1; lt:=v1;
  fcube.vmax:=v2; rb:=v2;
end;

procedure TXyzList.Transit(const tr: TMatrix);
var
  i: Integer; lp: pxyz_array; v: txyz;
begin
  lp:=First;
  for i:=1 to Count do begin
    v:=xyz_matrix(lp[0],tr);
    lp[0]:=v; lp:=@lp[1]
  end
end;

function TXyzList.IsLock(eps: Double): Boolean;
var
  p1,p2: pxyz;
begin
  Result:=false;
  if Count >= 2 then begin
    p1:=First; p2:=Last;
    if Abs(p1.x-p2.x) <= eps then
    if Abs(p1.y-p2.y) <= eps then
    if Abs(p1.z-p2.z) <= eps then
    Result:=true
  end
end;

function TXyzList.Lock(eps: Double): Boolean;
var
  p: pxyz;
begin
  Result:=false;
  if not IsLock(eps) then
  if Count >= 3 then begin
    p:=First; Add(p);
    Result:=IsLock(eps) 
  end
end;

function TXyzList.Sum_values: txyz;
var
  i,n: Integer; xp1,xp2: pxyz_array;
  v: txyz;
begin
  Result:=_xyz(0,0,0);

  while Count > 1 do begin

    i:=0; n:=0; xp1:=First; xp2:=xp1;
    while i < Count-1 do begin
      v.x:=(xp1[0].x + xp1[1].x) / 2;
      v.y:=(xp1[0].y + xp1[1].y) / 2;
      v.z:=(xp1[0].z + xp1[1].z) / 2;
      xp2[n]:=v; Inc(n); xp1:=@xp1[2];
      Inc(i,2)
    end;

    if i < Count then begin
      xp2[n]:=xp2[i]; Inc(n)
    end;

    Truncate(n);
  end;

  if Count > 0 then
  Result:=Points[0]
end;

function TXyzList.SwapList: Boolean;
var
  i1,i2: int; p1,p2: pxyz; v: txyz;
begin
  Result:=true;

  if Count > 1 then begin
    i1:=0; i2:=Count-1;
    p1:=Items[0]; p2:=Items[i2];

    while i1 < i2 do begin
      v:=p1^; p1^:=p2^; p2^:=v;
      Inc(i1); Inc(p1);
      Dec(i2); Dec(p2);
    end
  end
end;

function TXyzList.Get_Length: Double;
var
  i: Integer; lp: pxyz_array;
  ax: Extended;
begin
  lp:=First; ax:=0;
  for i:=1 to Count-1 do begin
    ax:=ax + xyz_dist(lp[0],lp[1]);
    lp:=@lp[1]
  end;

  Result:=ax;
end;

procedure TXyzList.up_z(dz: float);
var
  i,n: int; vp: pxyz_array;
begin
  vp:=First; n:=Count;
  for i:=0 to n-1 do
  vp[i].z:=vp[i].z + dz;
end;

constructor TKeyList.Create;
begin
  inherited Create(Sizeof(TKeyRecord),1024)
end;

function TKeyList.AddItem(Id,Val: Int64): Integer;
var
  i1,i2,ii: int; _id: int64;
  lp: PKeyArray; r: TKeyRecord;
begin
  Result:=-1;

  r.Id:=Id; r.Val:=Val;

  if Count = 0 then begin
    fminId:=Id; fmaxId:=Id
  end
  else begin
    if Id < fminId then fminId:=Id;
    if Id > fmaxId then fmaxId:=Id
  end;

  if not Sorted then
    Result:=Add(@r)
  else begin
    lp:=First; i1:=0; i2:=Count-1;

    while i1 <= i2 do begin
      ii:=(i1+i2) div 2; _id:=lp[ii].Id;

      if Id < _id then i2:=ii-1 else
      if Id > _id then i1:=ii+1 else begin
        Result:=Insert_range(@r,ii+1,1);
        Break
      end
    end;

    Inc(i2); if Result < 0 then begin
      if i2 >= Count then Result:=Add(@r)
      else Result:=Insert_range(@r,i2,1);
    end
  end
end;

function TKeyList.Id_Indexof(Id: Int64): Integer;
var
  i,i1,i2: int; _id: int64; p: PKeyArray;
begin
  Result:=-1;

  if Id >= fminId then
  if Id <= fmaxId then begin

    p:=PKeyArray(Buffer);

    if fSorted then begin

      i1:=0; i2:=Count-1;
      while i1 <= i2 do begin
        i:=(i1+i2) div 2; _id:=p[i].Id;

        if Id < _id then i2:=i-1 else
        if Id > _id then i1:=i+1 else begin

          Result:=i;
          while i > 0 do begin Dec(i);
            if p[i].Id <> _Id then Break;
            Result:=i
          end;

          Break
        end
      end

    end else

    for i:=0 to Count-1 do begin
      if p[0].Id = Id then begin
        Result:=i; Break
      end; p:=@p[1]
    end
  end
end;

function TKeyList.Val_Indexof(Val: Int64): Integer;
var
  i: int; p: PKeyArray;
begin
  Result:=-1; p:=PKeyArray(Buffer);

  for i:=0 to Count-1 do begin
    if p[0].Val = Val then begin
      Result:=i; Break
    end; p:=@p[1]
  end;
end;

function TKeyList.Get_Value(Id: Int64): Int64;
var
  P: PKeyRecord; I: Integer;
begin
  Result:=0; I:=Id_Indexof(Id);

  if I >= 0 then begin
    P:=Items[I]; Result:=P.Val
  end
end;

function TKeyList.Get_Name(Val: Int64): Int64;
var
  P: PKeyRecord; I: Integer;
begin
  Result:=0; I:=Val_Indexof(Val);

  if I >= 0 then begin
    P:=Items[I]; Result:=P.Id
  end
end;

function TKeyList.Get_Left(I: Integer): Int64;
var
  P: PKeyRecord;
begin
  Result:=0; P:=Items[I];
  if P <> nil then Result:=P.Id
end;

function TKeyList.Get_Right(I: Integer): Int64;
var
  P: PKeyRecord;
begin
  Result:=0; P:=Items[I];
  if P <> nil then Result:=P.Val
end;

function TKeyList.Up_item(p1,p2: Pointer): Boolean;
begin
  Result:=PKeyRecord(p1).Val > PKeyRecord(p2).Val 
end;

constructor TCharArray.Create;
begin
  inherited Create(1,1024)
end;

function TCharArray.Get_Strings: PChar;
begin
  Result:=PChar(fBuffer)
end;

function TCharArray.AddLine(S: PChar): Integer;
var
  len: Integer; P: PChar;
begin
  if Assigned(S) then begin
    len:=Strlen(S);

    if Count = 0 then Inc(len)
    else Inc(len,2);

    if Realloc(fCount+len) then begin
      P:=Strings;

      if Count = 0 then StrCopy(P,S)
      else StrCat(StrCat(P,#13#10),S);

      fCount:=Strlen(P)+1
    end
  end;

  Result:=fCount
end;

function TCharArray.AddString(const S: String): Integer;
var
  buf: TShortstr;
begin
  StrPLCopy(buf,S,255);
  Result:=AddLine(buf)
end;

function TCharArray.AddReal(V: Double; M: Integer): Integer;
begin
  Result:=AddString( RealToStr(V,M) )
end;

function TCharArray.AddInteger(V: Integer): Integer;
begin
  Result:=AddString( IntToStr(V) )
end;

function TCharArray.AddStrings(Lines: TStrings): Integer;
var
  I: Integer; S: TShortStr;
begin
  for I:=0 to Lines.Count-1 do begin
    StrPCopy(S,Lines[I]);
    AddLine(LStr(RStr(S)));
  end;

  Result:=fCount
end;

function TIdList.id_Indexof(Id: Integer): Integer;
var
  P: PInteger; I: Integer;
begin
  Result:=-1; P:=First;
  for I:=0 to Count-1 do begin
    if P^ = Id then begin
      Result:=I; Break
    end;

    P:=Pointer(@PBytes(P)[fItemLen])
  end
end;

function TIdList.id_Ptr(Id: Integer): Pointer;
var
  P: PInteger; I: Integer;
begin
  Result:=nil; P:=First;
  for I:=0 to Count-1 do begin
    if P^ = Id then begin
      Result:=P; Break
    end;

    P:=Pointer(@PBytes(P)[fItemLen])
  end
end;

function TIdList.Id_Delete(Id: Integer): Boolean;
begin
  Result:=Delete(id_IndexOf(Id)) >= 0
end;

destructor TIndexControl.Destroy;
begin
  xFreePtr(fBits); inherited
end;

procedure TIndexControl.Set_max_NNNN(Value: Cardinal);
var
  cx: Integer;
begin
  cx:=int_Tiles(Value,32);
  fBits:=xAllocInt(cx);
  fmax_NNNN:=cx * 32 - 1
end;

function TIndexControl.Control(Id: Cardinal): Boolean;
var
  dx,bit: Cardinal; si: PInteger;
begin
  Result:=true; if Id > 0 then

  if Id > fmax_NNNN then begin

    if IndexOf(Id) >= 0 then
      Result:=false
    else
      Add(@Id)

  end else
  if Assigned(fBits) then begin
    dx:=Id div 32; si:=@fBits[dx];
    bit:=1 shl (Id and 31);

    Result:=si^ and bit = 0;
    si^:=si^ or bit;
  end
end;

constructor TChannelList.Create(ACapacity: Integer);
begin
  inherited Create(Sizeof(tchannel),ACapacity)
end;

constructor THChannelList.Create(ACapacity: Integer);
begin
  inherited Create(Sizeof(thchannel),ACapacity)
end;

constructor TDataStream.Create(AIncrement: Integer);
begin
  inherited Create;
  fIncrement:=AIncrement
end;

destructor TDataStream.Destroy;
begin
  xFreePtr(fBuffer); inherited
end;

function TDataStream.Pop(len: Integer): Integer;
begin
  if len >= fSize then fSize:=0 else begin

    Dec(fSize,len);
    Move(fBuffer[len],fBuffer[0],fSize);

  end; Result:=len
end;

function TDataStream.LoadFromBuf(Data: Pointer; DataLen: int): bool;
begin
  Result:=false;

  if DataLen > 0 then
  if Resize(DataLen) then begin
    Move(Data^,fBuffer^,DataLen);
    fSize:=DataLen; Result:=true
  end
end;

function TDataStream.doc_LoadFrom(const stg: IStorage;
                                  AName: PChar): Integer;
var
  stm: IStream; sz: Integer;
begin
  fSize:=0; fPosition:=0;

  if xOpenStream(stg,AName,false,stm) then begin
    sz:=xSize(stm); if sz > 0 then
    if Resize(sz) then begin
      xSeek(stm,0); xRead(stm,fBuffer^,sz);
      fSize:=sz
    end

  end; stm:=nil;
end;

function TDataStream.LoadStream(stm: TStream): Integer;
begin
  fSize:=0;
  fPosition:=0;

  if stm.Size > 0 then
  if Resize(stm.Size) then begin
    stm.Seek(0,soFromBeginning);
    stm.Read(fBuffer^,stm.Size);
    fSize:=stm.Size
  end;

  Result:=fSize
end;

function TDataStream.LoadData(stm: TDataStream): Integer;
begin
  fSize:=0;
  fPosition:=0;

  if stm.Size > 0 then
  if Resize(stm.Size) then begin
    Move(stm.Buffer^,Buffer^,stm.Size);
    fSize:=stm.Size
  end;

  Result:=fSize
end;

function TDataStream.LoadFile(APath: PChar): Integer;
var
  vm: TReadfile; sz: Integer;
begin
  Size:=0;

  vm:=TReadfile.Create;
  try
    if vm.Open(APath) then
    if vm.Size > 0 then begin

      sz:=vm.Size;
      if Resize(sz) then begin
        Move(vm.Buf[0],fBuffer^,sz);
        fSize:=sz
      end
    end;
  finally
    vm.Free
  end;

  Result:=fSize
end;

function TDataStream.LoadFromFile(APath: PChar): Integer;
var
  vm: TReadfile; mag,sz: Integer;
begin
  Size:=0;

  vm:=TReadfile.Create;
  try
    if vm.Open(APath) then
    if vm.Size > 8 then begin

      mag:=vm.Get_long(0);
      sz:=vm.Get_long(4);

      if mag = 999999 then
      if sz = vm.Size-8 then
      if Resize(sz) then begin
        Move(vm.Buf[8],fBuffer^,sz);
        fSize:=sz
      end
    end;
  finally
    vm.Free
  end;

  Result:=fSize
end;

function TDataStream.SaveToBin(APath: PChar): Boolean;
var
  h: THandle;
begin
  Result:=false;
  if fSize > 0 then begin
    h:=FileCreate(APath);
    if h > 0 then begin
      FileWrite(h,fBuffer^,fSize);
      FileClose(h); Result:=true
    end
  end
end;

function TDataStream.SaveToFile(APath: PChar): Boolean;
var
  h,mag,sz: Integer;
begin
  Result:=false;
  if fSize > 0 then begin
    h:=FileCreate(Strpas(APath));
    if h > 0 then begin
      mag:=999999; sz:=fSize;
      FileWrite(h,mag,4);
      FileWrite(h,sz,4);
      FileWrite(h,fBuffer^,fSize);
      FileClose(h); Result:=true
    end
  end
end;

function TDataStream.SaveAs(APath: PChar): Boolean;
var
  h: Integer;
begin
  Result:=false;
  if fSize > 0 then begin
    h:=FileCreate(Strpas(APath));
    if h > 0 then begin
      FileWrite(h,fBuffer^,fSize);
      FileClose(h); Result:=true
    end
  end
end;

function TDataStream.Resize(NewSize: Integer): Boolean;
var
  cx: Integer;
begin
  cx:=int_Round(NewSize,fIncrement);

  if fBuffer = nil then begin
    fCapacity:=cx;
    fBuffer:=xAllocPtr(cx)
  end else
  if NewSize > fCapacity then begin
    fCapacity:=cx;
    fBuffer:=xReAllocPtr(fBuffer,cx)
  end;

  if fBuffer = nil then fSize:=0;
  Result:=Assigned(fBuffer)
end;

function TDataStream.Expand(len: Integer): Boolean;
begin
  Result:=false;
  if Resize(fSize + len) then begin
    Inc(fSize,len); Result:=true
  end
end;

function TDataStream.Append(var rec; len: Integer): Integer;
begin
  Result:=-1;
  if Resize(Size + len) then begin
    Move(rec,fBuffer[Size],len);
    Result:=Size; Inc(fSize,len)
  end
end;

function TDataStream.Appendi1(v: Integer): Integer;
begin
  Result:=-1;
  if Resize(Size + 1) then begin
    fBuffer[Size]:=v;
    Result:=Size; Inc(fSize)
  end
end;

function TDataStream.Appendi2(v: Integer): Integer;
begin
  Result:=-1;
  if Resize(Size + 2) then begin
    Move(v,fBuffer[Size],2);
    Result:=Size; Inc(fSize,2)
  end
end;

function TDataStream.Appendi4(v: Integer): Integer;
begin
  Result:=-1;
  if Resize(Size + 4) then begin
    Move(v,fBuffer[Size],4);
    Result:=Size; Inc(fSize,4)
  end
end;

function TDataStream.Appendi8(v: Int64): Integer;
begin
  Result:=-1;
  if Resize(Size + 8) then begin
    Move(v,fBuffer[Size],8);
    Result:=Size; Inc(fSize,8)
  end
end;

function TDataStream.Appendf(v: float): Integer;
begin
  Result:=-1;
  if Resize(Size + 4) then begin
    Move(v,fBuffer[Size],4);
    Result:=Size; Inc(fSize,4)
  end
end;

function TDataStream.AppendStream(stm: TStream): Integer;
var
  len: Integer;
begin
  Result:=-1;

  len:=stm.Size; if len > 0 then

  if Resize(Size + len) then begin
    stm.Seek(0,soFromBeginning);
    stm.Read(fBuffer[Size],len);
    Result:=Size; Inc(fSize,len)
  end
end;

function TDataStream.Load(pos: Integer; var rec; len: Integer): Integer;
begin
  if pos + len <= Size then
  Move(fBuffer[pos],rec,len);
  Result:=pos + len
end;

function TDataStream.Store(pos: Integer; var rec; len: Integer): Integer;
begin
  if pos + len <= Size then
  Move(rec,fBuffer[pos],len);
  Result:=pos + len
end;

function TDataStream.Seek(Offset,Origin: Integer): Boolean;
var
  pos: Integer;
begin
  Result:=false; pos:=fPosition;

  case Origin of
soFromBeginning:
    pos:=Offset;
soFromCurrent:
    pos:=fPosition + Offset;
soFromEnd:
    pos:=fSize + Offset;
  end;

  if (pos >= 0) and (pos <= fSize) then begin
    fPosition:=pos; Result:=true
  end
  else fPosition:=fSize
end;

function TDataStream.Get(var rec; len: Integer): Boolean;
begin
  Result:=false;
  if fPosition + len <= fSize then begin
    Move(fBuffer[fPosition],rec,len);
    Inc(fPosition,len); Result:=true
  end
end;

function TDataStream.Get_long(out v: Integer): Boolean;
begin
  Result:=false;
  if fPosition + 4 <= fSize then begin
    v:=PInteger(@fBuffer[fPosition])^;
    Inc(fPosition,4); Result:=true;
  end
end;

function TDataStream.Get_data(offs,len: int): Pointer;
begin
  Result:=nil;
  if Assigned(fBuffer) then

  if offs >= 0 then
  if offs+len <= fSize then
  Result:=@fBuffer[offs];
end;

function TDataStream.Get_Pointer(Offset: Integer): Pointer;
begin
  Result:=nil;
  if Assigned(fBuffer) then
  if Offset <= fSize then
  Result:=@fBuffer[Offset];
end;

function TDataStream.Get_Ptr(len: Integer): Pointer;
begin
  Result:=nil;
  if Assigned(fBuffer) then
  if fPosition + len <= fSize then
  Result:=@fBuffer[fPosition];
end;

function TDataStream.Append_poly(lp: PLPoly; n: Integer): Integer;
begin
  if Append(n,Sizeof(n)) >= 0 then
  Result:=Append(lp^,n*Sizeof(TPoint))
end;

function TDataStream.Get_poly(lp: PLPoly; lp_Max: Integer): Integer;
var
  n: Integer;
begin
  Result:=0; if Get(n,Sizeof(n)) then
  if (n > 0) and (n <= lp_Max) then
  if Get(lp^,n * Sizeof(TPoint)) then
  Result:=n
end;

function TDataStream.Skip_poly: Integer;
var
  n: Integer;
begin
  if Get(n,Sizeof(n)) and (n > 0) then
  Seek(n * Sizeof(TPoint),soFromCurrent);
  Result:=fPosition
end;

function TDataStream.Cut(pos,len: Integer): Integer;
var
  bx,cx: Integer;
begin
  Result:=0;
  if (pos >= 0) and (pos < fSize) then begin

    if pos+len > fSize then len:=fSize-pos;

    bx:=pos+len; cx:=Size-bx; if cx > 0 then
    Move(Buffer[bx],Buffer[pos],cx);

    fSize:=fSize - len;
    
    Changed; Result:=len
  end
end;

procedure TDataStream.Changed;
begin
  fIsChanged:=true;
  Notify(Self,fOnChanged)
end;

procedure TUndoStream.Push(Buf: PBytes; BufLen: Integer);
var
  bx: Integer;
begin
  bx:=Size; Append(BufLen,4);
  Append(Buf^,BufLen); Append(bx,4);
end;

function TUndoStream.Pop(Buf: PBytes; BufSize: Integer): Integer;
var
  bx,cx: Integer;
begin
  Result:=0;
  if fSize >= 8 then begin
    Load(fSize-4,bx,4);
    if bx < 0 then bx:=0;

    if bx > fSize-8 then
      bx:=0
    else begin
      bx:=Load(bx,cx,4);

      if bx+cx+4 <> fSize then
        bx:=0
      else begin
        cx:=Min(cx,BufSize);
        Load(bx,Buf^,cx); Result:=cx
      end

    end;

    fSize:=Max(0,bx-4)
  end
end;

end.
