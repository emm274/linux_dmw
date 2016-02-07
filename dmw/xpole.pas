unit xpole; interface {$A1}

uses
  Windows,Classes,Contnrs,
  otypes,xlist,oframes,activeX;

const
  pole_Ok           = 0;
  pole_False        = 1;
  pole_OpenFailed   = 2;
  pole_NotOLE       = 3;
  pole_BadOLE       = 4;
  pole_UnknownError = 5;

type
  pu16 = ^u16;
  u16 = word;

  pu32 = ^u32;
  u32 = uint;

  TStreamIO = class;

  TDIFAT = Array[0..108] of ulong;

  TOleHeader = record
    magic: int64;           // signature, or magic identifier

    byteOrder: uint;

    b_shift: uint;          // bbat->blockSize = 1 << b_shift
    s_shift: uint;          // sbat->blockSize = 1 << s_shift

    num_bat: uint;          // blocks allocated for big bat

    dirent_start: uint;     // starting block for directory info
    num_dirent: uint;       // blocks allocated for directory

    threshold: uint;        // switch from small to big file (usually 4K)

    sbat_start: uint;       // starting block index to store small bat
    num_sbat: uint;         // blocks allocated for small bat

    mbat_start: uint;       // starting block to store meta bat
    num_mbat: uint;         // blocks allocated for meta bat

    DIFAT: TDIFAT;
  end;

  TOleAllocTable = class

    constructor Create;
    destructor Destroy; override;

    procedure clear;
    function count: uint;

    procedure resize(newcount: int);
    procedure preserve(n: int);
    function unused: int;

    function getChain(chain: TIntegerList; start: int): int;
    procedure setChain(chain: TIntegerList);

    function GetByChain(const ffile: IFile;
                        bat: TIntegerList;
                        start: int): int;

    procedure save(bp: PBytes);
    function size: uint;

  private
    data: TIntegerList;
    blockSize: uint;

    function GetValue(index: ulong): ulong;
    procedure SetValue(index,value: ulong);

  public
    property Values[index: ulong]: ulong read GetValue
                                         write SetValue; default;
  end;

  PDirEntry  = ^TDirEntry;
  TDirEntry = record
    valid: bool;          // false if invalid (should be skipped)
    dir: bool;            // true if directory
    size: uint;           // size (not valid if directory)
    start: uint;          // starting block
    prev: uint;           // previous sibling
    next: uint;           // next sibling
    child: uint;          // first child

    state: uint;
    name: TNameStr1;      // the name, not in unicode anymore

    _next: int;
    _child: int;
  end;

  PDirEntries = ^TDirEntries;
  TDirEntries = Array[0..31] of TDirEntry;

  TDirTree = class(TCustomList)
    constructor Create;
    destructor Destroy; override;

    function GetChilds(Index: int): int;
    function GetNext(Index: int): int;

    function seek(Index: int; name: PChar): int;

  private
    flist: TIntegerList;
    fEntries: PDirEntries;

    function load(bp: PBytes; size: uint): uint;
    function entry(name: PChar): PDirEntry;

    procedure debug(log: TLogProc);
    procedure debug1(log: TLogProc; index,lev: int);
  end;

  TStorageIO = class
    constructor Create;
    destructor Destroy; override;

    procedure debug(log,con: TLogProc);

    function open(APath: PChar): bool;
    procedure close;

    function streamIO(name: PChar): TStreamIO;

    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;

  private
    ffile: IFile;
    fResult: int;

    fHeader: TOleHeader;
    fDirTree: TDirTree;
    fbbat: TOleAllocTable;    // for big blocks
    fsbat: TOleAllocTable;    // for small blocks

    fsb_blocks: TIntegerList; // blocks for "small" files

    ftemp: TIntegerList;

    fstreams: TObjectList;

    fPath: TShortstr;
    wbuf: TWideStr;

    procedure loadBigBlock(block: uint; buf: PBytes);
    procedure loadSmallBlock(block: uint; buf,big: PBytes);

  public
    property Header: TOleHeader read fHeader;

    property DirTree: TDirTree read fDirTree;
    property bbat: TOleAllocTable read fbbat;
    property sbat: TOleAllocTable read fsbat;
  end;

  TStreamIO = class(TInterfacedObject,IStream)

    constructor Create(s: TStorageIO;
                       const e: TDirEntry);

    destructor Destroy; override;

    // Reads a byte.
    function __getch: int;

    // Reads a block of data.
    function __read(data: Pointer; maxlen: uint): uint;

    function Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD) : HRESULT;stdcall;
    function Write(pv: Pointer; cb: DWORD; pcbWritten: PDWORD): HRESULT;stdcall;

    function Seek(dlibMove: Largeint; dwOrigin: Longint;
      out libNewPosition: Largeint): HResult; stdcall;
    function SetSize(libNewSize: Largeint): HResult; stdcall;
    function CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
      out cbWritten: Largeint): HResult; stdcall;
    function Commit(grfCommitFlags: Longint): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function UnlockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
      stdcall;
    function Clone(out stm: IStream): HResult; stdcall;
    
  private
    io: TStorageIO;
    entry: TDirEntry;

    fthreshold: uint;
    fsmallBlockSize: uint;
    fbigBlockSize: uint;

    fchain: TIntegerList;
    fblocks: PIntegers;
    fblockN: int;

    fbuffer: PBytes;
    fbuffer1: PBytes;

    m_pos: uint;

    fcache_data: PBytes;
    fcache_capacity: int;
    fcache_size: int;
    fcache_pos: int;

    wbuf: TWideStr;

    procedure __updateCache;

  public
    property Position: uint read m_pos
                            write m_pos;

    property Size: uint read entry.Size;
  end;

  TStorage = class(TInterfacedObject,IStorage)

    function Open(Astg: TStorageIO; AIndex: int): bool;

    function CreateStream(pwcsName: POleStr; grfMode: Longint;
      reserved1,reserved2: Longint; out stm: IStream): HResult; stdcall;
    function OpenStream(pwcsName: POleStr; reserved1: Pointer;
      grfMode: Longint; reserved2: Longint; out stm: IStream): HResult; stdcall;
    function CreateStorage(pwcsName: POleStr; grfMode: Longint;
      reserved1,reserved2: Longint; out stg: IStorage): HResult;
      stdcall;
    function OpenStorage(pwcsName: POleStr; const stgPriority: IStorage;
      grfMode: Longint; snbExclude: TSNB; reserved: Longint;
      out stg: IStorage): HResult; stdcall;
    function CopyTo(ciidExclude: Longint; rgiidExclude: PGUID;
      snbExclude: TSNB; const stgDest: IStorage): HResult; stdcall;
    function MoveElementTo(pwcsName: POleStr; const stgDest: IStorage;
      pwcsNewName: POleStr; grfFlags: Longint): HResult; stdcall;
    function Commit(grfCommitFlags: Longint): HResult; stdcall;
    function Revert: HResult; stdcall;
    function EnumElements(reserved1: Longint; reserved2: Pointer; reserved3: Longint;
      out enm: IEnumStatStg): HResult; stdcall;
    function DestroyElement(pwcsName: POleStr): HResult; stdcall;
    function RenameElement(pwcsOldName: POleStr;
      pwcsNewName: POleStr): HResult; stdcall;
    function SetElementTimes(pwcsName: POleStr; const ctime: TFileTime;
      const atime: TFileTime; const mtime: TFileTime): HResult;
      stdcall;
    function SetClass(const clsid: TCLSID): HResult; stdcall;
    function SetStateBits(grfStateBits: Longint; grfMask: Longint): HResult;
      stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
      stdcall;

  private
    fstg: TStorageIO;
    ftop,findex: int;
  end;

  TFileStorage = class(TStorage)

    constructor Create;
    destructor Destroy; override;

    function OpenFile(Path: PChar): bool;

  private
    fstg: TStorageIO;
  end;

  TEnumStatStg = class(TInterfacedObject,IEnumStatStg)

    constructor Create(ADirTree: TDirTree; ATop: int);

    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumStatStg): HResult; stdcall;

  private
    fDirTree: TDirTree;
    fTop,fIndex: int;
    wbuf: TWideStr
  end;

function PoleGetStorage(Path: PChar; Mode: int; out stg: IStorage): Boolean;

implementation

uses
  Math,Sysutils,
  convert,ofiles,
  wstrings;

const
  at_Avail   = $ffffffff;
  at_Eof     = $fffffffe;
  at_Bat     = $fffffffd;
  at_MetaBat = $fffffffc;

  dir_end    = $ffffffff;

  pole_magic: int64 = $e11ab1a1e011cfd0;

function InitHeader: TOleHeader;
var
  i: int; h: TOleHeader;
begin
  h.magic:=pole_magic;

  h.b_shift:=9;
  h.s_shift:=6;
  h.num_bat:=0;
  h.dirent_start:=0;
  h.threshold:=4096;
  h.sbat_start:=0;
  h.num_sbat:=0;
  h.mbat_start:=0;
  h.num_mbat:=0;

  for i:=0 to 108 do
  h.DIFAT[i]:=at_Avail;

  Result:=h
end;

function verify_Header(const Header: TOleHeader): bool;
begin
  Result:=true;

  with Header do
  if ( threshold <> 4096 )
  or ( byteOrder <> $FFFE)
  or ( num_bat = 0 )
  or ( (num_bat > 109) and (num_bat > (num_mbat * 127) + 109))
  or ( (num_bat < 109) and (num_mbat <> 0) )
  or ( s_shift > b_shift )
  or ( b_shift <= 6 )
  or ( b_shift >= 31 ) then Result:=false
end;

type
  PCFHeader = ^TCFHeader;
  TCFHeader = record
    magic: Int64;
    clsid: TCLSID;
    minorVer: word;
    majorVer: word;
    byteOrder: word;
    SectorSize: word;
    MiniSectorSize: word;
    Reserved1: word;
    Reserved2: uint;
    NumberOfDirSectors: uint;
    NumberOfFATSectors: uint;
    FirstDirSector: uint;
    TransactionNumber: uint;
    threshold: uint;
    FirstMiniFatSector: uint;
    NumberOfMiniFatSectors: uint;
    FirstDIFATSector: uint;
    NumberOfDIFATSectors: uint;
    DIFAT: TDIFAT;
  end;

  PCFDirEntry = ^TCFDirEntry;
  TCFDirEntry = record
    Name: Array[0..31] of WideChar;
    NameLen: Word;
    typ: byte;
    color: byte;
    left: uint;
    right: uint;
    child: uint;
    clsid: TCLSID;
    state: uint;
    createTime: int64;
    modifiedTime: int64;
    startSector: uint;
    size: int64;
  end;

function load_Header(var Header: TOleHeader; pc: PCFHeader): int;
begin
  Result:=pole_False;

  with Header do
  if Assigned(pc) then begin
    magic:=pc.magic;

    byteOrder:=pc.byteOrder;

    b_shift:=pc.SectorSize;
    s_shift:=pc.MiniSectorSize;

    num_bat:=pc.NumberOfFATSectors;

    dirent_start:=pc.FirstDirSector;
    num_dirent:=pc.NumberOfDirSectors;

    threshold:=pc.threshold;

    sbat_start:=pc.FirstMiniFatSector;
    num_sbat:=pc.NumberOfMiniFatSectors;

    mbat_start:=pc.FirstDIFATSector;
    num_mbat:=pc.NumberOfDIFATSectors;

    DIFAT:=pc.DIFAT;

    Result:=pole_NotOLE;

    if magic = pole_magic then begin
      Result:=pole_BadOLE;
      if verify_Header(Header) then
      Result:=pole_OK
    end
  end
end;

procedure debug_Header(const Header: TOleHeader; log: TLogProc);
var
  i,s: int;
begin
  with Header do begin
    log('');
    log('b_shift '+IntToStr(b_shift));
    log('s_shift '+IntToStr(s_shift));
    log('num_bat '+IntToStr(num_bat));
    log('dirent_start '+IntToStr(dirent_start));
    log('threshold '+IntToStr(threshold));
    log('sbat_start '+IntToStr(sbat_start));
    log('num_sbat '+IntToStr(num_sbat));
    log('mbat_start '+IntToStr(mbat_start));
    log('num_mbat '+IntToStr(num_mbat));

    s:=Min(109,num_bat);
    log('difat '+IntToStr(s));
    for i:=0 to s-1 do
    log('  '+IntToStr(DIFAT[i]));
  end
end;

constructor TDirTree.Create;
begin
  inherited Create(Sizeof(TDirEntry),256);
  flist:=TIntegerList.Create;
end;

destructor TDirTree.Destroy;
begin
  flist.Clear;
  inherited
end;

function TDirTree.GetChilds(Index: int): int;
begin
  Result:=-1;
  if Index >= 0 then
  if Index < Count then
  if fEntries[index].valid then
  if fEntries[index].dir then
  Result:=fEntries[index]._child
end;

function TDirTree.GetNext(Index: int): int;
begin
  Result:=-1;
  if Index >= 0 then
  if Index < Count then
  if fEntries[index].valid then
  Result:=fEntries[index]._next
end;

function TDirTree.seek(Index: int; name: PChar): int;
var
  i: int; e: TDirEntry;
begin
  Result:=-1;

  for i:=1 to 1024 do begin
    if index < 0 then Break;
    if index >= Count then Break;

    e:=fEntries[index];

    if e.valid then
    if StrIComp(e.name,name) = 0 then begin
      Result:=index; Break
    end;

    e:=fEntries[index];
    index:=e._next
  end
end;

function TDirTree.load(bp: PBytes; size: uint): uint;

// recursively fill [_next,_child]
procedure fill_tree(index,parent,left,right: int);
var
  e: TDirEntry;
begin
  if (index > 0) and (index < Count) then begin
    e:=fEntries[index];
    if e.valid then begin

      if left < 0 then
        fEntries[parent]._child:=index
      else begin
        e._next:=fEntries[left]._next;
        fEntries[left]._next:=index;
      end;

      if right > 0 then e._next:=right;
      fEntries[index]:=e;

      if e.child >= 0 then
      fill_tree(e.child,index,-1,-1);

      fill_tree(e.prev,parent,left,index);
      fill_tree(e.next,-1,index,-1)
    end
  end
end;

var
  pc: PCFDirEntry; ws: WideString;
  i,j,k,n,typ: int; wp: PWideChar;
  ep: PDirEntries; e: TDirEntry;
begin
  Result:=0; Clear;

  n:=size div 128;
  for i:=0 to n-1 do begin

    Fillchar(e,Sizeof(e),0);
    e.valid:=true;

    pc:=Pointer(bp);

    // parse name of this entry, which stored as Unicode 16-bit
    k:=pc.NameLen;
    if k > 64 then k:=64;

    if k < 1 then e.valid:=false;

    wp:=pc.Name; ws:='';
    for j:=0 to k-1 do ws:=ws+wp[j];
    StrPCopy(e.name,ws);

    // first char isn't printable ? remove it...
    if e.name[0] < ' ' then begin
      Move(e.name[1],e.name[0],Sizeof(e.name)-1);
      e.name[Sizeof(e.name)-1]:=#0
    end;

    // 2 = file (aka stream), 1 = directory (aka storage), 5 = root
    typ:=pc.typ;

    e.start:=pc.startSector;
    e.size :=pc.size;
    e.prev :=pc.left;
    e.next :=pc.right;
    e.child:=pc.child;
    e.dir:=typ <> 2;

    e.state:=pc.state;

    // sanity checks
    if (typ <> 2) and (typ <> 1 ) and (typ <> 5 ) then e.valid:=false;

    e._next:=-1;
    e._child:=-1;

    Add(@e); bp:=@bp[128]
  end;

  ep:=First;
  if ep[0].valid then
  for i:=0 to Count-1 do
  if ep[i].valid then Inc(Result);

  fEntries:=ep;
  if Count > 0 then
  if ep[0].valid then
  fill_tree(ep[0].child,0,-1,-1);

  Result:=Count
end;

procedure TDirTree.debug(log: TLogProc);
var
  i,n: int; e: TDirEntry; s: String;
begin
  n:=Count;

  while (n > 0) and not fEntries[n-1].valid do Dec(n);

  log('');
  log('dir '+IntToStr(n));

  for i:=0 to n-1 do begin
    e:=fEntries[i]; s:=IntTostr(i)+': ';

    if not e.valid then s:=s+'INVALID ';
    s:=s+StrLPas(e.name,Sizeof(e.name))+' ';

    if e.dir then s:=s+'(Dir) '
             else s:=s+'(File) ';

    s:=s+Format('size:%d ',[e.size]);
    s:=s+Format('start:%d ',[e.start]);

    s:=s+'(';
    if e.child = dir_end then s:=s+'-'
                          else s:=s+IntToStr(e.child);

    s:=s+' ';
    if e.prev = dir_end then s:=s+'-'
                        else s:=s+IntToStr(e.prev);

    s:=s+':';
    if e.next = dir_end then s:=s+'-'
                        else s:=s+IntToStr(e.next);

    s:=s+')';

    s:=s+Format(' [%d,%d]',[e._next,e._child]);

    log(s)
  end
end;

procedure TDirTree.debug1(log: TLogProc; index,lev: int);
var
  i: int; e: TDirEntry; s: String;
begin
  if (index >= 0) and (index < Count) then begin
    e:=fEntries[index];

    s:='';
    for i:=1 to lev do s:=s+'  ';

    if not e.valid then s:=s+'INVALID ';
    s:=s+StrLPas(e.name,Sizeof(e.name))+' ';

    if e.dir then s:=s+'(Dir) '
             else s:=s+'(File) ';

    s:=s+Format('size:%d ',[e.size]);
    log(s);

    if e._child >= 0 then
    debug1(log,e._child,lev+1);

    if e._next >= 0 then
    debug1(log,e._next,lev)
  end
end;

function TDirTree.entry(name: PChar): PDirEntry;
var
  ep: PDirEntries;
  index: int; p1,p2: PChar;
  e: TDirEntry; s: TShortstr;
begin
  Result:=nil;

  // start from root
  if Count > 0 then

  if name = nil then
    Result:=@fEntries[0]
  else begin
    index:=0; ep:=fEntries;

    p1:=StrCopy(s,name);
    while p1 <> nil do begin
      p2:=StrScan(p1,'/');
      if Assigned(p2) then begin
        p2[0]:=#0; p2:=@p2[1]
      end;

      if p1[0] = #0 then Break;

      if index < 0 then Break;

      e:=ep[index];
      if not e.valid then Break;

      index:=e._child;
      while index > 0 do begin

        e:=ep[index];
        if e.valid then
        if StrIComp(e.name,p1) = 0 then Break;

        index:=e._next
      end;

      // traverse to the child
      if index <= 0 then
        Break
      else
      if p2 = nil then begin
        Result:=@ep[index];
        Break
      end;

      p1:=p2
    end
  end
end;

constructor TOleAllocTable.Create;
begin
  inherited Create;
  data:=TIntegerList.Create;
  blockSize:=4096;
  resize(128)
end;

destructor TOleAllocTable.Destroy;
begin
  data.Free; inherited
end;

function TOleAllocTable.GetValue(index: ulong): ulong;
begin
  Result:=data[index]
end;

procedure TOleAllocTable.SetValue(index,value: ulong);
begin
  if index >= data.Count then resize(index + 1);
  data[index]:=value
end;

procedure TOleAllocTable.clear;
begin
  data.Clear
end;

function TOleAllocTable.count: uint;
begin
  Result:=data.Count
end;

procedure TOleAllocTable.resize(newcount: int);
var
  i,oldcount: int; ip: PIntegers;
begin
  oldcount:=data.Count;
  data.Extend(newcount);

  if data.Count > oldcount then begin

    ip:=data.First;
    for i:=oldcount to data.Count-1 do
    ip[i]:=at_AVail
  end
end;

// make sure there're still free blocks
procedure TOleAllocTable.preserve(n: int);
begin
  while n > 0 do begin
    unused; Dec(n)
  end
end;

function TOleAllocTable.unused: int;
var
  i,n: int; ip: PIntegers;
begin
  Result:=-1;

  n:=data.Count;
  ip:=data.First;

  // find first available block
  for i:=0 to n-1 do
  if ip[i] = at_Avail then begin
    Result:=i; Break
  end;

  if Result < 0 then begin
    Result:=n; resize(n+10)
  end
end;

procedure TOleAllocTable.setChain(chain: TIntegerList);
var
  i,n: int; ip: PIntegers;
begin
  n:=chain.Count;
  if n > 0 then begin
    ip:=chain.First;

    for i:=0 to n-2 do
    Values[ip[i]]:=ip[i+1];

    Values[n-1]:=at_Eof;
  end
end;

function TOleAllocTable.getChain(chain: TIntegerList; start: int): int;
var
  p,n: uint; ip: PIntegers;
begin
  chain.Clear;

  ip:=data.First;
  n:=data.Count;

  if start < n then begin
    p:=start;
    while p < n do begin
      if (p = at_Eof)
      or (p = at_Bat)
      or (p = at_MetaBat) then break;

      chain.AddItem(p);
      p:=ip[p];
    end
  end;

  Result:=chain.Count
end;

function TOleAllocTable.GetByChain(const ffile: IFile;
                                   bat: TIntegerList;
                                   start: int): int;
var
  p,n,bx,cx: uint; ip,bp: PIntegers;
begin
  Result:=0;

  ip:=data.First;
  n:=data.Count;
  bx:=BlockSize;
  cx:=bx div 4;

  if start < n then begin
    p:=start;
    while p < n do begin
      if (p = at_Eof)
      or (p = at_Bat)
      or (p = at_MetaBat) then break;

      bp:=ffile.Seek( bx * (p+1),bx );

      if Assigned(bp) then begin
        bat.Insert_range(bp,-1,cx);
        Inc(Result)
      end;

      p:=ip[p]
    end
  end
end;

procedure TOleAllocTable.save(bp: PBytes);
var
  n: int;
begin
  n:=data.Count; if n > 0 then
  Move(data.First^,bp^,n*4)
end;

function TOleAllocTable.size: uint;
begin
  Result:=data.Count * 4
end;

constructor TStorageIO.Create;
begin
  inherited Create;

  fDirTree:=TDirTree.Create;
  fbbat:=TOleAllocTable.Create;
  fsbat:=TOleAllocTable.Create;

  fsb_blocks:=TIntegerList.Create;
  fstreams:=TObjectList.Create;

  ftemp:=TIntegerList.Create;

  fHeader:=InitHeader;

  fbbat.blockSize:=1 shl fHeader.b_shift;
  fsbat.blockSize:=1 shl fHeader.s_shift
end;

destructor TStorageIO.Destroy;
begin
  close;

  ftemp.Free;

  fstreams.Free;
  fsb_blocks.Free;
  fsbat.Free;
  fbbat.Free;
  fDirTree.Free;

  inherited
end;

procedure TStorageIO.close;
begin
  ffile:=nil;
  fbbat.clear;
  fsbat.clear;
  fsb_blocks.Clear;
  fDirTree.Clear;
  fstreams.Clear;
  StrCopy(fPath,'')
end;

procedure TStorageIO.debug(log,con: TLogProc);

procedure debug_bat(bat: TIntegerList;
                    log: TLogProc; capt: PChar);
var
  i,n: int; p: uint; ip: PIntegers; s: String;
begin
  log('');

  n:=bat.Count;
  log(Format('%s count %d',[capt,n]));

  ip:=bat.First;
  for i:=0 to n-1 do begin
    p:=ip[i];
    if p = at_Avail then continue;

    s:=IntToStr(i)+': ';

    if p = at_Eof then s:=s+'[eof]' else
    if p = at_Bat then s:=s+'[bat]' else
    if p = at_MetaBat then s:=s+'[metabat]'
    else s:=s+IntTostr(p);

    log(s)
  end
end;

begin
  if Assigned(log) then begin
    debug_Header(fHeader,log);
    debug_bat(fbbat.data, log,'bbat');
    debug_bat(fsbat.data,log,'sbat');
    debug_bat(fsb_blocks,log,'sb_blocks');
    fDirTree.debug(log);
    fDirTree.debug1(log,0,0)
  end;

  if Assigned(con) then begin
    fDirTree.debug(con);
    fDirTree.debug1(con,0,0)
  end
end;

function TStorageIO.open(APath: PChar): bool;

procedure bat_difat(bat: TIntegerList; ip: PIntegers; n: int);
var
  bp: PIntegers; i,bx,cx: int;
begin
  bx:=fbbat.blockSize;
  cx:=bx div 4;

  for i:=0 to n-1 do begin
    bp:=ffile.Seek( bx * (ip[i]+1),bx );

    if Assigned(bp) then
    bat.Insert_range(bp,-1,cx)
  end;

  bp:=bp
end;

function bat_difat2(bat: TIntegerList;
                    start,num_mbat,num_bat: int): int;
var
  bp: PIntegers; i,k,p,bx,cx,dx: uint;
begin
  bx:=fbbat.blockSize;
  cx:=bx div 4;

  if num_mbat > 0 then
  if ftemp.Extend(cx) then begin

    bp:=ftemp.First;

    p:=start;
    for i:=1 to num_mbat do begin
      if num_bat <= 0 then Break;

      dx:=(p+1)*bx;
      if ffile.IsData(dx,bx) then begin

        ffile.Load(dx,bp^,bx);

        k:=cx-1;
        if k > num_bat then k:=num_bat;
        Dec(num_bat,k);

        bat_difat(bat,bp,k);

        p:=bp[cx-1];
        if p = at_Eof then Break;
      end
    end
  end;

  ftemp.Clear;

  Result:=pole_False;
  if num_bat <= 0 then
  Result:=pole_Ok
end;

var
  ds: Pointer;
  k,num_bat,num_mbat: int;
  root: PDirEntry;
begin
  Result:=false; close;

  if GetOpenFileIntf(APath,ffile) then begin

    StrCopy(fPath,APath);

    ds:=ffile.Seek(0,512);
    fResult:=load_Header(fHeader,ds);

    if fResult = pole_Ok then begin

      // important block size
      fbbat.blockSize:=1 shl fHeader.b_shift;
      fsbat.blockSize:=1 shl fHeader.s_shift;

      num_bat:=fHeader.num_bat;
      num_mbat:=fHeader.num_mbat;

      k:=Min(109,num_bat);
      bat_difat(fbbat.data,@fHeader.DIFAT,k);

      // find blocks allocated to store big bat
      // the first 109 blocks are in header, the rest in meta bat
      if (num_bat > 109) and (num_mbat > 0) then
      fResult:=bat_difat2(fbbat.data,
                          fHeader.mbat_start,
                          num_mbat,num_bat-109);

      if fResult = pole_Ok then begin fResult:=pole_False;

        fbbat.GetByChain(ffile,
                         fsbat.data,
                         fHeader.sbat_start);

        // load directory tree
        fbbat.GetByChain(ffile,ftemp,
                         fHeader.dirent_start);

        fDirTree.load(ftemp.First,ftemp.BufferSize);
        root:=fDirTree.First;

        if Assigned(root) then begin
          // fetch block chain as data for small-files
          fbbat.getChain(fsb_blocks,root.start);

          fResult:=pole_Ok;
          Result:=true
        end
      end
    end
  end;

  if not Result then close
end;

function TStorageIO.streamIO(name: PChar): TStreamIO;
var
  e: PDirEntry; stm: TStreamIO;
begin
  Result:=nil;

  if Strlen(name) > 0 then begin
    e:=fDirTree.entry(name);
    if Assigned(e) then
    if not e.dir then begin
      stm:=TStreamIO.Create(Self,e^);
      Result:=stm
    end
  end
end;

function TStorageIO.Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
var
  e: PDirEntry; ws: WideString;
begin
  Result:=S_FALSE;

  FillChar(statstg,SizeOf(statstg),0);

  if Assigned(fFile) then begin

    e:=fDirTree.entry(nil);
    if Assigned(e) then
    if e.dir then begin

      statstg.grfStateBits:=e.state;
      statstg.cbSize:=fFile.GetSize;

      ws:=Strpas(e.name);
      StrPCopyW(wbuf,ws,127);
      if wbuf[0] <> #0 then
      statstg.pwcsName:=wbuf;

      Result:=S_OK
    end
  end
end;

procedure TStorageIO.loadBigBlock(block: uint; buf: PBytes);
var
  cx: uint;
begin
  cx:=bbat.blockSize;
  ffile.Load(cx * (block+1),buf^,cx)
end;

procedure TStorageIO.loadSmallBlock(block: uint; buf,big: PBytes);
var
  pos,sbsize,bbsize,bbindex,bx,cx: uint;
begin
  sbsize:=sbat.blockSize;
  bbsize:=bbat.blockSize;

  // find where the small-block exactly is
  pos:=block * sbsize;
  bbindex:=pos div bbsize;

  if bbindex < fsb_blocks.Count then begin

    loadBigBlock(fsb_blocks[bbindex],big);

    // copy the data
    bx:=pos mod bbsize; cx:=sbsize;
    Move(big[bx],buf[0],sbsize)
  end
end;

constructor TStreamIO.Create(s: TStorageIO;
                             const e: TDirEntry);
begin
  inherited Create;

  fchain:=TIntegerList.Create;

  io:=s;
  entry:=e;

  fthreshold:=io.Header.threshold;
  fbigBlockSize:=io.bbat.blockSize;
  fsmallBlockSize:=0;

  m_pos:=0;

  if entry.size >= fthreshold then
    io.bbat.getChain(fchain,entry.start)
  else begin
    io.sbat.getChain(fchain,entry.start);
    fsmallBlockSize:=io.sbat.blockSize;
  end;

  fblocks:=fchain.First;
  fblockN:=fchain.Count;

  fcache_pos:=0;
  fcache_capacity:=0;
  fcache_size:=0;

  fbuffer:=xAllocPtr(fbigBlockSize +
                     fsmallBlockSize);

  if Assigned(fbuffer) then
  fbuffer1:=@fbuffer[fbigBlockSize];
end;

destructor TStreamIO.Destroy;
begin
  xFreePtr(fcache_data);
  xFreePtr(fbuffer);
  fchain.Free;
  inherited
end;

procedure TStreamIO.__updateCache;
var
  bx,cx,dx: uint;
begin
  if fcache_data = nil then
  if fcache_capacity = 0 then begin
    fcache_capacity:=4096;
    fcache_data:=xAllocPtr(fcache_capacity);
  end;

  if Assigned(fcache_data) then begin

    dx:=fcache_capacity;
    bx:=m_pos - (m_pos mod dx);
    cx:=dx;

    if bx + cx > Size then cx:=Size - bx;

    fcache_pos:=bx;
    fcache_size:=__read(fcache_data,cx);
  end
end;

function TStreamIO.__getch: int;
var
  ax: int;
begin
  ax:=-1;

  if m_pos <= Size then begin
    if (fcache_size = 0)
    or (m_pos < fcache_pos)
    or (m_pos >= fcache_pos + fcache_size) then
    __updateCache;

    if fcache_size > 0 then
    ax:=fcache_data[m_pos - fcache_pos];

    Inc(m_pos)
  end;

  Result:=ax
end;

function TStreamIO.__read(data: Pointer; maxlen: uint): uint;
var
  bytes,index,pos,bx,cx,dx: uint; si,di: PBytes;
begin
  bytes:=0; pos:=m_pos; di:=data;

  if Assigned(data) then
  if maxlen > 0 then

  if entry.size < fthreshold then begin // small file

    dx:=fsmallBlockSize;
    index:=pos div dx;

    if index < fblockN then begin

      si:=fbuffer1; bx:=pos mod dx;

      while maxlen > 0 do begin

        if index >= fblockN then break;

        cx:=dx - bx;
        if cx > maxlen then cx:=maxlen;

        if cx > 0 then begin
          if (bx = 0) and (cx = dx) then
            io.loadSmallBlock(fblocks[index], di,fbuffer)
          else begin
            io.loadSmallBlock(fblocks[index], si,fbuffer);
            move(si[bx],di[0],cx);
          end;

          Inc(bytes,cx);
          Dec(maxlen,cx);
          di:=@di[cx]
        end;

        bx:=0; Inc(index)
      end
    end
  end
  else begin // big file

    dx:=fbigBlockSize;
    index:=pos div dx;
    if index < fblockN then begin

      si:=fbuffer; bx:=pos mod dx;

      while maxlen > 0 do begin

        if index >= fblockN then break;

        cx:=dx - bx;
        if cx > maxlen then cx:=maxlen;

        if cx > 0 then begin
          if (bx = 0) and (cx = dx) then
            io.loadBigBlock(fblocks[index], di)
          else begin
            io.loadBigBlock(fblocks[index], si);
            move(si[bx],di[0],cx);
          end;

          Inc(bytes,cx);
          Dec(maxlen,cx);
          di:=@di[cx]
        end;

        bx:=0; Inc(index)
      end
    end
  end;

  Inc(m_pos,bytes);
  Result:=bytes
end;

function TStreamIO.Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HResult;
var
  cx: uint;
begin
  Result:=S_OK; cx:=__read(pv,cb);
  if Assigned(pcbRead) then pcbRead^:=cx;
end;

function TStreamIO.Write(pv: Pointer; cb: DWORD; pcbWritten: PDWORD): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStreamIO.Seek(dlibMove: Largeint; dwOrigin: Longint;
  out libNewPosition: Largeint): HResult;
begin

  case dwOrigin of
0:  m_pos:=dlibMove;
1:  Inc(m_pos,dlibMove);
2:  m_pos:=Size+dlibMove;
  end;

  libNewPosition:=m_pos;
  Result:=S_OK
end;

function TStreamIO.SetSize(libNewSize: Largeint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStreamIO.CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
  out cbWritten: Largeint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStreamIO.Commit(grfCommitFlags: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStreamIO.Revert: HResult;
begin
  Result:=E_NOTIMPL
end;

function TStreamIO.LockRegion(libOffset: Largeint; cb: Largeint;
  dwLockType: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStreamIO.UnlockRegion(libOffset: Largeint; cb: Largeint;
  dwLockType: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStreamIO.Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
var
  ws: WideString;
begin
  FillChar(statstg,SizeOf(statstg),0);

  statstg.grfStateBits:=entry.state;
  statstg.cbSize:=Size;

  ws:=Strpas(entry.name);
  StrPCopyW(wbuf,ws,127);
  if wbuf[0] <> #0 then
  statstg.pwcsName:=wbuf;

  Result:=S_OK
end;

function TStreamIO.Clone(out stm: IStream): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStorage.Open(Astg: TStorageIO; AIndex: int): bool;
begin
  fstg:=Astg;
  ftop:=fstg.fDirTree.GetChilds(AIndex);
  findex:=ftop;

  Result:=ftop >= 0
end;

function TStorage.CreateStream(pwcsName: POleStr; grfMode: Longint;
  reserved1: longint; reserved2: Longint; out stm: IStream): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStorage.OpenStream(pwcsName: POleStr; reserved1: Pointer;
  grfMode: Longint; reserved2: Longint; out stm: IStream): HResult;
var
  obj: TStreamIO;
  ind: int; e: PDirEntry;
  fn: TShortstr;
begin
  Result:=S_FALSE; stm:=nil;

  if grfMode and 15 = STGM_READ then begin

    StrPLCopy(fn,WideCharToString(pwcsName),255);

    if fn[0] <> #0 then begin

      ind:=fstg.DirTree.seek(ftop,fn);
      e:=fstg.DirTree[ind];

      if Assigned(e) then
      if not e.dir then begin
        obj:=TStreamIO.Create(fstg,e^);
        try
          if obj.GetInterface(IStream,stm) then begin
            obj:=nil; Result:=S_OK;
          end
        finally
          obj.Free
        end
      end
    end
  end
end;

function TStorage.CreateStorage(pwcsName: POleStr; grfMode: Longint;
  reserved1,reserved2: Longint; out stg: IStorage): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStorage.OpenStorage(pwcsName: POleStr; const stgPriority: IStorage;
  grfMode: Longint; snbExclude: TSNB; reserved: Longint;
  out stg: IStorage): HResult;
var
  obj: TStorage;
  ind: int; e: PDirEntry;
  fn: TShortstr;
begin
  Result:=S_FALSE; stg:=nil;

  if grfMode and 15 = STGM_READ then begin

    StrPLCopy(fn,WideCharToString(pwcsName),255);

    if fn[0] <> #0 then begin

      ind:=fstg.DirTree.seek(ftop,fn);
      e:=fstg.DirTree[ind];

      if Assigned(e) then
      if e.dir then begin
        obj:=TStorage.Create;
        try
          if obj.Open(fstg,ind) then
          if obj.GetInterface(IStorage,stg) then begin
            obj:=nil; Result:=S_OK;
          end
        finally
          obj.Free
        end
      end
    end
  end
end;

function TStorage.CopyTo(ciidExclude: Longint; rgiidExclude: PGUID;
  snbExclude: TSNB; const stgDest: IStorage): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStorage.MoveElementTo(pwcsName: POleStr; const stgDest: IStorage;
  pwcsNewName: POleStr; grfFlags: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStorage.Commit(grfCommitFlags: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStorage.Revert: HResult;
begin
  Result:=E_NOTIMPL
end;

function TStorage.EnumElements(reserved1: Longint; reserved2: Pointer; reserved3: Longint;
  out enm: IEnumStatStg): HResult;
var
  obj: TEnumStatStg;
begin
  Result:=S_FALSE; TPointer(enm):=0;

  obj:=TEnumStatStg.Create(fstg.DirTree,ftop);
  try
    if obj.GetInterface(IEnumStatStg,enm) then begin
      Result:=S_OK; obj:=nil
    end;
  finally
    obj.Free
  end
end;

function TStorage.DestroyElement(pwcsName: POleStr): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStorage.RenameElement(pwcsOldName: POleStr;
  pwcsNewName: POleStr): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStorage.SetElementTimes(pwcsName: POleStr; const ctime: TFileTime;
  const atime: TFileTime; const mtime: TFileTime): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStorage.SetClass(const clsid: TCLSID): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStorage.SetStateBits(grfStateBits: Longint; grfMask: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TStorage.Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
begin
  Result:=fstg.Stat(statstg,grfStatFlag);
end;

constructor TFileStorage.Create;
begin
  inherited;
  fstg:=TStorageIO.Create;
end;

destructor TFileStorage.Destroy;
begin
  fstg.Free; inherited
end;

function TFileStorage.OpenFile(Path: PChar): bool;
begin
  Result:=false; fstg.close;
  if fstg.open(Path) then
  Result:=Open(fstg,0)
end;

constructor TEnumStatStg.Create(ADirTree: TDirTree; ATop: int);
begin
  inherited Create;
  fDirTree:=ADirTree;
  ftop:=Atop; findex:=ftop;
end;

function TEnumStatStg.Next(celt: Longint; out elt;
                           pceltFetched: PLongint): HResult;
var
  e: PDirEntry; stat: TStatStg; cb: int; ws: WideString;
begin
  Result:=S_FALSE;
  if findex >= 0 then begin
    e:=fDirTree[findex];
    if Assigned(e) then begin
      Fillchar(stat,Sizeof(stat),0);

      cb:=Strlen(e.name);
      if cb > 0 then begin
        ws:=Strpas(e.name);

        if Length(ws) = cb then begin
          Move(ws[1],wbuf,cb*2); wbuf[cb]:=#0;
          stat.pwcsName:=wbuf
        end
      end;

      if e.dir then
        stat.dwType:=stgty_STORAGE
      else
        stat.dwType:=stgty_STREAM;

      Move(stat,elt,Sizeof(stat));
      if Assigned(pceltFetched) then
      pceltFetched^:=Sizeof(stat);

      Result:=S_OK
    end;

    findex:=fDirTree.GetNext(findex)
  end
end;

function TEnumStatStg.Skip(celt: Longint): HResult; stdcall;
begin
  Result:=E_NOTIMPL
end;

function TEnumStatStg.Reset: HResult;
begin
  findex:=ftop; Result:=S_OK
end;

function TEnumStatStg.Clone(out enm: IEnumStatStg): HResult;
begin
  Result:=E_NOTIMPL
end;

function PoleGetStorage(Path: PChar; Mode: int; out stg: IStorage): Boolean;
var
  obj: TFileStorage;
begin
  Result:=false; stg:=nil;

  if Mode and 15 = STGM_READ then
  if FileExist(Path) then begin

    obj:=TFileStorage.Create;
    try
      if obj.OpenFile(Path) then
      if obj.GetInterface(IStorage,stg) then begin
        obj:=nil; Result:=true
      end
    finally
      obj.Free
    end
  end
end;

end.
