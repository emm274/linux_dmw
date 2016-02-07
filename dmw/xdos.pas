unit xdos; interface

uses
  Windows,activeX,
  otypes,convert,ofiles;

type
  TDosStorage = class(TInterfacedObject,IStorage)

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
  end;

  TOnlyRead = class(TInterfacedObject,IStream)

    constructor Create(fh: longint);
    destructor Destroy; override;

    function Active: Boolean;

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
    vm: TFileMemory;
    vm_p: PBytes;
    vm_Size: Integer;
    vm_Pos: Integer;
  end;

  TOnlyFile = class(TInterfacedObject,IStream)

    constructor Create(h: Integer; rw: Boolean);
    destructor Destroy; override;

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
    vm_h: Longint;
    vm_rw: bool;
  end;

  TCashFile = class(TInterfacedObject,IStream)

    constructor Create(h: Integer; rw: Boolean);
    destructor Destroy; override;

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
    vm_h: Longint;
    vm_rw: bool;

    vm_Pos: Int64;
    vm_Size: Int64;

    vm_Page: pbytes;
    pg_Pos: Int64;
    pg_Put: Boolean;

    function Get_Page: Integer;
    procedure Put_Page;
  end;

  TMemoryStorage = class(TInterfacedObject,IStorage)

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
  end;

  TOnlyMemory = class(TInterfacedObject,IStream)

    constructor Create;
    destructor Destroy; override;

    function Active: Boolean;

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
    vm_buf: PBytes;
    vm_Capacity: Integer;

    vm_Size: Integer;
    vm_Pos: Integer;

    function Resize(NewSize: Integer): Boolean;
  end;

var
  DosStorage: TDosStorage;

implementation

uses
  SysUtils,Math,xlog;

const
  pg_Size = 4096 * 4;

function TDosStorage.CreateStream(pwcsName: POleStr; grfMode: Longint;
  reserved1: longint; reserved2: Longint; out stm: IStream): HResult;
var
  h: integer;
begin
  Result:=S_FALSE;

  h:=FileCreate(WideCharToString(pwcsName));

  if h > 0 then
  with TOnlyFile.Create(h,true) do
  Result:=QueryInterface(IStream,stm)

end;

function TDosStorage.OpenStream(pwcsName: POleStr; reserved1: Pointer;
  grfMode: Longint; reserved2: Longint; out stm: IStream): HResult;
var
  fh: longint; fn: TShortStr;
begin
  Result:=S_FALSE; stm:=nil;

  StrPLCopy(fn,WideCharToString(pwcsName),255);
  if FileExist(fn) then begin

    if grfMode and STGM_READWRITE <> 0 then
      grfMode:=fmOpenReadWrite
    else
      grfMode:=fmOpenRead + fmShareDenyNone;

    fh:=FileOpen(StrPas(fn),grfMode);

    if fh <= 0 then
      std_err.Write_int('DosStorage.OpenStream: error=',GetLastOSError)
    else

    if fh > 0 then

    if grfMode and 15 = fmOpenRead then begin

      with TOnlyRead.Create(fh) do
      Result:=QueryInterface(IStream,stm)

    end
    else begin

      with TOnlyFile.Create(fh,true) do
      Result:=QueryInterface(IStream,stm);

   end
  end
end;

function TDosStorage.CreateStorage(pwcsName: POleStr; grfMode: Longint;
  reserved1,reserved2: Longint; out stg: IStorage): HResult;
begin
  Result:=E_NOTIMPL
end;

function TDosStorage.OpenStorage(pwcsName: POleStr; const stgPriority: IStorage;
  grfMode: Longint; snbExclude: TSNB; reserved: Longint;
  out stg: IStorage): HResult;
begin
  Result:=E_NOTIMPL
end;

function TDosStorage.CopyTo(ciidExclude: Longint; rgiidExclude: PGUID;
  snbExclude: TSNB; const stgDest: IStorage): HResult;
begin
  Result:=E_NOTIMPL
end;

function TDosStorage.MoveElementTo(pwcsName: POleStr; const stgDest: IStorage;
  pwcsNewName: POleStr; grfFlags: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TDosStorage.Commit(grfCommitFlags: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TDosStorage.Revert: HResult;
begin
  Result:=E_NOTIMPL
end;

function TDosStorage.EnumElements(reserved1: Longint; reserved2: Pointer; reserved3: Longint;
  out enm: IEnumStatStg): HResult;
begin
  Result:=E_NOTIMPL
end;

function TDosStorage.DestroyElement(pwcsName: POleStr): HResult;
var
  fn: TShortStr;
begin
  Result:=S_FALSE;
  StrPLCopy(fn,WideCharToString(pwcsName),255);
  if FileErase(fn) then Result:=S_OK
end;

function TDosStorage.RenameElement(pwcsOldName: POleStr;
  pwcsNewName: POleStr): HResult;
begin
  Result:=E_NOTIMPL
end;

function TDosStorage.SetElementTimes(pwcsName: POleStr; const ctime: TFileTime;
  const atime: TFileTime; const mtime: TFileTime): HResult;
begin
  Result:=E_NOTIMPL
end;

function TDosStorage.SetClass(const clsid: TCLSID): HResult;
begin
  Result:=E_NOTIMPL
end;

function TDosStorage.SetStateBits(grfStateBits: Longint; grfMask: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TDosStorage.Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

constructor TOnlyRead.Create(fh: longint);
begin
  inherited Create;

  if fh > 0 then begin
    vm:=FileMemoryReadH(fh);

    vm_p:=vm.ptr;
    vm_size:=vm.size;
    vm_pos:=0;

    FileClose(fh)
  end
end;

destructor TOnlyRead.Destroy;
begin
  FileMemoryFree(vm);
  inherited
end;

function TOnlyRead.Active: Boolean;
begin
  Result:=Assigned(vm_p)
end;

function TOnlyRead.Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HResult;
begin
  cb:=Min(cb,vm_Size-vm_Pos);
  Move(vm_p[vm_Pos],pv^,cb);
  vm_Pos:=vm_Pos+cb; pcbRead^:=cb;
  Result:=S_OK
end;

function TOnlyRead.Write(pv: Pointer; cb: DWORD; pcbWritten: PDWORD): HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyRead.Seek(dlibMove: Largeint; dwOrigin: Longint;
  out libNewPosition: Largeint): HResult;
var
  pos: Largeint;
begin
  Result:=S_FALSE;
  pos:=vm_Pos;

  case dwOrigin of
0:  pos:=dlibMove;
1:  pos:=pos+dlibMove;
2:  pos:=vm_Size+dlibMove;
  end;

  if pos >= 0 then
  if pos <= vm_Size then begin
    vm_Pos:=pos; Result:=S_OK
  end;

  libNewPosition:=vm_Pos
end;

function TOnlyRead.SetSize(libNewSize: Largeint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyRead.CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
  out cbWritten: Largeint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyRead.Commit(grfCommitFlags: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyRead.Revert: HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyRead.LockRegion(libOffset: Largeint; cb: Largeint;
  dwLockType: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyRead.UnlockRegion(libOffset: Largeint; cb: Largeint;
  dwLockType: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyRead.Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
begin
  FillChar(statstg,SizeOf(statstg),0);
  statstg.cbSize:=vm_Size;
  Result:=S_OK
end;

function TOnlyRead.Clone(out stm: IStream): HResult;
begin
  Result:=E_NOTIMPL
end;

constructor TOnlyFile.Create(h: Integer; rw: Boolean);
begin
  inherited Create;
  vm_h:=h; vm_rw:=rw
end;

destructor TOnlyFile.Destroy;
begin
  if vm_h > 0 then FileClose(vm_h);
  inherited
end;

function TOnlyFile.Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HResult;
begin
  pcbRead^:=FileRead(vm_h,pv^,cb);
  Result:=S_OK
end;

function TOnlyFile.Write(pv: Pointer; cb: DWORD; pcbWritten: PDWORD): HResult;
begin
  Result:=S_FALSE;

  if vm_rw then begin
    pcbWritten^:=FileWrite(vm_h,pv^,cb);
    Result:=S_OK
  end
end;

function TOnlyFile.Seek(dlibMove: Largeint; dwOrigin: Longint;
  out libNewPosition: Largeint): HResult;
begin
  libNewPosition:=FileSeek(vm_h,dlibMove,dwOrigin);
  Result:=S_OK
end;

function TOnlyFile.SetSize(libNewSize: Largeint): HResult;
var
  Old: Int64; cx: Integer; buf: tbytes;
begin
  Result:=S_FALSE;

  if vm_rw then begin
    Old:=FileSeek(vm_H,0,2);

    if Old < libNewSize then begin

      Fillchar(buf,Sizeof(buf),0);
      while Old < libNewSize do begin
        cx:=Min(libNewSize-Old,Sizeof(buf));
        FileWrite(vm_H,buf,cx); Inc(Old,cx)
      end;

    end
    else begin
      FileSeek(vm_H,libNewSize,0);
      FileWrite(vm_H,buf,0);
    end;

    if FileSeek(vm_H,0,2) = libNewSize then
    Result:=S_OK
  end
end;

function TOnlyFile.CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
  out cbWritten: Largeint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyFile.Commit(grfCommitFlags: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyFile.Revert: HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyFile.LockRegion(libOffset: Largeint; cb: Largeint;
  dwLockType: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyFile.UnlockRegion(libOffset: Largeint; cb: Largeint;
  dwLockType: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyFile.Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
begin
  FillChar(statstg,SizeOf(statstg),0);
  statstg.cbSize:=FileSeek(vm_H,0,2);
  Result:=S_OK
end;

function TOnlyFile.Clone(out stm: IStream): HResult;
begin
  Result:=E_NOTIMPL
end;

constructor TCashFile.Create(h: Integer; rw: Boolean);
begin
  inherited Create;
  vm_h:=h; vm_rw:=rw;

  vm_Size:=FileSeek(vm_h,0,2);

  vm_Page:=xAllocPtr(pg_Size);
  if vm_Page <> nil then begin
    if vm_Size = 0 then begin
      FillChar(vm_Page^,pg_Size,0);
      vm_Size:=FileWrite(vm_h,vm_Page^,pg_Size)
    end;

    FileSeek(vm_h,0,0);
    FileRead(vm_h,vm_Page^,pg_Size);
  end
end;

destructor TCashFile.Destroy;
begin
  Put_Page; xFreePtr(vm_Page);
  if vm_h > 0 then FileClose(vm_h);
  inherited
end;

function TCashFile.Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HResult;
var
  buf: pbytes; len,ofs,pos: Integer;
begin
  if vm_Page <> nil then begin
    pcbRead^:=cb; buf:=pv; ofs:=0;

    while cb > 0 do begin
      pos:=Get_Page; len:=Min(cb,pg_Size-pos);
      Move(vm_Page[pos],buf[ofs],len);
      Inc(ofs,len); Inc(vm_Pos,len);
      Dec(cb,len)
    end

  end else

  pcbRead^:=FileRead(vm_h,pv^,cb);

  Result:=S_OK
end;

function TCashFile.Write(pv: Pointer; cb: DWORD; pcbWritten: PDWORD): HResult;
var
  buf: pbytes; len,ofs,pos: Integer;
begin
  Result:=S_FALSE;

  if vm_rw then begin

    if vm_Page <> nil then begin
      pcbWritten^:=cb; buf:=pv; ofs:=0;

      while cb > 0 do begin
        pos:=Get_Page; len:=Min(cb,pg_Size-pos);
        Move(buf[ofs],vm_Page[pos],len);
        Inc(ofs,len); Inc(vm_Pos,len);
        Dec(cb,len); pg_Put:=true
      end

    end else

    pcbWritten^:=FileWrite(vm_h,pv^,cb);

    Result:=S_OK
  end
end;

function TCashFile.Seek(dlibMove: Largeint; dwOrigin: Longint;
  out libNewPosition: Largeint): HResult;
begin
  vm_Pos:=FileSeek(vm_h,dlibMove,dwOrigin);
  libNewPosition:=vm_Pos; Result:=S_OK
end;

function TCashFile.SetSize(libNewSize: Largeint): HResult;
begin
  Result:=S_FALSE;

  if vm_rw then begin
    put_Page; FileSeek(vm_H,libNewSize,0);
    FileWrite(vm_H,vm_H,0); Result:=S_OK
  end
end;

function TCashFile.CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
  out cbWritten: Largeint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TCashFile.Commit(grfCommitFlags: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TCashFile.Revert: HResult;
begin
  Result:=E_NOTIMPL
end;

function TCashFile.LockRegion(libOffset: Largeint; cb: Largeint;
  dwLockType: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TCashFile.UnlockRegion(libOffset: Largeint; cb: Largeint;
  dwLockType: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TCashFile.Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
begin
  FillChar(statstg,SizeOf(statstg),0);
  statstg.cbSize:=FileSeek(vm_H,0,2);
  Result:=S_OK
end;

function TCashFile.Clone(out stm: IStream): HResult;
begin
  Result:=E_NOTIMPL
end;

function TCashFile.Get_Page: Integer;
var
  pg,pos: Int64;
begin
  pg:=vm_Pos div pg_Size;
  pos:=pg * pg_Size;

  if pos <> pg_Pos then begin
    Put_Page; pg_Pos:=pos;

    if pos < vm_Size then begin
      FileSeek(vm_h,pos,0);
      FileRead(vm_h,vm_Page^,pg_Size)
    end
    else FillChar(vm_Page^,pg_Size,0)
  end;

  Result:=vm_Pos-pos
end;

procedure TCashFile.Put_Page;
begin
  if pg_Put then begin
    FileSeek(vm_H,pg_Pos,0);
    FileWrite(vm_H,vm_Page^,pg_Size);
    vm_Size:=Max(vm_Size,pg_Pos+pg_Size);
    pg_Put:=false;
  end
end;

function TMemoryStorage.CreateStream(pwcsName: POleStr; grfMode: Longint;
  reserved1: longint; reserved2: Longint; out stm: IStream): HResult;
begin
  Result:=S_FALSE; stm:=nil;

  with TOnlyMemory.Create do
  Result:=QueryInterface(IStream,stm)
end;

function TMemoryStorage.OpenStream(pwcsName: POleStr; reserved1: Pointer;
  grfMode: Longint; reserved2: Longint; out stm: IStream): HResult;
begin
  Result:=S_FALSE; stm:=nil;
end;

function TMemoryStorage.CreateStorage(pwcsName: POleStr; grfMode: Longint;
  reserved1,reserved2: Longint; out stg: IStorage): HResult;
begin
  Result:=E_NOTIMPL
end;

function TMemoryStorage.OpenStorage(pwcsName: POleStr; const stgPriority: IStorage;
  grfMode: Longint; snbExclude: TSNB; reserved: Longint;
  out stg: IStorage): HResult;
begin
  Result:=E_NOTIMPL
end;

function TMemoryStorage.CopyTo(ciidExclude: Longint; rgiidExclude: PGUID;
  snbExclude: TSNB; const stgDest: IStorage): HResult;
begin
  Result:=E_NOTIMPL
end;

function TMemoryStorage.MoveElementTo(pwcsName: POleStr; const stgDest: IStorage;
  pwcsNewName: POleStr; grfFlags: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TMemoryStorage.Commit(grfCommitFlags: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TMemoryStorage.Revert: HResult;
begin
  Result:=E_NOTIMPL
end;

function TMemoryStorage.EnumElements(reserved1: Longint; reserved2: Pointer; reserved3: Longint;
  out enm: IEnumStatStg): HResult;
begin
  Result:=E_NOTIMPL
end;

function TMemoryStorage.DestroyElement(pwcsName: POleStr): HResult;
begin
  Result:=E_NOTIMPL
end;

function TMemoryStorage.RenameElement(pwcsOldName: POleStr;
  pwcsNewName: POleStr): HResult;
begin
  Result:=E_NOTIMPL
end;

function TMemoryStorage.SetElementTimes(pwcsName: POleStr; const ctime: TFileTime;
  const atime: TFileTime; const mtime: TFileTime): HResult;
begin
  Result:=E_NOTIMPL
end;

function TMemoryStorage.SetClass(const clsid: TCLSID): HResult;
begin
  Result:=E_NOTIMPL
end;

function TMemoryStorage.SetStateBits(grfStateBits: Longint; grfMask: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TMemoryStorage.Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

constructor TOnlyMemory.Create;
begin
  inherited; Resize(4096);
end;

destructor TOnlyMemory.Destroy;
begin
  xFreePtr(vm_buf);
  inherited
end;

function TOnlyMemory.Active: Boolean;
begin
  Result:=Assigned(vm_buf)
end;

function TOnlyMemory.Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HResult;
var
  len: Longint;
begin
  len:=Min(cb,vm_Size-vm_Pos);
  if len < cb then Fillchar(pv^,cb,0);
  if len > 0 then Move(vm_buf[vm_Pos],pv^,len);
  if Assigned(pcbRead) then pcbRead^:=len;
  vm_Pos:=vm_Pos+len; Result:=S_OK
end;

function TOnlyMemory.Write(pv: Pointer; cb: DWORD; pcbWritten: PDWORD): HResult;
var
  len,size: Integer;
begin
  Result:=S_False; len:=0;

  size:=vm_Pos + cb;
  if Resize(size) then begin
    Move(pv^,vm_buf[vm_Pos],cb);
    vm_Size:=Max(vm_Size,size);
    Inc(vm_Pos,cb); len:=cb;
    Result:=S_OK
  end;

  if Assigned(pcbWritten) then
  pcbWritten^:=len;
end;

function TOnlyMemory.Seek(dlibMove: Largeint; dwOrigin: Longint;
  out libNewPosition: Largeint): HResult;
var
  pos: Largeint;
begin
  Result:=S_FALSE;
  pos:=vm_Pos;

  case dwOrigin of
0:  pos:=dlibMove;
1:  pos:=pos+dlibMove;
2:  pos:=vm_Size+dlibMove;
  end;

  if pos >= 0 then
  if pos <= vm_Size then begin
    vm_Pos:=pos; Result:=S_OK
  end;

  libNewPosition:=vm_Pos
end;

function TOnlyMemory.SetSize(libNewSize: Largeint): HResult;
begin
  if libNewSize >= 0 then
  if libNewSize < vm_Capacity then
  vm_Size:=libNewSize;
  Result:=S_OK
end;

function TOnlyMemory.CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
  out cbWritten: Largeint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyMemory.Commit(grfCommitFlags: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyMemory.Revert: HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyMemory.LockRegion(libOffset: Largeint; cb: Largeint;
  dwLockType: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyMemory.UnlockRegion(libOffset: Largeint; cb: Largeint;
  dwLockType: Longint): HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyMemory.Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
begin
  FillChar(statstg,SizeOf(statstg),0);
  statstg.cbSize:=vm_Size;
  Result:=S_OK
end;

function TOnlyMemory.Clone(out stm: IStream): HResult;
begin
  Result:=E_NOTIMPL
end;

function TOnlyMemory.Resize(NewSize: Integer): Boolean;
var
  tmp: Pointer;
begin
  Result:=false;

  if vm_buf = nil then begin
    NewSize:=int_Round(NewSize,4096);
    vm_buf:=xAllocPtr(NewSize);
    vm_Capacity:=NewSize;
  end else
  if NewSize > vm_Capacity then begin
    NewSize:=int_Round(NewSize,4096);
    tmp:=xReAllocPtr(vm_buf,NewSize);
    if Assigned(tmp) then begin
      vm_buf:=tmp; vm_Capacity:=NewSize;
    end
  end;

  if Assigned(vm_buf) then
  Result:=vm_Capacity >= NewSize
end;

initialization
begin
  DosStorage:=TDosStorage.Create;
  DosStorage._AddRef
end;

finalization
  DosStorage._Release;

end.