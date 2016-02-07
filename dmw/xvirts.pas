unit xvirts; interface {$H-}

uses
  Windows,SysUtils,Classes,activeX,
  Math,otypes,ofiles,convert,xdos,
  storage;

const
  vm_MaxLen = $FF00;
  vm_MaxLen32 = $FF0000;

type
  vm_Len = record Size,Len: word end;
  vmd_Len = record Size,Len: DWord end;

  TVirtMem = class
    vm_Storage: IStorage;
    vm_Stream: IStream;

    vm_Edit: Boolean;
    vm_ole: Boolean;

    vm_Ind: longint;
    vm_Path: TShortStr;

    constructor Create(cash,ind: Integer);
    destructor Destroy; override;

    function doc_Create(Path: PChar): Boolean;
    function doc_Open(Path: PChar; rw: Boolean): Boolean;

    function fld_Create(Name: PChar): Boolean;
    function fld_Open(Name: PChar; rw: Boolean): Boolean;

    procedure vm_Assign(const doc: IStorage;
                        const stm: IStream;
                        init: Boolean);

    function stm_Open(const doc: IStorage;
                      Name: PChar; Mode: Integer): Boolean;

    function stm_Create(const doc: IStorage;
                        Name: PChar): Boolean;

    function stm_Memory(Name: PChar): Boolean;

    function vm_Open(Path: PChar): Boolean;
    function vm_Make(Path: PChar): Boolean;
    function vm_Temp(Prefix: PChar): Boolean;
    function vm_Work(Prefix: PChar): Boolean;
    function vm_Work1(Prefix: PChar): Boolean;

    function vm_Open_bin(fn: PChar): Boolean;

    function vm_Open_ext(Path,ext: PChar): Boolean;
    function vm_Make_ext(Path,ext: PChar): Boolean;
    function vm_FileName: string;

    procedure vm_Activate;
    procedure vm_Truncate;

    procedure vm_Begin; virtual;
    procedure vm_Close; virtual;

    function vm_ReadOnly: Boolean;
    function vm_Update: Boolean;
    function vm_Back: Boolean;

    function vm_Refresh(Edit: Boolean): Boolean;
    procedure vm_Return(Temp: TVirtMem);

    procedure vm_Startup; virtual;
    procedure vm_Erase;

    function vm_Rename(Path: PChar): Boolean;

    procedure vm_Fill(Adr: longint; Len: longint; ch: byte);

    function vm_GetMem(out Adr: longint; Len: Integer): longint;
    procedure vm_FreeMem(Adr: longint; Len: Integer);

    function vm_Load (Adr: longint; out buf; Len: Integer): longint;
    function vm_Store(Adr: longint; var buf; Len: Integer): longint;

    function vmx_Load(Adr: longint; out buf; Len: Integer): Boolean;

    procedure vm_Move(inp,dst,len: longint);
    procedure vm_Down(inp,dst,len: longint);

    procedure vm_Swap(ofs1,ofs2,len: longint);

    function vmd_FreeBuf(p: longint): longint;
    function vmd_BufLen(p: longint): Integer;
    function vmd_Buffer(p: Longint): longint;
    function vmd_AllocBuf(len: Integer): longint;
    function vmd_ResizeBuf(p, len: longint): longint;

    function vm_AllocBuf(len: Integer): longint;
    function vm_FreeBuf(p: longint): longint;
    function vm_BufLen(p: longint): Integer;
    function vm_BufSize(p: longint): Integer;
    function vm_ResizeBuf(p, len: longint): longint;

    function vm_LoadBuf(p: longint; var buf; max: Integer): Integer;
    function vm_UpDateBuf(p: longint; var buf; len: Integer): longint;
    function vm_UpdateMsg(p: longint; Cmd: string): longint;

    function vmx_FreeBuf(p: longint): longint;
    function vmx_Buffer(p: Longint): longint;
    function vmx_BufLen(p: longint): Integer;

    function vmx_Block(p: longint; out len: int): int;
    function vmx_Verify(p: longint; out size: int): int;

    function vmx_ResizeBuf(var p: longint; len: Longint): Boolean;
    function vmx_UpdateBuf(var p: longint; var buf; len: Integer): Boolean;
    function vmx_TruncBuf(p, len: longint): longint;

    function vm_CopyBuf(inp,dst: longint; VM: TVirtMem): longint;
    function vm_CopyMsg(inp: longint; vm: TVirtMem): longint;

    function vm_LoadStr(Adr: longint; Str: PChar): longint;
    function vm_LoadMsg(Adr: longint; var Msg: string): longint;
    function vm_ReadMsg(Adr: longint; var Msg: PString): longint;
    function vm_Verify_str(Adr: longint): Boolean;

    function vm_Byte(Adr: longint): Integer;
    function vm_Word(Adr: longint): Integer;
    function vm_Long(Adr: longint): Longint;
    function vm_String(Adr: longint): string;

    function vm_Append(var buf; len: Integer): longint;
    function vm_Expand(len: longint): longint;

    function Stream_Copy(p: longint; sf: TStream): Integer;
    function Stream_Paste(p: longint; sf: TStream): longint;

    function Stm_Copy(p: longint; const stm: IStream): Integer;

    function Stm_Paste(p: longint; const stm: IStream; pos: Integer): longint;

    function vm_LoadFrom(Path: PChar): longint;
    function vm_SaveAs(Path: PChar): PChar;

    procedure CopyTo(Dest: TVirtMem);

    function vmx_Append(stg: TVirtMem; pos,len: int): int;

    function vm_Domain(ind: longint): boolean;

    procedure vm_Push(buf: Pointer; len: Integer);
    function vm_Pop(buf: Pointer; Size: Integer): Integer;
    procedure vm_Skip;

    procedure min_max_long(Ind,Count: Integer;
                           out v1,v2: Integer);

    procedure min_max_inc(Ind,Count: Integer;
                          out v1,v2: Integer);

    function min_max_xy(Ind,Count: Integer;
                        out lt,rb: TPoint): Integer;

    function min_max_dxy(Ind,Count: Integer;
                         out lt,rb: TPoint): Integer;

  private
    vm_doc: IStorage;
    vm_Off: Integer;

    FOnActivate: TNotifyEvent;
    FOnClose: TNotifyEvent;

    vm_Page: pbytes;
    pg_Pos: longint;
    pg_Put: Longbool;

    fvm_x32: Longbool;

    function Get_Active: Boolean;

    procedure Set_Cached(Cached: Boolean);

    function page_Get(ofs: longint): Integer;
    procedure page_Clear;
    procedure page_Put;

  public
    property vm_Active: Boolean read Get_Active;
    property vm_Cached: Boolean write Set_Cached;
    property vm_Buffer: pbytes read vm_Page;

    property vm_x32: Longbool read fvm_x32
                              write fvm_x32;

    property OnActivate: TNotifyEvent write FOnActivate;
    property OnClose: TNotifyEvent write FOnClose;
  end;

  TReadStreamVM = class(TStream)

    constructor Create(vm: TVirtMem; p: longint);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

  private
    vm_mem: TVirtMem;
    vm_Adr: longint;
    vm_Size: longint;
    vm_Pos: longint;
  end;

  TCtrlMem = class
    constructor Create(mem: TVirtMem);
    destructor Destroy; override;
    function ctrl(p: Integer): Boolean;
  private
    lpbits: PBytes;
    vm_size: Integer;
    vm: TVirtMem;
  end;

implementation {$R-}

const
  pg_Size = 4096 * 4;

var
  mem: tbytes;

constructor TVirtMem.Create(cash,ind: Integer);
begin
  inherited Create;
  DosStorage.GetInterface(IStorage,vm_Storage);
  vm_Off:=ind; pg_Pos:=-1;
end;

destructor TVirtMem.Destroy;
begin
  vm_Close; xFreePtr(vm_Page);
  inherited
end;

function TVirtMem.Get_Active: Boolean;
begin
  Result:=Assigned(vm_Stream)
end;

procedure TVirtMem.Set_Cached(Cached: Boolean);
begin
  page_Clear;

  if Cached then begin
    if not Assigned(vm_Page) then
    vm_Page:=xAllocPtr(pg_Size)
  end else

  vm_Page:=xFreePtr(vm_Page);
end;

function TVirtMem.page_Get(ofs: longint): Integer;
var
  pg,top,cbRead: longint; pos: Int64;
begin
  pg:=ofs div pg_Size;
  top:=pg * pg_Size;

  if top <> pg_Pos then begin
    Page_Put; pg_Pos:=top;

    if top < vm_Ind then begin
      vm_Stream.Seek(top,0,pos);
      vm_Stream.Read(vm_Page,pg_Size,@cbRead)
    end
    else FillChar(vm_Page^,pg_Size,0)
  end;

  Result:=ofs-top
end;

procedure TVirtMem.page_Put;
var
  cbWritten: Longint; Pos: Int64;
begin
  if pg_Put then begin
    vm_Stream.Seek(pg_Pos,0,Pos);
    vm_Stream.Write(vm_Page,pg_Size,@cbWritten);
    pg_Put:=false;
  end
end;

procedure TVirtMem.page_Clear;
begin
  page_Put; pg_Pos:=-1;
end;

function TVirtMem.doc_Create(Path: PChar): Boolean;
begin
  Result:=xCreateStorage(Path, vm_Storage) = S_OK
end;

function TVirtMem.doc_Open(Path: PChar; rw: Boolean): Boolean;
var
  mode: Integer;
begin
  if rw then mode:=ole_ReadWrite
  else mode:=ole_Read; vm_Edit:=rw;

  Result:=false; if xStgIsStorageFile(Path) then
  Result:=xOpenStorage(Path,mode,vm_Storage) = S_OK
end;

function TVirtMem.fld_Create(Name: PChar): Boolean;
var
  stg: IStorage;
begin
  Result:=false;

  if Assigned(vm_Storage) then begin
    xDeleteElement(vm_Storage,Name);
    if xCreateFolder(vm_Storage,Name,stg) = S_OK then begin
      vm_Edit:=true; vm_Storage:=stg; Result:=true
    end
  end
end;

function TVirtMem.fld_Open(Name: PChar; rw: Boolean): Boolean;
var
  mode: Integer; stg: IStorage;
begin
  Result:=false;

  if rw then mode:=ole_ReadWrite
  else mode:=ole_Read;

  if Assigned(vm_Storage) then
  if xOpenFolder(vm_Storage,Name,mode,stg) then begin
    vm_Edit:=rw; vm_Storage:=stg; Result:=true
  end
end;

procedure TVirtMem.vm_Assign(const doc: IStorage;
                             const stm: IStream;
                             init: Boolean);
var
  fn: tShortStr;
begin
  vm_Close;

  vm_Stream:=stm; vm_doc:=doc;

  if stm_FileName(stm,fn) <> nil then
  StrCopy(vm_Path,StrLower(fn));

  if init then begin
    vm_Edit:=true; vm_Startup
  end;

  vm_Ind:=xSize(stm);
  vm_Activate;
end;

function TVirtMem.stm_Open(const doc: IStorage;
                           Name: PChar; Mode: Integer): Boolean;
var
  stm: IStream; fn: TWideStr;
begin
  Result:=false; vm_Close;

  if StrToWideChar(Name,fn,255) <> nil then
  if doc.OpenStream(fn,nil,Mode,0,stm) = S_OK then
  vm_Assign(doc,stm,false); stm:=nil;

  vm_Edit:=Mode and STGM_READWRITE <> 0;

  if vm_Active then begin
    if vm_ole then vm_Storage:=doc;
    Result:=true
  end
end;

function TVirtMem.stm_Create(const doc: IStorage;
                             Name: PChar): Boolean;
var
  stm: IStream; fn: TWideStr;
begin
  Result:=false; vm_Close;

  if StrToWideChar(Name,fn,255) <> nil then
  if doc.CreateStream(fn,ole_CREATE,0,0,stm) = s_OK then
  vm_Assign(nil,stm,true); stm:=nil;

  if vm_Active then begin
    if vm_ole then vm_Storage:=doc;
    Result:=true
  end
end;

function TVirtMem.stm_Memory(Name: PChar): Boolean;
var
  stg: IStorage; stm: IStream; fn: TWideStr;
begin
  Result:=false; vm_Close;

  with TMemoryStorage.Create do
  GetInterface(IStorage,stg);

  if Assigned(stg) then
  if StrToWideChar(Name,fn,255) <> nil then
  if stg.CreateStream(fn,ole_CREATE,0,0,stm) = s_OK then
  vm_Assign(nil,stm,true); stm:=nil;

  if vm_Active then begin
    vm_Storage:=stg; Result:=true
  end
end;

function TVirtMem.vm_Open(Path: PChar): Boolean;
var
  mode: Integer; fn: TWideStr;
begin
  if not vm_Active
  or (StrIComp(vm_Path,Path) <> 0) then begin

    vm_Close; if StrLen(Path) > 0 then

    if Assigned(vm_Storage) then
    if StrToWideChar(Path,fn,255) <> nil then begin

      mode:=ole_Read; if vm_Edit then
      mode:=ole_ReadWrite;

      if vm_Storage.OpenStream(fn,nil,mode,0, vm_Stream) = S_OK then
      begin
        StrLower(StrCopy(vm_Path,Path));
        vm_Ind:=xSize(vm_Stream);
        vm_Activate;
      end
    end
  end;

  Result:=vm_Active
end;

function TVirtMem.vm_Make(Path: PChar): Boolean;
var
  fn: TWideStr;
begin
  Result:=false; vm_Close;

  if StrLen(Path) > 0 then

  if Assigned(vm_Storage) then
  if StrToWideChar(Path,fn,255) <> nil then
  if vm_Storage.CreateStream(fn,ole_Create,0,0,vm_Stream) = S_OK then
  begin
    vm_Edit:=true;
    StrLower(StrCopy(vm_Path,Path));
    vm_Startup; vm_Activate;
    Result:=true
  end
end;

function TVirtMem.vm_Temp(Prefix: PChar): Boolean;
var
  Path: TShortStr;
begin
  Result:=false;
  if xStrTempFileName(Path,Prefix) <> nil then
  Result:=vm_Make(Path)
end;

function TVirtMem.vm_Work(Prefix: PChar): Boolean;
var
  Path: TShortStr;
begin
  Result:=false;
  if zStrTempFileName(Path,Prefix) <> nil then
  Result:=vm_Make(Path)
end;

function TVirtMem.vm_Work1(Prefix: PChar): Boolean;
var
  Path: TShortStr;
begin
  Result:=false;
  if wStrTempFileName1(Path,Prefix) <> nil then
  Result:=vm_Make(Path)
end;

function TVirtMem.vm_Open_bin(fn: PChar): Boolean;
var
  Path: TShortStr;
begin
  StrPath(Path,BinDir,fn);
  Result:=vm_Open(Path) 
end;

function TVirtMem.vm_Open_ext(Path,ext: PChar): Boolean;
var
  fn: TShortStr;
begin
  Result:=vm_Open(StrUpdateExt(fn,Path,ext))
end;

function TVirtMem.vm_Make_ext(Path,ext: PChar): Boolean;
var
  fn: TShortStr;
begin
  Result:=vm_Make(StrUpdateExt(fn,Path,ext))
end;

function TVirtMem.vm_FileName: string;
begin
  Result:=ExtractFileName(StrPas(vm_Path))
end;

procedure TVirtMem.vm_Activate;
begin
  vm_Begin;
  if Assigned(FOnActivate) then
  FOnActivate(Self)
end;

procedure TVirtMem.vm_Begin;
begin
end;

procedure TVirtMem.vm_Truncate;
begin
  if vm_Active then begin
    page_Clear;

    if vm_Edit then
    vm_Stream.SetSize(vm_Ind);
  end
end;

procedure TVirtMem.vm_Close;
begin
  if vm_Active then begin
    page_Clear;

    if vm_Edit then
    vm_Stream.SetSize(vm_Ind);

    vm_Stream:=nil; vm_doc:=nil;
    if vm_ole then vm_Storage:=nil;

    if Assigned(FOnClose) then
    FOnClose(Self)
  end
end;

procedure TVirtMem.vm_Startup;
begin
  page_Clear;
  vm_Fill(0,vm_Off,0);

  if vm_Active then
  vm_Stream.SetSize(vm_Off);

  vm_Ind:=vm_Off;
  page_Clear
end;

function TVirtMem.vm_ReadOnly: Boolean;
begin
  Result:=vm_Refresh(false)
end;

function TVirtMem.vm_Update: Boolean;
begin
  Result:=vm_Refresh(true)
end;

function TVirtMem.vm_Back: Boolean;
begin
  if not vm_Active then
  vm_Open(vm_Path); Result:=vm_Active
end;

function TVirtMem.vm_Refresh(Edit: Boolean): Boolean;
begin
  if Strlen(vm_Path) > 0 then
  if not vm_Active or (vm_Edit <> Edit) then
  if not Edit or not FileReadOnly(vm_Path) then begin
    vm_Close; vm_Edit:=Edit; vm_Open(vm_Path)
  end; Result:=vm_Active
end;

procedure TVirtMem.vm_Return(Temp: TVirtMem);
begin
  if vm_Active then vm_Close;
  if StrLen(vm_Path) > 0 then begin
    if Temp.vm_Active then Temp.vm_Close;
    Temp.vm_Rename(vm_Path);
    vm_Back
  end
end;

procedure TVirtMem.vm_Erase;
var
  fn: TWideStr;
begin
  if vm_Active then vm_Close;

  if Assigned(vm_Storage) then
  if StrToWideChar(vm_Path,fn,255) <> nil then
  vm_Storage.DestroyElement(fn)
end;

function TVirtMem.vm_Rename(Path: PChar): Boolean;
begin
  Result:=false; FileErase(Path);
  if not vm_Active then if StrLen(vm_Path) > 0 then
  Result:=RenameFile(StrPas(vm_Path),StrPas(Path))
end;

procedure TVirtMem.vm_Fill(Adr: longint; Len: longint; ch: byte);
var
  loc: Integer;
begin
  FillChar(mem,SizeOf(mem),ch);

  while Len > 0 do begin
    loc:=Min(Len,SizeOf(mem));
    Adr:=vm_Store(Adr,mem,loc);
    Dec(Len,loc);
  end
end;

function TVirtMem.vm_GetMem(out Adr: longint; Len: Integer): longint;
begin
  Result:=0; Adr:=0;

  if vm_Edit then begin
    if Len < 8 then Len:=8;
    Len:=((Len+3) div 4) * 4;
    Adr:=vm_Expand(Len);
    Result:=Len
  end
end;

procedure TVirtMem.vm_FreeMem(Adr: longint; Len: Integer);
begin
  if Adr + Len = vm_Ind then
  vm_Ind:=Adr
end;

function TVirtMem.vm_Load(Adr: longint; out buf; Len: Integer): longint;
var
  cbRead: Longint; Pos: Int64;
  bufp: tbytes absolute buf;
  src,dst,cb: Integer;
begin
  Result:=Adr+Len;

  if Assigned(vm_Stream) then

  if vm_Page <> nil then begin

    dst:=0; while Len > 0 do begin
      src:=page_Get(Adr); cb:=Min(Len,pg_Size-src);
      Move(vm_Page[src],bufp[dst],cb); Inc(dst,cb);
      Inc(Adr,cb); Dec(Len,cb)
    end

  end
  else begin
    vm_Stream.Seek(Adr,0,Pos);
    vm_Stream.Read(@buf,Len,@cbRead)
  end

  else FillChar(buf,Len,0)
end;

function TVirtMem.vm_Store(Adr: longint; var buf; Len: Integer): longint;
var
  cbWritten: Longint; Pos: Int64;
  bufp: tbytes absolute buf;
  src,dst,cb: Integer;
begin
  Result:=Adr+Len;

  if vm_Edit then
  if Assigned(vm_Stream) then

  if vm_Page <> nil then begin

    src:=0; while Len > 0 do begin
      dst:=page_Get(Adr); cb:=Min(Len,pg_Size-dst);
      Move(bufp[src],vm_Page[dst],cb); Inc(src,cb);
      Inc(Adr,cb); Dec(Len,cb); pg_Put:=true
    end

  end
  else begin
    vm_Stream.Seek(Adr,0,Pos);
    vm_Stream.Write(@buf,Len,@cbWritten)
  end
end;

function TVirtMem.vmx_Load(Adr: longint; out buf; Len: Integer): Boolean;
begin
  Result:=false; if Len > 0 then

  if (Adr >= 0) and (Adr+Len <= vm_Ind) then begin
    vm_Load(Adr,buf,Len); Result:=true
  end else

  Fillchar(buf,Len,0)
end;

procedure TVirtMem.vm_Move(inp,dst,len: longint);
var
  buf: tbytes; cx: Integer;
begin
  while len > 0 do begin
    cx:=Min(len,SizeOf(buf));
    inp:=vm_Load(inp,buf,cx);
    dst:=vm_Store(dst,buf,cx);
    Dec(len,cx);
  end
end;

procedure TVirtMem.vm_Down(inp,dst,len: longint);
var
  buf: tbytes; cx: int;
begin
  Inc(inp,len); Inc(dst,len);

  cx:=dst-vm_Ind;
  if cx > 0 then vm_Expand(cx);

  while len > 0 do begin
    cx:=SizeOf(buf); if cx > len then cx:=len;
    Dec(inp,cx); Dec(dst,cx); vm_Load(inp,buf,cx);
    vm_Store(dst,buf,cx); Dec(len,cx)
  end
end;

procedure TVirtMem.vm_Swap(ofs1,ofs2,len: longint);
var
  buf1,buf2: tbytes; cx: longint;
begin
  while len > 0 do begin
    cx:=Min(len,SizeOf(buf1));

    vm_Load(ofs1,buf1,cx);
    vm_Load(ofs2,buf2,cx);

    ofs1:=vm_Store(ofs1,buf2,cx);
    ofs2:=vm_Store(ofs2,buf1,cx);

    Dec(len,cx)
  end
end;

function TVirtMem.vmd_FreeBuf(p: longint): longint;
begin
  Result:=0; if p > 0 then
  vm_FreeMem(p,vm_Long(p)+8)
end;

function TVirtMem.vmd_BufLen(p: longint): Integer;
var
  Hdr: vmd_Len;
begin
  Result:=0;

  if p > 0 then
  if p+8 < vm_Ind then begin
    vm_Load(p,Hdr,8);
    if Hdr.Len > 0 then
    if Hdr.Len <= Hdr.Size then
    if p+8+Hdr.Size <= vm_Ind then
    Result:=Hdr.Len
  end
end;

function TVirtMem.vmd_Buffer(p: Longint): longint;
begin
  Result:=0; if p > 0 then
  Result:=p+8
end;

function TVirtMem.vmd_AllocBuf(len: Integer): longint;
var
  p: Longint; hdr: vmd_Len;
begin
  Result:=0;
  hdr.Size:=vm_GetMem(p,len+8)-8;
  if hdr.Size >= len then begin
    hdr.Len:=len; vm_Store(p,hdr,8);
    Result:=p
  end
end;

function TVirtMem.vmd_ResizeBuf(p, len: longint): longint;
var
  Hdr: vmd_Len;
begin
  if len > 0 then begin
    Hdr.Size:=0; Hdr.Len:=len;
    if p > 0 then Hdr.Size:=vm_Long(p);

    if Hdr.Len <= Hdr.Size then
      vm_Store(p,Hdr,8)
    else begin
      if p > 0 then
      vm_FreeMem(p,Hdr.Size+8);
      p:=vmd_AllocBuf(len)
    end
  end
  else p:=vmd_FreeBuf(p);

  Result:=p
end;

function TVirtMem.vm_AllocBuf(len: Integer): longint;
var
  p: Longint; hdr: vm_Len;
begin
  Result:=0;
  hdr.Size:=vm_GetMem(p,len+4)-4;

  if hdr.Size >= len then begin
    hdr.Len:=len; vm_Store(p,hdr,4);
    Result:=p
  end
end;

function TVirtMem.vm_FreeBuf(p: longint): longint;
begin
  Result:=0; if p > 0 then
  vm_FreeMem(p,vm_Word(p)+4)
end;

function TVirtMem.vm_BufLen(p: longint): Integer;
begin
  Result:=0; if p > 0 then
  Result:=vm_Word(p+2)
end;

function TVirtMem.vm_BufSize(p: longint): Integer;
begin
  Result:=0; if p > 0 then
  Result:=vm_Word(p)
end;

function TVirtMem.vm_ResizeBuf(p, len: longint): longint;
var
  Hdr: vm_Len;
begin
  if (len > 0) and (len <= vm_MaxLen) then begin
    Hdr.Size:=0; Hdr.Len:=len;
    if p > 0 then Hdr.Size:=vm_Word(p);

    if Hdr.Len > Hdr.Size then begin
      if p > 0 then vm_FreeMem(p,Hdr.Size+4);
      Hdr.Size:=vm_GetMem(p,Hdr.Len+4)-4
    end;

    if p > 0 then vm_Store(p,Hdr,4)
  end
  else p:=vm_FreeBuf(p);

  Result:=p
end;

function TVirtMem.vm_LoadBuf(p: longint; var buf; max: Integer): Integer;
begin
  Result:=vm_BufLen(p);
  if Result > max then Result:=max;
  if Result > 0 then vm_Load(p+4,buf,Result);
end;

function TVirtMem.vm_UpdateBuf(p: longint; var buf; len: Integer): longint;
begin
  Result:=vm_ResizeBuf(p,len);

  if Result > 0 then
  vm_Store(Result+4,buf,len)
end;

function TVirtMem.vmx_FreeBuf(p: longint): longint;
var
  cx: int;
begin
  Result:=0; if p > 0 then begin

    if fvm_x32 then cx:=vm_Long(p)+8
               else cx:=vm_Word(p)+4;

    vm_FreeMem(p,cx)
  end
end;

function TVirtMem.vmx_Buffer(p: Longint): longint;
begin
  if p > 0 then begin
    Inc(p,4); if fvm_x32 then Inc(p,4)
  end; Result:=p
end;

function TVirtMem.vmx_BufLen(p: longint): Integer;
begin
  if fvm_x32 then
    Result:=vmd_BufLen(p)
  else
    Result:=vm_BufLen(p)
end;

function TVirtMem.vmx_Block(p: longint; out len: int): int;
var
  b1: vm_Len; b2: vmd_Len;
begin
  Result:=0; len:=0;

  if p > 0 then begin

    if fvm_x32 then begin
      p:=vm_Load(p,b2,8);

      if b2.Len > 0 then
      if b2.Len <= b2.Size then
      if p+b2.Size <= vm_Ind then
      len:=b2.Len
    end
    else begin
      p:=vm_Load(p,b1,4);

      if b1.Len > 0 then
      if b1.Len <= b1.Size then
      if p+b1.Size <= vm_Ind then
      len:=b1.Len
    end;

    Result:=p
  end
end;

function TVirtMem.vmx_Verify(p: longint; out size: int): int;
var
  b1: vm_Len; b2: vmd_Len;
begin
  Result:=0; size:=0;

  if p > 0 then begin

    if fvm_x32 then begin
      p:=vm_Load(p,b2,8);

      if b2.Len > 0 then
      if b2.Len <= b2.Size then
      if p+b2.Size <= vm_Ind then
      size:=b2.Size+8
    end
    else begin
      p:=vm_Load(p,b1,4);

      if b1.Len > 0 then
      if b1.Len <= b1.Size then
      if p+b1.Size <= vm_Ind then
      size:=b1.Size+4
    end;

    Result:=p
  end
end;

function TVirtMem.vmx_ResizeBuf(var p: longint; len: Longint): Boolean;
begin
  Result:=false;

  if fvm_x32 then begin

    if len <= vm_MaxLen32 then begin
      p:=vmd_ResizeBuf(p,len); Result:=p > 0
    end

  end else

  if len <= vm_MaxLen then begin
    p:=vm_ResizeBuf(p,len); Result:=p > 0
  end
end;

function TVirtMem.vmx_UpDateBuf(var p: longint; var buf; len: Integer): Boolean;
var
  di: int;
begin
  Result:=vmx_ResizeBuf(p,len);
  if Result and (p > 0) then begin
    di:=p+4; if fvm_x32 then Inc(di,4);
    vm_Store(di,buf,len)
  end
end;

function TVirtMem.vmx_TruncBuf(p, len: longint): longint;

function vmd_TruncBuf(p, len: longint): longint;
var
  sz: long;
begin
  if len <= 0 then
    p:=vmd_FreeBuf(p)
  else begin
    sz:=vm_Long(p);
    if len <= sz then
    vm_Store(p+4,len,4)
  end;

  Result:=p
end;

function vm_TruncBuf(p, len: longint): longint;
var
  sz: long;
begin
  if len <= 0 then
    p:=vm_FreeBuf(p)
  else begin
    sz:=vm_Word(p);
    if len <= sz then
    vm_Store(p+2,len,2)
  end;

  Result:=p
end;

begin
  if p > 0 then

  if fvm_x32 then
    p:=vmd_TruncBuf(p,len)
  else
    p:=vm_TruncBuf(p,len);

  Result:=p
end;

function TVirtMem.vm_UpdateMsg(p: longint; Cmd: string): longint;
var
  len: byte;
begin
  len:=0; if p > 0 then vm_Load(p,len,1);

  if length(Cmd) = 0 then begin
    if len > 0 then vm_FreeMem(p,len+2); p:=0
  end
  else begin
    if len < length(Cmd) then begin
      if len > 0 then vm_FreeMem(p,len+2);
      vm_GetMem(p,length(Cmd)+2);
      vm_Store(p,Cmd[0],1)
    end;

    vm_Store(p+1,Cmd,length(Cmd)+1)
  end;

  Result:=p
end;

function TVirtMem.vm_CopyBuf(inp,dst: longint; VM: TVirtMem): longint;
var
  len,loc,si,di: int; buf: TBytes;
begin
  Result:=0;

  len:=vmx_BufLen(inp);
  if len > 0 then begin

    if VM.vmx_ResizeBuf(dst,len) then begin

      Result:=dst;

      si:=vmx_Buffer(inp);
      di:=VM.vmx_Buffer(Result);

      while len > 0 do begin
        loc:=Min(len,SizeOf(buf));
        si:=vm_Load(si,buf,loc);
        di:=VM.vm_Store(di,buf,loc);
        Dec(len,loc)
      end
    end
  end
end;

function TVirtMem.vm_CopyMsg(inp: longint; VM: TVirtMem): longint;
var
  s: string; len: byte;
begin
  Result:=0; if inp > 0 then begin
    vm_Load(inp+1,s[0],1); vm_Load(inp+2,s[1],length(s));
    len:=(length(s)+2+3) and $FFFC; Dec(len,2);

    Result:=VM.vm_Ind; VM.vm_Store(Result,len,1);
    VM.vm_Store(Result+1,s,length(s)+1);
    Inc(VM.vm_Ind,len+2)
  end
end;

function TVirtMem.vm_LoadStr(Adr: longint; Str: PChar): longint;
var
  len: byte;
begin
  StrCopy(Str,''); Result:=vm_Load(Adr,len,1);
  if len > 0 then begin
    Result:=vm_Load(Result,Str[0],len);
    Str[len]:=#0
  end
end;

function TVirtMem.vm_LoadMsg(Adr: longint; var Msg: string): longint;
var
  len: byte;
begin
  Result:=vm_Load(Adr,len,1); Msg[0]:=chr(len);
  if len > 0 then Result:=vm_Load(Result,Msg[1],len)
end;

function TVirtMem.vm_ReadMsg(Adr: longint; var Msg: PString): longint;
var
  len: Integer; p: PString;
begin
  len:=vm_Byte(Adr)+1;

  try
    begin
      GetMem(p,len);
      vm_Load(Adr,p^,len)
    end;
  except
    on EOutOfMemory do p:=nil;
  end;

  Msg:=p; Result:=Adr+len
end;

function TVirtMem.vm_Verify_str(Adr: longint): Boolean;
var
  len: Integer;
begin
  Result:=false;

  if Adr = 0 then
    Result:=true
  else
  if Adr < vm_Ind then begin
    len:=vm_Byte(Adr);
    Result:=Adr+1+len <= vm_Ind
  end
end;

function TVirtMem.vm_Byte(Adr: longint): Integer;
begin
  Result:=0; vm_Load(Adr,Result,1)
end;

function TVirtMem.vm_Word(Adr: longint): Integer;
begin
  Result:=0; vm_Load(Adr,Result,2)
end;

function TVirtMem.vm_Long(Adr: longint): Longint;
begin
  vm_Load(Adr,Result,4)
end;

function TVirtMem.vm_String(Adr: longint): string;
begin
  Result:=''; if Adr > 0 then
  vm_LoadMsg(Adr+1,Result)
end;

function TVirtMem.vm_Append(var buf; len: Integer): longint;
var
  NewSize: Int64;
begin
  Result:=0; NewSize:=vm_Ind + len;

  if vm_Edit then
  if Assigned(vm_Stream) then

  if (vm_Page <> nil) or (vm_doc = nil)
  or (vm_Stream.SetSize(NewSize) = S_OK) then
  begin
    vm_Store(vm_Ind,buf,len);
    Result:=vm_Ind; vm_Ind:=NewSize
  end
end;

function TVirtMem.vm_Expand(len: longint): longint;
var
  NewSize: Int64;
begin
  Result:=0; NewSize:=vm_Ind + len;

  if vm_Edit then
  if Assigned(vm_Stream) then

  if (vm_Page <> nil) or (vm_doc = nil)
  or (vm_Stream.SetSize(NewSize) = S_OK) then
  begin
    vm_Fill(vm_Ind,len,0);
    Result:=vm_Ind; vm_Ind:=NewSize
  end
end;

function TVirtMem.Stream_Copy(p: longint; sf: TStream): Integer;
var
  len,loc: Integer;
begin
  Result:=0;

  if p > 0 then begin
    len:=vm_BufLen(p);
    sf.Size:=len;

    if len > 0 then begin
      sf.Seek(0,soFromBeginning); Inc(p,4);

      while len > 0 do begin
        loc:=Min(SizeOf(mem),len);
        p:=vm_Load(p,mem,loc); Dec(len,loc);
        sf.Write(mem,loc)
      end
    end;

    Result:=sf.Size
  end
end;

function TVirtMem.Stream_Paste(p: longint; sf: TStream): longint;
var
  len,loc: Integer;
begin
  len:=sf.Size;
  Result:=vm_ResizeBuf(p,len);

  if Result > 0 then begin

    p:=Result+4;
    sf.Seek(0,soFromBeginning);

    while len > 0 do begin
      loc:=Min(SizeOf(mem),len);
      sf.Read(mem,loc); Dec(len,loc);
      p:=vm_Store(p,mem,loc);
    end
  end
  else vm_FreeBuf(p);
end;

function TVirtMem.Stm_Copy(p: longint; const stm: IStream): Integer;
var
  len,loc,si: int;
begin
  Result:=-1;

  si:=vmx_Buffer(p);
  len:=vmx_BufLen(p);

  if len > 0 then begin
    Result:=xSeek_Bottom(stm);

    stm.Write(@len,SizeOf(len),nil);

    while len > 0 do begin
      loc:=Min(SizeOf(mem),len);
      si:=vm_Load(si,mem,loc);
      stm.Write(@mem,loc,nil);
      Dec(len,loc);
    end
  end
end;

function TVirtMem.Stm_Paste(p: longint; const stm: IStream; pos: Integer): longint;
var
  len,loc: Integer;
begin
  Result:=0;

  if pos >= 0 then
  if xSeek(stm,pos) then
  
  if xRead(stm,len,SizeOf(len)) then
  if vmx_ResizeBuf(p,len) then
  if p > 0 then begin

    Result:=p;
    p:=vmx_Buffer(p);

    while len > 0 do begin
      loc:=Min(SizeOf(mem),len);
      stm.Read(@mem,loc,nil);
      p:=vm_Store(p,mem,loc);
      Dec(len,loc);
    end
  end;

  if Result = 0 then
  vmx_FreeBuf(p)
end;

function TVirtMem.vm_LoadFrom(Path: PChar): longint;
var
  stm: TReadFile; ic: Longint;
begin
  Result:=0; page_Clear;

  if vm_Active and vm_Edit then
  if StrIComp(Path,vm_Path) <> 0 then begin

    stm:=TReadFile.Create;
    try
      if stm.Open(Path) then
      if stm.Size < MaxLongint then begin
        xSeek(vm_Stream,0); vm_Ind:=0;
        if vm_Stream.Write(stm.Buf,stm.Size,@ic) = s_Ok then
        Result:=ic; vm_Ind:=Result; vm_Activate;
      end;
    finally
      stm.Free
    end
  end
end;

function TVirtMem.vm_SaveAs(Path: PChar): PChar;
var
  stm: IStream; fn: TWideStr;
begin
  Result:=nil; page_Clear;

  if vm_Active then
  if StrIComp(Path,vm_Path) <> 0 then

  if StrToWideChar(Path,fn,255) <> nil then
  if DosStorage.CreateStream(fn,ole_Create,0,0,stm) = S_OK then
  begin
    vm_Stream.SetSize(vm_Ind);
    if xCopy(vm_Stream,stm) > 0 then
    Result:=Path
  end
end;

procedure TVirtMem.CopyTo(Dest: TVirtMem);
var
  Ind,Len: Integer; buf: TBytes;
begin
  if Dest.vm_Active then begin
    Dest.vm_Ind:=0; Dest.vm_Expand(vm_Ind);
    Ind:=0; while Ind < vm_Ind do begin
      Len:=Min(SizeOf(buf),vm_Ind-Ind);
      vm_Load(Ind,buf,Len);
      Ind:=Dest.vm_Store(Ind,buf,Len)
    end
  end
end;

function TVirtMem.vmx_Append(stg: TVirtMem; pos,len: int): int;
var
  bx,cx,dx: int; buf: TBytes;
begin
  Result:=0; dx:=vm_Ind;

  if len > 0 then
  if vm_Active then begin

    bx:=vm_Expand(len);
    if bx+len = vm_Ind then begin

      Result:=len;

      bx:=pos;
      while len > 0 do begin
        cx:=Min(SizeOf(buf),len);
        bx:=stg.vm_Load(bx,buf,cx);
        dx:=vm_Store(dx,buf,cx);
        Dec(len,cx)
      end
    end
  end
end;

function TVirtMem.vm_Domain(ind: longint): Boolean;
var
  blk1: vm_Len; blk2: vmd_Len;
begin
  Result:=true;
  if ind > 0 then begin

    Result:=false;

    if fvm_x32 then begin
      if ind < vm_Ind-8 then begin
        ind:=vm_Load(ind,blk2,8);
        if blk2.Len <= blk2.Size then
        Result:=ind+blk2.Size <= vm_Ind
      end
    end else
    if ind < vm_Ind-4 then begin
      ind:=vm_Load(ind,blk1,4);
      if blk1.Len <= blk1.Size then
      Result:=ind+blk1.Size <= vm_Ind
    end
  end
end;

procedure TVirtMem.vm_Push(buf: Pointer; len: Integer);
var
  pos: Integer;
begin
  if vm_Active then begin
    pos:=vm_Ind; vm_Append(len,4);
    if len > 0 then vm_Append(buf^,len);
    vm_Append(pos,4)
  end
end;

function TVirtMem.vm_Pop(buf: Pointer; size: Integer): Integer;
var
  pos,len: Integer;
begin
  Result:=0;
  if vm_Active then
  if vm_Ind > 32 then begin
    vm_Load(vm_Ind-4,pos,4);
    vm_Load(pos,len,4);

    if pos+len = vm_Ind-8 then begin

      if buf = nil then
        Result:=len
      else
      if len > 0 then
      if size > 0 then begin
        len:=Min(len,size);
        vm_Load(pos+4,buf^,len);
        Result:=len
      end;

      if buf <> nil then
      vm_Ind:=pos
    end
  end
end;

procedure TVirtMem.vm_Skip;
var
  pos,len: Integer;
begin
  if vm_Active then
  if vm_Ind > 32 then begin
    vm_Load(vm_Ind-4,pos,4);
    vm_Load(pos,len,4);

    if pos+len = vm_Ind-8 then
    vm_Ind:=pos
  end
end;

procedure TVirtMem.min_max_long(Ind,Count: Integer;
                                out v1,v2: Integer);
var
  i,k,n,v,a,b: Integer; buf: TIntegers;
begin
  a:=0; b:=0;

  if Count > 0 then begin
    Ind:=vm_Load(Ind,v,Sizeof(v));
    a:=v; b:=v; Dec(Count);

    k:=Sizeof(buf) div Sizeof(v);

    while Count > 0 do begin
      n:=Min(k,Count); Dec(Count,n);
      Ind:=vm_Load(Ind,buf,n * Sizeof(v));

      for i:=0 to n-1 do begin
        v:=buf[i]; a:=Min(a,v); b:=Max(b,v);
      end
    end
  end;

  v1:=a; v2:=b
end;

procedure TVirtMem.min_max_inc(Ind,Count: Integer;
                               out v1,v2: Integer);
var
  i,k,n,v,a,b: Integer;
  d: SMallint; buf: TSmallInts;
begin
  a:=0; b:=0; v:=0;

  if Count > 0 then begin
    k:=Sizeof(buf) div Sizeof(d);

    while Count > 0 do begin
      n:=Min(k,Count); Dec(Count,n);
      Ind:=vm_Load(Ind,buf,n * Sizeof(d));

      for i:=0 to n-1 do begin
        d:=buf[i]; Inc(v,d);
        a:=Min(a,v); b:=Max(b,v)
      end
    end
  end;

  v1:=a; v2:=b
end;

function TVirtMem.min_max_xy(Ind,Count: Integer;
                             out lt,rb: TPoint): Integer;
var
  i,k,n: Integer; p: PLPoly;
  v,a,b: TPoint; buf: TLPoly;
begin
  a.x:=0; a.y:=0; b:=a;

  if Count > 0 then begin
    Ind:=vm_Load(Ind,v,Sizeof(v));
    a:=v; b:=v; Dec(Count);

    k:=Sizeof(buf) div Sizeof(v);

    while Count > 0 do begin
      n:=Min(k,Count); Dec(Count,n);
      Ind:=vm_Load(Ind,buf,n * Sizeof(v));

      p:=@buf; for i:=1 to n do begin
        v:=p[0]; p:=@p[1];
        a.x:=Min(a.x,v.x); b.x:=Max(b.x,v.x);
        a.y:=Min(a.y,v.y); b.y:=Max(b.y,v.y);
      end
    end
  end;

  lt:=a; rb:=b; Result:=Ind
end;

function TVirtMem.min_max_dxy(Ind,Count: Integer;
                              out lt,rb: TPoint): Integer;
var
  i,k,n: Integer; p: PIPoly;
  a,b,v: TPoint; d: IPoint; buf: TIPoly;
begin
  a.x:=0; a.y:=0; b:=a; v:=a;

  if Count > 0 then begin
    k:=Sizeof(buf) div Sizeof(d);

    while Count > 0 do begin
      n:=Min(k,Count); Dec(Count,n);
      Ind:=vm_Load(Ind,buf,n * Sizeof(d));

      p:=@buf; for i:=1 to n do begin
        d:=p[0]; p:=@p[1];
        Inc(v.X,d.X); Inc(v.Y,d.Y);
        a.X:=Min(a.X,v.X); b.X:=Max(b.X,v.X);
        a.Y:=Min(a.Y,v.Y); b.Y:=Max(b.Y,v.Y);
      end
    end
  end;

  lt:=a; rb:=b; Result:=Ind
end;

constructor TReadStreamVM.Create(vm: TVirtMem; p: longint);
var
  len: vm_Len;
begin
  inherited Create;

  vm_mem:=vm; if p > 0 then
  if vm.vm_Active then begin
    vm_Adr:=vm.vm_Load(p,len,SizeOf(len));
    vm_Size:=Len.Len
  end
end;

function TReadStreamVM.Read(var Buffer; Count: Longint): Longint;
begin
  Result:=Min(Count,vm_Size-vm_Pos);

  if Result > 0 then begin

    if vm_mem.vm_Buffer <> nil then
      vm_mem.vm_Load(vm_Adr+vm_Pos,Buffer,Result)
    else
      vm_mem.vm_Stream.Read(@Buffer,Result,@Count);

    Inc(vm_Pos,Result)
  end
end;

function TReadStreamVM.Seek(Offset: Longint; Origin: Word): Longint;
var
  pos: Int64;
begin
  if vm_Size > 0 then

  case Origin of
soFromBeginning:
    vm_Pos:=Offset;

soFromCurrent:
    Inc(vm_Pos,Offset);

soFromEnd:
    vm_Pos:=vm_Size+Offset
  end;

  if vm_mem.vm_Buffer = nil then
  vm_mem.vm_Stream.Seek(vm_Adr+vm_Pos,0,Pos);

  Result:=vm_Pos
end;

constructor TCtrlMem.Create(mem: TVirtMem);
var
  len: Integer;
begin
  inherited Create;
  vm:=mem; vm_size:=mem.vm_Ind;

  len:=int_Round(vm_size,4);
  len:=((len div 4) + 7) div 8;
  lpbits:=xAllocPtr(len)
end;

destructor TCtrlMem.Destroy;
begin
  xFreePtr(lpbits); inherited
end;

function TCtrlMem.ctrl(p: Integer): Boolean;
const
  _bit: array[0..7] of Byte = (128,64,32,16,8,4,2,1);
var
  i,len: Integer;
  si: PBytes; bit: Byte;
begin
  Result:=true; if p > 0 then

  if p >= vm_size then
    Result:=true
  else
  if lpbits <> nil then begin

    Result:=false;

    if p < vm_size-4 then begin
      len:=vm.vm_BufSize(p);

      if p + len <= vm_size then begin

        Result:=true;

        p:=p div 4; len:=len div 4;

        si:=@lpbits[p div 8];
        bit:=_bit[p and 7]; p:=p div 8;

        for i:=1 to len do
        if si[0] and bit <> 0 then begin
          Result:=false; Break
        end
        else begin
          si[0]:=si[0] or bit;
          bit:=bit shr 1;

          if bit = 0 then begin
            si:=@si[1]; Inc(p); bit:=128
          end
        end;
      end
    end
  end
end;

end.
