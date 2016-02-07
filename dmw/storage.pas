unit storage; interface {$H-}

uses
  Windows,activeX,SysUtils,Math,
  Classes,otypes,ofiles,convert;

const
  ole_Read = STGM_READ + STGM_SHARE_EXCLUSIVE;
  ole_ReadWrite = STGM_READWRITE + STGM_SHARE_EXCLUSIVE;
  ole_Create = ole_ReadWrite + STGM_CREATE;
  ole_Temp = ole_Create + STGM_DELETEONRELEASE;

type
  TStorage = class
    function Create_new: hResult;
    function Load(ADoc: TStorage): Boolean;
    function Open(APath: PChar; rw: Boolean): Boolean;
    procedure Close;

    function SaveAs(APath: PChar): Boolean;

    function Create_doc: TStorage; virtual; abstract;
    function This_doc: Boolean; virtual;
    function Pack_doc: Boolean; virtual;
    procedure After_Load_doc; virtual;

    function Open_Stream(Name: PChar; rw: Boolean;
                         out stm: IStream): Boolean;

    function Create_Stream(Name: PChar; out stm: IStream): Boolean;
    function Delete_Element(Name: PChar): Boolean;

  private
    FDoc: IStorage;
    FEditCount: Integer;
    FEditPath: TShortStr;
    FOnChange: TEventProc;
    FIsTemp: Boolean;

    function Get_EditPath: PChar;
    procedure Set_EditPath(Path: PChar);

  public
    property Doc: IStorage read FDoc;

    property EditCount: Integer read FEditCount
                                write FEditCount;

    property OnChange: TEventProc read FOnChange
                                  write FOnChange;

    property IsTemp: Boolean read FIsTemp write FIsTemp;

    property EditPath: PChar read Get_EditPath
                             write Set_EditPath;
  end;

function xStgIsStorageFile(Path: PChar): Boolean;

function xStorageFlags(const stg: IStorage): Integer;

function xCreateStorage(Path: PChar;
                        out doc: IStorage): HResult;

function xOpenFolder(const owner: IStorage;
                     Name: PChar; Mode: Integer;
                     out stg: IStorage): Boolean;

function xCreateFolder(const owner: IStorage; Name: PChar;
                       out stg: IStorage): HResult;

function xOpenStorage(Path: PChar; Mode: uint;
                      out stg: IStorage): HResult;

function xCopyStorage(const stg: IStorage; Path: PChar): Boolean;

function xEnumFolders(stg: IStorage; Items: TStrings): Integer;
function xEnumStreams(stg: IStorage; Items: TStrings): Integer;

function xRewriteStream(stg: IStorage; Name: PChar): Boolean;
function xRewriteFolder(stg: IStorage; Name: PChar): Boolean;

function xContainsStream(stg: IStorage; Name: PChar): Boolean;
function xContainsStorage(stg: IStorage; Name: PChar): Boolean;

function sFolderDate(const stg: IStorage; Name: PChar): Double;

function xOpenStream(const stg: IStorage;
                     Name: PChar; rw: Boolean;
                     out sf: IStream): Boolean;

function xCreateStream(doc: IStorage; Name: PChar;
                       out stm: IStream): Boolean;

procedure xClearStorage(const stg: IStorage);

function xRenameElement(const stg: IStorage; old,new: PChar): Boolean;
function xDeleteElement(const stg: IStorage; Name: PChar): Boolean;
function xCopyElement(const dst,src: IStorage; Name,_Name: PChar): Boolean;
function xCopyFolder(const dst,src: IStorage): Integer;

function xCopyStream(const stg: IStorage; stm: IStream): Boolean;

function xSizeStream(const stg: IStorage; Name: PChar): Integer;
function xDateStream(const stg: IStorage; Name: PChar): Double;

function xLoadFromStream(const stg: IStorage; Name: PChar;
                         var buf; Len: Integer): Boolean;

function xGetFromStream(const stg: IStorage; Name: PChar;
                        var buf; Len: Integer): Boolean;

function StrFromStream(const stg: IStorage; Name: PChar;
                       Str: PChar; MaxLen: int): PChar;

function PathFromStream(const stg: IStorage;
                        Name,Path: PChar): PChar;

function DirFromStream(const stg: IStorage;
                        Name,Dir: PChar): PChar;

function xSaveAsStream(const stg: IStorage; Name: PChar;
                       buf: Pointer; len: Integer): int;

procedure xStrAsStream(const stg: IStorage; Name,Str: PChar);

function xFileToStream(const stg: IStorage; Name,Path: PChar): int;
function xStreamToFile(const stg: IStorage; Name,Path: PChar): int;

function xSize(const sf: IStream): Largeint;

function stm_FileName(const sf: IStream; fn: PChar): PChar;
function stg_FileName(const stg: IStorage; fn: PChar): PChar;

function xPos(const sf: IStream): LargeInt;
function xSeek(const sf: IStream; ofs: LargeInt): Boolean;
function xSeek_Bottom(const sf: IStream): LargeInt;

function xRead(const sf: IStream; var buf; len: longint): Boolean;
function xWrite(const sf: IStream; var buf; len: longint): Boolean;
function xAppend(const sf: IStream; var buf; len: longint): Boolean;

function xLoad(sf: IStream; ofs,len: longint; var buf): Boolean;
function xSave(sf: IStream; ofs,len: longint; var buf): Boolean;

function xReadStr(sf: IStream): string;
procedure xWriteStr(sf: IStream; s: string);

function xCopy(src,dst: IStream): Largeint;

implementation

uses
  xpole;

function xStgIsStorageFile(Path: PChar): Boolean;
var
  stg: IStorage;
begin
  Result:=false;
  if FileExist(Path) then begin
    Result:=PoleGetStorage(Path,ole_Read,stg);
    stg:=nil
  end
end;

function xStorageFlags(const stg: IStorage): Integer;
var
  statstg: TStatStg;
begin
  Result:=0;
  if stg.Stat(statstg,0) = s_OK then
  Result:=statstg.grfStateBits;
end;

function xCreateStorage(Path: PChar;
                        out doc: IStorage): HResult;
begin
  Result:=s_FALSE; doc:=nil;
end;

function xOpenFolder(const owner: IStorage;
                     Name: PChar; Mode: Integer;
                     out stg: IStorage): Boolean;
var
  fn: TWideStr;
begin
  StrToWideChar(Name,fn,255);
  Result:=owner.OpenStorage(fn,nil,Mode,nil,0,stg) = S_OK
end;

function xCreateFolder(const owner: IStorage; Name: PChar;
                       out stg: IStorage): HResult;
var
  fn: TWideStr;
begin
  StrToWideChar(Name,fn,255);
  Result:=owner.CreateStorage(fn,ole_CREATE,0,0,stg)
end;

function xRewriteStream(stg: IStorage; Name: PChar): Boolean;
var
  stm: IStream;
begin
  Result:=false;

  if not xContainsStream(stg,Name) then
  if xOpenStream(stg,Name,true,stm) then begin
    stm:=nil; xDeleteElement(stg,Name);
    Result:=true
  end
end;

function xRewriteFolder(stg: IStorage; Name: PChar): Boolean;
var
  fld: IStorage;
begin
  Result:=false;

  if not xContainsStorage(stg,Name) then
  if xCreateFolder(stg,Name,fld) = S_OK then begin
    fld:=nil; xDeleteElement(stg,Name);
    Result:=true
  end
end;

function xOpenStorage(Path: PChar; Mode: uint;
                      out stg: IStorage): HResult;
begin
  Result:=S_FALSE;
  if PoleGetStorage(Path,Mode,stg) then
  Result:=S_OK
end;

function xCopyStorage(const stg: IStorage; Path: PChar): Boolean;
var
  dst: IStorage;
begin
  Result:=false; if Assigned(stg) then
  if xCreateStorage(Path,dst) = s_OK then
  if stg.CopyTo(0,nil,nil,dst) = s_OK then
  Result:=true
end;

function xEnumFolders(stg: IStorage; Items: TStrings): Integer;
var
  enum: IEnumStatStg;
  statstg: TStatStg; ic: int;
begin
  Result:=0; Items.Clear;

  if stg.EnumElements(0,nil,0,enum) = s_OK then

  while enum.Next(1,statstg,@ic) = s_OK do
  with statstg do begin

    if dwType = stgty_STORAGE then
    Items.Add(WideCharToString(pwcsName));

  end;

  Result:=Items.Count
end;

function xEnumStreams(stg: IStorage; Items: TStrings): Integer;
var
  enum: IEnumStatStg;
  statstg: TStatStg; ic: int;
begin
  Result:=0;

  if Assigned(Items) then Items.Clear;

  if stg.EnumElements(0,nil,0,enum) = s_OK then

  while enum.Next(1,statstg,@ic) = s_OK do
  with statstg do begin

    if dwType = stgty_STREAM then begin
      if Assigned(Items) then
      Items.Add(WideCharToString(pwcsName));
      Inc(Result)
    end;

  end;
end;

function xContainsStream(stg: IStorage; Name: PChar): Boolean;
var
  stm: IStream; fn: TWideStr;
begin
  StrToWideChar(Name,fn,255);
  Result:=stg.OpenStream(fn,nil,ole_Read,0,stm) = S_OK
end;

function xContainsStorage(stg: IStorage; Name: PChar): Boolean;
var
  tmp: IStorage; fn: TWideStr;
begin
  StrToWideChar(Name,fn,255);
  Result:=stg.OpenStorage(fn,nil,ole_READ,nil,0,tmp) = S_OK;
  tmp:=nil
end;

function xFolderDate(const stg: IStorage): Double;
var
  stat: TStatStg;
begin
  Result:=0;
  if stg.Stat(stat,0) = s_OK then
  Result:=FileTimeToDateTime(stat.mtime);
end;

function sFolderDate(const stg: IStorage; Name: PChar): Double;
var
  fld: IStorage;
begin
  Result:=0; if Assigned(stg) then
  if xOpenFolder(stg,Name,OLE_READ,fld) then
  Result:=xFolderDate(fld); fld:=nil
end;

function xSize(const sf: IStream): Largeint;
var
  stat: TStatStg;
begin
  Result:=0;
  if sf.Stat(stat,0) = s_OK then
  Result:=stat.cbSize;
end;

function xDate(const sf: IStream): Double;
var
  stat: TStatStg;
begin
  Result:=0;
  if sf.Stat(stat,0) = s_OK then
  Result:=FileTimeToDateTime(stat.mtime);
end;

function stm_FileName(const sf: IStream; fn: PChar): PChar;
var
  stat: TStatStg;
  Path: POleStr;
begin
  Result:=nil; StrCopy(fn,'');

  if sf.Stat(stat,0) = s_OK then begin
    Path:=stat.pwcsName; if Assigned(Path) then
    Result:=StrPCopy(fn,WideCharToString(Path));
  end;
end;

function stg_FileName(const stg: IStorage; fn: PChar): PChar;
var
  stat: TStatStg; Path: POleStr;
begin
  Result:=nil; StrCopy(fn,'');

  if stg.Stat(stat,0) = s_OK then begin
    Path:=stat.pwcsName; if Assigned(Path) then
    Result:=StrPCopy(fn,WideCharToString(Path));
  end;
end;

function xOpenStream(const stg: IStorage;
                     Name: PChar; rw: Boolean;
                     out sf: IStream): Boolean;
var
  Mode,rc: Integer; fn: TWideStr;
begin
  Result:=false; Mode:=ole_Read;
  if rw then Mode:=ole_ReadWrite;
  StrToWideChar(Name,fn,255);

  rc:=stg.OpenStream(fn,nil,Mode,0,sf);

  if rc = s_OK then
    Result:=true
  else
  if rc = STG_E_FILENOTFOUND then
  if stg.CreateStream(fn,ole_CREATE,0,0,sf) = s_OK then
    Result:=true
end;

function xCreateStream(doc: IStorage; Name: PChar;
                       out stm: IStream): Boolean;
var
  fn: TWideStr;
begin
  Result:=false; StrToWideChar(Name,fn,255);
  if doc.CreateStream(fn,ole_CREATE,0,0,stm) = s_OK then
  Result:=true
end;

procedure xClearStorage(const stg: IStorage);
var
  items: TStringList;
  i: Integer; fn: TShortstr;
begin
  if Assigned(stg) then begin
    items:=TStringList.Create;
    try
      xEnumFolders(stg,Items);
      for i:=0 to Items.Count-1 do
      if StrPCopy(fn,Items[i]) <> nil then
      xDeleteElement(stg,fn);

      xEnumStreams(stg,Items);
      for i:=0 to Items.Count-1 do
      if StrPCopy(fn,Items[i]) <> nil then
      xDeleteElement(stg,fn);
    finally
      items.Free
    end
  end
end;

function xRenameElement(const stg: IStorage; old,new: PChar): Boolean;
var
  old_,new_: TWideStr;
begin
  Result:=false;

  if StrToWideChar(old,old_,255) <> nil then
  if StrToWideChar(new,new_,255) <> nil then

  Result:=stg.RenameElement(old_,new_) = S_OK
end;

function xDeleteElement(const stg: IStorage; Name: PChar): Boolean;
var
  fn: TWideStr;
begin
  Result:=false;

  if Assigned(stg) then begin
    StrToWideChar(Name,fn,255);
    Result:=stg.DestroyElement(fn) = s_OK
  end
end;

function xCopyElement(const dst,src: IStorage; Name,_Name: PChar): Boolean;
var
  rc: int; fn,_fn: TWideStr;
begin
  Result:=false;

  if Assigned(src) then
  if Assigned(dst) then begin
    StrToWideChar(Name,fn,255);
    StrToWideChar(Name,_fn,255);

    if Assigned(_Name) then
    StrToWideChar(_Name,_fn,255);

    dst.DestroyElement(_fn);

    rc:=src.MoveElementTo(fn,dst,_fn,STGMOVE_COPY);
    Result:=rc = S_OK
  end
end;

function xCopyFolder(const dst,src: IStorage): Integer;
var
  list: TStringList;
  i: int; nm: TShortstr;
begin
  Result:=0;

  list:=TStringList.Create;
  try
    xEnumStreams(src,list);

    for i:=0 to list.Count-1 do begin
      StrPCopy(nm,list[i]);
      if xCopyElement(dst,src,nm,nm) then
      Inc(Result)
    end
  finally
    list.Free
  end
end;

function xCopyStream(const stg: IStorage; stm: IStream): Boolean;
var
  dst: IStream; fn: TShortStr;
begin
  Result:=false;
  if Assigned(stm) then
  if stm_FileName(stm,fn) <> nil then
  if xCreateStream(stg,fn,dst) then
  Result:=xCopy(stm,dst) > 0 
end;

function xSizeStream(const stg: IStorage; Name: PChar): Integer;
var
  stm: IStream;
begin
  Result:=0; if Assigned(stg) then
  if xOpenStream(stg,Name,false,stm) then
  Result:=xSize(stm); stm:=nil
end;

function xDateStream(const stg: IStorage; Name: PChar): Double;
var
  stm: IStream;
begin
  Result:=0; if Assigned(stg) then
  if xOpenStream(stg,Name,false,stm) then begin
    Result:=xDate(stm); stm:=nil
  end
end;

function xLoadFromStream(const stg: IStorage; Name: PChar;
                         var buf; Len: Integer): Boolean;
var
  stm: IStream;
begin
  Result:=false;
  if Assigned(stg) then
  if xOpenStream(stg,Name,false,stm) then begin
    Result:=xRead(stm,buf,len); stm:=nil
  end
end;

function xGetFromStream(const stg: IStorage; Name: PChar;
                        var buf; Len: Integer): Boolean;
var
  stm: IStream;
begin
  Result:=false;
  if Assigned(stg) then
  if xOpenStream(stg,Name,false,stm) then begin

    if xSize(stm) = len then
    if xSeek(stm,0) then
    Result:=xRead(stm,buf,len);

    stm:=nil
  end
end;

function StrFromStream(const stg: IStorage; Name: PChar;
                       Str: PChar; MaxLen: int): PChar;
var
  stm: IStream; len,stm_size: int;
begin
  Result:=nil;

  if Assigned(Str) then StrCopy(Str,'');

  if Assigned(stg) then
  if xOpenStream(stg,Name,false,stm) then begin

    stm_size:=xSize(stm); xSeek(stm,0);

    if Assigned(Str) then begin

      if MaxLen = 0 then MaxLen:=255;

      len:=Min(stm_size,MaxLen);
      if len > 0 then begin
        xRead(stm,Str^,len);
        Str[len]:=#0; Result:=Str
      end

    end
    else begin
      Result:=xAllocPtr(stm_size+1);
      if Assigned(Result) then begin
        xRead(stm,Result^,stm_size);
        Result[stm_size]:=#0
      end
    end;

    stm:=nil
  end
end;

function PathFromStream(const stg: IStorage;
                        Name,Path: PChar): PChar;
var
  buf: TShortStr;
begin
  Result:=nil; StrCopy(Path,'');
  if StrFromStream(stg,Name,buf,SizeOf(buf)-1) <> nil then
  if FileExist(buf) then Result:=StrCopy(Path,buf)
end;

function DirFromStream(const stg: IStorage;
                        Name,Dir: PChar): PChar;
var
  buf: TShortStr;
begin
  Result:=nil; StrCopy(Dir,'');
  if StrFromStream(stg,Name,buf,SizeOf(buf)-1) <> nil then
  if Dir_Exists(buf) then Result:=StrCopy(Dir,buf)
end;

function xSaveAsStream(const stg: IStorage; Name: PChar;
                        buf: Pointer; len: Integer): int;
var
  stm: IStream;
begin
  Result:=-1;
  if Assigned(stg) then begin
    xDeleteElement(stg,Name);
    if (len > 0) and Assigned(buf) then
    if xCreateStream(stg,Name,stm) then begin
      xWrite(stm,buf^,len); Result:=len
    end; stm:=nil
  end
end;

procedure xStrAsStream(const stg: IStorage; Name,Str: PChar);
begin
  xSaveAsStream(stg,Name,Str,StrLen(Str))
end;

function xFileToStream(const stg: IStorage; Name,Path: PChar): int;
var
  vm: TReadfile; nm: TShortstr;
begin
  Result:=-1;
  if FileExist(Path) then begin

    vm:=TReadfile.Create;
    try
      if vm.Open(Path) then
      if vm.Size > 0 then begin

        StrCopy(nm,'');
        if Assigned(Name) then
        StrCopy(nm,Name);

        if Strlen(nm) = 0 then
        StrNameExt(nm,Path);

        if Strlen(nm) > 0 then
        Result:=xSaveAsStream(stg,nm,vm.Buf,vm.Size)
      end
    finally
      vm.Free
    end
  end
end;

function xStreamToFile(const stg: IStorage; Name,Path: PChar): int;
var
  buf: Pointer; len: int;
begin
  Result:=-1;

  len:=xSizeStream(stg,Name);
  if len > 0 then begin
    buf:=xAllocPtr(len);
    if Assigned(buf) then
    if xLoadFromStream(stg,Name,buf^,len) then
    if xFileDump(Path,buf^,len) then Result:=len;
    xFreeptr(buf)
  end
end;

function xPos(const sf: IStream): LargeInt;
var
  pos: LargeInt;
begin
  Result:=0;
  if sf.Seek(0,STREAM_SEEK_CUR,pos) = s_OK then
  Result:=pos
end;

function xSeek(const sf: IStream; ofs: LargeInt): Boolean;
var
  pos: LargeInt;
begin
  Result:=sf.Seek(ofs,0,pos) = s_OK
end;

function xSeek_Bottom(const sf: IStream): LargeInt;
var
  pos: LargeInt;
begin
  Result:=0;
  if sf.Seek(xSize(sf),0,pos) = s_Ok then
  Result:=pos
end;

function xRead(const sf: IStream; var buf; len: longint): Boolean;
var
  ic: longint;
begin
  Result:=false;
  if sf.Read(@buf,len,@ic) = s_Ok then
  if ic = len then Result:=true
end;

function xWrite(const sf: IStream; var buf; len: longint): Boolean;
var
  ic: longint;
begin
  Result:=false;
  if sf.Write(@buf,len,@ic) = s_Ok then
  if ic = len then Result:=true
end;

function xAppend(const sf: IStream; var buf; len: longint): Boolean;
begin
  xSeek_Bottom(sf);
  Result:=xWrite(sf,buf,len)
end;

function xLoad(sf: IStream; ofs,len: longint; var buf): Boolean;
begin
  Result:=xSeek(sf,ofs) and xRead(sf,buf,len)
end;

function xSave(sf: IStream; ofs,len: longint; var buf): Boolean;
begin
  Result:=xSeek(sf,ofs) and xWrite(sf,buf,len)
end;

function xReadStr(sf: IStream): string;
var
  len: byte;
begin
  Result:='';
  if xRead(sf,len,1) then begin
    Result[0]:=chr(len);
    xRead(sf,Result[1],len)
  end
end;

procedure xWriteStr(sf: IStream; s: string);
var
  len: Integer;
begin
  len:=length(s);
  xWrite(sf,s,len+1)
end;

function xCopy(src,dst: IStream): Largeint;
const
  buf_size = 4096*4;
var
  len,size: longint; buf: pbytes;
begin
  dst.SetSize(0);

  buf:=xAllocPtr(buf_size);
  if buf <> nil then begin

    size:=xSize(src); Result:=0;
    xSeek(src,0); xSeek(dst,0);

    while size > 0 do begin
      len:=Min(buf_size,size);
      if not xRead(src,buf^,len) then Break;
      if not xWrite(dst,buf^,len) then Break;
      Dec(size,len); Inc(Result,len)
    end;

    dst.SetSize(Result)
  end;

  xFreePtr(buf); Result:=xSize(dst)
end;

function TStorage.Open_Stream(Name: PChar; rw: Boolean;
                              out stm: IStream): Boolean;
begin
  Result:=xOpenStream(FDoc,Name,rw, stm)
end;

function TStorage.Create_Stream(Name: PChar; out stm: IStream): Boolean;
begin
  Result:=xCreateStream(FDoc,Name,stm)
end;

function TStorage.Delete_Element(Name: PChar): Boolean;
begin
  Result:=xDeleteElement(FDoc,Name)
end;

function TStorage.Create_new: hResult;
begin
  Result:=xCreateStorage(nil,FDoc)
end;

function TStorage.Load(ADoc: TStorage): Boolean;
begin
  Result:=false; Close;

  if Create_new = s_OK then
  if ADoc.Doc.CopyTo(0,nil,nil,FDoc) = s_OK then
  if This_doc then begin
    EditPath:=ADoc.EditPath;
    After_Load_doc;
    Result:=true
  end;

  if not Result then Close;
end;

function TStorage.Open(APath: PChar; rw: Boolean): Boolean;
var
  Mode: Integer;
begin
  Result:=false; Close; Mode:=ole_Read;
  if rw then Mode:=ole_ReadWrite;

  if xOpenStorage(APath,Mode,FDoc) = s_OK then
  if StrCopy(FEditPath,APath) <> nil then
  Result:=This_doc
end;

function TStorage.SaveAs(APath: PChar): Boolean;
var
  tmp: TStorage; fn: tShortStr;
begin
  Result:=false;

  StrCopy(fn,FEditPath); if APath <> nil then
  StrCopy(fn,APath); if StrLen(fn) > 0 then

  if FEditCount > 0 then begin

    tmp:=Create_doc;
    try
      if tmp.Load(Self) and tmp.Pack_doc then
        Result:=xCopyStorage(tmp.Doc,fn)
      else
        Result:=xCopyStorage(FDoc,fn);
    finally
      tmp.Free
    end;

  end else

  Result:=xCopyStorage(FDoc,fn);

  if Result then begin
    if APath <> nil then
    StrCopy(FEditPath,APath);
    FEditCount:=0
  end
end;

procedure TStorage.Close;
begin
  if Assigned(FDoc) then
  FDoc:=nil; FEditCount:=0;
  StrCopy(FEditPath,'')
end;

function TStorage.Get_EditPath: PChar;
begin
  Result:=nil;
  if StrLen(fEditPath) > 0 then
  Result:=fEditPath
end;

procedure TStorage.Set_EditPath(Path: PChar);
begin
  xStrCopy(fEditPath,Path)
end;

function TStorage.This_doc: Boolean;
begin
  Result:=true
end;

function TStorage.Pack_doc: Boolean;
begin
  Result:=true
end;

procedure TStorage.After_Load_doc;
begin
end;

end.