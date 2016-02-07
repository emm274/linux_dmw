unit oframes; interface

uses
  Windows,otypes;

type
  IFile = interface(IUnknown)
    ['{7847AB3F-03AC-4A95-B4E0-3A503CAB740F}']

    function OpenFile(Path: PChar): Boolean; stdcall;
    function UpdateFile(Path: PChar): Boolean; stdcall;
    procedure CloseFile; stdcall;

    function GetSize: Int64; stdcall;

    function IsData(pos: Int64; len: Integer): Boolean; stdcall;
    function Seek(pos: Int64; len: Integer): Pointer; stdcall;

    function Load(pos: Int64; var rec; len: Integer): Int64; stdcall;
    function Store(pos: Int64; var rec; len: Integer): Int64; stdcall;

    function Get_long(pos: Int64): Integer; stdcall;
    function Get_word(pos: Int64): Integer; stdcall;

    function Append(var rec; len: Integer): Int64; stdcall;
  end;

  TBufFile = class(TInterfacedObject,IFile)

    constructor Create(Abuf: Pointer; Asize: Integer);

    function OpenFile(Path: PChar): Boolean; stdcall;
    function UpdateFile(Path: PChar): Boolean; stdcall;
    procedure CloseFile; stdcall;

    function GetSize: Int64; stdcall;

    function IsData(pos: Int64; len: Integer): Boolean; stdcall;
    function Seek(pos: Int64; len: Integer): Pointer; stdcall;

    function Load(pos: Int64; var rec; len: Integer): Int64; stdcall;
    function Store(pos: Int64; var rec; len: Integer): Int64; stdcall;

    function Get_long(pos: Int64): Integer; stdcall;
    function Get_word(pos: Int64): Integer; stdcall;

    function Append(var rec; len: Integer): Int64; stdcall;

  private
    fPosition: Integer;
    fbuf: PBytes; fsize: Integer;

  public
    property Position: Integer read fPosition;  
  end;

  TLargeFile = class(TInterfacedObject,IFile)

    destructor Destroy; override;

    function OpenFile(Path: PChar): Boolean; stdcall;
    function UpdateFile(Path: PChar): Boolean; stdcall;
    procedure CloseFile; stdcall;

    function GetSize: Int64; stdcall;

    function IsData(pos: Int64; len: Integer): Boolean; stdcall;
    function Seek(pos: Int64; len: Integer): Pointer; stdcall;

    function Load(pos: Int64; var rec; len: Integer): Int64; stdcall;
    function Store(pos: Int64; var rec; len: Integer): Int64; stdcall;

    function Get_long(pos: Int64): Integer; stdcall;
    function Get_word(pos: Int64): Integer; stdcall;

    function Append(var rec; len: Integer): Int64; stdcall;

    function Open(Path: PChar): Boolean; virtual;
    function Update(Path: PChar): Boolean; virtual;
    procedure Close; virtual;

    function Read(var rec; len: Integer): Integer;

  protected
    function Is_known_data(Path: PChar): Boolean; virtual;
  private
    fHandle: Integer;
    fPosition: Int64;
    fSize: Int64;
    fIsUpdated: LongBool;

    frameBuf: PBytes;
    frameBuf1: PBytes;
    frameSize: Integer;
    framePos: Int64;

    function Get_Active: Boolean;

    function GetHandle(Path: PChar;
                       AccessMode: DWord;
                       ShareMode: DWord): Boolean;

  public
    property Active: Boolean read Get_Active;
    property Handle: Integer read fHandle;
    property Size: Int64 read fSize;
    property IsUpdated: LongBool read fIsUpdated;
  end;

procedure InitGlobalFrameBuf;

function GetOpenFileIntf(Path: PChar; var Obj): Boolean;
function GetUpdateFileIntf(Path: PChar; var Obj): Boolean;

implementation

uses
  Math,Sysutils,FileUtil,
  convert,ofiles;

const
  FrameMaxSize = 16 * 1024 * 1024;
  FrameHalfSize = FrameMaxSize div 2;

var
  GlobalFrameBuf: Pointer;

procedure InitGlobalFrameBuf;
begin
  if GlobalFrameBuf = nil then
  GlobalFrameBuf:=xAllocPtr(FrameMaxSize)
end;

constructor TBufFile.Create(Abuf: Pointer; Asize: Integer);
begin
  inherited Create;
  fbuf:=Abuf; fsize:=Asize
end;

function TBufFile.OpenFile(Path: PChar): Boolean;
begin
  Result:=false
end;

function TBufFile.UpdateFile(Path: PChar): Boolean;
begin
  Result:=false
end;

procedure TBufFile.CloseFile;
begin
end;

function TBufFile.GetSize: Int64;
begin
  Result:=fsize
end;

function TBufFile.IsData(pos: Int64; len: Integer): Boolean;
begin
  Result:=(pos >= 0) and (pos+len <= fsize)
end;

function TBufFile.Seek(pos: Int64; len: Integer): Pointer;
begin
  fPosition:=pos;
  Result:=nil; if IsData(pos,len) then
  Result:=@fbuf[pos]
end;

function TBufFile.Load(pos: Int64; var rec; len: Integer): Int64;
begin
  if IsData(pos,len) then
  Move(fbuf[pos],rec,len);
  Result:=pos+len
end;

function TBufFile.Store(pos: Int64; var rec; len: Integer): Int64;
begin
  Result:=-1
end;

function TBufFile.Get_long(pos: Int64): Integer;
var
  ax: Integer;
begin
  ax:=0; Load(pos,ax,4); Result:=ax
end;

function TBufFile.Get_word(pos: Int64): Integer;
var
  ax: Integer;
begin
  ax:=0; Load(pos,ax,2); Result:=ax
end;

function TBufFile.Append(var rec; len: Integer): Int64;
begin
  Result:=-1
end;

destructor TLargeFile.Destroy;
begin
  xFreePtr(frameBuf1);
  Close; inherited
end;

function TLargeFile.GetHandle(Path: PChar;
                              AccessMode: DWord;
                              ShareMode: DWord): Boolean;
begin
  Result:=false; Close;

  fSize:=winFileSize(Path);
  fPosition:=0; framePos:=fSize;

  fHandle:=FileOpen(Path, AccessMode or ShareMode);

  if fHandle > 0 then
  Result:=Is_known_data(Path);

  if not Result then Close
end;

function TLargeFile.OpenFile(Path: PChar): Boolean;
begin
  fIsUpdated:=false;
  Result:=GetHandle(Path,fmOpenRead,fmShareDenyWrite);
end;

function TLargeFile.UpdateFile(Path: PChar): Boolean;
begin
  fIsUpdated:=true;
  Result:=GetHandle(Path,fmOpenReadWrite,fmShareDenyWrite);
end;

procedure TLargeFile.CloseFile;
begin
  if fHandle <> 0 then FileClose(fHandle);
  fHandle:=0; fSize:=0; fPosition:=0;
end;

function TLargeFile.GetSize: Int64;
begin
  Result:=fSize
end;

function TLargeFile.Open(Path: PChar): Boolean;
begin
  Result:=OpenFile(Path)
end;

function TLargeFile.Update(Path: PChar): Boolean;
begin
  Result:=UpdateFile(Path)
end;

procedure TLargeFile.Close;
begin
  CloseFile;
end;

function TLargeFile.Get_Active: Boolean;
begin
  Result:=fHandle <> 0
end;

function TLargeFile.Is_known_data(Path: PChar): Boolean;
begin
  Result:=true
end;

function TLargeFile.IsData(pos: Int64; len: Integer): Boolean;
begin
  Result:=(pos >= 0) and (pos+len <= fSize)
end;

function TLargeFile.Seek(pos: Int64; len: Integer): Pointer;
begin
  Result:=nil;

  if pos >= 0 then
  if pos+len <= fSize then begin

    if frameBuf = nil then begin

      frameBuf:=GlobalFrameBuf;
      if frameBuf = nil then begin
        frameBuf1:=xAllocPtr(FrameMaxSize);
        frameBuf:=frameBuf1
      end;

      frameSize:=FrameMaxSize;
      framePos:=fSize
    end;

    if Assigned(frameBuf) then
    if len <= frameSize then begin

      if (pos < framePos)
      or (pos+len > framePos+frameSize) then begin
        framePos:=pos;

        if framePos + frameSize > fSize then
        framePos:=fSize - frameSize;

        if framePos >= 0 then
          Load(framePos,frameBuf^,frameSize)
        else begin
          framePos:=0;
          Load(0,frameBuf^,Min(frameSize,fSize))
        end
      end;

      Result:=@frameBuf[pos - framePos]
    end
  end
end;

function TLargeFile.Load(pos: Int64; var rec; len: Integer): Int64;
begin
  if (pos < 0) or (pos+len > fSize) then
    Fillchar(rec,len,0)
  else begin
    FileSeek(fHandle,pos,0);
    fPosition:=pos; Read(rec,len)
  end;

  fPosition:=pos + len;
  Result:=fPosition;
end;

function TLargeFile.Store(pos: Int64; var rec; len: Integer): Int64;
begin
  if (pos >= 0) and (pos+len <= fSize) then begin
    FileSeek(fHandle,pos,0);
    FileWrite(fHandle,rec,len)
  end;

  fPosition:=pos + len;
  Result:=fPosition
end;

function TLargeFile.Get_long(pos: Int64): Integer;
var
  ax: Integer;
begin
  ax:=0; Load(pos,ax,4); Result:=ax
end;

function TLargeFile.Get_word(pos: Int64): Integer;
var
  ax: Integer;
begin
  ax:=0; Load(pos,ax,2); Result:=ax
end;

function TLargeFile.Read(var rec; len: Integer): Integer;
var
  di: PBytes; cx,rc: DWord;
begin
  Result:=0; di:=@rec;
  while len > 0 do begin cx:=len;
    if cx > 64000 then cx:=64000;

    rc:=FileRead(fHandle,di[0],cx);
    if rc < 0 then begin
      Result:=-1; Break
    end;

    Inc(Result,rc); Inc(fPosition,rc);
    if rc < cx then Break;
    di:=@di[cx]; Dec(len,cx)
  end;
end;

function TLargeFile.Append(var rec; len: Integer): Int64;
begin
  Result:=FileSeek(fHandle,0,2);
  FileWrite(fHandle,rec,len);
  Inc(fSize,len)
end;

function GetOpenFileIntf(Path: PChar; var Obj): Boolean;
var
  large: TLargeFile;
begin
  Result:=false; TPointer(Obj):=0;

  large:=TLargeFile.Create;
  try
    if large.Open(Path) then
    if large.GetInterface(IFile,Obj) then
    begin large:=nil; Result:=true end;
  finally
    large.Free
  end;
end;

function GetUpdateFileIntf(Path: PChar; var Obj): Boolean;
var
  large: TLargeFile;
begin
  Result:=false; TPointer(Obj):=0;

  large:=TLargeFile.Create;
  try
    if large.Update(Path) then
    if large.GetInterface(IFile,Obj) then
    begin large:=nil; Result:=true end;
  finally
    large.Free
  end
end;

initialization
  GlobalFrameBuf:=nil;

finalization
  xFreePtr(GlobalFrameBuf);

end.
