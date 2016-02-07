unit img_glu; interface

uses
  Classes,LCLType,
  otypes,ofiles;

const
  cid_checked = true;
  cid_key = $8B450850;

type
  pcid_key = ^tcid_key;
  tcid_key = record key,a,b,c,d,map: DWord end;

type
  tzlb_Compress = function(const InBuf: Pointer; InBytes: Integer;
                           OutBuf: Pointer; OutBytes: Integer): Integer;
                           stdcall;

  tzlb_Decompress = function(const InBuf: Pointer; InBytes: Integer;
                             OutBuf: Pointer; OutBytes: Integer): Integer;
                             stdcall;

  tzlibCompress = function(const InBuf: Pointer; InBytes: Integer;
                           OutBuf: Pointer; OutBytes: Integer): Integer;
                           cdecl;

  tzlibDecompress = function(const InBuf: Pointer; InBytes: Integer;
                             OutBuf: Pointer; OutBytes: Integer): Integer;
                             cdecl;

  tzlb = class
    destructor Destroy; override;

    function xCompress(const InBuf: Pointer; InBytes: int;
                       OutBuf: Pointer; OutBytes: int): int;

    function xDecompress(const InBuf: Pointer; InBytes: int;
                         OutBuf: Pointer; OutBytes: int): Boolean;

    function CompressFile(Source,Dest: PChar;
                          user: PBytes; userSize: Integer): Boolean;

    function DecompressFile(Source,Dest: PChar;
                            user: PBytes; userSize: Integer): Boolean;

  private
    dll: THandle;

    fCompress: tzlb_Compress;
    fDecompress: tzlb_Decompress;

    finit: bool;
    fActive: bool;

    procedure load_dll;
    function GetActive: bool;

  public
    property Active: bool read GetActive;
  end;

var
  zlb: tzlb;

function get_cid: DWord;
function cid_to_key(cid: DWord): DWord;
function cid_verify(FName: PChar): Boolean;

function key_Packet(Id, a,b,c,d, map: DWord): DWord;
function key_UnPacket(Id, a,b,c,d, map, key: DWord): Dword;
function cid_UnPacket(a,b,c,d, map, key: DWord): Dword;

function key_Verify(key: DWord): Boolean;

function Decrypt(Key,Val: DWord): DWord;
function Encrypt(Key,Val: DWord): DWord;

procedure Decrypt_buf(buf: PIntegers; Count,Key: DWord);
procedure Encrypt_buf(buf: PIntegers; Count,Key: DWord);

procedure key_dump(FName: PChar;
                   a,b,c,d,map,key: DWord);

function key_load(FName: PChar): DWord;

function cid_Load(FName: PChar;
                  out a,b,c,d,map,key: DWord): Boolean;

implementation

uses
  dynlibs,
  Sysutils,
  zlib,img_x;

destructor tzlb.Destroy;
begin
  xFreeLibrary(dll);
  inherited
end;

procedure tzlb.load_dll;
begin
  if not finit then begin

    dll:=xLoadLibrary('dll_zlb.so');
    if dll >= 32 then begin
      fCompress:=GetProcAddress(dll,'zlb_Compress');
      fDecompress:=GetProcAddress(dll,'zlb_Decompress');

      fActive:=Assigned(fCompress) and Assigned(fDecompress)
    end;

    finit:=true
  end
end;

function tzlb.GetActive: bool;
begin
  load_dll; Result:=fActive
end;

function tzlb.xCompress(const InBuf: Pointer; InBytes: Integer;
                        OutBuf: Pointer; OutBytes: Integer): Integer;

function zlibCompress(InBuf: Pointer; InBytes: int;
             				  OutBuf: Pointer; OutBytes: int): int;
var
  rc,rc1: int; strm: z_stream;
begin
  rc:=0;
  Fillchar(strm,sizeOf(strm),0);

	strm.next_in  :=InBuf;
	strm.avail_in :=InBytes;
  strm.next_out :=OutBuf;
  strm.avail_out:=OutBytes;

	rc1:=deflateInit(strm,Z_DEFAULT_COMPRESSION);

	if rc1 = Z_OK then begin
		rc1:=deflate(strm,Z_FINISH);
		if rc1 = Z_STREAM_END then rc:=strm.total_out;
		deflateEnd(strm);
	end;

	Result:=rc;
end;


begin
  Result:=0; load_dll;

  if Assigned(fCompress) then
    Result:=fCompress(InBuf,InBytes, OutBuf,OutBytes)
end;

function tzlb.xDecompress(const InBuf: Pointer; InBytes: Integer;
                          OutBuf: Pointer; OutBytes: Integer): Boolean;

function zlibDecompress(InBuf: Pointer; InBytes: int;
   	     		OutBuf: Pointer; OutBytes: int): int;
var
  rc,rc1: int; strm: z_stream;
begin
  rc:=0;
  Fillchar(strm,sizeOf(strm),0);

  strm.next_in  :=InBuf;
  strm.avail_in :=InBytes;
  strm.next_out :=OutBuf;
  strm.avail_out:=OutBytes;

  rc1:=inflateInit(strm);
  if rc1 <> Z_OK then
    rc:=-1
  else begin
    rc1:=inflate(strm, Z_FINISH);
    if rc1 <> Z_STREAM_END then
      rc:=-2
	  else
		  rc:=strm.total_out;

	  rc1:=inflateEnd(strm);
    if (rc1 <> Z_OK) and (rc >= 0) then rc:=-3;
  end;

  Result:=rc
end;

begin
  Result:=false; load_dll;

  if Assigned(fDecompress) then
    Result:=fDecompress(InBuf,InBytes, OutBuf,OutBytes) = OutBytes
  else
    Result:=zlibDecompress(InBuf,InBytes, OutBuf,OutBytes) = OutBytes
end;

function tzlb.CompressFile(Source,Dest: PChar;
                           user: PBytes; userSize: Integer): Boolean;
var
  vm: TReadfile;
  si,di: PBytes;
  dst,sz,bx,cx,bsize: Integer;
begin
  Result:=false;

  if FileExist(Dest) then
  FileErase(Dest);

  bsize:=1024*64;
  si:=xAllocPtr(bsize*4);

  vm:=TReadfile.Create;
  try
    if Assigned(si) then
    if vm.Open(Source) then
    if vm.Size > 0 then begin
      sz:=vm.Size;
      if sz = vm.Size then begin
        dst:=FileCreate(Dest);
        if dst > 0 then begin

          if userSize > 0 then
          FileWrite(dst,user^,userSize);

          FileWrite(dst,sz,4);

          bx:=0; di:=@si[bsize];

          while bx < sz do begin
            cx:=bsize;
            if bx + cx > sz then begin
              Fillchar(si^,bsize,0);
              cx:=sz - bx;
            end;

            Move(vm.Buf[bx],si^,cx);
            Inc(bx,cx);

            cx:=xCompress(si,bsize,di,bsize*3);
            if cx > 0 then begin
              FileWrite(dst,cx,4);
              FileWrite(dst,di^,cx);
            end
          end;

          FileClose(dst); Result:=true
        end
      end;
    end;
  finally
    vm.Free
  end;

  xFreePtr(si)
end;

function tzlb.DecompressFile(Source,Dest: PChar;
                             user: PBytes; userSize: Integer): Boolean;
var
  vm: TReadfile; si,di: PBytes;
  dst,sz,sz1,sz2,bx,cx,dx,bsize: Integer;
begin
  Result:=false;

  if Assigned(Dest) then
  if FileExist(Dest) then
  FileErase(Dest);

  bsize:=1024*64;
  di:=xAllocPtr(bsize*4);

  vm:=TReadfile.Create;
  try
    if Assigned(di) then
    if vm.Open(Source) then
    if vm.Size > 0 then begin
      sz:=vm.Size;
      if sz = vm.Size then
      if sz > userSize+4 then begin

        bx:=userSize; if bx > 0 then
        Move(vm.Buf[0],user^,bx);

        Move(vm.Buf[bx],sz1,4); Inc(bx,4);

        if sz1 > 0 then

        if Dest = nil then begin

          while bx+4 < sz do begin
            Move(vm.Buf[bx],cx,4); Inc(bx,4);
            if bx+cx <= sz then begin
              si:=@vm.Buf[bx];
              if not xDecompress(si,cx,di,bsize) then
              begin bx:=0; Break end;
            end; Inc(bx,cx)
          end;
 
          Result:=bx = sz
        end
        else begin

          dst:=FileCreate(Dest); sz2:=0;

          if dst > 0 then begin

            while bx+4 < sz do begin
              Move(vm.Buf[bx],cx,4); Inc(bx,4);
              if bx+cx <= sz then begin
                si:=@vm.Buf[bx];
                if xDecompress(si,cx,di,bsize) then begin
                  dx:=bsize; if sz2+dx > sz1 then
                  dx:=sz1 - sz2; if dx > 0 then
                  FileWrite(dst,di^,dx);
                  Inc(sz2,dx)
                end
                else begin
                  bx:=0; Break
                end;

              end; Inc(bx,cx)
            end;

            FileClose(dst);

            if bx <> sz then
              FileErase(Dest)
            else
              Result:=true
          end
        end
      end;
    end;
  finally
    vm.Free
  end;

  xFreePtr(di)
end;

function get_cid: DWord;
begin
  Result:=cid_key xor $FFFFFFFF;
end;

function cid_to_key(cid: DWord): DWord;
var
  ax: Extended;
begin
  ax:=cid; ax:=Ln(ax);
  ax:=Frac(ax)*1E+8;
  Result:=Abs(Trunc(ax))
end;

function key_loop(id, a,b,c,d: Dword): DWord;
var
  i1,i240,i,ax: DWord;
begin
  i1:=1; i240:=240;

  ax:=(i1 shl (a mod 6)) xor
      (i1 shl ((a+b) mod 6)) xor
      (i1 shl ((c+b+a) mod 6)) xor
      (i1 shl ((c+d+a+b) mod 6));

  ax:=ax xor (i1 shl (6 + (a mod 27))) xor
      (i1 shl (6 + ((a+b) mod 27))) xor
      (i1 shl (6 + (c mod 27))) xor
      (i1 shl (6 + ((c+d) mod 27)));

  ax:=(id xor ax xor ((id and 240) shr 2)) and 31;

  for i:=1 to 14 do
  ax:=ax xor ((id and (i240 shl (i+i))) shr (2*(i+1))) and 31;

  Result:=ax
end;

function key_Packet(id, a,b,c,d, map: DWord): DWord;
var
  ax,al,i,n,l,k: Dword;
begin
  l:=$80000000; k:=cid_key;

  ax:=(k + a*b + c*d + map);

  n:=key_loop(id, a,b,c,d);

  for i:=0 to n-1 do begin

    if ax xor l < ax then al:=1
                     else al:=0;

    ax:=(ax shl 1) + al;
  end;

  Result:=ax
end;

function key_UnPacket(Id, a,b,c,d, map, key: DWord): Dword;
var
  i,n,ax,al,l: Dword;
begin
  l:=$80000000;

  n:=key_loop(id, a,b,c,d);

  ax:=key; n:=n mod 32;

  for i:=0 to n-1 do begin
    al:=ax and 1; ax:=ax shr 1;
    ax:=ax xor (al*l)
  end;

  Result:=ax -a*b - c*d - map
end;

function cid_UnPacket(a,b,c,d, map, key: DWord): Dword;
begin
  Result:=key_UnPacket(get_cid, a,b,c,d, map, key)
end;

function key_Verify(key: DWord): Boolean;
begin
  Result:=key = cid_key
end;

function Decrypt(Key,Val: Dword): DWord;
var
  i,j: Longint; c: tlong;
begin
  c.c:=Val;

  for i:=31 downto 0 do begin j:=i and 3;

    if Key and 1 > 0 then
      c.b[j]:=c.b[j] xor c.b[(i+1) and 3]
    else
      c.b[j]:=c.b[j] xor (Key and $FF);

    Key:=Key shr 1
  end;

  Result:=c.c
end;

function Encrypt(Key,Val: Dword): DWord;
var
  i,j,Kmod: Longint; c: tlong;
begin
  c.c:=Val; Kmod:=Key;

  for i:=0 to 31 do begin j:=i and 3;

    if Kmod < 0 then
      c.b[j]:=c.b[j] xor c.b[(i+1) and 3]
    else
      c.b[j]:=c.b[j] xor ((Key shr (31-i)) and $FF);

    Kmod:=Kmod shl 1
  end;

  Result:=c.c
end;

procedure Decrypt_buf(buf: PIntegers; Count,Key: DWord);
var
  i: Integer;
begin
  for i:=1 to Count do begin
    buf[0]:=Decrypt(Key,buf[0]); buf:=@buf[1]
  end
end;

procedure Encrypt_buf(buf: PIntegers; Count,Key: DWord);
var
  i: Integer;
begin
  for i:=1 to Count do begin
    buf[0]:=Encrypt(Key,buf[0]); buf:=@buf[1]
  end
end;

procedure key_dump(FName: PChar;
                   a,b,c,d,map,key: DWord);
var
  txt: TTextfile;
begin
  txt:=TTextfile.Create;
  try
    if txt.Make_bin(FName) then begin
      txt.WriteInt(key);
      txt.WriteInt(a);
      txt.WriteInt(b);
      txt.WriteInt(c);
      txt.WriteInt(d);
      txt.WriteInt(map);
    end;
  finally
    txt.Free
  end
end;

function cid_Load(FName: PChar;
                  out a,b,c,d,map,key: DWord): Boolean;
var
  txt: TTextfile;
begin
  Result:=false;

  a:=0; b:=0; c:=0; d:=0; map:=0; key:=0;

  txt:=TTextfile.Create;
  try
    if txt.Open_bin(FName) then

    if txt.xStrLine <> nil then
    if txt.x_dword(key) then

    if txt.xStrLine <> nil then
    if txt.x_dword(a) then

    if txt.xStrLine <> nil then
    if txt.x_dword(b) then

    if txt.xStrLine <> nil then
    if txt.x_dword(c) then

    if txt.xStrLine <> nil then
    if txt.x_dword(d) then

    if txt.xStrLine <> nil then
    if txt.x_dword(map) then

    Result:=true
  finally
    txt.Free
  end
end;

function key_load(FName: PChar): DWord;
var
  a,b,c,d,map,key: DWord;
begin
  Result:=0;
  if cid_Load(FName, a,b,c,d,map,key) then
  Result:=key_UnPacket(get_cid, a,b,c,d, map, key);
end;

function cid_verify(FName: PChar): Boolean;
begin
  Result:=true; if cid_checked then
  Result:=key_Verify(key_load(FName))
end;

initialization
  zlb:=tzlb.Create;

finalization
  zlb.Free;

end.
