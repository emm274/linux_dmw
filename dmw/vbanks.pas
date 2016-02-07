unit vbanks; interface

uses
  SysUtils,Math,
  otypes,ofiles,xvirts;

type
  TVirtBank = class(TVirtMem)

    t_ind,t_max,t_len,t_cnt: Integer;
    EditFlag: Boolean;

    constructor Create(Cash,Ind,Max,Len: Integer);

    procedure New_Status;
    procedure Get_Status; virtual;
    procedure Put_Status;

    function Open(Path: PChar): Boolean;
    function Make(Path: PChar): Boolean;

    procedure Create_list;
    procedure Clear_list;

    function ext_Open(Path,Ext: PChar): Boolean;
    function ext_Make(Path,Ext: PChar): Boolean;
    function ext_Edit(Path,Ext: PChar): Boolean;
    function tmp_Make(Prefix: PChar): Boolean;

    procedure vm_Close; override;

    function EditRecord(i: Integer): Boolean;

    function Offset(i: Integer): longint;
    function GetRecord(i: Integer; out buf): longint; virtual;
    function PutRecord(i: Integer; var buf): longint;

    function Insert(i: Integer; var buf): Integer;
    function Delete(i: Integer): Boolean;
    procedure Swap(i,j: Integer);
  end;

  TIndBank = class(TVirtBank)
    Loc: record Ind,Pos: longint end;

    constructor Create(cash,max: Integer);
    procedure vm_Close; override;

    function Ind_Load(i: Integer): longint;
    function Ind_Delete(i: Integer): Boolean;
    procedure Ind_Dump;

    function Copy_buf(i: Integer; var buf: PBytes): Integer;

    function Get_buf_len(i: Integer): Integer;

    function Get_buf(i: Integer; var buf; size: Integer): Integer;
    procedure Update_buf(i: Integer; var buf; len: Integer);

    function Create_bank(max: Integer): TIndBank; virtual;

    function Ind_Copy(i: Integer; dst: TIndBank): Integer; virtual;
    function Ind_Paste(i: Integer; src: TIndBank): Integer; virtual;
    procedure Ind_More(i: Integer; dst: TIndBank); virtual;

    procedure Comp_Bank;
    procedure Compress(dst: TIndBank); virtual;
  end;

  TIndRead = class(TReadFile)
    constructor Create;
    function Open(Path: PChar): Boolean; override;
    procedure Close; override;

    function Item_Seek(Ind: Integer): Integer; virtual;

  private
    fStart: Integer;
    fIndexes: PIntegers;
    fCount: Integer;

  protected
    fItemBuf: PBytes;
    fItemLen: Integer;

  public
    property Count: Integer read fCount;

    property ItemBuf: PBytes read fItemBuf;
    property ItemLen: Integer read fItemLen;
  end;

implementation

constructor TVirtBank.Create(Cash,Ind,Max,Len: Integer);
begin
  inherited Create(Cash,Ind);
  t_ind:=Ind; t_max:=Max; t_len:=Len;
  t_cnt:=0; EditFlag:=false
end;

procedure TVirtBank.New_Status;
var
  n: Integer;
begin
  n:=0; vm_Store(t_ind,n,2);
  vm_Store(t_ind+2,t_max,2)
end;

procedure TVirtBank.Get_Status;
begin
  t_cnt:=0; if vm_Active then begin
    t_cnt:=vm_Word(t_ind);
    t_max:=vm_Word(t_ind+2);

    if Offset(t_max+1) > vm_Ind then begin
      t_max:=(vm_Ind - t_ind - t_len) div t_len;
      t_max:=Max(0,t_max)
    end
  end
end;

procedure TVirtBank.Put_Status;
begin
  vm_Store(t_ind,t_cnt,2)
end;

function TVirtBank.Open(Path: PChar): Boolean;
begin
  Result:=vm_Open(Path);
  Get_Status;
end;

function TVirtBank.Make(Path: PChar): Boolean;
begin
  Result:=vm_Make(path); Create_list
end;

procedure TVirtBank.Create_list;
begin
  if vm_Active then begin
    if t_max = 0 then t_max:=64;
    t_ind:=vm_Expand((t_max+1) * t_len);
    t_cnt:=0; New_Status
  end
end;

procedure TVirtBank.Clear_list;
begin
  vm_Startup; Create_list
end;

function TVirtBank.ext_Open(Path,Ext: PChar): Boolean;
var
  fn: TShortStr;
begin
  StrUpdateExt(fn,Path,Ext);
  Result:=Open(fn)
end;

function TVirtBank.ext_Make(Path,Ext: PChar): Boolean;
var
  fn: tShortStr;
begin
  Result:=Make(StrUpdateExt(fn,Path,Ext))
end;

function TVirtBank.ext_Edit(Path,Ext: PChar): Boolean;
begin
  vm_Edit:=true; Result:=ext_Open(Path,Ext);
  if not Result then Result:=ext_Make(Path,Ext)
end;

function TVirtBank.tmp_Make(Prefix: PChar): Boolean;
var
  fn: tShortStr;
begin
  vm_Close; Result:=false;
  if xStrTempFileName(fn,Prefix) <> nil then
  Result:=Make(fn)
end;

procedure TVirtBank.vm_Close;
begin
  if vm_Edit then Put_Status;
  inherited vm_Close
end;

function TVirtBank.Offset(i: Integer): longint;
begin
  Result:=i; Result:=t_ind + Result*t_len
end;

function TVirtBank.EditRecord(i: Integer): Boolean;
begin
  Result:=false;
  if (i > 0) and (i <= t_cnt) then
  Result:=vm_Update 
end;

function TVirtBank.GetRecord(i: Integer; out buf): longint;
begin
  Result:=Offset(i); vm_Load(Result,buf,t_len)
end;

function TVirtBank.PutRecord(i: Integer; var buf): longint;
begin
  Result:=0;
  if (i > 0) and (i <= t_max) then begin
    Result:=Offset(i); vm_Store(Result,buf,t_len);
    if i > t_cnt then t_cnt:=i; EditFlag:=true
  end
end;

function TVirtBank.Insert(i: Integer; var buf): Integer;
var
  ind: longint;
begin
  Result:=0; if vm_Active then
  if (t_cnt < t_max) or (t_max = 0) then begin

    ind:=Offset(t_cnt+2); if ind > vm_Ind then
    vm_Expand(ind-vm_Ind); if i > t_cnt then i:=0;
    if i <= 0 then i:=t_cnt; if i <= t_cnt then Inc(i);

    ind:=Offset(i); if i <= t_cnt then
    vm_Down(ind,ind+t_len,Offset(t_cnt+1)-ind);

    vm_Store(ind,buf,t_len); Inc(t_cnt);
    EditFlag:=true; Result:=i
  end
end;

function TVirtBank.Delete(i: Integer): Boolean;
var
  ind: longint;
begin
  Result:=false;

  if vm_Active then
  if (i > 0) and (i <= t_cnt) then begin

    if i < t_cnt then begin ind:=Offset(i);
      vm_Move(ind+t_len,ind,Offset(t_cnt)-ind)
    end; Dec(t_cnt); EditFlag:=true;

    Result:=true
  end
end;

procedure TVirtBank.Swap(i,j: Integer);
var
  rec1,rec2: array[0..255] of byte;
begin
  if vm_Active then
  if t_len <= SizeOf(rec1) then begin
    vm_Load(Offset(i),rec1,t_len);
    vm_Load(Offset(j),rec2,t_len);

    vm_Store(Offset(i),rec2,t_len);
    vm_Store(Offset(j),rec1,t_len);
    EditFlag:=true
  end
end;

constructor TIndBank.Create(cash,max: Integer);
begin
  inherited Create(cash,32,max,SizeOf(longint));
end;

procedure TIndBank.vm_Close;
begin
  Loc.Ind:=0; Loc.Pos:=0;
  inherited vm_Close;
end;

function TIndBank.Ind_Load(i: Integer): longint;
begin
  Loc.Pos:=0; Loc.Ind:=0;

  if vm_Active then
  if (i > 0) and (i <= t_cnt) then begin
    Loc.Pos:=t_ind+i*SizeOf(longint);
    Loc.Ind:=vm_Long(Loc.Pos);
  end;

  Result:=Loc.Ind
end;

function TIndBank.Ind_Delete(i: Integer): Boolean;
begin
  vm_FreeBuf(Ind_Load(i)); Loc.Ind:=0;
  Ind_Dump; Result:=Delete(i)
end;

procedure TIndBank.Ind_Dump;
begin
  if vm_Update then with Loc do
  vm_Store(Pos,Ind,SizeOf(longint))
end;

function TIndBank.Copy_buf(i: Integer; var buf: PBytes): Integer;
var
  len: Integer;
begin
  Result:=0; buf:=nil;

  if Ind_Load(i) > 4 then begin
    len:=vm_BufLen(Loc.Ind);

    buf:=xAllocPtr(len);
    if Assigned(buf) then begin
      vm_Load(Loc.Ind+4,buf^,len);
      Result:=len
    end
  end
end;

function TIndBank.Get_buf_len(i: Integer): Integer;
begin
  Result:=0; if Ind_Load(i) > 4 then
  Result:=vm_BufLen(Loc.Ind);
end;

function TIndBank.Get_buf(i: Integer; var buf; size: Integer): Integer;
var
  len: Integer;
begin
  Result:=0; if Ind_Load(i) > 4 then begin
    len:=vm_BufLen(Loc.Ind); if len <= size then begin
      vm_Load(Loc.Ind+4,buf,len); Result:=len
    end
  end
end;

procedure TIndBank.Update_buf(i: Integer; var buf; len: Integer);
begin
  if vm_Update then begin
    Ind_Load(i); if Loc.Pos > 0 then begin
      Loc.Ind:=vm_UpDateBuf(Loc.Ind,buf,len);
      vm_Store(Loc.Pos,Loc.Ind,SizeOf(longint))
    end
  end
end;

function TIndBank.Create_bank(max: Integer): TIndBank;
begin
  Result:=TIndBank.Create(0,max);
end;

function TIndBank.Ind_Copy(i: Integer; dst: TIndBank): Integer;
var
  ofs,ind: longint; cnt: Integer;
begin
  Result:=0;
  if vm_Active and dst.vm_Active then
  if (i > 0) and (i <= t_cnt) then begin

    GetRecord(i,ofs); cnt:=dst.t_cnt;

    ind:=0; if ofs > 0 then
    if dst.Insert(0,ind) > cnt then begin
      ind:=vm_CopyBuf(ofs,0,dst);
      dst.PutRecord(dst.t_cnt,ind);
      Result:=dst.t_cnt
    end
  end
end;

function TIndBank.Ind_Paste(i: Integer; src: TIndBank): Integer;
var
  ofs,ind: longint; cnt: Integer;
begin
  Result:=0;
  if vm_Active and src.vm_Active then
  if (i > 0) and (i <= src.t_cnt) then begin

    src.GetRecord(i,ofs); cnt:=t_cnt;

    if ofs > 0 then begin ind:=0;

      if t_cnt = t_max then Comp_Bank;

      if vm_Active then
      if t_cnt < t_max then
      if Insert(0,ind) > cnt then begin
        ind:=src.vm_CopyBuf(ofs,0,Self);
        PutRecord(t_cnt,ind);
        Result:=t_cnt
      end
    end
  end
end;

procedure TIndBank.Ind_More(i: Integer; dst: TIndBank);
begin
end;

procedure TIndBank.Comp_Bank;
var
  dst: TIndBank; i,ind,off: int;
  tmp: TShortstr;
begin
  if vm_Active then begin

    dst:=Create_bank(t_max+64);
    try
      StrWorkPath(tmp,'tmp');

      if dst.Make(tmp) then begin

        ind:=t_Ind+SizeOf(longint);

        for i:=1 to t_cnt do begin
          vm_Load(ind,off,SizeOf(longint));

          off:=vm_CopyBuf(off,0,dst);
          Ind_more(i,dst);

          dst.vm_Store(ind,off,SizeOf(longint));
          Inc(ind,SizeOf(longint));
          Inc(dst.t_cnt)
        end;

        Compress(dst);
        vm_Return(dst);
        Get_Status
      end;
    finally
      dst.Free
    end;
  end
end;

procedure TIndBank.Compress(dst: TIndBank);
begin
end;

constructor TIndRead.Create;
begin
  inherited Create; fStart:=32
end;

function TIndRead.Open(Path: PChar): Boolean;
var
  cx: Integer;
begin
  Result:=inherited Open(Path);

  if Result then begin
    cx:=Get_word(fStart); if cx > 0 then
    if fStart + (cx+1) * 4 < Size then begin
      fIndexes:=@Buf[fStart]; fCount:=cx
    end
  end;

  if fCount = 0 then Close
end;

procedure TIndRead.Close;
begin
  fItemBuf:=nil; fItemLen:=0;
  fIndexes:=nil; fCount:=0;
  inherited Close
end;

function TIndRead.Item_Seek(Ind: Integer): Integer;
var
  bx,cx: Integer;
begin
  fItemBuf:=nil; fItemLen:=0;

  if Ind > 0 then
  if Ind <= fCount then begin
    bx:=fIndexes[Ind];
    if bx > 0 then
    if bx + 4 < Size then begin
      cx:=Get_word(bx+2); Inc(bx,4);

      if cx > 0 then
      if bx + cx <= Size then begin
        fItemBuf:=@Buf[bx];
        fItemLen:=cx
      end
    end
  end;

  Result:=fItemLen
end;

end.