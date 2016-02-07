unit dmw_bln; interface {$H-}

uses
  Classes,
  otypes,vbanks,xlist;
                                                         
const
  Abc_Max = 128;

type
  Id_Tag = (_byte,_word,_int,_long,_time,
            _date,_float,_real,_angle,
            _string,_dBase,_enum,_bool,
            _link,_double,_unicode,_list,
            _text,_unk,_color,_label);

  Id_Set = set of Id_Tag;

const
  Id_DBase: Id_Set = [_byte,_word,_int,_long,_dBase];

  Id_Int: Id_Set = [_byte,_word,_int,_long,_time,_date,
                    _dBase,_enum,_bool,_link,_color];

  Id_Real: Id_Set = [_float,_real,_angle,_double];

  Id_Str: Id_Set = [_string,_unicode,_list,_text];

  Id_Len: array[Id_Tag] of Integer =
  (1,2,2,4,4,4,4,4,4,0,2,2,1,4,8,0,0,0,1,4,0);

  mif_len: array[Id_Tag] of Integer =
  (3,5,6,10,8,8,12,12,12,64,5,5,2,6,12,64,64,64,0,12,0);

  mif_dec: array[Id_Tag] of Integer =
  (0,0,0,0,0,0,2,2,4,0,0,0,0,0,8,0,0,0,0,0,0);

  cTag: array[ID_Tag] of alfa =
    ('byte    ','word    ','int     ',
     'long    ','time    ','date    ',
     'float   ','single  ','angle   ',
     'string  ','dbase   ','enum    ',
     'logic   ','link    ','double  ',
     'unicode ','list    ','text    ',
     'unknown ','color   ','label   ');

  UnicodeMaxLen: Integer = 0;

  info_comp_uppercase: Longbool = false;

type
  PHF_Tags = ^THF_Tags;
  THF_Tags = array[0..1023] of Id_Tag;

  TBlnHdr = record w,h,n: word end;

  PAbc_Rec = ^Abc_Rec; Abc_Rec = record
    nnn: word; id: Id_Tag; what,flags,decs: byte; alt: Id_Tag;
    ix,iy: SmallInt; TxtP: longint; iw,ih: SmallInt; TxtL: word;
    col,tab: byte; MinVal: Single; MsgP: longint; okay: Boolean;
    ii: longint; rr: Single; MaxVal: Single; indx: word;
  end;

  PAbcTbl = ^TAbcTbl;
  TAbcTbl = array[1..Abc_Max] of Abc_Rec;

  TBlnBank = class(TIndBank)

    procedure vm_Begin; override;

    function IsBlank(i: Integer): boolean;
    function Hdr_Load(i: Integer; out hdr: TBlnHdr): longint;

    function Def_HF(bln,nnn: Integer; id: Id_Tag; var buf): boolean;

    function Get_std_hf(bln: Integer; List: PIntegers;
                        Capacity: Integer): Integer;

    function Get_std_list(bln: Integer; List: TList;
                          tags: PHF_Tags): Integer;

    function Get_Blank(bln: int; lp: PWLine; lp_max: int): int;
    function Ord_Blank(bln: int; lp: PLLine; lp_max: int): int;

    procedure Def_Blank(bln: int; lp: PLLine; lp_max: int);

    function Def_IValue(bln,def,nn: Integer): Integer;

    function EnumStr(bln,nn,iv: Integer): String;

    procedure ver_record(var Rec: Abc_Rec);

    procedure Update_Version;

  private
    fVersion: Integer;
  end;

  PDbaseRec = ^TDbaseRec;
  TDbaseRec = record
    Id,Col: Longint; Name: TShortstr
  end;

  TDBaseList = class(TCustomList)
    constructor Create;
  end;

  thfStream = class(TMemoryStream)
    constructor Create(Ax16: longbool);
    destructor Destroy; override;

    function xread(out nn: int; out id: Id_Tag;
                   out val; MaxChars: Integer): Longbool;

    function xwrite(nn: int; id: Id_Tag;
                    const val; rw: Longbool): Longbool;

  private
    fx16: longbool;
    fText: PWideChar;
    fTextBuf: PWideChar;
  public
    property x16: longbool read fx16 write fx16;
    property Text: PWideChar read fText;
  end;

  TInfoStream = class(thfStream)
    function Get_HF(n: int; id: Id_Tag; var equ): Boolean;
    procedure Put_HF(n: int; id: Id_Tag; const equ);
    procedure Del_HF(n: int; id: Id_Tag);

    function xGet_int(n: int; out v: Integer): Boolean;
    function xGet_real(n: int; out v: Double): Boolean;

    function Get_int(n: int): Integer;
    function Get_real(n: int): double;

    procedure Put_str(n: int; s: string);

  private
    fBuffer: TUnicode;
  end;

  TInfoList = class
    constructor Create;
    destructor Destroy; override;

    function LoadFrom(buf: thfStream): Integer;
    function SaveTo(buf: thfStream): Integer;

    function GetPool(buf: PBytes; bufSize: Integer): Integer;

    procedure Clear;

    function Get_hf(ind: Integer;
                    out nn: Integer; out id: Id_Tag;
                    var info; MaxChars: Integer): Boolean;

    function Get_id(ind: Integer;
                    out nn: Integer;
                    out id: Id_Tag): Boolean;

    function Get_equ(n: Integer; tag: Id_Tag;
                     out equ; pop: Boolean): Boolean;

    function Pop_equ(n: Integer; tag: Id_Tag;
                     out equ; out unk: Boolean): Boolean;

    function Get_val(n: Integer; id: Id_Tag): string;

    procedure Update_nn(ind,nn: Integer);

    function Push_equ(n: Integer; id: Id_Tag;
                      const info): Boolean;

    function Get_string(n: Integer; pop: Boolean): WideString;
    function Pop_string(n: Integer; out unk: Boolean): WideString;

    function push_string(n: Integer; const v: WideString): Boolean;
    function push_text(n: Integer; const s: WideString): Boolean;
    function push_str(n: Integer; s: PChar): Boolean;

    function AsInteger(i: Integer): Integer;
    function AsFloat(i: Integer): Double;
    function AsString(i: Integer): WideString;

    procedure Update_int(i: Integer; eq: Integer);
    procedure Update_real(i: Integer; eq: Double);
    procedure Update_string(i: Integer; eq: WideString);

    function xUpdate_int(nn: Integer; eq: Integer): Integer;

    function Delete(i: Integer): Boolean;
    procedure Delete_nn(n: Integer);

    function Send(Msg: PChar): PChar;
    procedure Receive(msg: PChar);

    procedure CopyTo(dst: TInfoList; nnn: Integer);

    function Contains_str(n: Integer; eq: string): Integer;
    function Contains_nn(nn: int): int;

    function Typeof(nn: Integer): Integer;

    function Is_unk(nn: Integer): Boolean;

    function xGet_int(n: Integer;
                      out v: Integer): Boolean;

    function xGet_real(n: Integer;
                       out v: Double): Boolean;

    function xGet_str(n: Integer; s: PChar): Boolean;
    function xStr_val(n: Integer; s: PChar): Boolean;

    function Get_int(n,def: Integer): Integer;
    function Get_real(n: Integer; def: Double): Double;

    function Get_str(n: Integer; def: string): string;

    procedure Add_equ(n: int; id: Id_Tag; eq: String);

    function Add_info(hf: TInfoList): Integer;

  private
    List: TIntegerList;
    mem: thfStream;
    tmem: thfStream;

    fx16: longbool;
    fIs_Edit: longbool;
    fIs_Empty: longbool;
    fIs_unk: longbool;

    fMaxStrLen: int;

    fBuffer: TUnicode;

    procedure Set_x16(v: longbool);

    function Get_Count: Integer;

    function Get_ansi_Count: Integer;
    function Get_dos_Count: Integer;

  public
    property Is_Edit: Longbool read fIs_Edit write fIs_Edit;
    property Is_Empty: Longbool read fIs_Empty write fIs_Empty;

    property Count: Integer read Get_Count;

    property ansi_Count: Integer read Get_ansi_Count;
    property dos_Count: Integer read Get_dos_Count;

    property x16: longbool read fx16
                           write Set_x16;

    property MaxStrLen: int write fMaxStrLen;

    property Stream: thfStream read Mem;
  end;

function IdStr(Id: Id_Tag): string;
function real_align(v: Double; id: id_tag): Double;

function Id_Equal(itag,otag: Id_Tag): boolean;
function IdToStr(id: Id_Tag; info: PChar): string;

function info_into_range(id: Id_Tag; const info;
                         min,max: Single): Boolean;

function info_verify(stm: thfStream): Integer;

function Get_equ(stm: thfStream;
                 nn: int; id: Id_Tag;
                 ansi: int; out equ): Boolean;

function Get_hint(stm: thfStream; n: Integer;
                  s: PChar; smax: int): bool;

function Get_equ_nn(stm: thfStream; nn: Integer;
                    out id: Id_Tag; out info): Boolean;

function info_Comp(const info1,info2; id1,id2: Id_Tag): Boolean;

function info_Comp_hf(hf1,hf2: thfStream): Boolean;

function info_Comp_nn(hf1,hf2: thfStream;
                      nn1,nn2: Integer): Boolean;

function info_hash_nn(hf: thfStream; nn: Integer): Integer;

function Info_AsString(id: Id_Tag; const info): WideString;

function hf_transit(id,id_: Id_Tag; eq: OTypes.PString): Id_Tag;

function enum_Abc_to_set(const f: Abc_Rec): int64;
function enum_Abc_to_set_str(const f: Abc_Rec): String;

procedure enum_set_to_Abc(var f: Abc_Rec; bits: int64);
procedure enum_str_set_to_Abc(var f: Abc_Rec; str: PChar);

implementation

uses
  SysUtils,convert,wstrings;

type
  PValue = ^TValue;
  TValue = record case integer of
0: (x: TUnicode);
1: (s: ShortString);
3: (d: Double);
4: (f: single);
5: (l: Longint);
6: (i: SmallInt);
7: (w: Word);
8: (b: Byte);
  end;

function IdStr(Id: Id_Tag): string;
begin
  Result:=sVal(cTag[id],SizeOf(cTag[id]))
end;

function real_align(v: Double; id: id_tag): Double;
var
  cx,d,i: Integer;
begin
  Result:=v;
  if id in [_float,_real,_angle] then
  if Abs(v) < MaxLongint then begin
    d:=1; cx:=Decimals(Round(Abs(v)));
    for i:=cx+1 to 7 do d:=d * 10;
    Result:=Round(v*d)/d
  end
end;

procedure TBlnBank.vm_Begin;
var
  ver: char4;
begin
  fVersion:=0; if vm_Active then begin
    vm_Load(28,ver,4); if ver[1] = '#' then
    fVersion:=AtoI(ver[2],3)
  end
end;

procedure TBlnBank.ver_record(var Rec: Abc_Rec);
begin
  if fVersion = 0 then begin
    Rec.MinVal:=0; Rec.MaxVal:=0;
  end else
  if fVersion = 1 then begin
    Rec.MinVal:=PLongint(@Rec.MinVal)^;
    Rec.MaxVal:=PLongint(@Rec.MinVal)^;
  end
end;

function TBlnBank.isBlank(i: Integer): boolean;
var
  hdr: TBlnHdr;
begin
  Result:=false; if vm_Back then
  if (i > 0) and (i <= t_cnt) then
  if Hdr_Load(i,hdr) > 0 then
  if hdr.n > 0 then Result:=true
end;

function TBlnBank.Hdr_Load(i: Integer; out hdr: TBlnHdr): longint;
begin
  FillChar(hdr,SizeOf(hdr),0);
  Result:=0; if Ind_Load(i) > 0 then
  Result:=vm_Load(Loc.Ind+4,hdr,SizeOf(hdr))
end;

function TBlnBank.Def_HF(bln,nnn: Integer; id: Id_Tag; var buf): boolean;
var
  hdr: TBlnHdr;
  ind, len: Integer;
  rec: Abc_Rec;

begin
  Result:=false; len:=Id_Len[id];

  if id < _string then if vm_Active then
  if (bln > 0) and (bln <= t_cnt) then begin

    FillChar(buf,len,0); ind:=Hdr_Load(bln,hdr);

    while hdr.n > 0 do begin
      ind:=vm_Load(ind,rec,SizeOf(rec)); Dec(hdr.n);

      if rec.what = 0 then
      if rec.nnn = nnn then
      if rec.id = id then begin

        case id of
      _byte,
      _bool,
      _word,
      _int,
      _long,
      _time,
      _date:
          Move(rec.ii,buf,len);

      _float,
      _real,
      _angle:
          Move(rec.rr,buf,len);
        end;

        Result:=true; Break
      end
    end
  end
end;

function TBlnBank.Get_std_hf(bln: Integer; List: PIntegers;
                             Capacity: Integer): Integer;
var
  i,ind: Integer; hdr: TBlnHdr; Rec: ABC_Rec;
begin
  Result:=0;

  ind:=Hdr_Load(bln,hdr);

  if ind > 0 then
  if Capacity > 0 then

  for i:=1 to hdr.n do begin
    ind:=vm_Load(ind,Rec,SizeOf(Rec));

    if Rec.What = 0 then begin
      if Rec.flags and 1 = 1 then begin
        List[Result]:=Rec.nnn; Inc(Result);
        if Result >= Capacity then Break
      end
    end
  end;
end;

function TBlnBank.Get_std_list(bln: Integer; List: TList;
                               tags: PHF_Tags): Integer;
var
  i,ind: Integer; hdr: TBlnHdr; Rec: ABC_Rec;
begin
  List.Clear;

  if Assigned(tags) then
  FillChar(tags^,SizeOf(tHF_Tags),Char(_label));

  ind:=Hdr_Load(bln,hdr);

  if ind > 0 then

  for i:=1 to hdr.n do begin
    ind:=vm_Load(ind,Rec,SizeOf(Rec));

    if Rec.What = 0 then begin

      if Rec.nnn >= 0 then
      if Rec.nnn < 1024 then
      if Assigned(tags) then
      tags[Rec.nnn]:=Rec.id;

      if Rec.nnn < 1000 then
      if Rec.flags and 1 = 1 then
      List.Add(Pointer(Rec.nnn))
    end
  end;

  Result:=List.Count
end;

function TBlnBank.Get_Blank(bln: int; lp: PWLine; lp_max: int): int;
var
  i,ind: int; hdr: TBlnHdr; Rec: ABC_Rec;
begin
  lp.N:=-1;

  ind:=Hdr_Load(bln,hdr);

  if ind > 0 then
  for i:=1 to hdr.n do begin
    ind:=vm_Load(ind,Rec,SizeOf(Rec));

    if Rec.What = 0 then begin
      Inc(lp.N); with lp.Pol[lp.N] do
      begin x:=Rec.nnn; y:=ord(Rec.id) end;

      if lp.N = lp_max-1 then
      if lp_max > 0 then Break
    end
  end;

  Result:=lp.N+1
end;

function TBlnBank.Ord_Blank(bln: int; lp: PLLine; lp_max: int): int;
var
  i,j,ind: int;
  hdr: TBlnHdr; Rec: ABC_Rec;
  p1,p2: TPoint;
begin
  lp.N:=-1;

  ind:=Hdr_Load(bln,hdr);

  if ind > 0 then
  for i:=1 to hdr.n do begin
    ind:=vm_Load(ind,Rec,SizeOf(Rec));

    if Rec.What = 0 then begin
      Inc(lp.N);
      with lp.Pol[lp.N] do begin
        x:=(Rec.nnn shl 8) or ord(Rec.id);
        y:=Rec.tab
      end;

      if lp.N = lp_max-1 then
      if lp_max > 0 then Break
    end
  end;

  for i:=0 to lp.N-1 do begin
    p1:=lp.Pol[i];
    for j:=i+1 to lp.N do begin
      p2:=lp.Pol[j];
      if p1.y > p2.y then begin
        lp.Pol[i]:=p2;
        lp.Pol[j]:=p1; p1:=p2
      end
    end
  end;

  for i:=0 to lp.N do
  with lp.Pol[i] do begin
    y:=x and $FF; x:=x shr 8
  end;

  Result:=lp.N+1
end;

procedure TBlnBank.Def_Blank(bln: int; lp: PLLine; lp_max: int);
var
  i,ind: int; hdr: TBlnHdr; Rec: ABC_Rec; p: TPoint;
begin
  lp.N:=-1; if vm_Active then
  if (bln > 0) and (bln <= t_cnt) then begin

    ind:=Hdr_Load(bln,hdr);

    for i:=1 to hdr.n do begin
      ind:=vm_Load(ind,Rec,SizeOf(Rec));

      if Rec.What = 0 then
      if (Rec.id in Id_Int) and (Rec.ii <> 0)
      or (Rec.id in Id_Real) and (Rec.rr <> 0) then

      begin

        p.X:=Rec.nnn;
        p.X:=(p.X shl 16) + ord(Rec.id);

        if Rec.id in Id_Int then p.Y:=Rec.ii
        else tlong(p.Y).f:=Rec.rr;

        Inc(lp.N); lp.Pol[lp.N]:=p;

        if lp.N = lp_max-1 then
        if lp_max > 0 then Break
      end
    end
  end
end;

function TBlnBank.Def_IValue(bln,def,nn: Integer): Integer;

function def_enum(def,ofs: Integer;
                  const Rec: Abc_Rec): Integer;
var
  buf: PBytes; len,pos: Integer;
  s: String; v,rc: Integer;
begin
  Result:=def;

  buf:=nil; len:=Rec.TxtL;

  if len > 0 then
  if Rec.TxtP > 0 then begin
    buf:=xGetMem(len);
    if buf <> nil then begin
      vm_Load(ofs,buf^,len);

      pos:=0; while pos < len do begin
        pos:=ReadPool(buf,pos,len,s);

        if def = 0 then begin

          if length(s) > 0 then begin
            val(s,v,rc); if rc = 0 then
            Result:=v
          end;

          Break
        end;

        Dec(def)
      end
    end;
  end
end;

var
  i,ind,len,ofs: Integer;
  hdr: TBlnHdr; Rec: ABC_Rec;
  s: String;
begin
  Result:=def;

  ind:=Hdr_Load(bln,hdr);
  if ind > 0 then begin

    len:=Hdr.n * SizeOf(ABC_Rec);
    ofs:=ind + len;

    for i:=1 to hdr.n do begin
      ind:=vm_Load(ind,Rec,SizeOf(Rec));

      s:=''; if Rec.MsgP <> 0 then
      ofs:=vm_LoadMsg(ofs,s);

      if Rec.What = 0 then
      if Rec.nnn = nn then begin

        if Rec.id = _enum then
          Result:=def_enum(Rec.ii,ofs,Rec)
        else
        if Rec.id in Id_Int then
          Result:=Rec.ii
        else
        if Rec.id in Id_Real then
          Result:=Round(Rec.rr);

        Break
      end;

      if Rec.What = 0 then
      if Rec.id = _Enum then
      if Rec.TxtP > 0 then
      Inc(ofs,Rec.TxtL);

    end
  end
end;

function TBlnBank.EnumStr(bln,nn,iv: Integer): String;

function enum_str(iv,ofs: Integer;
                  const Rec: Abc_Rec): String;
var
  buf: PBytes; len,pos: Integer; s: String;
begin
  Result:='';

  buf:=nil; len:=Rec.TxtL;

  if len > 0 then
  if Rec.TxtP > 0 then begin
    buf:=xGetMem(len);
    if buf <> nil then begin
      vm_Load(ofs,buf^,len);

      pos:=0; while pos < len do begin
        pos:=ReadPool(buf,pos,len,s);
        if iv = 0 then begin
          Result:=s; Break
        end; Dec(iv)
      end
    end;
  end
end;

var
  i,ind,len,ofs: Integer;
  hdr: TBlnHdr; Rec: ABC_Rec;
  s: String;
begin
  Result:='';

  ind:=Hdr_Load(bln,hdr);
  if ind > 0 then begin

    len:=Hdr.n * SizeOf(ABC_Rec);
    ofs:=ind + len;

    for i:=1 to hdr.n do begin
      ind:=vm_Load(ind,Rec,SizeOf(Rec));

      s:=''; if Rec.MsgP <> 0 then
      ofs:=vm_LoadMsg(ofs,s);

      if Rec.What = 0 then
      if Rec.nnn = nn then begin

        if Rec.id = _enum then
        Result:=enum_str(iv,ofs,Rec);

        Break
      end;

      if Rec.What = 0 then
      if Rec.id = _Enum then
      if Rec.TxtP > 0 then
      Inc(ofs,Rec.TxtL);

    end
  end
end;

procedure TBlnBank.Update_Version;
var
  ver: char4; i,j,ind,old: Integer;
  Hdr: TBlnHdr; Rec: Abc_Rec;
begin
  if fVersion < 2 then
  if vm_Update then begin

    old:=fVersion;
    ver[1]:='#'; ItoA(2,3,ver[2]);
    vm_Store(28,ver,4); fVersion:=2;

    for i:=1 to t_cnt do begin

      ind:=Hdr_Load(i,Hdr);
      if ind > 0 then

      for j:=1 to Hdr.n do begin
        vm_Load(ind,Rec,SizeOf(Rec));

        if old = 0 then begin
          Rec.flags:=0; Rec.MinVal:=0; Rec.MaxVal:=0;
        end
        else begin
          Rec.MinVal:=PLongint(@Rec.MinVal)^;
          Rec.MaxVal:=PLongint(@Rec.MinVal)^;
        end;

        ind:=vm_Store(ind,Rec,SizeOf(Rec))
      end
    end
  end
end;

constructor TDBaseList.Create;
begin
  inherited Create(Sizeof(TDbaseRec),1024)
end;

function Id_Equal(itag,otag: Id_Tag): boolean;
begin
  Result:=false;

  case itag of
_byte,
_int,
_word,
_long:
    Result:=otag in [_byte,_int,_word,_long,_dBase,
                     _enum,_time,_date,_bool,_link,
                     _real,_double,_color];

_dBase,
_enum,
_Bool,
_link:
    Result:=otag in [_byte,_int,_word,_long,
                     _dBase,_enum,_bool,_link];

_time,
_date:
    Result:=otag in [_int,_word,_long,_time,_date,
                     _real,_double];
_real,
_float,
_double:
    Result:=otag in [_byte,_int,_word,_long,_time,_date,
                     _real,_float,_angle,_double];
_angle:
    Result:=otag in [_real,_float,_angle,_double];

_string,
_unicode,
_text,
_list:
    Result:=otag in [_string,_unicode,_text,_list];

_unk:
    Result:=true;

  end
end;

function IdToStr(id: Id_Tag; info: PChar): string;
var
  ii: longint; dd: double; ff: float;
begin
  Result:='';

  case id of
_Bool,
_Byte,
_Int,
_Word,
_dBase,
_Enum,
_Long,
_Time,
_Date,
_Link:
    begin ii:=0;
      Move(info^,ii,Id_Len[id]);
      Result:=IntToStr(ii)
    end;

_Float,
_Real,
_Angle:
    begin ff:=0;
      Move(info^,ff,4);

      if Short_Correct(ff) then
      Result:=xRealToStr(real_align(ff,id),0)
    end;

_Double:
    begin dd:=0;
      Move(info^,dd,8);

      if Double_Correct(dd) then
      Result:=xRealToStr(dd,0)
    end;

_string:
    Result:=WinString(StrPas(info));
  end
end;

function info_into_range(id: Id_Tag; const info;
                         min,max: Single): Boolean;
var
  ii: Integer absolute info;
  rr: float absolute info;
  dd: double absolute info;
begin
  Result:=true;

  if min < max then

  case id of
_Bool,
_Byte,
_Int,
_Word,
_dBase,
_Enum,
_Long,
_Time,
_Date,
_Link:
    if (ii < min) or (ii > max) then
    Result:=false;

_Float,
_Real:
    if (rr < min) or (rr > max) then
    Result:=false;

_Double:
    if (dd < min) or (dd > max) then
    Result:=false;
  end

end;

function info_AsInteger(id: Id_Tag; const info): Integer;
var
  rc: Integer; ii: Integer;
  rr: float absolute info;
  dd: double absolute info;
  ss: string absolute info;
begin
  Result:=0;

  case id of
_Bool,
_Byte,
_Int,
_Word,
_dBase,
_Enum,
_Long,
_Time,
_Date,
_Link:
    begin ii:=0;
      Move(info,ii,Id_Len[id]);

      if id = _int then
      if ii and $8000 <> 0 then
      ii:=ii or $FFFF0000;

      Result:=ii
    end;
_Float,
_Real,
_Angle:
    if Abs(rr) < MaxInt then
    Result:=Round(rr);
_Double:
    if Abs(dd) < MaxInt then
    Result:=Round(dd);
_string:
    begin
      val(ss,ii,rc);
      if rc = 0 then
      Result:=ii
    end
  end
end;

function info_AsFloat(id: Id_Tag; const info): Double;
var
  ii,rc: Integer;
  rr: float absolute info;
  dd: Double absolute info;
  ss: string absolute info;
begin
  Result:=0;

  case id of
_Bool,
_Byte,
_Int,
_Word,
_dBase,
_Enum,
_Long,
_Time,
_Date,
_Link:
    begin ii:=0;
      Move(info,ii,Id_Len[id]);
      Result:=ii
    end;
_Float,
_Real,
_Angle:
    Result:=rr;

_Double:
    Result:=dd;

_string:
    begin
      val(ss,Result,rc);
      if rc <> 0 then Result:=0
    end

  end
end;

function Info_AsString(id: Id_Tag; const info): WideString;
var
  ii: Integer;
  rr: float absolute info;
  dd: Double absolute info;
  ss: string absolute info;
  ww: TWideStr absolute info;
begin
  Result:='';

  case id of
_Bool,
_Byte,
_Int,
_Word,
_dBase,
_Enum,
_Long,
_Time,
_Date,
_Link:
    begin ii:=0;
      Move(info,ii,Id_Len[id]);

      if id in [_Int,_Long] then begin

        if id = _Int then
        if ii and $8000 <> 0 then
        ii:=ii or $FFFF0000;

        Result:=IntToStr(ii)
      end else
      Result:=IntToStr(Cardinal(ii))
    end;

_Float,
_Real:
    if Short_Correct(rr) then
    Result:=xRealToStr(real_align(rr,id),0);

_Angle:
    if Short_Correct(rr) then
    Result:=xRealToStr(rr*180/Pi,0);

_Double:
    if Double_Correct(dd) then
    Result:=xRealToStr(dd,0);

_String:
    Result:=WinString(ss);

_unicode,
_text:
    Result:=xWideCharToString1(ww,0);
  end
end;

function info_verify(stm: thfStream): Integer;
var
  nn,pos: Integer; tag: Id_Tag; v: TUnicode;
begin
  stm.Seek(0,soFromBeginning);

  while true do begin pos:=stm.Position;
    if not stm.xread(nn,tag,v,Sizeof(v)) then
    begin stm.Size:=pos; Break end
  end;

  Result:=stm.Size
end;

function Get_equ(stm: thfStream;
                 nn: int; id: Id_Tag;
                 ansi: int; out equ): Boolean;
var
  _nn: int; tag: Id_Tag;
  ii: int; rr: float; dd: double;
  s: String; info: TUnicode;
  info_s: string absolute info;
  equ_s: string absolute equ;
begin
  Result:=false;

  stm.Seek(0,soFromBeginning);

  while stm.xread(_nn,tag,info,1023) do begin

    if _nn = nn then

    if id = _label then begin
      Result:=true; Break
    end else
    if id = _unk then begin
      Result:=tag = _unk; Break
    end else

    if Id_Equal(tag,id) then begin

      case id of
    _Bool,
    _Byte,
    _Int,
    _Word,
    _dBase,
    _Enum,
    _Long,
    _Time,
    _Date,
    _Link:
        begin
          ii:=info_AsInteger(tag,info);
          Move(ii,equ,Id_Len[id]);
          Result:=true
        end;

    _Float,
    _Real,
    _Angle:
        begin
          rr:=info_AsFloat(tag,info);
          Move(rr,equ,4); Result:=true
        end;

    _Double:
        begin
          dd:=info_AsFloat(tag,info);
          Move(dd,equ,8); Result:=true
        end;

    _string:
        if tag = _string then begin
          equ_s:=info_s; Result:=true
        end else
        if tag in [_unicode,_text] then begin
          equ_s:=DosString(WideCharToString(info));
          Result:=true
        end;

    _unicode,
    _text:
        if tag = _string then begin

          if ansi = 0 then
            DosToUnicodep(info_s,@equ)
          else begin
            s:=info_s;
            if ansi = rus_unicode then
            s:=WinString(s);

            xStringToWideChar(s,@equ)
          end;

          Result:=true
        end else
        if tag in [_unicode,_text] then begin
          if ansi = dos_unicode then
            WideStrPack(@equ,info,255)
          else
          if UnicodeMaxLen > 255 then
            WideStrLCopy(@equ,info,UnicodeMaxLen)
          else
            WideStrCopy(@equ,info);

          Result:=true
        end;

      end;

      if Result then Break
    end
  end
end;

function Get_hint(stm: thfStream; n: Integer;
                  s: PChar; smax: int): bool;
var
  bx,nn: int; id: Id_Tag;
  val: TValue;
begin
  Result:=false;

  stm.Seek(0,soFromBeginning); bx:=0;

  while stm.xread(nn,id,val,1023) do begin
    if nn = n then begin

      case id of
    _Bool,
    _Byte:
        begin
          StrPCopy(s,IntToStr(val.b));
          Result:=true
        end;
    _Int:
        begin
          StrPCopy(s,IntToStr(val.i));
          Result:=true
        end;
    _Word,
    _dBase,
    _Enum:
        begin
          StrPCopy(s,IntToStr(val.w));
          Result:=true
        end;
    _Long,
    _Time,
    _Date,
    _Link:
        begin
          StrPCopy(s,IntToStr(val.l));
          Result:=true
        end;

    _Float,
    _Real,
    _Angle:
        begin
          StrPCopy(s,RealToStr(val.f,1));
          Result:=true
        end;

    _Double:
        begin
          StrPCopy(s,RealToStr(val.d,1));
          Result:=true
        end;

    _string:
        begin
          StrPLCopy(s,WinString(val.s),smax);
          Result:=true
        end;

    _unicode,
    _text:
        begin
          bx:=(stm.Position-bx-2) div 2;
          StrCopyWA(s,smax,val.x,bx);
          Result:=true
        end;
      end;

      Break
    end;

    bx:=stm.Position
  end
end;

function Get_equ_nn(stm: thfStream; nn: Integer;
                    out id: Id_Tag; out info): Boolean;
var
  n: Integer;
begin
  Result:=false; id:=_label;
  stm.Seek(0,soFromBeginning);

  while stm.xread(n,id,info,0) do
  if n = nn then begin
    Result:=true; Break
  end
end;

function info_Comp(const info1,info2; id1,id2: Id_Tag): Boolean;
var
  ss: string absolute info2;
  r1,r2: Double; i1,i2,rc,l,t: int;
  w1,w2: WideString; s: String;
begin
  Result:=false;

  case id1 of
_Bool,
_Byte,
_Int,
_Word,
_dBase,
_Enum,
_Long,
_Time,
_Date,
_Link:
    begin
      i1:=info_AsInteger(id1,info1); rc:=0;

      if id2 <> _string then
        i2:=info_AsInteger(id2,info2)
      else begin
        val(ss,i2,rc);
        if rc > 1 then
        if ss[rc] in [',','.'] then begin
          l:=Length(ss)-rc;
          if l > 0 then begin
            s:=Copy(ss,rc+1,l);
            val(s,t,rc); if rc = 0 then
            rc:=t
          end
        end
      end;

      if rc = 0 then Result:=i1 = i2
    end;

_Float,
_Real,
_Angle,
_Double:
    begin
      r1:=info_AsFloat(id1,info1); rc:=0;

      if id2 <> _string then
        r2:=info_AsFloat(id2,info2)
      else begin
        val(ss,r2,rc); if rc <> 0 then
        if ss[rc] = ',' then begin
          s:=ss; s[rc]:='.'; val(s,r2,rc)
        end
      end;

      if rc = 0 then
      if Abs(r1-r2) < 0.001 then
      Result:=true
    end;

_string:
    begin
      w1:=Info_AsString(id1,info1);
      w2:=Info_AsString(id2,info2);

      if not (id2 in Id_Str) then
      ClsWideString(w1);

      Result:=w1 = w2;

      if not Result then
      if info_comp_uppercase then
      Result:=AnsiUpperCase(w1) = AnsiUpperCase(w2);
    end
  end
end;

function info_hash(const val; id: Id_Tag): Integer;
var
  s: String;
begin
  Result:=0; s:='';

  case id of
_Bool,
_Byte,
_Int,
_Word,
_dBase,
_Enum,
_Long,
_Time,
_Date,
_Link:
    s:=IntToStr( info_AsInteger(id,val) );

_Float,
_Real,
_Angle,
_Double:
    s:=RealToStr( info_AsFloat(id,val),-1 );

_string:
    begin
      s:=Info_AsString(id,val);
      s:=ClsString(s)
    end
  end;

  if length(s) > 0 then
  Result:=chk_Str(s)
end;

function info_Comp_hf(hf1,hf2: thfStream): Boolean;
var
  nn: Integer; id: Id_Tag;
  info1,info2: TString;
begin
  Result:=true;

  hf1.Seek(0,soFromBeginning);

  while hf1.xread(nn,id,info1,0) do begin

    if nn <> 1000 then

    if Get_equ(hf2,nn,id,0,info2) then

      Result:=info_Comp(info1,info2, id,id)

    else Result:=false;

    if not Result then Break
  end
end;

function info_Comp_nn(hf1,hf2: thfStream;
                      nn1,nn2: Integer): Boolean;
var
  id1,id2: Id_Tag;
  val1,val2: TString;
begin
  Result:=false;
  if Get_equ_nn(hf1,nn1,id1,val1) then
  if Get_equ_nn(hf2,nn2,id2,val2) then
  Result:=info_Comp(val1,val2, id1,id2)
end;

function info_hash_nn(hf: thfStream; nn: Integer): Integer;
var
  id: Id_Tag; val: TString;
begin
  Result:=0;
  if Get_equ_nn(hf,nn,id,val) then
  Result:=info_hash(val,id)
end;

constructor thfStream.Create(Ax16: longbool);
begin
  inherited Create; fx16:=Ax16
end;

destructor thfStream.Destroy;
begin
  if Assigned(fTextBuf) then
  xFreePtr(fTextBuf);
  inherited
end;

function thfStream.xread(out nn: int; out id: Id_Tag;
                         out val; MaxChars: Integer): Longbool;
var
  bx,cx,dx,eol,mem_size: int;
  s: string absolute val;
  w: TWideStr absolute val;
begin
  Result:=false; nn:=0; id:=_byte;

  mem_size:=Size; fText:=nil;
  
  if Position+2 < mem_Size then begin

    nn:=0;

    if fx16 then begin
      Read(nn,3);
      id:=Id_Tag(nn and $FF);
      nn:=nn shr 8;
    end
    else begin
      Read(nn,2);
      id:=Id_Tag(nn and $3F);
      nn:=nn shr 6;
    end;

    if id < _label then begin

      cx:=Id_len[id];
      if Position+cx <= mem_Size then

      case id of

    _Bool,
    _Byte,
    _Int,
    _Word,
    _dBase,
    _Enum,
    _Long,
    _Time,
    _Date,
    _Link,
    _unk:
        begin
          Read(val,cx); Result:=true
        end;

    _Float,
    _Real,
    _Angle:
        begin
          Read(val,4); Result:=true
        end;

    _Double:
        begin
          Read(val,8); Result:=true
        end;

    _string:
        if Position < mem_Size then begin
          cx:=0; Read(cx,1); s:='';

          if cx = 0 then Result:=true else
          if Position+cx <= mem_Size then begin
            s[0]:=chr(cx); Read(s[1],cx);
            Result:=true
          end
        end;

    _unicode,
    _text:
        if Position+1 < mem_Size then begin
          bx:=0; Read(bx,2); cx:=bx;
          eol:=0; Move(eol,w,2);

          if cx = 0 then Result:=true else
          if Position+cx+cx <= mem_Size then begin

            dx:=MaxChars;
            if dx = 0 then dx:=255;

            if cx > dx then begin

              if fTextBuf = nil then
              fTextBuf:=xAllocPtr( 32000*2 );

              if Assigned(fTextBuf) then
              if cx < 32000 then fText:=fTextBuf;

              cx:=dx 
            end;

            Read(w,cx+cx);
            Move(eol,w[cx],2);

            if Assigned(fText) then begin
              Move(w,fText[0],cx+cx);
              fText[cx]:=#0
            end;

            dx:=bx-cx;

            if dx > 0 then

            if fText = nil then
              Seek(dx+dx,soFromCurrent)
            else begin
              Read(fText[cx],dx+dx);
              fText[bx]:=#0
            end;

            Result:=true
          end
        end
      end
    end
  end
end;

function thfStream.xwrite(nn: int; id: Id_Tag;
                          const val; rw: Longbool): Longbool;
var
  bx,cx,len,loc: int;
  s: string absolute val;
  w: TWideStr absolute val;
begin
  Result:=false;

  loc:=Id_Len[id]; len:=0;

  if id = _string then begin
    loc:=Length(s); Inc(loc);
    if rw then loc:=256
  end else
  if id in [_unicode,_text] then begin
    len:=WideStrLen(w);
    if len >= 32000 then len:=32000-1;
    loc:=len+len;
  end;

  if loc > 0 then begin

    if fx16 then begin
      nn:=(nn shl 8) or ord(id); bx:=3;
    end
    else begin
      nn:=(nn shl 6) or ord(id); bx:=2;
    end;

    cx:=bx+loc;
    if len > 0 then Inc(cx,2);

    if Size+cx < 64000 then begin

      Write(nn,bx);
      if len > 0 then Write(len,2);
      Write(val,loc);

      Result:=true;
    end
  end
end;

function TInfoStream.Get_HF(n: int; id: Id_Tag; var equ): Boolean;
begin
  Result:=Get_Equ(Self, n,id,0, equ)
end;

procedure TInfoStream.Put_HF(n: int; id: Id_Tag; const equ);
var
  tmp: thfStream;
  nn: int; tag: Id_Tag;
  upd: Boolean;
begin
  tmp:=thfStream.Create(fx16);
  try
    Seek(0,soFromBeginning); upd:=false;

    while xread(nn,tag,fBuffer,Sizeof(fBuffer)) do begin
      if (nn = n) and ((id = _unk) or Id_Equal(tag,id)) then begin
        tmp.xwrite(n,id,equ,false); upd:=true;
      end
      else tmp.xwrite(nn,tag,fBuffer,false);
    end;

    if upd then begin
      Clear; CopyFrom(tmp,0)
    end
    else begin
      Seek(0,soFromEnd);
      xwrite(n,id,equ,false)
    end
  finally
    tmp.Free
  end
end;

procedure TInfoStream.Del_HF(n: int; id: Id_Tag);
var
  tmp: thfStream;
  nn: int; tag: Id_Tag;
  del: Boolean;
begin
  tmp:=thfStream.Create(fx16);
  try
    Seek(0,soFromBeginning); del:=false;

    while xread(nn,tag,fBuffer,0) do begin

      if (nn = n) and
         ((tag = _unk) or
          Id_Equal(tag,id)) then

        del:=true

      else
        tmp.xwrite(nn,tag,fBuffer,false)
    end;

    if del then begin
      Clear; CopyFrom(tmp,0)
    end
  finally
    tmp.Free
  end
end;

function TInfoStream.xGet_int(n: int; out v: Integer): Boolean;
begin
  v:=0; Result:=Get_Equ(Self, n,_long,0, v)
end;

function TInfoStream.xGet_real(n: int; out v: Double): Boolean;
begin
  v:=0; Result:=Get_Equ(Self, n,_double,0, v)
end;

function TInfoStream.Get_int(n: int): Integer;
begin
  Result:=0; Get_Equ(Self, n,_long,0, Result)
end;

function TInfoStream.Get_real(n: int): Double;
begin
  Result:=0; Get_HF(n,_double,Result)
end;

procedure TInfoStream.Put_str(n: int; s: string);
begin
  Put_hf(n,_string,s)
end;

constructor TInfoList.Create;
begin
  inherited Create;
  mem:=thfStream.Create(false);
  List:=TIntegerList.Create;
end;

destructor TInfoList.Destroy;
begin
  tmem.Free;
  List.Free; mem.Free;
  inherited
end;

procedure TInfoList.Set_x16(v: longbool);
begin
  if mem.x16 <> v then
  if mem.Size > 0 then begin

    if tmem = nil then
    tmem:=thfStream.Create(fx16);
    tmem.fx16:=fx16;

    tmem.LoadFromStream(mem);

    fx16:=v; mem.fx16:=v;
    LoadFrom(tmem)
  end;

  fx16:=v; mem.fx16:=v
end;

function TInfoList.LoadFrom(buf: thfStream): Integer;
var
  nn,ind: Integer; id: Id_Tag;
begin
  Clear; buf.Seek(0,soFromBeginning);

  while buf.xread(nn,id,fBuffer,Unicode_Max) do begin
    ind:=mem.Position;

    if Assigned(buf.Text) then begin
      if mem.xwrite(nn,id,buf.Text^,fIs_Edit) then
      List.AddItem(ind)
    end else

    if mem.xwrite(nn,id,fBuffer,fIs_Edit) then
    List.AddItem(ind);
  end;

  Result:=mem.Size
end;

function TInfoList.SaveTo(buf: thfStream): Integer;
var
  i,nn: int; id: Id_Tag;
begin
  for i:=0 to Count-1 do
  if Get_hf(i,nn,id,fBuffer,Unicode_Max) then

  if mem.Text = nil then
    buf.xwrite(nn,id,fBuffer,false)
  else
    buf.xwrite(nn,id,mem.Text^,false);

  Result:=buf.Size
end;

function TInfoList.GetPool(buf: PBytes; bufSize: Integer): Integer;
var
  i,pos,len: Integer; di: PBytes;
begin
  Result:=0; di:=buf;
  for i:=0 to List.Count-1 do begin
    pos:=List[i];
    if i < List.Count-1 then
      len:=List[i+1] - pos
    else
      len:=mem.Size - pos;

    if Result + len <= bufSize then begin
      mem.Seek(pos,soFromBeginning);
      mem.Read(di[0],len); di:=@di[len];
      Inc(Result,len);
    end
  end
end;

procedure TInfoList.Clear;
begin
  List.Clear; mem.Clear
end;

function TInfoList.Get_hf(ind: Integer;
                          out nn: Integer; out id: Id_Tag;
                          var info; MaxChars: Integer): Boolean;
var
  pos: Integer;
begin
  Result:=false; nn:=0; id:=_byte;

  if ind >= 0 then
  if ind < Count then begin
    pos:=List[ind];

    if pos >= 0 then
    if pos < mem.Size then begin
      mem.Seek(pos,soFromBeginning);
      Result:=mem.xread(nn,id,info,MaxChars)
    end
  end
end;

function TInfoList.Get_id(ind: Integer;
                          out nn: Integer;
                          out id: Id_Tag): Boolean;
begin
  Result:=Get_hf(ind, nn,id, fBuffer,0)
end;

function TInfoList.Get_equ(n: Integer; tag: Id_Tag;
                           out equ; pop: Boolean): Boolean;
var
  i,nn: Integer; id: Id_Tag;
  ii: Integer absolute equ;
  rr: Double absolute equ;
  ss: WideString; info: TString;
begin
  Result:=false;

  for i:=0 to List.Count-1 do
  if Get_hf(i, nn,id,info,0) then

  if nn = n then

  if id = _unk then begin
    if pop then List.Delete(i);
    fis_unk:=false; Break
  end else

  if Id_Equal(id,tag) then begin

    case tag of
  _Bool,
  _Byte,
  _Int,
  _Word,
  _dBase,
  _Enum,
  _Long,
  _Time,
  _Date,
  _Link,
  _Color:
      begin
        ii:=info_AsInteger(id,info);
        Result:=true
      end;

  _Float,
  _Real,
  _Angle,
  _Double:
      begin
        rr:=info_AsFloat(id,info);

        if tag = _Angle then
        if id <> _Angle then
        rr:=rr/180*Pi; 

        Result:=true
      end;

  _string:
      begin
        ss:=info_AsString(id,info);
        StringToWideChar(ss,@equ,255);
        Result:=true
      end
    end;

    if Result then begin
      if pop then List.Delete(i);
      Break
    end
  end
end;

function TInfoList.Pop_equ(n: Integer; tag: Id_Tag;
                           out equ; out unk: Boolean): Boolean;
begin
  fis_unk:=true; unk:=false;
  Result:=Get_equ(n,tag,equ,true);
  if not fis_unk then unk:=true;
  fis_unk:=false;
end;

function TInfoList.Get_val(n: Integer; id: Id_Tag): string;
var
  val: tstring;
begin
  Result:='';

  if Get_equ(n,id,val,false) then

  if id in Id_int then
    Result:=IntToStr(val.l)
  else
  if id in Id_real then
    Result:=xRealToStr(val.d,0)
  else
  if id = _string then
    Result:=WideCharToString(val.x)

end;

procedure TInfoList.Update_nn(ind,nn: Integer);
var
  pos,tag,len: int;
begin
  if ind >= 0 then
  if ind < Count then begin
    pos:=List[ind];

    if pos >= 0 then
    if pos+2 < mem.Size then begin
      mem.Seek(pos,soFromBeginning);

      tag:=0; len:=2;
      if fx16 then begin
        len:=3; mem.Read(tag,3);
        tag:=tag and $FF;
        tag:=tag or (nn shl 8);
      end
      else begin
        len:=2; mem.Read(tag,2);
        tag:=tag and $3F;
        tag:=tag or (nn shl 6);
      end;

      mem.Seek(pos,soFromBeginning);
      mem.Write(tag,len)
    end
  end
end;

function TInfoList.push_equ(n: Integer; id: Id_Tag;
                            const info): Boolean;
var
  ind: Integer;
begin
  Result:=false;
  mem.Seek(0,soFromEnd); ind:=mem.Position;
  if mem.xwrite(n,id,info,fIs_Edit) then begin
    List.AddItem(ind); Result:=true
  end
end;

function TInfoList.Get_string(n: Integer; pop: Boolean): WideString;
var
  i,nn: int; id: Id_Tag; w: PWideChar;
begin
  Result:='';

  for i:=0 to List.Count-1 do
  if Get_hf(i, nn,id,fBuffer,Unicode_Max) then

  if nn = n then

  if id = _unk then begin
    if pop then List.Delete(i);
    fis_unk:=false; Break
  end else

  if id in Id_Str then begin

    if id = _String then
      Result:=info_AsString(id,fBuffer)
    else begin
      w:=mem.Text;
      if w = nil then w:=fBuffer;
      Result:=xWideCharToString1(w,fMaxStrLen)
    end;

    if pop then List.Delete(i);
    Break
  end
end;

function TInfoList.Pop_string(n: Integer; out unk: Boolean): WideString;
begin
  fis_unk:=true; unk:=false;
  Result:=Get_string(n,true);
  if not fis_unk then unk:=true;
  fis_unk:=false;
end;

function TInfoList.Push_string(n: Integer; const v: WideString): Boolean;
var
  s: string; is_pack: Boolean;
begin
  Result:=false;

  if (length(v) > 0)
  or Is_Empty then begin

    is_pack:=false;
    if rus_interface then
    is_pack:=length(v) <= 255;

    if is_pack then begin
      s:=v; s:=DosString(v);
      Result:=push_equ(n,_string,s)
    end
    else begin
      StringToWideChar(v,fBuffer,1023);
      Result:=push_equ(n,_unicode,fBuffer)
    end

  end
end;

function TInfoList.push_text(n: Integer; const s: WideString): Boolean;
var
  P,Q: PWideChar; Len: Integer;
begin
  Result:=false;
  Len:=Length(s);
  if Len > 0 then begin
    P:=nil; Q:=@fBuffer;
    if Len > Unicode_Max then begin
      P:=xAllocPtr((Len+1)*2); Q:=P
    end;

    if Assigned(Q) then begin
      StringToWideChar(s,Q,Len+1);
      Result:=push_equ(n,_unicode,Q[0])
    end;

    if Assigned(P) then
    xFreePtr(P)
  end
end;

function TInfoList.push_str(n: Integer; s: PChar): Boolean;
var
  v: String;
begin
  v:=Strpas(s);
  Result:=push_equ(n,_string,v)
end;

function TInfoList.AsInteger(i: Integer): Integer;
var
  nn: Integer; id: Id_Tag; val: TWideStr;
begin
  Result:=0;
  if Get_hf(i, nn,id,val,0) then
  Result:=info_AsInteger(id,val)
end;

function TInfoList.AsFloat(i: Integer): Double;
var
  nn: Integer; id: Id_Tag; val: tWideStr;
begin
  Result:=0;
  if Get_hf(i, nn,id,val,0) then
  Result:=info_AsFloat(id,val)
end;

function TInfoList.AsString(i: Integer): WideString;
var
  nn: Integer; id: Id_Tag;
begin
  Result:='';
  if Get_hf(i, nn,id,fBuffer,Unicode_Max) then
  Result:=info_AsString(id,fBuffer)
end;

procedure TInfoList.Update_int(i: Integer; eq: Integer);
var
  nn: Integer; id: Id_Tag;
  ind: Integer; info: TWideStr;
  ii: Integer absolute info;
  rr: float absolute info;
  dd: Double absolute info;
  ss: string absolute info;

begin
  if Get_id(i, nn,id) then begin
    ind:=List[i];
    mem.Seek(ind,soFromBeginning);
    FillChar(info,SizeOf(info),0);

    case id of
  _Bool,
  _Byte,
  _Int,
  _Word,
  _dBase,
  _Enum,
  _Long,
  _Time,
  _Date,
  _Link:
      ii:=eq;
  _Float,
  _Real:
      rr:=eq;
  _Angle:
      rr:=eq/180 * Pi;
  _Double:
      dd:=eq;

  _String:
      ss:=IntToStr(eq)
    end;

    mem.xwrite(nn,id,info,false)
  end
end;

procedure TInfoList.Update_real(i: Integer; eq: Double);
var
  nn: Integer; id: Id_Tag;
  ind: Integer; info: tWideStr;
  ii: Integer absolute info;
  rr: float absolute info;
  dd: Double absolute info;
  ss: string absolute info;

begin
  if Get_id(i, nn,id) then begin
    ind:=List[i];
    mem.Seek(ind,soFromBeginning);
    FillChar(info,SizeOf(info),0);

    case id of
  _Bool,
  _Byte,
  _Int,
  _Word,
  _dBase,
  _Enum,
  _Long,
  _Time,
  _Date,
  _Link:
      if Abs(eq) < MaxInt then
      ii:=Round(eq);
  _Float,
  _Real:
      rr:=eq;
  _Double:
      dd:=eq;

  _String:
      ss:=xRealToStr(eq,0)
    end;

    mem.xwrite(nn,id,info,false)
  end
end;

procedure TInfoList.Update_string(i: Integer; eq: WideString);

function Resize_val(ind,len: Integer): Integer;
var
  pos: Integer;
begin
  pos:=mem.Size; List[ind]:=pos;
  mem.Seek(pos,soFromBeginning);
  Result:=pos
end;

var
  nn,rc: Integer; id: Id_Tag;
  pos: Integer; info: TWideStr;
  ii: Integer absolute info;
  rr: float absolute info;
  dd: Double absolute info;
  ss: string absolute info;

begin
  if Get_id(i, nn,id) then begin
    pos:=List[i];
    mem.Seek(pos,soFromBeginning);
    FillChar(info,SizeOf(info),0);

    case id of
  _Bool,
  _Byte,
  _Int,
  _Word,
  _dBase,
  _Enum,
  _Long,
  _Time,
  _Date,
  _Link:
      begin
        val(eq,ii,rc);
        if rc <> 0 then ii:=0
      end;
  _Float,
  _Real:
      begin
        val(eq,rr,rc);
        if rc <> 0 then rr:=0
      end;
  _Double:
      begin
        val(eq,dd,rc);
        if rc <> 0 then dd:=0
      end;

  _String:
      begin
        ss:=DosString(eq);
        Resize_val(i,Length(ss)+1);
      end
    end;

    mem.xwrite(nn,id,info,false)
  end
end;

function TInfoList.xUpdate_int(nn: Integer; eq: Integer): Integer;
begin
  Result:=Contains_nn(nn);
  if Result >= 0 then Update_int(Result,eq)
end;

function TInfoList.Delete(i: Integer): Boolean;
var
  nn: Integer; id: Id_Tag;
begin
  Result:=false; if i >= 0 then
  if Get_id(i, nn,id) then begin
    List.Delete(i); Result:=true
  end;
end;

procedure TInfoList.Delete_nn(n: Integer);
var
  i, nn: Integer; id: Id_Tag;
begin
  for i:=0 to List.Count-1 do
  if Get_id(i, nn,id) then
  if nn = n then begin
    Delete(i); Break
  end
end;

function TInfoList.Send(Msg: PChar): PChar;
var
  i,nn: Integer; id: Id_Tag;
  eq: TShortStr;
begin
  Result:=nil; xStrCopy(Msg,'');

  for i:=0 to List.Count-1 do
  if Get_id(i, nn,id) then

  if StrWide(eq,AsString(i),255) <> nil then
  if StrLen(eq) > 0 then

  if StrLen(Msg)+StrLen(eq)+8 < 255 then begin
    StrCat(Msg,' ');
    xyStrCat(Msg,nn,ord(id),false);
    Result:=StrCat(Msg,eq)
  end
end;

procedure TInfoList.Receive(msg: PChar);
var
  n,t: longint; id: Id_Tag;
  eq: tShortStr; s: string;
  i: longint; r: float;
  d: Double; rc: Integer;

begin
  while IntToken(msg,n) and IntToken(msg,t) and
        (StrToken(eq,msg) <> nil) do begin

    if (n > 0) and (n < 1024) then
    if (t >= 0) and (t < ord(_label)) then begin

      id:=Id_Tag(t); s:=StrPas(eq);

      case id of
    _Bool,
    _Byte,
    _Int,
    _Word,
    _dBase,
    _Enum,
    _Long,
    _Time,
    _Date,
    _Link:
        begin
          Val(s,i,rc);
          if rc = 0 then
          Push_equ(n,id,i)
        end;

    _Float,
    _Real,
    _Angle:
        begin
          Val(s,r,rc);
          if rc = 0 then
          Push_equ(n,id,r)
        end;
    _Double:
        begin
          Val(s,d,rc);
          if rc = 0 then
          Push_equ(n,id,d)
        end;

    _string:
        Push_string(n,s);
    else Break
      end;
    end

  end
end;

procedure TInfoList.CopyTo(dst: TInfoList; nnn: Integer);
var
  i,nn: int; id: Id_Tag;
begin
  dst.Clear; dst.x16:=x16;

  if nnn > 0 then dst.Push_equ(1000,_long,nnn);

  for i:=0 to Count-1 do
  if Get_hf(i,nn,id, fBuffer,Unicode_Max) then

  if mem.Text = nil then
    dst.push_equ(nn,id,fBuffer)
  else
    dst.push_equ(nn,id,mem.Text^)
end;

function TInfoList.Contains_str(n: Integer; eq: string): Integer;
var
  i,nn: Integer; id: Id_Tag;
begin
  Result:=-1;

  for i:=0 to List.Count-1 do
  if Get_hf(i, nn,id,fBuffer,0) then

  if nn = n then
  if UpString(info_AsString(id,fBuffer)) = eq then
  begin Result:=i; Break end
end;

function TInfoList.Contains_nn(nn: int): int;
var
  i,_nn: int; id: Id_Tag;
begin
  Result:=-1;

  for i:=0 to List.Count-1 do
  if Get_id(i, _nn,id) then

  if _nn = nn then begin
    Result:=i; Break
  end
end;

function TInfoList.Typeof(nn: Integer): Integer;
var
  i,_nn: Integer; _id: Id_Tag;
begin
  Result:=-1;

  for i:=0 to List.Count-1 do
  if Get_id(i, _nn,_id) then

  if _nn = nn then begin
    Result:=ord(_id); Break
  end
end;

function TInfoList.Is_unk(nn: Integer): Boolean;
begin
  fis_unk:=true; Get_equ(nn,_unk,fBuffer,false);
  Result:=not fis_unk; fis_unk:=false
end;

function TInfoList.xGet_int(n: Integer;
                            out v: Integer): Boolean;
var
  i,nn: Integer; id: Id_Tag;
  buf: TWideStr;
begin
  Result:=false; v:=0;

  for i:=0 to Count-1 do
  if Get_hf(i,nn,id,buf,0) then

  if nn = n then
  if Id_Equal(id,_int) then begin
    v:=info_AsInteger(id,buf);
    Result:=true; Break
  end
end;

function TInfoList.xGet_real(n: Integer;
                             out v: Double): Boolean;
var
  i,nn: Integer; id: Id_Tag;
  buf: TWideStr;
begin
  Result:=false; v:=0;

  for i:=0 to Count-1 do
  if Get_hf(i,nn,id,buf,0) then

  if nn = n then
  if Id_Equal(id,_double) then begin
    v:=info_AsFloat(id,buf);
    Result:=true; Break
  end
end;

function TInfoList.xGet_str(n: Integer; s: PChar): Boolean;
var
  i,nn: Integer; id: Id_Tag;
  val: TString;
begin
  Result:=false; StrCopy(s,'');

  for i:=0 to Count-1 do
  if Get_hf(i,nn,id,val,0) then

  if nn = n then
  if id = _string then begin
    StrPCopy(s,val.s);
    Result:=true; Break
  end
end;

function TInfoList.xStr_val(n: Integer; s: PChar): Boolean;
var
  i,nn: Integer; id: Id_Tag;
  val: ^TString;
begin
  Result:=false; StrCopy(s,'');

  val:=@fBuffer;

  for i:=0 to Count-1 do
  if Get_hf(i,nn,id,val^,0) then

  if nn = n then

  if id = _string then begin
    StrWin(StrPCopy(s,val.s));
    Result:=true; Break
  end else
  if id = _unicode then begin
    StrPLCopy(s,WideCharToString(val.x),255);
    Result:=true; Break
  end

end;

function TInfoList.Get_int(n,def: Integer): Integer;
var
  val: Integer;
begin
  Result:=def;
  if xGet_int(n,val) then
  Result:=val
end;

function TInfoList.Get_real(n: Integer; def: Double): Double;
var
  val: Double;
begin
  Result:=def;
  if xGet_real(n,val) then
  Result:=val
end;

function TInfoList.Get_str(n: Integer; def: string): string;
var
  s: TShortStr;
begin
  Result:=def;
  if xGet_str(n,s) then
  Result:=Strpas(s);
end;

procedure TInfoList.Add_equ(n: Integer; id: Id_Tag; eq: String);
var
  _id: Id_Tag; i,rc: Integer;
  r: Single; d: Double;
begin
  _id:=_string;

  if id <> _string then begin

    val(eq,i,rc);
    if rc = 0 then begin
      _id:=_int; r:=i; d:=i;

      if id in Id_Int+Id_Real then
      _id:=id

    end
    else begin
      val(eq,r,rc);
      if rc = 0 then begin
        _id:=_Real; d:=r;

        if id in Id_Real then
        _id:=id

      end
      else begin
        val(eq,d,rc);
        if rc = 0 then
        _id:=_Double
      end
    end;
  end;

  if _id in Id_Int then
    Push_equ(n,_id,i)
  else
  if _id = _Double then
    Push_equ(n,_id,d)
  else
  if _id in Id_Real then
    Push_equ(n,_id,r)
  else
    Push_equ(n,_string,eq)
end;

function TInfoList.Get_Count: Integer;
begin
  Result:=List.Count
end;

function TInfoList.Get_ansi_Count: Integer;
var
  i,nn: Integer; id: Id_Tag;
  val: TString; s: WideString;
begin
  Result:=0;

  for i:=0 to Count-1 do
  if Get_hf(i,nn,id,val,0) then begin

    if id = _unicode then
      s:=AsString(i)
    else
      s:=Info_AsString(id,val);

    if (id = _unicode) and (Length(s) > 255) then
      Inc(Result)
    else
    if is_rus_wide(s) then
      Inc(Result)
  end
end;

function TInfoList.Get_dos_Count: Integer;
begin
  Result:=Count - ansi_Count
end;

function TInfoList.Add_info(hf: TInfoList): Integer;
var
  i,nn: Integer; id: Id_Tag; val: PBytes;
begin
  val:=@hf.fbuffer;

  for i:=0 to hf.Count-1 do
  if hf.Get_hf(i, nn,id, val^,Unicode_Max) then
  if Contains_nn(nn) < 0 then push_equ(nn,id,val^);

  Result:=Count
end;

function hf_transit(id,id_: Id_Tag; eq: OTypes.PString): Id_Tag;
var
  i,rc: int; f: single; d: double; s: string;
begin
  Result:=id; s:='';

  if id in Id_Int then
    s:=IntToStr(eq.l)
  else
  if id in [_float,_real,_angle] then
    s:=xRealToStr(eq.f,0)
  else
  if id = _double then
    s:=xRealToStr(eq.d,0)
  else
  if id = _string then
    s:=eq.s
  else
    Exit;

  if id_ in Id_Int then begin

    if id in Id_Real then begin

      if id = _double then eq.l:=Round(eq.d)
                      else eq.l:=Round(eq.f);
      Result:=id_

    end
    else begin
      s:=ClsString(s); val(s,i,rc);
      if rc = 0 then begin
        eq.l:=i; Result:=id_
      end
      else begin
        val(s,d,rc);
        if rc = 0 then begin
          i:=Round(d);
          if i = Round(d) then begin
            eq.l:=i; Result:=id_
          end
        end
      end
    end
  end else
  if id_ in [_float,_real,_angle] then begin
    s:=ClsString(s); val(s,f,rc);
    if rc = 0 then begin
      eq.f:=f; Result:=id_
    end
  end else
  if id_ = _double then begin
    s:=ClsString(s); val(s,d,rc);
    if rc = 0 then begin
      eq.d:=d; Result:=id_
    end
  end else
  if id_ = _string then begin
    eq.s:=s; Result:=id_
  end
end;

function enum_Abc_to_set(const f: Abc_Rec): int64;
var
  x: TInt64;
begin
  x.i[0]:=PInteger(@f.MinVal)^;
  x.i[1]:=PInteger(@f.MaxVal)^;
  Result:=x.x
end;

function enum_Abc_to_set_str(const f: Abc_Rec): String;
var
  i: int; b,x: int64; s: String;
begin
  x:=enum_Abc_to_set(f); b:=1;

  s:='';
  for i:=1 to 64 do begin
    if b and x <> 0 then begin
      if length(s) > 0 then s:=s+',';
      s:=s+IntToStr(i);
      if length(s) > 64 then Break
    end; b:=b shl 1
  end;

  Result:=s
end;

procedure enum_set_to_Abc(var f: Abc_Rec; bits: int64);
var
  x: TInt64;
begin
  x.x:=bits;
  f.MinVal:=pfloat(@x.i[0])^;
  f.MaxVal:=pfloat(@x.i[1])^
end;

procedure enum_str_set_to_Abc(var f: Abc_Rec; str: PChar);
var
  v: int; x: int64; fl: bool; s: TShortstr;
begin
  x:=0;

  fl:=Is_Comma_Delimiter;
  Is_Comma_Delimiter:=true;

  StrCopy(s,str);
  while IntToken(s,v) do
  if v > 0 then
  x:=x or (1 shl (v-1));

  Is_Comma_Delimiter:=fl;

  enum_set_to_Abc(f,x)
end;

end.