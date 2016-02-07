unit xini; interface

uses
  Classes,Forms,Graphics,
  Math,otypes,ofiles;

const
  reg_form_up: Boolean = false;

type
  tini = class
    constructor Create(AName: PChar);
    destructor Destroy; override;

    function LoadFromFile(APath: PChar): Boolean;
    procedure SaveToFile(Dest: PChar);

    function GetDir(Key,Dir: PChar): Boolean;
    procedure PutDir(Key,Dir: PChar);

    function GetIniDir(Key,Dir: PChar): Boolean;
    function PutIniDir(Key,Dir: PChar): Boolean;

    function GetFile(Key,Path: PChar): Boolean;
    procedure PutFile(Key,Path: PChar);
    procedure DelFile(Key: PChar);

    function GetForm(Form: TForm): Boolean;
    procedure PutForm(Form: TForm);

    function DefInt(v: Integer; Key: PChar): Integer;
    function DefEnum(v,max: int; Key: PChar): int;

    function GetInt(Key: PChar; out v: Integer): Boolean;
    function xGetInt(Key: PChar; eq: Integer): Boolean;
    procedure PutInt(Key: PChar; v: Integer);
    procedure DelInt(Key: PChar);

    function GetCode(Key: PChar; out v: Integer): Boolean;
    function DefCode(v: Integer; Key: PChar): Integer;
    procedure PutCode(Key: PChar; v: Integer);
    procedure DelCode(Key: PChar);

    function DefBool(v: Boolean; Key: PChar): Boolean;
    function GetBool(Key: PChar; out fl: Boolean): Boolean;
    procedure PutBool(Key: PChar; fl: Boolean);

    function GetReal(Key: PChar; out v: Double): Boolean;
    function defReal(Key: PChar; def: Double): Double;
    procedure PutReal(Key: PChar; v: Double);

    function GetPoint(Key: PChar; out p: TPoint): Boolean;
    function defPoint(Key: PChar; X,Y: Integer): TPoint;
    procedure PutPoint(Key: PChar; X,Y: Integer);

    function GetXyz(Key: PChar; out v: txyz): Boolean;
    procedure PutXyz(Key: PChar; const v: txyz);

    function GetStr(Key,Str: PChar): Boolean;
    procedure PutStr(Key,Str: PChar);
    procedure DelStr(Key: PChar);

    function GetWorkDir: Boolean;

    function GetFont(Font: TFont; Key: PChar): Boolean;
    procedure PutFont(Font: TFont; Key: PChar);

  private
    fdir: TStringList;
    ffiles: TStringList;
    ffonts: TStringList;
    fwin: TStringList;
    ival: TStringList;
    cval: TStringList;
    bool: TStringList;
    rval: TStringList;
    sval: TStringList;
    pval: TStringList;

    fPath: TCmdStr;

    function Get_str(list: TStrings; Key,Val: PChar): Boolean;
    procedure Put_str(list: TStrings; Key,Val: PChar);
    procedure Put_val(list: TStrings; Key,Val: PChar);

    procedure Del_key(list: TStrings; Key: PChar);
  end;

var
  ini: tini;

function StrForm(Str: PChar; Form: TForm): PChar;
function StrToForm(Str: PChar; Form: TForm): Boolean;

function FontToken(Font: TFont; Str: PChar): Boolean;
function StrFont(Str: PChar; Font: TFont): PChar;

implementation

uses
  Controls,Sysutils,convert,ofiles1;

var
  scrRect: TRect;

procedure InitScreen;
var
  i: int; m: TMonitor;
begin
  scrRect:=Rect(0,0,Screen.Width,Screen.Height);

  for i:=0 to Screen.MonitorCount-1 do begin
    m:=Screen.Monitors[i];
    if not m.Primary then
    with scrRect do begin
      Left:=Min(Left,m.Left);
      Top:=Min(Top,m.Top);
      Right:=Max(Right,m.Left+m.Width);
      Bottom:=Max(Bottom,m.Top+m.Height);
    end
  end
end;

function StrForm(Str: PChar; Form: TForm): PChar;
begin
  with Form do
  Result:=StrFmt(Str,'%d %d %d %d %d',
  [Left,Top,Width,Height,ord(Visible)]);
end;

function StrToForm(Str: PChar; Form: TForm): Boolean;
var
  x,y,w,h,v: int;
begin
  Result:=false;

  if Str[0] <> #0 then
  if IntToken(Str,x) and IntToken(Str,y) then
  if IntToken(Str,w) and IntToken(Str,h) then begin

    if Form.BorderStyle <> bsSizeable then begin
      w:=Form.Width; h:=Form.Height
    end;

    if x+w > scrRect.Left+32 then
    if x+32 < scrRect.Right then

    if y+h > scrRect.Top+32 then
    if y+32 < scrRect.Bottom then
      Form.SetBounds(x,y,w,h);

    if IntToken(Str,v) then
    Result:=v = 1;

    if reg_form_up then
    Form.Visible:=Result
  end
end;

function FontToken(Font: TFont; Str: PChar): Boolean;
var
  nm: TShortStr; style,color,size: Integer;
begin
  Result:=false;

  if StrToken(nm,Str) <> nil then
  if IntToken(Str,style) then
  if IntToken(Str,color) then
  if IntToken(Str,size) then begin
    Font.Name:=StrPas(nm);
    Font.Style:=TFontStyles(style);
    Font.Color:=color;
    Font.Size:=size;
    Result:=true
  end
end;

function StrFont(Str: PChar; Font: TFont): PChar;
begin
  Result:=Str; with Font do
  StrLFmt(str,255,'"%s" %d %d %d',[Name,int(Style),Color,Size]);
end;

constructor tini.Create(AName: PChar);

procedure LoadFrom(AName: PChar);
var
  dir: TPathStr;
begin
  StrIniDir(dir);
  if not dir_Exists(dir) then
  StrCopy(dir,WorkDir);

  StrPath(fPath,dir,AName);
  LoadFromFile(fPath)
end;

var
  nm: TShortStr;
begin
  inherited Create;

  fdir:=TStringList.Create;
  fdir.CaseSensitive:=false;

  ffiles:=TStringList.Create;
  ffiles.CaseSensitive:=false;

  fwin:=TStringList.Create;
  fwin.CaseSensitive:=false;

  ival:=TStringList.Create;
  ival.CaseSensitive:=false;

  cval:=TStringList.Create;
  cval.CaseSensitive:=false;

  bool:=TStringList.Create;
  bool.CaseSensitive:=false;

  rval:=TStringList.Create;
  rval.CaseSensitive:=false;

  sval:=TStringList.Create;
  sval.CaseSensitive:=false;

  pval:=TStringList.Create;
  pval.CaseSensitive:=false;

  ffonts:=TStringList.Create;
  ffonts.CaseSensitive:=false;

  if Assigned(AName) then
    StrCopy(nm,AName)
  else begin
    xGetModuleFileName(nm,255);
    StrNameExt(nm,nm); StrUpdateExt(nm,nm,'');
  end;

  LoadFrom(nm);
end;

destructor tini.Destroy;
begin
  SaveToFile(fPath);

  ffonts.Free;

  pval.Free;
  sval.Free;
  rval.Free;
  bool.Free;
  cval.Free;
  ival.Free;

  fwin.Free;

  ffiles.Free;
  fdir.Free;

  inherited
end;

function tini.LoadFromFile(APath: PChar): Boolean;

function ReadSection(txt: TTextfile; list: TStrings): int;
begin
  list.Clear;

  while txt.xStrLine <> nil do begin
    LStr(txt.str);
    if StrLIComp(txt.str,'[end_',5) = 0 then Break;
    list.Add(txt.str)
  end;

  Result:=list.Count
end;

var
  txt: TTextfile; key: TShortstr;
begin
  Result:=false;

  txt:=TTextfile.Create;
  try
    if txt.Open(APath) then begin

      while txt.xStrLine <> nil do

      if txt.x_str(key) <> nil then

      if key[0] = '[' then
      if StrIComp(key,'[dir]') = 0 then
        ReadSection(txt,fdir)
      else
      if StrIComp(key,'[files]') = 0 then
        ReadSection(txt,ffiles)
      else
      if StrIComp(key,'[fonts]') = 0 then
        ReadSection(txt,ffonts)
      else
      if StrIComp(key,'[win]') = 0 then
        ReadSection(txt,fwin)
      else
      if StrIComp(key,'[ival]') = 0 then
        ReadSection(txt,ival)
      else
      if StrIComp(key,'[cval]') = 0 then
        ReadSection(txt,cval)
      else
      if StrIComp(key,'[bool]') = 0 then
        ReadSection(txt,bool)
      else
      if StrIComp(key,'[rval]') = 0 then
        ReadSection(txt,rval)
      else
      if StrIComp(key,'[sval]') = 0 then
        ReadSection(txt,sval)
      else
      if StrIComp(key,'[pval]') = 0 then
        ReadSection(txt,pval);

      Result:=true
    end
  finally
    txt.Free
  end
end;

procedure tini.SaveToFile(Dest: PChar);

function WriteSection(txt: TTextWrite; list: TStrings; key: PChar): int;
var
  i: int;
begin
  if list.Count > 0 then begin
    txt.WriteStr(Format('[%s]',[key]));
    for i:=0 to list.Count-1 do txt.WriteStr(list[i]);
    txt.WriteStr(Format('[end_%s]',[key]));
  end; Result:=list.Count
end;

var
  txt: TTextWrite; rc: int;
begin
  rc:=0;
  txt:=TTextWrite.Create;
  try
    if txt.New(Dest) then
    rc:=WriteSection(txt,fdir,'dir')+
        WriteSection(txt,ffiles,'files')+
        WriteSection(txt,ffonts,'fonts')+
        WriteSection(txt,fwin,'win')+
        WriteSection(txt,ival,'ival')+
        WriteSection(txt,cval,'cval')+
        WriteSection(txt,bool,'bool')+
        WriteSection(txt,rval,'rval')+
        WriteSection(txt,sval,'sval')+
        WriteSection(txt,pval,'pval');
  finally
    txt.Free
  end;

  if rc = 0 then FileErase(Dest);
end;

procedure tini.Put_str(list: TStrings; Key,Val: PChar);
var
  Ind: Integer; str: TShortstr;
begin
  if Assigned(Val) then begin
    StrLFmt(str,255,'%s="%s"',[Key,Val]);
    Ind:=list.IndexOfName(Key);
    if Ind >= 0 then list[Ind]:=str
    else list.Add(str)
  end
end;

procedure tini.Put_val(list: TStrings; Key,Val: PChar);
var
  Ind: Integer; str: TShortstr;
begin
  StrLFmt(str,255,'%s=%s',[Key,Val]);
  Ind:=list.IndexOfName(Key);
  if Ind >= 0 then list[Ind]:=str
  else list.Add(str)
end;

function tini.Get_str(list: TStrings; Key,Val: PChar): Boolean;
var
  l: Integer;
begin
  Result:=false;

  StrPLCopy(Val,list.Values[Key],255);

  l:=Strlen(Val); if l > 0 then begin

    if l > 1 then
    if Val[0] = '"' then
    if Val[l-1] = '"' then begin
      Val[l-1]:=#0; StrCopy(Val,@Val[1]);
    end;

    Result:=Strlen(Val) > 0
  end
end;

procedure tini.Del_key(list: TStrings; Key: PChar);
var
  i: Integer;
begin
  i:=list.IndexOfName(Key);
  if i >= 0 then list.Delete(i);
end;

function tini.GetDir(Key,Dir: PChar): Boolean;
var
  l1,l2: int;
begin
  Result:=false;
  if Get_str(fdir,Key,Dir) then
  if dir_Exists(Dir) then
    Result:=true
  else
  while true do begin
    l1:=Strlen(Dir);
    if l1 = 0 then Break;
    StrDirectory(Dir,Dir);
    l2:=Strlen(Dir);
    if l2 >= l1 then Break;

    if dir_Exists(Dir) then begin
      Result:=true; Break
    end
  end
end;

procedure tini.PutDir(Key,Dir: PChar);
begin
  Put_str(fdir,Key,Dir)
end;

function tini.GetIniDir(Key,Dir: PChar): Boolean;
var
  tmp: TShortstr;
begin
  Result:=false; StrCopy(Dir,'');

  if UsrDir(tmp,'/INI') <> nil then
  if dir_Exists(tmp) then begin
    StrCopy(dir,tmp); Result:=true
  end;

  if not Result then
  Result:=GetDir(Key,Dir)
end;

function tini.PutIniDir(Key,Dir: PChar): Boolean;
var
  tmp: TShortstr;
begin
  Result:=false;

  if UsrDir(tmp,'/INI') <> nil then
  if dir_Exists(tmp) then
  Result:=xStrComp(tmp,Dir);

  if not Result then
  PutDir(Key,Dir)
end;

function tini.GetFile(Key,Path: PChar): Boolean;
begin
  Result:=false;
  if Get_str(ffiles,Key,Path) then
  Result:=FileExist(Path)
end;

procedure tini.PutFile(Key,Path: PChar);
begin
  Put_str(ffiles,Key,Path)
end;

procedure tini.DelFile(Key: PChar);
begin
  Del_key(ffiles,Key)
end;

function tini.GetForm(Form: TForm): Boolean;
var
  key,str: TShortstr;
begin
  Result:=false;
  if Assigned(Form) then begin
    StrPCopy(key,Form.Name);
    if Get_str(fwin,key,str) then
    Result:=StrToForm(str,Form)
  end
end;

procedure tini.PutForm(Form: TForm);
var
  key,str: TShortstr;
begin
  if Assigned(Form) then begin
    StrPCopy(key,Form.Name);
    StrForm(str,Form);
    Put_str(fwin,key,str)
  end
end;

function tini.GetInt(Key: PChar; out v: Integer): Boolean;
var
  rc: Integer; s: TShortstr;
begin
  Result:=false; v:=0;
  if Get_str(ival,key,s) then begin
    Val(s,v,rc); if rc = 0 then
    Result:=true else v:=0
  end
end;

function tini.DefInt(v: Integer; Key: PChar): Integer;
begin
  Result:=v;
  if GetInt(Key,v) then
  Result:=v;
end;

function tini.DefEnum(v,max: int; Key: PChar): int;
var
  t: int;
begin
  if GetInt(Key,t) then v:=t;
  if v < 0 then v:=0;
  if v > max then v:=max;
  Result:=v;
end;

function tini.xGetInt(Key: PChar; eq: Integer): Boolean;
var
  v,rc: Integer; s: TShortstr;
begin
  Result:=false;
  if Get_str(ival,key,s) then begin
    Val(s,v,rc); if rc = 0 then
    Result:=v = eq
  end
end;

procedure tini.PutInt(Key: PChar; v: Integer);
var
  s: TShortstr;
begin
  StrPCopy(s,IntToStr(v));
  Put_val(ival,Key,s)
end;

procedure tini.DelInt(Key: PChar);
begin
  Del_key(ival,Key)
end;

function tini.GetCode(Key: PChar; out v: Integer): Boolean;
var
  s: TShortstr;
begin
  Result:=false; v:=0;
  if Get_str(cval,key,s) then
  if is_Code(Strpas(s)) then begin
    v:=StrToCode(Strpas(s));
    Result:=true
  end
end;

function tini.DefCode(v: Integer; Key: PChar): Integer;
begin
  Result:=v;
  if GetCode(Key,v) then
  Result:=v;
end;

procedure tini.PutCode(Key: PChar; v: Integer);
var
  s: TShortstr;
begin
  StrPCopy(s,CodeToStr(v));
  Put_val(cval,Key,s)
end;

procedure tini.DelCode(Key: PChar);
begin
  Del_key(cval,Key)
end;

function tini.DefBool(v: Boolean; Key: PChar): Boolean;
begin
  Result:=v;
  if GetBool(key,v) then
  Result:=v;
end;

function tini.GetBool(Key: PChar; out fl: Boolean): Boolean;
var
  v,rc: Integer; s: TShortstr;
begin
  Result:=false; fl:=false;
  if Get_str(bool,key,s) then begin
    Val(s,v,rc); if rc = 0 then
    if v in [0..1] then begin
      fl:=v = 1; Result:=true
    end
  end
end;

procedure tini.PutBool(Key: PChar; fl: Boolean);
var
  s: TShortstr;
begin
  StrPCopy(s,IntToStr(ord(fl)));
  Put_val(bool,Key,s)
end;

function tini.GetReal(Key: PChar; out v: Double): Boolean;
var
  rc: Integer; str: TShortstr;
begin
  Result:=false; v:=0;
  if Get_str(rval,key,str) then begin
    Val(str,v,rc); if rc = 0 then
    Result:=true else v:=0
  end
end;

function tini.defReal(Key: PChar; def: Double): Double;
var
  v: Double;
begin
  Result:=def;
  if GetReal(Key,v) then Result:=v
end;

procedure tini.PutReal(Key: PChar; v: Double);
var
  str: TShortstr;
begin
  StrPCopy(str,xRealToStr(v,6));
  Put_val(rval,Key,str)
end;

function tini.GetXyz(Key: PChar; out v: txyz): Boolean;
var
  s: TShortstr; t: txyz;
begin
  Result:=false; v:=_xyz(0,0,0);

  if GetStr(Key,s) then
  if RealToken(s,t.x) then
  if RealToken(s,t.y) then
  if RealToken(s,t.z) then begin
    v:=t; Result:=true
  end
end;

procedure tini.PutXyz(Key: PChar; const v: txyz);
var
  s: TShortstr;
begin
  StrPCopy(s,RealToStr(v.x,6)+' '+
             RealToStr(v.y,6)+' '+
             RealToStr(v.z,6));

  PutStr(Key,s)
end;

function tini.GetStr(Key,Str: PChar): Boolean;
begin
  Result:=Get_str(sval,Key,Str)
end;

procedure tini.PutStr(Key,Str: PChar);
begin
  Put_str(sval,Key,Str)
end;

procedure tini.DelStr(Key: PChar);
begin
  Del_key(sval,Key)
end;

function tini.GetPoint(Key: PChar; out p: TPoint): Boolean;
var
  s: TShortstr;
begin
  Result:=false; p:=Point(0,0);
  if Get_str(pval,key,s) then
  Result:=PointToken(s,p)
end;

function tini.defPoint(Key: PChar; X,Y: Integer): TPoint;
var
  p: TPoint;
begin
  Result:=Point(X,Y);
  if GetPoint(Key,p) then
  Result:=p
end;

procedure tini.PutPoint(Key: PChar; X,Y: Integer);
var
  s: TShortstr;
begin
  StrFmt(s,'%d %d',[X,Y]);
  Put_val(pval,Key,s)
end;

function tini.GetFont(Font: TFont; Key: PChar): Boolean;
var
  str: TShortStr;
begin
  Result:=false;
  if Get_str(ffonts,key,str) then
  Result:=FontToken(Font,str)
end;

procedure tini.PutFont(Font: TFont; Key: PChar);
var
  str: TShortStr;
begin
  StrFont(str,Font);
  Put_str(ffonts,Key,str)
end;

const
  Working_Directory = 'work_dir';

function tini.GetWorkDir: Boolean;
var
  dir: TShortStr;
begin
  if GetDir(Working_Directory,dir) then
  StrCopy(WorkDir,dir)
end;

initialization
begin
  InitScreen;
  ini:=tini.Create(nil);
end;

finalization
  ini.Free;

end.