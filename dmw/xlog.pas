unit xlog; interface

uses
  Classes,otypes,xintf,ofiles;

type
  TLogfile = class(TInterfacedObject,ILog)
    constructor Create;
    destructor Destroy; override;

    function New(Path: PChar): Boolean;
    function Open(Path: PChar): Boolean;

    function New_work(Name: PChar): Boolean;
    function Open_work(Name: PChar): Boolean;

    procedure Add(Msg: PChar); stdcall;
    procedure Add_time(Msg: PChar; dt: Longint); stdcall;
    procedure Begin_section(Msg,Alt: PChar); stdcall;
    procedure End_section(Msg,Alt: PChar); stdcall;
    procedure Ok_section(Msg: PChar; ok: LongBool); stdcall;

    procedure sAdd(const Msg: ShortString); stdcall;

  private
    ftxt: TTextFile;
    flev: int;
    fSect: TShortstr;
  end;

  TLog = class
    destructor Destroy; override;

    function New(Path: PChar): Boolean;
    function NewWork(Name: PChar): Boolean;

    procedure Add(Msg: PChar);
    procedure sAdd(const Msg: String);

    procedure AddStrings(Text: TStrings);

    procedure Add_str(Name,Str: PChar);

    procedure Add_int(Name: PChar; Val: int);
    procedure Add_real(Name: PChar; Val: double; m: int);

  private
    fIntf: ILog;
  public
    property Intf: ILog read fIntf;
  end;

var
  std_log: TTextFile;
  std_err: TTextFile;

function GetLogWorkIntf(Name: PChar; var Obj): Boolean;

implementation

uses
  Windows,Sysutils,Convert;

procedure Init;
var
  dir,nm,fn: TShortstr;
begin
  xGetModuleFileName(nm,255);
  StrNameExt(nm,nm); StrUpdateExt(nm,nm,'');

  StrTempDir(dir);
  if not dir_Exists(dir) then
  StrCopy(dir,WorkDir);

  std_log:=TTextFile.Create;

  StrCopy(fn,nm);
  StrLCat(fn,'_log',255);
  StrPath(fn,dir,fn);
  std_log.Make(fn);

  std_err:=TTextFile.Create;

  StrCopy(fn,nm);
  StrLCat(fn,'_err',255);
  StrPath(fn,dir,fn);
  std_err.Make(fn);

  std_log.Write_str2('BinDir',BinDir);
end;

procedure Done;
begin
  std_err.Free;
  std_log.Free
end;

constructor TLogfile.Create;
begin
  inherited;
  ftxt:=TTextFile.Create
end;

destructor TLogfile.Destroy;
begin
  ftxt.Free; inherited
end;

function TLogfile.New(Path: PChar): Boolean;
begin
  Result:=ftxt.Make(Path)
end;

function TLogfile.Open(Path: PChar): Boolean;
begin
  Result:=ftxt.Open(Path)
end;

function TLogfile.New_work(Name: PChar): Boolean;
var
  fn: TShortstr;
begin
  StrWorkPath(fn,Name);
  Result:=New(fn)
end;

function TLogfile.Open_work(Name: PChar): Boolean;
var
  fn: TShortstr;
begin
  StrWorkPath(fn,Name);
  Result:=Open(fn)
end;

procedure TLogfile.Add(Msg: PChar);
var
  i: Integer; s: TShortstr;
begin
  if ftxt.Active then begin
    StrCopy(s,''); for i:=1 to flev do
    StrCat(s,#9); StrCat(s,Msg);
    ftxt.WriteAnsi(s)
  end
end;

procedure TLogfile.Add_time(Msg: PChar; dt: Longint);
var
  s: TShortstr;
begin
  if ftxt.Active then begin
    StrCopy(s,''); if Assigned(Msg) then
    StrCat(StrCopy(s,Msg),': ');
    pStrCat(s,RealToStr(dt/1000,3)+'sec');
    ftxt.WriteAnsi(s)
  end
end;

procedure TLogfile.Begin_section(Msg,Alt: PChar);
var
  s: TShortstr;
begin
  StrCopy(s,Msg); StrCat(s,'_begin');
  if xStrlen(Alt) > 0 then StrCat(StrCat(s,': '),Alt);
  StrCat(s,'.'); Add(s); Inc(flev)
end;

procedure TLogfile.End_section(Msg,Alt: PChar);
var
  s: TShortstr;
begin
  if flev > 0 then Dec(flev);

  if Msg = nil then StrCopy(s,fSect)
               else StrCopy(s,Msg);

  StrCat(s,'_end');
  if xStrlen(Alt) > 0 then
  StrCat(StrCat(s,': '),Alt);

  StrCat(s,'.'); Add(s); Add('')
end;

procedure TLogfile.Ok_section(Msg: PChar; ok: LongBool);
begin
  if ok then End_section(Msg,'ok')
        else End_section(Msg,'false')
end;

procedure TLogfile.sAdd(const Msg: ShortString);
begin
  ftxt.WriteStr(Msg);
end;

destructor TLog.Destroy;
begin
  fIntf:=nil; inherited
end;

function TLog.New(Path: PChar): Boolean;
var
  obj: TLogfile;
begin
  Result:=false; fIntf:=nil;

  obj:=TLogfile.Create;
  try
    if obj.New(Path) then
    if obj.GetInterface(ILog,fIntf) then begin
      Result:=true; obj:=nil
    end;
  finally
    obj.Free
  end
end;

function TLog.NewWork(Name: PChar): Boolean;
var
  fn: TShortstr;
begin
  StrWorkPath(fn,Name);
  Result:=New(fn)
end;

procedure TLog.Add(Msg: PChar);
begin
  if Assigned(fIntf) then
  if Msg[0] = #0 then fIntf.Add(' ')
                 else fIntf.Add(Msg)
end;

procedure TLog.sAdd(const Msg: String);
begin
  if Assigned(fIntf) then fIntf.sAdd(Msg)
end;

procedure TLog.AddStrings(Text: TStrings);
var
  i: int;
begin
  if Assigned(fIntf) then
  for i:=0 to Text.Count-1 do
  fIntf.sAdd(Text[i])
end;

procedure TLog.Add_str(Name,Str: PChar);
begin
  sAdd(Format('%s: %s',[Name,Str]))
end;

procedure TLog.Add_int(Name: PChar; Val: int);
begin
  sAdd(Format('%s: %d',[Name,Val]))
end;

procedure TLog.Add_real(Name: PChar; Val: double; m: int);
begin
  sAdd(Strpas(Name)+': '+RealToStr(Val,m))
end;

function GetLogWorkIntf(Name: PChar; var Obj): Boolean;
var
  log: TLogfile;
begin
  Result:=false; TPointer(Obj):=0;

  log:=TLogfile.Create;
  try
    if log.New_work(Name) then
    if log.GetInterface(ILog,Obj) then begin
      Result:=true; log:=nil
    end;
  finally
    log.Free
  end
end;

initialization Init;
finalization Done;

end.
