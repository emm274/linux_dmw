unit use_sas; interface

uses
  Classes,Contnrs,
  otypes,xlist,xproj;

type
  TSasChart = class

    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Assign(Chart: TSasChart);

    function Backup(const sas: IGoogle): Boolean;

    procedure Apply(const sas: IGoogle);

    function GetChart(Cache,Str: PChar): Boolean;
    function SetChart(Cache,Str: PChar): Boolean;

    function SetMapLayer(Cache,Name: PChar): int;
    function SetSklLayer(Cache,Name: PChar; Up: int): int;
    procedure RefreshCaption(Cache: PChar);

    function StrChart(Str: PChar): Boolean;

    function StrToChart(Str: PChar): Boolean;

    function GetStr(Str: PChar): Boolean;
    function SetStr(Str: PChar): Boolean;

  private
    fmapId: int;
    fsklList: TIntegerList;
    fCaption: String;

    fEnabled: Longbool;

    function GetActive: Boolean;

  public
    property Active: Boolean read GetActive;
    property mapId: int read fmapId write fmapId;
    property sklList: TIntegerList read fsklList;
    property Caption: String read fCaption;
    property Enabled: Longbool read fEnabled
                               write fEnabled;
  end;

  TSasChartList = class(TObjectList)
    constructor Create(maxCount: int);
    procedure Reset;

    procedure ini_save;
    procedure ini_load;

    procedure addChart(chart: TSasChart);
    procedure delChart(capt: PChar);

    function GetStrings(List: TStrings): int;
  end;

function sasGetInterface(Dir: PChar; out Obj): HResult;
procedure sasHttpRequest(key,data: PChar);

implementation

uses
  dynlibs,Sysutils,
  convert,ofiles,xini;

constructor TSasChart.Create;
begin
  inherited Create;
  fsklList:=TIntegerList.Create;
  fEnabled:=true
end;

destructor TSasChart.Destroy;
begin
  fsklList.Free;
  inherited
end;

function TSasChart.GetActive: Boolean;
begin
  Result:=(fmapId > 0) or (fsklList.Count > 0)
end;

procedure TSasChart.Clear;
begin
  fmapId:=0;
  fsklList.Clear;
  fCaption:=''
end;

procedure TSasChart.Assign(Chart: TSasChart);
begin
  fmapId:=Chart.mapId;
  fsklList.LoadList(Chart.sklList);
  fCaption:=Chart.Caption
end;

function TSasChart.Backup(const sas: IGoogle): Boolean;
var
  id,i: int;
begin
  Clear;

  id:=sas.GetParam($50000);
  if id > 0 then fmapId:=id;

  for i:=0 to 99 do begin
    id:=sas.GetParam($60000+i);
    if id < 0 then Break;
    if id > 0 then fsklList.AddItem(id)
  end;

  Result:=Active
end;

procedure TSasChart.Apply(const sas: IGoogle);
var
  i,rc: int; ip: PIntegers;
begin
  sas.SelectMap(fmapId);

  i:=0; ip:=fSklList.First;
  while i < fsklList.Count do begin
    rc:=sas.SelectSkl(ip[i],1);
    Inc(i); if rc < 0 then begin
      Dec(i); fsklList.Delete(i)
    end
  end;
end;

function TSasChart.GetChart(Cache,Str: PChar): Boolean;
var
  sas: IGoogle; i,j,n,id,id1: int;
  ip: PIntegers; nm: TShortstr;
begin
  Result:=false; StrCopy(Str,'');

  if sasGetInterface(Cache,sas) = S_OK then begin

    StrCopy(Str,'-');
    if fmapId > 0 then begin
      n:=sas.GetMapCount;
      for i:=0 to n-1 do begin
        id:=sas.GetMapName(i,nm);
        if id = fmapId then begin
          StrCopy(Str,nm); Break
        end
      end
    end;

    ip:=fsklList.First;
    for i:=0 to fsklList.Count-1 do begin
      id1:=ip[i];

      n:=sas.GetSklCount;
      for j:=0 to n-1 do begin
        id:=sas.GetSklName(i,nm);
        if id = id1 then begin
          StrLCat(Str,' ',255);
          StrLCat(Str,nm,255);
          Break
        end
      end
    end;

    sas:=nil
  end;
end;

function TSasChart.SetChart(Cache,Str: PChar): Boolean;
var
  sas: IGoogle; i,n,id: int; s1,s2: TShortstr;
begin
  Clear;

  if Strlen(Str) > 0 then
  if StrToken(s1,Str) <> nil then

  if sasGetInterface(Cache,sas) = S_OK then begin

    if StrComp(s1,'-') <> 0 then begin
      n:=sas.GetMapCount;
      for i:=0 to n-1 do begin
        id:=sas.GetMapName(i,s2);
        if id > 0 then
        if StrIComp(s1,s2) = 0 then begin
          fmapId:=id; Break
        end
      end
    end;

    while StrToken(s1,Str) <> nil do begin
      n:=sas.GetSklCount;
      for i:=0 to n-1 do begin
        id:=sas.GetSklName(i,s2);
        if id > 0 then
        if StrIComp(s1,s2) = 0 then begin
          if fsklList.IndexOf(id) < 0 then
          fsklList.AddItem(id);
          Break
        end
      end
    end;

    sas:=nil
  end;

  Result:=Active
end;

function TSasChart.SetMapLayer(Cache,Name: PChar): int;
var
  sas: IGoogle; i,n,id: int; nm: TShortstr;
begin
  fmapId:=0;

  if Assigned(Name) then
  if Strlen(Name) > 0 then

  if sasGetInterface(Cache,sas) = S_OK then begin
    n:=sas.GetMapCount;
    for i:=0 to n-1 do begin
      id:=sas.GetMapName(i,nm);
      if id > 0 then
      if StrIComp(nm,Name) = 0 then begin
        fmapId:=id; Break
      end
    end;

    sas:=nil
  end;

  Result:=fmapId
end;

function TSasChart.SetSklLayer(Cache,Name: PChar; Up: int): int;
var
  sas: IGoogle; i,n,id: int; nm: TShortstr;
begin
  Result:=-1;

  if Assigned(Name) then
  if Strlen(Name) = 0 then
  Name:=nil;

  if Name = nil then
    fsklList.Clear
  else
  if sasGetInterface(Cache,sas) = S_OK then begin
    n:=sas.GetSklCount;
    for i:=0 to n-1 do begin
      id:=sas.GetSklName(i,nm);
      if id > 0 then
      if StrIComp(nm,Name) = 0 then begin

        if Up = 0 then
          fsklList.DeleteItem(id)
        else
        if fsklList.IndexOf(id) < 0 then
          fsklList.AddItem(id);

        Result:=id; Break
      end
    end;

    sas:=nil
  end
end;

procedure TSasChart.RefreshCaption(Cache: PChar);
var
  sas: IGoogle; i,id,ind: int;
  ip: PIntegers; s,t: TShortstr;
begin
  StrCopy(s,'');

  if Active then
  if sasGetInterface(Cache,sas) = S_OK then begin

    sas.SetParam(1,1);  // IsExtCapt=true

    id:=fmapId;
    if id > 0 then begin
      ind:=sas.GetParam($20000+id);
      if sas.GetMapCapt(ind,t) > 0 then
      StrCopy(s,t)
    end;

    ip:=fsklList.First;
    for i:=0 to fsklList.Count-1 do begin
      id:=ip[i];
      if id > 0 then begin
        ind:=sas.GetParam($30000+id);
        if sas.GetSklCapt(ind,t) > 0 then begin
          if s[0] <> #0 then
          StrLCat(s,' + ',255);
          StrLCat(s,t,255)
        end
      end;
    end;

    sas.SetParam(1,0);  // IsExtCapt=false
  end;

  fCaption:=Strpas(s);
end;

function TSasChart.StrChart(Str: PChar): Boolean;
var
  i,id: int; ip: PIntegers; t: TShortstr;
begin
  Result:=false;

  if Active then begin

    StrFmt(Str,'%d',[fmapId]);

    ip:=fsklList.First;
    for i:=0 to fsklList.Count-1 do begin
      id:=ip[i];
      if id > 0 then begin
        StrPCopy(t,IntToStr(id));
        StrLCat(Str,' ',255);
        StrLCat(Str,t,255);
      end
    end;

    Result:=true
  end
end;

function TSasChart.StrToChart(Str: PChar): Boolean;
var
  id: int;
begin
  Clear;

  if IntToken(Str,id) then begin
    fmapId:=id;
    while IntToken(Str,id) do
    if id > 0 then fsklList.AddItem(id);
  end;

  Result:=Active
end;

function TSasChart.GetStr(Str: PChar): Boolean;
var
  s: TShortstr;
begin
  Result:=false;

  if StrChart(s) then
  if Length(fCaption) > 0 then begin
    StrFmt(Str,'"%s" %s',[fCaption,s]);
    Result:=true
  end
end;

function TSasChart.SetStr(Str: PChar): Boolean;
var
  id: int; t: TShortstr;
begin
  Clear;

  if StrToken(t,Str) <> nil then
  if Strlen(t) > 0 then
  if IntToken(Str,id) then begin

    fmapId:=id;
    fCaption:=Strpas(t);

    while IntToken(Str,id) do
    if id > 0 then fsklList.AddItem(id);

    if not Active then Clear
  end;

  Result:=Active
end;

constructor TSasChartList.Create(maxCount: int);
var
  i: int; chart: TSasChart;
begin
  inherited Create;

  for i:=0 to 3 do begin
    chart:=TSasChart.Create;
    Add(chart)
  end
end;

procedure TSasChartList.Reset;
var
  i: int; chart: TSasChart;
begin
  for i:=0 to Count-1 do begin
    chart:=Items[i] as TSasChart;
    chart.Clear
  end
end;

procedure TSasChartList.ini_save;
var
  chart: TSasChart;
  i,ind: int; s,t: TShortstr;
begin
  ind:=0;
  for i:=0 to Count-1 do begin
    chart:=Items[i] as TSasChart;

    if chart.GetStr(s) then begin
      Inc(ind);
      StrFmt(t,'sasUser%d',[ind]);
      ini.PutStr(t,s)
    end
  end
end;

procedure TSasChartList.ini_load;
var
  chart: TSasChart;
  i,j: int; s,t: TShortstr;
begin
  Reset; j:=0;

  for i:=0 to Count-1 do begin

    StrFmt(t,'sasUser%d',[i+1]);
    if ini.GetStr(t,s) then begin
      chart:=Items[j] as TSasChart;
      if chart.SetStr(s) then Inc(j)
    end
  end
end;

procedure TSasChartList.addChart(chart: TSasChart);
var
  i,n: int; l1,l2: TSasChart;
begin
  n:=Count;
  if n > 0 then begin

    for i:=n-1 downto 1 do begin
      l1:=Items[i] as TSasChart;
      l2:=Items[i-1] as TSasChart;
      l1.Assign(l2);
    end;

    l1:=Items[0] as TSasChart;
    l1.Assign(chart)
  end
end;

procedure TSasChartList.delChart(capt: PChar);
var
  i,j,n: int; l1,l2: TSasChart; s: String;
begin
  s:=Strpas(capt);

  n:=Count;
  if n > 0 then
  for i:=0 to n-1 do begin
    l1:=Items[i] as TSasChart;
    if l1.Caption = s then begin

      for j:=i+1 to n-1 do begin
        l2:=Items[j] as TSasChart;
        l1.Assign(l1)
      end;

      l1.Clear
    end
  end
end;

function TSasChartList.GetStrings(List: TStrings): int;
var
  i: int; chart: TSasChart;
begin
  Result:=0;
  for i:=0 to Count-1 do begin
    chart:=Items[i] as TSasChart;
    if Length(chart.Caption) > 0 then begin
      List.Add(chart.Caption);
      Inc(Result)
    end
  end
end;

var
  dll_sas: THandle;

procedure Init;
begin
  dll_sas:=0;
end;

procedure Done;
begin
  xFreeLibrary(dll_sas);
end;

function sasGetInterface(Dir: PChar; out Obj): HResult;
type
  tfunc = function(Dir: PChar; out Obj): HResult; stdcall;
var
  func: tfunc;
begin
  Result:=S_FALSE; TPointer(Obj):=0;

  if dll_sas = 0 then
  dll_sas:=LoadLibrary('dll_sas.dll');

  if dll_sas >= 32 then begin
    @func:=GetProcAddress(dll_sas,'sasGetInterface');
    if Assigned(func) then Result:=func(Dir,Obj);
  end
end;

procedure sasHttpRequest(key,data: PChar);
type
  tproc = procedure(key,data: PChar); stdcall;
var
  proc: tproc;
begin
  if dll_sas = 0 then
  dll_sas:=LoadLibrary('dll_sas.dll');

  if dll_sas >= 32 then begin
    @proc:=GetProcAddress(dll_sas,'sasHttpRequest');
    if Assigned(proc) then proc(key,data);
  end
end;

initialization Init;
finalization Done;

end.