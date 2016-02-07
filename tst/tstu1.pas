unit tstu1;

//{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, freetypep;

type

  { TDfmTstu1 }

  TDfmTstu1 = class(TForm)
    lbLog: TListBox;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbLogMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fonts: TFontManager;

    procedure log(const Msg: String);

  public
  end;

var
  DfmTstu1: TDfmTstu1;

implementation {$R *.lfm}

uses
  linux,
  dynlibs,

  LCLType,

  activeX,
  otypes,
  convert,
  ofiles,
  xdc,
  xutils,
  xpoly,
  xbl_use,

  xpole,
  storage,

  xvirts,
  vbanks,
  xlist,
  xini,

  freetypeh;

{ TDfmTstu1 }

procedure TDfmTstu1.FormCreate(Sender: TObject);
begin
  fonts:=TFontManager.Create;
  ini.GetForm(Self);
end;

procedure TDfmTstu1.FormDestroy(Sender: TObject);
begin
  ini.PutForm(Self);
  fonts.Free;
end;

procedure TDfmTstu1.lbLogMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TDfmTstu1.log(const Msg: String);
begin
  lbLog.Items.Add(Msg)
end;

procedure TDfmTstu1.FormActivate(Sender: TObject);

procedure test_dll_bl;
var
  elp: int; a,b: double;
begin
  log('test_dll_bl');

  for elp:=1 to Ellipsoids_Count do begin
    Get_Ellipsoid(elp,a,b);
    log(Format('[%d]=%0.3f %0.3f',[elp,a,b]))
  end;
end;

procedure test_dll_poly;
var
  l: TLPoly; v: TLLine; len,sqr,cx,cy: double;
begin
  log('');
  log('lib_poly');

  l[0]:=Point(0,0);
  l[1]:=Point(100,100);
  l[2]:=Point(200,0);
  l[3]:=l[0];

  v.N:=3; v.Pol:=l;

  len:=PolyLength(@l,3);
  sqr:=Square_Poly(@l,3);
  Centre_Poly(@v,cx,cy);

  log(Format('PolyLength=%0.2f',[len]));
  log(Format('Square_Poly=%0.2f',[sqr]));
  log(Format('Centre_Poly=%0.2f %0.2f',[cx,cy]));
end;

procedure test_xdc;
var
  dc: HDC;
begin
  dc:=xGetDC(0);
  if dc <> 0 then begin
    log(Format('DeviceRatio=%0.3f',[xDeviceRatio(dc)]));
    log(Format('DeviceAspect=%0.3f',[xDeviceAspect(dc)]));
    log(Format('DeviceDpi=%0d',[xDevice_dpi(dc)]));
    log(Format('DeviceMpp=%0.6f',[xMetersPerPixel(dc)]));

    log(Format('BitsPerPixel=%0d',[xBitsPerPixel(dc)]));
    log(Format('PlanesPerPixel=%0d',[xPlanesPerPixel(dc)]));
  end;

  xReleaseDC(0,dc)
end;

procedure test_sysInfo;
const
  m1 = 1024 * 1024;
var
  inf: TSysInfo; rc: uint;
begin
  Fillchar(inf,Sizeof(inf),0);
  rc:=Sysinfo(@inf);

  log('');
  with inf do begin
    log(Format('rc=%d',[rc]));

    log(Format('uptime=%d',[uptime]));
    log(Format('totalram=%dM',[totalram div m1]));
    log(Format('freeram=%dM',[freeram div m1]));
    log(Format('sharedram=%dM',[sharedram div m1]));
    log(Format('bufferram=%dM',[bufferram div m1]));
    log(Format('totalswap=%dM',[totalswap div m1]));
    log(Format('freeswap=%dM',[freeswap div m1]));
    log(Format('procs=%d',[procs]));
  end;

  log('');
  log(Format('GetTotalMemorySize=%dM',[GetTotalMemorySize]));
  log(Format('GetAvailMemorySize=%dM',[GetAvailMemorySize]));
end;

procedure test_vm;

var
  vm: TReadFile;
  fh1,sz: int; rc: Boolean;
begin
  log('');

  log(Format('PointSize=%d',[sizeof(TPoint)]));
  log(Format('CharSize=%d',[sizeof(Char)]));

  rc:=FileExist('/home/emm/tol/dmw/Convert.pas');
  log(Format('FileExist rc=%d',[ibool[rc]]));

  vm:=TReadFile.Create;
  try
    if not vm.Open('/home/emm/tol/dmw/Convert.pas') then
      log('File "Convert.pas" not found')
    else begin
      log(Format('File "Convert.pas" opened: size=%d',[vm.Size]));
      log(Format('FileDateTime=%s',[DateTimeToStr(vm.Date)]));

      sz:=vm.Size;
      if sz > 0 then begin
        fh1:=FileCreate('/home/emm/temp/Convert.pas');
        if fh1 > 0 then begin
          FileWrite(fh1,vm.Buf^,sz); FileClose(fh1);
          log('File "Convert.pas" copy to folder <tmp>')
        end;
      end
    end
  finally
    vm.Free;
  end
end;

procedure test_ofiles;
var
  s: TPathStr;
begin
  log('');

  StrHomeDir(s);
  log(Format('HomeDir="%s"',[s]));
end;

procedure test_pole;

procedure tree_stg(const stg: IStorage;
                   Name: PChar; lev: int);

function levStr(lev: int): String;
var
  s: String;
begin
  s:='';
  while lev > 0 do begin
    s:=s+'  '; Dec(lev)
  end; Result:=s
end;

var
  fld: IStorage;
  list: TStringList;
  i: int; nm: TShortstr;
  s: String;
begin
  list:=TStringList.Create;
  try
    log(Format('%s[%s]',[levStr(lev),Name]));

    if xEnumStreams(stg,list) > 0 then
    for i:=0 to list.Count-1 do begin
      StrPCopy(nm,list[i]);

      s:=Format('stream[%s] size=%d',[nm,xSizeStream(stg,nm)]);
      log(levStr(lev+1)+s)
    end;

    if xEnumFolders(stg,list) > 0 then
    for i:=0 to list.Count-1 do begin
      StrPCopy(nm,list[i]);
      if xOpenFolder(stg,nm,ole_Read,fld) then
      tree_stg(fld,nm,lev+1)
    end;

    log(Format('%s[end_%s]',[levStr(lev),Name]));
  finally
    list.Free
  end
end;

procedure sf_tree(const stg: TStorageIO);
var
  sf: TStreamIO; root,cb: int; pos: int64;
begin
  sf:=stg.streamIO('sf');
  if Assigned(sf) then begin
    sf.Seek(16,0,pos);
    sf.Read(@root,4,@cb);
    log(Format('sf root=%d cb=%d',[root,cb]));
  end
end;

var
  fn: TShortstr;
  istg: IStorage;
  pstg: TStorageIO;
begin
  log('');
  log('test_pole');

  StrCopy(fn,'/home/emm/neva/data/smol.dm');

  if not xStgIsStorageFile(fn) then
    log(Format('"%s" not is storage file.',[fn]))
  else
  if xOpenStorage(fn,ole_Read,istg) <> S_OK then
    log(Format('OpenStorage "%s" false.',[fn]))
  else begin
    log(Format('OpenStorage "%s" ok.',[fn]));
    tree_stg(istg,'Root',0);
  end;

  istg:=nil;

  pstg:=TStorageIO.Create;
  try
    if pstg.open(fn) then
    sf_tree(pstg);
  finally
    pstg.Free;
  end;
end;

procedure test_fonts;
const
  ttf = 'bm431n';
var
  txt: TTextfile;
  ifnt,i: int; s: String; face: PFT_Face;
  tlist: TStringBitmaps; b: TRect; bmp: PFontBitmap;
  fn: TShortstr;
begin
  log('');

  if not fonts.Enabled then
    log('freetype.so not enabled')
  else begin
    s:=Format('version %d.%d.%d',[fonts.major,fonts.minor,fonts.patch]);
    log(s);

    ifnt:=fonts.RequestFont(ttf,0);
    if ifnt < 0 then
      log(ttf+'.ttf not loaded.')
    else begin
      log(ttf+'.ttf open.');
      face:=fonts.GetFreeTypeFont(ifnt);

      txt:=TTextfile.Create;
      try
        StrPCopy(fn,ttf+'.log');
        if txt.Make_work(fn) then begin
          dump_face(txt,face);
          log(ttf+'.log created.')
        end
      finally
        txt.Free;
      end;

      tlist:=fonts.GetString(ifnt,
                             'Привет Вася',
                             16,0,true);
      try
        if Assigned(tlist) then begin
          log('Привет Вася: OK');

          tlist.GetBoundRect(b);
          s:=Format('rect: %d %d %d %d',[b.Left,b.Top,b.Right,b.Bottom]);
          log(s);

          for i:=0 to tlist.Count-1 do begin
            bmp:=tlist.Bitmaps[i];
            with bmp^ do
            s:=Format('%d: %dx%d %d (%d %d) (%d %d)',
              [i,height, width, pitch, x,y, advanceX, advanceY]);
            log(s)
          end
        end;

      finally
        tlist.Free
      end;

    end;
  end;
end;

begin
  Caption:=ParamStr(0);

//  test_dll_t1;
//  test_dll_poly;
//  test_dll_bl;

//  test_xdc;
//  test_sysInfo;
//  test_vm;
//  test_ofiles;

  test_pole;
  test_fonts;
end;

end.

