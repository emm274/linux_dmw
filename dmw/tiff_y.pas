unit tiff_y; interface

uses
  Windows,
  OTypes,OFrames,XList,XLine;

const
  GeoSubKeyMax = 32;

type
  tiff_hdr = record cod,ver: word; ind: uint end;
  tiff_dir32 = record tag,typ: word; len,ind: uint end;
  tiff_dir64 = record tag,typ: word; len,ind: int64 end;

  ptiff_dir_arr64 = ^ttiff_dir_arr64;
  ttiff_dir_arr64 = Array[0..63] of tiff_dir64;

  TTiffDirList = class(TCustomList)
    constructor Create(AIsBig,AIsMac: bool);
    function Load(DS: IFile; var pos: int64): bool;
  private
    fIsBig,fIsMac: bool;
  public
    property IsBig: bool read fIsBig;

    property IsMac: bool read fIsMac
                         write fIsMac;
  end;

  TTiffData = class(TTiffDirList)
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

    function xAdd(const dir: tiff_dir64; ds: Pointer): int;
    function xSeek(var dir: tiff_dir64): Pointer;

  private
    fBuffer: TDataStream;
  end;

  PTiepoint = ^TTiepoint;
  TTiepoint = record a,b: txyz end;

  PTieFrame = ^TTieFrame;
  TTieFrame = Array[0..3] of TTiePoint;

  PTiepoints = ^TTiepoints;
  TTiepoints = Array[0..1023] of TTiepoint;

  TTiepointList = class(TCustomList)
    constructor Create;
  end;

  TGeoSubKey = record
    w1,w2,w3,w4: Word
  end;

  TGeoSubKeys = Array[0..GeoSubKeyMax-1] of TGeoSubKey;

  TGeoTiff = class

    constructor Create;
    destructor Destroy; override;

    function Open(Path: PChar): Boolean;
  private
    fFile: IFile;
    fSpy: TTiffData;

    fPoints: TTiePointList;

    fWidth: Integer;
    fHeight: Integer;
    fMatrix: Real3x3;

    fsys: tsys;
    fisBig: Longbool;
    fisMac: Longbool;

    fGTCitation: TShortstr;
    fGeogCitation: TShortstr;
    fCSCitation: TShortstr;

  public
    property Spy: TTiffData write fSpy;
    property Matrix: Real3x3 read fMatrix;
    property Points: TTiePointList read fPoints;
    property Width: Integer read fWidth;
    property Height: Integer read fHeight;
    property sys: tsys read fsys;
  end;

const
  rsw_magic = $00575352;

type
  trswHdr = record
    mag: uint;
    ver: uint;
    len: uint;
    thumb: uint;
    sit: uint;
    name: char32;
    bits: uint;
    height: uint;
    width: uint;
    xtiles: int;
    ytiles: int;
    tileh: uint;
    tilew: uint;
    tileh1: uint;
    tilew1: uint;
    ramkaOfs: uint;
    ramkaLen: uint;
    palOfs: uint;
    palLen: uint;
    tileOfs: uint;
    tileLen: uint;
    res1: uint;
    res2: uint;
    res3: uint;
    typ: int;
    prj: int;
    res4: uint;
    scale: double;
    ppm: double;
    mpp: double;
    XPos: double;
    YPos: double;
    b1: double;
    b2: double;
    lc: double;
    b3: double;
    comp: byte;
    tmask: byte;
    smask: byte;
    fmask: byte;
    bitOfs: uint;
    bitLen: uint;
    len0: uint;
    len1: uint;
    res5: int64;
    res6: int;
    res7: int;
    link: byte;
    inv: byte;
    res8: Word;
    fhide: TByte32;
    minh: double;
    maxh: double;
    nilh: double;
    ed: uint;
    mtw: byte;
    res9: TByte11;
  end;

function GeoSubKey(w1,w2,w3,w4: Integer): TGeoSubKey;

function tiff_get_geo(Geo: TTiffData; Path: PChar): Integer;

implementation

uses
  Sysutils,
  OFiles,OFiles1,
  OGauss,xyz,xbl_use;

function GeoSubKey(w1,w2,w3,w4: Integer): TGeoSubKey;
var
  r: TGeoSubKey;
begin
  r.w1:=w1; r.w2:=w2; r.w3:=w3; r.w4:=w4;
  Result:=r
end;

constructor TTiffDirList.Create(AIsBig,AIsMac: bool);
begin
  inherited Create(Sizeof(tiff_dir64),32);
  fIsBig:=AIsBig; fIsMac:=AIsMac
end;

function TTiffDirList.Load(DS: IFile; var pos: int64): bool;
type
  ptiff_dir_arr32 = ^ttiff_dir_arr32;
  ttiff_dir_arr32 = Array[0..63] of tiff_dir32;
var
  i: int; bx,n: int64; cx: uint;
  si,di: ptiff_dir_arr64; r: tiff_dir64;
  si_: ptiff_dir_arr32; r_: tiff_dir32;
begin
  Result:=false; Clear;

  bx:=pos; if bx > 0 then
  if DS.IsData(bx,16) then begin

    n:=0;
    if IsBig then begin
      bx:=DS.Load(bx,n,8);
      if fIsMac then n:=SwapInt64(n);
    end
    else begin
      bx:=DS.Load(bx,n,2);
      if fIsMac then n:=SwapWord(n);
    end;

    if n > 0 then
    if n <= 1024 then begin

      cx:=n*Sizeof(tiff_dir32);
      if fIsBig then cx:=n*Sizeof(tiff_dir64);

      si:=DS.Seek(bx,cx);
      if Assigned(si) then
      if Resize(n) then begin

        di:=First; si_:=Pointer(si);

        for i:=0 to n-1 do begin
          if fIsBig then begin
            r:=si[i];

            if fisMac then begin
              r.tag:=SwapWord(r.tag);
              r.typ:=SwapWord(r.typ);
              r.len:=SwapInt64(r.len);
              r.ind:=SwapInt64(r.ind)
            end
          end
          else begin
            r_:=si_[i];

            if fisMac then begin
              r_.tag:=SwapWord(r_.tag);
              r_.typ:=SwapWord(r_.typ);

              r_.len:=SwapInt(r_.len);

              if (r_.typ = 3) and (r_.len = 1) then
                r_.ind:=SwapWord(r_.ind)
              else
                r_.ind:=SwapInt(r_.ind);
            end;

            r.tag:=r_.tag;
            r.typ:=r_.typ;
            r.len:=r_.len;
            r.ind:=r_.ind
          end;

          di[i]:=r
        end;

        pos:=bx + cx; Result:=true
      end
    end
  end
end;

constructor TTiffData.Create;
begin
  inherited Create(false,false);
  fBuffer:=TDataStream.Create(1024)
end;

destructor TTiffData.Destroy;
begin
  fBuffer.Free; inherited
end;

procedure TTiffData.Clear;
begin
  inherited; fBuffer.Size:=0
end;

function TTiffData.xAdd(const dir: tiff_dir64; ds: Pointer): int;
var
  r: tiff_dir64; loc: int;
begin
  Result:=-1;

  r:=dir; loc:=r.len;
  if r.typ = 3 then loc:=loc * 2 else
  if r.typ = 4 then loc:=loc * 4 else
  if r.typ = 5 then loc:=loc * 8 else
  if r.typ = 12 then loc:=loc * 8;

  if r.len = 1 then
  if r.typ in [1,3,4] then
  loc:=0;

  if ((loc = 0) and (ds = nil))
  or ((loc > 0) and (ds <> nil)) then begin

    if Assigned(ds) then
    r.ind:=fBuffer.Append(ds^,loc);

    Result:=Add(@r)
  end
end;

function TTiffData.xSeek(var dir: tiff_dir64): Pointer;
var
  r: tiff_dir64; loc: int;
begin
  Result:=nil;

  r:=dir; loc:=r.len;
  if r.typ = 3 then loc:=loc * 2 else
  if r.typ = 4 then loc:=loc * 4 else
  if r.typ = 5 then loc:=loc * 8 else
  if r.typ = 12 then loc:=loc * 8;

  if loc > 0 then begin dir.len:=loc;
    Result:=fBuffer.Get_Pointer(r.ind)
  end
end;

constructor TTiepointList.Create;
begin
  inherited Create(Sizeof(TTiePoint),8)
end;

constructor TGeoTiff.Create;
begin
  inherited;
  fPoints:=TTiePointList.Create;
end;

destructor TGeoTiff.Destroy;
begin
  fFile:=nil;
  fPoints.Free;
  inherited
end;

function TGeoTiff.Open(Path: PChar): Boolean;

function Read_int(pos: int64; typ: int): int;
begin
  Result:=0;
  if typ = 3 then begin
    Result:=fFile.Get_word(pos);
    if fisMac then
    Result:=SwapWord(Result);
  end
end;

procedure dir_rational(var dir: tiff_dir64);
var
  c: TRational;
begin
  with dir do
  if typ = 5 then begin
    fFile.Load(ind,c,8);

    if fisMac then begin
      c.c1:=SwapInt(c.c1);
      c.c2:=SwapInt(c.c2);
    end;

    if c.c2 <> 0 then
      ind:=c.c1 div c.c2
    else
      ind:=0
  end;
end;

function StrKey(Str,Key: PChar; Ind,Len: Integer): PChar;
begin
  Result:=nil; StrCopy(Str,'');
  if Ind >= 0 then
  if (Ind+Len) <= Strlen(Key) then begin
    if Len > 255 then Len:=255;
    Result:=StrLCopy(Str,@Key[Ind],Len)
  end
end;

function StrGeo(Str,Key: PChar; wp: PWords): PChar;
begin
  Result:=nil; StrCopy(Str,'');
  if wp[1] = 34737 then
  Result:=StrKey(Str,Key,wp[3],wp[2])
end;

function GetParam(dat: PDoubles; datn: int;
                  wp: PWords; var v: Double): bool;
begin
  Result:=false;
  if wp[1] = 34736 then
  if wp[2] = 1 then
  if wp[3] >= 0 then
  if wp[3] < datn then begin
    v:=dat[wp[3]]; Result:=true
  end
end;

function GetParams(dat: PDoubles; datn: int;
                   wp: PWords; vp: PDoubles; vn: int): bool;
var
  i: int;
begin
  Result:=false;
  if wp[1] = 34736 then
  if wp[2] = vn then
  if wp[3] >= 0 then
  if wp[3]+vn <= datn then begin
    for i:=0 to vn-1 do
    vp[i]:=dat[wp[3]+i];
    Result:=true
  end
end;

procedure Swap_doubles(fp: PDoubles; n: int);
var
  i: int;
begin
  if fIsMac then
  for i:=0 to n-1 do
  fp[i]:=SwapDouble(fp[i])
end;

procedure Swap_xyz(vp: PDoubles; n: int);
begin
  if fIsMac then
  Swap_doubles(vp,n*3)
end;

const
  ModelTypeProjected   = 1;   // Projection Coordinate System
  ModelTypeGeographic  = 2;   // Geographic latitude-longitude System
  ModelTypeGeocentric  = 3;   // Geocentric (X,Y,Z) Coordinate System

type
  TParamsd = Array[0..255] of Double;

var
  log: TTextwrite;
  dir: TTiffDirList;
  dp: ptiff_dir_arr64;
  dr: tiff_dir64;
  hdr: tiff_hdr;

  pos,i,j,l,v: int;
  dir_top,sz: Int64;

  modelType: int;
  linearUnits,angularUnits: int;
  pcs,iz,iz1,dx: int; kp: double;
  GeogAngularUnitSize: double;

  ip: PIntegers;

  fp: PDoubles; tp: PTiepoints;
  wp,gp: PWords; pc: PChar; s: txyz;
  datN,geoN: int; tr: Real3x3;
  dat: TParamsd; geo: TWords;
  sys1,sys2: tsys; gs: TCmdStr;
begin
  Result:=false;

  fMatrix:=Identify_3x3;
  fPoints.Clear;

  s:=xyz_nil; geoN:=0; datN:=0;
  StrCopy(gs,''); fsys:=sys_nil;

  sys1:=sys_nil;

  StrCopy(fGTCitation,'');
  StrCopy(fGeogCitation,'');
  StrCopy(fCSCitation,'');

  Fillchar(geo,Sizeof(geo),0);
  Fillchar(dat,Sizeof(dat),0);

  log:=nil;
  if log_enabled then begin
    log:=TTextWrite.Create;
    log.NewTemp('geotiff')
  end;

  fFile:=nil;
  try
    if not GetOpenFileIntf(Path,fFile) then begin
      if Assigned(log) then
      log.WriteStr(Format('"%s" not opened.',[xStrNameExt(Path)]));
    end else
    if fFile.GetSize > SizeOf(hdr) then begin

      fFile.Load(0,hdr,8); sz:=fFile.GetSize;

      fIsBig:=false; fIsMac:=false;
      if hdr.cod = $4D4D then begin
        hdr.cod:=$4949; fisMac:=true;
        hdr.ver:=SwapWord(hdr.ver);
        hdr.ind:=SwapInt(hdr.ind)
      end;

      dir_top:=hdr.ind;
      if hdr.ver = $2B then
      if hdr.ind <> 8 then
        hdr.cod:=0
      else begin
        fFile.Load(8,dir_top,8);
        if fIsMac then
        dir_top:=SwapInt64(dir_top);
        fIsBig:=true;
      end;

      if Assigned(log) then
      log.WriteStr(Format('big=%d, dir=%d',[ibool[fIsBig],dir_top]));

      dir:=TTiffDirList.Create(fIsBig,fisMac);
      try
        if hdr.cod = $4949 then
        if dir.Load(fFile,dir_top) then begin

          dp:=dir.First;
          for i:=0 to dir.Count-1 do begin

            dr:=dp[i]; dir_rational(dr);

            if Assigned(log) then
            log.WriteStr(Format('tag=%d, typ=%d, len=%d, ind=%d',
            [dr.tag,dr.typ,dr.len,dr.ind]));

            with dr do
            case tag of
          256:fWidth:=ind;
          257:fHeight:=ind;

          33922: // ModelTiepoint
              if typ = 12 then
              if len >= 6 then
              if len mod 6 = 0 then begin
                fp:=fFile.Seek(ind,len*Sizeof(double));
                if Assigned(fp) then begin
                  fPoints.Insert_range(fp,-1,len div 6);
                  Swap_xyz(fPoints.First,fPoints.Count*2);
                  if Assigned(fSpy) then
                  fSpy.xAdd(dr,fp)
                end
              end;

          33550: // ModelPixelScale
              if typ = 12 then
              if len = 3 then begin
                fp:=fFile.Seek(ind,len*Sizeof(double));
                if Assigned(fp) then begin
                  s:=pxyz(fp)^; Swap_xyz(@s,1);
                  if Assigned(fSpy) then
                  fSpy.xAdd(dr,fp)
                end
              end;

          34735: // GeoKeyDirectory
              if typ in [3,4] then
              if len >= 4 then
              if len div 4 <= GeoSubKeyMax then begin

                gp:=@geo;

                if typ = 3 then begin
                  wp:=fFile.Seek(ind,len*2);
                  if Assigned(wp) then begin
                    for j:=0 to len-1 do begin
                      v:=wp[j]; if fIsMac then
                      v:=SwapWord(v); gp[j]:=v
                    end; geoN:=len div 4;

                    if Assigned(fSpy) then
                    fSpy.xAdd(dr,wp);
                  end
                end else
                if typ = 4 then begin
                  ip:=fFile.Seek(ind,len*4);
                  if Assigned(ip) then begin
                    for j:=0 to len-1 do begin
                      v:=ip[j]; if fIsMac then
                      v:=SwapWord(v); gp[j]:=v
                    end; geoN:=len div 4;

                    if Assigned(fSpy) then
                    fSpy.xAdd(dr,ip)
                  end
                end
              end;

          34736: // GeoDoubleParams
              if typ = 12 then
              if len > 0 then
              if len*8 < Sizeof(dat) then begin
                fp:=fFile.Seek(ind,len*2);
                if Assigned(fp) then begin
                  Move(fp^,dat,len*8); datN:=len;
                  Swap_doubles(fp,datN);
                  if Assigned(fSpy) then
                  fSpy.xAdd(dr,fp)
                end
              end;

          34737: // GeoAsciiParams
              if typ = 2 then
              if len > 0 then begin
                pc:=fFile.Seek(ind,len);
                if Assigned(pc) then begin
                  l:=len; if l >= Sizeof(gs) then
                  l:=Sizeof(gs)-1; StrLCopy(gs,pc,l);

                  if Assigned(fSpy) then
                  fSpy.xAdd(dr,pc)
                end
              end;

            end
          end;

          modelType:=0;
          linearUnits:=0;
          angularUnits:=9102;
          GeogAngularUnitSize:=-1;
          pcs:=0; dx:=0; // projectedCSType

          wp:=@geo;
          if geoN > 1 then
          for i:=1 to geoN do begin

            case wp[0] of
          1024:
              modelType:=wp[3];
          2052:
              linearUnits:=wp[3];
          2054:
              angularUnits:=wp[3];

          2048:
             if wp[3] = 4284 then begin //	GCS_Pulkovo_1942
                sys1.elp:=1;
                sys1.dat:=ru42_Datum2
             end else
             if wp[3] = 4326 then
               sys1.elp:=9;

          2056:
              if wp[3] = 7030 then
                sys1.elp:=9
              else
              if wp[3] = 7024 then
                sys1.elp:=1;

          3072:
              pcs:=wp[3];

          1026:
              StrGeo(fGTCitation,gs,wp);
          2049:
              StrGeo(fGeogCitation,gs,wp);

          2055:
              GetParam(@dat,datn,wp,GeogAngularUnitSize);

          3073:
              StrGeo(fCSCitation,gs,wp);

          3075:
              case wp[3] of
            1:  sys1.prj:=2;
            7:  sys1.prj:=3;
              end;

          3078:
              GetParam(@dat,datn,wp,sys1.b1);
          3079:
              GetParam(@dat,datn,wp,sys1.b2);
          3080:
              GetParam(@dat,datn,wp,sys1.lc);
          3081:
              GetParam(@dat,datn,wp,sys1.b3);
          3082:
              GetParam(@dat,datn,wp,sys1.y0);
          3083:
              GetParam(@dat,datn,wp,sys1.x0);

          3092:
              GetParam(@dat,datn,wp,sys1.rn); // scale

          2062:
              if wp[2] = 7 then
              if GetParams(@dat,datn,wp,@sys1.dat,7) then
              Datum_norm(sys1.dat);
            end;

            wp:=@wp[4]
          end;

          tp:=fPoints.First;

          if modelType = 1 then // Projected

          if pcs = 32767 then begin  // user-defined

            if sys1.prj > 0 then begin

              if sys1.elp <= 1 then
              if Abs(sys1.dat.dX) < Small then
              sys1.dat:=ru42_Datum;

              if angularUnits = 32767 then
              angularUnits:=9102;

              kp:=GeogAngularUnitSize;

              if kp > 0 then begin

                if (Abs(sys1.lc*kp) >= 2*Pi)
                or (Abs(sys1.b1*kp) >= Pi/2)
                or (Abs(sys1.b2*kp) >= Pi/2) then
                kp:=Pi/180;

                sys1.lc:=sys1.lc*kp;
                sys1.b1:=sys1.b1*kp;
                sys1.b2:=sys1.b2*kp;
              end else
              if angularUnits = 9102 then begin
                sys1.lc:=sys1.lc/180*Pi;
                sys1.b1:=sys1.b1/180*Pi;
                sys1.b2:=sys1.b2/180*Pi;
              end;

              if sys1.prj = 2 then
              if xEqual(sys1.rn,1) then
              sys1.prj:=1; sys1.rn:=0;

              if sys1.prj > 2 then
                fsys:=sys1
              else begin

                if sys1.prj in [1,2] then
                if Round(sys1.y0 / 1000000) = 0 then begin

                  iz:=iZone(sys1.lc);
                  if iz > 0 then
                  with tp[0].b do begin
                    x:=x + iz*1000000;
                    if sys1.prj = 2 then
                    x:=x + 30000000;
                  end
                end;

                sys1.x0:=0; sys1.y0:=0;

                sys2:=sys_nil; with tp[0].b do
                sys_projection(sys2,y,x);

                if sys2.prj > 0 then
                if sys1.prj = sys2.prj then
                  fsys:=sys1
                else
                  fsys:=sys2;
              end;

              if fsys.prj > 0 then fsys.pps:=1;
            end

          end else

          if (pcs >= 28404) and (pcs <= 28432) then begin
            fsys.prj:=1; fsys.elp:=1; iz:=pcs - 28400;
            fsys.lc:=(iz*6-3)/180*Pi;
            dx:=iz*1000000; fsys.pps:=1;
            fsys.dat:=ru42_Datum
          end else

          if (pcs >= 32601) and (pcs <= 32660) then begin
            fsys.prj:=2; fsys.elp:=9; iz:=pcs - 32600;
            fsys.lc:=((iz-30)*6-3)/180*Pi;
            dx:=iz*1000000; fsys.pps:=1;
          end else

          if ((pcs >= 32701) and (pcs  <= 32760)) then begin
            fsys.elp:=9;
            fsys.prj:=2; iz:=pcs - 32700;
            fsys.lc:=((iz-30)*6-3)/180*Pi;
            fsys.x0:=10000000;
            fsys.y0:=500000;
            fsys.pps:=1;
          end else

          if (pcs >= 5361) and (pcs  <= 5362) then begin
            fsys.prj:=2; fsys.elp:=17;
            iz:=5361 - pcs + 19;
            fsys.lc:=((iz-30)*6-3)/180*Pi;
            fsys.x0:=10000000;
            fsys.y0:=500000;
            fsys.pps:=1;
          end else

          if pcs = 123 then begin
            fsys:=sys7(1,30,6, 0,0,0);  //  Cuba_Norte
            fsys.dat:=cu_Datum
          end;

          if fPoints.Count > 0 then begin

            if modelType = 0 then
            if Abs(tp[0].b.x) < 360 then
            if Abs(tp[0].b.y) < 90 then begin
              modelType:=2; angularUnits:=9102
            end;

            case modelType of
          1: // Projected
              if dx > 0 then begin
                if tp[0].b.x > 1000000 then begin
                  fsys:=sys_nil; with tp[0].b do
                  sys_projection(fsys,y,x);
                  if fsys.prj > 0 then fsys.pps:=1
                end else

                if Abs(tp[0].b.x) < 1000000 then
                for i:=0 to fPoints.Count-1 do
                with tp[i].b do x:=x+dx
              end else
              if fsys.pps = 0 then
              if fsys.elp = 0 then
              if fsys.prj = 0 then

              if StrPos(fGTCitation,'WGS') <> nil then
              if StrPos(fGTCitation,'84') <> nil then
              if StrPos(fGTCitation,'World') <> nil then
              if StrPos(fGTCitation,'Mercator') <> nil then
              fsys:=sys7(1,3,9,0,0,0);

          2:  // Geographic
              begin
                fsys:=sys7(1,prj_deg,sys1.elp,0,0,0);
                if fsys.elp = 0 then fsys.elp:=9;

                kp:=1;

                case angularUnits of
              9101: kp:=180/Pi;
                end;

                if kp <> 1 then begin
                  s.x:=s.x*kp; s.y:=s.y*kp;
                  for i:=0 to fPoints.Count-1 do
                  with tp[i].b do begin
                    x:=x*kp; y:=y*kp;
                  end
                end;

                fsys.b1:=tp[0].b.y/180*Pi
              end
            end;

            if fPoints.Count >= 3 then
              Result:=true
            else
            if s.x > 0 then
            if s.y > 0 then

            if fPoints.Count = 1 then begin

              with tp[0].a do
              Init_3x3(tr,-x,y,1,-1);

              xy_Scale_3x3(tr,s.x,s.y);

              with tp[0].b do t_Move_3x3(tr,x,y);

              fPoints.Clear; fMatrix:=tr;

              Result:=true
            end
          end
        end;
      finally
        dir.Free;
      end
    end;

  finally
    log.Free
  end;

  if not Result then
  if Assigned(fSpy) then fSpy.Clear;
end;

function tiff_get_geo(Geo: TTiffData; Path: PChar): Integer;
var
  tiff: TGeoTiff;
begin
  Geo.Clear;

  tiff:=TGeoTiff.Create;
  try
    tiff.Spy:=Geo; tiff.Open(Path)
  finally
    tiff.Free
  end;

  Result:=Geo.Count
end;

end.
