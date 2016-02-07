unit dmw_vgm; interface

uses
  Classes,Graphics,Math,
  otypes,vbanks,xline,xdib;

const
  vgm_sign_size = 8192;

type
  TSignHeader = array[1..6] of byte; (* counter,xor,cx,cy,dx,dy *)
  TSignBuf = array[0..vgm_Sign_Size-1] of byte;

  TVgmRead = class(TIndRead)

    function Open(Path: PChar): Boolean; override;
    procedure Close; override;

    function Item_Seek(Ind: Integer): Integer; override;

    function Get_Radius: Integer;
    function Get_Lift: Float;

    function Get_res(Ind: Integer): Double;

  private
    fHeights: PFloats;

    function Get_Height(Ind: Integer): float;
    function Get_Width(Ind: Integer): float;
    function Get_Header(Ind: Integer): Integer;

  public
    property Heights[I: Integer]: Float read Get_Height;
    property Widths[I: Integer]: Float read Get_Width;
    property Header[I: Integer]: Integer read Get_Header;
  end;

  TVgmBank = class(TIndBank)
    t_t: Real3x3;
    pp_k: double;

    hdr: TSignHeader;

    is_Skip: Boolean;
    is_Game: Boolean;

    v_lt,v_rb: TPoint;
    v_Rect: LOrient;

    constructor Create(cash,max: Integer);
    destructor Destroy; override;

    function Create_bank(max: Integer): TIndBank; override;

    function Ind_Copy(i: Integer; dst: TIndBank): Integer; override;
    function Ind_Paste(i: Integer; src: TIndBank): Integer; override;
    procedure Ind_More(i: Integer; dst: TIndBank); override;

    function dxf_verify(i: Integer): Boolean;

    function Vgm_Read(i: Integer): longint;
    function Vgm_Load(i: Integer): longint;
    function Vgm_mm_k(h: Double): Double;

    function Get_sign(i: int; var buf; maxSize: int): int;

    function Get_res(i: Integer): Double;
    function Vgm_scale(i: Integer; w,h: Double): Double;

    function Letter_Width(ch: Integer; k: Double): Double;
    function Char_Width(ch: Integer; k: Double): Double;

    function Vgm_Pack(i: Integer; cls: Double;
                      dst: PBytes; size: Integer): Integer;

    function Vgm_Min_Max(out lt,rb: TPoint): Integer;

    function Vgm_Draw(DC: XCanvas; const p: TPoint;
                      bc,cl,i: Integer): Boolean;

    function Vgm_Curve(DC: XCanvas; v: PLPoly; cl,i: Integer): Boolean;
    procedure Vgm_Skip(DC: XCanvas; const p: TPoint; cl,i: Integer); virtual;

    function Out_Letter(DC: XCanvas; x,y, cl,i: Integer): Boolean;

    function Vgm_Line(DC: XCanvas;
                      lp: PLPoly; lp_N, cl,sign,r: Integer;
                      mmk,vx,vy,vs: Double): Boolean;

    function Vgm_Sign(DC: XCanvas; v: PLLine;
                      mmk: double; bc,cl,i,r: Integer): Boolean;

    function Vgm_disp(i: Integer; mmk: double;
                      h1,h2: Float): Double;

    procedure Vgm_Mark(DC: XCanvas; v: PLLine; cl,i: Integer);

    function Vgm_Wide(DC: XCanvas; const p1,p2: TPoint;
                      u,k,g: float; s: PWideChar): Integer;

    function Out_vgm(DC: XCanvas; v: PLLine;
                     ind,cl: Integer; k: Double): Boolean;

    procedure Plot_vgm(DC: TCanvas; const a,b: TPoint;
                       ind,cl: Integer; k: Double);

    procedure Out_Sign(DC: XCanvas; const a,b: TPoint; i: int);
    procedure Rect_Sign(DC: TCanvas; const R: TRect; i: int);
    procedure Zoom_Sign(DC: TCanvas; const R: TRect; i,cl: int);

    procedure Ico_Sign(DC: XCanvas; i, x,y,w,h: Integer);
    function Ico_Rect(i: Integer; R: PRect): Boolean;

    function Vgm_Height(i: Integer): float;
    procedure Dump_Height(i: Integer; h: float);
    procedure Delete_Height(i: Integer);

    function Y_Move(x,y,k: Double): tgauss;
    function IY_Move(x,y,k: Double): TPoint;
    function Y_Plot(x,y,h: Double): tgauss;
    function X_Plot(x,y: Double; w: Integer): tgauss;

    procedure Vgm_Rect(const a: TGauss; w: Integer; k: Double);
    procedure Set_Vgm_Rect(G: PGPoly);
    function Vgm_Size: tgauss;

    procedure Virt_Rect(Sender: TObject); virtual;

    procedure Poly_Line(DC: XCanvas; lp: PPoly; n,s,cl: Integer; k: double);

    procedure Vgm_Start(DC: XCanvas); virtual;
    procedure Vgm_Poly(DC: XCanvas; lp: PLPoly; N, Cl,Tag: Integer); virtual;
    procedure Vgm_Close(DC: XCanvas); virtual;

  private
    fbufp: PBytes;
    fbuf_load: Integer;
    ipoly: PLPoly;
    xpoly: PGPoly;

    ipoly_n: Integer;
    ipoly_cl: Integer;

    g_Rect: GOrient;

    fIsColor: Boolean;

    function Vgm_Display(DC: XCanvas; const p: TPoint): Boolean;

  public
    property bufp: PBytes read fbufp;
    property buf_load: Integer read fbuf_load;

    property IsColor: Boolean write fIsColor;
  end;

  TVgmSign = class(TVgmBank)
    procedure Vgm_Poly(DC: XCanvas; lp: PLPoly; N, Cl,Tag: Integer); override;
  private
    fdc_lt,fdc_rb: TPoint;
    fIs_Bound: Boolean;
    fIs_Begin: Boolean;

    function Get_Width: Integer;
    function Get_Height: Integer;

  public
    property Is_Bound: Boolean write fIs_Bound;
    property Is_Begin: Boolean write fIs_Begin;

    property dc_lt: TPoint read fdc_lt;
    property dc_rb: TPoint read fdc_rb;

    property Width: Integer read Get_Width;
    property Height: Integer read Get_Height;
  end;

  TVgmPixel = record x,y: byte end;

function IsVgtSeparator(ch: Char): Boolean;
function IsSimpleText(s: PChar): Boolean;

implementation

uses
  SysUtils,convert,
  xpoly,xclip,xcurve,
  xdibcv;

function IsVgtSeparator(ch: Char): Boolean;
begin
  Result:=ch in [' ','.',',','~','^','@','_','-']
end;

function IsSimpleText(s: PChar): Boolean;
begin
  Result:=true;
  if Strlen(s) > 0 then
  if (StrScan(s,'~') <> nil)
  or (StrScan(s,'^') <> nil)
  or (StrScan(s,'@') <> nil)
  or (StrScan(s,'_') <> nil) then
  Result:=false
end;

type
  TVgm_IPoly = array[0..255] of TPoint;
  TVgm_GPoly = array[0..255] of tgauss;

  TClipDraw = class(TClipPoly)

    constructor Create(ADC: XCanvas;
                       AVgm: TVgmBank;
                       ACl,ATag: Integer);

    procedure lp_Dump(lp: PLLine); override;
  private
    DC: XCanvas;
    Vgm: TVgmBank;
    Cl,Tag: Integer;
  end;

constructor TClipDraw.Create(ADC: XCanvas;
                             AVgm: TVgmBank;
                             ACl,ATag: Integer);
begin
  inherited Create;
  DC:=ADC; Vgm:=AVgm;
  Cl:=ACl; Tag:=ATag
end;

procedure TClipDraw.lp_Dump(lp: PLLine);
begin
  Vgm.Vgm_Poly(DC,@lp.Pol,lp.N,Cl,Tag)
end;

function TVgmRead.Open(Path: PChar): Boolean;
var
  bx: Integer;
begin
  Result:=inherited Open(Path);
  if Result then begin
    bx:=Get_long(12); if bx >= 16 then
    if bx + Count * 4 <= Size then
    fHeights:=@Buf[bx-4]
  end
end;

procedure TVgmRead.Close;
begin
  fHeights:=nil;
  inherited Close
end;

function TVgmRead.Item_Seek(Ind: Integer): Integer;
begin
  Result:=inherited Item_Seek(Ind);
  if Result < 6 then begin
    fItemBuf:=nil; fItemLen:=0;
  end;

  Result:=fItemLen
end;

function TVgmRead.Get_Height(Ind: Integer): Float;
begin
  Result:=0;
  if Assigned(fHeights) then
  if (Ind > 0) and (Ind <= Count) then
  Result:=fHeights[Ind]
end;

function TVgmRead.Get_Width(Ind: Integer): float;
var
  h: Float; hdr: TSignHeader;
begin
  Result:=0;
  if Count > 0 then begin
    h:=Heights[Ind];
    if Item_Seek(Ind) > 0 then begin
      Move(fItemBuf^,hdr,Sizeof(hdr));
      if hdr[6] > 0 then Result:=h/hdr[6]*hdr[5]
    end
  end
end;

function TVgmRead.Get_Header(Ind: Integer): Integer;
begin
  Result:=ItemBuf[Ind-1]
end;

function TVgmRead.Get_Radius: Integer;
begin
  Result:=Max(Header[5],Header[6])
end;

function TVgmRead.Get_Lift: Float;
var
  cy,h: Integer;
begin
  Result:=0; cy:=Header[4]; h:=Header[6];
  if h > 0 then Result:=(h-cy)/h
end;

function TVgmRead.Get_res(Ind: Integer): Double;
begin
  Result:=0;

  if Item_Seek(Ind) > 0 then
  if Header[6] > 0 then

  Result:=Heights[Ind]/Header[6]
end;

constructor TVgmBank.Create(cash,max: Integer);
var
  ind,len: Integer;
begin
  inherited Create(cash,max); pp_k:=1;
  t_t[1,1]:=1; t_t[1,2]:=0; t_t[2,1]:=0; t_t[2,2]:=1;

  len:=vgm_sign_size+
       SizeOf(TVgm_IPoly)+
       SizeOf(TVgm_GPoly);

  fbufp:=xAllocPtr(len);

  if Assigned(bufp) then begin
    ind:=vgm_sign_size; ipoly:=@bufp[ind];
    Inc(ind,SizeOf(TVgm_IPoly));
    xpoly:=@bufp[ind]
  end;

  is_Skip:=false
end;

destructor TVgmBank.Destroy;
begin
  xFreePtr(bufp);
  inherited
end;

function TVgmBank.Create_bank(max: Integer): TIndBank;
begin
  Result:=TVgmBank.Create(0,max)
end;

function TVgmBank.Ind_Copy(i: Integer; dst: TIndBank): Integer;
begin
  Result:=inherited Ind_Copy(i,dst); if Result > 0 then
  (dst as TVgmBank).Dump_Height(Result,Vgm_Height(i))
end;

function TVgmBank.Ind_Paste(i: Integer; src: TIndBank): Integer;
begin
  Result:=inherited Ind_Paste(i,src); if Result > 0 then
  Dump_Height(Result,(src as TVgmBank).Vgm_Height(i))
end;

procedure TVgmBank.Ind_More(i: Integer; dst: TIndBank);
begin
  (dst as TVgmBank).Dump_Height(i,Vgm_Height(i))
end;

function TVgmBank.dxf_verify(i: Integer): Boolean;
var
  len: Integer;
begin
  Result:=false;
  if Vgm_Read(i) > 0 then begin
    len:=vm_BufLen(Loc.Ind);
    Result:=len >= SizeOf(hdr)
  end
end;

function TVgmBank.Vgm_Read(i: Integer): longint;
begin
  if Ind_Load(i) > 0 then
    vm_Load(Loc.Ind+4,hdr,SizeOf(hdr))
  else
    FillChar(hdr,SizeOf(hdr),0);

  Result:=Loc.Ind
end;

function TVgmBank.Vgm_Load(i: Integer): longint;
var
  cx: Integer;
begin
  Result:=0; fbuf_load:=0;

  if vm_Active then begin
    cx:=Get_buf(i,bufp^,vgm_sign_size);
    if cx > SizeOf(hdr) then begin
      Move(bufp^,hdr,SizeOf(hdr));
      fbuf_load:=cx; Result:=cx
    end
  end
end;

function TVgmBank.Get_sign(i: int; var buf; maxSize: int): int;
var
  bx,cx,_bx,_cx,n: int; bp: PBytes;
  top: TVgmPixel; upd: bool;
begin
  cx:=Get_buf(i,buf,maxSize); _cx:=cx;

  bp:=@buf;
  if cx < 6 then begin
    bp[0]:=0; bp[1]:=0;
    bp[2]:=63; bp[3]:=63;
    bp[4]:=127; bp[5]:=127;
    cx:=6
  end
  else begin
    bx:=6; for i:=1 to bp[0] do begin

      _bx:=bx;
      if bx+2 > cx then begin
        bp[0]:=i-1; cx:=_bx; Break
      end;

      Move(bp[bx],top,2); Inc(bx,2);
      n:=top.x and $7F;

      Inc(bx,(n+1)*2);
      if bx > cx then begin
        bp[0]:=i-1; cx:=_bx; Break
      end
    end
  end;

  if cx <> _cx then begin
    upd:=vm_Edit;
    if upd or vm_Update then begin
      Loc.Ind:=vm_UpdateBuf(Loc.Ind,buf,cx);
      Ind_Dump; if not upd then vm_ReadOnly
    end
  end;

  Result:=cx
end;

function TVgmBank.Vgm_mm_k(h: Double): Double;
begin
  Result:=0; if hdr[6] > 0 then
  Result:=h / hdr[6]
end;

function TVgmBank.Get_res(i: Integer): Double;
begin
  Result:=0;

  if Vgm_Read(i) > 0 then
  if hdr[6] > 0 then

  Result:=Vgm_Height(i)/hdr[6]
end;

function TVgmBank.Vgm_scale(i: Integer; w,h: Double): Double;
begin
  Result:=0; Begin_3x3(t_t, 0,0);

  if Vgm_Read(i) > 0 then

  if hdr[5] > 0 then
  if hdr[6] > 0 then begin

    Result:=h / hdr[6]; if w > 0 then
    Result:=Min(Result,w / hdr[5]);

    t_t[1,1]:=Result;
    t_t[2,2]:=Result;
  end
end;

function TVgmBank.Letter_Width(ch: Integer; k: Double): Double;
begin
  Result:=0;
  if Vgm_Read(ch+1) > 0 then
  Result:=(hdr[5]-hdr[3])*k
end;

function TVgmBank.Char_Width(ch: Integer; k: Double): Double;
begin
  Result:=0;
  if Vgm_Read(ch+1) > 0 then
  Result:=hdr[5] * k
end;

function TVgmBank.Vgm_Display(DC: XCanvas; const p: TPoint): Boolean;

procedure Next_v_lt_rb(x,y: Integer; c: TPoint);
var
  p: TPoint;
begin
  Dec(x,hdr[3]); Dec(y,hdr[4]);

  p.x:=c.x+Round(x*t_t[1,1]-y*t_t[1,2]);
  p.y:=c.y+Round(-x*t_t[2,1]+y*t_t[2,2]);

  v_lt.x:=Min(v_lt.x,p.x);
  v_lt.y:=Min(v_lt.y,p.y);
  v_rb.x:=Max(v_rb.x,p.x);
  v_rb.y:=Max(v_rb.y,p.y);
end;

begin
  Next_v_lt_rb(0,0,p);
  Next_v_lt_rb(hdr[5],0,p);
  Next_v_lt_rb(hdr[5],hdr[6],p);
  Next_v_lt_rb(0,hdr[6],p);

  Result:=false; with DC.ClipRect do

  if (Left <= v_rb.x) and (v_lt.x <= Right) then
  if (Top <= v_rb.y) and (v_lt.y <= Bottom) then

  Result:=true;
end;

function TVgmBank.Vgm_Pack(i: Integer; cls: Double;
                           dst: PBytes; size: Integer): Integer;
var
  len,ind,i1,i2,old: Integer;
  lp: PLLine; top: TVgmPixel;
begin
  Result:=0;
  lp:=Alloc_LLine(256);

  if Assigned(lp) then
  if size >= vgm_sign_size then begin

    len:=Get_buf(i,bufp^,vgm_sign_size);

    if len >= SizeOf(hdr) then begin

      Move(bufp^,hdr,SizeOf(hdr));
      ind:=SizeOf(hdr); Dec(len);

      Move(hdr,dst^,SizeOf(hdr));
      Result:=SizeOf(hdr); dst[0]:=0;

      for i1:=1 to hdr[1] do
      if ind < len then begin
        top.x:=bufp[ind]; Inc(ind);
        top.y:=bufp[ind]; Inc(ind);

        lp.N:=top.x and $7F;

        for i2:=0 to lp.N do
        if ind < len then begin
          lp.Pol[i2].x:=bufp[ind]; Inc(ind);
          lp.Pol[i2].y:=bufp[ind]; Inc(ind)
        end
        else lp.N:=0;

        if lp.N > 0 then
        if cls_Poly(lp,cls) > 0 then begin

          top.x:=(top.x and $80)+lp.N;

          dst[Result]:=top.x; Inc(Result);
          dst[Result]:=top.y; Inc(Result);

          for i2:=0 to lp.N do begin
            dst[Result]:=lp.Pol[i2].x; Inc(Result);
            dst[Result]:=lp.Pol[i2].y; Inc(Result);
          end;

          Inc(dst[0])
        end
      end;

      if dst[0] = 0 then Result:=0 else
      if Result = len then Result:=0
    end
  end;

  xFreePtr(lp)
end;

function TVgmBank.Vgm_Min_Max(out lt,rb: TPoint): Integer;
var
  si: PBytes; i1,i2,n,cx: Integer;
  top: TVgmPixel; c,p: TPoint;
begin
  Result:=0;

  cx:=vm_BufLen(Loc.Ind);

  if cx >= SizeOf(hdr) then
  if cx <= vgm_sign_size then begin

    vm_Load(Loc.Ind+4,bufp^,cx);

    si:=@bufp[SizeOf(hdr)];
    cx:=(cx - Sizeof(hdr)) div 2;

    c.X:=hdr[3]; c.Y:=hdr[4];

    for i1:=1 to hdr[1] do
    if cx > 0 then begin
      top.x:=si[0]; si:=@si[1];
      top.y:=si[0]; si:=@si[1];

      n:=top.x and $7F;

      if n+1 <= cx then begin
        for i2:=0 to n do begin
          p.X:=si[0]; si:=@si[1];
          p.Y:=si[0]; si:=@si[1];

          Dec(p.X,c.X); Dec(p.Y,c.Y);

          if Result = 0 then begin
            lt:=p; rb:=p
          end
          else begin
            lt.X:=Min(lt.X,p.X); lt.Y:=Min(lt.Y,p.Y);
            rb.X:=Max(rb.X,p.X); rb.Y:=Max(rb.Y,p.Y);
          end;

          Inc(Result)
        end;
      end; Dec(cx,n+1+1)
    end
  end
end;

function TVgmBank.Vgm_Draw(DC: XCanvas; const p: TPoint;
                           bc,cl,i: Integer): Boolean;
var
  si: PBytes; i1,i2,_cl,n,cx: Integer;
  top: TVgmPixel; c,q: TPoint;
begin
  Result:=false;

  if Vgm_Read(i) > 0 then

  if (DC <> nil) and not Vgm_Display(DC,p) then

    Result:=true

  else begin
    cx:=vm_BufLen(Loc.Ind);

    if cx >= SizeOf(hdr) then
    if cx <= vgm_sign_size then begin

      vgm_Start(DC); Result:=true;
      vm_Load(Loc.Ind+4,bufp^,cx);

      si:=@bufp[SizeOf(hdr)];
      cx:=(cx - Sizeof(hdr)) div 2;

      c.x:=hdr[3]; c.y:=hdr[4];

      for i1:=1 to hdr[1] do
      if cx > 0 then begin
        top.x:=si[0]; si:=@si[1];
        top.y:=si[0]; si:=@si[1];

        n:=top.x and $7F;

        if is_game then begin
          _cl:=top.y; if _cl = 0 then _cl:=cl
        end
        else begin
          _cl:=cl; if not fIsColor then
          if cl = 0 then begin _cl:=top.y;
            if _cl = 0 then _cl:=bc
          end;
        end;

        if n+1 <= cx then begin
          for i2:=0 to n do begin
            q.x:=si[0]; si:=@si[1];
            q.y:=si[0]; si:=@si[1];

            Dec(q.x,c.x); Dec(q.y,c.y);

            with ipoly[i2] do begin
              x:=p.x + Round(q.x*t_t[1,1]-q.y*t_t[1,2]);
              y:=p.y + Round(-q.x*t_t[2,1]+q.y*t_t[2,2])
            end
          end;

          if n > 0 then
          Vgm_Poly(DC,ipoly,n,_cl,top.x and $80)

        end; Dec(cx,n+1+1)
      end;

      Vgm_Close(DC); Result:=true
    end
  end;

  if not Result then begin
    if Vgm_Read(ord('A')+1) = 0 then begin
      FillChar(hdr,SizeOf(hdr),0);
      hdr[3]:=8; hdr[4]:=8;
      hdr[5]:=16; hdr[6]:=16
    end;

    vgm_Start(DC);
    Vgm_Skip(DC,p,cl,i);
    Vgm_Close(DC);
  end
end;

function TVgmBank.Vgm_Curve(DC: XCanvas; v: PLPoly; cl,i: Integer): Boolean;
var
  si: PBytes; i1,i2, _cl,n,cx: Integer;
  top: TVgmPixel; Curve: TSignCurve; lp: PLLine;
  Clip: TClipDraw; p, dc_lt,dc_rb, lt,rb: TPoint;
  vv: LOrient;
begin
  Result:=false;

  Curve:=TSignCurve.Create;
  try
    if Vgm_Read(i) > 0 then
    if hdr[5] > 0 then begin

      cx:=vm_BufLen(Loc.Ind);

      vv[0]:=v[0]; vv[1]:=v[1]; vv[2]:=v[2];
      Mirror_Point(v[1],v[2],vv[3]);

      if cx > SizeOf(hdr) then
      if cx <= vgm_sign_size then
      if Curve.lBezier(@vv) > 0 then begin

        Curve.kx:=1/hdr[5];
        Curve.ky:=Curve.Length/hdr[5];
        if Vgm_Height(i) < 0 then
        Curve.ky:=t_t[2,2];

        vm_Load(Loc.Ind+4,bufp^,cx);

        si:=@bufp[SizeOf(hdr)];
        cx:=(cx - Sizeof(hdr)) div 2;
        Dec(cx,hdr[1]);

        for i1:=1 to hdr[1] do

        if cx > 0 then begin
          top.x:=si[0]; si:=@si[1];
          top.y:=si[0]; si:=@si[1];

          n:=top.x and $7F; _cl:=cl;
          if cl = 0 then _cl:=top.y;

          if n+1 <= cx then begin
            for i2:=0 to n do begin
              p.x:=si[0]; si:=@si[1];
              p.y:=si[0]; si:=@si[1];

              Dec(p.y,hdr[4]); ipoly[i2]:=p;
            end;

            if n > 0 then begin
              top.x:=top.x and $80;

              lp:=Curve.Plot_lp(ipoly,n);

              if lp <> nil then begin
                lt:=Curve.lt; rb:=Curve.rb;

                dc_lt.x:=-16; dc_rb.x:=DC.Width+16;
                dc_lt.y:=-16; dc_rb.y:=DC.Height+16;

                if (lt.x < dc_lt.x) or (rb.x > dc_rb.x)
                or (lt.y < dc_lt.y) or (rb.y > dc_rb.y) then
                begin

                  Clip:=TClipDraw.Create(DC,Self,_cl,top.x);
                  try
                    Clip.Draw_LLine(lp,dc_lt,dc_rb,top.x <> 0);
                  finally
                    Clip.Free;
                  end

                end else

                Vgm_Poly(DC,@lp.Pol,lp.N,_cl,top.x)
              end
            end

          end; Dec(cx,n+1)
        end
      end

    end else Vgm_Read(ord('A')+1);

  finally
    Curve.Free
  end
end;

procedure TVgmBank.Vgm_Skip(DC: XCanvas; const p: TPoint; cl,i: Integer);
var
  R: lOrient; j: Integer; x_fill: Boolean;
  dc_lt,dc_rb, lt,rb, q: TPoint; clip: tClipDraw;
begin
  if is_Skip then begin
    FillChar(R,SizeOf(R),0);
    R[1].x:=hdr[5]-1; R[2].x:=R[1].x;
    R[2].y:=hdr[6]-1; R[3].y:=R[2].y;

    for j:=0 to 4 do with ipoly[j] do begin
      q.x:=R[j].x-hdr[3]; q.y:=R[j].y-hdr[4];
      x:=p.x+Round(q.x*t_t[1,1]-q.y*t_t[1,2]);
      y:=p.y+Round(-q.x*t_t[2,1]+q.y*t_t[2,2])
    end;

    Max_Poly_Bound(ipoly,4, lt,rb);

    dc_lt.x:=-16; dc_rb.x:=DC.Width+16;
    dc_lt.y:=-16; dc_rb.y:=DC.Height+16;

    if (lt.x < dc_lt.x) or (rb.x > dc_rb.x) or
       (lt.y < dc_lt.y) or (rb.y > dc_rb.y) then
    begin
      clip:=tClipDraw.Create(DC,Self,cl,0);
      try
        clip.Draw_LPoly(ipoly,4, dc_lt,dc_rb,false);
      finally
        clip.Free
      end
    end else

    Vgm_Poly(DC,ipoly,4,cl,0);
  end
end;

function TVgmBank.Out_Letter(DC: XCanvas; x,y, cl,i: Integer): Boolean;
var
  p: TPoint;
begin
  Result:=false;

  if Vgm_Read(i) > 0 then begin
    p.x:=x + Round(hdr[3]*t_t[1,1]);
    p.y:=y - Round((hdr[6]-hdr[4])*t_t[2,2]);

    Result:=Vgm_Draw(DC, p, 0,cl,i)
  end
end;

function TVgmBank.Vgm_Line(DC: XCanvas;
                           lp: PLPoly; lp_N, cl,sign,r: Integer;
                           mmk,vx,vy,vs: Double): Boolean;
var
  i,dx,dy: Integer;
  dr,rib,w,h,k,nx,ny: Double;
  a,b,p: TPoint; tr: Real3x3;
begin
  Result:=false;

  if lp_N > 0 then
  if vm_Active then
  if Vgm_Read(sign) > 0 then begin

    k:=1;
    if hdr[6] > 0 then begin
      h:=Abs(Vgm_Height(sign));
      if Abs(h) > 0 then k:=Abs(h)*mmk/hdr[6];
    end;

    if r < 2 then r:=2;
    w:=hdr[5]*k; h:=hdr[6]*k;
    if (w >= r) or (h >= r) then begin

      tr[1,1]:=k; tr[1,2]:=0; tr[1,3]:=0;
      tr[2,1]:=0; tr[2,2]:=k; tr[2,3]:=0;
      tr[3,1]:=0; tr[3,2]:=0; tr[3,3]:=1;

      b:=lp[0]; dr:=vx; vs:=vs + w;

      for i:=1 to lp_N do begin
        a:=b; b:=lp[i];
        dx:=b.X - a.X; dy:=b.Y - a.Y;
        rib:=Hypot(dx,dy);

        if rib >= 0.1 then begin

          nx:=+dy/rib*vy;
          ny:=-dx/rib*vy;

          while dr <= rib do begin

            k:=dr / rib;
            p.X:=a.X + Round(k*dx+nx);
            p.Y:=a.Y + Round(k*dy+ny);

            t_t:=tr;
            xy_Rotate_3x3(t_t,dx,dy);
            Vgm_Draw(DC, p, 0,cl,sign);

            if vs < 1 then begin
              dr:=-1; Break
            end;

            dr:=dr + vs
          end;

          if dr < 0 then Break;
        end;

        dr:=dr - rib
      end;

      Result:=true
    end
  end
end;

function TVgmBank.Vgm_disp(i: Integer; mmk: double;
                           h1,h2: Float): Double;
var
  h: Float;
begin
  Result:=mmk; i:=i and $7FFF;

  if Vgm_Read(i) > 0 then begin
    h:=Vgm_Height(i);

    if Abs(h) > 0 then
    if hdr[6] > 0 then begin

      h:=Abs(h)*mmk;

      if h < h1 then
        Result:=mmk / h * h1
      else
      if h1 < h2 then
      if h > h2 then
        Result:=mmk / h * h2
    end;
  end
end;

function TVgmBank.Vgm_Sign(DC: XCanvas; v: PLLine;
                           mmk: double; bc,cl,i,r: Integer): Boolean;
var
  a,b: TPoint; p: tgauss; h,kx,ky: double;
begin
  Result:=false; i:=i and $7FFF;

  if v.N >= 0 then
  if Vgm_Read(i) > 0 then begin
    h:=Vgm_Height(i); kx:=mmk; ky:=mmk;

    if Abs(h) > 0 then
    if hdr[6] > 0 then begin
      ky:=Abs(h)*mmk/hdr[6]; kx:=ky
    end;

    a:=v.Pol[0]; b:=a;
    if v.N = 1 then b:=v.Pol[1] else
    if v.N = 3 then b:=v.Pol[2];
    Dec(b.X,a.X); Dec(b.Y,a.Y);

    if h <= 0 then begin

      kx:=pp_k; if v.N > 0 then
      if hdr[5] > hdr[3] then
      kx:=Hypot(b.X,b.Y)/(hdr[5] - hdr[3]);

      if h >= 0 then ky:=kx
    end;

    if r < 2 then r:=2;
    if (hdr[5]*kx >= r) or (hdr[6]*ky >= r) then begin

      t_t[1,1]:=kx; t_t[1,2]:=0;  t_t[1,3]:=0;
      t_t[2,1]:=0;  t_t[2,2]:=ky; t_t[2,3]:=0;
      t_t[3,1]:=0;  t_t[3,2]:=0;  t_t[3,3]:=1;

      if v.N = 2 then
        Vgm_Curve(DC, @v.Pol, cl,i)
      else begin
        xy_Rotate_3x3(t_t,b.X,b.Y);
        Vgm_Draw(DC, a, bc,cl,i)
      end;

      p:=X_Plot(a.X,a.Y,-hdr[3]);
      p:=Y_Plot(p.x,p.y,hdr[6]-hdr[4]);
      Vgm_Rect(p,hdr[5],1);

      Result:=true
    end
  end
end;

procedure TVgmBank.Vgm_Mark(DC: XCanvas; v: PLLine; cl,i: Integer);
var
  pix: tVgmPixel; p: LVector;
begin
  if Vgm_Read(i and $7FFF) > 0 then
  if vm_BufLen(Loc.Ind) > SizeOf(hdr)+2 then begin

    vm_Load(Loc.Ind+4+SizeOf(hdr),pix,2);
    if cl = 0 then cl:=pix.y;

    p[0]:=v.Pol[0]; p[1]:=p[0];
    if v.N = 1 then p[1]:=v.Pol[1] else
    if v.N = 3 then p[1]:=v.Pol[2];

    if Points_Equal(p[0],p[1]) then
      DC.SetPixel(p[0].x,p[0].y,cl)
    else begin
      DC.xPen(1,cl);
      DC.PolyLine(@p,1)
    end
  end
end;

function TVgmBank.Vgm_Wide(DC: XCanvas; const p1,p2: TPoint;
                           u,k,g: float; s: PWideChar): Integer;
var
  i,dx,w: Integer; dn_k,up_k,nx,ny,r: Double;
  p,q,org: tgauss; ch: WideChar; a,b: TPoint;
begin
  Result:=0; if vm_Active then begin

    p.x:=p1.x; p.y:=p1.y;
    q.x:=p2.x; q.y:=p2.y;

    if u > 0 then begin
      if (p1.x = p2.x) and (p1.y = p2.y) then begin
        p.y:=p.y + u; q.y:=p.y
      end
      else begin
        nx:=p.y - q.y; ny:=q.x - p.x;
        u:=u / Hypot(nx,ny);

        nx:=nx * u; ny:=ny * u;

        p.x:=p.x + nx; q.x:=q.x + nx;
        p.y:=p.y + ny; q.y:=q.y + ny;
      end
    end;

    t_t[1,1]:=k; t_t[1,2]:=Sin(g)/Cos(g)*k; t_t[1,3]:=0;
    t_t[2,1]:=0; t_t[2,2]:=k;               t_t[2,3]:=0;
    t_t[3,1]:=0; t_t[3,2]:=0;               t_t[3,3]:=1;

    xy_Rotate_3x3(t_t,q.x-p.x,q.y-p.y); q:=p;

    org:=p;

    dx:=0; w:=0; dn_k:=0; up_k:=0; k:=0;

    i:=0; while i < 256 do begin

      ch:=s[i]; Inc(i);
      if ch = #0 then Break;

      if ch = '~' then begin k:=k-4/3;
        p:=y_move(p.x,p.y,4/3); Dec(w,dx); dx:=0
      end else
      if ch = '^' then begin
        p:=y_move(q.x,q.y,-2/3);
        k:=k+2/3; dx:=0
      end else
      if ch = '@' then begin
        u:=AtoI(s[i],2)/100; Inc(i,2);
        p:=y_move(q.x,q.y,-u); k:=k+u; dx:=0
      end else
      if ch = '_' then begin
        if DC <> nil then begin
          a:=iy_move(p.x,p.y,0.1);
          b:=iy_move(q.x,q.y,0.1);
          DC.Line(a.x,a.y,b.x,b.y)
        end
      end
      else begin
        if DC <> nil then begin
          a:=Point(Round(q.x),Round(q.y));
          Vgm_Draw(DC,a,0,0,ord(ch)+1);
        end
        else Vgm_Read(ord(ch)+1);

        Dec(hdr[5],hdr[3]); Inc(dx,hdr[5]);
        Inc(w,hdr[5]); Result:=Max(Result,w)
      end;

      if k < dn_k then dn_k:=k;
      if k > up_k then up_k:=k;

      q.x:=p.x+t_t[1,1]*dx;
      q.y:=p.y-t_t[2,1]*dx
    end;

    Vgm_Read(ord('A')+1);

    if dn_k < 0 then
    org:=y_move(org.x,org.y,-dn_k);

    Vgm_Rect(org,Result,up_k-dn_k+1)
  end
end;

function TVgmBank.Out_vgm(DC: XCanvas; v: PLLine;
                          ind,cl: Integer; k: Double): Boolean;
var
  a,b: TPoint; dx,dy: int; p: TGauss;
begin
  Result:=false;

  if v.N >= 0 then

  if ind > 0 then
  if Vgm_Read(ind) > 0 then begin

    a:=v.Pol[0]; b:=a;
    if v.N = 1 then b:=v.Pol[1];

    if k < 0 then
    if hdr[6] > 0 then k:=Abs(k)/hdr[6];

    if k > 0 then begin
      t_t[1,1]:=k; t_t[1,2]:=0; t_t[1,3]:=0;
      t_t[2,1]:=0; t_t[2,2]:=k; t_t[2,3]:=0;
      t_t[3,1]:=0; t_t[3,2]:=0; t_t[3,3]:=1;

      dx:=b.X-a.X; dy:=b.Y-a.Y;
      if (dx <> 0) or (dy <> 0) then
      xy_Rotate_3x3(t_t,dx,dy);

      Vgm_Draw(DC,a,0,cl,ind);

      p:=X_Plot(a.X,a.Y,-hdr[3]);
      p:=Y_Plot(p.x,p.y,hdr[6]-hdr[4]);
      Vgm_Rect(p,hdr[5],1);

      Result:=true
    end
  end
end;

procedure TVgmBank.Plot_vgm(DC: TCanvas; const a,b: TPoint;
                            ind,cl: Integer; k: Double);
var
  paint: XPaintDC;
begin
  if ind > 0 then
  if Vgm_Read(ind) > 0 then begin
    paint:=XPaintDC.Create(DC);
    try
      Init_3x3(t_t, 0,0,k,k);
      if (a.x <> b.x) and (a.y <> b.y) then
      xy_Rotate_3x3(t_t,b.x-a.x,b.y-a.y);

      Vgm_Draw(paint,a,0,cl,ind)
    finally
      paint.Free
    end
  end
end;

procedure TVgmBank.Out_Sign(DC: XCanvas; const a,b: TPoint; i: Integer);
var
  len: Integer; k: double; p: TPoint;
begin
  if Vgm_Read(i) > 0 then begin
    len:=hdr[5]-hdr[3];
    if a.x = b.x then len:=hdr[6]-hdr[4];
    k:=1; if len > 0 then k:=Long_Dist(a,b)/len;

    t_t[1,1]:=k; t_t[1,2]:=0; t_t[1,3]:=0;
    t_t[2,1]:=0; t_t[2,2]:=k; t_t[2,3]:=0;
    t_t[3,1]:=0; t_t[3,2]:=0; t_t[3,3]:=1;

    p:=a; p.x:=a.x + Round(hdr[3]*k);

    if (p.x <> b.x) and (p.y <> b.y) then
    xy_Rotate_3x3(t_t,b.x-p.x,b.y-p.y);

    Vgm_Draw(DC,p,0,0,i)
  end
end;

procedure TVgmBank.Ico_Sign(DC: XCanvas; i, x,y,w,h: Integer);
var
  cx,cy,kx,ky,k: double; p: TPoint; R: TRect;
begin
  if Ico_Rect(i,@R) then begin

    kx:=w / (R.Right+R.Left);
    ky:=h / (R.Bottom+R.Top); k:=Min(kx,ky);

    cx:=(R.Left+R.Right) / 2;
    cy:=(R.Top+R.Bottom) / 2;

    t_t[1,1]:=k; t_t[1,2]:=0;  t_t[1,3]:=0;
    t_t[2,1]:=0;  t_t[2,2]:=k; t_t[2,3]:=0;
    t_t[3,1]:=0;  t_t[3,2]:=0;  t_t[3,3]:=1;

    p.x:=x + w div 2 + Round((hdr[3]-cx)*k);
    p.y:=y - h div 2 + Round((hdr[4]-cy)*k);

    Vgm_Draw(DC,p,0,0,i)
  end
end;

procedure TVgmBank.Rect_Sign(DC: TCanvas; const R: TRect; i: Integer);
var
  s,p: TPoint; paint: XPaintDC;
begin
  if Vgm_Read(i) > 0 then begin

    t_t[1,1]:=1; t_t[1,2]:=0; t_t[1,3]:=0;
    t_t[2,1]:=0; t_t[2,2]:=1; t_t[2,3]:=0;
    t_t[3,1]:=0; t_t[3,2]:=0; t_t[3,3]:=1;

    s.x:=R.Right-R.Left+1; s.y:=R.Bottom-R.Top+1;

    p.x:=R.Left+((s.x-hdr[5]) div 2) + hdr[3];
    p.y:=R.Top+((s.y-hdr[6]) div 2) + hdr[4];

    paint:=XPaintDC.Create(DC);
    try
      Vgm_Draw(paint,p,0,0,i);
    finally
      paint.Free
    end
  end
end;

procedure TVgmBank.Zoom_Sign(DC: TCanvas; const R: TRect; i,cl: int);
var
  x,y,w,h: Integer; k: Double;
  s,p: TPoint; xdc: XPaintDC;
begin
  if Vgm_Read(i) > 0 then begin

    s.x:=R.Right-R.Left+1; s.y:=R.Bottom-R.Top+1;
    x:=hdr[3]; y:=hdr[4]; w:=hdr[5]; h:=hdr[6];

    k:=Min(s.x/Max(w,1), s.y/Max(h,1));

    t_t[1,1]:=k; t_t[1,2]:=0; t_t[1,3]:=0;
    t_t[2,1]:=0; t_t[2,2]:=k; t_t[2,3]:=0;
    t_t[3,1]:=0; t_t[3,2]:=0; t_t[3,3]:=k;

    p.x:=R.Left + Round(((s.x - w*k) / 2) + x*k);
    p.y:=R.Top  + Round(((s.y - h*k) / 2) + y*k);

    fIsColor:=cl <> 0;

    xdc:=XPaintDC.Create(DC);
    try
      Vgm_Draw(xdc,p,0,cl,i);
    finally
      xdc.Free
    end;

    fIsColor:=false
  end
end;

function TVgmBank.Ico_Rect(i: Integer; R: PRect): Boolean;
var
  ind: longint; p: tVgmPixel;
  i1,i2,n,len: Integer;
begin
  Result:=false;

  if Vgm_Read(i) > 0 then begin

    len:=vm_BufLen(Loc.Ind);

    if len >= SizeOf(hdr) then
    if len <= vgm_sign_size then begin

      vm_Load(Loc.Ind+4,bufp^,len);
      ind:=SizeOf(hdr); Dec(len);

      R.Left:=hdr[5]; R.Right:=0;
      R.Top:=hdr[6]; R.Bottom:=0;

      for i1:=1 to hdr[1] do
      if ind < len then begin
        p.x:=bufp[ind]; Inc(ind);
        p.y:=bufp[ind]; Inc(ind);

        n:=p.x and $7F;

        if n = 0 then
          Inc(ind,2)
        else
        for i2:=0 to n do
        if ind < len then begin
          p.x:=bufp[ind]; Inc(ind);
          p.y:=bufp[ind]; Inc(ind);

          R.Left:=Min(R.Left,p.x);
          R.Top:=Min(R.Top,p.y);
          R.Right:=Max(R.Right,p.x);
          R.Bottom:=Max(R.Bottom,p.y);
        end;
      end;

      if R.Left < R.Right then
      if R.Top < R.Bottom then
      Result:=true
    end
  end
end;

function TVgmBank.Vgm_Height(i: Integer): float;
var
  top: longint;
begin
  Result:=0; if vm_Active then
  if (i > 0) and (i <= t_cnt) then begin
    top:=vm_Long(12); if top > 0 then begin
      Dec(i); vm_Load(top+i+i+i+i,Result,4)
    end
  end
end;

procedure TVgmBank.Dump_Height(i: Integer; h: float);

function Append_Height: longint;
var
  h: float; i: Integer;
begin
  vm_Load(12,Result,4); if Result = 0 then begin
    Result:=vm_Ind; vm_Store(12,vm_Ind,4); h:=0;
    for i:=1 to t_max do vm_Append(h,SizeOf(h))
  end
end;

var
  top: longint;
begin
  Dec(i); if vm_Update then
  if (i >= 0) and (i < t_max) then begin
    top:=Append_Height; if top > 0 then
    vm_Store(top+i+i+i+i,h,SizeOf(h))
  end
end;

procedure TVgmBank.Delete_Height(i: Integer);
var
  top,ind: longint; h: float;
begin
  if vm_Update then begin
    top:=vm_Long(12);
    if top > 0 then begin
      Dec(i); ind:=top+i+i+i+i;
      while i < t_cnt do begin
        vm_Load(ind+4,h,4);
        ind:=vm_Store(ind,h,4);
        Inc(i)
      end
    end
  end
end;

function TVgmBank.Y_Move(x,y,k: Double): TGauss;
begin
  Vgm_Read(ord('A')+1);
  Result:=Y_Plot(x,y,hdr[6]*k)
end;

function TVgmBank.IY_Move(x,y,k: Double): TPoint;
var
  g: tgauss;
begin
  g:=Y_Move(x,y,k);
  Result.x:=Round(g.x);
  Result.y:=Round(g.y)
end;

function TVgmBank.Y_Plot(x,y,h: Double): tgauss;
begin
  Result.x:=x - h * t_t[1,2];
  Result.y:=y + h * t_t[2,2]
end;

function TVgmBank.X_Plot(x,y: Double; w: Integer): tgauss;
begin
  Result.x:=x + t_t[1,1]*w;
  Result.y:=y - t_t[2,1]*w
end;

procedure TVgmBank.Vgm_Rect(const a: TGauss; w: Integer; k: Double);
var
  b: TGauss; i: Integer;
begin
  k:=(hdr[6]-1)*k;
  b:=X_Plot(a.x,a.y,w);

  g_Rect[0]:=a; g_Rect[1]:=Y_Plot(a.x,a.y,-k);
  g_Rect[2]:=Y_Plot(b.x,b.y,-k); g_Rect[3]:=b;

  for i:=0 to 3 do
  v_Rect[i]:=_IGauss(g_Rect[i]);

  g_Rect[4]:=g_Rect[0];
  v_Rect[4]:=v_Rect[0];

  Virt_Rect(Self)
end;

procedure TVgmBank.Set_Vgm_Rect(G: PGPoly);
var
  i: Integer;
begin
  for i:=0 to 3 do begin
    g_Rect[i]:=G[i];
    v_Rect[i]:=_IGauss(g_Rect[i]);
  end;

  g_Rect[4]:=g_Rect[0];
  v_Rect[4]:=v_Rect[0];

  Virt_Rect(Self)
end;

function TVgmBank.Vgm_Size: tgauss;
begin
  Result.x:=Gauss_Dist(g_Rect[0],g_Rect[3]);
  Result.y:=Gauss_Dist(g_Rect[0],g_Rect[1]);
end;

procedure TVgmBank.Virt_Rect(Sender: TObject);
begin
end;

procedure TVgmBank.Poly_Line(DC: XCanvas; lp: pPoly; n,s,cl: Integer; k: double);

procedure ipoly_add(p: TPoint);
begin
  if (ipoly_n < 0)
  or not Points_Equal(ipoly[ipoly_n],p) then begin
    Inc(ipoly_n); ipoly[ipoly_n]:=p;

    if ipoly_n = 255 then begin
      Vgm_Poly(DC,ipoly,ipoly_n,ipoly_cl,0);
      ipoly[0]:=p; ipoly_n:=0
    end
  end
end;

procedure ipoly_next(const a,b: TPoint; ix,iy: Double);
var
  p: TPoint;
begin
  Get_Norm_Point(a,b,ix,iy,p);
  ipoly_add(p)
end;

var
  width,height: double; top,pix: tVgmPixel;
  len,run,pos,rib,nxt,r: double; a,b: TPoint;
  i,j,jn, ip,ic: integer; ind: longint;
  p,q: tgauss; a_,a1,a2: TPoint;
begin
  while (n > 0) and Points_Equal(lp[n-1],lp[n]) do
  Dec(n); if n > 0 then

  if vm_Active then
  if Assigned(xpoly) then
  if Vgm_Read(s) > 0 then begin

    k:=Abs(Vgm_Height(s))*k/hdr[6];
    height:=hdr[6]*k; width:=hdr[5]*k;

    if (height <= 1) or (width < 1) then begin
      vm_Load(Loc.Ind+4+SizeOf(hdr),top,2);
      if cl = 0 then cl:=top.y;
      Vgm_Poly(DC,pLPoly(lp),n,cl,0);
      Exit
    end;

    len:=0; for i:=0 to n-1 do
    len:=len+Points_Dist(lp[i],lp[i+1]);

    run:=0;
    while run < len do begin

      ind:=Loc.Ind+4+SizeOf(hdr);

      for i:=1 to hdr[1] do begin ic:=0;

        ind:=vm_Load(ind,top,2); jn:=top.x and $7F;
        ipoly_cl:=cl; if cl = 0 then ipoly_cl:=top.y;

        for j:=0 to jn do begin
          ind:=vm_Load(ind,pix,2);
          p.x:=run+pix.x*k;
          p.y:=-hdr[4]*k+pix.y*k;

          if p.x < len then Inc(ic);
          xpoly[j]:=p;

          if j > 0 then begin
            if q.x > len then begin
              xpoly[j-1].y:=CutLine(p.x,p.y,q.x,q.y, len);
              xpoly[j-1].x:=len
            end;

            if p.x > len then begin
              xpoly[j].y:=CutLine(q.x,q.y,p.x,p.y, len);
              xpoly[j].x:=len
            end
          end;

          q:=p
        end;

        if ic > 0 then
        if jn > 0 then begin

          ipoly_n:=-1; p:=xpoly[0];
          pos:=0; b:=TPoint(lp[0]); ip:=0;

          while ip < n do begin
            a:=b; Inc(ip); b:=TPoint(lp[ip]);
            nxt:=pos+Long_Dist(a,b);
            if p.x <= nxt then Break;
            pos:=nxt
          end;

          ipoly_next(a,b,p.x-pos,p.y);

          for j:=1 to jn do begin
            q:=p; p:=xpoly[j];

            if p.x < pos then

              while (pos > 0) and (p.x < pos) do begin
                b:=a; Dec(ip); a:=TPoint(lp[ip]);
                pos:=pos-Long_Dist(a,b)
              end

            else
            if p.x > pos then

              while ip < n do begin
                nxt:=pos+Long_Dist(a,b);
                if p.x <= nxt then Break;

                a_:=a; a:=b; Inc(ip); b:=TPoint(lp[ip]);

                if nxt > pos then begin
                  r:=CutLine(q.x,q.y,p.x,p.y,nxt);
                  Get_Tube_Points(a_,a,b, r,r, a1,a2);
                  ipoly_add(a1); pos:=nxt
                end
              end;

            ipoly_next(a,b,p.x-pos,p.y)
          end;

          if ipoly_n > 0 then
          Vgm_Poly(DC,ipoly,ipoly_n,ipoly_cl,0)
        end
      end;

      run:=run + width
    end
  end
end;

procedure TVgmBank.Vgm_Start(DC: XCanvas);
begin
end;

procedure TVgmBank.Vgm_Poly(DC: XCanvas; lp: PLPoly; N, Cl,Tag: Integer);
begin
  DC.xPen(1,cl);

  if (tag = 0) or not DC.Fill then
    DC.PolyLine(pPoly(lp),N)
  else begin
    DC.xBrush(0,cl);
    DC.Polygon(PPoly(lp),N)
  end
end;

procedure TVgmBank.Vgm_Close(DC: XCanvas);
begin
end;

procedure TVgmSign.Vgm_Poly(DC: XCanvas; lp: PLPoly; N, Cl,Tag: Integer);
var
  lt,rb: TPoint;
begin
  if fIs_Bound then begin
    Max_Poly_Bound(lp,N+1, lt,rb);

    if fIs_Begin then begin
      fdc_lt:=lt; fdc_rb:=rb;
      fIs_Begin:=false
    end else

    Add_LRect(fdc_lt,fdc_rb,lt,rb)
  end else

  inherited Vgm_Poly(DC, lp,N, Cl,Tag);
end;

function TVgmSign.Get_Width: Integer;
begin
  Result:=fdc_rb.x - fdc_lt.x;
end;

function TVgmSign.Get_Height: Integer;
begin
  Result:=fdc_rb.y - fdc_lt.y
end;

end.