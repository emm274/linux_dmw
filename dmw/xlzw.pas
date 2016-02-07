unit xlzw; interface

uses
  OTypes;

type
  phoff_code = ^thoff_code;
  thoff_code = record len,cod,run,tag: word end;

  phoff_codes = ^thoff_codes;
  thoff_codes = array[0..1023] of thoff_code;

  thoff_ind = array[1..16] of int;
  thoff_tbl = array[1..128] of thoff_code;

  THoffman = class
    constructor Create;
    destructor Destroy; override;

    function Unpack_gr3(AStm: PBytes; AStmSize: Integer;
                        Bmp: PBytes; RowCount: Integer;
                        biWidth,biSize: Integer): Integer;

    function Unpack_gr4(AStm: PBytes; AStmSize: Integer;
                        Bmp: PBytes; RowCount: Integer;
                        biWidth,biSize: Integer): Integer;

  private
    main_tbl: thoff_tbl;
    main_ind: thoff_ind;

    white_tbl: thoff_tbl;
    white_ind: thoff_ind;

    black_tbl: thoff_tbl;
    black_ind: thoff_ind;

    fCount: Integer;

    fstm: PBytes;
    fStmSize: Integer;
    fStmIndex: Integer;

    fByte,fBit: byte;

    fRuns: PWords;
    fRunCapacity: Integer;
    fRunSize: Integer;

    hoff_t: text;

    function InByte: byte;

    function Hoffman_run(const tbl: thoff_tbl;
                         const ind: thoff_ind): phoff_code;

    function x_Hoffman_run(const tbl: thoff_tbl;
                           const ind: thoff_ind): int;

  public
    property Count: Integer read fCount;
  end;

const
  lzw_table_max = 4096;
type
  tlzw_table = array[0..4095] of WPoint;

  tlzw = class
    constructor Create(si: Pointer; len: Integer;
                       di: Pointer; size: Integer);

    procedure Unpack;

  private
    si_ptr: PBytes;
    si_len: Integer;

    di_ptr: PBytes;
    di_size: Integer;

    NumBits: Integer;
    acc,bit: byte;

    table: tlzw_table;
    TableCount: Integer;
  end;

var
  Hoffman: THoffman;

implementation

uses
  Sysutils,
  Convert,OFiles;

const
  hoff_check = false;
  hoff_codes = false;
  hoff_bmp   = true;

  B_Bit: array[0..7] of byte =
  ($80,$40,$20,$10,$08,$04,$02,$01);

  cf2_run_horz_val = 3;
  cf2_run_horz_cod = 1;

  cf2_run_pass_val = 1;
  cf2_run_pass_len = 4;

  run_eol_code_len = 12;
  run_eol_code_val = 1;

  gr4_horz = 1;
  gr4_pass = 2;
  gr4_v0   = 3;
  gr4_vr   = 4;
  gr4_vl   = 5;
  gr4_eol  = 6;
  hoff_eol = 10;

constructor THoffman.Create;

function hoff_code(len,cod,run,tag: Integer): thoff_code;
begin
  Result.len:=len;
  Result.cod:=cod;
  Result.run:=run;
  Result.tag:=tag;
end;

function Bits(s: PChar): Integer;
var
  p: PChar;
begin
  Result:=0; p:=s;
  while p^ <> #0 do begin
    Result:=(Result shl 1)+(ord(p^)-ord('0'));
    p:=@p[1]
  end
end;

procedure Sort_tbl(var tbl: thoff_tbl;
                   var ind: thoff_ind;
                   ACount: int);
var
  rec: thoff_code; i,j,len: Integer;
begin
  for i:=1 to ACount-1 do
  for j:=i+1 to ACount do begin
    if tbl[i].len > tbl[j].len then begin
      rec:=tbl[i]; tbl[i]:=tbl[j]; tbl[j]:=rec
    end
  end;

  for i:=1 to 16 do ind[i]:=1;

  i:=1; while i <= ACount do begin
    len:=tbl[i].len;

    if len in [1..16] then begin

      while (i <= ACount) and (tbl[i].len = len) do
      Inc(i); ind[len]:=i;

    end else Break
  end;
end;

var
  txt: TTextFile; r: thoff_code;
  rc: Integer; s,t: TShortStr;
begin
  inherited;

  Fillchar(r,Sizeof(r),0);

  txt:=TTextFile.Create;
  try
    if txt.Open_bin('tiff.gr3') then

    while not txt.End_of_file do
    if txt.StrRead(s) <> nil then
    if Trunc_comment('-/',s) <> nil then
    if StrLen(s) > 0 then begin

      StrToken(t,s);

      if StrComp(t,'EOL') = 0 then begin
        r.run:=$FFFF; r.tag:=hoff_eol
      end
      else begin
        val(t,r.run,rc); r.tag:=0
      end;

      if StrToken(t,s) <> nil then begin
        r.len:=StrLen(t); r.cod:=Bits(t)
      end;

      Inc(fCount); white_tbl[fCount]:=r;

      if StrToken(t,s) <> nil then begin
        r.len:=StrLen(t); r.cod:=Bits(t)
      end; black_tbl[fCount]:=r;

      if fCount = 128 then Break
    end;

  finally
    txt.Free
  end;

  Sort_tbl(white_tbl,white_ind,fCount);
  Sort_tbl(black_tbl,black_ind,fCount);

  main_tbl[1]:=hoff_code(12,Bits('000000000001'),0,0);
  main_tbl[2]:=hoff_code(4,Bits('0001'),0,gr4_pass);

  main_tbl[3]:=hoff_code(1,Bits('1')      ,0,gr4_v0);
  main_tbl[4]:=hoff_code(3,Bits('011')    ,1,gr4_vr);
  main_tbl[5]:=hoff_code(6,Bits('000011') ,2,gr4_vr);
  main_tbl[6]:=hoff_code(7,Bits('0000011'),3,gr4_vr);
  main_tbl[7]:=hoff_code(3,Bits('010')    ,1,gr4_vl);
  main_tbl[8]:=hoff_code(6,Bits('000010') ,2,gr4_vl);
  main_tbl[9]:=hoff_code(7,Bits('0000010'),3,gr4_vl);

  main_tbl[10]:=hoff_code(3,Bits('001'),0,gr4_horz);
  main_tbl[11]:=hoff_code(10,Bits('0000001111'),0,0);

  main_tbl[12]:=hoff_code(7,Bits('0000000'),0,gr4_eol);
  Sort_tbl(main_tbl,main_ind,12);

  if hoff_check then
  if hoff_bmp then begin
    System.Assign(hoff_t,'\dmw\gr4');
    Rewrite(hoff_t)
  end
end;

destructor THoffman.Destroy;
begin
  xFreePtr(fRuns);

  if hoff_check then
  if hoff_bmp then
  System.Close(hoff_t);
end;

function THoffman.InByte: byte;
begin
  Result:=0;

  if fStmIndex < fStmSize then begin
    Result:=fstm[fStmIndex]; Inc(fStmIndex)
  end
end;

function THoffman.Hoffman_run(const tbl: thoff_tbl;
                              const ind: thoff_ind): phoff_code;
var
  cod, ind_i,i,i1,i2: Integer; lp: phoff_codes;
begin
  Result:=nil; i1:=1; cod:=0; lp:=@tbl;

  for ind_i:=1 to 16 do begin
    i2:=ind[ind_i];

    if fBit = 8 then begin
      if fStmIndex = fStmSize then Break;
      fByte:=InByte; fBit:=0;
    end;

    cod:=cod shl 1; Inc(fBit);
    if fByte and $80 <> 0 then cod:=cod or 1;
    fByte:=fByte shl 1;

    for i:=i1 to i2-1 do begin

      if lp[0].cod = cod then begin

        Result:=@lp[0];

        if hoff_check then
        if Result.tag = hoff_eol then
        writeln(hoff_t,'EOL');

        Exit
      end;

      lp:=@lp[1]
    end;

    if i2 > i1 then i1:=i2
  end;

  cod:=cod
end;

function THoffman.x_Hoffman_run(const tbl: thoff_tbl;
                                const ind: thoff_ind): Integer;
var
  loc: phoff_code;
begin
  Result:=0;

  while true do begin
    loc:=Hoffman_run(tbl,ind);

    if loc = nil then begin
      Result:=-1; Break
    end else
    if loc.tag = hoff_eol then begin
      Result:=-1; Break
    end
    else Inc(Result,loc.run);

    if hoff_codes then
    write(hoff_t,loc.run,' ');

    if loc.run < 64 then Break
  end
end;

function THoffman.Unpack_gr3(AStm: PBytes; AStmSize: Integer;
                             Bmp: PBytes; RowCount: Integer;
                             biWidth,biSize: Integer): Integer;
var
  iy, i, ofs, cx,l1,l2: Integer; p: PByte;
begin
  Result:=0;

  fStm:=AStm; fStmSize:=AStmSize;
  fStmIndex:=0; fBit:=8; ofs:=0;

  Fillchar(Bmp^,RowCount * biSize,0);

  if hoff_check then begin
    System.Assign(hoff_t,'\dmw\gr3');
    Rewrite(hoff_t)
  end;

  repeat
   l1:=x_Hoffman_run(white_tbl,white_ind);
  until l1 < 0;

  for iy:=1 to RowCount do begin

    cx:=0;
    while cx < biWidth do begin

      l1:=x_Hoffman_run(white_tbl,white_ind);

      if l1 < 0 then Break; Inc(cx,l1);

      if hoff_check then
      writeln(hoff_t,'1 ',l1,' ',cx);

      l2:=x_Hoffman_run(black_tbl,black_ind);

      if l2 < 0 then Break;

      for i:=1 to l2 do begin
        if cx < biWidth then begin
          p:=@Bmp[ofs + cx shr 3];
          p^:=p^ or B_Bit[cx and 7]
        end;

        Inc(cx)
      end;

      if hoff_check then
      writeln(hoff_t,'0 ',l2,' ',cx);
    end;

    Inc(ofs,biSize)
  end;

  if hoff_check then begin
    writeln(hoff_t,'Strip ',fStmIndex,' ',fStmSize);
    System.Close(hoff_t)
  end
end;

function THoffman.Unpack_gr4(AStm: PBytes; AStmSize: Integer;
                             Bmp: PBytes; RowCount: Integer;
                             biWidth,biSize: Integer): Integer;
var
  RunLength: Integer;           // Length of current run
  a0: Integer;                  // reference element
  b1: Integer;                  // next change on previous line
  run0,run1: PWords;            // run length arrays
  thisrun,pa,pb: PWords;        // pointers into runs
  LineNum: Integer;             // line number

  rc: phoff_code; ax,bx,l1,l2: Integer;
  i,j,rows: Integer; p: PByte;
begin
  Result:=0;

  fStm:=AStm; fStmSize:=AStmSize;
  fStmIndex:=0; fBit:=8;

  Fillchar(Bmp^,RowCount * biSize,0);

  if hoff_check then
  if not hoff_bmp then begin
    System.Assign(hoff_t,'\dmw\gr4');
    Rewrite(hoff_t)
  end;

  fRunSize:=biWidth * 2;
  if fRunSize*2 > fRunCapacity then begin
    fRunCapacity:=int_Round(fRunSize*2,1024);
    fRuns:=xReAllocPtr(fRuns,fRunCapacity*Sizeof(Word));
    if fRuns = nil then fRunCapacity:=0
  end;

  if Assigned(fRuns) then begin
    run0:=fRuns;
    run1:=@fRuns[fRunSize];

    run1[0]:=biWidth;           // initial reference line
    run1[1]:=0;

    LineNum:=0; rows:=RowCount;
    while rows > 0 do begin

      Dec(rows); RunLength:=0;

      if Odd(LineNum) then begin
        pa:=run1; pb:=run0
      end
      else begin
        pa:=run0; pb:=run1
      end;

      thisrun:=pa; a0:=0; bx:=0;
      b1:=pb[0]; pb:=@pb[1];

      while a0 < biWidth do begin

        rc:=Hoffman_run(main_tbl,main_ind);
        if rc = nil then begin if hoff_check then
          writeln(hoff_t,'*** main unk'); rows:=0; Break;
        end;

        if rc.tag = gr4_horz then begin

          if Odd(bx) then begin
            l1:=x_Hoffman_run(black_tbl,black_ind);
            if l1 < 0 then begin if hoff_check then
              writeln(hoff_t,'*** black unk'); rows:=0; Break;
            end;

            l2:=x_Hoffman_run(white_tbl,white_ind);
            if l2 < 0 then begin if hoff_check then
              writeln(hoff_t,'*** white unk'); rows:=0; Break;
            end;
          end
          else begin
            l1:=x_Hoffman_run(white_tbl,white_ind);
            if l1 < 0 then begin if hoff_check then
              writeln(hoff_t,'*** white unk'); rows:=0; Break;
            end;

            l2:=x_Hoffman_run(black_tbl,black_ind);
            if l2 < 0 then begin if hoff_check then
              writeln(hoff_t,'*** black unk'); rows:=0; Break;
            end
          end;

          pa[0]:=RunLength + l1; pa:=@pa[1];
          Inc(bx); Inc(a0,l1); RunLength:=0;

          pa[0]:=RunLength + l2; pa:=@pa[1];
          Inc(bx); Inc(a0,l2); RunLength:=0;

          if hoff_check then

          if Odd(bx) then
            writeln(hoff_t,'#horz b',l1,' w',l2)
          else
            writeln(hoff_t,'#horz w',l1,' b',l2)
        end;

        if pa <> thisrun then     // check b1
        while (b1 <= a0) and (b1 < biWidth) do begin
          Inc(b1, pb[0] + pb[1]); pb:=@pb[2]
        end;

        case rc.tag of
      gr4_pass:
          begin
            Inc(b1,pb[0]); pb:=@pb[1];
            Inc(RunLength,b1-a0); a0:=b1;
            Inc(b1,pb[0]); pb:=@pb[1];

            if hoff_check then
            writeln(hoff_t,'#pass a0=',a0);
          end;

      gr4_v0:
          begin
            ax:=b1 - a0;            // SetVal
            pa[0]:=RunLength + ax; pa:=@pa[1];
            Inc(bx); Inc(a0,ax); RunLength:=0;

            Inc(b1,pb[0]); pb:=@pb[1];

            if hoff_check then
            writeln(hoff_t,'#v0 a0=',a0);
          end;

      gr4_vr:
          begin
            if hoff_check then
            writeln(hoff_t,'#vr ',rc.run);

            ax:=b1 - a0 + rc.run;   // SetVal
            pa[0]:=RunLength + ax; pa:=@pa[1];
            Inc(bx); Inc(a0,ax); RunLength:=0;

            Inc(b1,pb[0]); pb:=@pb[1];
          end;

      gr4_vl:
          begin
            if hoff_check then
            writeln(hoff_t,'#vl ',rc.run);

            ax:=b1 - a0 - rc.run;   // SetVal
            pa[0]:=RunLength + ax; pa:=@pa[1];
            Inc(bx); Inc(a0,ax); RunLength:=0;

            Dec(TPointer(pb),2);
            Dec(b1,pb[0])
          end;

      gr4_eol:
          begin
            if hoff_check then
            writeln(hoff_t,'#eol');

            pa[0]:=biWidth - a0; Break
          end;

      gr4_horz: ;

        end
      end;

      if RunLength > 0 then begin
        rc:=Hoffman_run(main_tbl,main_ind);

        if (rc = nil) or (rc.tag <> gr4_v0) then begin
          if hoff_check then
          writeln(hoff_t,'*** pass err');
          rows:=0; Break;
        end;

        pa[0]:=RunLength; pa:=@pa[1];
        Inc(bx); RunLength:=0;
      end;

      if a0 < biWidth then begin
        if Odd(bx) then begin
          pa[0]:=RunLength; pa:=@pa[1];
          Inc(bx); RunLength:=0;
        end;

        ax:=biWidth - a0;
        pa[0]:=RunLength + ax; pa:=@pa[1];
        Inc(bx); Inc(a0,ax); RunLength:=0;
      end;

      pa[0]:=RunLength; pa:=@pa[1];
      Inc(bx); RunLength:=0;

      if hoff_check then
      if bx >= 2 then write(hoff_t,'run');

      a0:=0; pa:=thisrun;
      for i:=1 to bx div 2 do begin

        if hoff_check then
        write(hoff_t,' ',pa[0],' ',pa[1]);

        Inc(a0,pa[0]); pa:=@pa[1];

        for j:=1 to pa[0] do begin

          if a0 < biWidth then begin
            p:=@Bmp[a0 shr 3];
            p^:=p^ or B_Bit[a0 and 7]
          end;

          Inc(a0)
        end;

        pa:=@pa[1]
      end;

      if hoff_check then writeln(hoff_t);

      Bmp:=@Bmp[biSize]; Inc(LineNum)
    end
  end;

  if hoff_check then begin
    writeln(hoff_t,'Strip ',fStmIndex,' ',fStmSize);
    if not hoff_bmp then System.Close(hoff_t)
  end
end;

constructor tlzw.Create(si: Pointer; len: Integer;
                        di: Pointer; size: Integer);
begin
  inherited Create;
  si_ptr:=si; si_len:=len;
  di_ptr:=di; di_size:=size
end;

procedure tlzw.Unpack;

function InByte: Integer;
var
  al: Byte;
begin
  Result:=257;

  if si_len > 0 then begin
    Result:=si_ptr[0];
    si_ptr:=@si_ptr[1];
    Dec(si_len)
  end
end;

function Read_Lzw: Integer;
var
  i,al: Integer;
begin
  Result:=0;

  for i:=1 to NumBits do begin
    bit:=bit shr 1; if bit = 0 then begin
      al:=Inbyte; if al = 257 then
      begin Result:=257; Break end;
      bit:=$80; acc:=al;
    end;

    Result:=(Result shl 1) or ord(acc and bit > 0)
  end
end;

procedure Write_Dst(ch: Integer);
begin
  if di_size > 0 then begin
    di_ptr[0]:=ch;
    di_ptr:=@di_ptr[1];
    Dec(di_size)
  end
end;

function Popup(ch: Integer): Integer;
var
  p: wpoint;
begin
  if ch < 256 then Write_Dst(ch) else begin
    Dec(ch,258); if ch < TableCount then begin
      p:=table[ch]; ch:=Popup(p.y); p.x:=Popup(p.x)
    end else ch:=0
  end;

  Result:=ch
end;

var
  ax,MaxCode: Integer; s: wpoint;
begin
  NumBits:=9; MaxCode:=512;
  TableCount:=0; bit:=0; acc:=0;

  ax:=Read_Lzw; if ax = 256 then
  ax:=Read_Lzw; Write_Dst(ax);

  while true do begin
    s.y:=ax; ax:=Read_Lzw; if ax = 257 then Break;

    if ax = 256 then begin
      NumBits:=9; MaxCode:=512; TableCount:=0;
      ax:=Read_Lzw; Write_Dst(ax)
    end else begin
      s.x:=ax;

      if s.x < TableCount + 258 then s.x:=Popup(s.x) else
      begin s.x:=Popup(s.y); Write_Dst(s.x) end;

      if TableCount < lzw_table_max then begin
        table[TableCount]:=s; Inc(TableCount)
      end;

      if TableCount + 259 = MaxCode then begin
        Inc(NumBits); MaxCode:=MaxCode shl 1
      end
    end
  end
end;

initialization
begin
  Hoffman:=THoffman.Create;
end;

finalization
begin
  Hoffman.Free;
end;

end.