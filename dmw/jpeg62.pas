unit jpeg62; interface

uses
  Windows,Classes,Math,LCLType,jpeglib,
  otypes,ijpg,xclasses;

type
  tjpegTable_ptrs = record
    quant: array[0..NUM_QUANT_TBLS -1] of JQUANT_TBL_ptr;
    dc_huff: array[0..NUM_HUFF_TBLS -1] of JHUFF_TBL_ptr;
    ac_huff: array[0..NUM_HUFF_TBLS -1] of JHUFF_TBL_ptr;
  end;

  tjpegObj = class(TInterfacedObject,IJpeg)

    constructor Create;
    destructor Destroy; override;

    function GetTablesSize(var Tables: tjpegTables): int; stdcall;

    function LoadTables(var Tables: tjpegTables;
                        Data: Pointer; DataLen: int): Boolean; stdcall;

    function GetInfo(Jpeg: Pointer; JpegLen: int;
                     Path: PChar; Info: PBitmap): Boolean; stdcall;

    function Decompress(dib: Pointer;
                        dib_w,dib_h,dib_bits: int;
                        Jpeg: Pointer; JpegLen: int;
                        Tables: Pointer): Boolean; stdcall;

    function Decompress1(dib: Pointer;
                         dib_w,dib_h,dib_bits: int;
                         posX,posY,zoom: int;
                         Jpeg: Pointer; JpegLen: int;
                         Tables: Pointer): Boolean; stdcall;

    function Decompressf(dib: Pointer;
                         dib_w,dib_h,dib_bits: int;
                         Jpeg: PChar): Boolean; stdcall;

    function Compress(Dest: Pointer; DestSize: int;

                      Dib: Pointer;
                      Dib_w,Dib_h,Dib_bits: int;
                      Quality: int): int; stdcall;

    function Compressf(Dest: PChar;
                       const img: IImage;
                       Quality: int): int64; stdcall;

  private
    fStream: XMemoryStream;
    fbuf: XMemoryStream;

    mErr: jpeg_error_mgr;

    dInfoInit: bool;
    dInfo: jpeg_decompress_struct;

    cInfoInit: bool;
    cInfo: jpeg_compress_struct;

    fTables: tjpegTable_ptrs;

    function back_tables(Tables: Pointer): Boolean;
  end;

implementation

uses
  Graphics,
  jcomAPI,
  JMemMgr,
  JdAPImin,
  JdAPIstd,
  JcAPImin,
  jcparam,
  JcAPIstd,
  JDataSrc,
  OFiles,
  img_x;

type
  EJPEG = class(EInvalidGraphic);

  PJPeg_Tables = ^TJPeg_Tables;
  TJPeg_Tables = record
    nqtables      : DWORD;
    nhuffActables : DWORD;
    nhuffDctables : DWORD;
    quant: array[0..NUM_QUANT_TBLS-1] of JQUANT_TBL;
    dc_huff: array[0..NUM_HUFF_TBLS-1] of JHUFF_TBL;
    ac_huff: array[0..NUM_HUFF_TBLS-1] of JHUFF_TBL;
  end;

const
  jpeg_raise: boolean = true;
  jpeg_err_code: Integer = 0;

procedure JpegError(cinfo: j_common_ptr);
begin
  jpeg_err_code:=cinfo.err.msg_code;

  cinfo.global_state:=0;
  jpeg_abort(cinfo);

  if jpeg_raise then
  raise EJPEG.CreateFmt('JPEG error #%d',[jpeg_err_code])
end;

procedure EmitMessage(cinfo: j_common_ptr; msg_level: int);
begin
  //!!
end;

procedure OutputMessage(cinfo: j_common_ptr);
begin
  cinfo.global_state:=0;
end;

procedure FormatMessage(cinfo: j_common_ptr; var buffer: AnsiString);
begin
end;

procedure ResetErrorMgr(cinfo: j_common_ptr);
begin
  cinfo.err.num_warnings:=0;
  cinfo.err.msg_code:=0;
end;

constructor tjpegObj.Create;
begin
  inherited;

  fStream:=XMemoryStream.Create(nil,0);
  fbuf:=XMemoryStream.Create(nil,0);

  with mErr do begin
    error_exit:=JpegError;
    emit_message:=EmitMessage;
    output_message:=OutputMessage;
    format_message:=FormatMessage;
    reset_error_mgr:=ResetErrorMgr;
  end
end;

destructor tjpegObj.Destroy;
begin
  if dInfoInit then
  jpeg_destroy_decompress(@dInfo);

  if cInfoInit then
  jpeg_destroy_compress(@cInfo);

  fbuf.Free;
  fStream.Free;

  inherited
end;

function tjpegObj.GetTablesSize(var Tables: tjpegTables): int;
begin
  Tables.IsValid:=false;
  Tables.size:=Sizeof(TJPeg_Tables);
  Result:=Tables.size
end;

function tjpegObj.LoadTables(var Tables: tjpegTables;
                             Data: Pointer; DataLen: int): Boolean;

procedure huff(stm: XReadStream; out tb: JHUFF_TBL);
var
  i,cx,sz: int;
begin
  Fillchar(tb,Sizeof(tb),0);

  sz:=stm.Size;
  if stm.Position+16 > sz then
    stm.Position:=sz
  else begin
    stm.Read(tb.bits[1],16);

    cx:=0;
    for i:=1 to 16 do Inc(cx,tb.bits[i]);

    if stm.Position+cx > sz then
      stm.Position:=sz
    else
      stm.Read(tb.huffval,cx)
  end
end;

var
  stm1,stm2: XReadStream;
  p: PJPeg_Tables; i,j: int;
  mk,len,sz: Word; ht: JHUFF_TBL;
  al,tag: byte;
begin
  Tables.IsValid:=false;

  p:=Tables.data;
  if Assigned(p) then begin

    p.nqtables:=0;
    p.nhuffActables:=0;
    p.nhuffDctables:=0;

    sz:=DataLen;
    stm1:=XReadStream.Create(Data,DataLen);
    stm2:=XReadStream.Create(nil,0);
    try
      if stm1.x_word($D8FF) then   // SOI

      while stm1.get_word(mk) do

      if mk and $FF <> $FF then
        Break
      else
      if not stm1.get_word(len) then
        Break
      else begin

        len:=SwapWord(len);

        if len < 2 then
          Break
        else
        if stm1.Position+len-2 > sz then
          Break
        else begin
          mk:=mk shr 8; Dec(len,2);
          stm2.Assign(stm1.Get_pointer(-1),len);

          if mk = $DB then begin
            i:=p.nqtables;

            if len = 65 then
            if i < NUM_QUANT_TBLS then begin
              stm2.Get_byte(tag);
              for j:=0 to 63 do begin
                stm2.Read(al,1);
                p.quant[i].quantval[j]:=al
              end;

              p.nqtables:=i+1
            end
          end else
          if mk = $C4 then begin
            while stm2.Get_byte(tag) do begin
              huff(stm2,ht);
              if tag shr 4 = 0 then begin // DC
                i:=p.nhuffDctables;
                if i < NUM_HUFF_TBLS then begin
                  p.dc_huff[i]:=ht;
                  p.nhuffDctables:=i+1
                end
              end
              else begin
                i:=p.nhuffActables;
                if i < NUM_HUFF_TBLS then begin
                  p.Ac_huff[i]:=ht;
                  p.nhuffActables:=i+1
                end
              end
            end
          end;

          stm1.Seek(len,soFromCurrent)
        end
      end
    finally
      stm2.Free;
      stm1.Free
    end;

    Tables.IsValid:=true;
  end;

  Result:=Tables.IsValid
end;

function tjpegObj.back_tables(Tables: Pointer): Boolean;

procedure zigzag(var src,dst: JQUANT_TBL);
const
  _zigzag: Array[0..63] of int =
    ( 1,  2,  6,  7, 15, 16, 28, 29,
      3,  5,  8, 14, 17, 27, 30, 43,
      4,  9, 13, 18, 26, 31, 42, 44,
     10, 12, 19, 25, 32, 41, 45, 54,
     11, 20, 24, 33, 40, 46, 53, 55,
     21, 23, 34, 39, 47, 52, 56, 61,
     22, 35, 38, 48, 51, 57, 60, 62,
     36, 37, 49, 50, 58, 59, 63, 64);
var
  i,j: int;
begin
  for i:=0 to 63 do begin
    j:=_zigzag[i]-1;
    dst.quantval[i]:=src.quantval[j]
  end
end;

var
  tb1: pjpegTables;
  tb: PJPeg_Tables; i: int; p: Pointer;
begin
  Result:=false;

  tb1:=Tables;
  if tb1.IsValid then begin

    tb:=tb1.data;

    for i:=0 to NUM_QUANT_TBLS-1 do
    if fTables.quant[i] = nil then
    fTables.quant[i]:=dInfo.mem.alloc_small(@dInfo,0,sizeOf(JQUANT_TBL));

    for i:=0 to NUM_HUFF_TBLS-1 do begin
      if fTables.dc_huff[i] = nil then
      fTables.dc_huff[i]:=dInfo.mem.alloc_small(@dInfo,0,sizeOf(JHUFF_TBL));
      if fTables.ac_huff[i] = nil then
      fTables.ac_huff[i]:=dInfo.mem.alloc_small(@dInfo,0,sizeOf(JHUFF_TBL));
    end;

    for i:=0 to NUM_QUANT_TBLS-1 do begin
      p:=nil;
      if i < tb.nqtables then begin
        zigzag(tb.quant[i],fTables.quant[i]^);
        p:=fTables.quant[i]
      end;

      dInfo.quant_tbl_ptrs[i]:=p
    end;

    for i:=0 to NUM_HUFF_TBLS-1 do begin
      p:=nil;
      if i < tb.nhuffActables then begin
        fTables.ac_huff[i]^:=tb.ac_huff[i];
        p:=fTables.ac_huff[i]
      end;

      dInfo.ac_huff_tbl_ptrs[i]:=p
    end;

    for i:=0 to NUM_HUFF_TBLS-1 do begin
      p:=nil;
      if i < tb.nhuffDctables then begin
        fTables.dc_huff[i]^:=tb.dc_huff[i];
        p:=fTables.dc_huff[i]
      end;
      dInfo.dc_huff_tbl_ptrs[i]:=p
    end;

    Result:=true;
  end
end;

function tjpegObj.GetInfo(Jpeg: Pointer; JpegLen: int;
                          Path: PChar; Info: PBitmap): Boolean;
var
  vm: TReadfile;
begin
  Result:=false;

  if Assigned(Path) then begin

      vm:=TReadfile.Create;
      try
        if vm.Open(Path) then
        if vm.Size >= 64 then
        if vm.Buf[0] = $FF then
        Result:=GetInfo(vm.Buf,vm.Size,nil,Info)
      finally
        vm.Free
      end

  end
  else begin
    if not dInfoInit then begin
      dInfo.err:=@mErr;
      jpeg_create_decompress(@dInfo);
      jinit_memory_mgr(@dInfo);
      dInfoInit:=true
    end;

    fStream.Assign(Jpeg,JpegLen);
    jpeg_stdio_src(@dInfo, @fStream);

    dInfo.global_state:=DSTATE_START;

    if jpeg_read_header(@dInfo, True) > 0 then

    if dInfo.image_width > 0 then
    if dInfo.image_height > 0 then begin

      Fillchar(Info^,sizeof(Bitmap),0);

      Info.bmBitsPixel:=24;
      if dInfo.jpeg_color_space = JCS_GRAYSCALE then
      Info.bmBitsPixel:=8;

      Info.bmWidth:=dInfo.image_width;
      Info.bmHeight:=dInfo.image_height;

      Result:=true
    end
  end
end;

function tjpegObj.Decompress(dib: Pointer;
                              dib_w,dib_h,dib_bits: int;
                              Jpeg: Pointer; JpegLen: int;
                              Tables: Pointer): Boolean;
begin
  Result:=Decompress1(dib,dib_w,dib_h,dib_bits, 0,0,0,
                      Jpeg,JpegLen,Tables)
end;

function tjpegObj.Decompress1(dib: Pointer;
                              dib_w,dib_h,dib_bits: int;
                              posX,posY,zoom: int;
                              Jpeg: Pointer; JpegLen: int;
                              Tables: Pointer): Boolean;

procedure setScale(zoom: int);
var
  z: int;
begin
  z:=0;
  while z < 3 do begin
    if zoom <=1 then Break;
    Inc(z); zoom:=zoom div 2
  end;

  dInfo.scale_num:=1;
  dInfo.scale_denom:=1 shl z;
end;

var
  di,di1: PBytes;
  i,bx,dx,y,h,bits: int;
  _tables: tjpegTable_ptrs;
begin
  Result:=false;

  bits:=dib_bits;
  if bits in [8,24] then begin

    if not dInfoInit then begin
      dInfo.err:=@mErr;
      jpeg_create_decompress(@dInfo);
      jinit_memory_mgr(@dInfo);
      dInfoInit:=true
    end;

    fStream.Assign(Jpeg,JpegLen);
    jpeg_stdio_src(@dInfo, @fStream);

    dInfo.global_state:=DSTATE_START;

    if jpeg_read_header(@dInfo, True) > 0 then

    if dInfo.image_width > 0 then
    if dInfo.image_height > 0 then begin

      for i:=0 to NUM_QUANT_TBLS -1 do
      _tables.quant[i]:=dInfo.quant_tbl_ptrs[i];

      for i:=0 to NUM_HUFF_TBLS -1 do begin
        _tables.dc_huff[i]:=dInfo.dc_huff_tbl_ptrs[i];
        _tables.ac_huff[i]:=dInfo.ac_huff_tbl_ptrs[i];
      end;

      if Assigned(Tables) then
      back_tables(Tables);

      if bits = 24 then
        dInfo.out_color_space:=JCS_RGB
      else begin
        dInfo.out_color_space:=JCS_GRAYSCALE;
        dInfo.quantize_colors:=true;
        dInfo.desired_number_of_colors:=236;
      end;

      di1:=nil;
      if zoom = 0 then begin
        setScale(dInfo.image_width div dib_w);
        dInfo.output_width:=dib_w;
        dInfo.output_height:=dib_h;
      end
      else begin
        setScale(zoom);
        dInfo.output_width:=dInfo.image_width div zoom;
        dInfo.output_height:=dInfo.image_height div zoom;
        dInfo.output_height:=Min(dInfo.output_height,PosY+dib_h);
        fbuf.Size:=img_Line(dInfo.output_width,bits);
        di1:=fbuf.Memory; dx:=PosX*bits div 8;
      end;

      dInfo.out_color_components:=dib_bits div 8;

      if jpeg_start_decompress(@dInfo) then begin
        Result:=true;

        di:=dib;
        bx:=img_Line(dib_w,dib_Bits);

        h:=dInfo.output_height;

        if zoom = 0 then
          for y:=0 to h-1 do begin

            if dInfo.global_state = 0 then begin
              Result:=false; Break
            end;

            jpeg_read_scanlines(@dInfo, @di, 1);
            di:=@di[bx]
          end
        else
        if Assigned(di1) then
          for y:=0 to h-1 do begin

            if dInfo.global_state = 0 then begin
              Result:=false; Break
            end;

            jpeg_read_scanlines(@dInfo, @di1, 1);

            if y >=  posY then
            if dib_h > 0 then begin
              Move(di1[dx],di[0],bx);
              di:=@di[bx]; Dec(dib_h);
            end
          end;

        if dInfo.global_state <> 0 then
        jpeg_finish_decompress(@dInfo);
      end;

      for i:=0 to NUM_QUANT_TBLS-1 do
      dInfo.quant_tbl_ptrs[i]:=_tables.quant[i];

      for i:=0 to NUM_HUFF_TBLS-1 do begin
        dInfo.dc_huff_tbl_ptrs[i]:=_tables.dc_huff[i];
        dInfo.ac_huff_tbl_ptrs[i]:=_tables.ac_huff[i]
      end
    end
  end
end;

function tjpegObj.Decompressf(dib: Pointer;
                              dib_w,dib_h,dib_bits: int;
                              Jpeg: PChar): Boolean;
var
  vm: TReadfile;
begin
  Result:=false;

  vm:=TReadfile.Create;
  try
    if vm.Open(Jpeg) then
    if vm.Size >= 64 then
    if vm.Buf[0] = $FF then
    Result:=Decompress(dib,dib_w,dib_h,dib_bits,
                       vm.Buf,vm.Size,nil);
  finally
    vm.Free
  end
end;

const
  jpg_max_width  = 65536;
  jpg_max_height = 65536;

  jpg_Lines_Max = 16;

type
  tjpg_rows = array[0..jpg_Lines_Max] of pointer;

function tjpegObj.Compress(Dest: Pointer; DestSize: int;

                           Dib: Pointer;
                           Dib_w,Dib_h,Dib_bits: int;
                           Quality: int): int;
var
  rows: tjpg_rows;
  i,dx,Lines,LinesPerCall: int;
  si: PBytes;
begin
  Result:=0;

  if not (dib_bits in [8,24]) then
    Result:=-2
  else

  if (dib_w <= 0) or (dib_h <= 0) then
    Result:=-3
  else

  if (dib_w >= jpg_max_width)
  or (dib_h >= jpg_max_height) then
    Result:=-3
  else begin

    if not cInfoInit then begin
      FillChar(cInfo, SizeOf(cInfo), 0);

      cInfo.err:=@mErr;
      jpeg_create_compress(@cInfo);
      jinit_memory_mgr(@cInfo);

      cInfoInit:=true
    end;

    fStream.Clear;

    cInfo.client_data:=@fStream;
    cInfo.global_state:=CSTATE_START;

    cInfo.image_width:=dib_w;
    cInfo.image_height:=dib_h;

    cInfo.input_components:=Dib_bits div 8;

    if dib_bits = 8 then
      cInfo.in_color_space:=JCS_GRAYSCALE
    else
      cInfo.in_color_space:=JCS_RGB;

    jpeg_set_defaults(@cInfo);
    jpeg_set_quality(@cInfo, Quality, True);

    if cInfo.global_state = 0 then
      Result:=-4
    else begin
      jpeg_start_compress(@cInfo,True);
      try
        si:=dib;
        dx:=img_Line(dib_w,dib_bits);

        Lines:=dib_h;
        while Lines > 0 do begin
          LinesPerCall:=Lines;
          if LinesPerCall > jpg_Lines_Max then
          LinesPerCall:=jpg_Lines_Max;

          for i:=0 to LinesPerCall-1 do begin
            rows[i]:=si; si:=@si[dx]
          end;

          jpeg_write_scanlines(@cInfo, @rows, LinesPerCall);
          Dec(Lines,LinesPerCall)
        end;
      finally
        if cInfo.global_state <> 0 then begin
          jpeg_finish_compress(@cInfo);

          dx:=fStream.Position;
          if dx > 0 then
          if dx <= DestSize then begin
            fStream.Position:=0;
            fStream.Read(Dest^,dx);
            Result:=dx
          end
        end
      end
    end
  end
end;

function tjpegObj.Compressf(Dest: PChar;
                            const img: IImage;
                            Quality: int): int64;
var
  stm: TFileStream;
  rows: tjpg_rows;
  dib_w,dib_h,dib_bits: int;
  i,y,h,dx,Lines,LinesPerCall: int;
  si: PBytes;
begin
  Result:=0;

  dib_w:=img.GetWidth;
  dib_h:=img.GetHeight;
  dib_bits:=img.GetBits;

  if not (dib_bits in [8,24]) then
    Result:=-2
  else

  if (dib_w <= 0) or (dib_h <= 0) then
    Result:=-3
  else

  if (dib_w >= jpg_max_width)
  or (dib_h >= jpg_max_height) then
    Result:=-3
  else begin

    if not cInfoInit then begin
      FillChar(cInfo, SizeOf(cInfo), 0);

      cInfo.err:=@mErr;
      jpeg_create_compress(@cInfo);
      jinit_memory_mgr(@cInfo);

      cInfoInit:=true
    end;

    stm:=TFileStream.Create(Dest,fmCreate);
    try
      if stm.Handle = 0 then
        Result:=-5
      else begin
        cInfo.client_data:=@stm;
        cInfo.global_state:=CSTATE_START;

        cInfo.image_width:=dib_w;
        cInfo.image_height:=dib_h;

        cInfo.input_components:=Dib_bits div 8;

        if dib_bits = 8 then
          cInfo.in_color_space:=JCS_GRAYSCALE
        else
          cInfo.in_color_space:=JCS_RGB;

        jpeg_set_defaults(@cInfo);
        jpeg_set_quality(@cInfo, Quality, True);

        if cInfo.global_state = 0 then
          Result:=-4
        else begin
          jpeg_start_compress(@cInfo,True);
          try
            dx:=img_Line(dib_w,dib_bits);

            y:=0;
            while y < dib_h do begin

              h:=256;
              if y+h > dib_h then h:=dib_h-y;

              si:=img.GetRect(0,y,dib_w,h);
              if Assigned(si) then begin

                Lines:=h;
                while Lines > 0 do begin
                  LinesPerCall:=Lines;
                  if LinesPerCall > jpg_Lines_Max then
                  LinesPerCall:=jpg_Lines_Max;

                  for i:=0 to LinesPerCall-1 do begin
                    rows[i]:=si; si:=@si[dx]
                  end;

                  jpeg_write_scanlines(@cInfo, @rows, LinesPerCall);
                  Dec(Lines,LinesPerCall)
                end;

              end;

              Inc(y,h)
            end
          finally
            if cInfo.global_state <> 0 then begin
              jpeg_finish_compress(@cInfo);
              Result:=stm.Size
            end
          end
        end
      end
    finally
      stm.Free
    end
  end
end;

end.
