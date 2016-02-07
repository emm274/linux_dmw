unit xecw; interface

uses
  LCLType,
  otypes;

const
	NCSECW_READ_OK        = 0; // Successful read
	NCSECW_READ_FAILED    = 1; // Read failed due to an error
	NCSECW_READ_CANCELLED	= 2; // Read was cancelled,
                             // either because a new SetView arrived
                             // or a library shutdown is in progress

	ECW_CELL_UNITS_INVALID	=	0; // Invalid cell units
	ECW_CELL_UNITS_METERS	  =	1; // Cell units are standard meters
	ECW_CELL_UNITS_DEGREES	=	2; // Degrees
	ECW_CELL_UNITS_FEET		  =	3; // US Survey feet
	ECW_CELL_UNITS_UNKNOWN	=	4; // Unknown cell units

	NCS_FILE_UNKNOWN  =	0; 	// Unknown file type or no file open
	NCS_FILE_ECW	    =	1; 	// The file is an ECW
	NCS_FILE_JP2	    =	2;	// The file is a Jpeg 2000 File

	NCSCS_NONE 		  	= 0;  // No color space
	NCSCS_GREYSCALE	  = 1;	// Greyscale image
	NCSCS_YUV				  = 2;	// YUV - JPEG Digital, JP2 ICT
                          // Luminance-chrominance color space
	NCSCS_MULTIBAND	  = 3;	// Multi-band imagery
	NCSCS_sRGB			  = 4;	// sRGB color space
	NCSCS_YCbCr				= 5;	// YCbCr - JP2 ONLY, Auto-converted to sRGB
                        	// Modified luminance-chrominance color space

type
  TCallback = procedure; cdecl;

  TNCSFileView = Longint;

  PNCSFileViewFileInfo = ^TNCSFileViewFileInfo;
  TNCSFileViewFileInfo = record
    nSizeX: Cardinal;       // Dataset cells in X direction
    nSizeY: Cardinal;       // Dataset cells in Y direction
		nBands: Word;           // Number of bands in the file,
                            // e.g. 3 for a RGB file
	  nCompressionRate: Word; // Target compression rate,
                            // e,g, 20 == 20:1 compression.
                            // May be zero
	  eCellSizeUnits: Integer; 	// Units used for pixel size
  	fCellIncrementX: Double;  // Increment in eCellSizeUnits in X direction.
                            // May be negative.  Never zero
	  fCellIncrementY: Double;  // Increment in eCellSizeUnits in Y direction.
                            // May be negative.  Never zero
  	fOriginX: Double;         // World X origin for top left corner of top left cell,
                            // in eCellSizeUnits
	  fOriginY: Double;         // World Y origin for top left corner of top left cell,
                            // in eCellSizeUnits
    szDatum: PChar;           // ER Mapper style Datum name string,
                            // e.g. "RAW" or "NAD27".  Never NULL
	  szProjection: PChar;      // ER Mapper style Projection name string,
                            // e.g. "RAW" or "GEODETIC".  Never NULL
  end;

  PRawLines = ^TRawLines;
  TRawLines = Array[0..1023] of PBytes;

  TNCScbmOpenFileView = function(szUrlPath: PChar;
                                 out Handle: TNCSFileView;
                                 RefreshCallback: TCallBack): Integer; cdecl;

  TNCScbmCloseFileViewEx = function(Handle: TNCSFileView;
                                    bFreeCachedFile: Boolean): Integer; cdecl;

  TNCScbmGetViewFileInfo = function(Handle: TNCSFileView;
                                    out PNCSFileViewFileInfo): Integer; cdecl;

(*
 *	Sets the extents and band content of an open file view, and the output view size.  This function can be called at
 *	any time after a successful call to NCScbmOpenFileView.  In progressive mode, multiple calls to NCScbmSetFileView
 *	can be made, even if previous SetViews are still being processed, enhancing client interaction with the view.  After
 *	the call to NCScbmSetFileView, the band list array pBandList can be freed if so desired.  It is used only during the
 *  processing of the call, and not afterward.
 *
 *	@param[in]	pNCSFileView			The open file view to set
 *	@param[in]	pBandList				An array of integers specifying which bands of the image to read, and in which order
 *	@param[in]	nTopX					Left edge of the view in dataset cells
 *	@param[in]	nLeftY					Top edge of the view in dataset cells
 *	@param[in]	nBottomX				Right edge of the view in dataset cells
 *	@param[in]	nRightY					Bottom edge of the view in dataset cells
 *	@param[in]	nSizeX					Width of the view to be constructed from the image subset
 *	@param[in]	nSizeY					Height of the view to be constructed from the image subset
 *)

  TNCScbmSetFileView = function(Handle: TNCSFileView;
           										  nBands: Cardinal; pBandList: PIntegers;
              					 			  nTopX,nLeftY,nBottomX,nRightY: Cardinal;
                                nSizeX,nSizeY: Cardinal): Integer; cdecl;

(*
 *	Read line by line in BIL format.
 *
 *	@param[in]	pNCSFileView			The open file view from which to read view lines
 *	@param[out]	ppOutputLine			The buffer into which to read the interleaved band information
 *)
  TNCScbmReadViewLineBIL = function(Handle: TNCSFileView;
                                    Lines: PRawLines): Integer; cdecl;

(*
 *	Read line by line in RGB format.
 *
 *	@param[in]	pNCSFileView			The open file view from which to read view lines
 *	@param[out]	pRGBTriplets			The buffer into which to read the red-green-blue sample triplets.
 *)
  TNCScbmReadViewLineRGB = function(Handle: TNCSFileView; buf: PBytes): Integer; cdecl;

  TNCScbmReadViewLineRGBA = function(Handle: TNCSFileView; buf: PBytes): Integer; cdecl;

  TFileOpenCB = function(szFileName: PChar; out pClient: Pointer): Integer; cdecl;
  TFileCloseCB = function(pClient: Pointer): Integer; cdecl;
  TFileReadCB = function(pClient: Pointer; OutBuf: PBytes; nLength: Integer): Integer; cdecl;
  TFileSeekCB = function(pClient: Pointer; nOffset: Int64): Integer; cdecl;
  TFileTellCB = function(pClient: Pointer; out pOffset: Int64): Integer; cdecl;

  TNCSecwSetIOCallbacks = function(pOpenCB: TFileOpenCB;
                          				 pCloseCB: TFileCloseCB;
                                   pReadCB: TFileReadCB;
                                   pSeekCB: TFileSeekCB;
                                   pTellCB: TFileTellCB): Integer cdecl;

const
  MAX_PATH      = 259;
  MAX_DATUM_LEN = 15;
  MAX_PROJ_LEN  = 15;

	COMPRESS_NONE		= NCSCS_NONE;
	COMPRESS_UINT8 	= NCSCS_GREYSCALE;  // Greyscale format
	COMPRESS_YUV		= NCSCS_YUV;        // JPEG standard YUV digital format
	COMPRESS_MULTI 	= NCSCS_MULTIBAND;  // Multiband format
	COMPRESS_RGB		= NCSCS_sRGB;       // RGB images

	COMPRESS_HINT_NONE	   = 0;
	COMPRESS_HINT_FAST	   = 1; // Do the fastest compression possible
	COMPRESS_HINT_BEST	   = 2; // Try to achieve the maximum possible
                              // compression ratio
	COMPRESS_HINT_INTERNET = 3; // Optimise compression process for
                              // later Internet use of the compressed file

type
  TPathStr = Array[0..MAX_PATH] of Char;

  TDatumStr = Array[0..MAX_DATUM_LEN] of Char;
  TProjStr = Array[0..MAX_PROJ_LEN] of Char;

  PNCSEcwCompressClient = ^TNCSEcwCompressClient;

  POutputBandBufferArray = ^TOutputBandBufferArray;
  TOutputBandBufferArray = Array[0..7] of PFloats;

  TReadCallback = function(pClient: PNCSEcwCompressClient; nNextLine: Cardinal;
                           Lines: POutputBandBufferArray): Boolean; cdecl;

	TStatusCallback = procedure(pClient: PNCSEcwCompressClient;
										          nCurrentLine: Cardinal); cdecl;

	TCancelCallback = function(pClient: PNCSEcwCompressClient): Boolean; cdecl;

  TNCSEcwCompressClient = record
    szInputFilename:  TPathStr;
	  szOutputFilename: TPathStr;

	  fTargetCompression: Single; // The target compression ratio - must be specified
	  eCompressFormat: Integer;   //	The compression format to use.

  	eCompressHint: Integer;     // A guideline for an appropriate compression scheme to use.
                                // This currently has	no effect,  though the default
                                // value is COMPRESS_HINT_INTERNET.  Reserved for
                                // future use, see the related enumerated type definition

	  nBlockSizeX: Cardinal;      // X dimension of the block size to use.
    nBlockSizeY: Cardinal;      // Y dimension of the block size to use.
                                // Can be 64, 128, 256, 512, 1024, or 2048.
                                // The default for these is set to 64
                                // which produces preferred performance
                                // over the internet.

	  nInOutSizeX: Cardinal;      // Number of cells of input data in the X direction
	  nInOutSizeY: Cardinal;      // Number of cells of input data in the Y direction
	  nInputBands: Cardinal;      // Number of bands in the input data
	  nOutputBands: Cardinal;     // Number of bands in the output file

    unk1: longint;

	  nInputSize: Int64;          // Size of the input file in bytes

	  fCellIncrementX: Double;    // Optional field specifying the cell size
    fCellIncrementY: Double;    // in the X,Y direction in eCellSizeUnits

	  fOriginX: Double;           // Optional fields specifying the X,Y world origin
    fOriginY: Double;           // of the input data in eCellSizeUnits

	  eCellSizeUnits: Integer;    // Optional field specifying the units in which world cell
                                // sizes are specified, e.g. meters, feet

	  szDatum: TDatumStr;         // ER Mapper GDT style datum string
    szProj: TProjStr;           // ER Mapper GDT style projection string

    pReadCallback: TReadCallback;
    pStatusCallback: TStatusCallback;
    pCancelCallback: TCancelCallback;
	  pClientData: Pointer;

    pTask: Pointer;             // Created by NCSEcwCompressOpen

                              	// The remaining fields are populated by
                                // NCSEcwCompressClose
    fActualCompression: Single; // Actual compression rate achieved -
                                // ratio of input data size to output file size
    unk2: Longint;
	  fCompressionTime: Double;   // Time taken to perform the complete compression,
                                // in seconds
	  fCompressionMBSec: Double;  // MB/s throughput during the compression process

  	nOutputSize: Int64;         // Total size of the output file in bytes
  end;

  TNCSEcwCompressAllocClient = function: PNCSEcwCompressClient; cdecl;

  TNCSEcwCompressFreeClient = function(pInfo: PNCSEcwCompressClient): Integer; cdecl;

  TNCSEcwCompressOpen = function(pInfo: PNCSEcwCompressClient;
                                 bCalculateSizesOnly: Boolean): Integer; cdecl;

  TNCSEcwCompressClose = function(pInfo: PNCSEcwCompressClient): Integer; cdecl;

  TNCSEcwCompress = function(pInfo: PNCSEcwCompressClient): Integer; cdecl;

  tecw = class
    destructor destroy; override;
    function begin_compress(Dest: PChar): Boolean;

    function Compress(InBuf: PBytes;
                      nSizeX,nSizeY,nBands: Integer;
                      OutBuf: Pointer; OutBytes: Integer): Integer;

  private
    fTemp: TShortstr;
  end;

var
  NCScbmOpenFileView: TNCScbmOpenFileView;
  NCScbmCloseFileViewEx: TNCScbmCloseFileViewEx;
  NCScbmGetViewFileInfo: TNCScbmGetViewFileInfo;
  NCScbmSetFileView: TNCScbmSetFileView;
  NCScbmReadViewLineBIL: TNCScbmReadViewLineBIL;
  NCScbmReadViewLineRGB: TNCScbmReadViewLineRGB;
  NCScbmReadViewLineRGBA: TNCScbmReadViewLineRGBA;
  NCSecwSetIOCallbacks: TNCSecwSetIOCallbacks;

  NCSEcwCompressAllocClient: TNCSEcwCompressAllocClient;
  NCSEcwCompressFreeClient: TNCSEcwCompressFreeClient;
  NCSEcwCompressOpen: TNCSEcwCompressOpen;
  NCSEcwCompressClose: TNCSEcwCompressClose;
  NCSEcwCompress: TNCSEcwCompress;

function ecw_Init: Boolean;
function ecwc_Init: Boolean;

function ecwBitmapInfo(NCSFileView: TNCSFileView;
                       Info: PBitmapInfoHeader): Boolean;

function ecwDecompress(OutBuf: PBytes;
                       nSizeX,nSizeY,nBands: Integer;
                       InBuf: Pointer; InBytes: Integer): Boolean;

implementation

uses
  Sysutils,dynlibs,
  ofiles,img_x;

var
  ecw_lib: THandle;
  ecwc_lib: THandle;

function ecw_Init: Boolean;
begin
  Result:=false;

  if ecw_lib < 32 then begin
    ecw_lib:=LoadLibrary('NCSEcw.dll');
    if ecw_lib >= 32 then begin
      NCScbmOpenFileView:=GetProcAddress(ecw_lib,'NCScbmOpenFileView');
      NCScbmCloseFileViewEx:=GetProcAddress(ecw_lib,'NCScbmCloseFileViewEx');
      NCScbmGetViewFileInfo:=GetProcAddress(ecw_lib,'NCScbmGetViewFileInfo');
      NCScbmSetFileView:=GetProcAddress(ecw_lib,'NCScbmSetFileView');
      NCScbmReadViewLineBIL:=GetProcAddress(ecw_lib,'NCScbmReadViewLineBIL');
      NCScbmReadViewLineRGB:=GetProcAddress(ecw_lib,'NCScbmReadViewLineRGB');
      NCScbmReadViewLineRGBA:=GetProcAddress(ecw_lib,'NCScbmReadViewLineRGBA');
      NCSecwSetIOCallbacks:=GetProcAddress(ecw_lib,'NCSecwSetIOCallbacks');

      if Assigned(NCScbmOpenFileView) then
      if Assigned(NCScbmCloseFileViewEx) then
      if Assigned(NCScbmGetViewFileInfo) then
      if Assigned(NCScbmSetFileView) then
      if Assigned(NCScbmReadViewLineBIL) then
      if Assigned(NCScbmReadViewLineRGB) then
      if Assigned(NCScbmReadViewLineRGBA) then
      if Assigned(NCSecwSetIOCallbacks) then
      Result:=true;

      if not Result then begin
        FreeLibrary(ecw_lib); ecw_lib:=0
      end
    end
  end;

  Result:=ecw_lib >= 32
end;

function ecwc_Init: Boolean;
begin
  Result:=false;

  if ecw_init then
  if ecwc_lib < 32 then begin
    ecwc_lib:=LoadLibrary('NCSEcwC.dll');
    if ecwc_lib >= 32 then begin
      NCSEcwCompressAllocClient:=GetProcAddress(ecwc_lib,'NCSEcwCompressAllocClient');
      NCSEcwCompressFreeClient:=GetProcAddress(ecwc_lib,'NCSEcwCompressFreeClient');
      NCSEcwCompressOpen:=GetProcAddress(ecwc_lib,'NCSEcwCompressOpen');
      NCSEcwCompressClose:=GetProcAddress(ecwc_lib,'NCSEcwCompressClose');
      NCSEcwCompress:=GetProcAddress(ecwc_lib,'NCSEcwCompress');

      if Assigned(NCSEcwCompressAllocClient) then
      if Assigned(NCSEcwCompressFreeClient) then
      if Assigned(NCSEcwCompressOpen) then
      if Assigned(NCSEcwCompressClose) then
      if Assigned(NCSEcwCompress) then
      Result:=true;

      if not Result then begin
        FreeLibrary(ecwc_lib); ecwc_lib:=0
      end
    end
  end;

  Result:=ecwc_lib >= 32
end;

function ecwBitmapInfo(NCSFileView: TNCSFileView;
                       Info: PBitmapInfoHeader): Boolean;
var
  infp: PNCSFileViewFileInfo;
begin
  Result:=false;

  if NCScbmGetViewFileInfo(NCSFileView,infp) = 0 then
  if infp.nBands in [1,3] then begin
    Info.biSize:=Sizeof(TBitmapInfoHeader);
    Info.biWidth:=infp.nSizeX;
    INfo.biHeight:=infp.nSizeY;
    Info.biPlanes:=1;
    Info.biBitCount:=infp.nBands*8;
    Info.biCompression:=bi_RGB;
    Result:=true
  end
end;

type
  PReadImage = ^TReadImage;
  TReadImage = record
    Buffer: PBytes;
    Width,Height,Bands: Integer;
    LineBytes: Integer;
  end;

function ecw_ReadCallback(pClient: PNCSEcwCompressClient; nNextLine: Cardinal;
                          Lines: POutputBandBufferArray): Boolean; cdecl;
var
  Image: PReadImage;

  i,j: Integer;
  si: PBytes; di1,di2,di3: PFloats;
begin
  Result:=true;

  Image:=pClient.pClientData;

  if nNextLine >= 0 then
  if nNextLine < Image.Height then begin

    si:=@Image.Buffer[nNextLine * Image.LineBytes];

    di1:=Lines[0]; di2:=nil; di3:=nil;
    if pClient.nInputBands = 3 then begin
      di2:=Lines[1]; di3:=Lines[2];
    end;

    for i:=1 to pClient.nInOutSizeX do begin
      di1[0]:=si[0]; di1:=@di1[1]; si:=@si[1];
      if Assigned(di2) then begin
        di2[0]:=si[0]; di2:=@di2[1]; si:=@si[1];
        di3[0]:=si[0]; di3:=@di3[1]; si:=@si[1];
      end
    end
  end
end;

destructor tecw.Destroy;
begin
  if Strlen(fTemp) > 0 then
  FileErase(fTemp); inherited
end;

function tecw.begin_compress(Dest: PChar): Boolean;
var
  buf: longint;
begin
  Result:=false; if ecwc_init then begin
    StrUpdateExt(fTemp,Dest,'.ecw'); buf:=$0f0f0f0f;
    Result:=magic_FileWrite(fTemp,'temp',buf,4);
  end
end;

function tecw.Compress(InBuf: PBytes;
                       nSizeX,nSizeY,nBands: Integer;
                       OutBuf: Pointer; OutBytes: Integer): Integer;
var
  ReadImage: TReadImage;
  comp: PNCSEcwCompressClient;
  vm: TReadFile;
begin
  Result:=0;

  ReadImage.Buffer:=InBuf;
  ReadImage.Width:=nSizeX;
  ReadImage.Height:=nSizeY;
  ReadImage.Bands:=nBands;
  ReadImage.LineBytes:=img_Line(nSizeX,nBands*8);

  comp:=NCSEcwCompressAllocClient;
  if Assigned(comp) then begin

    StrCopy(comp.szOutputFilename,fTemp);

    comp.fTargetCompression:=10;

    comp.nInOutSizeX:=nSizeX;
    comp.nInOutSizeY:=nSizeY;
    comp.nInputBands:=nBands;

    if nBands = 1 then
      comp.eCompressFormat:=COMPRESS_UINT8
    else
      comp.eCompressFormat:=COMPRESS_RGB;

    comp.pReadCallback:=ecw_ReadCallback;
    comp.pClientData:=@ReadImage;

    if NCSEcwCompressOpen(comp,FALSE) = 0 then begin
      NCSEcwCompress(comp);
      NCSEcwCompressClose(comp)
    end;

    NCSEcwCompressFreeClient(comp);

    vm:=TReadFile.Create;
    try
      if vm.Open(fTemp) then
      if vm.Size <= OutBytes then begin
        Move(vm.Buf^,OutBuf^,vm.Size);
        Result:=vm.Size
      end
    finally
      vm.Free
    end

  end
end;

type
  PECWTileRead = ^TECWTileRead;
  TECWTileRead = record
    InBuf: PBytes;
    InSize: Integer;
    Position: Integer;
  end;

var
  ECWTileRead: TECWTileRead;

// Open File CB - open the file, fill in MyDataStruct
function FileOpenCB(szFileName: PChar; out pClient: PECWTileRead): Integer; cdecl;
begin
  Result:=0; pClient:=@ECWTileRead
end;

// Close File CB - close file, free MyDataStruct
function FileCloseCB(pClient: PECWTileRead): Integer; cdecl;
begin
  Result:=0
end;

// Read File CB - read given length from current offset into buffer
function FileReadCB(pClient: PECWTileRead; OutBuf: PBytes; nLength: Integer): Integer; cdecl;
begin
	Result:=1;

  with pClient^ do
  if Position + nLength <= InSize then begin
    Move(InBuf[Position],OutBuf^,nLength);
    Inc(Position,nLength); Result:=0
  end
end;

// Seek File CB - seek file to given offset
function FileSeekCB(pClient: PECWTileRead; nOffset: Int64): Integer; cdecl;
begin
	Result:=1;

  if nOffset >= 0 then
  if nOffset <= pClient.InSize then begin
    pClient.Position:=nOffset; Result:=0
  end
end;

// Tell File CB - get the current file offset
function FileTellCB(pClient: PECWTileRead; out pOffset: Int64): Integer; cdecl;
begin
  pOffset:=pClient.Position;
  Result:=0
end;

function ecwDecompress(OutBuf: PBytes;
                       nSizeX,nSizeY,nBands: Integer;
                       InBuf: Pointer; InBytes: Integer): Boolean;
var
  h: TNCSFileView;
  infp: PNCSFileViewFileInfo;
  bands: Array[0..2] of Integer;
  di: PBytes; y,bx: Integer;
begin
  Result:=false;

  ECWTileRead.InBuf:=InBuf;
  ECWTileRead.InSize:=InBytes;
  ECWTileRead.Position:=0;

	NCSecwSetIOCallbacks(@FileOpenCB,@FileCloseCB,
                       @FileReadCB,@FileSeekCB,@FileTellCB);

  if NCScbmOpenFileView('tile',h,nil) = 0 then begin

    if NCScbmGetViewFileInfo(h,infp) = 0 then
    if infp.nSizeX = nSizeX then
    if infp.nSizeY = nSizeY then
    if infp.nBands = nBands then begin

      bands[0]:=0; bands[1]:=1; bands[2]:=2;

      if NCScbmSetFileView(h,nBands,@bands,
                           0,0,nSizeX-1,nSizeY-1,
                           nSizeX,nSizeY) = 0 then begin

        di:=OutBuf;
        bx:=img_Line(nSizeX,nBands*8);
        for y:=1 to nSizeY do begin

          if nBands = 1 then
        		NCScbmReadViewLineBIL(h,@di)
          else
            NCScbmReadViewLineRGB(h,di);

          di:=@di[bx]
        end;

        Result:=true
      end
    end;

    NCScbmCloseFileViewEx(h,true)
  end
end;

initialization
begin
  ecw_lib:=0;
  ecwc_lib:=0;
end;

finalization
begin
  if ecw_lib >= 32 then
  FreeLibrary(ecw_lib);

  if ecwc_lib >= 32 then
  FreeLibrary(ecwc_lib);
end;

end.
