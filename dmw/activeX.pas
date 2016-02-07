unit activeX; interface

uses
  types,Windows;

const
  {$EXTERNALSYM STGM_READ}
  STGM_READ             = $00000000;
  {$EXTERNALSYM STGM_WRITE}
  STGM_WRITE            = $00000001;
  {$EXTERNALSYM STGM_READWRITE}
  STGM_READWRITE        = $00000002;

  {$EXTERNALSYM STGM_SHARE_DENY_NONE}
  STGM_SHARE_DENY_NONE  = $00000040;
  {$EXTERNALSYM STGM_SHARE_DENY_READ}
  STGM_SHARE_DENY_READ  = $00000030;
  {$EXTERNALSYM STGM_SHARE_DENY_WRITE}
  STGM_SHARE_DENY_WRITE = $00000020;
  {$EXTERNALSYM STGM_SHARE_EXCLUSIVE}
  STGM_SHARE_EXCLUSIVE  = $00000010;

  {$EXTERNALSYM STGM_PRIORITY}
  STGM_PRIORITY         = $00040000;
  {$EXTERNALSYM STGM_DELETEONRELEASE}
  STGM_DELETEONRELEASE  = $04000000;
  {$EXTERNALSYM STGM_NOSCRATCH}
  STGM_NOSCRATCH        = $00100000;

  {$EXTERNALSYM STGM_CREATE}
  STGM_CREATE           = $00001000;
  {$EXTERNALSYM STGM_CONVERT}
  STGM_CONVERT          = $00020000;
  {$EXTERNALSYM STGM_FAILIFTHERE}
  STGM_FAILIFTHERE      = $00000000;

  {$EXTERNALSYM STGTY_STORAGE}
  STGTY_STORAGE   = 1;
  {$EXTERNALSYM STGTY_STREAM}
  STGTY_STREAM    = 2;
  {$EXTERNALSYM STGTY_LOCKBYTES}
  STGTY_LOCKBYTES = 3;
  {$EXTERNALSYM STGTY_PROPERTY}
  STGTY_PROPERTY  = 4;

  {$EXTERNALSYM STGMOVE_MOVE}
  STGMOVE_MOVE        = 0;
  {$EXTERNALSYM STGMOVE_COPY}
  STGMOVE_COPY        = 1;
  {$EXTERNALSYM STGMOVE_SHALLOWCOPY}
  STGMOVE_SHALLOWCOPY = 2;

  {$EXTERNALSYM STREAM_SEEK_SET}
  STREAM_SEEK_SET = 0;
  {$EXTERNALSYM STREAM_SEEK_CUR}
  STREAM_SEEK_CUR = 1;
  {$EXTERNALSYM STREAM_SEEK_END}
  STREAM_SEEK_END = 2;

type
  POleStr = types.POleStr;

  LargeInt = types.Largeint;

  PStatStg = types.PStatStg;
  TStatStg = types.TStatStg;

  TCLSID = types.TCLSID;

  ISequentialStream = types.ISequentialStream;
  IStream = types.IStream;

{ IEnumStatStg interface }

  IEnumStatStg = interface(IUnknown)
    ['{0000000D-0000-0000-C000-000000000046}']
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumStatStg): HResult; stdcall;
  end;

{ IStorage interface }

  TSNB = ^POleStr;

  IStorage = interface(IUnknown)
    ['{0000000B-0000-0000-C000-000000000046}']
    function CreateStream(pwcsName: POleStr; grfMode: Longint; reserved1: Longint;
      reserved2: Longint; out stm: IStream): HResult; stdcall;
    function OpenStream(pwcsName: POleStr; reserved1: Pointer; grfMode: Longint;
      reserved2: Longint; out stm: IStream): HResult; stdcall;
    function CreateStorage(pwcsName: POleStr; grfMode: Longint;
      dwStgFmt: Longint; reserved2: Longint; out stg: IStorage): HResult;
      stdcall;
    function OpenStorage(pwcsName: POleStr; const stgPriority: IStorage;
      grfMode: Longint; snbExclude: TSNB; reserved: Longint;
      out stg: IStorage): HResult; stdcall;
    function CopyTo(ciidExclude: Longint; rgiidExclude: PGUID;
      snbExclude: TSNB; const stgDest: IStorage): HResult; stdcall;
    function MoveElementTo(pwcsName: POleStr; const stgDest: IStorage;
      pwcsNewName: POleStr; grfFlags: Longint): HResult; stdcall;
    function Commit(grfCommitFlags: Longint): HResult; stdcall;
    function Revert: HResult; stdcall;
    function EnumElements(reserved1: Longint; reserved2: Pointer; reserved3: Longint;
      out enm: IEnumStatStg): HResult; stdcall;
    function DestroyElement(pwcsName: POleStr): HResult; stdcall;
    function RenameElement(pwcsOldName: POleStr;
      pwcsNewName: POleStr): HResult; stdcall;
    function SetElementTimes(pwcsName: POleStr; const ctime: TFileTime;
      const atime: TFileTime; const mtime: TFileTime): HResult;
      stdcall;
    function SetClass(const clsid: TCLSID): HResult; stdcall;
    function SetStateBits(grfStateBits: Longint; grfMask: Longint): HResult;
      stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
      stdcall;
  end;


implementation

end.

