unit Windows; interface

uses
  types;

const
  STG_E_FILENOTFOUND = HRESULT($80030002);

type
  PUCHAR = ^UCHAR;
  UCHAR  = BYTE;

  TSize = types.TSize;
  TFileTime = types.TFileTime;

implementation

end.

