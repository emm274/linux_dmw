unit xcursors; interface

const
  _cCursor  = 1;
  _cLocator = 2;
  _cRegion  = 3;
  _cTarget  = 4;
  _pTarget  = 5;
  _vTarget  = 6;
  _xTarget  = 7;
  _xTrack   = 8;
  _xZoom    = 9;
  _xDrag    = 10;
  _zTarget  = 11;
  _xHand    = 12;
  _xLocator = 13;
  _wTarget  = 14;
  _cRotate  = 15;
  _cGround  = 16;
  _cTeta    = 17;
  _cRing    = 18;
  _cRing1   = 19;

implementation {$R DMX.RES}

uses
  LResources,Sysutils,
  LCLType,Graphics,Forms;

function LoadCursor(hInstance: THandle; lpCursorName: PChar): HCursor;
var
  Cur: TCursorImage;
begin
  Cur := TCursorImage.Create;
  try
    if PtrUInt(lpCursorName) > High(Word)
    then Cur.LoadFromResourceName(hInstance, lpCursorName)
    else Cur.LoadFromResourceID(hInstance, PtrInt(lpCursorName));
    Result := Cur.ReleaseHandle;
  finally
    Cur.Free;
  end;
end;

function xLoadCursor(Id: Integer): HCURSOR;
begin
  Result:=LoadCursor(hInstance,MakeIntResource(100+Id));
  Screen.Cursors[Id]:=Result
end;

procedure Init_Cursors;
begin
  xLoadCursor(_cCursor);
  xLoadCursor(_cLocator);
  xLoadCursor(_cRegion );
  xLoadCursor(_cTarget );
  xLoadCursor(_pTarget );
  xLoadCursor(_vTarget );
  xLoadCursor(_xTarget );
  xLoadCursor(_xTrack  );
  xLoadCursor(_xZoom   );
  xLoadCursor(_xDrag   );
  xLoadCursor(_zTarget );
  xLoadCursor(_xHand   );
  xLoadCursor(_xLocator);
  xLoadCursor(_wTarget );
  xLoadCursor(_cRotate );
  xLoadCursor(_cGround );
  xLoadCursor(_cTeta   );
  xLoadCursor(_cRing   );
  xLoadCursor(_cRing1  );
end;

begin
  Init_Cursors
end.