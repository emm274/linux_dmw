unit xdibx; interface

{$mode delphi}

uses
  Classes, xdib;

function CreateDib: TDib;

implementation

uses
  xdibcv,xdib32;

function CreateDib: TDib;
begin
  if true then
    Result:=XBitmap32.Create
  else
    Result:=XBitmap.Create;
end;

end.

