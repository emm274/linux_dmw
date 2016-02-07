unit xdlg; interface

uses
  otypes;

type
  IStgDlg = interface(IUnknown)
    ['{C1AAE3FA-139C-4545-A4F6-9E9AB904CECA}']
    procedure Message(Capt,Msg: PChar); stdcall;
    function Warning(Capt,Msg: PChar): bool; stdcall;
    function Answer(Capt,Msg: PChar): int; stdcall;
  end;

procedure xMessage(Capt,Msg: PChar);
function xWarning(Capt,Msg: PChar): bool;
function xAnswer(Capt,Msg: PChar): int;

implementation

var
  StgDlg: IStgDlg;

procedure xMessage(Capt,Msg: PChar);
begin
  if Assigned(StgDlg) then
  StgDlg.Message(Capt,Msg);
end;

function xWarning(Capt,Msg: PChar): bool;
begin
  Result:=false;
  if Assigned(StgDlg) then
  Result:=StgDlg.Warning(Capt,Msg)
end;

function xAnswer(Capt,Msg: PChar): int;
begin
  Result:=0;
  if Assigned(StgDlg) then
  Result:=StgDlg.Answer(Capt,Msg)
end;

begin
  StgDlg:=nil
end.