unit xwin;

{$mode delphi}

interface

uses
  Classes, SysUtils, LCLType;

procedure Cls_MouseWheel(Wnd: hWnd);

implementation

uses
  LCLIntf,LMessages,LCLProc,Forms;

procedure Cls_MouseWheel(Wnd: hWnd);
var
  Msg: TMsg;
begin
  if GetActiveWindow = Wnd then
  while PeekMessage(Msg, 0,0,0, pm_Remove) do
  if Msg.message <> LM_MOUSEWHEEL then begin
    Application.ProcessMessages; Break
  end
end;

end.

