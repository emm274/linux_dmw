unit xkeys; interface

uses
  Classes,LCLType,otypes;
  
type
  TTickCount = class
    function Now: int64;
    function MSeconds(t: int64): int;

    function Start: int64;
    function Stop: int;   // MSeconds

  private
    ftick: int64;
  end;

function msg_KeyUp(const Msg: TMsg; Key: Word): Boolean;

function xEscape(Wnd: hWnd; Skip: Word): Boolean;

procedure Cls_KeyDown(Wnd: hWnd; Key: Word);
procedure Cls_MouseWheel(Wnd: hWnd);

function key_Pressed(Key: Word): Boolean;

function Shift_Pressed: boolean;
function Ctrl_Pressed: boolean;
function Tab_Pressed: boolean;

function Plus_Pressed(Key: Word): Boolean;
function Minus_Pressed(Key: Word): Boolean;
function Number_Pressed(Key: Word): Boolean;

function alt_Char(Key: Word; Shift: TShiftState;
                  Ch: Char): Boolean;

function ctrl_Char(Key: Word; Shift: TShiftState;
                   Ch: Char): Boolean;

function Shift_Char(Key: Word; Shift: TShiftState;
                    Ch: Char): Boolean;

function only_Char(Key: Word; Shift: TShiftState;
                   Ch: Char): Boolean;

function alt_Key(Key: Word; Shift: TShiftState;
                 vk: Word): Boolean;

function ctrl_Key(Key: Word; Shift: TShiftState;
                  vk: Word): Boolean;

function Shift_Key(Key: Word; Shift: TShiftState;
                   vk: Word): Boolean;

function only_Key(Key: Word; Shift: TShiftState;
                  vk: Word): Boolean;

function only_Number(Key: Word; Shift: TShiftState): Boolean;

implementation

uses
  LMessages,
  Sysutils, unix,
  LCLIntf;

var
  st_Shift,st_Control,st_Tab: int;

function TTickCount.Now: int64;
begin
  Result:=GetTickCount64
end;

function TTickCount.MSeconds(t: int64): int;
begin
  Result:=(Now-t) div 1000000
end;

function TTickCount.Start: int64;
begin
  ftick:=Now; Result:=ftick
end;

function TTickCount.Stop: int;
begin
  Result:=MSeconds(ftick)
end;

function msg_KeyUp(const Msg: TMsg; Key: Word): Boolean;
begin
  Result:=false; if (Msg.Message = LM_KeyDown) or
  (Msg.Message = LM_KeyUp) or (Msg.Message = LM_Char) then
  if Msg.wParam = Key then Result:=true
end;

function GetAsyncKeyState(Key: int): int;
begin
  Result:=GetKeyState(Key)
end;

procedure BackMessage(const Msg: TMsg);
begin
  PostMessage(Msg.hwnd,
              Msg.message,
              Msg.wParam,
              Msg.lParam)
end;

function xEscape(Wnd: hWnd; Skip: Word): Boolean;
var
  Msg: TMsg;
begin
  Result:=false;
  if GetAsyncKeyState(vk_Escape) < 0 then begin

    while PeekMessage(Msg, 0,0,0, pm_Remove) do
    if msg_KeyUp(Msg,vk_Escape) then else
    if (Skip <> 0) and msg_KeyUp(Msg,Skip) then
    else begin
      BackMessage(Msg); Break
    end;

    Result:=true
  end

  else Cls_KeyDown(Wnd,Skip)
end;

procedure Cls_KeyDown(Wnd: hWnd; Key: Word);
var
  Msg: TMsg;
begin
  if Key > 0 then

  if GetActiveWindow = Wnd then
  if GetAsyncKeyState(Key) < 0 then

  while PeekMessage(Msg, 0,0,0, pm_Remove) do
  if not msg_KeyUp(Msg,Key) then begin
    BackMessage(Msg);
    Break
  end
end;

procedure Cls_MouseWheel(Wnd: hWnd);
var
  Msg: TMsg;
begin
  if GetActiveWindow = Wnd then
  while PeekMessage(Msg, 0,0,0, pm_Remove) do
  if Msg.message <> LM_MOUSEWHEEL then begin
    BackMessage(Msg); Break
  end
end;

procedure Reset_Pressed;
begin
  st_Shift:=GetKeyState(vk_Shift) and 1;
  st_Control:=GetKeyState(vk_Control) and 1;
  st_Tab:=GetKeyState(vk_Tab) and 1;
end;

function key_Pressed(Key: Word): Boolean;
begin
  Result:=GetKeyState(Key) < 0
end;

function Shift_Pressed: boolean;
var
  st: integer;
begin
  Result:=false; st:=GetKeyState(vk_Shift);
  if (st >= 0) and (st <> st_Shift) then
  begin st_Shift:=st; Result:=true end
end;

function Ctrl_Pressed: boolean;
var
  st: integer;
begin
  Result:=false; st:=GetKeyState(vk_Control);
  if (st >= 0) and (st <> st_Control) then
  begin st_Control:=st; Result:=true end
end;

function Tab_Pressed: boolean;
var
  st: integer;
begin
  Result:=false; st:=GetKeyState(vk_Tab);
  if (st >= 0) and (st <> st_Tab) then
  begin st_Tab:=st; Result:=true end
end;

function Plus_Pressed(Key: Word): Boolean;
begin
  Result:=Key in [ord('k'),187]
end;

function Minus_Pressed(Key: Word): Boolean;
begin
  Result:=Key in [ord('m'),189]
end;

function Number_Pressed(Key: Word): Boolean;
begin
  Result:=Key in [ord('0')..ord('9')]
end;

function alt_Char(Key: Word; Shift: TShiftState;
                  Ch: Char): Boolean;
begin
  Result:=false;
  if ssAlt in Shift then
  Result:=Key = ord(Ch)
end;

function ctrl_Char(Key: Word; Shift: TShiftState;
                   Ch: Char): Boolean;
begin
  Result:=false;
  if ssCtrl in Shift then
  Result:=Key = ord(Ch)
end;

function Shift_Char(Key: Word; Shift: TShiftState;
                    Ch: Char): Boolean;
begin
  Result:=false;
  if ssShift in Shift then
  Result:=Key = ord(Ch)
end;

function only_Char(Key: Word; Shift: TShiftState;
                   Ch: Char): Boolean;
begin
  Result:=false;
  if Shift = [] then
  Result:=Key = ord(Ch)
end;

function alt_Key(Key: Word; Shift: TShiftState;
                 vk: Word): Boolean;
begin
  Result:=false;
  if ssAlt in Shift then
  Result:=Key = vk
end;

function ctrl_Key(Key: Word; Shift: TShiftState;
                  vk: Word): Boolean;
begin
  Result:=false;
  if ssCtrl in Shift then
  Result:=Key = vk
end;

function Shift_Key(Key: Word; Shift: TShiftState;
                   vk: Word): Boolean;
begin
  Result:=false;
  if ssShift in Shift then
  Result:=Key = vk
end;

function only_Key(Key: Word; Shift: TShiftState;
                  vk: Word): Boolean;
begin
  Result:=false;
  if Shift = [] then
  Result:=Key = vk
end;

function only_Number(Key: Word; Shift: TShiftState): Boolean;
begin
  Result:=false; if Shift = [] then
  Result:=Key in [ord('0')..ord('9')]
end;

begin
  st_Shift:=0; st_Control:=0; st_Tab:=0
end.