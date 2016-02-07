unit xevents; interface

uses
  Classes;

type
  TOnStr = function(Msg: PChar): PChar of object;
  TOnLFrame = procedure(Sender: TObject; const lt,rb: TPoint) of object;

  TProgressShow = procedure(Capt: PChar; Count: Integer) of object;
  TProgressContinue = function: Boolean of object;
  TProgressBreak = function: Boolean of object;

  PWaitRecord = ^TWaitRecord;
  TWaitRecord = record
    show: TProgressShow;
    stepIt: TProgressContinue;
    break: TProgressBreak;
    Pos,Count: Integer;
    kp: Double
  end;

implementation

end.