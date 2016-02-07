program tst2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  OTypes, convert, xpoly;

type

  { TTstConsole1 }

  TTstConsole1 = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TTstConsole1 }

procedure TTstConsole1.DoRun;

procedure test_otypes;
var
  iv: TIntegers;
  ib: TBytes;
  ax,bx,cx: long;
  f: single;
begin
  ax:=Long_Words(1,2);
  bx:=ax and $FFFF;
  cx:=ax shr 16;

  ax:=SwapInt(ax);
  bx:=ax and $FFFF;
  cx:=ax shr 16;

  f:=123;
  move(f,ax,4);
  ax:=SwapInt(ax);
  move(ax,f,4);
  f:=SwapSingle(f);

  ax:=SwapWord($0102);
  bx:=ax and $FF;
  cx:=ax shr 8;

  SetInts(@iv,1024,123);
  SwapInts(@iv,1024);
end;

var
  rc: int; ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg <> '' then
    ShowException(Exception.Create(ErrorMsg))
  else
  // parse parameters
  if HasOption('h','help') then
    WriteHelp
  else begin
    write('memManager...');

    rc:=VerifyMM;
    if rc = 0 then writeln('OK')
              else writeln(rc);

    test_otypes;
  end;

  readln;
  Terminate;
end;

constructor TTstConsole1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTstConsole1.Destroy;
begin
  inherited Destroy;
end;

procedure TTstConsole1.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TTstConsole1;
begin
  Application:=TTstConsole1.Create(nil);
  Application.Title:='Tst console1';
  Application.Run;
  Application.Free;
end.

