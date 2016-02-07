unit xapp; interface

uses
  otypes;

procedure dll_DumpIcon(Name: PChar);
procedure dll_LoadIcon(Name: PChar);

implementation

uses
  Forms,errors,
  SysUtils,ofiles;

procedure dll_DumpIcon(Name: PChar);
var
  fn: TShortstr;
begin
  if not IsLibrary then
  if Assigned(Application) then
  if Application.Icon.Handle <> 0 then begin

    if Assigned(Name) then
      StrBinPath(fn,Name)
    else
      StrWorkPath(fn,'dll.ico');

    try
      Application.Icon.SaveToFile(fn)
    except
      on EInOutError do;
    end;
  end
end;

procedure dll_LoadIcon(Name: PChar);
var
  fn: TShortstr;
begin
  if Assigned(Name) then
    StrBinPath(fn,Name)
  else
    StrWorkPath(fn,'dll.ico');

  if FileExist(fn) then
  if xFileSize(fn) > 0 then

  try
    Application.Icon.LoadFromFile(fn)
  except
    on EInOutError do;
  end
end;

end.