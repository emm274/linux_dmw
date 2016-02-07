unit idmw; interface

uses
  OTypes,LCLType;

type
  idmwProject = interface(IUnknown)
    ['{DAFF9202-E291-46DF-818B-0F3F62F67267}']

    // �������� ������
    procedure Clear; stdcall;

    // ��������� ������ "path"
    function LoadFrom(path: PChar): int; stdcall;

    // ��������� ������� ������ ��� ������ "path"
    function SaveAs(path: PChar): PChar; stdcall;

    // ������� ���������� ���� � �������
    function GetMapCount: int; stdcall;

    // ������� ����� �������� �����
    function GetActiveMapIndex: int; stdcall;

    // �������� � ������ �����
    function AddMap(path: PChar): int; stdcall;

    // ������� ����� �� �������
    function DeleteMap(path: PChar): int; stdcall;

    // ������� ��� �������� �����
    function GetActiveMap(path: PChar): int; stdcall;

    // ���������� ����� ��������
    function SetActiveMap(path: PChar): int; stdcall;

    // ������� ���� [i] �����
    function GetMapPath(i: int; path: PChar): PChar; stdcall;

    // ������� ������ ����� 
    function Contains_Map(path: PChar): int; stdcall;

    // ���������� ��������
    procedure set_proj(elp,prj: int; b1,b2,lc: double); stdcall;

    // ������� �������� � �������� ������� (�)
    // 0 - ������������� ����������
    // 1 - ������������� ����������
    procedure GetBound(pps: int; bp: pdoubles); stdcall;

    function XY_BL(x,y: double; out b,l: double): int; stdcall;
    function BL_XY(b,l: double; out x,y: double): int; stdcall;

    // ������ -> �����. [map] - ���� ��������
    // [left] - ����� ������� (�);
    // [top]  - ������� ������� (�);
    // [pix]  - ������ �������;
    // [alfa] - ������������; [0..255]
    function to_tile(map: PBitmap; left,top,pix: double; alfa: int): int; stdcall;

    // ������ -> �����. [map] - ���� ��������
    // [left] - ����� ������� (�);
    // [top]  - ������� ������� (�);
    // [pixx] - ������ ������� �� [X] (�);
    // [pixy]  - ������ ������� �� [Y] (�);
    // [alfa] - ������������; [0..255]
    function to_tile1(map: PBitmap; left,top,pixx,pixy: double; alfa: int): int; stdcall;

    function bmp_saveAs(Dest: PChar; map: PBitmap): int; stdcall;

    procedure SetBinDir(Path: PChar); stdcall;
  end;

  tdll_prj = class
    constructor Create;
    destructor Destroy; override;

  private
    fdll: HModule;
    fIntf: idmwProject;
    fEnabled: bool;
  public
    property Enabled: bool read fEnabled;

    property Intf: idmwProject read fIntf;
  end;

implementation

uses
  dynlibs,
  ofiles;

constructor tdll_prj.Create;

type
  tfunc = function(IID: PGUID; var Obj): HResult; stdcall;

var
  func: tfunc; rc: HResult;
begin
  inherited Create;
  fdll:=xLoadLibrary('lib_prj.so');

  if fdll >= 32 then begin

    func:=GetProcAddress(fdll,'dllGetInterface');
    if Assigned(func) then begin
      rc:=func(nil,fIntf);
      if rc = S_OK then
      fEnabled:=true
    end
  end;
end;

destructor tdll_prj.Destroy;
begin
  fIntf:=nil;
  xFreeLibrary(fdll);
  inherited
end;

end.
