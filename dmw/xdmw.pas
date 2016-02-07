unit xdmw; interface {$A1}

uses
  LCLType,otypes,xintf;

type
  tdmRec = record
    Code, ox1,oy1,ox2,oy2, Color: longint;
    Loc,Level: byte; Flags: word;
  end;

  IDmwDraw = interface(IUnknown)
    ['{EE3B9781-E103-11D2-A776-F570F46B847D}']

    function Open(Path: PChar; rw: WordBool): WordBool; stdcall;
    procedure Close; stdcall;

    function New(Path,Obj: PChar;
                 xmin,ymin,xmax,ymax: double;
                 sys: psys): WordBool; stdcall;

    function Get_Scale: Integer; stdcall;
    function Get_Nom(Nom: PChar): PChar; stdcall;
    function Get_Legend(Name: PChar): PChar; stdcall;

    function Get_sys(out sys: tsys): Integer; stdcall;

    function Get_Link(l,g: PGPoly): Integer; stdcall;

    procedure Get_Object(p: longint; out dRec: tdmRec; txt: PChar); stdcall;

    procedure Draw_Start(x1,y1,x2,y2, lev: longint); stdcall;
    procedure Disp_Start(p, x1,y1,x2,y2, lev: longint); stdcall;
    function Draw_Buffer(out cash: pbytes): longint; stdcall;
    procedure Draw_Stop; stdcall;

    procedure Draw_direct(Wnd: HWnd; Bmp: HBitmap; DC: HDC;
                          Tr: PDoubles; Color: Integer); stdcall;

    procedure Draw_bound(DC: HDC; Tr: PDoubles; Color: Integer); stdcall;

    function Get_Poly_Cnt(p: longint): Integer; stdcall;
    function Get_Poly(p: longint; lp: PLPoly; lp_max: Integer): Integer; stdcall;

    function Get_mf(p: longint; lp: PLPoly; hp: PIntegers;
                    lp_max: Integer): Integer; stdcall;

    function Capt_Object(p: longint; capt: PChar): PChar; stdcall;
    procedure Info_Object(p: longint); stdcall;

    function Id_Object(p: Integer): Integer; stdcall;

    function Items_Count: Integer; stdcall;
    function Get_Item(I: Integer): Integer; stdcall;

    procedure Set_SpaceDir(Dir: PChar); stdcall;
    procedure Set_IntfDir(Dir: PChar); stdcall;
  end;

  IDmwWindow = interface(IUnknown)
    ['{80BC432E-330F-44E6-8C79-E8D2CE78C623}']
    procedure HideMap; stdcall;

    function GetMaps(out Obj: IStrings1): Boolean; stdcall;
    function GetRels(out Obj: IStrings1): Boolean; stdcall;
    function GetAlxs(out Obj: IStrings1): Boolean; stdcall;
    function GetTifs(out Obj: IStrings1): Boolean; stdcall;

    function GetListv(out Obj: IIntegers1): Boolean; stdcall;

    function GetActiveMap(Path: PChar): PChar; stdcall;
    function GetSceneMap(Path: PChar): PChar; stdcall;

    function GetFrame(G,B: PGPoly; out s: tsys): Integer; stdcall;
    function GetCentre(out cx,cy,sc: Double): Boolean; stdcall;

    function GetMpp: Double; stdcall;

    procedure DispObject(dm: PChar; ptr,mode: Integer); stdcall;
    procedure Draft(x1,y1,x2,y2,Tag: Integer); stdcall;
  end;

implementation

end.