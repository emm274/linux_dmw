library dll_prj;

uses
  Classes,
  Interfaces,
  otypes,
  prj_com;

exports
  dllGetInterface,

  prj_init,
  prj_done,

  prj_GetMapCount,
  prj_AddMap,

  prj_set_proj,
  prj_GetBound,

  prj_XY_BL,
  prj_BL_XY,

  prj_to_tile,
  prj_to_tile1,
  prj_bmp_SaveAs,

  jprj_XY_BL,
  jprj_BL_XY,

  jprj_to_tile,
  jprj_to_tile1,
  jprj_bmp_SaveAs,

  prj_SetBinDir;

begin
end.

