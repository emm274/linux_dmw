unit xfiles; interface {$H-}

uses
  Classes,LCLType,
  SysUtils,Graphics,Menus,
  otypes,ofiles;

const
  is_ext_fmt: Boolean = false;
  is_ext_ind: Boolean = false;
  is_old_dlg: Boolean = false;

type
  TPathStr = TShortStr;

  TOnClickFile = procedure(Path: PChar) of object;

  TFileList = class(TStringList)

    constructor Create(AMenu: TMenuItem);

    procedure Clear; override;

    procedure Load_ini(txt: TTextfile);
    procedure Save_ini(txt: TTextfile);

    procedure Add_file(Path: PChar);
    procedure delete_file(Path: PChar);

  private
    fMenu: TMenuItem;
    fOnClick: TOnClickFile;

    procedure ClickEvent(Sender: TObject);

  public
    property OnClick: TOnClickFile write fOnClick;
  end;

var
  save_ext_index: int;

procedure app_desktop;

procedure dll_app_handle(Wnd: HWnd); stdcall;
procedure dll_app_help(Help: PChar); stdcall;

procedure dll_DumpIcon(Name: PChar);
procedure dll_LoadIcon(Name: PChar);

function dll_load(Name: PChar): THandle;

function FilterStr(Filter: PChar): string;
function GetFilterIndex(Def: Integer; Dir,Filter: PChar): Integer;

function DefaultExtStr(Filter: PChar; Index: Integer): string;

function cOpenFile(ADir,AFilter,ATitle,APath: PChar): Boolean; stdcall;
function xOpenFile(ADir,AFilter,ATitle,APath: PChar; AIndex: Integer): Integer; stdcall;
function cOpenWork(AFilter,ATitle,APath: PChar): Boolean;

function cAssignFile(ADir,AFilter,ATitle,APath: PChar): Boolean;
function cSaveFile(ADir,AFilter,ATitle,APath: PChar): Boolean;
function cSaveAsFile(ADir,AFilter,ATitle,APath: PChar): Integer;

function cBatchFile(ADir,AFilter,ATitle: PChar): TStringList;

function xBatchFile(ADir,AFilter,ATitle: PChar;
                    var Index: Integer): TStringList;

function cSaveWork(AFilter,ATitle,APath: PChar): Boolean;

function dial_Color(def: TColor; out cl: TColor): Boolean;
function dial_Font(Font: TFont): Boolean;

procedure xMessage(Capt,Msg: PChar);
function xWarning(Capt,Msg: PChar): Boolean;

function File_Delete(Capt,Path: PChar): Boolean;
function File_Continue(Capt,Cmd,Path: PChar): Boolean;

procedure File_not_Found(Capt,Path: PChar);
procedure File_not_Create(Capt,Path: PChar);
procedure File_Not_Rewrite(Capt,Path: PChar);

function File_updating(Path,Capt: PChar): Boolean;

function Break_Process(Capt: PChar): Boolean;
function xBreak_Process(const Capt: String): Boolean;

function xAnswer(Capt,Msg: PChar): integer;
function xAnswerPos(Capt,Msg: PChar; var X,Y: Integer): Integer;

function Disk_Space_Expected(Path: PChar; size: Int64): Boolean;

function dlg_win_Color(var cl: TColorRef): Boolean; stdcall;

implementation

uses
  dynlibs,
  Forms,Dialogs,
  convert,LConvEncoding;

constructor TFileList.Create(AMenu: TMenuItem);
begin
  inherited Create; fMenu:=AMenu
end;

procedure TFileList.Clear;
var
  i: Integer;
begin
  if Count > 0 then
  for i:=0 to Count do
  fMenu.Items[fMenu.Count-1].Free;

  inherited
end;

procedure TFileList.ClickEvent(Sender: TObject);
var
  i: Integer; fn: TShortstr;
begin
  if Sender is TMenuItem then
  if Assigned(fOnClick) then begin

    i:=fMenu.IndexOf(Sender as TMenuItem);

    if i > 0 then begin
      i:=i - (fMenu.Count - Count);

      if i >= 0 then
      if i < Count then begin

        StrPCopy(fn,Strings[i]);

        if Strlen(fn) > 0 then
        fOnClick(fn)
      end
    end
  end
end;

procedure TFileList.Add_file(Path: PChar);
var
  Item: TMenuItem; fn: TShortStr;
begin
  if FileExist(Path) then begin

    if Count = 4 then begin
      fMenu.Items[fMenu.Count-4].Free;
      Delete(0)
    end;

    if Count < 4 then begin

      StrLower(StrCopy(fn,Path));

      if Add(StrPas(fn)) = 0 then begin

        Item:=TMenuItem.Create(fMenu.Owner);
        try
          Item.Caption:='-';
          fMenu.Add(Item)
        finally
        end

      end;

      StrLower(StrFNameCapt(fn,Path,24));

      Item:=TMenuItem.Create(fMenu.Owner);
      try
        Item.Caption:=StrPas(fn);
        Item.OnClick:=ClickEvent;
        fMenu.Add(Item)
      finally
      end
    end
  end
end;

procedure TFileList.delete_file(Path: PChar);
var
  i: Integer; fn: TShortStr;
begin
  StrUpper(StrCopy(fn,Path));
  i:=IndexOf(Strpas(fn));

  if i >= 0 then begin
    fMenu.Items[fMenu.Count-Count+i].Free;
    Delete(i); if Count = 0 then
    fMenu.Items[fMenu.Count-1].Free
  end
end;

procedure TFileList.Load_ini(txt: TTextfile);
begin
  Clear;
  while txt.xStrLine <> nil do begin
    if txt.str[0] = '#' then Break;
    if FileExist(txt.str) then
    if Indexof(txt.str) < 0 then
    Add_file(txt.str)
  end;
end;

procedure TFileList.Save_ini(txt: TTextfile);
var
  i: Integer;
begin
  for i:=0 to Count-1 do
  txt.WriteStr(Strings[i]);
end;

procedure app_desktop;
var
  i: Integer; dfm: TComponent;
begin
  for i:=0 to Application.ComponentCount-1 do begin
    dfm:=Application.Components[i]; if dfm is TForm then
    (dfm as TForm).DefaultMonitor:=dmDesktop;
  end;
end;

procedure dll_app_handle(Wnd: HWnd);
begin
//  Application.Handle:=Wnd;
  if Wnd <> 0 then dll_LoadIcon(nil);
end;

procedure dll_app_help(Help: PChar);
begin
  Application.HelpFile:=Strpas(Help)
end;

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
      on EFCreateError do;
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
    on EFOpenError do;
  end
end;

function dll_load(Name: PChar): THandle;
begin
  dll_DumpIcon(nil);
  Result:=LoadLibrary(Name);
  if Result >= 32 then begin
    set_dll_app_handle(Result);
    set_dll_intf_dir(Result)
  end
end;

function FilterStr(Filter: PChar): string;

function StrCatExt(dst,ext: PChar): PChar;
var
  w: String; s: TShortstr;
begin
  if Strlen(ext) > 0 then begin

    StrFmt(s,'Files (%s)|%s|',[ext,ext]);

    if rus_interface then begin
      w:=Format('Файлы (%s)|%s|',[ext,ext]);
      StrPCopy(s,CP1251ToUTF8(w))
    end;

    Result:=StrCat(dst,s)
  end
end;

var
  p,q,s,t: PChar; w: String;
  tmp,files: TShortStr; buf: TLongStr;
begin
  if Filter[0] = '#' then begin

    StrCopy(buf,'');
    StrCopy(tmp,Filter);

    p:=StrScan(tmp,'&');
    if Assigned(p) then begin

      p:=@tmp[1]; StrCopy(files,'');

      while p <> nil do begin
        q:=StrScan(p,'&');
        t:=StrScan(p,'#');

        if q = nil then q:=t else
        if Assigned(t) and (t < q) then q:=t;

        if q <> t then t:=nil;

        if q <> nil then begin
          q[0]:=#0; q:=@q[1];
        end;

        if StrLen(p) > 0 then begin
          if Strlen(files) > 0 then StrCat(files,';');
          StrCat(files,'*.'); StrCat(files,p)
        end;

        if Assigned(t) then begin
          StrCatExt(buf,files);
          StrCopy(files,'');
        end;

        p:=q
      end;

      StrCatExt(buf,files);
    end
    else begin
      p:=nil;
      if StrLen(tmp) > 1 then p:=@tmp[1];

      s:=nil;
      while p <> nil do begin
        q:=StrScan(p,'#');
        if q <> nil then begin
          q[0]:=#0; q:=@q[1]
        end;

        if StrLen(p) > 0 then begin

          if (s = nil) or not is_ext_fmt then s:=p;

          if rus_interface then begin
            if p[0] = '*' then
              w:=Format('Все файлы (*.%s)|*.%s|',[p,p])
            else
              w:=Format('Файлы %s(*.%s)|*.%s|',[p,s,s]);

            StrPCopy(files,CP1251ToUTF8(w))
          end
          else begin
            if p[0] = '*' then
              StrFmt(files,'All files (*.%s)|*.%s|',[p,p])
            else
              StrFmt(files,'Files %s(*.%s)|*.%s|',[p,s,s]);
          end;

          StrCat(buf,files)
        end; p:=q
      end
    end
  end
  else begin
    StrCopy(buf,Filter);
    if not rus_interface then

    while true do begin
      p:=StrScan(buf,'Ф');
      if p <> nil then begin
        if p[0] = 'Ф' then p[0]:='F';
        if p[1] = 'а' then p[1]:='i';
        if p[2] = 'й' then p[2]:='l';
        if p[3] = 'л' then p[3]:='e';
        if p[4] = 'ы' then p[4]:='s';
      end else Break
    end
  end;

  Result:=StrPas(buf)
end;

function GetFilterIndex(Def: Integer; Dir,Filter: PChar): Integer;
var
  Ind,Len: Integer; Q,P: PChar;
  Ext,Mask,tmp: TShortstr;
begin
  Result:=Def;

  if Strlen(Dir) > 0 then begin

    Q:=StrCopy(tmp,Filter);
    while true do begin
      P:=StrScan(tmp,'&');
      if P = nil then Break;
      P[0]:='#'
    end;

    Q:=Filter; Ind:=0;

    while Q[0] = '#' do begin

      P:=StrScan(@Q[1],'#');

      if P <> nil then
        Len:=P - Q - 1
      else begin
        Len:=Strlen(Q)-1; P:=StrEnd(Q)
      end;

      if Len > 0 then begin
        StrLCopy(Ext,@Q[1],Len);

        StrCopy(Mask,Dir);
        StrLCat(Mask,'\*.',255);
        StrLCat(Mask,Ext,255);

        if Add_FNames(Mask,nil) > 0 then begin
          Result:=Ind; if Ind = Def then Break
        end
      end;

      Q:=P; Inc(Ind)
    end

  end
end;

function DefaultExtStr(Filter: PChar; Index: Integer): string;
var
  rc,s,t: PChar; ext,buf: TShortStr;
begin
  Result:=''; rc:=nil;
  s:=StrCopy(buf,Filter);

  while Index > 0 do begin
    rc:=nil; Dec(Index);

    if s <> nil then
    if StrLen(s) > 0 then

    if s[0] = '#' then begin
      t:=StrScan(@s[1],'#');
      if t <> nil then t[0]:=#0;
      rc:=StrCopy(ext,@s[1]); s:=t;
      if s <> nil then s[0]:='#'
    end
    else begin
      s:=StrScan(s,'(');

      if s <> nil then
      if s[1] = '*' then
      if s[2] = '.' then begin
        s:=@s[3]; t:=StrScan(s,')');

        if t <> nil then
        if t <> s then begin
          t[0]:=#0; rc:=StrCopy(ext,@s[3]);
          s:=@t[1]
        end
      end
    end;

    if rc = nil then Break
  end;

  if rc <> nil then
  Result:=StrPas(ext)
end;

function Initial_Dir(Dir: PChar): String;
begin
  if dir_Exists(dir) then
    Result:=StrPas(Dir)
  else
    Result:=StrPas(SpaceDir);

  if not DirectoryExists(Result) then
  Result:=''
end;

function cOpenFile(ADir,AFilter,ATitle,APath: PChar): Boolean;
var
  dlg: TOpenDialog;
begin
  Result:=false;

  dlg:=TOpenDialog.Create(Application);
  try
    with dlg do begin
      InitialDir:=Initial_Dir(ADir);
      Filter:=FilterStr(AFilter);
      DefaultExt:=DefaultExtStr(AFilter,1);
      Title:=StrPas(ATitle);

      if is_ext_ind and Assigned(ADir) then
      FilterIndex:=GetFilterIndex(0,ADir,AFilter)+1;

      Options:=Options+[ofHideReadOnly];

      if is_old_dlg then
      Options:=Options+[ofOldStyleDialog];

      if Execute then begin if ADir <> nil then
        StrPCopy(ADir,ExtractFileDir(FileName));
        StrPLCopy(APath,FileName,255); Result:=true
      end
    end;
  finally
    dlg.Free;
  end;

  Application.ProcessMessages
end;

function xOpenFile(ADir,AFilter,ATitle,APath: PChar; AIndex: Integer): Integer;
var
  dlg: TOpenDialog;
begin
  Result:=-1;

  dlg:=TOpenDialog.Create(Application);
  try
    with dlg do begin
      InitialDir:=xStrPas(ADir);
      Filter:=FilterStr(AFilter);
      Title:=StrPas(ATitle);
      FilterIndex:=AIndex;

      DefaultExt:=DefaultExtStr(AFilter,1);

      Options:=options+[ofHideReadOnly];

      if is_old_dlg then
      Options:=Options+[ofOldStyleDialog];

      if Execute then begin
        xStrPCopy(ADir,ExtractFileDir(FileName));
        StrPCopy(APath,FileName); Result:=FilterIndex
      end
    end;
  finally
    dlg.Free;
  end;

  Application.ProcessMessages
end;

function cOpenWork(AFilter,ATitle,APath: PChar): Boolean;
var
  Dir: TShortStr;
begin
  StrCopy(Dir,WorkDir);
  Result:=cOpenFile(Dir,AFilter,ATitle,APath)
end;

function cAssignFile(ADir,AFilter,ATitle,APath: PChar): Boolean;
var
  dlg: TOpenDialog;
begin
  Result:=false;

  dlg:=TOpenDialog.Create(Application);
  try
    with dlg do begin
      InitialDir:=ExtractFileDir(StrPas(APath));
      FileName:=ExtractFileName(StrPas(StrLower(APath)));
      if length(InitialDir) = 0 then
      InitialDir:=xStrPas(ADir);

      Filter:=FilterStr(AFilter);
      DefaultExt:=DefaultExtStr(AFilter,1);
      Title:=StrPas(ATitle);

      Options:=Options+[ofHideReadOnly];

      if Execute then begin
        xStrPCopy(ADir,ExtractFileDir(FileName));
        StrPCopy(APath,FileName); Result:=true
      end
    end;
  finally
    dlg.Free;
  end;

  Application.ProcessMessages
end;

function cSaveFile(ADir,AFilter,ATitle,APath: PChar): Boolean;
var
  dlg: TSaveDialog;
begin
  Result:=false; StrCopy(APath,'');

  dlg:=TSaveDialog.Create(Application);
  try
    with dlg do begin
      InitialDir:=xStrPas(ADir);

      Filter:=FilterStr(AFilter);
      DefaultExt:=DefaultExtStr(AFilter,1);
      Title:=StrPas(ATitle);

      Options:=Options+[ofOverwritePrompt,ofHideReadOnly];

      if is_old_dlg then
      Options:=Options+[ofOldStyleDialog];

      if Execute then begin
        xStrPCopy(ADir,ExtractFileDir(FileName));
        StrPCopy(APath,FileName); Result:=true
      end
    end;
  finally
    dlg.Free;
  end;

  Application.ProcessMessages
end;

type
  TSaveAsDialog = class(TSaveDialog)
    constructor Create(AOwner: TComponent); override;
    procedure FilterChange(Sender: TObject);
  end;

constructor TSaveAsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnTypeChange:=FilterChange
end;

procedure TSaveAsDialog.FilterChange(Sender: TObject);
var
  s: string; i,p: Integer;
begin
  s:=Filter;
  for i:=1 to FilterIndex do begin
    p:=System.Pos('|*.',s);
    if p > 0 then begin
      System.Delete(s,1,p);
      p:=System.Pos('|',s);
      if p > 3 then begin
        if i = FilterIndex then begin
          DefaultExt:=System.Copy(s,3,p-3);
          ChangeFileExt(FileName,'.'+DefaultExt)
        end; System.Delete(s,1,p);
      end
    end
  end
end;

function cSaveAsFile(ADir,AFilter,ATitle,APath: PChar): Integer;
var
  dlg: TSaveAsDialog; ext: TFileName; i: Integer;
begin
  Result:=-1;

  dlg:=TSaveAsDialog.Create(Application);
  try
    with dlg do begin
      InitialDir:=ExtractFileDir(StrPas(APath));
      FileName:=ExtractFileName(StrPas(StrLower(APath)));
      if length(InitialDir) = 0 then if ADir <> nil then
      InitialDir:=StrPas(ADir);

      Title:=StrPas(ATitle);
      Filter:=FilterStr(AFilter);

      i:=1;
      if save_ext_index > 0 then
      i:=save_ext_index;

      DefaultExt:=DefaultExtStr(AFilter,i);

      if i > 0 then FilterIndex:=i;  

      Options:=Options+[ofOverwritePrompt,ofHideReadOnly];

      if is_old_dlg then
      Options:=Options+[ofOldStyleDialog];
      
      if Execute then begin if Adir <> nil then
        StrPCopy(ADir,ExtractFileDir(FileName));
        StrPCopy(APath,FileName);

        if ExtractFileExt(StrPas(APath)) = '' then begin
          ext:=DefaultExt; i:=System.Pos('.',ext);
          if i > 1 then System.Delete(Ext,1,i-1);
          StrPCopy(APath,FileName+ext)
        end;

        Result:=FilterIndex
      end;

    end;
  finally
    dlg.Free;
  end;

  Application.ProcessMessages
end;

function cBatchFile(ADir,AFilter,ATitle: PChar): TStringList;
var
  dlg: TOpenDialog; Path: TShortStr;
begin
  Result:=nil;

  dlg:=TOpenDialog.Create(Application);
  try
    with dlg do begin
      dlg.Files.Capacity:=1000;

      InitialDir:=Initial_Dir(ADir);
      Filter:=FilterStr(AFilter);
      DefaultExt:=DefaultExtStr(AFilter,1);
      Title:=StrPas(ATitle);

      Options:=Options+[ofAllowMultiSelect];

      if is_old_dlg then
      Options:=Options+[ofOldStyleDialog];
      
      if Execute then begin

        if ADir <> nil then
        StrPCopy(ADir,ExtractFileDir(FileName));

        if dlg.Files.Count > 0 then begin

          Result:=TStringList.Create;
          try
            if dlg.Files.Count < 1000 then
            Result.Sorted:=true;

            Result.AddStrings(dlg.Files);

            if Assigned(ADir) then
            if StrPCopy(path,files[0]) <> nil then
            StrDirectory(ADir,path);

          finally
          end
        end;

      end
    end;
  finally
    dlg.Free;
  end;
end;

function xBatchFile(ADir,AFilter,ATitle: PChar;
                    var Index: Integer): TStringList;
var
  dlg: TOpenDialog; path: TShortStr;
begin
  Result:=nil;

  dlg:=TOpenDialog.Create(Application);
  try
    with dlg do begin
      if dir_Exists(ADir) then
      InitialDir:=StrPas(ADir);

      Filter:=FilterStr(AFilter);
      DefaultExt:=DefaultExtStr(AFilter,Index+1);
      Title:=StrPas(ATitle);
      FilterIndex:=Index+1;

      Options:=Options+[ofAllowMultiSelect];

      if Execute then begin

        if ADir <> nil then
        StrPCopy(ADir,ExtractFileDir(FileName));

        if dlg.Files.Count > 0 then begin

          Result:=TStringList.Create;
          try
            Result.Sorted:=true;
            Result.AddStrings(dlg.Files);

            if Assigned(ADir) then
            if StrPCopy(path,files[0]) <> nil then
            StrDirectory(ADir,path);

            Index:=FilterIndex;

          finally
          end
        end;

      end
    end;
  finally
    dlg.Free;
  end;
end;

function cSaveWork(AFilter,ATitle,APath: PChar): Boolean;
var
  Dir: TShortStr;
begin
  StrCopy(Dir,WorkDir);
  Result:=cSaveFile(Dir,AFilter,ATitle,APath)
end;

function dial_Color(def: TColor; out cl: TColor): Boolean;
var
  dlg: TColorDialog;
begin
  Result:=false; cl:=def;

  dlg:=TColorDialog.Create(Application);
  try
    dlg.Color:=def;
    if dlg.Execute then begin
      cl:=dlg.Color; Result:=true
    end;
  finally
    dlg.Free
  end
end;

function dial_Font(Font: TFont): Boolean;
var
  dlg: TFontDialog;
begin
  Result:=false;

  dlg:=TFontDialog.Create(Application);
  try
    dlg.Font.Assign(Font);
    if dlg.Execute then begin
      Font.Assign(dlg.Font);
      Result:=true
    end;

  finally
    dlg.Free
  end
end;

procedure xMessage(Capt,Msg: PChar);
begin
  with Application do
  MessageBox(Msg,Capt,mb_Ok + mb_IconInformation)
end;

function xWarning(Capt,Msg: PChar): Boolean;
begin
  with Application do
  Result:=MessageBox(Msg,Capt,mb_OkCancel + mb_IconQuestion) = IDOK
end;

function File_Delete(Capt,Path: PChar): Boolean;
var
  fmt,msg,fn: tShortStr;
begin
  if rus_interface then
    StrCopy(fmt,'Удалить "%s"?')
  else
    StrCopy(fmt,'Delete "%s"?');

  StrFNameCapt(fn,Path,47);
  StrFmt(Msg,fmt,[fn]);

  Result:=xWarning(Capt,Msg)
end;

function File_Continue(Capt,Cmd,Path: PChar): Boolean;
var
  Msg: TShortStr;
begin
  StrFmt(Msg,'%s "%s"?',[Cmd,xStrNameExt(Path)]);
  Result:=xWarning(Capt,Msg)
end;

procedure File_Not_Found(Capt,Path: PChar);
var
  Msg,fmt,fn: tShortStr;
begin
  if rus_interface then
    StrCopy(fmt,'Файл "%s" не найден.')
  else
    StrCopy(fmt,'File "%s" not found.');

  StrFNameCapt(fn,Path,47);
  StrFmt(Msg,fmt,[fn]);

  xMessage(Capt,Msg)
end;

procedure File_Not_Create(Capt,Path: PChar);
var
  msg,fn: tShortStr;
begin
  StrNameExt(fn,Path);

  if rus_interface then
    StrFmt(msg,'Файл "%s" не создан.',[fn])
  else
    StrFmt(msg,'File "%s" not create.',[fn]);

  xMessage(Capt,msg)
end;

procedure File_Not_Rewrite(Capt,Path: PChar);
var
  msg,fn: tShortStr;
begin
  StrNameExt(fn,Path);

  if rus_interface then
    StrFmt(msg,'Файл "%s" закрыт для записи.',[fn])
  else
    StrFmt(msg,'File "%s" not overwrite.',[fn]);

  xMessage(Capt,msg)
end;

function File_updating(Path,Capt: PChar): Boolean;
var
  msg: TShortstr;
begin
  Result:=not FileIsReadOnly(Strpas(Path));

  if not Result then begin

    if rus_interface then

      StrFmt(msg,'Файл "%s" защищен от записи.'+
                 ^M^J'Снять защиту?',[xStrNameExt(Path)])

    else

      StrFmt(msg,'File "%s" is readonly.'+
                 ^M^J'Take off protection?',
                 [xStrNameExt(Path)]);

    if xWarning(Capt,msg) then
    Result:=FileReadWrite(Path)
  end
end;

function Break_Process(Capt: PChar): Boolean;
var
  Msg: TShortStr;
begin
  StrCopy(Msg,'Прервать процесс?');
  if not rus_interface then
  StrCopy(Msg,'Break process?');

  Result:=xWarning(Capt,Msg)
end;

function xBreak_Process(const Capt: String): Boolean;
var
  S: TShortStr;
begin
  Result:=Break_Process(StrPCopy(S,Capt))
end;

function xAnswer(Capt,Msg: PChar): integer;
begin
  with Application do
  Result:=MessageBox(Msg,Capt,mb_YesNoCancel + mb_IconQuestion)
end;

function xAnswerPos(Capt,Msg: PChar; var X,Y: Integer): Integer;
var
  dlg: TForm;
begin
  Result:=IDCancel;

  dlg:=CreateMessageDialog(StrPas(Msg), mtConfirmation, [mbYes,mbNo,mbCancel]);
  try
    if Capt <> nil then
    dlg.Caption:=StrPas(Capt);

    if X >= 0 then dlg.Left:=X;
    if Y >= 0 then dlg.Top:=Y;
    if (Y < 0) and (X < 0) then
    dlg.Position:=poScreenCenter;
    Result:=dlg.ShowModal;

    X:=dlg.Left; Y:=dlg.Top;

  finally
    dlg.Free;
  end;
end;

function Disk_Space_Expected(Path: PChar; size: Int64): Boolean;
var
  s,kb: String;
begin
  Result:=true;

  if not Wait_Free(Path,size) then begin

    size:=size div 1024; kb:='Kb';
    if size > 1024*10 then begin
      size:=size div 1024; kb:='Mb';
    end;

    s:=Format('Disk "%s": expected %d%s.',
              [DriveStr(Path),size,kb]);

    ShowMessage(s); Result:=false
  end
end;

function dlg_win_Color(var cl: TColorRef): Boolean; stdcall;
var
  dial: TColorDialog;
begin
  Result:=false;
  dial:=TColorDialog.Create(Application);
  try
    dial.Color:=cl;
    dial.CustomColors.Add('ColorA='+HexToStr(cl,6));
    if dial.Execute then begin
      cl:=dial.Color; Result:=true
    end;
  finally
    dial.Free
  end
end;

begin
  save_ext_index:=0
end.