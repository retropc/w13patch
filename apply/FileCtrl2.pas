unit FileCtrl2;

interface

function SelectDirectory(const AHandle: THandle; const Caption: string; const Root: WideString;
  var Directory: string): Boolean;

  implementation

uses
  Windows, SysUtils;

const
  SID_IShellFolder       = '{000214E6-0000-0000-C000-000000000046}';
  SID_IEnumIDList        = '{000214F2-0000-0000-C000-000000000046}';
  BIF_NEWDIALOGSTYLE = $40;
  BIF_EDITBOX = $10;
  BIF_NONEWFOLDERBUTTON = $200;
type
  TSelectDirCB = function(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer; stdcall;
  PItemIDList = Pointer;
  TFNBFFCallBack = TSelectDirCB;
  TBrowseInfo =  record
    hwndOwner: HWND;
    pidlRoot: PItemIDList;
    pszDisplayName: PAnsiChar;  { Return display name of item selected. }
    lpszTitle: PAnsiChar;      { text to go in the banner over the tree. }
    ulFlags: UINT;           { Flags that control the return stuff }
    lpfn: TFNBFFCallBack;
    lParam: LPARAM;          { extra info that's passed back in callbacks }
    iImage: Integer;         { output var: where to return the Image index. }
  end;
  {$EXTERNALSYM IMalloc}
  IMalloc = interface(IUnknown)
    ['{00000002-0000-0000-C000-000000000046}']
    function Alloc(cb: Longint): Pointer; stdcall;
    function Realloc(pv: Pointer; cb: Longint): Pointer; stdcall;
    procedure Free(pv: Pointer); stdcall;
    function GetSize(pv: Pointer): Longint; stdcall;
    function DidAlloc(pv: Pointer): Integer; stdcall;
    procedure HeapMinimize; stdcall;
  end;
  {$EXTERNALSYM IEnumIDList}
  IEnumIDList = interface(IUnknown)
    [SID_IEnumIDList]
    function Next(celt: ULONG; out rgelt: PItemIDList;
      var pceltFetched: ULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumIDList): HResult; stdcall;
  end;
  POleStr = PWideChar;
  TIID = TGUID;
  _STRRET = record
     uType: UINT;              { One of the STRRET_* values }
     case Integer of
       0: (pOleStr: LPWSTR);                    { must be freed by caller of GetDisplayNameOf }
       1: (pStr: LPSTR);                        { NOT USED }
       2: (uOffset: UINT);                      { Offset into SHITEMID (ANSI) }
       3: (cStr: array[0..MAX_PATH-1] of Char); { Buffer to fill in }
    end;

  TStrRet = _STRRET;
  {$EXTERNALSYM IShellFolder}
  IShellFolder = interface(IUnknown)
    [SID_IShellFolder]
    function ParseDisplayName(hwndOwner: HWND;
      pbcReserved: Pointer; lpszDisplayName: POLESTR; out pchEaten: ULONG;
      out ppidl: PItemIDList; var dwAttributes: ULONG): HResult; stdcall;
    function EnumObjects(hwndOwner: HWND; grfFlags: DWORD;
      out EnumIDList: IEnumIDList): HResult; stdcall;
    function BindToObject(pidl: PItemIDList; pbcReserved: Pointer;
      const riid: TIID; out ppvOut): HResult; stdcall;
    function BindToStorage(pidl: PItemIDList; pbcReserved: Pointer;
      const riid: TIID; out ppvObj): HResult; stdcall;
    function CompareIDs(lParam: LPARAM;
      pidl1, pidl2: PItemIDList): HResult; stdcall;
    function CreateViewObject(hwndOwner: HWND; const riid: TIID;
      out ppvOut): HResult; stdcall;
    function GetAttributesOf(cidl: UINT; var apidl: PItemIDList;
      var rgfInOut: UINT): HResult; stdcall;
    function GetUIObjectOf(hwndOwner: HWND; cidl: UINT; var apidl: PItemIDList;
      const riid: TIID; prgfInOut: Pointer; out ppvOut): HResult; stdcall;
    function GetDisplayNameOf(pidl: PItemIDList; uFlags: DWORD;
      var lpName: TStrRet): HResult; stdcall;
    function SetNameOf(hwndOwner: HWND; pidl: PItemIDList; lpszName: POLEStr;
      uFlags: DWORD; var ppidlOut: PItemIDList): HResult; stdcall;
  end;

  const
  shell32 = 'shell32.dll';

function SHBrowseForFolder(var lpbi: TBrowseInfo): PItemIDList; stdcall;          external shell32 name 'SHBrowseForFolderA';
function SHGetMalloc(var ppMalloc: IMalloc): HResult; stdcall; external shell32 name 'SHGetMalloc';
function SHGetDesktopFolder(var ppshf: IShellFolder): HResult; stdcall;     external shell32 name 'SHGetDesktopFolder';
function SHGetPathFromIDList(pidl: PItemIDList; pszPath: PChar): BOOL; stdcall; external shell32 name 'SHGetPathFromIDListA';

function DirectoryExists(const Directory: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Directory));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;
const
  BIF_RETURNONLYFSDIRS   = $0001;  { For finding a folder to start document searching }
  BFFM_INITIALIZED       = 1;
  WM_USER             = $0400;
  BFFM_SETSELECTIONA          = WM_USER + 102;
  BFFM_SETSELECTION       = BFFM_SETSELECTIONA;

function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
begin
  if (uMsg = BFFM_INITIALIZED) and (lpData <> 0) then
    SendMessage(Wnd, BFFM_SETSELECTION, Integer(True), lpdata);
  result := 0;
end;

function SelectDirectory(const AHandle: THandle; const Caption: string; const Root: WideString;
  var Directory: string): Boolean;
var
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  OldErrorMode: Cardinal;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
begin
  Result := False;
  if not DirectoryExists(Directory) then
    Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(AHandle, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      end;
      with BrowseInfo do
      begin
        hwndOwner := AHandle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS + BIF_NEWDIALOGSTYLE + BIF_EDITBOX + BIF_NONEWFOLDERBUTTON;
        if Directory <> '' then
        begin
          lpfn := SelectDirCB;
          lParam := Integer(PChar(Directory));
        end;
      end;
//      WindowList := DisableTaskWindows(0);
      OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
        SetErrorMode(OldErrorMode);
//        EnableTaskWindows(WindowList);
      end;
      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

end.







