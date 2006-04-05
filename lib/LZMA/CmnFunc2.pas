unit CmnFunc2;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Common non-VCL functions

  $jrsoftware: issrc/Projects/CmnFunc2.pas,v 1.44 2004/03/03 02:30:19 jr Exp $
}

{$B-,R-}

interface

{$I VERSION.INC}

uses
  Windows, SysUtils;

{ Delphi 2.01's RegStr unit should never be used because it contains many
  wrong declarations. Delphi 3's RegStr unit doesn't have this problem, but
  for backward compatibility, it defines a few of the correct registry key
  constants here. }
const
  { Do NOT localize any of these }
  NEWREGSTR_PATH_SETUP = 'Software\Microsoft\Windows\CurrentVersion';
  NEWREGSTR_PATH_EXPLORER = NEWREGSTR_PATH_SETUP + '\Explorer';
  NEWREGSTR_PATH_SPECIAL_FOLDERS = NEWREGSTR_PATH_EXPLORER + '\Shell Folders';
  NEWREGSTR_PATH_UNINSTALL = NEWREGSTR_PATH_SETUP + '\Uninstall';
  NEWREGSTR_VAL_UNINSTALLER_DISPLAYNAME = 'DisplayName';
  NEWREGSTR_VAL_UNINSTALLER_COMMANDLINE = 'UninstallString';

function NewFileExists(const Name: String): Boolean;
function DirExists(const Name: String): Boolean;
function FileOrDirExists(const Name: String): Boolean;
function GetIniString(const Section, Key, Default, Filename: String): String;
function GetIniInt(const Section, Key: String; const Default, Min, Max: Longint; const Filename: String): Longint;
function GetIniBool(const Section, Key: String; const Default: Boolean; const Filename: String): Boolean;
function IniKeyExists(const Section, Key, Filename: String): Boolean;
function IsIniSectionEmpty(const Section, Filename: String): Boolean;
function SetIniString(const Section, Key, Value, Filename: String): Boolean;
function SetIniInt(const Section, Key: String; const Value: Longint; const Filename: String): Boolean;
function SetIniBool(const Section, Key: String; const Value: Boolean; const Filename: String): Boolean;
procedure DeleteIniEntry(const Section, Key, Filename: String);
procedure DeleteIniSection(const Section, Filename: String);
function GetEnv(const EnvVar: String): String;
function GetCmdTail: String;
function NewParamCount: Integer;
function NewParamStr(Index: Integer): string;
function AddQuotes(const S: String): String;
function RemoveQuotes(const S: String): String;
function GetShortName(const LongName: String): String;
function GetWinDir: String;
function GetSystemDir: String;
function GetTempDir: String;
procedure StringChange(var S: String; const FromStr, ToStr: String);
function AdjustLength(var S: String; const Res: Cardinal): Boolean;
function UsingWinNT: Boolean;
function ConvertConstPercentStr(var S: String): Boolean;
function ConvertPercentStr(var S: String): Boolean;
function ConstPos(const Ch: Char; const S: String): Integer;
function SkipPastConst(const S: String; const Start: Integer): Integer;
function RegQueryStringValue(H: HKEY; Name: PChar; var ResultStr: String): Boolean;
function RegQueryMultiStringValue(H: HKEY; Name: PChar; var ResultStr: String): Boolean;
function RegValueExists(H: HKEY; Name: PChar): Boolean;
function RegDeleteKeyIncludingSubkeys(const Key: HKEY; const Name: PChar): Longint;
function RegDeleteKeyIfEmpty(const RootKey: HKEY; const SubkeyName: PChar): Longint;
function GetShellFolderPath(const FolderID: Integer): String;
function GetProgramFilesPath: String;
function GetCommonFilesPath: String;
function IsAdminLoggedOn: Boolean;
function IsPowerUserLoggedOn: Boolean;
function IsMultiByteString(S: String): Boolean;
function FontExists(const FaceName: String): Boolean;
{$IFNDEF IS_D5}
procedure FreeAndNil(var Obj);
function SafeLoadLibrary(const Filename: String; ErrorMode: UINT): HMODULE;
{$ENDIF}
function GetUILanguage: LANGID;
function RemoveAccelChar(const S: String): String;
function GetTextWidth(const DC: HDC; S: String; const Prefix: Boolean): Integer;
function AddPeriod(const S: String): String;
function GetPreferredUIFont: String;
function IsWildcard(const Pattern: String): Boolean;
function WildcardMatch(const Text, Pattern: PChar): Boolean;
function IntMax(const A, B: Integer): Integer;
function Win32ErrorString(ErrorCode: Integer): String;

type
  PLeadByteSet = ^TLeadByteSet;
  TLeadByteSet = set of Char;
var
  ConstLeadBytes: PLeadByteSet = nil;

implementation

uses
  {$IFNDEF Delphi3orHigher} OLE2, {$ELSE} ActiveX, {$ENDIF} ShlObj, PathFunc;

function InternalGetFileAttr(const Name: String): Integer;
var
  OldErrorMode: UINT;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);  { Prevent "Network Error" boxes }
  try
    Result := GetFileAttributes(PChar(RemoveBackslashUnlessRoot(Name)));
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

function NewFileExists(const Name: String): Boolean;
{ Returns True if the specified file exists.
  This function is better than Delphi's FileExists function because it works
  on files in directories that don't have "list" permission. There is, however,
  one other difference: FileExists allows wildcards, but this function does
  not. }
var
  Attr: Integer;
begin
  Attr := InternalGetFileAttr(Name);
  Result := (Attr <> -1) and (Attr and faDirectory = 0);
end;

function DirExists(const Name: String): Boolean;
{ Returns True if the specified directory name exists. The specified name
  may include a trailing backslash.
  NOTE: Delphi's FileCtrl unit has a similar function called DirectoryExists.
  However, the implementation is different between Delphi 1 and 2. (Delphi 1
  does not count hidden or system directories as existing.) }
var
  Attr: Integer;
begin
  Attr := InternalGetFileAttr(Name);
  Result := (Attr <> -1) and (Attr and faDirectory <> 0);
end;

function FileOrDirExists(const Name: String): Boolean;
{ Returns True if the specified directory or file name exists. The specified
  name may include a trailing backslash. }
begin
  Result := InternalGetFileAttr(Name) <> -1;
end;

function GetIniString(const Section, Key, Default, Filename: String): String;
begin
  SetLength(Result, 1023);
  if Filename <> '' then
    SetLength(Result, GetPrivateProfileString(
      PChar(Section), PChar(Key), PChar(Default),
      @Result[1], 1024, PChar(Filename)))
  else
    SetLength(Result, GetProfileString(
      PChar(Section), PChar(Key), PChar(Default),
      @Result[1], 1024));
end;

function GetIniInt(const Section, Key: String;
  const Default, Min, Max: Longint; const Filename: String): Longint;
{ Reads a Longint from an INI file. If the Longint read is not between Min/Max
  then it returns Default. If Min=Max then Min/Max are ignored }
var
  S: String;
  E: Integer;
begin
  S := GetIniString(Section, Key, '', Filename);
  if S = '' then
    Result := Default
  else begin
    Val(S, Result, E);
    if (E <> 0) or ((Min <> Max) and ((Result < Min) or (Result > Max))) then
      Result := Default;
  end;
end;

function GetIniBool(const Section, Key: String; const Default: Boolean;
  const Filename: String): Boolean;
begin
  Result := GetIniInt(Section, Key, Ord(Default), 0, 0, Filename) <> 0;
end;

function IniKeyExists(const Section, Key, Filename: String): Boolean;
  function Equals(const Default: PChar): Boolean;
  var
    Test: array[0..7] of Char;
  begin
    Test[0] := #0;
    if Filename <> '' then
      GetPrivateProfileString(PChar(Section), PChar(Key), Default,
        Test, SizeOf(Test), PChar(Filename))
    else
      GetProfileString(PChar(Section), PChar(Key), Default,
        Test, SizeOf(Test));
    Result := lstrcmp(Test, Default) = 0;
  end;
begin
  { If the key does not exist, a default string is returned both times. }
  Result := not Equals('x1234x') or not Equals('x5678x');  { <- don't change }
end;

function IsIniSectionEmpty(const Section, Filename: String): Boolean;
var
  Test: array[0..255] of Char;
begin
  Test[0] := #0;
  if Filename <> '' then
    GetPrivateProfileString(PChar(Section), nil, '', Test,
      SizeOf(Test), PChar(Filename))
  else
    GetProfileString(PChar(Section), nil, '', Test, SizeOf(Test));
  Result := Test[0] = #0;
end;

function SetIniString(const Section, Key, Value, Filename: String): Boolean;
begin
  if Filename <> '' then
    Result := WritePrivateProfileString(PChar(Section), PChar(Key),
      PChar(Value), PChar(Filename))
  else
    Result := WriteProfileString(PChar(Section), PChar(Key),
      PChar(Value));
end;

function SetIniInt(const Section, Key: String; const Value: Longint;
  const Filename: String): Boolean;
begin
  Result := SetIniString(Section, Key, IntToStr(Value), Filename);
end;

function SetIniBool(const Section, Key: String; const Value: Boolean;
  const Filename: String): Boolean;
begin
  Result := SetIniInt(Section, Key, Ord(Value), Filename);
end;

procedure DeleteIniEntry(const Section, Key, Filename: String);
begin
  if Filename <> '' then
    WritePrivateProfileString(PChar(Section), PChar(Key),
      nil, PChar(Filename))
  else
    WriteProfileString(PChar(Section), PChar(Key),
      nil);
end;

procedure DeleteIniSection(const Section, Filename: String);
begin
  if Filename <> '' then
    WritePrivateProfileString(PChar(Section), nil, nil,
      PChar(Filename))
  else
    WriteProfileString(PChar(Section), nil, nil);
end;

function GetEnv(const EnvVar: String): String;
{ Gets the value of the specified environment variable. (Just like TP's GetEnv) }
var
  Res: DWORD;
begin
  SetLength(Result, 255);
  repeat
    Res := GetEnvironmentVariable(PChar(EnvVar), PChar(Result), Length(Result));
    if Res = 0 then begin
      Result := '';
      Break;
    end;
  until AdjustLength(Result, Res);
end;

function GetCmdTail: String;
{ Returns all command line parameters passed to the process as a single
  string. }
var
  CmdLine: PChar;
  InQuote: Boolean;
begin
  CmdLine := GetCommandLine;
  InQuote := False;
  while True do begin
    case CmdLine^ of
       #0: Break;
      '"': InQuote := not InQuote;
      ' ': if not InQuote then Break;
    end;
    Inc(CmdLine);
  end;
  while CmdLine^ = ' ' do
    Inc(CmdLine);
  Result := CmdLine;
end;

function GetParamStr(P: PChar; var Param: String): PChar;
var
  Len: Integer;
  Buffer: array[0..4095] of Char;
begin
  while True do begin
    while (P[0] <> #0) and (P[0] <= ' ') do Inc(P);
    if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
  end;
  Len := 0;
  while (P[0] > ' ') and (Len < SizeOf(Buffer)) do
    if P[0] = '"' then begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do begin
        Buffer[Len] := P[0];
        Inc(Len);
        Inc(P);
      end;
      if P[0] <> #0 then Inc(P);
    end
    else begin
      Buffer[Len] := P[0];
      Inc(Len);
      Inc(P);
    end;
  SetString(Param, Buffer, Len);
  Result := P;
end;

function NewParamCount: Integer;
var
  P2: String;
  P: PChar;
  S: string;
begin
  P2 := GetCmdTail;
  P := PChar(P2);
  Result := 0;
  while True do begin
    P := GetParamStr(P, S);
    if S = '' then Break;
    Inc(Result);
  end;
end;

function NewParamStr(Index: Integer): string;
var
  Buffer: array[0..MAX_PATH-1] of Char;
  P2: String;
  P: PChar;
begin
  if Index = 0 then begin
    SetString(Result, Buffer, GetModuleFileName(0, Buffer, SizeOf(Buffer)));
  end
  else begin
    P2 := GetCmdTail;
    P := PChar(P2);
    while True do begin
      P := GetParamStr(P, Result);
      if (Index = 1) or (Result = '') then Break;
      Dec(Index);
    end;
  end;
end;

function AddQuotes(const S: String): String;
{ Adds a quote (") character to the left and right sides of the string if
  the string contains a space and it didn't have quotes already. This is
  primarily used when spawning another process with a long filename as one of
  the parameters. }
begin
  Result := Trim(S);
  if (PathPos(' ', Result) <> 0) and
     ((Result[1] <> '"') or (PathLastChar(Result)^ <> '"')) then
    Result := '"' + Result + '"';
end;

function RemoveQuotes(const S: String): String;
{ Opposite of AddQuotes; removes any quotes around the string. }
begin
  Result := S;
  while (Result <> '') and (Result[1] = '"') do
    Delete(Result, 1, 1);
  while (Result <> '') and (PathLastChar(Result)^ = '"') do
    SetLength(Result, Length(Result)-1);
end;

function ConvertPercentStr(var S: String): Boolean;
{ Expands all %-encoded characters in the string (see RFC 2396). Returns True
  if all were successfully expanded. }
var
  I, C, E: Integer;
  N: String;
begin
  Result := True;
  I := 1;
  while I <= Length(S) do begin
    if S[I] = '%' then begin
      N := Copy(S, I, 3);
      if Length(N) <> 3 then begin
        Result := False;
        Break;
      end;
      N[1] := '$';
      Val(N, C, E);
      if E <> 0 then begin
        Result := False;
        Break;
      end;
      { delete the two numbers following '%', and replace '%' with the character }
      Delete(S, I+1, 2);
      S[I] := Chr(C);
    end;
    Inc(I);
  end;
end;

function SkipPastConst(const S: String; const Start: Integer): Integer;
{ Returns the character index following the Inno Setup constant embedded
  into the string S at index Start.
  If the constant is not closed (missing a closing brace), it returns zero. }
var
  L, BraceLevel, LastOpenBrace: Integer;
begin
  Result := Start;
  L := Length(S);
  if Result < L then begin
    Inc(Result);
    if S[Result] = '{' then begin
      Inc(Result);
      Exit;
    end
    else begin
      BraceLevel := 1;
      LastOpenBrace := -1;
      while Result <= L do begin
        case S[Result] of
          '{': begin
                   if LastOpenBrace <> Result-1 then begin
                     Inc(BraceLevel);
                     LastOpenBrace := Result;
                   end
                   else
                     { Skip over '{{' when in an embedded constant }
                     Dec(BraceLevel);
                 end;
          '}': begin
                 Dec(BraceLevel);
                 if BraceLevel = 0 then begin
                   Inc(Result);
                   Exit;
                 end;
               end;
        else
          if S[Result] in ConstLeadBytes^ then
            Inc(Result);
        end;
        Inc(Result);
      end;
    end;
  end;
  Result := 0;
end;

function ConvertConstPercentStr(var S: String): Boolean;
{ Same as ConvertPercentStr, but is designed to ignore embedded Inno Setup
  constants. Any '%' characters between braces are not translated. Two
  consecutive braces are ignored. }
var
  I, C, E: Integer;
  N: String;
begin
  Result := True;
  I := 1;
  while I <= Length(S) do begin
    case S[I] of
      '{': begin
             I := SkipPastConst(S, I);
             if I = 0 then begin
               Result := False;
               Break;
             end;
             Dec(I);  { ...since there's an Inc below }
           end;
      '%': begin
             N := Copy(S, I, 3);
             if Length(N) <> 3 then begin
               Result := False;
               Break;
             end;
             N[1] := '$';
             Val(N, C, E);
             if E <> 0 then begin
               Result := False;
               Break;
             end;
             { delete the two numbers following '%', and replace '%' with the character }
             Delete(S, I+1, 2);
             S[I] := Chr(C);
           end;
    else
      if S[I] in ConstLeadBytes^ then
        Inc(I);
    end;
    Inc(I);
  end;
end;

function ConstPos(const Ch: Char; const S: String): Integer;
{ Like the standard Pos function, but skips over any Inno Setup constants
  embedded in S }
var
  I, L: Integer;
begin
  Result := 0;
  I := 1;
  L := Length(S);
  while I <= L do begin
    if S[I] = Ch then begin
      Result := I;
      Break;
    end
    else if S[I] = '{' then begin
      I := SkipPastConst(S, I);
      if I = 0 then
        Break;
    end
    else begin
      if S[I] in ConstLeadBytes^ then
        Inc(I);
      Inc(I);
    end;
  end;
end;

function GetShortName(const LongName: String): String;
{ Gets the short version of the specified long filename. If the file does not
  exist, or some other error occurs, it returns LongName. }
var
  Res: DWORD;
begin
  SetLength(Result, MAX_PATH);
  repeat
    Res := GetShortPathName(PChar(LongName), PChar(Result), Length(Result));
    if Res = 0 then begin
      Result := LongName;
      Break;
    end;
  until AdjustLength(Result, Res);
end;

function GetWinDir: String;
{ Returns fully qualified path of the Windows directory. Only includes a
  trailing backslash if the Windows directory is the root directory. }
var
  Buf: array[0..MAX_PATH-1] of Char;
begin
  GetWindowsDirectory(Buf, SizeOf(Buf));
  Result := StrPas(Buf);
end;

function GetSystemDir: String;
{ Returns fully qualified path of the Windows System directory. Only includes a
  trailing backslash if the Windows System directory is the root directory. }
var
  Buf: array[0..MAX_PATH-1] of Char;
begin
  GetSystemDirectory(Buf, SizeOf(Buf));
  Result := StrPas(Buf);
end;

function GetTempDir: String;
{ Returns fully qualified path of the temporary directory, with trailing
  backslash. This does not use the Win32 function GetTempPath, due to platform
  differences. }
label 1;
begin
  Result := GetEnv('TMP');
  if (Result <> '') and DirExists(Result) then
    goto 1;
  Result := GetEnv('TEMP');
  if (Result <> '') and DirExists(Result) then
    goto 1;
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    { Like Windows 2000's GetTempPath, return USERPROFILE when TMP and TEMP
      are not set }
    Result := GetEnv('USERPROFILE');
    if (Result <> '') and DirExists(Result) then
      goto 1;
  end;
  Result := GetWinDir;
1:Result := AddBackslash(ExpandFileName(Result));
end;

procedure StringChange(var S: String; const FromStr, ToStr: String);
{ Change all occurrences in S of FromStr to ToStr }
var
  StartPos, I: Integer;
label 1;
begin
  if FromStr = '' then Exit;
  StartPos := 1;
1:for I := StartPos to Length(S)-Length(FromStr)+1 do begin
    if Copy(S, I, Length(FromStr)) = FromStr then begin
      Delete(S, I, Length(FromStr));
      Insert(ToStr, S, I);
      StartPos := I + Length(ToStr);
      goto 1;
    end;
  end;
end;

function AdjustLength(var S: String; const Res: Cardinal): Boolean;
{ Returns True if successful. Returns False if buffer wasn't large enough,
  and called AdjustLength to resize it. }
begin
  Result := Integer(Res) < Length(S);
  SetLength(S, Res);
end;

function UsingWinNT: Boolean;
{ Returns True if system is running any version of Windows NT. Never returns
  True on Windows 95 or 3.1. }
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
end;

function InternalRegQueryStringValue(H: HKEY; Name: PChar; var ResultStr: String;
  Type1, Type2: DWORD): Boolean;
var
  Typ, Size: DWORD;
  S: String;
begin
  Result := False;
  if (RegQueryValueEx(H, Name, nil, @Typ, nil, @Size) = ERROR_SUCCESS) and
     ((Typ = Type1) or (Typ = Type2)) then begin
    if Size < 2 then begin  {for the following code to work properly, Size can't be 0 or 1}
      ResultStr := '';
      Result := True;
    end
    else begin
      SetLength(S, Size-1); {long strings implicity include a null terminator}
      if RegQueryValueEx(H, Name, nil, nil, @S[1], @Size) = ERROR_SUCCESS then begin
        ResultStr := S;
        Result := True;
      end;
    end;
  end;
end;

function RegQueryStringValue(H: HKEY; Name: PChar; var ResultStr: String): Boolean;
{ Queries the specified REG_SZ or REG_EXPAND_SZ registry key/value, and returns
  the value in ResultStr. Returns True if successful. When False is returned,
  ResultStr is unmodified. }
begin
  Result := InternalRegQueryStringValue(H, Name, ResultStr, REG_SZ,
    REG_EXPAND_SZ);
end;

function RegQueryMultiStringValue(H: HKEY; Name: PChar; var ResultStr: String): Boolean;
{ Queries the specified REG_MULTI_SZ registry key/value, and returns the value
  in ResultStr. Returns True if successful. When False is returned, ResultStr
  is unmodified. }
begin
  Result := InternalRegQueryStringValue(H, Name, ResultStr, REG_MULTI_SZ,
    REG_MULTI_SZ);
end;

function RegValueExists(H: HKEY; Name: PChar): Boolean;
{ Returns True if the specified value exists. Requires KEY_QUERY_VALUE access
  to the key. }
var
  I: Integer;
  EnumName: array[0..1] of Char;
  Count: DWORD;
  ErrorCode: Longint;
begin
  Result := RegQueryValueEx(H, Name, nil, nil, nil, nil) = ERROR_SUCCESS;
  if Result and ((Name = nil) or (Name^ = #0)) and
     (Win32Platform <> VER_PLATFORM_WIN32_NT) then begin
    { On Win9x/Me a default value always exists according to RegQueryValueEx,
      so it must use RegEnumValue instead to check if a default value
      really exists }
    Result := False;
    I := 0;
    while True do begin
      Count := SizeOf(EnumName);
      ErrorCode := RegEnumValue(H, I, EnumName, Count, nil, nil, nil, nil);
      if (ErrorCode <> ERROR_SUCCESS) and (ErrorCode <> ERROR_MORE_DATA) then
        Break;
      { is it the default value? }
      if (ErrorCode = ERROR_SUCCESS) and (EnumName[0] = #0) then begin
        Result := True;
        Break;
      end;
      Inc(I);
    end;
  end;
end;

function RegDeleteKeyIncludingSubkeys(const Key: HKEY; const Name: PChar): Longint;
{ Deletes the specified key and all subkeys.
  Returns ERROR_SUCCESS if the key was successful deleted. }
var
  H: HKEY;
  KeyName: String;
  KeyNameCount, MaxCount: DWORD;
  FT: TFileTime;
  I: Integer;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    if RegOpenKeyEx(Key, Name, 0, KEY_ENUMERATE_SUB_KEYS or KEY_QUERY_VALUE, H) = ERROR_SUCCESS then begin
      if RegQueryInfoKey(H, nil, nil, nil, nil, @MaxCount, nil, nil, nil, nil,
         nil, nil) = ERROR_SUCCESS then begin
        if MaxCount < 1 then MaxCount := 1;
        SetLength(KeyName, MaxCount);
        I := 0;
        while True do begin
          KeyNameCount := MaxCount+1;
          if RegEnumKeyEx(H, I, PChar(KeyName), KeyNameCount, nil, nil, nil, @FT) <> ERROR_SUCCESS then
            Break;
          if RegDeleteKeyIncludingSubkeys(H, PChar(KeyName)) <> ERROR_SUCCESS then
            Inc(I);
        end;
      end;
      RegCloseKey(H);
    end;
  end;
  Result := RegDeleteKey(Key, Name);
end;

function RegDeleteKeyIfEmpty(const RootKey: HKEY; const SubkeyName: PChar): Longint;
{ Deletes the specified subkey if it has no subkeys or values.
  Returns ERROR_SUCCESS if the key was successful deleted, ERROR_DIR_NOT_EMPTY
  if it was not deleted because it contained subkeys or values, or possibly
  some other Win32 error code. }
var
  K: HKEY;
  NumSubkeys, NumValues: DWORD;
begin
  Result := RegOpenKeyEx(RootKey, SubkeyName, 0, KEY_QUERY_VALUE, K);
  if Result <> ERROR_SUCCESS then
    Exit;
  Result := RegQueryInfoKey(K, nil, nil, nil, @NumSubkeys, nil, nil,
    @NumValues, nil, nil, nil, nil);
  RegCloseKey(K);
  if Result <> ERROR_SUCCESS then
    Exit;
  if (NumSubkeys = 0) and (NumValues = 0) then
    Result := RegDeleteKey(RootKey, SubkeyName)
  else
    Result := ERROR_DIR_NOT_EMPTY;
end;

function GetShellFolderPath(const FolderID: Integer): String;
var
  pidl: PItemIDList;
  Buffer: array[0..MAX_PATH-1] of Char;
  Malloc: IMalloc;
begin
  Result := '';
  if FAILED(SHGetMalloc(Malloc)) then
    Malloc := nil;
  if SUCCEEDED(SHGetSpecialFolderLocation(0, FolderID, pidl)) then begin
    if SHGetPathFromIDList(pidl, Buffer) then
      Result := Buffer;
    if Assigned(Malloc) then
      Malloc.Free(pidl);
  end;
end;

function GetPathFromRegistry(const Name: PChar): String;
var
  H: HKEY;
begin
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, NEWREGSTR_PATH_SETUP, 0,
     KEY_QUERY_VALUE, H) = ERROR_SUCCESS then begin
    if not RegQueryStringValue(H, Name, Result) then
      Result := '';
    RegCloseKey(H);
  end
  else
    Result := '';
end;

function GetProgramFilesPath: String;
{ Gets path of Program Files.
  Returns blank string if not found in registry. }
begin
  Result := GetPathFromRegistry('ProgramFilesDir');
end;

function GetCommonFilesPath: String;
{ Gets path of Common Files.
  Returns blank string if not found in registry. }
begin
  Result := GetPathFromRegistry('CommonFilesDir');
end;

function IsMemberOfGroup(const DomainAliasRid: DWORD): Boolean;
{ Returns True if the logged-on user is a member of the specified local
  group. Always returns True on Windows 9x/Me. }
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  SE_GROUP_ENABLED           = $00000004;
  SE_GROUP_USE_FOR_DENY_ONLY = $00000010;
var
  Sid: PSID;
  CheckTokenMembership: function(TokenHandle: THandle; SidToCheck: PSID;
    var IsMember: BOOL): BOOL; stdcall;
  IsMember: BOOL;
  Token: THandle;
  GroupInfoSize: DWORD;
  GroupInfo: PTokenGroups;
  I: Integer;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then begin
    Result := True;
    Exit;
  end;

  Result := False;

  if not AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
     SECURITY_BUILTIN_DOMAIN_RID, DomainAliasRid,
     0, 0, 0, 0, 0, 0, Sid) then
    Exit;
  try
    { Use CheckTokenMembership if available. MSDN states:
      "The CheckTokenMembership function should be used with Windows 2000 and
      later to determine whether a specified SID is present and enabled in an
      access token. This function eliminates potential misinterpretations of
      the active group membership if changes to access tokens are made in
      future releases." }
    CheckTokenMembership := nil;
    if Lo(GetVersion) >= 5 then
      CheckTokenMembership := GetProcAddress(GetModuleHandle(advapi32),
        'CheckTokenMembership');
    if Assigned(CheckTokenMembership) then begin
      if CheckTokenMembership(0, Sid, IsMember) then
        Result := IsMember;
    end
    else begin
      GroupInfo := nil;
      if not OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True,
         {$IFDEF Delphi3orHigher} Token {$ELSE} @Token {$ENDIF}) then begin
        if GetLastError <> ERROR_NO_TOKEN then
          Exit;
        if not OpenProcessToken(GetCurrentProcess, TOKEN_QUERY,
           {$IFDEF Delphi3orHigher} Token {$ELSE} @Token {$ENDIF}) then
          Exit;
      end;
      try
        GroupInfoSize := 0;
        if not GetTokenInformation(Token, TokenGroups, nil, 0, GroupInfoSize) and
           (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
          Exit;

        GetMem(GroupInfo, GroupInfoSize);
        if not GetTokenInformation(Token, TokenGroups, GroupInfo,
           GroupInfoSize, GroupInfoSize) then
          Exit;

        for I := 0 to GroupInfo.GroupCount-1 do begin
          if EqualSid(Sid, GroupInfo.Groups[I].Sid) and
             (GroupInfo.Groups[I].Attributes and (SE_GROUP_ENABLED or
              SE_GROUP_USE_FOR_DENY_ONLY) = SE_GROUP_ENABLED) then begin
            Result := True;
            Break;
          end;
        end;
      finally
        FreeMem(GroupInfo);
        CloseHandle(Token);
      end;
    end;
  finally
    FreeSid(Sid);
  end;
end;

function IsAdminLoggedOn: Boolean;
{ Returns True if the logged-on user is a member of the Administrators local
  group. Always returns True on Windows 9x/Me. }
const
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
begin
  Result := IsMemberOfGroup(DOMAIN_ALIAS_RID_ADMINS);
end;

function IsPowerUserLoggedOn: Boolean;
{ Returns True if the logged-on user is a member of the Power Users local
  group. Always returns True on Windows 9x/Me. }
const
  DOMAIN_ALIAS_RID_POWER_USERS = $00000223;
begin
  Result := IsMemberOfGroup(DOMAIN_ALIAS_RID_POWER_USERS);
end;

function IsMultiByteString(S: String): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(S) do
    if IsDBCSLeadByte(Ord(S[I])) then begin
      Result := True;
      Break;
    end;
end;

function FontExistsCallback(const lplf: TLogFont; const lptm: TTextMetric;
  dwType: DWORD; lpData: LPARAM): Integer; stdcall;
begin
  Boolean(Pointer(lpData)^) := True;
  Result := 1;
end;

function FontExists(const FaceName: String): Boolean;
var
  DC: HDC;
begin
  Result := False;
  DC := GetDC(0);
  try
    EnumFonts(DC, PChar(FaceName), @FontExistsCallback, @Result);
  finally
    ReleaseDC(0, DC);
  end;
end;

{$IFNDEF IS_D5}
procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;
{$ENDIF}

{$IFNDEF IS_D5}
function SafeLoadLibrary(const Filename: String; ErrorMode: UINT): HMODULE;
var
  SaveErrorMode: UINT;
  SaveFPUControlWord: Word;
begin
  SaveErrorMode := SetErrorMode(ErrorMode);
  try
    asm
      FNSTCW SaveFPUControlWord
    end;
    try
      Result := LoadLibrary(PChar(Filename));
    finally
      asm
        FNCLEX
        FLDCW SaveFPUControlWord
      end;
    end;
  finally
    SetErrorMode(SaveErrorMode);
  end;
end;
{$ENDIF}

function GetUILanguage: LANGID;
{ Platform-independent version of GetUserDefaultUILanguage. May return 0 in
  case of failure. }
var
  GetUserDefaultUILanguage: function: LANGID; stdcall;
  K: HKEY;
  S: String;
  E: Integer;
begin
  GetUserDefaultUILanguage := GetProcAddress(GetModuleHandle(kernel32),
    'GetUserDefaultUILanguage');
  if Assigned(GetUserDefaultUILanguage) then
    { This function is available on Windows 2000, Me, and later }
    Result := GetUserDefaultUILanguage
  else begin
    if Win32Platform = VER_PLATFORM_WIN32_NT then begin
      { Windows NT 4.0 }
      if RegOpenKeyEx(HKEY_USERS, '.DEFAULT\Control Panel\International',
         0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        RegQueryStringValue(K, 'Locale', S);
        RegCloseKey(K);
      end;
    end
    else begin
      { Windows 95/98 }
      if RegOpenKeyEx(HKEY_CURRENT_USER, 'Control Panel\Desktop\ResourceLocale',
         0, KEY_QUERY_VALUE, K) = ERROR_SUCCESS then begin
        RegQueryStringValue(K, '', S);
        RegCloseKey(K);
      end;
    end;
    Val('$' + S, Result, E);
    if E <> 0 then
      Result := 0;
  end;
end;

function RemoveAccelChar(const S: String): String;
var
  I: Integer;
begin
  Result := S;
  I := 1;
  while I <= Length(Result) do begin
    if Result[I] = '&' then begin
      System.Delete(Result, I, 1);
      if I > Length(Result) then
        Break;
    end;
    Inc(I, CharLength(Result, I));
  end;
end;

function GetTextWidth(const DC: HDC; S: String; const Prefix: Boolean): Integer;
{ Returns the width of the specified string using the font currently selected
  into DC. If Prefix is True, it first removes "&" characters as necessary. }
var
  Size: TSize;
begin
  { This procedure is 10x faster than using DrawText with the DT_CALCRECT flag }
  if Prefix then
    S := RemoveAccelChar(S);
  GetTextExtentPoint32(DC, PChar(S), Length(S), Size);
  Result := Size.cx;
end;

function AddPeriod(const S: String): String;
begin
  Result := S;
  if (Result <> '') and (PathLastChar(Result)^ > '.') then
    Result := Result + '.';
end;

function GetPreferredUIFont: String;
{ Gets the preferred UI font. Returns Microsoft Sans Serif, or MS Sans Serif
  if it doesn't exist.
  Microsoft Sans Serif (which is available on Windows 2000 and later) has two
  advantages over MS Sans Serif:
  1) On Windows XP, it can display password dots in edit boxes.
  2) In my tests on Japanese XP, Microsoft Sans Serif can display Japanese
     characters (MS Sans Serif cannot). }
begin
  if FontExists('Microsoft Sans Serif') then
    Result := 'Microsoft Sans Serif'
  else
    Result := 'MS Sans Serif';
end;

function IsWildcard(const Pattern: String): Boolean;
begin
  Result := (Pos('*', Pattern) <> 0) or (Pos('?', Pattern) <> 0);
end;

function WildcardMatch(const Text, Pattern: PChar): Boolean;
type
  TWildcardMatchResult = (wmFalse, wmTrue, wmAbort);

  function InternalWildcardMatch(T, P: PChar): TWildcardMatchResult;
  begin
    while P^ <> #0 do begin
      case P^ of
        '?': ;  { Match any character }
        '*': begin
               Inc(P);
               while P^ = '*' do begin
                 { Consecutive stars act just like one }
                 Inc(P);
               end;
               if P^ = #0 then begin
                 { Trailing star matches everything }
                 Result := wmTrue;
                 Exit;
               end;
               while T^ <> #0 do begin
                 Result := InternalWildcardMatch(T, P);
                 if Result <> wmFalse then
                   Exit;
                 T := CharNext(T);
               end;
               Result := wmAbort;
               Exit;
             end;
      else
        if not CharCompare(T, P) then begin
          Result := wmFalse;
          Exit;
        end;
      end;
      T := CharNext(T);
      P := CharNext(P);
    end;
    if T^ = #0 then
      Result := wmTrue
    else
      Result := wmFalse;
  end;

begin
  Result := (InternalWildcardMatch(Text, Pattern) = wmTrue);
end;

function IntMax(const A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Win32ErrorString(ErrorCode: Integer): String;
{ Like SysErrorMessage but also passes the FORMAT_MESSAGE_IGNORE_INSERTS flag
  which allows the function to succeed on errors like 129 }
var
  Len: Integer;
  Buffer: array[0..1023] of Char;
begin
  Len := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_ARGUMENT_ARRAY, nil,
    ErrorCode, 0, Buffer, SizeOf(Buffer), nil);
  while (Len > 0) and (Buffer[Len - 1] in [#0..#32, '.']) do
    Dec(Len);
  SetString(Result, Buffer, Len);
end;

end.
