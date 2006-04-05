unit PathFunc;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  This unit provides some path-related, MBCS-aware functions.

  These functions should always be used in lieu of their SysUtils counterparts
  since they aren't MBCS-aware on Delphi 2, and sometimes not MBCS-aware on
  Delphi 6 and 7 either (see QC#5096).

  $jrsoftware: issrc/Components/PathFunc.pas,v 1.17 2004/03/19 00:19:49 jr Exp $
}

interface

function AddBackslash(const S: String): String;
function CharLength(const S: String; const Index: Integer): Integer;
function CharCompare(const S1, S2: PChar): Boolean;
function PathChangeExt(const Filename, Extension: String): String;
function PathCompare(const S1, S2: String): Integer;
function PathDrivePartLength(const Filename: String): Integer;
function PathExtractDir(const Filename: String): String;
function PathExtractDrive(const Filename: String): String;
function PathExtractExt(const Filename: String): String;
function PathExtractName(const Filename: String): String;
function PathExtractPath(const Filename: String): String;
function PathLastChar(const S: String): PChar;
function PathLastDelimiter(const Delimiters, S: string): Integer;
function PathLowercase(const S: String): String;
function PathPos(Ch: Char; const S: String): Integer;
function PathRemoveExtraBackslashes(const S: String): String;
function PathStrScan(const S: PChar; const C: Char): PChar;
function RemoveBackslash(const S: String): String;
function RemoveBackslashUnlessRoot(const S: String): String;

implementation

uses
  Windows, SysUtils;

{
  Some notes:
  1. Whenever possible I try to use CharNext() and friends instead of checking
     for lead bytes, just in case these functions or Windows itself support
     UTF-8 some day.
}

function AddBackslash(const S: String): String;
{ Returns S plus a trailing backslash, unless S is an empty string or already
  ends in a backslash. }
begin
  Result := S;
  if (Result <> '') and (PathLastChar(Result)^ <> '\') then
    Result := Result + '\';
end;

function CharLength(const S: String; const Index: Integer): Integer;
{ Returns the length of the character at Index in S.
  Note: Nulls are treated as characters with a length of 1. }
var
  P: PChar;
begin
  P := @S[Index];
  if P^ = #0 then
    Result := 1
  else
    Result := CharNext(P) - P;
end;

function CharCompare(const S1, S2: PChar): Boolean;
{ Compares two first characters, and returns True if they are equal. }
var
  N, I: Integer;
begin
  N := CharNext(S1) - S1;
  if N = CharNext(S2) - S2 then begin
    for I := 0 to N-1 do begin
      if S1[I] <> S2[I] then begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end else
    Result := False;
end;

function PathChangeExt(const Filename, Extension: String): String;
{ Takes Filename, removes any existing extension, then adds the extension
  specified by Extension and returns the resulting string. }
var
  I: Integer;
begin
  I := PathLastDelimiter('.\:', Filename);
  if (I = 0) or (Filename[I] <> '.') then
    I := Maxint;
  Result := Copy(Filename, 1, I - 1) + Extension;
end;

function PathCompare(const S1, S2: String): Integer;
{ Compares two filenames, and returns 0 if they are equal. }
begin
  Result := AnsiCompareStr(PathLowercase(S1), PathLowercase(S2));
end;

function PathDrivePartLength(const Filename: String): Integer;
{ Returns length of the drive portion of Filename (either 'x:' or
  '\\server\share'), or 0 if there is no drive portion.
  Note: This is MBCS-safe, unlike the Delphi's ExtractFileDrive function.
  (Computer and share names can include multi-byte characters!) }
var
  Len, I, C: Integer;
begin
  Len := Length(Filename);

  { x: }
  if Len > 0 then begin
    I := CharLength(Filename, 1) + 1;
    if (Len >= I) and (Filename[I] = ':') then begin
      Result := I;
      Exit;
    end;
  end;

  { \\server\share }
  if (Len >= 2) and (Filename[1] = '\') and (Filename[2] = '\') then begin
    I := 3;
    C := 0;
    while I <= Len do begin
      if Filename[I] = '\' then begin
        Inc(C);
        if C >= 2 then
          Break;
      end;
      Inc(I, CharLength(Filename, I));
    end;
    Result := I - 1;
    Exit;
  end;

  Result := 0;
end;

function PathExtractDir(const Filename: String): String;
{ Like PathExtractPath, but strips any trailing backslash, unless the resulting
  path is the root directory of a drive (i.e. 'C:\' or '\'). }
var
  I: Integer;
begin
  I := PathLastDelimiter('\:', Filename);
  if (I > 1) and (Filename[I] = '\') and
     not (CharPrev(Pointer(Filename), @Filename[I])^ in ['\', ':']) then
    Dec(I);
  Result := Copy(FileName, 1, I);
end;

function PathExtractDrive(const Filename: String): String;
{ Returns the drive portion of Filename (either 'x:' or '\\server\share'),
  or an empty string if there is no drive portion. }
var
  L: Integer;
begin
  L := PathDrivePartLength(Filename);
  if L = 0 then
    Result := ''
  else
    Result := Copy(Filename, 1, L);
end;

function PathExtractExt(const Filename: String): String;
{ Returns the extension portion of the last component of Filename (e.g. '.txt')
  or an empty string if there is no extension. }
var
  I: Integer;
begin
  I := PathLastDelimiter('.\:', Filename);
  if (I > 0) and (Filename[I] = '.') then
    Result := Copy(Filename, I, Maxint)
  else
    Result := '';
end;

function PathExtractName(const Filename: String): String;
{ Returns the filename portion of Filename (e.g. 'filename.txt'). If Filename
  ends in a backslash or colon, the result will be an empty string.
  This function is essentially the opposite of PathExtractPath. }
var
  I: Integer;
begin
  I := PathLastDelimiter('\:', Filename);
  Result := Copy(Filename, I + 1, Maxint);
end;

function PathExtractPath(const Filename: String): String;
{ Returns the path portion of Filename (e.g. 'c:\dir\'). If Filename contains
  no backslash or colon, the result will be an empty string.
  This function is essentially the opposite of PathExtractName. }
var
  I: Integer;
begin
  I := PathLastDelimiter('\:', Filename);
  Result := Copy(Filename, 1, I);
end;

function PathLastChar(const S: String): PChar;
{ Returns pointer to last character in the string. Is MBCS-aware. Returns nil
  if the string is empty. }
begin
  if S = '' then
    Result := nil
  else
    Result := CharPrev(Pointer(S), @S[Length(S)+1]);
end;

function PathLastDelimiter(const Delimiters, S: string): Integer;
{ Returns the index of the last occurrence in S of one of the characters in
  Delimiters, or 0 if none were found.
  Note: S is allowed to contain null characters. }
var
  P, E: PChar;
begin
  Result := 0;
  if (S = '') or (Delimiters = '') then
    Exit;
  P := Pointer(S);
  E := @P[Length(S)];
  while P < E do begin
    if P^ <> #0 then begin
      if StrScan(Pointer(Delimiters), P^) <> nil then
        Result := (P - Pointer(S)) + 1;
      P := CharNext(P);
    end
    else
      Inc(P);
  end;
end;

function PathLowercase(const S: String): String;
{ Converts the specified path name to lowercase }
var
  I, L: Integer;
begin
  if (Win32Platform <> VER_PLATFORM_WIN32_NT) and
     (GetSystemMetrics(SM_DBCSENABLED) <> 0) then begin
    { Japanese Windows 98's handling of double-byte Roman characters in
      filenames is case sensitive, so we can't change the case of double-byte
      characters. (Japanese Windows NT/2000 is case insensitive.) Based on code
      from AnsiLowerCaseFileName. }
    Result := S;
    L := Length(Result);
    I := 1;
    while I <= L do begin
      if Result[I] in ['A'..'Z'] then begin
        Inc(Byte(Result[I]), 32);
        Inc(I);
      end
      else
        Inc(I, CharLength(Result, I));
    end;
  end
  else
    Result := AnsiLowerCase(S);
end;

function PathPos(Ch: Char; const S: String): Integer;
{ This is an MBCS-aware Pos function.
  Note: This function cannot search past null characters. }
var
  P: PChar;
begin
  Result := 0;
  if S <> '' then begin
    P := PathStrScan(Pointer(S), Ch);
    if P <> nil then
      Result := (P - Pointer(S)) + 1;
  end;
end;

function PathRemoveExtraBackslashes(const S: String): String;
{ Returns S minus any superfluous backslashes. For example, if S is
  'C:\\\some\\path', it returns 'C:\some\path'. Does not remove a double
  backslash at the beginning of the string, since that signifies a UNC path. }  
var
  I: Integer;
begin
  Result := S;
  I := 1;
  while I < Length(Result) do begin
    if (Result[I] = '\') and (Result[I+1] = '\') and (I > 1) then
      Delete(Result, I+1, 1)
    else
      Inc(I, CharLength(Result, I));
  end;
end;

function PathStrScan(const S: PChar; const C: Char): PChar;
{ Returns pointer to first occurrence of C in S, or nil if there are no
  occurrences. Like StrScan, but MBCS-aware.
  Note: As with StrScan, specifying #0 for the search character is legal. }
begin
  Result := S;
  while Result^ <> C do begin
    if Result^ = #0 then begin
      Result := nil;
      Break;
    end;
    Result := CharNext(Result);
  end;
end;

function RemoveBackslash(const S: String): String;
{ Removes the trailing backslash from the string, if one exists }
begin
  Result := S;
  if (Result <> '') and (PathLastChar(Result)^ = '\') then
    SetLength(Result, Length(Result)-1);
end;

function RemoveBackslashUnlessRoot(const S: String): String;
{ Removes the trailing backslash from the string, if one exists and does
  not specify a root directory of a drive (i.e. 'C:\' or '\') }
var
  L: Integer;
  P: PChar;
begin
  Result := S;
  L := Length(Result);
  if L < 2 then
    Exit;
  P := CharPrev(Pointer(S), @S[L+1]);
  if (P^ = '\') and (CharPrev(Pointer(S), P)^ <> ':') then
    SetLength(Result, L-1);
end;

end.
