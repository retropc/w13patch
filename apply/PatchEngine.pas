unit PatchEngine;

interface

uses
  SysUtils, Classes, Tokeniser, Windows;
  
type
  THunkFile = packed record
    Filename: string;
    Comment: string;
  end;

  THunk = packed record
    Offset: DWORD;
    FileIndex: word;
    OldValue: char;
    NewValue: char;
  end;

  TPatch = class(TPersistent)
  private
  public
    FFiles: array of THunkFile;
    FFileCount: integer;
    FHunks: array of THunk;
    FHunkCount: integer;
    constructor Create(const APatchFile: TStream);
    destructor Destroy; override;
  end;

  TPatchEngine = class
  private
  public
    FPatch: TPatch;
    constructor Create(const APatchFile: TStream);
    destructor Destroy; override;
  end;

implementation

{ TPatchEngine }

constructor TPatchEngine.Create(const APatchFile: TStream);
begin
  FPatch := TPatch.Create(APatchFile);
end;

destructor TPatchEngine.Destroy;
begin
  FPatch.Free;
  
  inherited;
end;

{ TPatch }

constructor TPatch.Create(const APatchFile: TStream);
function IsFilename(const AData: string): boolean;
begin
  Result := Copy(ADAta, 9, 1) <> ':';
end;
function IsValidFilename(const AData: string): boolean;
begin
  Result := Pos('../', AData) = 0;
end;
function CountString(const AData: string; const AWhat: char): integer;
var
  iCount: integer;
begin
  Result := 0;
  for iCount := 0 to length(AData) - 1 do
    if AData[iCount] = AWhat then
      inc(Result);
end;
var
  pTokens: TTokenList;
  strLine: string;
  iCount: integer;
  iSep: integer;
begin
  if APatchFile is TStringStream then
  begin
    pTokens := TTokenList.Create;
    try
      pTokens.Tokenise(TStringStream(APatchFile).DataString, #10);
      setlength(FFiles, pTokens.Count);
      setlength(FHunks, pTokens.Count);
      for iCount := 0 to pTokens.Count - 1 do
      begin
        strLine := pTokens[iCount];
        if IsFilename(strLine) then
        begin
          with FFiles[FFileCount] do
          begin
            iSep := Pos(strLine, '/');
            if iSep = 0 then
            begin
              Filename := strLine;
              Comment := '';
            end
            else
            begin
              Filename := Copy(strLine, 0, iSep - 1);
              Comment := Copy(strLine, iSep, length(strLine));
            end;
            if Filename[1] = '\' then
              Filename := Copy(Filename, 1, length(Filename));
            if not IsValidFilename(Filename) then
              raise Exception.Create('Patch contains invalid filenames!');
          end;
          inc(FFileCount);
        end
        else
        begin
          with FHunks[FHunkCount] do
          begin
            FileIndex := FFileCount - 1;
            Offset := StrToInt('$' + Copy(strLine, 0, 8));
            OldValue := chr(StrToInt('$' + Copy(strLine, 11, 2)));
            NewValue := chr(StrToInt('$' + Copy(strLine, 14, 2)));
          end;
          inc(FHunkCount);   
        end;
      end;
      setlength(FFiles, FFileCount);
      setlength(FHunks, FHunkCount);
    finally
      pTokens.Free;
    end;
  end
  else
    raise Exception.Create('Unknown patch format.');
end;

destructor TPatch.Destroy;
begin
  // not required
  inherited;
end;

end.
