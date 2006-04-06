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
    Files: array of THunkFile;
    Hunks: array of THunk;
    constructor Create(const APatchFile: TStream);
    destructor Destroy; override;
  end;

  TLocateEvent = procedure(const AFile: string; var ANewPath: string; out AContinue: boolean) of object;

  TPatchEngine = class
  private
    FOnCantLocateFile: TLocateEvent;
  public
    FPatch: TPatch;
    procedure PatchEngine(const AList: TList; const AUndo: boolean; const ALastHunk: PInteger; const ALimit: integer = -1);
    constructor Create(const APatchFile: TStream);
    destructor Destroy; override;
    property OnCantLocateFile: TLocateEvent read FOnCantLocateFile write FOnCantLocateFile;
    procedure Patch(const ADirectory: string; const AUndo: boolean = false);
  end;

implementation

function LookupOrigValue(const AHunk: THunk; const AUndo: boolean): char;
begin
  if AUndo then
    Result := AHunk.NewValue
  else
    Result := AHunk.OldValue;
end;
function LookupNewValue(const AHunk: THunk; const AUndo: boolean): char;
begin
  if not AUndo then
    Result := AHunk.NewValue
  else
    Result := AHunk.OldValue;
end;

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

procedure TPatchEngine.Patch(const ADirectory: string;
  const AUndo: boolean);
begin

end;

procedure TPatchEngine.PatchEngine(const AList: TList;
  const AUndo: boolean; const ALastHunk: PInteger; const ALimit: integer = -1);
var
  iHunk: integer;
  iLimit: integer;
  iLastFile: integer;
  pFile: TFileStream;
  cByte: char;
begin
  pFile := nil;

  iLimit := ALimit;
  if iLimit = -1 then
    iLimit := length(FPatch.Hunks) - 1;

  iLastFile := -1;
  for iHunk := 0 to iLimit do
  begin
    if Assigned(ALastHunk) then
      ALastHunk^ := iHunk;
    with FPatch.Hunks[iHunk] do
    begin
      if FileIndex <> iLastFile then
      begin
        inc(iLastFile);
        pFile := AList[iLastFile];
      end;
      if not Assigned(pFile) then
        continue;

      pFile.Position := Offset;
      cByte := LookupNewValue(FPatch.Hunks[iHunk], AUndo);
      pFile.WriteBuffer(cByte, 1);
    end;
  end;
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
  iHunkCount: integer;
  iFileCount: integer;
begin
  iHunkCount := 0;
  iFileCount := 0;
  if APatchFile is TStringStream then
  begin
    pTokens := TTokenList.Create;
    try
      pTokens.Tokenise(TStringStream(APatchFile).DataString, #10);
      setlength(Files, pTokens.Count);
      setlength(Hunks, pTokens.Count);
      for iCount := 0 to pTokens.Count - 1 do
      begin
        strLine := pTokens[iCount];
        if IsFilename(strLine) then
        begin
          with Files[iFileCount] do
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
          inc(iFileCount);
        end
        else
        begin
          with Hunks[iHunkCount] do
          begin
            FileIndex := iFileCount - 1;
            Offset := StrToInt('$' + Copy(strLine, 0, 8));
            OldValue := chr(StrToInt('$' + Copy(strLine, 11, 2)));
            NewValue := chr(StrToInt('$' + Copy(strLine, 14, 2)));
          end;
          inc(iHunkCount);   
        end;
      end;
      setlength(Files, iFileCount);
      setlength(Hunks, iHunkCount);
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
