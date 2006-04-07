unit PatchEngine;

interface

uses
  Kol, SysUtils, Classes, Tokeniser, Windows, Common;
  
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

  TPatch = class
  private
    Files: array of THunkFile;
    Hunks: array of THunk;
  public
{$IFNDEF DontLoad}
    procedure LoadOldFormat(const APatchFile: TStream);
    procedure SaveToStream(const APatchFile: TStream);
{$ENDIF}
    procedure LoadFormat(const APatchFile: TStream);
  end;

  TLocateEvent = procedure(const AFile: string; out ANewPath: string; out AContinue: boolean) of object;

  TPatchEngine = class
  private
    FOnCantLocateFile: TLocateEvent;
    FPatch: TPatch;
    procedure PatchEngine(const AList: TList; const AUndo: boolean; const ALastHunk: PInteger; const ALimit: integer = -1);
    function SelectDirectory(const AFilename: string; var AOutput: string): boolean;
  public
    constructor Create(const APatch: TPatch);
    destructor Destroy; override;
    property OnCantLocateFile: TLocateEvent read FOnCantLocateFile write FOnCantLocateFile;
    function Patch(var ADirectory: string; const AUndo: boolean = false): string;
    property PatchFile: TPatch read FPatch write FPatch;
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

constructor TPatchEngine.Create(const APatch: TPatch);
begin
  FPatch := APatch; 
end;

destructor TPatchEngine.Destroy;
begin
  inherited;
end;

function TPatchEngine.Patch(var ADirectory: string; const AUndo: boolean): string;
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
  strDirectory: string;
  pFiles: TList;
  iFound: integer;
  iCount: integer;
  pFile: TFileStream;
  cByte: char;
  fFirst: boolean;
  fMatch: boolean;
  fAllFileMatch: boolean;
  fAllNil: boolean;
  strFile: string;
  iHunk: integer;
  strComment: string;
  iCount2: integer;
  iLastHunk: integer;
begin
  Result := '';
  strFile := '';
  fAllNil := true;
  iHunk := 0;
  pFile := nil;
  fMatch := false;
  fAllFileMatch := false;

  strDirectory := ADirectory;
  try
    pFiles := TList.Create;
    try
      for iCount := 0 to length(FPatch.Files) - 1 do
      begin
        //if its not the same file as last time, different hunk
        if FPatch.Files[iCount].Filename <> strFile then
        begin
          strFile := FPatch.Files[iCount].Filename;
          if not FileExists(strDirectory + strFile) then
            if iCount = 0 then
            begin

              if not SelectDirectory(ExtractFileName(strFile), strDirectory) then
                raise ECancelled.Create('');
              strDirectory := IncludeTrailingPathDelimiter(strDirectory);
              if not FileExists(strDirectory + ExtractFilename(strFile)) then
                raise Exception.Create('Unable to locate file to patch!');
              iFound := CountString(strFile, '\') - 1;
              while iFound > 0 do
              begin
                strDirectory := strDirectory + '..\';
                dec(iFound);
              end;
            end
            else
              raise Exception.Create('File missing!');

          if fMatch and fAllFileMatch then
          begin
            pFile := pFiles[pFiles.Count - 1];
            for iCount2 := 0 to pFiles.Count -  1do
              if pFiles[iCount2] = pFile then
                pFiles[iCount2] := nil;

            pFile.Free;
          end;
          fAllFileMatch := true;

          pFile := TFileStream.Create(strDirectory + strFile, fmOpenReadWrite or fmShareExclusive);
          try
            pFiles.Add(pFile);
          except
            pFile.Free;
            raise;
          end;

        end
        else // same file
          pFiles.Add(pFile);

        strComment := FPatch.Files[iCount].Comment;
        if FPatch.Files[iCount].Comment = '' then
          strComment := ExtractFilename(strFile);

        // try applying the patch
        fFirst := true;
        fMatch := false;
        while (iHunk < length(FPatch.Hunks)) and (FPatch.Hunks[iHunk].FileIndex = iCount) do
        begin
          with FPatch.Hunks[iHunk] do
          begin
            pFile.Position := Offset;
            pFile.ReadBuffer(cByte, 1);
            if cByte <> LookupOrigValue(FPatch.Hunks[iHunk], AUndo) then
            begin
              if fFirst then
              begin
                fMatch := true;

                if Result = '' then
                  Result := strComment
                else
                  Result := Result + ', ' + strComment;
              end
              else
              begin
                if not fMatch then
                  raise Exception.Create('File does not match patch!');
              end;
            end
            else if fMatch then
               raise Exception.Create('File does not match patch!');
          end;
          inc(iHunk);
          fFirst := false;
        end;
        if not fMatch then
        begin
          fAllNil := false;
          fAllFileMatch := false;
        end;
      end;

      if fMatch and fAllFileMatch then
      begin
        for iCount2 := 0 to pFiles.Count -  1do
          if pFiles[iCount2] = pFile then
            pFiles[iCount2] := nil;

        pFile.Free;
      end;

      if fAllNil then
        raise EPatchApplied.Create('Patch has already been applied.');

      // ok, it checked out, so lets patch!
      iLastHunk := 0;

      try
        PatchEngine(pFiles, AUndo, @iLastHunk);
      except
        PatchEngine(pFiles, not AUndo, nil, iLastHunk);
        raise;
      end;

      ADirectory := strDirectory;
    finally
      pFile := nil;
      while pFiles.Count > 0 do
      begin
        if Assigned(pFiles[0]) and (pFiles[0] <> pFile) then
          TFileStream(pFiles[0]).Free;
        pFile := pFiles[0];
        pFiles.Delete(0);
      end;
      pFiles.Free;
    end;
  except
    raise;
  end;
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

function TPatchEngine.SelectDirectory(const AFilename: string;
  var AOutput: string): boolean;
begin
  Result := false;
  if not Assigned(OnCantLocateFile) then
    exit;
  OnCantLocateFile(AFilename, AOutput, Result);
end;

{ TPatch }

{$IFNDEF DontLoad}
procedure TPatch.LoadOldFormat(const APatchFile: TStream);
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
          iSep := Pos('/', strLine);
          if iSep = 0 then
          begin
            Filename := strLine;
            Comment := '';
          end
          else
          begin
            Filename := Copy(strLine, 0, iSep - 1);
            Comment := Copy(strLine, iSep + 1, length(strLine));
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
end;

procedure TPatch.SaveToStream(const APatchFile: TStream);
var
  iHunk: integer;
  iFile: integer;
  iLength: integer;
  iMarker: integer;
  iMarker2: integer;
  iHunkCount: integer;
begin
  with APatchFile do
  begin
    iLength := length(Files);
    WriteBuffer(iLength, sizeof(integer));
    iLength := length(Hunks);
    WriteBuffer(iLength, sizeof(integer));

    iFile := -1;
    iMarker := 0;

    for iHunk := 0 to length(Hunks) - 1 do
    begin
      with Hunks[iHunk] do
      begin
        if FileIndex <> iFile then
        begin
          if iFile <> -1 then
          begin
            iMarker2 := Position;
            Position := iMarker;
            WriteBuffer(iHunkCount, sizeof(iHunkCount));
            Position := iMarker2;
          end;
          inc(iFile);
          iLength := length(Files[iFile].Filename);
          WriteBuffer(iLength, sizeof(iLength));
          WriteBuffer(Files[iFile].Filename[1], iLength);
          iLength := length(Files[iFile].Comment);
          WriteBuffer(iLength, sizeof(iLength));
          WriteBuffer(Files[iFile].Comment[1], iLength);
          iMarker := APatchFile.Position;
          iHunkCount := 0;
          APatchFile.WriteBuffer(iHunkCount, sizeof(iHunkCount));
        end;
        WriteBuffer(Offset, sizeof(Offset));
        WriteBuffer(OldValue, sizeof(OldValue));
        WriteBuffer(NewValue, sizeof(NewValue));
        inc(iHunkCount);
      end;
    end;
    if iFile <> -1 then
    begin
      iMarker2 := APatchFile.Position;
      Position := iMarker;
      WriteBuffer(iHunkCount, sizeof(iHunkCount));
      Position := iMarker2;
    end;
  end;
end;
{$ENDIF}

procedure TPatch.LoadFormat(const APatchFile: TStream);
var
  iLength: integer;
  iFile: integer;
  iHunk: integer;
  iHunkCount: integer;
  iCount: integer;
begin
  iHunk := 0;

  with APatchFile do
  begin
    ReadBuffer(iLength, sizeof(integer));
    setlength(Files, iLength);
    ReadBuffer(iLength, sizeof(integer));
    setlength(Hunks, iLength);
    for iFile := 0 to length(Files) - 1 do
    begin
      ReadBuffer(iLength, sizeof(iLength));
      setlength(Files[iFile].Filename, iLength);
      ReadBuffer(Files[iFile].Filename[1], iLength);

      ReadBuffer(iLength, sizeof(iLength));
      setlength(Files[iFile].Comment, iLength);
      ReadBuffer(Files[iFile].Comment[1], iLength);

      ReadBuffer(iHunkCount, sizeof(iHunkCount));
      for iCount := 1 to iHunkCount do
      begin
        with Hunks[iHunk] do
        begin
          ReadBuffer(Offset, sizeof(Offset));
          ReadBuffer(OldValue, sizeof(OldValue));
          ReadBuffer(NewValue, sizeof(NewValue));
          FileIndex := iFile;
        end;
        inc(iHunk);
      end;
    end;
  end;
end;

end.
