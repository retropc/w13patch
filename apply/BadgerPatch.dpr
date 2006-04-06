program BadgerPatch;

{$R 'badger.res' 'badger.rc'}

uses
  Kol,
  Windows,
  FormatVersion,
  Classes,
  FileCtrl2,
  SysUtils,
  PatchEngine;
  
type
  EPatchApplied = class(Exception);
  TDummy = class
    class procedure btnClick(Sender: PObj);
    class procedure frmResize(Sender: PObj);
    class procedure OnShow(Sender: PObj);
  end;
const
  CR = #13;
  LF = #10;
  CRLF = CR + LF;

var
  pForm: PControl;
  pnlBadger: PControl;
  memText: PControl;
  btnPatch: PControl;
  cbxUndo: PControl;
  pBadgerImage: PImageList;
  pEngine: TPatchEngine;

  FLocation: string;
  FCompressedSize: integer;
  FCompressed: TObject;

procedure MessageB(const A: string; const B: integer);
begin
  if Assigned(pForm) then
    MessageBox(pForm.Handle, PAnsiChar(A), 'BadgerPatch', MB_OK + B)
  else
    MessageBox(0, PAnsiChar(A), 'BadgerPatch', MB_OK + B);
end;

function LoadData(Sender: TObject): boolean;
var
  strFilename: string;
  cMagic: array[1..5] of char;
  iLen: integer;
  strData: string;
  pFile: TFileStream;
begin
  Result := false;
  FLocation := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
  if (ParamCount > 0) and FileExists(ParamStr(1)) then
    strFilename := ParamStr(1)
  else
    strFilename := ParamStr(0);
  try
    pFile := TFileStream.Create(strFilename, fmOpenRead or fmShareDenyNone);
    with pFile do
      try
        Position := Size - 9;
        ReadBuffer(cMagic, sizeof(cMagic));
        if cMagic <> 'BDGR' + chr(FORMAT_VERSION) then
          raise Exception.Create('Not a BadgerPatch.');
        ReadBuffer(iLen, sizeof(iLen));
        Position := Size - iLen - 4;
        ReadBuffer(iLen, sizeof(iLen));
        setlength(strData, iLen);
        ReadBuffer(strData[1], iLen);
        pForm.Caption := strData + pForm.Caption;
        ReadBuffer(iLen, sizeof(iLen));
        setlength(strData, iLen);
        ReadBuffer(strData[1], iLen);
        memText.Text := strData;
        ReadBuffer(iLen, sizeof(iLen));
        FCompressedSize := iLen;
        iLen := Size - Position - 9;
        FCompressed := TStringStream.Create('');
        (FCompressed as TStream).CopyFrom(pFile, iLen);
        (FCompressed as TStream).Position := 0;
      finally
        pFile.Free;
      end;
  except
    on E: Exception do
    begin
      MessageB(E.Message, MB_ICONERROR);
      exit;
    end;
  end;
  Result := true;
end;

procedure Cleanup(Sender: TObject);
begin
  if Assigned(FCompressed) then
    FCompressed.Free;
end;

function Patch(const AUndo: boolean; var AStartDirectory: string): string;
function IsFilename(const AData: string): boolean;
begin
  Result := Copy(ADAta, 9, 1) <> ':';
end;
function IsValidFilename(const AData: string): boolean;
begin                                  
  Result := Pos('../', AData) = 0;
end;
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
  iLastFile: integer;
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

  strDirectory := AStartDirectory;
  try
    pFiles := TList.Create;
    try
      for iCount := 0 to pEngine.FPatch.FFileCount - 1 do
      begin
        //if its not the same file as last time, different hunk
        if pEngine.FPatch.FFiles[iCount].Filename <> strFile then
        begin
          strFile := pEngine.FPatch.FFiles[iCount].Filename;
          if not FileExists(strDirectory + strFile) then
            if iCount = 0 then
            begin
              if not SelectDirectory(pForm.Handle, 'Select directory that contains: ' + ExtractFileName(strFile), '', strDirectory) then
                raise Exception.Create('Unable to locate file to patch!');
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

        strComment := pEngine.FPatch.FFiles[iCount].Comment;
        if pEngine.FPatch.FFiles[iCount].Comment = '' then
          strComment := ExtractFilename(strFile);

        // try applying the patch
        fFirst := true;
        fMatch := false;
        while (iHunk < pEngine.FPatch.FHunkCount) and (pEngine.FPatch.FHunks[iHunk].FileIndex = iCount) do
        begin
          with pEngine.FPatch.FHunks[iHunk] do
          begin
            pFile.Position := Offset;
            pFile.ReadBuffer(cByte, 1);
            if cByte <> LookupOrigValue(pEngine.FPatch.FHunks[iHunk], AUndo) then
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
        exit;
        
      // ok, it checked out, so lets patch!
      iLastHunk := 0;
      
      try
        iLastFile := -1;
        for iHunk := 0 to pEngine.FPatch.FHunkCount - 1 do
        begin
          iLastHunk := iHunk;
          with pEngine.FPatch.FHunks[iHunk] do
          begin
            if FileIndex <> iLastFile then
            begin
              inc(iLastFile);
              pFile := pFiles[iLastFile];
            end;
            if not Assigned(pFile) then
              continue;

            pFile.Position := Offset;
            cByte := LookupNewValue(pEngine.FPatch.FHunks[iHunk], AUndo);
            pFile.WriteBuffer(cByte, 1);
          end;
        end;
      except
        iLastFile := -1;
        for iHunk := 0 to iLastHunk do
        begin
          with pEngine.FPatch.FHunks[iHunk] do
          begin
            if FileIndex <> iLastFile then
            begin
              inc(iLastFile);
              pFile := pFiles[iLastFile];
            end;
            if not Assigned(pFile) then
              continue;

            pFile.Position := Offset;
            cByte := LookupOrigValue(pEngine.FPatch.FHunks[iHunk], AUndo);
            pFile.WriteBuffer(cByte, 1);
          end;
        end;

        raise;
      end;

      AStartDirectory := strDirectory;
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
  if fAllNil then
    raise EPatchApplied.Create('Patch has already been applied.'); 
end;

class procedure TDummy.btnClick(Sender: PObj);
var
  strExtra: string;
begin
  if not Assigned(pEngine) then
    pEngine := TPatchEngine.Create(FCompressed as TStream);

  try
    strExtra := Patch(cbxUndo.Checked, FLocation);
  except
    on E: EPatchApplied do
    begin
      if cbxUndo.Checked then
        MessageB('Patch has not been applied.', MB_ICONWARNING)
      else
        MessageB(E.Message, MB_ICONINFORMATION);
      exit;
    end;
    on E: Exception do
    begin
      FLocation := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
      MessageB(E.Message, MB_ICONWARNING);
      exit;
    end;
  end;
  if strExtra <> '' then
    strExtra := CRLF + CRLF + 'Following files were already patched:' + CRLF + strExtra;
  MessageB('Patch complete!' + strExtra, MB_ICONINFORMATION);
end;

procedure SetAlignments(const AFirst: boolean = false);
const
  GAP = 6;
begin
  memText.Left := pnlBadger.Width + GAP;
  if AFirst then
  begin
    pForm.ClientHeight := pnlBadger.Height + 4;
    pForm.Width := 600;
  end;
  memText.Height := pForm.ClientHeight - btnPatch.Height - 8;
  memText.Width := pForm.ClientWidth - pnlBadger.Width - GAP - 2;
  cbxUndo.Top := memText.Height + 5;
  cbxUndo.Left := memText.Left;
  btnPatch.Left := pForm.ClientWidth - btnPatch.Width - 2;
  btnPatch.Top := cbxUndo.Top + 1;
  cbxUndo.Invalidate;
  btnPatch.Invalidate;
end;

procedure SetFont(const AControl: PControl);
begin
  AControl.Font.FontName := 'Tahoma';
  AControl.Font.FontHeight := 13;
end;

class procedure TDummy.frmResize(Sender: PObj);
begin
  SetAlignments;
end;

var
  pBitmap: Cardinal;

class procedure TDummy.OnShow(Sender: PObj);
begin
  btnPatch.DefaultBtn := true;
end;

begin
  pForm := NewForm(nil, ' - BadgerPatch');
  with pForm^ do
  begin
    OnResize := TDummy.frmResize;
    OnShow := TDummy.OnShow;
    Tabulate;
    SupportMnemonics;
  end;

  pBitmap := LoadBitmap(hInstance, 'badger');
  pBadgerImage := NewImageList(pForm);
  if pBitmap <> 0 then
    with pBadgerImage^ do
    begin
      imgWidth := 214;
      ImgHeight := 152;
      Colors := ilcColor24;
      Add(pBitmap, 0);
    end;

  pnlBadger := NewImageShow(pForm, pBadgerImage, 0);
  with pnlBadger^ do
  begin
    Width := 214;
    Height := 152;
  end;

  memText := NewEditbox(pForm, [eoMultiline, eoReadOnly, eoNoHScroll]);
  SetFont(memText);
  with memText^ do
  begin
    TabOrder := 2;
  end;

  cbxUndo := NewCheckbox(pForm, '&Undo');
  SetFont(cbxUndo);
  with cbxUndo^ do
  begin
    TabOrder := 1;
  end;

  btnPatch := NewButton(pForm, '&Patch');
  SetFont(btnPatch);
  with btnPatch^ do
  begin
    Font.FontName := 'Tahoma';
    Font.FontHeight := 14;
    OnClick := TDummy.btnClick;
    Focused := true;
    DefaultBtn := true;
    TabOrder := 0;
  end;

  SetAlignments(true);
  with pForm^ do
  begin
    Left := (GetSystemMetrics(SM_CXSCREEN) - Width) div 2;
    Top := (GetSystemMetrics(SM_CYSCREEN) - Height) div 2;
    MinWidth := Width;
    MinHeight := Height;
  end;
  if not LoadData(nil) then
    exit;

  Run(pForm);
  Cleanup(nil);
  if pBitmap <> 0 then
    DeleteObject(pBitmap);
end.
