program BadgerPatch;

{$R 'badger.res' 'badger.rc'}

uses
  Kol, Windows, Tokeniser, FormatVersion, Classes, FileCtrl2, SysUtils;

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

function Patch(const AData: TTokenList; const AUndo: boolean; var AStartDirectory: string): string;
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
  iLocation: integer;
  iFindPos: integer;
  iRepPos: integer;
  cByte: char;
  iValue: integer;
  fFirst: boolean;
  fMatch: boolean;
  strFilename: string;
  iAt: integer;
  fAllNil: boolean;
begin
  fFirst := true;
  fMatch := false;
  Result := '';
  strFilename := '';
  fAllNil := true;

  if AUndo then
  begin
    iFindPos := 14;
    iRepPos := 11;
  end
  else
  begin
    iFindPos := 11;
    iRepPos := 14;
  end;
  strDirectory := AStartDirectory;
  pFile := nil;
  try
    pFiles := TList.Create;
    try
      for iCount := 0 to AData.Count - 1 do
      begin
        if IsFilename(AData[iCount]) then
        begin
          if AData[iCount] <> strFilename then
          begin
            if fMatch then
            begin
              TFileStream(pFiles[pFiles.Count - 1]).Free;
              pFiles[pFiles.Count - 1] := nil;
            end;
            strFilename := ExtractFileName(AData[iCount]);
            if AData[iCount][1] = '\' then
              AData[iCount] := Copy(AData[iCount], 1, length(AData[iCount]));
            if not IsValidFilename(AData[iCount]) then
              raise Exception.Create('Patch contains invalid filenames!');
            if not FileExists(strDirectory + AData[iCount]) then
              if iCount = 0 then
              begin
                if not SelectDirectory(pForm.Handle, 'Select directory that contains: ' + ExtractFileName(strFilename), '', strDirectory) then
                  raise Exception.Create('Unable to locate file to patch!');
                strDirectory := IncludeTrailingPathDelimiter(strDirectory);
                if not FileExists(strDirectory + ExtractFilename(AData[iCount])) then
                  raise Exception.Create('Unable to locate file to patch!');
                iFound := CountString(AData[iCount], '\') - 1;
                while iFound > 0 do
                begin
                  strDirectory := strDirectory + '..\';
                  dec(iFound);
                end;
              end;
            pFile := TFileStream.Create(strDirectory + AData[iCount], fmOpenReadWrite or fmShareExclusive);
            try
              pFiles.Add(pFile);
            except
              pFile.Free;
              raise;
            end;
          end;
          fFirst := true;
          fMatch := false;
        end
        else
        begin
          iLocation := StrToInt('$' + Copy(AData[iCount], 0, 8));
          pFile.Position := iLocation;
          pFile.ReadBuffer(cByte, 1);
          iValue := StrToInt('$' + Copy(AData[iCount], iFindPos, 2));
          if iValue <> ord(cByte) then
          begin
            if not fFirst then
            begin
              if not fMatch then
                raise Exception.Create('File does not match patch!');
            end
            else
            begin
              fMatch := true;
              if Result = '' then
                Result := strFilename
              else
                Result := Result + ', ' + strFilename;
            end
          end
          else
            if fMatch then
              raise Exception.Create('File does not match patch!');        
          fFirst := false;
        end;
      end;

      if fMatch then
      begin
        TFileStream(pFiles[pFiles.Count - 1]).Free;
        pFiles[pFiles.Count - 1] := nil;
      end;

      iFound := 0;
      try
        strFilename := '';
        iAt := -1;

        for iCount := 0 to AData.Count - 1 do
        begin
          inc(iFound);
          if IsFilename(AData[iCount]) then
          begin
            if strFilename <> AData[iCount] then
              inc(iAt);

            pFile := TFileStream(pFiles[iAt]);
            strFilename := AData[iCount];
          end
          else
          begin
            if not Assigned(pFile) then
              continue;

            fAllNil := false;
            iLocation := StrToInt('$' + Copy(AData[iCount], 0, 8));
            pFile.Position := iLocation;
            cByte := chr(StrToInt('$' + Copy(AData[iCount], iRepPos, 2)));
            pFile.WriteBuffer(cByte, 1);
          end;
        end;
      except
        if iFound > 0 then
        begin
          iAt := -1;
          strFilename := '';

          for iCount := 0 to iFound do
            if IsFilename(AData[iCount]) then
            begin
              if strFilename <> AData[iCount] then
                inc(iAt);

              pFile := TFileStream(pFiles[iAt]);
              strFilename := AData[iCount];
            end
            else
            begin
              if not Assigned(pFile) then
                continue;
              iLocation := StrToInt('$' + Copy(AData[iCount], 0, 8));
              pFile.Position := iLocation;
              cByte := chr(StrToInt('$' + Copy(AData[iCount], iFindPos, 2)));
              pFile.WriteBuffer(cByte, 1);
            end;
          end;
        raise;
      end;
      AStartDirectory := strDirectory;
    finally
      while pFiles.Count > 0 do
      begin
        TFileStream(pFiles[0]).Free;
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
  pTokens: TTokenList;
  strExtra: string;
begin
  if FCompressed is TStringStream then
  begin
    pTokens := TTokenList.Create;
    try
      pTokens.Tokenise(TStringStream(FCompressed).DataString, #10);
    except
      on E: Exception do
      begin
        pTokens.Free;
        MessageB(E.Message, MB_ICONWARNING);
        exit;
      end;
    end;
    FCompressed.Free;
    FCompressed := pTokens;
  end;
  if FCompressed is TTokenList then
  begin
    try
      strExtra := Patch(FCompressed as TTokenList, cbxUndo.Checked, FLocation);
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
