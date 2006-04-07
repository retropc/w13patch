program W13Patch;

{$R 'w13.res' 'w13.rc'}

uses
  Kol,
  Windows,
  FormatVersion,
  Classes,
  FileCtrl2,
  SysUtils,
  PatchEngine,
  Common;

type
  TDummy = class
    class procedure btnClick(Sender: PObj);
    class procedure frmResize(Sender: PObj);
    class procedure OnShow(Sender: PObj);
    class procedure OnCantFildFile(const AFile: string; out ADirectory: string; out AContinue: boolean);
  end;
  
const
  CR = #13;
  LF = #10;
  CRLF = CR + LF;

var
  pForm: PControl;
  memText: PControl;
  btnPatch: PControl;
  cbxUndo: PControl;
  pPatch: TPatch;

  FLocation: string;

procedure MessageB(const A: string; const B: integer);
begin
  if Assigned(pForm) then
    MessageBox(pForm.Handle, PAnsiChar(A), 'W13Patch', MB_OK + B)
  else
    MessageBox(0, PAnsiChar(A), 'W13Patch', MB_OK + B);
end;

function LoadData(): boolean;
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
        if cMagic <> 'W13P' + chr(FORMAT_VERSION) then
          raise Exception.Create('Not a valid W13Patch.');
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

        pPatch.LoadFormat(pFile);
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

procedure Cleanup();
begin

end;

class procedure TDummy.btnClick(Sender: PObj);
var
  strExtra: string;
  pEngine: TPatchEngine;
begin
  pEngine := TPatchEngine.Create(pPatch);
  try
    pEngine.OnCantLocateFile := TDummy.OnCantFildFile;

    try
      strExtra := pEngine.Patch(FLocation, cbxUndo.Checked);
    except
      on E: ECancelled do
        exit;

      on E: EPatchApplied do
      begin
        FLocation := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
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
      strExtra := CRLF + CRLF + 'Following features already patched:' + CRLF + strExtra;
    MessageB('Patch complete!' + strExtra, MB_ICONINFORMATION);
  finally
    pEngine.Free;
  end;
end;

class procedure TDummy.OnCantFildFile(const AFile: string; out ADirectory: string; out AContinue: boolean);
begin
  AContinue := SelectDirectory(pForm.Handle, 'Select directory that contains: ' + ExtractFileName(AFile), '', ADirectory);
end;

procedure SetAlignments(const AFirst: boolean = false);
const
  GAP = 6;
begin
  memText.Left := 0;
  if AFirst then
  begin
    pForm.ClientHeight := 200;
    pForm.Width := 600;
  end;
  memText.Height := pForm.ClientHeight - btnPatch.Height - GAP;
  memText.Width := pForm.ClientWidth;
  cbxUndo.Top := memText.Height + GAP + (btnPatch.Height - cbxUndo.Height) div 2 - 1;
  cbxUndo.Left := memText.Left + 1;
  btnPatch.Left := pForm.ClientWidth - btnPatch.Width;
  btnPatch.Top := memText.Height + GAP;
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

class procedure TDummy.OnShow(Sender: PObj);
begin
  btnPatch.DefaultBtn := true;
end;

begin
  pForm := NewForm(nil, ' - W13Patch');
  with pForm^ do
  begin
    OnResize := TDummy.frmResize;
    OnShow := TDummy.OnShow;
    Tabulate;
    SupportMnemonics;
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

  pPatch := TPatch.Create;
  try
    if not LoadData() then
      exit;

    Run(pForm);
  finally
    pPatch.Free;
  end;

  Cleanup();
end.
