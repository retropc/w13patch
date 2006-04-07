unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, FormatVersion, PatchEngine;

type
  TMainForm = class(TForm)
    edtPatch: TLabeledEdit;
    btnBrowse: TButton;
    memData: TMemo;
    lblReadme: TLabel;
    btnSave: TButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    edtTitle: TLabeledEdit;
    OpenText: TOpenDialog;
    btnLoadText: TButton;
    procedure btnBrowseClick(Sender: TObject);
    procedure edtPatchChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadTextClick(Sender: TObject);
  private
    FPatch: string;
    procedure GenerateBGR(const AOutStream: TStream);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.btnBrowseClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    edtPatch.Text := OpenDialog.FileName;
    FPatch := '';
  end;
end;

procedure TMainForm.edtPatchChange(Sender: TObject);
begin
  btnSave.Enabled := (edtPatch.Text <> '') and FileExists(edtPatch.Text) and (edtTitle.Text <> '') and (memData.Text <> '');
end;

procedure TMainForm.GenerateBGR(const AOutStream: TStream);
var
  pFile: TFileStream;
  pData: TStringStream;
  iLen: integer;
  strMagic: string;
  strPatch: string;
  pPatch: TPatch;
  pMemStream: TMemoryStream;
begin
  with AOutStream do
  begin
    iLen := length(edtTitle.Text);
    WriteBuffer(iLen, sizeof(iLen));
    WriteBuffer(edtTitle.Text[1], iLen);
    iLen := length(memData.Text);
    WriteBuffer(iLen, sizeof(iLen));
    WriteBuffer(memData.Text[1], iLen);
    pData := TStringStream.Create('');
    try
      pFile := TFileStream.Create(edtPatch.Text, fmOpenRead);
      try
        pFile.Position := pFile.Position + 67;
        iLen := pFile.Size - pFile.Position;
        pData.CopyFrom(pFile, iLen);
      finally
        pFile.Free;
      end;
      strPatch := StringReplace(pData.DataString, #13#10, #10, [rfReplaceAll]);
    finally
      pData.Free;
    end;
    pMemStream := TMemoryStream.Create;
    try
      pPatch := TPatch.Create;
      try
        pData := TStringStream.Create(strPatch);
        try
          pPatch.LoadOldFormat(pData);
        finally
          pData.Free;
        end;
        pPatch.SaveToStream(pMemStream);
      finally
        pPatch.Free;
      end;
      CopyFrom(pMemStream, 0);
    finally
      pMemStream.Free;
    end;
    strMagic := 'W13P' + chr(FORMAT_VERSION);
    WriteBuffer(strMagic[1], length(strMagic));
  end;
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
var
  pStream: TFileStream;
  pRes: TResourceStream;
  iStubSize: integer;
begin
  if not SaveDialog.Execute then
    exit;
  pStream := TFileStream.Create(SaveDialog.Filename, fmCreate);
  try
    pRes := TResourceStream.Create(hInstance, 'STUB', 'DATA');
    try
      pStream.CopyFrom(pRes, 0);
      iStubSize := pStream.Position;
    finally
      pRes.Free;
    end;
    GenerateBGR(pStream);
    iStubSize := pStream.Position - iStubSize;
    pStream.WriteBuffer(iStubSize, sizeof(iStubSize));
  finally
    pStream.Free;
  end;
end;

procedure TMainForm.btnLoadTextClick(Sender: TObject);
begin
  if OpenText.Execute then
    memData.Lines.LoadFromFile(OpenText.Filename);

end;

end.
