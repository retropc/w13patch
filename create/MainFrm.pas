unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, FormatVersion;

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
    btnLoadText: TButton;
    OpenText: TOpenDialog;
    procedure btnBrowseClick(Sender: TObject);
    procedure edtPatchChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLoadTextClick(Sender: TObject);
  private
    FPatch: string;
    FModule: integer;
    procedure GenerateBGR(const AOutStream: TStream);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Compress, LZMA;
  
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
  pCompressor: TCompressedBlockWriter;
  pFile: TFileStream;
  pData: TStringStream;
  iLen: integer;
  strMagic: string;
begin
  with AOutStream do
  begin
    iLen := length(edtTitle.Text);
    WriteBuffer(iLen, sizeof(iLen));
    WriteBuffer(edtTitle.Text[1], iLen);
    iLen := length(memData.Text);
    WriteBuffer(iLen, sizeof(iLen));
    WriteBuffer(memData.Text[1], iLen);
    pCompressor := TCompressedBlockWriter.Create(AOutStream, TLZMACompressor, clLZMAUltra, nil);
    try
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
        strMagic := StringReplace(pData.DataString, #13#10, #10, [rfReplaceAll]);
      finally
        pData.Free;
      end;
      pData := TStringStream.Create(strMagic);
      try
        iLen := pData.Size;
        WriteBuffer(iLen, sizeof(iLen));
        pCompressor.CopyFrom(pData, 0);
      finally
        pData.Free;
      end;
    finally
      pCompressor.Free;
    end;
    strMagic := 'BDGR' + chr(FORMAT_VERSION);
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

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FModule := LoadLibrary('lzma.dll');
  if FModule = 0 then
    raise Exception.Create('Unable to load LZMA');
  LZMAInitCompressFunctions(FModule);
end;

procedure TMainForm.btnLoadTextClick(Sender: TObject);
begin
  if OpenText.Execute then
    memData.Lines.LoadFromFile(OpenText.Filename);

end;

end.
