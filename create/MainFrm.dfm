object MainForm: TMainForm
  Left = 300
  Top = 138
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'CreatePatch'
  ClientHeight = 270
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblReadme: TLabel
    Left = 8
    Top = 88
    Width = 43
    Height = 13
    Caption = '&Readme:'
    FocusControl = memData
  end
  object edtPatch: TLabeledEdit
    Left = 8
    Top = 24
    Width = 321
    Height = 21
    EditLabel.Width = 48
    EditLabel.Height = 13
    EditLabel.Caption = '&Patch file:'
    TabOrder = 0
    OnChange = edtPatchChange
  end
  object btnBrowse: TButton
    Left = 336
    Top = 24
    Width = 35
    Height = 21
    Caption = '...'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object memData: TMemo
    Left = 8
    Top = 104
    Width = 361
    Height = 129
    ScrollBars = ssVertical
    TabOrder = 3
    OnChange = edtPatchChange
  end
  object btnSave: TButton
    Left = 296
    Top = 240
    Width = 75
    Height = 25
    Caption = '&Create'
    Default = True
    Enabled = False
    TabOrder = 5
    OnClick = btnSaveClick
  end
  object edtTitle: TLabeledEdit
    Left = 8
    Top = 64
    Width = 361
    Height = 21
    EditLabel.Width = 24
    EditLabel.Height = 13
    EditLabel.Caption = '&Title:'
    TabOrder = 2
    OnChange = edtPatchChange
  end
  object btnLoadText: TButton
    Left = 317
    Top = 210
    Width = 36
    Height = 21
    Caption = '...'
    TabOrder = 4
    OnClick = btnLoadTextClick
  end
  object OpenDialog: TOpenDialog
    Filter = 'Difference files (*.dif)|*.dif|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 344
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'exe'
    Filter = 'Executables (*.exe)|*.exe|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 184
    Top = 216
  end
  object OpenText: TOpenDialog
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 344
    Top = 72
  end
end
