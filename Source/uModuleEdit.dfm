object ModuleForm: TModuleForm
  Left = 0
  Top = 0
  Caption = 'Add/Edit module'
  ClientHeight = 388
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    512
    388)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 24
    Top = 13
    Width = 107
    Height = 13
    Caption = 'Module/program name'
  end
  object Bevel1: TBevel
    Left = 24
    Top = 345
    Width = 480
    Height = 7
    Anchors = [akLeft, akBottom]
    Shape = bsTopLine
    ExplicitTop = 167
  end
  object bSearch: TSpeedButton
    Left = 479
    Top = 31
    Width = 23
    Height = 22
    Caption = '...'
    OnClick = bSearchClick
  end
  object eParameters: TLabeledEdit
    Left = 24
    Top = 80
    Width = 480
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 55
    EditLabel.Height = 13
    EditLabel.Caption = 'Parameters'
    TabOrder = 0
    OnChange = eParametersChange
  end
  object eInstanceNumber: TLabeledEdit
    Left = 24
    Top = 314
    Width = 121
    Height = 21
    Anchors = [akLeft, akBottom]
    EditLabel.Width = 82
    EditLabel.Height = 13
    EditLabel.Caption = 'Instance Number'
    TabOrder = 1
    Text = '1'
  end
  object bOK: TButton
    Left = 333
    Top = 354
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = bOKClick
  end
  object bCancel: TButton
    Left = 429
    Top = 354
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object eProgramName: TComboBox
    Left = 24
    Top = 32
    Width = 449
    Height = 21
    TabOrder = 4
  end
  object eInstanceCount: TLabeledEdit
    Left = 168
    Top = 314
    Width = 121
    Height = 21
    Anchors = [akLeft, akBottom]
    EditLabel.Width = 72
    EditLabel.Height = 13
    EditLabel.Caption = 'Instance count'
    TabOrder = 5
    Text = '1'
  end
  object GroupBox1: TGroupBox
    Left = 24
    Top = 107
    Width = 480
    Height = 185
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Parameter list'
    TabOrder = 6
    object ParameterList: TValueListEditor
      Left = 2
      Top = 15
      Width = 476
      Height = 168
      Align = alClient
      KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
      TabOrder = 0
      TitleCaptions.Strings = (
        'Param key'
        'Param value')
      OnStringsChange = ParameterListStringsChange
      ColWidths = (
        150
        320)
    end
  end
  object cbAddInstanceID: TCheckBox
    Left = 311
    Top = 316
    Width = 193
    Height = 17
    Caption = 'Add instance ID to parameters'
    TabOrder = 7
  end
  object OD: TOpenDialog
    DefaultExt = '.exe'
    Filter = 'Executable files (*.exe; *.bat)|*.exe; *.bat'
    FilterIndex = 0
    Left = 352
    Top = 16
  end
end
