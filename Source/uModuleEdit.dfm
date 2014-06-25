object ModuleForm: TModuleForm
  Left = 0
  Top = 0
  Caption = 'Add/Edit module'
  ClientHeight = 624
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    512
    624)
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
    Top = 581
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
    Top = 550
    Width = 121
    Height = 21
    Anchors = [akLeft, akBottom]
    EditLabel.Width = 82
    EditLabel.Height = 13
    EditLabel.Caption = 'Instance Number'
    TabOrder = 1
    Text = '1'
    ExplicitTop = 314
  end
  object bOK: TButton
    Left = 333
    Top = 590
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = bOKClick
    ExplicitTop = 354
  end
  object bCancel: TButton
    Left = 429
    Top = 590
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ExplicitTop = 354
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
    Top = 550
    Width = 121
    Height = 21
    Anchors = [akLeft, akBottom]
    EditLabel.Width = 72
    EditLabel.Height = 13
    EditLabel.Caption = 'Instance count'
    TabOrder = 5
    Text = '1'
    ExplicitTop = 314
  end
  object GroupBox1: TGroupBox
    Left = 24
    Top = 128
    Width = 480
    Height = 192
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Parameter list'
    TabOrder = 6
    ExplicitHeight = 186
    object ParameterList: TValueListEditor
      Left = 2
      Top = 15
      Width = 476
      Height = 175
      Align = alClient
      KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
      TabOrder = 0
      TitleCaptions.Strings = (
        'Param key'
        'Param value')
      OnStringsChange = ParameterListStringsChange
      ExplicitHeight = 168
      ColWidths = (
        150
        320)
    end
  end
  object GroupBox2: TGroupBox
    Left = 24
    Top = 326
    Width = 478
    Height = 89
    Anchors = [akLeft, akBottom]
    Caption = 'Output'
    TabOrder = 7
    ExplicitTop = 320
    object Label1: TLabel
      Left = 24
      Top = 37
      Width = 62
      Height = 13
      Caption = 'Regexp filter'
    end
    object cbProcessResponse: TCheckBox
      Left = 10
      Top = 16
      Width = 167
      Height = 17
      Caption = 'Filter response messages'
      TabOrder = 0
      OnClick = cbProcessResponseClick
    end
    object eRegexpFilter: TEdit
      Left = 24
      Top = 56
      Width = 441
      Height = 21
      TabOrder = 1
    end
  end
  object cbAddInstanceID: TCheckBox
    Left = 26
    Top = 105
    Width = 193
    Height = 17
    Caption = 'Add instance ID as --instance'
    TabOrder = 8
  end
  object GroupBox3: TGroupBox
    Left = 24
    Top = 421
    Width = 480
    Height = 105
    Anchors = [akLeft, akBottom]
    Caption = 'Timer'
    TabOrder = 9
    ExplicitTop = 415
    object Label3: TLabel
      Left = 24
      Top = 47
      Width = 63
      Height = 13
      Caption = 'Restart after'
    end
    object Label4: TLabel
      Left = 116
      Top = 69
      Width = 91
      Height = 13
      Caption = '(format: 5 or 5-10)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 92
      Top = 69
      Width = 16
      Height = 13
      Caption = 'sec'
    end
    object cbAutoRestart: TCheckBox
      Left = 10
      Top = 24
      Width = 185
      Height = 17
      Caption = 'Auto restart module if stopped'
      TabOrder = 0
      OnClick = cbAutoRestartClick
    end
    object eRestartTimer: TEdit
      Left = 24
      Top = 66
      Width = 62
      Height = 21
      TabOrder = 1
      Text = '5'
    end
  end
  object OD: TOpenDialog
    DefaultExt = '.exe'
    Filter = 'Executable files (*.exe; *.bat)|*.exe; *.bat'
    FilterIndex = 0
    Left = 352
    Top = 16
  end
end
