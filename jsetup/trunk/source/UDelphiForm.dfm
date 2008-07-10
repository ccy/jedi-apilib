object DelphiForm: TDelphiForm
  Left = 0
  Top = 0
  Caption = 'DelphiForm'
  ClientHeight = 448
  ClientWidth = 666
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 23
    Top = 112
    Width = 217
    Height = 254
    Caption = ' JEDI API '
    TabOrder = 0
    object JwaCheckScrollBox: TCheckScrollBox
      Left = 9
      Top = 24
      Width = 185
      Height = 193
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      FocusLabelColor = clMaroon
    end
    object SetChecksButton1: TButton
      Left = 13
      Top = 223
      Width = 28
      Height = 25
      Caption = '[X]'
      TabOrder = 1
      OnClick = SetChecksButton2Click
    end
    object RemoveChecksButton1: TButton
      Left = 47
      Top = 223
      Width = 26
      Height = 25
      Caption = '[ ]'
      TabOrder = 2
      OnClick = SetChecksButton2Click
    end
    object ReverseChecksButton1: TButton
      Left = 79
      Top = 223
      Width = 26
      Height = 25
      Caption = '[?]'
      TabOrder = 3
      OnClick = SetChecksButton2Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 271
    Top = 112
    Width = 217
    Height = 254
    Caption = ' JEDI WSCL (optional)'
    TabOrder = 1
    object JwsclCheckScrollBox: TCheckScrollBox
      Left = 11
      Top = 24
      Width = 185
      Height = 193
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      FocusLabelColor = clMaroon
    end
    object SetChecksButton2: TButton
      Tag = 1
      Left = 13
      Top = 223
      Width = 28
      Height = 25
      Caption = '[X]'
      TabOrder = 1
      OnClick = SetChecksButton2Click
    end
    object RemoveChecksButton2: TButton
      Tag = 1
      Left = 47
      Top = 223
      Width = 26
      Height = 25
      Caption = '[ ]'
      TabOrder = 2
      OnClick = SetChecksButton2Click
    end
    object ReverseChecksButton2: TButton
      Tag = 1
      Left = 79
      Top = 223
      Width = 26
      Height = 25
      Caption = '[?]'
      TabOrder = 3
      OnClick = SetChecksButton2Click
    end
  end
  object StaticText1: TStaticText
    Left = 32
    Top = 16
    Width = 465
    Height = 50
    AutoSize = False
    Caption = 
      'Select the target Delphi versions which you want to use to compi' +
      'le the JEDI project. You can deselect all target Delphi version ' +
      'for JWSCL  if you don'#39't want JWSCL to be installed. However each' +
      ' JWSCL installation needs its JWA for the appropriate Delphi ver' +
      'sion.'
    TabOrder = 2
  end
  object ErrorStaticText: TStaticText
    Left = 23
    Top = 380
    Width = 465
    Height = 29
    AutoSize = False
    Caption = 
      'You must at least select one Delphi version for JEDI API . The J' +
      'EDI WSCL project needs the JEDI API project to be installed. '
    TabOrder = 3
  end
  object Button1: TButton
    Left = 32
    Top = 72
    Width = 129
    Height = 25
    Caption = 'Update Delphi versions'
    TabOrder = 4
    OnClick = Button1Click
  end
end
