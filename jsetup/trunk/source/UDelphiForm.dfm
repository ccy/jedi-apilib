object DelphiForm: TDelphiForm
  Left = 0
  Top = 0
  Caption = 'DelphiForm'
  ClientHeight = 280
  ClientWidth = 520
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 16
    Width = 353
    Height = 13
    Caption = 
      'Check your target Delphi version which you want to use with JEDI' +
      ' project'
  end
  object GroupBox1: TGroupBox
    Left = 32
    Top = 56
    Width = 217
    Height = 193
    Caption = ' JEDI API '
    TabOrder = 0
    object CheckBox1: TCheckBox
      Left = 16
      Top = 24
      Width = 169
      Height = 17
      Caption = 'Delphi 5'
      TabOrder = 0
    end
    object CheckBox3: TCheckBox
      Left = 16
      Top = 47
      Width = 169
      Height = 17
      Caption = 'Delphi 6'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox4: TCheckBox
      Left = 16
      Top = 70
      Width = 169
      Height = 17
      Caption = 'Delphi 7'
      TabOrder = 2
    end
    object CheckBox5: TCheckBox
      Left = 16
      Top = 93
      Width = 169
      Height = 17
      Caption = 'Delphi 8'
      Enabled = False
      TabOrder = 3
    end
    object CheckBox6: TCheckBox
      Left = 16
      Top = 116
      Width = 169
      Height = 17
      Caption = 'Delphi RAD 2005'
      Enabled = False
      TabOrder = 4
    end
    object CheckBox7: TCheckBox
      Left = 16
      Top = 139
      Width = 169
      Height = 17
      Caption = 'Delphi RAD 2006'
      Enabled = False
      TabOrder = 5
    end
    object CheckBox8: TCheckBox
      Left = 16
      Top = 162
      Width = 169
      Height = 17
      Caption = 'Delphi RAD 2007'
      TabOrder = 6
    end
  end
  object GroupBox2: TGroupBox
    Left = 280
    Top = 56
    Width = 217
    Height = 193
    Caption = ' JEDI WSCL'
    TabOrder = 1
    object CheckBox2: TCheckBox
      Left = 16
      Top = 24
      Width = 169
      Height = 17
      Caption = 'Delphi 5'
      TabOrder = 0
    end
    object CheckBox9: TCheckBox
      Left = 16
      Top = 47
      Width = 169
      Height = 17
      Caption = 'Delphi 6'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox10: TCheckBox
      Left = 16
      Top = 70
      Width = 169
      Height = 17
      Caption = 'Delphi 7'
      TabOrder = 2
    end
    object CheckBox11: TCheckBox
      Left = 16
      Top = 93
      Width = 169
      Height = 17
      Caption = 'Delphi 8'
      Enabled = False
      TabOrder = 3
    end
    object CheckBox12: TCheckBox
      Left = 16
      Top = 116
      Width = 169
      Height = 17
      Caption = 'Delphi RAD 2005'
      Enabled = False
      TabOrder = 4
    end
    object CheckBox13: TCheckBox
      Left = 16
      Top = 139
      Width = 169
      Height = 17
      Caption = 'Delphi RAD 2006'
      Enabled = False
      TabOrder = 5
    end
    object CheckBox14: TCheckBox
      Left = 16
      Top = 162
      Width = 169
      Height = 17
      Caption = 'Delphi RAD 2007'
      TabOrder = 6
    end
  end
end
