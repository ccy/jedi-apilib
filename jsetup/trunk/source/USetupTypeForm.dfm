object SetupTypeForm: TSetupTypeForm
  Left = 0
  Top = 0
  Caption = 'SetupTypeForm'
  ClientHeight = 418
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 48
    Top = 112
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 48
    Top = 175
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label3: TLabel
    Left = 24
    Top = 16
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label4: TLabel
    Left = 48
    Top = 246
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object UpdateRadioButton: TRadioButton
    Left = 24
    Top = 152
    Width = 345
    Height = 17
    Caption = 'Update JEDI project to newer revision or newer release'
    TabOrder = 0
  end
  object CheckoutRadioButton: TRadioButton
    Left = 24
    Top = 80
    Width = 345
    Height = 17
    Caption = 'Checkout the JEDI project the first time'
    Checked = True
    TabOrder = 1
    TabStop = True
  end
  object RemoveRadioButton: TRadioButton
    Left = 24
    Top = 223
    Width = 113
    Height = 17
    Caption = 'Remove JEDI project'
    TabOrder = 2
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 352
    Width = 281
    Height = 17
    Caption = 'Install JEDI API Headers (JWA)'
    TabOrder = 3
  end
  object CheckBox2: TCheckBox
    Left = 16
    Top = 384
    Width = 281
    Height = 17
    Caption = 'Install JEDI Windows Security Code Library (JWSCL)'
    TabOrder = 4
  end
end
