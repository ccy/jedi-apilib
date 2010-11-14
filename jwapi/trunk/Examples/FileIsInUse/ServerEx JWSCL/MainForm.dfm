object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'File In Use Server Example'
  ClientHeight = 497
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 72
    Width = 192
    Height = 13
    Caption = 'Drag a file on the form to hold it opened'
  end
  object Label2: TLabel
    Left = 179
    Top = 118
    Width = 238
    Height = 26
    Caption = 
      'Sets the access and launch permission in the App registration in' +
      ' registry:'
    WordWrap = True
  end
  object btnOpen: TButton
    Left = 16
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 0
    OnClick = btnOpenClick
  end
  object btnClose: TButton
    Left = 97
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCloseClick
  end
  object rg1: TRadioGroup
    Left = 16
    Top = 118
    Width = 137
    Height = 107
    Caption = 'Current integrity level'
    TabOrder = 2
  end
  object rbHigh: TRadioButton
    Left = 32
    Top = 144
    Width = 113
    Height = 17
    Caption = 'High'
    Enabled = False
    TabOrder = 3
  end
  object rbLow: TRadioButton
    Left = 32
    Top = 187
    Width = 113
    Height = 17
    Caption = 'Low'
    Enabled = False
    TabOrder = 4
  end
  object edtFileName: TEdit
    Left = 16
    Top = 91
    Width = 401
    Height = 21
    ReadOnly = True
    TabOrder = 5
  end
  object rbMedium: TRadioButton
    Left = 32
    Top = 165
    Width = 113
    Height = 17
    Caption = 'Medium'
    Enabled = False
    TabOrder = 6
  end
  object mmoLog: TMemo
    Left = 16
    Top = 265
    Width = 401
    Height = 200
    TabOrder = 7
  end
  object btnClear: TButton
    Left = 342
    Top = 234
    Width = 75
    Height = 25
    Caption = 'Clear log'
    TabOrder = 8
    OnClick = btnClearClick
  end
  object StaticText1: TStaticText
    Left = 16
    Top = 242
    Width = 21
    Height = 17
    Caption = 'Log'
    TabOrder = 9
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 478
    Width = 433
    Height = 19
    Panels = <
      item
        Text = '123'
        Width = 50
      end>
    SimplePanel = True
  end
  object btnRegister: TButton
    Tag = 1
    Left = 247
    Top = 8
    Width = 169
    Height = 25
    Caption = 'Register COM class'
    ElevationRequired = True
    TabOrder = 11
    OnClick = btnRegisterClick
  end
  object btnRegister1: TButton
    Tag = 1
    Left = 247
    Top = 39
    Width = 169
    Height = 25
    Caption = 'UnRegister COM class'
    ElevationRequired = True
    TabOrder = 12
    OnClick = btnRegister1Click
  end
  object btnRegisterRegSecurity: TButton
    Tag = 1
    Left = 209
    Top = 152
    Width = 208
    Height = 25
    Caption = 'Set App Security in Registry '
    ElevationRequired = True
    TabOrder = 13
    OnClick = btnRegisterRegSecurityClick
  end
  object btnUnRegisterRegSecurity: TButton
    Tag = 1
    Left = 209
    Top = 183
    Width = 208
    Height = 25
    Caption = 'Remove App Security from Registry '
    ElevationRequired = True
    TabOrder = 14
    OnClick = btnUnRegisterRegSecurityClick
  end
  object OpenDialog1: TOpenDialog
    Filter = '*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofShareAware, ofEnableSizing]
    Title = 'Open a file'
    Left = 352
    Top = 400
  end
end
