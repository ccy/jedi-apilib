object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'File In Use Server Example'
  ClientHeight = 338
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 72
    Width = 401
    Height = 13
    Caption = 'Drag a file on the form to open a file'
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
    Top = 120
    Width = 193
    Height = 81
    Caption = 'Current integrity level'
    TabOrder = 2
  end
  object RadioButton1: TRadioButton
    Left = 32
    Top = 144
    Width = 113
    Height = 17
    Caption = 'High'
    TabOrder = 3
  end
  object RadioButton2: TRadioButton
    Left = 32
    Top = 167
    Width = 113
    Height = 17
    Caption = 'Low'
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
  object OpenDialog1: TOpenDialog
    Filter = '*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofShareAware, ofEnableSizing]
    Title = 'Open a file'
    Left = 248
    Top = 16
  end
end
