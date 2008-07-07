object PathForm: TPathForm
  Left = 608
  Top = 190
  Caption = 'PathForm'
  ClientHeight = 312
  ClientWidth = 564
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
    Left = 16
    Top = 48
    Width = 113
    Height = 13
    Caption = 'Path to JEDI API (JWA)'
  end
  object Label2: TLabel
    Left = 17
    Top = 154
    Width = 256
    Height = 13
    Caption = 'Path to JEDI Windows Security Code Library (JWSCL)'
  end
  object Label3: TLabel
    Left = 16
    Top = 16
    Width = 227
    Height = 13
    Caption = 'Select the path where to download the sources'
  end
  object JvDirectoryJwa: TJvDirectoryEdit
    Left = 17
    Top = 77
    Width = 352
    Height = 21
    DialogKind = dkWin32
    AutoCompleteOptions = [acoAutoSuggest, acoSearch]
    DialogOptions = [sdAllowCreate, sdPrompt]
    TabOrder = 0
    Text = 'JvDirectoryJwa'
  end
  object JvDirectoryJwscl: TJvDirectoryEdit
    Left = 17
    Top = 173
    Width = 352
    Height = 21
    DialogKind = dkWin32
    AutoCompleteOptions = [acoAutoSuggest, acoSearch]
    DialogOptions = [sdAllowCreate, sdPrompt]
    TabOrder = 1
    Text = 'JvDirectoryEdit1'
  end
end
