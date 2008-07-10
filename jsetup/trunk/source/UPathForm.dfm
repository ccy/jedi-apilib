object PathForm: TPathForm
  Left = 608
  Top = 190
  Caption = 'PathForm'
  ClientHeight = 394
  ClientWidth = 620
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 64
    Width = 102
    Height = 13
    Caption = 'Path to JEDI projects'
  end
  object Label3: TLabel
    Left = 24
    Top = 32
    Width = 263
    Height = 13
    Caption = 'Select an existing path where to download the sources'
  end
  object JvDirectoryJwa: TJvDirectoryEdit
    Left = 25
    Top = 83
    Width = 352
    Height = 24
    DialogKind = dkWin32
    AutoCompleteOptions = [acoAutoSuggest, acoSearch]
    DialogOptions = [sdAllowCreate, sdPrompt]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object StaticText1: TStaticText
    Left = 25
    Top = 128
    Width = 520
    Height = 33
    AutoSize = False
    Caption = 
      'Select the main path where to install both projects. Into the gi' +
      'ven path, the JEDI API and WSCL will be copied into separate fol' +
      'ders.'
    TabOrder = 1
  end
end
