object JwaTypeForm: TJwaTypeForm
  Left = 0
  Top = 0
  Caption = 'JwaTypeForm'
  ClientHeight = 402
  ClientWidth = 624
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
  object Label1: TLabel
    Left = 248
    Top = 40
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object MainVirtualStringTree: TVirtualStringTree
    Left = 8
    Top = 72
    Width = 409
    Height = 289
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoVisible]
    TabOrder = 0
    OnGetText = MainVirtualStringTreeGetText
    OnInitChildren = MainVirtualStringTreeInitChildren
    OnInitNode = MainVirtualStringTreeInitNode
    Columns = <
      item
        Position = 0
      end>
  end
end
