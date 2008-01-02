object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 415
  ClientWidth = 702
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object VirtualStringTree1: TVirtualStringTree
    Left = 32
    Top = 24
    Width = 561
    Height = 241
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible]
    TabOrder = 0
    OnGetText = VirtualStringTree1GetText
    OnGetImageIndex = VirtualStringTree1GetImageIndex
    OnGetNodeDataSize = VirtualStringTree1GetNodeDataSize
    OnInitNode = VirtualStringTree1InitNode
    Columns = <
      item
        Position = 0
        Width = 407
        WideText = 'Column 0'
      end
      item
        Position = 1
        WideText = 'Column 0'
      end
      item
        Position = 2
        WideText = 'Column 0'
      end
      item
        Position = 3
        WideText = 'Column 0'
      end
      item
        Position = 4
      end
      item
        Position = 5
      end
      item
        Position = 6
      end
      item
        Position = 7
      end>
  end
  object Button1: TButton
    Left = 608
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 48
    Top = 288
    Width = 569
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
end
