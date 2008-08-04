object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 432
  ClientWidth = 752
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
  object pgc1: TPageControl
    Left = 24
    Top = 23
    Width = 720
    Height = 401
    ActivePage = ts2
    TabOrder = 0
    object ts1: TTabSheet
      Caption = 'ts1'
      OnShow = ts1Show
    end
    object ts2: TTabSheet
      Caption = 'ts2'
      ImageIndex = 1
    end
  end
end
