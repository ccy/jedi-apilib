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
  PixelsPerInch = 96
  TextHeight = 13
  object pgc1: TPageControl
    Left = 24
    Top = 24
    Width = 720
    Height = 401
    ActivePage = ts1
    TabOrder = 0
    object ts1: TTabSheet
      Caption = 'ts1'
      ExplicitHeight = 472
      inline frm: TFrame2
        Left = 4
        Top = 4
        Width = 705
        Height = 365
        TabOrder = 0
        ExplicitLeft = 4
        ExplicitTop = 4
        ExplicitWidth = 705
        ExplicitHeight = 365
        inherited jvpnlJWA: TJvPanel
          inherited btnJWAReleaseUpdate: TButton
            OnClick = nil
          end
          inherited rbPrecompiledJWA: TJvRadioButton
            OnClick = nil
          end
          inherited btnHistoryJWA: TButton
            OnClick = nil
          end
        end
        inherited jvpnl1: TJvPanel
          inherited btnHistoryJWSCL: TButton
            OnClick = nil
          end
        end
      end
    end
  end
end
