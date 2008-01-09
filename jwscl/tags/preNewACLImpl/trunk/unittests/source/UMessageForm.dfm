object frmMessage: TfrmMessage
  Left = 2256
  Top = 369
  Width = 704
  Height = 591
  Caption = 'Message'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object memMessage: TMemo
    Left = 8
    Top = 8
    Width = 678
    Height = 493
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'memMessage')
    ReadOnly = True
    TabOrder = 0
    WantReturns = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 508
    Width = 696
    Height = 49
    Align = alBottom
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 1
    object Button1: TButton
      Left = 294
      Top = 13
      Width = 104
      Height = 25
      Anchors = []
      Caption = '&Close'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object Timer1: TTimer
    Interval = 1500
    OnTimer = Timer1Timer
    Left = 296
    Top = 48
  end
end
