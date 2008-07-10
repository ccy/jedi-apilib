object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'MainForm'
  ClientHeight = 570
  ClientWidth = 782
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button3: TButton
    Left = 664
    Top = 527
    Width = 100
    Height = 25
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = Button3Click
  end
  object ButtonNext: TButton
    Left = 558
    Top = 527
    Width = 100
    Height = 25
    Action = ActionNext
    TabOrder = 1
  end
  object ButtonBack: TButton
    Left = 452
    Top = 527
    Width = 100
    Height = 25
    Caption = 'Back'
    TabOrder = 2
    OnClick = ButtonBackClick
  end
  object Panel1: TPanel
    Left = 160
    Top = 24
    Width = 604
    Height = 497
    Caption = 'Panel1'
    TabOrder = 3
  end
  object JvCreateProcess1: TJvCreateProcess
    Left = 64
    Top = 32
  end
  object ActionList1: TActionList
    Left = 112
    Top = 24
    object ActionNext: TAction
      Caption = 'Next'
      OnExecute = ButtonNextClick
      OnUpdate = ActionNextUpdate
    end
  end
end
