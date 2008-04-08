object SM_SecurityTreeReset_Form: TSM_SecurityTreeReset_Form
  Left = 687
  Top = 357
  Width = 422
  Height = 189
  BorderIcons = [biSystemMenu]
  Caption = 'Setting security'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Object_Label: TLabel
    Left = 8
    Top = 80
    Width = 401
    Height = 41
    AutoSize = False
    Caption = 'Object_Label'
    WordWrap = True
  end
  object Timer_Label: TLabel
    Left = 8
    Top = 136
    Width = 58
    Height = 13
    Caption = 'Timer_Label'
  end
  object Cancel_BitBtn: TBitBtn
    Left = 312
    Top = 128
    Width = 99
    Height = 25
    Caption = '&Cancel'
    TabOrder = 0
    OnClick = Cancel_BitBtnClick
    Kind = bkCancel
  end
  object Animate_Ctrl: TAnimate
    Left = 8
    Top = 8
    Width = 272
    Height = 60
    CommonAVI = aviCopyFiles
    StopFrame = 31
  end
  object Timer_Main: TTimer
    Enabled = False
    OnTimer = Timer_MainTimer
    Left = 304
    Top = 8
  end
end
