object FormCredentials: TFormCredentials
  Left = 2049
  Top = 274
  BorderStyle = bsSingle
  Caption = 'FormCredentials'
  ClientHeight = 334
  ClientWidth = 515
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object JvBevel1: TJvBevel
    Left = 16
    Top = 80
    Width = 482
    Height = 80
    Style = bsCustomStyle
    Inner = bvRaised
    VerticalLines.Bold = True
  end
  object Image2: TImage
    Left = 24
    Top = 179
    Width = 64
    Height = 64
    Transparent = True
  end
  object Label1: TLabel
    Left = 120
    Top = 91
    Width = 369
    Height = 20
    AutoSize = False
    Caption = 'Label1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Image1: TImage
    Left = 24
    Top = 88
    Width = 64
    Height = 64
    Transparent = True
  end
  object ButtonDefaultUser: TJvTransparentButton
    Left = 8
    Top = 72
    Width = 497
    Height = 93
    GroupIndex = 1
    HotTrack = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    FrameStyle = fsLight
    OnClick = ButtonDefaultUserClick
  end
  object JvBevel2: TJvBevel
    Left = 16
    Top = 171
    Width = 482
    Height = 80
    Style = bsCustomStyle
    Inner = bvRaised
    VerticalLines.Bold = True
  end
  object ButtonUser: TJvTransparentButton
    Left = 8
    Top = 165
    Width = 497
    Height = 93
    GroupIndex = 1
    HotTrack = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    FrameStyle = fsLight
    OnClick = ButtonUserClick
  end
  object EditPassword2: TEdit
    Left = 120
    Top = 214
    Width = 365
    Height = 28
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    PasswordChar = #7
    TabOrder = 2
    OnChange = EditPassword2Change
  end
  object BitBtn1: TBitBtn
    Left = 400
    Top = 288
    Width = 107
    Height = 25
    TabOrder = 4
    Kind = bkCancel
  end
  object BitBtn2: TBitBtn
    Left = 296
    Top = 288
    Width = 99
    Height = 25
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    Kind = bkOK
  end
  object EditPassword1: TEdit
    Left = 120
    Top = 120
    Width = 365
    Height = 28
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    PasswordChar = #7
    TabOrder = 0
    OnChange = EditPassword1Change
  end
  object UsersComboBox: TComboBox
    Left = 120
    Top = 181
    Width = 366
    Height = 28
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 20
    ParentFont = False
    Sorted = True
    TabOrder = 1
    Text = 'UsersComboBox'
    OnChange = UsersComboBoxChange
    Items.Strings = (
      '1'
      '2'
      '3')
  end
  object CheckBoxSaveLogon: TCheckBox
    Left = 8
    Top = 278
    Width = 193
    Height = 21
    Caption = 'Save logon credentials'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    OnClick = CheckBoxSaveLogonClick
  end
end
