object Frame2: TFrame2
  Left = 0
  Top = 0
  ClientHeight = 378
  ClientWidth = 716
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object jvlbLabel: TJvLabel
    Left = 8
    Top = 8
    Width = 165
    Height = 13
    Caption = 'Target Path for JWA and JWSCL :'
    Transparent = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'Tahoma'
    HotTrackFont.Style = []
  end
  object edtTargetPath: TJvDirectoryEdit
    Left = 15
    Top = 27
    Width = 673
    Height = 21
    DialogKind = dkWin32
    TabOrder = 0
    Text = 'edtTargetPath'
  end
  object chkForAllDelphi: TCheckBox
    Left = 15
    Top = 54
    Width = 209
    Height = 17
    Caption = 'Use this path for all Delphi version'
    TabOrder = 1
  end
  object jvpnlJWA: TJvPanel
    Left = 8
    Top = 77
    Width = 360
    Height = 281
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'Tahoma'
    HotTrackFont.Style = []
    BevelInner = bvLowered
    TabOrder = 2
    object JLabel2: TJvLabel
      Left = 11
      Top = 10
      Width = 163
      Height = 13
      Caption = 'Select available JEDI API release:'
      Transparent = True
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'Tahoma'
      HotTrackFont.Style = []
    end
    object JLabel3: TJvLabel
      Left = 11
      Top = 58
      Width = 319
      Height = 13
      Caption = 
        'Select the JWA configuration that should be integrated into Delp' +
        'hi'
      Transparent = True
      WordWrap = True
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'Tahoma'
      HotTrackFont.Style = []
    end
    object JLabel1: TJvLabel
      Left = 11
      Top = 170
      Width = 110
      Height = 13
      Caption = 'Configure preferences'
      Transparent = True
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'Tahoma'
      HotTrackFont.Style = []
    end
    object btnJWAReleaseUpdate: TButton
      Left = 169
      Top = 27
      Width = 75
      Height = 25
      Caption = 'Update'
      TabOrder = 0
    end
    object cbbJWARelease: TJvComboBox
      Left = 18
      Top = 29
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
    object cbbJwaCurrentConfiguration: TJvComboBox
      Left = 32
      Top = 143
      Width = 298
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
    end
    object rbSingleJWAMode: TJvRadioButton
      Left = 18
      Top = 77
      Width = 314
      Height = 33
      Alignment = taLeftJustify
      Caption = 
        'Use single unit mode for JWA. This means that the Setup will add' +
        ' the source path to your Delphi configuration. '#13#10
      TabOrder = 3
      WordWrap = True
      AutoSize = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'Tahoma'
      HotTrackFont.Style = []
      LinkedControls = <>
    end
    object rbPrecompiledJWA: TJvRadioButton
      Left = 18
      Top = 107
      Width = 331
      Height = 34
      Alignment = taLeftJustify
      Caption = 'Use precompiled JWA sources'
      Checked = True
      TabOrder = 4
      TabStop = True
      WordWrap = True
      AutoSize = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'Tahoma'
      HotTrackFont.Style = []
      LinkedControls = <>
    end
    object chckscrlbxJWAConfiguration: TCheckScrollBox
      Left = 21
      Top = 189
      Width = 331
      Height = 83
      TabOrder = 5
      FocusLabelColor = clTeal
    end
    object btnHistoryJWA: TButton
      Left = 250
      Top = 27
      Width = 75
      Height = 25
      Caption = 'History'
      TabOrder = 6
    end
  end
  object jvpnl1: TJvPanel
    Left = 366
    Top = 77
    Width = 329
    Height = 281
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'Tahoma'
    HotTrackFont.Style = []
    BevelInner = bvLowered
    TabOrder = 3
    object JLabel4: TJvLabel
      Left = 9
      Top = 10
      Width = 174
      Height = 13
      Caption = 'Select available JEDI WSCL release:'
      Transparent = True
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'Tahoma'
      HotTrackFont.Style = []
    end
    object JLabel6: TJvLabel
      Left = 9
      Top = 58
      Width = 110
      Height = 13
      Caption = 'Configure preferences'
      Transparent = True
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'Tahoma'
      HotTrackFont.Style = []
    end
    object btnJWSCLReleaseUpdate: TButton
      Left = 167
      Top = 27
      Width = 75
      Height = 25
      Caption = 'Update'
      TabOrder = 0
    end
    object cbbJWSCLRelease: TJvComboBox
      Left = 16
      Top = 29
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
    end
    object chckscrlbxJWSCLConfiguration: TCheckScrollBox
      Left = 16
      Top = 77
      Width = 306
      Height = 92
      TabOrder = 2
      FocusLabelColor = clTeal
    end
    object btnHistoryJWSCL: TButton
      Left = 247
      Top = 27
      Width = 75
      Height = 25
      Caption = 'History'
      TabOrder = 3
    end
  end
end
