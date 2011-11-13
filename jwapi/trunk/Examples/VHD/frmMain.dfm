object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = '  VHD API Demo'
  ClientHeight = 238
  ClientWidth = 722
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnMount: TButton
    Left = 615
    Top = 58
    Width = 89
    Height = 25
    Caption = 'Mount Image'
    TabOrder = 0
    OnClick = btnMountClick
  end
  object btnDismount: TButton
    Left = 615
    Top = 89
    Width = 89
    Height = 25
    Caption = 'Dismount'
    TabOrder = 1
    OnClick = btnDismountClick
  end
  object MemoInfo: TMemo
    Left = 8
    Top = 8
    Width = 601
    Height = 223
    TabOrder = 2
  end
  object btnInfo: TButton
    Left = 615
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Disk Infos'
    TabOrder = 3
    OnClick = btnInfoClick
  end
  object OpenDialogVHD: TOpenDialog
    Filter = 'Virtual Hard Disk|*.vhd|All Files|*.*'
    Left = 40
    Top = 24
  end
end
