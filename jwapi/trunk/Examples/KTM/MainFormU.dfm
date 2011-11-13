object DemoForm: TDemoForm
  Left = 46
  Top = 159
  ActiveControl = SelectDemoFolderBtn
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'NTFS Transaktions Demo'
  ClientHeight = 557
  ClientWidth = 618
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 12
    Top = 232
    Width = 214
    Height = 16
    Caption = '&Folder content within the transaction:'
    FocusControl = TransactedViewLB
  end
  object Label2: TLabel
    Left = 312
    Top = 232
    Width = 236
    Height = 16
    Caption = 'F&older content outside of the transaction:'
    FocusControl = NormalViewLB
  end
  object DemoOrdnerGB: TGroupBox
    Left = 12
    Top = 16
    Width = 593
    Height = 57
    Caption = ' Test folder '
    TabOrder = 0
    object DemoFolderEdit: TEdit
      Left = 12
      Top = 20
      Width = 385
      Height = 24
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
    end
    object SelectDemoFolderBtn: TButton
      Left = 404
      Top = 20
      Width = 85
      Height = 25
      Caption = '&Select'
      TabOrder = 1
      OnClick = SelectDemoFolderBtnClick
    end
    object ExplorerBtn: TButton
      Left = 496
      Top = 20
      Width = 85
      Height = 25
      Caption = 'E&xplorer'
      Enabled = False
      TabOrder = 2
      OnClick = ExplorerBtnClick
    end
  end
  object TransactionsGB: TGroupBox
    Left = 12
    Top = 88
    Width = 291
    Height = 129
    Caption = ' Transaction '
    TabOrder = 1
    object NewTransactionBtn: TButton
      Left = 12
      Top = 20
      Width = 125
      Height = 25
      Caption = '&New Transaction'
      TabOrder = 0
      OnClick = NewTransactionBtnClick
    end
    object CommitTransactionBtn: TButton
      Left = 12
      Top = 56
      Width = 265
      Height = 25
      Caption = 'Transaction "&Commit"'
      Enabled = False
      TabOrder = 2
      OnClick = CommitTransactionBtnClick
    end
    object RollbackTransactionBtn: TButton
      Left = 12
      Top = 92
      Width = 265
      Height = 25
      Caption = 'Transaction "&Rollback"'
      Enabled = False
      TabOrder = 3
      OnClick = RollbackTransactionBtnClick
    end
    object NewTimeoutBtn: TButton
      Left = 152
      Top = 20
      Width = 125
      Height = 25
      Caption = 'N&ew with Timeout'
      TabOrder = 1
      OnClick = NewTransactionBtnClick
    end
  end
  object OperationsGB: TGroupBox
    Left = 314
    Top = 88
    Width = 291
    Height = 129
    Caption = ' Actions within the Transaction '
    TabOrder = 2
    object CreateFileBtn: TButton
      Left = 12
      Top = 20
      Width = 265
      Height = 25
      Caption = 'Crea&te file'
      TabOrder = 0
      OnClick = CreateFileBtnClick
    end
    object RenameFilesBtn: TButton
      Left = 12
      Top = 56
      Width = 265
      Height = 25
      Caption = 'Ren&ame file(s)'
      TabOrder = 1
      OnClick = RenameFilesBtnClick
    end
    object DeleteFilesBtn: TButton
      Left = 12
      Top = 92
      Width = 265
      Height = 25
      Caption = '&Delete file(s)'
      TabOrder = 2
      OnClick = DeleteFilesBtnClick
    end
  end
  object TransactedViewLB: TListBox
    Left = 12
    Top = 248
    Width = 291
    Height = 277
    Sorted = True
    TabOrder = 3
  end
  object NormalViewLB: TListBox
    Left = 314
    Top = 248
    Width = 291
    Height = 277
    Sorted = True
    TabOrder = 4
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 538
    Width = 618
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object TransactionTimer: TTimer
    Enabled = False
    OnTimer = TransactionTimerTimer
    Left = 24
    Top = 264
  end
end
