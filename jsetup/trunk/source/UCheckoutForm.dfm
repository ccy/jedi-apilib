object CheckoutForm: TCheckoutForm
  Left = 0
  Top = 0
  Caption = 'CheckoutForm'
  ClientHeight = 484
  ClientWidth = 719
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 130
    Height = 13
    Caption = 'Checkout new JEDI project'
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 32
    Width = 445
    Height = 209
    Caption = 'JWA'
    TabOrder = 0
    object Label2: TLabel
      Left = 16
      Top = 24
      Width = 124
      Height = 13
      Caption = 'Available release versions'
    end
    object Label3: TLabel
      Left = 16
      Top = 104
      Width = 91
      Height = 13
      Caption = 'Revision of release'
    end
    object Label4: TLabel
      Left = 24
      Top = 70
      Width = 31
      Height = 13
      Caption = 'Label4'
    end
    object Label5: TLabel
      Left = 24
      Top = 150
      Width = 31
      Height = 13
      Caption = 'Label4'
    end
    object ReleaseComboBoxJWA: TComboBox
      Left = 16
      Top = 43
      Width = 417
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'Latest Release 1.01'
      OnChange = ReleaseComboBoxJWAChange
      Items.Strings = (
        'Latest Release 1.01'
        '1.0'
        '0.9'
        'trunk')
    end
    object RevisionComboBoxJWA: TComboBox
      Left = 16
      Top = 123
      Width = 417
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'Latest Revision 544'
      Items.Strings = (
        'Latest Revision 544'
        '533'
        '450')
    end
    object JWARevisionReadHistory: TButton
      Left = 358
      Top = 150
      Width = 75
      Height = 25
      Action = RevisionJwaReadHistoryAction
      TabOrder = 2
    end
    object JWAReleaseReadHistory: TButton
      Left = 358
      Top = 70
      Width = 75
      Height = 25
      Action = ReleaseJwaReadHistoryAction
      TabOrder = 3
    end
  end
  object GroupBox2: TGroupBox
    Left = 16
    Top = 247
    Width = 445
    Height = 209
    Caption = 'JWSCL'
    TabOrder = 1
    object Label6: TLabel
      Left = 16
      Top = 24
      Width = 124
      Height = 13
      Caption = 'Available release versions'
    end
    object Label7: TLabel
      Left = 16
      Top = 104
      Width = 91
      Height = 13
      Caption = 'Revision of release'
    end
    object Label8: TLabel
      Left = 24
      Top = 70
      Width = 31
      Height = 13
      Caption = 'Label4'
    end
    object Label9: TLabel
      Left = 24
      Top = 150
      Width = 31
      Height = 13
      Caption = 'Label4'
    end
    object ReleaseComboBoxJwscl: TComboBox
      Left = 16
      Top = 43
      Width = 417
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'Latest Release 1.01'
      OnChange = ReleaseComboBoxJwsclChange
      Items.Strings = (
        'Latest Release 1.01'
        '1.0'
        '0.9'
        'trunk')
    end
    object RevisionComboBoxJwscl: TComboBox
      Left = 16
      Top = 123
      Width = 417
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'Latest Revision 544'
      Items.Strings = (
        'Latest Revision 544'
        '533'
        '450')
    end
    object Button1: TButton
      Left = 358
      Top = 150
      Width = 75
      Height = 25
      Action = RevisionJwsclReadHistoryAction
      TabOrder = 2
    end
    object Button2: TButton
      Left = 358
      Top = 81
      Width = 75
      Height = 25
      Action = ReleaseJwsclReadHistoryAction
      TabOrder = 3
    end
  end
  object OutputMemo: TMemo
    Left = 467
    Top = 32
    Width = 89
    Height = 51
    Lines.Strings = (
      'OutputMemo')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object UpdateButton: TButton
    Left = 356
    Top = 8
    Width = 105
    Height = 25
    Action = UpdateAction
    TabOrder = 3
  end
  object HTTPSCertCheckBox: TCheckBox
    Left = 16
    Top = 462
    Width = 337
    Height = 17
    Caption = 'Accept JEDI API sourceforge SVN certificate permanently.'
    TabOrder = 4
  end
  object JvCreateProcessJWA: TJvCreateProcess
    ApplicationName = 'svn.exe'
    CommandLine = 'svn.exe help'
    StartupInfo.ShowWindow = swHide
    StartupInfo.DefaultWindowState = False
    ConsoleOptions = [coOwnerData, coRedirect]
    OnTerminate = JvCPReleaseJwaTerminate
    OnRead = JvCPReleaseJWARead
    Left = 312
    Top = 56
  end
  object JvCreateProcessJWSCL: TJvCreateProcess
    ApplicationName = 'svn.exe'
    CommandLine = 'svn.exe help'
    StartupInfo.ShowWindow = swHide
    StartupInfo.DefaultWindowState = False
    ConsoleOptions = [coOwnerData, coRedirect]
    OnTerminate = JvCPReleaseJwsclTerminate
    OnRead = JvCPReleaseJwsclRead
    Left = 328
    Top = 288
  end
  object ActionList1: TActionList
    Left = 472
    object RevisionJwaReadHistoryAction: TAction
      Caption = 'Read History'
      OnExecute = ReadHistoryJwaButtonClick
      OnUpdate = RevisionJwaReadHistoryActionUpdate
    end
    object ReleaseJwaReadHistoryAction: TAction
      Caption = 'Read history'
      OnExecute = ReleaseJwaReadHistoryActionExecute
      OnUpdate = ReleaseJwaReadHistoryActionUpdate
    end
    object UpdateAction: TAction
      Caption = 'Update'
      OnExecute = UpdateActionExecute
      OnUpdate = UpdateActionUpdate
    end
    object ReleaseJwsclReadHistoryAction: TAction
      Caption = 'Read History'
      OnExecute = ReleaseJwsclReadHistoryActionExecute
      OnUpdate = ReleaseJwsclReadHistoryActionUpdate
    end
    object RevisionJwsclReadHistoryAction: TAction
      Caption = 'Read history'
      OnExecute = RevisionJwsclReadHistoryActionExecute
      OnUpdate = RevisionJwsclReadHistoryActionUpdate
    end
  end
  object JvCreateProcessRevisionJWA: TJvCreateProcess
    ApplicationName = 'svn.exe'
    CommandLine = 'svn.exe log %s/%s'
    StartupInfo.ShowWindow = swHide
    StartupInfo.DefaultWindowState = False
    ConsoleOptions = [coOwnerData, coRedirect]
    OnTerminate = JvCPRevisionJWATerminate
    OnRead = JvCPRevisionJWARead
    Left = 312
    Top = 144
  end
  object JvCreateProcessRevisionJwscl: TJvCreateProcess
    ApplicationName = 'svn.exe'
    CommandLine = 'svn.exe help'
    StartupInfo.ShowWindow = swHide
    StartupInfo.DefaultWindowState = False
    ConsoleOptions = [coOwnerData, coRedirect]
    OnTerminate = JvCPRevisionJwsclTerminate
    OnRead = JvCPRevisionJwsclRead
    Left = 328
    Top = 368
  end
  object JvCreateProcessHistoryJWA: TJvCreateProcess
    ApplicationName = 'svn.exe'
    CommandLine = 'svn.exe help'
    StartupInfo.ShowWindow = swHide
    StartupInfo.DefaultWindowState = False
    ConsoleOptions = [coOwnerData, coRedirect]
    OnTerminate = JvCPHistoryJWATerminate
    OnRead = JvCPHistoryJWARead
    Left = 312
    Top = 104
  end
  object JvCreateProcessHistoryJwscl: TJvCreateProcess
    ApplicationName = 'svn.exe'
    CommandLine = 'svn.exe help'
    StartupInfo.ShowWindow = swHide
    StartupInfo.DefaultWindowState = False
    ConsoleOptions = [coOwnerData, coRedirect]
    OnTerminate = JvCPHistoryJwsclTerminate
    OnRead = JvCPHistoryJwsclRead
    Left = 328
    Top = 328
  end
  object JvHttpUrlGrabber1: TJvHttpUrlGrabber
    FileName = 'svn-win32-1.4.6.zip'
    Agent = 'JEDI-VCL'
    Url = 'http://wimmer.teamchris.info/prog/.misc/jwscl.rar'
    Port = 0
    ProxyAddresses = 'proxyserver'
    ProxyIgnoreList = '<local>'
    OnDoneFile = JvHttpUrlGrabber1DoneFile
    OnError = JvHttpUrlGrabber1Error
    OnProgress = JvHttpUrlGrabber1Progress
    Left = 200
    Top = 8
  end
  object JvProgressDialog1: TJvProgressDialog
    Smooth = True
    ScreenPosition = poOwnerFormCenter
    OnCancel = JvProgressDialog1Cancel
    Left = 160
    Top = 8
  end
  object ZipMaster1: TZipMaster
    AddOptions = []
    AddStoreSuffixes = [assGIF, assPNG, assZ, assZIP, assZOO, assARC, assLZH, assARJ, assTAZ, assTGZ, assLHA, assRAR, assACE, assCAB, assGZ, assGZIP, assJAR, assEXE, assJPG, assJPEG, ass7Zp, assMP3, assWMV, assWMA, assDVR, assAVI]
    Dll_Load = False
    ExtrOptions = []
    KeepFreeOnAllDisks = 0
    KeepFreeOnDisk1 = 0
    MaxVolumeSize = 0
    PasswordReqCount = 1
    SFXOptions = []
    SFXOverWriteMode = OvrConfirm
    SFXPath = 'DZSFXUS.bin'
    SpanOptions = []
    Trace = False
    Unattended = False
    Verbose = False
    Version = '1.79.08.07'
    VersionInfo = '1.79.08.07'
    OnTotalProgress = ZipMaster1TotalProgress
    Left = 232
    Top = 8
  end
end
