object CheckoutForm: TCheckoutForm
  Left = 1884
  Top = 234
  Caption = 'CheckoutForm'
  ClientHeight = 526
  ClientWidth = 862
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
    Left = 8
    Top = 13
    Width = 130
    Height = 13
    Caption = 'Checkout new JEDI project'
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
    TabOrder = 0
    WordWrap = False
  end
  object MainPageControl: TPageControl
    Left = 0
    Top = 48
    Width = 862
    Height = 478
    ActivePage = TabSheet3
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    OnChange = MainPageControlChange
    object TabSheet3: TTabSheet
      Caption = 'General'
      ImageIndex = 2
      OnEnter = TabSheet3Enter
      object HTTPSCertCheckBox: TCheckBox
        Left = 36
        Top = 119
        Width = 337
        Height = 17
        Caption = 'Accept JEDI API sourceforge SVN certificate permanently.'
        TabOrder = 0
      end
      object UpdateButton: TButton
        Left = 36
        Top = 192
        Width = 173
        Height = 25
        Action = UpdateAction
        TabOrder = 1
      end
      object StaticText1: TStaticText
        Left = 36
        Top = 40
        Width = 421
        Height = 57
        AutoSize = False
        Caption = 
          'Klick the Update button to start with the receiving of JEDI API ' +
          '&& WSCL Version information. Currently only version information ' +
          'is returned. No actual download is initiated.'
        TabOrder = 2
      end
      object StaticText2: TStaticText
        Left = 20
        Top = 17
        Width = 42
        Height = 17
        Caption = '1. Read'
        TabOrder = 3
      end
      object StaticText3: TStaticText
        Left = 20
        Top = 96
        Width = 445
        Height = 17
        Caption = 
          '2. You can accept the server certificate permanently if you acti' +
          'vate the following checkbox:'
        TabOrder = 4
      end
      object StaticText4: TStaticText
        Left = 20
        Top = 161
        Width = 234
        Height = 17
        Caption = '3. Update JEDI API&&WSCL version information'
        TabOrder = 5
      end
      object StaticText5: TStaticText
        Left = 20
        Top = 233
        Width = 445
        Height = 32
        AutoSize = False
        Caption = 
          '4. You can skip to the next step or adapt the default options in' +
          ' the JEDI API and JEDI WSCL tab. Currently the newest release wi' +
          'th latest source will be used for the installation.'
        TabOrder = 6
      end
      object JWAInfoListView: TListView
        Left = 36
        Top = 294
        Width = 189
        Height = 75
        Columns = <
          item
            AutoSize = True
            Caption = 'Name'
          end
          item
            Caption = 'Value'
            Width = 60
          end>
        GridLines = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 7
        ViewStyle = vsReport
      end
      object JwsclInfoListView: TListView
        Left = 244
        Top = 294
        Width = 189
        Height = 75
        Columns = <
          item
            AutoSize = True
            Caption = 'Name'
          end
          item
            Caption = 'Value'
            Width = 60
          end>
        GridLines = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 8
        ViewStyle = vsReport
      end
      object StaticText6: TStaticText
        Left = 36
        Top = 271
        Width = 46
        Height = 17
        Caption = 'JEDI API'
        TabOrder = 9
      end
      object StaticText7: TStaticText
        Left = 244
        Top = 271
        Width = 57
        Height = 17
        Caption = 'JEDI WSCL'
        TabOrder = 10
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'JEDI API'
      object GroupBox1: TGroupBox
        Left = 4
        Top = 3
        Width = 445
        Height = 246
        Caption = 'JEDI Windows API'
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
          Top = 136
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
          Top = 182
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
          Top = 155
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
          Top = 182
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
    end
    object TabSheet2: TTabSheet
      Caption = 'JEDI WSCL'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox2: TGroupBox
        Left = 4
        Top = 3
        Width = 445
        Height = 246
        Caption = 'JEDI Windows Security Code Library'
        TabOrder = 0
        object Label6: TLabel
          Left = 16
          Top = 24
          Width = 124
          Height = 13
          Caption = 'Available release versions'
        end
        object Label7: TLabel
          Left = 16
          Top = 136
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
          Top = 182
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
          ItemHeight = 0
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
          Top = 155
          Width = 417
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
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
          Top = 182
          Width = 75
          Height = 25
          Action = RevisionJwsclReadHistoryAction
          TabOrder = 2
        end
        object Button2: TButton
          Left = 358
          Top = 70
          Width = 75
          Height = 25
          Action = ReleaseJwsclReadHistoryAction
          TabOrder = 3
        end
      end
    end
  end
  object JvCreateProcessJWA: TJvCreateProcess
    ApplicationName = 'svn.exe'
    CommandLine = 'svn.exe help'
    StartupInfo.ShowWindow = swHide
    StartupInfo.DefaultWindowState = False
    ConsoleOptions = [coOwnerData, coRedirect]
    OnTerminate = JvCPReleaseJwaTerminate
    OnRead = JvCPReleaseJWARead
    Left = 504
    Top = 72
  end
  object JvCreateProcessJWSCL: TJvCreateProcess
    ApplicationName = 'svn.exe'
    CommandLine = 'svn.exe help'
    StartupInfo.ShowWindow = swHide
    StartupInfo.DefaultWindowState = False
    ConsoleOptions = [coOwnerData, coRedirect]
    OnTerminate = JvCPReleaseJwsclTerminate
    OnRead = JvCPReleaseJwsclRead
    Left = 544
    Top = 280
  end
  object ActionList1: TActionList
    Left = 472
    object RevisionJwaReadHistoryAction: TAction
      Caption = 'Read history'
      OnExecute = ReadHistoryJwaButtonClick
      OnUpdate = RevisionJwaReadHistoryActionUpdate
    end
    object ReleaseJwaReadHistoryAction: TAction
      Caption = 'Read history'
      OnExecute = ReleaseJwaReadHistoryActionExecute
      OnUpdate = ReleaseJwaReadHistoryActionUpdate
    end
    object UpdateAction: TAction
      Caption = 'Receive version information'
      OnExecute = UpdateActionExecute
      OnUpdate = UpdateActionUpdate
    end
    object ReleaseJwsclReadHistoryAction: TAction
      Caption = 'Read history'
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
    Left = 504
    Top = 160
  end
  object JvCreateProcessRevisionJwscl: TJvCreateProcess
    ApplicationName = 'svn.exe'
    CommandLine = 'svn.exe help'
    StartupInfo.ShowWindow = swHide
    StartupInfo.DefaultWindowState = False
    ConsoleOptions = [coOwnerData, coRedirect]
    OnTerminate = JvCPRevisionJwsclTerminate
    OnRead = JvCPRevisionJwsclRead
    Left = 544
    Top = 360
  end
  object JvCreateProcessHistoryJWA: TJvCreateProcess
    ApplicationName = 'svn.exe'
    CommandLine = 'svn.exe help'
    StartupInfo.ShowWindow = swHide
    StartupInfo.DefaultWindowState = False
    ConsoleOptions = [coOwnerData, coRedirect]
    OnTerminate = JvCPHistoryJWATerminate
    OnRead = JvCPHistoryJWARead
    Left = 504
    Top = 112
  end
  object JvCreateProcessHistoryJwscl: TJvCreateProcess
    ApplicationName = 'svn.exe'
    CommandLine = 'svn.exe help'
    StartupInfo.ShowWindow = swHide
    StartupInfo.DefaultWindowState = False
    ConsoleOptions = [coOwnerData, coRedirect]
    OnTerminate = JvCPHistoryJwsclTerminate
    OnRead = JvCPHistoryJwsclRead
    Left = 544
    Top = 320
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
    OnConnectedToServer = JvHttpUrlGrabber1ConnectedToServer
    OnClosingConnection = JvHttpUrlGrabber1ClosingConnection
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
