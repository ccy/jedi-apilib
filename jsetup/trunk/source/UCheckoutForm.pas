{$I jedi.inc}
unit UCheckoutForm;

interface


uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UPage, JvComponentBase, JvCreateProcess,
  SyncObjsEx, UDataModule, ActnList,
  JvUrlListGrabber, JvUrlGrabbers, JvBaseDlg, JvProgressDialog, ZipMstr {http://www.delphizip.org/}
{$IFDEF DELPHI7}
  //you need an adapted version of Delphi2007 SyncObjs.pas to run in Delphi7
  ,SyncObjs11
{$ELSE}
  ,SyncObjs, ComCtrls
{$ENDIF DELPH7}
  ;

type
  TSVNState = (ssList, ssLog);

  TCheckoutForm = class(TPageForm)
    JvCreateProcessJWA: TJvCreateProcess;
    JvCreateProcessJWSCL: TJvCreateProcess;
    OutputMemo: TMemo;
    ActionList1: TActionList;
    RevisionJwaReadHistoryAction: TAction;
    ReleaseJwaReadHistoryAction: TAction;
    UpdateAction: TAction;
    JvCreateProcessRevisionJWA: TJvCreateProcess;
    JvCreateProcessRevisionJwscl: TJvCreateProcess;
    JvCreateProcessHistoryJWA: TJvCreateProcess;
    JvCreateProcessHistoryJwscl: TJvCreateProcess;
    ReleaseJwsclReadHistoryAction: TAction;
    RevisionJwsclReadHistoryAction: TAction;
    JvHttpUrlGrabber1: TJvHttpUrlGrabber;
    JvProgressDialog1: TJvProgressDialog;
    ZipMaster1: TZipMaster;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ReleaseComboBoxJWA: TComboBox;
    RevisionComboBoxJWA: TComboBox;
    JWARevisionReadHistory: TButton;
    JWAReleaseReadHistory: TButton;
    GroupBox2: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ReleaseComboBoxJwscl: TComboBox;
    RevisionComboBoxJwscl: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    TabSheet3: TTabSheet;
    HTTPSCertCheckBox: TCheckBox;
    UpdateButton: TButton;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    JWAInfoListView: TListView;
    JwsclInfoListView: TListView;
    StaticText6: TStaticText;
    StaticText7: TStaticText;

    procedure JvCPReleaseJWARead(Sender: TObject; const S: string;
      const StartsOnNewLine: Boolean);
    procedure JvCPReleaseJwaTerminate(Sender: TObject; ExitCode: Cardinal);

    procedure JvCPRevisionJWARead(Sender: TObject; const S: string;
      const StartsOnNewLine: Boolean);

    procedure JvCPHistoryJWARead(Sender: TObject; const S: string;
      const StartsOnNewLine: Boolean);
    procedure JvCPHistoryJWATerminate(Sender: TObject; ExitCode: Cardinal);
    procedure JvCPRevisionJwsclTerminate(Sender: TObject; ExitCode: Cardinal);
    procedure ReadHistoryJwaButtonClick(Sender: TObject);
    procedure RevisionJwaReadHistoryActionUpdate(Sender: TObject);
    procedure ReleaseJwaReadHistoryActionExecute(Sender: TObject);
    procedure ReleaseJwaReadHistoryActionUpdate(Sender: TObject);
    procedure ReleaseComboBoxJWAChange(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure UpdateActionUpdate(Sender: TObject);
    procedure UpdateActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JvCPReleaseJwsclRead(Sender: TObject; const S: string;
      const StartsOnNewLine: Boolean);
    procedure JvCPRevisionJWATerminate(Sender: TObject; ExitCode: Cardinal);
    procedure JvCPReleaseJwsclTerminate(Sender: TObject;
      ExitCode: Cardinal);
    procedure ReleaseComboBoxJwsclChange(Sender: TObject);
    procedure JvCPRevisionJwsclRead(Sender: TObject; const S: string;
      const StartsOnNewLine: Boolean);
    procedure RevisionJwsclReadHistoryActionExecute(Sender: TObject);
    procedure JvCPHistoryJwsclRead(Sender: TObject; const S: string;
      const StartsOnNewLine: Boolean);
    procedure JvCPHistoryJwsclTerminate(Sender: TObject;
      ExitCode: Cardinal);
    procedure ReleaseJwsclReadHistoryActionExecute(Sender: TObject);
    procedure ReleaseJwsclReadHistoryActionUpdate(Sender: TObject);
    procedure RevisionJwsclReadHistoryActionUpdate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure JvHttpUrlGrabber1DoneFile(Sender: TObject; FileName: string;
      FileSize: Integer; Url: string);
    procedure JvHttpUrlGrabber1Progress(Sender: TObject; Position,
      TotalSize: Int64; Url: string; var Continue: Boolean);
    procedure JvHttpUrlGrabber1Error(Sender: TObject; ErrorMsg: string);
    procedure ZipMaster1TotalProgress(Sender: TObject; TotalSize: Int64;
      PerCent: Integer);
    procedure JvProgressDialog1Cancel(Sender: TObject);
    procedure JvCreateProcessSVNTestTerminate(Sender: TObject;
      ExitCode: Cardinal);
    procedure JvHttpUrlGrabber1ClosingConnection(Sender: TObject);
    procedure JvHttpUrlGrabber1ConnectedToServer(Sender: TObject);
    procedure TabSheet3Enter(Sender: TObject);

  private
    fSVNState : TSVNState;
    { Private-Deklarationen }
    fReadHistoryString : String;
    fWaitMessage : TForm;
    fWaitMessageCS : TCriticalSection;
    fCancelStatus : Boolean;

    fOutputWindow : TForm;

    function CheckCert(const Sender: TObject; var S: String; const StartsOnNewLine: Boolean) : Boolean;

    procedure AddOutput(const S: string;
      const StartsOnNewLine: Boolean);

    procedure UpdateVersionListViews;

    procedure GetNextUpdate(Sender : TObject); override;

    procedure GetVersions;
  public
    { Public-Deklarationen }
     function GetNextPageIndex(const showGui : Boolean) : Integer; override;
  end;

var
  CheckoutForm: TCheckoutForm;
  SVNPath : String = '';

const
  JWA_URL = 'https://jedi-apilib.svn.sourceforge.net/svnroot/jedi-apilib/jwapi';
  JWA_BRANCHES = JWA_URL + '/branches';
  JWA_TRUNK = JWA_URL + '/trunk';

  JWSCL_URL = 'https://jedi-apilib.svn.sourceforge.net/svnroot/jedi-apilib/jwscl';
  JWSCL_BRANCHES = JWSCL_URL + '/branches';
  JWSCL_TRUNK = JWSCL_URL + '/trunk';

implementation
uses UMainForm;
{$R *.dfm}

function GetSVNExe(const IncludeQuotes : Boolean = false) : String;
begin
  if not IncludeQuotes then
    result := SVNPath+'svn.exe'
  else
    result := '"'+SVNPath+'svn.exe"';
end;

function GetSvnUrl(const ComboBox : TComboBox; const Jwa : Boolean) : String;
  function GetTrunk : String;
  begin
    if Jwa then
      result := JWA_TRUNK
    else
      result := JWSCL_TRUNK;
  end;
  function GetBranch : String;
  begin
    if Jwa then
      result := JWA_BRANCHES+'/'+ComboBox.Items[ComboBox.ItemIndex]
    else
      result := JWSCL_BRANCHES+'/'+ComboBox.Items[ComboBox.ItemIndex];
  end;
begin
  if Integer(ComboBox.Items.Objects[ComboBox.ItemIndex]) = 1 then
    result := GetTrunk
  else
    result := GetBranch;
end;

function IncludeTrailingBackslash(const S: string): string;
begin
  Result := S;
  if not IsPathDelimiter(Result, Length(Result)) then Result := Result + '\';
end;

{ TCheckoutForm }
procedure TCheckoutForm.GetNextUpdate(Sender: TObject);
  function CorrectComboBox(const Box : TComboBox) : Boolean;
  begin
    result := Assigned(Box) and
      (Box.Items.Count > 0) and (Box.ItemIndex >= 0);
  end;

var B : Boolean;
begin
  B := FileExists(GetSVNExe);
//  inherited;
  if (Sender is TAction) then
  begin
    B := B and
      (JvCreateProcessHistoryJwscl.State = psReady) and
      (JvCreateProcessHistoryJWA.State   = psReady) and
      (JvCreateProcessJwa.State   = psReady) and
      (JvCreateProcessJwscl.State = psReady) and

      CorrectComboBox(ReleaseComboBoxJWA) and
      CorrectComboBox(RevisionComboBoxJWA) and
      CorrectComboBox(ReleaseComboBoxJwscl) and
      CorrectComboBox(RevisionComboBoxJwscl);

    (Sender as TAction).Enabled := B;
  end;
end;


procedure TCheckoutForm.AddOutput(const S: string;
  const StartsOnNewLine: Boolean);
begin
if StartsOnNewLine or
    (OutputMemo.Lines.Count = 0) then
    OutputMemo.Lines.Add(S)
  else
    OutputMemo.Lines[OutputMemo.Lines.Count-1]
     := OutputMemo.Lines[OutputMemo.Lines.Count-1] + S;
end;

procedure TCheckoutForm.ReadHistoryJwaButtonClick(Sender: TObject);
begin
  if RevisionComboBoxJWA.Items.Count = 0 then
    exit;

  fReadHistoryString := '';

  JvCreateProcessHistoryJWA.ApplicationName := GetSVNExe;
  JvCreateProcessHistoryJWA.CommandLine :=

   Format('%s -r %s log %s',
      [GetSVNExe(true),
       RevisionComboBoxJWA.Items[RevisionComboBoxJWA.ItemIndex],
       GetSvnUrl(ReleaseComboBoxJWA,true)
       ]);
  AddOutput(JvCreateProcessHistoryJWA.CommandLine,true);
  JvCreateProcessHistoryJWA.Run;
end;

procedure TCheckoutForm.ReleaseComboBoxJWAChange(Sender: TObject);
begin
  if ReleaseComboBoxJWA.ItemIndex < 0 then
    exit;

  JvCreateProcessRevisionJWA.ApplicationName := GetSVNExe;
  JvCreateProcessRevisionJWA.CommandLine :=
    Format('%s log %s',
     [GetSVNExe(true),
     GetSvnUrl(ReleaseComboBoxJWA,true)]
     );
  AddOutput(JvCreateProcessRevisionJWA.CommandLine,true);
  JvCreateProcessRevisionJWA.Run;
end;

procedure TCheckoutForm.ReleaseComboBoxJwsclChange(Sender: TObject);
begin
  if ReleaseComboBoxJwscl.ItemIndex < 0 then
    exit;      


  JvCreateProcessRevisionJwscl.ApplicationName := GetSVNExe;
  JvCreateProcessRevisionJwscl.CommandLine :=
    Format('%s log %s',
     [GetSVNExe(true),
     GetSvnUrl(ReleaseComboBoxJwscl,false)]
     );

  AddOutput(JvCreateProcessRevisionJwscl.CommandLine,true);
  JvCreateProcessRevisionJwscl.Run;
end;

procedure TCheckoutForm.ReleaseJwaReadHistoryActionExecute(Sender: TObject);
begin
  if RevisionComboBoxJWA.Items.Count = 0 then
    exit;

  fReadHistoryString := '';

  JvCreateProcessHistoryJWA.ApplicationName := GetSVNExe(false);
  JvCreateProcessHistoryJWA.CommandLine :=
   Format('%s -r %s log %s',
      [GetSVNExe(true),
       RevisionComboBoxJWA.Items[0],
       GetSvnUrl(ReleaseComboBoxJWA,true)

       ]);
  AddOutput(JvCreateProcessHistoryJWA.CommandLine,true);
  JvCreateProcessHistoryJWA.Run;
end;

procedure TCheckoutForm.ReleaseJwaReadHistoryActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    (JvCreateProcessHistoryJwscl.State = psReady) and
    (JvCreateProcessHistoryJWA.State = psReady) and
    (JvCreateProcessJwa.State = psReady) and
    (ReleaseComboBoxJWA.Items.Count > 0)
    and (ReleaseComboBoxJWA.ItemIndex >= 0);
end;

procedure TCheckoutForm.ReleaseJwsclReadHistoryActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
   (JvCreateProcessHistoryJwa.State = psReady) and
   (JvCreateProcessHistoryJwscl.State = psReady) and
   (JvCreateProcessJwscl.State = psReady) and
   (RevisionComboBoxJwscl.Items.Count > 0)
    and (RevisionComboBoxJwscl.ItemIndex >= 0);
end;

procedure TCheckoutForm.RevisionJwaReadHistoryActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
   (JvCreateProcessHistoryJwscl.State = psReady) and
   (JvCreateProcessHistoryJWA.State = psReady) and
   (JvCreateProcessJwa.State = psReady) and
   (RevisionComboBoxJWA.Items.Count > 0)
    and (RevisionComboBoxJWA.ItemIndex >= 0);
end;

function CheckAllDllFiles : Boolean;
begin
  result := FileExists(SVNPath+'ssleay32.dll') and
     FileExists(SVNPath+'libeay32.dll') and
     FileExists(SVNPath+'libdb44.dll') and
     FileExists(SVNPath+'libaprutil-1.dll') and
     FileExists(SVNPath+'libapriconv-1.dll') and
     FileExists(SVNPath+'libapr-1.dll') and
     FileExists(SVNPath+'intl3_svn.dll');
end;

procedure TCheckoutForm.UpdateActionExecute(Sender: TObject);

var
  tmpPath : array[0..MAX_PATH] of AnsiChar;
  Path : AnsiString;
  Error : DWORD;
  SI : TStartupInfo;
  PI : TProcessInformation;
begin
    //GetVersions;
  //
  ZeroMemory(@tmpPath, sizeof(tmpPath));
  GetTempPath(sizeof(tmpPath), @tmpPath);
  SvnPath := IncludeTrailingBackslash(tmpPath)+'svnclienttemp\';

  OutputMemo.Clear;

  Error := 0;
  if not (FileExists(GetSVNExe(false)) and CheckAllDllFiles)
      then
  begin
    JvProgressDialog1.InitValues(0,0,1,0,'Downloading...','Getting subversion client.');
    JvProgressDialog1.Show;

    JvHttpUrlGrabber1.Url := 'http://wimmer.teamchris.info/prog/.misc/bin.zip';
    //JvHttpUrlGrabber1.Url := 'http://wimmer.teamchris.info/prog/.misc/whoami.zip';
    //JvHttpUrlGrabber1.Url := 'http://subversion.tigris.org/files/documents/15/41077/svn-win32-1.4.6.zip';


    if ForceDirectories(SvnPath) then
    begin
      JvHttpUrlGrabber1.FileName := SvnPath+'svnclient.zip';

      Enabled := false;
      JvHttpUrlGrabber1.Start;
    end
    else
    begin
      ShowMessage('Could not create '+SvnPath);
    end;
  end
  else
  begin
    OutputMemo.Lines.Add(Format('Found svn at %s',[SVNPath]));
    GetVersions;
  end;
end;

procedure TCheckoutForm.UpdateActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
   ((JvCreateProcessJwa.State = psReady) and
    (JvCreateProcessJwscl.State = psReady) and
    (JvCreateProcessRevisionJWA.State = psReady) and
    (JvCreateProcessRevisionJwscl.State = psReady) and
    (JvCreateProcessHistoryJWA.State = psReady) and
    (JvCreateProcessHistoryJwscl.State = psReady)
    );
end;

procedure TCheckoutForm.UpdateButtonClick(Sender: TObject);
begin
  GetVersions;
end;

procedure TCheckoutForm.UpdateVersionListViews;
begin
  if ReleaseComboBoxJWA.ItemIndex >= 0 then
    JWAInfoListView.Items[0].SubItems[0] := ReleaseComboBoxJWA.Items[ReleaseComboBoxJWA.ItemIndex]
  else
    JWAInfoListView.Items[0].SubItems[0] := '';

  if RevisionComboBoxJWA.ItemIndex >= 0 then
    JWAInfoListView.Items[1].SubItems[0] := RevisionComboBoxJWA.Items[RevisionComboBoxJWA.ItemIndex]
  else
    JWAInfoListView.Items[1].SubItems[0] := '';

  if ReleaseComboBoxJwscl.ItemIndex >= 0 then
    JwsclInfoListView.Items[0].SubItems[0] := ReleaseComboBoxJwscl.Items[ReleaseComboBoxJwscl.ItemIndex]
  else
    JwsclInfoListView.Items[0].SubItems[0] := '';

  if RevisionComboBoxJwscl.ItemIndex >= 0 then
    JwsclInfoListView.Items[1].SubItems[0] := RevisionComboBoxJwscl.Items[RevisionComboBoxJwscl.ItemIndex]
  else
    JwsclInfoListView.Items[1].SubItems[0] := '';
end;

procedure TCheckoutForm.ZipMaster1TotalProgress(Sender: TObject;
  TotalSize: Int64; PerCent: Integer);
begin
  JvProgressDialog1.Max := 100;
  JvProgressDialog1.Position := PerCent;
  Sleep(10);

  Application.ProcessMessages;
end;

procedure TCheckoutForm.ReleaseJwsclReadHistoryActionExecute(Sender: TObject);
begin
  if RevisionComboBoxJwscl.Items.Count = 0 then
    exit;

  fReadHistoryString := '';

  JvCreateProcessHistoryJwscl.ApplicationName := GetSVNExe;
  JvCreateProcessHistoryJwscl.CommandLine :=

   Format('%s -r %s log %s',
      [GetSVNExe(true),
       RevisionComboBoxJwscl.Items[0],
       GetSvnUrl(ReleaseComboBoxJwscl,false)

       ]);
  AddOutput(JvCreateProcessHistoryJwscl.CommandLine,true);
  JvCreateProcessHistoryJwscl.Run;
end;

procedure TCheckoutForm.RevisionJwsclReadHistoryActionExecute(Sender: TObject);
begin
  if RevisionComboBoxJwscl.Items.Count = 0 then
    exit;

  fReadHistoryString := '';

  JvCreateProcessHistoryJWA.ApplicationName := GetSVNExe(false);
  JvCreateProcessHistoryJwscl.CommandLine :=
   Format('%s -r %s log %s',
      [GetSVNExe(true),
       RevisionComboBoxJwscl.Items[RevisionComboBoxJwscl.ItemIndex],
       GetSvnUrl(ReleaseComboBoxJwscl,false)

       ]);
  AddOutput(JvCreateProcessHistoryJwscl.CommandLine,true);
  JvCreateProcessHistoryJwscl.Run;
end;

procedure TCheckoutForm.RevisionJwsclReadHistoryActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled :=
    (JvCreateProcessHistoryJwa.State = psReady) and
    (JvCreateProcessHistoryJwscl.State = psReady) and
    (JvCreateProcessJwscl.State = psReady) and
    (RevisionComboBoxJwscl.Items.Count > 0)
    and (RevisionComboBoxJwscl.ItemIndex >= 0);
end;

procedure TCheckoutForm.TabSheet3Enter(Sender: TObject);
begin
  UpdateVersionListViews;
end;

procedure TCheckoutForm.CancelButtonClick(Sender: TObject);
begin
//
  fWaitMessageCS.Enter;
  try
    fCancelStatus := true;
    try
      JvCreateProcessJwa.Terminate;
    except
    end;
    try
      JvCreateProcessJwscl.Terminate;
    except
    end;

    //FreeAndNil(fWaitMessage); //don't do this!!
    fWaitMessage.Hide;
    Enabled := true;
  finally
    fWaitMessageCS.Leave;
  end;
end;

function TCheckoutForm.CheckCert(const Sender: TObject; var S: String; const StartsOnNewLine: Boolean) : Boolean;
var i : Integer;
begin
  result := pos('(R)eject', S) > 0;
  if result then
   begin
     if HTTPSCertCheckBox.Checked then
     begin
       TJvCreateProcess(Sender).Writeln('p');
       AddOutput(S + 'p', true);
     end
     else
     begin
       TJvCreateProcess(Sender).Writeln('t');
       AddOutput(S + 't', true);
     end;


     if not StartsOnNewLine then
     begin
       i := pos('?',S);
       Delete(S, 1,i+1);
       AddOutput(S, true);
     end;
   end;
end;

procedure TCheckoutForm.FormActivate(Sender: TObject);
begin
  if Sender is TMainForm then
  begin

   // GetVersions;

  end;
end;

procedure TCheckoutForm.FormCreate(Sender: TObject);
begin
  fOutputWindow := TForm.Create(self);
  fOutputWindow.SetBounds(0,0, 500, 200);
  fOutputWindow.Caption := 'svn output window';
  fOutputWindow.BorderStyle := bsSizeToolWin;

  OutputMemo.Parent := fOutputWindow;
  OutputMemo.Align := alClient;

  fWaitMessageCS := TCriticalSection.Create;

  ReleaseComboBoxJwa.Items.Clear;
  RevisionComboBoxJWA.Items.Clear;
  ReleaseComboBoxJwscl.Items.Clear;
  RevisionComboBoxJwscl.Items.Clear;
end;

function TCheckoutForm.GetNextPageIndex(const showGui : Boolean): Integer;
begin
  result := 3;
  //result := -1;
end;

procedure TCheckoutForm.GetVersions;
var
  Button : TButton;

  Entered : Boolean;
begin
  if (JvCreateProcessJwa.State <> psReady) or
    (JvCreateProcessJwscl.State <> psReady) then
    exit;

  try
    fOutputWindow.Show;

    fCancelStatus := False;

    fWaitMessage.Free;
    

    fWaitMessage := CreateMessageDialog('The current version information is being downloaded. Please be patient...',mtInformation, [mbCancel]);
    fWaitMessage.Parent := Self;
    Button := fWaitMessage.FindChildControl('Cancel') as TButton;
    Button.OnClick := CancelButtonClick;

    

    ReleaseComboBoxJwa.Items.Clear;
    RevisionComboBoxJWA.Items.Clear;
    ReleaseComboBoxJwscl.Items.Clear;
    RevisionComboBoxJwscl.Items.Clear;

    ReleaseComboBoxJwa.Items.AddObject('developer version',Pointer(1));
    ReleaseComboBoxJwscl.Items.AddObject('developer version',Pointer(1));

    JvCreateProcessJwa.ApplicationName := GetSVNExe;
    JvCreateProcessJWA.CommandLine := GetSVNExe(true)+' list '+JWA_BRANCHES;
    AddOutput(JvCreateProcessJWA.CommandLine,true);
    JvCreateProcessJWA.Run;

    if fCancelStatus then
      exit;

    JvCreateProcessJwscl.ApplicationName := GetSVNExe;
    JvCreateProcessJwscl.CommandLine := GetSVNExe(true)+' list '+JWSCL_BRANCHES;
    AddOutput(JvCreateProcessJwscl.CommandLine,true);

    JvCreateProcessJwscl.Run;
    //wartet nicht, da wir hier in der VCL nicht warten dürfen!


    Enabled := false;
    fWaitMessage.Show;
  except
  end;
end;

procedure TCheckoutForm.JvCPReleaseJWARead(Sender: TObject; const S: string;
  const StartsOnNewLine: Boolean);
var S2 : String;
begin
  S2 := S;
  if not CheckCert(Sender, S2, StartsOnNewLine) then
    AddOutput(S2, StartsOnNewLine);

  if (Length(S2) = 0) then
    exit;

  if (StrToIntDef(S2[1],-1) <> -1) then
  begin
    ReleaseComboBoxJwa.Items.Add(S2);
  end;
end;

procedure TCheckoutForm.JvCPReleaseJwaTerminate(Sender: TObject;
  ExitCode: Cardinal);
begin
  ReleaseComboBoxJwa.ItemIndex := 0;
  ReleaseComboBoxJwa.Sorted := true;

  ReleaseComboBoxJWAChange(Sender);
end;

procedure TCheckoutForm.JvCPReleaseJwsclRead(Sender: TObject; const S: string;
  const StartsOnNewLine: Boolean);
var S2 : String;
begin
  S2 := S;
  if not CheckCert(Sender, S2, StartsOnNewLine) then
    AddOutput(S2, StartsOnNewLine);

  if (Length(S2) > 0)
   and (StrToIntDef(S2[1],-1) <> -1) then
    ReleaseComboBoxJwscl.Items.Add(S2);
end;

procedure TCheckoutForm.JvCPRevisionJwsclTerminate(Sender: TObject;
  ExitCode: Cardinal);
begin
  RevisionComboBoxJwscl.ItemIndex := 0;
//  RevisionComboBoxJwscl.Sorted := true;

  fWaitMessageCS.Enter;
  try
    if JvCreateProcessJwa.State = psReady then
    begin
      FreeAndNil(fWaitMessage);
      Enabled := true;
    end;
  finally
    fWaitMessageCS.Leave;
  end;

  UpdateVersionListViews;
end;


procedure TCheckoutForm.JvCreateProcessSVNTestTerminate(Sender: TObject;
  ExitCode: Cardinal);
begin
  //
end;

procedure TCheckoutForm.JvHttpUrlGrabber1ClosingConnection(Sender: TObject);
begin
//  ShowMessage('');
end;

procedure TCheckoutForm.JvHttpUrlGrabber1ConnectedToServer(Sender: TObject);
begin
// ShowMessage('');
end;

procedure TCheckoutForm.JvHttpUrlGrabber1DoneFile(Sender: TObject;
  FileName: string; FileSize: Integer; Url: string);
begin
  //
  //GetVersions;
  ShowMessage(FileName);
  if FileSize < 4000 then
  begin
    ShowMessage('Subversion client could not be downloaded.');
    JvProgressDialog1.Hide;
    Enabled := true;
  end
  else
  begin
    try
      JvProgressDialog1.Text := 'Extracting client...';

     // Sleep(1000);
      Application.ProcessMessages;


      ZipMaster1.ZipFileName := FileName;
      ZipMaster1.ExtrOptions := [ExtrOverWrite, ExtrForceDirs];
      ZipMaster1.FSpecArgs.Clear;
      ZipMaster1.FSpecArgs.Add('*svn.exe');
      ZipMaster1.FSpecArgs.Add('*.DLL');

      //ZipMaster1.FSpecArgs.Add('*.*');
      ZipMaster1.ExtrBaseDir := SVNPath;
      if ZipMaster1.Extract <> 0 then
      begin
        ShowMessage(ZipMaster1.Message);
        exit;
      end;

      if not (FileExists(GetSVNExe(false)) and
        CheckAllDllFiles) then
      begin
        ShowMessage('Svn command line client could not be found.');
        exit;
      end;    

      OutputMemo.Lines.Add('*************************************');
      OutputMemo.Lines.Add(Format('Successfully downloaded subversion to: %s',[SVNPath]));
    finally
      JvProgressDialog1.Hide;
      Enabled := true;
    end;

    GetVersions;
  end;
end;

procedure TCheckoutForm.JvHttpUrlGrabber1Error(Sender: TObject;
  ErrorMsg: string);
begin
  //
  ShowMessage(ErrorMsg);
  JvProgressDialog1.Hide;

  Enabled := true;
end;

procedure TCheckoutForm.JvHttpUrlGrabber1Progress(Sender: TObject; Position,
  TotalSize: Int64; Url: string; var Continue: Boolean);
begin
  JvProgressDialog1.Max := TotalSize;
  JvProgressDialog1.Position := Position;
end;

procedure TCheckoutForm.JvProgressDialog1Cancel(Sender: TObject);
begin
  //
  try
    JvHttpUrlGrabber1.Stop;
  except
  end;
  try
    ZipMaster1.Cancel;
  except
  end;
end;

procedure TCheckoutForm.JvCPHistoryJwsclTerminate(Sender: TObject;
  ExitCode: Cardinal);
begin
  ShowMessage(fReadHistoryString);
end;

procedure TCheckoutForm.JvCPHistoryJwsclRead(Sender: TObject;
  const S: string; const StartsOnNewLine: Boolean);
var S2 : String;
begin
  S2 := S;

  if not CheckCert(Sender, S2, StartsOnNewLine) then
    AddOutput(S2, StartsOnNewLine);

  if (Length(S2) > 1) and (S2[1] <> '-') and (S2[2] <> '-') then 
  if StartsOnNewLine then
    fReadHistoryString := fReadHistoryString + #13#10 + S2
  else
    fReadHistoryString := fReadHistoryString + S2;
end;

procedure TCheckoutForm.JvCPRevisionJwsclRead(Sender: TObject; const S: string;
  const StartsOnNewLine: Boolean);
var
  S2,S3 : String;
  i : Integer;
begin
  S2 := S;
//
  if not CheckCert(Sender, S2, StartsOnNewLine) then
    AddOutput(S2, StartsOnNewLine);


  if (Length(S2) = 0) then
    exit;

  if (S2[1] = '-') then
    exit;

  if (S2[1] = 'r') then
  begin

    i := pos(' ',S2);
    //BUG BUG
    S3 := Copy(S2, 2, i-1);

    RevisionComboBoxJwscl.Items.Add(S3);
  end;
end;

procedure TCheckoutForm.JvCPReleaseJwsclTerminate(Sender: TObject;
  ExitCode: Cardinal);
begin
  ReleaseComboBoxJwscl.ItemIndex := 0;
  ReleaseComboBoxJwscl.Sorted := true;

  ReleaseComboBoxJwsclChange(Sender);
end;

procedure TCheckoutForm.JvCPRevisionJWARead(Sender: TObject; const S: string;
  const StartsOnNewLine: Boolean);
var
  S2,S3 : String;
  i : Integer;
begin
  S2 := S;
//
  if not CheckCert(Sender, S2, StartsOnNewLine) then
    AddOutput(S2, StartsOnNewLine);


  if (Length(S2) = 0) then
    exit;

  if (S2[1] = '-') then
    exit;

  if (S2[1] = 'r') then
  begin

    i := pos(' ',S2);
    //BUG BUG
    S3 := Copy(S2, 2, i-1);

    RevisionComboBoxJWA.Items.Add(S3);
  end;
end;

procedure TCheckoutForm.JvCPRevisionJWATerminate(Sender: TObject;
  ExitCode: Cardinal);
begin
  RevisionComboBoxJWA.ItemIndex := 0;

  fWaitMessageCS.Enter;
  try
    if JvCreateProcessJwscl.State = psReady then
    begin
      FreeAndNil(fWaitMessage);
      Enabled := true;
    end;
  finally
    fWaitMessageCS.Leave;
  end;

  UpdateVersionListViews;
end;

procedure TCheckoutForm.JvCPHistoryJWARead(Sender: TObject; const S: string;
  const StartsOnNewLine: Boolean);
var S2 : String;
begin
  S2 := S;

  if not CheckCert(Sender, S2, StartsOnNewLine) then
    AddOutput(S2, StartsOnNewLine);

  if (Length(S2) > 1) and (S2[1] <> '-') and (S2[2] <> '-') then 
  if StartsOnNewLine then
    fReadHistoryString := fReadHistoryString + #13#10 + S2
  else
    fReadHistoryString := fReadHistoryString + S2;
end;

procedure TCheckoutForm.JvCPHistoryJWATerminate(Sender: TObject;
  ExitCode: Cardinal);
begin
  //
  ShowMessage(fReadHistoryString);
end;

procedure TCheckoutForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fWaitMessageCS);
  FreeAndNil(fWaitMessage);
end;

end.
