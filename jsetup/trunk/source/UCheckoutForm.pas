{$I jedi.inc}
unit UCheckoutForm;

interface


uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UPage, UProcessThread, JvComponentBase, JvCreateProcess,
  SyncObjsEx, UDataModule, ActnList
{$IFDEF DELPHI7}
  //you need an adapted version of Delphi2007 SyncObjs.pas to run in Delphi7
  ,SyncObjs11
{$ELSE}
  ,SyncObjs
{$ENDIF DELPH7}
  ;

type
  TSVNState = (ssList, ssLog);

  TCheckoutForm = class(TPageForm)
    Label1: TLabel;
    JvCreateProcessJWA: TJvCreateProcess;
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
    JvCreateProcessJWSCL: TJvCreateProcess;
    OutputMemo: TMemo;
    ActionList1: TActionList;
    RevisionJwaReadHistoryAction: TAction;
    ReleaseJwaReadHistoryAction: TAction;
    UpdateButton: TButton;
    UpdateAction: TAction;
    HTTPSCertCheckBox: TCheckBox;
    JvCreateProcessRevisionJWA: TJvCreateProcess;
    JvCreateProcessRevisionJwscl: TJvCreateProcess;
    JvCreateProcessHistoryJWA: TJvCreateProcess;
    JvCreateProcessHistoryJwscl: TJvCreateProcess;
    ReleaseJwsclReadHistoryAction: TAction;
    RevisionJwsclReadHistoryAction: TAction;

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

  private
    fSVNState : TSVNState;
    { Private-Deklarationen }
    fProcessThread : TProcessThread;
    fReadHistoryString : String;
    fWaitMessage : TForm;
    fWaitMessageCS : TCriticalSection;
    fCancelStatus : Boolean;

    fOutputWindow : TForm;

    function CheckCert(const Sender: TObject; var S: String; const StartsOnNewLine: Boolean) : Boolean;

    procedure AddOutput(const S: string;
      const StartsOnNewLine: Boolean);



    procedure GetVersions;
  public
    { Public-Deklarationen }
     function GetNextPageIndex : Integer; override;
  end;

var
  CheckoutForm: TCheckoutForm;

const
  JWA_URL = 'https://jedi-apilib.svn.sourceforge.net/svnroot/jedi-apilib/jwapi';
  JWA_BRANCHES = JWA_URL + '/branches';

  JWSCL_URL = 'https://jedi-apilib.svn.sourceforge.net/svnroot/jedi-apilib/jwscl';
  JWSCL_BRANCHES = JWSCL_URL + '/branches';

implementation
uses UMainForm;
{$R *.dfm}

{ TCheckoutForm }

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

  JvCreateProcessHistoryJWA.ApplicationName := 'svn.exe';
  JvCreateProcessHistoryJWA.CommandLine :=

   Format('svn.exe -r %s log %s/%s',
      [RevisionComboBoxJWA.Items[RevisionComboBoxJWA.ItemIndex],
       JWA_BRANCHES,
       ReleaseComboBoxJWA.Items[ReleaseComboBoxJWA.ItemIndex]
       ]);
  AddOutput(JvCreateProcessHistoryJWA.CommandLine,true);
  JvCreateProcessHistoryJWA.Run;
end;

procedure TCheckoutForm.ReleaseComboBoxJWAChange(Sender: TObject);
begin
  if ReleaseComboBoxJWA.ItemIndex < 0 then
    exit;

  JvCreateProcessRevisionJWA.ApplicationName := 'svn.exe';
  JvCreateProcessRevisionJWA.CommandLine :=
    Format('svn.exe log %s/%s',
     [JWA_BRANCHES,
     ReleaseComboBoxJWA.Items[ReleaseComboBoxJWA.ItemIndex]]);
  AddOutput(JvCreateProcessRevisionJWA.CommandLine,true);
  JvCreateProcessRevisionJWA.Run;
end;

procedure TCheckoutForm.ReleaseComboBoxJwsclChange(Sender: TObject);
begin
  if ReleaseComboBoxJwscl.ItemIndex < 0 then
    exit;      


  JvCreateProcessRevisionJwscl.ApplicationName := 'svn.exe';
  JvCreateProcessRevisionJwscl.CommandLine :=
    Format('svn.exe log %s/%s',
     [JWA_BRANCHES,
     ReleaseComboBoxJWA.Items[ReleaseComboBoxJWA.ItemIndex]]);
  AddOutput(JvCreateProcessRevisionJwscl.CommandLine,true);
  JvCreateProcessRevisionJwscl.Run;
end;

procedure TCheckoutForm.ReleaseJwaReadHistoryActionExecute(Sender: TObject);
begin
  if RevisionComboBoxJWA.Items.Count = 0 then
    exit;

  fReadHistoryString := '';

  JvCreateProcessHistoryJWA.CommandLine :=
   Format('svn.exe -r %s log %s/%s',
      [RevisionComboBoxJWA.Items[0],
       JWA_BRANCHES,
       ReleaseComboBoxJWA.Items[ReleaseComboBoxJWA.ItemIndex]
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

procedure TCheckoutForm.UpdateActionExecute(Sender: TObject);
begin
  GetVersions;
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

procedure TCheckoutForm.ReleaseJwsclReadHistoryActionExecute(Sender: TObject);
begin
  if RevisionComboBoxJwscl.Items.Count = 0 then
    exit;

  fReadHistoryString := '';

  JvCreateProcessHistoryJwscl.ApplicationName := 'svn.exe';
  JvCreateProcessHistoryJwscl.CommandLine :=

   Format('svn.exe -r %s log %s/%s',
      [RevisionComboBoxJwscl.Items[0],
       JWSCL_BRANCHES,
       ReleaseComboBoxJwscl.Items[ReleaseComboBoxJwscl.ItemIndex]
       ]);
  AddOutput(JvCreateProcessHistoryJwscl.CommandLine,true);
  JvCreateProcessHistoryJwscl.Run;
end;

procedure TCheckoutForm.RevisionJwsclReadHistoryActionExecute(Sender: TObject);
begin
  if RevisionComboBoxJwscl.Items.Count = 0 then
    exit;

  fReadHistoryString := '';

  JvCreateProcessHistoryJwscl.CommandLine :=
   Format('svn.exe -r %s log %s/%s',
      [RevisionComboBoxJwscl.Items[RevisionComboBoxJwscl.ItemIndex],
       JWSCL_BRANCHES,
       ReleaseComboBoxJwscl.Items[ReleaseComboBoxJwscl.ItemIndex]
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
end;

function TCheckoutForm.GetNextPageIndex: Integer;
begin
  //result := 3;
  result := -1;
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
    

    OutputMemo.Clear;

    fWaitMessage := CreateMessageDialog('The current version information is downloaded. Please be patient...',mtInformation, [mbCancel]);
    fWaitMessage.Parent := Self;
    Button := fWaitMessage.FindChildControl('Cancel') as TButton;
    Button.OnClick := CancelButtonClick;

    

    ReleaseComboBoxJwa.Items.Clear;
    RevisionComboBoxJWA.Items.Clear;
    ReleaseComboBoxJwscl.Items.Clear;
    RevisionComboBoxJwscl.Items.Clear;

    JvCreateProcessJWA.CommandLine := 'svn.exe list '+JWA_BRANCHES;
    AddOutput(JvCreateProcessJWA.CommandLine,true);
    JvCreateProcessJWA.Run;

    if fCancelStatus then
      exit;

    JvCreateProcessJwscl.ApplicationName := 'svn.exe';
    JvCreateProcessJwscl.CommandLine := 'svn.exe list '+JWSCL_BRANCHES;
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
