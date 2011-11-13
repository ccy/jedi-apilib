{******************************************************************************}
{ JEDI File Transactions API example                                           }
{ http://jedi-apilib.sourceforge.net                                           }
{ http://wiki.delphi-jedi.org/                                                 }
{ http://blog.delphi-jedi.net/                                                 }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ Author(s): Olaf Hess (olafhess at newsguy dot com)                           }
{ Creation date: December 1st 2009                                             }
{ Last modification date: November 28th 2010                                   }
{                                                                              }
{ Description: Demonstrates how to use the file system transactions introduced }
{              with Windows Vista / Server 2008                                }
{                                                                              }
{ Preparations: JWA must be ready to use.                                      }
{                                                                              }
{ Version history: December 1st 2009: initial version                          }
{                  November 28th 2010: adapted to use JWA                      }
{                                                                              }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in         }
{ production environments.                                                     }
{******************************************************************************}

//----------------------------------------------------------------------------//
// This code was first published as part of the article                       //
// "Transaktionen mit Vista und NTFS" ("Transactions with Vista and NTFS")    //
// in issue 1 / 2010 of "Toolbox Magazin" (http://www.toolbox-mag.de) written //
// by Olaf Hess. Used with permission. Donated to the JEDI API Project.       //
//----------------------------------------------------------------------------//

// This program uses the "TShellChangeNotifier" component. If you on loading
// this form see an error message that it is missing please install the
// "ShellControls" package from the "ShellControls" folder in the "Demos"
// folder of your Delphi installation (search for "ShellCtrls.pas").

{$I ..\..\Includes\jproject\jedi.inc}

{$IFDEF DELPHI7_UP}
    {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

unit MainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SyncObjs,
  TransactionClassU, ComCtrls, ExtCtrls, ShellCtrls;

type
  TDemoForm = class(TForm)
    DemoOrdnerGB: TGroupBox;
    DemoFolderEdit: TEdit;
    SelectDemoFolderBtn: TButton;
    ExplorerBtn: TButton;
    TransactionsGB: TGroupBox;
    OperationsGB: TGroupBox;
    NewTransactionBtn: TButton;
    CommitTransactionBtn: TButton;
    RollbackTransactionBtn: TButton;
    CreateFileBtn: TButton;
    RenameFilesBtn: TButton;
    DeleteFilesBtn: TButton;
    TransactedViewLB: TListBox;
    NormalViewLB: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    NewTimeoutBtn: TButton;
    StatusBar: TStatusBar;
    TransactionTimer: TTimer;
    ShellChangeNotifier: TShellChangeNotifier;

    procedure FormCreate(Sender: TObject);
    procedure NewTransactionBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CommitTransactionBtnClick(Sender: TObject);
    procedure RollbackTransactionBtnClick(Sender: TObject);
    procedure SelectDemoFolderBtnClick(Sender: TObject);
    procedure ExplorerBtnClick(Sender: TObject);
    procedure CreateFileBtnClick(Sender: TObject);
    procedure RenameFilesBtnClick(Sender: TObject);
    procedure DeleteFilesBtnClick(Sender: TObject);
    procedure TransactionTimerTimer(Sender: TObject);
    procedure ShellChangeNotifierChange;

  private
    Transaction : TTransaction;
    CS : TCriticalSection;

    procedure EnableOperationsButtons (const bEnable: Boolean);
    procedure ChangeTransactionButtonsStatus (const bInProgress: Boolean);
    procedure Update_Left_LB;
    procedure Update_Right_LB;

  public
    { Public declarations }
  end;

var
    DemoForm : TDemoForm;

function GetAppTitle : String;

implementation

{$R *.dfm}

uses StrUtils,
     GetFolderDialogU;

(* ---- *)

function GetAppTitle : String;
begin
    Result := ExtractFileName (ParamStr (0));
end; { GetAppTitle }

(* ---- *)

procedure TDemoForm.FormCreate (Sender: TObject);
begin
    Application.Title := Caption;
    CS := TCriticalSection.Create;

    EnableOperationsButtons (false);
end; { TDemoForm.FormCreate }

(* ---- *)

procedure TDemoForm.FormDestroy (Sender: TObject);
begin
    if (Assigned (Transaction)) then
        Transaction.Free;

    CS.Free;
end; { TDemoForm.FormDestroy }

(* ---- *)

procedure TDemoForm.SelectDemoFolderBtnClick (Sender: TObject);

var
    sDemoFolder : TFileName;

begin
    sDemoFolder := ExtractFileDir (Application.ExeName);

    if (GetFolderDialog (Handle, 'Select test folder', sDemoFolder)) then
    begin
        sDemoFolder := IncludeTrailingPathDelimiter (sDemoFolder) +
                       'TestFolder';

        if not (DirectoryExists (sDemoFolder)) then
            if (MessageDlg (Format ('Create folder "%s"?', [sDemoFolder]),
                            mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
            begin
                if not (CreateDir (sDemoFolder)) then
                begin
                    MessageDlg (Format ('Error creating folder "%s"!',
                                        [sDemoFolder]),
                                mtError, [mbOK], 0);
                    exit;
                end; { if }
            end { if }
            else exit;

        DemoFolderEdit.Text := sDemoFolder;
        ExplorerBtn.Enabled := true;
        NewTransactionBtn.SetFocus;
        SelectDemoFolderBtn.Enabled := false;

        Update_Left_LB;
        Update_Right_LB;

        ShellChangeNotifier.Root := sDemoFolder;
        ShellChangeNotifier.OnChange := ShellChangeNotifierChange;
    end; { if }
end; { TDemoForm.SelectDemoFolderBtnClick }

(* ---- *)

procedure TDemoForm.ExplorerBtnClick (Sender: TObject);

const
    cCmdLine = 'explorer.exe "%s"';

var
    sCmdLine : AnsiString;

begin
    sCmdLine := AnsiString (Format (cCmdLine, [DemoFolderEdit.Text]));
    WinExec (PAnsiChar (sCmdLine), sw_Show);
end; { TDemoForm.ExplorerBtnClick }

(* ---- *)

procedure TDemoForm.NewTransactionBtnClick (Sender: TObject);

    (* ---- *)

    function GetTimeout (out dwTimeout: DWord) : Boolean;

    var
        sTimeout : String;
        iTimeout : Integer;

    begin
        repeat
            sTimeout := '0';
            Result := InputQuery ('Create transaction with timeout',
                                  'Timeout (seconds):', sTimeout);

            if not (Result) then
                exit;

            iTimeout := StrToIntDef (sTimeout, 0);

            Result := iTimeout > 0;

            if (Result) then
                dwTimeout := DWord (iTimeout)
            else MessageDlg ('Please enter a number greater 0!',
                             mtError, [mbOK], 0);
        until (Result);
    end; { GetTimeout }

    (* ---- *)

const
    cAbortMsg = 'Please be aware that the transaction will automatically get ' +
                'aborted after %d seconds'#13'unless you perform a "Commit" ' +
                'before that period expires!';

var
    dwTimeout : DWord;

begin { TDemoForm.NewTransactionBtnClick }
    if (DemoFolderEdit.Text = '') then
    begin
        MessageDlg ('No "test folder" selected!', mtError, [mbOK], 0);
        SelectDemoFolderBtn.SetFocus;
        exit;
    end; { if }

    if (Assigned (Transaction)) then
    begin
        MessageDlg ('Transaction <> NIL', mtError, [mbOK], 0);
        exit;
    end; { if }

    if (Sender = NewTimeoutBtn) then
    begin
        if not (GetTimeout (dwTimeout)) then
            exit;

        MessageDlg (Format (cAbortMsg, [dwTimeout]), mtInformation, [mbOK], 0); 

        Transaction := TTransaction.CreateTimeout (Caption, dwTimeout * 1000);

        TransactionTimer.Enabled := true;
    end { if}
    else
    begin
        Transaction := TTransaction.Create (Caption);
        StatusBar.SimpleText := Format (' Transaction "%s" created', [Caption])
    end; { else }

    ChangeTransactionButtonsStatus (true);
end; { TDemoForm.NewTransactionBtnClick }

(* ---- *)

procedure TDemoForm.CommitTransactionBtnClick (Sender: TObject);
begin
    try
        Transaction.Commit;

    finally
        FreeAndNil (Transaction);
        ChangeTransactionButtonsStatus (false);
    end; { try / finally }
end; { TDemoForm.CommitTransactionBtnClick }

(* ---- *)

procedure TDemoForm.RollbackTransactionBtnClick (Sender: TObject);
begin
    try
        Transaction.Rollback;

    finally
        FreeAndNil (Transaction);
        ChangeTransactionButtonsStatus (false);
    end; { try / finally }
end; { TDemoForm.RollbackTransactionBtnClick }

(* ---- *)

procedure TDemoForm.CreateFileBtnClick (Sender: TObject);

    (* ---- *)

    function GetNewFileName : TFileName;

    var
        iCount : Integer;
        sFileName : TFileName;

    begin
        iCount := 0;

        repeat
            Inc (iCount);

            sFileName := Format ('File%3.d.txt', [iCount]);
            sFileName := AnsiReplaceStr (sFileName, ' ', '0');

            Result := Format ('%s\%s', [DemoFolderEdit.Text, sFileName]);
        until (Transaction.FileExists (Result) = false);
    end; { GetNewFileName }

    (* ---- *)

var
    sNewFileName : TFileName;
    hFile : Integer;

begin { TDemoForm.CreateFileBtnClick }
    sNewFileName := GetNewFileName;

    hFile := Transaction.FileCreate (sNewFileName);

    if (FileWrite (hFile, PChar (sNewFileName)^,
                   Length (sNewFileName)) <> (-1)) then
        FileClose (hFile);

    Update_Left_LB;
end; { TDemoForm.CreateFileBtnClick }

(* ---- *)

procedure TDemoForm.RenameFilesBtnClick (Sender: TObject);

var
    iIndex, iPos : Integer;
    sDir, sNewName : TFileName;

begin
    sDir := DemoFolderEdit.Text + '\';

    with TransactedViewLB do
        if (Items.Count = 0) then
            MessageDlg ('No files found!', mtError, [mbOK], 0)
        else
        begin
            for iIndex := 0 to Items.Count - 1 do
            begin
                sNewName := Items [iIndex];

                iPos := Pos ('.', sNewName);

                if (iPos = 0) then
                    Continue;

                Insert (Char (iIndex + $61), sNewName, iPos);

                Transaction.MoveFile (sDir + Items [iIndex], sDir + sNewName);
            end; { for }
            
            Update_Left_LB;
        end; { else }
end; { TDemoForm.RenameFilesBtnClick }

(* ---- *)

procedure TDemoForm.DeleteFilesBtnClick (Sender: TObject);

var
    iIndex : Integer;
    sDir : TFileName;

begin
    sDir := DemoFolderEdit.Text + '\';

    with TransactedViewLB do
        if (Items.Count = 0) then
            MessageDlg ('No files found!', mtError, [mbOK], 0)
        else
        begin
            for iIndex := 0 to Items.Count - 1 do
                Transaction.DeleteFile (sDir + Items [iIndex]);

            Update_Left_LB;
        end; { else }
end; { TDemoForm.DeleteFilesBtnClick }

(* ---- *)

procedure TDemoForm.TransactionTimerTimer (Sender: TObject);

const
    cStatusMsg = ' Time since start of the transaction: %d seconds';

begin
    with TransactionTimer do
    begin
        Tag := Tag + 1;
        StatusBar.SimpleText := Format (cStatusMsg, [Tag]);
    end; { with }
end; { TDemoForm.TimerTimer }

(* ---- *)

procedure TDemoForm.ShellChangeNotifierChange;
begin
    Update_Right_LB;
end; { TDemoForm.ShellChangeNotifierChange }

(* ---- *)

procedure TDemoForm.EnableOperationsButtons (const bEnable: Boolean);
begin
    CreateFileBtn.Enabled := bEnable;
    RenameFilesBtn.Enabled := bEnable;
    DeleteFilesBtn.Enabled := bEnable;
end; { TDemoForm.EnableOperationsButtons }

(* ---- *)

procedure TDemoForm.ChangeTransactionButtonsStatus (const bInProgress: Boolean);

    (* ---- *)

    procedure ResetTimer;
    begin
        TransactionTimer.Enabled := false;
        TransactionTimer.Tag := 0;

        StatusBar.SimpleText := '';
    end; { ResetTimer }

    (* ---- *)

begin
    if (bInProgress) then
    begin
        EnableOperationsButtons (true);
        CreateFileBtn.SetFocus;

        NewTransactionBtn.Enabled := false;
        NewTimeoutBtn.Enabled := false;

        CommitTransactionBtn.Enabled := true;
        RollbackTransactionBtn.Enabled := true;
    end { if }
    else
    begin
        ResetTimer;

        NewTransactionBtn.Enabled := true;
        NewTransactionBtn.SetFocus;
        NewTimeoutBtn.Enabled := true;

        CommitTransactionBtn.Enabled := false;
        RollbackTransactionBtn.Enabled := false;

        EnableOperationsButtons (false);
    end; { else }

    Update_Left_LB;
end; { TDemoForm.ChangeTransactionButtonsStatus }

(* ---- *)

procedure TDemoForm.Update_Left_LB;

var
    SearchRec : TSearchRec;

begin
    TransactedViewLB.Items.Clear;

    if (Assigned (Transaction)) then
    begin
        FillChar (SearchRec, SizeOf (TSearchRec), #0);

        if (Transaction.FindFirst (DemoFolderEdit.Text + '\*.*', faAnyFile,
                                   SearchRec) = 0) then
            repeat
                if (SearchRec.Attr and faDirectory = 0) then
                    TransactedViewLB.Items.Add (SearchRec.Name);
            until (FindNext (SearchRec) <> 0);

        FindClose (SearchRec);
    end; { if }
end; { TDemoForm.Update_Left_LB }

(* ---- *)

procedure TDemoForm.Update_Right_LB;

var
    SearchRec : TSearchRec;

begin
    CS.Acquire;

    try
        NormalViewLB.Items.Clear;

        FillChar (SearchRec, SizeOf (TSearchRec), #0);

        if (FindFirst (DemoFolderEdit.Text + '\*.*', faAnyFile,
                       SearchRec) = 0) then
            repeat
                if (SearchRec.Attr and faDirectory = 0) then
                    NormalViewLB.Items.Add (SearchRec.Name);
            until (FindNext (SearchRec) <> 0);

        FindClose (SearchRec);

    finally
        CS.Release;
    end; { try / finally }
end; { TDemoForm.Update_Right_LB }

(* ---- *)

end.

