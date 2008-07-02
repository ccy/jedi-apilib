unit UDataModule;

interface

uses
  JwaWindows, Forms,
  SysUtils, Classes, Dialogs, JvCreateProcess;

type
  TSetupType = (stCheckout, stUpdate, stRemove);
  TProjectType = (ptJWA, ptJWSCL);
  TProjectTypes = set of TProjectType;

  TDelphiVersion = (dv5, dv6,dv7, dv8, dv2005, dv2006, sv2007);
  TDelphiVersions = set of TDelphiVersion;


  TDataModule1 = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    fSetupType : TSetupType;
    fProjectTypes : TProjectTypes;
    fPathJWA,
    fPathJWSCL : String;

    fDelphiJWA,
    fDelphiJWSCL : TDelphiVersions;

    FJvCreateProcess : TJvCreateProcess;

    procedure JvCreateProcess1Read(Sender: TObject; const S: string;
        const StartsOnNewLine: Boolean);
    
    procedure JvCreateProcess1Terminate(Sender: TObject;
        ExitCode: Cardinal);

  public
    function GetVersions(const ProjectType : TProjectType) : TStringList;

    { Public-Deklarationen }
    property SetupType : TSetupType read fSetupType write fSetupType;
    property ProjectTypes : TProjectTypes read fProjectTypes write fProjectTypes;

    property PathJWA   : String read fPathJWA write fPathJWA;
    property PathJWSCL : String read fPathJWSCL write fPathJWSCL;

    property DelphiJWA   : TDelphiVersions read fDelphiJWA write fDelphiJWA;
    property DelphiJWSCL : TDelphiVersions read fDelphiJWSCL write fDelphiJWSCL;
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.dfm}

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  FJvCreateProcess :=  TJvCreateProcess.Create(self);
end;

function TDataModule1.GetVersions(
  const ProjectType: TProjectType): TStringList;
var Lines : TStringList;
  C : Integer;
begin
  //FJvCreateProcess.CommandLine := 'svn.exe';
  FJvCreateProcess.CommandLine := 'cmd.exe';
  FJvCreateProcess.StartupInfo.ShowWindow := swHide;
  FJvCreateProcess.StartupInfo.DefaultWindowState := False;
//  FJvCreateProcess.CreationFlags := [{cfSuspended,} cfNewConsole,cfUnicode];

  FJvCreateProcess.OnRead := JvCreateProcess1Read;
  FJvCreateProcess.OnTerminate := JvCreateProcess1Terminate;

//  FJvCreateProcess.ConsoleOptions := [coRedirect];

  FJvCreateProcess.WaitForTerminate := true;
  FJvCreateProcess.Run;
//  ResumeThread(FJvCreateProcess.ProcessInfo.hThread);

  //WaitForSingleObject(FJvCreateProcess.ProcessInfo.hProcess, INFINITE);
 // Sleep(2000);

 { C := FJvCreateProcess.ConsoleOutput.Count;
  ShowMessage(FJvCreateProcess.ConsoleOutput.TExt);
  Application.Terminate;
  }
  result := TStringList.Create;
  //FJvCreateProcess.ConsoleOptions
  result.Assign(FJvCreateProcess.ConsoleOutput); 

end;

procedure TDataModule1.JvCreateProcess1Read(Sender: TObject; const S: string;
  const StartsOnNewLine: Boolean);
begin
//
  if StartsOnNewLine or
    (FJvCreateProcess.ConsoleOutput.Count = 0) then
    FJvCreateProcess.ConsoleOutput.Add(S)
  else
    FJvCreateProcess.ConsoleOutput[FJvCreateProcess.ConsoleOutput.Count-1]
     := FJvCreateProcess.ConsoleOutput[FJvCreateProcess.ConsoleOutput.Count-1] + S;
end;

procedure TDataModule1.JvCreateProcess1Terminate(Sender: TObject;
  ExitCode: Cardinal);
begin
  //
  ShowMessage(TJvCreateProcess(Sender).ConsoleOutput.TExt);
end;

end.
