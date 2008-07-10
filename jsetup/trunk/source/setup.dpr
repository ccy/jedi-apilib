{$I jedi.inc}
program setup;

uses
  ExceptionLog,
  Forms,
  UWelcomeForm in 'UWelcomeForm.pas' {WelcomeForm},
  USetupTypeForm in 'USetupTypeForm.pas' {SetupTypeForm},
  UCheckoutForm in 'UCheckoutForm.pas' {CheckoutForm},
  UMainForm in 'UMainForm.pas' {MainForm},
  UPage in 'UPage.pas',
  UPathForm in 'UPathForm.pas' {PathForm},
  UDelphiForm in 'UDelphiForm.pas' {DelphiForm},
  UInstallation in 'UInstallation.pas' {InstallationForm},
  UReview in 'UReview.pas' {CompilationForm},
  UDataModule in 'UDataModule.pas' {DataModule1: TDataModule},
  UFinishedForm in 'UFinishedForm.pas' {FinishedForm},
  USetupModule in 'USetupModule.pas' {DataModule3: TDataModule};

{$R *.res}

begin
  Application.Initialize;
{$IFDEF DELPHI11_UP}
  Application.MainFormOnTaskbar := True;
{$ENDIF DELPHI11_UP}
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TDataModule3, DataModule3);
  Application.Run;
end.
