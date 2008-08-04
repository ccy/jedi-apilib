program setup;

uses
  Forms,
  UMainForm in 'UMainForm.pas' {Form1},
  UInstallPage in 'UInstallPage.pas',
  UTestFrame in 'UTestFrame.pas' {Frame2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
