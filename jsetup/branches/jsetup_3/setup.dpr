program setup;

uses
  Forms,
  UMainForm in 'UMainForm.pas' {Form1},
  UTestFrame in 'UTestFrame.pas' {Frame2: TFrame},
  UInstallPage in 'UInstallPage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
