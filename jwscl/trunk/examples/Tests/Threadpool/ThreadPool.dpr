program ThreadPool;

uses
  ExceptionLog,
  Forms,
  ThreadPoolForm in 'ThreadPoolForm.pas' {Form1},
  ProcessList in 'ProcessList.pas';

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
