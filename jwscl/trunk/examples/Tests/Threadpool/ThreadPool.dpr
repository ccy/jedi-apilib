program ThreadPool;

uses
  Forms,
  ThreadPoolForm in 'ThreadPoolForm.pas' {Form1},
  ProcessList in 'ProcessList.pas';

{$R *.res}

var M1, M2 : TProcessListMemory;
  Processes,
  Processes2: TProcessEntries;
begin
  M1 := TProcessListMemory.Create('Global\test');

  SetLength(Processes,3);
  Processes[0].Handle := 1;
  Processes[1].Handle := 2;
  Processes[2].Handle := 3;

  M1.Write(Processes);


  M2 := TProcessListMemory.CreateOpen('Global\test');
  M2.Read(Processes2);

  exit;
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
