program ThreadPool2;

{$APPTYPE CONSOLE}

uses
  JwaWindows,
  Forms,
  classes,
  sysutils,
  ThreadPoolForm in 'ThreadPoolForm.pas' {Form1},
  ProcessList in 'ProcessList.pas';

var
  M1, M2 : TProcessListMemory;
  Processes,
  Processes2: TProcessEntries;
  MEm : TFileStream;
begin
  M2 := TProcessListMemory.CreateOpen('test');
  M2.Read(Processes2);


  M2 := TProcessListMemory.CreateOpen('test');
  WaitForSingleObject(M2.Mutex, INFINITE);
  M2.Read(Processes2);
  ReleaseMutex(M2.Mutex)
end.
