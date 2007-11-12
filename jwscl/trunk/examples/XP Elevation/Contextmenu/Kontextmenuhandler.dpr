program Kontextmenuhandler;

uses
  SysUtils,
  JwaWindows;

var Pipe: THandle; str: String; Dummy: Cardinal;
begin
  if Paramcount<1 then
    exit;
  if not WaitNamedPipe('\\.\pipe\UAC in XP', 5000) then
  begin
    MessageBox(0, 'The UAC service is currently not available.', 'UAC in XP', MB_OK);
    exit;
  end;
  Pipe:=CreateFile('\\.\pipe\UAC in XP', GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);
  if Pipe=INVALID_HANDLE_VALUE then
  begin
    MessageBox(0, 'The communication pipe could not be created.', 'UAC in XP', MB_OK);
    Exit;
  end;
  try
    str:=Paramstr(1);
    WriteFile(Pipe, @str[1], Length(str), @Dummy, nil);
  finally
    CloseHandle(Pipe);
  end;
end.
