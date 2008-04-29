program AskCredentials;

//{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  JwaWindows,
  JwsclDescriptor,
  JwsclDesktops,
  JwsclCredentials,
  JwsclTypes,
  Graphics,
  Registry,
  Controls,
  Forms,
  MainForm in 'MainForm.pas' {FormMain},
  SessionPipe in '..\SessionPipe.pas',
  UMain in 'UMain.pas',
  CredentialsForm in 'CredentialsForm.pas' {FormCredentials},
  uLogging in '..\Service Code\uLogging.pas';

begin
  uLogging.ApplicationFileName := 'AskCredentials';
  uLogging.InitFileLocation;
  uLogging.InitLog;
  uLogging.SwitchLog(true);
  try
  //  MessageBox(0,'Debug breakpoint','',MB_ICONEXCLAMATION or MB_OK);
    Main;
  finally
    uLogging.DoneLog;
  end;
end.
