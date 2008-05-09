{
This project is released under the terms of the
GNU General Public License 3.0 (the  "GPL License").

For more information about the GPL: http://www.gnu.org/licenses/gpl-3.0.txt

Original authors are
 	Philip Dittmann
  Christian Wimmer

This application is part of the JEDI API Project.
Visit at http://blog.delphi-jedi.net/
}
program CredentialsPrompt;

{$R *.res}

uses
  ExceptionLog,
  JwaWindows,
  JwsclEurekaLogUtils,
  SysUtils,
  Dialogs,
  Forms,
  Classes,
  JwsclDesktops,
  JwsclUtils,
  Graphics,
  CredentialsThread in 'CredentialsThread.pas',
  CredentialUtils in 'CredentialUtils.pas',
  uLogging in '..\Service Code\uLogging.pas',
  SessionPipe in '..\SessionPipe.pas';


procedure AttachedFilesRequestProc(EurekaExceptionRecord: TEurekaExceptionRecord;
    AttachedFiles: TStrings);
begin
  AttachedFiles.Add(LogFileNameLocation);
end;


procedure SwitchToDefault;
var Desk : TJwSecurityDesktop;
begin
  if TJwSecurityDesktops.IsStationLocked then
    exit;

  Desk := TJwSecurityDesktop.OpenDesktop(nil, false,'default',[],false, MAXIMUM_ALLOWED);
  try
    Desk.SwitchDesktop;
  finally
    Desk.Free;
  end;
end;

var
  Res : Integer;
  H : THandle;
  ConsentThread : TConsentThread;

begin

  if HasParameter('/debug') then
      MessageBox(0,'Debug breakpoint','',MB_ICONEXCLAMATION or MB_OK);


  ExceptionLog.CustomWebFieldsRequest := JEDI_WebFieldsRequestNotify;
  ExceptionLog.AttachedFilesRequest := AttachedFilesRequestProc;



 if not HasParameter('/cred') then
    halt(1);

  if HasParameter('/switchdefault') then
  begin
    SwitchToDefault;
    exit;
  end;

  try
    {If SvcMgr unit is included we have
    to free them before freeing Forms.Application
    }
    //SvcMgr.Application.Free;
    //SvcMgr.Application := nil;

    Forms.Application.Free;
  except
  end;

  Forms.Application := TApplication.Create(nil);
  Forms.Application.Initialize;


  uLogging.ApplicationFileName := 'JEDI XP CredentialsPrompt';

  uLogging.InitFileLocation;
  uLogging.InitLog;
  uLogging.SwitchLog(true);

  try

    ConsentThread := TConsentThread.CreateNewThread(false);
    try
      Res := ConsentThread.WaitFor;
    finally
      ConsentThread.Free;
    end;

    try
      Application.Free;
    except
    end;
    //creates mem leak
    Application := TApplication.Create(nil);
    Application.Initialize;

    { if Res = ERROR_INVALID_HANDLE then
        MessageDlg('1',mtError,[mbOK],0);     }
   // Application := Nil;
    //MessageDlg('123',mtWarning,mbOKCancel,0);
  finally
    uLogging.DoneLog;
  end;
  Halt(Res);
end.
