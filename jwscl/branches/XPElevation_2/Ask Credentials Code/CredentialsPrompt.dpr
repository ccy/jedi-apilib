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
  JvLinkLabelTools,
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
  SessionPipe in '..\SessionPipe.pas',
  CredentialsForm in 'CredentialsForm.pas' {FormCredentials};

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
  ShRes,
  Res : Integer;
  H : THandle;
  ConsentThread : TConsentThread;
  S : String;

  ErrorValue,
  LastError : DWORD;
  IsServiceError : Boolean;
begin
  ExceptionLog.CustomWebFieldsRequest := JEDI_WebFieldsRequestNotify;
  ExceptionLog.AttachedFilesRequest := AttachedFilesRequestProc;


  if HasParameter('/cred') and HasParameter('/switchdefault') then
  begin
    SwitchToDefault;
    exit;
  end;



  //if HasParameter('/debug') then
      MessageBox(0,'Debug breakpoint','',MB_ICONEXCLAMATION or MB_OK);


 if not HasParameter('/cred') then
    halt(1);




  try
    {If SvcMgr unit is included we have
    to free them before freeing Forms.Application
    }
    //SvcMgr.Application.Free;
    //SvcMgr.Application := nil;

    Forms.Application.Free;
  except
  end;
  Forms.Application := nil;

{  Forms.Application := TApplication.Create(nil);
  Forms.Application.Initialize; }



  uLogging.ApplicationFileName := 'JEDI XP CredentialsPrompt';

  uLogging.InitFileLocation;
  uLogging.InitLog;
  uLogging.SwitchLog(true);

  ErrorValue := 0;
  LastError := 0;
  IsServiceError := false;
  try
    ConsentThread := TConsentThread.CreateNewThread(false);
    try
      Res := ConsentThread.WaitFor;
      if Res <> 0 then
      begin
        ErrorValue := ConsentThread.ErrorValue;
        LastError  := ConsentThread.LastError;
        IsServiceError := ConsentThread.IsServiceError;
      end;

      if ConsentThread.ShowWebPage then
      begin
        ShRes := ShellExecute(0, 'open', PChar('http://blog.delphi-jedi.net/'), nil, nil, SW_SHOWNORMAL);
      end;
    finally
      ConsentThread.Free;
    end;

    Forms.Application := TApplication.Create(nil);
    Forms.Application.Initialize;

    if Res <> 0 then
    begin
      if (ErrorValue = 0) and (LastError <> 0) then
      begin
        case LastError of
          ERROR_INVALID_HANDLE : S :='The given pipe handle is invalid.';
          ERROR_PIPE_NOT_CONNECTED : S:= 'Pipe connection could not be established';
          //ERROR_INVALID_DATA: S:= 'data from service is invalid';
          ERROR_BAD_FORMAT : S:= 'data from service is invalid';
          ERROR_INVALID_SIGNATURE : S:= 'data from service has invalid signature';
          ERROR_BAD_ENVIRONMENT: S:= 'secure desktop could not be established';
          ERROR_INVALID_PASSWORD: S:= 'credentials form failed';
          ERROR_PIPE_BUSY: S:= 'The service could not more be contacted. The reason is a broken '+
              'connection to the service. The service maybe disconnected from this application or was shutdown.'+
              'Try to restart the service.';
          ERROR_PIPE_LISTENING: S:= 'The communication to the service is broken. The service maybe disconnected from this application or was shutdown.'+
              'Try to restart the service.';
        else
          s := '';
        end;
        if Length(S) > 0  then
            MessageBox(0,PAnsiChar(S),'Error',MB_ICONERROR or MB_OK);
      end
      else
      if (ErrorValue <> 0) and (IsServiceError) then
      begin
        TConsentThread.ProcessLogonResult(ErrorValue, LastError);
      end;
    end;
  finally
    uLogging.DoneLog;
  end;
  Halt(Res);
end.
