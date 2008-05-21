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
  Controls,
  JwaWindows,
  JwsclEurekaLogUtils,
  JvLinkLabelTools,
  SysUtils,
  Dialogs,
  Forms,
  Classes,
  ActiveX,
  JwsclDesktops,
  JwsclUtils,
  JwsclToken,
  JwsclTypes,
  JwsclKnownSid,
  JwsclStrings,
  JwaVista,
  Graphics,
  CredentialsThread in 'CredentialsThread.pas',
  CredentialUtils in 'CredentialUtils.pas',
  uLogging in '..\Service Code\uLogging.pas',
  SessionPipe in '..\SessionPipe.pas',
  CredentialsForm in 'CredentialsForm.pas' {FormCredentials},
  MainForm in 'MainForm.pas' {FormMain},
  XPElevationCommon in '..\XPElevationCommon.pas',
  XPRemoteRegistry in '..\XPRemoteRegistry.pas';



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
         (*
procedure Test;
var
  T,T2 : TJwSecurityToken;
  B : Boolean;
  c1 : DWORD;
  C2 : TTokenElevationType;
  c3 : TJwIntegrityLabelType;
begin
  JwInitWellKnownSIDs;
  T := TJwSecurityToken.CreateTokenEffective(MAXIMUM_ALLOWED);
  B := T.IsRestricted;
  c3 := T.TokenIntegrityLevelType;
  C1 := T.RunElevation;
  c2 := T.ElevationType;

  T2 := T.LinkedToken;
  B := T2.IsRestricted;
  c3 := T2.TokenIntegrityLevelType;
  C1 := T2.RunElevation;
  c2 := T2.ElevationType;
end;

          *)
function StartProcess(const PathName : WideString) : THandle;
var ProcInfo: PROCESS_INFORMATION;
    StartInfo : TStartupInfoW;
begin
  ZeroMemory(@StartInfo, sizeof(StartInfo));
  StartInfo.cb := sizeof(StartInfo);
//  StartInfo.lpDesktop := 'winsta0\default';
  StartInfo.wShowWindow := SW_SHOW;


  if not CreateProcessW(PWideChar(PathName),nil, nil,nil,false, CREATE_NEW_CONSOLE, nil, nil, StartInfo, ProcInfo) then
    ShowMessage(Format('The application could not be started: (%d) %s',[GetLastError,SysErrorMessage(GetLastError)]));

  CloseHandle(ProcInfo.hThread);
  result := ProcInfo.hProcess;
end;


function GetSystem32Path : WideString;
var Path : Array[0..MAX_PATH] of Widechar;
begin
  Result := '';
  if SUCCEEDED(SHGetFolderPathW(0,CSIDL_SYSTEM,0,SHGFP_TYPE_DEFAULT, @Path)) then
    result := IncludeTrailingBackslash(Path);  //Oopps, may convert unicode to ansicode
end;

function IsDefaultDesktop : Boolean;
var Desk : TJwSecurityDesktop;
begin
  Desk := TJwSecurityDesktop.CreateAndGetInputDesktop([], false, DESKTOP_READOBJECTS);
  try
    result := JwCompareString(Desk.Name,'default', true) = 0;
  finally
    Desk.Free;
  end;
  MessageBox(0,'The desktop has been restored','',MB_ICONINFORMATION or MB_OK);

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

  Resolution : Trect;
  ScreenBitmap : TBitmap;
begin

  //StartProcess(GetSystem32Path+'osk.exe');

  //exit;


{$IFDEF FORMONLY}
 // ExceptionLog.SetEurekaLogState(false);


  ScreenBitmap := GetScreenBitmap(0, Resolution);
  FreeAndNil(ScreenBitmap);

  Application.Initialize;

//  Application.CreateForm(TFormCredentials, FormCredentials);
  Application.CreateForm(TFormMain, FormMain);
  FormMain.ScreenBitmap := ScreenBitmap;
  FormMain.Resolution := Resolution;


  Application.Run;
  exit;
{$ENDIF FORMONLY}

  if GetSystemMetrics(SM_SHUTTINGDOWN) <> 0 then
    halt(1);

  {if not IsDefaultDesktop then
    halt(1);
  }

  ExceptionLog.CustomWebFieldsRequest := JEDI_WebFieldsRequestNotify;
  ExceptionLog.AttachedFilesRequest := AttachedFilesRequestProc;


  if HasParameter('/cred') and HasParameter('/switchdefault') then
  begin
    SwitchToDefault;
    exit;
  end;

 // if HasParameter('/debug') then
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
      if (Res <> 0) and (not ConsentThread.NoErrorDialogs) then
      begin
        ErrorValue := ConsentThread.ErrorValue;
        LastError  := ConsentThread.LastError;
        IsServiceError := ConsentThread.IsServiceError;
      end
      else
        Res := 0;

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
