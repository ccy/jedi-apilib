unit CredentialsThread;

interface

uses
  JwaWindows,
  JwsclUtils,
  JwsclDescriptor,
  JwsclComUtils,
  JwsclDesktops,
  JwsclCredentials,
  JwsclExceptions,
  JwsclEurekaLogUtils,
  JwsclTypes,
  JwsclAcl,
  JwsclLogging,
  JwsclKnownSid,
  JwsclStrings,
  JwsclSecureObjects,
  JwsclSid,
  TempWindow,
  ULogging,
  SessionPipe,
  SysUtils,
  Forms,
  Dialogs,
  Controls,
  Classes,
  Graphics,
  MainForm,
  XPElevationCommon,
  EaseAccessMan,
  CredentialsForm;



type


  TConsentThread = class(TJwThread)
  protected
    fDefaultDesktop,
    fDesktop : TJwSecurityDesktop;
    fMaxRepetitionCount : DWORD;
    fLastError,
    fErrorValue : DWORD;
    fIsServiceError : Boolean;

    fSecureDesktop,
    fNoErrorDialogs,
    fShowWebPage : Boolean;

    function SetUpDesktop : TJwSecurityDesktop;
    procedure EndDesktop(var Desktop : TJwSecurityDesktop);
    function Logon(const PipeSession : TClientSessionPipe; SessionInfo : TSessionInfo): Integer;

    function ShowCredentialsForm(
      const ScreenBitmap : TBitmap;
      const Resolution : TRect;
      const Desktop : TJwSecurityDesktop;
      var SessionInfo : TSessionInfo;
      const PipeSession : TClientSessionPipe;
      const ShowLogonError : Boolean): Integer;
  protected
    fSwitchedSecureDesktopEvent : HEVENT;
    fEaseAppList : TEaseAppList;
  public
    class function CreateNewThread(Suspended : Boolean) : TConsentThread;

    destructor Destroy; override;

    procedure Execute; override;



    class procedure ProcessLogonResult(const Value, LastError : Integer);
    property MaxRepetitionCount : DWORD read fMaxRepetitionCount;

    property LastError : DWORD read fLastError;
    property ErrorValue : DWORD read fErrorValue;
    property IsServiceError : Boolean read fIsServiceError;

    property ShowWebPage : Boolean read fShowWebPage;
    property NoErrorDialogs : Boolean read fNoErrorDialogs;

    property SecureDesktop : Boolean read fSecureDesktop;
  end;

function HasParameter(const Name : String; out Index : Integer) : Boolean;overload;
function HasParameter(const Name : String) : Boolean;overload;
function GetParameterIndex(const Name : String) : Integer;




implementation
uses CredentialUtils;

function GetParameterIndex(const Name : String) : Integer;
begin
  HasParameter(Name, Result);
end;

function HasParameter(const Name : String) : Boolean;overload;
var i : Integer;
begin
  result := HasParameter(Name, i);
end;

function HasParameter(const Name : String; out Index : Integer) : Boolean;
var i : Integer;
begin
  result := false;
  for i := 1 to ParamCount do
  begin
    result := CompareText(Name,ParamStr(i)) = 0;
    Index := i;
    if result then
      exit;
  end;
  Index := -1;
end;

function TConsentThread.SetUpDesktop() : TJwSecurityDesktop;
var
  SD: TJwSecurityDescriptor;
  LastError : DWORD;
  Log : IJwLogClient;
  L : TJwSecureGeneralObject;
var LogServer : IJwLogServer;
begin
  if not Assigned(ULogging.LogServer) then
    LogServer := CreateLogServer(nil, LogEventTypes, nil);

  Log := uLogging.LogServer.Connect(etMethod,ClassName,'SetupDesktop','CredentialsThreads.pas','');

  result := nil;
  if not SecureDesktop then
    exit;


  JwInitWellKnownSIDs;

  try
    Application.Free;
  except
  end;
  Application := nil;

  //SD := TJwSecurityDescriptor.CreateDefaultByToken();
  SD := TJwSecurityDescriptor.Create;
  SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_ALL, JwLocalSystemSID));
  {
  Do not deny SWITCH_DESKTOP to System because if the winlogon desktop is shown by
  a call to LockWorkStation the desktop is locked and cannot be unlocked again. 
  }
{$IFDEF DEBUG}
  SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_ALL, JwAdministratorsSID));
{$ENDIF DEBUG}

  TJwAutoPointer.Wrap(SD);
  try
   try
     SecureDesktopName := 'SecureElevation_'+IntToStr(GetCurrentProcessId);
     result := TJwSecurityDesktop.CreateDesktop(nil, true, SecureDesktopName, [],
       false, GENERIC_ALL,  SD);

    // TJwSecureGeneralObject.SetSecurityInfo(result.Handle,SE_KERNEL_OBJECT,[siDaclSecurityInformation], SD);
{      result := TJwSecurityDesktop.OpenDesktop(nil, false, 'winlogon', [],
       false, GENERIC_ALL);
 }

      //SD := TJwSecureGeneralObject.GetSecurityInfo(result.Handle,SE_KERNEL_OBJECT,[siOwnerSecurityInformation, siGroupSecurityInformation, siDaclSecurityInformation]);
      //MessageBoxW(0,PWideChar(SD.Text),'', MB_OK);

{$IFNDEF FORMONLY}
      result.SetThreadDesktop;
{$ENDIF FORMONLY}
    except
      on E : EJwsclWinCallFailedException do
      begin
        Log.Exception(E);
        Log.Log(lsError,'Wincall failed with '+IntToStr(E.LastError));
        raise;
      end;
    end;

  finally
  end;
end;





destructor TConsentThread.Destroy;
begin
  //FreeEaseAccessAppsList;

  CloseHandle(fSwitchedSecureDesktopEvent);
  try
     FreeAndNil(fDesktop);
  except
  end;
  inherited;
end;

procedure TConsentThread.EndDesktop(var Desktop : TJwSecurityDesktop);


var
  InputDesk : TJwSecurityDesktop;
  Desks : TJwSecurityDesktops;
  Log : IJwLogClient;
  LogServer : IJwLogServer;
begin
  if not Assigned(ULogging.LogServer) then
    LogServer := CreateLogServer(nil, LogEventTypes, nil);

  Log := uLogging.LogServer.Connect(etMethod,ClassName,'EndDesktop','CredentialsThreads.pas','');

  if Assigned(Desktop) then
  begin
    try
      Application.Free;
    except
    end;
    Application := nil;
    try
      {Forms.Application or any other VCL component
       seems to leave a hook behind, so this will going to fail.
       Since we are in a thread, we have to show message boxes
       in the main thread.
      }
      Desktop.SetLastThreadDesktop;
    except

    end;

    //don't change from winlogon - locked workstation 
    if not TJwSecurityDesktops.IsStationLocked then    
    try
      Desktop.SwitchDesktopBack;
      Log.Log('SwitchDesktopBack succeeded.');
    except
      try
        //failsafe
        InputDesk := TJwSecurityDesktop.Create(nil,dcfOpen, true, 'default',[],false, DESKTOP_READOBJECTS or DESKTOP_SWITCHDESKTOP ,nil);
        TJwAutoPointer.Wrap(InputDesk);
        InputDesk.SwitchDesktop;
      except
      end;
    end;
  end;
end;


function TConsentThread.ShowCredentialsForm(
  const ScreenBitmap : TBitmap;
  const Resolution : TRect;
  const Desktop : TJwSecurityDesktop;
  var SessionInfo: TSessionInfo;
  const PipeSession : TClientSessionPipe;
  const ShowLogonError : Boolean): Integer;
var
  Prompt: TJwCredentialsPrompt;
  Log : IJwLogClient;
var LogServer : IJwLogServer;
begin
  if not Assigned(ULogging.LogServer) then
    LogServer := CreateLogServer(nil, LogEventTypes, nil);

  Log := uLogging.LogServer.Connect(etMethod,ClassName,'ShowCredentialsForm','CredentialsThreads.pas','');

  try
    Application.Free;
  except
  end;
  Application := TApplication.Create(nil);
  Application.Initialize;
  //create credential form

  Application.CreateForm(TFormCredentials, FormCredentials);
  FormCredentials.CenterInMonitor(0);

  FormCredentials.AppName := SessionInfo.Application;
  FormCredentials.AppCmdLine := SessionInfo.Commandline;

  FormCredentials.UserName := SessionInfo.UserName;
//     FormCredentials.Password := SessionInfo.Password; //pass is not sent by server
  FormCredentials.Flags := SessionInfo.Flags;
  FormCredentials.TimeOut := SessionInfo.TimeOut;
  FormCredentials.PipeSession := PipeSession;
  FormCredentials.LogonError := ShowLogonError;
  FormCredentials.ControlFlags := SessionInfo.ControlFlags;
  FormCredentials.UserRegKey := SessionInfo.UserRegKey;
  FormCredentials.UserImage := SessionInfo.UserProfileImage;

  FormCredentials.UserImageType := SessionInfo.UserProfileImageType;
  FormCredentials.SecureDesktop := SecureDesktop;
  FormCredentials.Desktop := Desktop;


  {if Assigned(SessionInfo.UserProfileImage) then
    SessionInfo.UserProfileImage.SaveToFile('E:\Temp\_test2.jpg');}


  Application.CreateForm(TFormMain, FormMain);
  //The switch desktop process is done after the background image form has been
  //established. This avoids showing the standard desktop background
  FormMain.Desktop := Desktop;
  FormMain.Resolution := Resolution;
  FormMain.ScreenBitmap := ScreenBitmap;
  FormMain.ControlFlags := SessionInfo.ControlFlags;
  FormMain.EaseAppList := fEaseAppList;

  //set visibility at the end
  FormMain.Visible := true;



  Log.Log('Run Application...');
  Application.Run;

  SessionInfo.Flags    := CLIENT_CANCELED;
  try
    result := FormCredentials.ModalResult;

    Log.Log('ModalResult returned : '+IntToStr(Result));
    Log.Log('SessionInfo.Flags : '+IntToStr(SessionInfo.Flags));

    fShowWebPage := FormCredentials.ShowWebPage;
    if result = mrOK then
    begin
      SessionInfo.UserName := FormCredentials.UserName;
      SessionInfo.Password := FormCredentials.Password;
      SessionInfo.Flags    := FormCredentials.Flags;
    end
    else
    //abort only works on DEBUG service. It also shuts down the service properly
    //otherwise the service does not shutdown
    if result = mrAbort then
    begin
      //service terminates only if also canceled
      SessionInfo.Flags    := CLIENT_CANCELED or CLIENT_DEBUGTERMINATE;
    end
    else
      SessionInfo.Flags    := CLIENT_CANCELED;
  finally
    try
      Application.Free;
    except
    end;
    Application := nil;
  end;
end;

class procedure TConsentThread.ProcessLogonResult(const Value, LastError : Integer);
var
  Win32Error,
  ErrorMessage : String;
  i : Integer;

  Log : IJwLogClient;
  LogServer : IJwLogServer;
begin
  if not Assigned(ULogging.LogServer) then
    LogServer := CreateLogServer(nil, LogEventTypes, nil);

  Log := uLogging.LogServer.Connect(etMethod,ClassName,'ProcessLogonResult','CredentialsThreads.pas','');

  if (Value = ERROR_WIN32) and (LastError <> ERROR_SUCCESS) then
  begin
    Win32Error := Format('The service encountered a Win32 error. %sError: %d (%s)'+
          'You can try it again next time. If this error persists you should contact the author.',
       [#13#10,LastError,SysErrorMessage(LastError)]);
    Log.Log(lsError,Win32Error);
    MessageBoxA(0,PAnsiChar(Win32Error),'Win32Error report from XP Elevation Service', MB_ICONERROR or MB_OK);
  end
  else
  if (Value <> ERROR_SUCCESS) and (Value <> ERROR_ABORTBYUSER) then
  begin
    case Value of
      ERROR_CREATEPROCESSASUSER_FAILED : ErrorMessage := Format('The service failed on creating the process.'+
          'The returned error code %d (%s) was supplied. You can try it again next time. If this error persists you should contact the author.',[LastError,SysErrorMessage(LastError)]);
      ERROR_INVALID_USER : ErrorMessage := 'The given user is invalid or unknown. Elevate again with a correct username. Cannot continue.';
      ERROR_ABORTBYUSER : ErrorMessage := 'The elevation process was aborted by user.';
      ERROR_LOGONUSERFAILED : ErrorMessage := 'The user could not be logged on. Maybe it is restricted by an Administrator. Allow the user to logon interactively or contact your Administrator';
      ERROR_LOADUSERPROFILE : ErrorMessage := 'The service could not load the user''s profile. This is a Windows problem. We fail here.';

      ERROR_TIMEOUT : ErrorMessage := 'The elevation process timed out.'#13#10'The application aborts. You can try another elevation next time.';
      ERROR_SHUTDOWN : ErrorMessage := 'The service shuts down. The application aborts. If you need to elevate again, you have to start the service again.';
      ERROR_WIN32 : ErrorMessage := 'The service encountered a problem that stops the elevation. You can try another elevation next time.';
      ERROR_GENERAL_EXCEPTION : ErrorMessage := 'A general exception was raised by the service. This is maybe due to a developer error.'+
          'You can try another elevation next time. If it happens again you should consider contact the author.';
      ERROR_NO_SUCH_LOGONSESSION : ErrorMessage := 'The service could not elevate the application because the given logon session does not exists.';
      ERROR_TOO_MANY_LOGON_ATTEMPTS : ErrorMessage := 'The logon was aborted because too many attempt were used. ';
    else
      ErrorMessage := 'The error value from the process is unknown. You should report this error value to the author: '+IntToStr(Value);
    end;
    if LastError <> 0 then
      Win32Error := SysErrorMessage(LastError);

    Log.Log(lsError,ErrorMessage);
    Log.Log(lsError,Win32Error);
    MessageBoxA(0, PAnsiChar(ErrorMessage+#13#10+Win32Error),'Report from XP Elevation Service', MB_ICONERROR or MB_OK);
  end;
end;



function TConsentThread.Logon(
  const PipeSession : TClientSessionPipe;
  SessionInfo : TSessionInfo): Integer;


var
  LastError,
  Value : Cardinal;

  CurrentAttempt : Integer;


  PromptResult : Integer;

  ScreenBitmap : TBitmap;
  Resolution : TRect;

  Log : IJwLogClient;
  LogServer : IJwLogServer;
begin
  if not Assigned(ULogging.LogServer) then
    LogServer := CreateLogServer(nil, LogEventTypes, nil);

  Log := ULogging.LogServer.Connect(etMethod,ClassName,'Logon','CredentialsThread','');

  result := 0;
  fLastError := 0;
  fErrorValue := 0;

  ScreenBitmap := nil;

  //create background image if this is not a remote session
  //we save data
  if ((GetSystemMetrics(SM_REMOTESESSION) = 0) or
     (SessionInfo.ControlFlags and 1{XPCTRL_FORCE_BACKGROUND_IMAGE} = 1)) then
  begin
    try
      ScreenBitmap := GetScreenBitmap(SessionInfo.ParentWindow, Resolution);
      //will be destroyed by BackgroundForm
    except
      ScreenBitmap := nil;
      Log.Log(lsWarning,'Screenbitmap failed to load.');
    end;
  end
  else
  begin
    Resolution := GetMaxResolution;
    Log.Log(lsWarning,'Screenbitmap is not loadedl.');
    ScreenBitmap := nil;
  end;

  fDefaultDesktop := TJwSecurityDesktop.CreateAndGetInputDesktop([], false, GENERIC_ALL);
  TJwAutoPointer.Wrap(fDefaultDesktop);

  if SecureDesktop then
  begin
    fEaseAppList.AddManagedApp('osk.exe',ShGetPath(CSIDL_SYSTEM), true, false);
    fEaseAppList.AddManagedApp('magnify.exe',ShGetPath(CSIDL_SYSTEM), true, true);
    //fEaseAppList.CloseAll(false);

    try
      //create new and switch desktop
      fDesktop := SetUpDesktop();
    except
      on E : Exception do
      begin
        Log.Log(lsError,'The secure desktop could not be established. '#13#10+SysErrorMessage(GetLastError));
        Log.Exception(E);
        FreeAndNil(ScreenBitmap);

        Result := 1;
        fIsServiceError := false;
        fErrorValue := 0;
        fLastError := ERROR_BAD_ENVIRONMENT;
        exit;
      end;
    end;

    //RestartEaseAccessAppsOnNewDesktop(nil);
  end
  else
    fDesktop := nil;

  try
    try
      SessionInfo.Password := GetFillPasswd;
      SessionInfo.Password := '';

      CurrentAttempt := 0;

      if SecureDesktop then
      begin
        SetEvent(fSwitchedSecureDesktopEvent);
        Sleep(50);
        ResetEvent(fSwitchedSecureDesktopEvent);
      end;

      repeat
        LastError := ERROR_INVALID_PASSWORD;

        Log.Log(lsMessage,'Call ShowCredentialsForm');
        PromptResult := ShowCredentialsForm(ScreenBitmap,Resolution, fDesktop, SessionInfo, PipeSession, CurrentAttempt > 0);

        //time out while we were on the winlogon desktop?
        //ignore pipe if service timed out and we didn't respons immediately
        if PromptResult = mrIgnore then
        begin
          Result := 0;
          fIsServiceError := false;
          fErrorValue := 0;
          fLastError := 0;
          //return values from service
          //since we timed out on the logon desktop
          //the service won't response anymore
          Value := 0;
          LastError := 0;
          break;
        end
        else
        begin
          if PromptResult <> mrIgnore then //ignore timeout
          begin
            Log.Log(lsMessage,'Send client data...');
            LastError := ERROR_PIPE_BUSY;
            PipeSession.SendClientData(SessionInfo);
          end;

          Log.Log(lsMessage,'Get service results');
          LastError := ERROR_PIPE_LISTENING;
          PipeSession.ReadServerProcessResult(Value, LastError, 0);

          case Value of
            ERROR_SUCCESS : break;
            ERROR_LOGONUSERFAILED :
            begin
              Log.Log(lsWarning,'Service says: invalid logon credentials');
              CurrentAttempt := LastError;
            end
          else
            begin
              Log.Log(lsWarning,Format('Service returns error. Value:%d LastError:%d',[Value,LastError]));

              Result := 1;
              fIsServiceError := true;
              fErrorValue := Value;
              fLastError := LastError;
              exit;
            end;
          end;
        end;

        if PromptResult <> mrOK then
          break;
      until CurrentAttempt > MaxRepetitionCount;

      //ignore pipe if service timed out and we didn't respons immediately
      if PromptResult <> mrIgnore then
      begin
        LastError := ERROR_PIPE_LISTENING;
        PipeSession.ReadServerProcessResult(Value, LastError, 0);
      end;

      Result := 0;

      if Value <> 0 then
      begin
        Log.Log(lsWarning,Format('2nd chance: Service returns error. Value:%d LastError:%d',[Value,LastError]));

        fIsServiceError := true;
        fErrorValue := Value;
        fLastError := LastError;
        Result := 1;

        exit;  //we don't want to set Result = 0
      end;

    finally
      if SecureDesktop then
      begin
        //CloseEaseAccessApps;
        //RememberAndCloseEaseAccessApps;

        EndDesktop(fDesktop);

        //then restart them
        //RestartEaseAccessAppsOnNewDesktop(fDesktop);
        fEaseAppList.RestartEaseAccessAppsOnNewDesktop(fDefaultDesktop);

        SetEvent(fSwitchedSecureDesktopEvent);
        Sleep(50);
        ResetEvent(fSwitchedSecureDesktopEvent);
      end;
    end;
  except
    on E : Exception do
    begin
      Log.Exception(E);
      Log.Log(lsWarning,Format('Exception occured: GetLastError: %s',[SysErrorMessage(GetLastError)]));

      fIsServiceError := false;
      fErrorValue := 0;
      fLastError := LastError;

      OutputDebugStringA(PAnsiChar(E.Message));
      Result := 1;

      exit;
    end;
  end;
  Result := 0;
end;


function ExistsCredApp(out MutexHandle : IJwAutoPointer) : Boolean;
var hMut : THandle;
begin
  result := false;

  hMut := CreateMutexW(nil, true, 'MutexCredAppRunning');

  if hMut = 0 then
    exit
  else
  if GetLastError() = ERROR_ALREADY_EXISTS then
  begin
    result := true;
  end;
end;

procedure TConsentThread.Execute;
var
  Log : IJwLogClient;
  PipeSession : TClientSessionPipe;
  SessionInfo : TSessionInfo;
  Idx : Integer;
  RunningMutexHandle : IJwAutoPointer;
  dwData : DWORD;
  TempWindow : TTempElevationWindowThread;


procedure Test1;
var h: HDESK;
  h2 : DWORD;
begin
   MessageBox(0,'Debug breakpoint','',MB_ICONEXCLAMATION or MB_OK);

  h := GetProcessWindowStation; //OpenDesktopW('default',0, true, GENERIC_ALL);
  //h := CreateEvent(nil, false, false, nil);

  if not WriteFile(
      PipeSession.Handle,//__in         HANDLE hFile,
      @h,//__out        LPVOID lpBuffer,
      sizeof(h),//__in         DWORD nNumberOfBytesToRead,
      @h2,//
      nil//@OvLapped//
    ) then
    begin
      LogAndRaiseLastOsError(Log,ClassName, 'Connect::(Winapi)WriteFile','SessionPipe.pas');
    end;

  sleep(1000);
  //halt;
  //CloseHandle(h);

end;

procedure CreateSwitchEvent;
var
  SD : TJwSecurityDescriptor;
  pSA : JwaWindows.PSECURITY_ATTRIBUTES;
  SID : TJwSecurityId;
begin
  SD := TJwSecurityDescriptor.Create;
  try
    SD.Owner := JwAdministratorsSID;
    SD.PrimaryGroup := JwAdministratorsSID;

{$IFDEF DEBUG}
    SD.DACL := nil;
{$ELSE}
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwLocalSystemSID,false));
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwAdministratorsSID,false));
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],SYNCHRONIZE,JwWorldSID,true));
{$ENDIF}
    pSA := SD.Create_SA();

    try
      //manual reset, so several threads can be wakened
      fSwitchedSecureDesktopEvent := CreateEventW(LPSECURITY_ATTRIBUTES(pSA), true, false, 'XPElevation Switched Desktop');
    finally
      TJwSecurityDescriptor.Free_SA(pSA);
    end;
  finally
    SD.Free;
  end;
end;

var LogServer : IJwLogServer;
begin
  if not Assigned(ULogging.LogServer) then
    LogServer := CreateLogServer(nil, LogEventTypes, nil);

  //das stimmt hier garnet

  Log := ULogging.LogServer.Connect(etMethod,ClassName,'Execute','CredentialsThread','');




  inherited;

  fSecureDesktop := true;

  fEaseAppList := TEaseAppList.Create;
  TJwAutoPointer.Wrap(fEaseAppList);

  CreateSwitchEvent;

  fNoErrorDialogs := false;
  fIsServiceError := false;
  fErrorValue := 0;
  fLastError := 0;

  //check whether we can find the pipe name in parameter list
  Idx := GetParameterIndex('/pipe');
  if (Idx <= 0) or (Idx+1 > ParamCount) then
  begin
    Log.Log(lsError,'The given pipe handle could not be retrieved from command line.');
    ReturnValue := 1;
    fLastError := ERROR_INVALID_HANDLE;
    exit;
  end;

  //free old application
  try
    Application.Free;
  except
  end;
  Application := nil;

  try
    try
      //create auto pointer Pipe
      PipeSession := TClientSessionPipe.Create;
      TJwAutoPointer.Wrap(PipeSession);

      //connect to pipe name if possible
      ReturnValue := 1;
      fLastError := ERROR_PIPE_NOT_CONNECTED;

      Log.Log('Connect to service');
      PipeSession.Connect(ParamStr(Idx+1));

      //Test1; //test for sending desktop or winsta handle

      //read service information
      ReturnValue := 1;
      fLastError := ERROR_INVALID_DATA;

      try
        Log.Log('Get service data');
        PipeSession.ReadServerData(SessionInfo);

        TJwAutoPointer.Wrap(SessionInfo.UserProfileImage);
      except
        on E : EOSError do
        begin
          Log.Exception(E);
          case E.ErrorCode of
            ERROR_BAD_FORMAT,
            ERROR_INVALID_SIGNATURE :
              begin
                ReturnValue := 1;
                fLastError := E.ErrorCode;
                raise;
              end
          else
            raise;
          end;
        end;
      end;

      dwData := GetWindowLongW(SessionInfo.ParentWindow,GWL_ID);
      if not ExistsCredApp(RunningMutexHandle) or //cred app
         (SessionInfo.ParentWindow = 0) or
         (dwData = 0)
         then
      begin
        TempWindow := CreateTempElevationThread('Elevation', '', 0);
        try
          TempWindow.Resume;

          ReturnValue := 1001;
          if (TempWindow.WaitWithTimeOut(120*1000,false) = WAIT_TIMEOUT) then
            exit;

          ReturnValue := 1002;
          if TempWindow.GetReturnValue <> 0 then
            exit;
        finally
          TempWindow.Free;
        end;
      end;


      fNoErrorDialogs := SessionInfo.ControlFlags and XPCTRL_FORCE_NO_ERROR_DIALOGS = XPCTRL_FORCE_NO_ERROR_DIALOGS; 

      //save max possible logon attempts
      fMaxRepetitionCount := SessionInfo.MaxLogonAttempts;
      Log.Log('MaxRepetitionCount: '+IntToStr(fMaxRepetitionCount));

      //switch desktop and show logon prompt
      ReturnValue := Logon(PipeSession, SessionInfo);
    except
      on E : Exception do
      begin
        Log.Exception(E);
        //Returnvalue defines the error code
        //
        OutputDebugStringA(PAnsiChar(E.Message));
        exit;
      end;
    end;
  finally
    try
      //may create exception
      Application.Free;
    except
      Application := nil;
    end;
    SetEvent(FTerminatedEvent);
  end;

  if ReturnValue = 0 then
  begin
    Log.Log('Nulling return values');
    ReturnValue := 0;
    fErrorValue := 0;
    fLastError := 0;
  end;


end;


class function TConsentThread.CreateNewThread(
  Suspended: Boolean): TConsentThread;
begin
  result := TConsentThread.Create(true, 'ConsentThread');
  result.FreeOnTerminate := false;

  if not Suspended then
    result.Resume;
end;


end.
