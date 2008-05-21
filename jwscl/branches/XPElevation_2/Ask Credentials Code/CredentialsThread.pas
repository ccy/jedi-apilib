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
  JwsclSid,
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
  CredentialsForm;



type
  PEaseAccessProcess = ^TEaseAccessProcess;
  TEaseAccessProcess = record
    PID : DWORD;
    Path : WideString;
    UseCreateProcess : Boolean;
  end;

  TConsentThread = class(TJwThread)
  protected
    fDesktop : TJwSecurityDesktop;
    fMaxRepetitionCount : DWORD;
    fLastError,
    fErrorValue : DWORD;
    fIsServiceError : Boolean;

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
    EaseAccessProcesses: TList;
    fUseSecureDesktop : Boolean;

    procedure RememberAndCloseEaseAccessApps;
    procedure RestartEaseAccessAppsOnNewDesktop(const RestoreDesktop : TJwSecurityDesktop);
    procedure CloseEaseAccessApps;
    procedure FreeEaseAccessAppsList;
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

function TConsentThread.SetUpDesktop : TJwSecurityDesktop;
var
  SD: TJwSecurityDescriptor;
  LastError : DWORD;
  Log : IJwLogClient;
begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,'SetupDesktop','CredentialsThreads.pas','');

  try
    Application.Free;
  except
  end;
  Application := nil;

  SD := TJwSecurityDescriptor.CreateDefaultByToken();
  TJwAutoPointer.Wrap(SD);
  try
    //SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_ALL, JwLocalSystemSID));

{    if SD.DACL.FindSID(JwLocalSystemSID) >= 0 then
    begin
      SD.DACL.Delete(SD.DACL.FindSID(JwLocalSystemSID));
    end;}
   try
     result := TJwSecurityDesktop.CreateDesktop(nil, true, 'SecureElevation', [],
       false, GENERIC_ALL,  SD);

{      result := TJwSecurityDesktop.OpenDesktop(nil, false, 'winlogon', [],
       false, GENERIC_ALL);
 }

      result.SetThreadDesktop;
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
  FreeEaseAccessAppsList;

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
begin
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
begin
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


  Application.CreateForm(TFormMain, FormMain);
  //The switch desktop process is done after the background image form has been
  //established. This avoids showing the standard desktop background
  FormMain.Desktop := Desktop;
  FormMain.Resolution := Resolution;
  FormMain.ScreenBitmap := ScreenBitmap;
  FormMain.ControlFlags := SessionInfo.ControlFlags;

  //set visibility at the end
  FormMain.Visible := true;





  Log.Log('Run Application...');
  Application.Run;

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

  try
    Application.Free;
  except
  end;
  Application := nil;
end;

class procedure TConsentThread.ProcessLogonResult(const Value, LastError : Integer);
var
  Win32Error,
  ErrorMessage : String;
  i : Integer;

  Log : IJwLogClient;
begin
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

procedure TConsentThread.RememberAndCloseEaseAccessApps;

function FindProcess(const Name, ExpectedPath : WideString; out ProcessEntry32 : TProcessEntry32W) : boolean;
var
  hSnap : THandle;
  ProcEntry : TProcessEntry32W;
  len,
  currentID : DWORD;
  Continue : Boolean;
  Path : WideString;
  hProc : THandle;
begin
  hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  result := false;

  ZeroMemory(@ProcessEntry32, sizeof(ProcessEntry32));

  try
    ProcEntry.dwSize := sizeof(ProcEntry);
    Continue := Process32FirstW(hSnap, ProcEntry);

    while (continue) do
    begin
      if (JwCompareString(WideString(ProcEntry.szExeFile), Name, true) = 0) then
      begin
        hProc := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
          false, ProcEntry.th32ProcessID);

        if hProc > 0 then
        begin
          SetLength(Path,MAX_PATH);

          //path to exe must be correct
          len :=  GetModuleFileNameExW(hProc, 0, @Path[1], MAX_PATH);
          if Len > 0 then
          begin
            SetLength(Path, Len);

            if JwCompareString(Path, ExpectedPath+Name, true) = 0 then
            begin
              result := true;
              ProcessEntry32 := ProcEntry;
              break;
            end;
          end;
          CloseHandle(hProc);
        end;
      end;

      continue := Process32NextW(hSnap,ProcEntry);
    end;
  finally
    CloseHandle(hSnap);
  end;
end;

procedure AddAndCloseApp(const EaseAccessProcesses : TList;
  ProcessEntry32 : TProcessEntry32W; UseCreateProcess : Boolean);
var
  hProc : THandle;
  hID : PEaseAccessProcess;
  len : DWORD;
begin
  hProc := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ or PROCESS_TERMINATE, false, ProcessEntry32.th32ProcessID);
  if hProc <> 0 then
  begin
    New(hID);
    hID.PID := 0;

    SetLength(hID.Path,MAX_PATH);
    len :=  GetModuleFileNameExW(hProc, 0, @hID.Path[1], MAX_PATH);
    if Len > 0 then
    begin
      SetLength(hID.Path, len);
      hID.UseCreateProcess := UseCreateProcess;
      EaseAccessProcesses.Add(Pointer(hID));

      TerminateProcess(hProc, 0);
    end
    else
    begin
      Dispose(hId);
    end;

    CloseHandle(hProc);
  end;
end;

function GetPath(ID : Integer) : WideString;
var Path : Array[0..MAX_PATH] of Widechar;
begin
  Result := '';
  if SUCCEEDED(SHGetFolderPathW(0,ID,0,SHGFP_TYPE_DEFAULT, @Path)) then
    result := IncludeTrailingBackslash(Path);  //Oopps, may convert unicode to ansicode
end;

var
  ProcessEntry32 : TProcessEntry32W;
begin
  EaseAccessProcesses := TList.Create;

{  if FindProcess('osk.exe',GetPath(CSIDL_SYSTEM), ProcessEntry32) then
  begin
    AddAndCloseApp(EaseAccessProcesses, ProcessEntry32, false);
  end;   }
  if FindProcess('magnify.exe',GetPath(CSIDL_SYSTEM), ProcessEntry32) then
  begin
    AddAndCloseApp(EaseAccessProcesses, ProcessEntry32, true);
  end;
end;

type
  TConsentThreadEaseAppThread = class(TJwThread)
  public
    EaseAccessProcesses : TList;
    RestoreDesktop      : TJwSecurityDesktop;

    procedure Execute; override;
  end;

procedure TConsentThreadEaseAppThread.Execute;
var
  i : Integer;
  Log : IJwLogClient;
begin
  Log := ULogging.LogServer.Connect(etMethod,ClassName,'Logon','ConsentThreadEaseAppThread','');

  Sleep(50);

  try
    if Assigned(EaseAccessProcesses) then
    with EaseAccessProcesses do
    begin
      if Assigned(RestoreDesktop) then
        RestoreDesktop.SetLastThreadDesktop;

      for i := 0 to EaseAccessProcesses.Count - 1 do
      begin
        PEaseAccessProcess(EaseAccessProcesses[i]).PID :=
          TFormMain.StartShellExecute(PEaseAccessProcess(EaseAccessProcesses[i]).Path,'',
            PEaseAccessProcess(EaseAccessProcesses[i]).UseCreateProcess);
      end;
    end;
  except
    on E : Exception do
      Log.Exception(E);


  end;
end;

procedure TConsentThread.RestartEaseAccessAppsOnNewDesktop(const RestoreDesktop : TJwSecurityDesktop);
var ConsentThreadEaseAppThread : TConsentThreadEaseAppThread;
begin
  ConsentThreadEaseAppThread := TConsentThreadEaseAppThread.Create(true, 'ConsentThreadEaseAppThread');
  try
    ConsentThreadEaseAppThread.EaseAccessProcesses := EaseAccessProcesses;
    ConsentThreadEaseAppThread.RestoreDesktop := RestoreDesktop;

    if Assigned(RestoreDesktop) then
    begin
      ConsentThreadEaseAppThread.FreeOnTerminate := False;
      ConsentThreadEaseAppThread.Resume;
      ConsentThreadEaseAppThread.WaitFor;
    end
    else
      ConsentThreadEaseAppThread.Execute;
  finally
    ConsentThreadEaseAppThread.Free;
  end;
end;

procedure TConsentThread.CloseEaseAccessApps;
var
  i : Integer;
  PID : DWORD;

  hProc : THandle;
  hID : PEaseAccessProcess;
  len : DWORD;

begin
  //first we try to terminate the known apps
  for i := 0 to EaseAccessProcesses.Count - 1 do
  begin
    PID := PEaseAccessProcess(EaseAccessProcesses[i]).PID;

    hProc := OpenProcess(PROCESS_TERMINATE, false, PID);
    if hProc <> 0 then
    begin
      TerminateProcess(hProc,0);
      CloseHandle(hProc);
    end;
  end;
end;

procedure TConsentThread.FreeEaseAccessAppsList;
var i : Integer;
begin
  for i := 0 to EaseAccessProcesses.Count - 1 do
  begin
    Dispose(EaseAccessProcesses[i]);
  end;
  FreeAndNil(EaseAccessProcesses);
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
begin
  Log := ULogging.LogServer.Connect(etMethod,ClassName,'Logon','CredentialsThread','');

  result := 0;
  fLastError := 0;
  fErrorValue := 0;

  ScreenBitmap := nil;

  //create background image if this is not a remote session
  //we save data
  if fUseSecureDesktop and
     ((GetSystemMetrics(SM_REMOTESESSION) = 0) or
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

  if fUseSecureDesktop then
  begin
    RememberAndCloseEaseAccessApps;
    try
      //create new and switch desktop
      fDesktop := SetUpDesktop;
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

    RestartEaseAccessAppsOnNewDesktop(nil);
  end
  else
    fDesktop := nil;



  try
    try
      SessionInfo.Password := GetFillPasswd;
      SessionInfo.Password := '';

      CurrentAttempt := 0;

      if fUseSecureDesktop then
        SetEvent(fSwitchedSecureDesktopEvent);

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
      if fUseSecureDesktop then
      begin
        CloseEaseAccessApps;
        EndDesktop(fDesktop);

        //then restart them
        RestartEaseAccessAppsOnNewDesktop(fDesktop);

        SetEvent(fSwitchedSecureDesktopEvent);
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


procedure TConsentThread.Execute;
var
  Log : IJwLogClient;
  PipeSession : TClientSessionPipe;
  SessionInfo : TSessionInfo;
  Idx : Integer;


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
      fSwitchedSecureDesktopEvent := CreateEventW(LPSECURITY_ATTRIBUTES(pSA), false, false, 'XPElevation Switched Desktop');
    finally
      TJwSecurityDescriptor.Free_SA(pSA);
    end;
  finally
    SD.Free;
  end;
end;

begin
  Log := ULogging.LogServer.Connect(etMethod,ClassName,'Execute','CredentialsThread','');

  inherited;

  CreateSwitchEvent;

  fUseSecureDesktop := true;

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
