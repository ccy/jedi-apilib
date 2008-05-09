unit CredentialsThread;

interface

uses
  JwaWindows,
  JwsclUtils,
  JwsclDescriptor,
  JwsclComUtils,
  JwsclDesktops,
  JwsclCredentials,
  JwsclEurekaLogUtils,
  JwsclTypes,
  JwsclLogging,
  SessionPipe,
  SysUtils,
  Forms,
  Dialogs,
  Controls,
  Classes,
  Graphics,
  MainForm,
  CredentialsForm;

type

  TConsentThread = class(TJwThread)
  protected
    fDesktop : TJwSecurityDesktop;
    fMaxRepetitionCount : DWORD;
    function SetUpDesktop : TJwSecurityDesktop;
    procedure EndDesktop(var Desktop : TJwSecurityDesktop);
    function Logon(const PipeSession : TClientSessionPipe; SessionInfo : TSessionInfo): Integer;
    procedure ProcessLogonResult(const Value, LastError : Integer);
    function ShowCredentialsForm(
      const ScreenBitmap : TBitmap;
      const Resolution : TRect;
      var SessionInfo : TSessionInfo     ): Integer;
  protected

  public
    class function CreateNewThread(Suspended : Boolean) : TConsentThread;
    destructor Destroy; override;

    procedure Execute; override;

    property MaxRepetitionCount : DWORD read fMaxRepetitionCount;
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


function TConsentThread.ShowCredentialsForm(
  const ScreenBitmap : TBitmap;
  const Resolution : TRect;
  var SessionInfo: TSessionInfo): Integer;
var
  Prompt: TJwCredentialsPrompt;
  Desktop: TJwSecurityDesktop;



begin 
  //create credential form
  Application.CreateForm(TFormCredentials, FormCredentials);
  FormCredentials.CenterInMonitor(0);

  FormCredentials.AppName := SessionInfo.Application;
  FormCredentials.AppCmdLine := SessionInfo.Commandline;

  FormCredentials.UserName := SessionInfo.UserName;
//     FormCredentials.Password := SessionInfo.Password; //pass is not sent by server
  FormCredentials.Flags := SessionInfo.Flags;
  FormCredentials.TimeOut := SessionInfo.TimeOut;

  Application.CreateForm(TFormMain, FormMain);
  FormMain.Image1.Picture.Bitmap.Assign(ScreenBitmap);
  FormMain.Image1.Align  := alClient;
  FormMain.BoundsRect := Resolution;
  FormMain.Visible := true;
    
    Application.Run;

    result := FormCredentials.ModalResult;
    if result = mrOK then
    begin
      SessionInfo.UserName := FormCredentials.UserName;
      SessionInfo.Password := FormCredentials.Password;
      SessionInfo.Flags    := FormCredentials.Flags;
    end
    else
    if result = mrAbort then
    begin
      //service terminates only if also canceled
      SessionInfo.Flags    := CLIENT_CANCELED or CLIENT_DEBUGTERMINATE;
    end
    else
      SessionInfo.Flags    := CLIENT_CANCELED;
end;

procedure TConsentThread.ProcessLogonResult(const Value, LastError : Integer);
var
  ErrorResults : array[1..2] of String;
  Win32Error,
  ErrorMessage : String;
  i : Integer;
begin
  ErrorResults[1] := 'Invalid data';
  ErrorResults[2] := 'Process creation failed';
  if (Value = ERROR_WIN32) and (LastError <> ERROR_SUCCESS) then
  begin
    SetLastError(LastError);
    MessageDlg(ErrorResults[i]+#13#10+SysErrorMessage(LastError),mtError,[mbok],0);
  end
  else
  if (Value <> ERROR_SUCCESS) and (Value <> ERROR_ABORTBYUSER) then
  begin
    case Value of
      ERROR_CREATEPROCESSASUSER_FAILED : ErrorMessage := ''+#13#10+SysErrorMessage(LastError);
      ERROR_INVALID_USER : ErrorMessage := 'ERROR_INVALID_USER';
      ERROR_ABORTBYUSER : ErrorMessage := 'ERROR_ABORTBYUSER';
      ERROR_LOGONUSERFAILED : ErrorMessage := 'ERROR_LOGONUSERFAILED';
      ERROR_LOADUSERPROFILE : ErrorMessage := 'ERROR_LOADUSERPROFILE';

      ERROR_TIMEOUT : ErrorMessage := 'ERROR_TIMEOUT';
      ERROR_SHUTDOWN : ErrorMessage := 'ERROR_SHUTDOWN';
      ERROR_WIN32 : ErrorMessage := 'ERROR_WIN32';
      ERROR_GENERAL_EXCEPTION : ErrorMessage := 'A general exception was raised by the service. ';
      ERROR_NO_SUCH_LOGONSESSION : ErrorMessage := 'ERROR_NO_SUCH_LOGONSESSION';
      ERROR_TOO_MANY_LOGON_ATTEMPTS : ErrorMessage := 'The logon was aborted because too many attempt were used.';
    else
      ErrorMessage := 'Unknown: '+IntToStr(Value);
    end;
    if LastError <> 0 then
      Win32Error := SysErrorMessage(LastError);
    MessageDlg(ErrorMessage+#13#10+Win32Error,mtError,[mbok],0);
  end;
end;

function TConsentThread.Logon(
  const PipeSession : TClientSessionPipe;
  SessionInfo : TSessionInfo): Integer;

var
  LastError,
  Value : Cardinal;

  CurrentAttempt : Integer;
  SecureDesktop : TJwSecurityDesktop;

  PromptResult : Integer;

  ScreenBitmap : TBitmap;
  ActiveWindowHandle : HWND;
  Resolution : TRect;
begin
  result := 0;

  ActiveWindowHandle := GetForegroundWindow; //GetActiveWindow;

  //create background image
  ScreenBitmap := GetScreenBitmap(Resolution);
  TJwAutoPointer.Wrap(ScreenBitmap);


  try
    //create new and switch desktop
    fDesktop := SetUpDesktop;
  except
    on E : Exception do
    begin
      MessageDlg('The secure desktop could not be established. '#13#10+SysErrorMessage(GetLastError),mtError,[mbok],0);
      Result := ERROR_BAD_ENVIRONMENT;
      exit;
    end;
  end;
                           
  //setup VCL on new thread
  Application := TApplication.Create(nil);
  Application.Initialize;

  try
    SessionInfo.Password := GetFillPasswd;
    SessionInfo.Password := '';

    CurrentAttempt := 0;
    repeat
      Result := ERROR_INVALID_PASSWORD;

      PromptResult := ShowCredentialsForm(ScreenBitmap,Resolution,SessionInfo);

      Result := ERROR_PIPE_NOT_CONNECTED;
      PipeSession.SendClientData(SessionInfo);

      Result := ERROR_PIPE_NOT_CONNECTED;
      PipeSession.ReadServerProcessResult(Value, LastError, 0);

      case Value of
        ERROR_SUCCESS : break;
        ERROR_LOGONUSERFAILED : CurrentAttempt := LastError;
      else
        begin
          EndDesktop(SecureDesktop);
          ProcessLogonResult(Value, LastError);
          break;
        end;
      end;

      if PromptResult <> mrOK then
        break;
    until CurrentAttempt > MaxRepetitionCount;

    //we could check for a result, but we just abort here
    if PromptResult <> mrOK then
    begin
      PipeSession.ReadServerProcessResult(Value, LastError, 0);
      ProcessLogonResult(Value, LastError);
    end;

  finally
    EndDesktop(fDesktop);

  end;
  Result := 0;
end;

function TConsentThread.SetUpDesktop : TJwSecurityDesktop;
var SD: TJwSecurityDescriptor;
begin
  SD := TJwSecurityDescriptor.CreateDefaultByToken;
  try
    result := TJwSecurityDesktop.CreateDesktop(nil, true, 'SecureElevation', [],
       false, GENERIC_ALL,  SD);
{$IFDEF SECURE_DESKTOP}
    result.SwitchDesktop;
    try
      result.SetThreadDesktop;
    except
      result.SwitchDesktopBack;
    end;
{$ENDIF}
  finally
    SD.Free;
  end;
end;



destructor TConsentThread.Destroy;
begin
  try
     FreeAndNil(fDesktop);
  except
  end;
  inherited;
end;

procedure TConsentThread.EndDesktop(var Desktop : TJwSecurityDesktop);
var D : TJwSecurityDesktop;

begin
  if Assigned(Desktop) then
  begin
    try
{$IFDEF SECURE_DESKTOP}
      try
        Desktop.SwitchDesktopBack;
      except
      end;
{$ENDIF SECURE_DESKTOP}
    finally

    end;
  end;
end;

procedure TConsentThread.Execute;
var
  Log : IJwLogClient;
  PipeSession : TClientSessionPipe;
  SessionInfo : TSessionInfo;
  Idx : Integer;
begin
  inherited;



  //check whether we can find the pipe name in parameter list
  Idx := GetParameterIndex('/pipe');
  if (Idx <= 0) or (Idx+1 > ParamCount) then
  begin
    ReturnValue := ERROR_INVALID_HANDLE;
    exit;
  end;

  //free old application
  try
    Application.Free;
  except
  end;


  try
    //create auto pointer Pipe
    PipeSession := TClientSessionPipe.Create;
    TJwAutoPointer.Wrap(PipeSession);
    
    //connect to pipe name if possible
    ReturnValue := ERROR_PIPE_NOT_CONNECTED;
    PipeSession.Connect(ParamStr(Idx+1));


    //read service information
    ReturnValue := ERROR_INVALID_DATA;
    PipeSession.ReadServerData(SessionInfo);

    //save max possible logon attempts
    fMaxRepetitionCount := SessionInfo.MaxLogonAttempts;

    //switch desktop and show logon prompt
    ReturnValue := Logon(PipeSession, SessionInfo);

  finally
    try
      //FreeAndNil(Application);
      Application.Free;
    except
      Application := nil;
    end;

  end;
  SetEvent(FTerminatedEvent);
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
