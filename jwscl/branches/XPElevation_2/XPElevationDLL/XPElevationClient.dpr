program XPElevationClient;

{.$APPTYPE CONSOLE}

uses
  dialogs,
  JwaWindows,
  JwsclComUtils,
  JwsclUtils,
  ComObj,
  XPElevationDLL_TLB,
  ActiveX,

  SysUtils,
  XPElevationCommon in '..\XPElevationCommon.pas';

function XPElevationSysMessage(const HRes : HRESULT) : WideString;
begin
  case HRes of
    S_OK                     : result := 'Success';
    E_INVALID_USER           : result := 'The given user is invalid or unknown to the system.';
    E_ABORTED                : result := 'The user has aborted the elevation.';
    E_CREATEPROCESS_FAILED   : result := 'The process creation failed.';
    E_RESPONSE_TIME_OUT      : result := 'The service timed out while waiting for response. The client needed too long.';
    E_SERVICE_SHUTDOWN       : result := 'The service could not process the request because it is shutting down.';
    E_INVALID_RECORD_SIZE    : result := 'The internal data structure size is unknown to the service.';
    E_SERVICE_OUT_OF_ORDER   : result := 'The client could not contact the service. It may be unresponsible.';
    E_SERVICE_CONTACT_FAILED : result := 'The service could be contacted at first but failed to respond later time.';
    E_INVALID_PARAMETER      : result := 'One or more supplied parameters to the service are invalid.';
    E_SERVICE_TIME_OUT       : result := 'A timeout occurred while waiting for service response. ';
  else
    result := Format('The given error code %x is unknown ',[Hres]);
  end;
end;

function ExecuteProcess(const ApplicationPath, Parameters,
  CurrentDirectory: WideString; ParentWindow, ControlFlags: LongWord;
  out PID: Integer): HResult;


type
  TXPElevationStruct = record
    Size : DWORD;

    ParentWindow : HWND;

    ApplicationName  : array[0..MAX_PATH] of WideChar;
    CurrentDirectory : array[0..MAX_PATH] of WideChar;
    Parameters : Pointer; {no use}

    ControlFlags : DWORD;
    StartupInfo : TStartupInfoW;

    ParameterCharCount : DWORD;

    Reserved1,
    Reserved2 : DWORD;
  end;

function CheckPipe(const Value : Boolean) : Boolean;
begin
  result := (Value )
      or (not Value and (GetLastError() = ERROR_IO_PENDING));
end;  
const PIPE_NAME =  '\\.\pipe\XPElevationPipe';
var
  Pipe : THandle;
  XPElevationStruct : TXPElevationStruct;
  TimeOut : DWORD;
  read : DWORD;
  Timer : THandle;
  PipeName : WideString;
  ProtocolVersion,
  PipeNameSize : DWORD;
  OvLapW,
  OvLapR : TOverlapped;


  ServiceResult : array[0..1] of DWORD;

  PW : PWideChar;
begin
  SetLastError(0);

//  MessageBox(0,'debug','',MB_OK);

  if not WaitNamedPipe(PIPE_NAME, 5{sec} *1000) then
  begin
    result := E_SERVICE_OUT_OF_ORDER;
    exit;
  end;

  Pipe := CreateFile(PIPE_NAME, GENERIC_READ or GENERIC_WRITE or SYNCHRONIZE, 0, nil, OPEN_EXISTING, {FILE_FLAG_OVERLAPPED or SECURITY_IMPERSONATION}0, 0);

  if Pipe = INVALID_HANDLE_VALUE then
  begin
    result := E_SERVICE_CONTACT_FAILED;
    Exit;
  end;
  TJwAutoPointer.Wrap(Pipe);
  ZeroMemory(@OvLapR, sizeof(OvLapR));
  ZeroMemory(@OvLapW, sizeof(OvLapW));

  ProtocolVersion := 1;
  if not WriteFile(Pipe, @ProtocolVersion, sizeof(ProtocolVersion), nil, @OvLapW) then
  begin
    result := E_SERVICE_CONTACT_FAILED;
    Exit;
  end;


  SetLastError(0);
  if not CheckPipe(ReadFile(Pipe, @PipeNameSize, sizeof(PipeNameSize), nil, @OvLapR)) then
  begin
    result := E_SERVICE_CONTACT_FAILED;
    Exit;
  end;

  SetLastError(0);
  SetLength(PipeName, PipeNameSize);
  if not CheckPipe(ReadFile(Pipe, @PipeName[1], PipeNameSize*sizeof(WideChar), nil, @OvLapR)) then
  begin
    result := E_SERVICE_CONTACT_FAILED;
    Exit;
  end;

  Sleep(2000);

  Pipe := CreateFileW(PWideChar(PipeName), GENERIC_READ or GENERIC_WRITE or SYNCHRONIZE, 0, nil, OPEN_EXISTING,
    FILE_FLAG_OVERLAPPED or SECURITY_IMPERSONATION, 0);

  if Pipe = INVALID_HANDLE_VALUE then
  begin
    result := E_SERVICE_CONTACT_FAILED;
    Exit;
  end;
  TJwAutoPointer.Wrap(Pipe);

  OvLapR.hEvent := CreateEvent(nil,false,false,nil);
  TJwAutoPointer.Wrap(OvLapR.hEvent);

  if not CheckPipe(ReadFile(Pipe, @TimeOut, sizeof(TimeOut), nil, @OvLapR)) then
  begin
    result := E_SERVICE_CONTACT_FAILED;
    Exit;
  end;
  WaitForSingleObject(OvLapR.hEvent, 5000 {INFINITE});

  ZeroMemory(@XPElevationStruct, sizeof(XPElevationStruct));

  //E_INVALID_PARAMETER

  XPElevationStruct.Size := sizeof(XPElevationStruct);
  result := StringCchCopyW(@XPElevationStruct.ApplicationName, sizeof(XPElevationStruct.ApplicationName), PWideChar(ApplicationPath));
  if result <> S_OK then
  begin
    exit;
  end;
  XPElevationStruct.ParameterCharCount := Length(Parameters);
  XPElevationStruct.ParentWindow := ParentWindow;

  XPElevationStruct.ControlFlags := ControlFlags;

  if not CheckPipe(WriteFile(Pipe, @XPElevationStruct, sizeof(XPElevationStruct), nil, @OvLapW)) then
  begin
    result := E_SERVICE_CONTACT_FAILED;
    exit;
  end;

  if Length(Parameters) > 0 then
  begin
    GetMem(PW, XPElevationStruct.ParameterCharCount*sizeof(WideChar)+4);

    StringCchCopyW(PW, XPElevationStruct.ParameterCharCount*sizeof(WideChar)+4, @Parameters[1]);

    try
      if not CheckPipe(WriteFile(Pipe, {PW}@Parameters[1],
        XPElevationStruct.ParameterCharCount*sizeof(WideChar), nil, @OvLapW)) then
      begin
        result := E_SERVICE_CONTACT_FAILED;
        exit;
      end;
    finally
      FreeMem(PW);
    end;
  end;

  if not CheckPipe(ReadFile(Pipe, @ServiceResult, sizeof(ServiceResult), nil, @OvLapR)) then
  begin
    result := E_SERVICE_CONTACT_FAILED;
    Exit;
  end;

  ServiceResult[0] := E_SERVICE_TIME_OUT;
  if JwWaitForMultipleObjects([OvLapR.hEvent],false, {200000}Timeout) = WAIT_OBJECT_0 then
    ServiceResult[0] := S_OK;


  //MessageBox(0,PChar(Format('%x',[ServiceResult[0]])),'',MB_OK);
  result := ServiceResult[0];
  PID := ServiceResult[1];

end;

var
  XP : IXPElevation;
  //XP : TXPElevation;
  PID: Integer;
  HRes : HRESULT;
begin
  CoInitialize(nil);

  XP := CoXPElevation.Create;
  HRES := XP.ExecuteProcess('C:\Windows\system32\cmd.exe',// const ApplicationPath: WideString;
      '/k echo Hello',//const Parameters: WideString;
      '',//const CurrentDirectory: WideString;
      GetForegroundWindow,//ParentWindow: LongWord;
      XPCTRL_FORCE_NO_ERROR_DIALOGS or
      XPCTRL_FORCE_BACKGROUND_IMAGE,
      {XPCTRL_FORCE_NO_ALTERNATE_LOGON or
      XPCTRL_FORCE_NO_CACHE_CREDENTIALS or
      XPCTRL_FORCE_NO_ERROR_DIALOGS or
      XPCTRL_FORCE_BACKGROUND_IMAGE}
      PID//out PID: Integer
  );
  if HRes <> S_OK then
  begin
    ShowMEssage(Format('HRESULT: (%x)%s%sLastError: (%d)%s',[
      HRes, XPElevationSysMessage(Hres), #13#10, GetLastError, SysErrorMessage(GetLastError)]));
  end;  
end.


 { HRes := ExecuteProcess('C:\Windows\system32\cmd.exe',// const ApplicationPath: WideString;
      '/k echo hello',//const Parameters: WideString;
      '',//const CurrentDirectory: WideString;
      $40272,//ParentWindow: LongWord;
      XPCTRL_FORCE_NO_ALTERNATE_LOGON or
      XPCTRL_FORCE_NO_CACHE_CREDENTIALS or
      XPCTRL_FORCE_NO_ERROR_DIALOGS or
      XPCTRL_FORCE_BACKGROUND_IMAGE,
      PID//out PID: Integer
      );    }

