unit HandleRequestThread;

interface

uses
  Classes, JwaWindows, ElevationHandler, JwsclToken, JwsclSid,
  JwsclDescriptor, JwsclACL, JwsclKnownSid, JwsclComUtils, JwsclVersion,
  JwsclUtils, SessionPipe, SysUtils, JwsclLogging, uLogging, JwsclProcess,
  XPElevationCommon,
  ThreadedPasswords;

type
  THandleRequestThread = class(TJwThread)
  private
    { Private declarations }
    fAllowedSIDs:  TJwSecurityIdList;
    fPasswords   : TCredentialsList;
    fThreadsStoppedEvent,
    fServiceStopEvent  : THandle;
    fOnServiceProcessRequest : TOnServiceProcessRequest;
    fStopState : PBoolean;
    fPipeToken : TJwSecurityToken;
    fPipeID : DWORD;

    procedure IncreaseRequestThreadCount;
    procedure DecreaseRequestThreadCount;
  public
    constructor Create(
        CreateSuspended: Boolean;
        const PipeID : DWORD;
        const PipeToken : TJwSecurityToken;
        const AllowedSIDs:  TJwSecurityIdList;
        const Passwords   : TCredentialsList;
        const ServiceStopEvent : THandle;
        const ThreadsStoppedEvent : THandle;

        const OnServiceProcessRequest : TOnServiceProcessRequest;
        const StopState : PBoolean);

    procedure Execute; override;

    class function CreatePipeName(const ID : Integer) : WideString;
  end;

implementation
uses MainUnit, Math, ComObj;

{ THandleRequestThread }

constructor THandleRequestThread.Create(
  CreateSuspended: Boolean;
  const PipeID : DWORD;
  const PipeToken : TJwSecurityToken;
  const AllowedSIDs: TJwSecurityIdList;
  const Passwords: TCredentialsList;
  const ServiceStopEvent: THandle;
  const ThreadsStoppedEvent : THandle;
  const OnServiceProcessRequest: TOnServiceProcessRequest;
  const StopState: PBoolean);
begin

  fAllowedSIDs := AllowedSIDs;
  fPasswords := Passwords;
  fServiceStopEvent := ServiceStopEvent;
  fThreadsStoppedEvent := ThreadsStoppedEvent;
  fOnServiceProcessRequest := OnServiceProcessRequest;
  fStopState := StopState;
  fPipeID := PipeID;
  fPipeToken := PipeToken;


  inherited Create(CreateSuspended,'HandleRequest');
end;

procedure THandleRequestThread.IncreaseRequestThreadCount;
begin
  if InterlockedExchangeAdd(XPService.fHReqThreadCount, 1) = 0 then
    ResetEvent(fThreadsStoppedEvent);
end;

class function THandleRequestThread.CreatePipeName(const ID : Integer) : WideString;
begin
  result := '\\.\pipe\XPElevationPipe_'+IntToStr(ID);
end;

procedure THandleRequestThread.Execute;

function CheckStruct(
  const XPElevationStruct : TXPElevationStruct;
  out HRes : HRESULT) : Boolean;
var
  Log : IJwLogClient;
begin
  Log := uLogging.LogServer.Connect(etThread,ClassName,'CheckStruct','HandleRequestThread.pas','');


  HRes := S_OK;

  if XPElevationStruct.Size <> sizeof(XPElevationStruct) then
  begin
    Log.Log('XPElevation data struct size is invalid');
    HRes := E_INVALID_RECORD_SIZE;
  end
  else
  if Length(XPElevationStruct.ApplicationName) = 0 then
    HRes := E_INVALID_APPLICATION_NAME;
  {else
  if XPElevationStruct.Flags then}


  result := SUCCEEDED(hRes);

end;

function CreateClientPipe(Log : IJwLogClient) : THandle;
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
    SID := fPipeToken.GetTokenUser;

    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwLocalSystemSID,false));
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL or WRITE_DAC or WRITE_OWNER,JwAdministratorsSID,false));
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,SID,true));
    try
      SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],GENERIC_ALL,JwNetworkServiceSID,false));
    except
    end;
{$ENDIF}
    pSA := SD.Create_SA();

    try
      result := CreateNamedPipeW(PWideChar(CreatePipeName(fPipeID)),
        PIPE_ACCESS_DUPLEX
          or FILE_FLAG_OVERLAPPED
          or WRITE_DAC or WRITE_OWNER {or ACCESS_SYSTEM_SECURITY},
        PIPE_WAIT, PIPE_UNLIMITED_INSTANCES, 0, 0, 0, LPSECURITY_ATTRIBUTES(pSA));

      if result = INVALID_HANDLE_VALUE then
      begin
        LogAndRaiseLastOsError(Log,ClassName, 'ServiceExecute::(winapi)CreateNamedPipe OUT', 'ElevationHandler.pas');
        abort;
      end;
    finally
      TJwSecurityDescriptor.Free_SA(pSA);
    end;
  finally
    SD.Free;
  end;
end;

var
  AppName: string; PipeSize: Cardinal;
  OvLappedR,
  OvLappedW: OVERLAPPED;
  ElevationObj : TElevationHandler;
  Log : IJwLogClient;
  hRes : HRESULT;
  XPElevationStruct : TXPElevationStruct;
  hPipe : THandle;
  ServiceResult : array[0..1] of DWORD;
  StructParameter : WideString;
  Data : array[0..100] of widechar;

const TIMEOUT = {$IFNDEF DEBUG}INFINITE{$ELSE}5000{$ENDIF};
begin
  Self.Name := 'HandleRequest Pipe: '+CreatePipeName(fPipeID);

  Log := uLogging.LogServer.Connect(etThread,ClassName,'Execute','HandleRequestThread.pas','Init Requestthread: '+Name);
  TJwAutoPointer.Wrap(fPipeToken);

  //count number of threads
  IncreaseRequestThreadCount;

  try
    hPipe := CreateClientPipe(Log);
    TJwAutoPointer.Wrap(hPipe);

    ZeroMemory(@OvLappedR, sizeof(OvLappedR));
    OvLappedR.hEvent := CreateEvent(nil, false, false, nil);
    TJwAutoPointer.Wrap(OvLappedR.hEvent);

    ZeroMemory(@OvLappedW, sizeof(OvLappedW));

    ConnectNamedPipe(hPipe, @OvLappedR);
    if JwWaitForMultipleObjects([fServiceStopEvent, OvLappedR.hEvent],false,TIMEOUT) <> WAIT_OBJECT_0+1 then
    begin
      Log.Log('Wait for request client failed or timed out: '+SysErrorMessage(GetLastError));
      exit;
    end;



    try
      if (fStopState <> nil) and (fStopState^) then
      begin
        Log.Log(lsStop,'XPService.Stopped = true ');
        exit;
      end;


      try
         //send timeout of the service
        hres := MAX(TIMEOUT, USERTIMEOUT);
        if not CheckPipe(WriteFile(hPipe, @hres, sizeof(hres), nil, @OvLappedW)) then
          LogAndRaiseLastOsError(Log,ClassName,'ReadFile@Execute','');

        if not CheckPipe(ReadFile(hPipe, nil, 0, nil, @OvLappedR)) then
          LogAndRaiseLastOsError(Log,ClassName,'ReadFile@Execute','');

        case JwWaitForMultipleObjects([OvLappedR.hEvent, XPService.ServiceStopEvent], false, TIMEOUT) of
          WAIT_TIMEOUT:
          begin
            Log.Log('HandleRequestThread was timed out');
            hres := E_RESPONSE_TIME_OUT;
            WriteFile(hPipe, @hres, sizeof(hres), nil, @OvLappedW);

            exit;
          end;
          WAIT_OBJECT_0+1:
          begin
            hres := E_SERVICE_SHUTDOWN;
            WriteFile(hPipe, @hres, sizeof(hres), nil, @OvLappedW);

            Log.Log('Server shutdown registered.');
            exit;
          end;
        end;

        if not CheckPipe(ReadFile(hPipe, @XPElevationStruct, sizeof(XPElevationStruct), nil, @OvLappedR)) then
          LogAndRaiseLastOsError(Log,ClassName,'ReadFile@Execute','')
        else
        begin
          Log.Log('Waiting for client data to be received...');

          if JwWaitForMultipleObjects([OvLappedR.hEvent, XPService.ServiceStopEvent], false, TIMEOUT) = WAIT_OBJECT_0 then
          begin
            ServiceResult[1] := 0;
            if CheckStruct(XPElevationStruct, hRes) then
            begin
              hRes := S_OK;

              if XPElevationStruct.ParameterCharCount > 0 then
              begin
                SetLength(XPElevationStruct.Parameters, XPElevationStruct.ParameterCharCount);
                if not CheckPipe(ReadFile(hPipe, @XPElevationStruct.Parameters[1]{@Data},
                   XPElevationStruct.ParameterCharCount*sizeof(WideChar), nil, @OvLappedR)) then
                  LogAndRaiseLastOsError(Log,ClassName,'ReadFile@Execute','');

                if JwWaitForMultipleObjects([OvLappedR.hEvent, XPService.ServiceStopEvent], false, TIMEOUT) = WAIT_OBJECT_0 then
                begin

                  hRes := S_OK;
                end
                else
                  hRes := E_RESPONSE_TIME_OUT;
              end;

              if hRes = S_OK then
              begin


                if (TJwWindowsVersion.IsWindowsXP(true) or
                  TJwWindowsVersion.IsWindows2003(true)) and
                    (GetSystemMetrics(SM_SHUTTINGDOWN) <> 0) then
                  hRes := E_ABORTED;

                if hRes = S_OK then
                begin
                  TJwSecurityToken.ImpersonateNamedPipeClient(hPipe);
                  //impersonated thread is used in elevation handler
                  
                  ElevationObj := TElevationHandler.Create(
                     fAllowedSIDs,
                     fPasswords,
                     fServiceStopEvent,
                     fStopState);
                  try
                    hRes := ElevationObj.StartApplication(XPElevationStruct, ServiceResult[1]);
                    TJwSecurityToken.RevertToSelf;
                  except
                    on E : Exception do
                    begin
                      Log.Exception(E);
                      hres := E_SERVICE_FAILED;
                    end;
                  end;

                  ElevationObj.Free;
                end;
              end;
            end;
          end
          else
            hRes := E_RESPONSE_TIME_OUT;

          ServiceResult[0] := hres;
          if not WriteFile(hPipe, @ServiceResult, sizeof(ServiceResult), nil, @OvLappedW) then
          begin
            LogAndRaiseLastOsError(log,ClassName,'Execute','HandleRequestThread');
          end;
        end;

        //Sleep(10*1000);
        FlushFileBuffers(hPipe);

      finally

      end;

    finally
      DecreaseRequestThreadCount;
      //throws exception because of XPElevationStruct??? if not done
      ZeroMemory(@XPElevationStruct, sizeof(XPElevationStruct));
    end;
  except
    on E : Exception do
     Log.Exception(E);
  end;

end;

procedure THandleRequestThread.DecreaseRequestThreadCount;
begin
  if InterlockedExchangeAdd(XPService.fHReqThreadCount, -1) = 1 then
    SetEvent(fThreadsStoppedEvent);
end;

end.
