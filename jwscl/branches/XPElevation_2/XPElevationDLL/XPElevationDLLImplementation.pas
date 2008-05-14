unit XPElevationDLLImplementation;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  JwaWindows, ActiveX, Classes, ComObj, XPElevationDLL_TLB, StdVcl;

type
  TXPElevation = class(TTypedComObject, IXPElevation)
  protected
    function ExecuteProcess(const ApplicationPath, Parameters,
      CurrentDirectory: WideString; ParentWindow, Flags: LongWord;
      out PID: Integer): HResult; stdcall;
    {IXPElevation-Methoden hier deklarieren}
  end;

implementation

uses ComServ, JwsclComUtils, JwsclUtils;


const
  E_INVALID_USER = $83E70001;
  E_ABORTED = $83E70002;
  E_CREATEPROCESS_FAILED = $83E70003;
  E_RESPONSE_TIME_OUT = $83E70004;
  E_SERVICE_SHUTDOWN = $83E70005;
  E_INVALID_RECORD_SIZE = $83E70006;
  E_SERVICE_OUT_OF_ORDER = $83E70007;
  E_SERVICE_CONTACT_FAILED = $83E70008;
  E_INVALID_PARAMETER = $83E70009;


type
  TXPElevationStruct = record
    Size : DWORD;

    ParentWindow : HWND;

    ApplicationName  : array[0..MAX_PATH] of WideChar;
    CurrentDirectory : array[0..MAX_PATH] of WideChar;
    Parameters : WideString; {in public: Pointer. Must be nil}

    Flags : DWORD;
    StartupInfo : TStartupInfoW;

    ParameterCharCount : DWORD;

    Reserved1,
    Reserved2 : DWORD;
  end;

const PIPE_NAME =  '\\.\pipe\XPCredentials';

function TXPElevation.ExecuteProcess(const ApplicationPath, Parameters,
  CurrentDirectory: WideString; ParentWindow, Flags: LongWord;
  out PID: Integer): HResult;
var
  Pipe : THandle;
  XPElevationStruct : TXPElevationStruct;
  TimeOut : DWORD;
  read : DWORD;
  OvLapped : TOverlapped;
  pOvLapped : POverlapped;
  Timer : THandle;
begin
  SetLastError(0);

  MessageBox(0,'debug','',MB_OK);

  if not WaitNamedPipe(PIPE_NAME, 5{sec} *1000) then
  begin
    result := E_SERVICE_OUT_OF_ORDER;
    exit;
  end;

  Pipe := CreateFile(PIPE_NAME, GENERIC_WRITE, 0, nil, OPEN_EXISTING, SECURITY_IMPERSONATION, 0);

  if Pipe = INVALID_HANDLE_VALUE then
  begin
    result := E_SERVICE_CONTACT_FAILED;
    Exit;
  end;

  TJwAutoPointer.Wrap(Pipe);

  if not ReadFile(Pipe, @TimeOut, sizeof(TimeOut), nil, nil) then
  begin
    result := E_SERVICE_CONTACT_FAILED;
    Exit;
  end;

  ZeroMemory(@XPElevationStruct, sizeof(XPElevationStruct));

  //E_INVALID_PARAMETER

  if not WriteFile(Pipe, @XPElevationStruct, sizeof(XPElevationStruct), nil, nil) then
  begin
    result := E_SERVICE_CONTACT_FAILED;
    exit;
  end;


  if (TimeOut <> 0) and (TimeOut <> INFINITE) then
  begin
    ZeroMemory(@OvLapped,sizeof(OvLapped));
    OvLapped.hEvent := JwCreateWaitableTimer(Timeout+2000);
    TJwAutoPointer.Wrap(OvLapped.hEvent);
    pOvLapped := @OvLapped;
  end
  else
    pOvLapped := nil;

  if not ReadFile(Pipe, @result, sizeof(result), nil, pOvLapped) then
  begin
    result := E_SERVICE_CONTACT_FAILED;
    Exit;
  end;

  result := S_OK;
end;

initialization
  TTypedComObjectFactory.Create(ComServer, TXPElevation, Class_XPElevation,
    ciMultiInstance, tmApartment);
end.
