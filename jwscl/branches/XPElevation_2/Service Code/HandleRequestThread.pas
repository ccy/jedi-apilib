unit HandleRequestThread;

interface

uses
  Classes, JwaWindows, ElevationHandler, JwsclToken, JwsclSid,
  JwsclUtils, SessionPipe, SysUtils,
  ThreadedPasswords;

type
  THandleRequestThread = class(TJwThread)
  private
    { Private declarations }
    fPipeHandle: THandle;
    fAllowedSIDs:  TJwSecurityIdList;
    fPasswords   : TPasswordList;
    fThreadsStopEvent,
    fServiceStopEvent  : THandle;
    fOnServiceProcessRequest : TOnServiceProcessRequest;
    fStopState : PBoolean;
  protected
    procedure Execute; override;
  public
    constructor Create(
        CreateSuspended: Boolean;
        const AllowedSIDs:  TJwSecurityIdList;
        const Passwords   : TPasswordList;
        const ServiceStopEvent : THandle;
        const ThreadsStopEvent : THandle;

        const OnServiceProcessRequest : TOnServiceProcessRequest;
        const StopState : PBoolean);
    property PipeHandle: THandle read fPipeHandle write fPipeHandle;
  end;

//function StringCbLengthHelper(
//    {__in}const psz : STRSAFE_LPCTSTR;
//    {__in}cbMax : size_t) : Size_t;

function StringCbLengthHelperA(
    {__in}const psz : STRSAFE_LPCSTR;
    {__in}cbMax : size_t) : Size_t;

function StringCbLengthHelperW(
    {__in}const psz : STRSAFE_LPCWSTR;
    {__in}cbMax : size_t) : Size_t;

function StringCchLengthHelperA(
    {__in}const psz : STRSAFE_LPCSTR;
    {__in}cchMax : size_t) : Size_t;

function StringCchLengthHelperW(
    {__in}const psz : STRSAFE_LPCWSTR;
    {__in}cchMax : size_t) : Size_t;

//function StringCchLengthHelper(
//    {__in}const psz : STRSAFE_LPCTSTR;
//    {__in}cchMax : size_t) : Size_t;


implementation
uses MainUnit, ComObj;
{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure THandleRequestThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ THandleRequestThread }

constructor THandleRequestThread.Create(
  CreateSuspended: Boolean;
  const AllowedSIDs: TJwSecurityIdList;
  const Passwords: TPasswordList;
  const ServiceStopEvent: THandle;
  const ThreadsStopEvent : THandle;
  const OnServiceProcessRequest: TOnServiceProcessRequest;
  const StopState: PBoolean);
begin

  fAllowedSIDs := AllowedSIDs;
  fPasswords := Passwords;
  fServiceStopEvent := ServiceStopEvent;
  fThreadsStopEvent := ThreadsStopEvent;
  fOnServiceProcessRequest := OnServiceProcessRequest;
  fStopState := StopState;

  inherited Create(CreateSuspended,'HandleRequest');
end;

procedure THandleRequestThread.Execute;
var AppName: string; PipeSize: Cardinal;
    OvLapped: OVERLAPPED;
    ar: array[0..1] of THandle;
    ElevationObj : TElevationHandler;
begin
  Self.Name := 'HandleRequest: '+IntToStr(fPipeHandle);
  if InterlockedExchangeAdd(XPService.fHReqThreadCount, 1)=0 then
   ResetEvent(fThreadsStopEvent);
  try
    if XPService.Stopped then
     exit;

    OvLapped.hEvent := CreateEvent(nil, false, false, nil);
    try
      ar[0] := OvLapped.hEvent;
      ar[1] := XPService.ServiceStopEvent;
      
      ReadFile(fPipeHandle, nil, 0, nil, @OvLapped);
      case WaitForMultipleObjects(2, @ar[0], false, 10000) of
        WAIT_TIMEOUT:
        begin
          XPService.LogEvent('HandleRequestThread was timed out');
          exit;
        end;
        WAIT_OBJECT_0+1: exit;
      end;

      PeekNamedPipe(fPipeHandle, nil, 0, nil, @PipeSize, nil);
      SetLength(AppName, PipeSize);
      ReadFile(fPipeHandle, @AppName[1], PipeSize, nil, @OvLapped);
      WaitForSingleObject(OvLapped.hEvent, INFINITE);
    //              MessageBox(0, PChar(AppName), 'App is to start', MB_SERVICE_NOTIFICATION or MB_OK);
      TJwSecurityToken.ImpersonateNamedPipeClient(fPipeHandle);

      ElevationObj := TElevationHandler.Create(
         fAllowedSIDs,
         fPasswords,
         fServiceStopEvent,
         fOnServiceProcessRequest,
         fStopState);
      try
        ElevationObj.StartApplication(AppName);
      finally
        ElevationObj.Free;
      end;
    //  TJwSecurityToken.RevertToSelf; //now made in StartApp
    finally
      CloseHandle(OvLapped.hEvent);
      DisconnectNamedPipe(fPipeHandle);
      CloseHandle(fPipeHandle);
    end;
  finally
    if InterlockedExchangeAdd(XPService.fHReqThreadCount, -1)=1 then
      SetEvent(fThreadsStopEvent);
  end;
end;


//function StringCbLengthHelper(
//    {__in}const psz : STRSAFE_LPCTSTR;
//    {__in}cbMax : size_t) : Size_t;

function StringCbLengthHelperA(
    {__in}const psz : STRSAFE_LPCSTR;
    {__in}cbMax : size_t) : Size_t;
begin
  OleCheck(StringCbLengthA(psz, cbMax, @result));
end;

function StringCbLengthHelperW(
    {__in}const psz : STRSAFE_LPCWSTR;
    {__in}cbMax : size_t) : Size_t;
begin
  OleCheck(StringCbLengthW(psz, cbMax, @result));
end;

function StringCchLengthHelperA(
    {__in}const psz : STRSAFE_LPCSTR;
    {__in}cchMax : size_t) : Size_t;
begin
  OleCheck(StringCchLengthA(psz, cchMax, @result));
end;

function StringCchLengthHelperW(
    {__in}const psz : STRSAFE_LPCWSTR;
    {__in}cchMax : size_t) : Size_t;
begin
  OleCheck(StringCchLengthW(psz, cchMax, @result));
end;

//function StringCchLengthHelper(
//    {__in}const psz : STRSAFE_LPCTSTR;
//    {__in}cchMax : size_t) : Size_t;

end.
