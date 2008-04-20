unit SessionPipe;

interface
uses SysUtils, JwaWindows, ComObj, SvcMgr, ULogging, JwsclUtils, JwsclLogging,
    JwsclExceptions;

type
  PClientBuffer = ^TClientBuffer;
  TClientBuffer = record
    Signature : array[0..15] of Char;
    UserName  :  array[0..UNLEN] of WideChar;
    Domain      : array[0..MAX_DOMAIN_NAME_LEN] of WideChar;
    Password  : array[0..MAX_PASSWD_LEN] of WideChar;
    Flags     : DWORD;
  end;

const CLIENT_CANCELED = $1;
      CLIENT_USECACHECREDS = $2;
      CLIENT_CACHECREDS = $3;
      CLIENT_CLEARCACHE = $4;

      SERVER_TIMEOUT = $1;
      SERVER_USECACHEDCREDS = $2;
      SERVER_CACHEAVAILABLE = $3;

      FILL_PASSWORD : WideString = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX';

      ERROR_CREATEPROCESSASUSER_FAILED = 1;
      ERROR_INVALID_USER = 2;
      ERROR_ABORTBYUSER = 3;
      ERROR_LOGONUSERFAILED = 4;
      ERROR_LOADUSERPROFILE = 5;


type
  PServerBuffer = ^TServerBuffer;
  TServerBuffer = record
    Signature   : array[0..15] of Char;
    Application : array[0..MAX_PATH] of WideChar;
    UserName    : array[0..UNLEN] of WideChar;
    Domain      : array[0..MAX_DOMAIN_NAME_LEN] of WideChar;
    TimeOut   : DWORD;
    Flags     : DWORD;
  end;

  TSessionInfo = record
    Application,
    UserName,
    Domain,
    Password  : WideString;
    Flags  : DWORD;
  end;

  ETimeOutException = class(Exception);
  EShutdownException = class(Exception);

  TOnServiceProcessRequest = procedure (WaitForMessage: Boolean) of object;

  TSessionPipe = class(TObject)
  protected
    fPipe : HANDLE;
    fTimeOut : DWORD;
    OvLapped : TOverlapped;
    fEvent : HANDLE;
  protected

  public
    constructor Create();
    destructor Destroy(); override;
    procedure Connect(const PipeName : WideString); virtual;

    class function IsValidPipe(const SessionPipe : TSessionPipe) : Boolean;

    property ServerTimeOut : DWORD read fTimeOut;
    property Handle : HANDLE read fPipe;
  end;

  TClientSessionPipe = class(TSessionPipe)
  private
    { private-Deklarationen }

  protected
    { protected-Deklarationen }
  public
    { public-Deklarationen }
    constructor Create();
    destructor Destroy(); override;

    procedure ReadServerData(out SessionInfo : TSessionInfo);
    procedure SendClientData(const SessionInfo : TSessionInfo);

    procedure ReadServerProcessResult(out Value, LastError : DWORD;
        const StopEvent: HANDLE);
  published
    { published-Deklarationen }
  end;

  TServerSessionPipe = class(TSessionPipe)
  private
    { private-Deklarationen }

  protected
    { protected-Deklarationen }
  public
    { public-Deklarationen }
    constructor Create();
    destructor Destroy(); override;

    procedure Assign(const PipeHandle : THandle;
                    TimeOut : DWORD);

    function WaitForClientToConnect(const ProcessID, TimeOut: DWORD;
      const StopEvent: HANDLE): DWORD;

    function WaitForClientAnswer(const TimeOut: DWORD;
      const StopEvent: HANDLE): DWORD;


    procedure SendServerData(const SessionInfo : TSessionInfo);
    procedure SendServerResult(const Value, LastError : DWORD);

    procedure ReadClientData(out SessionInfo : TSessionInfo);

//    procedure SendServerProcessResult(const Value, LastError : DWORD);


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


{ TClientSessionPipe }



constructor TClientSessionPipe.Create();
begin
  inherited;
end;

destructor TClientSessionPipe.Destroy;
begin

  inherited;
end;

procedure TClientSessionPipe.ReadServerData(out SessionInfo: TSessionInfo);
var ServerBuffer : TServerBuffer;
    lpNumberOfBytesRead,
    nNumberOfBytesToRead : DWORD;

    Log : IJwLogClient;

begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'ReadServerData','ElevationHandler.pas','');

  if not ReadFile(
    fPipe,//__in         HANDLE hFile,
    @ServerBuffer,//__out        LPVOID lpBuffer,
    sizeof(ServerBuffer),//__in         DWORD nNumberOfBytesToRead,
    @lpNumberOfBytesRead,//__out_opt    LPDWORD lpNumberOfBytesRead,
    nil//@OvLapped//__inout_opt  LPOVERLAPPED lpOverlapped
  ) then
    LogAndRaiseLastOsError(Log,ClassName, 'ReadServerData::(Winapi)ReadFile','SessionPipe.pas');

  ZeroMemory(@SessionInfo, sizeof(SessionInfo));
  SetLength(SessionInfo.Application,
    StringCbLengthHelperW(ServerBuffer.Application, sizeof(ServerBuffer.Application)));
  SetLength(SessionInfo.UserName,
    StringCbLengthHelperW(ServerBuffer.UserName, sizeof(ServerBuffer.UserName)));
  SetLength(SessionInfo.Domain,
    StringCbLengthHelperW(ServerBuffer.Domain, sizeof(ServerBuffer.Domain)));


  OleCheck(StringCchCopyW(@PWideChar(SessionInfo.Application)[1],
    Length(SessionInfo.Application), ServerBuffer.Application));
  //SessionInfo.Application := ServerBuffer.Application;

  OleCheck(StringCchCopyW(@PWideChar(SessionInfo.UserName)[1],
    Length(SessionInfo.UserName), ServerBuffer.UserName));
  //SessionInfo.UserName  := ServerBuffer.UserName;

  OleCheck(StringCchCopyW(@PWideChar(SessionInfo.Domain)[1],
    Length(SessionInfo.Domain), ServerBuffer.Domain));
  //SessionInfo.Domain    := ServerBuffer.Domain;

  SessionInfo.Flags     := ServerBuffer.Flags;
  fTimeOut := ServerBuffer.TimeOut;

  ZeroMemory(@ServerBuffer, sizeof(ServerBuffer));
end;

procedure TClientSessionPipe.ReadServerProcessResult(out Value,
  LastError: DWORD; const StopEvent: HANDLE);
var ClientBuffer : TClientBuffer;
    NumBytesRead : DWORD;
    Log : IJwLogClient;

    Data, P : Pointer;
begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'ReadServerProcessResult','ElevationHandler.pas','');
  ZeroMemory(@ClientBuffer, sizeof(ClientBuffer));

  GetMem(Data, sizeof(Value) + sizeof(LastError));
  try
    if not ReadFile(
       fPipe,//__in         HANDLE hFile,
       Pointer(@Data),//__out        LPVOID lpBuffer,
       sizeof(Value) + sizeof(LastError),//__in         DWORD nNumberOfBytesToRead,
       @NumBytesRead,//__out_opt    LPDWORD lpNumberOfBytesRead,
       @OvLapped//__inout_opt  LPOVERLAPPED lpOverlapped
        ) then
    begin
      LogAndRaiseLastOsError(Log,ClassName, 'ReadServerProcessResult::(Winapi)ReadFile','SessionPipe.pas');
    end;

    CopyMemory(@Value, Data, sizeof(Value));
    P := Data;
    Inc(DWORD(P), sizeof(LastError));
    CopyMemory(@LastError, P, sizeof(LastError));
  finally
    FreeMem(Data);
  end;
end;

procedure TClientSessionPipe.SendClientData(const SessionInfo: TSessionInfo);
var ClientBuffer : TClientBuffer;
    nNumberOfBytesToRead : DWORD;
    Log : IJwLogClient;

begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'SendClientData','ElevationHandler.pas','');

  ZeroMemory(@ClientBuffer, sizeof(ClientBuffer));

  ClientBuffer.Signature := 'client';
  try
    OleCheck(StringCbCopyW(ClientBuffer.UserName, sizeof(ClientBuffer.UserName),
      @PWideChar(SessionInfo.UserName)[1]));
    //lstrcpynW(@ClientBuffer.UserName, PWideChar(SessionInfo.UserName), sizeof(ClientBuffer.UserName)-1);

    OleCheck(StringCbCopyW(ClientBuffer.Domain, sizeof(ClientBuffer.Domain),
      @PWideChar(SessionInfo.Domain)[1]));
    //lstrcpynW(@ClientBuffer.Domain, PWideChar(SessionInfo.Domain), sizeof(ClientBuffer.Domain)-1);

    OleCheck(StringCbCopyW(ClientBuffer.Password, sizeof(ClientBuffer.Password),
      @PWideChar(SessionInfo.Password)[1]));
    //lstrcpynW(@ClientBuffer.Password, PWideChar(SessionInfo.Password), sizeof(ClientBuffer.Password)-1);

    
    ClientBuffer.Flags := SessionInfo.Flags;

    if not WriteFile(
      fPipe,//__in         HANDLE hFile,
      @ClientBuffer,//__out        LPVOID lpBuffer,
      sizeof(ClientBuffer),//__in         DWORD nNumberOfBytesToRead,
      nil,//
      nil//
    ) then
    begin
      LogAndRaiseLastOsError(Log,ClassName, 'Connect::(Winapi)WriteFile','SessionPipe.pas');
    end;
  finally
    ZeroMemory(@ClientBuffer, sizeof(ClientBuffer));
  end;

end;

{ TSessionPipe }

procedure TSessionPipe.Connect(const PipeName: WideString);
var NewMode : DWORD;
    Log : IJwLogClient;

begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'Connect','ElevationHandler.pas','');

  fPipe := CreateFileW(
    PWideChar(PipeName),//__in      LPCTSTR lpFileName,
    GENERIC_READ or GENERIC_WRITE,//__in      DWORD dwDesiredAccess,
    0,//__in      DWORD dwShareMode,
    nil,//__in_opt  LPSECURITY_ATTRIBUTES lpSecurityAttributes,
    OPEN_EXISTING,//__in      DWORD dwCreationDisposition,
    {FILE_FLAG_OVERLAPPED}0,//__in      DWORD dwFlagsAndAttributes,
    0//__in_opt  HANDLE hTemplateFile
   );
  if fPipe = INVALID_HANDLE_VALUE then
  begin
    LogAndRaiseLastOsError(Log,ClassName, 'Connect::(Winapi)CreateFileW','SessionPipe.pas');
  end;


  try
    NewMode := PIPE_READMODE_MESSAGE or PIPE_WAIT;
    if not JwaWindows.SetNamedPipeHandleState(
      fPipe,//hNamedPipe: HANDLE;
      @NewMode,//lpMode: LPDWORD;
      nil,//lpMaxCollectionCount: LPDWORD;
      nil//lpCollectDataTimeout: LPDWORD
      ) then
      LogAndRaiseLastOsError(Log,ClassName, 'Connect::(Winapi)SetNamedPipeHandleState','SessionPipe.pas');
  except
    CloseHandle(fPipe);
    raise;
  end;
end;

constructor TSessionPipe.Create;
begin
  inherited;
  fPipe := INVALID_HANDLE_VALUE;

  ZeroMemory(@OvLapped, sizeof(OvLapped));
  OvLapped.hEvent := CreateEvent(nil, false, false, nil);
end;

destructor TSessionPipe.Destroy;
begin
  if (fPipe <> INVALID_HANDLE_VALUE) and
     (fPipe <> 0) then
    CloseHandle(fPipe);
  inherited;
end;

class function TSessionPipe.IsValidPipe(
  const SessionPipe: TSessionPipe): Boolean;
begin
  result := Assigned(SessionPipe) and
          (SessionPipe.fPipe <> INVALID_HANDLE_VALUE) and
          (SessionPipe.fPipe <> 0);
end;

{ TServerSessionPipe }

procedure TServerSessionPipe.Assign(const PipeHandle: THandle;
    TimeOut : DWORD);
begin
  fPipe := PipeHandle;
  fTimeOut := TimeOut;
end;

constructor TServerSessionPipe.Create;
begin

end;

destructor TServerSessionPipe.Destroy;
begin

  inherited;
end;

procedure TServerSessionPipe.ReadClientData(
  out SessionInfo: TSessionInfo);
var ClientBuffer : TClientBuffer;
    NumBytesRead : DWORD;
    Log : IJwLogClient;

begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'ReadClientData','ElevationHandler.pas','');
  ZeroMemory(@ClientBuffer, sizeof(ClientBuffer));

  try
    if not ReadFile(
       fPipe,//__in         HANDLE hFile,
       Pointer(@ClientBuffer),//__out        LPVOID lpBuffer,
       sizeof(TClientBuffer),//__in         DWORD nNumberOfBytesToRead,
       @NumBytesRead,//__out_opt    LPDWORD lpNumberOfBytesRead,
       @OvLapped//__inout_opt  LPOVERLAPPED lpOverlapped
        ) then
    begin
      LogAndRaiseLastOsError(Log,ClassName, 'ReadClientData::(Winapi)ReadFile','SessionPipe.pas');
    end;

    if NumBytesRead < sizeof(TClientBuffer) then
    begin
      SetLastError(ERROR_BAD_FORMAT);
      Log.Log(lsError, 'ReadFile returned invalid buffer size.');
      LogAndRaiseLastOsError(Log,ClassName, 'ReadClientData','SessionPipe.pas');
    end;


    if ClientBuffer.Signature <> 'client' then
    begin
      Abort;
    end;

    SetLength(SessionInfo.UserName, StringCbLengthHelperW(ClientBuffer.UserName, sizeof(ClientBuffer.UserName)));
    SetLength(SessionInfo.Domain, StringCbLengthHelperW(ClientBuffer.Domain, sizeof(ClientBuffer.Domain)));
    SetLength(SessionInfo.Password, StringCbLengthHelperW(ClientBuffer.Password, sizeof(ClientBuffer.Password)));

    OleCheck(StringCchCopyW(@PWideChar(SessionInfo.UserName)[1], Length(SessionInfo.UserName), ClientBuffer.UserName));
    //SessionInfo.UserName := ClientBuffer.UserName;

    OleCheck(StringCchCopyW(@PWideChar(SessionInfo.Domain)[1], Length(SessionInfo.Domain), ClientBuffer.Domain));
    //SessionInfo.Domain := ClientBuffer.Domain;

    OleCheck(StringCchCopyW(@PWideChar(SessionInfo.Password)[1], Length(SessionInfo.Password), ClientBuffer.Password));
    //SessionInfo.Password := ClientBuffer.Password;

    SessionInfo.Flags := ClientBuffer.Flags;
  finally
    ZeroMemory(@ClientBuffer, sizeof(ClientBuffer));
  end;
end;

procedure TServerSessionPipe.SendServerResult(const Value, LastError : DWORD);
var NumBytesWritten: DWORD;
    Log : IJwLogClient;
    A : Array[0..1] of DWORD;
begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'SendServerResult','ElevationHandler.pas','');

  A[0] := Value;
  A[1] := LastError;
  try
    if not WriteFile(
         fPipe,//hFile: HANDLE;
         Pointer(@A),//lpBuffer: LPCVOID;
         sizeof(A),//nNumberOfBytesToWrite: DWORD;
         @NumBytesWritten,//lpNumberOfBytesWritten: LPDWORD;
         @OvLapped//lpOverlapped: LPOVERLAPPED
         ) then
    begin
      LogAndRaiseLastOsError(Log,ClassName, 'SendServerResult::(Winapi)WriteFile','SessionPipe.pas');
    end;
  finally
  end;
end;

procedure TServerSessionPipe.SendServerData(const SessionInfo: TSessionInfo);
var ServerBuffer : TServerBuffer;
    NumBytesWritten: DWORD;
    Log : IJwLogClient;

begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'SendServerData','ElevationHandler.pas','');

  ZeroMemory(@ServerBuffer, sizeof(ServerBuffer));
  try
    OleCheck(StringCbCopyA(ServerBuffer.Signature, sizeof(ServerBuffer.Signature),
        PChar('server')));

    ServerBuffer.Flags := 0;
    ServerBuffer.TimeOut := fTimeOut;

    OleCheck(StringCbCopyW(ServerBuffer.Application, sizeof(ServerBuffer.Application),
       PWideChar(WideString(SessionInfo.Application))));

    OleCheck(StringCbCopyW(ServerBuffer.UserName, sizeof(SessionInfo.UserName),
       PWideChar(WideString(ServerBuffer.UserName))));

    OleCheck(StringCbCopyW(ServerBuffer.Domain, sizeof(SessionInfo.Domain),
       PWideChar(WideString(ServerBuffer.Domain))));

    ServerBuffer.Flags := 0;

    if not WriteFile(
         fPipe,//hFile: HANDLE;
         Pointer(@ServerBuffer),//lpBuffer: LPCVOID;
         sizeof(TServerBuffer),//nNumberOfBytesToWrite: DWORD;
         @NumBytesWritten,//lpNumberOfBytesWritten: LPDWORD;
         @OvLapped//lpOverlapped: LPOVERLAPPED
         ) then
    begin
      LogAndRaiseLastOsError(Log,ClassName, 'SendServerData::(Winapi)WriteFile','SessionPipe.pas');
    end;
  finally
    ZeroMemory(@ServerBuffer, sizeof(ServerBuffer));
  end;
end;



{procedure TServerSessionPipe.SendServerProcessResult(const Value, LastError: DWORD);
var ServerBuffer : TServerBuffer;
    NumBytesWritten: DWORD;
    Log : IJwLogClient;
    Data, P : Pointer;
begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'SendServerProcessResult','ElevationHandler.pas','');

  GetMem(Data, sizeof(Value) + sizeof(Lasterror));
  CopyMemory(Data, @Value, sizeof(Value));
  P := Data;
  Inc(DWORD(P), sizeof(Value));
  CopyMemory(P, @LastError, sizeof(LastError));


  //if Assigned(ServerPipe) then

  ZeroMemory(@ServerBuffer, sizeof(ServerBuffer));
  try

    if not WriteFile(
         fPipe,//hFile: HANDLE;
         Pointer(@Data),//lpBuffer: LPCVOID;
         sizeof(Value) + sizeof(Lasterror),//nNumberOfBytesToWrite: DWORD;
         @NumBytesWritten,//lpNumberOfBytesWritten: LPDWORD;
         @OvLapped//lpOverlapped: LPOVERLAPPED
         ) then
    begin
      try
        LogAndRaiseLastOsError(Log,ClassName, 'SendServerProcessResult::(Winapi)WriteFile','SessionPipe.pas');
      except
      end;
    end;
  finally
    FreeMem(Data);
  end;
end; }

function TServerSessionPipe.WaitForClientAnswer(const TimeOut: DWORD;

      const StopEvent: HANDLE): DWORD;
var NumBytesRead,
    NumBytesToBeRead,
    Timer : DWORD;
    Data : DWORD;
    TimeOutInt64 : LARGE_INTEGER;
    fTimer : THANDLE;
    Ar: Array[0..2] of THandle;
    Log : IJwLogClient;

begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'WaitForClientAnswer','ElevationHandler.pas','');

  ZeroMemory(@TimeOutInt64,sizeof(TimeOutInt64));
  TimeOutInt64.HighPart := -1;
  TimeOutInt64.LowPart := high(TimeOutInt64.LowPart) - (Int64(TimeOut) * 1 * 100000)+1 ;

  fTimer := CreateWaitableTimer(nil, TRUE, PChar('WFClient_'+IntToStr(GetCurrentThreadId)));
  if fTimer = 0 then
  begin
    LogAndRaiseLastOsError(Log,ClassName, 'WaitForClientAnswer::(Winapi)CreateWaitableTimer','SessionPipe.pas');
  end;

  try
    if not SetWaitableTimer(fTimer, TimeOutInt64, 0, nil, nil, false) then
    begin
      LogAndRaiseLastOsError(Log,ClassName, 'WaitForClientAnswer::(Winapi)SetWaitableTimer','SessionPipe.pas');
    end;

    
    while (NumBytesToBeRead < sizeof(TClientBuffer)) do
    begin
      NumBytesToBeRead := 0;
      PeekNamedPipe(
        fPipe,//__in       HANDLE hNamedPipe,
        @Data,//__out_opt  LPVOID lpBuffer,
        sizeof(Data),//__in       DWORD nBufferSize,
        @NumBytesRead,//__out_opt  LPDWORD lpBytesRead,
        @NumBytesToBeRead,//__out_opt  LPDWORD lpTotalBytesAvail,
        @OvLapped//__out_opt  LPDWORD lpBytesLeftThisMessage
      );

      {wait for 50msec or event
        0 : StopEvent - Server shuts down
        1 : connection timeout occured
      }
      result := JwWaitForMultipleObjects([StopEvent, fTimer], false, 50);
      if result = WAIT_OBJECT_0+1 then
      begin
        Log.Log(lsWarning,'Server timeout limit reached. Aborting elevation...');
        raise ETimeOutException.Create('');
      end;
      if result = WAIT_OBJECT_0 then
      begin
        Log.Log(lsWarning,'Server shutdown introduced...');
        raise EShutdownException.Create('');
      end;

    end;
  finally
    CloseHandle(fTimer);
  end;
end;


{returns
0 - Server shuts down
1 - client connects to pipe
}
function TServerSessionPipe.WaitForClientToConnect(const ProcessID,
  TimeOut: DWORD; const StopEvent: HANDLE): DWORD;
var NumBytesRead,
    NumBytesToBeRead,

    Data : DWORD;
    TimeOutInt64 : LARGE_INTEGER;
    fTimer : THANDLE;
    Log : IJwLogClient;

begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'WaitForClientToConnect','ElevationHandler.pas','');

  ConnectNamedPipe(fPipe, @OvLapped);

  repeat
    result := JwWaitForMultipleObjects([StopEvent,OvLapped.hEvent], false, INFINITE);
  until result <> WAIT_OBJECT_0+2;

end;



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


end.
