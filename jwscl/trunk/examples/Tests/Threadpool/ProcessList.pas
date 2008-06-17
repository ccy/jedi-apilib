unit ProcessList;

interface
uses JwaWindows, Classes, SysUtils;

type
  PProcessEntry = ^TProcessEntry;
  TProcessEntry = record
    Handle : THandle;
    ID,
    Session,
    Error : DWORD;
    Checked : Boolean;
  end;

  TProcessEntries = array of TProcessEntry;

  TOnCloseAppsPrePrep = procedure(const SessionID : DWORD; out ProcessHandle : THandle) of object;
  TOnCloseAppsPostPrep = procedure(const SessionID : DWORD; const Processes : TProcessEntries) of object;



  TProcessList = class
  protected
    fCloseAll : Boolean;
    fOnCloseAppsPrePrep  : TOnCloseAppsPrePrep;
    fOnCloseAppsPostPrep : TOnCloseAppsPostPrep;
    fWaitHandles,
    fContext,
    fList : TList;
    fCritSec : TMultiReadExclusiveWriteSynchronizer;

    function GetProcessHandle(Index : Integer) : THandle;
    function GetProcessID(Index : Integer) : DWORD;


  public
    constructor Create;
    destructor Destroy;

    function Add(const Handle : THandle; const Duplicate : Boolean = false) : Integer;
    procedure Remove(const Index : Integer; const Close : Boolean = true);

    procedure CloseAll;

    class procedure SendEndSessionMessage(const Wnd : HWND);

  public
    property CloseAllOnDestroy : Boolean read fCloseAll write fCloseAll;
    property Process[Index : Integer] : THandle read GetProcessHandle; default;
    property ProcessID[Index : Integer] : DWORD read GetProcessID;

    property OnCloseAppsPrePrep  : TOnCloseAppsPrePrep read fOnCloseAppsPrePrep write fOnCloseAppsPrePrep;
    property OnCloseAppsPostPrep : TOnCloseAppsPostPrep read fOnCloseAppsPostPrep write fOnCloseAppsPostPrep;
  end;

implementation
uses JwsclToken, JwsclComUtils;

{ TProcessList }



function CreateProcessEntry(const Handle : THandle; Session : DWORD) : PProcessEntry;
begin
  New(result);
  result.Handle := Handle;
  result.ID     := GetProcessId(Handle);
  result.Session := Session;
  result.Error   := 0;
  result.Checked := false;

end;

type
  PCallbackContext = ^TCallbackContext;
  TCallbackContext = record
    Self : TProcessList;
    ID : DWORD;
  end;

procedure WaitOrTimerCallback(lpParameter : Pointer; TimerOrWaitFired : Boolean) stdcall;
var
  Data : PCallbackContext;
  i : Integer;
begin
  Data := PCallbackContext(lpParameter);
  Data.Self.fCritSec.BeginWrite;

  if Assigned(Data.Self.fList) then
  try
    for I := 0 to Data.Self.fList.Count - 1 do
    begin
      if Data.Self.ProcessID[i] = Data.ID then
      begin
        Data.Self.Remove(i, false);
        break;
      end;
    end;
  finally
    Data.Self.fCritSec.EndWrite;
  end;
end;

function TProcessList.Add(const Handle: THandle;
  const Duplicate: Boolean): Integer;
var
  T : TJwSecurityToken;
  WaitHandle : THandle;

  CallbackContext : PCallbackContext;
begin
  T := TJwSecurityToken.CreateTokenByProcess(Handle, TOKEN_READ or TOKEN_QUERY);
  TJwAutoPointer.Wrap(T);

  New(CallbackContext);
  CallbackContext.Self := Self;
  CallbackContext.ID := GetProcessId(Handle);

  fCritSec.BeginWrite;
  try
    WaitHandle := INVALID_HANDLE_VALUE;
    if not RegisterWaitForSingleObject(
       WaitHandle,//_out     PHANDLE phNewWaitObject,
       Handle,//__in      HANDLE hObject,
       @WaitOrTimerCallback,//__in      WAITORTIMERCALLBACK Callback,
       CallbackContext,//_in_opt  PVOID Context,
       INFINITE,//__in      ULONG dwMilliseconds,
       WT_EXECUTEDEFAULT//__in      ULONG dwFlags
      ) then
    begin
      Dispose(CallbackContext);
      RaiseLastOSError;
    end;

    result := fList.Add(CreateProcessEntry(Handle,T.TokenSessionId));
    fContext.Add(CallbackContext);

    if fWaitHandles.IndexOf(Pointer(WaitHandle)) < 0 then
      fWaitHandles.Add(Pointer(WaitHandle));
  finally
    fCritSec.EndWrite;
  end;
end;

procedure TProcessList.CloseAll;
var
  i,i2 : Integer;
  P : PProcessEntry;
  Sessions : array of TProcessEntries;
  ProcessHandle : THandle;
begin
  fCritSec.BeginWrite;

  try
    //create a list of sessions that each contains
    //a list of indexes of the fList
    SetLength(Sessions, 1);
    for i := 0 to fList.Count - 1 do
    begin
      P := PProcessEntry(fList[i]);

      if P.Session > high(Sessions) then
        SetLength(Sessions, P.Session);

      SetLength(Sessions[P.Session], high(Sessions[P.Session])+1);
    
      //create copy of this entry
      Sessions[P.Session, high(Sessions[P.Session])+1] := PProcessEntry(fList[i])^;

      P.Checked := not P.Checked;
    end;

    for i := 0 to Length(Sessions) - 1 do
    begin
      if Length(Sessions[i]) > 0 then
      begin
        //let process handles be created into the target session
        OnCloseAppsPrePrep(i, ProcessHandle);

        for i2 := 0 to Length(Sessions[i]) - 1 do
        begin
          if not DuplicateHandle(
            GetCurrentProcess,//hSourceProcessHandle: HANDLE;
            Sessions[i,i2].Handle,//hSourceHandle: HANDLE;
            ProcessHandle,//hTargetProcessHandle: HANDLE;
            @Sessions[i,i2].Handle,//lpTargetHandle: LPHANDLE;
            0,//dwDesiredAccess: DWORD;
            false,//bInheritHandle: BOOL;
            DUPLICATE_SAME_ACCESS//dwOptions: DWORD): BOOL;
          ) then
            Sessions[i,i2].Error := GetLastError;
        end;

        //send new process handles 
        OnCloseAppsPostPrep(i, Sessions[i]);
      end;
    end;
  finally
    fCritSec.EndWrite;
  end;
end;

constructor TProcessList.Create;
begin
  fList := TList.Create;
  fWaitHandles := TList.Create;
  fContext := TList.Create;
  fCritSec := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TProcessList.Destroy;
 procedure ClearContext;
 var i : Integer;
 begin
   for i := 0 to fContext.Count - 1 do
    Dispose(fContext[i]);
 end;

var i : Integer;
begin
  for I := 0 to fWaitHandles.Count - 1 do
    UnregisterWait(DWORD(fWaitHandles[i]));

  fCritSec.BeginWrite;
  try
    ClearContext;

    FreeAndNil(fContext);
    FreeAndNil(fList);
    FreeAndNil(fWaitHandles);
  finally
    fCritSec.EndWrite;
  end;

  FreeAndNil(fCritSec);
end;

function TProcessList.GetProcessHandle(Index: Integer): THandle;
begin
  fCritSec.BeginRead;
  try

  finally
    fCritSec.EndRead;
  end;
end;

function TProcessList.GetProcessID(Index: Integer): DWORD;
begin
  fCritSec.BeginRead;
  try

  finally
    fCritSec.EndRead;
  end;
end;

procedure TProcessList.Remove(const Index: Integer; const Close: Boolean);
begin
  fCritSec.BeginWrite;
  try
    if fList[Index] <> nil then
    begin
      Dispose(PProcessEntry(fList[Index]));
      fList.Delete(Index);
    end;
  finally
    fCritSec.EndWrite;
  end;
end;

class procedure TProcessList.SendEndSessionMessage(const Wnd: HWND);
begin

end;

end.
