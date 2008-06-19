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
    Duplicated,
    Checked : Boolean;
  end;

  TProcessEntries = array of TProcessEntry;
  TProcessEntriesArray = array of TProcessEntries;
  TProcessList = class;

  TOnCloseAppsPrePrep = procedure(Sender : TProcessList; const SessionID : DWORD; out ProcessHandle : THandle) of object;
  TOnCloseAppsPostPrep = procedure(Sender : TProcessList; const SessionID : DWORD; const Processes : TProcessEntries) of object;
  TOnCloseApps = procedure(Sender : TProcessList; const Processes : TProcessEntriesArray) of object;



  TProcessList = class
  protected
    fCloseAll : Boolean;
    fOnCloseAppsPrePrep  : TOnCloseAppsPrePrep;
    fOnCloseAppsPostPrep : TOnCloseAppsPostPrep;
    fOnCloseApps : TOnCloseApps;

    fWaitHandles,
    fContext,
    fList : TList;
    fCritSec : TMultiReadExclusiveWriteSynchronizer;

    function GetProcessHandle(Index : Integer) : THandle;
    function GetProcessID(Index : Integer) : DWORD;


  public
    constructor Create;
    destructor Destroy;

    function Add(Handle : THandle; const Duplicate : Boolean = false) : Integer;
    procedure Remove(const Index : Integer; const Close : Boolean = true);

    procedure CloseAll;

    class procedure SendEndSessionMessage(const Wnd : HWND);

  public
    property CloseAllOnDestroy : Boolean read fCloseAll write fCloseAll;
    property Process[Index : Integer] : THandle read GetProcessHandle; default;
    property ProcessID[Index : Integer] : DWORD read GetProcessID;

    property OnCloseAppsPrePrep  : TOnCloseAppsPrePrep read fOnCloseAppsPrePrep write fOnCloseAppsPrePrep;
    property OnCloseAppsPostPrep : TOnCloseAppsPostPrep read fOnCloseAppsPostPrep write fOnCloseAppsPostPrep;
    property OnCloseApps : TOnCloseApps read fOnCloseApps write fOnCloseApps;
  end;

  TProcessListMemory = class
  protected
    fWriteEvent,
    fHandle : THandle;
  public
    constructor Create(const Name : WideString);
    constructor CreateOpen(const Name : WideString);
    destructor Destory;

    procedure Write(const Processes : TProcessEntries);
    procedure Read(out Processes : TProcessEntries);

    procedure FireWriteEvent;
    property WritEvent : THandle read fWriteEvent;
  end;

implementation
uses math, JwsclToken, JwsclExceptions, JwsclComUtils, JwsclCryptProvider, JwsclTypes;

{ TProcessList }

const MAP_SIZE = 1024 * 1024;



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

function TProcessList.Add(Handle: THandle;
  const Duplicate: Boolean): Integer;
var
  T : TJwSecurityToken;
  WaitHandle : THandle;

  CallbackContext : PCallbackContext;
begin
  if Duplicate then
  begin
    if not DuplicateHandle(
      GetCurrentProcess,//hSourceProcessHandle: HANDLE;
      Handle,//hSourceHandle: HANDLE;
      GetCurrentProcess,//hTargetProcessHandle: HANDLE;
      @Handle,//lpTargetHandle: LPHANDLE;
      0,//dwDesiredAccess: DWORD;
      false,//bInheritHandle: BOOL;
      DUPLICATE_SAME_ACCESS//dwOptions: DWORD): BOOL;
    ) then
      RaiseLastOSError;
  end;


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
       WT_EXECUTEDEFAULT or WT_EXECUTEONLYONCE//__in      ULONG dwFlags
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
  Sessions : TProcessEntriesArray;
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
        OnCloseAppsPrePrep(Self,i, ProcessHandle);

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
        OnCloseAppsPostPrep(Self,i, Sessions[i]);
      end;

      OnCloseApps(Self,Sessions);
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

 procedure ClearList;
 var i : Integer;
 begin
   for i := 0 to fList.Count - 1 do
   begin
     if PProcessEntry(fList[i])^.Duplicated then
       CloseHandle(PProcessEntry(fList[i])^.Handle);
     Dispose(fList[i]);
   end;
 end;

var i : Integer;
begin
  for I := 0 to fWaitHandles.Count - 1 do
    UnregisterWait(DWORD(fWaitHandles[i]));

  fCritSec.BeginWrite;
  try
    ClearContext;
    ClearList;

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
      if PProcessEntry(fList[Index]).Duplicated then
        CloseHandle(PProcessEntry(fList[Index]).Handle);
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

{ TProcessListMemory }

constructor TProcessListMemory.Create(const Name: WideString);
begin
  fHandle := CreateFileMappingW(
      INVALID_HANDLE_VALUE,//__in      HANDLE hFile,
      nil,//__in_opt  LPSECURITY_ATTRIBUTES lpAttributes,
      PAGE_READWRITE,//__in      DWORD flProtect,
      0,//__in      DWORD dwMaximumSizeHigh,
      MAP_SIZE,//__in      DWORD dwMaximumSizeLow,
      PWideChar(Name) //__in_opt  LPCTSTR lpName
      );
  if fHandle = 0 then
    RaiseLastOSError;

  fWriteEvent := CreateEventW(nil, true, false, PWideChar(Name+'_WriteEvent'));
end;

constructor TProcessListMemory.CreateOpen(const Name: WideString);
begin
  fHandle := CreateFileMappingW(
      INVALID_HANDLE_VALUE,//__in      HANDLE hFile,
      nil,//__in_opt  LPSECURITY_ATTRIBUTES lpAttributes,
      PAGE_READONLY,//__in      DWORD flProtect,
      0,//__in      DWORD dwMaximumSizeHigh,
      MAP_SIZE,//in      DWORD dwMaximumSizeLow,
      PWideChar(Name) //__in_opt  LPCTSTR lpName
      );
  if fHandle = 0 then
    RaiseLastOSError;
  fWriteEvent := 0;
end;

destructor TProcessListMemory.Destory;
begin
  CloseHandle(fHandle);
  CloseHandle(fWriteEvent);
end;

procedure TProcessListMemory.FireWriteEvent;
begin
  SetEvent(fWriteEvent);
end;

procedure TProcessListMemory.Read(out Processes: TProcessEntries);
var
  Point,
  ReadHashData,
  HashData,
  Data : Pointer;
  i,
  ReadHashLen,
  HashLen,
  Len,
  Size : DWORD;
  Hash : TJwHash;
begin
  Size := 0;
  Data := MapViewOfFile(fHandle,   // handle to map object
                        FILE_MAP_READ, // read/write permission
                        0,
                        0,
                        MAP_SIZE);
  Point := Data;
  try
    CopyMemory(@ReadHashLen, Point, sizeof(ReadHashLen));
    Inc(DWORD(Point), sizeof(ReadHashLen));

    ReadHashData := Point;

    Inc(DWORD(Point), ReadHashLen);
    HashData := Point;
    CopyMemory(@Size, Point, sizeof(Size));
    Inc(DWORD(Point), sizeof(Size));

    Hash := TJwHash.Create(haMD5);
    Hash.HashData(HashData, Size);

    HashData := Hash.RetrieveHash(HashLen);

    try
      if not CompareMem(ReadHashData, HashData, min(HashLen,ReadHashLen)) then
        raise EJwsclHashMismatch.Create('');
    finally
      Hash.FreeBuffer(HashData);
    end;



    CopyMemory(@Len, Point, sizeof(Point));
    Inc(DWORD(Point), sizeof(Point));

    SetLength(Processes, Len);
    for i := 0 to Len - 1 do
    begin
      CopyMemory(@Processes[i], Point, sizeof(Processes[i]));
      Inc(DWORD(Point), sizeof(Processes[i]));
    end;
  finally
    UnmapViewOfFile(Data);
  end;
end;

procedure TProcessListMemory.Write(const Processes: TProcessEntries);
var
  Point,
  Data,
  HashData,
  ProcData,
  PtProcData : Pointer;
  i, Len,
  HashLen,
  ProcDataSize,
  Size : DWORD;
  Hash : TJwHash;
begin
  Size := Length(Processes) * sizeof(TProcessEntry);

  Data := MapViewOfFile(fHandle,   // handle to map object
                        FILE_MAP_ALL_ACCESS, // read/write permission
                        0,
                        0,
                        MAP_SIZE);
  ProcDataSize := Size+sizeof(Size)+sizeof(Len);
  GetMem(ProcData,ProcDataSize);
  try
    Point := ProcData;
    CopyMemory(Point, @ProcDataSize, sizeof(ProcDataSize));
    Inc(DWORD(Point), sizeof(ProcDataSize));

    Len := Length(Processes);
    CopyMemory(Point, @Len, sizeof(Len));
    Inc(DWORD(Point), sizeof(Len));

    for i := 0 to Length(Processes) - 1 do
    begin
      CopyMemory(Point, @Processes[i], sizeof(Processes[i]));
      Inc(DWORD(Point), sizeof(Processes[i]));
    end;

    Hash := TJwHash.Create(haMD5);
    Hash.HashData(ProcData, ProcDataSize);
    HashData := Hash.RetrieveHash(HashLen);

    try
      Point := Data;

      CopyMemory(Point, @HashLen, sizeof(HashLen));
      Inc(DWORD(Point), sizeof(HashLen));

      CopyMemory(Point, HashData, HashLen);
      Inc(DWORD(Point), HashLen);

      CopyMemory(Point,ProcData, ProcDataSize);
    finally
      Hash.FreeBuffer(HashData);
      Hash.Free;

    end;
  finally
    UnmapViewOfFile(Data);
  end;
end;

end.
