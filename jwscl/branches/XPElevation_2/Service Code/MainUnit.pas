{
This project is released under the terms of the
GNU General Public License 3.0 (the  "GPL License").

For more information about the GPL: http://www.gnu.org/licenses/gpl-3.0.txt

Original authors are
 	Philip Dittmann
  Christian Wimmer

This application is part of the JEDI API Project.
Visit at http://blog.delphi-jedi.net/
}
unit MainUnit;

interface

uses
{$IFDEF EUREKALOG}
  ExceptionLog,
{$ENDIF EUREKALOG}
  Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs, Math, ComObj,
  Registry, 

  JwaWindows, JwsclToken, JwsclLsa, JwsclCredentials, JwsclDescriptor, JwsclDesktops,
  JwsclExceptions, JwsclSID, JwsclAcl,JwsclKnownSID, JwsclEncryption, JwsclTypes,
  JwsclProcess, JwsclSecureObjects, JwsclComUtils, JwsclVersion,
  JwsclLogging, JwsclUtils, JwsclCryptProvider, JwsclStreams,
  uLogging, //Logging utils unit
  SessionPipe, //Pipe classes
  ThreadedPasswords, //Credentials stored threadsafe in a list
  JwsclStrings;



const
  //RegKeyName of credentials app path
  CredApplicationKey='CredentialsApplication';
  //RegKey Path
  XPElevationRegKey = 'Software\XPElevation';
  
var
  //Credentials App Path read from Reg
  CredentialsAppPath : WideString = '';
  //Hash of CredApp app when the service started
  CredentialsHash : TJwFileHashData;


type
  TXPService = class(TService)
    EurekaLog1: TEurekaLog;
    procedure ServiceExecute(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceCreate(Sender: TObject);
{$IFDEF EUREKALOG}
    procedure EurekaLog1ExceptionActionNotify(
      EurekaExceptionRecord: TEurekaExceptionRecord;
      EurekaAction: TEurekaActionType; var Execute: Boolean);
{$ENDIF EUREKALOG}
  private
    { Private declarations }
    {fServiceStopEvent is signaled when this service must shut down.}
    fServiceStopEvent,
    {fThreadsStoppedEvent is signaled when all elevent threads exited.}
    fThreadsStoppedEvent:           THandle;
    {fJobs defines a list. It contains one job per Session (The Windows Session).
    All process in a job can only belong to the same session.
    }
    fJobs : TJwJobObjectSessionList;

    {fStopCriticalSection saves the variable fServiceStopEvent}
    fStopCriticalSection :  TMultiReadExclusiveWriteSynchronizer;
    //fLogFile:             Textfile;

    //fStopped is set to true if the service shuts down
    fStopped:             boolean;

    {fPasswords defines a credentials list (domain, username, password)
     per session. That means that a user in a session can save its credentials only
     in one list item. The user is identifed by the session, not by the logon name.
     The list is thread safe.
    }
    fPasswords:           TCredentialsList;



    procedure SetStopped(const Value: boolean);

    procedure OnJobNotification(Sender : TJwJobObject; ProcessId : TJwProcessId; JobLimits : TJwJobMessages; Data : Pointer);
    procedure OnNoActiveProcesses(Sender : TJwJobObject);
    procedure OnNewJobObject(Sender : TJwJobObjectSessionList;    ProcessHandle : TJwProcessHandle;
      ProcessSessionID,
      CurrentSessionID : Cardinal;
      var NewJobObject : TJwJobObject);


  public
    {Reference count for fThreadsStoppedEvent.
    If 0 the fThreadsStoppedEvent Event eill be signaled.}
    fHReqThreadCount:     Integer;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


    function GetServiceController: TServiceController; override;
    { Public declarations }

    {ThreadsStoppedEvent is signaled when all elevent threads exited.}
    property ThreadsStopEvent: THandle read fThreadsStoppedEvent;

    {ServiceStopEvent is signaled when this service must shut down.}
    property ServiceStopEvent: THandle read fServiceStopEvent;

    //If set to true it shuts down the server
    property Stopped: boolean read fStopped write SetStopped;
  end;


var
    XPService: TXPService;


implementation
uses
  HandleRequestThread, //elevetion Thread unit
  ElevationHandler; //
{$R *.DFM}


procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  XPService.Controller(CtrlCode);
end;

destructor TXPService.Destroy;
begin
  FreeAndNil(fStopCriticalSection);
  inherited;
end;

{$IFDEF EUREKALOG}
procedure TXPService.EurekaLog1ExceptionActionNotify(
  EurekaExceptionRecord: TEurekaExceptionRecord;
  EurekaAction: TEurekaActionType; var Execute: Boolean);
begin
  if EurekaAction = atShowingExceptionInfo then
  begin
{$IFDEF DEBUG}
    Execute := True;
{$ELSE}
    Execute := false;
{$ENDIF DEBUG}
  end;
end;
{$ENDIF EUREKALOG}

function TXPService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;






procedure TXPService.OnJobNotification(Sender: TJwJobObject;
  ProcessId: TJwProcessId; JobLimits: TJwJobMessages; Data : Pointer);

var
  P : PProcessJobData;
  Log : IJwLogClient;
begin
  Log := uLogging.LogServer.Connect(
      etMethod,ClassName,'OnJobNotification','MainUnitpas',
          JwFormatString('Received Job notifiction on ProcessID %d in session %d',
            [ProcessId, Sender.Session]));

  try
    if (jmsgEXITPROCESS in JobLimits) or
       (jmsgABNORMALEXITPROCESS in JobLimits) then
    begin
      Sender.Lock.BeginWrite;
      try
        P := Sender.DataList[ProcessId];
        if (P <> nil) and Assigned(P^.UserToken) then
        begin
          Log.Log('Unloading profile.');
          P^.UserToken.UnLoadUserProfile(P^.UserProfile);
          Dispose(P);
          try
            Sender.DataList.DeleteIndex(ProcessId);
          except
          end;
       end;
      finally
        Sender.Lock.EndWrite;
      end;
    end;
  except
    on E : Exception do
      Log.Exception(E);
  end;
end;

procedure TXPService.OnNewJobObject(Sender: TJwJobObjectSessionList;
  ProcessHandle: TJwProcessHandle; ProcessSessionID, CurrentSessionID: Cardinal;
  var NewJobObject: TJwJobObject);
var Name : TJwString;
begin
  Name := JwFormatString('XPElevationJobObject%d.%d.%d(%d)',
    [CurrentSessionID, ProcessSessionID, GetProcessId(ProcessHandle), ProcessHandle]);

  NewJobObject                     := TJwJobObject.Create(Name,true, nil);
  NewJobObject.OnNotification      := OnJobNotification;
  NewJobObject.OnNoActiveProcesses := OnNoActiveProcesses;
  NewJobObject.Session             := ProcessSessionID;
end;

procedure TXPService.OnNoActiveProcesses(Sender: TJwJobObject);
begin

end;

constructor TXPService.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TXPService.ServiceCreate(Sender: TObject);

begin
  fStopCriticalSection := TMultiReadExclusiveWriteSynchronizer.Create;
end;

procedure TXPService.ServiceExecute(Sender: TService);

function CreateServicePipe(Log : IJwLogClient) : THandle;
var
  SD : TJwSecurityDescriptor;
  pSA : JwaWindows.PSECURITY_ATTRIBUTES;
begin
  SD := TJwSecurityDescriptor.Create;
  try
    SD.Owner := JwAdministratorsSID;
    SD.PrimaryGroup := JwAdministratorsSID;

{$IFDEF DEBUG}
    SD.DACL := nil;
{$ELSE}
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwLocalSystemSID,false));
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL or WRITE_DAC or WRITE_OWNER,JwAdministratorsSID,false));
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwWorldSID,false));
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],GENERIC_ALL,JwNetworkServiceSID,false));
{$ENDIF}
    pSA := SD.Create_SA();

    try
      result := CreateNamedPipe('\\.\pipe\XPElevationPipe',
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


function RegGetFullPath(PathKey: string): string;
var Reg: TRegistry; Unresolved: string;
begin
  Reg:=TRegistry.Create(KEY_QUERY_VALUE);
  try
    Reg.RootKey:=HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('Software\XPElevation\Paths\', false) then
    try
      Unresolved:=Reg.ReadString(PathKey);
      SetLength(Result, MAX_PATH+1);
      ExpandEnvironmentStrings(PChar(Unresolved), @Result[1], MAX_PATH+1);
      SetLength(Result, StrLen(PChar(Result)));
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;


{
 HKEY_LOCAL_MACHINE;
 KEY_CREDENTIALS_HASH_SIZE)
        and Reg.ValueExists(KEY_CREDENTIALS_HASH_

}





procedure CreateCredentialsBinaryHash;
var
  Stream : TJwFileStreamEx;
  M : TMemoryStream;
  Size : Cardinal;
  fCredentialsAppHash : TJwHash;
  Log : IJwLogClient;
  Reg: TRegistry;

  StoredHash : TJwFileHashData;
  HashCheck : boolean;

const
  KEY_CREDENTIALS_HASH_SIZE = 'CredentialsHashSize';
  KEY_CREDENTIALS_HASH_ = 'CredentialsHash';

  KEY_OSK_HASH_SIZE = 'OskHashSize';
  KEY_OSK_HASH_ = 'OskHash';

  KEY_MAGNIFY_HASH_SIZE = 'MagnifyHashSize';
  KEY_MAGNIFY_HASH_ = 'MagnifyHash';



begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'CreateCredentialsBinaryHash','MainUnit.pas','');

  {Hash := nil;
  try
    Reg := TRegistry.Create(KEY_ALL_ACCESS);// KEY_QUERY_VALUE or KEY_READ);
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKey(XPElevationRegKey, false)
        and Reg.ValueExists(KEY_CREDENTIALS_HASH_SIZE)
        and Reg.ValueExists(KEY_CREDENTIALS_HASH_)
        then
      try
        HashSize := Reg.ReadInteger(KEY_CREDENTIALS_HASH_SIZE);
        if (HashSize > 0) and (HashSize < 1024) then
        begin
          GetMem(Hash, HashSize+2);
          ZeroMemory(Hash, HashSize+2);
          HashSize := Reg.ReadBinaryData(KEY_CREDENTIALS_HASH_,Hash^,HashSize);
        end;    
      finally
        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;
  except
  end;    }
  try
    StoredHash := JwLoadHashFromRegistry(HKEY_LOCAL_MACHINE,
      XPElevationRegKey, KEY_CREDENTIALS_HASH_, KEY_CREDENTIALS_HASH_SIZE);
    TJwAutoPointer.Wrap(StoredHash.Hash, StoredHash.Size, ptGetMem);
  except
    ZeroMemory(@StoredHash, sizeof(StoredHash));
  end;

  CredentialsHash := JwCreateFileHash(CredentialsAppPath);

  if (StoredHash.Hash <> nil) and (StoredHash.Size > 0) then
  begin
    HashCheck := (CredentialsHash.Size = StoredHash.Size) and
        (StoredHash.Hash <> nil) and (CredentialsHash.Hash <> nil) and
        (CompareMem(CredentialsHash.Hash,StoredHash.Hash, CredentialsHash.Size));
  end
  else
  begin
    HashCheck := true;

    try
      JwSaveHashToRegistry(HKEY_LOCAL_MACHINE,
         XPElevationRegKey, KEY_CREDENTIALS_HASH_, KEY_CREDENTIALS_HASH_SIZE,
         StoredHash);

     { Reg:=TRegistry.Create(KEY_SET_VALUE or KEY_CREATE_SUB_KEY);
      try
        Reg.RootKey:=HKEY_LOCAL_MACHINE;
        if Reg.OpenKey(XPElevationRegKey, true) then
        try
          Reg.WriteInteger(KEY_CREDENTIALS_HASH_SIZE, CredentialsHash.Size);
          Reg.WriteBinaryData(KEY_CREDENTIALS_HASH_,CredentialsHash.Hash^,CredentialsHash.Size);
        finally
          Reg.CloseKey;
        end;
      finally
        Reg.Free;
      end;  }
    except
    end;
  end;

{$IFNDEF DEBUG}
  if not HashCheck then
  begin
    raise EHashMismatch.Create('The hash of the credentials application is not the same as the one stored. Exiting..');
  end;
{$ENDIF DEBUG}
end;

procedure AddXPElevationGroup;
var
  Log : IJwLogClient;
  p : TLocalGroupInfo1;
  res, err : DWORD;
  Sid : TJwSecurityId;
begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'AddXPElevationGroup','MainUnit.pas','');

  try
    Sid := TJwSecurityId.Create('','XPElevationUser');
    Sid.Free;
  except
    p.lgrpi1_name := 'XPElevationUser';
    p.lgrpi1_comment := 'JEDI XPElevation User. The members of this group is '+
      'allowed to be elevated as Administrator even '+
      'if they are not in the Administrators group.';

    res := NetLocalGroupAdd(
        nil,//__in   LPCWSTR servername,
        1,//__in   DWORD level,
        @p,//__in   LPBYTE buf,
        @err//__out  LPDWORD parm_err
        );
  end;

end;

var Pipe: THandle;

    OvLapped,
    OvLapped2: OVERLAPPED;

    
    AppName: String; PipeSize: Cardinal;
    i: integer;
    Log : IJwLogClient;
    WaitResult : DWORD;
    Msg : TMsg;

    UniquePipeID : DWORD;

    PipeNameSize : DWORD;
    PipeToken : TJwSecurityToken;
    PipeName : WideString;
    ProtocolVersion : DWORD;


begin
  JwSetThreadName('XP Elevation Service Thread');
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'ServiceExecute','MainUnit.pas','');

  AddXPElevationGroup;

  CredentialsHash.Hash := nil;
  try
    UniquePipeID := 10001;

    CredentialsAppPath := RegGetFullPath(CredApplicationKey);

    if not FileExists(CredentialsAppPath) then
    begin
      Log.Log(lsError,'Credentials app not found: '+CredentialsAppPath);
      exit;
    end;

    //uses CredentialsAppPath
    try
      //loads the stored hash of last time and compares
      //it to the newly generated hash
      //if not equal it raises EHashMismatch
      CreateCredentialsBinaryHash;
    except
      on e : Exception do
      begin
        Log.Exception(E);
        exit;
      end;
    end;


    if (CredentialsHash.Hash = nil) or (CredentialsHash.Size = 0) then
    begin
      Log.Log(lsError,'Could not get hash from credentials prompt file');
  {$IFNDEF DEBUG}
      exit;
  {$ENDIF DEBUG}
    end;

    try //except
      try
        fThreadsStoppedEvent   := CreateEvent(nil, true, false, nil);
        fServiceStopEvent   := CreateEvent(nil, true, false, nil);
        Sleep(1000);


        ZeroMemory(@OvLapped, sizeof(OvLapped));
        OvLapped.hEvent := CreateEvent(nil, false, false, nil);

          fPasswords := TCredentialsList.Create;
          {fPasswords.LockList.Count := fAllowedSIDs.Count;
          fPasswords.UnlockList;}

          try
            //create job objects for all sessions
            fJobs := TJwJobObjectSessionList.Create(OnNewJobObject);

            Pipe := CreateServicePipe(Log);
            repeat
              ConnectNamedPipe(Pipe, @OvLapped);

              repeat
                if Assigned(ServiceThread) then
                  ServiceThread.ProcessRequests(False);


                if (TJwWindowsVersion.IsWindowsXP(true) or
                    TJwWindowsVersion.IsWindows2003(true)) and
                      (GetSystemMetrics(SM_SHUTTINGDOWN) <> 0) then
                    Stopped := true;

                SetLastError(0);
                WaitResult := JwMsgWaitForMultipleObjects([fServiceStopEvent, OvLapped.hEvent], false, INFINITE, QS_ALLINPUT);

                case WaitResult of
                  WAIT_OBJECT_0 + 1 :  ResetEvent(OvLapped.hEvent);
                  WAIT_OBJECT_0 + 2 :  PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE); //tag message as read
                else
                  OutputDebugString(PChar(IntToStr(GetLastError)));
                end;

              until WaitResult <> WAIT_OBJECT_0 + 2; //


              if WaitResult = WAIT_OBJECT_0 +1 then  //OvLapped.hEvent
              begin
                Inc(UniquePipeID);

                ZeroMemory(@OvLapped2, sizeof(OvLapped2));
                OvLapped2.hEvent := CreateEvent(nil,false,false,nil);
                TJwAutoPointer.Wrap(OvLapped2.hEvent);

                {We use this pipe only a very short time so it is very unlikely
                that several clients are going to connect at the same time.
                }
                PipeName := THandleRequestThread.CreatePipeName(UniquePipeID);

                //Sleep(2000);

                ReadFile(Pipe, @ProtocolVersion, sizeof(ProtocolVersion) ,nil, @OvLapped2);

                if JwWaitForMultipleObjects([fServiceStopEvent, OvLapped2.hEvent], false,
                      {$IFNDEF DEBUG}1000{$ELSE}INFINITE{$ENDIF}) = WAIT_OBJECT_0 +1 then
                begin
                  try //on E : Exception do
                    //try to impersonate. May fail, so we don't do anything!
                    //and this will lead to next on E : Exception
                    TJwSecurityToken.ImpersonateNamedPipeClient(Pipe);

                    //if sth happens here, we also fail to exception trap
                    try
                      PipeToken := TJwSecurityToken.CreateTokenByThread(0, MAXIMUM_ALLOWED, true);
                    finally
                      TJwSecurityToken.RevertToSelf;
                    end;


                    PipeNameSize := Length(PipeName);
                    WriteFile(Pipe, @PipeNameSize, sizeof(PipeNameSize) ,nil, @OvLapped2);

                    SetLastError(0);
                    if CheckPipe(WriteFile(Pipe, @PipeName[1], Length(PipeName)*sizeof(WideChar),nil,@OvLapped2)) then
                    begin

                      with THandleRequestThread.Create(
                        true, //Create suspended
                        UniquePipeID,
                        PipeToken,
                        fJobs,
                        nil,//const AllowedSIDs:  TJwSecurityIdList;
                        fPasswords,//const Passwords   : TPasswordList;
                        fServiceStopEvent,//const StopEvent  : THandle;
                        fThreadsStoppedEvent,
                        nil,//ServiceThread.ProcessRequests,//const OnServiceProcessRequest : TOnServiceProcessRequest;

                        @fStopped //const StopState : PBoolean
                        ) do
                      begin
                        FreeOnTerminate := True;
                        Resume;
                      end;
                    end
                    else
                      Log.Log('Could not send Pipename to client: '+SysErrorMessage(GetLastError));
                  except
                    on E : Exception do
                    begin
                      Log.Exception(E);
                      Log.Log(E.Message);
                    end;
                  end;
                end;
              end
              else
                Log.Log('Read for protocl version failed.');


              Sleep(5000);
              DisconnectNamedPipe(Pipe);

            until Stopped;

          finally
            CloseHandle(Pipe);

            //signal server shutdown
            Stopped := true;

            {Wait for all threads to be stopped or timeout
            }
            WaitForSingleObject(ThreadsStopEvent, 30 * 1000);
          
            fPasswords.Free;
            FreeAndNil(fJobs);
          end;
        finally
          CloseHandle(OvLapped.hEvent);
        end;
      except
        on E : Exception do
          Log.Exception(E);
      end;

      Log.Log(lsStop,'*** XP Elevation Service finished. ');
  finally
    TJwHash.FreeBuffer(CredentialsHash.Hash);
    ZeroMemory(@CredentialsHash, sizeof(CredentialsHash));
  end;

end;

procedure TXPService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  Self.Stopped:=True;

end;

procedure TXPService.SetStopped(const Value: boolean);
var    Log : IJwLogClient;
begin
  Log := uLogging.LogServer.Connect(etNone,ClassName,
            'SetStopped','MainUnit.pas','');


  fStopCriticalSection.BeginWrite;
  try
    FStopped := Value;

    if not Value then
    begin
      Log.Log('Stopevent resetted....');
      ResetEvent(fServiceStopEvent);
    end
    else
    begin
      Log.Log('Stopevent executed....');
      SetEvent(fServiceStopEvent);
    end;
  finally
    fStopCriticalSection.EndWrite;
  end;
end;

procedure TXPService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  Started := true;
  fStopCriticalSection := TMultiReadExclusiveWriteSynchronizer.Create;
end;




procedure TXPService.ServiceShutdown(Sender: TService);
begin
  SetStopped(true);
end;

end.
