unit MainUnit;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs, Math, ComObj,
  JwaWindows, JwsclToken, JwsclLsa, JwsclCredentials, JwsclDescriptor, JwsclDesktops,
  JwsclExceptions, JwsclSID, JwsclAcl,JwsclKnownSID, JwsclEncryption, JwsclTypes,
  JwsclProcess,
  JwsclLogging, uLogging,JwsclUtils,
  SessionPipe, ThreadedPasswords,


  JwsclStrings, ExceptionLog;

type
  TLogType=(ltInfo, ltError);

type
  TXPService = class(TService)
    EurekaLog1: TEurekaLog;
    procedure ServiceExecute(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceCreate(Sender: TObject);
    procedure EurekaLog1ExceptionActionNotify(
      EurekaExceptionRecord: TEurekaExceptionRecord;
      EurekaAction: TEurekaActionType; var Execute: Boolean);
    procedure EurekaLog1HandledExceptionNotify(
      EurekaExceptionRecord: TEurekaExceptionRecord; var Handled: Boolean);
  private
    { Private declarations }
    fServiceStopEvent,
    fThreadsStoppedEvent:           THandle;
    fJobs : TJwJobObjectSessionList;

    fStopCriticalSection :  TMultiReadExclusiveWriteSynchronizer;
    //fLogFile:             Textfile;
    fStopped:             boolean;
    fTimer    : HANDLE;
//    fDesktop:             TJwSecurityDesktop;
    fAllowedSIDs:         TJwSecurityIdList;
    fPasswords:           TPasswordList;


    procedure InitAllowedSIDs;
    procedure SetStopped(const Value: boolean);
    function MayUserBeElevated(User: TJwSecurityID): boolean;

    procedure OnJobNotification(Sender : TJwJobObject; ProcessId : TJwProcessId; JobLimits : TJwJobMessages; Data : Pointer);
    procedure OnNoActiveProcesses(Sender : TJwJobObject);
    procedure OnNewJobObject(Sender : TJwJobObjectSessionList;    ProcessHandle : TJwProcessHandle;
      ProcessSessionID,
      CurrentSessionID : Cardinal;
      var NewJobObject : TJwJobObject);


  public
    fHReqThreadCount:     Integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


    function GetServiceController: TServiceController; override;
    procedure StartApp(AppToStart: String);
    { Public declarations }
    property ThreadsStopEvent: THandle read fThreadsStoppedEvent;
    property ServiceStopEvent: THandle read fServiceStopEvent;

    property Stopped: boolean read fStopped write SetStopped;
  end;

const MessageboxCaption= 'XP Elevation';

      

var
    XPService: TXPService;


implementation
uses HandleRequestThread, Registry, ElevationHandler;
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

procedure TXPService.EurekaLog1HandledExceptionNotify(
  EurekaExceptionRecord: TEurekaExceptionRecord; var Handled: Boolean);
begin
  //
end;

function TXPService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;



function TXPService.MayUserBeElevated(User: TJwSecurityID): boolean;
begin
  try
    result := fAllowedSIDs.FindSid(User)<>-1;
  except
    result := false;
  end;
end;

const EmptyPass = Pointer(-1);

procedure TXPService.StartApp(AppToStart: string);
begin
end;

procedure TXPService.InitAllowedSIDs;
var Reg: TRegistry; SIDStrings: TStringlist; i: integer;
begin
  fAllowedSIDs:=TJwSecurityIDList.Create(True);
  Reg:=TRegistry.Create(KEY_QUERY_VALUE);
  try
    Reg.RootKey:=HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('Software\XPElevation\AllowedUsers\', false) then
    try
      SIDStrings:=TStringlist.Create;
      try
        Reg.GetValueNames(SIDStrings);
        for i:=0 to SIDStrings.Count-1 do
        try
          fAllowedSIDs.Add(TJwSecurityId.Create(SIDStrings[i]));
        except
        end;
      finally
        SIDStrings.Free;
      end;
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  fPasswords := TPasswordList.Create;
  fPasswords.LockList.Count := fAllowedSIDs.Count;
  fPasswords.UnlockList;
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
var Pipe: THandle; OvLapped: OVERLAPPED;

    
    AppName: String; PipeSize: Cardinal; Descr: TJwSecurityDescriptor;
    SecAttr: JwaWindows.PSECURITY_ATTRIBUTES;
    i: integer;
    Log : IJwLogClient;
    WaitResult : DWORD;

begin

  JwSetThreadName('XP Elevation Service Thread');
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'ServiceExecute','MainUnit.pas','');


  
  try
    try
      fThreadsStoppedEvent   := CreateEvent(nil, true, false, nil);
      fServiceStopEvent   := CreateEvent(nil, true, false, nil);
      Sleep(1000);


      SecAttr := nil;
      ZeroMemory(@OvLapped, sizeof(OvLapped));
      OvLapped.hEvent := CreateEvent(nil, false, false, nil);
      try
        Descr:=TJwSecurityDescriptor.Create;
        try
          Log.Log('Create Pipe 1');
          Descr.Owner := JwSecurityProcessUserSID;
          Descr.PrimaryGroup := JwAdministratorsSID;

{$IFDEF DEBUG}
          Descr.DACL:=nil;
{$ELSE}
          Descr.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwLocalSystemSID,false));
          Descr.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwAdministratorsSID,false));
          //Descr.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwWorldSID,false));
{$ENDIF}
          SecAttr:=Descr.Create_SA(false);

          Pipe := CreateNamedPipe('\\.\pipe\XPElevationPipe', PIPE_ACCESS_INBOUND or FILE_FLAG_OVERLAPPED, PIPE_WAIT, PIPE_UNLIMITED_INSTANCES, 0, 0, 0, LPSECURITY_ATTRIBUTES(SecAttr));
        finally
          Descr.Free;
        end;
        if Pipe = INVALID_HANDLE_VALUE then
        begin
          LogAndRaiseLastOsError(Log,ClassName, 'ServiceExecute::(winapi)CreateNamedPipe OUT', 'ElevationHandler.pas');
          abort;
        end;

        InitAllowedSIDs;
        try
          fJobs := TJwJobObjectSessionList.Create(OnNewJobObject);
          
          repeat
            ConnectNamedPipe(Pipe, @OvLapped);

            repeat
              if Assigned(ServiceThread) then
                ServiceThread.ProcessRequests(False);
              WaitResult := JwMsgWaitForMultipleObjects([fServiceStopEvent, OvLapped.hEvent], false, INFINITE, QS_ALLINPUT);
            until WaitResult <> WAIT_OBJECT_0 + 2; //any event we declared
            ResetEvent(OvLapped.hEvent);

            if WaitResult = WAIT_OBJECT_0 +1 then
            begin
              with THandleRequestThread.Create(
                true, //Create suspended
                fJobs,
                fAllowedSIDs,//const AllowedSIDs:  TJwSecurityIdList;
                fPasswords,//const Passwords   : TPasswordList;
                fServiceStopEvent,//const StopEvent  : THandle;
                fThreadsStoppedEvent,
                nil,//ServiceThread.ProcessRequests,//const OnServiceProcessRequest : TOnServiceProcessRequest;

                @fStopped //const StopState : PBoolean
                ) do
              begin
                FreeOnTerminate := True;
                PipeHandle := Pipe;
                Resume;
              end;

              Pipe := CreateNamedPipe('\\.\pipe\XPElevationPipe', PIPE_ACCESS_INBOUND or FILE_FLAG_OVERLAPPED, PIPE_WAIT, PIPE_UNLIMITED_INSTANCES, 0, 0, 0, LPSECURITY_ATTRIBUTES(SecAttr));
              if Pipe = INVALID_HANDLE_VALUE then
              begin
                LogAndRaiseLastOsError(Log,ClassName, 'ServiceExecute::(winapi)CreateNamedPipe IN', 'ElevationHandler.pas');
              end;
            end;
          until Stopped;

        finally
          CloseHandle(Pipe);

          //signal server shutdown
          Stopped := true;
            
          {Wait for all threads to be stopped or timeout
          }
          WaitForSingleObject(ThreadsStopEvent, 60 * 1000);
          fAllowedSIDs.Free;

          fPasswords.Free;
          FreeAndNil(fJobs);
        end;
      finally
        if Assigned(SecAttr) then
        begin
          TJwSecurityDescriptor.Free_SA(SecAttr);
        end;
        CloseHandle(OvLapped.hEvent);
      end;
    except
      on E : Exception do
        Log.Exception(E);
    end;
  finally
    Log.Log(lsStop,'*** XP Elevation Service finished. ');
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
