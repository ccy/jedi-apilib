unit WinLogonLogoServiceUnit;

interface

uses
  JwaWindows, Messages, SysUtils, ActiveX, Classes, Graphics, Controls, SvcMgr, Dialogs,
  ComObj, ComServ, WinlogonLogoServiceProxy, WinLogonLogoService_TLB, SyncObjs,
  EventSystemLib_TLB, JwsclUtils, JwsclToken, JwsclTypes, JwsclProcess, JwsclComUtils,
  JwsclTerminalServer, JwsclVersion,
  SensEvents_TLB, StdVcl, JwsclStrings, ExtCtrls;

type
  TSENSTestService = class(TService, ISensLogon2)
    Timer1: TTimer;
    procedure ServiceExecute(Sender: TService);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServicePause(Sender: TService; var Paused: Boolean);
  private
    fStopEvent : TEvent;
    fJob : TJwJobObjectSessionList;
    fIsStopping : Boolean;

  protected
     procedure OnProcessFound(const Sender: TJwTerminalServer;
       var Process: TJwWTSProcess; var Cancel: Boolean; Data: Pointer);
     procedure OnNewJobObject(Sender : TJwJobObjectSessionList;
        ProcessHandle : TJwProcessHandle;
        ProcessSessionID,
        CurrentSessionID : Cardinal;
        var NewJobObject : TJwJobObject);
     procedure OnJobNotification(Sender : TJwJobObject; ProcessId : TJwProcessId;
        JobMessages : TJwJobMessages; Data : Pointer);
  public
    { Private-Deklarationen }
    procedure Logon(const bstrUserName: WideString; dwSessionId: LongWord); safecall;
    procedure Logoff(const bstrUserName: WideString; dwSessionId: LongWord); safecall;
    procedure SessionDisconnect(const bstrUserName: WideString; dwSessionId: LongWord); safecall;
    procedure SessionReconnect(const bstrUserName: WideString; dwSessionId: LongWord); safecall;
    procedure PostShell(const bstrUserName: WideString; dwSessionId: LongWord); safecall;

    procedure RunAppIntoSession(const Session : DWORD);
  public
    function GetServiceController: TServiceController; override;
    { Public-Deklarationen }
  end;

var
  SENSTestService: TSENSTestService;

implementation

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  SENSTestService.Controller(CtrlCode);
end;



function TSENSTestService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TSENSTestService.OnJobNotification(Sender: TJwJobObject;
  ProcessId: TJwProcessId; JobMessages: TJwJobMessages; Data: Pointer);
begin
  if (jmsgEXITPROCESS in JobMessages) and not fIsStopping then
  begin
    RunAppIntoSession(Sender.Session);
  end;
end;

procedure TSENSTestService.OnNewJobObject(Sender: TJwJobObjectSessionList;
  ProcessHandle: TJwProcessHandle; ProcessSessionID, CurrentSessionID: Cardinal;
  var NewJobObject: TJwJobObject);
var Name : TJwString;
begin
  Name := JwFormatString('XPElevationJobObject%d.%d.%d(%d)',
    [CurrentSessionID, ProcessSessionID, GetProcessId(ProcessHandle), ProcessHandle]);

  NewJobObject                     := TJwJobObject.Create(Name,true, nil);
  NewJobObject.OnNotification      := OnJobNotification;
  NewJobObject.OnNoActiveProcesses := nil;
  NewJobObject.Session             := ProcessSessionID;
end;

procedure TSENSTestService.Logoff(const bstrUserName: WideString;
  dwSessionId: LongWord);
begin
  LogMessage(Format('Benutzer: %s hat sich in Session %d ausgeloggt.',[bstrUserName, dwSessionId]));


{ Prozess beenden?
  try
    fJob.JobObject[dwSessionId].TerminateJobObject(0);
  except
  end;}
end;

procedure TSENSTestService.Logon(const bstrUserName: WideString;
  dwSessionId: LongWord);
begin
  LogMessage(Format('Benutzer: %s hat sich in Session %d eingeloggt.',[bstrUserName, dwSessionId]));
  RunAppIntoSession(dwSessionId);
end;



procedure TSENSTestService.PostShell(const bstrUserName: WideString;
  dwSessionId: LongWord);
begin

end;

procedure TSENSTestService.OnProcessFound(const Sender: TJwTerminalServer;
  var Process: TJwWTSProcess; var Cancel: Boolean; Data: Pointer);
begin
  Cancel := (Process.SessionId = DWORD(Data)) and
     (JwCompareString(Process.ProcessName,'winlogon.exe', true) = 0);
end;

procedure TSENSTestService.RunAppIntoSession(const Session: DWORD);
const AppName : WideString = 'C:\Windows\System32\cmd.exe';

var
  Token : TJwSecurityToken;
  StartupInfo : TStartupInfoW;
  ProcessInfo : TProcessInformation;
begin
  Token := JwGetTokenFromProcess (OnProcessFound, nil, Pointer(Session));
  TJwAutoPointer.Wrap(Token);

  ZeroMemory(@StartupInfo, sizeof(StartupInfo));
  StartupInfo.cb := sizeof(StartupInfo);
  StartupInfo.lpDesktop := 'winsta0\winlogon';

  ZeroMemory(@ProcessInfo, sizeof(ProcessInfo));

  if not CreateProcessAsUserW(
        Token.TokenHandle,//HANDLE hToken,
        PWideChar(AppName),//__in_opt     LPCTSTR lpApplicationName,
        PWideChar(AppName), //__inout_opt  LPTSTR lpCommandLine,
        nil,//__in_opt     LPSECURITY_ATTRIBUTES lpProcessAttributes,
        nil,//__in_opt     LPSECURITY_ATTRIBUTES lpThreadAttributes,
        false,//__in         BOOL bInheritHandles,
        0,//__in         DWORD dwCreationFlags,
        nil,//__in_opt     LPVOID lpEnvironment,
        nil,//__in_opt     LPCTSTR lpCurrentDirectory,
        StartupInfo,//__in         LPSTARTUPINFO lpStartupInfo,
        ProcessInfo //__out        LPPROCESS_INFORMATION lpProcessInformation
      ) then
  begin
    LogMessage(Format('CreateProcess failed with %d',[GetLastError]));
  end
  else
  begin
    fJob.AssignProcessToJob(ProcessInfo.hProcess, nil);
    LogMessage('CreateProcess returned success');
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end;

end;

procedure TSENSTestService.SessionDisconnect(const bstrUserName: WideString;
  dwSessionId: LongWord);
begin

end;

procedure TSENSTestService.SessionReconnect(const bstrUserName: WideString;
  dwSessionId: LongWord);
begin

end;


procedure TSENSTestService.Timer1Timer(Sender: TObject);
begin
ServiceShutdown(nil);
end;

procedure TSENSTestService.ServiceCreate(Sender: TObject);
begin
//
end;

procedure TSENSTestService.ServiceDestroy(Sender: TObject);
begin
  FreeAndNil(fJob);
  FreeAndNil(fStopEvent);

  CoUninitialize;
end;

procedure TSENSTestService.ServiceExecute(Sender: TService);

const SubscriptionID = '{C40B2659-B57F-4DD3-A223-D777B4A8F4CC}';
var
  Subscription : IEventSubscription;
  EventSystem  : IEventSystem;
  LogonSub : ISensLogon2;
  Msg : TMsg;
  WRes : DWORD;
  bIsRunning : Boolean;
  iErrorIndex : Integer;
begin
  try
    if TJwWindowsVersion.IsWindowsVista(true) or
      TJwWindowsVersion.IsWindows2008(true) then
      RunAppIntoSession(1)
    else
      RunAppIntoSession(0);

    LogonSub := CoSENSLogonProxy.Create;

    Subscription := CoCEventSubscription.Create;
    Subscription.SubscriptionID := SubscriptionID;
    Subscription.SubscriptionName := 'WinlogonLogo Client';
    Subscription.EventClassID := GUIDToString(SENSGUID_EVENTCLASS_LOGON2);
    Subscription.SubscriberInterface := LogonSub;

    CoCreateInstance(
      ProgIdToClassId('EventSystem.EventSystem'), nil, CLSCTX_SERVER,
      IID_IEventSystem, EventSystem);
    EventSystem.Store('EventSystem.EventSubscription',Subscription);


    try
      bIsRunning := true;
      while bIsRunning do
      begin
        SetLastError(0);
        wRes := JwMsgWaitForMultipleObjects([fStopEvent.Handle], false, INFINITE, QS_ALLINPUT);

        case wRes of
          WAIT_OBJECT_0 : bIsRunning := false;
        else  //one object behind the last wait object in [ and ]
        begin
          if Assigned(ServiceThread) then
            ServiceThread.ProcessRequests(False)
          else
          begin
            GetMessage(@Msg,0,0,0);
            TranslateMessage(@Msg);
            DispatchMessage(@Msg);
          end;
        end;
        end;
      end;
    finally
      CoCreateInstance(
        ProgIdToClassId('EventSystem.EventSystem'), nil,
        CLSCTX_SERVER, IID_IEventSystem, EventSystem);

      EventSystem.Remove('EventSystem.EventSubscription',
        'SubscriptionID == ' + SubscriptionID, iErrorIndex);
      if Error <> 0 then
        LogMessage(Format('Unsubscribe returned %d.',[iErrorIndex]));

      fJob.Clear(jtAll);
    end;
  except
    on E : Exception do
      LogMessage('WinLogonLogoService raised an exception: '+E.Message);

  end;



end;

procedure TSENSTestService.ServicePause(Sender: TService; var Paused: Boolean);
begin
  Paused := false;
end;

procedure TSENSTestService.ServiceShutdown(Sender: TService);
begin
  fIsStopping := true;
  fStopEvent.SetEvent;
end;

procedure TSENSTestService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  CoInitialize(nil);

  fIsStopping := false;
  fStopEvent := TEvent.Create;
  fJob := TJwJobObjectSessionList.Create(OnNewJobObject);
end;

procedure TSENSTestService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  fIsStopping := true;
  fStopEvent.SetEvent;
end;



end.
