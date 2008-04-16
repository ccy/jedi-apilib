unit MainUnit;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs, Math, ComObj,
  JwaWindows, JwsclToken, JwsclLsa, JwsclCredentials, JwsclDescriptor, JwsclDesktops,
  JwsclExceptions, JwsclSID, JwsclAcl,JwsclKnownSID, JwsclEncryption, JwsclTypes,
  JwsclProcess,
  JwsclLogging, uLogging,JwsclUtils,
  SessionPipe, ThreadedPasswords,


  JwsclStrings;

type
  TLogType=(ltInfo, ltError);

type {PClientBuffer = ^TClientBuffer;
     TClientBuffer = record
       Signature : array[0..15] of Char;
       UserName  :  array[0..UNLEN] of WideChar;
       Domain      : array[0..MAX_DOMAIN_NAME_LEN] of WideChar;
       Password  : array[0..MAX_PASSWD_LEN] of WideChar;
       Flags     : DWORD;
     end;

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
       UserName,
       Domain,
       Password  : WideString;
       Flags  : DWORD;
     end; }




  TXPService = class(TService)
    procedure ServiceExecute(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceShutdown(Sender: TService);
  private
    { Private declarations }
    fServiceStopEvent,
    fThreadsStopEvent:           THandle;

    fLogCriticalSection:  TCriticalSection;
    //fLogFile:             Textfile;
    fStopped:             boolean;
    fTimer    : HANDLE;
//    fDesktop:             TJwSecurityDesktop;
    fAllowedSIDs:         TJwSecurityIdList;
    fPasswords:           TPasswordList;


    procedure InitAllowedSIDs;
    procedure SetStopped(const Value: boolean);
    function MayUserBeElevated(User: TJwSecurityID): boolean;
    function AskCredentials(Token: TJwSecurityToken;
        AppToStart: String;
        var SessionInfo : TSessionInfo): boolean;
  public
    fHReqThreadCount:     Integer;
    function GetServiceController: TServiceController; override;
    procedure StartApp(AppToStart: String);
    procedure LogEvent(Event: String; EventType: TLogType=ltInfo);
    { Public declarations }
    property ThreadsStopEvent: THandle read fThreadsStopEvent;
    property ServiceStopEvent: THandle read fServiceStopEvent;

    property Stopped: boolean read fStopped write SetStopped;
  end;

const MessageboxCaption= 'XP Elevation';

      

var
    XPService: TXPService;


implementation
uses ThreadUnit, HandleRequestThread, Registry;
{$R *.DFM}


procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  XPService.Controller(CtrlCode);
end;

function TXPService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TXPService.LogEvent(Event: String; EventType: TLogType=ltInfo);
begin
{  EnterCriticalSection(fLogCriticalSection);
  try
    case EventType of
      ltInfo: Writeln(fLogfile, DateTimeToStr(Now)+' '+Event);
      ltError: Writeln(fLogfile, DateTimeToStr(Now), ' ERROR: ', Event);
    end;
    Flush(fLogfile);
  finally
    LeaveCriticalSection(fLogCriticalSection);
  end;}
end;



function TXPService.AskCredentials(Token: TJwSecurityToken; AppToStart: string;
  var SessionInfo : TSessionInfo): boolean;
begin
end;


function TXPService.MayUserBeElevated(User: TJwSecurityID): boolean;
begin
  result:=fAllowedSIDs.FindSid(User)<>-1;
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

procedure TXPService.ServiceExecute(Sender: TService);
var Pipe: THandle; OvLapped: OVERLAPPED;
    ar: array[0..2] of THandle;
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
//    Descr:=TJwSecurityDescriptor.Create;
//    try
//      fDesktop:=TJwSecurityDesktop.CreateDesktop(nil, true, 'UACinXPAskCredentialsDesktop', [], false, GENERIC_ALL, Descr);
//    finally
//      Descr.Free;
//    end;
    fThreadsStopEvent   := CreateEvent(nil, true, false, nil);
    fServiceStopEvent  := CreateEvent(nil, true, true, nil);
    Sleep(1000);

    try
      try
        SecAttr := nil;
        ZeroMemory(@OvLapped, sizeof(OvLapped));
        OvLapped.hEvent := CreateEvent(nil, false, false, nil);
        try
          Descr:=TJwSecurityDescriptor.Create;
          try
            //LogEvent('Create Pipe 1');
            Log.Log('Create Pipe 1');
            Descr.Owner := JwSecurityProcessUserSID;
            Descr.PrimaryGroup := JwAdministratorsSID;
            //Descr.DACL:=nil;
            Descr.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwLocalSystemSID,false));
            Descr.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwAdministratorsSID,false));
            Descr.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwWorldSID,false));
            SecAttr:=Descr.Create_SA(false);
            //SecAttr:= nil;
            Pipe:=CreateNamedPipe('\\.\pipe\XPElevationPipe', PIPE_ACCESS_INBOUND or FILE_FLAG_OVERLAPPED, PIPE_WAIT, PIPE_UNLIMITED_INSTANCES, 0, 0, 0, LPSECURITY_ATTRIBUTES(SecAttr));
          finally
            Descr.Free;
          end;
          if Pipe=INVALID_HANDLE_VALUE then
          begin
//            MessageBox(0, PChar('Error occured during pipe creation: '+SysErrorMessage(GetLastError)), 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
            //Log.Log('Error occured during pipe creation: '+SysErrorMessage(GetLastError), lsError);
            LogAndRaiseLastOsError(Log,ClassName, 'ServiceExecute::(winapi)CreateNamedPipe OUT', 'ElevationHandler.pas');
            abort;
          end;
//        MessageBox(0, PChar('Pipe ('+inttostr(Pipe)+') created successfully'), 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
          InitAllowedSIDs;
          try
            UnloadProfThread := TUnloadProfThread.Create;

            ar[0] := fServiceStopEvent;
            ar[1] := OvLapped.hEvent;
            ConnectNamedPipe(Pipe, @OvLapped);

            repeat
              ServiceThread.ProcessRequests(False);
              WaitResult := MsgWaitForMultipleObjects(2, @ar[0], false, INFINITE, QS_ALLINPUT)
            until WaitResult <> WAIT_OBJECT_0+2;

            while not Stopped do
            begin
              with THandleRequestThread.Create(
                true, //
                fAllowedSIDs,//const AllowedSIDs:  TJwSecurityIdList;
                fPasswords,//const Passwords   : TPasswordList;
                fServiceStopEvent,//const StopEvent  : THandle;
                fThreadsStopEvent,
                ServiceThread.ProcessRequests,//const OnServiceProcessRequest : TOnServiceProcessRequest;

                @fStopped //const StopState : PBoolean
                ) do
              begin
                FreeOnTerminate := True;
                PipeHandle := Pipe;
                Resume;
              end;
              LogEvent('Create Pipe 2');
              Pipe := CreateNamedPipe('\\.\pipe\XPElevationPipe', PIPE_ACCESS_INBOUND or FILE_FLAG_OVERLAPPED, PIPE_WAIT, PIPE_UNLIMITED_INSTANCES, 0, 0, 0, LPSECURITY_ATTRIBUTES(SecAttr));
              if Pipe = INVALID_HANDLE_VALUE then
              begin
//                MessageBox(0, PChar('Error occured during pipe creation: '+SysErrorMessage(GetLastError)), 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
                LogAndRaiseLastOsError(Log,ClassName, 'ServiceExecute::(winapi)CreateNamedPipe IN', 'ElevationHandler.pas');
                //Log.Log ('Error occured during pipe creation: '+SysErrorMessage(GetLastError), ltError);
                //abort;
              end;

              ConnectNamedPipe(Pipe, @OvLapped);
              repeat
                ServiceThread.ProcessRequests(False);
                WaitResult := MsgWaitForMultipleObjects(2, @ar[0], false, INFINITE, QS_ALLINPUT and not QS_POSTMESSAGE or QS_ALLPOSTMESSAGE)
              until WaitResult <> WAIT_OBJECT_0+2;
            end;
          finally
            CloseHandle(Pipe);
            UnloadProfThread.RequestTerminate;

            {Wait for all threads to be stopped
            }
            WaitForSingleObject(ThreadsStopEvent, 10000);
//            Sleep(1000);
            fAllowedSIDs.Free;
          {  with fPasswords.LockList do
              for i:=0 to Count-1 do
                if (Items[i]<>nil) and (Items[i]<>EmptyPass) then
                  FreeMem(Items[i]);   }
            fPasswords.Free;
            UnloadProfThread.WaitFor;
            UnloadProfThread.Free;
          end;
        finally
          if Assigned(SecAttr) then
          begin
            TJwSecurityDescriptor.Free_SA(SecAttr);
//            FreeMem(SecAttr);
          end;
          CloseHandle(OvLapped.hEvent);
        end;
      finally
//        fDesktop.Free;
//        MessageBox(0, 'Ending', 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
//        Log.Log('*** XP Elevation Service finished.');
      end;
    finally
      CloseHandle(fStopEvent);
      CloseHandle(fThreadsStopped);
    end;
  finally
    //LogEvent('*** XP Elevation Service finished. ');

    DeleteCriticalSection(fLogCriticalSection);
    //CloseFile(fLogFile);
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

  FStopped := Value;
  if Value then
  begin
    Log.Log('Stopevent executed....');
    SetEvent(fStopEvent);
  end;
end;

procedure TXPService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  Started := true;
end;




procedure TXPService.ServiceShutdown(Sender: TService);
begin
  SetStopped(true);
end;

end.
