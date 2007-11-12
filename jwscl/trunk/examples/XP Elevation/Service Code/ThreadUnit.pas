unit ThreadUnit;

interface

uses
  Classes, JwsclToken, JwaWindows, SysUtils;

type
  TUnloadProfThread = class(TThread)
  private
    { Private declarations }
    fProfile:          THandle;
    fJob:              THandle;
    fToken:            TJwSecurityToken;

    fIOCompletionPort: THandle;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create(Profile: THandle; Token: TJwSecurityToken; Job: THandle);
    property IOCompletionPort: THandle read fIOCompletionPort;
  end;

const
  MESSAGE_FROM_JOB     = 0;
  MESSAGE_FROM_SERVICE = 1;

implementation
uses MainUnit;
{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TUnloadProfThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TUnloadProfThread }

procedure TUnloadProfThread.Execute;
var Val, Key: Cardinal; Ov: POVERLAPPED;
begin
  repeat
    GetQueuedCompletionStatus(fIOCompletionPort, Val, Key, Ov, INFINITE);
  until (Key<>MESSAGE_FROM_JOB) or (Val=JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO);

  if Key=MESSAGE_FROM_SERVICE then
    TerminateJobObject(fJob, 0);

  UnloadUserProfile(fToken.TokenHandle, fProfile);
  fToken.Free;
  CloseHandle(fJob);
end;

procedure TUnloadProfThread.DoTerminate;
begin
  XPElevationService.LogEvent('Thread terminates');
  CloseHandle(fIOCompletionPort);
  inherited;
end;

constructor TUnloadProfThread.Create(Profile: THandle; Token: TJwSecurityToken; Job: THandle);
var JobIOCompletion: JOBOBJECT_ASSOCIATE_COMPLETION_PORT;
begin
  inherited Create(true);
  FreeOnTerminate:=True;
  fProfile:=Profile;
  fToken:=Token;
  fJob:=Job;
  fIOCompletionPort:=CreateIOCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 1);
  SetLength(XPElevationService.fCompletionPorts, Length(XPElevationService.fCompletionPorts)+1);
  EnterCriticalSection(XPElevationService.fCompletionPortsCrit);
  try
    if XPElevationService.Stopped then //this is necessary!
      exit;
    XPElevationService.fCompletionPorts[high(XPElevationService.fCompletionPorts)]:=fIOCompletionPort;
  finally
    LeaveCriticalSection(XPElevationService.fCompletionPortsCrit);
  end;
  JobIOCompletion.CompletionPort:=fIOCompletionPort;
  JobIOCompletion.CompletionKey:=Pointer(MESSAGE_FROM_JOB);
  if not SetInformationJobObject(fJob, JobObjectAssociateCompletionPortInformation, @JobIOCompletion, SizeOf(JobIOCompletion)) then
    XPElevationService.LogEvent('Error in SetInformationJobObject: '+inttostr(fIOCompletionPort)+' '+inttostr(fJob)+SysErrorMessage(GetLastError));
  Resume;
end;

end.
