unit ThreadUnit;

interface

uses
  Classes, JwsclToken, JwaWindows, SysUtils;

type
  TUnloadProfThread = class(TThread)
  private
    fList:             TThreadList;
    fIOCompletionPort: THandle;
    fCurrentId:        Integer;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create;
    procedure Add(Job, Profile: Cardinal; Token: TJwSecurityToken);
    property IOCompletionPort: THandle read fIOCompletionPort;
  end;

const
  MESSAGE_FROM_SERVICE = 0;

var UnloadProfThread: TUnloadProfThread;
implementation
uses MainUnit;

type
  PJobInformation = ^TJobInformation;
  TJobInformation = record
    Job:     Cardinal;
    Profile: Cardinal;
    Token:   TJwSecurityToken;
    Id:      Integer;
  end;

procedure TUnloadProfThread.Execute;
var Val, Key: Cardinal; Ov: POVERLAPPED; i: integer;
begin
  while True do
  begin
    repeat
      GetQueuedCompletionStatus(fIOCompletionPort, Val, Key, Ov, INFINITE);
    until (Key = MESSAGE_FROM_SERVICE) or
     (Val=JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO);

    if Key = MESSAGE_FROM_SERVICE then //Is this a stop message?
    begin
      with fList.LockList do
      begin
        for i:=0 to Count-1 do
        begin
          with PJobInformation(Items[i])^ do
          begin
            TerminateJobObject(Job, 0);
            CloseHandle(Job);
            UnloadUserProfile(Token.TokenHandle, Profile);
            Token.Free;
          end;
          Dispose(PJobInformation(Items[i]));
        end;
        break;
      end;
    end;

    with fList.LockList do
    try
      for i:=0 to Count-1 do
        if Cardinal(PJobInformation(Items[i])^.Id) = Key then
        begin
          with PJobInformation(Items[i])^ do
          begin
            CloseHandle(Job);
            UnloadUserProfile(Token.TokenHandle, Profile);
            Token.Free;
          end;
          Dispose(PJobInformation(Items[i]));
          Delete(i);
          break;
        end;
    finally
      fList.UnlockList;
    end;
  end;
end;

procedure TUnloadProfThread.Add(Job: Cardinal; Profile: Cardinal; Token: TJwSecurityToken);
var JobInformation: PJobInformation;
    AssocPort: JOBOBJECT_ASSOCIATE_COMPLETION_PORT;
begin
  New(JobInformation);
  JobInformation.Job := Job;
  JobInformation.Profile := Profile;
  JobInformation.Token := Token;
  JobInformation.Id := InterlockedExchangeAdd(fCurrentId, 1);
  AssocPort.CompletionPort := fIOCompletionPort;
  AssocPort.CompletionKey := Pointer(JobInformation.Id);
  if not SetInformationJobObject(Job, JobObjectAssociateCompletionPortInformation,
           @AssocPort, SizeOf(AssocPort)) then
    Service1.LogEvent('SetInformationJobObject failed with '+SysErrorMessage(GetLastError), ltError)
  else
    fList.Add(JobInformation);
end;

procedure TUnloadProfThread.DoTerminate;
begin
  CloseHandle(fIOCompletionPort);
  fList.Free;
  Service1.LogEvent('UnloadProfThread terminates');
end;

constructor TUnloadProfThread.Create;
begin
  fList := TThreadList.Create;
  fIOCompletionPort := CreateIOCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 1);
  fCurrentId := MESSAGE_FROM_SERVICE+1; //MESSAGE_FROM_SERVICE is reserved for the stop message
  inherited Create(false);
end;

end.
