unit HandleRequestThread;

interface

uses
  Classes, JwaWindows, JwsclToken;

type
  THandleRequestThread = class(TThread)
  private
    { Private declarations }
    fPipeHandle: THandle;
  protected
    procedure Execute; override;
  public
    property PipeHandle: THandle read fPipeHandle write fPipeHandle;
  end;

implementation
uses MainUnit;
{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure THandleRequestThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ THandleRequestThread }

procedure THandleRequestThread.Execute;
var AppName: string; PipeSize: Cardinal; OvLapped: OVERLAPPED; ar: array[0..1] of THandle;
begin
  if InterlockedExchangeAdd(XPService.fHReqThreadCount, 1)=0 then
   ResetEvent(XPService.ThreadsStopped);
  try
    if XPService.Stopped then
     exit;
    OvLapped.hEvent:=CreateEvent(nil, false, false, nil);
    try
      ar[0]:=OvLapped.hEvent;
      ar[1]:=XPService.StopEvent;
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
      XPService.StartApp(Appname);
    //  TJwSecurityToken.RevertToSelf; //now made in StartApp
    finally
      CloseHandle(OvLapped.hEvent);
      DisconnectNamedPipe(fPipeHandle);
      CloseHandle(fPipeHandle);
    end;
  finally
    if InterlockedExchangeAdd(XPService.fHReqThreadCount, -1)=1 then
      SetEvent(XPService.ThreadsStopped);
  end;
end;

end.
