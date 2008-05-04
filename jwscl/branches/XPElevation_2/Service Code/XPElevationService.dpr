program XPElevationService;

uses
  ExceptionLog,
  SysUtils,
  SvcMgr,
  Classes,
  MainUnit in 'MainUnit.pas' {XPService: TService},
  Windows,
  JwsclKnownSID,
  JwsclLogging,
  HandleRequestThread in 'HandleRequestThread.pas',
  SessionPipe in '..\SessionPipe.pas',
  ElevationHandler in 'ElevationHandler.pas',
  ThreadedPasswords in 'ThreadedPasswords.pas',
  uLogging in 'uLogging.pas',
  JwsclEurekaLogUtils in '..\..\..\trunk\source\JwsclEurekaLogUtils.pas';

{$R *.RES}


procedure EExceptionNotify(S : Pointer;EurekaExceptionRecord: TEurekaExceptionRecord;
    var Handled: Boolean);
begin
  //Handled := false;
end;


var
  ELog : TEurekaLog;
  fJwNotify : TJwEurekaLogNotify;
  P : Pointer;
  M : TMethod;

begin
  ELog := TEurekaLog.Create(nil);
  fJwNotify := TJwEurekaLogNotify.Create(ELog,'XPElevationService');

  M.Code := @EExceptionNotify;
  //Elog.OnExceptionNotify := TExceptionNotify(M);
  Elog.OnCustomWebFieldsRequest := fJwNotify.WebFieldsRequestNotify;


 (*
  try
    //raise Exception.Create('Test Error');
    GetMem(P,100);
  finally
   ELog.Free;
  end;

 // GetMem(P,100);
  exit;*)

  try
    //raise Exception.Create('');

    JwInitWellknownSIDs;
    // Windows 2003 Server requires StartServiceCtrlDispatcher to be
    // called before CoRegisterClassObject, which can be called indirectly
    // by Application.Initialize. TServiceApplication.DelayInitialize allows
    // Application.Initialize to be called from TService.Main (after
    // StartServiceCtrlDispatcher has been called).
    //
    // Delayed initialization of the Application object may affect
    // events which then occur prior to initialization, such as
    // TService.OnCreate. It is only recommended if the ServiceApplication
    // registers a class object with OLE and is intended for use with
    // Windows 2003 Server.
    //
    // Application.DelayInitialize := True;
    //
   { if not Application.DelayInitialize or Application.Installing then  }
    uLogging.ApplicationFileName := 'XPElevation';
    uLogging.InitFileLocation;

    uLogging.InitLog;

    //log by default in debug version
  {$IFDEF DEBUG}
    uLogging.SwitchLog(true);
  {$ENDIF DEBUG}

    try
      Application.Initialize;
      Application.CreateForm(TXPService, XPService);
    XPService.ServiceExecute(nil);
    //  Application.Run;
    finally
      DoneLog;
    end;
  finally
    FreeAndNil(Elog);
  end;

end.
