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
program XPElevationService;

uses
{$IFDEF EUREKALOG}
  ExceptionLog,
{$ENDIF EUREKALOG}
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

  
procedure AttachedFilesRequestProc(EurekaExceptionRecord: TEurekaExceptionRecord;
    AttachedFiles: TStrings);
begin
  AttachedFiles.Add(LogFileNameLocation);
end;

begin
  ExceptionLog.CustomWebFieldsRequest := JEDI_WebFieldsRequestNotify;
  ExceptionLog.AttachedFilesRequest := AttachedFilesRequestProc;


  try
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
    uLogging.ApplicationFileName := 'JEDI XP Elevation';
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
  end;

end.
