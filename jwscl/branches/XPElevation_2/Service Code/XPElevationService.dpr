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
  ExceptionLog,
  SysUtils,
  SvcMgr,
  Classes,
  MainUnit in 'MainUnit.pas' {XPService: TService},
  Dialogs,
  JwaWindows,
  JwsclKnownSID,
  JwsclLogging,
  HandleRequestThread in 'HandleRequestThread.pas',
  SessionPipe in '..\SessionPipe.pas',
  ElevationHandler in 'ElevationHandler.pas',
  ThreadedPasswords in 'ThreadedPasswords.pas',
  uLogging in 'uLogging.pas',
  JwsclEurekaLogUtils in '..\..\..\trunk\source\JwsclEurekaLogUtils.pas',
  XPElevationCommon in '..\XPElevationCommon.pas',
  MappedStreams in 'MappedStreams.pas';

{$R *.RES}


function StartProcess(const PathName : WideString) : THandle;
var ProcInfo: PROCESS_INFORMATION;
    StartInfo : TStartupInfoW;
begin
  ZeroMemory(@StartInfo, sizeof(StartInfo));
  StartInfo.cb := sizeof(StartInfo);
//  StartInfo.lpDesktop := 'winsta0\default';
  StartInfo.wShowWindow := SW_SHOW;


  if not CreateProcessW(PWideChar(PathName),nil, nil,nil,false, CREATE_NEW_CONSOLE, nil, nil, StartInfo, ProcInfo) then
    ShowMessage(Format('The application could not be started: (%d) %s',[GetLastError,SysErrorMessage(GetLastError)]));

  CloseHandle(ProcInfo.hThread);
  result := ProcInfo.hProcess;
end;


function GetSystem32Path : WideString;
var Path : Array[0..MAX_PATH] of Widechar;
begin
  Result := '';
  if SUCCEEDED(SHGetFolderPathW(0,CSIDL_SYSTEM,0,SHGFP_TYPE_DEFAULT, @Path)) then
    result := IncludeTrailingBackslash(Path);  //Oopps, may convert unicode to ansicode
end;
  
procedure AttachedFilesRequestProc(EurekaExceptionRecord: TEurekaExceptionRecord;
    AttachedFiles: TStrings);
begin
  AttachedFiles.Add(LogFileNameLocation);
end;

begin
  ExceptionLog.CustomWebFieldsRequest := JEDI_WebFieldsRequestNotify;
  ExceptionLog.AttachedFilesRequest := AttachedFilesRequestProc;

  //StartProcess(GetSystem32Path+'osk.exe');
  //StartProcess(GetSystem32Path+'ntprint.exe');

  //exit;


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
