program RunAsSys;

uses
  SvcMgr,
  SysUtils,
  Dialogs,

  Controls,
  uLogging,

  JwaWindows,
  JwsclLogging,
  JwsclToken,
  JwsclKnownSid,
  JwsclElevation,
  JwsclVersion,
  JwsclExceptions,
  RunAsSysService in 'RunAsSysService.pas' {RunAsSysSvc3: TService},
  RunAsSysCreateToken in 'RunAsSysCreateToken.pas';

{$R *.RES}


procedure StartTheService(Service : TService);
var
  hSCM: SC_HANDLE;
  hSvc: SC_HANDLE;
  args: array of PChar;
  i : Integer;
  Log : IJwLogClient;
begin
  Log := uLogging.LogServer.Connect(etFunction, '','StartTheService','RunAsSys.dpr','Starting service...');

  Log.Log('Open service manager...');
  hSCM := OpenSCManager(nil, SERVICES_ACTIVE_DATABASEW, SC_MANAGER_CONNECT);
  if hSCM = 0 then
    RaiseLastOSError;

  try
    Log.Log('Open service...');
    hSvc := OpenService(hSCM, PChar(Service.Name), SERVICE_START);
    if hSvc = 0 then
      RaiseLastOSError;

    try
      SetLength(args, ParamCount+1);

      args[0] := PChar(IntToStr(JwGetProcessLogonSession));
      for i := 1 to ParamCount do
      begin
        Log.Log('Adding Parameter: '+ParamStr(i));
        GetMem(args[i], Length(ParamStr(i))+2);
        StringCchCopy(args[i], Length(ParamStr(i))+2, PChar(ParamStr(i)));
      end;
      try
        Log.Log('Start service...');
        if not StartServiceA(hSvc, Length(args), @args[0]) then
          RaiseLastOSError;
        Log.Log('Cleanup...');
      finally
        for i := 1 to ParamCount do
        begin
          FreeMem(args[i]);
        end;
      end;
    finally
      CloseServiceHandle(hSvc);
    end;
  finally
   CloseServiceHandle(hScm);
  end;
end;

function CheckParameters(Log : IJwLogClient) : AnsiString;
var
  SysPath : Array[0..MAX_PATH] of AnsiChar;
  hres : HRESULT;
begin
  result := '';

  begin
    if Assigned(Log) then
      Log.Log('No parameters found. Starting command line as SYSTEM...');
    ZeroMemory(@SysPath, sizeof(SysPath));

    hres := SHGetFolderPathA(0,CSIDL_SYSTEMX86,0,0, SysPath);
    if SUCCEEDED(hres) then
       result := '"' +IncludeTrailingBackslash(SysPath)+'cmd.exe"'
    else
    begin
      if Assigned(Log) then
        Log.Log(Format('SHGetFolderPath returned error %d',[hres]),lsError);
      result := 'C:\Windows\System32\cmd.exe';
    end;
  end
end;

function MainWorkerProcedure : Integer;
var
  Log : IJwLogClient;

  procedure InitLog;
  begin
    uLogging.InitLog;

    Log := uLogging.LogServer.Connect(etThread, '','WinMain','RunAsSys.dpr','Entering main thread');
  end;


var
  i : Integer;
  Parameters : String;
  hRes : HRESULT;
begin
  result := 0;
  Log := nil;
  try
    JwInitWellKnownSIDs;


    if JwIsPrivilegeSet(SE_TCB_NAME, pqt_Available) and
     {  JwIsPrivilegeSet(SE_CREATE_TOKEN_NAME, pqt_Available) and}
       (JwSecurityProcessUserSID.EqualSid(JwLocalSystemSID)) then
    begin
      InitLog;
      
      Log.Log('Found service privileges. Starting service...');
      SvcMgr.Application.Initialize;
      SvcMgr.Application.CreateForm(TRunAsSysSvc3, RunAsSysSvc3);
  {$IFDEF CMD}
      RunAsSysSvc3.DoExecute();
  {$ELSE}
      SvcMgr.Application.Run;
  {$ENDIF}
      Log.Log('Ending service.');

      exit;
    end
    else
    if (JwCheckAdministratorAccess) then
    begin
      if ParamCount = 0 then
      begin
        JwShellExecute(0, //const hWnd: HWND;
          ParamStr(0), //FileName,
          CheckParameters(Log), //Parameters,
          '', //Directory: TJwString;
          0, //ShowCmd: Integer;
          [sefIgnoreElevationIfNotAvailable]//Flags : TJwShellExecuteFlags = [sefNoClosehProcess]): HANDLE;
        );
        exit;
      end;

      InitLog;

      Log.Log('Found Administrator privileges. Creating service...');


      SvcMgr.Application.Free;
      SvcMgr.Application := TServiceApplicationEx.Create(nil);
      SvcMgr.Application.Initialize;
      SvcMgr.Application.CreateForm(TRunAsSysSvc3, RunAsSysSvc3);

      try
        Log.Log('Registering service...');
        TServiceApplicationEx(SvcMgr.Application).RegisterServices(true, true);
      except
        on E : EOSError do
        begin
          Log.Exception(E);
          if E.ErrorCode <> 1073 then
            raise;
        end;
      end;
      try
        Log.Log('Starting service...');
        StartTheService(RunAsSysSvc3);
      finally
       Log.Log('Remove service.');
       TServiceApplicationEx(SvcMgr.Application).RegisterServices(false, true);
      end;
      SvcMgr.Application.Run;

      exit;
    end
    else
    begin
      InitLog;
      try
        if not (TJwWindowsVersion.IsWindowsVista(true) or
          TJwWindowsVersion.IsWindows2008(true)) then
        begin
          if MessageDlg('You have to logon as administrator. Please enter your admin credentials. ',
              mtInformation, [mbOK,mbCancel],0) = mrCancel then
          begin
            result := 2;
            exit;
          end;
        end;

        if ParamCount = 0 then
          Parameters := CheckParameters(log)
        else
        begin
          Parameters := '';
          for i := 1 to ParamCount do
          begin
            Parameters := Parameters + ' ' +ParamStr(i);
          end;
        end;

        Log.Log(Format('Try to elevate with these parameters: %s',[Parameters]));
        JwShellExecute(0, //const hWnd: HWND;
          ParamStr(0), //FileName,
          Parameters, //Parameters,
          '', //Directory: TJwString;
          0, //ShowCmd: Integer;
          [sefIgnoreElevationIfNotAvailable]//Flags : TJwShellExecuteFlags = [sefNoClosehProcess]): HANDLE;
        );
      except
        on E : EJwsclWinCallFailedException do
        begin
          Log.Log(lsError,'Catching EJwsclWinCallFailedException from JwShellExecute');
          Log.Exception(E);
          if E.LastError <> E_USER_CANCELED_OPERATION then
            ShowMessage(e.Message);

          result := E.LastError;
          exit;
        end;
        on E1 : Exception do
        begin
          Log.Log(lsError,'Catching Exception from JwShellExecute');
          ShowMessage(e1.Message);

          Result := 1;
          exit;
        end;
      end;
      exit;
    end;
  except
    on E1 : EOSError do
    begin
      result := E1.ErrorCode;
      Log.Exception(E1);
    end;
    on E2 : EJwsclSecurityException do
    begin
      result := E2.LastError;
      Log.Exception(E2);
    end;
    on E3 : Exception do
    begin
      result := 1;
      Log.Exception(E3);
    end;
  end;
end;

procedure DoHalt(const Value : Integer);

procedure EndLog;
var
  Log : IJwLogClient;
begin
  if Assigned(LogServer) then
  begin
    Log := uLogging.LogServer.Connect(etNone, '','','','');
    Log.Log(lsStop,Format('Halt(%d)',[Value]));

    uLogging.LogServer.Disconnect(Log);
  end;
end;

begin
  EndLog;
  DoneLog;
  LogServer := nil;
  halt(Value);
end;

begin
  DoHalt(MainWorkerProcedure);
end.
