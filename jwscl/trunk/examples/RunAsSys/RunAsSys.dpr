program RunAsSys;

uses
  SvcMgr,
  SysUtils,
  Dialogs,
  JwaWindows,
  JwsclToken,
  JwsclKnownSid,
  JwsclElevation,
  JwsclVersion,
  JwsclExceptions,
  RunAsSysService in 'RunAsSysService.pas' {RunAsSysSvc3: TService};

{$R *.RES}


procedure StartTheService(Service : TService);
var
  hSCM: SC_HANDLE;
  hSvc: SC_HANDLE;
  args: array of PChar;
  i : Integer;
begin
  hSCM := OpenSCManager(nil, SERVICES_ACTIVE_DATABASEW, SC_MANAGER_CONNECT);
  if hSCM = 0 then
    RaiseLastOSError;

  try
    hSvc := OpenService(hSCM, PChar(Service.Name), SERVICE_START);
    if hSvc = 0 then
      RaiseLastOSError;

    try
      SetLength(args, ParamCount+1);

      args[0] := PChar(IntToStr(GetProcessLogonSession));
      for i := 1 to ParamCount do
      begin
        GetMem(args[i], Length(ParamStr(i))+2);
        StringCchCopy(args[i], Length(ParamStr(i))+2, PChar(ParamStr(i)));
      end;
      try
        if not StartServiceA(hSvc, Length(args), @args[0]) then
          RaiseLastOSError;
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

var i : Integer;
   Parameters : String;
begin
  JwInitWellKnownSIDs;
  
  
  if JwIsPrivilegeSet(SE_TCB_NAME, pqt_Available) and
   {  JwIsPrivilegeSet(SE_CREATE_TOKEN_NAME, pqt_Available) and}
     (JwSecurityProcessUserSID.EqualSid(JwLocalSystemSID)) then
  begin
    //Sleep(10000);
    SvcMgr.Application.Initialize;
    SvcMgr.Application.CreateForm(TRunAsSysSvc3, RunAsSysSvc3);
{$IFDEF CMD}
    RunAsSysSvc.DoExecute();
{$ELSE}
    SvcMgr.Application.Run;
{$ENDIF}
  end
  else
  if (JwCheckAdministratorAccess) then
  begin
    //ShowMessage(CmdLine);

    SvcMgr.Application.Free;
    SvcMgr.Application := TServiceApplicationEx.Create(nil);
    SvcMgr.Application.Initialize;
    SvcMgr.Application.CreateForm(TRunAsSysSvc3, RunAsSysSvc3);

    try
      TServiceApplicationEx(SvcMgr.Application).RegisterServices(true, true);
    except
      on E : EOSError do
      begin
        if E.ErrorCode <> 1073 then
          raise;
      end;
    end;
    try
      StartTheService(RunAsSysSvc3);
    finally
     TServiceApplicationEx(SvcMgr.Application).RegisterServices(false, true);
    end;
    //FreeAndNil(SvcMgr.Application);
    SvcMgr.Application.Run;
  end
  else
  begin
    try
      if ParamCount = 0 then
	    halt(1);
	  
	  if not (TJwWindowsVersion.IsWindowsVista(true) or
        TJwWindowsVersion.IsWindows2008(true)) then
      begin
        ShowMessage('You have to logon as administrator. Please enter your admin credentials.');
      end;

      Parameters := '';
      for i := 1 to ParamCount do
      begin
        Parameters := Parameters + ' ' +ParamStr(i);
      end;

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
        if E.LastError <> E_USER_CANCELED_OPERATION then
          ShowMessage(e.Message);
        halt(E.LastError);  
      end;
      on E1 : Exception do
      begin
        ShowMessage(e1.Message);
        halt(1);
      end;
    end;

  end;
end.
