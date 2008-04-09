unit ElevationHandler;

interface
uses
  Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs, Math, ComObj,
  JwaWindows, JwsclToken, JwsclLsa, JwsclCredentials, JwsclDescriptor, JwsclDesktops,
  JwsclExceptions, JwsclSID, JwsclAcl,JwsclKnownSID, JwsclEncryption, JwsclTypes,
  SessionPipe, ThreadUnit, JwsclLogging, uLogging, ThreadedPasswords,
  JwsclStrings;

type
  TElevationHandler = class(TObject)
  private

  protected
    OvLapped: OVERLAPPED;
    ServerPipe : TServerSessionPipe;
    fAllowedSIDs : TJwSecurityIdList;
    fPasswords   : TPasswordList;
    fStopEvent : THandle;
    fOnServiceProcessRequest : TOnServiceProcessRequest;
    fStopState : PBoolean;


    function GetStopState : Boolean;
  public
    constructor Create(
      const AllowedSIDs: TJwSecurityIdList;
      const Passwords  : TPasswordList;
      const StopEvent  : THandle;
      const OnServiceProcessRequest : TOnServiceProcessRequest;
      const StopState : PBoolean);
    destructor Destroy; override;

    procedure StartApplication(const ApplicationPath: WideString);
    function AskCredentials(const ClientPipeUserToken: TJwSecurityToken;
        var SessionInfo : TSessionInfo): boolean;

    property StopEvent: THandle read fStopEvent;
    property OnServiceProcessRequest : TOnServiceProcessRequest read fOnServiceProcessRequest;
    property StopState : Boolean read GetStopState;
  end;

const
   EMPTYPASSWORD = Pointer(-1);

   CredApplicationKey='CredentialsApplication';


implementation
uses Registry;

function RegGetFullPath(PathKey: string): string;
var Reg: TRegistry; Unresolved: string;
begin
  Reg:=TRegistry.Create(KEY_QUERY_VALUE);
  try
    Reg.RootKey:=HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('Software\XPElevation\Paths\', false) then
    try
      Unresolved:=Reg.ReadString(PathKey);
      SetLength(Result, MAX_PATH+1);
      ExpandEnvironmentStrings(PChar(Unresolved), @Result[1], MAX_PATH+1);
      SetLength(Result, StrLen(PChar(Result)));
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;


{ TElevationHandler }

function TElevationHandler.AskCredentials(
  const ClientPipeUserToken: TJwSecurityToken;
  var SessionInfo: TSessionInfo): boolean;

//procedure WaitForClient(const hPipe : HANDLE;


const TIMEOUT = 3000;
var StartInfo: STARTUPINFOW;

const StrLenCancelled = Cardinal(-1);
      SaveBit         = Cardinal(1 shl 31);
var ProcInfo: PROCESS_INFORMATION;
    Ar: Array[0..2] of THandle; Dummy: Cardinal;
    ReadPipe, WritePipe: THandle;
    Loc: Cardinal;
    Desc: TJwSecurityDescriptor;
    SecAttr: LPSECURITY_ATTRIBUTES;
    CredApp: String;


    Status : DWORD;

    AppliationCmdLine,
    PipeName : WideString;
    Connected : Boolean;
    ServerBuffer : TServerBuffer;
    ClientBuffer : TClientBuffer;

    NumBytesWritten,
    NumBytesToBeRead,
    NumBytesRead : DWORD;
    Timer : DWORD;
    OvLapped: OVERLAPPED;
    TimeOutInt64 : LARGE_INTEGER;
    WaitRes : DWORD;
    Data : Array[0..100] of char;

    fTimer : THandle;

    hPipe : THandle;
    Log : IJwLogClient;

begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'AskCredentials','ElevationHandler.pas','');
  {if not Assigned(ClientPipeUserToken) then
    exit;}

  Desc := TJwSecurityDescriptor.Create;
  try
    Desc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_ALL, JwLocalSystemSID));
    Desc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_ALL, JwWorldSID));
    Desc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_ALL, ClientPipeUserToken.TokenUser, false));

    SecAttr := LPSECURITY_ATTRIBUTES(Desc.Create_SA());


    PipeName := '\\.\pipe\AskElevation';

    hPipe := CreateNamedPipeW(
        PWideChar(PipeName),//lpName: LPCWSTR;
        PIPE_ACCESS_DUPLEX or FILE_FLAG_OVERLAPPED,//dwOpenMode,
        PIPE_TYPE_MESSAGE or       // message type pipe
          PIPE_READMODE_MESSAGE or   // message-read mode
          PIPE_WAIT,//dwPipeMode,
        1,//nMaxInstances,
        max(sizeof(TServerBuffer) ,sizeof(TClientBuffer)),//nOutBufferSize,
        max(sizeof(TServerBuffer) ,sizeof(TClientBuffer)),//nInBufferSize,
        10000,//nDefaultTimeOut: DWORD;
        SecAttr
        );
    ServerPipe.Assign(hPipe, TIMEOUT);

    if hPipe = INVALID_HANDLE_VALUE then
    begin
      LogAndRaiseLastOsError(Log, ClassName, 'AskCredentials::(winapi)CreateNamedPipeW', 'ElevationHandler.pas');
    end;

  except
    on E : Exception do
    begin
      FreeAndNil(ServerPipe);

      TJwSecurityDescriptor.Free_SA(PSecurityAttributes(SecAttr));
      FreeAndNil(Desc);
      
      raise;
    end;
  end;

  try
    CredApp := RegGetFullPath(CredApplicationKey);

    AppliationCmdLine := Sysutils.WideFormat('"%s" "%s"',
       [CredApp, PipeName]);

    ZeroMemory(@StartInfo, Sizeof(StartInfo));
    StartInfo.cb:=Sizeof(StartInfo);
    StartInfo.lpDesktop:='WinSta0\Default';

    if not CreateProcessAsUserW(
       ClientPipeUserToken.TokenHandle,
       PWideChar(Widestring(CredApp)),
       //PWideChar(WideString('P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\jedi-api-lib\jwscl\trunk\examples\XP Elevation\pipetest\client.exe')),
       PWideChar(Widestring(AppliationCmdLine)) ,
      nil, nil, True, CREATE_NEW_CONSOLE, nil, nil, StartInfo, ProcInfo) then
    begin
      //!!LogEvent('Could not create the process asking for credentials '+SysErrorMessage(GetLastError), ltError);
      LogAndRaiseLastOsError(Log, ClassName, 'AskCredentials::(winapi)CreateProcessAsUserW', 'ElevationHandler.pas');
    end;

    ServerPipe.WaitForClientToConnect(0, 0, StopEvent);

//VISTA
//    if GetNamed
//    ProcInfo.dwProcessId


    ServerPipe.SendServerData(SessionInfo);

    ServerPipe.WaitForClientAnswer(TIMEOUT, StopEvent);

    ServerPipe.ReadClientData(SessionInfo);


   (* NumBytesToBeRead := 0;

    ZeroMemory(@TimeOutInt64,sizeof(TimeOutInt64));
    TimeOutInt64.HighPart := -1;
    TimeOutInt64.LowPart := high(TimeOutInt64.LowPart) - (Int64(TIMEOUT) * 1 * 100000)+1 ;

    fTimer := CreateWaitableTimer(nil, TRUE, PChar('InternalTimer_'+IntToStr(GetCurrentThreadId)));
    if fTimer = 0 then
    begin
      RaiseLastOSError;
    end;
    if not SetWaitableTimer(fTimer, TimeOutInt64, 0, nil, nil, false) then
    begin
      RaiseLastOSError;
    end;   *)
   // WaitForSingleObject(fTimer,INFINITE);

(*    Timer := GetTickCount;
    while (NumBytesToBeRead < sizeof(TClientBuffer)) do
    begin
      NumBytesToBeRead := 0;
      PeekNamedPipe(
        Pipe,//__in       HANDLE hNamedPipe,
        @Data,//__out_opt  LPVOID lpBuffer,
        sizeof(Data),//__in       DWORD nBufferSize,
        @NumBytesRead,//__out_opt  LPDWORD lpBytesRead,
        @NumBytesToBeRead,//__out_opt  LPDWORD lpTotalBytesAvail,
        @OvLapped//__out_opt  LPDWORD lpBytesLeftThisMessage
      );

      Ar[0] := fStopEvent;
      Ar[1] := fTimer;

      WaitRes := WaitForMultipleObjects(2, @Ar[0], false, 10);
      if WaitRes = WAIT_OBJECT_0+1 then
      begin
        Abort;
      end;
      if WaitRes = WAIT_OBJECT_0 then
      begin
        Abort;
      end;
      //WAIT_TIMEOUT is only for update purposes 

      OnServiceProcessRequest(false);
      //ServiceThread.ProcessRequests(False);
    end;

    ZeroMemory(@ServerBuffer, sizeof(ServerBuffer));
    ZeroMemory(@ClientBuffer, sizeof(ClientBuffer));

    if not ReadFile(
        Pipe,//__in         HANDLE hFile,
        Pointer(@ClientBuffer),//__out        LPVOID lpBuffer,
        sizeof(TClientBuffer),//__in         DWORD nNumberOfBytesToRead,
        @NumBytesRead,//__out_opt    LPDWORD lpNumberOfBytesRead,
        @OvLapped//__inout_opt  LPOVERLAPPED lpOverlapped
      ) then
    begin
      RaiseLastOSError;
      Abort;
    end;

    if NumBytesRead < sizeof(TClientBuffer) then
    begin
      Abort;
    end;


    if ClientBuffer.Signature <> 'client' then
    begin
      Abort;
    end;

    //UserName := ClientBuffer.UserName;

    SetLength(SessionInfo.UserName, StringCbLengthHelperW(ClientBuffer.UserName, sizeof(ClientBuffer.UserName)));
    SetLength(SessionInfo.Domain, StringCbLengthHelperW(ClientBuffer.Domain, sizeof(ClientBuffer.Domain)));
    SetLength(SessionInfo.Password, StringCbLengthHelperW(ClientBuffer.Password, sizeof(ClientBuffer.Password)));

    OleCheck(StringCchCopyW(@PWideChar(SessionInfo.UserName)[1], Length(SessionInfo.UserName), ClientBuffer.UserName));
    //SessionInfo.UserName := ClientBuffer.UserName;

    OleCheck(StringCchCopyW(@PWideChar(SessionInfo.Domain)[1], Length(SessionInfo.Domain), ClientBuffer.Domain));
    //SessionInfo.Domain := ClientBuffer.Domain;

    OleCheck(StringCchCopyW(@PWideChar(SessionInfo.Password)[1], Length(SessionInfo.Password), ClientBuffer.Password));
    //SessionInfo.Password := ClientBuffer.Password;

    SessionInfo.Flags := ClientBuffer.Flags;
     ZeroMemory(@ClientBuffer, sizeof(ClientBuffer));      *)
  finally
{    if Pipe <> INVALID_HANDLE_VALUE then
      CloseHandle(Pipe);
    if ProcInfo.hThread <> 0 then
       CloseHandle(ProcInfo.hThread);
    if ProcInfo.hProcess <> 0 then
      CloseHandle(ProcInfo.hProcess);}
  end;


  exit;
(*  Result:=False;
  InitStartInfo;
  ZeroMemory(@ProcInfo, Sizeof(ProcInfo));
  Desc:=TJwSecurityDescriptor.Create;
//  Assert(JwIsPrivilegeSet(SE_ASSIGNPRIMARYTOKEN_NAME), 'SE_ASSIGNPRIMARYTOKEN_NAME not held');
//  Assert(JwIsPrivilegeSet(SE_INCREASE_QUOTA_NAME), 'SE_INCREASE_QUOTA_NAME not held');
//  Assert((Token.AccessMask and TOKEN_ASSIGN_PRIMARY)=TOKEN_ASSIGN_PRIMARY, 'TOKEN_ASSIGN_PRIMARY not in access mask');
//  Assert((Token.AccessMask and TOKEN_DUPLICATE)=TOKEN_DUPLICATE, 'TOKEN_DUPLICATE not in access mask');
//  Assert((Token.AccessMask and TOKEN_QUERY)=TOKEN_QUERY, 'TOKEN_QUERY not in access mask');
  try

    SecAttr:=LPSECURITY_ATTRIBUTES(Desc.Create_SA(false));
    try
      CredApp:=RegGetFullPath(CredApplicationKey);
      //The application will immediately stop execution because it expects two parameters, but we need its exit code
      if not CreateProcessAsUser(Token.TokenHandle, PChar(CredApp), nil,
             SecAttr, SecAttr, True, CREATE_NEW_CONSOLE, nil, nil, StartInfo, ProcInfo) then
      begin
        LogEvent('Could not create the process asking for credentials '+SysErrorMessage(GetLastError), ltError);
        Abort;
      end
      else
      begin
        CloseHandle(ProcInfo.hThread);
        Ar[0]:=fStopEvent;
        Ar[1]:=ProcInfo.hProcess;
        //This function is called in a separate thread, so there is no need for ServiceThread.ProcessRequests!
//        while MsgWaitForMultipleObjects(2, @Ar[0], false, INFINITE, QS_ALLINPUT)=WAIT_OBJECT_0+2 do
//          ServiceThread.ProcessRequests(false);
        WaitForMultipleObjects(2, @Ar[0], false, INFINITE);
        if fStopped then
        begin
          //Terminate process???
          CloseHandle(ProcInfo.hProcess);
          Abort;
        end;
        //The application will return an address in its exit code
        GetExitCodeProcess(ProcInfo.hProcess, Loc);
        CloseHandle(ProcInfo.hProcess);

        if not CreateProcessAsUser(Token.TokenHandle, PChar(CredApp), PChar('"'+CredApp+'" "'+AppToStart+'" "'+UserAndDomain+'"'),
                 SecAttr, SecAttr, True, CREATE_NEW_CONSOLE or CREATE_SUSPENDED, nil, nil, StartInfo, ProcInfo) then
        begin
          LogEvent('Could not create the process asking for credentials '+SysErrorMessage(GetLastError), ltError);
          Abort;
        end
        else
        begin
          CreatePipe(ReadPipe, WritePipe, nil, 0);
          try
            DuplicateHandle(GetCurrentProcess, WritePipe, ProcInfo.hProcess, @WritePipe, 0, false, DUPLICATE_SAME_ACCESS or DUPLICATE_CLOSE_SOURCE);
            //write the new handle to the location given in the exit code
            WriteProcessMemory(ProcInfo.hProcess, Pointer(Loc), @WritePipe, SizeOf(WritePipe), nil);
            ResumeThread(ProcInfo.hThread);
            CloseHandle(ProcInfo.hThread);
            Ar[0]:=fStopEvent;
            Ar[1]:=ProcInfo.hProcess;
            WaitForMultipleObjects(2, @Ar[0], false, INFINITE);
            if fStopped then
            begin
              //Terminate process???
              CloseHandle(ProcInfo.hProcess);
              Abort;
            end;
            CloseHandle(ProcInfo.hProcess);
            //we just reuse loc here
            ReadFile(ReadPipe, @Loc, SizeOf(Loc), @Dummy, nil);
            Result:=Loc<>StrLenCancelled;
            if Result then
            begin
              Save:=Loc and SaveBit = SaveBit;
              SetLength(Password, Loc and not SaveBit);
              if Loc<>0 then
                ReadFile(ReadPipe, Pointer(Password), Loc and not SaveBit, @Dummy, nil);
            end;
          finally
            CloseHandle(ReadPipe);
            CloseHandle(WritePipe);
          end;
        end;
    end;
    finally
      Desc.Free_SA(PSECURITY_ATTRIBUTES(SecAttr));
    end;
  finally
    Desc.Free;
  end;*)
end;

constructor TElevationHandler.Create(
  const AllowedSIDs:  TJwSecurityIdList;
  const Passwords   : TPasswordList;
  const StopEvent  : THandle;
  const OnServiceProcessRequest : TOnServiceProcessRequest;
  const StopState : PBoolean);
begin
  fAllowedSIDs := AllowedSIDs;
  fPasswords := Passwords;
  fStopEvent := StopEvent;
  fOnServiceProcessRequest := OnServiceProcessRequest;
  fStopState := StopState;

  ServerPipe := TServerSessionPipe.Create;
  ZeroMemory(@OvLapped, sizeof(OvLapped));
  OvLapped.hEvent := StopEvent;

  ServerPipe.OnProcessRequest := OnServiceProcessRequest;
end;

destructor TElevationHandler.Destroy;
begin
  FreeAndNil(ServerPipe);
  inherited;
end;

function TElevationHandler.GetStopState: Boolean;
begin
  if fStopState <> nil then
    result := false
  else
    result := fStopState^;
end;

procedure TElevationHandler.StartApplication(
  const ApplicationPath: WideString);

var Password,
    Username, Domain: Widestring; LSA: TJwSecurityLsa;

    plogonData : PMSV1_0_INTERACTIVE_LOGON; Authlen: Cardinal;
    Source: TTokenSource; ProfBuffer: PMSV1_0_INTERACTIVE_PROFILE; ProfBufferLen: Cardinal;
    Token, NewToken: TJwSecurityToken; TokenLuid: TLUID; QuotaLimits: QUOTA_LIMITS; SubStatus: integer;

    ProfInfo: PROFILEINFO; AddGroups: TJwSecurityIDList; SID: TJwSecurityID;

    EnvirBlock: Pointer;
    StartInfo: STARTUPINFOW;
    ProcInfo: PROCESS_INFORMATION;
    Job: THandle; EncryptLength: Cardinal; SIDIndex: Integer;

     Save: Boolean;

    PassEntry : PPassEntry;

const EncryptionBlockSize = 8;
var SessionInfo : TSessionInfo;
    Log : IJwLogClient;
    LockedList : TList;
    ErrorResult : DWORD;
begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'StartApplication','ElevationHandler.pas','');

  ErrorResult := 0;
  try
    Token:=TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);
    try
      SID:=Token.TokenUser;
      try
        SIDIndex := fAllowedSIDs.FindSid(SID);
        If SIDIndex = -1 then
        begin
          ErrorResult := ERROR_INVALID_USER;
          Log.Log('Elevation of user '+SID.AccountName['']+' for application '+ApplicationPath+' not allowed.');
//!!          LogEvent('Elevation of user '+SID.AccountName['']+' for application '+AppToStart+' not allowed.');
//          //we do not want to wait for the user to abort this dialog
//          WTSSendMessage(WTS_CURRENT_SERVER_HANDLE, WTS_CURRENT_SESSION, 'XP Elevation', StrLen('XP Elevation'),
//                         PChar('The user '+SID.AccountName['']+' is not allowed to be elevated.'), Length('The user '+SID.AccountName['']+' is not alloqed to be elevated.'),
//                         MB_OK, 0, Dummy, false);
//!!          MessageBox(0, PChar('The user '+SID.AccountName['']+' is not allowed to be elevated.'), MessageboxCaption, MB_SERVICE_NOTIFICATION or MB_OK);
          abort;
        end;
        Username := SID.AccountName[''];
        try
          Domain := SID.GetAccountDomainName('');
        except
          Domain := 'local';
        end;

      finally
        SID.Free;
      end;
      TJwSecurityToken.RevertToSelf;
      Save:=False;

      Token.ConvertToPrimaryToken(TOKEN_ALL_ACCESS);
      LockedList := fPasswords.LockList;
      with LockedList  do
      try
        PassEntry := Items[SIDIndex];

        Log.Log('Credentials for user '+Username+' are asked');

        SessionInfo.Application := ApplicationPath;
        SessionInfo.UserName := Username;
        SessionInfo.Domain   := Domain;
        SessionInfo.Password := '';

        if PassEntry <> nil then
          SessionInfo.Flags := SERVER_CACHEAVAILABLE;

        if not AskCredentials(Token, SessionInfo) then
        begin
          Log.Log('Credentials prompt for '+ApplicationPath+' aborted by user');
           fPasswords.UnlockList;
          ErrorResult := ERROR_ABORTBYUSER;
          exit;
        end;



        {Get password from cache.
         In this case Username and Domain member of SessionInfo (from Client)
         are ignored
        }
        if (SessionInfo.Flags and CLIENT_USECACHECREDS = CLIENT_USECACHECREDS) and
           (PassEntry <> nil) and (SessionInfo.Password = '') then
        begin
          Log.Log('Credentials for user '+Username+' are retrieved from the cache');

          fPasswords.Get(SIDIndex, Domain, Username, Password);
        end;

        {Save the new credentials into the cache
         +only if no existing cache is used
        }
        if (SessionInfo.Flags and CLIENT_CACHECREDS = CLIENT_CACHECREDS) and
           (SessionInfo.Flags and CLIENT_USECACHECREDS <> CLIENT_USECACHECREDS) then
        begin
          fPasswords.FreeIndex(SIDIndex);
          Items[SIDIndex] := TPasswordList.CreatePassEntry(
             SessionInfo.Domain, SessionInfo.UserName, SessionInfo.Password);

          Domain   := SessionInfo.Domain;
          Username := SessionInfo.UserName;
          Password := SessionInfo.Password;

          SessionInfo.Password := FILL_PASSWORD;
        end;
        //else ignore CLIENT_USECACHECREDS

      finally
        SessionInfo.Password := FILL_PASSWORD;
        fPasswords.UnlockList;
      end;

      try
        LSA := TJwSecurityLsa.Create('StartApplicationsAsAdmin');
        try
          pLogonData := JwCreate_MSV1_0_INTERACTIVE_LOGON(MsV1_0InteractiveLogon, Domain, Username, Password, Authlen);
          Password := FILL_PASSWORD;
          try
            Source.SourceName := 'UACinXP';
            AllocateLocallyUniqueID(Source.SourceIdentifier);
            AddGroups := TJwSecurityIDList.Create(True);

            try
              SID:=JwGetLogonSID(Token);
              SID.Attributes:=SE_GROUP_MANDATORY or
                              SE_GROUP_ENABLED or
                              SE_GROUP_ENABLED_BY_DEFAULT or
                              SE_GROUP_LOGON_ID;
              AddGroups.Add(SID);

              SID := TJwSecurityID.Create(JwAdministratorsSID);
              SID.Attributes:=SE_GROUP_MANDATORY or
                              SE_GROUP_ENABLED or
                              SE_GROUP_ENABLED_BY_DEFAULT;
              AddGroups.Add(SID);
//              SID:=TJwSecurityID.Create(JwLocalGroupSID);
//              SID.Attributes:=SE_GROUP_MANDATORY or
//                              SE_GROUP_ENABLED or
//                              SE_GROUP_ENABLED_BY_DEFAULT;
//              AddGroups.Add(SID);
//              MessageBox(0, 'User is now to be logged on...', 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
              try
                Lsa.LsaLogonUser('UserAsAdmin', JwaWindows.Interactive, MSV1_0_PACKAGE_NAME,
                                    pLogonData, Authlen, AddGroups, Source, Pointer(ProfBuffer), ProfBufferLen,
                                    TokenLuid, NewToken, QuotaLimits, SubStatus);
              except
                on E: Exception do
                begin
                  ErrorResult := ERROR_LOGONUSERFAILED;
                  Log.Log('Logon of User '+Username+' failed',lsError);

                  abort;
                end;
              end;

              {Free cache password
              }
              if (SessionInfo.Flags and CLIENT_CLEARCACHE = CLIENT_CLEARCACHE) and
                (SessionInfo.Flags and CLIENT_USECACHECREDS <> CLIENT_USECACHECREDS) then
                 fPasswords.FreeIndex(SIDIndex);

            {  if Save then
              begin
                if Pass='' then
                  with fPasswords.LockList do
                  try
                    if Items[SIDIndex]=nil then
                      Items[SIDIndex] := EMPTYPASSWORD;
                  finally
                    fPasswords.UnlockList;
                  end
                else
                begin
                  if Length(Pass) mod EncryptionBlockSize = 0 then
                    EncryptLength:=Length(Pass)
                  else
                    EncryptLength:=((Length(Pass) div 8)+1)*8;
                  EncryptedPassword:=AllocMem(4+EncryptLength);//4 bytes length
                  Move(PCardinal(Cardinal(Pass)-4)^, EncryptedPassword^, 4+Length(Pass));
                  TJwEncryptionApi.CryptProtectMemory(Pointer(Cardinal(EncryptedPassword)+4), EncryptLength, [pmSameProcess]);
                  with fPasswords.LockList do
                  try
                    if Items[SIDIndex]=nil then
                      Items[SIDIndex]:=EncryptedPassword;
                  finally
                    fPasswords.UnlockList;
                  end;
                end;
              end;  }
            finally
              Password := 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX';
              AddGroups.Free;
            end;
            try
//              MessageBox(0, PChar('User logged on successfully '+Inttostr(NewToken.TokenHandle)), 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
              ZeroMemory(@Profinfo, Sizeof(Profinfo));
              ProfInfo.dwSize:=Sizeof(Profinfo);
              ProfInfo.dwFlags:=1;
              //Profinfo.lpProfilePath:=PWideChar(Widestring(ProfBuffer.ProfilePath.Buffer));
              Profinfo.lpUserName:='UserAsAdmin';
              if not LoadUserProfile(NewToken.TokenHandle, Profinfo) then
              begin
                //!!LogEvent('LoadUserProfile failed', ltError);
                ErrorResult := ERROR_LOADUSERPROFILE;
                NewToken.Free;
                LogAndRaiseLastOsError(Log, ClassName, 'StartApplication::(winapi)LoadUserProfile', 'ElevationHandler.pas');
              end;
//              MessageBox(0, 'Process is to be created...', 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
              ZeroMemory(@Startinfo, sizeof(Startinfo));
              Startinfo.cb:=Sizeof(Startinfo);
//              Startinfo.dwFlags:=STARTF_USESHOWWINDOW;
//              Startinfo.wShowWindow:=SW_SHOW;
              Startinfo.lpDesktop:='Winsta0\default';
              //if GetTokenInformation(Token.TokenHandle, TokenSessionId, @SeId1, 4, EncryptLength) then //EncryptLength is just a dummy here
              //  SetTokenInformation(NewToken.TokenHandle, TokenSessionId, @SeId1, 4);
              NewToken.TokenSessionId := Token.TokenSessionId;

              CreateEnvironmentBlock(@Envirblock, NewToken.TokenHandle, false);
              try
                Job := CreateJobObject(nil, nil);
                if not CreateProcessAsUserW(NewToken.TokenHandle, PWideChar(ApplicationPath), nil, nil, nil,
                                           True, CREATE_NEW_CONSOLE or CREATE_SUSPENDED or CREATE_UNICODE_ENVIRONMENT or CREATE_BREAKAWAY_FROM_JOB, EnvirBlock, nil, Startinfo, ProcInfo)  then
                begin
                  ErrorResult := ERROR_CREATEPROCESSASUSER_FAILED;

                  CloseHandle(Job);
                  UnloadUserProfile(NewToken.TokenHandle, Profinfo.hProfile);
                  NewToken.Free;


                  LogAndRaiseLastOsError(Log, ClassName, 'StartApplication::(winapi)CreateProcessAsUserW', 'ElevationHandler.pas');
                end;
              finally
                DestroyEnvironmentBlock(Envirblock);
              end;
              {!!if not AssignProcessToJobObject(Job, ProcInfo.hProcess) then
                LogEvent('Assignment to job failed '+SysErrorMessage(GetLastError)+' '+inttostr(Job)+' '+inttostr(ProcInfo.hProcess), ltError);
              }
              ResumeThread(ProcInfo.hThread);
              CloseHandle(Procinfo.hThread);
              CloseHandle(ProcInfo.hProcess);
              //!!LogEvent('Creation of process '+AppToStart+' as user '+User+' successful');
//              MessageBox(0, PChar('Process created successfully: '+inttostr(ProcInfo.hProcess)), 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
              UnloadProfThread.Add(Job, Profinfo.hProfile, NewToken);
            finally
              LsaFreeReturnBuffer(ProfBuffer);
            end;
          finally
            LocalFree(Cardinal(pLogonData));
          end;
        finally
          LSA.Free;
        end;
      finally
      end;
    finally
      Token.Free;
    end;
  except
    on E: EJwsclWinCallFailedException do
    begin
      //!!LogEvent('Error in TService1.StartApp: '+E.ClassName+': '+E.Message+SysErrorMessage(E.LastError), ltError);
      Log.Exception(E);
      if TSessionPipe.IsValidPipe(ServerPipe) then
        ServerPipe.SendServerProcessResult(ErrorResult, E.LastError);
    end;
    on E: Exception do
    begin
      //if not (E is EAbort) then
      begin
        Log.Exception(E);

        if TSessionPipe.IsValidPipe(ServerPipe) then
          ServerPipe.SendServerProcessResult(ErrorResult, GetLastError());
        //!!LogEvent('Error in TService1.StartApp: '+E.ClassName+': '+E.Message, ltError);
      end;
    end;
  end;
end;

end.
