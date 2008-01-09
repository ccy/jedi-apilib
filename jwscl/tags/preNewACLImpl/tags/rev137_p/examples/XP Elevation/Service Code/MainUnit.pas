unit MainUnit;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
  JwaWindows, JwsclToken, JwsclLsa, JwsclCredentials, JwsclDescriptor, JwsclDesktops,
  JwsclExceptions, JwsclSID, JwsclKnownSID, JwsclEncryption, JwsclTypes;

type
  TLogType=(ltInfo, ltError);

  TService1 = class(TService)
    procedure ServiceExecute(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private declarations }
    fStopEvent:           THandle;
    fThreadsStopped:      THandle;
    fLogCriticalSection:  TCriticalSection;
    fLogFile:             Textfile;
    fStopped:             boolean;
//    fDesktop:             TJwSecurityDesktop;
    fAllowedSIDs:         TJwSecurityIdList;
    fPasswords:           TThreadList;
    procedure InitAllowedSIDs;
    procedure SetStopped(const Value: boolean);
    function MayUserBeElevated(User: TJwSecurityID): boolean;
    function AskCredentials(Session: Cardinal; AppToStart: String; UserAndDomain: string; out Password: string; out Save: boolean): boolean;
  public
    fHReqThreadCount:     Integer;
    function GetServiceController: TServiceController; override;
    procedure StartApp(AppToStart: String);
    procedure LogEvent(Event: String; EventType: TLogType=ltInfo);
    { Public declarations }
    property StopEvent: THandle read fStopEvent;
    property ThreadsStopped: THandle read fThreadsStopped;
    property Stopped: boolean read fStopped write SetStopped;
  end;

const MessageboxCaption= 'XP Elevation';
      CredApplicationKey='CredentialsApplication';
      LogFileKey='Log';

var
  Service1: TService1;

implementation
uses ThreadUnit, HandleRequestThread, Registry;
{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  Service1.Controller(CtrlCode);
end;

function TService1.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TService1.LogEvent(Event: String; EventType: TLogType=ltInfo);
begin
  EnterCriticalSection(fLogCriticalSection);
  try
    case EventType of
      ltInfo: Writeln(fLogfile, DateTimeToStr(Now)+' '+Event);
      ltError: Writeln(fLogfile, DateTimeToStr(Now), ' ERROR: ', Event);
    end;
    Flush(fLogfile);
  finally
    LeaveCriticalSection(fLogCriticalSection);
  end;
end;

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

function TService1.AskCredentials(Session: Cardinal; AppToStart: string; UserAndDomain: string; out Password: string; out Save: boolean): boolean;
var StartInfo: STARTUPINFOA; Token: TJwSecurityToken;
  procedure InitStartInfo;
  begin
    ZeroMemory(@StartInfo, Sizeof(StartInfo));
    StartInfo.cb:=Sizeof(StartInfo);
    StartInfo.lpDesktop:='WinSta0\Default';
  end;

  procedure CreateToken;
  var ProcessToken: TJwSecurityToken; RestrGroups: TJwSecurityIdList; RestrPrivs: TJwPrivilegeSet;
  begin
    ProcessToken:=TJwSecurityToken.CreateTokenByProcess(GetCurrentProcess, TOKEN_ALL_ACCESS);
    try
      RestrGroups:=TJwSecurityIdList.Create(false);
      try
        RestrPrivs:=ProcessToken.GetTokenPrivileges;
        try
          ProcessToken.TokenSessionId:=Session;
          Token:=ProcessToken.CreateRestrictedToken(TOKEN_ALL_ACCESS,
                    0, RestrGroups, RestrPrivs, nil);
        finally
          RestrPrivs.Free;
        end;
      finally
        RestrGroups.Free;
      end;
    finally
      ProcessToken.Free;
    end;
  end;
const StrLenCancelled = Cardinal(-1);
      SaveBit         = Cardinal(1 shl 31);
var ProcInfo: PROCESS_INFORMATION; Ar: Array[0..1] of THandle; Dummy: Cardinal;
    ReadPipe, WritePipe: THandle; Loc: Cardinal; Desc: TJwSecurityDescriptor; SecAttr: LPSECURITY_ATTRIBUTES;
    CredApp: String; SeId: Cardinal;
begin
  Result:=False;
  InitStartInfo;
  ZeroMemory(@ProcInfo, Sizeof(ProcInfo));
  CreateToken;
  try
    Desc:=TJwSecurityDescriptor.CreateDefaultByToken(Token);
    try
    //  SecAttr:=LPSECURITY_ATTRIBUTES(Desc.Create_SA(false));
      SecAttr:=nil;
      try
        CredApp:=RegGetFullPath(CredApplicationKey);
        //The application will immediately stop execution because it expects two parameters, but we need its exit code
        if not CreateProcessAsUser(Token.TokenHandle, PChar(CredApp), nil,
               SecAttr, SecAttr, True, CREATE_NEW_CONSOLE, nil, nil, StartInfo, ProcInfo) then
        begin
          LogEvent('Could not create the process asking for credentials: '+SysErrorMessage(GetLastError), ltError);
          Abort;
        end
        else
        begin
          CloseHandle(ProcInfo.hThread);
          Ar[0]:=fStopEvent;
          Ar[1]:=ProcInfo.hProcess;
          //This function is called in a separate thread, so there is no need for ServiceThread.ProcessRequests!
//          while MsgWaitForMultipleObjects(2, @Ar[0], false, INFINITE, QS_ALLINPUT)=WAIT_OBJECT_0+2 do
//            ServiceThread.ProcessRequests(false);
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
            LogEvent('Could not create the process asking for credentials the second time: '+SysErrorMessage(GetLastError), ltError);
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
    end;
  finally
    Token.Free;
  end;
end;

function TService1.MayUserBeElevated(User: TJwSecurityID): boolean;
begin
  result:=fAllowedSIDs.FindSid(User)<>-1;
end;

const EmptyPass = Pointer(-1);

procedure TService1.StartApp(AppToStart: string);
var Pass, User, Domain: string; LSA: TJwSecurityLsa;

    plogonData : PMSV1_0_INTERACTIVE_LOGON; Authlen: Cardinal;
    Source: TTokenSource; ProfBuffer: PMSV1_0_INTERACTIVE_PROFILE; ProfBufferLen: Cardinal;
    Token, NewToken: TJwSecurityToken; TokenLuid: TLUID; QuotaLimits: QUOTA_LIMITS; SubStatus: integer;

    ProfInfo: PROFILEINFO; AddGroups: TJwSecurityIDList; SID: TJwSecurityID;

    EnvirBlock: Pointer; StartInfo: STARTUPINFO; ProcInfo: PROCESS_INFORMATION;
    Job: THandle; EncryptLength: Cardinal; SIDIndex: Integer;

    SeId1, SeId2, SeId3: Cardinal; EncryptedPassword: Pointer; Save: Boolean;

const EncryptionBlockSize = 8;
begin
  try
    Token:=TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);
    try
      SID:=Token.TokenUser;
      try
        SIDIndex:=fAllowedSIDs.FindSid(SID);
        If SIDIndex=-1 then
        begin
          LogEvent('Elevation of user '+SID.AccountName['']+' for application '+AppToStart+' not allowed.');
//          //we do not want to wait for the user to abort this dialog
//          WTSSendMessage(WTS_CURRENT_SERVER_HANDLE, WTS_CURRENT_SESSION, 'XP Elevation', StrLen('XP Elevation'),
//                         PChar('The user '+SID.AccountName['']+' is not allowed to be elevated.'), Length('The user '+SID.AccountName['']+' is not alloqed to be elevated.'),
//                         MB_OK, 0, Dummy, false);
          MessageBox(0, PChar('The user '+SID.AccountName['']+' is not allowed to be elevated.'), MessageboxCaption, MB_SERVICE_NOTIFICATION or MB_OK);
          abort;
        end;
        User:=SID.AccountName[''];
        try
          Domain:=SID.GetAccountDomainName('');
        except
          Domain:='local';
        end;

      finally
        SID.Free;
      end;
 //     Token.TokenSessionId;
      TJwSecurityToken.RevertToSelf;
      Save:=False;
  //    Token.ConvertToPrimaryToken(TOKEN_ALL_ACCESS);
      with fPasswords.LockList do
      try
        EncryptedPassword:=Items[SIDIndex];
        if EncryptedPassword=nil then
        begin
          LogEvent('Credentials for user '+User+' are asked');

          //I have no idea why we can't use Token.TokenSessionId, but this throws an exception
          if not GetTokenInformation(Token.TokenHandle, TokenSessionId, @SeId1, 4, SeId2) then
            LogEvent(SysErrorMessage(GetLastError));
          if not AskCredentials(SeId1, AppToStart, User+'@'+Domain, Pass, Save) then
          begin
            LogEvent('Credentials prompt for '+AppToStart+' aborted by user');
            exit;
          end
        end
        else
        begin
          LogEvent('Credentials for user '+User+' are retrieved from the cache');
          if PCardinal(EncryptedPassword)=EmptyPass then
            Pass:=''
          else
          begin
            //EncryptedPassword is a pointer to a structure containing an
            //4 bytes integer (the length of the decrypted password) and the encrypted password
            if PCardinal(EncryptedPassword)^ mod EncryptionBlockSize = 0 then
              SetLength(Pass, PCardinal(EncryptedPassword)^)
            else
              SetLength(Pass, ((PCardinal(EncryptedPassword)^ div EncryptionBlockSize)+1)*EncryptionBlockSize);
            System.Move(PCardinal(Cardinal(EncryptedPassword)+4)^, Pointer(Pass)^, Length(Pass));
            TJwEncryptionApi.CryptProtectMemory(Pointer(Pass), Length(Pass), [pmSameProcess]);
            SetLength(Pass, PCardinal(EncryptedPassword)^);
          end;
        end;
      finally
        fPasswords.UnlockList;
      end;
      try
        LSA:=TJwSecurityLsa.Create('StartApplicationsAsAdmin');
        try
//          MessageBox(0, 'Lsa created successfully', 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
          pLogonData:=JwCreate_MSV1_0_INTERACTIVE_LOGON(MsV1_0InteractiveLogon, Domain, User, Pass, Authlen);
          try
            Source.SourceName:='UACinXP';
            AllocateLocallyUniqueID(Source.SourceIdentifier);
            AddGroups:=TJwSecurityIDList.Create(True);
            try
              SID:=JwGetLogonSID(Token);
              SID.Attributes:=SE_GROUP_MANDATORY or
                              SE_GROUP_ENABLED or
                              SE_GROUP_ENABLED_BY_DEFAULT or
                              SE_GROUP_LOGON_ID;
              AddGroups.Add(SID);
              SID:=TJwSecurityID.Create(JwAdministratorsSID);
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
                  LogEvent('Logon of User '+User+' failed');
                  MessageBox(0, 'Invalid logon data (e.g. wrong password)', MessageboxCaption, MB_SERVICE_NOTIFICATION or MB_OK);
                  abort;
                end;
              end;
              if Save then
              begin
                if Pass='' then
                  with fPasswords.LockList do
                  try
                    if Items[SIDIndex]=nil then
                      Items[SIDIndex]:=EmptyPass;
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
              end;
            finally
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
                LogEvent('LoadUserProfile failed', ltError);
                NewToken.Free;
                Abort;
              end;
//              MessageBox(0, 'Process is to be created...', 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
              ZeroMemory(@Startinfo, sizeof(Startinfo));
              Startinfo.cb:=Sizeof(Startinfo);
//              Startinfo.dwFlags:=STARTF_USESHOWWINDOW;
//              Startinfo.wShowWindow:=SW_SHOW;
              Startinfo.lpDesktop:='Winsta0\default';
              if GetTokenInformation(   Token.TokenHandle, TokenSessionId, @SeId1, 4, EncryptLength) then //EncryptLength is just a dummy here
                SetTokenInformation(NewToken.TokenHandle, TokenSessionId, @SeId1, 4);
              CreateEnvironmentBlock(@Envirblock, NewToken.TokenHandle, false);
              try
                Job:=CreateJobObject(nil, nil);
                if not CreateProcessAsUser(NewToken.TokenHandle, PChar(AppToStart), nil, nil, nil,
                                           True, CREATE_NEW_CONSOLE or CREATE_SUSPENDED or CREATE_UNICODE_ENVIRONMENT or CREATE_BREAKAWAY_FROM_JOB, EnvirBlock, nil, Startinfo, ProcInfo)  then
                begin
                  LogEvent('Creation of process '+AppToStart+' failed'+SysErrorMessage(getLastError), ltError);
                  CloseHandle(Job);
                  UnloadUserProfile(NewToken.TokenHandle, Profinfo.hProfile);
                  NewToken.Free;
                  abort;
                end;
              finally
                DestroyEnvironmentBlock(Envirblock);
              end;
              if not AssignProcessToJobObject(Job, ProcInfo.hProcess) then
                LogEvent('Assignment to job failed '+SysErrorMessage(GetLastError)+' '+inttostr(Job)+' '+inttostr(ProcInfo.hProcess), ltError);
              ResumeThread(ProcInfo.hThread);
              CloseHandle(Procinfo.hThread);
              CloseHandle(ProcInfo.hProcess);
              LogEvent('Creation of process '+AppToStart+' as user '+User+' successful');
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
        ZeroMemory(Pointer(Pass), length(Pass));
      end;
    finally
      ZeroMemory(Pointer(Pass), Length(Pass));
      Token.Free;
    end;
  except
    on E: EJwsclWinCallFailedException do
    begin
      LogEvent('Error in TService1.StartApp: '+E.ClassName+': '+E.Message+SysErrorMessage(E.LastError), ltError);
    end;
    on E: Exception do
      if not (E is EAbort) then
      begin
        LogEvent('Error in TService1.StartApp: '+E.ClassName+': '+E.Message, ltError);
      end;
  end;
end;

procedure TService1.InitAllowedSIDs;
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
  fPasswords:=TThreadList.Create;
  fPasswords.LockList.Count:=fAllowedSIDs.Count;
  fPasswords.UnlockList;
end;

procedure TService1.ServiceExecute(Sender: TService);
var Pipe: THandle; OvLapped: OVERLAPPED; ar: array[0..1] of THandle;
    AppName: String; PipeSize: Cardinal; Descr: TJwSecurityDescriptor; SecAttr: PSECURITY_ATTRIBUTES;
    i: integer;
begin
  try
    AssignFile(fLogFile, RegGetFullPath(LogFileKey));
    if FileExists(RegGetFullPath(LogFileKey)) then
      Append(fLogFile)
    else
      Rewrite(fLogFile);
    InitializeCriticalSection(fLogCriticalSection);
  except
    on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), MessageboxCaption, MB_SERVICE_NOTIFICATION or MB_OK);
    end;
  end;
  try
//    MessageBox(0, 'Started', 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
    LogEvent('Started');
//    Descr:=TJwSecurityDescriptor.Create;
//    try
//      fDesktop:=TJwSecurityDesktop.CreateDesktop(nil, true, 'UACinXPAskCredentialsDesktop', [], false, GENERIC_ALL, Descr);
//    finally
//      Descr.Free;
//    end;
    fStopEvent:=CreateEvent(nil, true, false, nil);
    fThreadsStopped:=CreateEvent(nil, true, true, nil);
    try
      try
        SecAttr:=nil;
        ZeroMemory(@OvLapped, sizeof(OvLapped));
        OvLapped.hEvent:=CreateEvent(nil, false, false, nil);
        try
          Descr:=TJwSecurityDescriptor.Create;
          try
            Descr.DACL:=nil;
            SecAttr:=Descr.Create_SA(False);
            Pipe:=CreateNamedPipe('\\.\pipe\XPElevationPipe', PIPE_ACCESS_INBOUND or FILE_FLAG_OVERLAPPED, PIPE_WAIT, PIPE_UNLIMITED_INSTANCES, 0, 0, 0, LPSECURITY_ATTRIBUTES(SecAttr));
          finally
            Descr.Free;
          end;
          if Pipe=INVALID_HANDLE_VALUE then
          begin
//            MessageBox(0, PChar('Error occured during pipe creation: '+SysErrorMessage(GetLastError)), 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
            LogEvent('Error occured during pipe creation: '+SysErrorMessage(GetLastError), ltError);
            abort;
          end;
//        MessageBox(0, PChar('Pipe ('+inttostr(Pipe)+') created successfully'), 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
          InitAllowedSIDs;
          try
            UnloadProfThread := TUnloadProfThread.Create;
            ar[0]:=fStopEvent;
            ar[1]:=OvLapped.hEvent;
            ConnectNamedPipe(Pipe, @OvLapped);
            while MsgWaitForMultipleObjects(2, @ar[0], false, INFINITE, QS_ALLINPUT)=WAIT_OBJECT_0+2 do
              ServiceThread.ProcessRequests(False);
            while not Stopped do
            begin
              with THandleRequestThread.Create(True) do
              begin
                FreeOnTerminate:=True;
                PipeHandle:=Pipe;
                Resume;
              end;
              Pipe:=CreateNamedPipe('\\.\pipe\XPElevationPipe', PIPE_ACCESS_INBOUND or FILE_FLAG_OVERLAPPED, PIPE_WAIT, PIPE_UNLIMITED_INSTANCES, 0, 0, 0, LPSECURITY_ATTRIBUTES(SecAttr));
              if Pipe=INVALID_HANDLE_VALUE then
              begin
//                MessageBox(0, PChar('Error occured during pipe creation: '+SysErrorMessage(GetLastError)), 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
                LogEvent('Error occured during pipe creation: '+SysErrorMessage(GetLastError), ltError);
                abort;
              end;
              ConnectNamedPipe(Pipe, @OvLapped);
              while MsgWaitForMultipleObjects(2, @ar[0], false, INFINITE, QS_ALLINPUT and not QS_POSTMESSAGE or QS_ALLPOSTMESSAGE)=WAIT_OBJECT_0+2 do
                ServiceThread.ProcessRequests(False);
            end;
          finally
            CloseHandle(Pipe);
            PostQueuedCompletionStatus(UnloadProfThread.IOCompletionPort, 0, MESSAGE_FROM_SERVICE, nil);
            WaitForSingleObject(fThreadsStopped, 10000);
//            Sleep(1000);
            fAllowedSIDs.Free;
            with fPasswords.LockList do
              for i:=0 to Count-1 do
                if (Items[i]<>nil) and (Items[i]<>EmptyPass) then
                  FreeMem(Items[i]);
            fPasswords.Free;
            UnloadProfThread.WaitFor;
            UnloadProfThread.Free;
          end;
        finally
          if Assigned(SecAttr) then
          begin
            TJwSecurityDescriptor.Free_SD(PSECURITY_DESCRIPTOR(SecAttr.lpSecurityDescriptor));
            FreeMem(SecAttr);
          end;
          CloseHandle(OvLapped.hEvent);
        end;
      finally
//        fDesktop.Free;
//        MessageBox(0, 'Ending', 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
        LogEvent('Ending');
      end;
    finally
      CloseHandle(fStopEvent);
      CloseHandle(fThreadsStopped);
    end;
  finally
    DeleteCriticalSection(fLogCriticalSection);
    CloseFile(fLogFile);
  end;
end;

procedure TService1.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  Self.Stopped:=True;
end;

procedure TService1.SetStopped(const Value: boolean);
begin
  FStopped := Value;
  if Value then
    SetEvent(fStopEvent);
end;

initialization
  JwInitWellknownSIDs;

end.
