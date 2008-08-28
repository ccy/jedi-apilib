
unit ElevationHandler;

interface
uses
  Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs, Math, ComObj,
  JwaWindows, JwsclToken, JwsclLsa, JwsclCredentials, JwsclDescriptor, JwsclDesktops,
  JwsclExceptions, JwsclSID, JwsclAcl,JwsclKnownSID, JwsclEncryption, JwsclTypes,
  JwsclProcess, JwsclComUtils, XPElevationCommon, JwaVista, jwsclwinstations,
  JwsclUtils,
  SessionPipe, JwsclLogging, uLogging, ThreadedPasswords, JwsclCryptProvider,
  //MappedStreams,
  JwsclStreams,
  UserProfileImage,
  JwsclStrings;

const
  //response timeout with credential app
  TIMEOUT = {$IFNDEF DEBUG}60 * 30 * 1000;{$ELSE}INFINITE;{$ENDIF}
  //timeout of user response in cred app
  USERTIMEOUT = {$IFNDEF DEBUG}60 * 30 * 1000;{$ELSE}INFINITE;{$ENDIF}

  //maximum of logon attempts until connection is closed
  MAX_LOGON_ATTEMPTS = {$IFNDEF DEBUG} 3;{$ELSE}INFINITE;{$ENDIF}




type

  {TXPElevationStruct is a record that is sent by
   the Client COM.
   Most of this data is sen
  }
  TXPElevationStruct = record
    Size : DWORD; //size of this structure to compare

    ParentWindow : HWND; //window that should be highlited.

    ApplicationName  : array[0..MAX_PATH] of WideChar; //app absolute path
    CurrentDirectory : array[0..MAX_PATH] of WideChar; //

    {Parameters.
     Between Service and COM Client this is always just a nil pointer.
     Its filled with the parameters by the service automatically.
    }
    Parameters : WideString;

    ControlFlags  : DWORD; //Flags that controls the elevation prompt  XPCTRL_XXX in XPElevation.pas
    StartupInfo : TStartupInfoW; //Startup info for CreateProcess

    ParameterCharCount : DWORD; //Count of chars of parameter length

    Reserved1,   //not used
    Reserved2 : DWORD; //not used
  end;


type
  PProcessJobData = ^TProcessJobData;
  {TProcessJobData contains information about a process in a job.
  It defines the process token and the loaded profile
  }
  TProcessJobData = record
    UserToken : TJwSecurityToken; //process token
    UserProfile : TJwProfileInfo; //user's profile. Must be unloaded.
  end;

  {TElevationHandler starts the credentials app and after thet
   the elevates the app} 
  TElevationHandler = class(TObject)
  private
  protected
    //pipe to credentials app
    ServerPipe : TServerSessionPipe;

    //credential list - same as the one from TXPService
    fPasswords   : TCredentialsList;
    //service shuts down
    fStopEvent : THandle;
    //service stopped? - pointer to XPService.Stopped
    fStopState : PBoolean;

    
    function GetStopState : Boolean;
  public
    constructor Create(
      const AllowedSIDs: TJwSecurityIdList;
      const Passwords  : TCredentialsList;
      const StopEvent  : THandle;
      const StopState : PBoolean);
    destructor Destroy; override;

    {Runs the appropriate application elevated.}
    function StartApplication(const ElevationStruct : TXPElevationStruct; out PID : DWORD) : HRESULT;

    {Runs credential app and returns true on success.}
    function AskCredentials(const ClientPipeUserToken: TJwSecurityToken;
        const SessionID : Integer;
        out LastProcessID: TJwProcessId;
        var SessionInfo : TSessionInfo): Boolean;

    property StopState : Boolean read GetStopState;
  end;


implementation
uses Registry, MainUnit;

procedure RandomizePasswdA(var S : AnsiString);
var i,c : Integer;
begin
  for i := 1 to Length(S) do
  begin
    S[i] := Char(random(266));
  end;
  S := '';
end;

procedure RandomizePasswdW(var S : WideString);
var i,c : Integer;
begin
  for i := 1 to Length(S) do
  begin
    S[i] := WideChar(random(266));
  end;
  S := '';
end;






{ TElevationHandler }

function TElevationHandler.AskCredentials(
  const ClientPipeUserToken: TJwSecurityToken;
  const SessionID : Integer;
  out LastProcessID: TJwProcessId;
  var SessionInfo: TSessionInfo): boolean;

{Checks whether the given logon is possible}  
function CheckLogonUser(const SessionInfo : TSessionInfo) : Boolean;
var
  Token : TJwSecurityToken;
  Domain,
  UserName,
  Password : WideString;
  Log : IJwLogClient;
  i : Integer;
begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,'CheckLogonUser','ElevationHandler.pas','');
  
  if (SessionInfo.Flags and CLIENT_USECACHECREDS = CLIENT_USECACHECREDS) and
     (SessionInfo.Password = '') then
  begin
      Log.Log('Credentials for user '+Username+' are retrieved from the cache');

    try
        Log.Log('Retrieving cached credentials');
      fPasswords.GetBySession(SessionID, Domain, Username, Password);
    except
    on E : Exception do
       Log.Exception(E);
    end;
  end
  else
  begin
    UserName := SessionInfo.UserName;
    Domain := SessionInfo.Domain;
    Password := SessionInfo.Password;
  end;

  {LogonUser API mentions that we
    should use SSPI instead: http://support.microsoft.com/kb/180548
  But sample text tells us:
  Note LogonUser Win32 API does not require TCB privilege in Microsoft Windows Server 2003, however,
   for downlevel compatibility, this is still the best approach.

  On Windows XP, it is no longer required that a process have the SE_TCB_NAME privilege in
  order to call LogonUser. Therefore, the simplest method to validate a user's credentials
  on Windows XP, is to call the LogonUser API.

  }

  for i := 1 to 2 do
  begin
    try
      try
        Token := TJwSecurityToken.CreateLogonUser(UserName,Domain,Password,LOGON32_LOGON_NETWORK,LOGON32_PROVIDER_DEFAULT);
      except
        on E : EJwsclSecurityException do
        begin
          Log.Exception(E);
          if (i = 2) then
            if (E.LastError = ERROR_INVALID_PASSWORD) or
               (E.LastError = ERROR_LOGON_FAILURE) then
            begin
              Log.Log(lsError,'Logon user check failed.');
              result := false;
              exit;
            end
            else
              raise;
        end
        else //except
          raise;
      end;
    finally
      RandomizePasswdW(Password);
    end;
    break;
  end;

  Token.Free;
  result := true;
end;

{Creates a token the has SYSTEM power but less privileges.
First it tries to create its own token then it uses
CreateRestrictedToken
}
function GetRestrictedSYSTEMToken(UserToken : TJwSecurityToken) : TJwSecurityToken;

function CreateToken(UserToken : TJwSecurityToken) : TJwSecurityToken;
var
  SYSToken : TJwSecurityToken;
  ObjectAttributes: TObjectAttributes;
  aLuid,
  AuthenticationId: TLUID;

  NewGroups,
  UserGroups: TJwSecurityIdList;

  Privileges: TJwPrivilegeSet;
  TokenSource : TTokenSource;

  DefaultDACL : TJwDAccessControlList;

  StartInfo : TStartupInfo;
  ProcInfo : TProcessInformation;

  Sid,
  LogonSid,
  User, Owner,
  Group : TJwSecurityId;
  Stats : TJwSecurityTokenStatistics;

  C1 : DWORD;
  C2 : TTokenElevationType;
begin
  JwInitWellKnownSIDs;

  //get SYSTEM token
  SYSToken := TJwSecurityToken.CreateTokenEffective(MAXIMUM_ALLOWED);
  TJwAutoPointer.Wrap(SYSToken);




  Privileges := TJwPrivilegeSet.Create();
  //TJwAutoPointer.Wrap(Privileges);

  try
    Privileges.AddPrivilege(SE_CHANGE_NOTIFY_NAME);
    Privileges.AddPrivilege(SE_ASSIGNPRIMARYTOKEN_NAME);

    DefaultDACL := UserToken.GetTokenDefaultDacl;
    TJwAutoPointer.Wrap(DefaultDACL);


    ZeroMemory(@ObjectAttributes, sizeof(ObjectAttributes));
    ObjectAttributes.Length := sizeof(ObjectAttributes);

    Owner := SYSToken.GetTokenOwner;
    TJwAutoPointer.Wrap(Owner);

    User := SYSToken.GetTokenUser;
    TJwAutoPointer.Wrap(User);

    Group := SYSToken.GetPrimaryGroup;
    TJwAutoPointer.Wrap(Group);

    //create our own logon id
    //this logon ID must be registered first! (don't know how yet)
    //AllocateLocallyUniqueId(AuthenticationId);

    Stats := SYSToken.GetTokenStatistics;
    TJwAutoPointer.Wrap(Stats);

    //get any logon session we want
    {if not GetSession(UserToken, AuthenticationId) then
      exit;}
    //get the logon ID from the user token object
    AuthenticationId := Stats.AuthenticationId;
    //GetUserName reads the username from the token logon id rather than token user



    //get default token groups
    //UserGroups := SYSToken.TokenGroups;
    UserGroups := TJwSecurityIdList.Create(true);
    TJwAutoPointer.Wrap(UserGroups);


    AllocateLocallyUniqueId(aLuid);
    Sid := TJwSecurityId.Create(JwFormatString('S-1-5-5-%d-%d',[aLuid.HighPart, aLuid.LowPart]));
    Sid.AttributesType := [sidaGroupMandatory];
    UserGroups.Add(Sid);

   { Sid := TJwSecurityId.Create('S-1-5-21-2721915288-875847878-2597518166-513');
    Sid.AttributesType := [sidaGroupMandatory];
    UserGroups.Add(Sid);   }
  // UserGroups.Delete(3);


   { JwLocalServiceSID.AttributesType := [sidaGroupOwner];
    UserGroups.Add(JwLocalServiceSID);}

  {  JwUsersSID.AttributesType := [sidaGroupMandatory];
    UserGroups.Add(JwUsersSID);  }

  {  JwLocalSystemSID.AttributesType := [sidaGroupMandatory];
    UserGroups.Add(JwLocalSystemSID);}

    JwAdministratorsSID.AttributesType := [sidaGroupOwner];
    UserGroups.Add(JwAdministratorsSID);

    JwWorldSID.AttributesType := [sidaGroupMandatory];
    UserGroups.Add(JwWorldSID);

    JwIntegrityLabelSID[iltHigh].AttributesType := [sidaGroupIntegrity,sidaGroupIntegrityEnabled];
    UserGroups.Add(JwIntegrityLabelSID[iltHigh]);

  {
    Sid := TJwSecurityId.Create('S-1-5-5-0-10140476');
    Sid.AttributesType := [sidaGroupMandatory];
    UserGroups.Add(Sid);//S-1-5-1-1-1'));      }

  {  JwIntegrityLabelSID[iltSYSTEM].AttributesType := [sidaGroupIntegrity, sidaGroupIntegrityEnabled];
    UserGroups.Add(JwIntegrityLabelSID[iltSYSTEM]);
   }
    //add terminal server user (just for testing)
   { Sid := TJwSecurityId.Create('S-1-5-13');
    Sid.AttributesType := [sidaGroupMandatory];
    UserGroups.Add(Sid);//S-1-5-1-1-1'));          }

    //add unknown Sid
  {  Sid := TJwSecurityId.Create('S-1-5-5-0-2827688');
    //Sid.AttributesType := [sidaGroupMandatory];
    Sid.AttributesType := [sidaGroupMandatory,sidaGroupLogonId];
    UserGroups.Add(Sid);      }

    {  UserGroups.Delete(13);
    UserGroups.Delete(12);
    UserGroups.Delete(2);   }
    //ShowMessage(UserGroups.GetText(true));

    //JwLocalSystemSID.AttributesType := [sidaGroupOwner];
    //UserGroups.Add(JwLocalSystemSID);

    ZeroMemory(@TokenSource, sizeof(TokenSource));
    //TokenSource.SourceName := 'CTTest'; //CreateTokenTest identifier name
    //AllocateLocallyUniqueId(TokenSource.SourceIdentifier); //any luid that defines us
	
	Nach Kai muss hier der Sourcename und die TokenLUID die gleiche sein, wie vom
	User, damit Winlogon den neuen Prozess schließt.
	
	TokenSource.SourceName := 
	
    hier müssen wir den csrss.exe Prozess personifzieren, damit wir das SE_CREATE_TOKEN_NAME
	auf jeden Fall setzen können


    JwEnablePrivilege(SE_TCB_NAME,pst_Enable);
    JwEnablePrivilege(SE_CREATE_TOKEN_NAME,pst_Enable);



    try
      result := TJwSecurityToken.CreateNewToken(
          TOKEN_ALL_ACCESS,//const aDesiredAccess: TJwAccessMask;
          ObjectAttributes,//const anObjectAttributes: TObjectAttributes;
          AuthenticationId,//const anAuthenticationId: TLUID;
          0,//const anExpirationTime: int64;
          User,//anUser: TJwSecurityId;
          UserGroups,//aGroups: TJwSecurityIdList;
          Privileges,//aPrivileges: TJwPrivilegeSet;
          Owner,//anOwner,
          Group,//aPrimaryGroup: TJwSecurityId;
          DefaultDACL,//aDefaultDACL: TJwDAccessControlList;
          TokenSource //aTokenSource: TTokenSource
          );
    except
      on e : Exception do
      begin
        ShowMessage(E.Message);
        raise;
      end;
    end;
  finally
    Privileges.Free;
  end;


  //Target session ID from user
  result.TokenSessionId := UserToken.TokenSessionId;

  result.PrivilegeEnabled[SE_CHANGE_NOTIFY_NAME] := true;

  //result.TokenIntegrityLevelType := iltHigh;
 { C1 := result.RunElevation;
  c2 := result.ElevationType;}
//  result.TokenIntegrityLevel
end;


begin
  try
    result := CreateToken(UserToken);
  except
    //failsafe
    result := TJwSecurityToken.CreateRestrictedToken(
                UserToken.TokenHandle, //PrevTokenHandle : TJwTokenHandle;
                MAXIMUM_ALLOWED,//const TokenAccessMask: TJwTokenAccessMask;
              {DISABLE_MAX_PRIVILEGE}0,//const Flags: cardinal;
              nil,//const SidsToDisable: TJwSecurityIdList;
              nil,//const PrivilegesToDelete: TJwPrivilegeSet;
              nil//const RestrictedSids: TJwSecurityIdList
              );
  end;
  result.ConvertToPrimaryToken(MAXIMUM_ALLOWED);
end;

{Creates a copy of the user's current user reg key and returns it.}
function CopyUserRegKeyHandle(const UserToken : TJwSecurityToken;
  const TargetProcHandle : THandle) : THandle;
var
  Log : IJwLogClient;
  Key : HKEY;
begin
  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'CopyUserRegKeyHandle','ElevationHandler.pas','');
  UserToken.ImpersonateLoggedOnUser;
  try
    if RegOpenCurrentUser(KEY_ALL_ACCESS, Key) <> ERROR_SUCCESS then
    begin
      result := INVALID_HANDLE_VALUE;
      try
        RaiseLastOSError;
      except
        on E : EOSError do
          Log.Exception(E);
      end;
    end;
  finally
    UserToken.RevertToSelf;
  end;

  if not DuplicateHandle(
        GetCurrentProcess,//__in   HANDLE hSourceProcessHandle,
        key,//__in   HANDLE hSourceHandle,
        TargetProcHandle,//__in   HANDLE hTargetProcessHandle,
        @result, //__out  LPHANDLE lpTargetHandle,
        0,//__in   DWORD dwDesiredAccess,
        false,//__in   BOOL bInheritHandle,
        DUPLICATE_CLOSE_SOURCE or DUPLICATE_SAME_ACCESS//__in   DWORD dwOptions
        ) then
  begin
    result := INVALID_HANDLE_VALUE;
    try
      RaiseLastOSError;
    except
      on E : EOSError do
        Log.Exception(E);
    end;
  end;
end;

var
  StartInfo: STARTUPINFOW;
  ProcInfo: PROCESS_INFORMATION;
    
  Desc: TJwSecurityDescriptor;
  SecAttr: LPSECURITY_ATTRIBUTES;

  CreationFlags,
  LastError : DWORD;

  Token, LToken : TJwSecurityToken;

  AppliationCmdLine,
  PipeName : WideString;

  hPipe : THandle;
  Log : IJwLogClient;

  P : Pointer;

  MaxLoginRepetitionCount : Cardinal;
  LoginRepetitionCount : Integer;

  WaitResult : Integer;
  hUserKey : THandle;

  ChecksumError : Boolean;


begin
  result := false;

  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'AskCredentials','ElevationHandler.pas','');
  {if not Assigned(ClientPipeUserToken) then
    exit;}


  //secure the pipe to the credential app
  Desc :=  TJwAutoPointer.Wrap(TJwSecurityDescriptor.Create).Instance as TJwSecurityDescriptor;
  try
{$IFDEF DEBUG}
    Desc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_ALL, JwWorldSID));
{$ELSE}
    Desc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_ALL, JwLocalSystemSID));
    Desc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], FILE_ALL_ACCESS or SYNCHRONIZE,
    //Desc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], PIPE_ACCESS_DUPLEX or FILE_FLAG_OVERLAPPED or SYNCHRONIZE,
        ClientPipeUserToken.TokenUser, true));
    //Desc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], GENERIC_ALL, JwWorldSID));
{$ENDIF}

    SecAttr := LPSECURITY_ATTRIBUTES(Desc.Create_SA());
    try
      repeat
        PipeName := '\\.\pipe\XPCredentials'+IntToStr(GetCurrentThreadId);

        SetLastError(0);
        //create the pipe
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
        LastError := GetLastError;
        if LastError <> 0 then
        begin
          if LastError <> ERROR_ALREADY_EXISTS then
            Log.Log(lsError, JwFormatStringEx('Pipe creation of "%s" failed with %d',[PipeName, LastError]))
          else
            LogAndRaiseLastOsError(Log, ClassName, 'AskCredentials::(winapi)CreateNamedPipeW', 'ElevationHandler.pas');
        end;
      until hPipe <> 0;

      ServerPipe.Assign(hPipe, TIMEOUT);

    except
      on E : Exception do
      begin
        FreeAndNil(ServerPipe);

        raise;
      end;
    end;
  finally
    TJwSecurityDescriptor.Free_SA(PSecurityAttributes(SecAttr));
  end;

  try
    AppliationCmdLine := Sysutils.WideFormat('"%s" /cred /pipe "%s"',
       [CredentialsAppPath, PipeName]);
{$IFDEF DEBUG}
     AppliationCmdLine := AppliationCmdLine + ' /DEBUG';
     Log.Log('Starting credentials prompt with DEBUG.');
{$ENDIF DEBUG}

    ZeroMemory(@ProcInfo, Sizeof(ProcInfo));
    ZeroMemory(@StartInfo, Sizeof(StartInfo));
    StartInfo.cb:=Sizeof(StartInfo);
    StartInfo.lpDesktop:='WinSta0\Default';


    //necessary for shellexecute in new process
    CreateEnvironmentBlock(@P, ClientPipeUserToken.TokenHandle,false);
    if P <> nil then
      CreationFlags := CREATE_UNICODE_ENVIRONMENT
    else
      CreationFlags := 0;
    try
      Desc :=  TJwAutoPointer.Wrap(TJwSecurityDescriptor.Create).Instance as TJwSecurityDescriptor;

      Desc.OwnOwner := False;
      Desc.Owner := JwLocalSystemSID;

      Desc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], PROCESS_ALL_ACCESS, JwLocalSystemSID));
      {
      Don't try to add allow entries here for other users.
      The new process runs with SYSTEM account and thus no other user can debug it if it does not have
      SE_DEBUG_NAME privilege.
      }
      Desc.DACL.Add(TJwDiscretionaryAccessControlEntryDENY.Create(nil, [], PROCESS_ALL_ACCESS,  ClientPipeUserToken.TokenUser, true));

      SecAttr := LPSECURITY_ATTRIBUTES(Desc.Create_SA());
      try
        try
          ChecksumError := not JwCompareFileHash(CredentialsAppPath, CredentialsHash);
        except
          on e : Exception do
            Log.Exception(E);
        end;

        if ChecksumError then
        begin
          Log.Log(lsError, 'Hash of credentials application changed! Elevation is aborted.');
{$IFNDEF DEBUG}
          raise EHashMismatch.Create('Hash of credentials application changed! Elevation is aborted.');
          exit;
{$ENDIF DEBUG}
        end;

        //create an restricted token from system
        Token := GetRestrictedSYSTEMToken(ClientPipeUserToken);
        TJwAutoPointer.Wrap(Token);

        if not CreateProcessAsUserW(
           Token.TokenHandle,//ClientPipeUserToken.TokenHandle,
           PWideChar(Widestring(CredentialsAppPath)),
           PWideChar(Widestring(AppliationCmdLine)) ,
          SecAttr, SecAttr, false {mandatory between different sessions on XP},
          CREATE_NEW_CONSOLE or CreationFlags, P, nil, StartInfo, ProcInfo) then
        begin
          LogAndRaiseLastOsError(Log, ClassName, 'AskCredentials::(winapi)CreateProcessAsUserW', 'ElevationHandler.pas');
        end;
      finally
        TJwSecurityDescriptor.Free_SA(PSecurityAttributes(SecAttr));
      end;
    finally
      DestroyEnvironmentBlock(P);
    end;


    LastProcessID := GetProcessId(ProcInfo.hProcess);

    try
{$IFNDEF DEBUG}
      WaitResult := ServerPipe.WaitForClientToConnect(0, TIMEOUT{secs}, fStopEvent, ProcInfo.hProcess);
{$ELSE}
      //if ServerPipe.WaitForClientToConnect(0, INFINITE, fStopEvent) = 1{pipe event} then
      WaitResult := ServerPipe.WaitForClientToConnect(0, TIMEOUT{secs}, fStopEvent, ProcInfo.hProcess);
{$ENDIF}
      if WaitResult = 1{pipe event} then
      begin
        {protocol:
           Server    ->  Client : Send default username, domain, and password caching flag (possible or not)
           Wait for client to response
           Client    -> Server : receive username, domain, (password if not cache used) and flags
                                  Flags may contain cancel
           Server    -> Client : Send service result for connection data
           Server    -> Client : Send service result for createprocess
        }
        try
          SessionInfo.TimeOut := USERTIMEOUT;
          try
            MaxLoginRepetitionCount := MAX_LOGON_ATTEMPTS;
{$IFDEF DEBUG}
            SessionInfo.Flags := SessionInfo.Flags or SERVER_DEBUGTERMINATE;
{$ENDIF DEBUG}
            LoginRepetitionCount := 0;

            SessionInfo.MaxLogonAttempts := MaxLoginRepetitionCount;

            //get the user's reg key
            SessionInfo.UserRegKey := CopyUserRegKeyHandle(ClientPipeUserToken, ProcInfo.hProcess);
            //get the user's profile image (if any)
            SessionInfo.UserProfileImage := CreateUserProfileImage(ClientPipeUserToken,SessionInfo.UserProfileImageType);
            TJwAutoPointer.Wrap(SessionInfo.UserProfileImage);

            //send init data to cred app
            ServerPipe.SendServerData(SessionInfo);

            try
              {LoginRepetitionCount = 0}
              repeat
                ServerPipe.ReadClientData(SessionInfo,SessionInfo.TimeOut, fStopEvent);

                if Length(Trim(SessionInfo.UserName)) = 0 then
                  SessionInfo.UserName := ClientPipeUserToken.GetTokenUserName;

                {not LogonCorrect and (LoginRepetitionCount < MaxLoginRepetitionCount)}

                if (SessionInfo.Flags and CLIENT_CANCELED <> CLIENT_CANCELED) and
                  (not CheckLogonUser(SessionInfo)) then
                begin
                  Inc(LoginRepetitionCount);

                  if (LoginRepetitionCount <> MaxLoginRepetitionCount) then
                    ServerPipe.SendServerResult(ERROR_LOGONUSERFAILED,LoginRepetitionCount);
                  {not LogonCorrect and (LoginRepetitionCount+1 < MaxLoginRepetitionCount)}
                end
                else {LogonCorrect = true}
                  break;
              until LoginRepetitionCount >= MaxLoginRepetitionCount;
              {(not LogonCorrect and LoginRepetitionCount = MaxLoginRepetitionCount) or
               (LogonCorrect)
              }
            except
              on E : EOSError do
              begin
                Log.Log(lsWarning,'Failsafe for desktop switchback initiated.');

                //failsafe to switch back desktop
                CloseHandle(ProcInfo.hProcess);
                CloseHandle(ProcInfo.hThread);

                AppliationCmdLine := Sysutils.WideFormat('"%s" /cred /switchdefault', [CredentialsAppPath]);
                if not CreateProcessAsUserW(
                   Token.TokenHandle,//can't be ClientPipeUserToken because we need some more rights
                   PWideChar(Widestring(CredentialsAppPath)),
                   PWideChar(Widestring(AppliationCmdLine)) ,
                  nil, nil, false, CREATE_NEW_CONSOLE, nil, nil, StartInfo, ProcInfo) then
                begin
                   Log.Log('Failsafe for desktop switchback failed.');
                  LogAndRaiseLastOsError(Log, ClassName, 'AskCredentials::(winapi)CreateProcessAsUserW#2', 'ElevationHandler.pas');
                end;

                SessionInfo.Flags := CLIENT_CANCELED;
                Log.Log('Failsafe for desktop switchback succeeded.');

              end;


            end;

            //too many logon attempts - we shut down the connection
            if LoginRepetitionCount = MaxLoginRepetitionCount then
            begin
              ServerPipe.SendServerResult(ERROR_TOO_MANY_LOGON_ATTEMPTS,0);
              SessionInfo.Flags := CLIENT_CANCELED;
            end;

          except
            on E1a : ETimeOutException do
            begin
              ServerPipe.SendServerResult(ERROR_TIMEOUT, 0);
              Log.Exception(E1a);
              FreeAndNil(ServerPipe);
              //raise;
              exit;
            end;


            on E1b : EShutdownException do //service shuts down
            begin
              ServerPipe.SendServerResult(ERROR_SHUTDOWN, 0);
              Log.Exception(E1b);
              FreeAndNil(ServerPipe);
             // raise;
             exit;
            end;

            on E1c : EOSError do //ReadFile failed
            begin
              ServerPipe.SendServerResult(ERROR_WIN32, E1c.ErrorCode);
              Log.Exception(E1c);
              FreeAndNil(ServerPipe);
             // raise;
             exit;
            end;
            on E2 : EAbort do //recevied data error
            begin
              ServerPipe.SendServerResult(ERROR_ABORT, 0);
              Log.Exception(E2);
              FreeAndNil(ServerPipe);
             // raise;
             exit;
            end;
            on E3 : EOleError do //string copying failed
            begin
              ServerPipe.SendServerResult(ERROR_ABORT, 0);
              Log.Exception(E3);
              FreeAndNil(ServerPipe);
              //raise;
              exit;
            end;
            on E4 : Exception do //string copying failed
            begin
              ServerPipe.SendServerResult(ERROR_GENERAL_EXCEPTION, 0);
              Log.Exception(E4);
              FreeAndNil(ServerPipe);
              //raise;
              exit;
            end;
          end;

          try
            ServerPipe.SendServerResult(ERROR_SUCCESS,0);
          except
            on E : Exception do //string copying failed
            begin
              Log.Exception(E);
              //raise;
            end;
          end;

          result := True;
        except
          on E : EOSError do
          begin
            Log.Exception(E);
            FreeAndNil(ServerPipe);
            //raise;
          end;
        end;

        result := SessionInfo.Flags and CLIENT_CANCELED <> CLIENT_CANCELED;
      end
      else
        result := false;
    finally
      CloseHandle(ProcInfo.hProcess);
      CloseHandle(ProcInfo.hThread);
    end;

  finally
  end;



end;

constructor TElevationHandler.Create(
  const AllowedSIDs:  TJwSecurityIdList;
  const Passwords   : TCredentialsList;
  const StopEvent  : THandle;
  const StopState : PBoolean);
begin

  fPasswords   := Passwords;
  fStopEvent   := StopEvent;
  fStopState   := StopState;

  ServerPipe := TServerSessionPipe.Create;
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



function TElevationHandler.StartApplication(const ElevationStruct : TXPElevationStruct; out PID : DWORD) : HRESULT;

function CopyStartupInfo(const Startup : {$IFDEF UNICODE}TStartupInfoW{$ELSE}TStartupInfoA{$ENDIF})
   : {$IFDEF UNICODE}TStartupInfoW{$ELSE}TStartupInfoA{$ENDIF};
begin
//  ZeroMemory(@result, sizeof(result));
  CopyMemory(@result, @Startup, sizeof(result));

  result.cb := sizeof(result);


  result.lpReserved := nil;
end;



var Password,
    Username,
    DefaultUserName,
    Domain: Widestring;
    LSA: TJwSecurityLsa;
    ProfBuffer: PMSV1_0_INTERACTIVE_PROFILE;
    Token : TJwSecurityToken;
    ProfInfo: PROFILEINFO;
    SIDIndex: Integer;
    InVars : TJwCreateProcessInfo;
    OutVars : TJwCreateProcessOut;
    Groups : TJwSecurityIdList;
    XPElevationSID : TJwSecurityId;

const EncryptionBlockSize = 8;
var SessionInfo : TSessionInfo;
    Log : IJwLogClient;

    SessionID,
    ErrorResult : DWORD;
    ProcessID : TJwProcessId;
    UserData : PProcessJobData;
    Sid : TJwSecurityId;
    Identifier: TSidIdentifierAuthority;


begin
  result := SUCCESS;


  Log := uLogging.LogServer.Connect(etMethod,ClassName,
          'StartApplication','ElevationHandler.pas','');

  try //1.
    //get client token from pipe impersonation
    Token := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);
    try //2.

      Log.Log('Checking Token...');
      SID := Token.TokenUser;
      try //3.
        XPElevationSID := TJwSecurityId.Create('','XPElevationUser');
        TJwAutoPointer.Wrap(XPElevationSID);

        Groups := Token.TokenGroups;
        TJwAutoPointer.Wrap(Groups);

        {
        JEDI XPElevation User. The members of this group is allowed to be
        elevated as Administrator even if they are not in the Administrators group.
        }
        SIDIndex := Groups.FindSid(XPElevationSID);
{$IFNDEF DEBUG}
        If SIDIndex = -1 then
        begin
          ErrorResult := ERROR_INVALID_USER;
          Log.Log(JwFormatString('Elevation of user %s is not allowed. (%s)',
            [SID.AccountName[''], ElevationStruct.ApplicationName]));


          result := E_INVALID_USER;
          exit;
        end;
{$ENDIF DEBUG}


        Log.Log('Getting accountname');
        Username := SID.AccountName[''];
        DefaultUserName := Username;
        try //4.
          Domain := SID.GetAccountDomainName('');
        except //4.
          Domain := 'local';
        end;
        Log.Log('Done');
      finally //3.
        SID.Free;
      end;
      TJwSecurityToken.RevertToSelf;

      Token.ConvertToPrimaryToken(TOKEN_ALL_ACCESS);

      SessionID := Token.TokenSessionId;

      try //5.
        Log.Log('Credentials for user '+Username+' are requested.');

        SessionInfo.Application := ElevationStruct.ApplicationName;
        SessionInfo.Commandline := ElevationStruct.Parameters;
        SessionInfo.ParentWindow := ElevationStruct.ParentWindow;
        SessionInfo.ControlFlags := ElevationStruct.ControlFlags;

        SessionInfo.Flags := 0;
        //is password cache available?
        if fPasswords.IsSessionValid(SessionID) and
          (SessionInfo.ControlFlags and XPCTRL_FORCE_NO_CACHE_CREDENTIALS <> XPCTRL_FORCE_NO_CACHE_CREDENTIALS)
          then
        begin
          try
            Log.Log('Retrieving cached credentials');
            fPasswords.GetBySession(SessionID,
              SessionInfo.Domain,
              SessionInfo.UserName,
              SessionInfo.Password);
            RandomizePasswdW(SessionInfo.Password);
            SessionInfo.Password := '';
            SessionInfo.Flags    := SERVER_CACHEAVAILABLE;
          except
            on E : Exception do
            begin
              Log.Exception(E);

              SessionInfo.UserName := Username;
              SessionInfo.Domain   := Domain;
              RandomizePasswdW(SessionInfo.Password);
              SessionInfo.Password := '';

              SessionInfo.Flags    := 0;
            end;
          end;
        end
        else
        begin
          SessionInfo.UserName := Username;
          SessionInfo.Domain   := Domain;
          RandomizePasswdW(SessionInfo.Password);


          SessionInfo.Flags    := 0;
        end;




        //creates new process which asks user for credentials
        //ServerPipe is created here
        try //6.
          if not AskCredentials(Token, SessionID,ProcessID, SessionInfo) then
          begin
            Log.Log('Credentials prompt for '+ElevationStruct.ApplicationName+' was aborted ');
            if Assigned(ServerPipe) then
            try
              ServerPipe.SendServerResult(ERROR_ABORTBYUSER, 0);
            except
            end;  
{$IFDEF DEBUG}
             if (SessionInfo.Flags and CLIENT_DEBUGTERMINATE = CLIENT_DEBUGTERMINATE) then
             begin
               Log.Log('DEBUG: Termination of service initiated');
               XPService.Stopped := true;
             end;
{$ENDIF DEBUG}
            result := E_ABORTED;
            exit;
          end;
        except //6.
          on E : EJwsclHashMismatch do
          begin
            Log.Log('Error: '+E.Message);

            result := E_INVALID_CRED_APP;
            exit;
          end;
          on E : Exception do
          begin
            if Assigned(ServerPipe) then
            try
              ServerPipe.SendServerResult(ERROR_GENERAL_EXCEPTION, 0);
            except
            end;
            //credential process is already informed- FreeAndNil(ServerPipe) was called
            Log.Log('Error: Credentials prompt for '+ElevationStruct.ApplicationName+' canceled. '+E.Message);

            result := E_ABORTED;
            exit;
          end;
        end;

        if Length(Trim(SessionInfo.UserName)) = 0 then
          SessionInfo.UserName := DefaultUserName
        else
        begin
          if (SessionInfo.ControlFlags and XPCTRL_FORCE_NO_ALTERNATE_LOGON = XPCTRL_FORCE_NO_ALTERNATE_LOGON) and
             (JwCompareString(SessionInfo.UserName, DefaultUserName) <> 0) then
          begin
            if Assigned(ServerPipe) then
            try
              ServerPipe.SendServerResult(ERROR_INVALID_USER, 0);
            except
            end;

            result := E_INVALID_USER;
            exit;
          end;
        end;

        try  //6a.
          {Get password from cache.
           In this case Username and Domain member of SessionInfo (from Client)
           are ignored
          }
          if (SessionInfo.ControlFlags and XPCTRL_FORCE_NO_CACHE_CREDENTIALS <> XPCTRL_FORCE_NO_CACHE_CREDENTIALS) and
             (SessionInfo.Flags and CLIENT_USECACHECREDS = CLIENT_USECACHECREDS) and
             (fPasswords.IsSessionValid(SessionID)) {and (SessionInfo.Password = '') }then
          begin
            Log.Log('Credentials for user '+Username+' are retrieved from the cache');

            try
              Log.Log('Retrieving cached credentials');
              fPasswords.GetBySession(SessionID, SessionInfo.Domain, SessionInfo.Username, SessionInfo.Password);
            except
            on E : Exception do
              Log.Exception(E);
            end;
          end;

          {Add/Save the new credentials into the cache
           +only if no existing cache is used
          }
          if (SessionInfo.ControlFlags and XPCTRL_FORCE_NO_CACHE_CREDENTIALS <> XPCTRL_FORCE_NO_CACHE_CREDENTIALS) and
             (SessionInfo.Flags and CLIENT_CACHECREDS = CLIENT_CACHECREDS) and
             (SessionInfo.Flags and CLIENT_USECACHECREDS <> CLIENT_USECACHECREDS) then
          begin
            try
              Log.Log('Caching user input');
              fPasswords.SetBySession(SessionID, SessionInfo.Domain, SessionInfo.UserName, SessionInfo.Password);
            except
            on E : Exception do
              Log.Exception(E);
            end;



          end;
          try
            Domain   := SessionInfo.Domain;
            Username := SessionInfo.UserName;
            Password := SessionInfo.Password;
            RandomizePasswdW(SessionInfo.Password);

            ZeroMemory(@InVars.StartupInfo, sizeof(InVars.StartupInfo));
            InVars.StartupInfo := CopyStartupInfo(ElevationStruct.StartupInfo);

            try
              //Add specific group
              Sid := TJwSecurityId.Create(JwFormatString('S-1-5-5-%d-%d',
                [10000+Token.TokenSessionId, ProcessID]));
              Invars.AdditionalGroups := TJwSecurityIdList.Create;
              Sid.AttributesType := [sidaGroupMandatory,sidaGroupEnabled];
              Invars.AdditionalGroups.Add(Sid);
            except
            end;


          
            InVars.SourceName := 'XPElevation';
            InVars.SessionID := Token.TokenSessionId;
            InVars.UseSessionID := true;
            InVars.DefaultDesktop := true;
            InVars.LogonProcessName := 'XPElevation';
            InVars.LogonToken := nil;
            InVars.LogonSID := nil;

            ZeroMemory(@InVars.Parameters, sizeof(InVars.Parameters));
            InVars.Parameters.lpApplicationName := ElevationStruct.ApplicationName;
            InVars.Parameters.lpCommandLine := ElevationStruct.Parameters;
            InVars.Parameters.lpCurrentDirectory := ElevationStruct.CurrentDirectory;


            TJwSecurityToken.RevertToSelf;

            InVars.Parameters.dwCreationFlags := CREATE_NEW_CONSOLE or
                CREATE_SUSPENDED or CREATE_UNICODE_ENVIRONMENT or CREATE_BREAKAWAY_FROM_JOB;

            try //7.
              try
                JwCreateProcessAsAdminUser(
                   UserName,//const UserName,
                   Domain,//Domain,
                   Password,//Password : TJwString;
                   InVars,//const InVars : TJwCreateProcessInfo;
                   OutVars,//out OutVars : TJwCreateProcessOut;
                   uLogging.LogServer//LogServer : IJwLogServer
                 );

                PID := OutVars.ProcessInfo.dwProcessId;

                //send success
                if Assigned(ServerPipe) then
                try
                  ServerPipe.SendServerResult(ERROR_SUCCESS,0);
                except
                end;
            
                //save user profile for later unloading
                New(UserData);
                UserData.UserToken   := OutVars.UserToken;
                UserData.UserProfile := OutVars.ProfInfo;
                (*try
                  fJobs.AssignProcessToJob(OutVars.ProcessInfo.hProcess,
                    Pointer(UserData));
                except
                  //TODO: oops job assignment failed
                end;*)
                ResumeThread(OutVars.ProcessInfo.hThread);
              finally


                //free process handles 
                FreeAndNil(InVars.AdditionalGroups);

                FreeAndNil(OutVars.LinkedToken);
                FreeAndNil(OutVars.LSA);
                LsaFreeReturnBuffer(OutVars.ProfBuffer);
                DestroyEnvironmentBlock(OutVars.EnvironmentBlock);


                CloseHandle(OutVars.ProcessInfo.hProcess);
                CloseHandle(OutVars.ProcessInfo.hThread);
              end;
            finally
              RandomizePasswdW(Password);
            end;

            
            
          except //7.
            on E1 : EJwsclWinCallFailedException do
            begin
              result := E_CREATEPROCESS_FAILED;


              Log.Exception(E1);
              if Assigned(ServerPipe) then
                ServerPipe.SendServerResult(ERROR_WIN32,E1.LastError);

            end;
            on E2 : EJwsclNoSuchLogonSession do
            begin
              result := E_CREATEPROCESS_FAILED;

              Log.Exception(E2);
              if Assigned(ServerPipe) then
                ServerPipe.SendServerResult(ERROR_NO_SUCH_LOGONSESSION,0);
            end;
            on E3 : EJwsclCreateProcessFailed do
            begin
              result := E_CREATEPROCESS_FAILED;
              
              Log.Exception(E3);
              if Assigned(ServerPipe) then
                ServerPipe.SendServerResult(ERROR_CREATEPROCESSASUSER_FAILED, E3.LastError);
            end;
          
            on E : Exception do
            begin
              result := E_CREATEPROCESS_FAILED;

              Log.Exception(E);
              if Assigned(ServerPipe) then
                ServerPipe.SendServerResult(ERROR_GENERAL_EXCEPTION,0);
            end;

          end;
        finally //6a.
      
        end;
      finally  //5.
        
      end;
    finally //2.
      FreeAndNil(Token);
    end;

  finally //1.

  end;
end;

initialization
  randomize;

end.
