unit JwsclTokenTests;

interface

uses
  Forms,
  jwaWindows,
  ShellApi,

  SysUtils,

  UMessageForm,

  JwaVista,
  JwsclConstants,
  JwsclTypes,
  JwsclExceptions,
  JwsclToken,
  JwsclSid,
  JwsclStrings,
  JwsclEnumerations,

  TestFrameWork;

type
  TSecurityTokenTests = class(TTestCase)
  private
  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure Test_Create;
    procedure Test_ConvertToken;
    procedure Test_GetTokenPrivileges;
    procedure Test_SetTokenGroupsAttributes;

    procedure TestGetRunElevation;
    procedure TestGetElevationType;
    procedure TestGetVirtualizationAllowed;
    procedure TestGetVirtualizationEnabled;
    procedure TestGetIntegrityLevel;

    procedure TestGetLinkedToken;

    procedure TestGetMandatoryPolicy;
    procedure TestGetCurrentUserRegKey;
    procedure TestCreateWTSQueryUserTokenEx;

    procedure TestPrivilegeCheck;

   { procedure TestDestroy;
    procedure TestCheckTokenHandle;
    procedure TestCheckTokenAccessType;
    procedure TestCheckTokenPrivileges;
    procedure TestIsPrivilegeAvailable;
    procedure TestGetIsRestricted;
    procedure TestGetTokenType;
    procedure TestGetTokenInformationLength;
    procedure TestGetTokenInformation;
    procedure TestGetImpersonationLevel;
    procedure TestGetTokenUser;
    procedure TestGetTokenSource;
    procedure TestGetTokenGroups;
    procedure TestGetTokenRestrictedSids;
    procedure TestGetTokenDefaultDacl;
    procedure TestSetfTokenDefaultDacl;
    procedure TestGetTokenOrigin;
    procedure TestSetTokenOrigin;
    procedure TestGetTokenOwner;
    procedure TestSetTokenOwner;
    procedure TestGetPrimaryGroup;
    procedure TestSetPrimaryGroup;
    procedure TestGetTokenSessionId;
    procedure TestSetTokenSessionId;
    procedure TestGetPrivilegeEnabled;
    procedure TestSetPrivilegeEnabled;

    procedure TestCreateTokenByProcess;
    procedure TestCreateTokenByThread;
    procedure TestCreateTokenEffective;
    procedure TestCreateDuplicateExistingToken;
    procedure TestCreateRestrictedToken;
    procedure TestConvertToImpersonatedToken;
    procedure TestConvertToPrimaryToken;
    procedure TestGetTokenPrivileges;
    procedure TestCreateDuplicateToken;
    procedure TestCreateRestrictedToken;
    procedure TestCheckTokenMembership;
    procedure TestIsEqual;
    procedure TestSetThreadToken;
    procedure TestImpersonateLoggedOnUser;
    procedure TestPrivilegeCheck;
    procedure TestPrivilegeCheckEx;
    procedure TestPrivilegeCheck;
    procedure TestCopyLUID;
    procedure TestGetTokenStatistics;
    procedure TestFreeObjectMemory;
    procedure TestPrivilegedServiceAuditAlarm;
    procedure TestImpersonateAnonymousToken;
    procedure TestImpersonateSelf;
    procedure TestRevertToSelf;
    procedure TestImpersonateNamedPipeClient;    }

  end;

type
  TSecurityTokenStatisticsTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods
  {  procedure TestCreate;      }

  end;

type
  TPrivilegeTests = class(TTestCase)
  private
     fPrivilege : array[1..10] of TJwPrivilege;
     fPrivSet : TJwPrivilegeSet;
     newLuid : TLuid;
  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure Test_Create;
    procedure Test_GetSetEnabled;
    procedure Test_ClassMethods;
    procedure Test_RemoveIrrepealable;
    procedure Test_GetText;

  end;

type
  TPrivilegeSetTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods
 {   procedure TestGetPrivByIdx;
    procedure TestGetPrivByName;
    procedure TestGetPrivByLUID;
    procedure TestGetCount;
    procedure TestCreate;
    procedure TestCreate;
    procedure TestCreate;
    procedure TestDestroy;
    procedure TestGetText;
    procedure TestRemoveIrrepealable;
    procedure TestDisableAllPrivileges;
    procedure TestCreate_PLUID_AND_ATTRIBUTES;
    procedure TestCreate_PPRIVILEGE_SET;
    procedure TestFree_PLUID_AND_ATTRIBUTES;
    procedure TestFree_PPRIVILEGE_SET;
    procedure TestDeletePrivilege;
    procedure TestAddPrivilege;
    procedure TestAddPrivilege;
    procedure TestAddPrivilege;
    procedure TestAddPrivilege;     }

  end;

implementation

uses Classes;

{ TSecurityTokenTests }

{$I Compilers.inc}

{ TPrivilegeTests }

procedure TPrivilegeTests.SetUp;
var i : Integer;
begin
  inherited;
  for i := low(fPrivilege) to high(fPrivilege) do
    fPrivilege[i] := nil;
  newLuid := TJwPrivilege.TextToLUID(SE_TCB_NAME);
  fPrivSet := TJwPrivilegeSet.Create();
end;

procedure TPrivilegeTests.TearDown;
var i : Integer;
begin
  inherited;
  for i := low(fPrivilege) to high(fPrivilege) do
    fPrivilege[i].Free;
  fPrivSet.Free;
end;

procedure TPrivilegeTests.Test_Create;
begin

  try
    fPrivilege[1] := TJwPrivilege.Create(nil,TJwPrivilege.MakeLUID_AND_ATTRIBUTES(0,5,0));
    CheckIs(nil,EJwsclInvalidOwnerException);
  except
    on E : Exception do
     CheckIs(E,EJwsclInvalidOwnerException);
  end;

  try
    fPrivilege[1] := TJwPrivilege.Create(fPrivSet,TJwPrivilege.MakeLUID_AND_ATTRIBUTES(newLuid,0));
    CheckFalse(fPrivilege[1] = nil);

    CheckTrue(fPrivilege[1].Owner = fPrivSet);
    {$IFDEF COMPILER_5}CheckNotEqualsWideString{$ELSE}CheckNotEquals{$ENDIF}('',fPrivilege[1].DisplayName);
    {$IFDEF COMPILER_5}CheckNotEqualsWideString{$ELSE}CheckNotEquals{$ENDIF}('',fPrivilege[1].Name);
    CheckEquals(0,fPrivilege[1].Attributes);
    CheckFalse(fPrivilege[1].Privilege_Used_For_Access);

    CheckTrue(fPrivilege[1].LUID.LowPart = newLuid.LowPart);
    CheckTrue(fPrivilege[1].LUID.HighPart = newLuid.HighPart);

    CheckNotEquals(0,fPrivilege[1].LanguageID);
  finally
  end;

  try
    CheckFalse(fPrivilege[1].Enabled);
    CheckIs(nil,EJwsclNotImplementedException);
  except
    on E : Exception do
     CheckIs(E,EJwsclNotImplementedException);
  end;

end;

procedure TPrivilegeTests.Test_GetSetEnabled;

  function FindEnabledByDefault(const privs : TJwPrivilegeSet) : TJwPrivilege;
  var i : Integer;
  begin
    result := nil;
    for i := 0 to privs.Count -1 do
    begin
      if (privs.PrivByIdx[i].IsEnabledByDefault) then
      begin
        result := privs.PrivByIdx[i];
        break;
      end;
    end;

  end;
var token : TJwSecurityToken;
    privs : TJwPrivilegeSet;
    priv : TJwPrivilege;
begin
  token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or TOKEN_WRITE);


  try
    privs := token.GetTokenPrivileges;

    try
      if (privs.Count = 0) then
        Check(true,'no privilege available to test');

      priv := privs.PrivByIdx[0];

      priv.Enabled := false;
      CheckEquals(false,priv.Enabled);

      priv.Enabled := true;
      CheckEquals(true,priv.Enabled);
 
      priv.Enabled := true;
      CheckEquals(true,priv.Enabled);

      priv.Enabled := false;
      CheckEquals(false,priv.Enabled);

      //search for a privilege with attribute SE_PRIVILEGE_ENABLED_BY_DEFAULT
      priv := FindEnabledByDefault(privs);

      if Assigned(priv) then
      begin

        priv.Enabled := false;
        CheckEquals(false,priv.IsEnabledByDefault);

        priv.Enabled := true;
        CheckEquals(true,priv.IsEnabledByDefault);
        //SE_PRIVILEGE_ENABLED_BY_DEFAULT must be restored!
      end;
    finally
      privs.Free;
    end;
  finally
    token.Free;
  end;
end;

procedure TPrivilegeTests.Test_GetText;
var token : TJwSecurityToken;
    privs : TJwPrivilegeSet;
    priv : TJwPrivilege;

    sText : TJwString;
begin
  token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or TOKEN_WRITE);


  try
    privs := token.GetTokenPrivileges;

    try
      if (privs.Count = 0) then
        Check(true,'no privilege available to test');

      priv := privs.PrivByIdx[0];

      sText := priv.GetText;
      {$IFDEF COMPILER_5}CheckNotEqualsWideString{$ELSE}CheckNotEquals{$ENDIF}('',sText);


    finally
      privs.Free;
    end;
  finally
    token.Free;
  end;
end;
procedure TPrivilegeTests.Test_ClassMethods;
var aLuid : TLuid;
    aLA : TLuidAndAttributes;
    x,y : Integer;
    xAttr, zLow : Cardinal;
    yHigh : Integer;
begin
  //TJwPrivilege.SEAttributeToText
  begin
(*    {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('(unknown attributes)',             TJwPrivilege.PrivilegeAttributeToText(1000));
    {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('(none)',                           TJwPrivilege.PrivilegeAttributeToText(0));
    {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('SE_PRIVILEGE_ENABLED_BY_DEFAULT',  TJwPrivilege.PrivilegeAttributeToText(SE_PRIVILEGE_ENABLED_BY_DEFAULT));
    {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('SE_PRIVILEGE_ENABLED',             TJwPrivilege.PrivilegeAttributeToText(SE_PRIVILEGE_ENABLED));
    {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('SE_PRIVILEGE_USED_FOR_ACCESS',     TJwPrivilege.PrivilegeAttributeToText(SE_PRIVILEGE_USED_FOR_ACCESS));

    {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('SE_PRIVILEGE_ENABLED_BY_DEFAULT,SE_PRIVILEGE_ENABLED,SE_PRIVILEGE_USED_FOR_ACCESS',
      TJwPrivilege.SEAttributeToText(SE_PRIVILEGE_ENABLED_BY_DEFAULT or SE_PRIVILEGE_ENABLED or SE_PRIVILEGE_USED_FOR_ACCESS));
*)
  end;

  //TJwPrivilege.LUIDtoText
(*  begin
    for x := 0 to 100 do
     for y := -100 to 100 do
     begin
       aLuid := TJwPrivilege.MakeLUID(x,y);
       {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}(IntToStr(y)+'x'+IntToStr(x), TJwPrivilege.LUIDtoText(aLuid));
     end;

    aLuid := LUID_INVALID;
    {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('-1x0', TJwPrivilege.LUIDtoText(aLuid));
    aLuid := LUID_NULL;
    {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('0x0', TJwPrivilege.LUIDtoText(aLuid));

    aLuid.LowPart := high(aLuid.LowPart);
    aLuid.HighPart := high(aLuid.HighPart);
    {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('2147483647x4294967295', TJwPrivilege.LUIDtoText(aLuid));

    aLuid.LowPart := low(aLuid.LowPart);
    aLuid.HighPart := low(aLuid.HighPart);
    {$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}('-2147483648x0', TJwPrivilege.LUIDtoText(aLuid));
  end;*)

  //TextToLUID
  begin
    try
      aLuid := TJwPrivilege.TextToLUID('');
      CheckIs(nil,EJwsclWinCallFailedException);
    except
      on E : Exception do
        CheckIs(E,EJwsclWinCallFailedException);
    end;

    aLuid := TJwPrivilege.TextToLUID(SE_TCB_NAME);
    CheckEquals(7,aLuid.LowPart);
    CheckEquals(0,aLuid.HighPart);
  end;

  //MakeLUID_AND_ATTRIBUTES, MakeLUID_AND_ATTRIBUTES, MakeLUID
  begin
    for xAttr := 0 to 100 do
     for yHigh := -100 to 100 do
      for zLow := 0 to 100 do
      begin
        aLuid := TJwPrivilege.MakeLUID(zLow, yHigh);
        CheckEquals(zLow,aLuid.LowPart);
        CheckEquals(yHigh,aLuid.HighPart);

        aLA := TJwPrivilege.MakeLUID_AND_ATTRIBUTES(zLow,yHigh,xAttr);
        CheckEquals(xAttr,aLA.Attributes);
        CheckEquals(zLow,aLA.Luid.LowPart);
        CheckEquals(yHigh,aLA.Luid.HighPart);

        aLA := TJwPrivilege.MakeLUID_AND_ATTRIBUTES(aLuid,xAttr);
        CheckEquals(xAttr,aLA.Attributes);
        CheckEquals(zLow,aLA.Luid.LowPart);
        CheckEquals(yHigh,aLA.Luid.HighPart);

        Sleep(0);
        Application.ProcessMessages;
      end;
  end;


end;

const HIGH_LEVEL_PRIVILEGES : Array[1..12]of ShortString = (
      SE_SECURITY_NAME, SE_LOAD_DRIVER_NAME, SE_CREATE_PAGEFILE_NAME,
      SE_BACKUP_NAME, SE_RESTORE_NAME, SE_AUDIT_NAME, SE_REMOTE_SHUTDOWN_NAME,
      SE_SYNC_AGENT_NAME, SE_CREATE_GLOBAL_NAME, SE_IMPERSONATE_NAME, SE_SYSTEMTIME_NAME,
      SE_CHANGE_NOTIFY_NAME);



procedure TPrivilegeTests.Test_RemoveIrrepealable;
var token,token2 : TJwSecurityToken;
    privs : TJwPrivilegeSet;
    priv : TJwPrivilege;

    sText : TJwString;
    i : Integer;
begin
  token2 := TJwSecurityToken.CreateTokenEffective(TOKEN_DUPLICATE or TOKEN_ALL_ACCESS);

  if not token2.PrivilegeAvailable[SE_TCB_NAME] then
  begin
    token2.Free;
    Check(false,'This test needs to be run with TCB privilege.');
    exit;
  end;

  token := TJwSecurityToken.CreateDuplicateExistingToken(token2.TokenHandle, TOKEN_READ or TOKEN_WRITE or TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY);

  {do not call
   token2.Free;
   otherwise the privileges of the token.Tokenhandle are invalid. 
  }
//           stimmt was net
  try
    privs := token.GetTokenPrivileges;

    try
      if (privs.Count = 0) then
        Check(true,'no privilege available to test');

      priv := nil;
      for i := low(HIGH_LEVEL_PRIVILEGES) to high(HIGH_LEVEL_PRIVILEGES) do
      begin
        priv := privs.PrivByName[HIGH_LEVEL_PRIVILEGES[i]];
        if priv <> nil then
          break;
      end;

      if priv = nil then
      begin
        Check(false,'Could not find a privilege to remove.');
      end;

      priv.RemoveIrrepealable;


    finally
      privs.Free;
    end;
  finally

    token.Free;
    token2.Free;
  end;
end;




{ TSecurityTokenTests }

procedure TSecurityTokenTests.SetUp;
begin
  inherited;

end;

procedure TSecurityTokenTests.TearDown;
begin
  inherited;

end;



procedure TSecurityTokenTests.TestCreateWTSQueryUserTokenEx;
var Token : TJwSecurityToken;
begin
  try
    Token := TJwSecurityToken.CreateWTSQueryUserTokenEx(0,0);
  finally
    Token.Free;
  end;
end;

procedure TSecurityTokenTests.TestGetCurrentUserRegKey;
var Token : TJwSecurityToken;
begin
  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or TOKEN_QUERY);
  try
    RegCloseKey(Token.GetCurrentUserRegKey(MAXIMUM_ALLOWED));
  finally
    Token.Free;
  end;
end;

procedure TSecurityTokenTests.TestGetElevationType;
var Token : TJwSecurityToken;
begin
  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or TOKEN_QUERY);
  try
    case Token.ElevationType of
      TokenElevationTypeDefault : Check(false,'This test did not fail but reports: TokenElevationTypeDefault');
      TokenElevationTypeFull    : Check(false,'This test did not fail but reports: TokenElevationTypeFull');
      TokenElevationTypeLimited : Check(false,'This test did not fail but reports: TokenElevationTypeLimited');
    else
      Check(false,'Test failed because return value is incorrect.');
    end;
  finally
    Token.Free;
  end;
end;
procedure TSecurityTokenTests.TestGetIntegrityLevel;
var Token : TJwSecurityToken;
    RE : Cardinal;
    IntLevel : TJwSecurityIdList;
begin
  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or TOKEN_QUERY);
  try
    IntLevel := Token.GetIntegrityLevel;
    try
      ShowMessageForm(IntLevel.GetText(true));
    finally
      IntLevel.Free;
    end;
  finally
    Token.Free;
  end;
end;


procedure TSecurityTokenTests.TestGetLinkedToken;
var Token, LinkedToken : TJwSecurityToken;
    TokenGroups, LinkedTokenGroups,
    IntLevel : TJwSecurityIdList;
    si : STARTUPINFO;
    pi : PROCESS_INFORMATION;
begin
  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);

  try
    LinkedToken := Token.LinkedToken;
   // ShowMessageForm(LinkedToken.GetTokenUser.GetText(true));

    FillChar(si, sizeof(si),0);

    //LinkedToken.ConvertToImpersonatedToken(SecurityImpersonation, TOKEN_ALL_ACCESS);
   { if not CreateProcessWithTokenW(LinkedToken.TokenHandle,
        0,'C:\windows\system32\cmd.exe','',
          CREATE_NEW_CONSOLE,nil, '',@si,@pi ) then
        RaiseLastOSError;
     }
    {
    for a linked token this is not possible? why?
    LinkedToken.SetThreadToken(MAXIMUM_ALLOWED);
    ShellExecuteA(0,'open','cmd.exe','','',SW_SHOW);}

    IntLevel := LinkedToken.GetIntegrityLevel;
    try
      ShowMessageForm(IntLevel.GetText(true));
    finally
      IntLevel.Free;
    end;

    TokenGroups := Token.TokenGroups;
    LinkedTokenGroups := LinkedToken.TokenGroups;
    try
      ShowMessageForm('Actual Token '#13#10+TokenGroups.GetText(true)
              +#13#10#13#10+'Linked Token'#13#10+LinkedTokenGroups.GetText(true));
    finally
      TokenGroups.Free;
      LinkedTokenGroups.Free;
    end;
  finally
    LinkedToken.Free;
    Token.Free;
  end;
end;

procedure TSecurityTokenTests.TestGetMandatoryPolicy;
var Token : TJwSecurityToken;
    Pol,Pol2 : TJwTokenMandatoryPolicies;
    P : DWORD;
begin
  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or TOKEN_QUERY);
  try
    Pol := Token.GetMandatoryPolicy;

    P := TJwEnumMap.ConvertTokenMandatoryPolicyFlags(Pol);
    Pol2 := TJwEnumMap.ConvertTokenMandatoryPolicyFlags(P);

    CheckTrue(Pol = Pol2);

  finally
    Token.Free;
  end;
end;

procedure TSecurityTokenTests.TestGetRunElevation;
var Token : TJwSecurityToken;
    RE : Cardinal;
begin
  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or TOKEN_QUERY);
  try
    RE := Token.RunElevation;
    Check(false,'This test did not fail but reports: RunElevation='+IntToStr(RE));
  finally
    Token.Free;
  end;
end;

procedure TSecurityTokenTests.TestGetVirtualizationAllowed;
var Token : TJwSecurityToken;
begin
  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or TOKEN_QUERY);
  try
    if Token.VirtualizationAllowed then
      Check(false,'This test did not fail but reports: VirtualizationAllowed active')
    else
      Check(false,'This test did not fail but reports: VirtualizationAllowed inactive');
  finally
    Token.Free;
  end;
end;


procedure TSecurityTokenTests.TestGetVirtualizationEnabled;
var Token : TJwSecurityToken;
begin
  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or TOKEN_QUERY);
  try
    if Token.VirtualizationEnabled then
      Check(false,'This test did not fail but reports: VirtualizationEnabled active')
    else
      Check(false,'This test did not fail but reports: VirtualizationEnabled inactive');
  finally
    Token.Free;
  end;
end;

procedure TSecurityTokenTests.TestPrivilegeCheck;
var Token : TJwSecurityToken;
    Privs : TJwPrivilegeSet;
    bRes : Boolean; 
begin
  Privs := TJwPrivilegeSet.Create;
  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or TOKEN_QUERY);
  try
    try
      Token.PrivilegeCheckEx(nil, pcAllPrivsEnabled);
      Check(false,'Precondition not met');
    except
      on E : Exception do
       CheckEquals(EJwsclNILParameterException,E.ClassType);
    end;

    CheckTrue(Token.PrivilegeCheckEx(Privs, pcAllPrivsEnabled));

    Privs.Free;
    Privs := Token.GetTokenPrivileges;
    Privs.Control := PRIVILEGE_SET_ALL_NECESSARY;

    CheckTrue(Token.PrivilegeCheckEx(Privs, pcDefault));

  finally
    Token.Free;
  end;
end;


procedure TSecurityTokenTests.Test_ConvertToken;
var Token1, Token2, oldToken : TJwSecurityToken;
begin
  oldToken := nil;
  if TJwSecurityToken.HasThreadAToken() then
  begin
    oldToken := TJwSecurityToken.GetThreadToken(TOKEN_ALL_ACCESS, true);
    TJwSecurityToken.RemoveThreadToken(0);
  end;

  try
    Token1 := TJwSecurityToken.CreateTokenByProcess(0,TOKEN_ALL_ACCESS); //TOKEN_READ or TOKEN_DUPLICATE or TOKEN_WRITE);
    CheckEquals(Integer(TokenPrimary),Integer(Token1.TokenType));
  
    Token1.ConvertToImpersonatedToken(SECURITY_MAX_IMPERSONATION_LEVEL,TOKEN_ALL_ACCESS);

    try
      Token1.ConvertToImpersonatedToken(SECURITY_MAX_IMPERSONATION_LEVEL,TOKEN_ALL_ACCESS);
    except
      CheckFalse(true, 'ConvertToImpersonatedToken shouldnt raise an exception!');
    end;

    CheckEquals(Integer(TokenImpersonation),Integer(Token1.TokenType));
  
    Token1.SetThreadToken(0);


    CheckEquals(Integer(SECURITY_MAX_IMPERSONATION_LEVEL), Integer(Token1.ImpersonationLevel));

    Token2 := TJwSecurityToken.CreateTokenByThread(GetCurrentThread,TOKEN_ALL_ACCESS,false);
    CheckEquals(Integer(SECURITY_MAX_IMPERSONATION_LEVEL), Integer(Token2.ImpersonationLevel));

    Token2.ConvertToPrimaryToken(TOKEN_ALL_ACCESS);
    CheckEquals(Integer(TokenPrimary),Integer(Token2.TokenType));


    Token1.Free;
    Token2.Free;

  finally
    if Assigned(oldToken) then
      oldToken.SetThreadToken(0);
    oldToken.Free;
  end;

end;

procedure TSecurityTokenTests.Test_Create;
var Token1, Token2, oldToken : TJwSecurityToken;
    ThreadID : Cardinal;
    hThread : THandle;
    ThreadResult : Integer;
begin
  oldToken := nil;
  if TJwSecurityToken.HasThreadAToken() then
  begin
    oldToken := TJwSecurityToken.GetThreadToken(TOKEN_ALL_ACCESS, true);
    TJwSecurityToken.RemoveThreadToken(0);
  end;


  try
    try
      Token1 := TJwSecurityToken.CreateTokenByThread(0,TOKEN_ALL_ACCESS,true);
      CheckIs(nil,EJwsclNoThreadTokenAvailable);
    except
      on E : Exception do
        CheckIs(E,EJwsclNoThreadTokenAvailable);
    end;

    try
      Token1 := TJwSecurityToken.CreateTokenByThread(GetCurrentThread,TOKEN_ALL_ACCESS,true);
      CheckIs(nil,EJwsclNoThreadTokenAvailable);
    except
      on E : Exception do
        CheckIs(E,EJwsclNoThreadTokenAvailable);
    end;

    Token1 := nil;

    Token1 := TJwSecurityToken.CreateTokenByProcess(0,TOKEN_ALL_ACCESS);

    Token1.Free;

    Token1 := TJwSecurityToken.CreateTokenByProcess(0,TOKEN_ALL_ACCESS); //TOKEN_READ or TOKEN_DUPLICATE or TOKEN_WRITE);
    Token1.ConvertToImpersonatedToken(SECURITY_MAX_IMPERSONATION_LEVEL,TOKEN_ALL_ACCESS);
    Token1.SetThreadToken(0);

    Token2 := TJwSecurityToken.CreateTokenByThread(GetCurrentThread,TOKEN_READ,false);
    Token2.Free;

    Token1.Free;

    TJwSecurityToken.RemoveThreadToken(0);

    try
      Token1 := TJwSecurityToken.CreateTokenByThread(0,TOKEN_ALL_ACCESS,true);
      CheckIs(nil,EJwsclNoThreadTokenAvailable);
    except
      on E : Exception do
        CheckIs(E,EJwsclNoThreadTokenAvailable);
    end;
  finally
    if Assigned(oldToken) then
      oldToken.SetThreadToken(0);
    oldToken.Free;
  end;

end;

procedure TSecurityTokenTests.Test_GetTokenPrivileges;
var Token1, Token2 : TJwSecurityToken;
    ThreadID : Cardinal;
    hThread : THandle;
    ThreadResult : Integer;
    Privs, Privs2 : TJwPrivilegeSet;
    i : Integer;
begin
  Token1 := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);

  Privs := Token1.GetTokenPrivileges;
  CheckNotNull(Privs);
  CheckNotEquals(0,Privs.Count);
  ShowMessageForm(Privs.GetText);


  Privs2 := TJwPrivilegeSet.Create;

  for i := 1 to Privs.Count-1 do
    Privs2.AddPrivilege(Privs.PrivByIdx[i].LUID);



  Token2 := TJwSecurityToken.CreateRestrictedToken(Token1.TokenHandle,TOKEN_ALL_ACCESS, 0, nil, Privs2, nil);
  Privs2.Free;

  Privs2 := Token2.GetTokenPrivileges;
  CheckNotNull(Privs);
  CheckNotEquals(0,Privs2.Count);

  ShowMessageForm(Privs2.GetText);

  Privs2.Free;
  Privs.Free;

  Token2.Free;
  Token1.Free;
end;

procedure TSecurityTokenTests.Test_SetTokenGroupsAttributes;
var Token : TJwSecurityToken;
    i : Integer;
    groups : TJwSecurityIdList;

begin
  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);

  try
    groups := Token.TokenGroups;
    //we just simply change nothing - it must work!
    Token.TokenGroups := groups;
  finally
    FreeAndNil(groups);
    FreeAndNil(Token);
  end;

end;



initialization

  TestFramework.RegisterTest('JwsclTokenTests Suite',
    TSecurityTokenTests.Suite);
  TestFramework.RegisterTest('JwsclTokenTests Suite',
    TSecurityTokenStatisticsTests.Suite);
  TestFramework.RegisterTest('JwsclTokenTests Suite',
    TPrivilegeTests.Suite);
  TestFramework.RegisterTest('JwsclTokenTests Suite',
    TPrivilegeSetTests.Suite);

end.
