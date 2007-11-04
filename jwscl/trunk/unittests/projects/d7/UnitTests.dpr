// Uncomment the following directive to create a console application
// or leave commented to create a GUI application...
// {$APPTYPE CONSOLE}

program UnitTests;


{.$APPTYPE CONSOLE}

{Define one of these directive to use memory leak manager}
{.$DEFINE FASTMM4}
{$UNDEF FASTMM4}

{$IFDEF FASTMM4}
  {$UNDEF MEMCHECK}
  {$DEFINE LogErrorsToFile}
  {$DEFINE FullDebugMode}
  {$DEFINE LogMemoryLeakDetailToFile}
  {$DEFINE ManualLeakReportingControl}
{$ENDIF}

{$IFDEF EUREKALOG}
  {$UNDEF FASTMM4}
  {$UNDEF MEMCHECK}
  {$UNDEF FullDebugMode}
{$ENDIF EUREKALOG}


uses
{$IFDEF FASTMM4}
  FastMm4,
{$ENDIF FASTMM4}  
  //JEDI API LIB
  jwaWindows,
  JwaVista,
  
  //VCL
  SysUtils,
  Dialogs,
  Forms,
  Classes,
  Windows, //only for TKeyboardState 

  //DUNIT
  TestFramework,
  GUITestRunner,
  TextTestRunner,

  //Tools
  UMessageForm in '..\..\source\UMessageForm.pas' {frmMessage},

  //JEDI WSCL
  JwsclResource,
  JwsclAcl,
  JwsclConstants,
  JwsclCredentials,
  JwsclDescriptor,
  JwsclDesktops,
  JwsclExceptions,
  JwsclImpersonation,
  JwsclComUtils,
  JwsclKnownSid,
  JwsclLsa,
  JwsclMapping,
  JwsclProcess,
  JwsclSecureObjects,
  JwsclSecurityDialogs,
  JwsclSid,
  JwsclStrings,
  JwsclToken,
  JwsclTypes,
  JwsclUtils,
  JwsclVersion,
  JwsclEnumerations,
  JwsclWinStations,
  JwsclEncryption,
  JwsclPrivileges,
  JwsclTerminalServer,
  JwsclSecurePrivateObjects,
  JwsclSecureUserObjects,

  //JWSCL Unit Tests
  JwsclAclTests in '..\..\source\JwsclAclTests.pas',
  JwsclDescriptorTests in '..\..\source\JwsclDescriptorTests.pas',
  JwsclDesktopsTests in '..\..\source\JwsclDesktopsTests.pas',
  JwsclExceptionsTests in '..\..\source\JwsclExceptionsTests.pas',
  JwsclMappingTests in '..\..\source\JwsclMappingTests.pas',
  JwsclSecurityDialogsTests in '..\..\source\JwsclSecurityDialogsTests.pas',
  JwsclSecureObjectsTests in '..\..\source\JwsclSecureObjectsTests.pas',
  JwsclSIDTests in '..\..\source\JwsclSIDTests.pas',
  JwsclTokenTests in '..\..\source\JwsclTokenTests.pas',
  JwsclUnitUtilsTests in '..\..\source\JwsclUnitUtilsTests.pas',
  JwsclSecurePrivateObjectsTests in '..\..\source\JwsclSecurePrivateObjectsTests.pas';


//never ever use JwsclLibrary and one of the Jwscl units at the same time!!

{.$R *.RES}
                               
function StrSIDToName(const StrSID: widestring; var Name: wideString; var SIDType: DWORD): Boolean;
var
  SID               : PSID;
  Buffer,B2            : PwideChar;
  NameLen, TempLen  : Cardinal;
  success            : Boolean;
  TS : TJwSecurityId;
  res : Integer;
begin
  SID := nil;
  success := true;

  success := ConvertStringSidToSidW(PWideChar(StrSID), jwaWindows.PSID(SID));
  if success then
  begin
    NameLen := 0;
    TempLen := 0;
    SetLastError(0);
    if not LookupAccountSidW(nil, SID, nil, NameLen, nil, TempLen, SIDType) and
       (GetLastError() = ERROR_INSUFFICIENT_BUFFER) then
    begin
      GetMem(Buffer, NameLen * sizeOf(wideChar));
      GetMem(B2, TempLen * sizeOf(wideChar));


      if (Buffer <> nil) then
      try
        SetLastError(0);
        success := LookupAccountSidW(nil, SID, Buffer, NameLen, B2, TempLen, SIDType);
        if not success then
          RaiseLastOSError;
        if success then
          SetString(Name, Buffer, Namelen);
      finally
        FreeMem(Buffer);
        FreeMem(B2);
      end;
    end
    else
      RaiseLastOSError;
  end
  else
    RaiseLastOSError;

  result := success;
end;

var Prompt : TJwCredentialsPrompt;
    sUser, sDomain, sPass : TJwString;
    Token,t2 : TJwSecurityToken;
    s : TShiftState;
    k : TKeyboardState;
    Name: wideString;
    SIDType: DWORD;
    StrSID: Widestring;
begin
 // StrSIDToName('S-1-5-32-544',Name,SIDType);
  JwInitWellKnownSIDs;

  GetKeyboardState(K);
  s := KeyboardStateToShiftState(k);
  if (ssShift in s) {or (ParamStr(1) <> '') }then
  if true then
  begin
    Prompt := TJwCredentialsPrompt.Create;
    Prompt.UserName := 'Administrator';
    Prompt.Password := paramstr(1);
    Prompt.Caption := 'Security Library Testcenter - Elevation required';
    Prompt.MessageText := '';
    Prompt.Flags := [cfFlagsDoNotPersist, cfFlagsGenericCredentials];

    try
      if not Prompt.ShowModal(false) then
      begin
        FreeAndNil(Prompt);
        sUser := '';
      end
      else
      begin
        //parse domain and username from Prompt.UserName
        //TJwCredentialsTools.ParseUserName(Prompt.UserName, sUser, sDomain);
        Prompt.ParseInstanceUserName(sUser, sDomain);

        sPass := Prompt.Password;
      end;
    except
      sUser := '';
      FreeAndNil(Prompt);
    end;
  end
  else
  if (ParamStr(1) <> '') then
  begin
    sUser := 'Administrator';
    sPass := ParamStr(1);
  end;

  FreeAndNil(Prompt);
  
    Token := nil;
    //Logon user
    if (sUser <> '') then
    try
       Token := TJwSecurityToken.CreateLogonUser(sUser, sDomain, sPass, LOGON32_LOGON_INTERACTIVE,0);
    except
      on E : EJwsclSecurityException do
      begin
        MessageDlg(E.Message,mtError,[mbok],0);
        exit;
      end
      else
        raise;
    end;

  if Assigned(Token) then
    begin
    Token.ConvertToImpersonatedToken(jwaWindows.SecurityImpersonation,
     (*//TOKEN_DUPLICATE or
     TOKEN_WRITE or
     TOKEN_READ or
     //TOKEN_EXECUTE or
     TOKEN_ADJUST_PRIVILEGES or
     //TOKEN_ASSIGN_PRIMARY or
     TOKEN_IMPERSONATE*)
     TOKEN_ALL_ACCESS);
     Token.SetThreadToken(0);

     //t2 := TJwSecurityToken.CreateTokenByThread(0,GENERIC_READ,true);

     //Token.ImpersonateLoggedOnUser;
  end;

  {$IFDEF FASTMM4}
  {$IFNDEF COMPILER_8_UP}FastMM4.{$ENDIF}ReportMemoryLeaksOnShutdown := true;
  {$ENDIF}

   Application.Initialize;

{$IFDEF LINUX}
  QGUITestRunner.RunRegisteredTests;
{$ELSE}
  if System.IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
{$ENDIF}

  Token.Free;
  //clear password safely
  sUser := 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX';
  sPass := 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX';
  //free memory of strings (otherwise leak manager will report it)
  sUser := '';
  sPass := '';
end.
 

