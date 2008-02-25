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
  JwsclAccounts,

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
begin
  JwInitWellKnownSIDs;

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
end.
 

