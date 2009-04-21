// Uncomment the following directive to create a console application
// or leave commented to create a GUI application...
// {$APPTYPE CONSOLE}

program DescriptorUnitTest;


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
  FastMm4,
  jwaWindows,
  JwaVista,
  SysUtils,
  Dialogs,
  Forms,
  Classes,
  Windows,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  UMessageForm in '..\..\source\UMessageForm.pas' {frmMessage},
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
  JwsclSimpleDescriptor,
  JwsclAccounts,
  JwsclDescriptorTests in '..\..\source\JwsclDescriptorTests.pas',
  JwsclSimpleDescriptorTests in '..\..\source\JwsclSimpleDescriptorTests.pas';

//never ever use JwsclLibrary and one of the Jwscl units at the same time!!

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
 

