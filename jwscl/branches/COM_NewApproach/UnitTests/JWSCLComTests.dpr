// Uncomment the following directive to create a console application
// or leave commented to create a GUI application... 
// {$APPTYPE CONSOLE}

program JWSCLComTests;

uses
  FastMM4,
  TestFramework,
  Forms,
  GUITestRunner,
  TextTestRunner,
  TestJwsclCoSid in 'TestJwsclCoSid.pas',
  JWSCLCoException in '..\JWSCLCoException.pas',
  ComLib in '..\Common\ComLib.pas',
  TestsJwsclCoGenericList in 'TestsJwsclCoGenericList.pas';

{$R *.RES}

var P :Pointer;
begin
  //getmem(p, 100);
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

 