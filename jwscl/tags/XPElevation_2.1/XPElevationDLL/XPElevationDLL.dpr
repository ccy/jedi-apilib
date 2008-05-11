library XPElevationDLL;

uses
  ComServ,
  XPElevationDLL_TLB in 'XPElevationDLL_TLB.pas',
  XPElevationDLLImplementation in 'XPElevationDLLImplementation.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin
end.
