library XPElevationDLL;

{%File '..\Contextmenu\Kontextmenuhandler.dpr'}

uses
  ComServ,
  XPElevationDLL_TLB in 'XPElevationDLL_TLB.pas',
  XPElevationDLLImplementation in 'XPElevationDLLImplementation.pas' {XPElevation: CoClass},
  XPElevationControlImplementation in 'XPElevationControlImplementation.pas' {XPElevationControl: CoClass},
  XPElevationCommon in '..\XPElevationCommon.pas';

exports
  DllGetClassObject,                       
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}


begin
end.
