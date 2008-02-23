library JWSCLCom;

uses
  ComServ,
  JWSCLCom_TLB in 'JWSCLCom_TLB.pas',
  JwsclCoSid in 'JwsclCoSid.pas' {JwCoSid: CoClass},
  JwsclCOMExports in 'JwsclCOMExports.pas',
  JWSCLCoException in 'JWSCLCoException.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer,
  JwOleRaise,
  JwHasException,
  CreateSidAndAttributesStream
  ;



{$R *.TLB}

{$R *.RES}

begin
end.
