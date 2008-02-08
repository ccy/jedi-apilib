library JWSCLCom;

uses
  ComServ,
  JWSCLCom_TLB in 'JWSCLCom_TLB.pas',
  JwsclCoSid in 'JwsclCoSid.pas' {JwCoSid: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;



{$R *.TLB}

{$R *.RES}

begin
end.
