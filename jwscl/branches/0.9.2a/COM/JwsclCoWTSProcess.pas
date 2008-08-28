unit JwsclCoWTSProcess;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwWTSProcess = class(TAutoObject, IJwWTSProcess)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwWTSProcess, Class_JwWTSProcess,
    ciMultiInstance, tmApartment);
end.
