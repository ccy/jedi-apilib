unit JwsclCoImpersonation;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwImpersonation = class(TAutoObject, IJwImpersonation)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwImpersonation, Class_JwImpersonation,
    ciMultiInstance, tmApartment);
end.
