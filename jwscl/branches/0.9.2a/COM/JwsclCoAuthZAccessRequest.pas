unit JwsclCoAuthZAccessRequest;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwAuthZAccessRequest = class(TAutoObject, IJwAuthZAccessRequest)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwAuthZAccessRequest, Class_JwAuthZAccessRequest,
    ciMultiInstance, tmApartment);
end.
