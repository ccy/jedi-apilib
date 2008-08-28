unit JwsclCoAuthContext;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwAuthContext = class(TAutoObject, IJwAuthContext)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwAuthContext, Class_JwAuthContext,
    ciMultiInstance, tmApartment);
end.
