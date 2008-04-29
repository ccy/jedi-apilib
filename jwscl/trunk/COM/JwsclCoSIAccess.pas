unit JwsclCoSIAccess;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwSiAccess = class(TAutoObject, IJwSiAccess)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwSiAccess, Class_JwSiAccess,
    ciMultiInstance, tmApartment);
end.
