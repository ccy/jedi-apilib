unit JwsclCoLSA;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwLSA = class(TAutoObject, IJwLSA)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwLSA, Class_JwLSA,
    ciMultiInstance, tmApartment);
end.
