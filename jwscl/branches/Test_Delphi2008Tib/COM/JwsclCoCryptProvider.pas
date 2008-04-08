unit JwsclCoCryptProvider;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwCryptProvider = class(TAutoObject, IJwCryptProvider)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwCryptProvider, Class_JwCryptProvider,
    ciMultiInstance, tmApartment);
end.
