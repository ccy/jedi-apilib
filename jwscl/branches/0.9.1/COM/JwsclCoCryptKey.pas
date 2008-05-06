unit JwsclCoCryptKey;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwCryptKey = class(TAutoObject, IJwCryptKey)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwCryptKey, Class_JwCryptKey,
    ciMultiInstance, tmApartment);
end.
