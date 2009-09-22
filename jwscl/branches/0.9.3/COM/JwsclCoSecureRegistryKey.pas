unit JwsclCoSecureRegistryKey;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwSecureRegistryKey = class(TAutoObject, IJwSecureRegistryKey)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwSecureRegistryKey, Class_JwSecureRegistryKey,
    ciMultiInstance, tmApartment);
end.
