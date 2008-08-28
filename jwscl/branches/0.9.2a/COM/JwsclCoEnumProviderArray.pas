unit JwsclCoEnumProviderArray;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwEnumProviderArray = class(TAutoObject, IJwEnumProviderArray)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwEnumProviderArray, Class_JwEnumProviderArray,
    ciMultiInstance, tmApartment);
end.
