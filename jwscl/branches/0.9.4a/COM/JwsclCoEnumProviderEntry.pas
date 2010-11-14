unit JwsclCoEnumProviderEntry;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwEnumProviderEntry = class(TAutoObject, IJwEnumProviderEntry)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwEnumProviderEntry, Class_JwEnumProviderEntry,
    ciMultiInstance, tmApartment);
end.
