unit JwsclCoEnumAlgorithmsEntry;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwEnumAlgorithmsEntry = class(TAutoObject, IJwEnumAlgorithmsEntry)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwEnumAlgorithmsEntry, Class_JwEnumAlgorithmsEntry,
    ciMultiInstance, tmApartment);
end.
