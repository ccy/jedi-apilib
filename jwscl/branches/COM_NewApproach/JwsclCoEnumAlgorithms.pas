unit JwsclCoEnumAlgorithms;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwEnumAlgorithms = class(TAutoObject, IJwEnumAlgorithms)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwEnumAlgorithms, Class_JwEnumAlgorithms,
    ciMultiInstance, tmApartment);
end.
