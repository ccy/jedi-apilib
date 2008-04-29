unit JwsclCoExplicitAccessArray;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwExplicitAccessArray = class(TAutoObject, IJwExplicitAccessArray)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwExplicitAccessArray, Class_JwExplicitAccessArray,
    ciMultiInstance, tmApartment);
end.
