unit JwsclCoSIAccessArray;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwSiAccessArray = class(TAutoObject, IJwSiAccessArray)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwSiAccessArray, Class_JwSiAccessArray,
    ciMultiInstance, tmApartment);
end.
