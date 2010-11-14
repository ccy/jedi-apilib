unit JwsclCoAccessMaskArray;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwAccessMaskArray = class(TAutoObject, IJwAccessMaskArray)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwAccessMaskArray, Class_JwAccessMaskArray,
    ciMultiInstance, tmApartment);
end.
