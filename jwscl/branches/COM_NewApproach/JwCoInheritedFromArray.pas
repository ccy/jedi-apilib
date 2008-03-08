unit JwCoInheritedFromArray;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwInheritedFromArray = class(TAutoObject, IJwInheritedFromArray)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwInheritedFromArray, Class_JwInheritedFromArray,
    ciMultiInstance, tmApartment);
end.
