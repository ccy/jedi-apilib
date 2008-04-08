unit JwsclCoGuidArray;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwGuidArray = class(TAutoObject, IJwGuidArray)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwGuidArray, Class_JwGuidArray,
    ciMultiInstance, tmApartment);
end.
