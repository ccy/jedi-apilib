unit JwsclCoStringArray;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwStringArray = class(TAutoObject, IJwStringArray)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwStringArray, Class_JwStringArray,
    ciMultiInstance, tmApartment);
end.
