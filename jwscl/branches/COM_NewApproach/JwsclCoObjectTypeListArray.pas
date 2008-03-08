unit JwsclCoObjectTypeListArray;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwObjectTypeListArray = class(TAutoObject, IJwObjectTypeListArray)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwObjectTypeListArray, Class_JwObjectTypeListArray,
    ciMultiInstance, tmApartment);
end.
