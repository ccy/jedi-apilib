unit JwsclCoObjectTypeList;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwObjectTypeList = class(TAutoObject, IJwObjectTypeList)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwObjectTypeList, Class_JwObjectTypeList,
    ciMultiInstance, tmApartment);
end.
