unit JwsclCoInheritTypeList;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwInheritTypeList = class(TAutoObject, IJwInheritTypeList)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwInheritTypeList, Class_JwInheritTypeList,
    ciMultiInstance, tmApartment);
end.
