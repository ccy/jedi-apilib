unit JwsclCoSIDInfoData;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwSIDInfoData = class(TAutoObject, IJwSIDInfoData)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwSIDInfoData, Class_JwSIDInfoData,
    ciMultiInstance, tmApartment);
end.
