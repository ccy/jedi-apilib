unit JwsclCoSIDInfoDataArray;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwSIDInfoDataArray = class(TAutoObject, IJwSIDInfoDataArray)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwSIDInfoDataArray, Class_JwSIDInfoDataArray,
    ciMultiInstance, tmApartment);
end.
