unit JwsclCoEncryptData;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwEncryptData = class(TAutoObject, IJwEncryptData)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwEncryptData, Class_JwEncryptData,
    ciMultiInstance, tmApartment);
end.
