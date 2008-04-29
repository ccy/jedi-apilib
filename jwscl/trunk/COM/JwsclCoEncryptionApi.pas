unit JwsclCoEncryptionApi;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwEncryptionApi = class(TAutoObject, IJwEncryptionApi)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwEncryptionApi, Class_JwEncryptionApi,
    ciMultiInstance, tmApartment);
end.
