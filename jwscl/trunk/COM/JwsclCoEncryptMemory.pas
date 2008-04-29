unit JwsclCoEncryptMemory;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwEncryptMemory = class(TAutoObject, IJwEncryptMemory)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwEncryptMemory, Class_JwEncryptMemory,
    ciMultiInstance, tmApartment);
end.
