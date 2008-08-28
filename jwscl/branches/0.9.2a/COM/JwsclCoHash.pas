unit JwsclCoHash;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwHash = class(TAutoObject, IJwHash)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwHash, Class_JwHash,
    ciMultiInstance, tmApartment);
end.
