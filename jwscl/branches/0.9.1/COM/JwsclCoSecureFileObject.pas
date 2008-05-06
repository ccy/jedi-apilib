unit JwsclCoSecureFileObject;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwSecureFileObject = class(TAutoObject, IJwSecureFileObject)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwSecureFileObject, Class_JwSecureFileObject,
    ciMultiInstance, tmApartment);
end.
