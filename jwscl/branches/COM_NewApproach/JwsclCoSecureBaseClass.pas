unit JwsclCoSecureBaseClass;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwSecureBaseClass = class(TAutoObject, IJwSecureBaseClass)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwSecureBaseClass, Class_JwSecureBaseClass,
    ciMultiInstance, tmApartment);
end.
