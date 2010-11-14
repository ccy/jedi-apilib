unit JwsclCoExplicitAccess;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwExplicitAccess = class(TAutoObject, IJwExplicitAccess)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwExplicitAccess, Class_JwExplicitAccess,
    ciMultiInstance, tmApartment);
end.
