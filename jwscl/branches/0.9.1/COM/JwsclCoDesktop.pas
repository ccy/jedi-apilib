unit JwsclCoDesktop;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwDesktop = class(TAutoObject, IJwDesktop)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwDesktop, Class_JwDesktop,
    ciMultiInstance, tmApartment);
end.
