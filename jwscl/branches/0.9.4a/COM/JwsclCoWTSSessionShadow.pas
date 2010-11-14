unit JwsclCoWTSSessionShadow;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwWTSSessionShadow = class(TAutoObject, IJwWTSSessionShadow)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwWTSSessionShadow, Class_JwWTSSessionShadow,
    ciMultiInstance, tmApartment);
end.
