unit JwsclCoLSALogonSession;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwLSALogonSession = class(TAutoObject, IJwLSALogonSession)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwLSALogonSession, Class_JwLSALogonSession,
    ciMultiInstance, tmApartment);
end.
