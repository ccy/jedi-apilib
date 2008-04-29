unit JwsclCoWTSSession;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwWTSSession = class(TAutoObject, IJwWTSSession)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwWTSSession, Class_JwWTSSession,
    ciMultiInstance, tmApartment);
end.
