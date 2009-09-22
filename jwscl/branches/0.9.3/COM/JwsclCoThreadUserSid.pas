unit JwsclCoThreadUserSid;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwThreadUserSid = class(TAutoObject, IJwThreadUserSid)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwThreadUserSid, Class_JwThreadUserSid,
    ciMultiInstance, tmApartment);
end.
