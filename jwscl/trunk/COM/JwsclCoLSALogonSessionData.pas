unit JwsclCoLSALogonSessionData;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwLSALogonSessionData = class(TAutoObject, IJwLSALogonSessionData)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwLSALogonSessionData, Class_JwLSALogonSessionData,
    ciMultiInstance, tmApartment);
end.
