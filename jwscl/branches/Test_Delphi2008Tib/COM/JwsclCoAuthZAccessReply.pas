unit JwsclCoAuthZAccessReply;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwAuthZAccessReply = class(TAutoObject, IJwAuthZAccessReply)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwAuthZAccessReply, Class_JwAuthZAccessReply,
    ciMultiInstance, tmApartment);
end.
