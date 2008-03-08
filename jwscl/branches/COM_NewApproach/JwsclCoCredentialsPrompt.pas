unit JwsclCoCredentialsPrompt;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwCredentialsPrompt = class(TAutoObject, IJwCredentialsPrompt)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwCredentialsPrompt, Class_JwCredentialsPrompt,
    ciMultiInstance, tmApartment);
end.
