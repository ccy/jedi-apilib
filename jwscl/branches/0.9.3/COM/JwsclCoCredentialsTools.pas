unit JwsclCoCredentialsTools;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwCredentialsTools = class(TAutoObject, IJwCredentialsTools)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwCredentialsTools, Class_JwCredentialsTools,
    ciMultiInstance, tmApartment);
end.
