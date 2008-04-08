unit JwsclCoProcessUtils;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwProcessUtils = class(TAutoObject, IJwProcessUtils)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwProcessUtils, Class_JwProcessUtils,
    ciMultiInstance, tmApartment);
end.
