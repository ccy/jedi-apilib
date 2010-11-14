unit JwsclCoUtils;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwUtils = class(TAutoObject, IJwUtils)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwUtils, Class_JwUtils,
    ciMultiInstance, tmApartment);
end.
