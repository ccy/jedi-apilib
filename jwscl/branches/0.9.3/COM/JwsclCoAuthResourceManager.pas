unit JwsclCoAuthResourceManager;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwAuthResourceManager = class(TAutoObject, IJwAuthResourceManager)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwAuthResourceManager, Class_JwAuthResourceManager,
    ciMultiInstance, tmApartment);
end.
