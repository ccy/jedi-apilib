unit JwsclCoRootTuple;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwRootTuple = class(TAutoObject, IJwRootTuple)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwRootTuple, Class_JwRootTuple,
    ciMultiInstance, tmApartment);
end.
