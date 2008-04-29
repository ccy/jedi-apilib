unit JwsclCoElevationClassFactory;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwElevationClassFactory = class(TAutoObject, IJwElevationClassFactory)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwElevationClassFactory, Class_JwElevationClassFactory,
    ciMultiInstance, tmApartment);
end.
