unit JwsclCoWindowStations;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwWindowStations = class(TAutoObject, IJwWindowStations)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwWindowStations, Class_JwWindowStations,
    ciMultiInstance, tmApartment);
end.
