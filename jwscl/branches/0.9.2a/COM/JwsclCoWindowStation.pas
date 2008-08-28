unit JwsclCoWindowStation;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwWindowStation = class(TAutoObject, IJwWindowStation)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwWindowStation, Class_JwWindowStation,
    ciMultiInstance, tmApartment);
end.
