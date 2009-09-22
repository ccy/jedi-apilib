unit JwsclCoKnownSIDs;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwKnownSIDs = class(TAutoObject, IJwKnownSIDs)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwKnownSIDs, Class_JwKnownSIDs,
    ciMultiInstance, tmApartment);
end.
