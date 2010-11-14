unit JwsclCoInheritedFromRecord;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwInheritedFromRecord = class(TAutoObject, IJwInheritedFromRecord)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwInheritedFromRecord, Class_JwInheritedFromRecord,
    ciMultiInstance, tmApartment);
end.
