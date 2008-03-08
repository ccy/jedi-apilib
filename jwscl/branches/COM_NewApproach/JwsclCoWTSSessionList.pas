unit JwsclCoWTSSessionList;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwWTSSessionList = class(TAutoObject, IJwWTSSessionList)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwWTSSessionList, Class_JwWTSSessionList,
    ciMultiInstance, tmApartment);
end.
