unit JwsclCoWTSProcessList;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwWTSProcessList = class(TAutoObject, IJwWTSProcessList)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwWTSProcessList, Class_JwWTSProcessList,
    ciMultiInstance, tmApartment);
end.
