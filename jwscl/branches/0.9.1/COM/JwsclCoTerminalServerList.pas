unit JwsclCoTerminalServerList;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwTerminalServerList = class(TAutoObject, IJwTerminalServerList)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwTerminalServerList, Class_JwTerminalServerList,
    ciMultiInstance, tmApartment);
end.
