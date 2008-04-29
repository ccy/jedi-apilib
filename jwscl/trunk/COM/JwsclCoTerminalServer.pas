unit JwsclCoTerminalServer;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwTerminalServer = class(TAutoObject, IJwTerminalServer)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwTerminalServer, Class_JwTerminalServer,
    ciMultiInstance, tmApartment);
end.
