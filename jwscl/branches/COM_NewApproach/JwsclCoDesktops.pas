unit JwsclCoDesktops;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwDesktops = class(TAutoObject, IJwDesktops)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwDesktops, Class_JwDesktops,
    ciMultiInstance, tmApartment);
end.
