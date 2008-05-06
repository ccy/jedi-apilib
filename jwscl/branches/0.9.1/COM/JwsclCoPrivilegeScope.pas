unit JwsclCoPrivilegeScope;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwPrivilegeScope = class(TAutoObject, IJwPrivilegeScope)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwPrivilegeScope, Class_JwPrivilegeScope,
    ciMultiInstance, tmApartment);
end.
