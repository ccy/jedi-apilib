unit JwsclCoReplyErrorArray;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwReplyErrorArray = class(TAutoObject, IJwReplyErrorArray)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwReplyErrorArray, Class_JwReplyErrorArray,
    ciMultiInstance, tmApartment);
end.
