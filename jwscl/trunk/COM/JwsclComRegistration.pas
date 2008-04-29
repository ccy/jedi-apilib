unit JwsclComRegistration;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl, JwsclCoGenericList;



implementation

uses ComServ,
     JwsclCoLogging;

initialization
  TAutoObjectFactory.Create(ComServer, TJwLogServerImpl, CLASS_JwLogServer,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwLogClientImpl, CLASS_JwLogClient,
    ciMultiInstance, tmApartment);

  TAutoObjectFactory.Create(ComServer, TInternalJwEventType, CLASS_JwEventType,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TInternalJwXMLAttribute, CLASS_JwXMLAttribute,
    ciMultiInstance, tmApartment);

    TAutoObjectFactory.Create(ComServer, TJwGenericList, Class_JwGenericList,
    ciMultiInstance, tmApartment);

end.
