unit JwsclCoRandomDataGenerator;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwRandomDataGenerator = class(TAutoObject, IJwRandomDataGenerator)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwRandomDataGenerator, Class_JwRandomDataGenerator,
    ciMultiInstance, tmApartment);
end.
