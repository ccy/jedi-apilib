unit JwsclCoKeyRootTupleArray;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwKeyRootTupleArray = class(TAutoObject, IJwKeyRootTupleArray)
  protected

  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TJwKeyRootTupleArray, Class_JwKeyRootTupleArray,
    ciMultiInstance, tmApartment);
end.
