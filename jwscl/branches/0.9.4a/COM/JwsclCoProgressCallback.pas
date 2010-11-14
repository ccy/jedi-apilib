unit JwsclCoProgressCallback;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, ComObj, JWSCLCom_TLB, StdVcl;

type
  TJwProgressCallback = class(TTypedComObject, IJwProgressCallback)
  protected
    {IJwProgressCallback-Methoden hier deklarieren}
  end;

implementation

uses ComServ;

initialization
  TTypedComObjectFactory.Create(ComServer, TJwProgressCallback, Class_JwProgressCallback,
    ciMultiInstance, tmApartment);
end.
