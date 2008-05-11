unit XPElevationDLLImplementation;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, ComObj, XPElevationDLL_TLB, StdVcl;

type
  TXPElevation = class(TTypedComObject, IXPElevation)
  protected
    function CreateProcess(const Application, Parameters: WideString;
      StartInfo: Integer; out PID: Integer): HResult; stdcall;
    {IXPElevation-Methoden hier deklarieren}
  end;

implementation

uses ComServ;

function TXPElevation.CreateProcess(const Application, Parameters: WideString;
  StartInfo: Integer; out PID: Integer): HResult;
begin

end;

initialization
  TTypedComObjectFactory.Create(ComServer, TXPElevation, Class_XPElevation,
    ciMultiInstance, tmApartment);
end.
