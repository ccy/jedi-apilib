unit XPElevationControlImplementation;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, ComObj, XPElevationDLL_TLB, StdVcl;

type
  TXPElevationControl = class(TTypedComObject, IXPElevationControl)
  protected
    function Connect: HResult; stdcall;
    function Shutdown: HResult; stdcall;
    {IXPElevationControl-Methoden hier deklarieren}
  end;

implementation

uses ComServ;

function TXPElevationControl.Connect: HResult;
begin

end;

function TXPElevationControl.Shutdown: HResult;
begin

end;

initialization
  TTypedComObjectFactory.Create(ComServer, TXPElevationControl, Class_XPElevationControl,
    ciMultiInstance, tmApartment);
end.
