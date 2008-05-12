unit XPElevationDLLImplementation;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, ActiveX, Classes, ComObj, XPElevationDLL_TLB, StdVcl;

type
  TXPElevation = class(TTypedComObject, IXPElevation)
  protected
    function ExecuteProcess(const ApplicationPath, Parameters,
      CurrentDirectory: WideString; Flags: Integer;
      out PID: Integer): HResult; stdcall;
    {IXPElevation-Methoden hier deklarieren}
  end;

implementation

uses ComServ;

function TXPElevation.ExecuteProcess(const ApplicationPath, Parameters,
  CurrentDirectory: WideString; Flags: Integer; out PID: Integer): HResult;
begin

end;

initialization
  TTypedComObjectFactory.Create(ComServer, TXPElevation, Class_XPElevation,
    ciMultiInstance, tmApartment);
end.
