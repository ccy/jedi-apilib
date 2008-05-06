{
VistaElevation shows how to use out of process COM elevation
in a Windows Vista system.

Version: 0.5
Release: 1.10.2007
Written by Christian Wimmer
}
program VistaElevation;
{$I JwsclUAC.inc}
{.$APPTYPE CONSOLE}

uses
  SysUtils, Activex, ComObj, VistaElevationDLL_TLB,
  JwaWindows,
  JwsclElevation;

var  ElevatedObject: IElevationDemoObject;
begin
  CoInitialize(0);

  OleCheck(
    JwCoCreateInstanceAsAdmin(
      GetForegroundWindow,
      CLASS_ElevationDemoObject,
      IID_IElevationDemoObject,
      ElevatedObject));

  ElevatedObject.DoSomething('Hello from JWSCL Vista Elevation Demo');
end.
