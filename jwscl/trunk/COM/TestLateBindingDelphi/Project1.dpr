program Project1;

{$APPTYPE CONSOLE}

uses
  JwaWindows,
  ActiveX,
  ComObj,
  SysUtils;

procedure JwOleRaise(const Res : HRESULT); stdcall; external '..\JWSCLCom.dll';
function JwHasException(const Res : HRESULT) : BOOL; stdcall; external '..\JWSCLCom.dll';

var obj : Variant;
begin
  CoInitialize(0);

  obj := CreateOleObject('JWSCLCom.JwCoSid');
  JwOleRaise(obj.InitByName('','Christian'));

  Writeln(obj.StringSid);
end.
 