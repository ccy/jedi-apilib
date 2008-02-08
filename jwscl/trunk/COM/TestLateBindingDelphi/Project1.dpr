program Project1;

{$APPTYPE CONSOLE}

uses
  ActiveX,
  ComObj,
  SysUtils;

var obj : Variant;
begin
  CoInitialize(0);

  obj := CreateOleObject('JWSCLCom.JwCoSid');
  obj.InitByName('','Christian');
  { TODO -oUser -cConsole Main : Hier Code einfügen }
end.
 