unit JwsclCoLoggingTests;

interface

uses
  ComObj, ActiveX, StdVcl, Classes, JWSCLCom_TLB,
  JwaWindows,JwaVista, TypInfo, ComLib,
  JWSCLSid, JwsclMapping,
  TestFrameWork;

type
  TJwLogClientImplTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods

    procedure Test;

  end;



implementation

{ TJwLogClientImplTests }

procedure TJwLogClientImplTests.Test;
var Server : IJwLogServer;
  procedure Test2();
  var C : IJwLogClient;
  begin
    C := Server.Connect(etFunction,'','Test2','','123');
    C.Log(lsMessage, '123');
  end;
begin
  Server := CoJwLogServer.Create;
  Test2;

  
end;



initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TJwLogClientImplTests.Suite);

end.
 