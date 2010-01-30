unit JwIntegrityLevelTestUnit;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, JwsclConstants, Registry, JwsclMapping, Contnrs, SysUtils,
  JwsclKnownSid, JwsclAcl, JwsclToken, Dialogs, JwsclSid,
  JwsclUtils, JwsclTypes, Classes, JwsclSecureObjects, JwsclExceptions,
  jwaWindows, JwsclDescriptor, JwsclResource, JwsclVersion, JwsclStrings;

type
  // Testmethoden für Klasse TestTJwSecureFileObject

  TTestJwIntegrityLevel = class(TTestCase)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Create1;
    procedure Create2;
    procedure GetIL;
    procedure GetIncIL;
    procedure GetEffectiveIL;

  end;

implementation



{ TTestJwIntegrityLevel }

procedure TTestJwIntegrityLevel.Create1;
const
  LevelTypes : array[TJwIntegrityLabelType] of Cardinal =
    (0,  //iltNone
    4096, //iltLow
    8192,  //iltMedium
    12288, //iltHigh
    16384, //iltSystem
    20480); //iltProtected

begin
  CheckTrue(TJwIntegrityLevelSID.GetIL(1).LabelType = iltLow);
  CheckTrue(TJwIntegrityLevelSID.GetIL(4095).LabelType = iltLow);
  CheckTrue(TJwIntegrityLevelSID.GetIL(4096).LabelType = iltLow);

  CheckTrue(TJwIntegrityLevelSID.GetIL(8192).LabelType = iltMedium);
  CheckTrue(TJwIntegrityLevelSID.GetIL(8193).LabelType = iltMedium);
  CheckTrue(TJwIntegrityLevelSID.GetIL(12287).LabelType = iltMedium);

  CheckTrue(TJwIntegrityLevelSID.GetIL(12288).LabelType = iltHigh);
  CheckTrue(TJwIntegrityLevelSID.GetIL(16383).LabelType = iltHigh);

  CheckTrue(TJwIntegrityLevelSID.GetIL(16384).LabelType = iltSystem);
  CheckTrue(TJwIntegrityLevelSID.GetIL(20479).LabelType = iltSystem);

  CheckTrue(TJwIntegrityLevelSID.GetIL(20480).LabelType = iltProtected);
  CheckTrue(TJwIntegrityLevelSID.GetIL(Cardinal(-1)).LabelType = iltProtected);
end;

procedure TTestJwIntegrityLevel.Create2;
var K : TJwSecurityID;
begin
  //K := TJwSecurityId.Create(JwProtectedProcessIL);
  K := TJwSecurityThreadUserSID.Create;

  TJwIntegrityLevelSID.Create(K);
end;

procedure TTestJwIntegrityLevel.GetEffectiveIL;
begin
  TJwIntegrityLevelSID.GetEffectiveIL;
end;

procedure TTestJwIntegrityLevel.GetIL;
begin
  CheckTrue(TJwIntegrityLevelSID.GetIL(20480) = TJwIntegrityLevelSID.GetIL(20480));
end;

procedure TTestJwIntegrityLevel.GetIncIL;
begin
  CheckEquals(0, TJwIntegrityLevelSID.GetIL(10).CreateIncrement(-100).Level);

  CheckEquals(Cardinal(-1), TJwIntegrityLevelSID.GetIL(Cardinal(-1)-10).CreateIncrement(100).Level);
end;

procedure TTestJwIntegrityLevel.SetUp;
begin
  inherited;

end;

procedure TTestJwIntegrityLevel.TearDown;
begin
  inherited;

end;



initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TTestJwIntegrityLevel.Suite);
end.
