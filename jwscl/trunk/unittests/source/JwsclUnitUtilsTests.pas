unit JwsclUnitUtilsTests;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, JwsclExceptions, jwaWindows, Classes, Dialogs,SysUtils,
  JwsclResource, JwsclUtils, JwsclTypes, JwsclStrings, JwsclToken;
type
  // Testmethoden für Klasse EJwsclSecurityException

  TestUnitUtils = class(TTestCase)
  private
    FEJwsclSecurityException: EJwsclSecurityException;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLocalAllocMem;
    procedure TestMakeLocalAllocLeak;
    procedure TestGlobalAllocMem;
    procedure TestMakeGlobalAllocLeak;
    procedure TestCheckAdministratorAccess;


  end;

implementation



{ TestUnitUtils }

procedure TestUnitUtils.SetUp;
begin
  inherited;

end;

procedure TestUnitUtils.TearDown;
begin
  inherited;

end;

procedure TestUnitUtils.TestCheckAdministratorAccess;
begin
  if not JwCheckAdministratorAccess then
    Check(false,'Test failure is normal: User has not Administrator rights')
  else
    Check(false,'Test failure is normal: User has Administrator rights');
end;

procedure TestUnitUtils.TestGlobalAllocMem;
var Mem : HGLOBAL;
begin
  Mem := JwGlobalAllocMem(GPTR, 100);

  JwGlobalFreeMem(Mem);

end;

procedure TestUnitUtils.TestLocalAllocMem;
var Mem : HLocal;
begin
  Mem := JwLocalAllocMem(LPTR, 100);

  JwLocalFreeMem(Mem);

end;

procedure TestUnitUtils.TestMakeGlobalAllocLeak;
var Mem : HGLOBAL;
begin
  Mem := JwGlobalAllocMem(LPTR, 100);
end;

procedure TestUnitUtils.TestMakeLocalAllocLeak;
var Mem : HLocal;
begin
  Mem := JwLocalAllocMem(LPTR, 100);
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestUnitUtils.Suite);
end.

