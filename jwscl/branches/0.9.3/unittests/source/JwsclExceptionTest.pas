unit JwsclExceptionTest;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, SysUtils, D5impl, jwaWindows, Classes, JwsclExceptions, JwsclStrings,
  JwsclResource, JwsclTypes;

type
  // Testmethoden für Klasse EJwsclSecurityException

  TestEJwsclSecurityException = class(TTestCase)
  strict private
    FEJwsclSecurityException: array[1..2] of EJwsclSecurityException;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure SetDefaultJwsclValues(const E : Exception);
  published
    procedure TestStream;
    procedure TestCreateExceptionFromStream;
  end;

implementation

procedure TestEJwsclSecurityException.SetDefaultJwsclValues(const E: Exception);
begin
  if E is EJwsclSecurityException then
  begin
    (E as EJwsclSecurityException).LastError := high(DWORD);
    (E as EJwsclSecurityException).SourceProc    := 'XXXXXXXXXX';
    (E as EJwsclSecurityException).SourceClass   := 'YYYYYYYYYY';
    (E as EJwsclSecurityException).SourceFile    := 'ZZZZZZZZZZ';
    (E as EJwsclSecurityException).SourceLine    := high(DWORD);
    (E as EJwsclSecurityException).SimpleMessage := 'AAAAAAAAAAAA';
    (E as EJwsclSecurityException).WinCallName   := 'BBBBBBBBBBBBBB';
    (E as EJwsclSecurityException).Log           := 'CCCCCCCCCCCCCCCCC';
    (E as EJwsclSecurityException).ComSource     := 'DDDDDDDDDDDDDDDDDDDDD';
    (E as EJwsclSecurityException).StackTrace    := 'EEEEEEEEEEEEEEEEEEEEEEEE';
  end;
end;

procedure TestEJwsclSecurityException.SetUp;
begin
  FEJwsclSecurityException[1] := EJwsclSecurityException.Create('');
  FEJwsclSecurityException[2] := EJwsclSecurityException.Create('');

  SetDefaultJwsclValues(FEJwsclSecurityException[2]);
end;

procedure TestEJwsclSecurityException.TearDown;
begin
  FEJwsclSecurityException[1].Free;
  FEJwsclSecurityException[2].Free;

end;

procedure TestEJwsclSecurityException.TestCreateExceptionFromStream;
var Ex : Exception;
    Mem : TMemoryStream;
  i : Integer;
  E1 : EJwsclInvalidIndexPrivilegeException;
begin
  Mem := TMemoryStream.Create;
  FEJwsclSecurityException[1].SaveToStream(Mem);
  Mem.Position := 0;

  Ex := EJwsclExceptionClass.CreateExceptionFromStream(Mem);
  CheckIs(Ex, EJwsclSecurityException);
  Ex.Free;

  Mem.Clear;

  E1 := EJwsclInvalidIndexPrivilegeException.Create('');
  E1.SaveToStream(Mem);
  Mem.Position := 0;

  Ex := EJwsclExceptionClass.CreateExceptionFromStream(Mem);
  CheckIs(Ex, EJwsclPrivilegeException);
  CheckIs(Ex, EJwsclInvalidIndexPrivilegeException);
  Ex.Free;



  Mem.Free;
end;

procedure TestEJwsclSecurityException.TestStream;
var Mem : TMemoryStream;
begin
  Mem := TMemoryStream.Create;




  FEJwsclSecurityException[1].SaveToStream(Mem);

  Mem.Position := 0;

  FEJwsclSecurityException[2].LoadFromStream(Mem);

  CheckEquals(FEJwsclSecurityException[1].LastError         , FEJwsclSecurityException[2].LastError, 'LastError');
  CheckEqualsString(FEJwsclSecurityException[1].SourceProc        , FEJwsclSecurityException[2].SourceProc, 'SourceProc');
  CheckEqualsString(FEJwsclSecurityException[1].SourceClass       , FEJwsclSecurityException[2].SourceClass, 'SourceClass');
  CheckEqualsString(FEJwsclSecurityException[1].SourceFile        , FEJwsclSecurityException[2].SourceFile, 'SourceFile');
  CheckEquals(FEJwsclSecurityException[1].SourceLine        , FEJwsclSecurityException[2].SourceLine, 'SourceLine');
  CheckEqualsString(FEJwsclSecurityException[1].SimpleMessage     , FEJwsclSecurityException[2].SimpleMessage, 'SimpleMessage');
  CheckEqualsString(FEJwsclSecurityException[1].WinCallName       , FEJwsclSecurityException[2].WinCallName, 'WinCallName');
  CheckEqualsString(FEJwsclSecurityException[1].Log               , FEJwsclSecurityException[2].Log, 'Log');
  CheckEqualsString(FEJwsclSecurityException[1].ComSource         , FEJwsclSecurityException[2].ComSource, 'ComSource');
  CheckEqualsString(FEJwsclSecurityException[1].StackTrace        , FEJwsclSecurityException[2].StackTrace, 'StackTrace');
  CheckEqualsString('', FEJwsclSecurityException[1].UnsupportedProperties,'UnsupportedProperties');


  Mem.Free;
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestEJwsclSecurityException.Suite);
end.

