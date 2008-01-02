unit JwsclExceptionsTests;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, JwsclExceptions, jwaWindows, Classes, Dialogs,SysUtils, JwsclStrings,
  JwsclResource, JwsclUtils, JwsclTypes;
type
  // Testmethoden für Klasse EJwsclSecurityException
  
  TestEJwsclSecurityException = class(TTestCase)
  private
    FEJwsclSecurityException: EJwsclSecurityException;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Test_ctor;
    procedure Test_ctor1;
    procedure Test_ctor2;
    procedure Test_ctor3;
    procedure Test_ctor4;
    procedure TestGetErrorMessage;
    procedure TestGetLastErrorMessage;
  end;

implementation

procedure TestEJwsclSecurityException.SetUp;
begin
  //FEJwsclSecurityException := EJwsclSecurityException.Create;
  FEJwsclSecurityException := nil;
end;

procedure TestEJwsclSecurityException.TearDown;
begin
  FEJwsclSecurityException.Free;
  FEJwsclSecurityException := nil;
end;

procedure TestEJwsclSecurityException.Test_ctor;
var
  Msg: string;
begin
  // TODO: Methodenaufrufparameter einrichten
  FEJwsclSecurityException := EJwsclSecurityException.CreateFmtEx(
    '%0:s',//const MessageString: string;
    'ss',//sSourceProc,
    ClassName,//sSourceClass,
    'File',//sSourceFile: string;
    123,//iSourceLine: Cardinal;
    123,//iLastError: Cardinal;
    ['MessageString']//const Args: array of const
   );
  // TODO: Methodenergebnisse prüfen
  ShowMessage(FEJwsclSecurityException.Message);
end;

procedure TestEJwsclSecurityException.Test_ctor1;
{var
  Args: $1;
  bShowLastError: Boolean;
  iSourceLine: Cardinal;
  sSourceFile: string;
  sSourceClass: string;
  sSourceProc: string;
  MessageString: string;  }
begin
  // TODO: Methodenaufrufparameter einrichten
  FEJwsclSecurityException := EJwsclSecurityException.CreateFmtEx(
    '%0:s',//const MessageString: string;
    'Test_ctor1',ClassName,//sSourceProc, sSourceClass,
    '',//sSourceFile: string;
    0,//iSourceLine:  Cardinal;
    true,//bShowLastError: boolean;
    ['MessageString']//const Args: array of const
   );
   ShowMessage(FEJwsclSecurityException.Message);
{  FEJwsclSecurityException..ctor(MessageString, sSourceProc, sSourceClass, sSourceFile,
      iSourceLine, bShowLastError, Args);   }
  // TODO: Methodenergebnisse prüfen
end;

procedure TestEJwsclSecurityException.Test_ctor2;
{var
  Args: $2;
  iLastError: Cardinal;
  iSourceLine: Cardinal;
  sSourceFile: string;
  sSourceClass: string;
  sSourceProc: string;
  MessageString: string;  }
begin
  // TODO: Methodenaufrufparameter einrichten
 { FEJwsclSecurityException.ctor(MessageString, sSourceProc, sSourceClass, sSourceFile,
      iSourceLine, iLastError, Args);        }
  // TODO: Methodenergebnisse prüfen
end;

procedure TestEJwsclSecurityException.Test_ctor3;
{var
  Args: $3;
  sWinCall: string;
  bShowLastError: Boolean;
  iSourceLine: Cardinal;
  sSourceFile: string;
  sSourceClass: string;
  sSourceProc: string;
  sMsg: string;          }
begin
  // TODO: Methodenaufrufparameter einrichten
 { FEJwsclSecurityException.Create(sMsg, sSourceProc, sSourceClass, sSourceFile,
      iSourceLine, bShowLastError, sWinCall, Args);    }
  // TODO: Methodenergebnisse prüfen
end;

procedure TestEJwsclSecurityException.Test_ctor4;
var
  anException: EJwsclSecurityException;
begin
  // TODO: Methodenaufrufparameter einrichten
 { FEJwsclSecurityException.ctor(anException);    }
  // TODO: Methodenergebnisse prüfen
end;

procedure TestEJwsclSecurityException.TestGetErrorMessage;
var
  ReturnValue: TJwString;
  errNumber: TJwLastError;
begin
  // TODO: Methodenaufrufparameter einrichten
  {ReturnValue := FEJwsclSecurityException.GetErrorMessage(errNumber);    }
  // TODO: Methodenergebnisse prüfen
end;

procedure TestEJwsclSecurityException.TestGetLastErrorMessage;
var
  ReturnValue: TJwString;
  iGetLastError: Cardinal;
begin
  // TODO: Methodenaufrufparameter einrichten
 { ReturnValue := FEJwsclSecurityException.GetLastErrorMessage(iGetLastError); }
  // TODO: Methodenergebnisse prüfen
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestEJwsclSecurityException.Suite);
end.

