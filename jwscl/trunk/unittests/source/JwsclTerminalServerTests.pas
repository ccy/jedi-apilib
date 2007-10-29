unit JwsclTerminalServerTests;

interface

uses
  JwsclTerminalServer,
  TestFrameWork;

type
  TJwTerminalServerTests = class(TTestCase)
  private

  protected
    {
     SetUp()
      TestXXX()
     TearDown()
     for every function!!
    }
    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestClose;
    procedure TestOpen;
    procedure TestCreate;
    procedure TestEnumerate;
    procedure TestFileTime2DateTime;

  end;

type
  TJwWTSSessionTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods
    procedure TestCreate;
    procedure TestGetOwner;
    procedure TestGetServerHandle;
    procedure TestGetSessionInfoDWORD;
    procedure TestGetSessionInfoStr;
    procedure TestGetSessionInfoPtr;

  end;

type
  TJwWTSSessionListTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods
    procedure TestGetOwner;
    procedure TestGetItem;
    procedure TestCreate;

  end;

implementation

{ TJwTerminalServerTests }

procedure TJwTerminalServerTests.SetUp;
begin
  inherited;

end;

procedure TJwTerminalServerTests.TearDown;
begin
  inherited;

end;

procedure TJwTerminalServerTests.TestClose;
begin

end;

procedure TJwTerminalServerTests.TestCreate;
begin

end;

procedure TJwTerminalServerTests.TestEnumerate;
begin

end;

procedure TJwTerminalServerTests.TestFileTime2DateTime;
begin

end;

procedure TJwTerminalServerTests.TestOpen;
begin

end;

{ TJwWTSSessionTests }

procedure TJwWTSSessionTests.TestCreate;
begin

end;

procedure TJwWTSSessionTests.TestGetOwner;
begin

end;

procedure TJwWTSSessionTests.TestGetServerHandle;
begin

end;

procedure TJwWTSSessionTests.TestGetSessionInfoDWORD;
begin

end;

procedure TJwWTSSessionTests.TestGetSessionInfoPtr;
begin

end;

procedure TJwWTSSessionTests.TestGetSessionInfoStr;
begin

end;

{ TJwWTSSessionListTests }

procedure TJwWTSSessionListTests.TestCreate;
begin

end;

procedure TJwWTSSessionListTests.TestGetItem;
begin

end;

procedure TJwWTSSessionListTests.TestGetOwner;
begin

end;

initialization

  TestFramework.RegisterTest('JwsclTerminalServerTests Suite',
    TJwTerminalServerTests.Suite);
  TestFramework.RegisterTest('JwsclTerminalServerTests Suite',
    TJwWTSSessionTests.Suite);
  TestFramework.RegisterTest('JwsclTerminalServerTests Suite',
    TJwWTSSessionListTests.Suite);

end.
