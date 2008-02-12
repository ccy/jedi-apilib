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
procedure Nop;
begin
end;

procedure TJwTerminalServerTests.SetUp;
begin
  inherited;

end;

procedure TJwTerminalServerTests.TearDown;
begin
  inherited;

end;

procedure Nop;
begin
end;

procedure TJwTerminalServerTests.TestClose;
begin
  Nop;
end;

procedure TJwTerminalServerTests.TestCreate;
begin
  Nop;

end;

procedure TJwTerminalServerTests.TestEnumerate;
begin
  Nop;

end;

procedure TJwTerminalServerTests.TestFileTime2DateTime;
begin
  Nop;

end;

procedure TJwTerminalServerTests.TestOpen;
begin
  Nop;

end;

{ TJwWTSSessionTests }

procedure TJwWTSSessionTests.TestCreate;
begin
  Nop;

end;

procedure TJwWTSSessionTests.TestGetOwner;
begin
  Nop;

end;

procedure TJwWTSSessionTests.TestGetServerHandle;
begin
  Nop;

end;

procedure TJwWTSSessionTests.TestGetSessionInfoDWORD;
begin
  Nop;

end;

procedure TJwWTSSessionTests.TestGetSessionInfoPtr;
begin
  Nop;

end;

procedure TJwWTSSessionTests.TestGetSessionInfoStr;
begin
  Nop;

end;

{ TJwWTSSessionListTests }

procedure TJwWTSSessionListTests.TestCreate;
begin
  Nop;

end;

procedure TJwWTSSessionListTests.TestGetItem;
begin
  Nop;

end;

procedure TJwWTSSessionListTests.TestGetOwner;
begin
  Nop;

end;

initialization

  TestFramework.RegisterTest('JwsclTerminalServerTests Suite',
    TJwTerminalServerTests.Suite);
  TestFramework.RegisterTest('JwsclTerminalServerTests Suite',
    TJwWTSSessionTests.Suite);
  TestFramework.RegisterTest('JwsclTerminalServerTests Suite',
    TJwWTSSessionListTests.Suite);

end.
