unit JwsclCoEnumSetTests;

interface

uses
  JWSCLCom_TLB,
  TestFrameWork;

type
  TJwEnumSetTests = class(TTestCase)
  private
    fSet : IJwEnumSet;
  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestGet_BitMask;
    procedure TestGet_EnumType;
    procedure TestExclude;
    procedure TestInclude;
    procedure TestSet_BitMask;
    procedure TestSet_EnumType;
    procedure TestState;
    procedure TestIsBitAvailable;

  end;

implementation

{ TJwEnumSetTests }



procedure TJwEnumSetTests.SetUp;
begin
  inherited;
  fSet := CoJwEnumSet.Create();
end;

procedure TJwEnumSetTests.TearDown;
begin
  inherited;

  fSet := nil;
end;

procedure TJwEnumSetTests.TestExclude;
begin

end;

procedure TJwEnumSetTests.TestGet_BitMask;
begin

end;

procedure TJwEnumSetTests.TestGet_EnumType;
begin

end;

procedure TJwEnumSetTests.TestInclude;
begin

end;


procedure TJwEnumSetTests.TestIsBitAvailable;
begin

end;

procedure TJwEnumSetTests.TestSet_BitMask;
begin
  fSet.EnumType := JwEnumSetSIDAttributes;
  fSet.BitMask := Integer(sidaPad5);
end;

procedure TJwEnumSetTests.TestSet_EnumType;
begin

end;

procedure TJwEnumSetTests.TestState;
begin

end;

initialization

  TestFramework.RegisterTest('JwsclCoEnumSetTests Suite',
    TJwEnumSetTests.Suite);

end.
