unit TestsJwsclCoGenericList;

interface

uses
  JWSCLCom_TLB,
  JwsclCoGenericList,
  TestFrameWork;

type
  TJwGenericListTests = class(TTestCase)
  private
    fGenericList : IJwGenericList;
  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestGetData;
    procedure TestAddData;
    procedure TestGet_ReadOnly;
    procedure TestDeleteData;
    procedure TestSet_ReadOnly;
    procedure TestGet_OwnData;
    procedure TestSet_OwnData;
    procedure TestAddAndDuplicate;
    procedure TestInitialize;
    procedure TestDestroy;

  end;



implementation

{ TJwGenericListTests }

procedure TJwGenericListTests.TestAddAndDuplicate;
begin

end;

procedure TJwGenericListTests.TestAddData;
begin
  fGenericList.Add(123);
  CheckEquals(1, fGenericList.Count);
  CheckEquals(123, fGenericList.Item[0]);
end;

procedure TJwGenericListTests.TestDeleteData;
begin
  fGenericList.Add(123);
  fGenericList.Delete(0);
  CheckEquals(0, fGenericList.Count);
end;

procedure TJwGenericListTests.TestDestroy;
begin

end;

procedure TJwGenericListTests.TestGet_OwnData;
begin

end;

procedure TJwGenericListTests.TestGet_ReadOnly;
begin

end;

procedure TJwGenericListTests.TestGetData;
begin

end;

procedure TJwGenericListTests.TestInitialize;
begin

end;

procedure TJwGenericListTests.TestSet_OwnData;
begin

end;

procedure TJwGenericListTests.TestSet_ReadOnly;
begin
  fGenericList.ReadOnly := true;
  CheckEquals(true,fGenericList.ReadOnly);

  try
    fGenericList.ReadOnly := false;
    CheckFalse(true);
  except
  end;
  CheckEquals(true,fGenericList.ReadOnly);
end;

procedure TJwGenericListTests.SetUp;
begin
  inherited;
  fGenericList := CoJwGenericList.Create;
end;

procedure TJwGenericListTests.TearDown;
begin
  inherited;

end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TJwGenericListTests.Suite);


end.
