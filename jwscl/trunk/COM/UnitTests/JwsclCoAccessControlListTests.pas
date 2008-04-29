unit JwsclCoAccessControlListTests;

interface

uses
  JwsclCoAccessControlList,
  TestFrameWork;

type
  TJwAccessControlListTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods
    procedure TestAddAllowAce;
    procedure TestAddDenyAce;
    procedure TestItem;
    procedure TestGet__NewEnum;
    procedure TestGetInternalObject;
    procedure TestDelete;
    procedure TestRemove;
    procedure TestToString;
    procedure TestFind;
    procedure TestGet_Revision;
    procedure TestSet_Revision;
    procedure TestInitByAccessList;
    procedure TestClear;

  end;

implementation

{ TJwAccessControlListTests }

procedure TJwAccessControlListTests.TestAddAllowAce;
begin

end;

procedure TJwAccessControlListTests.TestAddDenyAce;
begin

end;

procedure TJwAccessControlListTests.TestClear;
begin

end;

procedure TJwAccessControlListTests.TestDelete;
begin

end;

procedure TJwAccessControlListTests.TestFind;
begin

end;

procedure TJwAccessControlListTests.TestGet__NewEnum;
begin

end;

procedure TJwAccessControlListTests.TestGet_Revision;
begin

end;

procedure TJwAccessControlListTests.TestGetInternalObject;
begin

end;

procedure TJwAccessControlListTests.TestInitByAccessList;
begin

end;

procedure TJwAccessControlListTests.TestItem;
begin

end;

procedure TJwAccessControlListTests.TestRemove;
begin

end;

procedure TJwAccessControlListTests.TestSet_Revision;
begin

end;

procedure TJwAccessControlListTests.TestToString;
begin

end;

initialization

  TestFramework.RegisterTest('JwsclCoAccessControlListTests Suite',
    TJwAccessControlListTests.Suite);

end.
