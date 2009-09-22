unit JwsclSimpleDescriptorTests;

interface

uses
  JwsclSimpleDescriptor,
  SysUtils,
  JwaWindows,
  JwsclDescriptor,
  JwsclSid,
  JwsclTypes,
  JwsclAcl,
  JwsclStrings,
  TestFrameWork;

type
  TJwSimpleDescriptorTests = class(TTestCase)
  private

  protected
    SD : TJwSimpleDescriptor;
    Sid : TJwSecurityId;

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestAllow; overload;
    procedure TestDeny; overload;
    procedure TestCreate;
    procedure TestDestroy;
    procedure TestOwner;
    procedure TestSecurityDescriptor;
    procedure TestSecurityAttributes;

  end;

implementation

{ TJwSimpleDescriptorTests }

procedure TJwSimpleDescriptorTests.SetUp;
begin
  inherited;

  SD := TJwSimpleDescriptor.Create;
  Sid := TJwSecurityId.Create('S-1-0-0');
end;

procedure TJwSimpleDescriptorTests.TearDown;
begin
  inherited;
  FreeAndNil(Sid);
  FreeAndNil(SD);
end;

procedure TJwSimpleDescriptorTests.TestAllow;
begin   
  try
    SD.Allow('Jeder');
    CheckEquals(1, SD.SecurityDescriptor.DACL.Count);
  except
    try
      SD.Allow('Everybody');
      CheckEquals(1, SD.SecurityDescriptor.DACL.Count);
    except
    end;
  end;

  SD.Allow(Sid);
  CheckEquals(2, SD.SecurityDescriptor.DACL.Count);

  SD.Allow(Sid.Sid);
  CheckEquals(3, SD.SecurityDescriptor.DACL.Count);

  FreeAndNil(SD);
  CheckNotEquals(0, Integer(Sid.Sid), 'Sid was freed by SimpleSD but it shouldnt.');
end;

procedure TJwSimpleDescriptorTests.TestCreate;
begin
  CheckNull(SD.Owner);
  CheckNotNull(SD.SecurityDescriptor);
  CheckNotNull(SD.SecurityDescriptor.DACL);
  CheckEquals(0, SD.SecurityDescriptor.DACL.Count);
end;

procedure TJwSimpleDescriptorTests.TestDeny;
begin
  try
    SD.Deny('Jeder');
    CheckEquals(1, SD.SecurityDescriptor.DACL.Count);
  except
    try
      SD.Allow('Everybody');
      CheckEquals(1, SD.SecurityDescriptor.DACL.Count);
    except
    end;
  end;

  SD.Deny(Sid);
  CheckEquals(2, SD.SecurityDescriptor.DACL.Count);

  SD.Deny(Sid.Sid);
  CheckEquals(3, SD.SecurityDescriptor.DACL.Count);

  FreeAndNil(SD);
  CheckNotEquals(0, Integer(Sid.Sid), 'Sid was freed by SimpleSD but it shouldnt.');
end;


procedure TJwSimpleDescriptorTests.TestDestroy;
begin
  FreeAndNil(SD);
  if Assigned(Sid) then
    CheckNotEquals(0, Integer(Sid.Sid), 'Sid was freed by SimpleSD but it shouldnt.');
end;

procedure TJwSimpleDescriptorTests.TestOwner;
begin
  try
    SD.SetOwner('Jeder');
  except
    try
      SD.SetOwner('Everybody');
    except
    end;
  end;

  CheckNotNull(SD.Owner);

  SD.SetOwner(PSid(nil));
  CheckNull(SD.Owner);

  SD.SetOwner(TJwSecurityId(nil));
  CheckNull(SD.Owner);

  SD.SetOwner(Sid);
  CheckNotNull(SD.Owner);

  SD.SetOwner(TJwSecurityId(nil));
  CheckNull(SD.Owner);
  CheckNotEquals(0, Integer(Sid.Sid), 'Sid was freed by SimpleSD but it shouldnt.');

end;


procedure TJwSimpleDescriptorTests.TestSecurityAttributes;
begin
  SD.SetOwner(Sid);
  SD.Allow(Sid);
  SD.Deny(Sid);

  CheckNotNull(SD.SecurityDescriptor);
  Check(Sid.EqualSid(SD.SecurityDescriptor.Owner));
  CheckNotNull(SD.SecurityDescriptor.DACL);
  CheckEquals(2, SD.SecurityDescriptor.DACL.Count);
end;

procedure TJwSimpleDescriptorTests.TestSecurityDescriptor;
var
  temp : PSECURITY_ATTRIBUTES;
  SD2 : TJwSecurityDescriptor;
begin
  SD.SetOwner(Sid);
  SD.Allow(Sid);
  SD.Deny(Sid);

  temp := SD.SecurityAttributes;
  CheckNotEquals(0, Integer(temp));

  SD2 := TJwSecurityDescriptor.Create(temp.lpSecurityDescriptor);
  try
    Check(Sid.EqualSid(SD2.Owner));
    CheckNotNull(SD2.DACL);
    CheckEquals(2, SD2.DACL.Count);
  finally
    SD2.Free; 
  end;


end;

initialization

  TestFramework.RegisterTest('JwsclSimpleDescriptorTests Suite',
    TJwSimpleDescriptorTests.Suite);

end.
