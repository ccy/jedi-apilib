unit JwsclAclTests;

interface

uses
  Dialogs,
  UMessageForm,
  SysUtils,

  jwaWindows,
  jwaVista,

  JwsclMapping,
  JwsclTypes,
  JwsclAcl,
  JwsclSid,
  JwsclToken,
  JwsclDescriptor,
  JwsclExceptions,
  TestFrameWork;

type
  TJwSecurityAccessControlListTests = class(TTestCase)
  private

    fList : Array[1..10] of TJwSecurityAccessControlList;
  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure Test_Create_DAccessControlList;
    procedure Test_Create_SAccessControlList;

    procedure Test_CreateFree_PACL;
    procedure Test_Assign;
    procedure Test_Add_Insert_Remove;
    procedure Test_FirstLast;
    procedure Test_IndexOf;

    procedure Test_GetText;
  end;

type
  TJwSecurityAccessControlEntryTests = class(TTestCase)
  private
    fEntry : array[1..10] of TJwSecurityAccessControlEntry;
  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    procedure Test_ACEFlagsToCardinal;
    procedure Test_CardinalFlagsToACEFlags;

    procedure Test_Create;
    procedure Test_Create_aListOwner;
    procedure Test_Create_aACE;
    procedure Test_Create_accessACE_Allowed;
    procedure Test_Create_accessACE_Denied;

    procedure Test_Create_Allow_Denied;
    procedure Test_class_CreateACE_Allow;
    procedure Test_class_CreateACE_Deny;
    procedure Test_class_CreateACE_Audit;
    procedure Test_class_CreateACE_anAceType;

    procedure TestGetTextMap;

    procedure Test_Audit;

    procedure Test_Assign;

    procedure Test_ChangeProperties;
    procedure Test_MandatoryLabel;
  end;

implementation

uses TypInfo, JwsclEnumerations;

{ TJwSecurityAccessControlEntryTests }

procedure TJwSecurityAccessControlEntryTests.SetUp;
var i : Integer;
begin
  inherited;

  for i := low(fEntry) to high(fEntry) do
    fEntry[i] := nil;
end;


procedure TJwSecurityAccessControlEntryTests.TearDown;
var i : Integer;
begin
  inherited;

  for i := low(fEntry) to high(fEntry) do
    if Assigned(fEntry[i]) then
      fEntry[i].Free;

end;

procedure TJwSecurityAccessControlEntryTests.Test_Create;
begin
  fEntry[1] := TJwSecurityAccessControlEntry.Create;

  with fEntry[1] do
  begin
    CheckTrue(nil = ListOwner);
    CheckTRUE(Flags = []);
    CheckEquals(0,AccessMask);
    Check(nil = SID);
    CheckEquals(false,ownSID);
  end;
 // TJwAuditAccessControlEntry(fEntry[1]).
end;

procedure TJwSecurityAccessControlEntryTests.Test_Create_aListOwner;

  procedure CheckCreate(const aListOwner : TJwSecurityAccessControlList;
                          aFlags : TJwAceFlags;
                          anAccessMask : TJwAccessMask;
                          aSID : TJwSecurityId;
                         anownSID : Boolean);
  var E : TJwSecurityAccessControlEntry;
  begin
    E := TJwAuditAccessControlEntry.Create(aListOwner,aFlags,anAccessMask,aSID,anownSID);

    try
      with E do
      begin
        CheckTrue(aListOwner = ListOwner,'ListOWner');
        CheckTRUE(Flags = aFlags,'Flags');

        //Accessmask of an audit element depends on property AuditSuccess and AuditFailure
        //CheckEquals(anAccessMask,AccessMask,'AccessMask');
        Check(aSID = SID,'SID');
        CheckEquals(anownSID,ownSID,'ownSID');
      end;
    finally
      E.Free;
    end;
  end;

var list : TJwSecurityAccessControlList;
    aSID : TJwSecurityId;
begin
  list := TJwSAccessControlList.Create(false);

  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);
  try
    CheckCreate(nil, [], GENERIC_ALL, nil, false);
    CheckCreate(List, [afObjectInheritAce,
                   afContainerInheritAce,
                   afNoPropagateInheritAce,
                   afInheritOnlyAce,
                   afInheritedAce,
                   afValidInheritFlags,
                   afSuccessfulAccessAceFlag,
                   afFailedAccessAceFlag],GENERIC_ALL,aSID,false);
    CheckCreate(nil,[],GENERIC_ALL,nil,true);

    fEntry[1] := TJwAuditAccessControlEntry.Create(List,[],0,aSID,false);
    try
      fEntry[1].ListOwner := List;
      CheckIs(nil,EJwsclReadOnlyPropertyException);
    except
      on E : Exception do
        CheckIs(E,EJwsclReadOnlyPropertyException);
    end;

    fEntry[2] := TJwAuditAccessControlEntry.Create(nil,[],0,aSID,false);
    try
      fEntry[2].ListOwner := List;
    except
      Check(false,'Listowner');
    end;
  finally
    aSID.Free;
    List.Free;
  end;
end;

procedure TJwSecurityAccessControlEntryTests.Test_Create_aACE;
  procedure CheckCreate(const aListOwner : TJwSecurityAccessControlList;
                          aFlags : TJwAceFlags;
                          anAccessMask : TJwAccessMask;
                          aSID : TJwSecurityId;
                         ownSID : Boolean);
  var E1,E2 : TJwSecurityAccessControlEntry;
  begin

    E1 := TJwDiscretionaryAccessControlEntryAllow.Create(aListOwner,aFlags,anAccessMask,aSID,ownSID);
    E2 := TJwDiscretionaryAccessControlEntryAllow.Create(TJwDiscretionaryAccessControlEntryAllow(E1));
    try
      begin
        CheckTrue(nil = E2.ListOwner,'ListOwner of new TJwSecurityAccessControlEntry must not be added automatically to the list!!');

        CheckTRUE(E1.Flags = E2.Flags,'Flags');
        CheckEquals(E1.AccessMask,E2.AccessMask,'AccessMask');

        if (E1.SID <> nil) and (E2.SID <> nil) then
          CheckNotEqualsMem(@E1.SID,@E2.SID,sizeof(Pointer),'SID instances are equal');
        CheckEquals(true,E2.OwnSID,'ownSID');
      end;
    finally
      E1.Free;
      E2.Free;
    end;


  end;

var list : TJwDAccessControlList;
    aSID : TJwSecurityId;
begin
   list := TJwDAccessControlList.Create(true);
   aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);
  try
    try
      TJwDiscretionaryAccessControlEntryAllow.Create(PAccessAllowedACE(nil));
      CheckIs(nil,EJwsclNILParameterException);
    except
      on E : Exception do
      begin
        CheckIs(E,EJwsclNILParameterException);
      end;
    end;

    try
      TJwDiscretionaryAccessControlEntryDeny.Create(PAccessDeniedAce(nil));
      CheckIs(nil,EJwsclNILParameterException);
    except
      on E : Exception do
      begin
        CheckIs(E,EJwsclNILParameterException);
      end;
    end;




    CheckCreate(nil, [], GENERIC_ALL, nil, false);
    CheckCreate(List, [afObjectInheritAce,
                   afContainerInheritAce,
                   afNoPropagateInheritAce,
                   afInheritOnlyAce,
                   afInheritedAce,
                   afValidInheritFlags,
                   afSuccessfulAccessAceFlag,
                   afFailedAccessAceFlag],GENERIC_ALL,aSID,false);
   CheckCreate(nil,[],GENERIC_ALL,nil,true);
  finally
    aSID.Free;
    List.Free;
  end;
end;

procedure TJwSecurityAccessControlEntryTests.Test_Create_accessACE_Allowed;
var accessACE : PAccessAllowedAce;
    aSID : TJwSecurityId;
begin
  try
    TJwDiscretionaryAccessControlEntryAllow.Create(TJwDiscretionaryAccessControlEntryAllow(nil));
    CheckIs(nil,EJwsclNILParameterException);
  except
    on E : Exception do
    begin
      CheckIs(E,EJwsclNILParameterException);
    end;
  end;

  try
    TJwDiscretionaryAccessControlEntryDeny.Create(TJwDiscretionaryAccessControlEntryDeny(nil));
    CheckIs(nil,EJwsclNILParameterException);
  except
    on E : Exception do
    begin
      CheckIs(E,EJwsclNILParameterException);
    end;
  end;


  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);

  try
    fEntry[1] := TJwDiscretionaryAccessControlEntryAllow.Create(nil,[afObjectInheritAce
                   ,afContainerInheritAce,
                   afNoPropagateInheritAce,
                   afInheritOnlyAce,
                   afInheritedAce,
                   afValidInheritFlags,
                   afSuccessfulAccessAceFlag,
                   afFailedAccessAceFlag],GENERIC_ALL,aSID,false);

    accessACE := TJwDiscretionaryAccessControlEntryAllow(fEntry[1]).Create_AllowACE;
    fEntry[2] := TJwDiscretionaryAccessControlEntryAllow.Create(PAccessAllowedAce(accessACE));
    TJwDiscretionaryAccessControlEntryAllow(fEntry[1]).Free_PACE(accessACE);

    CheckTrue(fEntry[1].ListOwner = fEntry[2].ListOwner,'ListOWner');
    CheckTRUE(fEntry[1].Flags = fEntry[2].Flags,'Flags');
    CheckEquals(fEntry[1].AccessMask,fEntry[2].AccessMask,'AccessMask');
    Check(fEntry[1].SID.EqualSid(fEntry[2].SID),'SID');
    CheckEquals(true,fEntry[2].ownSID,'ownSID');
  finally
    aSID.Free;
  end;
end;

procedure TJwSecurityAccessControlEntryTests.Test_Create_accessACE_Denied;
var accessACE : PAccessDeniedAce;
    aSID : TJwSecurityId;
begin
  try
    TJwDiscretionaryAccessControlEntryDeny.Create(PAccessDeniedAce(nil));
    CheckIs(nil,EJwsclNILParameterException);
  except
    on E : Exception do
    begin
      CheckIs(E,EJwsclNILParameterException);
    end;
  end;


  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);

  try
    fEntry[1] := TJwDiscretionaryAccessControlEntryDeny.Create(nil,[afObjectInheritAce
                   ,afContainerInheritAce,
                   afNoPropagateInheritAce,
                   afInheritOnlyAce,
                   afInheritedAce,
                   afValidInheritFlags,
                   afSuccessfulAccessAceFlag,
                   afFailedAccessAceFlag],GENERIC_ALL,aSID,false);

    accessACE := TJwDiscretionaryAccessControlEntryDeny(fEntry[1]).Create_DenyACE;
    fEntry[2] := TJwDiscretionaryAccessControlEntryDeny.Create(PAccessDeniedAce(accessACE));
    TJwDiscretionaryAccessControlEntryDeny(fEntry[1]).Free_PACE(accessACE);

    CheckTrue(fEntry[1].ListOwner = fEntry[2].ListOwner,'ListOWner');
    CheckTRUE(fEntry[1].Flags = fEntry[2].Flags,'Flags');
    CheckEquals(fEntry[1].AccessMask,fEntry[2].AccessMask,'AccessMask');
    Check(fEntry[1].SID.EqualSid(fEntry[2].SID),'SID');
    CheckEquals(true,fEntry[2].ownSID,'ownSID');
  finally
    aSID.Free;
  end;
end;

procedure TJwSecurityAccessControlEntryTests.Test_Create_Allow_Denied;
var accessAllowACE : PAccessAllowedAce;
    accessDenyACE : PAccessDeniedAce;
    aSID : TJwSecurityId;
begin
  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);
  try
    fEntry[1] := TJwDiscretionaryAccessControlEntryDeny.Create(nil,[afObjectInheritAce
                   ,afContainerInheritAce,
                   afNoPropagateInheritAce,
                   afInheritOnlyAce,
                   afInheritedAce,
                   afValidInheritFlags,
                   afSuccessfulAccessAceFlag,
                   afFailedAccessAceFlag],GENERIC_ALL,aSID,false);

    accessDenyACE := TJwDiscretionaryAccessControlEntryDeny(fEntry[1]).Create_DenyACE;
    CheckEquals(ACCESS_DENIED_ACE_TYPE,accessDenyACE.Header.AceType);
    Checktrue(actDeny = fEntry[1].AceType,'Unequal ace type');


    accessDenyACE.Header.AceType := ACCESS_ALLOWED_ACE_TYPE;
    fEntry[2] := TJwDiscretionaryAccessControlEntryAllow.Create(PAccessAllowedAce(accessDenyACE));
    accessAllowACE := TJwDiscretionaryAccessControlEntryAllow(fEntry[2]).Create_AllowACE;
    CheckEquals(ACCESS_ALLOWED_ACE_TYPE,accessAllowACE.Header.AceType);
    Checktrue(actAllow = fEntry[2].AceType,'Unequal ace type');


    TJwDiscretionaryAccessControlEntryDeny(fEntry[1]).Free_PACE(accessDenyACE);
    TJwDiscretionaryAccessControlEntryAllow(fEntry[1]).Free_PACE(accessAllowACE);

    CheckTrue(fEntry[1].ListOwner = fEntry[2].ListOwner,'ListOWner');
    CheckTRUE(fEntry[1].Flags = fEntry[2].Flags,'Flags');
    CheckEquals(fEntry[1].AccessMask,fEntry[2].AccessMask,'AccessMask');
    Check(fEntry[1].SID.EqualSid(fEntry[2].SID),'SID');
    CheckEquals(true,fEntry[2].ownSID,'ownSID');
  finally
    aSID.Free;
  end;
end;

procedure TJwSecurityAccessControlEntryTests.TestGetTextMap;
var f : TJwSecurityAccessControlEntry;
begin
  f := TJwSecurityAccessControlEntry.CreateACE(actAllow);
  ShowMessage(f.GetTextMap(TJwSecurityGenericMapping));
end;

procedure TJwSecurityAccessControlEntryTests.Test_ACEFlagsToCardinal;
begin
  CheckEquals(OBJECT_INHERIT_ACE         or
              CONTAINER_INHERIT_ACE     or
              NO_PROPAGATE_INHERIT_ACE or
              INHERIT_ONLY_ACE  or
              INHERITED_ACE or
              VALID_INHERIT_FLAGS or
              SUCCESSFUL_ACCESS_ACE_FLAG or
              FAILED_ACCESS_ACE_FLAG,
  TJwEnumMap.ConvertAceFlags(
                [afObjectInheritAce,
                 afContainerInheritAce,
                 afNoPropagateInheritAce,
                 afInheritOnlyAce,
                 afInheritedAce,
                 afValidInheritFlags,
                 afSuccessfulAccessAceFlag,
                 afFailedAccessAceFlag]),'Flags conversion from TJwAceFlags to Cardinal is incorrect');

  CheckEquals(0,TJwEnumMap.ConvertAceFlags([]),'Flags conversion from TJwAceFlags to Cardinal is incorrect');
end;

procedure TJwSecurityAccessControlEntryTests.Test_CardinalFlagsToACEFlags;
var p,p1 : TJwAceFlags;
    d1,d2 : Cardinal;

begin
   d1 := OBJECT_INHERIT_ACE         or
              CONTAINER_INHERIT_ACE     or
              NO_PROPAGATE_INHERIT_ACE or
              INHERIT_ONLY_ACE  or
              INHERITED_ACE or
              VALID_INHERIT_FLAGS or
              SUCCESSFUL_ACCESS_ACE_FLAG or
              FAILED_ACCESS_ACE_FLAG;
  p := TJwEnumMap.ConvertAceFlags(d1);

  p1 := [afObjectInheritAce,
                 afContainerInheritAce,
                 afNoPropagateInheritAce,
                 afInheritOnlyAce,
                 afInheritedAce,
                 afValidInheritFlags,
                 afSuccessfulAccessAceFlag,
                 afFailedAccessAceFlag];

  CheckEqualsMem(@p1,@p,sizeof(p),'Flags conversion from Cardinal to TJwAceFlags is incorrect');

  p1 := [];
  p  := TJwEnumMap.ConvertAceFlags(0);
  CheckEqualsMem(@p1,@p,sizeof(p),'Flags conversion from Cardinal to TJwAceFlags is incorrect');
end;



procedure TJwSecurityAccessControlEntryTests.Test_class_CreateACE_Allow;
var accessACE : PAccessAllowedAce;
    aSID : TJwSecurityId;
begin
  try
    TJwDiscretionaryAccessControlEntryAllow.Create(PAccessAllowedAce(nil));
    CheckIs(nil,EJwsclNILParameterException);
  except
    on E : Exception do
    begin
      CheckIs(E,EJwsclNILParameterException);
    end;
  end;


  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);

  try
    //create a positive/allow ACE implicitly
    fEntry[1] := TJwDiscretionaryAccessControlEntryAllow.Create(nil,[afObjectInheritAce
                   ,afContainerInheritAce,
                   afNoPropagateInheritAce,
                   afInheritOnlyAce,
                   afInheritedAce,
                   afValidInheritFlags,
                   afSuccessfulAccessAceFlag,
                   afFailedAccessAceFlag],GENERIC_ALL,aSID,false);

    accessACE := TJwDiscretionaryAccessControlEntryAllow(fEntry[1]).Create_AllowACE;

    //create a positive/allow ACE explicitly
    fEntry[2] := TJwDiscretionaryAccessControlEntryAllow.Create(PAccessAllowedAce(accessACE));
    TJwDiscretionaryAccessControlEntryAllow(fEntry[1]).Free_PACE(accessACE);


    CheckIs(fEntry[2], TJwDiscretionaryAccessControlEntryAllow, 'CreateACE did not create TJwDiscretionaryAccessControlEntryAllow class type.');


    CheckTrue(fEntry[1].ListOwner = fEntry[2].ListOwner,'ListOWner');
    CheckTRUE(fEntry[1].Flags = fEntry[2].Flags,'Flags');
    CheckEquals(fEntry[1].AccessMask,fEntry[2].AccessMask,'AccessMask');
    Check(fEntry[1].SID.EqualSid(fEntry[2].SID),'SID');
    CheckEquals(true,fEntry[2].ownSID,'ownSID');

  finally
    aSID.Free;
  end;
end;


procedure TJwSecurityAccessControlEntryTests.Test_class_CreateACE_Deny;
var accessACE : PAccessDeniedAce;
    aSID : TJwSecurityId;
begin
  try
    TJwDiscretionaryAccessControlEntryDeny.Create(PAccessDeniedAce(nil));
    CheckIs(nil,EJwsclNILParameterException);
  except
    on E : Exception do
    begin
      CheckIs(E,EJwsclNILParameterException);
    end;
  end;

  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);
  try
    //create a negative/deny ACE explicitly
    fEntry[1] := TJwDiscretionaryAccessControlEntryDeny.Create(nil,[afObjectInheritAce
                   ,afContainerInheritAce,
                   afNoPropagateInheritAce,
                   afInheritOnlyAce,
                   afInheritedAce,
                   afValidInheritFlags,
                   afSuccessfulAccessAceFlag,
                   afFailedAccessAceFlag],GENERIC_ALL,aSID,false);

    accessACE := TJwDiscretionaryAccessControlEntryDeny(fEntry[1]).Create_DenyACE;

    //create a negative/deny ACE explicitly
    fEntry[2] := TJwDiscretionaryAccessControlEntryDeny.Create(PAccessDeniedAce(accessACE));
    TJwDiscretionaryAccessControlEntryDeny(fEntry[1]).Free_PACE(accessACE);

    CheckIs(fEntry[2], TJwDiscretionaryAccessControlEntryDeny, 'CreateACE did not create TJwDiscretionaryAccessControlEntryDeny class type.');


    CheckTrue(fEntry[1].ListOwner = fEntry[2].ListOwner,'ListOWner');
    CheckTRUE(fEntry[1].Flags = fEntry[2].Flags,'Flags');
    CheckEquals(fEntry[1].AccessMask,fEntry[2].AccessMask,'AccessMask');
    Check(fEntry[1].SID.EqualSid(fEntry[2].SID),'SID');
    CheckEquals(true,fEntry[2].ownSID,'ownSID');
  finally
    aSID.Free;
  end;
end;

procedure TJwSecurityAccessControlEntryTests.Test_class_CreateACE_Audit;
var accessACE : PSystemAuditAce;
    aSID : TJwSecurityId;
    audit1, audit2 : TJwAuditAccessControlEntry;
begin
  try
    TJwAuditAccessControlEntry.Create(PSystemAuditAce(nil));
    CheckIs(nil,EJwsclNILParameterException);
  except

    on E : Exception do
    begin
      CheckIs(E,EJwsclNILParameterException);
    end;
  end;

  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);
  try
    //create a negative/deny ACE explicitly
    audit1 := TJwAuditAccessControlEntry.Create(nil,[
                   afObjectInheritAce,
                   afContainerInheritAce,
                   afNoPropagateInheritAce,
                   afInheritOnlyAce,
                   afInheritedAce,
                   afValidInheritFlags,
                   afSuccessfulAccessAceFlag,
                   afFailedAccessAceFlag],GENERIC_ALL,aSID,false);
    fEntry[1]:= audit1;
    accessACE := audit1.Create_AuditACE;

    //create a negative/deny ACE explicitly
    audit2 := TJwAuditAccessControlEntry.Create(accessACE);
    fEntry[2]:= audit2;

    audit1.Free_PACE(accessACE);

    CheckIs(audit2, TJwAuditAccessControlEntry, 'CreateACE did not create TJwDiscretionaryAccessControlEntryDeny class type.');


    CheckTrue(audit1.ListOwner = audit2.ListOwner,'ListOWner');
    CheckTRUE(audit1.Flags = audit2.Flags,'Flags');
    CheckEquals(audit1.AccessMask,audit2.AccessMask,'AccessMask');
    Check(audit1.SID.EqualSid(audit2.SID),'SID');
    CheckEquals(true,audit2.ownSID,'ownSID');

    CheckEquals(audit1.AuditSuccess,audit2.AuditSuccess);
    CheckEquals(audit1.AuditFailure,audit2.AuditFailure);

  finally
    aSID.Free;
  end;
end;

procedure TJwSecurityAccessControlEntryTests.Test_class_CreateACE_anAceType;
begin
  fEntry[1] := TJwSecurityAccessControlEntry.CreateACE(actAllow);
  CheckIs(fEntry[1],TJwDiscretionaryAccessControlEntryAllow);
  CheckTrue(TJwSecurityAccessControlEntry(fEntry[1]).AceType = actAllow,'');

  fEntry[2] := TJwSecurityAccessControlEntry.CreateACE(actDeny);
  CheckIs(fEntry[2],TJwDiscretionaryAccessControlEntryDeny);
  CheckTrue(TJwSecurityAccessControlEntry(fEntry[2]).AceType = actDeny,'');

  fEntry[3] := TJwSecurityAccessControlEntry.CreateACE(actAudit);
  CheckIs(fEntry[3],TJwAuditAccessControlEntry);
  CheckTrue(TJwSecurityAccessControlEntry(fEntry[3]).AceType = actAudit,'');
end;



procedure TJwSecurityAccessControlEntryTests.Test_Audit;
var accessACE : PSystemAuditAce;
    aSID : TJwSecurityId;

    audit1, audit2 : TJwAuditAccessControlEntry;
begin
  try
    TJwAuditAccessControlEntry.Create(PSystemAuditAce(nil));
    CheckIs(nil,EJwsclNILParameterException);
  except
    on E : Exception do
    begin
      CheckIs(E,EJwsclNILParameterException);
    end;
  end;

  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);

  try
    fEntry[1] := TJwAuditAccessControlEntry.Create(nil,[],GENERIC_ALL,aSID,false);
    TJwAuditAccessControlEntry(fEntry[1]).AuditSuccess := true;
    TJwAuditAccessControlEntry(fEntry[1]).AuditFailure := true;
    audit1 := TJwAuditAccessControlEntry(fEntry[1]);

    with TJwAuditAccessControlEntry(fEntry[1]) do
    begin
      CheckTRUE(Flags = [afSuccessfulAccessAceFlag, afFailedAccessAceFlag],'Flags');
      CheckEquals(AuditSuccess,true);
      CheckEquals(AuditFailure,true);

      AuditSuccess := true;
      AuditFailure := false;
       CheckEquals(AuditSuccess,true);
       CheckEquals(AuditFailure,false);
      AuditSuccess := false;
      AuditFailure := true;
       CheckEquals(AuditSuccess,false);
       CheckEquals(AuditFailure,true);
    end;

    audit2 := TJwAuditAccessControlEntry.Create(TJwAuditAccessControlEntry(fEntry[1]));
    fEntry[2] := audit2;

    CheckTrue(fEntry[1].ListOwner = audit2.ListOwner,'ListOWner');
    CheckTRUE(fEntry[1].Flags = audit2.Flags,'Flags');
    CheckEquals(fEntry[1].AccessMask,audit2.AccessMask,'AccessMask');
    Check(fEntry[1].SID.EqualSid(audit2.SID),'SID');
    CheckEquals(true,audit2.ownSID,'ownSID');

    CheckEquals(audit1.AuditSuccess,audit2.AuditSuccess);
    CheckEquals(audit1.AuditFailure,audit2.AuditFailure);


    begin
      audit1.AuditSuccess := true;
      audit1.AuditFailure := false;

      audit2 := TJwAuditAccessControlEntry.Create(audit1);
      fEntry[3] := audit2;
      CheckEquals(audit1.AuditSuccess,audit2.AuditSuccess);
      CheckEquals(audit1.AuditFailure,audit2.AuditFailure);
    end;

    begin
      audit1.AuditSuccess := false;
      audit1.AuditFailure := true;

      audit2 := TJwAuditAccessControlEntry.Create(audit1);
      fEntry[4] := audit2;
      CheckEquals(audit1.AuditSuccess,audit2.AuditSuccess);
      CheckEquals(audit1.AuditFailure,audit2.AuditFailure);
    end;

    begin
      audit1.AuditSuccess := false;
      audit1.AuditFailure := false;

      audit2 := TJwAuditAccessControlEntry.Create(audit1);
      fEntry[5] := audit2;
      CheckEquals(audit1.AuditSuccess,audit2.AuditSuccess);
      CheckEquals(audit1.AuditFailure,audit2.AuditFailure);
    end;
  finally
    aSID.Free;
  end;
end;


procedure TJwSecurityAccessControlEntryTests.Test_Assign;
var aSID : TJwSecurityId;
    audit1, audit2 : TJwAuditAccessControlEntry;
begin
  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);

  try
    fEntry[1] := TJwDiscretionaryAccessControlEntryAllow.Create(nil,[afObjectInheritAce],GENERIC_READ,aSID,false);

    fEntry[2] := TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,nil,true);
    fEntry[2].Assign(fEntry[1]);


    CheckNull(fEntry[2].ListOwner,'ListOWner');
    CheckTRUE(fEntry[1].Flags = fEntry[2].Flags,'Flags');
    CheckEquals(fEntry[1].AccessMask,fEntry[2].AccessMask,'AccessMask');
    Check(fEntry[1].SID.EqualSid(fEntry[2].SID),'SID');

    //the new sid is always automatically freed if not set to false manuall
    CheckEquals(true,fEntry[2].ownSID,'ownSID');



    //audit class has an adapted assign method
    fEntry[3] := TJwAuditAccessControlEntry.Create(nil,[afObjectInheritAce],GENERIC_READ,aSID,false);
    audit1 := TJwAuditAccessControlEntry(fEntry[3]);

    fEntry[4] := TJwAuditAccessControlEntry.Create(nil,[],GENERIC_ALL,nil,true);
    fEntry[4].Assign(fEntry[3]);
    audit2 := TJwAuditAccessControlEntry(fEntry[4]); // audit2 = fEntry[3]

    CheckNull(audit2.ListOwner,'ListOWner');
    CheckTRUE([afObjectInheritAce] = audit2.Flags,'Flags');
    CheckEquals(audit1.AccessMask,audit2.AccessMask,'AccessMask');
    Check(audit1.SID.EqualSid(audit2.SID),'SID');

    //the new sid is always automatically freed if not set to false manuall
    CheckEquals(true,audit2.ownSID,'ownSID');

    CheckTrue(audit1.AuditSuccess = audit2.AuditSuccess);
    CheckTrue(audit1.AuditFailure = audit2.AuditFailure);

  finally
    aSID.Free;
  end;
end;

procedure TJwSecurityAccessControlEntryTests.Test_ChangeProperties;
var aSID : TJwSecurityId;
    audit1, audit2 : TJwAuditAccessControlEntry;
    List : TJwSecurityAccessControlList;
begin
  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);
  List := TJwDAccessControlList.Create(false);

  try
    fEntry[1] := TJwDiscretionaryAccessControlEntryAllow.Create(nil,[afObjectInheritAce],GENERIC_READ,aSID,false);

    //Entry is not added to list automatically!
    fEntry[2] := TJwDiscretionaryAccessControlEntryAllow.Create(List,[],GENERIC_ALL,nil,true);
    fEntry[2].Free; //to work this the ownObjects of List must be false
    fEntry[2] := nil;

  finally
    aSID.Free;
    List.Free;
  end;
end;





procedure TJwSecurityAccessControlEntryTests.Test_MandatoryLabel;
var audit1 : TJwSAccessControlList;
    ML : TJwSystemMandatoryAccessControlEntry;
begin
  audit1 := TJwSAccessControlList.Create;
  try
    CheckEquals(0,audit1.Count);
    CheckFalse(audit1.HasMandatoryLabel);

    audit1.SetMandatoryLabel(TJwSystemMandatoryAccessControlEntry.Create(MandatoryLevelHigh),
      cfPointAtInstance);

    CheckEquals(1,audit1.Count);
    CheckTrue(audit1.HasMandatoryLabel);

    audit1.SetMandatoryLabel(TJwSystemMandatoryAccessControlEntry.Create(MandatoryLevelLow),
      cfPointAtInstance);
    CheckEquals(1,audit1.Count);
    CheckTrue(audit1.HasMandatoryLabel);

    audit1.SetMandatoryLabel(nil,cfCopyInstance);

    CheckEquals(0,audit1.Count);
    CheckFalse(audit1.HasMandatoryLabel);


    CheckSame(nil, audit1.MandatoryLabel);

    ML := TJwSystemMandatoryAccessControlEntry.Create(MandatoryLevelLow);
    try
      audit1.SetMandatoryLabel(ML,cfCopyInstance);

      CheckFalse(ML = audit1.MandatoryLabel,'Created MandatorLabel and'+
        'added one must not be the same instance');

      audit1.MandatoryLabel := nil;
      CheckEquals(0,audit1.Count);
      CheckFalse(audit1.HasMandatoryLabel);

      audit1.MandatoryLabel := ML;

      CheckFalse(ML = audit1.MandatoryLabel,'Created MandatorLabel and'+
        'added one must not be the same instance');

      audit1.MandatoryLabel := nil;
      CheckSame(nil, audit1.MandatoryLabel);
    finally
      ML.Free;
    end;
  finally
    audit1.Free;
  end;
end;

{ TJwSecurityAccessControlListTests }

procedure TJwSecurityAccessControlListTests.SetUp;
var i : Integer;
begin
  inherited;

  for i := low(fList) to high(fList) do
    fList[i] := nil;
end;

procedure TJwSecurityAccessControlListTests.TearDown;
var i : Integer;
begin
  inherited;

  for i := low(fList) to high(fList) do
    if Assigned(fList[i]) then
      fList[i].Free;

end;

procedure TJwSecurityAccessControlListTests.Test_Create_DAccessControlList;
var pPACL : PACL;
    aPACE : PACE;
    aPSID : PSID;
    aSID,aSID2  : TJwSecurityId;
begin
  fList[1] := TJwDAccessControlList.Create;
  CheckEquals(0,fList[1].Count,'Invalid count');

  fList[2] := TJwDAccessControlList.Create(true);
  CheckTrue(fList[2].OwnsObjects,'Invalid ownObjects');

  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);
   TJwDiscretionaryAccessControlEntryAllow.Create(fList[2],[afObjectInheritAce],GENERIC_READ,aSID);
   TJwDiscretionaryAccessControlEntryAllow.Create(fList[2],[],GENERIC_WRITE,nil);
  CheckEquals(2,fList[2].Count,'Invalid count');

  try
    pPACL := fList[2].Create_PACL;
    CheckIs(nil,EJwsclInvalidSIDException);
  except
    on E : Exception do
      CheckIs(E,EJwsclInvalidSIDException);
  end;

  fList[2].Items[1].SID := TJwSecurityId.CreateWellKnownSid(WinLocalSid,nil);
  fList[2].Items[1].OwnSID := true;

  pPACL := fList[2].Create_PACL;
  CheckTrue(IsValidAcl(pPACL),'Invalid ACL');
  CheckEquals(2,pPACL.AceCount,'Invalid count in ACL');

  fList[3] := TJwDAccessControlList.Create(false);
  CheckFalse(fList[3].OwnsObjects,'Invalid OwnObjects');


  fList[4] := TJwDAccessControlList.Create(pPACL);
  CheckEquals(fList[2].Count,fList[4].Count,'Invalid count');

  CheckTrue(fList[2].Items[0].SID.EqualSid(fList[4].Items[0].SID),'Invalid SID in ACE');
  CheckTrue(fList[2].Items[1].SID.EqualSid(fList[4].Items[1].SID),'Invalid SID in ACE');

  fList[2].Free_PACL(pPACL);
end;

procedure TJwSecurityAccessControlListTests.Test_Create_SAccessControlList;
var pPACL : PACL;
    aPACE : PACE;
    aPSID : PSID;
    aSID,aSID2  : TJwSecurityId;

begin
  fList[1] := TJwSAccessControlList.Create;
  CheckEquals(0,fList[1].Count,'Invalid count');

  fList[2] := TJwSAccessControlList.Create(true);
  CheckTrue(fList[2].OwnsObjects,'Invalid ownObjects');

  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);
   TJwAuditAccessControlEntry.Create(fList[2],[afObjectInheritAce],GENERIC_READ,aSID);
   TJwAuditAccessControlEntry.Create(fList[2],[],GENERIC_WRITE,nil);
  CheckEquals(2,fList[2].Count,'Invalid count');

  try
    pPACL := fList[2].Create_PACL;
    CheckIs(nil,EJwsclInvalidSIDException);
  except
    on E : Exception do
      CheckIs(E,EJwsclInvalidSIDException);
  end;

  fList[2].Items[1].SID := TJwSecurityId.CreateWellKnownSid(WinLocalSid,nil);
  fList[2].Items[1].OwnSID := true;

  pPACL := fList[2].Create_PACL;
  CheckTrue(IsValidAcl(pPACL),'Invalid ACL');
  CheckEquals(2,pPACL.AceCount,'Invalid count in ACL');

  fList[3] := TJwSAccessControlList.Create(false);
  CheckFalse(fList[3].OwnsObjects,'Invalid OwnObjects');


  fList[4] := TJwSAccessControlList.Create(pPACL);
  CheckEquals(fList[2].Count,fList[4].Count,'Invalid count');

  CheckTrue(fList[2].Items[0].SID.EqualSid(fList[4].Items[0].SID),'Invalid SID in ACE');
  CheckTrue(fList[2].Items[1].SID.EqualSid(fList[4].Items[1].SID),'Invalid SID in ACE');

  fList[2].Free_PACL(pPACL);
end;

procedure TJwSecurityAccessControlListTests.Test_Add_Insert_Remove;
var pPACL : PACL;
    aPACE : PACE;
    aPSID : PSID;
    aSID,aSID2   : TJwSecurityId;
    aSIDs : array[1..10] of TJwSecurityId;
    aPosACEs : array[1..10] of TJwDiscretionaryAccessControlEntryAllow;
    aNegACEs : array[1..10] of TJwDiscretionaryAccessControlEntryDeny;

    fEntry : TJwSecurityAccessControlEntry;
begin
  fList[1] := TJwSAccessControlList.Create(true);
  fList[2] := TJwDAccessControlList.Create(true);

  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);


  {fList[1] := TJwSAccessControlList}
  try
     //do not set OwnSID to true, because the exception automatically calls .Destroy
    TJwDiscretionaryAccessControlEntryAllow.Create(fList[1],[afObjectInheritAce],GENERIC_READ,aSID,false);
    CheckIs(nil,EJwsclInvalidACEException,'EJwsclInvalidACEException should be raised but did not.');
  except
    on E : Exception do
      CheckIs(E,EJwsclInvalidACEException,'Exception was not an EJwsclInvalidACEException');
  end;

  try
     //do not set OwnSID to true, because the exception automatically calls .Destroy
    fEntry := TJwDiscretionaryAccessControlEntryAllow.Create(nil,[afObjectInheritAce],GENERIC_READ,aSID,false);
    try
      fList[1].Add(fEntry);
      CheckIs(nil,EJwsclInvalidACEException,'EJwsclInvalidACEException should be raised but did not.');
    except
      on E : Exception do
      begin
        fEntry.Free;
        CheckIs(E,EJwsclInvalidACEException,'Exception was not an EJwsclInvalidACEException');
      end;
    end;
  finally
  end;

  try
    fList[1].Add(nil);
    CheckIs(nil,EJwsclNILParameterException,'EJwsclNILParameterException should be raised but did not.');
  except
    on E : Exception do
      CheckIs(E,EJwsclNILParameterException,'Exception was not an EJwsclNILParameterException');
  end;

{check duplicate item}
  fEntry := TJwAuditAccessControlEntry.Create(nil,[afObjectInheritAce],GENERIC_READ,aSID,false);
  fList[1].Add(fEntry);
  try
    fList[1].Add(fEntry);
    CheckIs(nil,EJwsclDuplicateListEntryException,'EJwsclDuplicateListEntryException should be raised but did not');
  except
    on E : Exception do
      CheckIs(E,EJwsclDuplicateListEntryException,'Exception was not an EJwsclDuplicateListEntryException');
  end;

{INSERT invalid entry}
  try
    //do not set OwnSID to true, because the exception automatically calls .Destroy
    fEntry := TJwDiscretionaryAccessControlEntryAllow.Create(nil,[afObjectInheritAce],GENERIC_READ,aSID,false);

    try
      fList[1].Insert(0,fEntry);
      CheckIs(nil,EJwsclInvalidACEException,'EJwsclInvalidACEException should be raised but did not');
    except
      on E : Exception do
      begin
        fEntry.Free;
        CheckIs(E,EJwsclInvalidACEException,'Exception was not an EJwsclInvalidACEException');
      end;
    end;
  finally
  end;

  try
    fList[1].Insert(0,nil);
    CheckIs(nil,EJwsclNILParameterException,'EJwsclNILParameterException did not raise');
  except
    on E : Exception do
      CheckIs(E,EJwsclNILParameterException,'Exception was not EJwsclNILParameterException');
  end;
{check duplicate item}
  fEntry := TJwAuditAccessControlEntry.Create(nil,[afObjectInheritAce],GENERIC_READ,aSID,false);
  fList[1].Insert(0,fEntry);
  try
    fList[1].Insert(0,fEntry);
    CheckIs(nil,EJwsclDuplicateListEntryException,'EJwsclDuplicateListEntryException did not raise');
  except
    on E : Exception do
      CheckIs(E,EJwsclDuplicateListEntryException,'Exception is not EJwsclDuplicateListEntryException');
  end;






  {fList[2] := TJwDAccessControlList}
  try
    //do not set OwnSID to true, because the exception automatically calls .Destroy
    TJwAuditAccessControlEntry.Create(fList[2],[afObjectInheritAce],GENERIC_READ,aSID,false);
    CheckIs(nil,EJwsclInvalidACEException,'EJwsclInvalidACEException did not raise');
  except
    on E : Exception do
      CheckIs(E,EJwsclInvalidACEException,'Exception was not EJwsclInvalidACEException');
  end;


  try
     //do not set OwnSID to true, because the exception automatically calls .Destroy
     fEntry := TJwAuditAccessControlEntry.Create(nil,[afObjectInheritAce],GENERIC_READ,aSID,false);
    try
       fList[2].Add(fEntry);
       CheckIs(nil,EJwsclInvalidSIDException,'EJwsclInvalidSIDException did not raise');
    except
      on E : Exception do
      begin
        CheckIs(E,EJwsclInvalidACEException,'Exception was not EJwsclInvalidSIDException');
        fEntry.Free;
      end;
    end;
  finally
  end;


  try
    fList[2].Add(nil);
    CheckIs(nil,EJwsclNILParameterException,'EJwsclNILParameterException did not raise');
  except
    on E : Exception do
      CheckIs(E,EJwsclNILParameterException,'Exception was not EJwsclNILParameterException');
  end;



{check duplicate item}
  try
    fEntry := TJwDiscretionaryAccessControlEntryAllow.Create(nil,[afObjectInheritAce],GENERIC_READ,aSID,false);
     fList[2].Add(fEntry);
    try
      fList[2].Add(fEntry);
      CheckIs(nil,EJwsclDuplicateListEntryException,'EJwsclDuplicateListEntryException did not raise');
    except
      on E : Exception do
      CheckIs(E,EJwsclDuplicateListEntryException,'Exception was not EJwsclDuplicateListEntryException');
    end;
  except

  end;

{INSERT invalid entry}

  try
    fList[2].Insert(0,nil);
    CheckIs(nil,EJwsclNILParameterException);
  except
    on E : Exception do
      CheckIs(E,EJwsclNILParameterException);
  end;

{check duplicate item}
  try
     //do not set OwnSID to true, because the exception automatically calls .Destroy
    fEntry := TJwDiscretionaryAccessControlEntryAllow.Create(nil,[afObjectInheritAce],GENERIC_READ,aSID,false);
    fList[2].Add(fEntry);
    try
      fList[2].Add(fEntry);
      CheckIs(nil,EJwsclDuplicateListEntryException);
    except
      on E : Exception do
      CheckIs(E,EJwsclDuplicateListEntryException);
    end;
  except

  end;

  fList[2].Clear;

  aSIDs[1] := TJwSecurityId.CreateWellKnownSid(WinNullSid);
  aSIDs[2] := TJwSecurityId.CreateWellKnownSid(WinWorldSid);
  aSIDs[3] := TJwSecurityId.CreateWellKnownSid(WinLocalSid);
  aSIDs[4] := TJwSecurityId.CreateWellKnownSid(WinCreatorOwnerSid);

  //add ACEs
  aPosACEs[1] := TJwDiscretionaryAccessControlEntryAllow.Create(fList[2],[afInheritedAce],GENERIC_READ,aSIDs[1],true);
  aNegACEs[1] := TJwDiscretionaryAccessControlEntryDeny. Create(fList[2],[],GENERIC_READ,aSIDs[2],true);

  CheckSame(aNegACEs[1],fList[2].Items[0],'ACE is not in correct position in list'); //1. NEG 1 (added)
  CheckSame(aPosACEs[1],fList[2].Items[1],'ACE is not in correct position in list'); //2. POS 1 inherited (added)

  aPosACEs[2] := TJwDiscretionaryAccessControlEntryAllow.Create(fList[2],[],GENERIC_READ,aSIDs[3],true);

  CheckSame(aNegACEs[1],fList[2].Items[0],'ACE is not in correct position in list'); //1. NEG 1
  CheckSame(aPosACEs[2],fList[2].Items[1],'ACE is not in correct position in list'); //2. POS 2 (added)
  CheckSame(aPosACEs[1],fList[2].Items[2],'ACE is not in correct position in list'); //3. POS 1 inherited

  aNegACEs[2] := TJwDiscretionaryAccessControlEntryDeny.Create(fList[2],[],GENERIC_READ,aSIDs[4],true);

  CheckSame(aNegACEs[2],fList[2].Items[0],'ACE is not in correct position in list'); //1. NEG 2 (added)
  CheckSame(aNegACEs[1],fList[2].Items[1],'ACE is not in correct position in list'); //2. NEG 1
  CheckSame(aPosACEs[2],fList[2].Items[2],'ACE is not in correct position in list'); //3. POS 2
  CheckSame(aPosACEs[1],fList[2].Items[3],'ACE is not in correct position in list'); //4. POS 1 inherited


  aNegACEs[3] := TJwDiscretionaryAccessControlEntryDeny.Create(fList[2],[],GENERIC_WRITE,aSIDs[4],false);

  CheckSame(aNegACEs[3],fList[2].Items[0],'ACE is not in correct position in list'); //1. NEG 3 (added)
  CheckSame(aNegACEs[2],fList[2].Items[1],'ACE is not in correct position in list'); //2. NEG 2
  CheckSame(aNegACEs[1],fList[2].Items[2],'ACE is not in correct position in list'); //3. NEG 1
  CheckSame(aPosACEs[2],fList[2].Items[3],'ACE is not in correct position in list'); //4. POS 2
  CheckSame(aPosACEs[1],fList[2].Items[4],'ACE is not in correct position in list'); //5. POS 1 inherited

  aPosACEs[4] := TJwDiscretionaryAccessControlEntryAllow.Create(fList[2],[],GENERIC_READ,aSIDs[4],false);

  CheckSame(aNegACEs[3],fList[2].Items[0],'ACE is not in correct position in list'); //1. NEG 3
  CheckSame(aNegACEs[2],fList[2].Items[1],'ACE is not in correct position in list'); //2. NEG 2
  CheckSame(aNegACEs[1],fList[2].Items[2],'ACE is not in correct position in list'); //3. NEG 1
  CheckSame(aPosACEs[2],fList[2].Items[3],'ACE is not in correct position in list'); //4. POS 2
  CheckSame(aPosACEs[4],fList[2].Items[4],'ACE is not in correct position in list'); //5. POS 4 (added)
  CheckSame(aPosACEs[1],fList[2].Items[5],'ACE is not in correct position in list'); //6. POS 1 inherited

  aPosACEs[5] := TJwDiscretionaryAccessControlEntryAllow.Create(fList[2],[afInheritedAce],GENERIC_READ,aSIDs[4],false);

  CheckSame(aNegACEs[3],fList[2].Items[0],'ACE is not in correct position in list'); //1. NEG 3
  CheckSame(aNegACEs[2],fList[2].Items[1],'ACE is not in correct position in list'); //2. NEG 2
  CheckSame(aNegACEs[1],fList[2].Items[2],'ACE is not in correct position in list'); //3. NEG 1
  CheckSame(aPosACEs[2],fList[2].Items[3],'ACE is not in correct position in list'); //4. POS 2
  CheckSame(aPosACEs[4],fList[2].Items[4],'ACE is not in correct position in list'); //5. POS 3
  CheckSame(aPosACEs[1],fList[2].Items[5],'ACE is not in correct position in list'); //6. POS 1 inherited
  CheckSame(aPosACEs[5],fList[2].Items[6],'ACE is not in correct position in list'); //7. POS 5 inherited (added)

  aNegACEs[5] := TJwDiscretionaryAccessControlEntryDeny.Create(fList[2],[afInheritedAce],GENERIC_READ,aSIDs[4],false);

  CheckSame(aNegACEs[3],fList[2].Items[0],'ACE is not in correct position in list'); //1. NEG 3
  CheckSame(aNegACEs[2],fList[2].Items[1],'ACE is not in correct position in list'); //2. NEG 2
  CheckSame(aNegACEs[1],fList[2].Items[2],'ACE is not in correct position in list'); //3. NEG 1
  CheckSame(aPosACEs[2],fList[2].Items[3],'ACE is not in correct position in list'); //4. POS 2
  CheckSame(aPosACEs[4],fList[2].Items[4],'ACE is not in correct position in list'); //5. POS 3
  CheckSame(aNegACEs[5],fList[2].Items[5],'ACE is not in correct position in list'); //6. NEG 1 inherited
  CheckSame(aPosACEs[1],fList[2].Items[6],'ACE is not in correct position in list'); //6. POS 1 inherited
  CheckSame(aPosACEs[5],fList[2].Items[7],'ACE is not in correct position in list'); //7. POS 5 inherited (added)

  ShowMessageForm(fList[2].Text);

  fList[2].Clear;

  aNegACEs[6] := TJwDiscretionaryAccessControlEntryDeny.Create(fList[2],[afInheritedAce],GENERIC_READ,nil,false);
  aPosACEs[6] := TJwDiscretionaryAccessControlEntryAllow.Create(fList[2],[afInheritedAce],GENERIC_READ,nil,false);

  CheckSame(aNegACEs[6],fList[2].Items[0],'ACE is not in correct position in list'); //1. NEG inh
  CheckSame(aPosACEs[6],fList[2].Items[1],'ACE is not in correct position in list'); //2. POS inh

  aNegACEs[7] := TJwDiscretionaryAccessControlEntryDeny.Create(fList[2],[],GENERIC_READ,nil,false);
  aPosACEs[7] := TJwDiscretionaryAccessControlEntryAllow.Create(fList[2],[],GENERIC_READ,nil,false);

  CheckSame(aNegACEs[7],fList[2].Items[0],'ACE is not in correct position in list'); //1. NEG
  CheckSame(aPosACEs[7],fList[2].Items[1],'ACE is not in correct position in list'); //2. POS
  CheckSame(aNegACEs[6],fList[2].Items[2],'ACE is not in correct position in list'); //3. NEG inh
  CheckSame(aPosACEs[6],fList[2].Items[3],'ACE is not in correct position in list'); //4. POS inh


  ShowMessageForm(fList[2].Text);

 aSID.Free;
 fList[2].Free;
 fList[2] := nil;
end;

procedure TJwSecurityAccessControlListTests.Test_Assign;
var fEntry : TJwSecurityAccessControlEntry;
    aSID : TJwSecurityId;
begin
  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);

  fList[1] := TJwSAccessControlList.Create(true);
  //do not set ownSID parameter to true!
  TJwAuditAccessControlEntry.Create(fList[1],[afObjectInheritAce],GENERIC_READ,aSID,false);

  fList[2] := TJwDAccessControlList.Create(true);
  //do not set ownSID parameter to true!
  TJwDiscretionaryAccessControlEntryAllow.Create(fList[2],[afObjectInheritAce],GENERIC_READ,aSID,false);


  CheckEquals(1,fList[2].Count);

  try
    fList[1].Assign(nil);
    CheckIs(nil,EJwsclNILParameterException);
  except
    on E : EXception do
      CheckIs(E,EJwsclNILParameterException);
  end;

  //check for ACL mismatch  audit <-> discr.
  //in Assign
  try
    fList[2].Assign(fList[1]);
    CheckIs(nil,EJwsclInvalidACEException);
  except
    on E : EXception do
      CheckIs(E,EJwsclInvalidACEException);
  end;

  //check for ACL mismatch  audit <-> discr.
  //in AddACEs
  try
    fList[2].AddACEs(fList[1]);
    CheckIs(nil,EJwsclInvalidACEException);
  except
    on E : EXception do
      CheckIs(E,EJwsclInvalidACEException);
  end;


  TJwDiscretionaryAccessControlEntryAllow.Create(fList[2],[afObjectInheritAce],GENERIC_READ,aSID,false);

  fList[3] := TJwDAccessControlList.Create(true);
  fList[3].Assign(fList[2]);

  CheckEquals(1,fList[3].Count);

  fList[3].AddACEs(fList[2]);
  CheckEquals(2,fList[3].Count);

  aSID.Free;
end;



procedure TJwSecurityAccessControlListTests.Test_CreateFree_PACL;
var pPACL : PACL;
    aPACE : PACE;
    aPSID : PSID;
    aSID,aSID2  : TJwSecurityId;
begin
  fList[1] := TJwDAccessControlList.Create;
  CheckEquals(0,fList[1].Count,'Invalid count');


  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);
   TJwDiscretionaryAccessControlEntryAllow.Create(fList[1],[],GENERIC_READ,aSID,false);
   TJwDiscretionaryAccessControlEntryAllow.Create(fList[1],[],GENERIC_WRITE,nil,false);
  CheckEquals(2,fList[1].Count,'Invalid count');


  //check for nil SID
  try
    pPACL := fList[1].Create_PACL;
    CheckIs(nil,EJwsclInvalidSIDException);
  except
    on E : Exception do
      CheckIs(E,EJwsclInvalidSIDException);
  end;

  fList[1].Items[1].SID := fList[1].Items[0].SID;

  pPACL := fList[1].Create_PACL;
  CheckTrue(IsValidAcl(pPACL),'Invalid ACL');
  CheckEquals(2,pPACL.AceCount,'Invalid count in ACL');

  fList[2] := TJwDAccessControlList.Create(pPACL);
  CheckEquals(fList[2].Count,fList[1].Count,'Invalid count');


  fList[1].Free_PACL(pPACL);
  aSID.Free;
end;

procedure TJwSecurityAccessControlListTests.Test_FirstLast;
var f1,f2 : TJwSecurityAccessControlEntry;
begin
  fList[1] := TJwDAccessControlList.Create;

  CheckSame(nil,fList[1].First);
  CheckSame(nil,fList[1].Last);

  TJwDiscretionaryAccessControlEntryAllow.Create(fList[1],[],GENERIC_READ,nil,false);

  CheckSame(fList[1][0],fList[1].First);
  CheckSame(fList[1][0],fList[1].Last);

  f1 := TJwDiscretionaryAccessControlEntryDeny.Create(fList[1],[],GENERIC_READ,nil,false);
  f2 := TJwDiscretionaryAccessControlEntryAllow.Create(fList[1],[],GENERIC_READ,nil,false);

  CheckSame(f1,fList[1].First);
  CheckSame(f2,fList[1].Last);


  fList[1].Clear;

  CheckSame(nil,fList[1].First);
  CheckSame(nil,fList[1].Last);
end;

procedure TJwSecurityAccessControlListTests.Test_IndexOf;
var f1,f2 : TJwSecurityAccessControlEntry;
begin
  fList[1] := TJwDAccessControlList.Create;

  f1 := TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],GENERIC_READ,nil,false);

  CheckEquals(-1,fList[1].IndexOf(f1));

  fList[1].Add(f1);
  CheckEquals(0,fList[1].IndexOf(f1));

  f2 := TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],GENERIC_READ,nil,false);
  fList[1].Add(f2);

  CheckEquals(1,fList[1].IndexOf(f1));
  CheckEquals(0,fList[1].IndexOf(f2));



end;

procedure TJwSecurityAccessControlListTests.Test_GetText;
var pPACL : PACL;
    aPACE : PACE;
    aPSID : PSID;
    aSID,aSID2  : TJwSecurityId;
begin
  fList[1] := TJwDAccessControlList.Create;
  CheckEquals(0,fList[1].Count,'Invalid count');


  aSID := TJwSecurityId.CreateWellKnownSid(WinWorldSid,nil);
  try
   TJwDiscretionaryAccessControlEntryAllow.Create(fList[1],[afObjectInheritAce,afContainerInheritAce,afNoPropagateInheritAce],GENERIC_READ,aSID,false);
   TJwDiscretionaryAccessControlEntryAllow.Create(fList[1],[],GENERIC_WRITE,nil,false);

   ShowMessageForm(fList[1].Text);

  finally
   aSID.Free;
  end;
end;

initialization

  TestFramework.RegisterTest('JwsclAclTests Suite',
    TJwSecurityAccessControlListTests.Suite);
  TestFramework.RegisterTest('JwsclAclTests Suite',
    TJwSecurityAccessControlEntryTests.Suite);

end.
