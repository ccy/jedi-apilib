unit JwsclSecurePrivateObjectsTests;

interface

uses
  JwsclSecurePrivateObjects,
  Dialogs,
  TestFrameWork,
  jwaWindows,
  JwsclTypes, JwsclExceptions, JwsclAcl, JwsclMapping, JwsclSid,
  JwsclSecureObjects, JwsclResource, JwsclKnownSid,
  JwsclVersion, JwsclConstants, JwsclProcess, JwsclDescriptor, JwsclToken,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!;



type
  TSecurePrivateObject = class;
  TJwSecurePrivateObjectTests = class(TTestCase)
  private
    PrivObj : array[0..30,0..30] of TSecurePrivateObject;
  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestCheckMapGenericMask;
    procedure TestGetSecurityDescriptor;
    procedure TestSetSecurityDescriptor;
    procedure TestGetPrivateInheritanceSource;
    procedure TestRemoveInheritanceFlow;
    procedure TestRestoreInheritanceFlow;
    procedure TestTakeOwnerShip;
    procedure TestAccessCheck;
    procedure TestTreeFileObjectSetNamedSecurityInfo;
  end;

  TSecurePrivateObject = class(TJwInterfacedPrivateSecurityInformation)
  protected
    fInfo : TJwSecurityObjectInformation;
    fPrivateSecurityInformation : IJwPrivateSecurityInformation;
    fHasParent : Boolean;
    fUseAccessCheck : Boolean;
    fGenericMap : TJwSecurityGenericMappingClass;
    fIsDirectoryObject : Boolean;
    fInheritFlags : TJwInheritFlagSet;
    fChildren : TJwPrivateSecurityInformationArray;

  protected
    function GetName : TJwString;
    procedure SetName(const Name : TJwString);

    procedure SetParentProp(const NewParent : IJwPrivateSecurityInformation);
    function GetParentProp : IJwPrivateSecurityInformation;

  protected //inherited by IJwPrivateSecurityInformation
    function GetObjectInformation(const ObjectInformationSet :
            TJwSecurityObjectInformationFlagSet): TJwSecurityObjectInformation; override;
    function GetParent(out Parent : IJwPrivateSecurityInformation) : HRESULT; override;

    function GetUseAccessCheck(const AccessCheckType : TJwGetAccessCheckType)
      : Boolean; override;
    function MapGenericMask(out GenericMap : TJwSecurityGenericMappingClass)
      : HRESULT; override;
    function GetChildren(out Children : TJwPrivateSecurityInformationArray) : HRESULT; override;

  protected //inherited by TJwInterfacedPrivateSecurityInformation
    function GetInheritFlags(const FlagsType : TJwGetInheritFlagsType) : TJwInheritFlagSet; override;
    function GetIsDirectoryObject : Boolean; override;

    procedure Done;
  public
    constructor Create(const Name : TJwString;
      const UseDefault : Boolean = false;
      const DefaultSecurityDescriptor : TJwSecurityDescriptor = nil); overload;
    constructor CreateParented(const Name : TJwString;
      Parent : TSecurePrivateObject;
      const UseDefault : Boolean = false;
      const DefaultSecurityDescriptor: TJwSecurityDescriptor = nil); overload;

    destructor Destroy; override;

    procedure Add(Child : TSecurePrivateObject);

    property Info : TJwSecurityObjectInformation read fInfo write fInfo;
    property Parent : IJwPrivateSecurityInformation read GetParentProp write SetParentProp;

    {Set to true if this structure supports parents}
    property HasParent : Boolean read fHasParent write fHasParent;
    property UseAccessCheck : Boolean read fUseAccessCheck write fUseAccessCheck;
    property GenericMap : TJwSecurityGenericMappingClass read fGenericMap write fGenericMap;
    property Name : TJwString read GetName write SetName;

    property IsDirectoryObject : Boolean read fIsDirectoryObject write fIsDirectoryObject;
    property InheritFlags : TJwInheritFlagSet read fInheritFlags write fInheritFlags;
  end;


implementation
uses SysUtils;

{ TJwSecurePrivateObjectTests }

procedure TJwSecurePrivateObjectTests.SetUp;
var i : Integer;

begin
  inherited;

  //create secure object with default SD (from token)
  PrivObj[0,1] := TSecurePrivateObject.Create('Level 1 [Container]- ProcessSID', true);
  PrivObj[0,1].fSecurityDescriptor.DACL.Clear; //clear default DACL
  PrivObj[0,1].fSecurityDescriptor.DACL.Add(
    TJwDiscretionaryAccessControlEntryAllow.Create(nil,TJwAfThisFolderAndSubFoldersAndFiles,
      JwAllSecurityAccess,
      JwSecurityProcessUserSID, false));

  //create secure object without default SD - instead we want inheritance
  PrivObj[1,1] := TSecurePrivateObject.Create('Level 1.1 [Container]');
  PrivObj[1,1].IsDirectoryObject := true; //empty container
  PrivObj[0,1].Add(PrivObj[1,1]);

  PrivObj[1,2] := TSecurePrivateObject.Create('Level 1.2 [Object] + Administrators');
  PrivObj[0,1].Add(PrivObj[1,2]);

  PrivObj[1,2].fSecurityDescriptor.DACL.Add(
    TJwDiscretionaryAccessControlEntryAllow.Create(nil,TJwAfThisFolderAndSubFoldersAndFiles,
      JwAllSecurityAccess,
      JwAdministratorsSID, false));


  PrivObj[1,3] := TSecurePrivateObject.Create('Level 1.3 [Object]- Protected ACL - GuestSID');
  //inherits Owner from Level 1 - only owner and guest have access
  PrivObj[1,3].fSecurityDescriptor.Control := PrivObj[1,2].fSecurityDescriptor.Control + [sdcDaclProtected];
  PrivObj[1,3].fSecurityDescriptor.DACL.Clear;
  PrivObj[1,3].fSecurityDescriptor.DACL.Add(
    TJwDiscretionaryAccessControlEntryAllow.Create(nil,TJwAfThisFolderAndSubFoldersAndFiles,
      JwAllSecurityAccess,
      JwGuestsSID, false));

  PrivObj[1,5] := TSecurePrivateObject.Create('Level 1.5 [Object]- Protected ACL - GuestSID');
  PrivObj[1,5].fSecurityDescriptor.Control := PrivObj[1,5].fSecurityDescriptor.Control +
    [sdcDaclProtected,sdcOwnerDefaulted];
  PrivObj[1,5].fSecurityDescriptor.Owner := JwLocalSystemSID; //if set to nil, the parent owner will be used!

  PrivObj[1,5].fSecurityDescriptor.DACL.Clear;
  PrivObj[1,5].fSecurityDescriptor.DACL.Add(
    TJwDiscretionaryAccessControlEntryAllow.Create(nil,TJwAfThisFolderAndSubFoldersAndFiles,
      JwAllSecurityAccess,
      JwGuestsSID, false));

  PrivObj[0,1].Add(PrivObj[1,5]);
  PrivObj[0,1].Add(PrivObj[1,3]);

  PrivObj[2,1] := TSecurePrivateObject.Create('Level 1.1.1 [Object]- inherited');
  PrivObj[1,1].Add(PrivObj[2,1]);

  PrivObj[2,2] := TSecurePrivateObject.Create('Level 1.1.2 [Container]- Administrators');
  PrivObj[1,1].Add(PrivObj[2,2]);
  PrivObj[2,2].IsDirectoryObject := true; //empty container
  PrivObj[2,2].fSecurityDescriptor.DACL.Add(
    TJwDiscretionaryAccessControlEntryAllow.Create(nil,TJwAfThisFolderAndSubFoldersAndFiles,
      JwAllSecurityAccess,
      JwAdministratorsSID, false));

  PrivObj[4,1] := TSecurePrivateObject.Create('Level 1.4 [Object]- inherited');
  PrivObj[0,1].Add(PrivObj[4,1]);
end;

procedure TJwSecurePrivateObjectTests.TearDown;
var x,y : Integer;
begin
  //first destroy connections between parents and childs
  //its a circle so reference counting does not work!
  for x := low(PrivObj) to high(PrivObj) do
    for y := low(PrivObj[0]) to high(PrivObj[0]) do
      if Assigned(PrivObj[x,y]) then
        PrivObj[x,y].Done;

  for x := low(PrivObj) to high(PrivObj) do
    for y := low(PrivObj[0]) to high(PrivObj[0]) do
    begin
      if Assigned(PrivObj[x,y]) then
      begin
        if PrivObj[x,y]._Release = 0 then
          PrivObj[x,y] := nil;
      end;
    end;
  inherited;
end;

procedure TJwSecurePrivateObjectTests.TestAccessCheck;
begin
  CheckTrue(false, 'No implemented');
end;

procedure TJwSecurePrivateObjectTests.TestCheckMapGenericMask;
begin
  CheckTrue(false, 'No implemented');
end;

procedure TJwSecurePrivateObjectTests.TestGetPrivateInheritanceSource;
begin
  CheckTrue(false, 'No implemented');
end;

procedure TJwSecurePrivateObjectTests.TestGetSecurityDescriptor;
var SD : TJwSecurityDescriptor;
begin

  try
    {SD := TJwSecurePrivateObject.GetSecurityDescriptor(
      PrivObj1, [siOwnerSecurityInformation,siDaclSecurityInformation
          ]);
  //  CheckEquals(Privobj1.fSecurityDescriptor.DACL.Count+1,SD.DACL.Count);

    //ShowMessage(SD.Text);
    SD.Free;   }


    SD := TJwSecurePrivateObject.GetSecurityDescriptor(
      PrivObj[0,1], [siDaclSecurityInformation, siOwnerSecurityInformation]);
    try
   //   ShowMessage(PrivObj[0,1].Name+SD.Text);
      CheckNotNull(SD);
      CheckNotNull(SD.DACL);
      CheckNotNull(SD.Owner);
      CheckTrue(SD.Owner.EqualSid(JwSecurityProcessUserSID));
      CheckEquals(1, SD.DACL.Count);
    finally
      SD.Free;
    end;



    SD := TJwSecurePrivateObject.GetSecurityDescriptor(
      PrivObj[1,1], [siDaclSecurityInformation, siOwnerSecurityInformation]);
    try
     // ShowMessage(PrivObj[1,1].Name+#13#10+SD.Text);
      CheckNotNull(SD);
      CheckNotNull(SD.DACL);
      CheckNotNull(SD.Owner);
      CheckTrue(SD.Owner.EqualSid(JwSecurityProcessUserSID));
      CheckEquals(1, SD.DACL.Count);
    finally
      SD.Free;
    end;

    SD := TJwSecurePrivateObject.GetSecurityDescriptor(
      PrivObj[1,2], [siDaclSecurityInformation, siOwnerSecurityInformation]);
    try
     // ShowMessage(PrivObj[1,2].Name+#13#10+SD.Text);
      CheckNotNull(SD);
      CheckNotNull(SD.DACL);
      CheckNotNull(SD.Owner);
      CheckTrue(SD.Owner.EqualSid(JwSecurityProcessUserSID));
      CheckEquals(2, SD.DACL.Count);
    finally
      SD.Free;
    end;

    SD := TJwSecurePrivateObject.GetSecurityDescriptor(
      PrivObj[1,3], [siDaclSecurityInformation, siOwnerSecurityInformation]);
    try
      //ShowMessage(PrivObj[1,3].Name+#13#10+SD.Text);
      CheckNotNull(SD);
      CheckNotNull(SD.DACL);
      CheckNotNull(SD.Owner);
      CheckTrue(SD.Owner.EqualSid(JwSecurityProcessUserSID));
      CheckEquals(1, SD.DACL.Count);
    finally
      SD.Free;
    end;

     try
      SD := TJwSecurePrivateObject.GetSecurityDescriptor(
        PrivObj[1,5], [siDaclSecurityInformation, siOwnerSecurityInformation]);
      CheckTrue(false, 'This call should raise an access denied - make sure your are not LocalSYSTEM');
    except
      on E : EJwsclSecurityException do
        CheckIs(E,EJwsclAccessDenied);
    end;

    SD := TJwSecurePrivateObject.GetSecurityDescriptor(
      PrivObj[2,1], [siDaclSecurityInformation, siOwnerSecurityInformation]);
    try
      //ShowMessage(PrivObj[2,1].Name+#13#10+SD.Text);
      CheckNotNull(SD);
      CheckNotNull(SD.DACL);
      CheckNotNull(SD.Owner);
      CheckTrue(SD.Owner.EqualSid(JwSecurityProcessUserSID));
      CheckEquals(1, SD.DACL.Count);
    finally
      SD.Free;
    end;

      SD := TJwSecurePrivateObject.GetSecurityDescriptor(
      PrivObj[2,2], [siDaclSecurityInformation, siOwnerSecurityInformation]);
    try
      ShowMessage(PrivObj[2,2].Name+#13#10+SD.Text);
      CheckNotNull(SD);
      CheckNotNull(SD.DACL);
      CheckNotNull(SD.Owner);
      CheckTrue(SD.Owner.EqualSid(JwSecurityProcessUserSID));
      CheckEquals(2, SD.DACL.Count);
    finally
      SD.Free;
    end;

  finally

  end;
end;

procedure TJwSecurePrivateObjectTests.TestSetSecurityDescriptor;
var SD : TJwSecurityDescriptor;
    oldS : TJwString;
begin
  //create explicit DACL
  PrivObj[4,1].fSecurityDescriptor.Owner := JwNullSID;
 // PrivObj[4,1].fSecurityDescriptor.PrimaryGroup := JwNullSID;

  PrivObj[4,1].fSecurityDescriptor.DACL.Clear;
  {PrivObj[4,1].fSecurityDescriptor.DACL.Add(
    TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],
      JwAllSecurityAccess,
      JwSecurityProcessUserSID, false));  }



  SD := TJwSecurityDescriptor.CreateDefaultByToken(nil);
  SD.Owner := nil;
  SD.DACL.Clear;
  SD.DACL.Add(
    TJwDiscretionaryAccessControlEntryAllow.Create(nil,TJwAfThisFolderAndSubFoldersAndFiles,
      JwAllSecurityAccess,
      JwAdministratorsSID, false));

  if Assigned(PrivObj[4,1].fSecurityDescriptor) then
    oldS := PrivObj[4,1].fSecurityDescriptor.Text
  else
    oldS := '(nil)';

  try
    TJwSecurePrivateObject.SetSecurityDescriptor(PrivObj[4,1],
        [siDaclSecurityInformation],SD);
  finally
    ShowMessage(PrivObj[4,1].Name+#13#10+'Old:'#13#10+oldS+
                #13#10#13#10+'New:'#13#10+PrivObj[4,1].fSecurityDescriptor.Text);

    FreeAndNil(SD);
  end;

  SD := TJwSecurePrivateObject.GetSecurityDescriptor(
      PrivObj[4,1], [siDaclSecurityInformation, siOwnerSecurityInformation]);
    try
      ShowMessage(PrivObj[4,1].Name+#13#10+SD.Text);
      CheckNotNull(SD);
      CheckNotNull(SD.DACL);
      CheckNotNull(SD.Owner);
      CheckEquals(2, SD.DACL.Count);
    finally
      SD.Free;
    end;

end;

procedure TJwSecurePrivateObjectTests.TestRemoveInheritanceFlow;
begin
  CheckTrue(false, 'No implemented');
end;

procedure TJwSecurePrivateObjectTests.TestRestoreInheritanceFlow;
begin
  CheckTrue(false, 'No implemented');
end;



procedure TJwSecurePrivateObjectTests.TestTakeOwnerShip;
begin
  CheckTrue(false, 'No implemented');
end;

procedure TJwSecurePrivateObjectTests.TestTreeFileObjectSetNamedSecurityInfo;
begin
  CheckTrue(false, 'No implemented');
end;


{ TSecurePrivateObject }

procedure TSecurePrivateObject.Add(Child: TSecurePrivateObject);
begin
  if Assigned(Child.Parent) then
    raise EInvalidInsert.Create('TSecurePrivateObject.Add cannot be called with a child that has already parents.');

  Self.fIsDirectoryObject := true;

  SetLength(fChildren, Length(fChildren)+1);

  Child.fHasParent := true;
  Child.Parent := Self;
  fChildren[high(fChildren)] := Child;
end;

constructor TSecurePrivateObject.Create(const Name : TJwString;
  const UseDefault : boolean;const DefaultSecurityDescriptor: TJwSecurityDescriptor);
begin
  if UseDefault then
    inherited Create(DefaultSecurityDescriptor)
  else
    inherited CreateInherited;

  FillChar(fInfo,sizeof(fInfo),0);
  fInfo.Level := -1;
  fInfo.ObjectName := Name;
  fHasParent := false;
  fGenericMap := TJwSecurityGenericMapping;
  fUseAccessCheck := true;
  fIsDirectoryObject := false;
  fInheritFlags := [{ifAvoidOwnerCheck,ifAvoidPrivilegeCheck,}ifDaclAutoInherit];
  Parent := nil;
  fChildren := nil;
end;

constructor TSecurePrivateObject.CreateParented(const Name : TJwString;
  Parent : TSecurePrivateObject;
  const UseDefault : Boolean;
  const DefaultSecurityDescriptor: TJwSecurityDescriptor);
begin
  Create(Name, UseDefault, DefaultSecurityDescriptor);
  fHasParent := Assigned(Parent);
  Parent := Parent;
end;

destructor TSecurePrivateObject.Destroy;
begin
  Parent := nil;
  fChildren := nil;
  inherited;
end;

procedure TSecurePrivateObject.Done;
begin
  Parent := nil;
  fChildren := nil;
end;

function TSecurePrivateObject.GetChildren(
  out Children: TJwPrivateSecurityInformationArray): HRESULT;
begin
  if Assigned(fChildren) then
    Children := fChildren
  else
    result := E_NOTIMPL;
end;

function TSecurePrivateObject.GetInheritFlags(const FlagsType : TJwGetInheritFlagsType): TJwInheritFlagSet;
begin
  result := fInheritFlags;
  if FlagsType = giftCreatePrivate then
    Include(result,ifAvoidOwnerCheck);
end;

function TSecurePrivateObject.GetIsDirectoryObject: Boolean;
begin
  result := fIsDirectoryObject;
end;

function TSecurePrivateObject.GetName: TJwString;
begin
  result := fInfo.ObjectName;
end;

function TSecurePrivateObject.GetObjectInformation(
  const ObjectInformationSet: TJwSecurityObjectInformationFlagSet): TJwSecurityObjectInformation;
begin
  result := fInfo;
end;

function TSecurePrivateObject.GetParent(
  out Parent: IJwPrivateSecurityInformation): HRESULT;
begin
  if fHasParent then
  begin
    Parent := fPrivateSecurityInformation;
    result := S_OK;
  end
  else
  begin
    Parent := nil;
    result := E_NOTIMPL;
  end;
end;

function TSecurePrivateObject.GetParentProp: IJwPrivateSecurityInformation;
begin
 result := fPrivateSecurityInformation;
end;

function TSecurePrivateObject.GetUseAccessCheck(
  const AccessCheckType: TJwGetAccessCheckType): Boolean;
begin
  result := fUseAccessCheck;
end;

function TSecurePrivateObject.MapGenericMask(
  out GenericMap: TJwSecurityGenericMappingClass): HRESULT;
begin
  GenericMap := fGenericMap;
  result := S_OK;
end;



procedure TSecurePrivateObject.SetName(const Name: TJwString);
begin

end;

procedure TSecurePrivateObject.SetParentProp(
  const NewParent: IJwPrivateSecurityInformation);
begin
{  if Assigned(fPrivateSecurityInformation) then
    fPrivateSecurityInformation._Release;
  if Assigned(NewParent) then
    NewParent._AddRef;}
  fPrivateSecurityInformation := NewParent;
end;

initialization
  TestFramework.RegisterTest('JwsclSecurePrivateObjectsTests Suite',
    TJwSecurePrivateObjectTests.Suite);

end.
