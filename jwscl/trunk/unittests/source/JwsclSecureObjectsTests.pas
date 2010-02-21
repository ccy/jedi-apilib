unit JwsclSecureObjectsTests;

interface

uses
  Forms,
  Classes, SysUtils, Windows, Registry,


  jwaWindows,

  UMessageForm,

  JwsclMapping,
  JwsclTypes, JwsclExceptions, JwsclSid,JwsclAcl,
  JwsclVersion, JwsclConstants, JwsclProcess, JwsclToken,
  JwsclSecureObjects, JwsclDescriptor, JwsclKnownSid,
//  USecurityTreeResetForm,

  JwsclStrings,
  TestFrameWork;

const SECURE_OBJECT_KEY = 'SOFTWARE\SecurityLibrary\SecureObjectTest\';

type
  TSecureGeneralObjectTests = class(TTestCase)
  private
    fSecureGeneralObjects : array[1..10] of TJwSecureGeneralObject;
  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestConvertSecurityInformation1;
    procedure TestConvertSecurityInformation;
    procedure TestSetSecurityInfo;
    procedure TestSetNamedSecurityInfo;
    procedure TestGetSecurityInfo1;
    procedure TestGetSecurityInfo;
    procedure TestGetNamedSecurityInfo1;
    procedure TestGetNamedSecurityInfo;

    procedure TestGetOwnerShipMembers;

  end;

type
  TSecureFileObjectTests = class(TTestCase)
  private
    fSecureFileObjects : array[1..10] of TJwSecureFileObject;
    fFileNames : array[1..10] of String;
  protected

    procedure SetUp; override;
    procedure TearDown; override;

    function GetSecurityFileNameTemp(out HasSACL : Boolean) : String;
    procedure FNProgressMethod(const pObjectName : TJwString;              // Name of object just processed
                                const cStatus : Cardinal;                    // Status of operation on object
                                var pInvokeSetting : TJwProgInvokeSetting; // When to set
                                const E : EJwsclSecurityException;         //Exception type that was raised or nil if none - only supported by TJwSecureFileObject.TreeFileObjectSetNamedSecurityInfo
                                const Args : Pointer;                      // Caller specific data
                                const bSecuritySet : Boolean                 // Whether security was set
                                );

    procedure FNProgressMethod2(const pObjectName : TJwString;              // Name of object just processed
                                const cStatus : Cardinal;                    // Status of operation on object
                                var pInvokeSetting : TJwProgInvokeSetting; // When to set
                                const E : EJwsclSecurityException;         //Exception type that was raised or nil if none - only supported by TJwSecureFileObject.TreeFileObjectSetNamedSecurityInfo
                                const Args : Pointer;                      // Caller specific data
                                const bSecuritySet : Boolean                 // Whether security was set
                                );



  published

    // Test methods
//    procedure TestDummy;
    procedure Test_SetFileSecurity;

      procedure Test_Create1;
      procedure Test_Create2;
      procedure Test_Create3;

      procedure Test_Destroy;

      procedure Test_GetDACL;
      procedure Test_XetSACL;

      procedure Test_GetOwner;
      procedure Test_GetGroup;


      procedure Test_SetDACL;

      procedure Test_SetOwner;
      procedure Test_SetGroup;

      procedure Test_GetTempOwner;
      procedure Test_GetTempGroup;

      procedure Test_GetTempDACL;
      procedure Test_GetTempSACL;

      procedure Test_AccessCheck1;

      procedure Test_XetSecurityDescriptor;

      procedure Test_SupportACL;
      procedure Test_GetInheritanceSource;

      procedure Test_GetFileInheritanceSource;
      procedure Test_TreeResetNamedSecurityInfo;

      procedure Test_Inheritance;
      procedure Test_TreeFileObjectSetNamedSecurityInfo;
  end;

type
  TSecureRegistryKeyTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;
     procedure FNProgressMethod(const pObjectName : TJwString;              // Name of object just processed
                                const cStatus : Cardinal;                    // Status of operation on object
                                var pInvokeSetting : TJwProgInvokeSetting; // When to set
                                const E : EJwsclSecurityException;         //Exception type that was raised or nil if none - only supported by TJwSecureFileObject.TreeFileObjectSetNamedSecurityInfo
                                const Args : Pointer;                      // Caller specific data
                                const bSecuritySet : Boolean                 // Whether security was set
                                );
  published
    procedure Test_Create;
    procedure GetKeyInheritanceSource(const KeyName: TJwString);
    procedure Test_TreeKeyObjectSetNamedSecurityInfo;
    procedure Test_SecurityTreeResetForm;
    // Test methods
//    procedure TestDummy;

  end;

type
  TSecureWindowObjectTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods

  end;

type
  TSecurePrinterTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods


  end;

type
  TSecureServiceTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods

  end;

type
  TSecureLMShareTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods


  end;

type
  TSecureMutexTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods

  end;

type
  TSecureSemaphoreTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods

  end;

type
  TSecureEventTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods

  end;

type
  TSecureWaitableTimerTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods

  end;

type
  TSecureFileMappingTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods

  end;

type
  TSecureProcessTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods

  end;

type
  TSecureThreadTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods

  end;

type
  TSecureWindowStationObjectTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods

  end;

type
  TSecureDesktopObjectTests = class(TTestCase)
  private

  protected

//    procedure SetUp; override;
//    procedure TearDown; override;

  published

    // Test methods

  end;

implementation

{ TSecureGeneralObjectTests }

procedure TSecureGeneralObjectTests.SetUp;
var i : Integer;
begin
  inherited;
  for i := low(fSecureGeneralObjects) to high(fSecureGeneralObjects) do
    fSecureGeneralObjects[i] := nil;
end;

procedure TSecureGeneralObjectTests.TearDown;
var i : Integer;
begin
  inherited;
  for i := low(fSecureGeneralObjects) to high(fSecureGeneralObjects) do
    fSecureGeneralObjects[i].Free;
end;

procedure ThreadRoutine; stdcall;
begin
end;

procedure TSecureGeneralObjectTests.TestConvertSecurityInformation;
var anOwner : TJwSecurityId;
    anGroup : TJwSecurityId;
    aDACL : TJwDAccessControlList;
    aSACL : TJwSAccessControlList;



    hMutex : Cardinal;
    threadID : Cardinal;

    Desc : JwsclDescriptor.TJwSecurityDescriptor;

    pSA : jwaWindows.PSecurityAttributes;
    pSA2 : LPSECURITY_ATTRIBUTES;


begin
  anOwner := nil;
  anGroup := nil;
  aDACL := nil;
  aSACL := nil;

  fSecureGeneralObjects[1] := TJwSecureGeneralObject.Create;

  Desc := TJwSecurityDescriptor.Create;
  Desc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,TJwSecurityId.Create('S-1-5-18'),true));
  Desc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,TJwSecurityId.Create('S-1-10-5'),true));
  Desc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwSecurityProcessUserSID,false));

  pSA := jwaWindows.PSecurityAttributes(Desc.Create_SA(false,true));

  pSA2 := LPSECURITY_ATTRIBUTES(Pointer(pSA));


  hMutex := CreateMutex(pSA2,true,'123');

  try
    if (hMutex = INVALID_HANDLE_VALUE) then
      raise EJwsclSecurityException.CreateFmtEx('Invalid hThread','Free_SA',ClassName,'',0,true,[]);


    try
      fSecureGeneralObjects[1].GetSecurityInfo(hMutex, SE_KERNEL_OBJECT,
                          [siDaclSecurityInformation, siOwnerSecurityInformation,
                           siGroupSecurityInformation],
                          anOwner, anGroup, aDACL, aSACL);
    except
      on E : EJwsclWinCallFailedException do
      begin

        raise;
      end;
    end;

    if Assigned(aDACL) and Assigned(anOwner) then
      ShowMessageForm(aDACL.Text+#13#10+anOwner.GetText());

    CloseHandle(hMutex);

  finally

    Desc.Free_SA(jwaWindows.PSecurityAttributes(pSA));

    anOwner.Free;
    anGroup.Free;
    aDACL.Free;
    aSACL.Free;
    Desc.Free;

  end;
end;

procedure TSecureGeneralObjectTests.TestConvertSecurityInformation1;
begin

end;

procedure TSecureGeneralObjectTests.TestGetNamedSecurityInfo;
begin

end;

procedure TSecureGeneralObjectTests.TestGetNamedSecurityInfo1;
begin

end;

procedure TSecureGeneralObjectTests.TestGetOwnerShipMembers;
var sids  : TJwSecurityIdList;
begin
  sids := TJwSecureGeneralObject.GetOwnerShipMembers;
  try
    ShowMessageForm(Sids.GetText(true));
  finally
   FreeAndNil(sids);
  end;

end;

procedure TSecureGeneralObjectTests.TestGetSecurityInfo;
begin

end;

procedure TSecureGeneralObjectTests.TestGetSecurityInfo1;
begin

end;

procedure TSecureGeneralObjectTests.TestSetNamedSecurityInfo;
begin

end;

procedure TSecureGeneralObjectTests.TestSetSecurityInfo;
begin

end;

{ TSecureFileObjectTests }


procedure TSecureFileObjectTests.SetUp;
var i : Integer;
begin
  inherited;

  for i := low(fSecureFileObjects) to high(fSecureFileObjects) do
    fSecureFileObjects[i] := nil;


  for i := low(fFileNames) to high(fFileNames) do
    fFileNames[i] := '';
end;

procedure TSecureFileObjectTests.TearDown;
var i : Integer;
begin
  inherited;

  for i := low(fSecureFileObjects) to high(fSecureFileObjects) do
    fSecureFileObjects[i].Free;

  for i := low(fFileNames) to high(fFileNames) do
  begin
    try
      DeleteFile(PChar(fFileNames[i]));
    except
    end;
  end;
end;

procedure TSecureFileObjectTests.Test_Create1; //(const F : TFileStream)
var F : TFileStream;
begin
  F := TFileStream.Create(ParamStr(0),fmOpenRead or fmShareDenyNone);

  try
    fSecureFileObjects[1] := TJwSecureFileObject.Create(F);


    CheckNotEquals(0, fSecureFileObjects[1].Handle);

    FreeAndNil(fSecureFileObjects[1]);
  finally
    FreeAndNil(F);
  end;
end;

procedure TSecureFileObjectTests.Test_Create2; //(const FileHandle : THandle; bDuplicateHandle : Boolean = false);
begin
  //tested indirect by Create1
end;

procedure TSecureFileObjectTests.Test_Create3;//(const FileName : TJwString);
begin
  fSecureFileObjects[1] := TJwSecureFileObject.Create(ParamStr(0));

  CheckEquals(0, fSecureFileObjects[1].Handle);

  FreeAndNil(fSecureFileObjects[1]);
end;

procedure TSecureFileObjectTests.Test_Destroy;
begin
  //see Text_CreateX
end;

procedure TSecureFileObjectTests.Test_GetDACL;
var DACL_1, DACL_2 : TJwDAccessControlList;
    F : TFileStream;
begin
  fSecureFileObjects[1] := TJwSecureFileObject.Create(ParamStr(0));

  F := nil;
  DACL_1 := nil;
  DACL_2 := nil;

  try
    F := TFileStream.Create(ParamStr(0),fmOpenRead or fmShareDenyNone);
    fSecureFileObjects[2] := TJwSecureFileObject.Create(F);

    DACL_1 := fSecureFileObjects[1].GetDACL;
    DACL_2 := fSecureFileObjects[2].GetDACL;

    (*
    do not check for nil
    because it is possible that the DACL is not set
    a NULL DACL grants access to everyone.
    *)
  finally
    FreeAndNil(F);
    FreeAndNil(DACL_1);
    FreeAndNil(DACL_2);
    FreeAndNil(fSecureFileObjects[2]);
    FreeAndNil(fSecureFileObjects[1]);
  end;
end;

procedure TSecureFileObjectTests.Test_GetGroup;
var SID_1, SID_2 : TJwSecurityId;
    F : TFileStream;
begin
  fSecureFileObjects[1] := TJwSecureFileObject.Create(ParamStr(0));

  F := nil;
  SID_1 := nil;
  SID_2 := nil;

  try
    F := TFileStream.Create(ParamStr(0),fmOpenRead or fmShareDenyNone);
    fSecureFileObjects[2] := TJwSecureFileObject.Create(F);

    SID_1 := fSecureFileObjects[1].GetGroup;
    SID_2 := fSecureFileObjects[2].GetGroup;

  finally
    FreeAndNil(F);
    FreeAndNil(SID_1);
    FreeAndNil(SID_2);
    FreeAndNil(fSecureFileObjects[2]);
    FreeAndNil(fSecureFileObjects[1]);
  end;
end;

procedure TSecureFileObjectTests.Test_GetOwner;
var SID_1, SID_2 : TJwSecurityId;
    F : TFileStream;
begin
  fSecureFileObjects[1] := TJwSecureFileObject.Create(ParamStr(0));

  F := nil;
  SID_1 := nil;
  SID_2 := nil;

  try
    F := TFileStream.Create(ParamStr(0),fmOpenRead or fmShareDenyNone);
    fSecureFileObjects[2] := TJwSecureFileObject.Create(F);

    SID_1 := fSecureFileObjects[1].GetOwner;
    SID_2 := fSecureFileObjects[2].GetOwner;

  finally
    FreeAndNil(F);
    FreeAndNil(SID_1);
    FreeAndNil(SID_2);
    FreeAndNil(fSecureFileObjects[2]);
    FreeAndNil(fSecureFileObjects[1]);
  end;
end;

procedure TSecureFileObjectTests.Test_XetSACL;
var SACL_1, SACL_2 : TJwSAccessControlList;
    DACL : TJwDAccessControlList;
    g,o : TJwSecurityId;
    F : TFileStream;
    FileName : String;
    HasSACL : Boolean;
    i : Integer;
begin
  //do not call this with admin priv. because the process user is set as owner
  fFileNames[1] := GetSecurityFileNameTemp(HasSACL);

  FileName := ParamStr(0);

  CheckTrue(JwIsPrivilegeSet(SE_SECURITY_NAME,pqt_Available),'The privilege '+SE_SECURITY_NAME+' must be set to run this test');

 // SE_AUDIT_NAME is only useful if one want to create audit events
 //// JwEnablePrivilege(SE_AUDIT_NAME, pst_Enable);

  //enable read and write of SACL
  JwEnablePrivilege(SE_SECURITY_NAME, pst_Enable);

  //ShowMessageForm(JwGetPrivilegesText);


  CheckTrue(JwIsPrivilegeSet(SE_SECURITY_NAME),'The privilege '+SE_SECURITY_NAME+' could not be set. (should never happen!)');



  fSecureFileObjects[1] := TJwSecureFileObject.Create(fFileNames[1]);//ParamStr(0));



  F := nil;
  SACL_1 := nil;
  SACL_2 := nil;

  try
    i := ACCESS_SYSTEM_SECURITY;
    F := TFileStream.Create(fFileNames[1],fmOpenReadWrite {or fmShareExclusive});

    //TJwSecureGeneralObject.TakeOwnerShip(F.Handle,SE_KERNEL_OBJECT);
    //TJwSecureGeneralObject.GetSecurityInfo(F.Handle,SE_KERNEL_OBJECT,[siDaclSecurityInformation],g,o,DACL,SACL_1);
    //DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwWorldSID,false));
    //TJwSecureGeneralObject.SetSecurityInfo(F.Handle,SE_KERNEL_OBJECT,[siDaclSecurityInformation],nil,nil,DACL,nil);

    fSecureFileObjects[2] := TJwSecureFileObject.Create(F, 0);//FILE_READ_DATA or ACCESS_SYSTEM_SECURITY);

    SACL_1 := fSecureFileObjects[1].GetSACL;
    try
      SACL_2 := fSecureFileObjects[2].GetSACL; //TFileStream does not support SACL reading!  ACCESS DENIED
      CheckNotNull(nil,'TFileStream does not support SACL reading!  ACCESS DENIED');
    except
      on E : Exception do
        CheckIs(E, EJwsclSecurityException,'TFileStream does not support SACL reading!  ACCESS DENIED');
    end;

    (*
    do not check for nil
    because it is possible that the SACL is not set
    *)
  finally
    FreeAndNil(fSecureFileObjects[1]);

    FreeAndNil(F);
    FreeAndNil(SACL_1);
    FreeAndNil(SACL_2);
    FreeAndNil(fSecureFileObjects[2]);

    DeleteFile(PChar(fFileNames[1]));
  end;

end;

(*
  if not JwIsPrivilegeSet(SE_SECURITY_NAME) then
    CheckTrue(false,'SACL Test could not be run because the user has not the privilege '+SE_SECURITY_NAME);

  fFileNames[1] := GetSecurityFileNameTemp(HasSACL);

  fSecureFileObjects[1] := TJwSecureFileObject.Create(fFileNames[1]);

  JwEnablePrivilege(SE_SECURITY_NAME, pst_Enable);
  try

  finally
    JwEnablePrivilege(SE_SECURITY_NAME, pst_disable);
  end;
*)

procedure TSecureFileObjectTests.Test_SetDACL;
var F : TFileStream;
    pTempName : Array[0..MAX_PATH+1] of char;
    fSecureFileObjects : TJwSecureFileObject;

    aDACL1, aDACL2 : TJwDAccessControlList;
    hFile : THandle;

    bI : Boolean;
begin
  CheckNotEquals(0,
        GetTempFileName('.', // LPCTSTR lpPathName,
                        'SMTest', //   LPCTSTR lpPrefixString,
                        0, // UINT uUnique,
                        @pTempName //  LPTSTR lpTempFileName
                        ));


  fSecureFileObjects := TJwSecureFileObject.Create(String(pTempName));
  //fSecureFileObjects := TJwSecureFileObject.Create(String('testdat.tmp'));

  //remove old DACL
  fSecureFileObjects.SetDACL(nil);

  //get old DACL = NULL DACL
  aDACL1 := fSecureFileObjects.GetDACL;
  if not Assigned(aDACL1) then
    aDACL1 := TJwDAccessControlList.Create(true);

  aDACL1.Clear;

  //add process user
  aDACL1.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],
        (*
        do not use GENERIC_ALL, because some flags are discareded and
        so the written and read access mask would differ.
        *)
                FILE_ALL_ACCESS,
                JwSecurityCurrentThreadUserSID,true));//we are responsible to free

  fSecureFileObjects.SetDACL(aDACL1);


  aDACL2 := fSecureFileObjects.GetDACL;
  CheckNotNull(aDACL2);

  CheckEquals(aDACL1.Count, aDACL2.Count);

  CheckTRUE(aDACL1.Items[0].SID.EqualSid(aDACL2.Items[0].SID));
  CheckTRUE(aDACL1.Items[0].Flags = aDACL2.Items[0].Flags);
  CheckTRUE(aDACL1.Items[0].AccessMask = aDACL2.Items[0].AccessMask);

  aDACL1.Free;
  aDACL2.Free;
  fSecureFileObjects.Free;

  //*********************
  // set DACL by Handle
  //**********************
  hFile := CreateFile(
                      pTempName,                //LPCTSTR lpFileName,	// pointer to name of the file
                      GENERIC_ALL,              //DWORD dwDesiredAccess,	// access (read-write) mode
                      0,                        //DWORD dwShareMode,	// share mode
                      nil,                      //LPSECURITY_ATTRIBUTES lpSecurityAttributes,	// pointer to security attributes
                      OPEN_ALWAYS,              //DWORD dwCreationDistribution,	// how to create
                      FILE_ATTRIBUTE_NORMAL,    //DWORD dwFlagsAndAttributes,	// file attributes
                      0  //HANDLE hTemplateFile // handle to file with attributes to copy
                       );
  CheckNotEquals(INVALID_HANDLE_VALUE, hFile, 0, IntToStr(GetLastError()));
  CheckNotEquals(0, hFile);

  try
    for bI := false to true do
    begin
      fSecureFileObjects := TJwSecureFileObject.Create(hFile,0,bI);

      //remove old DACL
      fSecureFileObjects.SetDACL(nil);

      //get old DACL = NULL DACL
      aDACL1 := fSecureFileObjects.GetDACL;
      if not Assigned(aDACL1) then
        aDACL1 := TJwDAccessControlList.Create(true);

      aDACL1.Clear;


      //add process user
      aDACL1.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[], FILE_ALL_ACCESS , JwSecurityCurrentThreadUserSID,true));//we are responsible to free

      fSecureFileObjects.SetDACL(aDACL1);


      aDACL2 := fSecureFileObjects.GetDACL;
      CheckNotNull(aDACL2);

      CheckEquals(aDACL1.Count, aDACL2.Count);

      CheckTRUE(aDACL1.Items[0].SID.EqualSid(aDACL2.Items[0].SID));
      CheckTRUE(aDACL1.Items[0].Flags = aDACL2.Items[0].Flags);
      CheckTRUE(aDACL1.Items[0].AccessMask = aDACL2.Items[0].AccessMask);

      aDACL1.Free;
      aDACL2.Free;
      fSecureFileObjects.Free;
    end;
  finally
    CloseHandle(hFile);
  end;

end;

procedure TSecureFileObjectTests.Test_GetTempDACL;
var F : TFileStream;
    pTempName : Array[0..MAX_PATH+1] of char;
    fSecureFileObjects : TJwSecureFileObject;

    aDACL1, aDACL2 : TJwDAccessControlList;
    pDACL : ^TJwDAccessControlList;
    hFile : THandle;

    bI : Boolean;
begin
  CheckNotEquals(0,
        GetTempFileName('.', // LPCTSTR lpPathName,
                        'SMTest', //   LPCTSTR lpPrefixString,
                        0, // UINT uUnique,
                        @pTempName //  LPTSTR lpTempFileName
                        ));

  fSecureFileObjects := TJwSecureFileObject.Create(String(pTempName));

  try

    aDACL1 := fSecureFileObjects.GetTempDACL;
     aDACL2 := fSecureFileObjects.GetTempDACL;
    pDACL := @aDACL1;
    CheckSame(aDACL1,aDACL2);

    fSecureFileObjects.ResetTemp([treDacl]);

     aDACL2 := fSecureFileObjects.GetTempDACL;
    CheckFALSE(pDACL = @aDACL2);
  finally
    fSecureFileObjects.Free;
  end;

end;

procedure TSecureFileObjectTests.Test_GetTempGroup;
var F : TFileStream;
    pTempName : Array[0..MAX_PATH+1] of char;
    fSecureFileObjects : TJwSecureFileObject;

    v1,v2 : TJwSecurityId;
    pV : ^TJwSecurityId;
    hFile : THandle;

    bI : Boolean;
begin
  CheckNotEquals(0,
        GetTempFileName('.', // LPCTSTR lpPathName,
                        'SMTest', //   LPCTSTR lpPrefixString,
                        0, // UINT uUnique,
                        @pTempName //  LPTSTR lpTempFileName
                        ));

  fSecureFileObjects := TJwSecureFileObject.Create(String(pTempName));

  try
    v1 := fSecureFileObjects.GetTempGroup;
     v2 := fSecureFileObjects.GetTempGroup;
    pV := @v1;
    CheckSame(v1,v2);

    fSecureFileObjects.ResetTemp([treGroup]);

     v2 := fSecureFileObjects.GetTempGroup;
    CheckFALSE(v1 = @v2);
  finally
    fSecureFileObjects.Free;
  end;


end;

procedure TSecureFileObjectTests.Test_GetTempOwner;
begin

end;

procedure TSecureFileObjectTests.Test_GetTempSACL;
begin

end;



{
How to change the owner of a file?

To change the owner of a file you need some preparations.
1. The owner must grant your account the right to change the security descriptions (WRITE_DAC and WRITE_OWNER)
So change the DACL to get there.
2.
}
procedure TSecureFileObjectTests.Test_SetFileSecurity;
var aSID,
    anOwner : TJwSecurityId;
    anGroup : TJwSecurityId;
    aDACL : TJwDAccessControlList;
    aSACL : TJwSAccessControlList;

    adminToken : TJwSecurityToken;
    adminUser : TJwSecurityId;

    fSecureFileObjects : TJwSecureFileObject;
    privs : TJwPrivilegeSet;
    i : Integer;
    pTempName : Array[0..MAX_PATH+1] of char;

begin
  anOwner := nil;
  anGroup := nil;
  aDACL := nil;
  aSACL := nil;

  CheckTrue(JwIsPrivilegeSet(SE_SECURITY_NAME,pqt_Available),'The privilege '+SE_SECURITY_NAME+' must be set to run this test');



  FillChar(pTempName, sizeof(pTempName), 0);
  CheckNotEquals(0,
        GetTempFileName('.', // LPCTSTR lpPathName,
                        'SMLTest', //   LPCTSTR lpPrefixString,
                        0, // UINT uUnique,
                        @pTempName //  LPTSTR lpTempFileName
                        ));

  fSecureFileObjects := TJwSecureFileObject.Create(pTempName);

  JwEnablePrivilege(SE_SECURITY_NAME, pst_Enable);

  try
    aDACL := fSecureFileObjects.GetTempDACL;
    anOwner := fSecureFileObjects.GetTempOwner;
  finally
    //except: do error checking here
  end;

  adminUser := JwSecurityCurrentThreadUserSID;
  try
    fSecureFileObjects.SetOwner(adminUser);
  finally
    //except: do error checking here

  end;

  //aDACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwAdministratorsSID,false)); //add admingroup sid, but do not own it (false)
  //aDACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwLocalSystemSID,false));
  //aDACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_READ,JwWorldSID,false));

  //grant the process owner the right to change DACL and OWNER
  //to change the owner we need also the right WRITE_DAC!!
  //aDACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[], WRITE_DAC or WRITE_OWNER ,JwSecurityProcessUserSID,false));
  aDACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[], FILE_ALL_ACCESS ,adminUser,true)); //we are responsible to free


  //set new dacl
  try
    fSecureFileObjects.SetDACL(aDACL);

  finally
    //except: do error checking here
  end;



  {Make sure that aDACL is nil, because SetNamedSecurityInfo automatically
   uses it if it is not nil.
  }
  aDACL := nil;

  //set new owner to the process owner
  anOwner := JwSecurityCurrentThreadUserSID;
  try
    fSecureFileObjects.SetOwner(anOwner);
  finally
    //except: do error checking here
    anOwner.Free//we are responsible to free
  end;

  fSecureFileObjects.Free;

end;

procedure TSecureFileObjectTests.Test_SetGroup;
begin
  CheckTrue(false,'Test not implemented');
end;

procedure TSecureFileObjectTests.Test_SetOwner;
begin
  CheckTrue(false,'Test not implemented');
end;




procedure TSecureFileObjectTests.Test_XetSecurityDescriptor;
var HasSACL: Boolean;
    SD : TJwSecurityDescriptor;
    x : TJwSecurityInformationFlagSet;
    threadUser : TJwSecurityId;
begin
  fFileNames[1] := GetSecurityFileNameTemp(HasSACL);

  fSecureFileObjects[1] := TJwSecureFileObject.Create(fFileNames[1]);

  x := [siOwnerSecurityInformation, siDaclSecurityInformation];
  if HasSACL then
    Include(x, siSaclSecurityInformation);


  try
    fSecureFileObjects[1].SetSecurityDescriptor(nil,[]);
    CheckIs(nil,EJwsclNILParameterException);
  except
    on E : Exception do
      CheckIs(E,EJwsclNILParameterException);
  end;

  if JwIsPrivilegeSet(SE_SECURITY_NAME) then
  begin
    JwEnablePrivilege(SE_SECURITY_NAME, pst_Enable);
  end;

  try
    SD := fSecureFileObjects[1].GetSecurityDescriptor(x);



    threadUser := JwSecurityCurrentThreadUserSID;
    try
      CheckTRUE(SD.Owner.EqualSid(threadUser));
      CheckNotNull(SD.DACL);
      CheckEquals(2,SD.DACL.Count);
      CheckNotEquals(-1,SD.DACL.FindSID(JwGuestsSID));
      CheckNotEquals(-1,SD.DACL.FindSID(threadUser));
      SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[afObjectInheritAce],FILE_READ_ACCESS,JwAdministratorsSID,false));
    finally
      threadUser.Free;
    end;

    fSecureFileObjects[1].SetSecurityDescriptor(SD,[siDaclSecurityInformation]);

    SD.Free;

    SD := fSecureFileObjects[1].GetSecurityDescriptor(x);

    threadUser := JwSecurityCurrentThreadUserSID;
    try
      CheckTRUE(SD.Owner.EqualSid(threadUser));
      CheckNotNull(SD.DACL);
      CheckEquals(3,SD.DACL.Count);
      CheckNotEquals(-1,SD.DACL.FindSID(JwGuestsSID));
      CheckNotEquals(-1,SD.DACL.FindSID(threadUser));
      CheckNotEquals(-1,SD.DACL.FindSID(JwAdministratorsSID));

     finally
      threadUser.Free;
    end;

    SD.Free;
  finally
    FreeAndNil(fSecureFileObjects[1]);

    JwEnablePrivilege(SE_SECURITY_NAME, pst_Disable);

  end;

  DeleteFile(PChar(fFileNames[1]));
end;

procedure TSecureFileObjectTests.Test_AccessCheck1;
var SD : TJwSecurityDescriptor;
    privs : TJwPrivilegeSet;
    AccessMask : TJwAccessMask;
    AccessGranted : Boolean;

    oldToken,
    clientToken : TJwSecurityToken;
    Index : Integer;
    threadUser : TJwSecurityId;
begin
  JwInitWellKnownSIDs;

  threadUser := JwSecurityCurrentThreadUserSID;

try


  oldToken := nil;
  if TJwSecurityToken.HasThreadAToken() then
  begin
    oldToken := TJwSecurityToken.GetThreadToken(TOKEN_ALL_ACCESS, true);
    TJwSecurityToken.RemoveThreadToken(0);
  end;


  try
    SD := nil;
    try
      TJwSecureGeneralObject.AccessCheck(nil, nil, FILE_READ_ACCESS ,TJwSecurityGenericMapping, privs, AccessMask, AccessGranted);
      CheckIs( nil, EJwsclInvalidParameterException);
    except
      on E : Exception do
        CheckIs(E, EJwsclInvalidParameterException);
    end;

    SD := TJwSecurityDescriptor.Create;

    try
      TJwSecureGeneralObject.AccessCheck(SD, nil, FILE_READ_ACCESS ,TJwSecurityFileMapping, privs, AccessMask, AccessGranted);
      CheckIs(nil, EJwsclInvalidGroupSIDException);
    except
      on E : Exception do
        CheckIs(E, EJwsclInvalidGroupSIDException);
    end;

    SD.PrimaryGroup := JwNullSID;
    SD.OwnPrimaryGroup := false;

    try
      TJwSecureGeneralObject.AccessCheck(SD, nil, FILE_READ_ACCESS ,TJwSecurityFileMapping, privs, AccessMask, AccessGranted);
      CheckIs(nil, EJwsclInvalidOwnerSIDException);
    except
      on E : Exception do
        CheckIs(E, EJwsclInvalidOwnerSIDException);
    end;
    SD.Owner := JwNullSID;
    SD.OwnOwner := false;

    try
      TJwSecureGeneralObject.AccessCheck(SD, nil, FILE_READ_ACCESS ,TJwSecurityFileMapping, privs, AccessMask, AccessGranted);
    except
      on E : Exception do
        CheckIs(E, EJwsclWinCallFailedException,'AccessCheck should not raise an exception');
    end;

    SD.Free;



    SD := TJwSecurityDescriptor.Create;
    SD.OwnOwner := true;  //creates a copy of threadUser and frees it at the end
    SD.Owner := threadUser;
    //SD.Owner := JwAdministratorsSID;

    SD.PrimaryGroup := JwNullSID;
    SD.OwnPrimaryGroup := false;

    SD.DACL := nil;
    //Use the map method to map GENERIC_XXX access rights. Otherwise you need
    TJwSecureFileObject.AccessCheck(SD, nil, TJwSecurityFileMapping.Map(GENERIC_ALL) , privs, AccessMask, AccessGranted);
    CheckTrue(AccessGranted);
    CheckEquals(TJwSecurityFileMapping.Map(GENERIC_ALL), AccessMask);


    SD.OwnDACL := true;
    SD.DACL := TJwDAccessControlList.Create(true);
    SD.OwnDACL := false;
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],FILE_READ_DATA,JwSecurityCurrentThreadUserSID, true));
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],FILE_WRITE_ACCESS,JwSecurityCurrentThreadUserSID, true));
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],FILE_EXECUTE,JwSecurityCurrentThreadUserSID, true));

    TJwSecureFileObject.AccessCheck(SD, nil, TJwSecurityFileMapping.Map(GENERIC_ALL) , privs, AccessMask, AccessGranted);
    CheckFalse(AccessGranted);
    CheckEquals(0, AccessMask);

    TJwSecureFileObject.AccessCheck(SD, nil, FILE_READ_DATA , privs, AccessMask, AccessGranted);
    CheckTrue(AccessGranted);
    CheckEquals(FILE_READ_DATA, AccessMask);

    TJwSecureFileObject.AccessCheck(SD, nil, FILE_READ_DATA or FILE_WRITE_ACCESS, privs, AccessMask, AccessGranted);
    CheckTrue(AccessGranted);
    CheckEquals(FILE_READ_DATA or FILE_WRITE_ACCESS, AccessMask);


    //SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],FILE_READ_DATA,JwWorldSID, false));
    //SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],FILE_READ_ACCESS, JwWorldSID, false));
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],TJwSecurityFileMapping.Map(GENERIC_READ), JwWorldSID, false));

    TJwSecureFileObject.AccessCheck(SD, nil, FILE_READ_DATA or FILE_WRITE_ACCESS, privs, AccessMask, AccessGranted);
    CheckTrue(AccessGranted);
    CheckEquals(FILE_READ_DATA or FILE_WRITE_ACCESS, AccessMask);

    TJwSecureFileObject.AccessCheck(SD, nil, FILE_READ_DATA or FILE_EXECUTE, privs, AccessMask, AccessGranted);
    CheckFALSE(AccessGranted);
    CheckEquals(0, AccessMask);


    {Can we change owner?}
    TJwSecureGeneralObject.AccessCheck(SD, nil, WRITE_OWNER, TJwSecurityGenericMapping,privs, AccessMask, AccessGranted);
    CheckFALSE(AccessGranted);
    CheckEquals(0, AccessMask);


    Index := SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],WRITE_OWNER, JwSecurityCurrentThreadUserSID, true)); //Deny WRITE_OWNER

    TJwSecureGeneralObject.AccessCheck(SD, nil, WRITE_OWNER, TJwSecurityGenericMapping,privs, AccessMask, AccessGranted);
    CheckFalse(AccessGranted);
    CheckEquals(0, AccessMask);

    SD.DACL.Remove(Index); //remove Deny WRITE_OWNER
    Index := SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],WRITE_OWNER, JwSecurityCurrentThreadUserSID, true));

    TJwSecureGeneralObject.AccessCheck(SD, nil, WRITE_OWNER, TJwSecurityGenericMapping,privs, AccessMask, AccessGranted);
    CheckTRUE(AccessGranted);
    CheckEquals(WRITE_OWNER, AccessMask);


    SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],WRITE_DAC, threadUSer, false));
    {Can we change DAC?}


    //if Assigned(oldToken) then
    //  oldToken.SetThreadToken(0);

    {Because we removed the thread token - and AccessCheck uses the thread token, if any
    we use the oldToken as parameter.
    On app start the programm thread is elevated to the administrator (if possible) and this token is removed from thread
     in this test.
    Because the process and thread token can differ (user <> admin) we use the real token

    We could also set the thread token back.
    }
    TJwSecureGeneralObject.AccessCheck(SD, oldToken, WRITE_DAC, TJwSecurityGenericMapping, privs, AccessMask, AccessGranted);

    CheckTrue(AccessGranted);  //Although we denied WRITE_DAC, we can change it, because we are the owner!
    CheckEquals(WRITE_DAC, AccessMask);

    //TJwSecurityToken.RemoveThreadToken(0);

    SD.Owner := nil; //free previous owner before change OwnOwner !
    SD.OwnOwner := false;
    SD.Owner := JwAdministratorsSID;

    TJwSecureGeneralObject.AccessCheck(SD, nil, WRITE_DAC, TJwSecurityGenericMapping, privs, AccessMask, AccessGranted);
    CheckFALSE(AccessGranted);  //Now, the owner is Administrator group and we cannot change the DAC because of the deny entry!
    CheckEquals(0, AccessMask);



    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],FILE_WRITE_ACCESS,JwAdministratorsSID, false));
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],FILE_EXECUTE,JwWorldSID, false));

    clientToken := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);
    try
      TJwSecureGeneralObject.AccessCheck(SD, clientToken, TJwSecurityFileMapping.Map(GENERIC_EXECUTE) ,TJwSecurityFileMapping, privs, AccessMask, AccessGranted);
      CheckIs(nil, EJwsclWinCallFailedException);
    except
      on E : Exception do
        CheckIs(E, EJwsclWinCallFailedException);
    end;

    //use a thread token instead!!!
    clientToken.ConvertToImpersonatedToken(jwaWindows.TSecurityImpersonationLevel(SecurityImpersonation),TOKEN_ALL_ACCESS);

    TJwSecureFileObject.AccessCheck(SD, clientToken, TJwSecurityFileMapping.Map(GENERIC_EXECUTE) , privs, AccessMask, AccessGranted);
    CheckFALSE(AccessGranted);
    CheckEquals(0, AccessMask);

    clientToken.Free;


  finally
    if Assigned(oldToken) then
      oldToken.SetThreadToken(0);
    oldToken.Free;
    SD.Free;
  end;
finally
  threadUser.Free;
end;

end;

function TSecureFileObjectTests.GetSecurityFileNameTemp(
  out HasSACL: Boolean): String;

var
  pTempName : Array[0..MAX_PATH+1] of char;
  bI : Boolean;
  pSecAttr : jwaWindows.PSecurityAttributes;
  handle : THANDLE;
  SDesc : TJwSecurityDescriptor;
  pDACL : jwaWindows.PACL;
  b : Boolean;


begin
  FillChar(pTempName, sizeof(pTempName), 0);
  CheckNotEquals(0,
        GetTempFileName('.', // LPCTSTR lpPathName,
                        'SMLTest', //   LPCTSTR lpPrefixString,
                        0, // UINT uUnique,
                        @pTempName //  LPTSTR lpTempFileName
                        ));
  DeleteFile(pTempName);


  SDesc := TJwSecurityDescriptor.Create;

  try
    SDesc.OwnOwner := false; //first, we want to point the owner to internal data
    SDesc.Owner := JwSecurityCurrentThreadUserSID;
    SDesc.OwnOwner := true; //SD frees Owner

    SDesc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],FILE_ALL_ACCESS,JwSecurityCurrentThreadUserSID, true));
   // SDesc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],FILE_ALL_ACCESS,JwLocalSystemSID, false));
   // SDesc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[afNoPropagateInheritAce],FILE_ALL_ACCESS,JwWorldSID, false));
    SDesc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],FILE_READ_DATA,JwGuestsSID, false));


    if JwIsPrivilegeSet(SE_SECURITY_NAME) then
    begin
      JwEnablePrivilege(SE_SECURITY_NAME, pst_Enable);

      SDesc.SACL.Add(TJwAuditAccessControlEntry.Create(nil,true, false, FILE_ALL_ACCESS,SDesc.Owner,false));
      SDesc.SACL.Add(TJwAuditAccessControlEntry.Create(nil,true, false, FILE_ALL_ACCESS,JwWorldSID,false));

      HasSACL := true;
    end
    else
      HasSACL := false;

    result := '';
    try
      pSecAttr := SDesc.Create_SA(true);

      handle := jwaWindows.CreateFile(pTempName,
                 FILE_ALL_ACCESS,
                 0,
                 Pointer(pSecAttr),
                 CREATE_ALWAYS,
                 FILE_ATTRIBUTE_NORMAL,
                 0);
      CheckNotEquals(Integer(INVALID_HANDLE_VALUE),Integer(handle), EJwsclSecurityException.GetLastErrorMessage());

      CloseHandle(handle);

      //another way to set security without using TJwSecureFileObject
      //remove old dac
      TJwSecureGeneralObject.SetNamedSecurityInfo(pTempName,
                  SE_FILE_OBJECT,
                  [siOwnerSecurityInformation, siDaclSecurityInformation],
                  SDesc.Owner,
                  nil,
                  nil,
                  nil);

      TJwSecureGeneralObject.SetNamedSecurityInfo(pTempName,
                  SE_FILE_OBJECT,
                  [siOwnerSecurityInformation, siDaclSecurityInformation],
                  SDesc.Owner,
                  nil,
                  SDesc.DACL,
                  nil);

    finally
      SDesc.Free_SA(pSecAttr);
      if JwIsPrivilegeSet(SE_SECURITY_NAME) then
      begin
        JwEnablePrivilege(SE_SECURITY_NAME, pst_Disable);
      end;
    end;
  finally
    SDesc.Free;
  end;

  result := pTempName;
end;

procedure TSecureFileObjectTests.Test_Inheritance;
var d : TJwString;
    h : THandle;
begin
  //d := GetCurrentDir;
  (*d := 'G:\Temp\2\';


  try
    TJwSecureFileObject.RemoveInheritanceFlow(d,false); //false = remove inherited ACE
  finally
    TJwSecureFileObject.RestoreInheritanceFlow(d,true);
  end;*)

  //DACL of d should now be the same
end;

procedure OutputDebugString(o : TJwString);
begin
{$IFDEF UNICODE}
    OutputDebugStringW(TJwPChar(o));
{$ELSE}
    OutputDebugStringA(TJwPChar(o));
{$ENDIF}
end;

procedure TSecureFileObjectTests.FNProgressMethod2(const pObjectName : TJwString;              // Name of object just processed
                                const cStatus : Cardinal;                    // Status of operation on object
                                var pInvokeSetting : TJwProgInvokeSetting; // When to set
                                const E : EJwsclSecurityException;         //Exception type that was raised or nil if none - only supported by TJwSecureFileObject.TreeFileObjectSetNamedSecurityInfo
                                const Args : Pointer;                      // Caller specific data
                                const bSecuritySet : Boolean                 // Whether security was set
                                );
var R : Integer;
begin
  if pInvokeSetting = pis_ProgressInvokeOnError then
  begin
{$IFDEF UNICODE}
    R := MessageBoxW(GetActiveWindow,TJwPChar(EJwsclSecurityException.GetErrorMessage(cStatus) + #13#10+pObjectName),'Error',MB_CANCELTRYCONTINUE);
{$ELSE}
    R := MessageBoxA(GetActiveWindow,TJwPChar(EJwsclSecurityException.GetErrorMessage(cStatus) + #13#10+pObjectName),'Error',MB_CANCELTRYCONTINUE);
{$ENDIF}
    case R of
      IDTRYAGAIN : pInvokeSetting := pis_ProgressRetryOperation;
      IDCONTINUE : pInvokeSetting := pis_ProgressInvokeEveryObject;
    else
      pInvokeSetting := pis_ProgressCancelOperation;
    end;
    OutputDebugString('Security change failed for: '+pObjectName+#10);
  end
  else
  if pInvokeSetting = pis_ProgressFinished then
  begin
     OutputDebugString('Security iteration finished for: '+pObjectName+#10);

  end
  else
  begin
     OutputDebugString('Setting file/folder security to: '+pObjectName+#10);

  end;
end;

procedure TSecureFileObjectTests.Test_TreeResetNamedSecurityInfo;
var
  pTempName : Array[0..MAX_PATH+1] of char;
  bI : Boolean;
  DACL :TJwDAccessControlList;

  InhFromArray : TJwInheritedFromArray;
  i : Integer;
  s : TJwString;
  p : Pointer;
begin

  DACL := TJwDAccessControlList.Create;
  DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[afContainerInheritAce],GENERIC_ALL,JwWorldSID, false));

  try
  {TODO: error
   TJwSecureFileObject.TreeResetNamedSecurityInfo(
                                '',
                                                [siDaclSecurityInformation,siUnprotectedDaclSecurityInformation],//const aSecurityInfo : TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
                                                nil,//const Owner : TJwSecurityId;
                                                nil,//const Group : TJwSecurityId;
                                                DACL,//const DACL : TJwDAccessControlList;
                                                nil,//const SACL : TJwSAccessControlList;
                                                false,//const bKeepExplicit : Boolean;
                                                pis_ProgressInvokeEveryObject,//const fnProgress : TJwProgInvokeSetting;
                                                FNProgressMethod, //const FNProgressMethod : TJwFnProgressMethod;
                                                nil,//const FNProgressProcedure : TJwFnProgressProcedure;
                                                Pointer(1234),//const ProgressUserData : Pointer);
                                                );   }
  finally
    DACL.Free;
  end;
end;

procedure TSecureFileObjectTests.Test_TreeFileObjectSetNamedSecurityInfo;
var hThreadHandle : THandle;

    DACL,DACL2 : TJwDAccessControlList;
    SD : TJwSecurityDescriptor;
    thread : TJwTagThread;
begin
  DACL := TJwDAccessControlList.Create;
  DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwWorldSID, false));
  thread := TJwTagThread.Create;
  thread.FreeOnTerminate := true;
   try
  {
  TJwSecureFileObject.TreeFileObjectSetNamedSecurityInfo(
      'G:\Temp\2',//pObjectName : TJwString;
      [],//const aSecurityInfo : TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
      pis_ProgressInvokeEveryObject, //const Action : TTreeActionInvoker;
      nil,//const Owner : TJwSecurityId;
      nil,//const Group : TJwSecurityId;
      nil,//const DACL : TJwDAccessControlList;
      nil,//const SACL : TJwSAccessControlList;
      FNProgressMethod2,//const FNProgressMethod : TJwFnProgressMethod;
      nil,//const FNProgressProcedure : TJwFnProgressProcedure;
      nil,//const ProgressUserData : Pointer;
      false,//const bUseThread : Boolean;
      hThreadHandle,//out hThreadHandle : THandle;
      true//const Disable64Redirection : boolean = false);
      );   }

   TJwSecureFileObject.TreeFileObjectSetNamedSecurityInfo(
      'P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\SecurityManager\dunit\2',//pObjectName : TJwString;
      [siDaclSecurityInformation],//const aSecurityInfo : TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
      pis_ProgressInvokeEveryObject, //const Action : TTreeActionInvoker;
      tstReset,//tstReset,
      false, //keepExplicit
      nil,//const Owner : TJwSecurityId;
      nil,//const Group : TJwSecurityId;
      DACL,//const DACL : TJwDAccessControlList;
      nil,//const SACL : TJwSAccessControlList;
      FNProgressMethod2,//const FNProgressMethod : TJwFnProgressMethod;
      nil,//const FNProgressProcedure : TJwFnProgressProcedure;
      nil,//const ProgressUserData : Pointer;
      //true,//const bUseThread : Boolean;
      //hThreadHandle,//out hThreadHandle : THandle;
      thread,
      true//const Disable64Redirection : boolean = false);
      );
   //WaitForSingleObject(hThreadHandle,INFINITE);
   //CloseHandle(hThreadHandle);
   finally
     FreeAndNil(DACL);
   end;
end;


procedure TSecureFileObjectTests.Test_SupportACL;
var b : Boolean;
begin
  try
    b := TJwSecureFileObject.SupportACL('');
    CheckIs(nil,EJwsclInvalidParameterException);
  except
    on E : Exception do
      CheckIs(E,EJwsclInvalidParameterException);
  end;

  try
    b := TJwSecureFileObject.SupportACL('\\');
    CheckIs(nil,EJwsclWinCallFailedException);
  except
    on E : Exception do
      CheckIs(E,EJwsclWinCallFailedException);
  end;

  b := TJwSecureFileObject.SupportACL('.');
  b := TJwSecureFileObject.SupportACL(GetCurrentDir());
  //b := TJwSecureFileObject.SupportACL('\\127.0.0.1\DFAT32\');
end;

procedure TSecureFileObjectTests.Test_GetFileInheritanceSource;
var
  pTempName : Array[0..MAX_PATH+1] of char;
  bI : Boolean;
  pSecAttr : jwaWindows.PSecurityAttributes;
  handle : THANDLE;
  SDesc : TJwSecurityDescriptor;
  pDACL : jwaWindows.PACL;
  b : Boolean;

  InhFromArray : TJwInheritedFromArray;
  i : Integer;
  s : TJwString;
begin
  FillChar(pTempName, sizeof(pTempName), 0);
  CheckNotEquals(0,
        GetTempFileName('.', // LPCTSTR lpPathName,
                        'SMLTest', //   LPCTSTR lpPrefixString,
                        0, // UINT uUnique,
                        @pTempName //  LPTSTR lpTempFileName
                        ));


  //InhFromArray := TJwSecureFileObject.GetFileInheritanceSource('o:\');//pTempName);
  //InhFromArray := TJwSecureFileObject.GetFileInheritanceSource('P:\Eigene Dateien\Dezipaitor\Battlefield 2\Profiles\0001\Video.con');//pTempName);
  //InhFromArray := TJwSecureFileObject.GetFileInheritanceSource('P:\Eigene Dateien\Dezipaitor\Einwählen.txt');//pTempName);
  //InhFromArray := TJwSecureFileObject.GetFileInheritanceSource('P:\Eigene Dateien\Dezipaitor');//pTempName);
  //InhFromArray := TJwSecureFileObject.GetFileInheritanceSource('H:\Dokumente und Einstellungen\Administrator\Startmenü');//pTempName);

  s := '';
  for i := 0 to high(InhFromArray) do
  begin
    S := s + InhFromArray[i].SID +' ' + IntToStr(InhFromArray[i].GenerationGap) + ':'+ InhFromArray[i].AncestorName + #13#10;
  end;
  SetLength(InhFromArray,0);

  //ShowMessageForm(s);

  DeleteFile(pTempName);
end;

procedure TSecureFileObjectTests.FNProgressMethod(const pObjectName : TJwString;              // Name of object just processed
                                const cStatus : Cardinal;                    // Status of operation on object
                                var pInvokeSetting : TJwProgInvokeSetting; // When to set
                                const E : EJwsclSecurityException;         //Exception type that was raised or nil if none - only supported by TJwSecureFileObject.TreeFileObjectSetNamedSecurityInfo
                                const Args : Pointer;                      // Caller specific data
                                const bSecuritySet : Boolean                 // Whether security was set

                                );
begin
end;
procedure FNProgressProcedure(
                                const pObjectName : TJwString;              // Name of object just processed
                                const cStatus : Cardinal;                    // Status of operation on object
                                var pInvokeSetting : TJwProgInvokeSetting; // When to set
                                const E : EJwsclSecurityException;         //Exception type that was raised or nil if none - only supported by TJwSecureFileObject.TreeFileObjectSetNamedSecurityInfo
                                const Args : Pointer;                      // Caller specific data
                                const bSecuritySet : Boolean                 // Whether security was set

                                );
begin
end;



procedure TSecureFileObjectTests.Test_GetInheritanceSource;
var
  pTempName : Array[0..MAX_PATH+1] of char;
  bI : Boolean;
  pSecAttr : jwaWindows.PSecurityAttributes;
  handle : THANDLE;
  SDesc : TJwSecurityDescriptor;
  pDACL : jwaWindows.PACL;
  b : Boolean;

  InhFromArray : TJwInheritedFromArray;
begin
  FillChar(pTempName, sizeof(pTempName), 0);
  CheckNotEquals(0,
        GetTempFileName('.', // LPCTSTR lpPathName,
                        'SMLTest', //   LPCTSTR lpPrefixString,
                        0, // UINT uUnique,
                        @pTempName //  LPTSTR lpTempFileName
                        ));


  fSecureFileObjects[1] := TJwSecureFileObject.Create(pTempName);
  ShowMessageForm(fSecureFileObjects[1].DACL.Text);
  try
    InhFromArray := fSecureFileObjects[1].GetInheritanceSource(
                ExpandFileName(pTempName),
                [siDaclSecurityInformation],
                false,
                nil);

  finally
    FreeAndNil(fSecureFileObjects[1]);
  end;

  DeleteFile(pTempName);
end;

{ TSecureRegistryKeyTests }

procedure TSecureRegistryKeyTests.FNProgressMethod(
  const pObjectName: TJwString; const cStatus: Cardinal;
  var pInvokeSetting: TJwProgInvokeSetting; const E: EJwsclSecurityException;
  const Args: Pointer; const bSecuritySet: Boolean);
begin

end;

procedure TSecureRegistryKeyTests.GetKeyInheritanceSource(
  const KeyName: TJwString);
var arr : TJwInheritedFromArray;
begin
  //TJwSecureRegistryKey.SetSecurityDescriptorEx('\\local\HKEY_CURRENT_USER\__TEST', [], nil,false);
  //arr := TJwSecureRegistryKey.GetKeyInheritanceSource(rrkString,'\\xpp2\USERS\S-1-5-18\Control Panel\Current',false,[siDaclSecurityInformation]);
  //arr := TJwSecureRegistryKey.GetKeyInheritanceSource(rrkString,'\\local\HKEY_CURRENT_USER\Software',false,[siDaclSecurityInformation]);
//  arr := TJwSecureRegistryKey.GetKeyInheritanceSource(rrkString,'asdfasdf',false,[siDaclSecurityInformation]);
//  arr := TJwSecureRegistryKey.GetKeyInheritanceSource(rrkString,'\\xpp2\USERS',false,[siDaclSecurityInformation]);
  if Length(arr) = 0 then;
end;

procedure TSecureRegistryKeyTests.Test_Create;
var K : TJwSecureRegistryKey;
    F:  TRegistry;
begin
  F := TRegistry.Create();
  try

    F.RootKey := HKEY_CURRENT_USER;
    if F.OpenKey('Software\Microsoft',false) then
    begin
      K := TJwSecureRegistryKey.Create(F);
      try
      finally
        K.Free;
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TSecureRegistryKeyTests.Test_SecurityTreeResetForm;
begin
  //TODO: error TSM_SecurityTreeReset_Form.CreateSecurityTreeResetForm(Application.MainForm);
end;

procedure TSecureRegistryKeyTests.Test_TreeKeyObjectSetNamedSecurityInfo;
var hThreadHandle : THandle;

    DACL,DACL2 : TJwDAccessControlList;
    SD : TJwSecurityDescriptor;
    thread : TJwTagThread;
    Reg : TRegistry;
    i,i2 : Integer;

    //TODO: Error SecurityTreeReset_Form : TSM_SecurityTreeReset_Form;
begin
(*
//TODO: Error
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_CURRENT_USER;
 { Reg.OpenKey(SECURE_OBJECT_KEY, true);
  Reg.CloseKey;
  Reg.OpenKey(SECURE_OBJECT_KEY+'1', true);
  Reg.CloseKey;
  Reg.OpenKey(SECURE_OBJECT_KEY+'1\2', true);
  Reg.CloseKey;
  Reg.OpenKey(SECURE_OBJECT_KEY+'1\2\3', true);
  Reg.CloseKey;
  Reg.OpenKey(SECURE_OBJECT_KEY+'1a', true);
  Reg.CloseKey;
  Reg.OpenKey(SECURE_OBJECT_KEY+'1b', true);
  Reg.CloseKey;
  Reg.OpenKey(SECURE_OBJECT_KEY+'1c', true);
  Reg.CloseKey;                   }

  for i := 3 to 10000 do
  begin
    Reg.OpenKey(SECURE_OBJECT_KEY+IntToStr(i), true);
    Reg.CloseKey;
  end;



  //thread := nil;
  thread := TJwTagThread.Create;
  try
    DACL := TJwDAccessControlList.Create;
    DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,TJwAfThisKeyAndSubKeys,GENERIC_ALL,JwWorldSID, false));
    thread := TJwTagThread.Create;
    thread.FreeOnTerminate := true;


    SecurityTreeReset_Form := TSM_SecurityTreeReset_Form.Create(Application.MainForm);
    try
      //Application.MainForm.Enabled := FALSE;
      SecurityTreeReset_Form.Show;
       try
         TJwSecureRegistryKey.TreeKeySetNamedSecurityInfo(
            JwKeyRootTupleArray[rkeCurrentUser].RootName + '\' + SECURE_OBJECT_KEY,//pObjectName : TJwString;
            [siDaclSecurityInformation, siProtectedDaclSecurityInformation],//const aSecurityInfo : TJwSecurityInformationFlagSet; //  SECURITY_INFORMATION SecurityInfo,
            pis_ProgressInvokeEveryObject, //const Action : TTreeActionInvoker;
            tstReset,//tstReset,
            false, //keepExplicit
            nil,//const Owner : TJwSecurityId;
            nil,//const Group : TJwSecurityId;
            DACL,//const DACL : TJwDAccessControlList;
            nil,//const SACL : TJwSAccessControlList;
            SecurityTreeReset_Form.FNProgressMethod,//FNProgressMethod,//const FNProgressMethod : TJwFnProgressMethod;
            nil,//const FNProgressProcedure : TJwFnProgressProcedure;
            nil,//const ProgressUserData : Pointer;
            nil,//thread, //hThreadHandle,//out hThreadHandle : THandle;
            false//const Disable64Redirection : boolean = false);
            );
       //WaitForSingleObject(hThreadHandle,INFINITE);
       //CloseHandle(hThreadHandle);

       finally
         FreeAndNil(DACL);
       end;
     finally
        //Application.MainForm.Enabled := TRUE;
        SecurityTreeReset_Form.Free;
     end;
  finally
    Reg.Free;
  end;        *)
end;

initialization

  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecureGeneralObjectTests.Suite);
  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecureFileObjectTests.Suite);
  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecureRegistryKeyTests.Suite);
  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecureWindowObjectTests.Suite);
  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecurePrinterTests.Suite);
  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecureServiceTests.Suite);
  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecureLMShareTests.Suite);
  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecureMutexTests.Suite);
  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecureSemaphoreTests.Suite);
  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecureEventTests.Suite);
  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecureWaitableTimerTests.Suite);
  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecureFileMappingTests.Suite);
  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecureProcessTests.Suite);
  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecureThreadTests.Suite);
  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecureWindowStationObjectTests.Suite);
  TestFramework.RegisterTest('JwsclSecureObjectsTests Suite',
    TSecureDesktopObjectTests.Suite);

end.



(**

procedure TSecureGeneralObjectTests.TestConvertSecurityInformation;
var anOwner : TJwSecurityId;
    anGroup : TJwSecurityId;
    aDACL : TJwDAccessControlList;
    aSACL : TJwSAccessControlList;



    hMutex : Cardinal;
    threadID : Cardinal;

    Desc : JwsclDescriptor.TJwSecurityDescriptor;

    pSA : jwaWindows.PSecurityAttributes;
    pSA2 : LPSECURITY_ATTRIBUTES;

    adminToken, Threadtok : TJwSecurityToken;
    privs : TJwPrivilegeSet;
begin
  fSecureFileObjects[1] := TJwSecureGeneralObject.Create;


  adminToken := TJwSecurityToken.CreateLogonUser('Administrator','','',LOGON32_LOGON_INTERACTIVE, LOGON32_PROVIDER_DEFAULT);
//  adminToken.ConvertToImpersonatedToken(jwaWindows.SECURITY_MAX_IMPERSONATION_LEVEL,TOKEN_ALL_ACCESS);
  //adminToken.ImpersonateLoggedOnUser;
  //adminToken.SetThreadToken(0);


  Threadtok := TJwSecurityToken.CreateTokenByThread(0,TOKEN_ALL_ACCESS,true);
  ShowMessageForm(Threadtok.TokenUser.GetText(true));


 // privs := adminToken.GetTokenPrivileges;
 // ShowMessageForm(privs.GetText);
// privs.Free;

  Desc := TJwSecurityDescriptor.Create;
  Desc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,TJwSecurityId.Create('S-1-5-18'),true));
  Desc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,TJwSecurityId.Create('S-1-10-5'),true));
  Desc.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_ALL,JwSecurityProcessUserSID,false));


  adminToken.PrivilegeEnabled[SE_TAKE_OWNERSHIP_NAME] := true;
  adminToken.PrivilegeEnabled[SE_SECURITY_NAME] := true;

  Desc.Owner := TJwSecurityId.Create('','Administrator');
  Desc.OwnOwner := true;

  pSA := jwaWindows.PSecurityAttributes(Desc.Create_SA(false,true));

  pSA2 := LPSECURITY_ATTRIBUTES(Pointer(pSA));

  hMutex := CreateMutex(pSA2,true,'123');


  try
    if (hMutex = 0) then
      raise EJwsclSecurityException.CreateFmtEx('Invalid hThread','Free_SA',ClassName,'',0,true,[]);


    try
      fSecureFileObjects[1].GetSecurityInfo(hMutex, SE_KERNEL_OBJECT,
                          [siDaclSecurityInformation, siOwnerSecurityInformation,
                           siGroupSecurityInformation],
                          anOwner, anGroup, aDACL, aSACL);
    except
      on E : EJwsclWinCallFailedException do
      begin

        raise;
      end;
    end;

    if Assigned(aDACL) and Assigned(anOwner) then
      ShowMessageForm(aDACL.Text+#13#10+anOwner.GetText());


  finally
    adminToken.RemoveThreadToken(0);

    Desc.Free_SA(jwaWindows.PSecurityAttributes(pSA));

    anOwner.Free;
    anGroup.Free;
    aDACL.Free;
    aSACL.Free;
    Desc.Free;
    adminToken.Free;
  end;
end;

**)
