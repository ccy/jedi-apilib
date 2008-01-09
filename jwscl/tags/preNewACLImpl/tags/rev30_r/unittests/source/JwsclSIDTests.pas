unit JwsclSIDTests;

interface

uses
  Forms,
  UMessageForm,
  JwsclSid,
  jwaWindows,
  jwaVista,
  TestFrameWork,
  JwsclStrings;

type
  TSecurityIDListTests = class(TTestCase)
  private
     fSIDList : array[1..5] of TJwSecurityIdList;
  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure Test_Create;
//    procedure TestGetItem;
    procedure TestCreate_OwnSid;
    procedure TestCreate_token_groups;
    procedure TestCreate_PSID_Array;
    procedure TestFree_PSID_Array;

    procedure Test_GetText;

   { procedure TestAdd;
    procedure TestFirst;
    procedure TestIndexOf;
    procedure TestInsert;
    procedure TestLast;
    procedure TestRemove;
    procedure TestGetText;
    }

  end;

type
  TSecurityIDTests = class(TTestCase)
  private

  protected
    fSIDs : Array[1..5] of TJwSecurityId;

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestCreate_PSid;
    procedure TestCreate_TSecurityID;
    procedure TestCreate_PSid_and_Attributes;
    procedure TestCreate_TSubAuthorityArray;
    procedure TestCreate_Array_Of_Cardinal;
    procedure TestCreate_CreateWellKnownSid;
    procedure TestCreate_BinarySid;
    procedure TestCreate_SystemName_AccountName;

    procedure Test_GetWindowsAccountDomainSid;
    procedure Test_Class_GetWindowsAccountDomainSid;
    procedure Test_EqualPrefixSid;
    procedure Test_EqualSid;

    procedure Test_EqualDomainSid;
    procedure Test_GetAccountSidString;
    procedure Test_CreateSidIdentifierAuthority;
    procedure Test_Create_PSID_AND_ATTRIBUTES_AndFree;
    procedure Test_CheckSID;
    procedure TEst_GetText;
    procedure Test_GetText_Output;

    procedure Test_Properties;



   

  end;

implementation

uses SysUtils, JwsclExceptions;

{$I Compilers.inc}  


{ TSecurityIDTests }
{$IFDEF COMPILER_5}
procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;

{$ENDIF}


function GetProcessTokenOwner(out hToken : HANDLE) : PTokenOwner;
var

    iLen : Cardinal;
begin
  hToken := 0;
  if not jwaWindows.OpenProcessToken (GetCurrentProcess,TOKEN_READ or TOKEN_QUERY, hToken) then
  begin
    if not jwaWindows.OpenThreadToken(GetCurrentThread,TOKEN_READ or TOKEN_QUERY, true,hToken) then
      RaiseLastOSError;
  end;


  iLen := 0;
  GetTokenInformation(hToken, JwaWindows.TokenOwner,nil,0,iLen);

  if GetLastError() <> ERROR_INSUFFICIENT_BUFFER then
    RaiseLastOSError;

  GetMem(result,iLen);
  if not GetTokenInformation(hToken,JwaWindows.TokenOwner,result,iLen,iLen) then
  begin
    FreeMem(result);
    RaiseLastOSError;
  end;

  if result = nil then
    raise EOutOfMemory.Create('Cannot get Token Owner');
end;

function GetProcessTokenDACL(out hToken : HANDLE) : PTokenGroups;
var

    iLen : Cardinal;
begin
  hToken := 0;
  if not jwaWindows.OpenProcessToken (GetCurrentProcess,TOKEN_READ or TOKEN_QUERY, hToken) then
  begin
    if not jwaWindows.OpenThreadToken(GetCurrentThread,TOKEN_READ or TOKEN_QUERY, true,hToken) then
      RaiseLastOSError;
  end;

  iLen := 0;
  GetTokenInformation(hToken,JwaWindows.TokenGroups,nil,0,iLen);

  if GetLastError() <> ERROR_INSUFFICIENT_BUFFER then
    RaiseLastOSError;

  GetMem(result,iLen);
  if not GetTokenInformation(hToken,JwaWindows.TokenGroups,result,iLen,iLen) then
  begin
    FreeMem(result);
    RaiseLastOSError;
  end;

  if result = nil then
    raise EOutOfMemory.Create('Cannot get Token Owner');
end;



procedure TSecurityIDTests.SetUp;
var i : Integer;
begin
  inherited;

  for i := low(fSIDs) to high(fSIDs) do
    fSIDs[i] := nil;
end;

procedure TSecurityIDTests.TearDown;
var i : Integer;
begin
  inherited;

  for i := low(fSIDs) to high(fSIDs) do
    if Assigned(fSIDs[i]) then
      fSIDs[i].Free;
end;


procedure TSecurityIDTests.TestCreate_PSid;
var hToken : HANDLE;
    pOwner : PTokenOwner;

begin
  
 pOwner := GetProcessTokenOwner(hToken);
 try

  try
    fSIDs[2] := TJwSecurityId.Create(PSID(nil));
    Check(false,'EJwsclNILParameterException must be raised.');
  except
    on E1 : Exception do
      CheckIs(E1,EJwsclNILParameterException,'EJwsclNILParameterException must be raised.');
  end;
              

  fSIDs[1] := TJwSecurityId.Create(pOwner.Owner);
  CheckNotNull(fSIDs[1],'Could not create SID class');


  CheckEqualsMem(nil,fSIDs[1].SID,0,'Invalid SID pointer');

  CheckFalse(fSIDs[1].WellKnownSidType = WinNtAuthoritySid,'WellKnownSidType is not supported');
  CheckFalse(fSIDs[1].IsWellKnownSid,'IsWellKnownSid is not supported');


 finally
  FreeMem(pOwner);
  CloseHandle(hToken);
 end;
end;



procedure TSecurityIDTests.TestCreate_TSecurityID;
var hToken : HANDLE;
    pOwner : PTokenOwner;
begin


 pOwner := GetProcessTokenOwner(hToken);
 try
  fSIDs[1] := TJwSecurityId.Create(pOwner.Owner);
  CheckNotNull(fSIDs[1],'Could not create SID class');

  //check for Create(TJwSecurityId)
  begin
    fSIDs[2] := TJwSecurityId.Create(fSIDs[1]);

    CheckTrue(fSIDs[1].EqualSid(fSIDs[2]),'Sids are not the same');
  end;

  CheckFalse(fSIDs[2].WellKnownSidType = WinNtAuthoritySid,'WellKnownSidType is not supported');
  CheckFalse(fSIDs[2].IsWellKnownSid,'IsWellKnownSid is not supported');
 finally
  FreeMem(pOwner);
  CloseHandle(hToken);
 end;
end;

procedure TSecurityIDTests.TestCreate_PSid_and_Attributes;
var hToken : HANDLE;
    pOwner : PTokenOwner;
    pSIDA : SID_AND_ATTRIBUTES;
begin

 pOwner := GetProcessTokenOwner(hToken);
 try
  fSIDs[1] := TJwSecurityId.Create(pOwner^.Owner);
  CheckNotNull(fSIDs[1],'Could not create SID class');

  try
    fSIDs[2] := TJwSecurityId.Create(PSID_AND_ATTRIBUTES(nil));
    Check(false,'EJwsclNILParameterException must be raised.');
  except
    on E1 : Exception do
      CheckIs(E1,EJwsclNILParameterException,'EJwsclNILParameterException must be raised.');
  end;
  fSIDs[2] := nil;


  pSIDA.Sid := fSIDs[1].SID;
  pSIDA.Attributes := 123;
  fSIDs[3] := TJwSecurityId.Create(PSID_AND_ATTRIBUTES(@pSIDA));
  CheckEquals(123,pSIDA.Attributes,'pSIDA.Attributes was changed!');

  CheckTrue(fSIDs[3].EqualSid(fSIDs[1]),'Sids are not equal');

 finally
  FreeMem(pOwner);
  CloseHandle(hToken);
 end;
end;


procedure TSecurityIDTests.TestCreate_TSubAuthorityArray;
var subAuth, sidSubAuth : TJwSubAuthorityArray;
    i : Integer;
    p : Pointer;
begin
 try

  SetLength(subAuth,0);
  try
    fSIDs[1] := TJwSecurityId.Create(subAuth,SECURITY_LOCAL_SID_AUTHORITY);
    fSIDs[1].Free;
   Check(false,'EJwsclIndexOutOfBoundsException must be raised.');
  except
    on E1 : Exception do
    begin
      CheckIs(E1,EJwsclIndexOutOfBoundsException,'EJwsclIndexOutOfBoundsException must be raised.');
    end;
  end;
  fSIDs[1] := nil;

  SetLength(subAuth,10);

  try
    fSIDs[2] := TJwSecurityId.Create(subAuth,SECURITY_LOCAL_SID_AUTHORITY);
    fSIDs[2].Free;
    Check(false,'EJwsclIndexOutOfBoundsException must be raised.');
  except
    on E1 : Exception do
      CheckIs(E1,EJwsclIndexOutOfBoundsException,'EJwsclIndexOutOfBoundsException must be raised.');
  end;
  fSIDs[2] := nil;



  SetLength(subAuth,8);
  for i := low(subAuth) to high(subAuth) do
    subAuth[7]:= i;

  fSIDs[3] := TJwSecurityId.Create(subAuth,SECURITY_LOCAL_SID_AUTHORITY);
  CheckNotNull(fSIDs[3],'Could not create SID class');

  sidSubAuth := fSIDs[3].SubAuthorityArray;
  for i := low(sidSubAuth) to high(sidSubAuth) do
  begin
    Check(sidSubAuth[i] = subAuth[i],'Invalid sub authority');
  end;
  fSIDs[3].Free;
  fSIDs[3] := Nil;
 finally

 end;
end;



procedure TSecurityIDTests.TestCreate_Array_Of_Cardinal;
var hToken : HANDLE;
    pOwner : PTokenOwner;
    i : Integer;
begin
 try
  try
    fSIDs[1] := TJwSecurityId.Create([],SECURITY_LOCAL_SID_AUTHORITY);
    Check(false,'EJwsclIndexOutOfBoundsException must be raised.');
  except
    on E1 : Exception do
      CheckIs(E1,EJwsclIndexOutOfBoundsException,'EJwsclIndexOutOfBoundsException must be raised.');
  end;

  try
    fSIDs[1] := TJwSecurityId.Create([0,0,0,0,0,0,0,0,0],SECURITY_LOCAL_SID_AUTHORITY);
    Check(false,'EJwsclIndexOutOfBoundsException must be raised.');
  except
    on E1 : Exception do
      CheckIs(E1,EJwsclIndexOutOfBoundsException,'EJwsclIndexOutOfBoundsException must be raised.');
  end;


  fSIDs[1] := TJwSecurityId.Create([1,2,3,4,5,6,7,8],SECURITY_LOCAL_SID_AUTHORITY);
  CheckNotNull(fSIDs[1],'Could not create SID class');


  for i := low(fSIDs[1].SubAuthorityArray) to high(fSIDs[1].SubAuthorityArray) do
  begin
    Check(fSIDs[1].SubAuthorityArray[i] = (i+1),'Invalid sub authority');
  end;

  CheckFalse(fSIDs[1].WellKnownSidType = WinNtAuthoritySid,'WellKnownSidType is not supported');
  CheckFalse(fSIDs[1].IsWellKnownSid,'IsWellKnownSid is not supported');
 finally

 end;
end;

procedure TSecurityIDTests.TestCreate_CreateWellKnownSid;
var
    i : Integer;
begin

 try
  Status('we do not check domain SID!');

  fSIDs[1] := TJwSecurityId.CreateWellKnownSid(WinLocalServiceSid,nil);
  CheckNotNull(fSIDs[1],'Could not create SID WinNtAuthoritySid class');



  Check(fSIDs[1].WellKnownSidType = WinLocalServiceSid,'Invalid WellKnownSidType');
  Check(fSIDs[1].IsWellKnownSid,'Invalid IsWellKnownSid');
 finally


 end;
end;



procedure TSecurityIDTests.TestCreate_BinarySid;
begin
  try

    try
      fSIDs[1] := TJwSecurityId.Create('');
      CheckIs(nil,EJwsclWinCallFailedException,'');
    except
      on E : Exception do
        CheckIs(E,EJwsclWinCallFailedException,'');
    end;

    try
      fSIDs[1] := TJwSecurityId.Create('???');
      CheckIs(nil,EJwsclWinCallFailedException,'');
    except
      on E : Exception do
        CheckIs(E,EJwsclWinCallFailedException,'');
    end;

    try
      fSIDs[1] := TJwSecurityId.Create('S-1-5-20');
    except
      on E : Exception do
        Check(false,'Could not create SID WinNtAuthoritySid class');
    end;

    CheckNotNull(fSIDs[1],'Could not create SID WinNtAuthoritySid class');

    CheckEqualsWideString(fSIDs[1].StringSID,'S-1-5-20','SID does not match expected S-1-5-20');



    CheckFalse(fSIDs[1].WellKnownSidType = WinNtAuthoritySid,'WellKnownSidType is not supported');
    CheckFalse(fSIDs[1].IsWellKnownSid,'IsWellKnownSid is not supported');

    fSIDs[1].Free;
    fSIDs[1] := Nil;


  finally


  end;
end;





procedure TSecurityIDTests.TestCreate_SystemName_AccountName;
begin
  try
    fSIDs[1] := TJwSecurityId.CreateWellKnownSid(WinBuiltinAdministratorsSid,nil);
    CheckNotNull(fSIDs[1],'Could not create SID WinBuiltinAdministratorsSid class');

    try
      fSIDs[2] := TJwSecurityId.Create('','');
    except
      on E : Exception do
        CheckIs(E,EJwsclWinCallFailedException,'');
    end;

    CheckNotNull(fSIDs[2],'Could not create SID class');
    fSIDs[2].Free;

    try
      fSIDs[2] := TJwSecurityId.Create('??','???');
      fSIDs[2].Free;
      CheckIs(nil,EJwsclWinCallFailedException,'');
    except
      on E : Exception do
        CheckIs(E,EJwsclWinCallFailedException,'');
    end;


    try
      fSIDs[2] := TJwSecurityId.Create('', fSIDs[1].AccountName['']);
    except
      on E : Exception do
        Check(false,'Could not create SID WinNtAuthoritySid class');
    end;

    CheckNotNull(fSIDs[2],'Could not create SID class');

    CheckEqualsWideString(fSIDs[1].AccountName[''],fSIDs[2].AccountName[''],'SID does not match expected Administrators');


    CheckFalse(fSIDs[2].WellKnownSidType = WinNtAuthoritySid,'WellKnownSidType is not supported');
    CheckFalse(fSIDs[2].IsWellKnownSid,'IsWellKnownSid is not supported');
  finally


  end;

end;

procedure TSecurityIDTests.Test_Class_GetWindowsAccountDomainSid;
var hToken : HANDLE;
    pOwner : PTokenOwner;

begin
  pOwner := GetProcessTokenOwner(hToken);

  try
    fSIDs[1] := TJwSecurityId.Create(pOwner.Owner);
    CheckNotNull(fSIDs[1],'Could not create SID WinBuiltinAdministratorsSid class');

    try
      fSIDs[2] := TJwSecurityId.GetWindowsAccountDomainSid(nil);
      CheckIs(nil,EJwsclNILParameterException,'');
    except
      on E : Exception do
        CheckIs(E,EJwsclNILParameterException,'');
    end;
          

    try
      fSIDs[2] := TJwSecurityId.GetWindowsAccountDomainSid(fSIDs[1]);
    except
      on E1: EJwsclWinCallFailedException do
        begin

          Enabled := false;
          CheckNull(E1,'Test omitted because a domain not found.'#13#10+E1.MEssage);

        end;
      
      on E: EJwsclInvalidSIDException do
      begin
        Enabled := false;
        CheckNull(E,'Test omitted because domain not found.');
        exit;
      end;
    end;


    fSIDs[3] := fSIDs[2].GetWindowsAccountDomainSid(fSIDs[1]);

    CheckTrue(fSIDs[2].EqualSid(fSIDs[3]),'Domain Sids are not equal');
  finally
   FreeMem(pOwner);
   CloseHandle(hToken);
  end;
    
end;

procedure TSecurityIDTests.Test_GetWindowsAccountDomainSid;
var hToken : HANDLE;
    pOwner : PTokenOwner;

begin
  pOwner := GetProcessTokenOwner(hToken);

  try
    fSIDs[1] := TJwSecurityId.Create(pOwner.Owner);
    CheckNotNull(fSIDs[1],'Could not create SID WinBuiltinAdministratorsSid class');

    try
      fSIDs[2] := fSIDs[1].GetWindowsAccountDomainSid;
    except
      on E1: EJwsclWinCallFailedException do
      begin
        Enabled := false;
        CheckNull(E1,'Test omitted because a domain not found.'#13#10+E1.MEssage);
        exit;
      end;

      on E2: EJwsclInvalidSIDException do
      begin
        Enabled := false;
        CheckNull(E2,'Test omitted because a domain not found.');
        exit;
      end;
    end;

  finally
   FreeMem(pOwner);
   CloseHandle(hToken);
  end;

end;

procedure TSecurityIDTests.Test_EqualPrefixSid;
begin
   fSIDs[1] := TJwSecurityId.Create('S-1-3-22-1005');
   CheckNotNull(fSIDs[1],'Could not create SID class');

   fSIDs[2] := TJwSecurityId.Create('S-1-3-22-1005');
   CheckNotNull(fSIDs[2],'Could not create SID class');

   fSIDs[3] := TJwSecurityId.Create('S-1-3-23-1006');
   CheckNotNull(fSIDs[3],'Could not create SID class');

   try
     fSIDs[1].EqualPrefixSid(nil);
     CheckIs(nil,EJwsclNILParameterException,'EqualPrefixSid must raise exception');
   except
     on E: EJwsclNILParameterException do
       CheckIs(E,EJwsclNILParameterException,'EqualPrefixSid must raise exception');
   end;

   CheckTrue(fSIDs[1].EqualPrefixSid(fSIDs[2]),'EqualPrefixSid returned false although the SIDs are prefix equal');
   CheckFalse(fSIDs[1].EqualPrefixSid(fSIDs[3]),'EqualPrefixSid returned true although the SIDs are not prefix equal');
end;

procedure TSecurityIDTests.Test_EqualSid;
begin
   fSIDs[1] := TJwSecurityId.CreateWellKnownSid(WinBuiltinAdministratorsSid,nil);
   CheckNotNull(fSIDs[1],'Could not create SID WinBuiltinAdministratorsSid class');

   fSIDs[2] := TJwSecurityId.CreateWellKnownSid(WinBuiltinAdministratorsSid,nil);
   CheckNotNull(fSIDs[2],'Could not create SID WinBuiltinAdministratorsSid class');

   fSIDs[3] := TJwSecurityId.CreateWellKnownSid(WinBuiltinUsersSid,nil);
   CheckNotNull(fSIDs[3],'Could not create SID WinBuiltinUsersSid class');

   try
     fSIDs[1].EqualSid(nil);
     CheckIs(nil,EJwsclNILParameterException,'EqualSid must raise exception');
   except
     on E: EJwsclNILParameterException do
       CheckIs(E,EJwsclNILParameterException,'EqualSid must raise exception');
   end;

   CheckTrue(fSIDs[1].EqualSid(fSIDs[2]),'EqualSid returned false although the SIDs are equal');

   CheckFalse(fSIDs[1].EqualSid(fSIDs[3]),'EqualSid returned true although the SIDs are not equal');
end;

procedure TSecurityIDTests.Test_CheckSID;
begin
end;

procedure TSecurityIDTests.Test_Create_PSID_AND_ATTRIBUTES_AndFree;
var p : PSID_AND_ATTRIBUTES;
begin
  fSIDs[1] := TJwSecurityId.CreateWellKnownSid(WinBuiltinAdministratorsSid,nil);
  CheckNotNull(fSIDs[1],'Could not create SID WinBuiltinAdministratorsSid class');

  p := fSIDs[1].Create_PSID_AND_ATTRIBUTES(123);
  CheckEqualsMem(nil,p,0,'');

  CheckTrue(p.Attributes = 123,'Invalid attributes');
  fSIDs[2] := TJwSecurityId.Create(p);
  CheckNotNull(fSIDs[2],'Could not create SID class from PSID_AND_ATTRIBUTES');

  CheckTrue(fSIDs[2].EqualSid(fSIDs[1]),'Invalid SID in PSID_AND_ATTRIBUTES');




  fSIDs[1].Free_PSID_AND_ATTRIBUTES(p);
  CheckEqualsMem(nil,p,0,'Free_PSID_AND_ATTRIBUTES 1 did not set to nil pointer');

  fSIDs[1].Free_PSID_AND_ATTRIBUTES(p);
  CheckEqualsMem(nil,p,0,'Free_PSID_AND_ATTRIBUTES 2 did not ignore nil pointer');

end;


procedure TSecurityIDTests.Test_CreateSidIdentifierAuthority;
var S : TSidIdentifierAuthority;
    i : Integer;
begin
  S := TJwSecurityId.CreateSidIdentifierAuthority(65,66,67,68,69,70);
  for i := low(S.Value) to high(S.Value) do
    CheckEquals(i+65,S.Value[i],'Identifier authority mismatch');
end;

procedure TSecurityIDTests.Test_EqualDomainSid;
begin
  Enabled := false;
  CheckNotNull(nil,'This test is not implemented.');
end;

procedure TSecurityIDTests.Test_GetAccountSidString;
var SysName,DomainName,UserName : JwsclStrings.TJwString;
    SidNameUse : Cardinal;
begin
  fSIDs[1] := TJwSecurityId.CreateWellKnownSid(WinBuiltinAdministratorsSid,nil);
  CheckNotNull(fSIDs[1],'Could not create SID WinBuiltinAdministratorsSid class');

  UserName := fSIDs[1].GetAccountSidString(SysName,DomainName, SidNameUse);

  CheckNotEqualsString('',Username,'Invalid username');
  CheckNotEqualsString('',DomainName,'Invalid username');
end;

procedure TSecurityIDTests.TEst_GetText;
var Text : String;
begin
  fSIDs[1] := TJwSecurityId.CreateWellKnownSid(WinBuiltinAdministratorsSid,nil);
  CheckNotNull(fSIDs[1],'Could not create SID WinBuiltinAdministratorsSid class');

  try
    Text := fSIDs[1].GetText(true);
  except
    on E : Exception do
      Check(false,'GetText raised an error although it should not do it');
  end;
  CheckNotEqualsString('',text);


  try
    Text := fSIDs[1].GetText(false);
  except
  end;
  CheckNotEqualsString('',text);

end;

procedure TSecurityIDTests.Test_GetText_Output;
var Text : String;
begin
  fSIDs[1] := TJwSecurityId.CreateWellKnownSid(WinBuiltinAdministratorsSid,nil);
  CheckNotNull(fSIDs[1],'Could not create SID WinBuiltinAdministratorsSid class');

  Text := fSIDs[1].GetText(true);
  ShowMessageForm(Text);


end;

procedure TSecurityIDTests.Test_Properties;
var sub : TJwSubAuthorityArray;
    i :Integer;
begin
  fSIDs[1] := TJwSecurityId.Create('S-1-5-7-2');
  CheckNotNull(fSIDs[1],'Could not create SID WinBuiltinAdministratorsSid class');

  CheckEqualsMem(fSIDs[1].SID,nil,0,'Invalid SID property');

  CheckEquals(2,fSIDs[1].SubAuthorityCount,'Invalid sub authority count');


  SetLength(sub,2);
  sub[0] := 7;
  sub[1] := 2;

  CheckEqualsMem(fSIDs[1].SubAuthorityArray,sub,length(sub)*sizeof(Cardinal),'Invalid SID sub authority property');


  try
    fSIDs[1].SubAuthority[Cardinal(-1)];
    CheckIs(nil,EJwsclIndexOutOfBoundsException,'Property SubAuthority did not raise EJwsclIndexOutOfBoundsException');
  except
    on E : Exception do
      CheckIs(E,EJwsclIndexOutOfBoundsException,'Property SubAuthority did not raise EJwsclIndexOutOfBoundsException');
  end;

  for i := 0 to 10 do
  try
    fSIDs[1].SubAuthority[fSIDs[1].SubAuthorityCount+i];
    CheckIs(nil,EJwsclIndexOutOfBoundsException,'Property SubAuthority did not raise EJwsclIndexOutOfBoundsException');
  except
    on E : Exception do
      CheckIs(E,EJwsclIndexOutOfBoundsException,'Property SubAuthority did not raise EJwsclIndexOutOfBoundsException');
  end;

  for i := 0 to fSIDs[1].SubAuthorityCount -1 do
  begin
    CheckEquals(sub[i],fSIDs[1].SubAuthority[i],'Invalid subauthority');
  end;


  CheckEquals(5,fSIDs[1].IdentifierAuthority.Value[5],'Invalid identifier authority');


  CheckNotEquals(0,fSIDs[1].SIDLength,'Invalid SIDLength');


 {    We do not test these values
       property WellKnownSidType : TWellKnownSidType read GetWellKnownSidType;
       property IsWellKnownSid : Boolean read GetWellKnownSid;
       property StringSID : TJwString read GetStringSID;
       property AccountName[SystemName : TJwString]    : TJwString  read GetAccountName;
       property AccountDomainName[SystemName : TJwString]    : TJwString  read GetAccountDomainName;

       property AccountNameUse[SystemName : TJwString]    : TSidNameUse  read GetAccountNameUse;
         }
end;



{ TSecurityIDListTests }

procedure TSecurityIDListTests.SetUp;
var i : Integer;
begin
  inherited;

  for i := low(fSIDList) to high(fSIDList) do
    fSIDList[i] := nil;
end;

procedure TSecurityIDListTests.TearDown;
var i : Integer;
begin
  inherited;

  for i := low(fSIDList) to high(fSIDList) do
    if Assigned(fSIDList[i]) then
      fSIDList[i].Free;
end;


procedure TSecurityIDListTests.Test_Create;
var i : Integer;
begin

  fSIDList[1] := TJwSecurityIdList.Create;

  CheckEquals(0,fSIDList[1].Count,'Invalid count');

  for i := 0 to 5 do
    try
     fSIDList[1].Items[i];
     CheckIs(nil,Exception,'Items[i] must raise Exception');
    except
      on E : Exception do;
    end;

  fSIDList[1].Free;
  fSIDList[1] := nil;
end;

procedure TSecurityIDListTests.TestCreate_OwnSid;
var i : Integer;

    Sid1, Sid2 : TJwSecurityId;
    aSID : PSID;
begin

  fSIDList[1] := TJwSecurityIdList.Create(false);
  CheckEquals(0,fSIDList[1].Count,'Invalid count');
  CheckEquals(false,fSIDList[1].OwnsObjects);

  Sid1 := TJwSecurityId.CreateWellKnownSid(WinBuiltinAdministratorsSid,nil);
  fSIDList[1].Add(Sid1);

  //free list but not the sids in it
  fSIDList[1].Free;

  try
    Sid1.Free;
  except
    on E :Exception do
      Check(false,'sid was freed although ownobjects was set to false');
  end;


  fSIDList[1] := TJwSecurityIdList.Create(true);
  CheckEquals(0,fSIDList[1].Count,'Invalid count');
  CheckEquals(true,fSIDList[1].OwnsObjects);

  Sid1 := TJwSecurityId.CreateWellKnownSid(WinBuiltinAdministratorsSid,nil);

  aSID := @Sid1.SID; //we need a direct pointer to to the pointer to the sid, so we can check the pointer to sid for 0 values
  fSIDList[1].Add(Sid1);

  //free the list and its sids
  fSIDList[1].Free;

  //aSID now should only contains zeros (because FreeSID frees with 0)
  if IsValidSid(aSID) then
  begin
    Sid2 := TJwSecurityId.CreateWellKnownSid(WinBuiltinAdministratorsSid,nil);
    CheckNotEqualsMem(Sid2,aSID,Sid2.SIDLength,'PSID Error');
  end;


  fSIDList[1] := TJwSecurityIdList.Create(true);
  for i := 0 to 5 do
    try
     fSIDList[1].Items[i];
     CheckIs(nil,Exception,'Items[i] must raise Exception');
    except
      on E : Exception do;
    end;
end;

procedure TSecurityIDListTests.TestCreate_token_groups;
var groups : PTokenGroups;
    hToken : Handle;
    i : Integer;
begin

  try
    groups := GetProcessTokenDACL(hToken);

    if groups = nil then
      Check(false,'No token groups found');


    //we do not test owngroups here because we already did in TestCreate_OwnSid
    fSIDList[1] := TJwSecurityIdList.Create(true,groups);

    CheckEquals(groups.GroupCount, fSIDList[1].Count,'Unequal count');
  finally
    FreeMem(groups);
    CloseHandle(hToken);
  end;
end;

procedure TSecurityIDListTests.TestCreate_PSID_Array;
var sids : PSidAndAttributesArray;
begin
  
  fSIDList[1] := TJwSecurityIdList.Create(true);
  fSIDList[1].Add(
      TJwSecurityId.CreateWellKnownSid(WinBuiltinAdministratorsSid,nil));

  sids := fSIDList[1].Create_PSID_Array();
  if sids = nil then
    Check(false,'Invalid PSidAndAttributesArray');

  CheckEquals(fSIDList[1].Count,length(sids^),'Invalid count');
  if not EqualSid(sids[0].Sid,fSIDList[1][0].SID) then
    Check(false,'SIDs are not equal');

  fSIDList[1].Free_PSID_Array(sids);
end;


procedure TSecurityIDListTests.TestFree_PSID_Array;
var sids : PSidAndAttributesArray;
begin

  
  fSIDList[1] := TJwSecurityIdList.Create(true);
  fSIDList[1].Add(
      TJwSecurityId.CreateWellKnownSid(WinBuiltinAdministratorsSid,nil));

  sids := fSIDList[1].Create_PSID_Array();
  if sids = nil then
    Check(false,'Invalid PSidAndAttributesArray');

fSIDList[1].Free_PSID_Array(sids);
end;

{
procedure TSecurityIDListTests.TestAdd;
begin

end;


procedure TSecurityIDListTests.TestFirst;
begin

end;



procedure TSecurityIDListTests.TestGetItem;
begin

end;

procedure TSecurityIDListTests.TestGetText;
begin

end;

procedure TSecurityIDListTests.TestIndexOf;
begin

end;

procedure TSecurityIDListTests.TestInsert;
begin

end;

procedure TSecurityIDListTests.TestLast;
begin

end;

procedure TSecurityIDListTests.TestRemove;
begin

end;      }

procedure TSecurityIDListTests.Test_GetText;
var groups : PTokenGroups;
    hToken : Handle;
    i : Integer;
begin

  try
    groups := GetProcessTokenDACL(hToken);

    if groups = nil then
      Check(false,'No token groups found');


    //we do not test owngroups here because we already did in TestCreate_OwnSid
    fSIDList[1] := TJwSecurityIdList.Create(true,groups);


    ShowMessageForm(fSIDList[1].GetText(true));

  finally
    FreeMem(groups);
    CloseHandle(hToken);
  end;

end;

initialization

  TestFramework.RegisterTest('JwsclSidTests Suite',
    TSecurityIDListTests.Suite);
  TestFramework.RegisterTest('JwsclSidTests Suite',
    TSecurityIDTests.Suite);

end.
 
