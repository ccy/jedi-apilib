unit JwsclDescriptorTests;
{$I ..\includes\Jwscl.inc}
interface

uses
  Dialogs,

  jwaWindows,
  jwaVista,
  JwsclConstants,
  JwsclSid,
  JwsclKnownSid,
  JwsclToken,
  JwsclDescriptor,
  JwsclAcl,
  JwsclTypes,
  JwsclMapping,
  JwsclStrings,
  TestFrameWork;

type
  TSecurityDescriptorTests = class(TTestCase)
  private
    SecurityDescriptors : Array[1..15] of TJwSecurityDescriptor;
    theToken : TJwSecurityToken;

  protected
    function GetSD( iRequests : Cardinal) :  PSECURITY_DESCRIPTOR;
    function CanAudit : Boolean;

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods

    procedure TestCreate;
    procedure TestCreate_JwaWinNT_TSecurityDescriptor;

    procedure Test_Create_SD;

    procedure Test_Create_SA;
    procedure CreatePrivateObjectSecurity;
    procedure ReplaceDescriptorElements;
    procedure SetPrivateObjectSecurity;
    procedure GetPrivateObjectSecurity;

    procedure Test_StringSD;
    procedure Test_SetGetAudit;
    procedure TestSetGetDACL;

    procedure Test_Stream;
    procedure Test_StreamHash;
    procedure Test_SetGetSACL;

    procedure Test_Assign;

    procedure Test_Owner;

  end;

implementation
uses
     SysUTils,
     JwsclExceptions, Classes;



{$IFDEF COMPILER_5}
procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;

{$ENDIF}


{ TSecurityDescriptorTests }

procedure TSecurityDescriptorTests.SetUp;
var i : Integer;
begin
  inherited;

  theToken := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);//TOKEN_QUERY or TOKEN_READ or TOKEN_ADJUST_PRIVILEGES);


  for i := low(SecurityDescriptors) to high(SecurityDescriptors) do
    SecurityDescriptors[i] := nil;
end;

procedure TSecurityDescriptorTests.TearDown;
var i : Integer;
begin
  inherited;

  for i := low(SecurityDescriptors) to high(SecurityDescriptors) do
    if Assigned(SecurityDescriptors[i]) then
      SecurityDescriptors[i].Free;

  theToken.Free;
  theToken := nil;
end;
procedure TSecurityDescriptorTests.TestCreate;
begin
  SecurityDescriptors[1] := TJwSecurityDescriptor.Create;

  with SecurityDescriptors[1] do
  begin
    CheckNull(Owner,'Owner');
    CheckNull(PrimaryGroup,'PrimaryGroup');

    CheckNotNull(DACL,'DACL');
    CheckNotNull(AuditACL,'AuditACL');
     CheckEquals(0,DACL.Count,'DACL.Count');
     CheckEquals(0,AuditACL.Count,'AuditACL.Count');

    CheckEquals(0,RMControl,'RMControl');

    CheckEquals(false,OwnOwner,'OwnOwner');
    CheckEquals(false,OwnPrimaryGroup,'OwnPrimaryGroup');
  end;

  SecurityDescriptors[1].Owner := JwSecurityProcessUserSID;
  SecurityDescriptors[1].OwnOwner := false; //do not free a standard SID !!

  SecurityDescriptors[1].PrimaryGroup := TJwSecurityId.Create('S-1-10-5');
  SecurityDescriptors[1].OwnPrimaryGroup := true; //do not free a standard SID !!

  SecurityDescriptors[1].DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_READ,SecurityDescriptors[1].Owner,false));
  SecurityDescriptors[1].DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],GENERIC_WRITE,SecurityDescriptors[1].Owner,false));

  SecurityDescriptors[1].SACL.Add(TJwAuditAccessControlEntry.Create(nil,true,true,FILE_GENERIC_READ,SecurityDescriptors[1].Owner,false));
  SecurityDescriptors[1].SACL.Add(TJwAuditAccessControlEntry.Create(nil,true,false,FILE_GENERIC_WRITE,SecurityDescriptors[1].Owner,false));

  SecurityDescriptors[2] := TJwSecurityDescriptor.Create(SecurityDescriptors[1]);

  with SecurityDescriptors[2] do
  begin
    CheckNotNull(Owner);
    CheckNotNull(PrimaryGroup);

    CheckFalse(SecurityDescriptors[1].Owner = Owner);
    CheckFalse(SecurityDescriptors[1].PrimaryGroup = PrimaryGroup);

    CheckTrue(SecurityDescriptors[1].Owner.EqualSid(Owner));
    CheckTrue(SecurityDescriptors[1].PrimaryGroup.EqualSid(PrimaryGroup));

    CheckNotNull(DACL,'DACL');
    CheckNotNull(AuditACL,'AuditACL');
     CheckEquals(2,DACL.Count,'DACL.Count');
     CheckEquals(2,AuditACL.Count,'AuditACL.Count');

    CheckEquals(true,OwnOwner,'OwnOwner');
    CheckEquals(true,OwnPrimaryGroup,'OwnPrimaryGroup');
  end;

  //nil must not throw an exception
  SecurityDescriptors[3] := TJwSecurityDescriptor.Create(TJwSecurityDescriptor(nil));

end;

function TSecurityDescriptorTests.CanAudit : Boolean;
var t : TJwPrivilegeSet;
begin
  //check if we can get audit entries
  result := theToken.PrivilegeAvailable[SE_SECURITY_NAME];

  if result then
    theToken.PrivilegeEnabled[SE_SECURITY_NAME] := true;

  {t := theToken.GetTokenPrivileges;
   MessageDlg(t.GetText,mtInformation,[mbok],0);
  t.Free;}
end;

function TSecurityDescriptorTests.GetSD( iRequests : Cardinal) :  PSECURITY_DESCRIPTOR;
var FileName : String;
    needed : Cardinal;

begin
  FileName := ParamStr(0);//'P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\SecurityManager\description.txt';


  //iRequests := 0;
 (* iRequests := DACL_SECURITY_INFORMATION or
     GROUP_SECURITY_INFORMATION or OWNER_SECURITY_INFORMATION {or SACL_SECURITY_INFORMATION};
  *)

  //check if we can get audit entries
  if CanAudit then
    iRequests := iRequests or SACL_SECURITY_INFORMATION;

 if not GetFileSecurity(PChar(FileName),
     iRequests,
     result,
     0, needed) then
  begin
    if GetLastError() <> ERROR_INSUFFICIENT_BUFFER then
      RaiseLastOSError;

    GetMem(result, needed);
    InitializeSecurityDescriptor(result,SECURITY_DESCRIPTOR_REVISION);

    if not GetFileSecurity(PChar(FileName),
     iRequests ,
     result,
     needed, needed) then
     begin
       FreeMem(result);
       result := nil;
       RaiseLastOSError;
     end;

  end;
end;


procedure TSecurityDescriptorTests.TestCreate_JwaWinNT_TSecurityDescriptor;
var aPSecurityDescriptor : PSECURITY_DESCRIPTOR;
  iRequests : Cardinal;
begin
  aPSecurityDescriptor := GetSD(DACL_SECURITY_INFORMATION or  GROUP_SECURITY_INFORMATION or OWNER_SECURITY_INFORMATION {or SACL_SECURITY_INFORMATION});

  if IsValidSecurityDescriptor(aPSecurityDescriptor) then
       SecurityDescriptors[1] := TJwSecurityDescriptor.Create(aPSecurityDescriptor);

  FreeMem(aPSecurityDescriptor);
end;

procedure TSecurityDescriptorTests.Test_Create_SA;
var tSA : TSecurityAttributes;
    pSA : PSecurityAttributes;
begin
  SecurityDescriptors[1] := TJwSecurityDescriptor.Create;

  SecurityDescriptors[1].Owner := nil; //first free old owner
  SecurityDescriptors[1].OwnOwner := false; //set to false so the next step does not copy the secuirty id in a new instance
  SecurityDescriptors[1].Owner := TJwSecurityId.Create('','Administrator'); //set new Sid
  SecurityDescriptors[1].OwnOwner := true; //lets free the Sid automatically

  pSA := SecurityDescriptors[1].Create_SA();
  CheckFalse(pSA = nil);
  CheckFalse(pSA^.lpSecurityDescriptor = nil);
  CheckFalse(pSA^.bInheritHandle);

  tSA := SecurityDescriptors[1].Create_SAEx();
  CheckFalse(pSA^.lpSecurityDescriptor = nil);
  CheckFalse(pSA^.bInheritHandle);

  SecurityDescriptors[1].Free_SA(pSA);
  CheckTrue(pSA = nil);

  SecurityDescriptors[1].Free_SAEx(tSA);
  CheckTrue(tSA.lpSecurityDescriptor = nil);


  SecurityDescriptors[1].Control := [sdcDaclPresent];
  SecurityDescriptors[1].DACL := nil;
  pSA := SecurityDescriptors[1].Create_SA();
  SecurityDescriptors[1].Free_SA(pSA);


end;

procedure TSecurityDescriptorTests.Test_Create_SD;
var aPSecurityDescriptor,
    aPSD : PSECURITY_DESCRIPTOR;
    iRequests : Cardinal;
begin
  aPSecurityDescriptor := GetSD(DACL_SECURITY_INFORMATION or  GROUP_SECURITY_INFORMATION or OWNER_SECURITY_INFORMATION {or SACL_SECURITY_INFORMATION});

  //SecurityDescriptors[1] := TJwSecurityDescriptor.Create(aPSecurityDescriptor);
  SecurityDescriptors[1] := TJwSecurityDescriptor.Create;

  SecurityDescriptors[1].Owner := nil; //first free old owner
  SecurityDescriptors[1].OwnOwner := false; //set to false so the next step does not copy the secuirty id in a new instance
  SecurityDescriptors[1].Owner := TJwSecurityId.Create('','Administrator'); //set new Sid
  SecurityDescriptors[1].OwnOwner := true; //lets free the Sid automatically

  aPSD := SecurityDescriptors[1].Create_SD(true);

  try
    SecurityDescriptors[2] := TJwSecurityDescriptor.Create(aPSD);
  finally
    //LocalFree(Cardinal(aPSD));
    FreeMem(aPSecurityDescriptor);
  end;


  SecurityDescriptors[1].Free_SD(aPSD);
end;



procedure TSecurityDescriptorTests.Test_Owner;
var
  SD : TJwSecurityDescriptor;
  Sid, Sid2 : TJwSecurityId;
begin
  SD := TJwSecurityDescriptor.Create;

  try
    Sid := TJwSecurityId.Create('S-1-0-0');

    CheckFalse(SD.OwnOwner);
    CheckNull(SD.Owner);

    SD.Owner := nil;
    CheckNull(SD.Owner);

    SD.Owner := Sid;
    CheckNotNull(SD.Owner);

    SD.Owner := nil;
    CheckNull(SD.Owner);
    CheckNotNull(Sid);

    SD.Owner := Sid;
    CheckNotNull(SD.Owner);
    CheckNotNull(Sid);


    SD.OwnOwner := true;
    SD.Owner := nil;
    CheckNull(SD.Owner);
    {This is very likely to break if the memory is changed where the Sid
    object was saved. But it is the only way of checking whether
    the Sid was destroyed because the destructor frees and sets the Sid property
    to nil.
    }
    CheckEquals(0, Integer(Sid.Sid));

    //The following code can be used to set a newly created instance.
    //first free or disconnect old owner
    //1. If OwnOwner is true, the Owner instance will be freed
    //2. If OwnOwner is false, the property will be set to nil
    SD.Owner := nil;
    SD.OwnOwner := false; //set to false so the next step does not copy the security id in a new instance
    SD.Owner := TJwSecurityID.Create('S-1-0-0'); //set new Sid
    SD.OwnOwner := true; //lets free the Sid automatically
    SD.Owner := nil;
    CheckEquals(0, Integer(Sid.Sid));

    //This code is equivalent:
    SD.Owner := nil; //free or release old owner
    SD.OwnOwner := false; //set to false so the next step does not copy the security id in a new instance
    SD.Owner := TJwSecurityID.Create('S-1-0-0');
    SD.OwnOwner := true; //lets free the Sid automatically
    SD.Owner := nil;
    CheckEquals(0, Integer(Sid.Sid));


    //Use this code to release old owner and copy new owner into a new instance:
    Sid := TJwSecurityID.Create('S-1-0-0');
    SD.Owner := nil; //free or release old owner
    SD.OwnOwner := true; //next set copies owner
    SD.Owner := Sid; //create copy of owner and set it
    CheckNotNull(Sid);
    CheckNotNull(SD.Owner);
    SD.Owner := nil;
    CheckNotEquals(0, Integer(Sid.Sid));


    { Use this code to use the same instance from another SD instance in both
      security descriptors. In this instance the owner will not be freed.
      You should free this instance first, before freeing the other one because
      if the original instance is freed you cannot access the owner because
      it is invalid but differs from nil.
    }
    Sid := TJwSecurityID.Create('S-1-0-0');
    SD.Owner := nil; //free or release old owner
    SD.OwnOwner := false; //do not free owner
    SD.Owner := Sid; //just point to this instance
    CheckNotNull(Sid);
    CheckNotNull(SD.Owner);
    SD.Owner := nil;

    CheckNotNull(Sid);
    CheckNull(SD.Owner);
  finally
    SD.Free();
  end;
end;

procedure TSecurityDescriptorTests.Test_SetGetAudit;

  procedure AddAudit(ID : TJwSecurityId; f,s : Boolean; AMask : Cardinal);
  var Sid : TJwSecurityId;
      anAudit : TJwAuditAccessControlEntry;
  begin
    try
    //http://msdn2.microsoft.com/en-us/library/aa364399.aspx
      sid := ID;
      anAudit := TJwAuditAccessControlEntry.Create(nil,[],amask ,sid,true);
      anAudit.AuditSuccess := s;
      anAudit.AuditFailure := f;

      try
        SecurityDescriptors[1].AuditACL.Add(anAudit);
      except
        anAudit.Free;
      end;
    except
    end;
  end;
var sSD,sSD2 : TJwString;

    anACE : TJwDiscretionaryAccessControlEntry;
    anAudit : TJwAuditAccessControlEntry;

    pSD,pSD2 : PSecurityDescriptor;

    b : Boolean;
    lb : longBool;
    DACL : PACL;
    c : Integer;
    iRequests : Cardinal;
begin
  if not CanAudit then
  begin
    Check(false,'Auditing is not possible because of not enough rights');
    exit;
  end;


  iRequests := SACL_SECURITY_INFORMATION;
  pSD := GetSD(iRequests);

  try
    SecurityDescriptors[1] := TJwSecurityDescriptor.Create(pSD);

    SecurityDescriptors[1].AuditACL.Clear;

    AddAudit(TJwSecurityId.CreateWellKnownSid(WinWorldSid),true,false         , FILE_EXECUTE);
    AddAudit(TJwSecurityId.CreateWellKnownSid(WinCreatorOwnerSid),false,true , FILE_MAP_WRITE);
    AddAudit(TJwSecurityId.CreateWellKnownSid(WinCreatorOwnerSid),true,true , FILE_GENERIC_READ);

    pSD2 := SecurityDescriptors[1].Create_SD(true);

    b := SetFileSecurity(PChar(ParamStr(0)),     iRequests ,pSD2);

    if not b then
       RaiseLastOSError;
  finally
    FreeMem(pSD);
    SecurityDescriptors[1].Free_SD(pSD2);
  end;

  if not b then
    exit;


  pSD := GetSD(iRequests);
  try
    SecurityDescriptors[2] := TJwSecurityDescriptor.Create(pSD);

    CheckEquals(SecurityDescriptors[1].SACL.Count, SecurityDescriptors[2].SACL.Count,'Invalid audit counts');

    CheckTrue(SecurityDescriptors[1].SACL.Items[0].SID.EqualSid(SecurityDescriptors[2].SACL.Items[0].SID));
    CheckTrue(SecurityDescriptors[1].SACL.Items[1].SID.EqualSid(SecurityDescriptors[2].SACL.Items[1].SID));
    CheckTrue(SecurityDescriptors[1].SACL.Items[2].SID.EqualSid(SecurityDescriptors[2].SACL.Items[2].SID));


  finally
    FreeMem(pSD);
  end;


  (*
     DACL := nil;
  if not GetSecurityDescriptorDacl(pSD,lb,DACL,lb) then
    RaiseLastOSError;

  SecurityDescriptors[3] := TJwSecurityDescriptor.Create(pSD2);
  //SecurityDescriptors[1].Free_SD(pSD);



  sSD := SecurityDescriptors[1].StringSD[0];
  sSD2 := SecurityDescriptors[3].StringSD[0];

  SecurityDescriptors[2] := TJwSecurityDescriptor.Create(sSD);
  c := SecurityDescriptors[2].DACL.Count;


  sSD := '';

  SecurityDescriptors[1].Free_SD(pSD2);
 finally
   FreeMem(pSD);
 end;*)
end;

procedure TSecurityDescriptorTests.TestSetGetDACL;
var d : TJwDAccessControlList;
    s : TJwString;
begin
  JwInitWellKnownSIDs;

  SecurityDescriptors[1] := TJwSecurityDescriptor.Create;
  CheckEquals(0, SecurityDescriptors[1].DACL.Count);

  d := TJwDAccessControlList.Create(true);

  try
    CheckEquals(0, d.Count);


    d.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[afInheritedAce],GENERIC_READ,JwNullSID,false));
    d.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[afInheritedAce],GENERIC_READ,JwNullSID,false));
    d.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_READ,JwNullSID,false));
    d.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],GENERIC_WRITE,JwNullSID,false));

    S := d.Text;
    //ShowMessage(S);

    if s = '' then;

    Status(S);

    CheckEquals(4, d.count);

    SecurityDescriptors[1].DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[afInheritedAce],GENERIC_READ,JwNullSID,false));
    SecurityDescriptors[1].DACL := nil;

    CheckNull(SecurityDescriptors[1].DACL);

    SecurityDescriptors[1].DACL := d; //point to our DACL (no a copy!)
    SecurityDescriptors[1].OwnDACL := false; //do not free DACL on desctruction
    CheckTrue(d = SecurityDescriptors[1].DACL,'The pointer to SD should be the same.');

    CheckEquals(d.count, SecurityDescriptors[1].DACL.Count);



    CheckIs(SecurityDescriptors[1].DACL.Items[0],TJwDiscretionaryAccessControlEntryDeny);
    CheckIs(SecurityDescriptors[1].DACL.Items[1],TJwDiscretionaryAccessControlEntryAllow);
    CheckTrue(SecurityDescriptors[1].DACL.Items[0].Flags = []);
    CheckTrue(SecurityDescriptors[1].DACL.Items[1].Flags = []);

    CheckIs(SecurityDescriptors[1].DACL.Items[2],TJwDiscretionaryAccessControlEntryDeny);
    CheckIs(SecurityDescriptors[1].DACL.Items[3],TJwDiscretionaryAccessControlEntryAllow);
    CheckTrue(SecurityDescriptors[1].DACL.Items[2].Flags = [afInheritedAce]);
    CheckTrue(SecurityDescriptors[1].DACL.Items[3].Flags = [afInheritedAce]);

    S := SecurityDescriptors[1].DACL.Text;
    Status(S);
   finally
    d.Free;
   end;




end;


procedure TSecurityDescriptorTests.Test_StringSD;
var Str,S1 : TJwString;

begin
  SecurityDescriptors[1] := TJwSecurityDescriptor.Create;
//  SecurityDescriptors[1].Owner := TJwSecurityId.CreateWellKnownSid(WinNtAuthoritySid);
  SecurityDescriptors[1].Owner := JwSecurityProcessUserSID;
  SecurityDescriptors[1].OwnOwner := false; //do not free a standard SID !!

  //SecurityDescriptors[1].PrimaryGroup := TJwSecurityId.CreateWellKnownSid(WinAnonymousSid);
  SecurityDescriptors[1].PrimaryGroup := TJwSecurityId.Create('S-1-10-5');
  SecurityDescriptors[1].OwnPrimaryGroup := true; //do not free a standard SID !!

  SecurityDescriptors[1].DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_READ,SecurityDescriptors[1].Owner,false));
  SecurityDescriptors[1].DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],GENERIC_WRITE,SecurityDescriptors[1].Owner,false));

  SecurityDescriptors[1].SACL.Add(TJwAuditAccessControlEntry.Create(nil,true,true,FILE_GENERIC_READ,SecurityDescriptors[1].Owner,false));
  SecurityDescriptors[1].SACL.Add(TJwAuditAccessControlEntry.Create(nil,true,false,FILE_GENERIC_WRITE,SecurityDescriptors[1].Owner,false));

  //http://msdn2.microsoft.com/en-us/library/aa374928.aspx
  Str := SecurityDescriptors[1].GetSecurityDescriptorString(JwSecurityInformationAllFlags);
  //StringSD[0];
  S1 := Format('O:%sG:%sD:(D;;GW;;;%0:s)(A;;GR;;;%0:s)S:(AU;SAFA;FR;;;%0:s)(AU;SA;FW;;;%0:s)'
                ,[SecurityDescriptors[1].Owner.StringSID,SecurityDescriptors[1].PrimaryGroup.StringSID]);

{$IFDEF NO_VISTA} //string above may vary in vista!
{$IFDEF COMPILER_5}CheckEqualsWideString{$ELSE}CheckEquals{$ENDIF}
   (S1,Str,'Unequal StringSD');
{$ENDIF NO_VISTA}


  SecurityDescriptors[2] := TJwSecurityDescriptor.Create(Str);

  CheckTrue(SecurityDescriptors[2].Owner.EqualSid(SecurityDescriptors[1].Owner));
  CheckTrue(SecurityDescriptors[2].OwnOwner);

  CheckTrue(SecurityDescriptors[2].PrimaryGroup.EqualSid(SecurityDescriptors[1].PrimaryGroup));
  CheckTrue(SecurityDescriptors[2].OwnPrimaryGroup);

  CheckEquals(SecurityDescriptors[2].DACL.Count,SecurityDescriptors[1].DACL.Count);
  CheckEquals(SecurityDescriptors[2].SACL.Count,SecurityDescriptors[1].SACL.Count);

  CheckTrue(SecurityDescriptors[2].DACL[0].SID.EqualSid(SecurityDescriptors[1].DACL[0].SID));
  CheckTrue(SecurityDescriptors[2].DACL[1].SID.EqualSid(SecurityDescriptors[1].DACL[1].SID));

  CheckTrue(SecurityDescriptors[2].SACL[0].SID.EqualSid(SecurityDescriptors[1].SACL[0].SID));
  CheckTrue(SecurityDescriptors[2].SACL[1].SID.EqualSid(SecurityDescriptors[1].SACL[1].SID));

end;

(*
var aPSecurityDescriptor,
    aPSD : PSECURITY_DESCRIPTOR;

    Str :  TJwString;
    anACE : TJwDiscretionaryAccessControlEntry;
    anAudit : TJwAuditAccessControlEntry;
    b : Boolean;
    iRequests : Cardinal;
    sid : TJwSecurityId;

    lb : LongBool;

    auditsE : TJwExplicitAccessArray;
    c : Cardinal;
    pE : PEXPLICIT_ACCESS;
    pSACL : PACL;
begin
  aPSecurityDescriptor := GetSD(iRequests);

  SecurityDescriptors[1] := TJwSecurityDescriptor.Create(aPSecurityDescriptor);
  //SecurityDescriptors[1] := TJwSecurityDescriptor.Create;

 { SecurityDescriptors[1].Owner := nil; //first free old owner
  SecurityDescriptors[1].OwnOwner := false; //set to false so the next step does not copy the secuirty id in a new instance
  SecurityDescriptors[1].Owner := TJwSecurityId.Create('','Administrator'); //set new Sid
  SecurityDescriptors[1].OwnOwner := true; //lets free the Sid automatically

   }

  if CanAudit then
  begin
    try
    //http://msdn2.microsoft.com/en-us/library/aa364399.aspx
      sid := TJwSecurityId.Create('','Dezipaitor');
      anAudit := TJwAuditAccessControlEntry.Create(nil,[],FILE_TRAVERSE ,sid,true);
      anAudit.AuditSuccess := false;
      anAudit.AuditFailure := true;

      SecurityDescriptors[1].AuditACL.Clear;
      try
        SecurityDescriptors[1].AuditACL.Add(anAudit);
      except
        anAudit.Free;
      end;
    except
    end;
  end;

 { try
    anACE := TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_READ,TJwSecurityId.Create('','Administrator'),true);
    try
      SecurityDescriptors[1].DACL.Add(anACE);
    except
      anACE.Free;
    end;
  except
  end;
       }

  //Str := SecurityDescriptors[1].StringSD[0];

  aPSD := SecurityDescriptors[1].Create_SD(true);

  {DACL_SECURITY_INFORMATION or
     GROUP_SECURITY_INFORMATION or OWNER_SECURITY_INFORMATION} {or SACL_SECURITY_INFORMATION};
  iRequests := SACL_SECURITY_INFORMATION;
  b := SetFileSecurity(PChar(ParamStr(0)),     iRequests ,aPSD);
  if not b then
    RaiseLastOSError;

  Str := '';


  FreeMem(aPSecurityDescriptor);
end;
*)

procedure TSecurityDescriptorTests.Test_StreamHash;
var StreamArray : array[1..10] of TMemoryStream;
    c : char;
    i : Integer;
    Value : Integer;
    Hash : Int64;
begin
  Value := 1234;
  Hash := TJwSecurityDescriptor.hashCode(Pointer(@Value),sizeof(value));
  CheckEquals(214,Hash);

  StreamArray[1] := TMemoryStream.Create;


  SecurityDescriptors[2] := TJwSecurityDescriptor.Create;
  SecurityDescriptors[2].OnHashCodeMethod := TJwSecurityDescriptor.hashCode;
  SecurityDescriptors[2].SaveToStream(StreamArray[1]);

  StreamArray[1].Position := 0;
  //check for correct hash function
  SecurityDescriptors[2].LoadFromStream(StreamArray[1]);

  //this should also work
  StreamArray[1].Position := 0;
  SecurityDescriptors[2].OnHashCodeMethod := nil;
  SecurityDescriptors[2].LoadFromStream(StreamArray[1]);


  SecurityDescriptors[2].OnHashCodeMethod := TJwSecurityDescriptor.hashCode;

  //set new position to SD_HEADER_SIZE minus 1 because we start from zero
  //we do not use this because we want to remain the SD version
  StreamArray[1].Seek(SD_HEADER_SIZE,soFromBeginning);

  //this for loop overrides the whole SD structure in the stream with 0
  //we so check for a correct hash comparision with the value calculated in savestream
  for i := 1 to StreamArray[1].Size-SD_HEADER_SIZE do
  begin
    C := Char(0);
    StreamArray[1].Write(c,sizeof(char));
  end;

  StreamArray[1].Position := 0;

  try
    SecurityDescriptors[2].LoadFromStream(StreamArray[1]);
    CheckIs(nil,EJwsclStreamHashException);
  except
    on E: Exception do
      CheckIs(E,EJwsclStreamHashException);
  end;


  StreamArray[1].Position := 0;
  //this should also work
  //
  SecurityDescriptors[2].OnHashCodeMethod := nil;
  SecurityDescriptors[2].LoadFromStream(StreamArray[1]);

  StreamArray[1].Free;
end;

procedure TSecurityDescriptorTests.Test_Stream;
var StreamArray : array[1..10] of TMemoryStream;
    magic : array[0..SD_MAGIC_LENGTH-1] of AnsiChar;
    i,
    iSDSize : Cardinal;


begin


  for i := low(StreamArray) to high(StreamArray) do
    StreamArray[i] := nil;

  SecurityDescriptors[1] := TJwSecurityDescriptor.Create;
  SecurityDescriptors[1].Owner := JwSecurityProcessUserSID;
  SecurityDescriptors[1].OwnOwner := false; //do not free a standard SID !!

  //SecurityDescriptors[1].PrimaryGroup := TJwSecurityId.CreateWellKnownSid(WinAnonymousSid);
  SecurityDescriptors[1].PrimaryGroup := TJwSecurityId.Create('S-1-10-5');
  SecurityDescriptors[1].OwnPrimaryGroup := true; //do not free a standard SID !!

  SecurityDescriptors[1].DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_READ,SecurityDescriptors[1].Owner,false));
  SecurityDescriptors[1].DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],GENERIC_WRITE,SecurityDescriptors[1].Owner,false));

  SecurityDescriptors[1].SACL.Add(TJwAuditAccessControlEntry.Create(nil,true,true,FILE_GENERIC_READ,SecurityDescriptors[1].Owner,false));
  SecurityDescriptors[1].SACL.Add(TJwAuditAccessControlEntry.Create(nil,true,false,FILE_GENERIC_WRITE,SecurityDescriptors[1].Owner,false));



  //***************
  SecurityDescriptors[2] := TJwSecurityDescriptor.Create;

  {This test checks, if an empty security can be saved in and restored from a stream
  }

  StreamArray[1] := TMemoryStream.Create;

  try
    SecurityDescriptors[2].SaveToStream(StreamArray[1]);



    StreamArray[1].Position := 0;
    SecurityDescriptors[3] := TJwSecurityDescriptor.Create(StreamArray[1]);



    CheckSame(nil,SecurityDescriptors[3].Owner);
    CheckSame(nil,SecurityDescriptors[3].PrimaryGroup);

    CheckTrue(SecurityDescriptors[3].OwnOwner);
    CheckTrue(SecurityDescriptors[3].OwnPrimaryGroup);

    CheckEquals(0,SecurityDescriptors[3].DACL.Count);
    CheckEquals(0,SecurityDescriptors[3].SACL.Count);
  finally
    StreamArray[1].Free;
  end;

  {This test checks a regular case.
  }
  StreamArray[2] := TMemoryStream.Create;
  try
    SecurityDescriptors[1].SaveToStream(StreamArray[2]);

    StreamArray[2].Position := 0;
    SecurityDescriptors[4] := TJwSecurityDescriptor.Create(StreamArray[2]);

    CheckTrue(SecurityDescriptors[4].Owner.EqualSid(SecurityDescriptors[1].Owner));
    CheckTrue(SecurityDescriptors[4].PrimaryGroup.EqualSid(SecurityDescriptors[1].PrimaryGroup));

    CheckTrue(SecurityDescriptors[4].OwnOwner);
    CheckTrue(SecurityDescriptors[4].OwnPrimaryGroup);

    CheckEquals(SecurityDescriptors[1].DACL.Count,SecurityDescriptors[4].DACL.Count);
    CheckEquals(SecurityDescriptors[1].SACL.Count,SecurityDescriptors[4].SACL.Count);
  finally
    StreamArray[2].Free;
  end;


  {Check for a SD stream that hast only a header and zero size information
  }
  StreamArray[3] := TMemoryStream.Create;
  try
//    magic:= SD_MAGIC_HEADER;
    StreamArray[3].Write(SD_MAGIC_HEADER,SD_MAGIC_LENGTH);

    iSDSize := 0;
    StreamArray[3].Write(iSDSize,sizeof(iSDSize));

    StreamArray[3].Position := 0;
    SecurityDescriptors[5] := TJwSecurityDescriptor.Create(StreamArray[3]);

    CheckTrue(SecurityDescriptors[5].Owner = nil);
    CheckTrue(SecurityDescriptors[5].PrimaryGroup = nil);

    CheckTrue(SecurityDescriptors[5].OwnOwner);
    CheckTrue(SecurityDescriptors[5].OwnPrimaryGroup);

    CheckFalse(SecurityDescriptors[5].DACL = nil);
    CheckEquals(0,SecurityDescriptors[5].DACL.Count);
    CheckEquals(0,SecurityDescriptors[5].SACL.Count);
    //hier noch checks
  finally
    StreamArray[3].Free;
  end;


  {check for an incorrect header
  }
  StreamArray[4] := TMemoryStream.Create;
  try
    magic:= '123';
    StreamArray[4].Write(magic,SD_MAGIC_LENGTH);

    StreamArray[4].Position := 0;
    try
      SecurityDescriptors[6] := TJwSecurityDescriptor.Create(StreamArray[4]);
      CheckIs(nil,EJwsclStreamInvalidMagicException);
    except
      on E : Exception do
        CheckIs(E,EJwsclStreamInvalidMagicException);
    end;


  finally
    StreamArray[4].Free;
  end;

  {Check for invalid size}
  StreamArray[5] := TMemoryStream.Create;
  try
//    magic:= SD_MAGIC_HEADER;
    StreamArray[5].Write(SD_MAGIC_HEADER,SD_MAGIC_LENGTH);

    iSDSize := 1;
    StreamArray[5].Write(iSDSize,sizeof(iSDSize));

    StreamArray[5].Position := 0;
    try
      SecurityDescriptors[7] := TJwSecurityDescriptor.Create(StreamArray[5]);
      CheckIs(nil,EJwsclStreamSizeException);
    except
      on E : Exception do
        CheckIs(E,EJwsclStreamSizeException);
    end;

    //hier noch checks
  finally
    StreamArray[5].Free;
  end;

end;

procedure TSecurityDescriptorTests.Test_SetGetSACL;
begin
  SecurityDescriptors[1] := TJwSecurityDescriptor.Create();

  CheckTrue(SecurityDescriptors[1].SACL = SecurityDescriptors[1].AuditACL);

  SecurityDescriptors[1].SACL.Add(TJwAuditAccessControlEntry.Create(nil,true,true,FILE_GENERIC_READ,JwSecurityProcessUserSID,false));
  SecurityDescriptors[1].SACL.Add(TJwAuditAccessControlEntry.Create(nil,true,false,FILE_GENERIC_WRITE,JwSecurityProcessUserSID,false));

  CheckEquals(2,SecurityDescriptors[1].SACL.Count);

  CheckTrue(SecurityDescriptors[1].SACL = SecurityDescriptors[1].AuditACL);
end;

procedure TSecurityDescriptorTests.Test_Assign;
begin
  SecurityDescriptors[1] := TJwSecurityDescriptor.Create();

  SecurityDescriptors[5] := TJwSecurityDescriptor.Create();
  try
    SecurityDescriptors[5].Assign(nil);
    CheckIs(nil,EJwsclNILParameterException);
  except
    on E : Exception do
      CheckIs(E,EJwsclNILParameterException);
  end;


  SecurityDescriptors[1].Owner := JwSecurityProcessUserSID;
  SecurityDescriptors[1].OwnOwner := false; //do not free a standard SID !!

  SecurityDescriptors[1].PrimaryGroup := TJwSecurityId.Create('S-1-10-5');
  SecurityDescriptors[1].OwnPrimaryGroup := true; //do not free a standard SID !!

  SecurityDescriptors[1].DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,[],GENERIC_READ,SecurityDescriptors[1].Owner,false));
  SecurityDescriptors[1].DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],GENERIC_WRITE,SecurityDescriptors[1].Owner,false));

  SecurityDescriptors[1].SACL.Add(TJwAuditAccessControlEntry.Create(nil,true,true,FILE_GENERIC_READ,SecurityDescriptors[1].Owner,false));
  SecurityDescriptors[1].SACL.Add(TJwAuditAccessControlEntry.Create(nil,true,false,FILE_GENERIC_WRITE,SecurityDescriptors[1].Owner,false));

  SecurityDescriptors[2] := TJwSecurityDescriptor.Create();
  SecurityDescriptors[2].Assign(SecurityDescriptors[1]);

  with SecurityDescriptors[2] do
  begin
    CheckNotNull(Owner);
    CheckNotNull(PrimaryGroup);

    CheckFalse(SecurityDescriptors[1].Owner = Owner);
    CheckFalse(SecurityDescriptors[1].PrimaryGroup = PrimaryGroup);

    CheckTrue(SecurityDescriptors[1].Owner.EqualSid(Owner));
    CheckTrue(SecurityDescriptors[1].PrimaryGroup.EqualSid(PrimaryGroup));

    CheckNotNull(DACL,'DACL');
    CheckNotNull(AuditACL,'AuditACL');
     CheckEquals(2,DACL.Count,'DACL.Count');
     CheckEquals(2,AuditACL.Count,'AuditACL.Count');

    CheckEquals(true,OwnOwner,'OwnOwner');
    CheckEquals(true,OwnPrimaryGroup,'OwnPrimaryGroup');
  end;

end;

procedure TSecurityDescriptorTests.CreatePrivateObjectSecurity;
begin
  SecurityDescriptors[1] := TJwSecurityDescriptor.Create();


  SecurityDescriptors[1].Owner := JwSecurityProcessUserSID;
  SecurityDescriptors[1].OwnOwner := false; //do not free a standard SID !!

  SecurityDescriptors[1].DACL.Add(
    TJwDiscretionaryAccessControlEntryAllow.Create(
      nil,TJwAfThisFolderAndSubFoldersAndFiles,GENERIC_READ,JwAdministratorsSID,false));

  SecurityDescriptors[2] := TJwSecurityDescriptor.Create();
  SecurityDescriptors[1].DACLInherited := false;
  SecurityDescriptors[2].DACLInherited := false;

//  SecurityDescriptors[2].Owner := JwUsersSID;//JwSecurityProcessUserSID;
//  SecurityDescriptors[2].OwnOwner := false; //do not free a standard SID !!

  SecurityDescriptors[2].DACL.Add(
    TJwDiscretionaryAccessControlEntryAllow.Create(
      nil,TJwAfThisFolderAndSubFoldersAndFiles,GENERIC_ALL,SecurityDescriptors[1].Owner,false));

  SecurityDescriptors[2].DACL.Add(
    TJwDiscretionaryAccessControlEntryAllow.Create(
      nil,[],GENERIC_READ,JwUsersSID,false));

  SecurityDescriptors[3] := TJwSecurityDescriptor.CreatePrivateObjectSecurity(
      SecurityDescriptors[1],//const ParentSecurityDescriptor: TJwSecurityDescriptor;
      SecurityDescriptors[2],//const CreatorSecurityDescriptor: TJwSecurityDescriptor;
      nil,
      false,//const IsDirectoryObject : Boolean;
      [ifDaclAutoInherit],
      TJwSecurityGenericMapping,//const GenericMap : TJwSecurityGenericMappingClass;
      nil//const Token : TObject = nil
  );

  ShowMessage( SecurityDescriptors[3].Text);

  Check(SecurityDescriptors[3].Owner.EqualSid(SecurityDescriptors[1].Owner));
  CheckEquals(3, SecurityDescriptors[3].DACL.Count);



end;

procedure TSecurityDescriptorTests.GetPrivateObjectSecurity;
begin
  SecurityDescriptors[1] := TJwSecurityDescriptor.Create();


  SecurityDescriptors[1].Owner := JwSecurityProcessUserSID;
  SecurityDescriptors[1].OwnOwner := false; //do not free a standard SID !!

  SecurityDescriptors[1].PrimaryGroup := JwSecurityProcessUserSID;//JwSecurityProcessUserSID;
  SecurityDescriptors[1].OwnPrimaryGroup := false; //do not free a standard SID !!


  SecurityDescriptors[1].DACL.Add(
    TJwDiscretionaryAccessControlEntryAllow.Create(
      nil,TJwAfThisFolderAndSubFoldersAndFiles,READ_CONTROL or WRITE_DAC or WRITE_OWNER,SecurityDescriptors[1].Owner,false));

  SecurityDescriptors[2] := SecurityDescriptors[1].GetPrivateObjectSecurity(
    []);
  Check(SecurityDescriptors[2].DACL = nil);
  //do more stuff here


end;

procedure TSecurityDescriptorTests.SetPrivateObjectSecurity;
begin
  SecurityDescriptors[1] := TJwSecurityDescriptor.Create();


  SecurityDescriptors[1].Owner := JwSecurityProcessUserSID;
  SecurityDescriptors[1].OwnOwner := false; //do not free a standard SID !!

  SecurityDescriptors[1].PrimaryGroup := JwSecurityProcessUserSID;//JwSecurityProcessUserSID;
  SecurityDescriptors[1].OwnPrimaryGroup := false; //do not free a standard SID !!


  SecurityDescriptors[1].DACL.Add(
    TJwDiscretionaryAccessControlEntryAllow.Create(
      nil,TJwAfThisFolderAndSubFoldersAndFiles,READ_CONTROL or WRITE_DAC or WRITE_OWNER,SecurityDescriptors[1].Owner,false));

  SecurityDescriptors[2] := TJwSecurityDescriptor.Create();
  SecurityDescriptors[2].Owner := JwPowerUsersSID;//JwSecurityProcessUserSID;
  SecurityDescriptors[2].OwnOwner := false; //do not free a standard SID !!

  SecurityDescriptors[2].PrimaryGroup := JwUsersSID;//JwSecurityProcessUserSID;
  SecurityDescriptors[2].OwnPrimaryGroup := false; //do not free a standard SID !!

 // SecurityDescriptors[1].DACLInherited := false;
 // SecurityDescriptors[2].DACLInherited := true;

 SecurityDescriptors[2].DACL.Add(
    TJwDiscretionaryAccessControlEntryAllow.Create(
      nil,[afInheritedAce],READ_CONTROL or WRITE_DAC or WRITE_OWNER,JwUsersSID,false));

 { SecurityDescriptors[2].DACL.Add(
    TJwDiscretionaryAccessControlEntryAllow.Create(
      nil,[],GENERIC_READ,JwUsersSID,false));    }
  // SecurityDescriptors[2].DACL := nil;


//  SecurityDescriptors[2].Control := [sdcDaclProtected];
  SecurityDescriptors[2].SetPrivateObjectSecurity(
      [siDaclSecurityInformation,
       siOwnerSecurityInformation],//const SecurityInformation : TJwSecurityInformationFlagSet;
      SecurityDescriptors[1],//const ModificationDescriptor : TJwSecurityDescriptor;
      [ifDaclAutoInherit],
      TJwSecurityGenericMapping//const GenericMap: TJwSecurityGenericMappingClass;
      //const Token: TObject = nil
      );


  ShowMessage( SecurityDescriptors[2].Text);

  Check(SecurityDescriptors[2].Owner.EqualSid(SecurityDescriptors[1].Owner)); //set by siOwnerSecurityInformation
  CheckEquals(2, SecurityDescriptors[2].DACL.Count);
end;

procedure TSecurityDescriptorTests.ReplaceDescriptorElements;
begin

end;

initialization

  TestFramework.RegisterTest('JwsclDescriptorTests Suite',
    TSecurityDescriptorTests.Suite);

end.
