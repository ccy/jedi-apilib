{
Description:
This example shows how to retrieve the owner of a file using either
plain WinAPI calls or JWSCL.
There is also a function GetFileOwnerJWSCL2 that uses newer JWSCL features.

Remarks:
You may need to adapt the folder at the end of file.

Created: 11th March 2010
Author : Christian Wimmer

References
http://blog.delphi-jedi.net/2010/03/11/retrieving-file-owner/
}
uses
  JwaWindows,
  //only for GetFileOwnerWinapi
  {
  JwaWinBase,
  JwaWinType,
  JwaWinNT,
  JwaAclApi,
  JwaAccCtrl,
  JwaWinError,
  JwaSddl,
  }
  JwsclExceptions,
  JwsclSecureObjects,
  JwsclSid,
  JwsclDescriptor,

  Dialogs,
  SysUtils;


type
  TOwnerResult = (orNone, orName, orSID);

//JWSCL implementation
function GetFileOwnerJWSCL(const FileName: string;
  out Domain, Username: String): TOwnerResult;
var
  F : TJwSecureFileObject;
  Owner : TJwSecurityId;
begin
  result := orNone;
  F := TJwSecureFileObject.Create(FileName);
  try
    Owner := F.Owner;
    try
      Domain := Owner.GetAccountDomainName();
      Username := Owner.GetAccountName();
      Result := orName;
    except
      on E : EJwsclWinCallFailedException do
      begin
        if E.LastError = ERROR_NONE_MAPPED then
        begin
          Domain := '';
          Username := Owner.StringSID;
          Result := orSID;
        end
        else
          raise;
      end;
    end;
  finally
    F.Free;
  end;
end;

{
This function only works with developer trunk or JWSCL >= 0.9.4
function GetFileOwnerJWSCL2(const FileName: string;
  out Domain, Username: String): TOwnerResult;
var
  F : TJwSecureFileObject;
  Owner : TJwSecurityId;
begin
  F := TJwSecureFileObject.Create(FileName);
  try
    Owner := F.Owner;
    try
      Domain := Owner.GetAccountDomainName();
      Username := Owner.GetAccountName();
      Result := orName;
    except
      on E : EJwsclSidNotMappedException do
      begin
        Domain := '';
        Username := Owner.StringSID;
        Result := orSID;
      end;
    end;
  finally
    F.Free;
  end;
end;
}

//plain WinAPI calls
function GetFileOwnerWinapi(const FileName: string;
  out Domain, Username: String): TOwnerResult;
var
  pSD: PSecurityDescriptor;
  dwOwnerNameSize, dwDomainNameSize: DWORD;
  pOwnerSID: PSID;
  pszOwnerName, pszDomainName: PChar;
  OwnerType: SID_NAME_USE;
  dwError : DWORD;
begin
  result := orNone;

  pSD := nil;
  dwError := GetNamedSecurityInfo(
     PChar(FileName),//__in       LPTSTR pObjectName,
     SE_FILE_OBJECT,//__in       SE_OBJECT_TYPE ObjectType,
     OWNER_SECURITY_INFORMATION,//__in       SECURITY_INFORMATION SecurityInfo,
     @pOwnerSID,//__out_opt  PSID *ppsidOwner,
     nil,//__out_opt  PSID *ppsidGroup,
     nil,//__out_opt  PACL *ppDacl,
     nil,//__out_opt  PACL *ppSacl,
     pSD//__out_opt  PSECURITY_DESCRIPTOR *ppSecurityDescriptor
   );
  if (dwError <> NOERROR) then
  begin
    SetLastError(dwError);
    RaiseLastOSError;
  end;

  //First get necessary memory size for Owner and Domain
  dwOwnerNameSize  := 0;
  dwDomainNameSize := 0;
  if not LookupAccountSID(nil, pOwnerSID, nil,
    dwOwnerNameSize, nil, dwDomainNameSize, OwnerType) then
  begin
    //Check for SIDs that are unknown on this local system
    if (GetLastError() = ERROR_NONE_MAPPED) then
    begin
      Win32Check(ConvertSidToStringSid(pOwnerSID, pszOwnerName));
      Domain := '';
      Username := pszOwnerName;
      LocalFree(DWORD(pszOwnerName));
      result := orSID;
      exit;
    end
    else
    //Any other error exits the function
    if (GetLastError() <> ERROR_INSUFFICIENT_BUFFER) then
      Exit;
  end;

  //Allocate memory for owner and domain
  //Take into account the size of a char type (like WideCHAR)
  GetMem(pszOwnerName, dwOwnerNameSize*sizeof(CHAR));
  try
    GetMem(pszDomainName, dwDomainNameSize*sizeof(CHAR));
    try
      //Retrieve name and domain
      Win32Check(LookupAccountSID(nil, pOwnerSID, pszOwnerName,
        dwOwnerNameSize, pszDomainName, dwDomainNameSize, OwnerType));

      result := orName;

      Domain   := pszDomainName;
      Username := pszOwnerName;
    finally
      FreeMem(pszDomainName);
    end;
  finally
    FreeMem(pszOwnerName);
  end;
end;


var
  Domain, User : Array[1..2] of string;
begin
  GetFileOwnerWinapi('C:\',Domain[1],User[1]);
  GetFileOwnerJWSCL('C:\',Domain[2],User[2]);

  ShowMessage(Format('GetFileOwnerWinapi: %s\%s'#13#10'GetFileOwnerJwscl: %s\%s',
    [Domain[1],User[1],
     Domain[2],User[2] ]));
end.
