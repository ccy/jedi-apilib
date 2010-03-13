{
Description:
This example shows how to simulate the WinAPI function GetEffectiveRightsFromACL 
using MS AuthZ (JwsclAuthCthx.pas).

Remarks:
You need to adapt the constants Target and UserName accordingly.

Created: 13th March 2010
Author : Christian Wimmer

References
http://blog.delphi-jedi.net/2010/03/13/what-is-geteffectiverightsfromacl-for-2
}
uses
  JwaWindows,

  JwsclKnownSid,
  JwsclConstants,
  JwsclTypes,
  JwsclExceptions,
  JwsclSecureObjects,
  JwsclSid,
  JwsclDescriptor,
  JwsclAuthCtx,
  JwsclMapping,
  JwsclUtils,
  JwsclACL,

  Dialogs,
  SysUtils;
  
 const
  Target = 'C:\Windows';
  UserName = 'Christian';


//computes granted access mask for a security descriptor and a SID
//An error is thrown by an exception - typical JWSCL
function GetEffectiveAccess(const SD : TJwSecurityDescriptor;
                            const Sid : TJwSecurityId) : DWORD;
var
  AuthRM : TJwAuthResourceManager;
  AuthCtx : TJwAuthContext;
  AuthReply : TJwAuthZAccessReply;
  Request : TJwAuthZAccessRequest;
begin
  //The following code uses the MS AuthZ API
  //JWSCL wraps the function calls
  AuthRM := TJwAuthResourceManager.Create('', [], nil, nil);
  try
    //Tell AuthZ to use a SID for access checking
    //We also could use a token and prevent the problem of
    // restricted Administrators in Vista/7
    AuthCtx := TJwAuthContext.CreateBySid(AuthRM, [], Sid, 0, nil);
    try
      //As documented AccessCheck returns a GrantedAccessMask
      //  when using MAXIMUM_ALLOWED
      Request := TJwAuthZAccessRequest.Create(MAXIMUM_ALLOWED,
                  JwNullSID, nil, nil, shOwned);
      try
        //does the access check using a generic mapping to file/folder
        AuthCtx.AccessCheck(0, Request, 0, SD, nil,
           TJwSecurityFileFolderMapping, AuthReply, nil);

        //return the granted access mask
        result := AuthReply.GrantedAccessMask[0];
      finally
        Request.Free;
      end;
    finally
      AuthCtx.Free;
    end;
  finally
    AuthRM.Free;
  end;
end;

//computes granted access mask for a file and username
//An error is thrown by an exception - typical JWSCL
function GetEffectiveAccessFromFile(
             const FileName,
             DomainName, UserName : string) : DWORD;
var
  F : TJwSecureFileObject;
  SD : TJwSecurityDescriptor;
  Flags : TJwSecurityInformationFlagSet;
  Sid : TJwSecurityId;
begin
  //Translate username and domainname into a SID
  //Throws an EJwsclWinCallFailedException if
  //  the username could not be translated
  Sid := TJwSecurityId.Create(DomainName, UserName);
  try
    F := TJwSecureFileObject.Create(FileName);
    try
      //adding siLabelSecurityInformation makes no sense
      //  since it cannot be checked against a simple SID
      Flags := [siOwnerSecurityInformation,
                siGroupSecurityInformation,
                siDaclSecurityInformation];

      //get the security descriptor (result is not cached)
      SD := F.GetSecurityDescriptor(Flags);
      try
        result := GetEffectiveAccess(SD, Sid);
      finally
        SD.Free;
      end;
    finally
      F.Free;
    end;
  finally
    Sid.Free;
  end;
end;


var AccessMask : TJwAccessMask;
begin
  JwInitWellKnownSIDs;

  AccessMask := GetEffectiveAccessFromFile(Target, '', UserName);
  ShowMessage(Format('File/Folder: %s'#13#10'User: %s'#13#10'%s',
    [Target,
     UserName,
     JwFormatAccessRights(AccessMask, FileFolderMapping)]));
end.
