{This example adds Everyone group (full control) to the ACL of a folder.
It shows how to handle CreateDirectory and security inheritance.
}
program SetFileSecurity3;

uses
  JwaWindows,
  JwsclDescriptor,
  JwsclTypes,
  JwsclConstants,
  JwsclKnownSid,
  JwsclAcl,
  JwsclMapping,
  JwsclSecureObjects,
  JwsclSid,
  Dialogs,
  SysUtils;

const
  ParentFolder = '.';
  JWSCLTestFolder = ParentFolder+'\JWSCLTestFolder';


procedure SetFolder1;
var
  SD : TJwSecurityDescriptor;
  pSA : PSecurityAttributes;
begin
  JwInitWellKnownSIDs;

  SD := TJwSecurityDescriptor.Create;
  try
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,
      [afContainerInheritAce, afObjectInheritAce], FILE_ALL_ACCESS, JwWorldSID));

    pSA := SD.Create_SA();
    try
      Win32Check(CreateDirectory(JWSCLTestFolder, pSA));
    finally
      SD.Free_SA(pSA); //remember to free pointer
    end;
  finally
    SD.Free;
  end;
end;

procedure SetFolder2;
procedure MergeParentDACL(const Location : String; TargetSD : TJwSecurityDescriptor);
var DirSD : TJwSecureFileObject;
begin
  DirSD := TJwSecureFileObject.Create(Location);
  try
    TargetSD.DACL.AddACEs(DirSD.DACL);
  finally
    DirSD.Free;
  end;
end;

var
  DirSD : TJwSecureFileObject;

  SD, SD2 : TJwSecurityDescriptor;
  pSA : PSecurityAttributes;
begin
  JwInitWellKnownSIDs;

  SD := TJwSecurityDescriptor.Create;

  try
    SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,
      [afContainerInheritAce, afObjectInheritAce], FILE_ALL_ACCESS, JwWorldSID));

    MergeParentDACL(ParentFolder, SD);

    pSA := SD.Create_SA();
    try
      Win32Check(CreateDirectory(JWSCLTestFolder, pSA));
    finally
      SD.Free_SA(pSA);
    end;
  finally
    SD.Free;
  end;
end;

procedure SetFolder3;
var
  SD : TJwSecurityDescriptor;
  DirSD : TJwSecureFileObject;
begin
  JwInitWellKnownSIDs;

  Win32Check(CreateDirectory(JWSCLTestFolder, nil));

  DirSD := TJwSecureFileObject.Create(JWSCLTestFolder);
  try
    SD := DirSD.GetSecurityDescriptor([siDaclSecurityInformation]);
    try
      SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil,
        [afContainerInheritAce, afObjectInheritAce], FILE_ALL_ACCESS, JwWorldSID));

      DirSD.SetSecurityDescriptor(SD, [siDaclSecurityInformation]);
    finally
      SD.Free;
    end;
  finally
    DirSD.Free;
  end;
end;

procedure ShowFolderSecurity;
var
  DirSD : TJwSecureFileObject;
  SD : TJwSecurityDescriptor;
begin
  DirSD := TJwSecureFileObject.Create(JWSCLTestFolder);
  try
    SD := DirSD.GetSecurityDescriptor(JwSecurityInformationACLFlagsOD);
    try
      ShowMessage(SD.GetTextMap(TJwSecurityFileFolderMapping));
    finally
      SD.Free;
    end;
  finally
    DirSD.Free;
  end;
end;



begin
  JwInitWellKnownSIDs;

  Win32Check(RemoveDirectory(JWSCLTestFolder));
  SetFolder1;
  ShowFolderSecurity;

  Win32Check(RemoveDirectory(JWSCLTestFolder));
  SetFolder2;
  ShowFolderSecurity;

  Win32Check(RemoveDirectory(JWSCLTestFolder));
  SetFolder2;
  ShowFolderSecurity;


end.


