{@abstract(Contains well known sids)
@author(Christian Wimmer)
@created(03/23/2007)
@lastmod(09/10/2007)

Project JEDI Windows Security Code Library (JWSCL)

The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the  
GNU Lesser General Public License (the  "LGPL License"), in which case the   
provisions of the LGPL License are applicable instead of those above.        
If you wish to allow use of your version of this file only under the terms   
of the LGPL License and not to allow others to use your version of this file 
under the MPL, indicate your decision by deleting  the provisions above and  
replace  them with the notice and other provisions required by the LGPL      
License.  If you do not delete the provisions above, a recipient may use     
your version of this file under either the MPL or the LGPL License.          
                                                                             
For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html 

The Original Code is JwsclKnownSid.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

Description:

}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclKnownSid;
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $
{$INCLUDE Jwscl.inc}

interface

uses SysUtils,
  jwaWindows, JwsclResource,
  JwsclSid, JwsclToken,
  JwsclTypes, JwsclExceptions,
  JwsclVersion, JwsclConstants, JwsclProcess,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
  TJwSecurityKnownSID = class(TJwSecurityId)
  public
     (*@Name frees a known security instance.
       Do not call!
     *)
    procedure Free;


    function IsStandardSID(const aSID: TJwSecurityId): boolean;
      reintroduce; overload; virtual;

    function IsStandardSID: boolean; reintroduce; overload; virtual;
  end;

   (*@Name is a class that describes the actually user that is running
    the actual thread or process.
    Because the user must not be neccessary the actual logged on user
    it is called that way.
    Use the var JwSecurityProcessUserSID to get its date. But don't free it.
   *)
  TJwSecurityThreadUserSID = class(TJwSecurityKnownSID)
  public
    constructor Create; overload;
    procedure Free;

    function IsStandardSID: boolean; overload; override;
  end;


const JwLowIL = 'S-1-16-4096';
      JwMediumIL = 'S-1-16-8192';
      JwHighIL = 'S-1-16-12288';
      JwSystemIL = 'S-1-16-16384';
      JwProtectedProcessIL = 'S-1-16-20480';
var
   JwIntegrityLabelSID : array[TJwIntegrityLabelType] of TJwSecurityKnownSID;
    {@Name defines the current user SID that started the process.
     You need to call JwInitWellknownSIDs before accessing this variable!
     Use this:
      SD : TJwSecurityDescriptor;
      ...
      SD.OwnOwner := false;
      SD.Owner := JwSecurityProcessUserSID;
      ...
      SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],FILE_EXECUTE,JwSecurityProcessUserSID, false)); //see?: false

    }
  JwSecurityProcessUserSID,


    {@Name defines the local administrator group
     Do not free!!
     You need to call JwInitWellknownSIDs before accessing this variable!

     Use this:
      SD : TJwSecurityDescriptor;
      ...
      SD.OwnOwner := false;
      SD.Owner := JwAdministratorsSID;
      ...
      SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],FILE_EXECUTE,JwSecurityCurrentThreadUserSID, false)); //see?: false

    }
  JwAdministratorsSID,
    {@Name defines the local user group
     You need to call JwInitWellknownSIDs before accessing this variable!
    
     Use this:
      SD : TJwSecurityDescriptor;
      ...
      SD.OwnOwner := false;
      SD.Owner := JwUsersSID;
      SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],FILE_EXECUTE,JwSecurityCurrentThreadUserSID, false)); //see?: false
    }
  JwUsersSID,
    {@Name defines the local power user group - legacy in Vista
     You need to call JwInitWellknownSIDs before accessing this variable!

     Use this:
      SD : TJwSecurityDescriptor;
      ...
      SD.OwnOwner := false;
      SD.Owner := JwPowerUsersSID;
      SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],FILE_EXECUTE,JwPowerUsersSID, false)); //see?: false
    }
  JwPowerUsersSID,
    {@Name defines the local guest group
     You need to call JwInitWellknownSIDs before accessing this variable!
    
     Use this:
      SD : TJwSecurityDescriptor;
      ...
      SD.OwnOwner := false;
      SD.Owner := JwGuestsSID;
      SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],FILE_EXECUTE,JwGuestsSID, false)); //see?: false
    }
  JwGuestsSID,
    {@Name defines the local system account
     You need to call JwInitWellknownSIDs before accessing this variable!

     Use this:
      SD : TJwSecurityDescriptor;
      ...
      SD.OwnOwner := false;
      SD.Owner := JwLocalSystemSID;
      SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],FILE_EXECUTE,JwLocalSystemSID, false)); //see?: false
    }
  JwLocalSystemSID,
    {@Name defines the group that allows remote interaction with the machine
     You need to call JwInitWellknownSIDs before accessing this variable!
    
     Use this:
      SD : TJwSecurityDescriptor;
      ...
      SD.OwnOwner := false;
      SD.Owner := JwRemoteInteractiveLogonSID;
      SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],FILE_EXECUTE,JwRemoteInteractiveLogonSID, false)); //see?: false
    }
  JwRemoteInteractiveLogonSID,
    {@Name defines the NULL Logon SID
     You need to call JwInitWellknownSIDs before accessing this variable!

     Use this:
      SD : TJwSecurityDescriptor;
      ...
      SD.OwnOwner := false;
      SD.Owner := JwNullSID;
      SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],FILE_EXECUTE,JwNullSID, false)); //see?: false
    }
  JwNullSID,
    {@Name defines the Everybody group
     You need to call JwInitWellknownSIDs before accessing this variable!

     Use this:
      SD : TJwSecurityDescriptor;
      ...
      SD.OwnOwner := false;
      SD.Owner := JwWorldSID;
      SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],FILE_EXECUTE,JwWorldSID, false)); //see?: false
    }
  JwWorldSID,
    {@Name defines the local group
     You need to call JwInitWellknownSIDs before accessing this variable!

     Use this:
      SD : TJwSecurityDescriptor;
      ...
      SD.OwnOwner := false;
      SD.Owner := JwLocalGroupSID;
      SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],FILE_EXECUTE,JwLocalGroupSID, false)); //see?: false
    }
  JwLocalGroupSID: TJwSecurityKnownSID;

//    LocalAdministratorSID : TJwSecurityKnownSID;

{@Name gets the current owner or impersonated thread owner of the current thread
that is used to call this function.
The caller is responsible to free the SecurityID instance.

Use this:
SD : TJwSecurityDescriptor;
...
SD.OwnOwner := true;
SD.Owner := JwSecurityCurrentThreadUserSID;


SD.DACL.Add(TJwDiscretionaryAccessControlEntryDeny.Create(nil,[],FILE_EXECUTE,JwSecurityCurrentThreadUserSID, true)); //see?: true

}
function JwSecurityCurrentThreadUserSID: TJwSecurityThreadUserSID;


{@Name gets the logon SID of the given window station.


Remark:
This function is useful if a new user must have access to the given windows station
 so she can create windows. One do not have to add the new user to the window station DACL.
This does not work if the user is not logged on as an interactive user.

@param hWinStation defines the window station that is used. If 0 the window station "WinSta0" is used. 
@return The logon SID.
@raises EJwsclWinCallFailedException will be raised if a call to GetUserObjectInformation failed.
}
function JwGetLogonSID(const hWinStation: HWINSTA{TWindowStation} = 0)
  : TJwSecurityId; overload;

function JwGetLogonSID(aToken: TJwSecurityToken): TJwSecurityId; overload;

{@Name initializes the WellKnownSID variables.
 This function should not be called during initialization of
 a Jwscl-unit since it indirectly accesses various global variables,
 e.g. JwProcessHeap, which might not have been initialized yet.
}
procedure JwInitWellKnownSIDs;


{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

uses Classes;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}


var
  OnFinalization: boolean = False;
//fSecurityCurrentThreadUserSID : TJwSecurityThreadUserSID = nil;


function JwGetLogonSID(aToken: TJwSecurityToken): TJwSecurityId;
var
  i: integer;
  ptg: TJwSecurityIdList;
begin
  Result := nil;
  ptg := aToken.GetTokenGroups;

  // Loop through the groups to find the logon SID.
  for i := 0 to ptg.Count - 1 do
  begin
    if (ptg[i].Attributes and SE_GROUP_LOGON_ID) = SE_GROUP_LOGON_ID then
    begin
      // Found the logon SID; make a copy of it.
      Result := TJwSecurityId.Create(ptg[i].CreateCopyOfSID);
      Break;
    end;
  end;
end;

function JwGetLogonSID(const hWinStation: HWINSTA{TWindowStation})
: TJwSecurityId;
  {TODO: Use TWindowStation (if implemented) instead}
var
  hAWinst: HWINSTA;
  logonSID: PSID;
  dwSize: Cardinal;
begin
  haWinst := hWinStation;
  if (hWinStation = 0) or (hWinStation = INVALID_HANDLE_VALUE) then
    hAWinst := OpenWindowStation('winsta0',
      False, READ_CONTROL
      //READ_CONTROL or WRITE_DAC
      );
  Result := nil;

  if (hWinStation = 0) or (hWinStation = INVALID_HANDLE_VALUE) then
    exit;

  if not GetUserObjectInformation(hAWinst, UOI_USER_SID, nil, 0, dwSize) then
  begin
    // GetUserObjectInformation returns required size
    GetMem(LogonSid, dwSize + 1);
    if not GetUserObjectInformation(hAWinst, UOI_USER_SID,
      LogonSid, dwSize, dwSize) then
    begin
      raise EJwsclWinCallFailedException.CreateFmtWinCall(
        RsWinCallFailed,
        'JwGetLogonSID', '', RsUNKnownSid,
        0, True, 'GetUserObjectInformation', ['GetUserObjectInformation']);
    end;
    if logonSID <> nil then
    begin
      Result := TJwSecurityId.Create(logonSID);
      FreeMem(logonSID);
    end;
  end;

  if (hWinStation = 0) and (hWinStation = INVALID_HANDLE_VALUE) then
    CloseWindowStation(hAWinst);
end;




function JwSecurityCurrentThreadUserSID: TJwSecurityThreadUserSID;
begin
  //if Assigned(fSecurityCurrentThreadUserSID) then
  //  fSecurityCurrentThreadUserSID.Free;

  //fSecurityCurrentThreadUserSID := TJwSecurityThreadUserSID.Create;
  //result := fSecurityCurrentThreadUserSID;
  Result := TJwSecurityThreadUserSID.Create;
end;

{ TJwSecurityThreadUserSID }

constructor TJwSecurityThreadUserSID.Create;
var
  token: TJwSecurityToken;
  s: TJwSecurityId;
begin
  token := TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);
  S := nil;
  try
    S := token.GetTokenUser;
    inherited Create(S);
  finally
    token.Free;
    S.Free;
  end;
end;

procedure TJwSecurityThreadUserSID.Free;
begin
  //if (not OnFinalization) and (JwSecurityProcessUserSID = Self) then
  //  raise EJwsclSecurityException.CreateFmtEx('Call to Free failed, because the var JwSecurityProcessUserSID cannot be freed manually.','Free',ClassName,'JwsclKnownSid.pas',0,false,[]);

  inherited;
end;

function TJwSecurityThreadUserSID.IsStandardSID: boolean;
begin
  Result := False;
end;

{ TJwSecurityKnownSID }

procedure TJwSecurityKnownSID.Free;
begin
  inherited;
end;

function TJwSecurityKnownSID.IsStandardSID(const aSID: TJwSecurityId): boolean;
begin
  Result := (aSID.ClassType = TJwSecurityThreadUserSID) or
    (aSID.ClassType = TJwSecurityThreadUserSID);
end;

procedure JwInitWellKnownSIDs;
begin
  if not Assigned(JwAdministratorsSID) then
    JwAdministratorsSID := TJwSecurityKnownSID.Create('S-1-5-32-544');
  if not Assigned(JwUsersSID) then
    JwUsersSID := TJwSecurityKnownSID.Create('S-1-5-32-545');
  if not Assigned(JwGuestsSID) then
    JwGuestsSID := TJwSecurityKnownSID.Create('S-1-5-32-546');
  if not Assigned(JwPowerUsersSID) then
    JwPowerUsersSID := TJwSecurityKnownSID.Create('S-1-5-32-547');
  if not Assigned(JwLocalSystemSID) then
    JwLocalSystemSID := TJwSecurityKnownSID.Create('S-1-5-18');
  if not Assigned(JwRemoteInteractiveLogonSID) then
    JwRemoteInteractiveLogonSID := TJwSecurityKnownSID.Create('S-1-5-14');
  if not Assigned(JwNullSID) then
    JwNullSID := TJwSecurityKnownSID.Create('S-1-0-0');
  if not Assigned(JwWorldSID) then
    JwWorldSID := TJwSecurityKnownSID.Create('S-1-1-0');
  if not Assigned(JwLocalGroupSID) then
    JwLocalGroupSID := TJwSecurityKnownSID.Create('S-1-2-0');

  if not Assigned(JwSecurityProcessUserSID) then
    JwSecurityProcessUserSID := TJwSecurityThreadUserSID.Create;

  JwIntegrityLabelSID[iltNone]       := nil;
  if not Assigned(JwIntegrityLabelSID[iltLow]) then
    JwIntegrityLabelSID[iltLow]       := TJwSecurityKnownSID.Create(JwLowIL);
  if not Assigned(JwIntegrityLabelSID[iltMedium]) then
    JwIntegrityLabelSID[iltMedium]    := TJwSecurityKnownSID.Create(JwMediumIL);
  if not Assigned(JwIntegrityLabelSID[iltHigh]) then
    JwIntegrityLabelSID[iltHigh]      := TJwSecurityKnownSID.Create(JwHighIL);
  if not Assigned(JwIntegrityLabelSID[iltSystem]) then
    JwIntegrityLabelSID[iltSystem]    := TJwSecurityKnownSID.Create(JwSystemIL);
  if not Assigned(JwIntegrityLabelSID[iltProtected]) then
    JwIntegrityLabelSID[iltProtected] := TJwSecurityKnownSID.Create(JwProtectedProcessIL);
end;

procedure DoneWellKnownSIDs;
var ilts : TJwIntegrityLabelType;
begin

  FreeAndNil(JwAdministratorsSID);
  FreeAndNil(JwUsersSID);
  FreeAndNil(JwGuestsSID);
  FreeAndNil(JwPowerUsersSID);
  FreeAndNil(JwLocalSystemSID);
  FreeAndNil(JwRemoteInteractiveLogonSID);
  FreeAndNil(JwNullSID);
  FreeAndNil(JwWorldSID);
  FreeAndNil(JwLocalGroupSID);
  FreeAndNil(JwSecurityProcessUserSID);
  //  FreeAndNil(fSecurityCurrentThreadUserSID);

  for ilts := low(TJwIntegrityLabelType) to high(TJwIntegrityLabelType) do
    FreeAndNil(JwIntegrityLabelSID[ilts]);
end;

function TJwSecurityKnownSID.IsStandardSID: boolean;
begin
  Result := True;
end;

{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
initialization
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INITIALIZATION_SECTION}
  JwSecurityProcessUserSID := nil;
  JwAdministratorsSID := nil;
  JwUsersSID := nil;
  JwPowerUsersSID := nil;
  JwGuestsSID := nil;
  JwLocalSystemSID := nil;
  JwRemoteInteractiveLogonSID := nil;
  JwNullSID  := nil;
  JwWorldSID := nil;
  JwLocalGroupSID := nil;

{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
finalization
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_FINALIZATION_SECTION}
  OnFinalization := True;
  DoneWellKnownSIDs;
{$ENDIF SL_FINALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}