{@abstract(Contains Windows account control classes - Not implemented yet)
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

The Original Code is JwsclAccounts.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.


Description:
Not implemented yet


}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclAccounts;
{$INCLUDE Compilers.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $
//do not move header comment from above unit declaration!

interface

uses
  SysUtils, Contnrs, Classes,
  jwaWindows,
  JwsclTypes, JwsclExceptions, JwsclAcl,
  JwsclVersion, JwsclConstants, JwsclProcess,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
  {@Name is not implemented yet}
  TJwUserAccounts = class(TObjectList)
  private

  protected

  public
    //constructor Create; override;
    //destructor Destroy; override;
    //Alle Benutzer eines Systems
    //constructor CreateBySystemAccounts(const System: TJwString);

  end;

  {@Name is not implemented yet}
  TJwGroupAccounts = class(TObjectList)
  private

  protected

  public
    //constructor Create; override;
    //destructor Destroy; override;
    //Alle Gruppen eines Systems
    //constructor CreateBySystemAccounts(const System: TJwString);
  end;

  {@Name is not implemented yet}
  TJwUserAccount = class(TObject)
  private

  protected

  public
    //gehoert der Benutzer zu einer Gruppe?
    //function BelongsToGroup(const GroupID: TJwGroupAccount);
    //function BelongsToGroup(const GroupSID: TJwSecurityId);
    //function IsAdministrator;
    //function IsService;

    //erstellt Benutzerinformation über den Benutzer, der den Prozess aufgerufen hat
    //constructor CreateByLoggedInProcessUser;

    //constructor Create; override;
    //destructor Destroy; override;

    //property SID: TJwSecurityId;

    //property Groups: TJwSecurityIdList;
    //property UserToken: TToken;
  end;

  {@Name is not implemented yet}
  TJwGroupAccount = class(TObject)
  public
    //gehoert der Benutzer zu einer Gruppe?
    //function BelongsToUser(const User: TJwUserAccount);
    //function BelongsToUser(const UserSID: TJwSecurityId);
  public
    //constructor Create; override;
    //destructor Destroy; override;

    //property SID: TJwSecurityId;

    //property User: TJwSecurityIdList;
    //property GroupToken: TToken;
  end;


{TODO: do testing!} 
{@Name checks whether a given account on a given computer or domain
has an empty password.

This function does not raise exceptions!

@param(UserName defines the user account that is checked for an empty password)
@param(ComputerDomainName defines the computer or domain name that is used
for the user account.
WARNING: This parameter was not tested!)
@return(@Name returns true if the password is empty; otherwise false. It also
returns false if an error has occoured.)
}
function JwHasAccountPassword(
    const UserName : TJwString;
    const ComputerDomainName : TJwString = '') : Boolean;


{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

{origin: Assarbad
converted from C by Christian Wimmer}
function JwHasAccountPassword(
    const UserName : TJwString;
    const ComputerDomainName : TJwString = '') : Boolean;
var hToken : Cardinal;
    dwErr : DWORD;
    res : BOOL;
begin
  hToken := 0;
  res := {$IFDEF UNICODE}LogonUserW{$ELSE}LogonUserA{$ENDIF}
     (TJwPChar(Username), TJwPChar(ComputerDomainName), '', 2, 0, hToken);

  if res then
    CloseHandle(hToken);

  dwErr := GetLastError();

  result := (res) or (ERROR_ILL_FORMED_PASSWORD = dwErr);
end;


{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
initialization
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INITIALIZATION_SECTION}
{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}