{ Description
  Project JEDI Windows Security Code Library (JWSCL)

  Contains Windows account control classes - Not implemented yet

  <b>Not implemented yet</b>
  Author
  Christian Wimmer
  License
  The contents of this file are subject to the Mozilla Public License Version 1.1
  (the "License"); you may not use this file except in compliance with the
  \License. You may obtain a copy of the License at http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
  specific language governing rights and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the GNU
  Lesser General Public License (the "LGPL License"), in which case the provisions
  of the LGPL License are applicable instead of those above. If you wish to allow
  use of your version of this file only under the terms of the LGPL License and
  not to allow others to use your version of this file under the MPL, indicate
  your decision by deleting the provisions above and replace them with the notice
  and other provisions required by the LGPL License. If you do not delete the
  provisions above, a recipient may use your version of this file under either the
  MPL or the LGPL License.

  For more information about the LGPL: <i>http://www.gnu.org/copyleft/lesser.html</i>


  Note
  The Original Code is JwsclAccounts.pas.

  The Initial Developer of the Original Code is Christian Wimmer. Portions created
  by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.
                                                                                      }
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclAccounts;
{$INCLUDE ..\includes\Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $
//do not move header comment from above unit declaration!

interface

uses
  SysUtils, Contnrs,
  jwaWindows,
  JwsclTypes, JwsclExceptions, JwsclAcl, JwsclSid,
  JwsclVersion, JwsclConstants,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_IMPLEMENTATION_SECTION}

const
  IDXN_Name     = 1;
  IDXN_FullName = 2;
  IDXN_Comment   = 3;
  IDXN_HomeDirectory = 4;
  IDXN_Parameters   = 5;
  IDXN_LogonServer = 6;
  IDXN_ProfilePath = 7;
  IDXN_HomeDirDrive = 8;
  IDXN_UserComment = 9;
  IDXN_ScriptPath = 10;

  IDXN_Flags = 101;
  IDXN_PrivilegeLevel = 102;
  IDXN_AuthFlagLevel = 102;


  IDXN_BadPasswordCount = 201;
  IDXN_LogonCount       = 202;
  IDXN_CountryCode      = 203;
  IDXN_MaxStorage       = 204;
  IDXN_UnitsPerWeek     = 205;
  IDXN_CodePage         = 206;

  IDXN_PrimaryGroupdSid = 301;
  IDXN_SID              = 302;

  IDXN_PasswordAge      = 401;
  IDXN_LastLogoff       = 402;
  IDXN_AccountExpires   = 403;

  IDXN_PasswordExpired = 501;

type

  TJwUserAccount = class;

  {<B>TJwUserAccounts</B> is not implemented yet}

  TJwUserAccounts = class(TObjectList)
  private

  protected
    fUsers : TObjectList;
    fServerName : TJwString;
   // function GetUser(Index : Integer) : TJwUserAccount;
  public
   // constructor Create(const ServerName : TJwString = '');

    //constructor Create; override;
   // destructor Destroy; override;

   // procedure Update; virtual;

   // property User[Index : Integer] : TJwUserAccount read GetUser;

   // property ServerName : TJwString read fServerName;
  end;

  {<B>TJwGroupAccounts</B> is not implemented yet}
  TJwGroupAccounts = class(TObjectList)
  private

  protected

  public
    //constructor Create; override;
    //destructor Destroy; override;
    //Alle Gruppen eines Systems
    //constructor CreateBySystemAccounts(const System: TJwString);
  end;


  TJwUserAccountType = (
    uatUnknown,
    uatNormal, //UF_NORMAL_ACCOUNT
    uatTempDuplicate,//UF_TEMP_DUPLICATE_ACCOUNT
    uatWorkstationTrust,//UF_WORKSTATION_TRUST_ACCOUNT
    uatServerTrust,//UF_SERVER_TRUST_ACCOUNT
    uatInterDomainTrust//UF_INTERDOMAIN_TRUST_ACCOUNT
    );

  TJwWorkStations = array of WideString;

  TJwWeekDay = (wdMonday, wdTuesday, wdWednesday, wdThursday, wdFriday, wdSaturday, wdSunday);

  TJwLogonHours = array[TJwWeekDay, 0..24] of Boolean;

  TJwUserInfoLevel = Byte;
  TJwByteSet = set of TJwUserInfoLevel;



  {<B>TJwUserAccount</B> is not implemented yet}
  TJwUserAccount = class(TObject)
  private

  public
  //protected
    fLevel : TJwUserInfoLevel; //level für infos

    fUserName,
    fServerName : TJwString;

    fName,
    fComment,
    fUserComment, //11
    fFullName : TJwString;

    fFlags : DWORD;
    fAccountType : TJwUserAccountType;
    fSID : TJwSecurityID;

    //11
    fPrivilegeLevel : DWORD; //  DWORD usri11_priv;
    fAuthFlagLevel : DWORD; // DWORD usri11_auth_flags;
    fPasswordAge : TDateTime;// DWORD usri11_password_age;
    fHomeDirectory : TJwString; // LPWSTR usri11_home_dir;
    fParameters : TJwString; // LPWSTR usri11_parms;
    fLastLogon, //DWORD usri11_last_logon;
    fLastLogoff : TDateTime; //DWORD usri11_last_logoff;
    fBadPasswordCount : DWORD; //  DWORD usri11_bad_pw_count;
    fLogonCount : DWORD; //DWORD usri11_num_logons;
    fLogonServer : TJwString; // LPWSTR usri11_logon_server;
    fCountryCode : DWORD; //DWORD usri11_country_code;
    fWorkstations : TJwWorkStations; //LPWSTR usri11_workstations;
    fMaxStorage : DWORD; //DWORD usri11_max_storage;
    fUnitsPerWeek : DWORD; // DWORD usri11_units_per_week;
    fLogonHours : TJwLogonHours; //PBYTE usri11_logon_hours;
    fCodePage : DWORD; //DWORD usri11_code_page;
    fScriptPath : TJwString; //usri1_script_path;


    //4
    fAccountExpires : TDateTime; //DWORD usri4_acct_expires;
    //DWORD usri4_num_logons;
    //LPWSTR usri4_logon_server;
    //DWORD usri4_country_code;
    //DWORD usri4_code_page;
    //PSID usri4_user_sid;
    fPrimaryGroupdSid : TJwSecurityId; //DWORD usri4_primary_group_id;
    fProfilePath : TJwString; //LPWSTR usri4_profile;
    fHomeDirDrive : TJwString;//LPWSTR usri4_home_dir_drive;
    fPasswordExpired : Boolean; //DWORD usri4_password_expired;

    function GetSID(Index : Integer) : TJwSecurityID;
    function GetDWORD(Index : Integer) : DWORD;
    function GetBoolean(Index : Integer) : Boolean;
    function GetDateTime(Index : Integer) : TDateTime;
    function GetString(Index : Integer) : TJwString;
    function GetWorkStations(Index : Integer) : TJwWorkStations;
    function GetLogonHours(Index : Integer) : TJwLogonHours;
    function GetAccountType(Index : Integer) : TJwUserAccountType;
  public
    constructor Create;

    procedure Update(const Level : TJwUserInfoLevel = 0);

    property Level : TJwUserInfoLevel read fLevel;
    property Name : TJwString     index IDXN_Name read GetString;
    property FullName : TJwString index IDXN_FullName read GetString;
    property Comment : TJwString  index IDXN_Comment read GetString;
    property UserComment : TJwString  index IDXN_UserComment read GetString;


    property HomeDirectory : TJwString  index IDXN_HomeDirectory read GetString;
    property Parameters : TJwString  index IDXN_Parameters read GetString;
    property LogonServer : TJwString  index IDXN_LogonServer read GetString;
    property ProfilePath : TJwString  index IDXN_ProfilePath read GetString;
    property HomeDirDrive : TJwString  index IDXN_HomeDirDrive read GetString;

    property Flags : DWORD index IDXN_Flags read GetDWORD;
    property PrivilegeLevel : DWORD index IDXN_PrivilegeLevel read GetDWORD;
    property AuthFlagLevel : DWORD index IDXN_AuthFlagLevel read GetDWORD;

    property BadPasswordCount : DWORD index IDXN_BadPasswordCount read GetDWORD;
    property LogonCount : DWORD index IDXN_LogonCount read GetDWORD;
    property CountryCode : DWORD index IDXN_CountryCode read GetDWORD;
    property MaxStorage : DWORD index IDXN_MaxStorage read GetDWORD;
    property UnitsPerWeek : DWORD index IDXN_UnitsPerWeek read GetDWORD;
    property CodePage : DWORD index IDXN_CodePage read GetDWORD;

    property SID : TJwSecurityID index IDXN_Sid read GetSID;
    property PrimaryGroupdSid : TJwSecurityID index IDXN_PrimaryGroupdSid read GetSID;

    property PasswordAge : TDateTime index IDXN_PasswordAge read GetDateTime;
    property LastLogoff : TDateTime index IDXN_LastLogoff read GetDateTime;
    property AccountExpires : TDateTime index IDXN_AccountExpires read GetDateTime;

    property PasswordExpired : Boolean index IDXN_PasswordExpired read GetBoolean;

    property ScriptPath : TJwString index IDXN_ScriptPath read GetString;

  end;



type
  {<B>TJwGroupAccount</B> is not implemented yet}
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

  TJwNetServer = class(TInterfacedObject)

  public
//    constructor Create(const ServerName : TJwString = '');



  end;


{TODO: do testing!}
{!<B>JwHasAccountPassword</B> checks whether a given account on a given computer or domain
has an empty password.

This function does not raise exceptions!

@param UserName defines the user account that is checked for an empty password
@param ComputerDomainName defines the computer or domain name that is used
for the user account.
WARNING: This parameter was not tested!
@return <B>JwHasAccountPassword</B> returns true if the password is empty; otherwise false. It also
returns false if an error has occoured.
}
function JwHasAccountPassword(
    const UserName : TJwString;
    const ComputerDomainName : TJwString = '') : Boolean;


{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

uses Variants;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}


{
Level Table for available TJwUserAccount properties
                                        0    1    2    3    4   10   11   20   21   22   23
Name – usri11_name                      x    x    x    x    x    x    x    x         x    x
Comment - usri11_comment;                    x    x    x    x    x    x    x         x    x
UserComment – usri11_usr_comment;                 x    x    x    x    x              x
FullName – usri11_full_name;                      x    x    x    x    x    x         x    x
Flags – usri23_flags;                        x    x    x    x              x         x    x
AccountType – fFlags                         x    x    x    x              x              x
SID -  usri23_user_sid;                                x    x              x              x
privilegeLevel – usri11_priv;                x    x    x    x         x              x
AuthFlagLevel – usri11_auth_flags;                x    x    x         x              x
PasswordAge – usri11_password_age;           x    x    x    x         x              x
HomeDirectory – usri11_home_dir;             x    x    x    x         x              x
Parameters – usri11_parms;                        x    x    x         x              x
lastLogon – usri11_last_logon;                    x    x    x         x              x
lastLogoff – usri11_last_logoff;                  x    x    x         x              x
BadPasswordCount - usri11_bad_pw_count            x    x    x         x              x
logonCount – usri11_num_logons;                   x    x    x         x              x
logonServer – usri11_logon_server;                x    x    x         x              x
countryCode – usri11_country_code;                x    x    x         x              x
workstations – usri11_workstations;               x    x    x         x              x
MaxStorage – usri11_max_storage;                  x    x    x         x              x
unitsPerWeek – usri11_units_per_week;             x    x    x         x              x
logonHours – usri11_logon_hours;                  x    x    x         x              x
codePage – usri11_code_page;                      x    x    x         x              x
accountExpires – usri4_acct_expires;              x    x    x                        x
primaryGroupdSid – usri4_primary_group                 x    x
profilePath – usri4_profile;                           x    x
homeDirDrive – usri4_home_dir_drive;                   x    x
passwordExpired – usri4_password_expir                 x    x
ScriptPath - usri1_script_path;              x         x                   x         x
}


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


  dwErr := GetLastError();

  if res then
    CloseHandle(hToken);

  result := (res) or (ERROR_ILL_FORMED_PASSWORD = dwErr);
end;


{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
{ TJwUserAccount }

procedure CheckSupportedLevels(const Level : TJwUserInfoLevel; const Str : String; const SupportLevels : TJwByteSet);
begin
  if not (Level in SupportLevels) then
    raise Exception.CreateFmt('Current Level %d does not support "%s" property',[Integer(Level), Str]);
end;

function GetVal(const Level : TJwUserInfoLevel; Index : Integer; const B : array of const) : Variant;
var i,i2 : Integer;
  S : TJwByteSet;
  Str : String;
begin
  if B[Low(B)].VVariant <> nil then
    result := B[Low(B)].VVariant^
  else
    result := Null;


  i := Low(B)+1;
  while i < High(B) do
  begin
    begin
      //get a set of allowed levels
      S := [];
      i2 := i+2;
      while (i2 < High(B)) and (B[i2].VType = vtInteger) and (B[i2].VInteger >= 0) do
      begin
        Include(S, TJwUserInfoLevel(B[i2].VInteger));
        Inc(i2);
      end;

      //get in here only if the index is correct
      if (i2 <= High(B)) and (B[i2].VType in [vtString, vtAnsiString]) and
         (B[i].VInteger = Index) then
      begin
        if not (Level in S) then
        begin
          case B[i2].VType of
            vtAnsiString : Str := AnsiString(B[i2].VAnsiString);
            vtWideString : Str := WideString(B[i2].VWideString);
            vtString : Str := B[i2].VString^;
          else
            Str := 'unknown';
          end;

          CheckSupportedLevels(Level, Str, S);
        end;

        {case B[i+1].VType of
          vtAnsiString : result := AnsiString(B[i+1].VAnsiString);
          vtWideString : result := WideString(B[i+1].VWideString);
          vtString : result := B[i+1].VString^;
        end;  }
        result := B[i+1].VVariant^;
      end;

      if B[i].VInteger = Index then
        Break
      else
        i := i2+1; //get to the index after the property name index
    end;
  end;

  if result = null then
    raise Exception.CreateFmt('Could not find property for index %d',[Index]);
end;


constructor TJwUserAccount.Create;
begin
  fLevel := 4;
  fName := 'fName';
  fFullName := 'fFullName';
  fComment := 'fComment';
  fFlags := 2;
  fPrivilegeLevel := 3;
  //fPrimaryGroupdSid := TJwSecurityId.Create('','Christian');
  fLastLogoff := Now;
end;

function TJwUserAccount.GetAccountType(Index: Integer): TJwUserAccountType;
begin
  CheckSupportedLevels(Level, 'AccountType', [1,2,3,4,20,22,23]);
  result := fAccountType;
end;

function TJwUserAccount.GetBoolean(Index: Integer): Boolean;
begin
  result := GetVal(Level, Index,
    [Variant(false),
     IDXN_PasswordExpired,    Variant(fPasswordExpired), 3,4, 'PasswordExpired'
    ]);
end;

function TJwUserAccount.GetDateTime(Index: Integer): TDateTime;
var N : TDateTime;
begin
  N := Now;
  result := GetVal(Level, Index,
    [Variant(N),
     IDXN_PasswordAge,    Variant(fPasswordAge),     1,2,3,4,11,22, 'PasswordAge',
     IDXN_LastLogoff,     Variant(fLastLogoff),      2,3,4,11,22, 'LastLogoff',
     IDXN_AccountExpires, Variant(fAccountExpires),  2,3,4,22, 'AccountExpires'
    ]);
end;

function TJwUserAccount.GetDWORD(Index: Integer): DWORD;
begin
  result := GetVal(Level, Index,
    [Variant(0),
     IDXN_Flags,            Variant(fFlags),            1,2,3,4,20,22,23, 'Flags',
     IDXN_PrivilegeLevel,   Variant(fPrivilegeLevel),   1,2,3,4,11,22, 'PrivilegeLevel',
     IDXN_AuthFlagLevel,    Variant(fAuthFlagLevel),    2,3,4,11,22, 'AuthFlagLevel',
     IDXN_BadPasswordCount, Variant(fBadPasswordCount), 2,3,4,11,22, 'BadPasswordCount',
     IDXN_LogonCount,       Variant(fLogonCount),       2,3,4,11,22, 'LogonCount',
     IDXN_CountryCode,      Variant(fCountryCode),      2,3,4,11,22, 'CountryCode',
     IDXN_MaxStorage,       Variant(fMaxStorage),       2,3,4,11,22, 'MaxStorage',
     IDXN_UnitsPerWeek,     Variant(fUnitsPerWeek),     2,3,4,11,22, 'UnitsPerWeek',
     IDXN_CodePage,         Variant(fCodePage),         2,3,4,11,22, 'CodePage'
    ]);
end;

function TJwUserAccount.GetLogonHours(Index: Integer): TJwLogonHours;
begin
  CheckSupportedLevels(Level, 'LogonHours', [2,3,4,11,22]);
  result := fLogonHours;
end;

function TJwUserAccount.GetSID(Index: Integer): TJwSecurityID;
var V : Variant;
begin
  V := GetVal(Level, Index,
    [nil,
     IDXN_Sid,              Variant(Integer(@fSID)),              3,4,20,23, 'SID',
     IDXN_PrimaryGroupdSid, Variant(Integer(@fPrimaryGroupdSid)), 3,4, 'PrimaryGroupdSid'
    ]);
  result := TJwSecurityId(Pointer(Integer(V))^);
end;

function TJwUserAccount.GetString(Index: Integer): TJwString;
begin
  result := GetVal(Level, Index,
    [Variant(''),
     IDXN_Name,          Variant(fName),          0,1,2,3,4,10,11,20,22,23, 'Name',
     IDXN_FullName,      Variant(fFullName),      2,3,4,10,11,20,22,23, 'FullName',
     IDXN_Comment,       Variant(fComment),       2,3,4,10,11,20,22,23, 'Comment',
     IDXN_UserComment,   Variant(fUserComment),   1,2,3,4,10,11,22, 'UserComment',


     IDXN_HomeDirectory, Variant(fHomeDirectory), 1,2,3,4,11,22, 'HomeDirectory',
     IDXN_Parameters,    Variant(fParameters),    2,3,4,11,22, 'Parameters',
     IDXN_LogonServer,   Variant(fLogonServer),   2,3,4,11,22, 'LogonServer',
     IDXN_ProfilePath,   Variant(fProfilePath),   3,4, 'ProfilePath',
     IDXN_HomeDirDrive,  Variant(fHomeDirDrive),  3,4, 'HomeDirDrive',

     IDXN_ScriptPath, Variant(fScriptPath),       1,3,20,22, 'ScriptPath'
    ]);
end;

function TJwUserAccount.GetWorkStations(Index: Integer): TJwWorkStations;
begin
  CheckSupportedLevels(Level, 'Workstations', [2,3,4,11,22]);
  result := fWorkstations;
end;


procedure TJwUserAccount.Update(const Level: TJwUserInfoLevel);
var
  P : PByte;

  Info0  : PUserInfo0 absolute P;
  Info1  : PUserInfo1 absolute P;
  Info2  : PUserInfo2 absolute P;
  Info3  : PUserInfo3 absolute P;
  Info4  : PUserInfo4 absolute P;
  Info10 : PUserInfo10 absolute P;
  Info11 : PUserInfo11 absolute P;
  Info20 : PUserInfo20 absolute P;
  Info23 : PUserInfo23 absolute P;

  procedure Copy0;
  begin
    fName := TJwString(Info0.usri0_name);
  end;

  procedure Copy1;
  begin
    with Info1^ do
    begin
      fName := TJwString(usri1_name);

      //usri1_password_age: DWORD;
      fPrivilegeLevel := usri1_priv;
      fHomeDirectory := TJwString(usri1_home_dir);
      fComment := TJwString(usri1_comment);
      fFlags := usri1_flags;
      //fAccountType

      fScriptPath := TJwString(usri1_script_path);
    end;


  end;
begin
  fServerName := '';
  fUserName := 'Christian';

  if NetUserGetInfo(
    PWideChar(WideString(fServerName)),//__in   LPCWSTR servername,
    PWideChar(WideString(fUserName)),//__in   LPCWSTR username,
    Level,//__in   DWORD level,
    P//__out  LPBYTE *bufptr
  ) <> 0 then;

  fLevel := Level;
  try
    case Level of
      0 : Copy0;
      1 : Copy1;
    end;
  finally
    NetApiBufferFree(P);
  end;

end;

initialization
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INITIALIZATION_SECTION}
{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.

{$ENDIF SL_OMIT_SECTIONS}
