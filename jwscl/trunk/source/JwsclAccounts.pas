{
Description
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

Version
The following values are automatically injected by Subversion on commit.
<table>
\Description                                                        Value
------------------------------------------------------------------  ------------
Last known date the file has changed in the repository              \$Date$
Last known revision number the file has changed in the repository   \$Revision$
Last known author who changed the file in the repository.           \$Author$
Full URL to the latest version of the file in the repository.       \$HeadURL$
</table>
}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclAccounts;
{$INCLUDE ..\includes\Jwscl.inc}

{$WARNINGS ON}

{$IFNDEF DEBUG}
  {$MESSAGE FAIL 'File JwsclAccounts.pas is not intended for usage. It is under development'}
{$ELSE}
  {$MESSAGE WARN 'File JwsclAccounts.pas is not intended for usage. It is under development'}
{$ENDIF}


interface

uses
  SysUtils, Contnrs, Classes,
  jwaWindows,
  JwsclTypes, JwsclExceptions, JwsclAcl, JwsclSid,
  JwsclVersion, JwsclConstants,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_IMPLEMENTATION_SECTION}


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

  TJwPrivilegeLevel = (
     plGuest = USER_PRIV_GUEST,
     plUser,
     plAdmin
    );

  TJwWorkStations = array of WideString;

  TJwWeekDay = (wdMonday = 1, wdTuesday, wdWednesday, wdThursday, wdFriday, wdSaturday, wdSunday);
  TJwGMT = -12..12;
  TJwHour = 0..23;
  TJwLogonHours = array[0..20] of Byte;


  TJwUserInfoLevel = Byte;
  TJwByteSet = set of TJwUserInfoLevel;

  TDateHour = record
    Hour : ShortInt;
    case Boolean of
      true  : (Day : TJwWeekDay);
      false : (DayI : Integer);
  end;

  TJwBaseAccount = class
{$IFDEF UNITTEST}public{$ELSE}protected{$ENDIF}
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
    fPrivilegeLevel : TJwPrivilegeLevel; //  DWORD usri11_priv;
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
    fLogonHours : PByte; //PBYTE usri11_logon_hours;
    fCodePage : DWORD; //DWORD usri11_code_page;
    fScriptPath : TJwString; //usri1_script_path;

    //4
    fAccountExpires : TDateTime; //DWORD usri4_acct_expires;
    //DWORD usri4_num_logons;
    //LPWSTR usri4_logon_server;
    //DWORD usri4_country_code;
    //DWORD usri4_code_page;
    //PSID usri4_user_sid;
    fPrimaryGroupdSid : Cardinal; //DWORD usri4_primary_group_id;
    fProfilePath : TJwString; //LPWSTR usri4_profile;
    fHomeDirDrive : TJwString;//LPWSTR usri4_home_dir_drive;
    fPasswordExpired : Boolean; //DWORD usri4_password_expired;

    fGMT : TJwGMT;

{$IFDEF UNITTEST}public{$ELSE}protected{$ENDIF}
    //converts bit number (0..167) to (date, hour) tuple
    function BitToDateHour(Bits : PBYTE; BitNo : Byte) : TDateHour;
    //converts (date, hour) tuple to bit number (0..167)
    function DateHourToBit(DateHour : TDateHour) : Byte;

    //returns bit from Bits at position BitNo (0..167)
    function GetByteToBit(BitNo : Byte; Bits : PByte) : Boolean;
    //sets bit in Bits at position BitNo (0..167) and returns previous value
    function SetByteToBit(BitNo : Byte; var Bits : PByte; Value : Boolean) : Boolean;

    //adds (or subtracts) a GMT to a (date, hour) tuple. GMT can be negative
    Function AddGmt(DateHour : TDateHour; Gmt: Integer): TDateHour;

    //returns bit value in Bits for a (date, hour) tuple and additional GMT
    function GetDateHour(Bits : PBYTE; DateHour : TDateHour; Gmt : ShortInt) : Boolean; overload;
    function GetDateHour(Bits : PBYTE; Day : TJwWeekDay; Hour : Byte; Gmt : ShortInt) : Boolean; overload;

    //Sets bit value in Bits for a (date, hour) tuple and additional GMT and returns previous value
    function SetDateHour(Bits : PBYTE; DateHour : TDateHour; Gmt : ShortInt; Value : Boolean) : Boolean; overload;
    function SetDateHour(Bits : PBYTE; Day : TJwWeekDay; Hour : Byte; Gmt : ShortInt; Value : Boolean) : Boolean; overload;

    //returns bit value from fLogonHours for a (date, hour) tuple and GMT property
    //fLogonHours must be SAM_HOURS_PER_WEEK; otherwise raises EJwsclUnsupportedException
    function GetLogonHour(Day: TJwWeekDay; Hour: TJwHour): Boolean;
    //Sets bit value in fLogonHours for a (date, hour) tuple and GMT property
    //fLogonHours must be SAM_HOURS_PER_WEEK; otherwise raises EJwsclUnsupportedException
    procedure SetLogonHour(Day: TJwWeekDay; Hour: TJwHour; const Value: Boolean);

    procedure UpdateLogonHoursProperty(logon_hours : PByte);

    function GetGMT: TJwGMT;
    procedure SetGMT(const Value: TJwGMT);

    function UnixTimeToDateTime(const Seconds : DWORD) : TDateTime;
    function DateTimeToUnixTime(const Time : TDateTime) : DWORD;

    procedure RaiseOnInvalidLogonHours;



  public
    constructor Create;
    destructor Destroy; override;

  public
    procedure ResetLogonHours(const Pattern : Byte); overload;
    procedure ResetLogonHours(const Value : TJwLogonHours); overload;
    procedure ResetLogonHours(const Allowed : Boolean); overload;


    property LogonHours[Day : TJwWeekDay; Hour : TJwHour] : Boolean read GetLogonHour write SetLogonHour;
    property GMT : TJwGMT read GetGMT write SetGMT;


//  protected
//    property UserName : TJwString read fUserName write fUserName;
//    property ServerName : TJwString read fServerName write fServerName;
//
//    property Name : TJwString read fName write fName;
//    property Comment : TJwString read fComment write fComment;
//    property UserComment : TJwString read fUserComment write fUserComment;
//    property FullName : TJwString read fFullName write fFullName;
//
//    property Flags : DWORD read fFlags write fFlags;
//    property AccountType : TJwUserAccountType read fAccountType write fAccountType;
//    property SID : TJwSecurityID read fSID write fSID;
//
//    //11
//    property PrivilegeLevel : TJwPrivilegeLevel read fPrivilegeLevel write fPrivilegeLevel;//  DWORD usri11_priv;
//    property AuthFlagLevel : DWORD read fAuthFlagLevel write fAuthFlagLevel; // DWORD usri11_auth_flags;
//    property PasswordAge : TDateTime read fPasswordAge write fPasswordAge;// DWORD usri11_password_age;
//    property HomeDirectory : TJwString read fHomeDirectory write fHomeDirectory; // LPWSTR usri11_home_dir;
//    property Parameters : TJwString read fParameters write fParameters; // LPWSTR usri11_parms;
//    property LastLogon : TDateTime read fLastLogon write fLastLogon; //DWORD usri11_last_logon;
//    property LastLogoff : TDateTime read fLastLogoff write fLastLogoff; //DWORD usri11_last_logoff;
//    property BadPasswordCount : DWORD read fBadPasswordCount write fBadPasswordCount; //  DWORD usri11_bad_pw_count;
//    property LogonCount : DWORD read fLogonCount write fLogonCount; //DWORD usri11_num_logons;
//    property LogonServer : TJwString read fLogonServer write fLogonServer; // LPWSTR usri11_logon_server;
//    property CountryCode : DWORD read fCountryCode write fCountryCode; //DWORD usri11_country_code;
//    property Workstations : TJwWorkStations read fWorkstations write fWorkstations; //LPWSTR usri11_workstations;
//    property MaxStorage : DWORD read fMaxStorage write fMaxStorage; //DWORD usri11_max_storage;
//    property UnitsPerWeek : DWORD read fUnitsPerWeek write fUnitsPerWeek; // DWORD usri11_units_per_week;
//    property LogonHours : TJwLogonHours read fLogonHours write fLogonHours; //PBYTE usri11_logon_hours;
//    property CodePage : DWORD read fCodePage write fCodePage; //DWORD usri11_code_page;
//    property ScriptPath : TJwString read fScriptPath write fScriptPath; //usri1_script_path;
//
//    //4
//    property AccountExpires : TDateTime read fAccountExpires write fAccountExpires; //DWORD usri4_acct_expires;
//    //DWORD usri4_num_logons;
//    //LPWSTR usri4_logon_server;
//    //DWORD usri4_country_code;
//    //DWORD usri4_code_page;
//    //PSID usri4_user_sid;
//    property PrimaryGroupdSid : Cardinal read fPrimaryGroupdSid write fPrimaryGroupdSid; //DWORD usri4_primary_group_id;
//    property ProfilePath : TJwString read fProfilePath write fProfilePath; //LPWSTR usri4_profile;
//    property HomeDirDrive : TJwString read fHomeDirDrive write fHomeDirDrive;//LPWSTR usri4_home_dir_drive;
//    property PasswordExpired : Boolean read fPasswordExpired write fPasswordExpired; //DWORD usri4_password_expired;       }
  end;




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

  {<B>TJwUserAccount</B> is not implemented yet}
  TJwUserAccount = class(TJwBaseAccount)
  private
    procedure Update;
    procedure Apply;
  public
    constructor Create(const ServerName, UserName : TJwString);
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

uses Variants, Math;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

const
  LOGONHOURSSIZE = sizeof(TJwLogonHours);

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


constructor TJwUserAccount.Create(const ServerName, UserName : TJwString);
begin
  fUserName := UserName;
  fServerName := ServerName;

  Update();
end;

procedure TJwUserAccount.Update();

  function ByteToBit(BitNo : Byte; Bits : PByte) : Boolean;
  var I : Byte;
  begin
    I := Byte(BitNo div 8);
    Inc(Bits, I);
    BitNo := BitNo - (I*8);
    result := (Bits^ and (1 shl BitNo)) = (1 shl BitNo);
  end;

  function ResultDateTime(const Seconds : DWORD) : TDateTime;
  begin
    if Seconds = 0 then
      result := NaN
    else
      result := UnixTimeToDateTime(Seconds);
  end;

  function IsAccount(const Value, Bit : DWORD) : Boolean;
  begin
    result := (Value and Bit) = Bit;
  end;

var
  P : PByte;
  Status : NET_API_STATUS;
  LastError : DWORD;
  Level : DWORD;
  Info4 : PUserInfo4 absolute P;
  List : TStringList;
  I: Integer;

//  LogonHours : array[0..20] of Byte;
begin
  Level := 4;
  Status := NetUserGetInfo(
    PWideChar(WideString(fServerName)),//__in   LPCWSTR servername,
    PWideChar(WideString(fUserName)),//__in   LPCWSTR username,
    Level,//__in   DWORD level,
    P//__out  LPBYTE *bufptr
  );

  if Status <> NERR_Success then
  begin
    case Status of
      ERROR_ACCESS_DENIED,
      ERROR_BAD_NETPATH,
      ERROR_INVALID_LEVEL :
          LastError := Status;
      NERR_InvalidComputer :
          LastError := ERROR_INVALID_COMPUTERNAME;
      NERR_UserNotFound :
          LastError := ERROR_NO_SUCH_USER;
    else
      LastError := Status;
    end;

    SetLastError(LastError);
    RaiseLastOSError;
  end;

  try
    fName := TJwString(Info4.usri4_name);
    fComment := Info4.usri4_comment;
    fUserComment := Info4.usri4_usr_comment;
    fFullName := Info4.usri4_full_name;

    if IsAccount(Info4.usri4_priv, UF_NORMAL_ACCOUNT) then
      fAccountType := uatNormal
    else
    if IsAccount(Info4.usri4_priv, UF_TEMP_DUPLICATE_ACCOUNT) then
      fAccountType := uatTempDuplicate
    else
    if IsAccount(Info4.usri4_priv, UF_WORKSTATION_TRUST_ACCOUNT) then
      fAccountType := uatWorkstationTrust
    else
    if IsAccount(Info4.usri4_priv, UF_SERVER_TRUST_ACCOUNT) then
      fAccountType := uatServerTrust
    else
    if IsAccount(Info4.usri4_priv, UF_INTERDOMAIN_TRUST_ACCOUNT) then
      fAccountType := uatInterDomainTrust
    else
      fAccountType := uatUnknown;

    fPrivilegeLevel := TJwPrivilegeLevel(Info4.usri4_priv);
    fSID := TJwSecurityID.Create(Info4.usri4_user_sid);

    //11
    fAuthFlagLevel := Info4.usri4_auth_flags; // : DWORD; // DWORD usri11_auth_flags;
    fPasswordAge := ResultDateTime(Info4.usri4_password_age);  //  fPasswordAge :=  TDateTime;// DWORD usri11_password_age;

    fHomeDirectory := Info4.usri4_home_dir; // TJwString; // LPWSTR usri11_home_dir;
    fParameters := Info4.usri4_parms; //TJwString; // LPWSTR usri11_parms;
    fLastLogon := ResultDateTime(Info4.usri4_last_logon); //DWORD usri11_last_logon;
    fLastLogoff := ResultDateTime(Info4.usri4_last_logoff); // TDateTime; //DWORD usri11_last_logoff;

    fBadPasswordCount := Info4.usri4_bad_pw_count; // : DWORD; //  DWORD usri11_bad_pw_count;
    fLogonCount := Info4.usri4_num_logons; //: DWORD; //DWORD usri11_num_logons;
    fLogonServer := Info4.usri4_logon_server;// : TJwString; // LPWSTR usri11_logon_server;
    fCountryCode  := Info4.usri4_country_code; //: DWORD; //DWORD usri11_country_code;

    List := TStringList.Create;
    try
      List.CommaText := WideString(Info4.usri4_workstations);

      SetLength(fWorkstations, List.Count);
      for I := 0 to List.Count - 1 do
        fWorkstations[I] := List[I]; //: TJwWorkStations; //LPWSTR usri11_workstations;
    finally
      FreeAndNil(List);
    end;

    fMaxStorage := Info4.usri4_max_storage;  //DWORD; //DWORD usri11_max_storage;
    fUnitsPerWeek := Info4.usri4_units_per_week; //: DWORD; // DWORD usri11_units_per_week;

    //fLogonHours := Info4.usri4_logon_hours; // : TJwLogonHours; //PBYTE usri11_logon_hours;
    if fUnitsPerWeek = SAM_HOURS_PER_WEEK then //don't support anything else than hours
      UpdateLogonHoursProperty(Info4.usri4_logon_hours)
    else
      fLogonHours := nil;

    fCodePage := Info4.usri4_code_page; //: DWORD; //DWORD usri11_code_page;
    fScriptPath := TJwString(Info4.usri4_script_path);

    fPrimaryGroupdSid := Info4.usri4_primary_group_id; //: TJwSecurityId; //DWORD usri4_primary_group_id;
    fProfilePath := Info4.usri4_profile;  // : TJwString; //LPWSTR usri4_profile;
    fHomeDirDrive := Info4.usri4_home_dir_drive; // TJwString;//LPWSTR usri4_home_dir_drive;
    fPasswordExpired := Info4.usri4_password_expired <> 0; // : Boolean; //DWORD usri4_password_expired;
  finally
    NetApiBufferFree(P);
  end;
end;

procedure TJwUserAccount.Apply;
begin

end;

{ TJwBaseAccount }

function TJwBaseAccount.AddGmt(DateHour: TDateHour; Gmt: Integer): TDateHour;
Begin
  If gmt > 0 Then Begin
    DateHour.Hour := DateHour.Hour + gmt;
    While DateHour.Hour > 23 Do Begin
      DateHour.Hour := DateHour.Hour - 24;
      DateHour.DayI := (DateHour.DayI + 1) Mod 7;
    End;
  End
  Else If gmt < 0 Then Begin
    DateHour.Hour := DateHour.Hour + gmt;
    While DateHour.Hour < 0 Do Begin
      DateHour.Hour := DateHour.Hour + 24;
      DateHour.DayI := (DateHour.DayI + 6) Mod 7;
    End;
  End;
  if DateHour.Day < Low(DateHour.Day) then
    DateHour.Day := High(DateHour.Day)
  else
  if DateHour.Day > High(DateHour.Day) then
    DateHour.Day := low(DateHour.Day);

  result.DayI := DateHour.DayI;
  result.Hour := DateHour.Hour;
End;

function TJwBaseAccount.BitToDateHour(Bits: PBYTE; BitNo: Byte): TDateHour;
begin
  Assert(BitNo <= 167, 'BitNo must be smaller than 168');
  result.DayI := 1 + BitNo div 24;

  if BitNo < 24 then  //If BitNo < 24 (Sunday) assume input is on Monday
  begin
    Inc(BitNo, 24);
  end
  else
  if BitNo > (168-24) then //If BitNo > 144 (Saturday) assume input is Sunday
  begin
    Dec(BitNo, 168-24);
    result.Day := wdSunday;
  end;

  result.Hour := BitNo - ((BitNo div 24) * 24);
end;

constructor TJwBaseAccount.Create;
begin
  fLogonHours := nil;
end;

function TJwBaseAccount.DateHourToBit(DateHour: TDateHour): Byte;
begin
  if (DateHour.Day = wdSunday) then
    DateHour.DayI := 0;

  result := (DateHour.DayI * 24) + DateHour.Hour;
end;


destructor TJwBaseAccount.Destroy;
begin
  if fLogonHours <> nil then
    FreeMem(fLogonHours);

  inherited;
end;

function TJwBaseAccount.GetByteToBit(BitNo: Byte; Bits: PByte): Boolean;
var I : Byte;
begin
  I := Byte(BitNo div 8);
  Inc(Bits, I);
  BitNo := BitNo - (I*8);
  result := (Bits^ and (1 shl BitNo)) = (1 shl BitNo);
end;

function TJwBaseAccount.SetByteToBit(BitNo: Byte; var Bits: PByte; Value: Boolean): Boolean;
var
  I,Val : Byte;
  P : PByte;
begin
  I := Byte(BitNo div 8);
  P := Bits; //dont mess with Bits directly
  Inc(P, I);
  BitNo := BitNo - (I*8);
  result := (P^ and (1 shl BitNo)) = (1 shl BitNo);

  if Value then
  begin
    Val := 1 shl BitNo;
    P^ := P^ or Val;
  end
  else
  begin
    Val := not (1 shl BitNo);
    P^ := P^ and Val;
  end;
end;

function TJwBaseAccount.GetDateHour(Bits: PBYTE; DateHour: TDateHour; Gmt: ShortInt): Boolean;
begin
  Assert(DateHour.Hour < 24, 'Hour must be smaller than 24');
  Assert(DateHour.Hour >= 0, 'Hour must be greater than or equal to 0');
  Assert((Gmt <= 24) and (Gmt >= -24), 'Gmt must between 24 and -24');

  DateHour := AddGmt(DateHour, Gmt);

  result := GetByteToBit(DateHourToBit(DateHour), Bits);
end;

function TJwBaseAccount.GetDateHour(Bits: PBYTE; Day: TJwWeekDay; Hour: Byte; Gmt: ShortInt): Boolean;
var DateHour : TDateHour;
begin
  DateHour.Hour := Hour;
  DateHour.Day := Day;
  Result := GetDateHour(Bits, DateHour, Gmt);
end;



function TJwBaseAccount.GetGMT: TJwGMT;
begin
  result := fGMT;
end;

function TJwBaseAccount.GetLogonHour(Day: TJwWeekDay; Hour: TJwHour): Boolean;
begin
  RaiseOnInvalidLogonHours;

  if fLogonHours = nil then
  begin
    result := true;
    exit;
  end;

  result := GetDateHour(fLogonHours, Day, Hour, fGMT);
end;

procedure TJwBaseAccount.ResetLogonHours(const Pattern: Byte);
var Value : TJwLogonHours;
begin
  FillChar(Value, sizeof(Value), Pattern);
  ResetLogonHours(Value);
end;

procedure TJwBaseAccount.ResetLogonHours(const Value: TJwLogonHours);
begin
  RaiseOnInvalidLogonHours;

  UpdateLogonHoursProperty(@Value);
end;

procedure TJwBaseAccount.RaiseOnInvalidLogonHours;
begin
  if fUnitsPerWeek <> SAM_HOURS_PER_WEEK then
  begin
    raise EJwsclUnsupportedException.CreateFmtEx('The logon hours size (%d) is not supported (must be %d).',
      'LogonHours Property', ClassName, 'JwsclAccount.pas', 0, False, [fUnitsPerWeek, SAM_HOURS_PER_WEEK]);
  end;
end;

procedure TJwBaseAccount.ResetLogonHours(const Allowed: Boolean);
Var B : Byte;
begin
  if Allowed then
    B := $FF
  else
    B := 0;

  ResetLogonHours(B);
end;

//http://www.delphipraxis.net/4278-datetime-unixtimestamp-und-zurueck.html
function TJwBaseAccount.UnixTimeToDateTime(const Seconds: DWORD): TDateTime;
begin
  Result := ((Seconds + 7200) / 86400) + 25569;
end;

//http://www.delphipraxis.net/4278-datetime-unixtimestamp-und-zurueck.html
function TJwBaseAccount.DateTimeToUnixTime(const Time: TDateTime): DWORD;
begin
  Result := ((Trunc(Time) - 25569) * 86400) +
            Trunc(86400 * (Time - Trunc(Time))) - 7200;
end;


procedure TJwBaseAccount.UpdateLogonHoursProperty(logon_hours : PByte);
const
  Arr : TJwLogonHours =
     ($FF,$FF,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF, $FF);
begin
  if (logon_hours = nil) or
     (CompareMem(@Arr, logon_hours, LOGONHOURSSIZE)) then  //is the logon allowed completely?
  begin
    if fLogonHours <> nil then
      FreeMem(fLogonHours);
    fLogonHours := nil;
  end
  else
  begin
    RaiseOnInvalidLogonHours;

    GetMem(fLogonHours, LOGONHOURSSIZE);
    CopyMemory(fLogonHours, logon_hours, LOGONHOURSSIZE);
  end;
end;

function TJwBaseAccount.SetDateHour(Bits: PBYTE; DateHour: TDateHour; Gmt: ShortInt; Value: Boolean): Boolean;
begin
  Assert(DateHour.Hour < 24, 'Hour must be smaller than 24');
  Assert(DateHour.Hour >= 0, 'Hour must be greater than or equal to 0');
  Assert((Gmt <= 24) and (Gmt >= -24), 'Gmt must between 24 and -24');

  DateHour := AddGmt(DateHour, Gmt);

  result := SetByteToBit(DateHourToBit(DateHour), Bits, Value);
end;

function TJwBaseAccount.SetDateHour(Bits: PBYTE; Day: TJwWeekDay; Hour: Byte; Gmt: ShortInt; Value: Boolean): Boolean;
var DateHour : TDateHour;
begin
  DateHour.Hour := Hour;
  DateHour.Day := Day;
  Result := SetDateHour(Bits, DateHour, Gmt, Value);
end;

procedure TJwBaseAccount.SetGMT(const Value: TJwGMT);
begin
  fGMT := Value;
end;



procedure TJwBaseAccount.SetLogonHour(Day: TJwWeekDay; Hour: TJwHour; const Value: Boolean);
begin
  RaiseOnInvalidLogonHours;

  if fLogonHours = nil then
  begin
    GetMem(fLogonHours, LOGONHOURSSIZE);
    FillChar(fLogonHours^, LOGONHOURSSIZE, $FF);
  end;

  SetDateHour(fLogonHours, Day, Hour, fGMT, Value);
end;

initialization
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INITIALIZATION_SECTION}
{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.

{$ENDIF SL_OMIT_SECTIONS}
