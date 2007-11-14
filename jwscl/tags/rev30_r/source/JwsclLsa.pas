{@abstract(This unit provides access to the Local Security Authority Subsystem that provides function like LSALogonUser to create a logon session.)
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

The Original Code is JwsclLSA.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

Description:

}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclLsa;
{$INCLUDE Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $

interface

uses SysUtils,
  jwaWindows,
  JwsclResource,
  JwsclSid, JwsclToken,
  JwsclTypes, JwsclExceptions,
  JwsclVersion, JwsclConstants, JwsclProcess,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}


type
  TJwSecurityLsa = class
  protected
    fLsaHandle: THandle;


  public
    constructor Create(const LogonProcessName: string);
    destructor Destroy; override;

    procedure LsaLogonUser(anOriginName: string;
      aLogonType: SECURITY_LOGON_TYPE;
      anAuthenticationPackageName: string;
      anAuthenticationInformation: Pointer;
      anAuthenticationInformationLength: Cardinal;
      aLocalGroups: TJwSecurityIdList;
      aSourceContext: TTokenSource;
      out aProfileBuffer: Pointer;
      out aProfileBufferLength: Cardinal;
      out aTokenLuid: TLUID;
      out aToken: TJwSecurityToken;
      out aQuotaLimits: QUOTA_LIMITS;
      out SubStatus: NTSTATUS);

    property LsaHandle: Cardinal Read fLsaHandle;
  end;

  TJwLogonSessionArray = Array of TLuid;

  {@Name contains readonly information about a logon session.}
  TJwLsaLogonSessionData = class
  protected
    fSize : ULONG;
    fLogonId : TLuid;
    fUserName,
    fLogonDomain,
    fAuthenticationPackage : WideString;
    fLogonType : TSecurityLogonType;
    fSession  : ULONG;
    fSid : TJwSecurityId;
    fLogonTime : LARGE_INTEGER;
    fLogonServer,
    fDnsDomainName,
    fUpn : WideString;


  public
    constructor Create(const SessionData : PSecurityLogonSessionData = nil);
    destructor Destroy; override;

    property Size : ULONG read fSize;
    property LogonId : TLuid read fLogonId;
    property UserName : WideString read fUserName;
    property LogonDomain : WideString read fLogonDomain;
    property AuthenticationPackage : WideString read fAuthenticationPackage;
    property LogonType : TSecurityLogonType read fLogonType;
    property Session  : ULONG read fSession;
    property Sid : TJwSecurityId read fSid;
    property LogonTime : LARGE_INTEGER read fLogonTime;
    property LogonServer : WideString read fLogonServer;
    property DnsDomainName : WideString read fDnsDomainName;
    property Upn : WideString read fUpn;
  end;

  {@Name provides function for enumerating principal logon sessions
  and its data.}
  TJwLsaLogonSession = class
  protected
  public
     {@Name returns an array of logon ids (TLUID)
      @raises EJwsclWinCallFailedException if a call to LsaEnumerateLogonSessions failed.
     }
     class function GetSessions : TJwLogonSessionArray;
     {@Name returns TJwLsaLogonSessionData for a specific logon id.
     @param LogonID defines the logon id to be retrieved information from.
     @raises EJwsclWinCallFailedException if a call to LsaGetLogonSessionData failed.
     }
     class function GetSessionData(const LogonID : TLuid) :
       TJwLsaLogonSessionData;

  end;


{@Name is a helper function to create a MSV1_0_INTERACTIVE_LOGON
structure with strings that are added right behind the structure memory
so it can be used by LsaLogonUser.
The returned pointer can be freed by LocalFree .
}
function JwCreate_MSV1_0_INTERACTIVE_LOGON(
  MessageType: MSV1_0_LOGON_SUBMIT_TYPE;
  LogonDomainName, UserName,
  Password: WideString;
  out authLen: Cardinal): PMSV1_0_INTERACTIVE_LOGON;

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation


{ TJwSecurityLsa }
         

function JwCreateLSAString(const aString: string): LSA_STRING;
var
  pStr: PChar;
begin
  Result.Length := Length(aString);
  Result.MaximumLength := Result.Length;

  GetMem(pStr, Length(aString) + 2);
  FillChar(pStr^, Length(aString) + 2, 0);
  StrLCopy(pStr, PChar(aString), Length(aString));

  Result.Buffer := pStr;
end;

procedure JwFreeLSAString(var aString: LSA_STRING);
begin
  if aString.Buffer <> nil then
    FreeMem(aString.Buffer);

  FillChar(aString, sizeof(aString), 0);
end;


constructor TJwSecurityLsa.Create(const LogonProcessName: string);
var
  lsaHostString: LSA_STRING;
  res: Cardinal;
  lsaSecurityMode: LSA_OPERATIONAL_MODE;

const
  p1: _LSA_STRING = (Length: 3;
    MaximumLength: 3;
    Buffer: '12'#0);
begin
  // JwEnablePrivilege(SE_TCB_NAME,pst_Enable);

  lsaHostString := JwCreateLSAString(LogonProcessName);


  res := LsaRegisterLogonProcess(lsaHostString, fLsaHandle, @lsaSecurityMode);

  JwFreeLSAString(lsaHostString);

  if res <> STATUS_SUCCESS then
  begin
    res := LsaNtStatusToWinError(res);
    SetLastError(res);
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'Create', ClassName, 'JwsclLsa.pas',
      0, True, 'LsaRegisterLogonProcess',
      ['LsaRegisterLogonProcess']);
  end;
end;

destructor TJwSecurityLsa.Destroy;
begin
  LsaDeregisterLogonProcess(fLsaHandle);
  fLsaHandle := 0;
end;

procedure _initUnicodeString(target: PUNICODE_STRING;
  Source: PWideChar; cbMax: USHORT);
begin
  target.Length := cbMax;//-2;//- sizeof(source^);
  target.MaximumLength := cbMax;
  target.Buffer := Source;
end;


function JwCreate_MSV1_0_INTERACTIVE_LOGON(
  MessageType: MSV1_0_LOGON_SUBMIT_TYPE;
  LogonDomainName, UserName,
  Password: WideString;
  out authLen: Cardinal): PMSV1_0_INTERACTIVE_LOGON;
var
  iSize: integer;
  p: PWCHAR;

  cbHeader, cbDom, cbUser, cbPass: integer;

  pDom, pUser, pPass: PWChar;

const
  iUSHORT = sizeof(USHORT);
  iWCHAR  = sizeof(widechar);
begin
  cbHeader := sizeof(MSV1_0_INTERACTIVE_LOGON);
  cbDom  := Length(LogonDomainName) * iWCHAR;
  cbUser := Length(UserName) * iWCHAR;
  cbPass := Length(Password) * iWCHAR;

  iSize := cbHeader + cbDom + cbUser + cbPass;

  authLen := iSize;

  Result := PMSV1_0_INTERACTIVE_LOGON(LocalAlloc(LMEM_ZEROINIT or
    LMEM_FIXED, iSize));

  Result.MessageType := MessageType;
  p := PWCHAR(Result);
  Inc(integer(p), cbHeader);

  pDom  := p;
  pUser := PWChar(integer(p) + cbDom);
  pPass := PWChar(integer(p) + cbDom + cbUser);

  CopyMemory(pDom, @LogonDomainName[1], cbDom);
  CopyMemory(pUser, @UserName[1], cbUser);
  CopyMemory(pPass, @Password[1], cbPass);

  _initUnicodeString(@Result.LogonDomainName, pDom, cbDom);
  _initUnicodeString(@Result.UserName, pUser, cbUser);
  _initUnicodeString(@Result.Password, pPass, cbPass);
end;

procedure TJwSecurityLsa.LsaLogonUser(anOriginName: string;
  aLogonType: SECURITY_LOGON_TYPE; anAuthenticationPackageName: string;
  anAuthenticationInformation: Pointer;
  anAuthenticationInformationLength: Cardinal;
  aLocalGroups: TJwSecurityIdList; aSourceContext: TTokenSource;
  out aProfileBuffer: Pointer; out aProfileBufferLength: Cardinal;
  out aTokenLuid: TLUID; out aToken: TJwSecurityToken;
  out aQuotaLimits: QUOTA_LIMITS; out SubStatus: NTSTATUS);

var
  res: Cardinal;
  lsaOrig, lsaPackageName: LSA_STRING;

  pLocalGroups: PTokenGroups;
  hToken: TJwTokenHandle;
  cAuthenticationPackage: Cardinal;

const
  p1: _LSA_STRING = (Length: 20;
    MaximumLength: 20;
    Buffer: ''#0);
begin
  lsaPackageName := JwCreateLSAString(anAuthenticationPackageName);

  cAuthenticationPackage := 0;
  res := LsaLookupAuthenticationPackage(
    fLsaHandle,//HANDLE LsaHandle,
    lsaPackageName,//PLSA_STRING PackageName,
    cAuthenticationPackage);
  JwFreeLSAString(lsaPackageName);


  if res <> STATUS_SUCCESS then
  begin
    res := LsaNtStatusToWinError(res);
    SetLastError(res);
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'Create', ClassName, 'JwsclLsa.pas',
      0, True, 'LsaLookupAuthenticationPackage',
      ['LsaRegisterLogonProcess']);
  end;

  lsaOrig := JwCreateLSAString(anOriginName);

  pLocalGroups := nil;
  if Assigned(aLocalGroups) then
  begin
    pLocalGroups := aLocalGroups.Create_PTOKEN_GROUPS;
  end;
  aToken := nil;

  FillChar(aTokenLuid, sizeof(aTokenLuid), 0);
  hToken := 0;
  Fillchar(aQuotaLimits, sizeof(aQuotaLimits), 0);
  SubStatus := 0;

  res := jwaWindows.LsaLogonUser(fLsaHandle,//HANDLE LsaHandle,
    lsaOrig,//PLSA_STRING OriginName,
    aLogonType,//SECURITY_LOGON_TYPE LogonType,
    cAuthenticationPackage,//ULONG AuthenticationPackage,
    anAuthenticationInformation,//PVOID AuthenticationInformation,
    anAuthenticationInformationLength,//ULONG AuthenticationInformationLength,
    pLocalGroups,//PTOKEN_GROUPS LocalGroups,
    @aSourceContext,//PTOKEN_SOURCE SourceContext,
    aProfileBuffer,//PVOID* ProfileBuffer,
    aProfileBufferLength,//PULONG ProfileBufferLength,
    aTokenLuid,//PLUID LogonId,
    hToken, //PHANDLE Token,
    aQuotaLimits,//PQUOTA_LIMITS Quotas,
    SubStatus//PNTSTATUS SubStatus
    );
  JwFreeLSAString(lsaOrig);

  if Assigned(aLocalGroups) and (pLocalGroups <> nil) then
  begin
    aLocalGroups.Free_PTOKEN_GROUPS(pLocalGroups);
  end;

  if res <> STATUS_SUCCESS then
  begin
    res := LsaNtStatusToWinError(res);
    SetLastError(res);
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsLSALogonUserFailedSubStatus,
      'Create', ClassName, 'JwsclLsa.pas',
      0, True, 'LsaLogonUser', [SubStatus]);
  end;

  aToken := nil;
  if (hToken <> 0) and (hToken <> INVALID_HANDLE_VALUE) then
    aToken := TJwSecurityToken.Create(hToken, TOKEN_ALL_ACCESS);

end;

{ TJwLsaLogonSession }

class function TJwLsaLogonSession.GetSessionData(
  const LogonID: TLuid): TJwLsaLogonSessionData;
var p : PSecurityLogonSessionData;
    res : NTSTATUS;
begin
  p := nil;
  res := LsaGetLogonSessionData(@LogonId,p);

  if res <> STATUS_SUCCESS then
  begin
    SetLastError(LsaNtStatusToWinError(res));
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailedWithNTStatus,
      'GetSessionData', ClassName, 'JwsclLsa.pas',
      0, True, 'LsaGetLogonSessionData', ['LsaGetLogonSessionData', res]);
  end;

  result := TJwLsaLogonSessionData.Create(p);

  LsaFreeReturnBuffer(p);
end;

class function TJwLsaLogonSession.GetSessions: TJwLogonSessionArray;
var List,
    LuidPtr : PLuid;
    Count : ULONG;
    res : NTSTATUS;
  I: Integer;
begin
  List := nil;
  Count := 0;
  res := LsaEnumerateLogonSessions(@count, List);

  if res <> STATUS_SUCCESS then
  begin
    SetLastError(LsaNtStatusToWinError(res));
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailedWithNTStatus,
      'GetSessionData', ClassName, 'JwsclLsa.pas',
      0, True, 'LsaEnumerateLogonSessions', ['LsaEnumerateLogonSessions', res]);
  end;

  SetLength(result, Count);
  LuidPtr := List;
  for I := 0 to Count - 1 do
  begin
    result[i] := LuidPtr^;
    Inc(LuidPtr);
  end;
  LuidPtr := nil;


  LsaFreeReturnBuffer(List);
  List := nil;
end;

{ TJwLsaLogonSessionData }

constructor TJwLsaLogonSessionData.Create(
  const SessionData : PSecurityLogonSessionData = nil);
begin
  fSid := nil;
  if (SessionData <> nil) then
  begin
    fSize := SessionData.Size;
    fLogonId := SessionData.LogonId;
    fLogonType := SessionData.LogonType;
    fSession := SessionData.Session;
    fLogonTime := SessionData.LogonTime;

    if SessionData.Sid <> nil then
      fSid := TJwSecurityId.Create(SessionData.Sid);

    SetLength(fUserName, SessionData.UserName.Length);
    fUserName := SessionData.UserName.Buffer;

    SetLength(fLogonDomain, SessionData.LogonDomain.Length);
    fLogonDomain := SessionData.LogonDomain.Buffer;

    SetLength(fAuthenticationPackage, SessionData.AuthenticationPackage.Length);
    fAuthenticationPackage := SessionData.AuthenticationPackage.Buffer;

    SetLength(fLogonServer, SessionData.LogonServer.Length);
    fLogonServer := SessionData.LogonServer.Buffer;

    SetLength(fDnsDomainName, SessionData.DnsDomainName.Length);
    fDnsDomainName := SessionData.DnsDomainName.Buffer;

    SetLength(fUpn, SessionData.Upn.Length);
    fUpn := SessionData.Upn.Buffer;
  end;
end;


destructor TJwLsaLogonSessionData.Destroy;
begin
  FreeAndNil(fSid);
  inherited;
end;



initialization
{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_INITIALIZATION_SECTION}
  //_----

{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}