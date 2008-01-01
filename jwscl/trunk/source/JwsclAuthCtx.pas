{@abstract()
@author(Christian Wimmer)
@created(01/01/2007)
@lastmod(01/01/2007)

Project JEDI Windows Security Code Library (JWSCL)

The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

The Original Code is JwsclAuthCtx.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.



Description:

Unsupported structures :


}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclAuthCtx;
{$INCLUDE Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $


interface

uses
  SysUtils, Contnrs, Classes,
{DEBUG}
  Dialogs,
{DEBUG}
  jwaWindows, JwaVista,
  JwsclResource, JwsclUtils,

  JwsclTypes, JwsclExceptions, JwsclMapping, JwsclACL, JwsclToken,
  JwsclVersion, JwsclConstants, JwsclProcess, JwsclSid, JwsclDescriptor,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
  TAuthZClientContextHandle = AUTHZ_CLIENT_CONTEXT_HANDLE;
  TAuthZResourceManagerHandle = AUTHZ_RESOURCE_MANAGER_HANDLE;


  TAuthzAccessCheckCallback = function(
    hAuthzClientContext : AUTHZ_CLIENT_CONTEXT_HANDLE;
    pAce : PACE_HEADER;
    pArgs : Pointer;
    var pbAceApplicable : Boolean) : Boolean; stdcall;

  TAuthzComputeGroupsCallback = function(
    hAuthzClientContext : AUTHZ_CLIENT_CONTEXT_HANDLE;
    Args : Pointer;
    out pSidAttrArray : PSID_AND_ATTRIBUTES;
    out pSidCount : DWORD;
    out pRestrictedSidAttrArray : PSID_AND_ATTRIBUTES;
    out pRestrictedSidCount : DWORD
    ) : Boolean; stdcall;

  TAuthzFreeGroupsCallback = procedure(
    const pSidAttrArray : PSID_AND_ATTRIBUTES
    ); stdcall;

  TOnAuthzAccessCheckCallback = function(
    hAuthzClientContext : AUTHZ_CLIENT_CONTEXT_HANDLE;
    pAce : PACE_HEADER;
    pArgs : Pointer;
    var pbAceApplicable : Boolean) : Boolean of object; stdcall;

  TOnAuthzComputeGroupsCallback = function(
    hAuthzClientContext : AUTHZ_CLIENT_CONTEXT_HANDLE;
    Args : Pointer;
    out pSidAttrArray : array of SID_AND_ATTRIBUTES; //PSID_AND_ATTRIBUTES* pSidAttrArray,
    out pSidCount : DWORD;
    out pRestrictedSidAttrArray : array of SID_AND_ATTRIBUTES; //PSID_AND_ATTRIBUTES* pRestrictedSidAttrArray,
    out pRestrictedSidCount : DWORD
    ) : Boolean of object; stdcall;

  TOnAuthzFreeGroupsCallback = procedure(
    const pSidAttrArray : array of SID_AND_ATTRIBUTES
    ) of object; stdcall;

  TJwObjectTypeArray = array of TObjectTypeList;

  TJwAuthZAccessRequest = class
  protected
    fDesiredAccess    : TJwAccessMask;
    fPrincipalSelfSid : TJwSecurityID;
    fObjectTypeArray : TJwObjectTypeArray;
    fData : Pointer;
    fShared : TJwSharedHandle;
  public
    constructor Create(
      DesiredAccess    : TJwAccessMask;
      PrincipalSelfSid : TJwSecurityID;
      ObjectTypeArray : TJwObjectTypeArray;
      Data : Pointer;
      Shared : TJwSharedHandle);
    destructor Destroy;

    property DesiredAccess    : TJwAccessMask read fDesiredAccess;

    {@Name defines a SID that is used to replace a principle self sid
     found in an inherited ACE. A principle self SID (S-1-5-10)
     in a ACE will be replaced by this property SID. 
     }
    property PrincipalSelfSid : TJwSecurityID read fPrincipalSelfSid;
    property ObjectTypeArray : TJwObjectTypeArray read fObjectTypeArray;
    property Data : Pointer read fData write fData;
    property Shared : TJwSharedHandle read fShared write fShared;
  end;

  TJwAccessMaskArray = array of TJwAccessMask;
  TJwCardinalArray = array of Cardinal;

  TJwAuthZAccessReply = class
  protected
    fGrantedAccessMask : TJwAccessMaskArray;
    fSaclEvaluationResults,
    fError : TJwCardinalArray;
  public
    constructor Create(const ReplyStruct : TAuthzAccessReply);

    property GrantedAccessMask : TJwAccessMaskArray read fGrantedAccessMask;
    property SaclEvaluationResults : TJwCardinalArray read fSaclEvaluationResults;
    property Error : TJwCardinalArray read fError;
  end;

  TJwAuthResourceManager = class
  private
    fHandle : TAuthZResourceManagerHandle;
    fOnAuthzFreeGroupsCallback : TOnAuthzFreeGroupsCallback;
    fOnAuthzComputeGroupsCallback : TOnAuthzComputeGroupsCallback;
    fOnAuthzAccessCheckCallback : TOnAuthzAccessCheckCallback;
  protected
    procedure OnInternalAuthzFreeGroupsCallback(
        const pSidAttrArray : array of SID_AND_ATTRIBUTES
        ); stdcall;
  public
    constructor Create(
      const Name : WideString;
      Flags : TAuthZResourceManagerFlags;
      const OnAuthzFreeGroupsCallback : TOnAuthzFreeGroupsCallback;
      const OnAuthzComputeGroupsCallback : TOnAuthzComputeGroupsCallback;
      const OnAuthzAccessCheckCallback : TOnAuthzAccessCheckCallback
     );
     destructor Destroy;


  public
    property Handle : TAuthZResourceManagerHandle read fHandle;

    property OnAuthzFreeGroupsCallback : TOnAuthzFreeGroupsCallback
      read fOnAuthzFreeGroupsCallback;
    property OnAuthzComputeGroupsCallback : TOnAuthzComputeGroupsCallback
      read fOnAuthzComputeGroupsCallback;
    property OnAuthzAccessCheckCallback : TOnAuthzAccessCheckCallback
      read fOnAuthzAccessCheckCallback;
  end;

  AUTHZ_AUDIT_INFO_HANDLE = THandle;
  PAUTHZ_AUDIT_INFO_HANDLE = ^AUTHZ_AUDIT_INFO_HANDLE;
  TAuthZAuditInfoHandle = AUTHZ_AUDIT_INFO_HANDLE;
  PAuthZAuditInfoHandle = PAUTHZ_AUDIT_INFO_HANDLE;

  TAuthZAccessCheckResultHandle = AUTHZ_ACCESS_CHECK_RESULTS_HANDLE;

  TJwAuthContext = class
  protected
    fHandle : TAuthZClientContextHandle;
    fUserSid : TJwSecurityID;
    fGroupSids,
    fRestrictedSids : TJwSecurityIdList;
    fInfoPrivileges : TJwPrivilegeSet;
    fExpirationTime : Int64;
    fContextInfoIdentifier : TLuid;
    fContextInfoSource,
    fContextInfoAll,
    fContextInfoAuthenticationId : Pointer;

  protected
    function GetInformationFromContext(
      InfoClass : AUTHZ_CONTEXT_INFORMATION_CLASS) : Pointer;

    procedure InitProperties;
  public
    constructor Create; overload;

    constructor CreateByContext(
      const AuthContext : TJwAuthContext;
      Flags : TAuthZSidContextFlags;
      const ExpirationTime : Int64;
      const DynamicGroupArgs : Pointer); overload;

    constructor CreateByToken(
      const ResourceManager : TJwAuthResourceManager;
      Flags : TAuthZSidContextFlags;
      const Token : TJwSecurityToken;
      const ExpirationTime : Int64;
      const DynamicGroupArgs : Pointer); overload;
    constructor CreateBySid(
      const ResourceManager : TJwAuthResourceManager;
      Flags : TAuthZSidContextFlags;
      const Sid : TJwSecurityId;
      const ExpirationTime : Int64;
      const DynamicGroupArgs : Pointer); overload;

    destructor Destroy;

    {@Name
     @param(OptionalSecurityDescriptorArray defines additional security descriptor
      which are used for the access check. They a simply added at the end
      of the primary security descriptor (logical order).
       The canonical ACE order is not enforced.
      Deny ACE in the optional security descriptors may be useless if a positive
      ACE could be found in the primary security descriptor.

     Known Bug: Only the first security descriptor may be recognized!
     )
    }
    procedure AccessCheck(
      Flags : Cardinal;
      const Request : TJwAuthZAccessRequest;
      const AuditInfo : TAuthZAuditInfoHandle;
      const SecurityDescriptor : TJwSecurityDescriptor;
      const OptionalSecurityDescriptorArray : TJwSecurityDescriptorArray;
      out Reply : TJwAuthZAccessReply;
      out AuthZHandle : TAuthZAccessCheckResultHandle
      );

  public
    property Handle : TAuthZClientContextHandle read fHandle;

    property UserSid : TJwSecurityID read fUserSid;
    property GroupSids : TJwSecurityIdList read fGroupSids;
    property RestrictedSids : TJwSecurityIdList read fRestrictedSids;
    property InfoPrivileges : TJwPrivilegeSet read fInfoPrivileges;
    property ExpirationTime : Int64 read fExpirationTime;
    property ContextInfoIdentifier : TLuid read fContextInfoIdentifier;
    property ContextInfoSource : Pointer read fContextInfoSource;
    property ContextInfoAll : Pointer read fContextInfoAll;
    property ContextInfoAuthenticationId : Pointer read fContextInfoAuthenticationId;
  end;

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

uses Math, JwsclEnumerations;
{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_INTERFACE_SECTION}

{$ENDIF SL_INTERFACE_SECTION}




{ TJwAuthContext }

constructor TJwAuthContext.Create;
begin
  inherited;
  fHandle := 0;
end;

constructor TJwAuthContext.CreateByToken(
  const ResourceManager: TJwAuthResourceManager;
  Flags : TAuthZSidContextFlags;
  const Token: TJwSecurityToken;
  const ExpirationTime: Int64;
  const DynamicGroupArgs: Pointer);

var luid : TLuid;
    tempToken: TJwSecurityToken;
begin
  ZeroMemory(@luid, sizeof(TLuid));

  if not Assigned(Token) then
    tempToken := TJwSecurityToken.CreateTokenEffective(TOKEN_READ or TOKEN_QUERY)
  else
    tempToken := Token;

  try
    if not AuthzInitializeContextFromToken(
        TJwEnumMap.ConvertAuthZSidContextFlags(Flags),//__in   DWORD Flags,
        tempToken.TokenHandle,//__in   HANDLE TokenHandle,
        ResourceManager.Handle, //__in   AUTHZ_RESOURCE_MANAGER_HANDLE AuthzResourceManager,
        @ExpirationTime, //__in   PLARGE_INTEGER pExpirationTime,
        luid,//__in   LUID Identifier,
        DynamicGroupArgs,//__in   PVOID DynamicGroupArgs,
        @fHandle//__out  PAUTHZ_CLIENT_CONTEXT_HANDLE pAuthzClientContext
      ) then
     raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'Create', ClassName,
        RsUNSid, 0, True, ['AuthzInitializeContextFromToken']);
  finally
    if not Assigned(Token) then
      tempToken.Free;
  end;

  InitProperties;
end;

constructor TJwAuthContext.CreateBySid(
  const ResourceManager : TJwAuthResourceManager;
  Flags : TAuthZSidContextFlags;
  const Sid : TJwSecurityId;
  const ExpirationTime : Int64;
  const DynamicGroupArgs : Pointer);
var luid : TLuid;
begin
  ZeroMemory(@luid, sizeof(TLuid));

  try
    if not AuthzInitializeContextFromSid(
        TJwEnumMap.ConvertAuthZSidContextFlags(Flags),//__in   DWORD Flags,
        Sid.SID,//__in   PSID UserSid,
        ResourceManager.Handle, //__in   AUTHZ_RESOURCE_MANAGER_HANDLE AuthzResourceManager,
        @ExpirationTime, //__in   PLARGE_INTEGER pExpirationTime,
        luid,//__in   LUID Identifier,
        DynamicGroupArgs,//__in   PVOID DynamicGroupArgs,
        @fHandle//__out  PAUTHZ_CLIENT_CONTEXT_HANDLE pAuthzClientContext
      ) then
     raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'Create', ClassName,
        RsUNSid, 0, True, ['AuthzInitializeContextFromToken']);
  finally
  end;

  InitProperties;
end;

constructor TJwAuthContext.CreateByContext(
  const AuthContext: TJwAuthContext;
  Flags: TAuthZSidContextFlags;
  const ExpirationTime: Int64;
  const DynamicGroupArgs: Pointer);
var luid : TLuid;
begin
  ZeroMemory(@luid, sizeof(TLuid));

  if not Assigned(AuthContext) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'CreateByContext',
      ClassName, RsUNSid, 0, False, ['AuthContext']);

  try
    if not AuthzInitializeContextFromAuthzContext(
        TJwEnumMap.ConvertAuthZSidContextFlags(Flags),//__in   DWORD Flags,
        AuthContext.Handle,//__in   AUTHZ_CLIENT_CONTEXT_HANDLE AuthzHandle,
        @ExpirationTime, //__in   PLARGE_INTEGER pExpirationTime,
        luid,//__in   LUID Identifier,
        DynamicGroupArgs,//__in   PVOID DynamicGroupArgs,
        @fHandle//__out  PAUTHZ_CLIENT_CONTEXT_HANDLE pAuthzClientContext
      ) then
     raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'Create', ClassName,
        RsUNSid, 0, True, ['AuthzInitializeContextFromAuthzContext']);
  finally
  end;

  InitProperties;
end;

destructor TJwAuthContext.Destroy;
begin
  AuthzFreeContext(fHandle);
  fHandle := INVALID_HANDLE_VALUE;

  FreeAndNil(fUserSid);
end;

function TJwAuthContext.GetInformationFromContext(
  InfoClass: AUTHZ_CONTEXT_INFORMATION_CLASS): Pointer;
var Size,N : Cardinal;
begin
  result := nil;
  AuthzGetInformationFromContext(fHandle,
    InfoClass, 0, @Size, result);

  GetMem(result, Size);
  ZeroMemory(result, Size);

  if not AuthzGetInformationFromContext(fHandle,
    InfoClass, Size, @N, result) then
    raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'GetInformationFromContext', ClassName,
        RsUNSid, 0, True, ['AuthzGetInformationFromContext']);
end;

procedure TJwAuthContext.InitProperties;
var Sid : PTokenUser;
begin
  Sid := GetInformationFromContext(AuthzContextInfoUserSid);
  try
    fUserSid := TJwSecurityId.Create(PSidAndAttributes(@Sid.User));
  finally
    FreeMem(Sid);
  end;


end;



procedure TJwAuthContext.AccessCheck(Flags: Cardinal;
  const Request: TJwAuthZAccessRequest;
  const AuditInfo: TAuthZAuditInfoHandle;
  const SecurityDescriptor: TJwSecurityDescriptor;
  const OptionalSecurityDescriptorArray: TJwSecurityDescriptorArray;
  out Reply: TJwAuthZAccessReply;
  out AuthZHandle: TAuthZAccessCheckResultHandle);

type
   TPSDArray = array of PSECURITY_DESCRIPTOR;
   TSDArray = array of SECURITY_DESCRIPTOR;
var pRequest : AUTHZ_ACCESS_REQUEST;
    pSD : PSecurityDescriptor;
    //pOSD,pPOSD : PSECURITY_DESCRIPTOR;
    OSD : array of SECURITY_DESCRIPTOR;
    pOSD : array of PSECURITY_DESCRIPTOR;

    pReply : AUTHZ_ACCESS_REPLY;
    i : Integer;
begin
  Reply := nil;
  AuthZHandle := 0;
  
  ZeroMemory(@pRequest, sizeof(pRequest));
  pRequest.DesiredAccess    := Request.fDesiredAccess;
  pRequest.PrincipalSelfSid := Request.fPrincipalSelfSid.SID;
  pRequest.ObjectTypeList   := @Request.fObjectTypeArray;
  pRequest.ObjectTypeListLength := Length(Request.fObjectTypeArray);
  pRequest.OptionalArguments := Request.fData;

  pSD := nil;
  if Assigned(SecurityDescriptor) Then
    pSD := SecurityDescriptor.Create_SD();

 (* GetMem(pOSD, Length(OptionalSecurityDescriptorArray) * sizeof(PSECURITY_DESCRIPTOR));
//  pPOSD := pOSD;
  for i := 0 to Length(OptionalSecurityDescriptorArray)-1 do
  begin
    TSDArray(pOSD)[i] := OptionalSecurityDescriptorArray[i].Create_SD(false);

  end;*)

  //Bug??: Only the first security descriptor may be recognized!
  SetLength(pOSD, Length(OptionalSecurityDescriptorArray));
  SetLength(OSD, Length(OptionalSecurityDescriptorArray));
  for i := 0 to Length(OptionalSecurityDescriptorArray)-1 do
  begin
    pOSD[i] := OptionalSecurityDescriptorArray[i].Create_SD(false);
    OSD[i] := pOSD[i]^;
  end;


  ZeroMemory(@pReply, sizeof(pReply));

  pReply.ResultListLength := pRequest.ObjectTypeListLength;
  if pRequest.ObjectTypeListLength = 0 then
    pReply.ResultListLength := 1;

  GetMem(pReply.GrantedAccessMask,
    pReply.ResultListLength * sizeof(ACCESS_MASK));
  GetMem(pReply.SaclEvaluationResults,
    pReply.ResultListLength * sizeof(DWORD));
  GetMem(pReply.Error,
    pReply.ResultListLength * sizeof(DWORD));

  for i := 0 to pReply.ResultListLength-1 do
  begin
    TJwCardinalArray(pReply.GrantedAccessMask)[i] := 0;
    TJwCardinalArray(pReply.SaclEvaluationResults)[i] := 0;
    TJwCardinalArray(pReply.Error)[i] := 0;
  end;

  SetLastError(0);
  try
    try
      if not AuthzAccessCheck(
        Flags, //__in      DWORD flags,
        Handle,//__in      AUTHZ_CLIENT_CONTEXT_HANDLE AuthzClientContext,
        @pRequest,//__in      PAUTHZ_ACCESS_REQUEST pRequest,
        AuditInfo,//__in      AUTHZ_AUDIT_INFO_HANDLE AuditInfo,
        pSD,//__in      PSECURITY_DESCRIPTOR pSecurityDescriptor,
        @OSD,//__in_opt  PSECURITY_DESCRIPTOR* OptionalSecurityDescriptorArray,
        Length(OptionalSecurityDescriptorArray),//__in_opt  DWORD OptionalSecurityDescriptorCount,
        @pReply,//__inout   PAUTHZ_ACCESS_REPLY pReply,
        @AuthZHandle, //__out     PAUTHZ_ACCESS_CHECK_RESULTS_HANDLE pAuthzHandle
      ) then
      begin
        raise EJwsclWinCallFailedException.CreateFmtEx(
          RsWinCallFailed, 'AuthzAccessCheck', ClassName,
          RsUNSid, 0, True, ['AuthzAccessCheck']);
      end;

      Reply := TJwAuthZAccessReply.Create(pReply);
    finally
      FreeMem(pReply.GrantedAccessMask);
      FreeMem(pReply.SaclEvaluationResults);
      FreeMem(pReply.Error);
    
      TJwSecurityDescriptor.Free_SD(pSD);
    
      i := GetLastError;
      OSD := nil;
      for i := 0 to Length(OptionalSecurityDescriptorArray)-1 do
      begin
        TJwSecurityDescriptor.Free_SD(pOSD[i]);
      end;
      pOSD := nil;
    end;
  except
    on E : EJwsclWinCallFailedException do
    begin
      FreeAndNil(Reply);
    end;
  end;

  if Request.Shared = shOwned then
    Request.Free;
end;

{ TJwAuthResourceManager }

constructor TJwAuthResourceManager.Create(const Name: WideString;
  Flags: TAuthZResourceManagerFlags;
  const OnAuthzFreeGroupsCallback: TOnAuthzFreeGroupsCallback;
  const OnAuthzComputeGroupsCallback: TOnAuthzComputeGroupsCallback;
  const OnAuthzAccessCheckCallback: TOnAuthzAccessCheckCallback);
var lpwName : PWideChar;
    p1, p2, p3 : TMethod;
begin
  if Length(Name) = 0 then
    lpwName := nil
  else
    lpwName := @Name;

  ZeroMemory(@p1,sizeof(TMethod));
  ZeroMemory(@p2,sizeof(TMethod));
  ZeroMemory(@p3,sizeof(TMethod));

  if (authRM_Default in Flags) and
    (not JwIsPrivilegeSet(SE_SECURITY_NAME,pqt_Enabled)) then
    raise EJwsclPrivilegeNotFoundException.CreateFmtEx(
        RsSecureObjectsPrivilegeSecurityMissing2, 'Create',
        ClassName, RsUNSid, 0, False, []);

  if Flags = [] then
    Flags := [authRM_NoAudit];

  fOnAuthzFreeGroupsCallback := OnInternalAuthzFreeGroupsCallback;//debug
  //fOnAuthzFreeGroupsCallback := OnAuthzFreeGroupsCallback;
  if Assigned(fOnAuthzFreeGroupsCallback) then
    p1 := TMethod(fOnAuthzFreeGroupsCallback);

  fOnAuthzComputeGroupsCallback := OnAuthzComputeGroupsCallback;
  if Assigned(fOnAuthzComputeGroupsCallback) then
    p2 := TMethod(fOnAuthzComputeGroupsCallback);

  fOnAuthzAccessCheckCallback := OnAuthzAccessCheckCallback;
  if Assigned(fOnAuthzAccessCheckCallback) then
    p3 := TMethod(fOnAuthzAccessCheckCallback);

  if not AuthzInitializeResourceManager(
    TJwEnumMap.ConvertAuthZResourceManager(Flags),//__in   DWORD flags,
    p1.Code,//__in   PFN_AUTHZ_DYNAMIC_ACCESS_CHECK pfnAccessCheck,
    p2.Code,//__in   PFN_AUTHZ_COMPUTE_DYNAMIC_GROUPS pfnComputeDynamicGroups,
    p3.Code,//__in   PFN_AUTHZ_FREE_DYNAMIC_GROUPS pfnFreeDynamicGroups,
    lpwName,//__in   PCWSTR ResourceManagerName,
    @fHandle//__out  PAUTHZ_RESOURCE_MANAGER_HANDLE pAuthzResourceManager
  ) then
   raise EJwsclWinCallFailedException.CreateFmtEx(
      RsWinCallFailed, 'Create', ClassName,
      RsUNSid, 0, True, ['AuthzInitializeResourceManager']);
end;

destructor TJwAuthResourceManager.Destroy;
begin
  AuthzFreeResourceManager(Handle);
  fHandle := INVALID_HANDLE_VALUE; 
end;

procedure TJwAuthResourceManager.OnInternalAuthzFreeGroupsCallback(
  const pSidAttrArray: array of SID_AND_ATTRIBUTES);
begin
  //
end;



{ TJwAuthZAccessReply }

constructor TJwAuthZAccessReply.Create(
  const ReplyStruct: TAuthzAccessReply);
var i : Integer;
begin
  SetLength(fGrantedAccessMask, ReplyStruct.ResultListLength);
  SetLength(fSaclEvaluationResults, ReplyStruct.ResultListLength);
  SetLength(fError, ReplyStruct.ResultListLength);

  for i := 0 to ReplyStruct.ResultListLength-1 do
  begin
    fGrantedAccessMask[i] := TJwAccessMaskArray(ReplyStruct.GrantedAccessMask)[i];
    fSaclEvaluationResults[i] := TJwCardinalArray(ReplyStruct.SaclEvaluationResults)[i];
    fError[i] := TJwCardinalArray(ReplyStruct.Error)[i];
  end;
end;

{ TJwAuthZAccessRequest }

constructor TJwAuthZAccessRequest.Create(DesiredAccess: TJwAccessMask;
  PrincipalSelfSid: TJwSecurityID; ObjectTypeArray: TJwObjectTypeArray;
  Data: Pointer;Shared : TJwSharedHandle);
begin
  fDesiredAccess := DesiredAccess;
  fPrincipalSelfSid := TJwSecurityId.Create(PrincipalSelfSid);
  fObjectTypeArray := ObjectTypeArray;
  fData := Data;
  fShared := Shared;
end;

destructor TJwAuthZAccessRequest.Destroy;
begin
  FreeAndNil(fPrincipalSelfSid);
  fObjectTypeArray := nil;
end;

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}