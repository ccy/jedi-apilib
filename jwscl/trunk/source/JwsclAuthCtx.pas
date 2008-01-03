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
    var pSidAttrArray : array of SID_AND_ATTRIBUTES; //PSID_AND_ATTRIBUTES* pSidAttrArray,
    var pSidCount : DWORD;
    var pRestrictedSidAttrArray : array of SID_AND_ATTRIBUTES; //PSID_AND_ATTRIBUTES* pRestrictedSidAttrArray,
    var pRestrictedSidCount : DWORD
    ) : Boolean of object; stdcall;

  TOnAuthzFreeGroupsCallback = procedure(
    const pSidAttrArray : array of SID_AND_ATTRIBUTES
    ){of object}; stdcall;

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

    {@Name contains the error of the access check.
     Values are from
      http://msdn2.microsoft.com/en-us/library/aa376321(VS.85).aspx
     @unorderedlist(
      @item(ERROR_SUCCESS  All the access bits, not including MAXIMUM_ALLOWED, are granted and the GrantedAccessMask member is not zero.)
      @item(ERROR_PRIVILEGE_NOT_HELD DesiredAccess includes ACCESS_SYSTEM_SECURITY and the client does not have SeSecurityPrivilege.)
      @item(ERROR_ACCESS_DENIED Includes each of the following:
    * The requested bits are not granted.
    * MaximumAllowed bit is on and granted access is zero.
    * DesiredAccess is zero.)
      )
     }
    property Error : TJwCardinalArray read fError;
  end;

  TJwAuthResourceManager = class
  private
    fHandle : TAuthZResourceManagerHandle;
    fOnAuthzFreeGroupsCallback : TOnAuthzFreeGroupsCallback;
    fOnAuthzComputeGroupsCallback : TOnAuthzComputeGroupsCallback;
    fOnAuthzAccessCheckCallback : TOnAuthzAccessCheckCallback;
  protected
    {procedure OnInternalAuthzFreeGroupsCallback(
        const pSidAttrArray : array of SID_AND_ATTRIBUTES
        ); stdcall;}

    function OnInternalAuthzComputeGroupsCallback(
      hAuthzClientContext : AUTHZ_CLIENT_CONTEXT_HANDLE;
      Args : Pointer;
      var pSidAttrArray : array of SID_AND_ATTRIBUTES; //PSID_AND_ATTRIBUTES* pSidAttrArray,
      var pSidCount : DWORD;
      var pRestrictedSidAttrArray : array of SID_AND_ATTRIBUTES; //PSID_AND_ATTRIBUTES* pRestrictedSidAttrArray,
      var pRestrictedSidCount : DWORD
      ) : Boolean; stdcall;

    function OnInternalAuthzAccessCheckCallback(
      hAuthzClientContext : AUTHZ_CLIENT_CONTEXT_HANDLE;
      pAce : PACE_HEADER;
      pArgs : Pointer;
      var pbAceApplicable : Boolean) : Boolean;stdcall;


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

  TJwAuthContext = class;

  PJwCallbackData = ^TJwCallbackData;
  TJwCallbackData = record
    Hd : Integer;
    Context : TJwAuthContext;
    RM : TJwAuthResourceManager;
    UserData : Pointer;
  end;

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
    fAuthResourceManager : TJwAuthResourceManager;
    fDynamicGroupArgs : Pointer;

    fCallbackData : PJwCallbackData;
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


    { @param(Sid

      @raises(EJwsclWinCallFailedException will be raised if a call to
      AuthzInitializeContextFromSid failed.
      If GetLastError returns ERROR_INVALID_BLOCK, the reason is because
       a callback data block (TJwCallbackData) is invalid. This can only
       happen if someone changed the internal instance data.
      )

    }
    constructor CreateBySid(
      const ResourceManager : TJwAuthResourceManager;
      Flags : TAuthZSidContextFlags;
      const Sid : TJwSecurityId;
      const ExpirationTime : Int64;
      const DynamicGroupArgs : Pointer); overload;



    {@Name creates a new security context using an existing one.
     This function can also add additional positive and negative Sids to
     the context.

     @param(Sids receives a list of Sids and its Attributes to be used as
       new groups in the security context. (like TokenGroups in a token).

       The attributes (AttributesType) must be set
        to SE_GROUP_ENABLED (sidaGroupOwner) or
        SE_GROUP_USE_FOR_DENY_ONLY (sidaGroupUseForDenyOnly); otherwise
       the call to a WinAPI function wil fail with INVALID_PARAMETERS (87).

       Origin: http://msdn2.microsoft.com/en-us/library/aa375798.aspx
       @unorderedlist(
        @item(SE_GROUP_ENABLED - adds a group to the security context.
              It will be treated as if the User has entered a group.)
        @item(SE_GROUP_USE_FOR_DENY_ONLY - adds a group to the security context,
          but this group is only used for deny check. All positive ACE for
          this group in a DACL are ignored. Only Deny ACEs are recognized
          and can turn off other positive ACEs of other groups.)
        )
       )
     @param(RestrictedSids receives a list of Sids and its Attributes to be used as
       new deny only groups in the security context. (like TokenGroups in a token).

       The attributes (AttributesType) must be set
        to SE_GROUP_ENABLED (sidaGroupOwner) or
        SE_GROUP_USE_FOR_DENY_ONLY (sidaGroupUseForDenyOnly); otherwise
       the call to a WinAPI function wil fail with INVALID_PARAMETERS (87).

       Origin: http://msdn2.microsoft.com/en-us/library/aa375798.aspx
       @unorderedlist(
        @item(SE_GROUP_ENABLED - (probably) the same as parameter SID and
            attribute SE_GROUP_USE_FOR_DENY_ONLY.)
        @item(SE_GROUP_USE_FOR_DENY_ONLY - Results alway in Access Denied.
            However do not rely on me - maybe somebody with
            internal knowledge can comment it.)
       )
    }
    constructor CreateAndAddSids(
      const AuthContext : TJwAuthContext;
      const Sids : TJwSecurityIdList;
      const RestrictedSids : TJwSecurityIdList);



    destructor Destroy;



    {@Name
     @param(Request receives a class that contains information about the
      access check procedure. This instance is automatically freed
      if no exception was raised and Request.Shared is shOwned.

      Known Bug: If Request.ObjectTypeList is none nil the AuthZAccessCheck
        will always return "Invalid parameter".  
      )
     @param(OptionalSecurityDescriptorArray defines additional security descriptor
      which are used for the access check. They a simply added at the end
      of the primary security descriptor (logical order).
       The canonical ACE order is not enforced.
      Deny ACE in the optional security descriptors may be useless if a positive
      ACE could be found in the primary security descriptor.
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

    property AuthResourceManager : TJwAuthResourceManager read fAuthResourceManager;
  end;

  {$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

uses Math, JwsclEnumerations;
{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_INTERFACE_SECTION}

{$ENDIF SL_INTERFACE_SECTION}

const HEADERMAGIC = 12345678;
 
function OnInternalAuthzComputeGroupsCallback2(
      hAuthzClientContext : AUTHZ_CLIENT_CONTEXT_HANDLE;
      Args : Pointer;
      var pSidAttrArray : array of SID_AND_ATTRIBUTES; //PSID_AND_ATTRIBUTES* pSidAttrArray,
      var pSidCount : DWORD;
      var pRestrictedSidAttrArray : array of SID_AND_ATTRIBUTES; //PSID_AND_ATTRIBUTES* pRestrictedSidAttrArray,
      var pRestrictedSidCount : DWORD
      ) : Boolean; stdcall;
var CallbackData : PJwCallbackData;
begin
  CallbackData := PJwCallbackData(Args);
  if CallbackData.Hd <> HEADERMAGIC then
  begin
    SetLastError(ERROR_INVALID_BLOCK);
    result := false;
    exit;
  end;

  if Assigned(CallbackData) and
     Assigned(CallbackData.RM) and
     Assigned(CallbackData.RM.OnAuthzComputeGroupsCallback) then
    result := CallbackData.RM.OnAuthzComputeGroupsCallback(
        hAuthzClientContext, CallbackData.UserData, pSidAttrArray,
        pSidCount,pRestrictedSidAttrArray, pRestrictedSidCount)
  else
    result := false;
end;


function OnInternalAuthzAccessCheckCallback2(
  hAuthzClientContext : AUTHZ_CLIENT_CONTEXT_HANDLE;
  pAce : PACE_HEADER;
  pArgs : Pointer;
  var pbAceApplicable : Boolean) : Boolean;
var CallbackData : PJwCallbackData;
begin
  CallbackData := PJwCallbackData(pArgs);
  if CallbackData.Hd <> HEADERMAGIC then
  begin
    SetLastError(ERROR_INVALID_BLOCK);
    result := false;
    exit;
  end;

  if Assigned(CallbackData) and
     Assigned(CallbackData.RM) and
     Assigned(CallbackData.RM.OnAuthzAccessCheckCallback) then
    result := CallbackData.RM.OnAuthzAccessCheckCallback(
        hAuthzClientContext, pAce, CallbackData.UserData, pbAceApplicable)
  else
    result := false;
end;


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
  GetMem(fCallbackData, sizeof(TJwCallbackData));
  fCallbackData.Hd := HEADERMAGIC;
  fCallbackData.Context  := Self;
  fCallbackData.RM := ResourceManager;
  fCallbackData.UserData := DynamicGroupArgs;
  
  ZeroMemory(@luid, sizeof(TLuid));

  fDynamicGroupArgs := DynamicGroupArgs;
  fAuthResourceManager := ResourceManager;

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
        RsWinCallFailed, 'CreateByToken', ClassName,
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
  GetMem(fCallbackData, sizeof(TJwCallbackData));
  fCallbackData.Hd := HEADERMAGIC;
  fCallbackData.Context  := Self;
  fCallbackData.RM := ResourceManager;
  fCallbackData.UserData := DynamicGroupArgs;

  fAuthResourceManager := ResourceManager;
  fDynamicGroupArgs := DynamicGroupArgs;

  ZeroMemory(@luid, sizeof(TLuid));

  try
    if not AuthzInitializeContextFromSid(
        TJwEnumMap.ConvertAuthZSidContextFlags(Flags),//__in   DWORD Flags,
        Sid.SID,//__in   PSID UserSid,
        ResourceManager.Handle, //__in   AUTHZ_RESOURCE_MANAGER_HANDLE AuthzResourceManager,
        @ExpirationTime, //__in   PLARGE_INTEGER pExpirationTime,
        luid,//__in   LUID Identifier,
        fCallbackData,//__in   PVOID DynamicGroupArgs,
        @fHandle//__out  PAUTHZ_CLIENT_CONTEXT_HANDLE pAuthzClientContext
      ) then
     raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'CreateBySid', ClassName,
        RsUNSid, 0, True, ['AuthzInitializeContextFromSid']);
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
  if not Assigned(AuthContext) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'CreateByContext',
      ClassName, RsUNSid, 0, False, ['AuthContext']);
      
  fAuthResourceManager := AuthContext.AuthResourceManager;
  fDynamicGroupArgs := DynamicGroupArgs;

  GetMem(fCallbackData, sizeof(TJwCallbackData));
  fCallbackData.Hd := HEADERMAGIC;
  fCallbackData.Context  := Self;
  fCallbackData.RM := fAuthResourceManager;
  fCallbackData.UserData := DynamicGroupArgs;

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
        RsWinCallFailed, 'CreateByContext', ClassName,
        RsUNSid, 0, True, ['AuthzInitializeContextFromAuthzContext']);
  finally
  end;

  InitProperties;
end;


constructor TJwAuthContext.CreateAndAddSids(
  const AuthContext : TJwAuthContext;
  const Sids : TJwSecurityIdList;
  const RestrictedSids : TJwSecurityIdList);
var luid : TLuid;
    pSid, pRSid : PSidAndAttributesArray;
    tSid, tRSid : TSidAndAttributes;

    c1, c2 : Cardinal;
begin
  if not Assigned(AuthContext) then
    raise EJwsclNILParameterException.CreateFmtEx(
      RsNilParameter, 'CreateByContext',
      ClassName, RsUNSid, 0, False, ['AuthContext']);
      
  fAuthResourceManager := AuthContext.AuthResourceManager;
  fDynamicGroupArgs := AuthContext.fDynamicGroupArgs;

  GetMem(fCallbackData, sizeof(TJwCallbackData));
  fCallbackData.Hd := HEADERMAGIC;
  fCallbackData.Context  := Self;
  fCallbackData.RM := fAuthResourceManager;
  fCallbackData.UserData := fDynamicGroupArgs;

  ZeroMemory(@luid, sizeof(TLuid));


  pSid  := nil;
  pRSid := nil;
  c1 := 0;
  c2 := 0;

  if Assigned(Sids) then
  begin
    pSid := Sids.Create_PSID_Array;
    c1 := Sids.Count;
  end;

  if Assigned(RestrictedSids) then
  begin
    pRSid := RestrictedSids.Create_PSID_Array;
    c2 := RestrictedSids.Count;
  end;

 { asm
    int 3h;
  end; }
  try
     if not AuthzAddSidsToContext(
        AuthContext.Handle,//__in   AUTHZ_CLIENT_CONTEXT_HANDLE OrigClientContext,
        @pSid[0],//__in   PSID_AND_ATTRIBUTES Sids,
        c1,//__in   DWORD SidCount,
        @pRSid[0],//__in   PSID_AND_ATTRIBUTES RestrictedSids,
        c2,//__in   DWORD RestrictedSidCount,
        @fHandle//__out  PAUTHZ_CLIENT_CONTEXT_HANDLE pNewClientContext
      ) then
     raise EJwsclWinCallFailedException.CreateFmtEx(
        RsWinCallFailed, 'CreateAndAddSids', ClassName,
        RsUNSid, 0, True, ['AuthzAddSidsToContext']);
  finally
    if Assigned(Sids) then
      Sids.Free_PSID_Array(pSid);
    if Assigned(RestrictedSids) then
      RestrictedSids.Free_PSID_Array(pRSid);
  end;

  InitProperties;
end;





destructor TJwAuthContext.Destroy;
begin
  AuthzFreeContext(fHandle);
  fHandle := INVALID_HANDLE_VALUE;
  FreeMem(fCallbackData);

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

var pRequest : AUTHZ_ACCESS_REQUEST;
    pSD : PSecurityDescriptor;
    pOSD : array of PSECURITY_DESCRIPTOR;

    pReply : AUTHZ_ACCESS_REPLY;
    i : Integer;
begin
  Reply := nil;
  AuthZHandle := 0;
  
  ZeroMemory(@pRequest, sizeof(pRequest));
  pRequest.DesiredAccess    := Request.fDesiredAccess;
  pRequest.PrincipalSelfSid := Request.fPrincipalSelfSid.SID;
  pRequest.ObjectTypeList   := @Request.fObjectTypeArray[0];
  pRequest.ObjectTypeListLength := Length(Request.fObjectTypeArray);
  pRequest.OptionalArguments := Request.fData;

  pSD := nil;
  if Assigned(SecurityDescriptor) Then
    pSD := SecurityDescriptor.Create_SD();

  SetLength(pOSD, Length(OptionalSecurityDescriptorArray));
  for i := 0 to Length(OptionalSecurityDescriptorArray)-1 do
  begin
    pOSD[i] := OptionalSecurityDescriptorArray[i].Create_SD(false);
  end;


  ZeroMemory(@pReply, sizeof(pReply));

  pReply.ResultListLength := pRequest.ObjectTypeListLength;
  if pRequest.ObjectTypeListLength = 0 then
    pReply.ResultListLength := 1;

  GetMem(pReply.GrantedAccessMask,
    (pReply.ResultListLength+1) * sizeof(ACCESS_MASK));
  GetMem(pReply.SaclEvaluationResults,
    (pReply.ResultListLength+1) * sizeof(DWORD));
  GetMem(pReply.Error,
    (pReply.ResultListLength+1) * sizeof(DWORD));

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
        @pOSD[0],//__in_opt  PSECURITY_DESCRIPTOR* OptionalSecurityDescriptorArray,
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

  //fOnAuthzFreeGroupsCallback := OnInternalAuthzFreeGroupsCallback2;//debug
  //fOnAuthzFreeGroupsCallback := OnAuthzFreeGroupsCallback;


  //fOnAuthzComputeGroupsCallback := OnInternalAuthzComputeGroupsCallback2; //debug
  fOnAuthzComputeGroupsCallback := OnAuthzComputeGroupsCallback;
  //if Assigned(fOnAuthzComputeGroupsCallback) then
    //p2 := TMethod(fOnAuthzComputeGroupsCallback);


  fOnAuthzAccessCheckCallback := OnInternalAuthzAccessCheckCallback;
  //fOnAuthzAccessCheckCallback := OnAuthzAccessCheckCallback;
  if Assigned(fOnAuthzAccessCheckCallback) then
    p3 := TMethod(fOnAuthzAccessCheckCallback);


  if not AuthzInitializeResourceManager(
    TJwEnumMap.ConvertAuthZResourceManager(Flags),//__in   DWORD flags,
    @OnInternalAuthzAccessCheckCallback2,//__in   PFN_AUTHZ_DYNAMIC_ACCESS_CHECK pfnAccessCheck,
    @OnInternalAuthzComputeGroupsCallback2,//__in   PFN_AUTHZ_COMPUTE_DYNAMIC_GROUPS pfnComputeDynamicGroups,
    @OnAuthzFreeGroupsCallback,//__in   PFN_AUTHZ_FREE_DYNAMIC_GROUPS pfnFreeDynamicGroups,
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

function TJwAuthResourceManager.OnInternalAuthzAccessCheckCallback(
  hAuthzClientContext: AUTHZ_CLIENT_CONTEXT_HANDLE; pAce: PACE_HEADER;
  pArgs: Pointer; var pbAceApplicable: Boolean): Boolean;
begin
//
end;

function TJwAuthResourceManager.OnInternalAuthzComputeGroupsCallback(
  hAuthzClientContext: AUTHZ_CLIENT_CONTEXT_HANDLE; Args: Pointer;
  var pSidAttrArray: array of SID_AND_ATTRIBUTES;
  var pSidCount: DWORD;
  var pRestrictedSidAttrArray: array of SID_AND_ATTRIBUTES;
  var pRestrictedSidCount: DWORD): Boolean;
begin
  //
  result := false;
end;

{procedure TJwAuthResourceManager.OnInternalAuthzFreeGroupsCallback(
  const pSidAttrArray: array of SID_AND_ATTRIBUTES);
begin
  //
end;}



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