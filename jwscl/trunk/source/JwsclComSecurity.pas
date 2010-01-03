{
Description
Project JEDI Windows Security Code Library (JWSCL)

<Description here>

Author
<Author name>

License
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

Note
The Original Code is Jwscl<UnitName>.pas.

The Initial Developer of the Original Code is <Author Name>


TODO:
1.
CoCreateInstance and Integrity Levels
http://msdn.microsoft.com/en-us/ms679687%28VS.85%29.aspx
2. Implement CoCreateInstanceEx with impersonation

}
unit JwsclComSecurity;

{$INCLUDE ..\includes\Jwscl.inc}

//TODO: String -> TJwString

interface
uses
  ActiveX,
  JwaWinNT, //only TEMP

  JwaWindows,      //JEDI API unit

  //temp
  ComSvcs,
  ComObj,
  //

  JwaCOMSecurity,

  SiAuto,

  //more custom units here
  Classes,
  SysUtils,
  Registry,
  Dialogs,


  //more  JWSCL units here
  JwsclAuthCtx,
  JwsclAcl,
  JwsclToken,
  JwsclDescriptor,
  JwsclSid,
  JwsclVersion,
  JwsclUtils,      //utility functions like thread, localization, memory, exception utils
  JwsclExceptions, //exception classes
  JwsclEnumerations,
  JwsclKnownSid,
  JwsclMapping,
  JwsclSecureObjects,
  JwsclResource,   //resource strings
  JwsclConstants,  //all JWSCL constant
  JwsclTypes,      //JWSCL types
  JwsclStrings;    //TJwString, TJwPChar, JwCompareString, JwFormatString, LoadLocalizedString


type
  TJwAuthInfoType = (aitUnknown, aitWinNTAuthIdentity, aitWinNTAuthIdentityEx, aitCertContext);
  TJwComCustomSecurity = class;
  TJwComWinNTIdentity = class;

  PJwAuthInfo = ^TJwAuthInfo;
  TJwAuthInfo = record
    case TJwAuthInfoType of
      aitWinNTAuthIdentity : (WinNTAuthInfo   : TSecWinNTAuthIdentityW);
      aitWinNTAuthIdentityEx : (WinNTAuthInfoEx : TSecWinNTAuthIdentityExW);
      aitCertContext : (CertContext : TCertContext)
  end;


  TJwComCustomSecurity = class
  protected
    fUpdating,
    fReadOnly : Boolean;

    fAuthenticationInfo: RPC_AUTH_IDENTITY_HANDLE;
    fServerPrincipalName: String;
    fCapabilites: TJwComAuthenticationCapabilities;
    fAuthenticationLevel: TJwComAuthenticationLevel;
    fAuthorizationService: TJwComAuthorizationService;
    fImpersonationLevel: TJwComImpersonationLevel;
    fAuthenticationService: TJwComAuthenticationService;
    fProxy: IInterface;
  protected

    function GetAuthenticationInfo: RPC_AUTH_IDENTITY_HANDLE;
    function GetAuthenticationLevel: TJwComAuthenticationLevel;
    function GetAuthenticationService: TJwComAuthenticationService;
    function GetAuthorizationService: TJwComAuthorizationService;
    function GetImpersionationLevel: TJwComImpersonationLevel;
    function GetReadOnlyProperties: Boolean;
    function GetServerPrincipalName: String;
    procedure SetAuthenticationInfo(const Value: RPC_AUTH_IDENTITY_HANDLE);
    procedure SetAuthenticationLevel(const Value: TJwComAuthenticationLevel);
    procedure SetAuthenticationService(const Value: TJwComAuthenticationService);
    procedure SetAuthorizationService(const Value: TJwComAuthorizationService);
    procedure SetCapabilites(const Value: TJwComAuthenticationCapabilities);
    procedure SetImpersonationLevel(const Value: TJwComImpersonationLevel);
    procedure SetReadOnlyProperties(const Value: Boolean);
    procedure SetServerPrincipalName(const Value: String);



  protected
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    procedure CheckReadonly(const PropertyName : String);

    class procedure CoInitializeSecurity(
        {__in_opt}  pSecDesc : {PSECURITY_DESCRIPTOR }Pointer;
        {__in}      cAuthSvc : LONG;
        {__in_opt}  asAuthSvc : PSOLE_AUTHENTICATION_SERVICE;
        {__in}      dwAuthnLevel : TJwComAuthenticationLevel;
        {__in}      dwImpLevel : TJwComImpersonationLevel;
        {__in_opt}  pAuthList : Pointer;
        {__in}      dwCapabilities : TJwComAuthenticationCapabilities;
        IgnoreProcess : Boolean
      ); virtual;


    class function CreateAuthInfo(const InfoType : TJwAuthInfoType) : PJwAuthInfo; virtual;

    property ReadOnlyProperties : Boolean read GetReadOnlyProperties;

    //http://msdn.microsoft.com/en-us/library/ms692656%28VS.85%29.aspx
    property AuthenticationService : TJwComAuthenticationService read GetAuthenticationService write SetAuthenticationService;
    property AuthorizationService : TJwComAuthorizationService read GetAuthorizationService write SetAuthorizationService;
    property ServerPrincipalName : String read GetServerPrincipalName write SetServerPrincipalName;
    property AuthenticationLevel : TJwComAuthenticationLevel read GetAuthenticationLevel write SetAuthenticationLevel;

    //http://msdn.microsoft.com/en-us/library/ms693790(VS.85).aspx
    property ImpersionationLevel : TJwComImpersonationLevel read GetImpersionationLevel write SetImpersonationLevel;
    property AuthenticationInfo : RPC_AUTH_IDENTITY_HANDLE read GetAuthenticationInfo write SetAuthenticationInfo;
    property Capabilites  : TJwComAuthenticationCapabilities read fCapabilites write SetCapabilites;
  end;

  TJwComClientSecurity = class(TJwComCustomSecurity)
  private
    fWinNTIdentity: TJwComWinNTIdentity;

  protected
    fProxy: IInterface;
    procedure AuthWinNT(const Domain, User, Password : string);

    class procedure CoCopyProxy(
      {__in}   pProxy : IUnknown;
      {__out}  out ppCopy : IUnknown
    ); virtual;


    class procedure CoSetProxyBlanket(
        {__in}      pProxy : IUnknown;
        {__in}      dwAuthnSvc : TJwComAuthenticationService;
        {__in}      dwAuthzSvc : TJwComAuthorizationService;
        {__in_opt}  pServerPrincName : String;
        {__in}      AuthnLevel : TJwComAuthenticationLevel;
        {__in}      dwImpLevel : TJwComImpersonationLevel;
        {__in_opt}  pAuthInfo : Pointer;
        {__in}      dwCapabilities : TJwComAuthenticationCapabilities
      ); virtual;
  public
    class procedure CoQueryProxyBlanket(
        {__in}       pProxy : IUnknown;
        {__out_opt}  out pwAuthnSvc : TJwComAuthenticationService;
        {__out_opt}  out pAuthzSvc : TJwComAuthorizationService;
        {__out_opt}  out pServerPrincName : String;
        {__out_opt}  out pAuthnLevel : TJwComAuthenticationLevel;
        {__out_opt}  out pImpLevel : TJwComImpersonationLevel;
        {__out_opt}  out pAuthInfo : RPC_AUTH_IDENTITY_HANDLE;
        {__out_opt}  out pCapabilites : TJwComAuthenticationCapabilities
    ); virtual;


  public
    constructor Create(const ProxyInterface : IInterface; MakeCopy : Boolean); overload;
    constructor Create(const Proxy : TJwComClientSecurity); overload;//private kopie von einstellungen

    destructor Destroy; override;

    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    procedure UpdateProxyInfo;

    {
    Remarks
      With WinNT authorization any call that is made on the COM interface needs the SE_IMPERSONATE_PRIVILEGE.
      The function checks for this privilege once and throws EJwsclPrivilegeNotFoundException if the privilege
      is not enabled.

    Example
      <code lang="delphi">
        var ComSec : TJwComClientSecurity;
        begin
          JwEnablePrivilege(SE_IMPERSONATE_NAME, pst_Enable);
          ComSec := TJwComClientSecurity.Create(MyInterfacePointer, False);
          ComSec.AuthenticationService := asWinNT;
          ComSec.AuthenticationLevel := calDefault;
          ComSec.ImpersionationLevel := cilIdentify; //or cilImpersonate
          ComSec.AuthorizationService := azsDefault;
          ComSec.SetWinNTIdentity(TJwComWinNTIdentity.Create('Domain', 'User', 'Password'));

          MyInterfacePointer.Call;
        end;
    }
    procedure SetWinNTIdentity(const Value: TJwComWinNTIdentity);

    property Proxy : IInterface read fProxy;

    property WinNTIdentity : TJwComWinNTIdentity read fWinNTIdentity;

    property ReadOnlyProperties;

    //http://msdn.microsoft.com/en-us/library/ms692656%28VS.85%29.aspx
    property AuthenticationService;
    property AuthorizationService;
    property ServerPrincipalName;
    property AuthenticationLevel;

    //http://msdn.microsoft.com/en-us/library/ms693790(VS.85).aspx
    property ImpersionationLevel;
    property AuthenticationInfo;
    property Capabilites;


  end;

  TJwAuthenticationInfo = class
  protected
    fAuthenticationService : TJwComAuthenticationService;
    fAuthorizationService : TJwComAuthorizationService;
    fAuthenticationInfo : Pointer;
    fWinNTIdentity : TJwComWinNTIdentity;
    fAutoDestroy : Boolean;
  public
    {
    Parameters
      AuthenticationInfo A pointer to a authentication structure. Depends on parameter AuthenticationService.
              This pointer must be kept alive during the life time of the proxy it is assigned to. And if
              used in CoInitializeSecurity or TJwComProcessSecurity.Initialize it must be available
              until CoUnitilized was called.
      AutoDestroy The memory is freed automatically if this parameter is true. The memory must be allocated using CoTaskMemAlloc
    }
    constructor Create(
      const AuthenticationService : TJwComAuthenticationService;
      const AuthorizationService : TJwComAuthorizationService;
      const AuthenticationInfo : Pointer;
      const AutoDestroy : boolean = false); overload;

    {
    Parameters
      AuthenticationService Must be asWinNT or asGSSKerberos.

      AuthenticationInfo A TJwComWinNTIdentity class that contains information about the identity.
      AutoDestroy The class is freed automatically if this parameter is true.
    Remarks
      AuthenticationInfo must be allocated by CoTaskMemAlloc.
    }
    constructor Create(
      const AuthenticationService : TJwComAuthenticationService;
      const AuthorizationService : TJwComAuthorizationService;
      const AuthenticationInfo : TJwComWinNTIdentity;
      const AutoDestroy : boolean = false); overload;

    {
    }
    constructor CreateWinNT(
      const UserName  : String;
      const Domain    : String;
      const Password  : string;
      const AuthorizationService : TJwComAuthorizationService); overload;

    destructor Destroy; override;

    property AuthenticationService : TJwComAuthenticationService read fAuthenticationService;
    property AuthorizationService : TJwComAuthorizationService read fAuthorizationService;
    property AuthenticationInfo : Pointer read fAuthenticationInfo;
    property WinNTIdentity : TJwComWinNTIdentity read fWinNTIdentity;

    {AutoDestroy defines whether the properties WinNTIdentity and AuthenticationInfo
    are freed automatically or not (FALSE).

     Remarks
      AuthenticationInfo must be allocated by CoTaskMemAlloc.
    }
    property AutoDestroy : Boolean read fAutoDestroy write fAutoDestroy;
  end;

  //A list of authentication information class instances.
  TJwAuthenticationInfoList = array of TJwAuthenticationInfo;

  {A Pointer to TJwSecurityInitializationData.}
  PJwSecurityInitializationData = ^TJwSecurityInitializationData;
  {TJwSecurityInitializationData is a variable record
   That contains either a security descriptor class,
    a pointer to an IAccessControl interface or
    a GUID.
   It is used by TJwComProcessSecurity.Initialize
  }
  TJwSecurityInitializationData = record
    case Integer of
      0 : (SD : TJwSecurityDescriptor);
      1 : (IA : ^IAccessControl);
      2 : (ID : TGUID);
  end;

  {TJwComProcessSecurity provides wrapper methods for CoInitializeSecurity.
   They can bei used by a COM server or client. Some of them are only
   for either servers or clients.

   For an application that is COM server and client, use the Initialize method with
    parameter SecurityData : PJwSecurityInitializationData.

  }
  TJwComProcessSecurity = class(TJwComCustomSecurity)
  protected
  public
    {Creates a security descriptor with following preferences:
      User  : Local Administrators (Group)
      Group : Local Administrators (Group)
      DACL : Allow $FFFF (All specific access rights) to Administrators, SYSTEM and Authenticated User
         The generic and standard rights are not set.
    }
    class function CreateMinimumCOMSecurityDescriptor(const MergeSD : TJwSecurityDescriptor = nil) : TJwSecurityDescriptor; virtual;

    {Initialize is the main class procedure to initialize the process wide security settings for a client and server process using
      CoInitializeSecurity.

     Parameters
       SecurityData Defines a pointer to a TJwSecurityInitializationData record that contains either an AppID, an IAccessControl interface or a security descriptor.
          The content depends on the capabilities parameter. If acAccessControl or acAppId is set the member IA or ID of the record
          will be used; otherwise the SD member will be used. This parameter can be nil to use default security permission (default access permission key).

          However the data is not freed automatically.

       AuthenticationList A list of authentication services and identities. This list is automatically
          freed if parameter AutoDestroy is true. Make sure that also the AutoDestroy property of
          any TJwAuthenticationInfo member in the array is set to true so all memory is freed automatically.
          This parameter cannot be nil.

       AutoDestroy Set to true to free the AuthenticationList.
    Remarks
      Use this call only for a COM Server that is also a COM client.

     Exceptions
       EJwsclInvalidParameterException This exception will be raised if :
                      1. a member of AuthenticationList has the asWinNT or asGSSKerberos
                      flag set and AuthorizationService is azsDefault.
                      2. AuthenticationLevel is calNone but SecurityData contains none nil security information.
                      3. Capabilities contains acAccessControl and acAppId

       EJwsclAccessDenied This exception will be raised if a security descriptor is supplied which does not
          allow SYSTEM full access. COM cannot work then.

       EJwsclProcessNotFound This exception will be raised if IgnoreProcess is true and the current process' name
          can be found in global variable JwKnownComHostProcesses.

       EJwsclCoInitializeNotCalledException CoInitialize was not called or CoInitializeSecurity was already called.
          You need to call CoInitialize first or call CoUnitialize, CoInitialize and then again this method.

       EJwsclComException CoInitializeSecurity reported an error.
    }
    class procedure Initialize(
          SecurityData : PJwSecurityInitializationData;
          var AuthenticationList : TJwAuthenticationInfoList;
          AuthenticationLevel: TJwComAuthenticationLevel;
          ImpersonationLevel : TJwComImpersonationLevel;
          Capabilities : TJwComAuthenticationCapabilities;
          const AutoDestroy : Boolean;
          const IgnoreProcess : Boolean = false); overload; virtual;

    {Initialize initializes the process wide security settings for a server or client process using
      CoInitializeSecurity.


     Exceptions
       EJwsclProcessNotFound This exception will be raised if IgnoreProcess is true and the current process' name
          can be found in global variable JwKnownComHostProcesses.
          This exception can be disabled by setting JwIgnoreHostProcessesInServer to false.

       EJwsclCoInitializeNotCalledException CoInitialize was not called or CoInitializeSecurity was already called.
          You need to call CoInitialize first or call CoUnitialize, CoInitialize and then again this method.

       EJwsclComException CoInitializeSecurity reported an error.
    }
    class procedure Initialize(
          AuthenticationLevel : TJwComAuthenticationLevel;
          ImpersonationLevel : TJwComImpersonationLevel;
          Capabilities : TJwComAuthenticationCapabilities); overload; virtual;


    {Initialize initializes the process wide security settings for a server process using
      CoInitializeSecurity and a security descriptor.

      Remarks:
        Server only

     Exceptions
       EJwsclInvalidParameterException This exception will be raised if :
                      1. AuthenticationLevel is calNone

       EJwsclAccessDenied This exception will be raised if a security descriptor is supplied which does not
          allow SYSTEM full access. COM cannot work then.

       EJwsclProcessNotFound This exception will be raised if IgnoreProcess is true and the current process' name
          can be found in global variable JwKnownComHostProcesses. This exception can be disabled by setting JwIgnoreHostProcessesInServer to false.

       EJwsclCoInitializeNotCalledException CoInitialize was not called or CoInitializeSecurity was already called.
          You need to call CoInitialize first or call CoUnitialize, CoInitialize and then again this method.

       EJwsclComException CoInitializeSecurity reported an error.
    }
    class procedure Initialize(
          SecurityDescriptor : TJwSecurityDescriptor;
          AuthenticationLevel : TJwComAuthenticationLevel;
          ImpersonationLevel : TJwComImpersonationLevel;
          Capabilities : TJwComAuthenticationCapabilities); overload; virtual;


    {Initialize initializes the process wide security settings for a server process using
      CoInitializeSecurity and a security descriptor.

     Exceptions
       EJwsclInvalidParameterException This exception will be raised if :
                      1. AuthenticationLevel is calNone


       EJwsclAccessDenied This exception will be raised if a security descriptor is supplied which does not
          allow SYSTEM full access. COM cannot work then. This exception can be disabled by setting JwIgnoreHostProcessesInServer to false.

       EJwsclProcessNotFound This exception will be raised if IgnoreProcess is true and the current process' name
          can be found in global variable JwKnownComHostProcesses.

       EJwsclCoInitializeNotCalledException CoInitialize was not called or CoInitializeSecurity was already called.
          You need to call CoInitialize first or call CoUnitialize, CoInitialize and then again this method.

       EJwsclComException CoInitializeSecurity reported an error.
    }
    class procedure Initialize(
          AppID : TGUID;
          AuthenticationLevel : TJwComAuthenticationLevel;
          ImpersonationLevel : TJwComImpersonationLevel;
          Capabilities : TJwComAuthenticationCapabilities); overload; virtual;


    {Initialize initializes the process wide security settings for a server process using
      CoInitializeSecurity and a security descriptor.

     Exceptions
       EJwsclInvalidParameterException This exception will be raised if :
                      1. AuthenticationLevel is calNone

       EJwsclAccessDenied This exception will be raised if a security descriptor is supplied which does not
          allow SYSTEM full access. COM cannot work then.

       EJwsclProcessNotFound This exception will be raised if IgnoreProcess is true and the current process' name
          can be found in global variable JwKnownComHostProcesses. This exception can be disabled by setting JwIgnoreHostProcessesInServer to false.

       EJwsclCoInitializeNotCalledException CoInitialize was not called or CoInitializeSecurity was already called.
          You need to call CoInitialize first or call CoUnitialize, CoInitialize and then again this method.

       EJwsclComException CoInitializeSecurity reported an error.
    }
    class procedure Initialize(
          AccessControl : IAccessControl;
          AuthenticationLevel : TJwComAuthenticationLevel;
          ImpersonationLevel : TJwComImpersonationLevel;
          Capabilities : TJwComAuthenticationCapabilities); overload; virtual;


     {Initialize is the main class procedure to initialize the process wide security settings for a client using
      CoInitializeSecurity.

     Parameters
       AuthenticationList A list of authentication services and identities. This list is automatically
          freed if parameter AutoDestroy is true. Make sure that also the AutoDestroy property of
          any TJwAuthenticationInfo member in the array is set to true so all memory is freed automatically.
          This parameter cannot be nil.

       AutoDestroy Set to true to free the AuthenticationList.
    Remarks
      Use this call only for a COM Server that is also a COM client.

     Exceptions
       EJwsclInvalidParameterException This exception will be raised if :
                      1. a member of AuthenticationList has the asWinNT or asGSSKerberos
                      flag set and AuthorizationService is azsDefault.
                      2. AuthenticationLevel is calNone but SecurityData contains none nil security information.
                      3. Capabilities contains acAccessControl and acAppId

       EJwsclAccessDenied This exception will be raised if a security descriptor is supplied which does not
          allow SYSTEM full access. COM cannot work then.

       EJwsclProcessNotFound This exception will be raised if IgnoreProcess is true and the current process' name
          can be found in global variable JwKnownComHostProcesses. This exception can be disabled by setting JwIgnoreHostProcessesInServer to false.

       EJwsclCoInitializeNotCalledException CoInitialize was not called or CoInitializeSecurity was already called.
          You need to call CoInitialize first or call CoUnitialize, CoInitialize and then again this method.

       EJwsclComException CoInitializeSecurity reported an error.
    }
    class procedure Initialize(
          var AuthenticationList : TJwAuthenticationInfoList;
          AuthenticationLevel: TJwComAuthenticationLevel;
          ImpersonationLevel : TJwComImpersonationLevel;
          Capabilities : TJwComAuthenticationCapabilities;
          const AutoDestroy : Boolean); overload; virtual;




    {
      Create initializes the process wide security settings for a client process.

      Remarks
       It creates an authentication list with asWinNT authentication service set to the
       given user's identity (Domain, UserName, Password).

      Exceptions
       EJwsclInvalidParameterException This exception will be raised if :
                      1. AuthenticationLevel is calNone
                      2. Capabilities contains acAccessControl and acAppId
                      3. AuthorizationService is azsDefault

       EJwsclAccessDenied This exception will be raised if a security descriptor is supplied which does not
          allow SYSTEM full access. COM cannot work then.

       EJwsclProcessNotFound This exception will be raised if IgnoreProcess is true and the current process' name
          can be found in global variable JwKnownComHostProcesses. This exception can be disabled by setting JwIgnoreHostProcessesInServer to false.

       EJwsclCoInitializeNotCalledException CoInitialize was not called or CoInitializeSecurity was already called.
          You need to call CoInitialize first or call CoUnitialize, CoInitialize and then again this method.

       EJwsclComException CoInitializeSecurity reported an error.
    }
    class procedure Initialize(
          const Domain, UserName, Password: String;
          const AuthorizationService: TJwComAuthorizationService;
          const AuthenticationLevel: TJwComAuthenticationLevel;
          const ImpersonationLevel: TJwComImpersonationLevel;
          const Capabilities: TJwComAuthenticationCapabilities); overload; virtual;

  end;


  TServerImpersonationType = (
    //impersonate on creation, revert on destroying
    sitAutoImpersonation,
    //impersonate on creation, but do not revert
    sitOnlyImpersonation,
    //do not impersonate at all
    sitNoImpersonation);


  TJwComWinNTIdentity = class
  private
    function GetDomain: String;
    function GetFlags: DWORD;
    function GetPackageList: String;
    function GetPassword: String;
    function GetUser: String;
    function GetVersion: DWORD;
    procedure SetDomain(const Value: String);
    procedure SetFlags(const Value: DWORD);
    procedure SetPackageList(const Value: String);
    procedure SetPassword(const Value: String);
    procedure SetUser(const Value: String);
    procedure SetVersion(const Value: DWORD);
    function GetData: PSecWinNTAuthIdentityW;
    function GetDataEx: PSecWinNTAuthIdentityExW;
  protected
    fReadOnly : Boolean;
    fDataEx : TSecWinNTAuthIdentityExW;
    fData : TSecWinNTAuthIdentityW;

    procedure CheckReadonly(const PropertyName : String);
  public
    constructor Create; overload;
    constructor Create(const Identity : TSecWinNTAuthIdentityExW; ReadOnly : Boolean = false); overload;
    constructor Create(const Identity : TSecWinNTAuthIdentityW; ReadOnly : Boolean = false); overload;

    constructor Create(const Domain, UserName, Password : string); overload;

    destructor Destroy; override;

    property Version  : DWORD read GetVersion write SetVersion;
    property User     : String read GetUser write SetUser;
    property Domain   : String read GetDomain write SetDomain;
    property Password : String read GetPassword write SetPassword;
    property Flags    : DWORD read GetFlags write SetFlags;
    property PackageList : String read GetPackageList write SetPackageList;

    property ReadOnly : Boolean read fReadOnly;

    property AuthorizationInfoEx : PSecWinNTAuthIdentityExW read GetDataEx;
    property AuthorizationInfo : PSecWinNTAuthIdentityW read GetData;
  end;

  {TJwComServerSecurity provides functionality
  for a server to query client information, to impersonate
  and to do access checks.
  }
  TJwComServerSecurity = class(TJwComCustomSecurity)
  protected
    function GetToken: TJwSecurityToken;
    function GetUserName: String;
  private
    procedure SetAuthContext(const Value: TJwAuthContext);
    procedure SetAuthManager(const Value: TJwAuthResourceManager);

  protected
    fAuthManager : TJwAuthResourceManager;
    fAuthContext : TJwAuthContext;

    fToken : TJwSecurityToken;
    fWinNTIdentity : TJwComWinNTIdentity;

     //server
    class procedure CoQueryClientBlanket(
        {__out_opt}  out pwAuthnSvc : TJwComAuthenticationService;
        {__out_opt}  out pAuthzSvc : TJwComAuthorizationService;
        {__out_opt}  out pServerPrincName : String;
        {__out_opt}  out pAuthnLevel : TJwComAuthenticationLevel;
        {__out_opt}  out pImpLevel : TJwComImpersonationLevel;
                     out pPrivs : RPC_AUTHZ_HANDLE;
        {__out_opt}  var pCapabilites : TJwComAuthenticationCapabilities
    ); virtual;
  published

  public
    {Creates a COM server instance.

     Remarks
       All properties are read only. Trying to set a value raises EJwsclReadOnlyPropertyException.
    }
    constructor Create(const ImpersonationType : TServerImpersonationType = sitAutoImpersonation);
    destructor Destroy; override;

    {

    }
    procedure AccessCheck(
        Flags : Cardinal;
        const DesiredAccess : DWORD;
        const SecurityDescriptor : TJwSecurityDescriptor;
        const GenericMapping: TJwSecurityGenericMappingClass;
        out AccessGranted : Boolean;
        out GrantedAccessMask : DWORD
        ); virtual;

    procedure AccessCheckCached(
        var CacheResult : TAuthZAccessCheckResultHandle;
        Flags : Cardinal;
        const DesiredAccess : DWORD;
        const SecurityDescriptor : TJwSecurityDescriptor;
        const GenericMapping: TJwSecurityGenericMappingClass;
        out AccessGranted : Boolean;
        out GrantedAccessMask : DWORD
        ); virtual;


    {Impersonates the current thread and removes any previous thread token.

     Remarks
       Use this call instead of CoImpersonateClient because it raises an exception if the call fails.
       Without checking the return value any subsequent call will run on the process token which
       is a security breach.

     Exceptions
      EJwsclWinCallFailedException CoImpersonateClient failed.
    }
    class procedure ImpersonateClient;

    {Removes the thread token.

    Exceptions
      EJwsclWinCallFailedException CoRevertToSelf failed.
    }
    class procedure RevertToSelf;

    {Returns true if the current thread has a token assigned (is impersonating);
     otherwise false.

     The function does not distuingish between a client token connected to the server
     and any other assigned token.
    }
    class function IsImpersonating : Boolean;

    {WinNTIdentity contains the user's identity if the property AuthenticationService
     is asWinNT or asGSSKerberos; otherwise it is nil.

     This instance is cached by the class and must not be freed.
    }
    property WinNTIdentity : TJwComWinNTIdentity read fWinNTIdentity;

    {Returns the user's name connected to the server.
     Either it uses the current thread token or it impersonates the user
     if no thread token is available.
     If ImpersonationLevel is cilAnonymous the return value is an empty string.
    }
    property UserName : String read GetUserName;

    {Returns the client token.

     This instance is cached by the class and must not be freed.

     A first read call impersonates the user and removes a previously set thread token.
     Therefore all subsequent calls will run on the process token.

     If ImpersonationLevel is cilAnonymous an exception EJwsclNoThreadTokenAvailable is raised.
    }
    property Token : TJwSecurityToken read GetToken;

    property AuthenticationService;
    property AuthorizationService;
    property AuthenticationLevel;
    property ImpersionationLevel;

    property AuthenticationInfo;
    property Capabilites;

    {The authentication manager used by AccessCheck.
     This value is nil if AccessCheck wasn't called yet.

     On write the current instance of the authentication manager is freed and
     the value is directly written to the internal instance variable.
     So do not free it yourself, the class instance will do it.

     Do not free the instance and then call AccessCheckCached.
    }
    property AuthManager : TJwAuthResourceManager read fAuthManager write SetAuthManager;

    {The authentication context used by AccessCheck.
    This value is nil if AccessCheck wasn't called yet.

    On write the current instance of the authentication context is freed and
     the value is directly written to the internal instance variable.
     So do not free it yourself, the class instance will do it.

     Do not free the instance and then call AccessCheckCached.
    }
    property AuthContext : TJwAuthContext read fAuthContext write SetAuthContext;
  end;

  {TJwComRegistrySecurity provides functionality to retrieve
   COM related security information from registry.

   It allows to read machine wide security information.

   COM applications can read and write security related information with it.
   }
  TJwComRegistrySecurity = class
  private
    fAppID: TGuid;
    function GetAccessPermission: TJwSecurityDescriptor;
    function GetAuthenticationLevel: TJwComAuthenticationLevel;
    function GetLaunchPermission: TJwSecurityDescriptor;
    function GetLoadUserProfile: Boolean;
    function GetRunAs: TJwSecurityID;
    function GetServiceParameters: String;
    function GetSRPTrustLevel: TJwSaferLevelId;
    procedure SetAccessPermission(const Value: TJwSecurityDescriptor);
    procedure SetAuthenticationLevel(const Value: TJwComAuthenticationLevel);
    procedure SetLaunchPermission(const Value: TJwSecurityDescriptor);
    procedure SetLoadUserProfile(const Value: Boolean);
    procedure SetRunAs(const Value: TJwSecurityID);
    procedure SetServiceParameters(const Value: String);
    procedure SetSRPTrustLevel(const Value: TJwSaferLevelId);
    function GetAppIdRegFlags: TJwComAppIdRegFlags;
    procedure SetAppIdRegFlags(const Value: TJwComAppIdRegFlags);
    function GetRunAsString: String;
    procedure SetRunAsString(const Value: String);
  protected
    fLaunchPermission : TJwSecurityDescriptor;
    fAccessPermission : TJwSecurityDescriptor;
    fRunAs : TJwSecurityID;
    fServiceParameters : String;
    fAuthenticationLevel : TJwComAuthenticationLevel;
    fAppIdRegFlags : TJwComAppIdRegFlags;
    fSRPTrustLevel : TJwSaferLevelId;
    fLoadUserProfile : Boolean;
    fReadOnly : Boolean;

    Reg : TRegistry;

    procedure CheckReadonly(const PropertyName : String);

    //Reads a self relative SD from registry
    class function ReadSD(Reg : TRegistry; const KeyName : string; var Default :TJwSecurityDescriptor) : TJwSecurityDescriptor;
    //Writes a self relative SD to registry
    class procedure WriteSD(Reg : TRegistry; const KeyName : string; const SD : TJwSecurityDescriptor; DoDelete : Boolean);



  public
    {Create opens the AppID of a specific COM application.

    Parameter
      AppID GUID of the app.
      ReadOnly Defines whether all properties are read only (true) or not (false).
        If ReadOnly is true EJwsclReadOnlyPropertyException will be raised on every property write call.

    Remarks
      The registry calls are made on the 64bit registry if the process is running in a Win64 environment.
    }
    constructor Create(const AppID : TGuid; ReadOnly : Boolean);
    destructor Destroy; override;

    {Frees all internal variables and sets them to nil

     Remarks
       Usually a second read call to any of the properties returns the cached information
       from the first call.
       By calling FreeCache the chached information is rendered invalid so the properties
       rereads from e.g. registry. This also means that a second call can raise an exception
       if the situation has changed.
    }
    procedure FreeCache;

    {Returns a security descriptor of the DefaultLaunchPermission machine wide registry key.
    }
    class function GetDefaultLaunchPermission : TJwSecurityDescriptor; virtual;

    {Returns a security descriptor of the DefaultAccessPermission machine wide registry key.
    }
    class function GetDefaultAccessPermission : TJwSecurityDescriptor; virtual;

    {Returns whether Distrubuted COM is enabled (true) or not (false).
    }
    class function IsDCOMEnabled : Boolean; virtual;

    {Returns the legacy authentication level.}
    class function GetLegacyAuthenticationLevel : TJwComAuthenticationLevel; virtual;

    {Returns the legacy impersonation level.}
    class function GetLegacyImpersonationLevel : TJwComImpersonationLevel; virtual;


    //properties

    {Sets or gets launch permission. If this key is not available the default value (GetDefaultLaunchPermission) is returned.
    }
    property LaunchPermission : TJwSecurityDescriptor read GetLaunchPermission write SetLaunchPermission;

    {Sets or gets access permission. If this key is not available the default value (GetDefaultAccessPermission) is returned.
    }
    property AccessPermission : TJwSecurityDescriptor read GetAccessPermission write SetAccessPermission;

    {Sets or gets the RunAs registry value as a security ID class. If this value does not exist the return value is nil.
     The key value is deleted if the property is set to nil.
    }
    property RunAs : TJwSecurityID read GetRunAs write SetRunAs;

    {Sets or gets the RunAs registry value as a string. If this value does not exist the return value is empty.
    The key value is deleted if the property is set to empty.
    }
    property RunAsString : String read GetRunAsString write SetRunAsString;

    {Sets or gets the service parameters that are delivered to the service on startup.
    The key value is deleted if the property is set to empty.
    }
    property ServiceParameters : String read GetServiceParameters write SetServiceParameters;

    {Sets or gets the Authentication level.  If this key is not available the default value (GetLegacyAuthenticationLevel) is returned.
     The key value is deleted if the property is set to empty.
    }
    property AuthenticationLevel : TJwComAuthenticationLevel read GetAuthenticationLevel write SetAuthenticationLevel;

    {Sets or gets the AppID flags.  If this key is not available an empty set [] is returned.
     The key value is deleted if the property is set to empty.
    }
    property AppIdRegFlags : TJwComAppIdRegFlags read GetAppIdRegFlags write SetAppIdRegFlags;

    {Sets or gets the RSP trust  level.  If this key is not available the value sliDisallowed is returned.
     The key value is deleted if the property is set to sliInvalid.
    }
    property SRPTrustLevel : TJwSaferLevelId read GetSRPTrustLevel write SetSRPTrustLevel;

    {Sets or gets the value of LoadUserProfile. If this key is not available the value false is returned.
    }
    property LoadUserProfile : Boolean read GetLoadUserProfile write SetLoadUserProfile;

    //Returns the GUID supplied to Create
    property AppID : TGuid read fAppID;
  end;


var
  JwKnownComHostProcesses : array[1..1] of String =
    ('dllhost.exe'
     //'ComServerTest2.exe'
     );


  {JwIgnoreHostProcessesInServer is evaluated by server versions of TJwComProcessSecurity.Initialize
   to determine whether the process should be checked for possible problems.

   Remarks
     CoInitializeSecurity should not be called on COM host processes like dllhost.exe. Usually they
     host more than one COM server and a call to CoInitializeSecurity will create problems.
  }
  JwIgnoreHostProcessesInServer : boolean = false;


implementation

const AUTO_AUTHENTICATION_SERVICE = -1;


{

}

{ TJwComClientSecurity }

procedure TJwComClientSecurity.AuthWinNT(const Domain, User, Password: string);
{begin
     const UserName, DomainName, Psword: String; //-- User Account
     AuthenicationService: DWORD = RPC_C_AUTHN_WINNT;
     AuthorizationService: DWORD = RPC_C_AUTHZ_DEFAULT;
     AuthenicationLevel: DWORD = RPC_C_AUTHN_LEVEL_CALL;
     ImpersonationLevel: DWORD = RPC_C_IMP_LEVEL_IMPERSONATE;
     CapabiltiesFlag: DWORD = EOAC_NONE
                                    );
     }
var
  {AuthIdent: TSecWinNTAuthIdentityExW;   }
  AuthIdent: TSecWinNTAuthIdentityW;
  hRes : HRESULT;
begin
  ZeroMemory(@AuthIdent, sizeof(AuthIdent));

 { AuthIdent.Version := SEC_WINNT_AUTH_IDENTITY_VERSION;
  AuthIdent.Length := SizeOf(AuthIdent);               }

  AuthIdent.User := PWideChar(WideString(User));
  AuthIdent.UserLength := Length(User);
  AuthIdent.Domain := PWideChar(WideString(Domain));
  AuthIdent.DomainLength := Length(Domain);
  AuthIdent.Password := PWideChar(WideString(Password));
  AuthIdent.PasswordLength := Length(Password);
  AuthIdent.Flags := SEC_WINNT_AUTH_IDENTITY_UNICODE;

      (*
  CoSetProxyBlanket(
        Proxy,//{__in}      pProxy : IUnknown;
        asWinNT,//{__in}      dwAuthnSvc,
        azsDefault,//AuthorizationService,//{__in}      dwAuthzSvc : DWORD;
        '',//ServerPrincipalName,//{__in_opt}  pServerPrincName : String;
        calCall,//AuthenticationLevel,//{__in}      AuthnLevel : TJwComAuthenticationLevel;
        cilImpersonate, //ImpersionationLevel,//{__in}      dwImpLevel : TJwComImpersonationLevel;
        @AuthIdent,//{__in_opt}  pAuthInfo : RPC_AUTH_IDENTITY_HANDLE;
        []//Capabilites//{__in}      dwCapabilities : TJwComAuthenticationCapabilities
        ); *)
  hRes := JwaCOMSecurity.CoSetProxyBlanket(
    Proxy,//{__in} pProxy : IUnknown;
    RPC_C_AUTHN_DEFAULT,//{__in} dwAuthnSvc : DWORD;
    RPC_C_AUTHZ_NONE,//{__in} dwAuthzSvc  : DWORD;
    nil,//{__in_opt} pServerPrincName : POleStr;
    RPC_C_AUTHN_LEVEL_DEFAULT,//{__in} dwAuthnLevel  : DWORD;
    RPC_C_IMP_LEVEL_IMPERSONATE,//{__in} dwImpLevel : DWORD;
    @AuthIdent,//{__in_opt} pAuthInfo : RPC_AUTH_IDENTITY_HANDLE;
    DWORD(EOAC_NONE)//{__in} dwCapabilities : DWORD) : HRESULT; stdcall;
  );
end;

procedure TJwComClientSecurity.BeginUpdate;
begin
  inherited;
end;

constructor TJwComClientSecurity.Create(const ProxyInterface: IInterface; MakeCopy: Boolean);
begin
  fProxy := ProxyInterface;
  fReadOnly := false;
  fWinNTIdentity := nil;
  fAuthenticationInfo := nil;
  fServerPrincipalName := '';
  fUpdating := false;

  fAuthenticationLevel := calConnect;
  fAuthorizationService := azsDefault;
  fImpersonationLevel := cilImpersonate;
  fAuthenticationService := asDefault;

  if MakeCopy then
  begin
    CoCopyProxy(ProxyInterface, fProxy);

    BeginUpdate;
    EndUpdate;
  end
  else
  begin
    UpdateProxyInfo;
  end;
end;

constructor TJwComClientSecurity.Create(const Proxy: TJwComClientSecurity);
begin
  //TODO: !!
 { fProxy := Proxy;
  fReadOnly := false;
  fWinNTIdentity := nil;
  fAuthenticationInfo := nil;
  fServerPrincipalName := '';
  fUpdating := false;
  fAuthenticationLevel := calNone;
  fAuthorizationService := 0;
  fImpersonationLevel := cilIdentify;    }
end;

class procedure TJwComClientSecurity.CoCopyProxy(pProxy: IInterface; out ppCopy: IInterface);
var hr : HRESULT;
begin
  hr := {JwaWindows.}JwaCOMSecurity.CoCopyProxy(pProxy, ppCopy);
  if Failed(hr) then
    raise EJwsclWinCallFailedException.CreateFmtEx('', 'CoCopyProxy', ClassName, 'JwsclComSecurity.pas', 0, ResultCode(hr), []);
end;

class procedure TJwComClientSecurity.CoQueryProxyBlanket({__in}       pProxy : IUnknown;
        {__out_opt}  out pwAuthnSvc : TJwComAuthenticationService;
        {__out_opt}  out pAuthzSvc : TJwComAuthorizationService;
        {__out_opt}  out pServerPrincName : String;
        {__out_opt}  out pAuthnLevel : TJwComAuthenticationLevel;
        {__out_opt}  out pImpLevel : TJwComImpersonationLevel;
        {__out_opt}  out pAuthInfo : RPC_AUTH_IDENTITY_HANDLE;
        {__out_opt}  out pCapabilites : TJwComAuthenticationCapabilities);
var
  S : POleStr;
  hr : HRESULT;
  iCapabilites : DWORD;
begin
  iCapabilites := TJwEnumMap.ConvertComAuthenticationCapabilities(pCapabilites);

  hr := {JwaWindows.}JwaCOMSecurity.CoQueryProxyBlanket(
    pProxy,//{__in} pProxy : IUnknown;
    @pwAuthnSvc,//__out_opt  DWORD *pwAuthnSvc,
    @pAuthzSvc,//__out_opt  DWORD *pAuthzSvc,
    @S,//__out_opt  OLECHAR **pServerPrincName,
    @pAuthnLevel,//__out_opt  DWORD *pAuthnLevel,
    @pImpLevel,//__out_opt  DWORD *pImpLevel,
    @pAuthInfo,//__out_opt  RPC_AUTH_IDENTITY_HANDLE *pAuthInfo,
    @iCapabilites//__out_opt  DWORD *pCapabilites
  );

  if Failed(hr) then
  begin
    raise EJwsclWinCallFailedException.CreateFmtEx('', 'CoQueryProxyBlanket', ClassName, 'JwsclComSecurity.pas', 0, ResultCode(hr), []);
  end;

  pServerPrincName := String(WideString(S));
  CoTaskMemFree(S);

  pCapabilites := TJwEnumMap.ConvertComAuthenticationCapabilities(iCapabilites);
end;

class procedure TJwComClientSecurity.CoSetProxyBlanket(
        {__in}      pProxy : IUnknown;
        {__in}      dwAuthnSvc : TJwComAuthenticationService;
        {__in}      dwAuthzSvc : TJwComAuthorizationService;
        {__in_opt}  pServerPrincName : String;
        {__in}      AuthnLevel : TJwComAuthenticationLevel;
        {__in}      dwImpLevel : TJwComImpersonationLevel;
        {__in_opt}  pAuthInfo : Pointer;
        {__in}      dwCapabilities : TJwComAuthenticationCapabilities
      );
var hr : HRESULT;
  pServerName : PWideChar;
begin
  if pServerPrincName <> '' then
    pServerName := PWideChar(WideString(pServerPrincName))
  else
    pServerName := nil;

  hr := {JwaWindows.}JwaCOMSecurity.CoSetProxyBlanket(pProxy, DWORD(dwAuthnSvc), DWORD(dwAuthzSvc), pServerName,
    DWORD(AuthnLevel), DWORD(dwImpLevel), pAuthInfo,
        TJwEnumMap.ConvertComAuthenticationCapabilities(dwCapabilities));

  if Failed(hr) then
  begin
    raise EJwsclWinCallFailedException.CreateFmtEx(RsWinCallFailed, 'CoSetProxyBlanket', ClassName, 'JwsclComSecurity.pas', 0, ResultCode(hr), ['CoSetProxyBlanket']);
  end;
end;



destructor TJwComClientSecurity.Destroy;
begin
  FreeAndNil(fWinNTIdentity);

  inherited;
end;

procedure TJwComClientSecurity.EndUpdate;
begin
  inherited;

  if not fUpdating then
  begin
    CoSetProxyBlanket(
      fProxy,//{__in} pProxy : IUnknown;
      fAuthenticationService,//{__in} dwAuthnSvc : DWORD;
      fAuthorizationService,//{__in} dwAuthzSvc  : DWORD;
      fServerPrincipalName,//{__in_opt} pServerPrincName : POleStr;
      fAuthenticationLevel,//{__in} dwAuthnLevel  : DWORD;
      fImpersonationLevel,//{__in} dwImpLevel : DWORD;
      fAuthenticationInfo,//{__in_opt} pAuthInfo : RPC_AUTH_IDENTITY_HANDLE;
      fCapabilites//{__in} dwCapabilities : DWORD) : HRESULT; stdcall;
    );
      //AuthWinNT('', 'TestBenutzer', '');
  end;
end;



procedure TJwComClientSecurity.SetWinNTIdentity(const Value: TJwComWinNTIdentity);
begin
  CheckReadonly('WinNTIdentity');

  if (fAuthenticationService <> asWinNT) and
     (fAuthenticationService <> asGSSKerberos) then
  begin
    raise EJwsclInvalidParameterException.CreateFmtEx
      ('Only AuthenticationService = asWinNT and asGSSKerberos are supported.', 'SetWinNTIdentity', ClassName, 'JwsclComSecurity.pas', 0, false, []);
  end;

  if ((fAuthenticationService = asWinNT) or
     (fAuthenticationService = asGSSKerberos)) and
    not JwIsPrivilegeSet(SE_IMPERSONATE_NAME, pqt_Enabled) then
    raise EJwsclPrivilegeNotFoundException.CreateFmtEx
      ('Impersonating another user for COM needs the SE_IMPERSONATE_NAME privileges. '+
      'Otherwise all subsequent COM calls will fail (access denied).', 'SetWinNTIdentity', ClassName, 'JwsclComSecurity.pas', 0, false, []);


  BeginUpdate;
  try
    fWinNTIdentity := Value;

    case fAuthenticationService of
      asWinNT       : fAuthenticationInfo := fWinNTIdentity.AuthorizationInfo;
      asGSSKerberos : fAuthenticationInfo := fWinNTIdentity.AuthorizationInfoEx;
    end;


    if fAuthenticationLevel = calNone then
    begin
      fAuthenticationLevel := calConnect;
    end;
  finally
    EndUpdate;
  end;
  fReadOnly := True;
end;

procedure TJwComClientSecurity.UpdateProxyInfo;
begin
   CoQueryProxyBlanket(
    fProxy,//{__in} pProxy : IUnknown;
    fAuthenticationService,//{__in} dwAuthnSvc : DWORD;
    fAuthorizationService,//{__in} dwAuthzSvc  : DWORD;
    fServerPrincipalName,//{__in_opt} pServerPrincName : POleStr;
    fAuthenticationLevel,//{__in} dwAuthnLevel  : DWORD;
    fImpersonationLevel,//{__in} dwImpLevel : DWORD;
    fAuthenticationInfo,//{__in_opt} pAuthInfo : RPC_AUTH_IDENTITY_HANDLE;
    fCapabilites//{__in} dwCapabilities : DWORD) : HRESULT; stdcall;
  );
end;

{ TJwComServerSecurity }

procedure TJwComServerSecurity.AccessCheck(Flags: Cardinal; const DesiredAccess: DWORD; const SecurityDescriptor: TJwSecurityDescriptor;
  const GenericMapping: TJwSecurityGenericMappingClass; out AccessGranted: Boolean; out GrantedAccessMask: DWORD);
var
  CacheResult: TAuthZAccessCheckResultHandle;
begin
  CacheResult := INVALID_HANDLE_VALUE;
  Self.AccessCheckCached(CacheResult, Flags, DesiredAccess, SecurityDescriptor, GenericMapping, AccessGranted, GrantedAccessMask);
end;

procedure TJwComServerSecurity.AccessCheckCached(var CacheResult: TAuthZAccessCheckResultHandle; Flags: Cardinal; const DesiredAccess: DWORD;
  const SecurityDescriptor: TJwSecurityDescriptor; const GenericMapping: TJwSecurityGenericMappingClass; out AccessGranted: Boolean;
  out GrantedAccessMask: DWORD);

var
  Request : TJwAuthZAccessRequest;
  Reply : TJwAuthZAccessReply;
begin
  if ImpersionationLevel < cilIdentify then
  begin
    raise EJwsclNoThreadTokenAvailable.CreateFmtEx('The client does not have a token if ImpersionationLevel is cilAnonymous.',
      'GetToken', ClassName, 'JwsclComSecurity.pas', 0, false, []);
    exit;
  end;

  if not Assigned(fAuthManager) then
  begin
    fAuthManager := TJwAuthResourceManager.Create('', [], nil, nil);
  end;

  if not Assigned(fAuthContext) then
  begin
    fAuthContext := TJwAuthContext.CreateByToken(fAuthManager, [authZSF_ComputePrivileges], Token, 0, nil);
  end;

  Request := TJwAuthZAccessRequest.Create(DesiredAccess,
              JwNullSID, nil, nil, shOwned);

  Reply := nil;
  try
    if (CacheResult = 0) or (CacheResult = INVALID_HANDLE_VALUE) then
    begin
      fAuthContext.AccessCheck(Flags, Request, 0, SecurityDescriptor, nil, GenericMapping, Reply, CacheResult);
    end
    else
    begin
      fAuthContext.AccessCheckCached(CacheResult, Flags, Request, 0, Reply);
    end;

    AccessGranted := Reply.ErrorByType[0] = reSuccess;
    GrantedAccessMask := Reply.GrantedAccessMask[0];

  finally
    FreeAndNil(Reply);
    FreeAndNil(Request);
  end;
end;

class procedure TJwComServerSecurity.CoQueryClientBlanket(
        {__out_opt}  out pwAuthnSvc : TJwComAuthenticationService;
        {__out_opt}  out pAuthzSvc : TJwComAuthorizationService;
        {__out_opt}  out pServerPrincName : String;
        {__out_opt}  out pAuthnLevel : TJwComAuthenticationLevel;
        {__out_opt}  out pImpLevel : TJwComImpersonationLevel;
                     out pPrivs : RPC_AUTHZ_HANDLE;
        {__out_opt}  var pCapabilites : TJwComAuthenticationCapabilities);
var
  S : POleStr;
  hr : HRESULT;
  iCapabilites : DWORD;
begin
  iCapabilites := TJwEnumMap.ConvertComAuthenticationCapabilities(pCapabilites);

  hr := {JwaWindows.}JwaCOMSecurity.CoQueryClientBlanket(
      @pwAuthnSvc,//__out_opt    DWORD *pAuthnSvc,
      @pAuthzSvc,//__out_opt    DWORD *pAuthzSvc,
      @S,//__out_opt    OLECHAR **pServerPrincName,
      @pAuthnLevel,//__out_opt    DWORD *pAuthnLevel,
      @pImpLevel,//__out_opt    DWORD *pImpLevel,
      @pPrivs,//__out_opt    RPC_AUTHZ_HANDLE *pPrivs,
      @iCapabilites//__inout_opt  DWORD *pCapabilities
    );

  if Failed(hr) then
  begin
    raise EJwsclWinCallFailedException.CreateFmtEx('', 'CoQueryClientBlanket', ClassName, 'JwsclComSecurity.pas', 0, ResultCode(hr), []);
  end;

  pCapabilites := TJwEnumMap.ConvertComAuthenticationCapabilities(iCapabilites);

  pServerPrincName := String(WideString(S));
  CoTaskMemFree(S);
end;

constructor TJwComServerSecurity.Create(const ImpersonationType: TServerImpersonationType);
//var
  //p : IServerSecurity;
  //hr : HRESULT;
  //pServerPrincipalName : PWidechar;
  //pCapabilites : DWORD;
begin
  if TJwSecurityToken.HasThreadAToken then
  begin
    raise EJwsclInvalidTokenHandle.CreateFmtEx('The call cannot be made with an impersonated token. Revert to self first.', 'Create', ClassName, 'JwsclComSecurity.pas', 0, false, []);
  end;

  (*
  //this is one way to obtain the client,
  //it is just the way CoQueryClientBlanket does it

  p := nil;
  hr := CoGetCallContext(StringToGUID('{0000013E-0000-0000-C000-000000000046}'), @p);

  if Failed(hr) then
  begin
    raise EJwsclWinCallFailedException.CreateFmtEx('', 'CoGetCallContext', ClassName, 'JwsclComSecurity.pas', 0, ResultCode(hr), []);
  end;


  hr := p.QueryBlanket(
        @DWORD(fAuthenticationService),//{__out_opt}  out pwAuthnSvc : DWORD;
        @DWORD(fAuthorizationService),//{__out_opt}  out pAuthzSvc : DWORD;
        @pServerPrincipalName,//{__out_opt}  out pServerPrincName : String;
        @DWORD(fAuthenticationLevel),//{__out_opt}  out pAuthnLevel : TJwComAuthenticationLevel;
        @DWORD(fImpersonationLevel),//{__out_opt}  out pImpLevel : TJwComImpersonationLevel;
        fAuthenticationInfo,//             out pPrivs : RPC_AUTHZ_HANDLE;
        @pCapabilites//{__out_opt}  var pCapabilites : TJwComAuthenticationCapabilities);
  );

  if Failed(hr) then
  begin
    raise EJwsclWinCallFailedException.CreateFmtEx('', 'QueryBlanket', ClassName, 'JwsclComSecurity.pas', 0, ResultCode(hr), []);
  end;

  //SiMain.LogPWideChar('User', PSecWinNTAuthIdentityW(@fAuthenticationInfo)^.User);

  fServerPrincipalName := String(pServerPrincipalName);
  CoTaskMemFree(pServerPrincipalName);
  *)

  CoQueryClientBlanket(
        fAuthenticationService,//{__out_opt}  out pwAuthnSvc : DWORD;
        fAuthorizationService,//{__out_opt}  out pAuthzSvc : DWORD;
        fServerPrincipalName,//{__out_opt}  out pServerPrincName : String;
        fAuthenticationLevel,//{__out_opt}  out pAuthnLevel : TJwComAuthenticationLevel;
        fImpersonationLevel,//{__out_opt}  out pImpLevel : TJwComImpersonationLevel;
        fAuthenticationInfo,//             out pPrivs : RPC_AUTHZ_HANDLE;
        fCapabilites//{__out_opt}  var pCapabilites : TJwComAuthenticationCapabilities);
  );


  SetReadOnlyProperties(True);
  fToken := nil;
  fWinNTIdentity := nil;

  {SiMain.LogString('fServerPrincipalName', fServerPrincipalName);

  SiMain.LogCardinal('fAuthenticationService',DWORD(fAuthenticationService));
  SiMain.LogCardinal('fAuthorizationService',DWORD(fAuthorizationService));
  SiMain.LogCardinal('fAuthenticationLevel',DWORD(fAuthenticationLevel));
  SiMain.LogCardinal('fImpersonationLevel',DWORD(fImpersonationLevel));

  SiMain.LogMemory('WinNT', fAuthenticationInfo, 500);

  SiMain.LogCardinal('UserLen', PSecWinNTAuthIdentityW(@fAuthenticationInfo)^.UserLength);
  SiMain.LogPWideChar('User', PSecWinNTAuthIdentityW(@fAuthenticationInfo)^.User);

  SiMain.LogPWideChar('Domain', PSecWinNTAuthIdentityW(@fAuthenticationInfo)^.Domain);
  SiMain.LogPWideChar('User', PSecWinNTAuthIdentityW(@fAuthenticationInfo)^.Password);
 }

  if fAuthenticationService = asWinNT then
  begin
    fWinNTIdentity := TJwComWinNTIdentity.Create(PSecWinNTAuthIdentityW(@fAuthenticationInfo)^, True);
   { SiMain.LogString('User', fWinNTIdentity.User);
    SiMain.LogString('Domain', fWinNTIdentity.Domain);
    SiMain.LogString('Password', fWinNTIdentity.Password); }
  end;
end;

destructor TJwComServerSecurity.Destroy;
begin
  FreeAndNil(fToken);
  FreeAndNil(fWinNTIdentity);

  FreeAndNil(fAuthManager);
  FreeAndNil(fAuthContext);

  //RevertToSelf;

  inherited;
end;


function TJwComServerSecurity.GetToken: TJwSecurityToken;
begin
  if ImpersionationLevel < cilIdentify then
  begin
    raise EJwsclNoThreadTokenAvailable.CreateFmtEx('The client does not have a token if ImpersionationLevel is cilAnonymous.',
      'GetToken', ClassName, 'JwsclComSecurity.pas', 0, false, []);
    exit;
  end;

  if not Assigned(fToken) then
  begin
    Self.ImpersonateClient;
    try
      fToken := TJwSecurityToken.CreateTokenByThread(0, TOKEN_ALL_ACCESS, true);
    finally
      Self.RevertToSelf;
    end;
  end;
  result := fToken;
end;

function TJwComServerSecurity.GetUserName: String;
var
  Token : TJwSecurityToken;
  IsImpersonating : Boolean;
begin
  if ImpersionationLevel < cilIdentify then
  begin
    result := '';
    exit;
  end;

  IsImpersonating := TJwSecurityToken.HasThreadAToken;

  if not IsImpersonating then
    ImpersonateClient;

  Token := TJwSecurityToken.CreateTokenByThread(0, TOKEN_READ or TOKEN_QUERY, true);
  try
    if not IsImpersonating then
      RevertToSelf;

    result := Token.GetTokenUserName;
  finally
    Token.Free;
  end;
end;

class procedure TJwComServerSecurity.ImpersonateClient;
var
  hr : HRESULT;
begin
  hr := CoImpersonateClient();

  if Failed(hr) then
  begin
    raise EJwsclWinCallFailedException.CreateFmtEx('', 'ImpersonateClient', ClassName, 'JwsclComSecurity.pas', 0, ResultCode(hr), []);
  end;
end;

class function TJwComServerSecurity.IsImpersonating: Boolean;
{var
   Token : TJwSecurityToken;     }
begin
  result := TJwSecurityToken.HasThreadAToken;
 { if result then
  begin
    Token := TJwSecurityToken.CreateTokenByThread(0, TOKEN_ALL_ACCESS, true);
    try
      result := Token.IsEqual(GetToken);
    finally
      FreeAndNil(Token);
    end;
  end;  }
end;

class procedure TJwComServerSecurity.RevertToSelf;
var h : HRESULT;
begin
  h := CoRevertToSelf();

  if Failed(h) then
   raise EJwsclComException.CreateFmtEx('RevertToSelf failed: %d',
    'RevertToSelf', ClassName, 'JwsclComSecurity.pas',0, DWORD(H), []);
end;



procedure TJwComServerSecurity.SetAuthContext(const Value: TJwAuthContext);
begin
  FreeAndNil(fAuthContext);

  fAuthContext := Value;
end;

procedure TJwComServerSecurity.SetAuthManager(const Value: TJwAuthResourceManager);
begin
  FreeAndNil(fAuthManager);

  fAuthManager := Value;
end;

{ TJwComRegistrySecurity }

const
  KEY_ACCESS_PERMISSION = 'AccessPermission';
  KEY_LAUNCH_PERMISSION = 'LaunchPermission';

procedure TJwComRegistrySecurity.CheckReadonly(const PropertyName : String);
begin
  if fReadOnly then
  begin
    raise EJwsclReadOnlyPropertyException.CreateFmtEx('Property %s is readonly.', 'Getter', ClassName, 'JwsclComSecurity.pas', 0, false, [PropertyName]);
  end;
end;

constructor TJwComRegistrySecurity.Create(const AppID: TGuid; ReadOnly: Boolean);
begin
  Reg := TRegistry.Create();
  Reg.RootKey := HKEY_CLASSES_ROOT;

  if TJwWindowsVersion.IsWindows64 then
    Reg.Access := Reg.Access or KEY_WOW64_64KEY;

  if not Reg.OpenKey('AppID\'+GUIDToString(AppID), false) then
  begin
    Reg.Free;
    raise Exception.Create('Appid not found');
  end;

  fAppID := AppID;

  FreeCache;
end;

destructor TJwComRegistrySecurity.Destroy;
begin
  FreeCache;
  FreeAndNil(Reg);
  inherited;
end;

function TJwComRegistrySecurity.GetAccessPermission: TJwSecurityDescriptor;
begin
  if Reg.ValueExists(KEY_ACCESS_PERMISSION) then
  begin
    result := ReadSD(Reg, KEY_ACCESS_PERMISSION, fAccessPermission);
  end
  else
  begin
    result := GetDefaultAccessPermission;
    FreeAndNil(fAccessPermission);
    fAccessPermission := result;
  end;
end;

function TJwComRegistrySecurity.GetAppIdRegFlags: TJwComAppIdRegFlags;
var Value : Integer;
begin
  if Reg.ValueExists('AppIDFlags') then
  begin
    Value := Reg.ReadInteger('AppIDFlags');
    result := TJwEnumMap.ConvertComAppIdRegFlags(Value);
  end
  else
  begin
    result := [];
  end;
end;


function TJwComRegistrySecurity.GetAuthenticationLevel: TJwComAuthenticationLevel;
var Value : Integer;
begin
  if Reg.ValueExists('AuthenticationLevel') then
  begin
    Value := Reg.ReadInteger('AuthenticationLevel');
    Result := TJwComAuthenticationLevel(Value);
  end
  else
  begin
    result := GetLegacyAuthenticationLevel;
  end;
end;

class function TJwComRegistrySecurity.GetDefaultAccessPermission: TJwSecurityDescriptor;
var Reg : TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if not Reg.OpenKey('Software\Microsoft\OLE', false) then
    begin
      raise Exception.Create('Default Key not found');
    end;

    if not Reg.ValueExists('DefaultAccessPermission') then
    begin
      //TODO: create SD with SYSTEM, Admin, SELF
    end;


    result := nil;
    result := ReadSD(Reg, 'DefaultAccessPermission', result);
  finally
    Reg.Free;
  end;
end;

class function TJwComRegistrySecurity.GetDefaultLaunchPermission: TJwSecurityDescriptor;
var Reg : TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if not Reg.OpenKey('Software\Microsoft\OLE', false) then
    begin
      //TODO: create SD with SYSTEM, Admin, Interactive
      raise Exception.Create('Default Key not found');
    end;

    if not Reg.ValueExists('DefaultLaunchPermission') then
    begin
      //TODO: create SD with SYSTEM, Admin, SELF
    end;

    result := nil;
    result := ReadSD(Reg, 'DefaultLaunchPermission', result);
  finally
    Reg.Free;
  end;
end;

function TJwComRegistrySecurity.GetLaunchPermission: TJwSecurityDescriptor;
begin
  if Reg.ValueExists(KEY_LAUNCH_PERMISSION) then
  begin
    result := ReadSD(Reg, KEY_LAUNCH_PERMISSION, fLaunchPermission);
  end
  else
  begin
    result := GetDefaultLaunchPermission;
    FreeAndNil(fLaunchPermission);
    fLaunchPermission := result;
  end;
end;

function TJwComRegistrySecurity.GetLoadUserProfile: Boolean;
begin
  if Reg.ValueExists('LoadUserSettings') then
  begin
    result := Reg.ReadBool('LoadUserSettings');
  end
  else
  begin
    result := false;
  end;
end;

function TJwComRegistrySecurity.GetRunAs: TJwSecurityID;
var Value : string;
begin
  if Reg.ValueExists('RunAs') then
  begin
    Value := Reg.ReadString('RunAs');
    result := TJwSecurityId.Create('', Value);
  end
  else
  begin
    result := nil;
  end;
end;

function TJwComRegistrySecurity.GetRunAsString: String;
begin
  if Reg.ValueExists('RunAs') then
  begin
    result := Reg.ReadString('RunAs');
  end
  else
  begin
    result := '';
  end;
end;

function TJwComRegistrySecurity.GetServiceParameters: String;
begin
  if Reg.ValueExists('ServiceParameters') then
  begin
    result := Reg.ReadString('ServiceParameters');
  end
  else
  begin
    result := '';
  end;
end;

function TJwComRegistrySecurity.GetSRPTrustLevel: TJwSaferLevelId;
begin
  if Reg.ValueExists('SRPTrustLevel') then
  begin
    result := TJwSaferLevelId(Reg.ReadInteger('SRPTrustLevel'));
  end
  else
  begin
    result := sliDisallowed;
  end;
end;

class function TJwComRegistrySecurity.IsDCOMEnabled: Boolean;
var
  Reg : TRegistry;
  Value : String;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;

  try
    if Reg.OpenKey('\SOFTWARE\Microsoft\Ole', false) then
    begin
      Value := Reg.ReadString('EnableDCOM');
      result := (Value = 'Y') or (Value = 'y');
    end
    else
      result := false;
  finally
    Reg.Free;
  end;
end;

class function TJwComRegistrySecurity.GetLegacyAuthenticationLevel: TJwComAuthenticationLevel;
  //default: RPC_C_AUTHN_CONNECT
var
  Reg : TRegistry;
  Value : DWORD;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;

  try
    if Reg.OpenKey('\SOFTWARE\Microsoft\Ole', false) then
    begin
      Value := Reg.ReadInteger('LegacyAuthenticationLevel');
      result := TJwComAuthenticationLevel(Value);
    end
    else
      result := calConnect;
  finally
    Reg.Free;
  end;
end;

class function TJwComRegistrySecurity.GetLegacyImpersonationLevel: TJwComImpersonationLevel;
  //default: RPC_C_IMP_LEVEL_IDENTIFY
var
  Reg : TRegistry;
  Value : DWORD;
begin
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;

  try
    if Reg.OpenKey('\SOFTWARE\Microsoft\Ole', false) then
    begin
      Value := Reg.ReadInteger('LegacyImpersonationLevel');
      result := TJwComImpersonationLevel(Value);
    end
    else
      result := cilIdentify;
  finally
    Reg.Free;
  end;
end;

class function TJwComRegistrySecurity.ReadSD(Reg : TRegistry; const KeyName: string; var Default : TJwSecurityDescriptor): TJwSecurityDescriptor;
var
  Size : Integer;
  Buf  : Pointer;
begin
  if not Assigned(Default) then
  begin
    Size := Reg.GetDataSize(KeyName);

    if (Size < 0) or (not Reg.ValueExists(KeyName)) then
    begin
      raise ERegistryException.CreateFmt('Key %s\%s not found or value has zero size',[Reg.CurrentPath, KeyName]);
    end;

    GetMem(Buf, Size);
    try
      Reg.ReadBinaryData(KeyName, Buf^, Size);
      Default := TJwSecurityDescriptor.Create(JwaWindows.PSecurityDescriptor(Buf));
    finally
      FreeMem(Buf);
    end;
  end;

  result := Default;
end;

class procedure TJwComRegistrySecurity.WriteSD(Reg: TRegistry; const KeyName: string; const SD: TJwSecurityDescriptor; DoDelete: Boolean);
var
  Size : Cardinal;
  PSD : PSecurityDescriptor;
begin
  if not Assigned(SD) then
  begin
    if Reg.ValueExists(KeyName) then
      Reg.DeleteValue(KeyName);
  end
  else
  begin
    PSD := SD.Create_SD(Size, true);
    try
      Reg.WriteBinaryData(KeyName, PSD^, Size);
    finally
      SD.Free_SD(PSD);
    end;
  end;
end;

//fAccessPermission            KEY_ACCESS_PERMISSION
procedure TJwComRegistrySecurity.SetAccessPermission(const Value: TJwSecurityDescriptor);
begin
  CheckReadonly('AccessPermission');

  WriteSD(Reg, KEY_ACCESS_PERMISSION, Value, not Assigned(Value));
end;

procedure TJwComRegistrySecurity.SetAppIdRegFlags(const Value: TJwComAppIdRegFlags);
begin
  CheckReadonly('AppIdRegFlags');

  if Value = [] then
  begin
    Reg.DeleteValue('AppIDFlags');
  end
  else
  begin
    Reg.WriteInteger('AppIDFlags', TJwEnumMap.ConvertComAppIdRegFlags(Value));
  end;
end;

procedure TJwComRegistrySecurity.SetAuthenticationLevel(const Value: TJwComAuthenticationLevel);
begin
  CheckReadonly('AuthenticationLevel');

  if (Value = calInvalid) then
  begin
    Reg.DeleteValue('AuthenticationLevel');
  end
  else
  begin
    Reg.WriteInteger('AuthenticationLevel', Integer(Value));
  end;
end;


procedure TJwComRegistrySecurity.SetLaunchPermission(const Value: TJwSecurityDescriptor);
begin
  CheckReadonly('LaunchPermission');

  WriteSD(Reg, KEY_LAUNCH_PERMISSION, Value, not Assigned(Value));
end;

procedure TJwComRegistrySecurity.SetLoadUserProfile(const Value: Boolean);
begin
  CheckReadonly('LoadUserProfile');

  Reg.WriteBool('LoadUserProfile', Value);
end;

procedure TJwComRegistrySecurity.SetRunAs(const Value: TJwSecurityID);
var Domain, Name : string;
begin
  CheckReadonly('RunAs');

  if not Assigned(Value) then
  begin
    if Reg.ValueExists('RunAs') then
      Reg.DeleteValue('RunAs');
  end
  else
  begin
    Domain := Value.GetAccountDomainName('');
    Name := Value.GetAccountName('');

    Name := Format('%s\%s',[Domain, Name]);
    Reg.WriteString('RunAs', Name);
  end
end;

procedure TJwComRegistrySecurity.SetRunAsString(const Value: String);
begin
  CheckReadonly('RunAsString');

  if Value = '' then
  begin
    if Reg.ValueExists('RunAs') then
      Reg.DeleteValue('RunAs');
  end
  else
  begin
    Reg.WriteString('RunAs', Value);
  end;
end;

procedure TJwComRegistrySecurity.SetServiceParameters(const Value: String);
begin
  CheckReadonly('ServiceParameters');

  if Value = '' then
  begin
    if Reg.ValueExists('ServiceParameters') then
      Reg.DeleteValue('ServiceParameters');
  end
  else
  begin
    Reg.WriteString('ServiceParameters', Value);
  end;
end;

procedure TJwComRegistrySecurity.SetSRPTrustLevel(const Value: TJwSaferLevelId);
begin
  CheckReadonly('SRPTrustLevel');

  if Value = sliInvalid then
  begin
    if Reg.ValueExists('SRPTrustLevel') then
      Reg.DeleteValue('SRPTrustLevel');
  end
  else
  begin
    Reg.WriteInteger('SRPTrustLevel', Integer(Value));
  end;
end;



procedure TJwComRegistrySecurity.FreeCache;
begin
  FreeAndNil(fLaunchPermission);
  FreeAndNil(fAccessPermission);
  FreeAndNil(fRunAs);
  fServiceParameters := '';
  fAuthenticationLevel := calNone;
  fAppIdRegFlags := [];
  fSRPTrustLevel := TJwSaferLevelId(-1);
  fLoadUserProfile := false;

  fReadOnly := false;
end;

{ TJwComCustomSecurity }



procedure TJwComCustomSecurity.BeginUpdate;
begin
  fUpdating := true;
end;

procedure TJwComCustomSecurity.CheckReadonly(const PropertyName: String);
begin
  if fReadOnly then
  begin
    raise EJwsclReadOnlyPropertyException.CreateFmtEx('Property %s is readonly.', 'Getter', ClassName, 'JwsclComSecurity.pas', 0, false, [PropertyName]);
  end;
end;

class function TJwComCustomSecurity.CreateAuthInfo(const InfoType: TJwAuthInfoType): PJwAuthInfo;
var Size : Integer;
begin
  Size := 0;
  case InfoType of
    aitUnknown: ;
    aitWinNTAuthIdentity:   Size := sizeof(TSecWinNTAuthIdentityW);
    aitWinNTAuthIdentityEx: Size := sizeof(TSecWinNTAuthIdentityExW);
    aitCertContext: Size := 0;//sizeof(TCertContext);
  end;

  if Size = 0 then
    raise Exception.Create('Invalid auth info type');

  GetMem(Result, Size);
  ZeroMemory(Result, Size);
end;

procedure TJwComCustomSecurity.EndUpdate;
begin
  fUpdating := false;
end;



class procedure TJwComCustomSecurity.CoInitializeSecurity(
        {__in_opt}  pSecDesc : {PSECURITY_DESCRIPTOR }Pointer;
        {__in}      cAuthSvc : LONG;
        {__in_opt}  asAuthSvc : PSOLE_AUTHENTICATION_SERVICE;
        {__in}      dwAuthnLevel : TJwComAuthenticationLevel;
        {__in}      dwImpLevel : TJwComImpersonationLevel;
        {__in_opt}  pAuthList : Pointer;
        {__in}      dwCapabilities : TJwComAuthenticationCapabilities;
        IgnoreProcess : Boolean
      );
var
  hProc : THandle;
  Len : Cardinal;
  i : Integer;
  S : WideString;
  US : PUnicodeString;
  Status : NTSTATUS;
  hRes : HRESULT;

begin
  Assert(not (
         (dwImpLevel > cilDelegate) or
         (dwImpLevel < cilIdentify) or
         (dwAuthnLevel > calPktPrivacy) or
         (TJwEnumMap.ConvertComAuthenticationCapabilities(dwCapabilities) and $FFFFCC80 <> 0) or

         ((dwAuthnLevel = calNone) and (acStaticCloaking in dwCapabilities)) or
         ((dwAuthnLevel <> calNone) and
           (cAuthSvc = 0))
        ), 'Invalid Parameter');


  //todo if server, check for svchost process to disable this func
  if not IgnoreProcess and (Length(JwKnownComHostProcesses) > 0) and
    Assigned(@NtQueryInformationProcess) then
  begin
    hProc := OpenProcess(GENERIC_READ, False, GetCurrentProcessId);

    US := nil;

    Status := NtQueryInformationProcess(
      hProc,//__in       HANDLE ProcessHandle,
      ProcessImageFileName,//__in       PROCESSINFOCLASS ProcessInformationClass,
      US,//__out      PVOID ProcessInformation,
      0,//__in       ULONG ProcessInformationLength,
      @Len//__out_opt  PULONG ReturnLength
    );

    if (Status = STATUS_INFO_LENGTH_MISMATCH) then
    begin
      US := JwCreateUnicodeStringSelfRelative(Len);

      try
        Status := NtQueryInformationProcess(
          hProc,//__in       HANDLE ProcessHandle,
          ProcessImageFileName,//__in       PROCESSINFOCLASS ProcessInformationClass,
          US,//__out      PVOID ProcessInformation,
          Len,//__in       ULONG ProcessInformationLength,
          @Len//__out_opt  PULONG ReturnLength
        );

        if Status = STATUS_SUCCESS then
        begin
          S := JwUnicodeStringToJwString(US^);

          i := Length(S);
          while (i > 0) and (S[i] <> '\') do
          begin
            Dec(i);
          end;
          S := Copy(S, i + 1, Length(S) - i);


          for I := Low(JwKnownComHostProcesses) to High(JwKnownComHostProcesses) do
          begin
            if CompareText(S, JwKnownComHostProcesses[I]) = 0 then
            begin
              raise EJwsclProcessNotFound.
                CreateFmtEx('InitializeSecurity cannot be called in a shared process with other COM servers.',
                  'InitializeSecurity', ClassName, 'JwsclComSecurity.pas', 0, false, []);
            end;
          end;
        end;
      finally
        FreeMem(US);
      end;
    end;
  end;


  hRes := {JwaWindows.}JwaCOMSecurity.CoInitializeSecurity(
      pSecDesc,//{__in_opt}pSecDesc : PSECURITY_DESCRIPTOR;

      cAuthSvc,//{__in} cAuthSvc : LONG;
      asAuthSvc,//{__in_ecount_opt(cAuthSvc)   } asAuthSvc : PSOLE_AUTHENTICATION_SERVICE;
      nil,//{__in_opt} pReserved1 : Pointer;
      DWORD(dwAuthnLevel),//{__in} dwAuthnLevel : DWORD;
      DWORD(dwImpLevel),//{__in} dwImpLevel  : DWORD;
      pAuthList,//{__in_opt} pAuthList : Pointer;
      TJwEnumMap.ConvertComAuthenticationCapabilities(dwCapabilities),//{__in} dwCapabilities  : DWORD;
      nil//{__in_opt} pReserved3 : Pointer) : HRESULT; stdcall;
    );

  if Failed(hRes) then
  begin
    case hRes of
      RPC_E_TOO_LATE :
        raise EJwsclCoInitializeNotCalledException.
              CreateFmtEx('CoInitializeSecurity has been called already or CoInitialize has not been called at first. (HRESULT= RPC_E_TOO_LATE)',
                'InitializeSecurity', ClassName, 'JwsclComSecurity.pas', 0, DWORD(hRes), []);
    else
      raise EJwsclComException.
              CreateFmtEx('A call to %0:s failed.',
                'InitializeSecurity', ClassName, 'JwsclComSecurity.pas', 0, DWORD(hRes), ['CoInitializeSecurity']);
    end;
  end;
end;


procedure TJwComCustomSecurity.SetAuthenticationInfo(const Value: RPC_AUTH_IDENTITY_HANDLE);
begin
  CheckReadonly('SetAuthenticationInfo');

  fAuthenticationInfo := Value;
end;

procedure TJwComCustomSecurity.SetAuthenticationLevel(const Value: TJwComAuthenticationLevel);
begin
  CheckReadonly('SetAuthenticationLevel');

  fAuthenticationLevel := Value;
end;

procedure TJwComCustomSecurity.SetAuthenticationService(const Value: TJwComAuthenticationService);
begin
  CheckReadonly('SetAuthenticationService');

  fAuthenticationService := Value;
end;

procedure TJwComCustomSecurity.SetAuthorizationService(const Value: TJwComAuthorizationService);
begin
  CheckReadonly('SetAuthorizationService');

  fAuthorizationService := Value;
end;

procedure TJwComCustomSecurity.SetCapabilites(const Value: TJwComAuthenticationCapabilities);
begin
  CheckReadonly('SetCapabilites');

  fCapabilites := Value;
end;

procedure TJwComCustomSecurity.SetImpersonationLevel(const Value: TJwComImpersonationLevel);
begin
  CheckReadonly('SetImpersonationLevel');

  fImpersonationLevel := Value;
end;

procedure TJwComCustomSecurity.SetReadOnlyProperties(const Value: Boolean);
begin
  CheckReadonly('SetReadOnlyProperties');

  fReadOnly := Value;
end;

procedure TJwComCustomSecurity.SetServerPrincipalName(const Value: String);
begin
  CheckReadonly('SetServerPrincipalName');

  fServerPrincipalName := Value;
end;

function TJwComCustomSecurity.GetAuthenticationInfo: RPC_AUTH_IDENTITY_HANDLE;
begin
  result := fAuthenticationInfo;
end;

function TJwComCustomSecurity.GetAuthenticationLevel: TJwComAuthenticationLevel;
begin
  Result := fAuthenticationLevel;
end;

function TJwComCustomSecurity.GetAuthenticationService: TJwComAuthenticationService;
begin
  result := fAuthenticationService;
end;

function TJwComCustomSecurity.GetAuthorizationService: TJwComAuthorizationService;
begin
  result := fAuthorizationService;
end;

function TJwComCustomSecurity.GetImpersionationLevel: TJwComImpersonationLevel;
begin
  result := fImpersonationLevel;
end;

function TJwComCustomSecurity.GetReadOnlyProperties: Boolean;
begin
  result := fReadOnly;
end;

function TJwComCustomSecurity.GetServerPrincipalName: String;
begin
  result := fServerPrincipalName;
end;


{ TJwComProcessSecurity }
class procedure TJwComProcessSecurity.Initialize(
          SecurityData : PJwSecurityInitializationData;
          var AuthenticationList : TJwAuthenticationInfoList;
          AuthenticationLevel: TJwComAuthenticationLevel;
          ImpersonationLevel : TJwComImpersonationLevel;
          Capabilities : TJwComAuthenticationCapabilities;
          const AutoDestroy : Boolean;
          const IgnoreProcess : Boolean = false);


  procedure PrepareAuthInfo(AuthenticationList : TJwAuthenticationInfoList; out List : tagSOLE_AUTHENTICATION_LIST);
  var
    I, Count: Integer;
    AuthInfoP,
    AuthInfoI : PSOLE_AUTHENTICATION_INFO;
  begin
    ZeroMemory(@List, sizeof(List));

    Count := Length(AuthenticationList);
    if Count > 0 then
    begin
      AuthInfoP := AllocMem(sizeof(tagSOLE_AUTHENTICATION_INFO) * Count); //GetMem + ZeroMem

      AuthInfoI := AuthInfoP;
      for I := Low(AuthenticationList) to High(AuthenticationList) do
      begin
        //Check for invalid parameters
        case AuthenticationList[i].AuthenticationService of
          asWinNT, asGSSKerberos :
          begin
            if AuthenticationList[i].AuthenticationService = asDefault then
               raise EJwsclInvalidParameterException.CreateFmtEx(
                  'The value azsDefault is not allowed if used in the AuthenticationList and AuthenticationList[%d].AuthorizationService is asWinNT or asGSSKerberos.'+
                  'Use instead azsNone, azsDCE or azsName.',
                'Create', ClassName, 'JwsclComSecurity.pas', 0, false, [I]);
          end;
        end;
        AuthInfoI.dwAuthnSvc := DWORD(AuthenticationList[i].AuthenticationService);
        AuthInfoI.dwAuthzSvc := DWORD(AuthenticationList[i].AuthorizationService);
        AuthInfoI.pAuthInfo := AuthenticationList[i].AuthenticationInfo;

        Inc(AuthInfoI);
      end;
    end
    else
    begin
      Count := 0;
      AuthInfoP := nil;
    end;

    List.cAuthInfo := Count;
    List.aAuthInfo := AuthInfoP;
  end;

  procedure FreeAuthInfo(var List : tagSOLE_AUTHENTICATION_LIST; var AuthList : TJwAuthenticationInfoList; AutoDestroy : Boolean);
  var I : Integer;
  begin
    if List.aAuthInfo <> nil then
      FreeMem(List.aAuthInfo);

    if AutoDestroy then
    begin
      //free the list
      for I := Low(AuthList) to High(AuthList) do
      begin
        AuthList[I].Free; //makes sure password is destroyed
      end;

      SetLength(AuthList, 0);
    end;
  end;

  procedure PrepareAndCheckSecurityDescriptor(SecurityDescriptor : TJwSecurityDescriptor; out pSD : PSecurityDescriptor);
  var
    Manager : TJwAuthResourceManager;
    Auth : TJwAuthContext;
    Reply : TJwAuthZAccessReply;
    Handle : TAuthZAccessCheckResultHandle;
    Request : TJwAuthZAccessRequest;
  begin
    if Assigned(SecurityDescriptor) then
    begin
      //Check granted access for SYSTEM user. COM needs it.
      Manager := TJwAuthResourceManager.Create('', [], nil, nil);
      try
        Auth := TJwAuthContext.CreateBySid(Manager, [], JwLocalSystemSID, 0, nil);
        try
          Request := TJwAuthZAccessRequest.Create(
              COM_RIGHTS_EXECUTE or
              COM_RIGHTS_EXECUTE_LOCAL or
              COM_RIGHTS_EXECUTE_REMOTE or
              COM_RIGHTS_ACTIVATE_LOCAL or
              COM_RIGHTS_ACTIVATE_REMOTE,
              JwNullSID, nil, nil, shOwned);
          try
            Auth.AccessCheck(0, Request, 0, SecurityDescriptor, nil, nil, Reply, Handle);

            try
              if Reply.ErrorByType[0] <> reSuccess then
              begin
                raise EJwsclAccessDenied.
                  CreateFmtEx('The supplied security descriptor does not allow COM to work properly. You need to add an allow ACE for SYSTEM, Administrators or Authenticated Users.',
                    'Initialize (TJwSecurityDescriptor)',
                    ClassName, 'JwsclComSecurity.pas', 0, false, []);
              end;
            finally
              Reply.Free;
            end;
          finally
            Request.Free;
          end;
        finally
          Auth.Free;
        end;
      finally
        Manager.Free;
      end;


      pSD := SecurityDescriptor.Create_SD(False);
    end
    else
    begin
      pSD := nil;
    end;
  end;

  procedure PrepareSecurityInformation(SecurityData : PJwSecurityInitializationData;
          Capabilities : TJwComAuthenticationCapabilities; out SecInfo : PSecurityDescriptor);
  begin
    if (acAccessControl in Capabilities) and
       (acAppId in Capabilities) then
    begin
      raise EJwsclInvalidParameterException.
              CreateFmtEx('acAccessControl and acAppId cannot be used at the same time in parameter Capabilities.',
                'Initialize',
                ClassName, 'JwsclComSecurity.pas', 0, false, []);
    end;


    if SecurityData <> nil then
    begin
      if acAccessControl in Capabilities then
      begin
        SecInfo := PSecurityDescriptor(SecurityData.IA^);
      end
      else
      if acAppId in Capabilities then
      begin
        SecInfo := PSecurityDescriptor(@SecurityData.ID);
      end
      else
      begin
        PrepareAndCheckSecurityDescriptor(SecurityData.SD, SecInfo);
      end;
    end
    else
    begin
      SecInfo := nil;
      Exclude(Capabilities, acAccessControl);
      Exclude(Capabilities, acAppId);
    end;
  end;

  procedure FreeSecurityInformation(SecurityData : PJwSecurityInitializationData;
          Capabilities : TJwComAuthenticationCapabilities; var SecInfo : PSecurityDescriptor);
  begin
    if SecInfo <> nil then
    begin
      if not ((acAccessControl in Capabilities) or
         (acAppId in Capabilities)) then
      begin
        TJwSecurityDescriptor.Free_SD(SecInfo);
      end;
    end;
  end;

var
  SecInfo : PSecurityDescriptor;
  List : tagSOLE_AUTHENTICATION_LIST;
begin
  PrepareSecurityInformation(SecurityData, Capabilities, {out} SecInfo);
  try
    if (SecInfo <> nil) and (AuthenticationLevel = calNone) then
      raise EJwsclInvalidParameterException.
        CreateFmtEx('The parameter AuthenticationLevel cannot be calNone if a security information (AppID, IAccessControl or security descriptor) is supplied.',
         'Initialize', ClassName, 'JwsclComSecurity.pas', 0, false, []);


    PrepareAuthInfo(AuthenticationList, {Out}List);
    try
      CoInitializeSecurity(SecInfo,
        AUTO_AUTHENTICATION_SERVICE, nil, AuthenticationLevel, ImpersonationLevel, @List, Capabilities, IgnoreProcess);
    finally
      FreeAuthInfo(List, AuthenticationList, AutoDestroy);
    end;
  finally
    FreeSecurityInformation(SecurityData, Capabilities, {var} SecInfo);
  end;
end;

class procedure TJwComProcessSecurity.Initialize(var AuthenticationList: TJwAuthenticationInfoList;
   AuthenticationLevel: TJwComAuthenticationLevel; ImpersonationLevel: TJwComImpersonationLevel;
  Capabilities: TJwComAuthenticationCapabilities;
  const AutoDestroy : Boolean);
begin
  JwRaiseOnNilParameter(Pointer(AuthenticationList), 'AuthenticationList', 'Initialize (TJwAuthenticationInfoList)', ClassName, 'JwsclComSecurity.pas');

  Exclude(Capabilities, acAppId);
  Exclude(Capabilities, acAccessControl);

  Initialize(
          nil,//SecurityData : PJwSecurityInitializationData;
          AuthenticationList,//var AuthenticationList : TJwAuthenticationInfoList;
          AuthenticationLevel,//AuthenticationLevel: TJwComAuthenticationLevel;
          ImpersonationLevel,//ImpersonationLevel : TJwComImpersonationLevel;
          Capabilities,//Capabilities : TJwComAuthenticationCapabilities;
          AutoDestroy,//const AutoDestroy : Boolean
          True //IgnoreProcess
          );
end;


class procedure TJwComProcessSecurity.Initialize(
  const Domain, UserName, Password: String;
  const AuthorizationService: TJwComAuthorizationService;
  const AuthenticationLevel: TJwComAuthenticationLevel;
  const ImpersonationLevel: TJwComImpersonationLevel;
  const Capabilities: TJwComAuthenticationCapabilities);
var List : TJwAuthenticationInfoList;
begin
  if AuthorizationService = azsDefault then
    raise EJwsclInvalidParameterException.CreateFmtEx(
        'The value azsDefault is not allowed if AuthorizationService is asWinNT or asGSSKerberos.'+
        'Use instead azsNone, azsDCE or azsName.', 'Create',
            ClassName, 'JwsclComSecurity.pas', 0, false, []);

  SetLength(List, {2}1);
  List[0] := TJwAuthenticationInfo.CreateWinNT(UserName, Domain, Password, AuthorizationService);

{  List[1] := TJwAuthenticationInfo.CreateWinNT(UserName, Domain, Password, AuthorizationService);
  List[1].fAuthenticationService := asGSSKerberos;
  }

  Initialize(List, AuthenticationLevel, ImpersonationLevel, Capabilities, True);
end;



class procedure TJwComProcessSecurity.Initialize(SecurityDescriptor: TJwSecurityDescriptor; AuthenticationLevel: TJwComAuthenticationLevel;
  ImpersonationLevel: TJwComImpersonationLevel; Capabilities: TJwComAuthenticationCapabilities);
var
  SecurityData : TJwSecurityInitializationData;
  AuthenticationList : TJwAuthenticationInfoList;
begin
  Exclude(Capabilities, acAppId);
  Exclude(Capabilities, acAccessControl);

  AuthenticationList := nil;
  SecurityData.SD := SecurityDescriptor;

  Initialize(
          @SecurityData,//SecurityData : PJwSecurityInitializationData;
          AuthenticationList,//var AuthenticationList : TJwAuthenticationInfoList;
          AuthenticationLevel,//AuthenticationLevel: TJwComAuthenticationLevel;
          ImpersonationLevel,//ImpersonationLevel : TJwComImpersonationLevel;
          Capabilities,//Capabilities : TJwComAuthenticationCapabilities;
          false,//const AutoDestroy : Boolean
          JwIgnoreHostProcessesInServer //IgnoreProcess
          );
end;

class procedure TJwComProcessSecurity.Initialize(AppID: TGUID; AuthenticationLevel: TJwComAuthenticationLevel;
  ImpersonationLevel: TJwComImpersonationLevel; Capabilities: TJwComAuthenticationCapabilities);
var
  SecurityData : TJwSecurityInitializationData;
  AuthenticationList : TJwAuthenticationInfoList;
begin
  Include(Capabilities, acAppId);
  Exclude(Capabilities, acAccessControl);

  AuthenticationList := nil;
  SecurityData.ID := AppID;

  Initialize(
          @SecurityData,//SecurityData : PJwSecurityInitializationData;
          AuthenticationList,//var AuthenticationList : TJwAuthenticationInfoList;
          AuthenticationLevel,//AuthenticationLevel: TJwComAuthenticationLevel;
          ImpersonationLevel,//ImpersonationLevel : TJwComImpersonationLevel;
          Capabilities,//Capabilities : TJwComAuthenticationCapabilities;
          false, //const AutoDestroy : Boolean
          JwIgnoreHostProcessesInServer //IgnoreProcess
          );
end;

class function TJwComProcessSecurity.CreateMinimumCOMSecurityDescriptor(const MergeSD: TJwSecurityDescriptor): TJwSecurityDescriptor;
begin
  result := TJwSecurityDescriptor.Create;
  result.Owner := JwAdministratorsSID;
  result.PrimaryGroup := JwAdministratorsSID;

  result.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], $FFFF, JwLocalSystemSID, false));
  result.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], $FFFF, JwAuthenticatedUserSID, false));
  result.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create(nil, [], $FFFF, JwAdministratorsSID, false));

  if Assigned(MergeSD) and Assigned(MergeSD.DACL) then
  begin
    result.DACL.AddACEs(MergeSD.DACL);
  end;
end;

class procedure TJwComProcessSecurity.Initialize(AccessControl: IAccessControl; AuthenticationLevel: TJwComAuthenticationLevel;
  ImpersonationLevel: TJwComImpersonationLevel; Capabilities: TJwComAuthenticationCapabilities);
var
  SecurityData : TJwSecurityInitializationData;
  AuthenticationList : TJwAuthenticationInfoList;
begin
  JwRaiseOnNilParameter(Pointer(AccessControl), 'AccessControl', 'Initialize (IAccessControl)', ClassName, 'JwsclComSecurity.pas');


  Include(Capabilities, acAccessControl);
  Exclude(Capabilities, acAppId);

  AuthenticationList := nil;
  SecurityData.IA := @AccessControl;

  Initialize(
          @SecurityData,//SecurityData : PJwSecurityInitializationData;
          AuthenticationList,//var AuthenticationList : TJwAuthenticationInfoList;
          AuthenticationLevel,//AuthenticationLevel: TJwComAuthenticationLevel;
          ImpersonationLevel,//ImpersonationLevel : TJwComImpersonationLevel;
          Capabilities,//Capabilities : TJwComAuthenticationCapabilities;
          false,//const AutoDestroy : Boolean
          JwIgnoreHostProcessesInServer //IgnoreProcess
          );
end;

class procedure TJwComProcessSecurity.Initialize(AuthenticationLevel: TJwComAuthenticationLevel; ImpersonationLevel: TJwComImpersonationLevel;
  Capabilities: TJwComAuthenticationCapabilities);
var
  SecurityData : TJwSecurityInitializationData;
  AuthenticationList : TJwAuthenticationInfoList;
begin
  Exclude(Capabilities, acAccessControl);
  Exclude(Capabilities, acAppId);

  AuthenticationList := nil;
  ZeroMemory(@SecurityData, sizeof(SecurityData));

  Initialize(
          @SecurityData,//SecurityData : PJwSecurityInitializationData;
          AuthenticationList,//var AuthenticationList : TJwAuthenticationInfoList;
          AuthenticationLevel,//AuthenticationLevel: TJwComAuthenticationLevel;
          ImpersonationLevel,//ImpersonationLevel : TJwComImpersonationLevel;
          Capabilities,//Capabilities : TJwComAuthenticationCapabilities;
          false, //const AutoDestroy : Boolean
          JwIgnoreHostProcessesInServer //IgnoreProcess
          );
end;

{ TJwComWinNTIdentity }



procedure TJwComWinNTIdentity.CheckReadonly(const PropertyName: String);
begin
  if fReadOnly then
  begin
    raise EJwsclReadOnlyPropertyException.CreateFmtEx('Property %s is readonly.', 'Getter', ClassName, 'JwsclComSecurity.pas', 0, false, [PropertyName]);
  end;
end;
constructor TJwComWinNTIdentity.Create;
begin
  fReadOnly := false;
  ZeroMemory(@fDataEx, sizeof(fDataEx));

  {implicit set:
  Domain := '';
  PackageList := '';
  Password := '';
  User := '';  }
  Flags := SEC_WINNT_AUTH_IDENTITY_UNICODE;
  Version := SEC_WINNT_AUTH_IDENTITY_VERSION;
end;

constructor TJwComWinNTIdentity.Create(const Identity: TSecWinNTAuthIdentityExW; ReadOnly: Boolean);
begin
  fDataEx := Identity;
  fDataEx.Length := sizeof(fDataEx);

  if fDataEx.Flags and SEC_WINNT_AUTH_IDENTITY_ANSI <> 0 then
  begin
    Domain := String(WideString(PAnsiChar(Identity.Domain)));
    PackageList := String(WideString(PAnsiChar(Identity.PackageList)));
    Password := String(WideString(PAnsiChar(Identity.Password)));
    User := String(WideString(PAnsiChar(Identity.User)));
  end
  else
  begin
    Domain := String(WideString(Identity.Domain));
    PackageList := String(WideString(Identity.PackageList));
    Password := String(WideString(Identity.Password));
    User := String(WideString(Identity.User));
  end;
end;

constructor TJwComWinNTIdentity.Create(const Identity: TSecWinNTAuthIdentityW; ReadOnly: Boolean);
begin
  ZeroMemory(@fDataEx, SizeOf(fDataEx));
  fDataEx.Flags := Identity.Flags;

  if fDataEx.Flags and SEC_WINNT_AUTH_IDENTITY_ANSI <> 0 then
  begin
    Domain := String(WideString(PAnsiChar(Identity.Domain)));
    Password := String(WideString(PAnsiChar(Identity.Password)));
    User := String(WideString(PAnsiChar(Identity.User)));
  end
  else
  begin
    Domain := String(WideString(Identity.Domain));
    Password := String(WideString(Identity.Password));
    User := String(WideString(Identity.User));
  end;
  PackageList := '';
end;


destructor TJwComWinNTIdentity.Destroy;
begin
  CoTaskMemFree(Pointer(fDataEx.User));
  CoTaskMemFree(Pointer(fDataEx.Domain));

  if fDataEx.Password <> nil then
  begin
    ZeroMemory(fDataEx.Password, fDataEx.PasswordLength);
    CoTaskMemFree(Pointer(fDataEx.Password));
  end;

  CoTaskMemFree(Pointer(fDataEx.PackageList));
  ZeroMemory(@fDataEx, SizeOf(fDataEx));
  inherited;
end;


function TJwComWinNTIdentity.GetData: PSecWinNTAuthIdentityW;
begin
  fData.User := fDataEx.User;
  fData.UserLength := fDataEx.UserLength;
  fData.Domain := fDataEx.Domain;
  fData.DomainLength := fDataEx.DomainLength;
  fData.Password := fDataEx.Password;
  fData.PasswordLength := fDataEx.PasswordLength;
  fData.Flags := fDataEx.Flags;

  result := @fData;
end;

function TJwComWinNTIdentity.GetDataEx: PSecWinNTAuthIdentityExW;
begin
  result := @fDataEx;
end;

function TJwComWinNTIdentity.GetDomain: String;
begin
  result := string(WideString(fDataEx.Domain));
end;

function TJwComWinNTIdentity.GetFlags: DWORD;
begin
  result := fDataEx.Flags;
end;

function TJwComWinNTIdentity.GetPackageList: String;
begin
  result := string(WideString(fDataEx.PackageList));
end;

function TJwComWinNTIdentity.GetPassword: String;
begin
  result := string(WideString(fDataEx.Password));
end;

function TJwComWinNTIdentity.GetUser: String;
begin
  result := string(WideString(fDataEx.User));
end;

function TJwComWinNTIdentity.GetVersion: DWORD;
begin
  result := fDataEx.Version;
end;

procedure SetWideChar(var pch : PWideChar; out chLen : DWORD; S : string);
var
  Size : Integer;
begin
  if pch <> nil then
  begin
    CoTaskMemFree(pch);
  end;

  Size := (Length(S) + 2) * SizeOf(WideChar);
  pch := CoTaskMemAlloc(Size);

  if pch = nil then
    OutOfMemoryError;

  ZeroMemory(pch, Size);

  if Length(S) > 0 then
  begin
    CopyMemory(pch, @S[1], Size);
  end;

  chLen := Length(S);
end;

procedure TJwComWinNTIdentity.SetDomain(const Value: String);
begin
  CheckReadonly('Domain');
  SetWideChar(fDataEx.Domain, fDataEx.DomainLength, Value);
end;

procedure TJwComWinNTIdentity.SetFlags(const Value: DWORD);
begin
  CheckReadonly('Flags');
  fDataEx.Flags := Value;
end;

procedure TJwComWinNTIdentity.SetPackageList(const Value: String);
begin
  CheckReadonly('PackageList');
  SetWideChar(fDataEx.PackageList, fDataEx.PackageListLength, Value);
end;

procedure TJwComWinNTIdentity.SetPassword(const Value: String);
begin
  CheckReadonly('Password');
  SetWideChar(fDataEx.Password, fDataEx.PasswordLength, Value);
end;

procedure TJwComWinNTIdentity.SetUser(const Value: String);
begin
  CheckReadonly('User');
  SetWideChar(fDataEx.User, fDataEx.UserLength, Value);
end;

procedure TJwComWinNTIdentity.SetVersion(const Value: DWORD);
begin
  CheckReadonly('Version');
  fDataEx.Version := Version;
end;

constructor TJwComWinNTIdentity.Create(const Domain, UserName, Password: string);
begin
  Create;

  Self.User := UserName;
  Self.Domain := Domain;
  Self.Password := Password;
end;

var
  SaveInitProc: Pointer = nil;

procedure InitComServer;
begin
  //In a service, add code to CoInitializeSecurity here

  if SaveInitProc <> nil then TProcedure(SaveInitProc);
end;


{ TJwAuthenticationInfo }

constructor TJwAuthenticationInfo.Create(const AuthenticationService: TJwComAuthenticationService;
  const AuthorizationService: TJwComAuthorizationService; const AuthenticationInfo: Pointer; const AutoDestroy: boolean);
begin
  fWinNTIdentity := nil;
  fAuthenticationInfo := AuthenticationInfo;
  fAuthenticationService := AuthenticationService;
  fAuthorizationService := AuthorizationService;

  fAutoDestroy := AutoDestroy;
end;

constructor TJwAuthenticationInfo.Create(const AuthenticationService: TJwComAuthenticationService;
  const AuthorizationService: TJwComAuthorizationService; const AuthenticationInfo: TJwComWinNTIdentity; const AutoDestroy: boolean);
begin
  if (AuthenticationService <> asWinNT) and
     (AuthenticationService <> asGSSKerberos) then
  begin
    raise EJwsclInvalidParameterException.CreateFmtEx
      ('Only AuthenticationService = asWinNT and asGSSKerberos are supported.', 'Create', ClassName, 'JwsclComSecurity.pas', 0, false, []);
  end;

  fWinNTIdentity := AuthenticationInfo;
  fAuthenticationInfo := Pointer(fWinNTIdentity.AuthorizationInfo);
  fAuthorizationService := AuthorizationService;

  fAutoDestroy := AutoDestroy;
end;

constructor TJwAuthenticationInfo.CreateWinNT(const UserName, Domain, Password: string; const AuthorizationService: TJwComAuthorizationService);
begin
  fWinNTIdentity := TJwComWinNTIdentity.Create(Domain, UserName, Password);
  fAuthenticationInfo := Pointer(fWinNTIdentity.AuthorizationInfo);
  fAuthorizationService := AuthorizationService;
  fAuthenticationService := asWinNT;
end;

destructor TJwAuthenticationInfo.Destroy;
begin
  if AutoDestroy then
  begin
    if Assigned(fWinNTIdentity) then
    begin
      FreeAndNil(fWinNTIdentity);
    end
    else
    begin
      CoTaskMemFree(fAuthenticationInfo);


      fAuthenticationInfo := nil;
    end;
  end;
  inherited;
end;



initialization
  SaveInitProc := InitProc;
  //InitProc := @InitComServer;

end.

(*

   What we want to do
-----------------------
  When it comes to COM and client security, and the authentication level details used on interface call, you can be sure than sooner or later you'll need to specify the authentication information at runtime, or on a specific call to an interface.

  Usually you set up authentication/authorization/impersonation levels using the Dcomcnfg application provided. How ever, when you require these to be altered or wish to cloak or impersonate an alternate user on calls to a proxy so the server recongnises the alternate user as the callee rather than your natural login account, or the application user account ( i.e service user account).

   Finding out how..
---------------------
  Well to find out how to do this is really not very easy..
very differcult to find examples, people asking how to do it.. people responding where to maybe find out how...
  msdn is definately the best resourse for information on low level security side of COM and authentication documentation on this subject, but finding full examples which work, or all the syntax/constants/data types that's required is always lacking when it comes to msdn library documentation.

   So for all those who have been looking for this answer.


   How it's done.
------------------
  There are a few ways to setup this information, you can set it globally
to your process using the call CoInitializeSecurity(). But I am going to explain how to use the CoSetProxyBlanket() which can set this information on a specific interface (proxy) which is far more flexible.

  CoSetProxyBlanket is a wrapper that simply uses the IClientSecurity interface provided by the proxy and calls the SetBlanket method of the interface.

  I will give you a quick code example of this here and provide you with an attachment which contains all the contants, data types, and a the wrapper function to cater for all the options.

  Enjoy.....

const
  RPC_C_AUTHN_WINNT           = 10;
  RPC_C_AUTHZ_DEFAULT         = $ffffffff;
  RPC_C_AUTHN_LEVEL_CALL      = 3;
  RPC_C_IMP_LEVEL_IMPERSONATE = 3;
  EOAC_NONE                   = $0;

type
   (* The Auth Identity Structure *)
  PCoAuthIdentity = ^TCoAuthIdentity;
  _CoAuthIdentity = packed record
      User           : PChar;
      UserLength     : DWORD;
      Domain         : PChar;
      DomainLength   : DWORD;
      Password       : PChar;
      PasswordLength : DWORD;
      Flags          : DWORD;
  end;
  TCoAuthIdentity = _CoAuthIdentity;

implementation

(* Procedure to demonstrate the use of the CoSetProxyBlanket call
       and how to use it to impersonate another user when calling
       an interface. *)

procedure SetUserImpersonateOnProxy(
     Proxy: IUnknown;  //-- Interface
     const UserName, DomainName, Psword: String; //-- User Account
     AuthenicationService: DWORD = RPC_C_AUTHN_WINNT;
     AuthorizationService: DWORD = RPC_C_AUTHZ_DEFAULT;
     AuthenicationLevel: DWORD = RPC_C_AUTHN_LEVEL_CALL;
     ImpersonationLevel: DWORD = RPC_C_IMP_LEVEL_IMPERSONATE;
     CapabiltiesFlag: DWORD = EOAC_NONE
                                    );
var
    AuthIdent: TCoAuthIdentity;
    iResult: Integer;
begin
(* Populate an Auth Identity structure with the User Account Details *)
    ZeroMemory(@AuthIdent, 0);
    with AuthIdent do begin
        User := pChar(UserName);
        UserLength := length(UserName);
        Domain := pChar(DomainName);
        DomainLength := length(DomainName);
        Password := pChar(Psword);
        PasswordLength := length(Psword);
        Flags := SEC_WINNT_AUTH_IDENTITY_ANSI;
    end;

    iResult := CoSetProxyBlanket(Proxy,
          (* Authentication Service is the service which will be used
             for authentication i.e WinNT NTLM KERBEROS etc.. this rarely
             needs to be changed unless Delegation level of impersonation
             is required and this is only possible with Windows 2000 and
             Kerberos Authentication Service *)
                                 AuthenicationService,
                                 AuthorizationService,
                                 0,
          (* Authentication level should be CALL or PKT as this is the
             level when authentication will take place.. On each CALL. *)
                                 AuthenicationLevel,
          (* Impersonation Level regards to the servers rights to
             impersonate as the authenticated user. *)
                                 ImpersonationLevel,
                                 @AuthIdent,
                                 CapabiltiesFlag);
    case iResult of
        S_OK: (* Success *) ;
        E_INVALIDARG : Raise Exception.Create('Invalid Arguments.');
        E_OUTOFMEMORY : OutOfMemoryError;
        else
           Raise Exception.Create('Failed to blanket proxy')
    end;
end;


-------------

var
    Intf: IMyFooServer;
begin
    Intf := CoMyFooServer.Create;
    try
       SetUserImpersonateOnProxy(Intf, 'AUser', 'DELPHIDOMAIN', 'FooBar');
         (* Interface will authenticate the user AUser as the
             Callee on each call to the Server. *)
       Intf.CallMethodAsAUser(Blah);
    finally
       Intf := NIL;
    end;
end;

*)
