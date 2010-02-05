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
3. impl. LegacySecureReferences (S390)
4. impl LegacyMutualAuthentication
5. add feature to read appido from HKEY_LOCAL_MACHINE\SOFTWARE\Classes\AppID, inst. HKCRK
6. add dllsurrogate to reg class
7. String -> TJwString

9. RunAs für Services usw (Vista kein pass, < : leeres pass)
10. Test dllsurrgotate with several com servers in dllhost.exe


Things to know:

1. Call to CreateComObject/CoCreateInstance fails with EOleSysError "Failed to start server".
An out of process COM server (usually) uses the identity given by the caller's process token.
However if you impersonate the current thread and then call CoCreateInstance or CreateComObject, the server is created
using the given thread token. In some circumstances this may fail with an error "Failed to start server" and a Windows log event
"Invalid parameter".
This happened to me because I used LOGON32_LOGON_INTERACTIVE in a LogonUser call. Instead use LOGON32_LOGON_BATCH to solve this problem.

2. What is cAuthSvc in CoInitializeSecurity
cAuthSvc defines an array of by the server supported authentication services. It is an array of SOLE_AUTHENTICATION_SERVICE
in where you define which authentication services your server supports. E.g. you can support e.g. RPC_C_AUTHN_GSS_KERBEROS and
RPC_C_AUTHN_WINNT to allow users to be impersonated by the server. The dwAuthzSvc member is ignored by those two services.
COM uses RPC_C_AUTHN_WINNT with all members left set to empty or 0. (WinVista)

Here are some rules:
1. You need to define an authentication service if you want to get information about the client. Using RPC_C_AUTHN_NONE
will prevent you from calling ImpersonateClient or getting the client's context (CoGetClientContext).
2. If you set the member pPrincipalName only the client running with the principal's identity can get a class from the server. The client can impersonate before
it creates the class (CreateComObject/CoCreateInstance). Use an empty string to allow everyone to use this service.
a) For WinNT/Kerberos authentication level, the pPrincipalName cannot be a group.
b) The supplied credentials in pAuthList of CoInitializeSecurity are not used, at least on my Win2008 Server.

3. Usually dwAuthnLevel defines two different things for Client and Server.
 Server: It defines the lowest authentication level allowed to connect to the server. Lower levels are rejected.
 Client: It defines the authentication level wished to be used by the client. If it is higher than the auth level set by the server,
    the client's level is used. The higher the better.

 However, on my Win2008 and RPC_C_AUTHN_WINNT, the highest available level (RPC_C_AUTHN_LEVEL_PKT_PRIVACY) was used by default.
 Even if set a level too low in a call to CoInitializeSecurity, this high level was used. I could not make it smaller by setting a proxy
 on the interface. Only RPC_C_AUTHN_LEVEL_NONE turns off all authentication.


}
unit JwsclComSecurity;

{$INCLUDE ..\includes\Jwscl.inc}


interface
uses
  ActiveX,
  JwaWinNT, //only TEMP
  //only temp
  JwaCOMSecurity,

  JwaWindows,      //JEDI API unit

  ComObj,


  //more custom units here
  Classes,
  SysUtils,
  Registry,
  Dialogs,
  SyncObjs,


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

  {TJwComCustomSecurity is the base class for the JWSCL COM security implementation.
   Do not use it directly instead you can inherit from it to get access to the implementation.
  }
  TJwComCustomSecurity = class(TInterfacedObject{, IJwBase})
  protected
    fUpdating,
    fReadOnly : Boolean;

    fAuthenticationInfo: RPC_AUTH_IDENTITY_HANDLE;
    fServerPrincipalName: TJwString;
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
    function GetImpersonationLevel: TJwComImpersonationLevel;
    function GetReadOnlyProperties: Boolean;
    function GetServerPrincipalName: TJwString;
    procedure SetAuthenticationInfo(const Value: RPC_AUTH_IDENTITY_HANDLE);
    procedure SetAuthenticationLevel(const Value: TJwComAuthenticationLevel);
    procedure SetAuthenticationService(const Value: TJwComAuthenticationService);
    procedure SetAuthorizationService(const Value: TJwComAuthorizationService);
    procedure SetCapabilites(const Value: TJwComAuthenticationCapabilities);
    procedure SetImpersonationLevel(const Value: TJwComImpersonationLevel);
    procedure SetReadOnlyProperties(const Value: Boolean);
    procedure SetServerPrincipalName(const Value: TJwString);
    function GetCapabilites: TJwComAuthenticationCapabilities;



  protected
    {BeginUpdate is used
    }
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
    property ServerPrincipalName : TJwString read GetServerPrincipalName write SetServerPrincipalName;
    property AuthenticationLevel : TJwComAuthenticationLevel read GetAuthenticationLevel write SetAuthenticationLevel;

    //http://msdn.microsoft.com/en-us/library/ms693790(VS.85).aspx
    property ImpersonationLevel : TJwComImpersonationLevel read GetImpersonationLevel write SetImpersonationLevel;
    property AuthenticationInfo : RPC_AUTH_IDENTITY_HANDLE read GetAuthenticationInfo write SetAuthenticationInfo;
    property Capabilites  : TJwComAuthenticationCapabilities read GetCapabilites write SetCapabilites;
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
        {__in_opt}  pServerPrincName : TJwString;
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
        {__out_opt}  out pServerPrincName : TJwString;
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

    class function GetAuthenticationServices : TJwAuthenticationServiceInformationArray;

    property Proxy : IInterface read fProxy;

    {
    }
    property WinNTIdentity : TJwComWinNTIdentity read fWinNTIdentity;

    property ReadOnlyProperties;

    //http://msdn.microsoft.com/en-us/library/ms692656%28VS.85%29.aspx
    property AuthenticationService;
    property AuthorizationService;
    property ServerPrincipalName;
    property AuthenticationLevel;

    //http://msdn.microsoft.com/en-us/library/ms693790(VS.85).aspx
    property ImpersonationLevel;
    property AuthenticationInfo;
    property Capabilites;


  end;

  {TJwAuthenticationInfo wraps an array of authentication information
   to be used by TJwComProcessSecurity.
  }
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

    {Creates a WinNT authentication information structure.

    }
    constructor CreateWinNT(
      const UserName  : TJwString;
      const Domain    : TJwString;
      const Password  : TJwString;
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
   They can be used by a COM server or client. Some of them are only
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

      If you intend to use this function (effectively CoInitializeSecurity) in a service provided by VCL (TService)
      you need to call it in a different way:
        1. Application.DelayInitialize := true;
      or
        2. Use the demonstration code in your application as demonstrated at the end of this file:
        <pre lang="delphi">
          var
            SaveInitProc: Pointer = nil;

          procedure InitComServer;
          begin
            //In a service, add code to CoInitializeSecurity here

            if SaveInitProc <> nil then TProcedure(SaveInitProc);
          end;

          initialization
            SaveInitProc := InitProc;
            InitProc := @InitComServer;
        </pre>
        In this way the call to CoInitializeSecurity is made before VCL inits COM.

      For more information see
        http://blog.delphi-jedi.net/2009/12/16/windows-2003-server-requires-startservicectrldispatcher/

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
          const Domain, UserName, Password: TJwString;
          const AuthorizationService: TJwComAuthorizationService;
          const AuthenticationLevel: TJwComAuthenticationLevel;
          const ImpersonationLevel: TJwComImpersonationLevel;
          const Capabilities: TJwComAuthenticationCapabilities); overload; virtual;

    {CreateTightServerSecurityOptions returns settings for a COM server that shall running
     tight security settings.

    Parameters
      SecurityDescriptor A variable that receives a tight security descriptor. The function returns
        a new class instance that must be freed. This value must be a variable set to the constant JwDefaultComSD
        to work this way. Otherwise the variable will not be changed. In this way you can create your own
        SD before calling this function or leave it to COM to set the default descriptor (use NIL).
        Be aware that this security descriptor only has access for SYSTEM and Local Administrators. You need
        to add further accounts to allow other users to connect.

    Remarks
      The procedure uses the values from JwTightCOMSecuritySettings. You can change them to use "customized"
      security.
    }
    class procedure CreateTightServerSecurityOptions(
          var SecurityDescriptor : TJwSecurityDescriptor;
          out AuthenticationLevel : TJwComAuthenticationLevel;
          out ImpersonationLevel : TJwComImpersonationLevel;
          out Capabilities : TJwComAuthenticationCapabilities);
  end;


  TJwServerImpersonationType = (
    //impersonate on creation, revert on destroying
    sitAutoImpersonation,
    //impersonate on creation, but do not revert
    sitOnlyImpersonation,
    //do not impersonate at all
    sitNoImpersonation);

  {TJwComWinNTIdentity wraps the record TSecWinNTAuthIdentityExW.
  }
  TJwComWinNTIdentity = class
  private
    function GetDomain: TJwString;
    function GetFlags: DWORD;
    function GetPackageList: TJwString;
    function GetPassword: TJwString;
    function GetUser: TJwString;
    function GetVersion: DWORD;
    procedure SetDomain(const Value: TJwString);
    procedure SetFlags(const Value: DWORD);
    procedure SetPackageList(const Value: TJwString);
    procedure SetPassword(const Value: TJwString);
    procedure SetUser(const Value: TJwString);
    procedure SetVersion(const Value: DWORD);
    function GetData: PSecWinNTAuthIdentityW;
    function GetDataEx: PSecWinNTAuthIdentityExW;
  protected
    fReadOnly : Boolean;
    fDataEx : TSecWinNTAuthIdentityExW;
    fData : TSecWinNTAuthIdentityW;

    procedure CheckReadonly(const PropertyName : String);
  public
    {Creates a new TSecWinNTAuthIdentityXXW structure (XX is optional)
     All values are zeroed and writeable.
    }
    constructor Create; overload;

    {
    Parameters
      Identity:
        An existing TSecWinNTAuthIdentityExW structured which members are copied into internal structures.
      ReadOnly:
        A one time value that allows or disallows to write to the properties.
        If this parameter is set to true every write access to a properties throws the exception
        EJwsclReadOnlyPropertyException.
    }
    constructor Create(const Identity : TSecWinNTAuthIdentityExW; ReadOnly : Boolean = false); overload;

    {
    Parameters
      Identity:
        An existing TSecWinNTAuthIdentityW structured which members are copied into internal structures.
      ReadOnly:
        A one time value that allows or disallows to write to the properties.
        If this parameter is set to true every write access to a properties throws the exception
        EJwsclReadOnlyPropertyException.
    }
    constructor Create(const Identity : TSecWinNTAuthIdentityW; ReadOnly : Boolean = false); overload;

    {Creates a new internal TSecWinNTAuthIdentityW structure initialized with the given parameters.
    }
    constructor Create(const Domain, UserName, Password : TJwString); overload;

    destructor Destroy; override;

    {
  SEC_WINNT_AUTH_IDENTITY_VERSION;
    }
    property Version  : DWORD read GetVersion write SetVersion;
    {
    }
    property User     : TJwString read GetUser write SetUser;
    {
    }
    property Domain   : TJwString read GetDomain write SetDomain;
    {
    }
    property Password : TJwString read GetPassword write SetPassword;
    {
    Only valid values are
      SEC_WINNT_AUTH_IDENTITY_UNICODE and SEC_WINNT_AUTH_IDENTITY_ANSI
    }
    property Flags    : DWORD read GetFlags write SetFlags;
    {
    }
    property PackageList : TJwString read GetPackageList write SetPackageList;

    //The parameter supplied to the constructors Create. If set to true all properties cannot be changed.
    property ReadOnly : Boolean read fReadOnly;

    //A pointer to an internal structure to be used by WinAPI functions directly
    property AuthorizationInfoEx : PSecWinNTAuthIdentityExW read GetDataEx;

    //A pointer to an internal structure to be used by WinAPI functions directly
    property AuthorizationInfo : PSecWinNTAuthIdentityW read GetData;
  end;

  {IJwComServerSecurity is implemented by TJwComServerSecurity.
   It is returned by TJwComServerSecurity.GetServerSecurity

   You can use Delphi's interface management to get information about a client
   and do impersonation without bothering about reverting or destroying the instance.

   For more information on the methods see the documentation of TJwComServerSecurity.
  }
  IJwComServerSecurity = interface
    procedure AccessCheck(
        const DesiredAccess : DWORD;
        const SecurityDescriptor : TJwSecurityDescriptor;
        const GenericMapping: TJwSecurityGenericMappingClass;
        out AccessGranted : Boolean;
        out GrantedAccessMask : DWORD
        );

    procedure AccessCheckCached(
        var CacheResult : TAuthZAccessCheckResultHandle;

        const DesiredAccess : DWORD;
        const SecurityDescriptor : TJwSecurityDescriptor;
        const GenericMapping: TJwSecurityGenericMappingClass;
        out AccessGranted : Boolean;
        out GrantedAccessMask : DWORD
        );

    procedure ImpersonateClient;

    {Removes the thread token.

    Exceptions
      EJwsclWinCallFailedException CoRevertToSelf failed.
    }
    procedure RevertToSelf;

    function GetWinNTIdentity: TJwComWinNTIdentity;
    function GetUserName: TJwString;
    function GetAuthenticationInfo: RPC_AUTH_IDENTITY_HANDLE;
    function GetAuthenticationLevel: TJwComAuthenticationLevel;
    function GetAuthenticationService: TJwComAuthenticationService;
    function GetAuthorizationService: TJwComAuthorizationService;
    function GetImpersonationLevel: TJwComImpersonationLevel;
    function GetReadOnlyProperties: Boolean;
    procedure SetAuthenticationInfo(const Value: RPC_AUTH_IDENTITY_HANDLE);
    procedure SetAuthenticationLevel(const Value: TJwComAuthenticationLevel);
    procedure SetAuthenticationService(const Value: TJwComAuthenticationService);
    procedure SetAuthorizationService(const Value: TJwComAuthorizationService);
    procedure SetCapabilites(const Value: TJwComAuthenticationCapabilities);
    procedure SetImpersonationLevel(const Value: TJwComImpersonationLevel);
    function GetCapabilites: TJwComAuthenticationCapabilities;
    procedure SetAuthContext(const Value: TJwAuthContext);
    procedure SetAuthManager(const Value: TJwAuthResourceManager);
    function GetAuthManager: TJwAuthResourceManager;
    function GetAuthContext: TJwAuthContext;

    property WinNTIdentity : TJwComWinNTIdentity read GetWinNTIdentity;
    property UserName : TJwString read GetUserName;

    property AuthenticationService : TJwComAuthenticationService read GetAuthenticationService write SetAuthenticationService;
    property AuthorizationService : TJwComAuthorizationService read GetAuthorizationService write SetAuthorizationService;
    property AuthenticationLevel : TJwComAuthenticationLevel read GetAuthenticationLevel write SetAuthenticationLevel;

    property AuthenticationInfo : RPC_AUTH_IDENTITY_HANDLE read GetAuthenticationInfo write SetAuthenticationInfo;
    property Capabilites  : TJwComAuthenticationCapabilities read GetCapabilites write SetCapabilites;


    property AuthManager : TJwAuthResourceManager read GetAuthManager write SetAuthManager;
    property AuthContext : TJwAuthContext read GetAuthContext write SetAuthContext;

  end;

  {TJwComServerSecurity provides functionality
  for a server to query client information, to impersonate
  and to do access checks.
  }
  TJwComServerSecurity = class(TJwComCustomSecurity, IJwComServerSecurity)
  protected
    function GetToken: TJwSecurityToken;
    function GetUserName: TJwString;
  private
    procedure SetAuthContext(const Value: TJwAuthContext);
    procedure SetAuthManager(const Value: TJwAuthResourceManager);
    function GetWinNTIdentity: TJwComWinNTIdentity;
    function GetAuthManager: TJwAuthResourceManager;
    function GetAuthContext: TJwAuthContext;

  protected
    fAuthManager : TJwAuthResourceManager;
    fAuthContext : TJwAuthContext;

    fToken : TJwSecurityToken;
    fWinNTIdentity : TJwComWinNTIdentity;

    fImpersonationType : TJwServerImpersonationType;

    {Wraps the API function with same name.
     Used by server only.
    }
    class procedure CoQueryClientBlanket(
        {__out_opt}  out pwAuthnSvc : TJwComAuthenticationService;
        {__out_opt}  out pAuthzSvc : TJwComAuthorizationService;
        {__out_opt}  out pServerPrincName : TJwString;
        {__out_opt}  out pAuthnLevel : TJwComAuthenticationLevel;
        {__out_opt}  out pImpLevel : TJwComImpersonationLevel;
                     out pPrivs : RPC_AUTHZ_HANDLE;
        {__out_opt}  var pCapabilites : TJwComAuthenticationCapabilities
    ); virtual;

  public
    {Creates a COM server instance.

    Parameters
      ImpersonationType:

     Remarks
       All properties are read only. Trying to set a value raises EJwsclReadOnlyPropertyException.
    }
    constructor Create(const ImpersonationType : TJwServerImpersonationType = sitAutoImpersonation);
    destructor Destroy; override;

    class function GetServerSecurity(const ImpersonationType : TJwServerImpersonationType = sitAutoImpersonation) : IJwComServerSecurity;

    {AccessCheck does a one time AccessCheck using the current client's identity.

    Parameters
       DesiredAccess:
          Receives the access mask set by a client to get access.
       SecurityDescriptor:
          A security descriptor assigned to the secured object.
       GenericMapping:
          A mapping class that maps generic access rights to specific ones.
          It is used by the AccessCheck function to convert generic access rights in
           the security descriptor. Can be nil.

       AccessGranted:
          Returns whether access is granted (true) or not (false). This value is only
          valid if no exception is thrown.
       GrantedAccessMask:
          Returns the amount of access rights granted. This value is only
          valid if no exception is thrown.
    }
    procedure AccessCheck(
        const DesiredAccess : DWORD;
        const SecurityDescriptor : TJwSecurityDescriptor;
        const GenericMapping: TJwSecurityGenericMappingClass;
        out AccessGranted : Boolean;
        out GrantedAccessMask : DWORD
        );

    {AccessCheck does a cached AccessCheck using the current client's identity.


    Parameters
       CacheResult:
          Receives the value from the first AccessCheck made.
          The first time parameter CacheResult must be a variable with value zero or INVALID_HANDLE_VALUE.
          The handle must be freed by the WinAPI call AuthzFreeHandle .
       DesiredAccess:
          Receives the access mask set by a client to get access.
       SecurityDescriptor:
          A security descriptor assigned to the secured object.
       GenericMapping:
          A mapping class that maps generic access rights to specific ones.
          It is used by the AccessCheck function to convert generic access rights in
           the security descriptor. Can be nil.

       AccessGranted:
          Returns whether access is granted (true) or not (false). This value is only
          valid if no exception is thrown.
       GrantedAccessMask:
          Returns the amount of access rights granted. This value is only
          valid if no exception is thrown.
    }
    procedure AccessCheckCached(
        var CacheResult : TAuthZAccessCheckResultHandle;

        const DesiredAccess : DWORD;
        const SecurityDescriptor : TJwSecurityDescriptor;
        const GenericMapping: TJwSecurityGenericMappingClass;
        out AccessGranted : Boolean;
        out GrantedAccessMask : DWORD
        );


    {Impersonates the current thread and removes any previous thread token.

     Remarks
       Use this call instead of CoImpersonateClient because it raises an exception if the call fails.
       Without checking the return value any subsequent call will run on the process token which
       is a security breach.

     Exceptions
      EJwsclWinCallFailedException CoImpersonateClient failed.
    }
    procedure ImpersonateClient;

    {Removes the thread token.

    Exceptions
      EJwsclWinCallFailedException CoRevertToSelf failed.
    }
    procedure RevertToSelf;

    {Returns true if the current thread has a token assigned (is impersonating);
     otherwise false.

     The function does not distuingish between a client token connected to the server
     and any other assigned token.
    }
    function IsImpersonating : Boolean;

    {WinNTIdentity contains the user's identity if the property AuthenticationService
     is asWinNT or asGSSKerberos; otherwise it is nil.

     This instance is cached by the class and must not be freed.
    }
    property WinNTIdentity : TJwComWinNTIdentity read GetWinNTIdentity;

    {Returns the user's name connected to the server.
     Either it uses the current thread token or it impersonates the user
     if no thread token is available.
     If ImpersonationLevel is cilAnonymous the return value is an empty string.
    }
    property UserName : TJwString read GetUserName;

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
    property ImpersonationLevel;

    property AuthenticationInfo;
    property Capabilites;

    {The authentication manager used by AccessCheck.
     This value is nil if AccessCheck wasn't called yet.

     On write the current instance of the authentication manager is freed and
     the value is directly written to the internal instance variable.
     So do not free it yourself, the class instance will do it.

     Do not free the instance and then call AccessCheckCached.
    }
    property AuthManager : TJwAuthResourceManager read GetAuthManager write SetAuthManager;

    {The authentication context used by AccessCheck.
    This value is nil if AccessCheck wasn't called yet.

    On write the current instance of the authentication context is freed and
     the value is directly written to the internal instance variable.
     So do not free it yourself, the class instance will do it.

     Do not free the instance and then call AccessCheckCached.
    }
    property AuthContext : TJwAuthContext read GetAuthContext write SetAuthContext;
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
    function GetServiceParameters: TJwString;
    function GetSRPTrustLevel: TJwSaferLevelId;
    procedure SetAccessPermission(const Value: TJwSecurityDescriptor);
    procedure SetAuthenticationLevel(const Value: TJwComAuthenticationLevel);
    procedure SetLaunchPermission(const Value: TJwSecurityDescriptor);
    procedure SetLoadUserProfile(const Value: Boolean);
    procedure SetRunAs(const Value: TJwSecurityID);
    procedure SetServiceParameters(const Value: TJwString);
    procedure SetSRPTrustLevel(const Value: TJwSaferLevelId);
    function GetAppIdRegFlags: TJwComAppIdRegFlags;
    procedure SetAppIdRegFlags(const Value: TJwComAppIdRegFlags);
    function GetRunAsString: TJwString;
    procedure SetRunAsString(const Value: TJwString);
    function GetROTFlags: TJwComRotFlags;
    procedure SetROTFlags(const Value: TJwComRotFlags);
  protected
    fLaunchPermission : TJwSecurityDescriptor;
    fAccessPermission : TJwSecurityDescriptor;
    fRunAs : TJwSecurityID;
    fServiceParameters : TJwString;
    fAuthenticationLevel : TJwComAuthenticationLevel;
    fAppIdRegFlags : TJwComAppIdRegFlags;
    fSRPTrustLevel : TJwSaferLevelId;
    fLoadUserProfile : Boolean;
    fReadOnly : Boolean;

    Reg : TRegistry;

    procedure CheckReadonly(const PropertyName : String);

    //Reads a self relative SD from registry
    class function ReadSD(Reg : TRegistry; const KeyName : TJwString; var Default :TJwSecurityDescriptor) : TJwSecurityDescriptor;
    //Writes a self relative SD to registry
    class procedure WriteSD(Reg : TRegistry; const KeyName : TJwString; const SD : TJwSecurityDescriptor; DoDelete : Boolean);



  public
    {Create opens the AppID of a specific COM application.

    Parameter
      AppID: GUID of the app.
      ReadOnly: Defines whether all properties are read only (true) or not (false).
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

    {Returns the default legacy authentication level.}
    class function GetLegacyAuthenticationLevel : TJwComAuthenticationLevel; virtual;

    {Returns the default legacy impersonation level.}
    class function GetLegacyImpersonationLevel : TJwComImpersonationLevel; virtual;

    class function GetGlobalAuthenticationServices : TJwSecurityPackageInformationArray;

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
    property RunAsString : TJwString read GetRunAsString write SetRunAsString;

    {Sets or gets the service parameters that are delivered to the service on startup.
    The key value is deleted if the property is set to empty.
    }
    property ServiceParameters : TJwString read GetServiceParameters write SetServiceParameters;

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

    {Sets or get the value for running object table (ROT) flags.
    If this key is not available the value false is returned.
    }
    property ROTFlags : TJwComRotFlags read GetROTFlags write SetROTFlags;

    //Returns the GUID supplied to Create
    property AppID : TGuid read fAppID;
  end;


  TJwTrustee = record
  end;

  TJwServerAccessControl = class;

  {This method is a callback method used by property TJwServerAccessControl.OnIsAccessAllowed
   @param Sender  This parameter defines which instance of TJwServerAccessControl is calling the method.
   @param DesiredAccess Defines the access requested by the caller of the AccessCheck function.
   @param TrusteeName Defines the name trustee who is used to make the request.
   @param TrusteeSID Defines the trustee as a TJwSecurityID class who is used to make the request.
   @param AProperty Defines a property supplied to the TJwServerAccessControl.JwIsAccessAllowed.
   @param Trustee Not used. Always nil.
   @param AccessGranted Receives the previous result of the access check. Set to true to grant access; otherwise
    access will be denied.
   @param ErrorCode Defines an error code that is returned to any caller. Actually an error code of < 0 will
      raise an EOleSysError exception with a generic message. You can raise your own EOleSysError exception to return
      any HRESULT value to a COM caller. In this way you can see your custom made error message (good for debugging).
      However, if you use other exception classes COM callers will receive the HRESULT error E_UNEXPECTED.
  }
  TJwIsAccessAllowed = procedure (const Sender : TJwServerAccessControl;
      const DesiredAccess : DWORD;
      const TrusteeName : TJwString;
      const TrusteeSID : TJwSecurityId;
      const AProperty : TJwString;
      const Trustee : PTrusteeW;
      var AccessGranted : Boolean;
      var ErrorCode : HRESULT) of object;

  TJwFacilityType = 0..$7FF;

  {Defines the used method to load from or save to a stream a security descriptor.
  }
  TJwPersistStreamType = (

    pstPlain,
    pstJwscl,
    pstIAccessControl
  );

  {TJwServerAccessControl provides an implementation of
    IAccessControl, IPersist and IPersistStream.
   It implements the COM IAccessControl interface to support a security descriptor for
     server interfaces. It supports Owner, Group (in contrast to the MS implementation of IAccessControl)
      and DACL.
   This object also supports streaming through IPersistStream.

   Remarks
    All COM methods (the methods inherited by the interfaces) are exception proof. I.e. an arbitrary exception
    is resolved to the error value E_UNEXPECTED. However, the exception EOleSysError can be used to return any
    other COM error.
    On the other hand, in debug mode these COM methods raise an exception rather than returning a HRESULT value.
    It is easier to find an error by an exception than a quiet return value.
  }
  TJwServerAccessControl = class(TInterfacedObject, IAccessControl, IPersist, IPersistStream)
  protected
    fPersistStreamType: TJwPersistStreamType;
    fStrictACLVerify: Boolean;

    fFacilityCode: TJwFacilityType;
    fCriticalSection : TCriticalSection;
    fPersistClassID: TGUID;
    fSupportIPersistStream: Boolean;

    fSD : TJwSecurityDescriptor;
    fIsAccessAllowed : TJwIsAccessAllowed;

    fAuthManager : TJwAuthResourceManager;
    fGenericMapping: TJwSecurityGenericMappingClass;
    fCacheResult: TAuthZAccessCheckResultHandle;

    fIsDirty : Boolean;

    function TrusteeToSid(const Trustee : PTrusteeW) : TJwSecurityId; virtual;
    procedure CopyACLToObject(const pAccessList: PACTRL_ACCESSW;var ACL : TJwDAccessControlList); virtual;

    //trustee check for strict validation
    function IsValidTrustee(const Trustee : PTrusteeW) : Boolean;
  public
    {
    This method is affected by property StrictACLVerify when set to True.
    }
    function GrantAccessRights(pAccessList: PACTRL_ACCESSW): HRESULT; stdcall;

    {
    This method is affected by property StrictACLVerify when set to True.
    }
    function SetAccessRights(pAccessList: PACTRL_ACCESSW): HRESULT; stdcall;

    {
    This method is affected by property StrictACLVerify when set to True.
    }
    function SetOwner(pOwner: PTRUSTEEW; pGroup: PTRUSTEEW): HRESULT; stdcall;

    {
    This method is affected by property StrictACLVerify when set to True.
    }
    function RevokeAccessRights(lpProperty: LPWSTR; cTrustees: ULONG; prgTrustees: PTRUSTEEW): HRESULT; stdcall;

    {Returns all access rights, owner and group of the object.

    This method is affected by property StrictACLVerify when set to True.
    On the other hand if StrictACLVerify is false the function can produce incompatible results to IAccessControl.

    If SD.DACL is nil the result is incompatible with IAccessControl.
    }
    function GetAllAccessRights(lpProperty: LPWSTR; var ppAccessList: PACTRL_ACCESSW_ALLOCATE_ALL_NODES;
       var ppOwner, ppGroup: PTRUSTEEW): HRESULT; stdcall;

    {Frees the parameters returned by GetAllAccessRights}
    class procedure FreeAccessRights(var AccessList : PACTRL_ACCESSW_ALLOCATE_ALL_NODES; var Owner, Group : PTrusteeW);

    {
    This method is affected by property StrictACLVerify when set to True.


    In case of StrictACLVerify is true pTrustee must be a user or group SID.
    }
    function IsAccessAllowed(pTrustee: PTRUSTEEW; lpProperty: LPWSTR; AccessRights: ACCESS_RIGHTS; var pfAccessAllowed: BOOL): HRESULT; stdcall;

    function GetClassID(out classID: TCLSID): HRESULT; stdcall;

    function IsDirty: HRESULT; stdcall;
    function Load(const stm: IStream): HRESULT; stdcall;
    function Save(const stm: IStream; fClearDirty: BOOL): HRESULT; stdcall;
    function GetSizeMax(out cbSize: Largeint): HRESULT; stdcall;
  public
    constructor Create; overload;
    constructor Create(const SD : TJwSecurityDescriptor); overload;
    destructor Destroy; override;

    {CreateCOMImplementation returns an instance of a IAccessControl interface implemented by MS.
    }
    class function CreateCOMImplementation : IAccessControl; virtual;

    {Adds an access list to the DACL of the security descriptor.

    Remarks:
      The function is called by the interface method with the same name. Since the interface method returns a HRESULT value
      this version must raise an EOleSysError and supply the return value to EOleSysError constructor (ErrorCode).
      Use
      <code>
        raise EOleSysError.Create('text message', MAKE_HRESULT(1, FacilityCode, Win32_or_User_Code));
      </pre>
    }
    procedure JwGrantAccessRights(const AccessList: TJwDAccessControlList); virtual;

    {Replaces the DACL of the security descriptor with the given access list.

    Remarks:
      The function is called by the interface method with the same name. Since the interface method returns a HRESULT value
      this version must raise an EOleSysError and supply the return value to EOleSysError constructor (ErrorCode).
      Use
      <code>
        raise EOleSysError.Create('text message', MAKE_HRESULT(1, FacilityCode, Win32_or_User_Code));
      </pre>
    }
    procedure JwSetAccessRights(const AccessList: TJwDAccessControlList); virtual;

    {Replaces the owner and/or group of the security descriptor.


    Remarks:
      If a parameter is nil it will be ignored.

      The function is called by the interface method with the same name. Since the interface method returns a HRESULT value
      this version must raise an EOleSysError and supply the return value to EOleSysError constructor (ErrorCode).
      Use
      <code>
        raise EOleSysError.Create('text message', MAKE_HRESULT(1, FacilityCode, Win32_or_User_Code));
      </pre>
    }
    procedure JwSetOwner(const Owner, Group : TJwSecurityID); virtual;

    {
    JwRevokeAccessRights removes all given access entries from the DACL.

    Remarks:
      The function is called by the interface method with the same name. Since the interface method returns a HRESULT value
      this version must raise an EOleSysError and supply the return value to EOleSysError constructor (ErrorCode).
      Use
      <code>
        raise EOleSysError.Create('text message', MAKE_HRESULT(1, FacilityCode, Win32_or_User_Code));
      </pre>
    }
    procedure JwRevokeAccessRights(const AProperty: TJwString; const SecurityIDList : TJwSecurityIdList); virtual;

    {
    Remarks:
      The function is called by the interface method with the same name. Since the interface method returns a HRESULT value
      this version must raise an EOleSysError and supply the return value to EOleSysError constructor (ErrorCode).
      Use
      <code>
        raise EOleSysError.Create('text message', MAKE_HRESULT(1, FacilityCode, Win32_or_User_Code));
      </pre>
    }
    procedure JwGetAllAccessRights(const AProperty: TJwString; out AccessList: TJwDAccessControlList; out Owner, Group : TJwSecurityID); virtual;

    {
    Remarks:
      The function is called by the interface method with the same name. Since the interface method returns a HRESULT value
      this version must raise an EOleSysError and supply the return value to EOleSysError constructor (ErrorCode).
      Use
      <code>
        raise EOleSysError.Create('text message', MAKE_HRESULT(1, FacilityCode, Win32_or_User_Code));
      </pre>
    }
    procedure JwIsAccessAllowed(const Trustee: TJwSecurityID; const AProperty: TJwString; const AccessRights: TJwAccessMask; var AccessAllowed: Boolean); virtual;


    {Resets the AccessControl instance.
     * It removes the DACL and creates a new one with no access at all.
     * It sets owner and group to NULL SID.
     * Sets dirty flag to false
    }
    procedure Clear;

    procedure SaveToStream(const Stream : TStream); virtual;
    procedure LoadFromStream(const Stream : TStream); virtual;


    property SecurityDescriptor : TJwSecurityDescriptor read fSD;

    {OnIsAccessAllowed defines a method that is called after JwIsAccessAllowed made an AccessCheck (result isn't important).
     It can be used to overwrite the actual result of the method JwIsAccessAllowed to use dynamic access checking (e.g.
     always return true in debug mode)
     See TJwIsAccessAllowed for more information
    }
    property OnIsAccessAllowed : TJwIsAccessAllowed read fIsAccessAllowed write fIsAccessAllowed;

    property GenericMapping: TJwSecurityGenericMappingClass read fGenericMapping write fGenericMapping;

    {Defines the facility code used in the HRESULT value in the faciltiy part.
     By default the value is $FE.

     You can change the value to create your own custom code that is returned in case of errors as HRESULT.
    }
    property FacilityCode : TJwFacilityType read fFacilityCode write fFacilityCode;

    {PersistClassID is returned by GetClassID.
     It defines a registered COM class that provides the streaming methods for this object.
     A NULL_GUID defines that there is no class available. In this case the GetClassID
     returns E_FAIL.

     You can register this class (TJwServerAccessControl) as an official COM object. To do so
     you need to provide the class in a COM dll that is registered properly.
    }
    property PersistClassID : TGUID read fPersistClassID write fPersistClassID;

    {SupportIPersistStream defines whether this object can be stored into and loaded from a stream.
     If this value is false all stream COM methods returns E_NOTIMPL and streaming is not supported.
    }
    property SupportIPersistStream : Boolean read fSupportIPersistStream write fSupportIPersistStream;

    {This flag defines whether the object was changed in respect to the last save method call.
     Default value is false.
     This property is used by the interface method IsDirty
    }
    property Dirty : Boolean read fIsDirty write fIsDirty;

    {PersistStreamType defines how the security descriptor is stored into the stream.
     If this value is true a flat memory model of the security descriptor is directly stored into the stream.
     On the other hand if this value is false the LoadFromStream and SaveToStream of TJwSecurityDescriptor
     will be used.

     Remarks
       if PersistStreamType is true the stream layout is the following:
       <pre>
         [0..3]      size of descriptor
         [4..[size]] realtive security descriptor (as in memory representation)
       </pre>

      Be aware that this stream implementation is propably no compatible with the IAccessControl implementation of MS.
    }
    property PersistStreamType : TJwPersistStreamType read fPersistStreamType write fPersistStreamType;

    {This property applies strict compatibility to the implementation of IAccessControl of MS.
     Default value is false.

     The following methods are made compatible:
      * SetOwner
      ** Returns MAKE_HRESULT(1, FacilityCode, ERROR_INVALID_PARAMETER) if a trustee is not compatible to implementation of IAccessControl.
      ** Owner and Group is still supported (Implementation of IAccessControl does not support owner and group)

      * SetAccessRights
      ** This method fails with
      *** MAKE_HRESULT(1, FacilityCode, ERROR_INVALID_ACCESS) if the Access member is not COM_RIGHTS_EXECUTE or 0.
      *** MAKE_HRESULT(1, FacilityCode, ERROR_INVALID_PARAMETER) if any other value is not conform to IAccessControl implementation.
      *** E_INVALIDARG if pAccessList is not compatible to IAccessControl implementation.

      * GrantAccessRights
      ** Same as SetAccessRights

      * RevokeAccessRights
      ** Trustee check

      * GetAllAccessRights
      **  This method converts the internal security descriptor (TJwSecurityDescriptor) into a compatible version and returns it.
      ** Owner and Group is still supported (Implementation of IAccessControl always returns nil)

      All methods which receives a trustee validates them:
       MAKE_HRESULT(1, FacilityCode, ERROR_INVALID_PARAMETER) if trustee is not conform to IAccessControl implementation.

      Reversed IAccessControl Requirements:
      <pre>
        1. ACTRL_Access.cEntries = 1
        2. ACTRL_Access.pPropertyAccessList <> nil
        3. ACTRL_PROPERTY_ENTRY.lpProperty = nil
        4. ACTRL_PROPERTY_ENTRY.fListFlags = 0
        5. ACTRL_ACCESS_ENTRY_LIST.pAccessEntryList <> nil

        6. (ACTRL_ACCESS_ENTRY_LIST.cEntries <> 0) or
           (ACTRL_ACCESS_ENTRY_LIST.pAccessList <> nil)

        7. ACTRL_ACCESS_ENTRY.Access = 1 or 0
        8. ACTRL_ACCESS_ENTRY.ProvSpecificAccess = 0
        9. ACTRL_ACCESS_ENTRY.Inheritance = 0
        10. ACTRL_ACCESS_ENTRY.lpInheritProperty = 0
        11. ACTRL_ACCESS_ENTRY.fAccessFlags = 1 or 2

        12. TRUSTEE.pMultipleTrustee = nil
        13. TRUSTEE.MultipleTrusteeOperation = 0
        14. TRUSTEE.TrusteeForm = 0 or 1
        15. TRUSTEE.TrusteeType = 1 or 2
        16  TRUSTEE.ptstrName <> nil
      </pre>
    }
    property StrictACLVerify : Boolean read fStrictACLVerify write fStrictACLVerify;
  end;

  {TJwCOMSecuritySettings is used by JwTightCOMSecuritySettings
  to set default COM security settings.
  }
  TJwCOMSecuritySettings = record
    {A security descriptor string in the SDDL format}
    SDDL : TJwString;
    AuthenticationLevel: TJwComAuthenticationLevel;
    ImpersonationLevel: TJwComImpersonationLevel;
    Capabilities : TJwComAuthenticationCapabilities;
  end;

var
  JwKnownComHostProcesses : array[1..1] of TJwString =
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

const
  {Used by TJwComProcessSecurity.CreateTightServerSecurityOptions
   to create a default SD class using SDDL from JwTightCOMSecuritySettings.
  }
  JwDefaultComSD = TJwSecurityDescriptor(-1);





const
  {Used by TJwComProcessSecurity.CreateTightServerSecurityOptions
   to set a tight security setting for a COM server.

   Allows SYSTEM and LocalAdministrator full access to the server.

   http://alt.pluralsight.com/wiki/default.aspx/Keith.GuideBook/WhatIsCoInitializeSecurity.html

   Owner: Local Administrator
   Group: Builtin Administrators
   DACL:
      1.
      ACE-Type: Allow
      AccessMask : 0x1FFFFFF (0000 000 1 11111111 1111111111111111)
      SID: Local System
      2.
      ACE-Type: Allow
      AccessMask : 0x1FFFFFF (0000 000 1 11111111 1111111111111111)
      SID: Builtin Administrators

   acDisableActivateAsActivator
     Do not start the server as the activator so it will be spawn into the caller's logon session.
   acNoCustomMarshal
     Do not load unnecessary DLLs.
   acDynamicCloaking
     Use thread token on outgoing COM calls.
  }
  JwTightCOMSecuritySettings : TJwCOMSecuritySettings = (
    SDDL: 'O:LAG:BAD:(A;;0x1FFFFFF;;;SY)(A;;0x1FFFFFF;;;BA)';
    AuthenticationLevel: calPktPrivacy;
    ImpersonationLevel: cilIdentify;
    Capabilities : [acDisableActivateAsActivator, acNoCustomMarshal, acDynamicCloaking];
   );


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

class function TJwComClientSecurity.GetAuthenticationServices: TJwAuthenticationServiceInformationArray;
var
  Status : HRESULT;
  Count : Cardinal;
  PackI,
  Packages : PSOLE_AUTHENTICATION_SERVICE;
  I: Integer;
begin
  Status := CoQueryAuthenticationServices(
      Count,//__out  DWORD *pcAuthSvc,
      Packages//__out  SOLE_AUTHENTICATION_SERVICE **asAuthSvc
    );
  if Status <> S_OK then
  begin
    SetLastError(status);
    raise EJwsclWinCallFailedException.CreateFmtEx('CoQueryAuthenticationServices failed with result %d.',
        'GetGlobalAuthenticationServices', ClassName, 'JwsclComSecurity.pas', 0, Status, [Status]);
  end;

  try
    SetLength(result, Count);
    PackI := Packages;

    for I := 0 to Count - 1 do
    begin
      result[i].AuthenticationService := TJwComAuthenticationService(PackI.dwAuthnSvc);
      result[i].AuthorizationService := TJwComAuthorizationService(PackI.dwAuthzSvc);
      result[i].PrincipalName := TJwString(PWideChar(PackI.pPrincipalName));
      result[i].Result := PackI.hr;

      Inc(PackI);
    end;
  finally
    CoTaskMemFree(Packages);
  end;
end;

class procedure TJwComClientSecurity.CoQueryProxyBlanket({__in}       pProxy : IUnknown;
        {__out_opt}  out pwAuthnSvc : TJwComAuthenticationService;
        {__out_opt}  out pAuthzSvc : TJwComAuthorizationService;
        {__out_opt}  out pServerPrincName : TJwString;
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

  pServerPrincName := TJwString(WideString(S));
  CoTaskMemFree(S);

  pCapabilites := TJwEnumMap.ConvertComAuthenticationCapabilities(iCapabilites);
end;

class procedure TJwComClientSecurity.CoSetProxyBlanket(
        {__in}      pProxy : IUnknown;
        {__in}      dwAuthnSvc : TJwComAuthenticationService;
        {__in}      dwAuthzSvc : TJwComAuthorizationService;
        {__in_opt}  pServerPrincName : TJwString;
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

procedure TJwComServerSecurity.AccessCheck(const DesiredAccess: DWORD; const SecurityDescriptor: TJwSecurityDescriptor;
  const GenericMapping: TJwSecurityGenericMappingClass; out AccessGranted: Boolean; out GrantedAccessMask: DWORD);
var
  CacheResult: TAuthZAccessCheckResultHandle;
begin
  CacheResult := INVALID_HANDLE_VALUE;
  Self.AccessCheckCached(CacheResult, DesiredAccess, SecurityDescriptor, GenericMapping, AccessGranted, GrantedAccessMask);
end;

procedure TJwComServerSecurity.AccessCheckCached(var CacheResult: TAuthZAccessCheckResultHandle; const DesiredAccess: DWORD;
  const SecurityDescriptor: TJwSecurityDescriptor; const GenericMapping: TJwSecurityGenericMappingClass; out AccessGranted: Boolean;
  out GrantedAccessMask: DWORD);

var
  Request : TJwAuthZAccessRequest;
  Reply : TJwAuthZAccessReply;
begin
  if ImpersonationLevel < cilIdentify then
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
      fAuthContext.AccessCheck(0, Request, 0, SecurityDescriptor, nil, GenericMapping, Reply, nil);
    end
    else
    begin
      fAuthContext.AccessCheckCached(CacheResult, 0, Request, 0, Reply);
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
        {__out_opt}  out pServerPrincName : TJwString;
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

  pServerPrincName := TJwString(WideString(S));
  CoTaskMemFree(S);
end;

constructor TJwComServerSecurity.Create(const ImpersonationType: TJwServerImpersonationType);
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

  fImpersonationType := ImpersonationType;;

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
        @pServerPrincipalName,//{__out_opt}  out pServerPrincName : TJwString;
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

  fServerPrincipalName := TJwString(pServerPrincipalName);
  CoTaskMemFree(pServerPrincipalName);
  *)

  CoQueryClientBlanket(
        fAuthenticationService,//{__out_opt}  out pwAuthnSvc : DWORD;
        fAuthorizationService,//{__out_opt}  out pAuthzSvc : DWORD;
        fServerPrincipalName,//{__out_opt}  out pServerPrincName : TJwString;
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

  case fImpersonationType of
    //impersonate on creation, revert on destroying
    sitAutoImpersonation,
    //impersonate on creation, but do not revert
    sitOnlyImpersonation : ImpersonateClient;
    //do not impersonate at all
    sitNoImpersonation : ;
  end;
end;

destructor TJwComServerSecurity.Destroy;
begin
  FreeAndNil(fToken);
  FreeAndNil(fWinNTIdentity);

  FreeAndNil(fAuthManager);
  FreeAndNil(fAuthContext);


  case fImpersonationType of
    //impersonate on creation, revert on destroying
    sitAutoImpersonation : RevertToSelf;
    //impersonate on creation, but do not revert
    sitOnlyImpersonation : ;
    //do not impersonate at all
    sitNoImpersonation : ;
  end;

  inherited;
end;


function TJwComServerSecurity.GetAuthContext: TJwAuthContext;
begin
  result := fAuthContext;
end;

function TJwComServerSecurity.GetAuthManager: TJwAuthResourceManager;
begin
  result := fAuthManager;
end;

class function TJwComServerSecurity.GetServerSecurity(const ImpersonationType: TJwServerImpersonationType): IJwComServerSecurity;
begin
  result := Create(ImpersonationType);
end;

function TJwComServerSecurity.GetToken: TJwSecurityToken;
begin
  if ImpersonationLevel < cilIdentify then
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

function TJwComServerSecurity.GetUserName: TJwString;
var
  Token : TJwSecurityToken;
  IsImpersonating : Boolean;
begin
  if ImpersonationLevel < cilIdentify then
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

function TJwComServerSecurity.GetWinNTIdentity: TJwComWinNTIdentity;
begin
  result := fWinNTIdentity;
end;

procedure TJwComServerSecurity.ImpersonateClient;
var
  hr : HRESULT;
begin
  hr := CoImpersonateClient();

  if Failed(hr) then
  begin
    raise EJwsclWinCallFailedException.CreateFmtEx('', 'ImpersonateClient', ClassName, 'JwsclComSecurity.pas', 0, ResultCode(hr), []);
  end;
end;

function TJwComServerSecurity.IsImpersonating: Boolean;
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

procedure TJwComServerSecurity.RevertToSelf;
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

class function TJwComRegistrySecurity.GetGlobalAuthenticationServices: TJwSecurityPackageInformationArray;
var
  status : SECURITY_STATUS;
  Count : Cardinal;
  PackI,
  Packages : {$IFDEF UNICODE}PSecPkgInfoW;{$ELSE}PSecPkgInfoA;{$ENDIF}
  I: Integer;
begin
{$IFDEF UNICODE}
  status := EnumerateSecurityPackagesW(
{$ELSE}
  status := EnumerateSecurityPackagesA(
{$ENDIF}
    Count,//__in  PULONG pcPackages,
    Packages//__in  PSecPkgInfo *ppPackageInfo
    );
  if status <> SEC_E_OK then
  begin
    SetLastError(status);
    raise EJwsclWinCallFailedException.CreateFmtEx('EnumerateSecurityPackages failed with result %d.',
        'GetGlobalAuthenticationServices', ClassName, 'JwsclComSecurity.pas', 0, status, [status]);
  end;

  try
    SetLength(result, Count);
    PackI := Packages;

    for I := 0 to Count - 1 do
    begin
      result[i].Capabilities := TJwEnumMap.ConvertSecurityPackageCapabilities(PackI.fCapabilities);
      result[i].Version := PackI.wVersion;
      result[i].RPCID := PackI.wRPCID;
      result[i].MaxToken := PackI.cbMaxToken;
      result[i].Name := TJwString(TJwPChar(PackI.Name));
      result[i].Comment := TJwString(TJwPChar(PackI.Comment));

      Inc(PackI);
    end;
  finally
    FreeContextBuffer(Packages);
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

function TJwComRegistrySecurity.GetROTFlags: TJwComRotFlags;
var Value : DWORD;
begin
  if Reg.ValueExists('ROTFlags') then
  begin
    Value := Reg.ReadInteger('ROTFlags');
    result := TJwEnumMap.ConvertComRotFlags(Value);
  end
  else
  begin
    result := [];
  end;
end;

function TJwComRegistrySecurity.GetRunAs: TJwSecurityID;
var Value : TJwString;
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

function TJwComRegistrySecurity.GetRunAsString: TJwString;
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

function TJwComRegistrySecurity.GetServiceParameters: TJwString;
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
  Value : TJwString;
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

class function TJwComRegistrySecurity.ReadSD(Reg : TRegistry; const KeyName: TJwString; var Default : TJwSecurityDescriptor): TJwSecurityDescriptor;
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

class procedure TJwComRegistrySecurity.WriteSD(Reg: TRegistry; const KeyName: TJwString; const SD: TJwSecurityDescriptor; DoDelete: Boolean);
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

procedure TJwComRegistrySecurity.SetROTFlags(const Value: TJwComRotFlags);
begin
  CheckReadonly('ROTFlags');

  if (Value = []) or ([rfNone] = Value) then
  begin
    Reg.DeleteValue('ROTFlags');
  end
  else
  begin
    Reg.WriteInteger('ROTFlags', TJwEnumMap.ConvertComRotFlags(Value));
  end;
end;


procedure TJwComRegistrySecurity.SetRunAs(const Value: TJwSecurityID);
var Domain, Name : TJwString;
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

procedure TJwComRegistrySecurity.SetRunAsString(const Value: TJwString);
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

procedure TJwComRegistrySecurity.SetServiceParameters(const Value: TJwString);
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

procedure TJwComCustomSecurity.SetServerPrincipalName(const Value: TJwString);
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

function TJwComCustomSecurity.GetCapabilites: TJwComAuthenticationCapabilities;
begin
  result := fCapabilites;
end;

function TJwComCustomSecurity.GetImpersonationLevel: TJwComImpersonationLevel;
begin
  result := fImpersonationLevel;
end;

function TJwComCustomSecurity.GetReadOnlyProperties: Boolean;
begin
  result := fReadOnly;
end;

function TJwComCustomSecurity.GetServerPrincipalName: TJwString;
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
            Auth.AccessCheck(0, Request, 0, SecurityDescriptor, nil, nil, Reply, nil);

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



class procedure TJwComProcessSecurity.CreateTightServerSecurityOptions(var SecurityDescriptor: TJwSecurityDescriptor;
  out AuthenticationLevel: TJwComAuthenticationLevel; out ImpersonationLevel: TJwComImpersonationLevel;
  out Capabilities: TJwComAuthenticationCapabilities);
begin
  if (SecurityDescriptor = JwDefaultComSD) then
  begin
    SecurityDescriptor := TJwSecurityDescriptor.Create(JwTightCOMSecuritySettings.SDDL);
    ShowMessage(SecurityDescriptor.Text);
  end;

  AuthenticationLevel := JwTightCOMSecuritySettings.AuthenticationLevel;
  ImpersonationLevel := JwTightCOMSecuritySettings.ImpersonationLevel;
  Capabilities := JwTightCOMSecuritySettings.Capabilities;
end;

class procedure TJwComProcessSecurity.Initialize(
  const Domain, UserName, Password: TJwString;
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

class procedure TJwComProcessSecurity.Initialize(AppID: TGUID; Capabilities: TJwComAuthenticationCapabilities);
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
          calNone,//AuthenticationLevel: TJwComAuthenticationLevel;
          cilIdentify,//ImpersonationLevel : TJwComImpersonationLevel;
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
    Domain := TJwString(WideString(PAnsiChar(Identity.Domain)));
    PackageList := TJwString(WideString(PAnsiChar(Identity.PackageList)));
    Password := TJwString(WideString(PAnsiChar(Identity.Password)));
    User := TJwString(WideString(PAnsiChar(Identity.User)));
  end
  else
  begin
    Domain := TJwString(WideString(Identity.Domain));
    PackageList := TJwString(WideString(Identity.PackageList));
    Password := TJwString(WideString(Identity.Password));
    User := TJwString(WideString(Identity.User));
  end;
end;

constructor TJwComWinNTIdentity.Create(const Identity: TSecWinNTAuthIdentityW; ReadOnly: Boolean);
begin
  ZeroMemory(@fDataEx, SizeOf(fDataEx));
  fDataEx.Flags := Identity.Flags;

  if fDataEx.Flags and SEC_WINNT_AUTH_IDENTITY_ANSI <> 0 then
  begin
    Domain := TJwString(WideString(PAnsiChar(Identity.Domain)));
    Password := TJwString(WideString(PAnsiChar(Identity.Password)));
    User := TJwString(WideString(PAnsiChar(Identity.User)));
  end
  else
  begin
    Domain := TJwString(WideString(Identity.Domain));
    Password := TJwString(WideString(Identity.Password));
    User := TJwString(WideString(Identity.User));
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

function TJwComWinNTIdentity.GetDomain: TJwString;
begin
  result := TJwString(WideString(fDataEx.Domain));
end;

function TJwComWinNTIdentity.GetFlags: DWORD;
begin
  result := fDataEx.Flags;
end;

function TJwComWinNTIdentity.GetPackageList: TJwString;
begin
  result := TJwString(WideString(fDataEx.PackageList));
end;

function TJwComWinNTIdentity.GetPassword: TJwString;
begin
  result := TJwString(WideString(fDataEx.Password));
end;

function TJwComWinNTIdentity.GetUser: TJwString;
begin
  result := TJwString(WideString(fDataEx.User));
end;

function TJwComWinNTIdentity.GetVersion: DWORD;
begin
  result := fDataEx.Version;
end;

procedure SetWideChar(var pch : PWideChar; out chLen : DWORD; S : WideString);
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


procedure TJwComWinNTIdentity.SetDomain(const Value: TJwString);
begin
  CheckReadonly('Domain');
  SetWideChar(fDataEx.Domain, fDataEx.DomainLength, WideString(Value));
end;

procedure TJwComWinNTIdentity.SetFlags(const Value: DWORD);
begin
  CheckReadonly('Flags');
  fDataEx.Flags := Value;
end;

procedure TJwComWinNTIdentity.SetPackageList(const Value: TJwString);
begin
  CheckReadonly('PackageList');
  SetWideChar(fDataEx.PackageList, fDataEx.PackageListLength, WideString(Value))
end;

procedure TJwComWinNTIdentity.SetPassword(const Value: TJwString);
begin
  CheckReadonly('Password');
  SetWideChar(fDataEx.Password, fDataEx.PasswordLength, WideString(Value))
end;

procedure TJwComWinNTIdentity.SetUser(const Value: TJwString);
begin
  CheckReadonly('User');
  SetWideChar(fDataEx.User, fDataEx.UserLength, WideString(Value))
end;

procedure TJwComWinNTIdentity.SetVersion(const Value: DWORD);
begin
  CheckReadonly('Version');
  fDataEx.Version := Version;
end;

constructor TJwComWinNTIdentity.Create(const Domain, UserName, Password: TJwString);
begin
  Create;

  Self.User := UserName;
  Self.Domain := Domain;
  Self.Password := Password;
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

constructor TJwAuthenticationInfo.CreateWinNT(const UserName, Domain, Password: TJwString; const AuthorizationService: TJwComAuthorizationService);
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



{ TJwServerAccessControl }

constructor TJwServerAccessControl.Create(const SD: TJwSecurityDescriptor);
begin
  Create;

  fSD.Assign(SD);
end;


class function TJwServerAccessControl.CreateCOMImplementation: IAccessControl;
begin
  OleCheck(HelperCreateAccessControl(Result));
end;

constructor TJwServerAccessControl.Create;
begin
  inherited;

  JwCheckInitKnownSid([JwSecurityProcessUserSID], ['JwSecurityProcessUserSID'], 'Create', ClassName, 'JwsclComSecurity.pas');

  FacilityCode := $FE;
  fAuthManager := TJwAuthResourceManager.Create('', [], nil, nil);
  fGenericMapping := TJwSecurityGenericMapping;
  fCacheResult := 0;

  fSD := TJwSecurityDescriptor.Create();
  fSD.OwnDACL := true;

  fSD.OwnOwner := true;
  fSD.OwnPrimaryGroup := true;

  //true means, a private copy is made from these class instances
  fSD.Owner := JwSecurityProcessUserSID; //Standard SIDs are never freed
  fSD.PrimaryGroup := JwNullSID;

  fSupportIPersistStream := True;
  fIsDirty := False;
  fStrictACLVerify := true;

  fPersistClassID := NULL_GUID;
  fCacheResult := 0;

  fCriticalSection := TCriticalSection.Create;
end;

destructor TJwServerAccessControl.Destroy;
begin

  FreeAndNil(fSD);
  FreeAndNil(fAuthManager);
  FreeAndNil(fCriticalSection);
  inherited;
end;

class procedure TJwServerAccessControl.FreeAccessRights(var AccessList: PACTRL_ACCESSW_ALLOCATE_ALL_NODES; var Owner, Group: PTrusteeW);
begin
  CoTaskMemFree(AccessList);
  CoTaskMemFree(Owner);
  CoTaskMemFree(Group);
end;



procedure TJwServerAccessControl.JwGetAllAccessRights(const AProperty: TJwString; out AccessList: TJwDAccessControlList; out Owner,
  Group: TJwSecurityID);
begin
  if Assigned(fSD.DACL) then
  begin
    AccessList := TJwDAccessControlList.Create;
    AccessList.Assign(fSD.DACL);
  end
  else
    AccessList := nil;

  if Assigned(fSD.Owner) then
    Owner := TJwSecurityId.Create(fSD.Owner)
  else
    Owner := nil;

  if Assigned(fSD.PrimaryGroup) then
    Group := TJwSecurityId.Create(fSD.PrimaryGroup)
  else
    Group := nil;
end;

procedure TJwServerAccessControl.JwGrantAccessRights(const AccessList: TJwDAccessControlList);
var
  DenyEntryCount,
  NewDenyCount : Integer;

  I: Integer;
  ACE : TJwSecurityAccessControlEntry;
begin
  if Assigned(AccessList) then
  begin
    if not Assigned(fSD.DACL) then
    begin
      fSD.DACL := TJwDAccessControlList.Create();
      fSD.DACL.Assign(AccessList);
    end
    else
    begin
      {
      http://msdn.microsoft.com/en-us/library/ms694481%28VS.85%29.aspx
      Following a merge, the access rights on an object are ordered as follows:

       1. [New Access Denied]
       2. [Old Access Denied]
       3. [New Access Allowed]
       4. [Old Access Allowed]

      }
      DenyEntryCount := 0;
      for I := 0 to fSD.DACL.Count - 1 do
      begin
        if fSD.DACL[i] is TJwDiscretionaryAccessControlEntryDeny then
          Inc(DenyEntryCount);
      end;

      NewDenyCount := 0;
      for I := 0 to AccessList.Count - 1 do
      begin
        ACE := AccessList[i].ClassType.Create as TJwSecurityAccessControlEntry;
        ACE.Assign(AccessList[i]);

        if AccessList[i] is TJwDiscretionaryAccessControlEntryDeny then
        begin
          fSD.DACL.Insert(NewDenyCount, ACE);

          Inc(DenyEntryCount);
          Inc(NewDenyCount);
        end
        else
        if AccessList[i] is TJwDiscretionaryAccessControlEntryAllow then
        begin

          //add allow ACE in original order of AccessList directly behind the deny entries
          fSD.DACL.Insert(DenyEntryCount, ACE);
          Inc(DenyEntryCount); //keeps original order of AccessList
        end;
      end;
    end;
  end;
end;

procedure TJwServerAccessControl.JwIsAccessAllowed(const Trustee: TJwSecurityID; const AProperty: TJwString;
  const AccessRights: TJwAccessMask;
  var AccessAllowed: Boolean);
var
  AuthContext : TJwAuthContext;
  Request : TJwAuthZAccessRequest;
  Reply : TJwAuthZAccessReply;
  ErrorCode : HRESULT;
begin
  AccessAllowed := false;

  if not Assigned(Trustee) then
    Exit;

  AuthContext := TJwAuthContext.CreateBySid(fAuthManager, [authZSF_ComputePrivileges], Trustee, 0, nil);

  try
    Request := TJwAuthZAccessRequest.Create(AccessRights,
              JwNullSID, nil, nil, shOwned);

    Reply := nil;
    try
      AuthContext.AccessCheck(0, Request, 0, SecurityDescriptor, nil, GenericMapping, Reply, nil);


      AccessAllowed := Reply.ErrorByType[0] = reSuccess;

      //Use another way to do access check
      //This is more dynamic
      if Assigned(OnIsAccessAllowed) then
      begin
        ErrorCode := 0;
        OnIsAccessAllowed(
          Self,//const Sender : TJwServerAccessControl;
          AccessRights,//const DesiredAccess : DWORD;
          Trustee.GetCachedUserFromSid, //const TrusteeName : TJwString;
          Trustee, // const TrusteeSID : TJwSecurityId;
          AProperty,//const AProperty : TJwString;
          nil,  //const Trustee : PTrusteeW;
          AccessAllowed, //var AccessGranted : Boolean;
          ErrorCode //var ErrorCode : HRESULT) of object;
        );
        if Failed(ErrorCode) then
          raise EOleSysError.Create(Format('Method assigned to OnIsAccessAllowed failed with %d',[ErrorCode]), ErrorCode, 0);

      end;
    finally

      FreeAndNil(Reply);
      FreeAndNil(Request);

    end;
  finally
    FreeAndNil(AuthContext);
  end;
end;

procedure TJwServerAccessControl.JwRevokeAccessRights(const AProperty: TJwString; const SecurityIDList: TJwSecurityIdList);
var
  I, Idx: Integer;
begin
  if not Assigned(SecurityIDList) or
    not Assigned(fSD.DACL) then
    Exit;

  for I := 0 to SecurityIDList.Count - 1 do
  begin
    repeat
      Idx := fSD.DACL.FindSID(SecurityIDList[i]);

      if Idx >= 0 then
      begin
        fSD.DACL.Delete(Idx);
      end;
    until Idx < 0;
  end;
end;

procedure TJwServerAccessControl.JwSetAccessRights(const AccessList: TJwDAccessControlList);
begin
  if Assigned(AccessList) then
  begin
    fSD.DACL.Assign(AccessList);
  end
  else
  begin
    fSD.DACL := nil;
  end;
end;

procedure TJwServerAccessControl.JwSetOwner(const Owner, Group: TJwSecurityID);
begin
  if Assigned(Owner) then
    fSD.Owner := Owner;

  if Assigned(Group) then
    fSD.PrimaryGroup := Group;
end;

type
  TPtrPointer = record
    //Pointer to beginning of data
    StartPtr,
    //Current pointer in data
    OffsetPtr : Pointer;
    //Size of available allocated memory
    Size : Cardinal;
  end;

//create a memory block
function InitPtrData(var Target : TPtrPointer; const Size : Cardinal; const Offset : Cardinal = 0) : Pointer;
begin
  Target.StartPtr := CoTaskMemAlloc(Size);
  Target.OffsetPtr := Pointer(DWORD_PTR(Target.StartPtr) + Offset);
  Target.Size := Size;
  result := Target.StartPtr;
end;

//copies data into a memory block
function SetPtrData(var Target : TPtrPointer; const Source : Pointer; const Size : Cardinal) : Pointer;
begin
  Assert((DWORD_PTR(Target.OffsetPtr) - DWORD_PTR(Target.StartPtr)) <=  Target.Size, 'Memory block too small');

  result := Target.OffsetPtr;
  Target.OffsetPtr := Pointer(DWORD_PTR(Target.OffsetPtr) + Size);

  CopyMemory(result, Source, Size);
end;




function TJwServerAccessControl.GetAllAccessRights(lpProperty: LPWSTR; var ppAccessList: PACTRL_ACCESSW_ALLOCATE_ALL_NODES; var ppOwner,
  ppGroup: PTRUSTEEW): HRESULT;
var
  ACL : TJwDAccessControlList;
  Owner, Group : TJwSecurityId;
  SidLength,
  PropertyCount,
  I: Integer;
  PtrPtr : TPtrPointer;

  PropertyEntry : ACTRL_PROPERTY_ENTRYW;
  AccessEntryList : ACTRL_ACCESS_ENTRY_LISTW;
  AccessEntry : ACTRL_ACCESS_ENTRY;

  AccessEntryArray : array of PACTRL_ACCESS_ENTRYW;
  SID : PSid;
begin
  try
    fCriticalSection.Enter;
    try
      result := S_OK;

      //Retrieve data
      JwGetAllAccessRights(TJwString(lpProperty), ACL, Owner, Group);

      try
        if Assigned(Owner) then
        begin
          ppOwner := InitPtrData(PtrPtr, sizeof(TRUSTEEW) + Owner.SIDLength, sizeof(TRUSTEEW));
          ppOwner.MultipleTrusteeOperation := NO_MULTIPLE_TRUSTEE;
          ppOwner.TrusteeForm := TRUSTEE_IS_SID;
          ppOwner.TrusteeType := TRUSTEE_IS_USER;
          ppOwner.ptstrName := PWideChar(SetPtrData(PtrPtr, Owner.SID, Owner.SIDLength));
        end
        else
          ppOwner := nil;

        if Assigned(Group) then
        begin
          ppGroup := InitPtrData(PtrPtr, sizeof(TRUSTEEW) + Group.SIDLength, sizeof(TRUSTEEW));
          ppGroup.MultipleTrusteeOperation := NO_MULTIPLE_TRUSTEE;
          ppGroup.TrusteeForm := TRUSTEE_IS_SID;
          ppGroup.TrusteeType := TRUSTEE_IS_GROUP;
          ppGroup.ptstrName := PWideChar(SetPtrData(PtrPtr, Group.SID, Group.SIDLength));
        end
        else
          ppGroup := nil;

        //grant everyone access
        if not Assigned(ACL) then
        begin
          if StrictACLVerify then
            PropertyCount := 1 //IAccessControl does only support 1. Do not modify!
          else
            PropertyCount := 1; //we don't support properties

          //Create structure with CoTaskMemAlloc
          ppAccessList :=
            InitPtrData(PtrPtr, $FF + //security space
              (sizeof(ACTRL_PROPERTY_ENTRYW) + SizeOf(ACTRL_ACCESS_ENTRY_LIST)) * PropertyCount //Number of properties
              , sizeof(_ACTRL_ALISTW{=PACTRL_ACCESSW_ALLOCATE_ALL_NODES^} //add this offset so the next data is stored behind the ppAccessList in the memory block
            ));

          ppAccessList.cEntries := PropertyCount;

           //Init the property entry list - only one property list (no property) is supported
          ZeroMemory(@PropertyEntry, sizeof(PropertyEntry));
          ppAccessList.pPropertyAccessList :=
            SetPtrData(PtrPtr, @PropertyEntry, sizeof(PropertyEntry));


        end
        else
        begin
          SidLength := 0;
          //Check for supported access control entries
          //and determine sum of all SID structures
          for I := 0 to ACL.Count - 1 do
          begin
            if ACL.Items[I] is TJwDiscretionaryAccessControlEntryAllow then
            else
            if ACL.Items[I] is TJwDiscretionaryAccessControlEntryDeny then
            else
            begin
              raise EOleSysError.Create(
               'The ACL must only contain Allow and Deny ACEs.',
                MAKE_HRESULT(1, FacilityCode, ERROR_INVALID_LIST_FORMAT), 0);
            end;
            Inc(SidLength, ACL.Items[I].SID.SIDLength);
          end;

          PropertyCount := 1; //we don't support properties

          //Create structure with CoTaskMemAlloc
          ppAccessList :=
            InitPtrData(PtrPtr, $FF + //security space
              (sizeof(ACTRL_PROPERTY_ENTRYW) + SizeOf(ACTRL_ACCESS_ENTRY_LIST)) * PropertyCount +  //Number of properties
              sizeof(ACTRL_ACCESS_ENTRYW) * ACL.Count  //Number of Trustees (and therefore entries)
              + SidLength //the sids size
              , sizeof(_ACTRL_ALISTW{=PACTRL_ACCESSW_ALLOCATE_ALL_NODES^} //add this offset so the next data is stored behind the ppAccessList in the memory block
            ));

          ppAccessList.cEntries := PropertyCount;

          //Init the property entry list - only one property list (no property) is supported
          ZeroMemory(@PropertyEntry, sizeof(PropertyEntry));
          ppAccessList.pPropertyAccessList :=
            SetPtrData(PtrPtr, @PropertyEntry, sizeof(PropertyEntry));

          //this is the ACL structure
          ZeroMemory(@AccessEntryList, sizeof(AccessEntryList));
          AccessEntryList.cEntries := ACL.Count;

          //create the ACL memory and put it into the first (and only) property list.
          ppAccessList.pPropertyAccessList.pAccessEntryList :=
            SetPtrData(PtrPtr, @AccessEntryList, sizeof(AccessEntryList));

          //first, setup all ACTRL_ACCESS_ENTRY structures into the memory block returned
          //because the entries are allocated in a single line they are accessible as an array
          SetLength(AccessEntryArray, ACL.Count);
          for I := 0 to ACL.Count - 1 do
          begin
            ZeroMemory(@AccessEntry, sizeof(AccessEntry));

            //store the pointer to each array member for later usage
            AccessEntryArray[I] :=
              SetPtrData(PtrPtr, @AccessEntry, sizeof(AccessEntry));
            if i = 0 then //store the first array item
              ppAccessList.pPropertyAccessList.pAccessEntryList.pAccessList := AccessEntryArray[0];
          end;

          //now init the access entries using the temp array AccessEntryArray
          for I := Low(AccessEntryArray) to High(AccessEntryArray) do
          begin
            if ACL.Items[I] is TJwDiscretionaryAccessControlEntryAllow then
               AccessEntryArray[I].fAccessFlags := ACTRL_ACCESS_ALLOWED
            else
            if ACL.Items[I] is TJwDiscretionaryAccessControlEntryDeny then
               AccessEntryArray[I].fAccessFlags := ACTRL_ACCESS_DENIED;
            //other ACE types are not supported (this is checked in the beginning)

            //Convert the ACL access mask to IAccessControl compatible value
            if StrictACLVerify then
            begin
              //IAccessControl only allows bit 0 to be used (set by COM_RIGHTS_EXECUTE).
              AccessEntryArray[I].Access := ACL.Items[I].AccessMask and COM_RIGHTS_EXECUTE;
            end
            else
            begin
              //we access the access entries through the temp array
              AccessEntryArray[I].Access := ACL.Items[I].AccessMask;
            end;
            AccessEntryArray[I].Inheritance := NO_INHERITANCE;

            ZeroMemory(@AccessEntryArray[I].Trustee, sizeof(AccessEntryArray[I].Trustee));
            AccessEntryArray[I].Trustee.MultipleTrusteeOperation := NO_MULTIPLE_TRUSTEE;
            AccessEntryArray[I].Trustee.TrusteeForm := TRUSTEE_IS_SID;
            AccessEntryArray[I].Trustee.TrusteeType := TRUSTEE_IS_USER;

            //Defined by MS IAccessControl
            //AccessEntryArray[I].ProvSpecificAccess := 0;
            //AccessEntryArray[I].Inheritance := NO_INHERITANCE;
            //AccessEntryArray[I].lpInheritProperty := nil;



            //store the SID structures into the memory block
            SID :=
              SetPtrData(PtrPtr, ACL[i].SID.SID, ACL[i].SID.SIDLength);

            AccessEntryArray[I].Trustee.ptstrName := PWideChar(SID); //and set the value
          end;

        end;
      finally
        FreeAndNil(ACL);
        FreeAndNil(Owner);
        FreeAndNil(Group);
      end;
    finally
      fCriticalSection.Leave;
    end;
  except
    on E : EOleSysError do
    begin
  {$IFDEF DEBUG}
      raise;
  {$ELSE}
      Result := E.ErrorCode;
  {$ENDIF DEBUG}
    end;
    on e : Exception do
    begin
  {$IFDEF DEBUG}
      raise;
  {$ELSE}
      result := E_UNEXPECTED;
  {$ENDIF DEBUG}
    end;
  end;
end;

function TJwServerAccessControl.GetClassID(out classID: TCLSID): HRESULT;
begin
  if CompareMem(@NULL_GUID, @fPersistClassID, sizeof(TGUID)) then
  begin
    classID := NULL_GUID;
    result := E_FAIL;
  end
  else
  begin
    classID := fPersistClassID;
    result := S_OK;
  end;
end;




procedure TJwServerAccessControl.Clear;
begin
  fCriticalSection.Enter;

  try
    if Assigned(fSD.DACL) then
    begin
      fSD.DACL.Clear;
    end
    else
    begin
      fSD.DACL := TJwDAccessControlList.Create;
      fSD.DACL.OwnsObjects := true;
    end;

    fSD.Owner := JwNullSID;
    fSD.PrimaryGroup := JwNullSID;

    Dirty := False;

  finally
    fCriticalSection.Leave;
  end;
end;

procedure TJwServerAccessControl.CopyACLToObject(const pAccessList: PACTRL_ACCESSW; var ACL : TJwDAccessControlList);
var
  P : PACTRL_PROPERTY_ENTRYW;
  I, I2: Integer;
  pACE : PACTRL_ACCESS_ENTRYW;
  ACEClass : TJwSecurityAccessControlEntryClass;
  ACE : TJwSecurityAccessControlEntry;
  SID : TJwSecurityId;
begin
  P := pAccessList.pPropertyAccessList;

  if P.pAccessEntryList = nil then //grant access to everybody
  begin
    FreeAndNil(ACL);
    exit;
  end;

  for I := 0 to pAccessList.cEntries - 1 do
  begin
    pACE := P.pAccessEntryList.pAccessList;

    if pACE = nil then
      break;

    for i2 := 0 to P.pAccessEntryList.cEntries - 1 do
    begin
      if StrictACLVerify then
      begin
        if pACE.Access and $FFFFFFFE <> 0 then
        begin
          raise EOleSysError.Create(
               Format('The member Access of AccessEntry (PACTRL_ACCESS_ENTRYW) #%d is not conform to implementation of IAccessControl. Only COM_RIGHTS_EXECUTE is valid.', [i]),
                MAKE_HRESULT(1, FacilityCode, ERROR_INVALID_ACCESS), 0);
        end;

        if not IsValidTrustee(@pACE.Trustee) then
        begin
          begin
            raise EOleSysError.Create(Format('The member Trustee of AccessEntry (PACTRL_ACCESS_ENTRYW) #%d is not compatible/valid to IAccessControl. ', [i]),
               MAKE_HRESULT(1, FacilityCode, ERROR_INVALID_PARAMETER), 0);
          end;
        end;
      end;

      case pACE.fAccessFlags of
        ACTRL_ACCESS_ALLOWED:
           ACEClass := TJwDiscretionaryAccessControlEntryAllow;
        ACTRL_ACCESS_DENIED:
          ACEClass := TJwDiscretionaryAccessControlEntryDeny;
      else
        //ACTRL_AUDIT_SUCCESS,
        //ACTRL_AUDIT_FAILURE :
          ACEClass := nil;
      end;

      if Assigned(ACEClass) then
      begin
        SID := TrusteeToSid(@pACE.Trustee);

        if Assigned(SID) then
        begin
          ACE := ACEClass.Create(nil, TJwEnumMap.ConvertAceFlags(pACE.Inheritance), pACE.Access, SID, True);
          ACE.InheritProperty := TJwString(pACE.lpInheritProperty);

          ACL.Add(ACE);
        end;
      end;

      Inc(pACE);
    end;

    Inc(P);
  end;
end;
{

Return values:
  E_INVALIDARG : pAccessList = nil

}
function TJwServerAccessControl.GrantAccessRights(pAccessList: PACTRL_ACCESSW): HRESULT;
var
  ACL : TJwDAccessControlList;
begin
  try
    fCriticalSection.Enter;
    try
      result := S_OK;

      if StrictACLVerify and (
         (pAccessList = nil) or
         (pAccessList.pPropertyAccessList = nil) or
         (pAccessList.cEntries = 0) or
         (pAccessList.pPropertyAccessList.lpProperty <> nil) or
         (pAccessList.pPropertyAccessList.fListFlags <> 0)) then
      begin
        raise EOleSysError.Create('The parameter pAccessList or one of its member is not valid.',
             MAKE_HRESULT(1, FacilityCode, ERROR_INVALID_PARAMETER), 0);
      end;

      if (pAccessList = nil) or (pAccessList.pPropertyAccessList = nil) then
      begin
        result := E_INVALIDARG;
        exit;
      end;

      //grant access to everyone
      if pAccessList.pPropertyAccessList.pAccessEntryList = nil then
      begin
        JwGrantAccessRights(nil);
        fIsDirty := true;
        Result := S_OK;

        exit;
      end;

      ACL := TJwDAccessControlList.Create;
      try
        CopyACLToObject(pAccessList, ACL);

        JwGrantAccessRights(ACL);

        fIsDirty := true;
        Result := S_OK;
      finally
        FreeAndNil(ACL);
      end;
    finally
      fCriticalSection.Leave;
    end;
  except
    on E : EOleSysError do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      Result := E.ErrorCode;
{$ENDIF DEBUG}
    end;
    on e : Exception do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      result := E_UNEXPECTED;
{$ENDIF DEBUG}
    end;
  end;
end;

function TJwServerAccessControl.IsAccessAllowed(pTrustee: PTRUSTEEW; lpProperty: LPWSTR; AccessRights: ACCESS_RIGHTS;
  var pfAccessAllowed: BOOL): HRESULT;
var
  SID : TJwSecurityId;
  b : Boolean;
begin
  try
    fCriticalSection.Enter;
    try
      result := S_OK;

      pfAccessAllowed := false;

      if StrictACLVerify and
         not IsValidTrustee(pTrustee) then
      begin
        begin
          raise EOleSysError.Create('The member parameter pTrustee is not compatible/valid to implementation of IAccessControl. (StrictACLVerify is true)',
             MAKE_HRESULT(1, FacilityCode, ERROR_INVALID_PARAMETER), 0);
        end;
      end;

      if pTrustee = nil then
      begin
        result := E_INVALIDARG;
        exit;
      end;

      SID := TrusteeToSid(pTrustee);
      if not Assigned(SID) then
      begin
        result := MAKE_HRESULT(1, FacilityCode, ERROR_INVALID_SID);
        exit;
      end;

      try
        b := pfAccessAllowed;
        JwIsAccessAllowed(SID, TJwString(lpProperty), AccessRights, b);
        pfAccessAllowed := b;
      finally
        FreeAndNil(SID);
      end;
    finally
      fCriticalSection.Leave;
    end;

  except
    on E : EOleSysError do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      Result := E.ErrorCode;
{$ENDIF DEBUG}
    end;
    on e : Exception do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      result := E_UNEXPECTED;
{$ENDIF DEBUG}
    end;
  end;
end;


function TJwServerAccessControl.RevokeAccessRights(lpProperty: LPWSTR; cTrustees: ULONG; prgTrustees: PTRUSTEEW): HRESULT;
var
  SID : TJwSecurityId;
  SIDs : TJwSecurityIdList;
  I: Integer;
begin
  fCriticalSection.Enter;
  try
    result := S_OK;

    if StrictACLVerify and
       not IsValidTrustee(prgTrustees) then
    begin
      begin
        raise EOleSysError.Create('The member parameter prgTrustees is not compatible/valid to implementation of IAccessControl. ',
           MAKE_HRESULT(1, FacilityCode, ERROR_INVALID_PARAMETER), 0);
      end;
    end;

    if prgTrustees = nil then
    begin
      result := E_INVALIDARG;
      exit;
    end;

    if cTrustees = 0 then
      exit;

    SIDs := TJwSecurityIdList.Create(true);
    try
      try
        for I := 0 to cTrustees - 1 do
        begin
          if StrictACLVerify and not IsValidTrustee(prgTrustees) then
          begin
            raise EOleSysError.Create(Format('The member Trustee of AccessEntry (PACTRL_ACCESS_ENTRYW) #%d is not compatible/valid to IAccessControl. ', [i]),
                 MAKE_HRESULT(1, FacilityCode, ERROR_INVALID_PARAMETER), 0);
          end;

          SID := TrusteeToSid(prgTrustees);

          if Assigned(SID) then
          begin
            SIDs.Add(SID);
          end
          else
          begin
            result := MAKE_HRESULT(1, FacilityCode, ERROR_INVALID_SID);
            exit;
          end;
        end;

        JwRevokeAccessRights(TJwString(lpProperty), SIDs);
        fIsDirty := true;
      finally
        FreeAndNil(SIDs);
      end;
    finally
      fCriticalSection.Leave;
    end;
  except
    on E : EOleSysError do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      Result := E.ErrorCode;
{$ENDIF DEBUG}
    end;
    on e : Exception do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      result := E_UNEXPECTED;
{$ENDIF DEBUG}
    end;
  end;
end;

function TJwServerAccessControl.SetAccessRights(pAccessList: PACTRL_ACCESSW): HRESULT;
var
  ACL : TJwDAccessControlList;
begin
  fCriticalSection.Enter;
  try
    result := S_OK;

    if StrictACLVerify and (
       (pAccessList = nil) or
       (pAccessList.pPropertyAccessList = nil) or
       (pAccessList.cEntries = 0) or
       (pAccessList.pPropertyAccessList.lpProperty <> nil) or
       (pAccessList.pPropertyAccessList.fListFlags <> 0)) then
    begin
      raise EOleSysError.Create('The parameter pAccessList or one of its member is not valid.',
           MAKE_HRESULT(1, FacilityCode, ERROR_INVALID_PARAMETER), 0);
    end;

    if (pAccessList = nil) or (pAccessList.pPropertyAccessList = nil) then
    begin
      result := E_INVALIDARG;
      exit;
    end;

    try
      //Grant full access to everyone
      if pAccessList.pPropertyAccessList = nil then
      begin
        JwSetAccessRights(nil);
      end
      else
      begin
        if Assigned(fSD.DACL) then
          fSD.DACL.Clear;

        ACL := TJwDAccessControlList.Create;
        try
          CopyACLToObject({in}pAccessList, {"out"}ACL);

          JwSetAccessRights(ACL);
          fIsDirty := true;
        finally
          FreeAndNil(ACL);
        end;
      end;
    finally
      fCriticalSection.Leave;
    end;
  except
    on E : EOleSysError do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      Result := E.ErrorCode;
{$ENDIF DEBUG}
    end;
    on e : Exception do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      result := E_UNEXPECTED;
{$ENDIF DEBUG}
    end;
  end;
end;

function TJwServerAccessControl.SetOwner(pOwner, pGroup: PTRUSTEEW): HRESULT;
var Owner, Group : TJwSecurityId;
begin
  try
    if (pOwner = nil) and (pGroup = nil) then
    begin
      result := E_INVALIDARG;
      exit;
    end;

    result := S_OK;
    try
      Owner := nil;
      Group := nil;

      if StrictACLVerify and
         (not IsValidTrustee(pOwner) or
          not IsValidTrustee(pGroup)
         ) then
      begin
        begin
          raise EOleSysError.Create('Either pOwner or pGroup Trustee is not compatible/valid to MS implementation of IAccessControl. ',
             MAKE_HRESULT(1, FacilityCode, ERROR_INVALID_PARAMETER), 0);
        end;
      end;

      if pOwner <> nil then
      begin
        Owner := TJwSecurityId.Create(pOwner^);
      end;

      if pGroup <> nil then
      begin
        Group := TJwSecurityId.Create(pGroup^);
      end;

      JwSetOwner(Owner, Group);
      fIsDirty := true;
    finally
      fCriticalSection.Enter;
    end;
  except
    on E : EOleSysError do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      Result := E.ErrorCode;
{$ENDIF DEBUG}
    end;
    on e : Exception do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      result := E_UNEXPECTED;
{$ENDIF DEBUG}
    end;
  end;
end;



function TJwServerAccessControl.TrusteeToSid(const Trustee: PTrusteeW): TJwSecurityId;
begin
  if Trustee = nil then
  begin
    result := nil;
    exit;
  end;

  try
    result := TJwSecurityId.Create(Trustee^);
  except
    //on error we just return nil
    result := nil;
  end;
end;

const
  MAX_SECURITY_DESCRIPTOR_SIZE = 2 * $10000 {MAX_ACL_SIZE} + $FFF {2* SID + Header + security space};

type
  TMagicHeaderRecord = packed record
    Header : array[0..SD_MAGIC_LENGTH-1] of Byte;
    Size : Cardinal;
    Hash : Int64;
    HashUsed : Byte;
  end;

function TJwServerAccessControl.Load(const stm: IStream): HRESULT;
  procedure ValidateSD(const SD : TJwSecurityDescriptor);
  begin
    if Assigned(SD.DACL) then
    begin
      if not SD.DACL.IsValid then
      begin
        raise EOleSysError.Create('The DACL read from stream is invalid.',
           MakeResult(1, FacilityCode, ERROR_INVALID_ACL), 0);
      end;
    end;
  end;
var
  b : Boolean;
  cSize,
  cRead : Cardinal;
  pSD : PSecurityDescriptor;
  Mem : TMemoryStream;

  SD : TJwSecurityDescriptor;
  JwHeader : TMagicHeaderRecord;

  Stream : IPersistStream;
  IA : IAccessControl;
  PA : PACTRL_ACCESSW_ALLOCATE_ALL_NODES;
  pO, pG : PTRUSTEEW;
const
  SIZE_LENGTH = sizeof(Cardinal);
begin
  Result := E_FAIL;

  if not fSupportIPersistStream then
  begin
    result := E_NOTIMPL;
    exit;
  end;

  if stm = nil then
  begin
    result := E_INVALIDARG;
    exit;
  end;

  try
    fCriticalSection.Enter;

    try
      case PersistStreamType of
        pstIAccessControl:
        begin
          IA := CreateCOMImplementation;

          Stream := IA as IPersistStream;
          OleCheck(Stream.Load(stm));

          OleCheck(IA.GetAllAccessRights(nil, PA, pO, pG));
          try
            OleCheck(Self.SetAccessRights(PA));

            Self.SetOwner(pO, pG); //ignore result since pO = pG = nil

            result := S_OK;
          finally
            CoTaskMemFree(PA);
            CoTaskMemFree(pO);
            CoTaskMemFree(pG);
          end;

          result := S_OK;
          exit;
        end;
        pstPlain:
          begin
            result := stm.Read(@cSize, SizeOf(cSize), @cRead);

            if Succeeded(result) then
            begin
              //size of descriptor is invalid
              //If the stream wasn't written compatible
              //Then it is highly possible that the read value is out of bounds
              if (cSize < sizeof(TSecurityDescriptor)) or
                 (cSize > MAX_SECURITY_DESCRIPTOR_SIZE) then
              begin
                raise EOleSysError.Create(Format('The stream size of the security descriptor is invalid. '+
                  'The size %d read from stream is either too big (%d) or too small (%d).', [cSize, MAX_SECURITY_DESCRIPTOR_SIZE, sizeof(TSecurityDescriptor)]),
                  MakeResult(1, FacilityCode, ERROR_INVALID_BLOCK_LENGTH), 0);
              end;

              GetMem(pSD, cSize);
              try
                Result := stm.Read(pSD, cSize, @cRead);

                if Succeeded(Result) then
                begin
                  if not IsValidSecurityDescriptor(pSD) then
                  begin
                    raise EOleSysError.Create('The security descriptor read from stream is invalid.',
                      MakeResult(1, FacilityCode, ERROR_INVALID_SECURITY_DESCR), 0);
                  end;

                  SD := TJwSecurityDescriptor.Create(pSD);
                  try
                    ValidateSD(SD);
                  except
                    FreeAndNil(SD);
                    raise;
                  end;

                  FreeAndNil(fSD);
                  fSD := SD;

                  result := S_OK;

                end;
              finally
                FreeMem(pSD);
              end;
            end;
          end;
        pstJwscl:
          begin
            Assert(sizeof(TMagicHeaderRecord) <> SD_HEADER_SIZE, 'Size of TMagicHeaderRecord and SD_HEADER_SIZE differs.');

            {We need to read the MAGIC header written by JWSCL
             Then we get the size of the SD.
             Furthermore we read the SD data ([size] bytes) and write it into the memory stream
             behind the header and sd size.
            }

            result := stm.Read(@JwHeader, SD_HEADER_SIZE, nil);

            if Succeeded(result) then
            begin
              if (JwHeader.Size < sizeof(TSecurityDescriptor)) or
                 (JwHeader.Size > MAX_SECURITY_DESCRIPTOR_SIZE) then
              begin
                raise EOleSysError.Create(Format('The stream size of the security descriptor is invalid. '+
                  'The size %d read from stream is either too big (%d) or too small (%d).', [JwHeader.Size, MAX_SECURITY_DESCRIPTOR_SIZE, sizeof(TSecurityDescriptor)]),
                  MakeResult(1, FacilityCode, ERROR_INVALID_BLOCK_LENGTH), 0);
              end;

              Mem := TMemoryStream.Create;
              try

                Mem.SetSize(JwHeader.Size + SD_HEADER_SIZE);

                b := Mem.Write(JwHeader, SD_HEADER_SIZE) = SD_HEADER_SIZE;

                if not b then
                begin
                  raise EOleSysError.Create('The stream data could not be copied to internal memory.',
                      MakeResult(1, FacilityCode, ERROR_WRITE_FAULT), 0);
                end;

                {Reading the data from the stream to the memory of TMemoryStream.
                 We put it behind the first DWORD because this value contains the size of the
                 security descriptor used by TJwSecurityDescriptor

                 [0..4]       MAGIC HEADER
                 [5..8]       SD size
                 [9..18]      Misc header data
                 [19..cSize]  security descriptor

                 See TJwSecurityDescriptor.SaveToStream
                 }
                result := stm.Read(Pointer(DWORD_PTR(Mem.Memory) + SD_HEADER_SIZE), JwHeader.Size - SD_HEADER_SIZE, nil);

                if Succeeded(result) then
                begin
                  Mem.Position := 0;

                  SD := TJwSecurityDescriptor.Create(Mem);
                  try
                    ValidateSD(SD);
                  except
                    FreeAndNil(SD);
                    raise;
                  end;

                  FreeAndNil(fSD);
                  fSD := SD;

                  result := S_OK;
                end;
              finally
                Mem.Free;
              end;
            end;
          end;
      end;
    finally
      fCriticalSection.Leave;
    end;


  except
    on E : EOleSysError do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      Result := E.ErrorCode;
{$ENDIF DEBUG}
    end;
    on e : Exception do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      result := E_UNEXPECTED;
{$ENDIF DEBUG}
    end;
  end;
end;

procedure TJwServerAccessControl.LoadFromStream(const Stream: TStream);
var Mem : IStream;
begin
  Mem := TStreamAdapter.Create(Stream, soReference) as IStream;

  OleCheck(Load(Mem));
end;

function TJwServerAccessControl.Save(const stm: IStream; fClearDirty: BOOL): HRESULT;
var
  cSize,
  cWritten : Cardinal;
  pSD : PSecurityDescriptor;
  Mem : TMemoryStream;

  Stream : IPersistStream;
  IA : IAccessControl;
  PA : PACTRL_ACCESSW_ALLOCATE_ALL_NODES;
  pO, pG : PTRUSTEEW;

begin
  if not fSupportIPersistStream then
  begin
    result := E_NOTIMPL;
    exit;
  end;

  if stm = nil then
  begin
    result := E_INVALIDARG;
    exit;
  end;

  result := S_OK;
  try
    fCriticalSection.Enter;
    try

      case PersistStreamType of
        pstIAccessControl:
        begin
          OleCheck(Self.GetAllAccessRights(nil, PA, pO, pG));
          try
            IA := CreateCOMImplementation;
            OleCheck(IA.SetAccessRights(PA));
            IA.SetOwner(pO, pG);   //ignore result because it is not implemented currently

            Stream := IA as IPersistStream;
            OleCheck(Stream.Save(stm, fClearDirty));

          finally
            CoTaskMemFree(PA);
            CoTaskMemFree(pO);
            CoTaskMemFree(pG);
          end;

          result := S_OK;
          exit;
        end;
        pstPlain:
          begin
            try
              pSD := fSD.Create_SD(cSize, True);
            except
          {$IFDEF DEBUG}
              raise;
          {$ELSE}
              result := STG_E_CANTSAVE;
          {$ENDIF DEBUG}
            end;

            if Succeeded(result) then
            begin
              try
                result := stm.Write(@cSize, sizeof(cSize), nil);
                if Succeeded(result) then
                  result := stm.Write(pSD, cSize, nil);
              finally
                TJwSecurityDescriptor.Free_SD(pSD);
              end;
            end;
          end;
        pstJwscl:
          begin
            Mem := TMemoryStream.Create;
            try
              fSD.SaveToStream(Mem);
              Mem.Position := 0;
              result := stm.Write(Mem.Memory, Mem.Size, @cWritten);
            finally
              Mem.Free;
            end;
          end;
      end;

      if Succeeded(result) and fClearDirty then
        fIsDirty := false;
    finally
      fCriticalSection.Leave;
    end;

  except
    on E : EOleSysError do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      Result := E.ErrorCode;
{$ENDIF DEBUG}
    end;
    on e : Exception do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      result := E_UNEXPECTED;
{$ENDIF DEBUG}
    end;
  end;
end;

procedure TJwServerAccessControl.SaveToStream(const Stream: TStream);
var Mem : IStream;
begin
  Mem := TStreamAdapter.Create(Stream, soReference) as IStream;

  OleCheck(Save(Mem, false));
end;

function TJwServerAccessControl.IsDirty: HRESULT;
begin
  if not fSupportIPersistStream then
  begin
    result := E_NOTIMPL;
    exit;
  end;

  if fIsDirty then
  begin
    result := S_OK;
  end
  else
  begin
    result := S_FALSE;
  end;
end;



function TJwServerAccessControl.IsValidTrustee(const Trustee: PTrusteeW): Boolean;
begin
  Result := false;
  if Trustee = nil then
    exit;

  result :=
     (Trustee.pMultipleTrustee = nil) and
     (Trustee.MultipleTrusteeOperation = NO_MULTIPLE_TRUSTEE) and
     ((Trustee.TrusteeForm = TRUSTEE_IS_SID) or (Trustee.TrusteeForm = TRUSTEE_IS_NAME)) and
     ((Trustee.TrusteeType = TRUSTEE_IS_USER) or (Trustee.TrusteeType = TRUSTEE_IS_GROUP))
     ;
end;

function TJwServerAccessControl.GetSizeMax(out cbSize: Largeint): HRESULT;
var
  pSD : PSecurityDescriptor;
  Mem : TMemoryStream;
  cSize : Cardinal;

  Stream : IPersistStream;
  IA : IAccessControl;
  PA : PACTRL_ACCESSW_ALLOCATE_ALL_NODES;
  pO, pG : PTRUSTEEW;

begin
  if not fSupportIPersistStream then
  begin
    cbSize := 0;
    result := E_NOTIMPL;
    exit;
  end;

  result := S_OK;

  try
    case PersistStreamType of
      pstIAccessControl:
        begin
          OleCheck(Self.GetAllAccessRights(nil, PA, pO, pG));
          try
            IA := CreateCOMImplementation;
            OleCheck(IA.SetAccessRights(PA));
            IA.SetOwner(pO, pG); //ignore result because it is not implemented currently

            Stream := IA as IPersistStream;
            OleCheck(Stream.GetSizeMax(cbSize));
          finally
            CoTaskMemFree(PA);
            CoTaskMemFree(pO);
            CoTaskMemFree(pG);
          end;

          result := S_OK;
          exit;
        end;
      pstJwscl:
        begin
          pSD := fSD.Create_SD(cSize, True);
          try
            cbSize := cSize;
            {...}
          finally
            TJwSecurityDescriptor.Free_SD(pSD);
          end;
        end;

      pstPlain:
        begin
          Mem := TMemoryStream.Create;
          try
            fSD.SaveToStream(Mem);
            cbSize := Mem.Size;
          finally
            Mem.Free;
          end;
        end;
    end;
  except
    on E : EOleSysError do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      Result := E.ErrorCode;
{$ENDIF DEBUG}
    end;
    on e : Exception do
    begin
{$IFDEF DEBUG}
      raise;
{$ELSE}
      result := E_UNEXPECTED;
{$ENDIF DEBUG}
    end;
  end;
end;

{WARNING: ONLY FOR DEMONSTRATION PURPOSES}
var
  SaveInitProc: Pointer = nil;

procedure InitComServer; {WARNING: ONLY FOR DEMONSTRATION PURPOSES}
begin
  //In a service, add code to CoInitializeSecurity here

  if SaveInitProc <> nil then TProcedure(SaveInitProc);
end;

initialization
  SaveInitProc := InitProc;
  //InitProc := @InitComServer; {WARNING: ONLY FOR DEMONSTRATION PURPOSES}

end.


