{@abstract(This unit provides access to the Windows 2000 (and above) security ACL dialog also used by Windows Explorer.)
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

The Original Code is JwsclSecurityDialogs.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

Description:

Links:
Secure object types:
  http://msdn2.microsoft.com/en-us/library/aa379593.aspx
}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclSecurityDialogs;
{$INCLUDE Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $

interface

uses SysUtils, Classes, Registry, Contnrs,
{$IFDEF FPC}
  Buttons,
{$ENDIF FPC}
  jwaWindows, Dialogs, StdCtrls, ComCtrls,ActiveX,
  JwsclResource,
  JwsclTypes, JwsclExceptions, JwsclSid, JwsclAcl, JwsclToken,
  JwsclMapping, JwsclKnownSid, JwsclSecureObjects,
  JwsclVersion, JwsclConstants, JwsclProcess, JwsclDescriptor,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
  TJwSecurityDescriptorDialog = class;


  //TOnSecurityApply = procedure
     {@Name is a callback procedure that is called when a supported window has a handle
      and was initialized. Supported windows are the constants in TJwSecurityPageType.
      The window handles can be retrieved from the dialog instance (property WindowHandle, AdvWindowHandle and AdvancedPages)

      @param Sender Contains the dialog instance that called this callback message.
      @param PageType Defines which page was initialized and now contains a valid window handle.
     }
  TJwOnInitSecurityDialog = procedure(Sender: TJwSecurityDescriptorDialog;
    PageType: TJwSecurityPageType) of object;
     {@Name is a callback procedure that is called if a supported window is about to be initialized.
      At this point the window does not have a valid window handle.

      @param Sender Contains the dialog instance that called this callback message.
      @param PageType Defines which page was initialized and now contains a valid window handle.
     }
  TJwOnInitSecurityPageCreate = procedure(Sender: TJwSecurityDescriptorDialog;
    PageType: TJwSecurityPageType) of object;

     {@Name is a callback procedure that is called if a supported window is about to be destroyed.
      At this point the window does have a valid window handle.

      @param Sender Contains the dialog instance that called this callback message.
      @param PageType Defines which page is about to be destroyed.
     }
  TJwOnInitSecurityPageDestroy = procedure(Sender: TJwSecurityDescriptorDialog;
    PageType: TJwSecurityPageType) of object;

     {@Name is a callback function that is called if the ACL editor needs the source of inheritance for
      a list of access control entries. The parameter ACL contains the entries which inheritance
      must be retrieved. The parameter InheritanceArray receives the inheritance strings. It is resized to
      the count of access control entries.

      @param Sender Contains the dialog instance that called this callback message.
      @param Info Defines which security information (SACL or DACL) must be processed.
      @param ACL Defines the ACL which inheritance must be retrieved.
      @param(InheritanceArray This array receives the inheritance sources for all entries of the ACL.
             If the AncestorName is empty, the source is automatically a parent.
             The SID member of the array elements is not used.
             If the AncestorName is empty the ACL editor automatically sets the display string to
               1. inherited from superior object (if the ACE contains afInheritedAce in its flags)
               2. not inherited (if the ACE does not contain afInheritedAce)
             )
      @return(The function must return S_OK if the inheritance source could be retrieved. Otherwise
            S_FALSE.)
      }
  TJwOnGetInheriteSource = function(Sender: TJwSecurityDescriptorDialog;
    const Info: TJwSecurityInformationFlagSet;
    const ACL: TJwSecurityAccessControlList;
    var InheritanceArray: TJwInheritedFromArray)
    : Cardinal of object;

     {@Name is a callback function that is called if the user wants to get the
       effective permissions for a user on the effective permission tab of the ACL editor.

      @param Sender Contains the dialog instance that called this callback message.
      @param GuidObjectType defines the GUID of the object. Cann be GUID_NULL.
      @param sServerName defines the name of the server.
      @param SD defines the security descriptor that is used to check against the permissions
      @param(ObjectTypeList defines a list of objects types that are used for the check.
              It can be ignored to use default object type list.)
      @param(GrantedAccessList defines an array of granted access. It is automatically set to count of 1
            if the call to GetEffectiveRightsFromAcl was sucessfull. However it can be changed.
            If the size of the array is zero the effective permissions are not display. Instead an error message
            is shown)
      @return(The return value defines the sucess of the operation. If it is set to any other value than S_OK
            the effective permissions are not shown in the ACL editor.)
     }
  TJwOnGetEffectivePermissions = function(Sender: TJwSecurityDescriptorDialog;
    const GuidObjectType: TGUID;
    const sServerName: WideString;
    const SD: TJwSecurityDescriptor;
    var ObjectTypeList:
    TJwObjectTypeListArray;
    var GrantedAccessList: TJwAccessMaskArray): Cardinal of object;

  {Not supported}
  TJwOnLookupSIDs = function(Sender: TJwSecurityDescriptorDialog;
    const SIDList: TJwSecurityIdList;
    var SIDInfoList: TJwSidInfoRecordArray): Cardinal of object;

     {@Name is a callback procedure that is called if the an ACL has to be checked for correctness.
      @param Sender Contains the dialog instance that called this callback message.
      @param ACL defines the access control list which must be checked.
      @param(IsCanonical defines the result of the process that checks the ACL. Set it to true if the order of the ACL is correct,
              otherwise false. If it set to false the ACL editor shows a message that informs the user about the incorrect order.
              The ACL is automatically checked for correct order and the result value is predefined in IsCanonical.)
      }
  TOnJwIsDaclCanonical = procedure(Sender: TJwSecurityDescriptorDialog;
    const ACL: TJwSecurityAccessControlList; var IsCanonical: boolean) of object;

     {@Name is a callback procedure that is called if the ACL editor needs information about the security descriptor.
      Usually the property SecurityDescriptor of Sender is used. However if the parameter SD is not nil
      the parameter SD is used instead. If it is nil the property SecurityDescriptor is used.

      @param Sender Contains the dialog instance that called this callback message.
      @param(Information defines a set of security information flags that have to be stored in the security descriptor)
      @param(bDefault TBD)
      @param(SD defines a security descriptor that provides information for the ACL editor. It can be nil to use
              the property SecurityDescriptor of Sender is used)

      }
  TJwOnGetSecurity = procedure(Sender: TJwSecurityDescriptorDialog;
    const Information: TJwSecurityInformationFlagSet;
    const bDefault: boolean;
    var SD: TJwSecurityDescriptor) of object;

     {@Name defines a callback function that is called every time a window message is processed to
      any supported window (see TJwSecurityPageType) in the ACL editor. This function is called before the default window proc of the window is
      processed.

      @param Sender Contains the dialog instance that called this callback message.
      @param PageType defines which page has received the window message
      @param WindowHandle contains the handle of the window that received the window message
      @param Msg Contains the window message
      @param ParamW Contains additional information about the message.
      @param ParamL Contains additional information about the message.
      @param ProcessDefaultProc defines whether message is forwarded to the default window proc (true) or not (false).
      @return(The return value is used to return the status of the window message. The return value is only used if ProcessDefaultProc is false.)
     }
  TJwOnWindowProcCallBack = function(Sender: TJwSecurityDescriptorDialog;
    PageType: TJwSecurityPageType;
    WindowHandle: hWnd;
    Msg: longint;
    ParamW: longint;
    ParamL: longint;
    var ProcessDefaultProc: boolean)
    : longint;


     {@Name is a class that administers a list of SI_INHERIT_TYPE structures.
      The structure is used to provide information how access control entries
      can be inherited to its children.

      Simply use the property InheritTypeList to add or remove inheritance information.

      InheritTypeList.Add(GUID_NULL, [afContainerInheritAce, afObjectInheritAce], 'This object, inherited objects and containers');

     }
  TJwInheritTypeList = class(TList)
  protected
    fData: PSI_INHERIT_TYPE;
    function Get(Index: integer): PSiInheritType;
    procedure Put(Index: integer; Data: PSiInheritType);

    function NewRecord(const GUID: TGUID; const Flags: TJwAceFlags;
      const Name: WideString): PSiInheritType;
    function GetArray: PSI_INHERIT_TYPE;
  public
    destructor Destroy; override;
        {@Name adds a new entry to the list.
         @param GUID defines the GUID of the inheritance information. Can be NULL_GUID
         @param Flags defines a set of flags that defines the inheritance. see TJwAceFlags .
         @param Name defines a description that is displayed in the combo box for this item.
        }
    function Add(const GUID: TGUID; const Flags: TJwAceFlags;
      const Name: WideString): integer;

        {Removes an item from the list.
         @raises Exception is raised if the index is invalid.}
    procedure Delete(Index: integer);

    function First: PSiInheritType;
    function IndexOf(Item: PSiInheritType): integer;
        {
        @Name inserts a new entry to the list.
         @param Index defines the position in list where to add it.
         @param GUID defines the GUID of the inheritance information. Can be NULL_GUID
         @param Flags defines a set of flags that defines the inheritance. see TJwAceFlags .
         @param Name defines a description that is displayed in the combo box for this item.
        }
    procedure Insert(Index: integer; const GUID: TGUID;
      const Flags: TJwAceFlags; const Name: WideString);
    function Last: PSiInheritType;

    {@Name sets or gets a SI_INHERIT_TYPE. Do not free it!}
    property Items[Index: integer]: PSiInheritType Read Get Write Put;
      default;

        {@Name returns an array of SI_INHERIT_TYPE entries. It is used for winapi calls.
         The Array is created every read call to it and the previous list ist destroyed.
         If the TJwInheritTypeList instance is destroyed the array is also destroyed.
        }
    property ItemsArray: PSI_INHERIT_TYPE Read GetArray;
  end;




  {TJwSidInfoDataObject is used in TJwSecurityDescriptorDialog.LookUpSids
  to provide information about the sids for the system.
  }
  TJwSidInfoDataObject = class(TInterfacedObject, IDataObject)
  public
    fInfoList: PSID_INFO_LIST;
    fSidStrings, fPSIDList: TList;
  public
    constructor Create(pInfoList: PSID_INFO_LIST;
      aPSIDList, aSidStrings: TList);
    destructor Destroy; override;

    function GetData(const formatetcIn: TFormatEtc;
      out medium: TStgMedium): HRESULT; stdcall;
    function GetDataHere(const formatetc: TFormatEtc;
      out medium: TStgMedium): HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult;
      stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc;
      out formatetcOut: TFormatEtc): HResult; stdcall;

      
    function SetData(const formatetc: TFormatEtc;
      var medium: TStgMedium; fRelease: BOOL): HResult; stdcall;

    function EnumFormatEtc(dwDirection: {$IFDEF FPC}DWORD{$ELSE}Longint{$ENDIF FPC};
      out enumFormatEtc: IEnumFormatEtc): HResult; stdcall;

    function DAdvise(const formatetc: TFormatEtc;
      advf: {$IFDEF FPC}DWORD;{$ELSE}Longint;{$ENDIF FPC} const advSink: IAdviseSink;
      out dwConnection: {$IFDEF FPC}DWORD{$ELSE}Longint{$ENDIF FPC}): HResult; stdcall;
    function DUnadvise(dwConnection: {$IFDEF FPC}DWORD{$ELSE}Longint{$ENDIF FPC}): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
      stdcall;
  end;



     {@Name is a callback procedure that is called if the user hit the OK or Apply Button.
      If the function is not called the security descriptor is not changed and
      the user cannot apply the security information.


      @param Sender Sender contains the TJwSecurityDescriptorDialog implementation.
      @param SecurityType contains information which part of the SD is changed.
      @param(SecurityDialogFlags contains information about flags, states and checkboxes states
              in the dialog that are set)
      @param SecurityResetType defines whether the SD must be recursively iterated through the objects.
      @param Settings contains the SD control bits of the parameter NewSecurityDescriptor.
      @param(NewSecurityDescriptor is the security descriptor which contains the security information
               that was changed in the dialog)
      @param(MergedSecurityDescriptor contains the merged security information from
              NewSecurityDescriptor and the property SecurityDescriptor)
      @param(bSuccess Defines whether the security information could be set. Its default value is false.
            If true, the property SecurityDescriptor is to MergedSecurityDescriptor and the ACL editor updates its data.
            If false the ACL editor resets its data.
            )

      }
  TJwOnSetSecurity = procedure(Sender: TJwSecurityDescriptorDialog;
    SecurityType: TJwSecurityInformationFlagSet;
    SecurityDialogFlags: TJwSecurityDialogFlags;
    SecurityResetType: TJwSecurityResetType;
    Settings: TJwSecurityDescriptorControlSet;
    NewSecurityDescriptor, MergedSecurityDescriptor
    : TJwSecurityDescriptor;
    var bSuccess: boolean) of object;

  {@exclude}
  PJwMsgRec = ^TJwMsgRec;
  {for internal use only}
  TJwMsgRec = record
    {@Name contains a pointer to a saved default window proc}
    pProc:    Pointer;
    {@Name contains a handle to the window which proc is saved}
    hHandle:  HWND;
    {@Name contains information about the page type of the window}
    uPage:    SI_PAGE_TYPE;
    {@Name defines the security dialog instance which holds this record}
    Dlg:      TJwSecurityDescriptorDialog;
    {@Name contains the previous GWLP_USERDATA pointer}
    userData: Pointer;
  end;

  {@Name is  for internal use only.
   Todo: export to JEDI Api lib
  }
  ISecurityObjectTypeInfoW = interface(IUnknown)
    ['{fc3066eb-79ef-444b-9111-d18a75ebf2fa}']
    function GetInheritSource(si: SECURITY_INFORMATION;
      pACL: PACL; var ppInheritArray: PINHERITEDFROMW): HRESULT; stdcall;
  end;


  

     {@Name is a easy to use class to show a security ACL editor.
      It is generic, so many types of secure objects can be shown (not only files).
      }
  TJwSecurityDescriptorDialog = class({TInterfacedObject,}TObject,
    ISecurityInformation,
    ISecurityInformation2,
    ISecurityObjectTypeInfoW
    , IEffectivePermission)
  public
    FRefCount: integer;
    {ISecurityInformation}
    function GetObjectInformation(
      out pObjectInfo: SI_OBJECT_INFO): HRESULT; stdcall;
    function GetSecurity(RequestedInformation: SECURITY_INFORMATION;
      out ppSecurityDescriptor: PSECURITY_DESCRIPTOR;
      fDefault: BOOL): HRESULT; stdcall;
    function SetSecurity(SecurityInformation: SECURITY_INFORMATION;
      pSecurityDescriptor: PSECURITY_DESCRIPTOR): HRESULT; stdcall;
    function GetAccessRights(pguidObjectType: LPGUID;
      dwFlags: DWORD; out ppAccess: PSI_ACCESS;
      out pcAccesses, piDefaultAccess: ULONG): HRESULT; stdcall;
    function MapGeneric(pguidObjectType: LPGUID;
      pAceFlags: PUCHAR; pMask: PACCESS_MASK): HRESULT; stdcall;
    {ISecurityObjectTypeInfoW}
    function GetInheritTypes(out ppInheritTypes: PSI_INHERIT_TYPE;
      out pcInheritTypes: ULONG): HRESULT; stdcall;
    function PropertySheetPageCallback(aHwnd: HWND; uMsg: UINT;
      uPage: SI_PAGE_TYPE): HRESULT; stdcall;


    function GetInheritSource(si: SECURITY_INFORMATION;
      pACL: PACL; var ppInheritArray: PInheritedFromW): HRESULT; stdcall;
    function GetEffectivePermission(const pguidObjectType: TGUID;
      pUserSid: PSID; pszServerName: LPCWSTR;
      pSD: PSECURITY_DESCRIPTOR; var ppObjectTypeList: POBJECT_TYPE_LIST;
      var pcObjectTypeListLength: ULONG;
      var ppGrantedAccessList: PACCESS_MASK;
      var pcGrantedAccessListLength: ULONG): HRESULT;
      stdcall;




    {ISecurityInformation2}
    function IsDaclCanonical(pDacl: PACL): BOOL; stdcall;

    {@Name is not supported. See source code for more information}
    function LookupSids(cSids: ULONG; rgpSids: PPSID;
      out ppdo: IDataObject): HRESULT; stdcall;

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: integer; stdcall;
    function _Release: integer; stdcall;
  protected
    fSD:      TJwSecurityDescriptor;
    fParent:  HWND;
    fMapping: TJwSecurityGenericMappingClass;
    fResult:  Cardinal;
    fTag:     Pointer;

    pTempAccess: PSI_ACCESS;
    iTempAccess: Cardinal;

    fInstance:   Cardinal;
    fServerName, fObjectName, fPageTitle: TJwString;
    fObjectType: TGUID;

    //StringPool for ISecurityInformation.GetObjectInformation
    sPageTitle, sObjectName, sServerName: PWideChar;

    fAdvWindowHandle, fWindowHandle: HWND;
    fOnInitSecurityDialog: TJwOnInitSecurityDialog;
    fOnInitSecurityPageCreate: TJwOnInitSecurityPageCreate;
    fOnInitSecurityPageDestroy: TJwOnInitSecurityPageDestroy;
    fOnGetInheriteSource:  TJwOnGetInheriteSource;
    fOnWindowProcCallBack: TJwOnWindowProcCallBack;
    fOnLookupSIDs:         TJwOnLookupSIDs;
    fOnIsDACLCanonical:    TOnJwIsDaclCanonical;
    fOnGetSecurity:        TJwOnGetSecurity;

    fOnSetSecurity:   TJwOnSetSecurity;
    fInheritTypeList: TJwInheritTypeList;

    fSetOwnerButton: TButton;

    fOnGetEffectivePermissions: TJwOnGetEffectivePermissions;


    fObjectTypeList: TJwObjectTypeListArray;




    fFlags: TJwSecurityDialogFlags;

    ProcList: TList; //contains a list of PJwMsgRec

    function GetSecurityDescriptor: TJwSecurityDescriptor;
    procedure SetSecurityDescriptor(aSD: TJwSecurityDescriptor);

    function GetOwner: TJwSecurityId;
    procedure SetOwner(anOwner: TJwSecurityId);

    function GetDACL: TJwDAccessControlList;
    procedure SetDACL(aDACL: TJwDAccessControlList);

    function GetSACL: TJwSAccessControlList;
    procedure SetSACL(aSACL: TJwSAccessControlList);

    procedure SetInheritTypeList(aList: TJwInheritTypeList); virtual;

    function AddNewPageRec(hwnd: HWND; uPage: SI_PAGE_TYPE): boolean;
    function DeletePageRec(uPage: SI_PAGE_TYPE): boolean;

    function GetAdvWindowHandle(Page: TJwSecurityPageType): HWND; virtual;
  public
    {@Name creates a new ACL editor using a window handle as parent.}
    constructor Create(hParent: HWND);
    destructor Destroy; override;

    {@Name shows the security dialog.}
    function ShowModal: boolean; virtual;

    {@Name defines flags which controls the display of the ACL editor.}
    property Flags: TJwSecurityDialogFlags Read fFlags Write fFlags;

    {@Name defines the instance that is used to retrieve resource strings and more.}
    property Instance: Cardinal Read fInstance Write fInstance;
    {@Name defines a server name}
    property ServerName: TJwString Read fServerName Write fServerName;
    {@Name defines an object name which security information is shown/changed.
     If the length of @Name is smaller than 2 chars it will be filled by spaced
     due to a bug in the security dialog api.
    }
    property ObjectName: TJwString Read fObjectName Write fObjectName;
    {@Name defines the title of the page in the simple ACL tab sheet.}
    property PageTitle: TJwString Read fPageTitle Write fPageTitle;
    {@Name defines an object GUID. Can be NULL_GUID (default)}
    property ObjectType: TGUID Read fObjectType Write fObjectType;

    {@Name defines a security descriptor which is shown in the ACL editor.}
    property SecurityDescriptor: TJwSecurityDescriptor
      Read GetSecurityDescriptor Write SetSecurityDescriptor;

    {@Name defines an Owner. This instance points directly to the owner of the property SecurityDescriptor.}
    property Owner: TJwSecurityId Read GetOwner Write SetOwner;
    {@Name defines an DACL. This instance points directly to the DACL of the property SecurityDescriptor.}
    property DACL: TJwDAccessControlList Read GetDACL Write SetDACL;
    {@Name defines an SACL. This instance points directly to the SACL of the property SecurityDescriptor.}
    property SACL: TJwSAccessControlList Read GetSACL Write SetSACL;

        {@Name defines a class that provides information about a specific security information.
         It includes GENERIC mapping and converting access rights to description (shown in access rightslist).
        }
    property Mapping: TJwSecurityGenericMappingClass
      Read fMapping Write fMapping;


    {see @link(TJwOnInitSecurityDialog)}
    property OnInitSecurityDialog: TJwOnInitSecurityDialog
      Read fOnInitSecurityDialog Write fOnInitSecurityDialog;
    {see @link(TJwOnInitSecurityPageCreate)}
    property OnInitSecurityPageCreate: TJwOnInitSecurityPageCreate
      Read fOnInitSecurityPageCreate Write fOnInitSecurityPageCreate;
    {see @link(TJwOnInitSecurityPageDestroy)}
    property OnInitSecurityPageDestroy: TJwOnInitSecurityPageDestroy
      Read fOnInitSecurityPageDestroy Write fOnInitSecurityPageDestroy;
    {see @link(TJwOnSetSecurity)}
    property OnSetSecurity: TJwOnSetSecurity
      Read fOnSetSecurity Write fOnSetSecurity;
    {see @link(TJwOnGetInheriteSource)}
    property OnGetInheriteSource: TJwOnGetInheriteSource
      Read fOnGetInheriteSource Write fOnGetInheriteSource;
    {see @link(TJwOnGetEffectivePermissions)}
    property OnGetEffectivePermissions: TJwOnGetEffectivePermissions
      Read fOnGetEffectivePermissions Write fOnGetEffectivePermissions;
    {@Name is not supported actually and must not be used!}
    property OnLookupSIDs: TJwOnLookupSIDs
      Read fOnLookupSIDs Write fOnLookupSIDs;
    {see @link(TOnJwIsDaclCanonical)}
    property OnIsDACLCanonical: TOnJwIsDaclCanonical
      Read fOnIsDACLCanonical Write fOnIsDACLCanonical;
    {see @link(TJwOnGetSecurity)}
    property OnGetSecurity: TJwOnGetSecurity
      Read fOnGetSecurity Write fOnGetSecurity;
    {see @link(TJwOnWindowProcCallBack)}
    property OnWindowProcCallBack: TJwOnWindowProcCallBack
      Read fOnWindowProcCallBack Write fOnWindowProcCallBack;


        {@Name can contain a list of inherit type entries that are shown in the property dialog
         of an ACE entry. Each entry defines how an ACE is inherited to children.
        }
    property InheritTypeList: TJwInheritTypeList
      Read fInheritTypeList Write SetInheritTypeList;

    {@Name contains the window handle of the simple ACL editor window. If the window handle does not exist it is 0.}
    property WindowHandle: HWND Read fWindowHandle;
    {@Name contains the window handle of the advanced ACL editor window. If the window handle does not exist it is 0.}
    property AdvWindowHandle: HWND Read fAdvWindowHandle;

    {@Name contains the window handle of the pages of the advanced ACL editor. If the window handle does not exist it is 0.}
    property AdvancedPages[Page: TJwSecurityPageType]: HWND
      Read GetAdvWindowHandle;

    property Tag: Pointer Read fTag Write fTag;
  end;



{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

uses Controls, JwsclEnumerations;


{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_INTERFACE_SECTION}

//not public!
function CompareGUID(const G1, G2: TGUID): boolean;
begin
  Result := CompareMem(@G1, @G2, Sizeof(TGUID));
end;


{ TJwSecurityDescriptorDialog }

function TJwSecurityDescriptorDialog.GetAccessRights(pguidObjectType: LPGUID;
  dwFlags: DWORD; out ppAccess: PSI_ACCESS;
  out pcAccesses, piDefaultAccess: ULONG): HRESULT;
begin
  Mapping.FreeAccessNames(pTempAccess, iTempAccess);

  ppAccess := Mapping.GetAccessNames(pcAccesses);
  pTempAccess := ppAccess;
  iTempAccess := pcAccesses;


  piDefaultAccess := 0;
  Result := S_OK;
end;

function TJwSecurityDescriptorDialog.GetInheritTypes(
  out ppInheritTypes: PSI_INHERIT_TYPE;
  out pcInheritTypes: ULONG): HRESULT;
begin
  ppInheritTypes := nil;

  if fInheritTypeList.Count > 0 then
    ppInheritTypes := fInheritTypeList.ItemsArray;

  pcInheritTypes := fInheritTypeList.Count;

  Result := S_OK;
end;



function TJwSecurityDescriptorDialog.GetObjectInformation(
  out pObjectInfo: SI_OBJECT_INFO): HRESULT;

  procedure InitStringPool;
  begin
    if sPageTitle <> nil then
      FreeMem(sPageTitle);
    if sObjectName <> nil then
      FreeMem(sObjectName);
    if sServerName <> nil then
      FreeMem(sServerName);
  end;

  procedure InitString(var p: PWideChar; const default: TJwString);
  begin
    //we must create widechar because dest is a widechar!
    GetMem(p, (3 + Length(default)) * sizeof(widechar));
    FillChar(p^, (3 + Length(default)) * sizeof(widechar), 0);
    if (Length(Default) > 0) then
      CopyMemory(p, @WideString(default)[1],
        (Length(default)) * sizeof(widechar));
  end;

begin
  Result := S_OK;

  InitStringPool;

  FillChar(pObjectInfo, sizeof(pObjectInfo), 0);
  // We want to edit the DACL (PERMS), the OWNER,
  // and we want the Advanced button

  pObjectInfo.dwFlags := TJwEnumMap.ConvertFlags(Flags);


  // this determines the module used to discover stringtable entries
  pObjectInfo.hInstance := HInstance;

  if Length(ServerName) = 0 then
  begin
    pObjectInfo.pszServerName := nil;
  end
  else
  begin
    InitString(pObjectInfo.pszServerName, fServerName);
    sServerName := pObjectInfo.pszServerName;
  end;

  InitString(pObjectInfo.pszPageTitle, fPageTitle);
  sPageTitle := pObjectInfo.pszPageTitle;

  {BugBug. The objectname displayed in the title
   must at least 2 chars long to be displayed correctly.}
  if Length(fObjectName) = 0 then
    fObjectName := '  ';
  if Length(fObjectName) = 1 then
    fObjectName := fObjectName + ' ';

  InitString(pObjectInfo.pszObjectName, fObjectName);
  sObjectName := pObjectInfo.pszObjectName;


  if Length(PageTitle) <> 0 then
    pObjectInfo.dwFlags := pObjectInfo.dwFlags or SI_PAGE_TITLE;

end;

function TJwSecurityDescriptorDialog.GetSecurity(
  RequestedInformation: SECURITY_INFORMATION;
  out ppSecurityDescriptor: PSECURITY_DESCRIPTOR; fDefault: BOOL): HRESULT;
var
  ipSDSize: Cardinal;
  pSD: PSECURITY_DESCRIPTOR;

  
  aSD: TJwSecurityDescriptor;
begin
{  result := S_FALSE; //!!!!!
  exit;  }

  Result := S_OK;

  aSD := nil;
  if Assigned(fOnGetSecurity) then
    fOnGetSecurity(Self,
      TJwEnumMap.ConvertSecurityInformation(
      RequestedInformation),
      //TJwSecurityInformationFlagSet(RequestedInformation),
      fDefault,
      aSD);


  if Assigned(aSD) then
    pSD := aSD.Create_SD(ipSDSize, True)
  else
    pSD := fSD.Create_SD(ipSDSize, True);

  try
    ppSecurityDescriptor :=
      PSECURITY_DESCRIPTOR(LocalAlloc(LPTR, ipSDSize + 2));
    CopyMemory(ppSecurityDescriptor, pSD, ipSDSize);
  finally
    TJwSecurityDescriptor.Free_SD(pSD);
  end;

  if Assigned(aSD) then
    FreeAndNil(aSD);
end;

function TJwSecurityDescriptorDialog.IsDaclCanonical(pDacl: PACL): BOOL;
var
  DACL:  TJwSecurityAccessControlList;
  bBOOL: boolean;
begin
 { result := true; //!!!!!
  exit;  }

  try
    DACL := TJwDAccessControlList.Create(pDacl);
  except
    on E: EJwsclSecurityException do ;
  end;



  try
    Result := DACL.IsCanonical;
    if Assigned(fOnIsDACLCanonical) then
    begin
      bBOOL := Result;
      fOnIsDACLCanonical(
        Self,//Sender : TJwSecurityDescriptorDialog;
        DACL,//Const DACL : TJwSecurityAccessControlList;
        bBOOL//var IsCanonical : Boolean
        );
      Result := bBOOL;
    end;
  finally
    FreeAndNil(DACL);
  end;
end;


function TJwSecurityDescriptorDialog.LookupSids(cSids: ULONG; rgpSids: PPSID;
  out ppdo: IDataObject): HRESULT;
var
  SIDInfoList: TJwSidInfoRecordArray;
  SIDList: TJwSecurityIdList;
  SidPointer: PPSID;
  aSID: TJwSecurityId;
  i, iLen: integer;
  s: WideString;

  fInfoList: PSID_INFO_LIST;
  ExClass: EJwsclSecurityExceptionClass;

  fPSIDList: TList;
  fSidStrings: TList;
  //var guid : TGUID;

  //form : TFormatEtc;
  //  stgt : TStgMedium;
begin
  ppdo := nil;

  {result := E_NOTIMPL;  //!!!!
  exit;}


  {The following code must succeed in calling.
  Uncheck to see if this function LookUpSids will work with windows ACL editor
  }
  //IDataObject(ppdo).GetData(form,stgt);

  Result := E_NOTIMPL;

  if Assigned(fOnLookupSIDs) then
  begin
    SetLength(SIDInfoList, cSids);
    SIDList := TJwSecurityIdList.Create(True);

    {rgpSids is a list of pointer that points to TSid structures}
    SidPointer := rgpSids;

    try //1. SIDList := TJwSecurityIdList.Create(true);
      for i := 0 to cSids - 1 do
      begin
        try
          aSID := TJwSecurityId.Create(SidPointer^);
          Inc(SidPointer);

          SIDList.Add(aSID);
          FillChar(SIDInfoList[i], sizeof(SIDInfoList[i]), 0);
          SIDInfoList[i].pSid := TObject(aSID);
          try
            SIDInfoList[i].sClass := scnUser;
            SIDInfoList[i].sUPN := '';
            SIDInfoList[i].sCommonName :=
              aSID.AccountName[''] + ' (' + aSID.StringSID + ')';
          except
            on E: EJwsclSecurityException do
            begin
              SIDInfoList[i].sCommonName := '';

              ExClass := EJwsclSecurityExceptionClass(E.ClassType);
              SIDInfoList[i].Exception := ExClass.Create(E);
            end;
          end;
        finally
        end;
      end;

      try  //2.
        if Assigned(fOnLookupSIDs) then
          Result := fOnLookupSIDs(
            Self,//Sender : TJwSecurityDescriptorDialog;
            SIDList,//const SIDList : TJwSecurityIdList;
            SIDInfoList//var SIDInfoList : TJwSidInfoRecordArray) : Cardinal;
            );

        if (Length(SIDInfoList) = 0) or not Assigned(SIDList) or
          (Length(SIDInfoList) <> SIDList.Count) then
        begin
          FreeAndNil(SIDList);
          Result := E_NOTIMPL;
          exit;
        end;

        {fInfoList := PSID_INFO_LIST(LocalAlloc(LPTR,sizeof(SID_INFO_LIST)
                                + (sizeof(SID_INFO) * Length(SIDInfoList))
                                  ));}
        iLen := sizeof(SID_INFO_LIST) +
          (sizeof(SID_INFO) * (Length(SIDInfoList) + 2));
        //GetMem(fInfoList, iLen);
        fInfoList := PSID_INFO_LIST(GlobalAlloc(GHND, iLen));
        fInfoList := GlobalLock(Cardinal(fInfoList));

        fPSIDList := TList.Create;
        //manage PSID structures - remove in destroy of TJwSidInfoDataObject
        fSidStrings := TList.Create;
        try  //3. fInfoList := GlobalLock(Cardinal(fInfoList));
          ppdo := TJwSidInfoDataObject.Create(fInfoList, fPSIDList, fSidStrings);
          Result := S_OK;


          fInfoList.cItems := Length(SIDInfoList);

          for i := 0 to Length(SIDInfoList) - 1 do
          begin
            if Assigned(SIDInfoList[i].pSid) then
            begin
              fInfoList.aSidInfo[i].pSid :=
                TJwSecurityId(SIDInfoList[i].pSid).CreateCopyOfSID;
              fPSIDList.Add(fInfoList.aSidInfo[i].pSid);
            end;


            if Length(SIDInfoList[i].sCommonName) > 0 then
            begin
              GetMem(fInfoList.aSidInfo[i].pwzCommonName,
                (Length(SIDInfoList[i].sCommonName) + 1) * sizeof(widechar));
              CopyMemory(fInfoList.aSidInfo[i].pwzCommonName,
                @SIDInfoList[i].sCommonName[1],
                (Length(SIDInfoList[i].sCommonName)) * sizeof(widechar));
              fInfoList.aSidInfo[i].pwzCommonName[Length(
                SIDInfoList[i].sCommonName)] := #0;

              fSidStrings.Add(fInfoList.aSidInfo[i].pwzCommonName);
            end;

            if Length(SIDInfoList[i].sUPN) > 0 then
            begin
              GetMem(fInfoList.aSidInfo[i].pwzUPN,
                (Length(SIDInfoList[i].sUPN) + 1) * sizeof(widechar));
              CopyMemory(fInfoList.aSidInfo[i].pwzUPN,
                @SIDInfoList[i].sUPN[1], (Length(SIDInfoList[i].sUPN)) * sizeof(widechar));
              fInfoList.aSidInfo[i].pwzUPN[Length(SIDInfoList[i].sUPN)] := #0;

              fSidStrings.Add(fInfoList.aSidInfo[i].pwzUPN);
            end;

            fInfoList.aSidInfo[i].pwzClass := nil;

            if (SIDInfoList[i].sClass <> scnNone) then
            begin
              case SIDInfoList[i].sClass of
                scnComputer: s := 'Computer'; //Do not localize!!
                scnUser: s  := 'User';        //Do not localize!!
                scnGroup: s := 'Group';       //Do not localize!!
                scnUnknown:
                begin
                  s := 'Unknown'; //Do not localize!!
                  //Not working!
                                 {fPSIDList.Remove(fInfoList.aSidInfo[i].pSid);
                                 TJwSecurityId.FreeSID(fInfoList.aSidInfo[i].pSid);
                                 aSID := TJwSecurityId.Create('S-1-99-99-99');
                                 try
                                   fInfoList.aSidInfo[i].pSid := aSID.CreateCopyOfSID;
                                   fPSIDList.Add(fInfoList.aSidInfo[i].pSid);
                                 finally
                                   FreeAndNil(aSID);
                                 end;   }

                end;
              end;
              GetMem(fInfoList.aSidInfo[i].pwzClass,
                (Length(s) + 1) * sizeof(widechar));
              CopyMemory(fInfoList.aSidInfo[i].pwzClass, @s[1],
                (Length(s)) * sizeof(widechar));
              fInfoList.aSidInfo[i].pwzClass[Length(s)] := #0;

              fSidStrings.Add(fInfoList.aSidInfo[i].pwzClass);
            end;
          end;
        finally  //3: fInfoList := GlobalLock(Cardinal(fInfoList));
          GlobalUnlock(Cardinal(fInfoList));
        end;
      finally //2. call to fOnLookupSIDs(
      end;
    finally  //1. SIDList := TJwSecurityIdList.Create(true);
      FreeAndNil(SIDList);

      for i := 0 to cSids - 1 do
        FreeAndNil(SIDInfoList[i].Exception);

    end;
  end;
end;

function TJwSecurityDescriptorDialog.MapGeneric(pguidObjectType: LPGUID;
  pAceFlags: PUCHAR; pMask: PACCESS_MASK): HRESULT;
begin
  pMask^ := Mapping.GenericMap(pMask^);

  Result := S_OK;
end;




function NewWindowProc(WindowHandle: hWnd;
  Msg: longint;
  ParamW: longint;
  ParamL: longint): longint; stdcall;

var
  Rec: PJwMsgRec;
  bProcessDefaultProc: boolean;
begin
  Result := 0;

  Rec := PJwMsgRec(GetWindowLongPtr(WindowHandle, GWLP_USERDATA));

  if rec = nil then
    exit;


  if Assigned(Rec.Dlg.fOnWindowProcCallBack) then
  begin
    bProcessDefaultProc := True;
    Result := Rec.Dlg.fOnWindowProcCallBack(
      rec.Dlg,//Sender : TJwSecurityDescriptorDialog;
      TJwSecurityPageType(Rec.uPage),
      //PageType : TJwSecurityPageType;
      WindowHandle,//WindowHandle : hWnd;
      Msg,//Msg   : LongInt;
      ParamW,//ParamW       : LongInt;
      ParamL,//ParamL       : LongInt) : LongInt;
      bProcessDefaultProc);
    if not bProcessDefaultProc then
      exit;
  end;

  if Msg = WM_GETDLGCODE then
  begin
    Result := CallWindowProc(rec.pProc,
      WindowHandle, Msg,
      ParamW, ParamL);

    Result := Result or DLGC_DEFPUSHBUTTON or DLGC_BUTTON or
      DLGC_WANTMESSAGE;
    exit;
  end
  else
  if Msg = WM_COMMAND then
  begin
    {We get the result value :
      click OK or Cancel on the main ACL dialog
    }
    if (Rec.Dlg.WindowHandle = WindowHandle) and
      (Lo(ParamW) in [idOk, idCancel{,IDAPPLY}]) then
      Rec.Dlg.fResult := Lo(ParamW);

  end;
  (*if TheMessage = MyMsg  then begin
   {Tell the application to restore, let it restore the form}
    SendMessage(Application.handle, WM_SYSCOMMAND, SC_RESTORE, 0);
    SetForegroundWindow(Application.Handle);
   {We handled the message - we are done}
    Result := 0;
    exit;
  end;*)
  {Call the original winproc}
  Result := CallWindowProc(rec.pProc, WindowHandle,
    Msg, ParamW,
    ParamL);
end;

function TJwSecurityDescriptorDialog.AddNewPageRec(hwnd: HWND;
  uPage: SI_PAGE_TYPE): boolean;
var
  i: integer;
  Rec: PJwMsgRec;
begin
  Result := False;
  for i := 0 to ProcList.Count - 1 do
  begin
    Rec := ProcList[i];
    if (Rec.uPage = uPage) then
      exit;
  end;



  GetMem(Rec, Sizeof(TJwMsgRec));
  Rec.pProc := Pointer(GetWindowLongPtr(hwnd, GWLP_WNDPROC));
  Rec.uPage := uPage;
  Rec.Dlg := Self;
  REc.hHandle := hwnd;
  REc.userData := Pointer(GetWindowLongPtr(hwnd, GWLP_USERDATA));
  SetWindowLongPtr(hwnd, GWLP_USERDATA, integer(Rec));

  ProcList.Add(Rec);

  Result := True;
end;

function TJwSecurityDescriptorDialog.DeletePageRec(uPage: SI_PAGE_TYPE): boolean;
var
  i: integer;
  Rec: PJwMsgRec;
begin
  Result := False;
  for i := 0 to ProcList.Count - 1 do
  begin
    Rec := ProcList[i];
    if (Rec.uPage = uPage) then
    begin
      SetWindowLongPtr(Rec.hHandle, GWLP_WNDPROC, integer(Rec.pProc));
      FreeMem(Rec);
      ProcList.Delete(i);
      exit;
    end;
  end;
end;

function TJwSecurityDescriptorDialog.GetAdvWindowHandle(Page: TJwSecurityPageType): HWND;
var
  i: integer;
  Rec: PJwMsgRec;
begin
  Result := 0;
  for i := 0 to ProcList.Count - 1 do
  begin
    Rec := ProcList[i];
    if (Rec.uPage = SI_PAGE_TYPE(Page)) then
    begin
      Result := Rec.hHandle;
      exit;
    end;
  end;
end;

function TJwSecurityDescriptorDialog.PropertySheetPageCallback(aHwnd: HWND;
  uMsg: UINT; uPage: SI_PAGE_TYPE): HRESULT;
begin
 { result := E_NOTIMPL; //!!!!
  exit;  }
  if (uMsg = PSPCB_SI_INITDIALOG) then
  begin
    if (uPage = SI_PAGE_PERM) and (fWindowHandle = 0) then
    begin
      //fWindowHandle := GetActiveWindow;
      fWindowHandle := (GetParent(aHwnd)); //parent of tabsheet is main dialog
      if AddNewPageRec(fWindowHandle, SI_PAGE_TYPE(sptAclWindow)) then
        SetWindowLongPtr(fWindowHandle, GWLP_WNDPROC, integer(@NewWindowProc));

      if Assigned(fOnInitSecurityDialog) then
        fOnInitSecurityDialog(Self, sptAclWindow);
    end;

    if (uPage = SI_PAGE_ADVPERM) and (fAdvWindowHandle = 0) then
    begin
      fAdvWindowHandle := (GetParent(aHwnd));
      if AddNewPageRec(fAdvWindowHandle, SI_PAGE_TYPE(sptAdvWindow)) then
        SetWindowLongPtr(fAdvWindowHandle, GWLP_WNDPROC, integer(
          @NewWindowProc));

      if Assigned(fOnInitSecurityDialog) then
        fOnInitSecurityDialog(Self, sptAdvWindow);
    end;

    if Assigned(fOnInitSecurityDialog) then
      fOnInitSecurityDialog(Self, TJwSecurityPageType(uPage));

    if AddNewPageRec(aHwnd, uPage) then
      SetWindowLongPtr(aHwnd, GWLP_WNDPROC, integer(@NewWindowProc));
  end;

  if (uMsg = PSPCB_CREATE) then
  begin
    if Assigned(fOnInitSecurityPageCreate) then
      fOnInitSecurityPageCreate(Self, TJwSecurityPageType(uPage));
  end;

  if (uMsg = PSPCB_RELEASE) then
  begin
    if uPage = SI_PAGE_PERM then
    begin
      if Assigned(fOnInitSecurityPageDestroy) then
        fOnInitSecurityPageDestroy(Self, sptAclWindow);

      fWindowHandle := 0;
      DeletePageRec(SI_PAGE_TYPE(sptAclWindow));
    end;

    if uPage = SI_PAGE_ADVPERM then
    begin
      if Assigned(fOnInitSecurityPageDestroy) then
        fOnInitSecurityPageDestroy(Self, sptAdvWindow);

      fAdvWindowHandle := 0;
      DeletePageRec(SI_PAGE_TYPE(sptAdvWindow));
    end;

    DeletePageRec(uPage);


    if Assigned(fOnInitSecurityPageDestroy) then
      fOnInitSecurityPageDestroy(Self, TJwSecurityPageType(uPage));
  end;


  Result := S_OK;
end;


function TJwSecurityDescriptorDialog.SetSecurity(
  SecurityInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR): HRESULT;
var
  secInfo: TJwSecurityInformationFlagSet;
  secControl: TJwSecurityDescriptorControlSet;

  MergedSD, SD: TJwSecurityDescriptor;
  SecurityResetType: TJwSecurityResetType;
  secInfoEx: TJwSecurityDialogFlags;
  bSuccess:  boolean;

begin
  secInfo := TJwEnumMap.ConvertSecurityInformation(
    SecurityInformation);
  secControl := TJwEnumMap.ConvertSecurityControl(
    pSecurityDescriptor.Control);
  secInfoEx := TJwEnumMap.ConvertFlags(SecurityInformation);


  //create new SD that contains the changed security information
  SD := TJwSecurityDescriptor.Create(pSecurityDescriptor);

  //create new SD that contains the default SD and will receive the new one
  MergedSD := TJwSecurityDescriptor.Create(fSD);

  //replace Owner
  if siOwnerSecurityInformation in secInfo then
  begin
    MergedSD.OwnOwner := True;
    MergedSD.Owner := SD.Owner; //simply copy owner
  end;

  //replace group
  if siGroupSecurityInformation in secInfo then
  begin
    MergedSD.OwnPrimaryGroup := True;
    MergedSD.PrimaryGroup := SD.PrimaryGroup; //simply copy group
  end;

  //replace DACL
  if siDaclSecurityInformation in secInfo then
  begin
    MergedSD.OwnDACL := True;
    MergedSD.DACL := SD.DACL; //simply copy DACL
  end;

  //replace SACL
  if siSaclSecurityInformation in secInfo then
  begin
    MergedSD.SACL := SD.SACL; //simply copy SACL
  end;


  if siDaclSecurityInformation in secInfo then
  begin
    //protect DACL from inheritance
    if sdcDaclProtected in SD.Control then
    begin
      Include(secInfo, siProtectedDaclSecurityInformation);
      MergedSD.Control := MergedSD.Control + [sdcDaclProtected];
    end;

    //unprotect DACL so inheritance can flow
    if sdcDaclAutoInheritReq in SD.Control then
    begin
      Include(secInfo, siUnprotectedDaclSecurityInformation);
      MergedSD.Control := MergedSD.Control + [sdcDaclAutoInheritReq];
    end;
  end;

  if siSaclSecurityInformation in secInfo then
  begin
    //protect SACL from inheritance
    if sdcSaclProtected in SD.Control then
    begin
      Include(secInfo, siProtectedSaclSecurityInformation);
      MergedSD.Control := MergedSD.Control + [sdcSaclProtected];
    end;

    //unprotect SACL so inheritance can flow
    if sdcSaclAutoInheritReq in SD.Control then
    begin
      Include(secInfo, siUnprotectedSaclSecurityInformation);
      MergedSD.Control := MergedSD.Control + [sdcSaclAutoInheritReq];
    end;
  end;

  //set reset state, so user can easily see whether he must reset all children
  SecurityResetType := srtNone;
  if (SecurityInformation and SI_OWNER_RECURSE = SI_OWNER_RECURSE) then
    SecurityResetType := srtOwner
  else
  if (SecurityInformation and SI_RESET_DACL_TREE = SI_RESET_DACL_TREE) then
    SecurityResetType := srtDacl
  else
  if (SecurityInformation and SI_RESET_SACL_TREE = SI_RESET_SACL_TREE) then
    SecurityResetType := srtSacl;


  bSuccess := False;
  try
    if Assigned(fOnSetSecurity) then
      fOnSetSecurity(Self, secInfo, secInfoEx,
        SecurityResetType,
        SD.Control, SD, MergedSD, bSuccess);
    if bSuccess then
      fSD.Assign(MergedSD);
  finally
    SD.Free;
    MergedSD.Free;
  end;

  if bSuccess then
    Result := S_OK
  else
    Result := S_FALSE;
end;



constructor TJwSecurityDescriptorDialog.Create(hParent: HWND);

  procedure InitInheritType;
  begin
    //Default values
    fInheritTypeList.Add(GUID_NULL, [], RsSecurityDialogsInheritanceThisDefault);
    fInheritTypeList.Add(GUID_NULL, [afContainerInheritAce,
      afObjectInheritAce], RsSecurityDialogsInheritanceObjectContainerDefault);
    fInheritTypeList.Add(GUID_NULL, [afContainerInheritAce],
      RsSecurityDialogsInheritanceContainerDefault);
    fInheritTypeList.Add(GUID_NULL, [afObjectInheritAce],
      RsSecurityDialogsInheritanceObjectDefault);
    fInheritTypeList.Add(GUID_NULL, [afInheritOnlyAce,
      afContainerInheritAce, afObjectInheritAce],
      RsSecurityDialogsInheritanceOnlyObjectContainerDefault);
    fInheritTypeList.Add(GUID_NULL, [afInheritOnlyAce,
      afContainerInheritAce], RsSecurityDialogsInheritanceOnlyContainerDefault);
    fInheritTypeList.Add(GUID_NULL, [afInheritOnlyAce,
      afObjectInheritAce], RsSecurityDialogsInheritanceOnlyObjectDefault);

  end;

begin
  inherited Create;
  FRefCount := 0;

  fSD  := TJwSecurityDescriptor.Create;
  fParent := hParent;
  pTempAccess := nil;
  iTempAccess := 0;
  fTag := nil;

  ProcList := TList.Create;

  fInstance  := HINSTANCE;
  fServerName := '';
  fObjectName := '';
  fPageTitle := '';
  FillChar(fObjectType, sizeof(fObjectType), 0);

  fResult := idCancel;

  Flags := [sdfEditDacl];
  fWindowHandle := 0;

  fOnInitSecurityDialog := nil;
  fOnInitSecurityPageCreate := nil;
  fOnInitSecurityPageDestroy := nil;
  fOnGetInheriteSource := nil;
  fOnGetEffectivePermissions := nil;
  fOnWindowProcCallBack := nil;
  fOnLookupSIDs := nil;


  fObjectTypeList := nil;

  sPageTitle  := nil;
  sObjectName := nil;
  sServerName := nil;


  fInheritTypeList := TJwInheritTypeList.Create;
  InitInheritType;
end;

destructor TJwSecurityDescriptorDialog.Destroy;

  procedure DoneStringPool;
  begin
    if sPageTitle <> nil then
      FreeMem(sPageTitle);
    if sObjectName <> nil then
      FreeMem(sObjectName);
    if sServerName <> nil then
      FreeMem(sServerName);
  end;

begin
  Mapping.FreeAccessNames(pTempAccess, iTempAccess);
  FreeAndNil(fSD);
  FreeAndNil(fInheritTypeList);
  FreeAndNil(ProcList);
  SetLength(fObjectTypeList, 0);

  DoneStringPool;

  inherited;
end;

function TJwSecurityDescriptorDialog.GetDACL: TJwDAccessControlList;
begin
  Result := fSD.DACL;
end;

function TJwSecurityDescriptorDialog.GetOwner: TJwSecurityId;
begin
  Result := fSD.Owner;
end;

function TJwSecurityDescriptorDialog.GetSACL: TJwSAccessControlList;
begin
  Result := fSD.SACL;
end;

procedure TJwSecurityDescriptorDialog.SetSACL(aSACL: TJwSAccessControlList);
begin
  fSD.SACL := aSACL;
end;

function TJwSecurityDescriptorDialog.GetSecurityDescriptor: TJwSecurityDescriptor;
begin
  Result := fSD;
end;

procedure TJwSecurityDescriptorDialog.SetDACL(aDACL: TJwDAccessControlList);
begin
  fSD.OwnDACL := True;
  fSD.DACL := aDACL;
end;

procedure TJwSecurityDescriptorDialog.SetOwner(anOwner: TJwSecurityId);
begin
  fSD.OwnOwner := True;
  fSD.Owner := anOwner;
end;



procedure TJwSecurityDescriptorDialog.SetSecurityDescriptor(aSD: TJwSecurityDescriptor);
begin
  if not Assigned(aSD) then
  begin
    fSD.Free;
    fSD := TJwSecurityDescriptor.Create;
  end;
  fSD.Assign(aSD);
end;

function TJwSecurityDescriptorDialog.ShowModal: boolean;
begin
  _AddRef;
  try
    EditSecurity(GetActiveWindow, Self);
  finally
    _Release;
  end;
  Result := fResult = idOk;
end;




{TODO: test for D5}
procedure TJwSecurityDescriptorDialog.SetInheritTypeList(aList: TJwInheritTypeList);
{.$UNDEF DELPHI_6_UP}
{$IFNDEF DELPHI_6_UP}
var
   i : Integer;         
   G : TGuid;
{$ENDIF DELPHI_6_UP}
begin
{$IFDEF FPC}
  fInheritTypeList.Assign(aList);
{$ELSE}

{$IFNDEF DELPHI_6_UP}
  fInheritTypeList.Clear;
  for i := 0 to aList.Count -1 do
  begin
    G := NULL_GUID;
    if aList[i].pguid <> nil then
      G := aList[i].pguid^;

    fInheritTypeList.Add(G,
        TJwSecurityAccessControlEntry.ConvertAceCardinalToAceFlagSet(aList[i].dwFlags),
        aList[i].pszName);
  end;
{$ELSE}
  fInheritTypeList.Assign(aList);
{$ENDIF DELPHI_6_UP}
{$ENDIF FPC}


end;

const
  defaultObjectTypeList: OBJECT_TYPE_LIST =
    (Level: 0;
    Sbz: 0;
    ObjectType: @GUID_NULL);

  GUID_1: TGUID = (D1: 1; D2: 2; D3: 1; D4: (1, 2, 3, 4, 5, 6, 7, 8));

  defaultObjectTypeList_2: OBJECT_TYPE_LIST =
    (Level: ACCESS_PROPERTY_GUID;
    Sbz: 0;
    ObjectType: @GUID_1);


function TJwSecurityDescriptorDialog.GetEffectivePermission(
  const pguidObjectType: TGUID;
  pUserSid: PSID; pszServerName: LPCWSTR; pSD: PSECURITY_DESCRIPTOR;
  var ppObjectTypeList: POBJECT_TYPE_LIST; var pcObjectTypeListLength: ULONG;
  var ppGrantedAccessList: PACCESS_MASK;
  var pcGrantedAccessListLength: ULONG): HRESULT;
var
  SD: TJwSecurityDescriptor;
  pDACL: PACL;
  UserSID: TJwSecurityId;
  AccessRights: ACCESS_MASK;
  i, err: Cardinal;
  aTrustee: TTrusteeEx;

  GrantedAccessList: TJwAccessMaskArray;
begin
 { result := E_NOTIMPL; //!!!!
  exit;   }
  SD := TJwSecurityDescriptor.Create(pSD);

  try
    if (Assigned(SD.DACL)) then
    begin
      UserSID := TJwSecurityId.Create(pUserSid);
      pDACL := SD.DACL.Create_PACL;

      aTrustee := UserSID.Trustee;


      //FreeAndNil(SD);
   {$IFDEF UNICODE}
      err := GetEffectiveRightsFromAclW
   {$ELSE}
      err := GetEffectiveRightsFromAclA
   {$ENDIF}
        (pDACL, @aTrustee, AccessRights);

      SD.DACL.Free_PACL(pDACL);

      if (err = 0) then
      begin
        SetLength(GrantedAccessList, 1);
        GrantedAccessList[0] := AccessRights;
      end
      else
        SetLength(GrantedAccessList, 0);

      SetLength(fObjectTypeList, 1);

      fObjectTypeList[0] := defaultObjectTypeList;

      if Assigned(fOnGetEffectivePermissions) then
      begin
        try
          err := fOnGetEffectivePermissions(Self, // Sender : TJwSecurityDescriptorDialog;
            pguidObjectType,//const GuidObjectType: TGUID;
            WideString(pszServerName),
            //const sServerName : WideString;
            SD,//const SD : TJwSecurityDescriptor
            fObjectTypeList,
            //var ObjectTypeList : TJwObjectTypeListArray;
            GrantedAccessList
            //var GrantedAccessList : TJwAccessMaskArray) : Cardinal;
            );
        finally
          FreeAndNil(SD);
          FreeAndNil(UserSID);
          SetLength(fObjectTypeList, 0);
        end;
      end;

      FreeAndNil(SD);
      FreeAndNil(UserSID);

      if (err <> 0) or (Length(GrantedAccessList) = 0) then
      begin
        Result := err;
        exit;
      end;

      ppGrantedAccessList :=
        PACCESS_MASK(LocalAlloc(LPTR, sizeof(PACCESS_MASK) +
        (sizeof(ACCESS_MASK) * Length(GrantedAccessList))));
      //ppGrantedAccessList^ := AccessRights;
      for i := 0 to Length(GrantedAccessList) - 1 do
      begin
        TJwAccessMaskArray(ppGrantedAccessList)[i] := GrantedAccessList[i];
      end;
      pcGrantedAccessListLength := Length(GrantedAccessList);

      pcObjectTypeListLength := Length(fObjectTypeList);
     { SetLength(TJwObjectTypeListArray(ppObjectTypeList), pcObjectTypeListLength);
      for i := 0 to Length(ObjectTypeList)-1 do
      begin
        TJwObjectTypeListArray(ppObjectTypeList)[i] := ObjectTypeList[i];
      end;    }
      ppObjectTypeList := @fObjectTypeList;


      Result := err;
    end
    else
      Result := S_FALSE;
  finally
    FreeAndNil(SD);
  end;
end;

function TJwSecurityDescriptorDialog.GetInheritSource(si: SECURITY_INFORMATION;
  pACL: PACL; var ppInheritArray: PInheritedFromW): HRESULT;

  function GetInheritanceStringSize(anArray: TJwInheritedFromArray): Cardinal;
  var
    i: integer;
  begin
    Result := 0;
    for i := 0 to Length(anArray) - 1 do
    begin
      Inc(Result, Length(anArray[i].AncestorName) + 1);
    end;
  end;

type
  SInheritedFromArray = array of
    TInheritedFromW;{do not change to TInheritedFromA}

var
  ACL:  TJwSecurityAccessControlList;
  Info: TJwSecurityInformationFlagSet;
  InheritanceArray: TJwInheritedFromArray;
  i, i2, iSize: Cardinal;

  DataPtr: PWideChar;
begin
  Result := E_NOTIMPL;

  Info := TJwEnumMap.ConvertSecurityInformation(si);


  if not Assigned(fOnGetInheriteSource) then
    exit;


  if (siDaclSecurityInformation in Info) then
    ACL := TJwDAccessControlList.Create(pACL)
  else
  if (siSaclSecurityInformation in Info) then
    ACL := TJwSAccessControlList.Create(pACL)
  else
    ACL := nil;

  if ACL = nil then
    exit;

  SetLength(InheritanceArray, ACL.Count);
 { for i := 0 to ACL.Count -1 do
  begin
    FillChar(InheritanceArray[i],sizeof(InheritanceArray[i]),0);
  end;
       }
  try
    Result := fOnGetInheriteSource(Self, //Sender : TJwSecurityDescriptorDialog;
      Info,//const Info : SECURITY_INFORMATION;
      ACL, //const ACL : TJwSecurityAccessControlList;
      InheritanceArray   //var InheritanceArray : TJwInheritedFromArray
      );
    ppInheritArray := nil;


    if Assigned(InheritanceArray) and (Result = S_OK) then
    begin
      iSize := GetInheritanceStringSize(InheritanceArray);
      {Create whole block that contains the whole array PInheritedFromW
       and a block of strings which is right past the array.
       All strings in the PInheritedFromW structure points to the second block.
       Because the system frees the whole block, we do not need to free it ourself.

       +1 adds zero space
      }
      ppInheritArray := PInheritedFromW(LocalAlloc(LPTR,
        (ACL.Count + 1) * sizeof(TInheritedFromW) + iSize *
        sizeof(widechar)));

      //lots of pointer hacking frome here on!   
      //string block    
      DataPtr := Pointer(Cardinal(ppInheritArray) + ACL.Count *
        sizeof(TInheritedFromW));

      i2 := Length(InheritanceArray);
      for i := 0 to i2 - 1 do
      begin
        i2 := (Length(InheritanceArray[i].AncestorName)) * sizeof(widechar);
        if (i2 > 0) then
          CopyMemory(DataPtr,
            @WideString(InheritanceArray[i].AncestorName)[1], i2);

        SInheritedFromArray(ppInheritArray)[i].GenerationGap :=
          InheritanceArray[i].GenerationGap;
        if i2 > 0 then
          SInheritedFromArray(ppInheritArray)[i].AncestorName :=
            PWideChar(DataPtr)
        else
          SInheritedFromArray(ppInheritArray)[i].AncestorName := nil;

        Inc(DataPtr, (Length(InheritanceArray[i].AncestorName) + 1));
      end;
    end;
  finally
    FreeAndNil(ACL);
  end;

end;


function TJwSecurityDescriptorDialog._AddRef: integer;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TJwSecurityDescriptorDialog._Release: integer;
begin
  if FRefCount > 0 then
    Dec(FRefCount);
  Result := 1;
end;

function TJwSecurityDescriptorDialog.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := S_OK;
  if not GetInterface(IID, obj) then
    Result := E_NOINTERFACE;
end;

{ TJwInheritTypeList }

function TJwInheritTypeList.Add(const GUID: TGUID; const Flags: TJwAceFlags;
  const Name: WideString): integer;
begin
  Result := inherited Add(NewRecord(GUID, Flags, Name));
end;

procedure TJwInheritTypeList.Delete(Index: integer);
var
  p: PSiInheritType;
begin
  p := Get(Index);

  {LocalFree(Cardinal(p^.pguid));
  LocalFree(Cardinal(p^.pszName));
  LocalFree(Cardinal(p));  }

  FreeMem(p^.pguid);
  FreeMem(p^.pszName);
  FreeMem(p);

  inherited Delete(Index);
end;

destructor TJwInheritTypeList.Destroy;
var
  i: integer;
begin
  if fData <> nil then
    LocalFree(Cardinal(fData));

  for i := Count - 1 downto 0 do
    Delete(i);
  inherited;
end;

function TJwInheritTypeList.First: PSiInheritType;
begin
  Result := inherited First;
end;

function TJwInheritTypeList.Get(Index: integer): PSiInheritType;
begin
  Result := inherited Get(Index);
end;

function TJwInheritTypeList.GetArray: PSI_INHERIT_TYPE;
var
  i: integer;
  SiPointer : PSI_INHERIT_TYPE;
begin
  if fData <> nil then
    LocalFree(Cardinal(fData));

  Result := PSI_INHERIT_TYPE(LocalAlloc(LPTR, Count*sizeof(SI_INHERIT_TYPE)));
  fData  := Result;
  SiPointer := Result;

  for i := 0 to Count - 1 do
  begin
    SiPointer.pguid := Items[i].pguid;
    SiPointer.dwFlags := Items[i].dwFlags;
    SiPointer.pszName := Items[i].pszName;
    Inc(SiPointer);
  end;

end;

function TJwInheritTypeList.IndexOf(Item: PSiInheritType): integer;
begin
  Result := inherited IndexOf(Item);
end;

procedure TJwInheritTypeList.Insert(Index: integer; const GUID: TGUID;
  const Flags: TJwAceFlags; const Name: WideString);

begin
  inherited Insert(Index, NewRecord(GUID, Flags, Name));
end;

function TJwInheritTypeList.Last: PSiInheritType;
begin
  Result := inherited Last;
end;

function TJwInheritTypeList.NewRecord(const GUID: TGUID;
  const Flags: TJwAceFlags; const Name: WideString): PSiInheritType;
var
  p: PSiInheritType;
begin
  GetMem(p, sizeof(TSiInheritType));
  //p := PSiInheritType(LocalAlloc(LMEM_ZEROINIT,sizeof(TSiInheritType)));
  FillChar(p^, sizeof(TSiInheritType), 0);
  if Length(Name) > 0 then
  begin
    GetMem(p^.pszName, (Length(Name) + 1) * sizeof(widechar));
    FillChar(p^.pszName^, (Length(Name) + 1) * sizeof(widechar), 0);
    //p^.pszName := PWideChar(LocalAlloc(LMEM_ZEROINIT,Length(Name)*2+2));
    CopyMemory(p^.pszName, @Name[1], Length(Name) * sizeof(widechar));
  end;

  GetMem(p^.pguid, sizeof(TGUID));
  //p^.pguid := LPGUID(LocalAlloc(LMEM_ZEROINIT,sizeof(TGUID)));
  CopyMemory(p^.pguid, @GUID, sizeof(TGUID));

  p^.dwFlags := TJwEnumMap.ConvertAceFlags(Flags);

  Result := p;
end;

procedure TJwInheritTypeList.Put(Index: integer; Data: PSiInheritType);
var
  p: PSiInheritType;
begin
  p := Get(Index);

  {LocalFree(Cardinal(p^.pguid));
  LocalFree(Cardinal(p^.pszName));
  LocalFree(Cardinal(p)); }

  FreeMem(p^.pguid);
  FreeMem(p^.pszName);
  FreeMem(p);

  inherited Put(Index, Data);
end;



{ TJwSidInfoDataObject }
//function TJwSidInfoDataObject.GetData(formatetcIn: PFormatEtc; medium: PStgMedium):  HRESULT; stdcall;
function TJwSidInfoDataObject.GetData(const formatetcIn: TFormatEtc;
  out medium: TStgMedium): HRESULT; stdcall;

var
  aPSidList: PSID_INFO_LIST;
  i, cnt: integer;
  p1, p2, p3: PWideChar;
  //function TJwSidInfoDataObject.GetData(const formatetcIn: jwaWindows.TFormatEtc; out medium: jwaWindows.TStgMedium): HRESULT; stdcall;
begin
  FillChar(medium, sizeof(TStgMedium), 0);
  medium.hGlobal := Cardinal(fInfoList);
//  medium.unkForRelease := nil;
  medium.tymed := TYMED_HGLOBAL;


  aPSidList := PSID_INFO_LIST(GlobalLock(medium.hGlobal));
  cnt := aPSidList.cItems;

  for i := 0 to cnt - 1 do
  begin
    p1 := aPSidList.aSidInfo[i].pwzCommonName;
    p2 := aPSidList.aSidInfo[i].pwzClass;
    p3 := aPSidList.aSidInfo[i].pwzUPN;
  end;

  GlobalUnLock(medium.hGlobal);
 {
  FillChar(medium, sizeof(TStgMedium), 0);
  medium.hGlobal := GlobalAlloc(GHND,123);
  medium.unkForRelease := nil;
  medium.tymed := TYMED_HGLOBAL;
                                  }
  //  ReleaseStgMedium(medium);

  Result := S_OK;
end;

constructor TJwSidInfoDataObject.Create(pInfoList: PSID_INFO_LIST;
  aPSIDList, aSidStrings: TList);
begin
  inherited Create;
  fInfoList := pInfoList;
  fPSIDList := aPSIDList;
  fSidStrings := aSidStrings;
end;


destructor TJwSidInfoDataObject.Destroy;
var
  i: integer;
  apSID: PSID;
  p: Pointer;
begin
  // GlobalFree(Cardinal(fInfoList));
  fInfoList := nil;

  if Assigned(fPSIDList) then
    for i := 0 to fPSIDList.Count - 1 do
    begin
      apSID := PSID(fPSIDList[i]);
      TJwSecurityId.FreeSID(apSID);
      fPSIDList[i] := nil;
    end;
  FreeAndNil(fPSIDList);

  if Assigned(fSidStrings) then
    for i := 0 to fSidStrings.Count - 1 do
    begin
      p := fSidStrings[i];
      FreeMem(p);
      fSidStrings[i] := nil;
    end;
  FreeAndNil(fSidStrings);


  inherited;
end;




function TJwSidInfoDataObject.GetDataHere(const formatetc: TFormatEtc;
  out medium: TStgMedium): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TJwSidInfoDataObject.QueryGetData(
  const formatetc: TFormatEtc): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TJwSidInfoDataObject.GetCanonicalFormatEtc(const formatetc: TFormatEtc;
  out formatetcOut: TFormatEtc): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TJwSidInfoDataObject.SetData(const formatetc: TFormatEtc;
  var medium: TStgMedium; fRelease: BOOL): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TJwSidInfoDataObject.EnumFormatEtc(dwDirection: {$IFDEF FPC}DWORD{$ELSE}Longint{$ENDIF FPC};
  out enumFormatEtc: IEnumFORMATETC): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TJwSidInfoDataObject.DAdvise(const formatetc: TFormatEtc;
  advf: {$IFDEF FPC}DWORD{$ELSE}Longint{$ENDIF FPC}; const advSink: IAdviseSink;
  out dwConnection: {$IFDEF FPC}DWORD{$ELSE}Longint{$ENDIF FPC}): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TJwSidInfoDataObject.DUnadvise(dwConnection: {$IFDEF FPC}DWORD{$ELSE}Longint{$ENDIF FPC}): HRESULT;
  stdcall;
begin
  Result := E_NOTIMPL;
end;

function TJwSidInfoDataObject.EnumDAdvise(
  out enumAdvise: IEnumSTATDATA): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;




{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}



initialization
{$ENDIF SL_OMIT_SECTIONS}
{$IFNDEF SL_INITIALIZATION_SECTION}


  //x := TJwSidInfoDataObject.Create();

{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
finalization
{$ENDIF SL_OMIT_SECTIONS}
{$IFNDEF SL_FINALIZATION_SECTION}

{$ENDIF SL_FINALIZATION_SECTION}



{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}
