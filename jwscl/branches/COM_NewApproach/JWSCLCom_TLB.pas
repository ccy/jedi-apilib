unit JWSCLCom_TLB;

// ************************************************************************ //
// WARNUNG                                                                    
// -------                                                                    
// Die in dieser Datei deklarierten Typen wurden aus Daten einer Typbibliothek
// generiert. Wenn diese Typbibliothek explizit oder indirekt (über eine     
// andere Typbibliothek) reimportiert wird oder wenn die Anweisung            
// 'Aktualisieren' im Typbibliotheks-Editor während des Bearbeitens der     
// Typbibliothek aktiviert ist, wird der Inhalt dieser Datei neu generiert und 
// alle manuell vorgenommenen Änderungen gehen verloren.                           
// ************************************************************************ //

// PASTLWTR : 1.2
// Datei generiert am 26.02.2008 22:59:54 aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib: P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\jedi-api-lib\jwscl\trunk\COM\JWSCLCom.tlb (1)
// LIBID: {9EBCE2EF-4E69-4AC3-AA7F-F021E119E8BB}
// LCID: 0
// Hilfedatei: 
// Hilfe-String: JWSCLCom Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit muß ohne Typüberprüfung für Zeiger compiliert werden. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// In dieser Typbibliothek deklarierte GUIDS . Es werden folgende         
// Präfixe verwendet:                                                     
//   Typbibliotheken     : LIBID_xxxx                                     
//   CoClasses           : CLASS_xxxx                                     
//   DISPInterfaces      : DIID_xxxx                                      
//   Nicht-DISP-Schnittstellen: IID_xxxx                                       
// *********************************************************************//
const
  // Haupt- und Nebenversionen der Typbibliothek
  JWSCLComMajorVersion = 1;
  JWSCLComMinorVersion = 0;

  LIBID_JWSCLCom: TGUID = '{9EBCE2EF-4E69-4AC3-AA7F-F021E119E8BB}';

  IID_IJwSid: TGUID = '{5134BF4E-3D59-44FF-A273-C052BB9B64DE}';
  IID_IJwSidList: TGUID = '{C88787BF-0091-46F2-A732-0639244C54E5}';
  CLASS_JwSid: TGUID = '{95B55CCE-E93B-4DE1-AEBA-18E006FD06F6}';
  CLASS_JwSidList: TGUID = '{6F10E768-10B0-44E6-A14E-30C905B5B01E}';
  IID_IJwTest: TGUID = '{392DA373-0FE2-45D5-AF98-89BCC8D2D0CD}';
  CLASS_JwTest2: TGUID = '{6859DD54-E45C-4857-B63C-443D0B5D57E4}';
  IID_IJwAccessControlList: TGUID = '{41FB5FBD-E101-4A32-94AB-BD463E1536BE}';
  CLASS_JwAccessControlList: TGUID = '{99AF508A-33AA-439B-993E-4D771F0C4334}';
  IID_IJwAccessControlEntry: TGUID = '{FEB78113-1787-408A-B877-7063F23604ED}';
  IID_IJwSecurityDescriptor: TGUID = '{497C3246-F614-476B-874E-EB6FFC4571E3}';
  CLASS_JwSecurityDescriptor: TGUID = '{F59BD13A-5080-4027-8104-EFCF8A2B5F0A}';
  IID_IJwToken: TGUID = '{7115952F-3B76-4CE1-B052-E4D38D4CCC09}';
  CLASS_JwToken: TGUID = '{70CC88CE-7C38-4B8D-A782-D808E38A983D}';
  IID_IJwPrivilegeList: TGUID = '{D7B09407-170C-4407-A51B-500F9479CC15}';
  CLASS_JwPrivilegeList: TGUID = '{E6B61F1E-10E4-4F3E-9E69-98286ACD900B}';
  IID_IJwPrivilege: TGUID = '{FCA8F64B-B76D-45B4-A010-E3F1134AE1A9}';
  IID_IJwWindowsVersion: TGUID = '{632F08B7-7D3B-4865-B17A-00B97FBE3D9B}';
  CLASS_JwWindowsVersion: TGUID = '{8D6C4561-E2A7-4F91-8B28-2EB458B75318}';
  IID_IJwGenericMapping: TGUID = '{DD1015C5-9E4D-4062-8E0C-E2606705673A}';
  IID_IJwFileFolderMapping: TGUID = '{B713F708-5096-4A0B-868C-BF0B5451E54A}';

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten Enumerationen         
// *********************************************************************//
// Konstanten für enum JwCoFacilities
type
  JwCoFacilities = TOleEnum;
const
  FACILITY_JWSCL = $00000065;

// Konstanten für enum JwEnumDescriptorElements
type
  JwEnumDescriptorElements = TOleEnum;
const
  deOwner = $00000000;
  deGroup = $00000001;
  deDacl = $00000002;
  deSacl = $00000003;

// Konstanten für enum JwEnumAclProtectionState
type
  JwEnumAclProtectionState = TOleEnum;
const
  psUnprotected = $00000000;
  psProtected = $00000001;
  psForceUnprotect = $00000002;

// Konstanten für enum JwIntegrityLevelType
type
  JwIntegrityLevelType = TOleEnum;
const
  ltNone = $00000000;
  ltLow = $00000001;
  ltMedium = $00000002;
  ltHigh = $00000003;
  ltSystem = $00000004;
  ltProtected = $00000005;

type

// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen         
// *********************************************************************//
  IJwSid = interface;
  IJwSidDisp = dispinterface;
  IJwSidList = interface;
  IJwSidListDisp = dispinterface;
  IJwTest = interface;
  IJwTestDisp = dispinterface;
  IJwAccessControlList = interface;
  IJwAccessControlListDisp = dispinterface;
  IJwAccessControlEntry = interface;
  IJwAccessControlEntryDisp = dispinterface;
  IJwSecurityDescriptor = interface;
  IJwSecurityDescriptorDisp = dispinterface;
  IJwToken = interface;
  IJwTokenDisp = dispinterface;
  IJwPrivilegeList = interface;
  IJwPrivilegeListDisp = dispinterface;
  IJwPrivilege = interface;
  IJwPrivilegeDisp = dispinterface;
  IJwWindowsVersion = interface;
  IJwWindowsVersionDisp = dispinterface;
  IJwGenericMapping = interface;
  IJwGenericMappingDisp = dispinterface;
  IJwFileFolderMapping = interface;
  IJwFileFolderMappingDisp = dispinterface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass zu ihrer Standardschnittstelle        
// zugewiesen)                                                            
// *********************************************************************//
  JwSid = IJwSid;
  JwSidList = IJwSidList;
  JwTest2 = IJwTest;
  JwAccessControlList = IJwAccessControlList;
  JwSecurityDescriptor = IJwSecurityDescriptor;
  JwToken = IJwToken;
  JwPrivilegeList = IJwPrivilegeList;
  JwWindowsVersion = IJwWindowsVersion;


// *********************************************************************// 
// Deklaration von  Strukturen, Unions und Aliasen.                        
// *********************************************************************// 
  PWideString1 = ^WideString; {*}

  PCoSid = PChar; 
  PCoSidAndAttributes = PChar; 
  PCoTokenGroups = PChar; 

  TGenericMapping = packed record
    GenericRead: LongWord;
    GenericWrite: LongWord;
    GenericExecute: LongWord;
    GenericAll: LongWord;
  end;


// *********************************************************************//
// Schnittstelle: IJwSid
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5134BF4E-3D59-44FF-A273-C052BB9B64DE}
// *********************************************************************//
  IJwSid = interface(IDispatch)
    ['{5134BF4E-3D59-44FF-A273-C052BB9B64DE}']
    procedure InitByBinarySid(const BinarySid: WideString); safecall;
    procedure InitByStream(const SidAsStream: IUnknown); safecall;
    procedure InitByName(const SystemName: WideString; const AccountName: WideString); safecall;
    procedure InitByJwSid(const Sid: IJwSid); safecall;
    procedure InitByWellKnownSid(SidType: SYSUINT); safecall;
    procedure InitByAuthorities(Authorities: OleVariant; Identifier: OleVariant); safecall;
    function IsStandardSid: WordBool; safecall;
    function GetInternalObject: PChar; safecall;
    function Get_SubAuthorityCount: SYSUINT; safecall;
    function Get_SubAuthorityArray: OleVariant; safecall;
    function Get_IdentifierAttributesCount: SYSUINT; safecall;
    function Get_IdentifierAttributesArray: OleVariant; safecall;
    function Get_IsWellKnownSidType: WordBool; safecall;
    function GetAccountName(const SystemName: WideString): WideString; safecall;
    function Get_CachedSystemName: WideString; safecall;
    procedure Set_CachedSystemName(const Value: WideString); safecall;
    function GetAccountDomainName(const SystemName: WideString): WideString; safecall;
    function GetAccountNameInUse(const SystemName: WideString): LongWord; safecall;
    function GetCachedUserName: WideString; safecall;
    function Get_Attributes: LongWord; safecall;
    procedure Set_Attributes(Value: LongWord); safecall;
    function Get_AttributesByType: OleVariant; safecall;
    procedure Set_AttributesByType(Value: OleVariant); safecall;
    function GetStream: IUnknown; safecall;
    function Get_UserName: WideString; safecall;
    function IsEqualSid(const Sid: IJwSid): WordBool; safecall;
    function Get_StringSid: WideString; safecall;
    function ToString: WideString; safecall;
    property SubAuthorityCount: SYSUINT read Get_SubAuthorityCount;
    property SubAuthorityArray: OleVariant read Get_SubAuthorityArray;
    property IdentifierAttributesCount: SYSUINT read Get_IdentifierAttributesCount;
    property IdentifierAttributesArray: OleVariant read Get_IdentifierAttributesArray;
    property IsWellKnownSidType: WordBool read Get_IsWellKnownSidType;
    property CachedSystemName: WideString read Get_CachedSystemName write Set_CachedSystemName;
    property Attributes: LongWord read Get_Attributes write Set_Attributes;
    property AttributesByType: OleVariant read Get_AttributesByType write Set_AttributesByType;
    property UserName: WideString read Get_UserName;
    property StringSid: WideString read Get_StringSid;
  end;

// *********************************************************************//
// DispIntf:  IJwSidDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5134BF4E-3D59-44FF-A273-C052BB9B64DE}
// *********************************************************************//
  IJwSidDisp = dispinterface
    ['{5134BF4E-3D59-44FF-A273-C052BB9B64DE}']
    procedure InitByBinarySid(const BinarySid: WideString); dispid 201;
    procedure InitByStream(const SidAsStream: IUnknown); dispid 202;
    procedure InitByName(const SystemName: WideString; const AccountName: WideString); dispid 203;
    procedure InitByJwSid(const Sid: IJwSid); dispid 204;
    procedure InitByWellKnownSid(SidType: SYSUINT); dispid 205;
    procedure InitByAuthorities(Authorities: OleVariant; Identifier: OleVariant); dispid 206;
    function IsStandardSid: WordBool; dispid 207;
    function GetInternalObject: {??PChar}OleVariant; dispid 208;
    property SubAuthorityCount: SYSUINT readonly dispid 209;
    property SubAuthorityArray: OleVariant readonly dispid 210;
    property IdentifierAttributesCount: SYSUINT readonly dispid 211;
    property IdentifierAttributesArray: OleVariant readonly dispid 213;
    property IsWellKnownSidType: WordBool readonly dispid 212;
    function GetAccountName(const SystemName: WideString): WideString; dispid 214;
    property CachedSystemName: WideString dispid 215;
    function GetAccountDomainName(const SystemName: WideString): WideString; dispid 216;
    function GetAccountNameInUse(const SystemName: WideString): LongWord; dispid 217;
    function GetCachedUserName: WideString; dispid 218;
    property Attributes: LongWord dispid 219;
    property AttributesByType: OleVariant dispid 220;
    function GetStream: IUnknown; dispid 221;
    property UserName: WideString readonly dispid 222;
    function IsEqualSid(const Sid: IJwSid): WordBool; dispid 223;
    property StringSid: WideString readonly dispid 224;
    function ToString: WideString; dispid 225;
  end;

// *********************************************************************//
// Schnittstelle: IJwSidList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C88787BF-0091-46F2-A732-0639244C54E5}
// *********************************************************************//
  IJwSidList = interface(IDispatch)
    ['{C88787BF-0091-46F2-A732-0639244C54E5}']
    procedure InitBySidList(const SidList: IUnknown); safecall;
    procedure Add(const Sid: IJwSid); safecall;
    procedure Insert(Index: Integer; const Sid: IJwSid); safecall;
    procedure Remove(const Sid: IJwSid); safecall;
    procedure Delete(Index: Integer); safecall;
    function Get_Count: LongWord; safecall;
    function Get_Item(Index: LongWord): IJwSid; safecall;
    function Get__NewEnum: OleVariant; safecall;
    procedure Clear; safecall;
    function Find(const Sid: IJwSid; StartPos: Integer; UsePreFix: WordBool): Integer; safecall;
    property Count: LongWord read Get_Count;
    property Item[Index: LongWord]: IJwSid read Get_Item; default;
    property _NewEnum: OleVariant read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IJwSidListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C88787BF-0091-46F2-A732-0639244C54E5}
// *********************************************************************//
  IJwSidListDisp = dispinterface
    ['{C88787BF-0091-46F2-A732-0639244C54E5}']
    procedure InitBySidList(const SidList: IUnknown); dispid 201;
    procedure Add(const Sid: IJwSid); dispid 202;
    procedure Insert(Index: Integer; const Sid: IJwSid); dispid 203;
    procedure Remove(const Sid: IJwSid); dispid 204;
    procedure Delete(Index: Integer); dispid 205;
    property Count: LongWord readonly dispid 206;
    property Item[Index: LongWord]: IJwSid readonly dispid 0; default;
    property _NewEnum: OleVariant readonly dispid -4;
    procedure Clear; dispid 207;
    function Find(const Sid: IJwSid; StartPos: Integer; UsePreFix: WordBool): Integer; dispid 208;
  end;

// *********************************************************************//
// Schnittstelle: IJwTest
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {392DA373-0FE2-45D5-AF98-89BCC8D2D0CD}
// *********************************************************************//
  IJwTest = interface(IDispatch)
    ['{392DA373-0FE2-45D5-AF98-89BCC8D2D0CD}']
    procedure Method1; safecall;
  end;

// *********************************************************************//
// DispIntf:  IJwTestDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {392DA373-0FE2-45D5-AF98-89BCC8D2D0CD}
// *********************************************************************//
  IJwTestDisp = dispinterface
    ['{392DA373-0FE2-45D5-AF98-89BCC8D2D0CD}']
    procedure Method1; dispid 201;
  end;

// *********************************************************************//
// Schnittstelle: IJwAccessControlList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41FB5FBD-E101-4A32-94AB-BD463E1536BE}
// *********************************************************************//
  IJwAccessControlList = interface(IDispatch)
    ['{41FB5FBD-E101-4A32-94AB-BD463E1536BE}']
    procedure AddAllowAce(const Sid: IJwSid; Mask: LongWord; Flags: LongWord; ObjectType: TGUID; 
                          InheritedObjectType: TGUID); safecall;
    procedure AddDenyAce(const Sid: IJwSid; Mask: LongWord; Flags: LongWord; ObjectType: TGUID; 
                         InheritedObjectType: TGUID); safecall;
    procedure Item; safecall;
    function Get__NewEnum: IJwAccessControlEntry; safecall;
    function GetInternalObject: PChar; safecall;
    procedure Delete(Index: LongWord); safecall;
    procedure Remove(const Ace: IJwAccessControlEntry); safecall;
    function ToString: WideString; safecall;
    procedure Find(const Sid: IJwSid; Index: Integer); safecall;
    function Get_Revision: LongWord; safecall;
    procedure Set_Revision(Value: LongWord); safecall;
    procedure InitByAccessList(const List: IJwAccessControlList); safecall;
    procedure Clear; safecall;
    property _NewEnum: IJwAccessControlEntry read Get__NewEnum;
    property Revision: LongWord read Get_Revision write Set_Revision;
  end;

// *********************************************************************//
// DispIntf:  IJwAccessControlListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {41FB5FBD-E101-4A32-94AB-BD463E1536BE}
// *********************************************************************//
  IJwAccessControlListDisp = dispinterface
    ['{41FB5FBD-E101-4A32-94AB-BD463E1536BE}']
    procedure AddAllowAce(const Sid: IJwSid; Mask: LongWord; Flags: LongWord; 
                          ObjectType: {??TGUID}OleVariant; InheritedObjectType: {??TGUID}OleVariant); dispid 201;
    procedure AddDenyAce(const Sid: IJwSid; Mask: LongWord; Flags: LongWord; 
                         ObjectType: {??TGUID}OleVariant; InheritedObjectType: {??TGUID}OleVariant); dispid 202;
    procedure Item; dispid 203;
    property _NewEnum: IJwAccessControlEntry readonly dispid -4;
    function GetInternalObject: {??PChar}OleVariant; dispid 204;
    procedure Delete(Index: LongWord); dispid 205;
    procedure Remove(const Ace: IJwAccessControlEntry); dispid 206;
    function ToString: WideString; dispid 207;
    procedure Find(const Sid: IJwSid; Index: Integer); dispid 208;
    property Revision: LongWord dispid 209;
    procedure InitByAccessList(const List: IJwAccessControlList); dispid 210;
    procedure Clear; dispid 211;
  end;

// *********************************************************************//
// Schnittstelle: IJwAccessControlEntry
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FEB78113-1787-408A-B877-7063F23604ED}
// *********************************************************************//
  IJwAccessControlEntry = interface(IDispatch)
    ['{FEB78113-1787-408A-B877-7063F23604ED}']
    function Get_AccessMask: LongWord; safecall;
    procedure Set_AccessMask(Value: LongWord); safecall;
    function Get_Sid: IJwSid; safecall;
    procedure Set_Sid(const Value: IJwSid); safecall;
    function Get_Flags: LongWord; safecall;
    procedure Set_Flags(Value: LongWord); safecall;
    function Get_Revision: Word; safecall;
    procedure Set_Revision(Value: Word); safecall;
    function Get_ObjectType: TGUID; safecall;
    procedure Set_ObjectType(Value: TGUID); safecall;
    function Get_InheritedObjectType: TGUID; safecall;
    procedure Set_InheritedObjectType(Value: TGUID); safecall;
    function ToString(const Map: IUnknown): WideString; safecall;
    function Get_AceType: LongWord; safecall;
    procedure Init(AceType: LongWord); safecall;
    property AccessMask: LongWord read Get_AccessMask write Set_AccessMask;
    property Sid: IJwSid read Get_Sid write Set_Sid;
    property Flags: LongWord read Get_Flags write Set_Flags;
    property Revision: Word read Get_Revision write Set_Revision;
    property ObjectType: TGUID read Get_ObjectType write Set_ObjectType;
    property InheritedObjectType: TGUID read Get_InheritedObjectType write Set_InheritedObjectType;
    property AceType: LongWord read Get_AceType;
  end;

// *********************************************************************//
// DispIntf:  IJwAccessControlEntryDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FEB78113-1787-408A-B877-7063F23604ED}
// *********************************************************************//
  IJwAccessControlEntryDisp = dispinterface
    ['{FEB78113-1787-408A-B877-7063F23604ED}']
    property AccessMask: LongWord dispid 201;
    property Sid: IJwSid dispid 202;
    property Flags: LongWord dispid 203;
    property Revision: {??Word}OleVariant dispid 204;
    property ObjectType: {??TGUID}OleVariant dispid 205;
    property InheritedObjectType: {??TGUID}OleVariant dispid 206;
    function ToString(const Map: IUnknown): WideString; dispid 208;
    property AceType: LongWord readonly dispid 207;
    procedure Init(AceType: LongWord); dispid 209;
  end;

// *********************************************************************//
// Schnittstelle: IJwSecurityDescriptor
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {497C3246-F614-476B-874E-EB6FFC4571E3}
// *********************************************************************//
  IJwSecurityDescriptor = interface(IDispatch)
    ['{497C3246-F614-476B-874E-EB6FFC4571E3}']
    function Get_Owner: IJwSid; safecall;
    procedure Set_Owner(const Value: IJwSid); safecall;
    function Get_Group: IJwSid; safecall;
    procedure Set_Group(const Value: IJwSid); safecall;
    function Get_DACL: IJwAccessControlList; safecall;
    procedure Set_DACL(const Value: IJwAccessControlList); safecall;
    function Get_SACL: IJwAccessControlList; safecall;
    procedure Set_SACL(const Value: IJwAccessControlList); safecall;
    function ToString(const Map: IUnknown): WideString; safecall;
    function Get_Control: LongWord; safecall;
    procedure Set_Control(Value: LongWord); safecall;
    function Get_OwnerInherited: WordBool; safecall;
    procedure Set_OwnerInherited(Value: WordBool); safecall;
    function Get_GroupInherited: WordBool; safecall;
    procedure Set_GroupInherited(Value: WordBool); safecall;
    function Get_DACLInherited: WordBool; safecall;
    procedure Set_DACLInherited(Value: WordBool); safecall;
    function Get_InheritanceDACLProtection: LongWord; safecall;
    procedure Set_InheritanceDACLProtection(Value: LongWord); safecall;
    function Get_InheritanceSACLProtection: LongWord; safecall;
    procedure Set_InheritanceSACLProtection(Value: LongWord); safecall;
    procedure InitBySD(const SD: IJwSecurityDescriptor; Elements: LongWord); safecall;
    procedure InitByString(const SDString: WideString); safecall;
    procedure InitDefaultByToken(const Token: IUnknown; TokenType: LongWord); safecall;
    property Owner: IJwSid read Get_Owner write Set_Owner;
    property Group: IJwSid read Get_Group write Set_Group;
    property DACL: IJwAccessControlList read Get_DACL write Set_DACL;
    property SACL: IJwAccessControlList read Get_SACL write Set_SACL;
    property Control: LongWord read Get_Control write Set_Control;
    property OwnerInherited: WordBool read Get_OwnerInherited write Set_OwnerInherited;
    property GroupInherited: WordBool read Get_GroupInherited write Set_GroupInherited;
    property DACLInherited: WordBool read Get_DACLInherited write Set_DACLInherited;
    property InheritanceDACLProtection: LongWord read Get_InheritanceDACLProtection write Set_InheritanceDACLProtection;
    property InheritanceSACLProtection: LongWord read Get_InheritanceSACLProtection write Set_InheritanceSACLProtection;
  end;

// *********************************************************************//
// DispIntf:  IJwSecurityDescriptorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {497C3246-F614-476B-874E-EB6FFC4571E3}
// *********************************************************************//
  IJwSecurityDescriptorDisp = dispinterface
    ['{497C3246-F614-476B-874E-EB6FFC4571E3}']
    property Owner: IJwSid dispid 201;
    property Group: IJwSid dispid 202;
    property DACL: IJwAccessControlList dispid 203;
    property SACL: IJwAccessControlList dispid 204;
    function ToString(const Map: IUnknown): WideString; dispid 205;
    property Control: LongWord dispid 206;
    property OwnerInherited: WordBool dispid 207;
    property GroupInherited: WordBool dispid 208;
    property DACLInherited: WordBool dispid 209;
    property InheritanceDACLProtection: LongWord dispid 210;
    property InheritanceSACLProtection: LongWord dispid 211;
    procedure InitBySD(const SD: IJwSecurityDescriptor; Elements: LongWord); dispid 212;
    procedure InitByString(const SDString: WideString); dispid 213;
    procedure InitDefaultByToken(const Token: IUnknown; TokenType: LongWord); dispid 214;
  end;

// *********************************************************************//
// Schnittstelle: IJwToken
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7115952F-3B76-4CE1-B052-E4D38D4CCC09}
// *********************************************************************//
  IJwToken = interface(IDispatch)
    ['{7115952F-3B76-4CE1-B052-E4D38D4CCC09}']
    function Get_TokenType: LongWord; safecall;
    function Get_AccessMask: LongWord; safecall;
    procedure Set_AccessMask(Value: LongWord); safecall;
    function Get_Owner: IJwSid; safecall;
    procedure Set_Owner(const Value: IJwSid); safecall;
    function Get_TokenUser: IJwSid; safecall;
    function Get_Group: IJwSid; safecall;
    procedure Set_Group(const Value: IJwSid); safecall;
    function Get_Groups: IJwSidList; safecall;
    procedure Set_Groups(const Value: IJwSidList); safecall;
    function Get_IsRestricted: WordBool; safecall;
    function Get_SessionID: LongWord; safecall;
    procedure Set_SessionID(Value: LongWord); safecall;
    function Get_Privileges: IUnknown; safecall;
    procedure Set_Privileges(const Value: IUnknown); safecall;
    function Get_LinkedToken: IJwToken; safecall;
    function Get_IntegrityLevel: IJwSidList; safecall;
    function Get_DefaultDacl: IJwSecurityDescriptor; safecall;
    procedure Set_DefaultDacl(const Value: IJwSecurityDescriptor); safecall;
    function Get_ImpersonationLevel: LongWord; safecall;
    procedure InitByProcess(ProcessHandle: LongWord; DesiredAccess: LongWord; Duplicate: WordBool); safecall;
    procedure InitByThread(ThreadHandle: LongWord; DesiredAccess: LongWord; Duplicate: WordBool); safecall;
    procedure InitByEffective(DesiredAccess: LongWord); safecall;
    procedure InitByProcessId(ProcessId: LongWord; DesiredAccess: LongWord); safecall;
    procedure InitByDuplicateToken(const Token: IJwToken; DesiredAccess: LongWord); safecall;
    procedure InitWTSQueryUserToken(SessionId: LongWord); safecall;
    procedure InitWTSQueryUserTokenEx(Server: LongWord; SessionId: LongWord); safecall;
    procedure InitByCompatibilityQueryUserToken(DesiredAccess: LongWord; 
                                                const ProcessName: WideString); safecall;
    procedure InitRestricted(const PreviousToken: IJwToken; AccessMask: LongWord; Flags: LongWord; 
                             const SidsToDisable: IJwSidList; const PrivilegesToDisable: IUnknown; 
                             const RestrictedSids: IJwSidList); safecall;
    procedure InitLogonUser(const Name: WideString; const Domain: WideString; 
                            const Password: WideString; LogonType: LongWord; LogonProvider: LongWord); safecall;
    procedure ConvertToImpersonatedToken(ImpLevel: LongWord; DesiredAccess: LongWord); safecall;
    procedure ConvertToPrimaryToken(DesiredAccess: LongWord); safecall;
    procedure Method1; safecall;
    function Get_IntegrityLevelType: JwIntegrityLevelType; safecall;
    procedure Set_IntegrityLevelType(Value: JwIntegrityLevelType); safecall;
    function Get_VirtualizationAllowed: WordBool; safecall;
    function Get_VirtualiationEnabled: WordBool; safecall;
    function Get_MandatoryPolicy: LongWord; safecall;
    function Get_RunElevation: LongWord; safecall;
    function Get_ElevationType: LongWord; safecall;
    property TokenType: LongWord read Get_TokenType;
    property AccessMask: LongWord read Get_AccessMask write Set_AccessMask;
    property Owner: IJwSid read Get_Owner write Set_Owner;
    property TokenUser: IJwSid read Get_TokenUser;
    property Group: IJwSid read Get_Group write Set_Group;
    property Groups: IJwSidList read Get_Groups write Set_Groups;
    property IsRestricted: WordBool read Get_IsRestricted;
    property SessionID: LongWord read Get_SessionID write Set_SessionID;
    property Privileges: IUnknown read Get_Privileges write Set_Privileges;
    property LinkedToken: IJwToken read Get_LinkedToken;
    property IntegrityLevel: IJwSidList read Get_IntegrityLevel;
    property DefaultDacl: IJwSecurityDescriptor read Get_DefaultDacl write Set_DefaultDacl;
    property ImpersonationLevel: LongWord read Get_ImpersonationLevel;
    property IntegrityLevelType: JwIntegrityLevelType read Get_IntegrityLevelType write Set_IntegrityLevelType;
    property VirtualizationAllowed: WordBool read Get_VirtualizationAllowed;
    property VirtualiationEnabled: WordBool read Get_VirtualiationEnabled;
    property MandatoryPolicy: LongWord read Get_MandatoryPolicy;
    property RunElevation: LongWord read Get_RunElevation;
    property ElevationType: LongWord read Get_ElevationType;
  end;

// *********************************************************************//
// DispIntf:  IJwTokenDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7115952F-3B76-4CE1-B052-E4D38D4CCC09}
// *********************************************************************//
  IJwTokenDisp = dispinterface
    ['{7115952F-3B76-4CE1-B052-E4D38D4CCC09}']
    property TokenType: LongWord readonly dispid 201;
    property AccessMask: LongWord dispid 202;
    property Owner: IJwSid dispid 203;
    property TokenUser: IJwSid readonly dispid 204;
    property Group: IJwSid dispid 205;
    property Groups: IJwSidList dispid 206;
    property IsRestricted: WordBool readonly dispid 207;
    property SessionID: LongWord dispid 208;
    property Privileges: IUnknown dispid 209;
    property LinkedToken: IJwToken readonly dispid 210;
    property IntegrityLevel: IJwSidList readonly dispid 211;
    property DefaultDacl: IJwSecurityDescriptor dispid 213;
    property ImpersonationLevel: LongWord readonly dispid 214;
    procedure InitByProcess(ProcessHandle: LongWord; DesiredAccess: LongWord; Duplicate: WordBool); dispid 215;
    procedure InitByThread(ThreadHandle: LongWord; DesiredAccess: LongWord; Duplicate: WordBool); dispid 216;
    procedure InitByEffective(DesiredAccess: LongWord); dispid 217;
    procedure InitByProcessId(ProcessId: LongWord; DesiredAccess: LongWord); dispid 218;
    procedure InitByDuplicateToken(const Token: IJwToken; DesiredAccess: LongWord); dispid 219;
    procedure InitWTSQueryUserToken(SessionId: LongWord); dispid 220;
    procedure InitWTSQueryUserTokenEx(Server: LongWord; SessionId: LongWord); dispid 221;
    procedure InitByCompatibilityQueryUserToken(DesiredAccess: LongWord; 
                                                const ProcessName: WideString); dispid 222;
    procedure InitRestricted(const PreviousToken: IJwToken; AccessMask: LongWord; Flags: LongWord; 
                             const SidsToDisable: IJwSidList; const PrivilegesToDisable: IUnknown; 
                             const RestrictedSids: IJwSidList); dispid 223;
    procedure InitLogonUser(const Name: WideString; const Domain: WideString; 
                            const Password: WideString; LogonType: LongWord; LogonProvider: LongWord); dispid 224;
    procedure ConvertToImpersonatedToken(ImpLevel: LongWord; DesiredAccess: LongWord); dispid 225;
    procedure ConvertToPrimaryToken(DesiredAccess: LongWord); dispid 226;
    procedure Method1; dispid 227;
    property IntegrityLevelType: JwIntegrityLevelType dispid 228;
    property VirtualizationAllowed: WordBool readonly dispid 212;
    property VirtualiationEnabled: WordBool readonly dispid 229;
    property MandatoryPolicy: LongWord readonly dispid 230;
    property RunElevation: LongWord readonly dispid 231;
    property ElevationType: LongWord readonly dispid 232;
  end;

// *********************************************************************//
// Schnittstelle: IJwPrivilegeList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D7B09407-170C-4407-A51B-500F9479CC15}
// *********************************************************************//
  IJwPrivilegeList = interface(IDispatch)
    ['{D7B09407-170C-4407-A51B-500F9479CC15}']
    function Get__NewEnum: Integer; safecall;
    function Get_Item: IJwPrivilege; safecall;
    procedure ToString(var Value: WideString); safecall;
    procedure AddByName(const Name: WideString); safecall;
    procedure AddByLuid(HighValue: LongWord; LowValue: LongWord); safecall;
    procedure InitReadOnly; safecall;
    function Get_ReadOnly: WordBool; safecall;
    procedure DoneReadOnly; safecall;
    procedure DeleteByLuid(High: LongWord; Low: LongWord); safecall;
    procedure DeleteByName(const Name: WideString); safecall;
    procedure DisableAllPrivileges(Value: Integer); safecall;
    function Get_Control: LongWord; safecall;
    procedure Set_Control(Value: LongWord); safecall;
    property _NewEnum: Integer read Get__NewEnum;
    property Item: IJwPrivilege read Get_Item;
    property ReadOnly: WordBool read Get_ReadOnly;
    property Control: LongWord read Get_Control write Set_Control;
  end;

// *********************************************************************//
// DispIntf:  IJwPrivilegeListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D7B09407-170C-4407-A51B-500F9479CC15}
// *********************************************************************//
  IJwPrivilegeListDisp = dispinterface
    ['{D7B09407-170C-4407-A51B-500F9479CC15}']
    property _NewEnum: Integer readonly dispid -4;
    property Item: IJwPrivilege readonly dispid 202;
    procedure ToString(var Value: WideString); dispid 203;
    procedure AddByName(const Name: WideString); dispid 204;
    procedure AddByLuid(HighValue: LongWord; LowValue: LongWord); dispid 205;
    procedure InitReadOnly; dispid 206;
    property ReadOnly: WordBool readonly dispid 201;
    procedure DoneReadOnly; dispid 207;
    procedure DeleteByLuid(High: LongWord; Low: LongWord); dispid 208;
    procedure DeleteByName(const Name: WideString); dispid 209;
    procedure DisableAllPrivileges(Value: Integer); dispid 210;
    property Control: LongWord dispid 211;
  end;

// *********************************************************************//
// Schnittstelle: IJwPrivilege
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FCA8F64B-B76D-45B4-A010-E3F1134AE1A9}
// *********************************************************************//
  IJwPrivilege = interface(IDispatch)
    ['{FCA8F64B-B76D-45B4-A010-E3F1134AE1A9}']
    function Get_HiValue: LongWord; safecall;
    procedure Set_HiValue(Value: LongWord); safecall;
    function Get_LoValue: LongWord; safecall;
    procedure Set_LoValue(Value: LongWord); safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_ReadOnly: WordBool; safecall;
    function Get_Enabled: WordBool; safecall;
    procedure Set_Enabled(Value: WordBool); safecall;
    function Get_Attributes: LongWord; safecall;
    procedure Set_Attributes(Value: LongWord); safecall;
    property HiValue: LongWord read Get_HiValue write Set_HiValue;
    property LoValue: LongWord read Get_LoValue write Set_LoValue;
    property Name: WideString read Get_Name write Set_Name;
    property ReadOnly: WordBool read Get_ReadOnly;
    property Enabled: WordBool read Get_Enabled write Set_Enabled;
    property Attributes: LongWord read Get_Attributes write Set_Attributes;
  end;

// *********************************************************************//
// DispIntf:  IJwPrivilegeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {FCA8F64B-B76D-45B4-A010-E3F1134AE1A9}
// *********************************************************************//
  IJwPrivilegeDisp = dispinterface
    ['{FCA8F64B-B76D-45B4-A010-E3F1134AE1A9}']
    property HiValue: LongWord dispid 201;
    property LoValue: LongWord dispid 202;
    property Name: WideString dispid 203;
    property ReadOnly: WordBool readonly dispid 204;
    property Enabled: WordBool dispid 205;
    property Attributes: LongWord dispid 206;
  end;

// *********************************************************************//
// Schnittstelle: IJwWindowsVersion
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {632F08B7-7D3B-4865-B17A-00B97FBE3D9B}
// *********************************************************************//
  IJwWindowsVersion = interface(IDispatch)
    ['{632F08B7-7D3B-4865-B17A-00B97FBE3D9B}']
    function Get_WindowsType: SYSINT; safecall;
    function Get_IsWindows2000(OrHigher: WordBool): WordBool; safecall;
    function Get_IsWindows2003(OrHigher: WordBool): WordBool; safecall;
    function Get_IsWindows2003R2(OrHigher: WordBool): WordBool; safecall;
    function Get_IsWindowsXP(OrHigher: WordBool): WordBool; safecall;
    function Get_IsWindowsVista(OrHigher: WordBool): WordBool; safecall;
    function Get_IsWindows2008(OrHigher: WordBool): WordBool; safecall;
    function Get_IsServer: WordBool; safecall;
    function Get_IsTerminalServiceRunning: WordBool; safecall;
    property WindowsType: SYSINT read Get_WindowsType;
    property IsWindows2000[OrHigher: WordBool]: WordBool read Get_IsWindows2000;
    property IsWindows2003[OrHigher: WordBool]: WordBool read Get_IsWindows2003;
    property IsWindows2003R2[OrHigher: WordBool]: WordBool read Get_IsWindows2003R2;
    property IsWindowsXP[OrHigher: WordBool]: WordBool read Get_IsWindowsXP;
    property IsWindowsVista[OrHigher: WordBool]: WordBool read Get_IsWindowsVista;
    property IsWindows2008[OrHigher: WordBool]: WordBool read Get_IsWindows2008;
    property IsServer: WordBool read Get_IsServer;
    property IsTerminalServiceRunning: WordBool read Get_IsTerminalServiceRunning;
  end;

// *********************************************************************//
// DispIntf:  IJwWindowsVersionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {632F08B7-7D3B-4865-B17A-00B97FBE3D9B}
// *********************************************************************//
  IJwWindowsVersionDisp = dispinterface
    ['{632F08B7-7D3B-4865-B17A-00B97FBE3D9B}']
    property WindowsType: SYSINT readonly dispid 201;
    property IsWindows2000[OrHigher: WordBool]: WordBool readonly dispid 202;
    property IsWindows2003[OrHigher: WordBool]: WordBool readonly dispid 203;
    property IsWindows2003R2[OrHigher: WordBool]: WordBool readonly dispid 204;
    property IsWindowsXP[OrHigher: WordBool]: WordBool readonly dispid 205;
    property IsWindowsVista[OrHigher: WordBool]: WordBool readonly dispid 206;
    property IsWindows2008[OrHigher: WordBool]: WordBool readonly dispid 207;
    property IsServer: WordBool readonly dispid 208;
    property IsTerminalServiceRunning: WordBool readonly dispid 209;
  end;

// *********************************************************************//
// Schnittstelle: IJwGenericMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DD1015C5-9E4D-4062-8E0C-E2606705673A}
// *********************************************************************//
  IJwGenericMapping = interface(IDispatch)
    ['{DD1015C5-9E4D-4062-8E0C-E2606705673A}']
    function GetMapping: TGenericMapping; safecall;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; safecall;
    function GetAccessNames(out Count: LongWord): PChar; safecall;
  end;

// *********************************************************************//
// DispIntf:  IJwGenericMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {DD1015C5-9E4D-4062-8E0C-E2606705673A}
// *********************************************************************//
  IJwGenericMappingDisp = dispinterface
    ['{DD1015C5-9E4D-4062-8E0C-E2606705673A}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwFileFolderMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B713F708-5096-4A0B-868C-BF0B5451E54A}
// *********************************************************************//
  IJwFileFolderMapping = interface(IJwGenericMapping)
    ['{B713F708-5096-4A0B-868C-BF0B5451E54A}']
  end;

// *********************************************************************//
// DispIntf:  IJwFileFolderMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B713F708-5096-4A0B-868C-BF0B5451E54A}
// *********************************************************************//
  IJwFileFolderMappingDisp = dispinterface
    ['{B713F708-5096-4A0B-868C-BF0B5451E54A}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Die Klasse CoJwSid stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwSid, dargestellt von
// CoClass JwSid, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwSid = class
    class function Create: IJwSid;
    class function CreateRemote(const MachineName: string): IJwSid;
  end;

// *********************************************************************//
// Die Klasse CoJwSidList stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwSidList, dargestellt von
// CoClass JwSidList, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwSidList = class
    class function Create: IJwSidList;
    class function CreateRemote(const MachineName: string): IJwSidList;
  end;

// *********************************************************************//
// Die Klasse CoJwTest2 stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwTest, dargestellt von
// CoClass JwTest2, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwTest2 = class
    class function Create: IJwTest;
    class function CreateRemote(const MachineName: string): IJwTest;
  end;

// *********************************************************************//
// Die Klasse CoJwAccessControlList stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwAccessControlList, dargestellt von
// CoClass JwAccessControlList, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwAccessControlList = class
    class function Create: IJwAccessControlList;
    class function CreateRemote(const MachineName: string): IJwAccessControlList;
  end;

// *********************************************************************//
// Die Klasse CoJwSecurityDescriptor stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwSecurityDescriptor, dargestellt von
// CoClass JwSecurityDescriptor, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwSecurityDescriptor = class
    class function Create: IJwSecurityDescriptor;
    class function CreateRemote(const MachineName: string): IJwSecurityDescriptor;
  end;

// *********************************************************************//
// Die Klasse CoJwToken stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwToken, dargestellt von
// CoClass JwToken, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwToken = class
    class function Create: IJwToken;
    class function CreateRemote(const MachineName: string): IJwToken;
  end;

// *********************************************************************//
// Die Klasse CoJwPrivilegeList stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwPrivilegeList, dargestellt von
// CoClass JwPrivilegeList, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwPrivilegeList = class
    class function Create: IJwPrivilegeList;
    class function CreateRemote(const MachineName: string): IJwPrivilegeList;
  end;

// *********************************************************************//
// Die Klasse CoJwWindowsVersion stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwWindowsVersion, dargestellt von
// CoClass JwWindowsVersion, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwWindowsVersion = class
    class function Create: IJwWindowsVersion;
    class function CreateRemote(const MachineName: string): IJwWindowsVersion;
  end;

implementation

uses ComObj;

class function CoJwSid.Create: IJwSid;
begin
  Result := CreateComObject(CLASS_JwSid) as IJwSid;
end;

class function CoJwSid.CreateRemote(const MachineName: string): IJwSid;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwSid) as IJwSid;
end;

class function CoJwSidList.Create: IJwSidList;
begin
  Result := CreateComObject(CLASS_JwSidList) as IJwSidList;
end;

class function CoJwSidList.CreateRemote(const MachineName: string): IJwSidList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwSidList) as IJwSidList;
end;

class function CoJwTest2.Create: IJwTest;
begin
  Result := CreateComObject(CLASS_JwTest2) as IJwTest;
end;

class function CoJwTest2.CreateRemote(const MachineName: string): IJwTest;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwTest2) as IJwTest;
end;

class function CoJwAccessControlList.Create: IJwAccessControlList;
begin
  Result := CreateComObject(CLASS_JwAccessControlList) as IJwAccessControlList;
end;

class function CoJwAccessControlList.CreateRemote(const MachineName: string): IJwAccessControlList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwAccessControlList) as IJwAccessControlList;
end;

class function CoJwSecurityDescriptor.Create: IJwSecurityDescriptor;
begin
  Result := CreateComObject(CLASS_JwSecurityDescriptor) as IJwSecurityDescriptor;
end;

class function CoJwSecurityDescriptor.CreateRemote(const MachineName: string): IJwSecurityDescriptor;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwSecurityDescriptor) as IJwSecurityDescriptor;
end;

class function CoJwToken.Create: IJwToken;
begin
  Result := CreateComObject(CLASS_JwToken) as IJwToken;
end;

class function CoJwToken.CreateRemote(const MachineName: string): IJwToken;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwToken) as IJwToken;
end;

class function CoJwPrivilegeList.Create: IJwPrivilegeList;
begin
  Result := CreateComObject(CLASS_JwPrivilegeList) as IJwPrivilegeList;
end;

class function CoJwPrivilegeList.CreateRemote(const MachineName: string): IJwPrivilegeList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwPrivilegeList) as IJwPrivilegeList;
end;

class function CoJwWindowsVersion.Create: IJwWindowsVersion;
begin
  Result := CreateComObject(CLASS_JwWindowsVersion) as IJwWindowsVersion;
end;

class function CoJwWindowsVersion.CreateRemote(const MachineName: string): IJwWindowsVersion;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwWindowsVersion) as IJwWindowsVersion;
end;

end.
