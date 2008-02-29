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
// Datei generiert am 29.02.2008 22:56:06 aus der unten beschriebenen Typbibliothek.

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
  CLASS_JwGenericMapping: TGUID = '{47DDE0D2-C980-4438-B7E2-DDE2B2A10179}';
  CLASS_JwFileFolderMapping: TGUID = '{EC6AEC6D-B278-46E6-8D70-51BB574680C3}';
  IID_IJwFileMapping: TGUID = '{8E4EE90E-1864-475D-842E-C1FA9099EF0A}';
  IID_IJwRegistryMapping: TGUID = '{4213A33D-F495-4C8F-A6A3-7CDD5E626B5D}';
  IID_IJwWinStationMapping: TGUID = '{AF3DA16B-BDD7-43A7-BA86-BAB06E05C848}';
  IID_IJwDesktopMapping: TGUID = '{C389C422-764A-4FF1-B02B-7DBAC4A255DE}';
  IID_IJwServiceMapping: TGUID = '{D71696A4-36C1-4D75-9829-D470AD5B7623}';
  IID_IJwServiceManagerMapping: TGUID = '{3714347D-12AA-424F-A525-254AD715EDD8}';
  IID_IJwPrinterMapping: TGUID = '{0BCBBD6C-1DBD-41D9-A2D4-DC9540F3ECEB}';
  IID_IJwShareMapping: TGUID = '{794FDFE5-4A60-4394-BEAB-D2FCFEE0984C}';
  IID_IJwProcessMapping: TGUID = '{993CDB31-A24C-4EEA-BDD0-10BA1AB3D7E1}';
  IID_IJwThreadMapping: TGUID = '{389BEEFC-7562-4E53-878E-6E3E61A510C0}';
  IID_IJwJobMapping: TGUID = '{D5B64FC6-3A49-483C-9D7B-34A04E6E97C2}';
  IID_IJwSemaphoreMapping: TGUID = '{28EEA5E2-21E7-4CB3-A585-6D4FF82BABB5}';
  IID_IJwEventMapping: TGUID = '{64357431-6565-494D-83E5-0FAB085A9FF5}';
  IID_IJwMutexMapping: TGUID = '{F15F1B47-4DB8-4F6F-A362-51B88DA5C5C4}';
  IID_IJwFileMapMapping: TGUID = '{991D7CAB-58A1-4575-9F3B-CC34D6E2DE00}';
  IID_IJwTimerMapping: TGUID = '{10F1574D-F097-4081-8DFD-F956581FBA2C}';
  IID_IJwTokenMapping: TGUID = '{8F1F8AF3-42EA-4E51-93B0-3C5E04B47C49}';
  IID_IJwPipeMapping: TGUID = '{48260C48-B275-4C7C-BDB5-4678E1E89160}';
  CLASS_JwFileMapping: TGUID = '{3DF532B4-755F-42C9-BC59-A21E3CC708B0}';
  CLASS_JwRegistryMapping: TGUID = '{FD3ECC15-5DCE-4C33-9AD8-3E1C684B0A92}';
  CLASS_JwWinStationMapping: TGUID = '{992432AD-18A3-4258-9CC0-57FEB7E6B436}';
  CLASS_JwDesktopMapping: TGUID = '{16F54095-1737-4096-B935-34EF65C5CDBC}';
  CLASS_JwServiceMapping: TGUID = '{CFC4C3BE-DABA-4D6B-8E8B-7975BFC45565}';
  CLASS_JwServiceMappingManager: TGUID = '{2527AF41-57A8-4155-A2A3-410CF38F2DD4}';
  CLASS_JwPrinterMapping: TGUID = '{E93AFFDE-D202-41F5-BA0A-D581B522BD30}';
  CLASS_JwShareMapping: TGUID = '{55E9D4B9-C7E8-42A0-A722-92F3D14C3F15}';
  CLASS_JwProcessMapping: TGUID = '{43CA611E-9A39-4D87-9502-52DE539B9550}';
  CLASS_JwThreadMapping: TGUID = '{BFEC9D3D-CF1C-4469-91CB-6A71B3FD5AA6}';
  CLASS_JwJobMapping: TGUID = '{591054A9-C9F7-4CF6-8828-332B57B67118}';
  CLASS_JwSemaphoreMapping: TGUID = '{C44DE038-2DCD-4110-A2F6-119765F1B831}';
  CLASS_JwEventMapping: TGUID = '{C04BC95B-17F1-4724-9D11-B30CC8096329}';
  CLASS_JwMutexMapping: TGUID = '{0B253007-A4B3-4C57-ABD4-E64369929B76}';
  CLASS_JwFileMapMapping: TGUID = '{52E77685-6C77-4997-A765-10CDF08D05BE}';
  CLASS_JwTimerMapping: TGUID = '{88D8E485-04C2-4CAD-BF71-0C16092C6615}';
  CLASS_JwTokenMapping: TGUID = '{919E566A-F30C-40EF-91F8-0432E3EF19CA}';
  CLASS_JwPipeMapping: TGUID = '{4BC9B8CC-9106-43D8-A2F4-4BD0D0D39678}';
  IID_IJwLogServer: TGUID = '{213E956A-7BF3-4007-8191-1D743C818435}';
  CLASS_JwLogServer: TGUID = '{348E3372-4F93-4548-B89B-4A6EACBF8DEC}';
  IID_IJwWriterClass: TGUID = '{E225CFE1-FCAB-4109-8D64-B845D0C8FDD5}';
  IID_IJwLogClient: TGUID = '{742092DA-9FD9-420A-84AF-56D6EA8508D4}';
  CLASS_JwLogClient: TGUID = '{CB1006B5-4F9F-48D6-98AD-FCFDC71BDB95}';
  IID_ITestInt: TGUID = '{D2B0D3F0-F029-4550-A7CC-07BC612BF10B}';
  CLASS_TestInt: TGUID = '{D5B908FF-FE50-49AB-AFB2-5930687F4E82}';
  IID_IJwXMLAttribute: TGUID = '{B7C4FFC9-32AF-4341-9434-4F33FEDF1707}';
  IID_IJwEventType: TGUID = '{B95E0B5E-F424-4AC4-98F0-B1C79C19AD11}';
  CLASS_JwXMLAttribute: TGUID = '{7AB2866B-0017-4B7D-89F2-B31EA7B70A3C}';
  CLASS_JwEventType: TGUID = '{5EEE6B61-5685-4DBD-9740-ABF3109825AA}';

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

// Konstanten für enum JwEnumEnterType
type
  JwEnumEnterType = TOleEnum;
const
  etNone = $00000000;
  etFunction = $00000001;
  etMethod = $00000002;
  etThread = $00000003;

// Konstanten für enum JwEnumLogType
type
  JwEnumLogType = TOleEnum;
const
  lsNone = $00000000;
  lsMessage = $00000001;
  lsWarning = $00000002;
  lsError = $00000003;
  lsStop = $00000004;

// Konstanten für enum JwEnumSignalType
type
  JwEnumSignalType = TOleEnum;
const
  stNone = $00000000;
  stSend = $00000001;
  stReceived = $00000002;
  stWait = $00000003;

// Konstanten für enum JwEnumXMLTag
type
  JwEnumXMLTag = TOleEnum;
const
  xtLogFile = $00000000;
  xtLogProcess = $00000001;
  xtEnter = $00000002;
  xtLeave = $00000003;
  xtSignal = $00000004;
  xtMemory = $00000005;
  xtLog = $00000006;
  xtException = $00000007;
  xtType = $00000008;
  xtGuid = $00000009;
  xtGetLastError = $0000000A;
  xtWinApiFunction = $0000000B;
  xtLogString = $0000000C;
  xtComSource = $0000000D;
  xtStackTrace = $0000000E;
  xtMessage = $0000000F;
  xtLastErrorString = $00000010;
  xtSourceProc = $00000011;

// Konstanten für enum JwXMLLogTag
type
  JwXMLLogTag = TOleEnum;
const
  ltEnter = $00000000;
  ltLeave = $00000001;
  ltSignal = $00000002;
  ltMemory = $00000003;
  ltLog = $00000004;
  ltException = $00000005;

// Konstanten für enum JwXMLAttrTag
type
  JwXMLAttrTag = TOleEnum;
const
  atStart = $00000000;
  atEnd = $00000001;
  atType = $00000002;
  atMethod = $00000003;
  atClass = $00000004;
  atFile = $00000005;
  atThread = $00000006;
  atSource = $00000007;
  atTarget = $00000008;
  atMemType = $00000009;

// Konstanten für enum JwEnumMemoryType
type
  JwEnumMemoryType = TOleEnum;
const
  mtNone = $00000000;
  mtAlloc = $00000001;
  mtDeAlloc = $00000002;

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
  IJwFileMapping = interface;
  IJwFileMappingDisp = dispinterface;
  IJwRegistryMapping = interface;
  IJwRegistryMappingDisp = dispinterface;
  IJwWinStationMapping = interface;
  IJwWinStationMappingDisp = dispinterface;
  IJwDesktopMapping = interface;
  IJwDesktopMappingDisp = dispinterface;
  IJwServiceMapping = interface;
  IJwServiceMappingDisp = dispinterface;
  IJwServiceManagerMapping = interface;
  IJwServiceManagerMappingDisp = dispinterface;
  IJwPrinterMapping = interface;
  IJwPrinterMappingDisp = dispinterface;
  IJwShareMapping = interface;
  IJwShareMappingDisp = dispinterface;
  IJwProcessMapping = interface;
  IJwProcessMappingDisp = dispinterface;
  IJwThreadMapping = interface;
  IJwThreadMappingDisp = dispinterface;
  IJwJobMapping = interface;
  IJwJobMappingDisp = dispinterface;
  IJwSemaphoreMapping = interface;
  IJwSemaphoreMappingDisp = dispinterface;
  IJwEventMapping = interface;
  IJwEventMappingDisp = dispinterface;
  IJwMutexMapping = interface;
  IJwMutexMappingDisp = dispinterface;
  IJwFileMapMapping = interface;
  IJwFileMapMappingDisp = dispinterface;
  IJwTimerMapping = interface;
  IJwTimerMappingDisp = dispinterface;
  IJwTokenMapping = interface;
  IJwTokenMappingDisp = dispinterface;
  IJwPipeMapping = interface;
  IJwPipeMappingDisp = dispinterface;
  IJwLogServer = interface;
  IJwLogServerDisp = dispinterface;
  IJwWriterClass = interface;
  IJwWriterClassDisp = dispinterface;
  IJwLogClient = interface;
  IJwLogClientDisp = dispinterface;
  ITestInt = interface;
  ITestIntDisp = dispinterface;
  IJwXMLAttribute = interface;
  IJwXMLAttributeDisp = dispinterface;
  IJwEventType = interface;
  IJwEventTypeDisp = dispinterface;

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
  JwGenericMapping = IJwGenericMapping;
  JwFileFolderMapping = IJwFileFolderMapping;
  JwFileMapping = IJwFileMapping;
  JwRegistryMapping = IJwRegistryMapping;
  JwWinStationMapping = IJwWinStationMapping;
  JwDesktopMapping = IJwDesktopMapping;
  JwServiceMapping = IJwServiceMapping;
  JwServiceMappingManager = IJwServiceManagerMapping;
  JwPrinterMapping = IJwPrinterMapping;
  JwShareMapping = IJwShareMapping;
  JwProcessMapping = IJwProcessMapping;
  JwThreadMapping = IJwThreadMapping;
  JwJobMapping = IJwJobMapping;
  JwSemaphoreMapping = IJwSemaphoreMapping;
  JwEventMapping = IJwEventMapping;
  JwMutexMapping = IJwMutexMapping;
  JwFileMapMapping = IJwFileMapMapping;
  JwTimerMapping = IJwTimerMapping;
  JwTokenMapping = IJwTokenMapping;
  JwPipeMapping = IJwPipeMapping;
  JwLogServer = IJwLogServer;
  JwLogClient = IJwLogClient;
  TestInt = ITestInt;
  JwXMLAttribute = IJwXMLAttribute;
  JwEventType = IJwEventType;


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
    procedure InitWTSQueryUserToken(SessionID: LongWord); safecall;
    procedure InitWTSQueryUserTokenEx(Server: LongWord; SessionID: LongWord); safecall;
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
    procedure InitWTSQueryUserToken(SessionID: LongWord); dispid 220;
    procedure InitWTSQueryUserTokenEx(Server: LongWord; SessionID: LongWord); dispid 221;
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
// Schnittstelle: IJwFileMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E4EE90E-1864-475D-842E-C1FA9099EF0A}
// *********************************************************************//
  IJwFileMapping = interface(IJwGenericMapping)
    ['{8E4EE90E-1864-475D-842E-C1FA9099EF0A}']
  end;

// *********************************************************************//
// DispIntf:  IJwFileMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8E4EE90E-1864-475D-842E-C1FA9099EF0A}
// *********************************************************************//
  IJwFileMappingDisp = dispinterface
    ['{8E4EE90E-1864-475D-842E-C1FA9099EF0A}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwRegistryMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4213A33D-F495-4C8F-A6A3-7CDD5E626B5D}
// *********************************************************************//
  IJwRegistryMapping = interface(IJwGenericMapping)
    ['{4213A33D-F495-4C8F-A6A3-7CDD5E626B5D}']
  end;

// *********************************************************************//
// DispIntf:  IJwRegistryMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4213A33D-F495-4C8F-A6A3-7CDD5E626B5D}
// *********************************************************************//
  IJwRegistryMappingDisp = dispinterface
    ['{4213A33D-F495-4C8F-A6A3-7CDD5E626B5D}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwWinStationMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AF3DA16B-BDD7-43A7-BA86-BAB06E05C848}
// *********************************************************************//
  IJwWinStationMapping = interface(IJwGenericMapping)
    ['{AF3DA16B-BDD7-43A7-BA86-BAB06E05C848}']
  end;

// *********************************************************************//
// DispIntf:  IJwWinStationMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {AF3DA16B-BDD7-43A7-BA86-BAB06E05C848}
// *********************************************************************//
  IJwWinStationMappingDisp = dispinterface
    ['{AF3DA16B-BDD7-43A7-BA86-BAB06E05C848}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwDesktopMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C389C422-764A-4FF1-B02B-7DBAC4A255DE}
// *********************************************************************//
  IJwDesktopMapping = interface(IJwGenericMapping)
    ['{C389C422-764A-4FF1-B02B-7DBAC4A255DE}']
  end;

// *********************************************************************//
// DispIntf:  IJwDesktopMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C389C422-764A-4FF1-B02B-7DBAC4A255DE}
// *********************************************************************//
  IJwDesktopMappingDisp = dispinterface
    ['{C389C422-764A-4FF1-B02B-7DBAC4A255DE}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwServiceMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D71696A4-36C1-4D75-9829-D470AD5B7623}
// *********************************************************************//
  IJwServiceMapping = interface(IJwGenericMapping)
    ['{D71696A4-36C1-4D75-9829-D470AD5B7623}']
  end;

// *********************************************************************//
// DispIntf:  IJwServiceMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D71696A4-36C1-4D75-9829-D470AD5B7623}
// *********************************************************************//
  IJwServiceMappingDisp = dispinterface
    ['{D71696A4-36C1-4D75-9829-D470AD5B7623}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwServiceManagerMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3714347D-12AA-424F-A525-254AD715EDD8}
// *********************************************************************//
  IJwServiceManagerMapping = interface(IJwGenericMapping)
    ['{3714347D-12AA-424F-A525-254AD715EDD8}']
  end;

// *********************************************************************//
// DispIntf:  IJwServiceManagerMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3714347D-12AA-424F-A525-254AD715EDD8}
// *********************************************************************//
  IJwServiceManagerMappingDisp = dispinterface
    ['{3714347D-12AA-424F-A525-254AD715EDD8}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwPrinterMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0BCBBD6C-1DBD-41D9-A2D4-DC9540F3ECEB}
// *********************************************************************//
  IJwPrinterMapping = interface(IJwGenericMapping)
    ['{0BCBBD6C-1DBD-41D9-A2D4-DC9540F3ECEB}']
  end;

// *********************************************************************//
// DispIntf:  IJwPrinterMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0BCBBD6C-1DBD-41D9-A2D4-DC9540F3ECEB}
// *********************************************************************//
  IJwPrinterMappingDisp = dispinterface
    ['{0BCBBD6C-1DBD-41D9-A2D4-DC9540F3ECEB}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwShareMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {794FDFE5-4A60-4394-BEAB-D2FCFEE0984C}
// *********************************************************************//
  IJwShareMapping = interface(IJwGenericMapping)
    ['{794FDFE5-4A60-4394-BEAB-D2FCFEE0984C}']
  end;

// *********************************************************************//
// DispIntf:  IJwShareMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {794FDFE5-4A60-4394-BEAB-D2FCFEE0984C}
// *********************************************************************//
  IJwShareMappingDisp = dispinterface
    ['{794FDFE5-4A60-4394-BEAB-D2FCFEE0984C}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwProcessMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {993CDB31-A24C-4EEA-BDD0-10BA1AB3D7E1}
// *********************************************************************//
  IJwProcessMapping = interface(IJwGenericMapping)
    ['{993CDB31-A24C-4EEA-BDD0-10BA1AB3D7E1}']
  end;

// *********************************************************************//
// DispIntf:  IJwProcessMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {993CDB31-A24C-4EEA-BDD0-10BA1AB3D7E1}
// *********************************************************************//
  IJwProcessMappingDisp = dispinterface
    ['{993CDB31-A24C-4EEA-BDD0-10BA1AB3D7E1}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwThreadMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {389BEEFC-7562-4E53-878E-6E3E61A510C0}
// *********************************************************************//
  IJwThreadMapping = interface(IJwGenericMapping)
    ['{389BEEFC-7562-4E53-878E-6E3E61A510C0}']
  end;

// *********************************************************************//
// DispIntf:  IJwThreadMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {389BEEFC-7562-4E53-878E-6E3E61A510C0}
// *********************************************************************//
  IJwThreadMappingDisp = dispinterface
    ['{389BEEFC-7562-4E53-878E-6E3E61A510C0}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwJobMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D5B64FC6-3A49-483C-9D7B-34A04E6E97C2}
// *********************************************************************//
  IJwJobMapping = interface(IJwGenericMapping)
    ['{D5B64FC6-3A49-483C-9D7B-34A04E6E97C2}']
  end;

// *********************************************************************//
// DispIntf:  IJwJobMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D5B64FC6-3A49-483C-9D7B-34A04E6E97C2}
// *********************************************************************//
  IJwJobMappingDisp = dispinterface
    ['{D5B64FC6-3A49-483C-9D7B-34A04E6E97C2}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwSemaphoreMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {28EEA5E2-21E7-4CB3-A585-6D4FF82BABB5}
// *********************************************************************//
  IJwSemaphoreMapping = interface(IJwGenericMapping)
    ['{28EEA5E2-21E7-4CB3-A585-6D4FF82BABB5}']
  end;

// *********************************************************************//
// DispIntf:  IJwSemaphoreMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {28EEA5E2-21E7-4CB3-A585-6D4FF82BABB5}
// *********************************************************************//
  IJwSemaphoreMappingDisp = dispinterface
    ['{28EEA5E2-21E7-4CB3-A585-6D4FF82BABB5}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwEventMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {64357431-6565-494D-83E5-0FAB085A9FF5}
// *********************************************************************//
  IJwEventMapping = interface(IJwGenericMapping)
    ['{64357431-6565-494D-83E5-0FAB085A9FF5}']
  end;

// *********************************************************************//
// DispIntf:  IJwEventMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {64357431-6565-494D-83E5-0FAB085A9FF5}
// *********************************************************************//
  IJwEventMappingDisp = dispinterface
    ['{64357431-6565-494D-83E5-0FAB085A9FF5}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwMutexMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F15F1B47-4DB8-4F6F-A362-51B88DA5C5C4}
// *********************************************************************//
  IJwMutexMapping = interface(IJwGenericMapping)
    ['{F15F1B47-4DB8-4F6F-A362-51B88DA5C5C4}']
  end;

// *********************************************************************//
// DispIntf:  IJwMutexMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F15F1B47-4DB8-4F6F-A362-51B88DA5C5C4}
// *********************************************************************//
  IJwMutexMappingDisp = dispinterface
    ['{F15F1B47-4DB8-4F6F-A362-51B88DA5C5C4}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwFileMapMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {991D7CAB-58A1-4575-9F3B-CC34D6E2DE00}
// *********************************************************************//
  IJwFileMapMapping = interface(IJwGenericMapping)
    ['{991D7CAB-58A1-4575-9F3B-CC34D6E2DE00}']
  end;

// *********************************************************************//
// DispIntf:  IJwFileMapMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {991D7CAB-58A1-4575-9F3B-CC34D6E2DE00}
// *********************************************************************//
  IJwFileMapMappingDisp = dispinterface
    ['{991D7CAB-58A1-4575-9F3B-CC34D6E2DE00}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwTimerMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {10F1574D-F097-4081-8DFD-F956581FBA2C}
// *********************************************************************//
  IJwTimerMapping = interface(IJwGenericMapping)
    ['{10F1574D-F097-4081-8DFD-F956581FBA2C}']
  end;

// *********************************************************************//
// DispIntf:  IJwTimerMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {10F1574D-F097-4081-8DFD-F956581FBA2C}
// *********************************************************************//
  IJwTimerMappingDisp = dispinterface
    ['{10F1574D-F097-4081-8DFD-F956581FBA2C}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwTokenMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8F1F8AF3-42EA-4E51-93B0-3C5E04B47C49}
// *********************************************************************//
  IJwTokenMapping = interface(IJwGenericMapping)
    ['{8F1F8AF3-42EA-4E51-93B0-3C5E04B47C49}']
  end;

// *********************************************************************//
// DispIntf:  IJwTokenMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {8F1F8AF3-42EA-4E51-93B0-3C5E04B47C49}
// *********************************************************************//
  IJwTokenMappingDisp = dispinterface
    ['{8F1F8AF3-42EA-4E51-93B0-3C5E04B47C49}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwPipeMapping
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {48260C48-B275-4C7C-BDB5-4678E1E89160}
// *********************************************************************//
  IJwPipeMapping = interface(IJwGenericMapping)
    ['{48260C48-B275-4C7C-BDB5-4678E1E89160}']
  end;

// *********************************************************************//
// DispIntf:  IJwPipeMappingDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {48260C48-B275-4C7C-BDB5-4678E1E89160}
// *********************************************************************//
  IJwPipeMappingDisp = dispinterface
    ['{48260C48-B275-4C7C-BDB5-4678E1E89160}']
    function GetMapping: {??TGenericMapping}OleVariant; dispid 201;
    function MapAccessMaskToString(AccessMask: LongWord): WideString; dispid 202;
    function GetAccessNames(out Count: LongWord): {??PChar}OleVariant; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwLogServer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {213E956A-7BF3-4007-8191-1D743C818435}
// *********************************************************************//
  IJwLogServer = interface(IDispatch)
    ['{213E956A-7BF3-4007-8191-1D743C818435}']
    function Get_WriterClass: IJwWriterClass; safecall;
    procedure Set_WriterClass(const Value: IJwWriterClass); safecall;
    function Get_LogTypes: OleVariant; safecall;
    procedure Set_LogTypes(Value: OleVariant); safecall;
    function Connect(EnterType: JwEnumEnterType; const ClassName: WideString; 
                     const MethodName: WideString; const FileName: WideString; 
                     const LogMessage: WideString): IJwLogClient; safecall;
    property WriterClass: IJwWriterClass read Get_WriterClass write Set_WriterClass;
    property LogTypes: OleVariant read Get_LogTypes write Set_LogTypes;
  end;

// *********************************************************************//
// DispIntf:  IJwLogServerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {213E956A-7BF3-4007-8191-1D743C818435}
// *********************************************************************//
  IJwLogServerDisp = dispinterface
    ['{213E956A-7BF3-4007-8191-1D743C818435}']
    property WriterClass: IJwWriterClass dispid 201;
    property LogTypes: OleVariant dispid 202;
    function Connect(EnterType: JwEnumEnterType; const ClassName: WideString; 
                     const MethodName: WideString; const FileName: WideString; 
                     const LogMessage: WideString): IJwLogClient; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwWriterClass
// Flags:     (320) Dual OleAutomation
// GUID:      {E225CFE1-FCAB-4109-8D64-B845D0C8FDD5}
// *********************************************************************//
  IJwWriterClass = interface(IUnknown)
    ['{E225CFE1-FCAB-4109-8D64-B845D0C8FDD5}']
    function WriteSingleTag(IndentLevel: SYSINT; const TagName: WideString; 
                            const Value: WideString; Attributes: OleVariant): WideString; safecall;
    function StartWriteMultipleTags(IndentLevel: SYSINT; const TagName: WideString; 
                                    Attributes: OleVariant): WideString; safecall;
    function EndWriteMultipleTags: WideString; safecall;
    procedure Done; safecall;
    function CreateObject: IJwWriterClass; safecall;
  end;

// *********************************************************************//
// DispIntf:  IJwWriterClassDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {E225CFE1-FCAB-4109-8D64-B845D0C8FDD5}
// *********************************************************************//
  IJwWriterClassDisp = dispinterface
    ['{E225CFE1-FCAB-4109-8D64-B845D0C8FDD5}']
    function WriteSingleTag(IndentLevel: SYSINT; const TagName: WideString; 
                            const Value: WideString; Attributes: OleVariant): WideString; dispid 201;
    function StartWriteMultipleTags(IndentLevel: SYSINT; const TagName: WideString; 
                                    Attributes: OleVariant): WideString; dispid 202;
    function EndWriteMultipleTags: WideString; dispid 203;
    procedure Done; dispid 204;
    function CreateObject: IJwWriterClass; dispid 101;
  end;

// *********************************************************************//
// Schnittstelle: IJwLogClient
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {742092DA-9FD9-420A-84AF-56D6EA8508D4}
// *********************************************************************//
  IJwLogClient = interface(IDispatch)
    ['{742092DA-9FD9-420A-84AF-56D6EA8508D4}']
    procedure Log(LogType: JwEnumLogType; const LogMessage: WideString); safecall;
    procedure Signal(LogType: JwEnumSignalType; const Source: WideString; const Target: WideString; 
                     const LogMessage: WideString); safecall;
    procedure Memory(LogType: JwEnumMemoryType; const MemType: WideString; 
                     const LogMessage: WideString); safecall;
    procedure Exception(Exception: OleVariant); safecall;
  end;

// *********************************************************************//
// DispIntf:  IJwLogClientDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {742092DA-9FD9-420A-84AF-56D6EA8508D4}
// *********************************************************************//
  IJwLogClientDisp = dispinterface
    ['{742092DA-9FD9-420A-84AF-56D6EA8508D4}']
    procedure Log(LogType: JwEnumLogType; const LogMessage: WideString); dispid 201;
    procedure Signal(LogType: JwEnumSignalType; const Source: WideString; const Target: WideString; 
                     const LogMessage: WideString); dispid 202;
    procedure Memory(LogType: JwEnumMemoryType; const MemType: WideString; 
                     const LogMessage: WideString); dispid 203;
    procedure Exception(Exception: OleVariant); dispid 204;
  end;

// *********************************************************************//
// Schnittstelle: ITestInt
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2B0D3F0-F029-4550-A7CC-07BC612BF10B}
// *********************************************************************//
  ITestInt = interface(IDispatch)
    ['{D2B0D3F0-F029-4550-A7CC-07BC612BF10B}']
  end;

// *********************************************************************//
// DispIntf:  ITestIntDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {D2B0D3F0-F029-4550-A7CC-07BC612BF10B}
// *********************************************************************//
  ITestIntDisp = dispinterface
    ['{D2B0D3F0-F029-4550-A7CC-07BC612BF10B}']
  end;

// *********************************************************************//
// Schnittstelle: IJwXMLAttribute
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B7C4FFC9-32AF-4341-9434-4F33FEDF1707}
// *********************************************************************//
  IJwXMLAttribute = interface(IDispatch)
    ['{B7C4FFC9-32AF-4341-9434-4F33FEDF1707}']
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Value: WideString; safecall;
    procedure Set_Value(const Value: WideString); safecall;
    function ToString: WideString; safecall;
    property Name: WideString read Get_Name write Set_Name;
    property Value: WideString read Get_Value write Set_Value;
  end;

// *********************************************************************//
// DispIntf:  IJwXMLAttributeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B7C4FFC9-32AF-4341-9434-4F33FEDF1707}
// *********************************************************************//
  IJwXMLAttributeDisp = dispinterface
    ['{B7C4FFC9-32AF-4341-9434-4F33FEDF1707}']
    property Name: WideString dispid 201;
    property Value: WideString dispid 202;
    function ToString: WideString; dispid 203;
  end;

// *********************************************************************//
// Schnittstelle: IJwEventType
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B95E0B5E-F424-4AC4-98F0-B1C79C19AD11}
// *********************************************************************//
  IJwEventType = interface(IDispatch)
    ['{B95E0B5E-F424-4AC4-98F0-B1C79C19AD11}']
    function Get_TagName: JwXMLLogTag; safecall;
    procedure Set_TagName(Value: JwXMLLogTag); safecall;
    function Get_TypeValue: SYSINT; safecall;
    procedure Set_TypeValue(Value: SYSINT); safecall;
    function ToString: WideString; safecall;
    property TagName: JwXMLLogTag read Get_TagName write Set_TagName;
    property TypeValue: SYSINT read Get_TypeValue write Set_TypeValue;
  end;

// *********************************************************************//
// DispIntf:  IJwEventTypeDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B95E0B5E-F424-4AC4-98F0-B1C79C19AD11}
// *********************************************************************//
  IJwEventTypeDisp = dispinterface
    ['{B95E0B5E-F424-4AC4-98F0-B1C79C19AD11}']
    property TagName: JwXMLLogTag dispid 201;
    property TypeValue: SYSINT dispid 202;
    function ToString: WideString; dispid 203;
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

// *********************************************************************//
// Die Klasse CoJwGenericMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwGenericMapping, dargestellt von
// CoClass JwGenericMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwGenericMapping = class
    class function Create: IJwGenericMapping;
    class function CreateRemote(const MachineName: string): IJwGenericMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwFileFolderMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwFileFolderMapping, dargestellt von
// CoClass JwFileFolderMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwFileFolderMapping = class
    class function Create: IJwFileFolderMapping;
    class function CreateRemote(const MachineName: string): IJwFileFolderMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwFileMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwFileMapping, dargestellt von
// CoClass JwFileMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwFileMapping = class
    class function Create: IJwFileMapping;
    class function CreateRemote(const MachineName: string): IJwFileMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwRegistryMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwRegistryMapping, dargestellt von
// CoClass JwRegistryMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwRegistryMapping = class
    class function Create: IJwRegistryMapping;
    class function CreateRemote(const MachineName: string): IJwRegistryMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwWinStationMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwWinStationMapping, dargestellt von
// CoClass JwWinStationMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwWinStationMapping = class
    class function Create: IJwWinStationMapping;
    class function CreateRemote(const MachineName: string): IJwWinStationMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwDesktopMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwDesktopMapping, dargestellt von
// CoClass JwDesktopMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwDesktopMapping = class
    class function Create: IJwDesktopMapping;
    class function CreateRemote(const MachineName: string): IJwDesktopMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwServiceMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwServiceMapping, dargestellt von
// CoClass JwServiceMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwServiceMapping = class
    class function Create: IJwServiceMapping;
    class function CreateRemote(const MachineName: string): IJwServiceMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwServiceMappingManager stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwServiceManagerMapping, dargestellt von
// CoClass JwServiceMappingManager, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwServiceMappingManager = class
    class function Create: IJwServiceManagerMapping;
    class function CreateRemote(const MachineName: string): IJwServiceManagerMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwPrinterMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwPrinterMapping, dargestellt von
// CoClass JwPrinterMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwPrinterMapping = class
    class function Create: IJwPrinterMapping;
    class function CreateRemote(const MachineName: string): IJwPrinterMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwShareMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwShareMapping, dargestellt von
// CoClass JwShareMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwShareMapping = class
    class function Create: IJwShareMapping;
    class function CreateRemote(const MachineName: string): IJwShareMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwProcessMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwProcessMapping, dargestellt von
// CoClass JwProcessMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwProcessMapping = class
    class function Create: IJwProcessMapping;
    class function CreateRemote(const MachineName: string): IJwProcessMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwThreadMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwThreadMapping, dargestellt von
// CoClass JwThreadMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwThreadMapping = class
    class function Create: IJwThreadMapping;
    class function CreateRemote(const MachineName: string): IJwThreadMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwJobMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwJobMapping, dargestellt von
// CoClass JwJobMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwJobMapping = class
    class function Create: IJwJobMapping;
    class function CreateRemote(const MachineName: string): IJwJobMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwSemaphoreMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwSemaphoreMapping, dargestellt von
// CoClass JwSemaphoreMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwSemaphoreMapping = class
    class function Create: IJwSemaphoreMapping;
    class function CreateRemote(const MachineName: string): IJwSemaphoreMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwEventMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwEventMapping, dargestellt von
// CoClass JwEventMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwEventMapping = class
    class function Create: IJwEventMapping;
    class function CreateRemote(const MachineName: string): IJwEventMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwMutexMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwMutexMapping, dargestellt von
// CoClass JwMutexMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwMutexMapping = class
    class function Create: IJwMutexMapping;
    class function CreateRemote(const MachineName: string): IJwMutexMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwFileMapMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwFileMapMapping, dargestellt von
// CoClass JwFileMapMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwFileMapMapping = class
    class function Create: IJwFileMapMapping;
    class function CreateRemote(const MachineName: string): IJwFileMapMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwTimerMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwTimerMapping, dargestellt von
// CoClass JwTimerMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwTimerMapping = class
    class function Create: IJwTimerMapping;
    class function CreateRemote(const MachineName: string): IJwTimerMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwTokenMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwTokenMapping, dargestellt von
// CoClass JwTokenMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwTokenMapping = class
    class function Create: IJwTokenMapping;
    class function CreateRemote(const MachineName: string): IJwTokenMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwPipeMapping stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwPipeMapping, dargestellt von
// CoClass JwPipeMapping, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwPipeMapping = class
    class function Create: IJwPipeMapping;
    class function CreateRemote(const MachineName: string): IJwPipeMapping;
  end;

// *********************************************************************//
// Die Klasse CoJwLogServer stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwLogServer, dargestellt von
// CoClass JwLogServer, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwLogServer = class
    class function Create: IJwLogServer;
    class function CreateRemote(const MachineName: string): IJwLogServer;
  end;

// *********************************************************************//
// Die Klasse CoJwLogClient stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwLogClient, dargestellt von
// CoClass JwLogClient, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwLogClient = class
    class function Create: IJwLogClient;
    class function CreateRemote(const MachineName: string): IJwLogClient;
  end;

// *********************************************************************//
// Die Klasse CoTestInt stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle ITestInt, dargestellt von
// CoClass TestInt, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoTestInt = class
    class function Create: ITestInt;
    class function CreateRemote(const MachineName: string): ITestInt;
  end;

// *********************************************************************//
// Die Klasse CoJwXMLAttribute stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwXMLAttribute, dargestellt von
// CoClass JwXMLAttribute, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwXMLAttribute = class
    class function Create: IJwXMLAttribute;
    class function CreateRemote(const MachineName: string): IJwXMLAttribute;
  end;

// *********************************************************************//
// Die Klasse CoJwEventType stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwEventType, dargestellt von
// CoClass JwEventType, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwEventType = class
    class function Create: IJwEventType;
    class function CreateRemote(const MachineName: string): IJwEventType;
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

class function CoJwGenericMapping.Create: IJwGenericMapping;
begin
  Result := CreateComObject(CLASS_JwGenericMapping) as IJwGenericMapping;
end;

class function CoJwGenericMapping.CreateRemote(const MachineName: string): IJwGenericMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwGenericMapping) as IJwGenericMapping;
end;

class function CoJwFileFolderMapping.Create: IJwFileFolderMapping;
begin
  Result := CreateComObject(CLASS_JwFileFolderMapping) as IJwFileFolderMapping;
end;

class function CoJwFileFolderMapping.CreateRemote(const MachineName: string): IJwFileFolderMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwFileFolderMapping) as IJwFileFolderMapping;
end;

class function CoJwFileMapping.Create: IJwFileMapping;
begin
  Result := CreateComObject(CLASS_JwFileMapping) as IJwFileMapping;
end;

class function CoJwFileMapping.CreateRemote(const MachineName: string): IJwFileMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwFileMapping) as IJwFileMapping;
end;

class function CoJwRegistryMapping.Create: IJwRegistryMapping;
begin
  Result := CreateComObject(CLASS_JwRegistryMapping) as IJwRegistryMapping;
end;

class function CoJwRegistryMapping.CreateRemote(const MachineName: string): IJwRegistryMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwRegistryMapping) as IJwRegistryMapping;
end;

class function CoJwWinStationMapping.Create: IJwWinStationMapping;
begin
  Result := CreateComObject(CLASS_JwWinStationMapping) as IJwWinStationMapping;
end;

class function CoJwWinStationMapping.CreateRemote(const MachineName: string): IJwWinStationMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwWinStationMapping) as IJwWinStationMapping;
end;

class function CoJwDesktopMapping.Create: IJwDesktopMapping;
begin
  Result := CreateComObject(CLASS_JwDesktopMapping) as IJwDesktopMapping;
end;

class function CoJwDesktopMapping.CreateRemote(const MachineName: string): IJwDesktopMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwDesktopMapping) as IJwDesktopMapping;
end;

class function CoJwServiceMapping.Create: IJwServiceMapping;
begin
  Result := CreateComObject(CLASS_JwServiceMapping) as IJwServiceMapping;
end;

class function CoJwServiceMapping.CreateRemote(const MachineName: string): IJwServiceMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwServiceMapping) as IJwServiceMapping;
end;

class function CoJwServiceMappingManager.Create: IJwServiceManagerMapping;
begin
  Result := CreateComObject(CLASS_JwServiceMappingManager) as IJwServiceManagerMapping;
end;

class function CoJwServiceMappingManager.CreateRemote(const MachineName: string): IJwServiceManagerMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwServiceMappingManager) as IJwServiceManagerMapping;
end;

class function CoJwPrinterMapping.Create: IJwPrinterMapping;
begin
  Result := CreateComObject(CLASS_JwPrinterMapping) as IJwPrinterMapping;
end;

class function CoJwPrinterMapping.CreateRemote(const MachineName: string): IJwPrinterMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwPrinterMapping) as IJwPrinterMapping;
end;

class function CoJwShareMapping.Create: IJwShareMapping;
begin
  Result := CreateComObject(CLASS_JwShareMapping) as IJwShareMapping;
end;

class function CoJwShareMapping.CreateRemote(const MachineName: string): IJwShareMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwShareMapping) as IJwShareMapping;
end;

class function CoJwProcessMapping.Create: IJwProcessMapping;
begin
  Result := CreateComObject(CLASS_JwProcessMapping) as IJwProcessMapping;
end;

class function CoJwProcessMapping.CreateRemote(const MachineName: string): IJwProcessMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwProcessMapping) as IJwProcessMapping;
end;

class function CoJwThreadMapping.Create: IJwThreadMapping;
begin
  Result := CreateComObject(CLASS_JwThreadMapping) as IJwThreadMapping;
end;

class function CoJwThreadMapping.CreateRemote(const MachineName: string): IJwThreadMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwThreadMapping) as IJwThreadMapping;
end;

class function CoJwJobMapping.Create: IJwJobMapping;
begin
  Result := CreateComObject(CLASS_JwJobMapping) as IJwJobMapping;
end;

class function CoJwJobMapping.CreateRemote(const MachineName: string): IJwJobMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwJobMapping) as IJwJobMapping;
end;

class function CoJwSemaphoreMapping.Create: IJwSemaphoreMapping;
begin
  Result := CreateComObject(CLASS_JwSemaphoreMapping) as IJwSemaphoreMapping;
end;

class function CoJwSemaphoreMapping.CreateRemote(const MachineName: string): IJwSemaphoreMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwSemaphoreMapping) as IJwSemaphoreMapping;
end;

class function CoJwEventMapping.Create: IJwEventMapping;
begin
  Result := CreateComObject(CLASS_JwEventMapping) as IJwEventMapping;
end;

class function CoJwEventMapping.CreateRemote(const MachineName: string): IJwEventMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwEventMapping) as IJwEventMapping;
end;

class function CoJwMutexMapping.Create: IJwMutexMapping;
begin
  Result := CreateComObject(CLASS_JwMutexMapping) as IJwMutexMapping;
end;

class function CoJwMutexMapping.CreateRemote(const MachineName: string): IJwMutexMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwMutexMapping) as IJwMutexMapping;
end;

class function CoJwFileMapMapping.Create: IJwFileMapMapping;
begin
  Result := CreateComObject(CLASS_JwFileMapMapping) as IJwFileMapMapping;
end;

class function CoJwFileMapMapping.CreateRemote(const MachineName: string): IJwFileMapMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwFileMapMapping) as IJwFileMapMapping;
end;

class function CoJwTimerMapping.Create: IJwTimerMapping;
begin
  Result := CreateComObject(CLASS_JwTimerMapping) as IJwTimerMapping;
end;

class function CoJwTimerMapping.CreateRemote(const MachineName: string): IJwTimerMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwTimerMapping) as IJwTimerMapping;
end;

class function CoJwTokenMapping.Create: IJwTokenMapping;
begin
  Result := CreateComObject(CLASS_JwTokenMapping) as IJwTokenMapping;
end;

class function CoJwTokenMapping.CreateRemote(const MachineName: string): IJwTokenMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwTokenMapping) as IJwTokenMapping;
end;

class function CoJwPipeMapping.Create: IJwPipeMapping;
begin
  Result := CreateComObject(CLASS_JwPipeMapping) as IJwPipeMapping;
end;

class function CoJwPipeMapping.CreateRemote(const MachineName: string): IJwPipeMapping;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwPipeMapping) as IJwPipeMapping;
end;

class function CoJwLogServer.Create: IJwLogServer;
begin
  Result := CreateComObject(CLASS_JwLogServer) as IJwLogServer;
end;

class function CoJwLogServer.CreateRemote(const MachineName: string): IJwLogServer;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwLogServer) as IJwLogServer;
end;

class function CoJwLogClient.Create: IJwLogClient;
begin
  Result := CreateComObject(CLASS_JwLogClient) as IJwLogClient;
end;

class function CoJwLogClient.CreateRemote(const MachineName: string): IJwLogClient;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwLogClient) as IJwLogClient;
end;

class function CoTestInt.Create: ITestInt;
begin
  Result := CreateComObject(CLASS_TestInt) as ITestInt;
end;

class function CoTestInt.CreateRemote(const MachineName: string): ITestInt;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TestInt) as ITestInt;
end;

class function CoJwXMLAttribute.Create: IJwXMLAttribute;
begin
  Result := CreateComObject(CLASS_JwXMLAttribute) as IJwXMLAttribute;
end;

class function CoJwXMLAttribute.CreateRemote(const MachineName: string): IJwXMLAttribute;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwXMLAttribute) as IJwXMLAttribute;
end;

class function CoJwEventType.Create: IJwEventType;
begin
  Result := CreateComObject(CLASS_JwEventType) as IJwEventType;
end;

class function CoJwEventType.CreateRemote(const MachineName: string): IJwEventType;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwEventType) as IJwEventType;
end;

end.
