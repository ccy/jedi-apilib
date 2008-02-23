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
// Datei generiert am 22.02.2008 23:59:44 aus der unten beschriebenen Typbibliothek.

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

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten Enumerationen         
// *********************************************************************//
// Konstanten für enum JwCoFacilities
type
  JwCoFacilities = TOleEnum;
const
  FACILITY_JWSCL = $00000065;

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

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass zu ihrer Standardschnittstelle        
// zugewiesen)                                                            
// *********************************************************************//
  JwSid = IJwSid;
  JwSidList = IJwSidList;
  JwTest2 = IJwTest;


// *********************************************************************// 
// Deklaration von  Strukturen, Unions und Aliasen.                        
// *********************************************************************// 

  PCoSid = PChar; 
  PCoSidAndAttributes = PChar; 
  PCoTokenGroups = PChar; 

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
    function GetSidStream: IUnknown; safecall;
    function Get_UserName: WideString; safecall;
    function IsEqualSid(const Sid: IJwSid): WordBool; safecall;
    function Get_StringSid: WideString; safecall;
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
    function GetSidStream: IUnknown; dispid 221;
    property UserName: WideString readonly dispid 222;
    function IsEqualSid(const Sid: IJwSid): WordBool; dispid 223;
    property StringSid: WideString readonly dispid 224;
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
    function Get_Item: IJwSid; safecall;
    function Get__NewEnum: OleVariant; safecall;
    property Count: LongWord read Get_Count;
    property Item: IJwSid read Get_Item;
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
    property Item: IJwSid readonly dispid 0;
    property _NewEnum: OleVariant readonly dispid -4;
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

end.
