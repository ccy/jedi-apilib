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
// Datei generiert am 10.02.2008 20:23:16 aus der unten beschriebenen Typbibliothek.

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

  IID_IJwCoSid: TGUID = '{C63F1AD8-9436-45C8-A106-0861492D3D1D}';
  CLASS_JwCoSid: TGUID = '{B20B11D4-8157-47C3-A105-E2FF52E4034D}';
  IID_IJwCoSidList: TGUID = '{F4F0B142-B99B-46F7-825A-DC64454636DB}';
  CLASS_JwCoSidList: TGUID = '{6EB7C24D-2BB9-48B7-9C7C-5DF0674BA74D}';

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten Enumerationen         
// *********************************************************************//
// Konstanten für enum JwCoFacilities
type
  JwCoFacilities = TOleEnum;
const
  FACILITY_JWSCL = $00000065;

// Konstanten für enum JwCoSidAttribute
type
  JwCoSidAttribute = TOleEnum;
const
  sidaCoUnknown = $00000000;
  sidaCoGroupMandatory = $00000001;
  sidaCoGroupEnabledByDefault = $00000002;
  sidaCoGroupEnabled = $00000003;
  sidaCoGroupOwner = $00000004;
  sidaCoGroupUseForDenyOnly = $00000005;
  sidaCoGroupLogonId = $00000006;
  sidaCoGroupResource = $00000007;
  sidaCoGroupIntegrity = $00000008;
  sidaCoGroupIntegrityEnabled = $00000009;
  sidaCoPad0 = $0000000A;
  sidaCoPad1 = $0000000B;
  sidaCoPad2 = $0000000C;
  sidaCoPad3 = $0000000D;
  sidaCoPad4 = $0000000E;
  sidaCoPad5 = $0000000F;

type

// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen         
// *********************************************************************//
  IJwCoSid = interface;
  IJwCoSidList = interface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass zu ihrer Standardschnittstelle        
// zugewiesen)                                                            
// *********************************************************************//
  JwCoSid = IJwCoSid;
  JwCoSidList = IJwCoSidList;


// *********************************************************************// 
// Deklaration von  Strukturen, Unions und Aliasen.                        
// *********************************************************************// 

  PCoSid = PChar; 
  PCoSidAndAttributes = PChar; 
  PCoTokenGroups = PChar; 

// *********************************************************************//
// Schnittstelle: IJwCoSid
// Flags:     (4352) OleAutomation Dispatchable
// GUID:      {C63F1AD8-9436-45C8-A106-0861492D3D1D}
// *********************************************************************//
  IJwCoSid = interface(IDispatch)
    ['{C63F1AD8-9436-45C8-A106-0861492D3D1D}']
    function InitBySid(SidPtr: PCoSid): HResult; stdcall;
    function GetSidPtr(out SidDataPtr: PCoSid; out SidDataSize: Integer): HResult; stdcall;
    function InitByStream(const SidAsStream: IUnknown): HResult; stdcall;
    function GetSidStream(out SidAsStream: IUnknown): HResult; stdcall;
    function InitByBinarySid(const BinarySid: WideString): HResult; stdcall;
    function InitByName(const SystemName: WideString; const AccountName: WideString): HResult; stdcall;
    function Get_StringSid(out Value: WideString): HResult; stdcall;
    function InitByJwSid(const Sid: IJwCoSid): HResult; stdcall;
    function InitBySidAndAttributes(SidAndAttributes: PCoSidAndAttributes): HResult; stdcall;
    function InitByAuthorities(Authorities: OleVariant; Identifier: OleVariant): HResult; stdcall;
    function InitByWellKnownSid(SidType: Integer): HResult; stdcall;
    function Get_Sid(out Value: PChar): HResult; stdcall;
    function Get_SubAuthorityCount(out Value: SYSUINT): HResult; stdcall;
    function Get_SubAuthorityArray(out Value: OleVariant): HResult; stdcall;
    function GetSubAuthorityByIndex(Index: SYSUINT; out Value: LongWord): HResult; stdcall;
    function Get_IdentifierAttributesArray(out Value: OleVariant): HResult; stdcall;
    function Get_IdentifierAttributesCount(out Value: LongWord): HResult; stdcall;
    function GetIdentifierAttributeByIndex(Index: SYSUINT; out Value: Shortint): HResult; stdcall;
    function Get_SidLength(out Value: LongWord): HResult; stdcall;
    function Get_IsWellKnownSidType(out Value: WordBool): HResult; stdcall;
    function Get_WellKnownSidType(out Value: Integer): HResult; stdcall;
    function GetAccountName(const SystemName: WideString): HResult; stdcall;
    function GetAccountDomainName(const SystemName: WideString): HResult; stdcall;
    function GetAccountNameUse(const SystemName: WideString): HResult; stdcall;
    function GetChachedUserFromSid(out UserName: WideString): HResult; stdcall;
    function Get_Attributes(out Value: LongWord): HResult; stdcall;
    function Set_Attributes(Value: LongWord): HResult; stdcall;
    function Get_AttributesByType(out Value: OleVariant): HResult; stdcall;
    function Set_AttributesByType(Value: OleVariant): HResult; stdcall;
    function Get_CachedSystemName(out Value: WideString): HResult; stdcall;
    function GetInternalSid(out Value: PChar): HResult; stdcall;
    function IsStandardSid(out Value: WordBool): HResult; stdcall;
  end;

// *********************************************************************//
// Schnittstelle: IJwCoSidList
// Flags:     (4352) OleAutomation Dispatchable
// GUID:      {F4F0B142-B99B-46F7-825A-DC64454636DB}
// *********************************************************************//
  IJwCoSidList = interface(IDispatch)
    ['{F4F0B142-B99B-46F7-825A-DC64454636DB}']
    function Add(const Sid: IJwCoSid): HResult; stdcall;
    function FindSid(const Sid: IJwCoSid; StartPos: SYSINT; UsePrefix: WordBool; out Index: Integer): HResult; stdcall;
    function Init: HResult; stdcall;
    function InitByTokenGroups(TokenGroups: PCoTokenGroups): HResult; stdcall;
    function InitBySidAndAttributes(SidAndAttributes: OleVariant): HResult; stdcall;
    function Get_First(out Value: IJwCoSid): HResult; stdcall;
    function Get_Last(out Value: IJwCoSid): HResult; stdcall;
    function Insert(Index: Integer; const Sid: IJwCoSid): HResult; stdcall;
    function Remove(const Sid: IJwCoSid): HResult; stdcall;
    function IndexOf(const Sid: IJwCoSid; out Index: LongWord): HResult; stdcall;
    function ToString(out Value: WideString): HResult; stdcall;
    function Get_Items(Index: LongWord; out Value: IJwCoSid): HResult; stdcall;
    function Clear: HResult; stdcall;
    function Delete(Index: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Die Klasse CoJwCoSid stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwCoSid, dargestellt von
// CoClass JwCoSid, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwCoSid = class
    class function Create: IJwCoSid;
    class function CreateRemote(const MachineName: string): IJwCoSid;
  end;

// *********************************************************************//
// Die Klasse CoJwCoSidList stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IJwCoSidList, dargestellt von
// CoClass JwCoSidList, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoJwCoSidList = class
    class function Create: IJwCoSidList;
    class function CreateRemote(const MachineName: string): IJwCoSidList;
  end;

implementation

uses ComObj;

class function CoJwCoSid.Create: IJwCoSid;
begin
  Result := CreateComObject(CLASS_JwCoSid) as IJwCoSid;
end;

class function CoJwCoSid.CreateRemote(const MachineName: string): IJwCoSid;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwCoSid) as IJwCoSid;
end;

class function CoJwCoSidList.Create: IJwCoSidList;
begin
  Result := CreateComObject(CLASS_JwCoSidList) as IJwCoSidList;
end;

class function CoJwCoSidList.CreateRemote(const MachineName: string): IJwCoSidList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_JwCoSidList) as IJwCoSidList;
end;

end.
