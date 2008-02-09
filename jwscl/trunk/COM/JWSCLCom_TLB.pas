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
// Datei generiert am 09.02.2008 00:25:38 aus der unten beschriebenen Typbibliothek.

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
  IJwCoSid = interface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass zu ihrer Standardschnittstelle        
// zugewiesen)                                                            
// *********************************************************************//
  JwCoSid = IJwCoSid;


// *********************************************************************// 
// Deklaration von  Strukturen, Unions und Aliasen.                        
// *********************************************************************// 

  PCoSid = PChar; 

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

end.
