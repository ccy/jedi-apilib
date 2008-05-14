unit XPElevationDLL_TLB;

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
// Datei generiert am 12.05.2008 22:56:31 aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib: P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\jedi-api-lib\jwscl\branches\XPElevation_2\XPElevationDLL\XPElevationDLL.tlb (1)
// LIBID: {9A8BCDB2-A46B-48B5-9466-2F7DE4B4B870}
// LCID: 0
// Hilfedatei: 
// Hilfe-String: XPElevationDLL Bibliothek
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
  XPElevationDLLMajorVersion = 1;
  XPElevationDLLMinorVersion = 0;

  LIBID_XPElevationDLL: TGUID = '{9A8BCDB2-A46B-48B5-9466-2F7DE4B4B870}';

  IID_IXPElevation: TGUID = '{F986436F-64E9-4292-84AC-F31D6776C619}';
  CLASS_XPElevation: TGUID = '{DCF18AFA-3B25-417F-9E58-6E74D47F0D7C}';
  IID_IXPElevationControl: TGUID = '{5D419256-FB83-4CE0-B316-808DD0035B99}';
  CLASS_XPElevationControl: TGUID = '{3A0CF1B7-0E50-4B71-B296-36EC26891346}';

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten Enumerationen         
// *********************************************************************//
// Konstanten für enum XPElevationError
type
  XPElevationError = TOleEnum;
const
  E_INVALID_USER = $0D30B335;

type

// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen         
// *********************************************************************//
  IXPElevation = interface;
  IXPElevationControl = interface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass zu ihrer Standardschnittstelle        
// zugewiesen)                                                            
// *********************************************************************//
  XPElevation = IXPElevation;
  XPElevationControl = IXPElevationControl;


// *********************************************************************//
// Schnittstelle: IXPElevation
// Flags:     (256) OleAutomation
// GUID:      {F986436F-64E9-4292-84AC-F31D6776C619}
// *********************************************************************//
  IXPElevation = interface(IUnknown)
    ['{F986436F-64E9-4292-84AC-F31D6776C619}']
    function ExecuteProcess(const ApplicationPath: WideString; const Parameters: WideString; 
                            const CurrentDirectory: WideString; ParentWindow: LongWord; 
                            Flags: LongWord; out PID: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Schnittstelle: IXPElevationControl
// Flags:     (256) OleAutomation
// GUID:      {5D419256-FB83-4CE0-B316-808DD0035B99}
// *********************************************************************//
  IXPElevationControl = interface(IUnknown)
    ['{5D419256-FB83-4CE0-B316-808DD0035B99}']
    function Connect: HResult; stdcall;
    function Shutdown: HResult; stdcall;
  end;

// *********************************************************************//
// Die Klasse CoXPElevation stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IXPElevation, dargestellt von
// CoClass XPElevation, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoXPElevation = class
    class function Create: IXPElevation;
    class function CreateRemote(const MachineName: string): IXPElevation;
  end;

// *********************************************************************//
// Die Klasse CoXPElevationControl stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IXPElevationControl, dargestellt von
// CoClass XPElevationControl, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoXPElevationControl = class
    class function Create: IXPElevationControl;
    class function CreateRemote(const MachineName: string): IXPElevationControl;
  end;

implementation

uses ComObj;

class function CoXPElevation.Create: IXPElevation;
begin
  Result := CreateComObject(CLASS_XPElevation) as IXPElevation;
end;

class function CoXPElevation.CreateRemote(const MachineName: string): IXPElevation;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XPElevation) as IXPElevation;
end;

class function CoXPElevationControl.Create: IXPElevationControl;
begin
  Result := CreateComObject(CLASS_XPElevationControl) as IXPElevationControl;
end;

class function CoXPElevationControl.CreateRemote(const MachineName: string): IXPElevationControl;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XPElevationControl) as IXPElevationControl;
end;

end.
