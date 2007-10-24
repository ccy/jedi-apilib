unit VistaElevationDLL_TLB;

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
// Datei generiert am 24.10.2007 16:48:04 aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib: P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\jedi-api-lib\jwscl\trunk\examples\VistaElevation\VistaElevationDLL.tlb (1)
// LIBID: {C822DA82-0CA4-436C-B451-04A4AA57E7E3}
// LCID: 0
// Hilfedatei: 
// Hilfe-String: VistaElevationDLL Bibliothek
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
  VistaElevationDLLMajorVersion = 1;
  VistaElevationDLLMinorVersion = 0;

  LIBID_VistaElevationDLL: TGUID = '{C822DA82-0CA4-436C-B451-04A4AA57E7E3}';

  IID_IElevationDemoObject: TGUID = '{0CB0FB98-5AD7-4F1E-97E4-693CAF04AC9B}';
  CLASS_ElevationDemoObject: TGUID = '{E1859C9A-20EA-49E9-AEB0-DDA70CDFB7B7}';
type

// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen         
// *********************************************************************//
  IElevationDemoObject = interface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses             
// (HINWEIS: Hier wird jede CoClass zu ihrer Standardschnittstelle        
// zugewiesen)                                                            
// *********************************************************************//
  ElevationDemoObject = IElevationDemoObject;


// *********************************************************************//
// Schnittstelle: IElevationDemoObject
// Flags:     (256) OleAutomation
// GUID:      {0CB0FB98-5AD7-4F1E-97E4-693CAF04AC9B}
// *********************************************************************//
  IElevationDemoObject = interface(IUnknown)
    ['{0CB0FB98-5AD7-4F1E-97E4-693CAF04AC9B}']
    procedure DoSomething(const Str: WideString); stdcall;
    function DoTest: HResult; stdcall;
  end;

// *********************************************************************//
// Die Klasse CoElevationDemoObject stellt die Methoden Create und CreateRemote zur      
// Verfügung, um Instanzen der Standardschnittstelle IElevationDemoObject, dargestellt von
// CoClass ElevationDemoObject, zu erzeugen. Diese Funktionen können                     
// von einem Client verwendet werden, der die CoClasses automatisieren    
// möchte, die von dieser Typbibliothek dargestellt werden.               
// *********************************************************************//
  CoElevationDemoObject = class
    class function Create: IElevationDemoObject;
    class function CreateRemote(const MachineName: string): IElevationDemoObject;
  end;

implementation

uses ComObj;

class function CoElevationDemoObject.Create: IElevationDemoObject;
begin
  Result := CreateComObject(CLASS_ElevationDemoObject) as IElevationDemoObject;
end;

class function CoElevationDemoObject.CreateRemote(const MachineName: string): IElevationDemoObject;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ElevationDemoObject) as IElevationDemoObject;
end;

end.
