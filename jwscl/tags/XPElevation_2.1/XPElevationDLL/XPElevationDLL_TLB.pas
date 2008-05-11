unit XPElevationDLL_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 8291 $
// File generated on 11.05.2008 19:20:08 from Type Library described below.

// ************************************************************************  //
// Type Lib: P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\jedi-api-lib\jwscl\branches\XPElevation_2\XPElevationDLL\XPElevationDLL.tlb (1)
// LIBID: {0F35684C-C35E-42EE-AFA3-315BFEFF48B1}
// LCID: 0
// Helpfile: 
// HelpString: XPElevationDLL Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  XPElevationDLLMajorVersion = 1;
  XPElevationDLLMinorVersion = 0;

  LIBID_XPElevationDLL: TGUID = '{0F35684C-C35E-42EE-AFA3-315BFEFF48B1}';

  IID_IXPElevation: TGUID = '{945A8D22-E912-4F09-8CB8-32667746DA6E}';
  CLASS_XPElevation: TGUID = '{9EC823A7-F59F-4DA7-B895-7ACEF33A28AB}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IXPElevation = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  XPElevation = IXPElevation;


// *********************************************************************//
// Interface: IXPElevation
// Flags:     (256) OleAutomation
// GUID:      {945A8D22-E912-4F09-8CB8-32667746DA6E}
// *********************************************************************//
  IXPElevation = interface(IUnknown)
    ['{945A8D22-E912-4F09-8CB8-32667746DA6E}']
    function CreateProcess(const Application: WideString; const Parameters: WideString; 
                           StartInfo: Integer; out PID: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// The Class CoXPElevation provides a Create and CreateRemote method to          
// create instances of the default interface IXPElevation exposed by              
// the CoClass XPElevation. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoXPElevation = class
    class function Create: IXPElevation;
    class function CreateRemote(const MachineName: string): IXPElevation;
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

end.
