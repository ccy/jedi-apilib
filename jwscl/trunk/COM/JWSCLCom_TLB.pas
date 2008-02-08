unit JWSCLCom_TLB;

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
// File generated on 08.02.2008 23:57:20 from Type Library described below.

// ************************************************************************  //
// Type Lib: P:\Eigene Dateien\Dezipaitor\Projekte\Delphi\7\jedi-api-lib\jwscl\trunk\COM\JWSCLCom.tlb (1)
// LIBID: {9EBCE2EF-4E69-4AC3-AA7F-F021E119E8BB}
// LCID: 0
// Helpfile: 
// HelpString: JWSCLCom Library
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
  JWSCLComMajorVersion = 1;
  JWSCLComMinorVersion = 0;

  LIBID_JWSCLCom: TGUID = '{9EBCE2EF-4E69-4AC3-AA7F-F021E119E8BB}';

  IID_IJwCoSid: TGUID = '{C63F1AD8-9436-45C8-A106-0861492D3D1D}';
  CLASS_JwCoSid: TGUID = '{B20B11D4-8157-47C3-A105-E2FF52E4034D}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum JwCoFacilities
type
  JwCoFacilities = TOleEnum;
const
  FACILITY_JWSCL = $00000065;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IJwCoSid = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  JwCoSid = IJwCoSid;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//

  PCoSid = PChar; 

// *********************************************************************//
// Interface: IJwCoSid
// Flags:     (256) OleAutomation
// GUID:      {C63F1AD8-9436-45C8-A106-0861492D3D1D}
// *********************************************************************//
  IJwCoSid = interface(IUnknown)
    ['{C63F1AD8-9436-45C8-A106-0861492D3D1D}']
    function InitBySid(SidPtr: PCoSid): HResult; stdcall;
    function GetSidPtr(out SidDataPtr: PCoSid; out SidDataSize: Integer): HResult; stdcall;
    function InitByStream(const SidAsStream: IUnknown): HResult; stdcall;
    function GetSidStream(out SidAsStream: IUnknown): HResult; stdcall;
    function InitByBinarySid(const BinarySid: WideString): HResult; stdcall;
    function InitByName(const SystemName: WideString; const AccountName: WideString): HResult; stdcall;
  end;

// *********************************************************************//
// The Class CoJwCoSid provides a Create and CreateRemote method to          
// create instances of the default interface IJwCoSid exposed by              
// the CoClass JwCoSid. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
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
