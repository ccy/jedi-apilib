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
// File generated on 14.02.2008 21:22:55 from Type Library described below.

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

  IID_IJwSid: TGUID = '{5134BF4E-3D59-44FF-A273-C052BB9B64DE}';
  IID_IJwSidList: TGUID = '{C88787BF-0091-46F2-A732-0639244C54E5}';
  CLASS_JwSid: TGUID = '{95B55CCE-E93B-4DE1-AEBA-18E006FD06F6}';
  CLASS_JwSidList: TGUID = '{6F10E768-10B0-44E6-A14E-30C905B5B01E}';
  IID_IJwTest: TGUID = '{392DA373-0FE2-45D5-AF98-89BCC8D2D0CD}';
  CLASS_JwTest2: TGUID = '{6859DD54-E45C-4857-B63C-443D0B5D57E4}';

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
  IJwSid = interface;
  IJwSidDisp = dispinterface;
  IJwSidList = interface;
  IJwSidListDisp = dispinterface;
  IJwTest = interface;
  IJwTestDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  JwSid = IJwSid;
  JwSidList = IJwSidList;
  JwTest2 = IJwTest;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//

  PCoSid = PChar; 
  PCoSidAndAttributes = PChar; 
  PCoTokenGroups = PChar; 

// *********************************************************************//
// Interface: IJwSid
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5134BF4E-3D59-44FF-A273-C052BB9B64DE}
// *********************************************************************//
  IJwSid = interface(IDispatch)
    ['{5134BF4E-3D59-44FF-A273-C052BB9B64DE}']
    procedure InitByBinarySid(const BinarySid: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  IJwSidDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {5134BF4E-3D59-44FF-A273-C052BB9B64DE}
// *********************************************************************//
  IJwSidDisp = dispinterface
    ['{5134BF4E-3D59-44FF-A273-C052BB9B64DE}']
    procedure InitByBinarySid(const BinarySid: WideString); dispid 201;
  end;

// *********************************************************************//
// Interface: IJwSidList
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C88787BF-0091-46F2-A732-0639244C54E5}
// *********************************************************************//
  IJwSidList = interface(IDispatch)
    ['{C88787BF-0091-46F2-A732-0639244C54E5}']
    procedure Method1; safecall;
  end;

// *********************************************************************//
// DispIntf:  IJwSidListDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C88787BF-0091-46F2-A732-0639244C54E5}
// *********************************************************************//
  IJwSidListDisp = dispinterface
    ['{C88787BF-0091-46F2-A732-0639244C54E5}']
    procedure Method1; dispid 201;
  end;

// *********************************************************************//
// Interface: IJwTest
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
// The Class CoJwSid provides a Create and CreateRemote method to          
// create instances of the default interface IJwSid exposed by              
// the CoClass JwSid. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoJwSid = class
    class function Create: IJwSid;
    class function CreateRemote(const MachineName: string): IJwSid;
  end;

// *********************************************************************//
// The Class CoJwSidList provides a Create and CreateRemote method to          
// create instances of the default interface IJwSidList exposed by              
// the CoClass JwSidList. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoJwSidList = class
    class function Create: IJwSidList;
    class function CreateRemote(const MachineName: string): IJwSidList;
  end;

// *********************************************************************//
// The Class CoJwTest2 provides a Create and CreateRemote method to          
// create instances of the default interface IJwTest exposed by              
// the CoClass JwTest2. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
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
