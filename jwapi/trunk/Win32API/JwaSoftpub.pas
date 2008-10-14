{******************************************************************************}
{                                                                              }
{ Internet Security Authenticode Policy Provider API interface Unit            }
{ for Object Pascal                                                            }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2008 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by Philip Dittmann are Copyright (C) xxxx-xxxx            }
{ Philip Dittmann. All Rights Reserved.                                      }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI         }
{ APILIB home page, located at http://jedi-apilib.sourceforge.net              }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaSoftpub;


{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "Softpub.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I jediapilib.inc}

{$IFDEF DELPHI6_UP}
{$ALIGN 8}
{$ELSE}
{$ALIGN+} //WARNING: Incorrect alignment in this Delphi
{$ENDIF DELPHI6_UP}
interface

{$IFDEF JWA_WINDOWS}
  uses JwaWindows;
{$ENDIF JWA_WINDOWS}


{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_IMPLEMENTATIONSECTION}

//constants for WinVerifyTrust
const
  {$EXTERNALSYM DRIVER_ACTION_VERIFY}
  DRIVER_ACTION_VERIFY:                 TGUID = '{c3e650f7-38ee-11d1-85e5-00c04fc295ee}';
  {$EXTERNALSYM HTTPSPROV_ACTION}
  HTTPSPROV_ACTION:                     TGUID = '{573e31f8-aaba-11d0-8ccb-00c04fc295ee}';
  {$EXTERNALSYM OFFICESIGN_ACTION_VERIFY}
  OFFICESIGN_ACTION_VERIFY:             TGUID = '{5555c2cd-17fb-11d1-85c4-00c04fc295ee}';
  {$EXTERNALSYM WINTRUST_ACTION_GENERIC_CERT_VERIFY}
  WINTRUST_ACTION_GENERIC_CERT_VERIFY:  TGUID = '{189a3842-3041-11d1-85e1-00c04fc295ee}';
  {$EXTERNALSYM WINTRUST_ACTION_GENERIC_CHAIN_VERIFY}
	WINTRUST_ACTION_GENERIC_CHAIN_VERIFY: TGUID = '{fc451c16-ac75-11d1-b4b8-00c04fb66ea0}';
//  WINTRUST_ACTION_GENERIC_VERIFY //is no longer defined in the PSDK headers
  {$EXTERNALSYM WINTRUST_ACTION_GENERIC_VERIFY_V2}
  WINTRUST_ACTION_GENERIC_VERIFY_V2:    TGUID = '{00aac56b-cd44-11d0-8cc2-00c04fc295ee}';
  {$EXTERNALSYM WINTRUST_ACTION_TRUSTPROVIDER_TEST}
  WINTRUST_ACTION_TRUSTPROVIDER_TEST:   TGUID = '{573e31f8-ddba-11d0-8ccb-00c04fc295ee}';


{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDllNames;
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}


{$IFNDEF JWA_INCLUDEMODE}
const
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

{$ELSE}


{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
