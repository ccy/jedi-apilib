{******************************************************************************}
{                                                                              }
{ Windows trust API interface Unit for Object Pascal                                    }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2008 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by XXXXXXXXXXXXXXXXX are Copyright (C) xxxx-xxxx            }
{ XXXXXXXXXXXXXXXXX. All Rights Reserved.                                      }
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
unit JwaWintrust;


{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinTrust.h"'}
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
{$ELSE}
  uses JwaWinType, JwaWinCrypt;
{$ENDIF JWA_WINDOWS}


{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_IMPLEMENTATIONSECTION}

type
  PWintrustFileInfo = ^TWintrustFileInfo;
  {$EXTERNALSYM PWINTRUST_FILE_INFO}
  PWINTRUST_FILE_INFO = ^WINTRUST_FILE_INFO_;
  {$EXTERNALSYM WINTRUST_FILE_INFO_}
  WINTRUST_FILE_INFO_ = packed record
    cbStruct: DWORD;
    pcwszFilePath: LPCWSTR;
    hFile: THandle;
    pgKnownSubject: PGUID;
  end;
  {$EXTERNALSYM WINTRUST_FILE_INFO}
  WINTRUST_FILE_INFO = WINTRUST_FILE_INFO_;
  TWintrustFileInfo = WINTRUST_FILE_INFO_;

  PWintrustCatalogInfo = ^TWintrustCatalogInfo;
  {$EXTERNALSYM PWINTRUST_CATALOG_INFO}
  PWINTRUST_CATALOG_INFO = ^WINTRUST_CATALOG_INFO_;
  {$EXTERNALSYM WINTRUST_CATALOG_INFO_}
  WINTRUST_CATALOG_INFO_ = packed record
    cbStruct: DWORD;
    dwCatalogVersion: DWORD;
    pcwszCatalogFilePath: LPCWSTR;
    pcwszMemberTag: LPCWSTR;
    pcwszMemberFilePath: LPCWSTR;
    hMemberFile: THandle;
    pbCalculatedFileHash: PByte;
    cbCalculatedFileHash: DWORD;
    pcCatalogContext: PCCTL_CONTEXT;
  end;
  {$EXTERNALSYM WINTRUST_CATALOG_INFO}
  WINTRUST_CATALOG_INFO = WINTRUST_CATALOG_INFO_;
  TWintrustCatalogInfo = WINTRUST_CATALOG_INFO_;

  PWintrustBlobInfo = ^TWintrustBlobInfo;
  {$EXTERNALSYM PWINTRUST_BLOB_INFO}
  PWINTRUST_BLOB_INFO = ^WINTRUST_BLOB_INFO_;
  {$EXTERNALSYM WINTRUST_BLOB_INFO_}
  WINTRUST_BLOB_INFO_ = packed record
    cbStruct: DWORD;
    gSubject: TGUID;
    pcwszDisplayName: LPCWSTR;
    cbMemObject: DWORD;
    pbMemObject: PByte;
    cbMemSignedMsg: DWORD;
    pbMemSignedMsg: PByte;
  end;
  {$EXTERNALSYM WINTRUST_BLOB_INFO}
  WINTRUST_BLOB_INFO = WINTRUST_BLOB_INFO_;
  TWintrustBlobInfo = WINTRUST_BLOB_INFO_;

  PWintrustSgnrInfo = ^TWintrustSgnrInfo;
  {$EXTERNALSYM PWINTRUST_SGNR_INFO}
  PWINTRUST_SGNR_INFO = ^WINTRUST_SGNR_INFO_;
  {$EXTERNALSYM WINTRUST_SGNR_INFO_}
  WINTRUST_SGNR_INFO_ = packed record
    cbStruct: DWORD;
    pcwszDisplayName: LPCWSTR;
    psSignerInfo: PCMSG_SIGNER_INFO;
    chStores: DWORD;
    pahStores: PHCERTSTORE;
  end;
  {$EXTERNALSYM WINTRUST_SGNR_INFO}
  WINTRUST_SGNR_INFO = WINTRUST_SGNR_INFO_;
  TWintrustSgnrInfo = WINTRUST_SGNR_INFO_;

  PWintrustCertInfo = ^TWintrustCertInfo;
  {$EXTERNALSYM PWINTRUST_CERT_INFO}
  PWINTRUST_CERT_INFO = ^WINTRUST_CERT_INFO_;
  {$EXTERNALSYM WINTRUST_CERT_INFO_}
  WINTRUST_CERT_INFO_ = packed record
    cbStruct: DWORD;
    pcwszDisplayName: LPCWSTR;
    psCertContext: PCERT_CONTEXT;
    chStores: DWORD;
    pahStores: PHCERTSTORE;
    dwFlags: DWORD;
    psftVerifyAsOf: PFILETIME;
  end;
  {$EXTERNALSYM WINTRUST_CERT_INFO}
  WINTRUST_CERT_INFO = WINTRUST_CERT_INFO_;
  TWintrustCertInfo = WINTRUST_CERT_INFO_;

const
  {$EXTERNALSYM WTCI_DONT_OPEN_STORES}
  WTCI_DONT_OPEN_STORES = 1;
  {$EXTERNALSYM WTCI_OPEN_ONLY_ROOT}
  WTCI_OPEN_ONLY_ROOT   = 2;
type
  PWintrustData = ^TWintrustData;
  {$EXTERNALSYM PWINTRUST_DATA}
  PWINTRUST_DATA = ^WINTRUST_DATA;
  {$EXTERNALSYM _WINTRUST_DATA}
  _WINTRUST_DATA = packed record
    cbStruct: DWORD;
    pPolicyCallbackData: Pointer;
    pSIPClientData: Pointer;
    dwUIChoice: DWORD;
    fdwRevocationChecks: DWORD;
    dwUnionChoice: DWORD;
    InfoUnion: record
      case Integer of
        0: (pFile: PWintrustFileInfo);
        1: (pCatalog: PWintrustCatalogInfo);
        2: (pBlob: PWintrustBlobInfo);
        3: (pSgnr: PWintrustSgnrInfo);
        4: (pCert: PWintrustCertInfo);
    end;
    dwStateAction: DWORD;
    hWVTStateData: THandle;
    pwszUrlReference: LPCWSTR;
    dwProvFlags: DWORD;
    dwUIContext: DWORD;
  end;
  {$EXTERNALSYM WINTRUST_DATA}
  WINTRUST_DATA = _WINTRUST_DATA;
  TWintrustData = _WINTRUST_DATA;

const
  {$EXTERNALSYM WTD_UI_ALL}
  WTD_UI_ALL    = 1;
  {$EXTERNALSYM WTD_UI_NONE}
  WTD_UI_NONE   = 2;
  {$EXTERNALSYM WTD_UI_NOBAD}
  WTD_UI_NOBAD  = 3;
  {$EXTERNALSYM WTD_UI_NOGOOD}
  WTD_UI_NOGOOD = 4;

  {$EXTERNALSYM WTD_REVOKE_NONE}
  WTD_REVOKE_NONE       = 0;
  {$EXTERNALSYM WTD_REVOKE_WHOLECHAIN}
  WTD_REVOKE_WHOLECHAIN = 1;

  {$EXTERNALSYM WTD_CHOICE_FILE}
  WTD_CHOICE_FILE    = 1;
  {$EXTERNALSYM WTD_CHOICE_CATALOG}
  WTD_CHOICE_CATALOG = 2;
  {$EXTERNALSYM WTD_CHOICE_BLOB}
  WTD_CHOICE_BLOB    = 3;
  {$EXTERNALSYM WTD_CHOICE_SIGNER}
  WTD_CHOICE_SIGNER  = 4;
  {$EXTERNALSYM WTD_CHOICE_CERT}
  WTD_CHOICE_CERT    = 5;

  {$EXTERNALSYM WTD_STATEACTION_IGNORE}
  WTD_STATEACTION_IGNORE           = 0;
  {$EXTERNALSYM WTD_STATEACTION_VERIFY}
  WTD_STATEACTION_VERIFY           = 1;
  {$EXTERNALSYM WTD_STATEACTION_CLOSE}
  WTD_STATEACTION_CLOSE            = 2;
  {$EXTERNALSYM WTD_STATEACTION_AUTO_CACHE}
  WTD_STATEACTION_AUTO_CACHE       = 3;
  {$EXTERNALSYM WTD_STATEACTION_AUTO_CACHE_FLUSH}
  WTD_STATEACTION_AUTO_CACHE_FLUSH = 4;

  {$EXTERNALSYM WTD_PROV_FLAGS_MASK}
  WTD_PROV_FLAGS_MASK = $FFFF;
  {$EXTERNALSYM WTD_USE_IE4_TRUST_FLAG}
  WTD_USE_IE4_TRUST_FLAG   = 1;
  {$EXTERNALSYM WTD_NO_IE4_CHAIN_FLAG}
  WTD_NO_IE4_CHAIN_FLAG    = 2;
  {$EXTERNALSYM WTD_NO_POLICY_USAGE_FLAG}
  WTD_NO_POLICY_USAGE_FLAG = 4;
  {$EXTERNALSYM WTD_REVOCATION_CHECK_NONE}
  WTD_REVOCATION_CHECK_NONE               = $10;
  {$EXTERNALSYM WTD_REVOCATION_CHECK_END_CERT}
  WTD_REVOCATION_CHECK_END_CERT           = $20;
  {$EXTERNALSYM WTD_REVOCATION_CHECK_CHAIN}
  WTD_REVOCATION_CHECK_CHAIN              = $40;
  {$EXTERNALSYM WTD_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT}
  WTD_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT = $80;
  {$EXTERNALSYM WTD_SAFER_FLAG}
  WTD_SAFER_FLAG              = $100;
  {$EXTERNALSYM WTD_HASH_ONLY_FLAG}
  WTD_HASH_ONLY_FLAG          = $200;
  {$EXTERNALSYM WTD_USE_DEFAULT_OSVER_CHECK}
  WTD_USE_DEFAULT_OSVER_CHECK = $400;
  {$EXTERNALSYM WTD_LIFETIME_SIGNING_CHECK}
  WTD_LIFETIME_SIGNING_CHECK  = $800;
  {$EXTERNALSYM WTD_CACHE_ONLY_URL_RETRIEVAL}
  WTD_CACHE_ONLY_URL_RETRIEVAL = $1000;

  {$EXTERNALSYM WTD_UI_CONTEXT_EXECUTE}
  WTD_UI_CONTEXT_EXECUTE = 0;
  {$EXTERNALSYM WTD_UI_CONTEXT_INSTALL}
  WTD_UI_CONTEXT_INSTALL = 1;

{$EXTERNALSYM WinVerifyTrust}
function WinVerifyTrust(hwnd: HWND; const pgActionID: TGUID; const pWinTrustData: _WINTRUST_DATA): HRESULT;



{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDllNames;
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  wintrust = 'wintrust.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _WinVerifyTrust: Pointer;

function WinVerifyTrust;
begin
  GetProcedureAddress(_WinVerifyTrust, wintrust, 'WinVerifyTrust');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WinVerifyTrust]
  end;
end;


{$ELSE}

function WinVerifyTrust; external wintrust name 'WinVerifyTrust';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
