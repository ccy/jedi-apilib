{******************************************************************************}
{                                                                              }
{ Windows trust API interface Unit for Object Pascal                           }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2008 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by Christian Wimmer  are Copyright (C) 2009-2010            }
{ Christian Wimmer. All Rights Reserved.                                       }
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
{$I ..\Includes\JediAPILib.inc}


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
const
  WINTRUST_CONFIG_REGPATH = 'Software\Microsoft\Cryptography\Wintrust\Config';

{ The following are REG_DWORD's. These configuration parameters are used
  to limit the number of file bytes mapped at a time. Should be a multiple of
  dwAllocationGranularity returned by GetSystemInfo(). This allows
  very large files, > 2Gig bytes, to be authenticated signed and verified
  using a much smaller virtual memory address range.
}

{ The length of the header bytes for a PE, CAB or any file that is
  authenticode signed must be less than the following value.
}
  WINTRUST_MAX_HEADER_BYTES_TO_MAP_VALUE_NAME     = 'MaxHeaderBytesToMap';

{ 0x00A0'0000 (10,485,760) Bytes
}
  WINTRUST_MAX_HEADER_BYTES_TO_MAP_DEFAULT        = $00A00000;
{ If the file size doesn't fit within the above header length, the following
  value is used to set the maximum number of remaining file bytes that are
  mapped/hashed/unmapped at time.
}
  WINTRUST_MAX_HASH_BYTES_TO_MAP_VALUE_NAME       = 'MaxHashBytesToMap';
{ 0x0010'0000 (1,048,576) Bytes
}
  WINTRUST_MAX_HASH_BYTES_TO_MAP_DEFAULT          = $00100000;


type


  PWintrustFileInfo = ^TWintrustFileInfo;
  PWintrustCatalogInfo = ^TWintrustCatalogInfo;
  PWintrustSgnrInfo = ^TWintrustSgnrInfo;
  PWintrustBlobInfo = ^TWintrustBlobInfo;
  PWintrustCertInfo = ^TWintrustCertInfo;

{ Used when calling WinVerifyTrust to pass necessary information into
  the Providers.
}
  PWintrustData = ^TWintrustData;
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

{ Used when calling WinVerifyTrust against an individual file.
}
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

{ Used when calling WinVerifyTrust against a member of a Microsoft Catalog
  file.}
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

{  Used when calling WinVerifyTrust against a memory blob.
}
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

{ Used when calling WinVerifyTrust against a CMSG_SIGNER_INFO Structure
}
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


{ Call this function to verify the trust based on a digital signer.

  pWVTData points to a WINTRUST_DATA data structure.

  WTD_SAFER_FLAG should be set in WINTRUST_DATA's dwProvFlags to enable
  the following semantics for the WINTRUST_ACTION_GENERIC_VERIFY_V2
  policy provider specified in pgActionID:
   - return TRUST_E_NOSIGNATURE if the subject isn't signed, has an
     invalid signature or unable to find the signer certificate.
     UI will never be displayed when not signed.
   - ignore NO_CHECK revocation errors. Otherwise, continue to return
     CERT_E_REVOCATION_FAILURE.
   - search the code hash and publisher databases for the WTD_UI_NONE
     dwUIChoice case. The default is to only search these databases when
     UI has been enabled or user trust has been disabled.


  Returns:
          ERROR_SUCCESS               If the trust is authenticated or
                                      if the user accepted the risk.

          TRUST_E_PROVIDER_UNKNOWN    there was an error loading one of the
                                      required Providers.

          all error codes passed back are based on the Policy Provider used.

  The following errors are returned when the
  WINTRUST_ACTION_GENERIC_VERIFY_V2 policy provider is specified in
  pgActionID:

    TRUST_E_NOSIGNATURE (when WTD_SAFER_FLAG is set in dwProvFlags)
      The subject isn't signed, has an invalid signature or unable
      to find the signer certificate. All signature verification
      errors will map to this error. Basically all errors except for
      publisher or timestamp certificate verification.

      Call GetLastError() to get the underlying reason for not having
      a valid signature.

      The following LastErrors indicate that the file doesn't have a
      signature: TRUST_E_NOSIGNATURE, TRUST_E_SUBJECT_FORM_UNKNOWN or
      TRUST_E_PROVIDER_UNKNOWN.

      UI will never be displayed for this case.

    TRUST_E_EXPLICIT_DISTRUST
      Returned if the hash representing the subject is trusted as
      AUTHZLEVELID_DISALLOWED or the publisher is in the "Disallowed"
      store. Also returned if the publisher certificate is revoked.

      UI will never be displayed for this case.

    ERROR_SUCCESS
      No UI unless noted below.

      Returned for the following:
       - Hash representing the subject is trusted as
         AUTHZLEVELID_FULLYTRUSTED
       - The publisher certificate exists in the
         "TrustedPublisher" store and there weren't any verification errors.
       - UI was enabled and the user clicked "Yes" when asked
         to install and run the signed subject.
       - UI was disabled. No publisher or timestamp chain error.

    TRUST_E_SUBJECT_NOT_TRUSTED
      UI was enabled and the the user clicked "No" when asked to install
      and run the signed subject.

    CRYPT_E_SECURITY_SETTINGS
      The subject hash or publisher wasn't explicitly trusted and
      user trust wasn't allowed in the safer authenticode flags.
      No UI will be displayed for this case.

      The subject is signed and its signature successfully
      verified.

    Any publisher or timestamp chain error. If WTD_SAFER_FLAG wasn't set in
    dwProvFlags, any signed code verification error.
}
{$EXTERNALSYM WinVerifyTrust}
function WinVerifyTrust(hwnd: HWND; const pgActionID: TGUID; var pWinTrustData: _WINTRUST_DATA): LONG; stdcall;

{  WinVerifyTrustEx was not defined because it may be unavailable in subsequent Windows versions.
}

const
//////////////////////////////////////////////////////////////////////////////
//
// Wintrust Policy Flags
//----------------------------------------------------------------------------
//  These are set during install and can be modified by the user
//  through various means.  The SETREG.EXE utility (found in the Authenticode
//  Tools Pack) will select/deselect each of them.
//
  WTPF_TRUSTTEST              = $00000020;  // trust any "TEST" certificate
  WTPF_TESTCANBEVALID         = $00000080;
  WTPF_IGNOREEXPIRATION       = $00000100;  // Use expiration date
  WTPF_IGNOREREVOKATION       = $00000200;  // Do revocation check
  WTPF_OFFLINEOK_IND          = $00000400;  // off-line is ok individual certs
  WTPF_OFFLINEOK_COM          = $00000800;  // off-line is ok commercial certs
  WTPF_OFFLINEOKNBU_IND       = $00001000;  // off-line is ok individual certs, no bad ui
  WTPF_OFFLINEOKNBU_COM       = $00002000;  // off-line is ok commercial certs, no bad ui
  WTPF_VERIFY_V1_OFF          = $00010000;  // turn verify of v1 certs off
  WTPF_IGNOREREVOCATIONONTS   = $00020000;  // ignore TimeStamp revocation checks
  WTPF_ALLOWONLYPERTRUST      = $00040000;  // allow only items in personal trust db.

type

{ This API call is exported from WINTRUST.DLL and is the recommended method
  of retrieving the DWORD representing the Policy Flags.
}
  WintrustGetRegPolicyFlags = procedure (pdwPolicyFlags : PDWORD); stdcall;

{ This API call is exported from WINTRUST.DLL and is the recommended method
  of setting the DWORD representing the Policy Flags.  MAKE SURE to call
  WintrustGetRegPolicyFlags to get the current value and or/and the value
  you need then call the set the flags.
}
  WintrustSetRegPolicyFlags = function (dwPolicyFlags : DWORD) : BOOL; stdcall;

const
{ Trust Provider "Step" Error defines
  Each "step" of the Trust process has an error "slot" associated with it.
  If an error occurs, the "step" will assign its result to this "slot".  These
  errors can be any valid WINERROR.H HRESULT code.

  step errors 0 through 20 are reserved for Authenticode specific.  If
  you are not calling any of the SOFTPUB.DLL (Authenticode) providers, you
  may use these as needed.
}
  TRUSTERROR_STEP_WVTPARAMS                   = 0;
  TRUSTERROR_STEP_FILEIO                      = 2;
  TRUSTERROR_STEP_SIP                         = 3;
  TRUSTERROR_STEP_SIPSUBJINFO                 = 5;
  TRUSTERROR_STEP_CATALOGFILE                 = 6;
  TRUSTERROR_STEP_CERTSTORE                   = 7;
  TRUSTERROR_STEP_MESSAGE                     = 8;
  TRUSTERROR_STEP_MSG_SIGNERCOUNT             = 9;
  TRUSTERROR_STEP_MSG_INNERCNTTYPE            = 10;
  TRUSTERROR_STEP_MSG_INNERCNT                = 11;
  TRUSTERROR_STEP_MSG_STORE                   = 12;
  TRUSTERROR_STEP_MSG_SIGNERINFO              = 13;
  TRUSTERROR_STEP_MSG_SIGNERCERT              = 14;
  TRUSTERROR_STEP_MSG_CERTCHAIN               = 15;
  TRUSTERROR_STEP_MSG_COUNTERSIGINFO          = 16;
  TRUSTERROR_STEP_MSG_COUNTERSIGCERT          = 17;
  TRUSTERROR_STEP_VERIFY_MSGHASH              = 18;
  TRUSTERROR_STEP_VERIFY_MSGINDIRECTDATA      = 19;

{ step errors 30 through 37 are reserved for the ending error code for each
  entry point in the Trust Model.
}
  TRUSTERROR_STEP_FINAL_WVTINIT               = 30;
  TRUSTERROR_STEP_FINAL_INITPROV              = 31;
  TRUSTERROR_STEP_FINAL_OBJPROV               = 32;
  TRUSTERROR_STEP_FINAL_SIGPROV               = 33;
  TRUSTERROR_STEP_FINAL_CERTPROV              = 34;
  TRUSTERROR_STEP_FINAL_CERTCHKPROV           = 35;
  TRUSTERROR_STEP_FINAL_POLICYPROV            = 36;
  TRUSTERROR_STEP_FINAL_UIPROV                = 37;

  TRUSTERROR_MAX_STEPS                        = 38;


type
  PCRYPT_PROVIDER_PRIVDATA = ^CRYPT_PROVIDER_PRIVDATA;
  PCryptProviderData = ^TCryptProviderData;
  PCRYPT_PROVIDER_DATA = ^CRYPT_PROVIDER_DATA;
  PPROVDATA_SIP = ^PROVDATA_SIP;

  PCRYPT_PROVIDER_CERT = ^CRYPT_PROVIDER_CERT;
  PCRYPT_PROVIDER_SGNR = ^CRYPT_PROVIDER_SGNR;

//  allocation and free function prototypes
//----------------------------------------------------------------------------
//
  PFN_CPD_MEM_ALLOC = function ({IN} cbSize : DWORD ) : Pointer; cdecl;
  PFN_CPD_MEM_FREE = procedure ({IN} pvMem2Free : Pointer); cdecl;

  PFN_CPD_ADD_STORE = function ({IN} pProvData : PCRYPT_PROVIDER_DATA;
                                {IN} hStore2Add : HCERTSTORE) :  Bool; cdecl;

  PFN_CPD_ADD_SGNR = function ({IN} pProvData : PCRYPT_PROVIDER_DATA;
                               {IN} fCounterSigner : BOOL;
                               {IN OPTIONAL} idxSigner : DWORD;
                               {IN} pSgnr2Add : PCRYPT_PROVIDER_SGNR) : Bool; cdecl;

  PFN_CPD_ADD_CERT = function ({IN} pProvData : PCRYPT_PROVIDER_DATA;
                               {IN} idxSigner : DWORD;
                               {IN} fCounterSigner : BOOL;
                               {IN OPTIONAL} idxCounterSigner : DWORD;
                               {IN} pCert2Add : PCCERT_CONTEXT) : Bool; cdecl;


  PFN_CPD_ADD_PRIVDATA = function ({IN} pProvData : PCRYPT_PROVIDER_DATA;
                                   {IN} pPrivData2Add : PCRYPT_PROVIDER_PRIVDATA) : Bool; cdecl;




  _CRYPT_PROVIDER_PRIVDATA = packed record
    cbStruct: DWORD;
    gProviderID: GUID;
    cbProvData: DWORD;
    pvProvData: Pointer;
  end;
  CRYPT_PROVIDER_PRIVDATA = _CRYPT_PROVIDER_PRIVDATA;


  _PROVDATA_SIP = packed record
    cbStruct: DWORD;
    gSubject: GUID;

    //The following members are actually typed pointers. The
    //corresponding structures are defined in mssip.h.
    pSip: Pointer;
    pCATSip: Pointer;
    psSipSubjectInfo: Pointer;
    psSipCATSubjectInfo: Pointer;
    psIndirectData: Pointer;
  end;
  PROVDATA_SIP = _PROVDATA_SIP;

  _CRYPT_PROVIDER_CERT = packed record
    cbStruct: DWORD;
    pCert: PCCERT_CONTEXT;
    fCommercial: BOOL;
    fTrustedRoot: BOOL;
    fSelfSigned: BOOL;
    fTestCert: BOOL;
    dwRevokedReason: DWORD;
    dwConfidence: DWORD;
    dwError: DWORD;
    pTrustListContext: PCTL_CONTEXT;
    fTrustListSignerCert: BOOL;
    pCtlContext: PCCTL_CONTEXT;
    dwCtlError: DWORD;
    fIsCyclic: BOOL;
    pChainElement: PCERT_CHAIN_ELEMENT;
  end;
  CRYPT_PROVIDER_CERT = _CRYPT_PROVIDER_CERT;

  _CRYPT_PROVIDER_SGNR = packed record
    cbStruct: DWORD;
    sftVerifyAsOf: FILETIME;
    csCertChain: PCRYPT_PROVIDER_CERT;
    dwSignerType: DWORD;
    psSigner: PCMSG_SIGNER_INFO;
    dwError: DWORD;
    csCounterSigners: DWORD;
    pasCounterSigners: PCRYPT_PROVIDER_SGNR;
    pChainContext: PCCERT_CHAIN_CONTEXT;
  end;
  CRYPT_PROVIDER_SGNR = _CRYPT_PROVIDER_SGNR;

  _CRYPT_PROVIDER_DATA = packed record
    cbStruct: DWORD;
    pWintrustData: PWINTRUST_DATA;
    fOpenedFile: BOOL;
    hWndParent: HWND;
    pgActionId: PGUID;
    hProv: HCRYPTPROV;
    dwError: DWORD;
    dwRegSecuritySettings: DWORD;
    dwRegPolicySettings: DWORD;
    psPfns: Pointer; //actually a pointer to a _CRYPT_PROVIDER_FUNCTIONS
    cdwTrustStepErrors: DWORD;
    padwTrustStepErrors: PDWORD;
    chStores: DWORD;
    pahStores: PHCERTSTORE;
    dwEncoding: DWORD;
    hMsg: HCRYPTMSG;
    csSigners: DWORD;
    pasSigners: PCRYPT_PROVIDER_SGNR;
    csProvPrivData: DWORD;
    pasProvPrivData: PCRYPT_PROVIDER_PRIVDATA;
    dwSubjectChoice: DWORD;
    pPDSIP: PPROVDATA_SIP; //in C a union with one member
    pszUsageOID: PAnsiChar;
    fRecallWithState: BOOL;
    sftSystemTime: FILETIME;
    pszCTLSignerUsageOID: PAnsiChar;
    dwProvFlags: DWORD;
    dwFinalError: DWORD;
    pRequestUsage: PCERT_USAGE_MATCH;
    dwTrustPubSettings: DWORD;
    dwUIStateFlags: DWORD;
  end;
  CRYPT_PROVIDER_DATA = _CRYPT_PROVIDER_DATA;
  TCryptProviderData = _CRYPT_PROVIDER_DATA;


//////////////////////////////////////////////////////////////////////////////
//
//  Provider function prototypes
//----------------------------------------------------------------------------
//
//
//  entry point for the object provider
//
  PFN_PROVIDER_INIT_CALL = function({IN OUT} var pProvData : _CRYPT_PROVIDER_DATA) : HRESULT; cdecl;

//
//  entry point for the object provider
//
  PFN_PROVIDER_OBJTRUST_CALL = function({IN OUT} var pProvData : _CRYPT_PROVIDER_DATA) : HRESULT; cdecl;

//
//  entry point for the Signature Provider
//
  PFN_PROVIDER_SIGTRUST_CALL = function({IN OUT} var pProvData : _CRYPT_PROVIDER_DATA) : HRESULT; cdecl;

//
//  entry point for the Certificate Provider
//
  PFN_PROVIDER_CERTTRUST_CALL = function({IN OUT} var pProvData : _CRYPT_PROVIDER_DATA) : HRESULT; cdecl;

//
//  entry point for the Policy Provider's final call (from the trust provider)
//
  PFN_PROVIDER_FINALPOLICY_CALL = function({IN OUT} var pProvData : _CRYPT_PROVIDER_DATA) : HRESULT; cdecl;

//
//  entry point for the Policy Provider's "dump structure" call
//
  PFN_PROVIDER_TESTFINALPOLICY_CALL = function({IN OUT} var pProvData : _CRYPT_PROVIDER_DATA) : HRESULT; cdecl;

//
//  entry point for the Policy Provider's clean up routine for any PRIVDATA allocated
//
  PFN_PROVIDER_CLEANUP_CALL = function({IN OUT}var pProvData : _CRYPT_PROVIDER_DATA) : HRESULT; cdecl;

//
//  entry point for the Policy Provider's Cert Check call.  This will return
//  true if the Trust Provider is to continue building the certificate chain.
//  If the PP returns FALSE, it is assumed that we have reached a "TRUSTED",
//  self-signed, root.  it is also the CertCheck's responsibility to set the
//  fTrustedRoot flag in the certificate structure.
//
  PFN_PROVIDER_CERTCHKPOLICY_CALL = function({IN} pProvData : PCRYPT_PROVIDER_DATA;
                                             {IN} idxSigner : DWORD) : Bool; cdecl;


//#define WVT_OFFSETOF(t,f)   ((ULONG)((ULONG_PTR)(&((t*)0)->f)))

// WOB #1251526 -- macro must check whether _all_ bytes of the member
// lie within the struct size specified
//#define WVT_ISINSTRUCT(structtypedef, structpassedsize, member) \
//  ((WVT_OFFSETOF(structtypedef, member) + sizeof(((structtypedef *) 0)->member) <= structpassedsize) ? TRUE : FALSE)


//#define WVT_IS_CBSTRUCT_GT_MEMBEROFFSET(structtypedef, structpassedsize, member) \
//                    WVT_ISINSTRUCT(structtypedef, structpassedsize, member)

const
  WT_CURRENT_VERSION                  = $00000200;

type
  _CRYPT_TRUST_REG_ENTRY = record
      cbStruct : DWORD;
      pwszDLLName,
      pwszFunctionName : PWideChar;  // no more than WT_MAX_FUNC_NAME!
  end;
  CRYPT_TRUST_REG_ENTRY = _CRYPT_TRUST_REG_ENTRY;
  PCRYPT_TRUST_REG_ENTRY = ^_CRYPT_TRUST_REG_ENTRY;

  _CRYPT_REGISTER_ACTIONID = record
    cbStruct : DWORD;

    sInitProvider,
    sObjectProvider,
    sSignatureProvider,
    sCertificateProvider,
    sCertificatePolicyProvider,
    sFinalPolicyProvider,
    sTestPolicyProvider,
    sCleanupProvider : CRYPT_TRUST_REG_ENTRY;
  end;
  CRYPT_REGISTER_ACTIONID = _CRYPT_REGISTER_ACTIONID;
  PCRYPT_REGISTER_ACTIONID = ^_CRYPT_REGISTER_ACTIONID;


  _CRYPT_PROVIDER_REGDEFUSAGE = record
    cbStruct : DWORD;   // = sizeof CRYPT_PROVIDER_REGDEFUSAGE

    pgActionID : PGUID;

    pwszDllName : PWideChar;
    pwszLoadCallbackDataFunctionName,
    pwszFreeCallbackDataFunctionName : PAnsiChar;
  end;
  CRYPT_PROVIDER_REGDEFUSAGE = _CRYPT_PROVIDER_REGDEFUSAGE;
  PCRYPT_PROVIDER_REGDEFUSAGE = ^_CRYPT_PROVIDER_REGDEFUSAGE;

  TCryptProviderRegDefUsage = CRYPT_PROVIDER_REGDEFUSAGE;
  PCryptProviderRegDefUsage = PCRYPT_PROVIDER_REGDEFUSAGE;

  _CRYPT_PROVIDER_DEFUSAGE = record
    cbStruct : DWORD;               // = sizeof CRYPT_PROVIDER_DEFUSAGE

    gActionID : GUID;            // ActionID of provider

    pDefPolicyCallbackData, // normally filled in WINTRUST_DATA
    pDefSIPClientData  : Pointer;      // normally filled in WINTRUST_DATA
  end;
  CRYPT_PROVIDER_DEFUSAGE = _CRYPT_PROVIDER_DEFUSAGE;
  PCRYPT_PROVIDER_DEFUSAGE = ^CRYPT_PROVIDER_DEFUSAGE;

  TCryptProviderDefUsage = CRYPT_PROVIDER_DEFUSAGE;
  PCryptProviderDefUsage = PCRYPT_PROVIDER_DEFUSAGE;




  PFN_ALLOCANDFILLDEFUSAGE = function({IN} const pszUsageOID : PAnsiChar;
                                      {IN} psDefUsage : PCRYPT_PROVIDER_DEFUSAGE) : Bool; cdecl;
  PFN_FREEDEFUSAGE = function({IN} const pszUsageOID : PAnsiChar;
                              {IN} psDefUsage : PCRYPT_PROVIDER_DEFUSAGE) : Bool; cdecl;

const
//////////////////////////////////////////////////////////////////////////////
//
// WINTRUST.DLL Provider defines
//----------------------------------------------------------------------------
//  The following are definitions of the Microsoft Generic Cert Provider
//
  WT_PROVIDER_DLL_NAME                = 'WINTRUST.DLL';
  WT_PROVIDER_CERTTRUST_FUNCTION      = 'WintrustCertificateTrust';

//////////////////////////////////////////////////////////////////////////////
//
// WintrustAddActionID
//----------------------------------------------------------------------------
//  Adds a new Provider combination to the users'
//  system.  Creates all necessary registry entries, etc.  This should be done
//  during the Policy Provider's DllRegisterServer.
//
//  *** THE ONLY ONE WHO SHOULD CALL THIS IS THE POLICY PROVIDER ***
//
// Returns:
//      TRUE:                           No fatal errors
//      FALSE:                          Errors occured.  See GetLastError()
//
(*extern BOOL WINAPI  WintrustAddActionID(IN GUID *pgActionID,
                                        IN DWORD fdwFlags,
                                        IN CRYPT_REGISTER_ACTIONID *psProvInfo);
  *)
// By default, WintrustAddActionID doesn't return registry errors.
// Set this flag to return registry errors. If FALSE is returned,
// LastError is set.
const
  WT_ADD_ACTION_ID_RET_RESULT_FLAG    = $1;


//////////////////////////////////////////////////////////////////////////////
//
// WintrustRemoveActionID
//----------------------------------------------------------------------------
//  Removes the Provider action combination from the users'
//  system.
//
// Returns:
//      TRUE:                           No fatal errors
//      FALSE:                          Errors occured.  See GetLastError()
//
(*extern BOOL WINAPI  WintrustRemoveActionID(IN GUID *pgActionID);
  *)
//////////////////////////////////////////////////////////////////////////////
//
// WintrustLoadFunctionPointers
//----------------------------------------------------------------------------
//  Retrieves the function entry points based on the Action ID given.
//
// Returns:
//      TRUE                            success.
//      FALSE                           fail.
//
(*extern BOOL WINAPI WintrustLoadFunctionPointers(GUID *pgActionID, CRYPT_PROVIDER_FUNCTIONS *pPfns);
  *)

//////////////////////////////////////////////////////////////////////////////
//
// WintrustAddDefaultForUsage
//----------------------------------------------------------------------------
//  Sets the default Action ID for the usage.  If the provider uses this
//  function, and the provider requires any of the "callback" data in
//  WINTRUST_DATA to be filled out, it MUST completely fill out the
//  CRYPT_PROVIDER_REGDEFUSAGE structure.
//
// Returns:
//      TRUE                            success.
//      FALSE                           fail.
//
(*extern BOOL WINAPI              WintrustAddDefaultForUsage(IN const char *pszUsageOID,
                                                           IN CRYPT_PROVIDER_REGDEFUSAGE *psDefUsage);
  *)
//////////////////////////////////////////////////////////////////////////////
//
// WintrustGetDefaultForUsage
//----------------------------------------------------------------------------
//  Retrieves the Action ID and default callback data for the specified usage
//
//  this function must be called again with dwAction set to FREE to deallocate
//
//
// Returns:
//      TRUE                            success.
//      FALSE                           fail.
//
const
  DWACTION_ALLOCANDFILL           = 1;
  DWACTION_FREE                   = 2;

(*extern BOOL WINAPI              WintrustGetDefaultForUsage(IN DWORD dwAction,
                                                           IN const char *pszUsageOID,
                                                           IN OUT CRYPT_PROVIDER_DEFUSAGE *psUsage);

extern CRYPT_PROVIDER_SGNR * WINAPI     WTHelperGetProvSignerFromChain(CRYPT_PROVIDER_DATA *pProvData,
                                                                       DWORD idxSigner,
                                                                       BOOL fCounterSigner,
                                                                       DWORD idxCounterSigner);
extern CRYPT_PROVIDER_CERT * WINAPI     WTHelperGetProvCertFromChain(CRYPT_PROVIDER_SGNR *pSgnr,
                                                                     DWORD idxCert);

extern CRYPT_PROVIDER_DATA * WINAPI     WTHelperProvDataFromStateData(HANDLE hStateData);

extern CRYPT_PROVIDER_PRIVDATA * WINAPI WTHelperGetProvPrivateDataFromChain(CRYPT_PROVIDER_DATA *pProvData,
                                                                            GUID *pgProviderID);
extern BOOL WINAPI                      WTHelperCertIsSelfSigned(DWORD dwEncoding, CERT_INFO *pCert);

extern HRESULT WINAPI                   WTHelperCertCheckValidSignature(CRYPT_PROVIDER_DATA *pProvData);
  *)
//////////////////////////////////////////////////////////////////////////////
//
// Supported ASN structures contained in WINTRUST.DLL
//----------------------------------------------------------------------------
//
//#include <pshpack8.h>

//
//  CTL Trusted CA Lists
//
const
  szOID_TRUSTED_CODESIGNING_CA_LIST   = '1.3.6.1.4.1.311.2.2.1';
  szOID_TRUSTED_CLIENT_AUTH_CA_LIST   = '1.3.6.1.4.1.311.2.2.2';
  szOID_TRUSTED_SERVER_AUTH_CA_LIST   = '1.3.6.1.4.1.311.2.2.3';

//
//  encode/decode OID defines
//
  SPC_COMMON_NAME_OBJID               = szOID_COMMON_NAME;
  SPC_TIME_STAMP_REQUEST_OBJID        = '1.3.6.1.4.1.311.3.2.1';
  SPC_INDIRECT_DATA_OBJID             = '1.3.6.1.4.1.311.2.1.4';
  SPC_SP_AGENCY_INFO_OBJID            = '1.3.6.1.4.1.311.2.1.10';
  SPC_STATEMENT_TYPE_OBJID            = '1.3.6.1.4.1.311.2.1.11';
  SPC_SP_OPUS_INFO_OBJID              = '1.3.6.1.4.1.311.2.1.12';
  SPC_CERT_EXTENSIONS_OBJID           = '1.3.6.1.4.1.311.2.1.14';
  SPC_PE_IMAGE_DATA_OBJID             = '1.3.6.1.4.1.311.2.1.15';
  SPC_RAW_FILE_DATA_OBJID             = '1.3.6.1.4.1.311.2.1.18';
  SPC_STRUCTURED_STORAGE_DATA_OBJID   = '1.3.6.1.4.1.311.2.1.19';
  SPC_JAVA_CLASS_DATA_OBJID           = '1.3.6.1.4.1.311.2.1.20';
  SPC_INDIVIDUAL_SP_KEY_PURPOSE_OBJID = '1.3.6.1.4.1.311.2.1.21';
  SPC_COMMERCIAL_SP_KEY_PURPOSE_OBJID = '1.3.6.1.4.1.311.2.1.22';
  SPC_CAB_DATA_OBJID                  = '1.3.6.1.4.1.311.2.1.25';
  SPC_GLUE_RDN_OBJID                  = '1.3.6.1.4.1.311.2.1.25';    // obsolete!
  SPC_MINIMAL_CRITERIA_OBJID          = '1.3.6.1.4.1.311.2.1.26';
  SPC_FINANCIAL_CRITERIA_OBJID        = '1.3.6.1.4.1.311.2.1.27';
  SPC_LINK_OBJID                      = '1.3.6.1.4.1.311.2.1.28';
  SPC_SIGINFO_OBJID                   = '1.3.6.1.4.1.311.2.1.30';

//
//  Page hash versions
//
  SPC_PE_IMAGE_PAGE_HASHES_V1_OBJID   = '1.3.6.1.4.1.311.2.3.1';     // V1

//
//  Catalog entries
//
  CAT_NAMEVALUE_OBJID                 = '1.3.6.1.4.1.311.12.2.1';
  CAT_MEMBERINFO_OBJID                = '1.3.6.1.4.1.311.12.2.2';

//
//  encode/decode internal defines
//
  {  hier
  SPC_SP_AGENCY_INFO_STRUCT           : PAnsiChar = PAnsiChar(2000);
  SPC_MINIMAL_CRITERIA_STRUCT         ((LPCSTR) 2001)
  SPC_FINANCIAL_CRITERIA_STRUCT       ((LPCSTR) 2002)
  SPC_INDIRECT_DATA_CONTENT_STRUCT    ((LPCSTR) 2003)
  SPC_PE_IMAGE_DATA_STRUCT            ((LPCSTR) 2004)
  SPC_LINK_STRUCT                     ((LPCSTR) 2005)
  SPC_STATEMENT_TYPE_STRUCT           ((LPCSTR) 2006)
  SPC_SP_OPUS_INFO_STRUCT             ((LPCSTR) 2007)
  SPC_CAB_DATA_STRUCT                 ((LPCSTR) 2008)
  SPC_JAVA_CLASS_DATA_STRUCT          ((LPCSTR) 2009)

  SPC_SIGINFO_STRUCT                  ((LPCSTR) 2130)

  CAT_NAMEVALUE_STRUCT                ((LPCSTR) 2221)
  CAT_MEMBERINFO_STRUCT               ((LPCSTR) 2222)

  SPC_UUID_LENGTH    = 16;

type
  BYTE SPC_UUID = array[0..SPC_UUID_LENGTH-1] of byte;

const
  SpcSerializedObjectAttributesClassId    0xA6, 0xB5, 0x86, 0xD5, \
                                                 0xB4, 0xA1, 0x24, 0x66, \
                                                 0xAE, 0x05, 0xA2, 0x17, \
                                                 0xDA, 0x8E, 0x60, 0xD6
                         }

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
