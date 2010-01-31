{******************************************************************************}
{                                                                              }
{ XXXX API interface Unit for Object Pascal                                    }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2008 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by Christian Wimmer are Copyright (C) Christian Wimmer      }
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
{ This unit declares some COM interfaces and Co-functions for                  }
{ JwsclComSecurity.pas. Some of these declarations are already declared in     }
{ e.g. JwaActiveX.pas                                                          }
{ Currently this unit must not be added to JwaWindows.pas                      }
{ TODO:                                                                        }
{   1. Remove or exclude duplicate declarations to avoid errors with JwaWindows}
{   2. What about BCC ?                                                        }
{   3. What about Delphi >=5 ? Some of the declarations here could not work.   }
{******************************************************************************}
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaCOMSecurity;
{$ENDIF JWA_OMIT_SECTIONS}

{.$HPPEMIT ''}
{.$HPPEMIT '#include "xxxx.h"'}
{.$HPPEMIT ''}

{$I ..\Includes\JediAPILib.inc}

{$IFNDEF JWA_OMIT_SECTIONS}

interface

{$IFDEF JWA_WINDOWS}
  uses JwaWindows;
{$ELSE}
  uses JwaWinType, JwaActiveX, JwaWinNT, JwaRpcDce;
{$ENDIF JWA_WINDOWS}


{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}


type
  tagSOLE_AUTHENTICATION_SERVICE = record
    dwAuthnSvc,
    dwAuthzSvc : DWORD;
    pPrincipalName : POleStr;
    hr : HRESULT;
  end;
  SOLE_AUTHENTICATION_SERVICE = tagSOLE_AUTHENTICATION_SERVICE;
  PSOLE_AUTHENTICATION_SERVICE = ^SOLE_AUTHENTICATION_SERVICE;

  tagEOLE_AUTHENTICATION_CAPABILITIES = (
      EOAC_NONE               = 0,
      EOAC_MUTUAL_AUTH        = $1,
      EOAC_STATIC_CLOAKING    = $20,
      EOAC_DYNAMIC_CLOAKING   = $40,
      EOAC_ANY_AUTHORITY      = $80,
      EOAC_MAKE_FULLSIC       = $100,
      EOAC_DEFAULT            = $800,
      EOAC_SECURE_REFS        = $2,
      EOAC_ACCESS_CONTROL     = $4,
      EOAC_APPID              = $8,
      EOAC_DYNAMIC            = $10,
      EOAC_REQUIRE_FULLSIC    = $200,
      EOAC_AUTO_IMPERSONATE   = $400,
      EOAC_NO_CUSTOM_MARSHAL  = $2000,
      EOAC_DISABLE_AAA        = $1000
      );
  EOLE_AUTHENTICATION_CAPABILITIES = tagEOLE_AUTHENTICATION_CAPABILITIES;

const COLE_DEFAULT_PRINCIPAL = PWideChar (-1);

const COLE_DEFAULT_AUTHINFO = Pointer(-1);

type
  tagSOLE_AUTHENTICATION_INFO = record
    dwAuthnSvc,
    dwAuthzSvc : DWORD;
    pAuthInfo : Pointer;
  end;
  SOLE_AUTHENTICATION_INFO = tagSOLE_AUTHENTICATION_INFO;
  PSOLE_AUTHENTICATION_INFO = ^tagSOLE_AUTHENTICATION_INFO;

  tagSOLE_AUTHENTICATION_LIST = record
    cAuthInfo : DWORD;
    aAuthInfo : PSOLE_AUTHENTICATION_INFO;
  end;
  SOLE_AUTHENTICATION_LIST = tagSOLE_AUTHENTICATION_LIST;
  PSOLE_AUTHENTICATION_LIST = ^tagSOLE_AUTHENTICATION_LIST;

  IClientSecurity = interface
  ['{0000013D-0000-0000-C000-000000000046}']
      function QueryBlanket(
          {var} pProxy : IUnknown;
          out pAuthnSvc : DWORD;
          {out_opt}pAuthzSvc : PDWORD;
          {__RPC__deref_out_opt}  pServerPrincName : PPOleStr;
          {__out_opt}  pAuthnLevel : PDWORD;
          {__out_opt}  pImpLevel : PDWORD;
          {__deref_out_opt}  var pAuthInfo : Pointer;
          {__out_opt}  pCapabilites : PDWORD) : HRESULT; stdcall;

      function SetBlanket(
          {__in}  {var} pProxy : IUnknown;
          {__in}  dwAuthnSvc : DWORD;
          {__in}  dwAuthzSvc : DWORD;
          {__RPC__in_opt}  pServerPrincName : POleStr;
          {__in}  dwAuthnLevel : DWORD;
          {__in}  dwImpLevel : DWORD;
          {__in_opt}  pAuthInfo : Pointer;
          {__in}  dwCapabilities : DWORD) : HRESULT; stdcall;

      function CopyProxy(
          {__in}  pProxy : IUnknown;
          {__deref_out}  out ppCopy : IUnknown) : HRESULT; stdcall;
  end;

  IServerSecurity = interface
  ['{0000013E-0000-0000-C000-000000000046}']
      function QueryBlanket(
          {__out_opt}  pAuthnSvc : PDWORD;
          {__out_opt}  pAuthzSvc : PDWORD;
          {__RPC__deref_out_opt}  pServerPrincName : PPOleStr;
          {__out_opt}  pAuthnLevel : PDWORD;
          {__out_opt}  pImpLevel : PDWORD;
          {__deref_out_opt}  var pPrivs : Pointer;
          {__inout_opt}  pCapabilities : PDWORD) : HRESULT; stdcall;

      function ImpersonateClient() : HRESULT; stdcall;

      function RevertToSelf() : HRESULT; stdcall;

      function IsImpersonating() : Bool; stdcall;
  end;





function CoInitializeSecurity(
          {__in_opt}pSecDesc : PSECURITY_DESCRIPTOR;
          {__in} cAuthSvc : LONG;
          {__in_ecount_opt(cAuthSvc)   }
               asAuthSvc : PSOLE_AUTHENTICATION_SERVICE;
          {__in_opt} pReserved1 : Pointer;
          {__in} dwAuthnLevel : DWORD;
          {__in} dwImpLevel  : DWORD;
          {__in_opt} pAuthList : Pointer;
          {__in} dwCapabilities  : DWORD;
          {__in_opt} pReserved3 : Pointer) : HRESULT; stdcall;


function CoGetCallContext(
          {__in} const riid : TIID;
          {__deref_out} ppInterface : Pointer) : HRESULT; stdcall;


function CoQueryProxyBlanket(
    {__in} pProxy : IUnknown;
    {__out_opt} pwAuthnSvc : PDWORD;
    {__out_opt} pAuthzSvc : PDWORD;
    {__deref_opt_out} pServerPrincName : PPOleStr;
    {__out_opt} pAuthnLevel : PDWORD;
    {__out_opt} pImpLevel : PDWORD;
    {__out_opt} pAuthInfo : PRPC_AUTH_IDENTITY_HANDLE;
    {__out_opt} pCapabilites  : PDWORD) : HRESULT; stdcall;


function CoSetProxyBlanket(
    {__in} pProxy : IUnknown;
    {__in} dwAuthnSvc : DWORD;
    {__in} dwAuthzSvc  : DWORD;
    {__in_opt} pServerPrincName : POleStr;
    {__in} dwAuthnLevel  : DWORD;
    {__in} dwImpLevel : DWORD;
    {__in_opt} pAuthInfo : RPC_AUTH_IDENTITY_HANDLE;
    {__in} dwCapabilities : DWORD) : HRESULT; stdcall;


function CoCopyProxy(
    {__in} pProxy : IUnknown;
    {__deref_out} out ppCopy : IUnknown) : HRESULT; stdcall;


function CoQueryClientBlanket(
    {__out_opt} pAuthnSvc : PDWORD;
    {__out_opt} pAuthzSvc : PDWORD;
    {__out_opt} pServerPrincName : PPOleStr;
    {__out_opt} pAuthnLevel : PDWORD;
    {__out_opt} pImpLevel : PDWORD;
    {__out_opt} pPrivs : PRPC_AUTHZ_HANDLE;
    {__inout_opt} pCapabilities : PDWORD) : HRESULT; stdcall;


function CoImpersonateClient() : HRESULT; stdcall;


function CoRevertToSelf() : HRESULT; stdcall;


function CoQueryAuthenticationServices(
    {__out} out pcAuthSvc : DWORD;
    {__deref_out_ecount( pcAuthSvc)}
    var asAuthSvc : PSOLE_AUTHENTICATION_SERVICE) : HRESULT; stdcall;

const COM_RIGHTS_EXECUTE = 1;
const COM_RIGHTS_EXECUTE_LOCAL = 2;
const COM_RIGHTS_EXECUTE_REMOTE = 4;
const COM_RIGHTS_ACTIVATE_LOCAL = 8;
const COM_RIGHTS_ACTIVATE_REMOTE = 16;
{$ENDIF JWA_IMPLEMENTATIONSECTION}




{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDllNames;
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}



{$IFDEF DYNAMIC_LINK}


var
  _CoInitializeSecurity: Pointer;

function CoInitializeSecurity;
begin
  GetProcedureAddress(_CoInitializeSecurity, ole32, 'CoInitializeSecurity');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoInitializeSecurity]
  end;
end;

var
  _CoGetCallContext: Pointer;

function CoGetCallContext;
begin
  GetProcedureAddress(_CoGetCallContext, ole32, 'CoGetCallContext');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoGetCallContext]
  end;
end;

var
  _CoQueryProxyBlanket: Pointer;

function CoQueryProxyBlanket;
begin
  GetProcedureAddress(_CoQueryProxyBlanket, ole32, 'CoQueryProxyBlanket');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoQueryProxyBlanket]
  end;
end;

var
  _CoSetProxyBlanket: Pointer;

function CoSetProxyBlanket;
begin
  GetProcedureAddress(_CoSetProxyBlanket, ole32, 'CoSetProxyBlanket');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoSetProxyBlanket]
  end;
end;

var
  _CoCopyProxy: Pointer;

function CoCopyProxy;
begin
  GetProcedureAddress(_CoCopyProxy, ole32, 'CoCopyProxy');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoCopyProxy]
  end;
end;

var
  _CoQueryClientBlanket: Pointer;

function CoQueryClientBlanket;
begin
  GetProcedureAddress(_CoQueryClientBlanket, ole32, 'CoQueryClientBlanket');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoQueryClientBlanket]
  end;
end;

var
  _CoImpersonateClient: Pointer;

function CoImpersonateClient;
begin
  GetProcedureAddress(_CoImpersonateClient, ole32, 'CoImpersonateClient');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoImpersonateClient]
  end;
end;

var
  _CoRevertToSelf: Pointer;

function CoRevertToSelf;
begin
  GetProcedureAddress(_CoRevertToSelf, ole32, 'CoRevertToSelf');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoRevertToSelf]
  end;
end;

var
  _CoQueryAuthenticationServices: Pointer;

function CoQueryAuthenticationServices;
begin
  GetProcedureAddress(_CoQueryAuthenticationServices, ole32, 'CoQueryAuthenticationServices');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoQueryAuthenticationServices]
  end;
end;

var
  _CoSwitchCallContext: Pointer;

function CoSwitchCallContext;
begin
  GetProcedureAddress(_CoSwitchCallContext, ole32, 'CoSwitchCallContext');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoSwitchCallContext]
  end;
end;
{$ELSE}

function CoInitializeSecurity; external ole32 name 'CoInitializeSecurity';
function CoGetCallContext; external ole32 name 'CoGetCallContext';
function CoQueryProxyBlanket; external ole32 name 'CoQueryProxyBlanket';
function CoSetProxyBlanket; external ole32 name 'CoSetProxyBlanket';
function CoCopyProxy; external ole32 name 'CoCopyProxy';
function CoQueryClientBlanket; external ole32 name 'CoQueryClientBlanket';
function CoImpersonateClient; external ole32 name 'CoImpersonateClient';
function CoRevertToSelf; external ole32 name 'CoRevertToSelf';
function CoQueryAuthenticationServices; external ole32 name 'CoQueryAuthenticationServices';


{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
