{
@abstract(Contains structures to support scopy based impersonation.)
@author(Christian Wimmer)
@created(03/23/2007)
@lastmod(09/10/2007)



Project JEDI Windows Security Code Library (JWSCL)

The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the  
GNU Lesser General Public License (the  "LGPL License"), in which case the   
provisions of the LGPL License are applicable instead of those above.        
If you wish to allow use of your version of this file only under the terms   
of the LGPL License and not to allow others to use your version of this file 
under the MPL, indicate your decision by deleting  the provisions above and  
replace  them with the notice and other provisions required by the LGPL      
License.  If you do not delete the provisions above, a recipient may use     
your version of this file under either the MPL or the LGPL License.          
                                                                             
For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html 

The Original Code is JwsclImpersonation.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

}
unit JwsclImpersonation;
{$INCLUDE Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $


interface
uses ActiveX,
     JwaWindows,
     JwsclTypes,
     JwsclToken,
     JwsclSid,
     JwsclAcl,
     JwsclLsa,
     JwsclResource,
     JwsclExceptions,
     JwsclComUtils;

type {@Name defines an interface for TJwImpersonation. }
    IJwImpersonation = Interface(IUnknown)
     end;

     {@Name provides methods to impersonate a logged on client.
      Do not use this class instead use JwImpersonateLoggedOnUser, JwImpersonateLoggedOnUser or
      JwImpersonateLoggedOnUser.
     }
     TJwImpersonation = class(TInterfacedObject, IJwImpersonation)
     private

     protected
       fToken : TJwSecurityToken;
     public
       constructor Create(const LogonSessionLuid : TLuid); overload;
       constructor Create(const LogonSessionId : ULONG); overload;
       constructor Create(const UseWTSCall : Boolean = false); overload;

       destructor Destroy; override;

       property Token : TJwSecurityToken read fToken;
     end;

{@Name impersonates the current and returns an interface pointer to the
token. It's automatically freed and revert to self if run out of scope.
@raises(EJwsclProcessIdNotAvailable will be raised it the process does not have
  SE_TCB_NAME privilege and a try to get the explorer handle failed)
@raises(EJwsclWinCallFailedException will be raised if OpenProcess fails)
}
function JwImpersonateLoggedOnUser: IJwImpersonation; overload;

{@Name impersonates the current and returns an interface pointer to the
token. It's automatically freed and revert to self if run out of scope.
@param(LogonSessionId defines the user's logon session id.)
}
function JwImpersonateLoggedOnUser(const LogonSessionId : ULONG) : IJwImpersonation; overload;

{@Name impersonates the current and returns an interface pointer to the
token. It's automatically freed and revert to self if run out of scope.
@param(LogonSessionLuid defines the session luid to be impersonated.)}
function JwImpersonateLoggedOnUser(const LogonSessionLuid : TLuid) : IJwImpersonation; overload;



implementation
uses SysUtils;

function JwImpersonateLoggedOnUser: IJwImpersonation;
begin
  try
    //first try WTS call then look for explorer
    //WTSXXX calls need TCB privilege - so it often simply fails
    result := TJwImpersonation.Create(false);
  except
    on E : EJwsclSecurityException do
      result := TJwImpersonation.Create(false);
  end;
end;
function JwImpersonateLoggedOnUser(const LogonSessionId : ULONG) : IJwImpersonation;
begin
  result := TJwImpersonation.Create(LogonSessionID);
end;

function JwImpersonateLoggedOnUser(const LogonSessionLuid : TLuid) : IJwImpersonation;
begin
  result := TJwImpersonation.Create(LogonSessionLuid);
end;


{ TJwImpersonation }

constructor TJwImpersonation.Create(const LogonSessionLuid: TLuid);
var SessionData : TJwLsaLogonSessionData;
begin
  SessionData := TJwLsaLogonSession.GetSessionData(LogonSessionLuid);
  Create(SessionData.Session);
end;

constructor TJwImpersonation.Create(const LogonSessionId : ULONG);
begin
  fToken := TJwSecurityToken.CreateWTSQueryUserToken(LogonSessionId);
  fToken.ConvertToImpersonatedToken(SecurityImpersonation,
        TOKEN_IMPERSONATE or TOKEN_DUPLICATE or TOKEN_READ or TOKEN_QUERY);
  fToken.ImpersonateLoggedOnUser;
end;

constructor TJwImpersonation.Create(const UseWTSCall : Boolean = false);
var ShellWindow : HWND;
    ThreadId,
    ProcessId : DWORD;
    ProcessHandle : THandle;

const ProgManName : WideString = 'Progman1';

begin
  if UseWTSCall then
    Create(WTSGetActiveConsoleSessionId)
  else
  begin
    ShellWindow := FindWindowExW(0,0,PWideChar(ProgManName),nil);
    ThreadId := GetWindowThreadProcessId(ShellWindow,@ProcessId);
    if ThreadId = 0 then
    begin
      raise EJwsclProcessIdNotAvailable.CreateFmtEx(
        RsProcessIdNotFound,// const MessageString: string;
        'Create',ClassName,//sSourceProc, sSourceClass,
        RsUNImpersonation,//sSourceFile: string;
        0,//iSourceLine:  Cardinal;
        true,//bShowLastError: boolean;
        [ProgManName, ShellWindow]//const Args: array of const
        );
    end;

    ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION, false, ProcessId);
    if ProcessHandle = 0 then
    begin
      raise EJwsclWinCallFailedException.CreateFmtWinCall(
        RsOpenProcessFailed,// const MessageString: string;
        'Create',ClassName,//sSourceProc, sSourceClass,
        RsUNImpersonation,//sSourceFile: string;
        0,//iSourceLine:  Cardinal;
        true,//bShowLastError: boolean;
        'OpenProcess',
        [ProgManName, ShellWindow]//const Args: array of const
        );
    end;
    try
      fToken := TJwSecurityToken.CreateTokenByProcess(ProcessHandle,
        TOKEN_IMPERSONATE or TOKEN_DUPLICATE or TOKEN_READ or TOKEN_QUERY);
    finally
      CloseHandle(ProcessHandle);
    end;

    try
      fToken.ConvertToImpersonatedToken(SecurityImpersonation,
        TOKEN_IMPERSONATE or TOKEN_DUPLICATE or TOKEN_READ or TOKEN_QUERY);
      fToken.ImpersonateLoggedOnUser;
    except
      on E : EJwsclSecurityException do
      begin
        FreeAndNil(fToken);
        raise;
      end;
    end;
  end;
end;

destructor TJwImpersonation.Destroy;
begin
  if Assigned(fToken) then
    fToken.RevertToSelf;
   
  FreeAndNil(fToken);
  inherited;
end;

procedure Test;
var t : IJwImpersonation;
    K : TJwSecurityToken;
    user : TJwSecurityId;

begin
  t := JwImpersonateLoggedOnUser;
  K := TJwSecurityToken.CreateTokenEffective(MAXIMUM_ALLOWED);
  user := K.TokenUser;
  user.StringSID;
  user.Free;

  if t = nil then;
end;

initialization





end.
