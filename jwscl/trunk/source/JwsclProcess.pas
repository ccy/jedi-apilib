{@abstract(Contains process, thread und library classes that are used by the units of JWSCL)
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

The Original Code is JwsclProcess.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

Description:

}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclProcess;
{$INCLUDE Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $

interface

uses jwaWindows, JwsclTypes, JwsclToken,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
  TJwSecurityLibrary = class
  private
  protected
  public
    class function LoadLibProc(const LibName: string;
      const ProcName: string): Pointer;
  end;

  TJwProcessOutputInformation = record
    UserToken  : TJwSecurityToken;
    ProcessInfo: TProcessInformation;
    EnvBlock : Pointer;
    ProfileInfo : TJwProfileInfo;
  end;


{undocumented}  
procedure JwCreateProcessInSession(
  const ApplicationName : TJwString;
  const CommandLine : TJwString;
  const CurrentDirectory : TJwString;

  const SessionID : DWORD;
  const CreationFlags : DWORD;
  const Desktop: TJwString;

  StartupInfo : {$IFDEF UNICODE}TStartupInfoW{$ELSE}TStartupInfoA{$ENDIF};

  WaitForProcess : Boolean;
  out Output : TJwProcessOutputInformation;
  var LogInfo : TJwString
  );


{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation
uses SysUtils, Classes, JwsclTerminalServer;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

class function TJwSecurityLibrary.LoadLibProc(const LibName: string; const ProcName: string): Pointer;
var
  LibHandle: THandle;
begin
  LibHandle := LoadLibraryA(PChar(LibName));
  if LibHandle = 0 then
    Result := nil
  else
  begin
    try
      Result := GetProcAddress(LibHandle, PChar(ProcName));
    finally
      FreeLibrary(LibHandle); // Free Memory Allocated for the DLL
    end;
  end;
end;






procedure JwCreateProcessInSession(
  const ApplicationName : TJwString;
  const CommandLine : TJwString;
  const CurrentDirectory : TJwString;

  const SessionID : DWORD;
  const CreationFlags : DWORD;
  const Desktop: TJwString;

  StartupInfo : {$IFDEF UNICODE}TStartupInfoW{$ELSE}TStartupInfoA{$ENDIF};

  WaitForProcess : Boolean;
  out Output : TJwProcessOutputInformation;
  var LogInfo : TJwString
  );


  procedure GetPChar(const Str : TJwString; var CharPtr : TJwPChar);
  begin
    if Length(Str) > 0 then
      CharPtr := TJwPChar(Str)
    else
      CharPtr := nil;
  end;

  function CreateTokenByProcessAndSession(
    const SessionID : DWORD;
    const Log : TStringList) : TJwSecurityToken;
  var TSrv : TJwTerminalServer;
      i : Integer;
      ProcessID : DWORD;
  begin
    result := nil;

    Log.Add('Running CreateTokenByProcessAndSession');
    TSrv := TJwTerminalServer.Create;
    try
      ProcessID := 0;
      if TSrv.EnumerateProcesses then
        for i := 0 to TSrv.Processes.Count-1 do
        begin
          if TSrv.Processes[i].SessionId = SessionID then
          begin
            ProcessID := TSrv.Processes[i].ProcessId;
            break;
          end;
        end;

      if ProcessID > 0 then
      begin
        try
          result := TJwSecurityToken.CreateTokenByProcessId(ProcessID, MAXIMUM_ALLOWED)
        except
          On E : Exception do
          begin
            Log.Add('Could not get user token by Process: '#13#10+E.Message);
            raise;
          end;
        end;
      end
      else
        Log.Add('Could not find any process ID.');
    finally
      TSrv.Free;
      Log.Add('Exiting CreateTokenByProcessAndSession.');
    end;
  end;


var
    Log : TStringList;


    lpApplicationName  : TJwPChar;
    lpCommandLine      : TJwPChar;
    lpCurrentDirectory : TJwPChar;
begin
  Output.UserToken := nil;
  ZeroMemory(@Output.ProcessInfo, sizeof(Output.ProcessInfo));
  Output.EnvBlock := nil;
  ZeroMemory(@Output.ProfileInfo, sizeof(ProfileInfo));
  LogInfo := '';

  Log := TStringList.Create;
  try
    Log.Add(Format('Running CreateProcessInSession(Sesion=%d):',[SessionID]));
    try
      Log.Add('Getting user token...');
      Output.UserToken := TJwSecurityToken.CreateWTSQueryUserTokenEx(SessionID);
    except
      //on E2 : EJwsclUnsupportedWindowsVersionException do
      On E2 : Exception do
      begin
        try
          Output.UserToken := CreateTokenByProcessAndSession(SessionId,Log);
        except
          on E : Exception do
          begin
            Log.Add('Could not retrieve user token: '+#13#10+E.Message);
            raise;
          end;
        end;
      end;
    end;

    Log.Add('Loading user profile');
    try
      Output.UserToken.LoadUserProfile(Output.ProfileInfo, []);

      with StartupInfo do
      begin
        cb          := SizeOf(StartupInfo);
        if Length(Desktop) = 0 then
          lpDesktop   := 'WinSta0\Default'
        else
          lpDesktop   := TJwPChar(Desktop);
      end;

      GetPChar(ApplicationName, lpApplicationName);
      GetPChar(CommandLine, lpCommandLine);
      GetPChar(CurrentDirectory, lpCurrentDirectory);

      CreateEnvironmentBlock(@Output.EnvBlock, Output.UserToken.TokenHandle, true);

      if not CreateProcessAsUser(
        Output.UserToken.TokenHandle,//HANDLE hToken,
        lpApplicationName,//__in_opt     LPCTSTR lpApplicationName,
        lpCommandLine, //__inout_opt  LPTSTR lpCommandLine,
        nil,//__in_opt     LPSECURITY_ATTRIBUTES lpProcessAttributes,
        nil,//__in_opt     LPSECURITY_ATTRIBUTES lpThreadAttributes,
        true,//__in         BOOL bInheritHandles,
        CreationFlags or CREATE_UNICODE_ENVIRONMENT,//__in         DWORD dwCreationFlags,
        Output.EnvBlock,//__in_opt     LPVOID lpEnvironment,
        lpCurrentDirectory,//__in_opt     LPCTSTR lpCurrentDirectory,
        StartupInfo,//__in         LPSTARTUPINFO lpStartupInfo,
        Output.ProcessInfo //__out        LPPROCESS_INFORMATION lpProcessInformation
      ) then
      begin
        Log.Add('Failed CreateProcessAsUser.');
        RaiseLastOSError;
      end;

      if WaitForProcess then
      begin
        Log.Add('Wait for process...');
        WaitForSingleObject(Output.ProcessInfo.hProcess, INFINITE);
        Log.Add('Process exited... cleaning up');

        DestroyEnvironmentBlock(Output.EnvBlock);
        Output.EnvBlock := nil;
        Output.UserToken.UnLoadUserProfile(Output.ProfileInfo);
        FreeAndNil(Output.UserToken);
      end;

    except
      on E : Exception do
      begin
        DestroyEnvironmentBlock(Output.EnvBlock);
        Output.EnvBlock := nil;
        Output.UserToken.UnLoadUserProfile(Output.ProfileInfo);
        FreeAndNil(Output.UserToken);

        Log.Add('Exception (between LoadUserProfile and CreateProcessAsUser) : '#13#10+E.Message);
        raise;
      end;
    end;

  finally
    Log.Add(Format('Exiting CreateProcessInSession(Sesion=%d):',[SessionID]));

    LogInfo := LogInfo + #13#10 + Log.Text;
    Log.Free;
  end;
end;


{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}