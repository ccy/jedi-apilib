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

uses jwaWindows, JwsclTypes, JwsclToken, JwsclSid, JwsclTerminalServer,
  Classes,
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

function JwGetTokenFromProcess (const OnProcessFound : TJwOnProcessFound;
  Log : TStringList; Data : Pointer) : TJwSecurityToken;

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation
uses SysUtils, Dialogs, JwsclExceptions,
  JwsclKnownSid,
  JwsclAcl, JwsclConstants;

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

type
  PInternalProcessData = ^TInternalProcessData;
  TInternalProcessData = record
    SessionID : DWORD;
  end;

procedure OnProcessFoundInSameSession(const Sender1, Sender2 : TJwTerminalServer; var Process : TJwWTSProcess;
      var Cancel : Boolean; Data : Pointer);
var ProcessData : PInternalProcessData absolute Data;
begin
  Cancel :=
      {
        same session
      }
      (Process.SessionId = ProcessData.SessionID)

      {
        no idle process
      }
      and (Process.ProcessId > 0)
      {
        We have to check for system processes in that session.
        So we ignore them otherwise the user gets a system elevated process.
      }
      and (Assigned(Process.UserSid) and not Process.UserSid.EqualSid(JwLocalSystemSID));
end;



function JwGetTokenFromProcess (const OnProcessFound : TJwOnProcessFound; Log : TStringList; Data : Pointer) : TJwSecurityToken;
var TSrv : TJwTerminalServer;
    i : Integer;
    ProcessID : DWORD;
    Succ : Boolean;
    Sid : TJwSecurityId;
    Cancel : Boolean;
    Process : TJwWTSProcess;
begin
  result := nil;
  Succ := false;

  //try to enable debug privs if available - otherwise nothing
  JwEnablePrivilege(SE_DEBUG_NAME,pst_EnableIfAvail);

  Log.Add(Format('Running CreateTokenByProcessAndSession(SessionID: %d',[0]));

  TSrv := TJwTerminalServer.Create;
  try
    TSrv.Connect;

    ProcessID := 0;
    if TSrv.EnumerateProcesses then
    begin
      Log.Add('Proc count: ' + IntToStr(TSrv.Processes.Count));
      for i := 0 to TSrv.Processes.Count-1 do
      begin
        Log.Add(Format('Proc: %d, Name= %s SessionID: %d',[TSrv.Processes[i].ProcessId,
          TSrv.Processes[i].ProcessName, TSrv.Processes[i].SessionId]));


        Cancel := true;
        Process := TSrv.Processes[i];
        OnProcessFound(TSrv, Process, Cancel, Data);
        if Cancel then
        begin
          //found a process in that specified session
          ProcessID := TSrv.Processes[i].ProcessId;

          try
            Succ := true;
            {
              Get token by process handle and duplicate it
            }
            Log.Add('call CreateDuplicateExistingToken');
            result := TJwSecurityToken.CreateDuplicateExistingToken(TSrv.Processes[i].Token.TokenHandle,
                MAXIMUM_ALLOWED);
            {DEBUG: raise Exception.Create('');}
          except
            On E : Exception do
            begin
              Succ := False;
              Log.Add('CreateDuplicateExistingToken failed: '#13#10+E.Message);


              //try to get the token the old fashioned way
              try
                Succ := true;

                result := TJwSecurityToken.CreateTokenByProcessId(ProcessID,
                //these are the only necessary rights
                  TOKEN_ASSIGN_PRIMARY or
                  TOKEN_QUERY or TOKEN_IMPERSONATE or TOKEN_DUPLICATE or TOKEN_READ);

                //  MAXIMUM_ALLOWED); CreateTokenByProcessId copies the token handle so we always get full access

              except
                On E : Exception do
                begin
                  Log.Add('Could not get user token by Process: '#13#10+E.Message);
                  Succ := False;
                  ProcessID := 0;
                end;
              end;
            end;
          end;

          //quit loop if success and process is found
          if Succ and (ProcessID > 0) then
            break;
        end;
      end
    end
    else
      Log.Add('EnumerateProcesses failed.');

    if ProcessID = 0 then
      Log.Add('Could not find any process ID.');
  finally
    TSrv.Free;
    Log.Add('Exiting CreateTokenByProcessAndSession.');
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

  var //log data is stored here
      Log : TStringList;

  function CreateTokenByProcessAndSession(
    const SessionID : DWORD) : TJwSecurityToken;
  var TSrv : TJwTerminalServer;
      i : Integer;
      Succ : Boolean;
      Sid : TJwSecurityId;
      Meth : TMethod;
      Data : TInternalProcessData;
  begin
    result := nil;

    //try to enable debug privs if available - otherwise nothing
    JwEnablePrivilege(SE_DEBUG_NAME,pst_EnableIfAvail);

    Log.Add(Format('Running CreateTokenByProcessAndSession(SessionID: %d',[SessionID]));

    TSrv := TJwTerminalServer.Create;
    try
      TSrv.Connect;

      //convert procedure into method
      Meth.Code := @OnProcessFoundInSameSession;
      Meth.Data := nil; //Self pointer

      Data.SessionID := SessionID;
      // enumerate all processes of the terminal server
      // we also get many
      //
      result := JwGetTokenFromProcess (TJwOnProcessFound(Meth), Log, @Data);
    finally
      TSrv.Free;
      Log.Add('Exiting CreateTokenByProcessAndSession.');
    end;
  end;


var
    lpApplicationName  : TJwPChar;
    lpCommandLine      : TJwPChar;
    lpCurrentDirectory : TJwPChar;
begin
  JwInitWellKnownSIDs;

  try


  Output.UserToken := nil;
  ZeroMemory(@Output.ProcessInfo, sizeof(Output.ProcessInfo));
  Output.EnvBlock := nil;
  ZeroMemory(@Output.ProfileInfo, sizeof(ProfileInfo));
  LogInfo := '';

  Log := TStringList.Create;
  try
    Log.Add(Format('Running CreateProcessInSession(Sesion=%d):',[SessionID]));
    try
      Log.Add('Getting user token CreateWTSQueryUserTokenEx...');
      Output.UserToken := TJwSecurityToken.CreateWTSQueryUserTokenEx(nil, SessionID);
    except
      //on E2 : EJwsclUnsupportedWindowsVersionException do
      On E2 : Exception do
      begin
        try
          Log.Add('Getting user token CreateTokenByProcessAndSession...');
          Output.UserToken := CreateTokenByProcessAndSession(SessionId);
        except
          on E : Exception do
          begin
            Log.Add('Could not retrieve user token: '+#13#10+E.Message);
            raise;
          end;
        end;
      end;
    end;

    if not Assigned(Output.UserToken) then
    begin
      try
        raise EJwsclProcessIdNotAvailable.Create('There were no token found for specified session: '+IntToStr(SessionId));
      except
        on E : Exception do
        begin
          Log.Add(E.Message);
          raise;
        end;
      end;

      exit;
    end;


    try
      Log.Add('Loading user profile');
      // Load user's profile
      // We do not apply any in parameters, let the method do it automatically
      // may fail with an exception
      Output.UserToken.LoadUserProfile(Output.ProfileInfo, []);

      with StartupInfo do
      begin
        cb          := SizeOf(StartupInfo);
        //set default desktop if caller does not specify it
        if Length(Desktop) = 0 then
          lpDesktop   := 'WinSta0\Default'
        else
          lpDesktop   := TJwPChar(Desktop);
      end;


      Log.Add('Init strings');
      //get p(w)char pointer from string
      GetPChar(ApplicationName, lpApplicationName);
      GetPChar(CommandLine, lpCommandLine);
      GetPChar(CurrentDirectory, lpCurrentDirectory);


      Log.Add('Init env block');
      //Create environment block from user token
      //we do fail on that
      if not CreateEnvironmentBlock(@Output.EnvBlock, Output.UserToken.TokenHandle, false) then
        Log.Add('CreateEnvironmentBlock failed: '+IntToStr(GetLastError));

      Log.Add('Call CreateProcessAsUser');
      if not {$IFDEF UNICODE}CreateProcessAsUserW{$ELSE}CreateProcessAsUserA{$ENDIF}(
        Output.UserToken.TokenHandle,//HANDLE hToken,
        lpApplicationName,//__in_opt     LPCTSTR lpApplicationName,
        lpCommandLine, //__inout_opt  LPTSTR lpCommandLine,
        nil,//__in_opt     LPSECURITY_ATTRIBUTES lpProcessAttributes,
        nil,//__in_opt     LPSECURITY_ATTRIBUTES lpThreadAttributes,
        false,//__in         BOOL bInheritHandles,
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

  except
    //make sure log is filled with exception data
    on E : Exception do
    begin
      Log.Add(Format('Exiting CreateProcessInSession(Sesion=%d):',[SessionID]));

      LogInfo := LogInfo + #13#10 + Log.Text;

      if E is EJwsclSecurityException then
        (E as EJwsclSecurityException).Log := LogInfo;
      Log.Free;

      raise;
    end;
  end;

  finally

  end;
end;


{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}