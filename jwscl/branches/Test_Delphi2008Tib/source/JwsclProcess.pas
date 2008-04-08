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

uses Classes, jwaWindows,
  JwsclTypes, JwsclToken, JwsclSid, JwsclTerminalServer, JwsclUtils,
  JwsclLogging, JwsclLsa, JwsclDescriptor,JwsclEnumerations,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!
{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_IMPLEMENTATION_SECTION}

const
  IID_IJwJobObject = '{5F6DBA7A-B8DC-498E-A151-49AD0DCD8CF8}';
  IOJOBNAME = 'IOJobCompletion\';

type
  {@Name contains methods related to libraries.}
  TJwLibraryUtilities = class
  private
  protected
  public
    {@Name tries to get a pointer to a function within a DLL.
    @return(Return value is a function pointer to the specified function.
     If the library could not be found or the function is not located
     within the library the return value is nil. )
    }
    class function LoadLibProc(const LibName: AnsiString;
      const ProcName: AnsiString): Pointer;
  end;
{
  IJwJobObject = interface
   [IID_IJwJobObject]
     procedure AssignProcessToJobObject(hProcess : TJwProcessHandle);

BOOL WINAPI AssignProcessToJobObject(HANDLE hJob, HANDLE hProcess);
HANDLE WINAPI CreateJobObject(LPSECURITY_ATTRIBUTES lpJobAttributes,LPCTSTR lpName);
HANDLE WINAPI OpenJobObject(DWORD dwDesiredAccess,BOOL bInheritHandles, LPCTSTR lpName);
BOOL WINAPI SetInformationJobObject(HANDLE hJob,JOBOBJECTINFOCLASS JobObjectInfoClass,
        LPVOID lpJobObjectInfo,DWORD cbJobObjectInfoLength);
BOOL WINAPI QueryInformationJobObject(HANDLE hJob, JOBOBJECTINFOCLASS JobObjectInfoClass,
        LPVOID lpJobObjectInfo, DWORD cbJobObjectInfoLength, LPDWORD lpReturnLength);
BOOL WINAPI IsProcessInJob(HANDLE ProcessHandle, HANDLE JobHandle, PBOOL Result);
BOOL WINAPI TerminateJobObject(HANDLE hJob, UINT uExitCode);

  end;
 }
  TJwProcessList = array of TJwProcessId;

  TJwWaitState = (wsNonSignaled, wsSignaled, wsTimeOut);


  TJwJobObject = class;

  TJwOnJobNotification = procedure (Sender : TJwJobObject; Process : TJwProcessId;
    JobLimits : TJwJobMessages) {of object};

  TJwOnNoActiveProcesses = procedure (Sender : TJwJobObject) {of object};


  TJwInternalJobObjectIOCompletitionThread = Class(TJwThread)
  protected
    fJwJobObject : TJwJobObject;
    fIOHandle : THandle;
    fRemainPort : Boolean;
  public
    constructor Create(const Parent: TJwJobObject; const CreateSuspended: Boolean; const Name: AnsiString);
    constructor CreateWithIOPort(const Parent: TJwJobObject; IOPort : THandle; const CreateSuspended: Boolean; const Name: AnsiString);
    procedure Execute; override;

    procedure Terminate; reintroduce;
    property IOHandle : THandle read fIOHandle;
  end;

  TJwJobObject = class
  protected
    fHandle : THandle;
    fAccessMask : TJwAccessMask;

    fIOUniqueID : Integer;

    fJobName : TJwString;

    fTerminateOnDestroy : Boolean;

    fNotification : TJwOnJobNotification;
    fOnNoActiveProcesses : TJwOnNoActiveProcesses;

    fThread: TJwInternalJobObjectIOCompletitionThread;

    function GetJobObjectInformationLength(
       const JobObjectInfoClass : JOBOBJECTINFOCLASS) : Cardinal;

    procedure GetJobObjectInformation(
       const JobObjectInfoClass : JOBOBJECTINFOCLASS;
       out Data : Pointer);
  protected
    function GetProcesses : TJwProcessList;
    function GetProcessesCount : Cardinal;

    function GetBasicAndIOInformation : TJobObjectBasicAndIoAccountingInformation;
    function GetBasicLimitInformation : TJobObjectBasicLimitInformation;
    function GetBasicUIRestrictions : TJobObjectBasicUiRestrictions;
    procedure SetBasicUIRestrictions(Info : TJobObjectBasicUiRestrictions);

    function GetExtendedLimitInformation : TJobObjectExtendedLimitInformation;
    procedure SetExtendedLimitInformation(Info : TJobObjectExtendedLimitInformation);

    procedure SetBasicLimitInformation(Info : TJobObjectBasicLimitInformation);


    function GetAllotedCPUTimeSignalState : Boolean;

    function GetJobLimit : TJwJobLimits;
    procedure SetJobLimit(const Limit : TJwJobLimits);

    function GetJobUiLimit : TJwJobUiLimits;
    procedure SetJobUiLimit(const Limit : TJwJobUiLimits);

    function GetActiveProcessCount : Cardinal;

    procedure SetObjectAssociateCompletionPortInformation(
        const CompletionKey : Pointer; CompletionPort : THandle);

    //not supported by winapi
    procedure GetObjectAssociateCompletionPortInformation(
        out CompletionKey : Pointer; out CompletionPort : THandle);

    function GetIOHandle : THandle;
  public
    {@Param SecurityAttributes defines the security information for the job object.
      Use TJwSecurityDescriptor.InheritHandles to control handle inheritance.
    }
    constructor Create(const Name : TJwString;
      const ErrorIfAlreadyExists : Boolean;
      const SecurityAttributes : TJwSecurityDescriptor); overload;

    {@Name creates a new job object using an existing job object
    @param(Name defines the name of the existing job object)
    @param(DesiredAccess defines the desired access to open the job object)
    @param(InheritHandles defines whether processes created by this process
        will inherit the handle. Otherwise, the processes do not inherit this handle.)
    @param(CompletionKey defines an existing completion key to be assigned or
     used by the opened object. It is used for notifications.)
    @param(CompletionPort defines an existing completion handle to be assigned or
     used by the opened object. It is used for notifications.)

    }
    constructor Create(const Name : TJwString; const DesiredAccess : TJwAccessMask;
      const InheritHandles : Boolean; CompletionKey : Integer; CompletionPort : THandle); overload;

    destructor Destroy; override;

    {@Name returns whether a process is assigned to the job.
    @param(hProcess defines any handle to the process that is tested for membership.)
    @param(Returns tre if the process is a member of the job; otherwise false.)
    @raises(EJwsclWinCallFailedException can be raised if the call to an winapi function failed.)

    }
    function IsProcessInJob(hProcess : TJwProcessHandle) : Boolean;

    {
    @Name assigns an existing process to the job. The process must not already be
    assigned to a job. The process can be created with the flag CREATE_BREAKAWAY_FROM_JOB
    to be reassignable.
    @raises(EJwsclWinCallFailedException can be raised if the call to an winapi function failed.)
    }
    procedure AssignProcessToJobObject(hProcess : TJwProcessHandle);

    {@Name terminates all processes in the job with a predefined
     exit code.
    @raises(EJwsclWinCallFailedException can be raised if the call to an winapi function failed.)
    }
    procedure TerminateJobObject(const ExitCode : DWORD);

    {@Name resets the internal IO completition thread. This thread is necessary
    for triggering OnNotification and OnNoActiveProcesses.
      @param(Force defines whether the currenct IO thread should be forcibly terminated
       and restarted. If true the thread is shutdown and a new thread is created.
       However the IO port is not replaced. In fact there is no way to reassign a new
       port to the existing job object. If the parameter Force is set to false,
       the thread is only reset if it is not running.)
    @raises(EJwsclWinCallFailedException can be raised if the call to an winapi function failed.)
    }
    procedure ResetIOThread(const Force : Boolean);

    {
    @raises(EJwsclWinCallFailedException can be raised if the call to an winapi function failed.)
    }
    function WaitForAllotedCPUTimeSignal(const TimeOut : DWORD) : TJwWaitState;



    {@Name defines whether all processes should be terminated. If set to true
     and the @Classname instance is freed all processes assigned to this job
     are terminated. Default value is false. No process will be terminated.
    }
    property TerminateOnDestroy : Boolean read fTerminateOnDestroy write fTerminateOnDestroy;

    {@Name reads or sets User Interface limits of this job.
     This property has the same effect as property BasicUIRestrictions.

     May raise EJwsclWinCallFailedException if an error occurs.
    }
    property UiLimits : TJwJobUiLimits read GetJobUiLimit write SetJobUiLimit;

    {@Name receives the signal state of the job. It is set to true
    if all processes has allotted their CPU time restriction; otherwise it is false.
    }
    property AllotedCPUTimeSignalState : Boolean read GetAllotedCPUTimeSignalState;

    {@Name reads or sets basic limit information of this job.
     May raise EJwsclWinCallFailedException if an error occurs.
    }
    property BasicLimitInformation : TJobObjectBasicLimitInformation
      read GetBasicLimitInformation write SetBasicLimitInformation;

    {@Name reads or sets basic limit and IO information of this job.
     May raise EJwsclWinCallFailedException if an error occurs.
    }
    property BasicAndIOInformation : TJobObjectBasicAndIoAccountingInformation read GetBasicAndIOInformation;

    {@Name reads or sets basic limit and IO information of this job.
     This property has the same effect as property UiLimits.

     May raise EJwsclWinCallFailedException if an error occurs.
    }
    property BasicUiRestrictions : TJobObjectBasicUiRestrictions read GetBasicUIRestrictions write SetBasicUIRestrictions;

    {@Name reads or sets extended limit information of this job.
     May raise EJwsclWinCallFailedException if an error occurs.
    }
    property ExtendedLimitInformation : TJobObjectExtendedLimitInformation read GetExtendedLimitInformation write SetExtendedLimitInformation;

    {@Name returns the current active count of processes in this job.
    May raise EJwsclWinCallFailedException if an error occurs.
    }
    property ActiveProcessCount : Cardinal read GetActiveProcessCount;

    {@Name returns the handle of the job}
    property Handle : THandle read fHandle;

    {@Name returns the access mask of this job object as specified in Create
    when opening an existing job object}
    property AccessMask : TJwAccessMask read fAccessMask;

    {@Name returns an array of process ID values.
     Valud values are from low(Processes) to high(Processes).
     Always use a temporary variable for this property because
     each access to this property needs a system call.
     }
    property Processes : TJwProcessList read GetProcesses;

    {@Name returns a unique IO completion ID that was assigned to the IO
     completion port as key ID.
    }
    property IOUniqueID : Integer read fIOUniqueID;
    property IOHandle : THandle read GetIOHandle;

    property Name : TJwString read fJobName;

    property OnNotification : TJwOnJobNotification read fNotification write fNotification;
    property OnNoActiveProcesses : TJwOnNoActiveProcesses read fOnNoActiveProcesses write fOnNoActiveProcesses;
  end;

  {@Name contains information about result from JwCreateProcessInSession.}
  TJwProcessOutputInformation = record
    {@Name contains the token from the given Session ID.
     Free the token if it is no more necessary.   }
    UserToken  : TJwSecurityToken;
    {@Name receives information about the started process.  }
    ProcessInfo: TProcessInformation;
    {@Name receives an environment block used the new process.
     Call DestroyEnvironmentBlock to free its memory. }
    EnvBlock : Pointer;
    {@Name contains the users profile.
     You must call TJwSecurityToken.UnLoadUserProfile to unload this profile
     after the process has ended. }
    ProfileInfo : TJwProfileInfo;
  end;


{@Name creates a new process in a user's session using various ways to
achieve success.
This procedure needs JwInitWellKnownSIDs to be called.


@param(ApplicationName defines the application to be run in the session)
@param(CommandLine defines the parameters for the application)
@param(CurrentDirectory defines the start folder of the app. )
@param(SessionID defines the target session where the new application is to be started.)
@param(CreationFlags defines creation flags that are delivered to CreateProcess parameter with
 same name)
@param(Desktop defines the target windowstation and desktop name. If empty
the default target is "winsta0\default")
@param(StartupInfo defines startup info delivered to to CreateProcess parameter with
 same name)
@param(WaitForProcess defines whether the procedure should wait for the process to end
and clean up all allocated resources or just return to the caller. In last case
the caller is responsible to free the returned token, the environment block and
the users profile)
@param(Output contains returned data in case parameter WaitForProcess is false.
The caller is responsible to free the contained member allocation)
@param(LogServer receives a log server instance. It is used to log events for
mostly debugging purposes. If this parameter is nil, no events are logged)  

@raises(EJwsclProcessIdNotAvailable will be raised if no token could be found
for the given SessionID)
@raises(EJwsclNilPointer will be raised if JwInitWellKnownSIDs was not called before)
}
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
  LogServer : IJwLogServer
  );

{@Name tries to retrieve a token from a process.
For this purpose it enumerates all processes on the local machine (even from
other users) and calls the callback method OnProcessFound to determine
whether the given process should be used to return the token.
@param(OnProcessFound is a callback method that is called each time a process
was found. The callback function determines whether the process should be used
to return the token. If the process cannot be used to retrieve the token, @Name
will continue enumerating)
@param(LogServer receives a logging instance where log events are logged to.
 Can be nil if no logging is used)
@param(Data may contain user defined data to be assigned to a call to OnProcessFound)
@return(@Name returns the primary token of a process. If no process could be used
to get a token the return value is nil.)

@raises(EJwsclNILParameterException will be raised if parameter OnProcessFound is nil)
}
function JwGetTokenFromProcess (const OnProcessFound : TJwOnProcessFound;
  LogServer : IJwLogServer; Data : Pointer) : TJwSecurityToken;







type {@Name contains information supplied to CreateProcessAsUser}
     TJwCreateProcessParameters = record
        {@Name defines the application to be run with administrators group}
        lpApplicationName,
        {@Name defines the parameters for the application}
        lpCommandLine         :  TJwString;
        {@Name defines the new process security descriptor}
        ProcessAttributes,
        {@Name defines the new thread security descriptor}
        ThreadAttributes      : TJwSecurityDescriptor;

        {@Name defines the handle inheritance of the ProcessAttributes member}
        bProcInheritHandles,
        {@Name defines the handle inheritance of the ThreadAttributes member}
        bThreadInheritHandles,

        {@Name defines whether handles are inherited. Supplied to CreateProcess.}
        bInheritHandles       : Boolean;
        {@Name defines creation flags that are delivered to CreateProcess parameter with
        same name}
        dwCreationFlags       : DWORD;
        {@Name defines the start folder of the app.}
        lpCurrentDirectory    : TJwString;
      end;

     {Test} 
     TJwCreateProcessInfo = record
       {@Name defines startup info delivered to to CreateProcess parameter with
       same name}
       StartupInfo : {$IFDEF UNICODE}TStartupInfoW{$ELSE}TStartupInfoA{$ENDIF};

       AdditionalGroups : TJwSecurityIdList; //zusätzliche Groups fürs Token


       {@Name defines the source name which is stored in the token}
       SourceName : AnsiString;
       {@Name defines the initiator name of the token}
       OriginName : AnsiString;

       {@Name defines the target session ID of the new process
       if UseSessionID is true the new process will be spawn with this session ID.}
       SessionID  : Cardinal;
       {@Name defines whether the new process should get the sessionID.}
       UseSessionID : Boolean;

       {@Name If true the value StartupInfo.lpDesktop will be ignored and
        'WinSta0\Default' used instead}
       DefaultDesktop : Boolean;

       {Defines the logon process name for LSA connection}
       LogonProcessName : TJwString;

       //entweder LogonToken oder LogonSID oder keines!
       {@Name can contain the logon sid to be used for the new token.
        May be nil. In this case the member LogonSID is used.}
       LogonToken : TJwSecurityToken; //optionales logon Token für Tokengroups - LogonSid wird in diesem Token gesucht
       {@Name can be the logon sid to be used for the new token.
        May be nil. In this case (and LogonToken = nil) the logon sid of the token with the given SessionID
        is used.}
       LogonSID : TJwSecurityID; //optionales logon SID für Tokengroups - eigenes Token

       {@Name contains parameters for CreateProcessAsUser}
       Parameters : TJwCreateProcessParameters; //Parameter für CP

     end;

     {@Name contains output information after the process has started.
      Some of these information must be freed manually.}
     TJwCreateProcessOut = record
       {@Name contains process information of the new process}
       ProcessInfo: TProcessInformation;  //ProcessInfo von CP

       {@Name receives the newly created token from the user logon.
        This token may be restricted in Vista. (administrator groups is for deny only
        access check)

        Free the instance manually (even when an exception has occured.)
       }
       UserToken,             //Normales Token (in Vista ohne Admin)
       {@Name receives the twin token in Vista.
       This member is only valid if the user is member of administrator group.
       In this case the token has the administrator group enabled.
       In pre Vista OS versions this member is always nil.

       Free the instance manually (even when an exception has occured.)
       }
       LinkedToken : TJwSecurityToken; //elevated Token (in Vista), sonst nil

       {@Name flags whether a linked token is available (true) or not (false).}
       IsLinked : Boolean; //true, wenn Vista Elevation aktiv

       {@Name receives the profile information from LoadUserProfile.
       Call TJwSecurityToken.UnloadUserProfile to unload the profile when
       process finished.
       }
       ProfInfo: TJwProfileInfo; //LoadUserProfile output -> UnloadUserProfile

       {@Name receives the users environment block.
        Call DestroyEnvironmentBlock to free the space.}
       EnvironmentBlock: Pointer;  //Environmentblock ->DestroyEnvBlock

       //LSALogonUser Input
       {@Name receives the unique source ID generated by
        AllocateLocallyUniqueID}
       Source: TTokenSource; //Token source

       {@Name receives the LSA instance that was used to create the token.
        The instance must be freed if no exception occured.     }
       LSA : TJwSecurityLsa;  //LSA

       //Output von LsaLogonUser
       {@Name receives profile information about the logged on user.
       Call LsaFreeReturnBuffer to free it if no exception occured.
       }
       ProfBuffer: PMSV1_0_INTERACTIVE_PROFILE; //->   LsaFreeReturnBuffer(ProfBuffer);
       {@Name receives the memory size of ProfBuffer}
       ProfBufferLen: Cardinal;

       {@Name receives the LUID of the UserToken and LinkedToken.}
       TokenLuid: TLUID;  //LUID des neuen Tokens (UserToken + LinkedToken)

       {@Name receives information about quota limits}
       QuotaLimits: QUOTA_LIMITS;
       {@Name receives extended error information from LsaLogonUser}
       SubStatus: integer; //Fehler von LSALogonUser
     end;

{@Name logs on a user and creates a new process under its logon session.
The user will be a member of administrators group for this process but not
in the user database.
In Vista the linked token will be retrieved which has the administrator group enabled.
The user does not have to be an administrator at all!

@bold(Remarks:)
This procedure needs JwInitWellKnownSIDs to be called.
@Name can only run within a SYSTEM account and with TCB privilege available.

@bold(BETA: This function has not been tested thoroughly!) 

@param(LogServer receives a logging instance where log events are logged to.
 Can be nil if no logging is used)
@raises(EJwsclNilPointer will be raised if JwInitWellKnownSIDs was not called before)
@raises(EJwsclPrivilegeException will be raised if the TCB privilege is not available)
}

procedure JwCreateProcessAsAdminUser(
   const UserName, Domain, Password : TJwString;
   const InVars : TJwCreateProcessInfo;
   out OutVars : TJwCreateProcessOut;
   LogServer : IJwLogServer
   );




{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation
uses SysUtils, Dialogs, Math, JwsclExceptions,
  JwsclKnownSid, JwsclVersion,
  JwsclAcl, JwsclConstants;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

class function TJwLibraryUtilities.LoadLibProc(const LibName: AnsiString; const ProcName: AnsiString): Pointer;
var
  LibHandle: THandle;
begin
  LibHandle := LoadLibraryA(PAnsiChar(LibName));
  if LibHandle = 0 then
    Result := nil
  else
  begin
    try
      Result := GetProcAddress(LibHandle, PAnsiChar(ProcName));
    finally
      FreeLibrary(LibHandle); // Free Memory Allocated for the DLL
    end;
  end;
end;




procedure JwCreateProcessAsAdminUser(
   const UserName, Domain, Password : TJwString;
   const InVars : TJwCreateProcessInfo;
   out OutVars : TJwCreateProcessOut;
   LogServer : IJwLogServer);



var pLogonData : PMSV1_0_INTERACTIVE_LOGON;
    Authlen: Cardinal;
    Groups : TJwSecurityIDList;
    LogonSessionSID,
    SID : TJwSecurityID;
    SessionLogonToken : TJwSecurityToken;

    UserToken : TJwSecurityToken;
    StartupInfo : {$IFDEF UNICODE}TStartupInfoW{$ELSE}TStartupInfoA{$ENDIF};
    LastError : DWORD;
//
    fTokenHandle : Cardinal;

    CurrentDirectory,
    AppName, CmdLine : TJwPchar;

    Log : IJwLogClient;

    lpProcAttr, lpThreadAttr :  PSecurityAttributes;

    i : Integer;
begin
  JwRaiseOnNilMemoryBlock(JwLocalSystemSID,'JwCreateProcessAsAdminUser','','JwsclProcess.pas');

  //log to /dev/null
  if not Assigned(LogServer) then
    LogServer := CreateLogServer(nil, nil);

  Log := LogServer.Connect(etFunction, '', 'JwCreateProcessAsAdminUser', 'JwsclProcess.pas','');


  ZeroMemory(@OutVars, sizeof(OutVars));

  try //1.
    Log.Log(lsMessage, 'Try to enable TCB privilege...');
    JwEnablePrivilege(SE_TCB_NAME,pst_Enable);
    Log.Log(lsMessage, 'Success');


    Log.Log('Calling TJwSecurityLsa.Create');
    OutVars.LSA := TJwSecurityLsa.Create(InVars.LogonProcessName);
    Log.Log('Success');

    ZeroMemory(@OutVars.Source.SourceName, 0);
    StrLCopy(@OutVars.Source.SourceName,PAnsiChar(InVars.SourceName),
                Min(sizeof(OutVars.Source.SourceName), Length(InVars.SourceName)));
    AllocateLocallyUniqueID(OutVars.Source.SourceIdentifier);

    SessionLogonToken := nil;
    //
    // Init LSALogonUser parameters
    //
    pLogonData := JwCreate_MSV1_0_INTERACTIVE_LOGON(
                      MsV1_0InteractiveLogon, Domain, UserName, Password, Authlen);

    Groups := TJwSecurityIDList.Create(True);
    try //2.

      //
      // Add logon sid from caller if any.
      //
      if Assigned(InVars.LogonSID) then
      begin
        Log.Log('Adding user defined LogonSessionID to TokenGroups');
        SID := TJwSecurityId.Create(InVars.LogonSID);
        SID.Attributes := SE_GROUP_MANDATORY or
                          SE_GROUP_ENABLED or
                          SE_GROUP_ENABLED_BY_DEFAULT or
                          SE_GROUP_LOGON_ID;
        Groups.Add(SID); //"Groups" owns SID now
      end
      else
      // ...otherwise
      // Either use the callers token from InVars.LogonToken
      // or get the token from a specified SessionID. 
      //
      begin
        //
        // The user did not provide use with a token
        // so we must get the logon sid ourselves.
        //
        if not Assigned(InVars.LogonToken) then 
        try //3.
          Log.Log('Checking Windows Version...');

          //
          // Use a newer way to get the session token
          //
          if (TJwWindowsVersion.IsWindowsXP(true) or
             TJwWindowsVersion.IsWindows2003(true) or
             TJwWindowsVersion.IsWindows2003R2(true) or
             TJwWindowsVersion.IsWindowsVista(true) or
             TJwWindowsVersion.IsWindows2008(true)) then
          begin
            Log.Log('Getting user token from session: '+IntToStr(InVars.SessionID));

            {
            WARNING:
              TCB priv must be available and process must run under SYSTEM account!
            }
            try
              SessionLogonToken := TJwSecurityToken.CreateWTSQueryUserToken(InVars.SessionID);
            except
              {
              Do the old way: if we cannot get a connection
              and the session is should be the current session ID.
              This way the code can run in a none service app if the
               session id remains the current one.
              }
              on E : EJwsclWinCallFailedException do
                if (E.LastError = ERROR_ACCESS_DENIED)
                  and ((InVars.SessionID = INVALID_HANDLE_VALUE) or
                       (InVars.SessionID = WTSGetActiveConsoleSessionId))
                   then
                  SessionLogonToken := TJwSecurityToken.CreateCompatibilityQueryUserToken(TOKEN_READ or TOKEN_QUERY or TOKEN_DUPLICATE)
                else
                  raise;
            end;
          end
          else
          //
          // On Windows 2000 and earlier we use an old fashioned way
          //
          begin
            Log.Log('Getting user token from session which has explorer.exe: ');
            //TODO: use same mechanism as JwCreateProcessInSession to not rely on explorer.exe
            SessionLogonToken := TJwSecurityToken.CreateCompatibilityQueryUserToken(TOKEN_READ or TOKEN_QUERY or TOKEN_DUPLICATE);
          end;
        except //3.
          on E : Exception do
          begin
            SessionLogonToken := nil;
            if InVars.UseSessionID then
            begin
              Log.Exception(E);

              Log.Log(lsError,'Could not retrieve LogonSID ('+IntToStr(InVars.SessionID) + '): ');
              raise EJwsclNoSuchLogonSession.CreateFmt('Could not retrieve a LogonSID %d: ',[InVars.SessionID]);
            end
            else
              Log.Log('Failed to get LogonSID. Omitting step.');
          end;
        end; //3.
      end;

      //
      // The user did not specify a logon SID
      // but she provided a token OR
      //  we got our own token
      //
      if not Assigned(InVars.LogonSID) and
         (Assigned(SessionLogonToken) or Assigned(InVars.LogonToken)) then
      begin
        //
        // replace SessionLogonToken by the users one
        //
        if Assigned(InVars.LogonToken) then
        begin
          FreeAndNil(SessionLogonToken);
          SessionLogonToken := InVars.LogonToken;
        end;

        try
          //
          // Parse the logon sid from that token
          //
          Log.Log('Getting LogonSID...');
          try
            LogonSessionSID := JwGetLogonSID(SessionLogonToken);
          except
            on E : Exception do
            begin
              Log.Exception(E);
              LogonSessionSID := nil;
              Log.Log(lsError,'...call to JwGetLogonSID failed: ');
            end;
          end;

          //
          // add the logon sid from that token to the new token
          //
          if LogonSessionSID <> nil then
          begin
            Log.Log('Adding LogonSessionID to TokenGroups');
            SID := LogonSessionSID;
            SID.Attributes:= SE_GROUP_MANDATORY or
                             SE_GROUP_ENABLED or
                             SE_GROUP_ENABLED_BY_DEFAULT or
                             SE_GROUP_LOGON_ID;
            Groups.Add(SID); //"Groups" owns SID now
          end;
        finally
          if not Assigned(InVars.LogonToken) then
            FreeAndNil(SessionLogonToken);
        end;
      end;

      //
      // Add administrator group and enable it by default
      //
      Log.Log('Adding Administrator group to new token groups.');
      SID := TJwSecurityID.Create(JwAdministratorsSID);
      SID.Attributes:= SE_GROUP_MANDATORY or
                        SE_GROUP_ENABLED or
                        SE_GROUP_ENABLED_BY_DEFAULT;
      Groups.Add(SID); //"Groups" owns SID now

      //
      // add user groups to that token
      //
      if Assigned(InVars.AdditionalGroups) then
      begin
        for i := 0 to InVars.AdditionalGroups.Count -1 do
        begin
          Sid := TJwSecurityId.Create(InVars.AdditionalGroups[i]);

          //
          // we do not want a foreign logon ID to be in it 
          //
          if (sidaGroupLogonId in Sid.AttributesType) then
            Sid.AttributesType := Sid.AttributesType - [sidaGroupLogonId];

          Groups.Add(Sid);
        end;
      end;

      //
      // Logon user with special adapted token information
      //
      Log.Log('Calling LsaLogonUser...');
      try //4.
        OutVars.LSA.LsaLogonUser(InVars.OriginName, JwaWindows.Interactive, MSV1_0_PACKAGE_NAME,
                              pLogonData, Authlen, Groups, OutVars.Source,
                           {out...}
                              Pointer(OutVars.ProfBuffer), OutVars.ProfBufferLen,
                              OutVars.TokenLuid, OutVars.UserToken, OutVars.QuotaLimits, OutVars.SubStatus);
      except //4.
        on E: Exception do
        begin
          FreeAndNil(Groups);

          Log.Exception(E);
          Log.Log(lsError,'LsaLogonUser failed');
          raise;
        end;
      end; //4.

      Log.Log('...successfully.');
      FreeAndNil(Groups);

      //
      // On Vista or server 2008 LsaLogonUser creates a duplicate token
      // if it encounters the administrator group.
      // So we have to use the twin token for the new process
      // which we want to start with administrator group.
      //
      try
        OutVars.IsLinked := true;
        OutVars.LinkedToken := OutVars.UserToken.LinkedToken;
        UserToken := OutVars.LinkedToken; //token for CreateProcessAsUser
      except
        on E : Exception do
        begin
          Log.Exception(E);
          Log.Log(lsError,'Could not get Linked Token. Using token from LsaLogonUser...continue');

          OutVars.IsLinked := false;
          OutVars.LinkedToken := nil;
          UserToken := OutVars.UserToken;
        end;
      end;

      StartupInfo := InVars.StartupInfo;
      StartupInfo.cb := SizeOf(StartupInfo);

      if InVars.DefaultDesktop then
        StartupInfo.lpDesktop := 'WinSta0\Default';


      //
      // If the caller wants to set the tokens SessionID
      // we do it here.
      // The token needs the correct session ID to be set.
      //
      if InVars.UseSessionID then
      begin
        Log.Log('Setting TokenSession ID...');
        try
          UserToken.TokenSessionId := InVars.SessionID;//WtsGetActiveConsoleSessionID;
        except
          on E : Exception do
          begin
            Log.Exception(E);
            Log.Log(lsError,'...Failed to set TokenSessionId. ');
            raise;
          end;
        end;
        Log.Log('...successfully.');
      end;

      //
      // Load user profile
      //  may throw exception
      //
      Log.Log('Try to load user profile...');
      UserToken.LoadUserProfile(OutVars.ProfInfo, [{automatic config}]);
      Log.Log('Profile loaded');


      //
      // Create environment variables for the user
      //
      if not CreateEnvironmentBlock(@OutVars.EnvironmentBlock, UserToken.TokenHandle, true) then
        Log.Log(lsWarning,'Call to CreateEnvironmentBlock failed: '+EJwsclSecurityException.GetLastErrorMessage());

      try //6.
        AppName := nil;
        CmdLine := nil;
        CurrentDirectory := nil;
        lpProcAttr := nil;
        lpThreadAttr := nil;

        //
        // Copy variables to memory for CreateProcessAsUserX 
        //

        if Length(InVars.Parameters.lpApplicationName) > 0 then
          AppName := TJwPChar(InVars.Parameters.lpApplicationName);

        if Length(InVars.Parameters.lpCommandLine) > 0 then
          CmdLine := TJwPChar(InVars.Parameters.lpCommandLine);

        if Length(InVars.Parameters.lpCurrentDirectory) > 0 then
          CurrentDirectory := TJwPChar(InVars.Parameters.lpCurrentDirectory);

        if Assigned(InVars.Parameters.ProcessAttributes) then
          lpProcAttr := InVars.Parameters.ProcessAttributes.Create_SA(InVars.Parameters.bProcInheritHandles);

        if Assigned(InVars.Parameters.ThreadAttributes) then
          lpThreadAttr := InVars.Parameters.ThreadAttributes.Create_SA(InVars.Parameters.bThreadInheritHandles);

        //
        // Create new process
        //
        Log.Log('Calling CreateProcessAsUser...');
        SetLastError(0);
        if not {$IFDEF UNICODE}CreateProcessAsUserW{$ELSE}CreateProcessAsUserA{$ENDIF}(
              UserToken.TokenHandle,//HANDLE hToken,
              AppName,//__in_opt     LPCTSTR lpApplicationName,
              CmdLine, //__inout_opt  LPTSTR lpCommandLine,
              LPSECURITY_ATTRIBUTES(lpProcAttr),//__in_opt     LPSECURITY_ATTRIBUTES lpProcessAttributes,
              LPSECURITY_ATTRIBUTES(lpThreadAttr),//LPSECURITY_ATTRIBUTES(InVars.Parameters.lpThreadAttributes),//__in_opt     LPSECURITY_ATTRIBUTES lpThreadAttributes,
              InVars.Parameters.bInheritHandles,//__in         BOOL bInheritHandles,
              InVars.Parameters.dwCreationFlags,//__in         DWORD dwCreationFlags,
              OutVars.EnvironmentBlock,//__in_opt     LPVOID lpEnvironment,
              CurrentDirectory,//'',//TJwPChar(InVars.Parameters.lpCurrentDirectory),//__in_opt     LPCTSTR lpCurrentDirectory,
              StartupInfo,//__in         LPSTARTUPINFO lpStartupInfo,
              OutVars.ProcessInfo //__out        LPPROCESS_INFORMATION lpProcessInformation
          ) then
        begin
          Log.Log('Call to CreateProcessAsUser succeeded. Returning.');
        end
        else
        begin
          LastError := GetLastError();
          Log.Log(lsError,'CreateProcessAsUser failed: '+ EJwsclSecurityException.GetLastErrorMessage(LastError));

          raise EJwsclCreateProcessFailed.CreateFmtEx(
             'CreateProcessAsUser failed.',
             'CreateProcessAsAdminUser', '', '0',
             0, True, ['CreateProcessAsUser']);
        end;
      except //6.
        on E : Exception do
        begin
          Log.Log('Clean up after CreateProcessAsUser failure....');

          if lpProcAttr <> nil then
            TJwSecurityDescriptor.Free_SA(lpProcAttr);

          if lpThreadAttr <> nil then
            TJwSecurityDescriptor.Free_SA(lpThreadAttr);

          DestroyEnvironmentBlock(OutVars.EnvironmentBlock);
            OutVars.EnvironmentBlock := nil;

          OutVars.UserToken.UnloadUserProfile(OutVars.Profinfo);

          FreeAndNil(OutVars.LinkedToken);
          FreeAndNil(OutVars.UserToken);
          UserToken := nil;

          Log.Log('...sucessfully.');
          raise;
        end;
      end; //6.

    except //2.
      LocalFree(Cardinal(pLogonData));
      pLogonData := nil;

      LsaFreeReturnBuffer(OutVars.ProfBuffer);
      OutVars.ProfBuffer := nil;
      OutVars.ProfBufferLen := 0;

      raise;
    end; //2.

    //
    //vars that must be freed
    //
    if pLogonData <> nil then
      LocalFree(Cardinal(pLogonData));
  except //1.
    on E : Exception do
    begin
      Log.Exception(E);

      FreeAndNil(OutVars.LSA);

      raise;
    end;
  end; //1.
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



function JwGetTokenFromProcess (const OnProcessFound : TJwOnProcessFound; LogServer : IJwLogServer; Data : Pointer) : TJwSecurityToken;
var TSrv : TJwTerminalServer;
    i : Integer;
    ProcessID : DWORD;
    Succ : Boolean;
    Sid : TJwSecurityId;
    Cancel : Boolean;
    Process : TJwWTSProcess;
    Log : IJwLogClient;
begin
  JwRaiseOnNilParameter(@OnProcessFound,'OnProcessFound','JwGetTokenFromProcess','','JwsclProcess.pas');

  //log to /dev/null
  if not Assigned(LogServer) then
    LogServer := CreateLogServer(nil, nil);

  Log := LogServer.Connect(etFunction, '', 'JwGetTokenFromProcess123', 'JwsclProcess.pas','');

  result := nil;
  Succ := false;

  //try to enable debug privs if available - otherwise nothing
  JwEnablePrivilege(SE_DEBUG_NAME,pst_EnableIfAvail);

  Log.Log(lsMessage,Format('Running CreateTokenByProcessAndSession(SessionID: %d',[0]));

  TSrv := TJwTerminalServer.Create;
  try
    TSrv.Connect;

    ProcessID := 0;
    if TSrv.EnumerateProcesses then
    begin
      Log.Log(lsMessage, 'Proc count: ' + IntToStr(TSrv.Processes.Count));
      for i := 0 to TSrv.Processes.Count-1 do
      begin
        Log.Log(lsMessage, Format('Proc: %d, Name= %s SessionID: %d',[TSrv.Processes[i].ProcessId,
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
            Log.Log(lsMessage,'call CreateDuplicateExistingToken');
            result := TJwSecurityToken.CreateDuplicateExistingToken(TSrv.Processes[i].Token.TokenHandle,
                MAXIMUM_ALLOWED);
            {DEBUG: raise Exception.Create('');}
          except
            On E : Exception do
            begin
              Log.Log(lsWarning, 'CreateDuplicateExistingToken failed: '#13#10+E.Message);

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
                  Log.Exception(E);
                  //Log.Add('Could not get user token by Process: '#13#10+E.Message);
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
      Log.Log(lsMessage,'EnumerateProcesses failed.');

    if ProcessID = 0 then
      Log.Log(lsMessage,'Could not find any process ID.');         
  finally
    TSrv.Free;
//    Log.Log(lsMessage,'Exiting CreateTokenByProcessAndSession.');
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
  LogServer : IJwLogServer
  );


  procedure GetPChar(const Str : TJwString; var CharPtr : TJwPChar);
  begin
    if Length(Str) > 0 then
      CharPtr := TJwPChar(Str)
    else
      CharPtr := nil;
  end;

  function CreateTokenByProcessAndSession(
    const SessionID : DWORD) : TJwSecurityToken;
  var TSrv : TJwTerminalServer;
      i : Integer;
      Succ : Boolean;
      Sid : TJwSecurityId;
      Meth : TMethod;
      Data : TInternalProcessData;
      Log : IJwLogClient;
  begin
    Log := LogServer.Connect(etFunction, '', 'CreateTokenByProcessAndSession', 'JwsclProcess.pas','');
    result := nil;

    //try to enable debug privs if available - otherwise nothing
    JwEnablePrivilege(SE_DEBUG_NAME,pst_EnableIfAvail);

    Log.Log(lsMessage,Format('Running CreateTokenByProcessAndSession(SessionID: %d',[SessionID]));

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
      result := JwGetTokenFromProcess (TJwOnProcessFound(Meth), LogServer, @Data);
    finally
      TSrv.Free;
      Log.Log(lsMessage,'Exiting CreateTokenByProcessAndSession.');
    end;
  end;


var
    lpApplicationName  : TJwPChar;
    lpCommandLine      : TJwPChar;
    lpCurrentDirectory : TJwPChar;

    Log : IJwLogClient;
begin
  JwRaiseOnNilMemoryBlock(JwLocalSystemSID,'JwCreateProcessInSession','','JwsclProcess.pas');


  //log to /dev/null
  if not Assigned(LogServer) then
    LogServer := CreateLogServer(nil, nil);

  Log := LogServer.Connect(etFunction, '', 'JwCreateProcessInSession', 'JwsclProcess.pas','');



  Output.UserToken := nil;
  ZeroMemory(@Output.ProcessInfo, sizeof(Output.ProcessInfo));
  Output.EnvBlock := nil;
  ZeroMemory(@Output.ProfileInfo, sizeof(ProfileInfo));

  //First try WTS call to get token
  try
    Log.Log(lsMessage, Format('Running CreateProcessInSession(Sesion=%d):',[SessionID]));
    try
      Log.Log(lsMessage,'Getting user token CreateWTSQueryUserTokenEx...');
      Output.UserToken := TJwSecurityToken.CreateWTSQueryUserTokenEx(nil, SessionID);
    except
      On E2 : Exception do
      begin
        //as second chance we try to get the token from a process
        try
          Log.Log(lsMessage,'Getting user token CreateTokenByProcessAndSession...');
          Output.UserToken := CreateTokenByProcessAndSession(SessionId);
        except
          on E : Exception do
          begin
            Log.Exception(E); 

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
          //Log.Log(lsMessage,E.Message);
          Log.Exception(E);
          raise;
        end;
      end;

      exit;
    end;


    try
      Log.Log(lsMessage, 'Loading user profile');
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


      Log.Log(lsMessage,'Init strings');
      //get p(w)char pointer from string
      GetPChar(ApplicationName, lpApplicationName);
      GetPChar(CommandLine, lpCommandLine);
      GetPChar(CurrentDirectory, lpCurrentDirectory);


      Log.Log(lsMessage, 'Init env block');
      //Create environment block from user token
      //we do fail on that
      if not CreateEnvironmentBlock(@Output.EnvBlock, Output.UserToken.TokenHandle, false) then
        Log.Log(lsMessage, 'CreateEnvironmentBlock failed: '+IntToStr(GetLastError));

      Log.Log(lsMessage, 'Call CreateProcessAsUser');
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
        Log.Log(lsMessage,'Failed CreateProcessAsUser.');
        RaiseLastOSError;
      end;

      if WaitForProcess then
      begin
        Log.Signal(stWait,'','','Waiting for process to finish.');
        WaitForSingleObject(Output.ProcessInfo.hProcess, INFINITE);
        Log.Signal(stReceived,'','','Process finished.');

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

        Log.Log(lsMessage,'Exception (between LoadUserProfile and CreateProcessAsUser) : '#13#10+E.Message);
        raise;
      end;
    end;

  except
    //make sure log is filled with exception data
    on E : Exception do
    begin
     // LogInfo := LogInfo + #13#10 + Log.Text;

      {if E is EJwsclSecurityException then
        (E as EJwsclSecurityException).Log := LogInfo;
      Log.Free;}
      Log.Exception(E);

      raise;
    end;
  end;

end;

{

BOOL WINAPI AssignProcessToJobObject(HANDLE hJob, HANDLE hProcess);
HANDLE WINAPI CreateJobObject(LPSECURITY_ATTRIBUTES lpJobAttributes,LPCTSTR lpName);
HANDLE WINAPI OpenJobObject(DWORD dwDesiredAccess,BOOL bInheritHandles, LPCTSTR lpName);
BOOL WINAPI SetInformationJobObject(HANDLE hJob,JOBOBJECTINFOCLASS JobObjectInfoClass,
        LPVOID lpJobObjectInfo,DWORD cbJobObjectInfoLength);
BOOL WINAPI QueryInformationJobObject(HANDLE hJob, JOBOBJECTINFOCLASS JobObjectInfoClass,
        LPVOID lpJobObjectInfo, DWORD cbJobObjectInfoLength, LPDWORD lpReturnLength);
BOOL WINAPI IsProcessInJob(HANDLE ProcessHandle, HANDLE JobHandle, PBOOL Result);
BOOL WINAPI TerminateJobObject(HANDLE hJob, UINT uExitCode);
}

{TJwJobObject}
var CompletionUniqueID : Integer = 1;


constructor TJwJobObject.Create(const Name : TJwString;
  const ErrorIfAlreadyExists : Boolean;
  const SecurityAttributes : TJwSecurityDescriptor);

begin
  SetLastError(0);

  OnNotification := nil;
  OnNoActiveProcesses := nil;
  fIOUniqueID := InterlockedIncrement(CompletionUniqueID);

  fJobName := Name;

  if Length(Name) > 0 then
  begin
    fHandle := {$IFDEF UNICODE}CreateJobObjectW{$ELSE}CreateJobObjectA{$ENDIF}
      (nil, TJwPChar(Name));
    fThread := TJwInternalJobObjectIOCompletitionThread.Create(Self, true,IOJOBNAME+Name);
  end
  else
  begin
    fHandle := {$IFDEF UNICODE}CreateJobObjectW{$ELSE}CreateJobObjectA{$ENDIF}
      (nil, nil);
    fThread := TJwInternalJobObjectIOCompletitionThread.Create(Self, true,IOJOBNAME+IntToStr(GetCurrentThreadId));
  end;

  fAccessMask := JOB_OBJECT_ALL_ACCESS;
  fTerminateOnDestroy := false;


  if (fHandle = 0) or
   (ErrorIfAlreadyExists and (GetLastError() = ERROR_ALREADY_EXISTS))
  then
  begin
    FreeAndNil(fThread);
    if (GetLastError() <> 0) and (fHandle <> 0) then
      CloseHandle(fHandle);
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      '',
      'Create',                                //sSourceProc
      ClassName,                                //sSourceClass
      '',                          //sSourceFile
      0,                                           //iSourceLine
      True,                                  //bShowLastError
      'CreateJobObject',                   //sWinCall
      ['CreateJobObject']);                                  //const Args: array of const
  end;

  fThread.Resume;

  SetObjectAssociateCompletionPortInformation(Pointer(fIOUniqueID),
    fThread.IOHandle);
end;

constructor TJwJobObject.Create(const Name : TJwString; const DesiredAccess : TJwAccessMask;
  const InheritHandles : Boolean; CompletionKey : Integer; CompletionPort : THandle);
var
  IOPort : THandle;
begin
  fHandle := {$IFDEF UNICODE}OpenJobObjectW{$ELSE}OpenJobObjectA{$ENDIF}
    (DesiredAccess, InheritHandles, TJwPChar(Name));

  fJobName := Name;

  OnNotification := nil;
  OnNoActiveProcesses := nil;

  fAccessMask := DesiredAccess;
  fTerminateOnDestroy := false;


  if (fHandle = 0) then
  begin
    if fHandle <> 0 then
      CloseHandle(fHandle);
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      '',
      'Create',                                //sSourceProc
      ClassName,                                //sSourceClass
      '',                          //sSourceFile
      0,                                           //iSourceLine
      True,                                  //bShowLastError
      'OpenJobObject',                   //sWinCall
      ['OpenJobObject']);                                  //const Args: array of const
  end;

  fIOUniqueID := INVALID_HANDLE_VALUE;
  fThread := nil;

  {We cannot get CompletionKey and CompletionPort for an existing job object.
  So the user has to supply them
  }
  if (CompletionPort <> INVALID_HANDLE_VALUE) and
     (CompletionPort <> 0)
    and not DuplicateHandle(
     GetCurrentProcess,//__in        HANDLE hSourceProcessHandle,
      CompletionPort,//__in        HANDLE hSourceHandle,
     GetCurrentProcess,//__in        HANDLE hTargetProcessHandle,
      @IOPort,//__deref_out LPHANDLE lpTargetHandle,
      0,//__in        DWORD dwDesiredAccess,
      true,//__in        BOOL bInheritHandle,
      DUPLICATE_SAME_ACCESS//__in        DWORD dwOptions
    )
  then
  begin
    if fHandle <> 0 then
      CloseHandle(fHandle);
    RaiseLastOSError;
  end
  else
  if (CompletionPort <> INVALID_HANDLE_VALUE) and
     (CompletionPort <> 0) then
  begin
    fIOUniqueID := CompletionKey;
    fThread := TJwInternalJobObjectIOCompletitionThread.CreateWithIOPort(Self, IOPort, false, IOJOBNAME+Name);

    try
      SetObjectAssociateCompletionPortInformation(Pointer(fIOUniqueID),
        fThread.IOHandle);
    except
      //may fail, but doesn't matter
    end;
  end;
end;

destructor TJwJobObject.Destroy;
begin
  if TerminateOnDestroy then
    TerminateJobObject(0);

  if Assigned(fThread) then
  begin
    fThread.Terminate;
    fThread.WaitFor;
    FreeAndNil(fThread);
  end;

  if (fHandle <> 0) then
    CloseHandle(fHandle);
end;

function TJwJobObject.GetAllotedCPUTimeSignalState : Boolean;
begin
  result := WaitForSingleObject(fHandle,0) = WAIT_OBJECT_0;
end;

function TJwJobObject.WaitForAllotedCPUTimeSignal(const TimeOut : DWORD) : TJwWaitState;
begin
  case WaitForSingleObject(fHandle,TimeOut) of
    WAIT_OBJECT_0 : result := wsSignaled;
    WAIT_TIMEOUT  : result := wsTimeOut
  else
    result := wsNonSignaled;
  end;
end;


function TJwJobObject.GetProcesses : TJwProcessList;
var Data : Pointer;
    List : PJobObjectBasicProcessIdList;
  len, i,
  rlen,
  res : DWORD;
begin
  rlen := 0;
  len := 0;
  Data := nil;

  Len := GetJobObjectInformationLength(JobObjectBasicProcessIdList);

  GetMem(List, len);
  ZeroMemory(List, len);

  try
    if not QueryInformationJobObject(
      fHandle,//__in_opt   HANDLE hJob,
      JobObjectBasicProcessIdList,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
      List,//__out      LPVOID lpJobObjectInfo,
      len,//__in       DWORD cbJobObjectInfoLength,
      @rlen//__out_opt  LPDWORD lpReturnLength
      ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetJobObjectInformation',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const

    SetLength(result, List^.NumberOfProcessIdsInList);

    for i := low(result) to high(result) do
      result[i] :=   List^.ProcessIdList[i];

  finally
    FreeMem(List);
  end;
end;

function TJwJobObject.GetProcessesCount : Cardinal;
var Data : Pointer;
    List : PJobObjectBasicProcessIdList;
begin
  try
    result := GetBasicAndIOInformation.BasicInfo.ActiveProcesses;
  except
    on E : EJwsclWinCallFailedException do
    begin
      E.SourceProc := 'GetProcessesCount';
      raise E;
    end;
  end;
end;

function TJwJobObject.GetBasicAndIOInformation : TJobObjectBasicAndIoAccountingInformation;
var
  len : DWORD;
begin
  Len := sizeof(result); //return static structure

  try
    if not QueryInformationJobObject(
      fHandle,//__in_opt   HANDLE hJob,
      JobObjectBasicAndIoAccountingInformation,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
      @result,//__out      LPVOID lpJobObjectInfo,
      len,//__in       DWORD cbJobObjectInfoLength,
      nil//__out_opt  LPDWORD lpReturnLength
      ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetBasicInformation',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const
  finally
  end;
end;

function TJwJobObject.GetBasicLimitInformation : TJobObjectBasicLimitInformation;
var
  len : DWORD;
begin
  Len := sizeof(result); //return static structure

  try
    if not QueryInformationJobObject(
      fHandle,//__in_opt   HANDLE hJob,
      JobObjectBasicLimitInformation,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
      @result,//__out      LPVOID lpJobObjectInfo,
      len,//__in       DWORD cbJobObjectInfoLength,
      nil//__out_opt  LPDWORD lpReturnLength
      ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetBasicLimitInformation',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const
  finally
  end;
end;



function TJwJobObject.GetBasicUIRestrictions : TJobObjectBasicUiRestrictions;
var
  len : DWORD;
begin
  Len := sizeof(result); //return static structure

  try
    if not QueryInformationJobObject(
      fHandle,//__in_opt   HANDLE hJob,
      JobObjectBasicUIRestrictions,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
      @result,//__out      LPVOID lpJobObjectInfo,
      len,//__in       DWORD cbJobObjectInfoLength,
      nil//__out_opt  LPDWORD lpReturnLength
      ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetBasicUIRestrictions',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const
  finally
  end;
end;


procedure TJwJobObject.SetBasicLimitInformation(Info : TJobObjectBasicLimitInformation);
begin
  if not SetInformationJobObject(
      fHandle,//__in  HANDLE hJob,
      JobObjectBasicLimitInformation,//__in  JOBOBJECTINFOCLASS JobObjectInfoClass,
      @Info,//__in  LPVOID lpJobObjectInfo,
      sizeof(Info)//__in  DWORD cbJobObjectInfoLength
    ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetBasicUIRestrictions',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const
end;

procedure TJwJobObject.SetBasicUIRestrictions(Info : TJobObjectBasicUiRestrictions);
begin
  if not SetInformationJobObject(
      fHandle,//__in  HANDLE hJob,
      JobObjectBasicUIRestrictions,//__in  JOBOBJECTINFOCLASS JobObjectInfoClass,
      @Info,//__in  LPVOID lpJobObjectInfo,
      sizeof(Info)//__in  DWORD cbJobObjectInfoLength
    ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetBasicUIRestrictions',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const
end;

procedure TJwJobObject.GetObjectAssociateCompletionPortInformation(
        out CompletionKey : Pointer; out CompletionPort : THandle);
var
  len : DWORD;
  Data : TJobObjectAssociateCompletionPort;
begin
  Len := sizeof(Data); //return static structure

  try
    if not QueryInformationJobObject(
      fHandle,//__in_opt   HANDLE hJob,
      JobObjectAssociateCompletionPortInformation,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
      @Data,//__out      LPVOID lpJobObjectInfo,
      Len,//__in       DWORD cbJobObjectInfoLength,
      nil//__out_opt  LPDWORD lpReturnLength
      ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetBasicUIRestrictions',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const
  finally
  end;
  CompletionKey := Data.CompletionKey;
  CompletionPort := Data.CompletionPort;
end;

function TJwJobObject.GetIOHandle : THandle;
begin
  if Assigned(fThread) then
  begin
    result := fThread.fIOHandle;
  end
  else
    result := INVALID_HANDLE_VALUE;
end;

procedure TJwJobObject.SetObjectAssociateCompletionPortInformation(
    const CompletionKey : Pointer; CompletionPort : THandle);
var
  Data : TJobObjectAssociateCompletionPort;
  Len : Cardinal;
begin
  Len := sizeof(Data);
  Data.CompletionKey := CompletionKey;
  Data.CompletionPort := CompletionPort;

  if not SetInformationJobObject(
      fHandle,//__in  HANDLE hJob,
      JobObjectAssociateCompletionPortInformation,//__in  JOBOBJECTINFOCLASS JobObjectInfoClass,
      @Data,//__in  LPVOID lpJobObjectInfo,
      len//__in  DWORD cbJobObjectInfoLength
    ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'SetObjectAssociateCompletionPortInformation',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'SetInformationJobObject',                   //sWinCall
          ['SetInformationJobObject']);                                  //const Args: array of const
end;

function TJwJobObject.GetExtendedLimitInformation : TJobObjectExtendedLimitInformation;
var
  len : DWORD;
begin
  Len := sizeof(result); //return static structure

  ZeroMemory(@result, sizeof(result));

  try
    if not QueryInformationJobObject(
      fHandle,//__in_opt   HANDLE hJob,
      JobObjectExtendedLimitInformation,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
      @result,//__out      LPVOID lpJobObjectInfo,
      len,//__in       DWORD cbJobObjectInfoLength,
      nil//__out_opt  LPDWORD lpReturnLength
      ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'GetBasicUIRestrictions',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'QueryInformationJobObject',                   //sWinCall
          ['QueryInformationJobObject']);                                  //const Args: array of const
  finally
  end;
end;

procedure TJwJobObject.SetExtendedLimitInformation(Info : TJobObjectExtendedLimitInformation);
var i : Integer;
begin
  i := sizeof(Info);
  if not SetInformationJobObject(
      fHandle,//__in  HANDLE hJob,
      JobObjectExtendedLimitInformation,//__in  JOBOBJECTINFOCLASS JobObjectInfoClass,
      @Info,//__in  LPVOID lpJobObjectInfo,
      i//__in  DWORD cbJobObjectInfoLength
    ) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
          '',
          'SetExtendedLimitInformation',                                //sSourceProc
          ClassName,                                //sSourceClass
          '',                          //sSourceFile
          0,                                           //iSourceLine
          True,                                  //bShowLastError
          'SetInformationJobObject',                   //sWinCall
          ['SetInformationJobObject']);                                  //const Args: array of const
end;



function TJwJobObject.GetJobObjectInformationLength(
   const JobObjectInfoClass : JOBOBJECTINFOCLASS) : Cardinal;
var
  len,
  rlen,
  res : DWORD;
  Data : Pointer;
begin
  rlen := 0;
  len := 32;
  Data := nil;


  SetLastError(ERROR_MORE_DATA);

  while (GetLastError = ERROR_MORE_DATA) do
  begin
    GetMem(Data, len);
    try
      SetLastError(0);

      if not QueryInformationJobObject(
        fHandle,//__in_opt   HANDLE hJob,
        JobObjectInfoClass,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
        Data,//__out      LPVOID lpJobObjectInfo,
        len,//__in       DWORD cbJobObjectInfoLength,
        @rlen//__out_opt  LPDWORD lpReturnLength
        )
        and (GetLastError <> ERROR_MORE_DATA) then

       raise EJwsclWinCallFailedException.CreateFmtWinCall(
            '',
            'GetJobObjectInformationLength',                                //sSourceProc
            ClassName,                                //sSourceClass
            '',                          //sSourceFile
            0,                                           //iSourceLine
            True,                                  //bShowLastError
            'QueryInformationJobObject',                   //sWinCall
            ['QueryInformationJobObject'])
      else
      begin
        SetLastError(0);
        rlen := len;
      end;
    finally
      FreeMem(Data);
      if (GetLastError = ERROR_MORE_DATA) then
      begin
        Inc(Len, 32);
        SetLastError(ERROR_MORE_DATA);
      end;
    end;
  end;
  result := rlen;
end;

procedure TJwJobObject.GetJobObjectInformation(
   const JobObjectInfoClass : JOBOBJECTINFOCLASS;
   out Data : Pointer);
var
  len,
  rlen,
  res : DWORD;

begin
  rlen := 0;
  len := GetJobObjectInformationLength(JobObjectInfoClass);
  GetMem(Data, len);
  ZeroMemory(Data, len);

  if Data = nil then
    RaiseLastOSError;

  if not QueryInformationJobObject(
    fHandle,//__in_opt   HANDLE hJob,
    JobObjectInfoClass,//__in       JOBOBJECTINFOCLASS JobObjectInfoClass,
    Data,//__out      LPVOID lpJobObjectInfo,
    len,//__in       DWORD cbJobObjectInfoLength,
    @rlen//__out_opt  LPDWORD lpReturnLength
    ) then
   raise EJwsclWinCallFailedException.CreateFmtWinCall(
        '',
        'GetJobObjectInformation',                                //sSourceProc
        ClassName,                                //sSourceClass
        '',                          //sSourceFile
        0,                                           //iSourceLine
        True,                                  //bShowLastError
        'QueryInformationJobObject',                   //sWinCall
        ['QueryInformationJobObject']);                                  //const Args: array of const
end;

function TJwJobObject.IsProcessInJob(hProcess : TJwProcessHandle) : Boolean;
var LB : LongBool;
begin
  if not JwaWindows.IsProcessInJob(hProcess, fHandle, LB) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
        '',
        'IsProcessInJob',                                //sSourceProc
        ClassName,                                //sSourceClass
        '',                          //sSourceFile
        0,                                           //iSourceLine
        True,                                  //bShowLastError
        'IsProcessInJob',                   //sWinCall
        ['IsProcessInJob']);                                  //const Args: array of const
  result := LB;
end;

procedure TJwJobObject.AssignProcessToJobObject(hProcess : TJwProcessHandle);
begin
  if not JwaWindows.AssignProcessToJobObject(fHandle, hProcess) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
        '',
        'AssignProcessToJobObject',                                //sSourceProc
        ClassName,                                //sSourceClass
        '',                          //sSourceFile
        0,                                           //iSourceLine
        True,                                  //bShowLastError
        'AssignProcessToJobObject',                   //sWinCall
        ['AssignProcessToJobObject']);                                  //const Args: array of const
end;

procedure TJwJobObject.TerminateJobObject(const ExitCode : DWORD);
begin
  if not JwaWindows.TerminateJobObject(fHandle, ExitCode) then
     raise EJwsclWinCallFailedException.CreateFmtWinCall(
        '',
        'TerminateJobObject',                                //sSourceProc
        ClassName,                                //sSourceClass
        '',                          //sSourceFile
        0,                                           //iSourceLine
        True,                                  //bShowLastError
        'TerminateJobObject',                   //sWinCall
        ['TerminateJobObject']);                                  //const Args: array of const
end;

function TJwJobObject.GetJobLimit : TJwJobLimits;
var
  Info : TJobObjectBasicLimitInformation;
begin
  Info := GetBasicLimitInformation;
  Result := TJwEnumMap.ConvertJobLimit(Info.LimitFlags);
end;

procedure TJwJobObject.SetJobLimit(const Limit : TJwJobLimits);
var
  Info : TJobObjectBasicLimitInformation;
begin
  Info := GetBasicLimitInformation;
  Info.LimitFlags := TJwEnumMap.ConvertJobLimitType(Limit);
  SetBasicLimitInformation(Info);
end;

function TJwJobObject.GetJobUiLimit : TJwJobUiLimits;
var
  Info : TJobObjectBasicUiRestrictions;
begin
  Info := GetBasicUIRestrictions;
  Result := TJwEnumMap.ConvertJobUiLimit(Info.UIRestrictionsClass);
end;

procedure TJwJobObject.SetJobUiLimit(const Limit : TJwJobUiLimits);
var
  Info : TJobObjectBasicUiRestrictions;
begin
  Info := GetBasicUIRestrictions;
  Info.UIRestrictionsClass := TJwEnumMap.ConvertJobUiLimitType(Limit);
  SetBasicUIRestrictions(Info);
end;

function TJwJobObject.GetActiveProcessCount : Cardinal;
begin
  result := GetBasicAndIOInformation.BasicInfo.ActiveProcesses;
end;


procedure TJwJobObject.ResetIOThread(const Force : Boolean);
var
  IOPort : THandle;
begin
  if not Assigned(fThread) then
  begin
    fThread := TJwInternalJobObjectIOCompletitionThread.Create(Self, false,IOJOBNAME+Name);
  end
  else 
  begin
    if not Force and not fThread.Terminated then
      exit;

    fThread.fRemainPort := true;
    IOPort := fThread.IOHandle;

    fThread.Terminate;
    case WaitForSingleObject(fThread.Handle, 4000) of
      WAIT_TIMEOUT : TerminateThread(fThread.Handle, ERROR_TIMEOUT);
      WAIT_OBJECT_0 : ;
    end;

    try
      FreeAndNil(fThread);
    except
    end;
    
    fThread := TJwInternalJobObjectIOCompletitionThread.CreateWithIOPort(Self, IOPort,false,IOJOBNAME+Name);
  end;
end;


constructor TJwInternalJobObjectIOCompletitionThread.Create(
 const Parent: TJwJobObject;const CreateSuspended: Boolean; const Name: AnsiString);
begin
  fJwJobObject := Parent;
  fIOHandle := CreateIoCompletionPort(
                  INVALID_HANDLE_VALUE,//__in      HANDLE FileHandle,
                  0,//__in_opt  HANDLE ExistingCompletionPort,
                  Parent.IOUniqueID,//__in      ULONG_PTR CompletionKey,
                  0//__in      DWORD NumberOfConcurrentThreads
                );
  if fIOHandle = 0 then
    RaiseLastOSError;

  Self.FreeOnTerminate := false;
  fRemainPort := false;

  inherited Create(CreateSuspended,Name);
end;

constructor TJwInternalJobObjectIOCompletitionThread.CreateWithIOPort(
  const Parent: TJwJobObject; IOPort : THandle;
  const CreateSuspended: Boolean; const Name: AnsiString);
begin
  fJwJobObject := Parent;
  fIOHandle := IOPort;
  if fIOHandle = 0 then
  begin
    SetLastError(ERROR_INVALID_HANDLE);
    RaiseLastOSError;
  end;

  Self.FreeOnTerminate := false;
  fRemainPort := false;

  inherited Create(CreateSuspended,Name);
end;

procedure TJwInternalJobObjectIOCompletitionThread.Execute;
var
  pOV : POverlapped;
  lpNumberOfBytes,
  lpCompletionKey : Cardinal;
  Res : Boolean;
  L : DWORD;
const ERROR_ABANDONED_WAIT_0 = 735;

begin
  inherited; //sets thread name
  
  fRemainPort := false;
  pOV := nil;
  ReturnValue := 0;

  try
    repeat
      SetLastError(0);
      Res := GetQueuedCompletionStatus(
        fIOHandle,//__in   HANDLE CompletionPort,
        lpNumberOfBytes,//__out  LPDWORD lpNumberOfBytes,
        lpCompletionKey,//__out  PULONG_PTR lpCompletionKey,
        pOV,//__out  LPOVERLAPPED* lpOverlapped,
        INFINITE//__in   DWORD dwMilliseconds
      );

      L := GetLastError();
      if (not Res and (L <> 0)) or
         fRemainPort or Terminated then
      begin
        ReturnValue := L;

        inherited Terminate;
        break;
      end
      else
      if Assigned(fJwJobObject) and Res then
      begin
        if Assigned(fJwJobObject.OnNoActiveProcesses) and
          (fJwJobObject.GetActiveProcessCount = 0) then
        begin
          try
            fJwJobObject.OnNoActiveProcesses(fJwJobObject);
          except
          end;
        end;

        if Assigned(fJwJobObject.OnNotification) then
        begin
          try
            fJwJobObject.OnNotification(fJwJobObject, TJwProcessId(pOV),
              TJwEnumMap.ConvertJobMessage(lpNumberOfBytes));
          except
          end;
        end;
      end;
    until Terminated;
  finally
    if not fRemainPort then
    begin
      CloseHandle(fIOHandle);
      fIOHandle := INVALID_HANDLE_VALUE;
      fRemainPort := false;
    end;
  end;
end;


procedure TJwInternalJobObjectIOCompletitionThread.Terminate;
begin
  inherited;
  if not fRemainPort then
  begin
    //send dummy completion for GetQueuedCompletionStatus to return
    PostQueuedCompletionStatus(fIOHandle,0,0,nil);
  end;
end;



{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}