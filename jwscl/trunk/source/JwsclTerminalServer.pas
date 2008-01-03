{@abstract(This unit provides access to Terminal Server api functions)
@author(Remko Weijnen)
@created(10/26/2007)
@lastmod(10/26/2007)
This unit contains types that are used by the units of the Security Manager Suite


Project JEDI Windows Security Code Library (JWSCL)

The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

The Original Code is JwsclTerminalServer.pas.

The Initial Developer of the Original Code is Remko Weijnen.
Portions created by Remko Weijnen are Copyright (C) Remko Weijnen. All rights reserved.
}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclTerminalServer;
{$I Jwscl.inc}

interface

uses Classes, Contnrs, DateUtils, SysUtils,
{$IFDEF UNICODE}
  JclUnicode,
{$ENDIF UNICODE}
  JwaWindows,
  JwsclConstants, JwsclExceptions, JwsclResource, JwsclSid, JwsclTypes,
  JwsclVersion, JwsclStrings;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type

  { forward declarations }
  TJwTerminalServer = class;
  TJwTerminalServerList = class;
  TJwWTSEventThread = class;
  TJwWTSEnumServersThread = class;
  TJwWTSSession = class;
  TJwWTSSessionList = class;
  TJwWTSProcess = class;
  TJwWTSProcessList = class;

  PJwTerminalServer = ^TJwTerminalServer;
  TJwTerminalServer = class(TPersistent)
  private
    FComputerName: TJwString;
    FConnected: Boolean;
    FEnumServersThread: TJwWTSEnumServersThread;
    FIdleProcessName: TJwString;
    FLastEventFlag: DWORD;
    FOnServersEnumerated: TNotifyEvent;
    FOnSessionConnect: TNotifyEvent;
    FOnSessionCreate: TNotifyEvent;
    FOnSessionDelete: TNotifyEvent;
    FOnSessionDisconnect: TNotifyEvent;
    FOnSessionEvent: TNotifyEvent;
    FOnLicenseStateChange: TNotifyEvent;
    FOnSessionLogon: TNotifyEvent;
    FOnSessionLogoff: TNotifyEvent;
    FOnSessionStateChange: TNotifyEvent;
    FOnWinStationRename: TNotifyEvent;
    FServerHandle: THandle;
    FServerList: TStringList;
    FServers: {$IFDEF UNICODE}TWideStringList{$ELSE}TStringList{$ENDIF UNICODE};
    FSessions: TJwWTSSessionList;
    FProcesses: TJwWTSProcessList;
    FTerminalServerEventThread: TThread;
    FServer: TJwString;
    function GetIdleProcessName: TJwString;
    function GetServers: {$IFDEF UNICODE}TWideStringList{$ELSE}TStringList{$ENDIF UNICODE};
    function GetServer: TJwString;
    function GetWinStationName(const SessionId: DWORD): TJwString;
    procedure SetServer(const Value: TJwString);
  protected
    procedure FireEvent(EventFlag: DWORD);
  public
    procedure Connect;
    procedure Disconnect;
    property ComputerName: TJwString read FComputerName;
    property Connected: Boolean read FConnected;
    constructor Create;
    destructor Destroy; override;
    function GetAllProcesses: Boolean;
    function EnumerateProcesses: boolean;
    function EnumerateServers: boolean;
    function EnumerateSessions: boolean;
    function EnumerateServersEx: boolean;
    function FileTime2DateTime(FileTime: TFileTime): TDateTime;
    property IdleProcessName: TJwString read GetIdleProcessName;
    property LastEventFlag: DWORD read FLastEventFlag;
    property OnServersEnumerated: TNotifyEvent read FOnServersEnumerated write FOnServersEnumerated;
    property OnSessionEvent: TNotifyEvent read FOnSessionEvent write FOnSessionEvent;
    property OnSessionConnect: TNotifyEvent read FOnSessionConnect write FOnSessionConnect;
    property OnSessionCreate: TNotifyEvent read FOnSessionCreate write FOnSessionCreate;
    property OnSessionDelete: TNotifyEvent read FOnSessionDelete write FOnSessionDelete;
    property OnSessionDisconnect: TNotifyEvent read FOnSessionDisconnect write FOnSessionDisconnect;
    property OnLicenseStateChange: TNotifyEvent read FOnLicenseStateChange write FOnLicenseStateChange;
    property OnSessionLogon: TNotifyEvent read FOnSessionLogon write FOnSessionLogon;
    property OnSessionLogoff: TNotifyEvent read FOnSessionLogoff write FOnSessionLogoff;
    property OnWinStationRename: TNotifyEvent read FOnWinStationRename write FOnWinStationRename;
    property OnSessionStateChange: TNotifyEvent read FOnSessionStateChange write FOnSessionStateChange;
    property Processes: TJwWTSProcessList read FProcesses;
    property Server: TJwString read GetServer write SetServer;
    property ServerHandle: THandle read FServerHandle;
    property Servers: {$IFDEF UNICODE}TWideStringList{$ELSE}
      TStringList{$ENDIF UNICODE} read GetServers;
    property ServerList: TStringList read FServerList;
    property Sessions: TJwWTSSessionList read FSessions;
    function Shutdown(AShutdownFlag: DWORD): Boolean;
    function UnicodeStringToString(const AUnicodeString: UNICODE_STRING):
      TJwString;
  end;

  PJwTerminalServerList = ^TJwTerminalServerList;
  TJwTerminalServerList = class(TObjectList)
  private
    FOwnsObjects: Boolean;
    FOwner: TComponent;
  protected
    function GetItem(Index: Integer): TJwTerminalServer;
    procedure SetItem(Index: Integer; ATerminalServer: TJwTerminalServer);
    procedure SetOwner(const Value: TComponent);
  public
    destructor Destroy; reintroduce;
    function Add(ATerminalServer: TJwTerminalServer): Integer;
    function IndexOf(ATerminalServer: TJwTerminalServer): Integer;
    procedure Insert(Index: Integer; ATerminalServer: TJwTerminalServer);
    property Items[Index: Integer]: TJwTerminalServer read GetItem write SetItem; default;
    property Owner: TComponent read FOwner write SetOwner;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    function Remove(ATerminalServer: TJwTerminalServer): Integer;
  end;

  TJwWTSEventThread = class(TThread)
  private
    FOwner: TJwTerminalServer;
    FEventFlag: DWORD;
  protected
  public
    constructor Create(CreateSuspended: Boolean; AOwner: TJwTerminalServer);
    procedure DispatchEvent;
    procedure Execute; override;
  end;

  TJwWTSEnumServersThread = class(TThread)
  private
    FOwner: TJwTerminalServer;
    FServer: TJwString;
  protected
  public
    constructor Create(CreateSuspended: Boolean; AOwner: TJwTerminalServer);
    procedure AddToServerList;
    procedure ClearServerList;
    procedure DispatchEvent;
    procedure Execute; override;
    function WaitFor: LongWord; reintroduce;
  end;

  PJwWTSSession = ^TJwWTSSession;
  TJwWTSSession = class(TPersistent)
  private
  protected
    //TODO: I usually do not comment these things because
    //they are already commented in the property declaration
    FApplicationName: TJwString;
    FClientAddress: TJwString;
    FClientBuildNumber: DWORD;
    FColorDepth: DWORD;
    FClientDirectory: TJwString;
    FClientHardwareId: DWORD;
//    FClientInfo  // Vista only!
    FClientName: TJwString;
    FClientProductId: WORD;
    FClientProtocolType: WORD;
    FClientProtocolStr: TJwString;
//    FOEMId // Currently not used, so not implemented
    FCompressionRatio: TJwString;
    FConnectState: TWtsConnectStateClass;
    FConnectStateStr: TJwString;
    FConnectTime: TDateTime;
    FCurrentTime: TDateTime;
    FDisconnectTime: TDateTime;
    FDomain: TJwString;
    FIdleTime: TDateTime;
    FIdleTimeStr: TJwString;
    FIncomingBytes: DWORD;
    FIncomingCompressedBytes: DWORD;
    FIncomingFrames: DWORD;
    FHorizontalResolution: DWORD;
    FInitialProgram: TJwString;
    FLastInputTime: TDateTime;
    FLogonTime: TDateTime;
    FLogonTimeStr: TJwString;
    FOwner: TJwWTSSessionList;
    FOutgoingBytes: DWORD;
    FOutgoingCompressBytes: DWORD;
    FOutgoingFrames: DWORD;
    FProtocolTypeStr: TJwString;
    FRemoteAddress: TJwString;
    FRemotePort: WORD;
    FSessionId: TJwSessionId;
//    FState: TJwState;
    FUsername: TJwString;
    FVerticalResolution: DWORD;
    FWdFlag: DWORD;
    FWdName: TJwString;
    FWinStationName: TJwString;
    FWorkingDirectory: TJwString;


    //TODO: this func should be documented but not so detailed because of protected
    procedure GetClientDisplay;
    function GetSessionInfoDWORD(const WTSInfoClass: WTS_INFO_CLASS): DWORD;
    procedure GetSessionInfoPtr(const WTSInfoClass: WTS_INFO_CLASS;
      var ABuffer: Pointer);
    function GetSessionInfoStr(const WTSInfoClass: WTS_INFO_CLASS): TJwString;
    procedure GetWinStationInformation;
    procedure GetWinStationDriver;
  public
    {TODO: @Name create a new session here. <add here more information>

     Maybe some sample code to show problems or so ("#" is neccessary)
     @longcode(#
      var P : TJwWTSSession;
      begin
      end;
     #)

     Lists:
     @unorderedlist(
      @item(item1)
      @item(items2)
     )
     @orderedlist(
      @item(item1)
      @item(items2)
     )

     Find out more here: http://pasdoc.sipsolutions.net/SupportedTags

     <Properties do no have the following tags:>

     @param(AOwner receives the owner session list. It will be available
        through the property Owner. This parameter must not be nil.
        <Always adda precondition. like: must not be nil or zero.
        A precondition is tested in the function and a exception is raised.>
        )
     @param(ASessionId ...)
     <@return(Return value makes bla - only functions. Say something
      about the value semantic. Maybe : return of 0 means something special.
      Or the return value must be/must not be freed by the caller.
      )>

     @raises(EJwSecurityException <Show here the reason for that exception type.
        Also for every failed precondition >)
     @raises(EJwsclTerminalServerException < Create your own  >)
     @raises(EJwsclTerminalSessionException < dito>)
     @raises(EJwsclWinCallFailedException <winAPI call failed. Its always called that!>)
     @raises(EJwsclNILParameterException <a parameter is nil. Search for
      that exception in other classes for an example>)


  }
    constructor Create(const AOwner: TJwWTSSessionList;
      const ASessionId: TJwSessionId; const AWinStationName: TJwString;
      const AConnectState: TWtsConnectStateClass); reintroduce;
    property ApplicationName: TJwString read FApplicationName;
    property ClientAddress: TJwString read FClientAddress;
    property ClientBuildNumber: DWORD read FClientBuildNumber;
    property ClientDirectory: TJwString read FClientDirectory;
    property ClientHardwareId: DWORD read FClientHardwareId;
    property ClientName: TJwString read FClientName;
    property ClientProductId: WORD read FClientProductId;
    property ClientProtocolType: WORD read FClientProtocolType;
    property ClientProtocolStr: TJwString read FClientProtocolStr;
    property ColorDepth: DWORD read FColorDepth;
    property CompressionRatio: TJwString read FCompressionRatio;
    property ConnectState: TWtsConnectStateClass read FConnectState;
    property ConnectStateStr: TJwString read FConnectStateStr;
    property ConnectTime: TDateTime read FConnectTime;
    property CurrentTime: TDateTime read FCurrentTime;
    function Disconnect(bWait: Boolean): Boolean;
    property DisconnectTime: TDateTime read FDisconnectTime;
    property Domain: TJwString read FDomain;
    function GetClientAddress: TJwString;
    function GetServerHandle: THandle;
    function GetServerName: TJwString;
    property HorizontalResolution: DWORD read FHorizontalResolution;
    property IdleTimeStr: TJwString read FIdleTimeStr;
    property IncomingBytes: DWORD read FIncomingBytes;
    property InitialProgram: TJwString read FInitialProgram;
    property LastInputTime: TDateTime read FLastInputTime;
    function Logoff(bWait: Boolean): Boolean;
    property LogonTime: TDateTime read FLogonTime;
    property LogonTimeStr: TJwString read FLogonTimeStr;
    property Owner: TJwWTSSessionList read FOwner;
    property OutgoingBytes: DWORD read FOutgoingBytes;
    function PostMessage(const AMessage: TJwString; const ACaption: TJwString;
      const uType: DWORD): DWORD;
    function ProtocolTypeToStr(AProtocolType: DWORD): TJwString;
    property RemoteAddress: TJwString read FRemoteAddress;
    property RemotePort: WORD read FRemotePort;
    function SendMessage(const AMessage: TJwString; const ACaption: TJwString;
      const uType: DWORD; const ATimeOut: DWORD): DWORD;
    property SessionId: TJwSessionId read FSessionId;
    function Shadow: boolean;
    property Username: TJwString read FUsername;
    property VerticalResolution: DWORD read FVerticalResolution;
    property WdFlag: DWORD read FWdFlag;
    property WinStationDriverName: TJwString read FWdName;
    property WinStationName: TJwString read FWinStationName;
    property WorkingDirectory: TJwString read FWorkingDirectory;
  end;

  { List Of TJwWTSSession Objects }
  PJwWTSSessionList = ^TJwWTSSessionList;
  TJwWTSSessionList = class(TObjectList)
  private
    FOwnsObjects: Boolean;
    FOwner: TJwTerminalServer;
  protected
    function GetItem(Index: Integer): TJwWTSSession;
    procedure SetItem(Index: Integer; ASession: TJwWTSSession);
    procedure SetOwner(const Value: TJwTerminalServer);
  public
    destructor Destroy; reintroduce;
    function Add(ASession: TJwWTSSession): Integer;
    function IndexOf(ASession: TJwWTSSession): Integer;
    procedure Insert(Index: Integer; ASession: TJwWTSSession);
    property Items[Index: Integer]: TJwWTSSession read GetItem write SetItem; default;
    property Owner: TJwTerminalServer read FOwner write SetOwner;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    function Remove(ASession: TJwWTSSession): Integer;
  end;

  TJwWTSProcess = class(TPersistent)
  private
  protected
    FOwner: TJwWTSProcessList;
    FSessionId: TJwSessionID;
    FProcessId: TJwProcessID;
    FProcessName: TJwString;
    FUsername: TJwString;
    FWinStationName: TJwString;
    FProcessAge: TJwString;
    FProcessCreateTime: TJwString;
    FProcessCPUTime: TJwString;
    FProcessMemUsage: DWORD;
    FProcessVMSize: DWORD;
  public
    constructor Create(const AOwner: TJwWTSProcessList;
      const ASessionId: TJwSessionId; const AProcessID: TJwProcessId;
      const AProcessName: TJwString; const AUsername: TJwString); reintroduce;
    function GetServerHandle: THandle;
    property Owner: TJwWTSProcessList read FOwner;
    property SessionId: TJwSessionId read FSessionId;
    property ProcessAge: TJwString read FProcessAge;
    property ProcessId: TJwProcessId read FProcessId;
    property ProcessName: TJwString read FProcessName;
    function Terminate: boolean; overload;
    function Terminate(dwExitCode: DWORD): boolean; overload;
    property Username: TJwString read FUsername;
    property WinStationName: TJwString read FWinStationname;
    property ProcessCreateTime: TJwString read FProcessCreateTime;
    property ProcessCPUTime: TJwString read FProcessCPUTime;
    property ProcessMemUsage: DWORD read FProcessMemUsage;
    property ProcessVMSize: DWORD read FProcessVMSize;
  end;

  { List Of TJwWTSProcess Objects }
  TJwWTSProcessList = class(TObjectList)
  private
    FOwnsObjects: Boolean;
    FOwner: TJwTerminalServer;
  protected
    function GetItem(Index: Integer): TJwWTSProcess;
    procedure SetItem(Index: Integer; AProcess: TJwWTSProcess);
    procedure SetOwner(const Value: TJwTerminalServer);
  public
    function Add(AProcess: TJwWTSProcess): Integer;
    function IndexOf(AProcess: TJwWTSProcess): Integer;
    procedure Insert(Index: Integer; AProcess: TJwWTSProcess);
    property Items[Index: Integer]: TJwWTSProcess read GetItem write SetItem; default;
    property Owner: TJwTerminalServer read FOwner write SetOwner;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    function Remove(AProcess: TJwWTSProcess): Integer;
  end;

  { array of TWtsSessionInfoA }
  PJwWTSSessionInfoAArray = ^TJwWTSSessionInfoAArray;
  TJwWTSSessionInfoAArray = array[0..ANYSIZE_ARRAY-1] of TWtsSessionInfoA;

  { array of TWtsSessionInfoW }
  PJwWTSSessionInfoWArray = ^TJwWTSSessionInfoWArray;
  TJwWTSSessionInfoWArray = array[0..ANYSIZE_ARRAY-1] of TWtsSessionInfoW;

  { array of TWtsProcessInfoA }
  PJwWTSProcessInfoAArray = ^TJwWTSProcessInfoAArray;
  TJwWTSProcessInfoAArray = array[0..ANYSIZE_ARRAY-1] of TWtsProcessInfoA;

  { array of TWtsProcessInfoW }
  PJwWTSProcessInfoWArray = ^TJwWTSProcessInfoWArray;
  TJwWTSProcessInfoWArray = array[0..ANYSIZE_ARRAY-1] of TWtsProcessInfoW;

  { array of TWtsServerInfoA }
  PJwWtsServerInfoAArray = ^TJwWtsServerInfoAArray;
  TJwWtsServerInfoAArray = array[0..ANYSIZE_ARRAY-1] of TWtsServerInfoA;

  { array of TWtsServerInfoW }
  PJwWtsServerInfoWArray = ^TJwWtsServerInfoWArray;
  TJwWtsServerInfoWArray = array[0..ANYSIZE_ARRAY-1] of TWtsServerInfoW;
{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}

implementation
{$ENDIF SL_OMIT_SECTIONS}

function PWideCharToJwString(lpWideCharStr: PWideChar): TJwString;
{$IFNDEF UNICODE}
var cbMultiByte: Integer;
  lpMultiByteStr: PAnsiChar;
{$ENDIF UNICODE}
begin
  if lpWideCharStr = nil then
  begin
    Result := '';
  end
  else begin
{$IFDEF UNICODE}
    Result := lpWideCharStr;
{$ELSE}
    // WideCharToMultiByte returns needed size in bytes for lpMultiByteStr
    cbMultiByte := WideCharToMultiByte(CP_THREAD_ACP, WC_COMPOSITECHECK,
      lpWideCharStr, -1, nil, 0, nil, nil);

    // Reserve and Zero memory
    GetMem(lpMultiByteStr, cbMultiByte);
    ZeroMemory(lpMultiByteStr, cbMultiByte);

    // Now Convert PWideChar to PAnsiChar
    cbMultiByte := WideCharToMultiByte(CP_THREAD_ACP, WC_COMPOSITECHECK,
      lpWideCharStr, -1, lpMultiByteStr, cbMultiByte, nil, nil);

    // Return Result
    Result := lpMultiByteStr;

    // Cleanup
    FreeMem(lpMultiByteStr);
{$ENDIF}
  end;
end;

constructor TJwTerminalServer.Create;
begin
  inherited Create;
  FSessions := TJwWTSSessionList.Create(True);
  FSessions.Owner := Self;

  FProcesses := TJwWTSProcessList.Create(True);
  FProcesses.Owner := Self;

  FTerminalServerEventThread := nil;
  FServers := nil;

  FOnServersEnumerated := nil;
  FServerList := TStringList.Create;

end;

destructor TJwTerminalServer.Destroy;
var EventFlag: DWORD;
begin
  // Close connection
  if Assigned(FEnumServersThread) then
  begin
    OutputDebugString('Terminating thread');
    FEnumServersThread.Terminate;

    // Is the thread suspended?
    if FEnumServersThread.Suspended then
    begin
      // Resume the thread in order to terminate!
      OutputDebugString('Resuming the thread');
      FEnumServersThread.Resume;
    end;

    OutputDebugString('Waiting for thread termination');
    FEnumServersThread.WaitFor;
    OutputDebugString('Thread has terminated');
    FEnumServersThread.Free;
  end;

  if Connected then
  begin
    Disconnect;
  end;

  // Free the SessionList
  if Assigned(FSessions) then
  begin
    FreeAndNil(FSessions);
  end;

    // Free the ProcessList
  if Assigned(FProcesses) then
  begin
    FreeAndNil(FProcesses);
  end;

  // Free the Serverlist
  if Assigned(FServers) then
  begin
    FreeAndNil(FServers);
  end;

  // Free the Serverlist
  if Assigned(FServerList) then
  begin
    FreeAndNil(FServerList);
  end;
  
  inherited;
end;


{
The following table lists the events that trigger the different flags.
Events are listed across the top and the flags are listed down the
left column. An “X” indicates that the event triggers the flag.
+-------------+------+------+------+-------+----------+-----+------+-------+
| EventFlag   |Create|Delete|Rename|Connect|Disconnect|Logon|Logoff|License|
+-------------+------+------+------+-------+----------+-----+------+-------+
| Create      | X    |      |      | X     |          |     |      |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| Delete      |      | X    |      |       |          |     | X    |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| Rename      |      |      | X    |       |          |     |      |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| Connect     |      |      |      | X     |          |     |      |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| Disconnect  |      |      |      |       | X        |     |      |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| Logon       |      |      |      |       |          | X   |      |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| Logoff      |      |      |      |       |          |     | X    |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| License     |      |      |      |       |          |     |      | X     |
+-------------+------+------+------+-------+----------+-----+------+-------+
| StateChange | X    | X    |      | X     | X        | X   | X    |       |
+-------------+------+------+------+-------+----------+-----+------+-------+
| All         | X    | X    | X    | X     | X        | X   | X    | X     |
+-------------+------+------+------+-------+----------+-----+------+-------+

An WinStation is created when a user connects. When a user logs off, the
Winstation is deleted. When a user logs on to a disconnected session, the
existing session is deleted and the Delete flag is triggered. When users
connect to a disconnected session from within a session, their session is
disconnected and the Disconnect flag is triggered instead of the Delete flag.}

procedure TJwTerminalServer.FireEvent(EventFlag: Cardinal);
begin
  // Set LastEventFlag property
  FLastEventFlag := EventFlag;

  // The OnSessionEvent should be fired if anything happens that is session
  // related, like statechange, logon/logoff, disconnect and (re)connect.
  if (EventFlag > WTS_EVENT_CONNECT) and (EventFlag < WTS_EVENT_LICENSE) then
  begin
    if Assigned(FOnSessionEvent) then
    begin
      OnSessionEvent(Self);
    end;
  end;

  if (EventFlag and WTS_EVENT_LICENSE = WTS_EVENT_LICENSE) and
    Assigned(OnLicenseStateChange) then
  begin
    OnLicenseStateChange(Self);
  end;
  if (EventFlag and WTS_EVENT_STATECHANGE = WTS_EVENT_STATECHANGE) and
    Assigned(FOnSessionStateChange) then
  begin
    OnSessionStateChange(Self);
  end;
  if (EventFlag and WTS_EVENT_LOGOFF = WTS_EVENT_LOGOFF) and
    Assigned(FOnSessionLogoff) then
  begin
    OnSessionLogoff(Self);
  end;
  if (EventFlag and WTS_EVENT_LOGON = WTS_EVENT_LOGON) and
    Assigned(FOnSessionLogon) then
  begin
    OnSessionLogon(Self);
  end;
  if (EventFlag and WTS_EVENT_DISCONNECT = WTS_EVENT_DISCONNECT) and
    Assigned(FOnSessionDisconnect) then
  begin
    OnSessionDisconnect(Self);
  end;
  if (EventFlag and WTS_EVENT_CONNECT = WTS_EVENT_CONNECT) and
    Assigned(FOnSessionConnect) then
  begin
    OnSessionConnect(Self);
  end;
  if (EventFlag and WTS_EVENT_RENAME = WTS_EVENT_RENAME) and
    Assigned(FOnWinStationRename) then
  begin
    OnWinStationRename(Self);
  end;
  if (EventFlag and WTS_EVENT_DELETE = WTS_EVENT_DELETE) and
    Assigned(FOnSessionDelete) then
  begin
    OnSessionDelete(Self);
  end;
  if (EventFlag and WTS_EVENT_CREATE = WTS_EVENT_CREATE) and
    Assigned(FOnSessionCreate) then
  begin
    OnSessionCreate(Self);
  end;

end;

function TJwTerminalServer.GetServer: TJwString;
var nSize: DWORD;
  pComputerName: {$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF};
begin
  // If no server was specified we return the local computername
  // (we cache this in FComputerName)
  if FServer = '' then
  begin
    if FComputerName = '' then
    begin
      nSize := MAX_COMPUTERNAME_LENGTH + 1;
      GetMem(pComputerName, nSize * TJwCharSize);
{$IFDEF UNICODE}
      GetComputerNameW(pComputerName, nSize);
{$ELSE}
      GetComputerNameA(pComputerName, nSize);
{$ENDIF}
      FComputerName := pComputerName;
      FreeMem(pComputerName);
    end;
    Result := FComputerName;
  end
  else
  begin
    Result := FServer;
  end;
end;

function TJwTerminalServer.GetWinStationName(const SessionId: DWORD): TJwString;
var WinStationNamePtr: PWideChar;
begin
  GetMem(WinStationNamePtr, WINSTATIONNAME_LENGTH * SizeOf(WideChar));
  try
    ZeroMemory(WinStationNamePtr, WINSTATIONNAME_LENGTH * SizeOf(WideChar));

    if WinStationNameFromLogonIdW(FServerHandle, SessionId,
      WinStationNamePtr) then
    begin
      Result := PWideCharToJwString(WinStationNamePtr);
    end;

    // Return disconnected if WinStationName = empty
    if Result = '' then
    begin
      Result := PWideCharToJwString(
        StrConnectState(WTSDisconnected, False));
    end;
  finally
    // FreeMem
    FreeMem(WinStationNamePtr);
  end;
end;

procedure TJwTerminalServer.SetServer(const Value: TJwString);
begin
  FServer := Value;
  // Clear the computername variable (cache)
  FComputerName := '';
end;

function TJwTerminalServer.UnicodeStringToString(
  const AUnicodeString: UNICODE_STRING): TJwString;
var s: TJwString;
begin
  s := PWideCharToJwString(AUniCodeString.Buffer);
  // UNICODE_STRING specifies size in bytes instead number of WCHAR's
  // so we cut the string to right size
  SetLength(s, AUniCodeString.Length DIV SizeOf(WCHAR));
  Result := s;
end;

function TJwTerminalServer.GetAllProcesses: Boolean;
var
  Count: Integer;
  ProcessInfoPtr: PWINSTA_PROCESS_INFO_ARRAY;
  i: Integer;
  AProcess: TJwWTSProcess;
  strProcessName: TJwString;
  strUsername: TJwString;
  lpBuffer: PWideChar;
  DiffTime: TDiffTime;
begin
  //TODO: FServerHandle is valid?
  ProcessInfoPtr := nil;
  Count := 1;

  FProcesses.Clear;
  if not Connected then
  begin
    Connect;
  end;

  Result := WinStationGetAllProcesses(FServerHandle, 0, Count, ProcessInfoPtr);
  if Result then
  begin
    for i := 0 to Count-1 do
    begin
      with ProcessInfoPtr^[i], ExtendedInfo^ do
      begin
        // System Idle Process
        if ProcessId = 0 then
        begin
          strProcessName := GetIdleProcessName;
          strUserName := 'SYSTEM';
        end
        else
        begin
          strProcessName := UnicodeStringToString(ProcessName);

          if IsValidSid(pUserSid) then
          begin
            with TJwSecurityID.Create(pUserSid) do
            begin
              strUsername := GetChachedUserFromSid;
              Free;
            end;
          end;
        end;

        AProcess := TJwWTSProcess.Create(FProcesses, SessionId,
          ProcessId, strProcessName, strUsername);
        with AProcess do
        begin
          FProcesses.Add(AProcess);
          // Calculate Process Age
          CalculateElapsedTime(@CreateTime, DiffTime);

          try
            // Reserve Memory
            GetMem(lpBuffer, ELAPSED_TIME_STRING_LENGTH * SizeOf(WCHAR));
            // Format Elapsed Time String
            ElapsedTimeStringSafe(@DiffTime, False, lpBuffer,
                ELAPSED_TIME_STRING_LENGTH);
            FProcessAge := PWideCharToJwString(lpBuffer);
          finally
            // Free mem
            FreeMem(lpBuffer);
            lpBuffer := nil;
          end;

          // Some of the used counters are explained here:
          // http://msdn2.microsoft.com/en-us/library/aa394372.aspx
          
          FProcessCreateTime :=
            TimeToStr(FileTime2DateTime(FILETIME(CreateTime)));
          // The CPU Time column in Taskmgr.exe is Usertime + Kerneltime
          // So we take the sum of it and call it ProcessCPUTime
          FProcessCPUTime := CPUTime2Str(
            LARGE_INTEGER(UserTime.QuadPart + KernelTime.QuadPart));
          // Amount of memory in bytes that a process needs to execute
          // efficiently. Maps to Mem Size column in Task Manager.
          // So we call it ProcessMemUsage
          FProcessMemUsage := VmCounters.WorkingSetSize;
          // Pagefileusage is the amount of page file space that a process is
          // using currently. This value is consistent with the VMSize value
          // in TaskMgr.exe. So we call it ProcessVMSize
          FProcessVMSize := VmCounters.PagefileUsage;
        end;
      end;
    end;
  end;
  // Cleanup
  WinStationFreeGAPMemory(0, ProcessInfoPtr, Count);
  ProcessInfoPtr := nil;
end;

// todo: This function will removed as it will be replaced by
// WinStationGetAllProcesses
function TJwTerminalServer.EnumerateProcesses;
var Res: Bool;
  pCount: Cardinal;
  Username: TJwString;
  ProcessInfoPtr:
{$IFDEF UNICODE}
  PJwWtsProcessInfoWArray;
{$ELSE}
  PJwWtsProcessInfoAArray;
{$ENDIF UNICODE}
  i: Integer;
  AProcess: TJwWTSProcess;
begin
  FProcesses.Clear;

  if not Connected then
  begin
    Connect;
  end;

  Res :=
{$IFDEF UNICODE}

  WTSEnumerateProcessesW(FServerHandle, 0, 1, PWTS_PROCESS_INFOW(ProcessInfoPtr),
    pCount);
{$ELSE}
  WTSEnumerateProcessesA(FServerHandle, 0, 1, PWTS_PROCESS_INFOA(ProcessInfoPtr),
    pCount);
{$ENDIF UNICODE}

  if Res then
  begin
    for i := 0 to pCount - 1 do
    begin
      Username := '';
      if ProcessInfoPtr^[i].ProcessId = 0 then
      begin
        //TODO: use resource string
        ProcessInfoPtr^[i].pProcessName := 'System Idle Process';
        Username := 'SYSTEM';
      end;
      if ProcessInfoPtr^[i].pUserSid <> nil then
      begin
        with TJwSecurityID.Create(ProcessInfoPtr^[i].pUserSid) do
        begin
          Username := GetChachedUserFromSid;
          Free;
        end;
      end;
      AProcess := TJwWTSProcess.Create(FProcesses, ProcessInfoPtr^[i].SessionId,
        ProcessInfoPtr^[i].ProcessId, ProcessInfoPtr^[i].pProcessName,
        Username);
      FProcesses.Add(AProcess);
    end;
    WTSFreeMemory(ProcessInfoPtr);
  end;
  Result := Res;
end;

function TJwTerminalServer.GetServers: {$IFDEF UNICODE}TWideStringList{$ELSE}TStringList{$ENDIF UNICODE};
begin
  // Create the list
  if not Assigned(FServers) then
  begin
{$IFDEF UNICODE}
    FServers := TWideStringList.Create;
{$ELSE}
    FServers := TStringList.Create;
{$ENDIF UNICODE}
    // The list was empty so fill it!
    EnumerateServers;
  end;

  // Return the serverlist
  //TODO: Warning: User can free returned list (on purpose) this can
  //lead to problems in EnumerateServers
  Result := FServers;
end;

function TJwTerminalServer.EnumerateServers: Boolean;
var Res: Bool;
  ServerInfoPtr:
{$IFDEF UNICODE}
      PJwWtsServerInfoWArray;
{$ELSE}
      PJwWtsServerInfoAArray;
{$ENDIF UNICODE}
  pCount: DWORD;
  i: DWORD;
begin
  //TODO: warning: FServers can be nil or undefined!!

  // Clear the Serverlist
  FServers.Clear;
  Res :=
{$IFDEF UNICODE}
  WTSEnumerateServersW(nil, 0, 1, PWTS_SERVER_INFOW(ServerInfoPtr), pCount);
{$ELSE}
  WTSEnumerateServersA(nil, 0, 1, PWTS_SERVER_INFOA(ServerInfoPtr), pCount);
{$ENDIF UNICODE}

  //TODO: SessionInfoPtr = nil ???
  if Res then
  begin
    for i := 0 to pCount - 1 do
    begin
      FServers.Add(ServerInfoPtr^[i].pServerName);
    end;
  end
  else
  begin
    //TODO: replace by exception
    MessageBox(0, TJwPChar(SysErrorMessage(hResult(GetLastError))), 'WTSEnumerateServers', MB_OK);
  end;

  if ServerInfoPtr <> nil then
  begin
    WTSFreeMemory(ServerInfoPtr);
  end;
  Result := Res;
end;

function TJwTerminalServer.EnumerateSessions: boolean;
var SessionInfoPtr: {$IFDEF UNICODE}PJwWTSSessionInfoWArray;
  {$ELSE}PJwWTSSessionInfoAArray;{$ENDIF UNICODE}
  pCount: Cardinal;
  i: integer;
  Res: Longbool;
  ASession: TJwWTSSession;
begin
  if not Connected then
  begin
    Connect;
  end;

  Res :=
{$IFDEF UNICODE}
    WTSEnumerateSessionsW(FServerHandle, 0, 1, PWTS_SESSION_INFOW(SessionInfoPtr),
      pCount);
{$ELSE}
    WTSEnumerateSessions(FServerHandle, 0, 1, PWTS_SESSION_INFOA(SessionInfoPtr),
      pCount);
{$ENDIF UNICODE}

  // Clear the sessionslist
  FSessions.Clear;

  // Add all sessions to the SessionList
  for i := 0 to pCount - 1 do
  begin
    ASession := TJwWTSSession.Create(FSessions, SessionInfoPtr^[i].SessionId,
//      SessionInfoPtr^[i].pWinStationName, TWtsConnectStateClass(SessionInfoPtr^[i].State));
      GetWinStationName(SessionInfoPtr^[i].SessionId),
      TWtsConnectStateClass(SessionInfoPtr^[i].State));
    FSessions.Add(ASession);
  end;

  WTSFreeMemory(SessionInfoPtr);
  SessionInfoPtr := nil;
  
  // Pass the result
  Result := Res;
end;

function TJwTerminalServer.EnumerateServersEx: Boolean;
begin
  // Does the thread exist?
  if Assigned(FEnumServersThread) then
  begin
    // Is the thread suspended?
    if FEnumServersThread.Suspended then
    begin
      OutputDebugString('Resuming thread');
      // Resume (if not terminated, this will enumerate again)
      FEnumServersThread.Resume;
    end
    else begin
      OutputDebugString('thread is already running');
    end;
  end
  else begin
    // Create the thread
    OutputDebugString('create 1st thread');
    FEnumServersThread := TJwWTSEnumServersThread.Create(True, Self);
  end;
end;

procedure TJwTerminalServer.Connect;
begin
  if not FConnected then
  begin
    if FServer = '' then
    begin
      FServerHandle := WTS_CURRENT_SERVER_HANDLE;
      FConnected := True;
    end
    else
    begin
      FServerHandle :=
{$IFDEF UNICODE}
      WTSOpenServerW(PWideChar(WideString(FServer)));
{$ELSE}
      WTSOpenServerA(PChar(FServer));
{$ENDIF}
      // If WTSOpenServer fails the return value is 0
      if FServerHandle = 0 then
      begin
        raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
          'Connect', ClassName, RsUNTerminalServer, 0, True,
          'WTSOpenServer', ['FServer']);
      end
      else begin
        FConnected := True;
      end;
    end;

    if (FConnected) and (FTerminalServerEventThread = nil) then
    begin
      FTerminalServerEventThread := TJwWTSEventThread.Create(False, Self);
//      MessageBox(0, 'connected and threaded', 'debug', MB_OK);
    end;
  end;
end;

procedure TJwTerminalServer.Disconnect;
var EventFlag: DWORD;
begin
  // Terminate the Event Thread before closing the connection.
  if Assigned(FTerminalServerEventThread) then
  begin
    // Terminate Event Thread
    FTerminalServerEventThread.Terminate;

    // unblock the waiter
    WTSWaitSystemEvent(FServerHandle, WTS_EVENT_FLUSH, EventFlag);
    FreeAndNil(FTerminalServerEventThread);
  end;

  if FServerHandle <> WTS_CURRENT_SERVER_HANDLE then
  begin
    WTSCloseServer(FServerHandle);
  end;

  FServerHandle := INVALID_HANDLE_VALUE;
  FConnected := False;

end;

function TJwTerminalServer.FileTime2DateTime(FileTime: _FILETIME): TDateTime;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  FileTimeToLocalFileTime(FileTime, LocalFileTime);
  FileTimeToSystemTime(LocalFileTime, SystemTime);
  Result := SystemTimeToDateTime(SystemTime);
end;

function TJwTerminalServer.Shutdown(AShutdownFlag: Cardinal): Boolean;
begin
  Result := WTSShutdownSystem(FServerHandle, AShutdownFlag);
end;

constructor TJwWTSEventThread.Create(CreateSuspended: Boolean;
  AOwner: TJwTerminalServer);
begin
//  MessageBox(0, 'eventthread is created', 'debug', MB_OK);
  inherited Create(CreateSuspended);
  FOwner := AOwner;
  FreeOnTerminate := True;
end;

procedure TJwWTSEventThread.Execute;
begin
  while not Terminated do
  begin
//    MessageBox(0, 'thread is waiting', PChar(IntToStr(FEventFlag)), MB_OK);
    if WTSWaitSystemEvent(FOwner.ServerHandle, WTS_EVENT_ALL, FEventFlag) then
    begin
//      MessageBox(0, 'thread is dispatching', PChar(IntToStr(FEventFlag)), MB_OK);
      Synchronize(DispatchEvent);
    end;
    Sleep(10);
  end;
//  MessageBox(0, 'thread is terminated', 'debug', MB_OK);
end;

procedure TJwWTSEventThread.DispatchEvent;
begin
  if FEventFlag > WTS_EVENT_NONE then //) and (FEventFlag < WTS_EVENT_FLUSH) then
  begin
    FOwner.FireEvent(FEventFlag);
    FEventFlag := WTS_EVENT_NONE;
  end;
end;

constructor TJwWTSEnumServersThread.Create(CreateSuspended: Boolean;
  AOwner: TJwTerminalServer);
begin
  inherited Create(CreateSuspended);
//  Priority := tpLowest;
  FOwner := AOwner;
  FreeOnTerminate := False;
  Resume;
end;

procedure TJwWTSEnumServersThread.Execute;
var ServerInfoPtr: PJwWtsServerInfoAArray;
  pCount: DWORD;
  i: DWORD;
begin
  while not Terminated do
  begin
    OutputDebugString('thread is executing');
    // Clear the serverlist
    Synchronize(ClearServerList);

    // Since we return to a Stringlist (which does not support unicode)
    // we only use WTSEnumerateServersA
    if WTSEnumerateServersA(nil, 0, 1, PWTS_SERVER_INFOA(ServerInfoPtr),
      pCount) then
    begin
      for i := 0 to pCount - 1 do
      begin
        // If the thread is terminated then leave the loop
        if Terminated then Break;
        FServer := ServerInfoPtr^[i].pServerName;
        Synchronize(AddToServerList);
      end;
    end;

    // Note that on failure of WTSEnumerateServers we don't produce an
    // exception but return an empty ServerList instead. This is by design

    // If we have not been terminated we fire the OnServersEnumerated Event
    if not Terminated then
    begin
      Synchronize(DispatchEvent);
    end;

    // Cleanup
    if ServerInfoPtr <> nil then
    begin
      WTSFreeMemory(ServerInfoPtr);
    end;

    if not Terminated then
    begin
      // We are done for now so we suspend the thread. If the user wants to
      // enumerate servers again we simply resume the thread.
      OutputDebugString('suspending thread');
      Suspend;
    end;
  end;
end;

procedure TJwWTSEnumServersThread.AddToServerList;
begin
  FOwner.ServerList.Add(FServer);
end;

procedure TJwWTSEnumServersThread.ClearServerList;
begin
  FOwner.ServerList.Clear;
end;

procedure TJwWTSEnumServersThread.DispatchEvent;
begin
  if Assigned(FOwner.OnServersEnumerated) then
  begin
    // Fire the OnServersEnumerated event
    FOwner.OnServersEnumerated(FOwner);
  end;
end;

// Borland's WaitFor procedure contains a bug, in between loop iterations the
// TThread object can be freed and its Handle invalidated.  When
// MsgWaitForMultipleObjects() is called again, it fails, and then a call to
// CheckThreadError() afterwards throws on EOSError exception with an error
// code of 6 and an error message of "the handle is invalid".
function TJwWTSEnumServersThread.WaitFor;
var H: array[0..1] of THandle;
  WaitResult: Cardinal;
  Msg: TMsg;
begin
  H[0] := Handle;
  if GetCurrentThreadID = MainThreadID then
  begin
    WaitResult := 0;
    H[1] := SyncEvent;
    repeat
      { This prevents a potential deadlock if the background thread
        does a SendMessage to the foreground thread }
      if WaitResult = WAIT_OBJECT_0 + 2 then
        PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);
      WaitResult := MsgWaitForMultipleObjects(2, @H, False, 1000, QS_SENDMESSAGE);
      // The line below was added as Workaround to prevent Invalid Handle:
      if WaitResult = WAIT_FAILED then Break;
      CheckThreadError(WaitResult <> WAIT_FAILED);
      if WaitResult = WAIT_OBJECT_0 + 1 then
        CheckSynchronize;
    until WaitResult = WAIT_OBJECT_0;
  end else WaitForSingleObject(H[0], INFINITE);
  CheckThreadError(GetExitCodeThread(H[0], Result));
end;

destructor TJwWTSSessionList.Destroy;
begin
  inherited Destroy;
end;



function TJwWTSSessionList.Add(ASession: TJwWTSSession): Integer;
begin
  Result := inherited Add(ASession);
end;

function TJwWTSSessionList.GetItem(Index: Integer): TJwWTSSession;
begin
  Result := TJwWTSSession(inherited Items[Index]);
end;

function TJwWTSSessionList.IndexOf(ASession: TJwWTSSession): Integer;
begin
  Result := inherited IndexOf(ASession);
end;

procedure TJwWTSSessionList.Insert(Index: Integer; ASession: TJwWTSSession);
begin
  inherited Insert(Index, ASession);
end;

function TJwWTSSessionList.Remove(ASession: TJwWTSSession): Integer;
begin
  Result := inherited Remove(ASession);
end;

procedure TJwWTSSessionList.SetItem(Index: Integer; ASession: TJwWTSSession);
begin
  inherited Items[Index] := ASession;
end;

procedure TJwWTSSessionList.SetOwner(const Value: TJwTerminalServer);
begin
  FOwner := Value;
end;

function TJwWTSProcessList.Add(AProcess: TJwWTSProcess): Integer;
begin
  Result := inherited Add(AProcess);
end;

function TJwWTSProcessList.GetItem(Index: Integer): TJwWTSProcess;
begin
  Result := TJwWTSProcess(inherited Items[Index]);
end;

function TJwWTSProcessList.IndexOf(AProcess: TJwWTSProcess): Integer;
begin
  Result := inherited IndexOf(AProcess);
end;

procedure TJwWTSProcessList.Insert(Index: Integer; AProcess: TJwWTSProcess);
begin
  inherited Insert(Index, AProcess);
end;

function TJwWTSProcessList.Remove(AProcess: TJwWTSProcess): Integer;
begin
  Result := inherited Remove(AProcess);
end;

procedure TJwWTSProcessList.SetItem(Index: Integer; AProcess: TJwWTSProcess);
begin
  inherited Items[Index] := AProcess;
end;

function TJwTerminalServer.GetIdleProcessName: TJwString;
var hModule: THandle;
  lpBuffer: PWideChar;
  nBufferMax: Integer;
begin
  // The "System Idle Process" name is language dependant, therefore
  // we obtain it from Taskmgr and cache it in IdleProcessName property
  if FIdleProcessName = '' then
  begin
    hModule := LoadLibrary('taskmgr.exe');
    if hModule > 0 then
    begin
      nBufferMax := 256;  // 256 Chars seems safe for a resource string
      GetMem(lpBuffer, nBufferMax * SizeOf(WCHAR));
      // Windows NT4 and Windows XP Taskmgr have the System Idle Process
      // as resource string 10005. Windows Vista has it in MUI file
      // taskmgr.exe.mui with same id
      if LoadStringW(hModule, 10005, lpBuffer, nBufferMax) > 0 then
      begin
        FIdleProcessName := PWideCharToJwString(lpBuffer);
      end;
      // Cleanup
      FreeMem(lpBuffer);
      FreeLibrary(hModule);
    end;
  end;
  Result := FIdleProcessName;
end;

function TJwTerminalServerList.GetItem(Index: Integer): TJwTerminalServer;
begin
  Result := TJwTerminalServer(inherited Items[Index]);
end;

function TJwTerminalServerList.Add(ATerminalServer: TJwTerminalServer): Integer;
begin
  Result := inherited Add(ATerminalServer);
end;

function TJwTerminalServerList.IndexOf(ATerminalServer: TJwTerminalServer): Integer;
begin
  Result := inherited IndexOf(ATerminalServer);
end;

function TJwTerminalServerList.Remove(ATerminalServer: TJwTerminalServer): Integer;
begin
  Result := inherited Remove(ATerminalServer);
end;

procedure TJwTerminalServerList.SetItem(Index: Integer; ATerminalServer: TJwTerminalServer);
begin
  inherited Items[Index] := ATerminalServer;
end;

procedure TJwTerminalServerList.SetOwner(const Value: TComponent);
begin
  FOwner := Value;
end;

procedure TJwTerminalServerList.Insert(Index: Integer; ATerminalServer: TJwTerminalServer);
begin
  inherited Insert(Index, ATerminalServer);
end;

destructor TJwTerminalServerList.Destroy;
begin
  inherited Destroy;
end;

procedure TJwWTSProcessList.SetOwner(const Value: TJwTerminalServer);
begin
  FOwner := Value;
end;

function TJwWTSSession.ProtocolTypeToStr(AProtocolType: Cardinal): TJwString;
begin
  //TODO: use resource strings
  case AProtocolType of
    WTS_PROTOCOL_TYPE_CONSOLE: Result := 'Console';
    WTS_PROTOCOL_TYPE_ICA: Result := 'ICA';
    WTS_PROTOCOL_TYPE_RDP: Result := 'RDP';
  else
    Result := '';  // Should never happen
  end;
end;

procedure TJwWTSSession.GetSessionInfoPtr(const WTSInfoClass: _WTS_INFO_CLASS;
  var ABuffer: Pointer);
var dwBytesReturned: DWORD;
  Res: Bool;
begin
  Res :=
{$IFDEF UNICODE}
    WTSQuerySessionInformationW(GetServerHandle, FSessionId, WTSInfoClass,
      ABuffer, dwBytesReturned);
{$ELSE}
    WTSQuerySessionInformationA(GetServerHandle, FSessionId, WTSInfoClass,
      ABuffer, dwBytesReturned);
{$ENDIF}
  // function always returns an error 997: overlapped IO on session 0
  if (not Res) and (FSessionId <> 0) then
  begin
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
      'GetSessionInfoPtr', ClassName, RsUNTerminalServer, 0, True,
      'WTSQuerySessionInformation', ['WTSQuerySessionInformation']);
  end;
end;

function TJwWTSSession.GetSessionInfoStr(const WTSInfoClass: _WTS_INFO_CLASS):
  TJwString;
var
  dwBytesReturned: DWORD;
  aBuffer: Pointer;
  Res: Bool;
begin
  ABuffer := nil;
  Result := '';
  Res :=
{$IFDEF UNICODE}
    WTSQuerySessionInformationW(GetServerHandle, FSessionId, WTSInfoClass,
      ABuffer, dwBytesReturned);
{$ELSE}
    WTSQuerySessionInformationA(GetServerHandle, FSessionId, WTSInfoClass,
      ABuffer, dwBytesReturned);
{$ENDIF UNICODE}
  // function always returns an error 997: overlapped IO on session 0
  if (not Res) and (FSessionId <> 0) then
  begin
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
      'GetSessionInfoStr', ClassName, RSUNTerminalServer, 0, True,
      'WTSQuerySessionInformation', ['WTSQuerySessionInformation']);
  end
  else if ABuffer <> nil then
  begin
    Result :=
{$IFDEF UNICODE}
    TJwString(PWideChar(aBuffer));
{$ELSE}
    TJwString(PChar(aBuffer));
{$ENDIF UNICODE}
    WTSFreeMemory(aBuffer);
  end;
end;

function TJwWTSSession.GetSessionInfoDWORD(const WTSInfoClass: _WTS_INFO_CLASS): DWORD;
var dwBytesReturned: DWORD;
  ABuffer: Pointer;
  Res: Bool;
begin
  ABuffer := nil;
  Result := 0;
  Res :=
{$IFDEF UNICODE}
    WTSQuerySessionInformationW(GetServerHandle, FSessionId, WTSInfoClass,
      ABuffer, dwBytesReturned);
{$ELSE}
    WTSQuerySessionInformationA(GetServerHandle, FSessionId, WTSInfoClass,
      ABuffer, dwBytesReturned);
{$ENDIF}
  // function always returns an error 997: overlapped IO on session 0
  if (not Res) and (FSessionId <> 0) then
  begin
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
      'GetSessionInfoDWORD', ClassName, RSUNTerminalServer, 0, True,
      'WTSQuerySessionInformation', ['WTSQuerySessionInformation']);
  end
  else if ABuffer <> nil then
  begin
    Result := PDWord(ABuffer)^;
    WTSFreeMemory(ABuffer);
  end;
end;

function TJwWTSSession.GetServerHandle;
begin
  // The ServerHandle is stored in TJwTerminalServer
  //TODO: Owner = nil? or Owner.Owner = nil ?
  Result := Owner.Owner.FServerHandle;
end;

procedure TJwWTSSession.GetWinStationDriver;
var WinStationDriver: _WD_CONFIGW;
  dwReturnLength: DWORD;
begin
  FWdName := '';
  // ZeroMemory
  ZeroMemory(@WinStationDriver, SizeOf(WinStationDriver));

  if WinStationQueryInformationW(GetServerHandle, FSessionId,
    WdConfig, @WinStationDriver, SizeOf(WinStationDriver),
    dwReturnLength) then
  begin
    FWdName := PWideCharToJwString(WinStationDriver.WdName);
    FWdFlag := WinStationDriver.WdFlag;
  end;
end;

// #todo Remove IdleTime helper from JwaWinsta
procedure TJwWTSSession.GetWinStationInformation;
var WinStationInfo: _WINSTATION_INFORMATIONW;
  dwReturnLength: DWORD;
  lpBuffer: PWideChar;
begin
  // ZeroMemory
  ZeroMemory(@WinStationInfo, SizeOf(WinStationInfo));
  lpBuffer := nil;

  if WinStationQueryInformationW(GetServerHandle, FSessionId,
    WinStationInformation, @WinStationInfo, SizeOf(WinStationInfo),
    dwReturnLength) then
  begin
    // Only Active Session has Logon Time
    if FConnectState = WTSActive then
    begin
      // Reserve memory
      GetMem(lpBuffer, MAX_PATH * SizeOf(WCHAR));
      try
        // Format LogonTime string
        DateTimeStringSafe(@WinStationInfo.LogonTime, lpBuffer, MAX_PATH);
        FLogonTimeStr := PWideCharToJwString(lpBuffer);
      finally
        FreeMem(lpBuffer);
        lpBuffer := nil;
      end;

//      if FWinStationName <> 'Console' then
      if FWdFlag > WD_FLAG_CONSOLE then
      begin
        // Counter values (Status from TSAdmin)
        FIncomingBytes := WinStationInfo.IncomingBytes;
        FIncomingCompressedBytes := WinStationInfo.IncomingCompressedBytes;
        FIncomingFrames := WinStationInfo.IncomingFrames;

        FOutgoingBytes := WinStationInfo.OutgoingBytes;
        FOutgoingCompressBytes := WinStationInfo.OutgoingCompressBytes;
        FOutgoingFrames := WinStationInfo.OutgoingFrames;

        // Calculate Compression ratio and store as formatted string
        if WinStationInfo.OutgoingBytes > 0 then // 0 division check
        begin
          FCompressionRatio := Format('%1.2f',
            [WinStationInfo.OutgoingCompressBytes /
            WinStationInfo.OutgoingBytes]);
        end;
      end;
    end
    else if FConnectState = WTSDisconnected then
    begin
      // A disconnected session is Idle since DisconnectTime
      WinStationInfo.LastInputTime := WinStationInfo.DisconnectTime;
    end;

    if FUsername = '' then
    begin
      // A session without a user is not idle, usually these are special
      // sessions like Listener, Services or console session 
      FIdleTimeStr := '.';
    end
    else
    begin
      // Calculate & Format Idle Time String, DiffTimeString allocates the
      // memory for us
      DiffTimeString(WinStationInfo.LastInputTime, WinStationInfo.CurrentTime,
        lpBuffer);
      try
        FIdleTimeStr := PWideCharToJwString(lpBuffer);
      finally
        // We free the memory DiffTimeString has allocated for us
        FreeMem(lpBuffer);
      end;
    end;

    FConnectTime := FileTime2DateTime(WinStationInfo.ConnectTime);
    FDisconnectTime := FileTime2DateTime(WinStationInfo.DisconnectTime);
    // for A disconnected session LastInputTime has been set to DisconnectTime
    FLastInputTime := FileTime2DateTime(WinStationInfo.LastInputTime);
    FLogonTime := FileTime2DateTime(WinStationInfo.LogonTime);
    FCurrentTime := FileTime2DateTime(WinStationInfo.CurrentTime);
  end;

end;

function TJwWTSSession.GetClientAddress: TJwString;
var ClientAddressPtr: PWtsClientAddress;
begin
  GetSessionInfoPtr(WTSClientAddress, Pointer(ClientAddressPtr));
  //TODO:
  //if ClientAddressPtr = nil then
  //raise Exception

  
  {Note that the first byte of the IP address returned in the ppBuffer
   parameter will be located at an offset of 2 bytes from the start of
   the Address member of the returned WTS_CLIENT_ADDRESS structure.}
  case ClientAddressPtr^.AddressFamily of
    AF_INET:
      Result := Format('%d.%d.%d.%d', [ClientAddressPtr^.Address[2],
        ClientAddressPtr^.Address[3], ClientAddressPtr^.Address[4],
        ClientAddressPtr^.Address[5]]);
    AF_INET6:
      Result := 'IPv6 address not yet supported';
    AF_IPX:
      Result := 'IPX is no longer supported';
    AF_NETBIOS:
      Result := 'NETBIOS is not supported';
    AF_UNSPEC:
      Result := '';
  end;
  WTSFreeMemory(ClientAddressPtr);
end;

procedure TJwWTSSession.GetClientDisplay;
var ClientDisplayPtr: PWtsClientDisplay;
begin
  GetSessionInfoPtr(WTSClientDisplay, Pointer(ClientDisplayPtr));
  //TODO:
  //if ClientDisplayPtr = nil then
  //raise Exception

  FHorizontalResolution := ClientDisplayPtr^.HorizontalResolution;
  FVerticalResolution := ClientDisplayPtr^.VerticalResolution;
  FColorDepth := ClientDisplayPtr^.ColorDepth;
  WTSFreeMemory(ClientDisplayPtr);
end;

constructor TJwWTSSession.Create(const AOwner: TJwWTSSessionList;
  const ASessionId: TJwSessionId; const AWinStationName: TJwString;
  const AConnectState: TWtsConnectStateClass);
begin
  FOwner := AOwner; // Session is owned by the SessionList
  // First store the SessionID
  FSessionId := ASessionId;
  FConnectState := AConnectState;
  FConnectStateStr := PWideCharToJwString(StrConnectState(FConnectState, False));
//  ConnectStateToStr(FConnectState);
{  if (FConnectState = WTSDisconnected) and (FWdFlag <> WD_FLAG_CONSOLE) then
  begin
    // Disconnected sessions have no WinStationName so set this to Disconnected
    FWinStationName := FConnectStateStr;
  end
  else begin
    FWinStationName := AWinStationName;
  end;}
  FWinStationName := AWinStationName;
  FApplicationName := GetSessionInfoStr(WTSApplicationName);
  FClientAddress := GetClientAddress;
  FClientBuildNumber := GetSessionInfoDWORD(WTSClientBuildNumber);
  FClientDirectory := GetSessionInfoStr(WTSClientDirectory);
  FClientHardwareId := GetSessionInfoDWORD(WTSClientHardwareId);
  FClientName := GetSessionInfoStr(WTSClientName);
  FClientProductId := GetSessionInfoDWORD(WTSClientProductId);
  FClientProtocolType := GetSessionInfoDWORD(WTSClientProtocolType);
  FClientProtocolStr := ProtocolTypeToStr(FClientProtocolType);
  FInitialProgram := GetSessionInfoStr(WTSInitialProgram);
  FWorkingDirectory := GetSessionInfoStr(WTSWorkingDirectory);
  FDomain := GetSessionInfoStr(WTSDomainName); // Documented way:
  // FDomain := WinStationInfoPtr.Domain; // Undocumented way:
  FUsername := GetSessionInfoStr(WTSUsername); // Documented way:
  // FUsername := WinStationInfoPtr.Username; // Undocumented way:

//  if TJwWindowsVersion.IsWindowsVista(True) then
//  begin
    // Status: Not implemented yet}
    { Vista SP1 has a documented way of retrieving idle time, although
      the documentation is preliminary and is subject to change
      see here: http://msdn2.microsoft.com/en-us/library/bb736370.aspx}
//  end
//  else begin
    { Use undocumented API to retrieve Idle and LoginTime }
    { this is for Windows 2000, Windows XP and Windows 2003 }
    { Might work on Windows Vista and Windows Server 2008 (not tested) }
    { not expected to work on Windows NT 4 (not tested) }
    GetWinStationDriver;
    GetWinStationInformation;
    WinStationGetRemoteIPAddress(GetServerHandle, ASessionId, FRemoteAddress,
      FRemotePort);
//  end;
end;

function TJwWTSSession.GetServerName: TJwString;
begin
  //TODO: Owner = nil? or Owner.Owner = nil ?
  Result := Owner.Owner.Server;
end;

function TJwWTSSession.Logoff(bWait: Boolean): Boolean;
begin
  Result := WTSLogoffSession(GetServerHandle, FSessionId, bWait);
end;

function TJwWTSSession.Disconnect(bWait: Boolean): Boolean;
begin
  Result := WTSDisconnectSession(GetServerHandle, FSessionId, bWait);
end;

function TJwWTSSession.SendMessage(const AMessage: TJwString;
  const ACaption: TJwString; const uType: DWORD; const ATimeOut: DWORD): DWORD;
begin
{$IFDEF UNICODE}
  WTSSendMessageW(GetServerHandle, FSessionId, PWideChar(ACaption),
    Length(ACaption) * SizeOf(WCHAR), PWideChar(AMessage),
    Length(AMessage) * SizeOf(WCHAR), uType, ATimeOut, Result, ATimeOut <> 0);
{$ELSE}
  WTSSendMessageA(GetServerHandle, FSessionId, PChar(ACaption),
    Length(ACaption), PChar(AMessage), Length(AMessage), uType, ATimeOut,
    Result, ATimeOut <> 0);
{$ENDIF UNICODE}
end;

function TJwWTSSession.PostMessage(const AMessage: TJwString;
  const ACaption: TJwString; const uType: DWORD): DWORD;
begin
{$IFDEF UNICODE}
  WTSSendMessageW(GetServerHandle, FSessionId, PWideChar(ACaption),
    Length(ACaption) * SizeOf(WCHAR), PWideChar(AMessage),
    Length(AMessage) * SizeOf(WCHAR), uType, 0, Result, False);
{$ELSE}
    WTSSendMessageA(GetServerHandle, FSessionId, PChar(ACaption),
      Length(ACaption), PChar(AMessage), Length(AMessage), uType, 0,
      Result, False);
{$ENDIF UNICODE}
end;


function TJwWTSSession.Shadow: boolean;
begin
  // This function only exists in Unicode
  Result := WinStationShadow(GetServerHandle,
    PWideChar(WideString(GetServerName)), FSessionId, VK_MULTIPLY,
    MOD_CONTROL);
end;

constructor TJwWTSProcess.Create(const AOwner: TJwWTSProcessList;
  const ASessionId: TJwSessionId; const AProcessID: TJwProcessId;
  const AProcessName: TJwString; const AUsername: TjwString);
var pWinStationName: PWideChar;
  Res: Boolean;
begin
  FOwner := AOwner;
  FSessionID := ASessionId;

{  GetMem(pWinStationName, WINSTATIONNAME_LENGTH * SizeOf(WideChar));
  try
    ZeroMemory(pWinStationName, WINSTATIONNAME_LENGTH * SizeOf(WideChar));

    Res := WinStationNameFromLogonIdW(GetServerHandle, FSessionId,
      pWinStationName);
    if Res then
    begin
      FWinStationName := PWideCharToJwString(pWinStationName);
    end;
    if FWinStationName = '' then
    begin
      // Return disconnected if WinStationName = empty
      FWinStationName := PWideCharToJwString(
        StrConnectState(WTSDisconnected, False));
    end;
  finally
    // FreeMem
    FreeMem(pWinStationName);
  end;}
  FWinStationName := FOwner.FOwner.GetWinStationName(ASessionId);

  FProcessId := AProcessId;
  FProcessName := AProcessName;
  FUsername := AUsername;
end;


function TJwWTSProcess.GetServerHandle: THandle;
begin
  //TODO: Owner = nil? or Owner.Owner = nil ?
  // The ServerHandle is stored in TJwTerminalServer
  Result := Owner.Owner.FServerHandle;
end;

function TjwWTSProcess.Terminate: Boolean;
begin
  Result := WTSTerminateProcess(GetServerHandle, ProcessId, 0);
end;

function TJwWTSProcess.Terminate(dwExitCode: Cardinal): Boolean;
begin
  Result := WTSTerminateProcess(GetServerHandle, ProcessId, dwExitCode);
end;

end.
