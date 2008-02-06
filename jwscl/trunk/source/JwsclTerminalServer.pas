{@abstract(This unit provides access to Terminal Server api functions)
@author(Remko Weijnen)
@created(10/26/2007)
@lastmod(10/26/2007)



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

The Original Code is JwsclTerminalServer.pas.

The Initial Developer of the Original Code is Remko Weijnen.
Portions created by Remko Weijnen are Copyright (C) Remko Weijnen. All rights reserved.
}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclTerminalServer;
{$I Jwscl.inc}

interface

uses Classes, Contnrs, DateUtils, SysUtils,
  JwaWindows,
  JwsclExceptions, JwsclResource, JwsclSid, JwsclTypes,
  JwsclUtils, JwsclToken,
  JwsclVersion, JwsclStrings;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type

  { forward declarations }
  TJwTerminalServer = class;
  TJwTerminalServerList = class;
  TJwWTSEventThread = class;
  TJwWTSEnumServersThread = class;
  TJwWTSSessionShadow = class;
  TJwWTSSession = class;
  TJwWTSSessionList = class;
  TJwWTSProcess = class;
  TJwWTSProcessList = class;




  {@Name defines }
  PJwTerminalServer = ^TJwTerminalServer;
  TJwTerminalServer = class(TObject)
  protected
    FComputerName: TJwString;
    FConnected: Boolean;
    FData: Pointer;
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
    FTerminalServerEventThread: TJwWTSEventThread;
    FServer: TJwString;
    FTag: Integer;

    function GetIdleProcessName: TJwString;
    function GetServers: {$IFDEF UNICODE}TWideStringList{$ELSE}TStringList{$ENDIF UNICODE};
    function GetServer: TJwString;
    function GetWinStationName(const SessionId: DWORD): TJwString;
    procedure OnEnumServersThreadTerminate(Sender: TObject);
    procedure SetServer(const Value: TJwString);
    procedure FireEvent(EventFlag: DWORD);
  public
    procedure Connect;
    property Data: Pointer read FData write FData;
    procedure Disconnect;
    property ComputerName: TJwString read FComputerName;
    property Connected: Boolean read FConnected;
    constructor Create;
    destructor Destroy; override;
    function EnumerateProcesses: Boolean;
    function EnumerateServers(ADomain: String):Boolean;
    function EnumerateSessions: boolean;
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
    property Processes: TJwWTSProcessList read FProcesses write FProcesses;
    property Server: TJwString read GetServer write SetServer;
    property ServerHandle: THandle read FServerHandle;
    property Servers: {$IFDEF UNICODE}TWideStringList{$ELSE}
      TStringList{$ENDIF UNICODE} read GetServers;
    property ServerList: TStringList read FServerList;
    property Sessions: TJwWTSSessionList read FSessions write FSessions;
    function Shutdown(AShutdownFlag: DWORD): Boolean;
    property Tag: Integer read FTag write FTag;
  end;

  PJwTerminalServerList = ^TJwTerminalServerList;
  TJwTerminalServerList = class(TObjectList)
  protected
    FOwnsObjects: Boolean;
    FOwner: TComponent;
  protected
    function GetItem(Index: Integer): TJwTerminalServer;
    procedure SetItem(Index: Integer; ATerminalServer: TJwTerminalServer);
    procedure SetOwner(const Value: TComponent);
  public
    destructor Destroy; reintroduce;
    function Add(ATerminalServer: TJwTerminalServer): Integer;
    function FindByServer(const ServerName: WideString; const IgnoreCase: boolean = False): TJwTerminalServer;
    function IndexOf(ATerminalServer: TJwTerminalServer): Integer;
    procedure Insert(Index: Integer; ATerminalServer: TJwTerminalServer);
    property Items[Index: Integer]: TJwTerminalServer read GetItem write SetItem; default;
    property Owner: TComponent read FOwner write SetOwner;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    function Remove(ATerminalServer: TJwTerminalServer): Integer;
  end;

  TJwWTSEventThread = class(TJwThread)
  protected
    FOwner: TJwTerminalServer;
    FEventFlag: DWORD;
  public
    constructor Create(CreateSuspended: Boolean; AOwner: TJwTerminalServer);
    procedure DispatchEvent;
    procedure Execute; override;
  end;

  TJwWTSEnumServersThread = class(TThread)
  protected
    FDomain: TJwString;
    FOwner: TJwTerminalServer;
    FServer: TJwString;
    FTerminatedEvent: THandle;
  public
    constructor Create(CreateSuspended: Boolean; Owner: TJwTerminalServer;
      Domain: TJwString);
    procedure AddToServerList;
    procedure ClearServerList;
    procedure DispatchEvent;
    procedure Execute; override;
    procedure Wait;
    function WaitFor: LongWord;
  end;

  PJwWTSSession = ^TJwWTSSession;
  TJwWTSSession = class(TObject)
  protected
    {@exclude}
    FApplicationName: TJwString;
    {@exclude}
    FClientAddress: TJwString;
    {@exclude}
    FClientBuildNumber: DWORD;
    {@exclude}
    FColorDepth: DWORD;
    {@exclude}
    FClientDirectory: TJwString;
    {@exclude}
    FClientHardwareId: DWORD;
    {@exclude}
    FClientName: TJwString;
    {@exclude}
    FClientProductId: WORD;
    {@exclude}
    FClientProtocolType: WORD;
    {@exclude}
    FClientProtocolStr: TJwString;
    {@exclude}
    FCompressionRatio: TJwString;
    {@exclude}
    FConnectState: TWtsConnectStateClass;
    {@exclude}
    FConnectStateStr: TJwString;
    {@exclude}
    FConnectTime: TDateTime;
    {@exclude}
    FCurrentTime: TDateTime;
    {@exclude}
    FDisconnectTime: TDateTime;
    {@exclude}
    FDomain: TJwString;
    {@exclude}
    FIdleTime: Int64;
    {@exclude}
    FIdleTimeStr: TJwString;
    {@exclude}
    FIncomingBytes: DWORD;
    {@exclude}
    FIncomingCompressedBytes: DWORD;
    {@exclude}
    FIncomingFrames: DWORD;
    {@exclude}
    FHorizontalResolution: DWORD;
    {@exclude}
    FInitialProgram: TJwString;
    {@exclude}
    FLastInputTime: TDateTime;
    {@exclude}
    FLogonTime: Int64;
    {@exclude}
    FLogonTimeStr: TJwString;
    {@exclude}
    FOwner: TJwWTSSessionList;
    {@exclude}
    FOutgoingBytes: DWORD;
    {@exclude}
    FOutgoingCompressBytes: DWORD;
    {@exclude}
    FOutgoingFrames: DWORD;
    {@exclude}
    FProtocolTypeStr: TJwString;
    {@exclude}
    FRemoteAddress: TJwString;
    {@exclude}
    FRemotePort: WORD;
    {@exclude}
    FSessionId: TJwSessionId;
    {@exclude}
    FUsername: TJwString;
    {@exclude}
    FVerticalResolution: DWORD;
    {@exclude}
    FWdFlag: DWORD;
    {@exclude}
    FWdName: TJwString;
    {@exclude}
    FWinStationName: TJwString;
    {@exclude}
    FWorkingDirectory: TJwString;
    {@exclude}
    FShadow : TJwWTSSessionShadow;
    {@exclude}

    {@exclude}
    FToken : TJwSecurityToken;
    {@exclude}
    FUserSid : TJwSecurityID;

    {@exclude}
    procedure GetClientDisplay;
    {@exclude}
    function GetServer: TJwString;
    {@exclude}
    function GetSessionInfoDWORD(const WTSInfoClass: WTS_INFO_CLASS): DWORD;
    {@exclude}
    procedure GetSessionInfoPtr(const WTSInfoClass: WTS_INFO_CLASS;
      var ABuffer: Pointer);
    {@exclude}
    function GetSessionInfoStr(const WTSInfoClass: WTS_INFO_CLASS): TJwString;
    {@exclude}
    procedure GetWinStationInformation;
    {@exclude}
    procedure GetWinStationDriver;

    {@exclude}
    function GetToken : TJwSecurityToken;
    {@exclude}
    function GetUserSid : TJwSecurityID;

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

     @raises(EJwsclSecurityException <Show here the reason for that exception type.
        Also for every failed precondition >)
     @raises(EJwsclTerminalServerException < Create your own  >)
     @raises(EJwsclTerminalSessionException < dito>)
     @raises(EJwsclWinCallFailedException <winAPI call failed. Its always called that!>)
     @raises(EJwsclNILParameterException <a parameter is nil. Search for
      that exception in other classes for an example>)
  }
    constructor Create(const Owner: TJwWTSSessionList;
      const SessionId: TJwSessionId; const WinStationName: TJwString;
      const ConnectState: TWtsConnectStateClass);
    destructor Destroy; override;

    {@Name returns the the startup application as specified in the
     Terminal Server client. If no startup application was specified
     an empty string is returned.
     @br@br
     @bold(Remarks:) Console sessions always returns empty value.
     }
    property ApplicationName: TJwString read FApplicationName;

    {@Name returns the Client IP Address as string. This is the local IP
     address of a client as reported by the Terminal Server Client
     @br@br
     @bold(Remarks:) Console sessions always returns empty value.
     }
    property ClientAddress: TJwString read FClientAddress;

    {@Name returns the version number of the Terminal Server Client
     @br@br
     @bold(Remarks:) Console sessions always returns empty value.
     @seealso(RemoteAddress)
     @seealso(RemotePort)
     }
     property ClientBuildNumber: DWORD read FClientBuildNumber;

    {@Name returns the version number of the Terminal Server Client
     @br@br
     @bold(Remarks:) Console sessions always returns empty value.
     }
    property ClientDirectory: TJwString read FClientDirectory;

    {@Name returns a client-specific hardware identifier
     @br@br
     @bold(Remarks:) Console sessions always returns empty value.
     }
    property ClientHardwareId: DWORD read FClientHardwareId;

    {@Name returns the local computer name of the client
     @br@br
     @bold(Remarks:) Console sessions always returns empty value.
     }
    property ClientName: TJwString read FClientName;

    {@Name returns a client-specific product identifier.
     @br@br
     @bold(Remarks:) Console sessions always returns empty value.
     }
    property ClientProductId: WORD read FClientProductId;

    {@Name returns a value that indicates the protocol type
     This is one of the following values:
     @table(
     @rowHead(  @cell(ClientProtocolType) @cell(Meaning))
      @row(     @cell(WTS_PROTOCOL_TYPE_CONSOLE) @cell(The Console session))
      @row(     @cell(WTS_PROTOCOL_TYPE_ICA) @cell(The ICA protocol))
      @row(     @cell(WTS_PROTOCOL_TYPE_RDP) @cell(The RDP protocol))
      )
     @seealso(ClientProtocolStr)
     @seealso(RemoteAddress)
     @seealso(RemotePort)
      }
    property ClientProtocolType: WORD read FClientProtocolType;

    {@Name returns a string  that indicates the protocol type
     This is one of the following values:
     @table(
     @rowHead(  @cell(ClientProtocolType) @cell(Value))
      @row(     @cell(WTS_PROTOCOL_TYPE_CONSOLE) @cell(Console))
      @row(     @cell(WTS_PROTOCOL_TYPE_ICA) @cell(ICA))
      @row(     @cell(WTS_PROTOCOL_TYPE_RDP) @cell(RDP))
      )
     @seealso(ClientProtocolType)
     @seealso(RemoteAddress)
     @seealso(RemotePort)
      }
    property ClientProtocolStr: TJwString read FClientProtocolStr;

    property ColorDepth: DWORD read FColorDepth;

    {@Name returns the current compression ratio as string with 2 decimals.
     Compression Ratio equals OutgoingCompressBytes / OutgoingBytescompressed
     Console sessions always returns empty value.
     @br@br
     @seealso(IncomingBytes)
     @seealso(OutgoingBytes)
    }
    property CompressionRatio: TJwString read FCompressionRatio;

    {@Name returns the connection state of the session. Which can be one of the
     following values:
     @unorderedList(
      @itemSpacing Compact
      @item(WTSActive)
      @item(WTSConnected)
      @item(WTSConnectQuery)
      @item(WTSShadow)
      @item(WTSDisconnected)
      @item(WTSIdle)
      @item(WTSListen)
      @item(WTSReset)
      @item(WTSDown)
      @item(WTSInit)
     )
     @br
     @bold(Remarks:) On Windows XP, however, the state for session 0 can be
     misleading because it will be WTSDisconnected even if there is no user
     logged on. To accurately determine if a user has logged on to session 0,
     you can use the Username property
    @br
    @link(http://support.microsoft.com/kb/307642/en-us)
    }
    property ConnectState: TWtsConnectStateClass read FConnectState;
    {@Name returns a localised connection state string.
     }
    property ConnectStateStr: TJwString read FConnectStateStr;

    {@Name The most recent client connection time.
     }
    property ConnectTime: TDateTime read FConnectTime;

    {@Name The time that the TJwWTSSession info was queried. This can be
     used to calculate time differences such as idle time
     }
    property CurrentTime: TDateTime read FCurrentTime;

    {The @Name function disconnects the logged-on user
     from the specified Terminal Services session without closing the session.
     If the user subsequently logs on to the same terminal server, the user is
     reconnected to the same session.
     @param(bWait Indicates whether the operation is synchronous. Specify TRUE
     to wait for the operation to complete, or FALSE to return immediately.)
     }
    function Disconnect(bWait: Boolean): Boolean;

    {@Name The last client disconnection time.
    }
    property DisconnectTime: TDateTime read FDisconnectTime;
    {@Name the domain of the logged-on user
    }
    property Domain: TJwString read FDomain;

    {@exclude}
    function GetClientAddress: TJwString;

    {@exclude}
    function GetServerHandle: THandle;
    property HorizontalResolution: DWORD read FHorizontalResolution;

    {@Name The elapsed time since last user input in the session in the number
     of 100-nanosecond intervals since January 1, 1601 (TFileTime).
     @br
     @seealso(IdleTimeStr)
    }
    property IdleTime: Int64 read FIdleTime;

    {@Name The elapsed time since last user input in the session as formatted
     string. The string is formatted according to the table below:
     @table(
     @rowHead(  @cell(days) @cell(hours) @cell(minutes) @cell(value))
      @row(     @cell(> 0)  @cell(any)   @cell(any)     @cell(+d+hh:mm))
      @row(     @cell(0)    @cell(>0)    @cell(any)     @cell(hh:mm))
      @row(     @cell(0)    @cell(0)     @cell(any)     @cell(mm))
      @row(     @cell(0)    @cell(0)     @cell(0)       @cell(.))
      )
     @seealso(IdleTimeStr)
    }
    property IdleTimeStr: TJwString read FIdleTimeStr;

    {@Name Uncompressed Remote Desktop Protocol (RDP) data from the client
     to the server.
     @seealso(OutgoingBytes)
    }
    property IncomingBytes: DWORD read FIncomingBytes;

    {@Name string containing the name of the initial program that
     Terminal Services runs when the user logs on.
    }
    property InitialProgram: TJwString read FInitialProgram;

    {@Name The time of the last user input in the session.
    }
    property LastInputTime: TDateTime read FLastInputTime;

    {The @Name function logs off a specified Terminal Services session
     @param(bWait Indicates whether the operation is synchronous. Specify TRUE
     to wait for the operation to complete, or FALSE to return immediately.)
     }
    function Logoff(bWait: Boolean): Boolean;

    {@Name The time that the user logged on to the session in the number
     of 100-nanosecond intervals since January 1, 1601 (TFileTime).
     @seealso(LogonTimeStr)
    }
    property LogonTime: Int64 read FLogonTime;

    {@Name The time that the user logged on to the session as a localised
     Date Time string.
     @seealso(LogonTime)
    }
    property LogonTimeStr: TJwString read FLogonTimeStr;

    {@Name of this session object, which can only be a TJwWTSSessionList
    }
    property Owner: TJwWTSSessionList read FOwner write FOwner;
    {@Name Uncompressed RDP data from the server to the client.
     @seealso(IncomingBytes)
    }
    property OutgoingBytes: DWORD read FOutgoingBytes;

    {The @Name function displays a message box on the client desktop.
     @param(AMessage: string that contains the message to be displayed)
     @param(ACaption: string that contains the dialog box title.)
     @param(uType: Specifies the contents and behavior of the message box.
     This value is typically MB_OK. For a complete list of values, see the
     uType parameter of the MessageBox function.)
     @br
     @bold(Remarks:) PostMessage does not wait for the user to respond.
     @seealso(SendMessage)
    }
    function PostMessage(const AMessage: TJwString; const ACaption: TJwString;
      const uType: DWORD): DWORD;

    {@exclude}
    function ProtocolTypeToStr(const AProtocolType: DWORD): TJwString;

    {@Name returns the real IP Address that is connected to the Terminal Server.
     (As opposed to ClientAddress which returns the address as specified by
     the client which is usually just it's local ip address)

     @seealso(RemotePort)
    }
    property RemoteAddress: TJwString read FRemoteAddress;

    {@Name returns the Remote Port number which is is connected to the
    Terminal Server. The Terminal Server listens (by default) on port 3389
    but the client connects with a random available port.
     @seealso(RemoteAddress)
    }
    property RemotePort: WORD read FRemotePort;

    {The @Name function displays a message box on the client desktop.
     @param(AMessage string that contains the message to be displayed.)
     @param(ACaption string that contains the dialog box title.)
     @param(uType Specifies the contents and behavior of the message box.
     This value is typically MB_OK. For a complete list of values, see the
     uType parameter of the MessageBox function.)
     @param(ATimeOut Specifies the time, in seconds, that the SendMessage
     function waits for the user's response. If the user does not respond within
     the time-out interval, the pResponse parameter returns IDTIMEOUT.
     If the Timeout parameter is zero, WTSSendMessage will wait indefinitely
     for the user to respond.)
     @return(@Name returns the user's response, which can be one of the
     following values:
     @table(
     @rowHead(  @cell(Value) @cell(Meaning))
      @row(     @cell(IDABORT) @cell(Abort button was selected.))
      @row(     @cell(IDCANCEL) @cell(Cancel button was selected.))
      @row(     @cell(IDIGNORE) @cell(Ignore button was selected.))
      @row(     @cell(IDNO) @cell(No button was selected.))
      @row(     @cell(IDRETRY) @cell(Retry button was selected.))
      @row(     @cell(IDYES) @cell(Yes button was selected.))
      @row(     @cell(IDASYNC) @cell(The bWait parameter was FALSE, so the function returned without waiting for a response.))
      @row(     @cell(IDTIMEOUT) @cell(The bWait parameter was TRUE and the time-out interval elapsed.))
      ))
     @br
     @bold(Remarks:) If you don't need to wait for the user's response you can
     use the PostMessage function
     @seealso(PostMessage)
    }
    function SendMessage(const AMessage: TJwString; const ACaption: TJwString;
      const uType: DWORD; const ATimeOut: DWORD): DWORD;

    {@Name the netbios name of the Terminal Server.
     @br@br
     @bold(Remarks:) If you want to connect to a Terminal Server locally
     you should not specify the server name. Please note that in the case of a
     local connection this property @bold(will return the computername))
    }
    property Server: TJwString read GetServer;

    {@Name the session identifier
    }
    property SessionId: TJwSessionId read FSessionId;
    {The @Name function starts the remote control of another Terminal Services
    session. You must call this function from a remote session.
    @Return(If the function fails, the return value is zero. To get extended
    error information, call GetLastError)
    }
    function Shadow: boolean;

    {@Name returns information about the Shadow State and Shadow Mode of
    a session.
    @br
    Shadow State shows if the session is shadowing another session or is being
    shadowed by another session.
    @br@br
    Shadow Mode queries the shadow permissions for this session.
    }
    property ShadowInformation: TJwWTSSessionShadow read FShadow;

    {@Name the name of the user associated with the session.
    }
    property Username: TJwString read FUsername;

    property VerticalResolution: DWORD read FVerticalResolution;

    {WinStationDriver Flag (@Name) returns a value indicating the protocol and
     connection type. It's usefull for easy determination of console session.
     Possible values:
     @rowHead(  @cell(Value) @cell(Meaning))
      @row(     @cell(WD_FLAG_CONSOLE_XP) @cell(XP Console sessions))
      @row(     @cell(WD_FLAG_CONSOLE) @cell(2003/2008 Console Session))
      @row(     @cell(WD_FLAG_RDP) @cell(RDP Session))
      @row(     @cell(WD_FLAG_ICA) @cell(ICA Session))
      ))
    }
    property WdFlag: DWORD read FWdFlag;

    {WinStationDriver Name (@Name) returns a value indicating the protocol and
     protocol type.
     Known Microsoft values:
     @rowHead(  @cell(Operating System) @cell(Value))
      @row(     @cell(Windows 2000) @cell(Microsoft RDP 5.0))
      @row(     @cell(Windows XP) @cell(Microsoft RDP 5.1))
      @row(     @cell(Windows 2003) @cell(Microsoft RDP 5.2))
      @row(     @cell(Windows 2008/Vista) @cell(Microsoft RDP 6.0))
      ))
     Known Citrix values:
     @rowHead(  @cell(Version) @cell(Value))
      @row(     @cell(Citrix Presentation Server 4) @cell(Citrix ICA 3.0))
      ))

    }
    property WinStationDriverName: TJwString read FWdName;

    {@Name returns the session name.
     @br@br
     @bold(Remarks:) Despite its name, specifying this property does not return
     the window station name. Rather, it returns the name of the Terminal
     Services session.@br
     For RDP this will be something like RDP-Tcp#023@br
     For ICA this will be something like ICA-tcp#014
         }
    property WinStationName: TJwString read FWinStationName;

    {@Name the default directory used when launching the initial program.}
    property WorkingDirectory: TJwString read FWorkingDirectory;

    {@Name returns the token of the session.
     This call needs the TCB privilege and the process must run under
     SYSTEM account; otherwise EJwsclPrivilegeCheckException,
     EJwsclWinCallFailedException is raised.
     The returned value is cached and must not be freed!



    }
    property Token : TJwSecurityToken read GetToken;

    {@Name returns the logged on User of the session.
    This call needs the TCB privilege and the process must run under
     SYSTEM account; otherwise EJwsclPrivilegeCheckException,
     EJwsclWinCallFailedException is raised.

     The returned value is cached and must not be freed!

     If the value cannot be obtained the return value is nil.
    }
    property UserSid : TJwSecurityID read GetUserSid;
  end;

  { List Of TJwWTSSession Objects }
  PJwWTSSessionList = ^TJwWTSSessionList;
  TJwWTSSessionList = class(TObjectList)
  protected
    FOwner: TJwTerminalServer;

    function GetItem(Index: Integer): TJwWTSSession;
    procedure SetItem(Index: Integer; ASession: TJwWTSSession);

    procedure SetOwner(const Value: TJwTerminalServer);
  public
    destructor Destroy; reintroduce;
    function Add(ASession: TJwWTSSession): Integer;
    function IndexOf(ASession: TJwWTSSession): Integer;
    procedure Insert(Index: Integer; ASession: TJwWTSSession);
    property Items[Index: Integer]: TJwWTSSession read GetItem write SetItem; default;
    function Remove(ASession: TJwWTSSession): Integer;

    property Owner: TJwTerminalServer read FOwner write SetOwner;

  end;

  TJwWTSProcess = class(TObject)
  protected
    FOwner: TJwWTSProcessList;
    FProcessAge: Int64;
    FProcessAgeStr: TJwString;
    FProcessCreateTime: TJwString;
    FProcessCPUTime: Int64;
    FProcessCPUTimeStr: TJwString;
    FProcessId: TJwProcessID;
    FProcessMemUsage: DWORD;
    FProcessName: TJwString;
    FProcessVMSize: DWORD;
    FSessionId: TJwSessionID;
//    FSidStr: TJwString;
    FUsername: TJwString;
    FWinStationName: TJwString;
    FToken : TJwSecurityToken;
    FUserSid : TJwSecurityID;

    function GetServer: TJwString;
    function GetToken : TJwSecurityToken;
    function GetUserSid : TJwSecurityID;

  public
    constructor Create(const Owner: TJwWTSProcessList;
      const SessionId: TJwSessionId; const ProcessID: TJwProcessId;
      const ProcessName: TJwString; const Username: TJwString);

    destructor Destroy; override;

    function GetServerHandle: THandle; virtual;


  public
    function Terminate: boolean; overload;
    function Terminate(const dwExitCode: DWORD): boolean; overload;
  public

    property Owner: TJwWTSProcessList read FOwner write FOwner;
    property SessionId: TJwSessionId read FSessionId;
    property ProcessAge: Int64 read FProcessAge;
    property ProcessAgeStr: TJwString read FProcessAgeStr;
    property ProcessCPUTime: Int64 read FProcessCPUTime;
    property ProcessCPUTimeStr: TJwString read FProcessCPUTimeStr;
    property ProcessCreateTime: TJwString read FProcessCreateTime;
    property ProcessId: TJwProcessId read FProcessId;
    property ProcessName: TJwString read FProcessName;
    property ProcessMemUsage: DWORD read FProcessMemUsage;
    property ProcessVMSize: DWORD read FProcessVMSize;
    property Server: TJwString read GetServer;
//    property SidStr: TJwString read FSidStr;

    {@Name returns the token of the session.
     The returned value is cached and must not be freed!
     If the value cannot be obtained the return value is nil.
    }
    property Token : TJwSecurityToken read GetToken;

    {@Name returns the logged on User of the session.
    The returned value is cached and must not be freed!
    If the value cannot be obtained the return value is nil.
    }
    property UserSid : TJwSecurityID read GetUserSid;

    property Username: TJwString read FUsername;
    property WinStationName: TJwString read FWinStationname;

  end;

  { List Of TJwWTSProcess Objects }
  PJwWTSProcessList = ^TJwWTSProcessList;
  TJwWTSProcessList = class(TObjectList)
  protected
    FOwner: TJwTerminalServer;

    function GetItem(Index: Integer): TJwWTSProcess;
    procedure SetItem(Index: Integer; AProcess: TJwWTSProcess);
    procedure SetOwner(const Value: TJwTerminalServer);
  public
    function Add(AProcess: TJwWTSProcess): Integer;
    function IndexOf(AProcess: TJwWTSProcess): Integer;
    procedure Insert(Index: Integer; AProcess: TJwWTSProcess);
    property Items[Index: Integer]: TJwWTSProcess read GetItem write SetItem; default;
    property Owner: TJwTerminalServer read FOwner write SetOwner;
    function Remove(AProcess: TJwWTSProcess): Integer;
  end;

  TShadowState =
    (
     //@Name defines a none state
     ssNone,// = 0,
     //@Name ...
     ssShadowing,// = 1,
     //@Name ...
     ssBeingShadowed// = 2
  );
  TShadowMode = (
    smNoneAllowed, // = 0,
    smFullControlWithPermission,// = 1,
    smFullControlWithoutPermission,// = 2,
    smViewOnlyWithPermission,// = 3,
    smViewOnlyWithoutPermission// = 4
  );

  TJwWTSSessionShadow = class
  private
    FWinStationShadowInformation : TWinStationShadowInformation;
    FOwner : TJwWTSSession;
  protected
    function GetShadowState : TShadowState;
    function GetShadowMode : TShadowMode;
    procedure SetShadowMode(Const Value : TShadowMode);
    procedure UpdateShadowInformation(const Modify : Boolean);
  public
    constructor Create(AOwner : TJwWTSSession);
    property ShadowState : TShadowState read GetShadowState;
    property ShadowMode : TShadowMode read GetShadowMode write SetShadowMode;
  end;

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}

implementation
{$ENDIF SL_OMIT_SECTIONS}

type
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


constructor TJwTerminalServer.Create;
begin
  inherited Create;
  OutputDebugString('TJwTerminalServer.Create');
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
var
  EventFlag: DWORD;
begin
  // Close connection
  if Assigned(FEnumServersThread) then
  begin
    // Signal Termination to the thread
    FEnumServersThread.Terminate;
    // Wait a while, see if thread terminates
    if WaitForSingleObject(FEnumServersThread.Handle, 1000) = WAIT_TIMEOUT then
    begin
      // it didn't, so kill it (we don't want the user to wait forever)!
      // TSAdmin does it the same way...
      TerminateThread(FEnumServersThread.Handle, 0);
    end;

    FEnumServersThread.Wait;
  end;

  // Terminate the Event Thread before closing the connection.
  if Assigned(FTerminalServerEventThread) then
  begin
    // Terminate Event Thread
    FTerminalServerEventThread.Terminate;

    // unblock the waiter
    WTSWaitSystemEvent(FServerHandle, WTS_EVENT_FLUSH, EventFlag);

    // wait for the thread to finish
    FTerminalServerEventThread.WaitFor;

    // Free
    FreeAndNil(FTerminalServerEventThread);
  end;

  if Connected then
  begin
    Disconnect;
  end;

    // Free the SessionList
    FreeAndNil(FSessions);

    // Free the ProcessList
    FreeAndNil(FProcesses);

  // Free the Serverlist
    FreeAndNil(FServers);

  // Free the Serverlist
    FreeAndNil(FServerList);

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

procedure TJwTerminalServer.FireEvent(EventFlag: DWORD);
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
  pComputerName: TJwPChar;
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
  // Get and zero memory (
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
      Result := PWideCharToJwString(StrConnectState(WTSDisconnected, False));
    end;
  finally
    FreeMem(WinStationNamePtr);
  end;
end;

procedure TJwTerminalServer.OnEnumServersThreadTerminate(Sender: TObject);
begin
  // nil it!
  FEnumServersThread := nil;
  OutputDebugString('nil it!');
end;

procedure TJwTerminalServer.SetServer(const Value: TJwString);
begin
  FServer := Value;
  // Clear the computername variable (cache)
  FComputerName := '';
end;

function TJwTerminalServer.EnumerateProcesses: Boolean;
var Count: Integer;
  ProcessInfoPtr: PWINSTA_PROCESS_INFO_ARRAY;
  i: Integer;
  AProcess: TJwWTSProcess;
  strProcessName: TJwString;
  strUsername: TJwString;
  lpBuffer: PWideChar;
  DiffTime: TDiffTime;
//  strSid: TjwString;
begin
  ProcessInfoPtr := nil;
  Count := 0;

  FProcesses.Clear;

  if not Connected then
  begin
    Connect;
  end;

  ProcessInfoPtr := nil;

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
          strProcessName := JwUnicodeStringToJwString(ProcessName);

          if IsValidSid(pUserSid) then
          begin
            with TJwSecurityID.Create(pUserSid) do
            begin
              strUsername := GetCachedUserFromSid;
//              strSid := StringSID;
              Free;
            end;
          end;
        end;

        AProcess := TJwWTSProcess.Create(FProcesses, SessionId,
          ProcessId, strProcessName, strUsername);
        with AProcess do
        begin
          FProcesses.Add(AProcess);

//          FSidStr := strSid;
          // Calculate Process Age
          CalculateElapsedTime(@CreateTime, DiffTime);

            // Reserve Memory
            GetMem(lpBuffer, ELAPSED_TIME_STRING_LENGTH * SizeOf(WCHAR));
          try
            // Format Elapsed Time String
            ElapsedTimeStringSafe(@DiffTime, False, lpBuffer,
                ELAPSED_TIME_STRING_LENGTH);
            FProcessAge := (DiffTime.wDays * SECONDS_PER_DAY) +
              (DiffTime.wHours * SECONDS_PER_HOUR) +
              (DiffTime.wMinutes * SECONDS_PER_MINUTE);
            FProcessAgeStr := PWideCharToJwString(lpBuffer);
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
          FProcessCPUTime := UserTime.QuadPart + KernelTime.QuadPart;

          FProcessCPUTimeStr := CPUTime2Str(
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
  if ProcessInfoPtr <> nil then
  begin
    WinStationFreeGAPMemory(0, ProcessInfoPtr, Count);
  end;

  ProcessInfoPtr := nil;
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
    EnumerateServers('');
  end;

  // Return the serverlist
  //TODO: Warning: User can free returned list (on purpose) this can
  //lead to problems in EnumerateServers
  Result := FServers;
end;

function TJwTerminalServer.EnumerateSessions: boolean;
var SessionInfoPtr: {$IFDEF UNICODE}PJwWTSSessionInfoWArray;
  {$ELSE}PJwWTSSessionInfoAArray;{$ENDIF UNICODE}
  pCount: DWORD;
  i: integer;
  Res: Longbool;
  ASession: TJwWTSSession;
begin
  if not Connected then
  begin
    Connect;
  end;

  // Clear the sessionslist
  FSessions.Clear;

  Res :=
{$IFDEF UNICODE}
    WTSEnumerateSessionsW(FServerHandle, 0, 1, PWTS_SESSION_INFOW(SessionInfoPtr),
      pCount);
{$ELSE}
    WTSEnumerateSessions(FServerHandle, 0, 1, PWTS_SESSION_INFOA(SessionInfoPtr),
      pCount);
{$ENDIF UNICODE}


  if not Res then begin
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
      'EnumerateSessions', ClassName, RsUNTerminalServer, 923, True,
          'WTSEnumerateSessions', ['WTSEnumerateSessions']);
  end;

  // Add all sessions to the SessionList
  for i := 0 to pCount - 1 do
  begin
    ASession := TJwWTSSession.Create(FSessions, SessionInfoPtr^[i].SessionId,
      GetWinStationName(SessionInfoPtr^[i].SessionId),
      TWtsConnectStateClass(SessionInfoPtr^[i].State));
    FSessions.Add(ASession);
  end;

  // After enumerating we create an event thread to listen for session changes
  if FTerminalServerEventThread = nil then
  begin
    FTerminalServerEventThread := TJwWTSEventThread.Create(False, Self);
  end;


  WTSFreeMemory(SessionInfoPtr);
  SessionInfoPtr := nil;

  // Pass the result
  Result := Res;
end;

function TJwTerminalServer.EnumerateServers(ADomain: String): Boolean;
begin
  // Does the thread exist?
  if Assigned(FEnumServersThread) then
  begin
    OutputDebugString('thread is already assigned');
    Result := False;
  end
  else
  begin
    // Create the thread
    OutputDebugString('create thread');
    FEnumServersThread := TJwWTSEnumServersThread.Create(True, Self, ADomain);
    FEnumServersThread.OnTerminate := OnEnumServersThreadTerminate;
    FEnumServersThread.Resume;
    Result := True;
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
          'WTSOpenServer', ClassName, RsUNTerminalServer, 1000, True,
          'WTSOpenServer', ['WTSOpenServer', FServer]);
      end
      else
      begin
        FConnected := True;
      end;
    end;

  end;
end;

procedure TJwTerminalServer.Disconnect;
begin

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

function TJwTerminalServer.Shutdown(AShutdownFlag: DWORD): Boolean;
begin
  Result := WTSShutdownSystem(FServerHandle, AShutdownFlag);
end;

constructor TJwWTSEventThread.Create(CreateSuspended: Boolean;
  AOwner: TJwTerminalServer);
begin
  inherited Create(CreateSuspended, Format('%s (%s)', [ClassName, AOwner.Server]));
  FreeOnTerminate := False;

  OutputDebugString('creating wtsevent thread');

  FOwner := AOwner;
end;



procedure TJwWTSEventThread.Execute;
begin
  inherited Execute;  

  while not Terminated do
  begin
    OutputDebugString('Entering WTSWaitSystemEvent');
    if WTSWaitSystemEvent(FOwner.ServerHandle, WTS_EVENT_ALL, FEventFlag) then
    begin
      if FEventFlag > WTS_EVENT_FLUSH then
      begin
        // Wait some time to prevent duplicate event dispatch
        OutputDebugString('Dispatching');
        Synchronize(DispatchEvent);
      end;
    end
    else begin
      OutputDebugString(PChar(Format('WTSWaitSystemEvent, False: %s', [SysErrorMessage(GetLastError)])));
    end;
    Sleep(0);
  end;
end;

procedure TJwWTSEventThread.DispatchEvent;
begin
  if FEventFlag > WTS_EVENT_NONE then 
  begin
    FOwner.FireEvent(FEventFlag);
    FEventFlag := WTS_EVENT_NONE;
  end;
end;

constructor TJwWTSEnumServersThread.Create(CreateSuspended: Boolean;
  Owner: TJwTerminalServer; Domain: TJwString);
begin
  JwRaiseOnNilParameter(Owner, 'Owner','Create', ClassName, RsUNTerminalServer);

  OutputDebugString('Creating EnumServers thread');

  inherited Create(CreateSuspended);

  FTerminatedEvent := CreateEvent(nil, False, False, nil);
  FOwner := Owner;
  FDomain := Domain;
  FreeOnTerminate := True;
end;

procedure TJwWTSEnumServersThread.Execute;
type
  PWTS_SERVER_INFO = {$IFDEF UNICODE}PWTS_SERVER_INFOW{$ELSE}PWTS_SERVER_INFOA{$ENDIF UNICODE};
var ServerInfoPtr: PJwWtsServerInfoAArray;
  pCount: DWORD;
  i: DWORD;
begin
  OutputDebugString('thread is executing');
  // Clear the serverlist
  Synchronize(ClearServerList);

  ServerInfoPtr := nil;
  // Since we return to a Stringlist (which does not support unicode)
  // we only use WTSEnumerateServersA

  if {$IFDEF UNICODE}WTSEnumerateServersW{$ELSE}WTSEnumerateServersA{$ENDIF UNICODE}
   (TJwPChar(FDomain), 0, 1, PWTS_SERVER_INFO(ServerInfoPtr),
    pCount) then
  begin
    for i := 0 to pCount - 1 do
    begin
      // If the thread is terminated then leave the loop
      if Terminated then Break;
      FServer := ServerInfoPtr^[i].pServerName;
      Synchronize(AddToServerList);
    end;

    // Note that on failure of WTSEnumerateServers we don't produce an
    // exception but return an empty ServerList instead. This is by design

    // If we have not been terminated we fire the OnServersEnumerated Event
    if not Terminated then
    begin
      Synchronize(DispatchEvent);
    end;
  end;

  // Cleanup
  if ServerInfoPtr <> nil then
  begin
    WTSFreeMemory(ServerInfoPtr);
  end;

  // Signal Wait procedure that we are finished.
  SetEvent(FTerminatedEvent);
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


procedure TJwWTSEnumServersThread.Wait;
var Res: DWORD;
begin
  // we should wait only from the MainThreadId!
  if GetCurrentThreadID = MainThreadID then
  begin
    Res := WAIT_OBJECT_0+1;

    while (Res = WAIT_OBJECT_0+1) do
    begin
      // Wait for the thread to trigger the Terminated Event
      Res := WaitForSingleObject(FTerminatedEvent, INFINITE);
      OutputDebugString('WaitForSingleObject done');
    end;
  end;
end;

// Borland's WaitFor procedure contains a bug when using Waitfor in combination
// with FreeOnTerminate := True; During the loop in WaitFor the TThread object
// can be freed and its Handle invalidated.  When MsgWaitForMultipleObjects()
// is called again, it fails, and then a call to CheckThreadError() afterwards
// throws on EOSError exception with an error code of 6 and an error message of
//  "the handle is invalid". http://qc.borland.com/wc/qcmain.aspx?d=6080
// Therefore we override the WaitFor function and Create an Exception
function TJwWTSEnumServersThread.WaitFor: LongWord;
begin
  // Return error
  //Result := ERROR_NOT_SUPPORTED;

  raise EJwsclUnimplemented.CreateFmtWinCall(RsWinCallFailed,
    'WaitFor function is not supported, please use Wait instead', ClassName,
    RsUNTerminalServer, 1203, False,
    'WaitFor function is not supported, please use Wait instead',
    ['TJwWTSEnumServersThread.WaitFor']);
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

constructor TJwWTSSessionShadow.Create(AOwner : TJwWTSSession);
begin
  FOwner := AOwner;
end;

function TJwWTSSessionShadow.GetShadowMode;
begin
  UpdateShadowInformation(False);
  Result := TShadowMode(FWinStationShadowInformation.ShadowMode);
end;

function TJwWTSSessionShadow.GetShadowState : TShadowState;
begin
  UpdateShadowInformation(False);
  Result := TShadowState(FWinStationShadowInformation.CurrentShadowState);
end;

procedure TJwWTSSessionShadow.SetShadowMode(Const Value : TShadowMode);
begin
  FWinStationShadowInformation.ShadowMode := Ord(Value);
  UpdateShadowInformation(True);
end;

procedure TJwWTSSessionShadow.UpdateShadowInformation(const Modify : Boolean);
var
  ReturnedLength : DWORD;
begin
  if not Modify then
  begin
{    if not }WinStationQueryInformationW(FOwner.GetServerHandle, FOwner.SessionId,
     WinStationShadowInformation, @FWinStationShadowInformation,
     SizeOf(FWinstationShadowInformation), ReturnedLength);{ then
      raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
       'UpdateShadowInformation', ClassName, RsUNTerminalServer, 0, True,
       'WinStationQueryInformationW', ['WinStationQueryInformationW']);}
  end
  else
    if not WinStationSetInformationW(FOwner.GetServerHandle, FOwner.SessionId,
     WinStationShadowInformation, @FWinStationShadowInformation,
     SizeOf(FWinstationShadowInformation)) then
      raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
       'UpdateShadowInformation', ClassName, RsUNTerminalServer, 0, True,
       'WinStationSetInformationW', ['WinStationSetInformationW']);
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

function TJwTerminalServerList.FindByServer(const ServerName: WideString; const IgnoreCase: boolean = False): TJwTerminalServer;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    //if Items[i].Server = AServer then
    if JwCompareString(Items[i].Server,ServerName,IgnoreCase) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
  end;
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
  inherited SetItem(Index, ATerminalServer);
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

function TJwWTSSession.ProtocolTypeToStr(const AProtocolType: DWORD): TJwString;
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
begin
  ABuffer := nil;
{$IFDEF UNICODE}
    WTSQuerySessionInformationW(GetServerHandle, FSessionId, WTSInfoClass,
      ABuffer, dwBytesReturned);
{$ELSE}
    WTSQuerySessionInformationA(GetServerHandle, FSessionId, WTSInfoClass,
      ABuffer, dwBytesReturned);
{$ENDIF}
end;

function TJwWTSSession.GetSessionInfoStr(const WTSInfoClass: _WTS_INFO_CLASS):
  TJwString;
var
  aBuffer: Pointer;
begin
  result := '';
  GetSessionInfoPtr(WTSInfoClass, aBuffer);
  if ABuffer <> nil then
  begin
    Result := TJwString(TJwPChar(aBuffer));
    WTSFreeMemory(aBuffer);
  end;
end;

function TJwWTSSession.GetSessionInfoDWORD(const WTSInfoClass: _WTS_INFO_CLASS): DWORD;
var ABuffer: Pointer;
begin
  result := 0;
  GetSessionInfoPtr(WTSInfoClass, aBuffer);
  if ABuffer <> nil then
  begin
    Result := PDWord(ABuffer)^;
    WTSFreeMemory(ABuffer);
  end;
end;

function TJwWTSSession.GetServerHandle;
begin
  // The ServerHandle is stored in TJwTerminalServer
  //TODO: Owner = nil? or Owner.Owner = nil ?
  JwRaiseOnNilMemoryBlock(Owner, 'GetServerHandle', ClassName, RsUNTerminalServer);
  JwRaiseOnNilMemoryBlock(Owner.Owner, 'GetServerHandle', ClassName, RsUNTerminalServer);

  Result := Owner.Owner.FServerHandle;
end;

procedure TJwWTSSession.GetWinStationDriver;
var WinStationDriver: _WD_CONFIGW;
  dwReturnLength: DWORD;
begin
  FWdName := '';
  FWdFlag := 0;
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
        end
        else
          FCompressionRatio := '(inf)'; //infinite output
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
     // Store the IdleTime as elapsed seconds
      FIdleTime := 0;
    end
    else
    begin
      // Store the IdleTime as elapsed seconds
      FIdleTime := CalculateDiffTime(Int64(WinStationInfo.LastInputTime),
        Int64(WinStationInfo.CurrentTime));
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
//    FLogonTime := FileTime2DateTime(WinStationInfo.LogonTime);
    FLogonTime := Int64(WinStationInfo.LogonTime);
    FCurrentTime := FileTime2DateTime(WinStationInfo.CurrentTime);
  end;

end;

function TJwWTSSession.GetClientAddress: TJwString;
var ClientAddressPtr: PWtsClientAddress;
begin
  GetSessionInfoPtr(WTSClientAddress, Pointer(ClientAddressPtr));
  if ClientAddressPtr <> nil then
  begin
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

    // Cleanup
    WTSFreeMemory(ClientAddressPtr);
  end;
end;

procedure TJwWTSSession.GetClientDisplay;
var ClientDisplayPtr: PWtsClientDisplay;
begin
  GetSessionInfoPtr(WTSClientDisplay, Pointer(ClientDisplayPtr));
  if ClientDisplayPtr <> nil then
  begin
    FHorizontalResolution := ClientDisplayPtr^.HorizontalResolution;
    FVerticalResolution := ClientDisplayPtr^.VerticalResolution;
    FColorDepth := ClientDisplayPtr^.ColorDepth;
    // Cleanup
    WTSFreeMemory(ClientDisplayPtr);
  end;
end;


constructor TJwWTSSession.Create(const Owner: TJwWTSSessionList;
  const SessionId: TJwSessionId; const WinStationName: TJwString;
  const ConnectState: TWtsConnectStateClass);
var tempStr : String;
begin
  JwRaiseOnNilMemoryBlock(Owner, 'Create', ClassName, RsUNTerminalServer);
  JwRaiseOnNilMemoryBlock(Owner.Owner, 'Create', ClassName, RsUNTerminalServer);

  inherited Create;

  FOwner := Owner; // Session is owned by the SessionList
  // First store the SessionID
  FSessionId := SessionId;
  FShadow := TJwWTSSessionShadow.Create(Self); 
  FConnectState := ConnectState;
  FConnectStateStr := PWideCharToJwString(StrConnectState(FConnectState, False));
  FWinStationName := WinStationName;
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
  FDomain := GetSessionInfoStr(WTSDomainName);
  FUsername := GetSessionInfoStr(WTSUsername);
  // This retreives WinStationDriver info
  GetWinStationDriver;
  // Retreive WinStationInformation
  GetWinStationInformation;
  // This function queries Terminal Server for the real remote ip address
  // and port (as opposed to WTSClientAddress which retreives the client's
  // local ip address

  tempStr := String(FRemoteAddress);
  WinStationGetRemoteIPAddress(GetServerHandle, SessionId, tempStr,
    FRemotePort);

  FRemoteAddress := WideString(tempStr);

  FToken := nil;
  FUserSid := nil;
end;

destructor TJwWTSSession.Destroy;
begin
  FreeAndNil(FShadow);
  FreeAndNil(FToken);
  FreeAndNil(FUserSid);
end;

function TJwWTSSession.GetToken : TJwSecurityToken;
begin
  result := FToken;
  if Assigned(FToken) then
    exit;

  result := nil;

  //session on another Server? : CreateWTSQueryUserTokenEx
  try
    FToken := TJwSecurityToken.CreateWTSQueryUserToken(FSessionId);
  except
    on E : EJwsclOpenProcessTokenException do
      FToken := nil;
  end;

  result := FToken;
end;

function TJwWTSSession.GetUserSid : TJwSecurityID;
begin
  GetToken;

  result := FUserSid;

  if Assigned(FUserSid) then
    exit;

  if Assigned(FToken) then
  begin
    try
      FUserSid := FToken.GetTokenUser;
    except
      on E : EJwsclSecurityException do
        FUserSid := nil;
    end;
  end
  else
    FUserSid  := nil;

  result := FUserSid;
end;

function TJwWTSSession.GetServer: TJwString;
begin
  //TODO: Owner = nil? or Owner.Owner = nil ?
  JwRaiseOnNilMemoryBlock(Owner, 'GetServerName', ClassName, RsUNTerminalServer);
  JwRaiseOnNilMemoryBlock(Owner.Owner, 'GetServerName', ClassName, RsUNTerminalServer);

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
    PWideChar(WideString(GetServer)), FSessionId, VK_MULTIPLY,
    MOD_CONTROL);
end;

constructor TJwWTSProcess.Create(const Owner: TJwWTSProcessList;
  const SessionId: TJwSessionId; const ProcessID: TJwProcessId;
  const ProcessName: TJwString; const Username: TjwString);
begin
  JwRaiseOnNilParameter(Owner, 'Owner','TJwWTSProcess.Create', ClassName, RsUNTerminalServer);
  JwRaiseOnNilParameter(Owner.Owner, 'Owner.Owner','TJwWTSProcess.Create', ClassName, RsUNTerminalServer);

  inherited Create;

  FOwner := Owner;
  FSessionID := SessionId;

  FWinStationName := FOwner.Owner.GetWinStationName(SessionId);

  FProcessId := ProcessId;
  FProcessName := ProcessName;
  FUsername := Username;

  FToken := nil;
  FUserSid := nil;

end;

destructor TJwWTSProcess.Destroy;
begin
  FreeAndNil(FToken);
  FreeAndNil(FUserSid);
  inherited;
end;

function TJwWTSProcess.GetToken : TJwSecurityToken;
var hProc : HANDLE;
begin
  result := FToken;
  if Assigned(FToken) then
    exit;

  result := nil;
  JwEnablePrivilege(SE_DEBUG_NAME, pst_EnableIfAvail);

  SetLastError(0);
  hProc := OpenProcess(PROCESS_QUERY_INFORMATION, false, ProcessID);

  if hProc = 0 then
    exit;

  try
    FToken := TJwSecurityToken.CreateTokenByProcess(hProc, MAXIMUM_ALLOWED);
  except
    on E : EJwsclOpenProcessTokenException do
      FToken := nil;
  end;

  CloseHandle(hProc);

  result := FToken;
end;

function TJwWTSProcess.GetUserSid : TJwSecurityID;
begin
  GetToken;

  result := FUserSid;

  if Assigned(FUserSid) then
    exit;

  if Assigned(FToken) then
  begin
    try
      FUserSid := FToken.GetTokenUser;
    except
      on E : EJwsclSecurityException do
        FUserSid := nil;
    end;
  end
  else
    FUserSid  := nil;

  result := FUserSid;
end;

function TJwWTSProcess.GetServer;
begin
  JwRaiseOnNilMemoryBlock(Owner, 'GetServerName', ClassName, RsUNTerminalServer);
  JwRaiseOnNilMemoryBlock(Owner.Owner, 'GetServerName', ClassName, RsUNTerminalServer);

  // The Server is stored in TJwTerminalServer
  Result := Owner.Owner.Server;
end;

function TJwWTSProcess.GetServerHandle: THandle;
begin
  JwRaiseOnNilMemoryBlock(Owner, 'TJwWTSProcess.GetServerHandle', ClassName, RsUNTerminalServer);
  JwRaiseOnNilMemoryBlock(Owner.Owner, 'TJwWTSProcess.GetServerHandle', ClassName, RsUNTerminalServer);

  // The ServerHandle is stored in TJwTerminalServer
  Result := Owner.Owner.FServerHandle;
end;

function TjwWTSProcess.Terminate: Boolean;
begin
  Result := WTSTerminateProcess(GetServerHandle, ProcessId, 0);
end;

function TJwWTSProcess.Terminate(const dwExitCode: DWORD): Boolean;
begin
  Result := WTSTerminateProcess(GetServerHandle, ProcessId, dwExitCode);
end;

end.
