{@abstract(This unit provides access to Terminal Server api functions through it's
 key object TJwTerminalServer)
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
@br
br
The central object of the JwsclTerminalServer unit is the TJwTerminalServer
object. It represents a Terminal Server, the connection to this server and
holds the session- and processlist.

Some Key functions of TJwTerminalServer are:@br

@unorderedList(
  @item(TJwTerminalServer.EnumerateSessions enumerates all Terminal Server
  sessions into a TJwSessionList which can be accessed by the Sessions property.)
  @Item(TJwTerminalServer.EnumerateProcesses enumerates all Terminal Server
  processes into a TJwProcessList which can be accessed by the Processes property.)
  @Item(TJwTerminalServer.EnumerateServers enumerates all Terminal Servers in a
  domain.)
  @Item(TJwTerminalServer.Shutdown Shuts down and optionally restarts the specified
  Terminal Server.)
)
@br
TJwTerminalServer also offers Events to monitor Terminal Server activity such as
OnSessionConnect, OnSessionCreate, OnSessionLogon and OnSessionLogoff.@br
@br@br
A unique feature of TJwTerminalServer is that it's able to return detailled
information about Terminal Server, Sessions and Processes that is not available
using the normal Terminal Server API's or Microsoft Tools!@br
This includes detailled process memory usage information and extended session
information such as ShadowMode, ShadowState and Remote Address.@br
@br
The schema belows shows the relations between TJwTerminalServer,
the TJwWTSSessionList with TJwWTSSessions and the TJwWTSProcessList with
TjwWTSSessions.@br
@br
@image(.\..\documentation\TJwTerminalServer-Hierarchy.png)
}

{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclTerminalServer;
{$I Jwscl.inc}

interface

uses
  Classes, Contnrs, DateUtils, SysUtils,
  JwaWindows,
  JwsclExceptions, JwsclResource, JwsclSid, JwsclTypes,
  JwsclUtils, JwsclToken, JwsclVersion, JwsclStrings;

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

  TJwSessionsEnumerator = class;
  TJwProcessEnumerator = class;

  TJwOnProcessFound = procedure(const Sender: TJwTerminalServer;
    var Process: TJwWTSProcess; var Cancel: Boolean; Data: Pointer) of object;

  {@Name is a pointer to a TJwTerminalServer instance}
  PJwTerminalServer = ^TJwTerminalServer;
  {@Abstract(@Name is the central object of JwsclTerminalServer and holds the session- and processlist.)

   @Name offers connection to a Terminal Server which you can specify with the
   Server property. Key functions of @Name are:@br
   @unorderedList(
   @item(EnumerateSessions enumerates all Terminal Server sessions into a
   TJwSessionList which can be accessed by the Sessions property.)
   @Item(EnumerateProcesses enumerates all Terminal Server processes into a
   TJwProcessList which can be accessed by the Processes property.)
   @Item(EnumerateServers enumerates all Terminal Servers in a domain.)
   @Item(Shutdown Shuts down and optionally restarts the specified
   Terminal Server.)
   )
   @br
   @Name also offers Events to monitor Terminal Server activity such as
   OnSessionConnect, OnSessionCreate, OnSessionLogon and OnSessionLogoff.@br
   @br@br
   The schema belows shows the relations between TJwTerminalServer,
   the TJwWTSSessionList with TJwWTSSessions and the TJwWTSProcessList with
   TjwWTSSessions.@br
   @br
   @image(.\..\documentation\TJwTerminalServer-Hierarchy.png)
  }
  TJwTerminalServer = class(TObject)
  protected
    {@exclude}
    FComputerName: TJwString;
    {@exclude}
    FConnected: Boolean;
    {@exclude}
    FData: Pointer;
    {@exclude}
    FEnumServersThread: TJwWTSEnumServersThread;
    {@exclude}
    FIdleProcessName: TJwString;
    {@exclude}
    FLastEventFlag: DWORD;
    {@exclude}
    FOnServersEnumerated: TNotifyEvent;
    {@exclude}
    FOnSessionConnect: TNotifyEvent;
    {@exclude}
    FOnSessionCreate: TNotifyEvent;
    {@exclude}
    FOnSessionDelete: TNotifyEvent;
    {@exclude}
    FOnSessionDisconnect: TNotifyEvent;
    {@exclude}
    FOnSessionEvent: TNotifyEvent;
    {@exclude}
    FOnLicenseStateChange: TNotifyEvent;
    {@exclude}
    FOnSessionLogon: TNotifyEvent;
    {@exclude}
    FOnSessionLogoff: TNotifyEvent;
    {@exclude}
    FOnSessionStateChange: TNotifyEvent;
    {@exclude}
    FOnWinStationRename: TNotifyEvent;
    {@exclude}
    FServerHandle: THandle;
    {@exclude}
    FServers: TStringList;
    {@exclude}
    FSessions: TJwWTSSessionList;
    {@exclude}
    FProcesses: TJwWTSProcessList;
    {@exclude}
    FTerminalServerEventThread: TJwWTSEventThread;
    {@exclude}
    FServer: TJwString;
    {@exclude}
    FTag: Integer;

    {@exclude}
    function GetIdleProcessName: TJwString;
    {@exclude}
    function GetServers: TStringList;
    {@exclude}
    function GetServer: TJwString;
    {@exclude}
    function GetWinStationName(const SessionId: DWORD): TJwString;
    {@exclude}
    procedure OnEnumServersThreadTerminate(Sender: TObject);
    {@exclude}
    procedure SetServer(const Value: TJwString);
    {@exclude}
    procedure FireEvent(EventFlag: DWORD);

    procedure OnInternalProcessFound(const Sender: TJwTerminalServer;
      var Process: TJwWTSProcess; var Cancel: Boolean; Data: Pointer); virtual;
  public

    {@Name sets up the connection with the Terminal Server specified in the
     Server property.@br
     The Connected property can be used to check if we're already connected.
     @raises(EJwsclWinCallFailedException will be raised if the connection
     attempt was unsuccessfull)
     @br@br
     @bold(Remarks:) EnumerateSessions and EnumerateProcesses will automatically
     connect to the Terminal Server when needed.@br
     @br
     Example:
     @longcode(#
     var
       ATerminalServer: TJwTerminalServer;
       i: Integer;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TjwTerminalServer.Create;

       // Set servername (only in case of remote server)
       ATerminalServer.Server := 'TS001';

       // Remember that EnumerateProcesses will automatically connect to the
       // Terminal Server for you. The connect function raises an Exception
       // if the connection attempt was unsuccessfull, so better use try..except
       try
         Connect
       except
         on E: EJwsclWinCallFailedException do
         begin
           // Handle Exception here
         end;
       end;

       // Free Memory
       ATerminalServer.Free;
     end;
     #)
     }
    procedure Connect;

    {@Name allows storage of a pointer to user specific data and can be freely
     used.@br
     @br
     Example:
     @longcode(#
     var
       ATerminalServer: TJwTerminalServer;
       s: String;
     begin

       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TJwTerminalServer.Create;

       s: String;
       s := 'Remember this text';

       // Store pointer in Data property
       ATerminalServer.Data := PChar(s);

       s := '';
       ...

       // and retreive it!
       s := ATerminalServer.Data;

       // Don't forget to free!
       ATerminalServer.Free;

     end;
     #)
     }
    property Data: Pointer read FData write FData;

    {@Name will disconnect an existing connection to the Terminal Server.
     The Connected property can be used to check if we're already connected.
     @br@br
     @bold(Remarks:) If you disconnect you will not receive Session Events!
     @br
     Example:
     @longcode(#
     var
       ATerminalServer: TJwTerminalServer;
       i: Integer;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TjwTerminalServer.Create;

       // Set servername (only in case of remote server)
       ATerminalServer.Server := 'TS001';

       // Remember that EnumerateProcesses will automatically connect to the
       // Terminal Server for you. The connect function raises an Exception
       // if the connection attempt was unsuccessfull, so better use try..except
       try
         Connect
       except
         on E: EJwsclWinCallFailedException do
         begin
           // Handle Exception here
         end;
       end;

       ...

       // Disconnect
       ATerminalServer.Disconnect;

       // Free Memory (note that the free procedure will automatically
       // disconnect from the server if connected)
       ATerminalServer.Free;
     end;
    #)
     }
    procedure Disconnect;

    {@Name returns the local computername.@br
     This property is convenient if you are connecting to Terminal Server
     locally or want to check if a servername = computername.
    }
    property ComputerName: TJwString read FComputerName;

    {@Name indicates if we are connected to the Terminal Server
    }
    property Connected: Boolean read FConnected;

    {The @Name constructor creates a TJwTerminalServer instance and reservers
     memory for it.@br
     @br
     Example:
     @longcode(#
     var
       ATerminalServer: TJwTerminalServer;
       i: Integer;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TjwTerminalServer.Create;

       // Set servername (only in case of remote server)
       ATerminalServer.Server := 'TS001';

       // Remember that EnumerateProcesses will automatically connect to the
       // Terminal Server for you. The connect function raises an Exception
       // if the connection attempt was unsuccessfull, so better use try..except
       try
         if ATerminalServer.EnumerateProcesses then
         begin

           // Now loop through the list
           for i := 0 to ATerminalServer.Processes.Count - 1 do
           begin
             Memo1.Lines.Add(ATerminalServer.Processes[i].ProcessName);
           end;

         end;
       except
         on E: EJwsclWinCallFailedException do
         begin
           // Handle Exception here
         end;
       end;

       // Free Memory
       ATerminalServer.Free;
     end;
    #)
    }
    constructor Create;
    {@exclude}
    destructor Destroy; override;

    {@Name enumerates all processes on the Terminal Server and fills the
     Processes property with a TJwProcessList. This list contains all processes
     and their properties such as Process Name, Process Id, Username, Memory
     Usage and so on.@br
     @br
     Example:
     @longcode(#
     var
       ATerminalServer: TJwTerminalServer;
       i: Integer;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TjwTerminalServer.Create;

       // Set servername (only in case of remote server)
       ATerminalServer.Server := 'TS001';

       // Remember that EnumerateProcesses will automatically connect to the
       // Terminal Server for you. The connect function raises an Exception
       // if the connection attempt was unsuccessfull, so better use try..except
       try
         if ATerminalServer.EnumerateProcesses then
         begin

           // Now loop through the list
           for i := 0 to ATerminalServer.Processes.Count - 1 do
           begin
             Memo1.Lines.Add(ATerminalServer.Processes[i].ProcessName);
           end;

         end;
       except
         on E: EJwsclWinCallFailedException do
         begin
           // Handle Exception here
         end;
       end;

       // Free Memory
       ATerminalServer.Free;
     end;
    #)
    }
    function EnumerateProcesses: Boolean; overload;

    function EnumerateProcesses(const OnProcessFound : TJwOnProcessFound;
        Data : Pointer) : Boolean; overload;

    {@Name enumerates all Terminal Servers in the specified domain.
     @Param(ADomain name of the Domain to be queried, if empty string is
     specified the current domain is queried)
     @returns(If the function fails you can use GetLastError to get extended
     error information)@br@br
     @bold(Remarks:) This functions enumerates all Terminal Servers that
     advertise themselves on the network. By default only Terminal Servers in
     Application Mode advertise themselves. You can override this behaviour by
     modifying the following registry key:
     @longcode(#
     HKLM\SYSTEM\CurrentControlSet\Control\Terminal Server
     "TSAdvertise" = REG_DWORD:1
     #)
     @br
     @br@br
     Please note that enumerating Terminal Servers in large environments might
     take some time (especially over slow WAN links). Therefore this function
     runs in a seperate thread and signals the OnServersEnumerated Event.@br
     The enumerated servers can be retreived by reading the Servers property.@br
     @br
     If the TJwTerminalServer Instance is destroyed and the enumeration thread
     is still busy, the TJwTerminalServer will wait max. 1 second for the thread
     to finish and then terminates it.
    }
    function EnumerateServers(ADomain: String):Boolean;

    {@Name enumerates all sessions on the Terminal Server and fills the
     Sessions property with a TJwSessionList. This list contains all sessions
     and their properties such as Username, Session Id, Connection State, Idle
     Time and so on.@br
     @returns(If the function fails you can use GetLastError to get extended
     error information)@br
     @br
     Example:
     @longcode(#
     var
       ATerminalServer: TJwTerminalServer;
       i: Integer;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TjwTerminalServer.Create;

       // Set servername (only in case of remote server)
       ATerminalServer.Server := 'TS001';

       // Remember that EnumerateSessions will automatically connect to the
       // Terminal Server for you. The connect function raises an Exception
       // if the connection attempt was unsuccessfull, so better use try..except
       try
         if ATerminalServer.EnumerateSessions then
         begin

           // Now loop through the list
           for i := 0 to ATerminalServer.Sessions.Count - 1 do
           begin
             Memo1.Lines.Add(ATerminalServer.Sessions[i].Username);
           end;

         end;
       except
         on E: EJwsclWinCallFailedException do
         begin
           // Handle Exception here
         end;
       end;

       // Free Memory
       ATerminalServer.Free;
     end;
    #)
    }
    function EnumerateSessions: boolean;

    {@Name can be used to convert a non local (GMT time) FileTime to a
     localised TDateTime var.@br
     @param(FileTime TFileTime in GMT)
     @returns(TDateTime in local time)
     @br@br
     @bold(Remarks:) A TFileTime can be casted to Int64 (number 100-nanosecond
     intervals since January 1, 1601) and vice versa.
     }
    class function FileTime2DateTime(FileTime: TFileTime): TDateTime;
    {@exclude}
    property IdleProcessName: TJwString read GetIdleProcessName;

    {The @Name property can be used to see the Last Session Event that occured.
     This is usefull if you are listening on multiple event types.@br
     @seealso(OnSessionEvent)
    }
    property LastEventFlag: DWORD read FLastEventFlag;

    {The @Name event signals that the Server Enumeration thread has finished.@br
     The Enumerated Servers can be read through the Servers property.@br
     @br
     Example:
     @longcode(#
     procedure TMainForm.OnEnumerateServersDone(Sender: TObject);
     var
       i: Integer;
       TerminalServer: TJwTerminalServer;
      begin

        // Cast Sender to TJwTerminalServer
        TerminalServer := (Sender as TJwTerminalServer);

        // Loop through the enumerated Terminal Servers, if no servers were
        // found the count is 0.
        for i := 0 to TerminalServer.Servers.Count-1 do
        begin
          Memo1.Lines.Add(TerminalServer.Servers[i]);
        end;

        // Don't free TerminalServer var here!
      end;
     #)
    }
    property OnServersEnumerated: TNotifyEvent read FOnServersEnumerated write FOnServersEnumerated;

    {The @Name is a generic event which is fired if anything happens that is
     session related, like statechange, logon/logoff, disconnect and (re)connect.
     @br@br
     The table below shows which Terminal Server event triggers which event:@br
     @image(.\..\documentation\TJwWTSEvents-Table.png)
    }
    property OnSessionEvent: TNotifyEvent read FOnSessionEvent write FOnSessionEvent;

    {The @Name event is fired when a client connected to a session
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnSessionConnect: TNotifyEvent read FOnSessionConnect write FOnSessionConnect;

    {The @Name event is fired when a session is created
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnSessionCreate: TNotifyEvent read FOnSessionCreate write FOnSessionCreate;

    {The @Name event is fired when a session is deleted
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnSessionDelete: TNotifyEvent read FOnSessionDelete write FOnSessionDelete;

    {The @Name event is fired when a session is disconnected
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnSessionDisconnect: TNotifyEvent read FOnSessionDisconnect write FOnSessionDisconnect;

    {The @Name event is fired when when a license is added or deleted using
     License Manager.
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnLicenseStateChange: TNotifyEvent read FOnLicenseStateChange write FOnLicenseStateChange;

    {The @Name event is fired when a client logs on either through the console
     or a session
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnSessionLogon: TNotifyEvent read FOnSessionLogon write FOnSessionLogon;

    {The @Name event is fired when a client logs off either from the console
     or a session
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnSessionLogoff: TNotifyEvent read FOnSessionLogoff write FOnSessionLogoff;

    {The @Name event is fired when an existing session has been renamed
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnWinStationRename: TNotifyEvent read FOnWinStationRename write FOnWinStationRename;

    {The @Name event is fired when the connectstate of a session has changed
     @seealso(OnSessionEvent Overview of which events are triggered and when)
    }
    property OnSessionStateChange: TNotifyEvent read FOnSessionStateChange write FOnSessionStateChange;

    {@Name contains a TJwWTSProcessList of which each item contains a
     TJwWTSProcess. This processlist contains all enumerated processes
     and their properties such as Process Name, Process Id, Username, Memory
     Usage and so on.
     @br@br
     @bold(Remarks:) The Processlist is filled by calling the EnumerateProcesses
     function.
    }
    property Processes: TJwWTSProcessList read FProcesses write FProcesses;

    {@Name the netbios name of the Terminal Server.@br
     @br
     @bold(Remarks:) If you want to connect to a Terminal Server locally
     you should not specify the server name. Please note that in the case of a
     local connection this property @bold(will return the computername))
     @br@br
     Note that Windows XP SP 2 by default does not allow remote
     RPC connection to Terminal Server (enumerating sessions and processes).@br
     You can change this behaviour by creating the following registry entry
     on the XP machine:@br
     @br
     @longcode(#
     HKLM\SYSTEM\CurrentControlSet\Control\Terminal Server
     "AllowRemoteRPC" = REG_DWORD:1
     #)
     @br
     Also make sure that the Windows firewall on the client is configured to
     allow File and Print sharing, as well as Remote Desktop.
    }
    property Server: TJwString read GetServer write SetServer;
    {@exclude}
    property ServerHandle: THandle read FServerHandle;

    {@Name contains the list of Enumerated Terminal Servers
     @seealso(EnumerateServers)
     }
    property Servers: TStringList read GetServers;

    {@Name contains a TJwWTSSessionList of which each item contains a
     TJwWTSSession. This sessionlist contains all enumerated sessions
     and their properties such as Username, Connection State, Idle Time and so
     on.
     @br@br
     @bold(Remarks:) The Sessionlist is filled by calling the EnumerateSessions
     function.
    }
    property Sessions: TJwWTSSessionList read FSessions write FSessions;

    {@Name shuts down (and optionally restarts) the specified terminal server.@br
     @Param AShutdownFlag can be one of the following values:
     @table(
     @rowHead(  @cell(Value) @cell(Meaning))
      @row(     @cell(WTS_WSD_LOGOFF) @cell(Forces all client sessions to log off (except the session calling WTSShutdownSystem) and disables any subsequent remote logons. This can be used as a preliminary step before shutting down. Logons will be re-enabled when the terminal services service is restarted. Use this value only on the Terminal Services console.))
      @row(     @cell(WTS_WSD_POWEROFF) @cell(Shuts down the system on the terminal server and, on computers that support software control of AC power, turns off the power. This is equivalent to calling ExitWindowsEx with EWX_SHUTDOWN and EWX_POWEROFF. The calling process must have the SE_SHUTDOWN_NAME privilege enabled.))
      @row(     @cell(WTS_WSD_REBOOT) @cell(Shuts down and then restarts the system on the terminal server. This is equivalent to calling ExitWindowsEx with EWX_REBOOT. The calling process must have the SE_SHUTDOWN_NAME privilege enabled.))
      @row(     @cell(WTS_WSD_SHUTDOWN) @cell(Shuts down the system on the terminal server. This is equivalent to calling the ExitWindowsEx function with EWX_SHUTDOWN. The calling process must have the SE_SHUTDOWN_NAME privilege enabled.))
      @row(     @cell(WTS_WSD_FASTREBOOT) @cell(This value is not supported currently.))
      )@br
     @returns(If the function fails you can use GetLastError to get extended
     error information)@br@br
    }
    function Shutdown(AShutdownFlag: DWORD): Boolean;

    {@Name has no predefined meaning. The Tag property is provided for the
     convenience of developers. It can be used for storing an additional integer
     value or it can be typecast to any 32-bit value such as a component
     reference or a pointer.
    }
    property Tag: Integer read FTag write FTag;
  end;

  {@Name is a pointer to a TJwTerminalServerList}
  PJwTerminalServerList = ^TJwTerminalServerList;
  {@Abstract(@Name is a List of TJwTerminalServer Objects.)

   Each item in the list points to a TJwTerminalServer object that can be queried
   and manipulated.@br
   The list can be filled by adding TJwTerminalServer instances.@br
   Example:
   @longcode(#
   var
     ATerminalServerList : TjwTerminalServerList;
     ATerminalServer : TJwTerminalServer;
   begin
     ATerminalServerList := TjwTerminalServerList.Create;

     // Create a Terminal Server instance
     ATerminalServer := TJwTerminalServer.Create;

     // and add it to the list
     ATerminalServerList.Add(ATerminalServer);

     // Freeing the TerminalServerList will also free the Terminal Server
     // instances it owns.
     ATerminalServerList.Free;
   end;
   #)
  }
  TJwTerminalServerList = class(TObjectList)
  protected
    {@exclude}
    FOwner: TComponent;
  protected
    {@exclude}
    function GetItem(Index: Integer): TJwTerminalServer;
    {@exclude}
    procedure SetItem(Index: Integer; ATerminalServer: TJwTerminalServer);
    {@exclude}
    procedure SetOwner(const Value: TComponent);
  public
    {@exclude}
    destructor Destroy; reintroduce;

    {@Name adds a TerminalServer to the end of the TerminalServerList
     @returns(returns the index of the inserted object.)
    }
    function Add(ATerminalServer: TJwTerminalServer): Integer;
    {@Name looks up a Terminal Server in the List by Servername
     @Param(ServerName The Servername which is to be found)
     @Param(IgnoreCase Default = True)
     @Returns(If the server was found a TJwTerminalServer instance is returned.
     Always check if it's not nil!)
    }
    function FindByServer(const ServerName: WideString;
      const IgnoreCase: boolean = False): TJwTerminalServer;

    {@Returns(the index of the TerminalServer object in the TerminalServerList.)
    }
    function IndexOf(ATerminalServer: TJwTerminalServer): Integer;

    {@Name adds a TerminalServer to the end of the TerminalServerList
     @returns(returns the index of the inserted object.)
    }
    procedure Insert(Index: Integer; ATerminalServer: TJwTerminalServer);

    {The @Name property can be used to access the TerminalServer instances that
     are held by the list.
    }
    property Items[Index: Integer]: TJwTerminalServer read GetItem write SetItem; default;
    {A @ClassName can be owned by any component eg a TApplication or TForm
    }
    property Owner: TComponent read FOwner write SetOwner;

    {@Name removes the specified Terminal Server  from the TerminalServerList
     and if OwnsObjects is true (default) frees the TerminalServer.
     @returns(The value returned is the index of the object in the Items array
     before it was removed. If the specified object is not found on the list,
     Remove returns –1.)
    }
    function Remove(ATerminalServer: TJwTerminalServer): Integer;
  end;

   {@Abstract(The @Name Thread waits for Terminal Server Events and notifies the
    caller by firing Events.)

    It's not necessary to manually create an @Name Thread because
    TJwTerminalServer does this automatically after a successfull call to the
    EnumerateSessions function.@br
    @br
    @Name is Owned by a TJwTerminalServer instance
    @br@br
    @bold(Remarks:) @Name uses the WTSWaitSystemEvent API Call which can hang
    on Windows Vista after sending a WTS_FLUSH event. The bug was first
    corrected in winsta.dll version 6.0.6000.20664.@br
    @br
    See also: http://www.remkoweijnen.nl/blog/2008/01/25/using-wtswaitsystemevent/
  }
  TJwWTSEventThread = class(TJwThread)
  protected
    {@exclude}
    FOwner: TJwTerminalServer;
    {@exclude}
    FEventFlag: DWORD;
    {@exclude}
    procedure DispatchEvent;
  public

    {Call @Name to create a @classname Thread.
     @Param(CreateSuspended If CreateSuspended is False, Execute is called
     immediately. If CreateSuspended is True, Execute won't be called until
     after Resume is called.)
     @Param(Owner Specifies the TJwTerminalServer instance that owns the thread)
    }
    constructor Create(CreateSuspended: Boolean; AOwner: TJwTerminalServer);

    {@raises(EJwsclWinCallFailedException will be raised if WTSWaitSystemEvent
     failed.)
    }
    procedure Execute; override;
  end;

  {@Abstract(@Name is a Thread that enumerates all Terminal Server in the
   specified domain.)

   The Enumeration is done from a thread because it can take some time to
   enumerate all server, especially over a slow WAN connection.@br
   @br
   The thread is created by calling the EnumerateServers procedure from a
   TJwTerminalServer instance. Although allowed you normally don't create
   a TJwWTSEnumServersThread manually.
   @br@br
   Enumerated servers are returned by firing the OnServerEnumerated Event
   from the parent TJwTerminalServer instance.
   @br@br
   A TJwWTSEnumServersThread is owned by a TJwTerminalServer instance.
   }
  TJwWTSEnumServersThread = class(TJwThread)
  protected
    {@exclude}
    FDomain: TJwString;
    {@exclude}
    FOwner: TJwTerminalServer;
    {@exclude}
    FServer: TJwString;
    {@exclude}
    FTerminatedEvent: THandle;
    {@exclude}
    procedure AddToServerList;
    {@exclude}
    procedure ClearServerList;
    {@exclude}
    procedure DispatchEvent;
  public
    {Call @Name to create a @classname Thread.
     @Param(CreateSuspended If CreateSuspended is False, Execute is called
     immediately. If CreateSuspended is True, Execute won't be called until
     after Resume is called.)
     @Param(Owner Specifies the TJwTerminalServer instance that owns the thread)
     @Param(Domain Specifies the Domain that should be Enumerated. if you want
     to Enumerate the current domain (from a domain member) you can specify an
     empty string)
    }
    constructor Create(CreateSuspended: Boolean; Owner: TJwTerminalServer;
      Domain: TJwString);
    procedure Execute; override;
  end;

  {@Name is a pointer to a TJwWTSSession}
  PJwWTSSession = ^TJwWTSSession;

  {@abstract(@Name is a Class that encapsulates a Terminal Server session and
   it's properties)

   A session is uniquely identified with a SessionID, this is a number
   between 0 and 65535.@br
   @br
   A TJwWTSSession is owned by a JTwWTSSessionList.
   }
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
    {The @Name constructor creates a TJwWTSSession instance and allocates memory for it
     @Param(Owner Specifies the TJwTerminalServer instance that owns the session)
     @Param(SessionId The Session Identifier)
     @Param(WinStationName The Session Name)
     @Param(ConnectState The current connection state of the session)
     @br@br
     @bold(Remarks:) It's not necessary to manually create a session instance.
     Enumerating sessions with the EnumerateSessions function will create a
     SessionList filled with Sessions.
     @seealso(TJwTerminalServer.EnumerateSessions)
    }
    constructor Create(const Owner: TJwWTSSessionList;
      const SessionId: TJwSessionId; const WinStationName: TJwString;
      const ConnectState: TWtsConnectStateClass);

    {The @Name destructor disposes the Session object.
     @br@br
     @bold(Remarks:) Since a session is Owned by a SessionList by default
     @bold(you should not destroy/free a session manually). The only scenario
     where a sessions would need to be freed is when you manually create a
     sessionlist and specify False for the OwnsObject parameter.
    }
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
     following values:@br@br
     @table(
     @rowHead(  @cell(Session State) @cell(Description))
      @row(     @cell(WTSActive) @cell(The session is connected, and a user is logged on to the server.))
      @row(     @cell(WTSConnected) @cell(The session is connected, but there is no user logged on to the server.))
      @row(     @cell(WTSConnectQuery) @cell(The session is in the process of connecting. If this state continues, it indicates a problem with the connection.))
      @row(     @cell(WTSShadow) @cell(The session is in the process of remotely controlling another session.))
      @row(     @cell(WTSDisconnected) @cell(The user is disconnected from the session, but the session is still attached to the server and can be reconnected at any time.))
      @row(     @cell(WTSIdle) @cell(The session is initialized and ready to accept a connection. To optimize the performance of a server, two default (idle) sessions are initialized before any client connections are made.))
      @row(     @cell(WTSReset) @cell(The session failed to initialize correctly or could not be terminated, and is not available. If this state continues, it indicates a problem with the connection of the session.))
      @row(     @cell(WTSInit) @cell(The session is in the process of initializing.))
      )
     @br@br
     @bold(Remarks:) On Windows XP, however, the state for session 0 can be
     misleading because it will be WTSDisconnected even if there is no user
     logged on. To accurately determine if a user has logged on to session 0,
     you can use the Username property
    @br
    http://support.microsoft.com/kb/307642/en-us
    }
    property ConnectState: TWtsConnectStateClass read FConnectState;
    {@Name returns a localised connection state string.
     }
    property ConnectStateStr: TJwString read FConnectStateStr;

    {@Name The most recent client connection time.
     }
    property ConnectTime: TDateTime read FConnectTime;

    {@Name the time that the TJwWTSSession info was queried. This can be
     used to calculate time differences such as idle time
     }
    property CurrentTime: TDateTime read FCurrentTime;

    {The @Name function disconnects the logged-on user from the specified
     Terminal Services session without closing the session. The session remains
     attached to the terminal server in the disconnected state and currently
     running applications continue to run. When you attempt to reconnect to the
     @bold( same server), you are reconnected to the same session from which you
     disconnected, even if you are reconnecting from a different computer.
     Applications that were left open when you disconnected remain running when
     you reconnect to the session, with no loss of data.@br@br
     In NLB (Network Load Balancing) environments the Session Directory
     (starting from Server 2008 this is called TS Session Broker) care of
     redirecting a user to the server where he has a disconnected session.@br@br
     @param(bWait Indicates whether the operation is synchronous. Specify TRUE
     to wait for the operation to complete, or FALSE to return immediately.)
     @returns(If the function fails you can use GetLastError to get extended
     error information)
     }
    function Disconnect(bWait: Boolean): Boolean;

    {@Name the last client disconnection time.
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

    {@Name the elapsed time (relative to CurrentTime) since last user input in
     the session expressed in the number of 100-nanosecond intervals since
     January 1, 1601 (TFileTime).
     @br@br
     @bold(Remarks:) Please note the following remarks about Idle Time:@br
     A disconnected session is Idle since DisconnectTime. A session without a
     user is never idle, usually these are special sessions like Listener,
     Services or console session.@br
     IdleTimeStr returns a convenient formatted idle time string
     which can be used for displaying. This value is more convenient however for
     calculations such as sorting or comparing idle times.
     @seealso(IdleTimeStr)
    }
    property IdleTime: Int64 read FIdleTime;

    {@Name the elapsed time (relative to CurrentTime) since last user input in
     the session as formatted string. The string is formatted according to the
     table below:
     @table(
     @rowHead(  @cell(days) @cell(hours) @cell(minutes) @cell(value))
      @row(     @cell(> 0)  @cell(any)   @cell(any)     @cell(+d+hh:mm))
      @row(     @cell(0)    @cell(>0)    @cell(any)     @cell(hh:mm))
      @row(     @cell(0)    @cell(0)     @cell(any)     @cell(mm))
      @row(     @cell(0)    @cell(0)     @cell(0)       @cell(.))
      )
     @br@br
     @bold(Remarks:) Please note the following remarks about Idle Time:@br
     A disconnected session is Idle since DisconnectTime. A session without a
     user is never idle, usually these are special sessions like Listener,
     Services or console session.
     @seealso(IdleTimeStr)
     @seealso(CurrentTime)
    }
    property IdleTimeStr: TJwString read FIdleTimeStr;

    {@Name uncompressed Remote Desktop Protocol (RDP) data from the client
     to the server.
     @bold(Remarks:) This value is not returned for console sessions.
     @seealso(OutgoingBytes)
     @seealso(CompressionRatio)
    }
    property IncomingBytes: DWORD read FIncomingBytes;

    {@Name string containing the name of the initial program that
     Terminal Services runs when the user logs on.
    }
    property InitialProgram: TJwString read FInitialProgram;

    {@Name the time of the last user input in the session.
    }
    property LastInputTime: TDateTime read FLastInputTime;

    {The @Name function logs off a specified Terminal Services session
     @param(bWait Indicates whether the operation is synchronous. Specify TRUE
     to wait for the operation to complete, or FALSE to return immediately.)
     @returns(If the function fails you can use GetLastError to get extended
     error information)
     }
    function Logoff(bWait: Boolean): Boolean;

    {@Name the time that the user logged on to the session in the number
     of 100-nanosecond intervals since January 1, 1601 (TFileTime).
     @seealso(LogonTimeStr)
    }
    property LogonTime: Int64 read FLogonTime;

    {@Name the time that the user logged on to the session as a localised
     Date Time string.
     @seealso(LogonTime)
    }
    property LogonTimeStr: TJwString read FLogonTimeStr;

    {@Name of this session object, which can only be a TJwWTSSessionList
    }
    property Owner: TJwWTSSessionList read FOwner write FOwner;
    {@Name uncompressed RDP data from the server to the client.
     @br@br
     @bold(Remarks:) This value is not returned for console sessions.
     @seealso(IncomingBytes)
     @seealso(CompressionRatio)
    }
    property OutgoingBytes: DWORD read FOutgoingBytes;

    {The @Name function displays a message box on the client desktop.
     @param(AMessage: string that contains the message to be displayed)
     @param(ACaption: string that contains the dialog box title.)
     @param(uType: Specifies the contents and behavior of the message box.
     This value is typically MB_OK. For a complete list of values, see the
     uType parameter of the MessageBox function.)
     @br
     @returns(If the function fails you can use GetLastError to get extended
     error information)
     @br@br
     @bold(Remarks:) PostMessage does not wait for the user to respond.
     @seealso(SendMessage)
    }
    function PostMessage(const AMessage: TJwString; const ACaption: TJwString;
      const uType: DWORD): DWORD;

    {@exclude}
    function ProtocolTypeToStr(const AProtocolType: DWORD): TJwString;

    {@Name returns the real IP Address that is connected to the Terminal Server.
     @br@br
     @bold(Remarks:) @Name returns the IP address that is actually connected
     to the Terminal Server (as opposed to ClientAddress which returns the
     address as reported by the client which is usually just it's local ip
     address).@br
     @Name is the same adddress you will see when you examine netstat output@br
     @longcode(#
     C:\Documents and Settings\Remko>netstat -n | find /i "3389"
     TCP    192.168.2.2:3389       192.168.2.3:4096       ESTABLISHED
     #)
     In the output above, 192.168.2.2 is the IP Address of the Terminal Server
     which listens on port 3389. It has currently one Session from Remote IP
     192.168.2.3 on TCP port 4096. The @Name property is usefull because netstat
     cannot relate a connection to a Session Id.@br
     If you want to convert the to IP Address to a sockaddr structure you can
     use the WSAStringToAddress API.
     @seealso(RemotePort)
    }
    property RemoteAddress: TJwString read FRemoteAddress;

    {@Name returns the Remote Port number which is is connected to the
     Terminal Server. The Terminal Server listens (by default) on port 3389
     but the client connects with a random available port.
     @Name is the same port number you will see when you examine netstat output@br
     @longcode(#
     C:\Documents and Settings\Remko>netstat -n | find /i "3389"
     TCP    192.168.2.2:3389       192.168.2.3:4096       ESTABLISHED
     #)
     In the output above, 192.168.2.2 is the IP Address of the Terminal Server
     which listens on port 3389. It has currently one Session from Remote IP
     192.168.2.3 on TCP port 4096. The RemoteAddress and @Name properties are
     usefull because netstat cannot relate a connection to a Session Id.@br
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
     @returns(If the function fails you can use GetLastError to get extended
     error information)
     @br@br
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

    {@Name the session identifier@br
     @br@br
     There are some reserved SessionId's that serve a special purpose. The
     following table lists the reserved SessionId's:@br
     @br
     @table(
     @rowHead(  @cell(Value) @cell(Meaning))
      @row(     @cell(0) @cell(Console or Services session, see remarks))
      @row(     @cell(65536) @cell(RDP Listener))
      @row(     @cell(65537) @cell(ICA Listener))
      )
      @br@br
      @bold(Remarks:)@br
      @bold(Console Sessions)@br
      The system console session is usually identified as session 0 in the
      Session list when you connect to a terminal server. A console session is
      defined as the session you connect to at the physical console of the
      remote computer, as though you were logging on locally instead of
      remotely. You can send a message to the console session, but you cannot
      perform any of the other administrative actions on it.
      In Windows XP, Microsoft Windows Server 2003, and earlier versions of the
      Windows operating system, all services run in the same session as the
      first user who logs on to the console. This session is called Session 0.
      Running services and user applications together in Session 0 poses a
      security risk because services run at elevated privilege and therefore are
      targets for malicious agents who are looking for a way to elevate their
      own privilege level. The Vista operating system mitigates this security
      risk by isolating services in Session 0 and making Session 0
      noninteractive. In Windows Vista, only system processes and services run
      in Session 0. The first user logs on to Session 1, and subsequent users
      log on to subsequent sessions. This means that services never run in the
      same session as users’ applications and are therefore protected from
      attacks that originate in application code.@br
      @br
      @bold(Listener Sessions)@br
      Listener sessions are different from regular sessions.
      These sessions listen for and accept new Remote Desktop Protocol (RDP)
      client connections, thereby creating new sessions for the client requests.
      If you have configured more than one connection in Terminal Services
      Configuration, several listener sessions are available.@br
      You have the option to reset a listener session. However, this is not
      recommended, because doing so @bold(resets all sessions that use the same
      Terminal Services connection.) Resetting a user's session without warning
      can result in loss of data at the client.@br
      @br
      @bold(Idle sessions)@br
      To optimize the performance of a terminal server, idle sessions are
      initialized by the server before client connections are made.
      These sessions are available to clients for connection.
      Two idle sessions are created by default. User sessions can also be in
      idle state.
    }
    property SessionId: TJwSessionId read FSessionId;
    {The @Name function starts the remote control of another Terminal Services
     session. You must call this function from a remote session.
     @Return(If the function fails, the return value is zero. To get extended
     error information, call GetLastError)
     @br@br
     @bold(Remarks:) By default the console session cannot be shadowed. You can
     change this by modifying the following registry keys:
     @longcode(#
     HKLM\SYSTEM\CurrentControlSet\Control\Terminal Server\WinStations\Console
     "fInheritShadow" = REG_DWORD:1
     "Shadow" = REG_DWORD:1
     #)
     Where Shadow can be one of the TShadowMode values.
     @seealso(ShadowInformation)
     @seealso(TShadowMode)
     @seealso(TShadowState)
    }
    function Shadow: boolean;

    {@Name returns information about the Shadow State and Shadow Mode of
    a session.
    @br
    Shadow State shows if the session is shadowing another session or is being
    shadowed by another session.
    @br@br
    Shadow Mode queries the shadow permissions for this session.
    @seealso(Shadow)
    @seealso(TShadowMode)
    @seealso(TShadowState)
    }
    property ShadowInformation: TJwWTSSessionShadow read FShadow;

    {@Name the name of the user associated with the session.
    }
    property Username: TJwString read FUsername;

    property VerticalResolution: DWORD read FVerticalResolution;

    {WinStationDriver Flag (@Name) returns a value indicating the protocol and
     connection type. It's usefull for easy determination of console session.
     Possible values:
     @table(
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
     @table(
     @rowHead(  @cell(Operating System) @cell(Value))
      @row(     @cell(Windows 2000) @cell(Microsoft RDP 5.0))
      @row(     @cell(Windows XP) @cell(Microsoft RDP 5.1))
      @row(     @cell(Windows 2003) @cell(Microsoft RDP 5.2))
      @row(     @cell(Windows 2008/Vista) @cell(Microsoft RDP 6.0))
      )
     Known Citrix values:
     @table(
     @rowHead(  @cell(Version) @cell(Value))
      @row(     @cell(Citrix Presentation Server 4) @cell(Citrix ICA 3.0))
      )

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

  {@Name is a pointer to a TJwWTSSessionList}
  PJwWTSSessionList = ^TJwWTSSessionList;
  {@Abstract(@Name is a List of all Sessions running on the Terminal Server
   and their properties)

   Each item in the list points to a TJwWTSSession object that can be queried
   and manipulated.@br
   The list is filled by calling the EnumerateSessions function of the owning
   TJwTerminalServer instance.@br
   @br
   Example:
   @longcode(#
   var
     ATerminalServer: TJwTerminalServer;
     i: Integer;
   begin
     // Create Terminal Server instance and allocate memory for it
     ATerminalServer := TjwTerminalServer.Create;

     // Set servername (only in case of remote server)
     ATerminalServer.Server := 'TS001';

     // Remember that EnumerateSessions will automatically connect to the
     // Terminal Server for you. The connect function raises an Exception
     // if the connection attempt was unsuccessfull, so better use try..except
     try
       if ATerminalServer.EnumerateSessions then
       begin
         for i := 0 to ATerminalServer.Sessions.Count-1 do
         begin
           Memo1.Lines.Add(ATerminalServer.Sessions[i].Username);
         end;
       end;
     except
       on E: EJwsclWinCallFailedException do
       begin
        // Handle Exception here
       end;
     end;

     // Free Memory
     ATerminalServer.Free;
   end;
   #)
  }
  TJwWTSSessionList = class(TObjectList)
  protected
    {@exclude}
    FOwner: TJwTerminalServer;
    {@exclude}
    function GetItem(Index: Integer): TJwWTSSession;
    {@exclude}
    procedure SetItem(Index: Integer; ASession: TJwWTSSession);
    {@exclude}
    procedure SetOwner(const Value: TJwTerminalServer);
  public

    {The @Name destructor destroys the @Classname instance.@br
     Note that it is not necessary to manually free a @Classname as it will be
     freed when the TJwTerminalServer instance that owns the @Classname
     (Owner property) is freed.
     @br@br
     @bold(Remarks:) If you free a @Classname be sure to also set it to nil to
     prevent the Owner to free it as well (which would produce an Access
     Violation).
    }
    destructor Destroy; reintroduce;

    {@Name adds a Session to the end of the Sessionlist
     @returns(returns the index of the inserted object.)
    }
    function Add(ASession: TJwWTSSession): Integer;


    {(@Name returns an enumerator that can be used to iterate through
     the image list collection with Delphi's for in loop (Delphi 2005 and
     higher).@br
     @br
     @longcode(#
     var
       ATerminalServer: TJwTerminalServer;
       Session: TJwWTSSession;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TJwTerminalServer.Create;

       // It's recommended to wrap the Connect call in a try..except block since
       // a connection failure will raise an exception.
       try
         ATerminalServer.Connect;
       except
         on E: EJwsclWinCallFailedException do
         begin
           // Handle Exception here
         end;
       end;

       // EnumerateSessions might fail and return false, so always check the
       // result.
       if ATerminalServer.EnumerateSessions then
       begin

         // Loop through the Sessions with the for..in loop (needs Delphi 2005
         // or higher).
         for Session in ATerminalServer.Sessions do
         begin
           Memo1.Lines.Add(Format('User %s has session %d, the session state is %s',
             [Session.Username, Session.SessionId, Session.ConnectStateStr]));
         end;

       end;

       // Free Memory (note that free will automatically disconnect an active
       // connection)

       ATerminalServer.Free;

     end;
     #)
    }
    function GetEnumerator: TJwSessionsEnumerator;


    {@Returns(the index of the Session object in the SessionList.)
    }
    function IndexOf(ASession: TJwWTSSession): Integer;

    {@Name Adds an object to the list at a specified position
    }
    procedure Insert(Index: Integer; ASession: TJwWTSSession);

    {The @Name properties gives access to a Session and it's properties@br
     Example:
     @longcode(#
     var
       ATerminalServer: TJwTerminalServer;
       i: Integer;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TjwTerminalServer.Create;

       // Set servername (only in case of remote server)
       ATerminalServer.Server := 'TS001';

       // Remember that EnumerateSessions will automatically connect to the
       // Terminal Server for you. The connect function raises an Exception
       // if the connection attempt was unsuccessfull, so better use try..except
       try
         if ATerminalServer.EnumerateSessions then
         begin
           for i := 0 to ATerminalServer.Sessions.Count-1 do
           begin
             Memo1.Lines.Add(ATerminalServer.Sessions[i].Username);
           end;
         end;
       except
         on E: EJwsclWinCallFailedException do
         begin
          // Handle Exception here
         end;
       end;

       // Free Memory
       ATerminalServer.Free;
     end;
     #)
    }
    property Items[Index: Integer]: TJwWTSSession read GetItem write SetItem; default;

    {@Name Specifies the TJwTerminalServer instance that owns the @Classname
    }
    property Owner: TJwTerminalServer read FOwner write SetOwner;

    {@Name removes the specified Session from the SessionList and if
     OwnsObjects is true (default) frees the Session.
     @returns(The value returned is the index of the object in the Items array
     before it was removed. If the specified object is not found on the list,
     Remove returns –1.)
    }
    function Remove(ASession: TJwWTSSession): Integer;
  end;

  {@Abstract(@Name is the class that encapsulates a process that is running on
   a Terminal Server.)

   A process is uniquely identified by the Process Id (PID) in combination with
   it's Creation Time (the OS reused PID's).@br
   @br@br
   A @Name is owned by a TJwWTSProcessList.
   }
  TJwWTSProcess = class(TObject)
  protected
    {@exclude}
    FOwner: TJwWTSProcessList;
    {@exclude}
    FProcessAge: Int64;
    {@exclude}
    FProcessAgeStr: TJwString;
    {@exclude}
    FProcessCreateTime: TJwString;
    {@exclude}
    FProcessCPUTime: Int64;
    {@exclude}
    FProcessCPUTimeStr: TJwString;
    {@exclude}
    FProcessId: TJwProcessID;
    {@exclude}
    FProcessMemUsage: DWORD;
    {@exclude}
    FProcessName: TJwString;
    {@exclude}
    FProcessVMSize: DWORD;
    {@exclude}
    FSessionId: TJwSessionID;
    {@exclude}
    FUsername: TJwString;
    {@exclude}
    FWinStationName: TJwString;
    {@exclude}
    FToken : TJwSecurityToken;
    {@exclude}
    FUserSid : TJwSecurityID;
    {@exclude}
    function GetServer: TJwString;
    {@exclude}
    function GetToken : TJwSecurityToken;
    {@exclude}
    function GetUserSid : TJwSecurityID;
    function GetServerHandle: THandle; virtual;
  public
    {@Name create a TJwWTSProcess instance.
     @Param(Owner Specifies the TJwTerminalServer instance that owns the process.)
     @Param(SessionId The Session Identifier.)
     @Param(ProcessId The Process Identifier.)
     @Param(ProcessName The Process Name.)
    {@Param(Username the name of the user associated with the process.)
    }
    constructor Create(const Owner: TJwWTSProcessList;
      const SessionId: TJwSessionId; const ProcessID: TJwProcessId;
      const ProcessName: TJwString; const Username: TJwString);
    destructor Destroy; override;
  public
    {The @Name function terminates the specified process on the specified
     terminal server.
    }
    function Terminate: boolean; overload;
    {The @Name function terminates the specified process on the specified
     terminal server.
     @Param(dwExitCode Specifies the exit code for the terminated process.)
    }
    function Terminate(const dwExitCode: DWORD): boolean; overload;
  public
   {@Name specifies the TJwTerminalServer instance that owns the session)
    }
    property Owner: TJwWTSProcessList read FOwner write FOwner;
    {@Name the session identifier
    }
    property SessionId: TJwSessionId read FSessionId;
    {@Name the elapsed time since the process was created in
     100-nanosecond intervals since January 1, 1601 (TFileTime).
     @seealso(ProcessAgeStr)
    }
    property ProcessAge: Int64 read FProcessAge;
    {@Name the elapsed time since the process was created as formatted
     string. The string is formatted according to the table below:
     @table(
     @rowHead(  @cell(days) @cell(hours) @cell(minutes) @cell(value))
      @row(     @cell(> 0)  @cell(any)   @cell(any)     @cell(+d+hh:mm))
      @row(     @cell(0)    @cell(>0)    @cell(any)     @cell(hh:mm))
      @row(     @cell(0)    @cell(0)     @cell(any)     @cell(mm))
      @row(     @cell(0)    @cell(0)     @cell(0)       @cell(.))
      )
     @seealso(ProcessAge)
    }
    property ProcessAgeStr: TJwString read FProcessAgeStr;

    {@Name the total CPU Time (Usertime + Kerneltime) for the given process
     in 100-nanosecond intervals since January 1, 1601 (TFileTime).@br
     @br@br
     @bold(Remarks:) This value matches the CPU Time column in Task Manager.
     @seealso(ProcessCPUTimeStr)
    }
    property ProcessCPUTime: Int64 read FProcessCPUTime;

    {@Name the total CPU Time (Usertime + Kerneltime) for the given process
     as formatted string. (On Delphi 7 and higher this is a localised string
     for older version it is fixed at hh:mm)@br
     @br@br
     @bold(Remarks:) This value matches the CPU Time column in Task Manager.
     @seealso(ProcessCPUTime)
    }
    property ProcessCPUTimeStr: TJwString read FProcessCPUTimeStr;

    {@Name the Process Creation Time formatted as localised string.
    }
    property ProcessCreateTime: TJwString read FProcessCreateTime;

    {@Name the Process Identifier or PID
    }
    property ProcessId: TJwProcessId read FProcessId;

    {@Name the Process Name@br
     @br
     @bold(Remarks:) Windows XP (at least SP2) has the following bug:@br
     The Process Name is cut off at 18 characters for process on the local
     machine and at 15 characters for remote servers (even if the remote server
     is Windows Server 2003 which does not suffer from this bug).@br

    }
    property ProcessName: TJwString read FProcessName;

    {@Name the Amount of memory in Bytes used by the process
     @br@br
     @bold(Remarks:) This value matches the Mem Usage column in Task Manager.
    }
    property ProcessMemUsage: DWORD read FProcessMemUsage;

    {@Name the Amount of Virtual memory in Bytes used by the process
     @br@br
     @bold(Remarks:) This value matches the VM Size column in Task Manager.
    }
    property ProcessVMSize: DWORD read FProcessVMSize;

    {@Name the netbios name of the Terminal Server.
     @br@br
     @bold(Remarks:) If you want to connect to a Terminal Server locally
     you should not specify the server name. Please note that in the case of a
     local connection this property @bold(will return the computername))
    }
    property Server: TJwString read GetServer;

    {@Name returns the token of the session.
     The returned value is cached and must not be freed!
     If the value cannot be obtained the return value is nil.
     @br@br
     @bold(Remarks:) In order to obtain the Token the SE_DEBUG_NAME privilege
     is enabled, if this fails EJwsclPrivilegeException will be raised.
    }
    property Token : TJwSecurityToken read GetToken;

    {@Name returns a JwsclSid.TJwSecurityID instance pointing to the SID of the
     user that is associated with the process.@br
     @br
     @bold(Remarks:) The returned value is cached and must not be freed!
     If the value cannot be obtained the return value is nil.
    }
    property UserSid : TJwSecurityID read GetUserSid;

    {@Name the name of the user associated with the process.
    }
    property Username: TJwString read FUsername;

    {@Name returns the session name.
     @br@br
     @bold(Remarks:) Despite its name, specifying this property does not return
     the window station name. Rather, it returns the name of the Terminal
     Services session.@br
     For RDP this will be something like RDP-Tcp#023@br
     For ICA this will be something like ICA-tcp#014
    }
    property WinStationName: TJwString read FWinStationname;
  end;

  {@Name is a pointer to a TJwWTSProcessList}
  PJwWTSProcessList = ^TJwWTSProcessList;

  {@Abstract(@Name is a List of all Processes running on the Terminal Server
   and their properties)

   Each item in the list points to a TJwWTSProcess object that can be queried
   and manipulated.@br
   The list is filled by calling the EnumerateProcesses function of the owning
   TJwTerminalServer instance.@br
   @br
   Example:
   @longcode(#
   var
     ATerminalServer: TJwTerminalServer;
     i: Integer;
   begin
     // Create Terminal Server instance and allocate memory for it
     ATerminalServer := TjwTerminalServer.Create;

     // Set servername (only in case of remote server)
     ATerminalServer.Server := 'TS001';

     // Remember that EnumerateProcesses will automatically connect to the
     // Terminal Server for you. The connect function raises an Exception
     // if the connection attempt was unsuccessfull, so better use try..except
     try
       if ATerminalServer.EnumerateProcesses then
       begin
         for i := 0 to ATerminalServer.Processes.Count-1 do
         begin
           Memo1.Lines.Add(ATerminalServer.Processes[i].ProcessName);
         end;
       end;
     except
       on E: EJwsclWinCallFailedException do
       begin
        // Handle Exception here
       end;
     end;

     // Free Memory
     ATerminalServer.Free;
   end;
   #)
  }
  TJwWTSProcessList = class(TObjectList)
  protected
    {@exclude}
    FOwner: TJwTerminalServer;
    {@exclude}
    function GetItem(Index: Integer): TJwWTSProcess;
    {@exclude}
    procedure SetItem(Index: Integer; AProcess: TJwWTSProcess);
    {@exclude}
    procedure SetOwner(const Value: TJwTerminalServer);
  public
    {@Name adds a Process to the end of the Processlist
     @returns(returns the index of the inserted object.)
    }
    function Add(AProcess: TJwWTSProcess): Integer;


    {@Name returns an enumerator that can be used to iterate through
     the image list collection with Delphi's for in loop (Delphi 2005 and
     higher).@br
     @br
     @longcode(#
     var
       ATerminalServer: TJwTerminalServer;
       Process: TJwWTSProcess;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TJwTerminalServer.Create;

       // It's recommended to wrap the Connect call in a try..except block since
       // a connection failure will raise an exception.
       try
         ATerminalServer.Connect;
       except
         on E: EJwsclWinCallFailedException do
         begin
           // Handle Exception here
         end;
       end;

       // EnumerateProcesses might fail and return false, so always check the
       // result.
       if ATerminalServer.EnumerateProcesses then
       begin

         // Loop through the Processes with the for..in loop (needs Delphi 2005
         // or higher).
         for Process in ATerminalServer.Processes do
         begin
           Memo1.Lines.Add(Format('Process %s runs in  session %d with PID %d',
             [Process.Processname, Process.SessionId, Process.ProcessId]));
         end;

       end;

       // Free Memory (note that free will automatically disconnect an active
       // connection)

       ATerminalServer.Free;

     end;
     #)
    }
    function GetEnumerator: TJwProcessEnumerator;


    {@Returns(the index of the Process object in the ProcessList.)
    }
    function IndexOf(AProcess: TJwWTSProcess): Integer;

    {@Name Adds an object to the list at a specified position
    }
    procedure Insert(Index: Integer; AProcess: TJwWTSProcess);

    {The @Name properties gives access to a Process and it's properties@br
     Example:
     @longcode(#
     var
       ATerminalServer: TJwTerminalServer;
       i: Integer;
     begin
       // Create Terminal Server instance and allocate memory for it
       ATerminalServer := TjwTerminalServer.Create;

       // Set servername (only in case of remote server)
       ATerminalServer.Server := 'TS001';

       // Remember that EnumerateProcesses will automatically connect to the
       // Terminal Server for you. The connect function raises an Exception
       // if the connection attempt was unsuccessfull, so better use try..except
       try
         if ATerminalServer.EnumerateProcesses then
         begin
           for i := 0 to ATerminalServer.Processes.Count-1 do
           begin
             Memo1.Lines.Add(ATerminalServer.Processes[i].ProcessName);
           end;
         end;
       except
         on E: EJwsclWinCallFailedException do
         begin
          // Handle Exception here
         end;
       end;

       // Free Memory
       ATerminalServer.Free;
     end;
     #)
    }
    property Items[Index: Integer]: TJwWTSProcess read GetItem write SetItem; default;

    {@Name specifies the TJwTerminalServer instance that owns the ProcessList
    }
    property Owner: TJwTerminalServer read FOwner write SetOwner;

    {@Name removes the specified Session from the SessionList and if
     OwnsObjects is true (default) frees the Session.
     @returns(The value returned is the index of the object in the Items array
     before it was removed. If the specified object is not found on the list,
     Remove returns –1.)
    }
    function Remove(AProcess: TJwWTSProcess): Integer;
  end;

  {@Name indicates the Shadow State of a session}
  TShadowState =
    (
     {The session is not Shadowing or Being Shadowed}
     ssNone,
     {The session is not Shadowing another session}
     ssShadowing,
     {The session is being Shadowed by another session}
     ssBeingShadowed
  );

  {@Name indicates the Shadow Permissions of a session}
  TShadowMode = (
    {The sessions cannot be shadowed}
    smNoneAllowed,
    {The sessions be shadowed but needs the user's permission}
    smFullControlWithPermission,
    {The sessions be shadowed without the user's permission}
    smFullControlWithoutPermission,
    {The sessions can be viewed but needs the user's permission}
    smViewOnlyWithPermission,
    {The sessions can be viewed without the user's permission}
    smViewOnlyWithoutPermission
  );

  {@Abstract(@Name class gives access to the ShadowState and Shadowmode of a
   session.)

   @br@bold(Remarks:) Please note that changing the shadow mode with the SetShadow
   function does not take affect until the sessions has been disconnected
   and reconnected.
   @seealso(TShadowMode)
   @seealso(TShadowState) 
  }
  TJwWTSSessionShadow = class
  private
    {@exclude}
    FWinStationShadowInformation : TWinStationShadowInformation;
    {@exclude}
    FOwner : TJwWTSSession;
  protected
    {@exclude}
    function GetShadowState : TShadowState;
    {@exclude}
    function GetShadowMode : TShadowMode;
    {@exclude}
    procedure SetShadowMode(const Value : TShadowMode);
    {@exclude}
    procedure UpdateShadowInformation(const Modify : Boolean);
  public
    {The @Name constructor creates an @ClassName instance.
    }
    constructor Create(AOwner: TJwWTSSession);

    {The @Name property indicates the ShadowState of the Session
    }
    property ShadowState: TShadowState read GetShadowState;

    {The @Name property indicates the ShadowMode of the Session
    }
    property ShadowMode: TShadowMode read GetShadowMode write SetShadowMode;
  end;


  TJwSessionsEnumerator = class
  private
    FIndex: Integer;
    FSessions: TJwWTSSessionList;
  public
    constructor Create(ASessionList: TJwWTSSessionList);
    function GetCurrent: TJwWTSSession;
    function MoveNext: Boolean;
    property Current: TJwWTSSession read GetCurrent;
  end;



  TJwProcessEnumerator = class
  private
    FIndex: Integer;
    FProcesses: TJwWTSProcessList;
  public
    constructor Create(AProcessList: TJwWTSProcessList);
    function GetCurrent: TJwWTSProcess;
    function MoveNext: Boolean;
    property Current: TJwWTSProcess read GetCurrent;
  end;


{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

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
  FServers := TStringList.Create;

  FOnServersEnumerated := nil;
//  FServerLis := TStringList.Create;
end;

destructor TJwTerminalServer.Destroy;
var
  EventFlag: DWORD;
begin
  // Close connection
  if Assigned(FEnumServersThread) then
  begin
    // Don't handle any more events
    FOnServersEnumerated := nil;

    // Signal Termination to the thread
    FEnumServersThread.Terminate;

    // Wait a while, see if thread terminates
    if WaitForSingleObject(FEnumServersThread.Handle, 1000) = WAIT_TIMEOUT then
    begin
      // it didn't, so kill it (we don't want the user to wait forever)!
      // TSAdmin does it the same way...
      TerminateThread(FEnumServersThread.Handle, 0);
    end;

//    FEnumServersThread.Wait;
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
//    FreeAndNil(FServerList);

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
var
  nSize: DWORD;
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
var
  WinStationNamePtr: PWideChar;
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
//      Result := PWideCharToJwString(StrConnectState(WTSDisconnected, False));
      // Confirm to TSAdmin behaviour and list sessionname as (Idle) (we use
      // StrConnectState api to localise)
      Result := '(' + PWideCharToJwString(StrConnectState(WTSIdle, False)) + ')';
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

procedure TJwTerminalServer.OnInternalProcessFound(
      const Sender : TJwTerminalServer; var Process : TJwWTSProcess;
      var Cancel : Boolean; Data : Pointer);
begin
  Sender.FProcesses.Add(Process);
end;

function TJwTerminalServer.EnumerateProcesses: Boolean;
begin
  FProcesses.Clear;

  result := EnumerateProcesses(OnInternalProcessFound, nil);
end;

function TJwTerminalServer.EnumerateProcesses(const OnProcessFound : TJwOnProcessFound;
    Data : Pointer) : Boolean;
var
  Count: Integer;
  ProcessInfoPtr: PWINSTA_PROCESS_INFO_ARRAY;
  i: Integer;
  AProcess: TJwWTSProcess;
  strProcessName: TJwString;
  strUsername: TJwString;
  lpBuffer: PWideChar;
  DiffTime: TDiffTime;
//  strSid: TjwString;
  Cancel : Boolean;
begin
  ProcessInfoPtr := nil;
  Count := 0;

  JwRaiseOnNilParameter(@OnProcessFound, 'OnProcessFound','EnumerateProcesses',ClassName,RsUNTerminalServer);

  if not Connected then
  begin
    Connect;
  end;

  Cancel := false;
  ProcessInfoPtr := nil;

  Result := WinStationGetAllProcesses(FServerHandle, 0, Count, ProcessInfoPtr);

  try
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

          try
            OnProcessFound(Self, AProcess, Cancel, Data);
          finally
            //warning: AProcess may be deleted in there
          end;

          if Cancel then
              break;
        end;
      end;
    end;
  finally
    // Cleanup
    if ProcessInfoPtr <> nil then
    begin
      WinStationFreeGAPMemory(0, ProcessInfoPtr, Count);
    end;
  end;

  ProcessInfoPtr := nil;
end;

function TJwTerminalServer.GetServers: TStringList;
begin
  // Create the list
  if not Assigned(FServers) then
  begin
    FServers := TStringList.Create;
    // The list was empty so fill it!
    EnumerateServers('');
  end;

  // Return the serverlist
  //TODO: Warning: User can free returned list (on purpose) this can
  //lead to problems in EnumerateServers
  Result := FServers;
end;

function TJwTerminalServer.EnumerateSessions: boolean;
var
  SessionInfoPtr:
{$IFDEF UNICODE}
  PJwWTSSessionInfoWArray;
{$ELSE}
  PJwWTSSessionInfoAArray;
{$ENDIF UNICODE}
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
        // Mark handle as invalid
        FServerHandle := INVALID_HANDLE_VALUE;
        FConnected := False;
        
        // and raise exception
        raise EJwsclTerminalServerConnectException.CreateFmtWinCall(RsWinCallFailed,
          'WTSOpenServer', ClassName, RsUNTerminalServer, 0, True,
          'WTSOpenServer', ['WTSOpenServer', FServer]);
{        raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
          'WTSOpenServer', ClassName, RsUNTerminalServer, 0, True,
          'WTSOpenServer', ['WTSOpenServer', FServer]);}
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

class function TJwTerminalServer.FileTime2DateTime(FileTime: _FILETIME): TDateTime;
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
    // Wait some time to prevent duplicate event dispatch
    Sleep(500);
    OutputDebugString('Entering WTSWaitSystemEvent');

    if WTSWaitSystemEvent(FOwner.ServerHandle, WTS_EVENT_ALL, FEventFlag) then
    begin
      if FEventFlag > WTS_EVENT_FLUSH then
      begin
        OutputDebugString('Dispatching');
        Synchronize(DispatchEvent);
      end;
    end
    else if FEventFlag > WTS_EVENT_FLUSH then
    begin
      raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,
        'WTSWaitSystemEvent', ClassName, RsUNTerminalServer, 0, True,
        'Server: %s LastEvent: %d', [FOwner.Server, FOwner.LastEventFlag]);
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

  inherited Create(CreateSuspended, Format('%s (%s)', [ClassName, Owner.Server]));

  FTerminatedEvent := CreateEvent(nil, False, False, nil);
  FOwner := Owner;
  FDomain := Domain;
  FreeOnTerminate := True;
end;

procedure TJwWTSEnumServersThread.Execute;
type
  PWTS_SERVER_INFO = {$IFDEF UNICODE}PWTS_SERVER_INFOW{$ELSE}PWTS_SERVER_INFOA{$ENDIF UNICODE};
var
  ServerInfoPtr: PJwWtsServerInfoAArray;
  pCount: DWORD;
  i: DWORD;
begin
  inherited Execute;

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
  FOwner.Servers.Add(FServer);
end;

procedure TJwWTSEnumServersThread.ClearServerList;
begin
  FOwner.Servers.Clear;
end;

procedure TJwWTSEnumServersThread.DispatchEvent;
begin
  if Assigned(FOwner.OnServersEnumerated) then
  begin
    // Fire the OnServersEnumerated event
    FOwner.OnServersEnumerated(FOwner);
  end;
end;


{procedure TJwWTSEnumServersThread.Wait;
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
end;}

// Borland's WaitFor procedure contains a bug when using Waitfor in combination
// with FreeOnTerminate := True; During the loop in WaitFor the TThread object
// can be freed and its Handle invalidated.  When MsgWaitForMultipleObjects()
// is called again, it fails, and then a call to CheckThreadError() afterwards
// throws on EOSError exception with an error code of 6 and an error message of
//  "the handle is invalid". http://qc.borland.com/wc/qcmain.aspx?d=6080
// Therefore we override the WaitFor function and Create an Exception
{function TJwWTSEnumServersThread.WaitFor: LongWord;
begin
  // Return error
  //Result := ERROR_NOT_SUPPORTED;

  raise EJwsclUnimplemented.CreateFmtWinCall(RsWinCallFailed,
    'WaitFor function is not supported, please use Wait instead', ClassName,
    RsUNTerminalServer, 1203, False,
    'WaitFor function is not supported, please use Wait instead',
    ['TJwWTSEnumServersThread.WaitFor']);
end;}

destructor TJwWTSSessionList.Destroy;
begin
  inherited Destroy;
end;

function TJwWTSSessionList.Add(ASession: TJwWTSSession): Integer;
begin
  Result := inherited Add(ASession);
end;

function TJwWTSSessionList.GetEnumerator: TJwSessionsEnumerator;
begin
  Result := TJwSessionsEnumerator.Create(Self);
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

function TJwWTSProcessList.GetEnumerator: TJwProcessEnumerator;
begin
  Result := TJwProcessEnumerator.Create(Self);
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
var
  hModule: THandle;
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
var
  i: Integer;
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
var
  dwBytesReturned: DWORD;
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
var
  ABuffer: Pointer;
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
  JwRaiseOnNilMemoryBlock(Owner, 'GetServerHandle', ClassName, RsUNTerminalServer);
  JwRaiseOnNilMemoryBlock(Owner.Owner, 'GetServerHandle', ClassName, RsUNTerminalServer);

  Result := Owner.Owner.FServerHandle;
end;

procedure TJwWTSSession.GetWinStationDriver;
var
  WinStationDriver: _WD_CONFIGW;
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
var
  WinStationInfo: _WINSTATION_INFORMATIONW;
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
var
  ClientAddressPtr: PWtsClientAddress;
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
var
  ClientDisplayPtr: PWtsClientDisplay;
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
var
  tempStr : String;
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

  JwRaiseOnNilMemoryBlock(Owner,'GetToken',ClassName,RsUNTerminalServer);


  result := nil;

  try
    FToken := TJwSecurityToken.CreateWTSQueryUserTokenEx(Owner.Owner, FSessionId);
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
var
  hProc : HANDLE;
begin
  result := FToken;
  if Assigned(FToken) then
    exit;

  result := nil;
  //enable privilege is available
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


constructor TJwSessionsEnumerator.Create(ASessionList: TJwWTSSessionList);
begin
  inherited Create;
  FIndex := -1;
  FSessions := ASessionList;
end;

function TJwSessionsEnumerator.GetCurrent;
begin
  Result := FSessions[FIndex];
end;

function TJwSessionsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FSessions.Count - 1;
  if Result then
    Inc(FIndex);
end;

constructor TJwProcessEnumerator.Create(AProcessList: TJwWTSProcessList);
begin
  inherited Create;
  FIndex := -1;
  FProcesses := AProcessList;
end;

function TJwProcessEnumerator.GetCurrent;
begin
  Result := FProcesses[FIndex];
end;

function TJwProcessEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FProcesses.Count - 1;
  if Result then
    Inc(FIndex);
end;


{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}

