{
Description
Project JEDI Windows Security Code Library (JWSCL)

Provides functions and procedures for adding and removing ports and application rules to the windows firewall

Author
Heiko Adams

License
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

Note
The Original Code is JwsclFirewallPolicy.pas.

The Initial Developer of the Original Code is Heiko Adams


}
unit JwsclFirewallPolicy;

interface

uses ComObj, Variants, NetFwTypeLib_TLB, JwsclConstants, JwsclExceptions;

type
  TJwFirewallPolicy = class(TObject)
  strict private
    FFWMgr: INetFwMgr;
    FProfile: INetFwProfile;

    function GetFirewallState: Boolean;
    function GetExceptionsAllowed: Boolean;

    procedure SetFirewallState(Value: Boolean);
    procedure SetExceptionsAllowed(Value: Boolean);

    function GetIncommingPingAllowed: Boolean;
    procedure SetIncommingPingAllowed(Value: Boolean);

    function GetRemoteAdminAllowed(): Boolean;
    procedure SetRemoteAdminAllowed(Value: Boolean);

    function GetRemoteAdminAdress(): AnsiString;
    procedure SetRemoteAdminAdress(Value: AnsiString);
  public
    constructor Create();
    destructor Destroy(); override;

     {<B>AddToWinFirewall</B> Creates a firewallrule for specified program.
     @param ApplicationFilename defines the executable of the program
     @param NameOnExeptionlist defines the string that should be used in the firewall's
      rules list
     @param EnableRule defines if the new rule should be active or not
     }
    function AddToWinFirewall(ApplicationFilename, NameOnExeptionlist: AnsiString;
      EnableRule: Boolean): Boolean;

    {<B>DeleteFromWinFirewall</B> Removes a program's firewall rule.
     @param ApplicationFilename defines the executable of the program
    }
    function DeleteFromWinFirewall(ApplicationFilename: AnsiString): Boolean;

    {<B>AddTcpPortToFirewall</B> Adds a rule for a single tcp port to the firewall.
     @param ProtocollName defines the name of the protocol
     @param ProtocollPort defines the port of the protocol
     @param SubnetOnly defines if the rule affects only the pc's subnet or not
    }
    procedure AddTcpPortToFirewall(ProtocollName: AnsiString;
      ProtocollPort: Integer; const SubnetOnly: Boolean = False;
      const PortRemoteAddresses: AnsiString = '*');

    {<B>AddUpdPortToFirewall</B> Adds a rule for a single udp port to the firewall.
     @param ProtocollName defines the name of the protocol
     @param ProtocollPort defines the port of the protocol
     @param SubnetOnly defines if the rule affects only the pc's subnet or not
    }
    procedure AddUpdPortToFirewall(ProtocollName: AnsiString;
      ProtocollPort: Integer; const SubnetOnly: Boolean = False;
      const PortRemoteAddresses: AnsiString = '*');

    {<B>Active</B> Sets or gets if the windows firewall is active or not}
    property Active: Boolean read GetFirewallState write SetFirewallState;

    {<B>ExceptionsAllowed</B> Sets or gets if the windows firewall allows exceptions}
    property ExceptionsAllowed: Boolean read GetExceptionsAllowed
      write SetExceptionsAllowed;

    {<B>IncommingPingAllowed</B> Sets or gets if the windows firewall allows incomming pings}
    property IncommingPingAllowed: Boolean read GetIncommingPingAllowed
      write SetIncommingPingAllowed;

    {<B>RemoteAdminAllowed</B> Sets or gets if the windows firewall allows remote administration}
    property RemoteAdminAllowed: Boolean read GetRemoteAdminAllowed
      write SetRemoteAdminAllowed;

    {<B>ExceptionsAllowed</B> Sets or gets the adress(es) which are allowed for remote administration}
    property RemoteAdminAdress: AnsiString read GetRemoteAdminAdress
      write SetRemoteAdminAdress;
  end;

implementation

constructor TJwFirewallPolicy.Create();
var
    FwMgrDisp: IDispatch;
begin
  inherited;

  try
    try
      FwMgrDisp := CreateOleObject(FW_MGR_CLASS_NAME);
      FFWMgr := INetFwMgr(FwMgrDisp);
    except
      on e: EOleSysError do
      begin
        raise EJwsclFirewallProfileInitException.CreateFmtEx(e.Message,
          'Create', 'EOleSysError', 'JwsclFirewallPolicy',
          130, True, []);
      end;
    end;

    try
      FProfile := FFWMgr.LocalPolicy.CurrentProfile;
    except
      on e: EOleSysError do
      begin
        raise EJwsclFirewallProfileInitException.CreateFmtEx(e.Message,
          'Create', 'EOleSysError', 'JwsclFirewallPolicy',
          142, True, []);
      end;
    end;
  finally
    FwMgrDisp := unassigned;
  end;
end;

destructor TJwFirewallPolicy.Destroy();
begin
  inherited;
  FFWMgr := nil;
  FProfile := nil;
end;

{-------------------------------------------------------------------------------
  Procedure: TJwFirewallPolicy.SetFirewallState
  Author:    heiko.adams
  DateTime:  2008.07.04
  Arguments: Value: Boolean
  Result:    None
-------------------------------------------------------------------------------}
procedure TJwFirewallPolicy.SetFirewallState(Value: Boolean);
begin
  try
    FProfile.FirewallEnabled := Value;
  except
    on e: EOleSysError do
    begin
      raise EJwsclSetFWStateException.CreateFmtEx(e.Message,
        'SetFirewallState', 'EOleSysError', 'JwsclFirewallPolicy',
        173, True, []);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Function:  TJwFirewallPolicy.GetFirewallState
  Author:    heiko.adams
  DateTime:  2008.07.04
  Arguments:
  Result:    Boolean
-------------------------------------------------------------------------------}
function TJwFirewallPolicy.GetFirewallState(): Boolean;
begin
  try
    Result := FProfile.FirewallEnabled;
  except
    on e: EOleSysError do
    begin
      raise EJwsclGetFWStateException.CreateFmtEx(e.Message,
        'GetFirewallState', 'EOleSysError', 'JwsclFirewallPolicy',
        194, True, []);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Function:  TJwFirewallPolicy.GetExceptionsAllowed
  Author:    heiko.adams
  DateTime:  2008.07.04
  Arguments:
  Result:    Boolean
-------------------------------------------------------------------------------}
function TJwFirewallPolicy.GetExceptionsAllowed(): Boolean;
begin
  try
    Result := (not FProfile.ExceptionsNotAllowed);
  except
    on e: EOleSysError do
    begin
      raise EJwsclGetFWExceptionsAllowedException.CreateFmtEx(e.Message,
        'GetExceptionsAllowed', 'EOleSysError', 'JwsclFirewallPolicy',
        215, True, []);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: TJwFirewallPolicy.SetExceptionsAllowed
  Author:    heiko.adams
  DateTime:  2008.07.04
  Arguments: Value: Boolean
  Result:    None
-------------------------------------------------------------------------------}
procedure TJwFirewallPolicy.SetExceptionsAllowed(Value: Boolean);
begin
  try
    FProfile.ExceptionsNotAllowed := Value;
  except
    on e: EOleSysError do
    begin
      raise EJwsclSetFWExceptionsAllowedException.CreateFmtEx(e.Message,
        'SetExceptionsAllowed', 'EOleSysError', 'JwsclFirewallPolicy',
        236, True, []);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Function:  TJwFirewallPolicy.GetIncommingPingAllowed
  Author:    heiko.adams
  DateTime:  2008.07.04
  Arguments: None
  Result:    Boolean
-------------------------------------------------------------------------------}
function TJwFirewallPolicy.GetIncommingPingAllowed: Boolean;
begin
  try
    Result := FProfile.IcmpSettings.AllowInboundEchoRequest;
  except
    on e: EOleSysError do
    begin
      raise EJwsclGetIncommingPingAllowedException.CreateFmtEx(e.Message,
        'GetIncommingPingAllowed', 'EOleSysError', 'JwsclFirewallPolicy',
        257, True, []);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: TJwFirewallPolicy.SetIncommingPingAllowed
  Author:    heiko.adams
  DateTime:  2008.07.04
  Arguments: Value: Boolean
  Result:    None
-------------------------------------------------------------------------------}
procedure TJwFirewallPolicy.SetIncommingPingAllowed(Value: Boolean);
begin
  try
    FProfile.IcmpSettings.AllowInboundEchoRequest := Value;
  except
    on e: EOleSysError do
    begin
      raise EJwsclSetIncommingPingAllowedException.CreateFmtEx(e.Message,
        'SetIncommingPingAllowed', 'EOleSysError', 'JwsclFirewallPolicy',
        278, True, []);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Function:  TJwFirewallPolicy.GetRemoteAdminAllowed
  Author:    heiko.adams
  DateTime:  2008.07.04
  Arguments:
  Result:    Boolean
-------------------------------------------------------------------------------}
function TJwFirewallPolicy.GetRemoteAdminAllowed(): Boolean;
var
  RASettings: INetFwRemoteAdminSettings;
begin
  RASettings := FProfile.RemoteAdminSettings;
  try
    Result := RASettings.Enabled;
  except
    on e: EOleSysError do
    begin
      raise EJwsclGetRemoteAdminAllowedException.CreateFmtEx(e.Message,
        'GetRemoteAdminAllowed', 'EOleSysError', 'JwsclFirewallPolicy',
        302, True, []);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: TJwFirewallPolicy.SetRemoteAdminAllowed
  Author:    heiko.adams
  DateTime:  2008.07.04
  Arguments: Value: Boolean
  Result:    None
-------------------------------------------------------------------------------}
procedure TJwFirewallPolicy.SetRemoteAdminAllowed(Value: Boolean);
var
  RASettings: INetFwRemoteAdminSettings;
begin
  RASettings := FProfile.RemoteAdminSettings;
  try
    RASettings.Enabled := Value;
  except
    on e: EOleSysError do
    begin
      raise EJwsclGetRemoteAdminAllowedException.CreateFmtEx(e.Message,
        'SetRemoteAdminAllowed', 'EOleSysError', 'JwsclFirewallPolicy',
        326, True, []);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Function:  TJwFirewallPolicy.GetRemoteAdminAdress
  Author:    heiko.adams
  DateTime:  2008.07.04
  Arguments:
  Result:    AnsiString
-------------------------------------------------------------------------------}
function TJwFirewallPolicy.GetRemoteAdminAdress(): AnsiString;
var
  RASettings: INetFwRemoteAdminSettings;
begin
  RASettings := FProfile.RemoteAdminSettings;
  try
    Result := RASettings.RemoteAddresses;
  except
    on e: EOleSysError do
    begin
      raise EJwsclGetRemoteAdminAdressException.CreateFmtEx(e.Message,
        'GetRemoteAdminAdress', 'EOleSysError', 'JwsclFirewallPolicy',
        350, True, []);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: TJwFirewallPolicy.SetRemoteAdminAdress
  Author:    heiko.adams
  DateTime:  2008.07.04
  Arguments: Value: AnsiString
  Result:    None
-------------------------------------------------------------------------------}
procedure TJwFirewallPolicy.SetRemoteAdminAdress(Value: AnsiString);
var
  RASettings: INetFwRemoteAdminSettings;
begin
  RASettings := FProfile.RemoteAdminSettings;
  try
    RASettings.RemoteAddresses := Value;
  except
    on e: EOleSysError do
    begin
      raise EJwsclSetRemoteAdminAdressException.CreateFmtEx(e.Message,
        'SetRemoteAdminAdress', 'EOleSysError', 'JwsclFirewallPolicy',
        374, True, []);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: TJwFirewallPolicy.AddToWinFirewall
  Author:    heiko.adams
  DateTime:  2008.07.17
  Arguments: ApplicationFilename, NameOnExeptionlist: AnsiString; Enabled: Boolean
  Result:    Boolean
-------------------------------------------------------------------------------}
function TJwFirewallPolicy.AddToWinFirewall(ApplicationFilename,
  NameOnExeptionlist: AnsiString; EnableRule: Boolean): Boolean;
var
  AppDisp: IDispatch;
  App: INetFwAuthorizedApplication;
begin
  Result := False;

  if Active
    and ExceptionsAllowed then
  begin
    try
      AppDisp := CreateOleObject(FW_AUTHORIZEDAPPLICATION_CLASS_NAME);
      App := INetFwAuthorizedApplication(AppDisp);

      if Assigned(@App) then
      begin
        with App do
        begin
          ProcessImageFileName := ApplicationFilename;
          Name := NameOnExeptionlist;
          Scope := NET_FW_SCOPE_ALL;
          IpVersion := NET_FW_IP_VERSION_ANY;
          Enabled := EnableRule;
        end;

        try
          FProfile.AuthorizedApplications.Add(App);
          Result := True;
        except
          on e: EOleSysError do
          begin
            raise EJwsclFirewallAddRuleException.CreateFmtEx(e.Message,
              'AddToWinFirewall', 'EOleSysError', 'JwsclFirewallPolicy',
              419, True, []);
          end;
        end;
      end;
    finally
      AppDisp := Unassigned;
      App := nil;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: TJwFirewallPolicy.AddTcpPortToFirewall
  Author:    heiko.adams
  DateTime:  2008.07.04
  Arguments: ProtocollName: AnsiString; ProtocollPort: Integer; const SubnetOnly: Boolean = False; const PortRemoteAddresses: AnsiString = '*'
  Result:    None
-------------------------------------------------------------------------------}
procedure TJwFirewallPolicy.AddTcpPortToFirewall(ProtocollName: AnsiString;
  ProtocollPort: Integer; const SubnetOnly: Boolean = False;
  const PortRemoteAddresses: AnsiString = '*');
var
  PortDisp: IDispatch;
  Port: INetFwOpenPort;
begin
  try
    PortDisp := CreateOleObject(FW_OPENPORT_CLASS);
    Port := INetFwOpenPort(PortDisp);

    with Port do
    begin
      Name := ProtocollName;
      Protocol := NET_FW_IP_PROTOCOL_TCP;
      Port := ProtocollPort;
      if SubnetOnly then
        Scope := NET_FW_SCOPE_LOCAL_SUBNET
      else
        Scope := NET_FW_SCOPE_ALL;
      RemoteAddresses := PortRemoteAddresses;
      Enabled := True;
    end;

    try
      FProfile.GloballyOpenPorts.Add(Port);
    except
      on e: EOleSysError do
      begin
        raise EJwsclAddTcpPortToFirewallException.CreateFmtEx(e.Message,
          'AddTcpPortToFirewall', 'EOleSysError', 'JwsclFirewallPolicy',
          469, True, []);
      end;
    end;
  finally
    PortDisp := Unassigned;
    Port := nil;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: TJwFirewallPolicy.AddUpdPortToFirewall
  Author:    heiko.adams
  DateTime:  2008.07.04
  Arguments: ProtocollName: AnsiString; ProtocollPort: Integer; const SubnetOnly: Boolean = False; const PortRemoteAddresses: AnsiString = '*'
  Result:    None
-------------------------------------------------------------------------------}
procedure TJwFirewallPolicy.AddUpdPortToFirewall(ProtocollName: AnsiString;
  ProtocollPort: Integer; const SubnetOnly: Boolean = False;
  const PortRemoteAddresses: AnsiString = '*');
var
  PortDisp: IDispatch;
  Port: INetFwOpenPort;
begin
  try
    PortDisp := CreateOleObject(FW_OPENPORT_CLASS);
    Port := INetFwOpenPort(PortDisp);

    with Port do
    begin
      Name := ProtocollName;
      Protocol := NET_FW_IP_PROTOCOL_UDP;
      Port := ProtocollPort;
      if SubnetOnly then
        Scope := NET_FW_SCOPE_LOCAL_SUBNET
      else
        Scope := NET_FW_SCOPE_ALL;
      RemoteAddresses := PortRemoteAddresses;
      Enabled := True;
    end;

    try
      FProfile.GloballyOpenPorts.Add(Port);
    except
      on e: EOleSysError do
      begin
        raise EJwsclAddTcpPortToFirewallException.CreateFmtEx(e.Message,
          'AddUpdPortToFirewall', 'EOleSysError', 'JwsclFirewallPolicy',
          516, True, []);
      end;
    end;
  finally
    PortDisp := Unassigned;
    Port := nil;
  end;
end;

{-------------------------------------------------------------------------------
  Procedure: TJwFirewallPolicy.DeleteFromWinFirewall
  Author:    heiko.adams
  DateTime:  2008.07.17
  Arguments: ApplicationFilename: AnsiString
  Result:    Boolean
-------------------------------------------------------------------------------}
function TJwFirewallPolicy.DeleteFromWinFirewall(ApplicationFilename: AnsiString):
  Boolean;
begin
  Result := False;
  
  if Active
    and ExceptionsAllowed then
    try
      FProfile.AuthorizedApplications.Remove(ApplicationFilename);
      Result := True;
    except
      on e: EOleSysError do
      begin
        raise EJwsclFirewallDelRuleException.CreateFmtEx(e.Message,
          'DeleteFromWinFirewall', 'EOleSysError', 'JwsclFirewallPolicy',
          546, True, []);
      end;
    end;
end;

end.

