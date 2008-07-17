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

    function AddToWinFirewall(ApplicationFilename, NameOnExeptionlist: AnsiString;
      EnableRule: Boolean): Boolean;
    function DeleteFromWinFirewall(ApplicationFilename: AnsiString): Boolean;

    procedure AddTcpPortToFirewall(ProtocollName: AnsiString;
      ProtocollPort: Integer; const SubnetOnly: Boolean = False;
      const PortRemoteAddresses: AnsiString = '*');
    procedure AddUpdPortToFirewall(ProtocollName: AnsiString;
      ProtocollPort: Integer; const SubnetOnly: Boolean = False;
      const PortRemoteAddresses: AnsiString = '*');

    property Active: Boolean read GetFirewallState write SetFirewallState;

    property ExceptionsAllowed: Boolean read GetExceptionsAllowed
      write SetExceptionsAllowed;

    property IncommingPingAllowed: Boolean read GetIncommingPingAllowed
      write SetIncommingPingAllowed;

    property RemoteAdminAllowed: Boolean read GetRemoteAdminAllowed
      write SetRemoteAdminAllowed;

    property RemoteAdminAdress: AnsiString read GetRemoteAdminAdress
      write SetRemoteAdminAdress;
  end;

implementation

constructor TJwFirewallPolicy.Create();
var
    FwMgrDisp: IDispatch;
begin
  inherited;

  FwMgrDisp := CreateOleObject(FW_MGR_CLASS_NAME);
  FFWMgr := INetFwMgr(FwMgrDisp);

  if Assigned(@FFWMgr) then
    try
      FProfile := FFWMgr.LocalPolicy.CurrentProfile;
    except
      on e: EOleSysError do
      begin
        raise EJwsclFirewallInitException.CreateFmtEx(e.Message,
          'Create', 'EOleSysError', 'JwsclFirewallPolicy',
          104, True, []);
      end;
    end;
  FwMgrDisp := unassigned;
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
        132, True, []);
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
        153, True, []);
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
        174, True, []);
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
        195, True, []);
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
        216, True, []);
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
        237, True, []);
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
  try
    RASettings := FProfile.RemoteAdminSettings;
    if Assigned(@RASettings) then
    begin
      Result := RASettings.Enabled;
    end;
  except
    on e: EOleSysError do
    begin
      raise EJwsclGetRemoteAdminAllowedException.CreateFmtEx(e.Message,
        'GetRemoteAdminAllowed', 'EOleSysError', 'JwsclFirewallPolicy',
        260, True, []);
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
  try
    RASettings := FProfile.RemoteAdminSettings;
    if Assigned(@RASettings) then
    begin
      RASettings.Enabled := Value;
    end;
  except
    on e: EOleSysError do
    begin
      raise EJwsclGetRemoteAdminAllowedException.CreateFmtEx(e.Message,
        'SetRemoteAdminAllowed', 'EOleSysError', 'JwsclFirewallPolicy',
        288, True, []);
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
  try
    RASettings := FProfile.RemoteAdminSettings;
    if Assigned(@RASettings) then
    begin
      Result := RASettings.RemoteAddresses;
    end;
  except
    on e: EOleSysError do
    begin
      raise EJwsclGetRemoteAdminAdressException.CreateFmtEx(e.Message,
        'GetRemoteAdminAdress', 'EOleSysError', 'JwsclFirewallPolicy',
        316, True, []);
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
  try
    RASettings := FProfile.RemoteAdminSettings;
    if Assigned(@RASettings) then
    begin
      RASettings.RemoteAddresses := Value;
    end;
  except
    on e: EOleSysError do
    begin
      raise EJwsclSetRemoteAdminAdressException.CreateFmtEx(e.Message,
        'SetRemoteAdminAdress', 'EOleSysError', 'JwsclFirewallPolicy',
        344, True, []);
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
            387, True, []);
        end;
      end;

    end;
    AppDisp := Unassigned;
    App := nil;
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
  Port: INetFwOpenPort;
begin
  if Assigned(@Port) then
  begin
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
          431, True, []);
      end;
    end;
  end;
  Port := nil;  
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
  Port: INetFwOpenPort;
begin

  if Assigned(@Port) then
  begin
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
          474, True, []);
      end;
    end;
  end;
  Port := nil;  
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
          503, True, []);
      end;
    end;
end;

end.

