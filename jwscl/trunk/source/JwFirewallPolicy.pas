{******************************************************************************}
{ JEDI API example TJwFirewallPolicy              			 										   }
{ http://jedi-apilib.sourceforge.net                    										   }
{ 					                                    														   }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{ 										                                    									   }
{ Author(s): Heiko Adams                        														   }
{ Creation date: 04th July 2008 					   		                        		   }
{ Last modification date: 17th July 2008          													   }
{                                     																			   }
{ Description: Provides functions and procedures for adding and removing ports }
{ 						 and application rules to the windows firewall    						   }
{ 																			                                       }
{ Preparations: JWA must be ready to use.                     							   }
{ 													                                    						   }
{ Article link: -                                             							   }
{ 																                                    			   }
{ Version history: -              	                    										   }
{ 																			                                       }
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
{                                                                              }					                     }
{******************************************************************************}
unit JwFirewallPolicy;

interface

uses ComObj, Variants, JwsclConstants, JwsclExceptions;

type
  TJwFirewallPolicy = class(TObject)
  strict private
    FFWMgr: Variant;
    FProfile: Variant;

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
      Enabled: Boolean): Boolean;
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
begin
  inherited;
  try
    FFWMgr := CreateOleObject(FW_MGR_CLASS_NAME);
  except
    on e: Exception do
    begin
      raise EJwsclWinCallFailedException.Create(e.Message);
    end;
  end;

  if Assigned(@FFWMgr) then
    try
      FProfile := FFWMgr.LocalPolicy.CurrentProfile;
    except
      on e: Exception do
      begin
        raise EJwsclWinCallFailedException.Create(e.Message);
      end;
    end;
end;

destructor TJwFirewallPolicy.Destroy();
begin
  inherited;
  FFWMgr := Unassigned;
  FProfile := Unassigned;
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
    on e: Exception do
    begin
      raise EJwsclWinCallFailedException.Create(e.Message);
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
    on e: Exception do
    begin
      raise EJwsclWinCallFailedException.Create(e.Message);
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
    on e: Exception do
    begin
      raise EJwsclWinCallFailedException.Create(e.Message);
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
    on e: Exception do
    begin
      raise EJwsclWinCallFailedException.Create(e.Message);
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
    on e: Exception do
    begin
      raise EJwsclWinCallFailedException.Create(e.Message);
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
    on e: Exception do
    begin
      raise EJwsclWinCallFailedException.Create(e.Message);
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
  RASettings: Variant;
begin
  try
    RASettings := FProfile.RemoteAdminSettings;
    if Assigned(@RASettings) then
    begin
      Result := RASettings.Enabled;
      RASettings := Unassigned;
    end;
  except
    on e: Exception do
    begin
      raise EJwsclWinCallFailedException.Create(e.Message);
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
  RASettings: Variant;
begin
  try
    RASettings := FProfile.RemoteAdminSettings;
    if Assigned(@RASettings) then
    begin
      RASettings.Enabled := Value;
      RASettings := Unassigned;
    end;
  except
    on e: Exception do
    begin
      raise EJwsclWinCallFailedException.Create(e.Message);
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
  RASettings: Variant;
begin
  try
    RASettings := FProfile.RemoteAdminSettings;
    if Assigned(@RASettings) then
    begin
      Result := RASettings.RemoteAddresses;
      RASettings := Unassigned;
    end;
  except
    on e: Exception do
    begin
      raise EJwsclWinCallFailedException.Create(e.Message);
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
  RASettings: Variant;
begin
  try
    RASettings := FProfile.RemoteAdminSettings;
    if Assigned(@RASettings) then
    begin
      RASettings.RemoteAddresses := Value;
      RASettings := Unassigned;
    end;
  except
    on e: Exception do
    begin
      raise EJwsclWinCallFailedException.Create(e.Message);
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
  NameOnExeptionlist: AnsiString; Enabled: Boolean): Boolean;
var
  App: Variant;
begin
  Result := False;

  if Active
    and ExceptionsAllowed then
  begin
    try
      App := CreateOleObject(FW_AUTHORIZEDAPPLICATION_CLASS_NAME);
      if Assigned(@App) then
      begin
        App.ProcessImageFileName := applicationfilename;
        App.Name := NameOnExeptionlist;
        App.Scope := NET_FW_SCOPE_ALL;
        App.IpVersion := NET_FW_IP_VERSION_ANY;
        App.Enabled := Enabled;

        try
          FProfile.AuthorizedApplications.Add(App);
          Result := True;
        except
          on e: Exception do
          begin
            raise EJwsclWinCallFailedException.Create(e.Message);
          end;
        end;

      end;
    except
      on e: Exception do
      begin
        raise EJwsclWinCallFailedException.Create(e.Message);
      end;
    end;
  end;

  App := Unassigned;
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
  Port: Variant;
begin
  try
    Port := CreateOleObject(FW_OPENPORT_CLASS);
    if Assigned(@Port) then
    begin
      port.Name := ProtocollName;
      port.Protocol := NET_FW_IP_PROTOCOL_TCP;
      port.Port := ProtocollPort;
      if SubnetOnly then
        Port.Scope := NET_FW_SCOPE_LOCAL_SUBNET
      else
        port.Scope := NET_FW_SCOPE_ALL;
      port.RemoteAddresses := PortRemoteAddresses;
      port.Enabled := True;

      try
        FProfile.GloballyOpenPorts.Add(Port);
      except
        on e: Exception do
        begin
          raise EJwsclWinCallFailedException.Create(e.Message);
        end;
      end;
    end;
  except
    on e: Exception do
    begin
      raise EJwsclWinCallFailedException.Create(e.Message);
    end;
  end;

  Port := Unassigned;
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
  Port: Variant;
begin
  Result := False;
  try
    Port := CreateOleObject(FW_OPENPORT_CLASS);
    if Assigned(@Port) then
    begin
      port.Name := ProtocollName;
      port.Protocol := NET_FW_IP_PROTOCOL_UDP;
      port.Port := ProtocollPort;
      if SubnetOnly then
        Port.Scope := NET_FW_SCOPE_LOCAL_SUBNET
      else
        port.Scope := NET_FW_SCOPE_ALL;
      port.RemoteAddresses := PortRemoteAddresses;
      port.Enabled := True;

      try
        FProfile.GloballyOpenPorts.Add(Port);
      except
        on e: Exception do
        begin
          raise EJwsclWinCallFailedException.Create(e.Message);
        end;
      end;
    end;
  except
    on e: Exception do
    begin
      raise EJwsclWinCallFailedException.Create(e.Message);
    end;
  end;

  Port := Unassigned;
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
      on e: Exception do
      begin
        raise EJwsclWinCallFailedException.Create(e.Message);
      end;
    end;
end;

end.

