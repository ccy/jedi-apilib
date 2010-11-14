unit AccessibilityCplAdminLib_TLB;

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// $Rev: 8291 $
// File generated on 19.08.2008 18:51:10 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Windows\System32\AccessibilityCpl.dll (1)
// LIBID: {714DD4F6-7676-4BDE-925A-C2FEC2073F36}
// LCID: 0
// Helpfile:
// HelpString: AccessibilityCplAdmin 1.0 Type Library
// DepndLst:
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
// ************************************************************************ //
// *************************************************************************//
// NOTE:
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties
// which return objects that may need to be explicitly created via a function
// call prior to any access via the property. These items have been disabled
// in order to prevent accidental use from within the object inspector. You
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively
// removing them from the $IFDEF blocks. However, such items must still be
// programmatically created via a method of the appropriate CoClass before
// they can be used.
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  AccessibilityCplAdminLibMajorVersion = 1;
  AccessibilityCplAdminLibMinorVersion = 0;

  LIBID_AccessibilityCplAdminLib: TGUID = '{714DD4F6-7676-4BDE-925A-C2FEC2073F36}';

  IID_IAccessibilityCplAdmin: TGUID = '{97B9F488-B188-4B03-9B27-D74B25755464}';
  CLASS_AccessibilityCplAdmin: TGUID = '{434A6274-C539-4E99-88FC-44206D942775}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  IAccessibilityCplAdmin = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  AccessibilityCplAdmin = IAccessibilityCplAdmin;


// *********************************************************************//
// Interface: IAccessibilityCplAdmin
// Flags:     (384) NonExtensible OleAutomation
// GUID:      {97B9F488-B188-4B03-9B27-D74B25755464}
// *********************************************************************//
  IAccessibilityCplAdmin = interface(IUnknown)
    ['{97B9F488-B188-4B03-9B27-D74B25755464}']
    function LinktoSystemRestorePoint: HResult; stdcall;
    function ApplyToLogonDesktop(configuration: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// The Class CoAccessibilityCplAdmin provides a Create and CreateRemote method to
// create instances of the default interface IAccessibilityCplAdmin exposed by
// the CoClass AccessibilityCplAdmin. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoAccessibilityCplAdmin = class
    class function Create: IAccessibilityCplAdmin;
    class function CreateRemote(const MachineName: string): IAccessibilityCplAdmin;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TAccessibilityCplAdmin
// Help String      : AccessibilityCplAdmin Class
// Default Interface: IAccessibilityCplAdmin
// Def. Intf. DISP? : No
// Event   Interface:
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TAccessibilityCplAdminProperties= class;
{$ENDIF}
  TAccessibilityCplAdmin = class(TOleServer)
  private
    FIntf: IAccessibilityCplAdmin;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TAccessibilityCplAdminProperties;
    function GetServerProperties: TAccessibilityCplAdminProperties;
{$ENDIF}
    function GetDefaultInterface: IAccessibilityCplAdmin;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IAccessibilityCplAdmin);
    procedure Disconnect; override;
    function LinktoSystemRestorePoint: HResult;
    function ApplyToLogonDesktop(configuration: PWideChar): HResult;
    property DefaultInterface: IAccessibilityCplAdmin read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TAccessibilityCplAdminProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TAccessibilityCplAdmin
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TAccessibilityCplAdminProperties = class(TPersistent)
  private
    FServer:    TAccessibilityCplAdmin;
    function    GetDefaultInterface: IAccessibilityCplAdmin;
    constructor Create(AServer: TAccessibilityCplAdmin);
  protected
  public
    property DefaultInterface: IAccessibilityCplAdmin read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = '(none)';

  dtlOcxPage = '(none)';

implementation

uses ComObj;

class function CoAccessibilityCplAdmin.Create: IAccessibilityCplAdmin;
begin
  Result := CreateComObject(CLASS_AccessibilityCplAdmin) as IAccessibilityCplAdmin;
end;

class function CoAccessibilityCplAdmin.CreateRemote(const MachineName: string): IAccessibilityCplAdmin;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_AccessibilityCplAdmin) as IAccessibilityCplAdmin;
end;

procedure TAccessibilityCplAdmin.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{434A6274-C539-4E99-88FC-44206D942775}';
    IntfIID:   '{97B9F488-B188-4B03-9B27-D74B25755464}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TAccessibilityCplAdmin.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IAccessibilityCplAdmin;
  end;
end;

procedure TAccessibilityCplAdmin.ConnectTo(svrIntf: IAccessibilityCplAdmin);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TAccessibilityCplAdmin.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TAccessibilityCplAdmin.GetDefaultInterface: IAccessibilityCplAdmin;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TAccessibilityCplAdmin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TAccessibilityCplAdminProperties.Create(Self);
{$ENDIF}
end;

destructor TAccessibilityCplAdmin.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TAccessibilityCplAdmin.GetServerProperties: TAccessibilityCplAdminProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TAccessibilityCplAdmin.LinktoSystemRestorePoint: HResult;
begin
  Result := DefaultInterface.LinktoSystemRestorePoint;
end;

function TAccessibilityCplAdmin.ApplyToLogonDesktop(configuration: PWideChar): HResult;
begin
  Result := DefaultInterface.ApplyToLogonDesktop(configuration);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TAccessibilityCplAdminProperties.Create(AServer: TAccessibilityCplAdmin);
begin
  inherited Create;
  FServer := AServer;
end;

function TAccessibilityCplAdminProperties.GetDefaultInterface: IAccessibilityCplAdmin;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TAccessibilityCplAdmin]);
end;

end.
