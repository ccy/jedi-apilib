unit UDataModule;

interface

uses
  Classes;

type
  TJWAConfiguration = (
    jwacDynamicDebug = 0,
    jwacDynamicRelease,
    jwacStaticDebug,
    jwacStaticRelease);

  TDelphiInstallation = class
  private

  protected
    fTargetSvnPath,
    fTargetSvnRelease,
    fTargetSvnJWARelease,
    fTargetSvnJWSCLRelease : String;
    fIsJWASingleUnit : Boolean;

    fJWAConfigs,
    fJWSCLConfigs : TStringList;

    fJWAConfiguration : TJWAConfiguration;
    fTargetDelphiList : TList;
    function GetIsJwscl : Boolean;


  public
    constructor Create;
    destructor Destroy; override;


    procedure ClearJWAConfigs;
    procedure ClearJWSCLConfigs;

  published

    property TargetSvnPath : String read fTargetSvnPath write fTargetSvnPath;

    property TargetSvnJWARelease : String read fTargetSvnJWARelease write fTargetSvnJWARelease;
    property TargetSvnJWSCLRelease : String read fTargetSvnJWSCLRelease write fTargetSvnJWSCLRelease;
    property IsJWSCL : Boolean read GetIsJwscl;
    property IsJWASingleUnit : Boolean  read FIsJWASingleUnit write fIsJWASingleUnit;

    property JWAConfiguration : TJWAConfiguration read fJWAConfiguration write fJWAConfiguration;

    property JWAConfigs   : TStringList read fJWAConfigs;
    property JWSCLConfigs : TStringList read fJWSCLConfigs;

    {
    Contains a list of indexes pointing to items of
    TJclBorRADToolInstallations.Installations
    }
    property TargetDelphiList : TList read fTargetDelphiList write fTargetDelphiList;
  end;

implementation

uses
  SysUtils;

{ TDelphiInstallation }

procedure TDelphiInstallation.ClearJWAConfigs;
var i : Integer;
begin
  for I := 0 to fJWAConfigs.Count - 1 do
  begin
    if fJWAConfigs.Objects[i] <> nil then
      Dispose(Pointer(fJWAConfigs.Objects[i]));
  end;
  fJWAConfigs.Clear;
end;

procedure TDelphiInstallation.ClearJWSCLConfigs;
var i : Integer;
begin
  for I := 0 to fJWSCLConfigs.Count - 1 do
  begin
    if fJWSCLConfigs.Objects[i] <> nil then
      Dispose(Pointer(fJWSCLConfigs.Objects[i]));
  end;
  fJWSCLConfigs.Clear;
end;

constructor TDelphiInstallation.Create;
begin
  inherited;

  fJWAConfigs := TStringList.Create;
  fJWSCLConfigs := TStringList.Create;
  fTargetDelphiList := TList.Create;
end;

destructor TDelphiInstallation.Destroy;
begin
  ClearJWAConfigs;
  fJWAConfigs.Free;

  ClearJWSCLConfigs;
  fJWSCLConfigs.Free;
  fTargetDelphiList.Free;
end;

function TDelphiInstallation.GetIsJwscl: Boolean;
begin
  result := Length(Trim(TargetSvnJWSCLRelease)) <> 0;
end;

end.
