unit UDataModule;

interface

uses
  JwaWindows, Forms,
  SysUtils, Classes, Dialogs, DelphiVersionTool;

type
  TSetupType = (stCheckout, stUpdate, stRemove);
  TProjectType = (ptJWA, ptJWSCL);
  TProjectTypes = set of TProjectType;

  TDataModule1 = class(TDataModule)
  private
    { Private-Deklarationen }
    fSetupType : TSetupType;
    fProjectTypes : TProjectTypes;

    fTargetPath : String;
    fTargetDelphiJwa,
    fTargetDelphiJwscl : TList; //array of index to KnownDelphiVersions for selected delphi


  public
    function GetVersions(const ProjectType : TProjectType) : TStringList;

    { Public-Deklarationen }
    property SetupType : TSetupType read fSetupType write fSetupType;

    property TargetPath : String read fTargetPath write fTargetPath;
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.dfm}

end.
