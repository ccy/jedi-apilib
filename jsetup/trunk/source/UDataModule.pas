unit UDataModule;

interface

uses
  JwaWindows, Forms,
  SysUtils, Classes, Dialogs, DelphiVersionTool;

type
  TSetupType = (stCheckout, stUpdate, stRemove);
  TProjectType = (ptJWA, ptJWSCL);
  TProjectTypes = set of TProjectType;

  TSetupDataModule = class(TDataModule)
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    fSetupType : TSetupType;
    fProjectTypes : TProjectTypes;

    fTargetPath : String;
    fTargetDelphiJwa,
    fTargetDelphiJwscl : TList; //array of index to KnownDelphiVersions for selected delphi

    fJwaRevision,
    fJwsclRevision : Integer;

    fJwaReleaseSvnPath,
    fJwsclReleaseSvnPath : String;
  public
    function GetVersions(const ProjectType : TProjectType) : TStringList;

    { Public-Deklarationen }
    property SetupType : TSetupType read fSetupType write fSetupType;

    property TargetPath : String read fTargetPath write fTargetPath;

    property JwaRevision   : Integer read fJwaRevision write fJwaRevision;
    property JwsclRevision : Integer read fJwsclRevision write fJwsclRevision;

    property JwaReleaseSvnPath   : String read fJwaReleaseSvnPath write fJwaReleaseSvnPath;
    property JwsclReleaseSvnPath : String read fJwsclReleaseSvnPath write fJwsclReleaseSvnPath;

    property TargetDelphiJwa   : TList read fTargetDelphiJwa write fTargetDelphiJwa;
    property TargetDelphiJwscl : TList read fTargetDelphiJwscl write fTargetDelphiJwscl;
  end;

var
  SetupDataModule: TSetupDataModule;

const
  WELCOME_FORM        = 0;
  SETUP_TYPE_FORM     = 1;
  CHECKOUT_FORM       = 2;
  DELPHI_FORM         = 3;
  JWA_TYPE_FORM       = 4;
  PATH_FORM           = 5;
  REVIEW_FORM         = 6;
  INSTALLATION_FORM   = 7;
  FINISHED_FORM       = 8;

const NextArray : array[WELCOME_FORM..FINISHED_FORM-1] of byte =
    (
      {WELCOME_FORM->}SETUP_TYPE_FORM,
      {SETUP_TYPE_FORM->}CHECKOUT_FORM,
      {CHECKOUT_FORM->}DELPHI_FORM,
      {DELPHI_FORM->}JWA_TYPE_FORM,
      {JWA_TYPE_FORM->}PATH_FORM,
      {PATH_FORM->}REVIEW_FORM,
      {REVIEW_FORM->}INSTALLATION_FORM,
      {INSTALLATION_FORM->}FINISHED_FORM
    ) ;

implementation

{$R *.dfm}

{ TSetupDataModule }

procedure TSetupDataModule.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(fTargetDelphiJwa);
  FreeAndNil(fTargetDelphiJwscl);
end;

function TSetupDataModule.GetVersions(
  const ProjectType: TProjectType): TStringList;
begin

end;

end.
