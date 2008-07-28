unit UDataModule;

interface

uses
  JwaWindows, Forms,
  SysUtils, Classes, Dialogs, DelphiVersionTool;

type
  TSetupType = (stCheckout, stUpdate, stRemove);
  TProjectType = (ptJWA, ptJWSCL);
  TJwaType = (wtSingle, wtDcu);

  TProjectTypes = set of TProjectType;

  PJwaTypeRecord = ^TJwaTypeRecord;
  TJwaTypeRecord = record
    DelphiIndex : Integer;
    IsSingle : Boolean;
  end;

  TJwaTypesList = class(TList)
  protected
    function GetSingleJwa(DelphiIndex: Integer) : Boolean;
    procedure SetSingleJwa(DelphiIndex: Integer; IsSingleJwa :  Boolean); 
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); reintroduce;

    {DelphiIndex = Delphi version}
    property IsSingleJwa[DelphiIndex : Integer] : Boolean read GetSingleJwa write SetSingleJwa;
  end;

  TSetupDataModule = class(TDataModule)
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    fSetupType : TSetupType;
    fProjectTypes : TProjectTypes;

    fTargetPath : String;
    fJwaTypes : TJwaTypesList;
    fTargetDelphiJwa,
    fTargetDelphiJwscl : TList; //contains delphi version as found in KnownDelphiVersions

    fJwaRevision,
    fJwsclRevision : Integer;

    fJwaReleaseSvnPath,
    fJwsclReleaseSvnPath : String;
  public
    function GetVersions(const ProjectType : TProjectType) : TStringList;

    function TargetDelphiVerToKnownDelphiIndex(const DelphiVer : Integer) : Integer;

    { Public-Deklarationen }
    property SetupType : TSetupType read fSetupType write fSetupType;

    property TargetPath : String read fTargetPath write fTargetPath;

    property JwaRevision   : Integer read fJwaRevision write fJwaRevision;
    property JwsclRevision : Integer read fJwsclRevision write fJwsclRevision;

    property JwaReleaseSvnPath   : String read fJwaReleaseSvnPath write fJwaReleaseSvnPath;
    property JwsclReleaseSvnPath : String read fJwsclReleaseSvnPath write fJwsclReleaseSvnPath;

    {Contains a list of integers with the Delphi version.}
    property TargetDelphiJwa   : TList read fTargetDelphiJwa write fTargetDelphiJwa;
    property TargetDelphiJwscl : TList read fTargetDelphiJwscl write fTargetDelphiJwscl;

    property JwaTypes : TJwaTypesList read fJwaTypes write fJwaTypes;

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

procedure TSetupDataModule.DataModuleCreate(Sender: TObject);
begin
  fJwaTypes := TJwaTypesList.Create;
end;

procedure TSetupDataModule.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(fTargetDelphiJwa);
  FreeAndNil(fTargetDelphiJwscl);

  fJwaTypes.Clear;
  FreeAndNil(fJwaTypes);
end;

function TSetupDataModule.GetVersions(
  const ProjectType: TProjectType): TStringList;
begin

end;

function TSetupDataModule.TargetDelphiVerToKnownDelphiIndex(
  const DelphiVer: Integer): Integer;
var i : Integer;
begin
  result := -1;
  for i := low(KnownDelphiVersions) to High(KnownDelphiVersions) do
    if KnownDelphiVersions[i].Ver = DelphiVer then
    begin
      result := i;
      exit;
    end;
end;


{ TJwaTypesList }

procedure TJwaTypesList.Clear;
var i : Integer;
begin
  for i := Count - 1 downto 0 do
    Delete(i);
  inherited Clear;
end;

procedure TJwaTypesList.Delete(Index: Integer);
var p : PJwaTypeRecord;
begin
  p := PJwaTypeRecord(Items[Index]);
  Dispose(p);
  Items[Index] := nil;
  inherited Delete(Index);
end;

function TJwaTypesList.GetSingleJwa(DelphiIndex: Integer): Boolean;
var
  I: Integer;

begin
  result := false;
  for I := 0 to Count - 1 do
  begin
    if PJwaTypeRecord(Items[i])^.DelphiIndex = DelphiIndex then
    begin
      result := PJwaTypeRecord(Items[i])^.IsSingle;
      exit;
    end;
  end;
end;

procedure TJwaTypesList.SetSingleJwa(DelphiIndex: Integer;
  IsSingleJwa: Boolean);
var
  I: Integer;
  p : PJwaTypeRecord;
begin
  for I := 0 to Count - 1 do
  begin
    if PJwaTypeRecord(Items[i])^.DelphiIndex = DelphiIndex then
    begin
      PJwaTypeRecord(Items[i])^.IsSingle := IsSingleJwa;
      exit;
    end;
  end;

  New(P);
  P^.DelphiIndex := DelphiIndex;
  P^.IsSingle := IsSingleJwa;
  Add(P);
end;


end.
