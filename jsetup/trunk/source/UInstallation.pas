unit UInstallation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, UDataModule, Upage;

type
  TInstallationForm = class(TPageForm)
    ProgressBar1: TProgressBar;
    Memo1: TMemo;
    Label1: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function GetNextPageIndex(const showGui : Boolean) : Integer; override;
  end;

var
  InstallationForm: TInstallationForm;

implementation

{$R *.dfm}

{ TInstallationForm }

function TInstallationForm.GetNextPageIndex(const showGui : Boolean): Integer;
begin
  GetNextPageIndex := NextArray[INSTALLATION_FORM];
end;

end.
