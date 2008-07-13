unit UWelcomeForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UDataModule, UPage;

type
  TWelcomeForm = class(TPageForm)
    Label1: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function GetNextPageIndex(const showGui : Boolean) : Integer; override;
  end;

var
  WelcomeForm: TWelcomeForm;

implementation

{$R *.dfm}

{ TWelcomeForm }

function TWelcomeForm.GetNextPageIndex(const showGui : Boolean): Integer;
begin
  result := NextArray[WELCOME_FORM];
end;

end.
