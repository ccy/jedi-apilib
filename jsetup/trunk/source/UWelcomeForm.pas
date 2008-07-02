unit UWelcomeForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UPage;

type
  TWelcomeForm = class(TPageForm)
    Label1: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function GetNextPageIndex : Integer; override;
  end;

var
  WelcomeForm: TWelcomeForm;

implementation

{$R *.dfm}

{ TWelcomeForm }

function TWelcomeForm.GetNextPageIndex: Integer;
begin
  result := 1;
end;

end.
