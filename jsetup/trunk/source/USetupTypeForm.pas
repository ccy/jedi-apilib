unit USetupTypeForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UPage;

type
  TSetupTypeForm = class(TPageForm)
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
     function GetNextPageIndex : Integer; override;
  end;

var
  SetupTypeForm: TSetupTypeForm;

implementation

{$R *.dfm}

{ TSetupTypeForm }

function TSetupTypeForm.GetNextPageIndex: Integer;
begin
  result := 2;
end;

end.
