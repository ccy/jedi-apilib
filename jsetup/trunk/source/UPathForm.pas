unit UPathForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UPage;

type
  TPathForm = class(TPageForm)
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Edit2: TEdit;
    Label2: TLabel;
    Button2: TButton;
    Label3: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function GetNextPageIndex : Integer;override;
  end;

var
  PathForm: TPathForm;

implementation

{$R *.dfm}

{ TForm1 }

function TPathForm.GetNextPageIndex: Integer;
begin
  result := 5;
end;

end.
