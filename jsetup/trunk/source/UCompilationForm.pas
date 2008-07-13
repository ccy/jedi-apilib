unit UCompilationForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UPage;

type
  TCompilationForm = class(TPageForm)
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function GetNextPageIndex : Integer;override;
    procedure GetNextUpdate(Sender : TObject); override;    
  end;

var
  Form3: TCompilationForm;

implementation

{$R *.dfm}

{ TCompilationForm }

function TCompilationForm.GetNextPageIndex: Integer;
begin
  result := -1;
end;

procedure TCompilationForm.GetNextUpdate(Sender: TObject);
begin
  inherited;

end;

end.
