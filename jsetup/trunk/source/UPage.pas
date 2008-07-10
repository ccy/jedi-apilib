unit UPage;

interface
uses Forms, Classes;

type
  TPageForm = class(TForm)
  public
    function GetNextPageIndex(const showGui : Boolean) : Integer; virtual; abstract;
    procedure GetNextUpdate(Sender : TObject); virtual;

    function IsFinished : Boolean; virtual;

    procedure OnBack(Sender : TObject); virtual;
  end;

implementation

{ TPageForm }

procedure TPageForm.GetNextUpdate(Sender: TObject);
begin
  //
end;

function TPageForm.IsFinished: Boolean;
begin
  result := false;
end;

procedure TPageForm.OnBack(Sender: TObject);
begin

end;

end.
