unit UPage;

interface
uses Forms, Classes;

type
  TPageForm = class(TForm)
  public
    function GetNextPageIndex : Integer; virtual; abstract;
    procedure GetNextUpdate(Sender : TObject); virtual; 
  end;

implementation

{ TPageForm }

procedure TPageForm.GetNextUpdate(Sender: TObject);
begin
  //
end;

end.
