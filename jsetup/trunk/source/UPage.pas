unit UPage;

interface
uses Forms;

type
  TPageForm = class(TForm)
  public
    function GetNextPageIndex : Integer; virtual; abstract;
  end;

implementation

end.
