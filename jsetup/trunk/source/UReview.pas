unit UReview;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Upage, StdCtrls;

type
  TReviewForm = class(TPageForm)
    Label1: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function GetNextPageIndex(const showGui : Boolean) : Integer; override;
  end;

var
  ReviewForm: TReviewForm;

implementation

{$R *.dfm}

{ TReviewForm }

function TReviewForm.GetNextPageIndex(const showGui : Boolean): Integer;
begin
  result := 6;
end;

end.
