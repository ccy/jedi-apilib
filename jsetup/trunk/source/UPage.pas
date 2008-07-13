unit UPage;

interface
uses Forms, Classes, UDataModule;

type
  TPageForm = class(TForm)
  public
    function GetNextPageIndex(const showGui : Boolean) : Integer; virtual; abstract;
    procedure GetNextUpdate(Sender : TObject); virtual;

    function IsFinished : Boolean; virtual;

    procedure OnBack(Sender : TObject); virtual;

    procedure OnGetData(const DataModul : TSetupDataModule); virtual;
    procedure OnSetData(const DataModul : TSetupDataModule); virtual;
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

procedure TPageForm.OnGetData(const DataModul: TSetupDataModule);
begin

end;

procedure TPageForm.OnSetData(const DataModul: TSetupDataModule);
begin

end;

end.
