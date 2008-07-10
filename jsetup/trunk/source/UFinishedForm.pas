unit UFinishedForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UPage, StdCtrls;

type
  TFinishedForm = class(TPageForm)
    StaticText1: TStaticText;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function GetNextPageIndex(const showGui : Boolean) : Integer;override;
    procedure GetNextUpdate(Sender : TObject); override;
    procedure OnBack(Sender : TObject);  override;
    function IsFinished : Boolean; override;
  end;

var
  FinishedForm: TFinishedForm;

implementation

uses
  ActnList;

{$R *.dfm}

{ TFinishedForm }

function TFinishedForm.GetNextPageIndex(const showGui : Boolean): Integer;
begin
  result := -2;
end;

procedure TFinishedForm.GetNextUpdate(Sender: TObject);
begin
  inherited;

end;

function TFinishedForm.IsFinished: Boolean;
begin
  result := true;
end;

procedure TFinishedForm.OnBack(Sender: TObject);
begin
  inherited;
  
end;

end.
