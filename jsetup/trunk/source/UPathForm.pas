unit UPathForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UPage, Mask, JvExMask, JvToolEdit, ActnList;

type
  TPathForm = class(TPageForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    JvDirectoryJwa: TJvDirectoryEdit;
    JvDirectoryJwscl: TJvDirectoryEdit;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function GetNextPageIndex : Integer;override;
    procedure GetNextUpdate(Sender : TObject); override;
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

procedure TPathForm.GetNextUpdate(Sender: TObject);
begin
  inherited;
  if (Sender is TAction) then
  begin
    (Sender as TAction).Enabled :=
      (Length(Trim(JvDirectoryJwa.Text)) > 0) and
      (Length(Trim(JvDirectoryJwscl.Text)) > 0) and
      DirectoryExists(JvDirectoryJwa.Text) and
      DirectoryExists(JvDirectoryJwscl.Text);
  end;
end;

end.
