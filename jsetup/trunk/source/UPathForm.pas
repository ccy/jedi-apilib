unit UPathForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UPage, Mask, JvExMask, JvToolEdit, ActnList, ComCtrls, 
  UDataModule;

type
  TPathForm = class(TPageForm)
    Label1: TLabel;
    Label3: TLabel;
    JvDirectoryJwa: TJvDirectoryEdit;
    StaticText1: TStaticText;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private-Deklarationen }
    function CheckForPathAccess : Integer;

  public
    { Public-Deklarationen }
    function GetNextPageIndex(const showGui : Boolean) : Integer;override;
    procedure GetNextUpdate(Sender : TObject); override;

    procedure OnGetData(const DataModul : TSetupDataModule); override;
    procedure OnSetData(const DataModul : TSetupDataModule); override;
  end;

var
  PathForm: TPathForm;

implementation

{$R *.dfm}

{ TForm1 }

function TPathForm.CheckForPathAccess: Integer;
begin
  result := mrOK;
  if (Length(Trim(JvDirectoryJwa.Text)) > 0) and not DirectoryExists(JvDirectoryJwa.Text) then
  begin
    result := MessageDlg(Format('"%s" does not exist. Create?',[JvDirectoryJwa.Text]), mtConfirmation, [mbYes, mbNo], 0);
    case result of
      mrYes : if not ForceDirectories(JvDirectoryJwa.Text) then
       begin
         MessageDlg('Could not create "%s". Make sure you have write access or select another path. Setup cannot continue.', mtError, [mbOK], 0);
       end;
      mrCancel,
      mrNo : ;
    end;
  end;
end;

procedure TPathForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  //
  if not Assigned(Sender) then
  begin
    CanClose := (Length(Trim(JvDirectoryJwa.Text)) > 0) and
      DirectoryExists(JvDirectoryJwa.Text);
  end;
end;

function TPathForm.GetNextPageIndex(const showGui : Boolean): Integer;
begin
  if not DirectoryExists(JvDirectoryJwa.Text) then
  begin
    case CheckForPathAccess of
      mrOK : result := GetNextPageIndex(showGUI);
    else
      result := -1;
    end;
  end
  else
    result := NextArray[PATH_FORM];
end;

procedure TPathForm.GetNextUpdate(Sender: TObject);
begin
  inherited;
  if (Sender is TAction) then
  begin
    (Sender as TAction).Enabled :=
      (Length(Trim(JvDirectoryJwa.Text)) > 0){ and
      DirectoryExists(JvDirectoryJwa.Text)};
  end;
end;

procedure TPathForm.OnGetData(const DataModul: TSetupDataModule);
begin
  inherited;

  JvDirectoryJwa.Text := DataModul.TargetPath;
end;

procedure TPathForm.OnSetData(const DataModul: TSetupDataModule);
begin
  DataModul.TargetPath := JvDirectoryJwa.Text;
end;

end.
