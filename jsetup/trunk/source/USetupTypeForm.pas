unit USetupTypeForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UDataModule,UPage;

type
  TSetupTypeForm = class(TPageForm)
    UpdateRadioButton: TRadioButton;
    CheckoutRadioButton: TRadioButton;
    RemoveRadioButton: TRadioButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
     function GetNextPageIndex(const showGui : Boolean) : Integer; override;

     procedure GetNextUpdate(Sender : TObject); override;

     procedure OnGetData(const DataModul : TSetupDataModule); override;
    procedure OnSetData(const DataModul : TSetupDataModule); override;
  end;

var
  SetupTypeForm: TSetupTypeForm;

implementation

uses
  ActnList;

{$R *.dfm}

{ TSetupTypeForm }

//SetupDataModule

function TSetupTypeForm.GetNextPageIndex(const showGui : Boolean): Integer;
begin
  result := NextArray[SETUP_TYPE_FORM];
end;

procedure TSetupTypeForm.GetNextUpdate(Sender: TObject);
begin
  inherited;
  if (Sender is TAction) then
  begin
    (Sender as TAction).Enabled := CheckoutRadioButton.Checked xor
       UpdateRadioButton.Checked xor RemoveRadioButton.Checked;
  end;
end;

procedure TSetupTypeForm.OnGetData(const DataModul: TSetupDataModule);
begin
  case DataModul.SetupType of
    stCheckout : CheckoutRadioButton.Checked := true;
    stUpdate   : UpdateRadioButton.Checked := true;
    stRemove   : RemoveRadioButton.Checked := true;
  end;
end;

procedure TSetupTypeForm.OnSetData(const DataModul: TSetupDataModule);
begin
  if CheckoutRadioButton.Checked then
    DataModul.SetupType := stCheckout
  else
  if UpdateRadioButton.Checked then
    DataModul.SetupType := stUpdate
  else
  if RemoveRadioButton.Checked then
    DataModul.SetupType := stRemove;
end;

end.
