unit CredentialsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, JwsclStrings, JvExControls,
  JvLookOut, JvExExtCtrls, JvBevel, JvButton, JvTransparentButton;

type
  TFormCredentials = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    EditPassword1: TEdit;
    Image2: TImage;
    EditPassword2: TEdit;
    UsersComboBox: TComboBox;
    CheckBoxSaveLogon: TCheckBox;
    ButtonDefaultUser: TJvTransparentButton;
    JvBevel1: TJvBevel;
    JvBevel2: TJvBevel;
    ButtonUser: TJvTransparentButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure EditPassword1Change(Sender: TObject);
    procedure EditPassword2Change(Sender: TObject);
    procedure ButtonDefaultUserClick(Sender: TObject);
    procedure ButtonUserClick(Sender: TObject);
    procedure UsersComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxSaveLogonClick(Sender: TObject);
  private
    { Private-Deklarationen }
    fSaveLogon : Boolean;
    fUserName,
    fPassword : TJwString;
    internalMsg : Boolean;
  public
    procedure CenterInMonitor(const i : Integer);
    { Public-Deklarationen }
    property SaveLogon : Boolean read fSaveLogon write fSaveLogon;
    property UserName : TJwString read fUserName write fUserName;
    property Password : TJwString read fPassword write fPassword;
  end;

var
  FormCredentials: TFormCredentials;

implementation

{$R *.dfm}

procedure TFormCredentials.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  SaveLogon := CheckBoxSaveLogon.Checked;
//  UserName := 



  Application.Terminate;
end;

procedure TFormCredentials.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var bButtons,
    bDefaultUserPassword,
    bUser : Boolean;
begin
  CanClose := true;
  if ModalResult = mrOk then
  begin
    try
      bButtons := (ButtonDefaultUser.Down xor ButtonUser.Down);
      bDefaultUserPassword :=
        ButtonDefaultUser.Down and (Length(Trim(EditPassword1.Text)) > 0);
      bUser := ButtonUser.Down and (Length(Trim(UsersComboBox.Text)) > 0) and
              (Length(Trim(EditPassword2.Text)) > 0);

      CanClose := bButtons and (bDefaultUserPassword or bUser);
    finally
      //if not CanClose then
      //Do info here
    end;

    if CanClose then
    begin
      fSaveLogon := CheckBoxSaveLogon.Checked;
      if ButtonUser.Down then
      begin
        fUserName  := UsersComboBox.Text;
        fPassword  := EditPassword2.Text;
      end
      else
        fPassword  := EditPassword1.Text;
    end;
  end;
end;

procedure TFormCredentials.EditPassword1Change(Sender: TObject);
begin
  if internalMsg then
    exit;
  internalMsg := true;
  begin
    ButtonDefaultUser.Click;
  end;
  internalMsg := false;
end;

procedure TFormCredentials.EditPassword2Change(Sender: TObject);
begin
  if internalMsg then
    exit;
  internalMsg := true;
  begin
    ButtonUser.Click;
  end;
  internalMsg := false;
end;

procedure TFormCredentials.ButtonDefaultUserClick(Sender: TObject);
begin
  if internalMsg then
    exit;
  internalMsg := true;
  begin
    EditPassword1.SetFocus;
  end;
  internalMsg := false;
end;

procedure TFormCredentials.ButtonUserClick(Sender: TObject);
begin
  if internalMsg then
    exit;
  internalMsg := true;
  begin
    if Length(Trim(UsersComboBox.Text)) = 0 then
      UsersComboBox.SetFocus
    else
      EditPassword2.SetFocus;
  end;
  internalMsg := false;
end;

procedure TFormCredentials.UsersComboBoxChange(Sender: TObject);
begin
  if internalMsg then
    exit;
  internalMsg := true;
  begin
    ButtonUser.Click;
  end;
  internalMsg := false;
end;

procedure TFormCredentials.FormCreate(Sender: TObject);
begin
  internalMsg := false;
end;

procedure TFormCredentials.CenterInMonitor(const i : Integer);
var SX, SY : Integer;
begin
  SX := abs(abs(Screen.Monitors[I].BoundsRect.Right) - abs(Screen.Monitors[I].BoundsRect.Left));
  Left := Screen.Monitors[I].Left + ((SX div 2) - (Width div 2));

  SY := abs(abs(Screen.Monitors[I].BoundsRect.Bottom) - abs(Screen.Monitors[I].BoundsRect.Top));
  Top := Screen.Monitors[I].Top + ((SY div 2) - (Height div 2));

  exit;
  SY := Screen.Monitors[I].BoundsRect.Top - Screen.Monitors[I].BoundsRect.Bottom;
  SetBounds(Screen.Monitors[I].Left + SX, //+ ((SX - Width) div 2),
                    Top,
                    //(((Screen.Monitors[I].BoundsRect.Bottom - Screen.Monitors[I].BoundsRect.Top)- Height) div 2),
                     Width, Height);
//  Left := 0;
end;

procedure TFormCredentials.CheckBoxSaveLogonClick(Sender: TObject);
begin
  {if CheckBoxSaveLogon.Checked then
    MakeFullyVisible(Screen.Monitors[1])
  else
    MakeFullyVisible(Screen.Monitors[0]);
  DefaultMonitor := dmPrimary;
  Position := poScreenCenter;   }

end;

end.
