unit UDelphiForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UPage, CheckScrollBox,DelphiVersionTool;

type
  TDelphiForm = class(TPageForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    JwaCheckScrollBox: TCheckScrollBox;
    JwsclCheckScrollBox: TCheckScrollBox;
    StaticText1: TStaticText;
    ErrorStaticText: TStaticText;
    SetChecksButton2: TButton;
    RemoveChecksButton2: TButton;
    ReverseChecksButton2: TButton;
    SetChecksButton1: TButton;
    RemoveChecksButton1: TButton;
    ReverseChecksButton1: TButton;
    Button1: TButton;
    procedure FormActivate(Sender: TObject);
    procedure SetChecksButton2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function GetNextPageIndex(const showGui : Boolean) : Integer; override;
    procedure GetNextUpdate(Sender : TObject); override;
  end;

var
  DelphiForm: TDelphiForm;

implementation

uses
  ActnList;

{$R *.dfm}

{ TDelphiForm }

procedure TDelphiForm.SetChecksButton2Click(Sender: TObject);
var
  i: Integer;
begin
  if TControl(Sender).Tag = 0 then  //JWA
  begin
    for i := 0 to JwaCheckScrollBox.Count - 1 do
    begin
      if Sender = SetChecksButton1 then
        JwaCheckScrollBox.Checked[i] := true
      else
      if Sender = RemoveChecksButton1 then
        JwaCheckScrollBox.Checked[i] := false
      else
         JwaCheckScrollBox.Checked[i] := not JwaCheckScrollBox.Checked[i];
    end;
  end
  else
  begin
    for i := 0 to JwsclCheckScrollBox.Count - 1 do
    begin
      if Sender = SetChecksButton2 then
        JwsclCheckScrollBox.Checked[i] := true
      else
      if Sender = RemoveChecksButton2 then
        JwsclCheckScrollBox.Checked[i] := false
      else
         JwsclCheckScrollBox.Checked[i] := not JwsclCheckScrollBox.Checked[i];
    end;
  end;

end;

procedure TDelphiForm.Button1Click(Sender: TObject);
begin
  JwaCheckScrollBox.Clear;
  JwsclCheckScrollBox.Clear;
  FormActivate(nil);
end;

procedure TDelphiForm.FormActivate(Sender: TObject);
var
  Delphi: TStringList;
  I, p: Integer;
  s : String;
begin
  if JwaCheckScrollBox.Count > 0 then
    exit;

  JwaCheckScrollBox.Clear;
  JwsclCheckScrollBox.Clear;

  Delphi := TStringList.Create;
  GetInstalledDelphiVersions(Delphi);

  try
    for I := 0 to Delphi.Count - 1 do
    begin
      p := JwaCheckScrollBox.Add(Delphi[i], Delphi.Objects[i]{int DelphiVer});

      p := JwsclCheckScrollBox.Add(Delphi[i], Delphi.Objects[i]{int DelphiVer});
    end;


    for I := 0 to Delphi.Count - 1 do
    begin
      JwaCheckScrollBox.Checked[i] := true;
      JwaCheckScrollBox.CheckBoxHint[i] := IntToStr(Integer(JwaCheckScrollBox.Objects[i]));
      S := JwaCheckScrollBox.CheckBoxHint[i];

      JwsclCheckScrollBox.Checked[i] := true;
      JwsclCheckScrollBox.CheckBoxHint[i] := IntToStr(Integer(JwsclCheckScrollBox.Objects[i]));
      S := JwsclCheckScrollBox.CheckBoxHint[i];
    end;


  finally
    Delphi.Free;
  end;
end;

function TDelphiForm.GetNextPageIndex(const showGui : Boolean): Integer;
begin
  result := 4;
end;

procedure TDelphiForm.GetNextUpdate(Sender: TObject);
  function IsAtLeastOnChecked : Boolean;
  var i : Integer;
  begin
    result := true;
    for i := 0 to JwaCheckScrollBox.Count - 1 do
    begin
      result := JwaCheckScrollBox.Checked[i];
      if result then
        exit;
    end;
  end;

  function IsJwaAndJwsclSelected : Boolean;
  var
    i : Integer;
    a,b : Boolean;
  begin
    result := true;
    for i := 0 to JwaCheckScrollBox.Count - 1 do
    begin
      a := JwaCheckScrollBox.Checked[i];
      b := JwsclCheckScrollBox.Checked[i];
      //if a delphi ver for jwscl is selected there must be also
      //the same delphi ver for jwa selected
      {
        1 + 1 = 1
        1 + 0 = 1
        0 + 1 = 0
        0 + 0 = 1
      }
      result := (a or not b);
        
      if not result then
      begin
        JwaCheckScrollBox.Labels[i].FocusControl.SetFocus;
        exit;
      end;
     // else
      //  JwaCheckScrollBox.Labels[i].Caption := JwaCheckScrollBox.Captions[i];
      {else
        JwaCheckScrollBox.Labels[i].Color := clBtnFace;}
    end;
  end;

var
 B : Boolean;

begin
  inherited;

  if (Sender is TAction) then
  begin
    B := IsAtLeastOnChecked and IsJwaAndJwsclSelected;
    (Sender as TAction).Enabled := B;
    ErrorStaticText.Visible := not B;
  end;
end;

end.
