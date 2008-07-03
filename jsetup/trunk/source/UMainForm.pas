unit UMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, UPage, UWelcomeForm, USetupTypeForm, UCheckoutForm,
  ExtCtrls, UPathForm, UDelphiForm, JvComponentBase, JvCreateProcess, ActnList;

type
  TMainForm = class(TForm)
    Button3: TButton;
    ButtonNext: TButton;
    ButtonBack: TButton;
    Panel1: TPanel;
    JvCreateProcess1: TJvCreateProcess;
    ActionList1: TActionList;
    ActionNext: TAction;
    procedure FormCreate(Sender: TObject);
    procedure ButtonNextClick(Sender: TObject);
    procedure ButtonBackClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionNextUpdate(Sender: TObject);
  private
    { Private-Deklarationen }
    Pages : Array of TPageForm;
    PageHistory : array of Integer;
    fPageIndex : Integer;
    fFirstShow : Boolean;

  public
    { Public-Deklarationen }


    procedure ShowPage(const Index : Integer);
    property PageIndex : Integer read fPageIndex write ShowPage;
  end;

var
  MainForm: TMainForm;

implementation

uses UInstallation, UReview;

{$R *.dfm}

procedure TMainForm.ActionNextUpdate(Sender: TObject);
var
  I: Integer;
begin
  ButtonBack.Enabled := fPageIndex > 0;

  if fPageIndex < High(Pages) then
  begin
    for I := 0 to Length(Pages) - 1 do
    begin
      if Assigned(Pages[I]) and
        Pages[i].Visible then
      begin
        Pages[i].GetNextUpdate(Sender);
      end;
    end;
  end
  else
    ActionNext.Enabled := false;
end;

procedure TMainForm.ButtonBackClick(Sender: TObject);
var i : Integer;
begin
  if Length(PageHistory) <= 0 then
   exit;
  i := PageHistory[high(PageHistory)];
  SetLength(PageHistory, Length(PageHistory)-1);

  ShowPage(i);
end;

procedure TMainForm.ButtonNextClick(Sender: TObject);
var P : TForm;
  idx, i: Integer;
begin
  i := Pages[fPageIndex].GetNextPageIndex;

  ActionNext.Enabled := i >= 0;

  if i < 0 then
    exit;

  idx := fPageIndex;
  ShowPage(i);

  SetLength(PageHistory, Length(PageHistory)+1);
  PageHistory[high(PageHistory)] := idx;
end;

procedure TMainForm.FormCreate(Sender: TObject);

procedure AddPage(ClassType: TComponentClass; var Reference);
var Comp : TForm;
begin
  Comp := TForm(ClassType.Create(Self));
  Comp.Parent := Panel1;
  Comp.BorderIcons := [];
  Comp.SetBounds(0,0, Panel1.ClientWidth, Panel1.ClientHeight);
  Comp.BorderStyle := bsNone;
  //Page.Align := alClient;
  //Page.Color := clNavy;

  SetLength(Pages, Length(Pages)+1);
  TForm(Pages[high(Pages)]) := Comp;

  TComponent(Reference) := Comp;
end;

begin
  fFirstShow := true;
  
  AddPage(TWelcomeForm, WelcomeForm);  //0
  AddPage(TSetupTypeForm, SetupTypeForm); //1
  AddPage(TCheckoutForm, CheckoutForm); //2
  AddPage(TDelphiForm, DelphiForm); //3
  AddPage(TPathForm, PathForm); //4
  AddPage(TReviewForm, ReviewForm); //5
  AddPage(TInstallationForm, InstallationForm); //6



end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if fFirstShow then
  begin
    ShowPage(2);
    fFirstShow := not fFirstShow;
  end;
end;


procedure TMainForm.ShowPage(const Index: Integer);
var
  I: Integer;
begin
  for I := 0 to Length(Pages) - 1 do
  begin
    if Assigned(Pages[I]) and
      Assigned(Pages[i].OnDeactivate) and
      Pages[i].Visible then
    begin
      Pages[i].OnDeactivate(Self);
    end;
    if Assigned(Pages[I]) then
      Pages[i].Visible := False;
  end;
  if Assigned(Pages[Index].OnActivate) then
    Pages[Index].OnActivate(Self);
  Pages[Index].Visible := true;
  fPageIndex := Index;
end;

end.
