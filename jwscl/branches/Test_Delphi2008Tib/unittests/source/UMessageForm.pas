unit UMessageForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmMessage = class(TForm)
    memMessage: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmMessage: TfrmMessage;

procedure ShowMessageForm(Text : String);  

implementation

{$R *.dfm}

procedure ShowMessageForm(Text : String);
var frmMessage: TfrmMessage;
begin
  frmMessage := TfrmMessage.Create(Application.MainForm);
  try
    frmMessage.memMessage.Text := Text;
    frmMessage.ShowModal;
  finally
    frmMessage.Free;
  end;
end;

procedure TfrmMessage.Timer1Timer(Sender: TObject);
begin
  Close;
end;

procedure TfrmMessage.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Timer1.Enabled := false;
end;

end.
