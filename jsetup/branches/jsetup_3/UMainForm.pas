unit UMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, UTestFrame;

type
  TForm1 = class(TForm)
    pgc1: TPageControl;
    ts1: TTabSheet;
    ts2: TTabSheet;
    procedure ts1Show(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    Frm : TFrame2;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ts1Show(nil);
  pgc1.ActivePageIndex := 0;
end;

procedure TForm1.ts1Show(Sender: TObject);
begin
  if not Assigned(Frm) then
    Frm := TFrame2.Create(ts1);
  Frm.Parent := ts1;
  Frm.Align := alClient;
  Frm.Visible := true;
end;

end.
