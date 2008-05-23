unit LogoAppThread;

interface

uses
  Forms,
  Dialogs,
  Classes,
  windows,
  unit1;

type
  TLogoThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure OnIdle(Sender : TObject; var Done : Boolean);
    procedure Execute; override;
  end;

implementation

procedure TLogoThread.Execute;
begin
  try
   begin
     Application := TApplication.Create(nil);
     Application.Initialize;
     Application.CreateForm(TForm1, Form1);
     Application.OnIdle := OnIdle;
     Application.Run;
  end;
  finally
  end;
end;

procedure TLogoThread.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if Self.Terminated then
  begin
    try
     Application.MainForm.Close;
    except
     end;
   end;
end;

end.
