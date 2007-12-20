program TSAdminEx;

uses
{$IFDEF FASTMM}
  FastMM4,
{$ENDIF FASTMM}
  Forms,
  uMain in 'uMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.HelpFile := 'tsadmin.chm';
  Application.Title := 'TSAdminEx';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
