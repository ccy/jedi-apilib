program TSAdminEx;

uses
  Forms,
  uMain in 'uMain.pas' {MainForm},
  uAbout in 'uAbout.pas' {AboutDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.HelpFile := 'tsadmin.chm';
  Application.Title := 'TSAdminEx';
{$IFDEF VER185} // Delphi 2007
  Application.MainFormOnTaskBar := True;
{$ENDIF}
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
