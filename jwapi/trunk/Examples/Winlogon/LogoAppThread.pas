{******************************************************************************}
{ JEDI API example WinLogon Notification Package  			       }
{ http://jedi-apilib.sourceforge.net					       }
{ 									       }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{ 									       }
{ Author(s): Christian Wimmer, stOrM!					       }
{ Creation date: 23th May 2008 					 	       }
{ Last modification date:	26th May 2008				       }
{ 									       }
{ Description: Demonstrates how to create a Winlogon Notification Package and  }
{    draws a transparent window inside Winlogon Process	containing a 	       }
{ 	 PNG Image							       }
{ Preparations: JwaWindows, any layer based graphic apllication e.g. Gimp or   }
{ 				Adobe Photoshop				       }
{ Article link:   							       }
{ http://blog.delphi-jedi.net/2008/05/24/				       }
{     how-to-create-a-winlogon-notification-package			       }
{ Version history: 23/05/2008 first release        			       }
{ 									       }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in 	       }
{ productive environments.						       }
{ The code has surely some errors that need to be fixed. In such a case	       }
{ you can contact the author(s) through the JEDI API hompage, the mailinglist  }
{ or via the article link.						       }
{ 				                			       }
{ The JEDI API Logo is copyrighted and must not be used without permission     }
{******************************************************************************}

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
