{******************************************************************************}
{ JEDI Virtual Disk API example                                                }
{ http://jedi-apilib.sourceforge.net                                           }
{ http://wiki.delphi-jedi.org/                                                 }
{ http://blog.delphi-jedi.net/                                                 }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ Author(s):                                                                   }
{ Creation date: November 4th 2010                                             }
{ Last modification date: November 28th 2010                                   }
{                                                                              }
{ Description: Demonstrates how to use the virtual disk API                    }
{              with Windows 7                                                  }
{                                                                              }
{ Preparations: JWA must be ready to use.                                      }
{                                                                              }
{ Version history: November 4th 2010: initial version                          }
{                                                                              }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in         }
{ production environments.                                                     }
{******************************************************************************}
program Demo;

uses
  Forms,
  frmMain in 'frmMain.pas' {MainForm},
  checkwin7 in 'checkwin7.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
