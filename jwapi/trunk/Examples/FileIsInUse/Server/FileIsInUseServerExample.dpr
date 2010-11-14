{******************************************************************************}
{ JEDI FileIsInUse Example Project                                             }
{ http://jedi-apilib.sourceforge.net                                           }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ Author(s): Christian Wimmer                                                  }
{ Creation date: 6th October 2010                                              }
{ Last modification date: 6th October 2010                                     }
{                                                                              }
{ Description: Shows how to use the IFileIsInUse API                           }
{                                                                              }
{ Preparations: JWA must be ready to use.                                      }
{               Requires at least Windows Vista                                }
{                                                                              }
{ Version history: 14th November 2010 initial release                          }
{                                                                              }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in         }
{ productive environments.                                                     }
{ The code has surely some errors that need to be fixed. In such a case        }
{ you can contact the author(s) through the JEDI API hompage, the mailinglist  }
{ or via the article link.                                                     }
{                                                                              }
{******************************************************************************}
program FileIsInUseServerExample;

uses
  Forms,
  MainForm in 'MainForm.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
