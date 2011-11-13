{******************************************************************************}
{ JEDI File Transactions API example                                           }
{ http://jedi-apilib.sourceforge.net                                           }
{ http://wiki.delphi-jedi.org/                                                 }
{ http://blog.delphi-jedi.net/                                                 }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ Author(s): Olaf Hess (olafhess at newsguy dot com)                           }
{ Creation date: December 1st 2009                                             }
{ Last modification date: November 28th 2010                                   }
{                                                                              }
{ Description: Demonstrates how to use the file system transactions introduced }
{              with Windows Vista / Server 2008                                }
{                                                                              }
{ Preparations: JWA must be ready to use.                                      }
{                                                                              }
{ Version history: December 1st 2009: initial version                          }
{                  November 28th 2010: adapted to use JWA                      }
{                                                                              }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in         }
{ production environments.                                                     }
{******************************************************************************}

//----------------------------------------------------------------------------//
// This code was first published as part of the article                       //
// "Transaktionen mit Vista und NTFS" ("Transactions with Vista and NTFS")    //
// in issue 1 / 2010 of "Toolbox Magazin" (http://www.toolbox-mag.de) written //
// by Olaf Hess. Used with permission. Donated to the JEDI API Project.       //
//----------------------------------------------------------------------------//

// Program tested with Delphi 7 and Delphi 2009

{$I ..\..\Includes\jproject\jedi.inc}

program TransactionsDemo;

uses
  Forms,
  SysUtils,
  Windows,
  MainFormU in 'MainFormU.pas' {DemoForm},
  GetFolderDialogU in 'GetFolderDialogU.pas',
  TransactionClassU in 'TransactionClassU.pas',
  JwaKtmW32;

{$R *.res}
{$R ExecutionLevel_AsInvoker.RES}

(* ---- *)

procedure CheckWindowsVersion;

const
    cWrongVersion = 'This program requires at least Windows Vista or ' +
                    'Windows Server 2008!';

begin
    if (Win32Platform <> VER_PLATFORM_WIN32_NT) or
       (Win32MajorVersion < 6) then
    begin
        MessageBox (GetDesktopWindow, cWrongVersion, PChar (GetAppTitle),
                    mb_OK or mb_IconStop);
        Halt (1);
    end; { if }
end; { CheckWindowsVersion }

(* ---- *)

begin { TransactionsDemo }
  CheckWindowsVersion;

  Application.Initialize;
  Application.CreateForm(TDemoForm, DemoForm);
  Application.Run;
end.

