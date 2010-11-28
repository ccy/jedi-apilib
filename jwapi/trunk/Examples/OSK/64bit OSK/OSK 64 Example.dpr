{******************************************************************************}
{ JEDI FileIsInUse   Example Project                                           }
{ http://jedi-apilib.sourceforge.net                                           }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ Author(s): Christian Wimmer                                                  }
{                                                                              }
{ Description: Shows how to call the OnScreenKeyboard on a Win64Bit.           }
{              Works only on 64Bit.                                            }
{                                                                              }
{ Preparations: JWA must be ready to use.                                      }
{                                                                              }
{                                                                              }
{ Version history: 28th November 2010 initial release                          }
{                                                                              }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in         }
{ productive environments.                                                     }
{ The code has surely some errors that need to be fixed. In such a case        }
{ you can contact the author(s) through the JEDI API hompage, the mailinglist  }
{ or via the article link.                                                     }
{                                                                              }
{******************************************************************************}
uses
	ActiveX,
  SysUtils,
	JwaWinType,
	JwaWinBase,
	JwaShellAPI,
	JwaSHFolder,
	JwaShlObj;


//function Wow64DisableWow64FsRedirection(out OldValue: PVOID): BOOL; stdcall; external 'Kernel32.dll';
//function Wow64RevertWow64FsRedirection(const OldValue: PVOID): BOOL; stdcall; external 'Kernel32.dll';

procedure RunOnScreenKeyBoard;
  function GetNativeWindowsDirectory : String;
  var
    P : array[0..MAX_PATH] of Char;
  begin
    SHGetFolderPath(0, CSIDL_SYSTEM, 0, SHGFP_TYPE_DEFAULT, @P);

    result := P;
  end;
var
  oldValue : Pointer;
  Path : String;
  hMpr_DLL : HMODULE;
  ShInfo : SHELLEXECUTEINFO;
begin
  //CoInitializeEx(nil, COINIT_MULTITHREADED or COINIT_DISABLE_OLE1DDE);

  hMpr_DLL := LoadLibrary('mpr.dll');
  try
    if not Wow64DisableWow64FsRedirection(oldValue) then
      RaiseLastOSError;
    try
      Path := GetNativeWindowsDirectory + '\osk.exe';

      ZeroMemory(@ShInfo, sizeof(ShInfo));
      ShInfo.cbSize := sizeof(ShInfo);
      ShInfo.lpVerb := 'open';
      ShInfo.fMask := SEE_MASK_FLAG_NO_UI or SEE_MASK_NOASYNC;
      ShInfo.lpFile := PChar(Path);

      if not ShellExecuteEx(ShInfo) then
        RaiseLastOSError;
    finally
       Wow64RevertWow64FsRedirection(oldValue);
    end;
  finally
    FreeLibrary(hMpr_DLL);
  end;
end;

begin
  RunOnScreenKeyBoard;
end.