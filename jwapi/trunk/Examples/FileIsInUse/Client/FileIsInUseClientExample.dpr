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
{ Description: Shows how to use the IFileIsInUse API to retrieve an owner of   }
{              a locked file.                                                  }
{                                                                              }
{ Preparations: JWA must be ready to use.                                      }
{               Requires at least Windows Vista                                }
{                                                                              }
{ Version history: 6th October 2010 initial release                            }
{                                                                              }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in         }
{ productive environments.                                                     }
{ The code has surely some errors that need to be fixed. In such a case        }
{ you can contact the author(s) through the JEDI API hompage, the mailinglist  }
{ or via the article link.                                                     }
{                                                                              }
{******************************************************************************}
program FileIsInUseClientExample;


{Define this switch to use the definition of the IFileIsInUse interface from
 the JEDI API units.
 Undefine it, to use it from the file here.
}
{.$DEFINE JWA_BUILTIN_IFILEISINUSE}

uses
  ComObj,
  ActiveX,
  SysUtils,
  JwaWinType,
  JwaWinUser
{$IFDEF JWA_BUILTIN_IFILEISINUSE}
  ,JwaShlObj
{$ENDIF JWA_BUILTIN_IFILEISINUSE}
  ;

{$IFNDEF JWA_BUILTIN_IFILEISINUSE}
{$ALIGN 4}
const
  IID_IFileIsInUse: TGUID = (
    D1:$64a1cbf0; D2:$3a1a; D3:$4461; D4:($91,$58,$37,$69,$69,$69,$39,$50));

type
  tagFILE_USAGE_TYPE = (
    FUT_PLAYING = 0,
    FUT_EDITING = 1,
    FUT_GENERIC = 2
  );
  FILE_USAGE_TYPE = tagFILE_USAGE_TYPE;
  TFileUsageType = FILE_USAGE_TYPE;

const
  OF_CAP_CANSWITCHTO     = $0001;
  OF_CAP_CANCLOSE        = $0002;

type
  IFileIsInUse = interface(IUnknown)
    ['{64a1cbf0-3a1a-4461-9158-376969693950}']
    function GetAppName(out ppszName: LPWSTR) : HRESULT; stdcall;
    function GetUsage(out pfut : FILE_USAGE_TYPE) : HRESULT; stdcall;
    function GetCapabilities(out pdwCapFlags : DWORD) : HRESULT; stdcall;
    function GetSwitchToHWND(out phwnd : HWND) : HRESULT; stdcall;
    function CloseFile() : HRESULT; stdcall;
  end;
{$ENDIF JWA_BUILTIN_IFILEISINUSE}

function GetFileInUseInfo(const FileName : WideString) : IFileIsInUse;
var
  ROT : IRunningObjectTable;
  mFile, enumIndex, Prefix : IMoniker;
  enumMoniker : IEnumMoniker;
  MonikerType : LongInt;
  unkInt  : IInterface;
  ctx : IBindCtx;
  sEnumIndex, sFile : PWideChar;
begin
  result := nil;
  OleCheck(CreateBindCtx(0, ctx));

  //
  OleCheck(GetRunningObjectTable(0, ROT));
  OleCheck(CreateFileMoniker(PWideChar(FileName), mFile));

  OleCheck(ROT.EnumRunning(enumMoniker));

  while (enumMoniker.Next(1, enumIndex, nil) = S_OK) do
  begin
    OleCheck(enumIndex.IsSystemMoniker(MonikerType));
    if MonikerType = MKSYS_FILEMONIKER then
    begin
      OleCheck((EnumIndex as IMoniker).GetDisplayName(ctx, nil, sEnumIndex));
      OleCheck(mFile.GetDisplayName(ctx, nil, sFile));

      if Succeeded(mFile.CommonPrefixWith(enumIndex, Prefix)) and
         (mFile.IsEqual(Prefix) = S_OK) then
      begin
        if Succeeded(ROT.GetObject(enumIndex, unkInt)) then
        begin
          if Succeeded(unkInt.QueryInterface(IID_IFileIsInUse, result)) then
          begin
            result := unkInt as IFileIsInUse;
            exit;
          end;
        end;
      end;
    end;
  end;
end;

const
  TFileUsageTypeStr : array[TFileUsageType] of String = (
    'FUT_PLAYING (0)',
    'FUT_EDITING (1)',
    'FUT_GENERIC (2)');

  CapStr : array[1..3] of String = (
    'OF_CAP_CANSWITCHTO ($0001)',
    'OF_CAP_CANCLOSE ($0002)',
    'OF_CAP_CANSWITCHTO ($0001) or OF_CAP_CANCLOSE ($0002)'
  );


var
  FileInUse : IFileIsInUse;
  pAppName : PWidechar;
  Usage : TFileUsageType;
  Caps : Cardinal;
  WindowHandle : HWND;
  Msg, S : String;
  Buttons : Integer;
begin
  CoInitialize(nil);

  if not FileExists(ParamStr(1)) then
  begin
    MessageBox(0, 'Missing filename as command line parameter', '', MB_ICONERROR or MB_OK);
    exit;
  end;

  FileInUse := GetFileInUseInfo(ParamStr(1));

  if Assigned(FileInUse) then
  begin
    OleCheck(FileInUse.GetAppName(pAppName));
    OleCheck(FileInUse.GetUsage(Usage));
    OleCheck(FileInUse.GetCapabilities(Caps));
    OleCheck(FileInUse.GetSwitchToHWND(WindowHandle));

    Buttons := MB_OK;

    if (Caps and OF_CAP_CANSWITCHTO = OF_CAP_CANSWITCHTO) then
    begin
      Msg := 'YES = Switch to Window? NO = Send close file; Cancel= Do nothing';
      Buttons := MB_YESNOCANCEL;
    end;


    S := Format('AppName: %s'#13#10'Usage: %s'#13#10'Caps: %s'#13#10'Hwnd: %d'#13#10+Msg,
      [WideString(pAppName), TFileUsageTypeStr[Usage], CapStr[Caps], WindowHandle]);

    case MessageBox(0, PChar(S), '', MB_ICONINFORMATION or Buttons) of
      IDYES:
      begin
        SetForegroundWindow(WindowHandle);
        Sleep(2000); //allows the window to be displayed in front; otherwise IDE will be shown
      end;
      IDNO:
      begin
        OleCheck(FileInUse.CloseFile);
      end;
    end;

    CoTaskMemFree(pAppName);
  end;
end.
