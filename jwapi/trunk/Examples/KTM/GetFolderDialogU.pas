//original code: http://www.scalabium.com/faq/dct0157.htm
{$I ..\..\Includes\jproject\jedi.inc}

unit GetFolderDialogU;

interface

uses SysUtils;

function GetFolderDialog (Handle: Integer; Caption: string;
                          var strFolder: TFileName) : Boolean;

implementation

uses Windows, ShlObj;

function BrowseCallbackProc (hwnd: HWND; uMsg: UINT; lParam: LPARAM;
                             lpData: LPARAM): Integer; stdcall;
begin
  if (uMsg = BFFM_INITIALIZED) then
    SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);

  BrowseCallbackProc := 0;
end;

function GetFolderDialog (Handle: Integer; Caption: string;
                          var strFolder: TFileName) : Boolean;
const
  BIF_STATUSTEXT           = $0004;
  BIF_NEWDIALOGSTYLE       = $0040;
  BIF_RETURNONLYFSDIRS     = $0080;
  BIF_SHAREABLE            = $0100;
  BIF_USENEWUI             = BIF_EDITBOX or BIF_NEWDIALOGSTYLE;

var
  BrowseInfo: TBrowseInfo;
  ItemIDList: PItemIDList;
  JtemIDList: PItemIDList;
  Path: PChar;
begin
  Result := False;
  Path := StrAlloc(MAX_PATH);
  SHGetSpecialFolderLocation(Handle, CSIDL_DRIVES, JtemIDList);

  FillChar (BrowseInfo, SizeOf (TBrowseInfo), #0);

  with BrowseInfo do
  begin
    hwndOwner := GetActiveWindow;
    pidlRoot := JtemIDList;
    SHGetSpecialFolderLocation(hwndOwner, CSIDL_DRIVES, JtemIDList);

    { return display name of item selected }
    pszDisplayName := StrAlloc(MAX_PATH);

    { set the title of dialog }
    lpszTitle := PChar(Caption);//'Select the folder';
    { flags that control the return stuff }
    lpfn := TFNBFFCallBack (@BrowseCallbackProc);
    { extra info that's passed back in callbacks }
    lParam := LongInt(PChar(strFolder));
  end;

  ItemIDList := SHBrowseForFolder(BrowseInfo);

  if (ItemIDList <> nil) then
    if SHGetPathFromIDList(ItemIDList, Path) then
    begin
      strFolder := Path;
      Result := True
    end;
end;

end.

