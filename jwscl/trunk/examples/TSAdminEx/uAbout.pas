unit uAbout;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ShellApi;

type
  TFileVersionInfo = record
    CompanyName: string;
    FileDescription: string;
    FileVersion: string;
    InternalName: string;
    LegalCopyright: string;
    LegalTradeMarks: string;
    OriginalFilename: string;
    ProductName: string;
    ProductVersion: string;
    Comments: string;
  end;

type
  TAboutDialog = class(TForm)
    Panel1: TPanel;
    CodeGearLogo: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Website: TLabel;
    Copyright: TLabel;
    OKButton: TButton;
    UsedComponents: TLabel;
    WebsiteField: TLabel;
    VirtualTreeView: TLabel;
    Comments: TLabel;
    JediApiLib: TLabel;
    SecurityLibrary: TLabel;
    ProductNameField: TLabel;
    VersionField: TLabel;
    CopyrightField: TLabel;
    CommentsField: TLabel;
    AppIcon: TImage;
    procedure URLClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutDialog: TAboutDialog;

implementation

{$R *.dfm}

function GetVersion(const FileName: string): string;
var
  dwLen: DWORD;
  lpData: Pointer;
  FileInfo: PVSFixedFileInfo;
  puLen: DWORD;
  AHandle: DWORD;
begin
  dwLen := GetFileVersionInfoSize(PChar(FileName), AHandle);

  if dwLen > 0 then
  begin
    GetMem(lpData, dwLen);
    GetFileVersionInfo(PChar(FileName), 0, dwLen, lpData);

    if VerQueryValue(lpData, '\', Pointer(FileInfo), puLen) then
    begin

      with FileInfo^ do
      begin
        result := Format('%d.%d.%d.%d', [dwFileVersionMS shr 16,
          dwFileVersionMS and $FFFF, dwFileVersionLS shr 16, dwFileVersionLS
          and $FFFF]);
      end;

    end;

    FreeMem(lpData);
  end;
end;

function GetVersionInfo(const FileName: string; Info: string): string;
var
  dwLen: DWORD;
  lpData: Pointer;
  VersionInfo: Pointer;
  puLen: DWORD;
  AHandle: DWORD;
  VersionValue: string;
  Buffer: Pointer;
begin
  dwLen := GetFileVersionInfoSize(PChar(FileName), AHandle);

  if dwLen > 0 then
  begin
    GetMem(lpData, dwLen);
    GetFileVersionInfo(PChar(FileName), 0, dwLen, lpData);

    VerQueryValue(lpData, '\VarFileInfo\Translation', VersionInfo, puLen);

    VersionValue := '\\StringFileInfo\\' + IntToHex((PLongInt(VersionInfo)^ shl 16)
          or (PLongInt(VersionInfo)^ shr 16), 8) + '\\';

    VerQueryValue(lpData, PChar(VersionValue + 'Info'), Buffer, puLen);
    Result := PChar(Buffer);

    FreeMem(lpData);
  end;
end;


function GetFileInfo(const Filename: string;
  var FileVersionInfo: TFileVersionInfo): Boolean;
var VerInfoSize: DWORD;
 DummyVar: DWORD;
 VersionInfo: Pointer;
 Translation: Pointer;
 VersionValue: WideString;

function VerInfoQuery(VerInfo: Pointer; VerValue: WideString): string;
var VerInfoSize: DWORD;
  VerInfoPtr: Pointer;
begin
  Result := '';

  VerInfoSize := 0;
  if VerQueryValueW(VerInfo, PWideChar(VerValue), VerInfoPtr, VerInfoSize) then
  begin
    Result := Trim(PWideChar(VerInfoPtr));
  end;

  VerInfoPtr := nil; // Memory is freed when freeing VersionInfo Pointer
end;

begin
  Result := False;
  ZeroMemory(@FileVersionInfo, SizeOf(FileVersionInfo));

  VerInfoSize := GetFileVersionInfoSize(PChar(Filename), DummyVar);
  if VerInfoSize > 0 then begin

    GetMem(VersionInfo, VerInfoSize);

    try
      Result := GetFileVersionInfoW(PWideChar(WideString(Filename)), 0,
        VerInfoSize, VersionInfo);

      // Exit on failure
      if not Result then Exit;

      Result := VerQueryValueW(VersionInfo, '\VarFileInfo\Translation',
        Translation, VerInfoSize);

      // Exit on failure
      if not Result then Exit;

      VersionValue := '\StringFileInfo\' + IntToHex((PLongInt(Translation)^
        shl 16)or (PLongInt(Translation)^ shr 16), 8) + '\';

      FileVersionInfo.CompanyName := VerInfoQuery(VersionInfo,
        VersionValue + 'CompanyName');
      FileVersionInfo.FileDescription := VerInfoQuery(VersionInfo,
        VersionValue + 'FileDescription');
      FileVersionInfo.FileVersion := VerInfoQuery(VersionInfo,
        VersionValue + 'FileVersion');
      FileVersionInfo.InternalName := VerInfoQuery(VersionInfo,
        VersionValue + 'InternalName');
      FileVersionInfo.LegalCopyright := VerInfoQuery(VersionInfo,
        VersionValue + 'LegalCopyright');
      FileVersionInfo.LegalTradeMarks := VerInfoQuery(VersionInfo,
        VersionValue + 'LegalTrademarks');
      FileVersionInfo.OriginalFilename := VerInfoQuery(VersionInfo,
        VersionValue + 'OriginalFilename');
      FileVersionInfo.ProductName := VerInfoQuery(VersionInfo,
        VersionValue + 'ProductName');
      FileVersionInfo.ProductVersion := VerInfoQuery(VersionInfo,
        VersionValue + 'ProductVersion');
      FileVersionInfo.Comments := VerInfoQuery(VersionInfo,
        VersionValue + 'Comments');

    finally
      FreeMem(VersionInfo);
    end;
  end;
end;

procedure TAboutDialog.FormCreate(Sender: TObject);
var VerInfo: TFileVersionInfo;
begin
  AppIcon.Picture.Icon := Application.Icon;
  if GetFileInfo(Application.ExeName, VerInfo) then
  begin
    Caption := Format('About %s', [VerInfo.ProductName]);

    ProductNameField.Caption := VerInfo.ProductName;
    VersionField.Caption := VerInfo.FileVersion;
    WebSiteField.Caption := VerInfo.LegalTradeMarks;
    CopyRightField.Caption := VerInfo.LegalCopyright;
    CommentsField.Caption := VerInfo.Comments;
  end;
end;

procedure TAboutDialog.URLClick(Sender: TObject);
begin
  with Sender as TControl do
  begin
    ShellExecute(Application.Handle, 'Open', PChar(Hint), nil, nil, SW_NORMAL);
  end;
end;

end.

