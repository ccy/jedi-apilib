unit MainForm;

interface

uses
  jwaWindows, ShellApi,
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, JvExControls, JvButton, JvTransparentButton, StdCtrls,
  SessionPipe,
  Menus, JvComponent, ImgList;

type
  TFormMain = class(TForm)
    Image1: TImage;
    JvTransparentButton1: TJvTransparentButton;
    JvTransparentButton2: TJvTransparentButton;
    PopupMenuMain: TPopupMenu;
    Screenzoom1: TMenuItem;
    Screenkeyboard1: TMenuItem;
    N1: TMenuItem;
    Center1: TMenuItem;
    N2: TMenuItem;
    Beenden1: TMenuItem;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure JvTransparentButton1Click(Sender: TObject);
    procedure Center1Click(Sender: TObject);
    procedure Screenzoom1Click(Sender: TObject);
    procedure Screenkeyboard1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure Beenden1Click(Sender: TObject);
  private
    { Private-Deklarationen }
    fButtons : array of TJvTransparentButton;
    fJobs : array of TProcessInformation;

    procedure BackWindow;

    procedure CreateParams(var Params: TCreateParams); override;


  public
    { Public-Deklarationen }
  end;

var
  FormMain: TFormMain;



implementation

uses Types, CredentialsForm;

{$R *.dfm}

procedure TFormMain.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  if GetDesktopWindow <> 0 then
  begin
    Params.WndParent := GetDesktopWindow;
    //sets the window as bottom most (like explorer's desktop)
    Params.Style := WS_CHILD or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  end;
end;

procedure TFormMain.FormClick(Sender: TObject);
begin
  JvTransparentButton1.Click;
end;

procedure TFormMain.FormCreate(Sender: TObject);
  procedure Center(F : TControl; I : Integer);
  var SX, SY : Integer;
  begin
    SX := abs(abs(Screen.Monitors[I].BoundsRect.Right) - abs(Screen.Monitors[I].BoundsRect.Left));
    F.Left := Screen.Monitors[I].Left + ((SX div 2) - (F.Width div 2));

    SY := abs(abs(Screen.Monitors[I].BoundsRect.Bottom) - abs(Screen.Monitors[I].BoundsRect.Top));
    F.Top := Screen.Monitors[I].Top + ((SY div 2) - (F.Height div 2));
  end;

  procedure LeftBottom(F : TControl; I : Integer);
  var SX, SY : Integer;
  begin
    SX := abs(abs(Screen.Monitors[I].BoundsRect.Right) - abs(Screen.Monitors[I].BoundsRect.Left));
    F.Left := Screen.Monitors[I].Left;// + (F.Width);

    SY := abs(abs(Screen.Monitors[I].BoundsRect.Bottom) - abs(Screen.Monitors[I].BoundsRect.Top));
    F.Top := Screen.Monitors[I].Top + SY - (F.Height);
  end;

var i : Integer;
begin
  SetLength(fButtons, Screen.MonitorCount);
  for i := low(fButtons) to high(fButtons) do
  begin
    fButtons[i] := TJvTransparentButton.Create(Self);
    fButtons[i].Width := 128;
    fButtons[i].Height := 128;
    Center(fButtons[i], i);
    fButtons[i].Glyph.Assign(JvTransparentButton1.Glyph);
    fButtons[i].Parent := self;
    fButtons[i].HotTrack := true;
    fButtons[i].AutoGray := false;
    fButtons[i].FrameStyle := fsNone;;

    fButtons[i].Tag := i;
    fButtons[i].OnClick := JvTransparentButton1Click;
    fButtons[i].Visible := true;


    fButtons[i] := TJvTransparentButton.Create(Self);
    fButtons[i].Width := 128;
    fButtons[i].Height := 128;
    LeftBottom(fButtons[i], i);
    fButtons[i].Glyph.Assign(JvTransparentButton2.Glyph);
    fButtons[i].Parent := self;
    fButtons[i].HotTrack := true;
    fButtons[i].AutoGray := false;
    fButtons[i].FrameStyle := fsNone;;

    fButtons[i].PopupMenu := PopupMenuMain;
    fButtons[i].DropDownMenu := PopupMenuMain;
    fButtons[i].Tag := i;
   
    fButtons[i].Visible := true;
  end;

  PopupMenu := PopupMenuMain;

{$IFDEF TEST}
  Visible := false;
{$ENDIF}
end;


function EnumWindowsProc(hwnd : HWND; lParam : Pointer) : Boolean; stdcall;
var X : array[0..MAX_PATH] of char;
  P : DWORD;
begin
  GetWindowThreadProcessId(hwnd, @p);
  if p <> GetCurrentProcessId then
    SetParent(Hwnd,THandle(lparam))
end;


function QuitEnumWindowsProc(hwnd : HWND; lParam : Pointer) : Boolean; stdcall;
var X : array[0..MAX_PATH] of char;
  P : DWORD;
begin
  GetWindowThreadProcessId(hwnd, @p);
  if p <> GetCurrentProcessId then
    PostMessage(Hwnd, WM_QUIT,0,0);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  BackWindow;
end;

procedure TFormMain.JvTransparentButton1Click(Sender: TObject);
begin
  try
    FormCredentials.CenterInMonitor((Sender as TControl).Tag);
  except
  end;
end;

procedure TFormMain.BackWindow;
begin
  EnumWindows(@EnumWindowsProc,Handle);


{  SetWindowPos(Handle,HWND_NOTOPMOST,0,0,100,100,
    SWP_NOZORDER or
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);}
end;

procedure TFormMain.Beenden1Click(Sender: TObject);
begin
  try
    FormCredentials.BitBtnCancel.Click;
  except
  end;
end;

procedure TFormMain.Center1Click(Sender: TObject);
var ProcInfo: PROCESS_INFORMATION;
    StartInfo : TStartupInfo;
begin
  EnumWindows(@QuitEnumWindowsProc,0);

  exit; //doesnt work
  ZeroMemory(@StartInfo, sizeof(StartInfo));
  StartInfo.cb := sizeof(StartInfo);  StartInfo.lpDesktop := 'winsta0\SecureElevation';
  //StartInfo.dwFlags := STARTF_USESHOWWINDOW;
//  StartInfo.wShowWindow := SW_


  if not CreateProcess(nil,'C:\Windows\system32\control.exe /name Microsoft.EaseOfAccessCenter',nil,nil,true, CREATE_NEW_CONSOLE, nil, nil, StartInfo, ProcInfo) then
    ShowMessage(SysErrorMessage(GetLastError));

  //ShellExecute(0,'open','control.exe','/name Microsoft.EaseOfAccessCenter',nil,SW_SHOWNORMAL);
 //ShellExecute(0,'open','C:\Windows\Explorer.exe','/separate,/idlist,:49223:6916,::{26EE0668-A00A-44D7-9371-BEB064C98683}\7\::{D555645E-D4F8-4C29-A827-D93C859C4F2A}',nil,SW_SHOWNORMAL);

end;

procedure TFormMain.Screenzoom1Click(Sender: TObject);
var ProcInfo: PROCESS_INFORMATION;
    StartInfo : TStartupInfo;
begin
  ZeroMemory(@StartInfo, sizeof(StartInfo));
  StartInfo.cb := sizeof(StartInfo);
  StartInfo.lpDesktop := 'winsta0\SecureElevation';


  if not CreateProcess(nil,'C:\Windows\system32\magnify.exe',nil,nil,true, CREATE_NEW_CONSOLE, nil, nil, StartInfo, ProcInfo) then
    ShowMessage(SysErrorMessage(GetLastError));

  //
  //ShellExecute(0,'open','magnify.exe','',nil,SW_SHOWNORMAL);
end;

procedure TFormMain.Screenkeyboard1Click(Sender: TObject);
begin
  //
  ShellExecute(0,'open','osk.exe','',nil,SW_SHOWNORMAL);
end;

end.
