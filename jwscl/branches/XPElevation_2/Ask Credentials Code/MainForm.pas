unit MainForm;

interface

uses
  jwaWindows,
  Messages, ActiveX, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, JvExControls, JvButton, JvTransparentButton, StdCtrls,
  SessionPipe, CredentialUtils, JwsclComUtils, JwsclDesktops,
  Menus, JvComponent, ImgList, Buttons,  JvExExtCtrls, 
  ComCtrls, JvExComCtrls, JvProgressBar;

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
    ImageListButtons: TImageList;
    ImageListMask: TImageList;
    ImageListAlpha: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure JvTransparentButton1Click(Sender: TObject);
    procedure Center1Click(Sender: TObject);
    procedure Screenzoom1Click(Sender: TObject);
    procedure Screenkeyboard1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure Beenden1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    fButtons : array of TJvTransparentButton;
    fJobs : array of TProcessInformation;
    fResolution : Trect;
    fScreenBitmap : TBitmap;
    fControlFlags : DWORD;
    fDesktop : TJwSecurityDesktop;
    fProcessList : TList;

    //resets the parent of all visible and foreign windows to this form
    procedure SetupAllForeignWindows;

    procedure CreateParams(var Params: TCreateParams); override;


  public
    { Public-Deklarationen }
    //Size of this form to be used
    property Resolution : Trect read fResolution write fResolution;

    //Background bitmap to be used. If set to nil, the background will be black
    property ScreenBitmap : TBitmap read fScreenBitmap write fScreenBitmap;

    property ControlFlags : DWORD read fControlFlags write fControlFlags;

    property Desktop : TJwSecurityDesktop write fDesktop;
  end;

var
  FormMain: TFormMain;



implementation

uses Types, CredentialsForm;

{$R *.dfm}

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


procedure TFormMain.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
{$IFNDEF FORMONLY}
  if GetDesktopWindow <> 0 then
  begin
    Params.WndParent := GetDesktopWindow;
    //sets the window as bottom most (like explorer's desktop)
    Params.Style := WS_CHILD or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
  end;
{$ENDIF FORMONLY}
end;

procedure TFormMain.FormClick(Sender: TObject);
begin
  JvTransparentButton1.Click;
end;



procedure TFormMain.FormCreate(Sender: TObject);
begin
//  BackWindow;
  Self.DoubleBuffered := true;
  fProcessList := TList.Create;
end;


procedure TFormMain.FormDestroy(Sender: TObject);
var
  i : Integer;
  Proc : THandle;
begin
  EnumWindows(@QuitEnumWindowsProc,0);
  Sleep(100);

  for I := fProcessList.Count - 1 downto 0 do
  begin
    Proc := THandle(fProcessList[i]);
    if (Proc <> 0) and (Proc <> INVALID_HANDLE_VALUE) then
    begin
      TerminateProcess(Proc,0);
      CloseHandle(Proc);
      Sleep(5);
    end;
    fProcessList.Delete(i);
  end;

  FreeAndNil(fProcessList);
end;

procedure TFormMain.FormShow(Sender: TObject);
  //centers a form in a given monitor
  procedure Center(F : TControl; I : Integer);
  var SX, SY : Integer;
  begin
    SX := abs(abs(Screen.Monitors[I].BoundsRect.Right) - abs(Screen.Monitors[I].BoundsRect.Left));
    F.Left := Screen.Monitors[I].Left + ((SX div 2) - (F.Width div 2));

    SY := abs(abs(Screen.Monitors[I].BoundsRect.Bottom) - abs(Screen.Monitors[I].BoundsRect.Top));
    F.Top := Screen.Monitors[I].Top + ((SY div 2) - (F.Height div 2));
  end;

  //get the left bottom corner of a monitor
  procedure LeftBottom(F : TControl; I : Integer);
  var SX, SY : Integer;
  begin
    SX := abs(abs(Screen.Monitors[I].BoundsRect.Right) - abs(Screen.Monitors[I].BoundsRect.Left));
    F.Left := Screen.Monitors[I].Left;// + (F.Width);

    SY := abs(abs(Screen.Monitors[I].BoundsRect.Bottom) - abs(Screen.Monitors[I].BoundsRect.Top));
    F.Top := Screen.Monitors[I].Top + SY - (F.Height);
  end;

var
  i : Integer;
  NewImage{, BackGround} : TBitmap;
  Image, Mask : array of TBitmap;

  ImageListAlphas : array of TImageList;

  R : TRect;
begin
  SetupAllForeignWindows;
  Visible := true;

  //set the size
  Image1.BoundsRect := Resolution;
  //set standard black screen bitmap if we don't have any
  if not Assigned(ScreenBitmap) then
  begin
    ScreenBitmap := TBitmap.Create;
    ScreenBitmap.Width := abs(abs(Resolution.Right) - abs(Resolution.Left));
    ScreenBitmap.Height := abs(abs(Resolution.Bottom) - abs(Resolution.Top));
    SetRect(R, 0,0, ScreenBitmap.Width, ScreenBitmap.Height);
    ScreenBitmap.Canvas.Brush.Color := clBlack;
    ScreenBitmap.Canvas.FillRect(R);
  end;
  TJwAutoPointer.Wrap(ScreenBitmap); //auto destroy after this function
  Image1.Picture.Bitmap.Assign(ScreenBitmap);


  Align := alNone;
  BoundsRect := Resolution;


  //reads 2 images and 2 masks from the imagelists
  SetLength(Image, 2);
  Image[0] := TBitmap.Create;
  ImageListButtons.GetBitmap(0, Image[0]);
  TJwAutoPointer.Wrap(Image[0]); //autodestroy


  Image[1] := TBitmap.Create;
  ImageListButtons.GetBitmap(1, Image[1]);
  TJwAutoPointer.Wrap(Image[1]);

  SetLength(Mask, 2);
  Mask[0] := TBitmap.Create;
  ImageListMask.GetBitmap(0, Mask[0]);
  TJwAutoPointer.Wrap(Mask[0]);


  Mask[1] := TBitmap.Create;
  ImageListMask.GetBitmap(1, Mask[1]);
  TJwAutoPointer.Wrap(Mask[1]);

  //get the given background image
 { BackGround := TBitmap.Create;
  BackGround.Assign(Image1.Picture.Bitmap);
  TJwAutoPointer.Wrap(BackGround);      }


  SetLength(fButtons, Screen.MonitorCount);
  SetLength(ImageListAlphas, Screen.MonitorCount);

  for i := low(fButtons) to high(fButtons) do
  begin
    ImageListAlphas[i] := TImageList.Create(self);
    ImageListAlphas[i].Height := Image[0].Height;
    ImageListAlphas[i].Width := Image[0].Width;
    ImageListAlphas[i].BkColor := clNone;

    fButtons[i] := TJvTransparentButton.Create(Self);
    fButtons[i].Width := Image[0].Width;
    fButtons[i].Height := Image[0].Height;
    Center(fButtons[i], i);

    NewImage := AlphaBlendImage(Image[0], Mask[0], ScreenBitmap, fButtons[i].Left, fButtons[i].Top);
    ImageListAlphas[i].Add(NewImage, nil);
    NewImage.Free;

    NewImage := AlphaBlendImage(Image[1], Mask[1], ScreenBitmap, fButtons[i].Left, fButtons[i].Top);
    ImageListAlphas[i].Add(NewImage, nil);
    NewImage.Free;

   fButtons[i].Images.ActiveImage := ImageListAlphas[i];
   fButtons[i].Images.ActiveIndex := 1;
   fButtons[i].Images.DownImage := ImageListAlphas[i];
   fButtons[i].Images.DownIndex := 0;

    fButtons[i].Parent := self;
    fButtons[i].HotTrack := true;
    fButtons[i].AutoGray := false;
    fButtons[i].FrameStyle := fsNone;
    fButtons[i].PressOffset := 0;

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

  //DimTimer.Enabled := true;
{$IFDEF TEST}
  Visible := false;
{$ENDIF}
  if Assigned(fDesktop) then
  begin
    try
      fDesktop.SwitchDesktop;
    except
      //log
      on E : Exception do
      begin
        try
          FormCredentials.BitBtnCancel.Click;
        except
        end;
      end;
    end;
  end;
end;




procedure TFormMain.JvTransparentButton1Click(Sender: TObject);
begin
{$IFDEF FORMONLY}
//  ShowMessage('OK');
  exit;
{$ENDIF FORMONLY}
  try
    FormCredentials.CenterInMonitor((Sender as TControl).Tag);
    FormCredentials.BringToFront;
  except
  end;
end;

procedure TFormMain.SetupAllForeignWindows;
begin
{$IFNDEF FORMONLY}
  EnumWindows(@EnumWindowsProc,Handle);
{$ENDIF FORMONLY}


{  SetWindowPos(Handle,HWND_NOTOPMOST,0,0,100,100,
    SWP_NOZORDER or
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);}
end;

procedure TFormMain.Beenden1Click(Sender: TObject);
begin
{$IFNDEF FORMONLY}
  try
    FormCredentials.BitBtnCancel.Click;
  except
  end;
{$ENDIF FORMONLY}
end;

procedure TFormMain.Center1Click(Sender: TObject);
var ProcInfo: PROCESS_INFORMATION;
    StartInfo : TStartupInfo;
begin

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

function StartProcess(const PathName : WideString) : THandle;
var ProcInfo: PROCESS_INFORMATION;
    StartInfo : TStartupInfoW;
begin
  ZeroMemory(@StartInfo, sizeof(StartInfo));
  StartInfo.cb := sizeof(StartInfo);
  StartInfo.lpDesktop := 'winsta0\SecureElevation';


  if not CreateProcessW(nil,PWideChar(PathName),nil,nil,true, CREATE_NEW_CONSOLE, nil, nil, StartInfo, ProcInfo) then
    ShowMessage(Format('The application could not be started: (%d) %s',[GetLastError,SysErrorMessage(GetLastError)]));

  CloseHandle(ProcInfo.hThread);
  result := ProcInfo.hProcess;
end;


function GetSystem32Path : WideString;
var Path : Array[0..MAX_PATH] of Widechar;
begin
  Result := '';
  if SUCCEEDED(SHGetFolderPathW(0,CSIDL_SYSTEM,0,SHGFP_TYPE_DEFAULT, @Path)) then
    result := IncludeTrailingBackslash(Path);  //Oopps, may convert unicode to ansicode
end;


procedure TFormMain.Screenzoom1Click(Sender: TObject);
begin
  //TODO:
  fProcessList.Add(Pointer(StartProcess(GetSystem32Path+'magnify.exe')));
end;

procedure TFormMain.Screenkeyboard1Click(Sender: TObject);
var Sh : TShellExecuteInfoW;
begin
  ShellExecuteW(0,'open',PWideChar(GetSystem32Path+'osk.exe'),nil,nil,SW_SHOW);
  //
  //fProcessList.Add(Pointer(StartProcess(GetSystem32Path+'osk.exe')));
//  ShellExecuteW(0,'open',PWideChar(GetSystem32Path+'osk.exe'),nil,nil,SW_SHOW);
//  exit;
//
//
//  ZeroMemory(@Sh, sizeof(Sh));
//  Sh.cbSize := sizeof(Sh);
//  Sh.hwnd := Application.MainForm.Handle;
//  Sh.lpVerb := 'open';
//  Sh.lpFile := PWideChar(GetSystem32Path+'osk.exe');
//  Sh.fMask := SEE_MASK_NOCLOSEPROCESS {or SEE_MASK_HMONITOR};
//  Sh.nShow := SW_SHOW;
//
//  if Assigned(Application.MainForm) then
//    Sh.u.hMonitor := Application.MainForm.Monitor.MonitorNum;
//
//  if ShellExecuteExW(Sh) then
//  begin
//    fProcessList.Add(Pointer(Sh.hProcess));
//  end
//  else
//    ShowMessage(SysErrorMessage(GetLastError));
end;

end.
