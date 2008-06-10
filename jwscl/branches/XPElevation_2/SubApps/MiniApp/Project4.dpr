{MiniApp displays a TaskBar button only - like UAC}

program Project4;

uses
  Classes,
  SysUtils,
  Graphics,

  JwsclUtils,
  JwsclComUtils,
  JwaWindows;
{$R *.res}

type
  TTempElevationWindowThread = class(TJwThread)
  protected
    fTitle : WideString;
    fIcon  : TIcon;
    fTempWnd : HWND;
  public
    procedure Execute; override;

    procedure Terminate; reintroduce;
    procedure TerminateAndWait(const Timeout : DWORD); 
  public
     property Title : WideString read fTitle write fTitle;
    property Icon  : TIcon read fIcon write fIcon;
    property TempWnd : HWND read fTempWnd;
  end;


function CreateTempElevationThread(const Title,
  IconPath : WideString) : TTempElevationWindowThread; overload; forward;
function CreateTempElevationThread(const Title, IconFile: WideString;
   Const IconIndex : Integer) : TTempElevationWindowThread; forward; overload;


function WndMessageProc(hWnd: HWND; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
const RETURNNAME = WideString('WndReturnValue');
var F : TFlashInfo;
  Value : Integer;
  hSysMenu : HMENU;
  i : Integer;
  MenuItemInfo : TMenuItemInfoW;
  Str : WideString;
begin
  result := 0;
  case Msg of
    WM_CREATE :
    begin
      SetPropW(hwnd,RETURNNAME,0);

      ZeroMemory(@F, sizeof(F));
      F.cbSize := sizeof(F);
      F.hwnd := hwnd;
      F.dwFlags := FLASHW_TIMER or FLASHW_TRAY;

      FlashWindowEx(F);

      //change system menu
      hSysMenu := GetSystemMenu(hWnd, false);

      //first disable all items that can't close the window
      for i := 0 to GetMenuItemCount(hSysMenu) - 1 do
      begin
        if GetMenuItemID(hSysMenu, i) <> SC_CLOSE then
        begin
          EnableMenuItem(hSysMenu, i, MF_BYPOSITION or MF_DISABLED or MF_GRAYED);
        end;
      end;

      //then we can remove all disabled items
      //In this way, the order of items don't matter. There can be even
      //several close buttons
      for i := GetMenuItemCount(hSysMenu) - 1 downto 0 do
      begin
        if (GetMenuState(hSysMenu, i, MF_BYPOSITION) and MF_DISABLED = MF_DISABLED) then
        begin
          DeleteMenu(hSysMenu, i, MF_BYPOSITION);
        end;
      end;
      {FillChar(MenuItemInfo,SizeOf(TMenuItemInfo), #0) ;
      MenuItemInfo.cbSize := SizeOf(TMenuItemInfo) ;
      MenuItemInfo.fMask := MIIM_ID or MIIM_STRING;
      MenuItemInfo.fType := MFT_STRING;
      MenuItemInfo.wId := 1000;
      Str := '';
      MenuItemInfo.dwTypeData := PWideChar(Str);
      MenuItemInfo.cch := Length(Str) ;

      InsertMenuItemW(hSysMenu, 0, true, MenuItemInfo);}
    end;
    WM_DESTROY :
      begin

        Value := GetPropW(hwnd,RETURNNAME);
        PostQuitMessage(Value);
        RemovePropW(hwnd,RETURNNAME);
      end;
    WM_ACTIVATE :
      begin
//        OutputDebugString(PChar('wparam: '+IntToStr(wPAram)));

        if wParam = WA_ACTIVE then
        begin
          SetPropW(hwnd,RETURNNAME, 0);
          DestroyWindow(Hwnd);
        end;
       { else
          ShowWindow(hwnd,SW_MINIMIZE);    }
        exit;
      end;
    WM_CLOSE :
      begin
        //someone else closed the window - we abort the process
        SetPropW(hwnd,RETURNNAME, 1);
        DestroyWindow(Hwnd);
        exit;
      end;
  else
    Result := DefWindowProc(hWnd,Msg,wParam,lParam);
  end;
end;



procedure TTempElevationWindowThread.Execute;
var
  wClass: TWndClassW;
  Msg: TMSG;
begin
  inherited;

  ReturnValue := 0;

  if Assigned(fIcon) then
    TJwAutoPointer.Wrap(fIcon);

  ZeroMemory(@wClass, sizeof(wClass));
  wClass.hInstance := hInstance;
  with wClass do
  begin
  {there are more wClass parameters which are
  not used here to  keep this simple}
    style :=        0;

    if Assigned(fIcon) then
      hIcon :=        fIcon.Handle;
    lpfnWndProc :=  @WndMessageProc;
    hbrBackground:= COLOR_BTNFACE+1;
    {COLOR_BTNFACE is not a brush, but sets it
    to a system brush of that Color}
    lpszClassName:= 'XP Elevation Temporary Window';
    {you may use any class name, but you may want to make it descriptive
    if you register more than one class}
    hCursor :=      LoadCursor(0,IDC_ARROW);

    cbClsExtra := 0;
    cbWndExtra := 0;
    lpszMenuName := '';
  end;

  RegisterClassW(wClass);


  fTempWnd := CreateWindowW(
    wClass.lpszClassName,
    PWideChar(Name),
    WS_SYSMENU,//WS_CAPTION,
    GetSystemMetrics(SM_CXSCREEN)+10,
    GetSystemMetrics(SM_CYSCREEN)+10,
    0,
    0,
    0,
    0,
    hInstance,	
    nil 
   );

  if fTempWnd = 0 then
  begin
    UnRegisterClassW(wClass.lpszClassName, hInstance);

    ReturnValue := -1;
    Exit;
  end;

  ShowWindow(fTempWnd, SW_MINIMIZE);
  UpdateWindow(fTempWnd);

  while GetMessageW(@Msg,0,0,0) do
  begin
    TranslateMessage(@Msg);

    DispatchMessageW(@Msg);
  end;

  //return WM_QUIT wparam value
  ReturnValue := Msg.wParam;
end;

function CreateTempElevationThread(const Title, IconFile: WideString;
   Const IconIndex : Integer) : TTempElevationWindowThread;
begin
  result := CreateTempElevationThread(Title,IconFile+','+SysUtils.IntToStr(IconIndex));
end;


function CreateTempElevationThread(const Title,
  IconPath : WideString) : TTempElevationWindowThread;
var
  Icon : TIcon;
  s,f : WideString;
  p,Idx : Integer;
  pI,pB : HICON;
begin
  Icon := nil;

  p := pos(',',IconPath);
  if p > 0 then
  begin
    s := Copy(IconPath,p+1,Length(IconPath));
    Idx := SysUtils.StrToIntDef(S,-1);

    f := Copy(IconPath,1, p-1);
    if Idx <> -1 then
    begin
      if ExtractIconExW(PWideChar(f),Idx, nil, @pI, 1) = 1 then
      begin
        Icon := TIcon.Create;
        Icon.Handle := pI;
      end;
    end;
  end;
  if not Assigned(Icon) then
  begin
    Icon := TIcon.Create;
    Icon.Handle := LoadIcon(0,IDI_EXCLAMATION);
  end;


  result := TTempElevationWindowThread.Create(true,Title);
  result.Title := Title;

  result.fIcon := Icon;

  result.Resume;
end;

procedure TTempElevationWindowThread.Terminate;
begin
  inherited Terminate;

  SendMessage(TempWnd,WM_CLOSE,0,0);
end;

procedure TTempElevationWindowThread.TerminateAndWait(const Timeout: DWORD);
begin
  Terminate;
  WaitWithTimeOut(Timeout);
end;

var T : TTempElevationWindowThread;


begin
//  T := CreateTempElevationThread('hjfghj','%SystemRoot%\system32\wmploc.dll,-730');
  T := CreateTempElevationThread('hjfghj','%SystemRoot%\system32\wmploc.dll',731);
 // IntToStr(T.WaitWithTimeOut(3000));

 // SendMessage(T.TempWnd,WM_CLOSE,0,0);
 // WaitForSingleObject(T.Handle, 3000);
 // T.TerminateAndWait(4000);
  if T.WaitFor = 1 then
    MessageBox(0,'Aborted','Info',MB_ICONINFORMATION or MB_OK)
  else
    MessageBox(0,'Elevated','Info',MB_ICONINFORMATION or MB_OK);
  T.Free;
end.
