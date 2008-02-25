program AskCredentials;

//{$APPTYPE CONSOLE}

uses
  SysUtils,
  JwaWindows,
  JwsclDescriptor,
  JwsclDesktops,
  JwsclCredentials,
  JwsclTypes,
  Graphics,
  Registry;

function BitBltBlack(DestDC: HDC; DestX, DestY: Integer; nWidth, nHeight: integer; SrcDC: HDC; SrcX, SrcY: integer; Black: byte): LongBool;
var BlendStruct: BLENDFUNCTION;
begin
  BitBlt(DestDC, DestX, DestY, nWidth, nHeight, 0, 0, 0, BLACKNESS);
  BlendStruct.BlendFlags:=0;
  BlendStruct.BlendOp:=AC_SRC_OVER;
  BlendStruct.SourceConstantAlpha:=255-Black;
  BlendStruct.AlphaFormat:=0;
  Result:=AlphaBlend(DestDC, DestX, DestY, nWidth, nHeight, SrcDC, SrcX, SrcY, NWidth, nHeight, BlendStruct);
end;

var HookHandle: HHOOK;

function HookProcToDisableCombo(nCode: integer; wp: WParam; lp: LParam): LRESULT; stdcall;
var Buffer: array[0..255] of char;
begin
  if nCode>=0 then
  begin
    with PCWPRETSTRUCT(lp)^ do
      if Message=WM_CREATE then
      begin
        GetClassName(hwnd, @Buffer[0], 256);
        if Buffer='ComboBoxEx32' then
          SetWindowLong(hwnd, GWL_STYLE, WS_DISABLED or GetWindowLong(hwnd, GWL_STYLE));
      end;
  end;
  Result:=CallNextHookEx(HookHandle, nCode, wp, lp);
end;

const DesktopWallpaperKey='Software\XPElevation\Paths\';

function RegistryGetDeskBackgroundPath: string;
var Reg: TRegistry; Unresolved: String;
begin
  Reg:=TRegistry.Create(KEY_QUERY_VALUE);
  try
    Reg.RootKey:=HKEY_LOCAL_MACHINE;
    if Reg.OpenKey(DesktopWallpaperKey, false) then
    try
      Unresolved:=Reg.ReadString('TempDesktopPicture');
      SetLength(Result, MAX_PATH+1);
      ExpandEnvironmentStrings(@Unresolved[1], @Result[1], MAX_PATH+1);
      SetLength(Result, StrLen(PChar(Result)));
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function AskPassword(AppToStart: String; UserAndDomain: string; out Password: string): boolean;
var Prompt: TJwCredentialsPrompt; OldDesk: HDESK; OldWallpaperPath: PChar; DesktopWallpaperPath: string;
     Bmp: Graphics.TBitmap; DeskDC: HDC; Desktop: TJwSecurityDesktop; Descr: TJwSecurityDescriptor;
begin
  DesktopWallPaperPath:=RegistryGetDeskBackgroundPath;
  DeskDC:=GetDC(0);
  try
    Bmp:=Graphics.TBitmap.Create;
    try
      Bmp.Width :=GetDeviceCaps(DeskDC, HORZRES);
      Bmp.Height:=GetDeviceCaps(DeskDC, VERTRES);
      BitBltBlack(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, DeskDC, 0, 0, 50);
      Bmp.SaveToFile(PChar(DesktopWallpaperPath));
    finally
      Bmp.Free;
    end;
  finally
    ReleaseDC(0, DeskDC);
  end;
  GetMem(OldWallpaperPath, MAX_PATH);
  try
    OldDesk:=GetThreadDesktop(GetCurrentThreadID);
    SystemParametersInfo(SPI_GETDESKWALLPAPER, MAX_PATH, OldWallpaperPath, 0);
    Descr:=TJwSecurityDescriptor.Create;
    try
      Desktop:=TJwSecurityDesktop.CreateDesktop(nil, true, 'UACinXPAskCredentialsDesk', [], false, GENERIC_ALL, Descr);
    finally
      Descr.Free;
    end;
    try
      Desktop.SetThreadDesktop;
      try
        Desktop.SwitchDesktop;
        try
          SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, PChar(DesktopWallpaperPath), 0);
          Prompt:=TJwCredentialsPrompt.Create;
          try
            Prompt.Caption:='UAC in XP';
            Prompt.MessageText:='The application '+AppToStart+' shall be run as the user '+UserAndDomain+' as an administrator. '+
                                'Enter your credentials to continue!';
            Prompt.UserName:=UserAndDomain;
            Prompt.Flags:=Prompt.Flags+[cfFlagsDoNotPersist];
            HookHandle:=SetWindowsHookEx(WH_CALLWNDPROCRET, HookProcToDisableCombo, 0, GetCurrentThreadId);
            Result:=Prompt.ShowModal;
            UnhookWindowsHookEx(HookHandle);
            Password:=Prompt.Password;
          finally
            Prompt.Free;
          end;
        finally
          Desktop.SwitchDesktopBack;
        end;
      finally
        SetThreadDesktop(OldDesk);
        SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, OldWallpaperPath, 0);
      end;
    finally
      Desktop.Free;
    end;
  finally
    FreeMem(OldWallpaperPath);
  end;
  DeleteFile(PChar(DesktopWallpaperPath));
end;

function IfThen(Cond: Boolean; TrueVal, FalseVal: String): String;
begin
  if Cond then
    result:=TrueVal
  else
    result:=FalseVal;
end;

function HasAccessRights(Handle: THandle; RequestedMask: Cardinal): boolean;
var Dup: THandle;
begin
  Result:=DuplicateHandle(GetCurrentProcess, Handle, GetCurrentProcess, @Dup, RequestedMask, false, 0);
  if Result then
    CloseHandle(Dup)
  else
    MessageBox(0, PChar(SysErrorMessage(GetLastError)), 'Ask Credentials', MB_OK);
end;

var Password: String; Pipe: THandle;

function func(p: pointer): LRESULT; stdcall;
const StrLenCancelled: Cardinal=Cardinal(-1);
      Null: Cardinal=0;
var Dummy: Cardinal;
begin
  try
    if Pipe=0 then
      exit;
    if not AskPassword(Paramstr(1), Paramstr(2), Password) then
      WriteFile(Pipe, @StrLenCancelled, SizeOf(StrLenCancelled), @Dummy, nil)
    else
    begin
      if Length(Password)>0 then
        WriteFile(Pipe, Pointer(integer(Password)-4), SizeOf(Cardinal)+Length(Password), @Dummy, nil)
      else
        WriteFile(Pipe, @Null, SizeOf(Null), @Dummy, nil);
    end;
  except
    on E: Exception do
      MessageBox(0, PChar(E.ClassType.ClassName+#13#10+E.Message), '', MB_OK);
  end;
end;

begin
  if (Paramstr(2)='') then
    Halt(Integer(@Pipe));
  WaitForSingleObject(CreateThread(nil, 0, @func, nil, 0, nil), Infinite);
end.
