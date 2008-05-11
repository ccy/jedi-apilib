unit UMain;

interface
uses SysUtils,
  JwaWindows,
  JwsclDescriptor,
  JwsclDesktops,
  JwsclCredentials,
  JwsclTypes,
  Graphics,
  Registry,
  Controls,
  Forms,
  Dialogs,
  CredentialsForm,
  MainForm,
  SessionPipe;


function Main : Integer;

implementation


var HookHandle: HHOOK;

var ScreenBitmap : TBitmap;
    Resolution : TRect;
    PipeSession : TClientSessionPipe;
    SessionInfo : TSessionInfo;
    MaxRepetitionCount : Cardinal;

var Haltresult : Integer = 0;

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

function SetUpDesktop : TJwSecurityDesktop;
var Descr: TJwSecurityDescriptor;
begin
  try
    Application.Free;
  except
  end;

  //Descr := TJwSecurityDescriptor.CreateDefaultByToken;
  Descr := nil;
  try
    result := TJwSecurityDesktop.CreateDesktop(nil, true, 'SecureElevation', [],
       false, GENERIC_ALL,  Descr);
{$IFDEF SECURE_DESKTOP}
    result.SwitchDesktop;
    result.SetThreadDesktop;


{$ENDIF}
  finally
    Descr.Free;
    Application := TApplication.Create(nil);
    Application.Initialize;
  end;
end;

procedure EndDesktop(var Desktop : TJwSecurityDesktop);
begin
  if Assigned(Desktop) then
  begin
    //FreeAndNil(Application);
    try
      Application.Free;
      Application := TApplication.Create(nil);
      Application.Free;
    except
    end;

    try
{$IFDEF SECURE_DESKTOP}
      try
        try
          Desktop.SetLastThreadDesktop;
        except
        end;
        Desktop.SwitchDesktopBack;
      except
      end;
{$ENDIF SECURE_DESKTOP}
    finally
      FreeAndNil(Desktop);
      Application := TApplication.Create(nil);
    end;
  end;
end;

function AskPassword(var SessionInfo : TSessionInfo): Short;

var Prompt: TJwCredentialsPrompt; OldDesk: HDESK; OldWallpaperPath: PChar; DesktopWallpaperPath: string;
     Bmp: Graphics.TBitmap; DeskDC: HDC;
     Desktop: TJwSecurityDesktop;

const
  sFALSE = 0;
  sTRUE = 1;
  sDI = -1;
begin

{$IFNDEF TEST}
    {Application.CreateForm(TFormMain, FormMain);

    FormMain.Image1.Picture.Bitmap.Assign(ScreenBitmap);
    FormMain.Image1.Align  := alClient;
     }
    //FormMain.BoundsRect := Resolution;
    //FormMain.Show;
    //FormMain.Hide;
{$ENDIF TEST}

    Application.CreateForm(TFormCredentials, FormCredentials);
    FormCredentials.CenterInMonitor(0);

{$IFNDEF TEST}
    FormCredentials.AppName := SessionInfo.Application;
    FormCredentials.AppCmdLine := SessionInfo.Commandline;

    FormCredentials.UserName := SessionInfo.UserName;
//     FormCredentials.Password := SessionInfo.Password; //pass is not sent by server
    FormCredentials.Flags := SessionInfo.Flags;
    FormCredentials.TimeOut := SessionInfo.TimeOut;
{$ELSE}
    FormCredentials.AppName := 'C:\Windows\Explorer.exe';
    FormCredentials.AppCmdLine := '/separate,/e,/idlist,:49333:2321,::{20DFFFE0-3AFA-10A9-A348-0DF23442309D}';

    FormCredentials.UserName := 'Christian';
    FormCredentials.Password := '';
    FormCredentials.Flags := SERVER_CACHEAVAILABLE;
    FormCredentials.TimeOut := 60*1000;
{$ENDIF TEST}

    FormCredentials.Show;
    Application.Run;
    case FormCredentials.ModalResult of
      mrOk : result := sTRUE;
      mrCancel : result := sFALSE;
      mrAbort : result := sDI;
    end;
    if result = sTRUE then
    begin
      SessionInfo.UserName := FormCredentials.UserName;
      SessionInfo.Password := FormCredentials.Password;
      SessionInfo.Flags    := FormCredentials.Flags;
    end
    else
    if result = sDI then
    begin
      //service terminates only if also canceled
      SessionInfo.Flags    := CLIENT_CANCELED or CLIENT_DEBUGTERMINATE;
    end
    else
      SessionInfo.Flags    := CLIENT_CANCELED;
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

function GetScreenBitmap(out Resolution : TRect) : TBitmap;

  function BitBltBlack(DestDC: HDC; DestX, DestY : Integer; nWidth, nHeight: integer; SrcDC: HDC; SrcX, SrcY: integer; Black: byte): LongBool;
  var BlendStruct: BLENDFUNCTION;
  begin
    BitBlt(DestDC, DestX, DestY, nWidth, nHeight, 0, SrcX, SrcY, BLACKNESS);
    BlendStruct.BlendFlags:=0;
    BlendStruct.BlendOp:=AC_SRC_OVER;
    BlendStruct.SourceConstantAlpha:=255-Black;
    BlendStruct.AlphaFormat:=0;
    Result:= jwaWindows.AlphaBlend(DestDC, DestX, DestY, nWidth, nHeight, SrcDC, SrcX, SrcY, NWidth, nHeight, BlendStruct);
  end;

  function GetMaxResolution : TRect;
  var i : Integer;
  begin
    ZeroMemory(@result, sizeof(result));
    for i := 0 to Screen.MonitorCount -1 do
    begin
      if Screen.Monitors[i].BoundsRect.Left < result.Left then
        result.Left := Screen.Monitors[i].BoundsRect.Left;
      if Screen.Monitors[i].BoundsRect.Top < result.Top then
        result.Top := Screen.Monitors[i].BoundsRect.Top;

      if Screen.Monitors[i].BoundsRect.Right > result.Right then
        result.Right := Screen.Monitors[i].BoundsRect.Right;
      if Screen.Monitors[i].BoundsRect.Bottom > result.Bottom then
        result.Bottom := Screen.Monitors[i].BoundsRect.Bottom;
    end;
  end;

var i : Integer;
    R,R2 : TRect;
    DeskDC : HDC;
begin
  Resolution := GetMaxResolution;

  DeskDC := GetDC(0);
  try
    result := Graphics.TBitmap.Create;
    result.Width := abs(Resolution.Left) + abs(Resolution.Right);
    result.Height:= abs(Resolution.Top) + abs(Resolution.Bottom);
    BitBltBlack(result.Canvas.Handle, 0, 0, result.Width, result.Height, DeskDC, Resolution.Left, Resolution.Top, 200);
    //Bmp.SaveToFile(PChar(DesktopWallpaperPath));
   // result.SaveToFile('E:\Temp\_Bmp.bmp');
  finally
    ReleaseDC(0, DeskDC);
  end;

  Resolution.Right  := abs(Resolution.Left) + abs(Resolution.Right);
  Resolution.Bottom := abs(Resolution.Top) + abs(Resolution.Bottom);
end;

var Password: String; Pipe: THandle;


function AskCredential(): LRESULT; stdcall;

procedure CheckResult(Value, LastError : Integer);
var
  ErrorResults : array[1..2] of String;
  Win32Error,
  ErrorMessage : String;
  i : Integer;
begin
  ErrorResults[1] := 'Invalid data';
  ErrorResults[2] := 'Process creation failed';
  if (Value = ERROR_WIN32) and (LastError <> ERROR_SUCCESS) then
  begin
    SetLastError(LastError);
    MessageDlg(ErrorResults[i]+#13#10+SysErrorMessage(LastError),mtError,[mbok],0);
  end
  else
  if (Value <> ERROR_SUCCESS) and (Value <> ERROR_ABORTBYUSER) then
  begin
    case Value of
      ERROR_CREATEPROCESSASUSER_FAILED : ErrorMessage := ''+#13#10+SysErrorMessage(LastError);
      ERROR_INVALID_USER : ErrorMessage := 'ERROR_INVALID_USER';
      ERROR_ABORTBYUSER : ErrorMessage := 'ERROR_ABORTBYUSER';
      ERROR_LOGONUSERFAILED : ErrorMessage := 'ERROR_LOGONUSERFAILED';
      ERROR_LOADUSERPROFILE : ErrorMessage := 'ERROR_LOADUSERPROFILE';

      ERROR_TIMEOUT : ErrorMessage := 'ERROR_TIMEOUT';
      ERROR_SHUTDOWN : ErrorMessage := 'ERROR_SHUTDOWN';
      ERROR_WIN32 : ErrorMessage := 'ERROR_WIN32';
      ERROR_GENERAL_EXCEPTION : ErrorMessage := 'A general exception was raised by the service. ';
      ERROR_NO_SUCH_LOGONSESSION : ErrorMessage := 'ERROR_NO_SUCH_LOGONSESSION';
      ERROR_TOO_MANY_LOGON_ATTEMPTS : ErrorMessage := 'The logon was aborted because too many attempt were used.';
    else
      ErrorMessage := 'Unknown: '+IntToStr(Value);
    end;
    if LastError <> 0 then
      Win32Error := SysErrorMessage(LastError);
    MessageDlg(ErrorMessage+#13#10+Win32Error,mtError,[mbok],0);
  end;
end;


const StrLenCancelled: Cardinal=Cardinal(-1);
      Null: Cardinal=0;
var
  Dummy,
  i,
  LastError,
  Value : Cardinal;

  CurrentAttempt : Integer;
  SecureDesktop : TJwSecurityDesktop;
begin
 // MessageBox(0, 'DEBUG','DEBUG',MB_ICONWARNING or MB_OK);
  result := 0;
  ScreenBitmap := GetScreenBitmap(Resolution);
  try
    SessionInfo.Password := GetFillPasswd;
    SessionInfo.Password := '';

    SecureDesktop := SetUpDesktop;
    try
      CurrentAttempt := 0;
      repeat
        AskPassword(SessionInfo);
        Haltresult := 2;
        PipeSession.SendClientData(SessionInfo);

        PipeSession.ReadServerProcessResult(Value, LastError, 0);

        case Value of
          ERROR_SUCCESS : break;
          ERROR_LOGONUSERFAILED : CurrentAttempt := LastError;
        else
          begin
            EndDesktop(SecureDesktop);
            CheckResult(Value, LastError);
            break;
          end;
        end;
      until CurrentAttempt > MaxRepetitionCount;
    finally
      EndDesktop(SecureDesktop);
    end;

    PipeSession.ReadServerProcessResult(Value, LastError, 0);
    CheckResult(Value, LastError);

  finally
    ScreenBitmap.Free;
  end;

end;


function Main : Integer;
var p : String;
    i : Integer;
    SecureDesktop : TJwSecurityDesktop;
begin
 { if ParamStr(1) <> '/cred' then
    halt(10);

  if ParamStr(2) <> then
     }

  {p := '';
  for i := 0 to ParamCount do
  begin
    p := p + #13#10 + IntToStr(i) + ': '+ParamStr(i);
  end;
  MessageBoxW(0, PWideChar(WideString(p)),'Information',MB_OK);
  }

  SecureDesktop := SetUpDesktop;
  try
    MessageBox(0,'123','123',MB_OK);
  finally
    EndDesktop(SecureDesktop);
  end;
  exit;


  PipeSession := TClientSessionPipe.Create;
  try
{$IFNDEF TEST}
    Haltresult := 1;
    PipeSession.Connect(ParamStr(2));
    Haltresult := 2;
    PipeSession.ReadServerData(SessionInfo);

    MaxRepetitionCount := SessionInfo.MaxLogonAttempts;

    Haltresult := 2;
{$ELSE}
    ZeroMemory(@SessionInfo, sizeof(SessionInfo));
    SessionInfo.Application := 'C:\Windows\System32\cmd.exe';
{$ENDIF TEST}
    //FreeAndNil(Application);
    Application := Nil;

    Haltresult := AskCredential();
  except
 {   on E : EOSError do
      HaltResult := E.ErrorCode; }
    on E : Exception do
    begin
      MessageBoxW(0, PWideChar(WideString(E.Message)),'Error',MB_OK);
    end;
  end;
  PipeSession.Free;
  result := HaltResult;
end;

end.
