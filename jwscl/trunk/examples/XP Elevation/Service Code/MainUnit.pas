unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
  JwaWindows, JwsclToken, JwsclLsa, JwsclCredentials, JwsclDescriptor, JwsclDesktops,
  JwsclExceptions, JwsclSID, JwsclKnownSID;

type
  TLogType=(ltInfo, ltError);

  TXPElevationService = class(TService)
    procedure ServiceExecute(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceAfterInstall(Sender: TService);
  private
    { Private declarations }
    fStopEvent:           THandle;
    fLogCriticalSection:  TCriticalSection;
    fLogFile:             Textfile;
    fStopped:             boolean;
//    fDesktop:             TJwSecurityDesktop;
    fAllowedSIDs:         TJwSecurityIdList;
    procedure InitAllowedSIDs;
    procedure SetStopped(const Value: boolean);
    function MayUserBeElevated(User: TJwSecurityID): boolean;
    function AskCredentials(Token: TJwSecurityToken; AppToStart: String; UserAndDomain: string; out Password: string): boolean;
  public
    fCompletionPortsCrit: TCriticalSection;
    fCompletionPorts:     array of THandle;
    function GetServiceController: TServiceController; override;
    procedure StartApp(AppToStart: String);
    procedure LogEvent(Event: String; EventType: TLogType=ltInfo);
    { Public declarations }
    property StopEvent: THandle read fStopEvent;
    property Stopped: boolean read fStopped write SetStopped;
  end;

const MessageboxCaption= 'XP Elevation';
      CredApplicationKey='CredentialsApplication';
      LogFileKey='Log';

      PARAMETER_LOG_DEBUG = 'debug'; //log by debug output message
      PARAMETER_LOG_EVENT = 'event'; //log by windows event logging

      SERVICE_DESCRIPTION : WideString = 'XP Elevation service provides the fundamental elevation '+
      'process for starting processes as an elevated user. If this service is stopped or terminated '+
      'privileged users can no more elevate themselves as administrators.';


var
  XPElevationService: TXPElevationService;


function HasParameter(const Str : String) : Boolean;

implementation
uses ThreadUnit, HandleRequestThread, Registry;
{$R *.DFM}

function HasParameter(const Str : String) : Boolean;
var i : Integer;
begin
  result := false;
  for i := 1 to ParamCount -1 do
  begin
    result := (CompareText(ParamStr(i),'/'+Str) = 0) or
       (CompareText(ParamStr(i),'-'+Str) = 0);
    if result then
      break;
  end;
end;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  XPElevationService.Controller(CtrlCode);
end;

function TXPElevationService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TXPElevationService.LogEvent(Event: String; EventType: TLogType=ltInfo);
var S : String;
    ID : Integer;
begin
  ID := 0;
  EnterCriticalSection(fLogCriticalSection);
  try
    case EventType of
      ltInfo: begin
                S := DateTimeToStr(Now)+' '+Event;
                ID := EVENTLOG_INFORMATION_TYPE;
              end;
      ltError: begin
                 S := DateTimeToStr(Now)+ ' ERROR: ' + Event;
                 ID := EVENTLOG_ERROR_TYPE;
               end;
    end;
    Writeln(fLogfile, S);
    Flush(fLogfile);

    if HasParameter(PARAMETER_LOG_DEBUG) then
      OutputDebugString(PChar(S));
    if HasParameter(PARAMETER_LOG_EVENT) then
      Self.LogMessage(S, ID);

  finally
    LeaveCriticalSection(fLogCriticalSection);
  end;
end;

//function BitBltBlack(DestDC: HDC; DestX, DestY: Integer; nWidth, nHeight: integer; SrcDC: HDC; SrcX, SrcY: integer; Black: byte): LongBool;
//var BlendStruct: BLENDFUNCTION;
//begin
//  BitBlt(DestDC, DestX, DestY, nWidth, nHeight, 0, 0, 0, BLACKNESS);
//  BlendStruct.BlendFlags:=0;
//  BlendStruct.BlendOp:=AC_SRC_OVER;
//  BlendStruct.SourceConstantAlpha:=255-Black;
//  BlendStruct.AlphaFormat:=0;
//  Result:=AlphaBlend(DestDC, DestX, DestY, nWidth, nHeight, SrcDC, SrcX, SrcY, NWidth, nHeight, BlendStruct);
//end;
//
//var HookHandle: HHOOK;
//
//function HookProcToDisableCombo(nCode: integer; wp: WParam; lp: LParam): LRESULT; stdcall;
//var Buffer: array[0..255] of char;
//begin
//  if nCode>=0 then
//  begin
//    with PCWPRETSTRUCT(lp)^ do
//      if Message=WM_CREATE then
//      begin
//        GetClassName(hwnd, @Buffer[0], 256);
//        if Buffer='ComboBoxEx32' then
//          SetWindowLong(hwnd, GWL_STYLE, WS_DISABLED or GetWindowLong(hwnd, GWL_STYLE));
//      end;
//  end;
//  Result:=CallNextHookEx(HookHandle, nCode, wp, lp);
//end;
//
//function TService1.AskCredentials(AppToStart: String; UserAndDomain: string; out Password: string): boolean;
//var Prompt: TJwCredentialsPrompt; OldDesk: HDESK; OldWallpaperPath: PChar;
//     Bmp: Graphics.TBitmap; DeskDC: HDC; Winsta: HWINSTA;
//begin
//  Winsta:=OpenWindowStation('WinSta0', false, GENERIC_ALL);
//  try
//    SetProcessWindowStation(Winsta);
//  finally
//    CloseWindowStation(Winsta);
//  end;
//  DeskDC:=GetDC(0);
//  try
//    Bmp:=Graphics.TBitmap.Create;
//    try
//      Bmp.Width :=GetDeviceCaps(DeskDC, HORZRES);
//      Bmp.Height:=GetDeviceCaps(DeskDC, VERTRES);
//      if not BitBltBlack(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, DeskDC, 0, 0, 50) then
//        LogEvent('Blitting error: '+SysErrorMessage(GetLastError));
//      Bmp.SaveToFile(PChar(DesktopWallpaperPath));
//    finally
//      Bmp.Free;
//    end;
//  finally
//    ReleaseDC(0, DeskDC);
//  end;
//  GetMem(OldWallpaperPath, MAX_PATH);
//  try
//    OldDesk:=GetThreadDesktop(GetCurrentThreadID);
//    SystemParametersInfo(SPI_GETDESKWALLPAPER, MAX_PATH, OldWallpaperPath, 0);
//    fDesktop.SetThreadDesktop;
//    try
//      fDesktop.SwitchDesktop;
//      try
//        SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, PChar(DesktopWallpaperPath), 0);
//        Prompt:=TJwCredentialsPrompt.Create;
//        try
//          Prompt.Caption:='UAC in XP';
//          Prompt.MessageText:='The application '+AppToStart+' shall be run as the user '+UserAndDomain+' as an administrator. '+
//                              'Enter your credentials to continue!';
//          Prompt.UserName:=UserAndDomain;
//          HookHandle:=SetWindowsHookEx(WH_CALLWNDPROCRET, HookProcToDisableCombo, 0, GetCurrentThreadId);
//          Result:=Prompt.ShowModal;
//          UnhookWindowsHookEx(HookHandle);
//          Password:=Prompt.Password;
//        finally
//         Prompt.Free;
//        end;
//      finally
//        fDesktop.SwitchDesktopBack;
//      end;
//    finally
//      SetThreadDesktop(OldDesk);
//      SystemParametersInfo(SPI_SETDESKWALLPAPER, 0, OldWallpaperPath, 0);
//    end;
//  finally
//    FreeMem(OldWallpaperPath);
//  end;
//  DeleteFile(PChar(DesktopWallpaperPath));
//end;

function RegGetFullPath(PathKey: string): string;
var Reg: TRegistry; Unresolved: string;
begin
  Reg:=TRegistry.Create(KEY_QUERY_VALUE);
  try
    Reg.RootKey:=HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('Software\XPElevation\Paths\', false) then
    try
      Unresolved:=Reg.ReadString(PathKey);
      SetLength(Result, MAX_PATH+1);
      ExpandEnvironmentStrings(PChar(Unresolved), @Result[1], MAX_PATH+1);
      SetLength(Result, StrLen(PChar(Result)));
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TXPElevationService.AskCredentials(Token: TJwSecurityToken; AppToStart: string; UserAndDomain: string; out Password: string): boolean;
var StartInfo: STARTUPINFOA;
  procedure InitStartInfo;
  begin
    ZeroMemory(@StartInfo, Sizeof(StartInfo));
    StartInfo.cb:=Sizeof(StartInfo);
    StartInfo.lpDesktop:='WinSta0\Default';
  end;
const StrLenCancelled: Cardinal=Cardinal(-1);
var ProcInfo: PROCESS_INFORMATION; Ar: Array[0..1] of THandle; Dummy: Cardinal;
    ReadPipe, WritePipe: THandle; Loc: Cardinal; Desc: TJwSecurityDescriptor; SecAttr: LPSECURITY_ATTRIBUTES;
    CredApp: String;
begin
  Result:=False;
  InitStartInfo;
  ZeroMemory(@ProcInfo, Sizeof(ProcInfo));
  Desc:=TJwSecurityDescriptor.Create;
  try
    SecAttr:=LPSECURITY_ATTRIBUTES(Desc.Create_SA(false));
    try
      CredApp:=RegGetFullPath(CredApplicationKey);
      //The application will immediately stop execution because it expects two parameters, but we need its exit code
      if not CreateProcessAsUser(Token.TokenHandle, PChar(CredApp), nil,
             SecAttr, SecAttr, True, CREATE_NEW_CONSOLE, nil, nil, StartInfo, ProcInfo) then
      begin
        LogEvent('Could not create the process asking for credentials '+SysErrorMessage(GetLastError), ltError);
        Abort;
      end
      else
      begin
        CloseHandle(ProcInfo.hThread);
        Ar[0]:=fStopEvent;
        Ar[1]:=ProcInfo.hProcess;
        //This function is called in a separate thread, so there is no need for ServiceThread.ProcessRequests!
//        while MsgWaitForMultipleObjects(2, @Ar[0], false, INFINITE, QS_ALLINPUT)=WAIT_OBJECT_0+2 do
//          ServiceThread.ProcessRequests(false);
        WaitForMultipleObjects(2, @Ar[0], false, INFINITE);
        if fStopped then
        begin
          //Terminate process???
          CloseHandle(ProcInfo.hProcess);
          Abort;
        end;
        //The application will return an address in its exit code
        GetExitCodeProcess(ProcInfo.hProcess, Loc);
        CloseHandle(ProcInfo.hProcess);

        if not CreateProcessAsUser(Token.TokenHandle, PChar(CredApp), PChar('"'+CredApp+'" "'+AppToStart+'" "'+UserAndDomain+'"'),
                 SecAttr, SecAttr, True, CREATE_NEW_CONSOLE or CREATE_SUSPENDED, nil, nil, StartInfo, ProcInfo) then
        begin
          LogEvent('Could not create the process asking for credentials '+SysErrorMessage(GetLastError), ltError);
          Abort;
        end
        else
        begin
          CreatePipe(ReadPipe, WritePipe, nil, 0);
          DuplicateHandle(GetCurrentProcess, WritePipe, ProcInfo.hProcess, @WritePipe, 0, false, DUPLICATE_SAME_ACCESS or DUPLICATE_CLOSE_SOURCE);
          //write the new handle to the location given in the exit code
          WriteProcessMemory(ProcInfo.hProcess, Pointer(Loc), @WritePipe, SizeOf(WritePipe), nil);
          ResumeThread(ProcInfo.hThread);
          CloseHandle(ProcInfo.hThread);
          Ar[0]:=fStopEvent;
          Ar[1]:=ProcInfo.hProcess;
          WaitForMultipleObjects(2, @Ar[0], false, INFINITE);
          if fStopped then
          begin
            //Terminate process???
            CloseHandle(ProcInfo.hProcess);
            Abort;
          end;
          CloseHandle(ProcInfo.hProcess);
          //we just reuse loc here
          ReadFile(ReadPipe, @Loc, SizeOf(Loc), @Dummy, nil);
          Result:=Loc<>StrLenCancelled;
          if Result then
          begin
            SetLength(Password, Loc);
            if Loc<>0 then
              ReadFile(ReadPipe, Pointer(Password), Loc, @Dummy, nil);
          end;
      end;
    end;
    finally
      Desc.Free_SA(PSECURITY_ATTRIBUTES(SecAttr));
    end;
  finally
    Desc.Free;
  end;
end;

function TXPElevationService.MayUserBeElevated(User: TJwSecurityID): boolean;
begin
  result:=fAllowedSIDs.FindSid(User)<>-1;
end;

procedure TXPElevationService.StartApp(AppToStart: string);
var Pass, User, Domain: string; LSA: TJwSecurityLsa;

    plogonData : PMSV1_0_INTERACTIVE_LOGON; Authlen: Cardinal;
    Source: TTokenSource; ProfBuffer: PMSV1_0_INTERACTIVE_PROFILE; ProfBufferLen: Cardinal;
    Token, NewToken: TJwSecurityToken; TokenLuid: TLUID; QuotaLimits: QUOTA_LIMITS; SubStatus: integer;

    ProfInfo: PROFILEINFO; AddGroups: TJwSecurityIDList; SID: TJwSecurityID;

    EnvirBlock: Pointer; StartInfo: STARTUPINFO; ProcInfo: PROCESS_INFORMATION;
    Job: THandle; Dummy: Cardinal;

    SeId1, SeId2, SeId3: Cardinal;
begin
  try
    Token:=TJwSecurityToken.CreateTokenEffective(TOKEN_ALL_ACCESS);
    try
      SID:=Token.TokenUser;
      try
        If not MayUserBeElevated(SID) then
        begin
          LogEvent('Elevation of user '+SID.AccountName['']+' for application '+AppToStart+' not allowed.');
//          //we do not want to wait for the user to abort this dialog
//          WTSSendMessage(WTS_CURRENT_SERVER_HANDLE, WTS_CURRENT_SESSION, 'XP Elevation', StrLen('XP Elevation'),
//                         PChar('The user '+SID.AccountName['']+' is not allowed to be elevated.'), Length('The user '+SID.AccountName['']+' is not alloqed to be elevated.'),
//                         MB_OK, 0, Dummy, false);
          MessageBox(0, PChar('The user '+SID.AccountName['']+' is not allowed to be elevated.'), MessageboxCaption, MB_SERVICE_NOTIFICATION or MB_OK);
          abort;
        end;
        User:=SID.AccountName[''];
        try
          Domain:=SID.GetAccountDomainName('');
        except
          Domain:='local';
        end;
      finally
        SID.Free;
      end;
      TJwSecurityToken.RevertToSelf;
      Token.ConvertToPrimaryToken(TOKEN_ALL_ACCESS);
      if not AskCredentials(Token, AppToStart, User+'@'+Domain, Pass) then
      begin
        LogEvent('Credentials prompt for '+AppToStart+' aborted by user');
        exit;
      end;

//      Token:=TJwSecurityToken.CreateWTSQueryUserToken;
//      try
//        with Token.TokenUser do
//        begin
//          MessageBox(0, PChar(AccountName['']), 'XP Elevation', MB_SERVICE_NOTIFICATION or MB_OK);
//          Free;
//        end;
//        if not AskCredentials(Token, AppToStart, User+'@'+Domain, Pass) then
//        begin
//          LogEvent('Credentials prompt for '+AppToStart+' aborted by user');
//          exit;
//        end;
//      finally
//        Token.Free;
//      end;
      try
        LSA:=TJwSecurityLsa.Create('StartApplicationsAsAdmin');
        try
//          MessageBox(0, 'Lsa created successfully', 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
          pLogonData:=JwCreate_MSV1_0_INTERACTIVE_LOGON(MsV1_0InteractiveLogon, Domain, User, Pass, Authlen);
          try
            Source.SourceName:='UACinXP';
            AllocateLocallyUniqueID(Source.SourceIdentifier);
            AddGroups:=TJwSecurityIDList.Create(True);
            try
//              SID:=Token.TokenUser;
//              SID.Attributes:=SE_GROUP_ENABLED or
//                              SE_GROUP_ENABLED_BY_DEFAULT or
//                              SE_GROUP_LOGON_ID;
// //             MessageBox(0, PChar(SID.AccountName['']), '', MB_SERVICE_NOTIFICATION or MB_OK);
//              AddGroups.Add(SID);
              SID:=JwGetLogonSID(Token);
              SID.Attributes:=SE_GROUP_MANDATORY or
                              SE_GROUP_ENABLED or
                              SE_GROUP_ENABLED_BY_DEFAULT or
                              SE_GROUP_LOGON_ID;
              AddGroups.Add(SID);
              SID:=TJwSecurityID.Create(JwAdministratorsSID);
              SID.Attributes:=SE_GROUP_MANDATORY or
                              SE_GROUP_ENABLED or
                              SE_GROUP_ENABLED_BY_DEFAULT or
                              SE_GROUP_LOGON_ID;
              AddGroups.Add(SID);
//              SID:=TJwSecurityID.Create(JwLocalGroupSID);
//              SID.Attributes:=SE_GROUP_MANDATORY or
//                              SE_GROUP_ENABLED or
//                              SE_GROUP_ENABLED_BY_DEFAULT;
//              AddGroups.Add(SID);
//              MessageBox(0, 'User is now to be logged on...', 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
              try
                Lsa.LsaLogonUser('UserAsAdmin', JwaWindows.Interactive, MSV1_0_PACKAGE_NAME,
                                    pLogonData, Authlen, AddGroups, Source, Pointer(ProfBuffer), ProfBufferLen,
                                    TokenLuid, NewToken, QuotaLimits, SubStatus);
              except
                on E: Exception do
                begin
                  LogEvent('Logon of User '+User+' failed');
                  MessageBox(0, 'Invalid logon data (e.g. wrong password)', MessageboxCaption, MB_SERVICE_NOTIFICATION or MB_OK);
                  abort;
                end;
              end;
            finally
              AddGroups.Free;
            end;
            try
//              MessageBox(0, PChar('User logged on successfully '+Inttostr(NewToken.TokenHandle)), 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
              ZeroMemory(@Profinfo, Sizeof(Profinfo));
              ProfInfo.dwSize:=Sizeof(Profinfo);
              ProfInfo.dwFlags:=1;
              //Profinfo.lpProfilePath:=PWideChar(Widestring(ProfBuffer.ProfilePath.Buffer));
              Profinfo.lpUserName:='UserAsAdmin';
              if not LoadUserProfile(NewToken.TokenHandle, Profinfo) then
              begin
                LogEvent('LoadUserProfile failed', ltError);
                NewToken.Free;
                Abort;
              end;
//              MessageBox(0, 'Process is to be created...', 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
              ZeroMemory(@Startinfo, sizeof(Startinfo));
              Startinfo.cb:=Sizeof(Startinfo);
//              Startinfo.dwFlags:=STARTF_USESHOWWINDOW;
//              Startinfo.wShowWindow:=SW_SHOW;
              Startinfo.lpDesktop:='Winsta0\default';
//              with Token.TokenUser do
//              try
//                MessageBox(0, PChar(AccountName['']), 'Token TokenUser', MB_SERVICE_NOTIFICATION or MB_OK);
//              finally
//                Free;
//              end;
//              with NewToken.TokenUser do
//              try
//                MessageBox(0, PChar(AccountName['']), 'NewToken TokenUser', MB_SERVICE_NOTIFICATION or MB_OK);
//              finally
//                Free;
//              end;
//              with Token.TokenGroups do
//              try
//                MessageBox(0, PChar(GetText(True)), 'Token TokenGroups', MB_SERVICE_NOTIFICATION or MB_OK);
//              finally
//                Free;
//              end;
//              with NewToken.TokenGroups do
//              try
//                MessageBox(0, PChar(GetText(True)), 'NewToken TokenGroups', MB_SERVICE_NOTIFICATION or MB_OK);
//              finally
//                Free;
//              end;
//              with Token.TokenOwner do
//              try
//                MessageBox(0, PChar(GetText(True)), 'Token TokenOwner', MB_SERVICE_NOTIFICATION or MB_OK);
//              finally
//                Free;
//              end;
//              with NewToken.TokenOwner do
//              try
//                MessageBox(0, PChar(GetText(True)), 'NewToken TokenOwner', MB_SERVICE_NOTIFICATION or MB_OK);
//              finally
//                Free;
//              end;
//              GetTokenInformation(NewToken.TokenHandle, TokenSessionId, @SeId1, 4, Dummy);
//              GetTokenInformation(   Token.TokenHandle, TokenSessionId, @SeId2, 4, Dummy);
//              with TJwSecurityToken.CreateTokenByProcess(GetCurrentProcess, TOKEN_READ or TOKEN_QUERY) do
//              try
//                JwaWindows.GetTokenInformation(TokenHandle, JwaWindows.TokenSessionId, @SeId3, 4, Dummy);
//                MessageBox(0, PChar(Inttostr(SeId1)+#13#10+Inttostr(SeId2)+#13#10+Inttostr(SeId3)), MessageboxCaption, MB_SERVICE_NOTIFICATION or MB_OK);
//              finally
//                Free;
//              end;
              if GetTokenInformation(   Token.TokenHandle, TokenSessionId, @SeId1, 4, Dummy) then
                SetTokenInformation(NewToken.TokenHandle, TokenSessionId, @SeId1, 4);
              CreateEnvironmentBlock(@Envirblock, NewToken.TokenHandle, false);
              try
                Job:=CreateJobObject(nil, nil);
                if not CreateProcessAsUser(NewToken.TokenHandle, PChar(AppToStart), nil, nil, nil,
                                           True, CREATE_NEW_CONSOLE or CREATE_SUSPENDED or CREATE_UNICODE_ENVIRONMENT or CREATE_BREAKAWAY_FROM_JOB, EnvirBlock, nil, Startinfo, ProcInfo)  then
                begin
                  LogEvent('Creation of process '+AppToStart+' failed'+SysErrorMessage(getLastError), ltError);
                  CloseHandle(Job);
                  UnloadUserProfile(NewToken.TokenHandle, Profinfo.hProfile);
                  NewToken.Free;
                  abort;
                end;
              finally
                DestroyEnvironmentBlock(Envirblock);
              end;
              if not AssignProcessToJobObject(Job, ProcInfo.hProcess) then
                LogEvent('Assignment to job failed '+SysErrorMessage(GetLastError), ltError);
              ResumeThread(ProcInfo.hThread);
              CloseHandle(Procinfo.hThread);
              LogEvent('Creation of process '+AppToStart+' as user '+User+' successful');
//              MessageBox(0, PChar('Process created successfully: '+inttostr(ProcInfo.hProcess)), 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
              TUnloadProfThread.Create(Profinfo.hProfile, NewToken, Job);
            finally
              LsaFreeReturnBuffer(ProfBuffer);
            end;
          finally
            LocalFree(Cardinal(pLogonData));
          end;
        finally
          LSA.Free;
        end;
      finally
        ZeroMemory(Pointer(Password), length(Password));
      end;
    finally
      Token.Free;
    end;
  except
    on E: EJwsclWinCallFailedException do
    begin
      LogEvent('Error in TService1.StartApp: '+E.ClassName+': '+E.Message+SysErrorMessage(E.LastError), ltError);
    end;
    on E: Exception do
      if not (E is EAbort) then
      begin
        LogEvent('Error in TService1.StartApp: '+E.ClassName+': '+E.Message, ltError);
      end;
  end;
end;

procedure TXPElevationService.InitAllowedSIDs;
var Reg: TRegistry; SIDStrings: TStringlist; i: integer;
begin
  fAllowedSIDs:=TJwSecurityIDList.Create(True);
  Reg:=TRegistry.Create(KEY_QUERY_VALUE);
  try
    Reg.RootKey:=HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('Software\XPElevation\AllowedUsers\', false) then
    try
      SIDStrings:=TStringlist.Create;
      try
        Reg.GetValueNames(SIDStrings);
        for i:=0 to SIDStrings.Count-1 do
        try
          fAllowedSIDs.Add(TJwSecurityId.Create(SIDStrings[i]));
        except
        end;
      finally
        SIDStrings.Free;
      end;
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TXPElevationService.ServiceExecute(Sender: TService);
var Pipe: THandle; OvLapped: OVERLAPPED; ar: array[0..1] of THandle;
    AppName: String; PipeSize: Cardinal; Descr: TJwSecurityDescriptor; SecAttr: PSECURITY_ATTRIBUTES;
    i: integer;
begin
  try
    AssignFile(fLogFile, RegGetFullPath(LogFileKey));
    if FileExists(RegGetFullPath(LogFileKey)) then
      Append(fLogFile)
    else
      Rewrite(fLogFile);
    InitializeCriticalSection(fLogCriticalSection);
  except
    on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), MessageboxCaption, MB_SERVICE_NOTIFICATION or MB_OK);
    end;
  end;
  try
//    MessageBox(0, 'Started', 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
    LogEvent('Started');
//    Descr:=TJwSecurityDescriptor.Create;
//    try
//      fDesktop:=TJwSecurityDesktop.CreateDesktop(nil, true, 'UACinXPAskCredentialsDesktop', [], false, GENERIC_ALL, Descr);
//    finally
//      Descr.Free;
//    end;
    InitializeCriticalSection(fCompletionPortsCrit);
      try
        SecAttr:=nil;
        ZeroMemory(@OvLapped, sizeof(OvLapped));
        OvLapped.hEvent:=CreateEvent(nil, false, false, nil);
        try
          Descr:=TJwSecurityDescriptor.Create;
          try
            Descr.DACL:=nil;
            SecAttr:=Descr.Create_SA(False);
            Pipe:=CreateNamedPipe('\\.\pipe\UAC in XP', PIPE_ACCESS_INBOUND or FILE_FLAG_OVERLAPPED, PIPE_WAIT, PIPE_UNLIMITED_INSTANCES, 0, 0, 0, LPSECURITY_ATTRIBUTES(SecAttr));
          finally
            Descr.Free;
          end;
          if Pipe=INVALID_HANDLE_VALUE then
          begin
//            MessageBox(0, PChar('Error occured during pipe creation: '+SysErrorMessage(GetLastError)), 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
            LogEvent('Error occured during pipe creation: '+SysErrorMessage(GetLastError), ltError);
            abort;
          end;
//        MessageBox(0, PChar('Pipe ('+inttostr(Pipe)+') created successfully'), 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
          try
            InitAllowedSIDs;
            try
              ar[0]:=fStopEvent;
              ar[1]:=OvLapped.hEvent;
              ConnectNamedPipe(Pipe, @OvLapped);
              while MsgWaitForMultipleObjects(2, @ar[0], false, INFINITE, QS_ALLINPUT)=WAIT_OBJECT_0+2 do
                ServiceThread.ProcessRequests(False);
              while not Stopped do
              begin
////              MessageBox(0, 'Pipe connected', 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
//                PeekNamedPipe(Pipe, nil, 0, nil, @PipeSize, nil);
//                while (PipeSize=0) and not Stopped do
//                begin
//                  Sleep(0);
//                  PeekNamedPipe(Pipe, nil, 0, nil, @PipeSize, nil);
//                  ServiceThread.ProcessRequests(false);
//                end;
//                if Stopped then
//                  break;
//                SetLength(AppName, PipeSize);
//                ReadFile(Pipe, @AppName[1], PipeSize, nil, @OvLapped);
//                WaitForSingleObject(OvLapped.hEvent, INFINITE);
////              MessageBox(0, PChar(AppName), 'App is to start', MB_SERVICE_NOTIFICATION or MB_OK);
//                TJwSecurityToken.ImpersonateNamedPipeClient(Pipe);
//                StartApp(Appname);
//              //  TJwSecurityToken.RevertToSelf; //now made in StartApp
//                DisconnectNamedPipe(Pipe);
                with THandleRequestThread.Create(True) do
                begin
                  FreeOnTerminate:=True;
                  PipeHandle:=Pipe;
                  Resume;
                end;
                Pipe:=CreateNamedPipe('\\.\pipe\UAC in XP', PIPE_ACCESS_INBOUND or FILE_FLAG_OVERLAPPED, PIPE_WAIT, PIPE_UNLIMITED_INSTANCES, 0, 0, 0, LPSECURITY_ATTRIBUTES(SecAttr));
                if Pipe=INVALID_HANDLE_VALUE then
                begin
//                  MessageBox(0, PChar('Error occured during pipe creation: '+SysErrorMessage(GetLastError)), 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
                  LogEvent('Error occured during pipe creation: '+SysErrorMessage(GetLastError), ltError);
                  abort;
                end;
                ConnectNamedPipe(Pipe, @OvLapped);
                while MsgWaitForMultipleObjects(2, @ar[0], false, INFINITE, QS_ALLINPUT and not QS_POSTMESSAGE or QS_ALLPOSTMESSAGE)=WAIT_OBJECT_0+2 do
                  ServiceThread.ProcessRequests(False);
              end;
            finally
              fAllowedSIDs.Free;
            end;
        finally
          CloseHandle(Pipe);
          EnterCriticalSection(fCompletionPortsCrit);
          try
            for i:=0 to high(fCompletionPorts) do
              PostQueuedCompletionStatus(fCompletionPorts[i], 0, MESSAGE_FROM_SERVICE, nil);
          finally
            LeaveCriticalSection(fCompletionPortsCrit);
          end;
          Sleep(1000);
        end;
      finally
        if Assigned(SecAttr) then
        begin
          TJwSecurityDescriptor.Free_SD(PSECURITY_DESCRIPTOR(SecAttr.lpSecurityDescriptor));
          FreeMem(SecAttr);
        end;
        CloseHandle(OvLapped.hEvent);
      end;
    finally
      DeleteCriticalSection(fCompletionPortsCrit);
//      fDesktop.Free;
//      MessageBox(0, 'Ending', 'UAC-Nachbildung', MB_SERVICE_NOTIFICATION or MB_OK);
      LogEvent('Ending');
    end;
  finally
    DeleteCriticalSection(fLogCriticalSection);
    CloseFile(fLogFile);
  end;
end;

procedure TXPElevationService.ServiceShutdown(Sender: TService);
begin
  CloseHandle(fStopEvent);
end;

procedure TXPElevationService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  fStopEvent:=CreateEvent(nil, true, false, nil);
end;

procedure TXPElevationService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  Self.Stopped:=True;
end;

procedure TXPElevationService.SetStopped(const Value: boolean);
begin
  FStopped := Value;
  if Value then
    SetEvent(fStopEvent);
end;

procedure TXPElevationService.ServiceAfterInstall(Sender: TService);
var SvcMgr, Svc : Cardinal;
    sd : TServiceDescriptionW;
begin
  //Set service description
  SvcMgr := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SvcMgr = 0 then
    exit;


  Svc := OpenService(SvcMgr, PChar(Name), SERVICE_CHANGE_CONFIG);
  if Svc = 0 then
    exit;

  FillChar(sd, sizeof(sd),0);
  sd.lpDescription := PWideChar(SERVICE_DESCRIPTION);

  ChangeServiceConfig2W(
        Svc,                 // handle to service
        SERVICE_CONFIG_DESCRIPTION, // change: description
        @sd);

  CloseServiceHandle(Svc);
  CloseServiceHandle(SvcMgr);
end;

end.
