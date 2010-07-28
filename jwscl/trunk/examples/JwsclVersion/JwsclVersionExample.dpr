program JwsclVersionExample;

{$APPTYPE CONSOLE}

uses
{$IFDEF FASTMM4}
  FastMM4,
{$ENDIF FASTMM4}
  JwaWindows,
  JwsclUtils,
  JwsclVersion,
  JwsclTypes,
  JwsclStrings,
  JwsclKnownSid,
  Dialogs,
  SysUtils;

var
  Path : TJwWideString;
  B : Boolean;
  F : TJwShellRestrictions;
  Pack : TJwServicePackVersion;
  OS : TOSVersionInfoEx;
begin
{$IFDEF FASTMM4}
  FullDebugModeScanMemoryPoolBeforeEveryOperation := true;
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF FASTMM4}

  JwInitWellKnownSIDs;
  
  Writeln('JWSCL OS constant: ', TJwWindowsVersion.GetWindowsType(OS));
  Writeln('OS.dwMajorVersion : ', OS.dwMajorVersion);
  Writeln('OS.dwMinorVersion : ', OS.dwMinorVersion);
  Writeln('OS.szCSDVersion : ', OS.szCSDVersion);

  Writeln;
  Writeln('TJwWindowsVersion.');

  for B := False to true do
  begin
    Writeln(' [Function]  (bOrHigher)');
    Writeln(' IsWindows2000  (',B,'): ',   TJwWindowsVersion.IsWindows2000(B));
    Writeln(' IsWindows2003  (',B,'): ',   TJwWindowsVersion.IsWindows2003(B));
    Writeln(' IsWindows2003R2(',B,'): ', TJwWindowsVersion.IsWindows2003R2(B));
    Writeln(' IsWindowsXP    (',B,'): ',     TJwWindowsVersion.IsWindowsXP(B));
    Writeln(' IsWindowsVista (',B,'): ',  TJwWindowsVersion.IsWindowsVista(B));
    Writeln(' IsWindows2008  (',B,'): ',   TJwWindowsVersion.IsWindows2008(B));
    Writeln(' IsWindows2008R2(',B,'): ', TJwWindowsVersion.IsWindows2008R2(B));
    Writeln(' IsWindows7     (',B,'): ',      TJwWindowsVersion.IsWindows7(B));
  end;

  Writeln;
  Writeln(' IsServer: ',              TJwWindowsVersion.IsServer);
  Writeln(' IsWindowsX64: ',          TJwWindowsVersion.IsWindowsX64);
  Writeln(' IsWindowsIA64: ',         TJwWindowsVersion.IsWindowsIA64);
  Writeln(' IsWindows64: ',           TJwWindowsVersion.IsWindows64);
  Pack := TJwWindowsVersion.GetServicePackVersion;
  Writeln(' GetServicePackVersion: ', Pack.Name);
  Finalize(Pack); //to avoid leak detection by FastMM
  Writeln(' IsStarterEdition: ',      TJwWindowsVersion.IsStarterEdition);

  Writeln;
  Writeln('TJwSystemInformation.');
  Writeln(' IsTerminalServiceRunning: ',       TJwSystemInformation.IsTerminalServiceRunning());
  Writeln(' GetNativeProcessorArchitecture: ', Integer(TJwSystemInformation.GetNativeProcessorArchitecture()));
  Writeln(' IsRemoteSession: ',                TJwSystemInformation.IsRemoteSession());
  Writeln(' IsConsoleSession: ',               TJwSystemInformation.IsConsoleSession());
  Writeln(' IsShadowedSession: ',              TJwSystemInformation.IsShadowedSession());
  Writeln(' GetConsoleSessionId: ',            TJwSystemInformation.GetConsoleSessionId());
  Writeln(' GetSystemBootType: ',              Integer(TJwSystemInformation.GetSystemBootType()));
  Writeln(' IsShuttingDown: ',                 TJwSystemInformation.IsShuttingDown());
  Writeln(' GetSystemDEPPolicy: ',             Integer(TJwSystemInformation.GetSystemDEPPolicy()));
  Writeln(' GetProcessorFeatures: ',           Integer(TJwSystemInformation.GetProcessorFeatures()));
  Writeln(' IsProcess64: ',                    TJwSystemInformation.IsProcess64());
  Writeln(' IsWOWProcess64: ',                 TJwSystemInformation.IsWOWProcess64());
  Writeln(' GetNumberOfProcessors phys    : ',     TJwSystemInformation.GetNumberOfProcessors(pctPhysicalProcessors));
  Writeln(' GetNumberOfProcessors cores   : ',     TJwSystemInformation.GetNumberOfProcessors(pctCoreProcessors));
  Writeln(' GetNumberOfProcessors logical : ',      TJwSystemInformation.GetNumberOfProcessors(pctLogicalProcessors));
  Writeln(' GetProcessFileName: ',              TJwSystemInformation.GetProcessFileName());

  Writeln;
  Writeln('TJwShellInformation.');
  F := TJwShellInformation.GetShellRestrictions();
  Writeln(' GetShellRestrictions: ', Cardinal(Pointer(@F)^));
  Writeln(' IsUACEnabled: ', TJwShellInformation.IsUACEnabled());
  try
    Writeln(' IsUserAdmin: ', TJwShellInformation.IsUserAdmin());
  except
    on e : Exception do
      Writeln(E.Message);

  end;
  Writeln(' GetFolderPath(Windows folder): ', TJwShellInformation.GetFolderPath(0, CSIDL_WINDOWS, nil, sftCurrent));
  try
    Write(' GetKnownFolderPath(Windows folder): ');  
    TJwShellInformation.GetKnownFolderPath(FOLDERID_Windows, Path);
    Writeln(Path);
  except
    Writeln('(Unsupported by Windows)');
  end;



  Readln;
end.
 