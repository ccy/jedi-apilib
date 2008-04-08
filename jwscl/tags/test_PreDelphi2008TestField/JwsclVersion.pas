{@abstract(Contains types that are used by the units of JWSCL)
@author(Christian Wimmer)
@created(03/23/2007)
@lastmod(09/10/2007)



Project JEDI Windows Security Code Library (JWSCL)

The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the  
GNU Lesser General Public License (the  "LGPL License"), in which case the   
provisions of the LGPL License are applicable instead of those above.        
If you wish to allow use of your version of this file only under the terms   
of the LGPL License and not to allow others to use your version of this file 
under the MPL, indicate your decision by deleting  the provisions above and  
replace  them with the notice and other provisions required by the LGPL      
License.  If you do not delete the provisions above, a recipient may use     
your version of this file under either the MPL or the LGPL License.          
                                                                             
For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html 

The Original Code is JwsclVersion.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclVersion;
{$I Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $

interface

uses SysUtils, Contnrs, Classes,
  JwsclUtils, JwsclResource,
  jwaWindows, JwsclConstants, JwsclExceptions,
  JwsclStrings;

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type
  TFileVersionInfo = record
    CompanyName: TJwString;
    FileDescription: TJwString;
    FileVersion: TJwString;
    InternalName: TJwString;
    LegalCopyright: TJwString;
    LegalTradeMarks: TJwString;
    OriginalFilename: TJwString;
    ProductName: TJwString;
    ProductVersion: TJwString;
    Comments: TJwString;
  end;

  TJwFileVersion = class(TObject)
  private
  protected
  public
    {@Name retrieves a TFileVersionInfo structure for a given Filename.@br
    }
    class function GetFileInfo(const Filename: TJwString;
      var FileVersionInfo: TFileVersionInfo): Boolean;
  end;


  TJwServerInfo = class(TObject)
  private
  protected
    {@exclude}
    FServer: TJwString;
    {@exclude}
    FMajorVersion: Integer;
    {@exclude}
    FMinorVersion: Integer;
    {@exclude}
    FIsServer: Boolean;
    {@exclude}
    FIsTerminalServer: Boolean;
    {@exclude}
    FWindowsType: Integer;
  public
    {The @Name constructor creates the @ClassName object and reserves memory
     for it.@br
     @param(Server the servername for which you would like to retreive
     information. If Servername is empty string the local machine will be
     queried.)@br
     @br
     Example:
     @longcode(#
     var
       ServerInfo: TJwServerInfo;

     begin
       // Create TJwServerInfo object and reserve memory for it
       ServerInfo := TJwServerInfo.Create('REMOTESERVER');

       // Are we running on a Server?
       if ServerInfo.IsServer then
       begin
         Memo1.Lines.Add(Format('Server %s runs a Server OS', [ServerInfo.Server]));
       end;

       // Vista or higher code
       if ServerInfo.IsWindowsVista(True) then
       begin
         // Do some Vista specific stuff
       end;

       // Free Memory!
       ServerInfo.Free;
     end;
     #)
    }
    constructor Create(const Server: TJwString);

    {@Name returns a constant that defines the windows version the process is running.
     @return(The return value can be one of these constants defined in JwsclConstants
     Actually these items are supported
     @unorderedList(
       @itemSpacing(Compact)
       @item cOsUnknown = The system is unknown
       @item cOsWinNT   = running on Windows NT
       @item cOsWin2000 = running on Windows 2000
       @item cOsXP      = running on Windows XP
       @item cOS2003    = running on Windows 2003 or Windows 2003 Release 2
       @item cOSXP64    = running on Windows XP 64 Edition (not supported at the moment)
       @item cOsVista   = running on Windows Vista
       @item cOsWin2008 = running on Windows 2008 (tested on rc)
     ))
    }
    function GetWindowsType: Integer;

    {@Name checks if the system is a server version. @br
     @Name returns @true if the system is a Server; otherwise @false (Workstation).
    }

    property IsServer: Boolean read FIsServer;

    {@Name checks if the system is a Terminal Server. A server is considered to
     be a Terminal Server if it meets one of the following conditions:@br
     @unorderedList(
     @Item(The Terminal Server is in application mode)
     @Item(The Terminal Server is advertising itsself on the network)
     )
     @br
     @Name returns @true if the system is a Terminal Server in application mode
     ; otherwise @false.
     @seealso(TJwTerminalServer.EnumerateServers)
    }
    property IsTerminalServer: Boolean read FIsTerminalServer;

    {@Name checks if the system has the version given in the function name.
     @param(bOrHigher defines if the return value should also be @true if the system
     is better/higher than the requested system version.)
     @returns(@Name returns @true if the system is the requested version (or higher if bOrHigher is true);
     otherwise @false.
     If bOrHigher is @true the return value is the result of
     @TRUE if (bOrHigher and (GetWindowsType > iVer)) is true;
     @FALSE if GetWindowsType < (requested version)
     )
    }
    function IsWindows2000(bOrHigher: boolean = False): boolean;

    {@Name checks if the system has the version given in the function name.
     @param(bOrHigher defines if the return value should also be @true if the system
     is better/higher than the requested system version.)
     @return(@Name returns @true if the system is the requested version (or higher if bOrHigher is true);
     otherwise @false.
     If bOrHigher is @true the return value is the result of
     @TRUE if (bOrHigher and (GetWindowsType > iVer)) is true;
     @FALSE if GetWindowsType < (requested version)
     )
    }
    function IsWindows2003(bOrHigher: boolean = False): boolean;

    {@Name checks if the system has the version given in the function name.
     @param(bOrHigher defines if the return value should also be @true if the system
     is better/higher than the requested system version.)
     @return(@Name returns @true if the system is the requested version (or higher if bOrHigher is true);
     otherwise @false.
     If bOrHigher is @true the return value is the result of
     @TRUE if (bOrHigher and (GetWindowsType > iVer)) is true;
     @FALSE if GetWindowsType < (requested version)
     )
    }
    function IsWindowsXP(bOrHigher: boolean = False): boolean;

    {@Name checks if the system has the version given in the function name.
     @param(bOrHigher defines if the return value should also be @true if the system
     is better/higher than the requested system version.)
     @return(@Name returns @true if the system is the requested version (or higher if bOrHigher is true);
     otherwise @false.
     If bOrHigher is @true the return value is the result of
     @TRUE if (bOrHigher and (GetWindowsType > iVer)) is true;
     @FALSE if GetWindowsType < (requested version)
     )
    }
    function IsWindows2008(bOrHigher: boolean = False): boolean;


    {@Name checks if the system has the version given in the function name.
     @param(bOrHigher defines if the return value should also be @true if the system
     is better/higher than the requested system version.)
     @return(@Name returns @true if the system is the requested version (or higher if bOrHigher is true);
     otherwise @false.
     If bOrHigher is @true the return value is the result of
     @TRUE if (bOrHigher and (GetWindowsType > iVer)) is true;
     @FALSE if GetWindowsType < (requested version)
     )
    }
    function IsWindowsVista(bOrHigher: boolean = False): boolean;

    {@Name is the servername as returned by Windows Api}
    property Server: TJwString read FServer;
  end;


    {@Name provides methods to detect the windows version and product type.
     All methods are class methods so there is no need for an instance of @Name.
     }
  TJwWindowsVersion = class(TObject)
  private
  protected
  public
      {@Name returns a constant that defines the windows version the process is running.
       @return(The return value can be one of these constants defined in JwsclConstants
        Actually these items are supported
        @unorderedList(
         @item cOsUnknown = The system is unknown
         @item cOsWin95   = running on Windows 95
         @item cOsWin98   = running on Windows 98
         @item cOsWin98SE = running on Windows 98 Second Edition
         @item cOsWinME   = running on Windows ME
         @item cOsWinNT   = running on Windows NT
         @item cOsWin2000 = running on Windows 2000
         @item cOsXP      = running on Windows XP
         @item cOS2003    = running on Windows 2003
         @item cOS2003R2  = running on Windows 2003 Release 2
         @item cOSXP64    = running on Windows XP 64 Edition (not supported at the moment)
         @item cOsVista   = running on Windows Vista
         @item cOsWin2008 = running on Windows 2008 (tested on rc)
         ))
       }

    class function GetWindowsType: integer; virtual;

      {@Name checks if the system has the version given in the function name.
       @param(bOrHigher defines if the return value should also be @true if the system
              is better/higher than the requested system version.)
       @return(@Name returns @true if the system is the requested version (or higher if bOrHigher is true);
               otherwise @false.
               If bOrHigher is @true the return value is the result of
                @TRUE if (bOrHigher and (GetWindowsType > iVer)) is true;
                @FALSE if GetWindowsType < (requested version)
               )
       }
    class function IsWindows95(bOrHigher: boolean = False): boolean;
      virtual;
      {@Name checks if the system has the version given in the function name.
       @param(bOrHigher defines if the return value should also be @true if the system
              is better/higher than the requested system version.)
       @return(@Name returns @true if the system is the requested version (or higher if bOrHigher is true);
               otherwise @false.
               If bOrHigher is @true the return value is the result of
                @TRUE if (bOrHigher and (GetWindowsType > iVer)) is true;
                @FALSE if GetWindowsType < (requested version)
               )
       }

    class function IsWindows98(bOrHigher: boolean = False): boolean;
      virtual;

      {@Name checks if the system has the version given in the function name.
       @param(bOrHigher defines if the return value should also be @true if the system
              is better/higher than the requested system version.)
       @return(@Name returns @true if the system is the requested version (or higher if bOrHigher is true);
               otherwise @false.
               If bOrHigher is @true the return value is the result of
                @TRUE if (bOrHigher and (GetWindowsType > iVer)) is true;
                @FALSE if GetWindowsType < (requested version)
               )
       }

    class function IsWindowsME(bOrHigher: boolean = False): boolean;
      virtual;

            {@Name checks if the system has the version given in the function name.
       @param(bOrHigher defines if the return value should also be @true if the system
              is better/higher than the requested system version.)
       @return(@Name returns @true if the system is the requested version (or higher if bOrHigher is true);
               otherwise @false.
               If bOrHigher is @true the return value is the result of
                @TRUE if (bOrHigher and (GetWindowsType > iVer)) is true;
                @FALSE if GetWindowsType < (requested version)
               )
       }
    class function IsWindows2000(bOrHigher: boolean = False): boolean;
      virtual;


      {@Name checks if the system has the version given in the function name.
       @param(bOrHigher defines if the return value should also be @true if the system
              is better/higher than the requested system version.)
       @return(@Name returns @true if the system is the requested version (or higher if bOrHigher is true);
               otherwise @false.
               If bOrHigher is @true the return value is the result of
                @TRUE if (bOrHigher and (GetWindowsType > iVer)) is true;
                @FALSE if GetWindowsType < (requested version)
               )
       }
    class function IsWindows2003(bOrHigher: boolean = False): boolean;
      virtual;


      {@Name checks if the system has the version given in the function name.
       @param(bOrHigher defines if the return value should also be @true if the system
              is better/higher than the requested system version.)
       @return(@Name returns @true if the system is the requested version (or higher if bOrHigher is true);
               otherwise @false.
               If bOrHigher is @true the return value is the result of
                @TRUE if (bOrHigher and (GetWindowsType > iVer)) is true;
                @FALSE if GetWindowsType < (requested version)
               )
       }
    class function IsWindows2003R2(bOrHigher: boolean = False): boolean;
      virtual;


      {@Name checks if the system has the version given in the function name.
       @param(bOrHigher defines if the return value should also be @true if the system
              is better/higher than the requested system version.)
       @return(@Name returns @true if the system is the requested version (or higher if bOrHigher is true);
               otherwise @false.
               If bOrHigher is @true the return value is the result of
                @TRUE if (bOrHigher and (GetWindowsType > iVer)) is true;
                @FALSE if GetWindowsType < (requested version)
               )
       }
    class function IsWindowsXP(bOrHigher: boolean = False): boolean;
      virtual;


      {@Name checks if the system has the version given in the function name.

       Actually the parameter bOrHigher has no meaning in this function!

       @param(bOrHigher defines if the return value should also be @true if the system
              is better/higher than the requested system version.)
       @return(@Name returns @true if the system is the requested version (or higher if bOrHigher is true);
               otherwise @false.
               If bOrHigher is @true the return value is the result of
                @TRUE if (bOrHigher and (GetWindowsType > iVer)) is true;
                @FALSE if GetWindowsType < (requested version)
               )
       }
    class function IsWindowsVista(bOrHigher: boolean = False): boolean;
      virtual;

      {@Name checks if the system has the version given in the function name.

       Actually the parameter bOrHigher has no meaning in this function!

       @param(bOrHigher defines if the return value should also be @true if the system
              is better/higher than the requested system version.)
       @return(@Name returns @true if the system is the requested version (or higher if bOrHigher is true);
               otherwise @false.
               If bOrHigher is @true the return value is the result of
                @TRUE if (bOrHigher and (GetWindowsType > iVer)) is true;
                @FALSE if GetWindowsType < (requested version)
               )
       }
    class function IsWindows2008(bOrHigher: boolean = False): boolean;
      virtual;

      {@Name checks if the system is a server version
       @return(Returns true if the system is a server; otherwise false (Workstation).)}
    class function IsServer: boolean; virtual;


      {@Name raises an EJwsclUnsupportedWindowsVersionException exception if
       the actual windows version does not correspond to the required one in the parameters.

       @param(iWinVer contains a cOsXXXXX onstant that is defined in JwsclConstants.
              If iWinVer is not between the bounds of sOSVerString the value iWinVer will be set to -1 without an error.)
       @param(bOrHigher If true the exception will only be raised if the actual system version
               is smaller than the given on in iWinVer; otherwise the system version must be exactly the given one in iWinVer)
       @param(SourceProc contains the caller method name to be displayed in the exception message)
       @param(SourceClass contains the caller class name to be displayed in the exception message)
       @param(SourceFile contains the caller file name to be displayed in the exception message)
       @param(SourcePos contains the caller source position to be displayed in the exception message)

       @raises(EJwsclUnsupportedWindowsVersionException will be raised if the following expression is false :
                ((fWindowsType = iWinVer) or
                (bOrHigher and (fWindowsType > iWinVer))) )
       }
    class procedure CheckWindowsVersion(iWinVer: integer;
      bOrHigher: boolean; SourceProc, SourceClass, SourceFile: TJwString;
      SourcePos: Cardinal); virtual;

    class function IsTerminalServiceRunning : Boolean;
  end;


{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation

uses SysConst;


{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}




//pseudo class variable
var
  fWindowsType: integer;
  fIsServer: boolean;

class function TJwFileVersion.GetFileInfo(const Filename: TJwString;
  var FileVersionInfo: TFileVersionInfo): Boolean;
var VerInfoSize: DWORD;
 DummyVar: DWORD;
 VersionInfo: Pointer;
 Translation: Pointer;
 VersionValue: TJwString;

function VerInfoQuery(VerInfo: Pointer; VerValue: TJwString): string;
var VerInfoSize: DWORD;
  VerInfoPtr: Pointer;
begin
  Result := '';

  VerInfoSize := 0;
{$IFDEF UNICODE}
  if VerQueryValueW(VerInfo, PWideChar(VerValue), VerInfoPtr, VerInfoSize) then
  begin
    Result := Trim(PWideChar(VerInfoPtr));
  end;
{$ELSE}
  if VerQueryValueA(VerInfo, PChar(VerValue), VerInfoPtr, VerInfoSize) then
  begin
    Result := Trim(PChar(VerInfoPtr));
  end;
{$ENDIF}

  VerInfoPtr := nil; // Memory is freed when freeing VersionInfo Pointer
end;

begin
  Result := False;
  ZeroMemory(@FileVersionInfo, SizeOf(FileVersionInfo));

  VerInfoSize :=
{$IFDEF UNICODE}
  GetFileVersionInfoSizeW(PWideChar(Filename), DummyVar);
{$ELSE}
  GetFileVersionInfoSizeA(PChar(Filename), DummyVar);
{$ENDIF}
  if VerInfoSize > 0 then begin

    // GetFileVersionInfoSize returns bytes and not char's so should be okay
    // for unicode and ansi
    GetMem(VersionInfo, VerInfoSize);

    try
      Result :=
{$IFDEF UNICODE}
      GetFileVersionInfoW(PWideChar(Filename), 0, VerInfoSize, VersionInfo);
{$ELSE}
      GetFileVersionInfoA(PChar(Filename), 0, VerInfoSize, VersionInfo);
{$ENDIF}

      // Exit on failure
      if not Result then Exit;

      Result :=
{$IFDEF UNICODE}
      VerQueryValueW(VersionInfo, '\VarFileInfo\Translation',
        Translation, VerInfoSize);
{$ELSE}
      VerQueryValueA(VersionInfo, '\VarFileInfo\Translation',
        Translation, VerInfoSize);
{$ENDIF}

      // Exit on failure
      if not Result then Exit;

      VersionValue := Format('\StringFileInfo\%.4x%.4x\',
        [LoWord(Integer(Translation^)), HiWord(Integer(Translation^))]);

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


constructor TJwServerInfo.Create(const Server: TJwString);
var
  nStatus: NET_API_STATUS;
  pwServer: PWideChar;
  ServerInfoPtr: PServerInfo101;
begin
  pwServer := PWideChar(WideString(Server));

  nStatus := NetServerGetInfo(pwServer, 101, PByte(ServerInfoPtr));

  if nStatus = NERR_Success then
  begin
    FServer := ServerInfoPtr^.sv101_name;
    {Specifies, in the least significant 4 bits of the byte, the major release
     version number of the operating system. The most significant 4 bits of the
     byte specifies the server type. The mask MAJOR_VERSION_MASK should be used
     to ensure correct results.}
    FMajorVersion  := ServerInfoPtr^.sv101_version_major and MAJOR_VERSION_MASK;
    FMinorVersion  := ServerInfoPtr^.sv101_version_minor;
    FIsServer :=
      (JwCheckBitMask(ServerInfoPtr^.sv101_type, SV_TYPE_DOMAIN_CTRL)) or
      (JwCheckBitMask(ServerInfoPtr^.sv101_type, SV_TYPE_DOMAIN_BAKCTRL)) or
      (JwCheckBitMask(ServerInfoPtr^.sv101_type, SV_TYPE_SERVER_NT));
    FIsTerminalServer :=
      (JwCheckBitMask(ServerInfoPtr^.sv101_type, SV_TYPE_TERMINALSERVER));
    FWindowsType := GetWindowsType;

    // Free Memory
    NetApiBufferFree(ServerInfoPtr);
  end
  else begin
    raise EJwsclWinCallFailedException.CreateFmtEx('',
      'NetServerGetInfo', ClassName, RsUNVersion, 0, nStatus,
          [FServer]);
  end;
end;

function TJwServerInfo.GetWindowsType: Integer;
begin
  if FMajorVersion <= 4 then
  begin
    Result := cOsWinNT;
  end
  else if (FMajorVersion = 5) and (FMinorVersion = 0) then
  begin
    Result := cOsWin2000;
  end
  else if (FMajorVersion = 5) and (FMinorVersion = 1) then
  begin
    Result := cOsXP;
  end
  else if (FMajorVersion = 5) and (FMinorVersion = 2) then
  begin
    // Is there a way to determine 2003 R2 remotely?
    Result := cOS2003;
  end
  else if (FMajorVersion = 6) and (FMinorVersion = 0) and (not fIsServer) then
  begin
    Result := cOsVista;
  end
  else if (FMajorVersion = 6) and (FMinorVersion = 0) and (fIsServer) then
  begin
    Result := cOsWin2008;
  end
  else if (FMajorVersion = 6) and (FMinorVersion = 0) then
  begin
    Result := cOsVista;
  end
  else
  begin
    Result := cOsUnknown;
  end;
end;

function TJwServerInfo.IsWindows2000(bOrHigher: Boolean = False): Boolean;
const
  iVer = cOsWin2000;
begin
  Result := (FWindowsType = iVer) or (bOrHigher and (FWindowsType > iVer));
end;

function TJwServerInfo.IsWindows2003(bOrHigher: Boolean = False): Boolean;
const
  iVer = cOs2003;
begin
  Result := (FWindowsType = iVer) or (bOrHigher and (FWindowsType > iVer));
end;

function TJwServerInfo.IsWindowsXP(bOrHigher: Boolean = False): Boolean;
const
  iVer = cOsXP;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and (fWindowsType > iVer));
end;

function TJwServerInfo.IsWindowsVista(bOrHigher: Boolean = False): Boolean;
const
  iVer = cOsVista;
begin
  Result := (FWindowsType = iVer) or (bOrHigher and (FWindowsType > iVer));
end;

function TJwServerInfo.IsWindows2008(bOrHigher: Boolean = False): Boolean;
const
  iVer = cOsWin2008;
begin
  Result := (FWindowsType = iVer) or (bOrHigher and (FWindowsType > iVer));
end;



class procedure TJwWindowsVersion.CheckWindowsVersion(iWinVer: integer;
  bOrHigher: boolean; SourceProc, SourceClass, SourceFile: TJwString;
  SourcePos: Cardinal);
var
  sOrHigher, sActWinVer, sWinVer: TJwString;

begin
  if (iWinVer < low(sOSVerString)) or (iWinVer > high(sOSVerString)) then
    iWinVer := -1;

  sWinVer := sOSVerString[iWinVer];

  try
    sActWinVer := sOSVerString[fWindowsType];
  except
    sActWinVer := sOSVerString[-1];
  end;

  sOrHigher := '';
  if bOrHigher then
    sOrHigher := RsVersionOrHigher;

  if not ((fWindowsType = iWinVer) or (bOrHigher and
    (fWindowsType > iWinVer))) then
    raise EJwsclUnsupportedWindowsVersionException.CreateFmtEx(
      RsVersionUnsupportedVersion, SourceProc, SourceClass,
      SourceFile, SourcePos, False,
      [sActWinVer,
      sWinVer, sOrHigher]);
end;


class function TJwWindowsVersion.IsWindows95(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOsWin95;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindows98(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOsWin95;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindowsME(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOsWinME;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindows2000(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOsWin2000;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindows2003(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOs2003;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindows2003R2(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOs2003R2;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindowsXP(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOsXP;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindowsVista(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOsVista;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsWindows2008(bOrHigher: boolean = False)
: boolean;
const
  iVer = cOsWin2008;
begin
  Result := (fWindowsType = iVer) or (bOrHigher and
    (fWindowsType > iVer));
end;

class function TJwWindowsVersion.IsServer: boolean;
begin
  Result := fIsServer;
end;

class function TJwWindowsVersion.GetWindowsType: integer;
var
  osVerInfo:
{$IFDEF UNICODE}TOSVersionInfoExW{$ELSE}
  TOSVersionInfoExA
{$ENDIF}
  ;
  majorVer, minorVer: integer;
begin
  //  Result := cOsUnknown;

  osVerInfo.dwOSVersionInfoSize := SizeOf(osVerInfo);

  if
{$IFDEF UNICODE}GetVersionExW{$ELSE}
  GetVersionExA
{$ENDIF}
    (@osVerInfo) then
  begin
    majorVer  := osVerInfo.dwMajorVersion;
    minorVer  := osVerInfo.dwMinorVersion;
    fIsServer := osVerInfo.wProductType <> VER_NT_WORKSTATION;

    case osVerInfo.dwPlatformId of
      VER_PLATFORM_WIN32_NT: { Windows NT/2000 }
      begin
        if majorVer <= 4 then
          Result := cOsWinNT
        else if (majorVer = 5) and (minorVer = 0) then
          Result := cOsWin2000
        else if (majorVer = 5) and (minorVer = 1) then
          Result := cOsXP
        else if (majorVer = 5) and (minorVer = 2) then
        begin
          if boolean(GetSystemMetrics(SM_SERVERR2)) then
            Result := cOS2003R2
          else
            Result := cOS2003;
        end
        else if (majorVer = 6) and (minorVer = 0) and (not fIsServer) then
          Result := cOsVista
        else if (majorVer = 6) and (minorVer = 0) and (fIsServer) then
          Result := cOsWin2008
        else if (majorVer = 6) and (minorVer = 0) then
          Result := cOsVista
        else
          Result := cOsUnknown;
      end;
      VER_PLATFORM_WIN32_WINDOWS:  { Windows 9x/ME }
      begin
        if (majorVer = 4) and (minorVer = 0) then
          Result := cOsWin95
        else if (majorVer = 4) and (minorVer = 10) then
        begin
          if osVerInfo.szCSDVersion[1] = 'A' then
            Result := cOsWin98SE
          else
            Result := cOsWin98;
        end
        else if (majorVer = 4) and (minorVer = 90) then
          Result := cOsWinME
        else
          Result := cOsUnknown;
      end;
      else
        Result := cOsUnknown;
    end;
  end
  else
    Result := cOsUnknown;
end;

class function TJwWindowsVersion.IsTerminalServiceRunning: Boolean;
begin
  result := JwaWindows.IsTerminalServiceRunning;
end;

{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}


initialization
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INITIALIZATION_SECTION}
  fWindowsType := TJwWindowsVersion.GetWindowsType;

{$ENDIF SL_INITIALIZATION_SECTION}


{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}