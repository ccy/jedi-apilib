{******************************************************************************}
{ JEDI File Transactions API example                                           }
{ http://jedi-apilib.sourceforge.net                                           }
{ http://wiki.delphi-jedi.org/                                                 }
{ http://blog.delphi-jedi.net/                                                 }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ Author(s): Olaf Hess (olafhess at newsguy dot com)                           }
{ Creation date: December 1st 2009                                             }
{ Last modification date: November 28th 2010                                   }
{                                                                              }
{ Description: Demonstrates how to use the file system transactions introduced }
{              with Windows Vista / Server 2008                                }
{                                                                              }
{ Preparations: JWA must be ready to use.                                      }
{                                                                              }
{ Version history: December 1st 2009: initial version                          }
{                  November 28th 2010: adapted to use JWA                      }
{                                                                              }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in         }
{ production environments.                                                     }
{******************************************************************************}

//----------------------------------------------------------------------------//
// This code was first published as part of the article                       //
// "Transaktionen mit Vista und NTFS" ("Transactions with Vista and NTFS")    //
// in issue 1 / 2010 of "Toolbox Magazin" (http://www.toolbox-mag.de) written //
// by Olaf Hess. Used with permission. Donated to the JEDI API Project.       //
//----------------------------------------------------------------------------//

{$I ..\..\Includes\jproject\jedi.inc}

{$IFDEF DELPHI7_UP}
    {$WARN SYMBOL_PLATFORM OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

unit TransactionClassU;

interface

uses SysUtils,
{$IFDEF JWA_WINDOWS}
     JwaWindows;
{$ELSE}
     JwaWinBase, JwaWinType;
{$ENDIF JWA_WINDOWS}

type
    ETransactionError = class (Exception);

    TCopyFileFlag = (cffCopySymlink, cffFailIfExists, cffOpenSourceForWrite,
                     cffFileRestartable);
    TCopyFileFlags = set of TCopyFileFlag;

    TMoveFileFlag = (mffReplaceExisting, mffCopyAllowed, mffDelayUntilReboot,
                     mffWriteThrough, mffCreateHardLink, mffFailIfNotTrackable);
    TMoveFileFlags = set of TMoveFileFlag;

    TTransaction = class
      private
        FHandle : THandle;
        bRaiseExceptions : Boolean;

      public
        constructor Create (const sDescription: WideString;
                            const bRaiseExceptions: Boolean = true);
        constructor CreateTimeout (const sDescription: WideString;
                                   const dwTimeoutMSec: DWord;
                                   const bRaiseExceptions: Boolean = true);
        destructor Destroy; override;

        function Commit : Boolean;
        function Rollback : Boolean;

        function CopyFile (const sExistingFile, sNewFile: TFileName;
                           const CopyFileFlags: TCopyFileFlags = []) : Boolean;

        function DeleteFile (const sFileName: TFileName) : Boolean;

        function DirectoryExists (const sDirectory: TFileName) : Boolean;

        function FileCreate (const sFileName: String) : Integer;

        function FileExists (const sFileName: TFileName) : Boolean;

        function FileOpen (const sFileName: String;
                           const Mode: LongWord) : Integer;

        function FileGetAttr (const sFileName: String) : Integer;

        function FindFirst (const sPath: String; const iAttr: Integer;
                            var F: TSearchRec) : Integer;

        function MoveFile (const sExistingFile, sNewFile: TFileName;
                           const MoveFileFlags: TMoveFileFlags = []) : Boolean;

        function RemoveDir (const sDir: TFileName) : Boolean;

        property Handle : THandle read FHandle;
    end; { TTransaction }

implementation

uses
{$IFNDEF JWA_WINDOWS}
     JwaNative, JwaWinnt,
{$ENDIF JWA_WINDOWS}
     JwaKtmW32;

(* ---- *)

constructor TTransaction.Create (const sDescription: WideString;
                                 const bRaiseExceptions: Boolean = true);
begin
    CreateTimeout (sDescription, 0, bRaiseExceptions);
end; { TTransaction.Create }

(* ---- *)

constructor TTransaction.CreateTimeout (const sDescription: WideString;
                                        const dwTimeoutMSec: DWord;
                                        const bRaiseExceptions: Boolean);
var
    psDescription : PWideChar;

begin
    if (sDescription <> '') then
        psDescription := PWideChar (sDescription)
    else psDescription := NIL;

    FHandle := CreateTransaction (NIL, NIL, 0, 0, 0, dwTimeoutMSec,
                                  psDescription);

    Win32Check (FHandle <> INVALID_HANDLE_VALUE);

    Self.bRaiseExceptions := bRaiseExceptions;
end; { TTransaction.CreateTimeout }

(* ---- *)

destructor TTransaction.Destroy;

var
    bResult : Boolean;

begin
    if (FHandle <> INVALID_HANDLE_VALUE) then
    begin
        // Rolls back the transaction if not commited
        bResult := CloseHandle (FHandle);

        if (bResult = false) and (bRaiseExceptions) then
            RaiseLastWin32Error;
    end; { if }

    inherited;
end; { TTransaction.Destroy }

(* ---- *)

function TTransaction.Commit : Boolean;
begin
    Result := CommitTransaction (FHandle);

    if (Result) then
    begin
        CloseHandle (FHandle);
        FHandle := INVALID_HANDLE_VALUE;
    end; { if }

    if (Result = false) and (bRaiseExceptions) then
        RaiseLastWin32Error;
end; { TTransaction.Commit }

(* ---- *)

function TTransaction.Rollback : Boolean;
begin
    Result := RollbackTransaction (FHandle);

    if (Result) then
    begin
        CloseHandle (FHandle);
        FHandle := INVALID_HANDLE_VALUE;
    end; { if }

    if (Result = false) and (bRaiseExceptions) then
        RaiseLastWin32Error;
end; { TTransaction.Rollback }

(* ---- *)

function TTransaction.CopyFile (const sExistingFile, sNewFile: TFileName;
                                const CopyFileFlags: TCopyFileFlags) : Boolean;

const
    cCopyFileFlags : array [TCopyFileFlag] of DWord =
        (COPY_FILE_COPY_SYMLINK, COPY_FILE_FAIL_IF_EXISTS,
         COPY_FILE_OPEN_SOURCE_FOR_WRITE, COPY_FILE_RESTARTABLE);

var
    dwFlags : DWord;
    CopyFileFlag : TCopyFileFlag;

begin
    Assert ((sExistingFile <> '') and (sNewFile <> ''));

    dwFlags := 0;

    if (CopyFileFlags <> []) then
        for CopyFileFlag := Low (TCopyFileFlag) to High (TCopyFileFlag) do
            if (CopyFileFlag in CopyFileFlags) then
                dwFlags := dwFlags or cCopyFileFlags [CopyFileFlag];

    Result := CopyFileTransacted (PChar (sExistingFile), PChar (sNewFile), NIL,
                                  NIL, NIL, dwFlags, FHandle);

    if (Result = false) and (bRaiseExceptions) then
        RaiseLastWin32Error;
end; { TTransaction.CopyFile }

(* ---- *)

function TTransaction.DeleteFile (const sFileName: TFileName) : Boolean;
begin
    Result := DeleteFileTransacted (PChar (sFileName), FHandle);

    if (Result = false) and (bRaiseExceptions) then
        RaiseLastWin32Error;
end; { TTransaction.DeleteFile }

(* ---- *)

function TTransaction.DirectoryExists (const sDirectory: TFileName) : Boolean;

var
    FileInfo : TWin32FileAttributeData;

begin
    if (GetFileAttributesTransacted (PChar (sDirectory), GetFileExInfoStandard,
                                     FileInfo, FHandle)) then
        Result := (FileInfo.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) =
                                                        FILE_ATTRIBUTE_DIRECTORY
    else Result := false;
end; { TTransaction.DirectoryExists }

(* ---- *)

function TTransaction.FileCreate (const sFileName: String) : Integer;

var
    hResult : THandle;

begin
    hResult := CreateFileTransacted (PChar (sFileName),
                                     GENERIC_READ or GENERIC_WRITE, 0, NIL,
                                     CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0,
                                     FHandle, NIL, NIL);

    Result := Integer (hResult);

    if (hResult = INVALID_HANDLE_VALUE) and (bRaiseExceptions) then
        RaiseLastWin32Error;
end; { TTransaction.FileCreate }

(* ---- *)

function TTransaction.FileExists (const sFileName: TFileName) : Boolean;

var
    FileInfo : TWin32FileAttributeData;

begin
    if (GetFileAttributesTransacted (PChar (sFileName), GetFileExInfoStandard,
                                     FileInfo, FHandle)) then
        Result := (FileInfo.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <>
                                                        FILE_ATTRIBUTE_DIRECTORY
    else Result := false;
end; { TTransaction.FileExists }

(* ---- *)

function TTransaction.FileOpen (const sFileName: String;
                                const Mode: LongWord) : Integer;

const
    AccessMode : array [0..2] of LongWord = (GENERIC_READ,
                                             GENERIC_WRITE,
                                             GENERIC_READ or GENERIC_WRITE);
    ShareMode : array [0..4] of LongWord = (0,
                                            0,
                                            FILE_SHARE_READ,
                                            FILE_SHARE_WRITE,
                                            FILE_SHARE_READ or FILE_SHARE_WRITE);

var
    hResult : THandle;

begin
    hResult := INVALID_HANDLE_VALUE;

    if ((Mode and 3) <= fmOpenReadWrite) and
       ((Mode and $F0) <= fmShareDenyNone) then
        hResult := CreateFileTransacted (PChar (sFileName),
                                         AccessMode [Mode and 3],
                                         ShareMode [(Mode and $F0) shr 4],
                                         NIL, OPEN_EXISTING,
                                         FILE_ATTRIBUTE_NORMAL, 0, FHandle, NIL,
                                         NIL);

    Result := Integer (hResult);

    if (hResult = INVALID_HANDLE_VALUE) and (bRaiseExceptions) then
        RaiseLastWin32Error;
end; { TTransaction.FileOpen }

(* ---- *)

function TTransaction.FileGetAttr (const sFileName: String) : Integer;

var
    FileInfo : TWin32FileAttributeData;

begin
    if (GetFileAttributesTransacted (PChar (sFileName), GetFileExInfoStandard,
                                     FileInfo, FHandle)) then
        Result := Integer (FileInfo.dwFileAttributes)
    else
    begin
        Result := (-1);

        if (bRaiseExceptions) then
            RaiseLastWin32Error;
    end; { if };
end; { TTransaction.FileGetAttr }

(* ---- *)

function TTransaction.FindFirst (const sPath: String; const iAttr: Integer;
                                 var F: TSearchRec) : Integer;

    (* ---- *)

    function FindMatchingFile (var F: TSearchRec) : Integer;

    type
{$IFDEF JWA_WINDOWS}
        TFileTime = JwaWindows.TFileTime;
{$ELSE}
        TFileTime = JwaWinBase.TFileTime;
{$ENDIF JWA_WINDOWS}
//      TFileTime = Windows.TFileTime;

    var
        LocalFileTime : TFileTime;

    begin
        with F do
        begin
            while (FindData.dwFileAttributes and ExcludeAttr <> 0) do
                if not (FindNextFile (FindHandle,
                                      TWin32FindData (FindData))) then
                begin
                    Result := GetLastError;
                    exit;
                end; { if }

            FileTimeToLocalFileTime (TFileTime (FindData.ftLastWriteTime),
                                     TFileTime (LocalFileTime));
            FileTimeToDosDateTime (TFileTime (LocalFileTime),
                                   LongRec (Time).Hi, LongRec (Time).Lo);

            Size := FindData.nFileSizeLow;
            Attr := FindData.dwFileAttributes;
            Name := FindData.cFileName;
        end; { with }

        Result := 0;
    end; { FindMatchingFile }

    (* ---- *)

const
    faSpecial = faHidden or faSysFile or faVolumeID or faDirectory;

begin
    Assert (sPath <> '');

    F.ExcludeAttr := not iAttr and faSpecial;
    F.FindHandle := FindFirstFileTransacted (PChar (sPath), FindExInfoStandard,
                                             TWin32FindData (F.FindData),
                                             FindExSearchNameMatch, NIL, 0,
                                             FHandle);

    if (F.FindHandle <> INVALID_HANDLE_VALUE) then
    begin
        Result := FindMatchingFile (F);

        if (Result <> 0) then
            SysUtils.FindClose (F);
    end { if }
    else Result := GetLastError;
end; { TTransaction.FindFirst }

(* ---- *)

function TTransaction.MoveFile (const sExistingFile, sNewFile: TFileName;
                                const MoveFileFlags: TMoveFileFlags) : Boolean;

const
    cMoveFileFlags : array [TMoveFileFlag] of DWord =
        (MOVEFILE_REPLACE_EXISTING, MOVEFILE_COPY_ALLOWED,
         MOVEFILE_DELAY_UNTIL_REBOOT, MOVEFILE_WRITE_THROUGH,
         MOVEFILE_CREATE_HARDLINK, MOVEFILE_FAIL_IF_NOT_TRACKABLE);

var
    dwFlags : DWord;
    MoveFileFlag : TMoveFileFlag;

begin
    Assert ((sExistingFile <> '') and (sNewFile <> ''));

    dwFlags := 0;

    if (MoveFileFlags <> []) then
        for MoveFileFlag := Low (TMoveFileFlag) to High (TMoveFileFlag) do
            if (MoveFileFlag in MoveFileFlags) then
                dwFlags := dwFlags or cMoveFileFlags [MoveFileFlag];

    Result := MoveFileTransacted (PChar (sExistingFile), PChar (sNewFile),
                                  NIL, NIL, dwFlags, FHandle);

    if (Result = false) and (bRaiseExceptions) then
        RaiseLastWin32Error;
end; { TTransaction.MoveFile }

(* ---- *)

function TTransaction.RemoveDir (const sDir: TFileName) : Boolean;
begin
    Result := RemoveDirectoryTransacted (PChar (sDir), FHandle);

    if (Result = false) and (bRaiseExceptions) then
        RaiseLastWin32Error;
end; { TTransaction.RemoveDir }

(* ---- *)

end.
