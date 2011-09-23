{******************************************************************************}
{                                                                              }
{ FileIO Functions and Transactional NTFS API interface Unit for Object Pascal }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2008 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ Portions created by Olaf Hess are Copyright (C) 2010 Olaf Hess.              }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI         }
{ APILIB home page, located at http://jedi-apilib.sourceforge.net              }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaKtmW32;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

{$IFDEF JWA_WINDOWS}
  uses JwaWindows;
{$ELSE}
  uses JwaWinBase, JwaWinUser, JwaWinType, JwaWinReg;
{$ENDIF JWA_WINDOWS}

{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

const
    TRANSACTION_DO_NOT_PROMOTE = 1;
    FILE_SUPPORTS_TRANSACTIONS = $00200000;

    MOVEFILE_REPLACE_EXISTING       = $00000001;
    MOVEFILE_COPY_ALLOWED           = $00000002;
    MOVEFILE_DELAY_UNTIL_REBOOT     = $00000004;
    MOVEFILE_WRITE_THROUGH          = $00000008;
    MOVEFILE_CREATE_HARDLINK        = $00000010;
    MOVEFILE_FAIL_IF_NOT_TRACKABLE  = $00000020;

    // Constants used by CopyFileTransacted
    COPY_FILE_OPEN_SOURCE_FOR_WRITE = $00000004;
    COPY_FILE_COPY_SYMLINK              = $00000800;

//
//  Either of these constants may be used to identify the miniversion to open.
//
    TXFS_MINIVERSION_COMMITTED_VIEW = $0000;
    TXFS_MINIVERSION_DIRTY_VIEW     = $FFFF;
    TXFS_MINIVERSION_DEFAULT_VIEW   = $FFFE;

//
// MessageId: ERROR_TRANSACTIONS_UNSUPPORTED_REMOTE
//
// MessageText:
//
// The remote server or share does not support transacted file operations.
//
    ERROR_TRANSACTIONS_UNSUPPORTED_REMOTE = 6805;

type
    TCopyProgressRoutine = function (iTotalFileSize, iTotalBytesTransferred,
    	iStreamSize, iStreamBytesTransferred: Large_Integer;
        dwStreamNumber, dwCallbackReason: DWORD;
        hSourceFile, hDestinationFile: THandle;
        lpData: Pointer) : DWORD; stdcall;

function CommitTransaction (TransactionHandle: THandle) : Bool; stdcall;
{$EXTERNALSYM CommitTransaction}

function CommitTransactionAsync (TransactionHandle: THandle) : Bool; stdcall;
{$EXTERNALSYM CommitTransactionAsync}

function CopyFileTransactedA (lpExistingFileName, lpNewFileName: LPSTR;
  lpProgressRoutine: TCopyProgressRoutine; lpData: Pointer; pbCancel: PBool;
  dwCopyFlags: DWORD; hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM CopyFileTransactedA}
function CopyFileTransactedW (lpExistingFileName, lpNewFileName: LPWSTR;
  lpProgressRoutine: TCopyProgressRoutine; lpData: Pointer; pbCancel: PBool;
  dwCopyFlags: DWORD; hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM CopyFileTransactedW}

{$IFDEF UNICODE}
function CopyFileTransacted (lpExistingFileName, lpNewFileName: LPWSTR;
  lpProgressRoutine: TCopyProgressRoutine; lpData: Pointer; pbCancel: PBool;
  dwCopyFlags: DWORD; hTransaction: THandle) : Bool; stdcall;
{$ELSE}
function CopyFileTransacted (lpExistingFileName, lpNewFileName: LPSTR;
  lpProgressRoutine: TCopyProgressRoutine; lpData: Pointer; pbCancel: PBool;
  dwCopyFlags: DWORD; hTransaction: THandle) : Bool; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM CopyFileTransacted}

function CreateDirectoryTransactedA (lpTemplateDirectory, lpNewDirectory: LPSTR;
  lpSecurityAttributes: PSecurityAttributes;
  hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM CreateDirectoryTransactedA}
function CreateDirectoryTransactedW (lpTemplateDirectory, lpNewDirectory: LPWSTR;
  lpSecurityAttributes: PSecurityAttributes;
  hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM CreateDirectoryTransactedW}

{$IFDEF UNICODE}
function CreateDirectoryTransacted (lpTemplateDirectory, lpNewDirectory: LPWSTR;
  lpSecurityAttributes: PSecurityAttributes;
  hTransaction: THandle) : Bool; stdcall;
{$ELSE}
function CreateDirectoryTransacted (lpTemplateDirectory, lpNewDirectory: LPSTR;
  lpSecurityAttributes: PSecurityAttributes;
  hTransaction: THandle) : Bool; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM CreateDirectoryTransacted}

function CreateFileTransactedA (lpFileName: LPSTR;
  dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes;
  dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile, hTransaction: THandle; pusMiniVersion: PUShort;
  pExtendedParameter: Pointer) : THandle; stdcall;
{$EXTERNALSYM CreateFileTransactedA}
function CreateFileTransactedW (lpFileName: LPWSTR;
  dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes;
  dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile, hTransaction: THandle; pusMiniVersion: PUShort;
  pExtendedParameter: Pointer) : THandle; stdcall;
{$EXTERNALSYM CreateFileTransactedW}

{$IFDEF UNICODE}
function CreateFileTransacted (lpFileName: LPWSTR;
  dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes;
  dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile, hTransaction: THandle; pusMiniVersion: PUShort;
  pExtendedParameter: Pointer) : THandle; stdcall;
{$ELSE}
function CreateFileTransacted (lpFileName: LPSTR;
  dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes;
  dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile, hTransaction: THandle; pusMiniVersion: PUShort;
  pExtendedParameter: Pointer) : THandle; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM CreateFileTransacted}

function CreateHardLinkTransactedA (lpFileName, lpExistingFileName: LPSTR;
  lpSecurityAttributes: PSecurityAttributes;
  hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM CreateHardLinkTransactedA}
function CreateHardLinkTransactedW (lpFileName, lpExistingFileName: LPWSTR;
  lpSecurityAttributes: PSecurityAttributes;
  hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM CreateHardLinkTransactedW}

{$IFDEF UNICODE}
function CreateHardLinkTransacted (lpFileName, lpExistingFileName: LPWSTR;
  lpSecurityAttributes: PSecurityAttributes;
  hTransaction: THandle) : Bool; stdcall;
{$ELSE}
function CreateHardLinkTransacted (lpFileName, lpExistingFileName: LPSTR;
  lpSecurityAttributes: PSecurityAttributes;
  hTransaction: THandle) : Bool; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM CreateHardLinkTransacted}

function CreateSymbolicLinkTransactedA (
  lpSymlinkFileName, lpTargetFileName: LPSTR; dwFlags: DWORD;
  hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM CreateSymbolicLinkTransactedA}
function CreateSymbolicLinkTransactedW (
  lpSymlinkFileName, lpTargetFileName: LPWSTR; dwFlags: DWORD;
  hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM CreateSymbolicLinkTransactedW}

{$IFDEF UNICODE}
function CreateSymbolicLinkTransacted (
  lpSymlinkFileName, lpTargetFileName: LPWSTR; dwFlags: DWORD;
  hTransaction: THandle) : Bool; stdcall;
{$ELSE}
function CreateSymbolicLinkTransacted (
  lpSymlinkFileName, lpTargetFileName: LPSTR; dwFlags: DWORD;
  hTransaction: THandle) : Bool; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM CreateSymbolicLinkTransacted}

function CreateTransaction (lpTransactionAttributes: PSecurityAttributes;
  UOW: PGUID;
  dwCreateOptions, dwIsolationLevel, dwIsolationFlags, dwTimeout: DWORD;
  Description: LPWSTR) : THandle; stdcall;
{$EXTERNALSYM CreateTransaction}

function DeleteFileTransactedA (lpFileName: LPSTR;
  hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM DeleteFileTransactedA}
function DeleteFileTransactedW (lpFileName: LPWSTR;
  hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM DeleteFileTransactedW}

{$IFDEF UNICODE}
function DeleteFileTransacted (lpFileName: LPWSTR;
  hTransaction: THandle) : Bool; stdcall;
{$ELSE}
function DeleteFileTransacted (lpFileName: LPSTR;
  hTransaction: THandle) : Bool; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM DeleteFileTransacted}

function FindFirstFileNameTransactedW (lpFileName: LPWSTR;
  dwFlags: DWORD; var StringLength: DWORD; LinkName: LPWSTR;
  hTransaction: THandle) : THandle; stdcall;
{$EXTERNALSYM FindFirstFileNameTransactedW}

function FindFirstFileTransactedA (lpFileName: LPSTR;
  fInfoLevelId: TFindexInfoLevels;
  var lpFindFileData: TWIN32FindDataA; fSearchOp: TFindexSearchOps;
  lpSearchFilter: Pointer; dwAdditionalFlags: DWORD;
  hTransaction: THandle) : THandle; stdcall;
{$EXTERNALSYM FindFirstFileTransactedA}
function FindFirstFileTransactedW (lpFileName: LPWSTR;
  fInfoLevelId: TFindexInfoLevels;
  var lpFindFileData: TWIN32FindDataW; fSearchOp: TFindexSearchOps;
  lpSearchFilter: Pointer; dwAdditionalFlags: DWORD;
  hTransaction: THandle) : THandle; stdcall;
{$EXTERNALSYM FindFirstFileTransactedW}

{$IFDEF UNICODE}
function FindFirstFileTransacted (lpFileName: LPWSTR;
  fInfoLevelId: TFindexInfoLevels;
  var lpFindFileData: TWIN32FindDataW; fSearchOp: TFindexSearchOps;
  lpSearchFilter: Pointer; dwAdditionalFlags: DWORD;
  hTransaction: THandle) : THandle; stdcall;
{$ELSE}
function FindFirstFileTransacted (lpFileName: LPSTR;
  fInfoLevelId: TFindexInfoLevels;
  var lpFindFileData: TWIN32FindDataA; fSearchOp: TFindexSearchOps;
  lpSearchFilter: Pointer; dwAdditionalFlags: DWORD;
  hTransaction: THandle) : THandle; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM FindFirstFileTransacted}

function GetCompressedFileSizeTransactedA (lpFileName: LPSTR;
  lpFileSizeHigh: LPDWORD; hTransaction: THandle) : DWORD; stdcall;
{$EXTERNALSYM GetCompressedFileSizeTransactedA}
function GetCompressedFileSizeTransactedW (lpFileName: LPWSTR;
  lpFileSizeHigh: LPDWORD; hTransaction: THandle) : DWORD; stdcall;
{$EXTERNALSYM GetCompressedFileSizeTransactedW}

{$IFDEF UNICODE}
function GetCompressedFileSizeTransacted (lpFileName: LPWSTR;
  lpFileSizeHigh: LPDWORD; hTransaction: THandle) : DWORD; stdcall;
{$ELSE}
function GetCompressedFileSizeTransacted (lpFileName: LPSTR;
  lpFileSizeHigh: LPDWORD; hTransaction: THandle) : DWORD; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM GetCompressedFileSizeTransacted}

function GetFileAttributesTransactedA (lpFileName: LPSTR;
  fInfoLevelId: TGetFileExInfoLevels; var FileInfo: TWin32FileAttributeData;
  hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM GetFileAttributesTransactedA}
function GetFileAttributesTransactedW (lpFileName: LPWSTR;
  fInfoLevelId: TGetFileExInfoLevels; var FileInfo: TWin32FileAttributeData;
  hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM GetFileAttributesTransactedW}

{$IFDEF UNICODE}
function GetFileAttributesTransacted (lpFileName: LPWSTR;
  fInfoLevelId: TGetFileExInfoLevels; var FileInfo: TWin32FileAttributeData;
  hTransaction: THandle) : Bool; stdcall;
{$ELSE}
function GetFileAttributesTransacted (lpFileName: LPSTR;
  fInfoLevelId: TGetFileExInfoLevels; var FileInfo: TWin32FileAttributeData;
  hTransaction: THandle) : Bool; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM GetFileAttributesTransacted}

function GetFullPathNameTransactedA (lpFileName: LPSTR; nBufferLength: DWORD;
  lpBuffer: LPSTR; lpFilePart: PLPSTR; hTransaction: THandle) : DWORD; stdcall;
{$EXTERNALSYM GetFullPathNameTransactedA}
function GetFullPathNameTransactedW (lpFileName: LPWSTR; nBufferLength: DWORD;
  lpBuffer: LPWSTR; lpFilePart: PLPWSTR; hTransaction: THandle) : DWORD; stdcall;
{$EXTERNALSYM GetFullPathNameTransactedW}

{$IFDEF UNICODE}
function GetFullPathNameTransacted (lpFileName: LPWSTR; nBufferLength: DWORD;
  lpBuffer: LPWSTR; lpFilePart: PLPWSTR; hTransaction: THandle) : DWORD; stdcall;
{$ELSE}
function GetFullPathNameTransacted (lpFileName: LPSTR; nBufferLength: DWORD;
  lpBuffer: LPSTR; lpFilePart: PLPSTR; hTransaction: THandle) : DWORD; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM GetFullPathNameTransacted}

function GetLongPathNameTransactedA (lpszShortPath, lpszLongPath: LPSTR;
  cchBuffer: DWORD; hTransaction: THandle) : DWORD; stdcall;
{$EXTERNALSYM GetLongPathNameTransactedA}
function GetLongPathNameTransactedW (lpszShortPath, lpszLongPath: LPWSTR;
  cchBuffer: DWORD; hTransaction: THandle) : DWORD; stdcall;
{$EXTERNALSYM GetLongPathNameTransactedW}

{$IFDEF UNICODE}
function GetLongPathNameTransacted (lpszShortPath, lpszLongPath: LPWSTR;
  cchBuffer: DWORD; hTransaction: THandle) : DWORD; stdcall;
{$ELSE}
function GetLongPathNameTransacted (lpszShortPath, lpszLongPath: LPSTR;
  cchBuffer: DWORD; hTransaction: THandle) : DWORD; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM GetLongPathNameTransacted}

function GetTransactionId (TransactionHandle: THandle;
  out TransactionId: TGUID) : Bool; stdcall;
{$EXTERNALSYM GetTransactionId}

function MoveFileTransactedA (lpExistingFileName, lpNewFileName: LPSTR;
  lpProgressRoutine: TCopyProgressRoutine; lpData: Pointer; dwFlags: DWORD;
  hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM MoveFileTransactedA}
function MoveFileTransactedW (lpExistingFileName, lpNewFileName: LPWSTR;
  lpProgressRoutine: TCopyProgressRoutine; lpData: Pointer; dwFlags: DWORD;
  hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM MoveFileTransactedW}

{$IFDEF UNICODE}
function MoveFileTransacted (lpExistingFileName, lpNewFileName: LPWSTR;
  lpProgressRoutine: TCopyProgressRoutine; lpData: Pointer; dwFlags: DWORD;
  hTransaction: THandle) : Bool; stdcall;
{$ELSE}
function MoveFileTransacted (lpExistingFileName, lpNewFileName: LPSTR;
  lpProgressRoutine: TCopyProgressRoutine; lpData: Pointer; dwFlags: DWORD;
  hTransaction: THandle) : Bool; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM MoveFileTransacted}

function OpenTransaction (dwDesiredAccess: DWORD;
  TransactionId: PGUID) : THandle; stdcall;
{$EXTERNALSYM OpenTransaction}

function RegCreateKeyTransactedA (hKey: HKEY; lpSubKey: LPSTR;
  Reserved: DWORD; lpClass: LPSTR; dwOptions: DWORD; samDesired: REGSAM;
  lpSecurityAttributes: PSecurityAttributes; var phkResult: HKEY;
  lpdwDisposition: PDWORD; hTransaction: THandle;
  pExtendedParemeter: Pointer) : LongInt; stdcall;
{$EXTERNALSYM RegCreateKeyTransactedA}
function RegCreateKeyTransactedW (hKey: HKEY; lpSubKey: LPWSTR;
  Reserved: DWORD; lpClass: LPWSTR; dwOptions: DWORD; samDesired: REGSAM;
  lpSecurityAttributes: PSecurityAttributes; var phkResult: HKEY;
  lpdwDisposition: PDWORD; hTransaction: THandle;
  pExtendedParemeter: Pointer) : LongInt; stdcall;
{$EXTERNALSYM RegCreateKeyTransactedW}

{$IFDEF UNICODE}
function RegCreateKeyTransacted (hKey: HKEY; lpSubKey: LPWSTR;
  Reserved: DWORD; lpClass: LPWSTR; dwOptions: DWORD; samDesired: REGSAM;
  lpSecurityAttributes: PSecurityAttributes; var phkResult: HKEY;
  lpdwDisposition: PDWORD; hTransaction: THandle;
  pExtendedParemeter: Pointer) : LongInt; stdcall;
{$ELSE}
function RegCreateKeyTransacted (hKey: HKEY; lpSubKey: LPSTR;
  Reserved: DWORD; lpClass: LPSTR; dwOptions: DWORD; samDesired: REGSAM;
  lpSecurityAttributes: PSecurityAttributes; var phkResult: HKEY;
  lpdwDisposition: PDWORD; hTransaction: THandle;
  pExtendedParemeter: Pointer) : LongInt; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM RegCreateKeyTransacted}

function RegDeleteKeyTransactedA (hKey: HKEY; lpSubKey: LPSTR;
  samDesired: REGSAM; Reserved: DWORD; hTransaction: THandle;
  pExtendedParameter: Pointer) : LongInt; stdcall;
{$EXTERNALSYM RegDeleteKeyTransactedA}
function RegDeleteKeyTransactedW (hKey: HKEY; lpSubKey: LPWSTR;
  samDesired: REGSAM; Reserved: DWORD; hTransaction: THandle;
  pExtendedParameter: Pointer) : LongInt; stdcall;
{$EXTERNALSYM RegDeleteKeyTransactedW}

{$IFDEF UNICODE}
function RegDeleteKeyTransacted (hKey: HKEY; lpSubKey: LPWSTR;
  samDesired: REGSAM; Reserved: DWORD; hTransaction: THandle;
  pExtendedParameter: Pointer) : LongInt; stdcall;
{$ELSE}
function RegDeleteKeyTransacted (hKey: HKEY; lpSubKey: LPSTR;
  samDesired: REGSAM; Reserved: DWORD; hTransaction: THandle;
  pExtendedParameter: Pointer) : LongInt; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM RegDeleteKeyTransacted}

function RegOpenKeyTransactedA (hKey: HKEY; lpSubKey: LPSTR;
  ulOptions: DWORD; samDesired: REGSAM; var phkResult: HKEY;
  hTransaction: THandle; pExtendedParameter: Pointer) : LongInt; stdcall;
{$EXTERNALSYM RegOpenKeyTransactedA}
function RegOpenKeyTransactedW (hKey: HKEY; lpSubKey: LPWSTR;
  ulOptions: DWORD; samDesired: REGSAM; var phkResult: HKEY;
  hTransaction: THandle; pExtendedParameter: Pointer) : LongInt; stdcall;
{$EXTERNALSYM RegOpenKeyTransactedW}

{$IFDEF UNICODE}
function RegOpenKeyTransacted (hKey: HKEY; lpSubKey: LPWSTR;
  ulOptions: DWORD; samDesired: REGSAM; var phkResult: HKEY;
  hTransaction: THandle; pExtendedParameter: Pointer) : LongInt; stdcall;
{$ELSE}
function RegOpenKeyTransacted (hKey: HKEY; lpSubKey: LPSTR;
  ulOptions: DWORD; samDesired: REGSAM; var phkResult: HKEY;
  hTransaction: THandle; pExtendedParameter: Pointer) : LongInt; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM RegOpenKeyTransacted}

function RemoveDirectoryTransactedA (lpPathName: LPSTR;
  hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM RemoveDirectoryTransactedA}
function RemoveDirectoryTransactedW (lpPathName: LPWSTR;
  hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM RemoveDirectoryTransactedW}

{$IFDEF UNICODE}
function RemoveDirectoryTransacted (lpPathName: LPWSTR;
  hTransaction: THandle) : Bool; stdcall;
{$ELSE}
function RemoveDirectoryTransacted (lpPathName: LPSTR;
  hTransaction: THandle) : Bool; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM RemoveDirectoryTransacted}

function RollbackTransaction (TransactionHandle: THandle) : Bool; stdcall;
{$EXTERNALSYM RollbackTransaction}

function RollbackTransactionAsync (TransactionHandle: THandle) : Bool; stdcall;
{$EXTERNALSYM RollbackTransactionAsync}

function SetFileAttributesTransactedA (lpFileName: LPSTR;
    dwFileAttributes: DWORD; hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM SetFileAttributesTransactedA}
function SetFileAttributesTransactedW (lpFileName: LPWSTR;
    dwFileAttributes: DWORD; hTransaction: THandle) : Bool; stdcall;
{$EXTERNALSYM SetFileAttributesTransactedW}

{$IFDEF UNICODE}
function SetFileAttributesTransacted (lpFileName: LPWSTR;
    dwFileAttributes: DWORD; hTransaction: THandle) : Bool; stdcall;
{$ELSE}
function SetFileAttributesTransacted (lpFileName: LPSTR;
    dwFileAttributes: DWORD; hTransaction: THandle) : Bool; stdcall;
{$ENDIF}  // !UNICODE
{$EXTERNALSYM SetFileAttributesTransacted}

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation

uses JwaWinDllNames;

{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}
//add here implementation stuff

{$IFNDEF JWA_INCLUDEMODE}
const
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _CommitTransaction : Pointer;

function CommitTransaction;
begin
  GetProcedureAddress (_CommitTransaction, KtmW32Lib, 'CommitTransaction');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CommitTransaction]
  end;
end;

var
  _CommitTransactionAsync : Pointer;

function CommitTransactionAsync;
begin
  GetProcedureAddress (_CommitTransactionAsync, KtmW32Lib, 'CommitTransactionAsync');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CommitTransactionAsync]
  end;
end;

var
  _CopyFileTransactedA : Pointer;

function CopyFileTransactedA;
begin
  GetProcedureAddress (_CopyFileTransactedA, kernel32, 'CopyFileTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CopyFileTransactedA]
  end;
end;

var
  _CopyFileTransactedW : Pointer;

function CopyFileTransactedW;
begin
  GetProcedureAddress (_CopyFileTransactedW, kernel32, 'CopyFileTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CopyFileTransactedW]
  end;
end;

var
  _CopyFileTransacted : Pointer;

function CopyFileTransacted;
begin
  GetProcedureAddress (_CopyFileTransacted, kernel32, 'CopyFileTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CopyFileTransacted]
  end;
end;

var
  _CreateDirectoryTransactedA : Pointer;

function CreateDirectoryTransactedA;
begin
  GetProcedureAddress (_CreateDirectoryTransactedA, kernel32, 'CreateDirectoryTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateDirectoryTransactedA]
  end;
end;

var
  _CreateDirectoryTransactedW : Pointer;

function CreateDirectoryTransactedW;
begin
  GetProcedureAddress (_CreateDirectoryTransactedW, kernel32, 'CreateDirectoryTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateDirectoryTransactedW]
  end;
end;

var
  _CreateDirectoryTransacted : Pointer;

function CreateDirectoryTransacted;
begin
  GetProcedureAddress (_CreateDirectoryTransacted, kernel32, 'CreateDirectoryTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateDirectoryTransacted]
  end;
end;

var
  _CreateFileTransactedA : Pointer;

function CreateFileTransactedA;
begin
  GetProcedureAddress (_CreateFileTransactedA, kernel32, 'CreateFileTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateFileTransactedA]
  end;
end;

var
  _CreateFileTransactedW : Pointer;

function CreateFileTransactedW;
begin
  GetProcedureAddress (_CreateFileTransactedW, kernel32, 'CreateFileTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateFileTransactedW]
  end;
end;

var
  _CreateFileTransacted : Pointer;

function CreateFileTransacted;
begin
  GetProcedureAddress (_CreateFileTransacted, kernel32, 'CreateFileTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateFileTransacted]
  end;
end;

var
  _CreateHardLinkTransactedA : Pointer;

function CreateHardLinkTransactedA;
begin
  GetProcedureAddress (_CreateHardLinkTransactedA, kernel32, 'CreateHardLinkTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateHardLinkTransactedA]
  end;
end;

var
  _CreateHardLinkTransactedW : Pointer;

function CreateHardLinkTransactedW;
begin
  GetProcedureAddress (_CreateHardLinkTransactedW, kernel32, 'CreateHardLinkTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateHardLinkTransactedW]
  end;
end;

var
  _CreateHardLinkTransacted : Pointer;

function CreateHardLinkTransacted;
begin
  GetProcedureAddress (_CreateHardLinkTransacted, kernel32, 'CreateHardLinkTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateHardLinkTransacted]
  end;
end;

var
  _CreateSymbolicLinkTransactedA : Pointer;

function CreateSymbolicLinkTransactedA;
begin
  GetProcedureAddress (_CreateSymbolicLinkTransactedA, kernel32, 'CreateSymbolicLinkTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateSymbolicLinkTransactedA]
  end;
end;

var
  _CreateSymbolicLinkTransactedW : Pointer;

function CreateSymbolicLinkTransactedW;
begin
  GetProcedureAddress (_CreateSymbolicLinkTransactedW, kernel32, 'CreateSymbolicLinkTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateSymbolicLinkTransactedW]
  end;
end;

var
  _CreateSymbolicLinkTransacted : Pointer;

function CreateSymbolicLinkTransacted;
begin
  GetProcedureAddress (_CreateSymbolicLinkTransacted, kernel32, 'CreateSymbolicLinkTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateSymbolicLinkTransacted]
  end;
end;

var
  _CreateTransaction : Pointer;

function CreateTransaction;
begin
  GetProcedureAddress (_CreateTransaction, KtmW32Lib, 'CreateTransaction');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CreateTransaction]
  end;
end;

var
  _DeleteFileTransactedA : Pointer;

function DeleteFileTransactedA;
begin
  GetProcedureAddress (_DeleteFileTransactedA, kernel32, 'DeleteFileTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DeleteFileTransactedA]
  end;
end;

var
  _DeleteFileTransactedW : Pointer;

function DeleteFileTransactedW;
begin
  GetProcedureAddress (_DeleteFileTransactedW, kernel32, 'DeleteFileTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DeleteFileTransactedW]
  end;
end;

var
  _DeleteFileTransacted : Pointer;

function DeleteFileTransacted;
begin
  GetProcedureAddress (_DeleteFileTransacted, kernel32, 'DeleteFileTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DeleteFileTransacted]
  end;
end;

var
  _FindFirstFileNameTransactedW : Pointer;

function FindFirstFileNameTransactedW;
begin
  GetProcedureAddress (_FindFirstFileNameTransactedW, kernel32, 'FindFirstFileNameTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindFirstFileNameTransactedW]
  end;
end;

var
  _FindFirstFileTransactedA : Pointer;

function FindFirstFileTransactedA;
begin
  GetProcedureAddress (_FindFirstFileTransactedA, kernel32, 'FindFirstFileTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindFirstFileTransactedA]
  end;
end;

var
  _FindFirstFileTransactedW : Pointer;

function FindFirstFileTransactedW;
begin
  GetProcedureAddress (_FindFirstFileTransactedW, kernel32, 'FindFirstFileTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindFirstFileTransactedW]
  end;
end;

var
  _FindFirstFileTransacted : Pointer;

function FindFirstFileTransacted;
begin
  GetProcedureAddress (_FindFirstFileTransacted, kernel32, 'FindFirstFileTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FindFirstFileTransacted]
  end;
end;

var
  _GetCompressedFileSizeTransactedA : Pointer;

function GetCompressedFileSizeTransactedA;
begin
  GetProcedureAddress (_GetCompressedFileSizeTransactedA, kernel32, 'GetCompressedFileSizeTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetCompressedFileSizeTransactedA]
  end;
end;

var
  _GetCompressedFileSizeTransactedW : Pointer;

function GetCompressedFileSizeTransactedW;
begin
  GetProcedureAddress (_GetCompressedFileSizeTransactedW, kernel32, 'GetCompressedFileSizeTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetCompressedFileSizeTransactedW]
  end;
end;

var
  _GetCompressedFileSizeTransacted : Pointer;

function GetCompressedFileSizeTransacted;
begin
  GetProcedureAddress (_GetCompressedFileSizeTransacted, kernel32, 'GetCompressedFileSizeTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetCompressedFileSizeTransacted]
  end;
end;

var
  _GetFileAttributesTransactedA : Pointer;

function GetFileAttributesTransactedA;
begin
  GetProcedureAddress (_GetFileAttributesTransactedA, kernel32, 'GetFileAttributesTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFileAttributesTransactedA]
  end;
end;

var
  _GetFileAttributesTransactedW : Pointer;

function GetFileAttributesTransactedW;
begin
  GetProcedureAddress (_GetFileAttributesTransactedW, kernel32, 'GetFileAttributesTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFileAttributesTransactedW]
  end;
end;

var
  _GetFileAttributesTransacted : Pointer;

function GetFileAttributesTransacted;
begin
  GetProcedureAddress (_GetFileAttributesTransacted, kernel32, 'GetFileAttributesTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFileAttributesTransacted]
  end;
end;

var
  _GetFullPathNameTransactedA : Pointer;

function GetFullPathNameTransactedA;
begin
  GetProcedureAddress (_GetFullPathNameTransactedA, kernel32, 'GetFullPathNameTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFullPathNameTransactedA]
  end;
end;

var
  _GetFullPathNameTransactedW : Pointer;

function GetFullPathNameTransactedW;
begin
  GetProcedureAddress (_GetFullPathNameTransactedW, kernel32, 'GetFullPathNameTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFullPathNameTransactedW]
  end;
end;

var
  _GetFullPathNameTransacted : Pointer;

function GetFullPathNameTransacted;
begin
  GetProcedureAddress (_GetFullPathNameTransacted, kernel32, 'GetFullPathNameTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFullPathNameTransacted]
  end;
end;

var
  _GetLongPathNameTransactedA : Pointer;

function GetLongPathNameTransactedA;
begin
  GetProcedureAddress (_GetLongPathNameTransactedA, kernel32, 'GetLongPathNameTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetLongPathNameTransactedA]
  end;
end;

var
  _GetLongPathNameTransactedW : Pointer;

function GetLongPathNameTransactedW;
begin
  GetProcedureAddress (_GetLongPathNameTransactedW, kernel32, 'GetLongPathNameTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetLongPathNameTransactedW]
  end;
end;

var
  _GetLongPathNameTransacted : Pointer;

function GetLongPathNameTransacted;
begin
  GetProcedureAddress (_GetLongPathNameTransacted, kernel32, 'GetLongPathNameTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetLongPathNameTransacted]
  end;
end;

var
  _GetTransactionId : Pointer;

function GetTransactionId;
begin
  GetProcedureAddress (_GetTransactionId, KtmW32Lib, 'GetTransactionId');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetTransactionId]
  end;
end;

var
  _MoveFileTransactedA : Pointer;

function MoveFileTransactedA;
begin
  GetProcedureAddress (_MoveFileTransactedA, kernel32, 'MoveFileTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MoveFileTransactedA]
  end;
end;

var
  _MoveFileTransactedW : Pointer;

function MoveFileTransactedW;
begin
  GetProcedureAddress (_MoveFileTransactedW, kernel32, 'MoveFileTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MoveFileTransactedW]
  end;
end;

var
  _MoveFileTransacted : Pointer;

function MoveFileTransacted;
begin
  GetProcedureAddress (_MoveFileTransacted, kernel32, 'MoveFileTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MoveFileTransacted]
  end;
end;

var
  _OpenTransaction : Pointer;

function OpenTransaction;
begin
  GetProcedureAddress (_OpenTransaction, KtmW32Lib, 'OpenTransaction');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_OpenTransaction]
  end;
end;

var
  _RegCreateKeyTransactedA : Pointer;

function RegCreateKeyTransactedA;
begin
  GetProcedureAddress (_RegCreateKeyTransactedA, advapi32, 'RegCreateKeyTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegCreateKeyTransactedA]
  end;
end;

var
  _RegCreateKeyTransactedW : Pointer;

function RegCreateKeyTransactedW;
begin
  GetProcedureAddress (_RegCreateKeyTransactedW, advapi32, 'RegCreateKeyTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegCreateKeyTransactedW]
  end;
end;

var
  _RegCreateKeyTransacted : Pointer;

function RegCreateKeyTransacted;
begin
  GetProcedureAddress (_RegCreateKeyTransacted, advapi32, 'RegCreateKeyTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegCreateKeyTransacted]
  end;
end;

var
  _RegDeleteKeyTransactedA : Pointer;

function RegDeleteKeyTransactedA;
begin
  GetProcedureAddress (_RegDeleteKeyTransactedA, advapi32, 'RegDeleteKeyTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegDeleteKeyTransactedA]
  end;
end;

var
  _RegDeleteKeyTransactedW : Pointer;

function RegDeleteKeyTransactedW;
begin
  GetProcedureAddress (_RegDeleteKeyTransactedW, advapi32, 'RegDeleteKeyTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegDeleteKeyTransactedW]
  end;
end;

var
  _RegDeleteKeyTransacted : Pointer;

function RegDeleteKeyTransacted;
begin
  GetProcedureAddress (_RegDeleteKeyTransacted, advapi32, 'RegDeleteKeyTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegDeleteKeyTransacted]
  end;
end;

var
  _RegOpenKeyTransactedA : Pointer;

function RegOpenKeyTransactedA;
begin
  GetProcedureAddress (_RegOpenKeyTransactedA, advapi32, 'RegOpenKeyTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegOpenKeyTransactedA]
  end;
end;

var
  _RegOpenKeyTransactedW : Pointer;

function RegOpenKeyTransactedW;
begin
  GetProcedureAddress (_RegOpenKeyTransactedW, advapi32, 'RegOpenKeyTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegOpenKeyTransactedW]
  end;
end;

var
  _RegOpenKeyTransacted : Pointer;

function RegOpenKeyTransacted;
begin
  GetProcedureAddress (_RegOpenKeyTransacted, advapi32, 'RegOpenKeyTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RegOpenKeyTransacted]
  end;
end;

var
  _RemoveDirectoryTransactedA : Pointer;

function RemoveDirectoryTransactedA;
begin
  GetProcedureAddress (_RemoveDirectoryTransactedA, kernel32, 'RemoveDirectoryTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RemoveDirectoryTransactedA]
  end;
end;

var
  _RemoveDirectoryTransactedW : Pointer;

function RemoveDirectoryTransactedW;
begin
  GetProcedureAddress (_RemoveDirectoryTransactedW, kernel32, 'RemoveDirectoryTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RemoveDirectoryTransactedW]
  end;
end;

var
  _RemoveDirectoryTransacted : Pointer;

function RemoveDirectoryTransacted;
begin
  GetProcedureAddress (_RemoveDirectoryTransacted, kernel32, 'RemoveDirectoryTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RemoveDirectoryTransacted]
  end;
end;

var
  _RollbackTransaction : Pointer;

function RollbackTransaction;
begin
  GetProcedureAddress (_RollbackTransaction, KtmW32Lib, 'RollbackTransaction');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RollbackTransaction]
  end;
end;

var
  _RollbackTransactionAsync : Pointer;

function RollbackTransactionAsync;
begin
  GetProcedureAddress (_RollbackTransactionAsync, KtmW32Lib, 'RollbackTransactionAsync');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RollbackTransactionAsync]
  end;
end;

var
  _SetFileAttributesTransactedA : Pointer;

function SetFileAttributesTransactedA;
begin
  GetProcedureAddress (_SetFileAttributesTransactedA, kernel32, 'SetFileAttributesTransactedA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetFileAttributesTransactedA]
  end;
end;

var
  _SetFileAttributesTransactedW : Pointer;

function SetFileAttributesTransactedW;
begin
  GetProcedureAddress (_SetFileAttributesTransactedW, kernel32, 'SetFileAttributesTransactedW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetFileAttributesTransactedW]
  end;
end;

var
  _SetFileAttributesTransacted : Pointer;

function SetFileAttributesTransacted;
begin
  GetProcedureAddress (_SetFileAttributesTransacted, kernel32, 'SetFileAttributesTransacted' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetFileAttributesTransacted]
  end;
end;

{$ELSE}

function CommitTransaction; external KtmW32Lib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CommitTransaction';
function CommitTransactionAsync; external KtmW32Lib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CommitTransactionAsync';
function CopyFileTransactedA; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CopyFileTransactedA';
function CopyFileTransactedW; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CopyFileTransactedW';
function CopyFileTransacted; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CopyFileTransacted' + AWSuffix;
function CreateDirectoryTransactedA; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CreateDirectoryTransactedA';
function CreateDirectoryTransactedW; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CreateDirectoryTransactedW';
function CreateDirectoryTransacted; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CreateDirectoryTransacted' + AWSuffix;
function CreateFileTransactedA; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CreateFileTransactedA';
function CreateFileTransactedW; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CreateFileTransactedW';
function CreateFileTransacted; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CreateFileTransacted' + AWSuffix;
function CreateHardLinkTransactedA; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CreateHardLinkTransactedA';
function CreateHardLinkTransactedW; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CreateHardLinkTransactedW';
function CreateHardLinkTransacted; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CreateHardLinkTransacted' + AWSuffix;
function CreateSymbolicLinkTransactedA; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CreateSymbolicLinkTransactedA';
function CreateSymbolicLinkTransactedW; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CreateSymbolicLinkTransactedW';
function CreateSymbolicLinkTransacted; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CreateSymbolicLinkTransacted' + AWSuffix;
function CreateTransaction; external KtmW32Lib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'CreateTransaction';
function DeleteFileTransactedA; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'DeleteFileTransactedA';
function DeleteFileTransactedW; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'DeleteFileTransactedW';
function DeleteFileTransacted; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'DeleteFileTransacted' + AWSuffix;
function FindFirstFileNameTransactedW; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'FindFirstFileNameTransactedW';
function FindFirstFileTransactedA; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'FindFirstFileTransactedA';
function FindFirstFileTransactedW; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'FindFirstFileTransactedW';
function FindFirstFileTransacted; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'FindFirstFileTransacted' + AWSuffix;
function GetCompressedFileSizeTransactedA; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetCompressedFileSizeTransactedA';
function GetCompressedFileSizeTransactedW; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetCompressedFileSizeTransactedW';
function GetCompressedFileSizeTransacted; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetCompressedFileSizeTransacted' + AWSuffix;
function GetFileAttributesTransactedA; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetFileAttributesTransactedA';
function GetFileAttributesTransactedW; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetFileAttributesTransactedW';
function GetFileAttributesTransacted; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetFileAttributesTransacted' + AWSuffix;
function GetFullPathNameTransactedA; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetFullPathNameTransactedA';
function GetFullPathNameTransactedW; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetFullPathNameTransactedW';
function GetFullPathNameTransacted; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetFullPathNameTransacted' + AWSuffix;
function GetLongPathNameTransactedA; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetLongPathNameTransactedA';
function GetLongPathNameTransactedW; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetLongPathNameTransactedW';
function GetLongPathNameTransacted; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetLongPathNameTransacted' + AWSuffix;
function GetTransactionId; external KtmW32Lib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'GetTransactionId';
function MoveFileTransactedA; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'MoveFileTransactedA';
function MoveFileTransactedW; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'MoveFileTransactedW';
function MoveFileTransacted; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'MoveFileTransacted' + AWSuffix;
function OpenTransaction; external KtmW32Lib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'OpenTransaction';
function RegCreateKeyTransactedA; external advapi32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'RegCreateKeyTransactedA';
function RegCreateKeyTransactedW; external advapi32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'RegCreateKeyTransactedW';
function RegCreateKeyTransacted; external advapi32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'RegCreateKeyTransacted' + AWSuffix;
function RegDeleteKeyTransactedA; external advapi32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'RegDeleteKeyTransactedA';
function RegDeleteKeyTransactedW; external advapi32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'RegDeleteKeyTransactedW';
function RegDeleteKeyTransacted; external advapi32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'RegDeleteKeyTransacted' + AWSuffix;
function RegOpenKeyTransactedA; external advapi32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'RegOpenKeyTransactedA';
function RegOpenKeyTransactedW; external advapi32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'RegOpenKeyTransactedW';
function RegOpenKeyTransacted; external advapi32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'RegOpenKeyTransacted' + AWSuffix;
function RemoveDirectoryTransactedA; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'RemoveDirectoryTransactedA';
function RemoveDirectoryTransactedW; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'RemoveDirectoryTransactedW';
function RemoveDirectoryTransacted; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'RemoveDirectoryTransacted' + AWSuffix;
function RollbackTransaction; external KtmW32Lib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'RollbackTransaction';
function RollbackTransactionAsync; external KtmW32Lib {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'RollbackTransactionAsync';
function SetFileAttributesTransactedA; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'SetFileAttributesTransactedA';
function SetFileAttributesTransactedW; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'SetFileAttributesTransactedW';
function SetFileAttributesTransacted; external kernel32 {$IFDEF DELAYED_LOADING}delayed{$ENDIF} name 'SetFileAttributesTransacted' + AWSuffix;

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}

