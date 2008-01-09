{@abstract(This unit provides access to winstation api functions)
@author(Christian Wimmer)
@created(03/23/2007)
@lastmod(09/10/2007)
This unit contains types that are used by the units of the Security Manager Suite


Project JEDI Windows Security Code Library (JWSCL)

The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

The Original Code is JwsclWinStations.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.
}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclWinStations;
{$I Jwscl.inc}
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $


interface

uses SysUtils, Classes, Registry, Contnrs,

  JwsclUtils, JwsclResource,
  jwaWindows, StdCtrls, ComCtrls, ActiveX,
  JwsclTypes, JwsclExceptions, JwsclSid, JwsclAcl, JwsclToken,
  JwsclMapping, JwsclKnownSid, JwsclSecureObjects,
  JwsclVersion, JwsclConstants, JwsclProcess, JwsclDescriptor,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}
type

  TJwSecurityWindowStation  = class;
  TJwSecurityWindowStations = class;

  TJwSecurityWindowStationArray = array of TJwSecurityWindowStation;

  {@Name provides access to window station api.}
  TJwSecurityWindowStation = class(TPersistent)
  private
  protected
    fOldWinStations: TStack;
    fHandle: HWINSTA;
    fUserSID: TJwSecurityId;
    fSD: TJwSecurityDescriptor;

    fDestroyWinSta: boolean;

    function GetDesktopNames: TJwTJwStringArray; virtual;
    function GetName: TJwString; virtual;
    function GetFlags: TUserObjectFlags; virtual;
    procedure SetFlags(const Flags: TUserObjectFlags); virtual;
    function GetObjectType: TJwString; virtual;
    function GetUserSID: TJwSecurityId; virtual;

    constructor Create; overload;

    function GetSD(Info: TJwSecurityInformationFlagSet): TJwSecurityDescriptor;
      virtual;
    procedure PutSD(Info: TJwSecurityInformationFlagSet;
      aSD: TJwSecurityDescriptor); virtual;
  public
     {@Name opens a window station using a name and desired access rights.
      The window station must be of the actual session.
     @param sName defines the name of the window station.
     @param bInherit TBD
     @param cDesiredAccess defines winstation access righs

     @raises EJwsclOpenWindowStationException if the window station could not be opened.
     }
    constructor Open(const sName: TJwString; bInherit: boolean;
      cDesiredAccess: TJwAccessMask);

     {@Name creates a new window station. It exists until all handles to it are closed.
      @param sName
      @param bInherit
      @param cDesiredAccess defines winstation access righs
      @param bCreateOnly(Defines whether the winstation is opened if already exits (FALSE),
                or simply fails if already exists (true))

      @param SecurityDescriptor(Defines which security is set on the new window station.
                It can be nil. In that case everybody has full access)
      @raises EJwsclWinCallFailedException if a call to CreateWindowStation failed.
     }
    constructor Create(const sName: TJwString; bInherit: boolean;
      cDesiredAccess: TJwAccessMask; bCreateOnly: boolean;
      SecurityDescriptor: TJwSecurityDescriptor); overload;

    destructor Destroy; override;

     {@Name changes the actual window station for the process to the given on in the actual instance.
      @raises EJwsclWinCallFailedException if a call to SetProcessWindowStation failed.
     }
    procedure SetWindowStation;

     {@Name reverts to the saved window station from SetWindowStation.
     @raises(EJwsclWinCallFailedException if a call to SetProcessWindowStation failed.)
     }
    procedure RevertWindowStation;

     {@Name contains the handle of the window station. @Name is 0 if
      the window station was not opened or created.
      }
    property Handle: HWINSTA Read fHandle;

     {@Name defines whether a call to CloseHandle is done on instance destroying (true)
      or not.
     }
    property DestroyWinSta: boolean
      Read fDestroyWinSta Write fDestroyWinSta;

     {@Name returns an arrays of desktop names (TJwString)
      This method raises EJwsclWinCallFailedException if a call to EnumDesktops failed.
     }
    property DesktopNames: TJwTJwStringArray Read GetDesktopNames;

     {@Name returns the name of the window station.
      This method raises EJwsclWinCallFailedException if a call to GetUserObjectInformation failed.
     }
    property Name: TJwString Read GetName;

     {@Name sets or gets window station flags.
      This method raises EJwsclWinCallFailedException if a call to SetUserObjectInformation or GetUserObjectInformation failed.}
    property Flags: TUserObjectFlags Read GetFlags Write SetFlags;

     {@Name returns a string of this object ("window station")
     This method raises EJwsclWinCallFailedException if a call to GetUserObjectInformation failed.}
    property ObjectType: TJwString Read GetObjectType;

     {@Name returns the user of this window station.
      This method raises EJwsclWinCallFailedException if a call to GetUserObjectInformation failed.}
    property UserSID: TJwSecurityId Read GetUserSID;

     {@Name sets or gets the security descriptor of the actual window station.
      This property uses a parameter Info to set which information is to be set or get.
       ex. SecurityDescriptor[[sif_XXXX,sif_XXXX]]

      If used for getting the SD the caller is responsible for freeing the instance.

      Getting the security parameter uses @link(TJwSecureGeneralObject.GetSecurityInfo),
      setting the security parameter uses @link(TJwSecureGeneralObject.SetSecurityInfo),
      see the methods for more informations and errors.
     }
    property SecurityDescriptor[Info: TJwSecurityInformationFlagSet]
      : TJwSecurityDescriptor Read GetSD Write PutSD;
  end;


  {@Name provides access to window stations}
  TJwSecurityWindowStations = class(TPersistent)
  private
  protected
    fWinstationList: TJwSecurityWindowStationArray;
    fSessionID:      TJwSessionID;

  public
    { public declarations }
     {@Name creates an instance of @ClassName which can be used
      to get a list of window station instances.
     }
    constructor Create; overload;

    {Not implemented. Do not use!}
    constructor Create(const LogonId: TLuid); overload;

    destructor Destroy; override;


     {@Name returns a list of window station of the actual session.
      @raises EJwsclWinCallFailedException if a call to EnumWindowStationsW failed.}
    class function GetWindowStationNames: TJwTJwStringArray; virtual;
    //property WindowStationNames : TJwTJwStringArray read GetWindowStationNames;

    {@Name returns the SessionID}
    property SessionID: TJwSessionID Read fSessionID;

     {@Name returns a list of window station instances.
      Do not free these instances - they are automatically freed on destruction.
      The window station list is made on creating the @ClassName instance.
     }
    property WindowStations: TJwSecurityWindowStationArray
      Read fWinstationList;
  end;

{$ENDIF SL_IMPLEMENTATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
implementation
uses Dialogs;



{$ENDIF SL_OMIT_SECTIONS}


{$IFNDEF SL_INTERFACE_SECTION}


{ TJwSecurityWindowStation }


{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
{ TJwSecurityWindowStations }


type


  PLogonID = ^TLogonID;
  TLogonID = record
    LogonID : Cardinal; //assumed
    State : Cardinal;
  end;
  TWinStationEnumerateW = function(
                    hServer : THandle;
                    var pEntries : Cardinal;
                    var pLogonId : TLogonID;
                    var pByteCount : Cardinal;
                    var pIndex : Cardinal
                    ) : Cardinal; stdcall;

  TWinStationQueryInformationW = function (hServer : THandle;
              LogonID: ULONG;
              WinStaInfoClass : _WINSTATIONINFOCLASS;
              var WinStaInformation : _WINSTATIONINFORMATIONW;
              WinStationInfoLength : ULONG;
              out ReturnLength : ULONG) : Boolean; stdcall;



var  _WinStationEnumerateW : TWinStationEnumerateW;
     _WinStationQueryInformationW : TWinStationQueryInformationW;

constructor TJwSecurityWindowStations.Create(const LogonId: TLuid);
var ByteCount,
    Index,
    pEntries : Cardinal;
    Logon : TLogonID;

    res : NTSTATUS;

    Module : TModuleLoader;
//    b : Boolean;
    ReturnLength : ULONG;
    WinStaInformation : _WINSTATIONINFORMATIONW;
    WinStationInfoLength : ULONG;
begin
{$IFNDEF DEBUG}
  raise EJwsclNotImplementedException.Create('TJwSecurityWindowStations.Create is not implemented.');
{$ENDIF DEBUG}

  _WinStationEnumerateW := nil;
  Module := TModuleLoader.Create('winsta.dll');
  Module.GetProcedure('WinStationEnumerateW',@_WinStationEnumerateW);
  Module.GetProcedure('WinStationQueryInformationW',@_WinStationQueryInformationW);

  //WARNING!  Does not work!!!!
  try
    pEntries := 0;
    Index := 0;

    SetLastError(0);

    ByteCount := sizeof(Logon);
 //   Logon := 0;

    repeat
      res := _WinStationEnumerateW(
                      0,//hServer : THandle;
                      pEntries,//var pEntries : Cardinal;
                      Logon,//var pLogonId : TLogonID;
                      ByteCount,//var pByteCount : Cardinal;
                      Index//var pIndex : Cardinal
                      );
      FillChar(WinStaInformation, sizeof(WinStaInformation), 0);
      WinStationInfoLength := sizeof(WinStaInformation);
      {b := }_WinStationQueryInformationW(
              0,//hServer : THandle;
              Logon.LogonID,//LogonID: ULONG;
              WinStationInformation ,//WinStaInfoClass : WINSTATIONINFOCLASS;
              WinStaInformation,//WinStaInformation : PWINSTATIONINFORMATION;
              WinStationInfoLength,//WinStationInfoLength : ULONG;
              ReturnLength//out ReturnLength : ULONG)
              );
     Inc(Index);
    until res = 0;

    //BUGBUG: res is always 0
    if res <> 0 then
    begin
      SetLastError(res);  //BUGBUG? correct return value?

     // ShowMessage(IntToStr(res));
       raise EJwsclWinCallFailedException.CreateFmtWinCall(
        RsWinCallFailed,
        'Create',       //sSourceProc
        ClassName,                                //sSourceClass
        RsUNWinStation,                          //sSourceFile
        0,                                           //iSourceLine
        True,                                 //bShowLastError
        '_WinStationQueryInformationW',                  //sWinCall
        ['_WinStationQueryInformationW']);
    end;
   finally
     Module.Free;
   end;

//  until res <> 0;
end;

constructor TJwSecurityWindowStations.Create;
begin
  inherited Create;
  fSessionID := 0;

end;



destructor TJwSecurityWindowStations.Destroy;
begin

  inherited;
end;

function EnumWindowStationProc(lpszWindowStation: PWideChar;
  lParam: LPARAM): boolean; stdcall;
begin
  if lParam <> 0 then
    try
      SetLength(TJwTJwStringArray(Pointer(lParam)^), Length(
        TJwTJwStringArray(Pointer(lParam)^)) + 1);
      TJwTJwStringArray(Pointer(lParam)^)[high(TJwTJwStringArray(Pointer(lParam)^))] :=
        TJwString(lpszWindowStation);
    finally
    end;
  Result := True;
end;


class function TJwSecurityWindowStations.GetWindowStationNames: TJwTJwStringArray;
var
  list: TJwTJwStringArray;
begin
  if not jwaWindows.EnumWindowStationsW(@EnumWindowStationProc,
    LPARAM(@list)) then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'GetWindowStationNames',
      //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'EnumWindowStationsW',                  //sWinCall
      ['EnumWindowStations']);
  //const Args: array of const

  Result := list;
end;

{ TJwSecurityWindowStation }

constructor TJwSecurityWindowStation.Create;
begin
  inherited;
  fUserSid := nil;
  fHandle  := 0;

  DestroyWinSta := True;
  fOldWinStations := TStack.Create;
end;


constructor TJwSecurityWindowStation.Create(const sName: TJwString;
  bInherit: boolean; cDesiredAccess: TJwAccessMask; bCreateOnly: boolean;
  SecurityDescriptor: TJwSecurityDescriptor);
var
  pSD: PSecurityAttributes;
  flags: Cardinal;
begin
  Self.Create;


  flags := 0;
  if bCreateOnly then
    flags := CWF_CREATE_ONLY;

  pSD := nil;
  if Assigned(SecurityDescriptor) then
    pSD := SecurityDescriptor.Create_SA(True);

  fHandle := {$IFDEF UNICODE}CreateWindowStationW{$ELSE}CreateWindowStationA{$ENDIF}(TJwPChar(sName), //LPCTSTR lpwinsta,
    flags,//DWORD dwFlags,
    cDesiredAccess,//ACCESS_MASK dwDesiredAccess,
    LPSECURITY_ATTRIBUTES(
    pSD)//LPSECURITY_ATTRIBUTES lpsa
    );

  if Assigned(SecurityDescriptor) then
    SecurityDescriptor.Free_SA(pSD);

  if fHandle = 0 then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinStationCreateFailed,
      //const sMsg: string;
      'Create',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'Create',                  //sWinCall
      [sName]);
  //const Args: array of const

  DestroyWinSta := True;
end;


destructor TJwSecurityWindowStation.Destroy;
begin
  if fDestroyWinSta then
    CloseWindowStation(Handle);
  fHandle := 0;
  FreeAndNil(fOldWinStations);
  FreeAndNil(fUserSid);
  FreeAndNil(fSD);

  inherited;
end;


function EnumDesktopProc(lpszDesktop: TJwPChar; lParam: Pointer): boolean;
  stdcall;
begin
  if lParam <> nil then
    try
      SetLength(TJwTJwStringArray(Pointer(lParam)^), Length(
        TJwTJwStringArray(Pointer(lParam)^)) + 1);
      TJwTJwStringArray(Pointer(lParam)^)[high(TJwTJwStringArray(Pointer(lParam)^))] :=
        TJwString(lpszDesktop);
    finally
    end;
  Result := True;
end;

function TJwSecurityWindowStation.GetDesktopNames: TJwTJwStringArray;
var
  list: TJwTJwStringArray;
begin
  if not
{$IFDEF UNICODE}
  EnumDesktopsW(
{$ELSE}
  EnumDesktopsA(
{$ENDIF UNICODE}
    fHandle, @EnumDesktopProc, LPARAM(@list)) then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'GetDesktopNames',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'EnumDesktops',                   //sWinCall
      ['EnumDesktops']);
  Result := list;
end;

function TJwSecurityWindowStation.GetFlags: TUserObjectFlags;
var
  len: Cardinal;
begin
  if not GetUserObjectInformation(fHandle,//HANDLE hObj,
    UOI_FLAGS,//int nIndex,
    Pointer(@Result),//PVOID pvInfo,
    sizeof(Result),//DWORD nLength,
    len//LPDWORD lpnLengthNeeded
    ) then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'GetFlags',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'GetUserObjectInformation',                  //sWinCall
      ['GetUserObjectInformation']);                                  //const Args: array of const
end;

function TJwSecurityWindowStation.GetName: TJwString;
var
  len: Cardinal;
begin
{$IFDEF UNICODE}
  GetUserObjectInformationW(
{$ELSE}
  GetUserObjectInformationA(
{$ENDIF UNICODE}
    fHandle,//HANDLE hObj,
    UOI_NAME,//int nIndex,
    nil,//PVOID pvInfo,
    0, //DWORD nLength,
    len//LPDWORD lpnLengthNeeded
    );


  SetLength(Result, len div TJwCharSize );
  FillChar(Result[1], len div TJwCharSize , 0);
  if not
{$IFDEF UNICODE}
  GetUserObjectInformationW(
{$ELSE}
  GetUserObjectInformationA(
{$ENDIF UNICODE}
    fHandle,//HANDLE hObj,
    UOI_NAME,//int nIndex,
    @Result[1],//PVOID pvInfo,
    len,//DWORD nLength,
    len //LPDWORD lpnLengthNeeded
    ) then


    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'GetName',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'GetUserObjectInformation',                  //sWinCall
      ['GetUserObjectInformation']);                                  //const Args: array of const

  SetLength(Result, len div TJwCharSize - 1);
  if Result = '' then;
end;

function TJwSecurityWindowStation.GetObjectType: TJwString;
var
  len: Cardinal;
begin
{$IFDEF UNICODE}
  GetUserObjectInformationW(
{$ELSE}
  GetUserObjectInformationA(
{$ENDIF UNICODE}
    fHandle,//HANDLE hObj,
    UOI_TYPE,//int nIndex,
    nil,//PVOID pvInfo,
    0, //DWORD nLength,
    len//LPDWORD lpnLengthNeeded
    );


  SetLength(Result, len div TJwCharSize );
  FillChar(Result[1], len div TJwCharSize , 0);
  if not
{$IFDEF UNICODE}
  GetUserObjectInformationW(
{$ELSE}
  GetUserObjectInformationA(
{$ENDIF UNICODE}
    fHandle,//HANDLE hObj,
    UOI_TYPE,//int nIndex,
    @Result[1],//PVOID pvInfo,
    len,//DWORD nLength,
    len //LPDWORD lpnLengthNeeded
    ) then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'GetObjectType',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'GetUserObjectInformation',                  //sWinCall
      ['GetUserObjectInformation']);                                  //const Args: array of const

  SetLength(Result, len div TJwCharSize - 1);  
  if Result = '' then;
end;

function TJwSecurityWindowStation.GetSD(Info: TJwSecurityInformationFlagSet)
: TJwSecurityDescriptor;
begin
  FreeAndNil(fSD);
  Result := TJwSecureGeneralObject.GetSecurityInfo(
    Handle,//const aHandle : THandle;
    SE_WINDOW_OBJECT,//const aObjectType : TSeObjectType;
    Info//aSecurityInfo: TJwSecurityInformationFlagSet;
    );
  fSD := Result;
end;



function TJwSecurityWindowStation.GetUserSID: TJwSecurityId;
var
  len: Cardinal;
  apSID: PSID;
begin
  Result := nil;
  len := 0;
  apSID := nil;
  GetUserObjectInformation(fHandle,//HANDLE hObj,
    UOI_USER_SID,//int nIndex,
    nil,//PVOID pvInfo,
    0, //DWORD nLength,
    len//LPDWORD lpnLengthNeeded
    );

  if len = 0 then
    exit;

  GetMem(apSid, len);


  if not GetUserObjectInformation(fHandle,//HANDLE hObj,
    UOI_USER_SID,//int nIndex,
    apSID,//PVOID pvInfo,
    len,//DWORD nLength,
    len //LPDWORD lpnLengthNeeded
    ) then
  begin
    FreeMem(apSid);
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'GetUserSID',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'GetUserObjectInformation',                  //sWinCall
      ['GetUserObjectInformation']);                                  //const Args: array of const
  end;

  FreeAndNil(fUserSID);


  fUserSID := TJwSecurityId.Create(apSid);
  Result := fUserSID;

  FreeMem(apSid);
end;

constructor TJwSecurityWindowStation.Open(const sName: TJwString;
  bInherit: boolean; cDesiredAccess: TJwAccessMask);
begin
  Self.Create;


  fHandle :=
{$IFDEF UNICODE}
   OpenWindowStationW
{$ELSE}
    OpenWindowStationA
{$ENDIF}
    (TJwPChar(sName), bInherit, cDesiredAccess);
  if fHandle = 0 then
    raise EJwsclOpenWindowStationException.CreateFmtWinCall(
      RsWinStationOpenFailed,
      //const sMsg: string;
      'Open',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'OpenWindowStation',                  //sWinCall
      [sName]);
  //const Args: array of const
end;

procedure TJwSecurityWindowStation.PutSD(Info: TJwSecurityInformationFlagSet;
  aSD: TJwSecurityDescriptor);
begin
  FreeAndNil(fSD);
  TJwSecureGeneralObject.SetSecurityInfo(
    Handle,//const aHandle : THandle;
    SE_WINDOW_OBJECT,//const aObjectType : TSeObjectType;
    Info,//aSecurityInfo: TJwSecurityInformationFlagSet;
    aSD//const aSecurityDescriptor: TJwSecurityDescriptor);
    );
end;


procedure TJwSecurityWindowStation.RevertWindowStation;
var
  h: HWINSTA;
begin
  if fOldWinStations.Count = 0 then
    exit;
  h := HWINSTA(fOldWinStations.Peek);

  if not SetProcessWindowStation(h) then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'SetWindowStation',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'RevertWindowStation',                  //sWinCall
      ['SetProcessWindowStation']);

  fOldWinStations.Pop;
end;

procedure TJwSecurityWindowStation.SetFlags(const Flags: TUserObjectFlags);
begin
  if not SetUserObjectInformation(fHandle, UOI_FLAGS, @Flags,
    sizeof(Flags)) then
  begin
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'SetFlags',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'SetUserObjectInformation',                  //sWinCall
      ['SetUserObjectInformation']);                                  //const Args: array of const
  end;
end;

procedure TJwSecurityWindowStation.SetWindowStation;
var
  tempWinst: HWINSTA;
begin
  tempWinst := GetProcessWindowStation;

  if not SetProcessWindowStation(fHandle) then
    raise EJwsclWinCallFailedException.CreateFmtWinCall(
      RsWinCallFailed,
      'SetWindowStation',                               //sSourceProc
      ClassName,                                //sSourceClass
      RsUNWinStation,                          //sSourceFile
      0,                                           //iSourceLine
      True,                                 //bShowLastError
      'SetProcessWindowStation',                  //sWinCall
      ['SetProcessWindowStation']);

  //only save pointer if no error occured
  fOldWinStations.Push(Pointer(tempWinst));
end;

initialization
{$ENDIF SL_OMIT_SECTIONS}
{$IFNDEF SL_INITIALIZATION_SECTION}

{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
finalization
{$ENDIF SL_OMIT_SECTIONS}
{$IFNDEF SL_FINALIZATION_SECTION}

{$ENDIF SL_FINALIZATION_SECTION}



{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}
