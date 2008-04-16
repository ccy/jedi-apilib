{
@abstract(Contains structures to support vista elevation.)
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

The Original Code is JwsclElevation.pas.

WARNING:
Not compilable by FreePascal/Lazarus. Missing TTypedComObjectFactory.
}
{$IFDEF FPC}
{$ERROR Not compilable by FreePascal/Lazarus. Missing TTypedComObjectFactory.}
{$ENDIF FPC}

{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclElevation;
// Last modified: $Date: 2007-09-10 10:00:00 +0100 $
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}

interface
uses ComObj, JwaWindows, ShellApi,JwsclStrings;

type
  {@Name provides a registration for a typed com object.
   It also creates the necessary registry entries.}
  TJwElevationClassFactory = class(TTypedComObjectFactory)
  private
    fResourceId: AnsiString;
    fDisableProcessIsolation : Boolean;
  public
    {@Name registers a com object.
     Every com object that must be elevated must be registered by this constructor.

    @param(ResourceId defines a resource id as a string)
    @param(DisableProcessIsolation defines whether the elevated com process
     should be isolated (true) or not.)}
    constructor Create(
      const ResourceId: AnsiString;
      const DisableProcessIsolation : Boolean;
      const ComServer: TComServerObject;
      const TypedComClass: TTypedComClass;
      const ClassId: TGUID;
      const Instancing: TClassInstancing;
      const ThreadingModel: TThreadingModel = tmApartment
    ); overload;

    {@Name registers a com object.
    Every com object that must be elevated must be registered by this constructor.

    @param(ResourceId defines a delphi resource id.
      This id must be created by "resourcestring". Use @<ResourcestringName> as parameter input)
    @param(DisableProcessIsolation defines whether the elevated com process
     should be isolated (true) or not.)}
    constructor Create(
      const ResourceId: PResStringRec;
      const DisableProcessIsolation : Boolean;
      const ComServer: TComServerObject;
      const TypedComClass: TTypedComClass;
      const ClassId: TGUID;
      const Instancing: TClassInstancing;
      const ThreadingModel: TThreadingModel = tmApartment
    ); overload;

    {@Name registers or unregisters a com library.
     Use "regsvr32.exe <lib>" to register and "regsvr32.exe /u <lib>" to
     unregister a com library.}
{$IFDEF UNIT_TEST}
    class procedure UpdateRegistry(RegisterFactory: Boolean);
{$ELSE}
    procedure UpdateRegistry(RegisterFactory: Boolean); override;
{$ENDIF UNIT_TEST}
  end;

{@Name creates an out of process COM object and returns it in ObjectInterface.
If the current is already elevated, the function just creates an COM object
and returns it; otherwise the elevation dialog will be shown.

The COM class must be registered in a COM type library.
The executable which call this function must contain a manifest that defines
"asInvoker" as the requested execution level.
"<requestedExecutionLevel level="asInvoker"/>"

This function needs CoInitialize to be called.
This function only works on Windows Vista and newer OS versions.

@param(MonikerSequence defines a string that contains information how to use the moniker)
@param(ParentWindowHandle defines the window handle that is used to display the elevation dialog.
 If this parameter is 0 or the window is has not the input the elevation dialog
 will not be shown but a new task is displayed it the taskbar. Otherwise the elevation dialog
 will be shown direclty.)
@param(ClassId defines a guid that describes a registered com object)
@param(IID defines the requested com object to be returned)
@param(ObjectInterface returns the requested and elevated com object)
@return(Returns a COM result code. If the call was successfull the return value is S_OK)
}
function JwCoCreateInstanceAsEx(
  const MonikerSequence : WideString;
  const ParentWindowHandle: HWND;
  const ClassId: TGUID;
  const IID: TGUID;
  out ObjectInterface) : HRESULT;


{@Name creates an out of process COM object with administrator rights
and returns it in ObjectInterface.
If the current is already elevated, the function just creates an COM object
and returns it; otherwise the elevation dialog will be shown.

The COM class must be registered in a COM type library.
The executable which call this function must contain a manifest that defines
"asInvoker" as the requested execution level.
"<requestedExecutionLevel level="asInvoker"/>"

This function needs CoInitialize to be called.
This function only works on Windows Vista and newer OS versions.

@param(ParentWindowHandle defines the window handle that is used to display the elevation dialog.
 If this parameter is 0 or the window is has not the input the elevation dialog
 will not be shown but a new task is displayed it the taskbar. Otherwise the elevation dialog
 will be shown direclty.)
@param(ClassId defines a guid that describes a registered com object)
@param(IID defines the requested com object to be returned)
@param(ObjectInterface returns the requested and elevated com object)
@return(Returns a COM result code. If the call was successfull the return value is S_OK)
}
function JwCoCreateInstanceAsAdmin(
  const ParentWindowHandle: HWND;
  const ClassId: TGUID;
  const IID: TGUID;
  out ObjectInterface
) : HRESULT;


{@Name creates an out of process COM object with highest possible rights
and returns it in ObjectInterface.
If the current is already elevated, the function just creates an COM object
and returns it; otherwise the elevation dialog will be shown.

The COM class must be registered in a COM type library.
The executable which call this function must contain a manifest that defines
"asInvoker" as the requested execution level.
"<requestedExecutionLevel level="asInvoker"/>"

This function needs CoInitialize to be called.
This function only works on Windows Vista and newer OS versions.

@param(ParentWindowHandle defines the window handle that is used to display the elevation dialog.
 If this parameter is 0 or the window is has not the input the elevation dialog
 will not be shown but a new task is displayed it the taskbar. Otherwise the elevation dialog
 will be shown direclty.)
@param(ClassId defines a guid that describes a registered com object)
@param(IID defines the requested com object to be returned)
@param(ObjectInterface returns the requested and elevated com object)
@return(Returns a COM result code. If the call was successfull the return value is S_OK)
}
function JwCoCreateInstanceAsHighest(
  const ParentWindowHandle: HWND;
  const ClassId: TGUID;
  const IID: TGUID;
  out ObjectInterface
) : HRESULT;

{@Name creates an out of process COM class factory object with administrator rights
and returns it in ObjectInterface.
If the current is already elevated, the function just creates an COM object
and returns it; otherwise the elevation dialog will be shown.

The COM class must be registered in a COM type library.
The executable which call this function must contain a manifest that defines
"asInvoker" as the requested execution level.
"<requestedExecutionLevel level="asInvoker"/>"

This function needs CoInitialize to be called.

This function only works on Windows Vista and newer OS versions.

@param(ParentWindowHandle defines the window handle that is used to display the elevation dialog.
 If this parameter is 0 or the window is has not the input the elevation dialog
 will not be shown but a new task is displayed it the taskbar. Otherwise the elevation dialog
 will be shown direclty.)
@param(ClassId defines a guid that describes a registered com object)
@param(IID defines the requested com object to be returned)
@param(ObjectInterface returns the requested and elevated com object)
@return(Returns a COM result code. If the call was successfull the return value is S_OK)
}
function JwCoGetClassFactoyAsAdmin(
  const ParentWindowHandle: HWND;
  const ClassId: TGUID;
  const IID: TGUID;
  out ObjectInterface) : HRESULT;


type {@Name controls execution of JwShellExecute }
     TJwShellExecuteFlag = (
        //does not display GUI elements on errors
        sefNoUi,
        {does not try to elevate if it is not available
         In this case verb "open" is used.
        }
        sefIgnoreElevationIfNotAvailable,
        {On Elevation and a given directory it uses
         a trick to set the correct path for the target application
         This is because ShellExecute does not set given directory
         for the target app.
         This may lead to a command line window in background
        }
        sefFixDirWithRunAs,

        //does not close returned process handle
        sefNoClosehProcess
        );
     TJwShellExecuteFlags = set of TJwShellExecuteFlag;

{@Name runs a process with elevated privileges in Windows Vista.
If the current is already elevated the function simply opens the given
filename. The verb of shellexecute cannot be changed.

This function only works on Windows Vista and newer OS versions.

@return(The return value contains the instance of the newly created app.
The function returns before the new application has started therfore the app
can fail. If ShellExecute determines an error the return value is 0 and
an exception is raised.
)
@raises(EJwsclWinCallFailedException will be raised if a call to ShellExecuteEx failed)
}
function JwShellExecute(const hWnd: HWND; FileName, Parameters,
  Directory: TJwString; ShowCmd: Integer; Flags : TJwShellExecuteFlags = [sefNoClosehProcess]): HANDLE;



const
  E_USER_CANCELED_OPERATION = HRESULT($800704C7);//
  E_CLASS_IS_NOT_SETUP = HRESULT($80080017); 



{$ENDIF SL_IMPLEMENTATION_SECTION}



{$IFNDEF SL_OMIT_SECTIONS}
implementation
uses Registry, SysUtils, ActiveX, Dialogs,
     JwsclTypes,   JwsclExceptions, JwsclSid,     JwsclAcl,
     JwsclVersion, JwsclConstants,  JwsclUtils,
     JwsclToken, JwaVista,
     JwsclDescriptor, JwsclKnownSid, JwsclMapping, JwsclResource;
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

function JwShellExecute(const hWnd: HWND;  FileName, Parameters,
  Directory: TJwString; ShowCmd: Integer; Flags : TJwShellExecuteFlags = [sefNoClosehProcess]): HANDLE;
var
  shExecInfo : {$IFDEF UNICODE}SHELLEXECUTEINFOW{$ELSE}SHELLEXECUTEINFOA{$ENDIF};
  Token : TJwSecurityToken;
  IsElevated : Boolean;
begin
  result := 0;

  SetLastError(0);
  ZeroMemory(@shExecInfo,sizeof(shExecInfo));

  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_QUERY or TOKEN_READ);

  try
    IsElevated := Token.RunElevation <> 0;
  except
    on E : EJwsclSecurityException do
    begin
      //On 2000,XP admin is elevated
      IsElevated := JwCheckAdministratorAccess;
      Token.Free;
      if not (sefIgnoreElevationIfNotAvailable in Flags) then
        raise;
    end;
  end;

  if not IsElevated then
    shExecInfo.lpVerb := TJwPChar(TJwString('runas'))
  else //type mismatch? Recompile whole project: ansicode <-> unicode
    shExecInfo.lpVerb := TJwPChar(TJwString('open'));


  shExecInfo.cbSize := sizeof(SHELLEXECUTEINFO);
  shExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  if sefNoUi in Flags then
    shExecInfo.fMask := shExecInfo.fMask or SEE_MASK_FLAG_NO_UI;
    
  shExecInfo.Wnd := hWnd;


  if (sefFixDirWithRunAs in Flags) and (Length(Directory) > 0) and
    not IsElevated then
  begin
    shExecInfo.lpFile := 'cmd.exe';
    {Shellexecute does not set directory when combined with verb "runas"
    so we call cmd, set the directory and then call the application
    This may lead to a remaining cmd window
    }
    shExecInfo.lpParameters := TJwPChar(TJwString('/C cd /d "'+Directory+'" & '+ FileName + ' '+Parameters));
    //shExecInfo.lpFile := 'cmd';
    //shExecInfo.lpParameters := TJwPChar(TJwString('/c "start /D "'+Directory+'" "'+ FileName + '" '+Parameters)+'"');
    shExecInfo.lpDirectory := nil;
  end
  else
  begin
    shExecInfo.lpFile := TJwPChar(FileName);
    shExecInfo.lpParameters := TJwPChar(Parameters);
    shExecInfo.lpDirectory := TJwPChar(Directory);
  end;
  //shExecInfo.lpDirectory := TJwPChar('"'+Directory+'"');



  shExecInfo.nShow := ShowCmd;
  shExecInfo.hInstApp := NULL;

  SetLastError(0);
  if {$IFDEF UNICODE}ShellExecuteExW{$ELSE}ShellExecuteExA{$ENDIF}(@shExecInfo) then
    result := shExecInfo.hProcess
  else
    raise EJwsclWinCallFailedException.CreateFmtWinCall(RsWinCallFailed,'src','','JwsclElevation.pas',0,
          true,'ShellExecuteEx',['ShellExecuteEx']);

  if (result <> 0) and not (sefNoClosehProcess in Flags) then
  begin
    CloseHandle(result);
    result := 0;
  end;
end;




threadvar ResultValue : HRESULT;

function JwCoCreateInstanceAsEx(
  const MonikerSequence : WideString;
  const ParentWindowHandle: HWND;
  const ClassId: TGUID;
  const IID: TGUID;
  out ObjectInterface) : HRESULT;
var
  MonikerName : WideString;
  BindOptions : TBindOpts3;
  Token : TJwSecurityToken;
  LastError,
  iLen : Cardinal;
begin
  ResultValue := 0;

  Token := TJwSecurityToken.CreateTokenEffective(TOKEN_QUERY or TOKEN_READ);

  try
    if Token.RunElevation = 0 then
    begin
      MonikerName := MonikerSequence + GUIDToString(ClassId);

      iLen := SizeOf(TBindOpts3);
      FillChar(BindOptions, iLen, 0);

      BindOptions.cbStruct := iLen;
      BindOptions.dwClassContext := CLSCTX_LOCAL_SERVER;
      BindOptions.hwnd := ParentWindowHandle;

      result := CoGetObject(PWideChar(MonikerName), @BindOptions, IID, ObjectInterface);
      if result = E_USER_CANCELED_OPERATION then
        ResultValue := ERROR_CANCELLED
      else
      if result = E_CLASS_IS_NOT_SETUP then
        ResultValue := E_CLASS_IS_NOT_SETUP//ERROR_INVALID_ACCESS
      else
        //ResultValue := ERROR_ACCESS_DENIED;
        ResultValue := result;
    end
    else
    begin
      result := CoCreateInstance(ClassID, nil, CLSCTX_ALL, IID, ObjectInterface);
    end;
  finally
    Token.Free;
  end;
end;

function JwCoCreateInstanceAsAdmin(
  const ParentWindowHandle: HWND;           
  const ClassId: TGUID;
  const IID: TGUID;
 out ObjectInterface) : HRESULT;
var iLen : Cardinal;
begin
  result := JwCoCreateInstanceAsEx(
    'Elevation:Administrator!new:', ParentWindowHandle, ClassId, IID, ObjectInterface);
  SetLastError(ResultValue);
end;


function JwCoCreateInstanceAsHighest(
  const ParentWindowHandle: HWND;
  const ClassId: TGUID;
  const IID: TGUID;
  out ObjectInterface) : HRESULT;
begin
  result := JwCoCreateInstanceAsEx(
    'Elevation:Highest!new:', ParentWindowHandle, ClassId, IID, ObjectInterface);
  SetLastError(ResultValue);
end;

function JwCoGetClassFactoyAsAdmin(
  const ParentWindowHandle: HWND;
  const ClassId: TGUID;
  const IID: TGUID;
  out ObjectInterface) : HRESULT;
begin
  result := JwCoCreateInstanceAsEx(
    'Elevation:Administrator!clsid:', ParentWindowHandle, ClassId, IID, ObjectInterface);
  SetLastError(ResultValue);
end;


{ TJwElevationClassFactory }

constructor TJwElevationClassFactory.Create(const ResourceId: AnsiString;
  const DisableProcessIsolation : Boolean;
  const ComServer: TComServerObject; const TypedComClass: TTypedComClass;
  const ClassId: TGUID; const Instancing: TClassInstancing;
  const ThreadingModel: TThreadingModel);
begin
  inherited Create(ComServer, TypedComClass, ClassId, Instancing, ThreadingModel);
  fResourceId := ResourceId;
  fDisableProcessIsolation := DisableProcessIsolation;
end;

constructor TJwElevationClassFactory.Create(const ResourceId: PResStringRec;
  const DisableProcessIsolation : Boolean;
  const ComServer: TComServerObject; const TypedComClass: TTypedComClass;
  const ClassId: TGUID; const Instancing: TClassInstancing;
  const ThreadingModel: TThreadingModel);
begin
  Create(AnsiString(IntToStr(GetResourceStringIdentifier(ResourceId))),
    DisableProcessIsolation, ComServer, TypedComClass, ClassId, Instancing, ThreadingModel);
end;

(*
HKEY_LOCAL_MACHINE\SOFTWARE\Classes
{
  CLSID\{6BCFB187-C1DD-4807-96AD-F91AB4AB08AC}
  {
    (Default):       REG_SZ = 'MyPrivilegedObject'
    AppID:           REG_SZ = '{6BCFB187-C1DD-4807-96AD-F91AB4AB08AC}'
    LocalizedString: REG_SZ = '@C:\Your\Path\Here\PrivilegedLib.dll,-101'

    Elevation
    {
      (Default): REG_SZ    = null
      Enabled:   REG_DWORD = 1
    }
  }
}
*)


{$IFDEF UNIT_TEST}
class procedure TJwElevationClassFactory.UpdateRegistry(RegisterFactory: Boolean);
{$ELSE}
procedure TJwElevationClassFactory.UpdateRegistry(RegisterFactory: Boolean);

  procedure RaiseRegError(Reason, Key : AnsiString);
  begin
    try
      raise EJwsclAccessDenied.CreateFmtEx(
        Reason,
        'UpdateRegistry', ClassName, RsUNElevation, 0, false,
        [Key]);
    except
      on E : Exception do
      begin
        if Self.ShowErrors then
          ShowMessage(E.Message)
        else
          raise;
      end;
    end;
  end;
{$ENDIF UNIT_TEST}
var
  Reg : TRegistry;
  GuidString,
  DllPath,
  DllName : AnsiString;

  SD : TJwSecurityDescriptor;
  pSecDescr : PSecurityDescriptor;
  SecSize : Cardinal;
  RootKey : HKEY;


const
  ClsIdKey = 'CLSID\';
  AppIdKey = 'AppID\';
  ClassesKey = 'SOFTWARE\Classes\';
  DllSurrogateValue = 'DllSurrogate';
  ElevationKey = '\Elevation';
  AppIdValue = 'AppID';
  AccessPermissionValue = 'AccessPermission';
  LocalizedStringValue = 'LocalizedString';
  EnabledValue = 'Enabled';
  DisableProcessIsolationValue = 'DisableProcessIsolation';

{$IFDEF UNIT_TEST}
  Description = 'mydescr';
  fResourceId = '123';
  fDisableProcessIsolation = false;
{$ENDIF UNIT_TEST}

begin
  //JwInitWellKnownSIDs;

{$IFDEF UNIT_TEST}
  DllPath := 'c:\programme\mydll.dll';
  DllName := ExtractFileName(DllPath);
  GuidString := '{E108B186-B399-4E46-99B4-345F8179C26E}';
  RootKey := HKEY_CURRENT_USER;
{$ELSE}
  DllPath := ComServer.ServerFileName;
  DllName := ExtractFileName(DllPath);
  GuidString := GUIDToString(Self.ClassId);
  RootKey := HKEY_LOCAL_MACHINE;
{$ENDIF UNIT_TEST}

  if RegisterFactory then
  begin
    (*
    HKEY_LOCAL_MACHINE\SOFTWARE\Classes
    {
    AppID\xxxxxxxxxxxxx.dll
    {
     (Default): REG_SZ = null
     AppID:     REG_SZ = '{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}'
    }
    *)

    Reg := TRegistry.Create(KEY_SET_VALUE or KEY_WRITE);
    Reg.RootKey := RootKey;
    try
      if Reg.OpenKey(ClassesKey+AppIdKey+DllName,true) then
        Reg.WriteString(AppIdValue, GuidString)
      else
        RaiseRegError(RsElevationRegCreateError, ClassesKey+AppIdKey+DllName);
    finally
      Reg.Free;
    end;

    (*
    HKEY_LOCAL_MACHINE\SOFTWARE\Classes
    {
    AppID\{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}
    {
      (Default):        REG_SZ     = <Description>
      AccessPermission: REG_BINARY = <BINARY VALUE>
      DllSurrogate:     REG_SZ     = ''
    }
    *)

    Reg := TRegistry.Create(KEY_SET_VALUE or KEY_WRITE);
    Reg.RootKey := RootKey;
    try
      if Reg.OpenKey(ClassesKey+AppIdKey+GuidString,true) then
      begin
        Reg.WriteString('', Description);
        Reg.WriteString(DllSurrogateValue, '');

        { Request local call permissions for InteractiveUser and System. }
        SD := TJwSecurityDescriptor.Create('O:BAG:BAD:(A;;0x3;;;IU)(A;;0x3;;;SY)');

        try
         { SD.OwnOwner := false;
          SD.Owner := JwAdministratorsSID;
          SD.OwnPrimaryGroup := false;
          SD.Owner := JwAdministratorsSID;


          SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create
            (nil,[],KEY_QUERY_VALUE or KEY_SET_VALUE, JwLocalSystemSID, false));
          SD.DACL.Add(TJwDiscretionaryAccessControlEntryAllow.Create
            (nil,[],KEY_QUERY_VALUE or KEY_SET_VALUE, TJwSecurityId.Create('S-1-5-4')));
                                             }
          pSecDescr := SD.Create_SD(SecSize, true);
          try
            Reg.WriteBinaryData(AccessPermissionValue, pSecDescr^, SecSize);
          finally
            SD.Free_SD(pSecDescr);
          end;

        finally
          SD.Free;
        end;

      end
      else
        RaiseRegError(RsElevationRegCreateError, ClassesKey+AppIdKey+GuidString);
    finally
      Reg.Free;
    end;

    (*
    HKEY_LOCAL_MACHINE\SOFTWARE\Classes
{
  CLSID\{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}
  {
    (Default):       REG_SZ = <Description>
    AppID:           REG_SZ = '{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}'
    LocalizedString: REG_SZ = '@<dllpath>,-<resourceid>'


  }
    *)
    Reg := TRegistry.Create(KEY_SET_VALUE or KEY_WRITE);
    Reg.RootKey := RootKey;
    try
      if Reg.OpenKey(ClassesKey+ClsIdKey+GuidString,true) then
      begin
        Reg.WriteString('', Description);
        Reg.WriteString(AppIdValue, GuidString);
        Reg.WriteString(LocalizedStringValue, Format('@%s,-%s',[DllPath,fResourceId]));

        Reg.WriteBool(DisableProcessIsolationValue, fDisableProcessIsolation);
      end
      else
        RaiseRegError(RsElevationRegCreateError, ClassesKey+ClsIdKey+GuidString);
    finally
      Reg.Free;
    end;

    (*
    HKEY_LOCAL_MACHINE\SOFTWARE\Classes\CLSID\{xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx}\Elevation
    {
      (Default): REG_SZ    = null
      Enabled:   REG_DWORD = 1
    }
    *)
    Reg := TRegistry.Create(KEY_SET_VALUE or KEY_WRITE);
    Reg.RootKey := RootKey;
    try
      if Reg.OpenKey(ClassesKey+ClsIdKey+GuidString+ElevationKey,true) then
        Reg.WriteInteger(EnabledValue, 1)
      else
        RaiseRegError(RsElevationRegCreateError, ClassesKey+ClsIdKey+GuidString+ElevationKey);
    finally
      Reg.Free;
    end;

{$IFNDEF UNIT_TEST}
    inherited UpdateRegistry(RegisterFactory);
{$ENDIF UNIT_TEST}

  end
  else
  begin
    Reg := TRegistry.Create(KEY_SET_VALUE or KEY_WRITE or DELETE);
    Reg.RootKey := RootKey;
    try
      if Reg.KeyExists(ClassesKey+AppIdKey+DllName) then
        if not Reg.DeleteKey(ClassesKey+AppIdKey+DllName) then;
          //RaiseRegError(RsElevationRegDeleteError, ClassesKey+AppIdKey+DllName);

      if Reg.KeyExists(ClassesKey+AppIdKey+GuidString) then
        if not Reg.DeleteKey(ClassesKey+AppIdKey+GuidString) then;
          //RaiseRegError(RsElevationRegDeleteError, ClassesKey+AppIdKey+GuidString);

      if Reg.KeyExists(ClassesKey+ClsIdKey+GuidString+ElevationKey) then
        if not Reg.DeleteKey(ClassesKey+ClsIdKey+GuidString+ElevationKey) then;
          //RaiseRegError(RsElevationRegDeleteError, ClassesKey+ClsIdKey+GuidString+ElevationKey);

      if Reg.KeyExists(ClassesKey+ClsIdKey+GuidString) then
        if not Reg.DeleteKey(ClassesKey+ClsIdKey+GuidString) then;
          //RaiseRegError(RsElevationRegDeleteError, ClassesKey+ClsIdKey+GuidString);
    finally
      Reg.Free;
    end;

    inherited UpdateRegistry(RegisterFactory);
  end;
end;

{$ENDIF SL_INTERFACE_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}

initialization
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INITIALIZATION_SECTION}
{$ENDIF SL_INITIALIZATION_SECTION}

{$IFNDEF SL_OMIT_SECTIONS}
end.
{$ENDIF SL_OMIT_SECTIONS}
