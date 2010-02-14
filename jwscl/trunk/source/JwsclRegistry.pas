{
Description
Project JEDI Windows Security Code Library (JWSCL)

This unit hosts utilty functions.

Author
Christian Wimmer

License
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

Note

The Original Code is JwsclUtils.pas.

The Initial Developer of the Original Code is Christian Wimmer.
Portions created by Christian Wimmer are Copyright (C) Christian Wimmer. All rights reserved.

Todo

1. Extend TJwRegistry to support security.

}
unit JwsclRegistry;
{$INCLUDE ..\includes\Jwscl.inc}

//Check for FastMM4
{$IFDEF FASTMM4}
  //and also activate debug mode (leak detection for Local-/GlobalAlloc)
  {$DEFINE FullDebugMode}
{$ENDIF FASTMM4}
{.$UNDEF FullDebugMode}

//check for Eurekalog
{$IFDEF EUREKALOG}
  {$DEFINE FullDebugMode}
{to see if this memory manager catches Local/Global leaks}
  {.$UNDEF FASTMM4}
  {.$UNDEF MEMCHECK}
  {.$UNDEF FullDebugMode}
{$ENDIF EUREKALOG}

interface

uses
  Classes,
  Registry,
  JwaWindows,
{$IFDEF JCL}
  JclWideFormat,
  JclWideStrings,
{$ENDIF}
  JwsclTypes,
  JwsclExceptions,
  JwsclResource,
  JwsclDescriptor,
  JwsclStrings;


type
{TJwRegistry extends VCL TRegistry to support extended Registry support like
   * Loading the registry hive of an impersonated user

  }
  TJwRegistry = class(TRegistry)
  private
    fSecurityInformationFlags: TJwSecurityInformationFlagSet;
    function GetSD(KeyName: String): TJwSecurityDescriptor;
  protected
    fSD : TJwSecurityDescriptor;
    fLastKeyName : TJwString;

    function OpenCurrentUser(var Key: String) : Boolean;

  public
    constructor Create; overload;
    {OpenCurrentUserKey opens the registry key of the currently impersonated user.

    Remarks
      If the current thread is not impersonated the function behaves like TRegistry.OpenKey
    }
    function OpenCurrentUserKey(const Key: string; CanCreate: Boolean): Boolean;

    {OpenCurrentUserKeyReadOnly opens the registry key of the currently impersonated user readonly.

    Remarks
      If the current thread is not impersonated the function behaves like TRegistry.OpenCurrentUserKeyReadOnly
    }
    function OpenCurrentUserKeyReadOnly(const Key: String): Boolean;

    //Reads a self relative SD from registry
    function ReadBinarySecurityDescriptor(const KeyName : TJwString; var Default : TJwSecurityDescriptor) : TJwSecurityDescriptor; virtual;

    //Writes a self relative SD to registry
    procedure WriteBinarySecurityDescriptor(const KeyName : TJwString; const SD : TJwSecurityDescriptor); virtual;

    property SecurityInformationFlags : TJwSecurityInformationFlagSet read fSecurityInformationFlags write fSecurityInformationFlags;
    property SecurityDescriptor[KeyName : TJwString] : TJwSecurityDescriptor read GetSD;
  end;



implementation
uses SysUtils, Math, D5Impl, JwsclToken, JwsclKnownSid,  JwsclAcl,
     JwsclSecureObjects, JwsclMapping, JwsclStreams, JwsclCryptProvider,
     JwsclConstants
{$IFDEF JW_TYPEINFO}
     ,TypInfo
{$ENDIF JW_TYPEINFO}
      ;


{ TJwRegistry }

constructor TJwRegistry.Create;
begin
  inherited;
  fSecurityInformationFlags := [siOwnerSecurityInformation, siGroupSecurityInformation, siDaclSecurityInformation];
end;

function TJwRegistry.GetSD(KeyName: String): TJwSecurityDescriptor;
begin
  if (fLastKeyName <> '') and Assigned(fSD) and
    JwCompareString(KeyName, fLastKeyName, true) then
  begin
    Result := fSD;
  end
  else
  begin
    TJwSecureRegistryKey.GetSecurityDescriptorEx(CurrentP  ath, fSecurityInformationFlags, false)
  end;
end;

function TJwRegistry.OpenCurrentUser(var Key: String): Boolean;
  function IsRelative(const Value: string): Boolean;
  begin
    Result := not ((Value <> '') and (Value[1] = '\'));
  end;
var
  Relative: Boolean;
  TempKey : HKEY;
begin
  result := true;

  if TJwSecurityToken.HasThreadAToken then
  begin
    result := RegOpenCurrentUser(Access, TempKey) = ERROR_SUCCESS;

    if result then
    begin
      CloseKey;
      SetCurrentKey(TempKey);
    end;

    Relative := IsRelative(Key);
    if not Relative then
      System.Delete(Key, 1, 1);
  end;
end;

function TJwRegistry.OpenCurrentUserKey(const Key: string; CanCreate: Boolean): Boolean;
var S : String;
begin
  S := Key;
  result := OpenCurrentUser(S);
  result := result and OpenKey(S, CanCreate);
end;

function TJwRegistry.OpenCurrentUserKeyReadOnly(const Key: String): Boolean;
var S : String;
begin
  S := Key;
  result := OpenCurrentUser(S);
  result := result and OpenKeyReadOnly(S);
end;

function TJwRegistry.ReadBinarySecurityDescriptor(const KeyName: TJwString; var Default: TJwSecurityDescriptor): TJwSecurityDescriptor;
var
  Size : Integer;
  Buf  : PSecurityDescriptor;
begin
  Size := Self.GetDataSize(KeyName);

  if (Size < 0) or (not Self.ValueExists(KeyName)) then
  begin
    raise ERegistryException.CreateFmt('Key %s\%s not found or value has zero size',[Self.CurrentPath, KeyName]);
  end;

  GetMem(Buf, Size);
  try
    if (Self.ReadBinaryData(KeyName, Buf^, Size) = Size) and
      IsValidSecurityDescriptor(Buf) then
        Default := TJwSecurityDescriptor.Create(Buf);
  finally
    FreeMem(Buf);
  end;

  result := Default;
end;

procedure TJwRegistry.WriteBinarySecurityDescriptor(const KeyName: TJwString; const SD: TJwSecurityDescriptor);
var
  Size : Cardinal;
  PSD : PSecurityDescriptor;
begin
  JwRaiseOnNilParameter(SD, 'SD', 'WriteBinarySecurityDescriptor', ClassName, 'JwsclRegistry.pas');

  PSD := SD.Create_SD(Size, true);
  try
    Self.WriteBinaryData(KeyName, PSD^, Size);
  finally
    SD.Free_SD(PSD);
  end;
end;

end.
