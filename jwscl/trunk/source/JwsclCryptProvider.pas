{@abstract(Provides access to cryptgraphic service providers (CSPs) and objects depending on them.)
@author(Philip Dittmann)
@created(11/18/2007)
@lastmod(11/18/2007)

Project JEDI Windows Security Code Library (JWSCL)

The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
you may not use this file except in compliance with the License. You may obtain a copy of the
License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
ANY KIND, either express or implied. See the License for the specific language governing rights
and limitations under the License.

The Initial Developer of the Original Code is Philip Dittmann.
Portions created by Philip Dittmann are Copyright (C) Philip Dittmann. All rights reserved.

}
{$IFNDEF SL_OMIT_SECTIONS}
unit JwsclCryptProvider;
//do not move header comment from above unit declaration!
{$INCLUDE Jwscl.inc}


interface


uses
  SysUtils, Contnrs, Classes,
  jwaWindows,
  JwsclTypes, JwsclExceptions, JwsclResource,
  JwsclStrings; //JwsclStrings, must be at the end of uses list!!!

{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_IMPLEMENTATION_SECTION}

type
  {@abstract(Provides access to cryptographic service providers)}
  TJwCryptProvider = class
  protected
    //@exclude
    fCSPHandle: TJwCSPHandle;
    //@exclude
    class procedure RaiseApiError(const Procname, WinCallname: TJwString);
  public
    {@Name retrieves a handle to the specified CSP using CryptAcquireContext.
     @param(KeyContainerName The name of the key container to be used
            for subsequent operations. Can be @nil to specify the default name.
            Must be @nil if Flags contains ccfVerifyContext.)
     @param(CSPName The name of the CSP)
     @param(Flags Special flags to use with the call to CryptAcquireContext)}
    constructor Create(const KeyContainerName, CSPName: TJwString;
        CSPType: TJwCSPType; Flags: TJwCSPCreationFlagSet); overload;
    {@Name clones an existing CSP and increments its reference count.
     @param(OldCSP The CSP which shall be cloned)}
    constructor Create(const OldCSP: TJwCryptProvider); overload;
    {@Name releases the handle to the CSP.}
    destructor Destroy; override;
    {@Name deletes the specified keyset using CryptAcquireContext with
     the flag CRYPT_DELETEKEYSET.
     @param(KeysetName The name of the keyset to delete.)}
    class procedure DeleteKeyset(const KeysetName: TJwString);
    {@Name obtains the name of the default CSP of the specified type.
     @param(ProviderType The type of CSP for which the default
            is to retrieve)
     @param(MachineDefault If true, the machine default provider is returned.
            Otherwise, the user default is returned.)}
    class function GetDefaultProvider(const ProviderType: TJwCSPType;
      MachineDefault: Boolean): TJwString;

    {@Name is the handle to the CSP.}
    property CSPHandle: Cardinal read fCSPHandle;
  end;

  {@Name provides the possibility to compute hashes with keyless
   algorithms. These hashes cannot be signed since one would need
   a public/private key pair for that.
   @abstract(A class to compute hashes without keys)}
  TJwKeylessHash = class
  protected
    //@exclude
    fHashHandle: TJwHashHandle;
    //@exclude
    fProvider:   TJwCryptProvider;
    //@exclude
    class procedure RaiseApiError(const Procname, WinCallname: TJwString);
    //@exclude
    function GetHashParam(dwParam: Cardinal; pbData: PByte; var pdwDataLen: Cardinal): BOOL;
    //@exclude
    function GetAlgorithm: TJwKeylessHashAlgorithm;
  public
    {Creates a new hash object
     @param(Alg Specifies the algorithm to use)
     @param(CSP The provider the hash uses. If it is @nil,
            the default CSP of type ctDss is used.
            The CSP can be freed during lifetime of the hash
            since the hash increments the reference count of the CSP.)}
    constructor Create(const Alg: TJwKeylessHashAlgorithm; const CSP: TJwCryptProvider = nil);
    {Destroys the hash object and releases the CSP}
    destructor Destroy; override;
    {Adds data to the hash object.
     @param(Data Specifies the data to be added)
     @param(Size Specifies the size of the data)}
    procedure HashData(const Data; const Size: Cardinal);
    {@Name returns the size of the hash value in bytes.
     This value is constant for each algorithm.}
    function GetHashLength: Cardinal;
    {@Name computes the hash of the data previously added to
     the hash using @link(HashData). After a successful call
     to this function you cannot add more data to the hash.
     All additional calls to HashData will fail.
     @param(Hash Specifies the storage in which to put the
            computed hash value)
     @param(Len Specifies the size of Hash. The procedure will
            raise an exception if Len is not big enough.)}
    procedure RetrieveHash(var Hash; var Len: Cardinal);
    {The algorithm specified in the call to @link(create)}
    property Algorithm: TJwKeylessHashAlgorithm read GetAlgorithm;
    {@Name is the handle to the hash object.}
    property HashHandle: Cardinal read fHashHandle;
  end;


{$ENDIF SL_IMPLEMENTATION_SECTION}


{$IFNDEF SL_OMIT_SECTIONS}
implementation
uses JwsclEnumerations;
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

{ TJwCryptProvider }

constructor TJwCryptProvider.Create(const KeyContainerName, CSPName: TJwString; CSPType: TJwCSPType; Flags: TJwCSPCreationFlagSet);
begin
  inherited Create;
  if not CryptAcquireContext(fCSPHandle, TJwPChar(KeyContainerName), TJwPChar(CSPName), TJwEnumMap.ConvertCSPType(CSPType), TJwEnumMap.ConvertCSPCreationFlags(Flags)) then
    RaiseApiError('Create', 'CryptAcquireContext');
end;

constructor TJwCryptProvider.Create(const OldCSP: TJwCryptProvider);
begin
  inherited Create;
  fCSPHandle := OldCSP.CSPHandle;
  if not CryptContextAddRef(OldCSP.CSPHandle, nil, 0) then
    RaiseApiError('Create', 'CryptCOntextAddRef');
end;

destructor TJwCryptProvider.Destroy;
begin
  if not CryptReleaseContext(fCSPHandle, 0) then
    RaiseApiError('Destroy', 'CryptReleaseContext');
  inherited;
end;

class procedure TJwCryptProvider.DeleteKeyset(const KeysetName: TJwString);
var DummyHandle: Cardinal;
begin
  if not CryptAcquireContext(DummyHandle, TJwPChar(KeysetName), nil, PROV_RSA_FULL, CRYPT_DELETEKEYSET) then
    RaiseApiError('DeleteKeyset', 'CryptAcquireContext');
end;

class procedure TJwCryptProvider.RaiseApiError(const Procname, WinCallname: TJwString);
begin
  raise EJwsclCSPApiException.CreateFmtWinCall(
    '',
    Procname,
    ClassName,
    RsUNCryptProvider,
    0,
    True,
    WinCallname,
    [Procname]);
end;

class function TJwCryptProvider.GetDefaultProvider(const ProviderType: TJwCSPType; MachineDefault: Boolean): TJwString;
var Len: Cardinal; Flag: Cardinal;
begin
  Len := 0;
  if MachineDefault then
    Flag := CRYPT_MACHINE_DEFAULT
  else
    Flag := CRYPT_USER_DEFAULT;
  if not CryptGetDefaultProvider(TJwEnumMap.ConvertCSPType(ProviderType), nil, Flag, nil, Len) then
    RaiseApiError('GetDefaultProvider', 'CryptGetDefaultProvider');
  SetLength(Result, Len);
  if not CryptGetDefaultProvider(TJwEnumMap.ConvertCSPType(ProviderType), nil, Flag, TJwPChar(Result), Len) then
  begin
    Result := '';
    RaiseApiError('GetDefaultProvider', 'CryptGetDefaultProvider');
  end;

end;

{ TJwKeylessHash }

constructor TJwKeylessHash.Create(const Alg: TJwKeylessHashAlgorithm; const CSP: TJwCryptProvider = nil);
begin
  inherited Create;
  if CSP = nil then
    fProvider := TJwCryptProvider.Create('', '', ctDss, [ccfVerifyContext])
  else
    fProvider := TJwCryptProvider.Create(CSP);
  if not CryptCreateHash(fProvider.CSPHandle, TJwEnumMap.ConvertKeylessHashAlgorithm(Alg), 0, 0, fHashHandle) then
    RaiseApiError('Create', 'CryptCreateHash');
end;

destructor TJwKeylessHash.Destroy;
begin
  if not CryptDestroyHash(fHashHandle) then
    RaiseApiError('Destroy', 'CryptDestroyHash');
  fProvider.Free;
  inherited;
end;

class procedure TJwKeylessHash.RaiseApiError(const Procname, WinCallname: TJwString);
begin
  raise EJwsclHashApiException.CreateFmtWinCall(
    '',
    ProcName,
    ClassName,
    RsUNCryptProvider,
    0,
    True,
    WinCallName,
    [ProcName]);
end;

function TJwKeylessHash.GetHashParam(dwParam: Cardinal; pbData: PByte; var pdwDataLen: Cardinal): BOOL;
begin
  Result := CryptGetHashParam(fHashHandle, dwParam, pbData, pdwDataLen, 0);
end;

function TJwKeylessHash.GetAlgorithm: TJwKeylessHashAlgorithm;
var Alg: ALG_ID; Size: Cardinal;
begin
  Size := SizeOf(Alg);
  if not GetHashParam(HP_ALGID, @Alg, Size) then
    RaiseApiError('GetAlgorithm', 'CryptGetHashParam');;
  Result := TJwEnumMap.ConvertKeylessHashAlgorithm(Alg);
end;

procedure TJwKeylessHash.HashData(const Data; const Size: Cardinal);
begin
  if not CryptHashData(fHashHandle, @Data, Size, 0) then
    RaiseApiError('HashData', 'CryptHashData');
end;

function TJwKeylessHash.GetHashLength: Cardinal;
var Size: Cardinal;
begin
  Size := SizeOf(Result);
  if not GetHashParam(HP_HASHSIZE, @Result, Size) then
    RaiseApiError('GetHashLength', 'CryptGetHashParam');
end;

procedure TJwKeylessHash.RetrieveHash(var Hash; var Len: Cardinal);
begin
  if not GetHashParam(HP_HASHVAL, @Hash, Len) then
    RaiseApiError('RetrieveHash', 'CryptGetHashParam');
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
