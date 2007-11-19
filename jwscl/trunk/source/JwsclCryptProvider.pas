{@abstract(Provides access to cryptgraphic service providers (CSPs) and objects depending on them.)
@author(Philip Dittmann)
@created(11/18/2007)
@lastmod(11/19/2007)

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
     @param(Flags Special flags to use with the call to CryptAcquireContext)
     @raises EJwsclCSPApiException will be raised if the underlying Windows call fails.}
    constructor Create(const KeyContainerName, CSPName: TJwString;
        CSPType: TJwCSPType; Flags: TJwCSPCreationFlagSet); overload;
    {@Name clones an existing CSP and increments its reference count.
     @param(OldCSP The CSP which shall be cloned)
     @raises EJwsclCSPApiException will be raised if the underlying Windows call fails.}
    constructor Create(const OldCSP: TJwCryptProvider); overload;
    {@Name releases the handle to the CSP.
     @raises EJwsclCSPApiException will be raised if the underlying Windows call fails.}
    destructor Destroy; override;
    {@Name deletes the specified keyset using CryptAcquireContext with
     the flag CRYPT_DELETEKEYSET.
     @param(KeysetName The name of the keyset to delete.)
     @raises EJwsclCSPApiException will be raised if the underlying Windows call fails.}
    class procedure DeleteKeyset(const KeysetName: TJwString);
    {@Name obtains the name of the default CSP of the specified type.
     @param(ProviderType The type of CSP for which the default
            is to retrieve)
     @param(MachineDefault If true, the machine default provider is returned.
            Otherwise, the user default is returned.
     @return(@Name returns the name of the default provider of
             the specified type.))
     @raises EJwsclCSPApiException will be raised if the underlying Windows call fails
             because there is no default provider or for other reasons}
    class function GetDefaultProvider(const ProviderType: TJwCSPType;
      MachineDefault: Boolean): TJwString;

    {@Name is the handle to the CSP.}
    property CSPHandle: Cardinal read fCSPHandle;
  end;

  TJwCryptKey = class;

  {@Name provides the possibility to compute hashes.
   @abstract(A class to compute hashes)}
  TJwHash = class
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
    function GetAlgorithm: TJwHashAlgorithm;
  public
    {Creates a new hash object
     @param(Alg Specifies the algorithm to use)
     @param(CSP The provider the hash uses. If it is @nil,
            the default CSP of type ctDss is used. Be aware that you
            will not be able to call the @link(GetSignatureLength) and
            @link(Sign) functions in this case since the CSP is created
            with the flag ccfVerifyContext meaning that there are no
            lasting key pairs in the CSP.
            The CSP can be freed during lifetime of the hash
            since the hash increments the reference count of the CSP.)
     @param(The key used for data hashing if an keyed algorithm is specified.
            Otherwise this must be @nil.)
     @raises EJwsclHashException will be raised if there is a key specified
             for a non-keyed algorithm or if no key is specified for a keyed
             algorithm.
     @raises EJsclHashApiException will be raised if the underlying Windows call fails}
    constructor Create(const Alg: TJwHashAlgorithm; const CSP: TJwCryptProvider = nil; const Key: TJwCryptKey = nil);

    {Destroys the hash object and releases the CSP}
    destructor Destroy; override;

    {Adds data to the hash object.
     @param(Data Specifies the data to be added)
     @param(Size Specifies the size of the data)
     @raises EJwsclHashApiException will be raised if the
             underlying Windows call fails due to a
             previous call to @link(RetrieveHash) or
             @link(Sign) or for other reasons.}
    procedure HashData(const Data; const Size: Cardinal);

    {@Name returns the size of the hash value in bytes.
     This value is constant for each algorithm.
     @return(@Name returns the length of the hash value.)
     @raises EJwsclHashApiException will be raised if the
             underlying Windows call fails.}
    function GetHashLength: Cardinal;
    {@Name computes the hash of the data previously added to
     the hash using @link(HashData). After a successful call
     to this function you cannot add more data to the hash.
     Any additional calls to HashData will fail.
     @param(Hash Specifies the storage in which to put the
            computed hash value)
     @param(Len Specifies the size of Hash. The procedure will
            raise an exception if Len is not big enough.)
     @raises EJwsclHashApiException will be raised if the
             underlying Windows call fails because the specified
             buffer is to small or for other reasons.}
    procedure RetrieveHash(var Hash; var Len: Cardinal); overload;

    {@Name computes the hash of the data previously added to
     the hash using @link(HashData). After a successful call
     to this function you cannot add more data to the hash.
     Any additional calls to HashData will fail.
     The buffer returned by this function must be freed using
     @link(FreeBuffer).
     @param(Len Returns the size of the hash)
     @return(Pointer to a buffer containing the hash)
     @raises EJwsclHashApiException will be raised if the
             underlying Windows call fails.}
    function RetrieveHash(out Len: Cardinal): Pointer; overload;

    {This function returns the length of the signature for subsequent
     calls to the @link(Sign) routine.
     @return(@Name returns the length of the signature.)
     @raises EJwsclHashApiException will be raised if the
             underlying Windows call fails.}
    function GetSignatureLength: Cardinal;

    {@Name computes a signature of the data in the hash.
     The used CSP must have permanent key pairs and thus
     have not been created with the ccfVerifyContext flag.
     @param(Signature The buffer the signature is stored in)
     @param(Len The length of the buffer specified in Signature)
     @param(Key A CSP usually has two key pairs. This parameter specifies
            which should be used.)
     @raises EJwsclHashApiException will be raised if the
             underlying Windows call fails because the specified
             buffer is to small or for other reasons.}
    procedure Sign(var Signature; var Len: Cardinal; Key: TJwKeyPairType = kptSignature); overload;

    {@Name computes a signature of the data in the hash.
     The used CSP must have permanent key pairs and thus
     have not been created with the ccfVerifyContext flag.
     @param(Len The length of the returned buffer
     @param(Key A CSP usually has two key pairs. This parameter specifies
            which should be used.)
     @return(The buffer containing the signature. It should be freed
              using @link(FreeBuffer) when it is no longer needed.)
     @raises EJwsclHashApiException will be raised if the
             underlying Windows call fails.}
    function  Sign(out Len: Cardinal; Key: TJwKeyPairType = kptSignature): Pointer; overload;

    {@Name frees buffer returned by previous calls to
     @link(RetrieveHash) and @link(Sign).
     @Param(Buffer The buffer to be freed)}
    procedure FreeBuffer(Buffer: Pointer);

    {The algorithm specified in the call to @link(create)
     @raises EJwsclHashApiException will be raised if the
             underlying Windows call fails.}
    property Algorithm: TJwHashAlgorithm read GetAlgorithm;
    {@Name is the handle to the hash object.}
    property HashHandle: Cardinal read fHashHandle;
  end;

  {@Name is not completely implemented yet}
  TJwCryptKey = class
  protected
    //@exclude
    fHandle: TJwKeyHandle;
    //@exclude
    class procedure RaiseApiError(const Procname, WinCallname: TJwString);
  public
    constructor Create(OldKey: TJwCryptKey);
    constructor GetUserKey(CSP: TJwCryptProvider; Key: TJwKeyPairType);
    constructor Import(CSP: TJwCryptProvider; const Data; DataLen: Cardinal;
                       PubKey: TJwCryptKey; Flags: TJwKeyFlagSet);
//    constructor Generate;
//    constructor Derive;
    destructor Destroy; override;
//    procedure ExportKey(ExpKey: TJwCryptKey;
    property Handle: TJwKeyHandle read fHandle;
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

{ TJwHash }

constructor TJwHash.Create(const Alg: TJwHashAlgorithm; const CSP: TJwCryptProvider = nil; const Key: TJwCryptKey = nil);
var KeyHandle: TJwKeyHandle;
begin
  inherited Create;
  if Assigned(Key) and (Alg in KeylessHashAlgorithms)  then
    raise EJwsclHashException.CreateFmtEx(
      RsNonKeyedHash,
      'Create',
      ClassName,
      RsUNCryptProvider,
      0,
      false,
      []);
  if not Assigned(Key) and not (Alg in KeylessHashAlgorithms) then
    raise EJwsclHashException.CreateFmtEx(
      RsKeyedHashNeedsKey,
      'Create',
      ClassName,
      RsUNCryptProvider,
      0,
      false,
      []);

  if CSP = nil then
    fProvider := TJwCryptProvider.Create('', '', ctDss, [ccfVerifyContext])
  else
    fProvider := TJwCryptProvider.Create(CSP);
  if Assigned(Key) then
    KeyHandle := Key.Handle
  else
    KeyHandle := 0;
  if not CryptCreateHash(fProvider.CSPHandle, TJwEnumMap.ConvertHashAlgorithm(Alg), KeyHandle, 0, fHashHandle) then
    RaiseApiError('Create', 'CryptCreateHash');
end;

destructor TJwHash.Destroy;
begin
  if not CryptDestroyHash(fHashHandle) then
    RaiseApiError('Destroy', 'CryptDestroyHash');
  fProvider.Free;
  inherited;
end;

procedure TJwHash.FreeBuffer(Buffer: Pointer);
begin
  FreeMem(Buffer);
end;

class procedure TJwHash.RaiseApiError(const Procname, WinCallname: TJwString);
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

function TJwHash.GetHashParam(dwParam: Cardinal; pbData: PByte; var pdwDataLen: Cardinal): BOOL;
begin
  Result := CryptGetHashParam(fHashHandle, dwParam, pbData, pdwDataLen, 0);
end;

function TJwHash.GetAlgorithm: TJwHashAlgorithm;
var Alg: ALG_ID; Size: Cardinal;
begin
  Size := SizeOf(Alg);
  if not GetHashParam(HP_ALGID, @Alg, Size) then
    RaiseApiError('GetAlgorithm', 'CryptGetHashParam');;
  Result := TJwEnumMap.ConvertHashAlgorithm(Alg);
end;

procedure TJwHash.HashData(const Data; const Size: Cardinal);
begin
  if not CryptHashData(fHashHandle, @Data, Size, 0) then
    RaiseApiError('HashData', 'CryptHashData');
end;

function TJwHash.GetHashLength: Cardinal;
var Size: Cardinal;
begin
  Size := SizeOf(Result);
  if not GetHashParam(HP_HASHSIZE, @Result, Size) then
    RaiseApiError('GetHashLength', 'CryptGetHashParam');
end;

procedure TJwHash.RetrieveHash(var Hash; var Len: Cardinal);
begin
  if not GetHashParam(HP_HASHVAL, @Hash, Len) then
    RaiseApiError('RetrieveHash', 'CryptGetHashParam');
end;

function TJwHash.RetrieveHash(out Len: Cardinal): Pointer;
begin
  Len := GetHashLength;
  GetMem(Result, Len);
  try
    RetrieveHash(Result^, Len);
    ReallocMem(Result, Len);
  except
    FreeMem(Result);
    raise;
  end;
end;

function TJwHash.GetSignatureLength: Cardinal;
begin
  Result := 0;
  //the key type should not matter for the signature length, so just take AT_SIGNATURE here
  if not CryptSignHash(fHashHandle, AT_SIGNATURE, nil, 0, nil, Result) then
    RaiseApiError('GetSignatureLength', 'CryptSignHash');
end;

procedure TJwHash.Sign(var Signature; var Len: Cardinal; Key: TJwKeyPairType = kptSignature);
begin
  if not CryptSignHash(fHashHandle, TJwEnumMap.ConvertKeyPairType(Key), nil, 0, @Signature, Len) then
    RaiseApiError('Sign', 'CryptSignHash');
end;

function TJwHash.Sign(out Len: Cardinal; Key: TJwKeyPairType = kptSignature): Pointer;
begin
  Len := GetSignatureLength;
  GetMem(Result, Len);
  try
    Sign(Result^, Len, Key);
    ReallocMem(Result, Len);
  except
    FreeMem(Result);
    raise;
  end;
end;

{ TJwCryptKey }
constructor TJwCryptKey.Create(OldKey: TJwCryptKey);
begin
  inherited Create;
  if not CryptDuplicateKey(OldKey.Handle, nil, 0, fHandle) then
    RaiseApiError('Create', 'CryptDuplicateKey');
end;
constructor TJwCryptKey.GetUserKey(CSP: TJwCryptProvider; Key: TJwKeyPairType);
begin
  inherited Create;
  if not CryptGetUserKey(CSP.CSPHandle, TJwEnumMap.ConvertKeyPairType(Key), fHandle) then
    RaiseApiError('GetUserKey', 'CryptGetUserKey');
end;

constructor TJwCryptKey.Import(CSP: TJwCryptProvider; const Data; DataLen: Cardinal; PubKey: TJwCryptKey; Flags: TJwKeyFlagSet);
begin
  inherited Create;
  if not (Flags<=ImportKeyFlags) then
    raise EJwsclKeyException.CreateFmtEx(
      RsInvalidFlags,
      'Import',
      ClassName,
      RsUNCryptProvider,
      0,
      false,
      []);
  if not CryptImportKey(CSP.CSPHandle, @Data, DataLen, PubKey.Handle, TJwEnumMap.ConvertKeyFlagSet(Flags), fHandle) then
    RaiseApiError('Import', 'CryptImportKey');
end;

destructor TJwCryptKey.Destroy;
begin
  if not CryptDestroyKey(fHandle) then
    RaiseApiError('Destroy', 'CryptDestroyKey');
  inherited Destroy;
end;

class procedure TJwCryptKey.RaiseApiError(const Procname: string; const WinCallname: string);
begin
  raise EJwsclKeyApiException.CreateFmtWinCall(
    '',
    ProcName,
    ClassName,
    RsUNCryptProvider,
    0,
    True,
    WinCallName,
    [ProcName]);
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
