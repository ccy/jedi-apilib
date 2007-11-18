{@abstract(provides access to cryptgraphic service providers (CSPs) and objects depending on them)
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
  //put this into JwsclTypes
//  TJwCSPType = (ctRsaFull, {ctRsaAes, }ctRsaSig, ctRsaSchannel, //PROV_RSA_AES is not in JwaWinCrypt.pas yet
//                ctDss, ctDssDh, ctDhSchannel, ctFortezza,
//                ctMsExchange, ctSsl);
//  TJwCSPCreationFlag = (ccfVerifyContext, ccfNewKeyset,
//                        ccfMachineKeyset, ccfSilent);
//  TJwCSPCreationFlagSet = set of TJwCSPCreationFlag;
//
//  TJwKeylessHashAlgorithm = (khaMD2, khaMD4, khaMD5, khaSHA);
//
//  TJwCSPHandle = Cardinal;
//  TJwHashHandle = Cardinal;

  //put this into JwsclException

  TJwCryptProvider = class
  protected
    fCSPHandle: Cardinal; //redeclare as TJwCSPHandle
    class procedure RaiseApiError(const Procname, WinCallname: TJwString);
  public
    constructor Create(const KeyContainerName, CSPName: TJwString; CSPType: TJwCSPType; Flags: TJwCSPCreationFlagSet);
    destructor Destroy; override;
    class procedure DeleteKeyset(const KeysetName: TJwString);
    class function GetDefaultProvider(const ProviderType: TJwCSPType): TJwString;
    property CSPHandle: Cardinal read fCSPHandle;
  end;


  TJwKeylessHash = class
  protected
    fHashHandle: Cardinal; //redeclare as TJwHashHandle
    fProvider:   TJwCryptProvider;
    class procedure RaiseApiError(const Procname, WinCallname: TJwString);
    function GetHashParam(dwParam: Cardinal; pbData: PByte; var pdwDataLen: Cardinal): BOOL;
    function GetAlgorithm: TJwKeylessHashAlgorithm;
  public
    constructor Create(Alg: TJwKeylessHashAlgorithm);
    destructor Destroy; override;
    procedure HashData(const Data; const Size: Cardinal);
    function GetHashLength: Cardinal;
    procedure RetrieveHash(var Hash; var Len: Cardinal);
    property Algorithm: TJwKeylessHashAlgorithm read GetAlgorithm;
    property HashHandle: Cardinal read fHashHandle;
  end;


{$ENDIF SL_IMPLEMENTATION_SECTION}


{$IFNDEF SL_OMIT_SECTIONS}
implementation
{$ENDIF SL_OMIT_SECTIONS}

{$IFNDEF SL_INTERFACE_SECTION}

const KeylessHashAlgorithmValues: array[TJwKeylessHashAlgorithm] of Cardinal = (
        CALG_MD2
       ,CALG_MD4
       ,CALG_MD5
       ,CALG_SHA
       );

const CSPTypeValues: array[TJwCSPType] of Cardinal = (
        PROV_RSA_FULL
       ,PROV_RSA_SIG
       ,PROV_RSA_SCHANNEL
       ,PROV_DSS
       ,PROV_DSS_DH
       ,PROV_DH_SCHANNEL
       ,PROV_FORTEZZA
       ,PROV_MS_EXCHANGE
       ,PROV_SSL
       );

const CSPCreationFlagValues: array[TJwCSPCreationFlag] of Cardinal = (
        CRYPT_VERIFYCONTEXT
       ,CRYPT_NEWKEYSET
       ,CRYPT_MACHINE_KEYSET
       ,CRYPT_SILENT
       );

function ConvertKeylessHashAlgorithm(const Alg: TJwKeylessHashAlgorithm): DWORD; overload;
begin
  Result:=KeylessHashAlgorithmValues[Alg];
end;

function ConvertKeylessHashAlgorithm(const Alg: DWORD): TJwKeylessHashAlgorithm; overload;
var i: TJwKeylessHashAlgorithm;
begin
  for i := Low(TJwKeylessHashAlgorithm) to High(TJwKeylessHashAlgorithm) do
    if KeylessHashAlgorithmValues[i] = Alg then
    begin
      Result := i;
      Break;
    end;
end;

function ConvertCSPType(const CSPType: TJwCSPType): DWORD; overload;
begin
  Result := CSPTypeValues[CSPType];
end;

function ConvertCSPType(const CSPType: DWORD): TJwCSPType; overload;
var i: TJwCSPType;
begin
  for i := Low(TJwCSPType) to High(TJwCSPType) do
    if CSPTypeValues[i] = CSPType then
    begin
      Result := i;
      Break;
    end;
end;

function ConvertCSPCreationFlags(const FlagSet: TJwCSPCreationFlagSet): Cardinal; overload;
var I : TJwCSPCreationFlag;
begin
  result := 0;
  for I := Low(TJwCSPCreationFlag) to High(TJwCSPCreationFlag) do
  begin
    if I in FlagSet then
      result := result or CSPCreationFlagValues[I];
  end;
end;

function ConvertCSPCreationFlags(const FlagBits: Cardinal): TJwCSPCreationFlagSet; overload;
var I : TJwCSPCreationFlag;
begin
  result := [];
  for I := Low(TJwCSPCreationFlag) to High(TJwCSPCreationFlag) do
  begin
    if (FlagBits and CSPCreationFlagValues[I]) = CSPCreationFlagValues[I] then
      Include(result, I);
  end;
end;

{ TJwCryptProvider }

constructor TJwCryptProvider.Create(const KeyContainerName, CSPName: TJwString; CSPType: TJwCSPType; Flags: TJwCSPCreationFlagSet);
begin
  inherited Create;
  if not CryptAcquireContext(fCSPHandle, TJwPChar(KeyContainerName), TJwPChar(CSPName), ConvertCSPType(CSPType), ConvertCSPCreationFlags(Flags)) then
    RaiseApiError('Create', 'CryptAcquireContext');
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

class function TJwCryptProvider.GetDefaultProvider(const ProviderType: TJwCSPType): TJwString;
var Len: Cardinal;
begin
  Len := 0;
  if not CryptGetDefaultProvider(ConvertCSPType(ProviderType), nil, 0, nil, Len) then
    RaiseApiError('GetDefaultProvider', 'CryptGetDefaultProvider');
  SetLength(Result, Len);
  if not CryptGetDefaultProvider(ConvertCSPType(ProviderType), nil, 0, TJwPChar(Result), Len) then
  begin
    Result := '';
    RaiseApiError('GetDefaultProvider', 'CryptGetDefaultProvider');
  end;

end;

{ TJwKeylessHash }

constructor TJwKeylessHash.Create(Alg: TJwKeylessHashAlgorithm);
begin
  inherited Create;
  fProvider := TJwCryptProvider.Create('', '', ctDss, [ccfVerifyContext]);
  if not CryptCreateHash(fProvider.CSPHandle, ConvertKeylessHashAlgorithm(Alg), 0, 0, fHashHandle) then
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
  Result := ConvertKeylessHashAlgorithm(Alg);
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
