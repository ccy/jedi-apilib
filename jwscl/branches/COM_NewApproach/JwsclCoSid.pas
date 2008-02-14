unit JwsclCoSid;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, StdVcl, Classes, JWSCLCom_TLB,
  JwaWindows,TypInfo,
  JWSCLSid;
type
  TJwCoSid = class(TAutoObject, IJwCoSid, ISupportErrorInfo)
  //TJwCoSid = class(TTypedComObject, IJwCoSid, ISupportErrorInfo)
  protected
    fInternalSid : TJwSecurityID;
    function IsStandardSid(out Value: WordBool): HResult; stdcall;

  protected
    function InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
  protected
    function InitBySid(SidPtr: PCoSid): HResult; stdcall;

    function InitByStream(const SidAsStream: IUnknown): HResult; stdcall;

    function InitByBinarySid(const BinarySid: WideString): HResult; stdcall;
    function InitByName(const SystemName, AccountName: WideString): HResult;     stdcall;

    function Get_StringSid(out Value: WideString): HResult; stdcall;
    function GetSidStream(out SidAsStream: IUnknown): HResult; stdcall;
    function GetSidPtr(out SidDataPtr: PCoSid; out SidDataSize: Integer): HResult; stdcall;

    function InitByJwSid(const Sid: IJwCoSid): HResult; stdcall;

    function InitByAuthorities(Authorities, Identifier: OleVariant): HResult;    stdcall;
    function Get_Sid(out Value: PChar): HResult; stdcall;
    function InitBySidAndAttributes(SidAndAttributes: PChar): HResult; stdcall;
    function InitByWellKnownSid(SidType: Integer): HResult; stdcall;

    function Get_IdentifierAttributesArray(out Value: OleVariant): HResult; stdcall;
    function Get_IdentifierAttributesCount(out Value: LongWord): HResult; stdcall;
    function Get_SubAuthorityArray(out Value: OleVariant): HResult; stdcall;
    function Get_SubAuthorityCount(out Value: SYSUINT): HResult; stdcall;
    function GetIdentifierAttributeByIndex(Index: SYSUINT;
      out Value: Shortint): HResult; stdcall;
    function GetSubAuthorityByIndex(Index: SYSUINT; out Value: LongWord): HResult;  stdcall;


    function Get_IsWellKnownSidType(out Value: WordBool): HResult; stdcall;
    function Get_SidLength(out Value: LongWord): HResult; stdcall;
    function Get_WellKnownSidType(out Value: Integer): HResult; stdcall;
    function GetAccountName(const SystemName: WideString): HResult; stdcall;
    function GetAccountDomainName(const SystemName: WideString): HResult; stdcall;
    function GetAccountNameUse(const SystemName: WideString): HResult; stdcall;
    function GetChachedUserFromSid(out UserName: WideString): HResult; stdcall;
    function Get_Attributes(out Value: LongWord): HResult; stdcall;
    function Set_Attributes(Value: LongWord): HResult; stdcall;


    function Get_AttributesByType(out Value: OleVariant): HResult; stdcall;
    function Set_AttributesByType(Value: OleVariant): HResult; stdcall;
    function Get_CachedSystemName(out Value: WideString): HResult; stdcall;

    function IJwCoSid.InitBySidAndAttributes = IJwCoSid_InitBySidAndAttributes;
    function IJwCoSid_InitBySidAndAttributes(
      SidAndAttributes: PCoSidAndAttributes): HResult; stdcall;
    function GetInternalSid(out Value: PChar): HResult; stdcall;


  public
   {IJwCoSid-Methoden hier deklarieren}
   destructor Destroy; override; 

  end;


  TJwCoSidList = class(TAutoObject, IJwCoSidList, ISupportErrorInfo)
  //TJwCoSidList = class(TTypedComObject, IJwCoSidList, ISupportErrorInfo)
  protected
    fInternalSidIntList : TInterfaceList;//TJwSecurityIdList;
    fInternalSidList : TJwSecurityIdList;

    procedure InitDefault;
    function Delete(Index: Integer): HResult; stdcall;

  protected
    function Add(const Sid: IJwCoSid): HResult; stdcall;
    function FindSid(const Sid: IJwCoSid; StartPos: SYSINT;
      UsePrefix: WordBool; out Index: Integer): HResult; stdcall;
    function Init: HResult; stdcall;
    function InitByTokenGroups(TokenGroups: PCoTokenGroups): HResult;
      stdcall;
    function InitBySidAndAttributes(SidAndAttributes: OleVariant): HResult; stdcall;
    function Get_First(out Value: IJwCoSid): HResult; stdcall;
    function Get_Last(out Value: IJwCoSid): HResult; stdcall;
    function Insert(Index: Integer; const Sid: IJwCoSid): HResult; stdcall;
    function Remove(const Sid: IJwCoSid): HResult; stdcall;
    function IndexOf(const Sid: IJwCoSid; out Index: LongWord): HResult;
      stdcall;
    function ToString(out Value: WideString): HResult; stdcall;

    function Get_Items(Index: LongWord; out Value: IJwCoSid): HResult;  stdcall;
    function Clear: HResult; stdcall;
  public
    destructor Destroy; override;
  end;


implementation


uses ComServ,  SysUtils, Dialogs,
     JwsclUtils,
     JWSCLCoException;



function GetUnitName(argObject: TObject): string;
var
  ptrTypeData: PTypeData;
begin
  if (argObject.ClassInfo <> nil) then
  begin
    ptrTypeData := GetTypeData(argObject.ClassInfo);
    Result := ptrTypeData.UnitName;
  end;
end;


{ TJwCoSid }

function TJwCoSid.GetSidPtr(out SidDataPtr: PCoSid;
  out SidDataSize: Integer): HResult;
var Data : PSid;
begin
  result := S_OK;
 
  try
    SidDataPtr := CoTaskMemAlloc(SECURITY_MAX_SID_SIZE);
    SidDataSize := SECURITY_MAX_SID_SIZE;

    Data := fInternalSid.CreateCopyOfSID;
    CopyMemory(SidDataPtr, Data, SECURITY_MAX_SID_SIZE);
    fInternalSid.FreeSID(Data);
  except
    On E : Exception do
      JwSetCoException('InitBySid',ClassName, GetUnitName(Self), E, Result);
  end;
end;

function TJwCoSid.GetSidStream(out SidAsStream: IInterface): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.InitBySid(SidPtr: PCoSid): HResult;
begin
  result := S_OK;
  try
    fInternalSid := TJwSecurityId.Create(PSid(SidPtr));
  except
    On E : Exception do
      JwSetCoException('InitBySid',ClassName, GetUnitName(Self), E, Result);
  end;
end;

function TJwCoSid.InitByStream(const SidAsStream: IInterface): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.InterfaceSupportsErrorInfo(const iid: TIID): HResult;
begin
  if GetInterfaceEntry(iid) <> nil then
    Result := S_OK else
    Result := S_FALSE;
end;



function TJwCoSid.InitByBinarySid(const BinarySid: WideString): HResult;
begin
  result := S_OK;
  try
    fInternalSid := TJwSecurityId.Create(BinarySid);
  except
    On E : Exception do
      JwSetCoException('InitBySid',ClassName, GetUnitName(Self), E, Result);
  end;
end;

function TJwCoSid.InitByJwSid(const Sid: IJwCoSid): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.InitByName(const SystemName,
  AccountName: WideString): HResult;
begin
  result := S_OK;
  try
    fInternalSid := TJwSecurityId.Create(SystemName, AccountName);
  except
    On E : Exception do
      JwSetCoException('InitBySid',ClassName, GetUnitName(Self), E, Result);
  end;
end;


function TJwCoSid.Get_StringSid(out Value: WideString): HResult;
begin
  result := S_OK;
  try
    Value := fInternalSid.StringSID;
  except
    On E : Exception do
      JwSetCoException('InitBySid',ClassName, GetUnitName(Self), E, Result);
  end;
end;

function TJwCoSid.InitBySidAndAttributes(SidAndAttributes: PChar): HResult;
begin
  result := E_NOTIMPL;
end;


function TJwCoSid.IJwCoSid_InitBySidAndAttributes(
  SidAndAttributes: PCoSidAndAttributes): HResult;
begin
  result := E_NOTIMPL;
end;


function TJwCoSid.InitByAuthorities(Authorities,
  Identifier: OleVariant): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.Get_Sid(out Value: PChar): HResult;
begin
  result := E_NOTIMPL;
end;



function TJwCoSid.InitByWellKnownSid(SidType: Integer): HResult;
begin
  result := E_NOTIMPL;
end;


function TJwCoSid.Get_IdentifierAttributesArray(out Value: OleVariant): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.Get_IdentifierAttributesCount(out Value: LongWord): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.Get_SubAuthorityArray(out Value: OleVariant): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.Get_SubAuthorityCount(out Value: SYSUINT): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.GetIdentifierAttributeByIndex(Index: SYSUINT;
  out Value: Shortint): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.GetSubAuthorityByIndex(Index: SYSUINT;
  out Value: LongWord): HResult;
begin
  result := E_NOTIMPL;
end;


function TJwCoSid.Get_IsWellKnownSidType(out Value: WordBool): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.Get_SidLength(out Value: LongWord): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.Get_WellKnownSidType(out Value: Integer): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.GetAccountName(const SystemName: WideString): HResult;
begin
  result := E_NOTIMPL;
end;



function TJwCoSid.GetAccountDomainName(const SystemName: WideString): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.GetAccountNameUse(const SystemName: WideString): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.GetChachedUserFromSid(out UserName: WideString): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.Get_Attributes(out Value: LongWord): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.Set_Attributes(Value: LongWord): HResult;
begin
  result := E_NOTIMPL;
end;


function TJwCoSid.Get_AttributesByType(out Value: OleVariant): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.Set_AttributesByType(Value: OleVariant): HResult;
begin
  result := E_NOTIMPL;
end;



function TJwCoSid.Get_CachedSystemName(out Value: WideString): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSid.GetInternalSid(out Value: PChar): HResult;
begin
  Value := PChar(fInternalSid);
end;


destructor TJwCoSid.Destroy;
begin
  FreeAndNil(fInternalSid);

  inherited;
end;

{ TJwCoSidList }


function TJwCoSidList.Add(const Sid: IJwCoSid): HResult;
var Internal : TJwSecurityId;
    ref : Integer;
begin
  result := S_OK;
  try
    InitDefault;

    Sid.GetInternalSid(PChar(Internal));
    ref := fInternalSidList.Add(Internal);
    fInternalSidIntList.Insert(ref, Sid);
  except
    On E : Exception do
      JwSetCoException('InitBySid',ClassName, GetUnitName(Self), E, Result);
  end;
end;


function TJwCoSidList.FindSid(const Sid: IJwCoSid; StartPos: SYSINT;
  UsePrefix: WordBool; out Index: Integer): HResult;
var Internal : TJwSecurityId;
begin
  result := S_OK;
  try
    InitDefault;

    Sid.GetInternalSid(PChar(Internal));
    Index := fInternalSidList.FindSid(Internal, StartPos, UsePrefix);
  except
    On E : Exception do
      JwSetCoException('InitBySid',ClassName, GetUnitName(Self), E, Result);
  end;
end;

function TJwCoSid.IsStandardSid(out Value: WordBool): HResult;
begin
  result := S_OK;
  try
    Value := fInternalSid.IsStandardSID;
  except
    On E : Exception do
      JwSetCoException('IsStandardSid',ClassName, GetUnitName(Self), E, Result);
  end;
end;

function TJwCoSidList.Init: HResult;
begin
  result := S_OK;
  InitDefault;
end;

function TJwCoSidList.InitByTokenGroups(
  TokenGroups: PCoTokenGroups): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSidList.InitBySidAndAttributes(
  SidAndAttributes: OleVariant): HResult;
begin
  result := E_NOTIMPL;
end;

function TJwCoSidList.Get_First(out Value: IJwCoSid): HResult;
begin
  result := S_OK;
  try
    InitDefault;

    Value := fInternalSidIntList.First as IJwCoSid;
  except
    On E : Exception do
      JwSetCoException('InitBySid',ClassName, GetUnitName(Self), E, Result);
  end;
end;

function TJwCoSidList.Get_Last(out Value: IJwCoSid): HResult;
begin
  result := S_OK;
  try
    InitDefault;

    Value := fInternalSidIntList.Last as IJwCoSid;
  except
    On E : Exception do
      JwSetCoException('InitBySid',ClassName, GetUnitName(Self), E, Result);
  end;
end;

function TJwCoSidList.Insert(Index: Integer; const Sid: IJwCoSid): HResult;
var Internal : TJwSecurityId;
begin
  result := S_OK;
  try
    InitDefault;

    Sid.GetInternalSid(PChar(Internal));
    fInternalSidList.Insert(Index, Internal);
    //if this call raises an exception the lists may be inconsistent
    try
      fInternalSidIntList.Insert(Index, Sid);
    except
      fInternalSidList.Delete(Index);
      raise;
    end;
  except
    On E : Exception do
      JwSetCoException('InitBySid',ClassName, GetUnitName(Self), E, Result);
  end;
end;

function TJwCoSidList.Remove(const Sid: IJwCoSid): HResult;
var Internal : TJwSecurityId;
    ref1, ref2 : Integer;
    B : Wordbool;
begin
  result := S_OK;
  try
    InitDefault;

   { Sid.IsStandardSid(B);
    if B then
      exit;     }

    Sid.GetInternalSid(PChar(Internal));
    ref1 := fInternalSidList.Remove(Internal);
    //if this call raises an exception the lists will be inconsistent
    ref2 := fInternalSidIntList.Remove(Sid);

    if ref1 <> ref2 then
      result := E_INVALIDARG;
  except
    On E : Exception do
      JwSetCoException('Remove',ClassName, GetUnitName(Self), E, Result);
  end;
end;

function TJwCoSidList.IndexOf(const Sid: IJwCoSid;
  out Index: LongWord): HResult;
begin
  result := S_OK;
  try
    InitDefault;

    Index := fInternalSidIntList.IndexOf(Sid);
  except
    On E : Exception do
      JwSetCoException('IndexOf',ClassName, GetUnitName(Self), E, Result);
  end;
end;

function TJwCoSidList.ToString(out Value: WideString): HResult;
begin
  result := S_OK;
  try
    InitDefault;

    Value := fInternalSidList.GetText(true);
  except
    On E : Exception do
      JwSetCoException('ToString',ClassName, GetUnitName(Self), E, Result);
  end;
end;

function TJwCoSidList.Get_Items(Index: LongWord;
  out Value: IJwCoSid): HResult;
begin
  result := S_OK;
  try
    InitDefault;

    Value := fInternalSidIntList.Items[Index] as IJwCoSid;
  except
    On E : Exception do
      JwSetCoException('Get_Items',ClassName, GetUnitName(Self), E, Result);
  end;
end;


function TJwCoSidList.Clear: HResult;
begin
  result := S_OK;
  try
    InitDefault;

    fInternalSidList.Clear;
    fInternalSidIntList.Clear;
  except
    On E : Exception do
      JwSetCoException('Clear',ClassName, GetUnitName(Self), E, Result);
  end;
end;



destructor TJwCoSidList.Destroy;
begin
  FreeAndNil(fInternalSidList);
  FreeAndNil(fInternalSidIntList);
  inherited;
end;

procedure TJwCoSidList.InitDefault;
begin
  if not Assigned(fInternalSidList) then
    fInternalSidList := TJwSecurityIdList.Create(false);
  if not Assigned(fInternalSidIntList) then
    fInternalSidIntList := TInterfaceList.Create;
end;



function TJwCoSidList.Delete(Index: Integer): HResult;
begin
  result := S_OK;
  try
    InitDefault;

    fInternalSidList.Delete(Index);
    //if this call raises an exception the lists will be inconsistent
    fInternalSidIntList.Delete(Index);
  except
    On E : Exception do
      JwSetCoException('Clear',ClassName, GetUnitName(Self), E, Result);
  end;
end;

initialization
 { TTypedComObjectFactory.Create(ComServer, TJwCoSid, Class_JwCoSid,
    ciMultiInstance, tmApartment);}
  TAutoObjectFactory.Create(ComServer, TJwCoSid, Class_JwCoSid,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwCoSidList, CLASS_JwCoSidList,
    ciMultiInstance, tmApartment);
end.
