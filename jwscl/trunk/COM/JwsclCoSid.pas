unit JwsclCoSid;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, StdVcl, Classes, JWSCLCom_TLB,
  JwaWindows,JwaVista, TypInfo, ComLib, JwsclCoLogging,  JWSCLSid;

type
  TJwSid = class(TAutoObject, IJwSid)
  //TJwCoSid = class(TTypedComObject, IJwCoSid, ISupportErrorInfo)
  protected
    fInternalSid : TJwSecurityID;
    function Get_StringSid: WideString; safecall;

  protected
    function Get_Attributes: LongWord; safecall;
    function Get_AttributesByType: OleVariant; safecall;
    function Get_CachedSystemName: WideString; safecall;
    function Get_IdentifierAttributesArray: OleVariant; safecall;
    function Get_IdentifierAttributesCount: SYSUINT; safecall;
    function Get_IsWellKnownSidType: WordBool; safecall;
    function Get_SubAuthorityArray: OleVariant; safecall;
    function Get_SubAuthorityCount: SYSUINT; safecall;
    function Get_UserName: WideString; safecall;
    function GetAccountDomainName(const SystemName: WideString): WideString;
      safecall;
    function GetAccountName(const SystemName: WideString): WideString; safecall;
    function GetAccountNameInUse(const SystemName: WideString): LongWord;
      safecall;
    function GetCachedUserName: WideString; safecall;
    function GetInternalObject: PChar; safecall;
    function GetStream: IUnknown; safecall;
    function IsStandardSid: WordBool; safecall;

    procedure InitByBinarySid(const BinarySid: WideString); safecall;
    procedure InitByAuthorities(Authorities, Identifier: OleVariant); safecall;
    procedure InitByJwSid(const Sid: IJwSid); safecall;
    procedure InitByName(const SystemName, AccountName: WideString); safecall;
    procedure InitByStream(const SidAsStream: IUnknown); safecall;
    procedure InitByWellKnownSid(SidType: SYSUINT); safecall;
    procedure Set_Attributes(Value: LongWord); safecall;
    procedure Set_AttributesByType(Value: OleVariant); safecall;
    procedure Set_CachedSystemName(const Value: WideString); safecall;
    function IsEqualSid(const Sid: IJwSid): WordBool; safecall;
    function ToString: WideString; safecall;
  public
   {IJwCoSid-Methoden hier deklarieren}
   procedure Initialize; override;
   destructor Destroy; override;

  end;


  TJwSidList = class(TAutoObject, IJwSidList, IVariantCollection)
  //TJwCoSidList = class(TTypedComObject, IJwCoSidList, ISupportErrorInfo)
  protected
    fInternalSidIntList : TInterfaceList;//TJwSecurityIdList;
    fInternalSidList : TJwSecurityIdList;
    procedure Method1; safecall;
  protected
     //used by enumerator to lock list
    function GetController: IUnknown; stdcall;
    //used by enumerator to determine how many items
    function GetCount: integer; stdcall;
    //used by enumerator to retrieve items
    function GetItems (const Index: olevariant): olevariant; stdcall;

    function Get__NewEnum: OleVariant; safecall;
    procedure Add(const Sid: IJwSid); safecall;
    procedure Delete(Index: Integer); safecall;
    procedure InitBySidList(const SidList: IUnknown); safecall;
    procedure Insert(Index: Integer; const Sid: IJwSid); safecall;
    procedure Remove(const Sid: IJwSid); safecall;
    function Get_Count: LongWord; safecall;
    function Get_Item(Index: LongWord): IJwSid; safecall;
    procedure Clear; safecall;
    function Find(const Sid: IJwSid; StartPos: Integer;
      UsePreFix: WordBool): Integer; safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;


implementation


uses ComServ,  SysUtils, Dialogs,
     JwsclUtils, JwsclExceptions, Variants,
     JwsclCOMExports, JwsclTypes,
     JWSCLCoException;

type
  PDWordArray = ^TDWordArray;
  TDWordArray = array[0..16383] of DWord;

function VarArrayLength(const V : Variant; Dim : Integer = 1) : Cardinal;
var i1, i2 : Integer;
begin
  i1 := VarArrayHighBound(V,Dim);
  i2 := VarArrayLowBound(V,Dim);
  result := abs(abs(i1) - abs(i2))+1;
end;

{ TJwCoSid }

destructor TJwSid.Destroy;
begin
  FreeAndNil(fInternalSid);
  inherited;
end;

procedure TJwSid.InitByBinarySid(const BinarySid: WideString);
var fSid : TJwSecurityId;
begin
  try
    fSid := TJwSecurityID.Create(BinarySid);
  except
    on E : Exception do
      JwTrapAndReRaiseException('InitByBinarySid', ClassName, 'JwsclCoSid', E);
  end;

  FreeAndNil(fInternalSid);
  fInternalSid := fSid;
end;

procedure TJwSid.Initialize;
begin
  inherited;

  fInternalSid := TJwSecurityID.Create;
end;



function TJwSid.Get_Attributes: LongWord;
begin
  result := fInternalSid.Attributes;
end;


{
post:
     (VarType(result) = varArray) and
     (VarArrayLowBound(result) = 0) and
     (VarType(result[i]) = varByte)
}
function TJwSid.Get_AttributesByType: OleVariant;
var i : TJwSidAttribute;
begin
  if fInternalSid.SID = nil then;

  result := VarArrayCreate([0, Integer(high(TJwSidAttribute))], varByte);

  for i := low(TJwSidAttribute) to high(TJwSidAttribute) do
  begin
    if i in fInternalSid.AttributesType then
      result[Byte(i)] := Byte(i)
    else
      result[Byte(i)] := 0;
  end;
end;

function TJwSid.Get_CachedSystemName: WideString;
begin
  result := fInternalSid.CachedSystemName;
end;


{
post:
     (VarType(result) = varArray) and
     (VarArrayLowBound(result) = 0) and
     (VarArrayHighBound(result) <= 5) and
     (VarType(result[i]) = varByte)

}
function TJwSid.Get_IdentifierAttributesArray: OleVariant;
var i : Integer;
    p : PByteArray;
    Ident : TSidIdentifierAuthority;
begin
  result := VarArrayCreate([0,Get_IdentifierAttributesCount-1], varByte);
  p := VarArrayLock(result);
  try
    Ident := fInternalSid.IdentifierAuthority;
    Copymemory(p, @Ident.Value, Get_IdentifierAttributesCount);
  finally
    VarArrayUnlock(result);
  end;
end;

function TJwSid.Get_IdentifierAttributesCount: SYSUINT;
begin
  result := Length(fInternalSid.IdentifierAuthority.Value);
end;

function TJwSid.Get_IsWellKnownSidType: WordBool;
begin
  result := fInternalSid.IsWellKnownSid;
end;

{
post:
     (VarType(result) = varArray) and
     (VarArrayLowBound(result) = 0) and
     (VarType(result[i]) = varLongWord)

}
function TJwSid.Get_SubAuthorityArray: OleVariant;

var i : Integer;
    A : PDWORDArray;
    p : Pointer;
    v : VARIANT;
begin
  result := VarArrayCreate([0,fInternalSid.SubAuthorityCount-1], varLongWord);
  p := VarArrayLock(result);
  A := P;
  try
    for i := 0 to fInternalSid.SubAuthorityCount-1 do
    begin
      A^[i] := DWORD(fInternalSid.SubAuthority[i]);
    end;
  finally
    A := nil;
    VarArrayUnlock(result);
  end;
end;

function TJwSid.Get_SubAuthorityCount: SYSUINT;
begin
  result := fInternalSid.SubAuthorityCount;
end;

function TJwSid.Get_UserName: WideString;
begin
  try
    result := fInternalSid.GetAccountName(fInternalSid.CachedSystemName);
  except
    on E : Exception do
      JwTrapAndReRaiseException('Get_UserName', ClassName, 'JwsclCoSid', E);
  end;
end;

function TJwSid.GetAccountDomainName(const SystemName: WideString): WideString;
begin
  result := fInternalSid.GetAccountDomainName(SystemName);
end;

function TJwSid.GetAccountName(const SystemName: WideString): WideString;
begin
  result := fInternalSid.GetAccountName(SystemName);
end;

function TJwSid.GetAccountNameInUse(
  const SystemName: WideString): LongWord;
begin
  result := fInternalSid.GetAccountNameUse(SystemName);
end;

function TJwSid.GetCachedUserName: WideString;
begin
  result := fInternalSid.GetCachedUserFromSid;
end;

function TJwSid.GetInternalObject: PChar;
begin
  result := PChar(fInternalSid);
end;

function TJwSid.GetStream: IUnknown;
begin
  result := CreateSidAndAttributesStream(fInternalSid.SID, fInternalSid.Attributes);
end;

function TJwSid.IsStandardSid: WordBool;
begin
  result := fInternalSid.IsStandardSID;
end;



{
pre: (VarType(Authorities) = varArray) and
     (VarArrayLowBound(Authorities) = 0) and
     (VarType(Authorities[i]) = varLongWord)

     (VarType(Identifier) = varArray) and
     (VarArrayLowBound(Identifier) = 0) and
     (VarArrayHighBound(Identifier) < 0) and
     (VarType(Identifier[i]) = varByte)
}
procedure TJwSid.InitByAuthorities(Authorities, Identifier: OleVariant);
var fSid : TJwSecurityId;
    Auth : TJwSubAuthorityArray;
    Ident : TSidIdentifierAuthority;
    AuthBuf,
    IdentBuf : Pointer;
    i : Integer;
begin
  ASSERT(VarArrayLowBound(Identifier,1) = 0);
  ASSERT(VarArrayHighBound(Identifier,1) <= 5);
  ASSERT(VarArrayLowBound(Authorities,1) = 0);


  AuthBuf := VarArrayLock(Authorities);
  try
    IdentBuf := VarArrayLock(Identifier);

    try
      ZeroMemory(@Ident.Value, sizeof(Ident.Value));

      for i := 0 to VarArrayHighBound(Identifier,1) do
      begin
        ASSERT(VarType(Ident.Value[i]) = varByte);
        Ident.Value[i] := PByteArray(IdentBuf)^[i];
      end;

      SetLength(Auth, VarArrayLength(Authorities));
      for i := Low(Auth) to High(Auth) do
      begin
        ASSERT(VarType(Ident.Value[i]) = varLongWord);
        Auth[i] := PDWordArray(AuthBuf)^[i];
      end;

      try
        fSid := TJwSecurityID.Create(TJwSubAuthorityArray(Auth), Ident);
      except
        on E : Exception do
          JwTrapAndReRaiseException('InitByJwSid', ClassName, 'JwsclCoSid', E);
      end;
    finally
      Auth := nil;
      VarArrayUnlock(Identifier);
    end;
  finally
    VarArrayUnlock(Authorities);
  end;

  FreeAndNil(fInternalSid);
  fInternalSid := fSid;
end;


procedure TJwSid.InitByJwSid(const Sid: IJwSid);
var fSid : TJwSecurityId;
begin
  try
    fSid := TJwSecurityID.Create(TJwSecurityId(Sid.GetInternalObject));
  except
    on E : Exception do
      JwTrapAndReRaiseException('InitByJwSid', ClassName, 'JwsclCoSid', E);
  end;

  FreeAndNil(fInternalSid);
  fInternalSid := fSid;
end;

procedure TJwSid.InitByName(const SystemName, AccountName: WideString);
var fSid : TJwSecurityId;
begin
  try
    fSid := TJwSecurityID.Create(SystemName, AccountName);
  except
    on E : Exception do
      JwTrapAndReRaiseException('InitByName', ClassName, 'JwsclCoSid', E);
  end;

  FreeAndNil(fInternalSid);
  fInternalSid := fSid;
end;

{
pre:
SidAsStream.content(
 SidSize: sizeof(Byte)
 Sid: SidSize * sizeof(Byte)
 Attributes: sizeof(DWORD)
)

}
procedure TJwSid.InitByStream(const SidAsStream: IUnknown);
var
    SidSize : Integer;
    RetSize : Longint;
    Stream : IStream;
    SidType : Byte;
    fSid : TJwSecurityId;
    SID : PSID;
    Attributes : DWORD;
begin
//  ASSERT(SidAsStream is IStream);

  Stream := SidAsStream as IStream;

  try
    Sid := nil;

    OleCheck(Stream.Read(@SidSize, sizeof(SidSize), @RetSize));
    ASSERT(sizeof(SidSize) = RetSize);

    try
      GetMem(Sid, SidSize);

      OleCheck(Stream.Read(Sid, SidSize, @RetSize));
      ASSERT(SidSize = RetSize);

      OleCheck(Stream.Read(@Attributes, sizeof(Attributes), @RetSize));
      ASSERT(sizeof(Attributes) = RetSize);


      if not IsValidSid(Sid) then
        raise EJwsclInvalidSIDException.Create('Sid in Stream of Parameter SidAsStream is invalid');

      fSid := TJwSecurityID.Create(Sid);
      fSid.Attributes := Attributes;

    finally
      if Sid <> nil then
        FreeMem(Sid);
    end;
  except
    on E : Exception do
      JwTrapAndReRaiseException('InitByStream', ClassName, 'JwsclCoSid', E);
  end;


  FreeAndNil(fInternalSid);
  fInternalSid := fSid;
end;

procedure TJwSid.InitByWellKnownSid(SidType: SYSUINT);
var fSid : TJwSecurityId;
begin
  try
    fSid := TJwSecurityID.CreateWellKnownSid(jwaVista.TWellKnownSidType(SidType));
  except
    on E : Exception do
      JwTrapAndReRaiseException('InitByWellKnownSid', ClassName, 'JwsclCoSid', E);
  end;

  FreeAndNil(fInternalSid);
  fInternalSid := fSid;
end;

procedure TJwSid.Set_Attributes(Value: LongWord);
begin
  fInternalSid.Attributes := Value;
end;

{
pre: (VarType(Value) = varArray)
}
procedure TJwSid.Set_AttributesByType(Value: OleVariant);
var i : Integer;
    Sets : TJwSidAttributeSet;
begin
  Sets := [];
  for i := VarArrayLowBound(Value,1) to VarArrayHighBound(Value,1) do
  begin
    if Value[i] <> 0 then
      Include(Sets, TJwSidAttribute(i));
  end;
  fInternalSid.AttributesType := Sets;
end;

procedure TJwSid.Set_CachedSystemName(const Value: WideString);
begin
  fInternalSid.CachedSystemName := Value;
end;

function TJwSid.IsEqualSid(const Sid: IJwSid): WordBool;
begin
  try
    result := fInternalSid.EqualSid(TJwSecurityId(Sid.GetInternalObject));
  except
    on E : Exception do
      JwTrapAndReRaiseException('IsEqualSid', ClassName, 'JwsclCoSid', E);
  end;
end;

function TJwSid.Get_StringSid: WideString;
begin
  result := fInternalSid.StringSID;
end;

 { TJwCoSidList }

function TJwSidList.Get__NewEnum: OleVariant;
begin
  result := TEnumVariantCollection.Create(Self) as IUnknown;
end;

function TJwSidList.GetController: IUnknown;
begin
  result := Self;
end;

function TJwSidList.GetCount: integer;
begin
  result := fInternalSidList.Count;
end;

function TJwSidList.GetItems(const Index: olevariant): olevariant;
begin
  result := fInternalSidIntList.Items[Index] as IDispatch;
end;


destructor TJwSidList.Destroy;
begin
  FreeAndNil(fInternalSidIntList);
  FreeAndNil(fInternalSidList);
  inherited;
end;

procedure TJwSidList.Initialize;
begin
  inherited;
  fInternalSidIntList := TInterfaceList.Create;
  fInternalSidList := TJwSecurityIdList.Create(false);
end;

procedure TJwSidList.Method1;
begin

end;


procedure TJwSidList.Add(const Sid: IJwSid);
begin
  fInternalSidIntList.Add(Sid);
  fInternalSidList.Add(TJwSecurityId(Sid.GetInternalObject));
end;

procedure TJwSidList.Delete(Index: Integer);
begin
  fInternalSidIntList.Delete(Index);
  fInternalSidList.Delete(Index);
end;

{
pre: SidList is IJwSidList
}
procedure TJwSidList.InitBySidList(const SidList: IUnknown);
var List : IJwSidList;
    i : Integer;
    Sid : IJwSid;
begin
  List := SidList as IJwSidList;

  Self.Clear;

  for i := 0 to List.Count - 1 do
  begin
    Sid := CoJwSid.Create;
    //copy sids
    Sid.InitByJwSid(List.Item[i]);

    Self.Add(Sid);
  end;
end;

procedure TJwSidList.Insert(Index: Integer; const Sid: IJwSid);
begin
  fInternalSidIntList.Insert(Index, Sid);
  fInternalSidList.Insert(Index, TJwSecurityId(Sid.GetInternalObject));
end;

procedure TJwSidList.Remove(const Sid: IJwSid);
begin
  fInternalSidIntList.Remove(Sid);
  fInternalSidList.Remove(TJwSecurityId(Sid.GetInternalObject));
end;



function TJwSidList.Get_Count: LongWord;
begin
  result := fInternalSidIntList.Count;
end;

function TJwSidList.Get_Item(Index: LongWord): IJwSid;
begin
  result := fInternalSidIntList.Items[Index] as IJwSid;
end;

function TJwSid.ToString: WideString;
begin
  result := fInternalSid.GetText(true);
end;

procedure TJwSidList.Clear;
begin
  try
    fInternalSidList.OwnsObjects := false;
    fInternalSidList.Clear;
  finally
    fInternalSidIntList.Clear;
  end;
end;


function TJwSidList.Find(const Sid: IJwSid; StartPos: Integer;
  UsePreFix: WordBool): Integer;
begin
  result := fInternalSidList.FindSid(TJwSecurityId(Sid.GetInternalObject), StartPos, Boolean(UsePreFix));
end;



initialization

  TAutoObjectFactory.Create(ComServer, TJwSid, Class_JwSid,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwSidList, CLASS_JwSidList,
    ciMultiInstance, tmApartment);
end.
