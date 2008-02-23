unit JwsclCoSid;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, StdVcl, Classes, JWSCLCom_TLB,
  JwaWindows,JwaVista, TypInfo,
  JWSCLSid;
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
    function GetSidStream: IUnknown; safecall;
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
  public
   {IJwCoSid-Methoden hier deklarieren}
   procedure Initialize; override;
   destructor Destroy; override;

  end;


  TJwSidList = class(TAutoObject, IJwSidList)
  //TJwCoSidList = class(TTypedComObject, IJwCoSidList, ISupportErrorInfo)
  protected
    fInternalSidIntList : TInterfaceList;//TJwSecurityIdList;
    fInternalSidList : TJwSecurityIdList;
    procedure Method1; safecall;
  protected
    function Get__NewEnum: OleVariant; safecall;
    function Get_Count: LongWord; safecall;
    function Get_Item: IJwSid; safecall;
    procedure Add(const Sid: IJwSid); safecall;
    procedure Delete(Index: Integer); safecall;
    procedure InitBySidList(const SidList: IUnknown); safecall;
    procedure Insert(Index: Integer; const Sid: IJwSid); safecall;
    procedure Remove(const Sid: IJwSid); safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;


implementation


uses ComServ,  SysUtils, Dialogs,
     JwsclUtils, JwsclExceptions, Variants,
     JwsclCOMExports,
     JWSCLCoException;

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
    fInternalSid := TJwSecurityID.Create(BinarySid);
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

function TJwSid.Get_AttributesByType: OleVariant;
begin
  result := Null;
end;

function TJwSid.Get_CachedSystemName: WideString;
begin
  result := fInternalSid.CachedSystemName;
end;

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

function TJwSid.Get_SubAuthorityArray: OleVariant;
type TDWORDArray = array of DWORD;
var i : Integer;
    p : ^TDWORDArray;
begin
  result := VarArrayCreate([0,fInternalSid.SubAuthorityCount-1], varLongWord);
  p := VarArrayLock(result);
  try
    for i := 0 to fInternalSid.SubAuthorityCount-1 do
    begin
      p^[i] := fInternalSid.SubAuthority[i];
    end;
  finally
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

function TJwSid.GetSidStream: IUnknown;
begin
  result := CreateSidAndAttributesStream(fInternalSid.SID, fInternalSid.Attributes);
end;

function TJwSid.IsStandardSid: WordBool;
begin
  result := fInternalSid.IsStandardSID;
end;


procedure TJwSid.InitByAuthorities(Authorities, Identifier: OleVariant);
var fSid : TJwSecurityId;
    Auth : TJwSubAuthorityArray;
    Ident : TSidIdentifierAuthority;
    AuthBuf,
    IdentBuf : Pointer;
begin
  AuthBuf := VarArrayLock(Authorities);
  try
    IdentBuf := VarArrayLock(Identifier);

    try
      ZeroMemory(@Ident.Value, sizeof(Ident.Value));
      CopyMemory(@Ident.Value, PByteArray(@IdentBuf), VarArrayHighBound(Identifier,1)-1);

      try
        fSid := TJwSecurityID.Create(TJwSubAuthorityArray(AuthBuf), Ident);
      except
        on E : Exception do
          JwTrapAndReRaiseException('InitByJwSid', ClassName, 'JwsclCoSid', E);
      end;
    finally
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

procedure TJwSid.Set_AttributesByType(Value: OleVariant);
begin
  raise EAbstractError.Create('Unimplemented');
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
  raise EAbstractError.Create('Unimplemented');
end;

function TJwSidList.Get_Count: LongWord;
begin
  result := fInternalSidList.Count;
end;

function TJwSidList.Get_Item: IJwSid;
begin
  raise EAbstractError.Create('Unimplemented');
end;

destructor TJwSidList.Destroy;
begin

  inherited;
end;

procedure TJwSidList.Initialize;
begin
  inherited;

end;

procedure TJwSidList.Method1;
begin

end;


procedure TJwSidList.Add(const Sid: IJwSid);
begin

end;

procedure TJwSidList.Delete(Index: Integer);
begin

end;

procedure TJwSidList.InitBySidList(const SidList: IUnknown);
begin

end;

procedure TJwSidList.Insert(Index: Integer; const Sid: IJwSid);
begin

end;

procedure TJwSidList.Remove(const Sid: IJwSid);
begin

end;




initialization


  TAutoObjectFactory.Create(ComServer, TJwSid, Class_JwSid,
    ciMultiInstance, tmApartment);
  TAutoObjectFactory.Create(ComServer, TJwSidList, CLASS_JwSidList,
    ciMultiInstance, tmApartment);
end.
