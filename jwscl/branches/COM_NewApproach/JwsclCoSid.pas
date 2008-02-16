unit JwsclCoSid;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, StdVcl, Classes, JWSCLCom_TLB,
  JwaWindows,TypInfo,
  JWSCLSid;
type
  TJwSid = class(TAutoObject, IJwSid)
  //TJwCoSid = class(TTypedComObject, IJwCoSid, ISupportErrorInfo)
  protected
    fInternalSid : TJwSecurityID;
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
    function GetAccountNameInUse(const SystemName: WideString): WideString;
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
     JwsclUtils,
     JWSCLCoException;

{ TJwCoSid }

destructor TJwSid.Destroy;
begin
  FreeAndNil(fInternalSid);
  inherited;
end;

procedure TJwSid.InitByBinarySid(const BinarySid: WideString);
begin
  FreeAndNil(fInternalSid);
  try
    fInternalSid := TJwSecurityID.Create(BinarySid);
  except
    on E : Exception do
      JwTrapAndReRaiseException('InitByBinarySid', ClassName, 'JwsclCoSid', E);
  end;
end;

procedure TJwSid.Initialize;
begin
  inherited;

  fInternalSid := TJwSecurityID.Create;
end;

{ TJwCoSidList }

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

function TJwSid.Get_Attributes: LongWord;
begin

end;

function TJwSid.Get_AttributesByType: OleVariant;
begin

end;

function TJwSid.Get_CachedSystemName: WideString;
begin

end;

function TJwSid.Get_IdentifierAttributesArray: OleVariant;
begin

end;

function TJwSid.Get_IdentifierAttributesCount: SYSUINT;
begin

end;

function TJwSid.Get_IsWellKnownSidType: WordBool;
begin

end;

function TJwSid.Get_SubAuthorityArray: OleVariant;
begin

end;

function TJwSid.Get_SubAuthorityCount: SYSUINT;
begin

end;

function TJwSid.Get_UserName: WideString;
begin

end;

function TJwSid.GetAccountDomainName(const SystemName: WideString): WideString;
begin

end;

function TJwSid.GetAccountName(const SystemName: WideString): WideString;
begin

end;

function TJwSid.GetAccountNameInUse(const SystemName: WideString): WideString;
begin

end;

function TJwSid.GetCachedUserName: WideString;
begin

end;

function TJwSid.GetInternalObject: PChar;
begin

end;

function TJwSid.GetSidStream: IUnknown;
begin

end;

function TJwSid.IsStandardSid: WordBool;
begin

end;

function TJwSidList.Get__NewEnum: OleVariant;
begin

end;

function TJwSidList.Get_Count: LongWord;
begin

end;

function TJwSidList.Get_Item: IJwSid;
begin

end;

procedure TJwSid.InitByAuthorities(Authorities, Identifier: OleVariant);
begin

end;

procedure TJwSid.InitByJwSid(const Sid: IJwSid);
begin

end;

procedure TJwSid.InitByName(const SystemName, AccountName: WideString);
begin

end;

procedure TJwSid.InitByStream(const SidAsStream: IUnknown);
begin

end;

procedure TJwSid.InitByWellKnownSid(SidType: SYSUINT);
begin

end;

procedure TJwSid.Set_Attributes(Value: LongWord);
begin

end;

procedure TJwSid.Set_AttributesByType(Value: OleVariant);
begin

end;

procedure TJwSid.Set_CachedSystemName(const Value: WideString);
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
