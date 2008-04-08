unit JwsclCoAccessControlList;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwAccessControlList = class(TAutoObject, IJwAccessControlList)
  protected
    procedure AddAllowAce(const Sid: IJwSid; Mask: LongWord; Flags: LongWord; ObjectType: TGUID;
                          InheritedObjectType: TGUID); safecall;
    procedure AddDenyAce(const Sid: IJwSid; Mask: LongWord; Flags: LongWord; ObjectType: TGUID;
                         InheritedObjectType: TGUID); safecall;
    procedure Item; safecall;
    function Get__NewEnum: IJwAccessControlEntry; safecall;
    function GetInternalObject: PChar; safecall;
    procedure Delete(Index: LongWord); safecall;
    procedure Remove(const Ace: IJwAccessControlEntry); safecall;
    function ToString: WideString; safecall;
    procedure Find(const Sid: IJwSid; Index: Integer); safecall;
    function Get_Revision: LongWord; safecall;
    procedure Set_Revision(Value: LongWord); safecall;
    procedure InitByAccessList(const List: IJwAccessControlList); safecall;
    procedure Clear; safecall;
  end;

implementation

uses ComServ;

{ TJwAccessControlList }

procedure TJwAccessControlList.AddAllowAce(const Sid: IJwSid; Mask,
  Flags: LongWord; ObjectType, InheritedObjectType: TGUID);
begin

end;

procedure TJwAccessControlList.AddDenyAce(const Sid: IJwSid; Mask,
  Flags: LongWord; ObjectType, InheritedObjectType: TGUID);
begin

end;

procedure TJwAccessControlList.Clear;
begin

end;

procedure TJwAccessControlList.Delete(Index: LongWord);
begin

end;

procedure TJwAccessControlList.Find(const Sid: IJwSid; Index: Integer);
begin

end;

function TJwAccessControlList.Get__NewEnum: IJwAccessControlEntry;
begin

end;

function TJwAccessControlList.Get_Revision: LongWord;
begin

end;

function TJwAccessControlList.GetInternalObject: PChar;
begin

end;

procedure TJwAccessControlList.InitByAccessList(
  const List: IJwAccessControlList);
begin

end;

procedure TJwAccessControlList.Item;
begin

end;

procedure TJwAccessControlList.Remove(const Ace: IJwAccessControlEntry);
begin

end;

procedure TJwAccessControlList.Set_Revision(Value: LongWord);
begin

end;

function TJwAccessControlList.ToString: WideString;
begin

end;

initialization
  TAutoObjectFactory.Create(ComServer, TJwAccessControlList, CLASS_JwAccessControlList,
    ciMultiInstance, tmApartment);
end.
