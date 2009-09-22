unit JwsclCoAccessControlEntry;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TIJwAccessControlEntry = class(TAutoObject, IJwAccessControlEntry)
  protected
    function Get_AccessMask: LongWord; safecall;
    procedure Set_AccessMask(Value: LongWord); safecall;
    function Get_Sid: IJwSid; safecall;
    procedure Set_Sid(const Value: IJwSid); safecall;
    function Get_Flags: LongWord; safecall;
    procedure Set_Flags(Value: LongWord); safecall;
    function Get_Revision: Word; safecall;
    procedure Set_Revision(Value: Word); safecall;
    function Get_ObjectType: TGUID; safecall;
    procedure Set_ObjectType(Value: TGUID); safecall;
    function Get_InheritedObjectType: TGUID; safecall;
    procedure Set_InheritedObjectType(Value: TGUID); safecall;
    function ToString(const Map: IUnknown): WideString; safecall;
    function Get_AceType: LongWord; safecall;
    procedure Init(AceType: LongWord); safecall;
  end;

implementation

uses ComServ;

{ TIJwAccessControlEntry }

function TIJwAccessControlEntry.Get_AccessMask: LongWord;
begin

end;

function TIJwAccessControlEntry.Get_AceType: LongWord;
begin

end;

function TIJwAccessControlEntry.Get_Flags: LongWord;
begin

end;

function TIJwAccessControlEntry.Get_InheritedObjectType: TGUID;
begin

end;

function TIJwAccessControlEntry.Get_ObjectType: TGUID;
begin

end;

function TIJwAccessControlEntry.Get_Revision: Word;
begin

end;

function TIJwAccessControlEntry.Get_Sid: IJwSid;
begin

end;

procedure TIJwAccessControlEntry.Init(AceType: LongWord);
begin

end;

procedure TIJwAccessControlEntry.Set_AccessMask(Value: LongWord);
begin

end;

procedure TIJwAccessControlEntry.Set_Flags(Value: LongWord);
begin

end;

procedure TIJwAccessControlEntry.Set_InheritedObjectType(Value: TGUID);
begin

end;

procedure TIJwAccessControlEntry.Set_ObjectType(Value: TGUID);
begin

end;

procedure TIJwAccessControlEntry.Set_Revision(Value: Word);
begin

end;

procedure TIJwAccessControlEntry.Set_Sid(const Value: IJwSid);
begin

end;

function TIJwAccessControlEntry.ToString(
  const Map: IInterface): WideString;
begin

end;

initialization
  TAutoObjectFactory.Create(ComServer, TIJwAccessControlEntry, Class_JwAccessControlEntry,
    ciMultiInstance, tmApartment);
end.
