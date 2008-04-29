unit JwsclCoEnumSet;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwEnumSet = class(TAutoObject, IJwEnumSet)
  protected
    fBits : JwBitMask;
    fType : JwEnumSetType;
    function Get_BitMask: JwBitMask; safecall;
    function Get_EnumType: JwEnumSetType; safecall;
    procedure Exclude(EnumConst: Integer); safecall;
    procedure Include(EnumConst: Integer); safecall;
    procedure Set_BitMask(Value: JwBitMask); safecall;
    procedure Set_EnumType(Value: JwEnumSetType); safecall;
    function State(EnumConst: Integer): WordBool; safecall;
    function IsBitAvailable(EnumConst: Integer): WordBool; safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

implementation

uses ComServ, JwsclTypes, TypInfo;

function TJwEnumSet.Get_BitMask: JwBitMask;
begin
  result := fBits;
end;

function TJwEnumSet.Get_EnumType: JwEnumSetType;
begin
  result := fType;
end;

procedure TJwEnumSet.Exclude(EnumConst: Integer);
begin
  if IsBitAvailable(EnumConst) then;
end;

procedure TJwEnumSet.Include(EnumConst: Integer);
begin
  if IsBitAvailable(EnumConst) then;
end;

procedure TJwEnumSet.Set_BitMask(Value: JwBitMask);
var i : Integer;
    Bits : JwBitMask;
begin
  Bits := 0;
  for i := 0 to sizeof(Value)*8 do
  begin
    if (Value and i = i) and IsBitAvailable(I) then
      Bits := Bits or i;
  end;
end;

procedure TJwEnumSet.Set_EnumType(Value: JwEnumSetType);
begin
  fType := Value;
end;

destructor TJwEnumSet.Destroy;
begin

  inherited;
end;

procedure TJwEnumSet.Initialize;
begin
  inherited;
  fBits := 0;
  fType := JwEnumSetSIDAttributes;
end;

function TJwEnumSet.State(EnumConst: Integer): WordBool;
begin
  result := fBits and EnumConst = EnumConst; 
end;

function IsInBorders(const V : PTypeInfo; Value : Integer) : Boolean;
var i : Integer;
    TypeData : PTypeData;
begin
  result := false;

  if V^.Kind = tkEnumeration then
  begin
    TypeData := GetTypeData(V);
    result := (Value >= TypeData^.MinValue) and
             (Value <= TypeData^.MaxValue);
  end;
end;

function TJwEnumSet.IsBitAvailable(EnumConst: Integer): WordBool;
var i : integer;
    i1 : TJwSidAttribute;
begin
  result := false;
  case fType of
    JwEnumSetSIDAttributes : result := IsInBorders(TypeInfo(TJwSidAttribute), EnumConst);
  end;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TJwEnumSet, Class_JwEnumSet,
    ciMultiInstance, tmApartment);
end.
