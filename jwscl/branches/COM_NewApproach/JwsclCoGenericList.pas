unit JwsclCoGenericList;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, Classes, ActiveX, JWSCLCom_TLB, StdVcl;

type
  TJwGenericList = class(TAutoObject, IJwGenericList)
  protected
    fList : TList;
    fReadOnly : Boolean;

    procedure Add(Data: OleVariant); safecall;
    function Copy: IJwGenericList; safecall;
    function Get_Count: Integer; safecall;
    function Get_Item(Index: Integer): OleVariant; safecall;
    function Get_ReadOnly: WordBool; safecall;
    procedure Clear; safecall;
    procedure Delete(Index: Integer); safecall;
    procedure Insert(Index: Integer; Data: OleVariant); safecall;
    procedure Set_ReadOnly(Value: WordBool); safecall;
    procedure Exchange(Index: Integer; Value: OleVariant); safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

implementation

uses ComServ;

procedure TJwGenericList.Add(Data: OleVariant);
var V : PVariant;
begin
  Assert(not fReadOnly);

  if not fReadOnly then
  begin
    new(V);
    V^ := Data;

    fList.Add(V);
  end;
end;

destructor TJwGenericList.Destroy;
begin
  fReadOnly := false; 
  Clear;
  fList.Free;

  inherited;
end;

procedure TJwGenericList.Initialize;
begin
  inherited;
  fList     := TList.Create;
  fReadOnly := false;
end;

function TJwGenericList.Copy: IJwGenericList;
var i : Integer;
begin
  result := TJwGenericList.Create;
  for i := 0 to Get_Count -1 do
  begin
    result.Add(Get_Item(i));
  end;
end;

function TJwGenericList.Get_Count: Integer;
begin
  result := fList.Count;
end;

function TJwGenericList.Get_Item(Index: Integer): OleVariant;
begin
  result := PVariant(fList[Index])^;
end;

function TJwGenericList.Get_ReadOnly: WordBool;
begin
  result := fReadOnly;
end;

procedure TJwGenericList.Clear;
var i : Integer;
begin
  Assert(not fReadOnly);

  if not fReadOnly then
  begin
    for i := 0 to Get_Count -1 do
    begin
      dispose(PVariant(fList[i]));
    end;
    fList.Clear;
  end;
end;

procedure TJwGenericList.Delete(Index: Integer);
begin
  Assert(not fReadOnly);

  if not fReadOnly then
  begin
    dispose(fList[Index]);
    fList.Delete(Index);
  end;
end;

procedure TJwGenericList.Insert(Index: Integer; Data: OleVariant);
var P : PVariant;
begin
  Assert(not fReadOnly);

  if not fReadOnly then
  begin
    new(P);
    P^ := Data;

    fList.Insert(Index, P);
  end;
end;

procedure TJwGenericList.Set_ReadOnly(Value: WordBool);
begin
  //do not allow setting ReadOnly to false if fReadOnly was true
  Assert((not fReadOnly) and Value);

  if not fReadOnly then
    fReadOnly := Value;
end;

procedure TJwGenericList.Exchange(Index: Integer; Value: OleVariant);
var P : PVariant;
begin
  Assert(not fReadOnly);

  if not fReadOnly then
  begin
    dispose(PVariant(fList.Items[Index]));

    new(P);
    P^ := Value;
    fList.Items[Index] := P;
  end;
end;

initialization

end.
