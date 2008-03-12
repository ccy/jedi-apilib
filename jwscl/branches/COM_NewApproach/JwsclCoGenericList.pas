unit JwsclCoGenericList;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, Classes, ActiveX, JWSCLCom_TLB, StdVcl, SyncObjs;

type
  TJwGenericList = class(TAutoObject, IJwGenericList)
  protected
    fList : TList;
    fReadOnly : Boolean;
    fCS : TCriticalSection;
    fCallback : IJwListFindCallback;

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
    function Find(Data: OleVariant; UserData: PChar): Integer; safecall;
    function Get_Callback: IJwListFindCallback; safecall;
    procedure Set_Callback(const Value: IJwListFindCallback); safecall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

implementation

uses ComServ;

procedure TJwGenericList.Add(Data: OleVariant);
var V : PVariant;
begin
  fCS.Enter;
  try
    Assert(not fReadOnly);

    if not fReadOnly then
    begin
      new(V);
      V^ := Data;

      fList.Add(V);
    end;
  finally
    fCS.Leave;
  end;    
end;

destructor TJwGenericList.Destroy;
begin
  fCS.Enter;
  try
    fReadOnly := false;
  finally
    fCS.Leave;
  end;

  Clear;

  fCS.Enter;
  try
    fList.Free;
  finally
    fCS.Leave;
    fCS.Free;
  end;

  inherited;
end;

procedure TJwGenericList.Initialize;
begin
  inherited;
  fList     := TList.Create;
  fReadOnly := false;
  fCS := TCriticalSection.Create;
end;

function TJwGenericList.Copy: IJwGenericList;
var i : Integer;
begin
  fCS.Enter;
  try
    result := TJwGenericList.Create;
    for i := 0 to Get_Count -1 do
    begin
      result.Add(Get_Item(i));
    end;
  finally
    fCS.Leave;
  end;
end;

function TJwGenericList.Get_Count: Integer;
begin
  fCS.Enter;
  try
    result := fList.Count;
  finally
    fCS.Leave;
  end;
end;

function TJwGenericList.Get_Item(Index: Integer): OleVariant;
begin
  fCS.Enter;
  try
    result := PVariant(fList[Index])^;
  finally
    fCS.Leave;
  end;
end;

function TJwGenericList.Get_ReadOnly: WordBool;
begin
  fCS.Enter;
  try
    result := fReadOnly;
  finally
    fCS.Leave;
  end;
end;

procedure TJwGenericList.Clear;
var i : Integer;
begin
  fCS.Enter;
  try
    Assert(not fReadOnly);

    if not fReadOnly then
    begin
      for i := 0 to Get_Count -1 do
      begin
        dispose(PVariant(fList[i]));
      end;
      fList.Clear;
    end;
  finally
    fCS.Leave;
  end;
end;

procedure TJwGenericList.Delete(Index: Integer);
begin
  fCS.Enter;
  try
    Assert(not fReadOnly);

    if not fReadOnly then
    begin
      dispose(fList[Index]);
      fList.Delete(Index);
    end;
  finally
    fCS.Leave;
  end;
end;

procedure TJwGenericList.Insert(Index: Integer; Data: OleVariant);
var P : PVariant;
begin
  fCS.Enter;
  try
    Assert(not fReadOnly);

    if not fReadOnly then
    begin
      new(P);
      P^ := Data;

      fList.Insert(Index, P);
    end;
  finally
    fCS.Leave;
  end;
end;

procedure TJwGenericList.Set_ReadOnly(Value: WordBool);
begin
  fCS.Enter;
  try
    //do not allow setting ReadOnly to false if fReadOnly was true
    Assert((not fReadOnly) and Value);

    if not fReadOnly then
      fReadOnly := Value;
  finally
    fCS.Leave;
  end;
end;

procedure TJwGenericList.Exchange(Index: Integer; Value: OleVariant);
var P : PVariant;
begin
  fCS.Enter;
  try
    Assert(not fReadOnly);

    if not fReadOnly then
    begin
      dispose(PVariant(fList.Items[Index]));

      new(P);
      P^ := Value;
      fList.Items[Index] := P;
    end;
  finally
    fCS.Leave;
  end;
end;

function TJwGenericList.Find(Data: OleVariant; UserData: PChar): Integer;
var i : Integer;

begin
  result := -1;

  fCS.Enter;
  try
    for i := 0 to fList.Count-1 do
    begin
      if (not Assigned(fCallback) and
       (PVariant(fList[i])^ = Data)) or

        (Assigned(fCallback) and
        fCallback.OnIterateGeneric(I, PVariant(fList[i])^, UserData)) then
      begin
        result := i;
        break;
      end;
    end;
  finally
    fCS.Leave;
  end;
end;

function TJwGenericList.Get_Callback: IJwListFindCallback;
begin
  fCS.Enter;
  try
    result := fCallback;
  finally
    fCS.Leave;
  end;
end;

procedure TJwGenericList.Set_Callback(const Value: IJwListFindCallback);
begin
  fCS.Enter;
  try
   fCallback := Value;
  finally
    fCS.Leave;
  end;
end;

initialization

end.
