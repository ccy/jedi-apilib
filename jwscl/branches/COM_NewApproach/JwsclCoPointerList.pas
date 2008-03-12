unit JwsclCoPointerList;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  JwaWindows, ComObj, Classes, ActiveX, JWSCLCom_TLB, StdVcl, SyncObjs;

type
  TJwPointerList = class(TAutoObject, IJwPointerList)
  protected
    fList,
    fSizeList : TList;
    fReadOnly,
    fOwnData : Boolean;
    fCS : TCriticalSection;
    fCallback : IJwListFindCallback;

    function GetData(Index: Integer): PChar; safecall;
    function AddData(Data: PChar; Size: LongWord): Integer; safecall;
    function Get_ReadOnly: WordBool; safecall;
    procedure DeleteData(Index: Integer); safecall;
    procedure Set_ReadOnly(Value: WordBool); safecall;
    function Get_OwnData: WordBool; safecall;
    procedure Set_OwnData(Value: WordBool); safecall;
    function AddAndDuplicate(Data: PChar; Size: LongWord): Integer; safecall;
    function Copy(Duplicate: WordBool): IJwPointerList; safecall;
    function Get_Count: Integer; safecall;
    function Get_ItemSize(Index: Integer): LongWord; safecall;
    function Get_Item(Index: Integer; Duplicate: WordBool): PChar; safecall;
    procedure InsertData(Index: Integer; Data: PChar; Size: LongWord);
      safecall;
    procedure InsertDataAndDuplicate(Index: Integer; Data: PChar;
      Size: LongWord); safecall;
    procedure Clear; safecall;
    procedure Exchange(Index: Integer; Data: PChar; Size: LongWord); safecall;
    function Find(UserData: PChar): Integer; safecall;
    function Get_Callback: IJwListFindCallback; safecall;
    procedure Set_Callback(const Value: IJwListFindCallback); safecall;

  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

implementation

uses ComServ;

function TJwPointerList.GetData(Index: Integer): PChar;
begin
  result := fList.Items[Index];
end;

function TJwPointerList.AddData(Data: PChar; Size: LongWord): Integer;
begin
  fCS.Enter;
  try
    Assert(not fReadOnly);

    if not fReadOnly then
    begin
      result := fList.Add(Data);
      fSizeList.Add(Pointer(Size));
    end;
  finally
    fCS.Leave;
  end;
end;

function TJwPointerList.Get_ReadOnly: WordBool;
begin
  fCS.Enter;
  try
    result := fReadOnly;
  finally
    fCS.Leave;
  end;
end;

procedure TJwPointerList.DeleteData(Index: Integer);
begin
  fCS.Enter;
  try
    Assert(not fReadOnly);

    if not fReadOnly then
    begin
      fList.Delete(Index);
      fSizeList.Delete(Index);
    end;
  finally
    fCS.Leave;
  end;
end;

procedure TJwPointerList.Set_ReadOnly(Value: WordBool);
begin
  //do not allow setting ReadOnly to false if fReadOnly was true
  Assert(fReadOnly and not Value);

  if fReadOnly then
  begin
    fCS.Enter;
    try
      fReadOnly := Value;
    finally
      fCS.Leave;
    end;
  end;
end;

procedure TJwPointerList.Initialize;
begin
  inherited;
  fList     := TList.Create;
  fSizeList := TList.Create;
  fCS := TCriticalSection.Create;

  fOwnData := true;
  fReadOnly := false;
end;

destructor TJwPointerList.Destroy;
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
    fSizeList.Free;
  finally
    fCS.Leave;
    fCS.Free;
  end;
  inherited;
end;

function TJwPointerList.Get_OwnData: WordBool;
begin
  fCS.Enter;
  try
    result := fOwnData;
  finally
    fCS.Leave;
  end;
end;

procedure TJwPointerList.Set_OwnData(Value: WordBool);
begin
  fCS.Enter;
  try
    fOwnData := Value;
  finally
    fCS.Leave;
  end;
end;

function TJwPointerList.AddAndDuplicate(Data: PChar;
  Size: LongWord): Integer;
var p : Pointer;
begin
  fCS.Enter;
  try
    Assert(not fReadOnly);
    Assert(fOwnData);

    if not fReadOnly then
    begin
      P := CoTaskMemAlloc(Size + 10);
      Assert(P <> nil);

      ZeroMemory(P, Size + 10);
      CopyMemory(P, Data, Size);
      result := AddData(P, Size);
    end;
  finally
    fCS.Leave;
  end;

end;

function TJwPointerList.Copy(Duplicate: WordBool): IJwPointerList;
var i : Integer;
begin
  fCS.Enter;
  try
    Assert(fReadOnly and not Duplicate);
    if fReadOnly and not Duplicate then
    begin
      result := nil;
      fCS.Leave;
      exit;
    end;

    result := TJwPointerList.Create;
    result.OwnData := Duplicate;
    for i := 0 to Get_Count -1 do
    begin
      if Duplicate then
        result.AddAndDuplicate(GetData(i),Get_ItemSize(i))
      else
        result.AddData(GetData(i),Get_ItemSize(i));
    end;
  finally
    fCS.Leave;
  end;

end;

function TJwPointerList.Get_Count: Integer;
begin
  fCS.Enter;
  try
    result := fList.Count;
  finally
    fCS.Leave;
  end;

end;

function TJwPointerList.Get_ItemSize(Index: Integer): LongWord;
begin
  fCS.Enter;
  try
    result := LongWord(fSizeList[Index]);
  finally
    fCS.Leave;
  end;

end;


function TJwPointerList.Get_Item(Index: Integer;
  Duplicate: WordBool): PChar;
var P : Pointer;
begin
  fCS.Enter;
  try
    P := fList.Items[Index];
    Assert(P <> nil);

    if Duplicate then
    begin
      result := CoTaskMemAlloc(DWORD(fSizeList[Index]));
      Assert(result <> nil);

      ZeroMemory(result, DWORD(fSizeList[Index]));
      CopyMemory(result, P, DWORD(fSizeList[Index]));
    end
    else
      result := P;
  finally
    fCS.Leave;
  end;

end;

procedure TJwPointerList.InsertData(Index: Integer; Data: PChar; 
  Size: LongWord);
begin
  fCS.Enter;
  try
    Assert(not fReadOnly);

    if not fReadOnly then
    begin
      fList.Insert(Index, Data);
      fSizeList.Insert(Index, Pointer(Size));
    end;
  finally
    fCS.Leave;
  end;

end;

procedure TJwPointerList.InsertDataAndDuplicate(Index: Integer;
  Data: PChar; Size: LongWord);
var p : Pointer;
begin
  fCS.Enter;
  try
    Assert(not fReadOnly);

    if not fReadOnly then
    begin
      Assert(fOwnData);

      P := CoTaskMemAlloc(Size + 10);
      Assert(P <> nil);

      ZeroMemory(P, Size + 10);
      CopyMemory(P, Data, Size);
      InsertData(Index, PChar(P), Size);
    end;
  finally
    fCS.Leave;
  end;

end;

procedure TJwPointerList.Clear;
var i : Integer;
begin
  fCS.Enter;
  try
    Assert(not fReadOnly);

    if not fReadOnly and fOwnData then
    begin
      for i := 0 to fList.Count-1 do
      begin
        CoTaskMemFree(fList[i]);
      end;

      fList.Clear;
      fSizeList.Clear;
    end;
  finally
    fCS.Leave;
  end;

end;

procedure TJwPointerList.Exchange(Index: Integer; Data: PChar;
  Size: LongWord);
var P : PVariant;
begin
  fCS.Enter;
  try
    Assert(not fReadOnly);

    if fReadOnly then
    begin
      DeleteData(Index);
      if fOwnData then
        InsertDataAndDuplicate(Index, Data, Size)
      else
        InsertData(Index, Data, Size);
    end;
  finally
    fCS.Leave;
  end;
    
end;


function TJwPointerList.Find(UserData: PChar): Integer;
var i : Integer;

begin
  result := -1;
  if not Assigned(fCallback) then
    exit;

  fCS.Enter;
  try
    for i := 0 to fList.Count-1 do
    begin
      if fCallback.OnIteratePtr(I, PCHAR(fList[i]), DWORD(fSizeList[i]), UserData) then
      begin
        result := i;
        break;
      end;
    end;
  finally
    fCS.Leave;
  end;
end;
function TJwPointerList.Get_Callback: IJwListFindCallback;
begin
  fCS.Enter;
  try
    result := fCallback;
  finally
    fCS.Leave;
  end;
end;

procedure TJwPointerList.Set_Callback(const Value: IJwListFindCallback);
begin
  fCS.Enter;
  try
    fCallback := Value;
  finally
    fCS.Leave;
  end;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TJwPointerList, Class_JwPointerList,
    ciMultiInstance, tmApartment);
end.
