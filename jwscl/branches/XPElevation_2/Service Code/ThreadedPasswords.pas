unit ThreadedPasswords;


interface
uses JwaWindows, JwsclEncryption, JwsclTypes, Math, ComObj, Classes;

type
  PPassEntry = ^TPassEntry;
  TPassEntry = record
    EDomain,
    EUserName,
    EPassword : Pointer;
    SizeUser,
    SizeDomain,
    SizePass : Cardinal;
  end;

  TPasswordList = class(TThreadList)
  public
    constructor Create;
    destructor Destroy; override;

    class function CreatePassEntry(const Domain, UserName, Password : WideString)
      : PPassEntry;
    class procedure FreePassEntry(var P : PPassEntry);

    function Add(const Domain, UserName, Password : WideString) : Integer;
    procedure Get(Index : Integer;
          out Domain, UserName, Password : WideString);
    procedure Delete(Index : Integer);
    procedure FreeIndex(Index : Integer);
  end;


implementation

{ TPasswordList }

function TPasswordList.Add(const Domain, UserName,
  Password: WideString): Integer;
begin
  try
    result := LockList.Add(CreatePassEntry(Domain, UserName, Password));
  finally
    UnlockList;
  end;
end;

constructor TPasswordList.Create;
begin
  inherited;
end;

class function TPasswordList.CreatePassEntry(const Domain, UserName,
  Password: WideString): PPassEntry;
var P : PPassEntry;
    SizeUser,
    SizeDomain,
    SizePass : Cardinal;
begin
  New(P);

  SizeUser := (2+Length(UserName))*sizeof(WideChar);
  GetMem(P^.EUserName, SizeUser);
  SizeDomain := (2+Length(Domain))*sizeof(WideChar);
  GetMem(P^.EDomain, SizeDomain);
  SizePass := (2+Length(Password))*sizeof(WideChar);
  GetMem(P^.EPassword, SizePass);

  OleCheck(StringCbCopyW(P^.EUserName, SizeUser, PWideChar(UserName)));
  TJwEncryptMemory.EncryptMemory(P^.EUserName, SizeUser, [pmSameProcess],
      mtGetMem);

  OleCheck(StringCbCopyW(P^.EDomain, SizeDomain, PWideChar(Domain)));
  TJwEncryptMemory.EncryptMemory(P^.EDomain, SizeUser, [pmSameProcess],
      mtGetMem);

  OleCheck(StringCbCopyW(P^.EPassword, SizePass, PWideChar(Password)));
  TJwEncryptMemory.EncryptMemory(P^.EPassword, SizeUser, [pmSameProcess],
      mtGetMem);

  P^.SizeUser := SizeUser;
  P^.SizeDomain := SizeDomain;
  P^.SizePass := SizePass;

  result := P;
end;

procedure TPasswordList.Delete(Index: Integer);
var P : PPassEntry;
    L : TList;
begin
  L := LockList;
  try
    P := L.Items[Index];
    FreePassEntry(P);
    L.Items[Index] := nil;
  finally 
    UnlockList;
  end;
end;

destructor TPasswordList.Destroy;
var L : TList;
    i : Integer;
begin
  L := LockList;
  try
    for i := L.Count-1 downto 0 do
    begin
      Delete(i);
    end;
  finally
    UnlockList;
  end;
  Clear;

  inherited;
end;

procedure TPasswordList.FreeIndex(Index: Integer);
var L : TList;
    P: PPassEntry;
begin
  L := LockList;
  try
    P := L.Items[Index];
    FreePassEntry(P);
    L.Items[Index] := nil;
  finally
    UnlockList;
  end;
end;

class procedure TPasswordList.FreePassEntry(var P: PPassEntry);
begin
  if P <> nil then
  begin
    ZeroMemory(P^.EDomain, P^.SizeDomain);
    FreeMem(P^.EDomain);

    ZeroMemory(P^.EUserName, P^.SizeUser);
    FreeMem(P^.EUserName);

    ZeroMemory(P^.EPassword, P^.SizePass);
    FreeMem(P^.EPassword);
  end;
  P := nil;
end;

procedure TPasswordList.Get(Index: Integer; out Domain, UserName,
  Password: WideString);
var P : PPassEntry;
    L : TList;
    Data : Pointer;

    SizeUser,
    SizeDomain,
    SizePass : Cardinal;
begin
  L := LockList;
  try
    P := L.Items[Index];

    SizeUser := P^.SizeUser;
    SizeDomain := P^.SizeDomain;
    SizePass := P^.SizePass;

    if SizeUser > 0 then
    begin
      GetMem(Data, SizeUser);
      try
        CopyMemory(Data, P^.EUserName, SizeUser);
        TJwEncryptMemory.DecryptMemory(Data, SizeUser, [pmSameProcess], mtGetMem);
        SetLength(UserName, SizeUser div sizeof(WideChar));
        OleCheck(StringCbCopyW(PWideChar(@UserName[1]), SizeUser, Data));
      finally
        ZeroMemory(Data, SizeUser);
        FreeMem(Data);
      end;
    end;

    if SizeDomain > 0 then
    begin
      GetMem(Data, SizeDomain);
      try
        CopyMemory(Data, P^.EDomain, SizeDomain);
        TJwEncryptMemory.DecryptMemory(Data, SizeDomain, [pmSameProcess], mtGetMem);
        SetLength(Domain, SizeDomain div sizeof(WideChar));
        OleCheck(StringCbCopyW(PWideChar(@Domain[1]), SizeDomain, Data));
      finally
        ZeroMemory(Data, SizeDomain);
        FreeMem(Data);
      end;
    end;

    if SizePass > 0 then
    begin
      GetMem(Data, SizePass);
      try
        CopyMemory(Data, P^.EPassword, SizePass);
        TJwEncryptMemory.DecryptMemory(Data, SizePass, [pmSameProcess], mtGetMem);
        SetLength(Password, SizePass div sizeof(WideChar));
        OleCheck(StringCbCopyW(PWideChar(@Password[1]), SizePass, Data));
      finally
        ZeroMemory(Data, SizePass);
        FreeMem(Data);
      end;
    end;


  finally
    UnlockList;
  end;
end;

end.
