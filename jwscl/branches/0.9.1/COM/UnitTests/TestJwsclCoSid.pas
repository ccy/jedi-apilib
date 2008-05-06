unit TestJwsclCoSid;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, TypInfo, JWSCLSid, JwaWindows, Classes, ActiveX, StdVcl, ComObj,
  SysUtils,
  Dialogs,
  ComLib,
  JwsclTypes,
  JWSCLCoException,
  JwsclComExports,
  JWSCLCom_TLB;

type
  // Testmethoden für Klasse TJwSid
  
  TestTJwSid = class(TTestCase)
  private
    FJwSid: IJwSid;
    FJwSids : array of IJwSid;
    Excp : Exception;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInitByBinarySid;

    procedure TestInitByAuthorities;
    procedure TestInitByJwSid;
    procedure TestInitByName;
    procedure TestInitByStream;
    procedure TestInitByWellKnownSid;

    procedure TestGet_Attributes;
    procedure TestGet_AttributesByType;
    procedure TestGet_CachedSystemName;
    procedure TestGet_IdentifierAttributesArray;
    procedure TestGet_IdentifierAttributesCount;
    procedure TestGet_IsWellKnownSidType;
    procedure TestGet_SubAuthorityArray;
    procedure TestGet_SubAuthorityCount;
    procedure TestGet_UserName;
    procedure TestGetAccountDomainName;
    procedure TestGetAccountName;
    procedure TestGetAccountNameInUse;
    procedure TestGetCachedUserName;
    procedure TestGetInternalObject;
    procedure TestGetSidStream;
    procedure TestIsStandardSid;

    procedure TestItem;

    procedure TestSet_Attributes;
    procedure TestSet_AttributesByType;
    procedure TestSet_CachedSystemName;

    procedure TestIsEqualSid;

    procedure Test_1;
  end;
  // Testmethoden für Klasse TJwSidList

  TestTJwSidList = class(TTestCase)
  private
    FJwSidList: IJwSidList;
  public
    procedure SetUp; override;
    procedure TearDown; override;

    procedure TestInitBySidList;
    procedure TestAdd;
    procedure TestInsert;
    procedure TestRemove;
    procedure TestDelete;
    procedure TestCount;
    procedure TestItem;
  end;

function CreateSidAndAttributesStream(const Sid : PSid; const Attributes : DWORD) : IStream; stdcall;
  external '..\JwsclCom.dll';

implementation
{$IFDEF SM_JCLDEBUG}
uses jclDebug;       
{$ENDIF}


procedure TestTJwSid.SetUp;
var i : Integer;
begin
  FJwSid := CoJwSid.Create;
  SetLength(FJwSids,4);
  for i := 0 to high(FJwSids) do
    FJwSids[i] := CoJwSid.Create as IJwSid;
end;

procedure TestTJwSid.TearDown;
var i : Integer;
begin
  FJwSid := nil;
  for i := 0 to high(FJwSids) do
    FJwSids[i] := nil;
end;

procedure TestTJwSid.TestGetAccountDomainName;
var Excp : Exception;
begin
  try
 //   FJwSids[0].InitByName('','SYSTEM');
  //  FJwSids[0].GetAccountDomainName('');
  except
    on E : Exception do
    begin
      JwOleRaiseEx(E, Excp);
      raise Excp;
    end;
  end;
end;

procedure TestTJwSid.TestGetAccountName;
var Excp : Exception;
begin
  try
    FJwSids[0].InitByName('','SYSTEM');
    CheckEquals('SYSTEM', FJwSids[0].GetAccountName(''));
  except
    on E : Exception do
    begin
      JwOleRaiseEx(E, Excp);
      raise Excp;
    end;
  end;
end;

procedure TestTJwSid.TestGetAccountNameInUse;
begin

end;

procedure TestTJwSid.TestGetCachedUserName;
begin

end;

procedure TestTJwSid.TestGetInternalObject;
begin

end;

procedure TestTJwSid.TestGetSidStream;
var
  Stream : IStream;
begin
  try
    try
      FJwSids[0].InitByName('','SYSTEM');

      Stream := FJwSids[0].GetStream as IStream;

      FJwSids[1].InitByStream(Stream);
      CheckTrue(FJwSids[1].IsEqualSid(FJwSids[0]));
    except
      on E : Exception do
      begin
        JwOleRaiseEx(E, Excp);
        raise Excp;
      end;
    end;

  finally
  end;
end;

procedure TestTJwSid.TestGet_Attributes;
begin

end;

procedure TestTJwSid.TestGet_AttributesByType;
var
  BinarySid: WideString;
  E2 : Exception;
  V : Variant;
begin
  try
    FJwSids[0].InitByBinarySid('S-1-1-0');
    FJwSids[0].Attributes :=  SE_GROUP_MANDATORY or SE_GROUP_ENABLED_BY_DEFAULT;
    V := FJwSids[0].AttributesByType;
  except
    on E : Exception do
    begin
      JwOleRaiseEx(E, E2);
      raise E2;
    end;
  end;
end;

procedure TestTJwSid.TestGet_CachedSystemName;
begin

end;

procedure TestTJwSid.TestGet_IdentifierAttributesArray;
begin

end;

procedure TestTJwSid.TestGet_IdentifierAttributesCount;
begin

end;

procedure TestTJwSid.TestGet_IsWellKnownSidType;
begin

end;

procedure TestTJwSid.TestGet_SubAuthorityArray;
begin

end;

procedure TestTJwSid.TestGet_SubAuthorityCount;
begin

end;

procedure TestTJwSid.TestGet_UserName;
begin

end;

procedure TestTJwSid.TestInitByAuthorities;
var Auth,
    Ident : Variant;
    E2 : Exception;
begin
  try
    FJwSids[0].InitByName('','SYSTEM');
    Auth := FJwSids[0].SubAuthorityArray;
    Ident := FJwSids[0].IdentifierAttributesArray;

    FJwSids[1].InitByAuthorities(Auth, Ident);

    CheckTRUE(FJwSids[1].IsEqualSid(FJwSids[0]));

  except
    on E : Exception do
    begin
      JwOleRaiseEx(E, E2);
      raise E2;
    end;
  end;
end;

procedure TestTJwSid.TestInitByBinarySid;
var
  BinarySid: WideString;
  E2 : Exception;
begin
  try
    FJwSids[0].InitByBinarySid('S-1-1-0');
  except
    on E : Exception do
    begin
      JwOleRaiseEx(E, E2);
      raise E2;
    end;
  end;
end;

procedure TestTJwSid.TestInitByJwSid;
var
  BinarySid: WideString;
  E2 : Exception;
begin
  try
    FJwSids[0].InitByName('','SYSTEM');
    FJwSids[1].InitByJwSid(FJwSids[0]);
    CheckTRUE(FJwSids[0].IsEqualSid(FJwSids[1]));
  except
    on E : Exception do
    begin
      JwOleRaiseEx(E, E2);
      raise E2;
    end;
  end;
end;

procedure TestTJwSid.TestInitByName;
var
  BinarySid: WideString;
  E2 : Exception;
begin
  try
    FJwSid.InitByName('','SYSTEM1');
  except
    on E : Exception do
    begin
      JwOleRaiseEx(E, E2);
      //raise E2;
    end;
  end;
end;


procedure TestTJwSid.TestInitByStream;
var
  Stream : IStream;
  Data : TJwSecurityId;
begin
  Data := TJwSecurityId.Create('S-1-1-0');
  Data.Attributes := high(DWORD);
  try
    Stream := CreateSidAndAttributesStream(Data.SID, Data.Attributes);
    try
      FJwSids[0].InitByStream(Stream);
    except
      on E : Exception do
      begin
        JwOleRaiseEx(E, Excp);
        raise Excp;
      end;
    end;

    CheckEquals(Data.Attributes, FJwSids[0].Attributes);
    CheckEquals(Data.StringSID, FJwSids[0].StringSid);


  finally
    FreeAndNil(Data);
  end;
end;

procedure TestTJwSid.TestInitByWellKnownSid;
begin

end;

procedure TestTJwSid.TestIsEqualSid;
var
  BinarySid: WideString;
  E2 : Exception;
  JwSid1 : IJwSid;
begin
  try
    JwSid1 := CoJwSid.Create;
    JwSid1.InitByName('','SYSTEM');
    FJwSid.InitByName('','SYSTEM');

    CheckTrue(FJwSid.IsEqualSid(JwSid1));
  except
    on E : Exception do
    begin
      JwOleRaiseEx(E, E2);
      raise E2;
    end;
  end;
end;

procedure TestTJwSid.TestIsStandardSid;
begin

end;

procedure TestTJwSid.TestItem;
var SidO : OleVariant;
    List : IJwSidList;
    Enum : TEnumVariant;
    i,v : DWORD;
begin
  List := CoJwSidList.Create;

  for i := low(FJwSids) to high(FJwSids) do
  begin
    FJwSids[i].InitByBinarySid('S-1-1-'+IntToStr(i));
    List.Add(FJwSids[i]);
  end;


  i := 0;
  Enum := TEnumVariant.CreateUnknown(List);
  try
    while (Enum.ForEach(SidO)) do
    begin
      v := (IUnknown(SidO) as IJwSid).SubAuthorityArray[0];
      CheckEquals(i,v, 'SubAuthority Index is not equal to the one in list');
      Inc(i);
    end;
  finally
    Enum.Free;
  end;
end;

procedure TestTJwSid.TestSet_Attributes;
begin

end;

procedure TestTJwSid.TestSet_AttributesByType;
var
  BinarySid: WideString;
  E2 : Exception;
  V : Variant;
begin
  try
    FJwSids[0].InitByBinarySid('S-1-1-0');
    FJwSids[0].Attributes :=  SE_GROUP_MANDATORY or SE_GROUP_ENABLED_BY_DEFAULT;
    V := FJwSids[0].AttributesByType;
    FJwSids[0].Attributes :=  0;
    FJwSids[0].AttributesByType := V;
    CheckEquals(3, FJwSids[0].Attributes);
  except
    on E : Exception do
    begin
      JwOleRaiseEx(E, E2);
      raise E2;
    end;
  end;
end;

procedure TestTJwSid.TestSet_CachedSystemName;
begin

end;

procedure TestTJwSidList.SetUp;
begin
  FJwSidList := CoJwSidList.Create
end;

procedure TestTJwSidList.TearDown;
begin
  FJwSidList := nil;
end;

procedure TestTJwSidList.TestAdd;
begin

end;

procedure TestTJwSidList.TestCount;
begin

end;

procedure TestTJwSidList.TestDelete;
begin

end;

procedure TestTJwSidList.TestInitBySidList;
begin

end;

procedure TestTJwSidList.TestInsert;
begin

end;

procedure TestTJwSidList.TestItem;
begin

end;

procedure TestTJwSidList.TestRemove;
begin

end;

procedure TestTJwSid.Test_1;
var
   F : IJwGenericMapping;
  // F : IJwDesktopMapping;
begin
  //F := CoJwFileFolderMapping.Create;
  //F := CoJwGenericMapping.Create;
  F := CoJwDesktopMapping.Create;
  F.GetMapping;

end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTJwSid.Suite);
  RegisterTest(TestTJwSidList.Suite);
end.

