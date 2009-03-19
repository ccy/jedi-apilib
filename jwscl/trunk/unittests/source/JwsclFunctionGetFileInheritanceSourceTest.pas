unit JwsclFunctionGetFileInheritanceSourceTest;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der 
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, JwsclExceptions, Registry, JwsclMapping, Contnrs, SysUtils,
  JwsclKnownSid, JwsclAcl, JwsclToken, JwsclConstants, JwsclDescriptor, Dialogs,
  JwsclUtils, Classes, JwsclSid, JwsclSecureObjects, JwsclVersion, jwaWindows,
  JwsclComUtils,
  JwsclResource, JwsclTypes, JwsclPathSimulation,  JwsclStrings;

type
  TInheritanceEntry = class;
  TInheritanceEntryClass = class of TInheritanceEntry;
  // Testmethoden für Klasse TJwSecureFileObject

  TestTTestGetFileInheritanceFunction = class(TTestCase)
  strict private
    FJwSecureFileObject: TJwSecureFileObject;
    fInheritancePath : TJwInheritancePath;

    function OnGetNamedSecurityInfo(PathName : TJwString; SeType : TSeObjectType; const aSecurityInfo: TJwSecurityInformationFlagSet;
       var OwnedSD : Boolean; const Data : Pointer) : TJwSecurityDescriptor;
  public
    fMainPath : String;
    DACL : TJwDAccessControlList;

    procedure CheckInheritance(
       const ExcpetedInheritance : TJwInheritedFromArray;
       const Inheritance : TJwInheritedFromArray;
       const Msg : String = '');

    procedure SetUp; override;
    procedure TearDown; override;

    property MainPath : String read fMainPath;
    property InheritancePath : TJwInheritancePath read fInheritancePath;

  published
    procedure Test_1;
    procedure Test_2;
  end;

  TAccessEntry = class;

  //creates security access list
  TAccessList = class
  public
    class function CreateSecurity : TAccessEntry;
  end;

  //maintains an ACE
  TAccessEntry = class
  private
    SD : TJwSecurityDescriptor;

    function Add(ACEClass : TJwSecurityAccessControlEntryClass; const AccessMask: TJwAccessMask;
      const SID: TJwSecurityKnownSID;
      const Flags: TJwAceFlags) : TAccessEntry;
  public
    destructor Destroy; override;

    function AddAllow(const AccessMask: TJwAccessMask;
      const SID: TJwSecurityKnownSID;
      const Flags: TJwAceFlags) : TAccessEntry;
    function AddDeny(const AccessMask: TJwAccessMask;
      const SID: TJwSecurityKnownSID;
      const Flags: TJwAceFlags) : TAccessEntry;

    function ReturnAndFree : TJwSecurityDescriptor;
    procedure AssignAndFree(Assignee : TJwSecurityDescriptor);
  end;

  //maintains inheritance entry
  TInheritanceEntry = class
  private
    class var Arr : TJwInheritedFromArray;
  public
    class function Init : TInheritanceEntry;

    class function Add(
        const AncestorName : String;
        const Gap : Integer;
        const Sid : TJwSecurityKnownSID) : TInheritanceEntryClass;

    class function Get : TJwInheritedFromArray;
  end;

implementation

procedure TestTTestGetFileInheritanceFunction.SetUp;
begin
  FJwSecureFileObject := TJwSecureFileObject.Create('');
end;

procedure TestTTestGetFileInheritanceFunction.TearDown;
begin
  FJwSecureFileObject.Free;
  FJwSecureFileObject := nil;
  FreeAndNil(fInheritancePath);
end;

procedure TestTTestGetFileInheritanceFunction.CheckInheritance(
  const ExcpetedInheritance : TJwInheritedFromArray;
  const Inheritance: TJwInheritedFromArray;
  const Msg: String);
var i : Integer;
begin
  CheckEquals(Length(ExcpetedInheritance), Length(Inheritance), 'Number of inheritance entries differs' +#13#10 + msg);

  for I := 0 to Length(Inheritance) - 1 do
  begin
    Check(CompareMem(@Inheritance[i], @ExcpetedInheritance[i], sizeof(TJwInheritedFromRecord)),
      Format('Inheritance entry %d failed.'#13#10,[i])+ Msg);
  end;
end;

function TestTTestGetFileInheritanceFunction.OnGetNamedSecurityInfo(
  PathName : TJwString; SeType : TSeObjectType; const aSecurityInfo: TJwSecurityInformationFlagSet;
  var OwnedSD : Boolean; const Data : Pointer) : TJwSecurityDescriptor;
begin
  result := TJwSecurityDescriptor.Create;
  result.Assign(InheritancePath.SD[PathName]);
end;



procedure TestTTestGetFileInheritanceFunction.Test_1;
begin
  //Check(false, 'Not implemented');
  fMainPath := 'C:\';
  fInheritancePath := TJwInheritancePath.Create(fMainPath);


  TAccessList.CreateSecurity.
    AddAllow(GENERIC_ALL, JwAdministratorsSID, [afContainerInheritAce]).
  AssignAndFree(InheritancePath.SD['C:\']);

  CheckInheritance(
    TInheritanceEntry.Create.
      Add('', 0, nil).
    Get(),
    FJwSecureFileObject.GetFileInheritanceSource(fMainPath, true,
        [siDaclSecurityInformation], OnGetNamedSecurityInfo, nil),
    'No inheritance failed');
end;

procedure TestTTestGetFileInheritanceFunction.Test_2;
begin
  //Check(false, 'Not implemented');
  fMainPath := 'C:\Docs\';
  fInheritancePath := TJwInheritancePath.Create(fMainPath);

  TAccessList.CreateSecurity.
    AddAllow(GENERIC_ALL, JwAdministratorsSID, [afContainerInheritAce]).
    AddAllow(GENERIC_ALL, JwAdministratorsSID, [afObjectInheritAce]).
  AssignAndFree(InheritancePath.SD['C:\']);

  TAccessList.CreateSecurity.
    AddAllow(GENERIC_ALL, JwAdministratorsSID, [afInheritedAce,afContainerInheritAce]).
    AddAllow(GENERIC_ALL, JwAdministratorsSID, [afInheritedAce,afObjectInheritAce]).
  AssignAndFree(InheritancePath.SD['C:\Docs\']);

  CheckInheritance(

    TInheritanceEntry.Create.
      Add('',1, JwAdministratorsSID).
    Get(),

    FJwSecureFileObject.GetFileInheritanceSource(fMainPath, true,
        [siDaclSecurityInformation], OnGetNamedSecurityInfo, nil),
    '');
end;


{ TAccessList }

class function TAccessList.CreateSecurity : TAccessEntry;
begin
  result := TAccessEntry.Create;
end;

{ TAccessEntry }

function TAccessEntry.Add(ACEClass: TJwSecurityAccessControlEntryClass;
  const AccessMask: TJwAccessMask; const SID: TJwSecurityKnownSID;
  const Flags: TJwAceFlags): TAccessEntry;
begin
  if not Assigned(SD) then
  begin
    SD := TJwSecurityDescriptor.CreateDefaultByToken();
    SD.DACL.Clear;
  end;

  SD.DACL.Add(ACEClass.Create(nil,
      Flags, AccessMask, SID));

  result := Self;
end;

function TAccessEntry.AddAllow(const AccessMask: TJwAccessMask;
      const SID: TJwSecurityKnownSID;
      const Flags: TJwAceFlags) : TAccessEntry;
begin
  result := Add(TJwDiscretionaryAccessControlEntryAllow,
      AccessMask, Sid, Flags);
end;


function TAccessEntry.AddDeny(const AccessMask: TJwAccessMask;
  const SID: TJwSecurityKnownSID; const Flags: TJwAceFlags): TAccessEntry;
begin
  result := Add(TJwDiscretionaryAccessControlEntryDeny,
      AccessMask, Sid, Flags);
end;

procedure TAccessEntry.AssignAndFree(Assignee: TJwSecurityDescriptor);
begin
  Assignee.Assign(SD);
  FreeAndNil(SD);
  Free;
end;

destructor TAccessEntry.Destroy;
begin
  inherited;
end;

function TAccessEntry.ReturnAndFree: TJwSecurityDescriptor;
begin
  result := SD;
  Free;
end;

{ TInheritanceEntry }

class function TInheritanceEntry.Add(const AncestorName: String;
  const Gap: Integer; const Sid: TJwSecurityKnownSID): TInheritanceEntryClass;
var i : Integer;
    DomainName : TJwString;
    SidUse : Cardinal;
begin
  if Length(Arr) = 0 then
    SetLength(Arr, 1)
  else
    SetLength(Arr, High(Arr)+1);

  i := High(Arr);
  ZeroMemory(@Arr[i], sizeof(Arr[i]));
  Arr[i].GenerationGap := Gap;
  Arr[i].AncestorName := AncestorName;

  if Assigned(Sid) then
  begin
    Arr[i].SIDString := SID.CachedSidString;
    Arr[i].UserName := SID.GetAccountSidString('', DomainName, SidUse);
    Arr[i].System := DomainName;
    Arr[i].SID := Arr[i].UserName + '@' + Arr[i].SIDString;
  end;
end;

class function TInheritanceEntry.Get: TJwInheritedFromArray;
begin
  result := Arr;
end;

class function TInheritanceEntry.Init: TInheritanceEntry;
begin
  SetLength(Arr, 0);
end;

initialization
  JwInitWellKnownSIDs;
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestTTestGetFileInheritanceFunction.Suite);
end.

